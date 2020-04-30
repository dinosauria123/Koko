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

C       THIS IS THE FOURTH FILE OF RAYTRACING ROUTINES

C SUB PHASOR.FOR
      SUBROUTINE PHASOR
C
          IMPLICIT NONE
C
          REAL*8 L1,M1
C
          INTEGER IR,KLI
C
          LOGICAL FUN
C
          COMMON/NEFER/KLI
C
          COMMON/COMFUN/FUN
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datmac.inc'
          INTEGER WWVN
          COMMON/WVPASS/WWVN
C
          IF(ALENS(34,MYI).EQ.6.0D0) THEN
C
C     RADIAL PHASE SURFACE
              CALL PHAS6
              RETURN
          END IF
          IF(ALENS(34,MYI).EQ.9.0D0) THEN
C
C     66 TERM ZERNIKE PHASE
              CALL PHAS9
              RETURN
          END IF
          IF(ALENS(34,MYI).EQ.10.0D0) THEN
              CALL PHAS10
C
C     37 TERM ZERNIKE PHASE
              RETURN
          END IF
          IF(ALENS(34,MYI).EQ.15.0D0) THEN
              CALL PHAS15
C
C     48 TERM ABERRATION PHASE
              RETURN
          END IF
          IF(ALENS(34,MYI).EQ.7.0D0) THEN
C
C     91 TERM RECTANGULAR PHASE
              CALL PHAS7
              RETURN
          END IF
          IF(ALENS(34,MYI).EQ.11.0D0) THEN
              REG(40)=REG(9)
              REG(9)=MYX
              REG(10)=MYY
              REG(11)=INR
C     THIS SURFACE AUTOMATICALLY USES MACRO FUNCTION FUN09
C     FOR THE INITIAL USER FUCTION TO BE CALLED
C     PHASE IS RETURNED (IN RADIANS) IN THE IZ-REGISTER
C     CHANGE IN THE X DERIVATIVE OF THE PHASE IS RETURNED IN THE IX-REGISTER
C     CHANGE IN THE Y DERIVATIVE OF THE PHASE IS RETURNED IN THE IY-REGISTER
C
C     COEFFICIENTS C1 TO C96 ARE USED BY THE USER IN THE
C     MACRO FUNCTION FUN9 AND ANY MACRO FUNCTION CALLED BY FUN09.
C     THEY ARE STORED INITIALLY IN GENERAL PURPOSE STORAGE REGISTERS
C     NUMBERS 201 TO 248 AND ACCESSED BY THE USER VIA RCL COMMANDS
              IR=MYI
              GPREG(201)=FTFL01(1,IR)
              GPREG(202)=FTFL01(2,IR)
              GPREG(203)=FTFL01(3,IR)
              GPREG(204)=FTFL01(4,IR)
              GPREG(205)=FTFL01(5,IR)
              GPREG(206)=FTFL01(6,IR)
              GPREG(207)=FTFL01(7,IR)
              GPREG(208)=FTFL01(8,IR)
              GPREG(209)=FTFL01(9,IR)
              GPREG(210)=FTFL01(10,IR)
              GPREG(211)=FTFL01(11,IR)
              GPREG(212)=FTFL01(12,IR)
              GPREG(213)=FTFL01(13,IR)
              GPREG(214)=FTFL01(14,IR)
              GPREG(215)=FTFL01(15,IR)
              GPREG(216)=FTFL01(16,IR)
              GPREG(217)=FTFL01(17,IR)
              GPREG(218)=FTFL01(18,IR)
              GPREG(219)=FTFL01(19,IR)
              GPREG(220)=FTFL01(20,IR)
              GPREG(221)=FTFL01(21,IR)
              GPREG(222)=FTFL01(22,IR)
              GPREG(223)=FTFL01(23,IR)
              GPREG(224)=FTFL01(24,IR)
              GPREG(225)=FTFL01(25,IR)
              GPREG(226)=FTFL01(26,IR)
              GPREG(227)=FTFL01(27,IR)
              GPREG(228)=FTFL01(28,IR)
              GPREG(229)=FTFL01(29,IR)
              GPREG(230)=FTFL01(30,IR)
              GPREG(231)=FTFL01(31,IR)
              GPREG(232)=FTFL01(32,IR)
              GPREG(233)=FTFL01(33,IR)
              GPREG(234)=FTFL01(34,IR)
              GPREG(235)=FTFL01(35,IR)
              GPREG(236)=FTFL01(36,IR)
              GPREG(237)=FTFL01(37,IR)
              GPREG(238)=FTFL01(38,IR)
              GPREG(239)=FTFL01(39,IR)
              GPREG(240)=FTFL01(40,IR)
              GPREG(241)=FTFL01(41,IR)
              GPREG(242)=FTFL01(42,IR)
              GPREG(243)=FTFL01(43,IR)
              GPREG(244)=FTFL01(44,IR)
              GPREG(245)=FTFL01(45,IR)
              GPREG(246)=FTFL01(46,IR)
              GPREG(247)=FTFL01(47,IR)
              GPREG(248)=FTFL01(48,IR)
              GPREG(249)=FTFL01(49,IR)
              GPREG(250)=FTFL01(50,IR)
              GPREG(251)=FTFL01(51,IR)
              GPREG(252)=FTFL01(52,IR)
              GPREG(253)=FTFL01(53,IR)
              GPREG(254)=FTFL01(54,IR)
              GPREG(255)=FTFL01(55,IR)
              GPREG(256)=FTFL01(56,IR)
              GPREG(257)=FTFL01(57,IR)
              GPREG(258)=FTFL01(58,IR)
              GPREG(259)=FTFL01(59,IR)
              GPREG(260)=FTFL01(60,IR)
              GPREG(261)=FTFL01(61,IR)
              GPREG(262)=FTFL01(62,IR)
              GPREG(263)=FTFL01(63,IR)
              GPREG(264)=FTFL01(64,IR)
              GPREG(265)=FTFL01(65,IR)
              GPREG(266)=FTFL01(66,IR)
              GPREG(267)=FTFL01(67,IR)
              GPREG(268)=FTFL01(68,IR)
              GPREG(269)=FTFL01(69,IR)
              GPREG(270)=FTFL01(70,IR)
              GPREG(271)=FTFL01(71,IR)
              GPREG(272)=FTFL01(72,IR)
              GPREG(273)=FTFL01(73,IR)
              GPREG(274)=FTFL01(74,IR)
              GPREG(275)=FTFL01(75,IR)
              GPREG(276)=FTFL01(76,IR)
              GPREG(277)=FTFL01(77,IR)
              GPREG(278)=FTFL01(78,IR)
              GPREG(279)=FTFL01(79,IR)
              GPREG(280)=FTFL01(80,IR)
              GPREG(281)=FTFL01(81,IR)
              GPREG(282)=FTFL01(82,IR)
              GPREG(283)=FTFL01(83,IR)
              GPREG(284)=FTFL01(84,IR)
              GPREG(285)=FTFL01(85,IR)
              GPREG(286)=FTFL01(86,IR)
              GPREG(287)=FTFL01(87,IR)
              GPREG(288)=FTFL01(88,IR)
              GPREG(289)=FTFL01(89,IR)
              GPREG(290)=FTFL01(90,IR)
              GPREG(291)=FTFL01(91,IR)
              GPREG(292)=FTFL01(92,IR)
              GPREG(293)=FTFL01(93,IR)
              GPREG(294)=FTFL01(94,IR)
              GPREG(295)=FTFL01(95,IR)
              GPREG(296)=FTFL01(96,IR)
              IF(.NOT.FUNEXT(9)) THEN
C     NO FUN09 EXISTS, RETURN 0.0D0 AS THE Z CONTRIBUTION
                  PHASE=0.0D0
                  L1=0.0D0
                  M1=0.0D0
              ELSE
C     FUN9 EXISTS, RUN FUN9
                  SAVE_KDP(12)=SAVEINPT(12)

                  WC='FUN09'
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
                  FMNW(1:5)=0.0D0
                  FMDF(1:5)=1
                  NESFUN(0:10)=.FALSE.
                  KLI=9
                  FUN=.TRUE.
                  F26=1
                  CALL FUNEXC
                  F26=0
                  REST_KDP(12)=RESTINPT(12)
C     PHASE IN RADIAN MEASURE IN THE IZ-REGISTER
                  PHASE=REG(15)
C     CHANGE IN THE L-DIR COS IN THE IX-REGISTER
                  L1=REG(13)
C     CHANGE IN THE M-DIR COS IN THE IY-REGISTER
                  M1=REG(14)
              END IF
C     NOW MULTIPLY BY THE CONTROL WAVELENGTH IN LENS UNITS
              IF(SYSTEM1(6).EQ.4.0D0)
     1        PHASE=PHASE*SYSTEM1(INT(SYSTEM1(11)))*1.0D-6
              IF(SYSTEM1(6).EQ.3.0D0)
     1        PHASE=PHASE*SYSTEM1(INT(SYSTEM1(11)))*1.0D-3
              IF(SYSTEM1(6).EQ.2.0D0)
     1        PHASE=PHASE*SYSTEM1(INT(SYSTEM1(11)))*1.0D-4
              IF(SYSTEM1(6).EQ.1.0D0)
     1        PHASE=(PHASE*SYSTEM1(INT(SYSTEM1(11)))*1.0D-4)/2.54D0
              RETURN
          END IF
      END
C SUB PHAS6.FOR
      SUBROUTINE PHAS6
C
C     RADIAL PHASE SURFACE
C
          IMPLICIT NONE
C
          REAL*8 L1,M1,DERX6,DERY6,
     1    TERM,FAZE6,DEL,ERR,MMYX,MMYY
C
          EXTERNAL FAZE6,DERX6,DERY6
C
          INTEGER KLI
C
          LOGICAL FUN
C
          COMMON/NEFER/KLI
C
          COMMON/COMFUN/FUN
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datmac.inc'
          INTEGER WWVN
          COMMON/WVPASS/WWVN
C
C     RADIAL PHASE SURFACE
C
C       PROCEED WITH EVALUATION
          TERM=0.0D0
          MMYX=MYX
          MMYY=MYY
          PHASE=FAZE6(MMYX,MMYY)
C
          DEL=DELSUR
          L1=DERX6(MMYX,MMYY,DEL,ERR)
          M1=DERY6(MMYX,MMYY,DEL,ERR)
          IF(SYSTEM1(6).EQ.4.0D0)
     1    L1=L1*SYSTEM1(INT(SYSTEM1(11)))*1.0D-6
          IF(SYSTEM1(6).EQ.3.0D0)
     1    L1=L1*SYSTEM1(INT(SYSTEM1(11)))*1.0D-3
          IF(SYSTEM1(6).EQ.2.0D0)
     1    L1=L1*SYSTEM1(INT(SYSTEM1(11)))*1.0D-4
          IF(SYSTEM1(6).EQ.1.0D0)
     1    L1=(L1*SYSTEM1(INT(SYSTEM1(11)))*1.0D-4)/2.54D0
          IF(SYSTEM1(6).EQ.4.0D0)
     1    M1=M1*SYSTEM1(INT(SYSTEM1(11)))*1.0D-6
          IF(SYSTEM1(6).EQ.3.0D0)
     1    M1=M1*SYSTEM1(INT(SYSTEM1(11)))*1.0D-3
          IF(SYSTEM1(6).EQ.2.0D0)
     1    M1=M1*SYSTEM1(INT(SYSTEM1(11)))*1.0D-4
          IF(SYSTEM1(6).EQ.1.0D0)
     1    M1=(M1*SYSTEM1(INT(SYSTEM1(11)))*1.0D-4)/2.54D0
          CALL ADDPHASE(RLRL,RMRM,RNRN,L1,M1,LN,MN,NN)
C
C     NOW MULTIPLY BY THE CONTROL WAVELENGTH IN LENS UNITS
          IF(SYSTEM1(6).EQ.4.0D0)
     1    PHASE=PHASE*SYSTEM1(INT(SYSTEM1(11)))*1.0D-6
          IF(SYSTEM1(6).EQ.3.0D0)
     1    PHASE=PHASE*SYSTEM1(INT(SYSTEM1(11)))*1.0D-3
          IF(SYSTEM1(6).EQ.2.0D0)
     1    PHASE=PHASE*SYSTEM1(INT(SYSTEM1(11)))*1.0D-4
          IF(SYSTEM1(6).EQ.1.0D0)
     1    PHASE=(PHASE*SYSTEM1(INT(SYSTEM1(11)))*1.0D-4)/2.54D0
          RETURN
      END
      FUNCTION FAZE6(ARG1,ARG2)
          IMPLICIT NONE
          REAL*8 ARG,ARG1,ARG2,FAZE6
          INTEGER I
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datmac.inc'
          FAZE6=0.0D0
          ARG=DSQRT(DBLE(ARG1**2)+DBLE(ARG2**2))
          DO I=9,48
              IF(FTFL01(I,MYI).EQ.0.0D0.OR.ARG.EQ.0.0D0.AND.(I-9).EQ.0) THEN
                  FAZE6=FAZE6+FTFL01(I,MYI)
              ELSE
                  FAZE6=FAZE6+(FTFL01(I,MYI)*(ARG**(I-9)))
              END IF
          END DO
          RETURN
      END
      FUNCTION DERX6(X,Y,H,ERR)
C
C     SYMMETRIC DERIVATE FORMULA DESCRIBED IN NUMERICAL RECIPES 2ND ED.
C     PAGE 181, EQ. 5.7.7. THE CODE IS NOT FROM NUMERICAL RECIPES
C
          IMPLICIT NONE
          REAL*8 DERX6,ERR,H,X,Y,FAZE6,CON,CON2,BIG,SAFE
          EXTERNAL FAZE6
          INTEGER I,J,NTAB
          REAL*8 ERRT,FAC,HH,A(1:10,1:10)
C
          CON=1.4D0
          CON2=CON*CON
          BIG=1.0D30
          NTAB=10
          SAFE=2.0D0
C
          HH=H
          IF((FAZE6(X+HH,Y)-FAZE6(X-HH,Y)).EQ.0.0D0) THEN
              DERX6=0.0D0
              RETURN
          END IF
          A(1,1)=(FAZE6(X+HH,Y)-FAZE6(X-HH,Y))/(2.0D0*HH)
          ERR=BIG
          DO I=2,NTAB
              HH=HH/CON
              A(1,I)=(FAZE6(X+HH,Y)-FAZE6(X-HH,Y))/(2.0D0*HH)
              FAC=CON2
              DO J=2,I
                  A(J,I)=(A(J-1,I)*FAC-A(J-1,I-1))/(FAC-1.0D0)
                  FAC=CON2*FAC
                  ERRT=DMAX1(DABS(A(J,I)-A(J-1,I)),DABS(A(J,I)-A(J-1,I-1)))
                  IF(ERRT.LE.ERR) THEN
                      ERR=ERRT
                      DERX6=A(J,I)
                  END IF
              END DO
              IF(DABS(A(I,I)-A(I-1,I-1)).GE.SAFE*ERR) THEN
                  RETURN
              END IF
          END DO
          RETURN
      END
      FUNCTION DERY6(X,Y,H,ERR)
C
C     SYMMETRIC DERIVATE FORMULA DESCRIBED IN NUMERICAL RECIPES 2ND ED.
C     PAGE 181, EQ. 5.7.7. THE CODE IS NOT FROM NUMERICAL RECIPES
C
          IMPLICIT NONE
          REAL*8 DERY6,ERR,H,X,Y,FAZE6,CON,CON2,BIG,SAFE
          EXTERNAL FAZE6
          INTEGER I,J,NTAB
          REAL*8 ERRT,FAC,HH,A(1:10,1:10)
C
          CON=1.4D0
          CON2=CON*CON
          BIG=1.0D30
          NTAB=10
          SAFE=2.0D0
C
          HH=H
          IF((FAZE6(X,Y+HH)-FAZE6(X,Y-HH)).EQ.0.0D0) THEN
              DERY6=0.0D0
              RETURN
          END IF
          A(1,1)=(FAZE6(X,Y+HH)-FAZE6(X,Y-HH))/(2.0D0*HH)
          ERR=BIG
          DO I=2,NTAB
              HH=HH/CON
              A(1,I)=(FAZE6(X,Y+HH)-FAZE6(X,Y-HH))/(2.0D0*HH)
              FAC=CON2
              DO J=2,I
                  A(J,I)=(A(J-1,I)*FAC-A(J-1,I-1))/(FAC-1.0D0)
                  FAC=CON2*FAC
                  ERRT=DMAX1(DABS(A(J,I)-A(J-1,I)),DABS(A(J,I)-A(J-1,I-1)))
                  IF(ERRT.LE.ERR) THEN
                      ERR=ERRT
                      DERY6=A(J,I)
                  END IF
              END DO
              IF(DABS(A(I,I)-A(I-1,I-1)).GE.SAFE*ERR) THEN
                  RETURN
              END IF
          END DO
          RETURN
      END

      
C     SUB PHAS9.FOR
      SUBROUTINE PHAS9
C
C     30 TERM ZERNIKE PHASE
C
C     RADIAL PHASE SURFACE
C
          IMPLICIT NONE
C
          REAL*8 L1,M1,DERX9,DERY9,
     1    TERM,FAZE9,DEL,ERR,MMYX,MMYY
C
          EXTERNAL FAZE9,DERX9,DERY9
C
          INTEGER KLI
C
          LOGICAL FUN
C
          COMMON/NEFER/KLI
C
          COMMON/COMFUN/FUN
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datmac.inc'
          INTEGER WWVN
          COMMON/WVPASS/WWVN
C
C     RADIAL PHASE SURFACE
C
C       PROCEED WITH EVALUATION
          TERM=0.0D0
          MMYX=MYX/INR
          MMYY=MYY/INR
          PHASE=FAZE9(MMYX,MMYY)
C
          DEL=DELSUR
          L1=DERX9(MMYX,MMYY,DEL,ERR)
          M1=DERY9(MMYX,MMYY,DEL,ERR)
          IF(SYSTEM1(6).EQ.4.0D0)
     1    L1=L1*SYSTEM1(INT(SYSTEM1(11)))*1.0D-6
          IF(SYSTEM1(6).EQ.3.0D0)
     1    L1=L1*SYSTEM1(INT(SYSTEM1(11)))*1.0D-3
          IF(SYSTEM1(6).EQ.2.0D0)
     1    L1=L1*SYSTEM1(INT(SYSTEM1(11)))*1.0D-4
          IF(SYSTEM1(6).EQ.1.0D0)
     1    L1=(L1*SYSTEM1(INT(SYSTEM1(11)))*1.0D-4)/2.54D0
          IF(SYSTEM1(6).EQ.4.0D0)
     1    M1=M1*SYSTEM1(INT(SYSTEM1(11)))*1.0D-6
          IF(SYSTEM1(6).EQ.3.0D0)
     1    M1=M1*SYSTEM1(INT(SYSTEM1(11)))*1.0D-3
          IF(SYSTEM1(6).EQ.2.0D0)
     1    M1=M1*SYSTEM1(INT(SYSTEM1(11)))*1.0D-4
          IF(SYSTEM1(6).EQ.1.0D0)
     1    M1=(M1*SYSTEM1(INT(SYSTEM1(11)))*1.0D-4)/2.54D0
          CALL ADDPHASE(RLRL,RMRM,RNRN,L1,M1,LN,MN,NN)
C     NOW MULTIPLY BY THE CONTROL WAVELENGTH IN LENS UNITS
          IF(SYSTEM1(6).EQ.4.0D0)
     1    PHASE=PHASE*SYSTEM1(INT(SYSTEM1(11)))*1.0D-6
          IF(SYSTEM1(6).EQ.3.0D0)
     1    PHASE=PHASE*SYSTEM1(INT(SYSTEM1(11)))*1.0D-3
          IF(SYSTEM1(6).EQ.2.0D0)
     1    PHASE=PHASE*SYSTEM1(INT(SYSTEM1(11)))*1.0D-4
          IF(SYSTEM1(6).EQ.1.0D0)
     1    PHASE=(PHASE*SYSTEM1(INT(SYSTEM1(11)))*1.0D-4)/2.54D0
          RETURN
      END
      FUNCTION FAZE9(ARG1,ARG2)
          IMPLICIT NONE
          REAL*8 ARGA,ARGB,ARG1,ARG2,FAZE9,FF2
          EXTERNAL FF2
          INTEGER I
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datmac.inc'
          ARGA=DSQRT(DBLE(ARG1**2)+DBLE(ARG2**2))
          IF(ARG1.EQ.0.0D0.AND.ARG2.EQ.0.0D0) THEN
              ARGB=0.0D0
          ELSE
              IF(DABS(ARG2).GE.DABS(((1.0D35)*ARG1))) THEN
                  IF(ARG2.GE.0.0D0) THEN
                      ARGB=PII/2.0D0
                  END IF
                  IF(ARG2.LT.0.0D0) THEN
                      ARGB=(3.0D0*PII)/2.0D0
                  END IF
              ELSE
                  IF(DABS(ARG1).EQ.0.0D0.AND.DABS(ARG2).EQ.0.0D0) THEN
                      ARGB=0.0D0
                  ELSE
                      ARGB=DATAN2(ARG1,ARG2)
                  END IF
                  IF(ARGB.LT.0.0D0) ARGB=ARGB+(TWOPII)
              END IF
          END IF
          FAZE9=0.0D0
          DO I=1,66
              IF(FTFL01(I,MYI).NE.0.0D0) THEN
                  FAZE9=FAZE9+(FTFL01(I,MYI)*FF2(ARGA,ARGB,I))
              END IF
          END DO
          RETURN
      END


      FUNCTION DERX9(X,Y,H,ERR)
C
C     SYMMETRIC DERIVATE FORMULA DESCRIBED IN NUMERICAL RECIPES 2ND ED.
C     PAGE 181, EQ. 5.7.7. THE CODE IS NOT FROM NUMERICAL RECIPES
C
          IMPLICIT NONE
          REAL*8 DERX9,ERR,H,X,Y,FAZE9,CON,CON2,BIG,SAFE
          EXTERNAL FAZE9
          INTEGER I,J,NTAB
          REAL*8 ERRT,FAC,HH,A(1:10,1:10)
C
          CON=1.4D0
          CON2=CON*CON
          BIG=1.0D30
          NTAB=10
          SAFE=2.0D0
C
          HH=H
          IF((FAZE9(X+HH,Y)-FAZE9(X-HH,Y)).EQ.0.0D0) THEN
              DERX9=0.0D0
              RETURN
          END IF
          A(1,1)=(FAZE9(X+HH,Y)-FAZE9(X-HH,Y))/(2.0D0*HH)
          ERR=BIG
          DO I=2,NTAB
              HH=HH/CON
              A(1,I)=(FAZE9(X+HH,Y)-FAZE9(X-HH,Y))/(2.0D0*HH)
              FAC=CON2
              DO J=2,I
                  A(J,I)=(A(J-1,I)*FAC-A(J-1,I-1))/(FAC-1.0D0)
                  FAC=CON2*FAC
                  ERRT=DMAX1(DABS(A(J,I)-A(J-1,I)),DABS(A(J,I)-A(J-1,I-1)))
                  IF(ERRT.LE.ERR) THEN
                      ERR=ERRT
                      DERX9=A(J,I)
                  END IF
              END DO
              IF(DABS(A(I,I)-A(I-1,I-1)).GE.SAFE*ERR) THEN
                  RETURN
              END IF
          END DO
          RETURN
      END
      FUNCTION DERY9(X,Y,H,ERR)
C
C     SYMMETRIC DERIVATE FORMULA DESCRIBED IN NUMERICAL RECIPES 2ND ED.
C     PAGE 181, EQ. 5.7.7. THE CODE IS NOT FROM NUMERICAL RECIPES
C
          IMPLICIT NONE
          REAL*8 DERY9,ERR,H,X,Y,FAZE9,CON,CON2,BIG,SAFE
          EXTERNAL FAZE9
          INTEGER I,J,NTAB
          REAL*8 ERRT,FAC,HH,A(1:10,1:10)
C
          CON=1.4D0
          CON2=CON*CON
          BIG=1.0D30
          NTAB=10
          SAFE=2.0D0
C
          HH=H
          IF((FAZE9(X,Y+HH)-FAZE9(X,Y-HH)).EQ.0.0D0) THEN
              DERY9=0.0D0
              RETURN
          END IF
          A(1,1)=(FAZE9(X,Y+HH)-FAZE9(X,Y-HH))/(2.0D0*HH)
          ERR=BIG
          DO I=2,NTAB
              HH=HH/CON
              A(1,I)=(FAZE9(X,Y+HH)-FAZE9(X,Y-HH))/(2.0D0*HH)
              FAC=CON2
              DO J=2,I
                  A(J,I)=(A(J-1,I)*FAC-A(J-1,I-1))/(FAC-1.0D0)
                  FAC=CON2*FAC
                  ERRT=DMAX1(DABS(A(J,I)-A(J-1,I)),DABS(A(J,I)-A(J-1,I-1)))
                  IF(ERRT.LE.ERR) THEN
                      ERR=ERRT
                      DERY9=A(J,I)
                  END IF
              END DO
              IF(DABS(A(I,I)-A(I-1,I-1)).GE.SAFE*ERR) THEN
                  RETURN
              END IF
          END DO
          RETURN
      END
C SUB PHAS10.FOR
      SUBROUTINE PHAS10
C
C     37 TERM ZERNIKE PHASE
C
          IMPLICIT NONE
C
          REAL*8 L1,M1,DERX10,DERY10,
     1    TERM,FAZE10,DEL,ERR,MMYX,MMYY
C
          EXTERNAL FAZE10,DERX10,DERY10
C
          INTEGER KLI
C
          LOGICAL FUN
C
          COMMON/NEFER/KLI
C
          COMMON/COMFUN/FUN
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datmac.inc'
          INTEGER WWVN
          COMMON/WVPASS/WWVN
C
C     RADIAL PHASE SURFACE
C
C       PROCEED WITH EVALUATION
          TERM=0.0D0
          MMYX=MYX/INR
          MMYY=MYY/INR
          PHASE=FAZE10(MMYX,MMYY)
C
          DEL=DELSUR
          L1=DERX10(MMYX,MMYY,DEL,ERR)
          IF(SYSTEM1(6).EQ.4.0D0)
     1    L1=L1*SYSTEM1(INT(SYSTEM1(11)))*1.0D-6
          IF(SYSTEM1(6).EQ.3.0D0)
     1    L1=L1*SYSTEM1(INT(SYSTEM1(11)))*1.0D-3
          IF(SYSTEM1(6).EQ.2.0D0)
     1    L1=L1*SYSTEM1(INT(SYSTEM1(11)))*1.0D-4
          IF(SYSTEM1(6).EQ.1.0D0)
     1    L1=(L1*SYSTEM1(INT(SYSTEM1(11)))*1.0D-4)/2.54D0
          IF(SYSTEM1(6).EQ.4.0D0)
     1    M1=M1*SYSTEM1(INT(SYSTEM1(11)))*1.0D-6
          IF(SYSTEM1(6).EQ.3.0D0)
     1    M1=M1*SYSTEM1(INT(SYSTEM1(11)))*1.0D-3
          IF(SYSTEM1(6).EQ.2.0D0)
     1    M1=M1*SYSTEM1(INT(SYSTEM1(11)))*1.0D-4
          IF(SYSTEM1(6).EQ.1.0D0)
     1    M1=(M1*SYSTEM1(INT(SYSTEM1(11)))*1.0D-4)/2.54D0
          M1=DERY10(MMYX,MMYY,DEL,ERR)
          CALL ADDPHASE(RLRL,RMRM,RNRN,L1,M1,LN,MN,NN)
C
C     NOW MULTIPLY BY THE CONTROL WAVELENGTH IN LENS UNITS
          IF(SYSTEM1(6).EQ.4.0D0)
     1    PHASE=PHASE*SYSTEM1(INT(SYSTEM1(11)))*1.0D-6
          IF(SYSTEM1(6).EQ.3.0D0)
     1    PHASE=PHASE*SYSTEM1(INT(SYSTEM1(11)))*1.0D-3
          IF(SYSTEM1(6).EQ.2.0D0)
     1    PHASE=PHASE*SYSTEM1(INT(SYSTEM1(11)))*1.0D-4
          IF(SYSTEM1(6).EQ.1.0D0)
     1    PHASE=(PHASE*SYSTEM1(INT(SYSTEM1(11)))*1.0D-4)/2.54D0
          RETURN
      END
      FUNCTION FAZE10(ARG1,ARG2)
          IMPLICIT NONE
          REAL*8 ARGA,ARGB,ARG1,ARG2,FAZE10,FF3
          EXTERNAL FF3
          INTEGER I
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datmac.inc'
          FAZE10=0.0D0
          ARGA=DSQRT(DBLE(ARG1**2)+DBLE(ARG2**2))
          IF(ARG1.EQ.0.0D0.AND.ARG2.EQ.0.0D0) THEN
              ARGB=0.0D0
          ELSE
              IF(DABS(ARG2).GE.DABS(((1.0D35)*ARG1))) THEN
                  IF(ARG2.GE.0.0D0) THEN
                      ARGB=PII/2.0D0
                  END IF
                  IF(ARG2.LT.0.0D0) THEN
                      ARGB=(3.0D0*PII)/2.0D0
                  END IF
              ELSE
                  IF(DABS(ARG1).EQ.0.0D0.AND.DABS(ARG2).EQ.0.0D0) THEN
                      ARGB=0.0D0
                  ELSE
                      ARGB=DATAN2(ARG1,ARG2)
                  END IF
                  IF(ARGB.LT.0.0D0) ARGB=ARGB+(TWOPII)
              END IF
          END IF
          DO I=1,37
              IF(FTFL01(I,MYI).NE.0.0D0) THEN
                  FAZE10=FAZE10+(FTFL01(I,MYI)*FF3(ARGA,ARGB,I))
              END IF
          END DO
          RETURN
      END
      FUNCTION DERX10(X,Y,H,ERR)
C
C     SYMMETRIC DERIVATE FORMULA DESCRIBED IN NUMERICAL RECIPES 2ND ED.
C     PAGE 181, EQ. 5.7.7. THE CODE IS NOT FROM NUMERICAL RECIPES
C
          IMPLICIT NONE
          REAL*8 DERX10,ERR,H,X,Y,FAZE10,CON,CON2,BIG,SAFE
          EXTERNAL FAZE10
          INTEGER I,J,NTAB
          REAL*8 ERRT,FAC,HH,A(1:10,1:10)
C
          CON=1.4D0
          CON2=CON*CON
          BIG=1.0D30
          NTAB=10
          SAFE=2.0D0
C
          HH=H
          IF((FAZE10(X+HH,Y)-FAZE10(X-HH,Y)).EQ.0.0D0) THEN
              DERX10=0.0D0
              RETURN
          END IF
          A(1,1)=(FAZE10(X+HH,Y)-FAZE10(X-HH,Y))/(2.0D0*HH)
          ERR=BIG
          DO I=2,NTAB
              HH=HH/CON
              A(1,I)=(FAZE10(X+HH,Y)-FAZE10(X-HH,Y))/(2.0D0*HH)
              FAC=CON2
              DO J=2,I
                  A(J,I)=(A(J-1,I)*FAC-A(J-1,I-1))/(FAC-1.0D0)
                  FAC=CON2*FAC
                  ERRT=DMAX1(DABS(A(J,I)-A(J-1,I)),DABS(A(J,I)-A(J-1,I-1)))
                  IF(ERRT.LE.ERR) THEN
                      ERR=ERRT
                      DERX10=A(J,I)
                  END IF
              END DO
              IF(DABS(A(I,I)-A(I-1,I-1)).GE.SAFE*ERR) THEN
                  RETURN
              END IF
          END DO
          RETURN
      END
      FUNCTION DERY10(X,Y,H,ERR)
C
C     SYMMETRIC DERIVATE FORMULA DESCRIBED IN NUMERICAL RECIPES 2ND ED.
C     PAGE 181, EQ. 5.7.7. THE CODE IS NOT FROM NUMERICAL RECIPES
C
          IMPLICIT NONE
          REAL*8 DERY10,ERR,H,X,Y,FAZE10,CON,CON2,BIG,SAFE
          EXTERNAL FAZE10
          INTEGER I,J,NTAB
          REAL*8 ERRT,FAC,HH,A(1:10,1:10)
C
          CON=1.4D0
          CON2=CON*CON
          BIG=1.0D30
          NTAB=10
          SAFE=2.0D0
C
          HH=H
          IF((FAZE10(X,Y+HH)-FAZE10(X,Y-HH)).EQ.0.0D0) THEN
              DERY10=0.0D0
              RETURN
          END IF
          A(1,1)=(FAZE10(X,Y+HH)-FAZE10(X,Y-HH))/(2.0D0*HH)
          ERR=BIG
          DO I=2,NTAB
              HH=HH/CON
              A(1,I)=(FAZE10(X,Y+HH)-FAZE10(X,Y-HH))/(2.0D0*HH)
              FAC=CON2
              DO J=2,I
                  A(J,I)=(A(J-1,I)*FAC-A(J-1,I-1))/(FAC-1.0D0)
                  FAC=CON2*FAC
                  ERRT=DMAX1(DABS(A(J,I)-A(J-1,I)),DABS(A(J,I)-A(J-1,I-1)))
                  IF(ERRT.LE.ERR) THEN
                      ERR=ERRT
                      DERY10=A(J,I)
                  END IF
              END DO
              IF(DABS(A(I,I)-A(I-1,I-1)).GE.SAFE*ERR) THEN
                  RETURN
              END IF
          END DO
          RETURN
      END
C SUB PHAS7.FOR
      SUBROUTINE PHAS7
C
C     91 TERM RECTANGULAR PHASE
C
          IMPLICIT NONE
C
          REAL*8 L1,M1,DERX7,DERY7,
     1    TERM,FAZE7,DEL,ERR,MMYY,MMYX
C
          EXTERNAL FAZE7,DERX7,DERY7
C
          INTEGER KLI
C
          LOGICAL FUN
C
          COMMON/NEFER/KLI
C
          COMMON/COMFUN/FUN
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datmac.inc'
          INTEGER WWVN
          COMMON/WVPASS/WWVN
C
C     RADIAL PHASE SURFACE
C
C       PROCEED WITH EVALUATION
          TERM=0.0D0
          MMYX=MYX
          MMYY=MYY
          PHASE=FAZE7(MMYX,MMYY)
C
          DEL=DELSUR
          L1=DERX7(MMYX,MMYY,DEL,ERR)
          M1=DERY7(MMYX,MMYY,DEL,ERR)
          IF(SYSTEM1(6).EQ.4.0D0)
     1    L1=L1*SYSTEM1(INT(SYSTEM1(11)))*1.0D-6
          IF(SYSTEM1(6).EQ.3.0D0)
     1    L1=L1*SYSTEM1(INT(SYSTEM1(11)))*1.0D-3
          IF(SYSTEM1(6).EQ.2.0D0)
     1    L1=L1*SYSTEM1(INT(SYSTEM1(11)))*1.0D-4
          IF(SYSTEM1(6).EQ.1.0D0)
     1    L1=(L1*SYSTEM1(INT(SYSTEM1(11)))*1.0D-4)/2.54D0
          IF(SYSTEM1(6).EQ.4.0D0)
     1    M1=M1*SYSTEM1(INT(SYSTEM1(11)))*1.0D-6
          IF(SYSTEM1(6).EQ.3.0D0)
     1    M1=M1*SYSTEM1(INT(SYSTEM1(11)))*1.0D-3
          IF(SYSTEM1(6).EQ.2.0D0)
     1    M1=M1*SYSTEM1(INT(SYSTEM1(11)))*1.0D-4
          IF(SYSTEM1(6).EQ.1.0D0)
     1    M1=(M1*SYSTEM1(INT(SYSTEM1(11)))*1.0D-4)/2.54D0
          CALL ADDPHASE(RLRL,RMRM,RNRN,L1,M1,LN,MN,NN)
C
C     NOW MULTIPLY BY THE CONTROL WAVELENGTH IN LENS UNITS
          IF(SYSTEM1(6).EQ.4.0D0)
     1    PHASE=PHASE*SYSTEM1(INT(SYSTEM1(11)))*1.0D-6
          IF(SYSTEM1(6).EQ.3.0D0)
     1    PHASE=PHASE*SYSTEM1(INT(SYSTEM1(11)))*1.0D-3
          IF(SYSTEM1(6).EQ.2.0D0)
     1    PHASE=PHASE*SYSTEM1(INT(SYSTEM1(11)))*1.0D-4
          IF(SYSTEM1(6).EQ.1.0D0)
     1    PHASE=(PHASE*SYSTEM1(INT(SYSTEM1(11)))*1.0D-4)/2.54D0
          RETURN
      END
      FUNCTION FAZE7(ARG1,ARG2)
          IMPLICIT NONE
          REAL*8 ARG1,ARG2,FAZE7,FF4
          EXTERNAL FF4
          INTEGER I
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datmac.inc'
          FAZE7=0.0D0
          DO I=1,91
              IF(FTFL01(I,MYI).NE.0.0D0) THEN
                  FAZE7=FAZE7+(FTFL01(I,MYI)*FF4(ARG1,ARG2,I))
              END IF
          END DO
          RETURN
      END
      FUNCTION DERX7(X,Y,H,ERR)
C
C     SYMMETRIC DERIVATE FORMULA DESCRIBED IN NUMERICAL RECIPES 2ND ED.
C     PAGE 181, EQ. 5.7.7. THE CODE IS NOT FROM NUMERICAL RECIPES
C
          IMPLICIT NONE
          REAL*8 DERX7,ERR,H,X,Y,FAZE7,CON,CON2,BIG,SAFE
          EXTERNAL FAZE7
          INTEGER I,J,NTAB
          REAL*8 ERRT,FAC,HH,A(1:10,1:10)
C
          CON=1.4D0
          CON2=CON*CON
          BIG=1.0D30
          NTAB=10
          SAFE=2.0D0
C
          HH=H
          IF((FAZE7(X+HH,Y)-FAZE7(X-HH,Y)).EQ.0.0D0) THEN
              DERX7=0.0D0
              RETURN
          END IF
          A(1,1)=(FAZE7(X+HH,Y)-FAZE7(X-HH,Y))/(2.0D0*HH)
          ERR=BIG
          DO I=2,NTAB
              HH=HH/CON
              A(1,I)=(FAZE7(X+HH,Y)-FAZE7(X-HH,Y))/(2.0D0*HH)
              FAC=CON2
              DO J=2,I
                  A(J,I)=(A(J-1,I)*FAC-A(J-1,I-1))/(FAC-1.0D0)
                  FAC=CON2*FAC
                  ERRT=DMAX1(DABS(A(J,I)-A(J-1,I)),DABS(A(J,I)-A(J-1,I-1)))
                  IF(ERRT.LE.ERR) THEN
                      ERR=ERRT
                      DERX7=A(J,I)
                  END IF
              END DO
              IF(DABS(A(I,I)-A(I-1,I-1)).GE.SAFE*ERR) THEN
                  RETURN
              END IF
          END DO
          RETURN
      END
      FUNCTION DERY7(X,Y,H,ERR)
C
C     SYMMETRIC DERIVATE FORMULA DESCRIBED IN NUMERICAL RECIPES 2ND ED.
C     PAGE 181, EQ. 5.7.7. THE CODE IS NOT FROM NUMERICAL RECIPES
C
          IMPLICIT NONE
          REAL*8 DERY7,ERR,H,X,Y,FAZE7,CON,CON2,BIG,SAFE
          EXTERNAL FAZE7
          INTEGER I,J,NTAB
          REAL*8 ERRT,FAC,HH,A(1:10,1:10)
C
          CON=1.4D0
          CON2=CON*CON
          BIG=1.0D30
          NTAB=10
          SAFE=2.0D0
C
          HH=H
          IF((FAZE7(X,Y+HH)-FAZE7(X,Y-HH)).EQ.0.0D0) THEN
              DERY7=0.0D0
              RETURN
          END IF
          A(1,1)=(FAZE7(X,Y+HH)-FAZE7(X,Y-HH))/(2.0D0*HH)
          ERR=BIG
          DO I=2,NTAB
              HH=HH/CON
              A(1,I)=(FAZE7(X,Y+HH)-FAZE7(X,Y-HH))/(2.0D0*HH)
              FAC=CON2
              DO J=2,I
                  A(J,I)=(A(J-1,I)*FAC-A(J-1,I-1))/(FAC-1.0D0)
                  FAC=CON2*FAC
                  ERRT=DMAX1(DABS(A(J,I)-A(J-1,I)),DABS(A(J,I)-A(J-1,I-1)))
                  IF(ERRT.LE.ERR) THEN
                      ERR=ERRT
                      DERY7=A(J,I)
                  END IF
              END DO
              IF(DABS(A(I,I)-A(I-1,I-1)).GE.SAFE*ERR) THEN
                  RETURN
              END IF
          END DO
          RETURN
      END
C SUB PHAS15.FOR
      SUBROUTINE PHAS15
C
C     48 TERM ABERRATION PHASE
C
          IMPLICIT NONE
C
          REAL*8 L1,M1,DERX15,DERY15,
     1    TERM,FAZE15,DEL,ERR,MMYY,MMYX
C
          EXTERNAL FAZE15,DERX15,DERY15
C
          INTEGER KLI
C
          LOGICAL FUN
C
          COMMON/NEFER/KLI
C
          COMMON/COMFUN/FUN
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datmac.inc'
          INTEGER WWVN
          COMMON/WVPASS/WWVN
C
C     RADIAL PHASE SURFACE
C
C       PROCEED WITH EVALUATION
          TERM=0.0D0
          MMYX=MYX/INR
          MMYY=MYY/INR
          PHASE=FAZE15(MMYX,MMYY)
C
          DEL=DELSUR
          L1=DERX15(MMYX,MMYY,DEL,ERR)
          M1=DERY15(MMYX,MMYY,DEL,ERR)
          IF(SYSTEM1(6).EQ.4.0D0)
     1    L1=L1*SYSTEM1(INT(SYSTEM1(11)))*1.0D-6
          IF(SYSTEM1(6).EQ.3.0D0)
     1    L1=L1*SYSTEM1(INT(SYSTEM1(11)))*1.0D-3
          IF(SYSTEM1(6).EQ.2.0D0)
     1    L1=L1*SYSTEM1(INT(SYSTEM1(11)))*1.0D-4
          IF(SYSTEM1(6).EQ.1.0D0)
     1    L1=(L1*SYSTEM1(INT(SYSTEM1(11)))*1.0D-4)/2.54D0
          IF(SYSTEM1(6).EQ.4.0D0)
     1    M1=M1*SYSTEM1(INT(SYSTEM1(11)))*1.0D-6
          IF(SYSTEM1(6).EQ.3.0D0)
     1    M1=M1*SYSTEM1(INT(SYSTEM1(11)))*1.0D-3
          IF(SYSTEM1(6).EQ.2.0D0)
     1    M1=M1*SYSTEM1(INT(SYSTEM1(11)))*1.0D-4
          IF(SYSTEM1(6).EQ.1.0D0)
     1    M1=(M1*SYSTEM1(INT(SYSTEM1(11)))*1.0D-4)/2.54D0
          CALL ADDPHASE(RLRL,RMRM,RNRN,L1,M1,LN,MN,NN)
C
C     NOW MULTIPLY BY THE CONTROL WAVELENGTH IN LENS UNITS
          IF(SYSTEM1(6).EQ.4.0D0)
     1    PHASE=PHASE*SYSTEM1(INT(SYSTEM1(11)))*1.0D-6
          IF(SYSTEM1(6).EQ.3.0D0)
     1    PHASE=PHASE*SYSTEM1(INT(SYSTEM1(11)))*1.0D-3
          IF(SYSTEM1(6).EQ.2.0D0)
     1    PHASE=PHASE*SYSTEM1(INT(SYSTEM1(11)))*1.0D-4
          IF(SYSTEM1(6).EQ.1.0D0)
     1    PHASE=(PHASE*SYSTEM1(INT(SYSTEM1(11)))*1.0D-4)/2.54D0
          RETURN
      END
      FUNCTION FAZE15(ARG1,ARG2)
          IMPLICIT NONE
          REAL*8 ARGA,ARGB,ARG1,ARG2,FAZE15,FF5
          EXTERNAL FF5
          INTEGER I
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datmac.inc'
          FAZE15=0.0D0
          ARGA=DSQRT(DBLE(ARG1**2)+DBLE(ARG2**2))
          IF(ARG1.EQ.0.0D0.AND.ARG2.EQ.0.0D0) THEN
              ARGB=0.0D0
          ELSE
              IF(DABS(ARG2).GE.DABS(((1.0D35)*ARG1))) THEN
                  IF(ARG2.GE.0.0D0) THEN
                      ARGB=PII/2.0D0
                  END IF
                  IF(ARG2.LT.0.0D0) THEN
                      ARGB=(3.0D0*PII)/2.0D0
                  END IF
              ELSE
                  IF(DABS(ARG1).EQ.0.0D0.AND.DABS(ARG2).EQ.0.0D0) THEN
                      ARGB=0.0D0
                  ELSE
                      ARGB=DATAN2(ARG1,ARG2)
                  END IF
                  IF(ARGB.LT.0.0D0) ARGB=ARGB+(TWOPII)
              END IF
          END IF
          DO I=1,48
              IF(FTFL01(I,MYI).NE.0.0D0) THEN
                  FAZE15=FAZE15+(FTFL01(I,MYI)*FF5(ARGA,ARGB,I))
              END IF
          END DO
          RETURN
      END
      FUNCTION DERX15(X,Y,H,ERR)
C
C     SYMMETRIC DERIVATE FORMULA DESCRIBED IN NUMERICAL RECIPES 2ND ED.
C     PAGE 181, EQ. 5.7.7. THE CODE IS NOT FROM NUMERICAL RECIPES
C
          IMPLICIT NONE
          REAL*8 DERX15,ERR,H,X,Y,FAZE15,CON,CON2,BIG,SAFE
          EXTERNAL FAZE15
          INTEGER I,J,NTAB
          REAL*8 ERRT,FAC,HH,A(1:10,1:10)
C
          CON=1.4D0
          CON2=CON*CON
          BIG=1.0D30
          NTAB=10
          SAFE=2.0D0
C
          HH=H
          IF((FAZE15(X+HH,Y)-FAZE15(X-HH,Y)).EQ.0.0D0) THEN
              DERX15=0.0D0
              RETURN
          END IF
          A(1,1)=(FAZE15(X+HH,Y)-FAZE15(X-HH,Y))/(2.0D0*HH)
          ERR=BIG
          DO I=2,NTAB
              HH=HH/CON
              A(1,I)=(FAZE15(X+HH,Y)-FAZE15(X-HH,Y))/(2.0D0*HH)
              FAC=CON2
              DO J=2,I
                  A(J,I)=(A(J-1,I)*FAC-A(J-1,I-1))/(FAC-1.0D0)
                  FAC=CON2*FAC
                  ERRT=DMAX1(DABS(A(J,I)-A(J-1,I)),DABS(A(J,I)-A(J-1,I-1)))
                  IF(ERRT.LE.ERR) THEN
                      ERR=ERRT
                      DERX15=A(J,I)
                  END IF
              END DO
              IF(DABS(A(I,I)-A(I-1,I-1)).GE.SAFE*ERR) THEN
                  RETURN
              END IF
          END DO
          RETURN
      END
      FUNCTION DERY15(X,Y,H,ERR)
C
C     SYMMETRIC DERIVATE FORMULA DESCRIBED IN NUMERICAL RECIPES 2ND ED.
C     PAGE 181, EQ. 5.7.7. THE CODE IS NOT FROM NUMERICAL RECIPES
C
          IMPLICIT NONE
          REAL*8 DERY15,ERR,H,X,Y,FAZE15,CON,CON2,BIG,SAFE
          EXTERNAL FAZE15
          INTEGER I,J,NTAB
          REAL*8 ERRT,FAC,HH,A(1:10,1:10)
C
          CON=1.4D0
          CON2=CON*CON
          BIG=1.0D30
          NTAB=10
          SAFE=2.0D0
C
          HH=H
          IF((FAZE15(X,Y+HH)-FAZE15(X,Y-HH)).EQ.0.0D0) THEN
              DERY15=0.0D0
              RETURN
          END IF
          A(1,1)=(FAZE15(X,Y+HH)-FAZE15(X,Y-HH))/(2.0D0*HH)
          ERR=BIG
          DO I=2,NTAB
              HH=HH/CON
              A(1,I)=(FAZE15(X,Y+HH)-FAZE15(X,Y-HH))/(2.0D0*HH)
              FAC=CON2
              DO J=2,I
                  A(J,I)=(A(J-1,I)*FAC-A(J-1,I-1))/(FAC-1.0D0)
                  FAC=CON2*FAC
                  ERRT=DMAX1(DABS(A(J,I)-A(J-1,I)),DABS(A(J,I)-A(J-1,I-1)))
                  IF(ERRT.LE.ERR) THEN
                      ERR=ERRT
                      DERY15=A(J,I)
                  END IF
              END DO
              IF(DABS(A(I,I)-A(I-1,I-1)).GE.SAFE*ERR) THEN
                  RETURN
              END IF
          END DO
          RETURN
      END



      SUBROUTINE ADDPHASE(L,M,N,L1,M1,LN,MN,NN)
          IMPLICIT NONE
          REAL*8 N1,L,M,N,L1,M1,NV,LN,MN,NN
          REAL*8 IX,IY,LT,MT,NT,X1,Y1
          REAL*8 X,Y,IXP,IYP,XN,YN
          INCLUDE 'datmai.inc'
          NV=0.0D0
          IF(N.GE.0.0D0) NV=1.0D0
          IF(N.LT.0.0D0) NV=-1.0D0
          N1=NV
C     SURFACE NORMAL SLOPES ARE:
          IF(DABS(LN).EQ.0.0D0.AND.DABS(NN).EQ.0.0D0) THEN
              XN=0.0D0
          ELSE
              XN=DATAN2(LN,NN)
          END IF
          IF(DABS(MN).EQ.0.0D0.AND.DABS(NN).EQ.0.0D0) THEN
              YN=0.0D0
          ELSE
              YN=DATAN2(MN,NN)
          END IF
          IF(XN.GE.PII) XN=XN-(TWOPII)
          IF(YN.GE.PII) YN=YN-(TWOPII)
          IF(XN.LT.PII) XN=XN+(TWOPII)
          IF(YN.LT.PII) YN=YN+(TWOPII)
C     ANGLE OF INCIDENCE IS:
          IF(DABS(L).EQ.0.0D0.AND.DABS(N).EQ.0.0D0) THEN
              X=0.0D0
          ELSE
              X=DATAN2(L,N)
          END IF
          IF(DABS(M).EQ.0.0D0.AND.DABS(N).EQ.0.0D0) THEN
              Y=0.0D0
          ELSE
              Y=DATAN2(M,N)
          END IF
          IF(X.GE.PII) X=X-(TWOPII)
          IF(Y.GE.PII) Y=Y-(TWOPII)
          IF(X.LT.PII) X=X+(TWOPII)
          IF(Y.LT.PII) Y=Y+(TWOPII)
          IX=X-XN
          IY=Y-YN
C     ANGLE OF DIFFRACTION IS:
          IF(DABS(L1).EQ.0.0D0.AND.DABS(N1).EQ.0.0D0) THEN
              X1=0.0D0
          ELSE
              X1=DATAN2(L1,N1)
          END IF
          IF(DABS(M1).EQ.0.0D0.AND.DABS(N1).EQ.0.0D0) THEN
              Y1=0.0D0
          ELSE
              Y1=DATAN2(M1,N1)
          END IF
          IF(X1.GE.PII) X1=X1-(TWOPII)
          IF(Y1.GE.PII) Y1=Y1-(TWOPII)
          IF(X1.LT.PII) X1=X1+(TWOPII)
          IF(Y1.LT.PII) Y1=Y1+(TWOPII)
          IXP=DSIN(X1*DCOS(XN))+DSIN(IX)
          IYP=DSIN(Y1*DCOS(YN))+DSIN(IY)
          IF(IXP.GT.1.0D0) IXP=1.0D0
          IF(IYP.GT.1.0D0) IYP=1.0D0
          IF(IXP.LT.-1.0D0) IXP=-1.0D0
          IF(IYP.LT.-1.0D0) IYP=-1.0D0
          IXP=DASIN(IXP)
          IYP=DASIN(IYP)
          IF(IXP.GE.PII) IXP=IXP-(TWOPII)
          IF(IYP.GE.PII) IYP=IYP-(TWOPII)
          IF(IXP.LT.PII) IXP=IXP+(TWOPII)
          IF(IYP.LT.PII) IYP=IYP+(TWOPII)
          LT=DTAN(IXP+XN)
          MT=DTAN(IYP+YN)
          NT=NV*DSQRT((1.0D0)/(1.0D0+(LT**2)+(MT**2)))
          L=LT*NT
          M=MT*NT
          N=NV*DSQRT(1.0D0-(L**2)-(M**2))
          RETURN
      END
C SUB PH11.FOR
      SUBROUTINE PH11(L1,M1,XX,YY,PHA)
C
          IMPLICIT NONE
C
          REAL*8 WAVEY,L1,M1,PHA,XX,YY
C
          INTEGER IR,KLI
C
          LOGICAL FUN
C
          COMMON/NEFER/KLI
C
          COMMON/COMFUN/FUN
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datmac.inc'
C
          INTEGER WWVN
          COMMON/WVPASS/WWVN
          IF(WVN.EQ.1) WAVEY=SYSTEM1(1)
          IF(WVN.EQ.2) WAVEY=SYSTEM1(2)
          IF(WVN.EQ.3) WAVEY=SYSTEM1(3)
          IF(WVN.EQ.4) WAVEY=SYSTEM1(4)
          IF(WVN.EQ.5) WAVEY=SYSTEM1(5)
          IF(WVN.EQ.6) WAVEY=SYSTEM1(71)
          IF(WVN.EQ.7) WAVEY=SYSTEM1(72)
          IF(WVN.EQ.8) WAVEY=SYSTEM1(73)
          IF(WVN.EQ.9) WAVEY=SYSTEM1(74)
          IF(WVN.EQ.10) WAVEY=SYSTEM1(75)
          REG(40)=REG(9)
          REG(9)=XX
          REG(10)=YY
          REG(11)=INR
C     THIS SURFACE AUTOMATICALLY USES MACRO FUNCTION FUN09
C     FOR THE INITIAL USER FUCTION TO BE CALLED
C     PHASE IS RETURNED (IN RADIANS) IN THE IZ-REGISTER
C     CHANGE IN THE X DERIVATIVE OF THE PHASE IS RETURNED IN THE IX-REGISTER
C     CHANGE IN THE Y DERIVATIVE OF THE PHASE IS RETURNED IN THE IY-REGISTER
C
C     COEFFICIENTS C1 TO C96 ARE USED BY THE USER IN THE
C     MACRO FUNCTION FUN9 AND ANY MACRO FUNCTION CALLED BY FUN09.
C     THEY ARE STORED INITIALLY IN GENERAL PURPOSE STORAGE REGISTERS
C     NUMBERS 201 TO 248 AND ACCESSED BY THE USER VIA RCL COMMANDS
          IR=MYI
          GPREG(201)=FTFL01(1,IR)
          GPREG(202)=FTFL01(2,IR)
          GPREG(203)=FTFL01(3,IR)
          GPREG(204)=FTFL01(4,IR)
          GPREG(205)=FTFL01(5,IR)
          GPREG(206)=FTFL01(6,IR)
          GPREG(207)=FTFL01(7,IR)
          GPREG(208)=FTFL01(8,IR)
          GPREG(209)=FTFL01(9,IR)
          GPREG(210)=FTFL01(10,IR)
          GPREG(211)=FTFL01(11,IR)
          GPREG(212)=FTFL01(12,IR)
          GPREG(213)=FTFL01(13,IR)
          GPREG(214)=FTFL01(14,IR)
          GPREG(215)=FTFL01(15,IR)
          GPREG(216)=FTFL01(16,IR)
          GPREG(217)=FTFL01(17,IR)
          GPREG(218)=FTFL01(18,IR)
          GPREG(219)=FTFL01(19,IR)
          GPREG(220)=FTFL01(20,IR)
          GPREG(221)=FTFL01(21,IR)
          GPREG(222)=FTFL01(22,IR)
          GPREG(223)=FTFL01(23,IR)
          GPREG(224)=FTFL01(24,IR)
          GPREG(225)=FTFL01(25,IR)
          GPREG(226)=FTFL01(26,IR)
          GPREG(227)=FTFL01(27,IR)
          GPREG(228)=FTFL01(28,IR)
          GPREG(229)=FTFL01(29,IR)
          GPREG(230)=FTFL01(30,IR)
          GPREG(231)=FTFL01(31,IR)
          GPREG(232)=FTFL01(32,IR)
          GPREG(233)=FTFL01(33,IR)
          GPREG(234)=FTFL01(34,IR)
          GPREG(235)=FTFL01(35,IR)
          GPREG(236)=FTFL01(36,IR)
          GPREG(237)=FTFL01(37,IR)
          GPREG(238)=FTFL01(38,IR)
          GPREG(239)=FTFL01(39,IR)
          GPREG(240)=FTFL01(40,IR)
          GPREG(241)=FTFL01(41,IR)
          GPREG(242)=FTFL01(42,IR)
          GPREG(243)=FTFL01(43,IR)
          GPREG(244)=FTFL01(44,IR)
          GPREG(245)=FTFL01(45,IR)
          GPREG(246)=FTFL01(46,IR)
          GPREG(247)=FTFL01(47,IR)
          GPREG(248)=FTFL01(48,IR)
          GPREG(249)=FTFL01(49,IR)
          GPREG(250)=FTFL01(50,IR)
          GPREG(251)=FTFL01(51,IR)
          GPREG(252)=FTFL01(52,IR)
          GPREG(253)=FTFL01(53,IR)
          GPREG(254)=FTFL01(54,IR)
          GPREG(255)=FTFL01(55,IR)
          GPREG(256)=FTFL01(56,IR)
          GPREG(257)=FTFL01(57,IR)
          GPREG(258)=FTFL01(58,IR)
          GPREG(259)=FTFL01(59,IR)
          GPREG(260)=FTFL01(60,IR)
          GPREG(261)=FTFL01(61,IR)
          GPREG(262)=FTFL01(62,IR)
          GPREG(263)=FTFL01(63,IR)
          GPREG(264)=FTFL01(64,IR)
          GPREG(265)=FTFL01(65,IR)
          GPREG(266)=FTFL01(66,IR)
          GPREG(267)=FTFL01(67,IR)
          GPREG(268)=FTFL01(68,IR)
          GPREG(269)=FTFL01(69,IR)
          GPREG(270)=FTFL01(70,IR)
          GPREG(271)=FTFL01(71,IR)
          GPREG(272)=FTFL01(72,IR)
          GPREG(273)=FTFL01(73,IR)
          GPREG(274)=FTFL01(74,IR)
          GPREG(275)=FTFL01(75,IR)
          GPREG(276)=FTFL01(76,IR)
          GPREG(277)=FTFL01(77,IR)
          GPREG(278)=FTFL01(78,IR)
          GPREG(279)=FTFL01(79,IR)
          GPREG(280)=FTFL01(80,IR)
          GPREG(281)=FTFL01(81,IR)
          GPREG(282)=FTFL01(82,IR)
          GPREG(283)=FTFL01(83,IR)
          GPREG(284)=FTFL01(84,IR)
          GPREG(285)=FTFL01(85,IR)
          GPREG(286)=FTFL01(86,IR)
          GPREG(287)=FTFL01(87,IR)
          GPREG(288)=FTFL01(88,IR)
          GPREG(289)=FTFL01(89,IR)
          GPREG(290)=FTFL01(90,IR)
          GPREG(291)=FTFL01(91,IR)
          GPREG(292)=FTFL01(92,IR)
          GPREG(293)=FTFL01(93,IR)
          GPREG(294)=FTFL01(94,IR)
          GPREG(295)=FTFL01(95,IR)
          GPREG(296)=FTFL01(96,IR)
          IF(.NOT.FUNEXT(9)) THEN
C     NO FUN09 EXISTS, RETURN 0.0D0 AS THE Z CONTRIBUTION
              PHA=0.0D0
              L1=0.0D0
              M1=0.0D0
          ELSE
C     FUN9 EXISTS, RUN FUN9
              SAVE_KDP(12)=SAVEINPT(12)
              WC='FUN09'
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
              FMNW(1:5)=0.0D0
              FMDF(1:5)=1
              NESFUN(0:10)=.FALSE.
              KLI=9
              FUN=.TRUE.
              F26=1
              CALL FUNEXC
              F26=0
              REST_KDP(12)=RESTINPT(12)
              PHA=REG(15)*FTFL01(2,MYI)/WAVEY
              L1=REG(13)*FTFL01(2,MYI)/WAVEY
              M1=REG(14)*FTFL01(2,MYI)/WAVEY
          END IF
          RETURN
      END
C SUB PHAS13.FOR
      SUBROUTINE PHAS13
C
          IMPLICIT NONE
C
          REAL*8 DERX13,DERY13,FAZE13,MMYX,MMYY,WAVEY
C
          EXTERNAL FAZE13,DERX13,DERY13
C
!        INTEGER IR,KLI
C
!      LOGICAL FUN
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datmac.inc'
          INTEGER WWVN
          COMMON/WVPASS/WWVN
          IF(WVN.EQ.1) WAVEY=SYSTEM1(1)
          IF(WVN.EQ.2) WAVEY=SYSTEM1(2)
          IF(WVN.EQ.3) WAVEY=SYSTEM1(3)
          IF(WVN.EQ.4) WAVEY=SYSTEM1(4)
          IF(WVN.EQ.5) WAVEY=SYSTEM1(5)
          IF(WVN.EQ.6) WAVEY=SYSTEM1(71)
          IF(WVN.EQ.7) WAVEY=SYSTEM1(72)
          IF(WVN.EQ.8) WAVEY=SYSTEM1(73)
          IF(WVN.EQ.9) WAVEY=SYSTEM1(74)
          IF(WVN.EQ.10) WAVEY=SYSTEM1(75)
C
C     RECTANGULAR PHASE SURFACE
C
C       PROCEED WITH EVALUATION
          MMYX=MYX
          MMYY=MYY
C
          PHASE=PHASE+(FAZE13(MMYX,MMYY)*
     1    WAVEY/FTFL01(2,MYI))
          RETURN
      END
      FUNCTION FAZE13(ARG1,ARG2)
          IMPLICIT NONE
          REAL*8 ARG1,ARG2,FAZE13,FF4
          EXTERNAL FF4
          INTEGER I
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datmac.inc'
          IF(FTFL01(11,MYI).EQ.2.0D0) THEN
              FAZE13=0.0D0
              DO I=1,78
                  IF(FTFL01(I+11,MYI).NE.0.0D0) THEN
                      FAZE13=FAZE13+((FTFL01(I+11,MYI))*FF4(ARG1,ARG2,I))
                  END IF
              END DO
          END IF
          IF(FTFL01(11,MYI).EQ.3.0D0) THEN
              FAZE13=0.0D0
              DO I=1,78
                  IF(FTFL01(I+11,MYI).NE.0.0D0) THEN
                      FAZE13=FAZE13+((FTFL01(I+11,MYI))*FF4(DABS(ARG1),ARG2,I))
                  END IF
              END DO
          END IF
          IF(FTFL01(11,MYI).EQ.4.0D0) THEN
              FAZE13=0.0D0
              DO I=1,78
                  IF(FTFL01(I+11,MYI).NE.0.0D0) THEN
                      FAZE13=FAZE13+((FTFL01(I+11,MYI))*FF4(ARG1,DABS(ARG2),I))
                  END IF
              END DO
          END IF
          IF(FTFL01(11,MYI).EQ.5.0D0) THEN
              FAZE13=0.0D0
              DO I=1,78
                  IF(FTFL01(I+11,MYI).NE.0.0D0) THEN
                      FAZE13=FAZE13+((FTFL01(I+11,MYI))*
     1                FF4(DABS(ARG1),DABS(ARG2),I))
                  END IF
              END DO
          END IF
          RETURN
      END
      FUNCTION DERX13(X,Y,H,ERR)
C
C     SYMMETRIC DERIVATE FORMULA DESCRIBED IN NUMERICAL RECIPES 2ND ED.
C     PAGE 181, EQ. 5.7.7. THE CODE IS NOT FROM NUMERICAL RECIPES
C
          IMPLICIT NONE
          REAL*8 DERX13,ERR,H,X,Y,FAZE13,CON,CON2,BIG,SAFE
          EXTERNAL FAZE13
          INTEGER I,J,NTAB
          REAL*8 ERRT,FAC,HH,A(1:10,1:10)
C
          CON=1.4D0
          CON2=CON*CON
          BIG=1.0D30
          NTAB=10
          SAFE=2.0D0
C
          HH=H
          IF((FAZE13(X+HH,Y)-FAZE13(X-HH,Y)).EQ.0.0D0) THEN
              DERX13=0.0D0
          END IF
          A(1,1)=(FAZE13(X+HH,Y)-FAZE13(X-HH,Y))/(2.0D0*HH)
          ERR=BIG
          DO I=2,NTAB
              HH=HH/CON
              A(1,I)=(FAZE13(X+HH,Y)-FAZE13(X-HH,Y))/(2.0D0*HH)
              FAC=CON2
              DO J=2,I
                  A(J,I)=(A(J-1,I)*FAC-A(J-1,I-1))/(FAC-1.0D0)
                  FAC=CON2*FAC
                  ERRT=DMAX1(DABS(A(J,I)-A(J-1,I)),DABS(A(J,I)-A(J-1,I-1)))
                  IF(ERRT.LE.ERR) THEN
                      ERR=ERRT
                      DERX13=A(J,I)
                  END IF
              END DO
              IF(DABS(A(I,I)-A(I-1,I-1)).GE.SAFE*ERR) THEN
                  RETURN
              END IF
          END DO
          RETURN
      END
      FUNCTION DERY13(X,Y,H,ERR)
C
C     SYMMETRIC DERIVATE FORMULA DESCRIBED IN NUMERICAL RECIPES 2ND ED.
C     PAGE 181, EQ. 5.7.7. THE CODE IS NOT FROM NUMERICAL RECIPES
C
          IMPLICIT NONE
          REAL*8 DERY13,ERR,H,X,Y,FAZE13,CON,CON2,BIG,SAFE
          EXTERNAL FAZE13
          INTEGER I,J,NTAB
          REAL*8 ERRT,FAC,HH,A(1:10,1:10)
C
          CON=1.4D0
          CON2=CON*CON
          BIG=1.0D30
          NTAB=10
          SAFE=2.0D0
C
          HH=H
          IF((FAZE13(X,Y+HH)-FAZE13(X,Y-HH)).EQ.0.0D0) THEN
              DERY13=0.0D0
          END IF
          A(1,1)=(FAZE13(X,Y+HH)-FAZE13(X,Y-HH))/(2.0D0*HH)
          ERR=BIG
          DO I=2,NTAB
              HH=HH/CON
              A(1,I)=(FAZE13(X,Y+HH)-FAZE13(X,Y-HH))/(2.0D0*HH)
              FAC=CON2
              DO J=2,I
                  A(J,I)=(A(J-1,I)*FAC-A(J-1,I-1))/(FAC-1.0D0)
                  FAC=CON2*FAC
                  ERRT=DMAX1(DABS(A(J,I)-A(J-1,I)),DABS(A(J,I)-A(J-1,I-1)))
                  IF(ERRT.LE.ERR) THEN
                      ERR=ERRT
                      DERY13=A(J,I)
                  END IF
              END DO
              IF(DABS(A(I,I)-A(I-1,I-1)).GE.SAFE*ERR) THEN
                  RETURN
              END IF
          END DO
          RETURN
      END
C SUB PH12.FOR
      SUBROUTINE PH12(PX,PY,XX,YY)
C
          IMPLICIT NONE
C
          REAL*8 PX,PY,XX,YY,DERX12,DERY12
     1    ,DEL,ERR,WAVEY
C
          EXTERNAL DERX12,DERY12
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datmac.inc'
          INTEGER WWVN
          COMMON/WVPASS/WWVN
          IF(WVN.EQ.1) WAVEY=SYSTEM1(1)
          IF(WVN.EQ.2) WAVEY=SYSTEM1(2)
          IF(WVN.EQ.3) WAVEY=SYSTEM1(3)
          IF(WVN.EQ.4) WAVEY=SYSTEM1(4)
          IF(WVN.EQ.5) WAVEY=SYSTEM1(5)
          IF(WVN.EQ.6) WAVEY=SYSTEM1(71)
          IF(WVN.EQ.7) WAVEY=SYSTEM1(72)
          IF(WVN.EQ.8) WAVEY=SYSTEM1(73)
          IF(WVN.EQ.9) WAVEY=SYSTEM1(74)
          IF(WVN.EQ.10) WAVEY=SYSTEM1(75)
C
C     RADIAL PHASE SURFACE
C
C       PROCEED WITH EVALUATION
C
          DEL=DELSUR
          PX=(DERX12(XX,YY,DEL,ERR))*(WAVEY/FTFL01(2,MYI))
          PY=(DERY12(XX,YY,DEL,ERR))*(WAVEY/FTFL01(2,MYI))
          RETURN
      END
C SUB PH13.FOR
      SUBROUTINE PH13(PX,PY,XX,YY)
C
          IMPLICIT NONE
C
          REAL*8 PX,PY,XX,YY,DERX13,DERY13
     1    ,DEL,ERR,WAVEY
C
          EXTERNAL DERX12,DERY12
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datmac.inc'
          INTEGER WWVN
          COMMON/WVPASS/WWVN
          IF(WVN.EQ.1) WAVEY=SYSTEM1(1)
          IF(WVN.EQ.2) WAVEY=SYSTEM1(2)
          IF(WVN.EQ.3) WAVEY=SYSTEM1(3)
          IF(WVN.EQ.4) WAVEY=SYSTEM1(4)
          IF(WVN.EQ.5) WAVEY=SYSTEM1(5)
          IF(WVN.EQ.6) WAVEY=SYSTEM1(71)
          IF(WVN.EQ.7) WAVEY=SYSTEM1(72)
          IF(WVN.EQ.8) WAVEY=SYSTEM1(73)
          IF(WVN.EQ.9) WAVEY=SYSTEM1(74)
          IF(WVN.EQ.10) WAVEY=SYSTEM1(75)
C
C     RECT PHASE SURFACE
C
C       PROCEED WITH EVALUATION
C
          DEL=DELSUR
          PX=(DERX13(XX,YY,DEL,ERR))*(WAVEY/FTFL01(2,MYI))
          PY=(DERY13(XX,YY,DEL,ERR))*(WAVEY/FTFL01(2,MYI))
          RETURN
      END
C SUB PHAS12.FOR
      SUBROUTINE PHAS12
C
          IMPLICIT NONE
C
          REAL*8 DERX12,DERY12,FAZE12,MMYX,MMYY,WAVEY
C
          EXTERNAL FAZE12,DERX12,DERY12
C
!        INTEGER I,IR,KLI,II
C
!      LOGICAL FUN
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datmac.inc'
          INTEGER WWVN
          COMMON/WVPASS/WWVN
          IF(WVN.EQ.1) WAVEY=SYSTEM1(1)
          IF(WVN.EQ.2) WAVEY=SYSTEM1(2)
          IF(WVN.EQ.3) WAVEY=SYSTEM1(3)
          IF(WVN.EQ.4) WAVEY=SYSTEM1(4)
          IF(WVN.EQ.5) WAVEY=SYSTEM1(5)
          IF(WVN.EQ.6) WAVEY=SYSTEM1(71)
          IF(WVN.EQ.7) WAVEY=SYSTEM1(72)
          IF(WVN.EQ.8) WAVEY=SYSTEM1(73)
          IF(WVN.EQ.9) WAVEY=SYSTEM1(74)
          IF(WVN.EQ.10) WAVEY=SYSTEM1(75)
C
C     RADIAL PHASE SURFACE
C
C       PROCEED WITH EVALUATION
          MMYX=MYX
          MMYY=MYY
C
          PHASE=PHASE+(FAZE12(MMYX,MMYY)*
     1    WAVEY/FTFL01(2,MYI))
          RETURN
      END
      FUNCTION FAZE12(ARG1,ARG2)
          IMPLICIT NONE
          REAL*8 ARG1,ARG2,FAZE12,ARG
          INTEGER I
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datmac.inc'
          IF(FTFL01(11,MYI).EQ.1.0D0) THEN
              ARG=(ARG1**2)+(ARG2**2)
              FAZE12=0.0D0
              DO I=1,10
                  IF(ARG.EQ.0.0D0) THEN
                  ELSE
                      FAZE12=FAZE12+((FTFL01(I+11,MYI))*(ARG**I))
                  END IF
              END DO
          END IF
          IF(FTFL01(11,MYI).EQ.6.0D0) THEN
              ARG=DSQRT((ARG1**2)+(ARG2**2))
              FAZE12=0.0D0
              DO I=1,20
                  IF(ARG.EQ.0.0D0) THEN
                  ELSE
                      FAZE12=FAZE12+((FTFL01(I+11,MYI))*(ARG**I))
                  END IF
              END DO
          END IF
          RETURN
      END
      FUNCTION DERX12(X,Y,H,ERR)
C
C     SYMMETRIC DERIVATE FORMULA DESCRIBED IN NUMERICAL RECIPES 2ND ED.
C     PAGE 181, EQ. 5.7.7. THE CODE IS NOT FROM NUMERICAL RECIPES
C
          IMPLICIT NONE
          REAL*8 DERX12,ERR,H,X,Y,FAZE12,CON,CON2,BIG,SAFE
          EXTERNAL FAZE12
          INTEGER I,J,NTAB
          REAL*8 ERRT,FAC,HH,A(1:10,1:10)
C
          CON=1.4D0
          CON2=CON*CON
          BIG=1.0D30
          NTAB=10
          SAFE=2.0D0
C
          IF(X.LT.0.0D0) THEN
              HH=-H
          ELSE
              HH=H
          END IF
          IF((FAZE12(X+HH,Y)-FAZE12(X-HH,Y)).EQ.0.0D0) THEN
              DERX12=0.0D0
              RETURN
          END IF
          A(1,1)=(FAZE12(X+HH,Y)
     1    -FAZE12(X-HH,Y))/(2.0D0*HH)
          ERR=BIG
          DO I=2,NTAB
              HH=HH/CON
              A(1,I)=(FAZE12(X+HH,Y)
     1        -FAZE12(X-HH,Y))/(2.0D0*HH)
              FAC=CON2
              DO J=2,I
                  A(J,I)=(A(J-1,I)*FAC-A(J-1,I-1))/(FAC-1.0D0)
                  FAC=CON2*FAC
                  ERRT=DMAX1(DABS(A(J,I)-A(J-1,I)),DABS(A(J,I)-A(J-1,I-1)))
                  IF(ERRT.LE.ERR) THEN
                      ERR=ERRT
                      DERX12=A(J,I)
                  END IF
              END DO
              IF(DABS(A(I,I)-A(I-1,I-1)).GE.SAFE*ERR) THEN
                  RETURN
              END IF
          END DO
          RETURN
      END
      FUNCTION DERY12(X,Y,H,ERR)
C
C     SYMMETRIC DERIVATE FORMULA DESCRIBED IN NUMERICAL RECIPES 2ND ED.
C     PAGE 181, EQ. 5.7.7. THE CODE IS NOT FROM NUMERICAL RECIPES
C
          IMPLICIT NONE
          REAL*8 DERY12,ERR,H,X,Y,FAZE12,CON,CON2,BIG,SAFE
          EXTERNAL FAZE12
          INTEGER I,J,NTAB
          REAL*8 ERRT,FAC,HH,A(1:10,1:10)
C
          CON=1.4D0
          CON2=CON*CON
          BIG=1.0D30
          NTAB=10
          SAFE=2.0D0
C
          IF(Y.LT.0.0D0) THEN
              HH=-H
          ELSE
              HH=H
          END IF
          IF((FAZE12(X,Y+HH)-FAZE12(X,Y-HH)).EQ.0.0D0) THEN
              DERY12=0.0D0
              RETURN
          END IF
          A(1,1)=(FAZE12(X,Y+HH)
     1    -FAZE12(X,Y-HH))/(2.0D0*HH)
          ERR=BIG
          DO I=2,NTAB
              HH=HH/CON
              A(1,I)=(FAZE12(X,Y+HH)
     1        -FAZE12(X,Y-HH))/(2.0D0*HH)
              FAC=CON2
              DO J=2,I
                  A(J,I)=(A(J-1,I)*FAC-A(J-1,I-1))/(FAC-1.0D0)
                  FAC=CON2*FAC
                  ERRT=DMAX1(DABS(A(J,I)-A(J-1,I)),DABS(A(J,I)-A(J-1,I-1)))
                  IF(ERRT.LE.ERR) THEN
                      ERR=ERRT
                      DERY12=A(J,I)
                  END IF
              END DO
              IF(DABS(A(I,I)-A(I-1,I-1)).GE.SAFE*ERR) THEN
                  RETURN
              END IF
          END DO
          RETURN
      END
C SUB OLDSUR.FOR
      SUBROUTINE OLDSUR
C     RESTORES THE OBJECT,REFERENCE AND IMAGE SURFACES TO THERE
C     STORED VALUE1S
C     RESTORES THE OBJECT,REFERENCE AND IMAGE SURFACES TO THERE
C
          IMPLICIT NONE
C
          INCLUDE 'datlen.inc'
C
          NEWOBJ=OLDOBJ
          NEWREF=OLDREF
          NEWIMG=OLDIMG
C
          RETURN
      END
C SUB RESSUR.FOR
      SUBROUTINE RESSUR
C     SAVES THE CURRENT OBJECT,REFERENCE AND IMAGE SURFACES
C     SO THEY CAN BE RESTORED LATER
C
          IMPLICIT NONE
C
          INCLUDE 'datlen.inc'
C
          OLDOBJ=NEWOBJ
          OLDREF=NEWREF
          OLDIMG=NEWIMG
          NEWOBJ=0
          NEWREF=INT(SYSTEM1(25))
          NEWIMG=INT(SYSTEM1(20))
C
          RETURN
      END
      SUBROUTINE REALCOLR(I,ERRR)
C
C     DOES CALC FOR PACM,PACZ,SACM,SACZ,PLCM,PLCZ,SLCM,SLCZ
C     FOR GET.FOR AND CALCPRE.FOR
C
          IMPLICIT NONE
C
          REAL*8 VALUE1,NUM5,XRAY(1:4),YRAY(1:4)
     1    ,XFOB(1:4),YFOB(1:4),DDF1(1:4),DDF2(1:4),LAM(1:2),LAMCW
C
          INTEGER II,I,J
C
          LOGICAL ERRR,OLDLDIF,OLDLDIF2
C
          COMMON/GV/VALUE1,NUM5
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
C
          LAMCW=SYSTEM1(11)
C     SET COLORS PRIMARY
          IF(I.EQ.1.OR.I.EQ.2.OR.I.EQ.5.OR.I.EQ.6) THEN
C     PRIMARY COLOR
              LAM(1)=SYSTEM1(7)
              LAM(2)=SYSTEM1(8)
          END IF
C
C     SET COLORS SECONDARY
          IF(I.EQ.3.OR.I.EQ.4.OR.I.EQ.7.OR.I.EQ.8) THEN
C     SECONDARY COLOR
              LAM(1)=SYSTEM1(9)
              LAM(2)=SYSTEM1(10)
          END IF
C
C     SET FIELDS AND RAYS FOR AXIAL COLOR
          IF(I.GE.1.AND.I.LE.4) THEN
C     SET FIELDS
              DO J=1,4
                  XFOB(J)=0.0D0
                  YFOB(J)=0.0D0
              END DO
C     SET RAYS
              IF(I.EQ.1.OR.I.EQ.3) THEN
C     MARGINAL AXIAL
                  XRAY(1)=0.0D0
                  XRAY(2)=1.0D0
                  XRAY(3)=0.0D0
                  XRAY(4)=-1.0D0
                  YRAY(1)=1.0D0
                  YRAY(2)=0.0D0
                  YRAY(3)=-1.0D0
                  YRAY(4)=0.0D0
              END IF
              IF(I.EQ.2.OR.I.EQ.4) THEN
C     ZONAL AXIAL
                  XRAY(1)=0.0D0
                  XRAY(2)=0.7D0
                  XRAY(3)=0.0D0
                  XRAY(4)=-0.7D0
                  YRAY(1)=0.7D0
                  YRAY(2)=0.0D0
                  YRAY(3)=-0.7D0
                  YRAY(4)=0.0D0
              END IF
          END IF
C
C     SET FIELDS AND RAYS FOR LATERAL COLOR
          IF(I.GE.5.AND.I.LE.8) THEN
C     SET FIELDS
              IF(I.EQ.5.OR.I.EQ.7) THEN
C     MARGINAL FIELD
                  XFOB(1)=0.0D0
                  XFOB(2)=1.0D0
                  XFOB(3)=0.0D0
                  XFOB(4)=-1.0D0
                  YFOB(1)=1.0D0
                  YFOB(2)=0.0D0
                  YFOB(3)=-1.0D0
                  YFOB(4)=0.0D0
              END IF
              IF(I.EQ.6.OR.I.EQ.8) THEN
C     ZONAL FIELD
                  XFOB(1)=0.0D0
                  XFOB(2)=0.7D0
                  XFOB(3)=0.0D0
                  XFOB(4)=-0.7D0
                  YFOB(1)=0.7D0
                  YFOB(2)=0.0D0
                  YFOB(3)=-0.7D0
                  YFOB(4)=0.0D0
              END IF
C     SET RAYS
              DO J=1,4
                  XRAY(J)=0.0D0
                  YRAY(J)=0.0D0
              END DO
          END IF
          IF(I.GE.1.AND.I.LE.4) THEN
C     TRACE ONE FIELD AND FOUR RAY POSITIONS
C     TRACE CHIEF RAY AT CONTROL WAVELENGTH
C     DO THE FOB
              SAVE_KDP(1)=SAVEINPT(1)
              OLDLDIF2=LDIF2
              OLDLDIF=LDIF
              LDIF2=.FALSE.
              LDIF=.FALSE.
              WQ='        '
              SQ=0
              SST=0
              STI=0
              W1=0.0D0
              W2=0.0D0
              W3=0.0D0
              W4=LAMCW
              W5=0.0D0
              DF1=0
              DF2=0
              DF3=1
              DF4=0
              DF5=1
              S1=1
              S2=1
              S3=0
              S4=1
              S5=0
              SN=1
C     SET MSG TO FALSE
              MSG=.FALSE.
              WC='FOB     '
              CALL FFOB
              REST_KDP(1)=RESTINPT(1)
              IF(RAYCOD(1).NE.0) GO TO 200
              DO II=1,4
C NOW THE 4 RAYS AT THE WAVELENGTH PAIRS AND FORM
C     THE CHROMATIC DIFFERENCES
C     AT LAM1
                  SAVE_KDP(1)=SAVEINPT(1)
                  WQ='        '
                  SQ=0
                  SST=0
                  W1=YRAY(II)
                  W2=XRAY(II)
                  W3=LAM(1)
                  W4=0.0D0
                  W5=0.0D0
                  S1=1
                  S2=1
                  S3=1
                  S4=0
                  S5=0
                  DF1=0
                  DF2=0
                  DF3=0
                  DF4=1
                  DF5=1
                  SN=1
                  WC='RAY     '
                  NOCOAT=.TRUE.
                  GRASET=.FALSE.
                  DXFSET=.FALSE.
                  CALL RRAY
                  REST_KDP(1)=RESTINPT(1)
                  IF(SYSTEM1(30).LE.2.0D0) THEN
C     FOCAL
                      DDF1(II)=DABS(RAYRAY(1,NEWIMG))
                      DDF2(II)=DABS(RAYRAY(2,NEWIMG))
                  ELSE
C     AFOCAL
                      DDF1(II)=DABS(RAYRAY(11,NEWIMG))
                      DDF2(II)=DABS(RAYRAY(12,NEWIMG))
                  END IF
                  REST_KDP(1)=RESTINPT(1)
                  IF(RAYCOD(1).NE.0) GO TO 200
C     AT LAM(2)
                  SAVE_KDP(1)=SAVEINPT(1)
                  WQ='        '
                  SQ=0
                  SST=0
                  W1=YRAY(II)
                  W2=XRAY(II)
                  W3=LAM(2)
                  W4=0.0D0
                  W5=0.0D0
                  S1=1
                  S2=1
                  S3=1
                  S4=0
                  S5=0
                  DF1=0
                  DF2=0
                  DF3=0
                  DF4=1
                  DF5=1
                  SN=1
                  WC='RAY     '
                  NOCOAT=.TRUE.
                  GRASET=.FALSE.
                  DXFSET=.FALSE.
                  CALL RRAY
                  REST_KDP(1)=RESTINPT(1)
                  IF(SYSTEM1(30).LE.2.0D0) THEN
C     FOCAL
                      DDF1(II)=DABS(DDF1(II)-DABS(RAYRAY(1,NEWIMG)))
                      DDF2(II)=DABS(DDF2(II)-DABS(RAYRAY(2,NEWIMG)))
                  ELSE
C     AFOCAL
                      DDF1(II)=DABS(DDF1(II)-DABS(RAYRAY(11,NEWIMG)))
                      DDF2(II)=DABS(DDF2(II)-DABS(RAYRAY(12,NEWIMG)))
                      IF(REAL(DDF1(II)).GT.REAL(PII)) DDF1(II)=DDF1(II)-(TWOPII)
                      IF(REAL(DDF1(II)).EQ.REAL(TWOPII)) DDF1(II)=0.0D0
                      IF(REAL(DDF2(II)).GT.REAL(PII)) DDF2(II)=DDF2(II)-(TWOPII)
                      IF(REAL(DDF2(II)).EQ.REAL(TWOPII)) DDF2(II)=0.0D0
                  END IF
                  REST_KDP(1)=RESTINPT(1)
                  IF(RAYCOD(1).NE.0) GO TO 200
              END DO
          END IF
C
          IF(I.GE.5.AND.I.LE.8) THEN
C     TRACE ONE CENTRAL RAY AND FOUR FIELD POSITIONS
C     TRACE CHIEF RAYS AT CONTROL WAVELENGTH
              DO II=1,4
C     DO THE FOB
                  SAVE_KDP(1)=SAVEINPT(1)
                  OLDLDIF2=LDIF2
                  OLDLDIF=LDIF
                  LDIF2=.FALSE.
                  LDIF=.FALSE.
                  WQ='        '
                  SQ=0
                  SST=0
                  STI=0
                  W1=YFOB(II)
                  W2=XFOB(II)
                  W3=0.0D0
                  W4=LAMCW
                  W5=0.0D0
                  DF1=0
                  DF2=0
                  DF3=1
                  DF4=0
                  DF5=1
                  S1=1
                  S2=1
                  S3=0
                  S4=1
                  S5=0
                  SN=1
C     SET MSG TO FALSE
                  MSG=.FALSE.
                  WC='FOB     '
                  CALL FFOB
                  REST_KDP(1)=RESTINPT(1)
                  IF(RAYCOD(1).NE.0) GO TO 200
C NOW THE ONE CENTRAL RAY AT THE WAVELENGTH PAIRS AND FORM
C     THE CHROMATIC DIFFERENCES
C     AT LAM1
                  SAVE_KDP(1)=SAVEINPT(1)
                  WQ='        '
                  SQ=0
                  SST=0
                  W1=0.0D0
                  W2=0.0D0
                  W3=LAM(1)
                  W4=0.0D0
                  W5=0.0D0
                  S1=1
                  S2=1
                  S3=1
                  S4=0
                  S5=0
                  DF1=0
                  DF2=0
                  DF3=0
                  DF4=1
                  DF5=1
                  SN=1
                  WC='RAY     '
                  NOCOAT=.TRUE.
                  GRASET=.FALSE.
                  DXFSET=.FALSE.
                  CALL RRAY
                  REST_KDP(1)=RESTINPT(1)
                  IF(SYSTEM1(30).LE.2.0D0) THEN
C     FOCAL
                      DDF1(II)=DABS(RAYRAY(1,NEWIMG))
                      DDF2(II)=DABS(RAYRAY(2,NEWIMG))
                  ELSE
C     AFOCAL
                      DDF1(II)=DABS(RAYRAY(11,NEWIMG))
                      DDF2(II)=DABS(RAYRAY(12,NEWIMG))
                  END IF
                  REST_KDP(1)=RESTINPT(1)
                  IF(RAYCOD(1).NE.0) GO TO 200
C     AT LAM(2)
                  SAVE_KDP(1)=SAVEINPT(1)
                  WQ='        '
                  SQ=0
                  SST=0
                  W1=0.0D0
                  W2=0.0D0
                  W3=LAM(2)
                  W4=0.0D0
                  W5=0.0D0
                  S1=1
                  S2=1
                  S3=1
                  S4=0
                  S5=0
                  DF1=0
                  DF2=0
                  DF3=0
                  DF4=1
                  DF5=1
                  SN=1
                  WC='RAY     '
                  NOCOAT=.TRUE.
                  GRASET=.FALSE.
                  DXFSET=.FALSE.
                  CALL RRAY
                  REST_KDP(1)=RESTINPT(1)
                  IF(SYSTEM1(30).LE.2.0D0) THEN
C     FOCAL
                      DDF1(II)=DABS(DDF1(II)-DABS(RAYRAY(1,NEWIMG)))
                      DDF2(II)=DABS(DDF2(II)-DABS(RAYRAY(2,NEWIMG)))
                  ELSE
C     AFOCAL
                      DDF1(II)=DABS(DDF1(II)-DABS(RAYRAY(11,NEWIMG)))
                      DDF2(II)=DABS(DDF2(II)-DABS(RAYRAY(12,NEWIMG)))
                      IF(REAL(DDF1(II)).GT.REAL(PII)) DDF1(II)=DDF1(II)-(TWOPII)
                      IF(REAL(DDF1(II)).EQ.REAL(TWOPII)) DDF1(II)=0.0D0
                      IF(REAL(DDF2(II)).GT.REAL(PII)) DDF2(II)=DDF2(II)-(TWOPII)
                      IF(REAL(DDF2(II)).EQ.REAL(TWOPII)) DDF2(II)=0.0D0
                  END IF
                  REST_KDP(1)=RESTINPT(1)
                  IF(RAYCOD(1).NE.0) GO TO 200
              END DO
          END IF
C
C     NOW SUM THE ABSOLUTE VALUE1S OF THE ABERRATIONS
          VALUE1=0.0D0
          DO II=1,4
              VALUE1=VALUE1+DDF1(II)+DDF2(II)
          END DO
          ERRR=.FALSE.
          RETURN
C
C     ERROR DROP THROUGH POINT
 200      CONTINUE
          ERRR=.TRUE.
          VALUE1=0.0D0
          RETURN
      END
C SUB PRREF.FOR
      SUBROUTINE PRREF
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE PRREF. THIS SUBROUTINE IMPLEMENTS
C       THE PRINTOUT OF THE CURRENT REFERENCE RAY RAYTRACE VALUE1S
C
          INTEGER SF,I,IWAVNM
C
          REAL*8 TREFA,TREFB
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
          IWAVNM=INT(WVN)
C
          IF(.NOT.REFEXT) THEN
C       NO RAY DATA TO OUTPUT
              WRITE(OUTLYNE,*)
     1        'NO CURRENT REFERENCE RAY DATA EXISTS TO OUTPUT'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF

C
          TREFA=0.0D0
          TREFB=0.0D0
C
C       THE REFRY COMMAND ACCEPTS QUALIFIER OR NUMERIC
C       INPUT IN AN EITHER OR MODE. IT DOES NOT ACCEPT STING
C       INPUT. THE VALID QUALIFIERS ARE "ALL" AND "OBJ" AND "OB"
C       THE VALID NUMERIC INPUT IS THE SURFACE NUMBER FOR
C       WHICH IT IS DESIRED TO PRODUCE OUTPUT.
C
          IF(SST.EQ.1.OR.S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
              WRITE(OUTLYNE,*)
     1        '"PRREF" ONLY TAKES QUALIFIER OR NUMERIC WORD #1 INPUT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SQ.EQ.1.AND.S1.EQ.1.AND.WQ.NE.'P') THEN
              WRITE(OUTLYNE,*)
     1        '"PRREF" ONLY TAKES QUALIFIER OR NUMERIC WORD #1 INPUT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'BUT NOT BOTH'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SYSTEM1(20).EQ.0.0D0) THEN
              WRITE(OUTLYNE,*)'LENS SYSTEM HAS NO SURFACES'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'NO REFERENCE RAY DATA EXISTS'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SQ.EQ.1.AND.WQ.EQ.'ALL') THEN
              SF=NEWIMG
              WRITE(OUTLYNE,5001) INT(F12)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,5002)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,2501)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,5000)
              CALL SHOWIT(0)
              DO 10 I=NEWOBJ,SF
                  TREFA=DTAN(REFRY(11,I))
                  TREFB=DTAN(REFRY(12,I))
                  WRITE(OUTLYNE,2000)I,REFRY(1,I),REFRY(2,I)
     1            ,REFRY(3,I),TREFA,TREFB
                  CALL SHOWIT(0)
 10           CONTINUE
              RETURN
          END IF
          IF(SQ.EQ.1.AND.WQ.EQ.'OBJ'.OR.SQ.EQ.1.AND.WQ.EQ.'OB') THEN
              SF=NEWOBJ
              TREFA=DTAN(REFRY(11,SF))
              TREFB=DTAN(REFRY(12,SF))
              IF(HEADIN) WRITE(OUTLYNE,5000)
              IF(HEADIN) CALL SHOWIT(0)
              WRITE(OUTLYNE,1500) SF,REFRY(1,SF),REFRY(2,SF)
     1        ,REFRY(3,SF),TREFA,TREFB
              CALL SHOWIT(0)
              RETURN
          END IF
          IF(WQ.NE.'OBJ'.AND.
     1    WQ.NE.'ALL'.AND.WQ.NE.'OB'
     2    .AND.WQ.NE.'P'.AND.SQ.EQ.1) THEN
              WRITE(OUTLYNE,*)'INVALID QUALIFIER WORD'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SQ.EQ.0.AND.DF1.EQ.1) THEN
C       OUTPUT IMAGE SURFACE
              SF=NEWIMG
              TREFA=DTAN(REFRY(11,SF))
              TREFB=DTAN(REFRY(12,SF))
              IF(HEADIN) WRITE(OUTLYNE,5000)
              IF(HEADIN) CALL SHOWIT(0)
              WRITE(OUTLYNE,1500) SF,REFRY(1,SF),REFRY(2,SF),
     1        REFRY(3,SF),TREFA,TREFB
              CALL SHOWIT(0)
              RETURN
          END IF
          IF(SQ.EQ.0.AND.DF1.NE.1) THEN
              I=INT(W1)
              SF=NEWIMG
              IF(I.GT.SF.OR.I.LT.NEWOBJ) THEN
                  WRITE(OUTLYNE,*)'SURFACE NUMBER BEYOND LEGAL RANGE'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              TREFA=DTAN(REFRY(11,I))
              TREFB=DTAN(REFRY(12,I))
              IF(HEADIN) WRITE(OUTLYNE,5000)
              IF(HEADIN) CALL SHOWIT(0)
              WRITE(OUTLYNE,1500)I,REFRY(1,I),REFRY(2,I),
     1        REFRY(3,I),TREFA,TREFB
              CALL SHOWIT(0)
              RETURN
          END IF
          IF(WQ.EQ.'P'.AND.DF1.EQ.1) THEN
C       OUTPUT IMAGE SURFACE
              WRITE(OUTLYNE,1600)IWAVNM,RELY,RELX
              CALL SHOWIT(0)
 1600         FORMAT('RAY AT WAVL # ',I1,
     1        ' (REL AP Y, REL AP X) = ( ',G13.6,', ',G13.6,' )')
              SF=NEWIMG
              TREFA=DTAN(REFRY(11,SF))
              TREFB=DTAN(REFRY(12,SF))
              IF(HEADIN) WRITE(OUTLYNE,5000)
              IF(HEADIN) CALL SHOWIT(0)
              WRITE(OUTLYNE,1500) SF,REFRY(1,SF),REFRY(2,SF),
     1        REFRY(3,SF),TREFA,TREFB
              CALL SHOWIT(0)
              RETURN
          END IF
          IF(WQ.EQ.'P'.AND.DF1.NE.1) THEN
              WRITE(OUTLYNE,1600)IWAVNM,RELY,RELX
              CALL SHOWIT(0)
              I=INT(W1)
              SF=NEWIMG
              IF(I.GT.SF.OR.I.LT.NEWOBJ) THEN
                  WRITE(OUTLYNE,*)'SURFACE NUMBER BEYOND LEGAL RANGE'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              TREFA=DTAN(REFRY(11,I))
              TREFB=DTAN(REFRY(12,I))
              IF(HEADIN) WRITE(OUTLYNE,5000)
              IF(HEADIN) CALL SHOWIT(0)
              WRITE(OUTLYNE,1500)I,REFRY(1,I),REFRY(2,I),
     1        REFRY(3,I),TREFA,TREFB
              CALL SHOWIT(0)
              RETURN
          END IF
 1500     FORMAT(I3,2X,G13.6,1X,G13.6,1X,G13.6,1X,
     1    G11.4,1X,G11.4)
 2000     FORMAT(I3,2X,G13.6,1X,G13.6,1X,G13.6,1X,
     1    G11.4,1X,G11.4)
 5000     FORMAT('SURF',6X,'X',13X,'Y',13X,'Z',
     1    9X,'XA(TAN)',5X,'YA(TAN)')
 5001     FORMAT('CURRENT REFERENCE RAY: RAYTRACE DATA ',
     1    ' - (CFG #',I2,')')
 5002     FORMAT(
     1    'SLOPE ANGLES ARE MEASURED WITH RESPECT TO THE Z-AXIS')
 2501     FORMAT(1X)
      END
C SUB PRDIFF.FOR
      SUBROUTINE PRDIFF
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE PRDIFF. THIS SUBROUTINE IMPLEMENTS
C       THE PRINTOUT OF THE CURRENT DIFFERENTIAL RAY RAYTRACE VALUE1S
C
C     SUPPORTS COMMANDS PRDIFFXM, PRDIFFYM, PRDIFFXR AND PRDIFFYR
C
          INTEGER SF,I,IWAVNM
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
          IWAVNM=INT(WVN)
C
          IF(.NOT.REFEXT.OR..NOT.RAYEXT) THEN
C       NO RAY DATA TO OUTPUT
              WRITE(OUTLYNE,*)
     1        'NO CURRENT DIFFERENTIAL RAY DATA EXISTS TO OUTPUT'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(.NOT.LDIF) THEN
C       NO RAY DATA TO OUTPUT
              WRITE(OUTLYNE,*)
     1        'NO CURRENT DIFFERENTIAL RAY DATA EXISTS TO OUTPUT'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF

C       THESE COMMANDS ACCEPT QUALIFIER OR NUMERIC
C       INPUT IN AN EITHER OR MODE. IT DOES NOT ACCEPT STING
C       INPUT. THE VALID QUALIFIERS ARE "ALL" AND "OBJ" AND "OB"
C       THE VALID NUMERIC INPUT IS THE SURFACE NUMBER FOR
C       WHICH IT IS DESIRED TO PRODUCE OUTPUT.
C
          IF(SST.EQ.1.OR.S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
              WRITE(OUTLYNE,*)
     1        '"',WC(1:8),'" ONLY TAKES QUALIFIER OR NUMERIC WORD #1 INPUT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SQ.EQ.1.AND.S1.EQ.1.AND.WQ.NE.'P') THEN
              WRITE(OUTLYNE,*)
     1        '"',WQ(1:8),'" ONLY TAKES QUALIFIER OR NUMERIC WORD #1 INPUT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'BUT NOT BOTH'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SYSTEM1(20).EQ.0.0D0) THEN
              WRITE(OUTLYNE,*)'LENS SYSTEM HAS NO SURFACES'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'NO REFERENCE RAY DATA EXISTS'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SQ.EQ.1.AND.WQ.EQ.'ALL') THEN
              SF=NEWIMG
              IF(WC.EQ.'PRDIFFXM') WRITE(OUTLYNE,5001) INT(F12)
              IF(WC.EQ.'PRDIFFYM') WRITE(OUTLYNE,5002) INT(F12)
              IF(WC.EQ.'PRDIFFXR') WRITE(OUTLYNE,5003) INT(F12)
              IF(WC.EQ.'PRDIFFYR') WRITE(OUTLYNE,5004) INT(F12)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,2501)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,5000)
              CALL SHOWIT(0)
              DO 10 I=NEWOBJ,SF
                  IF(WC.EQ.'PRDIFFXM')
     1               WRITE(OUTLYNE,2000)I,DIFF(1,I),DIFF(2,I)
     1              ,DIFF(3,I),DIFF(4,I),DIFF(5,I),DIFF(6,I)
                  IF(WC.EQ.'PRDIFFYM')
     1               WRITE(OUTLYNE,2000)I,DIFF(7,I),DIFF(8,I)
     1              ,DIFF(9,I),DIFF(10,I),DIFF(11,I),DIFF(12,I)
                  IF(WC.EQ.'PRDIFFXR')
     1               WRITE(OUTLYNE,2000)I,RFDIFF(1,I),RFDIFF(2,I)
     1              ,RFDIFF(3,I),RFDIFF(4,I),RFDIFF(5,I),RFDIFF(6,I)
                  IF(WC.EQ.'PRDIFFYR')
     1               WRITE(OUTLYNE,2000)I,RFDIFF(7,I),RFDIFF(8,I)
     1              ,RFDIFF(9,I),RFDIFF(10,I),RFDIFF(11,I),RFDIFF(12,I)
                  CALL SHOWIT(0)
 10           CONTINUE
              RETURN
          END IF
          IF(SQ.EQ.1.AND.WQ.EQ.'OBJ'.OR.SQ.EQ.1.AND.WQ.EQ.'OB') THEN
              SF=NEWOBJ
              IF(HEADIN) WRITE(OUTLYNE,5000)
              IF(HEADIN) CALL SHOWIT(0)
              IF(WC.EQ.'PRDIFFXM')
     1           WRITE(OUTLYNE,2000)SF,DIFF(1,SF),DIFF(2,SF)
     1          ,DIFF(3,SF),DIFF(4,SF),DIFF(5,SF),DIFF(6,SF)
              IF(WC.EQ.'PRDIFFYM')
     1           WRITE(OUTLYNE,2000)SF,DIFF(7,SF),DIFF(8,SF)
     1          ,DIFF(9,SF),DIFF(10,SF),DIFF(11,SF),DIFF(12,SF)
              IF(WC.EQ.'PRDIFFXR')
     1           WRITE(OUTLYNE,2000)SF,RFDIFF(1,SF),RFDIFF(2,SF)
     1          ,RFDIFF(3,SF),RFDIFF(4,SF),RFDIFF(5,SF),RFDIFF(6,SF)
              IF(WC.EQ.'PRDIFFYR')
     1           WRITE(OUTLYNE,2000)SF,RFDIFF(7,SF),RFDIFF(8,SF)
     1          ,RFDIFF(9,SF),RFDIFF(10,SF),RFDIFF(11,SF),RFDIFF(12,SF)
              CALL SHOWIT(0)
              RETURN
          END IF
          IF(SQ.EQ.1.AND.WQ.NE.'OBJ'.AND.
     1    WQ.NE.'ALL'.AND.WQ.NE.'OB'.AND.WQ.NE.'P') THEN
              WRITE(OUTLYNE,*)'INVALID QUALIFIER WORD'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SQ.EQ.0.AND.DF1.EQ.1) THEN
C       OUTPUT IMAGE SURFACE
              SF=NEWIMG
              IF(HEADIN) WRITE(OUTLYNE,5000)
              IF(HEADIN) CALL SHOWIT(0)
              IF(WC.EQ.'PRDIFFXM')
     1           WRITE(OUTLYNE,2000)SF,DIFF(1,SF),DIFF(2,SF)
     1          ,DIFF(3,SF),DIFF(4,SF),DIFF(5,SF),DIFF(6,SF)
              IF(WC.EQ.'PRDIFFYM')
     1           WRITE(OUTLYNE,2000)SF,DIFF(7,SF),DIFF(8,SF)
     1          ,DIFF(9,SF),DIFF(10,SF),DIFF(11,SF),DIFF(12,SF)
              IF(WC.EQ.'PRDIFFXR')
     1           WRITE(OUTLYNE,2000)SF,RFDIFF(1,SF),RFDIFF(2,SF)
     1          ,RFDIFF(3,SF),RFDIFF(4,SF),RFDIFF(5,SF),RFDIFF(6,SF)
              IF(WC.EQ.'PRDIFFYR')
     1           WRITE(OUTLYNE,2000)SF,RFDIFF(7,SF),RFDIFF(8,SF)
     1          ,RFDIFF(9,SF),RFDIFF(10,SF),RFDIFF(11,SF),RFDIFF(12,SF)
              CALL SHOWIT(0)
              RETURN
          END IF
          IF(SQ.EQ.0.AND.DF1.NE.1) THEN
              I=INT(W1)
              SF=NEWIMG
              IF(I.GT.SF.OR.I.LT.NEWOBJ) THEN
                  WRITE(OUTLYNE,*)'SURFACE NUMBER BEYOND LEGAL RANGE'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(HEADIN) WRITE(OUTLYNE,5000)
              IF(HEADIN) CALL SHOWIT(0)
              IF(WC.EQ.'PRDIFFXM')
     1           WRITE(OUTLYNE,2000)I,DIFF(1,I),DIFF(2,I)
     1          ,DIFF(3,I),DIFF(4,I),DIFF(5,I),DIFF(6,I)
              IF(WC.EQ.'PRDIFFYM')
     1           WRITE(OUTLYNE,2000)I,DIFF(7,I),DIFF(8,I)
     1          ,DIFF(9,I),DIFF(10,I),DIFF(11,I),DIFF(12,I)
              IF(WC.EQ.'PRDIFFXR')
     1           WRITE(OUTLYNE,2000)I,RFDIFF(1,I),RFDIFF(2,I)
     1          ,RFDIFF(3,I),RFDIFF(4,I),RFDIFF(5,I),RFDIFF(6,I)
              IF(WC.EQ.'PRDIFFYR')
     1           WRITE(OUTLYNE,2000)I,RFDIFF(7,I),RFDIFF(8,I)
     1          ,RFDIFF(9,I),RFDIFF(10,I),RFDIFF(11,I),RFDIFF(12,I)
              CALL SHOWIT(0)
              RETURN
          END IF
          IF(WQ.EQ.'P'.AND.DF1.EQ.1) THEN
C       OUTPUT IMAGE SURFACE
              WRITE(OUTLYNE,1600)IWAVNM,RELY,RELX
              CALL SHOWIT(0)
 1600         FORMAT('RAY AT WAVL # ',I1,
     1        ' (REL AP Y, REL AP X) = ( ',G13.6,', ',G13.6,' )')
              SF=NEWIMG
              IF(HEADIN) WRITE(OUTLYNE,5000)
              IF(HEADIN) CALL SHOWIT(0)
              IF(WC.EQ.'PRDIFFXM')
     1           WRITE(OUTLYNE,2000)SF,DIFF(1,SF),DIFF(2,SF)
     1          ,DIFF(3,SF),DIFF(4,SF),DIFF(5,SF),DIFF(6,SF)
              IF(WC.EQ.'PRDIFFYM')
     1           WRITE(OUTLYNE,2000)SF,DIFF(7,SF),DIFF(8,SF)
     1          ,DIFF(9,SF),DIFF(10,SF),DIFF(11,SF),DIFF(12,SF)
              IF(WC.EQ.'PRDIFFXR')
     1           WRITE(OUTLYNE,2000)SF,RFDIFF(1,SF),RFDIFF(2,SF)
     1          ,RFDIFF(3,SF),RFDIFF(4,SF),RFDIFF(5,SF),RFDIFF(6,SF)
              IF(WC.EQ.'PRDIFFYR')
     1           WRITE(OUTLYNE,2000)SF,RFDIFF(7,SF),RFDIFF(8,SF)
     1          ,RFDIFF(9,SF),RFDIFF(10,SF),RFDIFF(11,SF),RFDIFF(12,SF)
              CALL SHOWIT(0)
              RETURN
          END IF
          IF(WQ.EQ.'P'.AND.DF1.NE.1) THEN
              WRITE(OUTLYNE,1600)IWAVNM,RELY,RELX
              CALL SHOWIT(0)
              I=INT(W1)
              SF=NEWIMG
              IF(I.GT.SF.OR.I.LT.NEWOBJ) THEN
                  WRITE(OUTLYNE,*)'SURFACE NUMBER BEYOND LEGAL RANGE'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(HEADIN) WRITE(OUTLYNE,5000)
              IF(HEADIN) CALL SHOWIT(0)
              IF(WC.EQ.'PRDIFFXM')
     1           WRITE(OUTLYNE,2000)I,DIFF(1,I),DIFF(2,I)
     1          ,DIFF(3,I),DIFF(4,I),DIFF(5,I),DIFF(6,I)
              IF(WC.EQ.'PRDIFFYM')
     1           WRITE(OUTLYNE,2000)I,DIFF(7,I),DIFF(8,I)
     1          ,DIFF(9,I),DIFF(10,I),DIFF(11,I),DIFF(12,I)
              IF(WC.EQ.'PRDIFFXR')
     1           WRITE(OUTLYNE,2000)I,RFDIFF(1,I),RFDIFF(2,I)
     1          ,RFDIFF(3,I),RFDIFF(4,I),RFDIFF(5,I),RFDIFF(6,I)
              IF(WC.EQ.'PRDIFFYR')
     1           WRITE(OUTLYNE,2000)I,RFDIFF(7,I),RFDIFF(8,I)
     1          ,RFDIFF(9,I),RFDIFF(10,I),RFDIFF(11,I),RFDIFF(12,I)
              CALL SHOWIT(0)
              RETURN
          END IF
! 1500   FORMAT(I3,2X,G11.4,1X,G11.4,1X,G11.4,1X,
!     1  G11.4,1X,G11.4,1X,G11.4)
 2000     FORMAT(I3,2X,G11.4,1X,G11.4,1X,G11.4,1X,
     1    G11.4,1X,G11.4,1X,G11.4)
 5000     FORMAT('SURF',7X,'X',11X,'Y',11X,'Z',
     1    11X,'L',11X,'M',11X,'N')
 5001     FORMAT('XZ-PLANE MARGINAL RAY DIFFERENTIAL RAYTRACE DATA ',
     1    ' - (CFG #',I2,')')
 5002     FORMAT('YZ-PLANE MARGINAL RAY DIFFERENTIAL RAYTRACE DATA ',
     1    ' - (CFG #',I2,')')
 5003     FORMAT('XZ-PLANE CHIEF RAY DIFFERENTIAL RAYTRACE DATA ',
     1    ' - (CFG #',I2,')')
 5004     FORMAT('YZ-PLANE CHIEF RAY DIFFERENTIAL RAYTRACE DATA ',
     1    ' - (CFG #',I2,')')
 2501     FORMAT(1X)
      END
C SUB PRLMN.FOR
      SUBROUTINE PRLMN
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE PRLMN. THIS SUBROUTINE IMPLEMENTS
C       THE RAY TRACE 'PRLMN' PRINTOUT
C       AT THE CMD LEVEL.
C
          INTEGER SF,IWAVNM,I
C
          LOGICAL DEG,RAD,TANG
C
          COMMON/ANGMOD/DEG,RAD,TANG
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
C       THE PRLMN COMMAND ACCEPTS QUALIFIER OR NUMERIC
C       INPUT IN AN EITHER OR MODE. IT DOES NOT ACCEPT SRTING
C       INPUT. THE VALID QUALIFIERS ARE "ALL" AND "OBJ" AND "OB"
C       THE VALID NUMERIC INPUT IS THE SURFACE NUMBER FOR
C       WHICH IT IS DESIRED TO PRODUCE OUTPUT. THE QUALIFIER "P"
C       CAUSES RELATIVE APERTURE COORDINATES TO BE PRINTED AS WELL.
C
          IWAVNM=INT(WVN)
          IF(SST.EQ.1.OR.S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
              WRITE(OUTLYNE,*)
     1        '"PRLMN" ONLY TAKES QUALIFIER OR NUMERIC WORD #1 INPUT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SQ.EQ.1.AND.S1.EQ.1.AND.WQ.NE.'P') THEN
              WRITE(OUTLYNE,*)
     1        '"PRLMN" ONLY TAKES QUALIFIER OR NUMERIC WORD #1 INPUT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'BUT NOT BOTH'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
          IF(SYSTEM1(20).EQ.0.0) THEN
              WRITE(OUTLYNE,*)'LENS SYSTEM HAS NO SURFACES'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'NO RAY DATA CAN EXIST'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(.NOT.RAYEXT) THEN
              WRITE(OUTLYNE,*)'NO RAY DATA EXISTS TO DISPLAY'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SQ.EQ.1.AND.WQ.EQ.'ALL') THEN
              SF=NEWIMG
              WRITE(OUTLYNE,5001) INT(F12)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,5002)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,2501)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,5000)
              CALL SHOWIT(0)
              DO 10 I=NEWOBJ,SF
                  WRITE(OUTLYNE,1500)I,RAYRAY(1,I),RAYRAY(2,I)
     1            ,RAYRAY(3,I),RAYRAY(4,I),RAYRAY(5,I),RAYRAY(6,I)
                  CALL SHOWIT(0)
 10           CONTINUE
              RETURN
          END IF
          IF(SQ.EQ.1.AND.WQ.EQ.'OBJ'.OR.SQ.EQ.1.AND.WQ.EQ.'OB') THEN
              SF=NEWOBJ
              IF(HEADIN) WRITE(OUTLYNE,5000)
              IF(HEADIN) CALL SHOWIT(0)
              WRITE(OUTLYNE,1500)SF,RAYRAY(1,SF),RAYRAY(2,SF)
     1        ,RAYRAY(3,SF),RAYRAY(4,SF),RAYRAY(5,SF),RAYRAY(6,SF)
              CALL SHOWIT(0)
              RETURN
          END IF
          IF(WQ.NE.'OBJ'.AND.WQ.NE.' '.AND.
     1    WQ.NE.'ALL'.AND.WQ.NE.'OB'.AND.WQ.NE.'P') THEN
              WRITE(OUTLYNE,*)'INVALID QUALIFIER WORD'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SQ.EQ.0.AND.DF1.EQ.1) THEN
C       OUTPUT IMAGE SURFACE
              SF=NEWIMG
              IF(HEADIN) WRITE(OUTLYNE,5000)
              IF(HEADIN) CALL SHOWIT(0)
              WRITE(OUTLYNE,1500)SF,RAYRAY(1,SF),RAYRAY(2,SF)
     1        ,RAYRAY(3,SF),RAYRAY(4,SF),RAYRAY(5,SF),RAYRAY(6,SF)
              CALL SHOWIT(0)
              RETURN
          END IF
          IF(SQ.EQ.0.AND.DF1.NE.1) THEN
              I=INT(W1)
              SF=NEWIMG
              IF(I.GT.SF.OR.I.LT.NEWOBJ) THEN
                  WRITE(OUTLYNE,*)'SURFACE NUMBER BEYOND LEGAL RANGE'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(HEADIN) WRITE(OUTLYNE,5000)
              IF(HEADIN) CALL SHOWIT(0)
              WRITE(OUTLYNE,1500)I,RAYRAY(1,I),RAYRAY(2,I)
     1        ,RAYRAY(3,I),RAYRAY(4,I),RAYRAY(5,I),RAYRAY(6,I)
              CALL SHOWIT(0)
              RETURN
          END IF
          IF(WQ.EQ.'P'.AND.DF1.EQ.1) THEN
C       OUTPUT IMAGE SURFACE WITH FRACTIONAL OUTPUT HEADER
              SF=NEWIMG
              WRITE(OUTLYNE,1600)IWAVNM,RELY,RELX
              CALL SHOWIT(0)
              IF(HEADIN) WRITE(OUTLYNE,5000)
              IF(HEADIN) CALL SHOWIT(0)
              WRITE(OUTLYNE,1500)SF,RAYRAY(1,SF),RAYRAY(2,SF)
     1        ,RAYRAY(3,SF),RAYRAY(4,SF),RAYRAY(5,SF),RAYRAY(6,SF)
              CALL SHOWIT(0)
              RETURN
          END IF
          IF(WQ.EQ.'P'.AND.DF1.NE.1) THEN
              I=INT(W1)
              SF=NEWIMG
              IF(I.GT.SF.OR.I.LT.NEWOBJ) THEN
                  WRITE(OUTLYNE,*)'SURFACE NUMBER BEYOND LEGAL RANGE'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              WRITE(OUTLYNE,1600) IWAVNM,RELY,RELX
              CALL SHOWIT(0)
              IF(HEADIN) WRITE(OUTLYNE,5000)
              IF(HEADIN) CALL SHOWIT(0)
              WRITE(OUTLYNE,1500)I,RAYRAY(1,I),RAYRAY(2,I)
     1        ,RAYRAY(3,I),RAYRAY(4,I),RAYRAY(5,I),RAYRAY(6,I)
              CALL SHOWIT(0)
              RETURN
          END IF
 1500     FORMAT(I3,2X,D9.2,2X,D9.2,2X,D9.2,2X,
     1    G12.4,2X,G12.4,2X,G12.4)
 1600     FORMAT('RAY AT WAVL # ',I1,
     1    ' (REL AP Y, REL AP X) = ( ',G13.6,', ',G13.6,' )')
 5000     FORMAT('SURF',6X,'X ',9X,'Y  ',8X,'Z  ',
     1    5X,'L-DIRCOS',6X,'M-DIRCOS',6X,'N-DIRCOS')
 5001     FORMAT('RAYTRACE DATA (XZ AND YZ-PLANE)',
     1    ' - (CFG #',I2,')')
 5002     FORMAT(
     1    'DIRECTION COSINES MEASURED WITH RESPECT TO THE Z-AXIS')
 2501     FORMAT(1X)
      END
C SUB PRXY.FOR
      SUBROUTINE PRXY
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE PRXY. THIS SUBROUTINE IMPLEMENTS
C       THE RAY TRACE 'PRY' AND 'PRX' PRINTOUT
C       AT THE CMD LEVEL.
C
          CHARACTER AMODE*8
C
          INTEGER
     6    SF,IWAVNM,I
C
          COMMON/CMODD/AMODE
C
          LOGICAL DEG,RAD,TANG
C
          REAL*8 XA,YA
C
          COMMON/ANGMOD/DEG,RAD,TANG
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
C       THE PRY COMMAND ACCEPTS QUALIFIER OR NUMERIC
C       INPUT IN AN EITHER OR MODE. IT DOES NOT ACCEPT SRTING
C       INPUT. THE VALID QUALIFIERS ARE "ALL" AND "OBJ" AND "OB"
C       THE VALID NUMERIC INPUT IS THE SURFACE NUMBER FOR
C       WHICH IT IS DESIRED TO PRODUCE OUTPUT. THE QUALIFIER "P"
C       CAUSES RELATIVE APERTURE COORDINATES TO BE PRINTED AS WELL.
C
          IWAVNM=INT(WVN)
          IF(SST.EQ.1.OR.S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
              WRITE(OUTLYNE,*)
     1        '"PRX" AND "PRY" ONLY TAKE QUALIFIER OR NUMERIC WORD #1 INPUT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SQ.EQ.1.AND.S1.EQ.1.AND.WQ.NE.'P') THEN
              WRITE(OUTLYNE,*)
     1        '"PRX" AND "PRY" ONLY TAKE QUALIFIER OR NUMERIC WORD #1 INPUT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'BUT NOT BOTH'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
C       SET AMODE
          IF(DEG) THEN
              AMODE='DEGREES '
          END IF
          IF(RAD)  THEN
              AMODE='RADIANS '
          END IF
          IF(TANG) THEN
              AMODE='TANGENT '
          END IF
C
          IF(SYSTEM1(20).EQ.0.0) THEN
              WRITE(OUTLYNE,*)'LENS SYSTEM HAS NO SURFACES'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'NO RAY DATA CAN EXIST'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(.NOT.RAYEXT) THEN
              WRITE(OUTLYNE,*)'NO RAY DATA EXISTS TO DISPLAY'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SQ.EQ.1.AND.WQ.EQ.'ALL') THEN
              SF=NEWIMG
              WRITE(OUTLYNE,5001) INT(F12)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,5003) AMODE
              CALL SHOWIT(0)
              WRITE(OUTLYNE,5002)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,2501)
              CALL SHOWIT(0)
              IF(WC.EQ.'PRY') WRITE(OUTLYNE,5000)
              IF(WC.EQ.'PRX') WRITE(OUTLYNE,6000)
              CALL SHOWIT(0)
              DO 10 I=NEWOBJ,SF
                  IF(WC.EQ.'PRY') THEN
                      XA=RAYRAY(11,I)
                      YA=RAYRAY(12,I)
                      CALL RS(XA,YA)
                      IF(DEG) THEN
                          WRITE(OUTLYNE,2000)I,RAYRAY(2,I),
     1                    ((180.0D0/PII)*YA)
                      END IF
                      IF(RAD) THEN
                          WRITE(OUTLYNE,2000)I,RAYRAY(2,I),
     1                    (YA)
                      END IF
                      IF(TANG) THEN
                          WRITE(OUTLYNE,2000)I,RAYRAY(2,I),
     1                    (DTAN(YA))
                      END IF
                      CALL SHOWIT(0)
                  END IF
                  XA=RAYRAY(11,I)
                  YA=RAYRAY(12,I)
                  CALL RS(XA,YA)
                  IF(WC.EQ.'PRX') THEN
                      IF(DEG) THEN
                          WRITE(OUTLYNE,2000)I,RAYRAY(1,I),
     1                    ((180.0D0/PII)*XA)
                      END IF
                      IF(RAD) THEN
                          WRITE(OUTLYNE,2000) I,RAYRAY(1,I),XA
                      END IF
                      IF(TANG) THEN
                          WRITE(OUTLYNE,2000)I,RAYRAY(1,I),
     1                    (DTAN(XA))
                      END IF
                      CALL SHOWIT(0)
                  END IF
 10           CONTINUE
              RETURN
          END IF
          IF(SQ.EQ.1.AND.WQ.EQ.'OBJ'.OR.SQ.EQ.1.AND.WQ.EQ.'OB') THEN
              SF=NEWOBJ
              IF(WC.EQ.'PRY') THEN
                  IF(HEADIN) WRITE(OUTLYNE,5000)
                  IF(HEADIN) CALL SHOWIT(0)
                  XA=RAYRAY(11,SF)
                  YA=RAYRAY(12,SF)
                  CALL RS(XA,YA)
                  IF(DEG) THEN
                      WRITE(OUTLYNE,1500)SF,RAYRAY(2,SF),
     1                ((180.0D0/PII)*YA)
                  END IF
                  IF(RAD) THEN
                      WRITE(OUTLYNE,1500)SF,RAYRAY(2,SF),
     1                (YA)
                  END IF
                  IF(TANG) THEN
                      WRITE(OUTLYNE,1500)SF,RAYRAY(2,SF),
     1                (DTAN(YA))
                  END IF
                  CALL SHOWIT(0)
              END IF
              IF(WC.EQ.'PRX') THEN
                  IF(HEADIN) WRITE(OUTLYNE,6000)
                  IF(HEADIN) CALL SHOWIT(0)
                  XA=RAYRAY(11,SF)
                  YA=RAYRAY(12,SF)
                  CALL RS(XA,YA)
                  IF(DEG) THEN
                      WRITE(OUTLYNE,1500)SF,RAYRAY(1,SF),
     1                ((180.0D0/PII)*XA)
                  END IF
                  IF(RAD) THEN
                      WRITE(OUTLYNE,1500)SF,RAYRAY(1,SF),
     1                (XA)
                  END IF
                  IF(TANG) THEN
                      WRITE(OUTLYNE,1500)SF,RAYRAY(1,SF),
     1                (DTAN(XA))
                  END IF
                  CALL SHOWIT(0)
              END IF
              RETURN
          END IF
          IF(WQ.NE.'OBJ'.AND.WQ.NE.' '.AND.
     1    WQ.NE.'ALL'.AND.WQ.NE.'OB'.AND.WQ.NE.'P') THEN
              WRITE(OUTLYNE,*)'INVALID QUALIFIER WORD'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SQ.EQ.0.AND.DF1.EQ.1) THEN
C       OUTPUT IMAGE SURFACE
              SF=NEWIMG
              IF(WC.EQ.'PRY') THEN
                  IF(HEADIN) WRITE(OUTLYNE,5000)
                  IF(HEADIN) CALL SHOWIT(0)
                  XA=RAYRAY(11,SF)
                  YA=RAYRAY(12,SF)
                  CALL RS(XA,YA)
                  IF(DEG) THEN
                      WRITE(OUTLYNE,1500)SF,RAYRAY(2,SF),
     1                ((180.0D0/PII)*YA)
                  END IF
                  IF(RAD) THEN
                      WRITE(OUTLYNE,1500)SF,RAYRAY(2,SF),
     1                (YA)
                  END IF
                  IF(TANG) THEN
                      WRITE(OUTLYNE,1500)SF,RAYRAY(2,SF),
     1                (DTAN(YA))
                  END IF
                  CALL SHOWIT(0)
              END IF
              IF(WC.EQ.'PRX') THEN
                  IF(HEADIN) WRITE(OUTLYNE,6000)
                  IF(HEADIN) CALL SHOWIT(0)
                  XA=RAYRAY(11,SF)
                  YA=RAYRAY(12,SF)
                  CALL RS(XA,YA)
                  IF(DEG) THEN
                      WRITE(OUTLYNE,1500)SF,RAYRAY(1,SF),
     1                ((180.0D0/PII)*XA)
                  END IF
                  IF(RAD) THEN
                      WRITE(OUTLYNE,1500)SF,RAYRAY(1,SF),
     1                (XA)
                  END IF
                  IF(TANG) THEN
                      WRITE(OUTLYNE,1500)SF,RAYRAY(1,SF),
     1                (DTAN(XA))
                  END IF
                  CALL SHOWIT(0)
              END IF
              RETURN
          END IF
          IF(SQ.EQ.0.AND.DF1.NE.1) THEN
              I=INT(W1)
              SF=NEWIMG
              IF(I.GT.SF.OR.I.LT.NEWOBJ) THEN
                  WRITE(OUTLYNE,*)'SURFACE NUMBER BEYOND LEGAL RANGE'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(WC.EQ.'PRY') THEN
                  IF(HEADIN) WRITE(OUTLYNE,5000)
                  IF(HEADIN) CALL SHOWIT(0)
                  XA=RAYRAY(11,I)
                  YA=RAYRAY(12,I)
                  CALL RS(XA,YA)
                  IF(DEG) THEN
                      WRITE(OUTLYNE,2000)I,RAYRAY(2,I),
     1                ((180.0D0/PII)*YA)
                  END IF
                  IF(RAD) THEN
                      WRITE(OUTLYNE,2000)I,RAYRAY(2,I),
     1                (YA)
                  END IF
                  IF(TANG) THEN
                      WRITE(OUTLYNE,2000)I,RAYRAY(2,I),
     1                (DTAN(YA))
                  END IF
                  CALL SHOWIT(0)
              END IF
              IF(WC.EQ.'PRX') THEN
                  IF(HEADIN) WRITE(OUTLYNE,6000)
                  IF(HEADIN) CALL SHOWIT(0)
                  XA=RAYRAY(11,I)
                  YA=RAYRAY(12,I)
                  CALL RS(XA,YA)
                  IF(DEG) THEN
                      WRITE(OUTLYNE,2000)I,RAYRAY(1,I),
     1                ((180.0D0/PII)*XA)
                  END IF
                  IF(RAD) THEN
                      WRITE(OUTLYNE,2000)I,RAYRAY(1,I),
     1                (XA)
                  END IF
                  IF(TANG) THEN
                      WRITE(OUTLYNE,2000)I,RAYRAY(1,I),
     1                (DTAN(XA))
                  END IF
                  CALL SHOWIT(0)
              END IF
              RETURN
          END IF
          IF(WQ.EQ.'P'.AND.DF1.EQ.1) THEN
C       OUTPUT IMAGE SURFACE WITH FRACTIONAL OUTPUT HEADER
              SF=NEWIMG
              IF(WC.EQ.'PRY') THEN
                  XA=RAYRAY(11,SF)
                  YA=RAYRAY(12,SF)
                  CALL RS(XA,YA)
                  IF(DEG) THEN
                      WRITE(OUTLYNE,1600)IWAVNM,RELY,RELX
                      CALL SHOWIT(0)
                      IF(HEADIN) WRITE(OUTLYNE,5000)
                      IF(HEADIN) CALL SHOWIT(0)
                      WRITE(OUTLYNE,1500)SF,RAYRAY(2,SF),
     1                ((180.0D0/PII)*YA)
                      CALL SHOWIT(0)
                  END IF
                  IF(RAD) THEN
                      WRITE(OUTLYNE,1600)IWAVNM,RELY,RELX
                      CALL SHOWIT(0)
                      IF(HEADIN) WRITE(OUTLYNE,5000)
                      IF(HEADIN) CALL SHOWIT(0)
                      WRITE(OUTLYNE,1500)SF,RAYRAY(2,SF),
     1                (YA)
                      CALL SHOWIT(0)
                  END IF
                  IF(TANG) THEN
                      WRITE(OUTLYNE,1600)IWAVNM,RELY,RELX
                      CALL SHOWIT(0)
                      IF(HEADIN) WRITE(OUTLYNE,5000)
                      IF(HEADIN) CALL SHOWIT(0)
                      WRITE(OUTLYNE,1500)SF,RAYRAY(2,SF),
     1                (DTAN(YA))
                      CALL SHOWIT(0)
                  END IF
              END IF
              IF(WC.EQ.'PRX') THEN
                  IF(DEG) THEN
                      WRITE(OUTLYNE,1600)IWAVNM,RELY,RELX
                      CALL SHOWIT(0)
                      IF(HEADIN) WRITE(OUTLYNE,6000)
                      IF(HEADIN) CALL SHOWIT(0)
                      XA=RAYRAY(11,SF)
                      YA=RAYRAY(12,SF)
                      CALL RS(XA,YA)
                      WRITE(OUTLYNE,1500)SF,RAYRAY(1,SF),
     1                ((180.0D0/PII)*XA)
                      CALL SHOWIT(0)
                  END IF
                  IF(RAD) THEN
                      WRITE(OUTLYNE,1600)IWAVNM,RELY,RELX
                      CALL SHOWIT(0)
                      IF(HEADIN) WRITE(OUTLYNE,6000)
                      IF(HEADIN) CALL SHOWIT(0)
                      XA=RAYRAY(11,SF)
                      YA=RAYRAY(12,SF)
                      CALL RS(XA,YA)
                      WRITE(OUTLYNE,1500)SF,RAYRAY(1,SF),
     1                (XA)
                      CALL SHOWIT(0)
                  END IF
                  IF(TANG) THEN
                      WRITE(OUTLYNE,1600)IWAVNM,RELY,RELX
                      CALL SHOWIT(0)
                      IF(HEADIN) WRITE(OUTLYNE,6000)
                      IF(HEADIN) CALL SHOWIT(0)
                      XA=RAYRAY(11,SF)
                      YA=RAYRAY(12,SF)
                      CALL RS(XA,YA)
                      WRITE(OUTLYNE,1500)SF,RAYRAY(1,SF),
     1                (DTAN(XA))
                      CALL SHOWIT(0)
                  END IF
              END IF
              RETURN
          END IF
          IF(WQ.EQ.'P'.AND.DF1.NE.1) THEN
              I=INT(W1)
              SF=NEWIMG
              IF(I.GT.SF.OR.I.LT.NEWOBJ) THEN
                  WRITE(OUTLYNE,*)'SURFACE NUMBER BEYOND LEGAL RANGE'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(WC.EQ.'PRY') THEN
                  XA=RAYRAY(11,I)
                  YA=RAYRAY(12,I)
                  CALL RS(XA,YA)
                  IF(DEG) THEN
                      WRITE(OUTLYNE,1600)IWAVNM,RELY,RELX
                      CALL SHOWIT(0)
                      IF(HEADIN) WRITE(OUTLYNE,5000)
                      IF(HEADIN) CALL SHOWIT(0)
                      WRITE(OUTLYNE,2000)I,RAYRAY(2,I),
     1                ((180.0D0/PII)*YA)
                      CALL SHOWIT(0)
                  END IF
                  IF(RAD) THEN
                      WRITE(OUTLYNE,1600)IWAVNM,RELY,RELX
                      CALL SHOWIT(0)
                      IF(HEADIN) WRITE(OUTLYNE,5000)
                      IF(HEADIN) CALL SHOWIT(0)
                      WRITE(OUTLYNE,2000)I,RAYRAY(2,I),
     1                (YA)
                      CALL SHOWIT(0)
                  END IF
                  IF(TANG) THEN
                      WRITE(OUTLYNE,1600)IWAVNM,RELY,RELX
                      CALL SHOWIT(0)
                      IF(HEADIN) WRITE(OUTLYNE,5000)
                      IF(HEADIN) CALL SHOWIT(0)
                      WRITE(OUTLYNE,2000)I,RAYRAY(2,I),
     1                (DTAN(YA))
                      CALL SHOWIT(0)
                  END IF
              END IF
              IF(WC.EQ.'PRX') THEN
                  IF(DEG) THEN
                      WRITE(OUTLYNE,1600)IWAVNM,RELY,RELX
                      CALL SHOWIT(0)
                      IF(HEADIN) WRITE(OUTLYNE,6000)
                      IF(HEADIN) CALL SHOWIT(0)
                      XA=RAYRAY(11,I)
                      YA=RAYRAY(12,I)
                      CALL RS(XA,YA)
                      WRITE(OUTLYNE,2000)I,RAYRAY(1,I),
     1                ((180.0D0/PII)*XA)
                      CALL SHOWIT(0)
                  END IF
                  IF(RAD) THEN
                      WRITE(OUTLYNE,1600)IWAVNM,RELY,RELX
                      CALL SHOWIT(0)
                      IF(HEADIN) WRITE(OUTLYNE,6000)
                      IF(HEADIN) CALL SHOWIT(0)
                      XA=RAYRAY(11,I)
                      YA=RAYRAY(12,I)
                      CALL RS(XA,YA)
                      WRITE(OUTLYNE,2000)I,RAYRAY(1,I),
     1                (XA)
                      CALL SHOWIT(0)
                  END IF
                  IF(TANG) THEN
                      WRITE(OUTLYNE,1600)IWAVNM,RELY,RELX
                      CALL SHOWIT(0)
                      IF(HEADIN) WRITE(OUTLYNE,6000)
                      IF(HEADIN) CALL SHOWIT(0)
                      XA=RAYRAY(11,I)
                      YA=RAYRAY(12,I)
                      CALL RS(XA,YA)
                      WRITE(OUTLYNE,2000)I,RAYRAY(1,I),
     1                (DTAN(XA))
                      CALL SHOWIT(0)
                  END IF
              END IF
              RETURN
          END IF
 1500     FORMAT(I3,2X,G13.6,2X,G13.6)
 1600     FORMAT('RAY AT WAVL # ',I1,
     1    ' (REL AP Y, REL AP X) = ( ',G13.6,', ',G13.6,' )')
 2000     FORMAT(I3,2X,G13.6,2X,G13.6)
 5000     FORMAT('SURF',6X,'Y ',13X,'YANG')
 5001     FORMAT('RAYTRACE DATA (YZ-PLANE)',
     1    ' - (CFG #',I2,')')
 5003     FORMAT('ANGULAR OUTPUT MODE IS ',A8)
 5002     FORMAT(
     1    'SLOPES/ANGLES  MEASURED WITH RESPECT TO THE Z-AXIS')
 2501     FORMAT(1X)
 6000     FORMAT('SURF',6X,'X ',13X,'XANG')
! 6001   FORMAT('RAYTRACE DATA (XZ-PLANE)',
!     1  ' - (CFG #',I2,')')
      END
C SUB PRR.FOR
      SUBROUTINE PRR
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE PRR. THIS SUBROUTINE IMPLEMENTS
C       THE RAY TRACE 'PRR' PRINTOUT
C       AT THE CMD LEVEL.
C
          CHARACTER AMODE*8
C
          INTEGER SF,IWAVNM,I
          REAL*8 COSARG
C
          REAL*8 RR,RRA
C
          COMMON/CMODD/AMODE
C
          LOGICAL DEG,RAD,TANG
C
          COMMON/ANGMOD/DEG,RAD,TANG
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
C       THE PRR COMMAND ACCEPTS QUALIFIER OR NUMERIC
C       INPUT IN AN EITHER OR MODE. IT DOES NOT ACCEPT SRTING
C       INPUT. THE VALID QUALIFIERS ARE "ALL" AND "OBJ" AND "OB"
C       THE VALID NUMERIC INPUT IS THE SURFACE NUMBER FOR
C       WHICH IT IS DESIRED TO PRODUCE OUTPUT. THE QUALIFIER "P"
C       CAUSES RELATIVE APERTURE COORDINATES TO BE PRINTED AS WELL.
C
          IWAVNM=INT(WVN)
          IF(SST.EQ.1.OR.S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
              WRITE(OUTLYNE,*)
     1        '"PRR" ONLY TAKES QUALIFIER OR NUMERIC WORD #1 INPUT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SQ.EQ.1.AND.S1.EQ.1.AND.WQ.NE.'P') THEN
              WRITE(OUTLYNE,*)
     1        '"PRR" ONLY TAKES QUALIFIER OR NUMERIC WORD #1 INPUT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'BUT NOT BOTH'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
C       SET AMODE
          IF(DEG) THEN
              AMODE='DEGREES '
          END IF
          IF(RAD)  THEN
              AMODE='RADIANS '
          END IF
          IF(TANG) THEN
              AMODE='TANGENT '
          END IF
C
          IF(SYSTEM1(20).EQ.0.0) THEN
              WRITE(OUTLYNE,*)'LENS SYSTEM HAS NO SURFACES'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'NO RAY DATA CAN EXIST'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(.NOT.RAYEXT) THEN
              WRITE(OUTLYNE,*)'NO RAY DATA EXISTS TO DISPLAY'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SQ.EQ.1.AND.WQ.EQ.'ALL') THEN
              SF=NEWIMG
              WRITE(OUTLYNE,5001) INT(F12)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,5003) AMODE
              CALL SHOWIT(0)
              WRITE(OUTLYNE,5002)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,2501)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,5000)
              CALL SHOWIT(0)
              DO 10 I=NEWOBJ,SF
                  RR=DSQRT((RAYRAY(1,I)**2)+(RAYRAY(2,I)**2))
                  COSARG=RAYRAY(6,I)
                  IF(COSARG.LT.0.0D0) COSARG=-COSARG
                  IF(COSARG.GT.1.0D0) COSARG=1.0D0
                  RRA=DACOS(COSARG)
                  IF(DEG) THEN
                      WRITE(OUTLYNE,2000)I,RR,((180.0D0/PII)*RRA)
                  END IF
                  IF(RAD) THEN
                      WRITE(OUTLYNE,2000)I,RR,RRA
                  END IF
                  IF(TANG) THEN
                      WRITE(OUTLYNE,2000)I,RR,DTAN(RRA)
                  END IF
                  CALL SHOWIT(0)
 10           CONTINUE
              RETURN
          END IF
          IF(SQ.EQ.1.AND.WQ.EQ.'OBJ'.OR.SQ.EQ.1.AND.WQ.EQ.'OB') THEN
              SF=NEWOBJ
              RR=DSQRT((RAYRAY(1,SF)**2)+(RAYRAY(2,SF)**2))
              COSARG=RAYRAY(6,SF)
              IF(COSARG.LT.0.0D0) COSARG=-COSARG
              IF(COSARG.GT.1.0D0) COSARG=1.0D0
              RRA=DACOS(COSARG)
              IF(HEADIN) WRITE(OUTLYNE,5000)
              IF(HEADIN) CALL SHOWIT(0)
              IF(DEG) THEN
                  WRITE(OUTLYNE,2000)SF,RR,((180.0D0/PII)*RRA)
              END IF
              IF(RAD) THEN
                  WRITE(OUTLYNE,2000)SF,RR,RRA
              END IF
              IF(TANG) THEN
                  WRITE(OUTLYNE,2000)SF,RR,DTAN(RRA)
              END IF
              CALL SHOWIT(0)
              RETURN
          END IF
          IF(WQ.NE.'OBJ'.AND.WQ.NE.' '.AND.
     1    WQ.NE.'ALL'.AND.WQ.NE.'OB'.AND.WQ.NE.'P') THEN
              WRITE(OUTLYNE,*)'INVALID QUALIFIER WORD'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SQ.EQ.0.AND.DF1.EQ.1) THEN
C       OUTPUT IMAGE SURFACE
              SF=NEWIMG
              RR=DSQRT((RAYRAY(1,SF)**2)+(RAYRAY(2,SF)**2))
              COSARG=RAYRAY(6,SF)
              IF(COSARG.LT.0.0D0) COSARG=-COSARG
              IF(COSARG.GT.1.0D0) COSARG=1.0D0
              RRA=DACOS(COSARG)
              IF(HEADIN) WRITE(OUTLYNE,5000)
              IF(HEADIN) CALL SHOWIT(0)
              IF(DEG) THEN
                  WRITE(OUTLYNE,2000)SF,RR,((180.0D0/PII)*RRA)
              END IF
              IF(RAD) THEN
                  WRITE(OUTLYNE,2000)SF,RR,RRA
              END IF
              IF(TANG) THEN
                  WRITE(OUTLYNE,2000)SF,RR,DTAN(RRA)
              END IF
              CALL SHOWIT(0)
              RETURN
          END IF
          IF(SQ.EQ.0.AND.DF1.NE.1) THEN
              I=INT(W1)
              SF=NEWIMG
              IF(I.GT.SF.OR.I.LT.NEWOBJ) THEN
                  WRITE(OUTLYNE,*)'SURFACE NUMBER BEYOND LEGAL RANGE'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              RR=DSQRT((RAYRAY(1,I)**2)+(RAYRAY(2,I)**2))
              COSARG=RAYRAY(6,I)
              IF(COSARG.LT.0.0D0) COSARG=-COSARG
              IF(COSARG.GT.1.0D0) COSARG=1.0D0
              RRA=DACOS(COSARG)
              IF(HEADIN) WRITE(OUTLYNE,5000)
              IF(HEADIN) CALL SHOWIT(0)
              IF(DEG) THEN
                  WRITE(OUTLYNE,2000)I,RR,((180.0D0/PII)*RRA)
              END IF
              IF(RAD) THEN
                  WRITE(OUTLYNE,2000)I,RR,RRA
              END IF
              IF(TANG) THEN
                  WRITE(OUTLYNE,2000)I,RR,DTAN(RRA)
              END IF
              CALL SHOWIT(0)
              RETURN
          END IF
          IF(WQ.EQ.'P'.AND.DF1.EQ.1) THEN
C       OUTPUT IMAGE SURFACE WITH FRACTIONAL OUTPUT HEADER
              SF=NEWIMG
              RR=DSQRT((RAYRAY(1,SF)**2)+(RAYRAY(2,SF)**2))
              COSARG=RAYRAY(6,SF)
              IF(COSARG.LT.0.0D0) COSARG=-COSARG
              IF(COSARG.GT.1.0D0) COSARG=1.0D0
              RRA=DACOS(COSARG)
              IF(DEG) THEN
                  WRITE(OUTLYNE,1600)IWAVNM,RELY,RELX
                  CALL SHOWIT(0)
                  IF(HEADIN) WRITE(OUTLYNE,5000)
                  IF(HEADIN) CALL SHOWIT(0)
                  WRITE(OUTLYNE,2000)SF,RR,((180.0D0/PII)*RRA)
                  CALL SHOWIT(0)
              END IF
              IF(RAD) THEN
                  WRITE(OUTLYNE,1600)IWAVNM,RELY,RELX
                  CALL SHOWIT(0)
                  IF(HEADIN) WRITE(OUTLYNE,5000)
                  IF(HEADIN) CALL SHOWIT(0)
                  WRITE(OUTLYNE,2000)SF,RR,RRA
                  CALL SHOWIT(0)
              END IF
              IF(TANG) THEN
                  WRITE(OUTLYNE,1600)IWAVNM,RELY,RELX
                  CALL SHOWIT(0)
                  IF(HEADIN) WRITE(OUTLYNE,5000)
                  IF(HEADIN) CALL SHOWIT(0)
                  WRITE(OUTLYNE,2000)SF,RR,DTAN(RRA)
                  CALL SHOWIT(0)
              END IF
              RETURN
          END IF
          IF(WQ.EQ.'P'.AND.DF1.NE.1) THEN
              I=INT(W1)
              SF=NEWIMG
              IF(I.GT.SF.OR.I.LT.NEWOBJ) THEN
                  WRITE(OUTLYNE,*)'SURFACE NUMBER BEYOND LEGAL RANGE'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              RR=DSQRT((RAYRAY(1,SF)**2)+(RAYRAY(2,SF)**2))
              COSARG=RAYRAY(6,SF)
              IF(COSARG.LT.0.0D0) COSARG=-COSARG
              IF(COSARG.GT.1.0D0) COSARG=1.0D0
              RRA=DACOS(COSARG)
              IF(DEG) THEN
                  WRITE(OUTLYNE,1600)IWAVNM,RELY,RELX
                  CALL SHOWIT(0)
                  IF(HEADIN) WRITE(OUTLYNE,5000)
                  IF(HEADIN) CALL SHOWIT(0)
                  WRITE(OUTLYNE,2000)SF,RR,((180.0D0/PII)*RRA)
                  CALL SHOWIT(0)
              END IF
              IF(RAD) THEN
                  WRITE(OUTLYNE,1600)IWAVNM,RELY,RELX
                  CALL SHOWIT(0)
                  IF(HEADIN) WRITE(OUTLYNE,5000)
                  IF(HEADIN) CALL SHOWIT(0)
                  WRITE(OUTLYNE,2000)SF,RR,RRA
                  CALL SHOWIT(0)
              END IF
              IF(TANG) THEN
                  WRITE(OUTLYNE,1600)IWAVNM,RELY,RELX
                  CALL SHOWIT(0)
                  IF(HEADIN) WRITE(OUTLYNE,5000)
                  IF(HEADIN) CALL SHOWIT(0)
                  WRITE(OUTLYNE,2000)SF,RR,DTAN(RRA)
                  CALL SHOWIT(0)
              END IF
              RETURN
          END IF
 1600     FORMAT('RAY AT WAVL # ',I1,
     1    ' (REL AP Y, REL AP X) = ( ',G13.6,', ',G13.6,' )')
 2000     FORMAT(I3,2X,G13.6,2X,G13.6)
 5000     FORMAT('SURF',6X,'R ',13X,'R-ANG')
 5001     FORMAT('RAYTRACE DATA (XZ AND YZ-PLANE)',
     1    ' - (CFG #',I2,')')
 5003     FORMAT('ANGULAR OUTPUT MODE IS ',A8)
 5002     FORMAT(
     1    'SLOPES/ANGLES  MEASURED WITH RESPECT TO THE Z-AXIS')
 2501     FORMAT(1X)
      END
C SUB PRXYD.FOR
      SUBROUTINE PRXYD
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE PRXYD. THIS SUBROUTINE IMPLEMENTS
C       THE RAY TRACE 'PRXYD' PRINTOUT
C       AT THE CMD LEVEL.
C
          CHARACTER AMODE*8
C
          INTEGER SF,IWAVNM,I
C
          REAL*8 OPD,DUMMY,XA,YA
C
          COMMON/CMODD/AMODE
C
          LOGICAL DEG,RAD,TANG,OPDERROR
C
          COMMON/ANGMOD/DEG,RAD,TANG
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
C
C       THE PRXYD COMMAND ACCEPTS QUALIFIER OR NUMERIC
C       INPUT IN AN EITHER OR MODE. IT DOES NOT ACCEPT SRTING
C       INPUT. THE VALID QUALIFIERS ARE "ALL" AND "OBJ" AND "OB"
C       THE VALID NUMERIC INPUT IS THE SURFACE NUMBER FOR
C       WHICH IT IS DESIRED TO PRODUCE OUTPUT. THE QUALIFIER "P"
C       CAUSES RELATIVE APERTURE COORDINATES TO BE PRINTED AS WELL.
C
          IWAVNM=INT(WVN)
          IF(SST.EQ.1.OR.S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
              WRITE(OUTLYNE,*)
     1        '"PRXYD" ONLY TAKES QUALIFIER OR NUMERIC WORD #1 INPUT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SQ.EQ.1.AND.S1.EQ.1.AND.WQ.NE.'P') THEN
              WRITE(OUTLYNE,*)
     1        '"PRXYD" ONLY TAKES QUALIFIER OR NUMERIC WORD #1 INPUT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'BUT NOT BOTH'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
C       SET AMODE
          IF(DEG) THEN
              AMODE='DEGREES '
          END IF
          IF(RAD)  THEN
              AMODE='RADIANS '
          END IF
          IF(TANG) THEN
              AMODE='TANGENT '
          END IF
C
          IF(SYSTEM1(20).EQ.0.0) THEN
              WRITE(OUTLYNE,*)'LENS SYSTEM HAS NO SURFACES'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'NO RAY DATA CAN EXIST'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(.NOT.RAYEXT) THEN
              WRITE(OUTLYNE,*)'NO RAY DATA EXISTS TO DISPLAY'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SQ.EQ.1.AND.WQ.EQ.'ALL') THEN
              SF=NEWIMG
              WRITE(OUTLYNE,5001) INT(F12)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,5003) AMODE
              CALL SHOWIT(0)
              WRITE(OUTLYNE,5002)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,2501)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,5000)
              CALL SHOWIT(0)
              DO 10 I=NEWOBJ,SF
                  XA=RAYRAY(11,I)
                  YA=RAYRAY(12,I)
                  CALL RS(XA,YA)
                  IF(DEG) THEN
                      WRITE(OUTLYNE,2000)I,RAYRAY(1,I),RAYRAY(2,I)
     1                ,RAYRAY(7,I),((180.0D0/PII)*XA),
     2                ((180.0D0/PII)*YA)
                  END IF
                  IF(RAD) THEN
                      WRITE(OUTLYNE,2000)I,RAYRAY(1,I),RAYRAY(2,I)
     1                ,RAYRAY(7,I),(XA),
     2                (YA)
                  END IF
                  IF(TANG) THEN
                      WRITE(OUTLYNE,2000)I,RAYRAY(1,I),RAYRAY(2,I)
     1                ,RAYRAY(7,I),(DTAN(XA)),
     2                (DTAN(YA))
                  END IF
                  CALL SHOWIT(0)
 10           CONTINUE
              IF(.NOT.NULL) THEN
C       CALCULATE AND PRINT RAY OPD IN WAVES AT WAVELENGTH
C       THAT RAY WAS TRACED.
                  CALL GETOPD(DUMMY,OPD,OPDERROR)
C
C       OUTPUT OPD OF CURRENT RAY AND WAVELENGTH IN MICRONS
                  IF(.NOT.OPDERROR) THEN
                      WRITE(OUTLYNE,1650) OPD,SYSTEM1(IWAVNM)
                      CALL SHOWIT(0)
                  END IF
              END IF
C
              RETURN
          END IF
          IF(SQ.EQ.1.AND.WQ.EQ.'OBJ'.OR.SQ.EQ.1.AND.WQ.EQ.'OB') THEN
              SF=NEWOBJ
              IF(HEADIN) WRITE(OUTLYNE,5000)
              IF(HEADIN) CALL SHOWIT(0)
              XA=RAYRAY(11,SF)
              YA=RAYRAY(12,SF)
              CALL RS(XA,YA)
              IF(DEG) THEN
                  WRITE(OUTLYNE,1500)SF,RAYRAY(1,SF),RAYRAY(2,SF)
     1            ,RAYRAY(7,SF),((180.0D0/PII)*XA),
     2            ((180.0D0/PII)*YA)
              END IF
              IF(RAD) THEN
                  WRITE(OUTLYNE,1500)SF,RAYRAY(1,SF),RAYRAY(2,SF)
     1            ,RAYRAY(7,SF),(XA),
     2            (YA)
              END IF
              IF(TANG) THEN
                  WRITE(OUTLYNE,1500)SF,RAYRAY(1,SF),RAYRAY(2,SF)
     1            ,RAYRAY(7,SF),(DTAN(XA)),
     2            (DTAN(YA))
              END IF
              CALL SHOWIT(0)
              RETURN
          END IF
          IF(WQ.NE.'OBJ'.AND.WQ.NE.' '.AND.
     1    WQ.NE.'ALL'.AND.WQ.NE.'OB'.AND.WQ.NE.'P') THEN
              WRITE(OUTLYNE,*)'INVALID QUALIFIER WORD'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SQ.EQ.0.AND.DF1.EQ.1) THEN
C       OUTPUT IMAGE SURFACE
              SF=NEWIMG
              IF(HEADIN) WRITE(OUTLYNE,5000)
              IF(HEADIN) CALL SHOWIT(0)
              XA=RAYRAY(11,SF)
              YA=RAYRAY(12,SF)
              CALL RS(XA,YA)
              IF(DEG) THEN
                  WRITE(OUTLYNE,1500)SF,RAYRAY(1,SF),RAYRAY(2,SF)
     1            ,RAYRAY(7,SF),((180.0D0/PII)*XA),
     2            ((180.0D0/PII)*YA)
              END IF
              IF(RAD) THEN
                  WRITE(OUTLYNE,1500)SF,RAYRAY(1,SF),RAYRAY(2,SF)
     1            ,RAYRAY(7,SF),(XA),
     2            (YA)
              END IF
              IF(TANG) THEN
                  WRITE(OUTLYNE,1500)SF,RAYRAY(1,SF),RAYRAY(2,SF)
     1            ,RAYRAY(7,SF),(DTAN(XA)),
     2            (DTAN(YA))
              END IF
              CALL SHOWIT(0)
              RETURN
          END IF
          IF(SQ.EQ.0.AND.DF1.NE.1) THEN
              I=INT(W1)
              SF=NEWIMG
              IF(I.GT.SF.OR.I.LT.NEWOBJ) THEN
                  WRITE(OUTLYNE,*)'SURFACE NUMBER BEYOND LEGAL RANGE'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(HEADIN) WRITE(OUTLYNE,5000)
              IF(HEADIN) CALL SHOWIT(0)
              XA=RAYRAY(11,I)
              YA=RAYRAY(12,I)
              CALL RS(XA,YA)
              IF(DEG) THEN
                  WRITE(OUTLYNE,2000)I,RAYRAY(1,I),RAYRAY(2,I)
     1            ,RAYRAY(7,I),((180.0D0/PII)*XA),
     2            ((180.0D0/PII)*YA)
              END IF
              IF(RAD) THEN
                  WRITE(OUTLYNE,2000)I,RAYRAY(1,I),RAYRAY(2,I)
     1            ,RAYRAY(7,I),XA,
     2            (YA)
              END IF
              IF(TANG) THEN
                  WRITE(OUTLYNE,2000)I,RAYRAY(1,I),RAYRAY(2,I)
     1            ,RAYRAY(7,I),(DTAN(XA)),
     2            (DTAN(YA))
              END IF
              CALL SHOWIT(0)
              RETURN
          END IF
          IF(WQ.EQ.'P'.AND.DF1.EQ.1) THEN
C       OUTPUT IMAGE SURFACE WITH FRACTIONAL OUTPUT HEADER
              SF=NEWIMG
              IF(DEG) THEN
                  WRITE(OUTLYNE,1600)IWAVNM,RELY,RELX
                  CALL SHOWIT(0)
                  IF(HEADIN) WRITE(OUTLYNE,5000)
                  IF(HEADIN) CALL SHOWIT(0)
                  XA=RAYRAY(11,SF)
                  YA=RAYRAY(12,SF)
                  CALL RS(XA,YA)
                  WRITE(OUTLYNE,1500)SF,RAYRAY(1,SF),RAYRAY(2,SF)
     1            ,RAYRAY(7,SF),((180.0D0/PII)*XA),
     2            ((180.0D0/PII)*YA)
                  CALL SHOWIT(0)
              END IF
              IF(RAD) THEN
                  WRITE(OUTLYNE,1600)IWAVNM,RELY,RELX
                  CALL SHOWIT(0)
                  IF(HEADIN) WRITE(OUTLYNE,5000)
                  IF(HEADIN) CALL SHOWIT(0)
                  XA=RAYRAY(11,SF)
                  YA=RAYRAY(12,SF)
                  CALL RS(XA,YA)
                  WRITE(OUTLYNE,1500)SF,RAYRAY(1,SF),RAYRAY(2,SF)
     1            ,RAYRAY(7,SF),(XA),
     2            (YA)
                  CALL SHOWIT(0)
              END IF
              IF(TANG) THEN
                  WRITE(OUTLYNE,1600)IWAVNM,RELY,RELX
                  CALL SHOWIT(0)
                  XA=RAYRAY(11,SF)
                  YA=RAYRAY(12,SF)
                  CALL RS(XA,YA)
                  IF(HEADIN) WRITE(OUTLYNE,5000)
                  IF(HEADIN) CALL SHOWIT(0)
                  WRITE(OUTLYNE,1500)SF,RAYRAY(1,SF),RAYRAY(2,SF)
     1            ,RAYRAY(7,SF),(DTAN(XA)),
     2            (DTAN(YA))
                  CALL SHOWIT(0)
              END IF
              RETURN
          END IF
          IF(WQ.EQ.'P'.AND.DF1.NE.1) THEN
              I=INT(W1)
              SF=NEWIMG
              IF(I.GT.SF.OR.I.LT.NEWOBJ) THEN
                  WRITE(OUTLYNE,*)'SURFACE NUMBER BEYOND LEGAL RANGE'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(DEG) THEN
                  WRITE(OUTLYNE,1600)IWAVNM,RELY,RELX
                  CALL SHOWIT(0)
                  XA=RAYRAY(11,I)
                  YA=RAYRAY(12,I)
                  CALL RS(XA,YA)
                  IF(HEADIN) WRITE(OUTLYNE,5000)
                  IF(HEADIN) CALL SHOWIT(0)
                  WRITE(OUTLYNE,2000)I,RAYRAY(1,I),RAYRAY(2,I)
     1            ,RAYRAY(7,I),((180.0D0/PII)*XA),
     2            ((180.0D0/PII)*YA)
                  CALL SHOWIT(0)
              END IF
              IF(RAD) THEN
                  WRITE(OUTLYNE,1600)IWAVNM,RELY,RELX
                  CALL SHOWIT(0)
                  XA=RAYRAY(11,I)
                  YA=RAYRAY(12,I)
                  CALL RS(XA,YA)
                  IF(HEADIN) WRITE(OUTLYNE,5000)
                  IF(HEADIN) CALL SHOWIT(0)
                  WRITE(OUTLYNE,2000)I,RAYRAY(1,I),RAYRAY(2,I)
     1            ,RAYRAY(7,I),(XA),
     2            (YA)
                  CALL SHOWIT(0)
              END IF
              IF(TANG) THEN
                  WRITE(OUTLYNE,1600)IWAVNM,RELY,RELX
                  CALL SHOWIT(0)
                  XA=RAYRAY(11,I)
                  YA=RAYRAY(12,I)
                  CALL RS(XA,YA)
                  IF(HEADIN) WRITE(OUTLYNE,5000)
                  IF(HEADIN) CALL SHOWIT(0)
                  WRITE(OUTLYNE,2000)I,RAYRAY(1,I),RAYRAY(2,I)
     1            ,RAYRAY(7,I),(DTAN(XA)),
     2            (DTAN(YA))
                  CALL SHOWIT(0)
              END IF
              RETURN
          END IF
 1500     FORMAT(I3,2X,G13.6,2X,G13.6,2X,G13.6,2X,
     1    G13.6,2X,G13.6)
 1600     FORMAT('RAY AT WAVL # ',I1,
     1    ' (REL AP Y, REL AP X) = ( ',G13.6,', ',G13.6,' )')
 1650     FORMAT
     1    ('OPD = ',G13.6,' WAVES AT WAVELENGTH = ',G13.6,' MICRONS')
 2000     FORMAT(I3,2X,G13.6,2X,G13.6,2X,G13.6,2X,
     1    G13.6,2X,G13.6)
 5000     FORMAT('SURF',6X,'X ',13X,'Y  ',12X,'OPL',
     1    11X,'XANG',11X,'YANG')
 5001     FORMAT('RAYTRACE DATA (XZ AND YZ-PLANE)',
     1    ' - (CFG #',I2,')')
 5003     FORMAT('ANGULAR OUTPUT MODE IS ',A8)
 5002     FORMAT(
     1    'SLOPES/ANGLES  MEASURED WITH RESPECT TO THE Z-AXIS')
 2501     FORMAT(1X)
      END
C SUB PRRAY.FOR
      SUBROUTINE PRRAY
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE PRRAY. THIS SUBROUTINE IMPLEMENTS
C       THE RAY TRACE 'PRXYZ','PRXYI' AND 'PRXYIP' PRINTOUT
C       AT THE CMD LEVEL.
C
          CHARACTER AMODE*8
C
          REAL*8 COSARG,XA,YA
C
          INTEGER
     6    SF,IWAVNM,I
C
          COMMON/CMODD/AMODE
C
          LOGICAL DEG,RAD,TANG
C
          COMMON/ANGMOD/DEG,RAD,TANG
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
C       THE VALID NUMERIC INPUT IS THE SURFACE NUMBER FOR
C       WHICH IT IS DESIRED TO PRODUCE OUTPUT. THE QUALIFIER "P"
C       CAUSES RELATIVE APERTURE COORDINATES TO BE PRINTED AS WELL.
C
          IWAVNM=INT(WVN)
          IF(WC.EQ.'PRXYZ') THEN
              IF(SST.EQ.1.OR.S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
                  WRITE(OUTLYNE,*)
     1            '"PRXYZ" ONLY TAKES QUALIFIER OR NUMERIC WORD #1 INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(SQ.EQ.1.AND.S1.EQ.1.AND.WQ.NE.'P') THEN
                  WRITE(OUTLYNE,*)
     1            '"PRXYZ" ONLY TAKES QUALIFIER OR NUMERIC WORD #1 INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'BUT NOT BOTH'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
C
          IF(WC.EQ.'PRXYI') THEN
              IF(SST.EQ.1.OR.S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
                  WRITE(OUTLYNE,*)
     1            '"PRXYI" ONLY TAKES QUALIFIER OR NUMERIC WORD #1 INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(SQ.EQ.1.AND.S1.EQ.1.AND.WQ.NE.'P') THEN
                  WRITE(OUTLYNE,*)
     1            '"PRXYI" ONLY TAKES QUALIFIER OR NUMERIC WORD #1 INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'BUT NOT BOTH'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
C
          IF(WC.EQ.'PRXYIP') THEN
              IF(SST.EQ.1.OR.S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
                  WRITE(OUTLYNE,*)
     1            '"PRXYIP" ONLY TAKES QUALIFIER OR NUMERIC WORD #1 INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(SQ.EQ.1.AND.S1.EQ.1.AND.WQ.NE.'P') THEN
                  WRITE(OUTLYNE,*)
     1            '"PRXYIP" ONLY TAKES QUALIFIER OR NUMERIC WORD #1 INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'BUT NOT BOTH'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
C
C       SET AMODE
          IF(DEG) THEN
              AMODE='DEGREES '
          END IF
          IF(RAD)  THEN
              AMODE='RADIANS '
          END IF
          IF(TANG) THEN
              AMODE='TANGENT '
          END IF
C
          IF(SYSTEM1(20).EQ.0.0) THEN
              WRITE(OUTLYNE,*)'LENS SYSTEM HAS NO SURFACES'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'NO RAY DATA CAN EXIST'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(.NOT.RAYEXT) THEN
              WRITE(OUTLYNE,*)'NO RAY DATA EXISTS TO DISPLAY'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SQ.EQ.1.AND.WQ.EQ.'ALL') THEN
              SF=NEWIMG
              WRITE(OUTLYNE,5001) INT(F12)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,5003) AMODE
              CALL SHOWIT(0)
              WRITE(OUTLYNE,5002)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,2501)
              CALL SHOWIT(0)
              IF(WC.EQ.'PRXYZ')WRITE(OUTLYNE,5000)
              IF(WC.EQ.'PRXYI')WRITE(OUTLYNE,6000)
              IF(WC.EQ.'PRXYIP')WRITE(OUTLYNE,7000)
              CALL SHOWIT(0)
              DO 10 I=NEWOBJ,SF
                  IF(WC.EQ.'PRXYZ') THEN
                      XA=RAYRAY(11,I)
                      YA=RAYRAY(12,I)
                      CALL RS(XA,YA)
                      IF(DEG) THEN
                          WRITE(OUTLYNE,2000)I,RAYRAY(1,I),RAYRAY(2,I)
     1                    ,RAYRAY(3,I),((180.0D0/PII)*XA),
     2                    ((180.0D0/PII)*YA)
                      END IF
                      IF(RAD) THEN
                          WRITE(OUTLYNE,2000)I,RAYRAY(1,I),RAYRAY(2,I)
     1                    ,RAYRAY(3,I),(XA),
     2                    (YA)
                      END IF
                      IF(TANG) THEN
                          WRITE(OUTLYNE,2000)I,RAYRAY(1,I),RAYRAY(2,I)
     1                    ,RAYRAY(3,I),(DTAN(XA)),
     2                    (DTAN(YA))
                      END IF
                      CALL SHOWIT(0)
                  END IF
                  IF(WC.EQ.'PRXYI') THEN
                      XA=RAYRAY(11,I)
                      YA=RAYRAY(12,I)
                      CALL RS(XA,YA)
                      IF(DEG) THEN
                          COSARG=RAYRAY(9,I)
                          IF(COSARG.LT.0.0D0) COSARG=-COSARG
                          IF(COSARG.GT.1.0D0) COSARG=1.0D0
                          WRITE(OUTLYNE,2000)I,RAYRAY(1,I),RAYRAY(2,I)
     1                    ,((180.0D0/PII)*DACOS(COSARG)),
     2                    ((180.0D0/PII)*XA),
     3                    ((180.0D0/PII)*YA)
                      END IF
                      IF(RAD) THEN
                          COSARG=RAYRAY(9,I)
                          IF(COSARG.LT.0.0D0) COSARG=-COSARG
                          IF(COSARG.GT.1.0D0) COSARG=1.0D0
                          WRITE(OUTLYNE,2000)I,RAYRAY(1,I),RAYRAY(2,I)
     1                    ,DACOS(COSARG),(XA),
     2                    (YA)
                      END IF
                      XA=RAYRAY(11,I)
                      YA=RAYRAY(12,I)
                      CALL RS(XA,YA)
                      IF(TANG) THEN
                          COSARG=RAYRAY(9,I)
                          IF(COSARG.LT.0.0D0) COSARG=-COSARG
                          IF(COSARG.GT.1.0D0) COSARG=1.0D0
                          WRITE(OUTLYNE,2000)I,RAYRAY(1,I),RAYRAY(2,I)
     1                    ,DTAN(DACOS(COSARG)),(DTAN(YA)),
     2                    (DTAN(YA))
                      END IF
                      CALL SHOWIT(0)
                  END IF
                  IF(WC.EQ.'PRXYIP') THEN
                      XA=RAYRAY(11,I)
                      YA=RAYRAY(12,I)
                      CALL RS(XA,YA)
                      IF(DEG) THEN
                          COSARG=RAYRAY(10,I)
                          IF(COSARG.LT.0.0D0) COSARG=-COSARG
                          IF(COSARG.GT.1.0D0) COSARG=1.0D0
                          WRITE(OUTLYNE,2000)I,RAYRAY(1,I),RAYRAY(2,I)
     1                    ,((180.0D0/PII)*DACOS(COSARG)),
     2                    ((180.0D0/PII)*XA),
     3                    ((180.0D0/PII)*YA)
                      END IF
                      IF(RAD) THEN
                          COSARG=RAYRAY(10,I)
                          IF(COSARG.LT.0.0D0) COSARG=-COSARG
                          IF(COSARG.GT.1.0D0) COSARG=1.0D0
                          WRITE(OUTLYNE,2000)I,RAYRAY(1,I),RAYRAY(2,I)
     1                    ,DACOS(COSARG),XA,
     2                    (YA)
                      END IF
                      IF(TANG) THEN
                          COSARG=RAYRAY(10,I)
                          IF(COSARG.LT.0.0D0) COSARG=-COSARG
                          IF(COSARG.GT.1.0D0) COSARG=1.0D0
                          WRITE(OUTLYNE,2000)I,RAYRAY(1,I),RAYRAY(2,I)
     1                    ,DTAN(DACOS(COSARG)),(DTAN(XA)),
     2                    (DTAN(YA))
                      END IF
                      CALL SHOWIT(0)
                  END IF
 10           CONTINUE
              RETURN
          END IF
          IF(SQ.EQ.1.AND.WQ.EQ.'OBJ'.OR.SQ.EQ.1.AND.WQ.EQ.'OB') THEN
              SF=NEWOBJ
              IF(WC.EQ.'PRXYZ') THEN
                  XA=RAYRAY(11,SF)
                  YA=RAYRAY(12,SF)
                  CALL RS(XA,YA)
                  IF(HEADIN) WRITE(OUTLYNE,5000)
                  IF(HEADIN) CALL SHOWIT(0)
                  IF(DEG) THEN
                      WRITE(OUTLYNE,1500)SF,RAYRAY(1,SF),RAYRAY(2,SF)
     1                ,RAYRAY(3,SF),((180.0D0/PII)*XA),
     2                ((180.0D0/PII)*YA)
                  END IF
                  IF(RAD) THEN
                      WRITE(OUTLYNE,1500)SF,RAYRAY(1,SF),RAYRAY(2,SF)
     1                ,RAYRAY(3,SF),XA,
     2                (YA)
                  END IF
                  IF(TANG) THEN
                      WRITE(OUTLYNE,1500)SF,RAYRAY(1,SF),RAYRAY(2,SF)
     1                ,RAYRAY(3,SF),(DTAN(XA)),
     2                (DTAN(YA))
                  END IF
                  CALL SHOWIT(0)
              END IF
              IF(WC.EQ.'PRXYI') THEN
                  XA=RAYRAY(11,SF)
                  YA=RAYRAY(12,SF)
                  CALL RS(XA,YA)
                  IF(HEADIN) WRITE(OUTLYNE,6000)
                  IF(HEADIN) CALL SHOWIT(0)
                  IF(DEG) THEN
                      COSARG=RAYRAY(9,SF)
                      IF(COSARG.LT.0.0D0) COSARG=-COSARG
                      IF(COSARG.GT.1.0D0) COSARG=1.0D0
                      WRITE(OUTLYNE,1500)SF,RAYRAY(1,SF),RAYRAY(2,SF)
     1                ,((180.0D0/PII)*DACOS(COSARG)),
     2                ((180.0D0/PII)*XA),
     3                ((180.0D0/PII)*YA)
                  END IF
                  IF(RAD) THEN
                      COSARG=RAYRAY(9,SF)
                      IF(COSARG.LT.0.0D0) COSARG=-COSARG
                      IF(COSARG.GT.1.0D0) COSARG=1.0D0
                      WRITE(OUTLYNE,1500)SF,RAYRAY(1,SF),RAYRAY(2,SF)
     1                ,DACOS(COSARG),XA,
     2                (YA)
                  END IF
                  IF(TANG) THEN
                      COSARG=RAYRAY(9,SF)
                      IF(COSARG.LT.0.0D0) COSARG=-COSARG
                      IF(COSARG.GT.1.0D0) COSARG=1.0D0
                      WRITE(OUTLYNE,1500)SF,RAYRAY(1,SF),RAYRAY(2,SF)
     1                ,DTAN(DACOS(COSARG)),(DTAN(XA)),
     2                (DTAN(YA))
                  END IF
                  CALL SHOWIT(0)
              END IF
              IF(WC.EQ.'PRXYIP') THEN
                  IF(HEADIN) WRITE(OUTLYNE,7000)
                  IF(HEADIN) CALL SHOWIT(0)
                  XA=RAYRAY(11,SF)
                  YA=RAYRAY(12,SF)
                  CALL RS(XA,YA)
                  IF(DEG) THEN
                      COSARG=RAYRAY(10,SF)
                      IF(COSARG.LT.0.0D0) COSARG=-COSARG
                      IF(COSARG.GT.1.0D0) COSARG=1.0D0
                      WRITE(OUTLYNE,1500)SF,RAYRAY(1,SF),RAYRAY(2,SF)
     1                ,((180.0D0/PII)*DACOS(COSARG)),
     2                ((180.0D0/PII)*XA),
     3                ((180.0D0/PII)*YA)
                  END IF
                  IF(RAD) THEN
                      COSARG=RAYRAY(10,SF)
                      IF(COSARG.LT.0.0D0) COSARG=-COSARG
                      IF(COSARG.GT.1.0D0) COSARG=1.0D0
                      WRITE(OUTLYNE,1500)SF,RAYRAY(1,SF),RAYRAY(2,SF)
                      WRITE(OUTLYNE,1500)SF,RAYRAY(1,SF),RAYRAY(2,SF)
     1                ,DACOS(COSARG),(XA),
     2                (YA)
                  END IF
                  IF(TANG) THEN
                      COSARG=RAYRAY(10,SF)
                      IF(COSARG.LT.0.0D0) COSARG=-COSARG
                      IF(COSARG.GT.1.0D0) COSARG=1.0D0
                      WRITE(OUTLYNE,1500)SF,RAYRAY(1,SF),RAYRAY(2,SF)
     1                ,DTAN(DACOS(COSARG)),(DTAN(XA)),
     2                (DTAN(YA))
                  END IF
                  CALL SHOWIT(0)
              END IF
              RETURN
          END IF
          IF(WQ.NE.'OBJ'.AND.WQ.NE.' '.AND.
     1    WQ.NE.'ALL'.AND.WQ.NE.'OB'.AND.WQ.NE.'P') THEN
              WRITE(OUTLYNE,*)'INVALID QUALIFIER WORD'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SQ.EQ.0.AND.DF1.EQ.1) THEN
C       OUTPUT IMAGE SURFACE
              SF=NEWIMG
              IF(WC.EQ.'PRXYZ') THEN
                  IF(HEADIN) WRITE(OUTLYNE,5000)
                  IF(HEADIN) CALL SHOWIT(0)
                  XA=RAYRAY(11,SF)
                  YA=RAYRAY(12,SF)
                  CALL RS(XA,YA)
                  IF(DEG) THEN
                      WRITE(OUTLYNE,1500)SF,RAYRAY(1,SF),RAYRAY(2,SF)
     1                ,RAYRAY(3,SF),((180.0D0/PII)*XA),
     2                ((180.0D0/PII)*YA)
                  END IF
                  IF(RAD) THEN
                      WRITE(OUTLYNE,1500)SF,RAYRAY(1,SF),RAYRAY(2,SF)
     1                ,RAYRAY(3,SF),(XA),
     2                (YA)
                  END IF
                  IF(TANG) THEN
                      WRITE(OUTLYNE,1500)SF,RAYRAY(1,SF),RAYRAY(2,SF)
     1                ,RAYRAY(3,SF),(DTAN(XA)),
     2                (DTAN(YA))
                  END IF
                  CALL SHOWIT(0)
              END IF
              IF(WC.EQ.'PRXYI') THEN
                  XA=RAYRAY(11,SF)
                  YA=RAYRAY(12,SF)
                  CALL RS(XA,YA)
                  IF(HEADIN) WRITE(OUTLYNE,6000)
                  IF(HEADIN) CALL SHOWIT(0)
                  IF(DEG) THEN
                      COSARG=RAYRAY(9,SF)
                      IF(COSARG.LT.0.0D0) COSARG=-COSARG
                      IF(COSARG.GT.1.0D0) COSARG=1.0D0
                      WRITE(OUTLYNE,1500)SF,RAYRAY(1,SF),RAYRAY(2,SF)
     1                ,((180.0D0/PII)*DACOS(COSARG)),
     2                ((180.0D0/PII)*XA),
     3                ((180.0D0/PII)*YA)
                  END IF
                  IF(RAD) THEN
                      COSARG=RAYRAY(9,SF)
                      IF(COSARG.LT.0.0D0) COSARG=-COSARG
                      IF(COSARG.GT.1.0D0) COSARG=1.0D0
                      WRITE(OUTLYNE,1500)SF,RAYRAY(1,SF),RAYRAY(2,SF)
     1                ,DACOS(COSARG),XA,
     2                (YA)
                  END IF
                  IF(TANG) THEN
                      COSARG=RAYRAY(9,SF)
                      IF(COSARG.LT.0.0D0) COSARG=-COSARG
                      IF(COSARG.GT.1.0D0) COSARG=1.0D0
                      WRITE(OUTLYNE,1500)SF,RAYRAY(1,SF),RAYRAY(2,SF)
     1                ,DTAN(DACOS(COSARG)),(DTAN(XA)),
     2                (DTAN(YA))
                  END IF
                  CALL SHOWIT(0)
              END IF
              IF(WC.EQ.'PRXYIP') THEN
                  IF(HEADIN) WRITE(OUTLYNE,7000)
                  IF(HEADIN) CALL SHOWIT(0)
                  XA=RAYRAY(11,SF)
                  YA=RAYRAY(12,SF)
                  CALL RS(XA,YA)
                  IF(DEG) THEN
                      COSARG=RAYRAY(10,SF)
                      IF(COSARG.LT.0.0D0) COSARG=-COSARG
                      IF(COSARG.GT.1.0D0) COSARG=1.0D0
                      WRITE(OUTLYNE,1500)SF,RAYRAY(1,SF),RAYRAY(2,SF)
     1                ,((180.0D0/PII)*DACOS(COSARG)),
     2                ((180.0D0/PII)*XA),
     3                ((180.0D0/PII)*YA)
                  END IF
                  IF(RAD) THEN
                      COSARG=RAYRAY(10,SF)
                      IF(COSARG.LT.0.0D0) COSARG=-COSARG
                      IF(COSARG.GT.1.0D0) COSARG=1.0D0
                      WRITE(OUTLYNE,1500)SF,RAYRAY(1,SF),RAYRAY(2,SF)
     1                ,DACOS(COSARG),(XA),
     2                (YA)
                  END IF
                  IF(TANG) THEN
                      COSARG=RAYRAY(10,SF)
                      IF(COSARG.LT.0.0D0) COSARG=-COSARG
                      IF(COSARG.GT.1.0D0) COSARG=1.0D0
                      WRITE(OUTLYNE,1500)SF,RAYRAY(1,SF),RAYRAY(2,SF)
     1                ,DTAN(DACOS(COSARG)),(DTAN(XA)),
     2                (DTAN(YA))
                  END IF
                  CALL SHOWIT(0)
              END IF
              RETURN
          END IF
          IF(SQ.EQ.0.AND.DF1.NE.1) THEN
              I=INT(W1)
              SF=NEWIMG
              IF(I.GT.SF.OR.I.LT.NEWOBJ) THEN
                  WRITE(OUTLYNE,*)'SURFACE NUMBER BEYOND LEGAL RANGE'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(WC.EQ.'PRXYZ') THEN
                  IF(HEADIN) WRITE(OUTLYNE,5000)
                  IF(HEADIN) CALL SHOWIT(0)
                  XA=RAYRAY(11,I)
                  YA=RAYRAY(12,I)
                  CALL RS(XA,YA)
                  IF(DEG) THEN
                      WRITE(OUTLYNE,2000)I,RAYRAY(1,I),RAYRAY(2,I)
     1                ,RAYRAY(3,I),((180.0D0/PII)*XA),
     2                ((180.0D0/PII)*YA)
                  END IF
                  IF(RAD) THEN
                      WRITE(OUTLYNE,2000)I,RAYRAY(1,I),RAYRAY(2,I)
     1                ,RAYRAY(3,I),(XA),
     2                (YA)
                  END IF
                  IF(TANG) THEN
                      WRITE(OUTLYNE,2000)I,RAYRAY(1,I),RAYRAY(2,I)
     1                ,RAYRAY(3,I),(DTAN(XA)),
     2                (DTAN(YA))
                  END IF
                  CALL SHOWIT(0)
              END IF
              IF(WC.EQ.'PRXYI') THEN
                  IF(HEADIN) WRITE(OUTLYNE,6000)
                  IF(HEADIN) CALL SHOWIT(0)
                  XA=RAYRAY(11,I)
                  YA=RAYRAY(12,I)
                  CALL RS(XA,YA)
                  IF(DEG) THEN
                      COSARG=RAYRAY(9,I)
                      IF(COSARG.LT.0.0D0) COSARG=-COSARG
                      IF(COSARG.GT.1.0D0) COSARG=1.0D0
                      WRITE(OUTLYNE,2000)I,RAYRAY(1,I),RAYRAY(2,I)
     1                ,((180.0D0/PII)*DACOS(COSARG)),
     2                ((180.0D0/PII)*XA),
     3                ((180.0D0/PII)*YA)
                  END IF
                  IF(RAD) THEN
                      COSARG=RAYRAY(9,I)
                      IF(COSARG.LT.0.0D0) COSARG=-COSARG
                      IF(COSARG.GT.1.0D0) COSARG=1.0D0
                      WRITE(OUTLYNE,2000)I,RAYRAY(1,I),RAYRAY(2,I)
     1                ,DACOS(COSARG),(XA),
     2                (YA)
                  END IF
                  IF(TANG) THEN
                      COSARG=RAYRAY(9,I)
                      IF(COSARG.LT.0.0D0) COSARG=-COSARG
                      IF(COSARG.GT.1.0D0) COSARG=1.0D0
                      WRITE(OUTLYNE,2000)I,RAYRAY(1,I),RAYRAY(2,I)
     1                ,DTAN(DACOS(COSARG)),(DTAN(XA)),
     2                (DTAN(YA))
                  END IF
                  CALL SHOWIT(0)
              END IF
              IF(WC.EQ.'PRXYIP') THEN
                  XA=RAYRAY(11,I)
                  YA=RAYRAY(12,I)
                  CALL RS(XA,YA)
                  IF(HEADIN) WRITE(OUTLYNE,7000)
                  IF(HEADIN) CALL SHOWIT(0)
                  IF(DEG) THEN
                      COSARG=RAYRAY(10,I)
                      IF(COSARG.LT.0.0D0) COSARG=-COSARG
                      IF(COSARG.GT.1.0D0) COSARG=1.0D0
                      WRITE(OUTLYNE,2000)I,RAYRAY(1,I),RAYRAY(2,I)
     1                ,((180.0D0/PII)*DACOS(COSARG)),
     2                ((180.0D0/PII)*XA),
     3                ((180.0D0/PII)*YA)
                  END IF
                  IF(RAD) THEN
                      COSARG=RAYRAY(10,I)
                      IF(COSARG.LT.0.0D0) COSARG=-COSARG
                      IF(COSARG.GT.1.0D0) COSARG=1.0D0
                      WRITE(OUTLYNE,2000)I,RAYRAY(1,I),RAYRAY(2,I)
     1                ,DACOS(COSARG),XA,
     2                (YA)
                  END IF
                  IF(TANG) THEN
                      COSARG=RAYRAY(10,I)
                      IF(COSARG.LT.0.0D0) COSARG=-COSARG
                      IF(COSARG.GT.1.0D0) COSARG=1.0D0
                      WRITE(OUTLYNE,2000)I,RAYRAY(1,I),RAYRAY(2,I)
     1                ,DTAN(DACOS(COSARG)),(DTAN(XA)),
     2                (DTAN(YA))
                  END IF
                  CALL SHOWIT(0)
              END IF
              RETURN
          END IF
          IF(WQ.EQ.'P'.AND.DF1.EQ.1) THEN
C       OUTPUT IMAGE SURFACE WITH FRACTIONAL OUTPUT HEADER
              SF=NEWIMG
              IF(WC.EQ.'PRXYZ') THEN
                  XA=RAYRAY(11,SF)
                  YA=RAYRAY(12,SF)
                  CALL RS(XA,YA)
                  IF(DEG) THEN
                      WRITE(OUTLYNE,1600)IWAVNM,RELY,RELX
                      CALL SHOWIT(0)
                      IF(HEADIN) WRITE(OUTLYNE,5000)
                      IF(HEADIN) CALL SHOWIT(0)
                      WRITE(OUTLYNE,1500)SF,RAYRAY(1,SF),RAYRAY(2,SF)
     1                ,RAYRAY(3,SF),((180.0D0/PII)*XA),
     2                ((180.0D0/PII)*YA)
                      CALL SHOWIT(0)
                  END IF
                  IF(RAD) THEN
                      WRITE(OUTLYNE,1600)IWAVNM,RELY,RELX
                      CALL SHOWIT(0)
                      IF(HEADIN) WRITE(OUTLYNE,5000)
                      IF(HEADIN) CALL SHOWIT(0)
                      WRITE(OUTLYNE,1500)SF,RAYRAY(1,SF),RAYRAY(2,SF)
     1                ,RAYRAY(3,SF),(XA),
     2                (YA)
                      CALL SHOWIT(0)
                  END IF
                  IF(TANG) THEN
                      WRITE(OUTLYNE,1600)IWAVNM,RELY,RELX
                      CALL SHOWIT(0)
                      IF(HEADIN) WRITE(OUTLYNE,5000)
                      IF(HEADIN) CALL SHOWIT(0)
                      WRITE(OUTLYNE,1500)SF,RAYRAY(1,SF),RAYRAY(2,SF)
     1                ,RAYRAY(3,SF),(DTAN(XA)),
     2                (DTAN(YA))
                      CALL SHOWIT(0)
                  END IF
              END IF
              IF(WC.EQ.'PRXYI') THEN
                  IF(DEG) THEN
                      WRITE(OUTLYNE,1600)IWAVNM,RELY,RELX
                      CALL SHOWIT(0)
                      XA=RAYRAY(11,SF)
                      YA=RAYRAY(12,SF)
                      CALL RS(XA,YA)
                      IF(HEADIN) WRITE(OUTLYNE,6000)
                      IF(HEADIN) CALL SHOWIT(0)
                      COSARG=RAYRAY(9,SF)
                      IF(COSARG.LT.0.0D0) COSARG=-COSARG
                      IF(COSARG.GT.1.0D0) COSARG=1.0D0
                      WRITE(OUTLYNE,1500)SF,RAYRAY(1,SF),RAYRAY(2,SF)
     1                ,((180.0D0/PII)*DACOS(COSARG)),
     2                ((180.0D0/PII)*XA),
     3                ((180.0D0/PII)*YA)
                      CALL SHOWIT(0)
                  END IF
                  IF(RAD) THEN
                      WRITE(OUTLYNE,1600)IWAVNM,RELY,RELX
                      CALL SHOWIT(0)
                      IF(HEADIN) WRITE(OUTLYNE,6000)
                      IF(HEADIN) CALL SHOWIT(0)
                      COSARG=RAYRAY(9,SF)
                      IF(COSARG.LT.0.0D0) COSARG=-COSARG
                      IF(COSARG.GT.1.0D0) COSARG=1.0D0
                      WRITE(OUTLYNE,1500)SF,RAYRAY(1,SF),RAYRAY(2,SF)
     1                ,DACOS(COSARG),(XA),
     2                (YA)
                      CALL SHOWIT(0)
                  END IF
                  IF(TANG) THEN
                      WRITE(OUTLYNE,1600)IWAVNM,RELY,RELX
                      CALL SHOWIT(0)
                      IF(HEADIN) WRITE(OUTLYNE,6000)
                      IF(HEADIN) CALL SHOWIT(0)
                      COSARG=RAYRAY(9,SF)
                      IF(COSARG.LT.0.0D0) COSARG=-COSARG
                      IF(COSARG.GT.1.0D0) COSARG=1.0D0
                      WRITE(OUTLYNE,1500)SF,RAYRAY(1,SF),RAYRAY(2,SF)
     1                ,DTAN(DACOS(COSARG)),(DTAN(XA)),
     2                (DTAN(YA))
                      CALL SHOWIT(0)
                  END IF
              END IF
              IF(WC.EQ.'PRXYIP') THEN
                  IF(DEG) THEN
                      WRITE(OUTLYNE,1600)IWAVNM,RELY,RELX
                      CALL SHOWIT(0)
                      IF(HEADIN) WRITE(OUTLYNE,7000)
                      IF(HEADIN) CALL SHOWIT(0)
                      XA=RAYRAY(11,SF)
                      YA=RAYRAY(12,SF)
                      CALL RS(XA,YA)
                      COSARG=RAYRAY(10,SF)
                      IF(COSARG.LT.0.0D0) COSARG=-COSARG
                      IF(COSARG.GT.1.0D0) COSARG=1.0D0
                      WRITE(OUTLYNE,1500)SF,RAYRAY(1,SF),RAYRAY(2,SF)
     1                ,((180.0D0/PII)*DACOS(COSARG)),
     2                ((180.0D0/PII)*XA),
     3                ((180.0D0/PII)*YA)
                      CALL SHOWIT(0)
                  END IF
                  XA=RAYRAY(11,SF)
                  YA=RAYRAY(12,SF)
                  CALL RS(XA,YA)
                  IF(RAD) THEN
                      WRITE(OUTLYNE,1600)IWAVNM,RELY,RELX
                      CALL SHOWIT(0)
                      IF(HEADIN) WRITE(OUTLYNE,7000)
                      IF(HEADIN) CALL SHOWIT(0)
                      COSARG=RAYRAY(10,SF)
                      IF(COSARG.LT.0.0D0) COSARG=-COSARG
                      IF(COSARG.GT.1.0D0) COSARG=1.0D0
                      WRITE(OUTLYNE,1500)SF,RAYRAY(1,SF),RAYRAY(2,SF)
     1                ,DACOS(COSARG),(XA),
     2                (YA)
                      CALL SHOWIT(0)
                  END IF
                  IF(TANG) THEN
                      WRITE(OUTLYNE,1600)IWAVNM,RELY,RELX
                      CALL SHOWIT(0)
                      IF(HEADIN) WRITE(OUTLYNE,7000)
                      IF(HEADIN) CALL SHOWIT(0)
                      COSARG=RAYRAY(10,SF)
                      IF(COSARG.LT.0.0D0) COSARG=-COSARG
                      IF(COSARG.GT.1.0D0) COSARG=1.0D0
                      WRITE(OUTLYNE,1500)SF,RAYRAY(1,SF),RAYRAY(2,SF)
     1                ,DTAN(DACOS(COSARG)),(DTAN(XA)),
     2                (DTAN(YA))
                      CALL SHOWIT(0)
                  END IF
              END IF
              RETURN
          END IF
          IF(WQ.EQ.'P'.AND.DF1.NE.1) THEN
              I=INT(W1)
              SF=NEWIMG
              IF(I.GT.SF.OR.I.LT.NEWOBJ) THEN
                  WRITE(OUTLYNE,*)'SURFACE NUMBER BEYOND LEGAL RANGE'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(WC.EQ.'PRXYZ') THEN
                  XA=RAYRAY(11,I)
                  YA=RAYRAY(12,I)
                  CALL RS(XA,YA)
                  IF(DEG) THEN
                      WRITE(OUTLYNE,1600)IWAVNM,RELY,RELX
                      CALL SHOWIT(0)
                      IF(HEADIN) WRITE(OUTLYNE,5000)
                      IF(HEADIN) CALL SHOWIT(0)
                      WRITE(OUTLYNE,2000)I,RAYRAY(1,I),RAYRAY(2,I)
     1                ,RAYRAY(3,I),((180.0D0/PII)*XA),
     2                ((180.0D0/PII)*YA)
                      CALL SHOWIT(0)
                  END IF
                  IF(RAD) THEN
                      WRITE(OUTLYNE,1600)IWAVNM,RELY,RELX
                      CALL SHOWIT(0)
                      IF(HEADIN) WRITE(OUTLYNE,5000)
                      IF(HEADIN) CALL SHOWIT(0)
                      WRITE(OUTLYNE,2000)I,RAYRAY(1,I),RAYRAY(2,I)
     1                ,RAYRAY(3,I),(XA),
     2                (YA)
                      CALL SHOWIT(0)
                  END IF
                  IF(TANG) THEN
                      WRITE(OUTLYNE,1600)IWAVNM,RELY,RELX
                      CALL SHOWIT(0)
                      IF(HEADIN) WRITE(OUTLYNE,5000)
                      IF(HEADIN) CALL SHOWIT(0)
                      WRITE(OUTLYNE,2000)I,RAYRAY(1,I),RAYRAY(2,I)
     1                ,RAYRAY(3,I),(DTAN(XA)),
     2                (DTAN(YA))
                      CALL SHOWIT(0)
                  END IF
              END IF
              IF(WC.EQ.'PRXYI') THEN
                  XA=RAYRAY(11,I)
                  YA=RAYRAY(12,I)
                  CALL RS(XA,YA)
                  IF(DEG) THEN
                      WRITE(OUTLYNE,1600)IWAVNM,RELY,RELX
                      CALL SHOWIT(0)
                      IF(HEADIN) WRITE(OUTLYNE,6000)
                      IF(HEADIN) CALL SHOWIT(0)
                      COSARG=RAYRAY(9,I)
                      IF(COSARG.LT.0.0D0) COSARG=-COSARG
                      IF(COSARG.GT.1.0D0) COSARG=1.0D0
                      WRITE(OUTLYNE,2000)I,RAYRAY(1,I),RAYRAY(2,I)
     1                ,((180.0D0/PII)*DACOS(COSARG)),
     2                ((180.0D0/PII)*XA),
     3                ((180.0D0/PII)*YA)
                      CALL SHOWIT(0)
                  END IF
                  IF(RAD) THEN
                      WRITE(OUTLYNE,1600)IWAVNM,RELY,RELX
                      CALL SHOWIT(0)
                      IF(HEADIN) WRITE(OUTLYNE,6000)
                      IF(HEADIN) CALL SHOWIT(0)
                      COSARG=RAYRAY(9,I)
                      IF(COSARG.LT.0.0D0) COSARG=-COSARG
                      IF(COSARG.GT.1.0D0) COSARG=1.0D0
                      WRITE(OUTLYNE,2000)I,RAYRAY(1,I),RAYRAY(2,I)
     1                ,DACOS(COSARG),(XA),
     2                (YA)
                      CALL SHOWIT(0)
                  END IF
                  IF(TANG) THEN
                      WRITE(OUTLYNE,1600)IWAVNM,RELY,RELX
                      CALL SHOWIT(0)
                      IF(HEADIN) WRITE(OUTLYNE,6000)
                      IF(HEADIN) CALL SHOWIT(0)
                      COSARG=RAYRAY(9,I)
                      IF(COSARG.LT.0.0D0) COSARG=-COSARG
                      IF(COSARG.GT.1.0D0) COSARG=1.0D0
                      WRITE(OUTLYNE,2000)I,RAYRAY(1,I),RAYRAY(2,I)
     1                ,DTAN(DACOS(COSARG)),(DTAN(XA)),
     2                (DTAN(YA))
                      CALL SHOWIT(0)
                  END IF
              END IF
              IF(WC.EQ.'PRXYIP') THEN
                  XA=RAYRAY(11,I)
                  YA=RAYRAY(12,I)
                  CALL RS(XA,YA)
                  IF(DEG) THEN
                      WRITE(OUTLYNE,1600)IWAVNM,RELY,RELX
                      CALL SHOWIT(0)
                      IF(HEADIN) WRITE(OUTLYNE,7000)
                      IF(HEADIN) CALL SHOWIT(0)
                      COSARG=RAYRAY(10,I)
                      IF(COSARG.LT.0.0D0) COSARG=-COSARG
                      IF(COSARG.GT.1.0D0) COSARG=1.0D0
                      WRITE(OUTLYNE,2000)I,RAYRAY(1,I),RAYRAY(2,I)
     1                ,((180.0D0/PII)*DACOS(COSARG)),
     2                ((180.0D0/PII)*XA),
     3                ((180.0D0/PII)*YA)
                      CALL SHOWIT(0)
                  END IF
                  IF(RAD) THEN
                      WRITE(OUTLYNE,1600)IWAVNM,RELY,RELX
                      CALL SHOWIT(0)
                      IF(HEADIN) WRITE(OUTLYNE,7000)
                      IF(HEADIN) CALL SHOWIT(0)
                      COSARG=RAYRAY(10,I)
                      IF(COSARG.LT.0.0D0) COSARG=-COSARG
                      IF(COSARG.GT.1.0D0) COSARG=1.0D0
                      WRITE(OUTLYNE,2000)I,RAYRAY(1,I),RAYRAY(2,I)
     1                ,DACOS(COSARG),(XA),
     2                (YA)
                      CALL SHOWIT(0)
                  END IF
                  IF(TANG) THEN
                      WRITE(OUTLYNE,1600)IWAVNM,RELY,RELX
                      CALL SHOWIT(0)
                      IF(HEADIN) WRITE(OUTLYNE,7000)
                      IF(HEADIN) CALL SHOWIT(0)
                      COSARG=RAYRAY(10,I)
                      IF(COSARG.LT.0.0D0) COSARG=-COSARG
                      IF(COSARG.GT.1.0D0) COSARG=1.0D0
                      WRITE(OUTLYNE,2000)I,RAYRAY(1,I),RAYRAY(2,I)
     1                ,DTAN(DACOS(COSARG)),(DTAN(XA)),
     2                (DTAN(YA))
                      CALL SHOWIT(0)
                  END IF
              END IF
              RETURN
          END IF
 1500     FORMAT(I3,2X,G13.6,2X,G13.6,2X,G13.6,2X,
     1    G13.6,2X,G13.6)
 1600     FORMAT('RAY AT WAVL # ',I1,
     1    ' (REL AP Y, REL AP X) = ( ',G13.6,', ',G13.6,' )')
 2000     FORMAT(I3,2X,G13.6,2X,G13.6,2X,G13.6,2X,
     1    G13.6,2X,G13.6)
 5000     FORMAT('SURF',6X,'X ',13X,'Y  ',12X,'Z  ',
     1    11X,'XANG',11X,'YANG')
 6000     FORMAT('SURF',6X,'X ',13X,'Y  ',12X,'IANG',
     1    10X,'XANG',11X,'YANG')
 7000     FORMAT('SURF',6X,'X ',13X,'Y  ',12X,'IPANG',
     1    9X,'XANG',11X,'YANG')
 5001     FORMAT('RAYTRACE DATA (XZ AND YZ-PLANE)',
     1    ' - (CFG #',I2,')')
 5003     FORMAT('ANGULAR OUTPUT MODE IS ',A8)
 5002     FORMAT(
     1    'SLOPES/ANGLES  MEASURED WITH RESPECT TO THE Z-AXIS')
 2501     FORMAT(1X)
      END
C SUB PRZ.FOR
      SUBROUTINE PRZ
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE PRY. THIS SUBROUTINE IMPLEMENTS
C       THE RAY TRACE 'PRZ' PRINTOUT
C       AT THE CMD LEVEL.
C
          CHARACTER AMODE*8
          INTEGER SF,IWAVNM,I
C
          COMMON/CMODD/AMODE
C
          REAL*8 OPD,DUMMY
C
          LOGICAL DEG,RAD,TANG,OPDERROR
C
          COMMON/ANGMOD/DEG,RAD,TANG
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
C       THE PRZ COMMAND ACCEPTS QUALIFIER OR NUMERIC
C       INPUT IN AN EITHER OR MODE. IT DOES NOT ACCEPT SRTING
C       INPUT. THE VALID QUALIFIERS ARE "ALL" AND "OBJ" AND "OB"
C       THE VALID NUMERIC INPUT IS THE SURFACE NUMBER FOR
C       WHICH IT IS DESIRED TO PRODUCE OUTPUT. THE QUALIFIER "P"
C       CAUSES RELATIVE APERTURE COORDINATES TO BE PRINTED AS WELL.
C
          IWAVNM=INT(WVN)
          IF(WC.EQ.'PRZ') THEN
              IF(SST.EQ.1.OR.S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
                  WRITE(OUTLYNE,*)
     1            '"PRZ" ONLY TAKES QUALIFIER OR NUMERIC WORD #1 INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(SQ.EQ.1.AND.S1.EQ.1.AND.WQ.NE.'P') THEN
                  WRITE(OUTLYNE,*)
     1            '"PRZ" ONLY TAKES QUALIFIER OR NUMERIC WORD #1 INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'BUT NOT BOTH'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
C
C
C       SET AMODE
          IF(DEG) THEN
              AMODE='DEGREES '
          END IF
          IF(RAD)  THEN
              AMODE='RADIANS '
          END IF
          IF(TANG) THEN
              AMODE='TANGENT '
          END IF
C
          IF(SYSTEM1(20).EQ.0.0) THEN
              WRITE(OUTLYNE,*)'LENS SYSTEM HAS NO SURFACES'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'NO RAY DATA CAN EXIST'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(.NOT.RAYEXT) THEN
              WRITE(OUTLYNE,*)'NO RAY DATA EXISTS TO DISPLAY'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SQ.EQ.1.AND.WQ.EQ.'ALL') THEN
              SF=NEWIMG
              WRITE(OUTLYNE,5001) INT(F12)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,2501)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,5000)
              CALL SHOWIT(0)
              DO 10 I=NEWOBJ,SF
                  WRITE(OUTLYNE,2000)I,RAYRAY(3,I),RAYRAY(7,I)
                  CALL SHOWIT(0)
 10           CONTINUE
              IF(.NOT.NULL) THEN
C       CALCULATE AND PRINT RAY OPD IN WAVES AT WAVELENGTH
C       THAT RAY WAS TRACED.
                  CALL GETOPD(DUMMY,OPD,OPDERROR)
C
C       OUTPUT OPD OF CURRENT RAY AND WAVELENGTH IN MICRONS
                  IF(.NOT.OPDERROR) THEN
                      WRITE(OUTLYNE,1650) OPD,SYSTEM1(IWAVNM)
                      CALL SHOWIT(0)
                  END IF
              END IF
C
              RETURN
          END IF
          IF(SQ.EQ.1.AND.WQ.EQ.'OBJ'.OR.SQ.EQ.1.AND.WQ.EQ.'OB') THEN
              SF=NEWOBJ
              IF(HEADIN) WRITE(OUTLYNE,5000)
              IF(HEADIN) CALL SHOWIT(0)
              WRITE(OUTLYNE,1500)SF,RAYRAY(3,SF),RAYRAY(7,SF)
              CALL SHOWIT(0)
              RETURN
          END IF
          IF(WQ.NE.'OBJ'.AND.WQ.NE.' '.AND.
     1    WQ.NE.'ALL'.AND.WQ.NE.'OB'.AND.WQ.NE.'P') THEN
              WRITE(OUTLYNE,*)'INVALID QUALIFIER WORD'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SQ.EQ.0.AND.DF1.EQ.1) THEN
C       OUTPUT IMAGE SURFACE
              SF=NEWIMG
              IF(HEADIN) WRITE(OUTLYNE,5000)
              IF(HEADIN) CALL SHOWIT(0)
              WRITE(OUTLYNE,1500)SF,RAYRAY(3,SF),RAYRAY(7,SF)
              CALL SHOWIT(0)
              RETURN
          END IF
          IF(SQ.EQ.0.AND.DF1.NE.1) THEN
              I=INT(W1)
              SF=NEWIMG
              IF(I.GT.SF.OR.I.LT.NEWOBJ) THEN
                  WRITE(OUTLYNE,*)'SURFACE NUMBER BEYOND LEGAL RANGE'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(HEADIN) WRITE(OUTLYNE,5000)
              IF(HEADIN) CALL SHOWIT(0)
              WRITE(OUTLYNE,2000)I,RAYRAY(3,I),RAYRAY(7,I)
              CALL SHOWIT(0)
              RETURN
          END IF
          IF(WQ.EQ.'P'.AND.DF1.EQ.1) THEN
C       OUTPUT IMAGE SURFACE WITH FRACTIONAL OUTPUT HEADER
              SF=NEWIMG
              WRITE(OUTLYNE,1600)IWAVNM,RELY,RELX
              CALL SHOWIT(0)
              IF(HEADIN) WRITE(OUTLYNE,5000)
              IF(HEADIN) CALL SHOWIT(0)
              WRITE(OUTLYNE,1500)SF,RAYRAY(3,SF),RAYRAY(7,SF)
              CALL SHOWIT(0)
              RETURN
          END IF
          IF(WQ.EQ.'P'.AND.DF1.NE.1) THEN
              I=INT(W1)
              SF=NEWIMG
              IF(I.GT.SF.OR.I.LT.NEWOBJ) THEN
                  WRITE(OUTLYNE,*)'SURFACE NUMBER BEYOND LEGAL RANGE'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              WRITE(OUTLYNE,1600)IWAVNM,RELY,RELX
              CALL SHOWIT(0)
              IF(HEADIN) WRITE(OUTLYNE,5000)
              IF(HEADIN) CALL SHOWIT(0)
              WRITE(OUTLYNE,2000)I,RAYRAY(3,I),RAYRAY(7,I)
              CALL SHOWIT(0)
              RETURN
          END IF
 1500     FORMAT(I3,2X,G13.6,2X,G13.6)
 1600     FORMAT('RAY AT WAVL # ',I1,
     1    ' (REL AP Y, REL AP X) = ( ',G13.6,', ',G13.6,' )')
 1650     FORMAT
     1    ('OPD = ',G13.6,' WAVES AT WAVELENGTH = ',G13.6,' MICRONS')
 2000     FORMAT(I3,2X,G13.6,2X,G13.6)
 5000     FORMAT('SURF',6X,'Z ',13X,'OPL')
 5001     FORMAT('RAYTRACE DATA (ALONG Z-AXIS)',
     1    ' - (CFG #',I2,')')
 2501     FORMAT(1X)
      END
      SUBROUTINE RS(XA,YA)
          IMPLICIT NONE
          REAL*8 XA,YA
          INCLUDE 'datmai.inc'
          IF(XA.GT.PII) XA=XA-(TWOPII)
          IF(XA.LT.-PII) XA=XA+(TWOPII)
          IF(XA.EQ.TWOPII) XA=0.0D0
          IF(YA.GT.PII) YA=YA-(TWOPII)
          IF(YA.LT.-PII) YA=YA+(TWOPII)
          IF(YA.EQ.TWOPII) YA=0.0D0
          RETURN
      END
C SUB PRFLUX.FOR
      SUBROUTINE PRFLUX
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE PRFLUX. THIS SUBROUTINE IMPLEMENTS
C       THE RAY TRACE PRINTOUT WHICH LISTS THE RAY ENERGY
C       IN THE LOCAL X AND Y DIRECTIONS AT EACH SURFACE
C       AFTER SURFACE INTERACTION
C       THIS OPERATES AT THE CMD LEVEL.
C
          REAL*8 XA,YA
C
          INTEGER
     6    SF,IWAVNM,I
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
C       THE PRFLUX COMMAND ACCEPTS QUALIFIER OR NUMERIC
C       INPUT IN AN EITHER OR MODE. IT DOES NOT ACCEPT SRTING
C       INPUT. THE VALID QUALIFIERS ARE "ALL" AND "OBJ" AND "OB"
C       THE VALID NUMERIC INPUT IS THE SURFACE NUMBER FOR
C       WHICH IT IS DESIRED TO PRODUCE OUTPUT. THE QUALIFIER "P"
C       CAUSES RELATIVE APERTURE COORDINATES TO BE PRINTED AS WELL.
C
          IWAVNM=INT(WVN)
          IF(SST.EQ.1.OR.S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
              WRITE(OUTLYNE,*)
     1        '"PRFLUX" ONLY TAKE QUALIFIER OR NUMERIC WORD #1 INPUT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SQ.EQ.1.AND.S1.EQ.1.AND.WQ.NE.'P') THEN
              WRITE(OUTLYNE,*)
     1        '"PRFLUX" ONLY TAKE QUALIFIER OR NUMERIC WORD #1 INPUT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'BUT NOT BOTH'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
          IF(SYSTEM1(20).EQ.0.0) THEN
              WRITE(OUTLYNE,*)'LENS SYSTEM HAS NO SURFACES'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'NO RAY DATA CAN EXIST'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(.NOT.RAYEXT) THEN
              WRITE(OUTLYNE,*)'NO RAY DATA EXISTS TO DISPLAY'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SQ.EQ.1.AND.WQ.EQ.'ALL') THEN
              SF=NEWIMG
              WRITE(OUTLYNE,5001) INT(F12)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,2501)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,5000)
              CALL SHOWIT(0)
              DO 10 I=NEWOBJ,SF
                  XA=RAYRAY(11,I)
                  YA=RAYRAY(12,I)
                  CALL RS(XA,YA)
                  WRITE(OUTLYNE,2000)I,RAYRAY(25,I)
                  CALL SHOWIT(0)
                  XA=RAYRAY(11,I)
                  YA=RAYRAY(12,I)
                  CALL RS(XA,YA)
 10           CONTINUE
              RETURN
          END IF
          IF(SQ.EQ.1.AND.WQ.EQ.'OBJ'.OR.SQ.EQ.1.AND.WQ.EQ.'OB') THEN
              SF=NEWOBJ
              IF(HEADIN) WRITE(OUTLYNE,5000)
              IF(HEADIN) CALL SHOWIT(0)
              XA=RAYRAY(11,SF)
              YA=RAYRAY(12,SF)
              CALL RS(XA,YA)
              WRITE(OUTLYNE,1500)SF,RAYRAY(25,SF)
              CALL SHOWIT(0)
              RETURN
          END IF
          IF(WQ.NE.'OBJ'.AND.WQ.NE.' '.AND.
     1    WQ.NE.'ALL'.AND.WQ.NE.'OB'.AND.WQ.NE.'P') THEN
              WRITE(OUTLYNE,*)'INVALID QUALIFIER WORD'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SQ.EQ.0.AND.DF1.EQ.1) THEN
C       OUTPUT IMAGE SURFACE
              SF=NEWIMG
              IF(HEADIN) WRITE(OUTLYNE,5000)
              IF(HEADIN) CALL SHOWIT(0)
              XA=RAYRAY(11,SF)
              YA=RAYRAY(12,SF)
              CALL RS(XA,YA)
              WRITE(OUTLYNE,1500)SF,RAYRAY(25,SF)
              CALL SHOWIT(0)
              RETURN
          END IF
          IF(SQ.EQ.0.AND.DF1.NE.1) THEN
              I=INT(W1)
              SF=NEWIMG
              IF(I.GT.SF.OR.I.LT.NEWOBJ) THEN
                  WRITE(OUTLYNE,*)'SURFACE NUMBER BEYOND LEGAL RANGE'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(HEADIN) WRITE(OUTLYNE,5000)
              IF(HEADIN) CALL SHOWIT(0)
              XA=RAYRAY(11,I)
              YA=RAYRAY(12,I)
              CALL RS(XA,YA)
              WRITE(OUTLYNE,2000)I,RAYRAY(25,I)
              CALL SHOWIT(0)
              RETURN
          END IF
          IF(WQ.EQ.'P'.AND.DF1.EQ.1) THEN
C       OUTPUT IMAGE SURFACE WITH FRACTIONAL OUTPUT HEADER
              SF=NEWIMG
              XA=RAYRAY(11,SF)
              YA=RAYRAY(12,SF)
              CALL RS(XA,YA)
              WRITE(OUTLYNE,1600)IWAVNM,RELY,RELX
              CALL SHOWIT(0)
              IF(HEADIN) WRITE(OUTLYNE,5000)
              IF(HEADIN) CALL SHOWIT(0)
              WRITE(OUTLYNE,1500)SF,RAYRAY(25,SF)
              CALL SHOWIT(0)
              RETURN
          END IF
          IF(WQ.EQ.'P'.AND.DF1.NE.1) THEN
              I=INT(W1)
              SF=NEWIMG
              IF(I.GT.SF.OR.I.LT.NEWOBJ) THEN
                  WRITE(OUTLYNE,*)'SURFACE NUMBER BEYOND LEGAL RANGE'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              XA=RAYRAY(11,I)
              YA=RAYRAY(12,I)
              CALL RS(XA,YA)
              WRITE(OUTLYNE,1600)IWAVNM,RELY,RELX
              CALL SHOWIT(0)
              IF(HEADIN) WRITE(OUTLYNE,5000)
              IF(HEADIN) CALL SHOWIT(0)
              WRITE(OUTLYNE,2000)I,RAYRAY(25,I)
              CALL SHOWIT(0)
              RETURN
          END IF
 1500     FORMAT(I3,2X,G13.6)
 1600     FORMAT('RAY AT WAVL # ',I1,
     1    ' (REL AP Y, REL AP X) = ( ',G13.6,', ',G13.6,' )')
 2000     FORMAT(I3,12X,G13.6)
 5000     FORMAT('SURF',6X,'RELATIVE RAY ENERGY')
 5001     FORMAT('RAYTRACE RELATIVE RAY ENERGY DATA',
     1    ' - (CFG #',I2,')')
 2501     FORMAT(1X)
      END
C SUB PRPOL.FOR
      SUBROUTINE PRPOL
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE PRPOL. THIS SUBROUTINE IMPLEMENTS
C       THE RAY TRACE PRINTOUT WHICH LISTS THE RAY POLARIZATION
C       INFORMATION WHEN POLARIZATION RAY TRACING IS ON.
C       THIS OPERATES AT THE CMD LEVEL.
C
          REAL*8 XA,YA
C
          INTEGER
     6    SF,IWAVNM,I
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
C       THE PRPOL COMMAND ACCEPTS QUALIFIER OR NUMERIC
C       INPUT IN AN EITHER OR MODE. IT DOES NOT ACCEPT STRING
C       INPUT. THE VALID QUALIFIERS ARE "ALL" AND "OBJ" AND "OB"
C       THE VALID NUMERIC INPUT IS THE SURFACE NUMBER FOR
C       WHICH IT IS DESIRED TO PRODUCE OUTPUT. THE QUALIFIER "P"
C       CAUSES RELATIVE APERTURE COORDINATES TO BE PRINTED AS WELL.
C
          IWAVNM=INT(WVN)
          IF(SST.EQ.1.OR.S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
              WRITE(OUTLYNE,*)
     1        '"PRPOL" ONLY TAKE QUALIFIER OR NUMERIC WORD #1 INPUT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SQ.EQ.1.AND.S1.EQ.1.AND.WQ.NE.'P') THEN
              WRITE(OUTLYNE,*)
     1        '"PRPOL" ONLY TAKE QUALIFIER OR NUMERIC WORD #1 INPUT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'BUT NOT BOTH'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
          IF(SYSTEM1(20).EQ.0.0) THEN
              WRITE(OUTLYNE,*)'LENS SYSTEM HAS NO SURFACES'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'NO RAY DATA CAN EXIST'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(.NOT.RAYEXT) THEN
              WRITE(OUTLYNE,*)'NO RAY DATA EXISTS TO DISPLAY'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(.NOT.POLEXT) THEN
              WRITE(OUTLYNE,*)'NO POLARIZATION DATA EXISTS TO DISPLAY'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SQ.EQ.1.AND.WQ.EQ.'ALL') THEN
              SF=NEWIMG
              WRITE(OUTLYNE,5001) INT(F12)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,2501)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,5000)
              CALL SHOWIT(0)
              DO 10 I=NEWOBJ,SF
                  XA=RAYRAY(11,I)
                  YA=RAYRAY(12,I)
                  CALL RS(XA,YA)
                  WRITE(OUTLYNE,2000)I,RAYRAY(34,I),RAYRAY(35,I),
     1            RAYRAY(36,I),RAYRAY(37,I),RAYRAY(38,I)
                  CALL SHOWIT(0)
                  XA=RAYRAY(11,I)
                  YA=RAYRAY(12,I)
                  CALL RS(XA,YA)
 10           CONTINUE
              RETURN
          END IF
          IF(SQ.EQ.1.AND.WQ.EQ.'OBJ'.OR.SQ.EQ.1.AND.WQ.EQ.'OB') THEN
              SF=NEWOBJ
              IF(HEADIN) WRITE(OUTLYNE,5000)
              IF(HEADIN) CALL SHOWIT(0)
              XA=RAYRAY(11,SF)
              YA=RAYRAY(12,SF)
              CALL RS(XA,YA)
              WRITE(OUTLYNE,1500)SF,RAYRAY(34,SF),RAYRAY(35,SF),
     1        RAYRAY(36,SF),RAYRAY(37,SF),RAYRAY(38,SF)
              CALL SHOWIT(0)
              RETURN
          END IF
          IF(WQ.NE.'OBJ'.AND.WQ.NE.' '.AND.
     1    WQ.NE.'ALL'.AND.WQ.NE.'OB'.AND.WQ.NE.'P') THEN
              WRITE(OUTLYNE,*)'INVALID QUALIFIER WORD'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SQ.EQ.0.AND.DF1.EQ.1) THEN
C       OUTPUT IMAGE SURFACE
              SF=NEWIMG
              IF(HEADIN) WRITE(OUTLYNE,5000)
              IF(HEADIN) CALL SHOWIT(0)
              XA=RAYRAY(11,SF)
              YA=RAYRAY(12,SF)
              CALL RS(XA,YA)
              WRITE(OUTLYNE,1500)SF,RAYRAY(34,SF),RAYRAY(35,SF),
     1        RAYRAY(36,SF),RAYRAY(37,SF),RAYRAY(38,SF)
              CALL SHOWIT(0)
              RETURN
          END IF
          IF(SQ.EQ.0.AND.DF1.NE.1) THEN
              I=INT(W1)
              SF=NEWIMG
              IF(I.GT.SF.OR.I.LT.NEWOBJ) THEN
                  WRITE(OUTLYNE,*)'SURFACE NUMBER BEYOND LEGAL RANGE'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(HEADIN) WRITE(OUTLYNE,5000)
              IF(HEADIN) CALL SHOWIT(0)
              XA=RAYRAY(11,I)
              YA=RAYRAY(12,I)
              CALL RS(XA,YA)
              WRITE(OUTLYNE,2000)I,RAYRAY(34,I),RAYRAY(35,I),
     1        RAYRAY(36,I),RAYRAY(37,I),RAYRAY(38,I)
              CALL SHOWIT(0)
              RETURN
          END IF
          IF(WQ.EQ.'P'.AND.DF1.EQ.1) THEN
C       OUTPUT IMAGE SURFACE WITH FRACTIONAL OUTPUT HEADER
              SF=NEWIMG
              XA=RAYRAY(11,SF)
              YA=RAYRAY(12,SF)
              CALL RS(XA,YA)
              WRITE(OUTLYNE,1600)IWAVNM,RELY,RELX
              CALL SHOWIT(0)
              IF(HEADIN) WRITE(OUTLYNE,5000)
              IF(HEADIN) CALL SHOWIT(0)
              WRITE(OUTLYNE,1500)SF,RAYRAY(34,SF),RAYRAY(35,SF),
     1        RAYRAY(36,SF),RAYRAY(37,SF),RAYRAY(38,SF)
              CALL SHOWIT(0)
              RETURN
          END IF
          IF(WQ.EQ.'P'.AND.DF1.NE.1) THEN
              I=INT(W1)
              SF=NEWIMG
              IF(I.GT.SF.OR.I.LT.NEWOBJ) THEN
                  WRITE(OUTLYNE,*)'SURFACE NUMBER BEYOND LEGAL RANGE'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              XA=RAYRAY(11,I)
              YA=RAYRAY(12,I)
              CALL RS(XA,YA)
              WRITE(OUTLYNE,1600)IWAVNM,RELY,RELX
              CALL SHOWIT(0)
              IF(HEADIN) WRITE(OUTLYNE,5000)
              IF(HEADIN) CALL SHOWIT(0)
              WRITE(OUTLYNE,2000)I,RAYRAY(34,I),RAYRAY(35,I),
     1        RAYRAY(36,I),RAYRAY(37,I),RAYRAY(38,I)
              CALL SHOWIT(0)
              RETURN
          END IF
 1500     FORMAT(I3,2X,G13.6,2X,G13.6,2X,G13.6,2X,G13.6,2X,G13.6)
 2000     FORMAT(I3,2X,G13.6,2X,G13.6,2X,G13.6,2X,G13.6,2X,G13.6)
 1600     FORMAT('RAY AT WAVL # ',I1,
     1    ' (REL AP Y, REL AP X) = ( ',G13.6,', ',G13.6,' )')
 5000     FORMAT('SURF',2X,'FACTOR-PAR.',4X,'FACTOR-PER.',5X,
     1    'PHASE_PAR.',5X,'PHASE_PER',6X,'POLANG(DEG.)')
 5001     FORMAT('RAYTRACE POLARIZATION DATA, PHASE IN RADIANS',
     1    ' - (CFG #',I2,')')
 2501     FORMAT(1X)
      END
C SUB PROPD.FOR
      SUBROUTINE PROPD
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE PROPD WHICH IMPLEMENTS THE OPD
C       COMMAND AT THE CMD LEVEL.
C
          INTEGER J,JJ,WWRF,WWVN
C
          REAL*8 LEN,LENW,WW,WAVE
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
C               CHECK FOR PRESENCE OF STRING,QUALIFIER,OR NUMERIC
C               INPUT AND
C               PRINT ERROR AND RETURN IF DISCOVERED.
C
          IF(SQ.EQ.1.OR.SST.EQ.1.OR.SN.EQ.1) THEN
              OUTLYNE=
     1        '"OPD" TAKES NO EXPLICIT INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(INT(LFOB(4)).EQ.1) WWRF=46
          IF(INT(LFOB(4)).EQ.2) WWRF=47
          IF(INT(LFOB(4)).EQ.3) WWRF=48
          IF(INT(LFOB(4)).EQ.4) WWRF=49
          IF(INT(LFOB(4)).EQ.5) WWRF=50
          IF(INT(LFOB(4)).EQ.6) WWRF=71
          IF(INT(LFOB(4)).EQ.7) WWRF=72
          IF(INT(LFOB(4)).EQ.8) WWRF=73
          IF(INT(LFOB(4)).EQ.9) WWRF=74
          IF(INT(LFOB(4)).EQ.10) WWRF=75
          IF(INT(CURLAM).EQ.1) WWVN=46
          IF(INT(CURLAM).EQ.2) WWVN=47
          IF(INT(CURLAM).EQ.3) WWVN=48
          IF(INT(CURLAM).EQ.4) WWVN=49
          IF(INT(CURLAM).EQ.5) WWVN=50
          IF(INT(CURLAM).EQ.6) WWVN=71
          IF(INT(CURLAM).EQ.7) WWVN=72
          IF(INT(CURLAM).EQ.8) WWVN=73
          IF(INT(CURLAM).EQ.9) WWVN=74
          IF(INT(CURLAM).EQ.10) WWVN=75
          IF(RAYEXT.AND.REFEXT) THEN
              LEN=0.0D0
              RCOR=0.0D0
              OCOR=0.0D0
              IF(DABS(ALENS(3,NEWOBJ)).GE.1.0D10) JJ=NEWOBJ+2
              IF(DABS(ALENS(3,NEWOBJ)).LT.1.0D10) JJ=NEWOBJ+1
              DO J=JJ,NEWIMG
                  LEN=LEN+RAYRAY(7,J)
     1            -(REFRY(7,J)*(ALENS(WWVN,J-1)/ALENS(WWRF,J-1)))
              END DO
              IF(SYSTEM1(30).EQ.1.0D0.OR.SYSTEM1(30).EQ.2.0D0) THEN
C       MODE FOCAL
                  RCOR=0.0D0
                  OCOR=0.0D0
                  CALL FOPD
                  LEN=LEN-(OCOR*ALENS(WWVN,NEWOBJ))
     1            +(RCOR*ALENS(WWVN,NEWOBJ))
                  RCOR=0.0D0
                  OCOR=0.0D0
                  CENCEN=.FALSE.
                  CALL LOPD
                  LEN=LEN-(OCOR*ALENS(WWVN,NEWIMG-1))
     1            +(RCOR*ALENS(WWVN,NEWIMG-1))
              ELSE
C       MODE AFOCAL
                  RCOR=0.0D0
                  OCOR=0.0D0
                  CALL FOPD
                  LEN=LEN-(OCOR*ALENS(WWVN,NEWOBJ))
     1            +(RCOR*ALENS(WWVN,NEWOBJ))
                  RCOR=0.0D0
                  OCOR=0.0D0
                  CENCEN=.FALSE.
                  CALL LOPD
                  LEN=LEN-(OCOR*ALENS(WWVN,NEWIMG-1))
     1            +(RCOR*ALENS(WWVN,NEWIMG-1))
              END IF
              WRITE(OUTLYNE,100) LEN
              CALL SHOWIT(0)
 100          FORMAT('OPD FOR LAST RAY TRACED = ',G18.10,
     1        ' LENS UNITS')
              WRITE(OUTLYNE,101)
              CALL SHOWIT(0)
 101          FORMAT(1X)
              IF(INT(CURLAM).GE.1.AND.INT(CURLAM).LE.5) THEN
                  WW=SYSTEM1(INT(CURLAM))
              END IF
              IF(INT(CURLAM).GE.6.AND.INT(CURLAM).LE.10) THEN
                  WW=SYSTEM1(INT(CURLAM)+65)
              END IF
              IF(SYSTEM1(6).EQ.1.0) WAVE=(WW*1.0D-3)/(25.4D0)
              IF(SYSTEM1(6).EQ.2.0) WAVE=WW*1.0D-4
              IF(SYSTEM1(6).EQ.3.0) WAVE=WW*1.0D-3
              IF(SYSTEM1(6).EQ.4.0) WAVE=WW*1.0D-6
              LEN=-LEN
              IF(REVSTR) LEN=-LEN
              LENW=LEN/WAVE
              IF(INT(CURLAM).GE.1.AND.INT(CURLAM).LE.5) THEN
                  WRITE(OUTLYNE,102) LENW,SYSTEM1(INT(CURLAM))
                  CALL SHOWIT(0)
              END IF
              IF(INT(CURLAM).GE.6.AND.INT(CURLAM).LE.10) THEN
                  WRITE(OUTLYNE,102) LENW,SYSTEM1(INT(CURLAM)+65)
                  CALL SHOWIT(0)
              END IF
 102          FORMAT('OPD FOR LAST RAY TRACED = ',G18.10,
     1        ' IN WAVES AT ',G12.4,' MICRONS')
          ELSE
C       NO RAY EXITS OR NO REF RAY
              IF(.NOT.REFEXT) THEN
                  OUTLYNE=
     1            'NO REFERENCE RAY DATA EXISTS, "OPD" CAN NOT BE CALCULATED'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(.NOT.RAYEXT) THEN
                  OUTLYNE=
     1            'NO RAY DATA EXISTS, "OPD" CAN NOT BE CALCULATED'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
          RETURN
      END
C SUB PRGLBL.FOR
      SUBROUTINE PRGLBL
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE PRGLBL. THIS SUBROUTINE IMPLEMENTS
C       THE CMD COMMAND "PRGLOBAL"
C
          CHARACTER UNIT*11
C
          INTEGER SF,I,JK
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
          IF(SYSTEM1(6).EQ.1.0D0) UNIT='INCHES'
          IF(SYSTEM1(6).EQ.2.0D0) UNIT='CENTIMETERS'
          IF(SYSTEM1(6).EQ.3.0D0) UNIT='MILLIMETERS'
          IF(SYSTEM1(6).EQ.4.0D0) UNIT='METERS'
          IF(.NOT.GLOBE) THEN
C       NO VERTEX DATA TO OUTPUT
              WRITE(OUTLYNE,*)
     1        'NO CURRENT GLOBAL RAY DATA EXISTS TO OUTPUT'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(.NOT.RAYEXT) THEN
C       NO RAY DATA TO OUTPUT
              WRITE(OUTLYNE,*)
     1        'NO RAY DATA EXISTS TO OUTPUT'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
          IF(SST.EQ.1.OR.S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
              WRITE(OUTLYNE,*)WC(1:8),
     1        ' ONLY TAKES QUALIFIER OR NUMERIC WORD #1 INPUT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SQ.EQ.1.AND.S1.EQ.1) THEN
              WRITE(OUTLYNE,*)WC(1:8),
     1        ' TAKES EITHER QUALIFIER OR NUMERIC WORD #1 INPUT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'BUT NOT BOTH'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SYSTEM1(20).EQ.0.0D0) THEN
              WRITE(OUTLYNE,*)'LENS SYSTEM HAS NO SURFACES'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'NO GLOBAL RAY DATA CAN EXIST'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SQ.EQ.1.AND.WQ.EQ.'ALL') THEN
C       PRINT WARNING FOR OBJECT THICKNESS INFINITE
              IF(DABS(ALENS(3,NEWOBJ)).GT.1.0D10)THEN
                  WRITE(OUTLYNE,*)'WARNING:'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)
     1            'INFINITE OBJECT THICKNESS (MAGNITUDE GREATER THAN 1.0D+10'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)
     1            'OBJECT THICKNESS WAS IGNORED IN ALL GLOBAL CALCULATIONS'
                  CALL SHOWIT(1)
                  JK=NEWOBJ+1
              ELSE
                  JK=NEWOBJ
              END IF
              SF=NEWIMG
              WRITE(OUTLYNE,5001) INT(F12)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,5002) GLSURF
              CALL SHOWIT(0)
              WRITE(OUTLYNE,102) OFFX,UNIT
              CALL SHOWIT(0)
              WRITE(OUTLYNE,103) OFFY,UNIT
              CALL SHOWIT(0)
              WRITE(OUTLYNE,104) OFFZ,UNIT
              CALL SHOWIT(0)
              WRITE(OUTLYNE,105) OFFA
              CALL SHOWIT(0)
              WRITE(OUTLYNE,106) OFFB
              CALL SHOWIT(0)
              WRITE(OUTLYNE,107) OFFC
              CALL SHOWIT(0)
              WRITE(OUTLYNE,2501)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,5000)
              CALL SHOWIT(0)
              DO 10 I=JK,SF
                  WRITE(OUTLYNE,2000)I,GLRAY(1,I),GLRAY(2,I),GLRAY(3,I)
                  CALL SHOWIT(0)
 10           CONTINUE
              WRITE(OUTLYNE,2501)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,5003)
              CALL SHOWIT(0)
              DO 11 I=JK,SF
                  WRITE(OUTLYNE,2000)I,GLRAY(4,I),GLRAY(5,I),GLRAY(6,I)
                  CALL SHOWIT(0)
 11           CONTINUE
              WRITE(OUTLYNE,2501)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,5004)
              CALL SHOWIT(0)
              DO 12 I=JK,SF
                  WRITE(OUTLYNE,2000)I,GLRAY(7,I),GLRAY(8,I),GLRAY(9,I)
                  CALL SHOWIT(0)
 12           CONTINUE
              RETURN
          END IF
          IF(SQ.EQ.1.AND.WQ.EQ.'OBJ'.OR.SQ.EQ.1.AND.WQ.EQ.'OB') THEN
              SF=NEWOBJ
              IF(DABS(ALENS(3,SF)).GT.1.0D10) THEN
                  WRITE(OUTLYNE,*)
     1            'NO GLOBAL RAY DATA EXISTS FOR THE OBJECT SURFACE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)
     1            'WHICH HAS INFINTE THICKNESS (GREATER THAN 1.0D10)'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(HEADIN) THEN
                  WRITE(OUTLYNE,5001) INT(F12)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,5002) GLSURF
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,102) OFFX,UNIT
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,103) OFFY,UNIT
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,104) OFFZ,UNIT
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,105) OFFA
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,106) OFFB
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,107) OFFC
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,2501)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,5000)
                  CALL SHOWIT(0)
              END IF
              WRITE(OUTLYNE,1500)SF,GLRAY(1,SF),GLRAY(2,SF),GLRAY(3,SF)
              CALL SHOWIT(0)
C
              IF(HEADIN) THEN
                  WRITE(OUTLYNE,2501)
                  WRITE(OUTLYNE,5003)
                  CALL SHOWIT(0)

              END IF
              WRITE(OUTLYNE,1500)SF,GLRAY(4,SF),GLRAY(5,SF),GLRAY(6,SF)
              CALL SHOWIT(0)

              IF(HEADIN) THEN
                  WRITE(OUTLYNE,2501)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,5004)
                  CALL SHOWIT(0)
              END IF
              WRITE(OUTLYNE,1500)SF,GLRAY(7,SF),GLRAY(8,SF),GLRAY(9,SF)
              CALL SHOWIT(0)
              RETURN
          END IF
          IF(SQ.EQ.1.AND.WQ.NE.'OBJ'.OR.SQ.EQ.1.AND.
     1    WQ.NE.'ALL'.OR.SQ.EQ.1.AND.WQ.NE.'OB') THEN
              WRITE(OUTLYNE,*)'INVALID QUALIFIER WORD'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SQ.EQ.0.AND.DF1.EQ.1) THEN
C       OUTPUT IMAGE SURFACE
              SF=NEWIMG
              IF(HEADIN) THEN
                  WRITE(OUTLYNE,5001) INT(F12)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,5002) GLSURF
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,102) OFFX,UNIT
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,103) OFFY,UNIT
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,104) OFFZ,UNIT
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,105) OFFA
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,106) OFFB
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,107) OFFC
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,2501)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,5000)
                  CALL SHOWIT(0)
              END IF
              WRITE(OUTLYNE,1500)SF,GLRAY(1,SF),GLRAY(2,SF),GLRAY(3,SF)
              CALL SHOWIT(0)
              IF(HEADIN) THEN
                  WRITE(OUTLYNE,2501)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,5003)
                  CALL SHOWIT(0)
              END IF
              WRITE(OUTLYNE,1500)SF,GLRAY(4,SF),GLRAY(5,SF),GLRAY(6,SF)
              CALL SHOWIT(0)
              IF(HEADIN) THEN
                  WRITE(OUTLYNE,2501)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,5004)
                  CALL SHOWIT(0)
              END IF
              WRITE(OUTLYNE,1500)SF,GLRAY(7,SF),GLRAY(8,SF),GLRAY(9,SF)
              CALL SHOWIT(0)
              RETURN
          END IF
          IF(SQ.EQ.0.AND.DF1.NE.1) THEN
              I=INT(W1)
              SF=NEWIMG
              IF(DABS(ALENS(3,I)).GT.1.0D10) THEN
                  WRITE(OUTLYNE,*)
     1            'NO GLOBAL RAY DATA EXISTS FOR THE OBJECT SURFACE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)
     1            'WHICH HAS INFINTE THICKNESS (GREATER THAN 1.0D10)'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(I.GT.SF.OR.I.LT.NEWOBJ) THEN
                  WRITE(OUTLYNE,*)'SURFACE NUMBER BEYOND LEGAL RANGE'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(HEADIN) THEN
                  WRITE(OUTLYNE,5001) INT(F12)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,5002) GLSURF
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,102) OFFX,UNIT
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,103) OFFY,UNIT
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,104) OFFZ,UNIT
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,105) OFFA
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,106) OFFB
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,107) OFFC
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,2501)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,5000)
                  CALL SHOWIT(0)

              END IF
              WRITE(OUTLYNE,1500)I,GLRAY(1,I),GLRAY(2,I),GLRAY(3,I)
              CALL SHOWIT(0)
              IF(HEADIN) THEN
                  WRITE(OUTLYNE,2501)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,5003)
                  CALL SHOWIT(0)
              END IF
              WRITE(OUTLYNE,1500)I,GLRAY(4,I),GLRAY(5,I),GLRAY(6,I)
              CALL SHOWIT(0)
              IF(HEADIN) THEN
                  WRITE(OUTLYNE,2501)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,5004)
                  CALL SHOWIT(0)
              END IF
              WRITE(OUTLYNE,1500)I,GLRAY(7,I),GLRAY(8,I),GLRAY(9,I)
              CALL SHOWIT(0)
              RETURN
          END IF
 1500     FORMAT(I3,1X,G13.6,1X,G13.6,1X,G13.6)
 2000     FORMAT(I3,1X,G13.6,1X,G13.6,1X,G13.6)
 5000     FORMAT('SURF',7X,'X',13X,'Y',13X,'Z')
 5003     FORMAT('SURF',7X,'LOLD',10X,'MOLD',10X,'NOLD')
 5004     FORMAT('SURF',7X,'L',13X,'M',13X,'N')
 5001     FORMAT('CURRENT GLOBAL RAY DATA ','
     1  - (CFG #',I2,')')
 5002     FORMAT(
     1    'GLOBAL RAY DATA REFERENCED TO SURFACE NUMBER ',I3)
 2501     FORMAT(1X)
 102      FORMAT('WITH X-OFFSET   = ',G18.10,1X,A11)
 103      FORMAT('WITH Y-OFFSET   = ',G18.10,1X,A11)
 104      FORMAT('WITH Z-OFFSET   = ',G18.10,1X,A11)
 105      FORMAT('WITH ALPHA TILT = ',G18.10,' DEGREES')
 106      FORMAT('WITH BETA  TILT = ',G18.10,' DEGREES')
 107      FORMAT('WITH GAMMA TILT = ',G18.10,' DEGREES')
          RETURN
      END
C SUB PRNSS.FOR
      SUBROUTINE PRNSS
          USE GLOBALS
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE PRNSS. THIS SUBROUTINE IMPLEMENTS
C       THE PRNSS COMMAND WHICH OUTPUTS ANY EXISTING NSS DATA
C       IN THE MULTIRAY_DAT.
C
          INTEGER I
C
          LOGICAL DATAIS
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
C       THE VALID NUMERIC INPUT IS THE SURFACE NUMBER FOR
C       WHICH IT IS DESIRED TO PRODUCE OUTPUT. THE QUALIFIER "P"
C       CAUSES RELATIVE APERTURE COORDINATES TO BE PRINTED AS WELL.
C
C
          IF(SST.EQ.1.OR.SN.EQ.1.OR.SST.EQ.1) THEN
              WRITE(OUTLYNE,*)
     1        '"PRNSS" TAKES NO ADDITIONAL INPUT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'INPUT IGNORED'
              CALL SHOWIT(1)
          END IF
C
          IF(SYSTEM1(20).EQ.0.0) THEN
              WRITE(OUTLYNE,*)'LENS SYSTEM HAS NO SURFACES'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'NO RAY DATA CAN EXIST'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(.NOT.RAYEXT) THEN
              WRITE(OUTLYNE,*)'NO RAY DATA EXISTS TO DISPLAY'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          DATAIS=.FALSE.
          DO I=0,INT(SYSTEM1(20))
              IF(NUMHITS(I).GT.1) THEN
                  DATAIS=.TRUE.
                  EXIT
              END IF
          END DO
! 1500   FORMAT(I3,2X,G13.6,2X,G13.6,2X,G13.6,2X,
!     1  G13.6,2X,G13.6)
! 1600   FORMAT('RAY AT WAVL # ',I1,
!     1  ' (REL AP Y, REL AP X) = ( ',G13.6,', ',G13.6,' )')
! 2000   FORMAT(I3,2X,G13.6,2X,G13.6,2X,G13.6,2X,
!     1  G13.6,2X,G13.6)
! 5000   FORMAT('SURF',6X,'X ',13X,'Y  ',12X,'Z  ',
!     1  11X,'XANG',11X,'YANG')
! 6000   FORMAT('SURF',6X,'X ',13X,'Y  ',12X,'IANG',
!     1  10X,'XANG',11X,'YANG')
! 7000   FORMAT('SURF',6X,'X ',13X,'Y  ',12X,'IPANG',
!     1  9X,'XANG',11X,'YANG')
! 5001   FORMAT('RAYTRACE DATA (XZ AND YZ-PLANE)',
!     1  ' - (CFG #',I2,')')
! 5003   FORMAT('ANGULAR OUTPUT MODE IS ',A8)
! 5002   FORMAT(
          !    1'SLOPES/ANGLES  MEASURED WITH RESPECT TO THE Z-AXIS')
! 2501   FORMAT(1X)
      END
