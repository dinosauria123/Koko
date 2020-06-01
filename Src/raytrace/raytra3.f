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

C       THIS IS THE THIRD FILE OF RAYTRACING ROUTINES

C SUB NR4.FOR
      SUBROUTINE NR4
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE NR4.FOR. THIS SUBROUTINE IMPLEMENTS
C       INTERSECTION CALCULATION TO A TYPE 17 USER-DEFINED SURFACE
C
          INTEGER SPDCD1,SPDCD2
C
          COMMON/SPRA2/SPDCD1,SPDCD2
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
C
C     THIS SURFACE AUTOMATICALLY USES MACRO FUNCTION FUN10
C     FOR THE INITIAL USER FUCTION TO BE CALLED TO EVALUATE
C     THE Z VALUE CORRESPONDING TO THE Y AND X VALUES.
C     COEFFICIENTS C1 TO C96 ARE USED BY THE USER IN THE
C     MACRO FUNCTION FUN10 AND ANY MACRO FUNCTION CALLED BY FUN10.
C     THEY ARE STORED INITIALLY IN GENERAL PURPOSE STORAGE REGISTERS
C     NUMBERS 301 TO 348 AND ACCESSED BY THE USER VIA RCL COMMANDS
          GPREG(301)=FTFL01(1,R_I)
          GPREG(302)=FTFL01(2,R_I)
          GPREG(303)=FTFL01(3,R_I)
          GPREG(304)=FTFL01(4,R_I)
          GPREG(305)=FTFL01(5,R_I)
          GPREG(306)=FTFL01(6,R_I)
          GPREG(307)=FTFL01(7,R_I)
          GPREG(308)=FTFL01(8,R_I)
          GPREG(309)=FTFL01(9,R_I)
          GPREG(310)=FTFL01(10,R_I)
          GPREG(311)=FTFL01(11,R_I)
          GPREG(312)=FTFL01(12,R_I)
          GPREG(313)=FTFL01(13,R_I)
          GPREG(314)=FTFL01(14,R_I)
          GPREG(315)=FTFL01(15,R_I)
          GPREG(316)=FTFL01(16,R_I)
          GPREG(317)=FTFL01(17,R_I)
          GPREG(318)=FTFL01(18,R_I)
          GPREG(319)=FTFL01(19,R_I)
          GPREG(320)=FTFL01(20,R_I)
          GPREG(321)=FTFL01(21,R_I)
          GPREG(322)=FTFL01(22,R_I)
          GPREG(323)=FTFL01(23,R_I)
          GPREG(324)=FTFL01(24,R_I)
          GPREG(325)=FTFL01(25,R_I)
          GPREG(326)=FTFL01(26,R_I)
          GPREG(327)=FTFL01(27,R_I)
          GPREG(328)=FTFL01(28,R_I)
          GPREG(329)=FTFL01(29,R_I)
          GPREG(330)=FTFL01(30,R_I)
          GPREG(331)=FTFL01(31,R_I)
          GPREG(332)=FTFL01(32,R_I)
          GPREG(333)=FTFL01(33,R_I)
          GPREG(334)=FTFL01(34,R_I)
          GPREG(335)=FTFL01(35,R_I)
          GPREG(336)=FTFL01(36,R_I)
          GPREG(337)=FTFL01(37,R_I)
          GPREG(338)=FTFL01(38,R_I)
          GPREG(339)=FTFL01(39,R_I)
          GPREG(340)=FTFL01(40,R_I)
          GPREG(341)=FTFL01(41,R_I)
          GPREG(342)=FTFL01(42,R_I)
          GPREG(343)=FTFL01(43,R_I)
          GPREG(344)=FTFL01(44,R_I)
          GPREG(345)=FTFL01(45,R_I)
          GPREG(346)=FTFL01(46,R_I)
          GPREG(347)=FTFL01(47,R_I)
          GPREG(348)=FTFL01(48,R_I)
          GPREG(349)=FTFL01(49,R_I)
          GPREG(350)=FTFL01(50,R_I)
          GPREG(351)=FTFL01(51,R_I)
          GPREG(352)=FTFL01(52,R_I)
          GPREG(353)=FTFL01(53,R_I)
          GPREG(354)=FTFL01(54,R_I)
          GPREG(355)=FTFL01(55,R_I)
          GPREG(356)=FTFL01(56,R_I)
          GPREG(357)=FTFL01(57,R_I)
          GPREG(358)=FTFL01(58,R_I)
          GPREG(359)=FTFL01(59,R_I)
          GPREG(360)=FTFL01(60,R_I)
          GPREG(361)=FTFL01(61,R_I)
          GPREG(362)=FTFL01(62,R_I)
          GPREG(363)=FTFL01(63,R_I)
          GPREG(364)=FTFL01(64,R_I)
          GPREG(365)=FTFL01(65,R_I)
          GPREG(366)=FTFL01(66,R_I)
          GPREG(367)=FTFL01(67,R_I)
          GPREG(368)=FTFL01(68,R_I)
          GPREG(369)=FTFL01(69,R_I)
          GPREG(370)=FTFL01(70,R_I)
          GPREG(371)=FTFL01(71,R_I)
          GPREG(372)=FTFL01(72,R_I)
          GPREG(373)=FTFL01(73,R_I)
          GPREG(374)=FTFL01(74,R_I)
          GPREG(375)=FTFL01(75,R_I)
          GPREG(376)=FTFL01(76,R_I)
          GPREG(377)=FTFL01(77,R_I)
          GPREG(378)=FTFL01(78,R_I)
          GPREG(379)=FTFL01(79,R_I)
          GPREG(380)=FTFL01(80,R_I)
          GPREG(381)=FTFL01(81,R_I)
          GPREG(382)=FTFL01(82,R_I)
          GPREG(383)=FTFL01(83,R_I)
          GPREG(384)=FTFL01(84,R_I)
          GPREG(385)=FTFL01(85,R_I)
          GPREG(386)=FTFL01(86,R_I)
          GPREG(387)=FTFL01(87,R_I)
          GPREG(388)=FTFL01(88,R_I)
          GPREG(389)=FTFL01(89,R_I)
          GPREG(390)=FTFL01(90,R_I)
          GPREG(391)=FTFL01(91,R_I)
          GPREG(392)=FTFL01(92,R_I)
          GPREG(393)=FTFL01(93,R_I)
          GPREG(394)=FTFL01(94,R_I)
          GPREG(395)=FTFL01(95,R_I)
          GPREG(396)=FTFL01(96,R_I)
          IF(.NOT.FUNEXT(10)) THEN
C     NO FUN10 EXISTS, LEAVE REGISTERS ALONE
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
              FMNW(1:5)=0.0D0
              FMDF(1:5)=1
              NESFUN(0:10)=.FALSE.
              KLI=10
              FUN=.TRUE.
              F26=1
              CALL FUNEXC
              F26=0
              REST_KDP(1)=RESTINPT(1)
C     USE WHAT REMAINS IN THE REGISTERS (DONE IN HIT17.FOR)
          END IF
          RETURN
      END
C SUB NR1.FOR
      SUBROUTINE NR1(ERRR)
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE NR1.FOR. THIS SUBROUTINE IMPLEMENTS
C       INTERSECTION CALCULATION TO AN ASPHERIC PLANO SURFACE
C
          EXTERNAL FNZ1,FUNC1X,FUNC1Y,DER1X,DER1Y
C
          INTEGER I
C
          LOGICAL DIFCAL,ERRR
C
          REAL*8 C1,C2,C3,C4,MAG,ZCAL,ZZTOP,
     1    FNXP,FNYP,FNZP,HV1,HV2,HV,XP,YP,ZP,C5,ACLENG,DEL,A34,FNZ1
     2    ,FUNC1X,FUNC1Y,SNGX,SNGY,SNGZ,DER1X,DER1Y
     3    ,C41,C42,C43,C44,C45,XPASS,YPASS,ZPASS
          REAL*8 LENG
          COMMON/PASSLENG/LENG
          REAL*8 ZZEMAX,ZZEMIN
          COMMON/KENMOOR/ZZEMIN,ZZEMAX
C
          COMMON/ACLEN/ACLENG
C
          COMMON/SAGPAS/XPASS,YPASS,ZPASS
C
          INTEGER SPDCD1,SPDCD2
C
          COMMON/SPRA2/SPDCD1,SPDCD2
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datmac.inc'
          IF(LN.NE.0.0D0) SNGX=LN/DABS(LN)
          IF(MN.NE.0.0D0) SNGY=MN/DABS(MN)
          IF(NN.NE.0.0D0) SNGZ=NN/DABS(NN)
          IF(LN.EQ.0.0D0) SNGX=1.0D0
          IF(MN.EQ.0.0D0) SNGY=1.0D0
          IF(NN.EQ.0.0D0) SNGZ=1.0D0
          DIFCAL=.FALSE.
          IF(ALENS(8,R_I).NE.0.0D0) DIFCAL=.TRUE.
          IF(ALENS(34,R_I).GT.0.0D0.AND.
     1    DABS(ALENS(34,R_I)).NE.6.0D0.AND.
     2    DABS(ALENS(34,R_I)).NE.7.0D0.AND.
     3    DABS(ALENS(34,R_I)).NE.9.0D0.AND.
     4    DABS(ALENS(34,R_I)).NE.10.0D0.AND.
     4    DABS(ALENS(34,R_I)).NE.11.0D0.AND.
     5    DABS(ALENS(34,R_I)).NE.12.0D0.AND.
     6    DABS(ALENS(34,R_I)).NE.13.0D0.AND.
     6    DABS(ALENS(34,R_I)).NE.15.0D0.AND.
     6    DABS(ALENS(34,R_I)).NE.16.0D0.AND.
     6    DABS(ALENS(34,R_I)).NE.17.0D0.AND.
     6    DABS(ALENS(34,R_I)).NE.19.0D0.AND.
     6    DABS(ALENS(34,R_I)).NE.20.0D0.AND.
     6    DABS(ALENS(34,R_I)).NE.22.0D0.AND.
     6    DABS(ALENS(34,R_I)).NE.24.0D0
     7    .OR.ALENS(103,R_I).EQ.1.0D0) THEN
C     WE GOT A REAL SPECIAL SURFACE WHICH NEEDS FINITE DIFFERENCE
C     SURFACE SLOPE CALCS IF SURFACE IS TYPE 5 OR THE COEFS ARE NOT
C     ALL ZERO
              IF(ALENS(34,R_I).GT.0.0D0) THEN
                  IF(FTFL01(1,R_I).NE.0.0D0) DIFCAL=.TRUE.
                  IF(FTFL01(2,R_I).NE.0.0D0) DIFCAL=.TRUE.
                  IF(FTFL01(3,R_I).NE.0.0D0) DIFCAL=.TRUE.
                  IF(FTFL01(4,R_I).NE.0.0D0) DIFCAL=.TRUE.
                  IF(FTFL01(5,R_I).NE.0.0D0) DIFCAL=.TRUE.
                  IF(FTFL01(6,R_I).NE.0.0D0) DIFCAL=.TRUE.
                  IF(FTFL01(7,R_I).NE.0.0D0) DIFCAL=.TRUE.
                  IF(FTFL01(8,R_I).NE.0.0D0) DIFCAL=.TRUE.
                  IF(FTFL01(9,R_I).NE.0.0D0) DIFCAL=.TRUE.
                  IF(FTFL01(10,R_I).NE.0.0D0) DIFCAL=.TRUE.
                  IF(FTFL01(11,R_I).NE.0.0D0) DIFCAL=.TRUE.
                  IF(FTFL01(12,R_I).NE.0.0D0) DIFCAL=.TRUE.
                  IF(FTFL01(13,R_I).NE.0.0D0) DIFCAL=.TRUE.
                  IF(FTFL01(14,R_I).NE.0.0D0) DIFCAL=.TRUE.
                  IF(FTFL01(15,R_I).NE.0.0D0) DIFCAL=.TRUE.
                  IF(FTFL01(16,R_I).NE.0.0D0) DIFCAL=.TRUE.
                  IF(FTFL01(17,R_I).NE.0.0D0) DIFCAL=.TRUE.
                  IF(FTFL01(18,R_I).NE.0.0D0) DIFCAL=.TRUE.
                  IF(FTFL01(19,R_I).NE.0.0D0) DIFCAL=.TRUE.
                  IF(FTFL01(20,R_I).NE.0.0D0) DIFCAL=.TRUE.
                  IF(FTFL01(21,R_I).NE.0.0D0) DIFCAL=.TRUE.
                  IF(FTFL01(22,R_I).NE.0.0D0) DIFCAL=.TRUE.
                  IF(FTFL01(23,R_I).NE.0.0D0) DIFCAL=.TRUE.
                  IF(FTFL01(24,R_I).NE.0.0D0) DIFCAL=.TRUE.
                  IF(FTFL01(25,R_I).NE.0.0D0) DIFCAL=.TRUE.
                  IF(FTFL01(26,R_I).NE.0.0D0) DIFCAL=.TRUE.
                  IF(FTFL01(27,R_I).NE.0.0D0) DIFCAL=.TRUE.
                  IF(FTFL01(28,R_I).NE.0.0D0) DIFCAL=.TRUE.
                  IF(FTFL01(29,R_I).NE.0.0D0) DIFCAL=.TRUE.
                  IF(FTFL01(30,R_I).NE.0.0D0) DIFCAL=.TRUE.
                  IF(FTFL01(31,R_I).NE.0.0D0) DIFCAL=.TRUE.
                  IF(FTFL01(32,R_I).NE.0.0D0) DIFCAL=.TRUE.
                  IF(FTFL01(33,R_I).NE.0.0D0) DIFCAL=.TRUE.
                  IF(FTFL01(34,R_I).NE.0.0D0) DIFCAL=.TRUE.
                  IF(FTFL01(35,R_I).NE.0.0D0) DIFCAL=.TRUE.
                  IF(FTFL01(36,R_I).NE.0.0D0) DIFCAL=.TRUE.
                  IF(FTFL01(37,R_I).NE.0.0D0) DIFCAL=.TRUE.
                  IF(FTFL01(38,R_I).NE.0.0D0) DIFCAL=.TRUE.
                  IF(FTFL01(39,R_I).NE.0.0D0) DIFCAL=.TRUE.
                  IF(FTFL01(40,R_I).NE.0.0D0) DIFCAL=.TRUE.
                  IF(FTFL01(41,R_I).NE.0.0D0) DIFCAL=.TRUE.
                  IF(FTFL01(42,R_I).NE.0.0D0) DIFCAL=.TRUE.
                  IF(FTFL01(43,R_I).NE.0.0D0) DIFCAL=.TRUE.
                  IF(FTFL01(44,R_I).NE.0.0D0) DIFCAL=.TRUE.
                  IF(FTFL01(45,R_I).NE.0.0D0) DIFCAL=.TRUE.
                  IF(FTFL01(46,R_I).NE.0.0D0) DIFCAL=.TRUE.
                  IF(FTFL01(47,R_I).NE.0.0D0) DIFCAL=.TRUE.
                  IF(FTFL01(48,R_I).NE.0.0D0) DIFCAL=.TRUE.
                  IF(FTFL01(49,R_I).NE.0.0D0) DIFCAL=.TRUE.
                  IF(FTFL01(50,R_I).NE.0.0D0) DIFCAL=.TRUE.
                  IF(FTFL01(51,R_I).NE.0.0D0) DIFCAL=.TRUE.
                  IF(FTFL01(52,R_I).NE.0.0D0) DIFCAL=.TRUE.
                  IF(FTFL01(53,R_I).NE.0.0D0) DIFCAL=.TRUE.
                  IF(FTFL01(54,R_I).NE.0.0D0) DIFCAL=.TRUE.
                  IF(FTFL01(55,R_I).NE.0.0D0) DIFCAL=.TRUE.
                  IF(FTFL01(56,R_I).NE.0.0D0) DIFCAL=.TRUE.
                  IF(FTFL01(57,R_I).NE.0.0D0) DIFCAL=.TRUE.
                  IF(FTFL01(58,R_I).NE.0.0D0) DIFCAL=.TRUE.
                  IF(FTFL01(59,R_I).NE.0.0D0) DIFCAL=.TRUE.
                  IF(FTFL01(60,R_I).NE.0.0D0) DIFCAL=.TRUE.
                  IF(FTFL01(61,R_I).NE.0.0D0) DIFCAL=.TRUE.
                  IF(FTFL01(62,R_I).NE.0.0D0) DIFCAL=.TRUE.
                  IF(FTFL01(63,R_I).NE.0.0D0) DIFCAL=.TRUE.
                  IF(FTFL01(64,R_I).NE.0.0D0) DIFCAL=.TRUE.
                  IF(FTFL01(65,R_I).NE.0.0D0) DIFCAL=.TRUE.
                  IF(FTFL01(66,R_I).NE.0.0D0) DIFCAL=.TRUE.
                  IF(FTFL01(67,R_I).NE.0.0D0) DIFCAL=.TRUE.
                  IF(FTFL01(68,R_I).NE.0.0D0) DIFCAL=.TRUE.
                  IF(FTFL01(69,R_I).NE.0.0D0) DIFCAL=.TRUE.
                  IF(FTFL01(70,R_I).NE.0.0D0) DIFCAL=.TRUE.
                  IF(FTFL01(71,R_I).NE.0.0D0) DIFCAL=.TRUE.
                  IF(FTFL01(72,R_I).NE.0.0D0) DIFCAL=.TRUE.
                  IF(FTFL01(73,R_I).NE.0.0D0) DIFCAL=.TRUE.
                  IF(FTFL01(74,R_I).NE.0.0D0) DIFCAL=.TRUE.
                  IF(FTFL01(75,R_I).NE.0.0D0) DIFCAL=.TRUE.
                  IF(FTFL01(76,R_I).NE.0.0D0) DIFCAL=.TRUE.
                  IF(FTFL01(77,R_I).NE.0.0D0) DIFCAL=.TRUE.
                  IF(FTFL01(78,R_I).NE.0.0D0) DIFCAL=.TRUE.
                  IF(FTFL01(79,R_I).NE.0.0D0) DIFCAL=.TRUE.
                  IF(FTFL01(80,R_I).NE.0.0D0) DIFCAL=.TRUE.
                  IF(FTFL01(81,R_I).NE.0.0D0) DIFCAL=.TRUE.
                  IF(FTFL01(82,R_I).NE.0.0D0) DIFCAL=.TRUE.
                  IF(FTFL01(83,R_I).NE.0.0D0) DIFCAL=.TRUE.
                  IF(FTFL01(84,R_I).NE.0.0D0) DIFCAL=.TRUE.
                  IF(FTFL01(85,R_I).NE.0.0D0) DIFCAL=.TRUE.
                  IF(FTFL01(86,R_I).NE.0.0D0) DIFCAL=.TRUE.
                  IF(FTFL01(87,R_I).NE.0.0D0) DIFCAL=.TRUE.
                  IF(FTFL01(88,R_I).NE.0.0D0) DIFCAL=.TRUE.
                  IF(FTFL01(89,R_I).NE.0.0D0) DIFCAL=.TRUE.
                  IF(FTFL01(90,R_I).NE.0.0D0) DIFCAL=.TRUE.
                  IF(FTFL01(91,R_I).NE.0.0D0) DIFCAL=.TRUE.
                  IF(FTFL01(92,R_I).NE.0.0D0) DIFCAL=.TRUE.
                  IF(FTFL01(93,R_I).NE.0.0D0) DIFCAL=.TRUE.
                  IF(FTFL01(94,R_I).NE.0.0D0) DIFCAL=.TRUE.
                  IF(FTFL01(95,R_I).NE.0.0D0) DIFCAL=.TRUE.
                  IF(FTFL01(96,R_I).NE.0.0D0) DIFCAL=.TRUE.
                  IF(FTFL01(96,R_I).NE.0.0D0) DIFCAL=.TRUE.
              END IF
          END IF

C
          C1=ALENS(4,R_I)
          C2=ALENS(5,R_I)
          C3=ALENS(6,R_I)
          C4=ALENS(7,R_I)
          C41=ALENS(81,R_I)
          C42=ALENS(82,R_I)
          C43=ALENS(83,R_I)
          C44=ALENS(84,R_I)
          C45=ALENS(85,R_I)
          C5=ALENS(43,R_I)
          A34=ALENS(34,R_I)
C
          DEL=DELSUR
C
C       THE RETURNED VALUES OF X,Y,Z ARE THE CORRECT INTERSECTIONS
C       TO WITHIN THE SURTOL VALUE
C
C       CALCULATE INTERSECTION
          DO 10 I=1,NRAITR
C
C     X AND Y DERIVATIVES OF THE SURFACE ARE CALCULATED HERE:
C     FUNCTION FNZ1 EVALUATES THE FUNCTION
C
              IF(DIFCAL) THEN
                  ERRR=.FALSE.
                  FNXP=-DER1X(FUNC1X,R_X,DELSUR,ERRR)
                  FNYP=-DER1Y(FUNC1Y,R_Y,DELSUR,ERRR)
                  IF(ERRR) RETURN
              ELSE
                  FNXP=-(
     1            ((1.0D0*2.0D0*R_X)*C5))
                  FNYP=-(
     1            ((1.0D0*2.0D0*R_Y)*C5))
              END IF
              FNZP=SNGZ
C
C     X AND Y DERIVATIVES OF THE SURFACE HAVE BEEN CALCULATED
C
              MAG=DSQRT((FNXP**2)+(FNYP**2)+(FNZP**2))
              LN=FNXP/MAG
              MN=FNYP/MAG
              NN=FNZP/MAG
C       THE INTERSECTION OF THE RAY WITH THIS TANGENT
C       PLANE
C       USING:
C
              ZCAL=FNZ1(R_X,R_Y,C1,C2,C3,C4,C5,R_I,A34,C41,C42,C43,C44,C45)
              HV1=(ZCAL-R_Z)
              HV2=((R_L*LN)+(R_M*MN)+(R_N*NN))
              HV=HV1/HV2
C       NEW X,Y AND Z ARE
              XP=R_X+(HV*R_L)
              YP=R_Y+(HV*R_M)
              ZP=R_Z+(HV*R_N)
C       SHOULD WE CONTINUE?
              ZZTOP=FNZ1(XP,YP,C1,C2,C3,C4,C5,R_I,A34,C41,C42,C43,C44,C45)
              LENG=DABS(ZZTOP-ZP)
              ACLENG=LENG
              IF(LENG.LE.SURTOL) THEN
                  R_X=XP
                  R_Y=YP
                  R_Z=ZP
C
C     X AND Y DERIVATIVES OF THE SURFACE ARE CALCULATED HERE:
C     FUNCTION FNZ1 EVALUATES THE FUNCTION
C
                  IF(DIFCAL) THEN
                      ERRR=.FALSE.
                      FNXP=-DER1X(FUNC1X,R_X,DELSUR,ERRR)
                      FNYP=-DER1Y(FUNC1Y,R_Y,DELSUR,ERRR)
                      IF(ERRR) RETURN
                  ELSE
                      FNXP=-(
     1                ((1.0D0*2.0D0*R_X)*C5))
                      FNYP=-(
     1                ((1.0D0*2.0D0*R_Y)*C5))
                  END IF
                  FNZP=SNGZ
C
C     X AND Y DERIVATIVES OF THE SURFACE HAVE BEEN CALCULATED
C
                  MAG=DSQRT((FNXP**2)+(FNYP**2)+(FNZP**2))
                  LN=FNXP/MAG
                  MN=FNYP/MAG
                  NN=FNZP/MAG
                  STOPP=0
                  RETURN
              ELSE
C       KEEP TRYING
C       MAKE X Y Z FROM XP YP ZP AND TRY AGAIN
                  R_X=XP
                  R_Y=YP
                  R_Z=ZP
              END IF
 10       CONTINUE
          IF(MSG) THEN
              WRITE(OUTLYNE,*)'RAY FAILURE OCCURRED AT SURFACE ',R_I
              CALL SHOWIT(1)
              OUTLYNE=
     1        'CONVERGENCE FAILURE OCCURRED DURING RAY INTERSECTION'
              CALL SHOWIT(1)
              OUTLYNE='WITH ASPHERIC, TORIC OR SPECIAL SURFACE'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'ACCURACY OF INTERSECTION WAS = ',LENG
              CALL SHOWIT(1)
          END IF
          RAYCOD(1)=2
          RAYCOD(2)=R_I
          SPDCD1=RAYCOD(1)
          SPDCD2=R_I
          STOPP=1
          RETURN
      END
      FUNCTION FNZ1(AX,AY,AC1,AC2,AC3,AC4,AC5,AR,A34,AC41,AC42,AC43,
     1AC44,AC45)
          USE GLOBALS
C
          IMPLICIT NONE
C
          EXTERNAL FF2,FF3,FF4,FF5
C
          INTEGER IPASS1
C
          REAL*8 FNZ1,AX,AY,AC1,AC2,AC3,AC4,AC5,AAC1,AAC2,AAC3
     1    ,AAC4,AAC5,AAX,AAY,FNZZ1,A34,R,THETA,FF2,FF3,FF4,XX,YY,FF5,AZ
     1    ,AAAX,AAAY,AAC41,AAC42,AAC43,AAC44,AAC45,AC41,AC42,AC43,AC44,AC45
C
          REAL*8 XPASS,YPASS,ZPASS,DA34
          REAL*8 ZZEMAX,ZZEMIN
          REAL*8 JK_WAVE
          REAL*8 AMP1,OMEGA1X,OMEGA1Y
          REAL*8 AMP2,OMEGA2X,OMEGA2Y
          REAL*8 AMP3,OMEGA3X,OMEGA3Y
          REAL*8 AMP4,OMEGA4X,OMEGA4Y
          REAL*8 AMP5,OMEGA5X,OMEGA5Y
          COMMON/KENMOOR/ZZEMIN,ZZEMAX
C
          COMMON/SAGPAS/XPASS,YPASS,ZPASS
C
          INTEGER KLI,AR,ISURF
C
          LOGICAL FUN,GERROR,GERROR1,GERROR2,UERROR
C
          COMMON/NEFER/KLI
C
          COMMON/COMFUN/FUN
C
          INTEGER SPDCD1,SPDCD2
C
          COMMON/SPRA2/SPDCD1,SPDCD2
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datmac.inc'
C
C     AR IS THE SURFACE NUMBER
C
          INTEGER I
C
          FNZZ1(AAX,AAY,AAC1,AAC2,AAC3,AAC4,AAC5
     1    ,AAC41,AAC42,AAC43,AAC44,AAC45)=
     1    (((AAX**2)+(AAY**2))*AAC5)+
     1    ((((AAX**2)+(AAY**2))**2)*AAC1)+
     1    ((((AAX**2)+(AAY**2))**3)*AAC2)+
     1    ((((AAX**2)+(AAY**2))**4)*AAC3)+
     1    ((((AAX**2)+(AAY**2))**5)*AAC4)+
     1    ((((AAX**2)+(AAY**2))**6)*AAC41)+
     1    ((((AAX**2)+(AAY**2))**7)*AAC42)+
     1    ((((AAX**2)+(AAY**2))**8)*AAC43)+
     1    ((((AAX**2)+(AAY**2))**9)*AAC44)+
     1    ((((AAX**2)+(AAY**2))**10)*AAC45)
C
C
          DA34=DABS(A34)
          IF(A34.EQ.0.0D0.AND.ALENS(103,AR).EQ.0.0D0.OR.DA34.EQ.6.0D0.OR.
     1    DA34.EQ.7.0D0.OR.DA34.EQ.9.0D0.OR.DA34.EQ.10.0D0.OR.
     1    DA34.EQ.11.0D0.OR.DA34.EQ.12.0D0.OR.
     1    DA34.EQ.15.0D0.OR.DA34.EQ.16.0D0.OR.DA34.EQ.17.0D0.OR.
     1    DA34.EQ.19.0D0.OR.DA34.EQ.20.0D0) THEN
              FNZ1=FNZZ1(AX,AY,AC1,AC2,AC3,AC4,AC5,AC41,AC42,AC43,AC44,AC45)
              RETURN
          END IF
          FNZ1=0.0D0
C     SPECIAL SURFACE TYPE 1
          IF(A34.EQ.1.0D0) THEN
              DO I=9,48
                  IF(FTFL01(I,AR).NE.0.0D0) FNZ1=FNZ1+
     1            (FTFL01(I,AR)*(((DSQRT((AX**2)+(AY**2)))**(I-9))))
              END DO
C     NOW ADD THE REGULAR TERM
              FNZ1=FNZ1+FNZZ1(AX,AY,AC1,AC2,AC3,AC4,AC5,AC41,AC42,AC43,AC44
     1        ,AC45)
              RETURN
          END IF
C     SPECIAL SURFACE TYPE 4
          IF(A34.EQ.4.0D0) THEN
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
              AMP1=DABS(FTFL01(1,AR)*JK_WAVE*0.5D0)
              AMP2=DABS(FTFL01(4,AR)*JK_WAVE*0.5D0)
              AMP3=DABS(FTFL01(7,AR)*JK_WAVE*0.5D0)
              AMP4=DABS(FTFL01(10,AR)*JK_WAVE*0.5D0)
              AMP5=DABS(FTFL01(13,AR)*JK_WAVE*0.5D0)
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
              AMP1=FTFL01(1,AR)/2.0D0
              AMP2=FTFL01(4,AR)/2.0D0
              AMP3=FTFL01(7,AR)/2.0D0
              AMP4=FTFL01(10,AR)/2.0D0
              AMP5=FTFL01(13,AR)/2.0D0
              IF(FTFL01(2,AR).EQ.0.0D0) THEN
                  OMEGA1X=0.0D0
              ELSE
                  OMEGA1X=TWOPII/DABS(FTFL01(2,AR))
              END IF
              IF(FTFL01(3,AR).EQ.0.0D0) THEN
                  OMEGA1Y=0.0D0
              ELSE
                  OMEGA1Y=TWOPII/DABS(FTFL01(3,AR))
              END IF
              IF(FTFL01(5,AR).EQ.0.0D0) THEN
                  OMEGA2X=0.0D0
              ELSE
                  OMEGA2X=TWOPII/DABS(FTFL01(5,AR))
              END IF
              IF(FTFL01(6,AR).EQ.0.0D0) THEN
                  OMEGA2Y=0.0D0
              ELSE
                  OMEGA2Y=TWOPII/DABS(FTFL01(6,AR))
              END IF
              IF(FTFL01(8,AR).EQ.0.0D0) THEN
                  OMEGA3X=0.0D0
              ELSE
                  OMEGA3X=TWOPII/DABS(FTFL01(8,AR))
              END IF
              IF(FTFL01(9,AR).EQ.0.0D0) THEN
                  OMEGA3Y=0.0D0
              ELSE
                  OMEGA3Y=TWOPII/DABS(FTFL01(9,AR))
              END IF
              IF(FTFL01(11,AR).EQ.0.0D0) THEN
                  OMEGA4X=0.0D0
              ELSE
                  OMEGA4X=TWOPII/DABS(FTFL01(11,AR))
              END IF
              IF(FTFL01(12,AR).EQ.0.0D0) THEN
                  OMEGA4Y=0.0D0
              ELSE
                  OMEGA4Y=TWOPII/DABS(FTFL01(12,AR))
              END IF
              IF(FTFL01(14,AR).EQ.0.0D0) THEN
                  OMEGA5X=0.0D0
              ELSE
                  OMEGA5X=TWOPII/DABS(FTFL01(14,AR))
              END IF
              IF(FTFL01(15,AR).EQ.0.0D0) THEN
                  OMEGA5Y=0.0D0
              ELSE
                  OMEGA5Y=TWOPII/DABS(FTFL01(15,AR))
              END IF
              FNZ1=(AMP1*(DCOS(OMEGA1X*AX)*(DCOS(OMEGA1Y*AY))))
     1        +(AMP2*(DCOS(OMEGA2X*AX)*(DCOS(OMEGA2Y*AY))))
     1        +(AMP3*(DCOS(OMEGA3X*AX)*(DCOS(OMEGA3Y*AY))))
     1        +(AMP4*(DCOS(OMEGA4X*AX)*(DCOS(OMEGA4Y*AY))))
     1        +(AMP5*(DCOS(OMEGA5X*AX)*(DCOS(OMEGA5Y*AY))))
C     NOW ADD THE REGULAR TERM
              FNZ1=FNZ1+FNZZ1(AX,AY,AC1,AC2,AC3,AC4,AC5,AC41,AC42,AC43,AC44
     1        ,AC45)
              RETURN
          END IF
C     SPECIAL SURFACE TYPE 2
          IF(A34.EQ.2.0D0) THEN
              INR=ALENS(76,AR)
              AAAX=AX/INR
              AAAY=AY/INR
              R=DSQRT((AAAX**2)+(AAAY**2))
              IF(DABS(AAAY).GE.DABS(((1.0D35)*AAAX))) THEN
                  IF(AAAY.GE.0.0D0) THETA=PII/2.0D0
                  IF(AAAY.LT.0.0D0) THETA=(3.0D0*PII)/2.0D0
              ELSE
                  IF(DABS(AAAY).EQ.0.0D0.AND.DABS(AAAX).EQ.0.0D0) THEN
                      THETA=0.0D0
                  ELSE
                      THETA=DATAN2(AAAY,AAAX)
                  END IF
                  IF(THETA.LT.0.0D0) THETA=THETA+(TWOPII)
              END IF
              FNZ1=0.0D0
              DO I=1,66
                  IF(FTFL01(I,AR).NE.0.0D0) FNZ1=FNZ1+
     1            (FTFL01(I,AR)*(FF2(R,THETA,I)))
              END DO
C     NOW ADD THE REGULAR TERM
              FNZ1=FNZ1+FNZZ1(AX,AY,AC1,AC2,AC3,AC4,AC5
     1        ,AC41,AC42,AC43,AC44,AC45)
              RETURN
          END IF
C     SPECIAL SURFACE TYPE 14
          IF(A34.EQ.14.0D0) THEN
              INR=ALENS(76,AR)
              AAAX=AX/INR
              AAAY=AY/INR
              R=DSQRT((AAAX**2)+(AAAY**2))
              IF(DABS(AAAY).GE.DABS(((1.0D35)*AAAX))) THEN
                  IF(AAAY.GE.0.0D0) THETA=PII/2.0D0
                  IF(AAAY.LT.0.0D0) THETA=(3.0D0*PII)/2.0D0
              ELSE
                  IF(DABS(AAAY).EQ.0.0D0.AND.DABS(AAAX).EQ.0.0D0) THEN
                      THETA=0.0D0
                  ELSE
                      THETA=DATAN2(AAAY,AAAX)
                  END IF
                  IF(THETA.LT.0.0D0) THETA=THETA+(TWOPII)
              END IF
              FNZ1=0.0D0
              DO I=1,48
                  IF(FTFL01(I,AR).NE.0.0D0) FNZ1=FNZ1+
     1            (FTFL01(I,AR)*(FF5(R,THETA,I)))
              END DO
C     NOW ADD THE REGULAR TERM
              FNZ1=FNZ1+FNZZ1(AX,AY,AC1,AC2,AC3,AC4,AC5
     1        ,AC41,AC42,AC43,AC44,AC45)
              RETURN
          END IF
C     SPECIAL SURFACE TYPE 3
          IF(A34.EQ.3.0D0) THEN
              INR=ALENS(76,AR)
              AAAY=AY/INR
              AAAX=AX/INR
              R=DSQRT((AAAX**2)+(AAAY**2))
              IF(DABS(AAAY).GE.DABS(((1.0D35)*AAAX))) THEN
                  IF(AAAY.GE.0.0D0) THETA=PII/2.0D0
                  IF(AAAY.LT.0.0D0) THETA=(3.0D0*PII)/2.0D0
              ELSE
                  IF(DABS(AAAY).EQ.0.0D0.AND.DABS(AAAX).EQ.0.0D0) THEN
                      THETA=0.0D0
                  ELSE
                      THETA=DATAN2(AAAY,AAAX)
                  END IF
                  IF(THETA.LT.0.0D0) THETA=THETA+(TWOPII)
              END IF
              FNZ1=0.0D0
              DO I=1,37
                  IF(FTFL01(I,AR).NE.0.0D0) FNZ1=FNZ1+
     1            (FTFL01(I,AR)*(FF3(R,THETA,I)))
              END DO
C     NOW ADD THE REGULAR TERM
              FNZ1=FNZ1+FNZZ1(AX,AY,AC1,AC2,AC3,AC4,AC5
     1        ,AC41,AC42,AC43,AC44,AC45)
              RETURN
          END IF
C     IF SPECIAL SURFACE TYPE 23
          IF(A34.EQ.23.0D0) THEN
C
              FNZ1=0.0D0
              IF(INT(FTFL01(1,AR)).LT.3.0D0) RETURN
              AZ=0.0D0
              IPASS1=3
              CALL SPL23(AR,AX,AY,AZ,IPASS1)
              FNZ1=AZ
C     NOW ADD THE REGULAR TERM
              FNZ1=FNZ1+FNZZ1(AX,AY,AC1,AC2,AC3,AC4,AC5
     1        ,AC41,AC42,AC43,AC44,AC45)
          END IF
C     SPECIAL SURFACE TYPE 8
          IF(A34.EQ.8.0D0) THEN
              XX=AX
              YY=AY
              FNZ1=0.0D0
              DO I=1,91
                  IF(FTFL01(I,AR).NE.0.0D0) FNZ1=FNZ1+
     1            (FTFL01(I,AR)*(FF4(XX,YY,I)))
              END DO
C     NOW ADD THE REGULAR TERM
              FNZ1=FNZ1+FNZZ1(AX,AY,AC1,AC2,AC3,AC4,AC5
     1        ,AC41,AC42,AC43,AC44,AC45)
              RETURN
          END IF
C     lSPECIAL SURFACE TYPE 5
C     USER DEFINED SURFACE
          IF(A34.EQ.5.0D0) THEN
              REG(40)=REG(9)
              REG(9)=AX
              REG(10)=AY
              REG(12)=DBLE(I)
C     THIS SURFACE AUTOMATICALLY USES MACRO FUNCTION FUN10
C     FOR THE INITIAL USER FUCTION TO BE CALLED TO EVALUATE
C     THE Z VALUE CORRESPONDING TO THE AY AND AX VALUES.
C     COEFFICIENTS C1 TO C96 ARE USED BY THE USER IN THE
C     MACRO FUNCTION FUN10 AND ANY MACRO FUNCTION CALLED BY FUN10.
C     THEY ARE STORED INITIALLY IN GENERAL PURPOSE STORAGE REGISTERS
C     NUMBERS 301 TO 348 AND ACCESSED BY THE USER VIA RCL COMMANDS
              GPREG(301)=FTFL01(1,AR)
              GPREG(302)=FTFL01(2,AR)
              GPREG(303)=FTFL01(3,AR)
              GPREG(304)=FTFL01(4,AR)
              GPREG(305)=FTFL01(5,AR)
              GPREG(306)=FTFL01(6,AR)
              GPREG(307)=FTFL01(7,AR)
              GPREG(308)=FTFL01(8,AR)
              GPREG(309)=FTFL01(9,AR)
              GPREG(310)=FTFL01(10,AR)
              GPREG(311)=FTFL01(11,AR)
              GPREG(312)=FTFL01(12,AR)
              GPREG(313)=FTFL01(13,AR)
              GPREG(314)=FTFL01(14,AR)
              GPREG(315)=FTFL01(15,AR)
              GPREG(316)=FTFL01(16,AR)
              GPREG(317)=FTFL01(17,AR)
              GPREG(318)=FTFL01(18,AR)
              GPREG(319)=FTFL01(19,AR)
              GPREG(320)=FTFL01(20,AR)
              GPREG(321)=FTFL01(21,AR)
              GPREG(322)=FTFL01(22,AR)
              GPREG(323)=FTFL01(23,AR)
              GPREG(324)=FTFL01(24,AR)
              GPREG(325)=FTFL01(25,AR)
              GPREG(326)=FTFL01(26,AR)
              GPREG(327)=FTFL01(27,AR)
              GPREG(328)=FTFL01(28,AR)
              GPREG(329)=FTFL01(29,AR)
              GPREG(330)=FTFL01(30,AR)
              GPREG(331)=FTFL01(31,AR)
              GPREG(332)=FTFL01(32,AR)
              GPREG(333)=FTFL01(33,AR)
              GPREG(334)=FTFL01(34,AR)
              GPREG(335)=FTFL01(35,AR)
              GPREG(336)=FTFL01(36,AR)
              GPREG(337)=FTFL01(37,AR)
              GPREG(338)=FTFL01(38,AR)
              GPREG(339)=FTFL01(39,AR)
              GPREG(340)=FTFL01(40,AR)
              GPREG(341)=FTFL01(41,AR)
              GPREG(342)=FTFL01(42,AR)
              GPREG(343)=FTFL01(43,AR)
              GPREG(344)=FTFL01(44,AR)
              GPREG(345)=FTFL01(45,AR)
              GPREG(346)=FTFL01(46,AR)
              GPREG(347)=FTFL01(47,AR)
              GPREG(348)=FTFL01(48,AR)
              GPREG(349)=FTFL01(49,AR)
              GPREG(350)=FTFL01(50,AR)
              GPREG(351)=FTFL01(51,AR)
              GPREG(352)=FTFL01(52,AR)
              GPREG(353)=FTFL01(53,AR)
              GPREG(354)=FTFL01(54,AR)
              GPREG(355)=FTFL01(55,AR)
              GPREG(356)=FTFL01(56,AR)
              GPREG(357)=FTFL01(57,AR)
              GPREG(358)=FTFL01(58,AR)
              GPREG(359)=FTFL01(59,AR)
              GPREG(360)=FTFL01(60,AR)
              GPREG(361)=FTFL01(61,AR)
              GPREG(362)=FTFL01(62,AR)
              GPREG(363)=FTFL01(63,AR)
              GPREG(364)=FTFL01(64,AR)
              GPREG(365)=FTFL01(65,AR)
              GPREG(366)=FTFL01(66,AR)
              GPREG(367)=FTFL01(67,AR)
              GPREG(368)=FTFL01(68,AR)
              GPREG(369)=FTFL01(69,AR)
              GPREG(370)=FTFL01(70,AR)
              GPREG(371)=FTFL01(71,AR)
              GPREG(372)=FTFL01(72,AR)
              GPREG(373)=FTFL01(73,AR)
              GPREG(374)=FTFL01(74,AR)
              GPREG(375)=FTFL01(75,AR)
              GPREG(376)=FTFL01(76,AR)
              GPREG(377)=FTFL01(77,AR)
              GPREG(378)=FTFL01(78,AR)
              GPREG(379)=FTFL01(79,AR)
              GPREG(380)=FTFL01(80,AR)
              GPREG(381)=FTFL01(81,AR)
              GPREG(382)=FTFL01(82,AR)
              GPREG(383)=FTFL01(83,AR)
              GPREG(384)=FTFL01(84,AR)
              GPREG(385)=FTFL01(85,AR)
              GPREG(386)=FTFL01(86,AR)
              GPREG(387)=FTFL01(87,AR)
              GPREG(388)=FTFL01(88,AR)
              GPREG(389)=FTFL01(89,AR)
              GPREG(390)=FTFL01(90,AR)
              GPREG(391)=FTFL01(91,AR)
              GPREG(392)=FTFL01(92,AR)
              GPREG(393)=FTFL01(93,AR)
              GPREG(394)=FTFL01(94,AR)
              GPREG(395)=FTFL01(95,AR)
              GPREG(396)=FTFL01(96,AR)
              IF(.NOT.FUNEXT(10)) THEN
C     NO FUN10 EXISTS, RETURN 0.0D0 AS THE Z CONTRIBUTION
                  FNZ1=0.0D0
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
                  FMNW(1:5)=0.0D0
                  FMDF(1:5)=1
                  NESFUN(0:10)=.FALSE.
                  KLI=10
                  FUN=.TRUE.
                  F26=1
                  CALL FUNEXC
                  F26=0
                  REST_KDP(1)=RESTINPT(1)
C     USE WHAT REMAINS IN THE Z-REGISTER
                  FNZ1=REG(11)
              END IF
C     NOW ADD THE REGULAR TERM
              FNZ1=FNZ1+FNZZ1(AX,AY,AC1,AC2,AC3,AC4,AC5
     1        ,AC41,AC42,AC43,AC44,AC45)
              RETURN
          END IF
          IF(A34.EQ.21.0D0) THEN
              CALL USERSURF(AR,AX,AY,FNZ1,UERROR)
              IF(UERROR) THEN
                  UERROR=.FALSE.
                  RETURN
              END IF
C     NOW ADD THE REGULAR TERM
              FNZ1=FNZ1+FNZZ1(AX,AY,AC1,AC2,AC3,AC4,AC5
     1        ,AC41,AC42,AC43,AC44,AC45)
              RETURN
          END IF
C     SPECIAL SURFACE TYPE 22
C     GRID SAG
          IF(A34.EQ.22.0D0) THEN
C     USE WHAT REMAINS IN THE Z-REGISTER
C     CALL GRID
C     TYPE 22 SAG GRID
              ISURF=AR
              XPASS=AX
              YPASS=AY
              ZPASS=0.0D0
              CALL GRIDS(4,ISURF,GERROR)
              IF(.NOT.GERROR) GRIDSUNLOADED22(ISURF)=.FALSE.
              IF(GERROR) THEN
                  IF(MSG) THEN
                      WRITE(OUTLYNE,*)'RAY FAILURE OCCURRED AT SURFACE ',ISURF
                      CALL SHOWIT(1)
                      OUTLYNE=
     1                'GRID FILE DOES NOT EXIST FOR THIS SURFACE'
                      CALL SHOWIT(1)
                      OUTLYNE=
     1                'OR IT HAS INSUFFICIENT DATA FOR THIS SURFACE'
                      CALL SHOWIT(1)
                  END IF
                  STOPP=1
                  RAYCOD(1)=15
                  RAYCOD(2)=ISURF
                  RAYEXT=.FALSE.
                  POLEXT=.FALSE.
                  SPDCD1=RAYCOD(1)
                  SPDCD2=ISURF
                  RETURN
              END IF
C     NOW ADD THE REGULAR TERM
C      IF(ZPASS.GE.ZZEMAX) ZZEMAX=ZPASS
C      IF(ZPASS.LE.ZZEMIN) ZZEMIN=ZPASS
              FNZ1=ZPASS+FNZZ1(AX,AY,AC1,AC2,AC3,AC4,AC5
     1        ,AC41,AC42,AC43,AC44,AC45)
              RETURN
          END IF
C     DEFORM SURFACE
          IF(ALENS(103,AR).EQ.1.0D0) THEN
C     USE WHAT REMAINS IN THE Z-REGISTER
C     CALL DEFGRID
              ISURF=AR
              XPASS=AX
              YPASS=AY
              ZPASS=0.0D0
              IPASS1=4
              CALL DEFGRIDS(4,ISURF,GERROR1,GERROR2)
              IF(GERROR1) THEN
                  IF(MSG) THEN
                      WRITE(OUTLYNE,*)'RAY FAILURE OCCURRED AT SURFACE ',ISURF
                      CALL SHOWIT(1)
                      OUTLYNE=
     1                'DEFORM FILE DOES NOT EXIST FOR THIS SURFACE'
                      CALL SHOWIT(1)
                      OUTLYNE=
     1                'OR IT HAS INSUFFICIENT DATA FOR THIS SURFACE'
                      CALL SHOWIT(1)
                  END IF
                  STOPP=1
                  RAYCOD(1)=17
                  RAYCOD(2)=ISURF
                  RAYEXT=.FALSE.
                  POLEXT=.FALSE.
                  SPDCD1=RAYCOD(1)
                  SPDCD2=ISURF
                  IF(GERROR2) THEN
                      IF(MSG) THEN
                          WRITE(OUTLYNE,*)'RAY FAILURE OCCURRED AT SURFACE ',ISURF
                          CALL SHOWIT(1)
                          OUTLYNE=
     1                    'RAY COORDINATE BEYOND DEFORMABLE SURFACE BOUNDS'
                          CALL SHOWIT(1)
                      END IF
                      STOPP=1
                      RAYCOD(1)=18
                      RAYCOD(2)=ISURF
                      RAYEXT=.FALSE.
                      POLEXT=.FALSE.
                      SPDCD1=RAYCOD(1)
                      SPDCD2=ISURF
                      RETURN
                  END IF
                  RETURN
              END IF
C     NOW ADD THE REGULAR TERM
C      IF(ZPASS.GE.ZZEMAX) ZZEMAX=ZPASS
C      IF(ZPASS.LE.ZZEMIN) ZZEMIN=ZPASS
              FNZ1=ZPASS+FNZZ1(AX,AY,AC1,AC2,AC3,AC4,AC5
     1        ,AC41,AC42,AC43,AC44,AC45)
              RETURN
          END IF
          RETURN
      END
C SUB NR2.FOR
      SUBROUTINE NR2(ERRR)
C
          IMPLICIT NONE
C
          LOGICAL DIFCAL,ERRR
C
          INTEGER SPDCD1,SPDCD2
C
          COMMON/SPRA2/SPDCD1,SPDCD2
C
C       THIS IS SUBROUTINE NR2.FOR. THIS SUBROUTINE IMPLEMENTS
C       INTERSECTION CALCULATION TO AN ASPHERIC/CONIC SURFACE
C
          EXTERNAL FNZ2
          EXTERNAL FUNC2X,FUNC2Y,DER2X,DER2Y
C
          REAL*8 C1,C2,C3,C4,ACLENG,FNZ2,DEL,SNGX,SNGY,SNGZ,
     1    FNXP,FNYP,FNZP,HV1,HV2,HV,XP,YP,ZP,C41,C42,C43,C44,C45,
     2    MAG,ZCAL,Q,C5,C6,ZZTOP,A34,ZTEST,CV,CC,DER2X,DER2Y,
     2    FUNC2X,FUNC2Y,XPASS,YPASS,ZPASS
          REAL*8 LENG
          COMMON/PASSLENG/LENG
          REAL*8 ZZEMAX,ZZEMIN
          COMMON/KENMOOR/ZZEMIN,ZZEMAX
C
          COMMON/SAGPAS/XPASS,YPASS,ZPASS
C
          COMMON/ACLEN/ACLENG
C
          INTEGER I
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          INCLUDE 'datmac.inc'
C
          IF(LN.NE.0.0D0) SNGX=LN/DABS(LN)
          IF(MN.NE.0.0D0) SNGY=MN/DABS(MN)
          IF(NN.NE.0.0D0) SNGZ=NN/DABS(NN)
          IF(LN.EQ.0.0D0) SNGX=1.0D0
          IF(MN.EQ.0.0D0) SNGY=1.0D0
          IF(NN.EQ.0.0D0) SNGZ=1.0D0
          DIFCAL=.FALSE.
          IF(ALENS(8,R_I).NE.0.0D0) DIFCAL=.TRUE.
          IF(ALENS(34,R_I).GT.0.0D0.AND.
     1    DABS(ALENS(34,R_I)).NE.6.0D0.AND.
     2    DABS(ALENS(34,R_I)).NE.7.0D0.AND.
     3    DABS(ALENS(34,R_I)).NE.9.0D0.AND.
     4    DABS(ALENS(34,R_I)).NE.10.0D0.AND.
     4    DABS(ALENS(34,R_I)).NE.11.0D0.AND.
     5    DABS(ALENS(34,R_I)).NE.12.0D0.AND.
     6    DABS(ALENS(34,R_I)).NE.13.0D0.AND.
     6    DABS(ALENS(34,R_I)).NE.15.0D0.AND.
     6    DABS(ALENS(34,R_I)).NE.16.0D0.AND.
     6    DABS(ALENS(34,R_I)).NE.17.0D0.AND.
     6    DABS(ALENS(34,R_I)).NE.19.0D0.AND.
     6    DABS(ALENS(34,R_I)).NE.20.0D0.AND.
     6    DABS(ALENS(34,R_I)).NE.22.0D0.AND.
     6    DABS(ALENS(34,R_I)).NE.24.0D0
     7    .OR.ALENS(103,R_I).EQ.1.0D0) THEN
C     WE GOT A REAL SPECIAL SURFACE WHICH NEEDS FINIT DIFFERENCE
C     SURFACE SLOPE CALCS IF SURFACE IS TYPE 5 OR THE COEFS ARE NOT
C     ALL ZERO
              IF(ALENS(34,R_I).GT.0.0D0) THEN
                  IF(FTFL01(1,R_I).NE.0.0D0) DIFCAL=.TRUE.
                  IF(FTFL01(2,R_I).NE.0.0D0) DIFCAL=.TRUE.
                  IF(FTFL01(3,R_I).NE.0.0D0) DIFCAL=.TRUE.
                  IF(FTFL01(4,R_I).NE.0.0D0) DIFCAL=.TRUE.
                  IF(FTFL01(5,R_I).NE.0.0D0) DIFCAL=.TRUE.
                  IF(FTFL01(6,R_I).NE.0.0D0) DIFCAL=.TRUE.
                  IF(FTFL01(7,R_I).NE.0.0D0) DIFCAL=.TRUE.
                  IF(FTFL01(8,R_I).NE.0.0D0) DIFCAL=.TRUE.
                  IF(FTFL01(9,R_I).NE.0.0D0) DIFCAL=.TRUE.
                  IF(FTFL01(10,R_I).NE.0.0D0) DIFCAL=.TRUE.
                  IF(FTFL01(11,R_I).NE.0.0D0) DIFCAL=.TRUE.
                  IF(FTFL01(12,R_I).NE.0.0D0) DIFCAL=.TRUE.
                  IF(FTFL01(13,R_I).NE.0.0D0) DIFCAL=.TRUE.
                  IF(FTFL01(14,R_I).NE.0.0D0) DIFCAL=.TRUE.
                  IF(FTFL01(15,R_I).NE.0.0D0) DIFCAL=.TRUE.
                  IF(FTFL01(16,R_I).NE.0.0D0) DIFCAL=.TRUE.
                  IF(FTFL01(17,R_I).NE.0.0D0) DIFCAL=.TRUE.
                  IF(FTFL01(18,R_I).NE.0.0D0) DIFCAL=.TRUE.
                  IF(FTFL01(19,R_I).NE.0.0D0) DIFCAL=.TRUE.
                  IF(FTFL01(20,R_I).NE.0.0D0) DIFCAL=.TRUE.
                  IF(FTFL01(21,R_I).NE.0.0D0) DIFCAL=.TRUE.
                  IF(FTFL01(22,R_I).NE.0.0D0) DIFCAL=.TRUE.
                  IF(FTFL01(23,R_I).NE.0.0D0) DIFCAL=.TRUE.
                  IF(FTFL01(24,R_I).NE.0.0D0) DIFCAL=.TRUE.
                  IF(FTFL01(25,R_I).NE.0.0D0) DIFCAL=.TRUE.
                  IF(FTFL01(26,R_I).NE.0.0D0) DIFCAL=.TRUE.
                  IF(FTFL01(27,R_I).NE.0.0D0) DIFCAL=.TRUE.
                  IF(FTFL01(28,R_I).NE.0.0D0) DIFCAL=.TRUE.
                  IF(FTFL01(29,R_I).NE.0.0D0) DIFCAL=.TRUE.
                  IF(FTFL01(30,R_I).NE.0.0D0) DIFCAL=.TRUE.
                  IF(FTFL01(31,R_I).NE.0.0D0) DIFCAL=.TRUE.
                  IF(FTFL01(32,R_I).NE.0.0D0) DIFCAL=.TRUE.
                  IF(FTFL01(33,R_I).NE.0.0D0) DIFCAL=.TRUE.
                  IF(FTFL01(34,R_I).NE.0.0D0) DIFCAL=.TRUE.
                  IF(FTFL01(35,R_I).NE.0.0D0) DIFCAL=.TRUE.
                  IF(FTFL01(36,R_I).NE.0.0D0) DIFCAL=.TRUE.
                  IF(FTFL01(37,R_I).NE.0.0D0) DIFCAL=.TRUE.
                  IF(FTFL01(38,R_I).NE.0.0D0) DIFCAL=.TRUE.
                  IF(FTFL01(39,R_I).NE.0.0D0) DIFCAL=.TRUE.
                  IF(FTFL01(40,R_I).NE.0.0D0) DIFCAL=.TRUE.
                  IF(FTFL01(41,R_I).NE.0.0D0) DIFCAL=.TRUE.
                  IF(FTFL01(42,R_I).NE.0.0D0) DIFCAL=.TRUE.
                  IF(FTFL01(43,R_I).NE.0.0D0) DIFCAL=.TRUE.
                  IF(FTFL01(44,R_I).NE.0.0D0) DIFCAL=.TRUE.
                  IF(FTFL01(45,R_I).NE.0.0D0) DIFCAL=.TRUE.
                  IF(FTFL01(46,R_I).NE.0.0D0) DIFCAL=.TRUE.
                  IF(FTFL01(47,R_I).NE.0.0D0) DIFCAL=.TRUE.
                  IF(FTFL01(48,R_I).NE.0.0D0) DIFCAL=.TRUE.
                  IF(FTFL01(49,R_I).NE.0.0D0) DIFCAL=.TRUE.
                  IF(FTFL01(50,R_I).NE.0.0D0) DIFCAL=.TRUE.
                  IF(FTFL01(51,R_I).NE.0.0D0) DIFCAL=.TRUE.
                  IF(FTFL01(52,R_I).NE.0.0D0) DIFCAL=.TRUE.
                  IF(FTFL01(53,R_I).NE.0.0D0) DIFCAL=.TRUE.
                  IF(FTFL01(54,R_I).NE.0.0D0) DIFCAL=.TRUE.
                  IF(FTFL01(55,R_I).NE.0.0D0) DIFCAL=.TRUE.
                  IF(FTFL01(56,R_I).NE.0.0D0) DIFCAL=.TRUE.
                  IF(FTFL01(57,R_I).NE.0.0D0) DIFCAL=.TRUE.
                  IF(FTFL01(58,R_I).NE.0.0D0) DIFCAL=.TRUE.
                  IF(FTFL01(59,R_I).NE.0.0D0) DIFCAL=.TRUE.
                  IF(FTFL01(60,R_I).NE.0.0D0) DIFCAL=.TRUE.
                  IF(FTFL01(61,R_I).NE.0.0D0) DIFCAL=.TRUE.
                  IF(FTFL01(62,R_I).NE.0.0D0) DIFCAL=.TRUE.
                  IF(FTFL01(63,R_I).NE.0.0D0) DIFCAL=.TRUE.
                  IF(FTFL01(64,R_I).NE.0.0D0) DIFCAL=.TRUE.
                  IF(FTFL01(65,R_I).NE.0.0D0) DIFCAL=.TRUE.
                  IF(FTFL01(66,R_I).NE.0.0D0) DIFCAL=.TRUE.
                  IF(FTFL01(67,R_I).NE.0.0D0) DIFCAL=.TRUE.
                  IF(FTFL01(68,R_I).NE.0.0D0) DIFCAL=.TRUE.
                  IF(FTFL01(69,R_I).NE.0.0D0) DIFCAL=.TRUE.
                  IF(FTFL01(70,R_I).NE.0.0D0) DIFCAL=.TRUE.
                  IF(FTFL01(71,R_I).NE.0.0D0) DIFCAL=.TRUE.
                  IF(FTFL01(72,R_I).NE.0.0D0) DIFCAL=.TRUE.
                  IF(FTFL01(73,R_I).NE.0.0D0) DIFCAL=.TRUE.
                  IF(FTFL01(74,R_I).NE.0.0D0) DIFCAL=.TRUE.
                  IF(FTFL01(75,R_I).NE.0.0D0) DIFCAL=.TRUE.
                  IF(FTFL01(76,R_I).NE.0.0D0) DIFCAL=.TRUE.
                  IF(FTFL01(77,R_I).NE.0.0D0) DIFCAL=.TRUE.
                  IF(FTFL01(78,R_I).NE.0.0D0) DIFCAL=.TRUE.
                  IF(FTFL01(79,R_I).NE.0.0D0) DIFCAL=.TRUE.
                  IF(FTFL01(80,R_I).NE.0.0D0) DIFCAL=.TRUE.
                  IF(FTFL01(81,R_I).NE.0.0D0) DIFCAL=.TRUE.
                  IF(FTFL01(82,R_I).NE.0.0D0) DIFCAL=.TRUE.
                  IF(FTFL01(83,R_I).NE.0.0D0) DIFCAL=.TRUE.
                  IF(FTFL01(84,R_I).NE.0.0D0) DIFCAL=.TRUE.
                  IF(FTFL01(85,R_I).NE.0.0D0) DIFCAL=.TRUE.
                  IF(FTFL01(86,R_I).NE.0.0D0) DIFCAL=.TRUE.
                  IF(FTFL01(87,R_I).NE.0.0D0) DIFCAL=.TRUE.
                  IF(FTFL01(88,R_I).NE.0.0D0) DIFCAL=.TRUE.
                  IF(FTFL01(89,R_I).NE.0.0D0) DIFCAL=.TRUE.
                  IF(FTFL01(90,R_I).NE.0.0D0) DIFCAL=.TRUE.
                  IF(FTFL01(91,R_I).NE.0.0D0) DIFCAL=.TRUE.
                  IF(FTFL01(92,R_I).NE.0.0D0) DIFCAL=.TRUE.
                  IF(FTFL01(93,R_I).NE.0.0D0) DIFCAL=.TRUE.
                  IF(FTFL01(94,R_I).NE.0.0D0) DIFCAL=.TRUE.
                  IF(FTFL01(95,R_I).NE.0.0D0) DIFCAL=.TRUE.
                  IF(FTFL01(96,R_I).NE.0.0D0) DIFCAL=.TRUE.
              END IF
          END IF
          A34=ALENS(34,R_I)
C
          CV=ALENS(1,R_I)
          CC=ALENS(2,R_I)
          IF(CC.EQ.-1.0D0.AND.CV.GT.0.0D0) ZTEST=1.0D20
          IF(CC.EQ.-1.0D0.AND.CV.LT.0.0D0) ZTEST=-1.0D20
          IF(CC.NE.-1.0D0) ZTEST=1.0D0/((CC+1.0D0)*CV)
C
          C1=ALENS(4,R_I)
          C2=ALENS(5,R_I)
          C3=ALENS(6,R_I)
          C4=ALENS(7,R_I)
          C41=ALENS(81,R_I)
          C42=ALENS(82,R_I)
          C43=ALENS(83,R_I)
          C44=ALENS(84,R_I)
          C45=ALENS(85,R_I)
          C5=ALENS(2,R_I)
          C6=(ALENS(1,R_I))
C
C       NOW REFINE TO INTERSECT WITH THE ASPHERIC
C
C       THE SAG EQUATION IS:
C
C       THE RETURNED VALUES OF X,Y,Z ARE THE CORRECT INTERSECTIONS
C       TO WITHIN THE SURTOL VALUE
C
C       C5=CONIC CONSTANT
C       C6=CURVATURE
C       C1 TO C4 ARE THE 4TH, 6TH, 8TH AND 10TH ORDER ASPHERIC
C       DEFORMATION TERMS
C       C41 TO C45 ARE THE 12TH THROUGH 20TH ORDER ASPHERIC
C       DEFORMATION TERMS
C
C       CALCULATE INTERSECTION
          DO 10 I=1,NRAITR
C
              Q=(1.0D0-((C5+1.0D0)*(C6**2)*
     1        (((R_X**2)+(R_Y**2)))))
              IF(Q.LT.0.0D0) THEN
                  IF(MSG) THEN
                      WRITE(OUTLYNE,*)'RAY FAILURE OCCURRED AT SURFACE ',R_I
                      CALL SHOWIT(1)
                      OUTLYNE='RAY DID NOT INTERSECT THE SURFACE'
                      CALL SHOWIT(1)
                  END IF
                  RAYCOD(1)=1
                  RAYCOD(2)=R_I
                  SPDCD1=RAYCOD(1)
                  SPDCD2=R_I
                  STOPP=1
                  RETURN
              END IF
              Q=DSQRT(Q)
              Q=1.0D0+Q
              DEL=DELSUR
C
C     X AND Y DERIVATIVES OF THE SURFACE ARE CALCULATED HERE:
C     FUNCTION FNZ2 EVALUATES THE FUNCTION
C
              IF(DIFCAL) THEN
                  ERRR=.FALSE.
                  FNXP=-DER2X(FUNC2X,R_X,DELSUR,ERRR)
                  FNYP=-DER2Y(FUNC2Y,R_Y,DELSUR,ERRR)
                  IF(ERRR) RETURN
              ELSE
                  FNXP=-(
     1            (((2.0D0*C6*R_X*Q*(Q-1.0D0))+
     1            ((C6**3)*R_X*((R_X**2)+(R_Y**2))*(C5+1.0D0)))/((Q-1.0D0)*(Q**2)))
     1            )
                  FNYP=-(
     1            (((2.0D0*C6*R_Y*Q*(Q-1.0D0))+
     1            ((C6**3)*R_Y*((R_X**2)+(R_Y**2))*(C5+1.0D0)))/((Q-1.0D0)*(Q**2)))
     1            )
              END IF
              FNZP=SNGZ
C
C     X AND Y DERIVATIVES OF THE SURFACE HAVE BEEN CALCULATED
C
              MAG=DSQRT((FNXP**2)+(FNYP**2)+(FNZP**2))
              LN=FNXP/MAG
              MN=FNYP/MAG
              NN=FNZP/MAG
C
C       THESE ARE THE DIRECTION COSINES FOR THE SURFACE NORMAL
C       FOR THE CURRENT X,Y AND Z
C
C       IF X,Y AND Z ARE THE CORRECT X,Y AND Z THEN
C       ZCAL WILL EQUAL Z SO THAT F(X,Y,Z)=0
              ZCAL=FNZ2(R_X,R_Y,C1,C2,C3,C4,C6,R_I,A34,Q
     1        ,C41,C42,C43,C44,C45)
              HV1=ZCAL-R_Z
              HV2=((R_L*LN)+(R_M*MN)+(R_N*NN))
              HV=HV1/HV2
C       NEW X,Y AND Z ARE
              XP=R_X+(HV*R_L)
              YP=R_Y+(HV*R_M)
              ZP=R_Z+(HV*R_N)
C       SHOULD WE CONTINUE?
              ZZTOP=FNZ2(XP,YP,C1,C2,C3,C4,C6,R_I,A34,Q
     1        ,C41,C42,C43,C44,C45)
              LENG=DABS(ZZTOP-ZP)
              ACLENG=LENG
              IF(LENG.LE.SURTOL) THEN
                  R_X=XP
                  R_Y=YP
                  R_Z=ZP
                  Q=(1.0D0-((C5+1.0D0)*(C6**2)*
     1            ((R_X**2)+(R_Y**2))))
                  IF(Q.LT.0.0D0) THEN
                      IF(MSG) THEN
                          WRITE(OUTLYNE,*)'RAY FAILURE OCCURRED AT SURFACE ',R_I
                          CALL SHOWIT(1)
                          OUTLYNE='RAY DID NOT INTERSECT THE SURFACE'
                          CALL SHOWIT(1)
                      END IF
                      RAYCOD(1)=1
                      RAYCOD(2)=R_I
                      SPDCD1=RAYCOD(1)
                      SPDCD2=R_I
                      STOPP=1
                      RETURN
                  END IF
                  Q=DSQRT(Q)
                  Q=1.0D0+Q
C
C     X AND Y DERIVATIVES OF THE SURFACE ARE CALCULATED HERE:
C     FUNCTION FNZ2 EVALUATES THE FUNCTION
C
                  IF(DIFCAL) THEN
                      ERRR=.FALSE.
                      FNXP=-DER2X(FUNC2X,R_X,DELSUR,ERRR)
                      FNYP=-DER2Y(FUNC2Y,R_Y,DELSUR,ERRR)
                      IF(ERRR) RETURN
                  ELSE
                      FNXP=-(
     1                (((2.0D0*C6*R_X*Q*(Q-1.0D0))+
     1                ((C6**3)*R_X*((R_X**2)+(R_Y**2))*(C5+1.0D0)))/((Q-1.0D0)*(Q**2)))
     1                )
                      FNYP=-(
     1                (((2.0D0*C6*R_Y*Q*(Q-1.0D0))+
     1                ((C6**3)*R_Y*((R_X**2)+(R_Y**2))*(C5+1.0D0)))/((Q-1.0D0)*(Q**2)))
     1                )
                  END IF
                  FNZP=SNGZ
C
C     X AND Y DERIVATIVES OF THE SURFACE HAVE BEEN CALCULATED
C
                  MAG=DSQRT((FNXP**2)+(FNYP**2)+(FNZP**2))
                  LN=FNXP/MAG
                  MN=FNYP/MAG
                  NN=FNZP/MAG
                  STOPP=0
                  RETURN
              ELSE
C       KEEP TRYING
C       MAKE X Y Z FROM XP YP ZP AND TRY AGAIN
                  R_X=XP
                  R_Y=YP
                  R_Z=ZP
              END IF
 10       CONTINUE
          IF(MSG) THEN
              WRITE(OUTLYNE,*)'RAY FAILURE OCCURRED AT SURFACE ',R_I
              CALL SHOWIT(1)
              OUTLYNE=
     1        'CONVERGENCE FAILURE OCCURRED DURING RAY INTERSECTION'
              CALL SHOWIT(1)
              OUTLYNE='WITH ASPHERIC, TORIC OR SPECIAL SURFACE'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'ACCURACY OF INTERSECTION WAS = ',LENG
              CALL SHOWIT(1)
          END IF
          RAYCOD(1)=2
          RAYCOD(2)=R_I
          SPDCD1=RAYCOD(1)
          SPDCD2=R_I
          R_X=XP
          R_Y=YP
          R_Z=ZP
          STOPP=0
          RETURN
      END
      FUNCTION FNZ2(AX,AY,AC1,AC2,AC3,AC4,AC6,AR,A34,Q
     1,AC41,AC42,AC43,AC44,AC45)
          USE GLOBALS
C
          IMPLICIT NONE
C
          EXTERNAL FF2,FF3,FF4,FF5
C
          REAL*8 FNZ2,AX,AY,AC1,AC2,AC3,AC4,AAC1,AAC2,AAC3
     1    ,AAC4,AAX,AAY,FNZZ2,A34,R,THETA,FF2,FF3,AQ,Q,AC6,FF4,XX,YY,
     2    AAC6,FF5,AAAX,AAAY,AC41,AC42,AC43,AC44,AC45,AAC41,AAC42,AAC43
     3    ,AAC44,AAC45,AZ
C
          INTEGER IPASS1
C
          REAL*8 XPASS,YPASS,ZPASS
          REAL*8 ZZEMAX,ZZEMIN,DA34
          REAL*8 JK_WAVE
          REAL*8 AMP1,OMEGA1X,OMEGA1Y
          REAL*8 AMP2,OMEGA2X,OMEGA2Y
          REAL*8 AMP3,OMEGA3X,OMEGA3Y
          REAL*8 AMP4,OMEGA4X,OMEGA4Y
          REAL*8 AMP5,OMEGA5X,OMEGA5Y
          COMMON/KENMOOR/ZZEMIN,ZZEMAX
C
          COMMON/SAGPAS/XPASS,YPASS,ZPASS
C
          INTEGER SPDCD1,SPDCD2
C
          COMMON/SPRA2/SPDCD1,SPDCD2
C
          INTEGER KLI,AR,ISURF
C
          LOGICAL FUN,GERROR,GERROR1,GERROR2,UERROR
C
          COMMON/NEFER/KLI
C
          COMMON/COMFUN/FUN
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datmac.inc'
C
          INTEGER I
C
          FNZZ2(AAX,AAY,AAC1,AAC2,AAC3,AAC4,AAC6,AQ,
     1     AAC41,AAC42,AAC43,AAC44,AAC45)=
     1     ((((AAX**2)+(AAY**2))**2)*AAC1)+
     1     ((((AAX**2)+(AAY**2))**3)*AAC2)+
     1     ((((AAX**2)+(AAY**2))**4)*AAC3)+
     1     ((((AAX**2)+(AAY**2))**5)*AAC4)+
     1     ((((AAX**2)+(AAY**2))**6)*AAC41)+
     1     ((((AAX**2)+(AAY**2))**7)*AAC42)+
     1     ((((AAX**2)+(AAY**2))**8)*AAC43)+
     1     ((((AAX**2)+(AAY**2))**9)*AAC44)+
     1     ((((AAX**2)+(AAY**2))**10)*AAC45)+
     1     ((AAC6*((AAX**2)+(AAY**2)))/AQ)
C
C     IF SPECIAL SURFACE TYPE 1, 2 OR 3 ARE TO BE APPLIED
C     THEY ARE APPLIED HERE
C
          DA34=DABS(A34)
          IF(A34.EQ.0.0D0.AND.ALENS(103,AR).EQ.0.0D0.OR.DA34.EQ.6.0D0.OR.
     1    DA34.EQ.7.0D0.OR.DA34.EQ.9.0D0.OR.DA34.EQ.10.0D0.OR.
     1    DA34.EQ.11.0D0.OR.DA34.EQ.12.0D0.OR.
     1    DA34.EQ.15.0D0.OR.DA34.EQ.16.0D0.OR.DA34.EQ.17.0D0.OR.
     1    DA34.EQ.19.0D0.OR.DA34.EQ.20.0D0) THEN
              FNZ2=FNZZ2(AX,AY,AC1,AC2,AC3,AC4,AC6,Q,AC41,AC42,AC43,AC44,AC45)
              RETURN
          END IF
          FNZ2=0.0D0
C     SPECIAL SURFACE TYPE 1
          IF(A34.EQ.1.0D0) THEN
              DO I=9,48
                  IF(FTFL01(I,AR).NE.0.0D0) FNZ2=FNZ2+
     1            (FTFL01(I,AR)*(((DSQRT((AX**2)+(AY**2)))**(I-9))))
              END DO
C     NOW ADD THE REGULAR TERM
              FNZ2=FNZ2+FNZZ2(AX,AY,AC1,AC2,AC3,AC4,AC6,Q
     1        ,AC41,AC42,AC43,AC44,AC45)
              RETURN
          END IF
C     SPECIAL SURFACE TYPE 4
          IF(A34.EQ.4.0D0) THEN
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
              AMP1=DABS(FTFL01(1,AR)*JK_WAVE*0.5D0)
              AMP2=DABS(FTFL01(4,AR)*JK_WAVE*0.5D0)
              AMP3=DABS(FTFL01(7,AR)*JK_WAVE*0.5D0)
              AMP4=DABS(FTFL01(10,AR)*JK_WAVE*0.5D0)
              AMP5=DABS(FTFL01(13,AR)*JK_WAVE*0.5D0)
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
              AMP1=FTFL01(1,AR)
              AMP2=FTFL01(4,AR)
              AMP3=FTFL01(7,AR)
              AMP4=FTFL01(10,AR)
              AMP5=FTFL01(13,AR)
              IF(FTFL01(2,AR).EQ.0.0D0) THEN
                  OMEGA1X=0.0D0
              ELSE
                  OMEGA1X=TWOPII/DABS(FTFL01(2,AR))
              END IF
              IF(FTFL01(3,AR).EQ.0.0D0) THEN
                  OMEGA1Y=0.0D0
              ELSE
                  OMEGA1Y=TWOPII/DABS(FTFL01(3,AR))
              END IF
              IF(FTFL01(5,AR).EQ.0.0D0) THEN
                  OMEGA2X=0.0D0
              ELSE
                  OMEGA2X=TWOPII/DABS(FTFL01(5,AR))
              END IF
              IF(FTFL01(6,AR).EQ.0.0D0) THEN
                  OMEGA2Y=0.0D0
              ELSE
                  OMEGA2Y=TWOPII/DABS(FTFL01(6,AR))
              END IF
              IF(FTFL01(8,AR).EQ.0.0D0) THEN
                  OMEGA3X=0.0D0
              ELSE
                  OMEGA3X=TWOPII/DABS(FTFL01(8,AR))
              END IF
              IF(FTFL01(9,AR).EQ.0.0D0) THEN
                  OMEGA3Y=0.0D0
              ELSE
                  OMEGA3Y=TWOPII/DABS(FTFL01(9,AR))
              END IF
              IF(FTFL01(11,AR).EQ.0.0D0) THEN
                  OMEGA4X=0.0D0
              ELSE
                  OMEGA4X=TWOPII/DABS(FTFL01(11,AR))
              END IF
              IF(FTFL01(12,AR).EQ.0.0D0) THEN
                  OMEGA4Y=0.0D0
              ELSE
                  OMEGA4Y=TWOPII/DABS(FTFL01(12,AR))
              END IF
              IF(FTFL01(14,AR).EQ.0.0D0) THEN
                  OMEGA5X=0.0D0
              ELSE
                  OMEGA5X=TWOPII/DABS(FTFL01(14,AR))
              END IF
              IF(FTFL01(15,AR).EQ.0.0D0) THEN
                  OMEGA5Y=0.0D0
              ELSE
                  OMEGA5Y=TWOPII/DABS(FTFL01(15,AR))
              END IF
              FNZ2=(AMP1*(DCOS(OMEGA1X*AX)*(DCOS(OMEGA1Y*AY))))
     1        +(AMP2*(DCOS(OMEGA2X*AX)*(DCOS(OMEGA2Y*AY))))
     1        +(AMP3*(DCOS(OMEGA3X*AX)*(DCOS(OMEGA3Y*AY))))
     1        +(AMP4*(DCOS(OMEGA4X*AX)*(DCOS(OMEGA4Y*AY))))
     1        +(AMP5*(DCOS(OMEGA5X*AX)*(DCOS(OMEGA5Y*AY))))
C     NOW ADD THE REGULAR TERM
              FNZ2=FNZ2+FNZZ2(AX,AY,AC1,AC2,AC3,AC4,AC6,Q
     1        ,AC41,AC42,AC43,AC44,AC45)
              RETURN
          END IF
C     SPECIAL SURFACE TYPE 2
          IF(A34.EQ.2.0D0) THEN
              INR=ALENS(76,AR)
              AAAX=AX/INR
              AAAY=AY/INR
              R=DSQRT((AAAX**2)+(AAAY**2))
              IF(DABS(AAAY).GE.DABS(((1.0D35)*AAAX))) THEN
                  IF(AAAY.GE.0.0D0) THETA=PII/2.0D0
                  IF(AAAY.LT.0.0D0) THETA=(3.0D0*PII)/2.0D0
              ELSE
                  IF(DABS(AAAY).EQ.0.0D0.AND.DABS(AAAX).EQ.0.0D0) THEN
                      THETA=0.0D0
                  ELSE
                      THETA=DATAN2(AAAY,AAAX)
                  END IF
                  IF(THETA.LT.0.0D0) THETA=THETA+(TWOPII)
              END IF
              FNZ2=0.0D0
              DO I=1,66
                  IF(FTFL01(I,AR).NE.0.0D0) FNZ2=FNZ2+
     1            (FTFL01(I,AR)*(FF2(R,THETA,I)))
              END DO
C     NOW ADD THE REGULAR TERM
              FNZ2=FNZ2+FNZZ2(AX,AY,AC1,AC2,AC3,AC4,AC6,Q
     1        ,AC41,AC42,AC43,AC44,AC45)
              RETURN
          END IF
C     SPECIAL SURFACE TYPE 14
          IF(A34.EQ.14.0D0) THEN
              INR=ALENS(76,AR)
              AAAX=AX/INR
              AAAY=AY/INR
              R=DSQRT((AAAX**2)+(AAAY**2))
              IF(DABS(AAAY).GE.DABS(((1.0D35)*AAAX))) THEN
                  IF(AAAY.GE.0.0D0) THETA=PII/2.0D0
                  IF(AAAY.LT.0.0D0) THETA=(3.0D0*PII)/2.0D0
              ELSE
                  IF(DABS(AAAY).EQ.0.0D0.AND.DABS(AAAX).EQ.0.0D0) THEN
                      THETA=0.0D0
                  ELSE
                      THETA=DATAN2(AAAY,AAAX)
                  END IF
                  IF(THETA.LT.0.0D0) THETA=THETA+(TWOPII)
              END IF
              FNZ2=0.0D0
              DO I=1,48
                  IF(FTFL01(I,AR).NE.0.0D0) FNZ2=FNZ2+
     1            (FTFL01(I,AR)*(FF5(R,THETA,I)))
              END DO
C     NOW ADD THE REGULAR TERM
              FNZ2=FNZ2+FNZZ2(AX,AY,AC1,AC2,AC3,AC4,AC6,Q
     1        ,AC41,AC42,AC43,AC44,AC45)
              RETURN
          END IF
C     SPECIAL SURFACE TYPE 3
          IF(A34.EQ.3.0D0) THEN
              INR=ALENS(76,AR)
              AAAX=AX/INR
              AAAY=AY/INR
              R=DSQRT((AAAX**2)+(AAAY**2))
              IF(DABS(AAAY).GE.DABS(((1.0D35)*AAAX))) THEN
                  IF(AAAY.GE.0.0D0) THETA=PII/2.0D0
                  IF(AAAY.LT.0.0D0) THETA=(3.0D0*PII)/2.0D0
              ELSE
                  IF(DABS(AAAY).EQ.0.0D0.AND.DABS(AAAX).EQ.0.0D0) THEN
                      THETA=0.0D0
                  ELSE
                      THETA=DATAN2(AAAY,AAAX)
                  END IF
                  IF(THETA.LT.0.0D0) THETA=THETA+(TWOPII)
              END IF
              FNZ2=0.0D0
              DO I=1,37
                  IF(FTFL01(I,AR).NE.0.0D0) FNZ2=FNZ2+
     1            (FTFL01(I,AR)*(FF3(R,THETA,I)))
              END DO
C     NOW ADD THE REGULAR TERM
              FNZ2=FNZ2+FNZZ2(AX,AY,AC1,AC2,AC3,AC4,AC6,Q
     1        ,AC41,AC42,AC43,AC44,AC45)
              RETURN
          END IF
C     IF SPECIAL SURFACE TYPE 23
          IF(A34.EQ.23.0D0) THEN
C
              FNZ2=0.0D0
              IF(INT(FTFL01(1,AR)).LT.3.0D0) RETURN
              AZ=0.0D0
              IPASS1=3
              CALL SPL23(AR,AX,AY,AZ,IPASS1)
              FNZ2=AZ
C     NOW ADD THE REGULAR TERM
              FNZ2=FNZ2+FNZZ2(AX,AY,AC1,AC2,AC3,AC4,AC6,Q
     1        ,AC41,AC42,AC43,AC44,AC45)
          END IF
C     SPECIAL SURFACE TYPE 8
          IF(A34.EQ.8.0D0) THEN
              XX=AX
              YY=AY
              FNZ2=0.0D0
              DO I=1,91
                  IF(FTFL01(I,AR).NE.0.0D0) FNZ2=FNZ2+
     1            (FTFL01(I,AR)*(FF4(XX,YY,I)))
              END DO
C     NOW ADD THE REGULAR TERM
              FNZ2=FNZ2+FNZZ2(AX,AY,AC1,AC2,AC3,AC4,AC6,Q
     1        ,AC41,AC42,AC43,AC44,AC45)
              RETURN
          END IF
C     SPECIAL SURFACE TYPE 5
C     USER DEFINED SURFACE
          IF(A34.EQ.5.0D0) THEN
              REG(40)=REG(9)
              REG(9)=AX
              REG(10)=AY
              REG(12)=DBLE(I)
C     THIS SURFACE AUTOMATICALLY USES MACRO FUNCTION FUN10
C     FOR THE INITIAL USER FUCTION TO BE CALLED TO EVALUATE
C     THE Z VALUE CORRESPONDING TO THE AY AND AX VALUES.
C     COEFFICIENTS C1 TO C96 ARE USED BY THE USER IN THE
C     MACRO FUNCTION FUN10 AND ANY MACRO FUNCTION CALLED BY FUN10.
C     THEY ARE STORED INITIALLY IN GENERAL PURPOSE STORAGE REGISTERS
C     NUMBERS 301 TO 396 AND ACCESSED BY THE USER VIA RCL COMMANDS
              GPREG(301)=FTFL01(1,AR)
              GPREG(302)=FTFL01(2,AR)
              GPREG(303)=FTFL01(3,AR)
              GPREG(304)=FTFL01(4,AR)
              GPREG(305)=FTFL01(5,AR)
              GPREG(306)=FTFL01(6,AR)
              GPREG(307)=FTFL01(7,AR)
              GPREG(308)=FTFL01(8,AR)
              GPREG(309)=FTFL01(9,AR)
              GPREG(310)=FTFL01(10,AR)
              GPREG(311)=FTFL01(11,AR)
              GPREG(312)=FTFL01(12,AR)
              GPREG(313)=FTFL01(13,AR)
              GPREG(314)=FTFL01(14,AR)
              GPREG(315)=FTFL01(15,AR)
              GPREG(316)=FTFL01(16,AR)
              GPREG(317)=FTFL01(17,AR)
              GPREG(318)=FTFL01(18,AR)
              GPREG(319)=FTFL01(19,AR)
              GPREG(320)=FTFL01(20,AR)
              GPREG(321)=FTFL01(21,AR)
              GPREG(322)=FTFL01(22,AR)
              GPREG(323)=FTFL01(23,AR)
              GPREG(324)=FTFL01(24,AR)
              GPREG(325)=FTFL01(25,AR)
              GPREG(326)=FTFL01(26,AR)
              GPREG(327)=FTFL01(27,AR)
              GPREG(328)=FTFL01(28,AR)
              GPREG(329)=FTFL01(29,AR)
              GPREG(330)=FTFL01(30,AR)
              GPREG(331)=FTFL01(31,AR)
              GPREG(332)=FTFL01(32,AR)
              GPREG(333)=FTFL01(33,AR)
              GPREG(334)=FTFL01(34,AR)
              GPREG(335)=FTFL01(35,AR)
              GPREG(336)=FTFL01(36,AR)
              GPREG(337)=FTFL01(37,AR)
              GPREG(338)=FTFL01(38,AR)
              GPREG(339)=FTFL01(39,AR)
              GPREG(340)=FTFL01(40,AR)
              GPREG(341)=FTFL01(41,AR)
              GPREG(342)=FTFL01(42,AR)
              GPREG(343)=FTFL01(43,AR)
              GPREG(344)=FTFL01(44,AR)
              GPREG(345)=FTFL01(45,AR)
              GPREG(346)=FTFL01(46,AR)
              GPREG(347)=FTFL01(47,AR)
              GPREG(348)=FTFL01(48,AR)
              GPREG(349)=FTFL01(49,AR)
              GPREG(350)=FTFL01(50,AR)
              GPREG(351)=FTFL01(51,AR)
              GPREG(352)=FTFL01(52,AR)
              GPREG(353)=FTFL01(53,AR)
              GPREG(354)=FTFL01(54,AR)
              GPREG(355)=FTFL01(55,AR)
              GPREG(356)=FTFL01(56,AR)
              GPREG(357)=FTFL01(57,AR)
              GPREG(358)=FTFL01(58,AR)
              GPREG(359)=FTFL01(59,AR)
              GPREG(360)=FTFL01(60,AR)
              GPREG(361)=FTFL01(61,AR)
              GPREG(362)=FTFL01(62,AR)
              GPREG(363)=FTFL01(63,AR)
              GPREG(364)=FTFL01(64,AR)
              GPREG(365)=FTFL01(65,AR)
              GPREG(366)=FTFL01(66,AR)
              GPREG(367)=FTFL01(67,AR)
              GPREG(368)=FTFL01(68,AR)
              GPREG(369)=FTFL01(69,AR)
              GPREG(370)=FTFL01(70,AR)
              GPREG(371)=FTFL01(71,AR)
              GPREG(372)=FTFL01(72,AR)
              GPREG(373)=FTFL01(73,AR)
              GPREG(374)=FTFL01(74,AR)
              GPREG(375)=FTFL01(75,AR)
              GPREG(376)=FTFL01(76,AR)
              GPREG(377)=FTFL01(77,AR)
              GPREG(378)=FTFL01(78,AR)
              GPREG(379)=FTFL01(79,AR)
              GPREG(380)=FTFL01(80,AR)
              GPREG(381)=FTFL01(81,AR)
              GPREG(382)=FTFL01(82,AR)
              GPREG(383)=FTFL01(83,AR)
              GPREG(384)=FTFL01(84,AR)
              GPREG(385)=FTFL01(85,AR)
              GPREG(386)=FTFL01(86,AR)
              GPREG(387)=FTFL01(87,AR)
              GPREG(388)=FTFL01(88,AR)
              GPREG(389)=FTFL01(89,AR)
              GPREG(390)=FTFL01(90,AR)
              GPREG(391)=FTFL01(91,AR)
              GPREG(392)=FTFL01(92,AR)
              GPREG(393)=FTFL01(93,AR)
              GPREG(394)=FTFL01(94,AR)
              GPREG(395)=FTFL01(95,AR)
              GPREG(396)=FTFL01(96,AR)
              IF(.NOT.FUNEXT(10)) THEN
C     NO FUN10 EXISTS, RETURN 0.0D0 AS THE Z CONTRIBUTION
                  FNZ2=0.0D0
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
                  FMNW(1:5)=0.0D0
                  FMDF(1:5)=1
                  NESFUN(0:10)=.FALSE.
                  KLI=10
                  FUN=.TRUE.
                  F26=1
                  CALL FUNEXC
                  REST_KDP(1)=RESTINPT(1)
                  F26=0
C     USE WHAT REMAINS IN THE Z-REGISTER
                  FNZ2=REG(11)
              END IF
C     NOW ADD THE REGULAR TERM
              FNZ2=FNZ2+FNZZ2(AX,AY,AC1,AC2,AC3,AC4,AC6,Q
     1        ,AC41,AC42,AC43,AC44,AC45)
              RETURN
          END IF
          IF(A34.EQ.21.0D0) THEN
              FNZ2=0.0D0
              CALL USERSURF(AR,AX,AY,FNZ2,UERROR)
              IF(UERROR) THEN
                  UERROR=.FALSE.
                  RETURN
              END IF
C     NOW ADD THE REGULAR TERM
              FNZ2=FNZ2+FNZZ2(AX,AY,AC1,AC2,AC3,AC4,AC6,Q
     1        ,AC41,AC42,AC43,AC44,AC45)
              RETURN
          END IF
C     SPECIAL SURFACE TYPE 22
C     GRID SAG
          IF(A34.EQ.22.0D0) THEN
C     USE WHAT REMAINS IN THE Z-REGISTER
C     CALL GRID
C     TYPE 22 SAG GRID
              ISURF=AR
              XPASS=AX
              YPASS=AY
              ZPASS=0.0D0
              CALL GRIDS(4,ISURF,GERROR)
              IF(.NOT.GERROR) GRIDSUNLOADED22(ISURF)=.FALSE.
              IF(GERROR) THEN
                  IF(MSG) THEN
                      WRITE(OUTLYNE,*)'RAY FAILURE OCCURRED AT SURFACE ',ISURF
                      CALL SHOWIT(1)
                      OUTLYNE=
     1                'GRID FILE DOES NOT EXIST FOR THIS SURFACE'
                      CALL SHOWIT(1)
                      OUTLYNE=
     1                'OR IT HAS INSUFFICIENT DATA FOR THIS SURFACE'
                      CALL SHOWIT(1)
                  END IF
                  STOPP=1
                  RAYCOD(1)=15
                  RAYCOD(2)=ISURF
                  RAYEXT=.FALSE.
                  POLEXT=.FALSE.
                  SPDCD1=RAYCOD(1)
                  SPDCD2=ISURF
                  RETURN
              END IF
C     NOW ADD THE REGULAR TERM
C     IF(ZPASS.GE.ZZEMAX) ZZEMAX=ZPASS
C     IF(ZPASS.LE.ZZEMIN) ZZEMIN=ZPASS
              FNZ2=ZPASS+FNZZ2(AX,AY,AC1,AC2,AC3,AC4,AC6,Q
     1        ,AC41,AC42,AC43,AC44,AC45)
              RETURN
          END IF
C     DEFORM SURFACE
          IF(ALENS(103,AR).EQ.1.0D0) THEN
C     USE WHAT REMAINS IN THE Z-REGISTER
C     CALL DEFGRID
              ISURF=AR
              XPASS=AX
              YPASS=AY
              ZPASS=0.0D0
              IPASS1=4
              CALL DEFGRIDS(4,ISURF,GERROR1,GERROR2)
              IF(GERROR1) THEN
                  IF(MSG) THEN
                      WRITE(OUTLYNE,*)'RAY FAILURE OCCURRED AT SURFACE ',ISURF
                      CALL SHOWIT(1)
                      OUTLYNE=
     1                'DEFORM FILE DOES NOT EXIST FOR THIS SURFACE'
                      CALL SHOWIT(1)
                      OUTLYNE=
     1                'OR IT HAS INSUFFICIENT DATA FOR THIS SURFACE'
                      CALL SHOWIT(1)
                  END IF
                  STOPP=1
                  RAYCOD(1)=17
                  RAYCOD(2)=ISURF
                  RAYEXT=.FALSE.
                  POLEXT=.FALSE.
                  SPDCD1=RAYCOD(1)
                  SPDCD2=ISURF
                  RETURN
              END IF
              IF(GERROR2) THEN
                  IF(MSG) THEN
                      WRITE(OUTLYNE,*)'RAY FAILURE OCCURRED AT SURFACE ',ISURF
                      CALL SHOWIT(1)
                      OUTLYNE=
     1                'RAY COORDINATE BEYOND DEFORMABLE SURFACE BOUNDS'
                      CALL SHOWIT(1)
                  END IF
                  STOPP=1
                  RAYCOD(1)=18
                  RAYCOD(2)=ISURF
                  RAYEXT=.FALSE.
                  POLEXT=.FALSE.
                  SPDCD1=RAYCOD(1)
                  SPDCD2=ISURF
                  RETURN
              END IF
C     NOW ADD THE REGULAR TERM
C     IF(ZPASS.GE.ZZEMAX) ZZEMAX=ZPASS
C     IF(ZPASS.LE.ZZEMIN) ZZEMIN=ZPASS
              FNZ2=ZPASS+FNZZ2(AX,AY,AC1,AC2,AC3,AC4,AC6,Q
     1        ,AC41,AC42,AC43,AC44,AC45)
              RETURN
          END IF
          RETURN
      END
C SUB NRPARAX.FOR
      SUBROUTINE NRPARAX
C
          IMPLICIT NONE
C
          INTEGER SPDCD1,SPDCD2
C
          COMMON/SPRA2/SPDCD1,SPDCD2
C
C       THIS IS SUBROUTINE NRPARAX.FOR. THIS DOES SURF NORM. FOR PARAXIAL
C       SURFACES
C
!        INTEGER I
C
          LOGICAL ERRR
C
          REAL*8 ACLENG,DEL,FNZ3
     1    ,FNXP,FNYP,FNZP,CV,ZTEST,SNGX,SNGY,SNGZ
     2    ,MAG
     2    ,FUNC1X,FUNC1Y,XPASS,YPASS,ZPASS
     2    ,FUNC2X,FUNC2Y,DER1X,DER1Y,DER2X,DER2Y
     2    ,FUNC3X,FUNC3Y,DER3X,DER3Y
          REAL*8 LENG
          COMMON/PASSLENG/LENG
C
          REAL*8 ZZEMAX,ZZEMIN
          COMMON/KENMOOR/ZZEMIN,ZZEMAX
C
          EXTERNAL FNZ1
          EXTERNAL FUNC1X,FUNC1Y
          EXTERNAL FNZ2
          EXTERNAL FUNC2X,FUNC2Y
          EXTERNAL FNZ3
          EXTERNAL FUNC3X,FUNC3Y
          EXTERNAL DER1X,DER1Y,DER2X,DER2Y,DER3X,DER3Y
C
          COMMON/SAGPAS/XPASS,YPASS,ZPASS
C
          COMMON/ACLEN/ACLENG
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          INCLUDE 'datmac.inc'
C
          IF(LN.NE.0.0D0) SNGX=LN/DABS(LN)
          IF(MN.NE.0.0D0) SNGY=MN/DABS(MN)
          IF(NN.NE.0.0D0) SNGZ=NN/DABS(NN)
          IF(LN.EQ.0.0D0) SNGX=1.0D0
          IF(MN.EQ.0.0D0) SNGY=1.0D0
          IF(NN.EQ.0.0D0) SNGZ=1.0D0
C
          IF(DABS(ALENS(1,R_I)).GT.DABS(ALENS(24,R_I))) THEN
              CV=ALENS(1,R_I)
          ELSE
              CV=ALENS(24,R_I)
          END IF
C
          IF(CV.NE.0.0D0) ZTEST=1.0D0/CV
C
C
          DEL=DELSUR
C
          ERRR=.FALSE.
          IF(ALENS(1,R_I).EQ.0.0D0.AND.ALENS(24,R_I).EQ.0.0D0) THEN
              FNXP=-DER1X(FUNC1X,R_X,DELSUR,ERRR)
              FNYP=-DER1Y(FUNC1Y,R_Y,DELSUR,ERRR)
              IF(ERRR) RETURN
          END IF
          IF(ALENS(1,R_I).NE.0.0D0.AND.ALENS(24,R_I).EQ.0.0D0) THEN
              FNXP=-DER2X(FUNC2X,R_X,DELSUR,ERRR)
              FNYP=-DER2Y(FUNC2Y,R_Y,DELSUR,ERRR)
              IF(ERRR) RETURN
          END IF
          IF(ALENS(24,R_I).NE.0.0D0) THEN
              FNXP=-DER3X(FUNC3X,R_X,DELSUR,ERRR)
              FNYP=-DER3Y(FUNC3Y,R_Y,DELSUR,ERRR)
              IF(ERRR) RETURN
          END IF
          FNZP=SNGZ
C
C     X AND Y DERIVATIVES OF THE SURFACE HAVE BEEN CALCULATED
C
          MAG=DSQRT((FNXP**2)+(FNYP**2)+(FNZP**2))
          LN=FNXP/MAG
          MN=FNYP/MAG
          NN=FNZP/MAG
          RETURN
      END
C SUB NR3.FOR
      SUBROUTINE NR3(ERRR)
C
          IMPLICIT NONE
C
          INTEGER SPDCD1,SPDCD2
C
          LOGICAL ERRR
C
          COMMON/SPRA2/SPDCD1,SPDCD2
C
C       THIS IS SUBROUTINE NR3.FOR. THIS SUBROUTINE IMPLEMENTS
C       INTERSECTION CALCULATION TO AN ASPHERIC/CONIC TORIC SURFACE
C
          INTEGER I
C
          EXTERNAL FNZ3
C
          EXTERNAL FUNC3X,FUNC3Y,DER1X,DER1Y
C
          REAL*8 C1,C2,C3,C4,ACLENG,DEL,A34,FNZ3
     1    ,FNXP,FNYP,FNZP,HV1,HV2,HV,XP,YP,ZP,SNGX,SNGY,SNGZ,
     2    MAG,ZCAL,QQ,C5,C6,C7,C8,C9,C10,C11,C12,C13,C14,
     3    CY,KY,EY,FY,DY,GY,CX,KX,DX,EX,FX,GX,ZZTOP,DER3X,DER3Y
     2    ,FUNC3X,FUNC3Y,XPASS,YPASS,ZPASS
          REAL*8 ZZEMAX,ZZEMIN
          COMMON/KENMOOR/ZZEMIN,ZZEMAX
          REAL*8 LENG
          COMMON/PASSLENG/LENG
C
          COMMON/SAGPAS/XPASS,YPASS,ZPASS
C
          COMMON/ACLEN/ACLENG
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          INCLUDE 'datmac.inc'
C
          IF(LN.NE.0.0D0) SNGX=LN/DABS(LN)
          IF(MN.NE.0.0D0) SNGY=MN/DABS(MN)
          IF(NN.NE.0.0D0) SNGZ=NN/DABS(NN)
          IF(LN.EQ.0.0D0) SNGX=1.0D0
          IF(MN.EQ.0.0D0) SNGY=1.0D0
          IF(NN.EQ.0.0D0) SNGZ=1.0D0
C
C       NOW REFINE TO INTERSECT WITH THE ASPHERIC
C
C       THE RETURNED VALUES OF X,Y,Z ARE THE CORRECT INTERSECTIONS
C       TO WITHIN THE SURTOL VALUE
C
C

C     AD
          C1=ALENS(4,R_I)
C     AE
          C2=ALENS(5,R_I)
C     AF
          C3=ALENS(6,R_I)
C     AG
          C4=ALENS(7,R_I)
C     CC
          C5=ALENS(2,R_I)
C     CV
          C6=ALENS(1,R_I)
C     TORIC FLAG
          C7=ALENS(23,R_I)
C     CVTOR
          C8=ALENS(24,R_I)
C     TASPH FLAG
          C9=ALENS(36,R_I)
C     ADTOR
          C10=ALENS(37,R_I)
C     AETOR
          C11=ALENS(38,R_I)
C     AFTOR
          C12=ALENS(39,R_I)
C     AGTOR
          C13=ALENS(40,R_I)
C     CCTOR
          C14=ALENS(41,R_I)
C     SPSRF FLAG
          A34=ALENS(34,R_I)
C
C       IF THE SURFACE IS AN Y-TORIC THE BASE CURVATURE (CV)
C       LIES IN THE YZ PLANE. IF,HOWEVER, THE SURFACE IS A
C       Y-TORIC, THE BASE CURVATURE LIES IN THE XZ PLANE.
C
          IF(C7.EQ.1.0D0) THEN
C       YTORIC, DO ASSIGNMENTS
              CY=C6
              KY=C5
              DY=C1
              EY=C2
              FY=C3
              GY=C4
              CX=C8
              KX=C14
              DX=C10
              EX=C11
              FX=C12
              GX=C13
          END IF
          IF(C7.EQ.2.0D0) THEN
C       XTORIC, DO ASSIGNMENTS
              CY=C8
              KY=C14
              DY=C10
              EY=C11
              FY=C12
              GY=C13
              CX=C6
              KX=C5
              DX=C1
              EX=C2
              FX=C3
              GX=C4
C
          END IF
C
C       CALCULATE INTERSECTION
          DO 10 I=1,NRAITR
              QQ=(1.0D0-((KX+1.0D0)*(CX**2)*(R_X**2))
     1        -((KY+1.0D0)*(CY**2)*(R_Y**2)))
              IF(QQ.LT.0.0D0) THEN
                  IF(MSG) THEN
                      WRITE(OUTLYNE,*)'RAY FAILURE OCCURRED AT SURFACE ',R_I
                      CALL SHOWIT(1)
                      OUTLYNE='RAY DID NOT INTERSECT THE SURFACE'
                      CALL SHOWIT(1)
                  END IF
                  RAYCOD(1)=1
                  RAYCOD(2)=R_I
                  SPDCD1=RAYCOD(1)
                  SPDCD2=R_I
                  STOPP=1
                  RETURN
              END IF
              QQ=DSQRT(QQ)
              QQ=1.0D0+QQ
C       CALCULATE CURRENT TANGENT PLANE NORMAL
              DEL=DELSUR
C
C     X AND Y DERIVATIVES OF THE SURFACE ARE CALCULATED HERE:
C     FUNCTION FNZ3 EVALUATES THE FUNCTION
C
              ERRR=.FALSE.
              FNXP=-DER3X(FUNC3X,R_X,DELSUR,ERRR)
              FNYP=-DER3Y(FUNC3Y,R_Y,DELSUR,ERRR)
              IF(ERRR) RETURN
              FNZP=SNGZ
C
C     X AND Y DERIVATIVES OF THE SURFACE HAVE BEEN CALCULATED
C
              MAG=DSQRT((FNXP**2)+(FNYP**2)+(FNZP**2))
              LN=FNXP/MAG
              MN=FNYP/MAG
              NN=FNZP/MAG
C       THE INTERSECTION OF THE RAY WITH THIS TANGENT
C       PLANE
C       USING:
C
              ZCAL=FNZ3(R_X,R_Y,CX,CY,DX,DY,EX,EY,FX,FY,GX,GY,
     1        R_I,A34,QQ)
              HV1=ZCAL-R_Z
              HV2=((R_L*LN)+(R_M*MN)+(R_N*NN))
              HV=HV1/HV2
C       NEW X,Y AND Z ARE
              XP=R_X+(HV*R_L)
              YP=R_Y+(HV*R_M)
              ZP=R_Z+(HV*R_N)
C       SHOULD WE CONTINUE?
              QQ=(1.0D0-((KX+1.0D0)*(CX**2)*(XP**2))
     1        -((KY+1.0D0)*(CY**2)*(YP**2)))
              IF(QQ.LT.0.0D0) THEN
                  IF(MSG) THEN
                      WRITE(OUTLYNE,*)'RAY FAILURE OCCURRED AT SURFACE ',R_I
                      CALL SHOWIT(1)
                      OUTLYNE='RAY DID NOT INTERSECT THE SURFACE'
                      CALL SHOWIT(1)
                  END IF
                  RAYCOD(1)=1
                  RAYCOD(2)=R_I
                  SPDCD1=RAYCOD(1)
                  SPDCD2=R_I
                  STOPP=1
                  RETURN
              END IF
              QQ=DSQRT(QQ)
              QQ=1.0D0+QQ
              ZZTOP=FNZ3(XP,YP,CX,CY,DX,DY,EX,EY,FX,FY,GX,GY,
     1        R_I,A34,QQ)
              LENG=DABS(ZZTOP-ZP)
              ACLENG=LENG
              IF(LENG.LE.SURTOL) THEN
                  R_X=XP
                  R_Y=YP
                  R_Z=ZP
                  QQ=(1.0D0-((KX+1.0D0)*(CX**2)*(R_X**2))
     1            -((KY+1.0D0)*(CY**2)*(R_Y**2)))
                  IF(QQ.LT.0.0D0) THEN
                      IF(MSG) THEN
                          WRITE(OUTLYNE,*)'RAY FAILURE OCCURRED AT SURFACE ',R_I
                          CALL SHOWIT(1)
                          OUTLYNE='RAY DID NOT INTERSECT THE SURFACE'
                          CALL SHOWIT(1)
                      END IF
                      RAYCOD(1)=1
                      RAYCOD(2)=R_I
                      SPDCD1=RAYCOD(1)
                      SPDCD2=R_I
                      STOPP=1
                      RETURN
                  END IF
                  QQ=DSQRT(QQ)
                  QQ=1.0D0+QQ
C       CALCULATE CURRENT TANGENT PLANE NORMAL
C
C     X AND Y DERIVATIVES OF THE SURFACE ARE CALCULATED HERE:
C     FUNCTION FNZ3 EVALUATES THE FUNCTION
C
                  ERRR=.FALSE.
                  FNXP=-DER3X(FUNC3X,R_X,DELSUR,ERRR)
                  FNYP=-DER3Y(FUNC3Y,R_Y,DELSUR,ERRR)
                  IF(ERRR) RETURN
                  FNZP=SNGZ
C
C     X AND Y DERIVATIVES OF THE SURFACE HAVE BEEN CALCULATED
C
                  MAG=DSQRT((FNXP**2)+(FNYP**2)+(FNZP**2))
                  LN=FNXP/MAG
                  MN=FNYP/MAG
                  NN=FNZP/MAG
                  STOPP=0
                  RETURN
              ELSE
C       KEEP TRYING
C       MAKE X Y Z FROM XP YP ZP AND TRY AGAIN
                  R_X=XP
                  R_Y=YP
                  R_Z=ZP
              END IF
 10       CONTINUE
          IF(MSG) THEN
              WRITE(OUTLYNE,*)'RAY FAILURE OCCURRED AT SURFACE ',R_I
              CALL SHOWIT(1)
              OUTLYNE=
     1        'CONVERGENCE FAILURE OCCURRED DURING RAY INTERSECTION'
              CALL SHOWIT(1)
              OUTLYNE='WITH ASPHERIC, TORIC OR SPECIAL SURFACE'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'ACCURACY OF INTERSECTION WAS = ',LENG
              CALL SHOWIT(1)
          END IF
          RAYCOD(1)=2
          RAYCOD(2)=R_I
          SPDCD1=RAYCOD(1)
          SPDCD2=R_I
          STOPP=1
          RETURN
      END
      FUNCTION FNZ3(AX,AY,ACX,ACY,ADX,ADY,AEX,AEY,AFX,AFY,AGX,AGY,
     1AR,A34,Q)
          USE GLOBALS
C
          IMPLICIT NONE
C
          EXTERNAL FF2,FF3,FF4,FF5
C
          REAL*8 FNZ3,AX,AY,ACX,ACY,ADX,ADY,AACX,AACY,AADX
     1    ,AADY,AAX,AAY,FNZZ3,A34,R,THETA,FF2,FF3,AQ,Q,AEX,AEY,
     2    AAEX,AAEY,AAFX,AAFY,AAGX,AAGY,AFX,AFY,AGX,AGY,XX,YY,FF4
     3    ,FF5,AAAX,AAAY,DA34
          REAL*8 ZZEMAX,ZZEMIN
          REAL*8 JK_WAVE
          REAL*8 AMP1,OMEGA1X,OMEGA1Y
          REAL*8 AMP2,OMEGA2X,OMEGA2Y
          REAL*8 AMP3,OMEGA3X,OMEGA3Y
          REAL*8 AMP4,OMEGA4X,OMEGA4Y
          REAL*8 AMP5,OMEGA5X,OMEGA5Y
          COMMON/KENMOOR/ZZEMIN,ZZEMAX
C
          REAL*8 XPASS,YPASS,ZPASS
C
          COMMON/SAGPAS/XPASS,YPASS,ZPASS
C
          INTEGER SPDCD1,SPDCD2
C
          INTEGER IPASS1
C
          COMMON/SPRA2/SPDCD1,SPDCD2
C
          INTEGER KLI,AR,ISURF
C
          LOGICAL FUN,GERROR,GERROR1,GERROR2,UERROR
C
          COMMON/NEFER/KLI
C
          COMMON/COMFUN/FUN
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datmac.inc'
C
          INTEGER I
C
          FNZZ3(AAX,AAY,AACX,AACY,AADX,AADY,AAEX,AAEY,AAFX,AAFY,
     1     AAGX,AAGY,AQ)=
     1            (((AACX*(AAX**2))+(AACY*(AAY**2)))/AQ)
     1      +(AADY*(((1.0D0-AADX)*(AAX**2))+((1.0D0+AADX)*(AAY**2)))**2)
     1      +(AAEY*(((1.0D0-AAEX)*(AAX**2))+((1.0D0+AAEX)*(AAY**2)))**3)
     1      +(AAFY*(((1.0D0-AAFX)*(AAX**2))+((1.0D0+AAFX)*(AAY**2)))**4)
     1      +(AAGY*(((1.0D0-AAGX)*(AAX**2))+((1.0D0+AAGX)*(AAY**2)))**5)
C
C     IF SPECIAL SURFACE TYPE 1, 2 OR 3 ARE TO BE APPLIED
C     THEY ARE APPLIED HERE
C
          DA34=DABS(A34)
          IF(A34.EQ.0.0D0.AND.ALENS(103,AR).EQ.0.0D0.OR.DA34.EQ.6.0D0.OR.
     1    DA34.EQ.7.0D0.OR.DA34.EQ.9.0D0.OR.DA34.EQ.10.0D0.OR.
     1    DA34.EQ.11.0D0.OR.DA34.EQ.12.0D0.OR.
     1    DA34.EQ.15.0D0.OR.DA34.EQ.16.0D0.OR.DA34.EQ.17.0D0.OR.
     1    DA34.EQ.19.0D0.OR.DA34.EQ.20.0D0) THEN
              FNZ3=FNZZ3(AX,AY,ACX,ACY,ADX,ADY,AEX,AEY,AFX,AFY,AGX,AGY,Q)
              RETURN
          END IF
          FNZ3=0.0D0
C     SPECIAL SURFACE TYPE 1
          IF(A34.EQ.1.0D0) THEN
              DO I=9,48
                  IF(FTFL01(I,AR).NE.0.0D0) FNZ3=FNZ3+
     1            (FTFL01(I,AR)*(((DSQRT((AX**2)+(AY**2)))**(I-9))))
              END DO
C     NOW ADD THE REGULAR TERM
              FNZ3=FNZ3+
     1        FNZZ3(AX,AY,ACX,ACY,ADX,ADY,AEX,AEY,AFX,AFY,AGX,AGY,Q)
              RETURN
          END IF
C     SPECIAL SURFACE TYPE 4
          IF(A34.EQ.4.0D0) THEN
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
              AMP1=DABS(FTFL01(1,AR)*JK_WAVE*0.5D0)
              AMP2=DABS(FTFL01(4,AR)*JK_WAVE*0.5D0)
              AMP3=DABS(FTFL01(7,AR)*JK_WAVE*0.5D0)
              AMP4=DABS(FTFL01(10,AR)*JK_WAVE*0.5D0)
              AMP5=DABS(FTFL01(13,AR)*JK_WAVE*0.5D0)
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
              AMP1=FTFL01(1,AR)
              AMP2=FTFL01(4,AR)
              AMP3=FTFL01(7,AR)
              AMP4=FTFL01(10,AR)
              AMP5=FTFL01(13,AR)
              IF(FTFL01(2,AR).EQ.0.0D0) THEN
                  OMEGA1X=0.0D0
              ELSE
                  OMEGA1X=TWOPII/DABS(FTFL01(2,AR))
              END IF
              IF(FTFL01(3,AR).EQ.0.0D0) THEN
                  OMEGA1Y=0.0D0
              ELSE
                  OMEGA1Y=TWOPII/DABS(FTFL01(3,AR))
              END IF
              IF(FTFL01(5,AR).EQ.0.0D0) THEN
                  OMEGA2X=0.0D0
              ELSE
                  OMEGA2X=TWOPII/DABS(FTFL01(5,AR))
              END IF
              IF(FTFL01(6,AR).EQ.0.0D0) THEN
                  OMEGA2Y=0.0D0
              ELSE
                  OMEGA2Y=TWOPII/DABS(FTFL01(6,AR))
              END IF
              IF(FTFL01(8,AR).EQ.0.0D0) THEN
                  OMEGA3X=0.0D0
              ELSE
                  OMEGA3X=TWOPII/DABS(FTFL01(8,AR))
              END IF
              IF(FTFL01(9,AR).EQ.0.0D0) THEN
                  OMEGA3Y=0.0D0
              ELSE
                  OMEGA3Y=TWOPII/DABS(FTFL01(9,AR))
              END IF
              IF(FTFL01(11,AR).EQ.0.0D0) THEN
                  OMEGA4X=0.0D0
              ELSE
                  OMEGA4X=TWOPII/DABS(FTFL01(11,AR))
              END IF
              IF(FTFL01(12,AR).EQ.0.0D0) THEN
                  OMEGA4Y=0.0D0
              ELSE
                  OMEGA4Y=TWOPII/DABS(FTFL01(12,AR))
              END IF
              IF(FTFL01(14,AR).EQ.0.0D0) THEN
                  OMEGA5X=0.0D0
              ELSE
                  OMEGA5X=TWOPII/DABS(FTFL01(14,AR))
              END IF
              IF(FTFL01(15,AR).EQ.0.0D0) THEN
                  OMEGA5Y=0.0D0
              ELSE
                  OMEGA5Y=TWOPII/DABS(FTFL01(15,AR))
              END IF
              FNZ3=(AMP1*(DCOS(OMEGA1X*AX)*(DCOS(OMEGA1Y*AY))))
     1        +(AMP2*(DCOS(OMEGA2X*AX)*(DCOS(OMEGA2Y*AY))))
     1        +(AMP3*(DCOS(OMEGA3X*AX)*(DCOS(OMEGA3Y*AY))))
     1        +(AMP4*(DCOS(OMEGA4X*AX)*(DCOS(OMEGA4Y*AY))))
     1        +(AMP5*(DCOS(OMEGA5X*AX)*(DCOS(OMEGA5Y*AY))))
C     NOW ADD THE REGULAR TERM
              FNZ3=FNZ3+
     1        FNZZ3(AX,AY,ACX,ACY,ADX,ADY,AEX,AEY,AFX,AFY,AGX,AGY,Q)
              RETURN
          END IF
C     SPECIAL SURFACE TYPE 2
          IF(A34.EQ.2.0D0) THEN
              INR=ALENS(76,AR)
              AAAX=AX/INR
              AAAY=AY/INR
              R=DSQRT((AAAX**2)+(AAAY**2))
              IF(DABS(AAAY).GE.DABS(((1.0D35)*AAAX))) THEN
                  IF(AAAY.GE.0.0D0) THETA=PII/2.0D0
                  IF(AAAY.LT.0.0D0) THETA=(3.0D0*PII)/2.0D0
              ELSE
                  IF(DABS(AAAY).EQ.0.0D0.AND.DABS(AAAX).EQ.0.0D0) THEN
                      THETA=0.0D0
                  ELSE
                      THETA=DATAN2(AAAY,AAAX)
                  END IF
                  IF(THETA.LT.0.0D0) THETA=THETA+(TWOPII)
              END IF
              FNZ3=0.0D0
              DO I=1,66
                  IF(FTFL01(I,AR).NE.0.0D0) FNZ3=FNZ3+
     1            (FTFL01(I,AR)*(FF2(R,THETA,I)))
              END DO
C     NOW ADD THE REGULAR TERM
              FNZ3=FNZ3+
     1        FNZZ3(AX,AY,ACX,ACY,ADX,ADY,AEX,AEY,AFX,AFY,AGX,AGY,Q)
              RETURN
          END IF
C     SPECIAL SURFACE TYPE 14
          IF(A34.EQ.14.0D0) THEN
              INR=ALENS(76,AR)
              AAAX=AX/INR
              AAAY=AY/INR
              R=DSQRT((AAAX**2)+(AAAY**2))
              IF(DABS(AAAY).GE.DABS(((1.0D35)*AAAX))) THEN
                  IF(AAAY.GE.0.0D0) THETA=PII/2.0D0
                  IF(AAAY.LT.0.0D0) THETA=(3.0D0*PII)/2.0D0
              ELSE
                  IF(DABS(AAAY).EQ.0.0D0.AND.DABS(AAAX).EQ.0.0D0) THEN
                      THETA=0.0D0
                  ELSE
                      THETA=DATAN2(AAAY,AAAX)
                  END IF
                  IF(THETA.LT.0.0D0) THETA=THETA+(TWOPII)
              END IF
              FNZ3=0.0D0
              DO I=1,48
                  IF(FTFL01(I,AR).NE.0.0D0) FNZ3=FNZ3+
     1            (FTFL01(I,AR)*(FF5(R,THETA,I)))
              END DO
C     NOW ADD THE REGULAR TERM
              FNZ3=FNZ3+
     1        FNZZ3(AX,AY,ACX,ACY,ADX,ADY,AEX,AEY,AFX,AFY,AGX,AGY,Q)
              RETURN
          END IF
C     SPECIAL SURFACE TYPE 3
          IF(A34.EQ.3.0D0) THEN
              INR=ALENS(76,AR)
              AAAX=AX/INR
              AAAY=AY/INR
              R=DSQRT((AAAX**2)+(AAAY**2))
              IF(DABS(AAAY).GE.DABS(((1.0D35)*AAAX))) THEN
                  IF(AAAY.GE.0.0D0) THETA=PII/2.0D0
                  IF(AAAY.LT.0.0D0) THETA=(3.0D0*PII)/2.0D0
              ELSE
                  IF(DABS(AAAY).EQ.0.0D0.AND.DABS(AAAX).EQ.0.0D0) THEN
                      THETA=0.0D0
                  ELSE
                      THETA=DATAN2(AAAY,AAAX)
                  END IF
                  IF(THETA.LT.0.0D0) THETA=THETA+(TWOPII)
              END IF
              FNZ3=0.0D0
              DO I=1,37
                  IF(FTFL01(I,AR).NE.0.0D0) FNZ3=FNZ3+
     1            (FTFL01(I,AR)*(FF3(R,THETA,I)))
              END DO
C     NOW ADD THE REGULAR TERM
              FNZ3=FNZ3+
     1        FNZZ3(AX,AY,ACX,ACY,ADX,ADY,AEX,AEY,AFX,AFY,AGX,AGY,Q)
              RETURN
          END IF
C     SPECIAL SURFACE TYPE 8
          IF(A34.EQ.8.0D0) THEN
              XX=AX
              YY=AY
              FNZ3=0.0D0
              DO I=1,91
                  IF(FTFL01(I,AR).NE.0.0D0) FNZ3=FNZ3+
     1            (FTFL01(I,AR)*(FF4(XX,YY,I)))
              END DO
C     NOW ADD THE REGULAR TERM
              FNZ3=FNZ3+
     1        FNZZ3(AX,AY,ACX,ACY,ADX,ADY,AEX,AEY,AFX,AFY,AGX,AGY,Q)
              RETURN
          END IF
C     SPECIAL SURFACE TYPE 5
C     USER DEFINED SURFACE
          IF(A34.EQ.5.0D0) THEN
              REG(40)=REG(9)
              REG(9)=AX
              REG(10)=AY
              REG(12)= 0.d0 ! was: DBLE(I), but I is undefined (U.G.)
C     THIS SURFACE AUTOMATICALLY USES MACRO FUNCTION FUN10
C     FOR THE INITIAL USER FUCTION TO BE CALLED TO EVALUATE
C     THE Z VALUE CORRESPONDING TO THE AY AND AX VALUES.
C     COEFFICIENTS C1 TO C96 ARE USED BY THE USER IN THE
C     MACRO FUNCTION FUN10 AND ANY MACRO FUNCTION CALLED BY FUN10.
C     THEY ARE STORED INITIALLY IN GENERAL PURPOSE STORAGE REGISTERS
C     NUMBERS 301 TO 396 AND ACCESSED BY THE USER VIA RCL COMMANDS
              DO I=1,96
                 GPREG(300+I) = FTFL01(I,AR)
              END DO

              IF(.NOT.FUNEXT(10)) THEN
C     NO FUN10 EXISTS, RETURN 0.0D0 AS THE Z CONTRIBUTION
                  FNZ3=0.0D0
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
                  FMNW(1:5)=0.0D0
                  FMDF(1:5)=1
                  NESFUN(0:10)=.FALSE.
                  KLI=10
                  FUN=.TRUE.
                  F26=1
                  CALL FUNEXC
                  REST_KDP(1)=RESTINPT(1)
                  F26=0
C     USE WHAT REMAINS IN THE Z-REGISTER
                  FNZ3=REG(11)
              END IF
C     NOW ADD THE REGULAR TERM
              FNZ3=FNZ3+
     1        FNZZ3(AX,AY,ACX,ACY,ADX,ADY,AEX,AEY,AFX,AFY,AGX,AGY,Q)
              RETURN
          END IF
C     SPECIAL SURFACE TYPE 21
C     USER DEFINED SURFACE
          IF(A34.EQ.21.0D0) THEN
              FNZ3=0.0D0
              CALL USERSURF(AR,AX,AY,FNZ3,UERROR)
              IF(UERROR) THEN
                  UERROR=.FALSE.
                  RETURN
              END IF
C     NOW ADD THE REGULAR TERM
              FNZ3=FNZ3+
     1        FNZZ3(AX,AY,ACX,ACY,ADX,ADY,AEX,AEY,AFX,AFY,AGX,AGY,Q)
              RETURN
          END IF
C     SPECIAL SURFACE TYPE 22
C     GRID SAG
          IF(A34.EQ.22.0D0) THEN
C     USE WHAT REMAINS IN THE Z-REGISTER
C     CALL GRID
C     TYPE 22 SAG GRID
              ISURF=AR
              XPASS=AX
              YPASS=AY
              ZPASS=0.0D0
              CALL GRIDS(4,ISURF,GERROR)
              IF(.NOT.GERROR) GRIDSUNLOADED22(ISURF)=.FALSE.
              IF(GERROR) THEN
                  IF(MSG) THEN
                      WRITE(OUTLYNE,*)'RAY FAILURE OCCURRED AT SURFACE ',ISURF
                      CALL SHOWIT(1)
                      OUTLYNE=
     1                'GRID FILE DOES NOT EXIST FOR THIS SURFACE'
                      CALL SHOWIT(1)
                      OUTLYNE=
     1                'OR IT HAS INSUFFICIENT DATA FOR THIS SURFACE'
                      CALL SHOWIT(1)
                  END IF
                  STOPP=1
                  RAYCOD(1)=15
                  RAYCOD(2)=ISURF
                  RAYEXT=.FALSE.
                  POLEXT=.FALSE.
                  SPDCD1=RAYCOD(1)
                  SPDCD2=ISURF
                  RETURN
              END IF
C     NOW ADD THE REGULAR TERM
C     IF(ZPASS.GE.ZZEMAX) ZZEMAX=ZPASS
C     IF(ZPASS.LE.ZZEMIN) ZZEMIN=ZPASS
              FNZ3=ZPASS+
     1        FNZZ3(AX,AY,ACX,ACY,ADX,ADY,AEX,AEY,AFX,AFY,AGX,AGY,Q)
              RETURN
          END IF
C     DEFORM SURFACE
          IF(ALENS(103,AR).EQ.1.0D0) THEN
C     USE WHAT REMAINS IN THE Z-REGISTER
C     CALL GRID
              ISURF=AR
              XPASS=AX
              YPASS=AY
              ZPASS=0.0D0
              IPASS1=4
              CALL DEFGRIDS(4,ISURF,GERROR1,GERROR2)
              IF(GERROR1) THEN
                  IF(MSG) THEN
                      WRITE(OUTLYNE,*)'RAY FAILURE OCCURRED AT SURFACE ',ISURF
                      CALL SHOWIT(1)
                      OUTLYNE=
     1                'DEFORM FILE DOES NOT EXIST FOR THIS SURFACE'
                      CALL SHOWIT(1)
                      OUTLYNE=
     1                'OR IT HAS INSUFFICIENT DATA FOR THIS SURFACE'
                      CALL SHOWIT(1)
                  END IF
                  STOPP=1
                  RAYCOD(1)=17
                  RAYCOD(2)=ISURF
                  RAYEXT=.FALSE.
                  POLEXT=.FALSE.
                  SPDCD1=RAYCOD(1)
                  SPDCD2=ISURF
                  RETURN
              END IF
              IF(GERROR2) THEN
                  IF(MSG) THEN
                      WRITE(OUTLYNE,*)'RAY FAILURE OCCURRED AT SURFACE ',ISURF
                      CALL SHOWIT(1)
                      OUTLYNE=
     1                'RAY COORDINATE BEYOND DEFORMABLE SURFACE BOUNDS'
                      CALL SHOWIT(1)
                  END IF
                  STOPP=1
                  RAYCOD(1)=18
                  RAYCOD(2)=ISURF
                  RAYEXT=.FALSE.
                  POLEXT=.FALSE.
                  SPDCD1=RAYCOD(1)
                  SPDCD2=ISURF
                  RETURN
              END IF
C     NOW ADD THE REGULAR TERM
C     IF(ZPASS.GE.ZZEMAX) ZZEMAX=ZPASS
C     IF(ZPASS.LE.ZZEMIN) ZZEMIN=ZPASS
              FNZ3=ZPASS+
     1        FNZZ3(AX,AY,ACX,ACY,ADX,ADY,AEX,AEY,AFX,AFY,AGX,AGY,Q)
              RETURN
          END IF
          RETURN
      END
      FUNCTION DER1X(FUNC1X,X,DELH,DERERROR)
C
C     SIMPLE X-DERIVATIVE FUNCTION
C
          IMPLICIT NONE
          REAL*8 DER1X,DELH,X,FUNC1X
          LOGICAL DERERROR

          EXTERNAL FUNC1X
C
          DER1X=(FUNC1X(X+DELH)-FUNC1X(X-DELH))/(2.0D0*DELH)
          IF(DABS(DER1X).GT.10.0D0) DERERROR=.TRUE.
          RETURN
      END
      FUNCTION DER1Y(FUNC1Y,Y,DELH,DERERROR)
C
C     SIMPLE Y-DERIVATIVE FUNCTION
C
          IMPLICIT NONE
          REAL*8 DER1Y,DELH,Y,FUNC1Y
          LOGICAL DERERROR

          EXTERNAL FUNC1Y
C
          DER1Y=(FUNC1Y(Y+DELH)-FUNC1Y(Y-DELH))/(2.0D0*DELH)
          IF(DABS(DER1Y).GT.10.0D0) DERERROR=.TRUE.
          RETURN
      END
      FUNCTION FUNC1X(AX)
C
          IMPLICIT NONE
C
          EXTERNAL FNZ1
C
          REAL*8 FUNC1X,AX,FNZ1,C1,C2,C3,C4,C5,A34
     1    ,C41,C42,C43,C44,C45
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datmac.inc'
C
          C1=ALENS(4,R_I)
          C2=ALENS(5,R_I)
          C3=ALENS(6,R_I)
          C4=ALENS(7,R_I)
          C5=ALENS(43,R_I)
          A34=ALENS(34,R_I)
          C41=ALENS(81,R_I)
          C42=ALENS(82,R_I)
          C43=ALENS(83,R_I)
          C44=ALENS(84,R_I)
          C45=ALENS(85,R_I)
          A34=ALENS(34,R_I)
C
          FUNC1X=FNZ1(AX,R_Y,C1,C2,C3,C4,C5,R_I,A34,C41,C42,C43,C44,C45)
C
          RETURN
      END
      FUNCTION FUNC1Y(AY)
C
          IMPLICIT NONE
C
          EXTERNAL FNZ1
C
          REAL*8 FUNC1Y,AY,FNZ1,C1,C2,C3,C4,C5,A34,C41,C42,C43
     1    ,C44,C45
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datmac.inc'
C
          C1=ALENS(4,R_I)
          C2=ALENS(5,R_I)
          C3=ALENS(6,R_I)
          C4=ALENS(7,R_I)
          C5=ALENS(43,R_I)
          C41=ALENS(81,R_I)
          C42=ALENS(82,R_I)
          C43=ALENS(83,R_I)
          C44=ALENS(84,R_I)
          C45=ALENS(85,R_I)
          A34=ALENS(34,R_I)
C
          FUNC1Y=FNZ1(R_X,AY,C1,C2,C3,C4,C5,R_I,A34,C41,C42,C43,C44,C45)
C
          RETURN
      END
C
      FUNCTION DER2X(FUNC2X,X,DELH,DERERROR)
C
C     SIMPLE X-DERIVATIVE FUNCTION
C
          IMPLICIT NONE
          REAL*8 DER2X,DELH,X,FUNC2X
          LOGICAL DERERROR

          EXTERNAL FUNC2X
C
          DER2X=(FUNC2X(X+DELH)-FUNC2X(X-DELH))/(2.0D0*DELH)
          IF(DABS(DER2X).GT.10.0D0) DERERROR=.TRUE.
          RETURN
      END
      FUNCTION DER2Y(FUNC2Y,Y,DELH,DERERROR)
C
C     SIMPLE Y-DERIVATIVE FUNCTION
C
          IMPLICIT NONE
          REAL*8 DER2Y,DELH,Y,FUNC2Y
          LOGICAL DERERROR

          EXTERNAL FUNC2Y
C
          DER2Y=(FUNC2Y(Y+DELH)-FUNC2Y(Y-DELH))/(2.0D0*DELH)
          IF(DABS(DER2Y).GT.10.0D0) DERERROR=.TRUE.
          RETURN
      END
      FUNCTION FUNC2X(AX)
C
          IMPLICIT NONE
C
          EXTERNAL FNZ2
C
          REAL*8 FUNC2X,AX,FNZ2,C1,C2,C3,C4,C5,C6,A34,Q
     1    ,C41,C42,C43,C44,C45
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datmac.inc'
C
          C1=ALENS(4,R_I)
          C2=ALENS(5,R_I)
          C3=ALENS(6,R_I)
          C4=ALENS(7,R_I)
          C5=ALENS(2,R_I)
          C6=(ALENS(1,R_I))
          C41=ALENS(81,R_I)
          C42=ALENS(82,R_I)
          C43=ALENS(83,R_I)
          C44=ALENS(84,R_I)
          C45=ALENS(85,R_I)
          A34=ALENS(34,R_I)
          Q=(1.0D0-((C5+1.0D0)*(C6**2)*
     1    ((AX**2)+(R_Y**2))))
          Q=DSQRT(DABS(Q))
          Q=1.0D0+Q
C
          FUNC2X=FNZ2(AX,R_Y,C1,C2,C3,C4,C6,R_I,A34,Q,C41,C42,C43,
     1    C44,C45)
C
          RETURN
      END
      FUNCTION FUNC2Y(AY)
C
          IMPLICIT NONE
C
          EXTERNAL FNZ2
C
          REAL*8 FUNC2Y,AY,FNZ2,C1,C2,C3,C4,C5,C6,A34,Q
     1    ,C41,C42,C43,C44,C45
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datmac.inc'
C
          C1=ALENS(4,R_I)
          C2=ALENS(5,R_I)
          C3=ALENS(6,R_I)
          C4=ALENS(7,R_I)
          C41=ALENS(81,R_I)
          C42=ALENS(82,R_I)
          C43=ALENS(83,R_I)
          C44=ALENS(84,R_I)
          C45=ALENS(85,R_I)
          A34=ALENS(34,R_I)
          C5=ALENS(2,R_I)
          C6=(ALENS(1,R_I))
          Q=(1.0D0-((C5+1.0D0)*(C6**2)*
     1    ((R_X**2)+(AY**2))))
          Q=DSQRT(DABS(Q))
          Q=1.0D0+Q
C
          FUNC2Y=FNZ2(R_X,AY,C1,C2,C3,C4,C6,R_I,A34,Q,C41,C42,C43,C44,
     1    C45)
C
          RETURN
      END
C
      FUNCTION DER3X(FUNC3X,X,DELH,DERERROR)
C
C     SIMPLE X-DERIVATIVE FUNCTION
C
          IMPLICIT NONE
          REAL*8 DER3X,DELH,X,FUNC3X
          LOGICAL DERERROR

          EXTERNAL FUNC3X
C
          DER3X=(FUNC3X(X+DELH)-FUNC3X(X-DELH))/(2.0D0*DELH)
          IF(DABS(DER3X).GT.10.0D0) DERERROR=.TRUE.
          RETURN
      END
      FUNCTION DER3Y(FUNC3Y,Y,DELH,DERERROR)
C
C     SIMPLE Y-DERIVATIVE FUNCTION
C
          IMPLICIT NONE
          REAL*8 DER3Y,DELH,Y,FUNC3Y
          LOGICAL DERERROR

          EXTERNAL FUNC3Y
C
          DER3Y=(FUNC3Y(Y+DELH)-FUNC3Y(Y-DELH))/(2.0D0*DELH)
          IF(DABS(DER3Y).GT.10.0D0) DERERROR=.TRUE.
          RETURN
      END
      FUNCTION FUNC3X(AX)
C
          IMPLICIT NONE
C
          EXTERNAL FNZ3
C
          REAL*8 FUNC3X,AX,FNZ3,C1,C2,C3,C4,C5,C6,C7,C8,C9
     1    ,C10,C11,C12,C13,C14,A34,CX,CY,KX,KY,DX,DY,EX,EY,FX,FY,GX,GY
     2    ,QQ
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datmac.inc'
          C1=ALENS(4,R_I)
          C2=ALENS(5,R_I)
          C3=ALENS(6,R_I)
          C4=ALENS(7,R_I)
          C5=ALENS(2,R_I)
          C6=ALENS(1,R_I)
          C7=ALENS(23,R_I)
          C8=ALENS(24,R_I)
          C9=ALENS(36,R_I)
          C10=ALENS(37,R_I)
          C11=ALENS(38,R_I)
          C12=ALENS(39,R_I)
          C13=ALENS(40,R_I)
          C14=ALENS(41,R_I)
          A34=ALENS(34,R_I)
          IF(C7.EQ.1.0D0) THEN
C       YTORIC, DO ASSIGNMENTS
              CY=C6
              KY=C5
              DY=C1
              EY=C2
              FY=C3
              GY=C4
              CX=C8
              KX=C14
              DX=C10
              EX=C11
              FX=C12
              GX=C13
          END IF
          IF(C7.EQ.2.0D0) THEN
C       XTORIC, DO ASSIGNMENTS
              CY=C8
              KY=C14
              DY=C10
              EY=C11
              FY=C12
              GY=C13
              CX=C6
              KX=C5
              DX=C1
              EX=C2
              FX=C3
              GX=C4
          END IF
          QQ=(1.0D0-((KX+1.0D0)*(CX**2)*(AX**2))
     1    -((KY+1.0D0)*(CY**2)*(R_Y**2)))
          QQ=DSQRT(DABS(QQ))
          QQ=1.0D0+QQ
C
          FUNC3X=FNZ3(AX,R_Y,CX,CY,DX,DY,EX,EY,FX,FY,GX,GY,R_I,A34,QQ)
C
          RETURN
      END
      FUNCTION FUNC3Y(AY)
C
          IMPLICIT NONE
C
          EXTERNAL FNZ3
C
          REAL*8 FUNC3Y,AY,FNZ3,C1,C2,C3,C4,C5,C6,C7,C8,C9
     1    ,C10,C11,C12,C13,C14,A34,CX,CY,KX,KY,DX,DY,EX,EY,FX,FY,GX,GY
     2    ,QQ
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datmac.inc'
          C1=ALENS(4,R_I)
          C2=ALENS(5,R_I)
          C3=ALENS(6,R_I)
          C4=ALENS(7,R_I)
          C5=ALENS(2,R_I)
          C6=ALENS(1,R_I)
          C7=ALENS(23,R_I)
          C8=ALENS(24,R_I)
          C9=ALENS(36,R_I)
          C10=ALENS(37,R_I)
          C11=ALENS(38,R_I)
          C12=ALENS(39,R_I)
          C13=ALENS(40,R_I)
          C14=ALENS(41,R_I)
          A34=ALENS(34,R_I)
          IF(C7.EQ.1.0D0) THEN
C       YTORIC, DO ASSIGNMENTS
              CY=C6
              KY=C5
              DY=C1
              EY=C2
              FY=C3
              GY=C4
              CX=C8
              KX=C14
              DX=C10
              EX=C11
              FX=C12
              GX=C13
          END IF
          IF(C7.EQ.2.0D0) THEN
C       XTORIC, DO ASSIGNMENTS
              CY=C8
              KY=C14
              DY=C10
              EY=C11
              FY=C12
              GY=C13
              CX=C6
              KX=C5
              DX=C1
              EX=C2
              FX=C3
              GX=C4
          END IF
          QQ=(1.0D0-((KX+1.0D0)*(CX**2)*(R_X**2))
     1    -((KY+1.0D0)*(CY**2)*(AY**2)))
          QQ=DSQRT(DABS(QQ))
          QQ=1.0D0+QQ
C
          FUNC3Y=FNZ3(R_X,AY,CX,CY,DX,DY,EX,EY,FX,FY,GX,GY,R_I,A34,QQ)
C
          RETURN
      END
C SUB NR5.FOR
      SUBROUTINE NR5
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE NR5.FOR. THIS SUBROUTINE IMPLEMENTS
C       INTERSECTION CALCULATION TO A CONIC SURFACE WITH FOURIER-
C       LEGENDRE COEFFICIENTS
C
          REAL*8 MAG,H,RHO,Z3,ZM
     1    ,X,Y,Z,L,M,N,XO,YO,ZO,LO,MO,NO,ZETA,Z1,Z2,CEE
C
          COMMON/FUNN5/XO,YO,ZO,LO,MO,NO
C
          COMMON/HFIND/H
C
          COMMON/NORMER/X,Y,Z,L,M,N
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          INCLUDE 'datmac.inc'
C
          CEE=ALENS(1,R_I)
          IF(R_Z.LT.0.0D0.AND.CEE.GT.0.0D0.OR.
     1    R_Z.GT.0.0D0.AND.CEE.LT.0.0D0) RETURN
          Z1=FTFL01(1,R_I)
          Z2=FTFL01(2,R_I)
          Z3=FTFL01(3,R_I)
          ZM=R_Z
          ZETA=(2.0D0*(ZM-Z3))/(Z2-Z1)
          RHO=DSQRT((R_X**2)+(R_Y**2))
          IF(DABS(ZETA).GT.1.5D0.OR.RHO.EQ.0.0D0) RETURN
C     INITIAL GUESS AT H IS 0
          H=0
          XO=R_X
          YO=R_Y
          ZO=R_Z
          LO=R_L
          MO=R_M
          NO=R_N
C
C     FIND THE NEW H VALUE WHICH MAKES F(H)=0
C
          CALL FINDH
          IF(STOPP.EQ.1) RETURN
C
C     H IS THE DISTANCE TRAVELED FROM THE CONIC INTERSECTION
C     TO THE DEFORMED CONIS INTERSECTION. IT IS A SMALL NUMBER!
C
C     THE RAY COORDINATES OF THE SURFACE INTERSECTION ARE NOW JUST:
          R_X=R_X+(H*R_L)
          R_Y=R_Y+(H*R_M)
          R_Z=R_Z+(H*R_N)
C
C     NOW CALCULATE THE DIRCTION COSINES OF THE SURFACE NORMAL
C
          X=R_X
          Y=R_Y
          Z=R_Z
          CALL FINDNORM
          LN=L
          MN=M
          NN=N
          MAG=DSQRT((LN**2)+(MN**2)+(NN**2))
          LN=LN/MAG
          MN=MN/MAG
          NN=NN/MAG
          RETURN
      END
C
      FUNCTION FUNC()
C     THIS IS THE SURFACE FUNCTION
          IMPLICIT NONE
          REAL*8 FUNC,H,X,Y,Z,KAPPA,CEE,RHO,LO,MO,NO,Z3,ZM
     1    ,XO,YO,ZO,THETA,ZETA,Z1,Z2,FL,FL1,FL2,FL3,FL4,FL5,FL6,
     2    FL7,FL8,FL9,FL10,FL11,FL12,FL13,FL14,FL15,FLT,FLZEE
     3    ,FLX,FLY
          EXTERNAL FL
          INTEGER FLII
          COMMON/FUNCER/H
          COMMON/FUNN5/XO,YO,ZO,LO,MO,NO
          COMMON/JIM/FLZEE,FLT,FLII,FLX,FLY
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
C
          X=XO+(H*LO)
          Y=YO+(H*MO)
          Z=ZO+(H*NO)
C
          KAPPA=ALENS(2,R_I)
          CEE=ALENS(1,R_I)
          IF(Z.LT.0.0D0.AND.CEE.GT.0.0D0) THEN
              Z=0.0D0
              H=-ZO/NO
          END IF
          IF(Z.GT.0.0D0.AND.CEE.LT.0.0D0) THEN
              Z=0.0D0
              H=-ZO/NO
          END IF
          Z1=FTFL01(1,R_I)
          Z2=FTFL01(2,R_I)
          Z3=FTFL01(3,R_I)
          ZM=Z
          ZETA=(2.0D0*(ZM-Z3))/(Z2-Z1)
          RHO=DSQRT((X**2)+(Y**2))
          IF(DABS(Y).GE.DABS(((1.0D35)*(X)))) THEN
              IF(Y.GE.0.0D0) THETA=PII/2.0D0
              IF(Y.LT.0.0D0) THETA=(3.0D0*PII)/2.0D0
          ELSE
              IF(DABS(Y).EQ.0.0D0.AND.DABS(X).EQ.0.0D0) THEN
                  THETA=0.0D0
              ELSE
                  THETA=DATAN2(Y,X)
              END IF
              IF(THETA.LT.0.0D0) THETA=THETA+(TWOPII)
          END IF
          FLZEE=ZETA
          FLT=THETA
          FLII=1
          FL1=FL()
          FLII=2
          FL2=FL()
          FLII=3
          FL3=FL()
          FLII=4
          FL4=FL()
          FLII=5
          FL5=FL()
          FLII=6
          FL6=FL()
          FLII=7
          FL7=FL()
          FLII=8
          FL8=FL()
          FLII=9
          FL9=FL()
          FLII=10
          FL10=FL()
          FLII=11
          FL11=FL()
          FLII=12
          FL12=FL()
          FLII=13
          FL13=FL()
          FLII=14
          FL14=FL()
          FLII=15
          FL15=FL()
          FUNC=-RHO+DSQRT(((2.0D0*Z)/CEE)-((Z**2)*(KAPPA+1.0D0)))
     1    +(FTFL01(4,R_I)*FL1)
     1    +(FTFL01(5,R_I)*FL2)
     1    +(FTFL01(6,R_I)*FL3)
     1    +(FTFL01(7,R_I)*FL4)
     1    +(FTFL01(8,R_I)*FL5)
     1    +(FTFL01(9,R_I)*FL6)
     1    +(FTFL01(10,R_I)*FL7)
     1    +(FTFL01(11,R_I)*FL8)
     1    +(FTFL01(12,R_I)*FL9)
     1    +(FTFL01(13,R_I)*FL10)
     1    +(FTFL01(14,R_I)*FL11)
     1    +(FTFL01(15,R_I)*FL12)
     1    +(FTFL01(16,R_I)*FL13)
     1    +(FTFL01(17,R_I)*FL14)
     1    +(FTFL01(18,R_I)*FL15)
          RETURN
      END
C
      SUBROUTINE FINDNORM
          IMPLICIT NONE
          REAL*8 L,M,N,KAPPA,X,Y,Z,CEE
     1    ,RHO,Z1,Z2,THETA,ZETA,FLX,FLY,FLZ,FLZEE,FLXX,FLYY
          EXTERNAL FLX,FLY,FLZ
          REAL*8 FL1,FL2,FL3,FL4,FL5,FL6,FL7,FLT
     1    ,FL8,FL9,FL10,FL11,FL12,FL13,FL14,FL15,ZETAD,Z3,ZM
          INTEGER FLII
          COMMON/NORMER/X,Y,Z,L,M,N
          COMMON/JIM/FLZEE,FLT,FLII,FLXX,FLYY
          COMMON/ZEETAA/ZETAD
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
          CEE=ALENS(1,R_I)
          KAPPA=ALENS(2,R_I)
          RHO=DSQRT((X**2)+(Y**2))
          Z1=FTFL01(1,R_I)
          Z2=FTFL01(2,R_I)
          Z3=FTFL01(3,R_I)
          ZM=Z
          ZETA=(2.0D0*(ZM-Z3))/(Z2-Z1)
          ZETAD=(Z2-Z1)
          IF(DABS(Y).GE.DABS(((1.0D35)*(X)))) THEN
              IF(Y.GE.0.0D0) THETA=PII/2.0D0
              IF(Y.LT.0.0D0) THETA=(3.0D0*PII)/2.0D0
          ELSE
              IF(DABS(Y).EQ.0.0D0.AND.DABS(X).EQ.0.0D0) THEN
                  THETA=0.0D0
              ELSE
                  THETA=DATAN2(Y,X)
              END IF
              IF(THETA.LT.0.0D0) THETA=THETA+(TWOPII)
          END IF
          FLZEE=ZETA
          FLT=THETA
          FLXX=X
          FLYY=Y
          FLII=1
          FL1=FLX()
          FLII=2
          FL2=FLX()
          FLII=3
          FL3=FLX()
          FLII=4
          FL4=FLX()
          FLII=5
          FL5=FLX()
          FLII=6
          FL6=FLX()
          FLII=7
          FL7=FLX()
          FLII=8
          FL8=FLX()
          FLII=9
          FL9=FLX()
          FLII=10
          FL10=FLX()
          FLII=11
          FL11=FLX()
          FLII=12
          FL12=FLX()
          FLII=13
          FL13=FLX()
          FLII=14
          FL14=FLX()
          FLII=15
          FL15=FLX()
          L=LN
     1    -(FTFL01(4,R_I)*FL1)
     1    -(FTFL01(5,R_I)*FL2)
     1    -(FTFL01(6,R_I)*FL3)
     1    -(FTFL01(7,R_I)*FL4)
     1    -(FTFL01(8,R_I)*FL5)
     1    -(FTFL01(9,R_I)*FL6)
     1    -(FTFL01(10,R_I)*FL7)
     1    -(FTFL01(11,R_I)*FL8)
     1    -(FTFL01(12,R_I)*FL9)
     1    -(FTFL01(13,R_I)*FL10)
     1    -(FTFL01(14,R_I)*FL11)
     1    -(FTFL01(15,R_I)*FL12)
     1    -(FTFL01(16,R_I)*FL13)
     1    -(FTFL01(17,R_I)*FL14)
     1    -(FTFL01(18,R_I)*FL15)
          FLII=1
          FL1=FLY()
          FLII=2
          FL2=FLY()
          FLII=3
          FL3=FLY()
          FLII=4
          FL4=FLY()
          FLII=5
          FL5=FLY()
          FLII=6
          FL6=FLY()
          FLII=7
          FL7=FLY()
          FLII=8
          FL8=FLY()
          FLII=9
          FL9=FLY()
          FLII=10
          FL10=FLY()
          FLII=11
          FL11=FLY()
          FLII=12
          FL12=FLY()
          FLII=13
          FL13=FLY()
          FLII=14
          FL14=FLY()
          FLII=15
          FL15=FLY()
          M=MN
     1    -(FTFL01(4,R_I)*FL1)
     1    -(FTFL01(5,R_I)*FL2)
     1    -(FTFL01(6,R_I)*FL3)
     1    -(FTFL01(7,R_I)*FL4)
     1    -(FTFL01(8,R_I)*FL5)
     1    -(FTFL01(9,R_I)*FL6)
     1    -(FTFL01(10,R_I)*FL7)
     1    -(FTFL01(11,R_I)*FL8)
     1    -(FTFL01(12,R_I)*FL9)
     1    -(FTFL01(13,R_I)*FL10)
     1    -(FTFL01(14,R_I)*FL11)
     1    -(FTFL01(15,R_I)*FL12)
     1    -(FTFL01(16,R_I)*FL13)
     1    -(FTFL01(17,R_I)*FL14)
     1    -(FTFL01(18,R_I)*FL15)
          FLII=1
          FL1=FLZ()
          FLII=2
          FL2=FLZ()
          FLII=3
          FL3=FLZ()
          FLII=4
          FL4=FLZ()
          FLII=5
          FL5=FLZ()
          FLII=6
          FL6=FLZ()
          FLII=7
          FL7=FLZ()
          FLII=8
          FL8=FLZ()
          FLII=9
          FL9=FLZ()
          FLII=10
          FL10=FLZ()
          FLII=11
          FL11=FLZ()
          FLII=12
          FL12=FLZ()
          FLII=13
          FL13=FLZ()
          FLII=14
          FL14=FLZ()
          FLII=15
          FL15=FLZ()
          N=NN
     1    -(FTFL01(4,R_I)*FL1)
     1    -(FTFL01(5,R_I)*FL2)
     1    -(FTFL01(6,R_I)*FL3)
     1    -(FTFL01(7,R_I)*FL4)
     1    -(FTFL01(8,R_I)*FL5)
     1    -(FTFL01(9,R_I)*FL6)
     1    -(FTFL01(10,R_I)*FL7)
     1    -(FTFL01(11,R_I)*FL8)
     1    -(FTFL01(12,R_I)*FL9)
     1    -(FTFL01(13,R_I)*FL10)
     1    -(FTFL01(14,R_I)*FL11)
     1    -(FTFL01(15,R_I)*FL12)
     1    -(FTFL01(16,R_I)*FL13)
     1    -(FTFL01(17,R_I)*FL14)
     1    -(FTFL01(18,R_I)*FL15)
C     NORMALS HAVE SAME SIGN AS THEY DID FOR JUST THE CONIC
C     INTERSECTION
C
          RETURN
      END


      SUBROUTINE FINDH
          IMPLICIT NONE
          REAL*8 XO,YO,ZO,LO,MO,NO,H,ACC,RTSEC,H1,H2
          COMMON/FUNN5/XO,YO,ZO,LO,MO,NO
          COMMON/HFIND/H
          EXTERNAL RTSEC
          COMMON/SECRTER/H1,H2,ACC
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
          H1=(FTFL01(1,R_I)-ZO)/NO
          H2=(FTFL01(2,R_I)-ZO)/NO
          ACC=1.0D-12
          IF(DABS(SURTOL).LE.1.0D-10) ACC=SURTOL
          H=RTSEC()
          RETURN
      END


      FUNCTION rtsec()
          IMPLICIT NONE
          INTEGER MAXIT
          REAL*8 rtsec,x1,x2,xacc,func
          EXTERNAL func
          INTEGER j
          COMMON/SECRTER/X1,X2,XACC
C
          INTEGER SPDCD1,SPDCD2
C
          COMMON/SPRA2/SPDCD1,SPDCD2
          REAL*8 dx,f,fl,swap,xl,H
          COMMON/FUNCER/H
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
          MAXIT=NRAITR
          H=X1
          fl=func()
          X1=H
          H=X2
          f=func()
          X2=H
          if(dabs(fl).lt.dabs(f))then
              rtsec=x1
              xl=x2
              swap=fl
              fl=f
              f=swap
          else
              xl=x1
              rtsec=x2
          endif
          do j=1,MAXIT
              dx=(xl-rtsec)*f/(f-fl)
              xl=rtsec
              fl=f
              rtsec=rtsec+dx
              H=RTSEC
              f=func()
              RTSEC=H
              if(dabs(dx).lt.xacc.or.f.eq.0.0d0)return
          end do
          IF(MSG) THEN
              WRITE(OUTLYNE,*)'RAY FAILURE OCCURRED AT SURFACE ',R_I
              CALL SHOWIT(1)
              OUTLYNE=
     1        'CONVERGENCE FAILURE OCCURRED DURING RAY INTERSECTION'
              CALL SHOWIT(1)
              OUTLYNE='WITH DEFORMED GRAZING INCIDENCE SURFACE'
              CALL SHOWIT(1)
          END IF
          RAYCOD(1)=2
          RAYCOD(2)=R_I
          SPDCD1=RAYCOD(1)
          SPDCD2=R_I
          STOPP=1
          RETURN
      END


C SUB FL.FOR
      FUNCTION FL()
C
          IMPLICIT NONE
C
          REAL*8 FL1,FL2,FL3,FL4,FL5,FL6,FL7,
     1    FL8,FL9,FL10,FL11,FL12,FL13,FL14,FL15
     5    ,ZETA,THETA,FL,Z,A,FLXX,FLYY
C
          INTEGER FLII
C
          COMMON/JIM/ZETA,THETA,FLII,FLXX,FLYY
C
C       FUNCTIONS FOR TYPE 18 SURFACE, FOURIER-LEGENDRE
          FL1(Z,A)=1.0D0
          FL2(Z,A)=DSQRT(3.0D0)*Z
          FL3(Z,A)=DSQRT(5.0D0)*(((3.0D0*(Z**2))-1.0D0)/2.0D0)
          FL4(Z,A)=DSQRT(2.0D0)*DCOS(A)
          FL5(Z,A)=DSQRT(2.0D0)*DSIN(A)
          FL6(Z,A)=DSQRT(6.0D0)*Z*DCOS(A)
          FL7(Z,A)=DSQRT(6.0D0)*Z*DSIN(A)
          FL8(Z,A)=DSQRT(10.0D0)*DCOS(A)*(((3.0D0*(Z**2))-1.0D0)/2.0D0)
          FL9(Z,A)=DSQRT(10.0D0)*DSIN(A)*(((3.0D0*(Z**2))-1.0D0)/2.0D0)
          FL10(Z,A)=DSQRT(2.0D0)*DCOS(2.0D0*A)
          FL11(Z,A)=DSQRT(2.0D0)*DSIN(2.0D0*A)
          FL12(Z,A)=DSQRT(6.0D0)*DCOS(2.0D0*A)*Z
          FL13(Z,A)=DSQRT(6.0D0)*DSIN(2.0D0*A)*Z
          FL14(Z,A)=DSQRT(10.0D0)*DCOS(2.0D0*A)*
     1    (((3.0D0*(Z**2))-1.0D0)/2.0D0)
          FL15(Z,A)=DSQRT(10.0D0)*DSIN(2.0D0*A)*
     1    (((3.0D0*(Z**2))-1.0D0)/2.0D0)
C

          IF(FLII.EQ.1) FL=FL1(ZETA,THETA)
          IF(FLII.EQ.2) FL=FL2(ZETA,THETA)
          IF(FLII.EQ.3) FL=FL3(ZETA,THETA)
          IF(FLII.EQ.4) FL=FL4(ZETA,THETA)
          IF(FLII.EQ.5) FL=FL5(ZETA,THETA)
          IF(FLII.EQ.6) FL=FL6(ZETA,THETA)
          IF(FLII.EQ.7) FL=FL7(ZETA,THETA)
          IF(FLII.EQ.8) FL=FL8(ZETA,THETA)
          IF(FLII.EQ.9) FL=FL9(ZETA,THETA)
          IF(FLII.EQ.10) FL=FL10(ZETA,THETA)
          IF(FLII.EQ.11) FL=FL11(ZETA,THETA)
          IF(FLII.EQ.12) FL=FL12(ZETA,THETA)
          IF(FLII.EQ.13) FL=FL13(ZETA,THETA)
          IF(FLII.EQ.14) FL=FL14(ZETA,THETA)
          IF(FLII.EQ.15) FL=FL15(ZETA,THETA)
          RETURN
      END


C SUB FLX.FOR
      FUNCTION FLX()
C
          IMPLICIT NONE
C
          REAL*8 FL1,FL2,FL3,FL4,FL5,FL6,FL7,
     1    FL8,FL9,FL10,FL11,FL12,FL13,FL14,FL15,CX,SX,
     2    A,AP,THETA,ZETA,X,Y,FLX,Z,AX,AY,P
C
          INTEGER FLII
C
          COMMON/JIM/ZETA,THETA,FLII,X,Y
          common Z,A
C
C       FUNCTIONS FOR TYPE 18 SURFACE, X-DERIVATIVE, FOURIER-LEGENDRE
          CX(AX,AY,AP)=(1.0D0/AP)-((AX**2)/(AP**3))
          SX(AX,AY,AP)=-(AX*AY)/(AP**3)
          FL1(Z,A)=0.0D0
          FL2(Z,A)=0.0D0
          FL3(Z,A)=0.0D0
          FL4(Z,A)=DSQRT(2.0D0)
          FL5(Z,A)=DSQRT(2.0D0)
          FL6(Z,A)=DSQRT(6.0D0)*Z
          FL7(Z,A)=DSQRT(6.0D0)*Z
          FL8(Z,A)=DSQRT(10.0D0)*(((3.0D0*(Z**2))-1.0D0)/2.0D0)
          FL9(Z,A)=DSQRT(10.0D0)*(((3.0D0*(Z**2))-1.0D0)/2.0D0)
          FL10(Z,A)=DSQRT(2.0D0)
          FL11(Z,A)=DSQRT(2.0D0)
          FL12(Z,A)=DSQRT(6.0D0)*Z
          FL13(Z,A)=DSQRT(6.0D0)*Z
          FL14(Z,A)=DSQRT(10.0D0)*
     1    (((3.0D0*(Z**2))-1.0D0)/2.0D0)
          FL15(Z,A)=DSQRT(10.0D0)*
     1    (((3.0D0*(Z**2))-1.0D0)/2.0D0)
C
          P=DSQRT((X**2)+(Y**2))
C
          IF(P.EQ.0.0D0) then
              FLX=0.0D0
              RETURN
          end if
          IF(FLII.EQ.1)  FLX=0.0D0
          IF(FLII.EQ.2)  FLX=0.0D0
          IF(FLII.EQ.3)  FLX=0.0D0
          IF(FLII.EQ.4)  FLX=FL4(ZETA,THETA)*CX(X,Y,P)
          IF(FLII.EQ.5)  FLX=FL5(ZETA,THETA)*SX(X,Y,P)
          IF(FLII.EQ.6)  FLX=FL6(ZETA,THETA)*CX(X,Y,P)
          IF(FLII.EQ.7)  FLX=FL7(ZETA,THETA)*SX(X,Y,P)
          IF(FLII.EQ.8)  FLX=FL8(ZETA,THETA)*CX(X,Y,P)
          IF(FLII.EQ.9)  FLX=FL9(ZETA,THETA)*SX(X,Y,P)
          IF(FLII.EQ.10) FLX=FL10(ZETA,THETA)*4.0D0*DCOS(THETA)*CX(X,Y,P)
          IF(FLII.EQ.11) FLX=FL11(ZETA,THETA)*
     1    ((2.0D0*CX(X,Y,P)*DSIN(THETA))+(2.0D0*SX(X,Y,P)*DCOS(THETA)))
          IF(FLII.EQ.12) FLX=FL12(ZETA,THETA)*4.0D0*DCOS(THETA)*CX(X,Y,P)
          IF(FLII.EQ.13) FLX=FL13(ZETA,THETA)*
     1    ((2.0D0*CX(X,Y,P)*DSIN(THETA))+(2.0D0*SX(X,Y,P)*DCOS(THETA)))
          IF(FLII.EQ.14) FLX=FL14(ZETA,THETA)*4.0D0*DCOS(THETA)*CX(X,Y,P)
          IF(FLII.EQ.15) FLX=FL15(ZETA,THETA)*
     1    ((2.0D0*CX(X,Y,P)*DSIN(THETA))+(2.0D0*SX(X,Y,P)*DCOS(THETA)))
          RETURN
      END


C SUB FLY.FOR
      FUNCTION FLY()
C
          IMPLICIT NONE
C
          REAL*8 FL1,FL2,FL3,FL4,FL5,FL6,FL7,
     1    FL8,FL9,FL10,FL11,FL12,FL13,FL14,FL15,CY,SY,FLY,
     2    AX,AY,AP,Z,A,ZETA,THETA,X,Y,P
C
          INTEGER FLII

          COMMON/JIM/ZETA,THETA,FLII,X,Y
          common Z,A
C
C       FUNCTIONS FOR TYPE 18 SURFACE, Y-DERIVATIVE, FOURIER-LEGENDRE
          CY(AX,AY,AP)=-(AY*AX)/(AP**3)
          SY(AX,AY,AP)=(1.0D0/AP)-((AY**2)/(AP**3))
          FL1(Z,A)=0.0D0
          FL2(Z,A)=0.0D0
          FL3(Z,A)=0.0D0
          FL4(Z,A)=DSQRT(2.0D0)
          FL5(Z,A)=DSQRT(2.0D0)
          FL6(Z,A)=DSQRT(6.0D0)*Z
          FL7(Z,A)=DSQRT(6.0D0)*Z
          FL8(Z,A)=DSQRT(10.0D0)*(((3.0D0*(Z**2))-1.0D0)/2.0D0)
          FL9(Z,A)=DSQRT(10.0D0)*(((3.0D0*(Z**2))-1.0D0)/2.0D0)
          FL10(Z,A)=DSQRT(2.0D0)
          FL11(Z,A)=DSQRT(2.0D0)
          FL12(Z,A)=DSQRT(6.0D0)*Z
          FL13(Z,A)=DSQRT(6.0D0)*Z
          FL14(Z,A)=DSQRT(10.0D0)*
     1    (((3.0D0*(Z**2))-1.0D0)/2.0D0)
          FL15(Z,A)=DSQRT(10.0D0)*
     1    (((3.0D0*(Z**2))-1.0D0)/2.0D0)
C
          P=DSQRT((X**2)+(Y**2))
C
          IF(P.EQ.0.0D0) THEN
              FLY=0
              RETURN
          END IF
          IF(FLII.EQ.1)  FLY=0.0D0
          IF(FLII.EQ.2)  FLY=0.0D0
          IF(FLII.EQ.3)  FLY=0.0D0
          IF(FLII.EQ.4)  FLY=FL4(ZETA,THETA)*CY(X,Y,P)
          IF(FLII.EQ.5)  FLY=FL5(ZETA,THETA)*SY(X,Y,P)
          IF(FLII.EQ.6)  FLY=FL6(ZETA,THETA)*CY(X,Y,P)
          IF(FLII.EQ.7)  FLY=FL7(ZETA,THETA)*SY(X,Y,P)
          IF(FLII.EQ.8)  FLY=FL8(ZETA,THETA)*CY(X,Y,P)
          IF(FLII.EQ.9)  FLY=FL9(ZETA,THETA)*SY(X,Y,P)
          IF(FLII.EQ.10) FLY=FL10(ZETA,THETA)*4.0D0*DCOS(THETA)*CY(X,Y,P)
          IF(FLII.EQ.11) FLY=FL11(ZETA,THETA)*
     1    ((2.0D0*CY(X,Y,P)*DSIN(THETA))+(2.0D0*SY(X,Y,P)*DCOS(THETA)))
          IF(FLII.EQ.12) FLY=FL12(ZETA,THETA)*4.0D0*DCOS(THETA)*CY(X,Y,P)
          IF(FLII.EQ.13) FLY=FL13(ZETA,THETA)*
     1    ((2.0D0*CY(X,Y,P)*DSIN(THETA))+(2.0D0*SY(X,Y,P)*DCOS(THETA)))
          IF(FLII.EQ.14) FLY=FL14(ZETA,THETA)*4.0D0*DCOS(THETA)*CY(X,Y,P)
          IF(FLII.EQ.15) FLY=FL15(ZETA,THETA)*
     1    ((2.0D0*CY(X,Y,P)*DSIN(THETA))+(2.0D0*SY(X,Y,P)*DCOS(THETA)))
          RETURN
      END


C SUB FLZ.FOR
      FUNCTION FLZ()
C
          IMPLICIT NONE
C
          REAL*8 FL1,FL2,FL3,FL4,FL5,FL6,FL7,
     1    FL8,FL9,FL10,FL11,FL12,FL13,FL14,FL15
     5    ,ZETA,THETA,FLZ,Z,A,X,Y,P,ZETAD
C
          COMMON/ZEETAA/ZETAD

          COMMON/JIM/ZETA,THETA,FLII,X,Y
C
          INTEGER FLII
C
C       FUNCTIONS FOR TYPE 18 SURFACE, Z-DERIVATIVE, FOURIER-LEGENDRE
          FL1(Z,A)=0.0D0
          FL2(Z,A)=DSQRT(3.0D0)
          FL3(Z,A)=DSQRT(5.0D0)*(((6.0D0*(Z)))/2.0D0)
          FL4(Z,A)=0.0D0
          FL5(Z,A)=0.0D0
          FL6(Z,A)=DSQRT(6.0D0)*DCOS(A)
          FL7(Z,A)=DSQRT(6.0D0)*DSIN(A)
          FL8(Z,A)=DSQRT(10.0D0)*DCOS(A)*(((6.0D0*(Z)))/2.0D0)
          FL9(Z,A)=DSQRT(10.0D0)*DSIN(A)*(((6.0D0*(Z)))/2.0D0)
          FL10(Z,A)=0.0D0
          FL11(Z,A)=0.0D0
          FL12(Z,A)=DSQRT(6.0D0)*DCOS(2.0D0*A)
          FL13(Z,A)=DSQRT(6.0D0)*DSIN(2.0D0*A)
          FL14(Z,A)=DSQRT(10.0D0)*DCOS(2.0D0*A)*
     1    (((6.0D0*(Z)))/2.0D0)
          FL15(Z,A)=DSQRT(10.0D0)*DSIN(2.0D0*A)*
     1    (((6.0D0*(Z)))/2.0D0)
C
          P=DSQRT((X**2)+(Y**2))
          IF(P.EQ.0.0D0) then
              FLZ=0.0D0
              RETURN
          end if
          IF(FLII.EQ.1) FLZ=0.0D0
          IF(FLII.EQ.2) FLZ=(FL2(ZETA,THETA)*2.0D0)/ZETAD
          IF(FLII.EQ.3) FLZ=(FL3(ZETA,THETA)*2.0D0)/ZETAD
          IF(FLII.EQ.4) FLZ=0.0D0
          IF(FLII.EQ.5) FLZ=0.0D0
          IF(FLII.EQ.6) FLZ=(FL6(ZETA,THETA)*2.0D0)/ZETAD
          IF(FLII.EQ.7) FLZ=(FL7(ZETA,THETA)*2.0D0)/ZETAD
          IF(FLII.EQ.8) FLZ=(FL8(ZETA,THETA)*2.0D0)/ZETAD
          IF(FLII.EQ.9) FLZ=(FL9(ZETA,THETA)*2.0D0)/ZETAD
          IF(FLII.EQ.10) FLZ=0.0D0
          IF(FLII.EQ.11) FLZ=0.0D0
          IF(FLII.EQ.12) FLZ=(FL12(ZETA,THETA)*2.0D0)/ZETAD
          IF(FLII.EQ.13) FLZ=(FL13(ZETA,THETA)*2.0D0)/ZETAD
          IF(FLII.EQ.14) FLZ=(FL14(ZETA,THETA)*2.0D0)/ZETAD
          IF(FLII.EQ.15) FLZ=(FL15(ZETA,THETA)*2.0D0)/ZETAD
          RETURN
      END
C SUB INTERACK.FOR
      SUBROUTINE INTERACK(OR_N,OR_Z)
          USE GLOBALS
C
          IMPLICIT NONE
C
C     ERROR RAY VARIABLES 3/5/2006
          REAL*8 ORIG_X,ORIG_Y,DELTAR
          REAL*8 DOUB,RANDMM,DELTAX,DELTAY
          EXTERNAL RANDMM
          REAL*8 RRR9,RRR10
          COMMON/RRRS/RRR9,RRR10
C
          REAL*8 SIGNNU,OR_N,OR_Z,PHA11,MAGO,MAGR,
     1    NUSUBS,TIRTESTER,SINI,RR_N,TESTLEN,RR_Z
     3    ,MAG,J,ARG,SNIND2
     4    ,XX,YY,AX,AY,AZ
     6    ,SGNB,SNINDX,UX,UY,UXP,UYP,INEW,L1,M1
C
          INTEGER ISURF
C
C     DIFFRACTION GRATING STUFF
          REAL*8 CHIX,CHIY,CHIZ,CHINOR,BLAM,CTERM,BTERM,FACTOR,
     1    SMU,WLU,BGAM,DD,DSPACE,QQUU,GAM1,
     3    QX,QY,QZ,PX,PY,PZ,PNOR,QNOR
C
C     HOE STUFF
          REAL*8 LO,MO,NO,LR,MR,NR,LAMC,LAMP,EMM,BTA
          REAL*8 GRO,GRS,GRX,GRY,GRZ
C
          LOGICAL GERROR
C
          INTEGER SPDCD1,SPDCD2
          COMMON/SPRA2/SPDCD1,SPDCD2
C
          REAL*8 XPASS,YPASS,ZPASS
C
          COMMON/SAGPAS/XPASS,YPASS,ZPASS
C
          INTEGER WWVN
          COMMON/WVPASS/WWVN
          LOGICAL TIR
          COMMON/RIT/TIR
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          TIR=.FALSE.
C
C     TYPE 13 SETUP
          IF(ALENS(34,R_I).EQ.13.0D0) THEN
C     SET UP THE STARTING RAY COORDINATED ON THE OBJECT
C     SURFACES IN THE TWO CFGS WHERE RAYS ARE TRACED
              HOE_X1=INT(FTFL01(3,R_I))
              HOE_Y1=INT(FTFL01(4,R_I))
              HOE_Z1=INT(FTFL01(5,R_I))
              HOE_X2=INT(FTFL01(8,R_I))
              HOE_Y2=INT(FTFL01(9,R_I))
              HOE_Z2=INT(FTFL01(10,R_I))
          END IF


          DD=0.0D0
          BTERM=0.0D0
          CTERM=0.0D0
          R_L0=R_L
          R_M0=R_M
          R_N0=R_N
C******THE COSINE SQUARED OF THE ANGLE OF INCIDENCE IS JUST
          COSI=(LN*R_L)+(MN*R_M)+(NN*R_N)
          IF(COSI.LT.-1.0D0) COSI=-1.0D0
          IF(COSI.GT.+1.0D0) COSI=+1.0D0
          PHASE=0.0D0
          RR_Z=OR_Z
          RR_N=OR_N
          IF(WVN.EQ.1) WWVN=46
          IF(WVN.EQ.2) WWVN=47
          IF(WVN.EQ.3) WWVN=48
          IF(WVN.EQ.4) WWVN=49
          IF(WVN.EQ.5) WWVN=50
          IF(WVN.EQ.6) WWVN=71
          IF(WVN.EQ.7) WWVN=72
          IF(WVN.EQ.8) WWVN=73
          IF(WVN.EQ.9) WWVN=74
          IF(WVN.EQ.10) WWVN=75
          SNINDX=DABS(ALENS(WWVN,R_I-1))/ALENS(WWVN,R_I-1)
          IF((1.0D0-(COSI**2)).LT.0.0D0) THEN
              SINI=0.0D0
          ELSE
              SINI=DSQRT(1.0D0-(COSI**2))
          END IF
          TIRTESTER=SINI*SNINDX
          IF(TIRTESTER.GT.1.0D0) TIR=.TRUE.
C     TIRTESTER IS USED DURING A REFLECTION SPECIFIED WITH A REFLTIRO
C     THIS MEANS THAT THE REFLECTION ONLY WORKS IF TIRTESTER>1.0
          SNIND2=DABS(ALENS(WWVN,R_I))/ALENS(WWVN,R_I)
          NUSUBS=((ALENS((WWVN),(R_I-1)))/
     1    (ALENS((WWVN),R_I)))
          SIGNNU=DABS(NUSUBS)/NUSUBS
          NUSUBS=DABS(NUSUBS)
          NUSUBS=DABS(NUSUBS)
          SMU=NUSUBS
          IF(ALENS(96,R_I).EQ.1.0D0) THEN
              GRO=ALENS(97,R_I)
              GRS=ALENS(98,R_I)
              GRX=ALENS(99,R_I)
              GRY=ALENS(100,R_I)
              GRZ=ALENS(101,R_I)
          ELSE
              GRO=0.0D0
              GRS=0.001D0
              GRX=0.0D0
              GRY=1.0D0
              GRZ=0.0D0
          END IF
C
C****************PERFECT LENS******************************************
C
          IF(GLANAM(R_I,2).EQ.'PERFECT      ') THEN
C     COMPUTE THE DIRECTION COSINES OF THE RAY
C     FOR A PERFECT LENS WITH EFL = ALENS(3,R_I)
              IF(R_L.EQ.0.0D0) THEN
                  IF(R_N.GE.0.0D0) UX=0.0D0
                  IF(R_N.LT.0.0D0) UX=PII
              ELSE
                  IF(DABS(R_L).GE.DABS(1.0D35*R_N)) THEN
                      IF(R_L.GE.0.0D0) UX=PII/2.0D0
                      IF(R_L.LT.0.0D0) UX=(3.0D0*PII)/2.0D0
                  ELSE
                      IF(DABS(R_L).EQ.0.0D0.AND.DABS(R_N).EQ.0.0D0) THEN
                          UX=0.0D0
                      ELSE
                          UX=DATAN2(R_L,R_N)
                      END IF
                  END IF
              END IF
              IF(R_M.EQ.0.0D0) THEN
                  IF(R_N.GE.0.0D0) UY=0.0D0
                  IF(R_N.LT.0.0D0) UY=PII
              ELSE
                  IF(DABS(R_M).GE.DABS(1.0D35*R_N)) THEN
                      IF(R_M.GE.0.0D0) UY=PII/2.0D0
                      IF(R_M.LT.0.0D0) UY=(3.0D0*PII)/2.0D0
                  ELSE
                      IF(DABS(R_M).EQ.0.0D0.AND.DABS(R_N).EQ.0.0D0) THEN
                          UY=0.0D0
                      ELSE
                          UY=DATAN2(R_M,R_N)
                      END IF
                  END IF
              END IF
              IF(UX.GT.(PII/2.0D0).AND.UX.LE.PII) THEN
                  UX=-(PII-UX)
              END IF
              IF(UX.LT.(-PII/2.0D0).AND.UX.GE.-PII) THEN
                  UX=(PII+UX)
              END IF
              IF(UY.GT.(PII/2.0D0).AND.UY.LE.PII) THEN
                  UY=-(PII-UY)
              END IF
              IF(UY.LT.(-PII/2.0D0).AND.UY.GE.-PII) THEN
                  UY=(PII+UY)
              END IF
              UXP=-((R_X/(ALENS(3,R_I)))-UX)
              UYP=-((R_Y/(ALENS(3,R_I)))-UY)
              INEW=ALENS(3,R_I)
              R_L=UXP*INEW
              R_M=UYP*INEW
              R_N=INEW
              MAG=DSQRT((R_L**2)+(R_M**2)+(R_N**2))
              R_L=R_L/MAG
              R_M=R_M/MAG
              R_N=R_N/MAG
              GO TO 90
          END IF
C****************PERFECT LENS DONE*************************************
C****************IDEAL LENS******************************************
C
          IF(GLANAM(R_I,2).EQ.'IDEAL        ') THEN
C     COMPUTE THE DIRECTION COSINES OF THE RAY
C     FOR A PERFECT LENS WITH EFL = ALENS(121,R_I)
              IF(R_L.EQ.0.0D0) THEN
                  IF(R_N.GE.0.0D0) UX=0.0D0
                  IF(R_N.LT.0.0D0) UX=PII
              ELSE
                  IF(DABS(R_L).GE.DABS(1.0D35*R_N)) THEN
                      IF(R_L.GE.0.0D0) UX=PII/2.0D0
                      IF(R_L.LT.0.0D0) UX=(3.0D0*PII)/2.0D0

                  ELSE
                      IF(DABS(R_L).EQ.0.0D0.AND.DABS(R_N).EQ.0.0D0) THEN
                          UX=0.0D0
                      ELSE
                          UX=DATAN2(R_L,R_N)
                      END IF
                  END IF
              END IF
              IF(R_M.EQ.0.0D0) THEN
                  IF(R_N.GE.0.0D0) UY=0.0D0
                  IF(R_N.LT.0.0D0) UY=PII
              ELSE
                  IF(DABS(R_M).GE.DABS(1.0D35*R_N)) THEN
                      IF(R_M.GE.0.0D0) UY=PII/2.0D0
                      IF(R_M.LT.0.0D0) UY=(3.0D0*PII)/2.0D0
                  ELSE
                      IF(DABS(R_M).EQ.0.0D0.AND.DABS(R_N).EQ.0.0D0) THEN
                          UY=0.0D0
                      ELSE
                          UY=DATAN2(R_M,R_N)
                      END IF
                  END IF
              END IF
              IF(UX.GT.(PII/2.0D0).AND.UX.LE.PII) THEN
                  UX=-(PII-UX)
              END IF
              IF(UX.LT.(-PII/2.0D0).AND.UX.GE.-PII) THEN
                  UX=(PII+UX)
              END IF
              IF(UY.GT.(PII/2.0D0).AND.UY.LE.PII) THEN
                  UY=-(PII-UY)
              END IF
              IF(UY.LT.(-PII/2.0D0).AND.UY.GE.-PII) THEN
                  UY=(PII+UY)
              END IF
              UXP=-((R_X/(ALENS(121,R_I)))-UX)
              UYP=-((R_Y/(ALENS(121,R_I)))-UY)
              INEW=ALENS(3,R_I)
              R_L=UXP*INEW
              R_M=UYP*INEW
              R_N=INEW
              MAG=DSQRT((R_L**2)+(R_M**2)+(R_N**2))
              R_L=R_L/MAG
              R_M=R_M/MAG
              R_N=R_N/MAG
              GO TO 90
          END IF
C****************PERFECT LENS DONE*************************************
C
          IF(STOPP.EQ.1) RETURN
C
C       THE RETURNED VALUES OF X,Y,Z ARE THE CORRECT INTERSECTIONS
C       TO WITHIN THE SURTOL VALUE
C
C
          IF(ALENS(96,R_I).EQ.1.0D0.AND..NOT.DUM(R_I).AND.ALENS(98,R_I)
     1    .NE.0.0D0) THEN
C
C     GRATING PRE-CALCULATIONS (A REAL LINEAR GRATING)
C
C     WE HAVE A GRATING, THINGS NEED TO BE CALCULATED
C     CALCULATE THE DIRCTION COSINES OF THE UNIT VECTOR NORMAL
C     TO THE GENERATING PLANES
C     MAKE SURE ALL DIRCOS NOT ZERO
              IF(GRX.EQ.0.0D0.AND.GRY.EQ.0.0D0
     1        .AND.GRZ.EQ.0.0D0) GRY=1.0D0
C
              CHINOR=DSQRT((GRX**2)+(GRY**2)+
     1        (GRZ**2))
              CHIX=GRX/CHINOR
              CHIY=GRY/CHINOR
              CHIZ=GRZ/CHINOR
C     CALCULATE THE COMPONENTS OF THE VECTOR Q WHICH IS PARALLEL
C     TO THE RULINGS
              QX=-(CHIY*NN)+(CHIZ*MN)
              QY=-(CHIZ*LN)+(CHIX*NN)
              QZ=-(CHIX*MN)+(CHIY*LN)
              QNOR=DSQRT((QX**2)+(QY**2)+(QZ**2))
              QX=QX/QNOR
              QY=QY/QNOR
              QZ=QZ/QNOR
C     CALCULATE THE COMPONENTS OF THE VECTOR P WHICH IS NORMAL
C     TO THE RULINGS
              PX=(QY*NN)-(QZ*MN)
              PY=(QZ*LN)-(QX*NN)
              PZ=(QX*MN)-(QY*LN)
              PNOR=DSQRT((PX**2)+(PY**2)+(PZ**2))
              PX=PX/PNOR
              PY=PY/PNOR
              PZ=PZ/PNOR
              FACTOR=(CHIX*PX)+(CHIY*PY)+(CHIZ*PZ)
C
              IF(DABS(FACTOR).GT.1E-6) DSPACE=GRS/FACTOR
              IF(DABS(FACTOR).LE.1E-6) DSPACE=1.0D20
C     NOW CALC BIG GAMMA AND SMALL MU
C      WLU=WAVELENGTH IN MICROMETER*(LENS UNITS/MICROMETER)
              IF(SYSTEM1(6).EQ.1.0D0)
     1        WLU=SYSTEM1(INT(WVN))*3.93700787402D-5
              IF(SYSTEM1(6).EQ.2.0D0)
     1        WLU=SYSTEM1(INT(WVN))*1.0D-4
              IF(SYSTEM1(6).EQ.3.0D0)
     1        WLU=SYSTEM1(INT(WVN))*1.0D-3
              IF(SYSTEM1(6).EQ.4.0D0)
     1        WLU=SYSTEM1(INT(WVN))*1.0D-6
C
              BLAM=-(GRO*WLU)/((DSPACE)*DABS(ALENS((WWVN),R_I)))
C
              BTERM=2.0D0*SMU*((R_L*LN)+(R_M*MN)+(R_N*NN))
              CTERM=(SMU**2)-1.0D0+(BLAM**2)
     1        -((2.0*SMU*BLAM)*((R_L*PX)+(R_M*PY)+(R_N*PZ)))
              DD=(BTERM**2)-(4.0D0*CTERM)
              IF(SIGNNU.LT.0.0D0) THEN
C
C                   WE HAVE A REFLECTION GRATING
C
                  IF(DD.LT.0.0D0) THEN
C       RAY ERROR OCCURRED, PRINT MESSAGE AND STOP TRACING
                      IF(MSG) THEN
                          WRITE(OUTLYNE,*)'RAY FAILURE OCCURRED AT SURFACE ',R_I
                          CALL SHOWIT(1)
                          OUTLYNE=
     1                    'ANGLE OF DIFFRACTION AT THE GRATING IS PHYSICALLY UNREALIZABLE'
                          CALL SHOWIT(1)
                      END IF
                      RAYCOD(1)=5
                      RAYCOD(2)=R_I
                      SPDCD1=RAYCOD(1)
                      SPDCD2=R_I
                      STOPP=1
                      RETURN
C       NO RAY ERROR
                  END IF
                  IF(ALENS(125,R_I).EQ.1.0D0.AND.TIRTESTER.GT.1.0D0) THEN
C       RAY ERROR OCCURRED, PRINT MESSAGE AND STOP TRACING
                      IF(MSG) THEN
                          WRITE(OUTLYNE,*)'RAY FAILURE OCCURRED AT SURFACE ',R_I
                          CALL SHOWIT(1)
                          OUTLYNE=
     1                    'TIR CONDITION NOT MET'
                          CALL SHOWIT(1)
                      END IF
                      RAYCOD(1)=20
                      RAYCOD(2)=R_I
                      SPDCD1=RAYCOD(1)
                      SPDCD2=R_I
                      STOPP=1
                      RETURN
C       NO RAY ERROR
                  END IF
                  SGNB=1.0
                  IF(BTERM.NE.0.0D0) SGNB=BTERM/DABS(BTERM)
                  QQUU=-0.5D0*(BTERM+(SGNB*DSQRT(DD)))
                  GAM1=QQUU
                  BGAM=GAM1
C
              ELSE
C
C     WE HAVE A TRANSMISSION GRATING
C
                  IF(DD.LT.0.0D0) THEN
C       RAY ERROR OCCURRED, PRINT MESSAGE AND STOP TRACING
                      IF(MSG) THEN
                          WRITE(OUTLYNE,*)'RAY FAILURE OCCURRED AT SURFACE ',R_I
                          CALL SHOWIT(1)
                          OUTLYNE=
     1                    'ANGLE OF DIFFRACTION AT THE GRATING IS PHYSICALLY UNREALIZABLE'
                          CALL SHOWIT(1)
                      END IF
                      RAYCOD(1)=5
                      RAYCOD(2)=R_I
                      SPDCD1=RAYCOD(1)
                      SPDCD2=R_I
                      STOPP=1
                      RETURN
C       NO RAY ERROR
                  END IF
                  SGNB=1.0D0
                  IF(BTERM.NE.0.0D0) SGNB=BTERM/DABS(BTERM)
                  QQUU=-0.5D0*(BTERM+(SGNB*DSQRT(DD)))
                  GAM1=QQUU
                  IF(GAM1.EQ.0.0D0) THEN
C       RAY ERROR OCCURRED, PRINT MESSAGE AND STOP TRACING
                      IF(MSG) THEN
                          WRITE(OUTLYNE,*)'RAY FAILURE OCCURRED AT SURFACE ',R_I
                          CALL SHOWIT(1)
                          OUTLYNE=
     1                    'ANGLE OF DIFFRACTION AT THE GRATING IS PHYSICALLY UNREALIZABLE'
                          CALL SHOWIT(1)
                      END IF
                      RAYCOD(1)=5
                      RAYCOD(2)=R_I
                      SPDCD1=RAYCOD(1)
                      SPDCD2=R_I
                      STOPP=1
                      RETURN
C       NO RAY ERROR
                  END IF
                  BGAM=CTERM/GAM1

              END IF
          ELSE
C     NOT A GRATING
              BLAM=0.0D0
C***************NON-GRATING PRE-CALCULATIONS***************************
C
              GRX=0.0D0
              GRY=0.0D0
              GRZ=0.0D0
              PX=0.0D0
              PY=0.0D0
              PZ=0.0D0
              BLAM=0.0D0
C     NOW CALC BIG GAMMA AND SMALL MU
              SMU=NUSUBS
C
              BTERM=2.0D0*SMU*((R_L*LN)+(R_M*MN)+(R_N*NN))
              CTERM=(SMU**2)-1.0D0+(BLAM**2)
     1        -((2.0*SMU*BLAM)*((R_L*PX)+(R_M*PY)+(R_N*PZ)))
              DD=(BTERM**2)-(4.0D0*CTERM)
              IF(SIGNNU.LT.0.0D0) THEN
C
C                   A REFLECTION
C
                  IF(ALENS(125,R_I).EQ.1.0D0.AND.TIRTESTER.GT.1.0D0) THEN
C       RAY ERROR OCCURRED, PRINT MESSAGE AND STOP TRACING
                      IF(MSG) THEN
                          WRITE(OUTLYNE,*)'RAY FAILURE OCCURRED AT SURFACE ',R_I
                          CALL SHOWIT(1)
                          OUTLYNE=
     1                    'TIR CONDITION NOT MET'
                          CALL SHOWIT(1)
                      END IF
                      RAYCOD(1)=20
                      RAYCOD(2)=R_I
                      SPDCD1=RAYCOD(1)
                      SPDCD2=R_I
                      STOPP=1
                      RETURN
C       NO RAY ERROR
                  END IF
                  SGNB=1.0D0
                  IF(BTERM.NE.0.0D0) SGNB=BTERM/DABS(BTERM)
                  QQUU=-0.5D0*(BTERM+(SGNB*DSQRT(DD)))
                  GAM1=QQUU
                  BGAM=GAM1
C
              ELSE
C
C     WE HAVE A REFRACTION
C
                  IF(DD.LT.0.0D0) THEN
C       RAY ERROR OCCURRED, PRINT MESSAGE AND STOP TRACING
                      IF(MSG) THEN
                          WRITE(OUTLYNE,*)'RAY FAILURE OCCURRED AT SURFACE ',R_I
                          CALL SHOWIT(1)
                          OUTLYNE=
     1                    'TOTAL INTERNAL REFLECTION'
                          CALL SHOWIT(1)
                      END IF
                      RAYCOD(1)=4
                      RAYCOD(2)=R_I
                      SPDCD1=RAYCOD(1)
                      SPDCD2=R_I
                      STOPP=1
                      RETURN
C       NO RAY ERROR
                  END IF
                  SGNB=1.0D0
                  IF(BTERM.NE.0.0D0) SGNB=BTERM/DABS(BTERM)
                  QQUU=-0.5D0*(BTERM+(SGNB*DSQRT(DD)))
                  GAM1=QQUU
                  IF(GAM1.EQ.0.0D0) THEN
C       RAY ERROR OCCURRED, PRINT MESSAGE AND STOP TRACING
                      IF(MSG) THEN
                          WRITE(OUTLYNE,*)'RAY FAILURE OCCURRED AT SURFACE ',R_I
                          CALL SHOWIT(1)
                          OUTLYNE=
     1                    'TOTAL INTERNAL REFLECTION'
                          CALL SHOWIT(1)
                      END IF
                      RAYCOD(1)=4
                      RAYCOD(2)=R_I
                      SPDCD1=RAYCOD(1)
                      SPDCD2=R_I
                      STOPP=1
                      RETURN
C       NO RAY ERROR
                  END IF
                  BGAM=CTERM/GAM1

              END IF
          END IF
C     ************************************************************************
C
          IF(ALENS(34,R_I).EQ.12.0D0.OR.ALENS(34,R_I).EQ.13.0D0) THEN
C     HOE PRECALCULATIONS
              EMM=FTFL01(1,R_I)
              LAMC=FTFL01(2,R_I)
              LAMP=SYSTEM1(INT(WVN))
C
              IF(ALENS(34,R_I).EQ.12.0D0) THEN
                  LO=FTFL01(3,R_I)-R_X
                  MO=FTFL01(4,R_I)-R_Y
                  NO=FTFL01(5,R_I)-R_Z
                  IF(FTFL01(6,R_I).GT.0.0D0.AND.NO.GT.0.0D0.OR.
     1            FTFL01(6,R_I).LT.0.0D0.AND.NO.LT.0.0D0) THEN
                      LO=-LO
                      MO=-MO
                      NO=-NO
                  END IF
              END IF
              IF(ALENS(34,R_I).EQ.13.0D0) THEN
                  LO=L1HOE
                  MO=M1HOE
                  NO=N1HOE
                  IF(FTFL01(6,R_I).LT.0.0D0) THEN
                      NO=-NO
                  END IF
                  IF(FTFL01(7,R_I).GT.0.0D0.AND.NO.GT.0.0D0.OR.
     1            FTFL01(7,R_I).LT.0.0D0.AND.NO.LT.0.0D0) THEN
                      LO=-LO
                      MO=-MO
                      NO=-NO
                  END IF
              END IF
              MAGO=DSQRT((LO**2)+(MO**2)+(NO**2))
              IF(MAGO.NE.0.0D0) THEN
                  LO=LO/MAGO
                  MO=MO/MAGO
                  NO=NO/MAGO
              END IF
C
              IF(ALENS(34,R_I).EQ.12.0D0) THEN
                  LR=FTFL01(7,R_I)-R_X
                  MR=FTFL01(8,R_I)-R_Y
                  NR=FTFL01(9,R_I)-R_Z
                  IF(FTFL01(10,R_I).GT.0.0D0.AND.NR.GT.0.0D0.OR.
     1            FTFL01(10,R_I).LT.0.0D0.AND.NR.LT.0.0D0) THEN
                      LR=-LR
                      MR=-MR
                      NR=-NR
                  END IF
              END IF
              IF(ALENS(34,R_I).EQ.13.0D0) THEN
                  LR=L2HOE
                  MR=M2HOE
                  NR=N2HOE
                  IF(FTFL01(11,R_I).LT.0.0D0) THEN
                      NR=-NR
                  END IF
                  IF(FTFL01(12,R_I).GT.0.0D0.AND.NR.GT.0.0D0.OR.
     1            FTFL01(12,R_I).LT.0.0D0.AND.NR.LT.0.0D0) THEN
                      LR=-LR
                      MR=-MR
                      NR=-NR
                  END IF
              END IF
              MAGR=DSQRT((LR**2)+(MR**2)+(NR**2))
              IF(MAGR.NE.0.0D0) THEN
                  LR=LR/MAGR
                  MR=MR/MAGR
                  NR=NR/MAGR
              END IF
C     BTA
              BTA=(EMM*LAMP)/(LAMC*DABS(ALENS((WWVN),R_I)))
              AX=BTA*(LO-LR)
              AY=BTA*(MO-MR)
              AZ=BTA*(NO-NR)
C     IS THERE PHASE FROM A TYPE 12 HOE TYPE SURFACE ?
              XX=R_X
              YY=R_Y
              MYI=R_I
              L1=0.0D0
              M1=0.0D0
              IF(FTFL01(11,R_I).EQ.1.0D0.OR.FTFL01(11,R_I).EQ.6.0D0)
     1        CALL PH12(L1,M1,XX,YY)
              IF(FTFL01(11,R_I).EQ.7.0D0) CALL PH11(L1,M1,XX,YY,PHA11)
              IF(FTFL01(11,R_I).EQ.2.0D0.OR.FTFL01(11,R_I).EQ.3.0D0.OR.
     1        FTFL01(11,R_I).EQ.4.0D0.OR.FTFL01(11,R_I).EQ.5.0D0)
     1        CALL PH13(L1,M1,XX,YY)
              GRX=-L1
              GRY=-M1
              GRZ=0.0D0
              IF(GRX.EQ.0.0D0.AND.GRY.EQ.0.0D0) THEN
                  BLAM=0.0D0
              ELSE
                  CHINOR=DSQRT((GRX**2)+(GRY**2)+
     1            (GRZ**2))
                  CHIX=GRX/CHINOR
                  CHIY=GRY/CHINOR
                  CHIZ=GRZ/CHINOR
C     CALCULATE THE COMPONENTS OF THE VECTOR Q WHICH IS PARALLEL
C     TO THE RULINGS
                  QX=-(CHIY*NN)+(CHIZ*MN)
                  QY=-(CHIZ*LN)+(CHIX*NN)
                  QZ=-(CHIX*MN)+(CHIY*LN)
                  QNOR=DSQRT((QX**2)+(QY**2)+(QZ**2))
                  QX=QX/QNOR
                  QY=QY/QNOR
                  QZ=QZ/QNOR
C     CALCULATE THE COMPONENTS OF THE VECTOR P WHICH IS NORMAL
C     TO THE RULINGS
                  PX=(QY*NN)-(QZ*MN)
                  PY=(QZ*LN)-(QX*NN)
                  PZ=(QX*MN)-(QY*LN)
                  PNOR=DSQRT((PX**2)+(PY**2)+(PZ**2))
                  PX=PX/PNOR
                  PY=PY/PNOR
                  PZ=PZ/PNOR
                  FACTOR=(CHIX*PX)+(CHIY*PY)+(CHIZ*PZ)
C
C     NOW CALC BIG GAMMA AND SMALL MU
C      WLU=WAVELENGTH IN MICROMETER*(LENS UNITS/MICROMETER)
                  IF(SYSTEM1(6).EQ.1.0D0)
     1            WLU=FTFL01(2,R_I)*3.93700787402D-5
                  IF(SYSTEM1(6).EQ.2.0D0)
     1            WLU=FTFL01(2,R_I)*1.0D-4
                  IF(SYSTEM1(6).EQ.3.0D0)
     1            WLU=FTFL01(2,R_I)*1.0D-3
                  IF(SYSTEM1(6).EQ.4.0D0)
     1            WLU=FTFL01(2,R_I)*1.0D-6
C
                  GRS=WLU/CHINOR
                  IF(DABS(FACTOR).GT.1E-6) DSPACE=GRS/FACTOR
                  IF(DABS(FACTOR).LE.1E-6) DSPACE=1.0D20
                  GRO=FTFL01(1,R_I)
                  BLAM=(GRO*WLU)/((DSPACE)*DABS(ALENS((WWVN),R_I)))
              END IF
C     NOW CALC BIG GAMMA AND SMALL MU
              SMU=NUSUBS
C
              BTERM=2.0D0*(SMU*((R_L*LN)+(R_M*MN)+(R_N*NN)))
              CTERM=((SMU**2)+((AX**2)+(AY**2)+(AZ**2)))
     1        +((BLAM**2)*((PX**2)+(PY**2)+(PZ**2)))
     2        -(2.0D0*SMU*((R_L*AX)+(R_M*AY)+(R_N*AZ)))
     3        -(2.0D0*SMU*BLAM*((R_L*PX)+(R_M*PY)+(R_N*PZ)))
     4        +(2.0D0*BLAM*((AX*PX)+(AY*PY)+(AZ*PZ)))
     5        -1.0D0
              DD=(BTERM**2)-(4.0D0*CTERM)
              IF(SIGNNU.LT.0.0D0) THEN
C     WE HAVE A REFLECTION HOE
                  IF(DD.LT.0.0D0) THEN
C       RAY ERROR OCCURRED, PRINT MESSAGE AND STOP TRACING
                      IF(MSG) THEN
                          WRITE(OUTLYNE,*)'RAY FAILURE OCCURRED AT SURFACE ',R_I
                          CALL SHOWIT(1)
                          OUTLYNE=
     1                    'ANGLE OF DIFFRACTION AT THE HOE IS PHYSICALLY UNREALIZABLE'
                          CALL SHOWIT(1)
                      END IF
                      RAYCOD(1)=9
                      RAYCOD(2)=R_I
                      SPDCD1=RAYCOD(1)
                      SPDCD2=R_I
                      STOPP=1
                      RETURN
C       NO RAY ERROR
                  END IF
                  IF(ALENS(125,R_I).EQ.1.0D0.AND.TIRTESTER.GT.1.0D0) THEN
C       RAY ERROR OCCURRED, PRINT MESSAGE AND STOP TRACING
                      IF(MSG) THEN
                          WRITE(OUTLYNE,*)'RAY FAILURE OCCURRED AT SURFACE ',R_I
                          CALL SHOWIT(1)
                          OUTLYNE=
     1                    'TIR CONDITION NOT MET'
                          CALL SHOWIT(1)
                      END IF
                      RAYCOD(1)=20
                      RAYCOD(2)=R_I
                      SPDCD1=RAYCOD(1)
                      SPDCD2=R_I
                      STOPP=1
                      RETURN
C       NO RAY ERROR
                  END IF
                  SGNB=1.0D0
                  IF(BTERM.NE.0.0D0) SGNB=BTERM/DABS(BTERM)
                  QQUU=-0.5D0*(BTERM+(SGNB*DSQRT(DD)))
                  GAM1=QQUU
C     CASE OF A REFLECTION HOE, TAKE THE LARGER ROOT
                  BGAM=GAM1
              ELSE
C     WE HAVE A TRANSMISSION HOE
                  IF(DD.LT.0.0D0) THEN
C       RAY ERROR OCCURRED, PRINT MESSAGE AND STOP TRACING
                      IF(MSG) THEN
                          WRITE(OUTLYNE,*)'RAY FAILURE OCCURRED AT SURFACE ',R_I
                          CALL SHOWIT(1)
                          OUTLYNE=
     1                    'ANGLE OF DIFFRACTION AT THE HOE IS PHYSICALLY UNREALIZABLE'
                          CALL SHOWIT(1)
                      END IF
                      RAYCOD(1)=9
                      RAYCOD(2)=R_I
                      SPDCD1=RAYCOD(1)
                      SPDCD2=R_I
                      STOPP=1
                      RETURN
C       NO RAY ERROR
                  END IF
                  SGNB=1.0D0
                  IF(BTERM.NE.0.0D0) SGNB=BTERM/DABS(BTERM)
                  QQUU=-0.5D0*(BTERM+(SGNB*DSQRT(DD)))
                  GAM1=QQUU
                  IF(GAM1.EQ.0.0D0) THEN
C       RAY ERROR OCCURRED, PRINT MESSAGE AND STOP TRACING
                      IF(MSG) THEN
                          WRITE(OUTLYNE,*)'RAY FAILURE OCCURRED AT SURFACE ',R_I
                          CALL SHOWIT(1)
                          OUTLYNE=
     1                    'ANGLE OF DIFFRACTION AT THE HOE IS PHYSICALLY UNREALIZABLE'
                          CALL SHOWIT(1)
                      END IF
                      RAYCOD(1)=9
                      RAYCOD(2)=R_I
                      SPDCD1=RAYCOD(1)
                      SPDCD2=R_I
                      STOPP=1
                      RETURN
C       NO RAY ERROR
                  END IF
                  BGAM=CTERM/GAM1
C     NOT A HOE, NO PRECALCULATIONS
              END IF
          END IF
C     END OF HOE PRECALCULATOIONS
C     ***********************************************************************
C
C     NOW DEFLECT THE RAY
C
          IF(.NOT.DUM(R_I)) THEN
C       NOT A DUMMY SURFACE
              IF(SIGNNU.LT.0.0D0) THEN
C
C                            REFLECTION
C
                  IF(ALENS(34,R_I).NE.12.0D0.AND.ALENS(34,R_I).NE.13.0D0) THEN
C     NO HOE
C     GRATING
                      R_L=(SMU*R_L)-(BLAM*PX)+(BGAM*LN)
                      R_M=(SMU*R_M)-(BLAM*PY)+(BGAM*MN)
                      R_N=(SMU*R_N)-(BLAM*PZ)+(BGAM*NN)
C       RE-NORMALIZE
                      MAG=DSQRT((R_L**2)+(R_M**2)+(R_N**2))
                      R_L=R_L/MAG
                      R_M=R_M/MAG
                      R_N=R_N/MAG
                      R_L0=(R_L0-((2.0D0*COSI)*LN))
                      R_M0=(R_M0-((2.0D0*COSI)*MN))
                      R_N0=(R_N0-((2.0D0*COSI)*NN))
                      MAG=DSQRT((R_L0**2)+(R_M0**2)+(R_N0**2))
                      R_L0=R_L0/MAG
                      R_M0=R_M0/MAG
                      R_N0=R_N0/MAG
                      COSIP=(R_L*LN)+(R_M*MN)+(R_N*NN)
                      IF(COSIP.LT.-1.0D0) COSIP=-1.0D0
                      IF(COSIP.GT.+1.0D0) COSIP=+1.0D0
                      IF(ALENS(96,R_I).EQ.1.0D0.AND.ALENS(98,R_I).NE.0.0D0
     1                .OR.ALENS(34,R_I).EQ.12.0D0.OR.
     1                ALENS(34,R_I).EQ.13.0D0)
     2                CALL DPHASE
                  END IF
                  IF(ALENS(34,R_I).EQ.12.0D0.OR.ALENS(13,R_I).EQ.13.0D0) THEN
C     HOE
                      R_L=((SMU*R_L)-(AX)-(BLAM*PX)+(BGAM*LN))
                      R_M=((SMU*R_M)-(AY)-(BLAM*PY)+(BGAM*MN))
                      R_N=((SMU*R_N)-(AZ)-(BLAM*PZ)+(BGAM*NN))
C       RE-NORMALIZE
                      MAG=DSQRT((R_L**2)+(R_M**2)+(R_N**2))
                      R_L=R_L/MAG
                      R_M=R_M/MAG
                      R_N=R_N/MAG
                      R_L0=(R_L0-((2.0D0*COSI)*LN))
                      R_M0=(R_M0-((2.0D0*COSI)*MN))
                      R_N0=(R_N0-((2.0D0*COSI)*NN))
                      MAG=DSQRT((R_L0**2)+(R_M0**2)+(R_N0**2))
                      R_L0=R_L0/MAG
                      R_M0=R_M0/MAG
                      R_N0=R_N0/MAG
                      COSIP=(R_L*LN)+(R_M*MN)+(R_N*NN)
                      IF(COSIP.LT.-1.0D0) COSIP=-1.0D0
                      IF(COSIP.GT.+1.0D0) COSIP=+1.0D0
                      IF(ALENS(96,R_I).EQ.1.0D0.AND.ALENS(98,R_I).NE.0.0D0
     1                .OR.ALENS(34,R_I).EQ.12.0D0.OR.
     1                ALENS(34,R_I).EQ.13.0D0)
     2                CALL DPHASE
                  END IF
              ELSE
C
C                NOT REFLECTION, PROCEED
C
                  IF(ALENS(34,R_I).NE.12.0D0.AND.ALENS(34,R_I).NE.13.0D0) THEN
C     NO HOE
C     GRATING
                      R_L=(SMU*R_L)-(BLAM*PX)+(BGAM*LN)
                      R_M=(SMU*R_M)-(BLAM*PY)+(BGAM*MN)
                      R_N=(SMU*R_N)-(BLAM*PZ)+(BGAM*NN)
                      MAG=DSQRT((R_L**2)+(R_M**2)+(R_N**2))
                      R_L=R_L/MAG
                      R_M=R_M/MAG
                      R_N=R_N/MAG
                      ARG=(1.0D0-(NUSUBS**2)*(1.0D0-(COSI**2)))
                      IF(ARG.LT.0.0D0) THEN
C       TIR OCCURRED, PRINT MESSAGE AND STOP TRACING
                          TIR=.TRUE.
                          IF(MSG) THEN
                              WRITE(OUTLYNE,*)'RAY FAILURE OCCURRED AT SURFACE ',R_I
                              CALL SHOWIT(1)
                              OUTLYNE='TOTAL INTERNAL REFLECTION'
                              CALL SHOWIT(1)
                          END IF
                          RAYCOD(1)=4
                          RAYCOD(2)=R_I
                          SPDCD1=RAYCOD(1)
                          SPDCD2=R_I
                          STOPP=1
                          RETURN
C       NO TIR
                      END IF
                      IF(COSI.LE.0.0D0) COSIP=-DSQRT(ARG)
                      IF(COSI.GT.0.0D0) COSIP=DSQRT(ARG)
                      J=((DABS(ALENS((WWVN),R_I))*COSIP)
     1                -(DABS(ALENS((WWVN),(R_I-1)))*COSI))/
     2                (DABS(ALENS((WWVN),R_I)))
                      R_L0=(NUSUBS*R_L0)+(J*LN)
                      R_M0=(NUSUBS*R_M0)+(J*MN)
                      R_N0=(NUSUBS*R_N0)+(J*NN)
C       RE-NORMALIZE
                      MAG=DSQRT((R_L0**2)+(R_M0**2)+(R_N0**2))
                      R_L0=R_L0/MAG
                      R_M0=R_M0/MAG
                      R_N0=R_N0/MAG
                      COSIP=(R_L*LN)+(R_M*MN)+(R_N*NN)
                      IF(COSIP.LT.-1.0D0) COSIP=-1.0D0
                      IF(COSIP.GT.+1.0D0) COSIP=+1.0D0
                      IF(ALENS(96,R_I).EQ.1.0D0.AND.ALENS(98,R_I).NE.0.0D0
     1                .OR.ALENS(34,R_I).EQ.12.0D0.OR.
     1                ALENS(34,R_I).EQ.13.0D0)
     2                CALL DPHASE
                  END IF
                  IF(ALENS(34,R_I).EQ.12.0D0.OR.ALENS(34,R_I).EQ.13.0D0) THEN
C     HOE
                      R_L=((SMU*R_L)-(AX)-(BLAM*PX)+(BGAM*LN))
                      R_M=((SMU*R_M)-(AY)-(BLAM*PY)+(BGAM*MN))
                      R_N=((SMU*R_N)-(AZ)-(BLAM*PZ)+(BGAM*NN))
C       RE-NORMALIZE
                      MAG=DSQRT((R_L**2)+(R_M**2)+(R_N**2))
                      R_L=R_L/MAG
                      R_M=R_M/MAG
                      R_N=R_N/MAG
                      R_L0=(R_L0-((2.0D0*COSI)*LN))
                      R_M0=(R_M0-((2.0D0*COSI)*MN))
                      R_N0=(R_N0-((2.0D0*COSI)*NN))
                      MAG=DSQRT((R_L0**2)+(R_M0**2)+(R_N0**2))
                      R_L0=R_L0/MAG
                      R_M0=R_M0/MAG
                      R_N0=R_N0/MAG
                      COSIP=(R_L*LN)+(R_M*MN)+(R_N*NN)
                      IF(COSIP.LT.-1.0D0) COSIP=-1.0D0
                      IF(COSIP.GT.+1.0D0) COSIP=+1.0D0
                      IF(ARG.LT.0.0D0) THEN
C       TIR OCCURRED, PRINT MESSAGE AND STOP TRACING
                          TIR=.TRUE.
                          IF(MSG) THEN
                              WRITE(OUTLYNE,*)'RAY FAILURE OCCURRED AT SURFACE ',R_I
                              CALL SHOWIT(1)
                              OUTLYNE='TOTAL INTERNAL REFLECTION'
                              CALL SHOWIT(1)
                          END IF
                          RAYCOD(1)=4
                          RAYCOD(2)=R_I
                          SPDCD1=RAYCOD(1)
                          SPDCD2=R_I
                          STOPP=1
                          RETURN
C       NO TIR
                      END IF
                      IF(COSI.LE.0.0D0) COSIP=-DSQRT(ARG)
                      IF(COSI.GT.0.0D0) COSIP=DSQRT(ARG)
                      J=((DABS(ALENS((WWVN),R_I))*COSIP)
     1                -(DABS(ALENS((WWVN),(R_I-1)))*COSI))/
     2                (DABS(ALENS((WWVN),R_I)))
                      R_L0=(NUSUBS*R_L0)+(J*LN)
                      R_M0=(NUSUBS*R_M0)+(J*MN)
                      R_N0=(NUSUBS*R_N0)+(J*NN)
C       RE-NORMALIZE
                      MAG=DSQRT((R_L0**2)+(R_M0**2)+(R_N0**2))
                      R_L0=R_L0/MAG
                      R_M0=R_M0/MAG
                      R_N0=R_N0/MAG
                      COSIP=(R_L*LN)+(R_M*MN)+(R_N*NN)
                      IF(COSIP.LT.-1.0D0) COSIP=-1.0D0
                      IF(COSIP.GT.+1.0D0) COSIP=+1.0D0
                      IF(ALENS(96,R_I).EQ.1.0D0.AND.ALENS(98,R_I).NE.0.0D0
     1                .OR.ALENS(34,R_I).EQ.12.0D0.OR.
     1                ALENS(34,R_I).EQ.13.0D0)
     2                CALL DPHASE
                  END IF
              END IF
C     DUM
          END IF
C
C     NOW ADD APPROPRIATE PHASE
C
C     IF A PHASE SURFACE, FIX THE PHASE AND DIRCOS
          IF(ALENS(34,R_I).EQ.6.0D0.OR.ALENS(34,R_I).EQ.7.0D0
     1    .OR.ALENS(34,R_I).EQ.9.0D0.OR.ALENS(34,R_I).EQ.10.0D0
     1    .OR.ALENS(34,R_I).EQ.11.0D0.OR.ALENS(34,R_I).EQ.15.0D0) THEN
              R_L0=R_L
              R_M0=R_M
              R_N0=R_N
              RLRL=R_L
              RMRM=R_M
              RNRN=R_N
              MYX=R_X
              MYY=R_Y
              MYI=R_I
              PHASE=0.0D0
              INR=ALENS(76,R_I)
              CALL PHASOR
              R_L=RLRL
              R_M=RMRM
              R_N=RNRN
              COSIP=(R_L*LN)+(R_M*MN)+(R_N*NN)
              IF(COSIP.LT.-1.0D0) COSIP=-1.0D0
              IF(COSIP.GT.+1.0D0) COSIP=+1.0D0
          END IF
C     IF A TYPE 12 SURFACE, FIX THE PHASE AND DIRCOS
          IF(ALENS(34,R_I).EQ.12.0D0.AND.FTFL01(11,R_I).EQ.1.0D0.OR.
     1    ALENS(34,R_I).EQ.13.0D0.AND.FTFL01(11,R_I).EQ.1.0D0) THEN
              MYX=R_X
              MYY=R_Y
              MYI=R_I
              PHASE=0.0D0
              IF(FTFL01(11,R_I).EQ.1.0D0.OR.FTFL01(11,R_I).EQ.6.0D0)
     1        CALL PHAS12
              IF(FTFL01(11,R_I).EQ.2.0D0.OR.FTFL01(11,R_I).EQ.3.0D0.OR.
     1        FTFL01(11,R_I).EQ.4.0D0.OR.FTFL01(11,R_I).EQ.5.0D0)
     1        CALL PHAS13
              IF(FTFL01(11,R_I).EQ.7.0D0) PHASE=PHASE+PHA11
          END IF
C     IF A PHASE SURFACE 20
          IF(ALENS(34,R_I).EQ.20.0D0) THEN
              RLRL=R_L
              RMRM=R_M
              RNRN=R_N
              MYX=R_X
              MYY=R_Y
              MYI=R_I
              ISURF=MYI
              PHASE=0.0D0
              XPASS=R_X
              YPASS=R_Y
              CALL GRIDS(3,ISURF,GERROR)
              IF(.NOT.GERROR) GRIDSUNLOADED20(ISURF)=.FALSE.
              IF(GERROR) THEN
                  IF(MSG) THEN
                      WRITE(OUTLYNE,*)'RAY FAILURE OCCURRED AT SURFACE ',R_I
                      CALL SHOWIT(1)
                      OUTLYNE=
     1                'GRID FILE DOES NOT EXIST FOR THIS SURFACE'
                      CALL SHOWIT(1)
                      OUTLYNE=
     1                'OR IT HAS INSUFFICIENT DATA FOR THIS SURFACE'
                      CALL SHOWIT(1)
                  END IF
                  STOPP=1
                  RAYCOD(1)=15
                  RAYCOD(2)=R_I
                  RAYEXT=.FALSE.
                  POLEXT=.FALSE.
                  SPDCD1=RAYCOD(1)
                  SPDCD2=R_I
                  RETURN
              END IF
              R_L=RLRL
              R_M=RMRM
              R_N=RNRN
              COSIP=(R_L*LN)+(R_M*MN)+(R_N*NN)
              IF(COSIP.LT.-1.0D0) COSIP=-1.0D0
              IF(COSIP.GT.+1.0D0) COSIP=+1.0D0
          END IF
C     IF AT A DUMMY SURFACE, THE THICKNESS GOES FROM POS TO NEG
C     OR NEG TO POS, THEN IF RV, THEN SET .NOT.RV AND IF
C     NOT RV SET TO RV
C     OR AT A DUMMY SURFACE, N POS INTO NEG TH OR N NEG INTO POS TH
C     THEN IF RV, THEN SET .NOT.RV AND IF
          TESTLEN=R_Z-RR_Z
C     NOT RV SET TO RV
C     IF AT A DUMMY SURFACE, THE THICKNESS CHANGES SIGN, THEN THE
C     RAY GETS "REVERSED"
          IF(ALENS(3,R_I).GT.0.0D0.AND.ALENS(3,R_I-1).LT
     1    .0.0D0.AND.DUM(R_I).OR.ALENS(3,R_I).LT.0.0D0.AND.ALENS(3,R_I-1)
     2    .GT.0.0D0.AND.DUM(R_I)) THEN
              IF(RV) THEN
                  RV=.FALSE.
              ELSE
                  RV=.TRUE.
              END IF
          END IF
C     IF THE RAY HAS TRAVELED A NEG DIST WITH A POS DIRCOS
C     OR THE RAY HAS TRAVELED A POS DIST WITH A NEG DIR COS
C     AND THE RAY WAS NOT REVERESED, THEN SET IT TO "REVERSED"
C     RAY GETS "REVERSED"
          IF(RR_N.GT.0.0D0.AND.TESTLEN.LT.0.0D0.AND..NOT.REVSTR.OR.
     1    RR_N.LT.0.0D0.AND.TESTLEN.GT.0.0D0.AND..NOT.REVSTR.OR.
     2    RR_N.GT.0.0D0.AND.TESTLEN.GT.0.0D0.AND.REVSTR.OR.
     3    RR_N.LT.0.0D0.AND.TESTLEN.LT.0.0D0.AND.REVSTR.OR.
     4    SNINDX.GT.0.0D0.AND.TESTLEN.LT.0.0D0.AND..NOT.REVSTR.OR.
     5    SNINDX.LT.0.0D0.AND.TESTLEN.GT.0.0D0.AND..NOT.REVSTR.OR.
     6    SNINDX.GT.0.0D0.AND.TESTLEN.GT.0.0D0.AND.REVSTR.OR.
     7    SNINDX.LT.0.0D0.AND.TESTLEN.LT.0.0D0.AND.REVSTR) THEN
              RV=.TRUE.
          ELSE
              RV=.FALSE.
          END IF
          IF(RR_N.GT.0.0D0.AND.TESTLEN.LT.0.0D0.AND.RVSTART.OR.
     1    RR_N.LT.0.0D0.AND.TESTLEN.GT.0.0D0.AND.RVSTART) THEN
              RV=.FALSE.
          END IF
          IF(RR_N.GT.0.0D0.AND.TESTLEN.GT.0.0D0.AND..NOT.REVSTR.OR.
     1    RR_N.LT.0.0D0.AND.TESTLEN.LT.0.0D0.AND..NOT.REVSTR.OR.
     2    RR_N.GT.0.0D0.AND.TESTLEN.LT.0.0D0.AND.REVSTR.OR.
     3    RR_N.LT.0.0D0.AND.TESTLEN.GT.0.0D0.AND.REVSTR) THEN
              RV=.FALSE.
              RVSTART=.FALSE.
          END IF
C       RAYTRACE THROUGH ASPHERE COMPLETED
          IF(R_I.EQ.NEWIMG) THEN
              R_L=OLDL
              R_M=OLDM
              R_N=OLDN
          END IF
          COSIP=(R_L*LN)+(R_M*MN)+(R_N*NN)
          IF(COSIP.LT.-1.0D0) COSIP=-1.0D0
          IF(COSIP.GT.+1.0D0) COSIP=+1.0D0
          STOPP=0
          RAYCOD(1)=0
          RAYCOD(2)=R_I
          SPDCD1=RAYCOD(1)
          SPDCD2=R_I
 90       CONTINUE
C     HERE IS WHERE WE ADD A RANDOM RAY ERROR IF IT IS
C     REQUESTED IN THE LENS DATABASE USING THE RAYERROR COMMAND
          IF(ALENS(144,R_I).NE.0.0D0.AND.FOBRUN) THEN
              FOBRUN=.FALSE.
              RETURN
          END IF
C     NOT A FOB
          IF(ALENS(144,R_I).NE.0.0D0.AND..NOT.FOBRUN) THEN
C     COMPUTE THE ORIGINAL X AND Y
              ORIG_X=DATAN2(R_L,R_N)
              ORIG_Y=DATAN2(R_M,R_N)
              IF(.NOT.DIFRAYTRACE) THEN
C     LOAD LINEAR AND GAUSSIAN RANDOMS INTO REG 9 AND 10
                  DOUB=RANDMM()
                  RRR9=REG(9)
                  RRR10=REG(10)
              END IF
C     DELTAR IN RADIANS IS:
              DELTAR=(ALENS(144,R_I)/(3600.0D0))*(PII/180.0D0)*RRR10
              DELTAX=DELTAR*DCOS(2.0D0*PII*RRR9)
              DELTAY=DELTAR*DSIN(2.0D0*PII*RRR9)
              R_L=R_N*DTAN(ORIG_X+DELTAX)
              R_M=R_N*DTAN(ORIG_Y+DELTAY)
              R_N=1.0D0-DSQRT((R_L**2)+(R_M**2))
          END IF
          RETURN
      END
