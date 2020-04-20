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

C       FIFTH SET OF OPTIMIZATION ROUTINES

C SUB CALCPRE.FOR
      SUBROUTINE CALCPRE
C
C     CALCULATES AND LOADS THE VALUE1 OF OPERAND NUMBER PREDEFI
C
          IMPLICIT NONE
C
          COMMON/SOLU/X
C
          CHARACTER WQLOCAL*8
C
          REAL*8 W2LOCAL,CLEAR
C
          COMMON/LOCALWQ/WQLOCAL,W2LOCAL
C
          REAL*8 X(1:96),NWN1,NWN2,NWN3,NWN4,JPX,TRAYY,TRAYX
     1    ,JPY,JPUX,JPUY,JPCX,JPCY,JPUCX,JPUCY,VXLO,VXHI,VYLO,VYHI
     2    ,V1,V2,VALVAL,OPDWT,GREYOP,RPOINT,FPOINT,RUN_OPT_MAC
C
          COMMON/WTOPD/OPDWT,GREYOP
C
          INTEGER IAUTO,ISFI,ITYP,SF,CW,IV,PREDEFI,I,NF,IIA,JIA,SF1
     1    ,K,II,WVNUMOP,ERROR,ISURF,CLRTYP
C
          COMMON/AUTOI/IAUTO
C
          REAL*8 WV,VALUE1,CONSUM,DELLELL,DELLELL1,DELLELL2
          INTEGER NUM5,ORI
          COMMON/GV/VALUE1,NUM5
C
          COMMON/PRCOM/WV,ITYP
C
          REAL*8 OLDREG9,W1A,TEMPR1,TEMPR2,TEMPSUM,TEMPDIF
     1    ,INDEX,EDGTHK,VNUM,PARTL,V,DISP,INTV,W1B,AAA
C
          COMMON/NSIN/NF,INDEX,DISP,VNUM,PARTL
C
          LOGICAL OPMAP,XIS,REFERR,GERROR1,GERROR2,OPDERROR
C
          COMMON/OPOPMP/OPMAP
C
          REAL*8 INV,COSARG,OPVALUE1,DUMMY,EFLY,EFLX
C
          COMMON/PREPRE/PREDEFI
C
          LOGICAL OLDLDIF,OLDLDIF2,ERRR,ERROP
C
          EXTERNAL EDGTHK
C
          COMMON/OPDFIT1/ERROP
          COMMON/OPDFIT2/WVNUMOP
C
          INTEGER ONUM,CFNUM
C
          INCLUDE 'datcfg.inc'
          INCLUDE 'datmac.inc'
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datspd.inc'
          INCLUDE 'datsub.inc'
C
          REG(9)=0.0D0

C
C     THESE ARE DEFINED IN DATSPD.FOR
C     THESE NEXT 6 VARIABLES TRACK STATUS OF FIELD,RAY,CFG,SPDEXT
C     CPFNEXT AND ZERNIKE FIT
C     FOR PREDEFINED OPERANDS TO MINIMIZE EXCESSIVE RAY TRACING
C     MAKE SURE ALL PARAXIAL AND RELATED VALUE1S ARE UP TO DATE
C
          I=PREDEFI
C
C     I IS THE NUMBER OF THE OPERAND IN THE MERIT FUNCTION
C
C
          ONUM=INT(OPERND(I,17))


C     ONUM IS THE IDENTIFYING INTEGER FOR OPERAND I
C
C     TEST FOR A BAD OPERAND NUMBER
          IF(ONUM.LT.1.OR.ONUM.GT.515.0D0.AND.ONUM.LT.801.OR.ONUM
     1    .GT.801) THEN
C     PUT IN A BAD NUMBER
              REG(9)=-666.666
              GO TO 777
          END IF
C
          CFNUM=INT(OPERND(I,16))
C     CFNUM IS THE CONFIG IN WHICH THE OPERAND MUST BE EVALUATED
C     SAVE ORIGINAL VALUE1 OF ACC
          OLDREG9=REG(9)
C     IF NECESSARY, CHGANGE CONFIGS
C     IF DESIRED CONFIG IS NOT THE CURRENT CONFIG
          IF(CFNUM.NE.F12) THEN
              IAUTO=CFNUM
              CALL CFGCHG2
          END IF
C
          SELECT CASE (ONUM)
C
            CASE(480)
C     CROSS TRACK SPECTRAL CTSX
              ERROR=0
              V1=OPERND(I,8)
              V2=OPERND(I,9)
              CACOCH=0
              CALL CTS(VALVAL,1,V1,V2,ERROR)
              IF(ERROR.EQ.1) THEN
                  REG(9)=0.0D0
                  CALL MACFAL
                  RETURN
              END IF
              REG(9)=VALVAL
              GO TO 777
C
            CASE(481)
C     CROSS TRACK SPECTRAL CTSY
              ERROR=0
              V1=OPERND(I,8)
              V2=OPERND(I,9)
              CACOCH=0
              CALL CTS(VALVAL,2,V1,V2,ERROR)
              IF(ERROR.EQ.1) THEN
                  REG(9)=0.0D0
                  CALL MACFAL
                  RETURN
              END IF
              REG(9)=VALVAL
              GO TO 777
C
            CASE(482)
C     SPATIAL COREGISTRATION ERROR SCEX
              ERROR=0
              V1=OPERND(I,8)
              CACOCH=0
              CALL SCE(VALVAL,1,V1,ERROR)
              IF(ERROR.EQ.1) THEN
                  REG(9)=0.0D0
                  CALL MACFAL
                  RETURN
              END IF
              REG(9)=VALVAL
              GO TO 777
C
            CASE(483)
C     SPATIAL COREGISTRATION ERROR SCEX
              ERROR=0
              V1=OPERND(I,8)
              CACOCH=0
              CALL SCE(VALVAL,2,V1,ERROR)
              IF(ERROR.EQ.1) THEN
                  REG(9)=0.0D0
                  CALL MACFAL
                  RETURN
              END IF
              REG(9)=VALVAL
              GO TO 777
C
            CASE(512)
C     OPERAND #512 "MACOPT"
              REG(9)=RUN_OPT_MAC(INT(OPERND(I,8)))
              GO TO 777
C
            CASE(801)
C     OPERAND #801 "ACT"
              ISURF=INT(OPERND(I,8))
              WQLOCAL='ACT     '
              W2LOCAL=OPERND(I,9)
              CALL DEFGRIDS(7,ISURF,GERROR1,GERROR2)
              IF(GERROR1.OR.GERROR2) THEN
                  VALUE1=0.0D0
                  CALL MACFAL
                  RETURN
              END IF
              REG(9)=VALUE1
              GO TO 777
C
          END SELECT

C
C     REAL RAY BASED OPERANDS
          IF(ONUM.GE.1.AND.ONUM.LE.69) THEN
C     CALCULATE VALUE1 AND PLACE IN ACC
C
C     TEST IF RAY NEEDS TO BE TRACED
C
              IF(ONUM.EQ.16.OR.ONUM.EQ.17) THEN
                  FPOINT=OPERND(I,8)
                  RPOINT=OPERND(I,9)
              ELSE
                  FPOINT=OPERND(I,9)
                  RPOINT=OPERND(I,10)
              END IF
              IF(REFEXT.AND.INT(FPOINT).EQ.OLDF.AND.
     1        CFNUM.EQ.OLDCFG) THEN
C     DON'T NEED TO TRACE THE FOB AGAIN, IT ALREDY EXITS
              ELSE
C     TRACE FOB AND RAY THEN GET VALUE1
C     TRACE FIELD DESIGNAMTED BY NW4 INT(FPOINT))
C     TRACE RAY   DESIGNAMTED BY NW5 INT(RPOINT)
C     FOR REFERENCE RAY STUFF, RAY 0 0 IS ALWAYS TRACED
C     DO THE FOB
                  SAVE_KDP(1)=SAVEINPT(1)
                  OLDLDIF2=LDIF2
                  OLDLDIF=LDIF
                  IF(OPDIF) THEN
                      LDIF2=.TRUE.
                      LDIF=.TRUE.
                  ELSE
                      LDIF2=.FALSE.
                      LDIF=.FALSE.
                  END IF
                  WC='FOB     '
                  WQ='        '
                  SQ=0
                  SST=0
                  STI=0
                  W1=FIELDY(INT(FPOINT))
                  W2=FIELDX(INT(FPOINT))
                  W3=FIELDZ(INT(FPOINT))
                  W4=FIELDW(INT(FPOINT))
                  W5=0.0D0
                  DF1=0
                  DF2=0
                  DF3=0
                  DF4=0
                  DF5=1
                  S1=1
                  S2=1
                  S3=1
                  S4=1
                  S5=0
                  SN=1
C     SET MSG TO FALSE
                  MSG=.FALSE.
                  BADOPS=.FALSE.
                  CALL FFOB2
                  REST_KDP(1)=RESTINPT(1)
                  IF(BADOPS) THEN
                      IF(F28.EQ.1) BAAD=1
                      IF(F28.EQ.1) REG(9)=0.0D0
                      IF(F31.EQ.1) CALL MACFAL
                      RETURN
                  END IF
              END IF
C NOW THE RAY 0 0
C       SAVE_KDP(1)=SAVEINPT(1)
C       WC='RAY     '
C       WQ='        '
C       SQ=0
C       SST=0
C       DF1=1
C       DF2=1
C       DF3=0
C       DF4=1
C       DF5=1
C       S1=0
C       S2=0
C       S3=1
C       S4=0
C       S5=0
C       SN=0
C     SET MSG TO FALSE
C       MSG=.FALSE.
C     BADOPS=.FALSE.
C     W3=FIELDW(INT(FPOINT))
C     NOCOAT=.TRUE.
C       GRASET=.FALSE.
C       DXFSET=.FALSE.
C       CALL RRAY2
C     CACOCH=0
C       REST_KDP(1)=RESTINPT(1)
C     IF(BADOPS) THEN
C     IF(F28.EQ.1) BAAD=1
C     IF(F28.EQ.1) REG(9)=0.0D0
C     IF(F31.EQ.1) CALL MACFAL
C                       RETURN
C                     END IF
C NOW THE RAY
              IF(RAYEXT.AND.
     1        INT(RPOINT).EQ.OLDR.AND.CFNUM.EQ.OLDCFG) THEN
C     DON'T NEED TO TRACE THE RAY AGAIN, IT ALREDY EXITS
              ELSE
                  SAVE_KDP(1)=SAVEINPT(1)
                  WC='RAY     '
                  WQ='        '
                  SQ=0
                  SST=0
                  DF1=0
                  DF2=0
                  DF3=0
                  DF4=1
                  DF5=1
                  S1=1
                  S2=1
                  S3=1
                  S4=0
                  S5=0
                  SN=1
                  IF(OPERND(I,17).GE.43.0D0.AND.OPERND(I,17).LE.58.0D0) THEN
                      W1=0.0D0
                      W2=0.0D0
                      W3=FIELDW(INT(FPOINT))
                  ELSE
C     ADDING VIGNETTING OPTIONS TO TRANSVERS ABERRATIONS, THEIR DERIVATIVES
C     AND ASSOCIATED OPD AND OPDW VALUE1S.
                      IF(OPERND(I,17).GE.7.0D0.AND.OPERND(I,17).LE.12.0D0.OR.
     1                OPERND(I,17).GE.16.0D0.AND.OPERND(I,17).LE.17.0D0.OR.
     1                OPERND(I,17).GE.35.0D0.AND.OPERND(I,17).LE.42.0D0) THEN
                          IF(LVIG) CALL VIGCAL(100,VXLO,VXHI,1)
                          IF(LVIG) CALL VIGCAL(100,VYLO,VYHI,2)
                      ELSE
                          VXLO=-1.0D0
                          VXHI=1.0D0
                          VYLO=-1.0D0
                          VYHI=1.0D0
                      END IF
                      IF(RAYY(INT(RPOINT)).GE.0.0D0)
     1                TRAYY=RAYY(INT(RPOINT))*DABS(VYHI)
                      IF(RAYY(INT(RPOINT)).LT.0.0D0)
     1                TRAYY=RAYY(INT(RPOINT))*DABS(VYLO)
                      IF(RAYX(INT(RPOINT)).GE.0.0D0)
     1                TRAYX=RAYX(INT(RPOINT))*DABS(VXHI)
                      IF(RAYX(INT(RPOINT)).LT.0.0D0)
     1                TRAYX=RAYX(INT(RPOINT))*DABS(VXLO)
                      W1=TRAYY
                      W2=TRAYX
                      W3=RAYW(INT(RPOINT))
                  END IF
                  W4=0.0D0
                  W5=0.0D0
C     SET MSG TO FALSE
                  MSG=.FALSE.
                  BADOPS=.FALSE.
                  NOCOAT=.TRUE.
                  GRASET=.FALSE.
                  DXFSET=.FALSE.
                  CALL RRAY2
                  CACOCH=0
                  REST_KDP(1)=RESTINPT(1)
                  IF(BADOPS) THEN
                      IF(F28.EQ.1) BAAD=1
                      IF(F28.EQ.1) REG(9)=0.0D0
                      IF(F31.EQ.1) CALL MACFAL
                      RETURN
                  END IF
                  LDIF2=OLDLDIF2
                  LDIF=OLDLDIF
              END IF
              IF(.NOT.REFEXT.OR..NOT.RAYEXT) THEN
                  IF(F28.EQ.1) REG(9)=0.0D0
                  IF(F28.EQ.1) BAAD=1
                  IF(F31.EQ.1) CALL MACFAL
                  RETURN
              END IF
              OLDF=INT(FPOINT)
              OLDW=0
              OLDR=INT(RPOINT)
              OLDCFG=CFNUM

C     GET DATA FOR SURFACE DESIGNATED BY NW3 INT(OPERND(I,8))
              IF(OPERND(I,17).EQ.1.0D0) THEN
                  REG(9)=RAYRAY(1,INT(OPERND(I,8)))
                  GO TO 777
              END IF
              IF(OPERND(I,17).EQ.2.0D0) THEN
                  REG(9)=RAYRAY(2,INT(OPERND(I,8)))
                  GO TO 777
              END IF
              IF(OPERND(I,17).EQ.3.0D0) THEN
                  REG(9)=RAYRAY(3,INT(OPERND(I,8)))
                  GO TO 777
              END IF
              IF(OPERND(I,17).EQ.4.0D0) THEN
                  REG(9)=RAYRAY(4,INT(OPERND(I,8)))
                  GO TO 777
              END IF
              IF(OPERND(I,17).EQ.5.0D0) THEN
                  REG(9)=RAYRAY(5,INT(OPERND(I,8)))
                  GO TO 777
              END IF
              IF(OPERND(I,17).EQ.6.0D0) THEN
                  REG(9)=RAYRAY(6,INT(OPERND(I,8)))
                  GO TO 777
              END IF
C
              IF(OPERND(I,17).EQ.7.0D0) THEN
C     DX
                  REG(9)=
     1            (RAYRAY(1,INT(OPERND(I,8)))-REFRY(1,INT(OPERND(I,8))))
                  GO TO 777
              END IF
C
              IF(OPERND(I,17).EQ.8.0D0) THEN
C     DY
                  REG(9)=
     1            (RAYRAY(2,INT(OPERND(I,8)))-REFRY(2,INT(OPERND(I,8))))
                  GO TO 777
              END IF
C
              IF(OPERND(I,17).EQ.9.0D0) THEN
                  REG(9)=
     1            DABS(DSQRT((RAYRAY(1,INT(OPERND(I,8)))**2)
     2            +(RAYRAY(2,INT(OPERND(I,8)))**2))
     3            -DSQRT((REFRY(1,INT(OPERND(I,8)))**2)
     4            +(REFRY(2,INT(OPERND(I,8)))**2)))
                  GO TO 777
              END IF
C
              IF(OPERND(I,17).EQ.10.0D0) THEN
                  REG(9)=
     1            RAYRAY(11,INT(OPERND(I,8)))
     2            -REFRY(11,INT(OPERND(I,8)))
                  IF(REG(9).LT.-PII) REG(9)=REG(9)+(TWOPII)
                  IF(REG(9).GT.PII) REG(9)=REG(9)-(TWOPII)
                  IF(REG(9).EQ.TWOPII) REG(9)=0.0D0
                  GO TO 777
              END IF
C
              IF(OPERND(I,17).EQ.11.0D0) THEN
                  REG(9)=
     1            RAYRAY(12,INT(OPERND(I,8)))
     2            -REFRY(12,INT(OPERND(I,8)))
                  IF(REG(9).LT.-PII) REG(9)=REG(9)+(TWOPII)
                  IF(REG(9).GT.PII) REG(9)=REG(9)-(TWOPII)
                  IF(REG(9).EQ.TWOPII) REG(9)=0.0D0
                  GO TO 777
              END IF
C
              IF(OPERND(I,17).EQ.12.0D0) THEN
                  COSARG=((RAYRAY(4,INT(OPERND(I,8)))
     1            *REFRY(4,INT(OPERND(I,8))))+
     2            (RAYRAY(5,INT(OPERND(I,8)))*REFRY(5,INT(OPERND(I,8))))+
     1            (RAYRAY(6,INT(OPERND(I,8)))*REFRY(6,INT(OPERND(I,8)))))
                  IF(COSARG.LT.0.0D0) COSARG=-COSARG
                  IF(COSARG.GT.1.0D0) COSARG=1.0D0
                  REG(9)=DACOS(COSARG)
                  IF(REG(9).EQ.TWOPII) REG(9)=0.0D0
                  IF(REG(9).LT.-PII) REG(9)=REG(9)+(TWOPII)
                  IF(REG(9).GT.PII) REG(9)=REG(9)-(TWOPII)
                  GO TO 777
              END IF
C
              IF(OPERND(I,17).EQ.13.0D0) THEN
                  REG(9)=RAYRAY(11,INT(OPERND(I,8)))
                  IF(REG(9).EQ.TWOPII) REG(9)=0.0D0
                  IF(REG(9).LT.-PII) REG(9)=REG(9)+(TWOPII)
                  IF(REG(9).GT.PII) REG(9)=REG(9)-(TWOPII)
                  GO TO 777
              END IF
              IF(OPERND(I,17).EQ.14.0D0) THEN
                  REG(9)=RAYRAY(12,INT(OPERND(I,8)))
                  IF(REG(9).EQ.TWOPII) REG(9)=0.0D0
                  IF(REG(9).LT.-PII) REG(9)=REG(9)+(TWOPII)
                  IF(REG(9).GT.PII) REG(9)=REG(9)-(TWOPII)
                  GO TO 777
              END IF
              IF(OPERND(I,17).EQ.15.0D0) THEN
                  REG(9)=RAYRAY(7,INT(OPERND(I,8)))
                  GO TO 777
              END IF
C
              IF(OPERND(I,17).EQ.16.0D0) THEN
                  CALL GETOPD(OPVALUE1,DUMMY,OPDERROR)
                  IF(OPDERROR) OPVALUE1=0.0D0
                  REG(9)=OPVALUE1
                  GO TO 777
              END IF
              IF(OPERND(I,17).EQ.17.0D0) THEN
                  CALL GETOPD(DUMMY,OPVALUE1,OPDERROR)
                  IF(OPDERROR) OPVALUE1=0.0D0
                  REG(9)=OPVALUE1
                  GO TO 777
              END IF
C
              IF(OPERND(I,17).EQ.18.0D0) THEN
                  REG(9)=RAYRAY(19,INT(OPERND(I,8)))
                  GO TO 777
              END IF
              IF(OPERND(I,17).EQ.19.0D0) THEN
                  REG(9)=RAYRAY(20,INT(OPERND(I,8)))
                  GO TO 777
              END IF
              IF(OPERND(I,17).EQ.20.0D0) THEN
                  REG(9)=RAYRAY(21,INT(OPERND(I,8)))
                  GO TO 777
              END IF
              IF(OPERND(I,17).EQ.21.0D0) THEN
                  REG(9)=RAYRAY(8,INT(OPERND(I,8)))
                  GO TO 777
              END IF
              IF(OPERND(I,17).EQ.22.0D0) THEN
                  REG(9)=RAYRAY(9,INT(OPERND(I,8)))
                  GO TO 777
              END IF
              IF(OPERND(I,17).EQ.23.0D0) THEN
                  REG(9)=RAYRAY(10,INT(OPERND(I,8)))
                  GO TO 777
              END IF
              IF(OPERND(I,17).EQ.24.0D0) THEN
                  REG(9)=RAYRAY(13,INT(OPERND(I,8)))
                  GO TO 777
              END IF
              IF(OPERND(I,17).EQ.25.0D0) THEN
                  REG(9)=RAYRAY(14,INT(OPERND(I,8)))
                  GO TO 777
              END IF
              IF(OPERND(I,17).EQ.26.0D0) THEN
                  REG(9)=RAYRAY(15,INT(OPERND(I,8)))
                  GO TO 777
              END IF
C
              IF(OPERND(I,17).EQ.27.0D0) THEN
                  IF(OPDIF) THEN
                      REG(9)=
     1                (RFDIFF(1,INT(OPERND(I,8)))-REFRY(1,INT(OPERND(I,8))))/RFDELX
                  ELSE
                      REG(9)=0.0D0
                  END IF
                  GO TO 777
              END IF
              IF(OPERND(I,17).EQ.28.0D0) THEN
                  IF(OPDIF) THEN
                      REG(9)=
     1                (RFDIFF(7,INT(OPERND(I,8)))-REFRY(1,INT(OPERND(I,8))))/RFDELY
                  ELSE
                      REG(9)=0.0D0
                  END IF
                  GO TO 777
              END IF
              IF(OPERND(I,17).EQ.29.0D0) THEN
                  IF(OPDIF) THEN
                      REG(9)=
     1                (RFDIFF(2,INT(OPERND(I,8)))-REFRY(2,INT(OPERND(I,8))))/RFDELX
                  ELSE
                      REG(9)=0.0D0
                  END IF
                  GO TO 777
              END IF
              IF(OPERND(I,17).EQ.30.0D0) THEN
                  IF(OPDIF) THEN
                      REG(9)=
     1                (RFDIFF(8,INT(OPERND(I,8)))-REFRY(2,INT(OPERND(I,8))))/RFDELY
                  ELSE
                      REG(9)=0.0D0
                  END IF
                  GO TO 777
              END IF
C
              IF(OPERND(I,17).EQ.31.0D0) THEN
                  IF(OPDIF) THEN
                      IF(DABS(RFDIFF(4,INT(OPERND(I,8)))).GE.
     1                (1.0D35*DABS(RFDIFF(6,INT(OPERND(I,8)))))) THEN
                          IF((RFDIFF(4,INT(OPERND(I,8)))).GE.0.0D0) V1=PII/2.0D0
                          IF((RFDIFF(4,INT(OPERND(I,8)))).LT.0.0D0) V1=(3.0D0*PII)/2.0D0
                      ELSE
                          IF(DABS(RFDIFF(4,INT(OPERND(I,8)))).EQ.0.0D0.AND.
     1                    DABS(RFDIFF(6,INT(OPERND(I,8)))).EQ.0.0D0) THEN
                              V1=0.0D0
                          ELSE
                              V1=DATAN2(RFDIFF(4,INT(OPERND(I,8))),RFDIFF(6,INT(OPERND(I,8))))
                          END IF
                          IF((V1).LT.0.0D0) V1=V1+(TWOPII)
                      END IF
                      REG(9)=(V1-REFRY(11,INT(OPERND(I,8))))
                      IF(REG(9).EQ.TWOPII) REG(9)=0.0D0
                      IF(REG(9).LT.-PII) REG(9)=REG(9)+(TWOPII)
                      IF(REG(9).GT.PII) REG(9)=REG(9)-(TWOPII)
                      REG(9)=REG(9)/RFDELX
                  ELSE
                      REG(9)=0.0D0
                  END IF
                  GO TO 777
              END IF
              IF(OPERND(I,17).EQ.32.0D0) THEN
                  IF(OPDIF) THEN
                      IF(DABS(RFDIFF(10,INT(OPERND(I,8)))).GE.
     1                (1.0D35*DABS(RFDIFF(12,INT(OPERND(I,8)))))) THEN
                          IF((RFDIFF(10,INT(OPERND(I,8)))).GE.0.0D0) V1=PII/2.0D0
                          IF((RFDIFF(10,INT(OPERND(I,8)))).LT.0.0D0) V1=(3.0D0*PII)/2.0D0
                      ELSE
                          IF(DABS(RFDIFF(10,INT(OPERND(I,8)))).EQ.0.0D0.AND.
     1                    DABS(RFDIFF(12,INT(OPERND(I,8)))).EQ.0.0D0) THEN
                              V1=0.0D0
                          ELSE
                              V1=DATAN2(RFDIFF(10,INT(OPERND(I,8))),RFDIFF(12,INT(OPERND(I,8))))
                          END IF
                          IF((V1).LT.0.0D0) V1=V1+(TWOPII)
                      END IF
                      REG(9)=(V1-REFRY(11,INT(OPERND(I,8))))
                      IF(REG(9).EQ.TWOPII) REG(9)=0.0D0
                      IF(REG(9).LT.-PII) REG(9)=REG(9)+(TWOPII)
                      IF(REG(9).GT.PII) REG(9)=REG(9)-(TWOPII)
                      REG(9)=REG(9)/RFDELY
                  ELSE
                      REG(9)=0.0D0
                  END IF
                  GO TO 777
              END IF
              IF(OPERND(I,17).EQ.33.0D0) THEN
                  IF(OPDIF) THEN
                      IF(DABS(RFDIFF(5,INT(OPERND(I,8)))).GE.
     1                (1.0D35*DABS(RFDIFF(6,INT(OPERND(I,8)))))) THEN
                          IF((RFDIFF(5,INT(OPERND(I,8)))).GE.0.0D0) V1=PII/2.0D0
                          IF((RFDIFF(5,INT(OPERND(I,8)))).LT.0.0D0) V1=(3.0D0*PII)/2.0D0
                      ELSE
                          IF(DABS(RFDIFF(5,INT(OPERND(I,8)))).EQ.0.0D0.AND.
     1                    DABS(RFDIFF(6,INT(OPERND(I,8)))).EQ.0.0D0) THEN
                              V1=0.0D0
                          ELSE
                              V1=DATAN2(RFDIFF(5,INT(OPERND(I,8))),RFDIFF(6,INT(OPERND(I,8))))
                          END IF
                          IF((V1).LT.0.0D0) V1=V1+(TWOPII)
                      END IF
                      REG(9)=(V1-REFRY(12,INT(OPERND(I,8))))
                      IF(REG(9).EQ.TWOPII) REG(9)=0.0D0
                      IF(REG(9).LT.-PII) REG(9)=REG(9)+(TWOPII)
                      IF(REG(9).GT.PII) REG(9)=REG(9)-(TWOPII)
                      REG(9)=REG(9)/RFDELX
                  ELSE
                      REG(9)=0.0D0
                  END IF
                  GO TO 777
              END IF
              IF(OPERND(I,17).EQ.34.0D0) THEN
                  IF(OPDIF) THEN
                      IF(DABS(RFDIFF(11,INT(OPERND(I,8)))).GE.
     1                (1.0D35*DABS(RFDIFF(12,INT(OPERND(I,8)))))) THEN
                          IF((RFDIFF(11,INT(OPERND(I,8)))).GE.0.0D0) V1=PII/2.0D0
                          IF((RFDIFF(11,INT(OPERND(I,8)))).LT.0.0D0) V1=(3.0D0*PII)/2.0D0
                      ELSE
                          IF(DABS(RFDIFF(11,INT(OPERND(I,8)))).EQ.0.0D0.AND.
     1                    DABS(RFDIFF(12,INT(OPERND(I,8)))).EQ.0.0D0) THEN
                              V1=0.0D0
                          ELSE
                              V1=DATAN2(RFDIFF(11,INT(OPERND(I,8))),RFDIFF(12,INT(OPERND(I,8))))
                          END IF
                          IF((V1).LT.0.0D0) V1=V1+(TWOPII)
                      END IF
                      REG(9)=(V1-REFRY(12,INT(OPERND(I,8))))
                      IF(REG(9).EQ.TWOPII) REG(9)=0.0D0
                      IF(REG(9).LT.-PII) REG(9)=REG(9)+(TWOPII)
                      IF(REG(9).GT.PII) REG(9)=REG(9)-(TWOPII)
                      REG(9)=REG(9)/RFDELY
                  ELSE
                      REG(9)=0.0D0
                  END IF
                  GO TO 777
              END IF
C
              IF(OPERND(I,17).EQ.35.0D0) THEN
                  IF(OPDIF) THEN
                      REG(9)=
     1                (DIFF(1,INT(OPERND(I,8)))-RAYRAY(1,INT(OPERND(I,8))))/DELX
                  ELSE
                      REG(9)=0.0D0
                  END IF
                  GO TO 777
              END IF
              IF(OPERND(I,17).EQ.36.0D0) THEN
                  IF(OPDIF) THEN
                      REG(9)=
     1                (DIFF(7,INT(OPERND(I,8)))-RAYRAY(1,INT(OPERND(I,8))))/DELY
                  ELSE
                      REG(9)=0.0D0
                  END IF
                  GO TO 777
              END IF
              IF(OPERND(I,17).EQ.37.0D0) THEN
                  IF(OPDIF) THEN
                      REG(9)=
     1                (DIFF(2,INT(OPERND(I,8)))-RAYRAY(2,INT(OPERND(I,8))))/DELX
                  ELSE
                      REG(9)=0.0D0
                  END IF
                  GO TO 777
              END IF
              IF(OPERND(I,17).EQ.38.0D0) THEN
                  IF(OPDIF) THEN
                      REG(9)=
     1                (DIFF(8,INT(OPERND(I,8)))-RAYRAY(2,INT(OPERND(I,8))))/DELY
                  ELSE
                      REG(9)=0.0D0
                  END IF
                  GO TO 777
              END IF
C
              IF(OPERND(I,17).EQ.39.0D0) THEN
                  IF(OPDIF) THEN
                      IF(DABS(DIFF(4,INT(OPERND(I,8)))).GE.
     1                (1.0D35*DABS(DIFF(6,INT(OPERND(I,8)))))) THEN
                          IF((DIFF(4,INT(OPERND(I,8)))).GE.0.0D0) V1=PII/2.0D0
                          IF((DIFF(4,INT(OPERND(I,8)))).LT.0.0D0) V1=(3.0D0*PII)/2.0D0
                      ELSE
                          IF(DABS(DIFF(4,INT(OPERND(I,8)))).EQ.0.0D0.AND.
     1                    DABS(DIFF(6,INT(OPERND(I,8)))).EQ.0.0D0) THEN
                              V1=0.0D0
                          ELSE
                              V1=DATAN2(DIFF(4,INT(OPERND(I,8))),DIFF(6,INT(OPERND(I,8))))
                          END IF
                          IF((V1).LT.0.0D0) V1=V1+(TWOPII)
                      END IF
                      REG(9)=(V1-RAYRAY(11,INT(OPERND(I,8))))
                      IF(REG(9).EQ.TWOPII) REG(9)=0.0D0
                      IF(REG(9).LT.-PII) REG(9)=REG(9)+(TWOPII)
                      IF(REG(9).GT.PII) REG(9)=REG(9)-(TWOPII)
                      REG(9)=REG(9)/DELX
                  ELSE
                      REG(9)=0.0D0
                  END IF
                  GO TO 777
              END IF
              IF(OPERND(I,17).EQ.40.0D0) THEN
                  IF(OPDIF) THEN
                      IF(DABS(DIFF(10,INT(OPERND(I,8)))).GE.
     1                (1.0D35*DABS(DIFF(12,INT(OPERND(I,8)))))) THEN
                          IF((DIFF(10,INT(OPERND(I,8)))).GE.0.0D0) V1=PII/2.0D0
                          IF((DIFF(10,INT(OPERND(I,8)))).LT.0.0D0) V1=(3.0D0*PII)/2.0D0
                      ELSE
                          IF(DABS(DIFF(10,INT(OPERND(I,8)))).EQ.0.0D0.AND.
     1                    DABS(DIFF(12,INT(OPERND(I,8)))).EQ.0.0D0) THEN
                              V1=0.0D0
                          ELSE
                              V1=DATAN2(DIFF(10,INT(OPERND(I,8))),DIFF(12,INT(OPERND(I,8))))
                          END IF
                          IF((V1).LT.0.0D0) V1=V1+(TWOPII)
                      END IF
                      REG(9)=(V1-RAYRAY(11,INT(OPERND(I,8))))
                      IF(REG(9).EQ.TWOPII) REG(9)=0.0D0
                      IF(REG(9).LT.-PII) REG(9)=REG(9)+(TWOPII)
                      IF(REG(9).GT.PII) REG(9)=REG(9)-(TWOPII)
                      REG(9)=REG(9)/DELY
                  ELSE
                      REG(9)=0.0D0
                  END IF
                  GO TO 777
              END IF
              IF(OPERND(I,17).EQ.41.0D0) THEN
                  IF(OPDIF) THEN
                      IF(DABS(DIFF(5,INT(OPERND(I,8)))).GE.
     1                (1.0D35*DABS(DIFF(6,INT(OPERND(I,8)))))) THEN
                          IF((DIFF(5,INT(OPERND(I,8)))).GE.0.0D0) V1=PII/2.0D0
                          IF((DIFF(5,INT(OPERND(I,8)))).LT.0.0D0) V1=(3.0D0*PII)/2.0D0
                      ELSE
                          IF(DABS(DIFF(5,INT(OPERND(I,8)))).EQ.0.0D0.AND.
     1                    DABS(DIFF(6,INT(OPERND(I,8)))).EQ.0.0D0) THEN
                              V1=0.0D0
                          ELSE
                              V1=DATAN2(DIFF(5,INT(OPERND(I,8))),DIFF(6,INT(OPERND(I,8))))
                          END IF
                          IF((V1).LT.0.0D0) V1=V1+(TWOPII)
                      END IF
                      REG(9)=(V1-RAYRAY(12,INT(OPERND(I,8))))
                      IF(REG(9).EQ.TWOPII) REG(9)=0.0D0
                      IF(REG(9).LT.-PII) REG(9)=REG(9)+(TWOPII)
                      IF(REG(9).GT.PII) REG(9)=REG(9)-(TWOPII)
                      REG(9)=REG(9)/DELX
                  ELSE
                      REG(9)=0.0D0
                  END IF
                  GO TO 777
              END IF
              IF(OPERND(I,17).EQ.42.0D0) THEN
                  IF(OPDIF) THEN
                      IF(DABS(DIFF(11,INT(OPERND(I,8)))).GE.
     1                (1.0D35*DABS(DIFF(12,INT(OPERND(I,8)))))) THEN
                          IF((DIFF(11,INT(OPERND(I,8)))).GE.0.0D0) V1=PII/2.0D0
                          IF((DIFF(11,INT(OPERND(I,8)))).LT.0.0D0) V1=(3.0D0*PII)/2.0D0
                      ELSE
                          IF(DABS(DIFF(11,INT(OPERND(I,8)))).EQ.0.0D0.AND.
     1                    DABS(DIFF(12,INT(OPERND(I,8)))).EQ.0.0D0) THEN
                              V1=0.0D0
                          ELSE
                              V1=DATAN2(DIFF(11,INT(OPERND(I,8))),DIFF(12,INT(OPERND(I,8))))
                          END IF
                          IF((V1).LT.0.0D0) V1=V1+(TWOPII)
                      END IF
                      REG(9)=(V1-RAYRAY(12,INT(OPERND(I,8))))
                      IF(REG(9).LT.-PII) REG(9)=REG(9)+(TWOPII)
                      IF(REG(9).GT.PII) REG(9)=REG(9)-(TWOPII)
                      IF(REG(9).EQ.TWOPII) REG(9)=0.0D0
                      REG(9)=REG(9)/DELY
                  ELSE
                      REG(9)=0.0D0
                  END IF
                  GO TO 777
              END IF
              IF(OPERND(I,17).EQ.43.0D0) THEN
                  REG(9)=
     1            REFRY(1,INT(OPERND(I,8)))
                  GO TO 777
              END IF
              IF(OPERND(I,17).EQ.44.0D0) THEN
                  REG(9)=
     1            REFRY(2,INT(OPERND(I,8)))
                  GO TO 777
              END IF
              IF(OPERND(I,17).EQ.45.0D0) THEN
                  REG(9)=
     1            REFRY(3,INT(OPERND(I,8)))
                  GO TO 777
              END IF
              IF(OPERND(I,17).EQ.46.0D0) THEN
                  REG(9)=
     1            REFRY(4,INT(OPERND(I,8)))
                  GO TO 777
              END IF
              IF(OPERND(I,17).EQ.47.0D0) THEN
                  REG(9)=
     1            REFRY(5,INT(OPERND(I,8)))
                  GO TO 777
              END IF
              IF(OPERND(I,17).EQ.48.0D0) THEN
                  REG(9)=
     1            REFRY(6,INT(OPERND(I,8)))
                  GO TO 777
              END IF
              IF(OPERND(I,17).EQ.49.0D0) THEN
                  REG(9)=
     1            REFRY(19,INT(OPERND(I,8)))
                  GO TO 777
              END IF
              IF(OPERND(I,17).EQ.50.0D0) THEN
                  REG(9)=
     1            REFRY(20,INT(OPERND(I,8)))
                  GO TO 777
              END IF
              IF(OPERND(I,17).EQ.51.0D0) THEN
                  REG(9)=
     1            REFRY(21,INT(OPERND(I,8)))
                  GO TO 777
              END IF
              IF(OPERND(I,17).EQ.52.0D0) THEN
                  REG(9)=
     1            REFRY(9,INT(OPERND(I,8)))
                  GO TO 777
              END IF
              IF(OPERND(I,17).EQ.53.0D0) THEN
                  REG(9)=
     1            REFRY(10,INT(OPERND(I,8)))
                  GO TO 777
              END IF
              IF(OPERND(I,17).EQ.54.0D0) THEN
                  REG(9)=REFRY(11,INT(OPERND(I,8)))
                  IF(REG(9).LT.-PII) REG(9)=REG(9)+(TWOPII)
                  IF(REG(9).GT.PII) REG(9)=REG(9)-(TWOPII)
                  IF(REG(9).EQ.TWOPII) REG(9)=0.0D0
                  GO TO 777
              END IF
              IF(OPERND(I,17).EQ.55.0D0) THEN
                  REG(9)=REFRY(12,INT(OPERND(I,8)))
                  IF(REG(9).LT.-PII) REG(9)=REG(9)+(TWOPII)
                  IF(REG(9).GT.PII) REG(9)=REG(9)-(TWOPII)
                  IF(REG(9).EQ.TWOPII) REG(9)=0.0D0
                  GO TO 777
              END IF
              IF(OPERND(I,17).EQ.56.0D0) THEN
                  REG(9)=
     1            REFRY(13,INT(OPERND(I,8)))
                  GO TO 777
              END IF
              IF(OPERND(I,17).EQ.57.0D0) THEN
                  REG(9)=
     1            REFRY(14,INT(OPERND(I,8)))
                  GO TO 777
              END IF
              IF(OPERND(I,17).EQ.58.0D0) THEN
                  REG(9)=
     1            REFRY(15,INT(OPERND(I,8)))
                  GO TO 777
              END IF
C
              IF(OPERND(I,17).GE.59.0D0.AND.OPERND(I,17).LE.67.0D0) THEN
                  IF(.NOT.GLOBE) THEN
                      IF(F28.EQ.1) REG(9)=0.0D0
                      IF(F28.EQ.1) BAAD=1
                      IF(F31.EQ.1) CALL MACFAL
                      RETURN
                  END IF
              END IF
              IF(OPERND(I,17).EQ.59.0D0) THEN
                  REG(9)=GLRAY(1,INT(OPERND(I,8)))
                  GO TO 777
              END IF
              IF(OPERND(I,17).EQ.60.0D0) THEN
                  REG(9)=GLRAY(2,INT(OPERND(I,8)))
                  GO TO 777
              END IF
              IF(OPERND(I,17).EQ.61.0D0) THEN
                  REG(9)=GLRAY(3,INT(OPERND(I,8)))
                  GO TO 777
              END IF
              IF(OPERND(I,17).EQ.62.0D0) THEN
                  REG(9)=GLRAY(4,INT(OPERND(I,8)))
                  GO TO 777
              END IF
              IF(OPERND(I,17).EQ.63.0D0) THEN
                  REG(9)=GLRAY(5,INT(OPERND(I,8)))
                  GO TO 777
              END IF
              IF(OPERND(I,17).EQ.64.0D0) THEN
                  REG(9)=GLRAY(6,INT(OPERND(I,8)))
                  GO TO 777
              END IF
              IF(OPERND(I,17).EQ.65.0D0) THEN
                  REG(9)=GLRAY(7,INT(OPERND(I,8)))
                  GO TO 777
              END IF
              IF(OPERND(I,17).EQ.66.0D0) THEN
                  REG(9)=GLRAY(8,INT(OPERND(I,8)))
                  GO TO 777
              END IF
              IF(OPERND(I,17).EQ.67.0D0) THEN
                  REG(9)=GLRAY(9,INT(OPERND(I,8)))
                  GO TO 777
              END IF
C
              IF(OPERND(I,17).EQ.68.0D0) THEN
                  REG(9)=
     1            REFRY(8,INT(OPERND(I,8)))
                  GO TO 777
              END IF
              IF(OPERND(I,17).EQ.69.0D0) THEN
                  REG(9)=
     1            REFRY(7,INT(OPERND(I,8)))
                  GO TO 777
              END IF
          END IF
C
C     CLEARX OPERAND
          IF(ONUM.EQ.514) THEN
C     CALCULATE VALUE1 AND PLACE IN ACC
              CLFOB1=DABS(DBLE(INT(OPERND(I,8))))
              CLRAY1=DBLE(NINT(DABS(DABS(OPERND(I,8))-
     1        DBLE(INT(DABS(OPERND(I,8)))))*10000.0D0))
              CLFOB2=DABS(DBLE(INT(OPERND(I,9))))
              CLRAY2=DBLE(NINT(DABS(DABS(OPERND(I,9))-
     1        DBLE(INT(DABS(OPERND(I,9)))))*10000.0D0))
              WRITE(OUTLYNE,*) OPERND(I,10)
              CALL SHOWIT(1)
              CLSRF1=DABS(DBLE(INT(OPERND(I,10))))
              AAA=DABS(OPERND(I,10))
              CLSRF2=AAA-CLSRF1
              CLSRF2=DBLE(NINT(CLSRF2*1000.0D0))
C
              CLRTYP=1
              CALL CLEARANCE(CLEAR,CLRTYP)
              REG(9)=CLEAR
              GO TO 777
          END IF
C
C     CLEARY OPERAND
          IF(ONUM.EQ.515) THEN
C     CALCULATE VALUE1 AND PLACE IN ACC
              CLFOB1=DABS(DBLE(INT(OPERND(I,8))))
              CLRAY1=DBLE(NINT(DABS(DABS(OPERND(I,8))-
     1        DBLE(INT(DABS(OPERND(I,8)))))*10000.0D0))
              CLFOB2=DABS(DBLE(INT(OPERND(I,9))))
              CLRAY2=DBLE(NINT(DABS(DABS(OPERND(I,9))-
     1        DBLE(INT(DABS(OPERND(I,9)))))*10000.0D0))
              CLSRF1=DABS(DBLE(INT(OPERND(I,10))))
              AAA=DABS(OPERND(I,10))
              CLSRF2=AAA-CLSRF1
              CLSRF2=DBLE(NINT(CLSRF2*1000.0D0))
C
              CLRTYP=2
              CALL CLEARANCE(CLEAR,CLRTYP)
              REG(9)=CLEAR
              GO TO 777
          END IF
C     CLEARY OPERAND, TRACE TWO RAYS AND CALCULATE THE OPERAND VALUE1
          IF(ONUM.EQ.515) THEN
C     CALCULATE VALUE1 AND PLACE IN ACC
C
              REG(9)=2702.0D0
              GO TO 777
          END IF
          IF(OPERND(I,17).GE.468.0D0.AND.OPERND(I,17).LE.471.0D0) THEN
C     SYMX,SYMY,ASYMX OR ASYMY. JUST TRACE THE RAYS AND CALC THE VALUE1S
C     OPERND(I,8) IS FRACTIONAL RAY HEIGHT
C     OPERND(I,9) IS FIELD NUMBER
C     OPERND(I,10) IS WAVELENGTH NUMBER FOR RAY, NOT REF RAY
C     DO THE FOB
              SAVE_KDP(1)=SAVEINPT(1)
              OLDLDIF2=LDIF2
              OLDLDIF=LDIF
              IF(OPDIF) THEN
                  LDIF2=.TRUE.
                  LDIF=.TRUE.
              ELSE
                  LDIF2=.FALSE.
                  LDIF=.FALSE.
              END IF
              WC='FOB     '
              WQ='        '
              SQ=0
              SST=0
              STI=0
              W1=FIELDY(INT(OPERND(I,9)))
              W2=FIELDX(INT(OPERND(I,9)))
              W3=FIELDZ(INT(OPERND(I,9)))
              W4=FIELDW(INT(OPERND(I,9)))
              W5=0.0D0
              DF1=0
              DF2=0
              DF3=0
              DF4=0
              DF5=1
              S1=1
              S2=1
              S3=1
              S4=1
              S5=0
              SN=1
C     SET MSG TO FALSE
              MSG=.FALSE.
              BADOPS=.FALSE.
              CALL FFOB2
              REST_KDP(1)=RESTINPT(1)
              IF(BADOPS) THEN
                  IF(F28.EQ.1) BAAD=1
                  IF(F28.EQ.1) REG(9)=0.0D0
                  IF(F31.EQ.1) CALL MACFAL
                  RETURN
              END IF
C NOW THE RAY 0 0
              SAVE_KDP(1)=SAVEINPT(1)
              WC='RAY     '
              WQ='        '
              SQ=0
              SST=0
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
C     SET MSG TO FALSE
              MSG=.FALSE.
              BADOPS=.FALSE.
              NOCOAT=.TRUE.
              GRASET=.FALSE.
              DXFSET=.FALSE.
              CALL RRAY2
              CACOCH=0
              REST_KDP(1)=RESTINPT(1)
              IF(BADOPS) THEN
                  IF(F28.EQ.1) BAAD=1
                  IF(F28.EQ.1) REG(9)=0.0D0
                  IF(F31.EQ.1) CALL MACFAL
                  RETURN
              END IF
C     NOW THE RAY PAIR
              IF(OPERND(I,17).EQ.468.0D0.OR.OPERND(I,17).EQ.470.0D0) THEN
C     XZ-PLANE
C NOW THE RAY #1
                  VXLO=-1.0D0
                  VXHI=1.0D0
                  IF(LVIG) CALL VIGCAL(100,VXLO,VXHI,1)
                  SAVE_KDP(1)=SAVEINPT(1)
                  WC='RAY     '
                  WQ='        '
                  SQ=0
                  SST=0
                  DF1=0
                  DF2=0
                  DF3=0
                  DF4=1
                  DF5=1
                  S1=1
                  S2=1
                  S3=1
                  S4=0
                  S5=0
                  SN=1
                  W1=0.0D0
                  W2=OPERND(I,8)*DABS(VXHI)
                  W3=OPERND(I,10)
                  W4=0.0D0
                  W5=0.0D0
C     SET MSG TO FALSE
                  MSG=.FALSE.
                  BADOPS=.FALSE.
                  NOCOAT=.TRUE.
                  GRASET=.FALSE.
                  DXFSET=.FALSE.
                  CALL RRAY2
                  CACOCH=0
                  REST_KDP(1)=RESTINPT(1)
                  IF(BADOPS) THEN
                      IF(F28.EQ.1) BAAD=1
                      IF(F28.EQ.1) REG(9)=0.0D0
                      IF(F31.EQ.1) CALL MACFAL
                      RETURN
                  END IF
                  LDIF2=OLDLDIF2
                  LDIF=OLDLDIF
                  IF(.NOT.REFEXT.OR..NOT.RAYEXT) THEN
                      IF(F28.EQ.1) BAAD=1
                      IF(F28.EQ.1) REG(9)=0.0D0
                      IF(F31.EQ.1) CALL MACFAL
                      RETURN
                  END IF
                  IF(SYSTEM1(30).LE.2.0D0)
     1            VALUE1=RAYRAY(1,NEWIMG)-REFRY(1,NEWIMG)
                  IF(SYSTEM1(30).GE.3.0D0) THEN
                      VALUE1=RAYRAY(11,NEWIMG)
     1                -REFRY(11,NEWIMG)
                      IF(VALUE1.LT.-PII) VALUE1=VALUE1+(TWOPII)
                      IF(VALUE1.GT.PII) VALUE1=VALUE1-(TWOPII)
                      IF(VALUE1.EQ.TWOPII) VALUE1=0.0D0
                  END IF
                  REG(9)=VALUE1
C NOW THE RAY #2
                  SAVE_KDP(1)=SAVEINPT(1)
                  WC='RAY     '
                  WQ='        '
                  SQ=0
                  SST=0
                  DF1=0
                  DF2=0
                  DF3=0
                  DF4=1
                  DF5=1
                  S1=1
                  S2=1
                  S3=1
                  S4=0
                  S5=0
                  SN=1
                  W1=0.0D0
                  W2=OPERND(I,8)*DABS(VXLO)
                  W3=OPERND(I,10)
                  W4=0.0D0
                  W5=0.0D0
C     SET MSG TO FALSE
                  MSG=.FALSE.
                  BADOPS=.FALSE.
                  NOCOAT=.TRUE.
                  GRASET=.FALSE.
                  DXFSET=.FALSE.
                  CALL RRAY2
                  CACOCH=0
                  REST_KDP(1)=RESTINPT(1)
                  IF(BADOPS) THEN
                      IF(F28.EQ.1) BAAD=1
                      IF(F28.EQ.1) REG(9)=0.0D0
                      IF(F31.EQ.1) CALL MACFAL
                      RETURN
                  END IF
                  LDIF2=OLDLDIF2
                  LDIF=OLDLDIF
                  IF(.NOT.REFEXT.OR..NOT.RAYEXT) THEN
                      IF(F28.EQ.1) REG(9)=0.0D0
                      IF(F28.EQ.1) BAAD=1
                      IF(F31.EQ.1) CALL MACFAL
                      RETURN
                  END IF
                  IF(SYSTEM1(30).LE.2.0D0)
     1            VALUE1=RAYRAY(1,NEWIMG)-REFRY(1,NEWIMG)
                  IF(SYSTEM1(30).GE.3.0D0) THEN
                      VALUE1=RAYRAY(11,NEWIMG)
     1                -REFRY(11,NEWIMG)
                      IF(VALUE1.LT.-PII) VALUE1=VALUE1+(TWOPII)
                      IF(VALUE1.GT.PII) VALUE1=VALUE1-(TWOPII)
                      IF(VALUE1.EQ.TWOPII) VALUE1=0.0D0
                  END IF
                  IF(OPERND(I,17).EQ.468.0D0)
     1            REG(9)=(REG(9)-VALUE1)/2.0D0
                  IF(OPERND(I,17).EQ.470.0D0)
     1            REG(9)=(REG(9)+VALUE1)/2.0D0
                  GO TO 777
              END IF
              IF(OPERND(I,17).EQ.469.0D0.OR.OPERND(I,17).EQ.471.0D0) THEN
C     YZ-PLANE
C NOW THE RAY #1
                  VYLO=-1.0D0
                  VYHI=1.0D0
                  IF(LVIG) CALL VIGCAL(100,VYLO,VYHI,2)
                  SAVE_KDP(1)=SAVEINPT(1)
                  WC='RAY     '
                  WQ='        '
                  SQ=0
                  SST=0
                  DF1=0
                  DF2=0
                  DF3=0
                  DF4=1
                  DF5=1
                  S1=1
                  S2=1
                  S3=1
                  S4=0
                  S5=0
                  SN=1
                  W1=OPERND(I,8)*DABS(VYHI)
                  W2=0.0D0
                  W3=OPERND(I,10)
                  W4=0.0D0
                  W5=0.0D0
C     SET MSG TO FALSE
                  MSG=.FALSE.
                  BADOPS=.FALSE.
                  NOCOAT=.TRUE.
                  GRASET=.FALSE.
                  DXFSET=.FALSE.
                  CALL RRAY2
                  CACOCH=0
                  REST_KDP(1)=RESTINPT(1)
                  IF(BADOPS) THEN
                      IF(F28.EQ.1) BAAD=1
                      IF(F28.EQ.1) REG(9)=0.0D0
                      IF(F31.EQ.1) CALL MACFAL
                      RETURN
                  END IF
                  LDIF2=OLDLDIF2
                  LDIF=OLDLDIF
                  IF(.NOT.REFEXT.OR..NOT.RAYEXT) THEN
                      IF(F28.EQ.1) REG(9)=0.0D0
                      IF(F28.EQ.1) BAAD=1
                      IF(F31.EQ.1) CALL MACFAL
                      RETURN
                  END IF
                  IF(SYSTEM1(30).LE.2.0D0)
     1            VALUE1=RAYRAY(2,NEWIMG)-REFRY(2,NEWIMG)
                  IF(SYSTEM1(30).GE.3.0D0) THEN
                      VALUE1=RAYRAY(12,NEWIMG)
     1                -REFRY(12,NEWIMG)
                      IF(VALUE1.LT.-PII) VALUE1=VALUE1+(TWOPII)
                      IF(VALUE1.GT.PII) VALUE1=VALUE1-(TWOPII)
                      IF(VALUE1.EQ.TWOPII) VALUE1=0.0D0
                  END IF
                  REG(9)=VALUE1
C NOW THE RAY #2
                  SAVE_KDP(1)=SAVEINPT(1)
                  WC='RAY     '
                  WQ='        '
                  SQ=0
                  SST=0
                  DF1=0
                  DF2=0
                  DF3=0
                  DF4=1
                  DF5=1
                  S1=1
                  S2=1
                  S3=1
                  S4=0
                  S5=0
                  SN=1
                  W1=OPERND(I,8)*DABS(VYLO)
                  W2=0.0D0
                  W3=OPERND(I,10)
                  W4=0.0D0
                  W5=0.0D0
C     SET MSG TO FALSE
                  MSG=.FALSE.
                  BADOPS=.FALSE.
                  NOCOAT=.TRUE.
                  GRASET=.FALSE.
                  DXFSET=.FALSE.
                  CALL RRAY2
                  CACOCH=0
                  REST_KDP(1)=RESTINPT(1)
                  IF(BADOPS) THEN
                      IF(F28.EQ.1) BAAD=1
                      IF(F28.EQ.1) REG(9)=0.0D0
                      IF(F31.EQ.1) CALL MACFAL
                      RETURN
                  END IF
                  LDIF2=OLDLDIF2
                  LDIF=OLDLDIF
                  IF(.NOT.REFEXT.OR..NOT.RAYEXT) THEN
                      IF(F28.EQ.1) REG(9)=0.0D0
                      IF(F28.EQ.1) BAAD=1
                      IF(F31.EQ.1) CALL MACFAL
                      RETURN
                  END IF
                  IF(SYSTEM1(30).LE.2.0D0)
     1            VALUE1=RAYRAY(2,NEWIMG)-REFRY(2,NEWIMG)
                  IF(SYSTEM1(30).GE.3.0D0) THEN
                      VALUE1=RAYRAY(12,NEWIMG)
     1                -REFRY(12,NEWIMG)
                      IF(VALUE1.LT.-PII) VALUE1=VALUE1+(TWOPII)
                      IF(VALUE1.GT.PII) VALUE1=VALUE1-(TWOPII)
                      IF(VALUE1.EQ.TWOPII) VALUE1=0.0D0
                  END IF
                  IF(OPERND(I,17).EQ.469.0D0)
     1            REG(9)=(REG(9)-VALUE1)/2.0D0
                  IF(OPERND(I,17).EQ.471.0D0)
     1            REG(9)=(REG(9)+VALUE1)/2.0D0
                  GO TO 777
              END IF
          END IF
C     REAL RAY CHROMATICS
          IF(OPERND(I,17).GE.472.0D0.AND.OPERND(I,17).LE.479.0D0.OR.
     1    OPERND(I,17).EQ.510.0D0) THEN
              ERRR=.FALSE.
              IF(OPERND(I,17).NE.510.0D0) THEN
                  IF(OPERND(I,17).EQ.472.0D0) CACOCH=0
                  IF(OPERND(I,17).EQ.473.0D0) CACOCH=0
                  IF(OPERND(I,17).EQ.474.0D0) CACOCH=0
                  IF(OPERND(I,17).EQ.475.0D0) CACOCH=0
                  IF(OPERND(I,17).EQ.476.0D0) CACOCH=0
                  IF(OPERND(I,17).EQ.477.0D0) CACOCH=0
                  IF(OPERND(I,17).EQ.478.0D0) CACOCH=0
                  IF(OPERND(I,17).EQ.479.0D0) CACOCH=0
                  IF(OPERND(I,17).EQ.472.0D0) CALL REALCOLR(1,ERRR)
                  IF(OPERND(I,17).EQ.473.0D0) CALL REALCOLR(2,ERRR)
                  IF(OPERND(I,17).EQ.474.0D0) CALL REALCOLR(3,ERRR)
                  IF(OPERND(I,17).EQ.475.0D0) CALL REALCOLR(4,ERRR)
                  IF(OPERND(I,17).EQ.476.0D0) CALL REALCOLR(5,ERRR)
                  IF(OPERND(I,17).EQ.477.0D0) CALL REALCOLR(6,ERRR)
                  IF(OPERND(I,17).EQ.478.0D0) CALL REALCOLR(7,ERRR)
                  IF(OPERND(I,17).EQ.479.0D0) CALL REALCOLR(8,ERRR)
                  IF(ERRR) THEN
                      REG(9)=0.0D0
                      CALL MACFAL
                      RETURN
                  END IF
              ELSE
C     CALCULATE CONRADY D-d OPERAND
C     CALC D-d
C     DO THE FOB
                  SAVE_KDP(1)=SAVEINPT(1)
                  OLDLDIF2=LDIF2
                  OLDLDIF=LDIF
                  IF(OPDIF) THEN
                      LDIF2=.TRUE.
                      LDIF=.TRUE.
                  ELSE
                      LDIF2=.FALSE.
                      LDIF=.FALSE.
                  END IF
                  WC='FOB     '
                  WQ='        '
                  SQ=0
                  SST=0
                  STI=0
                  W1=0.0D0
                  W2=0.0D0
                  W3=0.0D0
                  W4=SYSTEM1(11)
                  W5=0.0D0
                  DF1=0
                  DF2=0
                  DF3=0
                  DF4=0
                  DF5=1
                  S1=1
                  S2=1
                  S3=1
                  S4=1
                  S5=0
                  SN=1
C     SET MSG TO FALSE
                  MSG=.FALSE.
                  BADOPS=.FALSE.
                  CALL FFOB2
                  REST_KDP(1)=RESTINPT(1)
                  IF(BADOPS) THEN
                      IF(F28.EQ.1) BAAD=1
                      IF(F28.EQ.1) REG(9)=0.0D0
                      IF(F31.EQ.1) CALL MACFAL
                      RETURN
                  END IF
C NOW THE RAY 0.7 0
                  SAVE_KDP(1)=SAVEINPT(1)
                  WC='RAY     '
                  WQ='        '
                  SQ=0
                  SST=0
                  DF1=0
                  DF2=1
                  DF3=0
                  DF4=1
                  DF5=1
                  S1=1
                  S2=0
                  S3=1
                  S4=0
                  S5=0
                  SN=1
                  W1=0.7D0
                  W3=SYSTEM1(11)
C     SET MSG TO FALSE
                  MSG=.FALSE.
                  BADOPS=.FALSE.
                  NOCOAT=.TRUE.
                  GRASET=.FALSE.
                  DXFSET=.FALSE.
                  CALL RRAY2
                  CACOCH=0
                  REST_KDP(1)=RESTINPT(1)
                  IF(BADOPS) THEN
                      IF(F28.EQ.1) BAAD=1
                      IF(F28.EQ.1) REG(9)=0.0D0
                      IF(F31.EQ.1) CALL MACFAL
                      RETURN
                  END IF
                  CONSUM=0.0D0
                  DO K=1,INT(SYSTEM1(20))
                      DELLELL1=0.0D0
                      DELLELL2=0.0D0
                      DELLELL= 0.0D0
                      IF(SYSTEM1(7).EQ.1.0D0)  DELLELL1=ALENS(46,K)
                      IF(SYSTEM1(7).EQ.2.0D0)  DELLELL1=ALENS(47,K)
                      IF(SYSTEM1(7).EQ.3.0D0)  DELLELL1=ALENS(48,K)
                      IF(SYSTEM1(7).EQ.4.0D0)  DELLELL1=ALENS(49,K)
                      IF(SYSTEM1(7).EQ.5.0D0)  DELLELL1=ALENS(50,K)
                      IF(SYSTEM1(7).EQ.6.0D0)  DELLELL1=ALENS(71,K)
                      IF(SYSTEM1(7).EQ.7.0D0)  DELLELL1=ALENS(72,K)
                      IF(SYSTEM1(7).EQ.8.0D0)  DELLELL1=ALENS(73,K)
                      IF(SYSTEM1(7).EQ.9.0D0)  DELLELL1=ALENS(74,K)
                      IF(SYSTEM1(7).EQ.10.0D0) DELLELL1=ALENS(75,K)
                      IF(SYSTEM1(8).EQ.1.0D0)  DELLELL2=ALENS(46,K)
                      IF(SYSTEM1(8).EQ.2.0D0)  DELLELL2=ALENS(47,K)
                      IF(SYSTEM1(8).EQ.3.0D0)  DELLELL2=ALENS(48,K)
                      IF(SYSTEM1(8).EQ.4.0D0)  DELLELL2=ALENS(49,K)
                      IF(SYSTEM1(8).EQ.5.0D0)  DELLELL2=ALENS(50,K)
                      IF(SYSTEM1(8).EQ.6.0D0)  DELLELL2=ALENS(71,K)
                      IF(SYSTEM1(8).EQ.7.0D0)  DELLELL2=ALENS(72,K)
                      IF(SYSTEM1(8).EQ.8.0D0)  DELLELL2=ALENS(73,K)
                      IF(SYSTEM1(8).EQ.9.0D0)  DELLELL2=ALENS(74,K)
                      IF(SYSTEM1(8).EQ.10.0D0) DELLELL2=ALENS(75,K)
                      DELLELL=(DELLELL1-DELLELL2)
                      IF(DELLELL.NE.0.0D0)
     1                CONSUM=CONSUM+((RAYRAY(8,K)-ALENS(3,K-1))*DELLELL)
                  END DO
                  VALUE1=CONSUM
              END IF
              REG(9)=VALUE1
              GO TO 777
          END IF
          IF(OPERND(I,17).GE.70.0D0.AND.OPERND(I,17).LE.206.OR.
     1    OPERND(I,17).GE.485.0D0.AND.OPERND(I,17).LE.509.0D0.OR.
     1    OPERND(I,17).EQ.511.0D0) THEN
C     LENS DATABASE STUFF
              IF(OPERND(I,17).EQ.485.0D0) THEN
C     PIVX
                  REG(9)=ALENS(78,INT(OPERND(I,8)))
                  GO TO 777
              END IF
              IF(OPERND(I,17).EQ.486.0D0) THEN
C     PIVY
                  REG(9)=ALENS(79,INT(OPERND(I,8)))
                  GO TO 777
              END IF
              IF(OPERND(I,17).EQ.487.0D0) THEN
C     PIVZ
                  REG(9)=ALENS(80,INT(OPERND(I,8)))
                  GO TO 777
              END IF
              IF(OPERND(I,17).GE.94.0D0.AND.OPERND(I,17).LE.105.0D0) THEN
C     GLOBAL VERTEX DATA, IF NOT GLOBAL THEN OPERANDS NOT CALC. ELSE
C     CALC VALUE1 AND PROCEED
                  SAVE_KDP(31)=SAVEINPT(31)
                  WRITE(INPUT,*)'GLOBAL,',OPERND(I,9)
                  CALL PROCES
                  REST_KDP(31)=RESTINPT(31)
                  IF(.NOT.GLOBE) THEN
                      IF(F28.EQ.1) REG(9)=0.0D0
                      IF(F28.EQ.1) BAAD=1
                      IF(F31.EQ.1) CALL MACFAL
                      RETURN
                  END IF
C     CALCULATIONS FOR VERTEX STUFF GO HERE
                  IF(OPERND(I,17).EQ.94.0D0) THEN
                      REG(9)=
     1                VERTEX(1,INT(OPERND(I,8)))
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.95.0D0) THEN
                      REG(9)=
     1                VERTEX(2,INT(OPERND(I,8)))
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.96.0D0) THEN
                      REG(9)=
     1                VERTEX(3,INT(OPERND(I,8)))
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.97.0D0) THEN
                      REG(9)=
     1                VERTEX(4,INT(OPERND(I,8)))
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.98.0D0) THEN
                      REG(9)=
     1                VERTEX(5,INT(OPERND(I,8)))
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.99.0D0) THEN
                      REG(9)=
     1                VERTEX(6,INT(OPERND(I,8)))
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.100.0D0) THEN
                      REG(9)=
     1                VERTEX(7,INT(OPERND(I,8)))
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.101.0D0) THEN
                      REG(9)=
     1                VERTEX(8,INT(OPERND(I,8)))
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.102.0D0) THEN
                      REG(9)=
     1                VERTEX(9,INT(OPERND(I,8)))
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.103.0D0) THEN
                      REG(9)=
     1                VERTEX(10,INT(OPERND(I,8)))
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.104.0D0) THEN
                      REG(9)=
     1                VERTEX(11,INT(OPERND(I,8)))
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.105.0D0) THEN
                      REG(9)=
     1                VERTEX(12,INT(OPERND(I,8)))
                      GO TO 777
                  END IF
              END IF
              IF(OPERND(I,17).GE.70.0D0.AND.OPERND(I,17).LE.93.0D0.OR.
     1        OPERND(I,17).GE.106.AND.OPERND(I,17).LE.206.0D0.OR.
     1        OPERND(I,17).GE.488.0D0.AND.OPERND(I,17).LE.509.0D0.OR.
     1        OPERND(I,17).EQ.511.0D0) THEN
C     JUST GET VALUE1 AND PROCEED.
C     CALCULATIONS FOR NON-VERTEX LENS DATABASE STUFF GOES HERE


                  IF(OPERND(I,17).EQ.91.0D0) THEN
C     INDEX
                      REG(9)=ALENS(86,INT(OPERND(I,8)))
                      GO TO 777
                  END IF
C     N1 TO N10
                  IF(OPERND(I,17).EQ.488.0D0) THEN
                      REG(9)=ALENS(46,(INT(OPERND(I,8))))
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.489.0D0) THEN
                      REG(9)=ALENS(47,(INT(OPERND(I,8))))
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.490.0D0) THEN
                      REG(9)=ALENS(48,(INT(OPERND(I,8))))
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.491.0D0) THEN
                      REG(9)=ALENS(49,(INT(OPERND(I,8))))
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.492.0D0) THEN
                      REG(9)=ALENS(50,(INT(OPERND(I,8))))
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.493.0D0) THEN
                      REG(9)=ALENS(71,(INT(OPERND(I,8))))
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.494.0D0) THEN
                      REG(9)=ALENS(72,(INT(OPERND(I,8))))
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.495.0D0) THEN
                      REG(9)=ALENS(73,(INT(OPERND(I,8))))
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.496.0D0) THEN
                      REG(9)=ALENS(74,(INT(OPERND(I,8))))
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.497.0D0) THEN
                      REG(9)=ALENS(75,(INT(OPERND(I,8))))
                      GO TO 777
                  END IF
C     VALUE1 IS 70 TO 90, 92, 93,106 TO 206
C     LENGTH
                  IF(OPERND(I,17).EQ.106.0D0) THEN
                      V1=0.0D0
                      DO II=INT(OPERND(I,8)),(INT(OPERND(I,9))-1)
                          V1=V1+ALENS(3,II)
                      END DO
                      REG(9)=V1
                      GO TO 777
                  END IF
C     MLENGTH
                  IF(OPERND(I,17).EQ.107.0D0) THEN
                      V1=0.0D0
                      DO II=INT(OPERND(I,8)),(INT(OPERND(I,9))-1)
                          IF(INT(SYSTEM1(11)).GE.1.AND.INT(SYSTEM1(11)).LE.5) THEN
                              CW=INT(SYSTEM1(11))+45
                          END IF
                          IF(INT(SYSTEM1(11)).GE.6.AND.INT(SYSTEM1(11)).LE.10) THEN
                              CW=INT(SYSTEM1(11))+65
                          END IF
                          V1=V1+(ALENS(3,II)*ALENS(CW,II))
                      END DO
                      REG(9)=V1
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.70.0D0) THEN
C     RD
                      IF(ALENS(1,INT(OPERND(I,8))).EQ.0.0D0) REG(9)=1.0D300
                      IF(ALENS(1,INT(OPERND(I,8))).NE.0.0D0) REG(9)=1.0D0
     1                /ALENS(1,INT(OPERND(I,8)))
                      GO TO 777
                  END IF
C     CV
                  IF(OPERND(I,17).EQ.71.0D0) THEN
                      REG(9)=
     1                ALENS(1,INT(OPERND(I,8)))
                      GO TO 777
                  END IF
C     TH
                  IF(OPERND(I,17).EQ.72.0D0) THEN
                      REG(9)=
     1                ALENS(3,INT(OPERND(I,8)))
                      GO TO 777
                  END IF
C     CC
                  IF(OPERND(I,17).EQ.73.0D0) THEN
                      REG(9)=
     1                ALENS(2,INT(OPERND(I,8)))
                      GO TO 777
                  END IF
C     AC
                  IF(OPERND(I,17).EQ.74.0D0) THEN
                      REG(9)=
     1                ALENS(43,INT(OPERND(I,8)))
                      GO TO 777
                  END IF
C     AD
                  IF(OPERND(I,17).EQ.75.0D0) THEN
                      REG(9)=
     1                ALENS(4,INT(OPERND(I,8)))
                      GO TO 777
                  END IF
C     AE
                  IF(OPERND(I,17).EQ.76.0D0) THEN
                      REG(9)=
     1                ALENS(5,INT(OPERND(I,8)))
                      GO TO 777
                  END IF
C     AF
                  IF(OPERND(I,17).EQ.77.0D0) THEN
                      REG(9)=
     1                ALENS(6,INT(OPERND(I,8)))
                      GO TO 777
                  END IF
C     AG
                  IF(OPERND(I,17).EQ.78.0D0) THEN
                      REG(9)=
     1                ALENS(7,INT(OPERND(I,8)))
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.79.0D0) THEN
C     RDTOR
                      IF(ALENS(24,INT(OPERND(I,8))).EQ.0.0D0) REG(9)=1.0D300
                      IF(ALENS(24,INT(OPERND(I,8))).NE.0.0D0) REG(9)=1.0D0
     1                /ALENS(24,INT(OPERND(I,8)))
                      GO TO 777
                  END IF
C     CVTOR
                  IF(OPERND(I,17).EQ.80.0D0) THEN
                      REG(9)=
     1                ALENS(24,INT(OPERND(I,8)))
                      GO TO 777
                  END IF
C     CCTOR
                  IF(OPERND(I,17).EQ.81.0D0) THEN
                      REG(9)=
     1                ALENS(41,INT(OPERND(I,8)))
                      GO TO 777
                  END IF
C     ADTOR
                  IF(OPERND(I,17).EQ.82.0D0) THEN
                      REG(9)=
     1                ALENS(41,INT(OPERND(I,8)))
                      GO TO 777
                  END IF
C     AETOR
                  IF(OPERND(I,17).EQ.83.0D0) THEN
                      REG(9)=
     1                ALENS(41,INT(OPERND(I,8)))
                      GO TO 777
                  END IF
C     AFTOR
                  IF(OPERND(I,17).EQ.84.0D0) THEN
                      REG(9)=
     1                ALENS(41,INT(OPERND(I,8)))
                      GO TO 777
                  END IF
C     AGTOR
                  IF(OPERND(I,17).EQ.85.0D0) THEN
                      REG(9)=
     1                ALENS(41,INT(OPERND(I,8)))
                      GO TO 777
                  END IF
C     ALPHA
                  IF(OPERND(I,17).EQ.86.0D0) THEN
                      REG(9)=
     1                ALENS(118,INT(OPERND(I,8)))
                      GO TO 777
                  END IF
C     BETA
                  IF(OPERND(I,17).EQ.87.0D0) THEN
                      REG(9)=
     1                ALENS(119,INT(OPERND(I,8)))
                      GO TO 777
                  END IF
C     GAMMA
                  IF(OPERND(I,17).EQ.88.0D0) THEN
                      REG(9)=
     1                ALENS(120,INT(OPERND(I,8)))
                      GO TO 777
                  END IF
C     ABBE
                  IF(OPERND(I,17).EQ.498.0D0) THEN
                      NF=INT(OPERND(I,8))
                      CALL SINDEX
                      REG(9)=VNUM
                      GO TO 777
                  END IF
C     DPART
                  IF(OPERND(I,17).EQ.499.0D0) THEN
                      NF=INT(OPERND(I,8))
                      REG(9)=ALENS(89,NF)
                      GO TO 777
                  END IF
C     CLPX
                  IF(OPERND(I,17).EQ.500.0D0) THEN
                      NF=INT(OPERND(I,8))
                      IF(ALENS(9,NF).EQ.0.0D0.OR.ALENS(127,NF).NE.0.0D0)
     1                REG(9)=0.0D0
                      IF(ALENS(127,NF).EQ.0.0D0) THEN
                          IF(ALENS(9,NF).EQ.1.0D0)
     1                    REG(9)=ALENS(10,NF)
                          IF(ALENS(9,NF).EQ.2.0D0)
     1                    REG(9)=ALENS(11,NF)
                          IF(ALENS(9,NF).EQ.3.0D0)
     1                    REG(9)=ALENS(11,NF)
                          IF(ALENS(9,NF).EQ.4.0D0)
     1                    REG(9)=ALENS(11,NF)
                          IF(ALENS(9,NF).EQ.5.0D0)
     1                    REG(9)=ALENS(10,NF)
                          IF(ALENS(9,NF).EQ.6.0D0)
     1                    REG(9)=ALENS(11,NF)
                      END IF
                      GO TO 777
                  END IF
C     CLPY
                  IF(OPERND(I,17).EQ.501.0D0) THEN
                      NF=INT(OPERND(I,8))
                      IF(ALENS(9,NF).EQ.0.0D0.OR.ALENS(127,NF).NE.0.0D0)
     1                REG(9)=0.0D0
                      IF(ALENS(127,NF).EQ.0.0D0) THEN
                          IF(ALENS(9,NF).EQ.1.0D0)
     1                    REG(9)=ALENS(10,NF)
                          IF(ALENS(9,NF).EQ.2.0D0)
     1                    REG(9)=ALENS(10,NF)
                          IF(ALENS(9,NF).EQ.3.0D0)
     1                    REG(9)=ALENS(10,NF)
                          IF(ALENS(9,NF).EQ.4.0D0)
     1                    REG(9)=ALENS(10,NF)
                          IF(ALENS(9,NF).EQ.5.0D0)
     1                    REG(9)=ALENS(10,NF)
                          IF(ALENS(9,NF).EQ.6.0D0)
     1                    REG(9)=ALENS(11,NF)
                      END IF
                      GO TO 777
                  END IF
C     GDX
                  IF(OPERND(I,17).EQ.502.0D0) THEN
                      NF=INT(OPERND(I,8))
                      REG(9)=ALENS(90,NF)
                      GO TO 777
                  END IF
C     GDY
                  IF(OPERND(I,17).EQ.503.0D0) THEN
                      NF=INT(OPERND(I,8))
                      REG(9)=ALENS(91,NF)
                      GO TO 777
                  END IF
C     GDZ
                  IF(OPERND(I,17).EQ.504.0D0) THEN
                      NF=INT(OPERND(I,8))
                      REG(9)=ALENS(92,NF)
                      GO TO 777
                  END IF
C     GALPHA
                  IF(OPERND(I,17).EQ.505.0D0) THEN
                      NF=INT(OPERND(I,8))
                      REG(9)=ALENS(93,NF)
                      GO TO 777
                  END IF
C     GBETA
                  IF(OPERND(I,17).EQ.506.0D0) THEN
                      NF=INT(OPERND(I,8))
                      REG(9)=ALENS(94,NF)
                      GO TO 777
                  END IF
C     GGAMMA
                  IF(OPERND(I,17).EQ.507.0D0) THEN
                      NF=INT(OPERND(I,8))
                      REG(9)=ALENS(95,NF)
                      GO TO 777
                  END IF
C     GRS
                  IF(OPERND(I,17).EQ.508.0D0) THEN
                      NF=INT(OPERND(I,8))
                      REG(9)=ALENS(98,NF)
                      GO TO 777
                  END IF
C     WEIGHT
                  IF(OPERND(I,17).EQ.509.0D0) THEN
                      SAVE_KDP(1)=SAVEINPT(1)
                      WC='WEIGHT  '
                      WQ='ACC     '
                      SQ=1
                      W1=OPERND(I,8)
                      W2=OPERND(I,9)
                      W3=0.0D0
                      W4=0.0D0
                      W5=0.0D0
                      DF1=0
                      DF2=0
                      DF3=1
                      DF4=1
                      DF5=1
                      S1=1
                      S2=1
                      S3=0
                      S4=0
                      S5=0
                      SN=1
                      CALL WEIGHT
                      REST_KDP(1)=RESTINPT(1)
                      GO TO 777
                  END IF
C     COST
                  IF(OPERND(I,17).EQ.511.0D0) THEN
                      SAVE_KDP(1)=SAVEINPT(1)
                      WC='COST    '
                      WQ='ACC     '
                      SQ=1
                      W1=OPERND(I,8)
                      W2=OPERND(I,9)
                      W3=0.0D0
                      W4=0.0D0
                      W5=0.0D0
                      DF1=0
                      DF2=0
                      DF3=1
                      DF4=1
                      DF5=1
                      S1=1
                      S2=1
                      S3=0
                      S4=0
                      S5=0
                      SN=1
                      CALL COST
                      REST_KDP(1)=RESTINPT(1)
                      GO TO 777
                  END IF
C     VNUM
                  IF(OPERND(I,17).EQ.89.0D0) THEN
                      NF=INT(OPERND(I,8))
                      REG(9)=ALENS(87,NF)
                      GO TO 777
                  END IF
C     PARTL
                  IF(OPERND(I,17).EQ.90.0D0) THEN
                      NF=INT(OPERND(I,8))
                      CALL SINDEX
                      REG(9)=PARTL
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.92.0D0) THEN
C     XD
                      REG(9)=ALENS(114,INT(OPERND(I,8)))
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.467.0D0) THEN
C     ZD
                      REG(9)=ALENS(116,INT(OPERND(I,8)))
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.93.0D0) THEN
C     YD
                      REG(9)=ALENS(115,INT(OPERND(I,8)))
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.108.0D0) THEN
C     ET,ETY
                      W1A=OPERND(I,8)
                      REG(9)=EDGTHK(INT(W1A),1)
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.109.0D0) THEN
C     ETX
                      W1A=OPERND(I,8)
                      REG(9)=EDGTHK(INT(W1A),2)
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.110.0D0) THEN
C     SHAPEFAC
                      W1A=OPERND(I,8)
                      IF(DABS(ALENS(1,INT(W1A))).LT.1D-30) THEN
                          IF(ALENS(1,INT(W1A)).GT.0.0D0) TEMPR1=1.0D30
                          IF(ALENS(1,INT(W1A)).LT.0.0D0) TEMPR1=-1.0D30
                      ELSE
                          TEMPR1=1.0D0/ALENS(1,INT(W1A))
                      END IF
                      IF(DABS(ALENS(1,INT(W1A)+1)).LT.1D-30) THEN
                          IF(ALENS(1,INT(W1A)+1).GT.0.0D0) TEMPR2=1.0D30
                          IF(ALENS(1,INT(W1A)+1).LT.0.0D0) TEMPR2=-1.0D30
                      ELSE
                          TEMPR2=1.0D0/ALENS(1,INT(W1A)+1)
                      END IF
                      TEMPSUM=TEMPR2+TEMPR1
                      TEMPDIF=TEMPR2-TEMPR1
                      IF(DABS(TEMPDIF).EQ.0.0D0) THEN
                          IF(TEMPSUM.EQ.0.0D0) REG(9)=0.0D0
                          IF(TEMPSUM.GT.0.0D0.AND.TEMPDIF.GT.0.0D0) REG(9)=1.0D30
                          IF(TEMPSUM.LT.0.0D0.AND.TEMPDIF.LT.0.0D0) REG(9)=1.0D30
                          IF(TEMPSUM.GT.0.0D0.AND.TEMPDIF.LT.0.0D0) REG(9)=-1.0D30
                          IF(TEMPSUM.LT.0.0D0.AND.TEMPDIF.GT.0.0D0) REG(9)=-1.0D30
                      ELSE
                          REG(9)=(TEMPR2+TEMPR1)/(TEMPR2-TEMPR1)
                      END IF
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).GE.111.0D0.AND.OPERND(I,17).LE.206.0D0) THEN
C     SPSRF COEFS C1 TO C96
                      REG(9)=FTFL01((INT(OPERND(I,17))-110),INT(OPERND(I,8)))
                      GO TO 777
                  END IF
              END IF
          END IF
C
          IF(OPERND(I,17).GE.207.0D0.AND.OPERND(I,17).LE.236) THEN
C
C     PARAXIAL OPERANDS
C
              IF(OPERND(I,17).EQ.207.0D0.OR.OPERND(I,17).EQ.208.0D0.OR.
     1        OPERND(I,17).EQ.209.0D0.OR.OPERND(I,17).EQ.210.0D0) THEN
C       YZ AND XZ PLANE EFL CALCULATION
C
                  IF(OPERND(I,8).GT.0.0D0) THEN
                      IIA=INT(OPERND(I,8))-1
                  ELSE
                      IIA=INT(OPERND(I,8))
                  END IF
                  JIA=INT(OPERND(I,9))
                  EFLY=-(((PXTRAY(2,IIA)*PXTRAY(5,IIA+1))-(PXTRAY(1,IIA+1)*
     1            PXTRAY(6,IIA
     1            )))/((PXTRAY(2,IIA)*PXTRAY(6,JIA))-(PXTRAY(6,IIA)*PXTRAY(2,JIA))))
                  EFLX=-(((PXTRAX(2,IIA)*PXTRAX(5,IIA+1))-(PXTRAX(1,IIA+1)*
     1            PXTRAX(6,IIA
     1            )))/((PXTRAX(2,IIA)*PXTRAX(6,JIA))-(PXTRAX(6,IIA)*PXTRAX(2,JIA))))
                  IF(OPERND(I,17).EQ.209.0D0) THEN
                      REG(9)=EFLX
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.210.0D0) THEN
                      REG(9)=EFLY
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.207.0D0) THEN
                      IF(EFLY.NE.0.0D0) THEN
                          REG(9)=1.0D0/EFLY
                      ELSE
                          REG(9)=1.0D300
                      END IF
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.208.0D0) THEN
                      IF(EFLX.NE.0.0D0) THEN
                          REG(9)=1.0D0/EFLX
                      ELSE
                          REG(9)=0.0D0
                      END IF
                      GO TO 777
                  END IF
              END IF
C
              IF(OPERND(I,9).EQ.SYSTEM1(11)) THEN
C     PARAXIAL VALUE1S AT THE CONTROL WAVELENGTH
                  IF(OPERND(I,17).EQ.211.0D0) THEN
C     PY
                      REG(9)=PXTRAY(1,INT(OPERND(I,8)))
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.212.0D0) THEN
C     PX
                      REG(9)=PXTRAX(1,INT(OPERND(I,8)))
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.213.0D0) THEN
C     PCY
                      REG(9)=PXTRAY(5,INT(OPERND(I,8)))
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.214.0D0) THEN
C     PCX
                      REG(9)=PXTRAX(5,INT(OPERND(I,8)))
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.215.0D0) THEN
C     PUY
                      REG(9)=PXTRAY(2,INT(OPERND(I,8)))
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.216.0D0) THEN
C     PUX
                      REG(9)=PXTRAX(2,INT(OPERND(I,8)))
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.217.0D0) THEN
C     PUCY
                      REG(9)=PXTRAY(6,INT(OPERND(I,8)))
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.218.0D0) THEN
C     PUCX
                      REG(9)=PXTRAX(6,INT(OPERND(I,8)))
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.219.0D0) THEN
C     PIY
                      REG(9)=PXTRAY(3,INT(OPERND(I,8)))
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.220.0D0) THEN
C     PIX
                      REG(9)=PXTRAX(3,INT(OPERND(I,8)))
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.221.0D0) THEN
C     PICY
                      REG(9)=PXTRAY(7,INT(OPERND(I,8)))
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.222.0D0) THEN
C     PICX
                      REG(9)=PXTRAX(7,INT(OPERND(I,8)))
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.223.0D0) THEN
C     PIYP
                      REG(9)=PXTRAY(4,INT(OPERND(I,8)))
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.224.0D0) THEN
C     PIXP
                      REG(9)=PXTRAX(4,INT(OPERND(I,8)))
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.225.0D0) THEN
C     PICYP
                      REG(9)=PXTRAY(8,INT(OPERND(I,8)))
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.226.0D0) THEN
C     PICXP
                      REG(9)=PXTRAX(8,INT(OPERND(I,8)))
                      GO TO 777
                  END IF
              ELSE
C     NOT AT CONTROL WAVELENGTH
                  WV=OPERND(I,9)
                  ITYP=1
                  CALL PRCOL
                  WV=OPERND(I,9)
                  ITYP=2
                  CALL PRCOL
                  IF(OPERND(I,17).EQ.211.0D0) THEN
C     PY
                      REG(9)=COLY(1,INT(OPERND(I,8)))
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.212.0D0) THEN
C     PX
                      REG(9)=COLX(1,INT(OPERND(I,8)))
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.213.0D0) THEN
C     PCY
                      REG(9)=COLY(5,INT(OPERND(I,8)))
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.214.0D0) THEN
C     PCX
                      REG(9)=COLX(5,INT(OPERND(I,8)))
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.215.0D0) THEN
C     PUY
                      REG(9)=COLY(2,INT(OPERND(I,8)))
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.216.0D0) THEN
C     PUX
                      REG(9)=COLX(2,INT(OPERND(I,8)))
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.217.0D0) THEN
C     PUCY
                      REG(9)=COLY(6,INT(OPERND(I,8)))
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.218.0D0) THEN
C     PUCX
                      REG(9)=COLX(6,INT(OPERND(I,8)))
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.219.0D0) THEN
C     PIY
                      REG(9)=COLY(3,INT(OPERND(I,8)))
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.220.0D0) THEN
C     PIX
                      REG(9)=COLX(3,INT(OPERND(I,8)))
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.221.0D0) THEN
C     PICY
                      REG(9)=COLY(7,INT(OPERND(I,8)))
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.222.0D0) THEN
C     PICX
                      REG(9)=COLX(7,INT(OPERND(I,8)))
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.223.0D0) THEN
C     PIYP
                      REG(9)=COLY(4,INT(OPERND(I,8)))
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.224.0D0) THEN
C     PIXP
                      REG(9)=COLX(4,INT(OPERND(I,8)))
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.225.0D0) THEN
C     PICYP
                      REG(9)=COLY(8,INT(OPERND(I,8)))
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.226.0D0) THEN
C     PICXP
                      REG(9)=COLX(8,INT(OPERND(I,8)))
                      GO TO 777
                  END IF
              END IF
C
              IF(OPERND(I,17).GE.227.0D0.AND.OPERND(I,17).LE.234.0D0) THEN
C     CHROMATIC ABERRATIONS
                  CALL PRTRB
                  SF=INT(SYSTEM1(20))
                  IF(INT(OPERND(I,18)).EQ.0) SF1=INT(OPERND(I,8))
                  IF(INT(SYSTEM1(11)).GE.1.AND.INT(SYSTEM1(11)).LE.5) THEN
                      CW=INT(SYSTEM1(11))+45
                  END IF
                  IF(INT(SYSTEM1(11)).GE.6.AND.INT(SYSTEM1(11)).LE.10) THEN
                      CW=INT(SYSTEM1(11))+65
                  END IF
                  INTV=1.0D0
C       CALCULATE INTV
                  IF(OPERND(I,17).EQ.228.0D0.OR.OPERND(I,17).EQ.230.0D0.OR.
     1            OPERND(I,17).EQ.232.0D0.OR.OPERND(I,17).EQ.234.0D0) THEN
C     PACX,PLCX,SACX OR SLCX
                      INTV=((PXTRAX(5,SF)*ALENS(CW,(SF-1))*PXTRAX(2,(SF-1)))-
     1                (PXTRAX(1,SF)*ALENS(CW,(SF-1))*PXTRAX(6,(SF-1))))
                  END IF
                  IF(OPERND(I,17).EQ.227.0D0.OR.OPERND(I,17).EQ.229.0D0.OR.
     1            OPERND(I,17).EQ.231.0D0.OR.OPERND(I,17).EQ.233.0D0) THEN
C     PACY,PLCY,SACY OR SLCY
                      INTV=((PXTRAY(5,SF)*ALENS(CW,(SF-1))*PXTRAY(2,(SF-1)))-
     1                (PXTRAY(1,SF)*ALENS(CW,(SF-1))*PXTRAY(6,(SF-1))))
                  END IF
                  IF(INTV.EQ.0.0D0) THEN

                      IF(F28.EQ.1) REG(9)=0.0D0
                      IF(F28.EQ.1) BAAD=1
                      IF(F31.EQ.1) CALL MACFAL
                      RETURN
                  END IF
                  IF(INT(OPERND(I,18)).EQ.0) THEN
C     SINGLE SURFACE NOT IMAGE SURFACE
C     DO EACH CALC NOW
                      IF(SYSTEM1(30).LE.2.0D0) THEN
                          IF(OPERND(I,17).EQ.227.0D0) THEN
                              REG(9)=COLORY(1,SF1)/INTV
                              GO TO 777
                          END IF
                          IF(OPERND(I,17).EQ.229.0D0) THEN
                              REG(9)=COLORY(2,SF1)/INTV
                              GO TO 777
                          END IF
                          IF(OPERND(I,17).EQ.231.0D0) THEN
                              REG(9)=COLORY(3,SF1)/INTV
                              GO TO 777
                          END IF
                          IF(OPERND(I,17).EQ.233.0D0) THEN
                              REG(9)=COLORY(4,SF1)/INTV
                              GO TO 777
                          END IF
                          IF(OPERND(I,17).EQ.228.0D0) THEN
                              REG(9)=COLORX(1,SF1)/INTV
                              GO TO 777
                          END IF
                          IF(OPERND(I,17).EQ.230.0D0) THEN
                              REG(9)=COLORX(2,SF1)/INTV
                              GO TO 777
                          END IF
                          IF(OPERND(I,17).EQ.232.0D0) THEN
                              REG(9)=COLORX(3,SF1)/INTV
                              GO TO 777
                          END IF
                          IF(OPERND(I,17).EQ.234.0D0) THEN
                              REG(9)=COLORX(4,SF1)/INTV
                              GO TO 777
                          END IF
                      ELSE
C     AFOCAL
                          IF(OPERND(I,17).EQ.227.0D0) THEN
                              REG(9)=COLORY(5,SF1)/INTV
                              GO TO 777
                          END IF
                          IF(OPERND(I,17).EQ.229.0D0) THEN
                              REG(9)=COLORY(6,SF1)/INTV
                              GO TO 777
                          END IF
                          IF(OPERND(I,17).EQ.231.0D0) THEN
                              REG(9)=COLORY(7,SF1)/INTV
                              GO TO 777
                          END IF
                          IF(OPERND(I,17).EQ.233.0D0) THEN
                              REG(9)=COLORY(8,SF1)/INTV
                              GO TO 777
                          END IF
                          IF(OPERND(I,17).EQ.228.0D0) THEN
                              REG(9)=COLORX(5,SF1)/INTV
                              GO TO 777
                          END IF
                          IF(OPERND(I,17).EQ.230.0D0) THEN
                              REG(9)=COLORX(6,SF1)/INTV
                              GO TO 777
                          END IF
                          IF(OPERND(I,17).EQ.232.0D0) THEN
                              REG(9)=COLORX(7,SF1)/INTV
                              GO TO 777
                          END IF
                          IF(OPERND(I,17).EQ.234.0D0) THEN
                              REG(9)=COLORX(8,SF1)/INTV
                              GO TO 777
                          END IF
                      END IF
                  ELSE
C     FINAL SURFACE
                      IF(SYSTEM1(30).LE.2.0D0) THEN
                          IF(OPERND(I,17).EQ.227.0D0) THEN
                              V=0.0D0
                              DO IV=0,INT(SYSTEM1(20))
                                  V=V+COLORY(1,IV)
                              END DO
                              REG(9)=V/INTV
                              GO TO 777
                          END IF
                          IF(OPERND(I,17).EQ.229.0D0) THEN
                              V=0.0D0
                              DO IV=0,INT(SYSTEM1(20))
                                  V=V+COLORY(2,IV)
                              END DO
                              REG(9)=V/INTV
                              GO TO 777
                          END IF
                          IF(OPERND(I,17).EQ.231.0D0) THEN
                              V=0.0D0
                              DO IV=0,INT(SYSTEM1(20))
                                  V=V+COLORY(3,IV)
                              END DO
                              REG(9)=V/INTV
                              GO TO 777
                          END IF
                          IF(OPERND(I,17).EQ.233.0D0) THEN
                              V=0.0D0
                              DO IV=0,INT(SYSTEM1(20))
                                  V=V+COLORY(4,IV)
                              END DO
                              REG(9)=V/INTV
                              GO TO 777
                          END IF
                          IF(OPERND(I,17).EQ.228.0D0) THEN
                              V=0.0D0
                              DO IV=0,INT(SYSTEM1(20))
                                  V=V+COLORX(1,IV)
                              END DO
                              REG(9)=V/INTV
                              GO TO 777
                          END IF
                          IF(OPERND(I,17).EQ.230.0D0) THEN
                              V=0.0D0
                              DO IV=0,INT(SYSTEM1(20))
                                  V=V+COLORX(2,IV)
                              END DO
                              REG(9)=V/INTV
                              GO TO 777
                          END IF
                          IF(OPERND(I,17).EQ.232.0D0) THEN
                              V=0.0D0
                              DO IV=0,INT(SYSTEM1(20))
                                  V=V+COLORX(3,IV)
                              END DO
                              REG(9)=V/INTV
                              GO TO 777
                          END IF
                          IF(OPERND(I,17).EQ.234.0D0) THEN
                              V=0.0D0
                              DO IV=0,INT(SYSTEM1(20))
                                  V=V+COLORX(4,IV)
                              END DO
                              REG(9)=V/INTV
                              GO TO 777
                          END IF
                      ELSE
                          IF(OPERND(I,17).EQ.227.0D0) THEN
                              V=0.0D0
                              DO IV=0,INT(SYSTEM1(20))
                                  V=V+COLORY(5,IV)
                              END DO
                              REG(9)=V/INTV
                              GO TO 777
                          END IF
                          IF(OPERND(I,17).EQ.229.0D0) THEN
                              V=0.0D0
                              DO IV=0,INT(SYSTEM1(20))
                                  V=V+COLORY(6,IV)
                              END DO
                              REG(9)=V/INTV
                              GO TO 777
                          END IF
                          IF(OPERND(I,17).EQ.231.0D0) THEN
                              V=0.0D0
                              DO IV=0,INT(SYSTEM1(20))
                                  V=V+COLORY(7,IV)
                              END DO
                              REG(9)=V/INTV
                              GO TO 777
                          END IF
                          IF(OPERND(I,17).EQ.233.0D0) THEN
                              V=0.0D0
                              DO IV=0,INT(SYSTEM1(20))
                                  V=V+COLORY(8,IV)
                              END DO
                              REG(9)=V/INTV
                              GO TO 777
                          END IF
                          IF(OPERND(I,17).EQ.228.0D0) THEN
                              V=0.0D0
                              DO IV=0,INT(SYSTEM1(20))
                                  V=V+COLORX(5,IV)
                              END DO
                              REG(9)=V/INTV
                              GO TO 777
                          END IF
                          IF(OPERND(I,17).EQ.230.0D0) THEN
                              V=0.0D0
                              DO IV=0,INT(SYSTEM1(20))
                                  V=V+COLORX(6,IV)
                              END DO
                              REG(9)=V/INTV
                              GO TO 777
                          END IF
                          IF(OPERND(I,17).EQ.232.0D0) THEN
                              V=0.0D0
                              DO IV=0,INT(SYSTEM1(20))
                                  V=V+COLORX(7,IV)
                              END DO
                              REG(9)=V/INTV
                              GO TO 777
                          END IF
                          IF(OPERND(I,17).EQ.234.0D0) THEN
                              V=0.0D0
                              DO IV=0,INT(SYSTEM1(20))
                                  V=V+COLORX(8,IV)
                              END DO
                              REG(9)=V/INTV
                              GO TO 777
                          END IF
                      END IF
                  END IF
              END IF
C
              IF(OPERND(I,17).EQ.235.0D0) THEN
C       GET IMDISX
                  W1A=OPERND(I,8)
                  W1B=OPERND(I,8)
                  IF(W1B.NE.0.0D0) W1B=W1B-1.0D0
                  IF(PXTRAX(2,INT(W1B)).NE.0.0D0) THEN
                      REG(9)=((-PXTRAX(1,INT(W1A)))/(PXTRAX(2,INT(W1B))))
                  ELSE
                      REG(9)=1.0D300
                  END IF
                  GO TO 777
              END IF
              IF(OPERND(I,17).EQ.236.0D0) THEN
C       GET IMDISY
                  W1A=OPERND(I,8)
                  W1B=OPERND(I,8)
                  IF(W1B.NE.0.0D0) W1B=W1B-1.0D0
                  IF(PXTRAY(2,INT(W1B)).NE.0.0D0) THEN
                      REG(9)=((-PXTRAY(1,INT(W1A)))/(PXTRAY(2,INT(W1B))))
                  ELSE
                      REG(9)=1.0D300
                  END IF
                  GO TO 777
              END IF
          END IF
C
          IF(OPERND(I,17).GE.237.0D0.AND.OPERND(I,17).LE.244.0D0.OR.
     1    OPERND(I,17).GE.460.0D0.AND.OPERND(I,17).LE.461.0D0.OR.
     2    OPERND(I,17).GE.464.0D0.AND.OPERND(I,17).LE.465.0D0
     3    .OR.OPERND(I,17).EQ.513.0D0) THEN
C     SPOT DIAGRAM BASED OPERANDS
C     CALCULATE VALUE1 AND PLACE IN ACC
C     TEST IF SPOT NEEDS TO BE TRACED
C
              IF(OLDSPD.AND.INT(OPERND(I,8)).EQ.OLDF.AND.
     1        OLDW.EQ.INT(OPERND(I,9)).AND.
     1        CFNUM.EQ.OLDCFG) THEN
C     DON'T NEED TO TRACE THE SPOT AGAIN, IT ALREDY EXITS
              ELSE
C
C     TRACE FOB AND SPOT THEN GET VALUE1
C     TRACE FIELD DESIGNAMTED BY NW3 INT(OPERND(I,8)))
C     DO THE FOB
                  SAVE_KDP(1)=SAVEINPT(1)
                  OLDLDIF2=LDIF2
                  OLDLDIF=LDIF
                  IF(OPDIF) THEN
                      LDIF2=.TRUE.
                      LDIF=.TRUE.
                  ELSE
                      LDIF2=.FALSE.
                      LDIF=.FALSE.
                  END IF
                  WC='FOB     '
                  WQ='        '
                  SQ=0
                  SST=0
                  STI=0
                  W1=FIELDY(INT(OPERND(I,8)))
                  W2=FIELDX(INT(OPERND(I,8)))
                  W3=FIELDZ(INT(OPERND(I,8)))
                  W4=FIELDW(INT(OPERND(I,8)))
                  W5=0.0D0
                  DF1=0
                  DF2=0
                  DF3=0
                  DF4=0
                  DF5=1
                  S1=1
                  S2=1
                  S3=1
                  S4=1
                  S5=0
                  SN=1
C     SET MSG TO FALSE
                  MSG=.FALSE.
                  BADOPS=.FALSE.
                  CALL FFOB2
                  REST_KDP(1)=RESTINPT(1)
                  IF(BADOPS) THEN
                      IF(F28.EQ.1) BAAD=1
                      IF(F28.EQ.1) REG(9)=0.0D0
                      IF(F31.EQ.1) CALL MACFAL
                      RETURN
                  END IF
C NOW THE SPOT
                  IF(OPERND(I,17).NE.464.0D0.AND.OPERND(I,17).NE.465.0D0.AND.
     1            OPERND(I,17).NE.460.0D0.AND.OPERND(I,17).NE.461.0D0) THEN
C     NOT GOTF OR RED S
                      SAVE_KDP(1)=SAVEINPT(1)
C     SET MSG TO FALSE
                      MSG=.FALSE.
                      BADOPS=.FALSE.
                      IF(INT(OPERND(I,11)).EQ.1) THEN
                          OLDIF=LDIF
                          LDIF=.FALSE.
                          CALL SPOT1(2)
                          LDIF=OLDIF
                      ELSE
                          OLDSP(1) =SYSTEM1(31)
                          OLDSP(2) =SYSTEM1(32)
                          OLDSP(3) =SYSTEM1(33)
                          OLDSP(4) =SYSTEM1(34)
                          OLDSP(5) =SYSTEM1(35)
                          OLDSP(6) =SYSTEM1(76)
                          OLDSP(7) =SYSTEM1(77)
                          OLDSP(8) =SYSTEM1(78)
                          OLDSP(9) =SYSTEM1(79)
                          OLDSP(10)=SYSTEM1(80)
                          SYSTEM1(31:35)=0.0D0
                          SYSTEM1(76:80)=0.0D0
                          IF(INT(OPERND(I,9)).EQ.1)  SYSTEM1(31)=1.0D0
                          IF(INT(OPERND(I,9)).EQ.2)  SYSTEM1(32)=1.0D0
                          IF(INT(OPERND(I,9)).EQ.3)  SYSTEM1(33)=1.0D0
                          IF(INT(OPERND(I,9)).EQ.4)  SYSTEM1(34)=1.0D0
                          IF(INT(OPERND(I,9)).EQ.5)  SYSTEM1(35)=1.0D0
                          IF(INT(OPERND(I,9)).EQ.6)  SYSTEM1(76)=1.0D0
                          IF(INT(OPERND(I,9)).EQ.7)  SYSTEM1(77)=1.0D0
                          IF(INT(OPERND(I,9)).EQ.8)  SYSTEM1(78)=1.0D0
                          IF(INT(OPERND(I,9)).EQ.9)  SYSTEM1(79)=1.0D0
                          IF(INT(OPERND(I,9)).EQ.10) SYSTEM1(80)=1.0D0
                          OLDIF=LDIF
                          LDIF=.FALSE.
                          CALL SPOT1(2)
                          LDIF=OLDIF
                          SYSTEM1(31)=OLDSP(1)
                          SYSTEM1(32)=OLDSP(2)
                          SYSTEM1(33)=OLDSP(3)
                          SYSTEM1(34)=OLDSP(4)
                          SYSTEM1(35)=OLDSP(5)
                          SYSTEM1(76)=OLDSP(6)
                          SYSTEM1(77)=OLDSP(7)
                          SYSTEM1(78)=OLDSP(8)
                          SYSTEM1(79)=OLDSP(9)
                          SYSTEM1(80)=OLDSP(10)
                      END IF
                      REST_KDP(1)=RESTINPT(1)
                  ELSE
C     GOTFS AND REDS
                      OLDIF=LDIF
                      LDIF=.FALSE.
                      CALL SPOT1(2)
                      LDIF=OLDIF
                  END IF
                  IF(BADOPS) THEN
                      IF(F28.EQ.1) BAAD=1
                      IF(F28.EQ.1) REG(9)=0.0D0
                      IF(F31.EQ.1) CALL MACFAL
                      RETURN
                  END IF
                  LDIF2=OLDLDIF2
                  LDIF=OLDLDIF
                  IF(.NOT.REFEXT.OR..NOT.SPDEXT) THEN
                      IF(F28.EQ.1) REG(9)=0.0D0
                      IF(F28.EQ.1) BAAD=1
                      IF(F31.EQ.1) CALL MACFAL
                      RETURN
                  END IF
                  OLDF=INT(OPERND(I,8))
                  OLDW=INT(OPERND(I,9))
                  OLDSPD=SPDEXT
                  OLDCFG=CFNUM
              END IF
              IF(OPERND(I,17).EQ.237.0D0) THEN
                  REG(9)=CENTX
                  GO TO 777
              END IF
              IF(OPERND(I,17).EQ.238.0D0) THEN
                  REG(9)=CENTY
                  GO TO 777
              END IF
              IF(OPERND(I,17).EQ.239.0D0) THEN
                  REG(9)=RMSX
                  GO TO 777
              END IF
              IF(OPERND(I,17).EQ.240.0D0) THEN
                  REG(9)=RMSY
                  GO TO 777
              END IF
              IF(OPERND(I,17).EQ.513.0D0) THEN
                  IF(RMSX.EQ.0.0D0) THEN
                      ERROR=1
                      REG(9)=0.0D0
                  ELSE
                      ERROR=0
                      REG(9)=RMSY/RMSX
                  END IF
                  GO TO 777
              END IF
              IF(OPERND(I,17).EQ.241.0D0) THEN
                  REG(9)=RMS
                  GO TO 777
              END IF
              IF(OPERND(I,17).EQ.242.0D0) THEN
                  REG(9)=RSSX
                  GO TO 777
              END IF
C
              IF(OPERND(I,17).EQ.243.0D0) THEN
                  REG(9)=RSSY
                  GO TO 777
              END IF
C
              IF(OPERND(I,17).EQ.244.0D0) THEN
                  REG(9)=RSS
                  GO TO 777
              END IF
              IF(OPERND(I,17).EQ.464.0D0) THEN
C     RED
                  SAVE_KDP(5)=SAVEINPT(5)
                  W1=OPERND(I,9)
                  DF1=0
                  S1=1
                  DF2=1
                  DF3=1
                  DF4=1
                  DF5=1
                  S2=0
                  S3=0
                  S4=0
                  S5=0
                  SQ=1
                  WC='RED'
                  WQ='ACC     '
                  STI=0
                  SST=0
                  SN=1
                  CALL SPRED
C       CALL ROUTINE THAT CALCULATES THE RED VALUE1
                  REST_KDP(5)=RESTINPT(5)
                  OLDF=INT(OPERND(I,8))
                  OLDW=0
                  OLDSPD=SPDEXT
                  OLDCFG=CFNUM
                  GO TO 777
              END IF
              IF(OPERND(I,17).EQ.465.0D0) THEN
C REDCEN
C       CALL ROUTINE THAT CALCULATES THE RED CENT
                  SAVE_KDP(5)=SAVEINPT(5)
                  W1=OPERND(I,9)
                  DF1=0
                  S1=1
                  DF2=1
                  DF3=1
                  DF4=1
                  DF5=1
                  S2=0
                  S3=0
                  S4=0
                  S5=0
                  SQ=1
                  WC='RED'
                  WQ='CACC    '
                  STI=0
                  SST=0
                  SN=1
                  CALL SPRED
                  REST_KDP(5)=RESTINPT(5)
                  OLDF=INT(OPERND(I,8))
                  OLDW=0
                  OLDSPD=SPDEXT
                  OLDCFG=CFNUM
                  GO TO 777
              END IF
              IF(OPERND(I,17).EQ.460.0D0) THEN
C     GOTF MOD
                  SAVE_KDP(5)=SAVEINPT(5)
C       CALL ROUTINE THAT CALCULATES THE GOTF MOD
                  WC='GOTF'
                  W1=OPERND(I,9)
                  WQ='ACC'
                  DF1=0
                  S1=1
                  W2=OPERND(I,10)
                  DF2=0
                  DF3=1
                  DF4=1
                  DF5=1
                  S2=1
                  S3=0
                  S4=0
                  S5=0
                  SQ=1
                  STI=0
                  SST=0
                  SN=1
                  CALL GOTF
                  REST_KDP(5)=RESTINPT(5)
                  OLDF=INT(OPERND(I,8))
                  OLDW=0
                  OLDSPD=SPDEXT
                  OLDCFG=CFNUM
                  GO TO 777
              END IF
              IF(OPERND(I,17).EQ.461.0D0) THEN
C GOTF P
                  SAVE_KDP(5)=SAVEINPT(5)
C       CALL ROUTINE THAT CALCULATES THE GOTF PHASE
                  WC='GOTF'
                  W1=OPERND(I,9)
                  WQ='ACC'
                  DF1=0
                  S1=1
                  W2=OPERND(I,10)
                  DF2=0
                  DF3=1
                  DF4=1
                  DF5=1
                  S2=1
                  S3=0
                  S4=0
                  S5=0
                  SQ=1
                  STI=0
                  SST=0
                  SN=1
                  CALL GOTF
                  REST_KDP(5)=RESTINPT(5)
                  OLDF=INT(OPERND(I,8))
                  OLDW=0
                  OLDSPD=SPDEXT
                  OLDCFG=CFNUM
                  REG(9)=REG(10)
                  GO TO 777
              END IF
          END IF
C
          IF(OPERND(I,17).EQ.484.0D0) THEN
C     GREYS OPERAND GREYSPOT DIAGRAM BASED OPERANDS
C     CALCULATE VALUE1 AND PLACE IN ACC
C
C     TRACE FOB AND SPOT THEN GET VALUE1
C     TRACE FIELD DESIGNAMTED BY NW3 INT(OPERND(I,8)))
C     DO THE FOB
              SAVE_KDP(1)=SAVEINPT(1)
              OLDLDIF2=LDIF2
              OLDLDIF=LDIF
              IF(OPDIF) THEN
                  LDIF2=.TRUE.
                  LDIF=.TRUE.
              ELSE
                  LDIF2=.FALSE.
                  LDIF=.FALSE.
              END IF
              WC='FOB     '
              WQ='        '
              SQ=0
              SST=0
              STI=0
              W1=FIELDY(INT(OPERND(I,8)))
              W2=FIELDX(INT(OPERND(I,8)))
              W3=FIELDZ(INT(OPERND(I,8)))
C     USE WAVELENGTH FROM GREYS INPUT, NOT FROM FIELD POS INPUT
              W4=INT(OPERND(I,10))
              W5=0.0D0
              DF1=0
              DF2=0
              DF3=0
              DF4=0
              DF5=1
              S1=1
              S2=1
              S3=1
              S4=1
              S5=0
              SN=1
C     SET MSG TO FALSE
              MSG=.FALSE.
              BADOPS=.FALSE.
              CALL FFOB2
              REST_KDP(1)=RESTINPT(1)
              IF(BADOPS) THEN
                  IF(F28.EQ.1) BAAD=1
                  IF(F28.EQ.1) REG(9)=0.0D0
                  IF(F31.EQ.1) CALL MACFAL
                  RETURN
              END IF
C NOW THE GREYSPOT
              SAVE_KDP(1)=SAVEINPT(1)
              OLDLAMM=INT(OPERND(I,10))
C     OPD WEIGHT IS JUST OPERND(I,9)
              OPDWT=OPERND(I,9)
              OLDLDIF2=LDIF2
              OLDLDIF=LDIF
              IF(OPDIF) THEN
                  LDIF2=.TRUE.
                  LDIF=.TRUE.
              ELSE
                  LDIF2=.FALSE.
                  LDIF=.FALSE.
              END IF
C     SET MSG TO FALSE
              MSG=.FALSE.
              BADOPS=.FALSE.
              CALL GSPOT
              REST_KDP(1)=RESTINPT(1)
              IF(BADOPS) THEN
                  IF(F28.EQ.1) BAAD=1
                  IF(F28.EQ.1) REG(9)=0.0D0
                  IF(F31.EQ.1) CALL MACFAL
                  RETURN
              END IF
              LDIF2=OLDLDIF2
              LDIF=OLDLDIF
              IF(.NOT.REFEXT.OR..NOT.GSPDEXT) THEN
                  IF(F28.EQ.1) REG(9)=0.0D0
                  IF(F28.EQ.1) BAAD=1
                  IF(F31.EQ.1) CALL MACFAL
                  RETURN
              END IF
              OLDF=INT(OPERND(I,8))
              OLDW=0
              OLDGSPD=GSPDEXT
              OLDCFG=CFNUM
              REG(9)=GREYOP
              GO TO 777
          END IF
C
          IF(OPERND(I,17).GE.245.0D0.AND.OPERND(I,17).LE.246.0D0.OR.
     1    OPERND(I,17).GE.462.0D0.AND.OPERND(I,17).LE.463.0D0) THEN
C     CAPFN BASED OPERANDS
C     CALCULATE VALUE1 AND PLACE IN ACC
C
C     TEST IF SPOT NEEDS TO BE TRACED
C
              IF(OLDCPFN.AND.INT(OPERND(I,8)).EQ.OLDF.AND.
     1        CFNUM.EQ.OLDCFG) THEN
C     DON'T NEED TO TRACE THE CAPFN AGAIN, IT ALREDY EXITS
              ELSE
C     TRACE FOB AND SPOT THEN GET VALUE1
C     TRACE FIELD DESIGNAMTED BY NW3 INT(OPERND(I,8)))
C     DO THE FOB
                  SAVE_KDP(1)=SAVEINPT(1)
                  OLDLDIF2=LDIF2
                  OLDLDIF=LDIF
                  IF(OPDIF) THEN
                      LDIF2=.TRUE.
                      LDIF=.TRUE.
                  ELSE
                      LDIF2=.FALSE.
                      LDIF=.FALSE.
                  END IF
                  WC='FOB     '
                  WQ='        '
                  SQ=0
                  SST=0
                  STI=0
                  W1=FIELDY(INT(OPERND(I,8)))
                  W2=FIELDX(INT(OPERND(I,8)))
                  W3=FIELDZ(INT(OPERND(I,8)))
                  W4=FIELDW(INT(OPERND(I,8)))
                  W5=0.0D0
                  DF1=0
                  DF2=0
                  DF3=0
                  DF4=0
                  DF5=1
                  S1=1
                  S2=1
                  S3=1
                  S4=1
                  S5=0
                  SN=1
C     SET MSG TO FALSE
                  MSG=.FALSE.
                  BADOPS=.FALSE.
                  CALL FFOB2
                  REST_KDP(1)=RESTINPT(1)
                  IF(BADOPS) THEN
                      IF(F28.EQ.1) BAAD=1
                      IF(F28.EQ.1) REG(9)=0.0D0
                      IF(F31.EQ.1) CALL MACFAL
                      RETURN
                  END IF
C NOW THE SPOT
                  SAVE_KDP(1)=SAVEINPT(1)
C     SET MSG TO FALSE
                  MSG=.FALSE.
                  BADOPS=.FALSE.
                  NRDFACTOR=1.0D0
                  IF(OPERND(I,17).EQ.245.0D0) THEN
                      IF(INT(OPERND(I,11)).EQ.1) THEN
                          OLDIF=LDIF
                          LDIF=.FALSE.
                          CALL COMPAP(REFERR,2)
                          LDIF=OLDIF
                      ELSE
                          OLDSP(1) =SYSTEM1(31)
                          OLDSP(2) =SYSTEM1(32)
                          OLDSP(3) =SYSTEM1(33)
                          OLDSP(4) =SYSTEM1(34)
                          OLDSP(5) =SYSTEM1(35)
                          OLDSP(6) =SYSTEM1(76)
                          OLDSP(7) =SYSTEM1(77)
                          OLDSP(8) =SYSTEM1(78)
                          OLDSP(9) =SYSTEM1(79)
                          OLDSP(10)=SYSTEM1(80)
                          SYSTEM1(31:35)=0.0D0
                          SYSTEM1(76:80)=0.0D0
                          IF(INT(OPERND(I,9)).EQ.1)  SYSTEM1(31)=1.0D0
                          IF(INT(OPERND(I,9)).EQ.2)  SYSTEM1(32)=1.0D0
                          IF(INT(OPERND(I,9)).EQ.3)  SYSTEM1(33)=1.0D0
                          IF(INT(OPERND(I,9)).EQ.4)  SYSTEM1(34)=1.0D0
                          IF(INT(OPERND(I,9)).EQ.5)  SYSTEM1(35)=1.0D0
                          IF(INT(OPERND(I,9)).EQ.6)  SYSTEM1(76)=1.0D0
                          IF(INT(OPERND(I,9)).EQ.7)  SYSTEM1(77)=1.0D0
                          IF(INT(OPERND(I,9)).EQ.8)  SYSTEM1(78)=1.0D0
                          IF(INT(OPERND(I,9)).EQ.9)  SYSTEM1(79)=1.0D0
                          IF(INT(OPERND(I,9)).EQ.10) SYSTEM1(80)=1.0D0
                          OLDIF=LDIF
                          LDIF=.FALSE.
                          CALL COMPAP(REFERR,2)
                          LDIF=OLDIF
                          SYSTEM1(31)=OLDSP(1)
                          SYSTEM1(32)=OLDSP(2)
                          SYSTEM1(33)=OLDSP(3)
                          SYSTEM1(34)=OLDSP(4)
                          SYSTEM1(35)=OLDSP(5)
                          SYSTEM1(76)=OLDSP(6)
                          SYSTEM1(77)=OLDSP(7)
                          SYSTEM1(78)=OLDSP(8)
                          SYSTEM1(79)=OLDSP(9)
                          SYSTEM1(80)=OLDSP(10)
                      END IF
                  ELSE
                      OLDIF=LDIF
                      LDIF=.FALSE.
                      CALL COMPAP(REFERR,2)
                      LDIF=OLDIF
                  END IF
                  REST_KDP(1)=RESTINPT(1)
                  IF(BADOPS) THEN
                      IF(F28.EQ.1) BAAD=1
                      IF(F28.EQ.1) REG(9)=0.0D0
                      IF(F31.EQ.1) CALL MACFAL
                      RETURN
                  END IF
                  LDIF2=OLDLDIF2
                  LDIF=OLDLDIF
                  IF(.NOT.REFEXT.OR..NOT.CPFNEXT.OR.REFERR) THEN
                      IF(F28.EQ.1) REG(9)=0.0D0
                      IF(F28.EQ.1) BAAD=1
                      IF(F31.EQ.1) CALL MACFAL
                      RETURN
                  END IF
                  OLDF=INT(OPERND(I,8))
                  OLDW=0
                  OLDCPFN=CPFNEXT
                  OLDCFG=CFNUM
              END IF
              IF(OPERND(I,17).EQ.245.0D0) THEN
                  REG(9)=RMSOPD
                  GO TO 777
              END IF
              IF(OPERND(I,17).EQ.246.0D0) THEN
C     DO A FIT UNLESS ONE ALREADY EXISTS THEN GET OP VAL
                  ERROP=.FALSE.
                  OPMAP=.FALSE.

                  WVNUMOP=INT(OPERND(I,10))
                  CALL OPDLOD2
                  ERRR=ERROP
                  IF(ERRR) THEN
                      IF(F28.EQ.1) REG(9)=0.0D0
                      IF(F28.EQ.1) BAAD=1
                      IF(F31.EQ.1) CALL MACFAL
                      RETURN
                  END IF
C     FIT EXITS NOW EVAL THE OP
                  IF(.NOT.OPMAP) ERRR=.TRUE.
                  IF(OPMAP) ERRR=.FALSE.
                  IF(.NOT.OPMAP) OLDZRNFT=.FALSE.
                  IF(OPMAP) OLDZRNFT=.TRUE.
                  IF(ERRR) THEN
                      IF(F28.EQ.1) REG(9)=0.0D0
                      IF(F28.EQ.1) BAAD=1
                      IF(F31.EQ.1) CALL MACFAL
                      RETURN
                  END IF
                  REG(9)=X(INT(OPERND(I,9)))
                  GO TO 777
              END IF
              IF(OPERND(I,17).EQ.462.0D0) THEN
C     DOTF MOD
                  SAVE_KDP(5)=SAVEINPT(5)
C       CALL ROUTINE THAT CALCULATES THE DOTF MOD
                  WC='DOTF'
                  W1=OPERND(I,9)
                  DF1=0
                  S1=1
                  DF2=1
                  DF3=1
                  DF4=1
                  DF5=1
                  S2=0
                  S3=0
                  S4=0
                  S5=0
                  SQ=1
                  IF(OPERND(I,10).EQ.90.0D0) WQ='YACC    '
                  IF(OPERND(I,10).EQ.0.0D0)  WQ='XACC    '
                  STI=0
                  SST=0
                  SN=1
                  CALL DOTF
                  REST_KDP(5)=RESTINPT(5)
                  OLDF=INT(OPERND(I,8))
                  OLDW=0
                  OLDCSPD=CPFNEXT
                  OLDCFG=CFNUM
                  GO TO 777
              END IF
              IF(OPERND(I,17).EQ.463.0D0) THEN
C DOTF P
                  SAVE_KDP(5)=SAVEINPT(5)
C       CALL ROUTINE THAT CALCULATES THE DOTF PHASE
                  WC='DOTF'
                  W1=OPERND(I,9)
                  DF1=0
                  S1=1
                  DF2=1
                  DF3=1
                  DF4=1
                  DF5=1
                  S2=0
                  S3=0
                  S4=0
                  S5=0
                  SQ=1
                  IF(OPERND(I,10).EQ.90.0D0) WQ='YACC    '
                  IF(OPERND(I,10).EQ.0.0D0)  WQ='XACC    '
                  STI=0
                  SST=0
                  SN=1
                  CALL DOTF
                  REST_KDP(5)=RESTINPT(5)
                  OLDF=INT(OPERND(I,8))
                  OLDW=0
                  OLDCSPD=CPFNEXT
                  OLDCFG=CFNUM
                  REG(9)=REG(10)
                  GO TO 777
              END IF
          END IF
C
C     SPECIAL OPERANDS
          IF(OPERND(I,17).GE.247.0D0.AND.OPERND(I,17).LE.296.0D0.OR.
     1    OPERND(I,17).GE.452.0D0.AND.OPERND(I,17).LE.465.0D0) THEN
C     CALCULATE VALUE1 AND PLACE IN ACC
              CALL PRTRB
              CALL PRTRC
              CALL PRTRD
C
C     FIRST DO OPERANDS WHICH ARE PARAXIAL
C     THESE ARE NUMBER 279 TO 284
              IF(OPERND(I,17).GE.279.0D0.AND.OPERND(I,17).LE.284.0D0) THEN
C     PARAXIAL
C
C       PUPDIAX
                  IF(OPERND(I,17).EQ.279.0D0) THEN
                      W1A=OPERND(I,8)
                      W1B=OPERND(I,8)
                      IF(W1B.NE.0.0D0) W1B=W1B-1.0D0
                      V1=((-PXTRAX(5,INT(W1A)))/(PXTRAX(6,INT(W1B))))
                      REG(9)=2.0D0*(V1*PXTRAX(2,INT(W1B)))+PXTRAX(1,INT(W1A))
                      GO TO 777
                  END IF
C       PUPDIAY
                  IF(OPERND(I,17).EQ.280.0D0) THEN
                      W1A=OPERND(I,8)
                      W1B=OPERND(I,8)
                      IF(W1B.NE.0.0D0) W1B=W1B-1.0D0
                      V1=((-PXTRAY(5,INT(W1A)))/(PXTRAY(6,INT(W1B))))
                      REG(9)=2.0D0*(V1*PXTRAY(2,INT(W1B)))+PXTRAY(1,INT(W1A))
                      GO TO 777
                  END IF
C
                  IF(OPERND(I,17).EQ.281.0D0) THEN
C     PUPDISX
                      W1A=OPERND(I,8)
                      W1B=OPERND(I,8)
                      IF(W1B.NE.0.0D0) W1B=W1B-1.0D0
                      REG(9)=((-PXTRAX(5,INT(W1A)))/(PXTRAX(6,INT(W1B))))
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.282.0D0) THEN
C     PUPDISY
                      W1A=OPERND(I,8)
                      W1B=OPERND(I,8)
                      IF(W1B.NE.0.0D0) W1B=W1B-1.0D0
                      REG(9)=((-PXTRAY(5,INT(W1A)))/(PXTRAY(6,INT(W1B))))
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.283.0D0) THEN
C     CHFIMX
                      W1A=OPERND(I,8)
                      W1B=OPERND(I,8)
                      IF(W1B.NE.0.0D0) W1B=W1B-1.0D0
                      V1=((-PXTRAX(1,INT(W1A)))/(PXTRAX(2,INT(W1B))))
                      REG(9)=(V1*PXTRAX(6,INT(W1B)))+PXTRAX(5,INT(W1A))
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.284.0D0) THEN
C     CHFIMY
                      W1A=OPERND(I,8)
                      W1B=OPERND(I,8)
                      IF(W1B.NE.0.0D0) W1B=W1B-1.0D0
                      V1=((-PXTRAY(1,INT(W1A)))/(PXTRAY(2,INT(W1B))))
                      REG(9)=(V1*PXTRAY(6,INT(W1B)))+PXTRAY(5,INT(W1A))
                      GO TO 777
                  END IF
C
              END IF
C     NEXT DO OPERANDS WHICH NOT PARAXIAL
              IF(OPERND(I,17).GE.247.0D0.AND.OPERND(I,17).LE.278.0D0.OR.
     1        OPERND(I,17).GE.285.0D0.AND.OPERND(I,17).LE.296.0D0.OR.
     2        OPERND(I,17).GE.452.0D0.AND.OPERND(I,17).LE.465.0D0) THEN
C     NON-PARAXIAL STUFF
C     TEST IF FOB AND RAY O O NEED TO BE TRACED
C
                  IF(OPERND(I,17).GE.247.0D0.AND.OPERND(I,17).LE.278.0D0.AND.
     1            REFEXT.AND.RAYEXT.AND.INT(OPERND(I,8)).EQ.OLDF.AND.
     1            CFNUM.EQ.OLDCFG.OR.
     1            OPERND(I,17).GE.293.0D0.AND.OPERND(I,17).LE.296.0D0.AND.
     1            REFEXT.AND.RAYEXT.AND.INT(OPERND(I,8)).EQ.OLDF.AND.
     1            CFNUM.EQ.OLDCFG.OR.
     1            OPERND(I,17).GE.452.0D0.AND.OPERND(I,17).LE.459.0D0.AND.
     1            REFEXT.AND.RAYEXT.AND.INT(OPERND(I,8)).EQ.OLDF.AND.
     1            CFNUM.EQ.OLDCFG.OR.
     1            OPERND(I,17).EQ.466.0D0.AND.
     1            REFEXT.AND.RAYEXT.AND.INT(OPERND(I,8)).EQ.OLDF.AND.
     1            CFNUM.EQ.OLDCFG.OR.
     1            OPERND(I,17).GE.285.0D0.AND.OPERND(I,17).LE.292.0D0.AND.
     1            REFEXT.AND.RAYEXT.AND.INT(OPERND(I,9)).EQ.OLDF.AND.
     1            CFNUM.EQ.OLDCFG) THEN
C
C     DON'T NEED TO TRACE THE RAY AGAIN, IT ALREDY EXITS
                  ELSE
C     TRACE FOB AND A RAY 0 0 1 THEN GET VALUE1
C     TRACE FIELD DESIGNAMTED BY NW3 INT(OPERND(I,8)))
C     OR NW4 INT(OPERND(I,9))
C     TRACE RAY 0 0
C     FOR REFERENCE RAY STUFF, RAY 0 0 IS ALWAYS TRACED
C     DO THE FOB
                      SAVE_KDP(1)=SAVEINPT(1)
                      OLDLDIF2=LDIF2
                      OLDLDIF=LDIF
                      IF(OPDIF) THEN
                          LDIF2=.TRUE.
                          LDIF=.TRUE.
                      ELSE
                          LDIF2=.FALSE.
                          LDIF=.FALSE.
                      END IF
                      WC='FOB     '
                      WQ='        '
                      SQ=0
                      SST=0
                      STI=0
                      IF(OPERND(I,17).GE.247.0D0.AND.OPERND(I,17).LE.278.0D0.OR.
     1                OPERND(I,17).GE.293.AND.OPERND(I,17).LE.296.0D0.OR.
     2                OPERND(I,17).EQ.466.0D0) THEN
                          W1=FIELDY(INT(OPERND(I,8)))
                          W2=FIELDX(INT(OPERND(I,8)))
                          W3=FIELDZ(INT(OPERND(I,8)))
                          W4=FIELDW(INT(OPERND(I,8)))
                      END IF
                      IF(OPERND(I,17).GE.285.0D0.AND.OPERND(I,17).LE.292.0D0.OR.
     1                OPERND(I,17).GE.452.0D0.AND.OPERND(I,17).LE.465.0D0) THEN
                          W1=FIELDY(INT(OPERND(I,9)))
                          W2=FIELDX(INT(OPERND(I,9)))
                          W3=FIELDZ(INT(OPERND(I,9)))
                          W4=FIELDW(INT(OPERND(I,9)))
                      END IF
                      W5=0.0D0
                      DF1=0
                      DF2=0
                      DF3=0
                      DF4=0
                      DF5=1
                      S1=1
                      S2=1
                      S3=1
                      S4=1
                      S5=0
                      SN=1
C     SET MSG TO FALSE
                      MSG=.FALSE.
                      BADOPS=.FALSE.
                      CALL FFOB2
                      REST_KDP(1)=RESTINPT(1)
                      IF(BADOPS) THEN
                          IF(F28.EQ.1) BAAD=1
                          IF(F28.EQ.1) REG(9)=0.0D0
                          IF(F31.EQ.1) CALL MACFAL
                          RETURN
                      END IF
C NOW THE RAY 0 0
                      SAVE_KDP(1)=SAVEINPT(1)
                      WC='RAY     '
                      WQ='        '
                      SQ=0
                      SST=0
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
C     SET MSG TO FALSE
                      MSG=.FALSE.
                      BADOPS=.FALSE.
                      NOCOAT=.TRUE.
                      GRASET=.FALSE.
                      DXFSET=.FALSE.
                      CALL RRAY2
                      CACOCH=0
                      REST_KDP(1)=RESTINPT(1)
                      IF(BADOPS) THEN
                          IF(F28.EQ.1) BAAD=1
                          IF(F28.EQ.1) REG(9)=0.0D0
                          IF(F31.EQ.1) CALL MACFAL
                          RETURN
                      END IF
                      LDIF2=OLDLDIF2
                      LDIF=OLDLDIF
                      IF(.NOT.REFEXT.OR..NOT.RAYEXT) THEN
                          IF(F28.EQ.1) REG(9)=0.0D0
                          IF(F28.EQ.1) BAAD=1
                          IF(F31.EQ.1) CALL MACFAL
                          RETURN
                      END IF
C     NOW CALCULATE PUPIL STUFF
C     NOW CALL AUXFOB TO CALCULATE DIFFERENTIAL RAY BASED FFL,BFL AND MAGS
                      CALL LASTRAY(1)
                      CALL AUXFOB()
                      CALL LASTRAY(2)
C
                      IF(OPERND(I,17).GE.247.0D0.AND.OPERND(I,17).LE.278.0D0.OR.
     1                OPERND(I,17).GE.293.AND.OPERND(I,17).LE.296.0D0.OR.
     2                OPERND(I,17).EQ.466.0D0) THEN
                          OLDF=INT(OPERND(I,8))
                          OLDW=0
                      END IF
                      IF(OPERND(I,17).GE.285.0D0.AND.OPERND(I,17).LE.292.0D0.OR.
     1                OPERND(I,17).GE.452.0D0.AND.OPERND(I,17).LE.465.0D0) THEN
                          OLDF=INT(OPERND(I,9))
                          OLDW=0
                          OLDW=0
                      END IF
                      OLDCFG=CFNUM
                  END IF
C
C     NOW CALC THE OPS
                  IF(OPERND(I,17).EQ.247.0D0) THEN
                      REG(9)=RMAGX
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.248.0D0) THEN
                      REG(9)=RMAGY
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.249.0D0) THEN
                      REG(9)=MAGXOR
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.250.0D0) THEN
                      REG(9)=MAGYOR
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.251.0D0) THEN
                      REG(9)=RFFLX
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.252.0D0) THEN
                      REG(9)=RFFLY
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.253.0D0) THEN
                      REG(9)=RBFLX
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.254.0D0) THEN
                      REG(9)=RBFLY
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.255.0D0) THEN
                      REG(9)=RFFNX
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.256.0D0) THEN
                      REG(9)=RFFNY
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.257.0D0) THEN
                      REG(9)=RBFNX
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.258.0D0) THEN
                      REG(9)=RBFNY
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.259.0D0) THEN
                      REG(9)=REFLX
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.260.0D0) THEN
                      REG(9)=REFLY
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.261.0D0) THEN
                      REG(9)=ENDIAX
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.262.0D0) THEN
                      REG(9)=ENDIAY
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.263.0D0) THEN
                      REG(9)=EXDIAX
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.264.0D0) THEN
                      REG(9)=EXDIAY
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.265.0D0) THEN
                      REG(9)=ENPUX
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.266.0D0) THEN
                      REG(9)=ENPUY
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.267.0D0) THEN
                      REG(9)=ENPUZ
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.268.0D0) THEN
                      REG(9)=EXPUX
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.269.0D0) THEN
                      REG(9)=EXPUY
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.270.0D0) THEN
                      REG(9)=EXPUZ
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.271.0D0) THEN
C     FNUMX
                      ERRR=.FALSE.
                      MSG=.FALSE.
                      CACOCH=0
                      CALL FNUMX(V1,ERRR)
                      IF(ERRR) THEN
                          IF(F28.EQ.1) REG(9)=0.0D0
                          IF(F28.EQ.1) BAAD=1
                          IF(F31.EQ.1) CALL MACFAL
                          RETURN
                      END IF
                      REG(9)=V1
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.272.0D0) THEN
C     FNUMY
                      ERRR=.FALSE.
                      MSG=.FALSE.
                      CACOCH=0
                      CALL FNUMY(V1,ERRR)
                      IF(ERRR) THEN
                          IF(F28.EQ.1) REG(9)=0.0D0
                          IF(F28.EQ.1) BAAD=1
                          IF(F31.EQ.1) CALL MACFAL
                          RETURN
                      END IF
                      REG(9)=V1
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.273.0D0) THEN
C     OBFNUMX
                      ERRR=.FALSE.
                      MSG=.FALSE.
                      CACOCH=0
                      CALL OBFNUMX(V1,ERRR)
                      IF(ERRR) THEN
                          IF(F28.EQ.1) REG(9)=0.0D0
                          IF(F28.EQ.1) BAAD=1
                          IF(F31.EQ.1) CALL MACFAL
                          RETURN
                      END IF
                      REG(9)=V1
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.274.0D0) THEN
C     OBFNUMY
                      ERRR=.FALSE.
                      MSG=.FALSE.
                      CACOCH=0
                      CALL OBFNUMY(V1,ERRR)
                      IF(ERRR) THEN
                          IF(F28.EQ.1) REG(9)=0.0D0
                          IF(F28.EQ.1) BAAD=1
                          IF(F31.EQ.1) CALL MACFAL
                          RETURN
                      END IF
                      REG(9)=V1
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.275.0D0) THEN
C     ENPDIAX
                      ERRR=.FALSE.
                      MSG=.FALSE.
                      CACOCH=0
                      CALL ENPDIAX(V1,ERRR)
                      IF(ERRR) THEN
                          IF(F28.EQ.1) REG(9)=0.0D0
                          IF(F28.EQ.1) BAAD=1
                          IF(F31.EQ.1) CALL MACFAL
                          RETURN
                      END IF
                      REG(9)=V1
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.276.0D0) THEN
C     ENPDIAY
                      ERRR=.FALSE.
                      MSG=.FALSE.
                      CACOCH=0
                      CALL ENPDIAY(V1,ERRR)
                      IF(ERRR) THEN
                          IF(F28.EQ.1) REG(9)=0.0D0
                          IF(F28.EQ.1) BAAD=1
                          IF(F31.EQ.1) CALL MACFAL
                          RETURN
                      END IF
                      REG(9)=V1
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.277.0D0) THEN
C     EXPDIAX
                      ERRR=.FALSE.
                      MSG=.FALSE.
                      CACOCH=0
                      CALL EXPDIAX(V1,ERRR)
                      IF(ERRR) THEN
                          IF(F28.EQ.1) REG(9)=0.0D0
                          IF(F28.EQ.1) BAAD=1
                          IF(F31.EQ.1) CALL MACFAL
                          RETURN
                      END IF
                      REG(9)=V1
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.278.0D0) THEN
C     EXPDIAY
                      ERRR=.FALSE.
                      MSG=.FALSE.
                      CACOCH=0
                      CALL EXPDIAY(V1,ERRR)
                      IF(ERRR) THEN
                          IF(F28.EQ.1) REG(9)=0.0D0
                          IF(F28.EQ.1) BAAD=1
                          IF(F31.EQ.1) CALL MACFAL
                          RETURN
                      END IF
                      REG(9)=V1
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.285.0D0) THEN
C     GPX
                      ERRR=.FALSE.
                      NWN1=FIELDY(INT(OPERND(I,9)))
                      NWN2=FIELDX(INT(OPERND(I,9)))
                      NWN3=FIELDZ(INT(OPERND(I,9)))
                      NWN4=FIELDW(INT(OPERND(I,9)))
                      CALL GNPR2(INT(OPERND(I,8)),NWN1,NWN2,NWN3,NWN4,ERRR,0)
                      IF(ERRR) THEN
                          IF(F28.EQ.1) REG(9)=0.0D0
                          IF(F28.EQ.1) BAAD=1
                          IF(F31.EQ.1) CALL MACFAL
                          RETURN
                      END IF
                      REG(9)=VALUE1
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.286.0D0) THEN
C     GPY
                      ERRR=.FALSE.
                      NWN1=FIELDY(INT(OPERND(I,9)))
                      NWN2=FIELDX(INT(OPERND(I,9)))
                      NWN3=FIELDZ(INT(OPERND(I,9)))
                      NWN4=FIELDW(INT(OPERND(I,9)))
                      CALL GNPR1(INT(OPERND(I,8)),NWN1,NWN2,NWN3,NWN4,ERRR,0)
                      IF(ERRR) THEN
                          IF(F28.EQ.1) REG(9)=0.0D0
                          IF(F28.EQ.1) BAAD=1
                          IF(F31.EQ.1) CALL MACFAL
                          RETURN
                      END IF
                      REG(9)=VALUE1
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.287.0D0) THEN
C     GPUX
                      ERRR=.FALSE.
                      NWN1=FIELDY(INT(OPERND(I,9)))
                      NWN2=FIELDX(INT(OPERND(I,9)))
                      NWN3=FIELDZ(INT(OPERND(I,9)))
                      NWN4=FIELDW(INT(OPERND(I,9)))
                      CALL GNPR4(INT(OPERND(I,8)),NWN1,NWN2,NWN3,NWN4,ERRR,0)
                      IF(ERRR) THEN
                          IF(F28.EQ.1) REG(9)=0.0D0
                          IF(F28.EQ.1) BAAD=1
                          IF(F31.EQ.1) CALL MACFAL
                          RETURN
                      END IF
                      REG(9)=VALUE1
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.288.0D0) THEN
C     GPUY
                      ERRR=.FALSE.
                      NWN1=FIELDY(INT(OPERND(I,9)))
                      NWN2=FIELDX(INT(OPERND(I,9)))
                      NWN3=FIELDZ(INT(OPERND(I,9)))
                      NWN4=FIELDW(INT(OPERND(I,9)))
                      CALL GNPR3(INT(OPERND(I,8)),NWN1,NWN2,NWN3,NWN4,ERRR,0)
                      IF(ERRR) THEN
                          IF(F28.EQ.1) REG(9)=0.0D0
                          IF(F28.EQ.1) BAAD=1
                          IF(F31.EQ.1) CALL MACFAL
                          RETURN
                      END IF
                      REG(9)=VALUE1
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.289.0D0) THEN
C     GPCX
                      ERRR=.FALSE.
                      NWN1=FIELDY(INT(OPERND(I,9)))
                      NWN2=FIELDX(INT(OPERND(I,9)))
                      NWN3=FIELDZ(INT(OPERND(I,9)))
                      NWN4=FIELDW(INT(OPERND(I,9)))
                      CALL GNPR6(INT(OPERND(I,8)),NWN1,NWN2,NWN3,NWN4,ERRR,0)
                      IF(ERRR) THEN
                          IF(F28.EQ.1) REG(9)=0.0D0
                          IF(F28.EQ.1) BAAD=1
                          IF(F31.EQ.1) CALL MACFAL
                          RETURN
                      END IF
                      REG(9)=VALUE1
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.290.0D0) THEN
C     GPCY
                      ERRR=.FALSE.
                      NWN1=FIELDY(INT(OPERND(I,9)))
                      NWN2=FIELDX(INT(OPERND(I,9)))
                      NWN3=FIELDZ(INT(OPERND(I,9)))
                      NWN4=FIELDW(INT(OPERND(I,9)))
                      CALL GNPR5(INT(OPERND(I,8)),NWN1,NWN2,NWN3,NWN4,ERRR,0)
                      IF(ERRR) THEN
                          IF(F28.EQ.1) REG(9)=0.0D0
                          IF(F28.EQ.1) BAAD=1
                          IF(F31.EQ.1) CALL MACFAL
                          RETURN
                      END IF
                      REG(9)=VALUE1
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.291.0D0) THEN
C     GPUCX
                      ERRR=.FALSE.
                      NWN1=FIELDY(INT(OPERND(I,9)))
                      NWN2=FIELDX(INT(OPERND(I,9)))
                      NWN3=FIELDZ(INT(OPERND(I,9)))
                      NWN4=FIELDW(INT(OPERND(I,9)))
                      CALL GNPR8(INT(OPERND(I,8)),NWN1,NWN2,NWN3,NWN4,ERRR,0)
                      IF(ERRR) THEN
                          IF(F28.EQ.1) REG(9)=0.0D0
                          IF(F28.EQ.1) BAAD=1
                          IF(F31.EQ.1) CALL MACFAL
                          RETURN
                      END IF
                      REG(9)=VALUE1
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.292.0D0) THEN
C     GPUCY
                      ERRR=.FALSE.
                      NWN1=FIELDY(INT(OPERND(I,9)))
                      NWN2=FIELDX(INT(OPERND(I,9)))
                      NWN3=FIELDZ(INT(OPERND(I,9)))
                      NWN4=FIELDW(INT(OPERND(I,9)))
                      CALL GNPR7(INT(OPERND(I,8)),NWN1,NWN2,NWN3,NWN4,ERRR,0)
                      IF(ERRR) THEN
                          IF(F28.EQ.1) REG(9)=0.0D0
                          IF(F28.EQ.1) BAAD=1
                          IF(F31.EQ.1) CALL MACFAL
                          RETURN
                      END IF
                      REG(9)=VALUE1
                      GO TO 777
                  END IF
C     NOW THE GAUSSIAN BEAM OPERANDS
                  IF(OPERND(I,17).EQ.452.0D0) THEN
C     GBRADX
                      OLDOBJ=NEWOBJ
                      OLDIMG=NEWIMG
                      OLDREF=NEWREF
                      NEWREF=1
                      NEWIMG=INT(SYSTEM1(20))
                      NEWOBJ=0
                      ERRR=.FALSE.
                      NWN1=FIELDY(INT(OPERND(I,9)))
                      NWN2=FIELDX(INT(OPERND(I,9)))
                      NWN3=FIELDZ(INT(OPERND(I,9)))
                      NWN4=FIELDW(INT(OPERND(I,9)))
                      CALL GNPR2(INT(OPERND(I,8)),NWN1,NWN2,NWN3,NWN4,ERRR,1)
                      JPX=VALUE1
                      CALL GNPR6(INT(OPERND(I,8)),NWN1,NWN2,NWN3,NWN4,ERRR,1)
                      JPCX=VALUE1
                      IF(ERRR) THEN
                          IF(F28.EQ.1) REG(9)=0.0D0
                          IF(F28.EQ.1) BAAD=1
                          IF(F31.EQ.1) CALL MACFAL
                          RETURN
                      END IF
                      REG(9)=DSQRT((JPX**2)+(JPCX**2))
                      NEWREF=OLDREF
                      NEWIMG=OLDIMG
                      NEWOBJ=OLDOBJ
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.453.0D0) THEN
C     GBRADY
                      OLDOBJ=NEWOBJ
                      OLDIMG=NEWIMG
                      OLDREF=NEWREF
                      NEWREF=1
                      NEWIMG=INT(SYSTEM1(20))
                      NEWOBJ=0
                      ERRR=.FALSE.
                      NWN1=FIELDY(INT(OPERND(I,9)))
                      NWN2=FIELDX(INT(OPERND(I,9)))
                      NWN3=FIELDZ(INT(OPERND(I,9)))
                      NWN4=FIELDW(INT(OPERND(I,9)))
                      CALL GNPR1(INT(OPERND(I,8)),NWN1,NWN2,NWN3,NWN4,ERRR,1)
                      JPY=VALUE1
                      CALL GNPR5(INT(OPERND(I,8)),NWN1,NWN2,NWN3,NWN4,ERRR,1)
                      JPCY=VALUE1
                      IF(ERRR) THEN
                          IF(F28.EQ.1) REG(9)=0.0D0
                          IF(F28.EQ.1) BAAD=1
                          IF(F31.EQ.1) CALL MACFAL
                          RETURN
                      END IF
                      REG(9)=DSQRT((JPY**2)+(JPCY**2))
                      NEWREF=OLDREF
                      NEWIMG=OLDIMG
                      NEWOBJ=OLDOBJ
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.454.0D0) THEN
C     GBDISX
                      OLDOBJ=NEWOBJ
                      OLDIMG=NEWIMG
                      OLDREF=NEWREF
                      NEWREF=1
                      NEWIMG=INT(SYSTEM1(20))
                      NEWOBJ=0
                      ERRR=.FALSE.
                      NWN1=FIELDY(INT(OPERND(I,9)))
                      NWN2=FIELDX(INT(OPERND(I,9)))
                      NWN3=FIELDZ(INT(OPERND(I,9)))
                      NWN4=FIELDW(INT(OPERND(I,9)))
                      CALL GNPR2(INT(OPERND(I,8)),NWN1,NWN2,NWN3,NWN4,ERRR,1)
                      JPX=VALUE1
                      CALL GNPR4(INT(OPERND(I,8)),NWN1,NWN2,NWN3,NWN4,ERRR,1)
                      JPUX=VALUE1
                      CALL GNPR6(INT(OPERND(I,8)),NWN1,NWN2,NWN3,NWN4,ERRR,1)
                      JPCX=VALUE1
                      CALL GNPR8(INT(OPERND(I,8)),NWN1,NWN2,NWN3,NWN4,ERRR,1)
                      JPUCX=VALUE1
                      IF(ERRR) THEN
                          IF(F28.EQ.1) REG(9)=0.0D0
                          IF(F28.EQ.1) BAAD=1
                          IF(F31.EQ.1) CALL MACFAL
                          RETURN
                      END IF
                      IF(((JPUX**2)+(JPUCX**2)).EQ.0.0D0) THEN
                          NEWREF=OLDREF
                          NEWIMG=OLDIMG
                          NEWOBJ=OLDOBJ
                          IF(F28.EQ.1) REG(9)=0.0D0
                          IF(F28.EQ.1) BAAD=1
                          IF(F31.EQ.1) CALL MACFAL
                          RETURN
                      ELSE
                          REG(9)=
     1                    -((JPX*JPUX)+(JPCX*JPUCX))/((JPUX**2)+(JPUCX**2))
                      END IF
                      NEWREF=OLDREF
                      NEWIMG=OLDIMG
                      NEWOBJ=OLDOBJ
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.455.0D0) THEN
C     GBDISY
                      OLDOBJ=NEWOBJ
                      OLDIMG=NEWIMG
                      OLDREF=NEWREF
                      NEWREF=1
                      NEWIMG=INT(SYSTEM1(20))
                      NEWOBJ=0
                      ERRR=.FALSE.
                      NWN1=FIELDY(INT(OPERND(I,9)))
                      NWN2=FIELDX(INT(OPERND(I,9)))
                      NWN3=FIELDZ(INT(OPERND(I,9)))
                      NWN4=FIELDW(INT(OPERND(I,9)))
                      CALL GNPR1(INT(OPERND(I,8)),NWN1,NWN2,NWN3,NWN4,ERRR,1)
                      JPY=VALUE1
                      CALL GNPR3(INT(OPERND(I,8)),NWN1,NWN2,NWN3,NWN4,ERRR,1)
                      JPUY=VALUE1
                      CALL GNPR5(INT(OPERND(I,8)),NWN1,NWN2,NWN3,NWN4,ERRR,1)
                      JPCY=VALUE1
                      CALL GNPR7(INT(OPERND(I,8)),NWN1,NWN2,NWN3,NWN4,ERRR,1)
                      JPUCY=VALUE1
                      IF(ERRR) THEN
                          IF(F28.EQ.1) REG(9)=0.0D0
                          IF(F28.EQ.1) BAAD=1
                          IF(F31.EQ.1) CALL MACFAL
                          RETURN
                      END IF
                      IF(((JPUY**2)+(JPUCY**2)).EQ.0.0D0) THEN
                          NEWREF=OLDREF
                          NEWIMG=OLDIMG
                          NEWOBJ=OLDOBJ
                          IF(F28.EQ.1) REG(9)=0.0D0
                          IF(F28.EQ.1) BAAD=1
                          IF(F31.EQ.1) CALL MACFAL
                          RETURN
                      ELSE
                          REG(9)=
     1                    -((JPY*JPUY)+(JPCY*JPUCY))/((JPUY**2)+(JPUCY**2))
                      END IF
                      NEWREF=OLDREF
                      NEWIMG=OLDIMG
                      NEWOBJ=OLDOBJ
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.456.0D0) THEN
C     GBRCVX
                      OLDOBJ=NEWOBJ
                      OLDIMG=NEWIMG
                      OLDREF=NEWREF
                      NEWREF=1
                      NEWIMG=INT(SYSTEM1(20))
                      NEWOBJ=0
                      ERRR=.FALSE.
                      NWN1=FIELDY(INT(OPERND(I,9)))
                      NWN2=FIELDX(INT(OPERND(I,9)))
                      NWN3=FIELDZ(INT(OPERND(I,9)))
                      NWN4=FIELDW(INT(OPERND(I,9)))
                      CALL GNPR2(INT(OPERND(I,8)),NWN1,NWN2,NWN3,NWN4,ERRR,1)
                      JPX=VALUE1
                      CALL GNPR4(INT(OPERND(I,8)),NWN1,NWN2,NWN3,NWN4,ERRR,1)
                      JPUX=VALUE1
                      CALL GNPR6(INT(OPERND(I,8)),NWN1,NWN2,NWN3,NWN4,ERRR,1)
                      JPCX=VALUE1
                      CALL GNPR8(INT(OPERND(I,8)),NWN1,NWN2,NWN3,NWN4,ERRR,1)
                      JPUCX=VALUE1
                      IF(ERRR) THEN
                          IF(F28.EQ.1) REG(9)=0.0D0
                          IF(F28.EQ.1) BAAD=1
                          IF(F31.EQ.1) CALL MACFAL
                          RETURN
                      END IF
                      IF(((JPX**2)+(JPCX**2)).EQ.0.0D0) THEN
                          NEWREF=OLDREF
                          NEWIMG=OLDIMG
                          NEWOBJ=OLDOBJ
                          IF(F28.EQ.1) REG(9)=0.0D0
                          IF(F28.EQ.1) BAAD=1
                          IF(F31.EQ.1) CALL MACFAL
                          RETURN
                      END IF
                      V1=((JPX*JPUX)+(JPCX*JPUCX))/DSQRT((JPX**2)+(JPCX**2))
                      IF((V1).EQ.0.0D0) THEN
                          NEWREF=OLDREF
                          NEWIMG=OLDIMG
                          NEWOBJ=OLDOBJ
                          IF(F28.EQ.1) REG(9)=0.0D0
                          IF(F28.EQ.1) BAAD=1
                          IF(F31.EQ.1) CALL MACFAL
                          RETURN
                      END IF
                      REG(9)=-DSQRT((JPX**2)+(JPCX**2))/V1
                      NEWREF=OLDREF
                      NEWIMG=OLDIMG
                      NEWOBJ=OLDOBJ
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.457.0D0) THEN
C     GBRCVY
                      OLDOBJ=NEWOBJ
                      OLDIMG=NEWIMG
                      OLDREF=NEWREF
                      NEWREF=1
                      NEWIMG=INT(SYSTEM1(20))
                      NEWOBJ=0
                      ERRR=.FALSE.
                      NWN1=FIELDY(INT(OPERND(I,9)))
                      NWN2=FIELDX(INT(OPERND(I,9)))
                      NWN3=FIELDZ(INT(OPERND(I,9)))
                      NWN4=FIELDW(INT(OPERND(I,9)))
                      CALL GNPR1(INT(OPERND(I,8)),NWN1,NWN2,NWN3,NWN4,ERRR,1)
                      JPY=VALUE1
                      CALL GNPR3(INT(OPERND(I,8)),NWN1,NWN2,NWN3,NWN4,ERRR,1)
                      JPUY=VALUE1
                      CALL GNPR5(INT(OPERND(I,8)),NWN1,NWN2,NWN3,NWN4,ERRR,1)
                      JPCY=VALUE1
                      CALL GNPR7(INT(OPERND(I,8)),NWN1,NWN2,NWN3,NWN4,ERRR,1)
                      JPUCY=VALUE1
                      IF(ERRR) THEN
                          IF(F28.EQ.1) REG(9)=0.0D0
                          IF(F28.EQ.1) BAAD=1
                          IF(F31.EQ.1) CALL MACFAL
                          RETURN
                      END IF
                      IF(((JPY**2)+(JPCY**2)).EQ.0.0D0) THEN
                          NEWREF=OLDREF
                          NEWIMG=OLDIMG
                          NEWOBJ=OLDOBJ
                          IF(F28.EQ.1) REG(9)=0.0D0
                          IF(F28.EQ.1) BAAD=1
                          IF(F31.EQ.1) CALL MACFAL
                          RETURN
                      END IF
                      V1=((JPY*JPUY)+(JPCY*JPUCY))/DSQRT((JPY**2)+(JPCY**2))
                      IF((V1).EQ.0.0D0) THEN
                          NEWREF=OLDREF
                          NEWIMG=OLDIMG
                          NEWOBJ=OLDOBJ
                          IF(F28.EQ.1) REG(9)=0.0D0
                          IF(F28.EQ.1) BAAD=1
                          IF(F31.EQ.1) CALL MACFAL
                          RETURN
                      END IF
                      REG(9)=-DSQRT((JPY**2)+(JPCY**2))/V1
                      NEWREF=OLDREF
                      NEWIMG=OLDIMG
                      NEWOBJ=OLDOBJ
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.458.0D0) THEN
C     GWAISTX
                      OLDOBJ=NEWOBJ
                      OLDIMG=NEWIMG
                      OLDREF=NEWREF
                      NEWREF=1
                      NEWIMG=INT(SYSTEM1(20))
                      NEWOBJ=0
                      ERRR=.FALSE.
                      NWN1=FIELDY(INT(OPERND(I,9)))
                      NWN2=FIELDX(INT(OPERND(I,9)))
                      NWN3=FIELDZ(INT(OPERND(I,9)))
                      NWN4=FIELDW(INT(OPERND(I,9)))
                      CALL GNPR2(INT(OPERND(I,8)),NWN1,NWN2,NWN3,NWN4,ERRR,1)
                      JPX=VALUE1
                      CALL GNPR4(INT(OPERND(I,8)),NWN1,NWN2,NWN3,NWN4,ERRR,1)
                      JPUX=VALUE1
                      CALL GNPR6(INT(OPERND(I,8)),NWN1,NWN2,NWN3,NWN4,ERRR,1)
                      JPCX=VALUE1
                      CALL GNPR8(INT(OPERND(I,8)),NWN1,NWN2,NWN3,NWN4,ERRR,1)
                      JPUCX=VALUE1
                      IF(ERRR) THEN
                          IF(F28.EQ.1) REG(9)=0.0D0
                          IF(F28.EQ.1) BAAD=1
                          IF(F31.EQ.1) CALL MACFAL
                          RETURN
                      END IF
                      IF(((JPUX**2)+(JPUCX**2)).EQ.0.0D0) THEN
                          NEWREF=OLDREF
                          NEWIMG=OLDIMG
                          NEWOBJ=OLDOBJ
                          IF(F28.EQ.1) REG(9)=0.0D0
                          IF(F28.EQ.1) BAAD=1
                          IF(F31.EQ.1) CALL MACFAL
                          RETURN
                      END IF
                      V1=
     1                -((JPX*JPUX)+(JPCX*JPUCX))/((JPUX**2)+(JPUCX**2))
                      REG(9)=DSQRT(((JPX+(JPUX*V1))**2)+((JPCX+(JPUCX*V1))**2))
                      NEWREF=OLDREF
                      NEWIMG=OLDIMG
                      NEWOBJ=OLDOBJ
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.459.0D0) THEN
C     GWAISTY
                      OLDOBJ=NEWOBJ
                      OLDIMG=NEWIMG
                      OLDREF=NEWREF
                      NEWREF=1
                      NEWIMG=INT(SYSTEM1(20))
                      NEWOBJ=0
                      ERRR=.FALSE.
                      NWN1=FIELDY(INT(OPERND(I,9)))
                      NWN2=FIELDX(INT(OPERND(I,9)))
                      NWN3=FIELDZ(INT(OPERND(I,9)))
                      NWN4=FIELDW(INT(OPERND(I,9)))
                      CALL GNPR1(INT(OPERND(I,8)),NWN1,NWN2,NWN3,NWN4,ERRR,1)
                      JPY=VALUE1
                      CALL GNPR3(INT(OPERND(I,8)),NWN1,NWN2,NWN3,NWN4,ERRR,1)
                      JPUY=VALUE1
                      CALL GNPR5(INT(OPERND(I,8)),NWN1,NWN2,NWN3,NWN4,ERRR,1)
                      JPCY=VALUE1
                      CALL GNPR7(INT(OPERND(I,8)),NWN1,NWN2,NWN3,NWN4,ERRR,1)
                      JPUCY=VALUE1
                      IF(ERRR) THEN
                          IF(F28.EQ.1) REG(9)=0.0D0
                          IF(F28.EQ.1) BAAD=1
                          IF(F31.EQ.1) CALL MACFAL
                          RETURN
                      END IF
                      IF(((JPUY**2)+(JPUCY**2)).EQ.0.0D0) THEN
                          NEWREF=OLDREF
                          NEWIMG=OLDIMG
                          NEWOBJ=OLDOBJ
                          IF(F28.EQ.1) REG(9)=0.0D0
                          IF(F28.EQ.1) BAAD=1
                          IF(F31.EQ.1) CALL MACFAL
                          RETURN
                      END IF
                      V1=
     1                -((JPY*JPUY)+(JPCY*JPUCY))/((JPUY**2)+(JPUCY**2))
                      REG(9)=DSQRT(((JPY+(JPUY*V1))**2)+((JPCY+(JPUCY*V1))**2))
                      NEWREF=OLDREF
                      NEWIMG=OLDIMG
                      NEWOBJ=OLDOBJ
                      GO TO 777
                  END IF
C
                  IF(OPERND(I,17).EQ.293.0D0) THEN
C     DISTORTION
                      ERRR=.FALSE.
                      CALL DISTOP(INT(OPERND(I,8)),ERRR)
                      IF(ERRR) THEN
                          IF(F28.EQ.1) REG(9)=0.0D0
                          IF(F28.EQ.1) BAAD=1
                          IF(F31.EQ.1) CALL MACFAL
                          RETURN
                      END IF
                      REG(9)=VALUE1
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.466.0D0) THEN
C     DISTORTION
                      ERRR=.FALSE.
                      CALL FDISTOP(INT(OPERND(I,8)),ERRR)
                      IF(ERRR) THEN
                          IF(F28.EQ.1) REG(9)=0.0D0
                          IF(F28.EQ.1) BAAD=1
                          IF(F31.EQ.1) CALL MACFAL
                          RETURN
                      END IF
                      REG(9)=VALUE1
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.294.0D0) THEN
C     XFOC
                      ERRR=.FALSE.
                      ORI=0
                      CALL FLDOP(ORI,INT(OPERND(I,8)),ERRR)
                      IF(ERRR) THEN
                          IF(F28.EQ.1) REG(9)=0.0D0
                          IF(F28.EQ.1) BAAD=1
                          IF(F31.EQ.1) CALL MACFAL
                          RETURN
                      END IF
                      REG(9)=VALUE1
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.295.0D0) THEN
C     YFOC
                      ERRR=.FALSE.
                      ORI=1
                      CALL FLDOP(ORI,INT(OPERND(I,8)),ERRR)
                      IF(ERRR) THEN
                          IF(F28.EQ.1) REG(9)=0.0D0
                          IF(F28.EQ.1) BAAD=1
                          IF(F31.EQ.1) CALL MACFAL
                          RETURN
                      END IF
                      REG(9)=VALUE1
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.296.0D0) THEN
C     AST
                      ERRR=.FALSE.
                      ORI=2
                      CALL FLDOP(ORI,INT(OPERND(I,8)),ERRR)
                      IF(ERRR) THEN
                          IF(F28.EQ.1) REG(9)=0.0D0
                          IF(F28.EQ.1) BAAD=1
                          IF(F31.EQ.1) CALL MACFAL
                          RETURN
                      END IF
                      REG(9)=VALUE1
                      GO TO 777
                  END IF
C
              END IF
C
          END IF
C
          IF(OPERND(I,17).GE.297.0D0.AND.OPERND(I,17).LE.446) THEN
C
C     3,5,7 OPERANDS
              CALL PRTRB
              CALL PRTRC
              CALL PRTRD
              XIS=.FALSE.
              IF(OPERND(I,17).EQ.298.0D0) XIS=.TRUE.
              IF(OPERND(I,17).EQ.300.0D0) XIS=.TRUE.
              IF(OPERND(I,17).EQ.302.0D0) XIS=.TRUE.
              IF(OPERND(I,17).EQ.304.0D0) XIS=.TRUE.
              IF(OPERND(I,17).EQ.306.0D0) XIS=.TRUE.
              IF(OPERND(I,17).EQ.308.0D0) XIS=.TRUE.
              IF(OPERND(I,17).EQ.310.0D0) XIS=.TRUE.
              IF(OPERND(I,17).EQ.312.0D0) XIS=.TRUE.
              IF(OPERND(I,17).EQ.314.0D0) XIS=.TRUE.
              IF(OPERND(I,17).EQ.316.0D0) XIS=.TRUE.
              IF(OPERND(I,17).EQ.318.0D0) XIS=.TRUE.
              IF(OPERND(I,17).EQ.320.0D0) XIS=.TRUE.
              IF(OPERND(I,17).EQ.322.0D0) XIS=.TRUE.
              IF(OPERND(I,17).EQ.324.0D0) XIS=.TRUE.
              IF(OPERND(I,17).EQ.326.0D0) XIS=.TRUE.
              IF(OPERND(I,17).EQ.328.0D0) XIS=.TRUE.
              IF(OPERND(I,17).EQ.330.0D0) XIS=.TRUE.
              IF(OPERND(I,17).EQ.332.0D0) XIS=.TRUE.
              IF(OPERND(I,17).EQ.334.0D0) XIS=.TRUE.
              IF(OPERND(I,17).EQ.336.0D0) XIS=.TRUE.
              IF(OPERND(I,17).EQ.338.0D0) XIS=.TRUE.
              IF(OPERND(I,17).EQ.340.0D0) XIS=.TRUE.
              IF(OPERND(I,17).EQ.342.0D0) XIS=.TRUE.
              IF(OPERND(I,17).EQ.344.0D0) XIS=.TRUE.
              IF(OPERND(I,17).EQ.346.0D0) XIS=.TRUE.
              IF(OPERND(I,17).EQ.348.0D0) XIS=.TRUE.
              IF(OPERND(I,17).EQ.350.0D0) XIS=.TRUE.
              IF(OPERND(I,17).EQ.352.0D0) XIS=.TRUE.
              IF(OPERND(I,17).EQ.354.0D0) XIS=.TRUE.
              IF(OPERND(I,17).EQ.356.0D0) XIS=.TRUE.
              IF(OPERND(I,17).EQ.358.0D0) XIS=.TRUE.
              IF(OPERND(I,17).EQ.360.0D0) XIS=.TRUE.
              IF(OPERND(I,17).EQ.362.0D0) XIS=.TRUE.
              IF(OPERND(I,17).EQ.364.0D0) XIS=.TRUE.
              IF(OPERND(I,17).EQ.366.0D0) XIS=.TRUE.
              IF(OPERND(I,17).EQ.368.0D0) XIS=.TRUE.
              IF(OPERND(I,17).EQ.370.0D0) XIS=.TRUE.
              IF(OPERND(I,17).EQ.372.0D0) XIS=.TRUE.
              IF(OPERND(I,17).EQ.374.0D0) XIS=.TRUE.
              IF(OPERND(I,17).EQ.376.0D0) XIS=.TRUE.
              IF(OPERND(I,17).EQ.378.0D0) XIS=.TRUE.
              IF(OPERND(I,17).EQ.380.0D0) XIS=.TRUE.
              IF(OPERND(I,17).EQ.382.0D0) XIS=.TRUE.
              IF(OPERND(I,17).EQ.384.0D0) XIS=.TRUE.
              IF(OPERND(I,17).EQ.386.0D0) XIS=.TRUE.
              IF(OPERND(I,17).EQ.388.0D0) XIS=.TRUE.
              IF(OPERND(I,17).EQ.390.0D0) XIS=.TRUE.
              IF(OPERND(I,17).EQ.392.0D0) XIS=.TRUE.
              IF(OPERND(I,17).EQ.394.0D0) XIS=.TRUE.
              IF(OPERND(I,17).EQ.396.0D0) XIS=.TRUE.
              IF(OPERND(I,17).EQ.398.0D0) XIS=.TRUE.
              IF(OPERND(I,17).EQ.400.0D0) XIS=.TRUE.
              IF(OPERND(I,17).EQ.402.0D0) XIS=.TRUE.
              IF(OPERND(I,17).EQ.404.0D0) XIS=.TRUE.
              IF(OPERND(I,17).EQ.406.0D0) XIS=.TRUE.
              IF(OPERND(I,17).EQ.408.0D0) XIS=.TRUE.
              IF(OPERND(I,17).EQ.410.0D0) XIS=.TRUE.
              IF(OPERND(I,17).EQ.412.0D0) XIS=.TRUE.
              IF(OPERND(I,17).EQ.414.0D0) XIS=.TRUE.
              IF(OPERND(I,17).EQ.416.0D0) XIS=.TRUE.
              IF(OPERND(I,17).EQ.418.0D0) XIS=.TRUE.
              IF(OPERND(I,17).EQ.420.0D0) XIS=.TRUE.
              IF(OPERND(I,17).EQ.422.0D0) XIS=.TRUE.
              IF(OPERND(I,17).EQ.424.0D0) XIS=.TRUE.
              IF(OPERND(I,17).EQ.426.0D0) XIS=.TRUE.
              IF(OPERND(I,17).EQ.428.0D0) XIS=.TRUE.
              IF(OPERND(I,17).EQ.430.0D0) XIS=.TRUE.
              IF(OPERND(I,17).EQ.432.0D0) XIS=.TRUE.
              IF(OPERND(I,17).EQ.434.0D0) XIS=.TRUE.
              IF(OPERND(I,17).EQ.436.0D0) XIS=.TRUE.
              IF(OPERND(I,17).EQ.438.0D0) XIS=.TRUE.
              IF(OPERND(I,17).EQ.440.0D0) XIS=.TRUE.
              IF(OPERND(I,17).EQ.442.0D0) XIS=.TRUE.
              IF(OPERND(I,17).EQ.444.0D0) XIS=.TRUE.
              IF(OPERND(I,17).EQ.446.0D0) XIS=.TRUE.
              SF=INT(SYSTEM1(20))
              ISFI=INT(OPERND(I,8))
              IF(INT(SYSTEM1(11)).GE.1.AND.INT(SYSTEM1(11)).LE.5) THEN
                  CW=INT(SYSTEM1(11))+45
              END IF
              IF(INT(SYSTEM1(11)).GE.6.AND.INT(SYSTEM1(11)).LE.10) THEN
                  CW=INT(SYSTEM1(11))+65
              END IF
              INV=1.0D0
              IF(SYSTEM1(30).EQ.1.0D0) THEN
C       MODE IS FOCAL
                  IF(.NOT.XIS)
     1            INV=-2.0*ALENS(CW,(SF-1))*PXTRAY(2,(SF-1))
                  IF(XIS)
     1            INV=-2.0*ALENS(CW,(SF-1))*PXTRAX(2,(SF-1))
              END IF
              IF(SYSTEM1(30).EQ.3.0D0) THEN
C       MODE IS AFOCAL
                  IF(.NOT.XIS)
     1            INV= 2.0*ALENS(CW,(SF-1))*PXTRAY(1,SF)
                  IF(XIS)
     1            INV= 2.0*ALENS(CW,(SF-1))*PXTRAX(1,SF)
              END IF
              IF(INV.EQ.0.0D0) THEN
                  IF(F28.EQ.1) REG(9)=0.0D0
                  IF(F28.EQ.1) BAAD=1
                  IF(F31.EQ.1) CALL MACFAL
                  RETURN
              END IF
C
C     PROCEED WITH CALCULATION
              IF(INT(OPERND(I,8)).LT.INT(SYSTEM1(20))) THEN
C     SINGLE SURFACE STUFF
                  IF(OPERND(I,17).EQ.297.0D0) THEN
                      REG(9)=MAB3(1,ISFI)
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.298.0D0) THEN
                      REG(9)=XMAB3(1,ISFI)
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.299.0D0) THEN
                      REG(9)=3.0D0*MAB3(2,ISFI)
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.300.0D0) THEN
                      REG(9)=3.0D0*XMAB3(2,ISFI)
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.301.0D0) THEN
                      REG(9)=MAB3(3,ISFI)
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.302.0D0) THEN
                      REG(9)=XMAB3(3,ISFI)
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.303.0D0) THEN
                      REG(9)=MAB3(4,ISFI)
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.304.0D0) THEN
                      REG(9)=XMAB3(4,ISFI)
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.305.0D0) THEN
                      REG(9)=MAB3(5,ISFI)
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.306.0D0) THEN
                      REG(9)=XMAB3(5,ISFI)
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.307.0D0) THEN
                      REG(9)=MAB57(1,ISFI)
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.308.0D0) THEN
                      REG(9)=XMAB57(1,ISFI)
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.309.0D0) THEN
                      REG(9)=MAB57(2,ISFI)+MAB57(3,ISFI)
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.310.0D0) THEN
                      REG(9)=XMAB57(2,ISFI)+XMAB57(3,ISFI)
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.311.0D0) THEN
                      REG(9)=MAB57(10,ISFI)
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.312.0D0) THEN
                      REG(9)=XMAB57(10,ISFI)
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.313.0D0) THEN
                      REG(9)=MAB57(12,ISFI)
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.314.0D0) THEN
                      REG(9)=XMAB57(12,ISFI)
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.315.0D0) THEN
                      REG(9)=MAB57(11,ISFI)
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.316.0D0) THEN
                      REG(9)=XMAB57(11,ISFI)
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.317.0D0) THEN
                      REG(9)=MAB57(4,ISFI)+
     1                MAB57(5,ISFI)+MAB57(6,ISFI)
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.318.0D0) THEN
                      REG(9)=XMAB57(4,ISFI)+
     1                XMAB57(5,ISFI)+XMAB57(6,ISFI)
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.319.0D0) THEN
                      REG(9)=MAB57(5,ISFI)
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.320.0D0) THEN
                      REG(9)=XMAB57(5,ISFI)
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.321.0D0) THEN
                      REG(9)=MAB57(7,ISFI)+
     1                MAB57(8,ISFI)
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.322.0D0) THEN
                      REG(9)=XMAB57(7,ISFI)+
     1                XMAB57(8,ISFI)
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.323.0D0) THEN
                      REG(9)=MAB57(10,ISFI)+
     1                (5.0*MAB57(11,ISFI))
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.324.0D0) THEN
                      REG(9)=XMAB57(10,ISFI)+
     1                (5.0*XMAB57(11,ISFI))
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.325.0D0) THEN
                      REG(9)=MAB57(10,ISFI)+
     1                MAB57(11,ISFI)
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.326.0D0) THEN
                      REG(9)=XMAB57(10,ISFI)+
     1                XMAB57(11,ISFI)
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.327.0D0) THEN
                      REG(9)=MAB57(14,ISFI)
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.328.0D0) THEN
                      REG(9)=XMAB57(14,ISFI)
                      GO TO 777
                  END IF
C       PRIMARY CHROMATIC ABERRATION DIFFERENCES
                  IF(OPERND(I,17).EQ.329.0D0) THEN
                      REG(9)=PDF3(1,ISFI)
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.330.0D0) THEN
                      REG(9)=XPDF3(1,ISFI)
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.331.0D0) THEN
                      REG(9)=3.0D0*PDF3(2,ISFI)
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.332.0D0) THEN
                      REG(9)=3.0D0*XPDF3(2,ISFI)
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.333.0D0) THEN
                      REG(9)=PDF3(3,ISFI)
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.334.0D0) THEN
                      REG(9)=XPDF3(3,ISFI)
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.335.0D0) THEN
                      REG(9)=PDF3(4,ISFI)
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.336.0D0) THEN
                      REG(9)=XPDF3(4,ISFI)
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.337.0D0) THEN
                      REG(9)=PDF3(5,ISFI)
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.338.0D0) THEN
                      REG(9)=XPDF3(5,ISFI)
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.339.0D0) THEN
                      REG(9)=PDF57(1,ISFI)
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.340.0D0) THEN
                      REG(9)=XPDF57(1,ISFI)
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.341.0D0) THEN
                      REG(9)=PDF57(2,ISFI)+PDF57(3,ISFI)
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.342.0D0) THEN
                      REG(9)=XPDF57(2,ISFI)+XPDF57(3,ISFI)
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.343.0D0) THEN
                      REG(9)=PDF57(10,ISFI)
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.344.0D0) THEN
                      REG(9)=XPDF57(10,ISFI)
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.345.0D0) THEN
                      REG(9)=PDF57(12,ISFI)
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.346.0D0) THEN
                      REG(9)=XPDF57(12,ISFI)
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.347.0D0) THEN
                      REG(9)=PDF57(11,ISFI)
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.348.0D0) THEN
                      REG(9)=XPDF57(11,ISFI)
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.349.0D0) THEN
                      REG(9)=PDF57(4,ISFI)+
     1                PDF57(5,ISFI)+PDF57(6,ISFI)
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.350.0D0) THEN
                      REG(9)=XPDF57(4,ISFI)+
     1                XPDF57(5,ISFI)+XPDF57(6,ISFI)
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.351.0D0) THEN
                      REG(9)=PDF57(5,ISFI)
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.352.0D0) THEN
                      REG(9)=XPDF57(5,ISFI)
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.353.0D0) THEN
                      REG(9)=PDF57(7,ISFI)+
     1                PDF57(8,ISFI)
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.354.0D0) THEN
                      REG(9)=XPDF57(7,ISFI)+
     1                XPDF57(8,ISFI)
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.355.0D0) THEN
                      REG(9)=PDF57(10,ISFI)+
     1                (5.0*PDF57(11,ISFI))
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.356.0D0) THEN
                      REG(9)=XPDF57(10,ISFI)+
     1                (5.0*XPDF57(11,ISFI))
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.357.0D0) THEN
                      REG(9)=PDF57(10,ISFI)+
     1                PDF57(11,ISFI)
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.358.0D0) THEN
                      REG(9)=XPDF57(10,ISFI)+
     1                XPDF57(11,ISFI)
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.359.0D0) THEN
                      REG(9)=PDF57(14,ISFI)
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.360.0D0) THEN
                      REG(9)=XPDF57(14,ISFI)
                      GO TO 777
                  END IF
C       SECONDARY CHROMATIC ABERRATION DIFFERENCES
                  IF(OPERND(I,17).EQ.361.0D0) THEN
                      REG(9)=SDF3(1,ISFI)
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.362.0D0) THEN
                      REG(9)=XSDF3(1,ISFI)
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.363.0D0) THEN
                      REG(9)=3.0D0*SDF3(2,ISFI)
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.364.0D0) THEN
                      REG(9)=3.0D0*XSDF3(2,ISFI)
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.365.0D0) THEN
                      REG(9)=SDF3(3,ISFI)
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.366.0D0) THEN
                      REG(9)=XSDF3(3,ISFI)
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.367.0D0) THEN
                      REG(9)=SDF3(4,ISFI)
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.368.0D0) THEN
                      REG(9)=XSDF3(4,ISFI)
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.369.0D0) THEN
                      REG(9)=SDF3(5,ISFI)
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.370.0D0) THEN
                      REG(9)=XSDF3(5,ISFI)
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.371.0D0) THEN
                      REG(9)=SDF57(1,ISFI)
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.372.0D0) THEN
                      REG(9)=XSDF57(1,ISFI)
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.373.0D0) THEN
                      REG(9)=SDF57(2,ISFI)+SDF57(3,ISFI)
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.374.0D0) THEN
                      REG(9)=XSDF57(2,ISFI)+XSDF57(3,ISFI)
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.375.0D0) THEN
                      REG(9)=SDF57(10,ISFI)
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.376.0D0) THEN
                      REG(9)=XSDF57(10,ISFI)
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.377.0D0) THEN
                      REG(9)=SDF57(12,ISFI)
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.378.0D0) THEN
                      REG(9)=XSDF57(12,ISFI)
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.379.0D0) THEN
                      REG(9)=SDF57(11,ISFI)
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.380.0D0) THEN
                      REG(9)=XSDF57(11,ISFI)
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.381.0D0) THEN
                      REG(9)=SDF57(4,ISFI)+
     1                SDF57(5,ISFI)+SDF57(6,ISFI)
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.382.0D0) THEN
                      REG(9)=XSDF57(4,ISFI)+
     1                XSDF57(5,ISFI)+XSDF57(6,ISFI)
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.383.0D0) THEN
                      REG(9)=SDF57(5,ISFI)
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.384.0D0) THEN
                      REG(9)=XSDF57(5,ISFI)
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.385.0D0) THEN
                      REG(9)=SDF57(7,ISFI)+
     1                SDF57(8,ISFI)
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.386.0D0) THEN
                      REG(9)=XSDF57(7,ISFI)+
     1                XSDF57(8,ISFI)
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.387.0D0) THEN
                      REG(9)=SDF57(10,ISFI)+
     1                (5.0*SDF57(11,ISFI))
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.388.0D0) THEN
                      REG(9)=XSDF57(10,ISFI)+
     1                (5.0*XSDF57(11,ISFI))
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.389.0D0) THEN
                      REG(9)=SDF57(10,ISFI)+
     1                SDF57(11,ISFI)
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.390.0D0) THEN
                      REG(9)=XSDF57(10,ISFI)+
     1                XSDF57(11,ISFI)
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.391.0D0) THEN
                      REG(9)=SDF57(14,ISFI)
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.392.0D0) THEN
                      REG(9)=XSDF57(14,ISFI)
                      GO TO 777
                  END IF
C       5TH AND 7TH ORDER INTRINSIC SURFACE ABERRATIONS
                  IF(OPERND(I,17).EQ.393.0D0) THEN
                      REG(9)=SAB57(1,ISFI)
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.394.0D0) THEN
                      REG(9)=XSAB57(1,ISFI)
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.395.0D0) THEN
                      REG(9)=SAB57(2,ISFI)+SAB57(3,ISFI)
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.396.0D0) THEN
                      REG(9)=XSAB57(2,ISFI)+XSAB57(3,ISFI)
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.397.0D0) THEN
                      REG(9)=SAB57(10,ISFI)
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.398.0D0) THEN
                      REG(9)=XSAB57(10,ISFI)
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.399.0D0) THEN
                      REG(9)=SAB57(12,ISFI)
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.400.0D0) THEN
                      REG(9)=XSAB57(12,ISFI)
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.401.0D0) THEN
                      REG(9)=SAB57(11,ISFI)
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.402.0D0) THEN
                      REG(9)=XSAB57(11,ISFI)
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.403.0D0) THEN
                      REG(9)=SAB57(4,ISFI)+
     1                SAB57(5,ISFI)+SAB57(6,ISFI)
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.404.0D0) THEN
                      REG(9)=XSAB57(4,ISFI)+
     1                XSAB57(5,ISFI)+XSAB57(6,ISFI)
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.405.0D0) THEN
                      REG(9)=SAB57(5,ISFI)
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.406.0D0) THEN
                      REG(9)=XSAB57(5,ISFI)
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.407.0D0) THEN
                      REG(9)=SAB57(7,ISFI)+
     1                SAB57(8,ISFI)
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.408.0D0) THEN
                      REG(9)=XSAB57(7,ISFI)+
     1                XSAB57(8,ISFI)
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.409.0D0) THEN
                      REG(9)=SAB57(10,ISFI)+
     1                (5.0*SAB57(11,ISFI))
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.410.0D0) THEN
                      REG(9)=XSAB57(10,ISFI)+
     1                (5.0*XSAB57(11,ISFI))
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.411.0D0) THEN
                      REG(9)=SAB57(10,ISFI)+
     1                SAB57(11,ISFI)
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.412.0D0) THEN
                      REG(9)=XSAB57(10,ISFI)+
     1                XSAB57(11,ISFI)
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.413.0D0) THEN
                      REG(9)=SAB57(14,ISFI)
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.414.0D0) THEN
                      REG(9)=XSAB57(14,ISFI)
                      GO TO 777
                  END IF
C       3RD ORDER PUPIL ABERRATIONS
                  IF(OPERND(I,17).EQ.415.0D0) THEN
                      REG(9)=MAB3(6,ISFI)
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.416.0D0) THEN
                      REG(9)=XMAB3(6,ISFI)
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.417.0D0) THEN
                      REG(9)=3.0D0*MAB3(7,ISFI)
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.418.0D0) THEN
                      REG(9)=3.0D0*XMAB3(7,ISFI)
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.419.0D0) THEN
                      REG(9)=MAB3(8,ISFI)
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.420.0D0) THEN
                      REG(9)=XMAB3(8,ISFI)
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.421.0D0) THEN
                      REG(9)=MAB3(9,ISFI)
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.422.0D0) THEN
                      REG(9)=XMAB3(9,ISFI)
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.423.0D0) THEN
                      REG(9)=MAB3(10,ISFI)
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.424.0D0) THEN
                      REG(9)=XMAB3(10,ISFI)
                      GO TO 777
                  END IF
C       3RD ORDER PUPIL ABERRATION PRIMARY CHROMATIC DIFFERENCES
                  IF(OPERND(I,17).EQ.425.0D0) THEN
                      REG(9)=PDF3(6,ISFI)
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.426.0D0) THEN
                      REG(9)=XPDF3(6,ISFI)
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.427.0D0) THEN
                      REG(9)=3.0D0*PDF3(7,ISFI)
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.428.0D0) THEN
                      REG(9)=3.0D0*XPDF3(7,ISFI)
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.429.0D0) THEN
                      REG(9)=PDF3(8,ISFI)
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.430.0D0) THEN
                      REG(9)=XPDF3(8,ISFI)
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.431.0D0) THEN
                      REG(9)=PDF3(9,ISFI)
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.432.0D0) THEN
                      REG(9)=XPDF3(9,ISFI)
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.433.0D0) THEN
                      REG(9)=PDF3(10,ISFI)
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.434.0D0) THEN
                      REG(9)=XPDF3(10,ISFI)
                      GO TO 777
                  END IF
C       3RD ORDER PUPIL ABERRATION SECONDARY CHROMATIC DIFFERENCES
                  IF(OPERND(I,17).EQ.435.0D0) THEN
                      REG(9)=SDF3(6,ISFI)
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.436.0D0) THEN
                      REG(9)=XSDF3(6,ISFI)
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.437.0D0) THEN
                      REG(9)=3.0D0*SDF3(7,ISFI)
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.438.0D0) THEN
                      REG(9)=3.0D0*XSDF3(7,ISFI)
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.439.0D0) THEN
                      REG(9)=SDF3(8,ISFI)
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.440.0D0) THEN
                      REG(9)=XSDF3(8,ISFI)
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.441.0D0) THEN
                      REG(9)=SDF3(9,ISFI)
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.442.0D0) THEN
                      REG(9)=XSDF3(9,ISFI)
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.443.0D0) THEN
                      REG(9)=SDF3(10,ISFI)
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.444.0D0) THEN
                      REG(9)=XSDF3(10,ISFI)
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.445.0D0) THEN
                      REG(9)=MAB3(11,ISFI)
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.446.0D0) THEN
                      REG(9)=XMAB3(11,ISFI)
                      GO TO 777
                  END IF
              ELSE
                  V=0.0D0
C     FINAL SURFACE SUM
                  IF(OPERND(I,17).EQ.297.0D0) THEN
                      DO IV=0,INT(SYSTEM1(20))
                          V=V+MAB3(1,IV)
                      END DO
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.298.0D0) THEN
                      DO IV=0,INT(SYSTEM1(20))
                          V=V+XMAB3(1,IV)
                      END DO
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.299.0D0) THEN
                      DO IV=0,INT(SYSTEM1(20))
                          V=V+(3.0D0*MAB3(2,IV))
                      END DO
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.300.0D0) THEN
                      DO IV=0,INT(SYSTEM1(20))
                          V=V+(3.0D0*XMAB3(2,IV))
                      END DO
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.301.0D0) THEN
                      DO IV=0,INT(SYSTEM1(20))
                          V=V+MAB3(3,IV)
                      END DO
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.302.0D0) THEN
                      DO IV=0,INT(SYSTEM1(20))
                          V=V+XMAB3(3,IV)
                      END DO
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.303.0D0) THEN
                      DO IV=0,INT(SYSTEM1(20))
                          V=V+MAB3(4,IV)
                      END DO
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.304.0D0) THEN
                      DO IV=0,INT(SYSTEM1(20))
                          V=V+XMAB3(4,IV)
                      END DO
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.305.0D0) THEN
                      DO IV=0,INT(SYSTEM1(20))
                          V=V+MAB3(5,IV)
                      END DO
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.306.0D0) THEN
                      DO IV=0,INT(SYSTEM1(20))
                          V=V+XMAB3(5,IV)
                      END DO
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.307.0D0) THEN
                      DO IV=0,INT(SYSTEM1(20))
                          V=V+MAB57(1,IV)
                      END DO
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.308.0D0) THEN
                      DO IV=0,INT(SYSTEM1(20))
                          V=V+XMAB57(1,IV)
                      END DO
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.309.0D0) THEN
                      DO IV=0,INT(SYSTEM1(20))
                          V=V+MAB57(2,IV)+MAB57(3,IV)
                      END DO
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.310.0D0) THEN
                      DO IV=0,INT(SYSTEM1(20))
                          V=V+XMAB57(2,IV)+XMAB57(3,IV)
                      END DO
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.311.0D0) THEN
                      DO IV=0,INT(SYSTEM1(20))
                          V=V+MAB57(10,IV)
                      END DO
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.312.0D0) THEN
                      DO IV=0,INT(SYSTEM1(20))
                          V=V+XMAB57(10,IV)
                      END DO
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.313.0D0) THEN
                      DO IV=0,INT(SYSTEM1(20))
                          V=V+MAB57(12,IV)
                      END DO
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.314.0D0) THEN
                      DO IV=0,INT(SYSTEM1(20))
                          V=V+XMAB57(12,IV)
                      END DO
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.315.0D0) THEN
                      DO IV=0,INT(SYSTEM1(20))
                          V=V+MAB57(11,IV)
                      END DO
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.316.0D0) THEN
                      DO IV=0,INT(SYSTEM1(20))
                          V=V+XMAB57(11,IV)
                      END DO
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.317.0D0) THEN
                      DO IV=0,INT(SYSTEM1(20))
                          V=V+MAB57(4,IV)+
     1                    MAB57(5,IV)+MAB57(6,IV)
                      END DO
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.318.0D0) THEN
                      DO IV=0,INT(SYSTEM1(20))
                          V=V+XMAB57(4,IV)+
     1                    XMAB57(5,IV)+XMAB57(6,IV)
                      END DO
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.319.0D0) THEN
                      DO IV=0,INT(SYSTEM1(20))
                          V=V+MAB57(5,IV)
                      END DO
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.320.0D0) THEN
                      DO IV=0,INT(SYSTEM1(20))
                          V=V+XMAB57(5,IV)
                      END DO
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.321.0D0) THEN
                      DO IV=0,INT(SYSTEM1(20))
                          V=V+MAB57(7,IV)+
     1                    MAB57(8,IV)
                      END DO
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.322.0D0) THEN
                      DO IV=0,INT(SYSTEM1(20))
                          V=V+XMAB57(7,IV)+
     1                    XMAB57(8,IV)
                      END DO
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.323.0D0) THEN
                      DO IV=0,INT(SYSTEM1(20))
                          V=V+MAB57(10,IV)+
     1                    (5.0*MAB57(11,IV))
                      END DO
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.324.0D0) THEN
                      DO IV=0,INT(SYSTEM1(20))
                          V=V+XMAB57(10,IV)+
     1                    (5.0*XMAB57(11,IV))
                      END DO
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.325.0D0) THEN
                      DO IV=0,INT(SYSTEM1(20))
                          V=V+MAB57(10,IV)+
     1                    MAB57(11,IV)
                      END DO
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.326.0D0) THEN
                      DO IV=0,INT(SYSTEM1(20))
                          V=V+XMAB57(10,IV)+
     1                    XMAB57(11,IV)
                      END DO
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.327.0D0) THEN
                      DO IV=0,INT(SYSTEM1(20))
                          V=V+MAB57(14,IV)
                      END DO
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.328.0D0) THEN
                      DO IV=0,INT(SYSTEM1(20))
                          V=V+XMAB57(14,IV)
                      END DO
                      GO TO 777
                  END IF
C       PRIMARY CHROMATIC ABERRATION DIFFERENCES
                  IF(OPERND(I,17).EQ.329.0D0) THEN
                      DO IV=0,INT(SYSTEM1(20))
                          V=V+PDF3(1,IV)
                      END DO
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.330.0D0) THEN
                      DO IV=0,INT(SYSTEM1(20))
                          V=V+XPDF3(1,IV)
                      END DO
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.331.0D0) THEN
                      DO IV=0,INT(SYSTEM1(20))
                          V=V+(3.0D0*PDF3(2,IV))
                      END DO
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.332.0D0) THEN
                      DO IV=0,INT(SYSTEM1(20))
                          V=V+(3.0D0*XPDF3(2,IV))
                      END DO
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.333.0D0) THEN
                      DO IV=0,INT(SYSTEM1(20))
                          V=V+PDF3(3,IV)
                      END DO
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.334.0D0) THEN
                      DO IV=0,INT(SYSTEM1(20))
                          V=V+XPDF3(3,IV)
                      END DO
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.335.0D0) THEN
                      DO IV=0,INT(SYSTEM1(20))
                          V=V+PDF3(4,IV)
                      END DO
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.336.0D0) THEN
                      DO IV=0,INT(SYSTEM1(20))
                          V=V+XPDF3(4,IV)
                      END DO
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.337.0D0) THEN
                      DO IV=0,INT(SYSTEM1(20))
                          V=V+PDF3(5,IV)
                      END DO
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.338.0D0) THEN
                      DO IV=0,INT(SYSTEM1(20))
                          V=V+XPDF3(5,IV)
                      END DO
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.339.0D0) THEN
                      DO IV=0,INT(SYSTEM1(20))
                          V=V+PDF57(1,IV)
                      END DO
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.340.0D0) THEN
                      DO IV=0,INT(SYSTEM1(20))
                          V=V+XPDF57(1,IV)
                      END DO
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.341.0D0) THEN
                      DO IV=0,INT(SYSTEM1(20))
                          V=V+PDF57(2,IV)+PDF57(3,IV)
                      END DO
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.342.0D0) THEN
                      DO IV=0,INT(SYSTEM1(20))
                          V=V+XPDF57(2,IV)+XPDF57(3,IV)
                      END DO
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.343.0D0) THEN
                      DO IV=0,INT(SYSTEM1(20))
                          V=V+PDF57(10,IV)
                      END DO
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.344.0D0) THEN
                      DO IV=0,INT(SYSTEM1(20))
                          V=V+XPDF57(10,IV)
                      END DO
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.345.0D0) THEN
                      DO IV=0,INT(SYSTEM1(20))
                          V=V+PDF57(12,IV)
                      END DO
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.346.0D0) THEN
                      DO IV=0,INT(SYSTEM1(20))
                          V=V+XPDF57(12,IV)
                      END DO
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.347.0D0) THEN
                      DO IV=0,INT(SYSTEM1(20))
                          V=V+PDF57(11,IV)
                      END DO
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.348.0D0) THEN
                      DO IV=0,INT(SYSTEM1(20))
                          V=V+XPDF57(11,IV)
                      END DO
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.349.0D0) THEN
                      DO IV=0,INT(SYSTEM1(20))
                          V=V+PDF57(4,IV)+
     1                    PDF57(5,IV)+PDF57(6,IV)
                      END DO
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.350.0D0) THEN
                      DO IV=0,INT(SYSTEM1(20))
                          V=V+XPDF57(4,IV)+
     1                    XPDF57(5,IV)+XPDF57(6,IV)
                      END DO
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.351.0D0) THEN
                      DO IV=0,INT(SYSTEM1(20))
                          V=V+PDF57(5,IV)
                      END DO
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.352.0D0) THEN
                      DO IV=0,INT(SYSTEM1(20))
                          V=V+XPDF57(5,IV)
                      END DO
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.353.0D0) THEN
                      DO IV=0,INT(SYSTEM1(20))
                          V=V+PDF57(7,IV)+
     1                    PDF57(8,IV)
                      END DO
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.354.0D0) THEN
                      DO IV=0,INT(SYSTEM1(20))
                          V=V+XPDF57(7,IV)+
     1                    XPDF57(8,IV)
                      END DO
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.355.0D0) THEN
                      DO IV=0,INT(SYSTEM1(20))
                          V=V+PDF57(10,IV)+
     1                    (5.0*PDF57(11,IV))
                      END DO
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.356.0D0) THEN
                      DO IV=0,INT(SYSTEM1(20))
                          V=V+XPDF57(10,IV)+
     1                    (5.0*XPDF57(11,IV))
                      END DO
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.357.0D0) THEN
                      DO IV=0,INT(SYSTEM1(20))
                          V=V+PDF57(10,IV)+
     1                    PDF57(11,IV)
                      END DO
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.358.0D0) THEN
                      DO IV=0,INT(SYSTEM1(20))
                          V=V+XPDF57(10,IV)+
     1                    XPDF57(11,IV)
                      END DO
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.359.0D0) THEN
                      DO IV=0,INT(SYSTEM1(20))
                          V=V+PDF57(14,IV)
                      END DO
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.360.0D0) THEN
                      DO IV=0,INT(SYSTEM1(20))
                          V=V+XPDF57(14,IV)
                      END DO
                      GO TO 777
                  END IF
C       SECONDARY CHROMATIC ABERRATION DIFFERENCES
                  IF(OPERND(I,17).EQ.361.0D0) THEN
                      DO IV=0,INT(SYSTEM1(20))
                          V=V+SDF3(1,IV)
                      END DO
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.362.0D0) THEN
                      DO IV=0,INT(SYSTEM1(20))
                          V=V+XSDF3(1,IV)
                      END DO
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.363.0D0) THEN
                      DO IV=0,INT(SYSTEM1(20))
                          V=V+(3.0D0*SDF3(2,IV))
                      END DO
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.364.0D0) THEN
                      DO IV=0,INT(SYSTEM1(20))
                          V=V+(3.0D0*XSDF3(2,IV))
                      END DO
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.365.0D0) THEN
                      DO IV=0,INT(SYSTEM1(20))
                          V=V+SDF3(3,IV)
                      END DO
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.366.0D0) THEN
                      DO IV=0,INT(SYSTEM1(20))
                          V=V+XSDF3(3,IV)
                      END DO
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.367.0D0) THEN
                      DO IV=0,INT(SYSTEM1(20))
                          V=V+SDF3(4,IV)
                      END DO
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.368.0D0) THEN
                      DO IV=0,INT(SYSTEM1(20))
                          V=V+XSDF3(4,IV)
                      END DO
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.369.0D0) THEN
                      DO IV=0,INT(SYSTEM1(20))
                          V=V+SDF3(5,IV)
                      END DO
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.370.0D0) THEN
                      DO IV=0,INT(SYSTEM1(20))
                          V=V+XSDF3(5,IV)
                      END DO
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.371.0D0) THEN
                      DO IV=0,INT(SYSTEM1(20))
                          V=V+SDF57(1,IV)
                      END DO
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.372.0D0) THEN
                      DO IV=0,INT(SYSTEM1(20))
                          V=V+XSDF57(1,IV)
                      END DO
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.373.0D0) THEN
                      DO IV=0,INT(SYSTEM1(20))
                          V=V+SDF57(2,IV)+SDF57(3,IV)
                      END DO
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.374.0D0) THEN
                      DO IV=0,INT(SYSTEM1(20))
                          V=V+XSDF57(2,IV)+XSDF57(3,IV)
                      END DO
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.375.0D0) THEN
                      DO IV=0,INT(SYSTEM1(20))
                          V=V+SDF57(10,IV)
                      END DO
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.376.0D0) THEN
                      DO IV=0,INT(SYSTEM1(20))
                          V=V+XSDF57(10,IV)
                      END DO
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.377.0D0) THEN
                      DO IV=0,INT(SYSTEM1(20))
                          V=V+SDF57(12,IV)
                      END DO
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.378.0D0) THEN
                      DO IV=0,INT(SYSTEM1(20))
                          V=V+XSDF57(12,IV)
                      END DO
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.379.0D0) THEN
                      DO IV=0,INT(SYSTEM1(20))
                          V=V+SDF57(11,IV)
                      END DO
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.380.0D0) THEN
                      DO IV=0,INT(SYSTEM1(20))
                          V=V+XSDF57(11,IV)
                      END DO
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.381.0D0) THEN
                      DO IV=0,INT(SYSTEM1(20))
                          V=V+SDF57(4,IV)+
     1                    SDF57(5,IV)+SDF57(6,IV)
                      END DO
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.382.0D0) THEN
                      DO IV=0,INT(SYSTEM1(20))
                          V=V+XSDF57(4,IV)+
     1                    XSDF57(5,IV)+XSDF57(6,IV)
                      END DO
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.383.0D0) THEN
                      DO IV=0,INT(SYSTEM1(20))
                          V=V+SDF57(5,IV)
                      END DO
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.384.0D0) THEN
                      DO IV=0,INT(SYSTEM1(20))
                          V=V+XSDF57(5,IV)
                      END DO
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.385.0D0) THEN
                      DO IV=0,INT(SYSTEM1(20))
                          V=V+SDF57(7,IV)+
     1                    SDF57(8,IV)
                      END DO
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.386.0D0) THEN
                      DO IV=0,INT(SYSTEM1(20))
                          V=V+XSDF57(7,IV)+
     1                    XSDF57(8,IV)
                      END DO
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.387.0D0) THEN
                      DO IV=0,INT(SYSTEM1(20))
                          V=V+SDF57(10,IV)+
     1                    (5.0*SDF57(11,IV))
                      END DO
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.388.0D0) THEN
                      DO IV=0,INT(SYSTEM1(20))
                          V=V+XSDF57(10,IV)+
     1                    (5.0*XSDF57(11,IV))
                      END DO
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.389.0D0) THEN
                      DO IV=0,INT(SYSTEM1(20))
                          V=V+SDF57(10,IV)+
     1                    SDF57(11,IV)
                      END DO
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.390.0D0) THEN
                      DO IV=0,INT(SYSTEM1(20))
                          V=V+XSDF57(10,IV)+
     1                    XSDF57(11,IV)
                      END DO
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.391.0D0) THEN
                      DO IV=0,INT(SYSTEM1(20))
                          V=V+SDF57(14,IV)
                      END DO
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.392.0D0) THEN
                      DO IV=0,INT(SYSTEM1(20))
                          V=V+XSDF57(14,IV)
                      END DO
                      GO TO 777
                  END IF
C       5TH AND 7TH ORDER INTRINSIC SURFACE ABERRATIONS
                  IF(OPERND(I,17).EQ.393.0D0) THEN
                      DO IV=0,INT(SYSTEM1(20))
                          V=V+SAB57(1,IV)
                      END DO
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.394.0D0) THEN
                      DO IV=0,INT(SYSTEM1(20))
                          V=V+XSAB57(1,IV)
                      END DO
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.395.0D0) THEN
                      DO IV=0,INT(SYSTEM1(20))
                          V=V+SAB57(2,IV)+SAB57(3,IV)
                      END DO
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.396.0D0) THEN
                      DO IV=0,INT(SYSTEM1(20))
                          V=V+XSAB57(2,IV)+XSAB57(3,IV)
                      END DO
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.397.0D0) THEN
                      DO IV=0,INT(SYSTEM1(20))
                          V=V+SAB57(10,IV)
                      END DO
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.398.0D0) THEN
                      DO IV=0,INT(SYSTEM1(20))
                          V=V+XSAB57(10,IV)
                      END DO
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.399.0D0) THEN
                      DO IV=0,INT(SYSTEM1(20))
                          V=V+SAB57(12,IV)
                      END DO
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.400.0D0) THEN
                      DO IV=0,INT(SYSTEM1(20))
                          V=V+XSAB57(12,IV)
                      END DO
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.401.0D0) THEN
                      DO IV=0,INT(SYSTEM1(20))
                          V=V+SAB57(11,IV)
                      END DO
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.402.0D0) THEN
                      DO IV=0,INT(SYSTEM1(20))
                          V=V+XSAB57(11,IV)
                      END DO
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.403.0D0) THEN
                      DO IV=0,INT(SYSTEM1(20))
                          V=V+SAB57(4,IV)+
     1                    SAB57(5,IV)+SAB57(6,IV)
                      END DO
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.404.0D0) THEN
                      DO IV=0,INT(SYSTEM1(20))
                          V=V+XSAB57(4,IV)+
     1                    XSAB57(5,IV)+XSAB57(6,IV)
                      END DO
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.405.0D0) THEN
                      DO IV=0,INT(SYSTEM1(20))
                          V=V+SAB57(5,IV)
                      END DO
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.406.0D0) THEN
                      DO IV=0,INT(SYSTEM1(20))
                          V=V+XSAB57(5,IV)
                      END DO
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.407.0D0) THEN
                      DO IV=0,INT(SYSTEM1(20))
                          V=V+SAB57(7,IV)+
     1                    SAB57(8,IV)
                      END DO
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.408.0D0) THEN
                      DO IV=0,INT(SYSTEM1(20))
                          V=V+XSAB57(7,IV)+
     1                    XSAB57(8,IV)
                      END DO
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.409.0D0) THEN
                      DO IV=0,INT(SYSTEM1(20))
                          V=V+SAB57(10,IV)+
     1                    (5.0*SAB57(11,IV))
                      END DO
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.410.0D0) THEN
                      DO IV=0,INT(SYSTEM1(20))
                          V=V+XSAB57(10,IV)+
     1                    (5.0*XSAB57(11,IV))
                      END DO
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.411.0D0) THEN
                      DO IV=0,INT(SYSTEM1(20))
                          V=V+SAB57(10,IV)+
     1                    SAB57(11,IV)
                      END DO
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.412.0D0) THEN
                      DO IV=0,INT(SYSTEM1(20))
                          V=V+XSAB57(10,IV)+
     1                    XSAB57(11,IV)
                      END DO
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.413.0D0) THEN
                      DO IV=0,INT(SYSTEM1(20))
                          V=V+SAB57(14,IV)
                      END DO
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.414.0D0) THEN
                      DO IV=0,INT(SYSTEM1(20))
                          V=V+XSAB57(14,IV)
                      END DO
                      GO TO 777
                  END IF
C       3RD ORDER PUPIL ABERRATIONS
                  IF(OPERND(I,17).EQ.415.0D0) THEN
                      DO IV=0,INT(SYSTEM1(20))
                          V=V+MAB3(6,IV)
                      END DO
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.416.0D0) THEN
                      DO IV=0,INT(SYSTEM1(20))
                          V=V+XMAB3(6,IV)
                      END DO
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.417.0D0) THEN
                      DO IV=0,INT(SYSTEM1(20))
                          V=V+(3.0D0*MAB3(7,IV))
                      END DO
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.418.0D0) THEN
                      DO IV=0,INT(SYSTEM1(20))
                          V=V+(3.0D0*XMAB3(7,IV))
                      END DO
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.419.0D0) THEN
                      DO IV=0,INT(SYSTEM1(20))
                          V=V+MAB3(8,IV)
                      END DO
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.420.0D0) THEN
                      DO IV=0,INT(SYSTEM1(20))
                          V=V+XMAB3(8,IV)
                      END DO
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.421.0D0) THEN
                      DO IV=0,INT(SYSTEM1(20))
                          V=V+MAB3(9,IV)
                      END DO
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.422.0D0) THEN
                      DO IV=0,INT(SYSTEM1(20))
                          V=V+XMAB3(9,IV)
                      END DO
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.423.0D0) THEN
                      DO IV=0,INT(SYSTEM1(20))
                          V=V+MAB3(10,IV)
                      END DO
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.424.0D0) THEN
                      DO IV=0,INT(SYSTEM1(20))
                          V=V+XMAB3(10,IV)
                      END DO
                      GO TO 777
                  END IF
C       3RD ORDER PUPIL ABERRATION PRIMARY CHROMATIC DIFFERENCES
                  IF(OPERND(I,17).EQ.425.0D0) THEN
                      DO IV=0,INT(SYSTEM1(20))
                          V=V+PDF3(6,IV)
                      END DO
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.426.0D0) THEN
                      DO IV=0,INT(SYSTEM1(20))
                          V=V+XPDF3(6,IV)
                      END DO
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.427.0D0) THEN
                      DO IV=0,INT(SYSTEM1(20))
                          V=V+(3.0D0*PDF3(7,IV))
                      END DO
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.428.0D0) THEN
                      DO IV=0,INT(SYSTEM1(20))
                          V=V+(3.0D0*XPDF3(7,IV))
                      END DO
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.429.0D0) THEN
                      DO IV=0,INT(SYSTEM1(20))
                          V=V+PDF3(8,IV)
                      END DO
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.430.0D0) THEN
                      DO IV=0,INT(SYSTEM1(20))
                          V=V+XPDF3(8,IV)
                      END DO
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.431.0D0) THEN
                      DO IV=0,INT(SYSTEM1(20))
                          V=V+PDF3(9,IV)
                      END DO
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.432.0D0) THEN
                      DO IV=0,INT(SYSTEM1(20))
                          V=V+XPDF3(9,IV)
                      END DO
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.433.0D0) THEN
                      DO IV=0,INT(SYSTEM1(20))
                          V=V+PDF3(10,IV)
                      END DO
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.434.0D0) THEN
                      DO IV=0,INT(SYSTEM1(20))
                          V=V+XPDF3(10,IV)
                      END DO
                      GO TO 777
                  END IF
C       3RD ORDER PUPIL ABERRATION SECONDARY CHROMATIC DIFFERENCES
                  IF(OPERND(I,17).EQ.435.0D0) THEN
                      DO IV=0,INT(SYSTEM1(20))
                          V=V+SDF3(6,IV)
                      END DO
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.436.0D0) THEN
                      DO IV=0,INT(SYSTEM1(20))
                          V=V+XSDF3(6,IV)
                      END DO
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.437.0D0) THEN
                      DO IV=0,INT(SYSTEM1(20))
                          V=V+(3.0D0*SDF3(7,IV))
                      END DO
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.438.0D0) THEN
                      DO IV=0,INT(SYSTEM1(20))
                          V=V+(3.0D0*XSDF3(7,IV))
                      END DO
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.439.0D0) THEN
                      DO IV=0,INT(SYSTEM1(20))
                          V=V+SDF3(8,IV)
                      END DO
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.440.0D0) THEN
                      DO IV=0,INT(SYSTEM1(20))
                          V=V+XSDF3(8,IV)
                      END DO
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.441.0D0) THEN
                      DO IV=0,INT(SYSTEM1(20))
                          V=V+SDF3(9,IV)
                      END DO
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.442.0D0) THEN
                      DO IV=0,INT(SYSTEM1(20))
                          V=V+XSDF3(9,IV)
                      END DO
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.443.0D0) THEN
                      DO IV=0,INT(SYSTEM1(20))
                          V=V+SDF3(10,IV)
                      END DO
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.444.0D0) THEN
                      DO IV=0,INT(SYSTEM1(20))
                          V=V+XSDF3(10,IV)
                      END DO
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.445.0D0) THEN
                      DO IV=0,INT(SYSTEM1(20))
                          V=V+MAB3(11,IV)
                      END DO
                      GO TO 777
                  END IF
                  IF(OPERND(I,17).EQ.446.0D0) THEN
                      DO IV=0,INT(SYSTEM1(20))
                          V=V+XMAB3(11,IV)
                      END DO
                      GO TO 777
                  END IF
C
              END IF
C
          END IF
          IF(OPERND(I,17).EQ.447.0D0) THEN
C     AH
              REG(9)=
     1        ALENS(81,INT(OPERND(I,8)))
              GO TO 777
          END IF
          IF(OPERND(I,17).EQ.448.0D0) THEN
C     AI
              REG(9)=
     1        ALENS(82,INT(OPERND(I,8)))
              GO TO 777
          END IF
          IF(OPERND(I,17).EQ.449.0D0) THEN
C     AJ
              REG(9)=
     1        ALENS(83,INT(OPERND(I,8)))
              GO TO 777
          END IF
          IF(OPERND(I,17).EQ.450.0D0) THEN
C     AK
              REG(9)=
     1        ALENS(84,INT(OPERND(I,8)))
              GO TO 777
          END IF
          IF(OPERND(I,17).EQ.451.0D0) THEN
C     AL
              REG(9)=
     1        ALENS(85,INT(OPERND(I,8)))
              GO TO 777
          END IF

 777      CONTINUE

          IF(OPERND(I,17).GE.297.0D0.AND.OPERND(I,17).LE.444.0D0) THEN
              IF(INT(OPERND(I,8)).LT.INT(SYSTEM1(20))) THEN
                  REG(9)=REG(9)/INV
              ELSE
                  REG(9)=V/INV
              END IF
          END IF
          IF(OPERND(I,17).EQ.445.0D0.OR.OPERND(I,17).EQ.446.0D0) THEN
              IF(INT(OPERND(I,8)).LT.INT(SYSTEM1(20))) THEN
              ELSE
                  REG(9)=V
              END IF
          END IF
C     NOW LOAD THE CONTENTS OF THE ACCUMULATOR INTO THE OPERAND # I
C     LOAD VALUE1 FROM ACC
          IF(OPERND(I,15).EQ.0.0D0) THEN
C     SET ORIGINAL VALUE1S AND CHANGE HLD TO COR WITH
C     ORIG VALUE1 AS TARGET
C     SET ORIGINAL VALUE1
              OPERND(I,3)=REG(9)
              IF(OPERND(I,13).EQ.10.0D0) THEN
C     HLD FOUND TO CONVERT
                  OPERND(I,2)=OPERND(I,3)
                  OPERND(I,13)=1.0D0
              END IF
              OPERND(I,15)=1.0D0
          END IF
C     SET PREVIOUS VALUE1 TO OLD CURRENT VALUE1
          OPERND(I,5)=OPERND(I,4)
C     LOAD NEW CURRENT VALUE1
          OPERND(I,4)=REG(9)
C     CALCULATE NEW CHANGE VALUE1
          OPERND(I,6)=OPERND(I,4)-OPERND(I,5)
C
C     NOW CALCULATE THE SQUARE ROOT OF THE CONTRIBUTION TO THE MERIT FUMCTION
C     WHICH IS THE THE:
C     CURRENT OPERAND VALUE1-TARGET VALUE1 FOR THE OPERAND
C     MULTIPLIED BY THE SQUARE ROOT OF THE OPERAND WEIGHT
C
          IF(OPERND(I,13).EQ.0.0D0)
     1    OPERND(I,14)=0.0D0
C
          IF(OPERND(I,13).EQ.1.0D0) THEN
              OPERND(I,14)=(OPERND(I,4)-OPERND(I,2))
              IF(OPERND(I,14).EQ.0.0D0) OPERND(I,4)=OPERND(I,2)
              OPERND(I,14)=((OPERND(I,7)))*(OPERND(I,14))
          END IF
C
          IF(OPERND(I,13).EQ.-2.0D0) THEN
              IF(OPERND(I,4).LT.OPERND(I,2))THEN
                  OPERND(I,14)=(OPERND(I,4)-OPERND(I,2))
                  IF(OPERND(I,14).EQ.0.0D0) OPERND(I,4)=OPERND(I,2)
                  OPERND(I,14)=((OPERND(I,7)))*(OPERND(I,14))
              END IF
              IF(OPERND(I,4).GE.OPERND(I,2))
     1        OPERND(I,14)=0.0D0
          END IF
C
          IF(OPERND(I,13).EQ.2.0D0) THEN
              IF(OPERND(I,4).GT.OPERND(I,2)) THEN
                  OPERND(I,14)=(OPERND(I,4)-OPERND(I,2))
                  IF(OPERND(I,14).EQ.0.0D0) OPERND(I,4)=OPERND(I,2)
                  OPERND(I,14)=((OPERND(I,7)))*(OPERND(I,14))
              END IF
              IF(OPERND(I,4).LE.OPERND(I,2))
     1        OPERND(I,14)=0.0D0
          END IF
C
C     RESTORE ORIGINAL ACCUMULATOR VALUE1
          REG(9)=OLDREG9
          OLDLDIF2=.TRUE.
          OLDLDIF=.TRUE.
          LDIF2=.TRUE.
          LDIF=.TRUE.
          RETURN
      END


      FUNCTION RUN_OPT_MAC(I)
          IMPLICIT NONE
          INTEGER I,OLDOUT1
          REAL*8 RUN_OPT_MAC
          COMMON/OLDOUTT/OLDOUT1
          INCLUDE 'datmai.inc'
          OLDOUT1=OUT
          OUT=98
          SAVE_KDP(1)=SAVEINPT(1)
          INPUT='MACROOPT'
          CALL PROCES
          REST_KDP(1)=RESTINPT(1)
          OUT=OLDOUT1
          RUN_OPT_MAC=GPREG(I)
          RETURN
      END
