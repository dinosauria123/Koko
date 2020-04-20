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

C       EIGHTH SET OF OPTIMIZATION ROUTINES

C SUB MERIT2.FOR
      SUBROUTINE MERIT2
C
          IMPLICIT NONE
C
          LOGICAL PSFEXT2
C
          INCLUDE 'datsub.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          INCLUDE 'datcfg.inc'
          INCLUDE 'datspd.inc'
C
C       THIS IS SUBROUTINE MERIT2. THIS IS THE SUBROUTINE WHICH
C       HANDLES OPNRD
C       AT THE CMD LEVEL
          PSFEXT2=PSFEXT
C
C             NRD
C
          IF(WC.EQ.'NRD') THEN
C     SYNTAX CHECK
              IF(STI.EQ.1) THEN
                  WRITE(OUTLYNE,*)
     1            '"NRD" SETS PUPIL RAY GRID FOR CAPFN CALCULATIONS IN PSF'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'NRD CURRENTLY SET TO ',INT(NRD)
                  CALL SHOWIT(1)
                  RETURN
              END IF
              IF(S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1.OR.SST.EQ.1
     1        .OR.SQ.EQ.1) THEN
                  WRITE(OUTLYNE,*)'"NRD" ONLY TAKES NUMERIC WORD #1 INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(S1.EQ.0) THEN
                  WRITE(OUTLYNE,*)'"NRD" REQUIRES EXPLICIT NUMERIC WORD #1 INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(W1.LT.16.0D0.OR.W1.GT.512.0D0.OR.
     1        (W1-DBLE(INT(W1))).NE.0.0D0.OR.W1.GT.512.0D0.OR.
     2        ((W1/2.0D0)-DBLE(INT(W1/2.0D0))).NE.0.0D0) THEN
                  WRITE(OUTLYNE,*)
     1            '"NRD" REQUIERS NUMERIC INPUT TO BE A POSITIVE EVEN INTEGER'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)
     1            'EVEN INTEGER VALUE FROM 16 TO 512'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*) '"NRD" RESET TO 16'
                  CALL SHOWIT(1)
                  W1=16.0D0
              END IF
              IF(INT(W1).GT.TGR) THEN
                  WRITE(OUTLYNE,*)
     1            '"NRD" MAY NOT BE SET TO BE GREATER THAN THE CURRENT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)
     1            'VALUE OF TGR WHICH IS : ',TGR
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'"NRD" HAS BEEN RESET TO ',TGR
                  CALL SHOWIT(1)
                  NRD=TGR
                  RETURN
              END IF
              NRD=INT(W1)
              NRDFLG=1
              GRIFLG=0
              CPFNEXT=.FALSE.
              RETURN
          END IF
C
C             CAPFNNRD
C
          IF(WC.EQ.'CAPFNNRD') THEN
C     SYNTAX CHECK
              IF(STI.EQ.1) THEN
                  WRITE(OUTLYNE,*)
     1            '"CAPFNNRD" SETS PUPIL RAY GRID FOR CAPFN CALCULATIONS'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'CAPFNNRD CURRENTLY SET TO ',INT(CAPDEF)
                  CALL SHOWIT(1)
                  RETURN
              END IF
              IF(S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1.OR.SST.EQ.1
     1        .OR.SQ.EQ.1) THEN
                  WRITE(OUTLYNE,*)'"CAPFNNRD" ONLY TAKES NUMERIC WORD #1 INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(S1.EQ.0) THEN
                  WRITE(OUTLYNE,*)
     1            '"CAPFNNRD" REQUIRES EXPLICIT NUMERIC WORD #1 INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(W1.LT.16.0D0.OR.W1.GT.512.0D0.OR.
     1        (W1-DBLE(INT(W1))).NE.0.0D0.OR.
     2        ((W1/2.0D0)-DBLE(INT(W1/2.0D0))).NE.0.0D0) THEN
                  WRITE(OUTLYNE,*)
     1            '"CAPFNNRD" REQUIERS NUMERIC INPUT TO BE A POSITIVE EVEN INTEGER'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)
     1            'EVEN INTEGER VALUE FROM 16 TO 512'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              CAPDEF=DBLE(INT(W1))
              NRD=INT(CAPDEF)
              IF(NRD.EQ.8) TGR=32
              IF(NRD.EQ.16) TGR=64
              IF(NRD.GT.16.AND.NRD.LE.32) TGR=128
              IF(NRD.GT.32.AND.NRD.LE.64) TGR=256
              IF(NRD.GT.64.AND.NRD.LE.128) TGR=512
              PGR=TGR-1
              RETURN
          END IF
C
C             GRI
C
          IF(WC.EQ.'GRI') THEN
C     SYNTAX CHECK
              IF(STI.EQ.1) THEN
                  WRITE(OUTLYNE,*)
     1            '"GRI" SETS GRID SPACING IN THE PSF'
                  CALL SHOWIT(1)
                  IF(GRIFLG.EQ.1)
     1            WRITE(OUTLYNE,*)'GRI CURRENTLY SET TO ',GRI
                  IF(GRIFLG.EQ.0)
     1            WRITE(OUTLYNE,*)'GRI VALUE HAS NOT BEEN EXPLICITLY SET'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'AND WILL BE SET BY "TGR" AND "NRD"'
                  CALL SHOWIT(1)
                  RETURN
              END IF
              IF(S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1.OR.SST.EQ.1
     1        .OR.SQ.EQ.1) THEN
                  WRITE(OUTLYNE,*)'"GRI" ONLY TAKES NUMERIC WORD #1 INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(S1.EQ.0) THEN
                  WRITE(OUTLYNE,*)'"GRI" REQUIRES EXPLICIT NUMERIC WORD #1 INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(W1.LE.0.0D0) THEN
                  WRITE(OUTLYNE,*)
     1            '"GRI" REQUIERS NUMERIC INPUT TO BE GREATER THAN ZERO'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              GRI=W1
              NRDFLG=0
              GRIFLG=1
              CPFNEXT=.FALSE.
              RETURN
          END IF
C
C             EXTENT
C
          IF(WC.EQ.'EXTENT') THEN
C     SYNTAX CHECK
              IF(STI.EQ.1) THEN
                  WRITE(OUTLYNE,*)
     1            '"EXTENT" SETS GRID SPACING/EXTENT IN THE PSF'
                  CALL SHOWIT(1)
                  IF(GRIFLG.EQ.1)
     1            WRITE(OUTLYNE,*)'EXTENT CURRENTLY SET TO ',GRI*DBLE(PGR)
                  IF(GRIFLG.EQ.0)
     1            WRITE(OUTLYNE,*)'EXTENT VALUE HAS NOT BEEN EXPLICITLY SET'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'AND WILL BE SET BY "TGR", "NRD" AND "PGR"'
                  CALL SHOWIT(1)
                  RETURN
              END IF
              IF(S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1.OR.SST.EQ.1
     1        .OR.SQ.EQ.1) THEN
                  WRITE(OUTLYNE,*)'"EXTENT" ONLY TAKES NUMERIC WORD #1 INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(S1.EQ.0) THEN
                  WRITE(OUTLYNE,*)'"EXTENT" REQUIRES EXPLICIT NUMERIC WORD #1 INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(W1.LE.0.0D0) THEN
                  WRITE(OUTLYNE,*)
     1            '"EXTENT" REQUIERS NUMERIC INPUT TO BE GREATER THAN ZERO'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              GRI=W1/DBLE(PGR)
              NRDFLG=0
              GRIFLG=1
              CPFNEXT=.FALSE.
              RETURN
          END IF
C
C             TGR
C
          IF(WC.EQ.'TGR') THEN
C     SYNTAX CHECK
              IF(STI.EQ.1) THEN
                  WRITE(OUTLYNE,*)
     1            '"TGR" SETS TRANSFORM GRID FOR PSF'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'TGR CURRENTLY SET TO ',TGR
                  CALL SHOWIT(1)
                  RETURN
              END IF
              IF(S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1.OR.SST.EQ.1
     1        .OR.SQ.EQ.1) THEN
                  WRITE(OUTLYNE,*)'"TGR" ONLY TAKES NUMERIC WORD #1 INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(S1.EQ.0) THEN
                  WRITE(OUTLYNE,*)'"TGR" REQUIRES EXPLICIT NUMERIC WORD #1 INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(
     1        W1.NE.64.0D0.AND.W1.NE.128.0D0.AND.W1.NE.256.0D0
     1        .AND.W1.NE.512.0D0.AND.W1.NE.1024.0D0.AND.W1.NE.2048.0D0
     1        .AND.W1.NE.4096) THEN
                  PGR=TGR-1
                  WRITE(OUTLYNE,*)
     1            '"TGR" REQUIERS NUMERIC INPUT TO BE A POSITIVE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)
     1            'POWER OF 2 BETWEEN 32 AND 4096'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(INT(W1).LT.NRD) THEN
                  WRITE(OUTLYNE,*)
     1            '"TGR" MAY NOT BE SET TO LESS THAN THE CURRENT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)
     1            'VALUE OF NRD WHICH IS : ',NRD
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              TGR=INT(W1)
              TGRFLG=1
              CPFNEXT=.FALSE.
              IF((TGR-1).LE.PGR) THEN
                  PGR=TGR-1
                  RETURN
              END IF
          END IF
C
C
C             PGR
C
          IF(WC.EQ.'PGR') THEN
C     SYNTAX CHECK
              IF(STI.EQ.1) THEN
                  WRITE(OUTLYNE,*)
     1            '"PGR" SETS PLOTTING GRID FOR PSF'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'PGR CURRENTLY SET TO ',PGR
                  CALL SHOWIT(1)
                  RETURN
              END IF
              IF(S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1.OR.SST.EQ.1
     1        .OR.SQ.EQ.1) THEN
                  WRITE(OUTLYNE,*)'"PGR" ONLY TAKES NUMERIC WORD #1 INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(S1.EQ.0) THEN
                  WRITE(OUTLYNE,*)'"PGR" REQUIRES EXPLICIT NUMERIC WORD #1 INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(INT(W1).GE.TGR) THEN
                  WRITE(OUTLYNE,*)
     1            '"PGR" MUST BE LESS THAN TGR'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF((DBLE(INT(W1)/2)*2.0D0).EQ.W1) THEN
                  WRITE(OUTLYNE,*)
     1            '"PGR" MUST BE SET TO AN ODD INTEGER VALUE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              PGR=INT(W1)
              CPFNEXT=.FALSE.
          END IF
C
C             OPNRD
C
          IF(WC.EQ.'OPNRD') THEN
C     SYNTAX CHECK
              IF(STI.EQ.1) THEN
                  WRITE(OUTLYNE,*)
     1            '"OPNRD" SETS GRID FOR CAPFN CALCULATIONS IN OPTIMIZATION'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'OPNRD CURRENTLY SET TO ',OPNRD
                  CALL SHOWIT(1)
                  RETURN
              END IF
              IF(S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1.OR.SST.EQ.1
     1        .OR.SQ.EQ.1) THEN
                  WRITE(OUTLYNE,*)'"OPNRD" ONLY TAKES NUMERIC WORD #1 INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(S1.EQ.0) THEN
                  WRITE(OUTLYNE,*)'"OPNRD" REQUIRES EXPLICIT NUMERIC WORD #1 INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(W1.LT.16.0D0.OR.W1.GT.512.0D0.OR.
     1        (W1-DBLE(INT(W1))).NE.0.0D0.OR.
     2        ((W1/2.0D0)-DBLE(INT(W1/2.0D0))).NE.0.0D0) THEN
                  WRITE(OUTLYNE,*)
     1            '"OPNRD" REQUIERS NUMERIC INPUT TO BE A POSITIVE EVEN INTEGER'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)
     1            'EVEN INTEGER VALUE FROM 16 TO 512'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              OPNRD=INT(W1)
              CPFNEXT=.FALSE.
              RETURN
          END IF
C            TOLNRD
C
          IF(WC.EQ.'TOLNRD') THEN
C     SYNTAX CHECK
              IF(STI.EQ.1) THEN
                  WRITE(OUTLYNE,*)
     1            '"TOLNRD" SETS GRID FOR CAPFN CALCULATIONS IN TOLERANCING'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'TOLNRD CURRENTLY SET TO ',TOLNRD
                  CALL SHOWIT(1)
                  RETURN
              END IF
              IF(S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1.OR.SST.EQ.1
     1        .OR.SQ.EQ.1) THEN
                  WRITE(OUTLYNE,*)'"TOLNRD" ONLY TAKES NUMERIC WORD #1 INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(S1.EQ.0) THEN
                  WRITE(OUTLYNE,*)'"TOLNRD" REQUIRES EXPLICIT NUMERIC WORD #1 INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(W1.LT.16.0D0.OR.W1.GT.512.0D0.OR.
     1        (W1-DBLE(INT(W1))).NE.0.0D0.OR.
     2        ((W1/2.0D0)-DBLE(INT(W1/2.0D0))).NE.0.0D0) THEN
                  WRITE(OUTLYNE,*)
     1            '"TOLNRD" REQUIERS NUMERIC INPUT TO BE A POSITIVE EVEN INTEGER'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)
     1            'EVEN INTEGER VALUE FROM 16 TO 512'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              TOLNRD=INT(W1)
              CPFNEXT=.FALSE.
              RETURN
          END IF
C
          RETURN
      END
C SUB TOPER1.FOR
      SUBROUTINE TOPER1
C
          IMPLICIT NONE
C
          INTEGER OS5,ODF5,I,II,OPT
C
          REAL*8 OW5
C
          CHARACTER*8 OPNM
C
          LOGICAL YES
C
          REAL*8 OPWEIT,IREG
C
          REAL*8 OP1,OP2,OP3,OP4,OP5,OP6,OP7,OP8,OP9,OP10
     1    ,OP11,OP12,OP13,OP14,OP15,OP16,OP17,OP18,OP19,OP20

          INTEGER OPNNM

          LOGICAL OPOK
C
          INCLUDE 'datsub.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          INCLUDE 'datcfg.inc'
          INCLUDE 'datspd.inc'
C
C       THIS IS SUBROUTINE TOPER1. THIS IS THE SUBROUTINE WHICH
C       HANDLES TOPER INPUT AND TOPER UPDATE COMMANDS AND
C       OUTPUT COMMANDS AT THE CMD LEVEL
C
C       THE ARRAY OPERND AND OPNAM STORE TOPER INFORMATION
C       IT IS PASSED IN COMMON IN THE INCLUDE FILE DATSUB.FOR
C
C       OPERND(I,J) WHERE I COUNTS THE NUMBER OF OPERAND ENTRIES
C       AND J TAKES ON THE FOLLOWING VALUES AND MEANIINGS.
C
C       I WILL BE 1 TO 5 PLUS MAXFOCRIT ONLY
C
C       J=1  > 1 THROUGH 10, THE NUMBER OF THE FUNCTION IN WHICH
C               THE ACTUAL OPERAND IS CALCULATED (COMMAND WORD)
C         (0 IS PREDEFINED OPERAND)
C
C       J=2  > TARGET VALUE OF THE OPERAND, THIS WILL BE THE ORIGINAL VALUE
C       J=3  > OPERAND ORIGINAL VALUE
C       J=4  > OPERAND CURRENT VALUE
C       J=5  > OPERAND PREVIOUS VALUE
C       J=6  > LAST OPERAND CHANGE VALUE (CURRENT-PREVIOUS)
C       J=7  > MAX TOPER CHANGE BEFORE MESSAGE IS ISSUED
C
C       J=8  > NUMBER OF THE GENERAL PURPOSE REGISTER CONTAINING
C               THE ACTUAL OPERAND VALUE AS RETURNED FROM THE
C               CALCULATING FUNCTION (NUMERIC WORD #2) (VALUES 1 TO 300)
C               (A DEFAULT ENTRY HERE CAUSES THE ACCUMULATOR TO BE USED)
C       FOR PREDEFINED OPERAND, IT IS THE (I) INPUT VALUE
C
C       J=9  > OPTIONAL NW1 FOR THE FUNCTION (USING NSUB) (NUMERIC WORD #3)
C       J=10 > OPTIONAL NW2 FOR THE FUNCTION (USING NSUB) (NUMERIC WORD #4)
C     FOR PREDEFINED OPERANDS J=9 AND J=10 ARE THE (J) AND (K) INPUT VALUES
C       J=11 > DEFAULT FLAG FOR NW1
C       J=12 > DEFAULT FLAG FOR NW2
C       J=13 > COR MODE 10=HLD ALWAYS 10
          CORMOD=10
C       J=14 > SQUARE ROOT OF WEIGHT TIMES (CURRENT VAL-TARGET))
C       J=15 > 0 BEFORE THE FIRST CALCULATION OF OPERAND VALUES
C              1 AFTER THE FIRST CALULATION
C       J=16 > CFG # FOR PREDEFINED OPERANDS (ALWAYS 1)
C       J=17 > CODE FOR THE PREDEFINED OPERANDS
C       J=18 > DEFAULT FLAG FOR W3 = DBLE(DF3)
C       J=19 > 0 IF OP CALCULABLE, 1 IF NOT
C       J=20 > MAX TOPER CHANGE BEFORE MESSAGE IS ISSUED
C             1=X
C             2=Y
C             3=Z
C             4=DCL OR K
C             5=DCM OR L
C             6=DCN OR M
C             7=DX
C             8=DY
C             9=DR
C            10=DXA
C            11=DYA
C            12=DRA
C            13=XANG
C            14=YANG
C            15=OPL
C            16=OPD
C            17=OPDW
C            18=LOLD
C            19=MOLD
C            20=NOLD
C            21=LEN
C            22=AII
C            23=AIP
C            24=LN
C            25=MN
C            26=NN
C            27=PXPX
C            28=PXPY
C            29=PYPX
C            30=PYPY
C            31=PXAPX
C            32=PXAPY
C            33=PYAPX
C            34=PYAPY
C            35=DXDX
C            36=DXDY
C            37=DYDX
C            38=DYDY
C            39=DXADX
C            40=DXADY
C            41=DYADX
C            42=DYADY
C            43=XREF
C            44=YREF
C            45=ZREF
C            46=LREF
C            47=MREF
C            48=NREF
C            49=LREFOL
C            50=MREFOL
C            51=NREFOL
C            52=IREF
C            53=IPREF
C            54=XAREF
C            55=YAREF
C            56=LNREF
C            57=MNREF
C            58=NNREF
C            59=GLX
C            60=GLY
C            61=GLZ
C            62=GLL
C            63=GLM
C            64=GLN
C            65=GLLOLD
C            66=GLMOLD
C            67=GLNOLD
C            68=LENREF
C            69=OPLREF
C            70=RD
C            71=CV
C            72=TH
C            73=CC
C            74=AC
C            75=AD
C            76=AE
C            77=AF
C            78=AG
C            79=RDTOR
C            80=CVTOR
C            81=CCTOR
C            82=ADTOR
C            83=AETOR
C            84=AFTOR
C            85=AGTOR
C            86=ALPHA
C            87=BETA
C            88=GAMMA
C            89=VNUM
C            90=PARTL
C            91=INDEX
C            92=XD
C            93=YD
C            94=XVERT
C            95=YVERT
C            96=ZVERT
C            97=LXVERT
C            98=MXVERT
C            99=NXVERT
C           100=LYVERT
C           101=MYVERT
C           102=NYVERT
C           103=LZVERT
C           104=MZVERT
C           105=NZVERT
C           106=LENGTH
C           106=OAL
C           107=MLENGTH
C           107=OPTLEN
C           108=ET
C           108=ETY
C           109=ETX
C           110=SHAPEFAC
C           111 TO 206 = C1 THROUGH C96
C           207=PWRY
C           208=PWRX
C           209=FLCLTHX
C           210=FLCLTH OR FLCLTHY
C           211=PY
C           212=PX
C           213=PCY
C           214=PCX
C           215=PUY
C           216=PUX
C           217=PUCY
C           218=PUCX
C           219=PIY
C           220=PIX
C           221=PICY
C           222=PICX
C           223=PIYP
C           224=PIXP
C           225=PICYP
C           226=PICXP
C           227=PACY
C           228=PACX
C           229=PLCY
C           230=PLCX
C           231=SACY
C           232=SACX
C           233=SLCY
C           234=SLCX
C           235=IMDISX
C           236=IMDISY
C           237=CENTX
C           238=CENTY
C           239=RMSX
C           240=RMSY
C           241=RMS
C           242=RSSX
C           243=RSSY
C           244=RSS
C           245=RMSOPD
C           246=ZERN37
C           247=MAGX
C           248=MAGY
C           249=MAGXOR
C           250=MAGYOR
C           251=FFLX
C           252=FFLY
C           253=BFLX
C           254=BFLY
C           255=FFNX
C           256=FFNY
C           257=BFNX
C           258=BFNY
C           259=EFLX
C           260=EFLY
C           261=ENDIAX
C           262=ENDIAY
C           263=EXDIAX
C           264=EXDIAY
C           265=ENPOSX
C           266=ENPOSY
C           267=ENPOSZ
C           268=EXPOSX
C           269=EXPOSY
C           270=EXPOSZ
C           271=FNUMX
C           272=FNUMY
C           273=OBFNUMX
C           274=OBFNUMY
C           275=ENPDIAX
C           276=ENPDIAY
C           277=EXPDIAX
C           278=EXPDIAY
C           279=PUPDIAX
C           280=PUPDIAY
C           281=PUPDISX
C           282=PUPDISY
C           283=CHFIMX
C           284=CHFIMY
C           285=GPX
C           286=GPY
C           287=GPUX
C           288=GPUY
C           289=GPCX
C           290=GPCY
C           291=GPUCX
C           292=GPUCY
C           293=DIST
C           294=XFOC
C           295=YFOC
C           296=AST
C           297=SA3
C           298=XSA3
C           299=CMA3
C           300=XCMA3
C           301=AST3
C           302=XAST3
C           303=DIS3
C           304=XDIS3
C           305=PTZ3
C           306=XPTZ3
C           307=SA5
C           308=XSA5
C           309=CMA5
C           310=XCMA5
C           311=AST5
C           312=XAST5
C           313=DIS5
C           314=XDIS5
C           315=PTZ5
C           316=XPTZ5
C           317=TOBSA
C           318=XTOBSA
C           319=SOBSA
C           320=XSOBSA
C           321=ELCMA
C           322=XELCMA
C           323=TAS
C           324=XTAS
C           325=SAS
C           326=XSAS
C           327=SA7
C           328=XSA7
C           329=SA3P
C           330=XSA3P
C           331=CMA3P
C           332=XCMA3P
C           333=AST3P
C           334=XAST3P
C           335=DIS3P
C           336=XDIS3P
C           337=PTZ3P
C           338=XPTZ3P
C           339=SA5P
C           340=XSA5P
C           341=CMA3P
C           342=XCMA3P
C           343=AST5P
C           344=XAST5P
C           345=DIS5P
C           346=XDIS5P
C           347=PTZ5P
C           348=XPTZ5P
C           349=TOBSAP
C           350=XTOBSAP
C           351=SOBSAP
C           352=XSOBSAP
C           353=ELCMAP
C           354=XELCMAP
C           355=TASP
C           356=XTASP
C           357=SASP
C           358=XSASP
C           359=SA7P
C           360=XSA7P
C           361=SA3S
C           362=XSA3S
C           363=CMA3S
C           364=XCMA3S
C           365=AST3S
C           366=XAST3S
C           367=DIS3S
C           368=XDIS3S
C           369=PTZ3S
C           370=XPTZ3S
C           371=SA5S
C           372=XSA5S
C           373=CMA5S
C           374=XCMA5S
C           375=AST5S
C           376=XAST5S
C           377=DIS5S
C           378=XDIS5S
C           379=PTZ5S
C           380=XPTZ5S
C           381=TOBSAS
C           382=XTOBSAS
C           383=SOBSAS
C           384=XSOBSAS
C           385=ELCMAS
C           386=XELCMAS
C           387=TASS
C           388=XTASS
C           389=SASS
C           390=XSASS
C           391=SA7S
C           392=XSA7S
C           393=SA5I
C           394=XSA5I
C           395=CMA5I
C           396=XCMA5I
C           397=AST5I
C           398=XAST5I
C           399=DIS5I
C           400=XDIS5I
C           401=PTZ5I
C           402=XPTZ5I
C           403=TOBSAI
C           404=XTOBSAI
C           405=SOBSAI
C           406=XSOBSAI
C           407=ELCMAI
C           408=XELCMAI
C           409=TASI
C           410=XTASI
C           411=SASI
C           412=XSASI
C           413=SA7I
C           414=XSA7I
C           415=PSA3
C           416=XPSA3
C           417=PCMA3
C           418=XPCMA3
C           419=PAST3
C           420=XPAST3
C           421=PDIS3
C           422=XPDIS3
C           423=PPTZ3
C           424=XPPTZ3
C           425=PSA3P
C           426=XPSA3P
C           427=PCMA3P
C           428=XPCMA3P
C           429=PAST3P
C           430=XPAST3P
C           431=PDIS3P
C           432=XPDIS3P
C           433=PPTZ3P
C           434=XPPTZ3P
C           435=PSA3S
C           436=XPSA3S
C           437=PCMA3S
C           438=XPCMA3S
C           439=PAST3S
C           440=XPAST3S
C           441=PDIS3S
C           442=XPDIS3S
C           443=PPTZ3S
C           444=XPPTZ3S
C           445=PTZCV
C           446=XPTZCV
C           447=AH
C           448=AI
C           449=AJ
C           450=AK
C           451=AL
C           452='GBRADX'
C           453='GBRADY'
C           454='GBDISX'
C           455='GBDISY'
C           456='GBRCVX'
C           457='GBRCVY'
C           458='GBWAISTX'
C           459='GBWAISTY'
C           460='MGOTF'
C           461='PGOTF'
C           462='MDOTF'
C           463='PDOTF'
C           460='GOTFM'
C           461='GOTFP'
C           462='DOTFM'
C           463='DOTFP'
C           464='RED'
C           465='REDCEN'
C           466='FISHDIST'
C           467='ZD'
C           468='SYMX'
C           469='SYMY'
C           470='ASYMX'
C           471='ASYMY'
C           472='PACM'
C           473='PACZ'
C           474='SACM'
C           475='SACZ'
C           476='PLCM'
C           477='PLCZ'
C           478='SLCM'
C           479='SLCZ'
C           480='CTSX'
C           481='CTSY'
C           482='SCEX'
C           483='SCEY'
C           484='GREYS'
C           485='PIVX'
C           486='PIVY'
C           487='PIVZ'
C           488='N1'
C           489='N2'
C           490='N3'
C           491='N4'
C           492='N5'
C           493='N6'
C           494='N7'
C           495='N8'
C           496='N9'
C           497='N10'
C           498='ABBE'
C           499='DPART'
C           500='CLPX'
C           501='CLPY'
C           502='GDX'
C           503='GDY'
C           504='GDZ'
C           505='GALPHA'
C           506='GBETA'
C           507='GGAMMA'
C           508='GRS'
C           509='WEIGHT'
C           510='DMINUSD'
C           511='COST'
C           512='MACOPT'
C           513='RMSYX'
C
C       OPNM=(8 CHARACTER USER DEFINED, NON-DUPLICATED OPERAND NAME)
C
          IF(WC.EQ.'M'.OR.WC.EQ.'C') THEN
              CALL MESCOM
              RETURN
          END IF
C
C       "TOPS" OUTPUT TOPER DATA FROM INSIDE
C       AND OUTSIDE OF THE TOPER SUBFILE VIA SUBROUTINE FCOUT.FOR.
          IF(WC.EQ.'TOPS') THEN
              OPTMES=.FALSE.
              CALL TOPOUT
              OPTMES=.TRUE.
              RETURN
          END IF
C
C       NOW DO CASE OF WC = EOS
C
C***********************************************************************
C       DEAL WITH WC=EOS
          IF(WC.EQ.'EOS') THEN
              IF(SST.EQ.1.OR.SQ.EQ.1.OR.SN.EQ.1)THEN
                  WRITE(OUTLYNE,*)'"EOS" TAKES NO EXPLICIT INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C       PROCEED WITH ACTION FOR COMMAND
              F1=1
              F53=0
              YES=.FALSE.
              DO II=1,MAXTOP
                  IF(ISTOP(II)) YES=.TRUE.
              END DO
              IF(.NOT.YES) TOPCNT=0
              IF(YES) TOPCNT=1
              IF(TOPCNT.EQ.0) THEN
                  WRITE(OUTLYNE,*)'THE TOPER SUBFILE IS EMPTY'
                  CALL SHOWIT(1)
              END IF
              RETURN
C       ACTION COMPLETED
          END IF
C
C       EOS DONE
C***********************************************************************
C
C       NOW DO WC=DEL
          IF(WC.EQ.'DEL') THEN
              IF(F53.NE.2) THEN
                  WRITE(OUTLYNE,*)
     1            '"DEL" IS ONLY AVAILABLE FROM THE "UPDATE TOPER" LEVEL'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'NO ACTION TAKEN'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(SST.EQ.1.OR.SQ.EQ.1.OR.S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1
     1        .OR.S5.EQ.1) THEN
                  WRITE(OUTLYNE,*)
     1            '"DEL" ONLY TAKES NUMERIC WORD #1 INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(S1.EQ.0) THEN
                  WRITE(OUTLYNE,*)
     1            '"DEL" REQUIRES EXPLICIT NUMERIC WORD #1 INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(W1.LT.1.0D0.OR.W1.GT.5.0D0) THEN
                  WRITE(OUTLYNE,*)
     1            '"DEL" REQUIRES NUMERIC INPUT FROM 1 TO 5 ONLY'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              FMTEXT=.FALSE.
C
C     HERE IS WHERE TOPER IS DELETED
              ISTOP(INT(W1))=.FALSE.
              DO II=1,20
                  OPERND(INT(W1),II+MAXFOCRIT)=0.0D0
              END DO
              YES=.FALSE.
              DO II=1,MAXTOP
                  IF(ISTOP(II)) YES=.TRUE.
              END DO
              IF(.NOT.YES) TOPCNT=0
              IF(YES) TOPCNT=1
              IF(TOPCNT.EQ.0) THEN
                  WRITE(OUTLYNE,*)'THE TOPER SUBFILE IS EMPTY'
                  CALL SHOWIT(1)
              END IF
C     ALL DELETIONS COMPLETED
              RETURN
C
          END IF
C***********************************************************************
C
C       NOW DO WC=OP_DESC
          IF(WC.EQ.'OP_DESC') THEN
              IF(SN.EQ.1) THEN
                  WRITE(OUTLYNE,*)
     1            '"OP_DESC" TAKES NO NUMERIC INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(SQ.EQ.0.OR.SST.EQ.0) THEN
                  WRITE(OUTLYNE,*)
     1            '"OP_DESC" REQUIRES EXPLICIT QUALIFIER AND STRING INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              OPOK=.FALSE.
              IF(WQ.EQ.'OP1') OPOK=.TRUE.
              IF(WQ.EQ.'OP2') OPOK=.TRUE.
              IF(WQ.EQ.'OP3') OPOK=.TRUE.
              IF(WQ.EQ.'OP4') OPOK=.TRUE.
              IF(WQ.EQ.'OP5') OPOK=.TRUE.
              IF(WQ.EQ.'OP1') OPNNM=1
              IF(WQ.EQ.'OP2') OPNNM=2
              IF(WQ.EQ.'OP3') OPNNM=3
              IF(WQ.EQ.'OP4') OPNNM=4
              IF(WQ.EQ.'OP5') OPNNM=5
              IF(.NOT.OPOK) THEN
                  WRITE(OUTLYNE,*)
     1            'DURING TOLERANCE OPERAND INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)
     1            '"OP_DESC" REQUIRES A QUALIFIER WORD "OP1" THROUGH "OP5"'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(.NOT.ISTOP(OPNNM)) THEN
                  WRITE(OUTLYNE,*)
     1            'TOLERANCE OPERAND ',OPNNM,' HAS NOT YET BEEN DEFINED'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)
     1            'A TOLERANCE OPERAND DESCRIPTION ENTRY IS NOT ALLOWED'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'NO ACTION TAKEN'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C
C     HERE IS WHERE OP_DESC IS ASSIGNED
              IF(WS(1:8).NE.'        ') OPERDESC(MAXFOCRIT+OPNNM)=WS
              RETURN
C
          END IF
C
C     SAVE NUMERIC WORD #5
          OW5=W5
          ODF5=DF5
          OS5=S5
          IF(DF5.EQ.1) OW5=0.0D0
C
C     HERE WE SHIFT THE INPUT EXCEPT FOR W1
          W5=W4
          DF5=DF4
          S5=S4
          W4=W3
          DF4=DF3
          S4=S3
          W3=W2
          DF3=DF2
          S3=S2
          W2=0.0D0
          S2=0
          DF2=1
C
C
C     START DOING THE FUNCTION NAMES HERE
          OPT=-1
          IF(WC.EQ.'FUNC00  ') OPT=0
          IF(WC.EQ.'FUNC01  ') OPT=1
          IF(WC.EQ.'FUNC02  ') OPT=2
          IF(WC.EQ.'FUNC03  ') OPT=3
          IF(WC.EQ.'FUNC04  ') OPT=4
          IF(WC.EQ.'FUNC05  ') OPT=5
          IF(WC.EQ.'FUNC06  ') OPT=6
          IF(WC.EQ.'FUNC07  ') OPT=7
          IF(WC.EQ.'FUNC08  ') OPT=8
          IF(WC.EQ.'FUNC09  ') OPT=9
          IF(WC.EQ.'FUNC10  ') OPT=10
          IF(OPT.EQ.-1) THEN
              WRITE(OUTLYNE,*)
     1        'INVALID FUNCTION NAME COMMAND WORD'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
C     NUMERIC WORDS AND DEFAULTS
C
C     NW1 IS THE OPERAND NUMBER, DEFUALT WILL BE ORIGINAL VALUE
C
          IF(DF1.EQ.1) THEN
              WRITE(OUTLYNE,*)
     1        'EXPLICIT TOPER NUMBER MUST BE INPUT AS NUMERIC WORD #1'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(W1.LT.1.OR.W1.GT.5.0D0) THEN
              WRITE(OUTLYNE,*)
     1        'EXPLICIT TOPER NUMBER MUST BE IN THE RANGE 1 TO 5'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          OPWEIT=1.0D0
          IF(OPT.NE.0) THEN
C     NW2 IS GENERAL PURPOSE STORAGE REGISTER VALUE
              IF(DF3.EQ.1) THEN
                  IF(F53.EQ.1.OR.F53.EQ.2) THEN
                      WRITE(OUTLYNE,*)
     1                'NUMERIC WORD #2 (GP REGISTER ADDRESS) MUST BE EXPLICITLY INPUT'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'FOR A USER DEFINED TOLERANCE OPERAND'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              ELSE
C     NOT DEFAULT
                  IF((DBLE(DINT(W3))-W3).NE.0.0D0) THEN
                      WRITE(OUTLYNE,*)
     1                'NUMERIC WORD #2 (GP REGISTER ADDRESS) MUST BE AN INTEGER'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  IF(W3.LT.1.0D0.OR.W3.GT.400.0D0) THEN
                      WRITE(OUTLYNE,*)
     1                'NUMERIC WORD #2 (GP REGISTER ADDRESS) BEYOND LEGAL RANGE'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'VALID RANGE IS 1 TO 400'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  IF(DF3.NE.1.AND.W3.LT.0.0D0) THEN
                      WRITE(OUTLYNE,*)
     1                'NUMERIC WORD #2 (GP REGISTER ADDRESS) MAY NOT BE NEGATIVE'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  IREG=W3
                  OP8=W3
                  OP18=DBLE(DF3)
                  OP19=0.0D0
              END IF
          END IF

C     NW4 IS NW1 FOR FUNCTION NSUB FOR USER DEF OPS
C     NW5 IS NW2 FOR FUNCTION NSUB FOR USER DEF OPS
C     DEFAULT STATUS WILL BE HANDLED DURING FUNCTION CALLING
C     AND WILL BE PASSED IN ITEMS J=11 AND J=12
C
          IF(OPT.GE.1.AND.OPT.LE.10) GO TO 3141
C
          OPNM=WQ
          IF(OPNM.EQ.'X       ') OP17=1.0D0
          IF(OPNM.EQ.'Y       ') OP17=2.0D0
          IF(OPNM.EQ.'Z       ') OP17=3.0D0
          IF(OPNM.EQ.'DCL     ') OP17=4.0D0
          IF(OPNM.EQ.'K       ') OP17=4.0D0
          IF(OPNM.EQ.'DCM     ') OP17=5.0D0
          IF(OPNM.EQ.'L       ') OP17=5.0D0
          IF(OPNM.EQ.'DCN     ') OP17=6.0D0
          IF(OPNM.EQ.'M       ') OP17=6.0D0
          IF(OPNM.EQ.'DX      ') OP17=7.0D0
          IF(OPNM.EQ.'DY      ') OP17=8.0D0
          IF(OPNM.EQ.'DR      ') OP17=9.0D0
          IF(OPNM.EQ.'DXA     ') OP17=10.0D0
          IF(OPNM.EQ.'DYA     ') OP17=11.0D0
          IF(OPNM.EQ.'DRA     ') OP17=12.0D0
          IF(OPNM.EQ.'XANG    ') OP17=13.0D0
          IF(OPNM.EQ.'YANG    ') OP17=14.0D0
          IF(OPNM.EQ.'OPL     ') OP17=15.0D0
          IF(OPNM.EQ.'OPD     ') OP17=16.0D0
          IF(OPNM.EQ.'OPDW    ') OP17=17.0D0
          IF(OPNM.EQ.'LOLD    ') OP17=18.0D0
          IF(OPNM.EQ.'MOLD    ') OP17=19.0D0
          IF(OPNM.EQ.'NOLD    ') OP17=20.0D0
          IF(OPNM.EQ.'LEN     ') OP17=21.0D0
          IF(OPNM.EQ.'AII     ') OP17=22.0D0
          IF(OPNM.EQ.'AIP     ') OP17=23.0D0
          IF(OPNM.EQ.'LN      ') OP17=24.0D0
          IF(OPNM.EQ.'MN      ') OP17=25.0D0
          IF(OPNM.EQ.'NN      ') OP17=26.0D0
          IF(OPNM.EQ.'PXPX    ') OP17=27.0D0
          IF(OPNM.EQ.'PXPY    ') OP17=28.0D0
          IF(OPNM.EQ.'PYPX    ') OP17=29.0D0
          IF(OPNM.EQ.'PYPY    ') OP17=30.0D0
          IF(OPNM.EQ.'PXAPX   ') OP17=31.0D0
          IF(OPNM.EQ.'PXAPY   ') OP17=32.0D0
          IF(OPNM.EQ.'PYAPX   ') OP17=33.0D0
          IF(OPNM.EQ.'PYAPY   ') OP17=34.0D0
          IF(OPNM.EQ.'DXDX    ') OP17=35.0D0
          IF(OPNM.EQ.'DXDY    ') OP17=36.0D0
          IF(OPNM.EQ.'DYDX    ') OP17=37.0D0
          IF(OPNM.EQ.'DYDY    ') OP17=38.0D0
          IF(OPNM.EQ.'DXADX   ') OP17=39.0D0
          IF(OPNM.EQ.'DXADY   ') OP17=40.0D0
          IF(OPNM.EQ.'DYADX   ') OP17=41.0D0
          IF(OPNM.EQ.'DYADY   ') OP17=42.0D0
          IF(OPNM.EQ.'XREF    ') OP17=43.0D0
          IF(OPNM.EQ.'YREF    ') OP17=44.0D0
          IF(OPNM.EQ.'ZREF    ') OP17=45.0D0
          IF(OPNM.EQ.'LREF    ') OP17=46.0D0
          IF(OPNM.EQ.'MREF    ') OP17=47.0D0
          IF(OPNM.EQ.'NREF    ') OP17=48.0D0
          IF(OPNM.EQ.'LREFOL  ') OP17=49.0D0
          IF(OPNM.EQ.'MREFOL  ') OP17=50.0D0
          IF(OPNM.EQ.'NREFOL  ') OP17=51.0D0
          IF(OPNM.EQ.'IREF    ') OP17=52.0D0
          IF(OPNM.EQ.'IPREF   ') OP17=53.0D0
          IF(OPNM.EQ.'XAREF   ') OP17=54.0D0
          IF(OPNM.EQ.'YAREF   ') OP17=55.0D0
          IF(OPNM.EQ.'LNREF   ') OP17=56.0D0
          IF(OPNM.EQ.'MNREF   ') OP17=57.0D0
          IF(OPNM.EQ.'NNREF   ') OP17=58.0D0
          IF(OPNM.EQ.'GLX     ') OP17=59.0D0
          IF(OPNM.EQ.'GLY     ') OP17=60.0D0
          IF(OPNM.EQ.'GLZ     ') OP17=61.0D0
          IF(OPNM.EQ.'GLL     ') OP17=62.0D0
          IF(OPNM.EQ.'GLM     ') OP17=63.0D0
          IF(OPNM.EQ.'GLN     ') OP17=64.0D0
          IF(OPNM.EQ.'GLLOLD  ') OP17=65.0D0
          IF(OPNM.EQ.'GLMOLD  ') OP17=66.0D0
          IF(OPNM.EQ.'GLNOLD  ') OP17=67.0D0
          IF(OPNM.EQ.'LENREF  ') OP17=68.0D0
          IF(OPNM.EQ.'OPLREF  ') OP17=69.0D0
          IF(OPNM.EQ.'RD      ') OP17=70.0D0
          IF(OPNM.EQ.'CV      ') OP17=71.0D0
          IF(OPNM.EQ.'TH      ') OP17=72.0D0
          IF(OPNM.EQ.'CC      ') OP17=73.0D0
          IF(OPNM.EQ.'AC      ') OP17=74.0D0
          IF(OPNM.EQ.'AD      ') OP17=75.0D0
          IF(OPNM.EQ.'AE      ') OP17=76.0D0
          IF(OPNM.EQ.'AF      ') OP17=77.0D0
          IF(OPNM.EQ.'AG      ') OP17=78.0D0
          IF(OPNM.EQ.'RDTOR   ') OP17=79.0D0
          IF(OPNM.EQ.'CVTOR   ') OP17=80.0D0
          IF(OPNM.EQ.'CCTOR   ') OP17=81.0D0
          IF(OPNM.EQ.'ADTOR   ') OP17=82.0D0
          IF(OPNM.EQ.'AETOR   ') OP17=83.0D0
          IF(OPNM.EQ.'AFTOR   ') OP17=84.0D0
          IF(OPNM.EQ.'AGTOR   ') OP17=85.0D0
          IF(OPNM.EQ.'ALPHA   ') OP17=86.0D0
          IF(OPNM.EQ.'BETA    ') OP17=87.0D0
          IF(OPNM.EQ.'GAMMA   ') OP17=88.0D0
          IF(OPNM.EQ.'VNUM    ') OP17=89.0D0
          IF(OPNM.EQ.'PARTL   ') OP17=90.0D0
          IF(OPNM.EQ.'INDEX   ') OP17=91.0D0
          IF(OPNM.EQ.'XD      ') OP17=92.0D0
          IF(OPNM.EQ.'YD      ') OP17=93.0D0
          IF(OPNM.EQ.'XVERT   ') OP17=94.0D0
          IF(OPNM.EQ.'YVERT   ') OP17=95.0D0
          IF(OPNM.EQ.'ZVERT   ') OP17=96.0D0
          IF(OPNM.EQ.'LXVERT  ') OP17=97.0D0
          IF(OPNM.EQ.'MXVERT  ') OP17=98.0D0
          IF(OPNM.EQ.'NXVERT  ') OP17=99.0D0
          IF(OPNM.EQ.'LYVERT  ') OP17=100.0D0
          IF(OPNM.EQ.'MYVERT  ') OP17=101.0D0
          IF(OPNM.EQ.'NYVERT  ') OP17=102.0D0
          IF(OPNM.EQ.'LZVERT  ') OP17=103.0D0
          IF(OPNM.EQ.'MZVERT  ') OP17=104.0D0
          IF(OPNM.EQ.'NZVERT  ') OP17=105.0D0
          IF(OPNM.EQ.'LENGTH  ') OP17=106.0D0
          IF(OPNM.EQ.'OAL     ') OP17=106.0D0
          IF(OPNM.EQ.'MLENGTH ') OP17=107.0D0
          IF(OPNM.EQ.'OPTLEN  ') OP17=107.0D0
          IF(OPNM.EQ.'ET      ') OP17=108.0D0
          IF(OPNM.EQ.'ETY     ') OP17=108.0D0
          IF(OPNM.EQ.'ETX     ') OP17=109.0D0
          IF(OPNM.EQ.'SHAPEFAC') OP17=110.0D0
          IF(OPNM.EQ.'C1      ') OP17=111.0D0
          IF(OPNM.EQ.'C2      ') OP17=112.0D0
          IF(OPNM.EQ.'C3      ') OP17=113.0D0
          IF(OPNM.EQ.'C4      ') OP17=114.0D0
          IF(OPNM.EQ.'C5      ') OP17=115.0D0
          IF(OPNM.EQ.'C6      ') OP17=116.0D0
          IF(OPNM.EQ.'C7      ') OP17=117.0D0
          IF(OPNM.EQ.'C8      ') OP17=118.0D0
          IF(OPNM.EQ.'C9      ') OP17=119.0D0
          IF(OPNM.EQ.'C10     ') OP17=120.0D0
          IF(OPNM.EQ.'C11     ') OP17=121.0D0
          IF(OPNM.EQ.'C12     ') OP17=122.0D0
          IF(OPNM.EQ.'C13     ') OP17=123.0D0
          IF(OPNM.EQ.'C14     ') OP17=124.0D0
          IF(OPNM.EQ.'C15     ') OP17=125.0D0
          IF(OPNM.EQ.'C16     ') OP17=126.0D0
          IF(OPNM.EQ.'C17     ') OP17=127.0D0
          IF(OPNM.EQ.'C18     ') OP17=128.0D0
          IF(OPNM.EQ.'C19     ') OP17=129.0D0
          IF(OPNM.EQ.'C20     ') OP17=130.0D0
          IF(OPNM.EQ.'C21     ') OP17=131.0D0
          IF(OPNM.EQ.'C22     ') OP17=132.0D0
          IF(OPNM.EQ.'C23     ') OP17=133.0D0
          IF(OPNM.EQ.'C24     ') OP17=134.0D0
          IF(OPNM.EQ.'C25     ') OP17=135.0D0
          IF(OPNM.EQ.'C26     ') OP17=136.0D0
          IF(OPNM.EQ.'C27     ') OP17=137.0D0
          IF(OPNM.EQ.'C28     ') OP17=138.0D0
          IF(OPNM.EQ.'C29     ') OP17=139.0D0
          IF(OPNM.EQ.'C30     ') OP17=140.0D0
          IF(OPNM.EQ.'C31     ') OP17=141.0D0
          IF(OPNM.EQ.'C32     ') OP17=142.0D0
          IF(OPNM.EQ.'C33     ') OP17=143.0D0
          IF(OPNM.EQ.'C34     ') OP17=144.0D0
          IF(OPNM.EQ.'C35     ') OP17=145.0D0
          IF(OPNM.EQ.'C36     ') OP17=146.0D0
          IF(OPNM.EQ.'C37     ') OP17=147.0D0
          IF(OPNM.EQ.'C38     ') OP17=148.0D0
          IF(OPNM.EQ.'C39     ') OP17=149.0D0
          IF(OPNM.EQ.'C40     ') OP17=150.0D0
          IF(OPNM.EQ.'C41     ') OP17=151.0D0
          IF(OPNM.EQ.'C42     ') OP17=152.0D0
          IF(OPNM.EQ.'C43     ') OP17=153.0D0
          IF(OPNM.EQ.'C44     ') OP17=154.0D0
          IF(OPNM.EQ.'C45     ') OP17=155.0D0
          IF(OPNM.EQ.'C46     ') OP17=156.0D0
          IF(OPNM.EQ.'C47     ') OP17=157.0D0
          IF(OPNM.EQ.'C48     ') OP17=158.0D0
          IF(OPNM.EQ.'C49     ') OP17=159.0D0
          IF(OPNM.EQ.'C50     ') OP17=160.0D0
          IF(OPNM.EQ.'C51     ') OP17=161.0D0
          IF(OPNM.EQ.'C52     ') OP17=162.0D0
          IF(OPNM.EQ.'C53     ') OP17=163.0D0
          IF(OPNM.EQ.'C54     ') OP17=164.0D0
          IF(OPNM.EQ.'C55     ') OP17=165.0D0
          IF(OPNM.EQ.'C56     ') OP17=166.0D0
          IF(OPNM.EQ.'C57     ') OP17=167.0D0
          IF(OPNM.EQ.'C58     ') OP17=168.0D0
          IF(OPNM.EQ.'C59     ') OP17=169.0D0
          IF(OPNM.EQ.'C60     ') OP17=170.0D0
          IF(OPNM.EQ.'C61     ') OP17=171.0D0
          IF(OPNM.EQ.'C62     ') OP17=172.0D0
          IF(OPNM.EQ.'C63     ') OP17=173.0D0
          IF(OPNM.EQ.'C64     ') OP17=174.0D0
          IF(OPNM.EQ.'C65     ') OP17=175.0D0
          IF(OPNM.EQ.'C66     ') OP17=176.0D0
          IF(OPNM.EQ.'C67     ') OP17=177.0D0
          IF(OPNM.EQ.'C68     ') OP17=178.0D0
          IF(OPNM.EQ.'C69     ') OP17=179.0D0
          IF(OPNM.EQ.'C70     ') OP17=180.0D0
          IF(OPNM.EQ.'C71     ') OP17=181.0D0
          IF(OPNM.EQ.'C72     ') OP17=182.0D0
          IF(OPNM.EQ.'C73     ') OP17=183.0D0
          IF(OPNM.EQ.'C74     ') OP17=184.0D0
          IF(OPNM.EQ.'C75     ') OP17=185.0D0
          IF(OPNM.EQ.'C76     ') OP17=186.0D0
          IF(OPNM.EQ.'C77     ') OP17=187.0D0
          IF(OPNM.EQ.'C78     ') OP17=188.0D0
          IF(OPNM.EQ.'C79     ') OP17=189.0D0
          IF(OPNM.EQ.'C80     ') OP17=190.0D0
          IF(OPNM.EQ.'C81     ') OP17=191.0D0
          IF(OPNM.EQ.'C82     ') OP17=192.0D0
          IF(OPNM.EQ.'C83     ') OP17=193.0D0
          IF(OPNM.EQ.'C84     ') OP17=194.0D0
          IF(OPNM.EQ.'C85     ') OP17=195.0D0
          IF(OPNM.EQ.'C86     ') OP17=196.0D0
          IF(OPNM.EQ.'C87     ') OP17=197.0D0
          IF(OPNM.EQ.'C88     ') OP17=198.0D0
          IF(OPNM.EQ.'C89     ') OP17=199.0D0
          IF(OPNM.EQ.'C90     ') OP17=200.0D0
          IF(OPNM.EQ.'C91     ') OP17=201.0D0
          IF(OPNM.EQ.'C92     ') OP17=202.0D0
          IF(OPNM.EQ.'C93     ') OP17=203.0D0
          IF(OPNM.EQ.'C94     ') OP17=204.0D0
          IF(OPNM.EQ.'C95     ') OP17=205.0D0
          IF(OPNM.EQ.'C96     ') OP17=206.0D0
          IF(OPNM.EQ.'PWRY    ') OP17=207.0D0
          IF(OPNM.EQ.'PWRX    ') OP17=208.0D0
          IF(OPNM.EQ.'FLCLTHX ') OP17=209.0D0
          IF(OPNM.EQ.'FLCLTH  ') OP17=210.0D0
          IF(OPNM.EQ.'FLCLTHY ') OP17=210.0D0
          IF(OPNM.EQ.'PY      ') OP17=211.0D0
          IF(OPNM.EQ.'PX      ') OP17=212.0D0
          IF(OPNM.EQ.'PCY     ') OP17=213.0D0
          IF(OPNM.EQ.'PCX     ') OP17=214.0D0
          IF(OPNM.EQ.'PUY     ') OP17=215.0D0
          IF(OPNM.EQ.'PUX     ') OP17=216.0D0
          IF(OPNM.EQ.'PUCY    ') OP17=217.0D0
          IF(OPNM.EQ.'PUCX    ') OP17=218.0D0
          IF(OPNM.EQ.'PIY     ') OP17=219.0D0
          IF(OPNM.EQ.'PIX     ') OP17=220.0D0
          IF(OPNM.EQ.'PICY    ') OP17=221.0D0
          IF(OPNM.EQ.'PICX    ') OP17=222.0D0
          IF(OPNM.EQ.'PIYP    ') OP17=223.0D0
          IF(OPNM.EQ.'PIXP    ') OP17=224.0D0
          IF(OPNM.EQ.'PICYP   ') OP17=225.0D0
          IF(OPNM.EQ.'PICXP   ') OP17=226.0D0
          IF(OPNM.EQ.'PACY    ') OP17=227.0D0
          IF(OPNM.EQ.'PACX    ') OP17=228.0D0
          IF(OPNM.EQ.'PLCY    ') OP17=229.0D0
          IF(OPNM.EQ.'PLCX    ') OP17=230.0D0
          IF(OPNM.EQ.'SACY    ') OP17=231.0D0
          IF(OPNM.EQ.'SACX    ') OP17=232.0D0
          IF(OPNM.EQ.'SLCY    ') OP17=233.0D0
          IF(OPNM.EQ.'SLCX    ') OP17=234.0D0
          IF(OPNM.EQ.'IMDISX  ') OP17=235.0D0
          IF(OPNM.EQ.'IMDISY  ') OP17=236.0D0
          IF(OPNM.EQ.'CENTX   ') OP17=237.0D0
          IF(OPNM.EQ.'CENTY   ') OP17=238.0D0
          IF(OPNM.EQ.'RMSX    ') OP17=239.0D0
          IF(OPNM.EQ.'RMSY    ') OP17=240.0D0
          IF(OPNM.EQ.'RMS     ') OP17=241.0D0
          IF(OPNM.EQ.'RSSX    ') OP17=242.0D0
          IF(OPNM.EQ.'RSSY    ') OP17=243.0D0
          IF(OPNM.EQ.'RSS     ') OP17=244.0D0
          IF(OPNM.EQ.'RMSOPD  ') OP17=245.0D0
          IF(OPNM.EQ.'ZERN37  ') OP17=246.0D0
          IF(OPNM.EQ.'MAGX    ') OP17=247.0D0
          IF(OPNM.EQ.'MAGY    ') OP17=248.0D0
          IF(OPNM.EQ.'MAGXOR  ') OP17=249.0D0
          IF(OPNM.EQ.'MAGYOR  ') OP17=250.0D0
          IF(OPNM.EQ.'FFLX    ') OP17=251.0D0
          IF(OPNM.EQ.'FFLY    ') OP17=252.0D0
          IF(OPNM.EQ.'BFLX    ') OP17=253.0D0
          IF(OPNM.EQ.'BFLY    ') OP17=254.0D0
          IF(OPNM.EQ.'FFNX    ') OP17=255.0D0
          IF(OPNM.EQ.'FFNY    ') OP17=256.0D0
          IF(OPNM.EQ.'BFNX    ') OP17=257.0D0
          IF(OPNM.EQ.'BFNY    ') OP17=258.0D0
          IF(OPNM.EQ.'EFLX    ') OP17=259.0D0
          IF(OPNM.EQ.'EFLY    ') OP17=260.0D0
          IF(OPNM.EQ.'ENDIAX  ') OP17=261.0D0
          IF(OPNM.EQ.'ENDIAY  ') OP17=262.0D0
          IF(OPNM.EQ.'EXDIAX  ') OP17=263.0D0
          IF(OPNM.EQ.'EXDIAY  ') OP17=264.0D0
          IF(OPNM.EQ.'ENPOSX  ') OP17=265.0D0
          IF(OPNM.EQ.'ENPOSY  ') OP17=266.0D0
          IF(OPNM.EQ.'ENPOSZ  ') OP17=267.0D0
          IF(OPNM.EQ.'EXPOSX  ') OP17=268.0D0
          IF(OPNM.EQ.'EXPOSY  ') OP17=269.0D0
          IF(OPNM.EQ.'EXPOSZ  ') OP17=270.0D0
          IF(OPNM.EQ.'FNUMX   ') OP17=271.0D0
          IF(OPNM.EQ.'FNYMY   ') OP17=272.0D0
          IF(OPNM.EQ.'OBFNUMX ') OP17=273.0D0
          IF(OPNM.EQ.'OBFNUMY ') OP17=274.0D0
          IF(OPNM.EQ.'ENPDIAX ') OP17=275.0D0
          IF(OPNM.EQ.'ENPDIAY ') OP17=276.0D0
          IF(OPNM.EQ.'EXPDIAX ') OP17=277.0D0
          IF(OPNM.EQ.'EXPDIAY ') OP17=278.0D0
          IF(OPNM.EQ.'PUPDIAX ') OP17=279.0D0
          IF(OPNM.EQ.'PUPDIAY ') OP17=280.0D0
          IF(OPNM.EQ.'PUPDISX ') OP17=281.0D0
          IF(OPNM.EQ.'PUPDISY ') OP17=282.0D0
          IF(OPNM.EQ.'CHFIMX  ') OP17=283.0D0
          IF(OPNM.EQ.'CHFIMY  ') OP17=284.0D0
          IF(OPNM.EQ.'GPX     ') OP17=285.0D0
          IF(OPNM.EQ.'GPY     ') OP17=286.0D0
          IF(OPNM.EQ.'GPUX    ') OP17=287.0D0
          IF(OPNM.EQ.'GPUY    ') OP17=288.0D0
          IF(OPNM.EQ.'GPCX    ') OP17=289.0D0
          IF(OPNM.EQ.'GPCY    ') OP17=290.0D0
          IF(OPNM.EQ.'GPUCX   ') OP17=291.0D0
          IF(OPNM.EQ.'GPUCY   ') OP17=292.0D0
          IF(OPNM.EQ.'DIST    ') OP17=293.0D0
          IF(OPNM.EQ.'XFOC    ') OP17=294.0D0
          IF(OPNM.EQ.'YFOC    ') OP17=295.0D0
          IF(OPNM.EQ.'AST     ') OP17=296.0D0
          IF(OPNM.EQ.'SA3     ') OP17=297.0D0
          IF(OPNM.EQ.'XSA3    ') OP17=298.0D0
          IF(OPNM.EQ.'CMA3    ') OP17=299.0D0
          IF(OPNM.EQ.'XCMA3   ') OP17=300.0D0
          IF(OPNM.EQ.'AST3    ') OP17=301.0D0
          IF(OPNM.EQ.'XAST3   ') OP17=302.0D0
          IF(OPNM.EQ.'DIS3    ') OP17=303.0D0
          IF(OPNM.EQ.'XDIS3   ') OP17=304.0D0
          IF(OPNM.EQ.'PTZ3    ') OP17=305.0D0
          IF(OPNM.EQ.'XPTZ3   ') OP17=306.0D0
          IF(OPNM.EQ.'SA5     ') OP17=307.0D0
          IF(OPNM.EQ.'XSA5    ') OP17=308.0D0
          IF(OPNM.EQ.'CMA5    ') OP17=309.0D0
          IF(OPNM.EQ.'XCMA5   ') OP17=310.0D0
          IF(OPNM.EQ.'AST5    ') OP17=311.0D0
          IF(OPNM.EQ.'XAST5   ') OP17=312.0D0
          IF(OPNM.EQ.'DIS5    ') OP17=313.0D0
          IF(OPNM.EQ.'XDIS5   ') OP17=314.0D0
          IF(OPNM.EQ.'PTZ5    ') OP17=315.0D0
          IF(OPNM.EQ.'XPTZ5   ') OP17=316.0D0
          IF(OPNM.EQ.'TOBSA   ') OP17=317.0D0
          IF(OPNM.EQ.'XTOBSA  ') OP17=318.0D0
          IF(OPNM.EQ.'SOBSA   ') OP17=319.0D0
          IF(OPNM.EQ.'XSOBSA  ') OP17=320.0D0
          IF(OPNM.EQ.'ELCMA   ') OP17=321.0D0
          IF(OPNM.EQ.'XELCMA  ') OP17=322.0D0
          IF(OPNM.EQ.'TAS     ') OP17=323.0D0
          IF(OPNM.EQ.'XTAS    ') OP17=324.0D0
          IF(OPNM.EQ.'SAS     ') OP17=325.0D0
          IF(OPNM.EQ.'XSAS    ') OP17=326.0D0
          IF(OPNM.EQ.'SA7     ') OP17=327.0D0
          IF(OPNM.EQ.'XSA7    ') OP17=328.0D0
          IF(OPNM.EQ.'SA3P    ') OP17=329.0D0
          IF(OPNM.EQ.'XSA3P   ') OP17=330.0D0
          IF(OPNM.EQ.'CMA3P   ') OP17=331.0D0
          IF(OPNM.EQ.'XCMA3P  ') OP17=332.0D0
          IF(OPNM.EQ.'AST3P   ') OP17=333.0D0
          IF(OPNM.EQ.'XAST3P  ') OP17=334.0D0
          IF(OPNM.EQ.'DIS3P   ') OP17=335.0D0
          IF(OPNM.EQ.'XDIS3P  ') OP17=336.0D0
          IF(OPNM.EQ.'PTZ3P   ') OP17=337.0D0
          IF(OPNM.EQ.'XPTZ3P  ') OP17=338.0D0
          IF(OPNM.EQ.'SA5P    ') OP17=339.0D0
          IF(OPNM.EQ.'XSA5P   ') OP17=340.0D0
          IF(OPNM.EQ.'CMA5P   ') OP17=341.0D0
          IF(OPNM.EQ.'XCMA5P  ') OP17=342.0D0
          IF(OPNM.EQ.'AST5P   ') OP17=343.0D0
          IF(OPNM.EQ.'XAST5P  ') OP17=344.0D0
          IF(OPNM.EQ.'DIS5P   ') OP17=345.0D0
          IF(OPNM.EQ.'XDIS5P  ') OP17=346.0D0
          IF(OPNM.EQ.'PTZ5P   ') OP17=347.0D0
          IF(OPNM.EQ.'XPTZ5P  ') OP17=348.0D0
          IF(OPNM.EQ.'TOBSAP  ') OP17=349.0D0
          IF(OPNM.EQ.'XTOBSAP ') OP17=350.0D0
          IF(OPNM.EQ.'SOBSAP  ') OP17=351.0D0
          IF(OPNM.EQ.'XSOBSAP ') OP17=352.0D0
          IF(OPNM.EQ.'ELCMAP  ') OP17=353.0D0
          IF(OPNM.EQ.'XELCMAP ') OP17=354.0D0
          IF(OPNM.EQ.'TASP    ') OP17=355.0D0
          IF(OPNM.EQ.'XTASP   ') OP17=356.0D0
          IF(OPNM.EQ.'SASP    ') OP17=357.0D0
          IF(OPNM.EQ.'XSASP   ') OP17=358.0D0
          IF(OPNM.EQ.'SA7P    ') OP17=359.0D0
          IF(OPNM.EQ.'XSA7P   ') OP17=360.0D0
          IF(OPNM.EQ.'SA3S    ') OP17=361.0D0
          IF(OPNM.EQ.'XSA3S   ') OP17=362.0D0
          IF(OPNM.EQ.'CMA3S   ') OP17=363.0D0
          IF(OPNM.EQ.'XCMA3S  ') OP17=364.0D0
          IF(OPNM.EQ.'AST3S   ') OP17=365.0D0
          IF(OPNM.EQ.'XAST3S  ') OP17=366.0D0
          IF(OPNM.EQ.'DIS3S   ') OP17=367.0D0
          IF(OPNM.EQ.'XDIS3S  ') OP17=368.0D0
          IF(OPNM.EQ.'PTZ3S   ') OP17=369.0D0
          IF(OPNM.EQ.'XPTZ3S  ') OP17=370.0D0
          IF(OPNM.EQ.'SA5S    ') OP17=371.0D0
          IF(OPNM.EQ.'XSA5S   ') OP17=372.0D0
          IF(OPNM.EQ.'CMA5S   ') OP17=373.0D0
          IF(OPNM.EQ.'XCMA5S  ') OP17=374.0D0
          IF(OPNM.EQ.'AST5S   ') OP17=375.0D0
          IF(OPNM.EQ.'XAST5S  ') OP17=376.0D0
          IF(OPNM.EQ.'DIS5S   ') OP17=377.0D0
          IF(OPNM.EQ.'XDIS5S  ') OP17=378.0D0
          IF(OPNM.EQ.'PTZ5S   ') OP17=379.0D0
          IF(OPNM.EQ.'XPTZ5S  ') OP17=380.0D0
          IF(OPNM.EQ.'TOBSAS  ') OP17=381.0D0
          IF(OPNM.EQ.'XTOBSAS ') OP17=382.0D0
          IF(OPNM.EQ.'SOBSAS  ') OP17=383.0D0
          IF(OPNM.EQ.'XSOBSAS ') OP17=384.0D0
          IF(OPNM.EQ.'ELCMAS  ') OP17=385.0D0
          IF(OPNM.EQ.'XELCMAS ') OP17=386.0D0
          IF(OPNM.EQ.'TASS    ') OP17=387.0D0
          IF(OPNM.EQ.'XTASS   ') OP17=388.0D0
          IF(OPNM.EQ.'SASS    ') OP17=389.0D0
          IF(OPNM.EQ.'XSASS   ') OP17=390.0D0
          IF(OPNM.EQ.'SA7S    ') OP17=391.0D0
          IF(OPNM.EQ.'XSA7S   ') OP17=392.0D0
          IF(OPNM.EQ.'SA5I    ') OP17=393.0D0
          IF(OPNM.EQ.'XSA5I   ') OP17=394.0D0
          IF(OPNM.EQ.'CMA5I   ') OP17=395.0D0
          IF(OPNM.EQ.'XCMA5I  ') OP17=396.0D0
          IF(OPNM.EQ.'AST5I   ') OP17=397.0D0
          IF(OPNM.EQ.'XAST5I  ') OP17=398.0D0
          IF(OPNM.EQ.'DIS5I   ') OP17=399.0D0
          IF(OPNM.EQ.'XDIS5I  ') OP17=400.0D0
          IF(OPNM.EQ.'PTZ5I   ') OP17=401.0D0
          IF(OPNM.EQ.'XPTZ5I  ') OP17=402.0D0
          IF(OPNM.EQ.'TOBSAI  ') OP17=403.0D0
          IF(OPNM.EQ.'XTOBSAI ') OP17=404.0D0
          IF(OPNM.EQ.'SOBSAI  ') OP17=405.0D0
          IF(OPNM.EQ.'XSOBSAI ') OP17=406.0D0
          IF(OPNM.EQ.'ELCMAI  ') OP17=407.0D0
          IF(OPNM.EQ.'XELCMAI ') OP17=408.0D0
          IF(OPNM.EQ.'TASI    ') OP17=409.0D0
          IF(OPNM.EQ.'XTASI   ') OP17=410.0D0
          IF(OPNM.EQ.'SASI    ') OP17=411.0D0
          IF(OPNM.EQ.'XSASI   ') OP17=412.0D0
          IF(OPNM.EQ.'SA7I    ') OP17=413.0D0
          IF(OPNM.EQ.'XSA7I   ') OP17=414.0D0
          IF(OPNM.EQ.'PSA3    ') OP17=415.0D0
          IF(OPNM.EQ.'XPSA3   ') OP17=416.0D0
          IF(OPNM.EQ.'PCMA3   ') OP17=417.0D0
          IF(OPNM.EQ.'XPCMA3  ') OP17=418.0D0
          IF(OPNM.EQ.'PAST3   ') OP17=419.0D0
          IF(OPNM.EQ.'XPAST3  ') OP17=420.0D0
          IF(OPNM.EQ.'PDIS3   ') OP17=421.0D0
          IF(OPNM.EQ.'XPDIS3  ') OP17=422.0D0
          IF(OPNM.EQ.'PPTZ3   ') OP17=423.0D0
          IF(OPNM.EQ.'XPPTZ3  ') OP17=424.0D0
          IF(OPNM.EQ.'PSA3P   ') OP17=425.0D0
          IF(OPNM.EQ.'XPSA3P  ') OP17=426.0D0
          IF(OPNM.EQ.'PCMA3P  ') OP17=427.0D0
          IF(OPNM.EQ.'XPCMA3P ') OP17=428.0D0
          IF(OPNM.EQ.'PAST3P  ') OP17=429.0D0
          IF(OPNM.EQ.'XPAST3P ') OP17=430.0D0
          IF(OPNM.EQ.'PDIS3P  ') OP17=431.0D0
          IF(OPNM.EQ.'XPDIS3P ') OP17=432.0D0
          IF(OPNM.EQ.'PPTZ3P  ') OP17=433.0D0
          IF(OPNM.EQ.'XPPTZ3P ') OP17=434.0D0
          IF(OPNM.EQ.'PSA3S   ') OP17=435.0D0
          IF(OPNM.EQ.'XPSA3S  ') OP17=436.0D0
          IF(OPNM.EQ.'PCMA3S  ') OP17=437.0D0
          IF(OPNM.EQ.'XPCMA3S ') OP17=438.0D0
          IF(OPNM.EQ.'PAST3S  ') OP17=439.0D0
          IF(OPNM.EQ.'XPAST3S ') OP17=430.0D0
          IF(OPNM.EQ.'PDIS3S  ') OP17=431.0D0
          IF(OPNM.EQ.'XPDIS3S ') OP17=442.0D0
          IF(OPNM.EQ.'PPTZ3S  ') OP17=443.0D0
          IF(OPNM.EQ.'XPPTZ3S ') OP17=444.0D0
          IF(OPNM.EQ.'PTZCV   ') OP17=445.0D0
          IF(OPNM.EQ.'XPTZCV  ') OP17=446.0D0
          IF(OPNM.EQ.'AH      ') OP17=447.0D0
          IF(OPNM.EQ.'AI      ') OP17=448.0D0
          IF(OPNM.EQ.'AJ      ') OP17=449.0D0
          IF(OPNM.EQ.'AK      ') OP17=450.0D0
          IF(OPNM.EQ.'AL      ') OP17=451.0D0
          IF(OPNM.EQ.'GBRADX  ') OP17=452.0D0
          IF(OPNM.EQ.'GBRADY  ') OP17=453.0D0
          IF(OPNM.EQ.'GBDISX  ') OP17=454.0D0
          IF(OPNM.EQ.'GBDISY  ') OP17=455.0D0
          IF(OPNM.EQ.'GBRCVX  ') OP17=456.0D0
          IF(OPNM.EQ.'GBRCVY  ') OP17=457.0D0
          IF(OPNM.EQ.'GBWAISTX') OP17=458.0D0
          IF(OPNM.EQ.'GBWAISTY') OP17=459.0D0
          IF(OPNM.EQ.'MGOTF')    OP17=460.0D0
          IF(OPNM.EQ.'PGOTF')    OP17=461.0D0
          IF(OPNM.EQ.'MDOTF')    OP17=462.0D0
          IF(OPNM.EQ.'PDOTF')    OP17=463.0D0
          IF(OPNM.EQ.'GOTFM')    OP17=460.0D0
          IF(OPNM.EQ.'GOTFP')    OP17=461.0D0
          IF(OPNM.EQ.'DOTFM')    OP17=462.0D0
          IF(OPNM.EQ.'DOTFP')    OP17=463.0D0
          IF(OPNM.EQ.'RED')      OP17=464.0D0
          IF(OPNM.EQ.'REDCEN')   OP17=465.0D0
          IF(OPNM.EQ.'FISHDIST') OP17=466.0D0
          IF(OPNM.EQ.'ZD')       OP17=467.0D0
          IF(OPNM.EQ.'SYMX')     OP17=468.0D0
          IF(OPNM.EQ.'SYMY')     OP17=469.0D0
          IF(OPNM.EQ.'ASYMX')    OP17=470.0D0
          IF(OPNM.EQ.'ASYMY')    OP17=471.0D0
          IF(OPNM.EQ.'PACM')     OP17=472.0D0
          IF(OPNM.EQ.'PACZ')     OP17=473.0D0
          IF(OPNM.EQ.'SACM')     OP17=474.0D0
          IF(OPNM.EQ.'SACZ')     OP17=475.0D0
          IF(OPNM.EQ.'PLCM')     OP17=476.0D0
          IF(OPNM.EQ.'PLCZ')     OP17=477.0D0
          IF(OPNM.EQ.'SLCM')     OP17=478.0D0
          IF(OPNM.EQ.'SLCZ')     OP17=479.0D0
          IF(OPNM.EQ.'CTSX')     OP17=480.0D0
          IF(OPNM.EQ.'CTSY')     OP17=481.0D0
          IF(OPNM.EQ.'SCEX')     OP17=482.0D0
          IF(OPNM.EQ.'SCEY')     OP17=483.0D0
          IF(OPNM.EQ.'GREYS')    OP17=484.0D0
          IF(OPNM.EQ.'PIVX')     OP17=485.0D0
          IF(OPNM.EQ.'PIVY')     OP17=486.0D0
          IF(OPNM.EQ.'PIVZ')     OP17=487.0D0
          IF(OPNM.EQ.'N1')       OP17=488.0D0
          IF(OPNM.EQ.'N2')       OP17=489.0D0
          IF(OPNM.EQ.'N3')       OP17=490.0D0
          IF(OPNM.EQ.'N4')       OP17=491.0D0
          IF(OPNM.EQ.'N5')       OP17=492.0D0
          IF(OPNM.EQ.'N6')       OP17=493.0D0
          IF(OPNM.EQ.'N7')       OP17=494.0D0
          IF(OPNM.EQ.'N8')       OP17=495.0D0
          IF(OPNM.EQ.'N9')       OP17=496.0D0
          IF(OPNM.EQ.'N10')      OP17=497.0D0
          IF(OPNM.EQ.'ABBE')     OP17=498.0D0
          IF(OPNM.EQ.'DPART')    OP17=499.0D0
          IF(OPNM.EQ.'CLPX')     OP17=500.0D0
          IF(OPNM.EQ.'CLPY')     OP17=501.0D0
          IF(OPNM.EQ.'GDX')      OP17=502.0D0
          IF(OPNM.EQ.'GDY')      OP17=503.0D0
          IF(OPNM.EQ.'GDZ')      OP17=504.0D0
          IF(OPNM.EQ.'GALPHA')   OP17=505.0D0
          IF(OPNM.EQ.'GBETA')    OP17=506.0D0
          IF(OPNM.EQ.'GGAMMA')   OP17=507.0D0
          IF(OPNM.EQ.'GRS')      OP17=508.0D0
          IF(OPNM.EQ.'WEIGHT')   OP17=509.0D0
          IF(OPNM.EQ.'DMINUSD')  OP17=510.0D0
          IF(OPNM.EQ.'COST')     OP17=511.0D0
          IF(OPNM.EQ.'MACOPT')   OP17=512.0D0
          IF(OPNM.EQ.'RMSYX')    OP17=513.0D0
C
          IF(OP17.EQ.512.0D0) THEN
C     MACOPT OPERAND
              IF(INT(W3).LT.1.OR.INT(W3).GT.1000) THEN
                  WRITE(OUTLYNE,*)
     1            'PREDEFINED OPERAND ',OPNM,' REQUIRES'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)
     1            '"NUMERIC WORD #2 IN THE RANGE 1 TO 1000'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IREG=W3
              OP8 =IREG
              OP18 =DBLE(DF3)
              OP9=W4
              OP11=DBLE(DF4)
              OP10 =W5
              OP12 =DBLE(DF5)
          END IF
          IF(OP17.GE.480.0D0.AND.OP17.LE.481.0D0) THEN
C     CTSX AND CTSY OPERANDS
              IF(DF5.EQ.0.AND.W5.NE.0.0D0) THEN
                  WRITE(OUTLYNE,*)
     1            'PREDEFINED OPERAND ',OPNM
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)
     1            'TAKES NO NUMERIC WORD #4 INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IREG=W3
              OP8 =IREG
              OP18 =DBLE(DF3)
              OP9=W4
              OP11=DBLE(DF4)
              OP10 =W5
              OP12 =DBLE(DF5)
          END IF
C
          IF(OP17.GE.482.0D0.AND.OP17.LE.483.0D0) THEN
C     SCEX AND SCEY OPERANDS
              IF(DF4.EQ.0.AND.W4.NE.0.0D0.OR.DF5.EQ.0.AND.W5.NE.0.0D0) THEN
                  WRITE(OUTLYNE,*)
     1            'PREDEFINED OPERAND ',OPNM
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)
     1            'TAKES NO NUMERIC WORD #3 OR #4 INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IREG=W3
              OP8 =IREG
              OP18 =DBLE(DF3)
              OP9=W4
              OP11=DBLE(DF4)
              OP10 =W5
              OP12 =DBLE(DF5)
          END IF
C
C
          IF(OP17.GE.472.0D0.AND.OP17.LE.479.0D0.OR.OP17.EQ.479.0D0
     1    .OR.OP17.EQ.510.0D0) THEN
C     REAL RAY COLOR OPERANDS, NO INPUT NEEDED TO DEFINE
C     FRACTIONAL OBJECT POS., REL RAY POS OR COLORS.
              IF(DF3.EQ.0.OR.DF4.EQ.0.OR.DF5.EQ.0) THEN
                  WRITE(OUTLYNE,*)
     1            'PREDEFINED OPERAND ',OPNM
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)
     1            'ONLY TAKES NUMERIC WORD #1 AND #5(OPTIONAL) INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IREG=W3
              OP8 =IREG
              OP18 =DBLE(DF3)
              OP9=W4
              OP11=DBLE(DF4)
              OP10 =W5
              OP12 =DBLE(DF5)
          END IF
C     SYMX,SYMY,ASYMX AND ASYMY
          IF(OP17.GE.468.0D0.AND.OP17.LE.471.0D0) THEN
C     W3 (CALLED W2) ANY VALUE MAY BE INPUT ( ONLY ABS VALUE USED)
              IF(DF3.EQ.1) THEN
                  DF3=0
                  W3=0.7D0
              END IF
              W3=DABS(W3)
C     W4 (CALLED W3) 1 TO 200 (FIELD NUMBER)
              IF(DF4.EQ.1) THEN
                  DF4=0
                  W4=1.0D0
              END IF
C     W5 (CALLED W4) 1 TO 10 (WAVELENGTH NUMBER)
              IF(DF5.EQ.1) THEN
                  DF5=0
                  W5=SYSTEM1(11)
              END IF
C     OUT OF RANGE INPUT W4
              IF(INT(W4).LT.1.OR.INT(W4).GT.200.0D0) THEN
                  WRITE(OUTLYNE,*)
     1            'FIELD POSITION NUMBER BEYOND LEGAL RANGE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)
     1            'FOR PREDEFINED OPERAND ',OPNM
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C     OUT OF RANGE INPUT W5
              IF(INT(W5).LT.1.OR.INT(W5).GT.10.0D0) THEN
                  WRITE(OUTLYNE,*)
     1            'WAVELENGTH NUMBER BEYOND LEGAL RANGE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)
     1            'FOR PREDEFINED OPERAND ',OPNM
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IREG=W3
              OP8 =IREG
              OP18 =DBLE(DF3)
              OP9=DBLE(INT(W4))
              OP11=DBLE(DF4)
              OP10 =DBLE(INT(W5))
              OP12 =DBLE(DF5)
          END IF
C
          IF(OP17.GE.1.0D0.AND.OP17.LE.15.0D0.OR.OP17.GE.18.0D0.AND.OP17
     1    .LE.69.0D0) THEN
C     RAY BASED PREDEFINED OPERANDS
C
C
              IF(OP17.EQ.68.0D0.OR.OP17.EQ.69.0D0) THEN
                  IF(W3.LT.1.0D0) THEN
                      WRITE(OUTLYNE,*)
     1                'PREDEFINED OPERAND ',OPNM
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'REQUIRES NUMERIC WORD #2 INPUT GREATER THAN 0'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
C
C     DEFAULT NW3
              IF(OP17.GE.1.0D0.AND.OP17.LE.14.0D0.OR.
     1        OP17.GE.18.0D0.AND.OP17.LE.20.0D0.OR.
     2        OP17.GE.22.0D0.AND.OP17.LE.23.0D0.OR.
     3        OP17.GE.27.0D0.AND.OP17.LE.55.0D0) THEN
                  IF(DF3.EQ.1) THEN
                      W3=DBLE(NEWIMG)
                      DF3=0
                      IREG=W3
                      OP8 =IREG
                      OP18 =DBLE(DF3)
                  END IF
              END IF
C
              IF(OP17.EQ.15.0D0.OR.
     1        OP17.EQ.21D0.OR.
     2        OP17.GE.24.0D0.AND.OP17.LE.26.0D0.OR.
     3        OP17.GE.56.0D0.AND.OP17.LE.69.0D0) THEN
                  IF(DF3.EQ.1) THEN
                      WRITE(OUTLYNE,*)
     1                'PREDEFINED OPERAND ',OPNM
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'REQUIRES EXPLICIT NUMERIC WORD #2 INPUT'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
C
              IF(OP17.GE.43.AND.OP17.LE.58.0D0.OR.OP17.GE.68.0D0.AND.
     1        OP17.LE.69.0D0) THEN
C     REFERENCE RAYS
C     DEFAULT INPUT W4
C
                  IF(DF4.EQ.1) THEN
                      WRITE(OUTLYNE,*)
     1                'PREDEFINED OPERAND ',OPNM
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'REQUIRES EXPLICIT NUMERIC WORD #4 INPUT'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  IF(DF5.EQ.0.AND.W5.NE.0.0D0) THEN
                      WRITE(OUTLYNE,*)
     1                'PREDEFINED OPERAND ',OPNM
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'TAKES NO NUMERIC WORD #5 INPUT'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  DF5=0
                  S5=1
                  W5=0

C     OUT OF RANGE INPUT W3
                  IF(INT(W3).LT.NEWOBJ.OR.INT(W3).GT.NEWIMG) THEN
                      WRITE(OUTLYNE,*)
     1                'FOR PREDEFINED OPERAND ',OPNM
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'SURFACE NUMBER BEYOND LEGAL RANGE'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
C     OUT OF RANGE INPUT W4
                  IF(INT(W4).LT.1.OR.INT(W4).GT.200.0D0) THEN
                      WRITE(OUTLYNE,*)
     1                'FIELD POSITION NUMBER BEYOND LEGAL RANGE'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'FOR PREDEFINED OPERAND ',OPNM
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              ELSE
C     NOT REFERENCE RAYS
C     DEFAULT INPUT W4 AND W5
C
                  IF(DF4.EQ.1.OR.DF5.EQ.1) THEN
                      WRITE(OUTLYNE,*)
     1                'PREDEFINED OPERAND ',OPNM
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'REQUIRES EXPLICIT NUMERIC WORD #4 AND #5 INPUT'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF

C     OUT OF RANGE INPUT W3
                  IF(INT(W3).LT.NEWOBJ.OR.INT(W3).GT.NEWIMG) THEN
                      WRITE(OUTLYNE,*)
     1                'FOR PREDEFINED OPERAND ',OPNM
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'SURFACE NUMBER BEYOND LEGAL RANGE'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
C     OUT OF RANGE INPUT W4
                  IF(INT(W4).LT.1.OR.INT(W4).GT.200.0D0) THEN
                      WRITE(OUTLYNE,*)
     1                'FIELD POSITION NUMBER BEYOND LEGAL RANGE'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'FOR PREDEFINED OPERAND ',OPNM
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
C     OUT OF RANGE INPUT W5
                  IF(INT(W5).LT.1.OR.INT(W5).GT.500) THEN
                      WRITE(OUTLYNE,*)
     1                'RAY POSITION NUMBER BEYOND LEGAL RANGE'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'FOR PREDEFINED OPERAND ',OPNM
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF

          END IF
C     *****************************************************************
          IF(OP17.EQ.16.0D0.OR.OP17.EQ.17.0D0) THEN
C     OPD OR OPDW
C
              IF(DF3.EQ.1.OR.DF4.EQ.1) THEN
                  WRITE(OUTLYNE,*)
     1            'PREDEFINED OPERAND ',OPNM
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)
     1            'REQUIRES EXPLICIT NUMERIC WORD #2 AND #3 INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C     OUT OF RANGE INPUT W3
              IF(INT(W3).LT.1.OR.INT(W3).GT.200.0D0) THEN
                  WRITE(OUTLYNE,*)
     1            'FIELD POSITION NUMBER BEYOND LEGAL RANGE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)
     1            'FOR PREDEFINED OPERAND ',OPNM
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C     OUT OF RANGE INPUT W4
              IF(INT(W4).LT.1.OR.INT(W4).GT.500) THEN
                  WRITE(OUTLYNE,*)
     1            'RAY POSITION NUMBER BEYOND LEGAL RANGE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)
     1            'FOR PREDEFINED OPERAND ',OPNM
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C     W5
              IF(S5.EQ.1.AND.W5.NE.0.0D0) THEN
                  WRITE(OUTLYNE,*)
     1            'PREDEFINED OPERAND ',OPNM
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)
     1            'TAKES NO NUMERIC WORD #4 INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IREG=W3
              OP8 =IREG
              OP18 =DBLE(DF3)
          END IF
C     *****************************************************************
          IF(OP17.GE.70.0D0.AND.OP17.LE.206.0D0.OR.
     1    OP17.GE.447.0D0.AND.OP17.LE.451.0D0.OR.
     2    OP17.EQ.467.0D0.OR.OP17.GE.485.0D0.AND.OP17.LE.511.0D0) THEN
C     LENS DATABASE OPERANDS
              IF(OP17.EQ.15.0D0.OR.OP17.EQ.21.0D0) THEN
                  IF(W3.LT.1.0D0) THEN
                      WRITE(OUTLYNE,*)
     1                'PREDEFINED OPERAND ',OPNM
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'REQUIRES NUMERIC WORD #2 GREATER THAN 0'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
C     DEFAULT INPUT W5
              IF(DF5.EQ.0.AND.W5.NE.0.0D0) THEN
                  WRITE(OUTLYNE,*)
     1            'PREDEFINED OPERAND ',OPNM
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)
     1            'TAKES NO NUMERIC WORD #4 INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(OP17.NE.91.0D0.AND.OP17.NE.106.0D0.AND.
     1        OP17.NE.107.0D0.AND.OP17.NE.509.0D0.AND.OP17.NE.511.0D0) THEN
                  IF(DF5.EQ.0.AND.W5.NE.0.0D0) THEN
                      WRITE(OUTLYNE,*)
     1                'PREDEFINED OPERAND ',OPNM
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'TAKES NO NUMERIC WORD #4 INPUT'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
C     SET DEFAULTS FOR LENGTH AND MLENGTH AND WEIGHT
              IF(OP17.EQ.106.0D0.OR.
     1        OP17.EQ.107.0D0.OR.OP17.EQ.509.0D0.OR.OP17.EQ.511.0D0) THEN
                  IF(W3.LT.0.0D0) W3=SYSTEM1(20)+W3
                  IF(W4.LT.0.0D0) W4=SYSTEM1(20)+W4
                  IF(DF3.EQ.1) W3=0.0D0
                  IF(DF3.EQ.1) DF3=0
                  IF(DF4.EQ.1) W4=SYSTEM1(20)
                  OP9 = W4
                  IF(DF4.EQ.1) DF4=0
                  OP11 =DBLE(DF4)
                  IREG=W3
                  OP8=W3
                  OP18=DBLE(DF3)
                  OP19=0.0D0
              END IF
C     SET DEFAULTS FOR VERTEX DATA
              IF(OP17.GE.94.0D0.AND.OP17.LE.105.0D0) THEN
                  IF(DF3.EQ.1) THEN
                      WRITE(OUTLYNE,*)
     1                'PREDEFINED OPERAND ',OPNM
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'REQUIRES EXPLICIT NUMERIC WORD #2 INPUT'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  IF(DF4.EQ.1) THEN
                      WRITE(OUTLYNE,*)
     1                'PREDEFINED OPERAND ',OPNM
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'REQUIRES EXPLICIT NUMERIC WORD #3 INPUT'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
C     SET SURFACE DATA NUMERIC WORD #2
              IF(OP17.GE.70.0D0.AND.
     1        OP17.LE.89.0D0.OR.OP17.GE.447.0D0.AND.
     2        OP17.LE.451.0D0.OR.OP17.GE.92.0D0.AND.
     3        OP17.LE.93.0D0.OR.OP17.GE.108.0D0.AND.
     4        OP17.LE.206.0D0.OR.OP17.EQ.467.0D0.OR.OP17.GE.485.0D0.AND.
     5        OP17.LE.499.0D0.OR.OP17.GE.500.0D0.AND.OP17.LE.508.0D0) THEN
                  IF(DF3.EQ.1) THEN
                      WRITE(OUTLYNE,*)
     1                'PREDEFINED OPERAND ',OPNM
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'REQUIRES EXPLICIT NUMERIC WORD #2 INPUT'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
              IF(OP17.GE.500.0D0.AND.OP17.LE.508.0D0) THEN
                  IF(DF4.EQ.0.AND.W4.NE.0.0D0.OR.DF5.EQ.0.AND.W5.NE.0.0D0) THEN
                      WRITE(OUTLYNE,*)
     1                'PREDEFINED OPERAND ',OPNM
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'TAKES NO NUMERIC WORD #3 OR #4 INPUT'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
C
              IF(OP17.GE.70.0D0.AND.OP17.LE.90.0D0.OR.
     1        OP17.GE.92.0D0.AND.
     1        OP17.LE.93.0D0.OR.OP17.EQ.467.0D0.OR.OP17.GE.
     2        108.0D0.AND.OP17.LE.206.0D0.OR.OP17.GE.447.0D0
     3        .AND.OP17.LE.451.0D0.OR.OP17.GE.485.0D0.AND.OP17.LE.499.0D0
     4        .OR.OP17.GE.500.0D0.AND.OP17.LE.508.0D0) THEN
                  IF(W3.LT.0.0D0) W3=SYSTEM1(20)+W3
                  IF(W3.LT.0.0D0.OR.W3.GT.SYSTEM1(20)) THEN
C     BAD SURFACE NUMBER
                      WRITE(OUTLYNE,*)
     1                'INVALID SURFACE NUMBER ISSUED FOR'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'PREDEFINED OPERAND ',OPNM
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
C
              IF(OP17.GE.94.0D0.AND.OP17.LE.105.0D0) THEN
                  IF(W3.LT.DBLE(NEWOBJ).OR.W3.GT.DBLE(NEWIMG)) THEN
C     BAD SURFACE NUMBER
                      WRITE(OUTLYNE,*)
     1                'INVALID SURFACE NUMBER, NW2, ISSUED FOR'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'PREDEFINED OPERAND ',OPNM
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  IF(W4.LT.DBLE(NEWOBJ).OR.W4.GT.DBLE(NEWIMG)) THEN
C     BAD SURFACE NUMBER
                      WRITE(OUTLYNE,*)
     1                'INVALID SURFACE NUMBER, NW3, ISSUED FOR'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'PREDEFINED OPERAND ',OPNM
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
C
              IF(OP17.EQ.106.0D0.OR.OP17.EQ.107.0D0.OR.OP17.EQ.509.0D0
     1        .OR.OP17.EQ.511.0D0) THEN
                  IF(W3.LT.0.0D0) W3=SYSTEM1(20)+W3
                  IF(W4.LT.0.0D0) W4=SYSTEM1(20)+W4
                  IF(W3.LT.0.0D0.OR.W3.GT.SYSTEM1(20).OR.W3.GE.W4.OR.W4.LT.0.0D0
     1            .OR.W4.GT.SYSTEM1(20)) THEN
C     BAD SURFACE NUMBER
                      WRITE(OUTLYNE,*)
     1                'INVALID SURFACE NUMBERS ISSUED FOR'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'PREDEFINED OPERAND ',OPNM
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
C
              IF(OP17.EQ.91.0D0) THEN
                  IF(W3.LT.0.0D0) W3=SYSTEM1(20)+W3
                  IF(W3.LT.0.0D0.OR.W3.GT.SYSTEM1(20)) THEN
C     BAD SURFACE NUMBER
                      WRITE(OUTLYNE,*)
     1                'INVALID SURFACE NUMBER ISSUED FOR'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'PREDEFINED OPERAND ',OPNM
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
C
          END IF
          IF(OP17.GE.207.0D0.AND.OP17.LE.236.0D0) THEN
C
              IF(OP17.GE.207.0D0.AND.OP17.LE.236.0D0) THEN
C     PARAXIAL OPERANDS
C     DEFAULT INPUT W5
                  IF(DF5.EQ.0.AND.W5.NE.0.0D0) THEN
                      WRITE(OUTLYNE,*)
     1                'PREDEFINED OPERAND ',OPNM
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'TAKES NO NUMERIC WORD #4 INPUT'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
              IF(OP17.GE.227.0D0.AND.OP17.LE.234.0D0) THEN
                  IF(S4.EQ.1.AND.W4.NE.0.0D0.OR.S5.EQ.1.AND.W5.NE.0.0D0) THEN
                      WRITE(OUTLYNE,*)
     1                'PREDEFINED OPERAND ',OPNM
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'TAKES NO NUMERIC WORD #3 OR #4 INPUT'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
C     SET DEFAULTS FOR PWRY,PWRX,FLCLTH OR FLCLTHY AND FLCLTHX
              IF(OP17.GE.207.0D0.AND.
     1        OP17.LE.210.0D0) THEN
                  IF(W3.LT.0.0D0) W3=SYSTEM1(20)+W3
                  IF(W4.LT.0.0D0) W4=SYSTEM1(20)+W4
                  IF(DF3.EQ.1) W3=0.0D0
                  IF(DF3.EQ.1) DF3=0
                  IF(DF4.EQ.1) W4=SYSTEM1(20)
                  OP9 = W4
                  IF(DF4.EQ.1) DF4=0
                  OP11 =DBLE(DF4)
                  IREG=W3
                  OP8=W3
                  OP18=DBLE(DF3)
                  OP19=0.0D0
                  DF5=1
                  W5=0.0D0
              END IF
C     SET DEFAULTS FOR 211 TO 226
              IF(OP17.GE.211.0D0.AND.OP17.LE.226.0D0) THEN
                  IF(W3.LT.0.0D0) W3=SYSTEM1(20)+W3
                  IF(DF3.EQ.1) W3=SYSTEM1(20)
                  IF(DF3.EQ.1) DF3=0
                  IF(DF4.EQ.1) W4=SYSTEM1(11)
                  OP9 = W4
                  IF(DF4.EQ.1) DF4=0
                  OP11 =DBLE(DF4)
                  IREG=W3
                  OP8=W3
                  OP18=DBLE(DF3)
                  OP19=0.0D0
              END IF
C     SET DEFAULTS FOR 227 TO 234
              IF(OP17.GE.227.0D0.AND.OP17.LE.234.0D0) THEN
                  IF(W3.LT.0.0D0) W3=SYSTEM1(20)+W3
                  IF(DF3.EQ.1) W3=SYSTEM1(20)
                  IREG=W3
                  OP8=W3
                  OP18=DBLE(DF3)
                  OP19=0.0D0
              END IF
C     SET NW3 FOR 235 TO 236
              IF(OP17.GE.235.0D0.AND.OP17.LE.236.0D0) THEN
                  IF(DF3.EQ.1) THEN
                      WRITE(OUTLYNE,*)
     1                'PREDEFINED OPERAND ',OPNM
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'REQUIRES EXPLICIT NUMERIC WORD #2 INPUT'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
C
C     OUT OF RANGE INPUT
C
              IF(OP17.GE.207.0D0.AND.OP17.LE.210.0D0) THEN
                  IF(W3.LT.0.0D0) W3=SYSTEM1(20)+W3
                  IF(W4.LT.0.0D0) W4=SYSTEM1(20)+W4
                  IF(W3.LT.0.0D0.OR.W3.GT.SYSTEM1(20).OR.
     1            W4.LT.0.0D0.OR.W4.GT.SYSTEM1(20).OR.
     1            W4.LE.W3) THEN
C     BAD SURFACE NUMBERS
                      WRITE(OUTLYNE,*)
     1                'INVALID SURFACE NUMBERS ISSUED FOR'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'PREDEFINED OPERAND ',OPNM
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
              IF(OP17.GE.211.0D0.AND.OP17.LE.226.0D0) THEN
                  IF(W3.LT.0.0D0) W3=SYSTEM1(20)+W3
                  IF(W3.LT.0.0D0.OR.W3.GT.SYSTEM1(20)) THEN
C     BAD SURFACE NUMBER
                      WRITE(OUTLYNE,*)
     1                'INVALID SURFACE NUMBER ISSUED FOR'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'PREDEFINED OPERAND ',OPNM
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  IF(W4.LT.1.0D0.OR.W4.GT.10.0D0) THEN
C     BAD WAVELENGTH NUMBER
                      WRITE(OUTLYNE,*)
     1                'INVALID WAVELENGTH NUMBER ISSUED FOR'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'PREDEFINED OPERAND ',OPNM
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  ELSE
                      W4=DBLE(INT(W4))
                      OP9 = W4
                  END IF
              END IF
              IF(OP17.GE.235.0D0.AND.OP17.LE.236.0D0) THEN
                  IF(W3.LT.0.0D0) W3=SYSTEM1(20)+W3
                  IF(W3.LT.0.0D0.OR.W3.GT.SYSTEM1(20)) THEN
C     BAD SURFACE NUMBER
                      WRITE(OUTLYNE,*)
     1                'INVALID SURFACE NUMBER ISSUED FOR'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'PREDEFINED OPERAND ',OPNM
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
C
          END IF
C
C     *****************************************************************
          IF(OP17.GE.237.0D0.AND.OP17.LE.245.0D0.OR.OP17.EQ.513.0D0) THEN
C     FIRST GROUP OF SPOT OPERANDS
C     DEFAULT INPUT W5
              IF(DF5.EQ.0.AND.W5.NE.0.0D0) THEN
                  WRITE(OUTLYNE,*)
     1            'PREDEFINED OPERAND ',OPNM
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)
     1            'TAKES NO NUMERIC WORD #4 INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C     DEFAULT INPUT W3
              IF(DF3.EQ.1) THEN
                  WRITE(OUTLYNE,*)
     1            'PREDEFINED OPERAND ',OPNM
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)
     1            'REQUIRES EXPLICIT NUMERIC WORD #2 INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C
C     OUT OF RANGE INPUT
C
              IF(W3.LT.1.0D0.OR.W3.GT.200.0D0) THEN
C     BAD FIELD POS NUMBER
                  WRITE(OUTLYNE,*)
     1            'INVALID FIELD POSITION NUMBER ISSUED FOR'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)
     1            'PREDEFINED OPERAND ',OPNM
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C
C     OUT OF RANGE INPUT
C
              IF(W4.LE.0.0D0.OR.W4.GT.10.0D0) THEN
                  W4=0.0D0
                  DF4=1
              END IF
              IF(DF4.EQ.0) THEN
                  IF(W4.NE.1.0D0.AND.
     1            W4.NE.2.0D0.AND.
     1            W4.NE.3.0D0.AND.
     1            W4.NE.4.0D0.AND.
     1            W4.NE.5.0D0.AND.
     1            W4.NE.6.0D0.AND.
     1            W4.NE.7.0D0.AND.
     1            W4.NE.8.0D0.AND.
     1            W4.NE.9.0D0.AND.
     1            W4.NE.10.0D0) THEN
C     BAD WAVELENGTH NUMBER
                      WRITE(OUTLYNE,*)
     1                'INVALID WAVELENGTH NUMBER ISSUED FOR'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'PREDEFINED OPERAND ',OPNM
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
              OP8=W3
              OP18=DBLE(DF3)
              OP19=0.0D0
              OP9=W4
              OP11=DBLE(DF4)
C
          END IF
C     *****************************************************************
          IF(OP17.EQ.246.0D0.OR.OP17.GE.460.0D0.AND.
     1    OP17.LE.465.0D0.OR.OP17.EQ.484.0D0) THEN
C     CAPFN OPERANDS AND SECOND GROUP OF SPOT OPERANDS
C
              IF(OP17.EQ.246.0D0.OR.OP17.GE.460.0D0.AND.
     1        OP17.LE.465.0D0.OR.OP17.EQ.484.0D0) THEN
C     SPOT OPERANDS
C     DEFAULT INPUT W3
                  IF(DF3.EQ.1) THEN
                      WRITE(OUTLYNE,*)
     1                'PREDEFINED OPERAND ',OPNM
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'REQUIRES EXPLICIT NUMERIC WORD #2 INPUT'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
C
C     OUT OF RANGE INPUT
C
              IF(OP17.EQ.246.0D0.OR.OP17.GE.460.0D0.AND.
     1        OP17.LE.465.0D0.OR.OP17.EQ.484.0D0) THEN
                  IF(W3.LT.1.0D0.OR.W3.GT.200.0D0) THEN
C     BAD FIELD POS NUMBER
                      WRITE(OUTLYNE,*)
     1                'INVALID FIELD POSITION NUMBER ISSUED FOR'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'PREDEFINED OPERAND ',OPNM
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
              IF(OP17.EQ.246.0D0) THEN
                  IF(W4.LT.1.0D0.OR.W4.GT.37.0D0) THEN
C     BAD ZERN NUMBER
                      WRITE(OUTLYNE,*)
     1                'INVALID ZERN COEFFICIENT NUMBER ISSUED FOR'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'PREDEFINED OPERAND ',OPNM
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  ELSE
                      OP9=W4
                      OP11=DBLE(DF4)
                  END IF
              END IF
              IF(OP17.EQ.246.0D0) THEN
                  IF(DF4.EQ.1) THEN
C     BAD ZERN NUMBER
                      WRITE(OUTLYNE,*)
     1                'PREDEFINED OPERAND ',OPNM
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'REQUIRES EXPLICIT NUMERIC WORD #3 INPUT'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
              IF(OP17.EQ.484.0D0) THEN
                  IF(DF4.EQ.1) THEN
                      DF4=0
                      S4=1
                      W4=1.0D0
                  END IF
                  IF(W4.LT.0.0D0) THEN
C     BAD OPDWEIGT
                      WRITE(OUTLYNE,*)
     1                'OPD WEIGHT MUST BE GREATER THAN ZERO'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'PREDEFINED OPERAND ',OPNM
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  ELSE
                      OP9=W4
                      OP11=DBLE(DF4)
                  END IF
              END IF
              IF(OP17.EQ.464.0D0.OR.OP17.EQ.465.0D0) THEN
                  IF(W4.LE.0.0D0.OR.W4.GT.100.0D0) THEN
C     BAD % NUMBER
                      WRITE(OUTLYNE,*)
     1                'INVALID ENCIRLED ENERGY PERCENTAGE ISSUED FOR'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'PREDEFINED OPERAND ',OPNM
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  ELSE
                      OP9=W4
                      OP11=DBLE(DF4)
                  END IF
              END IF
              IF(OP17.GE.460.0D0.AND.OP17.LE.465.0D0) THEN
                  IF(DF4.EQ.1) THEN
C     BAD W4
                      WRITE(OUTLYNE,*)
     1                'PREDEFINED OPERAND ',OPNM
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'REQUIRES EXPLICIT NUMERIC WORD #3 INPUT'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
              IF(OP17.EQ.246.0D0.OR.OP17.EQ.484.0D0) THEN
                  IF(DF5.EQ.1) W5=SYSTEM1(11)
                  OP10 =W5
                  IF(DF5.EQ.1) DF5=0
                  OP12 =DBLE(DF5)
              END IF
              IF(OP17.EQ.246.0D0.OR.OP17.EQ.484.0D0) THEN
                  IF(W5.LT.1.0D0.OR.W5.GT.10.0D0) THEN
C     BAD WAVELENGTH NUMBER
                      WRITE(OUTLYNE,*)
     1                'INVALID WAVELENGTH NUMBER ISSUED FOR'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'PREDEFINED OPERAND ',OPNM
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  ELSE
                      W5=DBLE(INT(W5))
                      OP10 =W5
                      OP12 =DBLE(DF5)
                  END IF
              END IF
              IF(OP17.GE.464.0D0.AND.OP17.LE.465.0D0) THEN
                  IF(DF5.EQ.0) THEN
C     BAD W5 NUMBER
                      WRITE(OUTLYNE,*)
     1                'NO NUMERIC WORD #4 INPUT IS USED WITH'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'PREDEFINED OPERAND ',OPNM
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  ELSE
                      W5=DBLE(INT(W5))
                      OP10 =W5
                      OP12 =DBLE(DF5)
                  END IF
              END IF
              IF(OP17.GE.460.0D0.AND.OP17.LE.463.0D0) THEN
                  IF(DF5.EQ.1) THEN
                      WRITE(OUTLYNE,*)
     1                'EXPLICIT NUMERIC WORD #4 INPUT REQUIRED WITH'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'PREDEFINED OPERAND ',OPNM
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  ELSE
                      W5=DBLE(INT(W5))
                      OP10 =W5
                      OP12 =DBLE(DF5)
                  END IF
              END IF
              IF(OP17.GE.460.0D0.AND.OP17.LE.463.0D0) THEN
                  IF(DF5.EQ.1.AND.W1.NE.0.0D0.AND.W2.NE.90.0D0) THEN
                      WRITE(OUTLYNE,*)
     1                'NUMERIC WORD #4, ORIENTATION VALUE MUST BE 0 OR 90 FOR'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'PREDEFINED OPERAND ',OPNM
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  ELSE
                      W5=DBLE(INT(W5))
                      OP10 =W5
                      OP12 =DBLE(DF5)
                  END IF
              END IF
          END IF
C
C
C     *****************************************************************
C
          IF(OP17.GE.247.0D0.AND.OP17.LE.296.0D0.OR.
     1    OP17.GE.452.0D0.AND.OP17.LE.459.0D0) THEN
C
              IF(OP17.GE.247.0D0.AND.OP17.LE.296.0D0) THEN
C     SPECIAL OPERANDS
C     DEFAULT INPUT W5
                  IF(DF5.EQ.0.AND.W5.NE.0.0D0) THEN
                      WRITE(OUTLYNE,*)
     1                'PREDEFINED OPERAND ',OPNM
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'TAKES NO NUMERIC WORD #4 INPUT'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
              IF(OP17.GE.247.0D0.AND.OP17.LE.284.OR.
     1        OP17.GE.293.0D0.AND.OP17.LE.296.0D0.OR.OP17.EQ.466.0D0) THEN
C     CAPFN OPERANDS
C     DEFAULT INPUT W4
                  IF(DF4.EQ.0.AND.W4.NE.0.0D0) THEN
                      WRITE(OUTLYNE,*)
     1                'PREDEFINED OPERAND ',OPNM
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'TAKES NO NUMERIC WORD #3 INPUT'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
              IF(OP17.GE.247.0D0.AND.OP17.LE.296.0D0) THEN
C     SPECIAL OPERANDS
C     DEFAULT INPUT W3
                  IF(DF3.EQ.1) THEN
                      WRITE(OUTLYNE,*)
     1                'PREDEFINED OPERAND ',OPNM
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'REQUIRES EXPLICIT NUMERIC WORD #2 INPUT'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
C
C     OUT OF RANGE INPUT
C
              IF(OP17.GE.247.0D0.AND.OP17.LE.278.0D0.OR.
     1        OP17.LE.293.0D0.AND.OP17.LE.296.0D0.OR.OP17.EQ.466.0D0) THEN
                  IF(W3.LT.1.0D0.OR.W3.GT.200.0D0) THEN
C     BAD FIELD POS NUMBER
                      WRITE(OUTLYNE,*)
     1                'INVALID FIELD POSITION NUMBER ISSUED FOR'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'PREDEFINED OPERAND ',OPNM
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
              IF(OP17.GE.279.0D0.AND.OP17.LE.284.0D0) THEN
                  IF(W3.LT.0.0D0) W3=SYSTEM1(20)+W3
                  IF(W3.LT.1.0D0.OR.W3.GT.SYSTEM1(20)) THEN
C     BAD FIELD POS NUMBER
                      WRITE(OUTLYNE,*)
     1                'INVALID SURFACE NUMBER ISSUED FOR'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'PREDEFINED OPERAND ',OPNM
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
              IF(OP17.GE.285.0D0.AND.OP17.LE.292.0D0) THEN
                  IF(DF3.EQ.1) THEN
C     BAD SURF NUMBER
                      WRITE(OUTLYNE,*)
     1                'EXPLICIT SURFACE NUMBER REQUIRED FOR'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'PREDEFINED OPERAND ',OPNM
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  IF(DF4.EQ.1) THEN
C     BAD FIELD NUMBER
                      WRITE(OUTLYNE,*)
     1                'EXPLICIT FIELD NUMBER REQUIRED FOR'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'PREDEFINED OPERAND ',OPNM
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  IF(W4.LT.1.0D0.OR.W4.GT.200.0D0) THEN
C     BAD FIELD POS NUMBER
                      WRITE(OUTLYNE,*)
     1                'INVALID FIELD POSITION NUMBER ISSUED FOR'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'PREDEFINED OPERAND ',OPNM
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  IF(W3.LT.DBLE(NEWOBJ).OR.W3.GT.DBLE(NEWIMG)) THEN
C     BAD SURF NUMBER
                      WRITE(OUTLYNE,*)
     1                'INVALID SURFACE NUMBER ISSUED FOR'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'PREDEFINED OPERAND ',OPNM
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
              IF(OP17.GE.452.0D0.AND.OP17.LE.459.0D0) THEN
                  IF(DF3.EQ.1) THEN
                      DF3=0
                      W3=INT(SYSTEM1(20))
                  END IF
              END IF
              IF(OP17.GE.452.0D0.AND.OP17.LE.459.0D0) THEN
                  IF(DF4.EQ.1) THEN
C     BAD FIELD NUMBER
                      WRITE(OUTLYNE,*)
     1                'EXPLICIT FIELD NUMBER REQUIRED FOR'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'PREDEFINED OPERAND ',OPNM
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  IF(DF5.EQ.0) THEN
C     BAD SURF NUMBER
                      WRITE(OUTLYNE,*)
     1                'NO NUMERIC WORD #4 USED FOR'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'PREDEFINED OPERAND ',OPNM
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  IF(W4.LT.1.0D0.OR.W4.GT.200.0D0) THEN
C     BAD FIELD POS NUMBER
                      WRITE(OUTLYNE,*)
     1                'INVALID FIELD POSITION NUMBER ISSUED FOR'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'PREDEFINED OPERAND ',OPNM
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  IF(W3.LT.0.0D0) W3=SYSTEM1(20)+W3
                  IF(W3.LT.0.0D0.OR.W3.GT.SYSTEM1(20)) THEN
C     BAD FIELD SURF NUMBER
                      WRITE(OUTLYNE,*)
     1                'INVALID SURFACE NUMBER ISSUED FOR'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'PREDEFINED OPERAND ',OPNM
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
C
          END IF
C
C     *****************************************************************
          IF(OP17.GE.297.0D0.AND.OP17.LE.446.0D0) THEN
C     3,5,7 ABERRATIONS
C
C     DEFAULT INPUT W4 AND W5
              IF(DF4.EQ.0.AND.W4.NE.0.0D0.OR.DF5.EQ.0.AND.W5.NE.0.0D0) THEN
                  WRITE(OUTLYNE,*)
     1            'PREDEFINED OPERAND ',OPNM
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)
     1            'TAKES NO NUMERIC WORD #3 OR #4 INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(W3.LT.0.0D0) W3=SYSTEM1(20)+W3
              IF(DF3.EQ.1) W3=SYSTEM1(20)
              IF(DF3.EQ.1) DF3=0
              IREG=W3
              OP8=W3
              OP18=DBLE(DF3)
              OP19=0.0D0
C
              IF(W3.LT.1.0D0.OR.W3.GT.SYSTEM1(20)) THEN
C     BAD SURFACE NUMBER
                  WRITE(OUTLYNE,*)
     1            'INVALID SURFACE NUMBER ISSUED FOR'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)
     1            'PREDEFINED OPERAND ',OPNM
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C
          END IF
C
C     INPUT OK FOR W3, W4 AND W5
C
 3141     CONTINUE
C     ANY 8 CHARACTER QUALIFIER WORD OPERAND NAME MAY BE USED
C     BUT DUPLICATE NAMES CAUSE REPLACEMENT
C     HERE IS WERE THE REPLACEMENT HAPPENS
          I=INT(W1)+MAXFOCRIT
          IF(WQ.EQ.OPNAM(I).AND.
     1    W3.EQ.OPERND(I,8).AND.W4.EQ.OPERND(I,9)
     1    .AND.W5.EQ.OPERND(I,10).AND.OPT.EQ.OPERND(I,1)
     1    .AND.DBLE(CORMOD).EQ.OPERND(I,13)) THEN
C     FUNC, NAME, NW3,NW4,NW5,CFG AND CORMOD MUST MATCH OR NO REPLACEMENT
C
              OP1 =DBLE(OPT)
              OPNM=WQ
              OP2 =0.0D0
              OP3 =0.0D0
              OP4 =0.0D0
              OP5 =0.0D0
              OP6 =0.0D0
              OP7 =OW5
              IREG=W3
              OP8 =IREG
              OP18 =DBLE(DF3)
              OP9 = W4
              OP10 =W5
              OP11 =DBLE(DF4)
              OP12 =DBLE(DF5)
              OP13 =DBLE(CORMOD)
              OP14 = 0.0D0
              OP15 = 0.0D0
              IF(OPT.EQ.0) OP16 = 1.0D0
              IF(OPT.NE.0) OP16 = 0.0D0
              IREG=W3
              OP8=W3
              OP18=DBLE(DF3)
              OP19=0.0D0
              OP20=OW5
C
              OPERND(I,1) =OP1
              OPERND(I,2) =OP2
              OPERND(I,3) =OP3
              OPERND(I,4) =OP4
              OPERND(I,5) =OP5
              OPERND(I,6) =OP6
              OPERND(I,7) =OP7
              OPERND(I,8) =OP8
              OPERND(I,9) =OP9
              OPERND(I,10) =OP10
              OPERND(I,11) =OP11
              OPERND(I,12) =OP12
              OPERND(I,13) =OP13
              OPERND(I,14) =OP14
              OPERND(I,15) =OP15
              OPERND(I,16) =OP16
              OPERND(I,17) =OP17
              OPERND(I,18) =OP18
              OPERND(I,19) =OP19
              OPERND(I,20) =OP20
              ISTOP(I-MAXFOCRIT)=.TRUE.
              RETURN
          END IF
C
C     IT IS A NEW OPERAND
          OP1 =DBLE(OPT)
          OPNM=WQ
          OP2 =0.0D0
          OP3 =0.0D0
          OP4 =0.0D0
          OP5 =0.0D0
          OP6 =0.0D0
          OP7 =OPWEIT
          IREG=W3
          OP8 =IREG
          OP18 =DBLE(DF3)
          OP9 = W4
          OP10 =W5
          OP11 =DBLE(DF4)
          OP12 =DBLE(DF5)
          OP13 =DBLE(CORMOD)
          OP14 = 0.0D0
          OP15 = 0.0D0
          IREG=W3
          OP18=DBLE(DF3)
          OP19=0.0D0
          IF(OPT.EQ.0) OP16 = 1.0D0
          IF(OPT.NE.0) OP16 = 0.0D0
          OP20=OW5
C
          I=INT(W1)+MAXFOCRIT
          OPNAM(I)=OPNM
          OPERND(I,1) =OP1
          OPERND(I,2) =OP2
          OPERND(I,3) =OP3
          OPERND(I,4) =OP4
          OPERND(I,5) =OP5
          OPERND(I,6) =OP6
          OPERND(I,7) =OP7
          OPERND(I,8) =OP8
          OPERND(I,9) =OP9
          OPERND(I,10) =OP10
          OPERND(I,11) =OP11
          OPERND(I,12) =OP12
          OPERND(I,13) =OP13
          OPERND(I,14) =OP14
          OPERND(I,15) =OP15
          OPERND(I,16) =OP16
          OPERND(I,17) =OP17
          OPERND(I,18) =OP18
          OPERND(I,19) =OP19
          OPERND(I,20) =OP20
          ISTOP(I-MAXFOCRIT)=.TRUE.
          RETURN
C       ALL DONE
      END
C SUB FOCRIT1.FOR
      SUBROUTINE FOCRIT1
C
          IMPLICIT NONE
C
          INTEGER II,I,OPT
          INTEGER OPNNM
C
          CHARACTER*8 OPNM
C
          LOGICAL YES
          LOGICAL OPOK
C
          REAL*8 OPWEIT,IREG
C
          REAL*8 OP1,OP2,OP3,OP4,OP5,OP6,OP7,OP8,OP9,OP10
     1    ,OP11,OP12,OP13,OP14,OP15,OP16,OP17,OP18,OP19
C
          INCLUDE 'datsub.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          INCLUDE 'datcfg.inc'
          INCLUDE 'datspd.inc'
C
C       THIS IS SUBROUTINE FOCRIT. THIS IS THE SUBROUTINE WHICH
C       HANDLES FOCRIT INPUT AND FOCRIT UPDATE COMMANDS AND
C       OUTPUT COMMANDS AT THE CMD LEVEL
C
C       THE ARRAY OPERND AND OPNAM STORE FOCRIT INFORMATION
C       IT IS PASSED IN COMMON IN THE INCLUDE FILE DATSUB.FOR
C
C       OPERND(I,J) WHERE I COUNTS THE NUMBER OF OPERAND ENTRIES
C       AND J TAKES ON THE FOLLOWING VALUES AND MEANIINGS.
C
C       I WILL BE 1 TO 5 ONLY
C
C       J=1  > 1 THROUGH 10, THE NUMBER OF THE FUNCTION IN WHICH
C               THE ACTUAL OPERAND IS CALCULATED (COMMAND WORD)
C         (0 IS PREDEFINED OPERAND)
C
C       J=2  > TARGET VALUE OF THE OPERAND, THIS WILL BE THE ORIGINAL VALUE
C       J=3  > OPERAND ORIGINAL VALUE
C       J=4  > OPERAND CURRENT VALUE
C       J=5  > OPERAND PREVIOUS VALUE
C       J=6  > LAST OPERAND CHANGE VALUE (CURRENT-PREVIOUS)
C       J=7  > WT, THE WEIGHTING FACTOR DURING OPTIMIZATION ALWAYS 1
C
C       J=8  > NUMBER OF THE GENERAL PURPOSE REGISTER CONTAINING
C               THE ACTUAL OPERAND VALUE AS RETURNED FROM THE
C               CALCULATING FUNCTION (NUMERIC WORD #2) (VALUES 1 TO 400)
C               (A DEFAULT ENTRY HERE CAUSES THE ACCUMULATOR TO BE USED)
C       FOR PREDEFINED OPERAND, IT IS THE (I) INPUT VALUE
C
C       J=9  > OPTIONAL NW1 FOR THE FUNCTION (USING NSUB) (NUMERIC WORD #3)
C       J=10 > OPTIONAL NW2 FOR THE FUNCTION (USING NSUB) (NUMERIC WORD #4)
C     FOR PREDEFINED OPERANDS J=9 AND J=10 ARE THE (J) AND (K) INPUT VALUES
C       J=11 > DEFAULT FLAG FOR NW1
C       J=12 > DEFAULT FLAG FOR NW2
C       J=13 > COR MODE 10
          CORMOD=10
C       J=14 > SQUARE ROOT OF WEIGHT TIMES (CURRENT VAL-TARGET))
C       J=15 > 0 BEFORE THE FIRST CALCULATION OF OPERAND VALUES
C              1 AFTER THE FIRST CALULATION
C       J=16 > CFG # FOR PREDEFINED OPERANDS (ALWAYS 1)
C       J=17 > CODE FOR THE PREDEFINED OPERANDS
C       J=18 > DEFAULT FLAG FOR W2 = DBLE(DF2)
C       J=19 > 0 IF OP CALCULABLE, 1 IF NOT
C       J=20 > RESERVED FOR EXPANSION
C             1=X
C             2=Y
C             3=Z
C             4=L
C             5=M
C             6=N
C             7=DX
C             8=DY
C             9=DR
C            10=DXA
C            11=DYA
C            12=DRA
C            13=XANG
C            14=YANG
C            15=OPL
C            16=OPD
C            17=OPDW
C            18=LOLD
C            19=MOLD
C            20=NOLD
C            21=LEN
C            22=AII
C            23=AIP
C            24=LN
C            25=MN
C            26=NN
C            27=PXPX
C            28=PXPY
C            29=PYPX
C            30=PYPY
C            31=PXAPX
C            32=PXAPY
C            33=PYAPX
C            34=PYAPY
C            35=DXDX
C            36=DXDY
C            37=DYDX
C            38=DYDY
C            39=DXADX
C            40=DXADY
C            41=DYADX
C            42=DYADY
C            43=XREF
C            44=YREF
C            45=ZREF
C            46=LREF
C            47=MREF
C            48=NREF
C            49=LREFOL
C            50=MREFOL
C            51=NREFOL
C            52=IREF
C            53=IPREF
C            54=XAREF
C            55=YAREF
C            56=LNREF
C            57=MNREF
C            58=NNREF
C            59=GLX
C            60=GLY
C            61=GLZ
C            62=GLL
C            63=GLM
C            64=GLN
C            65=GLLOLD
C            66=GLMOLD
C            67=GLNOLD
C            68=LENREF
C            69=OPLREF
C            70=RD
C            71=CV
C            72=TH
C            73=CC
C            74=AC
C            75=AD
C            76=AE
C            77=AF
C            78=AG
C            79=RDTOR
C            80=CVTOR
C            81=CCTOR
C            82=ADTOR
C            83=AETOR
C            84=AFTOR
C            85=AGTOR
C            86=ALPHA
C            87=BETA
C            88=GAMMA
C            89=VNUM
C            90=PARTL
C            91=INDEX
C            92=XD
C            93=YD
C            94=XVERT
C            95=YVERT
C            96=ZVERT
C            97=LXVERT
C            98=MXVERT
C            99=NXVERT
C           100=LYVERT
C           101=MYVERT
C           102=NYVERT
C           103=LZVERT
C           104=MZVERT
C           105=NZVERT
C           106=LENGTH
C           106=OAL
C           107=MLENGTH
C           107=OPTLEN
C           108=ET
C           108=ETY
C           109=ETX
C           110=SHAPEFAC
C           111 TO 206 = C1 THROUGH C96
C           207=PWRY
C           208=PWRX
C           209=FLCLTHX
C           210=FLCLTH OR FLCLTHY
C           211=PY
C           212=PX
C           213=PCY
C           214=PCX
C           215=PUY
C           216=PUX
C           217=PUCY
C           218=PUCX
C           219=PIY
C           220=PIX
C           221=PICY
C           222=PICX
C           223=PIYP
C           224=PIXP
C           225=PICYP
C           226=PICXP
C           227=PACY
C           228=PACX
C           229=PLCY
C           230=PLCX
C           231=SACY
C           232=SACX
C           233=SLCY
C           234=SLCX
C           235=IMDISX
C           236=IMDISY
C           237=CENTX
C           238=CENTY
C           239=RMSX
C           240=RMSY
C           241=RMS
C           242=RSSX
C           243=RSSY
C           244=RSS
C           245=RMSOPD
C           246=ZERN37
C           247=MAGX
C           248=MAGY
C           249=MAGXOR
C           250=MAGYOR
C           251=FFLX
C           252=FFLY
C           253=BFLX
C           254=BFLY
C           255=FFNX
C           256=FFNY
C           257=BFNX
C           258=BFNY
C           259=EFLX
C           260=EFLY
C           261=ENDIAX
C           262=ENDIAY
C           263=EXDIAX
C           264=EXDIAY
C           265=ENPOSX
C           266=ENPOSY
C           267=ENPOSZ
C           268=EXPOSX
C           269=EXPOSY
C           270=EXPOSZ
C           271=FNUMX
C           272=FNUMY
C           273=OBFNUMX
C           274=OBFNUMY
C           275=ENPDIAX
C           276=ENPDIAY
C           277=EXPDIAX
C           278=EXPDIAY
C           279=PUPDIAX
C           280=PUPDIAY
C           281=PUPDISX
C           282=PUPDISY
C           283=CHFIMX
C           284=CHFIMY
C           285=GPX
C           286=GPY
C           287=GPUX
C           288=GPUY
C           289=GPCX
C           290=GPCY
C           291=GPUCX
C           292=GPUCY
C           293=DIST
C           294=XFOC
C           295=YFOC
C           296=AST
C           297=SA3
C           298=XSA3
C           299=CMA3
C           300=XCMA3
C           301=AST3
C           302=XAST3
C           303=DIS3
C           304=XDIS3
C           305=PTZ3
C           306=XPTZ3
C           307=SA5
C           308=XSA5
C           309=CMA5
C           310=XCMA5
C           311=AST5
C           312=XAST5
C           313=DIS5
C           314=XDIS5
C           315=PTZ5
C           316=XPTZ5
C           317=TOBSA
C           318=XTOBSA
C           319=SOBSA
C           320=XSOBSA
C           321=ELCMA
C           322=XELCMA
C           323=TAS
C           324=XTAS
C           325=SAS
C           326=XSAS
C           327=SA7
C           328=XSA7
C           329=SA3P
C           330=XSA3P
C           331=CMA3P
C           332=XCMA3P
C           333=AST3P
C           334=XAST3P
C           335=DIS3P
C           336=XDIS3P
C           337=PTZ3P
C           338=XPTZ3P
C           339=SA5P
C           340=XSA5P
C           341=CMA3P
C           342=XCMA3P
C           343=AST5P
C           344=XAST5P
C           345=DIS5P
C           346=XDIS5P
C           347=PTZ5P
C           348=XPTZ5P
C           349=TOBSAP
C           350=XTOBSAP
C           351=SOBSAP
C           352=XSOBSAP
C           353=ELCMAP
C           354=XELCMAP
C           355=TASP
C           356=XTASP
C           357=SASP
C           358=XSASP
C           359=SA7P
C           360=XSA7P
C           361=SA3S
C           362=XSA3S
C           363=CMA3S
C           364=XCMA3S
C           365=AST3S
C           366=XAST3S
C           367=DIS3S
C           368=XDIS3S
C           369=PTZ3S
C           370=XPTZ3S
C           371=SA5S
C           372=XSA5S
C           373=CMA5S
C           374=XCMA5S
C           375=AST5S
C           376=XAST5S
C           377=DIS5S
C           378=XDIS5S
C           379=PTZ5S
C           380=XPTZ5S
C           381=TOBSAS
C           382=XTOBSAS
C           383=SOBSAS
C           384=XSOBSAS
C           385=ELCMAS
C           386=XELCMAS
C           387=TASS
C           388=XTASS
C           389=SASS
C           390=XSASS
C           391=SA7S
C           392=XSA7S
C           393=SA5I
C           394=XSA5I
C           395=CMA5I
C           396=XCMA5I
C           397=AST5I
C           398=XAST5I
C           399=DIS5I
C           400=XDIS5I
C           401=PTZ5I
C           402=XPTZ5I
C           403=TOBSAI
C           404=XTOBSAI
C           405=SOBSAI
C           406=XSOBSAI
C           407=ELCMAI
C           408=XELCMAI
C           409=TASI
C           410=XTASI
C           411=SASI
C           412=XSASI
C           413=SA7I
C           414=XSA7I
C           415=PSA3
C           416=XPSA3
C           417=PCMA3
C           418=XPCMA3
C           419=PAST3
C           420=XPAST3
C           421=PDIS3
C           422=XPDIS3
C           423=PPTZ3
C           424=XPPTZ3
C           425=PSA3P
C           426=XPSA3P
C           427=PCMA3P
C           428=XPCMA3P
C           429=PAST3P
C           430=XPAST3P
C           431=PDIS3P
C           432=XPDIS3P
C           433=PPTZ3P
C           434=XPPTZ3P
C           435=PSA3S
C           436=XPSA3S
C           437=PCMA3S
C           438=XPCMA3S
C           439=PAST3S
C           440=XPAST3S
C           441=PDIS3S
C           442=XPDIS3S
C           443=PPTZ3S
C           444=XPPTZ3S
C           445=PTZCV
C           446=XPTZCV
C           447=AH
C           448=AI
C           449=AJ
C           450=AK
C           451=AL
C           452='GBRADX'
C           453='GBRADY'
C           454='GBDISX'
C           455='GBDISY'
C           456='GBRCVX'
C           457='GBRCVY'
C           458='GBWAISTX'
C           459='GBWAISTY'
C           460='MGOTF'
C           461='PGOTF'
C           462='MDOTF'
C           463='PDOTF'
C           460='GOTFM'
C           461='GOTFP'
C           462='DOTFM'
C           463='DOTFP'
C           464='RED'
C           465='REDCEN'
C           466='FISHDIST'
C           467='ZD'
C           468='SYMX'
C           469='SYMY'
C           470='ASYMX'
C           471='ASYMY'
C           472='PACM'
C           473='PACZ'
C           474='SACM'
C           475='SACZ'
C           476='PLCM'
C           477='PLCZ'
C           478='SLCM'
C           479='SLCZ'
C           480='CTSX'
C           481='CTSY'
C           482='SCEX'
C           483='SCEY'
C           484='GREYS'
C           485='PIVX'
C           486='PIVY'
C           487='PIVZ'
C           488='N1'
C           489='N2'
C           490='N3'
C           491='N4'
C           492='N5'
C           493='N6'
C           494='N7'
C           495='N8'
C           496='N9'
C           497='N10'
C           498='ABBE'
C           499='DPART'
C           500='CLPX'
C           501='CLPY'
C           502='GDX'
C           503='GDY'
C           504='GDZ'
C           505='GALPHA'
C           506='GBETA'
C           507='GGAMMA'
C           508='GRS'
C           509='WEIGHT'
C           510='DMINUSD'
C           511='COST'
C           512='MACOPT'
C           513='RMSYX'
C
C       OPNM=(8 CHARACTER USER DEFINED, NON-DUPLICATED OPERAND NAME)
C
          IF(WC.EQ.'M'.OR.WC.EQ.'C') THEN
              CALL MESCOM
              RETURN
          END IF
C
C       "CRITS" OUTPUT FOCRIT DATA FROM INSIDE
C       AND OUTSIDE OF THE FOCRIT SUBFILE VIA SUBROUTINE FCOUT.FOR.
          IF(WC.EQ.'CRITS') THEN
              OPTMES=.FALSE.
              CALL CRITOUT
              OPTMES=.TRUE.
              RETURN
          END IF
C
C       NOW DO CASE OF WC = EOS
C
C***********************************************************************
C       DEAL WITH WC=EOS
          IF(WC.EQ.'EOS') THEN
              IF(SST.EQ.1.OR.SQ.EQ.1.OR.SN.EQ.1)THEN
                  WRITE(OUTLYNE,*)'"EOS" TAKES NO EXPLICIT INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C       PROCEED WITH ACTION FOR COMMAND
              F1=1
              F54=0
              YES=.FALSE.
              DO II=1,MAXFOCRIT
                  IF(ISCRIT(II)) YES=.TRUE.
              END DO
              IF(.NOT.YES) FCCNT=0
              IF(YES) FCCNT=1
              IF(FCCNT.EQ.0) THEN
                  WRITE(OUTLYNE,*)'THE FOCRIT SUBFILE IS EMPTY'
                  CALL SHOWIT(1)
              END IF
              RETURN
C       ACTION COMPLETED
          END IF
C
C       EOS DONE
C***********************************************************************
C
C       NOW DO WC=DEL
          IF(WC.EQ.'DEL') THEN
              IF(F54.NE.2) THEN
                  WRITE(OUTLYNE,*)
     1            '"DEL" IS ONLY AVAILABLE FROM THE "UPDATE FOCRIT" LEVEL'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'NO ACTION TAKEN'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(SST.EQ.1.OR.SQ.EQ.1.OR.S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1
     1        .OR.S5.EQ.1) THEN
                  WRITE(OUTLYNE,*)
     1            '"DEL" ONLY TAKES NUMERIC WORD #1 INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(S1.EQ.0) THEN
                  WRITE(OUTLYNE,*)
     1            '"DEL" REQUIRES EXPLICIT NUMERIC WORD #1 INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(W1.LT.1.0D0.OR.W1.GT.5.0D0) THEN
                  WRITE(OUTLYNE,*)
     1            '"DEL" REQUIRES NUMERIC INPUT FROM 1 TO 5 ONLY'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              FMTEXT=.FALSE.
C
C     HERE IS WHERE FOCRIT IS DELETED
              ISCRIT(INT(W1))=.FALSE.
              OPERND(INT(W1),1:20)=0.0D0
              YES=.FALSE.
              DO II=1,MAXFOCRIT
                  IF(ISCRIT(II)) YES=.TRUE.
              END DO
              IF(.NOT.YES) FCCNT=0
              IF(YES) FCCNT=1
              IF(FCCNT.EQ.0) THEN
                  WRITE(OUTLYNE,*)'THE FOCRIT SUBFILE IS EMPTY'
                  CALL SHOWIT(1)
              END IF
C     ALL DELETIONS COMPLETED
              RETURN
C
          END IF
C***********************************************************************
C
C       NOW DO WC=OP_DESC
          IF(WC.EQ.'OP_DESC') THEN
              IF(SN.EQ.1) THEN
                  WRITE(OUTLYNE,*)
     1            '"OP_DESC" TAKES NO NUMERIC INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(SQ.EQ.0.OR.SST.EQ.0) THEN
                  WRITE(OUTLYNE,*)
     1            '"OP_DESC" REQUIRES EXPLICIT QUALIFIER AND STRING INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              OPOK=.FALSE.
              IF(WQ.EQ.'OP1') OPOK=.TRUE.
              IF(WQ.EQ.'OP2') OPOK=.TRUE.
              IF(WQ.EQ.'OP3') OPOK=.TRUE.
              IF(WQ.EQ.'OP4') OPOK=.TRUE.
              IF(WQ.EQ.'OP5') OPOK=.TRUE.
              IF(WQ.EQ.'OP1') OPNNM=1
              IF(WQ.EQ.'OP2') OPNNM=2
              IF(WQ.EQ.'OP3') OPNNM=3
              IF(WQ.EQ.'OP4') OPNNM=4
              IF(WQ.EQ.'OP5') OPNNM=5
              IF(.NOT.OPOK) THEN
                  WRITE(OUTLYNE,*)
     1            'DURING FOCRIT OPERAND INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)
     1            '"OP_DESC" REQUIRES A QUALIFIER WORD "OP1" THROUGH "OP5"'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(.NOT.ISTOP(OPNNM)) THEN
                  WRITE(OUTLYNE,*)
     1            'FOCRIT OPERAND ',OPNNM,' HAS NOT YET BEEN DEFINED'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)
     1            'A FOCRIT OPERAND DESCRIPTION ENTRY IS NOT ALLOWED'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'NO ACTION TAKEN'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C
C     HERE IS WHERE OP_DESC IS ASSIGNED
              IF(WS(1:8).NE.'        ') OPERDESC(OPNNM)=WS
              RETURN
C
          END IF
C
C     HERE WE SHIFT THE INPUT EXCEPT FOR W1
          W5=W4
          DF5=DF4
          S5=S4
          W4=W3
          DF4=DF3
          S4=S3
          W3=W2
          DF3=DF2
          S3=S2
          W2=0.0D0
          S2=0
          DF2=1
C
C
C     START DOING THE FUNCTION NAMES HERE
          OPT=-1
          IF(WC.EQ.'FUNC00  ') OPT=0
          IF(WC.EQ.'FUNC01  ') OPT=1
          IF(WC.EQ.'FUNC02  ') OPT=2
          IF(WC.EQ.'FUNC03  ') OPT=3
          IF(WC.EQ.'FUNC04  ') OPT=4
          IF(WC.EQ.'FUNC05  ') OPT=5
          IF(WC.EQ.'FUNC06  ') OPT=6
          IF(WC.EQ.'FUNC07  ') OPT=7
          IF(WC.EQ.'FUNC08  ') OPT=8
          IF(WC.EQ.'FUNC09  ') OPT=9
          IF(WC.EQ.'FUNC10  ') OPT=10
          IF(OPT.EQ.-1) THEN
              WRITE(OUTLYNE,*)
     1        'INVALID FUNCTION NAME COMMAND WORD'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
C     NUMERIC WORDS AND DEFAULTS
C
C     NW1 IS THE OPERAND NUMBER, DEFUALT WILL BE ORIGINAL VALUE
C
          IF(DF1.EQ.1) THEN
              WRITE(OUTLYNE,*)
     1        'EXPLICIT FOCRIT NUMBER MUST BE INPUT AS NUMERIC WORD #1'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(W1.LT.1.OR.W1.GT.5.0D0) THEN
              WRITE(OUTLYNE,*)
     1        'EXPLICIT FOCRIT NUMBER MUST BE IN THE RANGE 1 TO 5'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          OPWEIT=1.0D0
          IF(OPT.NE.0) THEN
C     NW3 IS GENERAL PURPOSE STORAGE REGISTER VALUE
              IF(DF3.EQ.1) THEN
                  IF(F54.EQ.1.OR.F54.EQ.2) THEN
                      WRITE(OUTLYNE,*)
     1                'NUMERIC WORD #2 (GP REGISTER ADDRESS) MUST BE EXPLICITLY INPUT'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'A USER DEFINED FOCRIT OPERAND'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              ELSE
C     NOT DEFAULT
                  IF((DBLE(DINT(W3))-W3).NE.0.0D0) THEN
                      WRITE(OUTLYNE,*)
     1                'NUMERIC WORD #2 (GP REGISTER ADDRESS) MUST BE AN INTEGER'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  IF(W3.LT.1.0D0.OR.W3.GT.400.0D0) THEN
                      WRITE(OUTLYNE,*)
     1                'NUMERIC WORD #2 (GP REGISTER ADDRESS) BEYOND LEGAL RANGE'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'VALID RANGE IS 1 TO 400'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  IF(DF3.NE.1.AND.W3.LT.0.0D0) THEN
                      WRITE(OUTLYNE,*)
     1                'NUMERIC WORD #2 (GP REGISTER ADDRESS) MAY NOT BE NEGATIVE'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  IREG=W3
                  OP8=W3
                  OP18=DBLE(DF3)
                  OP19=0.0D0
              END IF
          END IF

C     NW4 IS NW1 FOR FUNCTION NSUB FOR USER DEF OPS
C     NW5 IS NW2 FOR FUNCTION NSUB FOR USER DEF OPS
C     DEFAULT STATUS WILL BE HANDLED DURING FUNCTION CALLING
C     AND WILL BE PASSED IN ITEMS J=11 AND J=12
C
          IF(OPT.GE.1.AND.OPT.LE.10) GO TO 3141
C
          OPNM=WQ
          IF(OPNM.EQ.'X       ') OP17=1.0D0
          IF(OPNM.EQ.'Y       ') OP17=2.0D0
          IF(OPNM.EQ.'Z       ') OP17=3.0D0
          IF(OPNM.EQ.'DCL     ') OP17=4.0D0
          IF(OPNM.EQ.'K       ') OP17=4.0D0
          IF(OPNM.EQ.'DCM     ') OP17=5.0D0
          IF(OPNM.EQ.'L       ') OP17=5.0D0
          IF(OPNM.EQ.'DCN     ') OP17=6.0D0
          IF(OPNM.EQ.'M       ') OP17=6.0D0
          IF(OPNM.EQ.'DX      ') OP17=7.0D0
          IF(OPNM.EQ.'DY      ') OP17=8.0D0
          IF(OPNM.EQ.'DR      ') OP17=9.0D0
          IF(OPNM.EQ.'DXA     ') OP17=10.0D0
          IF(OPNM.EQ.'DYA     ') OP17=11.0D0
          IF(OPNM.EQ.'DRA     ') OP17=12.0D0
          IF(OPNM.EQ.'XANG    ') OP17=13.0D0
          IF(OPNM.EQ.'YANG    ') OP17=14.0D0
          IF(OPNM.EQ.'OPL     ') OP17=15.0D0
          IF(OPNM.EQ.'OPD     ') OP17=16.0D0
          IF(OPNM.EQ.'OPDW    ') OP17=17.0D0
          IF(OPNM.EQ.'LOLD    ') OP17=18.0D0
          IF(OPNM.EQ.'MOLD    ') OP17=19.0D0
          IF(OPNM.EQ.'NOLD    ') OP17=20.0D0
          IF(OPNM.EQ.'LEN     ') OP17=21.0D0
          IF(OPNM.EQ.'AII     ') OP17=22.0D0
          IF(OPNM.EQ.'AIP     ') OP17=23.0D0
          IF(OPNM.EQ.'LN      ') OP17=24.0D0
          IF(OPNM.EQ.'MN      ') OP17=25.0D0
          IF(OPNM.EQ.'NN      ') OP17=26.0D0
          IF(OPNM.EQ.'PXPX    ') OP17=27.0D0
          IF(OPNM.EQ.'PXPY    ') OP17=28.0D0
          IF(OPNM.EQ.'PYPX    ') OP17=29.0D0
          IF(OPNM.EQ.'PYPY    ') OP17=30.0D0
          IF(OPNM.EQ.'PXAPX   ') OP17=31.0D0
          IF(OPNM.EQ.'PXAPY   ') OP17=32.0D0
          IF(OPNM.EQ.'PYAPX   ') OP17=33.0D0
          IF(OPNM.EQ.'PYAPY   ') OP17=34.0D0
          IF(OPNM.EQ.'DXDX    ') OP17=35.0D0
          IF(OPNM.EQ.'DXDY    ') OP17=36.0D0
          IF(OPNM.EQ.'DYDX    ') OP17=37.0D0
          IF(OPNM.EQ.'DYDY    ') OP17=38.0D0
          IF(OPNM.EQ.'DXADX   ') OP17=39.0D0
          IF(OPNM.EQ.'DXADY   ') OP17=40.0D0
          IF(OPNM.EQ.'DYADX   ') OP17=41.0D0
          IF(OPNM.EQ.'DYADY   ') OP17=42.0D0
          IF(OPNM.EQ.'XREF    ') OP17=43.0D0
          IF(OPNM.EQ.'YREF    ') OP17=44.0D0
          IF(OPNM.EQ.'ZREF    ') OP17=45.0D0
          IF(OPNM.EQ.'LREF    ') OP17=46.0D0
          IF(OPNM.EQ.'MREF    ') OP17=47.0D0
          IF(OPNM.EQ.'NREF    ') OP17=48.0D0
          IF(OPNM.EQ.'LREFOL  ') OP17=49.0D0
          IF(OPNM.EQ.'MREFOL  ') OP17=50.0D0
          IF(OPNM.EQ.'NREFOL  ') OP17=51.0D0
          IF(OPNM.EQ.'IREF    ') OP17=52.0D0
          IF(OPNM.EQ.'IPREF   ') OP17=53.0D0
          IF(OPNM.EQ.'XAREF   ') OP17=54.0D0
          IF(OPNM.EQ.'YAREF   ') OP17=55.0D0
          IF(OPNM.EQ.'LNREF   ') OP17=56.0D0
          IF(OPNM.EQ.'MNREF   ') OP17=57.0D0
          IF(OPNM.EQ.'NNREF   ') OP17=58.0D0
          IF(OPNM.EQ.'GLX     ') OP17=59.0D0
          IF(OPNM.EQ.'GLY     ') OP17=60.0D0
          IF(OPNM.EQ.'GLZ     ') OP17=61.0D0
          IF(OPNM.EQ.'GLL     ') OP17=62.0D0
          IF(OPNM.EQ.'GLM     ') OP17=63.0D0
          IF(OPNM.EQ.'GLN     ') OP17=64.0D0
          IF(OPNM.EQ.'GLLOLD  ') OP17=65.0D0
          IF(OPNM.EQ.'GLMOLD  ') OP17=66.0D0
          IF(OPNM.EQ.'GLNOLD  ') OP17=67.0D0
          IF(OPNM.EQ.'LENREF  ') OP17=68.0D0
          IF(OPNM.EQ.'OPLREF  ') OP17=69.0D0
          IF(OPNM.EQ.'RD      ') OP17=70.0D0
          IF(OPNM.EQ.'CV      ') OP17=71.0D0
          IF(OPNM.EQ.'TH      ') OP17=72.0D0
          IF(OPNM.EQ.'CC      ') OP17=73.0D0
          IF(OPNM.EQ.'AC      ') OP17=74.0D0
          IF(OPNM.EQ.'AD      ') OP17=75.0D0
          IF(OPNM.EQ.'AE      ') OP17=76.0D0
          IF(OPNM.EQ.'AF      ') OP17=77.0D0
          IF(OPNM.EQ.'AG      ') OP17=78.0D0
          IF(OPNM.EQ.'RDTOR   ') OP17=79.0D0
          IF(OPNM.EQ.'CVTOR   ') OP17=80.0D0
          IF(OPNM.EQ.'CCTOR   ') OP17=81.0D0
          IF(OPNM.EQ.'ADTOR   ') OP17=82.0D0
          IF(OPNM.EQ.'AETOR   ') OP17=83.0D0
          IF(OPNM.EQ.'AFTOR   ') OP17=84.0D0
          IF(OPNM.EQ.'AGTOR   ') OP17=85.0D0
          IF(OPNM.EQ.'ALPHA   ') OP17=86.0D0
          IF(OPNM.EQ.'BETA    ') OP17=87.0D0
          IF(OPNM.EQ.'GAMMA   ') OP17=88.0D0
          IF(OPNM.EQ.'VNUM    ') OP17=89.0D0
          IF(OPNM.EQ.'PARTL   ') OP17=90.0D0
          IF(OPNM.EQ.'INDEX   ') OP17=91.0D0
          IF(OPNM.EQ.'XD      ') OP17=92.0D0
          IF(OPNM.EQ.'YD      ') OP17=93.0D0
          IF(OPNM.EQ.'XVERT   ') OP17=94.0D0
          IF(OPNM.EQ.'YVERT   ') OP17=95.0D0
          IF(OPNM.EQ.'ZVERT   ') OP17=96.0D0
          IF(OPNM.EQ.'LXVERT  ') OP17=97.0D0
          IF(OPNM.EQ.'MXVERT  ') OP17=98.0D0
          IF(OPNM.EQ.'NXVERT  ') OP17=99.0D0
          IF(OPNM.EQ.'LYVERT  ') OP17=100.0D0
          IF(OPNM.EQ.'MYVERT  ') OP17=101.0D0
          IF(OPNM.EQ.'NYVERT  ') OP17=102.0D0
          IF(OPNM.EQ.'LZVERT  ') OP17=103.0D0
          IF(OPNM.EQ.'MZVERT  ') OP17=104.0D0
          IF(OPNM.EQ.'NZVERT  ') OP17=105.0D0
          IF(OPNM.EQ.'LENGTH  ') OP17=106.0D0
          IF(OPNM.EQ.'OAL     ') OP17=106.0D0
          IF(OPNM.EQ.'MLENGTH ') OP17=107.0D0
          IF(OPNM.EQ.'OPTLEN  ') OP17=107.0D0
          IF(OPNM.EQ.'ET      ') OP17=108.0D0
          IF(OPNM.EQ.'ETY     ') OP17=108.0D0
          IF(OPNM.EQ.'ETX     ') OP17=109.0D0
          IF(OPNM.EQ.'SHAPEFAC') OP17=110.0D0
          IF(OPNM.EQ.'C1      ') OP17=111.0D0
          IF(OPNM.EQ.'C2      ') OP17=112.0D0
          IF(OPNM.EQ.'C3      ') OP17=113.0D0
          IF(OPNM.EQ.'C4      ') OP17=114.0D0
          IF(OPNM.EQ.'C5      ') OP17=115.0D0
          IF(OPNM.EQ.'C6      ') OP17=116.0D0
          IF(OPNM.EQ.'C7      ') OP17=117.0D0
          IF(OPNM.EQ.'C8      ') OP17=118.0D0
          IF(OPNM.EQ.'C9      ') OP17=119.0D0
          IF(OPNM.EQ.'C10     ') OP17=120.0D0
          IF(OPNM.EQ.'C11     ') OP17=121.0D0
          IF(OPNM.EQ.'C12     ') OP17=122.0D0
          IF(OPNM.EQ.'C13     ') OP17=123.0D0
          IF(OPNM.EQ.'C14     ') OP17=124.0D0
          IF(OPNM.EQ.'C15     ') OP17=125.0D0
          IF(OPNM.EQ.'C16     ') OP17=126.0D0
          IF(OPNM.EQ.'C17     ') OP17=127.0D0
          IF(OPNM.EQ.'C18     ') OP17=128.0D0
          IF(OPNM.EQ.'C19     ') OP17=129.0D0
          IF(OPNM.EQ.'C20     ') OP17=130.0D0
          IF(OPNM.EQ.'C21     ') OP17=131.0D0
          IF(OPNM.EQ.'C22     ') OP17=132.0D0
          IF(OPNM.EQ.'C23     ') OP17=133.0D0
          IF(OPNM.EQ.'C24     ') OP17=134.0D0
          IF(OPNM.EQ.'C25     ') OP17=135.0D0
          IF(OPNM.EQ.'C26     ') OP17=136.0D0
          IF(OPNM.EQ.'C27     ') OP17=137.0D0
          IF(OPNM.EQ.'C28     ') OP17=138.0D0
          IF(OPNM.EQ.'C29     ') OP17=139.0D0
          IF(OPNM.EQ.'C30     ') OP17=140.0D0
          IF(OPNM.EQ.'C31     ') OP17=141.0D0
          IF(OPNM.EQ.'C32     ') OP17=142.0D0
          IF(OPNM.EQ.'C33     ') OP17=143.0D0
          IF(OPNM.EQ.'C34     ') OP17=144.0D0
          IF(OPNM.EQ.'C35     ') OP17=145.0D0
          IF(OPNM.EQ.'C36     ') OP17=146.0D0
          IF(OPNM.EQ.'C37     ') OP17=147.0D0
          IF(OPNM.EQ.'C38     ') OP17=148.0D0
          IF(OPNM.EQ.'C39     ') OP17=149.0D0
          IF(OPNM.EQ.'C40     ') OP17=150.0D0
          IF(OPNM.EQ.'C41     ') OP17=151.0D0
          IF(OPNM.EQ.'C42     ') OP17=152.0D0
          IF(OPNM.EQ.'C43     ') OP17=153.0D0
          IF(OPNM.EQ.'C44     ') OP17=154.0D0
          IF(OPNM.EQ.'C45     ') OP17=155.0D0
          IF(OPNM.EQ.'C46     ') OP17=156.0D0
          IF(OPNM.EQ.'C47     ') OP17=157.0D0
          IF(OPNM.EQ.'C48     ') OP17=158.0D0
          IF(OPNM.EQ.'C49     ') OP17=159.0D0
          IF(OPNM.EQ.'C50     ') OP17=160.0D0
          IF(OPNM.EQ.'C51     ') OP17=161.0D0
          IF(OPNM.EQ.'C52     ') OP17=162.0D0
          IF(OPNM.EQ.'C53     ') OP17=163.0D0
          IF(OPNM.EQ.'C54     ') OP17=164.0D0
          IF(OPNM.EQ.'C55     ') OP17=165.0D0
          IF(OPNM.EQ.'C56     ') OP17=166.0D0
          IF(OPNM.EQ.'C57     ') OP17=167.0D0
          IF(OPNM.EQ.'C58     ') OP17=168.0D0
          IF(OPNM.EQ.'C59     ') OP17=169.0D0
          IF(OPNM.EQ.'C60     ') OP17=170.0D0
          IF(OPNM.EQ.'C61     ') OP17=171.0D0
          IF(OPNM.EQ.'C62     ') OP17=172.0D0
          IF(OPNM.EQ.'C63     ') OP17=173.0D0
          IF(OPNM.EQ.'C64     ') OP17=174.0D0
          IF(OPNM.EQ.'C65     ') OP17=175.0D0
          IF(OPNM.EQ.'C66     ') OP17=176.0D0
          IF(OPNM.EQ.'C67     ') OP17=177.0D0
          IF(OPNM.EQ.'C68     ') OP17=178.0D0
          IF(OPNM.EQ.'C69     ') OP17=179.0D0
          IF(OPNM.EQ.'C70     ') OP17=180.0D0
          IF(OPNM.EQ.'C71     ') OP17=181.0D0
          IF(OPNM.EQ.'C72     ') OP17=182.0D0
          IF(OPNM.EQ.'C73     ') OP17=183.0D0
          IF(OPNM.EQ.'C74     ') OP17=184.0D0
          IF(OPNM.EQ.'C75     ') OP17=185.0D0
          IF(OPNM.EQ.'C76     ') OP17=186.0D0
          IF(OPNM.EQ.'C77     ') OP17=187.0D0
          IF(OPNM.EQ.'C78     ') OP17=188.0D0
          IF(OPNM.EQ.'C79     ') OP17=189.0D0
          IF(OPNM.EQ.'C80     ') OP17=190.0D0
          IF(OPNM.EQ.'C81     ') OP17=191.0D0
          IF(OPNM.EQ.'C82     ') OP17=192.0D0
          IF(OPNM.EQ.'C83     ') OP17=193.0D0
          IF(OPNM.EQ.'C84     ') OP17=194.0D0
          IF(OPNM.EQ.'C85     ') OP17=195.0D0
          IF(OPNM.EQ.'C86     ') OP17=196.0D0
          IF(OPNM.EQ.'C87     ') OP17=197.0D0
          IF(OPNM.EQ.'C88     ') OP17=198.0D0
          IF(OPNM.EQ.'C89     ') OP17=199.0D0
          IF(OPNM.EQ.'C90     ') OP17=200.0D0
          IF(OPNM.EQ.'C91     ') OP17=201.0D0
          IF(OPNM.EQ.'C92     ') OP17=202.0D0
          IF(OPNM.EQ.'C93     ') OP17=203.0D0
          IF(OPNM.EQ.'C94     ') OP17=204.0D0
          IF(OPNM.EQ.'C95     ') OP17=205.0D0
          IF(OPNM.EQ.'C96     ') OP17=206.0D0
          IF(OPNM.EQ.'PWRY    ') OP17=207.0D0
          IF(OPNM.EQ.'PWRX    ') OP17=208.0D0
          IF(OPNM.EQ.'FLCLTHX ') OP17=209.0D0
          IF(OPNM.EQ.'FLCLTH  ') OP17=210.0D0
          IF(OPNM.EQ.'FLCLTHY ') OP17=210.0D0
          IF(OPNM.EQ.'PY      ') OP17=211.0D0
          IF(OPNM.EQ.'PX      ') OP17=212.0D0
          IF(OPNM.EQ.'PCY     ') OP17=213.0D0
          IF(OPNM.EQ.'PCX     ') OP17=214.0D0
          IF(OPNM.EQ.'PUY     ') OP17=215.0D0
          IF(OPNM.EQ.'PUX     ') OP17=216.0D0
          IF(OPNM.EQ.'PUCY    ') OP17=217.0D0
          IF(OPNM.EQ.'PUCX    ') OP17=218.0D0
          IF(OPNM.EQ.'PIY     ') OP17=219.0D0
          IF(OPNM.EQ.'PIX     ') OP17=220.0D0
          IF(OPNM.EQ.'PICY    ') OP17=221.0D0
          IF(OPNM.EQ.'PICX    ') OP17=222.0D0
          IF(OPNM.EQ.'PIYP    ') OP17=223.0D0
          IF(OPNM.EQ.'PIXP    ') OP17=224.0D0
          IF(OPNM.EQ.'PICYP   ') OP17=225.0D0
          IF(OPNM.EQ.'PICXP   ') OP17=226.0D0
          IF(OPNM.EQ.'PACY    ') OP17=227.0D0
          IF(OPNM.EQ.'PACX    ') OP17=228.0D0
          IF(OPNM.EQ.'PLCY    ') OP17=229.0D0
          IF(OPNM.EQ.'PLCX    ') OP17=230.0D0
          IF(OPNM.EQ.'SACY    ') OP17=231.0D0
          IF(OPNM.EQ.'SACX    ') OP17=232.0D0
          IF(OPNM.EQ.'SLCY    ') OP17=233.0D0
          IF(OPNM.EQ.'SLCX    ') OP17=234.0D0
          IF(OPNM.EQ.'IMDISX  ') OP17=235.0D0
          IF(OPNM.EQ.'IMDISY  ') OP17=236.0D0
          IF(OPNM.EQ.'CENTX   ') OP17=237.0D0
          IF(OPNM.EQ.'CENTY   ') OP17=238.0D0
          IF(OPNM.EQ.'RMSX    ') OP17=239.0D0
          IF(OPNM.EQ.'RMSY    ') OP17=240.0D0
          IF(OPNM.EQ.'RMS     ') OP17=241.0D0
          IF(OPNM.EQ.'RSSX    ') OP17=242.0D0
          IF(OPNM.EQ.'RSSY    ') OP17=243.0D0
          IF(OPNM.EQ.'RSS     ') OP17=244.0D0
          IF(OPNM.EQ.'RMSOPD  ') OP17=245.0D0
          IF(OPNM.EQ.'ZERN37  ') OP17=246.0D0
          IF(OPNM.EQ.'MAGX    ') OP17=247.0D0
          IF(OPNM.EQ.'MAGY    ') OP17=248.0D0
          IF(OPNM.EQ.'MAGXOR  ') OP17=249.0D0
          IF(OPNM.EQ.'MAGYOR  ') OP17=250.0D0
          IF(OPNM.EQ.'FFLX    ') OP17=251.0D0
          IF(OPNM.EQ.'FFLY    ') OP17=252.0D0
          IF(OPNM.EQ.'BFLX    ') OP17=253.0D0
          IF(OPNM.EQ.'BFLY    ') OP17=254.0D0
          IF(OPNM.EQ.'FFNX    ') OP17=255.0D0
          IF(OPNM.EQ.'FFNY    ') OP17=256.0D0
          IF(OPNM.EQ.'BFNX    ') OP17=257.0D0
          IF(OPNM.EQ.'BFNY    ') OP17=258.0D0
          IF(OPNM.EQ.'EFLX    ') OP17=259.0D0
          IF(OPNM.EQ.'EFLY    ') OP17=260.0D0
          IF(OPNM.EQ.'ENDIAX  ') OP17=261.0D0
          IF(OPNM.EQ.'ENDIAY  ') OP17=262.0D0
          IF(OPNM.EQ.'EXDIAX  ') OP17=263.0D0
          IF(OPNM.EQ.'EXDIAY  ') OP17=264.0D0
          IF(OPNM.EQ.'ENPOSX  ') OP17=265.0D0
          IF(OPNM.EQ.'ENPOSY  ') OP17=266.0D0
          IF(OPNM.EQ.'ENPOSZ  ') OP17=267.0D0
          IF(OPNM.EQ.'EXPOSX  ') OP17=268.0D0
          IF(OPNM.EQ.'EXPOSY  ') OP17=269.0D0
          IF(OPNM.EQ.'EXPOSZ  ') OP17=270.0D0
          IF(OPNM.EQ.'FNUMX   ') OP17=271.0D0
          IF(OPNM.EQ.'FNYMY   ') OP17=272.0D0
          IF(OPNM.EQ.'OBFNUMX ') OP17=273.0D0
          IF(OPNM.EQ.'OBFNUMY ') OP17=274.0D0
          IF(OPNM.EQ.'ENPDIAX ') OP17=275.0D0
          IF(OPNM.EQ.'ENPDIAY ') OP17=276.0D0
          IF(OPNM.EQ.'EXPDIAX ') OP17=277.0D0
          IF(OPNM.EQ.'EXPDIAY ') OP17=278.0D0
          IF(OPNM.EQ.'PUPDIAX ') OP17=279.0D0
          IF(OPNM.EQ.'PUPDIAY ') OP17=280.0D0
          IF(OPNM.EQ.'PUPDISX ') OP17=281.0D0
          IF(OPNM.EQ.'PUPDISY ') OP17=282.0D0
          IF(OPNM.EQ.'CHFIMX  ') OP17=283.0D0
          IF(OPNM.EQ.'CHFIMY  ') OP17=284.0D0
          IF(OPNM.EQ.'GPX     ') OP17=285.0D0
          IF(OPNM.EQ.'GPY     ') OP17=286.0D0
          IF(OPNM.EQ.'GPUX    ') OP17=287.0D0
          IF(OPNM.EQ.'GPUY    ') OP17=288.0D0
          IF(OPNM.EQ.'GPCX    ') OP17=289.0D0
          IF(OPNM.EQ.'GPCY    ') OP17=290.0D0
          IF(OPNM.EQ.'GPUCX   ') OP17=291.0D0
          IF(OPNM.EQ.'GPUCY   ') OP17=292.0D0
          IF(OPNM.EQ.'DIST    ') OP17=293.0D0
          IF(OPNM.EQ.'XFOC    ') OP17=294.0D0
          IF(OPNM.EQ.'YFOC    ') OP17=295.0D0
          IF(OPNM.EQ.'AST     ') OP17=296.0D0
          IF(OPNM.EQ.'SA3     ') OP17=297.0D0
          IF(OPNM.EQ.'XSA3    ') OP17=298.0D0
          IF(OPNM.EQ.'CMA3    ') OP17=299.0D0
          IF(OPNM.EQ.'XCMA3   ') OP17=300.0D0
          IF(OPNM.EQ.'AST3    ') OP17=301.0D0
          IF(OPNM.EQ.'XAST3   ') OP17=302.0D0
          IF(OPNM.EQ.'DIS3    ') OP17=303.0D0
          IF(OPNM.EQ.'XDIS3   ') OP17=304.0D0
          IF(OPNM.EQ.'PTZ3    ') OP17=305.0D0
          IF(OPNM.EQ.'XPTZ3   ') OP17=306.0D0
          IF(OPNM.EQ.'SA5     ') OP17=307.0D0
          IF(OPNM.EQ.'XSA5    ') OP17=308.0D0
          IF(OPNM.EQ.'CMA5    ') OP17=309.0D0
          IF(OPNM.EQ.'XCMA5   ') OP17=310.0D0
          IF(OPNM.EQ.'AST5    ') OP17=311.0D0
          IF(OPNM.EQ.'XAST5   ') OP17=312.0D0
          IF(OPNM.EQ.'DIS5    ') OP17=313.0D0
          IF(OPNM.EQ.'XDIS5   ') OP17=314.0D0
          IF(OPNM.EQ.'PTZ5    ') OP17=315.0D0
          IF(OPNM.EQ.'XPTZ5   ') OP17=316.0D0
          IF(OPNM.EQ.'TOBSA   ') OP17=317.0D0
          IF(OPNM.EQ.'XTOBSA  ') OP17=318.0D0
          IF(OPNM.EQ.'SOBSA   ') OP17=319.0D0
          IF(OPNM.EQ.'XSOBSA  ') OP17=320.0D0
          IF(OPNM.EQ.'ELCMA   ') OP17=321.0D0
          IF(OPNM.EQ.'XELCMA  ') OP17=322.0D0
          IF(OPNM.EQ.'TAS     ') OP17=323.0D0
          IF(OPNM.EQ.'XTAS    ') OP17=324.0D0
          IF(OPNM.EQ.'SAS     ') OP17=325.0D0
          IF(OPNM.EQ.'XSAS    ') OP17=326.0D0
          IF(OPNM.EQ.'SA7     ') OP17=327.0D0
          IF(OPNM.EQ.'XSA7    ') OP17=328.0D0
          IF(OPNM.EQ.'SA3P    ') OP17=329.0D0
          IF(OPNM.EQ.'XSA3P   ') OP17=330.0D0
          IF(OPNM.EQ.'CMA3P   ') OP17=331.0D0
          IF(OPNM.EQ.'XCMA3P  ') OP17=332.0D0
          IF(OPNM.EQ.'AST3P   ') OP17=333.0D0
          IF(OPNM.EQ.'XAST3P  ') OP17=334.0D0
          IF(OPNM.EQ.'DIS3P   ') OP17=335.0D0
          IF(OPNM.EQ.'XDIS3P  ') OP17=336.0D0
          IF(OPNM.EQ.'PTZ3P   ') OP17=337.0D0
          IF(OPNM.EQ.'XPTZ3P  ') OP17=338.0D0
          IF(OPNM.EQ.'SA5P    ') OP17=339.0D0
          IF(OPNM.EQ.'XSA5P   ') OP17=340.0D0
          IF(OPNM.EQ.'CMA5P   ') OP17=341.0D0
          IF(OPNM.EQ.'XCMA5P  ') OP17=342.0D0
          IF(OPNM.EQ.'AST5P   ') OP17=343.0D0
          IF(OPNM.EQ.'XAST5P  ') OP17=344.0D0
          IF(OPNM.EQ.'DIS5P   ') OP17=345.0D0
          IF(OPNM.EQ.'XDIS5P  ') OP17=346.0D0
          IF(OPNM.EQ.'PTZ5P   ') OP17=347.0D0
          IF(OPNM.EQ.'XPTZ5P  ') OP17=348.0D0
          IF(OPNM.EQ.'TOBSAP  ') OP17=349.0D0
          IF(OPNM.EQ.'XTOBSAP ') OP17=350.0D0
          IF(OPNM.EQ.'SOBSAP  ') OP17=351.0D0
          IF(OPNM.EQ.'XSOBSAP ') OP17=352.0D0
          IF(OPNM.EQ.'ELCMAP  ') OP17=353.0D0
          IF(OPNM.EQ.'XELCMAP ') OP17=354.0D0
          IF(OPNM.EQ.'TASP    ') OP17=355.0D0
          IF(OPNM.EQ.'XTASP   ') OP17=356.0D0
          IF(OPNM.EQ.'SASP    ') OP17=357.0D0
          IF(OPNM.EQ.'XSASP   ') OP17=358.0D0
          IF(OPNM.EQ.'SA7P    ') OP17=359.0D0
          IF(OPNM.EQ.'XSA7P   ') OP17=360.0D0
          IF(OPNM.EQ.'SA3S    ') OP17=361.0D0
          IF(OPNM.EQ.'XSA3S   ') OP17=362.0D0
          IF(OPNM.EQ.'CMA3S   ') OP17=363.0D0
          IF(OPNM.EQ.'XCMA3S  ') OP17=364.0D0
          IF(OPNM.EQ.'AST3S   ') OP17=365.0D0
          IF(OPNM.EQ.'XAST3S  ') OP17=366.0D0
          IF(OPNM.EQ.'DIS3S   ') OP17=367.0D0
          IF(OPNM.EQ.'XDIS3S  ') OP17=368.0D0
          IF(OPNM.EQ.'PTZ3S   ') OP17=369.0D0
          IF(OPNM.EQ.'XPTZ3S  ') OP17=370.0D0
          IF(OPNM.EQ.'SA5S    ') OP17=371.0D0
          IF(OPNM.EQ.'XSA5S   ') OP17=372.0D0
          IF(OPNM.EQ.'CMA5S   ') OP17=373.0D0
          IF(OPNM.EQ.'XCMA5S  ') OP17=374.0D0
          IF(OPNM.EQ.'AST5S   ') OP17=375.0D0
          IF(OPNM.EQ.'XAST5S  ') OP17=376.0D0
          IF(OPNM.EQ.'DIS5S   ') OP17=377.0D0
          IF(OPNM.EQ.'XDIS5S  ') OP17=378.0D0
          IF(OPNM.EQ.'PTZ5S   ') OP17=379.0D0
          IF(OPNM.EQ.'XPTZ5S  ') OP17=380.0D0
          IF(OPNM.EQ.'TOBSAS  ') OP17=381.0D0
          IF(OPNM.EQ.'XTOBSAS ') OP17=382.0D0
          IF(OPNM.EQ.'SOBSAS  ') OP17=383.0D0
          IF(OPNM.EQ.'XSOBSAS ') OP17=384.0D0
          IF(OPNM.EQ.'ELCMAS  ') OP17=385.0D0
          IF(OPNM.EQ.'XELCMAS ') OP17=386.0D0
          IF(OPNM.EQ.'TASS    ') OP17=387.0D0
          IF(OPNM.EQ.'XTASS   ') OP17=388.0D0
          IF(OPNM.EQ.'SASS    ') OP17=389.0D0
          IF(OPNM.EQ.'XSASS   ') OP17=390.0D0
          IF(OPNM.EQ.'SA7S    ') OP17=391.0D0
          IF(OPNM.EQ.'XSA7S   ') OP17=392.0D0
          IF(OPNM.EQ.'SA5I    ') OP17=393.0D0
          IF(OPNM.EQ.'XSA5I   ') OP17=394.0D0
          IF(OPNM.EQ.'CMA5I   ') OP17=395.0D0
          IF(OPNM.EQ.'XCMA5I  ') OP17=396.0D0
          IF(OPNM.EQ.'AST5I   ') OP17=397.0D0
          IF(OPNM.EQ.'XAST5I  ') OP17=398.0D0
          IF(OPNM.EQ.'DIS5I   ') OP17=399.0D0
          IF(OPNM.EQ.'XDIS5I  ') OP17=400.0D0
          IF(OPNM.EQ.'PTZ5I   ') OP17=401.0D0
          IF(OPNM.EQ.'XPTZ5I  ') OP17=402.0D0
          IF(OPNM.EQ.'TOBSAI  ') OP17=403.0D0
          IF(OPNM.EQ.'XTOBSAI ') OP17=404.0D0
          IF(OPNM.EQ.'SOBSAI  ') OP17=405.0D0
          IF(OPNM.EQ.'XSOBSAI ') OP17=406.0D0
          IF(OPNM.EQ.'ELCMAI  ') OP17=407.0D0
          IF(OPNM.EQ.'XELCMAI ') OP17=408.0D0
          IF(OPNM.EQ.'TASI    ') OP17=409.0D0
          IF(OPNM.EQ.'XTASI   ') OP17=410.0D0
          IF(OPNM.EQ.'SASI    ') OP17=411.0D0
          IF(OPNM.EQ.'XSASI   ') OP17=412.0D0
          IF(OPNM.EQ.'SA7I    ') OP17=413.0D0
          IF(OPNM.EQ.'XSA7I   ') OP17=414.0D0
          IF(OPNM.EQ.'PSA3    ') OP17=415.0D0
          IF(OPNM.EQ.'XPSA3   ') OP17=416.0D0
          IF(OPNM.EQ.'PCMA3   ') OP17=417.0D0
          IF(OPNM.EQ.'XPCMA3  ') OP17=418.0D0
          IF(OPNM.EQ.'PAST3   ') OP17=419.0D0
          IF(OPNM.EQ.'XPAST3  ') OP17=420.0D0
          IF(OPNM.EQ.'PDIS3   ') OP17=421.0D0
          IF(OPNM.EQ.'XPDIS3  ') OP17=422.0D0
          IF(OPNM.EQ.'PPTZ3   ') OP17=423.0D0
          IF(OPNM.EQ.'XPPTZ3  ') OP17=424.0D0
          IF(OPNM.EQ.'PSA3P   ') OP17=425.0D0
          IF(OPNM.EQ.'XPSA3P  ') OP17=426.0D0
          IF(OPNM.EQ.'PCMA3P  ') OP17=427.0D0
          IF(OPNM.EQ.'XPCMA3P ') OP17=428.0D0
          IF(OPNM.EQ.'PAST3P  ') OP17=429.0D0
          IF(OPNM.EQ.'XPAST3P ') OP17=430.0D0
          IF(OPNM.EQ.'PDIS3P  ') OP17=431.0D0
          IF(OPNM.EQ.'XPDIS3P ') OP17=432.0D0
          IF(OPNM.EQ.'PPTZ3P  ') OP17=433.0D0
          IF(OPNM.EQ.'XPPTZ3P ') OP17=434.0D0
          IF(OPNM.EQ.'PSA3S   ') OP17=435.0D0
          IF(OPNM.EQ.'XPSA3S  ') OP17=436.0D0
          IF(OPNM.EQ.'PCMA3S  ') OP17=437.0D0
          IF(OPNM.EQ.'XPCMA3S ') OP17=438.0D0
          IF(OPNM.EQ.'PAST3S  ') OP17=439.0D0
          IF(OPNM.EQ.'XPAST3S ') OP17=430.0D0
          IF(OPNM.EQ.'PDIS3S  ') OP17=431.0D0
          IF(OPNM.EQ.'XPDIS3S ') OP17=442.0D0
          IF(OPNM.EQ.'PPTZ3S  ') OP17=443.0D0
          IF(OPNM.EQ.'XPPTZ3S ') OP17=444.0D0
          IF(OPNM.EQ.'PTZCV   ') OP17=445.0D0
          IF(OPNM.EQ.'XPTZCV  ') OP17=446.0D0
          IF(OPNM.EQ.'AH      ') OP17=447.0D0
          IF(OPNM.EQ.'AI      ') OP17=448.0D0
          IF(OPNM.EQ.'AJ      ') OP17=449.0D0
          IF(OPNM.EQ.'AK      ') OP17=450.0D0
          IF(OPNM.EQ.'AL      ') OP17=451.0D0
          IF(OPNM.EQ.'GBRADX  ') OP17=452.0D0
          IF(OPNM.EQ.'GBRADY  ') OP17=453.0D0
          IF(OPNM.EQ.'GBDISX  ') OP17=454.0D0
          IF(OPNM.EQ.'GBDISY  ') OP17=455.0D0
          IF(OPNM.EQ.'GBRCVX  ') OP17=456.0D0
          IF(OPNM.EQ.'GBRCVY  ') OP17=457.0D0
          IF(OPNM.EQ.'GBWAISTX') OP17=458.0D0
          IF(OPNM.EQ.'GBWAISTY') OP17=459.0D0
          IF(OPNM.EQ.'MGOTF')    OP17=460.0D0
          IF(OPNM.EQ.'PGOTF')    OP17=461.0D0
          IF(OPNM.EQ.'MDOTF')    OP17=462.0D0
          IF(OPNM.EQ.'PDOTF')    OP17=463.0D0
          IF(OPNM.EQ.'GOTFM')    OP17=460.0D0
          IF(OPNM.EQ.'GOTFP')    OP17=461.0D0
          IF(OPNM.EQ.'DOTFM')    OP17=462.0D0
          IF(OPNM.EQ.'DOTFP')    OP17=463.0D0
          IF(OPNM.EQ.'RED')      OP17=464.0D0
          IF(OPNM.EQ.'REDCEN')   OP17=465.0D0
          IF(OPNM.EQ.'FISHDIST') OP17=466.0D0
          IF(OPNM.EQ.'ZD')       OP17=467.0D0
          IF(OPNM.EQ.'SYMX')     OP17=468.0D0
          IF(OPNM.EQ.'SYMY')     OP17=469.0D0
          IF(OPNM.EQ.'ASYMX')    OP17=470.0D0
          IF(OPNM.EQ.'ASYMY')    OP17=471.0D0
          IF(OPNM.EQ.'PACM')     OP17=472.0D0
          IF(OPNM.EQ.'PACZ')     OP17=473.0D0
          IF(OPNM.EQ.'SACM')     OP17=474.0D0
          IF(OPNM.EQ.'SACZ')     OP17=475.0D0
          IF(OPNM.EQ.'PLCM')     OP17=476.0D0
          IF(OPNM.EQ.'PLCZ')     OP17=477.0D0
          IF(OPNM.EQ.'SLCM')     OP17=478.0D0
          IF(OPNM.EQ.'SLCZ')     OP17=479.0D0
          IF(OPNM.EQ.'CTSX')     OP17=480.0D0
          IF(OPNM.EQ.'CTSY')     OP17=481.0D0
          IF(OPNM.EQ.'SCEX')     OP17=482.0D0
          IF(OPNM.EQ.'SCEY')     OP17=483.0D0
          IF(OPNM.EQ.'GREYS')    OP17=484.0D0
          IF(OPNM.EQ.'PIVX')     OP17=485.0D0
          IF(OPNM.EQ.'PIVY')     OP17=486.0D0
          IF(OPNM.EQ.'PIVZ')     OP17=487.0D0
          IF(OPNM.EQ.'N1')       OP17=488.0D0
          IF(OPNM.EQ.'N2')       OP17=489.0D0
          IF(OPNM.EQ.'N3')       OP17=490.0D0
          IF(OPNM.EQ.'N4')       OP17=491.0D0
          IF(OPNM.EQ.'N5')       OP17=492.0D0
          IF(OPNM.EQ.'N6')       OP17=493.0D0
          IF(OPNM.EQ.'N7')       OP17=494.0D0
          IF(OPNM.EQ.'N8')       OP17=495.0D0
          IF(OPNM.EQ.'N9')       OP17=496.0D0
          IF(OPNM.EQ.'N10')      OP17=497.0D0
          IF(OPNM.EQ.'ABBE')     OP17=498.0D0
          IF(OPNM.EQ.'DPART')    OP17=499.0D0
          IF(OPNM.EQ.'CLPX')     OP17=500.0D0
          IF(OPNM.EQ.'CLPY')     OP17=501.0D0
          IF(OPNM.EQ.'GDX')      OP17=502.0D0
          IF(OPNM.EQ.'GDY')      OP17=503.0D0
          IF(OPNM.EQ.'GDZ')      OP17=504.0D0
          IF(OPNM.EQ.'GALPHA')   OP17=505.0D0
          IF(OPNM.EQ.'GBETA')    OP17=506.0D0
          IF(OPNM.EQ.'GGAMMA')   OP17=507.0D0
          IF(OPNM.EQ.'GRS')      OP17=508.0D0
          IF(OPNM.EQ.'WEIGHT')   OP17=509.0D0
          IF(OPNM.EQ.'DMINUSD')  OP17=510.0D0
          IF(OPNM.EQ.'COST')     OP17=511.0D0
          IF(OPNM.EQ.'MACOPT')   OP17=512.0D0
          IF(OPNM.EQ.'RMSYX')    OP17=513.0D0
C
          IF(OP17.EQ.512.0D0) THEN
C     MACOPT OPERAND
              IF(INT(W3).LT.1.OR.INT(W3).GT.1000) THEN
                  WRITE(OUTLYNE,*)
     1            'PREDEFINED OPERAND ',OPNM,' REQUIRES'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)
     1            '"NUMERIC WORD #2 IN THE RANGE 1 TO 1000'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IREG=W3
              OP8 =IREG
              OP18 =DBLE(DF3)
              OP9=W4
              OP11=DBLE(DF4)
              OP10 =W5
              OP12 =DBLE(DF5)
          END IF
C
          IF(OP17.GE.480.0D0.AND.OP17.LE.481.0D0) THEN
C     CTSX AND CTSY OPERANDS
              IF(DF5.EQ.0.AND.W5.NE.0.0D0) THEN
                  WRITE(OUTLYNE,*)
     1            'PREDEFINED OPERAND ',OPNM
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)
     1            'TAKES NO NUMERIC WORD #4 INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IREG=W3
              OP8 =IREG
              OP18 =DBLE(DF3)
              OP9=W4
              OP11=DBLE(DF4)
              OP10 =W5
              OP12 =DBLE(DF5)
          END IF
C
          IF(OP17.GE.482.0D0.AND.OP17.LE.483.0D0) THEN
C     SCEX AND SCEY OPERANDS
              IF(DF4.EQ.0.AND.W4.NE.0.0D0.OR.DF5.EQ.0.AND.W5.NE.0.0D0) THEN
                  WRITE(OUTLYNE,*)
     1            'PREDEFINED OPERAND ',OPNM
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)
     1            'TAKES NO NUMERIC WORD #3 OR #4 INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IREG=W3
              OP8 =IREG
              OP18 =DBLE(DF3)
              OP9=W4
              OP11=DBLE(DF4)
              OP10 =W5
              OP12 =DBLE(DF5)
          END IF
C
C
          IF(OP17.GE.472.0D0.AND.OP17.LE.479.0D0.OR.OP17.EQ.479.0D0
     1    .OR.OP17.EQ.510.0D0) THEN
C     REAL RAY COLOR OPERANDS, NO INPUT NEEDED TO DEFINE
C     FRACTIONAL OBJECT POS., REL RAY POS OR COLORS.
              IF(DF3.EQ.0.OR.DF4.EQ.0.OR.DF5.EQ.0) THEN
                  WRITE(OUTLYNE,*)
     1            'PREDEFINED OPERAND ',OPNM
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)
     1            'ONLY TAKES NUMERIC WORD #1 INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IREG=W3
              OP8 =IREG
              OP18 =DBLE(DF3)
              OP9=W4
              OP11=DBLE(DF4)
              OP10 =W5
              OP12 =DBLE(DF5)
          END IF
C     SYMX,SYMY,ASYMX AND ASYMY
          IF(OP17.GE.468.0D0.AND.OP17.LE.471.0D0) THEN
C     W3 (CALLED W2) ANY VALUE MAY BE INPUT ( ONLY ABS VALUE USED)
              IF(DF3.EQ.1) THEN
                  DF3=0
                  W3=0.7D0
              END IF
              W3=DABS(W3)
C     W4 (CALLED W3) 1 TO 200 (FIELD NUMBER)
              IF(DF4.EQ.1) THEN
                  DF4=0
                  W4=1.0D0
              END IF
C     W5 (CALLED W4) 1 TO 10 (WAVELENGTH NUMBER)
              IF(DF5.EQ.1) THEN
                  DF5=0
                  W5=SYSTEM1(11)
              END IF
C     OUT OF RANGE INPUT W4
              IF(INT(W4).LT.1.OR.INT(W4).GT.200.0D0) THEN
                  WRITE(OUTLYNE,*)
     1            'FIELD POSITION NUMBER BEYOND LEGAL RANGE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)
     1            'FOR PREDEFINED OPERAND ',OPNM
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C     OUT OF RANGE INPUT W5
              IF(INT(W5).LT.1.OR.INT(W5).GT.10.0D0) THEN
                  WRITE(OUTLYNE,*)
     1            'WAVELENGTH NUMBER BEYOND LEGAL RANGE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)
     1            'FOR PREDEFINED OPERAND ',OPNM
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IREG=W3
              OP8 =IREG
              OP18 =DBLE(DF3)
              OP9=W4
              OP11=DBLE(DF4)
              OP10 =W5
              OP12 =DBLE(DF5)
          END IF
C
C
          IF(OP17.GE.1.0D0.AND.OP17.LE.15.0D0.OR.OP17.GE.18.0D0.AND.OP17
     1    .LE.69.0D0) THEN
C
C     RAY BASED PREDEFINED OPERANDS
C
C
              IF(OP17.EQ.68.0D0.OR.OP17.EQ.69.0D0) THEN
                  IF(W3.LT.1.0D0) THEN
                      WRITE(OUTLYNE,*)
     1                'PREDEFINED OPERAND ',OPNM
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'REQUIRES NUMERIC WORD #2 INPUT GREATER THAN 0'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
C
C     DEFAULT NW3
              IF(OP17.GE.1.0D0.AND.OP17.LE.14.0D0.OR.
     1        OP17.GE.18.0D0.AND.OP17.LE.20.0D0.OR.
     2        OP17.GE.22.0D0.AND.OP17.LE.23.0D0.OR.
     3        OP17.GE.27.0D0.AND.OP17.LE.55.0D0) THEN
                  IF(DF3.EQ.1) THEN
                      W3=DBLE(NEWIMG)
                      DF3=0
                      IREG=W3
                      OP8 =IREG
                      OP18 =DBLE(DF3)
                  END IF
              END IF
C
              IF(OP17.EQ.15.0D0.OR.
     1        OP17.EQ.21D0.OR.
     2        OP17.GE.24.0D0.AND.OP17.LE.26.0D0.OR.
     3        OP17.GE.56.0D0.AND.OP17.LE.69.0D0) THEN
                  IF(DF3.EQ.1) THEN
                      WRITE(OUTLYNE,*)
     1                'PREDEFINED OPERAND ',OPNM
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'REQUIRES EXPLICIT NUMERIC WORD #2 INPUT'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
C
              IF(OP17.GE.43.AND.OP17.LE.58.0D0.OR.OP17.GE.68.0D0.AND.
     1        OP17.LE.69.0D0) THEN
C     REFERENCE RAYS
C     DEFAULT INPUT W4
C
                  IF(DF4.EQ.1) THEN
                      WRITE(OUTLYNE,*)
     1                'PREDEFINED OPERAND ',OPNM
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'REQUIRES EXPLICIT NUMERIC WORD #3 INPUT'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  IF(DF5.EQ.0.AND.W5.NE.0.0D0) THEN
                      WRITE(OUTLYNE,*)
     1                'PREDEFINED OPERAND ',OPNM
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'TAKES NO NUMERIC WORD #4 INPUT'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  DF5=0
                  S5=1
                  W5=0

C     OUT OF RANGE INPUT W3
                  IF(INT(W3).LT.NEWOBJ.OR.INT(W3).GT.NEWIMG) THEN
                      WRITE(OUTLYNE,*)
     1                'FOR PREDEFINED OPERAND ',OPNM
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'SURFACE NUMBER BEYOND LEGAL RANGE'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
C     OUT OF RANGE INPUT W4
                  IF(INT(W4).LT.1.OR.INT(W4).GT.200.0D0) THEN
                      WRITE(OUTLYNE,*)
     1                'FIELD POSITION NUMBER BEYOND LEGAL RANGE'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'FOR PREDEFINED OPERAND ',OPNM
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              ELSE
C     NOT REFERENCE RAYS
C     DEFAULT INPUT W4 AND W5
C
                  IF(DF4.EQ.1.OR.DF5.EQ.1) THEN
                      WRITE(OUTLYNE,*)
     1                'PREDEFINED OPERAND ',OPNM
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'REQUIRES EXPLICIT NUMERIC WORD #4 AND #5 INPUT'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF

C     OUT OF RANGE INPUT W3
                  IF(INT(W3).LT.NEWOBJ.OR.INT(W3).GT.NEWIMG) THEN
                      WRITE(OUTLYNE,*)
     1                'FOR PREDEFINED OPERAND ',OPNM
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'SURFACE NUMBER BEYOND LEGAL RANGE'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
C     OUT OF RANGE INPUT W4
                  IF(INT(W4).LT.1.OR.INT(W4).GT.200.0D0) THEN
                      WRITE(OUTLYNE,*)
     1                'FIELD POSITION NUMBER BEYOND LEGAL RANGE'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'FOR PREDEFINED OPERAND ',OPNM
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
C     OUT OF RANGE INPUT W5
                  IF(INT(W5).LT.1.OR.INT(W5).GT.500) THEN
                      WRITE(OUTLYNE,*)
     1                'RAY POSITION NUMBER BEYOND LEGAL RANGE'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'FOR PREDEFINED OPERAND ',OPNM
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
          END IF
C     *****************************************************************
          IF(OP17.EQ.16.0D0.OR.OP17.EQ.17.0D0) THEN
C     OPD OR OPDW
C
              IF(DF3.EQ.1.OR.DF4.EQ.1) THEN
                  WRITE(OUTLYNE,*)
     1            'PREDEFINED OPERAND ',OPNM
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)
     1            'REQUIRES EXPLICIT NUMERIC WORD #2 AND #3 INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C     OUT OF RANGE INPUT W3
              IF(INT(W3).LT.1.OR.INT(W3).GT.200.0D0) THEN
                  WRITE(OUTLYNE,*)
     1            'FIELD POSITION NUMBER BEYOND LEGAL RANGE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)
     1            'FOR PREDEFINED OPERAND ',OPNM
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C     OUT OF RANGE INPUT W4
              IF(INT(W4).LT.1.OR.INT(W4).GT.500) THEN
                  WRITE(OUTLYNE,*)
     1            'RAY POSITION NUMBER BEYOND LEGAL RANGE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)
     1            'FOR PREDEFINED OPERAND ',OPNM
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C     W5
              IF(S5.EQ.1.AND.W5.NE.0.0D0) THEN
                  WRITE(OUTLYNE,*)
     1            'PREDEFINED OPERAND ',OPNM
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)
     1            'TAKES NO NUMERIC WORD #4 INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IREG=W3
              OP8 =IREG
              OP18 =DBLE(DF3)
          END IF
C     *****************************************************************
          IF(OP17.GE.70.0D0.AND.OP17.LE.206.0D0.OR.
     1    OP17.GE.447.0D0.AND.OP17.LE.451.0D0.OR.
     2    OP17.EQ.467.0D0.OR.OP17.GE.485.0D0.AND.OP17.LE.511.0D0) THEN
C     LENS DATABASE OPERANDS
              IF(OP17.EQ.15.0D0.OR.OP17.EQ.21.0D0) THEN
                  IF(W3.LT.1.0D0) THEN
                      WRITE(OUTLYNE,*)
     1                'PREDEFINED OPERAND ',OPNM
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'REQUIRES NUMERIC WORD #2 GREATER THAN 0'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
C     DEFAULT INPUT W5
              IF(DF5.EQ.0.AND.W5.NE.0.0D0) THEN
                  WRITE(OUTLYNE,*)
     1            'PREDEFINED OPERAND ',OPNM
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)
     1            'TAKES NO NUMERIC WORD #4 INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(OP17.NE.91.0D0.AND.OP17.NE.106.0D0.AND.
     1        OP17.NE.107.0D0.AND.OP17.NE.509.0D0.AND.OP17.NE.511.0D0) THEN
                  IF(DF5.EQ.0.AND.W5.NE.0.0D0) THEN
                      WRITE(OUTLYNE,*)
     1                'PREDEFINED OPERAND ',OPNM
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'TAKES NO NUMERIC WORD #4 INPUT'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
C     SET DEFAULTS FOR LENGTH AND MLENGTH AND WEIGHT
              IF(OP17.EQ.106.0D0.OR.
     1        OP17.EQ.107.0D0.OR.OP17.EQ.509.0D0.OR.OP17.EQ.511.0D0) THEN
                  IF(W3.LT.0.0D0) W3=SYSTEM1(20)+W3
                  IF(W4.LT.0.0D0) W4=SYSTEM1(20)+W4
                  IF(DF3.EQ.1) W3=0.0D0
                  IF(DF3.EQ.1) DF3=0
                  IF(DF4.EQ.1) W4=SYSTEM1(20)
                  OP9 = W4
                  IF(DF4.EQ.1) DF4=0
                  OP11 =DBLE(DF4)
                  IREG=W3
                  OP8=W3
                  OP18=DBLE(DF3)
                  OP19=0.0D0
              END IF
C     SET DEFAULTS FOR VERTEX DATA
              IF(OP17.GE.94.0D0.AND.OP17.LE.105.0D0) THEN
                  IF(DF3.EQ.1) THEN
                      WRITE(OUTLYNE,*)
     1                'PREDEFINED OPERAND ',OPNM
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'REQUIRES EXPLICIT NUMERIC WORD #2 INPUT'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  IF(DF4.EQ.1) THEN
                      WRITE(OUTLYNE,*)
     1                'PREDEFINED OPERAND ',OPNM
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'REQUIRES EXPLICIT NUMERIC WORD #3 INPUT'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
C     SET SURFACE DATA NUMERIC WORD #2
              IF(OP17.GE.70.0D0.AND.
     1        OP17.LE.89.0D0.OR.OP17.GE.447.0D0.AND.
     2        OP17.LE.451.0D0.OR.OP17.GE.92.0D0.AND.
     3        OP17.LE.93.0D0.OR.OP17.GE.108.0D0.AND.
     4        OP17.LE.206.0D0.OR.OP17.EQ.467.0D0.OR.OP17.GT.485.0D0.AND.OP17
     5        .LE.499.0D0.OR.OP17.GE.500.0D0.AND.OP17.LE.508.0D0) THEN
                  IF(DF3.EQ.1) THEN
                      WRITE(OUTLYNE,*)
     1                'PREDEFINED OPERAND ',OPNM
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'REQUIRES EXPLICIT NUMERIC WORD #2 INPUT'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
              IF(OP17.GE.500.0D0.AND.OP17.LE.508.0D0) THEN
                  IF(DF4.EQ.0.AND.W4.NE.0.0D0.OR.DF5.EQ.0.AND.W5.NE.0.0D0) THEN
                      WRITE(OUTLYNE,*)
     1                'PREDEFINED OPERAND ',OPNM
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'TAKES NO NUMERIC WORD #3 OR #4 INPUT'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
C
              IF(OP17.GE.70.0D0.AND.OP17.LE.90.0D0.OR.
     1        OP17.GE.92.0D0.AND.
     1        OP17.LE.93.0D0.OR.OP17.EQ.467.0D0.OR.OP17.GE.
     2        108.0D0.AND.OP17.LE.206.0D0.OR.OP17.GE.447.0D0
     3        .AND.OP17.LE.451.0D0.OR.OP17.GE.485.0D0.AND.OP17.LE.499.0D0
     4        .OR.OP17.GE.500.0D0.AND.OP17.LE.508.0D0) THEN
                  IF(W3.LT.0.0D0) W3=SYSTEM1(20)+W3
                  IF(W3.LT.0.0D0.OR.W3.GT.SYSTEM1(20)) THEN
C     BAD SURFACE NUMBER
                      WRITE(OUTLYNE,*)
     1                'INVALID SURFACE NUMBER ISSUED FOR'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'PREDEFINED OPERAND ',OPNM
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
C
              IF(OP17.GE.94.0D0.AND.OP17.LE.105.0D0) THEN
                  IF(W3.LT.DBLE(NEWOBJ).OR.W3.GT.DBLE(NEWIMG)) THEN
C     BAD SURFACE NUMBER
                      WRITE(OUTLYNE,*)
     1                'INVALID SURFACE NUMBER, NW2, ISSUED FOR'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'PREDEFINED OPERAND ',OPNM
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  IF(W4.LT.DBLE(NEWOBJ).OR.W4.GT.DBLE(NEWIMG)) THEN
C     BAD SURFACE NUMBER
                      WRITE(OUTLYNE,*)
     1                'INVALID SURFACE NUMBER, NW3, ISSUED FOR'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'PREDEFINED OPERAND ',OPNM
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
C
              IF(OP17.EQ.106.0D0.OR.OP17.EQ.107.0D0.OR.OP17.EQ.509.0D0
     1        .OR.OP17.EQ.511.0D0) THEN
                  IF(W3.LT.0.0D0) W3=SYSTEM1(20)+W3
                  IF(W4.LT.0.0D0) W4=SYSTEM1(20)+W4
                  IF(W3.LT.0.0D0.OR.W3.GT.SYSTEM1(20).OR.W3.GE.W4.OR.W4.LT.0.0D0
     1            .OR.W4.GT.SYSTEM1(20)) THEN
C     BAD SURFACE NUMBER
                      WRITE(OUTLYNE,*)
     1                'INVALID SURFACE NUMBERS ISSUED FOR'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'PREDEFINED OPERAND ',OPNM
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
C
              IF(OP17.EQ.91.0D0) THEN
                  IF(W3.LT.0.0D0) W3=SYSTEM1(20)+W3
                  IF(W3.LT.0.0D0.OR.W3.GT.SYSTEM1(20)) THEN
C     BAD SURFACE NUMBER
                      WRITE(OUTLYNE,*)
     1                'INVALID SURFACE NUMBER ISSUED FOR'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'PREDEFINED OPERAND ',OPNM
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
C
          END IF
          IF(OP17.GE.207.0D0.AND.OP17.LE.236.0D0) THEN
C
              IF(OP17.GE.207.0D0.AND.OP17.LE.236.0D0) THEN
C     PARAXIAL OPERANDS
C     DEFAULT INPUT W5
                  IF(DF5.EQ.0.AND.W5.NE.0.0D0) THEN
                      WRITE(OUTLYNE,*)
     1                'PREDEFINED OPERAND ',OPNM
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'TAKES NO NUMERIC WORD #4 INPUT'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
              IF(OP17.GE.227.0D0.AND.OP17.LE.234.0D0) THEN
                  IF(S4.EQ.1.AND.W4.NE.0.0D0.OR.S5.EQ.1.AND.W5.NE.0.0D0) THEN
                      WRITE(OUTLYNE,*)
     1                'PREDEFINED OPERAND ',OPNM
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'TAKES NO NUMERIC WORD #3 OR #4 INPUT'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
C     SET DEFAULTS FOR PWRY,PWRX,FLCLTH OR FLCLTHY AND FLCLTHX
              IF(OP17.GE.207.0D0.AND.
     1        OP17.LE.210.0D0) THEN
                  IF(W3.LT.0.0D0) W3=SYSTEM1(20)+W3
                  IF(W4.LT.0.0D0) W4=SYSTEM1(20)+W4
                  IF(DF3.EQ.1) W3=0.0D0
                  IF(DF3.EQ.1) DF3=0
                  IF(DF4.EQ.1) W4=SYSTEM1(20)
                  OP9 = W4
                  IF(DF4.EQ.1) DF4=0
                  OP11 =DBLE(DF4)
                  IREG=W3
                  OP8=W3
                  OP18=DBLE(DF3)
                  OP19=0.0D0
                  DF5=1
                  W5=0.0D0
              END IF
C     SET DEFAULTS FOR 211 TO 226
              IF(OP17.GE.211.0D0.AND.OP17.LE.226.0D0) THEN
                  IF(W3.LT.0.0D0) W3=SYSTEM1(20)+W3
                  IF(DF3.EQ.1) W3=SYSTEM1(20)
                  IF(DF3.EQ.1) DF3=0
                  IF(DF4.EQ.1) W4=SYSTEM1(11)
                  OP9 = W4
                  IF(DF4.EQ.1) DF4=0
                  OP11 =DBLE(DF4)
                  IREG=W3
                  OP8=W3
                  OP18=DBLE(DF3)
                  OP19=0.0D0
              END IF
C     SET DEFAULTS FOR 227 TO 234
              IF(OP17.GE.227.0D0.AND.OP17.LE.234.0D0) THEN
                  IF(W3.LT.0.0D0) W3=SYSTEM1(20)+W3
                  IF(DF3.EQ.1) W3=SYSTEM1(20)
                  IREG=W3
                  OP8=W3
                  OP18=DBLE(DF3)
                  OP19=0.0D0
              END IF
C     SET NW3 FOR 235 TO 236
              IF(OP17.GE.235.0D0.AND.OP17.LE.236.0D0) THEN
                  IF(DF3.EQ.1) THEN
                      WRITE(OUTLYNE,*)
     1                'PREDEFINED OPERAND ',OPNM
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'REQUIRES EXPLICIT NUMERIC WORD #2 INPUT'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
C
C     OUT OF RANGE INPUT
C
              IF(OP17.GE.207.0D0.AND.OP17.LE.210.0D0) THEN
                  IF(W3.LT.0.0D0) W3=SYSTEM1(20)+W3
                  IF(W4.LT.0.0D0) W4=SYSTEM1(20)+W4
                  IF(W3.LT.0.0D0.OR.W3.GT.SYSTEM1(20).OR.
     1            W4.LT.0.0D0.OR.W4.GT.SYSTEM1(20).OR.
     1            W4.LE.W3) THEN
C     BAD SURFACE NUMBERS
                      WRITE(OUTLYNE,*)
     1                'INVALID SURFACE NUMBERS ISSUED FOR'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'PREDEFINED OPERAND ',OPNM
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
              IF(OP17.GE.211.0D0.AND.OP17.LE.226.0D0) THEN
                  IF(W3.LT.0.0D0) W3=SYSTEM1(20)+W3
                  IF(W3.LT.0.0D0.OR.W3.GT.SYSTEM1(20)) THEN
C     BAD SURFACE NUMBER
                      WRITE(OUTLYNE,*)
     1                'INVALID SURFACE NUMBER ISSUED FOR'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'PREDEFINED OPERAND ',OPNM
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  IF(W4.LT.1.0D0.OR.W4.GT.10.0D0) THEN
C     BAD WAVELENGTH NUMBER
                      WRITE(OUTLYNE,*)
     1                'INVALID WAVELENGTH NUMBER ISSUED FOR'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'PREDEFINED OPERAND ',OPNM
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  ELSE
                      W4=DBLE(INT(W4))
                      OP9 = W4
                  END IF
              END IF
              IF(OP17.GE.235.0D0.AND.OP17.LE.236.0D0) THEN
                  IF(W3.LT.0.0D0) W3=SYSTEM1(20)+W3
                  IF(W3.LT.0.0D0.OR.W3.GT.SYSTEM1(20)) THEN
C     BAD SURFACE NUMBER
                      WRITE(OUTLYNE,*)
     1                'INVALID SURFACE NUMBER ISSUED FOR'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'PREDEFINED OPERAND ',OPNM
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
C
          END IF
C
C     *****************************************************************
          IF(OP17.GE.237.0D0.AND.OP17.LE.245.0D0.OR.OP17.EQ.513.0D0) THEN
C     SPOT OPERANDS
C     DEFAULT INPUT W5
              IF(DF5.EQ.0.AND.W5.NE.0.0D0) THEN
                  WRITE(OUTLYNE,*)
     1            'PREDEFINED OPERAND ',OPNM
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)
     1            'TAKES NO NUMERIC WORD #4 INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C     SPOT OPERANDS
C     DEFAULT INPUT W3
              IF(DF3.EQ.1) THEN
                  WRITE(OUTLYNE,*)
     1            'PREDEFINED OPERAND ',OPNM
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)
     1            'REQUIRES EXPLICIT NUMERIC WORD #2 INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C
C     OUT OF RANGE INPUT
C
              IF(W3.LT.1.0D0.OR.W3.GT.200.0D0) THEN
C     BAD FIELD POS NUMBER
                  WRITE(OUTLYNE,*)
     1            'INVALID FIELD POSITION NUMBER ISSUED FOR'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)
     1            'PREDEFINED OPERAND ',OPNM
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C
C     OUT OF RANGE INPUT
C
              IF(W4.LE.0.0D0.OR.W4.GT.10.0D0) THEN
                  W4=0.0D0
                  DF4=1
              END IF
              IF(DF4.EQ.0) THEN
                  IF(W4.NE.1.0D0.AND.
     1            W4.NE.2.0D0.AND.
     1            W4.NE.3.0D0.AND.
     1            W4.NE.4.0D0.AND.
     1            W4.NE.5.0D0.AND.
     1            W4.NE.6.0D0.AND.
     1            W4.NE.7.0D0.AND.
     1            W4.NE.8.0D0.AND.
     1            W4.NE.9.0D0.AND.
     1            W4.NE.10.0D0) THEN
C     BAD WAVELENGTH NUMBER
                      WRITE(OUTLYNE,*)
     1                'INVALID WAVELENGTH NUMBER ISSUED FOR'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'PREDEFINED OPERAND ',OPNM
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
              OP8=W3
              OP18=DBLE(DF3)
              OP19=0.0D0
              OP9=W4
              OP11=DBLE(DF4)
C
          END IF
C     *****************************************************************
C     *****************************************************************
          IF(OP17.EQ.246.0D0.OR.OP17.GE.460.0D0.AND.
     1    OP17.LE.465.0D0.OR.OP17.EQ.484.0D0) THEN
C     CAPFN OPERANDS AND SECOND GROUP OF SPOT OPERANDS
C
              IF(OP17.EQ.246.0D0.OR.OP17.GE.460.0D0.AND.
     1        OP17.LE.465.0D0.OR.OP17.EQ.484.0D0) THEN
C     SPOT OPERANDS
C     DEFAULT INPUT W3
                  IF(DF3.EQ.1) THEN
                      WRITE(OUTLYNE,*)
     1                'PREDEFINED OPERAND ',OPNM
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'REQUIRES EXPLICIT NUMERIC WORD #2 INPUT'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
C
C     OUT OF RANGE INPUT
C
              IF(OP17.EQ.246.0D0.OR.OP17.GE.460.0D0.AND.
     1        OP17.LE.465.0D0.OR.OP17.EQ.484.0D0) THEN
                  IF(W3.LT.1.0D0.OR.W3.GT.200.0D0) THEN
C     BAD FIELD POS NUMBER
                      WRITE(OUTLYNE,*)
     1                'INVALID FIELD POSITION NUMBER ISSUED FOR'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'PREDEFINED OPERAND ',OPNM
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
              IF(OP17.EQ.246.0D0) THEN
                  IF(W4.LT.1.0D0.OR.W4.GT.37.0D0) THEN
C     BAD ZERN NUMBER
                      WRITE(OUTLYNE,*)
     1                'INVALID ZERN COEFFICIENT NUMBER ISSUED FOR'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'PREDEFINED OPERAND ',OPNM
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  ELSE
                      OP9=W4
                      OP11=DBLE(DF4)
                  END IF
              END IF
              IF(OP17.EQ.246.0D0) THEN
                  IF(DF4.EQ.1) THEN
C     BAD ZERN NUMBER
                      WRITE(OUTLYNE,*)
     1                'PREDEFINED OPERAND ',OPNM
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'REQUIRES EXPLICIT NUMERIC WORD #3 INPUT'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
              IF(OP17.EQ.484.0D0) THEN
                  IF(DF4.EQ.1) THEN
                      DF4=0
                      S4=1
                      W4=1.0D0
                  END IF
                  IF(W4.LT.0.0D0) THEN
C     BAD OPDWEIGT
                      WRITE(OUTLYNE,*)
     1                'OPD WEIGHT MUST BE GREATER THAN ZERO'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'PREDEFINED OPERAND ',OPNM
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  ELSE
                      OP9=W4
                      OP11=DBLE(DF4)
                  END IF
              END IF
              IF(OP17.EQ.464.0D0.OR.OP17.EQ.465.0D0) THEN
                  IF(W4.LE.0.0D0.OR.W4.GT.100.0D0) THEN
C     BAD % NUMBER
                      WRITE(OUTLYNE,*)
     1                'INVALID ENCIRLED ENERGY PERCENTAGE ISSUED FOR'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'PREDEFINED OPERAND ',OPNM
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  ELSE
                      OP9=W4
                      OP11=DBLE(DF4)
                  END IF
              END IF
              IF(OP17.GE.460.0D0.AND.OP17.LE.465.0D0) THEN
                  IF(DF4.EQ.1) THEN
C     BAD W4
                      WRITE(OUTLYNE,*)
     1                'PREDEFINED OPERAND ',OPNM
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'REQUIRES EXPLICIT NUMERIC WORD #3 INPUT'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
              IF(OP17.EQ.246.0D0.OR.OP17.EQ.484.0D0) THEN
                  IF(DF5.EQ.1) W5=SYSTEM1(11)
                  OP10 =W5
                  IF(DF5.EQ.1) DF5=0
                  OP12 =DBLE(DF5)
              END IF
              IF(OP17.EQ.246.0D0.OR.OP17.EQ.484.0D0) THEN
                  IF(W5.LT.1.0D0.OR.W5.GT.10.0D0) THEN
C     BAD WAVELENGTH NUMBER
                      WRITE(OUTLYNE,*)
     1                'INVALID WAVELENGTH NUMBER ISSUED FOR'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'PREDEFINED OPERAND ',OPNM
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  ELSE
                      W5=DBLE(INT(W5))
                      OP10 =W5
                      OP12 =DBLE(DF5)
                  END IF
              END IF
              IF(OP17.GE.464.0D0.AND.OP17.LE.465.0D0) THEN
                  IF(DF5.EQ.0) THEN
C     BAD W5 NUMBER
                      WRITE(OUTLYNE,*)
     1                'NO NUMERIC WORD #4 INPUT IS USED WITH'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'PREDEFINED OPERAND ',OPNM
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  ELSE
                      W5=DBLE(INT(W5))
                      OP10 =W5
                      OP12 =DBLE(DF5)
                  END IF
              END IF
              IF(OP17.GE.460.0D0.AND.OP17.LE.463.0D0) THEN
                  IF(DF5.EQ.1) THEN
                      WRITE(OUTLYNE,*)
     1                'EXPLICIT NUMERIC WORD #4 INPUT REQUIRED WITH'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'PREDEFINED OPERAND ',OPNM
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  ELSE
                      W5=DBLE(INT(W5))
                      OP10 =W5
                      OP12 =DBLE(DF5)
                  END IF
              END IF
              IF(OP17.GE.460.0D0.AND.OP17.LE.463.0D0) THEN
                  IF(DF5.EQ.1.AND.W1.NE.0.0D0.AND.W2.NE.90.0D0) THEN
                      WRITE(OUTLYNE,*)
     1                'NUMERIC WORD #4, ORIENTATION VALUE MUST BE 0 OR 90 FOR'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'PREDEFINED OPERAND ',OPNM
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  ELSE
                      W5=DBLE(INT(W5))
                      OP10 =W5
                      OP12 =DBLE(DF5)
                  END IF
              END IF
          END IF
C
          IF(OP17.GE.247.0D0.AND.OP17.LE.296.0D0.OR.
     1    OP17.GE.452.0D0.AND.OP17.LE.459.0D0) THEN
C
              IF(OP17.GE.247.0D0.AND.OP17.LE.296.0D0) THEN
C     SPECIAL OPERANDS
C     DEFAULT INPUT W5
                  IF(DF5.EQ.0.AND.W5.NE.0.0D0) THEN
                      WRITE(OUTLYNE,*)
     1                'PREDEFINED OPERAND ',OPNM
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'TAKES NO NUMERIC WORD #4 INPUT'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
              IF(OP17.GE.247.0D0.AND.OP17.LE.284.OR.
     1        OP17.GE.293.0D0.AND.OP17.LE.296.0D0.OR.OP17.EQ.466.0D0) THEN
C     CAPFN OPERANDS
C     DEFAULT INPUT W4
                  IF(DF4.EQ.0.AND.W4.NE.0.0D0) THEN
                      WRITE(OUTLYNE,*)
     1                'PREDEFINED OPERAND ',OPNM
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'TAKES NO NUMERIC WORD #3 INPUT'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
              IF(OP17.GE.247.0D0.AND.OP17.LE.296.0D0) THEN
C     SPECIAL OPERANDS
C     DEFAULT INPUT W3
                  IF(DF3.EQ.1) THEN
                      WRITE(OUTLYNE,*)
     1                'PREDEFINED OPERAND ',OPNM
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'REQUIRES EXPLICIT NUMERIC WORD #2 INPUT'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
C
C     OUT OF RANGE INPUT
C
              IF(OP17.GE.247.0D0.AND.OP17.LE.278.0D0.OR.
     1        OP17.LE.293.0D0.AND.OP17.LE.296.0D0.OR.OP17.EQ.466.0D0) THEN
                  IF(W3.LT.1.0D0.OR.W3.GT.200.0D0) THEN
C     BAD FIELD POS NUMBER
                      WRITE(OUTLYNE,*)
     1                'INVALID FIELD POSITION NUMBER ISSUED FOR'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'PREDEFINED OPERAND ',OPNM
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
              IF(OP17.GE.279.0D0.AND.OP17.LE.284.0D0) THEN
                  IF(W3.LT.0.0D0) W3=SYSTEM1(20)+W3
                  IF(W3.LT.1.0D0.OR.W3.GT.SYSTEM1(20)) THEN
C     BAD FIELD POS NUMBER
                      WRITE(OUTLYNE,*)
     1                'INVALID SURFACE NUMBER ISSUED FOR'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'PREDEFINED OPERAND ',OPNM
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
              IF(OP17.GE.285.0D0.AND.OP17.LE.292.0D0) THEN
                  IF(DF3.EQ.1) THEN
C     BAD SURF NUMBER
                      WRITE(OUTLYNE,*)
     1                'EXPLICIT SURFACE NUMBER REQUIRED FOR'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'PREDEFINED OPERAND ',OPNM
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  IF(DF4.EQ.1) THEN
C     BAD FIELD NUMBER
                      WRITE(OUTLYNE,*)
     1                'EXPLICIT FIELD NUMBER REQUIRED FOR'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'PREDEFINED OPERAND ',OPNM
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  IF(W4.LT.1.0D0.OR.W4.GT.200.0D0) THEN
C     BAD FIELD POS NUMBER
                      WRITE(OUTLYNE,*)
     1                'INVALID FIELD POSITION NUMBER ISSUED FOR'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'PREDEFINED OPERAND ',OPNM
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  IF(W3.LT.DBLE(NEWOBJ).OR.W3.GT.DBLE(NEWIMG)) THEN
C     BAD SURF NUMBER
                      WRITE(OUTLYNE,*)
     1                'INVALID SURFACE NUMBER ISSUED FOR'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'PREDEFINED OPERAND ',OPNM
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
              IF(OP17.GE.452.0D0.AND.OP17.LE.459.0D0) THEN
                  IF(DF3.EQ.1) THEN
                      DF3=0
                      W3=INT(SYSTEM1(20))
                  END IF
              END IF
              IF(OP17.GE.452.0D0.AND.OP17.LE.459.0D0) THEN
                  IF(DF4.EQ.1) THEN
C     BAD FIELD NUMBER
                      WRITE(OUTLYNE,*)
     1                'EXPLICIT FIELD NUMBER REQUIRED FOR'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'PREDEFINED OPERAND ',OPNM
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  IF(DF5.EQ.0) THEN
C     BAD SURF NUMBER
                      WRITE(OUTLYNE,*)
     1                'NO NUMERIC WORD #4 USED FOR'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'PREDEFINED OPERAND ',OPNM
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  IF(W4.LT.1.0D0.OR.W4.GT.200.0D0) THEN
C     BAD FIELD POS NUMBER
                      WRITE(OUTLYNE,*)
     1                'INVALID FIELD POSITION NUMBER ISSUED FOR'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'PREDEFINED OPERAND ',OPNM
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  IF(W3.LT.0.0D0) W3=SYSTEM1(20)+W3
                  IF(W3.LT.0.0D0.OR.W3.GT.SYSTEM1(20)) THEN
C     BAD FIELD SURF NUMBER
                      WRITE(OUTLYNE,*)
     1                'INVALID SURFACE NUMBER ISSUED FOR'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'PREDEFINED OPERAND ',OPNM
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
C
          END IF
C
C     *****************************************************************
          IF(OP17.GE.297.0D0.AND.OP17.LE.446.0D0) THEN
C     3,5,7 ABERRATIONS
C
C     DEFAULT INPUT W4 AND W5
              IF(DF4.EQ.0.AND.W5.NE.0.0D0.OR.DF5.EQ.0.AND.W5.NE.0.0D0) THEN
                  WRITE(OUTLYNE,*)
     1            'PREDEFINED OPERAND ',OPNM
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)
     1            'TAKES NO NUMERIC WORD #3 OR #4 INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(W3.LT.0.0D0) W3=SYSTEM1(20)+W3
              IF(DF3.EQ.1) W3=SYSTEM1(20)
              IF(DF3.EQ.1) DF3=0
              IREG=W3
              OP8=W3
              OP18=DBLE(DF3)
              OP19=0.0D0
C
              IF(W3.LT.1.0D0.OR.W3.GT.SYSTEM1(20)) THEN
C     BAD SURFACE NUMBER
                  WRITE(OUTLYNE,*)
     1            'INVALID SURFACE NUMBER ISSUED FOR'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)
     1            'PREDEFINED OPERAND ',OPNM
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C
          END IF
C
C     INPUT OK FOR W3, W4 AND W5
C
 3141     CONTINUE
C     ANY 8 CHARACTER QUALIFIER WORD OPERAND NAME MAY BE USED
C     BUT DUPLICATE NAMES CAUSE REPLACEMENT
C     HERE IS WERE THE REPLACEMENT HAPPENS
          I=INT(W1)
          IF(WQ.EQ.OPNAM(I).AND.
     1    W3.EQ.OPERND(I,8).AND.W4.EQ.OPERND(I,9)
     1    .AND.W5.EQ.OPERND(I,10).AND.OPT.EQ.OPERND(I,1)
     1    .AND.DBLE(CORMOD).EQ.OPERND(I,13)) THEN
C     FUNC, NAME, NW3,NW4,NW5,CFG AND CORMOD MUST MATCH OR NO REPLACEMENT
C
              OP1 =DBLE(OPT)
              OPNM=WQ
              OP2 =0.0D0
              OP3 =0.0D0
              OP4 =0.0D0
              OP5 =0.0D0
              OP6 =0.0D0
              OP7 =OPWEIT
              IREG=W3
              OP8 =IREG
              OP18 =DBLE(DF3)
              OP9 = W4
              OP10 =W5
              OP11 =DBLE(DF4)
              OP12 =DBLE(DF5)
              OP13 =DBLE(CORMOD)
              OP14 = 0.0D0
              OP15 = 0.0D0
              IF(OPT.EQ.0) OP16 = 1.0D0
              IF(OPT.NE.0) OP16 = 0.0D0
              IREG=W3
              OP8=W3
              OP18=DBLE(DF3)
              OP19=0.0D0
C
              OPERND(I,1) =OP1
              OPERND(I,2) =OP2
              OPERND(I,3) =OP3
              OPERND(I,4) =OP4
              OPERND(I,5) =OP5
              OPERND(I,6) =OP6
              OPERND(I,7) =OP7
              OPERND(I,8) =OP8
              OPERND(I,9) =OP9
              OPERND(I,10) =OP10
              OPERND(I,11) =OP11
              OPERND(I,12) =OP12
              OPERND(I,13) =OP13
              OPERND(I,14) =OP14
              OPERND(I,15) =OP15
              OPERND(I,16) =OP16
              OPERND(I,17) =OP17
              OPERND(I,18) =OP18
              OPERND(I,19) =OP19
              ISCRIT(I)=.TRUE.
              RETURN
          END IF
C
C     IT IS A NEW OPERAND
          OP1 =DBLE(OPT)
          OPNM=WQ
          OP2 =0.0D0
          OP3 =0.0D0
          OP4 =0.0D0
          OP5 =0.0D0
          OP6 =0.0D0
          OP7 =OPWEIT
          IREG=W3
          OP8 =IREG
          OP18 =DBLE(DF3)
          OP9 = W4
          OP10 =W5
          OP11 =DBLE(DF4)
          OP12 =DBLE(DF5)
          OP13 =DBLE(CORMOD)
          OP14 = 0.0D0
          OP15 = 0.0D0
          IREG=W3
          OP18=DBLE(DF3)
          OP19=0.0D0
          IF(OPT.EQ.0) OP16 = 1.0D0
          IF(OPT.NE.0) OP16 = 0.0D0
C
          I=INT(W1)
          OPNAM(I)=OPNM
          OPERND(I,1) =OP1
          OPERND(I,2) =OP2
          OPERND(I,3) =OP3
          OPERND(I,4) =OP4
          OPERND(I,5) =OP5
          OPERND(I,6) =OP6
          OPERND(I,7) =OP7
          OPERND(I,8) =OP8
          OPERND(I,9) =OP9
          OPERND(I,10) =OP10
          OPERND(I,11) =OP11
          OPERND(I,12) =OP12
          OPERND(I,13) =OP13
          OPERND(I,14) =OP14
          OPERND(I,15) =OP15
          OPERND(I,16) =OP16
          OPERND(I,17) =OP17
          OPERND(I,18) =OP18
          OPERND(I,19) =OP19
          ISCRIT(I)=.TRUE.
          RETURN
C       ALL DONE
      END


C SUB MERIT1.FOR
      SUBROUTINE MERIT1
C
          IMPLICIT NONE
C
          INTEGER I,OPT
C
          CHARACTER OPNM*8,OPCHR*3
C
          INTEGER VL
C
          CHARACTER BF*140
C
          REAL*8 OPWEIT,IREG
C
          REAL*8 OP1,OP2,OP3,OP4,OP5,OP6,OP7,OP8,OP9,OP10
     1    ,OP11,OP12,OP13,OP14,OP15,OP16,OP17,OP18,OP19

          LOGICAL OPOK
C
          INCLUDE 'datsub.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          INCLUDE 'datcfg.inc'
          INCLUDE 'datspd.inc'
C
C       THIS IS SUBROUTINE MERIT. THIS IS THE SUBROUTINE WHICH
C       HANDLES MERIT INPUT AND MERIT UPDATE COMMANDS AND
C       OUTPUT COMMANDS AT THE CMD LEVEL
C
C       THE ARRAY OPERND AND OPNAM STORE MERIT INFORMATION
C       IT IS PASSED IN COMMON IN THE INCLUDE FILE DATSUB.FOR
C
C       OPERND(I,J) WHERE I COUNTS THE NUMBER OF OPERAND ENTRIES
C       AND J TAKES ON THE FOLLOWING VALUES AND MEANIINGS.
C
C       J=1  > 1 THROUGH 10, THE NUMBER OF THE FUNCTION IN WHICH
C               THE ACTUAL OPERAND IS CALCULATED (COMMAND WORD)
C         (0 IS PREDEFINED OPERAND)
C
C       J=2  > TARGET VALUE OF THE OPERAND (NUMERIC WORD #1)
C       J=3  > OPERAND ORIGINAL VALUE
C       J=4  > OPERAND CURRENT VALUE
C       J=5  > OPERAND PREVIOUS VALUE
C       J=6  > LAST OPERAND CHANGE VALUE (CURRENT-PREVIOUS)
C       J=7  > WT, THE WEIGHTING FACTOR DURING OPTIMIZATION (NUMERIC WORD #2)
C
C       J=8  > NUMBER OF THE GENERAL PURPOSE REGISTER CONTAINING
C               THE ACTUAL OPERAND VALUE AS RETURNED FROM THE
C               CALCULATING FUNCTION (NUMERIC WORD #3) (VALUES 1 TO 300)
C               (A DEFAULT ENTRY HERE CAUSES THE ACCUMULATOR TO BE USED)
C       FOR PREDEFINED OPERAND, IT IS THE (I) INPUT VALUE
C
C       J=9  > OPTIONAL NW1 FOR THE FUNCTION (USING NSUB) (NUMERIC WORD #4)
C       J=10 > OPTIONAL NW2 FOR THE FUNCTION (USING NSUB) (NUMERIC WORD #5)
C     FOR PREDEFINED OPERANDS J=9 AND J=10 ARE THE (J) AND (K) INPUT VALUES
C       J=11 > DEFAULT FLAG FOR NW1
C       J=12 > DEFAULT FLAG FOR NW2
C       J=13 > COR MODE 1=COR, 0=BYP, BLO,GTE=-2, BHI,LTE=+2, HLD=10
C       J=14 > SQUARE ROOT OF WEIGHT TIMES (CURRENT VAL-TARGET))
C       J=15 > 0 BEFORE THE FIRST CALCULATION OF OPERAND VALUES
C              1 AFTER THE FIRST CALULATION
C       J=16 > CFG # FOR PREDEFINED OPERANDS (NW3)
C       J=17 > CODE FOR THE PREDEFINED OPERANDS
C       J=18 > DEFAULT FLAG FOR W3 = DBLE(DF3)
C       J=19 > 0 IF OP CALCULABLE, 1 IF NOT
C       J=20 > RESERVED FOR EXPANSION
C             1=X
C             2=Y
C             3=Z
C             4=L
C             5=M
C             6=N
C             7=DX
C             8=DY
C             9=DR
C            10=DXA
C            11=DYA
C            12=DRA
C            13=XANG
C            14=YANG
C            15=OPL
C            16=OPD
C            17=OPDW
C            18=LOLD
C            19=MOLD
C            20=NOLD
C            21=LEN
C            22=AII
C            23=AIP
C            24=LN
C            25=MN
C            26=NN
C            27=PXPX
C            28=PXPY
C            29=PYPX
C            30=PYPY
C            31=PXAPX
C            32=PXAPY
C            33=PYAPX
C            34=PYAPY
C            35=DXDX
C            36=DXDY
C            37=DYDX
C            38=DYDY
C            39=DXADX
C            40=DXADY
C            41=DYADX
C            42=DYADY
C            43=XREF
C            44=YREF
C            45=ZREF
C            46=LREF
C            47=MREF
C            48=NREF
C            49=LREFOL
C            50=MREFOL
C            51=NREFOL
C            52=IREF
C            53=IPREF
C            54=XAREF
C            55=YAREF
C            56=LNREF
C            57=MNREF
C            58=NNREF
C            59=GLX
C            60=GLY
C            61=GLZ
C            62=GLL
C            63=GLM
C            64=GLN
C            65=GLLOLD
C            66=GLMOLD
C            67=GLNOLD
C            68=LENREF
C            69=OPLREF
C            70=RD
C            71=CV
C            72=TH
C            73=CC
C            74=AC
C            75=AD
C            76=AE
C            77=AF
C            78=AG
C            79=RDTOR
C            80=CVTOR
C            81=CCTOR
C            82=ADTOR
C            83=AETOR
C            84=AFTOR
C            85=AGTOR
C            86=ALPHA
C            87=BETA
C            88=GAMMA
C            89=VNUM
C            90=PARTL
C            91=INDEX
C            92=XD
C            93=YD
C            94=XVERT
C            95=YVERT
C            96=ZVERT
C            97=LXVERT
C            98=MXVERT
C            99=NXVERT
C           100=LYVERT
C           101=MYVERT
C           102=NYVERT
C           103=LZVERT
C           104=MZVERT
C           105=NZVERT
C           106=LENGTH
C           106=OAL
C           107=MLENGTH
C           107=OPTLEN
C           108=ET
C           108=ETY
C           109=ETX
C           110=SHAPEFAC
C           111 TO 206 = C1 THROUGH C96
C           207=PWRY
C           208=PWRX
C           209=FLCLTHX
C           210=FLCLTH OR FLCLTHY
C           211=PY
C           212=PX
C           213=PCY
C           214=PCX
C           215=PUY
C           216=PUX
C           217=PUCY
C           218=PUCX
C           219=PIY
C           220=PIX
C           221=PICY
C           222=PICX
C           223=PIYP
C           224=PIXP
C           225=PICYP
C           226=PICXP
C           227=PACY
C           228=PACX
C           229=PLCY
C           230=PLCX
C           231=SACY
C           232=SACX
C           233=SLCY
C           234=SLCX
C           235=IMDISX
C           236=IMDISY
C           237=CENTX
C           238=CENTY
C           239=RMSX
C           240=RMSY
C           241=RMS
C           242=RSSX
C           243=RSSY
C           244=RSS
C           245=RMSOPD
C           246=ZERN37
C           247=MAGX
C           248=MAGY
C           249=MAGXOR
C           250=MAGYOR
C           251=FFLX
C           252=FFLY
C           253=BFLX
C           254=BFLY
C           255=FFNX
C           256=FFNY
C           257=BFNX
C           258=BFNY
C           259=EFLX
C           260=EFLY
C           261=ENDIAX
C           262=ENDIAY
C           263=EXDIAX
C           264=EXDIAY
C           265=ENPOSX
C           266=ENPOSY
C           267=ENPOSZ
C           268=EXPOSX
C           269=EXPOSY
C           270=EXPOSZ
C           271=FNUMX
C           272=FNUMY
C           273=OBFNUMX
C           274=OBFNUMY
C           275=ENPDIAX
C           276=ENPDIAY
C           277=EXPDIAX
C           278=EXPDIAY
C           279=PUPDIAX
C           280=PUPDIAY
C           281=PUPDISX
C           282=PUPDISY
C           283=CHFIMX
C           284=CHFIMY
C           285=GPX
C           286=GPY
C           287=GPUX
C           288=GPUY
C           289=GPCX
C           290=GPCY
C           291=GPUCX
C           292=GPUCY
C           293=DIST
C           294=XFOC
C           295=YFOC
C           296=AST
C           297=SA3
C           298=XSA3
C           299=CMA3
C           300=XCMA3
C           301=AST3
C           302=XAST3
C           303=DIS3
C           304=XDIS3
C           305=PTZ3
C           306=XPTZ3
C           307=SA5
C           308=XSA5
C           309=CMA5
C           310=XCMA5
C           311=AST5
C           312=XAST5
C           313=DIS5
C           314=XDIS5
C           315=PTZ5
C           316=XPTZ5
C           317=TOBSA
C           318=XTOBSA
C           319=SOBSA
C           320=XSOBSA
C           321=ELCMA
C           322=XELCMA
C           323=TAS
C           324=XTAS
C           325=SAS
C           326=XSAS
C           327=SA7
C           328=XSA7
C           329=SA3P
C           330=XSA3P
C           331=CMA3P
C           332=XCMA3P
C           333=AST3P
C           334=XAST3P
C           335=DIS3P
C           336=XDIS3P
C           337=PTZ3P
C           338=XPTZ3P
C           339=SA5P
C           340=XSA5P
C           341=CMA3P
C           342=XCMA3P
C           343=AST5P
C           344=XAST5P
C           345=DIS5P
C           346=XDIS5P
C           347=PTZ5P
C           348=XPTZ5P
C           349=TOBSAP
C           350=XTOBSAP
C           351=SOBSAP
C           352=XSOBSAP
C           353=ELCMAP
C           354=XELCMAP
C           355=TASP
C           356=XTASP
C           357=SASP
C           358=XSASP
C           359=SA7P
C           360=XSA7P
C           361=SA3S
C           362=XSA3S
C           363=CMA3S
C           364=XCMA3S
C           365=AST3S
C           366=XAST3S
C           367=DIS3S
C           368=XDIS3S
C           369=PTZ3S
C           370=XPTZ3S
C           371=SA5S
C           372=XSA5S
C           373=CMA5S
C           374=XCMA5S
C           375=AST5S
C           376=XAST5S
C           377=DIS5S
C           378=XDIS5S
C           379=PTZ5S
C           380=XPTZ5S
C           381=TOBSAS
C           382=XTOBSAS
C           383=SOBSAS
C           384=XSOBSAS
C           385=ELCMAS
C           386=XELCMAS
C           387=TASS
C           388=XTASS
C           389=SASS
C           390=XSASS
C           391=SA7S
C           392=XSA7S
C           393=SA5I
C           394=XSA5I
C           395=CMA5I
C           396=XCMA5I
C           397=AST5I
C           398=XAST5I
C           399=DIS5I
C           400=XDIS5I
C           401=PTZ5I
C           402=XPTZ5I
C           403=TOBSAI
C           404=XTOBSAI
C           405=SOBSAI
C           406=XSOBSAI
C           407=ELCMAI
C           408=XELCMAI
C           409=TASI
C           410=XTASI
C           411=SASI
C           412=XSASI
C           413=SA7I
C           414=XSA7I
C           415=PSA3
C           416=XPSA3
C           417=PCMA3
C           418=XPCMA3
C           419=PAST3
C           420=XPAST3
C           421=PDIS3
C           422=XPDIS3
C           423=PPTZ3
C           424=XPPTZ3
C           425=PSA3P
C           426=XPSA3P
C           427=PCMA3P
C           428=XPCMA3P
C           429=PAST3P
C           430=XPAST3P
C           431=PDIS3P
C           432=XPDIS3P
C           433=PPTZ3P
C           434=XPPTZ3P
C           435=PSA3S
C           436=XPSA3S
C           437=PCMA3S
C           438=XPCMA3S
C           439=PAST3S
C           440=XPAST3S
C           441=PDIS3S
C           442=XPDIS3S
C           443=PPTZ3S
C           444=XPPTZ3S
C           445=PTZCV
C           446=XPTZCV
C           447=AH
C           448=AI
C           449=AJ
C           450=AK
C           451=AL
C           452='GBRADX'
C           453='GBRADY'
C           454='GBDISX'
C           455='GBDISY'
C           456='GBRCVX'
C           457='GBRCVY'
C           458='GBWAISTX'
C           459='GBWAISTY'
C           460='MGOTF'
C           461='PGOTF'
C           462='MDOTF'
C           463='PDOTF'
C           460='GOTFM'
C           461='GOTFP'
C           462='DOTFM'
C           463='DOTFP'
C           464='RED'
C           465='REDCEN'
C           466='FISHDIST'
C           467='ZD'
C           468='SYMX'
C           469='SYMY'
C           470='ASYMX'
C           471='ASYMY'
C           472='PACM'
C           473='PACZ'
C           474='SACM'
C           475='SACZ'
C           476='PLCM'
C           477='PLCZ'
C           478='SLCM'
C           479='SLCZ'
C           480='CTSX'
C           481='CTSY'
C           482='SCEX'
C           483='SCEY'
C           484='GREYS'
C           485='PIVX'
C           486='PIVY'
C           487='PIVZ'
C           488='N1'
C           489='N2'
C           490='N3'
C           491='N4'
C           492='N5'
C           493='N6'
C           494='N7'
C           495='N8'
C           496='N9'
C           497='N10'
C           498='ABBE'
C           499='DPART'
C           500='CLPX'
C           501='CLPY'
C           502='GDX'
C           503='GDY'
C           504='GDZ'
C           505='GALPHA'
C           506='GBETA'
C           507='GGAMMA'
C           508='GRS'
C           509='WEIGHT'
C           510='DMINUSD'
C           511='COST'
C           512='MACOPT'
C           513='RMSYX'
C           514='CLEARX'
C           515='CLEARY'
C           801='ACT'
C
C       OPNM=(8 CHARACTER USER DEFINED, NON-DUPLICATED OPERAND NAME)
C
          IF(WC.EQ.'M'.OR.WC.EQ.'C') THEN
              CALL MESCOM
              RETURN
          END IF
C
C       "MR"/"OP" AND "MRA"/"OPA" OUTPUT MERIT DATA FROM INSIDE
C       AND OUTSIDE OF THE MERIT SUBFILE VIA SUBROUTINE MAROUT.FOR.
          IF(WC.EQ.'MRA'.OR.WC.EQ.'MR'.OR.WC.EQ.'OP'.OR.WC.EQ.'OPA') THEN
              OPTMES=.FALSE.
              CALL MAROUT
              OPTMES=.TRUE.
              RETURN
          END IF
C
C             CFG
C
          IF(WC.EQ.'CFG') THEN
C     SYNTAX CHECK
              IF(STI.EQ.1) THEN
                  WRITE(OUTLYNE,*)
     1            '"CFG" SETS THE CONFIGURATION FOR PREDEFINED OPERANDS'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'"IT TAKES NUMERIC WORD #1 INPUT'
                  CALL SHOWIT(1)
                  RETURN
              END IF
              IF(S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1.OR.SST.EQ.1
     1        .OR.SQ.EQ.1) THEN
                  WRITE(OUTLYNE,*)'"CFG" ONLY TAKES NUMERIC WORD #1 INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(S1.EQ.0) THEN
                  WRITE(OUTLYNE,*)'"CFG" REQUIRES EXPLICIT NUMERIC WORD #1 INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(INT(W1).GT.MAXCFG) THEN
                  WRITE(OUTLYNE,*)'CFG# BEYOND CURRENTLY DEFINED BOUNDS'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(INT(W1).GT.1) THEN
                  IF(INT(W1).LE.MAXCFG.AND.CFGCNT(INT(W1)).LE.0) THEN
                      WRITE(OUTLYNE,*)'CFG# ',INT(W1),' IS IDENTICAL TO CFG# 1'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'NO ACTION TAKEN, "CFG" COMMAND WAS IGNORED'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
              CURFIG=W1
              RETURN
          END IF
C
C       NOW DO CASE OF WC = EOS
C
C***********************************************************************
C       DEAL WITH WC=EOS
          IF(WC.EQ.'EOS') THEN
              IF(SST.EQ.1.OR.SQ.EQ.1.OR.SN.EQ.1)THEN
                  WRITE(OUTLYNE,*)'"EOS" TAKES NO EXPLICIT INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C       PROCEED WITH ACTION FOR COMMAND
              F1=1
              F27=0
              IF(OPCNT.EQ.0) THEN
                  WRITE(OUTLYNE,*)'THE MERIT SUBFILE IS EMPTY'
                  CALL SHOWIT(1)
              END IF
              F1=1
              F27=0
              CALL OPRCLN
              RETURN
C       ACTION COMPLETED
          END IF
C
C       EOS DONE
C***********************************************************************
C
C       NOW DO WC=DEL
          IF(WC.EQ.'DEL') THEN
              IF(F27.NE.2) THEN
                  WRITE(OUTLYNE,*)
     1            '"DEL" IS ONLY AVAILABLE FROM THE "UPDATE MERIT" LEVEL'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'NO ACTION TAKEN'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(SST.EQ.1.OR.SQ.EQ.1.OR.S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1
     1        .OR.S5.EQ.1) THEN
                  WRITE(OUTLYNE,*)
     1            '"DEL" ONLY TAKES NUMERIC WORD #1 INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(S1.EQ.0) THEN
                  WRITE(OUTLYNE,*)
     1            '"DEL" REQUIRES EXPLICIT NUMERIC WORD #1 INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C     CHECK FOR VALID QUALIFIER WORDS AND ASSIGN ASSOCIATED NUMERICAL
C     VALUES
              DELOP = -1
              IF(INT(W1).LE.0.OR.INT(W1).GT.OPCNT) THEN
                  WRITE(OUTLYNE,*)
     1            'OPERAND NUMBER BEYOND LEGAL BOUNDS'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              DELOP=INT(W1)
C     HERE IS WHERE OPERAND IS DELETED
C     DELETE TAGGED ITEM, ENTRY DELOP
C     DELETE ITEM I=DELOP
              IF(OPCNT.GT.1) THEN
                  DO I=DELOP,OPCNT-1
                      OPERND(I,1:20)=OPERND(I+1,1:20)
                      OPNAM(I)=OPNAM(I+1)
                  END DO
                  OPCNT=OPCNT-1
              ELSE
C     OPCNT WAS 1
C     MAKE IT ZERO
                  OPCNT=0
                  FMTEXT=.FALSE.
              END IF
C     ALL DELETIONS COMPLETED
              CALL OPRCLN
              RETURN
C
          END IF
C***********************************************************************
C
C       NOW DO WC=OP_DESC
          IF(WC.EQ.'OP_DESC') THEN
              IF(SN.EQ.1) THEN
                  WRITE(OUTLYNE,*)
     1            '"OP_DESC" TAKES NO NUMERIC INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(SQ.EQ.0.OR.SST.EQ.0) THEN
                  WRITE(OUTLYNE,*)
     1            '"OP_DESC" REQUIRES EXPLICIT QUALIFIER AND STRING INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              OPOK=.FALSE.
              OPCHR=WQ(3:5)
              WRITE(BF,110) OPCHR
              READ(BF,100) VL
 100          FORMAT(I3)
 110          FORMAT(A3)
              IF(VL.GT.OPCNT) THEN
                  WRITE(OUTLYNE,*)
     1            'DURING OPERAND INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)
     1            '"OP_DESC" REQUIRES A QUALIFIER WORD "OP1" TO "OP(# OF OPERANDS)"'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C
C     HERE IS WHERE OP_DESC IS ASSIGNED
              IF(WS(1:8).NE.'        ') OPERDESC(VL)=WS
              RETURN
C
          END IF
C
C
C     WC = COR,BLO,BHI,HLD,BYP
          IF(WC.EQ.'COR'.OR.WC.EQ.'BYP'.OR.WC.EQ.'BHI'.OR.WC.EQ.'BLO'
     1    .OR.WC.EQ.'HLD'.OR.WC.EQ.'GTE'.OR.WC.EQ.'LTE') THEN
              IF(STI.EQ.1) THEN
                  IF(CORMOD.EQ.0) CORNAM='BYP'
                  IF(CORMOD.EQ.1) CORNAM='COR'
                  IF(CORMOD.EQ.-2) CORNAM='GTE'
                  IF(CORMOD.EQ.2) CORNAM='LTE'
                  IF(CORMOD.EQ.10) CORNAM='HLD'
                  WRITE(OUTLYNE,*)
     1            'CURRENT CORRECTIONAL MODE = ','"',CORNAM,'"'
                  CALL SHOWIT(1)
                  RETURN
              END IF
              IF(SQ.EQ.1.OR.SN.EQ.1.OR.SST.EQ.1) THEN
                  WRITE(OUTLYNE,*)
     1            '"',WC(1:3),'" TAKES NO ADDITIONAL INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(WC.EQ.'COR'.OR.WC.EQ.'BYP'.OR.WC.EQ.'BLO'.OR.WC.EQ.'GTE'.OR.
     1        WC.EQ.'BHI'.OR.WC.EQ.'LTE'.OR.WC.EQ.'HLD') JK_CHMODE=.TRUE.
              MODEFLAG=.TRUE.
              IF(WC.EQ.'COR') CORMOD=1
              IF(WC.EQ.'BYP') CORMOD=0
              IF(WC.EQ.'BLO'.OR.WC.EQ.'GTE') CORMOD=-2
              IF(WC.EQ.'BHI'.OR.WC.EQ.'LTE') CORMOD=2
              IF(WC.EQ.'HLD') CORMOD=10
              RETURN
          END IF
C
C     START DOING THE FUNCTION NAMES HERE
          OPT=-1
          IF(WC.EQ.'FUNC00  ') OPT=0
          IF(WC.EQ.'FUNC01  ') OPT=1
          IF(WC.EQ.'FUNC02  ') OPT=2
          IF(WC.EQ.'FUNC03  ') OPT=3
          IF(WC.EQ.'FUNC04  ') OPT=4
          IF(WC.EQ.'FUNC05  ') OPT=5
          IF(WC.EQ.'FUNC06  ') OPT=6
          IF(WC.EQ.'FUNC07  ') OPT=7
          IF(WC.EQ.'FUNC08  ') OPT=8
          IF(WC.EQ.'FUNC09  ') OPT=9
          IF(WC.EQ.'FUNC10  ') OPT=10
          IF(OPT.EQ.-1) THEN
              WRITE(OUTLYNE,*)
     1        'INVALID FUNCTION NAME COMMAND WORD'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
C     NUMERIC WORDS AND DEFAULTS
C
C     NW1 IS THE OPERAND TARGET, DEFUALT WILL BE 0.0D0
          IF(DF1.EQ.1) W1=0.0D0
C
C     NW2 IS WEIGHT, DEFAULT WILL BE 1.0D0
          IF(DF2.EQ.1) THEN
C     DEFAULT INPUT
              W2=1.0D0
              OPWEIT=W2
          ELSE
C     NOT DEFAULT
              IF(W2.LT.0.0D0) THEN
                  WRITE(OUTLYNE,*)
     1            'NUMERIC WORD #2 (WEIGHT) MUST NOT BE NEGATIVE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              ELSE
C     NW2 OK, PROCEED
                  OPWEIT=W2
              END IF
          END IF
          IF(OPT.NE.0) THEN
C     NW3 IS GENERAL PURPOSE STORAGE REGISTER VALUE
              IF(DF3.EQ.1) THEN
                  IF(F27.EQ.1.OR.F27.EQ.2) THEN
                      WRITE(OUTLYNE,*)
     1                'NUMERIC WORD #3 (GP REGISTER ADDRESS) MUST BE EXPLICITLY INPUT'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'AT THE "UPDATE MERIT" LEVEL FOR A USER DEFINED OPERAND'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              ELSE
C     NOT DEFAULT
                  IF((DBLE(DINT(W3))-W3).NE.0.0D0) THEN
                      WRITE(OUTLYNE,*)
     1                'NUMERIC WORD #3 (GP REGISTER ADDRESS) MUST BE AN INTEGER'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  IF(W3.LT.1.0D0.OR.W3.GT.400.0D0) THEN
                      WRITE(OUTLYNE,*)
     1                'NUMERIC WORD #3 (GP REGISTER ADDRESS) BEYOND LEGAL RANGE'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'VALID RANGE IS 1 TO 400'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  IF(DF3.NE.1.AND.W3.LT.0.0D0) THEN
                      WRITE(OUTLYNE,*)
     1                'NUMERIC WORD #3 (GP REGISTER ADDRESS) MAY NOT BE NEGATIVE'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  IREG=W3
                  OP8=W3
                  OP18=DBLE(DF3)
              END IF
          END IF

C     NW4 IS NW1 FOR FUNCTION NSUB FOR USER DEF OPS
C     NW5 IS NW2 FOR FUNCTION NSUB FOR USER DEF OPS
C     DEFAULT STATUS WILL BE HANDLED DURING FUNCTION CALLING
C     AND WILL BE PASSED IN ITEMS J=11 AND J=12
C
          IF(OPT.GE.1.AND.OPT.LE.10) GO TO 3141
C
          OPNM=WQ
          IF(OPNM.EQ.'X       ') OP17=1.0D0
          IF(OPNM.EQ.'Y       ') OP17=2.0D0
          IF(OPNM.EQ.'Z       ') OP17=3.0D0
          IF(OPNM.EQ.'DCL     ') OP17=4.0D0
          IF(OPNM.EQ.'K       ') OP17=4.0D0
          IF(OPNM.EQ.'DCM     ') OP17=5.0D0
          IF(OPNM.EQ.'L       ') OP17=5.0D0
          IF(OPNM.EQ.'DCN     ') OP17=6.0D0
          IF(OPNM.EQ.'M       ') OP17=6.0D0
          IF(OPNM.EQ.'DX      ') OP17=7.0D0
          IF(OPNM.EQ.'DY      ') OP17=8.0D0
          IF(OPNM.EQ.'DR      ') OP17=9.0D0
          IF(OPNM.EQ.'DXA     ') OP17=10.0D0
          IF(OPNM.EQ.'DYA     ') OP17=11.0D0
          IF(OPNM.EQ.'DRA     ') OP17=12.0D0
          IF(OPNM.EQ.'XANG    ') OP17=13.0D0
          IF(OPNM.EQ.'YANG    ') OP17=14.0D0
          IF(OPNM.EQ.'OPL     ') OP17=15.0D0
          IF(OPNM.EQ.'OPD     ') OP17=16.0D0
          IF(OPNM.EQ.'OPDW    ') OP17=17.0D0
          IF(OPNM.EQ.'LOLD    ') OP17=18.0D0
          IF(OPNM.EQ.'MOLD    ') OP17=19.0D0
          IF(OPNM.EQ.'NOLD    ') OP17=20.0D0
          IF(OPNM.EQ.'LEN     ') OP17=21.0D0
          IF(OPNM.EQ.'AII     ') OP17=22.0D0
          IF(OPNM.EQ.'AIP     ') OP17=23.0D0
          IF(OPNM.EQ.'LN      ') OP17=24.0D0
          IF(OPNM.EQ.'MN      ') OP17=25.0D0
          IF(OPNM.EQ.'NN      ') OP17=26.0D0
          IF(OPNM.EQ.'PXPX    ') OP17=27.0D0
          IF(OPNM.EQ.'PXPY    ') OP17=28.0D0
          IF(OPNM.EQ.'PYPX    ') OP17=29.0D0
          IF(OPNM.EQ.'PYPY    ') OP17=30.0D0
          IF(OPNM.EQ.'PXAPX   ') OP17=31.0D0
          IF(OPNM.EQ.'PXAPY   ') OP17=32.0D0
          IF(OPNM.EQ.'PYAPX   ') OP17=33.0D0
          IF(OPNM.EQ.'PYAPY   ') OP17=34.0D0
          IF(OPNM.EQ.'DXDX    ') OP17=35.0D0
          IF(OPNM.EQ.'DXDY    ') OP17=36.0D0
          IF(OPNM.EQ.'DYDX    ') OP17=37.0D0
          IF(OPNM.EQ.'DYDY    ') OP17=38.0D0
          IF(OPNM.EQ.'DXADX   ') OP17=39.0D0
          IF(OPNM.EQ.'DXADY   ') OP17=40.0D0
          IF(OPNM.EQ.'DYADX   ') OP17=41.0D0
          IF(OPNM.EQ.'DYADY   ') OP17=42.0D0
          IF(OPNM.EQ.'XREF    ') OP17=43.0D0
          IF(OPNM.EQ.'YREF    ') OP17=44.0D0
          IF(OPNM.EQ.'ZREF    ') OP17=45.0D0
          IF(OPNM.EQ.'LREF    ') OP17=46.0D0
          IF(OPNM.EQ.'MREF    ') OP17=47.0D0
          IF(OPNM.EQ.'NREF    ') OP17=48.0D0
          IF(OPNM.EQ.'LREFOL  ') OP17=49.0D0
          IF(OPNM.EQ.'MREFOL  ') OP17=50.0D0
          IF(OPNM.EQ.'NREFOL  ') OP17=51.0D0
          IF(OPNM.EQ.'IREF    ') OP17=52.0D0
          IF(OPNM.EQ.'IPREF   ') OP17=53.0D0
          IF(OPNM.EQ.'XAREF   ') OP17=54.0D0
          IF(OPNM.EQ.'YAREF   ') OP17=55.0D0
          IF(OPNM.EQ.'LNREF   ') OP17=56.0D0
          IF(OPNM.EQ.'MNREF   ') OP17=57.0D0
          IF(OPNM.EQ.'NNREF   ') OP17=58.0D0
          IF(OPNM.EQ.'GLX     ') OP17=59.0D0
          IF(OPNM.EQ.'GLY     ') OP17=60.0D0
          IF(OPNM.EQ.'GLZ     ') OP17=61.0D0
          IF(OPNM.EQ.'GLL     ') OP17=62.0D0
          IF(OPNM.EQ.'GLM     ') OP17=63.0D0
          IF(OPNM.EQ.'GLN     ') OP17=64.0D0
          IF(OPNM.EQ.'GLLOLD  ') OP17=65.0D0
          IF(OPNM.EQ.'GLMOLD  ') OP17=66.0D0
          IF(OPNM.EQ.'GLNOLD  ') OP17=67.0D0
          IF(OPNM.EQ.'LENREF  ') OP17=68.0D0
          IF(OPNM.EQ.'OPLREF  ') OP17=69.0D0
          IF(OPNM.EQ.'RD      ') OP17=70.0D0
          IF(OPNM.EQ.'CV      ') OP17=71.0D0
          IF(OPNM.EQ.'TH      ') OP17=72.0D0
          IF(OPNM.EQ.'CC      ') OP17=73.0D0
          IF(OPNM.EQ.'AC      ') OP17=74.0D0
          IF(OPNM.EQ.'AD      ') OP17=75.0D0
          IF(OPNM.EQ.'AE      ') OP17=76.0D0
          IF(OPNM.EQ.'AF      ') OP17=77.0D0
          IF(OPNM.EQ.'AG      ') OP17=78.0D0
          IF(OPNM.EQ.'RDTOR   ') OP17=79.0D0
          IF(OPNM.EQ.'CVTOR   ') OP17=80.0D0
          IF(OPNM.EQ.'CCTOR   ') OP17=81.0D0
          IF(OPNM.EQ.'ADTOR   ') OP17=82.0D0
          IF(OPNM.EQ.'AETOR   ') OP17=83.0D0
          IF(OPNM.EQ.'AFTOR   ') OP17=84.0D0
          IF(OPNM.EQ.'AGTOR   ') OP17=85.0D0
          IF(OPNM.EQ.'ALPHA   ') OP17=86.0D0
          IF(OPNM.EQ.'BETA    ') OP17=87.0D0
          IF(OPNM.EQ.'GAMMA   ') OP17=88.0D0
          IF(OPNM.EQ.'VNUM    ') OP17=89.0D0
          IF(OPNM.EQ.'PARTL   ') OP17=90.0D0
          IF(OPNM.EQ.'INDEX   ') OP17=91.0D0
          IF(OPNM.EQ.'XD      ') OP17=92.0D0
          IF(OPNM.EQ.'YD      ') OP17=93.0D0
          IF(OPNM.EQ.'XVERT   ') OP17=94.0D0
          IF(OPNM.EQ.'YVERT   ') OP17=95.0D0
          IF(OPNM.EQ.'ZVERT   ') OP17=96.0D0
          IF(OPNM.EQ.'LXVERT  ') OP17=97.0D0
          IF(OPNM.EQ.'MXVERT  ') OP17=98.0D0
          IF(OPNM.EQ.'NXVERT  ') OP17=99.0D0
          IF(OPNM.EQ.'LYVERT  ') OP17=100.0D0
          IF(OPNM.EQ.'MYVERT  ') OP17=101.0D0
          IF(OPNM.EQ.'NYVERT  ') OP17=102.0D0
          IF(OPNM.EQ.'LZVERT  ') OP17=103.0D0
          IF(OPNM.EQ.'MZVERT  ') OP17=104.0D0
          IF(OPNM.EQ.'NZVERT  ') OP17=105.0D0
          IF(OPNM.EQ.'LENGTH  ') OP17=106.0D0
          IF(OPNM.EQ.'OAL     ') OP17=106.0D0
          IF(OPNM.EQ.'MLENGTH ') OP17=107.0D0
          IF(OPNM.EQ.'OPTLEN  ') OP17=107.0D0
          IF(OPNM.EQ.'ET      ') OP17=108.0D0
          IF(OPNM.EQ.'ETY     ') OP17=108.0D0
          IF(OPNM.EQ.'ETX     ') OP17=109.0D0
          IF(OPNM.EQ.'SHAPEFAC') OP17=110.0D0
          IF(OPNM.EQ.'C1      ') OP17=111.0D0
          IF(OPNM.EQ.'C2      ') OP17=112.0D0
          IF(OPNM.EQ.'C3      ') OP17=113.0D0
          IF(OPNM.EQ.'C4      ') OP17=114.0D0
          IF(OPNM.EQ.'C5      ') OP17=115.0D0
          IF(OPNM.EQ.'C6      ') OP17=116.0D0
          IF(OPNM.EQ.'C7      ') OP17=117.0D0
          IF(OPNM.EQ.'C8      ') OP17=118.0D0
          IF(OPNM.EQ.'C9      ') OP17=119.0D0
          IF(OPNM.EQ.'C10     ') OP17=120.0D0
          IF(OPNM.EQ.'C11     ') OP17=121.0D0
          IF(OPNM.EQ.'C12     ') OP17=122.0D0
          IF(OPNM.EQ.'C13     ') OP17=123.0D0
          IF(OPNM.EQ.'C14     ') OP17=124.0D0
          IF(OPNM.EQ.'C15     ') OP17=125.0D0
          IF(OPNM.EQ.'C16     ') OP17=126.0D0
          IF(OPNM.EQ.'C17     ') OP17=127.0D0
          IF(OPNM.EQ.'C18     ') OP17=128.0D0
          IF(OPNM.EQ.'C19     ') OP17=129.0D0
          IF(OPNM.EQ.'C20     ') OP17=130.0D0
          IF(OPNM.EQ.'C21     ') OP17=131.0D0
          IF(OPNM.EQ.'C22     ') OP17=132.0D0
          IF(OPNM.EQ.'C23     ') OP17=133.0D0
          IF(OPNM.EQ.'C24     ') OP17=134.0D0
          IF(OPNM.EQ.'C25     ') OP17=135.0D0
          IF(OPNM.EQ.'C26     ') OP17=136.0D0
          IF(OPNM.EQ.'C27     ') OP17=137.0D0
          IF(OPNM.EQ.'C28     ') OP17=138.0D0
          IF(OPNM.EQ.'C29     ') OP17=139.0D0
          IF(OPNM.EQ.'C30     ') OP17=140.0D0
          IF(OPNM.EQ.'C31     ') OP17=141.0D0
          IF(OPNM.EQ.'C32     ') OP17=142.0D0
          IF(OPNM.EQ.'C33     ') OP17=143.0D0
          IF(OPNM.EQ.'C34     ') OP17=144.0D0
          IF(OPNM.EQ.'C35     ') OP17=145.0D0
          IF(OPNM.EQ.'C36     ') OP17=146.0D0
          IF(OPNM.EQ.'C37     ') OP17=147.0D0
          IF(OPNM.EQ.'C38     ') OP17=148.0D0
          IF(OPNM.EQ.'C39     ') OP17=149.0D0
          IF(OPNM.EQ.'C40     ') OP17=150.0D0
          IF(OPNM.EQ.'C41     ') OP17=151.0D0
          IF(OPNM.EQ.'C42     ') OP17=152.0D0
          IF(OPNM.EQ.'C43     ') OP17=153.0D0
          IF(OPNM.EQ.'C44     ') OP17=154.0D0
          IF(OPNM.EQ.'C45     ') OP17=155.0D0
          IF(OPNM.EQ.'C46     ') OP17=156.0D0
          IF(OPNM.EQ.'C47     ') OP17=157.0D0
          IF(OPNM.EQ.'C48     ') OP17=158.0D0
          IF(OPNM.EQ.'C49     ') OP17=159.0D0
          IF(OPNM.EQ.'C50     ') OP17=160.0D0
          IF(OPNM.EQ.'C51     ') OP17=161.0D0
          IF(OPNM.EQ.'C52     ') OP17=162.0D0
          IF(OPNM.EQ.'C53     ') OP17=163.0D0
          IF(OPNM.EQ.'C54     ') OP17=164.0D0
          IF(OPNM.EQ.'C55     ') OP17=165.0D0
          IF(OPNM.EQ.'C56     ') OP17=166.0D0
          IF(OPNM.EQ.'C57     ') OP17=167.0D0
          IF(OPNM.EQ.'C58     ') OP17=168.0D0
          IF(OPNM.EQ.'C59     ') OP17=169.0D0
          IF(OPNM.EQ.'C60     ') OP17=170.0D0
          IF(OPNM.EQ.'C61     ') OP17=171.0D0
          IF(OPNM.EQ.'C62     ') OP17=172.0D0
          IF(OPNM.EQ.'C63     ') OP17=173.0D0
          IF(OPNM.EQ.'C64     ') OP17=174.0D0
          IF(OPNM.EQ.'C65     ') OP17=175.0D0
          IF(OPNM.EQ.'C66     ') OP17=176.0D0
          IF(OPNM.EQ.'C67     ') OP17=177.0D0
          IF(OPNM.EQ.'C68     ') OP17=178.0D0
          IF(OPNM.EQ.'C69     ') OP17=179.0D0
          IF(OPNM.EQ.'C70     ') OP17=180.0D0
          IF(OPNM.EQ.'C71     ') OP17=181.0D0
          IF(OPNM.EQ.'C72     ') OP17=182.0D0
          IF(OPNM.EQ.'C73     ') OP17=183.0D0
          IF(OPNM.EQ.'C74     ') OP17=184.0D0
          IF(OPNM.EQ.'C75     ') OP17=185.0D0
          IF(OPNM.EQ.'C76     ') OP17=186.0D0
          IF(OPNM.EQ.'C77     ') OP17=187.0D0
          IF(OPNM.EQ.'C78     ') OP17=188.0D0
          IF(OPNM.EQ.'C79     ') OP17=189.0D0
          IF(OPNM.EQ.'C80     ') OP17=190.0D0
          IF(OPNM.EQ.'C81     ') OP17=191.0D0
          IF(OPNM.EQ.'C82     ') OP17=192.0D0
          IF(OPNM.EQ.'C83     ') OP17=193.0D0
          IF(OPNM.EQ.'C84     ') OP17=194.0D0
          IF(OPNM.EQ.'C85     ') OP17=195.0D0
          IF(OPNM.EQ.'C86     ') OP17=196.0D0
          IF(OPNM.EQ.'C87     ') OP17=197.0D0
          IF(OPNM.EQ.'C88     ') OP17=198.0D0
          IF(OPNM.EQ.'C89     ') OP17=199.0D0
          IF(OPNM.EQ.'C90     ') OP17=200.0D0
          IF(OPNM.EQ.'C91     ') OP17=201.0D0
          IF(OPNM.EQ.'C92     ') OP17=202.0D0
          IF(OPNM.EQ.'C93     ') OP17=203.0D0
          IF(OPNM.EQ.'C94     ') OP17=204.0D0
          IF(OPNM.EQ.'C95     ') OP17=205.0D0
          IF(OPNM.EQ.'C96     ') OP17=206.0D0
          IF(OPNM.EQ.'PWRY    ') OP17=207.0D0
          IF(OPNM.EQ.'PWRX    ') OP17=208.0D0
          IF(OPNM.EQ.'FLCLTHX ') OP17=209.0D0
          IF(OPNM.EQ.'FLCLTH  ') OP17=210.0D0
          IF(OPNM.EQ.'FLCLTHY ') OP17=210.0D0
          IF(OPNM.EQ.'PY      ') OP17=211.0D0
          IF(OPNM.EQ.'PX      ') OP17=212.0D0
          IF(OPNM.EQ.'PCY     ') OP17=213.0D0
          IF(OPNM.EQ.'PCX     ') OP17=214.0D0
          IF(OPNM.EQ.'PUY     ') OP17=215.0D0
          IF(OPNM.EQ.'PUX     ') OP17=216.0D0
          IF(OPNM.EQ.'PUCY    ') OP17=217.0D0
          IF(OPNM.EQ.'PUCX    ') OP17=218.0D0
          IF(OPNM.EQ.'PIY     ') OP17=219.0D0
          IF(OPNM.EQ.'PIX     ') OP17=220.0D0
          IF(OPNM.EQ.'PICY    ') OP17=221.0D0
          IF(OPNM.EQ.'PICX    ') OP17=222.0D0
          IF(OPNM.EQ.'PIYP    ') OP17=223.0D0
          IF(OPNM.EQ.'PIXP    ') OP17=224.0D0
          IF(OPNM.EQ.'PICYP   ') OP17=225.0D0
          IF(OPNM.EQ.'PICXP   ') OP17=226.0D0
          IF(OPNM.EQ.'PACY    ') OP17=227.0D0
          IF(OPNM.EQ.'PACX    ') OP17=228.0D0
          IF(OPNM.EQ.'PLCY    ') OP17=229.0D0
          IF(OPNM.EQ.'PLCX    ') OP17=230.0D0
          IF(OPNM.EQ.'SACY    ') OP17=231.0D0
          IF(OPNM.EQ.'SACX    ') OP17=232.0D0
          IF(OPNM.EQ.'SLCY    ') OP17=233.0D0
          IF(OPNM.EQ.'SLCX    ') OP17=234.0D0
          IF(OPNM.EQ.'IMDISX  ') OP17=235.0D0
          IF(OPNM.EQ.'IMDISY  ') OP17=236.0D0
          IF(OPNM.EQ.'CENTX   ') OP17=237.0D0
          IF(OPNM.EQ.'CENTY   ') OP17=238.0D0
          IF(OPNM.EQ.'RMSX    ') OP17=239.0D0
          IF(OPNM.EQ.'RMSY    ') OP17=240.0D0
          IF(OPNM.EQ.'RMS     ') OP17=241.0D0
          IF(OPNM.EQ.'RSSX    ') OP17=242.0D0
          IF(OPNM.EQ.'RSSY    ') OP17=243.0D0
          IF(OPNM.EQ.'RSS     ') OP17=244.0D0
          IF(OPNM.EQ.'RMSOPD  ') OP17=245.0D0
          IF(OPNM.EQ.'ZERN37  ') OP17=246.0D0
          IF(OPNM.EQ.'MAGX    ') OP17=247.0D0
          IF(OPNM.EQ.'MAGY    ') OP17=248.0D0
          IF(OPNM.EQ.'MAGXOR  ') OP17=249.0D0
          IF(OPNM.EQ.'MAGYOR  ') OP17=250.0D0
          IF(OPNM.EQ.'FFLX    ') OP17=251.0D0
          IF(OPNM.EQ.'FFLY    ') OP17=252.0D0
          IF(OPNM.EQ.'BFLX    ') OP17=253.0D0
          IF(OPNM.EQ.'BFLY    ') OP17=254.0D0
          IF(OPNM.EQ.'FFNX    ') OP17=255.0D0
          IF(OPNM.EQ.'FFNY    ') OP17=256.0D0
          IF(OPNM.EQ.'BFNX    ') OP17=257.0D0
          IF(OPNM.EQ.'BFNY    ') OP17=258.0D0
          IF(OPNM.EQ.'EFLX    ') OP17=259.0D0
          IF(OPNM.EQ.'EFLY    ') OP17=260.0D0
          IF(OPNM.EQ.'ENDIAX  ') OP17=261.0D0
          IF(OPNM.EQ.'ENDIAY  ') OP17=262.0D0
          IF(OPNM.EQ.'EXDIAX  ') OP17=263.0D0
          IF(OPNM.EQ.'EXDIAY  ') OP17=264.0D0
          IF(OPNM.EQ.'ENPOSX  ') OP17=265.0D0
          IF(OPNM.EQ.'ENPOSY  ') OP17=266.0D0
          IF(OPNM.EQ.'ENPOSZ  ') OP17=267.0D0
          IF(OPNM.EQ.'EXPOSX  ') OP17=268.0D0
          IF(OPNM.EQ.'EXPOSY  ') OP17=269.0D0
          IF(OPNM.EQ.'EXPOSZ  ') OP17=270.0D0
          IF(OPNM.EQ.'FNUMX   ') OP17=271.0D0
          IF(OPNM.EQ.'FNYMY   ') OP17=272.0D0
          IF(OPNM.EQ.'OBFNUMX ') OP17=273.0D0
          IF(OPNM.EQ.'OBFNUMY ') OP17=274.0D0
          IF(OPNM.EQ.'ENPDIAX ') OP17=275.0D0
          IF(OPNM.EQ.'ENPDIAY ') OP17=276.0D0
          IF(OPNM.EQ.'EXPDIAX ') OP17=277.0D0
          IF(OPNM.EQ.'EXPDIAY ') OP17=278.0D0
          IF(OPNM.EQ.'PUPDIAX ') OP17=279.0D0
          IF(OPNM.EQ.'PUPDIAY ') OP17=280.0D0
          IF(OPNM.EQ.'PUPDISX ') OP17=281.0D0
          IF(OPNM.EQ.'PUPDISY ') OP17=282.0D0
          IF(OPNM.EQ.'CHFIMX  ') OP17=283.0D0
          IF(OPNM.EQ.'CHFIMY  ') OP17=284.0D0
          IF(OPNM.EQ.'GPX     ') OP17=285.0D0
          IF(OPNM.EQ.'GPY     ') OP17=286.0D0
          IF(OPNM.EQ.'GPUX    ') OP17=287.0D0
          IF(OPNM.EQ.'GPUY    ') OP17=288.0D0
          IF(OPNM.EQ.'GPCX    ') OP17=289.0D0
          IF(OPNM.EQ.'GPCY    ') OP17=290.0D0
          IF(OPNM.EQ.'GPUCX   ') OP17=291.0D0
          IF(OPNM.EQ.'GPUCY   ') OP17=292.0D0
          IF(OPNM.EQ.'DIST    ') OP17=293.0D0
          IF(OPNM.EQ.'XFOC    ') OP17=294.0D0
          IF(OPNM.EQ.'YFOC    ') OP17=295.0D0
          IF(OPNM.EQ.'AST     ') OP17=296.0D0
          IF(OPNM.EQ.'SA3     ') OP17=297.0D0
          IF(OPNM.EQ.'XSA3    ') OP17=298.0D0
          IF(OPNM.EQ.'CMA3    ') OP17=299.0D0
          IF(OPNM.EQ.'XCMA3   ') OP17=300.0D0
          IF(OPNM.EQ.'AST3    ') OP17=301.0D0
          IF(OPNM.EQ.'XAST3   ') OP17=302.0D0
          IF(OPNM.EQ.'DIS3    ') OP17=303.0D0
          IF(OPNM.EQ.'XDIS3   ') OP17=304.0D0
          IF(OPNM.EQ.'PTZ3    ') OP17=305.0D0
          IF(OPNM.EQ.'XPTZ3   ') OP17=306.0D0
          IF(OPNM.EQ.'SA5     ') OP17=307.0D0
          IF(OPNM.EQ.'XSA5    ') OP17=308.0D0
          IF(OPNM.EQ.'CMA5    ') OP17=309.0D0
          IF(OPNM.EQ.'XCMA5   ') OP17=310.0D0
          IF(OPNM.EQ.'AST5    ') OP17=311.0D0
          IF(OPNM.EQ.'XAST5   ') OP17=312.0D0
          IF(OPNM.EQ.'DIS5    ') OP17=313.0D0
          IF(OPNM.EQ.'XDIS5   ') OP17=314.0D0
          IF(OPNM.EQ.'PTZ5    ') OP17=315.0D0
          IF(OPNM.EQ.'XPTZ5   ') OP17=316.0D0
          IF(OPNM.EQ.'TOBSA   ') OP17=317.0D0
          IF(OPNM.EQ.'XTOBSA  ') OP17=318.0D0
          IF(OPNM.EQ.'SOBSA   ') OP17=319.0D0
          IF(OPNM.EQ.'XSOBSA  ') OP17=320.0D0
          IF(OPNM.EQ.'ELCMA   ') OP17=321.0D0
          IF(OPNM.EQ.'XELCMA  ') OP17=322.0D0
          IF(OPNM.EQ.'TAS     ') OP17=323.0D0
          IF(OPNM.EQ.'XTAS    ') OP17=324.0D0
          IF(OPNM.EQ.'SAS     ') OP17=325.0D0
          IF(OPNM.EQ.'XSAS    ') OP17=326.0D0
          IF(OPNM.EQ.'SA7     ') OP17=327.0D0
          IF(OPNM.EQ.'XSA7    ') OP17=328.0D0
          IF(OPNM.EQ.'SA3P    ') OP17=329.0D0
          IF(OPNM.EQ.'XSA3P   ') OP17=330.0D0
          IF(OPNM.EQ.'CMA3P   ') OP17=331.0D0
          IF(OPNM.EQ.'XCMA3P  ') OP17=332.0D0
          IF(OPNM.EQ.'AST3P   ') OP17=333.0D0
          IF(OPNM.EQ.'XAST3P  ') OP17=334.0D0
          IF(OPNM.EQ.'DIS3P   ') OP17=335.0D0
          IF(OPNM.EQ.'XDIS3P  ') OP17=336.0D0
          IF(OPNM.EQ.'PTZ3P   ') OP17=337.0D0
          IF(OPNM.EQ.'XPTZ3P  ') OP17=338.0D0
          IF(OPNM.EQ.'SA5P    ') OP17=339.0D0
          IF(OPNM.EQ.'XSA5P   ') OP17=340.0D0
          IF(OPNM.EQ.'CMA5P   ') OP17=341.0D0
          IF(OPNM.EQ.'XCMA5P  ') OP17=342.0D0
          IF(OPNM.EQ.'AST5P   ') OP17=343.0D0
          IF(OPNM.EQ.'XAST5P  ') OP17=344.0D0
          IF(OPNM.EQ.'DIS5P   ') OP17=345.0D0
          IF(OPNM.EQ.'XDIS5P  ') OP17=346.0D0
          IF(OPNM.EQ.'PTZ5P   ') OP17=347.0D0
          IF(OPNM.EQ.'XPTZ5P  ') OP17=348.0D0
          IF(OPNM.EQ.'TOBSAP  ') OP17=349.0D0
          IF(OPNM.EQ.'XTOBSAP ') OP17=350.0D0
          IF(OPNM.EQ.'SOBSAP  ') OP17=351.0D0
          IF(OPNM.EQ.'XSOBSAP ') OP17=352.0D0
          IF(OPNM.EQ.'ELCMAP  ') OP17=353.0D0
          IF(OPNM.EQ.'XELCMAP ') OP17=354.0D0
          IF(OPNM.EQ.'TASP    ') OP17=355.0D0
          IF(OPNM.EQ.'XTASP   ') OP17=356.0D0
          IF(OPNM.EQ.'SASP    ') OP17=357.0D0
          IF(OPNM.EQ.'XSASP   ') OP17=358.0D0
          IF(OPNM.EQ.'SA7P    ') OP17=359.0D0
          IF(OPNM.EQ.'XSA7P   ') OP17=360.0D0
          IF(OPNM.EQ.'SA3S    ') OP17=361.0D0
          IF(OPNM.EQ.'XSA3S   ') OP17=362.0D0
          IF(OPNM.EQ.'CMA3S   ') OP17=363.0D0
          IF(OPNM.EQ.'XCMA3S  ') OP17=364.0D0
          IF(OPNM.EQ.'AST3S   ') OP17=365.0D0
          IF(OPNM.EQ.'XAST3S  ') OP17=366.0D0
          IF(OPNM.EQ.'DIS3S   ') OP17=367.0D0
          IF(OPNM.EQ.'XDIS3S  ') OP17=368.0D0
          IF(OPNM.EQ.'PTZ3S   ') OP17=369.0D0
          IF(OPNM.EQ.'XPTZ3S  ') OP17=370.0D0
          IF(OPNM.EQ.'SA5S    ') OP17=371.0D0
          IF(OPNM.EQ.'XSA5S   ') OP17=372.0D0
          IF(OPNM.EQ.'CMA5S   ') OP17=373.0D0
          IF(OPNM.EQ.'XCMA5S  ') OP17=374.0D0
          IF(OPNM.EQ.'AST5S   ') OP17=375.0D0
          IF(OPNM.EQ.'XAST5S  ') OP17=376.0D0
          IF(OPNM.EQ.'DIS5S   ') OP17=377.0D0
          IF(OPNM.EQ.'XDIS5S  ') OP17=378.0D0
          IF(OPNM.EQ.'PTZ5S   ') OP17=379.0D0
          IF(OPNM.EQ.'XPTZ5S  ') OP17=380.0D0
          IF(OPNM.EQ.'TOBSAS  ') OP17=381.0D0
          IF(OPNM.EQ.'XTOBSAS ') OP17=382.0D0
          IF(OPNM.EQ.'SOBSAS  ') OP17=383.0D0
          IF(OPNM.EQ.'XSOBSAS ') OP17=384.0D0
          IF(OPNM.EQ.'ELCMAS  ') OP17=385.0D0
          IF(OPNM.EQ.'XELCMAS ') OP17=386.0D0
          IF(OPNM.EQ.'TASS    ') OP17=387.0D0
          IF(OPNM.EQ.'XTASS   ') OP17=388.0D0
          IF(OPNM.EQ.'SASS    ') OP17=389.0D0
          IF(OPNM.EQ.'XSASS   ') OP17=390.0D0
          IF(OPNM.EQ.'SA7S    ') OP17=391.0D0
          IF(OPNM.EQ.'XSA7S   ') OP17=392.0D0
          IF(OPNM.EQ.'SA5I    ') OP17=393.0D0
          IF(OPNM.EQ.'XSA5I   ') OP17=394.0D0
          IF(OPNM.EQ.'CMA5I   ') OP17=395.0D0
          IF(OPNM.EQ.'XCMA5I  ') OP17=396.0D0
          IF(OPNM.EQ.'AST5I   ') OP17=397.0D0
          IF(OPNM.EQ.'XAST5I  ') OP17=398.0D0
          IF(OPNM.EQ.'DIS5I   ') OP17=399.0D0
          IF(OPNM.EQ.'XDIS5I  ') OP17=400.0D0
          IF(OPNM.EQ.'PTZ5I   ') OP17=401.0D0
          IF(OPNM.EQ.'XPTZ5I  ') OP17=402.0D0
          IF(OPNM.EQ.'TOBSAI  ') OP17=403.0D0
          IF(OPNM.EQ.'XTOBSAI ') OP17=404.0D0
          IF(OPNM.EQ.'SOBSAI  ') OP17=405.0D0
          IF(OPNM.EQ.'XSOBSAI ') OP17=406.0D0
          IF(OPNM.EQ.'ELCMAI  ') OP17=407.0D0
          IF(OPNM.EQ.'XELCMAI ') OP17=408.0D0
          IF(OPNM.EQ.'TASI    ') OP17=409.0D0
          IF(OPNM.EQ.'XTASI   ') OP17=410.0D0
          IF(OPNM.EQ.'SASI    ') OP17=411.0D0
          IF(OPNM.EQ.'XSASI   ') OP17=412.0D0
          IF(OPNM.EQ.'SA7I    ') OP17=413.0D0
          IF(OPNM.EQ.'XSA7I   ') OP17=414.0D0
          IF(OPNM.EQ.'PSA3    ') OP17=415.0D0
          IF(OPNM.EQ.'XPSA3   ') OP17=416.0D0
          IF(OPNM.EQ.'PCMA3   ') OP17=417.0D0
          IF(OPNM.EQ.'XPCMA3  ') OP17=418.0D0
          IF(OPNM.EQ.'PAST3   ') OP17=419.0D0
          IF(OPNM.EQ.'XPAST3  ') OP17=420.0D0
          IF(OPNM.EQ.'PDIS3   ') OP17=421.0D0
          IF(OPNM.EQ.'XPDIS3  ') OP17=422.0D0
          IF(OPNM.EQ.'PPTZ3   ') OP17=423.0D0
          IF(OPNM.EQ.'XPPTZ3  ') OP17=424.0D0
          IF(OPNM.EQ.'PSA3P   ') OP17=425.0D0
          IF(OPNM.EQ.'XPSA3P  ') OP17=426.0D0
          IF(OPNM.EQ.'PCMA3P  ') OP17=427.0D0
          IF(OPNM.EQ.'XPCMA3P ') OP17=428.0D0
          IF(OPNM.EQ.'PAST3P  ') OP17=429.0D0
          IF(OPNM.EQ.'XPAST3P ') OP17=430.0D0
          IF(OPNM.EQ.'PDIS3P  ') OP17=431.0D0
          IF(OPNM.EQ.'XPDIS3P ') OP17=432.0D0
          IF(OPNM.EQ.'PPTZ3P  ') OP17=433.0D0
          IF(OPNM.EQ.'XPPTZ3P ') OP17=434.0D0
          IF(OPNM.EQ.'PSA3S   ') OP17=435.0D0
          IF(OPNM.EQ.'XPSA3S  ') OP17=436.0D0
          IF(OPNM.EQ.'PCMA3S  ') OP17=437.0D0
          IF(OPNM.EQ.'XPCMA3S ') OP17=438.0D0
          IF(OPNM.EQ.'PAST3S  ') OP17=439.0D0
          IF(OPNM.EQ.'XPAST3S ') OP17=430.0D0
          IF(OPNM.EQ.'PDIS3S  ') OP17=431.0D0
          IF(OPNM.EQ.'XPDIS3S ') OP17=442.0D0
          IF(OPNM.EQ.'PPTZ3S  ') OP17=443.0D0
          IF(OPNM.EQ.'XPPTZ3S ') OP17=444.0D0
          IF(OPNM.EQ.'PTZCV   ') OP17=445.0D0
          IF(OPNM.EQ.'XPTZCV  ') OP17=446.0D0
          IF(OPNM.EQ.'AH      ') OP17=447.0D0
          IF(OPNM.EQ.'AI      ') OP17=448.0D0
          IF(OPNM.EQ.'AJ      ') OP17=449.0D0
          IF(OPNM.EQ.'AK      ') OP17=450.0D0
          IF(OPNM.EQ.'AL      ') OP17=451.0D0
          IF(OPNM.EQ.'GBRADX  ') OP17=452.0D0
          IF(OPNM.EQ.'GBRADY  ') OP17=453.0D0
          IF(OPNM.EQ.'GBDISX  ') OP17=454.0D0
          IF(OPNM.EQ.'GBDISY  ') OP17=455.0D0
          IF(OPNM.EQ.'GBRCVX  ') OP17=456.0D0
          IF(OPNM.EQ.'GBRCVY  ') OP17=457.0D0
          IF(OPNM.EQ.'GBWAISTX') OP17=458.0D0
          IF(OPNM.EQ.'GBWAISTY') OP17=459.0D0
          IF(OPNM.EQ.'MGOTF')    OP17=460.0D0
          IF(OPNM.EQ.'PGOTF')    OP17=461.0D0
          IF(OPNM.EQ.'MDOTF')    OP17=462.0D0
          IF(OPNM.EQ.'PDOTF')    OP17=463.0D0
          IF(OPNM.EQ.'GOTFM')    OP17=460.0D0
          IF(OPNM.EQ.'GOTFP')    OP17=461.0D0
          IF(OPNM.EQ.'DOTFM')    OP17=462.0D0
          IF(OPNM.EQ.'DOTFP')    OP17=463.0D0
          IF(OPNM.EQ.'RED')      OP17=464.0D0
          IF(OPNM.EQ.'REDCEN')   OP17=465.0D0
          IF(OPNM.EQ.'FISHDIST') OP17=466.0D0
          IF(OPNM.EQ.'ZD')       OP17=467.0D0
          IF(OPNM.EQ.'SYMX')     OP17=468.0D0
          IF(OPNM.EQ.'SYMY')     OP17=469.0D0
          IF(OPNM.EQ.'ASYMX')    OP17=470.0D0
          IF(OPNM.EQ.'ASYMY')    OP17=471.0D0
          IF(OPNM.EQ.'PACM')     OP17=472.0D0
          IF(OPNM.EQ.'PACZ')     OP17=473.0D0
          IF(OPNM.EQ.'SACM')     OP17=474.0D0
          IF(OPNM.EQ.'SACZ')     OP17=475.0D0
          IF(OPNM.EQ.'PLCM')     OP17=476.0D0
          IF(OPNM.EQ.'PLCZ')     OP17=477.0D0
          IF(OPNM.EQ.'SLCM')     OP17=478.0D0
          IF(OPNM.EQ.'SLCZ')     OP17=479.0D0
          IF(OPNM.EQ.'CTSX')     OP17=480.0D0
          IF(OPNM.EQ.'CTSY')     OP17=481.0D0
          IF(OPNM.EQ.'SCEX')     OP17=482.0D0
          IF(OPNM.EQ.'SCEY')     OP17=483.0D0
          IF(OPNM.EQ.'GREYS')    OP17=484.0D0
          IF(OPNM.EQ.'PIVX')     OP17=485.0D0
          IF(OPNM.EQ.'PIVY')     OP17=486.0D0
          IF(OPNM.EQ.'PIVZ')     OP17=487.0D0
          IF(OPNM.EQ.'N1')       OP17=488.0D0
          IF(OPNM.EQ.'N2')       OP17=489.0D0
          IF(OPNM.EQ.'N3')       OP17=490.0D0
          IF(OPNM.EQ.'N4')       OP17=491.0D0
          IF(OPNM.EQ.'N5')       OP17=492.0D0
          IF(OPNM.EQ.'N6')       OP17=493.0D0
          IF(OPNM.EQ.'N7')       OP17=494.0D0
          IF(OPNM.EQ.'N8')       OP17=495.0D0
          IF(OPNM.EQ.'N9')       OP17=496.0D0
          IF(OPNM.EQ.'N10')      OP17=497.0D0
          IF(OPNM.EQ.'ABBE')     OP17=498.0D0
          IF(OPNM.EQ.'DPART')    OP17=499.0D0
          IF(OPNM.EQ.'CLPX')     OP17=500.0D0
          IF(OPNM.EQ.'CLPY')     OP17=501.0D0
          IF(OPNM.EQ.'GDX')      OP17=502.0D0
          IF(OPNM.EQ.'GDY')      OP17=503.0D0
          IF(OPNM.EQ.'GDZ')      OP17=504.0D0
          IF(OPNM.EQ.'GALPHA')   OP17=505.0D0
          IF(OPNM.EQ.'GBETA')    OP17=506.0D0
          IF(OPNM.EQ.'GGAMMA')   OP17=507.0D0
          IF(OPNM.EQ.'GRS')      OP17=508.0D0
          IF(OPNM.EQ.'WEIGHT')   OP17=509.0D0
          IF(OPNM.EQ.'DMINUSD')  OP17=510.0D0
          IF(OPNM.EQ.'COST')     OP17=511.0D0
          IF(OPNM.EQ.'MACOPT')   OP17=512.0D0
          IF(OPNM.EQ.'RMSYX')    OP17=513.0D0
          IF(OPNM.EQ.'CLEARX')   OP17=514.0D0
          IF(OPNM.EQ.'CLEARY')   OP17=515.0D0
          IF(OPNM.EQ.'ACT')      OP17=801.0D0
C
          IF(OP17.EQ.512.0D0) THEN
C     MACOPT OPERAND
              IF(INT(W3).LT.1.OR.INT(W3).GT.1000) THEN
                  WRITE(OUTLYNE,*)
     1            'PREDEFINED OPERAND ',OPNM,' REQUIRES'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)
     1            '"NUMERIC WORD #2 IN THE RANGE 1 TO 1000'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IREG=W3
              OP8 =IREG
              OP18 =DBLE(DF3)
              OP9=W4
              OP11=DBLE(DF4)
              OP10 =W5
              OP12 =DBLE(DF5)
          END IF
C
          IF(OP17.GE.480.0D0.AND.OP17.LE.481.0D0) THEN
C     CTSX AND CTSY OPERANDS
              IF(DF5.EQ.0.AND.W5.NE.0.0D0) THEN
                  DF5=1
                  S5=0
                  W5=0.0D0
              END IF
              IREG=W3
              OP8 =IREG
              OP18 =DBLE(DF3)
              OP9=W4
              OP11=DBLE(DF4)
              OP10 =W5
              OP12 =DBLE(DF5)
          END IF
C
          IF(OP17.GE.482.0D0.AND.OP17.LE.483.0D0) THEN
C     SCEX AND SCEY OPERANDS
              IF(DF4.EQ.0.AND.W4.NE.0.0D0.OR.DF5.EQ.0.AND.
     1        W5.NE.0.0D0) THEN
                  DF4=1
                  S4=0
                  W4=0.0D0
                  DF5=1
                  S5=0
                  W5=0.0D0
              END IF
              IREG=W3
              OP8 =IREG
              OP18 =DBLE(DF3)
              OP9=W4
              OP11=DBLE(DF4)
              OP10 =W5
              OP12 =DBLE(DF5)
          END IF
C
          IF(OP17.GE.472.0D0.AND.OP17.LE.479.0D0.OR.OP17.EQ.479.0D0
     1    .OR.OP17.EQ.510.0D0) THEN
C     REAL RAY COLOR OPERANDS, NO INPUT NEEDED TO DEFINE
C     FRACTIONAL OBJECT POS., REL RAY POS OR COLORS.
              IF(DF3.EQ.0.OR.DF4.EQ.0.OR.DF5.EQ.0) THEN
                  DF3=1
                  S3=0
                  W3=0.0D0
                  DF4=1
                  S4=0
                  W4=0.0D0
                  DF5=1
                  S5=0
                  W5=0.0D0
              END IF
              IREG=W3
              OP8 =IREG
              OP18 =DBLE(DF3)
              OP9=W4
              OP11=DBLE(DF4)
              OP10 =W5
              OP12 =DBLE(DF5)
          END IF
C
C     SYMX,SYMY,ASYMX AND ASYMY
          IF(OP17.GE.468.0D0.AND.OP17.LE.471.0D0) THEN
C     W3 ANY VALUE MAY BE INPUT ( ONLY ABS VALUE USED)
              IF(DF3.EQ.1) THEN
                  DF3=0
                  W3=0.7D0
              END IF
              W3=DABS(W3)
C     W4 1 TO 200 (FIELD NUMBER)
              IF(DF4.EQ.1) THEN
                  DF4=0
                  W4=1.0D0
              END IF
C     W5 1 TO 10 (WAVELENGTH NUMBER)
              IF(DF5.EQ.1) THEN
                  DF5=0
                  W5=SYSTEM1(11)
              END IF
C     OUT OF RANGE INPUT W4
              IF(INT(W4).LT.1.OR.INT(W4).GT.200.0D0) THEN
                  WRITE(OUTLYNE,*)
     1            'FIELD POSITION NUMBER BEYOND LEGAL RANGE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)
     1            'FOR PREDEFINED OPERAND ',OPNM
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C     OUT OF RANGE INPUT W5
              IF(INT(W5).LT.1.OR.INT(W5).GT.10.0D0) THEN
                  WRITE(OUTLYNE,*)
     1            'WAVELENGTH NUMBER BEYOND LEGAL RANGE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)
     1            'FOR PREDEFINED OPERAND ',OPNM
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IREG=W3
              OP8 =IREG
              OP18 =DBLE(DF3)
              OP9=W4
              OP11=DBLE(DF4)
              OP10 =W5
              OP12 =DBLE(DF5)
          END IF
C
          IF(OP17.GE.1.0D0.AND.OP17.LE.15.0D0.OR.OP17.GE.18.0D0.AND.OP17
     1    .LE.69.0D0) THEN
C     RAY BASED PREDEFINED OPERANDS
C
              IF(OP17.EQ.68.0D0.OR.OP17.EQ.69.0D0) THEN
                  IF(W3.LT.1.0D0) THEN
                      WRITE(OUTLYNE,*)
     1                'PREDEFINED OPERAND ',OPNM
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'REQUIRES NUMERIC WORD #3 INPUT GREATER THAN 0'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
C
C     DEFAULT NW3
              IF(OP17.GE.1.0D0.AND.OP17.LE.14.0D0.OR.
     1        OP17.GE.18.0D0.AND.OP17.LE.20.0D0.OR.
     2        OP17.GE.22.0D0.AND.OP17.LE.23.0D0.OR.
     3        OP17.GE.27.0D0.AND.OP17.LE.55.0D0) THEN
                  IF(DF3.EQ.1) THEN
                      W3=DBLE(NEWIMG)
                      S1=1
                      DF3=0
                      IREG=W3
                      OP8 =IREG
                      OP18 =DBLE(DF3)
                  END IF
              END IF
C
              IF(OP17.EQ.15.0D0.OR.
     1        OP17.EQ.21D0.OR.
     2        OP17.GE.24.0D0.AND.OP17.LE.26.0D0.OR.
     3        OP17.GE.56.0D0.AND.OP17.LE.69.0D0) THEN
                  IF(DF3.EQ.1) THEN
                      WRITE(OUTLYNE,*)
     1                'PREDEFINED OPERAND ',OPNM
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'REQUIRES EXPLICIT NUMERIC WORD #3 INPUT'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
C
              IF(OP17.GE.43.AND.OP17.LE.58.0D0.OR.OP17.GE.68.0D0.AND.
     1        OP17.LE.69.0D0) THEN
C     REFERENCE RAYS
C     DEFAULT INPUT W4
C
                  IF(DF4.EQ.1) THEN
                      WRITE(OUTLYNE,*)
     1                'PREDEFINED OPERAND ',OPNM
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'REQUIRES EXPLICIT NUMERIC WORD #4 INPUT'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  IF(DF5.EQ.0.AND.W5.NE.0.0D0) THEN
                      DF5=1
                      S5=0
                      W5=0.0D0
                  END IF
                  DF5=0
                  S5=1
                  W5=0

C     OUT OF RANGE INPUT W3
                  IF(INT(W3).LT.NEWOBJ.OR.INT(W3).GT.NEWIMG) THEN
                      WRITE(OUTLYNE,*)
     1                'FOR PREDEFINED OPERAND ',OPNM
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'SURFACE NUMBER BEYOND LEGAL RANGE'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
C     OUT OF RANGE INPUT W4
                  IF(INT(W4).LT.1.OR.INT(W4).GT.200.0D0) THEN
                      WRITE(OUTLYNE,*)
     1                'FIELD POSITION NUMBER BEYOND LEGAL RANGE'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'FOR PREDEFINED OPERAND ',OPNM
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              ELSE
C     NOT REFERENCE RAYS
C     DEFAULT INPUT W4 AND W5
C
                  IF(DF4.EQ.1.OR.DF5.EQ.1) THEN
                      WRITE(OUTLYNE,*)
     1                'PREDEFINED OPERAND ',OPNM
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'REQUIRES EXPLICIT NUMERIC WORD #4 AND #5 INPUT'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF

C     OUT OF RANGE INPUT W3
                  IF(INT(W3).LT.NEWOBJ.OR.INT(W3).GT.NEWIMG) THEN
                      WRITE(OUTLYNE,*)
     1                'FOR PREDEFINED OPERAND ',OPNM
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'SURFACE NUMBER BEYOND LEGAL RANGE'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
C     OUT OF RANGE INPUT W4
                  IF(INT(W4).LT.1.OR.INT(W4).GT.200.0D0) THEN
                      WRITE(OUTLYNE,*)
     1                'FIELD POSITION NUMBER BEYOND LEGAL RANGE'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'FOR PREDEFINED OPERAND ',OPNM
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
C     OUT OF RANGE INPUT W5
                  IF(INT(W5).LT.1.OR.INT(W5).GT.500) THEN
                      WRITE(OUTLYNE,*)
     1                'RAY POSITION NUMBER BEYOND LEGAL RANGE'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'FOR PREDEFINED OPERAND ',OPNM
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
          END IF
C     *****************************************************************
          IF(OP17.EQ.16.0D0.OR.OP17.EQ.17.0D0) THEN
C     OPD OR OPDW
C
              IF(DF3.EQ.1.OR.DF4.EQ.1) THEN
                  WRITE(OUTLYNE,*)
     1            'PREDEFINED OPERAND ',OPNM
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)
     1            'REQUIRES EXPLICIT NUMERIC WORD #3 AND #4 INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C     OUT OF RANGE INPUT W3
              IF(INT(W3).LT.1.OR.INT(W3).GT.200.0D0) THEN
                  WRITE(OUTLYNE,*)
     1            'FIELD POSITION NUMBER BEYOND LEGAL RANGE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)
     1            'FOR PREDEFINED OPERAND ',OPNM
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C     OUT OF RANGE INPUT W4
              IF(INT(W4).LT.1.OR.INT(W4).GT.500) THEN
                  WRITE(OUTLYNE,*)
     1            'RAY POSITION NUMBER BEYOND LEGAL RANGE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)
     1            'FOR PREDEFINED OPERAND ',OPNM
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C     W5
              IF(S5.EQ.1.AND.W5.NE.0.0D0) THEN
                  DF5=1
                  S5=0
                  W5=0.0D0
              END IF
              IREG=W3
              OP8 =IREG
              OP18 =DBLE(DF3)
          END IF
C     *****************************************************************
          IF(OP17.GE.70.0D0.AND.OP17.LE.206.0D0.OR.
     1    OP17.GE.447.0D0.AND.OP17.LE.451.0D0.OR.
     2    OP17.EQ.467.0D0.OR.OP17.GE.485.0D0.AND.OP17.LE.511.0D0.OR.
     3    OP17.GE.801.0D0.AND.OP17.LE.805.0D0) THEN
C     LENS DATABASE OPERANDS
              IF(OP17.EQ.15.0D0.OR.OP17.EQ.21.0D0) THEN
                  IF(W3.LT.1.0D0) THEN
                      WRITE(OUTLYNE,*)
     1                'PREDEFINED OPERAND ',OPNM
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'REQUIRES NUMERIC WORD #3 GREATER THAN 0'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
              IF(OP17.GE.801.0D0.AND.OP17.LE.805.0D0) THEN
                  IF(DF3.EQ.1) THEN
                      WRITE(OUTLYNE,*)
     1                'PREDEFINED OPERAND ',OPNM
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'REQUIRES EXPLICIT NUMERIC WORD #3 INPUT'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  IF(W3.LT.0.0D0.OR.W3.GT.SYSTEM1(20)) THEN
                      WRITE(OUTLYNE,*)
     1                'PREDEFINED OPERAND ',OPNM
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'REQUIRES A VALID SURFACE NUMBER FOR NUMERIC WORD #3'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
              IF(OP17.EQ.801.0D0) THEN
                  IF(DF4.EQ.1) THEN
                      WRITE(OUTLYNE,*)
     1                'PREDEFINED OPERAND ',OPNM
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'REQUIRES EXPLICIT NUMERIC WORD #4 INPUT'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  IF(W4.LT.1.0D0.OR.W4.GT.(ALENS(105,INT(W3))**2)) THEN
                      WRITE(OUTLYNE,*)
     1                'PREDEFINED OPERAND ',OPNM
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'REQUIRES AN ACTIVE ACTUATOR NUMBER AS NUMERIC WORD #4'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
              IF(OP17.GE.802.0D0.AND.OP17.LE.805.0D0) THEN
                  IF(DF4.EQ.0.OR.DF5.EQ.0) THEN
                      DF4=1
                      S4=0
                      W4=0.0D0
                      DF5=1
                      S5=0
                      W5=0.0D0
                  END IF
              END IF
              IF(OP17.EQ.801.0D0) THEN
                  IF(DF5.EQ.0) THEN
                      DF5=1
                      S5=0
                      W5=0.0D0
                  END IF
              END IF
C     DEFAULT INPUT W5
              IF(DF5.EQ.0.AND.W5.NE.0.0D0) THEN
                  DF5=1
                  S5=0
                  W5=0.0D0
              END IF
              IF(OP17.NE.91.0D0.AND.OP17.NE.106.0D0.AND.
     1        OP17.NE.107.0D0.AND.OP17.NE.509.0D0.AND.OP17.NE.511.0D0) THEN
                  IF(DF5.EQ.0.AND.W5.NE.0.0D0) THEN
                      DF5=1
                      S5=0
                      W5=0.0D0
                  END IF
              END IF
C     SET DEFAULTS FOR LENGTH AND MLENGTH AND WEIGHT
              IF(OP17.EQ.106.0D0.OR.
     1        OP17.EQ.107.0D0.OR.OP17.EQ.509.0D0.OR.OP17.EQ.511.0D0) THEN
                  IF(W3.LT.0.0D0) W3=SYSTEM1(20)+W3
                  IF(W4.LT.0.0D0) W4=SYSTEM1(20)+W4
                  IF(DF3.EQ.1) W3=0.0D0
                  IF(DF3.EQ.1) DF3=0
                  IF(DF4.EQ.1) W4=SYSTEM1(20)
                  OP9 = W4
                  IF(DF4.EQ.1) DF4=0
                  OP11 =DBLE(DF4)
                  IREG=W3
                  OP8=W3
                  OP18=DBLE(DF3)
              END IF
C     SET DEFAULTS FOR VERTEX DATA
              IF(OP17.GE.94.0D0.AND.OP17.LE.105.0D0) THEN
                  IF(DF3.EQ.1) THEN
                      WRITE(OUTLYNE,*)
     1                'PREDEFINED OPERAND ',OPNM
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'REQUIRES EXPLICIT NUMERIC WORD #3 INPUT'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  IF(DF4.EQ.1) THEN
                      WRITE(OUTLYNE,*)
     1                'PREDEFINED OPERAND ',OPNM
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'REQUIRES EXPLICIT NUMERIC WORD #4 INPUT'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
C     SET SURFACE DATA NUMERIC WORD #3
              IF(OP17.GE.70.0D0.AND.
     1        OP17.LE.89.0D0.OR.OP17.GE.447.0D0.AND.
     2        OP17.LE.451.0D0.OR.OP17.GE.92.0D0.AND.
     3        OP17.LE.93.0D0.OR.OP17.EQ.467.0D0.OR.OP17.GE.108.0D0.AND.
     4        OP17.LE.206.0D0.OR.OP17.GE.485.0D0.AND.OP17.LE.499.0D0.OR.
     5        OP17.GE.500.0D0.AND.OP17.LE.508.0D0) THEN
                  IF(DF3.EQ.1) THEN
                      WRITE(OUTLYNE,*)
     1                'PREDEFINED OPERAND ',OPNM
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'REQUIRES EXPLICIT NUMERIC WORD #3 INPUT'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
              IF(OP17.GE.500.0D0.AND.OP17.LE.508.0D0) THEN
                  IF(DF4.EQ.0.AND.W4.NE.0.0D0.OR.DF5.EQ.0.AND.W5.NE.0.0D0) THEN
                      DF4=1
                      S4=0
                      W4=0.0D0
                      DF5=1
                      S5=0
                      W5=0.0D0
                  END IF
              END IF
C
              IF(OP17.GE.70.0D0.AND.OP17.LE.90.0D0.OR.
     1        OP17.GE.92.0D0.AND.
     1        OP17.LE.93.0D0.OR.OP17.EQ.467.0D0.OR.OP17.GE.
     2        108.0D0.AND.OP17.LE.206.0D0.OR.OP17.GE.447.0D0
     3        .AND.OP17.LE.451.0D0.OR.OP17.GE.485.0D0.AND.OP17.LE.499.0D0.OR.
     4        OP17.GE.500.0D0.AND.OP17.LE.508.0D0) THEN
                  IF(W3.LT.0.0D0) W3=SYSTEM1(20)+W3
                  IF(W3.LT.0.0D0.OR.W3.GT.SYSTEM1(20)) THEN
C     BAD SURFACE NUMBER
                      WRITE(OUTLYNE,*)
     1                'INVALID SURFACE NUMBER ISSUED FOR'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'PREDEFINED OPERAND ',OPNM
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
C
              IF(OP17.GE.94.0D0.AND.OP17.LE.105.0D0) THEN
                  IF(W3.LT.DBLE(NEWOBJ).OR.W3.GT.DBLE(NEWIMG)) THEN
C     BAD SURFACE NUMBER
                      WRITE(OUTLYNE,*)
     1                'INVALID SURFACE NUMBER, NW3, ISSUED FOR'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'PREDEFINED OPERAND ',OPNM
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  IF(W4.LT.DBLE(NEWOBJ).OR.W4.GT.DBLE(NEWIMG)) THEN
C     BAD SURFACE NUMBER
                      WRITE(OUTLYNE,*)
     1                'INVALID SURFACE NUMBER, NW4, ISSUED FOR'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'PREDEFINED OPERAND ',OPNM
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
C
              IF(OP17.EQ.106.0D0.OR.OP17.EQ.107.0D0.OR.OP17.EQ.509.0D0
     1        .OR.OP17.EQ.511.0D0) THEN
                  IF(W3.LT.0.0D0) W3=SYSTEM1(20)+W3
                  IF(W4.LT.0.0D0) W4=SYSTEM1(20)+W4
                  IF(W3.LT.0.0D0.OR.W3.GT.SYSTEM1(20).OR.W3.GE.W4.OR.W4.LT.0.0D0
     1            .OR.W4.GT.SYSTEM1(20)) THEN
C     BAD SURFACE NUMBER
                      WRITE(OUTLYNE,*)
     1                'INVALID SURFACE NUMBERS ISSUED FOR'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'PREDEFINED OPERAND ',OPNM
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
C
              IF(OP17.EQ.91.0D0) THEN
                  IF(W3.LT.0.0D0) W3=SYSTEM1(20)+W3
                  IF(W3.LT.0.0D0.OR.W3.GT.SYSTEM1(20)) THEN
C     BAD SURFACE NUMBER
                      WRITE(OUTLYNE,*)
     1                'INVALID SURFACE NUMBER ISSUED FOR'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'PREDEFINED OPERAND ',OPNM
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
C
          END IF
          IF(OP17.GE.207.0D0.AND.OP17.LE.236.0D0) THEN
C
              IF(OP17.GE.207.0D0.AND.OP17.LE.236.0D0) THEN
C     PARAXIAL OPERANDS
C     DEFAULT INPUT W5
                  IF(DF5.EQ.0.AND.W5.NE.0.0D0) THEN
                      DF5=1
                      S5=0
                      W5=0.0D0
                  END IF
              END IF
              IF(OP17.GE.227.0D0.AND.OP17.LE.234.0D0) THEN
                  IF(S4.EQ.1.AND.W4.NE.0.0D0.OR.S5.EQ.1.AND.W5.NE.0.0D0) THEN
                      DF4=1
                      S4=0
                      W4=0.0D0
                      DF5=1
                      S5=0
                      W5=0.0D0
                  END IF
              END IF
C     SET DEFAULTS FOR PWRY,PWRX,FLCLTH OR FLCLTHY AND FLCLTHX
              IF(OP17.GE.207.0D0.AND.
     1        OP17.LE.210.0D0) THEN
                  IF(W3.LT.0.0D0) W3=SYSTEM1(20)+W3
                  IF(W4.LT.0.0D0) W4=SYSTEM1(20)+W4
                  IF(DF3.EQ.1) W3=0.0D0
                  IF(DF3.EQ.1) DF3=0
                  IF(DF4.EQ.1) W4=SYSTEM1(20)
                  OP9 = W4
                  IF(DF4.EQ.1) DF4=0
                  OP11 =DBLE(DF4)
                  IREG=W3
                  OP8=W3
                  OP18=DBLE(DF3)
                  DF5=1
                  W5=0.0D0
              END IF
C     SET DEFAULTS FOR 211 TO 226
              IF(OP17.GE.211.0D0.AND.OP17.LE.226.0D0) THEN
                  IF(W3.LT.0.0D0) W3=SYSTEM1(20)+W3
                  IF(DF3.EQ.1) W3=SYSTEM1(20)
                  IF(DF3.EQ.1) DF3=0
                  IF(DF4.EQ.1) W4=SYSTEM1(11)
                  OP9 = W4
                  IF(DF4.EQ.1) DF4=0
                  OP11 =DBLE(DF4)
                  IREG=W3
                  OP8=W3
                  OP18=DBLE(DF3)
              END IF
C     SET DEFAULTS FOR 227 TO 234
              IF(OP17.GE.227.0D0.AND.OP17.LE.234.0D0) THEN
                  IF(W3.LT.0.0D0) W3=SYSTEM1(20)+W3
                  IF(DF3.EQ.1) W3=SYSTEM1(20)
                  IREG=W3
                  OP8=W3
                  OP18=DBLE(DF3)
              END IF
C     SET NW3 FOR 235 TO 236
              IF(OP17.GE.235.0D0.AND.OP17.LE.236.0D0) THEN
                  IF(DF3.EQ.1) THEN
                      WRITE(OUTLYNE,*)
     1                'PREDEFINED OPERAND ',OPNM
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'REQUIRES EXPLICIT NUMERIC WORD #3 INPUT'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
C
C     OUT OF RANGE INPUT
C
              IF(OP17.GE.207.0D0.AND.OP17.LE.210.0D0) THEN
                  IF(W3.LT.0.0D0) W3=SYSTEM1(20)+W3
                  IF(W4.LT.0.0D0) W4=SYSTEM1(20)+W4
                  IF(W3.LT.0.0D0.OR.W3.GT.SYSTEM1(20).OR.
     1            W4.LT.0.0D0.OR.W4.GT.SYSTEM1(20).OR.
     1            W4.LE.W3) THEN
C     BAD SURFACE NUMBERS
                      WRITE(OUTLYNE,*)
     1                'INVALID SURFACE NUMBERS ISSUED FOR'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'PREDEFINED OPERAND ',OPNM
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
              IF(OP17.GE.211.0D0.AND.OP17.LE.226.0D0) THEN
                  IF(W3.LT.0.0D0) W3=SYSTEM1(20)+W3
                  IF(W3.LT.0.0D0.OR.W3.GT.SYSTEM1(20)) THEN
C     BAD SURFACE NUMBER
                      WRITE(OUTLYNE,*)
     1                'INVALID SURFACE NUMBER ISSUED FOR'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'PREDEFINED OPERAND ',OPNM
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  IF(W4.LT.1.0D0.OR.W4.GT.10.0D0) THEN
C     BAD WAVELENGTH NUMBER
                      WRITE(OUTLYNE,*)
     1                'INVALID WAVELENGTH NUMBER ISSUED FOR'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'PREDEFINED OPERAND ',OPNM
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  ELSE
                      W4=DBLE(INT(W4))
                      OP9 = W4
                  END IF
              END IF
              IF(OP17.GE.235.0D0.AND.OP17.LE.236.0D0) THEN
                  IF(W3.LT.0.0D0) W3=SYSTEM1(20)+W3
                  IF(W3.LT.0.0D0.OR.W3.GT.SYSTEM1(20)) THEN
C     BAD SURFACE NUMBER
                      WRITE(OUTLYNE,*)
     1                'INVALID SURFACE NUMBER ISSUED FOR'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'PREDEFINED OPERAND ',OPNM
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
C
          END IF
C
C     *****************************************************************
          IF(OP17.GE.237.0D0.AND.OP17.LE.245.0D0.OR.OP17.EQ.513.0D0) THEN
C     SPOT OPERANDS
C     DEFAULT INPUT W5
              IF(DF5.EQ.0.AND.W5.NE.0.0D0) THEN
                  DF5=1
                  S5=0
                  W5=0.0D0
              END IF
C     SPOT OPERANDS
C     DEFAULT INPUT W3
              IF(DF3.EQ.1) THEN
                  WRITE(OUTLYNE,*)
     1            'PREDEFINED OPERAND ',OPNM
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)
     1            'REQUIRES EXPLICIT NUMERIC WORD #3 INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C
C     OUT OF RANGE INPUT
C
              IF(W3.LT.1.0D0.OR.W3.GT.200.0D0) THEN
C     BAD FIELD POS NUMBER
                  WRITE(OUTLYNE,*)
     1            'INVALID FIELD POSITION NUMBER ISSUED FOR'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)
     1            'PREDEFINED OPERAND ',OPNM
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C
C     OUT OF RANGE INPUT
C
              IF(W4.LE.0.0D0.OR.W4.GT.10.0D0) THEN
                  W4=0.0D0
                  DF4=1
              END IF
              IF(DF4.EQ.0) THEN
                  IF(W4.NE.1.0D0.AND.
     1            W4.NE.2.0D0.AND.
     1            W4.NE.3.0D0.AND.
     1            W4.NE.4.0D0.AND.
     1            W4.NE.5.0D0.AND.
     1            W4.NE.6.0D0.AND.
     1            W4.NE.7.0D0.AND.
     1            W4.NE.8.0D0.AND.
     1            W4.NE.9.0D0.AND.
     1            W4.NE.10.0D0) THEN
C     BAD WAVELENGTH NUMBER
                      WRITE(OUTLYNE,*)
     1                'INVALID WAVELENGTH NUMBER ISSUED FOR'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'PREDEFINED OPERAND ',OPNM
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
              OP8=W3
              OP18=DBLE(DF3)
              OP19=0.0D0
              OP9=W4
              OP11=DBLE(DF4)
C
          END IF
C     *****************************************************************
          IF(OP17.EQ.514.0D0.OR.OP17.EQ.515) THEN
C     SPOT OPERANDS
C     NO DEFAULTS
              IF(DF3.EQ.1.OR.DF4.EQ.1.OR.DF5.EQ.1) THEN
                  WRITE(OUTLYNE,*)
     1            'PREDEFINED OPERAND ',OPNM
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)
     1            'REQUIRES EXPLICIT NUMERIC WORDS #3, #4 AND #5 INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C FOB1,RAY1
              CLFOB1=DABS(DBLE(INT(W3)))
              CLRAY1=DBLE(NINT(DABS(DABS(W3)-DBLE(INT(DABS(W3))))*10000.0D0))
              CLFOB2=DABS(DBLE(INT(W4)))
              CLRAY2=DBLE(NINT(DABS(DABS(W4)-DBLE(INT(DABS(W4))))*10000.0D0))
              CLSRF1=DABS(DBLE(INT(W5)))
              CLSRF2=DBLE(NINT(DABS(DABS(W5)-DBLE(INT(DABS(W5))))*1000.0D0))
              IF(CLFOB1.LT.1.0D0.OR.CLFOB1.GT.200.0D0) THEN
                  WRITE(OUTLYNE,*)
     1            'PREDEFINED OPERAND ',OPNM
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)
     1            'FIRST FIELD OF VIEW POSITION OUT OF RANGE (1-200)'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(CLFOB2.LT.1.0D0.OR.CLFOB2.GT.200.0D0) THEN
                  WRITE(OUTLYNE,*)
     1            'PREDEFINED OPERAND ',OPNM
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)
     1            'SECOND FIELD OF VIEW POSITION OUT OF RANGE (1-200)'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(CLRAY1.LT.1.0D0.OR.CLRAY1.GT.5000.0D0) THEN
                  WRITE(OUTLYNE,*)
     1            'PREDEFINED OPERAND ',OPNM
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)
     1            'FIRST RAY POSITION OUT OF RANGE (1-5000)'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(CLRAY2.LT.1.0D0.OR.CLRAY2.GT.5000.0D0) THEN
                  WRITE(OUTLYNE,*)
     1            'PREDEFINED OPERAND ',OPNM
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)
     1            'SECOND RAY POSITION OUT OF RANGE (1-5000)'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(CLSRF1.LT.0.0D0.OR.CLSRF1.GT.DBLE(MAXSUR)) THEN
                  WRITE(OUTLYNE,*)
     1            'PREDEFINED OPERAND ',OPNM
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)
     1            'FIRST SURFACE NUMBER OUT OF RANGE (1-',MAXSUR,')'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(CLSRF2.LT.0.0D0.OR.CLSRF2.GT.DBLE(MAXSUR)) THEN
                  WRITE(OUTLYNE,*)
     1            'PREDEFINED OPERAND ',OPNM
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)
     1            'SECOND SURFACE NUMBER OUT OF RANGE (1-',MAXSUR,')'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF

              OP8=W3
              OP18=DBLE(DF3)
              OP19=0.0D0
              OP9=W4
              OP11=DBLE(DF4)
              OP10 =W5
              OP12 =DBLE(DF5)
          END IF
C     *****************************************************************
          IF(OP17.EQ.246.0D0.OR.OP17.GE.460.0D0.AND.
     1    OP17.LE.465.0D0.OR.OP17.EQ.484.0D0) THEN
C     CAPFN OPERANDS AND SECOND GROUP OF SPOT OPERANDS
              IF(OP17.EQ.246.0D0.OR.OP17.GE.460.0D0.AND.
     1        OP17.LE.465.0D0.OR.OP17.EQ.484.0D0) THEN
C     SPOT OPERANDS
C     DEFAULT INPUT W3
                  IF(DF3.EQ.1) THEN
                      WRITE(OUTLYNE,*)
     1                'PREDEFINED OPERAND ',OPNM
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'REQUIRES EXPLICIT NUMERIC WORD #3 INPUT'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
C
C     OUT OF RANGE INPUT
C
              IF(OP17.EQ.246.0D0.OR.OP17.GE.460.0D0.AND.
     1        OP17.LE.465.0D0.OR.OP17.EQ.484.0D0) THEN
                  IF(W3.LT.1.0D0.OR.W3.GT.200.0D0) THEN
C     BAD FIELD POS NUMBER
                      WRITE(OUTLYNE,*)
     1                'INVALID FIELD POSITION NUMBER ISSUED FOR'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'PREDEFINED OPERAND ',OPNM
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
              IF(OP17.EQ.246.0D0) THEN
                  IF(W4.LT.1.0D0.OR.W4.GT.37.0D0) THEN
C     BAD ZERN NUMBER
                      WRITE(OUTLYNE,*)
     1                'INVALID ZERN COEFFICIENT NUMBER ISSUED FOR'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'PREDEFINED OPERAND ',OPNM
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  ELSE
                      OP9=W4
                      OP11=DBLE(DF4)
                  END IF
              END IF
              IF(OP17.EQ.246.0D0) THEN
                  IF(DF4.EQ.1) THEN
C     BAD ZERN NUMBER
                      WRITE(OUTLYNE,*)
     1                'PREDEFINED OPERAND ',OPNM
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'REQUIRES EXPLICIT NUMERIC WORD #4 INPUT'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
              IF(OP17.EQ.484.0D0) THEN
                  IF(DF4.EQ.1) THEN
                      DF4=0
                      S4=1
                      W4=1.0D0
                  END IF
                  IF(W4.LT.0.0D0) THEN
C     BAD OPDWEIGT
                      WRITE(OUTLYNE,*)
     1                'OPD WEIGHT MUST BE GREATER THAN ZERO'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'PREDEFINED OPERAND ',OPNM
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  ELSE
                      OP9=W4
                      OP11=DBLE(DF4)
                  END IF
              END IF
              IF(OP17.EQ.464.0D0.OR.OP17.EQ.465.0D0) THEN
                  IF(W4.LE.0.0D0.OR.W4.GT.100.0D0) THEN
C     BAD % NUMBER
                      WRITE(OUTLYNE,*)
     1                'INVALID ENCIRLED ENERGY PERCENTAGE ISSUED FOR'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'PREDEFINED OPERAND ',OPNM
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  ELSE
                      OP9=W4
                      OP11=DBLE(DF4)
                  END IF
              END IF
              IF(OP17.GE.460.0D0.AND.OP17.LE.465.0D0) THEN
                  IF(DF4.EQ.1) THEN
C     BAD W4
                      WRITE(OUTLYNE,*)
     1                'PREDEFINED OPERAND ',OPNM
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'REQUIRES EXPLICIT NUMERIC WORD #4 INPUT'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
              IF(OP17.EQ.246.0D0.OR.OP17.EQ.484.0D0) THEN
                  IF(DF5.EQ.1) W5=SYSTEM1(11)
                  OP10 =W5
                  IF(DF5.EQ.1) DF5=0
                  OP12 =DBLE(DF5)
              END IF
              IF(OP17.EQ.246.0D0.OR.OP17.EQ.484.0D0) THEN
                  IF(W5.LT.1.0D0.OR.W5.GT.10.0D0) THEN
C     BAD WAVELENGTH NUMBER
                      WRITE(OUTLYNE,*)
     1                'INVALID WAVELENGTH NUMBER ISSUED FOR'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'PREDEFINED OPERAND ',OPNM
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  ELSE
                      W5=DBLE(INT(W5))
                      OP10 =W5
                      OP12 =DBLE(DF5)
                  END IF
              END IF
              IF(OP17.GE.464.0D0.AND.OP17.LE.465.0D0) THEN
                  IF(DF5.EQ.0) THEN
                      DF5=1
                      S5=0
                      W5=0.0D0
                  ELSE
                      W5=DBLE(INT(W5))
                      OP10 =W5
                      OP12 =DBLE(DF5)
                  END IF
              END IF
              IF(OP17.GE.460.0D0.AND.OP17.LE.463.0D0) THEN
                  IF(DF5.EQ.1) THEN
                      WRITE(OUTLYNE,*)
     1                'EXPLICIT NUMERIC WORD #5 INPUT REQUIRED WITH'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'PREDEFINED OPERAND ',OPNM
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  ELSE
                      W5=DBLE(INT(W5))
                      OP10 =W5
                      OP12 =DBLE(DF5)
                  END IF
              END IF
              IF(OP17.GE.460.0D0.AND.OP17.LE.463.0D0) THEN
                  IF(DF5.EQ.1.AND.W1.NE.0.0D0.AND.W2.NE.90.0D0) THEN
                      WRITE(OUTLYNE,*)
     1                'NUMERIC WORD #5, ORIENTATION VALUE MUST BE 0 OR 90 FOR'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'PREDEFINED OPERAND ',OPNM
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  ELSE
                      W5=DBLE(INT(W5))
                      OP10 =W5
                      OP12 =DBLE(DF5)
                  END IF
              END IF
          END IF
C     *****************************************************************
C
          IF(OP17.GE.247.0D0.AND.OP17.LE.296.0D0.OR.
     1    OP17.GE.452.0D0.AND.OP17.LE.459.0D0) THEN
C
              IF(OP17.GE.247.0D0.AND.OP17.LE.296.0D0) THEN
C     SPECIAL OPERANDS
C     DEFAULT INPUT W5
                  IF(DF5.EQ.0.AND.W5.NE.0.0D0) THEN
                      DF5=1
                      S5=0
                      W5=0.0D0
                  END IF
              END IF
              IF(OP17.GE.247.0D0.AND.OP17.LE.284.OR.
     1        OP17.GE.293.0D0.AND.OP17.LE.296.0D0.OR.OP17.EQ.466.0D0) THEN
C     CAPFN OPERANDS
C     DEFAULT INPUT W4
                  IF(DF4.EQ.0.AND.W4.NE.0.0D0) THEN
                      DF4=1
                      S4=0
                      W4=0.0D0
                  END IF
              END IF
              IF(OP17.GE.247.0D0.AND.OP17.LE.296.0D0) THEN
C     SPECIAL OPERANDS
C     DEFAULT INPUT W3
                  IF(DF3.EQ.1) THEN
                      WRITE(OUTLYNE,*)
     1                'PREDEFINED OPERAND ',OPNM
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'REQUIRES EXPLICIT NUMERIC WORD #3 INPUT'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
C
C     OUT OF RANGE INPUT
C
              IF(OP17.GE.247.0D0.AND.OP17.LE.278.0D0.OR.
     1        OP17.LE.293.0D0.AND.OP17.LE.296.0D0.OR.OP17.EQ.466.0D0) THEN
                  IF(W3.LT.1.0D0.OR.W3.GT.200.0D0) THEN
C     BAD FIELD POS NUMBER
                      WRITE(OUTLYNE,*)
     1                'INVALID FIELD POSITION NUMBER ISSUED FOR'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'PREDEFINED OPERAND ',OPNM
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
              IF(OP17.GE.279.0D0.AND.OP17.LE.284.0D0) THEN
                  IF(W3.LT.0.0D0) W3=SYSTEM1(20)+W3
                  IF(W3.LT.1.0D0.OR.W3.GT.SYSTEM1(20)) THEN
C     BAD FIELD POS NUMBER
                      WRITE(OUTLYNE,*)
     1                'INVALID SURFACE NUMBER ISSUED FOR'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'PREDEFINED OPERAND ',OPNM
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
              IF(OP17.GE.285.0D0.AND.OP17.LE.292.0D0) THEN
                  IF(DF3.EQ.1) THEN
C     BAD SURF NUMBER
                      WRITE(OUTLYNE,*)
     1                'EXPLICIT SURFACE NUMBER REQUIRED FOR'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'PREDEFINED OPERAND ',OPNM
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  IF(DF4.EQ.1) THEN
C     BAD FIELD NUMBER
                      WRITE(OUTLYNE,*)
     1                'EXPLICIT FIELD NUMBER REQUIRED FOR'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'PREDEFINED OPERAND ',OPNM
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  IF(W4.LT.1.0D0.OR.W4.GT.200.0D0) THEN
C     BAD FIELD POS NUMBER
                      WRITE(OUTLYNE,*)
     1                'INVALID FIELD POSITION NUMBER ISSUED FOR'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'PREDEFINED OPERAND ',OPNM
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  IF(W3.LT.DBLE(NEWOBJ).OR.W3.GT.DBLE(NEWIMG)) THEN
C     BAD SURF NUMBER
                      WRITE(OUTLYNE,*)
     1                'INVALID SURFACE NUMBER ISSUED FOR'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'PREDEFINED OPERAND ',OPNM
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
              IF(OP17.GE.452.0D0.AND.OP17.LE.459.0D0) THEN
                  IF(DF3.EQ.1) THEN
                      DF3=0
                      W3=INT(SYSTEM1(20))
                  END IF
              END IF
              IF(OP17.GE.452.0D0.AND.OP17.LE.459.0D0) THEN
                  IF(DF4.EQ.1) THEN
C     BAD FIELD NUMBER
                      WRITE(OUTLYNE,*)
     1                'EXPLICIT FIELD NUMBER REQUIRED FOR'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'PREDEFINED OPERAND ',OPNM
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  IF(DF5.EQ.0) THEN
                      DF5=1
                      S5=0
                      W5=0.0D0
                  END IF
                  IF(W4.LT.1.0D0.OR.W4.GT.200.0D0) THEN
C     BAD FIELD POS NUMBER
                      WRITE(OUTLYNE,*)
     1                'INVALID FIELD POSITION NUMBER ISSUED FOR'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'PREDEFINED OPERAND ',OPNM
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  IF(W3.LT.0.0D0) W3=SYSTEM1(20)+W3
                  IF(W3.LT.0.0D0.OR.W3.GT.SYSTEM1(20)) THEN
C     BAD FIELD SURF NUMBER
                      WRITE(OUTLYNE,*)
     1                'INVALID SURFACE NUMBER ISSUED FOR'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'PREDEFINED OPERAND ',OPNM
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
C
          END IF
C
C     *****************************************************************
          IF(OP17.GE.297.0D0.AND.OP17.LE.446.0D0) THEN
C     3,5,7 ABERRATIONS
C
C     DEFAULT INPUT W4 AND W5
              IF(DF4.EQ.0.AND.W4.NE.0.0D0.OR.DF5.EQ.0.AND.W5.NE.0.0D0) THEN
                  DF4=1
                  DF5=1
                  S4=0
                  S5=0
                  W4=0.0D0
                  W5=0.0D0
              END IF
              IF(W3.LT.0.0D0) W3=SYSTEM1(20)+W3
              IF(DF3.EQ.1) W3=SYSTEM1(20)
              IF(DF3.EQ.1) DF3=0
              IREG=W3
              OP8=W3
              OP18=DBLE(DF3)
C
              IF(W3.LT.1.0D0.OR.W3.GT.SYSTEM1(20)) THEN
C     BAD SURFACE NUMBER
                  WRITE(OUTLYNE,*)
     1            'INVALID SURFACE NUMBER ISSUED FOR'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)
     1            'PREDEFINED OPERAND ',OPNM
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C
          END IF
C
C     INPUT OK FOR W3, W4 AND W5
C
 3141     CONTINUE
C     ANY 8 CHARACTER QUALIFIER WORD OPERAND NAME MAY BE USED
C     BUT DUPLICATE NAMES CAUSE REPLACEMENT
C     HERE IS WERE THE REPLACEMENT HAPPENS
          DO I=1,OPCNT
              IF(WQ.EQ.OPNAM(I).AND.CURFIG.EQ.OPERND(I,16).AND.
     1        W3.EQ.OPERND(I,8).AND.W4.EQ.OPERND(I,9)
     1        .AND.W5.EQ.OPERND(I,10).AND.OPT.EQ.OPERND(I,1)
     1        .AND.DBLE(CORMOD).EQ.OPERND(I,13)) THEN
C     FUNC, NAME, NW3,NW4,NW5,CFG AND CORMOD MUST MATCH OR NO REPLACEMENT
C
                  OP1 =DBLE(OPT)
                  OPNM=WQ
                  OP2 =W1
                  OP3 =0.0D0
                  OP4 =0.0D0
                  OP5 =0.0D0
                  OP6 =0.0D0
                  OP7 =OPWEIT
                  IREG=W3
                  OP8 =IREG
                  OP18 =DBLE(DF3)
                  OP9 = W4
                  OP10 =W5
                  OP11 =DBLE(DF4)
                  OP12 =DBLE(DF5)
                  OP13 =DBLE(CORMOD)
                  OP14 = 0.0D0
                  OP15 = 0.0D0
                  IF(OPT.EQ.0) OP16 = CURFIG
                  IF(OPT.NE.0) OP16 = 0.0D0
                  IREG=W3
                  OP8=W3
                  OP18=DBLE(DF3)
                  OP19=0.0D0
C
                  OPERND(I,1) =OP1
                  OPERND(I,2) =OP2
                  OPERND(I,3) =OP3
                  OPERND(I,4) =OP4
                  OPERND(I,5) =OP5
                  OPERND(I,6) =OP6
                  OPERND(I,7) =OP7
                  OPERND(I,8) =OP8
                  OPERND(I,9) =OP9
                  OPERND(I,10) =OP10
                  OPERND(I,11) =OP11
                  OPERND(I,12) =OP12
                  OPERND(I,13) =OP13
                  OPERND(I,14) =OP14
                  OPERND(I,15) =OP15
                  OPERND(I,16) =OP16
                  OPERND(I,17) =OP17
                  OPERND(I,18) =OP18
                  OPERND(I,19) =OP19
                  CALL OPRCLN
                  RETURN
              END IF
          END DO
C
C     IT IS A NEW OPERAND
C     CHECK FOR A POSSIBLE OVERFLOW
          IF(OPCNT.EQ.MAXOPT) THEN
C     NO MORE OPERANDS
              WRITE(OUTLYNE,*)
     1        'THE MAXIMUM OF ',MAXOPT,' OPERANDS HAS BEEN REACHED'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'NO MORE MAY BE ENTERED'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          OP1 =DBLE(OPT)
          OPNM=WQ
          OP2 =W1
          OP3 =0.0D0
          OP4 =0.0D0
          OP5 =0.0D0
          OP6 =0.0D0
          OP7 =OPWEIT
          IREG=W3
          OP8 =IREG
          OP18 =DBLE(DF3)
          OP9 = W4
          OP10 =W5
          OP11 =DBLE(DF4)
          OP12 =DBLE(DF5)
          OP13 =DBLE(CORMOD)
          OP14 = 0.0D0
          OP15 = 0.0D0
          IREG=W3
          OP18=DBLE(DF3)
          OP19=0.0D0
          IF(OPT.EQ.0) OP16 = CURFIG
          IF(OPT.NE.0) OP16 = 0.0D0
C
          OPNAM(OPCNT+1)=OPNM
          OPERND(OPCNT+1,1) =OP1
          OPERND(OPCNT+1,2) =OP2
          OPERND(OPCNT+1,3) =OP3
          OPERND(OPCNT+1,4) =OP4
          OPERND(OPCNT+1,5) =OP5
          OPERND(OPCNT+1,6) =OP6
          OPERND(OPCNT+1,7) =OP7
          OPERND(OPCNT+1,8) =OP8
          OPERND(OPCNT+1,9) =OP9
          OPERND(OPCNT+1,10) =OP10
          OPERND(OPCNT+1,11) =OP11
          OPERND(OPCNT+1,12) =OP12
          OPERND(OPCNT+1,13) =OP13
          OPERND(OPCNT+1,14) =OP14
          OPERND(OPCNT+1,15) =OP15
          OPERND(OPCNT+1,16) =OP16
          OPERND(OPCNT+1,17) =OP17
          OPERND(OPCNT+1,18) =OP18
          OPERND(OPCNT+1,19) =OP19
          OPCNT=OPCNT+1
          CALL OPRCLN
          RETURN
C       ALL DONE
      END
