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

C       FIFTH FILE FOR LENS DATABASE MANAGER FILES

C SUB SNAO.FOR
      SUBROUTINE SNAO
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE SNAO WHICH IMPLEMENTS THE NAO(X OR Y) COMMAND
C       AT THE LENS OF UPDATE LENS LEVEL OR THE NAO(X OR Y) COMMAND AT
C       THE CMD LEVEL
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
          IF(F1.EQ.1) THEN
C
C               CHECK FOR ADDITIONAL INPUT AND
C               PRINT ERROR AND RETURN IF DISCOVERED.
C
              IF(SQ.EQ.1.OR.SN.EQ.1.OR.SST.EQ.1) THEN
                  OUTLYNE='AT THE CMD LEVEL, "NAOY" AND "NAOX"'
                  CALL SHOWIT(1)
                  OUTLYNE='TAKE NO EXPLICIT INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C
              IF(STI.EQ.1.OR.STI.EQ.0) THEN
                  IF(WC.EQ.'NAOY') THEN
                      IF(SYSTEM1(64).NE.1.0D0.AND.SYSTEM1(64).NE.3.0D0) THEN
                          OUTLYNE='NOTE:'
                          CALL SHOWIT(1)
                          OUTLYNE='"'//WC(1:4)//'" HAS NOT BEEN EXPLICITLY SET'
                          CALL SHOWIT(1)
                          IF(INT(SYSTEM1(11)).GE.1.AND.INT(SYSTEM1(11)).LE.5) THEN
                              SYSTEM1(65)=(ALENS(45+INT(SYSTEM1(11)),0)*SYSTEM1(12))/
     1                        DSQRT((ALENS(3,0)**2)+(SYSTEM1(12)**2))
                              SYSTEM1(83)=0.0D0
                              SYSTEM1(84)=0.0D0
                          END IF
                          IF(INT(SYSTEM1(11)).GE.6.AND.INT(SYSTEM1(11)).LE.10) THEN
                              SYSTEM1(65)=(ALENS(70-5+INT(SYSTEM1(11)),0)*SYSTEM1(12))/
     1                        DSQRT((ALENS(3,0)**2)+(SYSTEM1(12)**2))
                              SYSTEM1(83)=0.0D0
                              SYSTEM1(84)=0.0D0
                          END IF
                      END IF
                      WRITE(OUTLYNE,2000) SYSTEM1(65)
                      CALL SHOWIT(0)
                      RETURN
                  END IF
                  IF(WC.EQ.'NAOX') THEN
                      IF(SYSTEM1(64).NE.2.0D0.AND.SYSTEM1(64).NE.3.0D0) THEN
                          OUTLYNE='NOTE:'
                          CALL SHOWIT(1)
                          OUTLYNE='"'//WC(1:4)//'" HAS NOT BEEN EXPLICITLY SET'
                          CALL SHOWIT(1)
                          IF(INT(SYSTEM1(11)).GE.1.AND.INT(SYSTEM1(11)).LE.5) THEN
                              SYSTEM1(66)=(ALENS(45+INT(SYSTEM1(11)),0)*SYSTEM1(13))/
     1                        DSQRT((ALENS(3,0)**2)+(SYSTEM1(13)**2))
                              SYSTEM1(83)=0.0D0
                              SYSTEM1(84)=0.0D0
                          END IF
                          IF(INT(SYSTEM1(11)).GE.6.AND.INT(SYSTEM1(11)).LE.10) THEN
                              SYSTEM1(66)=(ALENS(70-5+INT(SYSTEM1(11)),0)*SYSTEM1(13))/
     1                        DSQRT((ALENS(3,0)**2)+(SYSTEM1(13)**2))
                              SYSTEM1(83)=0.0D0
                              SYSTEM1(84)=0.0D0
                          END IF
                      END IF
                      WRITE(OUTLYNE,2001)
                      CALL SHOWIT(0)
                      WRITE(OUTLYNE,3000) SYSTEM1(66)
                      CALL SHOWIT(0)
                      RETURN
                  END IF
              END IF
C               NOT AT CMD LEVEL
          END IF
C
          IF(F5.EQ.1.OR.F6.EQ.1) THEN
              IF(STI.EQ.1) THEN
                  OUTLYNE=
     1            'QUERRY OBJECT N.A. VALUES FROM THE CMD LEVEL WITH THE'
                  CALL SHOWIT(1)
                  OUTLYNE=
     1            '"NAOY" OR "NAOX" COMMANDS'
                  CALL SHOWIT(1)
                  RETURN
              END IF
C
C               CHECK FOR PRESENCE OF QUALIFIER OR STRING INPUT
C               PRINT ERROR AND RETURN IF DISCOVERED.
C
              IF(SST.EQ.1) THEN
C
                  IF(WC.EQ.'NAOY') THEN
                      OUTLYNE='"NAOY" TAKES NO STRING INPUT'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  IF(WC.EQ.'NAOX') THEN
                      OUTLYNE='"NAOX" TAKES NO STRING INPUT'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
              IF(S2.EQ.1.OR.S3.EQ.1
     1        .OR.S4.EQ.1.OR.S5.EQ.1) THEN
C
                  IF(WC.EQ.'NAOY') THEN
                      OUTLYNE='"NAOY" TAKES NO NUMERIC WORD #2 THROUGH #5 INPUT'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  IF(WC.EQ.'NAOX') THEN
                      OUTLYNE='"NAOX" TAKES NO NUMERIC WORD #2 THROUGH #5 INPUT'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
              IF(SQ.EQ.1.AND.F5.EQ.1) THEN
C
                  IF(WC.EQ.'NAOY') THEN
                      OUTLYNE='"NAOY" TAKES NO QUALIFIER WORD IN LENS INPUT MODE'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  IF(WC.EQ.'NAOX') THEN
                      OUTLYNE='"NAOX" TAKES NO QUALIFIER WORD IN LENS INPUT MODE'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
              IF(F6.EQ.1) THEN
                  IF(WQ.NE.'        '.AND.WQ.NE.'DELT'.AND.WQ.NE.'CENT') THEN
C
                      IF(WC.EQ.'NAOY') THEN
                          OUTLYNE='INVALID QUALIFIER WORD USED WITH "NAOY"'
                          CALL SHOWIT(1)
                          OUTLYNE='RE-ENTER COMMAND'
                          CALL SHOWIT(1)
                          CALL MACFAL
                          RETURN
                      END IF
                      IF(WC.EQ.'NAOX') THEN
                          OUTLYNE='INVALID QUALIFIER WORD USED WITH "NAOX"'
                          CALL SHOWIT(1)
                          OUTLYNE='RE-ENTER COMMAND'
                          CALL SHOWIT(1)
                          CALL MACFAL
                          RETURN
                      END IF
                  END IF
              END IF
              IF(DF1.EQ.1) THEN
C
                  IF(WC.EQ.'NAOY') THEN
                      OUTLYNE='"NAOY" REQUIRES EXPLICIT NUMERIC WORD #1 INPUT'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  IF(WC.EQ.'NAOX') THEN
                      OUTLYNE='"NAOX" REQUIRES EXPLICIT NUMERIC WORD #1 INPUT'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
              IF(DABS(ALENS(3,0)).GE.1.0D10) THEN
C     INFINITE OBJECT THICKNESS, DON'T ALLOW
                  IF(WC.EQ.'NAOY') THEN
                      OUTLYNE='OBJECT DISTANCE IS GREATER THAN +/-1.0D10'
                      CALL SHOWIT(1)
                      OUTLYNE='"NAOY" MAY NOT BE USED'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  IF(WC.EQ.'NAOX') THEN
                      OUTLYNE='OBJECT DISTANCE IS GREATER THAN +/-1.0D10'
                      CALL SHOWIT(1)
                      OUTLYNE='"NAOX" MAY NOT BE USED'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
C
C               WE ARE AT LENS INPUT OR LENS UPDATE LEVEL
C
              IF(S1.EQ.0) THEN
                  IF(WC.EQ.'NAOY') THEN
                      WRITE(OUTLYNE,2000) SYSTEM1(65)
                      CALL SHOWIT(0)
                  END IF
                  IF(WC.EQ.'NAOX') THEN
                      WRITE(OUTLYNE,3000) SYSTEM1(66)
                      CALL SHOWIT(0)
                      IF(SYSTEM1(49).EQ.0.0D0) SYSTEM1(49)=1.0D0
                      IF(SYSTEM1(49).EQ.2.0D0) SYSTEM1(49)=3.0D0
                  END IF
                  RETURN
              ELSE
                  IF(W1.EQ.0.0D0) THEN
                      IF(WC.EQ.'NAOY') THEN
                          OUTLYNE='"NAOY" MAY NOT BE SET TO ZERO'
                          CALL SHOWIT(1)
                          OUTLYNE='RE-ENTER COMMAND'
                          CALL SHOWIT(1)
                          CALL MACFAL
                          RETURN
                      END IF
                      IF(WC.EQ.'NAOX') THEN
                          OUTLYNE='"NAOX" MAY NOT BE SET TO ZERO'
                          CALL SHOWIT(1)
                          OUTLYNE='RE-ENTER COMMAND'
                          CALL SHOWIT(1)
                          CALL MACFAL
                          RETURN
                      END IF
                      RETURN
                  END IF
                  IF(SQ.EQ.0) THEN
                      IF(WC.EQ.'NAOY') THEN
                          SYSTEM1(83)=0.0D0
                          SYSTEM1(84)=0.0D0
                          IF(SYSTEM1(64).NE.3.0D0.AND.SYSTEM1(64).NE.1.0D0) THEN
                              IF(SYSTEM1(64).EQ.0.0D0) SYSTEM1(64)=1.0D0
                              IF(SYSTEM1(64).EQ.2.0D0) SYSTEM1(64)=3.0D0
                          END IF
                      END IF
                      IF(WC.EQ.'NAOX') THEN
                          IF(SYSTEM1(64).NE.3.0D0.AND.SYSTEM1(64).NE.2.0D0) THEN
                              IF(SYSTEM1(64).EQ.0.0D0) SYSTEM1(64)=2.0D0
                              IF(SYSTEM1(64).EQ.1.0D0) SYSTEM1(64)=3.0D0
                              SYSTEM1(83)=0.0D0
                              SYSTEM1(84)=0.0D0
                          END IF
                      END IF
                      SYSTEM1(67)=0.0D0
                      IF(WC.EQ.'NAOY') SYSTEM1(65)=DABS(W1)
                      IF(WC.EQ.'NAOX') SYSTEM1(66)=DABS(W1)
                  END IF
                  IF(WQ.EQ.'DELT') THEN
                      SYSTEM1(67)=0.0D0
                      IF(WC.EQ.'NAOY') SYSTEM1(65)=SYSTEM1(65)+(W1)
                      IF(WC.EQ.'NAOX') SYSTEM1(66)=SYSTEM1(66)+(W1)
                  END IF
                  IF(WQ.EQ.'CENT') THEN
                      SYSTEM1(67)=0.0D0
                      IF(WC.EQ.'NAOY') SYSTEM1(65)=SYSTEM1(65)+(W1*0.0D0*SYSTEM1(65))
                      IF(WC.EQ.'NAOX') SYSTEM1(66)=SYSTEM1(66)+(W1*0.0D0*SYSTEM1(66))
                  END IF
                  RETURN
              END IF
          END IF
 2001     FORMAT(1X)
 2000     FORMAT('NAOY=',1X,D23.15)
 3000     FORMAT('NAOX=',1X,D23.15)
          RETURN
      END
C SUB SMODE.FOR
      SUBROUTINE SMODE
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE SMODE WHICH IMPLEMENTS THE MODE
C       COMMAND AT THE LENS INPUT, UPDATE LENS AND CMD LEVEL.
C
          INTEGER AMODE
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
          IF(STI.EQ.1) THEN
              IF(F5.EQ.1.OR.F6.EQ.1) THEN
                  AMODE=INT(SYSTEM1(30))
                  IF(AMODE.EQ.1) THEN
                      WRITE(OUTLYNE,1000)
                      CALL SHOWIT(0)
                  ELSE
                  END IF
                  IF(AMODE.EQ.2) THEN
                      WRITE(OUTLYNE,2000)
                      CALL SHOWIT(0)
                  ELSE
                  END IF
                  IF(AMODE.EQ.3) THEN
                      WRITE(OUTLYNE,3000)
                      CALL SHOWIT(0)
                  ELSE
                  END IF
                  IF(AMODE.EQ.4) THEN
                      WRITE(OUTLYNE,4000)
                      CALL SHOWIT(0)
                  ELSE
                  END IF
                  RETURN
              ELSE
C       AT CMD
              END IF
          ELSE
C       NOT STI
          END IF
C       BEHAVIOR AT CMD LEVEL
          AMODE=INT(SYSTEM1(30))
          IF(F1.EQ.1) THEN
C
C               CHECK FOR ADDITIONAL INPUT AND
C               PRINT ERROR AND RETURN IF DISCOVERED.
C
              IF(SST.EQ.1.OR.SN.EQ.1) THEN
                  OUTLYNE='"MODE" TAKES NO STRING OR NUMERIC INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C
C       MODE CHANGE AT CMD LEVEL. INORDER TO BE COMPATABLE
C       WITH ACCOS V AND HEXAGON, THE MODE MAY BE CHANGED AT
C       THE CMD LEVEL.
C
              IF(SQ.EQ.1) THEN
                  IF(WQ.EQ.'FOCAL')THEN
                      SYSTEM1(30)=1.0
                      IF(F12.EQ.1) SYSP(30)=1.0
                      RETURN
                  ELSE
                  END IF
                  IF(WQ.EQ.'UFOCAL')THEN
                      SYSTEM1(30)=2.0
                      IF(F12.EQ.1) SYSP(30)=2.0
                      RETURN
                  ELSE
                  END IF
                  IF(WQ.EQ.'AFOCAL')THEN
                      SYSTEM1(30)=3.0
                      IF(F12.EQ.1) SYSP(30)=3.0
                      RETURN
                  ELSE
                  END IF
                  IF(WQ.EQ.'UAFOCAL')THEN
                      SYSTEM1(30)=4.0
                      IF(F12.EQ.1) SYSP(30)=4.0
                      RETURN
                  ELSE
                  END IF
                  OUTLYNE='INVALID QUALIFIER INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C
              IF(AMODE.EQ.1) THEN
                  WRITE(OUTLYNE,1000)
                  CALL SHOWIT(0)
              ELSE
              END IF
              IF(AMODE.EQ.2) THEN
                  WRITE(OUTLYNE,2000)
                  CALL SHOWIT(0)
              ELSE
              END IF
              IF(AMODE.EQ.3) THEN
                  WRITE(OUTLYNE,3000)
                  CALL SHOWIT(0)
              ELSE
              END IF
              IF(AMODE.EQ.4) THEN
                  WRITE(OUTLYNE,4000)
                  CALL SHOWIT(0)
              ELSE
              END IF
              RETURN
          ELSE
C       NOT AT CMD LEVEL
          END IF
C
C
C       BEHAVIOR AT LENS INPUT AND LENS UPDATE LEVEL
C
          IF(F5.EQ.1.OR.F6.EQ.1) THEN
C
C               CHECK FOR PRESENCE OF STRING OR NUMERIC
C               INPUT AND
C               PRINT ERROR AND RETURN IF DISCOVERED.
C
              IF(SST.EQ.1.OR.SN.EQ.1) THEN
                  OUTLYNE='"MODE" ACCEPTS NO STRING OR NUMERIC INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C
C               WE ARE AT LENS INPUT OR LENS UPDATE LEVEL
C
              IF(SQ.EQ.0) THEN
                  OUTLYNE='"MODE" REQUIRES EXPLICIT NUMERIC INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  RETURN
              END IF
C
              IF(WQ.NE.'FOCAL'.AND.WQ.NE.'UFOCAL'.AND.
     1        WQ.NE.'AFOCAL'.AND.WQ.NE.'UAFOCAL'.AND.WQ.NE.' ') THEN
                  OUTLYNE='INVALID QUALIFIER WORD'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              ELSE
                  IF(WQ.EQ.'FOCAL') THEN
                      SYSTEM1(30)=1.0
                      IF(F12.EQ.1) SYSP(30)=1.0
                  ELSE
                  END IF
                  IF(WQ.EQ.'UFOCAL') THEN
                      SYSTEM1(30)=2.0
                      IF(F12.EQ.1) SYSP(30)=2.0
                  ELSE
                  END IF
                  IF(WQ.EQ.'AFOCAL') THEN
                      SYSTEM1(30)=3.0
                      IF(F12.EQ.1) SYSP(30)=3.0
                  ELSE
                  END IF
                  IF(WQ.EQ.'UAFOCAL') THEN
                      SYSTEM1(30)=4.0
                      IF(F12.EQ.1) SYSP(30)=4.0
                  ELSE
                  END IF
              END IF
C
          ELSE
          END IF
 1000     FORMAT('EVALUATION MODE IS FOCAL')
 2000     FORMAT('EVALUATION MODE IS UFOCAL')
 3000     FORMAT('EVALUATION MODE IS AFOCAL')
 4000     FORMAT('EVALUATION MODE IS UAFOCAL')
C
          RETURN
      END
C SUB SMAG.FOR
      SUBROUTINE SMAG
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE SMAG WHICH IMPLEMENTS THE MAGY AND MAGX
C       COMMANDS AT THE CMD LEVEL.
C
          REAL*8 MAG
C
          INTEGER I,J
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
C               CHECK FOR ADDITIONAL INPUT AND
C               PRINT ERROR AND RETURN IF DISCOVERED.
C
          IF(SST.EQ.1.OR.SQ.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
C
              IF(WC.EQ.'MAGY') THEN
                  OUTLYNE='"MAGY" TAKES ONLY NUMERIC WORDS #1,#2 AND #3 INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              ELSE
              END IF
              IF(WC.EQ.'MAGX') THEN
                  OUTLYNE='"MAGX" TAKES ONLY NUMERIC WORD #1, #2 AND #3 INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
          IF(DF1.EQ.1) THEN
              IF(WC.EQ.'MAGY') THEN
                  OUTLYNE=
     1            '"MAGY" REQUIRES EXPLICIT NUMERIC WORD #1 INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              ELSE
              END IF
              IF(WC.EQ.'MAGX') THEN

                  OUTLYNE=
     1            '"MAGX" REQUIRES EXPLICIT NUMERIC WORD #1 INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
          IF(DF2.EQ.1) W2=0.0D0
          IF(DF2.EQ.1) W3=SYSTEM1(20)
          IF(W2.LT.0.0.OR.W3.GT.SYSTEM1(20)) THEN
              OUTLYNE=
     1        'REQUESTED SURFACES BEYOND LEGAL RANGE FOR CURRENT LENS'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C       NUMERIC INPUT NOT DEFAULT
          IF(WC.EQ.'MAGY') THEN
              IF(W1.EQ.0.0D0) W1=1.0D0
              IF(W2.EQ.0.0D0) W2=0.0D0
              IF(W3.EQ.0.0D0) W3=SYSTEM1(20)
              GO TO 25
          ELSE
C       WC MUST BE 'MAGX'
              IF(W1.EQ.0.0D0) W1=1.0D0
              IF(W2.EQ.0.0D0) W2=0.0D0
              IF(W3.EQ.0.0D0) W3=SYSTEM1(20)
          END IF
          GO TO 35
 25       CONTINUE
          MAG=W1
          I=INT(W2)
          J=INT(W3)
C       HERE IS WHERE THE MAGY ADJUSTMENT IS MADE BY CALLING
C       SUBROUTINE MGYADJ.FOR
          CALL MGADJ(MAG,I,J)
          RETURN
 35       CONTINUE
          MAG=W1
          I=INT(W2)
          J=INT(W3)
C       HERE IS WHERE THE MAGX ADJUSTMENT IS MADE BY CALLING
C       SUBROUTINE MGXADJ.FOR
          CALL MGADJ(MAG,I,J)
          RETURN
      END
C SUB SLVRS.FOR
      SUBROUTINE SLVRS
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE SLVRS . IT IS USED TO HANDLE
C       THE RESOLUTION OF ALL Y-Z/X-Z PLANE SOLVES ON SURFACE SL.
C       IT IS CALLED PRIMARILY FROM SUBROUTINE PRTRA.
C       CAY AND CAX SOLVES DO AMAMORPHIC ASPERES AS OF 1 FEB 1992
C
          INTEGER TAR,L,I,SLV1,SLV2,FINY
C
          COMMON/CSLVRS/SLV1,SLV2
C
          COMMON/FINER/FINY
C
          REAL*8 ARG,DIS,EDGVAL,SAGL,SAGLP1,N,J_NP
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
          L=SLV1
          IF(SLV2.EQ.1) THEN
C       THE SURFACE OF SOLVE EVALUATION IS THE PASSED PARAMETER L.
C       NOW HANDLE ALL SOLVES ON SURFACE L
C       SIMPLIFY THE REPRESENTATION OF THE REFRACTIVE INDICES
C       AT THE CONTROL WAVELENGTH AT L-1 AND L
              IF(INT(SYSTEM1(11)).GE.1.AND.INT(SYSTEM1(11)).LE.5) THEN
                  N=ALENS(45+INT(SYSTEM1(11)),(L-1))
                  J_NP=ALENS(45+INT(SYSTEM1(11)),(L))
              END IF
              IF(INT(SYSTEM1(11)).GE.6.AND.INT(SYSTEM1(11)).LE.10) THEN
                  N=ALENS(65+INT(SYSTEM1(11)),(L-1))
                  J_NP=ALENS(65+INT(SYSTEM1(11)),(L))
              END IF
C
C                       FIRST,CURVATURE SOLVES
C***************************************************************************
C                       PUY SOLVES
C***************************************************************************
C       IS THERE A PUY SOLVE ON SURFACE L
              IF(SOLVE(8,L).EQ.3.0D0) THEN
C       YES THERE IS A PUY SOLVE ON SURFACE L, HANDLE IT
C       FOR A PUY SOLVE
C       CV(L)=(-SOLVE TARGET+(N/N')*PUY(L-1))*(N'/(N'-N))*(1/PY(L))
C       AND PUY(L)=SOLVE TARGET
C       IS SURFACE L AN X-TORIC,IF SO THE TORIC CURVATURE AND
C       NOT THE MAIN CURVATURE IS CHANGED.
                  IF(ALENS(23,L).EQ.2.0D0) THEN
C       SURFACE IS X-TORIC
                      PXTRAY(2,L)=SOLVE(9,L)
                      IF(PXTRAY(1,L).EQ.0.0D0.OR.(J_NP-N).EQ.0.0D0) THEN
                          ALENS(24,L)=0.0D0
                      ELSE
                          ALENS(24,L)=((-PXTRAY(2,L))+((N/J_NP)*PXTRAY(2,(L-1))))*
     1                    (J_NP/(J_NP-N))*(1.0D0/PXTRAY(1,L))
                      END IF
                  ELSE
C       SURFACE NOT X-TORIC,PROCEED
                      PXTRAY(2,L)=SOLVE(9,L)
                      IF(PXTRAY(1,L).EQ.0.0D0.OR.(J_NP-N).EQ.0.0D0) THEN
                          ALENS(1,L)=0.0D0
C
                          IF(ALENS(1,L).EQ.0.0D0.AND.ALENS(2,L).NE.0.0D0) THEN
                              ALENS(2,L)=0.0D0
                              OUTLYNE='WARNING:'
                              CALL SHOWIT(1)
                              WRITE(OUTLYNE,*) 'FOR SURFACE ',L
                              CALL SHOWIT(1)
                              OUTLYNE=
     1                        'THE CONIC CONSTANT RESET TO 0.0 FOR THIS PLANO SURFACE'
                              CALL SHOWIT(1)
                          END IF
                          IF(ALENS(1,L).NE.0.0D0.AND.ALENS(43,L).NE.0.0D0) THEN
                              ALENS(43,L)=0.0D0
                              OUTLYNE='WARNING:'
                              CALL SHOWIT(1)
                              WRITE(OUTLYNE,*) 'FOR SURFACE ',L
                              CALL SHOWIT(1)
                              OUTLYNE=
     1                        'THE "AC" TERM RESET TO 0.0 FOR THIS NON-PLANO SURFACE'
                              CALL SHOWIT(1)
                          END IF
C
                      ELSE
                          ALENS(1,L)=((-PXTRAY(2,L))+((N/J_NP)*PXTRAY(2,(L-1))))*
     1                    (J_NP/(J_NP-N))*(1.0D0/PXTRAY(1,L))
C
                          IF(ALENS(1,L).EQ.0.0D0.AND.ALENS(2,L).NE.0.0D0) THEN
                              ALENS(2,L)=0.0D0
                              OUTLYNE='WARNING:'
                              CALL SHOWIT(1)
                              WRITE(OUTLYNE,*) 'FOR SURFACE ',L
                              CALL SHOWIT(1)
                              OUTLYNE=
     1                        'THE CONIC CONSTANT RESET TO 0.0 FOR THIS PLANO SURFACE'
                              CALL SHOWIT(1)
                          END IF
                          IF(ALENS(1,L).NE.0.0D0.AND.ALENS(43,L).NE.0.0D0) THEN
                              ALENS(43,L)=0.0D0
                              OUTLYNE='WARNING:'
                              CALL SHOWIT(1)
                              WRITE(OUTLYNE,*) 'FOR SURFACE ',L
                              CALL SHOWIT(1)
                              OUTLYNE=
     1                        'THE "AC" TERM RESET TO 0.0 FOR THIS NON-PLANO SURFACE'
                              CALL SHOWIT(1)
                          END IF
C
                      END IF
                  END IF
C       PUY SOLVE FOR SURFACE L HANDLED.
              ELSE
C       NO PUY SOLVE
              END IF
C*****************************************************************************
C                       PUCY SOLVES
C***************************************************************************
C       IS THERE A PUCY SOLVE ON SURFACE L
              IF(SOLVE(8,L).EQ.6.0D0) THEN
C       YES THERE IS A PUCY SOLVE ON SURFACE L, HANDLE IT
C       FOR A PUCY SOLVE
C       CV(L)=(-SOLVE TARGET+(N/N')*PUCY(L-1))*(N'/(N'-N))*(1/PCY(L))
C       AND PUCY(L)=SOLVE TARGET
C       IS SURFACE L AN X-TORIC,IF SO THE TORIC CURVATURE AND
C       NOT THE MAIN CURVATURE IS CHANGED.
                  IF(ALENS(23,L).EQ.2.0D0) THEN
C       SURFACE IS X-TORIC
                      PXTRAY(6,L)=SOLVE(9,L)
                      IF(PXTRAY(5,L).EQ.0.0D0.OR.(J_NP-N).EQ.0.0D0) THEN
                          ALENS(24,L)=0.0D0
                      ELSE
                          ALENS(24,L)=((-PXTRAY(6,L))+((N/J_NP)*PXTRAY(6,(L-1))))*
     1                    (J_NP/(J_NP-N))*(1.0D0/PXTRAY(5,L))
                      END IF
                  ELSE
C       SURFACE NOT X-TORIC,PROCEED
                      PXTRAY(6,L)=SOLVE(9,L)
                      IF(PXTRAY(5,L).EQ.0.0D0.OR.(J_NP-N).EQ.0.0D0) THEN
                          ALENS(1,L)=0.0D0
C
                          IF(ALENS(1,L).EQ.0.0D0.AND.ALENS(2,L).NE.0.0D0) THEN
                              ALENS(2,L)=0.0D0
                              OUTLYNE='WARNING:'
                              CALL SHOWIT(1)
                              WRITE(OUTLYNE,*) 'FOR SURFACE ',L
                              CALL SHOWIT(1)
                              OUTLYNE=
     1                        'THE CONIC CONSTANT RESET TO 0.0 FOR THIS PLANO SURFACE'
                              CALL SHOWIT(1)
                          END IF
                          IF(ALENS(1,L).NE.0.0D0.AND.ALENS(43,L).NE.0.0D0) THEN
                              ALENS(43,L)=0.0D0
                              OUTLYNE='WARNING:'
                              CALL SHOWIT(1)
                              WRITE(OUTLYNE,*) 'FOR SURFACE ',L
                              CALL SHOWIT(1)
                              OUTLYNE=
     1                        'THE "AC" TERM RESET TO 0.0 FOR THIS NON-PLANO SURFACE'
                              CALL SHOWIT(1)
                          END IF
C
                      ELSE
                          ALENS(1,L)=((-PXTRAY(6,L))+((N/J_NP)*PXTRAY(6,(L-1))))*
     1                    (J_NP/(J_NP-N))*(1.0D0/PXTRAY(5,L))
C
                          IF(ALENS(1,L).EQ.0.0D0.AND.ALENS(2,L).NE.0.0D0) THEN
                              ALENS(2,L)=0.0D0
                              OUTLYNE='WARNING:'
                              CALL SHOWIT(1)
                              WRITE(OUTLYNE,*) 'FOR SURFACE ',L
                              CALL SHOWIT(1)
                              OUTLYNE=
     1                        'THE CONIC CONSTANT RESET TO 0.0 FOR THIS PLANO SURFACE'
                              CALL SHOWIT(1)
                          END IF
                          IF(ALENS(1,L).NE.0.0D0.AND.ALENS(43,L).NE.0.0D0) THEN
                              ALENS(43,L)=0.0D0
                              OUTLYNE='WARNING:'
                              CALL SHOWIT(1)
                              WRITE(OUTLYNE,*) 'FOR SURFACE ',L
                              CALL SHOWIT(1)
                              OUTLYNE=
     1                        'THE "AC" TERM RESET TO 0.0 FOR THIS NON-PLANO SURFACE'
                              CALL SHOWIT(1)
                          END IF
C
                      END IF
                  END IF
C       PUCY SOLVE FOR SURFACE L HANDLED.
              ELSE
C       NO PUCY SOLVE
              END IF
C***************************************************************************
C                       PIY SOLVES
C***************************************************************************
C       IS THERE A PIY SOLVE ON SURFACE L
              IF(SOLVE(8,L).EQ.2.0D0) THEN
C       YES THERE IS A PIY SOLVE ON SURFACE L, HANDLE IT
C       FOR A PIY SOLVE
C       CV(L)=(SOLVE TARGET-PUY(L-1))/PY(L)
C       AND PIY(L)=SOLVE TARGET
C       IS SURFACE L AN X-TORIC,IF SO THE TORIC CURVATURE AND
C       NOT THE MAIN CURVATURE IS CHANGED.
                  IF(ALENS(23,L).EQ.2.0D0) THEN
C       SURFACE IS X-TORIC
                      PXTRAY(3,L)=SOLVE(9,L)
                      IF(PXTRAY(1,L).EQ.0.0D0) THEN
                          ALENS(24,L)=0.0D0
                      ELSE
                          ALENS(24,L)=(PXTRAY(3,L)-PXTRAY(2,(L-1)))/
     1                    (PXTRAY(1,(L)))
                      END IF
                  ELSE
C       SURFACE NOT X-TORIC,PROCEED
                      PXTRAY(3,L)=SOLVE(9,L)
                      IF(PXTRAY(1,L).EQ.0.0D0) THEN
                          ALENS(1,L)=0.0D0
C
                          IF(ALENS(1,L).EQ.0.0D0.AND.ALENS(2,L).NE.0.0D0) THEN
                              ALENS(2,L)=0.0D0
                              OUTLYNE='WARNING:'
                              CALL SHOWIT(1)
                              WRITE(OUTLYNE,*) 'FOR SURFACE ',L
                              CALL SHOWIT(1)
                              OUTLYNE=
     1                        'THE CONIC CONSTANT RESET TO 0.0 FOR THIS PLANO SURFACE'
                              CALL SHOWIT(1)
                          END IF
                          IF(ALENS(1,L).NE.0.0D0.AND.ALENS(43,L).NE.0.0D0) THEN
                              ALENS(43,L)=0.0D0
                              OUTLYNE='WARNING:'
                              CALL SHOWIT(1)
                              WRITE(OUTLYNE,*) 'FOR SURFACE ',L
                              CALL SHOWIT(1)
                              OUTLYNE=
     1                        'THE "AC" TERM RESET TO 0.0 FOR THIS NON-PLANO SURFACE'
                              CALL SHOWIT(1)
                          END IF
C
                      ELSE
                          ALENS(1,L)=(PXTRAY(3,L)-PXTRAY(2,(L-1)))/
     1                    (PXTRAY(1,(L)))
C
                          IF(ALENS(1,L).EQ.0.0D0.AND.ALENS(2,L).NE.0.0D0) THEN
                              ALENS(2,L)=0.0D0
                              OUTLYNE='WARNING:'
                              CALL SHOWIT(1)
                              WRITE(OUTLYNE,*) 'FOR SURFACE ',L
                              CALL SHOWIT(1)
                              OUTLYNE=
     1                        'THE CONIC CONSTANT RESET TO 0.0 FOR THIS PLANO SURFACE'
                              CALL SHOWIT(1)
                          END IF
                          IF(ALENS(1,L).NE.0.0D0.AND.ALENS(43,L).NE.0.0D0) THEN
                              ALENS(43,L)=0.0D0
                              OUTLYNE='WARNING:'
                              CALL SHOWIT(1)
                              WRITE(OUTLYNE,*) 'FOR SURFACE ',L
                              CALL SHOWIT(1)
                              OUTLYNE=
     1                        'THE "AC" TERM RESET TO 0.0 FOR THIS NON-PLANO SURFACE'
                              CALL SHOWIT(1)
                          END IF
C
                      END IF
                  END IF
C       PIY SOLVE FOR SURFACE L HANDLED.
              ELSE
C       NO PIY SOLVE
              END IF
C*****************************************************************************
C                       PICY SOLVES
C***************************************************************************
C       IS THERE A PICY SOLVE ON SURFACE L
              IF(SOLVE(8,L).EQ.5.0D0) THEN
C       YES THERE IS A PICY SOLVE ON SURFACE L, HANDLE IT
C       FOR A PICY SOLVE
C       CV(L)=(SOLVE TARGET-PUCY(L-1))/PCY(L)
C       AND PICY(L)=SOLVE TARGET
C       IS SURFACE L AN X-TORIC,IF SO THE TORIC CURVATURE AND
C       NOT THE MAIN CURVATURE IS CHANGED.
                  IF(ALENS(23,L).EQ.2.0D0) THEN
C       SURFACE IS X-TORIC
                      PXTRAY(7,L)=SOLVE(9,L)
                      IF(PXTRAY(5,L).EQ.0.0D0) THEN
                          ALENS(24,L)=0.0D0
                      ELSE
                          ALENS(24,L)=(PXTRAY(7,L)-PXTRAY(6,(L-1)))/
     1                    (PXTRAY(5,(L)))
                      END IF
                  ELSE
C       SURFACE NOT X-TORIC,PROCEED
                      PXTRAY(7,L)=SOLVE(9,L)
                      IF(PXTRAY(5,L).EQ.0.0D0) THEN
                          ALENS(1,L)=0.0D0
C
                          IF(ALENS(1,L).EQ.0.0D0.AND.ALENS(2,L).NE.0.0D0) THEN
                              ALENS(2,L)=0.0D0
                              OUTLYNE='WARNING:'
                              CALL SHOWIT(1)
                              WRITE(OUTLYNE,*) 'FOR SURFACE ',L
                              CALL SHOWIT(1)
                              OUTLYNE=
     1                        'THE CONIC CONSTANT RESET TO 0.0 FOR THIS PLANO SURFACE'
                              CALL SHOWIT(1)
                          END IF
                          IF(ALENS(1,L).NE.0.0D0.AND.ALENS(43,L).NE.0.0D0) THEN
                              ALENS(43,L)=0.0D0
                              OUTLYNE='WARNING:'
                              CALL SHOWIT(1)
                              WRITE(OUTLYNE,*) 'FOR SURFACE ',L
                              CALL SHOWIT(1)
                              OUTLYNE=
     1                        'THE "AC" TERM RESET TO 0.0 FOR THIS NON-PLANO SURFACE'
                              CALL SHOWIT(1)
                          END IF
C
                      ELSE
                          ALENS(1,L)=(PXTRAY(7,L)-PXTRAY(6,(L-1)))/
     1                    (PXTRAY(5,(L-1)))
C
                          IF(ALENS(1,L).EQ.0.0D0.AND.ALENS(2,L).NE.0.0D0) THEN
                              ALENS(2,L)=0.0D0
                              OUTLYNE='WARNING:'
                              CALL SHOWIT(1)
                              WRITE(OUTLYNE,*) 'FOR SURFACE ',L
                              CALL SHOWIT(1)
                              OUTLYNE=
     1                        'THE CONIC CONSTANT RESET TO 0.0 FOR THIS PLANO SURFACE'
                              CALL SHOWIT(1)
                          END IF
                          IF(ALENS(1,L).NE.0.0D0.AND.ALENS(43,L).NE.0.0D0) THEN
                              ALENS(43,L)=0.0D0
                              OUTLYNE='WARNING:'
                              CALL SHOWIT(1)
                              WRITE(OUTLYNE,*) 'FOR SURFACE ',L
                              CALL SHOWIT(1)
                              OUTLYNE=
     1                        'THE "AC" TERM RESET TO 0.0 FOR THIS NON-PLANO SURFACE'
                              CALL SHOWIT(1)
                          END IF
C
                      END IF
                  END IF
C       PICY SOLVE FOR SURFACE L HANDLED.
              ELSE
C       NO PICY SOLVE
              END IF
C*****************************************************************************
C                       APY SOLVES
C***************************************************************************
C       IS THERE A APY SOLVE ON SURFACE L
              IF(SOLVE(8,L).EQ.1.0D0) THEN
C       YES THERE IS A APY SOLVE ON SURFACE L, HANDLE IT
C       FOR AN APY SOLVE
C       CV(L)=-(PUY(L-1)/PY(L))*((N'+N)/N)
C       THEN
C       PUY(L)=(N/N')*PUY(L-1)-CV(L)*PY(L)*((N'-N)/N')
C       PIY(L)=CV(L)*PY(L)+PUY(L-1)
C       PIY'(L)=(N/N')*PIY(L)
C       IS SURFACE L AN X-TORIC,IF SO THE TORIC CURVATURE AND
C       NOT THE MAIN CURVATURE IS CHANGED.
                  IF(ALENS(23,L).EQ.2.0D0) THEN
C       SURFACE IS X-TORIC, PROCEED
                      IF(PXTRAY(1,L).EQ.0.0D0.OR.(J_NP+N).EQ.0.0D0) THEN
                          ALENS(24,L)=0.0D0
                      ELSE
                          ALENS(24,L)=-(PXTRAY(2,(L-1))/PXTRAY(1,L))*((J_NP+N)/N)
                      END IF
C       RECALCULATE ANGLES OF INCIDENCE,REFLECTION/REFRACTION
C       AND EXITING SLOPE ANGLE
                      PXTRAY(2,L)=((N/J_NP)*PXTRAY(2,(L-1)))-(ALENS(24,L)*
     1                PXTRAY(1,L)*((J_NP-N)/J_NP))
                      PXTRAY(3,L)=(ALENS(24,L)*PXTRAY(1,L))+PXTRAY(2,(L-1))
                      PXTRAY(4,L)=(N/J_NP)*PXTRAY(3,L)
                  ELSE
C       SURFACE NOT X-TORIC,PROCEED
                      IF(PXTRAY(1,L).EQ.0.0D0.OR.(J_NP+N).EQ.0.0D0) THEN
                          ALENS(1,L)=0.0D0
C
                          IF(ALENS(1,L).EQ.0.0D0.AND.ALENS(2,L).NE.0.0D0) THEN
                              ALENS(2,L)=0.0D0
                              OUTLYNE='WARNING:'
                              CALL SHOWIT(1)
                              WRITE(OUTLYNE,*) 'FOR SURFACE ',L
                              CALL SHOWIT(1)
                              OUTLYNE=
     1                        'THE CONIC CONSTANT RESET TO 0.0 FOR THIS PLANO SURFACE'
                              CALL SHOWIT(1)
                          END IF
                          IF(ALENS(1,L).NE.0.0D0.AND.ALENS(43,L).NE.0.0D0) THEN
                              ALENS(43,L)=0.0D0
                              OUTLYNE='WARNING:'
                              CALL SHOWIT(1)
                              WRITE(OUTLYNE,*) 'FOR SURFACE ',L
                              CALL SHOWIT(1)
                              OUTLYNE=
     1                        'THE "AC" TERM RESET TO 0.0 FOR THIS NON-PLANO SURFACE'
                              CALL SHOWIT(1)
                          END IF
C
                      ELSE
                          ALENS(1,L)=-(PXTRAY(2,(L-1))/PXTRAY(1,L))*((J_NP+N)/N)
C
                          IF(ALENS(1,L).EQ.0.0D0.AND.ALENS(2,L).NE.0.0D0) THEN
                              ALENS(2,L)=0.0D0
                              OUTLYNE='WARNING:'
                              CALL SHOWIT(1)
                              WRITE(OUTLYNE,*) 'FOR SURFACE ',L
                              CALL SHOWIT(1)
                              OUTLYNE=
     1                        'THE CONIC CONSTANT RESET TO 0.0 FOR THIS PLANO SURFACE'
                              CALL SHOWIT(1)
                          END IF
                          IF(ALENS(1,L).NE.0.0D0.AND.ALENS(43,L).NE.0.0D0) THEN
                              ALENS(43,L)=0.0D0
                              OUTLYNE='WARNING:'
                              CALL SHOWIT(1)
                              WRITE(OUTLYNE,*) 'FOR SURFACE ',L
                              CALL SHOWIT(1)
                              OUTLYNE=
     1                        'THE "AC" TERM RESET TO 0.0 FOR THIS NON-PLANO SURFACE'
                              CALL SHOWIT(1)
                          END IF
C
                      END IF
C       RECALCULATE ANGLES OF INCIDENCE,REFLECTION/REFRACTION
C       AND EXITING SLOPE ANGLE
                      PXTRAY(2,L)=((N/J_NP)*PXTRAY(2,(L-1)))-(ALENS(1,L)*
     1                PXTRAY(1,L)*((J_NP-N)/J_NP))
                      PXTRAY(3,L)=(ALENS(1,L)*PXTRAY(1,L))+PXTRAY(2,(L-1))
                      PXTRAY(4,L)=(N/J_NP)*PXTRAY(3,L)
                  END IF
C       APY SOLVE FOR SURFACE L HANDLED.
              ELSE
C       NO APY SOLVE
              END IF
C***************************************************************************
C                       APCY SOLVES
C***************************************************************************
C       IS THERE A APCY SOLVE ON SURFACE L
              IF(SOLVE(8,L).EQ.4.0D0) THEN
C       YES THERE IS A APCY SOLVE ON SURFACE L, HANDLE IT
C       FOR AN APCY SOLVE
C       CV(L)=-(PUCY(L-1)/PCY(L))*((N'+N)/N)
C       THEN
C       PUCY(L)=(N/N')*PUCY(L-1)-CV(L)*PCY(L)*((N'-N)/N')
C       PICY(L)=CV(L)*PCY(L)+PUCY(L-1)
C       PICY'(L)=(N/N')*PICY(L)
C       IS SURFACE L AN X-TORIC,IF SO THE TORIC CURVATURE AND
C       NOT THE MAIN CURVATURE IS CHANGED.
                  IF(ALENS(23,L).EQ.2.0D0) THEN
C       SURFACE IS X-TORIC, PROCEED
                      IF(PXTRAY(5,L).EQ.0.0D0.OR.(J_NP+N).EQ.0.0D0) THEN
                          ALENS(24,L)=0.0D0
                      ELSE
                          ALENS(24,L)=-(PXTRAY(6,(L-1))/PXTRAY(5,L))*((J_NP+N)/N)
                      END IF
C       RECALCULATE ANGLES OF INCIDENCE,REFLECTION/REFRACTION
C       AND EXITING SLOPE ANGLE
                      PXTRAY(6,L)=((N/J_NP)*PXTRAY(6,(L-1)))-(ALENS(24,L)*
     1                PXTRAY(5,L)*((J_NP-N)/J_NP))
                      PXTRAY(7,L)=(ALENS(24,L)*PXTRAY(5,L))+PXTRAY(6,(L-1))
                      PXTRAY(8,L)=(N/J_NP)*PXTRAY(7,L)
                  ELSE
C       SURFACE NOT X-TORIC,PROCEED
                      IF(PXTRAY(5,L).EQ.0.0D0.OR.(J_NP+N).EQ.0.0D0) THEN
                          ALENS(1,L)=0.0D0
C
                          IF(ALENS(1,L).EQ.0.0D0.AND.ALENS(2,L).NE.0.0D0) THEN
                              ALENS(2,L)=0.0D0
                              OUTLYNE='WARNING:'
                              CALL SHOWIT(1)
                              WRITE(OUTLYNE,*) 'FOR SURFACE ',L
                              CALL SHOWIT(1)
                              OUTLYNE=
     1                        'THE CONIC CONSTANT RESET TO 0.0 FOR THIS PLANO SURFACE'
                              CALL SHOWIT(1)
                          END IF
                          IF(ALENS(1,L).NE.0.0D0.AND.ALENS(43,L).NE.0.0D0) THEN
                              ALENS(43,L)=0.0D0
                              OUTLYNE='WARNING:'
                              CALL SHOWIT(1)
                              WRITE(OUTLYNE,*) 'FOR SURFACE ',L
                              CALL SHOWIT(1)
                              OUTLYNE=
     1                        'THE "AC" TERM RESET TO 0.0 FOR THIS NON-PLANO SURFACE'
                              CALL SHOWIT(1)
                          END IF
C
                      ELSE
                          ALENS(1,L)=-(PXTRAY(6,(L-1))/PXTRAY(5,L))*((J_NP+N)/N)
C
                          IF(ALENS(1,L).EQ.0.0D0.AND.ALENS(2,L).NE.0.0D0) THEN
                              ALENS(2,L)=0.0D0
                              OUTLYNE='WARNING:'
                              CALL SHOWIT(1)
                              WRITE(OUTLYNE,*) 'FOR SURFACE ',L
                              CALL SHOWIT(1)
                              OUTLYNE=
     1                        'THE CONIC CONSTANT RESET TO 0.0 FOR THIS PLANO SURFACE'
                              CALL SHOWIT(1)
                          END IF
                          IF(ALENS(1,L).NE.0.0D0.AND.ALENS(43,L).NE.0.0D0) THEN
                              ALENS(43,L)=0.0D0
                              OUTLYNE='WARNING:'
                              CALL SHOWIT(1)
                              WRITE(OUTLYNE,*) 'FOR SURFACE ',L
                              CALL SHOWIT(1)
                              OUTLYNE=
     1                        'THE "AC" TERM RESET TO 0.0 FOR THIS NON-PLANO SURFACE'
                              CALL SHOWIT(1)
                          END IF
C
                      END IF
C       RECALCULATE ANGLES OF INCIDENCE,REFLECTION/REFRACTION
C       AND EXITING SLOPE ANGLE
                      PXTRAY(6,L)=((N/J_NP)*PXTRAY(6,(L-1)))-(ALENS(1,L)*
     1                PXTRAY(5,L)*((J_NP-N)/J_NP))
                      PXTRAY(7,L)=(ALENS(1,L)*PXTRAY(5,L))+PXTRAY(6,(L-1))
                      PXTRAY(8,L)=(N/J_NP)*PXTRAY(7,L)
                  END IF
C       APCY SOLVE FOR SURFACE L HANDLED.
              ELSE
C       NO APCY SOLVE
              END IF
C**************************************************************************
C                       COCY SOLVES
C***************************************************************************
C       IS THERE A COCY SOLVE ON SURFACE L
              IF(SOLVE(8,L).EQ.7.0D0) THEN
                  DIS=0.0D0
C       YES THERE IS A COCY SOLVE ON SURFACE L, HANDLE IT
C       FOR AN COCY SOLVE
C       CV(L)=SIGNED DISTANCE TO THE SPECIFIED SURFACE(TAR)
C       FROM SURFACE L EQUAL TO DIS
                  TAR=INT(SOLVE(9,L))
                  IF(TAR.LT.L) THEN
                      DO I=TAR,(L-1)
                          DIS=DIS+ALENS(3,I)
                      END DO
                  ELSE
                  END IF
                  IF(TAR.GT.L) THEN
                      DO I=L,(TAR-1)
                          DIS=DIS+ALENS(3,I)
                      END DO
                  ELSE
                  END IF
C       CASE OF TAR=L ELIMINATED INSIDE OD SUBROUTINE LNSEOS.
C       THEN RECALCULATE
C       PUCY(L)=(N/N')*PUCY(L-1)-CV(L)*PCY(L)*((N'-N)/N')
C       PICY(L)=CV(L)*PCY(L)+PUCY(L-1)
C       PICY'(L)=(N/N')*PICY(L)
C       IS SURFACE L AN X-TORIC,IF SO THE TORIC CURVATURE AND
C       NOT THE MAIN CURVATURE IS CHANGED.
                  IF(ALENS(23,L).EQ.2.0D0) THEN
C       SURFACE IS X-TORIC, PROCEED
                      IF(DIS.NE.0.0D0) THEN
                          IF(TAR.LT.L) ALENS(24,L)=-1.0D0/DIS
                          IF(TAR.GT.L) ALENS(24,L)= 1.0D0/DIS
                      ELSE
C       DIS IS ZERO, CONCENTRIC SOLVE IGNORED
                          OUTLYNE='(COCY) SOLVE RESULTS IN A ZERO RADIUS OF CURVATURE'
                          CALL SHOWIT(1)
                          OUTLYNE='(COCY) SOLVE REMOVED'
                          CALL SHOWIT(1)
                          SOLVE(8,L)=0.0D0
                          SOLVE(9,0)=0.0D0
                      END IF
C       RECALCULATE ANGLES OF INCIDENCE,REFLECTION/REFRACTION
C       AND EXITING SLOPE ANGLE
                      PXTRAY(6,L)=((N/J_NP)*PXTRAY(6,(L-1)))-(ALENS(24,L)*
     1                PXTRAY(5,L)*((J_NP-N)/J_NP))
                      PXTRAY(7,L)=(ALENS(24,L)*PXTRAY(5,L))+PXTRAY(6,(L-1))
                      PXTRAY(8,L)=(N/J_NP)*PXTRAY(7,L)
                  ELSE
C       SURFACE NOT X-TORIC,PROCEED
                      IF(DIS.NE.0.0D0) THEN
                          IF(TAR.LT.L) ALENS(1,L)=-1.0D0/DIS
                          IF(TAR.GT.L) ALENS(1,L)=1.0D0/DIS
C
                          IF(ALENS(1,L).EQ.0.0D0.AND.ALENS(2,L).NE.0.0D0) THEN
                              ALENS(2,L)=0.0D0
                              OUTLYNE='WARNING:'
                              CALL SHOWIT(1)
                              WRITE(OUTLYNE,*) 'FOR SURFACE ',L
                              CALL SHOWIT(1)
                              OUTLYNE=
     1                        'THE CONIC CONSTANT RESET TO 0.0 FOR THIS PLANO SURFACE'
                              CALL SHOWIT(1)
                          END IF
                          IF(ALENS(1,L).NE.0.0D0.AND.ALENS(43,L).NE.0.0D0) THEN
                              ALENS(43,L)=0.0D0
                              OUTLYNE='WARNING:'
                              CALL SHOWIT(1)
                              WRITE(OUTLYNE,*) 'FOR SURFACE ',L
                              CALL SHOWIT(1)
                              OUTLYNE=
     1                        'THE "AC" TERM RESET TO 0.0 FOR THIS NON-PLANO SURFACE'
                              CALL SHOWIT(1)
                          END IF
C
                      ELSE
C       DIS IS ZERO, CONCENTRIC SOLVE IGNORED
                          OUTLYNE='(COCY) SOLVE RESULTS IN A ZERO RADIUS OF CURVATURE'
                          CALL SHOWIT(1)
                          OUTLYNE='(COCY) SOLVE REMOVED'
                          CALL SHOWIT(1)
                          SOLVE(8,L)=0.0D0
                          SOLVE(9,0)=0.0D0
                      END IF
C       RECALCULATE ANGLES OF INCIDENCE,REFLECTION/REFRACTION
C       AND EXITING SLOPE ANGLE
                      PXTRAY(6,L)=((N/J_NP)*PXTRAY(6,(L-1)))-(ALENS(1,L)*
     1                PXTRAY(5,L)*((J_NP-N)/J_NP))
                      PXTRAY(7,L)=(ALENS(1,L)*PXTRAY(5,L))+PXTRAY(6,(L-1))
                      PXTRAY(8,L)=(N/J_NP)*PXTRAY(7,L)
                  END IF
C       COCY SOLVE FOR SURFACE L HANDLED.
              ELSE
C       NO COCY SOLVE
              END IF
C**************************************************************************
C               NOW THICKNESS SOLVES
C**************************************************************************
C                       PY SOLVES
C***************************************************************************
C       IS THERE A PY SOLVE ON SURFACE L
              IF(SOLVE(6,L).EQ.1.0D0) THEN
C       YES THERE IS A PY SOLVE ON SURFACE L
C       WHICH AFFECTS THE PY VALUE ON (L+1), HANDLE IT
C       TH(L)=(SOLVE TARGET-PY(L))/PUY(L)
C
                  IF(PXTRAY(2,L).EQ.0.0D0) THEN
                      ALENS(3,L)=1D20
                  ELSE
                      ALENS(3,L)=(SOLVE(7,L)-PXTRAY(1,(L)))/(PXTRAY(2,L))
                      IF(DABS(ALENS(3,L)).GT.1.0D20)THEN
                          IF(ALENS(3,L).GT.0.0D0) THEN
                              ALENS(3,L)=1.0D20
                          ELSE
                          END IF
                          IF(ALENS(3,L).LT.0.0D0) THEN
                              ALENS(3,L)=-1.0D20
                          ELSE
                          END IF
                      ELSE
                      END IF
                  END IF
C       PY SOLVE FOR SURFACE L HANDLED.
              ELSE
C       NO PY SOLVE
              END IF
C**************************************************************************
C                       PCY SOLVES
C***************************************************************************
C       IS THERE A PCY SOLVE ON SURFACE L
              IF(SOLVE(6,L).EQ.2.0D0) THEN
C       YES THERE IS A PCY SOLVE ON SURFACE L
C       WHICH AFFECTS THE PCY VALUE ON (L+1), HANDLE IT
C       TH(L)=(SOLVE TARGET-PCY(L))/PUCY(L)
C
                  IF(PXTRAY(6,L).EQ.0.0D0) THEN
                      ALENS(3,L)=1.0D20
                  ELSE
                      ALENS(3,L)=(SOLVE(7,L)-PXTRAY(5,(L)))/(PXTRAY(6,L))
                      IF(DABS(ALENS(3,L)).GT.1D20)THEN
                          IF(ALENS(3,L).GT.0.0D0) THEN
                              ALENS(3,L)=1.0D20
                          ELSE
                          END IF
                          IF(ALENS(3,L).LT.0.0D0) THEN
                              ALENS(3,L)=-1.0D20
                          ELSE
                          END IF
                      ELSE
                      END IF
                  END IF
C       PCY SOLVE FOR SURFACE L HANDLED.
              ELSE
C       NO PCY SOLVE
              END IF
C***************************************************************************
C                       CAY SOLVES
C******************************************************************************
C       CHECK FOR CAY SOLVE ON (L) AND IF FOUND, HANDLE IT.
              IF(SOLVE(6,(L)).EQ.3.0D0) THEN
C       THERE IS A CAY SOLVE ON (L) WHICH AFFECTS TH(L), HANDLE IT.
C       IS THER A CIRCULAR CLEAR APERTURE ON SURFACE L
                  IF(ALENS(9,(L)).EQ.1.0D0.AND.ALENS(127,L).NE.0.0D0.OR.
     1            ALENS(9,(L)).EQ.1.0D0.AND.ALENS(127,L).NE.0.0D0) THEN
C       YES THERE IS, SET EDGVAL TO IT
                      IF(ALENS(10,L).LE.ALENS(11,L)) THEN
                          EDGVAL=DABS(ALENS(10,(L)))
                      ELSE
                          EDGVAL=DABS(ALENS(11,(L)))
                      END IF
                  ELSE
C       NO CIRCULAR CLAP, USE PY(L)+PCY(L) FOR EDGVAL
                      EDGVAL=DABS(PXTRAY(1,(L)))+DABS(PXTRAY(5,(L)))
                  END IF
C                       NOW EDGEVAL IS SET
C       NOW CALCULATE THE SAG OF SURFACES L AND L+1
C       CONSIDER THE YZ-PLANE ONLY AND IGNORE TILTS,
C       DECENTERS AND SPECIAL SURFACE DATA. CONSIDER
C       TORICS IF PRESENT. CONSIDER CONIC AND ASPHERIC
C       TERMS IF PRESENT.
C       CHECK FOR ANAMORPHICS
                  IF(ALENS(36,L).EQ.0.0D0) THEN
C       IS THE SURFACE L X-TORIC BUT NOT ANAMORPHIC ASPHERE?
                      IF(ALENS(23,(L)).EQ.2.0D0) THEN
C       USE TORIC DATA IN SAG CALC
                          ARG=(1.0D0-((1.0D0)*(ALENS(24,L)**2)*(EDGVAL**2)))
                          IF(ARG.LT.0.0D0) ARG=0.0D0
                          SAGL=(((EDGVAL**2)*(ALENS(24,(L))))/
     1                    (1.0D0+DSQRT(ARG)))
                      ELSE
C       NOT X-TORIC
                          ARG=(1.0D0-((ALENS(2,L)+1.0D0)*(ALENS(1,L)**2)*(EDGVAL**2)))
                          IF(ARG.LT.0.0D0) ARG=0.0D0
                          SAGL=(((EDGVAL**2)*(ALENS(1,(L))))/
     1                    (1.0D0+DSQRT(ARG)))+
     3                    (ALENS(4,(L))*(EDGVAL**4))+
     4                    (ALENS(5,(L))*(EDGVAL**6))+
     5                    (ALENS(6,(L))*(EDGVAL**8))+
     6                    (ALENS(7,(L))*(EDGVAL**10))+
     6                    (ALENS(81,(L))*(EDGVAL**12))+
     6                    (ALENS(82,(L))*(EDGVAL**14))+
     6                    (ALENS(83,(L))*(EDGVAL**16))+
     6                    (ALENS(84,(L))*(EDGVAL**18))+
     6                    (ALENS(85,(L))*(EDGVAL**20))
                      END IF
                  ELSE
C       ANAMORPHIC ASPHERIC
                      IF(ALENS(23,L).EQ.1.0D0) THEN
C       Y-TORIC
                          ARG=(1.0D0-((ALENS(2,L)+1.0D0)*(ALENS(1,L)**2)*(EDGVAL**2)))
                          IF(ARG.LT.0.0D0) ARG=0.0D0
                          SAGL=(((EDGVAL**2)*(ALENS(1,L)))/
     1                    (1.0D0+DSQRT(ARG)))+
     3                    (ALENS(4,L)*(((1.0D0-ALENS(37,L))*(EDGVAL**2))**2))+
     3                    (ALENS(5,L)*(((1.0D0-ALENS(38,L))*(EDGVAL**2))**3))+
     3                    (ALENS(6,L)*(((1.0D0-ALENS(39,L))*(EDGVAL**2))**4))+
     3                    (ALENS(7,L)*(((1.0D0-ALENS(40,L))*(EDGVAL**2))**5))
                      ELSE
                      END IF
                      IF(ALENS(23,L).EQ.2.0D0) THEN
C       X-TORIC
                          ARG=(1.0D0-((ALENS(41,L)+1.0D0)*(ALENS(24,L)**2)*(EDGVAL**2)))
                          IF(ARG.LT.0.0D0) ARG=0.0D0
                          SAGL=(((EDGVAL**2)*(ALENS(24,L)))/
     1                    (1.0D0+DSQRT(ARG)))+
     3                    (ALENS(4,L)*(((1.0D0+ALENS(37,L))*(EDGVAL**2))**2))+
     3                    (ALENS(5,L)*(((1.0D0+ALENS(38,L))*(EDGVAL**2))**3))+
     3                    (ALENS(6,L)*(((1.0D0+ALENS(39,L))*(EDGVAL**2))**4))+
     3                    (ALENS(7,L)*(((1.0D0+ALENS(40,L))*(EDGVAL**2))**5))
                      ELSE
                      END IF
                  END IF
C       CHECK FOR ANAMORPHICS
                  IF(ALENS(36,(L+1)).EQ.0.0D0) THEN
C       IS THE SURFACE L+1 X-TORIC BUT NOT ANAMORPHIC ASPHERE ?
                      IF(ALENS(23,(L+1)).EQ.2.0D0) THEN
C       USE TORIC DATA IN SAG CALC
                          ARG=(1.0D0-((1.0D0)*(ALENS(24,L)**2)*(EDGVAL**2)))
                          IF(ARG.LT.0.0D0) ARG=0.0D0
                          SAGLP1=(((EDGVAL**2)*(ALENS(24,(L+1))))/
     1                    (1.0D0+DSQRT(ARG)))
                      ELSE
C       NOT X-TORIC
                          ARG=(1.0D0-((ALENS(2,L+1)+1.0D0)*(ALENS(1,L+1)**2)*(EDGVAL**2)))
                          IF(ARG.LT.0.0D0) ARG=0.0D0
                          SAGLP1=(((EDGVAL**2)*(ALENS(1,(L+1))))/
     1                    (1.0D0+DSQRT(ARG)))+
     3                    (ALENS(4,(L+1))*(EDGVAL**4))+
     4                    (ALENS(5,(L+1))*(EDGVAL**6))+
     5                    (ALENS(6,(L+1))*(EDGVAL**8))+
     6                    (ALENS(7,(L+1))*(EDGVAL**10))+
     6                    (ALENS(81,(L+1))*(EDGVAL**12))+
     6                    (ALENS(82,(L+1))*(EDGVAL**14))+
     6                    (ALENS(83,(L+1))*(EDGVAL**16))+
     6                    (ALENS(84,(L+1))*(EDGVAL**18))+
     6                    (ALENS(85,(L+1))*(EDGVAL**20))
                      END IF
                  ELSE
C       ANAMORPHIC ASPHERIC
                      IF(ALENS(23,(L+1)).EQ.1.0D0) THEN
C       Y-TORIC
                          ARG=(1.0D0-((ALENS(2,L+1)+1.0D0)*(ALENS(1,L+1)**2)*(EDGVAL**2)))
                          IF(ARG.LT.0.0D0) ARG=0.0D0
                          SAGL=(((EDGVAL**2)*(ALENS(1,(L+1))))/
     1                    (1.0D0+DSQRT(ARG)))+
     3                    (ALENS(4,(L+1))*(((1.0D0-ALENS(37,(L+1)))*(EDGVAL**2))**2))+
     3                    (ALENS(5,(L+1))*(((1.0D0-ALENS(38,(L+1)))*(EDGVAL**2))**3))+
     3                    (ALENS(6,(L+1))*(((1.0D0-ALENS(39,(L+1)))*(EDGVAL**2))**4))+
     3                    (ALENS(7,(L+1))*(((1.0D0-ALENS(40,(L+1)))*(EDGVAL**2))**5))
                      ELSE
                      END IF
                      IF(ALENS(23,L).EQ.2.0D0) THEN
C       X-TORIC
                          ARG=(1.0D0-((ALENS(41,L+1)+1.0D0)*(ALENS(24,L+1)**2)*(EDGVAL**2)))
                          IF(ARG.LT.0.0D0) ARG=0.0D0
                          SAGL=(((EDGVAL**2)*(ALENS(24,(L+1))))/
     1                    (1.0D0+DSQRT(ARG)))+
     3                    (ALENS(4,(L+1))*(((1.0D0+ALENS(37,(L+1)))*(EDGVAL**2))**2))+
     3                    (ALENS(5,(L+1))*(((1.0D0+ALENS(38,(L+1)))*(EDGVAL**2))**3))+
     3                    (ALENS(6,(L+1))*(((1.0D0+ALENS(39,(L+1)))*(EDGVAL**2))**4))+
     3                    (ALENS(7,(L+1))*(((1.0D0+ALENS(40,(L+1)))*(EDGVAL**2))**5))
                      ELSE
                      END IF
                  END IF
C       NOW THE SAGS OF SURFACES L AND L+1 HAVE BEEN
C       CALCULATED. NOW CALCULATE THE NEW ADJUSTED
C       THICKNESS, TH(L).
C       RULES:
C               IF SAGL-SAGLP1>0 THEN
C               TH(L)=(CAY TARGET VALUE)+(SAGL-SAGLP1)
C               IF SAGL-SAGLP1)<OR= 0 THEN
C               TH(L)=(CAY TARGET VALUE)
                  IF((SAGL-SAGLP1).GT.0.0D0)
     1            ALENS(3,(L))=SOLVE(7,(L))+(SAGL-SAGLP1)
                  IF((SAGL-SAGLP1).LE.0.0D0)
     1            ALENS(3,(L))=SOLVE(7,(L))
C       HANDLING OF CAY SOLVE IS COMPLETED
              ELSE
C       NO CAY SOLVE IS PRESENT, PROCEED
              END IF
C       FINISHED WITH POSSIBLE CAY SOLVES ON TH(L)
C
C               ALL SURFACE (L) SOLVES NOW HANDLED.
C               NOW RETURN TO THE CALLING SUBROUTINE
              IF(SOLVE(6,L).NE.0.0D0.OR.SOLVE(8,L).NE.0.0D0) THEN
C     THERE WERE YZ=PLANE SOLVES, CALC VALUES
                  FINY=L
                  CALL FINIYZ
              ELSE
              END IF
              RETURN
          ELSE
C       SLV2 NOT 1
          END IF
          IF(SLV2.EQ.2) THEN
C
C       THE SURFACE OF SOLVE EVALUATION IS THE PASSED PARAMETER L.
C       NOW HANDLE ALL SOLVES ON SURFACE L
C       SIMPLIFY THE REPRESENTATION OF THE REFRACTIVE INDICES
C       AT THE CONTROL WAVELENGTH AT L-1 AND L
              IF(INT(SYSTEM1(11)).GE.1.AND.INT(SYSTEM1(11)).LE.5) THEN
                  N=ALENS(45+INT(SYSTEM1(11)),(L-1))
                  J_NP=ALENS(45+INT(SYSTEM1(11)),(L))
              END IF
              IF(INT(SYSTEM1(11)).GE.6.AND.INT(SYSTEM1(11)).LE.10) THEN
                  N=ALENS(65+INT(SYSTEM1(11)),(L-1))
                  J_NP=ALENS(65+INT(SYSTEM1(11)),(L))
              END IF
C
C                       FIRST,CURVATURE SOLVES
C***************************************************************************
C                       PUX SOLVES
C***************************************************************************
C       IS THERE A PUX SOLVE ON SURFACE L
              IF(SOLVE(2,L).EQ.10.0D0) THEN
C       YES THERE IS A PUX SOLVE ON SURFACE L, HANDLE IT
C       FOR A PUX SOLVE
C       CV(L)=(-SOLVE TARGET+(N/N')*PUX(L-1))*(N'/(N'-N))*(1/PX(L))
C       AND PUX(L)=SOLVE TARGET
C       IS SURFACE L AN Y-TORIC,IF SO THE TORIC CURVATURE AND
C       NOT THE MAIN CURVATURE IS CHANGED.
                  IF(ALENS(23,L).EQ.1.0D0) THEN
C       SURFACE IS Y-TORIC
                      PXTRAX(2,L)=SOLVE(1,L)
                      IF(PXTRAX(1,L).EQ.0.0D0.OR.(J_NP-N).EQ.0.0D0) THEN
                          ALENS(24,L)=0.0D0
                      ELSE
                          ALENS(24,L)=((-PXTRAX(2,L))+((N/J_NP)*PXTRAX(2,(L-1))))*
     1                    (J_NP/(J_NP-N))*(1.0D0/PXTRAX(1,L))
                      END IF
                  ELSE
C       SURFACE NOT Y-TORIC,PROCEED
                      PXTRAX(2,L)=SOLVE(1,L)
                      IF(PXTRAX(1,L).EQ.0.0D0.OR.(J_NP-N).EQ.0.0D0) THEN
                          ALENS(1,L)=0.0D0
C
                          IF(ALENS(1,L).EQ.0.0D0.AND.ALENS(2,L).NE.0.0D0) THEN
                              ALENS(2,L)=0.0D0
                              OUTLYNE='WARNING:'
                              CALL SHOWIT(1)
                              WRITE(OUTLYNE,*) 'FOR SURFACE ',L
                              CALL SHOWIT(1)
                              OUTLYNE=
     1                        'THE CONIC CONSTANT RESET TO 0.0 FOR THIS PLANO SURFACE'
                              CALL SHOWIT(1)
                          END IF
                          IF(ALENS(1,L).NE.0.0D0.AND.ALENS(43,L).NE.0.0D0) THEN
                              ALENS(43,L)=0.0D0
                              OUTLYNE='WARNING:'
                              CALL SHOWIT(1)
                              WRITE(OUTLYNE,*) 'FOR SURFACE ',L
                              CALL SHOWIT(1)
                              OUTLYNE=
     1                        'THE "AC" TERM RESET TO 0.0 FOR THIS NON-PLANO SURFACE'
                              CALL SHOWIT(1)
                          END IF
C
                      ELSE
                          ALENS(1,L)=((-PXTRAX(2,L))+((N/J_NP)*PXTRAX(2,(L-1))))*
     1                    (J_NP/(J_NP-N))*(1.0D0/PXTRAX(1,L))
C
                          IF(ALENS(1,L).EQ.0.0D0.AND.ALENS(2,L).NE.0.0D0) THEN
                              ALENS(2,L)=0.0D0
                              OUTLYNE='WARNING:'
                              CALL SHOWIT(1)
                              WRITE(OUTLYNE,*) 'FOR SURFACE ',L
                              CALL SHOWIT(1)
                              OUTLYNE=
     1                        'THE CONIC CONSTANT RESET TO 0.0 FOR THIS PLANO SURFACE'
                              CALL SHOWIT(1)
                          END IF
                          IF(ALENS(1,L).NE.0.0D0.AND.ALENS(43,L).NE.0.0D0) THEN
                              ALENS(43,L)=0.0D0
                              OUTLYNE='WARNING:'
                              CALL SHOWIT(1)
                              WRITE(OUTLYNE,*) 'FOR SURFACE ',L
                              CALL SHOWIT(1)
                              OUTLYNE=
     1                        'THE "AC" TERM RESET TO 0.0 FOR THIS NON-PLANO SURFACE'
                              CALL SHOWIT(1)
                          END IF
C
                      END IF
                  END IF
C       PUX SOLVE FOR SURFACE L HANDLED.
              ELSE
C       NO PUX SOLVE
              END IF
C*****************************************************************************
C                       PUCX SOLVES
C***************************************************************************
C       IS THERE A PUCX SOLVE ON SURFACE L
              IF(SOLVE(2,L).EQ.13.0D0) THEN
C       YES THERE IS A PUCX SOLVE ON SURFACE L, HANDLE IT
C       FOR A PUCX SOLVE
C       CV(L)=(-SOLVE TARGET+(N/N')*PUCX(L-1))*(N'/(N'-N))*(1/PCX(L))
C       AND PUCX(L)=SOLVE TARGET
C       IS SURFACE L AN Y-TORIC,IF SO THE TORIC CURVATURE AND
C       NOT THE MAIN CURVATURE IS CHANGED.
                  IF(ALENS(23,L).EQ.1.0D0) THEN
C       SURFACE IS Y-TORIC
                      PXTRAX(6,L)=SOLVE(1,L)
                      IF(PXTRAX(5,L).EQ.0.0D0.OR.(J_NP-N).EQ.0.0D0) THEN
                          ALENS(24,L)=0.0D0
                      ELSE
                          ALENS(24,L)=((-PXTRAX(6,L))+((N/J_NP)*PXTRAX(6,(L-1))))*
     1                    (J_NP/(J_NP-N))*(1.0D0/PXTRAX(5,L))
                      END IF
                  ELSE
C       SURFACE NOT Y-TORIC,PROCEED
                      PXTRAX(6,L)=SOLVE(1,L)
                      IF(PXTRAX(5,L).EQ.0.0D0.OR.(J_NP-N).EQ.0.0D0) THEN
                          ALENS(1,L)=0.0D0
C
                          IF(ALENS(1,L).EQ.0.0D0.AND.ALENS(2,L).NE.0.0D0) THEN
                              ALENS(2,L)=0.0D0
                              OUTLYNE='WARNING:'
                              CALL SHOWIT(1)
                              WRITE(OUTLYNE,*) 'FOR SURFACE ',L
                              CALL SHOWIT(1)
                              OUTLYNE=
     1                        'THE CONIC CONSTANT RESET TO 0.0 FOR THIS PLANO SURFACE'
                              CALL SHOWIT(1)
                          END IF
                          IF(ALENS(1,L).NE.0.0D0.AND.ALENS(43,L).NE.0.0D0) THEN
                              ALENS(43,L)=0.0D0
                              OUTLYNE='WARNING:'
                              CALL SHOWIT(1)
                              WRITE(OUTLYNE,*) 'FOR SURFACE ',L
                              CALL SHOWIT(1)
                              OUTLYNE=
     1                        'THE "AC" TERM RESET TO 0.0 FOR THIS NON-PLANO SURFACE'
                              CALL SHOWIT(1)
                          END IF
C
                      ELSE
                          ALENS(1,L)=((-PXTRAX(6,L))+((N/J_NP)*PXTRAX(6,(L-1))))*
     1                    (J_NP/(J_NP-N))*(1.0D0/PXTRAX(5,L))
C
                          IF(ALENS(1,L).EQ.0.0D0.AND.ALENS(2,L).NE.0.0D0) THEN
                              ALENS(2,L)=0.0D0
                              OUTLYNE='WARNING:'
                              CALL SHOWIT(1)
                              WRITE(OUTLYNE,*) 'FOR SURFACE ',L
                              CALL SHOWIT(1)
                              OUTLYNE=
     1                        'THE CONIC CONSTANT RESET TO 0.0 FOR THIS PLANO SURFACE'
                              CALL SHOWIT(1)
                          END IF
                          IF(ALENS(1,L).NE.0.0D0.AND.ALENS(43,L).NE.0.0D0) THEN
                              ALENS(43,L)=0.0D0
                              OUTLYNE='WARNING:'
                              CALL SHOWIT(1)
                              WRITE(OUTLYNE,*) 'FOR SURFACE ',L
                              CALL SHOWIT(1)
                              OUTLYNE=
     1                        'THE "AC" TERM RESET TO 0.0 FOR THIS NON-PLANO SURFACE'
                              CALL SHOWIT(1)
                          END IF
C
                      END IF
                  END IF
C       PUCX SOLVE FOR SURFACE L HANDLED.
              ELSE
C       NO PUCX SOLVE
              END IF
C***************************************************************************
C                       PIX SOLVES
C***************************************************************************
C       IS THERE A PIX SOLVE ON SURFACE L
              IF(SOLVE(2,L).EQ.9.0D0) THEN
C       YES THERE IS A PIX SOLVE ON SURFACE L, HANDLE IT
C       FOR A PIX SOLVE
C       CV(L)=(SOLVE TARGET-PUX(L-1))/PX(L)
C       AND PIX(L)=SOLVE TARGET
C       IS SURFACE L AN Y-TORIC,IF SO THE TORIC CURVATURE AND
C       NOT THE MAIN CURVATURE IS CHANGED.
                  IF(ALENS(23,L).EQ.1.0D0) THEN
C       SURFACE IS Y-TORIC
                      PXTRAX(3,L)=SOLVE(1,L)
                      IF(PXTRAX(1,L).EQ.0.0D0) THEN
                          ALENS(24,L)=0.0D0
                      ELSE
                          ALENS(24,L)=(PXTRAX(3,L)-PXTRAX(2,(L-1)))/
     1                    (PXTRAX(1,(L)))
                      END IF
                  ELSE
C       SURFACE NOT Y-TORIC,PROCEED
                      PXTRAX(3,L)=SOLVE(1,L)
                      IF(PXTRAX(1,L).EQ.0.0D0.OR.(J_NP-N).EQ.0.0D0) THEN
                          ALENS(1,L)=0.0D0
C
                          IF(ALENS(1,L).EQ.0.0D0.AND.ALENS(2,L).NE.0.0D0) THEN
                              ALENS(2,L)=0.0D0
                              OUTLYNE='WARNING:'
                              CALL SHOWIT(1)
                              WRITE(OUTLYNE,*) 'FOR SURFACE ',L
                              CALL SHOWIT(1)
                              OUTLYNE=
     1                        'THE CONIC CONSTANT RESET TO 0.0 FOR THIS PLANO SURFACE'
                              CALL SHOWIT(1)
                          END IF
                          IF(ALENS(1,L).NE.0.0D0.AND.ALENS(43,L).NE.0.0D0) THEN
                              ALENS(43,L)=0.0D0
                              OUTLYNE='WARNING:'
                              CALL SHOWIT(1)
                              WRITE(OUTLYNE,*) 'FOR SURFACE ',L
                              CALL SHOWIT(1)
                              OUTLYNE=
     1                        'THE "AC" TERM RESET TO 0.0 FOR THIS NON-PLANO SURFACE'
                              CALL SHOWIT(1)
                          END IF
C
                      ELSE
                          ALENS(1,L)=(PXTRAX(3,L)-PXTRAX(2,(L-1)))/
     1                    (PXTRAX(1,(L)))
C
                          IF(ALENS(1,L).EQ.0.0D0.AND.ALENS(2,L).NE.0.0D0) THEN
                              ALENS(2,L)=0.0D0
                              OUTLYNE='WARNING:'
                              CALL SHOWIT(1)
                              WRITE(OUTLYNE,*) 'FOR SURFACE ',L
                              CALL SHOWIT(1)
                              OUTLYNE=
     1                        'THE CONIC CONSTANT RESET TO 0.0 FOR THIS PLANO SURFACE'
                              CALL SHOWIT(1)
                          END IF
                          IF(ALENS(1,L).NE.0.0D0.AND.ALENS(43,L).NE.0.0D0) THEN
                              ALENS(43,L)=0.0D0
                              OUTLYNE='WARNING:'
                              CALL SHOWIT(1)
                              WRITE(OUTLYNE,*) 'FOR SURFACE ',L
                              CALL SHOWIT(1)
                              OUTLYNE=
     1                        'THE "AC" TERM RESET TO 0.0 FOR THIS NON-PLANO SURFACE'
                              CALL SHOWIT(1)
                          END IF
C
                      END IF
                  END IF
C       PIX SOLVE FOR SURFACE L HANDLED.
              ELSE
C       NO PIX SOLVE
              END IF
C*****************************************************************************
C                       PICX SOLVES
C***************************************************************************
C       IS THERE A PICX SOLVE ON SURFACE L
              IF(SOLVE(2,L).EQ.12.0D0) THEN
C       YES THERE IS A PICX SOLVE ON SURFACE L, HANEEL IT
C       FOR A PICX SOLVE
C       CV(L)=(SOLVE TARGET-PUCX(L-1))/PCX(L)
C       AND PICX(L)=SOLVE TARGET
C       IS SURFACE L AN Y-TORIC,IF SO THE TORIC CURVATURE AND
C       NOT THE MAIN CURVATURE IS CHANGED.
                  IF(ALENS(23,L).EQ.1.0D0) THEN
C       SURFACE IS Y-TORIC
                      PXTRAX(7,L)=SOLVE(1,L)
                      IF(PXTRAX(5,L).EQ.0.0D0) THEN
                          ALENS(24,L)=0.0D0
                      ELSE
                          ALENS(24,L)=(PXTRAX(7,L)-PXTRAX(6,(L-1)))/
     1                    (PXTRAX(5,(L)))
                      END IF
                  ELSE
C       SURFACE NOT Y-TORIC,PROCEED
                      PXTRAX(7,L)=SOLVE(1,L)
                      IF(PXTRAX(5,L).EQ.0.0D0) THEN
                          ALENS(1,L)=0.0D0
C
                          IF(ALENS(1,L).EQ.0.0D0.AND.ALENS(2,L).NE.0.0D0) THEN
                              ALENS(2,L)=0.0D0
                              OUTLYNE='WARNING:'
                              CALL SHOWIT(1)
                              WRITE(OUTLYNE,*) 'FOR SURFACE ',L
                              CALL SHOWIT(1)
                              OUTLYNE=
     1                        'THE CONIC CONSTANT RESET TO 0.0 FOR THIS PLANO SURFACE'
                              CALL SHOWIT(1)
                          END IF
                          IF(ALENS(1,L).NE.0.0D0.AND.ALENS(43,L).NE.0.0D0) THEN
                              ALENS(43,L)=0.0D0
                              OUTLYNE='WARNING:'
                              CALL SHOWIT(1)
                              WRITE(OUTLYNE,*) 'FOR SURFACE ',L
                              CALL SHOWIT(1)
                              OUTLYNE=
     1                        'THE "AC" TERM RESET TO 0.0 FOR THIS NON-PLANO SURFACE'
                              CALL SHOWIT(1)
                          END IF
C
                      ELSE
                          ALENS(1,L)=(PXTRAX(7,L)-PXTRAX(6,(L-1)))/
     1                    (PXTRAX(5,(L-1)))
C
                          IF(ALENS(1,L).EQ.0.0D0.AND.ALENS(2,L).NE.0.0D0) THEN
                              ALENS(2,L)=0.0D0
                              OUTLYNE='WARNING:'
                              CALL SHOWIT(1)
                              WRITE(OUTLYNE,*) 'FOR SURFACE ',L
                              CALL SHOWIT(1)
                              OUTLYNE=
     1                        'THE CONIC CONSTANT RESET TO 0.0 FOR THIS PLANO SURFACE'
                              CALL SHOWIT(1)
                          END IF
                          IF(ALENS(1,L).NE.0.0D0.AND.ALENS(43,L).NE.0.0D0) THEN
                              ALENS(43,L)=0.0D0
                              OUTLYNE='WARNING:'
                              CALL SHOWIT(1)
                              WRITE(OUTLYNE,*) 'FOR SURFACE ',L
                              CALL SHOWIT(1)
                              OUTLYNE=
     1                        'THE "AC" TERM RESET TO 0.0 FOR THIS NON-PLANO SURFACE'
                              CALL SHOWIT(1)
                          END IF
C
                      END IF
                  END IF
C       PICX SOLVE FOR SURFACE L HANDLED.
              ELSE
C       NO PICX SOLVE
              END IF
C*****************************************************************************
C                       APX SOLVES
C***************************************************************************
C       IS THERE A APX SOLVE ON SURFACE L
              IF(SOLVE(2,L).EQ.8.0D0) THEN
C       YES THERE IS A APX SOLVE ON SURFACE L, HANDLE IT
C       FOR AN APX SOLVE
C       CV(L)=-(PUX(L-1)/PX(L))*((N'+N)/N)
C       THEN
C       PUX(L)=(N/N')*PUX(L-1)-CV(L)*PX(L)*((N'-N)/N')
C       PIX(L)=CV(L)*PX(L)+PUX(L-1)
C       PIX'(L)=(N/N')*PIX(L)
C       IS SURFACE L AN Y-TORIC,IF SO THE TORIC CURVATURE AND
C       NOT THE MAIN CURVATURE IS CHANGED.
                  IF(ALENS(23,L).EQ.1.0D0) THEN
C       SURFACE IS Y-TORIC, PROCEED
                      IF(PXTRAX(1,L).EQ.0.0D0.OR.(J_NP+N).EQ.0.0D0) THEN
                          ALENS(24,L)=0.0D0
                      ELSE
                          ALENS(24,L)=-(PXTRAX(2,(L-1))/PXTRAX(1,L))*((J_NP+N)/N)
                      END IF
C       RECALCULATE ANGLES OF INCIDENCE,REFLECTION/REFRACTION
C       AND EXITING SLOPE ANGLE
                      PXTRAX(2,L)=((N/NP)*PXTRAX(2,(L-1)))-(ALENS(24,L)*
     1                PXTRAX(1,L)*((J_NP-N)/J_NP))
                      PXTRAX(3,L)=(ALENS(24,L)*PXTRAX(1,L))+PXTRAX(2,(L-1))
                      PXTRAX(4,L)=(N/J_NP)*PXTRAX(3,L)
                  ELSE
C       SURFACE NOT Y-TORIC,PROCEED
                      IF(PXTRAX(1,L).EQ.0.0D0.OR.(J_NP+N).EQ.0.0D0) THEN
                          ALENS(1,L)=0.0D0
C
                          IF(ALENS(1,L).EQ.0.0D0.AND.ALENS(2,L).NE.0.0D0) THEN
                              ALENS(2,L)=0.0D0
                              OUTLYNE='WARNING:'
                              CALL SHOWIT(1)
                              WRITE(OUTLYNE,*) 'FOR SURFACE ',L
                              CALL SHOWIT(1)
                              OUTLYNE=
     1                        'THE CONIC CONSTANT RESET TO 0.0 FOR THIS PLANO SURFACE'
                              CALL SHOWIT(1)
                          END IF
                          IF(ALENS(1,L).NE.0.0D0.AND.ALENS(43,L).NE.0.0D0) THEN
                              ALENS(43,L)=0.0D0
                              OUTLYNE='WARNING:'
                              CALL SHOWIT(1)
                              WRITE(OUTLYNE,*) 'FOR SURFACE ',L
                              CALL SHOWIT(1)
                              OUTLYNE=
     1                        'THE "AC" TERM RESET TO 0.0 FOR THIS NON-PLANO SURFACE'
                              CALL SHOWIT(1)
                          END IF
C
                      ELSE
                          ALENS(1,L)=-(PXTRAX(2,(L-1))/PXTRAX(1,L))*((J_NP+N)/N)
C
                          IF(ALENS(1,L).EQ.0.0D0.AND.ALENS(2,L).NE.0.0D0) THEN
                              ALENS(2,L)=0.0D0
                              OUTLYNE='WARNING:'
                              CALL SHOWIT(1)
                              WRITE(OUTLYNE,*) 'FOR SURFACE ',L
                              CALL SHOWIT(1)
                              OUTLYNE=
     1                        'THE CONIC CONSTANT RESET TO 0.0 FOR THIS PLANO SURFACE'
                              CALL SHOWIT(1)
                          END IF
                          IF(ALENS(1,L).NE.0.0D0.AND.ALENS(43,L).NE.0.0D0) THEN
                              ALENS(43,L)=0.0D0
                              OUTLYNE='WARNING:'
                              CALL SHOWIT(1)
                              WRITE(OUTLYNE,*) 'FOR SURFACE ',L
                              CALL SHOWIT(1)
                              OUTLYNE=
     1                        'THE "AC" TERM RESET TO 0.0 FOR THIS NON-PLANO SURFACE'
                              CALL SHOWIT(1)
                          END IF
C
                      END IF
C       RECALCULATE ANGLES OF INCIDENCE,REFLECTION/REFRACTION
C       AND EXITING SLOPE ANGLE
                      PXTRAX(2,L)=((N/J_NP)*PXTRAX(2,(L-1)))-(ALENS(1,L)*
     1                PXTRAX(1,L)*((J_NP-N)/J_NP))
                      PXTRAX(3,L)=(ALENS(1,L)*PXTRAX(1,L))+PXTRAX(2,(L-1))
                      PXTRAX(4,L)=(N/J_NP)*PXTRAX(3,L)
                  END IF
C       APX SOLVE FOR SURFACE L HANDLED.
              ELSE
C       NO APX SOLVE
              END IF
C***************************************************************************
C                       APCX SOLVES
C***************************************************************************
C       IS THERE A APCX SOLVE ON SURFACE L
              IF(SOLVE(2,L).EQ.11.0D0) THEN
C       YES THERE IS A APCX SOLVE ON SURFACE L, HANDLE IT
C       FOR AN APCX SOLVE
C       CV(L)=-(PUCX(L-1)/PCX(L))*((N'+N)/N)
C       THEN
C       PUCX(L)=(N/N')*PUCX(L-1)-CV(L)*PCX(L)*((N'-N)/N')
C       PICX(L)=CV(L)*PCX(L)+PUCX(L-1)
C       PICX'(L)=(N/N')*PICX(L)
C       IS SURFACE L AN Y-TORIC,IF SO THE TORIC CURVATURE AND
C       NOT THE MAIN CURVATURE IS CHANGED.
                  IF(ALENS(23,L).EQ.1.0D0) THEN
C       SURFACE IS Y-TORIC, PROCEED
                      IF(PXTRAX(5,L).EQ.0.0D0.OR.(J_NP+N).EQ.0.0D0) THEN
                          ALENS(24,L)=0.0D0
                      ELSE
                          ALENS(24,L)=-(PXTRAX(6,(L-1))/PXTRAX(5,L))*((J_NP+N)/N)
                      END IF
C       RECALCULATE ANGLES OF INCIDENCE,REFLECTION/REFRACTION
C       AND EXITING SLOPE ANGLE
                      PXTRAX(6,L)=((N/J_NP)*PXTRAX(6,(L-1)))-(ALENS(24,L)*
     1                PXTRAX(5,L)*((J_NP-N)/J_NP))
                      PXTRAX(7,L)=(ALENS(24,L)*PXTRAX(5,L))+PXTRAX(6,(L-1))
                      PXTRAX(8,L)=(N/J_NP)*PXTRAX(7,L)
                  ELSE
C       SURFACE NOT Y-TORIC,PROCEED
                      IF(PXTRAX(5,L).EQ.0.0D0.OR.(J_NP+N).EQ.0.0D0) THEN
                          ALENS(1,L)=0.0D0
C
                          IF(ALENS(1,L).EQ.0.0D0.AND.ALENS(2,L).NE.0.0D0) THEN
                              ALENS(2,L)=0.0D0
                              OUTLYNE='WARNING:'
                              CALL SHOWIT(1)
                              WRITE(OUTLYNE,*) 'FOR SURFACE ',L
                              CALL SHOWIT(1)
                              OUTLYNE=
     1                        'THE CONIC CONSTANT RESET TO 0.0 FOR THIS PLANO SURFACE'
                              CALL SHOWIT(1)
                          END IF
                          IF(ALENS(1,L).NE.0.0D0.AND.ALENS(43,L).NE.0.0D0) THEN
                              ALENS(43,L)=0.0D0
                              OUTLYNE='WARNING:'
                              CALL SHOWIT(1)
                              WRITE(OUTLYNE,*) 'FOR SURFACE ',L
                              CALL SHOWIT(1)
                              OUTLYNE=
     1                        'THE "AC" TERM RESET TO 0.0 FOR THIS NON-PLANO SURFACE'
                              CALL SHOWIT(1)
                          END IF
C
                      ELSE
                          ALENS(1,L)=-(PXTRAX(6,(L-1))/PXTRAX(5,L))*((J_NP+N)/N)
C
                          IF(ALENS(1,L).EQ.0.0D0.AND.ALENS(2,L).NE.0.0D0) THEN
                              ALENS(2,L)=0.0D0
                              OUTLYNE='WARNING:'
                              CALL SHOWIT(1)
                              WRITE(OUTLYNE,*) 'FOR SURFACE ',L
                              CALL SHOWIT(1)
                              OUTLYNE=
     1                        'THE CONIC CONSTANT RESET TO 0.0 FOR THIS PLANO SURFACE'
                              CALL SHOWIT(1)
                          END IF
                          IF(ALENS(1,L).NE.0.0D0.AND.ALENS(43,L).NE.0.0D0) THEN
                              ALENS(43,L)=0.0D0
                              OUTLYNE='WARNING:'
                              CALL SHOWIT(1)
                              WRITE(OUTLYNE,*) 'FOR SURFACE ',L
                              CALL SHOWIT(1)
                              OUTLYNE=
     1                        'THE "AC" TERM RESET TO 0.0 FOR THIS NON-PLANO SURFACE'
                              CALL SHOWIT(1)
                          END IF
C
                      END IF
C       RECALCULATE ANGLES OF INCIDENCE,REFLECTION/REFRACTION
C       AND EXITING SLOPE ANGLE
                      PXTRAX(6,L)=((N/J_NP)*PXTRAX(6,(L-1)))-(ALENS(1,L)*
     1                PXTRAX(5,L)*((J_NP-N)/J_NP))
                      PXTRAX(7,L)=(ALENS(1,L)*PXTRAX(5,L))+PXTRAX(6,(L-1))
                      PXTRAX(8,L)=(N/J_NP)*PXTRAX(7,L)
                  END IF
C       APCX SOLVE FOR SURFACE L HANDLED.
              ELSE
C       NO APCX SOLVE
              END IF
C**************************************************************************
C                       COCX SOLVES
C***************************************************************************
C       IS THERE A COCX SOLVE ON SURFACE L
              IF(SOLVE(2,L).EQ.14.0D0) THEN
                  DIS=0.0D0
C       YES THERE IS A COCX SOLVE ON SURFACE L, HANDLE IT
C       FOR AN COCX SOLVE
C       CV(L)=SIGNED DISTANCE TO THE SPECIFIED SURFACE(TAR)
C       FROM SURFACE L EQUAL TO DIS
                  TAR=INT(SOLVE(1,L))
                  IF(TAR.LT.L) THEN
                      DO I=TAR,(L-1)
                          DIS=DIS+ALENS(3,I)
                      END DO
                  END IF
                  IF(TAR.GT.L) THEN
                      DO I=L,(TAR-1)
                          DIS=DIS+ALENS(3,I)
                      END DO
                  END IF
C       CASE OF TAR=L ELIMINATED INSIDE OD SUBROUTINE LNSEOS.
C       THEN RECALCULATE
C       PUCX(L)=(N/N')*PUCX(L-1)-CV(L)*PCX(L)*((N'-N)/N')
C       PICX(L)=CV(L)*PCX(L)+PUCX(L-1)
C       PICX'(L)=(N/N')*PICX(L)
C       IS SURFACE L AN Y-TORIC,IF SO THE TORIC CURVATURE AND
C       NOT THE MAIN CURVATURE IS CHANGED.
                  IF(ALENS(23,L).EQ.1.0D0) THEN
C       SURFACE IS Y-TORIC, PROCEED
                      IF(DIS.NE.0.0D0) THEN
                          IF(TAR.LT.L) ALENS(24,L)=-1.0D0/DIS
                          IF(TAR.GT.L) ALENS(24,L)= 1.0D0/DIS
                      ELSE
C       DIS IS ZERO, CONCENTRIC SOLVE IGNORED
                          OUTLYNE='(COCX) SOLVE RESULTS IN A ZERO RADIUS OF CURVATURE'
                          CALL SHOWIT(1)
                          OUTLYNE='(COCX) SOLVE REMOVED'
                          CALL SHOWIT(1)
                          SOLVE(2,L)=0.0D0
                          SOLVE(1,0)=0.0D0
                      END IF
C       RECALCULATE ANGLES OF INCIDENCE,REFLECTION/REFRACTION
C       AND EXITING SLOPE ANGLE
                      PXTRAX(6,L)=((N/J_NP)*PXTRAX(6,(L-1)))-(ALENS(24,L)*
     1                PXTRAX(5,L)*((J_NP-N)/J_NP))
                      PXTRAX(7,L)=(ALENS(24,L)*PXTRAX(5,L))+PXTRAX(6,(L-1))
                      PXTRAX(8,L)=(N/J_NP)*PXTRAX(7,L)
                  ELSE
C       SURFACE NOT Y-TORIC,PROCEED
                      IF(DIS.NE.0.0D0) THEN
                          IF(TAR.LT.L) ALENS(1,L)=-1.0D0/DIS
                          IF(TAR.GT.L) ALENS(1,L)=1.0D0/DIS
C
                          IF(ALENS(1,L).EQ.0.0D0.AND.ALENS(2,L).NE.0.0D0) THEN
                              ALENS(2,L)=0.0D0
                              OUTLYNE='WARNING:'
                              CALL SHOWIT(1)
                              WRITE(OUTLYNE,*) 'FOR SURFACE ',L
                              CALL SHOWIT(1)
                              OUTLYNE=
     1                        'THE CONIC CONSTANT RESET TO 0.0 FOR THIS PLANO SURFACE'
                              CALL SHOWIT(1)
                          END IF
                          IF(ALENS(1,L).NE.0.0D0.AND.ALENS(43,L).NE.0.0D0) THEN
                              ALENS(43,L)=0.0D0
                              OUTLYNE='WARNING:'
                              CALL SHOWIT(1)
                              WRITE(OUTLYNE,*) 'FOR SURFACE ',L
                              CALL SHOWIT(1)
                              OUTLYNE=
     1                        'THE "AC" TERM RESET TO 0.0 FOR THIS NON-PLANO SURFACE'
                              CALL SHOWIT(1)
                          END IF
C
                      ELSE
C       DIS IS ZERO, CONCENTRIC SOLVE IGNORED
                          OUTLYNE='(COCX) SOLVE RESULTS IN A ZERO RADIUS OF CURVATURE'
                          CALL SHOWIT(1)
                          OUTLYNE='(COCX) SOLVE REMOVED'
                          CALL SHOWIT(1)
                          SOLVE(2,L)=0.0D0
                          SOLVE(1,0)=0.0D0
                      END IF
C       RECALCULATE ANGLES OF INCIDENCE,REFLECTION/REFRACTION
C       AND EXITING SLOPE ANGLE
                      PXTRAX(6,L)=((N/J_NP)*PXTRAX(6,(L-1)))-(ALENS(1,L)*
     1                PXTRAX(5,L)*((J_NP-N)/J_NP))
                      PXTRAX(7,L)=(ALENS(1,L)*PXTRAX(5,L))+PXTRAX(6,(L-1))
                      PXTRAX(8,L)=(N/J_NP)*PXTRAX(7,L)
                  END IF
C       COCX SOLVE FOR SURFACE L HANDLED.
              ELSE
C       NO COCX SOLVE
              END IF
C**************************************************************************
C               NOW THICKNESS SOLVES
C**************************************************************************
C                       PX SOLVES
C***************************************************************************
C       IS THERE A PX SOLVE ON SURFACE L
              IF(SOLVE(4,L).EQ.4.0D0) THEN
C       YES THERE IS A PX SOLVE ON SURFACE L
C       WHICH AFFECTS THE PX VALUE ON (L+1), HANDLE IT
C       TH(L)=(SOLVE TARGET-PX(L))/PUX(L)
C
                  IF(PXTRAX(2,L).EQ.0.0D0) THEN
                      ALENS(3,L)=1D20
                  ELSE
                      ALENS(3,L)=(SOLVE(3,L)-PXTRAX(1,(L)))/(PXTRAX(2,L))
                      IF(DABS(ALENS(3,L)).GT.1D20) THEN
                          IF(ALENS(3,L).GT.0.0D0)THEN
                              ALENS(3,L)=1D20
                          ELSE
                          END IF
                          IF(ALENS(3,L).LT.0.0D0) THEN
                              ALENS(3,L)=-1D20
                          ELSE
                          END IF
                      ELSE
                      END IF
                  END IF
C       PX SOLVE FOR SURFACE L HANDLED.
              ELSE
C       NO PX SOLVE
              END IF
C**************************************************************************
C                       PCX SOLVES
C***************************************************************************
C       IS THERE A PCX SOLVE ON SURFACE L
              IF(SOLVE(4,L).EQ.5.0D0) THEN
C       YES THERE IS A PCX SOLVE ON SURFACE L
C       WHICH AFFECTS THE PCX VALUE ON (L+1), HANDLE IT
C       TH(L)=(SOLVE TARGET-PCX(L))/PUCX(L)
C
                  IF(PXTRAX(6,L).EQ.0.0D0) THEN
                      ALENS(3,L)=1D20
                  ELSE
                      ALENS(3,L)=(SOLVE(3,L)-PXTRAX(5,(L)))/(PXTRAX(6,L))
                      IF(DABS(ALENS(3,L)).GT.1D20) THEN
                          IF(ALENS(3,L).GT.0.0D0)THEN
                              ALENS(3,L)=1D20
                          ELSE
                          END IF
                          IF(ALENS(3,L).LT.0.0D0) THEN
                              ALENS(3,L)=-1D20
                          ELSE
                          END IF
                      ELSE
                      END IF
                  END IF
C       PCX SOLVE FOR SURFACE L HANDLED.
              ELSE
C       NO PCX SOLVE
              END IF
C***************************************************************************
C                       CAX SOLVES
C******************************************************************************
C       CHECK FOR CAX SOLVE ON (L) AND IF FOUND, HANDLE IT.
              IF(SOLVE(4,(L)).EQ.6.0D0) THEN
C       THERE IS A CAX SOLVE ON (L) WHICH AFFECTS TH(L), HANDLE IT.
C       IS THER A CIRCULAR CLEAR APERTURE ON SURFACE L
                  IF(ALENS(9,(L)).EQ.1.0D0.AND.ALENS(127,L).NE.0.0D0.OR.
     1            ALENS(9,(L)).EQ.1.0D0.AND.ALENS(127,L).NE.0.0D0) THEN
C       YES THERE IS, SET EDGVAL TO IT
                      IF(ALENS(10,L).LE.ALENS(11,L)) THEN
                          EDGVAL=DABS(ALENS(10,(L)))
                      ELSE
                          EDGVAL=DABS(ALENS(11,(L)))
                      END IF
                  ELSE
C       NO CIRCULAR CLAP, USE PX(L)+PCX(L) FOR EDGVAL
                      EDGVAL=DABS(PXTRAX(1,(L)))+DABS(PXTRAX(5,(L)))
                  END IF
C                       NOW EDGEVAL IS SET
C       NOW CALCULATE THE SAG OF SURFACES L AND L+1
C       CONSIDER THE XZ-PLANE ONLY AND IGNORE TILTS,
C       DECENTERS AND SPECIAL SURFACE DATA. CONSIDER
C       TORICS IF PRESENT. CONSIDER CONIC AND ASPHERIC
C       TERMS IF PRESENT.
C       CHECK FOR ANAMORPHICS
                  IF(ALENS(36,L).EQ.0.0D0) THEN
C       IS THE SURFACE L Y-TORIC BUT NOT ANAMORPHIC ASPHERIC ?
                      IF(ALENS(23,(L)).EQ.1D0) THEN
C       USE TORIC DATA IN SAG CALC
                          ARG=(1.0D0-(1.0D0*(ALENS(1,L)**2)*(EDGVAL**2)))
                          IF(ARG.LT.0.0D0) ARG=0.0D0
                          SAGL=(((EDGVAL**2)*(ALENS(24,(L))))/
     1                    (1.0D0+DSQRT(ARG)))
                      ELSE
C       NOT Y-TORIC
                          ARG=1.0D0-((ALENS(2,L)+1.0D0)*(ALENS(1,L)**2)*(EDGVAL**2))
                          IF(ARG.LT.0.0D0) ARG=0.0D0
                          SAGL=(((EDGVAL**2)*(ALENS(1,(L))))/
     1                    (1.0D0+DSQRT(ARG)))+
     3                    (ALENS(4,(L))*(EDGVAL**4))+
     4                    (ALENS(5,(L))*(EDGVAL**6))+
     5                    (ALENS(6,(L))*(EDGVAL**8))+
     6                    (ALENS(7,(L))*(EDGVAL**10))+
     6                    (ALENS(81,(L))*(EDGVAL**12))+
     6                    (ALENS(82,(L))*(EDGVAL**14))+
     6                    (ALENS(83,(L))*(EDGVAL**16))+
     6                    (ALENS(84,(L))*(EDGVAL**18))+
     6                    (ALENS(85,(L))*(EDGVAL**20))
                      END IF
                  ELSE
C       ANAMORPHIC ASPHERIC
                      IF(ALENS(23,L).EQ.1.0D0) THEN
C       Y-TORIC
                          ARG=1.0D0-((ALENS(41,L)+1.0D0)*(ALENS(24,L)**2)*(EDGVAL**2))
                          IF(ARG.LT.0.0D0) ARG=0.0D0
                          SAGL=(((EDGVAL**2)*(ALENS(24,L)))/
     1                    (1.0D0+DSQRT(ARG)))+
     3                    (ALENS(4,L)*(((1.0D0+ALENS(37,L))*(EDGVAL**2))**2))+
     3                    (ALENS(5,L)*(((1.0D0+ALENS(38,L))*(EDGVAL**2))**3))+
     3                    (ALENS(6,L)*(((1.0D0+ALENS(39,L))*(EDGVAL**2))**4))+
     3                    (ALENS(7,L)*(((1.0D0+ALENS(40,L))*(EDGVAL**2))**5))
                      ELSE
                      END IF
                      IF(ALENS(23,L).EQ.2.0D0) THEN
C       X-TORIC
                          ARG=1.0D0-((ALENS(2,L)+1.0D0)*(ALENS(1,L)**2)*(EDGVAL**2))
                          IF(ARG.LT.0.0D0) ARG=0.0D0
                          SAGL=(((EDGVAL**2)*(ALENS(1,L)))/
     1                    (1.0D0+DSQRT(ARG)))+
     3                    (ALENS(4,L)*(((1.0D0-ALENS(37,L))*(EDGVAL**2))**2))+
     3                    (ALENS(5,L)*(((1.0D0-ALENS(38,L))*(EDGVAL**2))**3))+
     3                    (ALENS(6,L)*(((1.0D0-ALENS(39,L))*(EDGVAL**2))**4))+
     3                    (ALENS(7,L)*(((1.0D0-ALENS(40,L))*(EDGVAL**2))**5))
                      ELSE
                      END IF
                  END IF
C       CHECK FOR ANAMORPHICS
                  IF(ALENS(36,(L+1)).EQ.0.0D0) THEN
C       IS THE SURFACE L+1 Y-TORIC BUT NOT ANAMORPHIC ASPHERIC ?
                      IF(ALENS(23,(L+1)).EQ.1.0D0) THEN
C       USE TORIC DATA IN SAG CALC
                          ARG=1.0D0-((1.0D0)*(ALENS(2,L)**2)*(EDGVAL**2))
                          IF(ARG.LT.0.0D0) ARG=0.0D0
                          SAGLP1=(((EDGVAL**2)*(ALENS(24,(L+1))))/
     1                    (1.0D0+DSQRT(ARG)))
                      ELSE
C       NOT Y-TORIC
                          ARG=1.0D0-((ALENS(2,L+1)+1.0D0)*(ALENS(1,L+1)**2)*(EDGVAL**2))
                          IF(ARG.LT.0.0D0) ARG=0.0D0
                          SAGLP1=(((EDGVAL**2)*(ALENS(1,(L+1))))/
     1                    (1.0D0+DSQRT(ARG)))+
     3                    (ALENS(4,(L+1))*(EDGVAL**4))+
     4                    (ALENS(5,(L+1))*(EDGVAL**6))+
     5                    (ALENS(6,(L+1))*(EDGVAL**8))+
     6                    (ALENS(7,(L+1))*(EDGVAL**10))+
     6                    (ALENS(81,(L+1))*(EDGVAL**12))+
     6                    (ALENS(82,(L+1))*(EDGVAL**14))+
     6                    (ALENS(83,(L+1))*(EDGVAL**16))+
     6                    (ALENS(84,(L+1))*(EDGVAL**18))+
     6                    (ALENS(85,(L+1))*(EDGVAL**20))
                      END IF
                  ELSE
C       ANAMORPHIC ASPHERIC
                      IF(ALENS(23,(L+1)).EQ.1.0D0) THEN
C       Y-TORIC
                          ARG=1.0D0-((ALENS(41,L+1)+1.0D0)*(ALENS(24,L+1)**2)*(EDGVAL**2))
                          IF(ARG.LT.0.0D0) ARG=0.0D0
                          SAGL=(((EDGVAL**2)*(ALENS(24,(L+1))))/
     1                    (1.0D0+DSQRT(ARG)))+
     3                    (ALENS(4,(L+1))*(((1.0D0+ALENS(37,(L+1)))*(EDGVAL**2))**2))+
     3                    (ALENS(5,(L+1))*(((1.0D0+ALENS(38,(L+1)))*(EDGVAL**2))**3))+
     3                    (ALENS(6,(L+1))*(((1.0D0+ALENS(39,(L+1)))*(EDGVAL**2))**4))+
     3                    (ALENS(7,(L+1))*(((1.0D0+ALENS(40,(L+1)))*(EDGVAL**2))**5))
                      ELSE
                      END IF
                      IF(ALENS(23,L).EQ.2.0D0) THEN
C       X-TORIC
                          ARG=1.0D0-((ALENS(2,L+1)+1.0D0)*(ALENS(1,L+1)**2)*(EDGVAL**2))
                          IF(ARG.LT.0.0D0) ARG=0.0D0
                          SAGL=(((EDGVAL**2)*(ALENS(1,(L+1))))/
     1                    (1.0D0+DSQRT(ARG)))+
     3                    (ALENS(4,(L+1))*(((1.0D0-ALENS(37,(L+1)))*(EDGVAL**2))**2))+
     3                    (ALENS(5,(L+1))*(((1.0D0-ALENS(38,(L+1)))*(EDGVAL**2))**3))+
     3                    (ALENS(6,(L+1))*(((1.0D0-ALENS(39,(L+1)))*(EDGVAL**2))**4))+
     3                    (ALENS(7,(L+1))*(((1.0D0-ALENS(40,(L+1)))*(EDGVAL**2))**5))
                      ELSE
                      END IF
                  END IF
C       NOW THE SAGS OF SURFACES L AND L+1 HAVE BEEN
C       CALCULATED. NOW CALCULATE THE NEW ADJUSTED
C       THICKNESS, TH(L).
C       RULES:
C               IF SAGL-SAGLP1>0 THEN
C               TH(L)=(CAX TARGET VALUE)+(SAGL-SAGLP1)
C               IF SAGL-SAGLP1)<OR= 0 THEN
C               TH(L)=(CAX TARGET VALUE)
                  IF((SAGL-SAGLP1).GT.0.0D0)
     1            ALENS(3,(L))=SOLVE(3,(L))+(SAGL-SAGLP1)
                  IF((SAGL-SAGLP1).LE.0.0D0)
     1            ALENS(3,(L))=SOLVE(3,(L))
C       HANDLEING OF CAX SOLVE IS COMPLETED
              ELSE
C       NO CAX SOLVE IS PRESENT, PROCEED
              END IF
C       FINISHED WITH POSSIBLE CAX SOLVES ON TH(L)
              IF(SOLVE(4,L).NE.0.0D0.OR.SOLVE(2,L).NE.0.0D0) THEN
C     THERE WERE XZ=PLANE SOLVES, CALC VALUES
                  FINY=L
                  CALL FINIXZ
              ELSE
              END IF
C               ALL SURFACE (L) SOLVES NOW HANDLED.
C               NOW RETURN TO THE CALLING SUBROUTINE
              RETURN
          ELSE
C       SLV2 NOT 2
          END IF
          RETURN
      END
C SUB SLI.FOR
      SUBROUTINE SLI
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE SLI WHICH IMPLEMENTS THE LI AND LIC
C       COMMAND
C       AT THE LENS OF UPDATE LENS LEVEL OR THE LI COMMAND AT
C       THE CMD LEVEL
C
          CHARACTER DTY*10,TMY*8,NNTT1*99
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
C
C               CHECK FOR PRESENCE OF QUALIFIER OR NUMERIC WORDS
C               PRINT ERROR AND RETURN IF DISCOVERED.
C
C
          IF(F1.EQ.1) THEN
              IF(SQ.EQ.1.OR.SN.EQ.1.OR.SST.EQ.1)THEN
                  OUTLYNE=
     1            '"LI" AND "LIC" TAKE NO EXPLICIT INPUT AT THE CMD LEVEL"'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(SYSTEM1(20).EQ.0.0D0) THEN
                  OUTLYNE='LENS IDENTIFIER DOES NOT EXIST'
                  CALL SHOWIT(1)
                  OUTLYNE='LENS SYSTEM HAS NO SURFACES'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(WC.EQ.'LI') THEN
                  IF(LI.EQ.CNULL) WRITE(OUTLYNE,1001)
                  IF(LI.EQ.CNULL) CALL SHOWIT(0)
                  IF(STMPT) CALL MYTIME(TMY)
                  IF(STMPD) CALL MYDATE(DTY)
                  IF(.NOT.STMPT.AND..NOT.STMPD) NNTT1=LI
                  IF(STMPT.AND.STMPD) NNTT1=TMY//' '//DTY//LI
                  IF(STMPT.AND..NOT.STMPD) NNTT1=TMY//' '//LI
                  IF(.NOT.STMPT.AND.STMPD) NNTT1=DTY//LI
                  IF(LI.NE.CNULL) WRITE(OUTLYNE,1000) NNTT1
                  IF(LI.NE.CNULL) CALL SHOWIT(0)
                  IF(LIC(1).NE.CNULL) THEN
                      WRITE(OUTLYNE,1000) LIC(1)
                      CALL SHOWIT(0)
                  END IF
                  IF(LIC(2).NE.CNULL) THEN
                      WRITE(OUTLYNE,1000) LIC(2)
                      CALL SHOWIT(0)
                  END IF
                  IF(LIC(2).NE.CNULL) THEN
                      WRITE(OUTLYNE,1000) LIC(3)
                      CALL SHOWIT(0)
                  END IF
                  IF(LIC(4).NE.CNULL) THEN
                      WRITE(OUTLYNE,1000) LIC(4)
                      CALL SHOWIT(0)
                  END IF
                  RETURN
              ELSE
C     NOT LI
              END IF
              IF(WC.EQ.'LIC') THEN
                  IF(LIC(1).NE.CNULL) THEN
                      WRITE(OUTLYNE,1000) LIC(1)
                      CALL SHOWIT(0)
                  ELSE
                      WRITE(OUTLYNE,1002)
                      CALL SHOWIT(0)
                  END IF
                  IF(LIC(2).NE.CNULL) THEN
                      WRITE(OUTLYNE,1000) LIC(2)
                      CALL SHOWIT(0)
                  ELSE
                  END IF
                  IF(LIC(3).NE.CNULL) THEN
                      WRITE(OUTLYNE,1000) LIC(3)
                      CALL SHOWIT(0)
                  ELSE
                  END IF
                  IF(LIC(4).NE.CNULL) THEN
                      WRITE(OUTLYNE,1000) LIC(4)
                      CALL SHOWIT(0)
                  ELSE
                  END IF
                  RETURN
              ELSE
C     NOT LIC
              END IF
          ELSE
C               NOT AT CMD LEVEL
C
              IF(F5.EQ.1.OR.F6.EQ.1) THEN
C
C               WE ARE AT LENS INPUT OR LENS UPDATE LEVEL
C
                  IF(WC.EQ.'LIC'.OR.WC.EQ.'LI') THEN
                      IF(SST.EQ.0) THEN
                          OUTLYNE='"LI" AND "LIC" REQUIRE STRING INPUT'
                          CALL SHOWIT(1)
                          OUTLYNE='RE-ENTER COMMAND'
                          CALL SHOWIT(1)
                          CALL MACFAL
                          RETURN
                      END IF
                  END IF
                  IF(WC.EQ.'LIC') THEN
C       DO AN LIC(LICCNT) ASSIGNMENT
C
C
                      LIC(LICCNT)=WS
C       ADVANCE THE LICCNT COUNTER IF LICCNT LESS THAN 4
C
                      IF(LICCNT.LT.4) THEN
                          LICCNT=LICCNT+1
                      ELSE
                          LICCNT=LICCNT
                      END IF
                  END IF
              END IF
C
              IF(WC.EQ.'LI') THEN
                  LI=WS
                  LIC(1)=CNULL
                  LIC(2)=CNULL
                  LIC(3)=CNULL
                  LIC(4)=CNULL
                  LICCNT=1

              ELSE
C       NOT LI
              END IF
          END IF
C
 1000     FORMAT(A79)
 1001     FORMAT('THE CURRENT LI IS BLANK')
 1002     FORMAT('THE CURRENT LIC IS BLANK')
          RETURN
      END


C SUB SINI.FOR
      SUBROUTINE SINI
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE SINI WHICH IMPLEMENTS THE INI
C       COMMAND
C
          CHARACTER NNTT1*99
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
C
C               CHECK FOR PRESENCE OF QUALIFIER OR NUMERIC WORDS
C               PRINT ERROR AND RETURN IF DISCOVERED.
C
C
          IF(F1.EQ.1) THEN
              IF(SQ.EQ.1.OR.SN.EQ.1.OR.SST.EQ.1)THEN
                  OUTLYNE=
     1            '"INI" TAKES NO EXPLICIT INPUT AT THE CMD LEVEL"'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(SYSTEM1(20).EQ.0.0D0) THEN
                  OUTLYNE='DESIGNER INITIALS DO NOT EXIST'
                  CALL SHOWIT(1)
                  OUTLYNE='LENS SYSTEM HAS NO SURFACES'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(INNI.EQ.CNULL) WRITE(OUTLYNE,1001)
              IF(INNI.EQ.CNULL) CALL SHOWIT(0)
              NNTT1(1:80)=INNI(1:70)
              IF(INNI.NE.CNULL) WRITE(OUTLYNE,1000) NNTT1(1:70)
              IF(INNI.NE.CNULL) CALL SHOWIT(0)
              RETURN
          ELSE
C               NOT AT CMD LEVEL
          END IF
C
          IF(F5.EQ.1.OR.F6.EQ.1) THEN
C
C               WE ARE AT LENS INPUT OR LENS UPDATE LEVEL
C
              IF(SST.EQ.0) THEN
                  OUTLYNE='"INI" REQUIRES STRING INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              INNI(1:80)=WS(1:70)
          END IF
C
 1000     FORMAT('INI = ',A70)
 1001     FORMAT('THE CURRENT INI IS BLANK')
          RETURN
      END
C SUB SLTYPE.FOR
      SUBROUTINE SLTYPE
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE SLTYPE WHICH IMPLEMENTS THE LTYPE
C       COMMAND
C
          CHARACTER NNTT1*80
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
C
C               CHECK FOR PRESENCE OF QUALIFIER OR NUMERIC WORDS
C               PRINT ERROR AND RETURN IF DISCOVERED.
C
C
          IF(F1.EQ.1) THEN
              IF(SQ.EQ.1.OR.SN.EQ.1.OR.SST.EQ.1)THEN
                  OUTLYNE=
     1            '"LTYPE" TAKES NO EXPLICIT INPUT AT THE CMD LEVEL"'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(SYSTEM1(20).EQ.0.0D0) THEN
                  OUTLYNE='LENS TYPE IDENTIFIER DOES NOT EXIST'
                  CALL SHOWIT(1)
                  OUTLYNE='LENS SYSTEM HAS NO SURFACES'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(LLTYPE(1:5).EQ.CNULL(1:5)) WRITE(OUTLYNE,1001)
              IF(LLTYPE(1:5).EQ.CNULL(1:5)) CALL SHOWIT(0)
              NNTT1(1:80)=LLTYPE(1:5)
              IF(LLTYPE(1:5).NE.CNULL(1:5)) WRITE(OUTLYNE,1000) NNTT1(1:5)
              IF(LLTYPE.NE.CNULL) CALL SHOWIT(0)
              RETURN
          ELSE
C               NOT AT CMD LEVEL
          END IF
C
          IF(F5.EQ.1.OR.F6.EQ.1) THEN
C
C               WE ARE AT LENS INPUT OR LENS UPDATE LEVEL
C
              IF(SST.EQ.0) THEN
                  OUTLYNE='"LTYPE" REQUIRES STRING INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              LLTYPE(1:80)=WS(1:5)
          END IF
C
 1000     FORMAT('LTYPE = ',A5)
 1001     FORMAT('THE CURRENT LTYPE IS BLANK')
          RETURN
      END
