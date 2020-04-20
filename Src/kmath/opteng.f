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

C       FIRST FILE OF GENERAL OPTICAL ENGINEERING UTILITIES

C SUB XXF.FOR
      SUBROUTINE XXF
C
          IMPLICIT NONE
C
C     XXF IS A SUBROUTINE WHICH CALCULATES OBJECT OR IMAGE
C     DISTACE OR FOCAL LENGTH GIVEN ANY TWO OF THE THREE VALUES
C     USING THE NEWTONIAN EQUATION:
C
C     f**2=-X*X'
C
C       XXF, NW1 NW2 NW2
C       WHERE NW1 IS THE DISTANCE X
C       WHERE NW2 IS THE DISTANCE X'
C       WHERE NW3 IS THE FOCAL LENGTH
C
          REAL*8 VALUE
C
          INCLUDE 'datmai.inc'
C
          IF(SST.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
              WRITE(OUTLYNE,*)
     1        '"XXF (P)" ONLY TAKES NUMERIC WORD #1, #2 AND #3 INPUT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
          IF(SQ.EQ.1.AND.WQ.NE.'P') THEN
              WRITE(OUTLYNE,*)
     1        '"P" IS THE ONLY OPTIONAL QUALIFIER WORD FOR "XXF"'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(DF1.EQ.0.AND.W1.EQ.0.0D0.OR.DF2.EQ.0.AND.W2.EQ.0.0D0
     1    .OR.DF3.EQ.0.AND.W3.EQ.0.0D0) THEN
              WRITE(OUTLYNE,*)
     1        '"XXF (P)" DOES NOT ACCEPT 0.0 FOR ANY OF ITS INPUT VALUES'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(W1.GT.0.0D0.AND.W2.GT.0.0D0.OR.W1.LT.0.0D0.AND.W2.LT.0.0D0)
     1    THEN
              WRITE(OUTLYNE,*)
     1        '"(X) AND (X`) MAY NOT BE BOTH POSITIVE OF BOTH NEGATIVE'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
          IF(DF1.EQ.1.AND.DF2.EQ.0.AND.DF3.EQ.0.OR.
     1       DF1.EQ.0.AND.DF2.EQ.1.AND.DF3.EQ.0.OR.
     2       DF1.EQ.0.AND.DF2.EQ.0.AND.DF3.EQ.1) THEN
C     PROCEED

              IF(DF1.EQ.1) VALUE=-(W3**2)/(W2)
              IF(DF2.EQ.1) VALUE=-(W3**2)/(W1)
              IF(DF3.EQ.1) VALUE=DSQRT(-(W1*W2))
C       UPDATE LASTX
              REG(40)=REG(9)
              REG(9) = VALUE
C
              IF(WQ.EQ.'P') THEN
                  IF(DF1.EQ.1) WRITE(OUTLYNE,100) VALUE
                  IF(DF1.EQ.1) CALL SHOWIT(0)
                  IF(DF1.EQ.1) WRITE(OUTLYNE,20) W2
                  IF(DF1.EQ.1) CALL SHOWIT(0)
                  IF(DF1.EQ.1) WRITE(OUTLYNE,30) W3
                  IF(DF1.EQ.1) CALL SHOWIT(0)
                  IF(DF2.EQ.1) WRITE(OUTLYNE,200) VALUE
                  IF(DF2.EQ.1) CALL SHOWIT(0)
                  IF(DF2.EQ.1) WRITE(OUTLYNE,10) W1
                  IF(DF2.EQ.1) CALL SHOWIT(0)
                  IF(DF2.EQ.1) WRITE(OUTLYNE,30) W3
                  IF(DF2.EQ.1) CALL SHOWIT(0)
                  IF(DF3.EQ.1) WRITE(OUTLYNE,300) VALUE
                  IF(DF3.EQ.1) CALL SHOWIT(0)
                  IF(DF3.EQ.1) WRITE(OUTLYNE,10) W1
                  IF(DF3.EQ.1) CALL SHOWIT(0)
                  IF(DF3.EQ.1) WRITE(OUTLYNE,20) W2
                  IF(DF4.EQ.1) CALL SHOWIT(0)
 10               FORMAT('  INPUT (X) DISTANCE WAS = ',G13.6)
 20               FORMAT(' INPUT (X`) DISTANCE WAS = ',G13.6)
 30               FORMAT('  INPUT FOCAL LENGTH WAS = ',G13.6)
 100              FORMAT(' THE NEW (X) DISTANCE IS = ',G13.6)
 200              FORMAT('THE NEW (X`) DISTANCE IS = ',G13.6)
 300              FORMAT(' THE NEW FOCAL LENGTH IS = ',G13.6)
              END IF
              RETURN
          ELSE
              WRITE(OUTLYNE,*)'"XXF (P)" REQUIRES TWO EXPLICIT NUMERIC VALUES'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
          END IF
          RETURN
      END
C SUB XXFF.FOR
      SUBROUTINE XXFF
C
          IMPLICIT NONE
C
C
C     XXFF IS A SUBROUTINE WHICH CALCULATES OBJECT OR IMAGE
C     DISTACE OR FOCAL LENGTH GIVEN ANY THREE OF THE FOUR VALUES
C     USING THE NEWTONIAN EQUATION:
C
C     ff'=-X*X'
C
C       XXF, NW1 NW2 NW2
C       WHERE NW1 IS THE DISTANCE X
C       WHERE NW2 IS THE DISTANCE X'
C       WHERE NW3 IS THE FOCAL LENGTH f
C       WHERE NW3 IS THE FOCAL LENGTH f'
C
          REAL*8 VALUE
C
          INCLUDE 'datmai.inc'
C
          IF(SST.EQ.1.OR.S5.EQ.1) THEN
              WRITE(OUTLYNE,*)
     1        '"XXFF (P)" ONLY TAKES NUMERIC WORD #1, #2, #3 AND #4 INPUT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
          IF(SQ.EQ.1.AND.WQ.NE.'P') THEN
              WRITE(OUTLYNE,*)
     1        '"P" IS THE ONLY OPTIONAL QUALIFIER WORD FOR "XXFF"'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(DF1.EQ.0.AND.W1.EQ.0.0D0.OR.DF2.EQ.0.AND.W2.EQ.0.0D0
     1    .OR.DF3.EQ.0.AND.W3.EQ.0.0D0.OR.DF4.EQ.0.AND.W4.EQ.0.0D0) THEN
              WRITE(OUTLYNE,*)
     1        '"XXFF (P)" DOES NOT ACCEPT 0.0 FOR ANY OF ITS INPUT VALUES'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
          IF(DF1.EQ.1.AND.DF2.EQ.0.AND.DF3.EQ.0.AND.DF4.EQ.1.OR.
     1    DF1.EQ.0.AND.DF2.EQ.1.AND.DF3.EQ.0.AND.DF4.EQ.0.OR.
     2    DF1.EQ.0.AND.DF2.EQ.0.AND.DF3.EQ.1.AND.DF4.EQ.0.OR.
     3    DF1.EQ.0.AND.DF2.EQ.0.AND.DF3.EQ.0.AND.DF4.EQ.1) THEN
C     PROCEED

              IF(DF1.EQ.1) VALUE=-(W3*W4)/(W2)
              IF(DF2.EQ.1) VALUE=-(W3*W4)/(W1)
              IF(DF3.EQ.1) VALUE=(-(W1*W2))/W4
              IF(DF4.EQ.1) VALUE=(-(W1*W2))/W3
C       UPDATE LASTX
              REG(40)=REG(9)
              REG(9) =VALUE
C
              IF(WQ.EQ.'P') THEN
                  IF(DF1.EQ.1) WRITE(OUTLYNE,100) VALUE
                  IF(DF1.EQ.1) CALL SHOWIT(0)
                  IF(DF1.EQ.1) WRITE(OUTLYNE,20) W2
                  IF(DF1.EQ.1) CALL SHOWIT(0)
                  IF(DF1.EQ.1) WRITE(OUTLYNE,30) W3
                  IF(DF1.EQ.1) CALL SHOWIT(0)
                  IF(DF1.EQ.1) WRITE(OUTLYNE,40) W4
                  IF(DF1.EQ.1) CALL SHOWIT(0)
                  IF(DF2.EQ.1) WRITE(OUTLYNE,200) VALUE
                  IF(DF2.EQ.1) CALL SHOWIT(0)
                  IF(DF2.EQ.1) WRITE(OUTLYNE,10) W1
                  IF(DF2.EQ.1) CALL SHOWIT(0)
                  IF(DF2.EQ.1) WRITE(OUTLYNE,30) W3
                  IF(DF2.EQ.1) CALL SHOWIT(0)
                  IF(DF2.EQ.1) WRITE(OUTLYNE,40) W4
                  IF(DF2.EQ.1) CALL SHOWIT(0)
                  IF(DF3.EQ.1) WRITE(OUTLYNE,300) VALUE
                  IF(DF3.EQ.1) CALL SHOWIT(0)
                  IF(DF3.EQ.1) WRITE(OUTLYNE,10) W1
                  IF(DF3.EQ.1) CALL SHOWIT(0)
                  IF(DF3.EQ.1) WRITE(OUTLYNE,20) W2
                  IF(DF3.EQ.1) CALL SHOWIT(0)
                  IF(DF3.EQ.1) WRITE(OUTLYNE,40) W4
                  IF(DF3.EQ.1) CALL SHOWIT(0)
                  IF(DF4.EQ.1) WRITE(OUTLYNE,400) VALUE
                  IF(DF4.EQ.1) CALL SHOWIT(0)
                  IF(DF4.EQ.1) WRITE(OUTLYNE,10) W1
                  IF(DF4.EQ.1) CALL SHOWIT(0)
                  IF(DF4.EQ.1) WRITE(OUTLYNE,20) W2
                  IF(DF4.EQ.1) CALL SHOWIT(0)
                  IF(DF4.EQ.1) WRITE(OUTLYNE,30) W3
                  IF(DF4.EQ.1) CALL SHOWIT(0)
 10               FORMAT('  INPUT (X) DISTANCE WAS = ',G13.6)
 20               FORMAT(' INPUT (X`) DISTANCE WAS = ',G13.6)
 30               FORMAT('           INPUT (F) WAS = ',G13.6)
 40               FORMAT('           INPUT (F`) WAS = ',G13.6)
 100              FORMAT(' THE NEW (X) DISTANCE IS = ',G13.6)
 200              FORMAT('THE NEW (X`) DISTANCE IS = ',G13.6)
 300              FORMAT('          THE NEW (F) IS = ',G13.6)
 400              FORMAT('         THE NEW (F`) IS = ',G13.6)
              END IF
              RETURN
          ELSE
              WRITE(OUTLYNE,*)
     1        '"XXFF (P)" REQUIRES TWO EXPLICIT NUMERIC VALUES'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
          END IF
          RETURN
      END
C SUB OUTFLT.FOR
      SUBROUTINE OUTFLT
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE OUTFLT. THIS SUBROUTINE IMPLEMENTS THE
C       CURVATURE FOR M FRINGES OUT OF FLAT CALCULATION AND
C     OUTPUTS THE CURVATURE CORRESPONDING
C     TO AN OUT OF FLAT CONDITION MEASURES IN FRINGES AT AN ARBITRARY
C     WAVELENGTH
C       NW1=NUMBER FOR FRINGES OR FRACTION OF FRINGES (REQUIRED)
C       NW2=PART DIAMETER IN CURRENT LENS UNITS (REQUIRED)
C       NW3=WAVELENGTH IN MICRONS (DEFAULT = 0.5461)
C         NW4 AND NW5 NOT USED
C         WQ NOT USED
C         WS NOT USED
C
          CHARACTER UN*12
C
          REAL*8 CURVE,WAVE
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
C
          IF(SYSTEM1(6).EQ.1.0) UN='/INCHES     '
          IF(SYSTEM1(6).EQ.2.0) UN='/CENTIMETERS'
          IF(SYSTEM1(6).EQ.3.0) UN='/MILLIMETERS'
          IF(SYSTEM1(6).EQ.4.0) UN='/METERS'
C
C
          IF(SST.EQ.1) THEN
              WRITE(OUTLYNE,*)'"OUTFLT" TAKES NO ALPHANUMERIC STRING INPUT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(S4.EQ.1.OR.S5.EQ.1) THEN
              WRITE(OUTLYNE,*)'"OUTFLT" TAKES NO NUMERIC WORD #4 OR #5 INPUT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SQ.EQ.1.AND.WQ.NE.'ACC') THEN
              WRITE(OUTLYNE,*)'INVALID QUALIFIER WORD USED WITH "OUTFLT"'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C       HANDLE DEFAULT VALUES FOR NW1, NW2 AND NW3
C
          IF(DF1.EQ.1.OR.DF2.EQ.1) THEN
              WRITE(OUTLYNE,*)
     1        '"OUTFLT" REQUIRES EXPLICIT NUMERIC WORD #1 AND #2 INPUT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(DF3.EQ.1) W3=0.5461D0
          IF(W1.EQ.0.0D0) THEN
              WRITE(OUTLYNE,*)'NUMERIC WORD #1 MUST BE NON-ZERO'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(W2.EQ.0.0D0) THEN
              WRITE(OUTLYNE,*)'NUMERIC WORD #2 MUST BE NON-ZERO'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(W3.EQ.0.0D0) THEN
              WRITE(OUTLYNE,*)'NUMERIC WORD #3 MUST BE NON-ZERO'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SYSTEM1(6).EQ.1.0) WAVE=(W3*1.0D-3)/(25.4D0)
          IF(SYSTEM1(6).EQ.2.0) WAVE=W3*1.0D-4
          IF(SYSTEM1(6).EQ.3.0) WAVE=W3*1.0D-3
          IF(SYSTEM1(6).EQ.4.0) WAVE=W3*1.0D-6
          CURVE=1.0D0/DABS((W2**2)/(4.0D0*W1*WAVE))
          IF(WQ.EQ.'ACC') THEN
              REG(40)=REG(9)
              REG(9)=CURVE
              RETURN
          END IF
          IF(SQ.EQ.0) THEN
              WRITE(OUTLYNE,1000)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,2000)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,2010) W1
              CALL SHOWIT(0)
              WRITE(OUTLYNE,2001) W2,UN(2:12)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,2002) W3
              CALL SHOWIT(0)
              WRITE(OUTLYNE,2003) CURVE,UN
              CALL SHOWIT(0)
              RETURN
          END IF
 1000     FORMAT(1X)
 2000     FORMAT('    CURVATURE FOR M FRINGES OUT OF FLAT')
 2010     FORMAT('    NUMBER OF FRINGES OUT OF FLAT = ',G12.4)
 2001     FORMAT('                    PART DIAMETER = ',G15.6,1X,A11)
 2002     FORMAT
     1    ('                       WAVELENGTH = ',G12.4,' MICRONS')
 2003     FORMAT('   CORRESPONDING SURFCE CURVATURE = ',G15.6,1X,A12)
          RETURN
      END
C SUB SCHWARTZ.FOR
      SUBROUTINE SCHWARTZ
C       MAY THE SCHWARTZ BE WITH YOU
C
          IMPLICIT NONE
C
C       THIS SUBROUTINE HANDELS THE "RADUNITS", "WIEN", "PLANK"
C       AND "STEFBOLT" CMD LEVEL COMMANDS
C
C                       DEFINE VARIABLES
C
C       "MAY THE SCHWARTZ BE WITH YOU!"
C
C
          REAL*8 C1,C1P,C2,VAL
     1    ,VAL1,VAL2,SIG,SIGP,RETVAL
C
          EXTERNAL RETVAL
C
          INCLUDE 'datmai.inc'
C
          C1=37415.0D0
          C1P=1.88365D23
          C2=14387.9D0
          SIG=5.6697D-12
          SIGP=1.52041D+11
C
C     WC IS RADUNITS
          IF(WC.EQ.'RADUNITS') THEN
              IF(STI.EQ.1) THEN
                  IF(RADUNI.EQ.1) WRITE(OUTLYNE,10)
                  IF(RADUNI.EQ.2) WRITE(OUTLYNE,20)
                  CALL SHOWIT(0)
                  RETURN
              END IF
              IF(SQ.EQ.1.AND.WQ.NE.'WATTS'.AND.WQ.NE.'PHOTONS') THEN
                  WRITE(OUTLYNE,*)'INVALID QUALIFIER WORD USED WITH "RADUNITS"'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(SQ.EQ.0..OR.SST.EQ.1) THEN
                  IF(RADUNI.EQ.1) WRITE(OUTLYNE,10)
                  IF(RADUNI.EQ.2) WRITE(OUTLYNE,20)
                  CALL SHOWIT(0)
 10               FORMAT('"RADUNITS" IS SET TO "WATTS"')
 20               FORMAT('"RADUNITS" IS SET TO "PHOTONS"')
                  RETURN
              END IF
              IF(WQ.EQ.'WATTS') RADUNI=1
              IF(WQ.EQ.'PHOTONS') RADUNI=2
          END IF
C
C
C     WC IS WIEN
          IF(WC.EQ.'WIEN') THEN
              IF(SQ.EQ.1.AND.WQ.NE.'P') THEN
                  WRITE(OUTLYNE,*)'INVALID QUALIFIER WORD USED WITH "WIEN"'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(SST.EQ.1) THEN
                  WRITE(OUTLYNE,*) 'NO ADDITIONAL INFORMATION AVAILABLE'
                  CALL SHOWIT(1)
                  RETURN
              END IF
              IF(DF1.EQ.1) THEN
                  WRITE(OUTLYNE,*) '"WIEN" REQUIRES EXPLICIT NUMERIC WORD #1 INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*) 'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(W1.LE.0.0D0) THEN
                  WRITE(OUTLYNE,*) '"WIEN" REQUIRES (T) GREATER THAN 0'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*) 'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(RADUNI.EQ.1) THEN
                  VAL=2897.8D0/W1
              END IF
              IF(RADUNI.EQ.2) THEN
                  VAL=3669.73D0/W1
              END IF
              IF(WQ.EQ.'P') THEN
                  IF(RADUNI.EQ.1) WRITE(OUTLYNE,30) VAL
 30               FORMAT('WAVELENGTH FOR PEAK RADIANT EMITTANCE = ',
     1            G23.15,' MICRON(S)')
                  IF(RADUNI.EQ.2) WRITE(OUTLYNE,40) VAL
 40               FORMAT('WAVELENGTH FOR PEAK RADIANT PHOTON EMITTANCE = ',
     1            G23.15,' MICRON(S)')
                  CALL SHOWIT(0)
              END IF
              REG(40)=REG(9)
              REG(9)=VAL
          END IF
C
C
C     WC IS PLANK
          IF(WC.EQ.'PLANK') THEN
              IF(SQ.EQ.1.AND.WQ.NE.'P') THEN
                  WRITE(OUTLYNE,*)'INVALID QUALIFIER WORD USED WITH "PLANK"'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(SST.EQ.1) THEN
                  WRITE(OUTLYNE,*) 'NO ADDITIONAL INFORMATION AVAILABLE'
                  CALL SHOWIT(1)
                  RETURN
              END IF
              IF(DF1.EQ.1.OR.DF2.EQ.1) THEN
                  WRITE(OUTLYNE,*)
     1            '"PLANK" REQUIRES EXPLICIT NUMERIC WORD #1 AND #2 INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*) 'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(W1.LE.0.0D0) THEN
                  WRITE(OUTLYNE,*) '"PLANK" REQUIRES (T) GREATER THAN 0'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*) 'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(W2.LE.0.0D0) THEN
                  WRITE(OUTLYNE,*) '"PLANK" REQUIRES (LAMBDA) GREATER THAN 0'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*) 'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(RADUNI.EQ.1) THEN
                  VAL=(C1/(W2**5))/(DEXP(C2/(W1*W2))-1.0D0)
              END IF
              IF(RADUNI.EQ.2) THEN
                  VAL=(C1P/(W2**4))/(DEXP(C2/(W1*W2))-1.0D0)
              END IF
              IF(WQ.EQ.'P') THEN
                  IF(RADUNI.EQ.1) WRITE(OUTLYNE,35) VAL
 35               FORMAT('RADIANT EMITTANCE = ',G23.15,' WATTS/CM2-MICRON')

                  IF(RADUNI.EQ.2) WRITE(OUTLYNE,45) VAL
 45               FORMAT('RADIANT PHOTON EMITTANCE = ',G23.15,
     1            ' PHOTONS/SEC-CM2-MICRON')
                  CALL SHOWIT(0)
              END IF
              REG(40)=REG(9)
              REG(9)=VAL
          END IF
C
C
C     WC IS STEFBOLT
          IF(WC.EQ.'STEFBOLT') THEN
              IF(SQ.EQ.1.AND.WQ.NE.'P') THEN
                  WRITE(OUTLYNE,*)'INVALID QUALIFIER WORD USED WITH "STEFBOLT"'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(SST.EQ.1) THEN
                  WRITE(OUTLYNE,*) 'NO ADDITIONAL INFORMATION AVAILABLE'
                  CALL SHOWIT(1)
                  RETURN
              END IF
              IF(DF1.EQ.1) THEN
                  WRITE(OUTLYNE,*)
     1            '"STEFBOLT" REQUIRES EXPLICIT NUMERIC WORD #1'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*) 'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(W1.LE.0.0D0) THEN
                  WRITE(OUTLYNE,*) '"STEFBOLT" REQUIRES (T) GREATER THAN 0'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*) 'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(DF2.EQ.1) W2=1.0D20
              IF(DF3.EQ.1) W3=0.0D0
              IF(W2.LE.0.0D0) THEN
                  WRITE(OUTLYNE,*)
     1            '"STEFBOLT" REQUIRES (LAMBDA UPPER) GREATER THAN 0'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*) 'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(W3.LT.0.0D0) THEN
                  WRITE(OUTLYNE,*)
     1            '"STEFBOLT" REQUIRES (LAMBDA LOWER) GREATER THAN OR EQUAL TO 0'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*) 'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(W3.GE.W2) THEN
                  WRITE(OUTLYNE,*) '"STEFBOLT" REQUIRES (LAMBDA UPPER)'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*) 'GREATER THAN (LAMBDA LOWER)'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*) 'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(RADUNI.EQ.1) THEN
C     CASE OF TOTAL
                  IF(W2.EQ.1.0D20.AND.W3.EQ.0.0D0) THEN
                      VAL=SIG*(W1**4)
                      GO TO 200
                  END IF
C     CASE OF ONLY LAMBDA UPPER
                  IF(W2.LT.1.0D20.AND.W3.EQ.0.0D0) THEN
                      VAL=RETVAL(W1,W2,RADUNI)
                      GO TO 200
                  END IF
C     CASE OF RANGE
                  IF(W2.LT.1.0D20.AND.W3.GT.0.0D0) THEN
                      VAL1=RETVAL(W1,W2,RADUNI)
                      VAL2=RETVAL(W1,W3,RADUNI)
                      VAL=VAL1-VAL2
                      GO TO 200
                  END IF
              END IF
              IF(RADUNI.EQ.2) THEN
C     CASE OF TOTAL
                  IF(W2.EQ.1.0D20.AND.W3.EQ.0.0D0) THEN
                      VAL=SIGP*(W1**3)
                      GO TO 200
                  END IF
C     CASE OF ONLY LAMBDA UPPER
                  IF(W2.LT.1.0D20.AND.W3.EQ.0.0D0) THEN
                      VAL=RETVAL(W1,W2,RADUNI)
                      GO TO 200
                  END IF
C     CASE OF RANGE
                  IF(W2.LT.1.0D20.AND.W3.GT.0.0D0) THEN
                      VAL1=RETVAL(W1,W2,RADUNI)
                      VAL2=RETVAL(W1,W3,RADUNI)
                      VAL=VAL1-VAL2
                      GO TO 200
                  END IF
              END IF
 200          CONTINUE
              IF(WQ.EQ.'P') THEN
C     HEADING
                  IF(W2.EQ.1.0D20.AND.W3.EQ.0.0D0) WRITE(OUTLYNE,150)
                  IF(W2.EQ.1.0D20.AND.W3.EQ.0.0D0) CALL SHOWIT(0)
                  IF(W2.LT.1.0D20.AND.W3.EQ.0.0D0) WRITE(OUTLYNE,151)W2
                  IF(W2.LT.1.0D20.AND.W3.EQ.0.0D0) CALL SHOWIT(0)
                  IF(W2.LT.1.0D20.AND.W3.GT.0.0D0) WRITE(OUTLYNE,152)W2,W3
                  IF(W2.LT.1.0D20.AND.W3.GT.0.0D0) CALL SHOWIT(0)
 150              FORMAT('INTEGERATED OVER ALL WAVELENGTHS')
 151              FORMAT('BELOW ',G14.6,' MICRONS')
 152              FORMAT('IN THE ',G14.6,' TO ',G14.6,' MICRON BAND')
C
                  IF(RADUNI.EQ.1) WRITE(OUTLYNE,135) VAL
 135              FORMAT('TOTAL RADIANT EMITTANCE = '
     1            ,G23.15,' WATTS/CM2')

                  IF(RADUNI.EQ.2) WRITE(OUTLYNE,145) VAL
 145              FORMAT('TOTAL RADIANT PHOTON EMITTANCE = ',G23.15,
     1            ' PHOTONS/SEC-CM2')
                  CALL SHOWIT(0)
              END IF
              REG(40)=REG(9)
              REG(9)=VAL
          END IF
C
          RETURN
      END
      FUNCTION RETVAL(T,LAMBDA,RADUNI)
          IMPLICIT NONE
          REAL*8 T,LAMBDA,C1,C2,C1P,SUMOLD,RETVAL,SUM
     1    ,A
          INTEGER RADUNI,K
          C1=37415.0D0
          C1P=1.88365D23
          C2=14387.9D0
          IF(RADUNI.EQ.1) THEN
C     WATT UNITS
              SUM=0.0D0
              DO K=1,1000000
                  SUMOLD=SUM
                  A=DEXP((-C2*DBLE(K))/(LAMBDA*T))/DBLE(K)
                  SUM=SUM+
     1            (((1.0D0/LAMBDA**3)+
     2            ((3.0D0*T)/(DBLE(K)*C2*(LAMBDA**2)))+
     3            ((6.0D0*(T**2))/(DBLE((K**2))*(C2**2)*LAMBDA))+
     4            ((6.0D0*(T**3))/(DBLE((K**3))*(C2**3))))*A)
                  IF(DABS(SUM-SUMOLD).LE.1.0D-10) GO TO 999
              END DO
 999          RETVAL=((T*C1)/C2)*SUM
              RETURN
          END IF
          IF(RADUNI.EQ.2) THEN
C     PHOTON UNITS
              SUM=0.0D0
              DO K=1,1000000
                  SUMOLD=SUM
                  A=DEXP((-C2*DBLE(K))/(LAMBDA*T))/DBLE(K)
                  SUM=SUM+
     1            (((1.0D0/(LAMBDA**2))+
     2            ((2.0D0*T)/(DBLE(K)*C2*LAMBDA))+
     3            ((2.0D0*(T**2))/(DBLE((K**2))*(C2**2))))*A)
                  IF(DABS(SUM-SUMOLD).LE.1.0D-10) GO TO 888
              END DO
 888          RETVAL=((T*C1P)/C2)*SUM
              RETURN
          END IF
          RETURN
      END
C
C     THIS ROUTINE DOES CROSS TRACK SPECTRAL
C
      SUBROUTINE SCE(VALVAL,TYPE,V1,ERROR)
          IMPLICIT NONE
          INTEGER ERROR,TYPE,I,J,K,JJ
          REAL*8 VALVAL,VALVAL1(1:3),V1,YV1,YV2,YV3
     1    ,MINMIN,JKV,VV1,VAL(1:10),MAXMAX
          LOGICAL OLDLDIF,OLDLDIF2
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
C
C     IF TYPE=1 XFOB
C     IF TYPE=2 YFOB
          YV1=0.0D0
          YV2=0.0D0
          YV3=0.0D0
C     FOCAL AND UFOCAL ONLY
          IF(SYSTEM1(30).GE.3.0D0) THEN
              ERROR=1
              RETURN
          END IF
C
C     WAVELENGTHS ARE OK
C
          IF(TYPE.EQ.1) THEN
              VALVAL1(1:3)=0.0D0
              DO K=1,3
                  IF(K.EQ.1) JKV=1.0
                  IF(K.EQ.2) JKV=0.0
                  IF(K.EQ.3) JKV=-1.0
                  J=0
                  VAL(1:10)=0.0D0
                  DO 10 I=1,10
                      VV1=DBLE(I)
C     CHECK WAVELENGTH
                      IF(VV1.EQ.1.0D0.AND.SYSTEM1(1).EQ.0.0D0.OR.
     1                VV1.EQ.2.0D0.AND.SYSTEM1(2).EQ.0.0D0.OR.
     1                VV1.EQ.3.0D0.AND.SYSTEM1(3).EQ.0.0D0.OR.
     1                VV1.EQ.4.0D0.AND.SYSTEM1(4).EQ.0.0D0.OR.
     1                VV1.EQ.5.0D0.AND.SYSTEM1(5).EQ.0.0D0.OR.
     1                VV1.EQ.6.0D0.AND.SYSTEM1(71).EQ.0.0D0.OR.
     1                VV1.EQ.7.0D0.AND.SYSTEM1(72).EQ.0.0D0.OR.
     1                VV1.EQ.8.0D0.AND.SYSTEM1(73).EQ.0.0D0.OR.
     1                VV1.EQ.9.0D0.AND.SYSTEM1(74).EQ.0.0D0.OR.
     1                VV1.EQ.10.0D0.AND.SYSTEM1(75).EQ.0.0D0) GO TO 10
C     Y COMPONENTS OF XFOB AT DESIGNATED WAVELENGTH
                      J=J+1
C
C     DO THE FOB 0 JKV 0 VV1
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
                      W2=JKV
                      W3=0.0D0
                      W4=VV1
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
                      WC='FOB     '
                      CALL FFOB
                      IF(.NOT.REFEXT) THEN
                          ERROR=1
                          RETURN
                      END IF
                      REST_KDP(1)=RESTINPT(1)
C NOW THE RAY 0 0
                      SAVE_KDP(1)=SAVEINPT(1)
                      WQ='        '
                      SQ=0
                      SST=0
                      DF1=1
                      DF2=1
                      DF3=0
                      DF4=1
                      DF5=1
                      S1=0
                      S2=0
                      S3=1
                      S4=0
                      S5=0
                      SN=0
                      W1=0.0D0
                      W2=0.0D0
                      W3=VV1
                      W4=0.0D0
                      W5=0.0D0
                      WC='RAY     '
                      NOCOAT=.TRUE.
                      GRASET=.FALSE.
                      DXFSET=.FALSE.
                      CALL RRAY
                      IF(.NOT.RAYEXT) THEN
                          ERROR=1
                          RETURN
                      END IF
                      REST_KDP(1)=RESTINPT(1)
C     WRITE DOWN THE FIRST Y VALUE
                      VAL(J)=RAYRAY(1,NEWIMG)
 10               CONTINUE
                  MAXMAX=-1.0D300
                  MINMIN=1.0D300
                  DO JJ=1,J
                      IF(VAL(JJ).GT.MAXMAX) MAXMAX=VAL(JJ)
                      IF(VAL(JJ).LT.MINMIN) MINMIN=VAL(JJ)
                  END DO
                  VALVAL1(K)=DABS(MAXMAX-MINMIN)
              END DO
              MAXMAX=-1.0D300
              DO K=1,3
                  IF(VALVAL1(K).GT.MAXMAX) MAXMAX=VALVAL1(K)
              END DO
              VALVAL=MAXMAX
              IF(V1.NE.0.0D0) VALVAL=(VALVAL/V1)*100.0D0
              ERROR=0
              RETURN
          END IF
          IF(TYPE.EQ.2) THEN
              VALVAL1(1:3)=0.0D0
              DO K=1,3
                  IF(K.EQ.1) JKV=1.0
                  IF(K.EQ.2) JKV=0.0
                  IF(K.EQ.3) JKV=-1.0
                  J=0
                  VAL(1:10)=0.0D0
                  DO 20 I=1,10
                      VV1=DBLE(I)
C     CHECK WAVELENGTH
                      IF(VV1.EQ.1.0D0.AND.SYSTEM1(1).EQ.0.0D0.OR.
     1                VV1.EQ.2.0D0.AND.SYSTEM1(2).EQ.0.0D0.OR.
     1                VV1.EQ.3.0D0.AND.SYSTEM1(3).EQ.0.0D0.OR.
     1                VV1.EQ.4.0D0.AND.SYSTEM1(4).EQ.0.0D0.OR.
     1                VV1.EQ.5.0D0.AND.SYSTEM1(5).EQ.0.0D0.OR.
     1                VV1.EQ.6.0D0.AND.SYSTEM1(71).EQ.0.0D0.OR.
     1                VV1.EQ.7.0D0.AND.SYSTEM1(72).EQ.0.0D0.OR.
     1                VV1.EQ.8.0D0.AND.SYSTEM1(73).EQ.0.0D0.OR.
     1                VV1.EQ.9.0D0.AND.SYSTEM1(74).EQ.0.0D0.OR.
     1                VV1.EQ.10.0D0.AND.SYSTEM1(75).EQ.0.0D0) GO TO 20
C     Y COMPONENTS OF XFOB AT DESIGNATED WAVELENGTH
                      J=J+1
C
C     DO THE FOB 0 JKV 0 VV1
                      SAVE_KDP(1)=SAVEINPT(1)
                      OLDLDIF2=LDIF2
                      OLDLDIF=LDIF
                      LDIF2=.FALSE.
                      LDIF=.FALSE.
                      WQ='        '
                      SQ=0
                      SST=0
                      STI=0
                      W1=JKV
                      W2=0.0D0
                      W3=0.0D0
                      W4=VV1
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
                      WC='FOB     '
                      CALL FFOB
                      IF(.NOT.REFEXT) THEN
                          ERROR=1
                          RETURN
                      END IF
                      REST_KDP(1)=RESTINPT(1)
C NOW THE RAY 0 0
                      SAVE_KDP(1)=SAVEINPT(1)
                      WQ='        '
                      SQ=0
                      SST=0
                      DF1=1
                      DF2=1
                      DF3=0
                      DF4=1
                      DF5=1
                      S1=0
                      S2=0
                      S3=1
                      S4=0
                      S5=0
                      SN=0
                      W1=0.0D0
                      W2=0.0D0
                      W3=VV1
                      W4=0.0D0
                      W5=0.0D0
                      WC='RAY     '
                      NOCOAT=.TRUE.
                      GRASET=.FALSE.
                      DXFSET=.FALSE.
                      CALL RRAY
                      IF(.NOT.RAYEXT) THEN
                          ERROR=1
                          RETURN
                      END IF
                      REST_KDP(1)=RESTINPT(1)
C     WRITE DOWN THE FIRST Y VALUE
                      VAL(J)=RAYRAY(2,NEWIMG)
 20               CONTINUE
                  MAXMAX=-1.0D300
                  MINMIN=1.0D300
                  VAL(1:10)=DABS(VAL(1:10))
                  DO JJ=1,J
                      IF(VAL(JJ).GT.MAXMAX) MAXMAX=VAL(JJ)
                      IF(VAL(JJ).LT.MINMIN) MINMIN=VAL(JJ)
                  END DO
                  VALVAL1(K)=DABS(MAXMAX-MINMIN)
              END DO
              MAXMAX=-1.0D300
              DO K=1,3
                  IF(VALVAL1(K).GT.MAXMAX) MAXMAX=VALVAL1(K)
              END DO
              VALVAL=MAXMAX
              IF(V1.NE.0.0D0) VALVAL=(VALVAL/V1)*100.0D0
              ERROR=0
              RETURN
          END IF
      END
C SUB OIF.FOR
      SUBROUTINE OIF
C
          IMPLICIT NONE
C
C
C     OIF IS A SUBROUTINE WHICH CALCULATES OBJECT OR IMAGE
C     DISTACE OR FOCAL LENGTH GIVEN ANY TWO OF THE THREE VALUES
C     USING:
C
C     1/F=1/0+1/I
C
C       OIF, NW1 NW2 NW2
C       WHERE NW1 IS THE IMAGE DISTANCE
C       WHERE NW2 IS THE OBJECT DISTANCE
C       WHERE NW3 IS THE FOCAL LENGTH
C
          REAL*8 VALUE
C
          INCLUDE 'datmai.inc'
C
          IF(SST.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
              WRITE(OUTLYNE,*)
     1        '"OIF (P)" ONLY TAKES NUMERIC WORD #1, #2 AND #3 INPUT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
          IF(SQ.EQ.1.AND.WQ.NE.'P') THEN
              WRITE(OUTLYNE,*)
     1        '"P" IS THE ONLY OPTIONAL QUALIFIER WORD FOR "OIF"'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(DF1.EQ.0.AND.W1.EQ.0.0D0.OR.DF2.EQ.0.AND.W2.EQ.0.0D0
     1    .OR.DF3.EQ.0.AND.W3.EQ.0.0D0) THEN
              WRITE(OUTLYNE,*)
     1        '"OIF (P)" DOES NOT ACCEPT 0.0 FOR ANY OF ITS INPUT VALUES'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
          IF(DF1.EQ.1.AND.DF2.EQ.0.AND.DF3.EQ.0.OR.
     1       DF1.EQ.0.AND.DF2.EQ.1.AND.DF3.EQ.0.OR.
     2       DF1.EQ.0.AND.DF2.EQ.0.AND.DF3.EQ.1) THEN
C     PROCEED

              IF(DF1.EQ.1) VALUE=(1.0D0/W3)-(1.0D0/W2)
              IF(DF2.EQ.1) VALUE=(1.0D0/W3)-(1.0D0/W1)
              IF(DF3.EQ.1) VALUE=(1.0D0/W1)+(1.0D0/W2)
C       UPDATE LASTX
              REG(40)=REG(9)
              REG(9) = 1.0D0/VALUE
C
              IF(WQ.EQ.'P') THEN
                  IF(DF1.EQ.1) WRITE(OUTLYNE,100) (1.0D0/VALUE)
                  IF(DF1.EQ.1) CALL SHOWIT(0)
                  IF(DF1.EQ.1) WRITE(OUTLYNE,20) W2
                  IF(DF1.EQ.1) CALL SHOWIT(0)
                  IF(DF1.EQ.1) WRITE(OUTLYNE,30) W3
                  IF(DF1.EQ.1) CALL SHOWIT(0)
                  IF(DF2.EQ.1) WRITE(OUTLYNE,200) (1.0D0/VALUE)
                  IF(DF2.EQ.1) CALL SHOWIT(0)
                  IF(DF2.EQ.1) WRITE(OUTLYNE,10) W1
                  IF(DF2.EQ.1) CALL SHOWIT(0)
                  IF(DF2.EQ.1) WRITE(OUTLYNE,30) W3
                  IF(DF2.EQ.1) CALL SHOWIT(0)
                  IF(DF3.EQ.1) WRITE(OUTLYNE,300) (1.0D0/VALUE)
                  IF(DF3.EQ.1) CALL SHOWIT(0)
                  IF(DF3.EQ.1) WRITE(OUTLYNE,10) W1
                  IF(DF3.EQ.1) CALL SHOWIT(0)
                  IF(DF3.EQ.1) WRITE(OUTLYNE,20) W2
                  IF(DF3.EQ.1) CALL SHOWIT(0)
 10               FORMAT(' INPUT OBJECT DISTANCE WAS = ',G13.6)
 20               FORMAT('  INPUT IMAGE DISTANCE WAS = ',G13.6)
 30               FORMAT('    INPUT FOCAL LENGTH WAS = ',G13.6)
 100              FORMAT('THE NEW OBJECT DISTANCE IS = ',G13.6)
 200              FORMAT(' THE NEW IMAGE DISTANCE IS = ',G13.6)
 300              FORMAT('   THE NEW FOCAL LENGTH IS = ',G13.6)
              END IF
              RETURN
          ELSE
              WRITE(OUTLYNE,*)'"OIF (P)" REQUIRES TWO EXPLICIT NUMERIC VALUES'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
          END IF
          RETURN
      END
C SUB RAYLEIGH.FOR
      SUBROUTINE RAYLEIGH
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE RAYLEIGH. THIS SUBROUTINE IMPLEMENTS THE
C       RAYLEIGH RANGE CALCULATION
C       NW1=OPTIONAL BEAM WAIST DIAMETER AT 1/E2 POINT
C       NW2=OPTIONAL WAVELENGTH
C         NW3, NW4 AND NW5 NOT USED
C         WQ IS EITHER BLANK OR 'ACC'
C         WS NOT USED
C
!      INTEGER I
C
          REAL*8 W0,B,LAMBDA,CFAC,LAM0
C
          CHARACTER*8 UN

C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
C
          IF(SYSTEM1(6).EQ.1.0) CFAC=1.0D-3/25.4D0
          IF(SYSTEM1(6).EQ.2.0) CFAC=1.0D-4
          IF(SYSTEM1(6).EQ.3.0) CFAC=1.0D-3
          IF(SYSTEM1(6).EQ.4.0) CFAC=1.0D-6
          IF(SYSTEM1(6).EQ.1.0) UN='INCH(ES)'
          IF(SYSTEM1(6).EQ.2.0) UN='CM(S)'
          IF(SYSTEM1(6).EQ.3.0) UN='MM(S)'
          IF(SYSTEM1(6).EQ.4.0) UN='METER(S)'
C
          IF(SST.EQ.1) THEN
              WRITE(OUTLYNE,*)'"RAYLEIGH" TAKES NO ALPHANUMERIC STRING INPUT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
              WRITE(OUTLYNE,*)
     1        '"RAYLEIGH" TAKES NO NUMERIC WORD #3, #4 OR #5 INPUT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SQ.EQ.1.AND.WQ.NE.'ACC') THEN
              WRITE(OUTLYNE,*)'INVALID QUALIFIER WORD USED WITH "RAYLEIGH"'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
          IF(DF1.EQ.1) THEN
              W0=2.0D0*SYSTEM1(86)
          ELSE
              W0=W1
          END IF
          IF(DF2.EQ.1) THEN
              IF(SYSTEM1(11).LE.5.0D0) THEN
                  LAMBDA=SYSTEM1(INT(SYSTEM1(11)))
              ELSE
                  LAMBDA=SYSTEM1(65+INT(SYSTEM1(11)))
              END IF
          ELSE
              LAMBDA=W2
          END IF
          LAM0=LAMBDA
          LAMBDA=LAMBDA*CFAC
          IF(W0.LE.0.0D0) THEN
              WRITE(OUTLYNE,*)
     1        'WAIST DIAMETER MUST BE GREATER THAN 0.0 WITH "RAYLEIGH"'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(LAMBDA.LE.0.0D0) THEN
              WRITE(OUTLYNE,*)
     1        'WAVELENGTH MUST BE GREATER THAN 0.0 WITH "RAYLEIGH"'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
          B=(TWOPII*(W0**2))/LAMBDA
          IF(WQ.EQ.'ACC') THEN
              REG(40)=REG(9)
              REG(9)=B
              RETURN
          END IF
          IF(SQ.EQ.0) THEN
              REG(40)=REG(9)
              REG(9)=B
              WRITE(OUTLYNE,1000)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,2000)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,2010) W0
              CALL SHOWIT(0)
              WRITE(OUTLYNE,2020) LAM0
              CALL SHOWIT(0)
              WRITE(OUTLYNE,2002) B,UN
              CALL SHOWIT(0)
              RETURN
          END IF
 1000     FORMAT(1X)
 2000     FORMAT('RAYLEIGH RANGE CALCULATION')
 2010     FORMAT('BEAM WAIST DIAMETER = ',G15.6,1X,A8)
 2020     FORMAT('         WAVELENGTH = ',G15.6,1X,'MICRON(S)')
 2002     FORMAT('     RAYLEIGH RANGE = ',G15.6,1X,A8)
          RETURN
      END
C SUB COST.FOR
      SUBROUTINE COST
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE COST. THIS SUBROUTINE IMPLEMENTS THE
C       COST CALCULATION
C       NW1=STARTING SURFACE NUMBER
C       NW2=ENDING SURFACE NUMBER
C         NW3, NW4 AND NW5 NOT USED
C         WQ IS EITHER BLANK OR 'ACC'
C         WS NOT USED
C
          INTEGER I
C
          REAL*8 CFAC,WEIGT,RHO2,COR1,COR2,SAG1
     1    ,C2,RD,C,SIGNIT1,RHOA,RHOB,H,PLUG,TOTALW,MYWEIGHT,SAG2,
     2    SIGNIT2,THMFACTOR
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
C
          IF(SYSTEM1(6).EQ.1.0) CFAC=(2.54D0)**3
          IF(SYSTEM1(6).EQ.2.0) CFAC=1.0D0
          IF(SYSTEM1(6).EQ.3.0) CFAC=0.1D0**3
          IF(SYSTEM1(6).EQ.4.0) CFAC=100.D0**3
C
C
          IF(SST.EQ.1) THEN
              WRITE(OUTLYNE,*)'"COST" TAKES NO ALPHANUMERIC STRING INPUT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
              WRITE(OUTLYNE,*)
     1        '"COST" TAKES NO NUMERIC WORD #3, #4 OR #5 INPUT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SQ.EQ.1.AND.WQ.NE.'ACC') THEN
              WRITE(OUTLYNE,*)'INVALID QUALIFIER WORD USED WITH "COST"'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
          IF(DF1.EQ.1) THEN
              WRITE(OUTLYNE,*)
     1        '"COST" REQUIRES EXPLICIT NUMERIC WORD #1 INPUT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(W1.LT.0.0D0.OR.W1.GT.SYSTEM1(20)) THEN
              WRITE(OUTLYNE,*)
     1        'NUMERIC WORD #1, SURFACE NUMBER BEYOND LEGAL BOUNDS'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(S2.EQ.1) THEN
              IF(W2.LT.0.0D0.OR.W2.GT.SYSTEM1(20)) THEN
                  WRITE(OUTLYNE,*)
     1            'NUMERIC WORD #2, SURFACE NUMBER BEYOND LEGAL BOUNDS'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(W2.LE.W1) THEN
                  WRITE(OUTLYNE,*)
     1            'SECOND SURFACE MUST FOLLOW FIRST SURFACE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
          IF(DF2.EQ.1) W2=W1+1
C
C     DO COST CALC HERE
          TOTALW=0.0D0
          IF((INT(W2)).GT.INT(W1)) THEN
              DO I=INT(W1),INT(W2)-1
                  IF(ALENS(102,I).NE.0.0D0) THEN
C     ADJUSTMENT FOR R1
                      IF(ALENS(9,I).EQ.1.0D0.AND.ALENS(127,I).EQ.0.0D0) THEN
                          RHOA=DABS(ALENS(10,I))
                      ELSE
                          RHOA=DABS(PXTRAY(1,I))+DABS(PXTRAY(5,I))
                      END IF
                      IF(ALENS(1,I).EQ.0.0D0) THEN
                          COR1=0.0D0
                      ELSE
                          RD=DABS(1.0D0/ALENS(1,I))
                          C=ALENS(1,I)
                          C2=ALENS(1,I)**2
                          RHO2=RHOA**2
                          SAG1=((C*RHO2)/
     1                    (1.0D0+(DSQRT(1.0D0-(C2*RHO2)))))
                          IF(SAG1.GE.0.0D0) SIGNIT1=1.0D0
                          IF(SAG1.LT.0.0D0) SIGNIT1=-1.0D0
                          SAG1=DABS(SAG1)
                          COR1=(1.0D0/3.0D0)*PII*(SAG1**2)*((3.0D0*RD)-SAG1)
                          COR1=DABS(COR1)
                          IF(SIGNIT1.LT.0.0D0) THEN
                              COR1=(PII*RHO2*SAG1)-COR1
                          END IF
                      END IF
C     ADJUSTMENT FOR R2
                      IF(ALENS(9,I+1).EQ.1.0D0.AND.ALENS(127,I+1).EQ.0.0D0) THEN
                          RHOB=DABS(ALENS(10,I+1))
                      ELSE
                          RHOB=DABS(PXTRAY(1,I+1))+DABS(PXTRAY(5,I+1))
                      END IF
                      IF(ALENS(1,I+1).EQ.0.0D0) THEN
                          COR2=0.0D0
                      ELSE
                          RD=DABS(1.0D0/ALENS(1,I+1))
                          C=ALENS(1,I+1)
                          C2=ALENS(1,I+1)**2
                          RHO2=RHOB**2
                          SAG2=((C*RHO2)/
     1                    (1.0D0+(DSQRT(1.0D0-(C2*RHO2)))))
                          IF(SAG2.GE.0.0D0) SIGNIT2=1.0D0
                          IF(SAG2.LT.0.0D0) SIGNIT2=-1.0D0
                          SAG2=DABS(SAG2)
                          COR2=(1.0D0/3.0D0)*PII*(SAG2**2)*((3.0D0*RD)-SAG2)
                          COR2=DABS(COR2)
                          IF(SIGNIT2.GT.0.0D0) THEN
                              COR2=(PII*RHO2*SAG2)-COR2
                          END IF
                      END IF
C     WEIGHT OF PLUG
                      H=DABS(ALENS(3,I))
                      IF(SIGNIT1.GT.0.0D0) H=H-SAG1
                      IF(SIGNIT2.LT.0.0D0) H=H-SAG2
                      PLUG=(1.0D0/3.0D0)*PII*H*((RHOA**2)+(RHOA*RHOB)+(RHOB**2))
                      MYWEIGHT=(((PLUG+COR1+COR2)*CFAC)*ALENS(102,I))
                      IF(MYWEIGHT.LT.0.0D0) MYWEIGHT=0.0D0
                      TOTALW=TOTALW+MYWEIGHT
                      MYWEIGHT=0.0D0
                  END IF
              END DO
          END IF
          DO I=INT(W1),INT(W2)
              IF(ALENS(110,I).NE.0.0D0) THEN
C     MIRROR THICKNESS
C     ADJUSTMENT FOR R1
                  IF(ALENS(9,I).EQ.1.0D0.AND.ALENS(127,I).EQ.0.0D0) THEN
                      RHOA=DABS(ALENS(10,I))
                  ELSE
                      RHOA=DABS(PXTRAY(1,I))+DABS(PXTRAY(5,I))
                  END IF
                  IF(ALENS(1,I).EQ.0.0D0) THEN
                      COR1=0.0D0
                  ELSE
                      RD=DABS(1.0D0/ALENS(1,I))
                      C=ALENS(1,I)
                      C2=ALENS(1,I)**2
                      RHO2=RHOA**2
                      SAG1=((C*RHO2)/
     1                (1.0D0+(DSQRT(1.0D0-(C2*RHO2)))))
                      IF(SAG1.GE.0.0D0) SIGNIT1=1.0D0
                      IF(SAG1.LT.0.0D0) SIGNIT1=-1.0D0
                      SAG1=DABS(SAG1)
                      COR1=(1.0D0/3.0D0)*PII*(SAG1**2)*((3.0D0*RD)-SAG1)
                      COR1=DABS(COR1)
                      IF(SIGNIT1.LT.0.0D0) THEN
                          COR1=(PII*RHO2*SAG1)-COR1
                      END IF
                  END IF
C     ADJUSTMENT FOR BACK OF MIRROR
                  IF(ALENS(9,I).EQ.1.0D0.AND.ALENS(127,I).EQ.0.0D0) THEN
                      RHOB=DABS(ALENS(10,I))
                  ELSE
                      RHOB=DABS(PXTRAY(1,I))+DABS(PXTRAY(5,I))
                  END IF
                  IF(ALENS(1,I).EQ.0.0D0) THEN
                      COR2=0.0D0
                  ELSE
                      IF(ALENS(46,I).LT.0.0D0) THEN
                          RD=(1.0D0/ALENS(1,I))-DABS(ALENS(110,I))
                      ELSE
                          RD=(1.0D0/ALENS(1,I))+DABS(ALENS(110,I))
                      END IF
                      C=1.0D0/RD
                      C2=C**2
                      RHO2=RHOB**2
                      SAG2=((C*RHO2)/
     1                (1.0D0+(DSQRT(1.0D0-(C2*RHO2)))))
                      IF(SAG2.GE.0.0D0) SIGNIT2=1.0D0
                      IF(SAG2.LT.0.0D0) SIGNIT2=-1.0D0
                      SAG2=DABS(SAG2)
                      COR2=(1.0D0/3.0D0)*PII*(SAG2**2)*((3.0D0*RD)-SAG2)
                      COR2=DABS(COR2)
                      IF(SIGNIT2.GT.0.0D0) THEN
                          COR2=(PII*RHO2*SAG2)-COR2
                      END IF
                  END IF
C     WEIGHT OF PLUG
                  H=DABS(ALENS(110,I))
                  IF(SIGNIT1.GT.0.0D0) H=H-SAG1
                  IF(SIGNIT2.LT.0.0D0) H=H-SAG2
                  PLUG=(1.0D0/3.0D0)*PII*H*((RHOA**2)+(RHOA*RHOB)+(RHOB**2))
                  THMFACTOR=(((PLUG+COR1+COR2)*CFAC)*ALENS(102,I))
                  IF(THMFACTOR.LT.0.0D0) THMFACTOR=0.0D0
                  MYWEIGHT=MYWEIGHT+THMFACTOR
                  TOTALW=TOTALW+((MYWEIGHT/1000.0D0)*ALENS(111,I))
              END IF
          END DO
          MYWEIGHT=0.0D0
          THMFACTOR=0.0D0
          WEIGT=TOTALW
          TOTALW=0.0D0
C
          IF(WQ.EQ.'ACC') THEN
              REG(40)=REG(9)
              REG(9)=WEIGT
              RETURN
          END IF
          IF(SQ.EQ.0) THEN
              REG(40)=REG(9)
              REG(9)=WEIGT
              WRITE(OUTLYNE,1000)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,2000)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,2010) INT(W1)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,2020) INT(W2)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,2002) WEIGT
              CALL SHOWIT(0)
              RETURN
          END IF
 1000     FORMAT(1X)
 2000     FORMAT('    WEIGHT CALCULATION')
 2010     FORMAT('    FROM SURFACE # = ',I3)
 2020     FORMAT('      TO SURFACE # = ',I3)
 2002     FORMAT('              COST = ',G15.6,1X,'Cost Units')
          RETURN
      END
C SUB WEIGHT.FOR
      SUBROUTINE WEIGHT
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE WEIGHT. THIS SUBROUTINE IMPLEMENTS THE
C       WEIGHT CALCULATION
C       NW1=STARTING SURFACE NUMBER
C       NW2=ENDING SURFACE NUMBER
C         NW3, NW4 AND NW5 NOT USED
C         WQ IS EITHER BLANK OR 'ACC'
C         WS NOT USED
C
          INTEGER I
C
          REAL*8 CFAC,WEIGT,RHO2,COR1,COR2,SAG1
     1    ,C2,RD,C,SIGNIT1,RHOA,RHOB,H,PLUG,TOTALW,MYWEIGHT,SAG2,
     2    SIGNIT2,THMFACTOR
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
C
          IF(SYSTEM1(6).EQ.1.0) CFAC=(2.54D0)**3
          IF(SYSTEM1(6).EQ.2.0) CFAC=1.0D0
          IF(SYSTEM1(6).EQ.3.0) CFAC=0.1D0**3
          IF(SYSTEM1(6).EQ.4.0) CFAC=100.D0**3
C
C
          IF(SST.EQ.1) THEN
              WRITE(OUTLYNE,*)'"WEIGHT" TAKES NO ALPHANUMERIC STRING INPUT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
              WRITE(OUTLYNE,*)
     1        '"WEIGHT" TAKES NO NUMERIC WORD #3, #4 OR #5 INPUT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SQ.EQ.1.AND.WQ.NE.'ACC') THEN
              WRITE(OUTLYNE,*)'INVALID QUALIFIER WORD USED WITH "WEIGHT"'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
          IF(DF1.EQ.1) THEN
              WRITE(OUTLYNE,*)
     1        '"WEIGHT" REQUIRES EXPLICIT NUMERIC WORD #1 INPUT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(S2.EQ.1) THEN
              IF(W1.LT.0.0D0.OR.W1.GT.SYSTEM1(20)) THEN
                  WRITE(OUTLYNE,*)
     1            'NUMERIC WORD #1, SURFACE NUMBER BEYOND LEGAL BOUNDS'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(W2.LT.0.0D0.OR.W2.GT.SYSTEM1(20)) THEN
                  WRITE(OUTLYNE,*)
     1            'NUMERIC WORD #2, SURFACE NUMBER BEYOND LEGAL BOUNDS'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(W2.LE.W1) THEN
                  WRITE(OUTLYNE,*)
     1            'SECOND SURFACE MUST FOLLOW FIRST SURFACE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
          IF(DF2.EQ.1) W2=W1+1
C
C     DO WEIGHT CALC HERE
          TOTALW=0.0D0
          IF((INT(W2)).GT.INT(W1)) THEN
              DO I=INT(W1),INT(W2)-1
                  IF(ALENS(102,I).NE.0.0D0) THEN
C     ADJUSTMENT FOR R1
                      IF(ALENS(9,I).EQ.1.0D0.AND.ALENS(127,I).EQ.0.0D0) THEN
                          RHOA=DABS(ALENS(10,I))
                      ELSE
                          RHOA=DABS(PXTRAY(1,I))+DABS(PXTRAY(5,I))
                      END IF
                      IF(ALENS(1,I).EQ.0.0D0) THEN
                          COR1=0.0D0
                      ELSE
                          RD=DABS(1.0D0/ALENS(1,I))
                          C=ALENS(1,I)
                          C2=ALENS(1,I)**2
                          RHO2=RHOA**2
                          SAG1=((C*RHO2)/
     1                    (1.0D0+(DSQRT(1.0D0-(C2*RHO2)))))
                          IF(SAG1.GE.0.0D0) SIGNIT1=1.0D0
                          IF(SAG1.LT.0.0D0) SIGNIT1=-1.0D0
                          SAG1=DABS(SAG1)
                          COR1=(1.0D0/3.0D0)*PII*(SAG1**2)*((3.0D0*RD)-SAG1)
                          COR1=DABS(COR1)
                          IF(SIGNIT1.LT.0.0D0) THEN
                              COR1=(PII*RHO2*SAG1)-COR1
                          END IF
                      END IF
C     ADJUSTMENT FOR R2
                      IF(ALENS(9,I+1).EQ.1.0D0.AND.ALENS(127,I+1).EQ.0.0D0) THEN
                          RHOB=DABS(ALENS(10,I+1))
                      ELSE
                          RHOB=DABS(PXTRAY(1,I+1))+DABS(PXTRAY(5,I+1))
                      END IF
                      IF(ALENS(1,I+1).EQ.0.0D0) THEN
                          COR2=0.0D0
                      ELSE
                          RD=DABS(1.0D0/ALENS(1,I+1))
                          C=ALENS(1,I+1)
                          C2=ALENS(1,I+1)**2
                          RHO2=RHOB**2
                          SAG2=((C*RHO2)/
     1                    (1.0D0+(DSQRT(1.0D0-(C2*RHO2)))))
                          IF(SAG2.GE.0.0D0) SIGNIT2=1.0D0
                          IF(SAG2.LT.0.0D0) SIGNIT2=-1.0D0
                          SAG2=DABS(SAG2)
                          COR2=(1.0D0/3.0D0)*PII*(SAG2**2)*((3.0D0*RD)-SAG2)
                          COR2=DABS(COR2)
                          IF(SIGNIT2.GT.0.0D0) THEN
                              COR2=(PII*RHO2*SAG2)-COR2
                          END IF
                      END IF
C     WEIGHT OF PLUG
                      H=DABS(ALENS(3,I))
                      IF(SIGNIT1.GT.0.0D0) H=H-SAG1
                      IF(SIGNIT2.LT.0.0D0) H=H-SAG2
                      PLUG=(1.0D0/3.0D0)*PII*H*((RHOA**2)+(RHOA*RHOB)+(RHOB**2))
                      MYWEIGHT=(((PLUG+COR1+COR2)*CFAC)*ALENS(102,I))
                      IF(MYWEIGHT.LT.0.0D0) MYWEIGHT=0.0D0
                      TOTALW=TOTALW+MYWEIGHT
                  END IF
              END DO
          END IF
          DO I=INT(W1),INT(W2)
              IF(ALENS(110,I).NE.0.0D0) THEN
C     MIRROR THICKNESS
C     ADJUSTMENT FOR R1
                  IF(ALENS(9,I).EQ.1.0D0.AND.ALENS(127,I).EQ.0.0D0) THEN
                      RHOA=DABS(ALENS(10,I))
                  ELSE
                      RHOA=DABS(PXTRAY(1,I))+DABS(PXTRAY(5,I))
                  END IF
                  IF(ALENS(1,I).EQ.0.0D0) THEN
                      COR1=0.0D0
                  ELSE
                      RD=DABS(1.0D0/ALENS(1,I))
                      C=ALENS(1,I)
                      C2=ALENS(1,I)**2
                      RHO2=RHOA**2
                      SAG1=((C*RHO2)/
     1                (1.0D0+(DSQRT(1.0D0-(C2*RHO2)))))
                      IF(SAG1.GE.0.0D0) SIGNIT1=1.0D0
                      IF(SAG1.LT.0.0D0) SIGNIT1=-1.0D0
                      SAG1=DABS(SAG1)
                      COR1=(1.0D0/3.0D0)*PII*(SAG1**2)*((3.0D0*RD)-SAG1)
                      COR1=DABS(COR1)
                      IF(SIGNIT1.LT.0.0D0) THEN
                          COR1=(PII*RHO2*SAG1)-COR1
                      END IF
                  END IF
C     ADJUSTMENT FOR BACK OF MIRROR
                  IF(ALENS(9,I).EQ.1.0D0.AND.ALENS(127,I).EQ.0.0D0) THEN
                      RHOB=DABS(ALENS(10,I))
                  ELSE
                      RHOB=DABS(PXTRAY(1,I))+DABS(PXTRAY(5,I))
                  END IF
                  IF(ALENS(1,I).EQ.0.0D0) THEN
                      COR2=0.0D0
                  ELSE
                      IF(ALENS(46,I).LT.0.0D0) THEN
                          RD=(1.0D0/ALENS(1,I))-DABS(ALENS(110,I))
                      ELSE
                          RD=(1.0D0/ALENS(1,I))+DABS(ALENS(110,I))
                      END IF
                      C=1.0D0/RD
                      C2=C**2
                      RHO2=RHOB**2
                      SAG2=((C*RHO2)/
     1                (1.0D0+(DSQRT(1.0D0-(C2*RHO2)))))
                      IF(SAG2.GE.0.0D0) SIGNIT2=1.0D0
                      IF(SAG2.LT.0.0D0) SIGNIT2=-1.0D0
                      SAG2=DABS(SAG2)
                      COR2=(1.0D0/3.0D0)*PII*(SAG2**2)*((3.0D0*RD)-SAG2)
                      COR2=DABS(COR2)
                      IF(SIGNIT2.GT.0.0D0) THEN
                          COR2=(PII*RHO2*SAG2)-COR2
                      END IF
                  END IF
C     WEIGHT OF PLUG
                  H=DABS(ALENS(110,I))
                  IF(SIGNIT1.GT.0.0D0) H=H-SAG1
                  IF(SIGNIT2.LT.0.0D0) H=H-SAG2
                  PLUG=(1.0D0/3.0D0)*PII*H*((RHOA**2)+(RHOA*RHOB)+(RHOB**2))
                  THMFACTOR=(((PLUG+COR1+COR2)*CFAC)*ALENS(102,I))
                  IF(THMFACTOR.LT.0.0D0) THMFACTOR=0.0D0
                  MYWEIGHT=MYWEIGHT+THMFACTOR
                  TOTALW=TOTALW+MYWEIGHT
                  MYWEIGHT=0.0D0
                  THMFACTOR=0.0D0
              END IF
          END DO
C     CONVERT TO Kgs from gms
          WEIGT=TOTALW/1000.0D0
C
          IF(WQ.EQ.'ACC') THEN
              REG(40)=REG(9)
              REG(9)=WEIGT
              RETURN
          END IF
          IF(SQ.EQ.0) THEN
              REG(40)=REG(9)
              REG(9)=WEIGT
              WRITE(OUTLYNE,1000)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,2000)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,2010) INT(W1)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,2020) INT(W2)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,2002) WEIGT
              CALL SHOWIT(0)
              RETURN
          END IF
 1000     FORMAT(1X)
 2000     FORMAT('    WEIGHT CALCULATION')
 2010     FORMAT('    FROM SURFACE # = ',I3)
 2020     FORMAT('      TO SURFACE # = ',I3)
 2002     FORMAT('     MASS (WEIGHT) = ',G15.6,1X,'Kgs')
          RETURN
      END
C SUB DLRPFR.FOR
      SUBROUTINE DLRPFR
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE DLRPFR. THIS SUBROUTINE IMPLEMENTS THE
C       DELTA RADIUS PER FRINGE CALCULATION
C       NW1=RADIUS OF CURVATURE IN CURRENT LENS UNITS (REQUIRED)
C       NW2=PART DIAMETER IN CURRENT LENS UNITS (REQUIRED)
C       NW3=WAVELENGTH IN MICRONS (DEFAULT = 0.5461)
C         NW4 AND NW5 NOT USED
C         WQ NOT USED
C         WS NOT USED
C
          CHARACTER UN*11
C
          REAL*8 DR,WAVE,ARG1
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
C
          IF(SYSTEM1(6).EQ.1.0) UN='INCHES     '
          IF(SYSTEM1(6).EQ.2.0) UN='CENTIMETERS'
          IF(SYSTEM1(6).EQ.3.0) UN='MILLIMETERS'
          IF(SYSTEM1(6).EQ.4.0) UN='METERS'
C
C
          IF(SST.EQ.1) THEN
              WRITE(OUTLYNE,*)'"DR/FR" TAKES NO ALPHANUMERIC STRING INPUT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(S4.EQ.1.OR.S5.EQ.1) THEN
              WRITE(OUTLYNE,*)'"DR/FR" TAKES NO NUMERIC WORD #4 OR #5 INPUT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SQ.EQ.1.AND.WQ.NE.'ACC') THEN
              WRITE(OUTLYNE,*)'INVALID QUALIFIER WORD USED WITH "DR/FR"'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C       HANDLE DEFAULT VALUES FOR NW1, NW2 AND NW3
C
          IF(DF1.EQ.1.OR.DF2.EQ.1) THEN
              WRITE(OUTLYNE,*)
     1        '"DR/FR" REQUIRES EXPLICIT NUMERIC WORD #1 AND #2 INPUT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(DF3.EQ.1) W3=0.5461D0
          IF(W1.EQ.0.0D0) THEN
              WRITE(OUTLYNE,*)'NUMERIC WORD #1 MUST BE NON-ZERO'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(W2.EQ.0.0D0) THEN
              WRITE(OUTLYNE,*)'NUMERIC WORD #2 MUST BE NON-ZERO'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(W3.EQ.0.0D0) THEN
              WRITE(OUTLYNE,*)'NUMERIC WORD #3 MUST BE NON-ZERO'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          ARG1=(W1**2)-((W2/2.0D0)**2)
          IF(ARG1.LT.0.0D0) THEN
              WRITE(OUTLYNE,*)
     1        'DR/FR CAN NOT BE CALCULATED FOR CURRENT INPUT VALUES'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SYSTEM1(6).EQ.1.0) WAVE=(W3*1.0D-3)/(25.4D0)
          IF(SYSTEM1(6).EQ.2.0) WAVE=W3*1.0D-4
          IF(SYSTEM1(6).EQ.3.0) WAVE=W3*1.0D-3
          IF(SYSTEM1(6).EQ.4.0) WAVE=W3*1.0D-6
          DR=DABS((WAVE*DSQRT(ARG1))
     1    /(2.0D0*(DSQRT(ARG1)-DABS(W1))))
          IF(WQ.EQ.'ACC') THEN
              REG(40)=REG(9)
              REG(9)=DR
              RETURN
          END IF
          IF(SQ.EQ.0) THEN
              WRITE(OUTLYNE,1000)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,2000)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,2010) W1,UN
              CALL SHOWIT(0)
              WRITE(OUTLYNE,2001) W2,UN
              CALL SHOWIT(0)
              WRITE(OUTLYNE,2002) W3
              CALL SHOWIT(0)
              WRITE(OUTLYNE,2003) DR,UN
              CALL SHOWIT(0)
              RETURN
          END IF
 1000     FORMAT(1X)
 2000     FORMAT('    DELTA RADIUS PER FRINGE CALCULATION')
 2010     FORMAT('    RADIUS OF CURVATURE = ',G15.6,1X,A11)
 2001     FORMAT('          PART DIAMETER = ',G15.6,1X,A11)
 2002     FORMAT('             WAVELENGTH = ',G12.4,' MICRONS')
 2003     FORMAT('DELTA RADIUS PER FRINGE = ',G15.6,1X,A11)
          RETURN
      END
C SUB DTOR.FOR
      SUBROUTINE DTOR
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE DTOR. THIS SUBROUTINE IMPLEMENTS
C     THE CMD LEVEL "DTOR" COMMAND
C
C       FORM OF THE DTOR COMMAND IS:
C
C       DTOR , DVALUE ,EVALUE
C
          INCLUDE 'datmai.inc'
C
          IF(STI.EQ.1) THEN
              WRITE(OUTLYNE,*)
     1        '"DTOR" CONVERTS D TO R USING THE (e) VALUE'
              CALL SHOWIT(1)
              RETURN
          END IF
          IF(SST.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1.OR.SQ.EQ.1) THEN
              WRITE(OUTLYNE,*)
     1        '"DTOR" ONLY TAKES NUMERIC WORD #1 AND #2 INPUT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C       HANDLE DEFAULT VALUES FOR NW1 AND NW2 AND NW3
C
          IF(DF1.EQ.1.OR.DF2.EQ.1) THEN
              WRITE(OUTLYNE,*)
     1        '"DTOR" REQUIRES EXPLICIT NUMERIC WORD #1 AND #2 INPUT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
          IF(W1.EQ.0.0D0) THEN
              WRITE(OUTLYNE,*)
     1        '"DTOR" REQUIRES A NON-ZERO D VALUE'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(W2.EQ.0.0D0) THEN
              WRITE(OUTLYNE,*)
     1        '"DTOR" REQUIRES A NON-ZERO (e) VALUE'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
          WRITE(OUTLYNE,1000)
          CALL SHOWIT(0)
          WRITE(OUTLYNE,5000)
          CALL SHOWIT(0)
          WRITE(OUTLYNE,5001) W1
          CALL SHOWIT(0)
          WRITE(OUTLYNE,5002) W2
          CALL SHOWIT(0)
          WRITE(OUTLYNE,1000)
          CALL SHOWIT(0)
          WRITE(OUTLYNE,5003) -(W1*W2)
          CALL SHOWIT(0)
          REG(40)=REG(9)
          REG(9)=-(W1*W2)
C
 1000     FORMAT(1X)
 5000     FORMAT('FOR THE INPUT VALUES OF:')
 5001     FORMAT('D = ',D23.15,' LENS UNITS')
 5002     FORMAT('e = ',D23.15)
 5003     FORMAT('R = ',D23.15,' LENS UNITS')
          RETURN
      END
C SUB RTOD.FOR
      SUBROUTINE RTOD
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE RTOD. THIS SUBROUTINE IMPLEMENTS
C     THE CMD LEVEL "RTOD" COMMAND
C
C       FORM OF THE DTOR COMMAND IS:
C
C       RTOD , RVALUE ,EVALUE
C
          INCLUDE 'datmai.inc'
C
          IF(STI.EQ.1) THEN
              WRITE(OUTLYNE,*)
     1        '"RTOD" CONVERTS R TO D USING THE (e) VALUE'
              CALL SHOWIT(1)
              RETURN
          END IF
          IF(SST.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1.OR.SQ.EQ.1) THEN
              WRITE(OUTLYNE,*)
     1        '"RTOD" ONLY TAKES NUMERIC WORD #1 AND #2 INPUT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C       HANDLE DEFAULT VALUES FOR NW1 AND NW2 AND NW3
C
          IF(DF1.EQ.1.OR.DF2.EQ.1) THEN
              WRITE(OUTLYNE,*)
     1        '"RTOD" REQUIRES EXPLICIT NUMERIC WORD #1 AND #2 INPUT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
          IF(W1.EQ.0.0D0) THEN
              WRITE(OUTLYNE,*)
     1        '"RTOD" REQUIRES A NON-ZERO R VALUE'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(W2.EQ.0.0D0) THEN
              WRITE(OUTLYNE,*)
     1        '"RTOD" REQUIRES A NON-ZERO (e) VALUE'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
          WRITE(OUTLYNE,1000)
          CALL SHOWIT(0)
          WRITE(OUTLYNE,5000)
          CALL SHOWIT(0)
          WRITE(OUTLYNE,5001) W1
          CALL SHOWIT(0)
          WRITE(OUTLYNE,5002) W2
          CALL SHOWIT(0)
          WRITE(OUTLYNE,1000)
          CALL SHOWIT(0)
          WRITE(OUTLYNE,5003) -W1/W2
          CALL SHOWIT(0)
          REG(40)=REG(9)
          REG(9)=-(W1/W2)
C
 1000     FORMAT(1X)
 5000     FORMAT('FOR THE INPUT VALUES OF:')
 5001     FORMAT('R = ',D23.15,' LENS UNITS')
 5002     FORMAT('e = ',D23.15)
 5003     FORMAT('D = ',D23.15,' LENS UNITS')
          RETURN
      END
C SUB ETOCC.FOR
      SUBROUTINE ETOCC
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE ETOCC. THIS SUBROUTINE IMPLEMENTS
C     THE CMD LEVEL "ETOCC" COMMAND
C
C       FORM OF THE ETOCC COMMAND IS:
C
C       ETOCC , (e) , (-1.0 IF e IS IMAGINARY)
C
          INCLUDE 'datmai.inc'
C
          IF(STI.EQ.1) THEN
              WRITE(OUTLYNE,*)
     1        '"ETOCC" CONVERTS (e) TO CC'
              CALL SHOWIT(1)
              RETURN
          END IF
          IF(SST.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1.OR.SQ.EQ.1) THEN
              WRITE(OUTLYNE,*)
     1        '"ETOCC" ONLY TAKES NUMERIC WORD #1 AND #2 INPUT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C       HANDLE DEFAULT VALUES FOR NW1
C
          IF(DF1.EQ.1) THEN
              WRITE(OUTLYNE,*)
     1        '"ETOCC" REQUIRES EXPLICIT NUMERIC WORD #1'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(W1.LT.0.0D0) THEN
              WRITE(OUTLYNE,*)
     1        '"ETOCC" REQUIRES A POSITIVE NUMERIC WORD #1'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(S2.EQ.1.AND.W2.NE.-1.0D0) THEN
              WRITE(OUTLYNE,*)
     1        'EXPLICIT NUMERIC WORD #2 INPUT MUST BE "-1.0"'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
          IF(DF2.EQ.1) W2=1.0D0
C
C
          WRITE(OUTLYNE,1000)
          CALL SHOWIT(0)
          WRITE(OUTLYNE,5000)
          CALL SHOWIT(0)
          WRITE(OUTLYNE,5001) W1
          CALL SHOWIT(0)
          IF(W2.NE.-1.0D0) WRITE(OUTLYNE,5003)
          IF(W2.NE.-1.0D0) CALL SHOWIT(0)
          IF(W2.EQ.-1.0D0) WRITE(OUTLYNE,5004)
          IF(W2.EQ.-1.0D0) CALL SHOWIT(0)
          CALL SHOWIT(0)
          WRITE(OUTLYNE,1000)
          CALL SHOWIT(0)
          WRITE(OUTLYNE,5002) -W2*(W1**2)
          CALL SHOWIT(0)
          REG(40)=REG(9)
          REG(9)=-W2*(W1**2)
C
 1000     FORMAT(1X)
 5000     FORMAT('FOR THE INPUT VALUE OF:')
 5001     FORMAT('(e) = ',D23.15)
 5002     FORMAT('CC  = ',D23.15)
 5003     FORMAT('(e) WAS INPUT AS REAL')
 5004     FORMAT('(e) WAS INPUT AS IMAGINARY')
          RETURN
      END
C SUB CCTOE.FOR
      SUBROUTINE CCTOE
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE CCTOE. THIS SUBROUTINE IMPLEMENTS
C     THE CMD LEVEL "CCTOE" COMMAND
C
C       FORM OF THE CCTOE COMMAND IS:
C
C       CCTOE , CC
C
          INCLUDE 'datmai.inc'
C
          IF(STI.EQ.1) THEN
              WRITE(OUTLYNE,*)
     1        '"CCTOE" CONVERTS CC TO (e)'
              CALL SHOWIT(1)
              RETURN
          END IF
          IF(SST.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1.OR.SQ.EQ.1
     1    .OR.S2.EQ.1) THEN
              WRITE(OUTLYNE,*)
     1        '"CCTOE" ONLY TAKES NUMERIC WORD #1 INPUT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C       HANDLE DEFAULT VALUES FOR NW1
C
          IF(DF1.EQ.1) THEN
              WRITE(OUTLYNE,*)
     1        '"CCTOE" REQUIRES EXPLICIT NUMERIC WORD #1'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
          WRITE(OUTLYNE,1000)
          CALL SHOWIT(0)
          WRITE(OUTLYNE,5000)
          CALL SHOWIT(0)
          WRITE(OUTLYNE,5001) W1
          CALL SHOWIT(0)
          WRITE(OUTLYNE,1000)
          CALL SHOWIT(0)
          IF(W1.NE.0.0D0) WRITE(OUTLYNE,5002) DSQRT(DABS(W1))
          IF(W1.NE.0.0D0) CALL SHOWIT(0)
          IF(W1.EQ.0.0D0) WRITE(OUTLYNE,5002) W1
          IF(W1.EQ.0.0D0) CALL SHOWIT(0)
          IF(W1.LE.0.0D0) WRITE(OUTLYNE,5003)
          IF(W1.LE.0.0D0) CALL SHOWIT(0)
          IF(W1.GT.0.0D0) WRITE(OUTLYNE,5004)
          IF(W1.GT.0.0D0) CALL SHOWIT(0)
          REG(40)=REG(9)
          IF(W1.NE.0.0D0) REG(9)=DSQRT(DABS(W1))
          IF(W1.EQ.0.0D0) REG(9)=0.0D0
C
 1000     FORMAT(1X)
 5000     FORMAT('FOR THE INPUT VALUE OF:')
 5001     FORMAT('CC  = ',D23.15)
 5002     FORMAT('(e)  = ',D23.15)
 5003     FORMAT('(e) IS REAL')
 5004     FORMAT('(e) IS IMAGINARY')
          RETURN
      END
C SUB ZZEEOO.FOR
      SUBROUTINE ZZEEOO
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE ZZEEOO. THIS SUBROUTINE IMPLEMENTS
C     THE CMD LEVEL "Z0" COMMAND WHICH CALCULTES THE Z POSITIONS OF
C     OF CONIC GIVEN THE CURVATURE CV, THE CONIC CONSTANT CC
C     AND A RHO HEIGHT.
C
C       FORM OF THE FOCI COMMAND IS:
C
C       Z0 (R or C) ,(radius or curvature) , (conic constant) , RHO
C
          REAL*8 RHO,CEE,Z1,Z2,AC,KAPPA
C
          INCLUDE 'datmai.inc'
C
          IF(STI.EQ.1) THEN
              WRITE(OUTLYNE,*)
     1        '"Z0" COMPUTES AND DISPLAYS CONIC SECTION Z POSITIONS'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)
     1        'GIVEN RADII OR CURVATURE CONIC CONSTANT AND RHO AS INPUTS'
              CALL SHOWIT(1)
              RETURN
          END IF
          IF(SST.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
              WRITE(OUTLYNE,*)
     1        '"Z0" ONLY TAKES QUALIFIER AND NUMERIC WORD #1, #2 AND #3 INPUT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C       HANDLE DEFAULT VALUES FOR NW1 AND NW2 AND NW3
C
          IF(DF1.EQ.1.OR.DF2.EQ.1.OR.DF3.EQ.1) THEN
              WRITE(OUTLYNE,*)
     1        '"Z0" REQUIRES EXPLICIT NUMERIC WORD #1, #2 AND #3 INPUT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SQ.EQ.0) THEN
              SQ=1
              WQ='R       '
          END IF
          IF(WQ(1:1).NE.'R'.AND.WQ(1:1).NE.'C') THEN
              WRITE(OUTLYNE,*)
     1        '"Z0" ONLY TAKES "R" (FOR RADIUS)'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)
     1        '               OR "C" (FOR CURVATURE) AS QUALIFIER WORDS'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
C     FLAT SURFACES
          IF(W1.EQ.0.0D0.OR.WQ.EQ.'R'.AND.DABS(W1).GT.1.0D20) THEN
              WRITE(OUTLYNE,*)
     1        '"Z0" REQUIRES A NON-PLANO SURFACE'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
          Z1=0.0D0
          Z2=0.0D0
          IF(WQ(1:1).EQ.'R') CEE=1/W1
          IF(WQ(1:1).EQ.'C') CEE=W1
          KAPPA=W2
          RHO=W3
          AC=4.0D0-(4.0D0*(KAPPA+1.0D0)*(RHO**2)*(CEE**2))
          IF(AC.LT.0.0D0) THEN
              WRITE(OUTLYNE,*)
     1        '"Z0" CAN NOT BE CALCULATED WITH CURRENT INPUT VALUES'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*) 'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF((KAPPA+1.0D0).NE.0.0D0) THEN
              Z1=(2.0D0+DSQRT(AC))/(2.0D0*(KAPPA+1.0D0)*CEE)
              Z2=(2.0D0-DSQRT(AC))/(2.0D0*(KAPPA+1.0D0)*CEE)
          ELSE
C     PARABOLA
              Z1=((RHO**2)*CEE)/2.0D0
          END IF
          IF((KAPPA+1.0D0).EQ.0.0D0) THEN
C     ONE Z VALUE
              WRITE(OUTLYNE,1000)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,5000)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,5001)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,1001) CEE,(1.0D0/CEE)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,1002) KAPPA
              CALL SHOWIT(0)
              WRITE(OUTLYNE,1003) RHO
              CALL SHOWIT(0)
              WRITE(OUTLYNE,1000)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,2000) Z1
              CALL SHOWIT(0)
              REG(40)=REG(9)
              REG(9)=(Z1)
          ELSE
C     TWO ZEE VALUES
              WRITE(OUTLYNE,1000)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,5000)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,5002)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,1001) CEE,(1.0D0/CEE)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,1002) KAPPA
              CALL SHOWIT(0)
              WRITE(OUTLYNE,1003) RHO
              CALL SHOWIT(0)
              WRITE(OUTLYNE,1000)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,2001) Z1
              CALL SHOWIT(0)
              WRITE(OUTLYNE,2002) Z2
              CALL SHOWIT(0)
              REG(40)=REG(9)
              REG(10)=Z1
              REG(9)=Z2
          END IF
C
 1000     FORMAT(1X)
 5000     FORMAT('FOR THE INPUT VALUES OF:')
 5001     FORMAT('PARABOLA WITH ONE Z VALUE')
 5002     FORMAT('NON-PARABOLA WITH TWO Z VALUES')
 1001     FORMAT('     CURVATURE = ',D23.15,' (RADIUS = ',D23.15,')')
 1002     FORMAT('CONIC CONSTANT = ',D23.15)
 1003     FORMAT('           RHO = ',D23.15)
 2000     FORMAT('  THE NEW Z IS = ',D23.15)
 2001     FORMAT(' THE NEW Z1 IS = ',D23.15)
 2002     FORMAT(' THE NEW Z2 IS = ',D23.15)
          RETURN
      END
C SUB RRHHOO.FOR
      SUBROUTINE RRHHOO
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE RRHHOO. THIS SUBROUTINE IMPLEMENTS
C     THE CMD LEVEL "RHO" COMMAND WHICH CALCULTES THE RHO POSITIONS OF
C     OF CONICS GIVEN THE CURVATURE CV, THE CONIC CONSTANT CC
C     AND A Z-POSITION (Z).
C
C       FORM OF THE FOCI COMMAND IS:
C
C       RHO (R or C) ,(radius or curvature) , (conic constant) , Z
C
          REAL*8 RHO2,RHO,CEE,ZEE,KAPPA
C
          INCLUDE 'datmai.inc'
C
          IF(STI.EQ.1) THEN
              WRITE(OUTLYNE,*)
     1        '"RHO" COMPUTES AND DISPLAYS CONIC SECTION RHO HEIGHTS'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)
     1        'GIVEN RADII OR CURVATURE CONIC CONSTANT AND Z AS INPUTS'
              CALL SHOWIT(1)
              RETURN
          END IF
          IF(SST.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
              WRITE(OUTLYNE,*)
     1        '"RHO" ONLY TAKES QUALIFIER AND NUMERIC WORD #1, #2 AND #3 INPUT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C       HANDLE DEFAULT VALUES FOR NW1 AND NW2 AND NW3
C
          IF(DF1.EQ.1.OR.DF2.EQ.1.OR.DF3.EQ.1) THEN
              WRITE(OUTLYNE,*)
     1        '"RHO" REQUIRES EXPLICIT NUMERIC WORD #1, #2 AND #3 INPUT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SQ.EQ.0) THEN
              SQ=1
              WQ='R       '
          END IF
          IF(WQ(1:1).NE.'R'.AND.WQ(1:1).NE.'C') THEN
              WRITE(OUTLYNE,*)
     1        '"RHO" ONLY TAKES "R" (FOR RADIUS)'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)
     1        '               OR "C" (FOR CURVATURE) AS QUALIFIER WORDS'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
C     FLAT SURFACES
          IF(W1.EQ.0.0D0.OR.WQ.EQ.'R'.AND.DABS(W1).GT.1.0D20) THEN
              WRITE(OUTLYNE,*)
     1        '"RHO" REQUIRES A NON-PLANO SURFACE'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
          RHO2=0.0D0
          IF(WQ(1:1).EQ.'R') CEE=1/W1
          IF(WQ(1:1).EQ.'C') CEE=W1
          KAPPA=W2
          ZEE=W3
          RHO2=(((2.0D0*ZEE)-(CEE*(KAPPA+1.0D0)*(ZEE**2)))/CEE)
          IF(RHO2.LT.0.0D0) THEN
              WRITE(OUTLYNE,*)
     1        '"RHO" WILL BE IMAGINARY WITH CURRENT INPUT VALUES'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          RHO=DSQRT(RHO2)
          WRITE(OUTLYNE,1000)
          CALL SHOWIT(0)
          WRITE(OUTLYNE,5000)
          CALL SHOWIT(0)
          WRITE(OUTLYNE,1001) CEE,(1.0D0/CEE)
          CALL SHOWIT(0)
          WRITE(OUTLYNE,1002) KAPPA
          CALL SHOWIT(0)
          WRITE(OUTLYNE,1003) ZEE
          CALL SHOWIT(0)
          WRITE(OUTLYNE,1000)
          CALL SHOWIT(0)
          WRITE(OUTLYNE,2000) RHO
          CALL SHOWIT(0)
          REG(40)=REG(9)
          REG(9)=RHO
C
C
 1000     FORMAT(1X)
 5000     FORMAT('FOR THE INPUT VALUES OF:')
 1001     FORMAT('     CURVATURE = ',D23.15,' (RADIUS = ',D23.15,')')
 1002     FORMAT('CONIC CONSTANT = ',D23.15)
 1003     FORMAT('             Z = ',D23.15)
 2000     FORMAT('THE NEW RHO IS = ',D23.15)
          RETURN
      END
C SUB FOCII.FOR
      SUBROUTINE FOCII
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE FOCII. THIS SUBROUTINE IMPLEMENTS
C     THE CMD LEVEL "FOCI" COMMAND WHICH CALCULTES THE POSITIONS OF
C     FOCI OF CONICS GIVEN THE CURVATURE CV AND THE CONIC CONSTANT CC
C
C       FORM OF THE FOCI COMMAND IS:
C
C       FOCI (R or C) ,(radius or curvature) , (conic constant)
C
          REAL*8 W11,B,FC,FC1,FC2
C
          INCLUDE 'datmai.inc'
C
          FC=0.0D0
          FC1=0.0D0
          FC2=0.0D0
C       THE FOCI COMMAND ACCEPTS ONLY NW1 AND NW2 NUMERIC
C       INPUT . IT DOES NOT ACCEPT STING OR QUALIFIER
C       INPUT.
C
          IF(STI.EQ.1) THEN
              WRITE(OUTLYNE,*)
     1        '"FOCI" COMPUTES AND DISPLAYS CONIC SECTION FOCI POSITIONS'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)
     1        'GIVEN RADII OR CURVATURE AND CONIC CONSTANT AS INPUTS'
              CALL SHOWIT(1)
              RETURN
          END IF
          IF(SST.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
              WRITE(OUTLYNE,*)
     1        '"FOCI" ONLY TAKES QUALIFIER AND NUMERIC WORD #1 AND #2 INPUT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C       HANDLE DEFAULT VALUES FOR NW1 AND NW2
C
          IF(DF1.EQ.1.OR.DF2.EQ.1) THEN
              WRITE(OUTLYNE,*)
     1        '"FOCI" REQUIRES EXPLICIT NUMERIC WORD #1 AND #2 INPUT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SQ.EQ.0) THEN
              SQ=1
              WQ='R       '
          END IF
          IF(WQ(1:1).NE.'R'.AND.WQ(1:1).NE.'C') THEN
              WRITE(OUTLYNE,*)
     1        '"FOCI" ONLY TAKES "R" (FOR RADIUS)'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)
     1        '               OR "C" (FOR CURVATURE) AS QUALIFIER WORDS'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
C     FLAT SURFACES
          IF(W1.EQ.0.0D0.OR.WQ.EQ.'R'.AND.DABS(W1).GT.1.0D20) THEN
              WRITE(OUTLYNE,*)
     1        '"FOCI" REQUIRES A NON-PLANO SURFACE'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
C     SPHERICAL SURFACES
          IF(W2.EQ.0.0D0) THEN
              WRITE(OUTLYNE,*)
     1        '"FOCI" REQUIRES A NON-SPHERICAL SURFACE (CC NOT = 0)'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
C     CC > 0 OBLATE SHPEROID SURFACES
          IF(W2.GT.0.0D0) THEN
              WRITE(OUTLYNE,*)
     1        '"FOCI" DOES NOT WORK WITH OBLATE SPHERIODS'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)
     1        '"FOCI" REQUIRES CC < 0)'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
          IF(W2.EQ.-1.0D0) THEN
C     PARABOLA
              IF(WQ(1:1).EQ.'C') W11=W1
              IF(WQ(1:1).EQ.'R') W11=1.0D0/W1
              FC=1.0D0/(W11*2.0D0)
              WRITE(OUTLYNE,1000)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,1001)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,1000)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,1002) FC
              CALL SHOWIT(0)
              REG(40)=REG(9)
              REG(9)=FC
              RETURN
          END IF
          IF(W2.LT.-1.0D0) THEN
C     HYPERBOLA
              IF(WQ(1:1).EQ.'C') W11=W1
              IF(WQ(1:1).EQ.'R') W11=1.0D0/W1
              B=-1.0D0/(W11*(W2+1.0D0))
              FC1=B*(DSQRT(-W2)-1.0D0)
              FC2=B*(-DSQRT(-W2)-1.0D0)
              WRITE(OUTLYNE,1000)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,2001)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,1000)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,2002) FC1
              CALL SHOWIT(0)
              WRITE(OUTLYNE,2003) FC2
              CALL SHOWIT(0)
              REG(40)=REG(9)
              REG(10)=FC1
              REG(9)=FC2
              RETURN
          END IF
          IF(W2.LT.0.0D0.AND.W2.GT.-1.0D0) THEN
C     PROLATE SPHERIOD
              IF(WQ(1:1).EQ.'C') W11=W1
              IF(WQ(1:1).EQ.'R') W11=1.0D0/W1
              B=(1.0D0/(W11*(W2+1.0D0)))
              FC1=B*(1.0D0-DSQRT(DABS(W2)))
              FC2=B*(1.0D0+DSQRT(DABS(W2)))
              WRITE(OUTLYNE,1000)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,3001)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,1000)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,3002) FC1
              CALL SHOWIT(0)
              WRITE(OUTLYNE,3003) FC2
              CALL SHOWIT(0)
              REG(40)=REG(9)
              REG(10)=FC1
              REG(9)=FC2
              RETURN
          END IF
 1000     FORMAT(1X)
 1001     FORMAT('SURFACE TYPE = PARABOLA')
 1002     FORMAT('SURFACE VERTEX TO FOCUS DISTANCE IS = ',D23.15)
 2001     FORMAT('SURFACE TYPE = HYPERBOLA')
 2002     FORMAT('SURFACE VERTEX TO NEAR FOCUS DISTANCE IS = ',D23.15)
 2003     FORMAT('SURFACE VERTEX TO FAR  FOCUS DISTANCE IS = ',D23.15)
 3001     FORMAT('SURFACE TYPE = PROLATE SPHERIOD')
 3002     FORMAT('SURFACE VERTEX TO NEAR FOCUS DISTANCE IS = ',D23.15)
 3003     FORMAT('SURFACE VERTEX TO FAR  FOCUS DISTANCE IS = ',D23.15)
          RETURN
      END
C SUB APOVERT.FOR
      SUBROUTINE APOVERT
C
          IMPLICIT NONE
C
C       THIS SUBROUTINE HANDELS THE "K0" AND "CVG"
C       CMD LEVEL COMMANDS
C
C                       DEFINE VARIABLES
C
          REAL*8 CURVE,ZZERO
C
          INCLUDE 'datmai.inc'
C
          ZZERO=0.0D0
          IF(SQ.EQ.1.OR.SST.EQ.1) THEN
              WRITE(OUTLYNE,*)
     1        '"K0" AND "CVG" TAKE NO QUALIFIER OR STRING INPUT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(WC.EQ.'K0'.AND.STI.EQ.1) THEN
              WRITE(OUTLYNE,*)'"K0" DOES A K0, APO-VERTEXT CALCULATION'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              RETURN
          END IF
          IF(WC.EQ.'CVG'.AND.STI.EQ.1) THEN
              WRITE(OUTLYNE,*)
     1        '"CVG" DOES A CURVATURE, APO-VERTEXT CALCULATION'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              RETURN
          END IF
          IF(WC.EQ.'K0') THEN
              IF(S4.EQ.1.OR.S5.EQ.1) THEN
                  WRITE(OUTLYNE,*)'"K0" TAKES NO NUMERIC WORD #4 OR #5 INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  RETURN
              END IF
              IF(DF1.EQ.1.OR.DF2.EQ.1.OR.DF3.EQ.1) THEN
                  WRITE(OUTLYNE,*)
     1            '"K0" REQUIRES EXPLICIT NUMERIC WORD #1, #2 AND #3 INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  RETURN
              END IF
              IF(W1.EQ.0.0D0) THEN
                  WRITE(OUTLYNE,*)
     1            '"K0" REQUIRES A NON-ZERO NUMERIC WORD #1 INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  RETURN
              END IF
              ZZERO=(1.0D0/W1)+((1.0D0+W2)*W3)
 10           FORMAT('THE "Z0" VALUE IS : ',D23.15,' LENS UNITS')
              WRITE(OUTLYNE,10) ZZERO
              CALL SHOWIT(0)
              REG(40)=REG(9)
              REG(9)=ZZERO
              RETURN
          END IF
          IF(WC.EQ.'CVG') THEN
              IF(S4.EQ.1.OR.S5.EQ.1) THEN
                  WRITE(OUTLYNE,*)'"CVG" TAKES NO NUMERIC WORD #4 OR #5 INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  RETURN
              END IF
              IF(DF1.EQ.1.OR.DF2.EQ.1.OR.DF3.EQ.1) THEN
                  WRITE(OUTLYNE,*)
     1            '"CVG" REQUIRES EXPLICIT NUMERIC WORD #1, #2 AND #3 INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  RETURN
              END IF
              CURVE=(W1)+((1.0D0+W2)*W3)
              IF(CURVE.EQ.0.0D0) THEN
                  WRITE(OUTLYNE,*)
     1            'THE "CVR" VALUE WAS INFINITE. CHECK YOUR INPUT VALUES'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  REG(40)=REG(9)
                  REG(9)=0.0D0
                  CALL MACFAL
                  RETURN
              END IF
 20           FORMAT('THE "CVR" VALUE IS : ',D23.15,' INVERSE LENS UNITS')
              WRITE(OUTLYNE,20) (1.0D0/CURVE)
              CALL SHOWIT(0)
              REG(40)=REG(9)
              REG(9)=(1.0D0/CURVE)
              RETURN
          END IF
C
      END
