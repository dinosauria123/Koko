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

C       SEVENTH FILE OF CAPFN/SPOT ROUTINES

C SUB FLDS.FOR
      SUBROUTINE FLDS
C
          IMPLICIT NONE
C
C       THIS SUBROUTINE HANDELS THE ASSIGNMENT OF MULTIPLE FIELDS OF VIEW
C       TO THE CURRENT LENS
C
          INTEGER I
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
C
          IF(STI.EQ.1.OR.SQ.EQ.0.AND.SN.EQ.0) THEN
              OUTLYNE=
     1        '"FLDS" SETS AND QUERRIES THE STATUS OF MULTIPLE FIELDS OF VIEW'
              CALL SHOWIT(1)
              OUTLYNE=
     1        'ASSOCIATED WITH THE CURRENT LENS'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)
     1        'THERE ARE CURRENTLY ',CFLDCNT,
     1        ' FRACTIONAL FIELD POSITIONS DEFINED'
              CALL SHOWIT(1)
              IF(CFLDCNT.GT.0) THEN
                  CALL SORTFIELDS
                  DO I=1,CFLDCNT
                      WRITE(OUTLYNE,10)I,CFLDS(1,I),CFLDS(2,I)
                      CALL SHOWIT(1)
                  END DO
 10               FORMAT('POS # = ',I2,' X-VALUE = ',G13.6,' Y-VALUE = ',G13.6)
              END IF
              RETURN
          END IF
C
          IF(SST.EQ.1) THEN
              OUTLYNE='"FLDS" TAKES NO STRING INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SQ.EQ.1) THEN
              IF(WQ.NE.'MAX') THEN
                  OUTLYNE=
     1            '"FLDS" ONLY TAKES "MAX" AS VALID'
                  CALL SHOWIT(1)
                  OUTLYNE='QUALIFIER WORD INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
C     WQ="MAX"
          IF(WQ.EQ.'MAX') THEN
              IF(S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
                  OUTLYNE='"FLDS MAX" ONLY TAKES NUMERIC WORD #1 INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(DF1.EQ.1) THEN
                  OUTLYNE='"FLDS MAX" REQUIRES EXPLICIT NUMERIC WORD #1 INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(W1.LT.1.0D0.OR.W1.GT.10.0D0) THEN
                  OUTLYNE='MAXIMUM NUMBER OF FIELDS MUST BE BETWEEN 1 AND 10'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              CFLDCNT=INT(W1)
              DO I=CFLDCNT+1,10
                  CFLDS(1,I)=0.0D0
                  CFLDS(2,I)=0.0D0
              END DO
              RETURN
          END IF
          CFLDTYPE=0
          IF(INT(W1).GT.CFLDCNT) THEN
              OUTLYNE='FIELD SPECIFIER (NW#1) IS GREATER THAN THE CURRENTLY'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)
     1        'DEFINED MAXIMUM FIELD COUNT SPECIFIER OF ',CFLDCNT
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          CFLDS(1,INT(W1))=W2
          CFLDS(2,INT(W1))=W3
          RETURN
      END
      SUBROUTINE SORTFIELDS
C
          INTEGER I
C
          REAL*8 JK_A(1:10),JK_1(1:10),JK_2(1:10)
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          IF(CFLDCNT.GT.1) THEN
C     SORT BY INCREASING VALUE
              DO I=1,CFLDCNT
                  JK_A(I)=(CFLDS(1,I)**2)+(CFLDS(2,I)**2)
                  JK_1(I)=CFLDS(1,I)
                  JK_2(I)=CFLDS(2,I)
              END DO
              CALL SORT_JK1(CFLDCNT,JK_A(1:CFLDCNT),JK_1(1:CFLDCNT)
     1        ,JK_2(1:CFLDCNT))
              DO I=1,CFLDCNT
                  CFLDS(1,I)=JK_1(I)
                  CFLDS(2,I)=JK_2(I)
              END DO
          ELSE
C     NO SORT
          END IF
          RETURN
      END
C SUB FLDSARE.FOR
      SUBROUTINE FLDSARE
C
          IMPLICIT NONE
C
C       THIS SUBROUTINE HANDELS THE FLDSARE COMMAND
C
          INTEGER I
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
          CALL SORTFIELDS
C
          IF(STI.EQ.1) THEN
              OUTLYNE=
     1        '"FLDSARE" QUERRIES THE STATUS OF MULTIPLE FIELDS OF VIEW'
              CALL SHOWIT(1)
              OUTLYNE=
     1        'ASSOCIATED WITH THE CURRENT LENS'
              CALL SHOWIT(1)
              RETURN
          END IF
          IF(SN.EQ.1.OR.SST.EQ.1.OR.SQ.EQ.1) THEN
              OUTLYNE=
     1        '"FLDSARE" TAKES NO ADDITIONAL INPUT'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(CFLDCNT.EQ.0) THEN
              WRITE(OUTLYNE,*)
     1        'THERE ARE CURRENTLY ',CFLDCNT,' ANALYSIS FIELD POSITIONS DEFINED'
              CALL SHOWIT(0)
          END IF
          IF(CFLDCNT.GT.0) THEN
              WRITE(OUTLYNE,*)
     1        'THERE ARE CURRENTLY ',CFLDCNT,' ANALYSIS FIELD POSITIONS DEFINED'
              CALL SHOWIT(0)
              WRITE(OUTLYNE,*)
     1        'UNITS ARE DEGREES'
              CALL SHOWIT(0)
              DO I=1,CFLDCNT
                  WRITE(OUTLYNE,10)I,CFLDS(1,I),CFLDS(2,I)
                  CALL SHOWIT(0)
              END DO
          END IF
 10       FORMAT('POS # = ',I2,' X-VALUE = ',G13.6,' Y-VALUE = ',G13.6)
          RETURN
      END
C SUB TFMOTION.FOR
      SUBROUTINE TFMOTION
C
          IMPLICIT NONE
C
C       THIS SUBROUTINE HANDELS THE "TFMOTION" COMMAND
C       TO THE CURRENT LENS
C
          REAL*8 TEMPMIN,TEMPMAX
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
C
          IF(STI.EQ.1.OR.SQ.EQ.0.AND.SN.EQ.0.AND.SST.EQ.0) THEN
              OUTLYNE=
     1        '"TFMOTION" SETS UP THE FOCUS MOTION CHARACTERISTICS FOR'
              CALL SHOWIT(1)
              OUTLYNE=
     1        '"THRU-FOCUS OPTICAL TRANSFER FUNCTION ANALYSIS'
              CALL SHOWIT(1)
              IF(TFDIRECTION.EQ.0) WRITE(OUTLYNE,100)
              IF(TFDIRECTION.EQ.1) WRITE(OUTLYNE,101)
              IF(TFDIRECTION.EQ.2) WRITE(OUTLYNE,102)
 100          FORMAT('THRU-FOCUS MOTION IS CURRENTLY THE X-DIRECTION')
 101          FORMAT('THRU-FOCUS MOTION IS CURRENTLY THE Y-DIRECTION')
 102          FORMAT('THRU-FOCUS MOTION IS CURRENTLY THE Z-DIRECTION')
              CALL SHOWIT(1)
              WRITE(OUTLYNE,103) TFSURF
              CALL SHOWIT(1)
              WRITE(OUTLYNE,104) TFDELT
              CALL SHOWIT(1)
              WRITE(OUTLYNE,105) TFTMIN
              CALL SHOWIT(1)
              WRITE(OUTLYNE,106) TFTMAX
              CALL SHOWIT(1)
              RETURN
          END IF
 103      FORMAT('THRU-FOCUS SURFACE = ',I3)
 104      FORMAT('THRU-FOCUS DELTA   = ',G13.6)
 105      FORMAT('THRU-FOCUS T-MIN   = ',G13.6)
 106      FORMAT('THRU-FOCUS T-MAX   = ',G13.6)

          IF(DF5.EQ.0) THEN
              OUTLYNE=
     1        '"TFMOTION" TAKES NO NUMERIC WORD #5 INPUT'
              CALL SHOWIT(1)
              OUTLYNE=
     1        'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SST.EQ.1) THEN
              OUTLYNE=
     1        '"TFMOTION" TAKES NO STRING INPUT'
              CALL SHOWIT(1)
              OUTLYNE=
     1        'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SQ.EQ.0) THEN
              SQ=1
              WQ='Z       '
          END IF
          IF(WQ.NE.'X       '.AND.WQ.NE.'Y       '.AND.WQ
     1    .NE.'Z       ') THEN
              OUTLYNE=
     1        '"TFMOTION" ONLY TAKES "X", "Y" OR "Z" AS VALID QUALIFIER INPUT'
              CALL SHOWIT(1)
              OUTLYNE=
     1        'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(DF1.EQ.1) W1=SYSTEM1(20)
          IF(INT(W1).LT.1) THEN
              OUTLYNE=
     1        'SURFACE NUMBER, NUMERIC WORD #1, MUST BE GREATER THAN 1'
              CALL SHOWIT(1)
              OUTLYNE=
     1        'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(INT(W1).GT.INT(SYSTEM1(20))) THEN
              WRITE(OUTLYNE,*)
     1        'SURFACE NUMBER, NUMERIC WORD #1, MUST BE LESS THAN '
     1        ,INT(SYSTEM1(20))
              CALL SHOWIT(1)
              OUTLYNE=
     1        'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(DF2.EQ.1) THEN
              TFDELT=0.001D0
          ELSE
              TFDELT=DABS(W2)
          END IF
          IF(DF3.EQ.1) W3=-(TFDELT*5.0D0)
          IF(DF4.EQ.1) W4=(TFDELT*5.0D0)
          IF(W3.GE.W4) THEN
              WRITE(OUTLYNE,*)
     1        '"TMIN" (NUMERIC WORD #3) MUST BE LESS THAN'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)
     1        '"TMAX" (NUMERIC WORD #4)'
              CALL SHOWIT(1)
              OUTLYNE=
     1        'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(W2.LT.0.0D0) THEN
              WRITE(OUTLYNE,*)
     1        '"TDELT" MUST BE GREATER THAN ZERO'
              CALL SHOWIT(1)
              OUTLYNE=
     1        'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          TFSURF=INT(W1)

C
          TEMPMIN=DABS(DBLE(INT(W3/TFDELT)))
          IF(TEMPMIN.LT.1.0D0) THEN
              TEMPMIN=1.0D0
              TFTMIN=-TEMPMIN*TFDELT
          ELSE
              TFTMIN=W3
          END IF
          TEMPMAX=DABS(DBLE(INT(W4/TFDELT)))
          IF(TEMPMAX.LT.1.0D0) THEN
              TEMPMAX=1.0D0
              TFTMAX=TEMPMAX*TFDELT
          ELSE
              TFTMAX=W4
          END IF
          RETURN
      END


      SUBROUTINE GEOMTF(FREQ,MTF,PPHAS)
C
          REAL*8 AFUNC(-51:51),DELTA,DENOM,FREQ
     2    ,SINNOM,COSNOM,MTF,PPHAS,COORD(-51:51)
     3    ,XCOORD(1:110),YCOORD(1:110)
C
          COMMON/CORDITE/XCOORD,YCOORD
C
          LOGICAL EXTGLSF,LSFCENT
C
          COMMON/GLSFEXT/EXTGLSF,LSFCENT
C
          INTEGER I
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datspd.inc'
          INCLUDE 'datsp1.inc'
c
C     LINE SPREAD FUNCTION EXISTS NOW. NOW CALCULATE THE MTF FROM
C     FREQUENCY 0 TO THE DIFFRACTION CUT-OFF FREQUENCY
C
C     LSFDAT(I,1) CONTAINS THE SPREAD FUNCTION FUNCTIONAL VALUE
C     LSFDAT(I,2) CONTAINS THE COORDINATE POSITION OF THE SREAD FUNCTION
C     IN EITHER SYSTEM UNITS OF RADIANS(AFOCAL OR UAFOCAL MODES)
C     FREQ IS IN LP/LENS UNIT OR LP/RADIAN ONLY INSIDE THIS SUBROUTINE
C
C     FIRST CALCULATE THE DENOMINATOR WHICH IS THE INTEGRAL OF THE
C     SPREAD FUNCTION OVER ITS RANGE. ASSUME THAT THE LSF HAS ZERO
C     VALUE WHEN THE POSITION IN THE FUNCTION IS BEYOND THE FUNCTION
C     END POINTS BY A DISTANCE EQUAL TO THE SPACING BETWEEN
C     THE 101 TABULATED POINTS.
C
          AFUNC(-51)=0.0D0
          AFUNC(51)=0.0D0
          DO I=-50,50
              AFUNC(I)=
     1        (1.0D-6*DBLE(NINT(YCOORD(I+51)*1.0D6)))
              COORD(I)=
     1        (1.0D-6*DBLE(NINT(XCOORD(I+51)*1.0D6)))
          END DO
C
          DELTA=((DABS(COORD(-50))+DABS(COORD(50)))/100.0D0)
          COORD(-51)=COORD(-50)-DELTA
          COORD(51)=COORD(50)+DELTA
C
C     INTEGRATE USING THE TRAPEZOIDAL RULE
C
          DENOM=0.0D0
          DO I=-51,50
              DENOM=DENOM+(AFUNC(I)+AFUNC(I+1))
          END DO
          DENOM=0.5D0*DELTA*DENOM
          IF(DENOM.EQ.0.0D0) DENOM=1.0D0
C
C     NOW THE NUMERATORS
C     SINE TERM NUMERATOR
C     THE SINE TERMS IS JUST DSIN(2*PI*FREQ*COORD)
C      SINTRM=DSIN(TWOPII*FREQ*COORD(I))
C
C     INTEGRATE USING THE TRAPEZOIDAL RULE
C
          SINNOM=0.0D0
          DO I=-51,50
              SINNOM=SINNOM+(
     1        AFUNC(I)*DSIN(TWOPII*FREQ*COORD(I))
     1        +
     1        AFUNC(I+1)*DSIN(TWOPII*FREQ*COORD(I+1)))
          END DO
          SINNOM=0.5D0*DELTA*SINNOM
          SINNOM=SINNOM/DENOM
C     COSINE TERM NUMERATOR
C     THE COSINE TERMS IS JUST DCOS(2*PI*FREQ*COORD)
C      SINTRM=DCOS(TWOPII*FREQ*COORD(I))
C
C     INTEGRATE USING THE TRAPEZOIDAL RULE
C
          COSNOM=0.0D0
          DO I=-51,50
              COSNOM=COSNOM+(
     1        AFUNC(I)*DCOS(TWOPII*FREQ*COORD(I))
     1        +
     1        AFUNC(I+1)*DCOS(TWOPII*FREQ*COORD(I+1)))
          END DO
          COSNOM=0.5D0*DELTA*COSNOM
          COSNOM=COSNOM/DENOM
C
          IF(SNGL(COSNOM).EQ.0.0) THEN
              MTF=1.0D0
          ELSE
              MTF=DSQRT((SINNOM**2)+(COSNOM**2))
          END IF
          IF(DABS(SINNOM).EQ.0.0D0.AND.
     1    DABS(COSNOM).EQ.0.0D0) THEN
              PPHAS=0.0D0
          ELSE
              PPHAS=(DATAN2(SINNOM,COSNOM)*180.0D0)/PII
          END IF
C
          RETURN
      END


C SUB GOTF.FOR
      SUBROUTINE GOTF
C
          IMPLICIT NONE
C
C     THIS IS SUBROUTINE GOTF.FOR.
C     CALLED BY CMDER FOR COMMAND GOTF
C     THIS DOES GEOMETRICAL OTF CALCULATIONS
C
          REAL*8 MINFREQ,ZMIN,DELTAZ
     1    ,FREQ,ZMAX,OLDDZ,OLDDF,MTF,PPHAS
C
          CHARACTER UNIT*11
C
          REAL GDTAF(1:101),GDTAM(1:101,1:20),GDTAP(1:101,1:20)
          REAL*8 MTHETA1,DELFREQ,DELZ,MAXFREQ,MTHETA2,FREQ1,FREQ2
          REAL*8 AL,INCORFACT,CUTFR
C
          INTEGER ENN,IG,IIG
          COMMON/GII/IIG
C
          LOGICAL MULTIOTF
          COMMON/OTFMULTI/MULTIOTF
C
          COMMON/GMTFPASS/ENN,GDTAF,GDTAM,GDTAP,MTHETA1,
     1    DELZ,DELFREQ,MAXFREQ,MTHETA2
C
          LOGICAL EXTGMTF1,EXTGMTF2,ERROR
C
          COMMON/GMTFEXT/EXTGMTF1,EXTGMTF2
C
          LOGICAL EXTGLSF,LSFCENT
C
          COMMON/GLSFEXT/EXTGLSF,LSFCENT
C
          INTEGER I,OLDOUT
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datspd.inc'
          INCLUDE 'datsp1.inc'
          CALL SORTFIELDS
          IF(SYSTEM1(6).EQ.1.0D0) AL=DABS(ALENS(3,NEWOBJ))*25.4D0
          IF(SYSTEM1(6).EQ.2.0D0) AL=DABS(ALENS(3,NEWOBJ))*10.0D0
          IF(SYSTEM1(6).EQ.3.0D0) AL=DABS(ALENS(3,NEWOBJ))
          IF(SYSTEM1(6).EQ.4.0D0) AL=DABS(ALENS(3,NEWOBJ))*1000.0D0
C
C     DETERMINE NEAR FOR FAR
          IF(NEAR_FAR.EQ.0) NEAR=.TRUE.
          IF(NEAR_FAR.EQ.1) NEAR=.FALSE.
          IF(NEAR.AND.SPACEBALL.EQ.1.AND.AL.GT.
     1    1.0D5) THEN
              OUTLYNE='NO GOTF CAN BE PERFORMED WHEN SPACE IS SET TO "O"'
              CALL SHOWIT(1)
              OUTLYNE='WITH "NEAR" AND WITH AN OBJECT DISTANCE'
              CALL SHOWIT(1)
              OUTLYNE='GREATER THAN 1.0D5 MILLIMETERS.'
              CALL SHOWIT(1)
              OUTLYNE='NO ACTION TAKEN'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
          ERROR=.FALSE.
          CALL CUTTOFF(FREQ1,FREQ2,ERROR)
          IF(ERROR) THEN
              OUTLYNE='ERROR IN OBJECT/IMAGE SPACE FREQUENCY RELATIONSHIP'
              CALL SHOWIT(1)
              OUTLYNE='CALCULATION OCCURED'
              CALL SHOWIT(1)
              OUTLYNE='NO ACTION TAKEN'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
C     FREQ1 IS THE IMAGE  SPACE CUTOFF FREQ
C     FREQ2 IS THE OBJECT SPACE CUTOFF FREQ
          IF(SPACEBALL.EQ.1) THEN
              CUTFR=FREQ2
          ELSE
              CUTFR=FREQ1
          END IF
C     CUTFR HAS BEEN COMPUTED
C
          INCORFACT=1.0D0
          IF(SPACEBALL.EQ.1)
     1    INCORFACT=(FREQ1/FREQ2)
C
          EXTGMTF1=.FALSE.
          EXTGMTF2=.FALSE.
C
C     COMMAND STRUCTURE
C     WC IS GOTF
C     WQ MAY BE NONE or FOCUS or FREQ or ACC
C     IF SQ = 0 THEN COMMAND TAKES NW1 THROUGH NW4
C     NW1=MAXFREQ (MAXIMUM FREQUENCY) MINIMUM FREQ=0
C     NW2=DELFREQ (DELTA FREQUENCY)
C     NW3=MTHETA1 OR 2 (IN DEGREES)
C     NW4=DELZEE (FOCUS SHIFT)
C     ALL ARE REQUIRED AND STI IS NOT SUPPORTED
C
          IF(WQ.EQ.'MFREQ') THEN
              MFREQ=.TRUE.
              WQ='        '
              SQ=0
          ELSE
              MFREQ=.FALSE.
          END IF
C
          IF(STI.EQ.1) THEN
              OUTLYNE='"?" HAS NO MEANING WITH "GOTF"'
              CALL SHOWIT(1)
              OUTLYNE='NO ACTION TAKEN'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SQ.EQ.1) THEN
              IF(WQ.NE.'ACC'.AND.WQ.NE.'TFOCUS'.AND.WQ.NE
     1        .'TFREQ') THEN
                  OUTLYNE='INVALID QUALIFIER WORD USED WITH "GOTF"'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
C
          IF(WQ.EQ.'ACC') THEN
              IF(DF1.EQ.1) THEN
                  OUTLYNE='"GOTF ACC" REQUIRES EXPLICIT NUMERIC WORD #1 INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
          IF(SQ.EQ.0) THEN
              IF(DF1.EQ.1) THEN
                  W1=CUTFR
              END IF
              IF(DF2.EQ.1) THEN
                  W2=W1/10.0D0
              ELSE
              END IF
              IF(DF3.EQ.1) THEN
                  W3=0.0D0
              ELSE
              END IF
              IF(DF4.EQ.1) THEN
                  W4=0.0D0
              ELSE
              END IF
              IF(S5.EQ.1) THEN
                  OUTLYNE='"GOTF" TAKES NO NUMERIC WORD #5 INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
          IF(WQ.EQ.'ACC') THEN
              IF(DF1.EQ.1) THEN
                  OUTLYNE='"GOTF ACC"'
                  CALL SHOWIT(1)
                  OUTLYNE='REQUIRE EXPLICIT NUMERIC WORD #1 INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(DF2.EQ.1) THEN
                  W2=0.0D0
              ELSE
              END IF
              IF(DF3.EQ.1) THEN
                  W3=0.0D0
              ELSE
              END IF
              IF(S4.EQ.1) THEN
                  OUTLYNE='"GOTF ACC" TAKE NO NUMERIC WORD #4 INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(S5.EQ.1) THEN
                  OUTLYNE='"GOTF ACC" TAKE NO NUMERIC WORD #5 INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
          IF(WQ.EQ.'TFOCUS'.OR.WQ.EQ.'TFREQ') THEN
              IF(DF1.EQ.1) THEN
                  OUTLYNE='"GOTF TFOCUS"'
                  CALL SHOWIT(1)
                  OUTLYNE='REQUIRE EXPLICIT NUMERIC WORD #1 INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(DF2.EQ.1) THEN
                  OUTLYNE='"GOTF TFOCUS"'
                  CALL SHOWIT(1)
                  OUTLYNE='REQUIRE EXPLICIT NUMERIC WORD #2 INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(DF3.EQ.1) THEN
                  OUTLYNE='"GOTF TFOCUS"'
                  CALL SHOWIT(1)
                  OUTLYNE='REQUIRE EXPLICIT NUMERIC WORD #3 INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(DF4.EQ.1) THEN
                  W4=DABS(W3-W2)/10.0D0
              END IF
              IF(DF5.EQ.1) THEN
                  W5=0.0D0
              END IF
          END IF
          IF(WQ.EQ.'TFREQ') THEN
              IF(DF1.EQ.1) THEN
                  W1=0.0D0
              ELSE
              END IF
              IF(DF2.EQ.1) THEN
                  OUTLYNE='"GOTF TFREQ"'
                  CALL SHOWIT(1)
                  OUTLYNE='REQUIRE EXPLICIT NUMERIC WORD #2 INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(DF3.EQ.1) THEN
                  OUTLYNE='"GOTF TFREQ"'
                  CALL SHOWIT(1)
                  OUTLYNE='REQUIRE EXPLICIT NUMERIC WORD #3 INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(DF4.EQ.1) THEN
                  W4=(W3-W2)/10.0D0
              END IF
              IF(DF5.EQ.1) THEN
                  W5=0.0D0
              END IF
          END IF
C
C     ASSIGN VALUES HERE
          IF(SQ.EQ.0) THEN
              MAXFREQ=W1*INCORFACT
              DELFREQ=W2*INCORFACT
              MTHETA1=W3
              DELZ=W4
          ELSE
          END IF
          IF(WQ.EQ.'ACC') THEN
              FREQ=W1*INCORFACT
              MTHETA1=W2
              DELZ=W3
          ELSE
          END IF
          IF(WQ.EQ.'TFOCUS') THEN
              FREQ=W1*INCORFACT
              ZMIN=W2
              ZMAX=W3
              DELZ=W4
              MTHETA1=W5
          ELSE
          END IF
          IF(WQ.EQ.'TFREQ') THEN
              DELZ=W1*INCORFACT
              MINFREQ=W2*INCORFACT
              MAXFREQ=W3*INCORFACT
              DELFREQ=W4*INCORFACT
              MTHETA1=W5
          ELSE
          END IF
C
C     VALUES ASSIGNED TO VARIABLES
C
C     SANITY CHECK NUMERIC INPUT
C     FREQ
          IF(WQ.EQ.'TFOCUS'.OR.WQ.EQ.'ACC') THEN
              IF(FREQ.LT.0.0D0) THEN
                  OUTLYNE='A NEGATIVE FREQUENCY MAY NOT BE REQUESTED'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
          IF(WQ.EQ.'TFREQ'.OR.SQ.EQ.0) THEN
C     MAXFREQ
              IF(MAXFREQ.LT.0.0D0) THEN
                  OUTLYNE='A NEGATIVE MAXIMUM FREQUENCY MAY NOT BE REQUESTED'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
          IF(WQ.EQ.'TFREQ') THEN
C     MINFREQ
              IF(MINFREQ.LT.0.0D0) THEN
                  OUTLYNE='A NEGATIVE MINIMUM FREQUENCY MAY NOT BE REQUESTED'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
          IF(SQ.EQ.0.OR.WQ.EQ.'TFREQ') THEN
C     DELFREQ
              IF(DELFREQ.LT.0.0D0) THEN
                  OUTLYNE='A NEGATIVE DELTA FREQUENCY MAY NOT BE REQUESTED'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
C     MTHETA1
          IF(MTHETA1.LT.0.0D0) THEN
              OUTLYNE='THETA MAY NOT BE NEGATIVE'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(MTHETA1.GT.360.0D0) THEN
              OUTLYNE='THETA MAY NOT BE GREATER THAN 360 DEGREES'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
          IF(SQ.EQ.0) THEN
              IF(DELFREQ.GT.MAXFREQ) THEN
                  OUTLYNE='THE DELTA FREQUENCY MAY NOT BE GREATER THAN THE'
                  CALL SHOWIT(1)
                  OUTLYNE='MAXIMUM FREQUENCY'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
          IF(WQ.EQ.'TFREQ') THEN
              IF(DELFREQ.GT.MAXFREQ) THEN
                  OUTLYNE='THE DELTA FREQUENCY MAY NOT BE GREATER THAN THE'
                  CALL SHOWIT(1)
                  OUTLYNE='MAXIMUM FREQUENCY'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
          IF(WQ.EQ.'TFOCUS') THEN
              IF(ZMIN.GT.ZMAX) THEN
                  OUTLYNE='ZMIN MAY NOT BE GREATER THAN ZMAX'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(DABS(ZMAX-ZMIN).LE.DABS(DELZ)) THEN
                  OUTLYNE='DELTA Z MAY NOT BE GREATER THAN (ZMAX-ZMIN)'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
          IF(WQ.EQ.'TFREQ') THEN
              IF(MINFREQ.GT.MAXFREQ) THEN
                  OUTLYNE='THE MINIMUM FREQUENCY MAY NOT BE GREATER THAN'
                  CALL SHOWIT(1)
                  OUTLYNE='THE MAXIMIM FREQUENCY'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(DABS(MAXFREQ-MINFREQ).LT.DELFREQ) THEN
                  OUTLYNE='THE DELTA FREQUENCY MAY NOT BE GREATER THAN'
                  CALL SHOWIT(1)
                  OUTLYNE='THE (MAXIMIM FREQUENCY-MINIMUM FREQUENCY)'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
C
          IF(SYSTEM1(30).EQ.3.0D0.OR.SYSTEM1(30).EQ.4.0D0) THEN
C     MODE AFOCAL OR UAFOCAL
              IF(WQ.EQ.'TFOCUS') THEN
                  OUTLYNE=
     1            '"GOTF TFOCUS" NOT AVAILABLE IN MODES AFOCAL AND UAFOCAL'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(DELZ.NE.0.0D0) THEN
                  OUTLYNE=
     1            'NON-ZERO DELTA-Z VALUES NOT ALLOWED IN MODES AFOCAL AND UAFOCAL'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          ELSE
C     FOCAL OR UFOCAL
          END IF
C     NOW FIX DELZ AND DELFREQ SO THEY ARE EVEN MULTIPLES OF RANGES
C     REQUESTED.
          IF(SQ.EQ.0) THEN
              OLDDF=DELFREQ
              ENN=NINT(MAXFREQ/DELFREQ)
              IF(ENN.GT.100) THEN
                  WRITE(OUTLYNE,11)
                  CALL SHOWIT(0)
                  ENN=100
              END IF
              DELFREQ=MAXFREQ/DBLE(ENN)
C     PRINT MESSAGE TO THAT EFFECT
              IF(REAL(OLDDF).NE.REAL(DELFREQ)) THEN
                  WRITE(OUTLYNE,10) DELFREQ
                  CALL SHOWIT(0)
              END IF
          ELSE
          END IF
          IF(WQ.EQ.'TFOCUS') THEN
              OLDDZ=DELZ
              ENN=NINT(((ZMAX-ZMIN)/DELZ))
              DELZ=(ZMAX-ZMIN)/DBLE(ENN)
C     PRINT MESSAGE TO THAT EFFECT
              WRITE(OUTLYNE,20) DELZ
              CALL SHOWIT(0)
              IF(REAL(OLDDZ).NE.REAL(DELZ)) THEN
                  WRITE(OUTLYNE,10) DELZ
                  CALL SHOWIT(0)
              END IF
          ELSE
          END IF
          IF(WQ.EQ.'TFREQ') THEN
              OLDDF=DELFREQ
              ENN=NINT(((MAXFREQ-MINFREQ)/DELFREQ))
              DELFREQ=(MAXFREQ-MINFREQ)/DBLE(ENN)
C     PRINT MESSAGE TO THAT EFFECT
              IF(REAL(OLDDF).NE.REAL(DELFREQ)) THEN
                  WRITE(OUTLYNE,10) DELFREQ
                  CALL SHOWIT(0)
              END IF
          END IF
C
          IF(.NOT.SPDEXT) THEN
              OUTLYNE=
     1        'NO SPOT DIAGRAM EXISTS. GOTF REQUIRES A SPOT DIAGRAM'
              CALL SHOWIT(1)
              OUTLYNE='NO ACTION TAKEN'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SYSTEM1(6).EQ.1.0D0) UNIT='INCH'
          IF(SYSTEM1(6).EQ.2.0D0) UNIT='CENTIMETER'
          IF(SYSTEM1(6).EQ.3.0D0) UNIT='MILLIMETER'
          IF(SYSTEM1(6).EQ.4.0D0) UNIT='METER'
C
C     NOW FIRST DO THE CASE OF WQ = ACC
C
          IF(WQ.EQ.'ACC') THEN
              OLDOUT=OUT
              SAVE_KDP(6)=SAVEINPT(6)
C     SET OUTPUT TO NULL DEVICE
              WC='OUTPUT'
              WQ='NULL'
              SQ=1
              SST=0
              S1=0
              S2=0
              S3=0
              S4=0
              S5=0
              SN=0
              DF1=1
              DF2=1
              DF3=1
              DF4=1
              DF5=1
              STI=0
              CALL CONTRO
C
C     NOW COMPUTE THE APPROPRIATE LINE SPREAD FUNCTION USING THE
C     CENT OPTION TO CENTER THE LSF AT THE SPOT CENTROID. APPLY
C     THE ORIENTATION AND DELTA Z AS APPROPRIATE.
              IF(DELZ.EQ.0.0D0) THEN
                  WC='LSF'
                  WQ='CENT'
                  SQ=1
                  SST=0
                  S1=1
                  S2=0
                  S3=1
                  S4=0
                  S5=0
                  SN=1
                  DF1=0
                  DF2=1
                  DF3=0
                  DF4=1
                  DF5=1
                  STI=0
                  W1=101.0D0
                  W3=MTHETA1
              ELSE
                  WC='LSF'
                  WQ='CENT'
                  SQ=1
                  SST=0
                  S1=1
                  S2=1
                  S3=1
                  S4=0
                  S5=0
                  SN=1
                  DF1=0
                  DF2=0
                  DF3=0
                  DF4=1
                  DF5=1
                  STI=0
                  W1=101.0D0
                  W2=DELZ
                  W3=MTHETA1
              END IF
              CALL CONTRO
              EXTGLSF=.FALSE.
C
C     NOW SET OUT BACK TO TP
              WC='OUT'
              IF(OLDOUT.EQ.6) WQ='TP'
              IF(OLDOUT.EQ.7) WQ='LP'
              IF(OLDOUT.EQ.8) WQ='CP'
              IF(OLDOUT.EQ.9) WQ='ED'
              IF(OLDOUT.EQ.10) WQ='PU'
              IF(OLDOUT.EQ.98) WQ='NULL'
              IF(OLDOUT.EQ.97) WQ='FILE'
              OFILN=LASTFIL
              SQ=1
              SST=0
              S1=0
              S2=0
              S3=0
              S4=0
              S5=0
              SN=0
              DF1=1
              DF2=1
              DF3=1
              DF4=1
              DF5=1
              STI=0
              CALL CONTRO
              REST_KDP(6)=RESTINPT(6)
C
C     LINE SPREAD FUNCTION EXISTS NOW. NOW CALCULATE THE MTF
C     AT FREQUENCY F.
C
              IF(SYSTEM1(30).GE.3.0D0) FREQ=FREQ*1000.0D0
              IF(SYSTEM1(30).LE.2.0D0) THEN
                  IF(SYSTEM1(6).EQ.1.0D0) FREQ=FREQ*25.4D0
                  IF(SYSTEM1(6).EQ.2.0D0) FREQ=FREQ*10.0D0
                  IF(SYSTEM1(6).EQ.3.0D0) FREQ=FREQ
                  IF(SYSTEM1(6).EQ.4.0D0) FREQ=FREQ*1000.0D0
              ELSE
              END IF
              CALL GEOMTF(FREQ,MTF,PPHAS)
C
              IF(SYSTEM1(30).GE.3.0D0) FREQ=FREQ/1000.0D0
              IF(SYSTEM1(30).LE.2.0D0) THEN
                  IF(SYSTEM1(6).EQ.1.0D0) FREQ=FREQ/25.4D0
                  IF(SYSTEM1(6).EQ.2.0D0) FREQ=FREQ/10.0D0
                  IF(SYSTEM1(6).EQ.3.0D0) FREQ=FREQ
                  IF(SYSTEM1(6).EQ.4.0D0) FREQ=FREQ/1000.0D0
              ELSE
              END IF
C
              REG(40)=REG(9)
              REG(10)=PPHAS
              REG(9)=MTF
              RETURN
          ELSE
C     WQ NOT ACC
          END IF
C
C     NOW DO THE CASE OF WQ = '        '
C
C
          IF(DF3.EQ.1) IIG=2
          IF(DF3.EQ.0) IIG=1
          IF(SQ.EQ.0) THEN
              DO IG=1,IIG
C     JUST GOTF
                  OLDOUT=OUT
                  SAVE_KDP(6)=SAVEINPT(6)
C     SET OUTPUT TO NULL DEVICE
                  WC='OUTPUT'
                  WQ='NULL'
                  SQ=1
                  SST=0
                  S1=0
                  S2=0
                  S3=0
                  S4=0
                  S5=0
                  SN=0
                  DF1=1
                  DF2=1
                  DF3=1
                  DF4=1
                  DF5=1
                  STI=0
                  CALL CONTRO
C
C     NOW COMPUTE THE APPROPRIATE LINE SPREAD FUNCTION USING THE
C     CENT OPTION TO CENTER THE LSF AT THE SPOT CENTROID. APPLY
C     THE ORIENTATION AND DELTA Z AS APPROPRIATE.
                  IF(DELZ.EQ.0.0D0) THEN
                      WC='LSF'
                      WQ='CENT'
                      SQ=1
                      SST=0
                      S1=1
                      S2=0
                      S3=1
                      S4=0
                      S5=0
                      SN=1
                      DF1=0
                      DF2=1
                      DF3=0
                      DF4=1
                      DF5=1
                      STI=0
                      W1=101.0D0
                      IF(IIG.EQ.2) THEN
                          IF(IG.EQ.1) MTHETA1=0.0D0
                          IF(IG.EQ.2) MTHETA2=90.0D0
                          IF(IG.EQ.1) W3=MTHETA1
                          IF(IG.EQ.2) W3=MTHETA2
                      ELSE
                          W3=MTHETA1
                      END IF
                  ELSE
                      WC='LSF'
                      WQ='CENT'
                      SQ=1
                      SST=0
                      S1=1
                      S2=1
                      S3=1
                      S4=0
                      S5=0
                      SN=1
                      DF1=0
                      DF2=0
                      DF3=0
                      DF4=1
                      DF5=1
                      STI=0
                      W1=101.0D0
                      W2=DELZ
                      IF(IIG.EQ.2) THEN
                          IF(IG.EQ.1) MTHETA1=0.0D0
                          IF(IG.EQ.2) MTHETA2=90.0D0
                          IF(IG.EQ.1) W3=MTHETA1
                          IF(IG.EQ.2) W3=MTHETA2
                      ELSE
                          W3=MTHETA1
                      END IF
                  END IF
                  CALL CONTRO
                  EXTGLSF=.FALSE.
C
C     NOW SET OUT BACK TO TP
                  WC='OUT'
                  IF(OLDOUT.EQ.6) WQ='TP'
                  IF(OLDOUT.EQ.7) WQ='LP'
                  IF(OLDOUT.EQ.8) WQ='CP'
                  IF(OLDOUT.EQ.9) WQ='ED'
                  IF(OLDOUT.EQ.10) WQ='PU'
                  IF(OLDOUT.EQ.98) WQ='NULL'
                  IF(OLDOUT.EQ.97) WQ='FILE'
                  OFILN=LASTFIL
                  SQ=1
                  SST=0
                  S1=0
                  S2=0
                  S3=0
                  S4=0
                  S5=0
                  SN=0
                  DF1=1
                  DF2=1
                  DF3=1
                  DF4=1
                  DF5=1
                  STI=0
                  CALL CONTRO
                  REST_KDP(6)=RESTINPT(6)
C
C     LINE SPREAD FUNCTION EXISTS NOW. NOW CALCULATE THE MTF FROM
C     FREQUENCY 0 TO Fmax FREQUENCY
C
 100              FORMAT('GEOMETRICAL OPTICAL TRANSFER FUNCTION')
 200              FORMAT(
     1            'THROUGH FOCUS GEOMETRICAL OPTICAL TRANSFER FUNCTION')
 105              FORMAT('DE-FOCUS = ',G13.6,1X,A6)
 107              FORMAT('FREQUENCY = ',G13.6,1X,'(lp/mm)')
 106              FORMAT('TARGET ORIENTATION ANGLE = ',G13.6,1X,'DEGREES')
 101              FORMAT(' ')
 102              FORMAT('FREQ.-(lp/mm)',7X,'MODULUS',11X,'PHASE(DEG)')
 108              FORMAT(' DEL(Z)-(IN) ',7X,'MODULUS',11X,'PHASE(DEG)')
 109              FORMAT(' DEL(Z)-(CM) ',7X,'MODULUS',11X,'PHASE(DEG)')
 110              FORMAT(' DEL(Z)-(MM) ',7X,'MODULUS',11X,'PHASE(DEG)')
 111              FORMAT(' DEL(Z)-(M)  ',7X,'MODULUS',11X,'PHASE(DEG)')
 103              FORMAT('FREQ.-(lp/mrad)',5X,'MODULUS',11X,'PHASE(DEG)')
                  WRITE(OUTLYNE,101)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,100)
                  CALL SHOWIT(0)
                  IF(DELZ.NE.0.0D0.AND.SYSTEM1(30).LE.2.0D0) THEN
                      WRITE(OUTLYNE,105) DELZ,UNIT
                      CALL SHOWIT(0)
                  END IF
                  IF(IG.EQ.0.OR.IG.EQ.1) THEN
                      WRITE(OUTLYNE,106) MTHETA1
                      CALL SHOWIT(0)
                  END IF
                  IF(IG.EQ.2) THEN
                      WRITE(OUTLYNE,106) MTHETA2
                      CALL SHOWIT(0)
                  END IF
                  IF(SYSTEM1(30).LE.2.0D0.AND.SPACEBALL.EQ.2.OR.
     1            SPACEBALL.EQ.1.AND.NEAR) THEN
                      WRITE(OUTLYNE,102)
                      CALL SHOWIT(0)
                  END IF
                  IF(SYSTEM1(30).GE.3.0D0.AND.SPACEBALL.EQ.2.OR.
     1            SPACEBALL.EQ.1.AND..NOT.NEAR) THEN
                      WRITE(OUTLYNE,103)
                      CALL SHOWIT(0)
                  END IF
C
                  FREQ=0.0D0
                  IF(IIG.EQ.1) EXTGMTF1=.TRUE.
                  IF(IIG.EQ.2.AND.IG.EQ.1) EXTGMTF1=.TRUE.
                  IF(IIG.EQ.2.AND.IG.EQ.2) EXTGMTF2=.TRUE.
                  DO I=0,ENN
C
                      IF(SYSTEM1(30).GE.3.0D0) FREQ=FREQ*1000.0D0
                      IF(SYSTEM1(30).LE.2.0D0) THEN
                          IF(SYSTEM1(6).EQ.1.0D0) FREQ=FREQ*25.4D0
                          IF(SYSTEM1(6).EQ.2.0D0) FREQ=FREQ*10.0D0
                          IF(SYSTEM1(6).EQ.3.0D0) FREQ=FREQ
                          IF(SYSTEM1(6).EQ.4.0D0) FREQ=FREQ*1000.0D0
                      ELSE
                      END IF

                      CALL GEOMTF(FREQ,MTF,PPHAS)
C     REMEMBER DATA FOR PLOTS
C
                      IF(SYSTEM1(30).GE.3.0D0) FREQ=FREQ/1000.0D0
                      IF(SYSTEM1(30).LE.2.0D0) THEN
                          IF(SYSTEM1(6).EQ.1.0D0) FREQ=FREQ/25.4D0
                          IF(SYSTEM1(6).EQ.2.0D0) FREQ=FREQ/10.0D0
                          IF(SYSTEM1(6).EQ.3.0D0) FREQ=FREQ
                          IF(SYSTEM1(6).EQ.4.0D0) FREQ=FREQ/1000.0D0
                      ELSE
                      END IF
C
C
                      WRITE(OUTLYNE,104)FREQ/INCORFACT,MTF,PPHAS
                      CALL SHOWIT(0)
C
                      IF(DABS(PPHAS).LT.1.0D-6) PPHAS=0.0D0
                      IF(DABS(MTF).LT.1.0D-6) MTF=0.0D0
                      GDTAF(I+1)=REAL(FREQ/INCORFACT)
                      IF(IG.EQ.1) GDTAM(I+1,((OTFPAIR*2)-1))=REAL(MTF)
                      IF(IG.EQ.1) GDTAP(I+1,((OTFPAIR*2)-1))=REAL(PPHAS)
                      IF(IG.EQ.2) GDTAM(I+1,(OTFPAIR*2))=REAL(MTF)
                      IF(IG.EQ.2) GDTAP(I+1,(OTFPAIR*2))=REAL(PPHAS)
                      FREQ=FREQ+DELFREQ
C
                  END DO
C     THE NEXT END DO IS THE END OF THE DOUBLE LOOP FOR BOTH
C     TARGET  ORIENTATIONS
              END DO
              RETURN
          ELSE
C     WQ NOT '        '
          END IF
          IF(WQ.EQ.'TFREQ') THEN
              OLDOUT=OUT
              SAVE_KDP(6)=SAVEINPT(6)
C     SET OUTPUT TO NULL DEVICE
              WC='OUTPUT'
              WQ='NULL'
              SQ=1
              SST=0
              S1=0
              S2=0
              S3=0
              S4=0
              S5=0
              SN=0
              DF1=1
              DF2=1
              DF3=1
              DF4=1
              DF5=1
              STI=0
              CALL CONTRO
C
C     NOW COMPUTE THE APPROPRIATE LINE SPREAD FUNCTION USING THE
C     CENT OPTION TO CENTER THE LSF AT THE SPOT CENTROID. APPLY
C     THE ORIENTATION AND DELTA Z AS APPROPRIATE.
              IF(DELZ.EQ.0.0D0) THEN
                  WC='LSF'
                  WQ='CENT'
                  SQ=1
                  SST=0
                  S1=1
                  S2=0
                  S3=1
                  S4=0
                  S5=0
                  SN=1
                  DF1=0
                  DF2=1
                  DF3=0
                  DF4=1
                  DF5=1
                  STI=0
                  W1=101.0D0
                  W3=MTHETA1
              ELSE
                  WC='LSF'
                  WQ='CENT'
                  SQ=1
                  SST=0
                  S1=1
                  S2=1
                  S3=1
                  S4=0
                  S5=0
                  SN=1
                  DF1=0
                  DF2=0
                  DF3=0
                  DF4=1
                  DF5=1
                  STI=0
                  W1=101.0D0
                  W2=DELZ
                  W3=MTHETA1
              END IF
              CALL CONTRO
              EXTGLSF=.FALSE.
C
C     NOW SET OUT BACK TO TP
              WC='OUT'
              IF(OLDOUT.EQ.6) WQ='TP'
              IF(OLDOUT.EQ.7) WQ='LP'
              IF(OLDOUT.EQ.8) WQ='CP'
              IF(OLDOUT.EQ.9) WQ='ED'
              IF(OLDOUT.EQ.10) WQ='PU'
              IF(OLDOUT.EQ.98) WQ='NULL'
              IF(OLDOUT.EQ.97) WQ='FILE'
              OFILN=LASTFIL
              SQ=1
              SST=0
              S1=0
              S2=0
              S3=0
              S4=0
              S5=0
              SN=0
              DF1=1
              DF2=1
              DF3=1
              DF4=1
              DF5=1
              STI=0
              CALL CONTRO
              REST_KDP(6)=RESTINPT(6)
C
C     LINE SPREAD FUNCTION EXISTS NOW. NOW CALCULATE THE MTF FROM
C     FREQUENCY Fmin TO Fmax
C
              WRITE(OUTLYNE,101)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,100)
              CALL SHOWIT(0)
              IF(DELZ.NE.0.0D0.AND.SYSTEM1(30).LE.2.0D0) THEN
                  WRITE(OUTLYNE,105) DELZ,UNIT
                  CALL SHOWIT(0)
              END IF
              WRITE(OUTLYNE,106) MTHETA1
              CALL SHOWIT(0)
              IF(SYSTEM1(30).LE.2.0D0.AND.SPACEBALL.EQ.2.OR.
     1        SPACEBALL.EQ.1.AND.NEAR) THEN
                  WRITE(OUTLYNE,102)
                  CALL SHOWIT(0)
              END IF
              IF(SYSTEM1(30).GE.3.0D0.AND.SPACEBALL.EQ.2.OR.
     1        SPACEBALL.EQ.1.AND..NOT.NEAR) THEN
                  WRITE(OUTLYNE,103)
                  CALL SHOWIT(0)
              END IF
C
              FREQ=MINFREQ
              DO I=0,ENN
C
                  IF(SYSTEM1(30).GE.3.0D0) FREQ=FREQ*1000.0D0
                  IF(SYSTEM1(30).LE.2.0D0) THEN
                      IF(SYSTEM1(6).EQ.1.0D0) FREQ=FREQ*25.4D0
                      IF(SYSTEM1(6).EQ.2.0D0) FREQ=FREQ*10.0D0
                      IF(SYSTEM1(6).EQ.3.0D0) FREQ=FREQ
                      IF(SYSTEM1(6).EQ.4.0D0) FREQ=FREQ*1000.0D0
                  ELSE
                  END IF

                  CALL GEOMTF(FREQ,MTF,PPHAS)
C
                  IF(SYSTEM1(30).GE.3.0D0) FREQ=FREQ/1000.0D0
                  IF(SYSTEM1(30).LE.2.0D0) THEN
                      IF(SYSTEM1(6).EQ.1.0D0) FREQ=FREQ/25.4D0
                      IF(SYSTEM1(6).EQ.2.0D0) FREQ=FREQ/10.0D0
                      IF(SYSTEM1(6).EQ.3.0D0) FREQ=FREQ
                      IF(SYSTEM1(6).EQ.4.0D0) FREQ=FREQ/1000.0D0
                  ELSE
                  END IF
C
 104              FORMAT(G13.6,5X,G13.6,7X,G13.6)
                  IF(DABS(PPHAS).LT.1.0D-6) PPHAS=0.0D0
                  IF(DABS(MTF).LT.1.0D-6) MTF=0.0D0
                  WRITE(OUTLYNE,104)FREQ/INCORFACT,MTF,PPHAS
                  CALL SHOWIT(0)
C
                  FREQ=FREQ+DELFREQ
C
              END DO
              RETURN
          ELSE
C     WQ NOT 'TFREQ'
          END IF
          IF(WQ.EQ.'TFOCUS') THEN
              WRITE(OUTLYNE,101)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,200)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,107) FREQ/INCORFACT
              CALL SHOWIT(0)
              WRITE(OUTLYNE,106) MTHETA1
              CALL SHOWIT(0)
              IF(SYSTEM1(6).EQ.1.0D0) WRITE(OUTLYNE,108)
              CALL SHOWIT(0)
              IF(SYSTEM1(6).EQ.2.0D0) WRITE(OUTLYNE,109)
              CALL SHOWIT(0)
              IF(SYSTEM1(6).EQ.3.0D0) WRITE(OUTLYNE,110)
              CALL SHOWIT(0)
              IF(SYSTEM1(6).EQ.4.0D0) WRITE(OUTLYNE,111)
              CALL SHOWIT(0)
              DELTAZ=DELZ
              DELZ=ZMIN
              DO I=0,ENN
                  OLDOUT=OUT
                  SAVE_KDP(6)=SAVEINPT(6)
C     SET OUTPUT TO NULL DEVICE
                  WC='OUTPUT'
                  WQ='NULL'
                  SQ=1
                  SST=0
                  S1=0
                  S2=0
                  S3=0
                  S4=0
                  S5=0
                  SN=0
                  DF1=1
                  DF2=1
                  DF3=1
                  DF4=1
                  DF5=1
                  STI=0
                  CALL CONTRO
C
C     NOW COMPUTE THE APPROPRIATE LINE SPREAD FUNCTION USING THE
C     CENT OPTION TO CENTER THE LSF AT THE SPOT CENTROID. APPLY
C     THE ORIENTATION AND DELTA Z AS APPROPRIATE.
                  IF(DELZ.EQ.0.0D0) THEN
                      WC='LSF'
                      WQ='CENT'
                      SQ=1
                      SST=0
                      S1=1
                      S2=0
                      S3=1
                      S4=0
                      S5=0
                      SN=1
                      DF1=0
                      DF2=1
                      DF3=0
                      DF4=1
                      DF5=1
                      STI=0
                      W1=101.0D0
                      W3=MTHETA1
                  ELSE
                      WC='LSF'
                      WQ='CENT'
                      SQ=1
                      SST=0
                      S1=1
                      S2=1
                      S3=1
                      S4=0
                      S5=0
                      SN=1
                      DF1=0
                      DF2=0
                      DF3=0
                      DF4=1
                      DF5=1
                      STI=0
                      W1=101.0D0
                      W2=DELZ
                      W3=MTHETA1
                  END IF
                  CALL CONTRO
                  EXTGLSF=.FALSE.
C
C     NOW SET OUT BACK TO TP
                  WC='OUT'
                  IF(OLDOUT.EQ.6) WQ='TP'
                  IF(OLDOUT.EQ.7) WQ='LP'
                  IF(OLDOUT.EQ.8) WQ='CP'
                  IF(OLDOUT.EQ.9) WQ='ED'
                  IF(OLDOUT.EQ.10) WQ='PU'
                  IF(OLDOUT.EQ.98) WQ='NULL'
                  IF(OLDOUT.EQ.97) WQ='FILE'
                  OFILN=LASTFIL
                  SQ=1
                  SST=0
                  S1=0
                  S2=0
                  S3=0
                  S4=0
                  S5=0
                  SN=0
                  DF1=1
                  DF2=1
                  DF3=1
                  DF4=1
                  DF5=1
                  STI=0
                  CALL CONTRO
                  REST_KDP(6)=RESTINPT(6)
C
C     LINE SPREAD FUNCTION EXISTS NOW. NOW CALCULATE THE MTF FROM
C     FREQUENCY Fmin TO Fmax
C
C
                  IF(SYSTEM1(30).GE.3.0D0) FREQ=FREQ*1000.0D0
                  IF(SYSTEM1(30).LE.2.0D0) THEN
                      IF(SYSTEM1(6).EQ.1.0D0) FREQ=FREQ*25.4D0
                      IF(SYSTEM1(6).EQ.2.0D0) FREQ=FREQ*10.0D0
                      IF(SYSTEM1(6).EQ.3.0D0) FREQ=FREQ
                      IF(SYSTEM1(6).EQ.4.0D0) FREQ=FREQ*1000.0D0
                  ELSE
                  END IF

                  CALL GEOMTF(FREQ,MTF,PPHAS)
C
                  IF(SYSTEM1(30).GE.3.0D0) FREQ=FREQ/1000.0D0
                  IF(SYSTEM1(30).LE.2.0D0) THEN
                      IF(SYSTEM1(6).EQ.1.0D0) FREQ=FREQ/25.4D0
                      IF(SYSTEM1(6).EQ.2.0D0) FREQ=FREQ/10.0D0
                      IF(SYSTEM1(6).EQ.3.0D0) FREQ=FREQ
                      IF(SYSTEM1(6).EQ.4.0D0) FREQ=FREQ/1000.0D0
                  END IF
C
                  IF(DABS(PPHAS).LT.1.0D-6) PPHAS=0.0D0
                  IF(DABS(MTF).LT.1.0D-6) MTF=0.0D0
                  WRITE(OUTLYNE,104) DELZ,MTF,PPHAS
                  CALL SHOWIT(0)
C
                  DELZ=DELZ+DELTAZ
C
              END DO
              RETURN
          END IF
          RETURN
 10       FORMAT(
     1    'DELTA FREQUENCY (ADJUSTED FOR INTEGER # OF STEPS) = : ',G14.6)
 11       FORMAT(
     1    'DELTA FREQUENCY WAS ADJUSTED FOR THE MAXIMUN OF 100 STEPS')
 20       FORMAT(
     1    'DELTA Z (ADJUSTED FOR INTEGER # OF STEPS) = : ',G14.6)
      END
      SUBROUTINE LSFSUM(LARGER,M,N)
          IMPLICIT NONE
C     N IS ALWAYS 2, M IS SET BY ILSF
          INTEGER I,J,NUM,M,N
          REAL*8 DEL2,XX(1:21),YY(1:21),XXX(1:43),YYY(1:43)
          REAL*8 POSEXT,NEGEXT,DELEXT,MAXEXT
          INCLUDE 'datmai.inc'
          INCLUDE 'datspd.inc'
          INCLUDE 'datsp1.inc'
C
          REAL*8 LARGER
C
          DIMENSION LARGER(M,N)
C
          NUM=21
C
          READ(UNIT=66,REC=1) ILSF
          NEGEXT=1.0D10
          POSEXT=-1.0D10
          DO I=2,ILSF
              READ(UNIT=66,REC=I) LARGER(I-1,1),LARGER(I-1,2)
              IF(REAL(LARGER(I-1,2)).LE.REAL(NEGEXT)) NEGEXT=LARGER(I-1,2)
              IF(REAL(LARGER(I-1,2)).GE.REAL(POSEXT)) POSEXT=LARGER(I-1,2)
          END DO
          MAXEXT=DABS(POSEXT)
          IF(DABS(NEGEXT).GT.DABS(POSEXT)) MAXEXT=DABS(NEGEXT)
          NEGEXT=-MAXEXT
          POSEXT=MAXEXT
          ILSF=ILSF-1
          XXX(1:43)=0.0D0
          YYY(1:43)=0.0D0
          XX(1:21)=0.0D0
          YY(1:21)=0.0D0
C
          DELEXT=DABS((POSEXT-NEGEXT)/DBLE(NUM-1))
          DEL2=DELEXT/2.0D0
C
C     THE XX COORDINATES ARE :
          XX(1)=NEGEXT
          YY(1)=0.0D0
          DO I=2,21
              YY(I)=0.0D0
              XX(I)=XX(I-1)+(DELEXT)
          END DO
C
          DO I=1,ILSF
C     LOOK AT THE ITH LARGER(,2) VALUE AND SEE WHICH OF THE 21
C     BOXES IT FITS IN
C     ALL THE OTHER BOXES
              DO J=1,21
                  IF(REAL(LARGER(I,2)).GT.REAL((XX(J)-DEL2)).AND.
     1            REAL(LARGER(I,2)).LT.REAL((XX(J)+DEL2))) THEN
                      YY(J)=YY(J)+LARGER(I,1)
                  END IF
                  IF(REAL(LARGER(I,2)).EQ.REAL((XX(J)-DEL2)).AND.J.NE.1) THEN
                      YY(J)=YY(J)+(LARGER(I,1)/2.0D0)
                      YY(J-1)=YY(J-1)+(LARGER(I,1)/2.0D0)
                  END IF
                  IF(REAL(LARGER(I,2)).EQ.REAL((XX(J)+DEL2)).AND.J.NE.21) THEN
                      YY(J)=YY(J)+(LARGER(I,1)/2.0D0)
                      YY(J+1)=YY(J+1)+(LARGER(I,1)/2.0D0)
                  END IF
              END DO
          END DO
C     THERE ARE NOW 21 DATA POINTS FROM I=1 TO I=21
C     INTERPOLATE TO 43 DATA POINTS
C
C     END POINTS
          YYY(1)=0.0D0
          YYY(43)=0.0D0
          XXX(1)=XX(1)-(DELEXT/2.0D0)
          XXX(43)=XX(21)+(DELEXT/2.0D0)
          DO I=2,42,2
              J=I/2
              YYY(I)=YY(J)
              XXX(I)=XX(J)
          END DO
          DO I=3,41,2
              YYY(I)=(YYY(I-1)+YYY(I+1))/2.0D0
              XXX(I)=XXX(I-1)+(DELEXT/2.0D0)
          END DO
C
          ILSF=44
          WRITE(UNIT=66,REC=1) ILSF
          DO I=2,44
              WRITE(UNIT=66,REC=I) YYY(I-1),XXX(I-1)
          END DO
          RETURN
      END
      SUBROUTINE LSFSOR(N,LARGER,M1,N1)
          IMPLICIT NONE
          REAL*8 RRA,RRB
          INTEGER L,N,J,IR,I,M1,N1
          INCLUDE 'datmai.inc'
          INCLUDE 'datspd.inc'
          INCLUDE 'datsp1.inc'
C
          REAL*8 LARGER
C
          DIMENSION LARGER(M1,N1)
C
          READ(UNIT=66,REC=1) ILSF
          DO I=2,ILSF
              READ(UNIT=66,REC=I) LARGER(I-1,1),LARGER(I-1,2)
              LARGER(I-1,1)=DBLE(REAL(LARGER(I-1,1)))
              LARGER(I-1,2)=DBLE(REAL(LARGER(I-1,2)))
          END DO
          ILSF=ILSF-1
C
          L=N/2+1
          IR=N
10        CONTINUE
          IF(L.GT.1)THEN
              L=L-1
              RRB=LARGER(L,1)
              RRA=LARGER(L,2)
          ELSE
              RRB=LARGER(IR,1)
              RRA=LARGER(IR,2)
              LARGER(IR,1)=LARGER(1,1)
              LARGER(IR,2)=LARGER(1,2)
              IR=IR-1
              IF(IR.EQ.1)THEN
                  LARGER(1,1) = RRB
                  LARGER(1,2) = RRA
                  ILSF=ILSF+1
                  WRITE(UNIT=66,REC=1) ILSF
                  DO I=2,ILSF
                      WRITE(UNIT=66,REC=I) LARGER(I-1,1),LARGER(I-1,2)
                  END DO
                  RETURN
              ENDIF
          ENDIF
          I=L
          J=L+L
20        IF(J.LE.IR)THEN
              IF(J.LT.IR)THEN
                  IF((LARGER(J,2)).LT.(LARGER(J+1,2))) J=J+1
              ENDIF
              IF((RRA).LT.(LARGER(J,2)))THEN
                  LARGER(I,1)=LARGER(J,1)
                  LARGER(I,2)=LARGER(J,2)
                  I=J
                  J=J+J
              ELSE
                  J=IR+1
              ENDIF
              GO TO 20
          ENDIF
          LARGER(I,1)=RRB
          LARGER(I,2)=RRA
          GO TO 10
      END
      SUBROUTINE LSFPAK(LARGER,M,N)
          IMPLICIT NONE
          INTEGER I,K,M,N
!      REAL*8 RED1,RED2,REDP1,REDP2
          INCLUDE 'datmai.inc'
          INCLUDE 'datspd.inc'
          INCLUDE 'datsp1.inc'
C
          REAL*8 LARGER
          DIMENSION LARGER(M,N)
C
          READ(UNIT=66,REC=1) ILSF
          DO I=2,ILSF
              READ(UNIT=66,REC=I) LARGER(I-1,1),LARGER(I-1,2)
          END DO
          ILSF=ILSF-1
C
 11       CONTINUE
          DO I=1,ILSF-1
              IF(REAL(LARGER(I+1,2)).EQ.REAL(LARGER(I,2))) THEN
                  LARGER(I,1)=LARGER(I,1)+LARGER(I+1,1)
                  LARGER(I,2)=LARGER(I+1,2)
                  LARGER(I+1,1)=0.0D0
                  LARGER(I+1,2)=0.0D0
                  GO TO 10
              ELSE
C     PROCEED
              END IF
          END DO
          ILSF=ILSF+1
          WRITE(UNIT=66,REC=1) ILSF
          DO I=2,ILSF
              WRITE(UNIT=66,REC=I) LARGER(I-1,1),LARGER(I-1,2)
          END DO
          RETURN
 10       CONTINUE
          DO I=1,ILSF
              IF(REAL(LARGER(I,1)).EQ.0.0) THEN
C     COPY EVERY THING UP ONE SPACE
                  DO K=I,ILSF
                      IF(K.NE.ILSF) THEN
                          LARGER(K,1)=LARGER(K+1,1)
                          LARGER(K,2)=LARGER(K+1,2)
                      ELSE
C     K=ILSF, NOTHING TO DO
                      END IF
                  END DO
                  ILSF=ILSF-1
                  GO TO 10
              ELSE
C     DATA OK CONTINUE
              END IF
          END DO
          CONTINUE
          GO TO 11
      END
C SUB LSFLSF.FOR
      SUBROUTINE LSFLSF
C
          IMPLICIT NONE
C
C     CALLED BY CMDER FOR COMMAND LSF
C     THESE DO THE GEOMETRICAL LINE SPREAD FUNCTIONS
C
          REAL*8 ABC,IMAX,THETA,IL1,IL2,
     1    XV,YV,INTEN,IX1,IX2,IY1,IY2,USDEL,NEWW1,
     2    JA,JB,SPDELX,SPDELY,TOPVAL,MTHETA,DELZ
     3    ,XCOORD(1:110),YCOORD(1:110),LSF1,LSF2,LSFP1,LSFP2
C
          LOGICAL EXTGLSF,LSFCENT
C
          COMMON/GLSFEXT/EXTGLSF,LSFCENT
C
          REAL GDTAP(1:102),GDTAV(1:102)
C
          COMMON/CORDITE/XCOORD,YCOORD
C
          INTEGER IE,I,M1,N1,NOPNTS,IKE,ENNL,ALLOERR
C
          CHARACTER J_UN*13
C
          COMMON/GLSFPASS/ENNL,GDTAP,GDTAV,MTHETA,SPDELX,SPDELY,DELZ
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datspd.inc'
          INCLUDE 'datsp1.inc'
C
          REAL*8 LARGER
          DIMENSION LARGER(:,:)
          ALLOCATABLE :: LARGER
          JA=COS_A_ANG
          JB=COS_B_ANG
C
          EXTGLSF=.FALSE.
          LSFCENT=.FALSE.
          ENNL=0
C     STRING INPUT
          IF(SST.EQ.1) THEN
              OUTLYNE=
     1        '"LSF", "LSF (ACC)", "LSF (CACC) AND "LSF (CENT)"'
              CALL SHOWIT(1)
              OUTLYNE='TAKE NO STRING INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SQ.EQ.1) THEN
              IF(WQ.NE.'CENT'.AND.WQ.NE.'ACC'.AND.WQ.NE.'CACC') THEN
                  OUTLYNE=
     1            '"INVALID QUALIFIER USED WITH "LSF"'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
C     NUMERIC WORD #1
          IF(WQ.NE.'CACC'.AND.WQ.NE.'ACC') THEN
C     NUMMERIC WORD 1 IS THE DESIRED NUMBER OF POINTS ACROSS THE LSF
              IF(DF1.EQ.1) W1=21.0D0
              IF(W1.LE.2.0D0) THEN
                  OUTLYNE=
     1            '"LSF AND "LSF CENT"'
                  CALL SHOWIT(1)
                  OUTLYNE=
     1            'REQUIRE NUMERIC WORD 1 INPUT GREATER THAN 2.0'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(W1.GT.101.0D0) THEN
                  OUTLYNE=
     1            '"LSF AND "LSF CENT"'
                  CALL SHOWIT(1)
                  OUTLYNE=
     1            'REQUIRE NUMERIC WORD 1 INPUT LESS THAN OR EQUAL TO 101.0'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          ELSE
C     MUST BE ACC OR CACC
              IF(DF1.EQ.1) THEN
                  OUTLYNE=
     1            '"LSF (ACC)" AND "LSF (CACC)",'
                  CALL SHOWIT(1)
                  OUTLYNE='REQUIRE EXPLICIT NUMERIC WORD #1 INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
          IF(DF2.EQ.1) W2=0.0D0
          IF(DF3.EQ.1) W3=0.0D0
          IF(DF4.EQ.1) W4=0.0D0
          IF(DF5.EQ.1) W5=0.0D0
          MTHETA=W5
          DELZ=W4
C     NUMERIC WORDS 4 AND 5
          IF(WC.EQ.'LSF'.AND.WQ.EQ.'CENT'.OR.WC.EQ.'LSF'.AND.WQ
     1    .EQ.'CACC') THEN
              IF(DF4.EQ.0.OR.DF5.EQ.0) THEN
                  OUTLYNE=
     1            '"LSF (CENT)" AND "LSF (CACC)"'
                  CALL SHOWIT(1)
                  OUTLYNE='TAKE NO NUMERIC WORD 4 OR 5 INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
C       PROCEED
          IF(WQ.EQ.'CENT'.AND.DF2.EQ.0.AND.W2.NE.0.0D0.OR.
     1    WQ.EQ.'CACC'.AND.DF2.EQ.0.AND.W2.NE.0.0D0) THEN
              DELZ=W2
              ABC=W2
              CALL SPMV(ABC)
          ELSE
          END IF
          IF(WQ.NE.'CENT'.AND.DF4.EQ.0.AND.W4.NE.0.0D0.OR.
     1    WQ.NE.'CACC'.AND.DF4.EQ.0.AND.W4.NE.0.0D0) THEN
              DELZ=W3
              ABC=W4
              CALL SPMV(ABC)
          ELSE
          END IF
C
          IF(.NOT.SPDEXT) THEN
              OUTLYNE=
     1        'NO CURRENT SPOT DIAGRAM DATA EXISTS'
              CALL SHOWIT(1)
              OUTLYNE='NO ACTION TAKEN'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C     HERE IS WHERE THE LINE SPREAD FUNCTION DATA IS PROCESSED.
C     LOAD THE CURRENT SPOT DIAGRAM DATA INTO THE LAS ARRAY
C     THE LSF ARRAY HAS 2 COLUMNS. COLUMN 1 INITIALLY CONTAINS
C     THE RAY INTENSITY TERM. COLUMN 2 EITHER HAS THE OFF-SET
C     COMPONENT MINUS EITHER THE CORRESPONDING CHEIF RAY COMPONENT
C     OR THE CORRESPONDINIG COMPONENT OF THE SPOT CENTROID. IF
C     FOB NULL WAS USED, NO LSF CAN BE DONE.
C
          IF(NULL) THEN
              OUTLYNE=
     1        '"FOB NULL" IS CURRENTLY IN EFFECT'
              CALL SHOWIT(1)
              OUTLYNE=
     1        '"FOB NULL" PRECLUDES LINE SPREAD FUNCTION CALCULATIONS'
              CALL SHOWIT(1)
              OUTLYNE='NO ACTION TAKEN'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
C     GET THE NEW MOVED CENTRIOD POSITIONS IF NECESSARY
          IF(WQ.EQ.'CENT'.AND.DF2.EQ.0.AND.W2.NE.0.0D0) THEN
              IF(SYSTEM1(30).GT.2.0D0) THEN
                  OUTLYNE=
     1            'DEFOCUSING IS NOT ALLOWED IN MODES AFOCAL AND UAFOCAL'
                  CALL SHOWIT(1)
                  OUTLYNE='NO ACTION TAKEN'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              ELSE
                  ABC=W2
                  CALL SPMV(ABC)
              END IF
          END IF
          IF(WQ.NE.'CENT'.AND.DF4.EQ.0.AND.W4.NE.0.0D0) THEN
              IF(SYSTEM1(30).GT.2.0D0) THEN
                  OUTLYNE=
     1            'DEFOCUSING IS NOT ALLOWED IN MODES AFOCAL AND UAFOCAL'
                  CALL SHOWIT(1)
                  OUTLYNE='NO ACTION TAKEN'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              ELSE
                  ABC=W4
                  CALL SPMV(ABC)
              END IF
          END IF
C
C     CALCULATE REDVLX AND REDVLY (THE OFFSET VALUES)
C
          IF(WQ.EQ.'CENT'.OR.WC.EQ.'CACC') THEN
              REDVLX=CENTX
              REDVLY=CENTY
              SPDELX=0.0D0
              SPDELY=0.0D0
          ELSE
              IF(SYSTEM1(30).LT.3.0D0) THEN
C     FOCAL
                  REDVLX=REFRY(1,NEWIMG)+W2
     1            +(DTAN(REFRY(11,NEWIMG))*W4)
                  SPDELX=W2
                  REDVLY=REFRY(2,NEWIMG)+W3
     1            +(DTAN(REFRY(12,NEWIMG))*W4)
                  SPDELY=W3
              ELSE
C     AFOCAL
                  REDVLX=REFRY(11,NEWIMG)+W2
                  SPDELX=W2
                  REDVLY=REFRY(12,NEWIMG)+W3
                  SPDELY=W3
              END IF
          END IF
C
C     REDVLX AND REDVLY HAVE BEEN SET
C
C     NOW PROCEED WITH THE DATA LOADING
C
C     OPEN AND CLOSE TO CLEAR
          OPEN(UNIT=66,ACCESS='DIRECT',FILE=LIBSPO//'LSF.DAT',
     1    FORM='UNFORMATTED',RECL=(10*NRECL),STATUS='UNKNOWN')
          CALL CLOSE_FILE(66,0)
C
          ILSF=1
C
          OPEN(UNIT=66,ACCESS='DIRECT',FILE=LIBSPO//'LSF.DAT',
     1    FORM='UNFORMATTED',RECL=(10*NRECL),STATUS='UNKNOWN')
          DO I=2,ITOT
C     LOAD DSPOT(*) WITH DSPOTT(*,ID)
              ID=I-1
              CALL SPOTIT(4)
              IF(DSPOT(17).NE.0.0D0) THEN
                  IF(DSPOT(12).NE.0.0D0) THEN
                      ILSF=ILSF+1
                      LSF1=DSPOT(12)
                      IF(SYSTEM1(30).LT.3.0D0) THEN
C     FOCAL
                          IF(WQ.NE.'CENT'.AND.WQ.NE.'CACC') THEN
                              XV=((DSPOT(1))+(DTAN(DSPOT(9))*W4))
                              YV=((DSPOT(2))+(DTAN(DSPOT(10))*W4))
                          ELSE
C     WQ MUST BE CENT OR CACC
                              XV=((DSPOT(1))+(DTAN(DSPOT(9))*W2))
                              YV=((DSPOT(2))+(DTAN(DSPOT(10))*W2))
                          END IF
                          IF(WQ.NE.'CENT'
     1                    .OR.WQ.NE.'CACC') THETA=W5*(PII/180.0D0)
                          IF(WQ.NE.'CENT'
     1                    .OR.WQ.NE.'CACC') MTHETA=W5
                          IF(WQ.EQ.'CENT'
     1                    .OR.WQ.EQ.'CACC')THETA=W3*(PII/180.0D0)
                          IF(WQ.EQ.'CENT'
     1                    .OR.WQ.EQ.'CACC') MTHETA=W3
                          IF(SYSTEM1(30).LT.3.0D0) THEN
                              LSF2=((((XV-REDVLX)/JB)*DCOS(THETA))+
     1                        (((YV-REDVLY)/JA)*DSIN(THETA)))
                          ELSE
                              LSF2=(((XV-REDVLX)*DCOS(THETA))+
     1                        ((YV-REDVLY)*DSIN(THETA)))
                          END IF
                      ELSE
C     AFOCAL
                          IF(WQ.NE.'CENT'.AND.WQ.NE.'CACC') THEN
                              XV=(((DSPOT(9)+W2)))
                              YV=(((DSPOT(10)+W3)))
                          ELSE
C     WQ MUST BE CENT OR CACC
                              XV=(((DSPOT(9))))
                              YV=(((DSPOT(10))))
                          END IF
                          IF(WQ.NE.'CENT'
     1                    .OR.WQ.NE.'CACC') THETA=W5*(PII/180.0D0)
                          IF(WQ.NE.'CENT'
     1                    .OR.WQ.NE.'CACC') MTHETA=W5
                          IF(WQ.EQ.'CENT'
     1                    .OR.WQ.EQ.'CACC') THETA=W3*(PII/180.0D0)
                          IF(WQ.EQ.'CENT'
     1                    .OR.WQ.EQ.'CACC') MTHETA=W3
                          IF(SYSTEM1(30).LT.3.0D0) THEN
                              LSF2=((((XV-REDVLX)/JB)*DCOS(THETA))+
     1                        (((YV-REDVLY)/JA)*DSIN(THETA)))
                          ELSE
                              LSF2=(((XV-REDVLX)*DCOS(THETA))+
     1                        ((YV-REDVLY)*DSIN(THETA)))
                          END IF
                      END IF
                  ELSE
C     NO CALCULATION, ENERGY WAS ZERO
                      GO TO 666
                  END IF
              ELSE
C     NO CALCULATION, WEIGHT WAS ZERO
                  GO TO 666
              END IF
              WRITE(UNIT=66,REC=ILSF) LSF1,LSF2
 666          CONTINUE
          END DO
          WRITE(UNIT=66,REC=1) ILSF
C
          M1=ILSF
          N1=2
          DEALLOCATE (LARGER,STAT=ALLOERR)
          ALLOCATE (LARGER(M1,N1),STAT=ALLOERR)
C
C     THERE ARE NOW ILSF PAIRS OF LSF DATA
C
          IF(ILSF.GT.2) CALL LSFSOR(ILSF-1,LARGER,M1,N1)
C                        ILSF=ILSF-1
C     THERE ARE NOW ILSF PAIRS OF LSF DATA
          IF(ILSF.GT.2)CALL LSFPAK(LARGER,M1,N1)
C     THERE ARE NOW ILSF PAIRS OF LSF DATA
          IF(ILSF.GT.2) THEN
              CALL LSFSUM(LARGER,M1,N1)
          END IF
          ILSF=44
          DEALLOCATE(LARGER,STAT=ALLOERR)
C     THERE ARE NOW 43 PAIRS OF LSF DATA
          IMAX=-1.0D10
          IF(ILSF.GT.2) THEN
              DO I=2,ILSF
                  READ(UNIT=66,REC=I) LSF1,LSF2
                  IF(LSF1.GT.IMAX) IMAX=LSF1
              END DO
          ELSE
              READ(UNIT=66,REC=2) LSF1,LSF2
              IMAX=LSF1
          END IF
          IF(ILSF.GT.2) THEN
              DO I=2,ILSF
                  READ(UNIT=66,REC=I) LSF1,LSF2
                  WRITE(UNIT=66,REC=I) (LSF1/IMAX),LSF2
              END DO
          ELSE
              LSF1=1.0D0
              READ(UNIT=66,REC=2) LSF1,LSF2
              WRITE(UNIT=66,REC=2) LSF1/IMAX,LSF2
          END IF
C     INTERPOLATE AND OUTPUT THE DATA REQUESTED
C
C     NOW DO APPROPRIATE OUTPUT BASED UPON COMMAND
C     FIRST, IF WQ IS ACC OR CACC, NO OUTPUT, JUST CALCULATE THE
C     INTENSITY FOR THE REQUESTED IMAGE POSITION (+ OR -)
          IF(WQ.EQ.'ACC'.OR.WQ.EQ.'CACC') THEN
              IF(ILSF.EQ.2) THEN
C     PERFECT SYSTEM, ONLY ONE NON-ZERO POINT
                  READ(UNIT=66,REC=2) LSF1,LSF2
                  IF(REAL(W1).EQ.REAL(LSF2)) INTEN=LSF1
                  IF(REAL(W1).NE.REAL(LSF2)) INTEN=0.0D0
              ELSE
C     MORE THAN ONE POINT, INTERPOLATE
C
                  INTEN=0.0D0
C     W1 IS THE POSITION FOR WHICH WE WISH TO KNOW THE INTENSITY
                  DO I=2,ILSF-1
                      READ(UNIT=66,REC=I) LSF1,LSF2
                      READ(UNIT=66,REC=I+1) LSFP1,LSFP2
                      IF(W1.GE.LSF2.AND.W1.LE.LSFP2) THEN
C     DATA IS BETWEEN THESE POINTS, LINEARLY INTERPOLATE IT
                          IX1=LSF2
                          IX2=LSFP2
                          IY1=LSF1
                          IY2=LSFP1
                          IL1=((W1-IX2)/(IX1-IX2))
                          IL2=((W1-IX1)/(IX2-IX1))
                          INTEN=(IL1*IY1)+(IL2*IY2)
                      ELSE
C     KEEP SEARCHING
                      END IF
                  END DO
              END IF
C
              REG(40)=REG(9)
              REG(9)=INTEN
              LSF=.TRUE.
              RETURN
          ELSE
C     WQ NOT ACC OR CACC
          END IF
C     NOW INTERPOLATE THE VALUES AND DISPLAY THEM
          WRITE(OUTLYNE,20)
          CALL SHOWIT(0)
 20       FORMAT('LINE SPREAD FUNCTION')
C
C     PRINT ORIENTATION OF KNIFE EDGE
          WRITE(OUTLYNE,30) THETA*(180.0D0/PII)
          CALL SHOWIT(0)
 30       FORMAT('SCAN ORIENTATION = ',F7.2,' DEGREE(S)')
C
          IF(WQ.EQ.'CENT'.AND.SYSTEM1(30).LT.3.0D0) THEN
              IF(SYSTEM1(6).EQ.1.0D0) J_UN='INCH(S)'
              IF(SYSTEM1(6).EQ.2.0D0) J_UN='CENTIMETER(S)'
              IF(SYSTEM1(6).EQ.3.0D0) J_UN='MILLIMETER(S)'
              IF(SYSTEM1(6).EQ.4.0D0) J_UN='METER(S)'

              IF(W3.NE.0.0D0) WRITE(OUTLYNE,40) W3,J_UN
              IF(W3.NE.0.0D0) CALL SHOWIT(0)
 40           FORMAT('APPLIED DEFOCUS (Z-DIRECTION) = ',G13.6,1X,A13)
          ELSE
          END IF
          IF(WQ.NE.'CENT'.AND.SYSTEM1(30).LT.3.0D0) THEN
              IF(SYSTEM1(6).EQ.1.0D0) J_UN='INCH(S)'
              IF(SYSTEM1(6).EQ.2.0D0) J_UN='CENTIMETER(S)'
              IF(SYSTEM1(6).EQ.3.0D0) J_UN='MILLIMETER(S)'
              IF(SYSTEM1(6).EQ.4.0D0) J_UN='METER(S)'

              IF(W2.NE.0.0D0) WRITE(OUTLYNE,41) W2,J_UN
              IF(W2.NE.0.0D0) CALL SHOWIT(0)
 41           FORMAT('APPLIED X-OFFSET = ',G13.6,1X,A13)

              IF(W3.NE.0.0D0) WRITE(OUTLYNE,42) W3,J_UN
              IF(W3.NE.0.0D0) CALL SHOWIT(0)
 42           FORMAT('APPLIED Y-OFFSET = ',G13.6,1X,A13)

              IF(W3.NE.0.0D0) WRITE(OUTLYNE,40) W4,J_UN
              IF(W3.NE.0.0D0) CALL SHOWIT(0)
          ELSE
          END IF
          IF(WQ.NE.'CENT'.AND.SYSTEM1(30).GT.2.0D0) THEN
              J_UN='RADIAN(S)'
              IF(W2.NE.0.0D0) WRITE(OUTLYNE,43) W2,J_UN
              IF(W2.NE.0.0D0) CALL SHOWIT(0)
 43           FORMAT('APPLIED X-ANGULAR OFFSET = ',G13.6,1X,A13)
              IF(W3.NE.0.0D0) WRITE(OUTLYNE,44) W3,J_UN
              IF(W3.NE.0.0D0) CALL SHOWIT(0)
 44           FORMAT('APPLIED Y-ANGULAR OFFSET = ',G13.6,1X,A13)
          ELSE
          END IF
          IF(WQ.EQ.'CENT'.AND.SYSTEM1(30).LT.3.0D0) THEN
              IF(SYSTEM1(6).EQ.1.0D0) J_UN='INCH(S)'
              IF(SYSTEM1(6).EQ.2.0D0) J_UN='CENTIMETER(S)'
              IF(SYSTEM1(6).EQ.3.0D0) J_UN='MILLIMETER(S)'
              IF(SYSTEM1(6).EQ.4.0D0) J_UN='METER(S)'
              WRITE(OUTLYNE,50) CENTX,J_UN
              CALL SHOWIT(0)
              WRITE(OUTLYNE,60) CENTX,J_UN
              CALL SHOWIT(0)
 50           FORMAT('CENTROID X-COORDINATE = ',G13.6,1X,A13)
 60           FORMAT('CENTROID Y-COORDINATE = ',G13.6,1X,A13)
          ELSE
          END IF
          IF(WQ.EQ.'CENT'.AND.SYSTEM1(30).GT.2.0D0) THEN
              J_UN='RADIANS(S)'
              WRITE(OUTLYNE,51) CENTX,J_UN
              CALL SHOWIT(0)
              WRITE(OUTLYNE,61) CENTX,J_UN
              CALL SHOWIT(0)
 51           FORMAT('CENTROID X-ANGULAR COORDINATE = ',G13.6,1X,A13)
 61           FORMAT('CENTROID Y-ANGULAR COORDINATE = ',G13.6,1X,A13)
          ELSE
          END IF
          IF(WQ.NE.'CENT'.AND.SYSTEM1(30).LT.3.0D0) THEN
              IF(SYSTEM1(6).EQ.1.0D0) J_UN='INCH(S)'
              IF(SYSTEM1(6).EQ.2.0D0) J_UN='CENTIMETER(S)'
              IF(SYSTEM1(6).EQ.3.0D0) J_UN='MILLIMETER(S)'
              IF(SYSTEM1(6).EQ.4.0D0) J_UN='METER(S)'
              WRITE(OUTLYNE,70) REFRY(1,NEWIMG),J_UN
              CALL SHOWIT(0)
              WRITE(OUTLYNE,80) REFRY(2,NEWIMG),J_UN
              CALL SHOWIT(0)
 70           FORMAT('CHIEF RAY X-COORDINATE = ',G13.6,1X,A13)
 80           FORMAT('CHIEF RAY Y-COORDINATE = ',G13.6,1X,A13)
          ELSE
          END IF
          IF(WQ.NE.'CENT'.AND.SYSTEM1(30).GT.2.0D0) THEN
              J_UN='RADIAN(S)'
              WRITE(OUTLYNE,71) REFRY(11,NEWIMG),J_UN
              CALL SHOWIT(0)
              WRITE(OUTLYNE,81) REFRY(12,NEWIMG),J_UN
              CALL SHOWIT(0)
 71           FORMAT('CHIEF RAY X-ANGULAR COORDINATE = ',G13.6,1X,A13)
 81           FORMAT('CHIEF RAY Y-ANGULAR COORDINATE = ',G13.6,1X,A13)
          ELSE
          END IF
          IF(SYSTEM1(30).LT.3.0D0) THEN
              IF(SYSTEM1(6).EQ.1.0D0) J_UN='INCH(S)'
              IF(SYSTEM1(6).EQ.2.0D0) J_UN='CENTIMETER(S)'
              IF(SYSTEM1(6).EQ.3.0D0) J_UN='MILLIMETER(S)'
              IF(SYSTEM1(6).EQ.4.0D0) J_UN='METER(S)'
          ELSE
              J_UN='RADIAN(S)'
          END IF
          IF(ILSF.NE.2) WRITE(OUTLYNE,902) J_UN
          IF(ILSF.NE.2) CALL SHOWIT(0)
 902      FORMAT('RELATIVE IMAGE INTENSITY',4X,'   POSITION    ',1X,A13)
C
C     W1 SPECIFIES THE NUMBER OF DATA POINTS
C     SO THERE WILL BE (ROUNDED TO THE NEAREST INTEGER)
          IF(DF1.EQ.1) THEN
              NOPNTS=21
          ELSE
              NOPNTS=INT(W1)
          END IF
C
          IF(NOPNTS.NE.1) THEN
              READ(UNIT=66,REC=1) ILSF
              READ(UNIT=66,REC=2) LSF1,LSF2
              READ(UNIT=66,REC=ILSF) LSFP1,LSFP2
              USDEL=DABS((LSFP2-LSF2)/DBLE(NOPNTS-1))
              NEWW1=LSF2
              NP=NOPNTS+1
              IKE=0
              IE=-1
 987          CONTINUE
              IKE=IKE+1
              NP=NP-1
              IF(NP.EQ.0) GO TO 988
              IF(NP.EQ.NOPNTS) THEN
                  READ(UNIT=66,REC=2) INTEN,NEWW1
              END IF
              IF(NP.EQ.1) THEN
                  READ(UNIT=66,REC=ILSF) INTEN,NEWW1
              END IF
              IF(NP.LT.NOPNTS.AND.NP.GT.1) THEN
                  READ(UNIT=66,REC=1) ILSF
                  IF(WQ.EQ.'CENT') LSFCENT=.TRUE.
                  DO I=2,ILSF-1
                      READ(UNIT=66,REC=I) LSF1,LSF2
                      READ(UNIT=66,REC=I+1) LSFP1,LSFP2
                      IF((NEWW1).GE.(LSF2)
     1                .AND.(NEWW1).LE.(LSFP2)) THEN
C     DATA IS BETWEEN THESE POINTS, LINEARLY INTERPOLATE IT
C     INTERPOLATE
                          IX1=LSF2
                          IX2=LSFP2
                          IY1=LSF1
                          IY2=LSFP1
                          IL1=((NEWW1-IX2)/(IX1-IX2))
                          IL2=((NEWW1-IX1)/(IX2-IX1))
                          INTEN=(IL1*IY1)+(IL2*IY2)
                          GO TO 899
                      END IF
                  END DO
 899              CONTINUE
              END IF
              WRITE(OUTLYNE,903) INTEN,NEWW1
              CALL SHOWIT(0)
              IE=IE+1
              GDTAP(IE+1)=REAL(NEWW1)
              GDTAV(IE+1)=REAL(INTEN)
              ENNL=IE
              EXTGLSF=.TRUE.
              IF(NOPNTS.LT.105) YCOORD(IKE)=INTEN
              IF(NOPNTS.LT.105) XCOORD(IKE)=NEWW1
              NEWW1=NEWW1+USDEL
              INTEN=0.0D0
              GO TO 987
 988          CONTINUE
          ELSE
C     NOPNTS=1
              WRITE(OUTLYNE,904)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,905)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,906)
              CALL SHOWIT(0)
 904          FORMAT('THE CURRENT SYSTEM IS GEOMETRICALLY PERFECT')
 905          FORMAT('AND THE SPREAD FUNCTION IS A DELTA FUNCTION')
 906          FORMAT('WITH INTENSITY = 1.0 EXACTLY')
C     SET UP A DELTA FUNCTION LSF FOR USE BY THE GOTF COMMAND
              TOPVAL=-21.0D-20
              ILSF=41
              WRITE(UNIT=66,REC=1) ILSF
              DO I=1,41
                  IF(I.NE.21) LSF1=0.0D0
                  IF(I.EQ.21) LSF1=1.0D0
                  TOPVAL=TOPVAL+1.0D-20
                  LSF2=TOPVAL
                  WRITE(UNIT=66,REC=I+1) LSF1,LSF2
              END DO
          END IF
          LSF=.TRUE.
 903      FORMAT(5X,G14.6,13X,G14.6)
C
C     INTENSITY CALCULATIONS ARE COMPLETE
C
          RETURN
      END
