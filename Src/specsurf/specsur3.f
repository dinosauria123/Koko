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

C       THIRD GROUP OF SPECIAL SURFACE AND FITTING FILES

C SUB FITTYP.FOR
      SUBROUTINE FITTYP
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE FITTYP. THIS IS THE SUBROUTINE WHICH
C       HANDLES THE "TYPE" COMMAND FROM THE SPFIT PROGRAM LEVEL.
C
          REAL*8  STYPE,SSURF
C
          COMMON/ST/STYPE
C
          COMMON/SSS/SSURF
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
C
          IF(SST.EQ.1.OR.SQ.EQ.1.OR.S2.EQ.1.OR.S3.EQ.1
     1    .OR.S4.EQ.1.OR.S5.EQ.1) THEN
              OUTLYNE='"TYPE" ONLY TAKES NUMERIC WORD NUMBER #1 INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(DF1.EQ.1) THEN
              OUTLYNE='"TYPE" COMMAND REQUIRES NUMERIC WORD #1 INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
          STYPE=W1
          IF(DABS(STYPE).LT.1.0.OR.DABS(STYPE).GT.30.0) THEN
              OUTLYNE='"TYPE" OUTSIDE DEFINED BOUNDS'
              CALL SHOWIT(1)
              OUTLYNE='VALID TYPES 1 TO 30'
              CALL SHOWIT(1)
              STYPE=0.0
              CALL MACFAL
              RETURN
          END IF
          IF(DABS(STYPE).GT.6.0) THEN
              OUTLYNE='TYPES 6 THROUGH 30 NOT YET SUPPORTED'
              CALL SHOWIT(1)
              CALL MACFAL
              F23=0
              STYPE=0.0
              RETURN
          END IF
          F23=1
          F24=0
          F25=0
          STYPE=W1
          WRITE(OUTLYNE,*) 'FITTING "TYPE" SET TO TYPE #',INT(W1)
          CALL SHOWIT(1)
          SURF=-99
          SSURF=-1.0D0
C       SET SSURF TO NON-VALID TYPE (NEGATIVE)
          RETURN
      END
C SUB FITSUR.FOR
      SUBROUTINE FITSUR
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE FITSUR. THIS IS THE SUBROUTINE WHICH
C       HANDLES THE "SURF" COMMAND FROM THE SPFIT PROGRAM LEVEL.
C
          REAL*8 SSURF,STYPE
C
          COMMON/SSS/SSURF
C
          COMMON/ST/STYPE
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
C
          IF(SST.EQ.1.OR.SQ.EQ.1.OR.S2.EQ.1.OR.S3.EQ.1
     1    .OR.S4.EQ.1.OR.S5.EQ.1) THEN
              OUTLYNE='"SURF" ONLY TAKES NUMERIC WORD #1 INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(DF1.EQ.1) THEN
              OUTLYNE=
     1        '"SURF" COMMAND REQUIRES EXPLICIT NUMERIC WORD #1 INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL MACFAL
              RETURN
          END IF
C
          SURF=INT(W1)
          SSURF=ALENS(34,SURF)
          IF(SSURF.LT.1.0.OR.SSURF.GT.30.0) THEN
              WRITE(OUTLYNE,*)'WARNING FOR SURFACE ',INT(W1)
              CALL SHOWIT(1)
              OUTLYNE='SURFACE "TYPE" NUMBER OUTSIDE DEFINED BOUNDS'
              CALL SHOWIT(1)
              OUTLYNE='VALID TYPES 1 TO 30'
              CALL SHOWIT(1)
              CALL MACFAL
              SSURF=0.0D0
              SURF=-99
              RETURN
          END IF
          IF(SSURF.GT.18.0) THEN
              OUTLYNE='SURF TYPES 18 THROUGH 30 NOT YET SUPPORTED'
              CALL SHOWIT(1)
              CALL MACFAL
              SSURF=0.0D0
              SURF=-99
              F23=0
              RETURN
          END IF

C       VALID SSURF, SET STYPE TO NEGATIVE
          WRITE(OUTLYNE,*)
     1      'TYPE #',INT(SSURF),' DATA FITTING HAS BEEN DEFINED'
          CALL SHOWIT(1)
          WRITE(OUTLYNE,*) 'FOR SURFACE #',INT(W1)
          CALL SHOWIT(1)
          F23=2
          F24=0
          F25=0
          STYPE=-1.0D0
          RETURN
      END
C SUB FITFIT.FOR
      SUBROUTINE FITFIT

          USE SVDSUB
      
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE FITFIT. THIS IS THE SUBROUTINE WHICH
C       HANDLES THE "FIT" COMMAND FROM THE SPFIT PROGRAM LEVEL.
C
          INTEGER SSN,SM,NP2,MP,COUNT,I,J,N,M,II
C
          COMMON/PRSIZE/COUNT
C
          INTEGER M1,ALLOERR
C
C       THIS PASSES THE SOLUTION OF THE LEAST SQUARES PROBLEM
          COMMON/SOLU/X
          CHARACTER FITGLASS*8
          COMMON/GLASSFIT/FITGLASS
C
          REAL*8 ACCUM
     1    ,CCOL,W,V,U,
     2    X,B,XXX

          DIMENSION ACCUM(1:96,1:96),
     1    CCOL(1:96),W(:),V(:,:),U(:,:),
     2    X(1:96),B(:),XXX(:)
          ALLOCATABLE :: W,V,U,B,XXX
C
          COMMON/ACDATA/ACCUM,CCOL
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
C
          M1=96
          DEALLOCATE(W,V,U,
     2    B,XXX,STAT=ALLOERR)
          ALLOCATE( W(M1),V(M1,M1),U(M1,M1),
     2    B(M1),XXX(M1),STAT=ALLOERR)
          W(1:96)=0.0D0
          B(1:96)=0.0D0
          XXX(1:96)=0.0D0
          V(1:96,1:96)=0.0D0
          U(1:96,1:96)=0.0D0
C
          IF(WC.EQ.'FIT') THEN
              IF(SST.EQ.1.OR.SQ.EQ.1.OR.S1.EQ.1.OR.S2.EQ.1.OR.
     1        S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1)THEN
                  OUTLYNE=
     1            '"FIT" TAKES NO EXPLICIT INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  DEALLOCATE(W,V,U,B,XXX,STAT=ALLOERR)
                  RETURN
              END IF
          END IF
          IF(WC.EQ.'FITGLASS') THEN
              IF(SST.EQ.1.OR.S1.EQ.1.OR.S2.EQ.1.OR.
     1        S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1)THEN
                  OUTLYNE=
     1            '"FITGLASS" ONLY TAKES QUALIFIER (GLASS NAME) INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  DEALLOCATE(W,V,U,B,XXX,STAT=ALLOERR)
                  RETURN
              END IF
              IF(SQ.EQ.0) THEN
                  OUTLYNE=
     1            '"FITGLASS" REQUIRES EXPLICIT QUALIFIER (GLASS NAME) INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  DEALLOCATE(W,V,U,B,XXX,STAT=ALLOERR)
                  RETURN
              END IF
              IF(SQ.EQ.1) THEN
                  FITGLASS=WQ
              END IF
          END IF
          IF(F23.EQ.2) THEN
              X(1:96)=0.0D0
              XXX(1:96)=0.0D0
              CFTYPE(1:96)=0.0D0
              FTFL01(1:96,SURF)=0.0D0
          END IF
C
C       SET F24=1 SAYING THERE IS DATA FITTED AND READY FOR
C       EVALUATION
          F24=1
C
C       THE LARGE ARRAY WITH ACCUMULATED DATA IS
C       ACCUM. THE COLUMN VECTOR WITH FUNCTIONAL DATA
C       IS CCOL.
          MP=96
          NP2=96
          N=COUNT
          M=COUNT
          I=COUNT
          B(1:I)=CCOL(1:I)
          J=COUNT
          U(1:I,1:J)=ACCUM(1:I,1:J)
C       DO SINGULAR VALUE DECOMPOSITION
C
          SM=M
          SSN=N
          CALL SVDCMP(U,SM,SSN,MP,NP2,W,V)
          M=SM
          N=SSN
C
C       SOLVE LINEAR EQUATION
C
          SM=M
          SSN=N
          CALL SVBKSB(U,W,V,SM,SSN,MP,NP2,B,XXX)
          M=SM
          N=SSN
C
          IF(F23.EQ.1) THEN
C       SIMPLE DATA FIT
              II=0
              DO 351 I=1,96
                  IF(CFF(I).EQ.1) THEN
                      II=II+1
                      CFTYPE(I)=XXX(II)
                      X(I)=CFTYPE(I)
                  ELSE
                      CFTYPE(I)=0.0D0
                      X(I)=CFTYPE(I)
                  END IF
 351          CONTINUE
          END IF
          IF(F23.EQ.2) THEN
C       SURFACE DATA FIT
              II=0
              DO 352 I=1,96
                  IF(CFF(I).EQ.1) THEN
                      II=II+1
                      CFTYPE(I)=XXX(II)
                      FTFL01(I,SURF)=XXX(II)
                      X(I)=FTFL01(I,SURF)
                  ELSE
                      CFTYPE(I)=0.0D0
                      FTFL01(I,SURF)=0.0D0
                      X(I)=FTFL01(I,SURF)
                  END IF
 352          CONTINUE
          END IF
C
C       THIS IS WHERE THE ACTUAL SOLUTION TO THE LEAST
C       SQUARES PROBLEM IS DONE. ALL THE COEFFICIENTS
C       OF THE NORMAL EQUATION HAVE BEEN DETERMINED IN
C       THE SUBROUTINE "FITDAT"
          DEALLOCATE(W,V,U,B,XXX,STAT=ALLOERR)
          RETURN
C
      END
C
C SUB FITEOS.FOR
      SUBROUTINE FITEOS
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE FITEOS. THIS IS THE SUBROUTINE WHICH
C       HANDLES THE EXIT FROM THE SPFIT PROGRAM LEVEL TO THE CMD
C       LEVEL.
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
C
          IF(SST.EQ.1.OR.SN.EQ.1.OR.SQ.EQ.1) THEN
              OUTLYNE='"EOS" TAKES NO EXPLICIT INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
C       RETURN TO CMD LEVEL
C
          F1=1
          F9=0
C
          RETURN
      END
C SUB FITCFF.FOR
      SUBROUTINE FITCFF
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE FITCFF. THIS IS THE SUBROUTINE WHICH
C       HANDLES THE "COEF" COMMAND FROM THE SPFIT PROGRAM LEVEL.
C
          INTEGER I
C
          REAL*8 X(1:96)
C
          COMMON/SOLU/X
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
C
          IF(SST.EQ.1.OR.SQ.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1)
     1    THEN
              OUTLYNE=
     1        '"COEF" ONLY TAKES NUMERIC WORD #1, #2 AND #3 INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
          IF(DF1.EQ.1.OR.DF2.EQ.1) THEN
              OUTLYNE=
     1        '"COEF" REQUIRES EXPLICIT NUMERIC WORD #1 AND #2 INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
C       CHECK THAT TYPE OR SURF WAS ENTERED
          IF(F23.EQ.0) THEN
              OUTLYNE=
     1        'THE "TYPE" OR "SURF" COMMANDS MUST BE ENTERED BEFORE'
              CALL SHOWIT(1)
              OUTLYNE='COEFFICIENT FLAGS CAN BE SET'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
          IF(INT(W1).LT.1.OR.INT(W1).GT.96) THEN
              OUTLYNE='INVALID COEFFICIENT NUMBER'
              CALL SHOWIT(1)
              OUTLYNE='VALID TYPES 1 TO 96'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(INT(W2).LT.0.OR.INT(W2).GT.1) THEN
              OUTLYNE='INVALID COEFFICIENT FLAG VALUE'
              CALL SHOWIT(1)
              OUTLYNE='VALID VALUES ARE 0-(OFF) AND 1-(ON)'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
C       SET COEFFICIENT FLAG VALUE
          CFF(INT(W1))=INT(W2)
          IF(DF3.NE.1) THEN
C       EXPLICIT COEFF VALUES ARE BEING ENTERED.
              F24=1
              F25=1
C
              IF(F23.EQ.2) FTFL01(INT(W1),SURF)=W3
              IF(F23.EQ.2) X(INT(W1))=FTFL01(INT(W1),SURF)
              IF(F23.EQ.1) CFTYPE(INT(W1))=W3
              IF(F23.EQ.1) X(INT(W1))=CFTYPE(INT(W1))
C       EVALUATION AS IF FITTING IS DONE IS ASSUMED
          ELSE
              F24=0
              F25=0
C       NO COEFF VALUES
              IF(F23.EQ.2) FTFL01(INT(W1),SURF)=0.0D0
              IF(F23.EQ.2) X(INT(W1))=FTFL01(INT(W1),SURF)
              IF(F23.EQ.1) CFTYPE(INT(W1))=0.0D0
              IF(F23.EQ.1) X(INT(W1))=CFTYPE(INT(W1))
          END IF
C
C       NOW NOVE THE COEFFICIENT VALUES TO THE SOULTION VECTOR
C
C       SET UP THE SOLUTION VECTOR FOR EVALUATION
          IF(F23.EQ.1) THEN
C       SIMPLE DATA FIT
              DO 9898 I=1,96
                  IF(CFF(I).EQ.0) GO TO 9898
                  X(I)=CFTYPE(I)

 9898         CONTINUE
          ELSE
C       NOT A SIMPLE DATA FIT
          END IF
          IF(F23.EQ.2) THEN
C       SURFACE DATA FIT
              DO 8989 I=1,96
                  IF(CFF(I).EQ.0) GO TO 8989
                  X(I)=FTFL01(I,SURF)
 8989         CONTINUE
          ELSE
C       NOT A SURFACE DATA FIT
          END IF
          RETURN
      END
C SUB FITDAT.FOR
      SUBROUTINE FITDAT
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE FITDAT. THIS IS THE SUBROUTINE WHICH
C       HANDLES THE "DATA" COMMAND FROM THE SPFIT PROGRAM LEVEL.
C
          EXTERNAL FF2,FF3,FF4,FF5
C
          INTEGER COUNT1
     6    ,DATCNT,TYPE,COUNT2,COUNT,IDATA,I,J,II,JJ
C
          COMMON/PRSIZE/COUNT
C
          COMMON/NUMDAT/DATCNT
C
          REAL*8 ACCUM(1:96,1:96),XX,YY,
     1    TERM,CCOL(1:96),Z,FF2,FF3,FF4,FF5
C
          COMMON/ACDATA/ACCUM,CCOL
C
          REAL*8  STYPE,SSURF,RHO,THETA
C
          COMMON/SPIDAT/IDATA
C
          COMMON/ST/STYPE
C
          COMMON/SSS/SSURF
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
C
          IF(SST.EQ.1.OR.SQ.EQ.1.OR.S5.EQ.1) THEN
              OUTLYNE=
     1        '"DATA" ONLY TAKES NUMERIC WORD #1, #2, #3 AND #4 INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          ELSE
C       INPUT FORM OK SO FAR
          END IF
          IF(WC.EQ.'GDATA') THEN
              IF(SST.EQ.1.OR.SQ.EQ.1.OR.S5.EQ.1.OR.S4.EQ.1.OR.S3.EQ.1) THEN
                  OUTLYNE=
     1            '"GDATA" ONLY TAKES NUMERIC WORD #1 AND #2 INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              ELSE
C       INPUT FORM OK SO FAR
              END IF
          END IF
          IF(WC.EQ.'GDATA') THEN
C     RESET THE DATA FOR A "DATA" LINE
              W1=W1
              W3=W2**2
              W2=0.0D0
              W4=1.0D0
              DF1=0
              DF2=0
              DF3=0
              DF4=0
              S1=1
              S2=1
              S3=1
              S4=1
              SN=1
          END IF
C       W1 IS Y VALUE
C       W2 IS X VALUE
C       W3 IF FUNCTION VALUE
C       W4 IS WEIGHT VALUE (DEFAULT 1.0)
          IF(DF1.EQ.1) W1=0.0D0
          IF(DF2.EQ.1) W2=0.0D0
          IF(DF3.EQ.1) W3=0.0D0
          IF(DF4.EQ.1) W4=1.0D0
          IF(DF1.EQ.1.AND.DF2.EQ.1.AND.DF3.EQ.1.AND.DF4.EQ.1) THEN
              IF(WC.EQ.'GDATA')
     1        OUTLYNE='"GDATA" COMMAND REQUIRES SOME EXPLICIT NUMERIC INPUT'
              IF(WC.EQ.'DATA')
     1        OUTLYNE='"DATA" COMMAND REQUIRES SOME EXPLICIT NUMERIC INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          ELSE
C       EXPLICIT INPUT GIVEN, CONTINUE
          END IF
C       CHECK FOR ALL ZERO COEF FLAGS
C
C       COUNT1 IS NUMBER OF LOWEST COEF
C       COUNT2 IS NUMBER OF HIGHEST COEF
          COUNT1=0
          COUNT2=0
          DO 100 I=1,96
              IF(CFF(I).EQ.1) THEN
                  COUNT1=I
                  GO TO 105
              ELSE
              END IF
 100      CONTINUE
 105      DO 111 I=1,96
              IF(CFF(I).EQ.1) COUNT2=I
 111      CONTINUE
C
          IF(COUNT2.EQ.0) THEN
              OUTLYNE='ALL COEFFICIENT FLAGS ARE ZERO'
              CALL SHOWIT(1)
              OUTLYNE='NO FITTING POSSIBLE'
              CALL SHOWIT(1)
              OUTLYNE='RETURNED TO CMD LEVEL'
              CALL SHOWIT(1)
              F9=0
              F1=1
              CALL MACFAL
              RETURN
          ELSE
C       COUNT1 AND COUNT2 NOT ZERO, PROCEED
          END IF
C
          IF(F23.EQ.1.AND.STYPE.EQ.1.0D0.OR.
     1    F23.EQ.2.AND.SSURF.EQ.1.0D0.OR.
     1    F23.EQ.2.AND.SSURF.EQ.6.0D0) THEN
C
C       SPECIAL SURFACE TYPE 1 OR 6 OR TYPE 1 FUNCTIONAL FORM
C       IS A RADIAL POWER SERIES
C
C       FROM RHO**-8 TO RHO**39 INCLUDING RHO**0=1
C
C       PROCEED WITH FITTING
C       SET F24=0, NO DATA READY TO EVALUATE UNTIL A FIT IS DONE
              F24=0
C       DATA NOW EXISTS TO FIT
              F25=1
C       FILL THE ACCUM ARRAY
              Z=DSQRT((W1**2)+(W2**2))
C       INCREMENT THE DATA ITEM COUNTER
              DATCNT=DATCNT+1
              IDATA=IDATA+1
C       PROCEED
              II=0
              DO 10 I=COUNT1,COUNT2
C       FILL THE COLLUMN ARRAY
                  IF(CFF(I).EQ.1) THEN
                      II=II+1
C       CALCULATE COLUMN VECTOR
                      TERM= Z**(I-9)
                      TERM=TERM*W3*W4
                      CCOL(II)=CCOL(II)+TERM
                      JJ=0
                      DO 20 J=COUNT1,COUNT2
C       DO THE APPROPRIATE ROW FOR COLUMN I
                          IF(CFF(J).EQ.1) THEN
                              JJ=JJ+1
                              TERM= Z**(J-9+I-9)
                              ACCUM(II,JJ)=ACCUM(II,JJ)+(W4*TERM)
                          ELSE
C               CFF 0, GO TO NEXT ELEMENT
                          END IF
 20                   CONTINUE
                  ELSE
                  END IF
 10           CONTINUE
              COUNT=II
C
C       ALL VALUES CALCULATED FOR THE CURRENT "DATA"
C       STATEMENT.
C
              GO TO 3500
          ELSE
          END IF
          IF(F23.EQ.1.AND.STYPE.EQ.2.0D0.OR.
     1    F23.EQ.2.AND.SSURF.EQ.2.0D0
     1    .OR.F23.EQ.2.AND.SSURF.EQ.9.0D0) THEN
C
C       PROCEED WITH FITTING A TYPE 2 TYPE/SURFACE
C       SET F24=0, NO DATA READY TO EVALUATE UNTIL A FIT IS DONE
              F24=0
C       DATA NOW EXISTS TO FIT
              F25=1
              RHO=DSQRT((W1**2)+(W2**2))
              IF(DABS(W1).GE.DABS(((1.0D35)*W2))) THEN
                  IF(W1.GE.0.0D0) THETA=PII/2.0D0
                  IF(W1.LT.0.0D0) THETA=(3.0D0*PII)/2.0D0
              ELSE
                  IF(DABS(W1).LE.1.0D-15.AND.DABS(W2).LE.1.0D-15) THEN
                      THETA=0.0D0
                  ELSE
                      THETA=DATAN2(W1,W2)
                  END IF
                  IF(THETA.LT.0.0D0) THETA=THETA+(TWOPII)
              END IF
C       INCREMENT THE DATA ITEM COUNTER
              DATCNT=DATCNT+1
              IDATA=IDATA+1
C
              II=0
              DO 210 I=COUNT1,COUNT2
C       FILL THE COLLUMN ARRAY
                  IF(CFF(I).EQ.1) THEN
                      II=II+1
C       CALCULATE COLUMN VECTOR
C       FUNCTION FF2 RETURNS THE CORRECT VALUE
C       FOR THE ITH ENTRY IN THE COLUMN VECTOR
C       WITHOUT THE GIVEN DEPENDENT VARIABLE VALUE
C       OF THE WEIGHT FUNCTION FACTORED IN. THIS IS
C       FOR THE STANDARD ZERNIKE. I RANGES FROM COUNT1
C       TO COUNT2
C       MAX VALUE FOR TYPE 2 SURFACE IS 66
                      TERM= FF2(RHO,THETA,I)
                      TERM=TERM*W3*W4
                      CCOL(II)=CCOL(II)+TERM
                      JJ=0
                      DO 220 J=COUNT1,COUNT2
C       DO THE APPROPRIATE ROW FOR COLUMN I
                          IF(CFF(J).EQ.1) THEN
                              JJ=JJ+1
                              TERM= FF2(RHO,THETA,I)*FF2(RHO,THETA,J)
                              ACCUM(II,JJ)=ACCUM(II,JJ)+(W4*TERM)
                          ELSE
C               CFF 0, GO TO NEXT ELEMENT
                          END IF
 220                  CONTINUE
                  ELSE
                  END IF
 210          CONTINUE
              COUNT=II
C
C       ALL VALUES CALCULATED FOR THE CURRENT "DATA"
C       STATEMENT.
C
              GO TO 3500
          ELSE
          END IF
          IF(F23.EQ.1.AND.STYPE.EQ.5.0D0.OR.
     1    F23.EQ.2.AND.SSURF.EQ.14.0D0.OR.F23.EQ.2.AND.SSURF.EQ.15.0D0)
     2    THEN
C
C       PROCEED WITH FITTING A TYPE 5 TYPE/SURFACE
C       SET F24=0, NO DATA READY TO EVALUATE UNTIL A FIT IS DONE
              F24=0
C       DATA NOW EXISTS TO FIT
              F25=1
              RHO=DSQRT((W1**2)+(W2**2))
              IF(DABS(W1).GE.DABS(((1.0D35)*W2))) THEN
                  IF(W1.GE.0.0D0) THETA=PII/2.0D0
                  IF(W1.LT.0.0D0) THETA=(3.0D0*PII)/2.0D0
              ELSE
                  IF(DABS(W1).LE.1.0D-15.AND.DABS(W2).LE.1.0D-15) THEN
                      THETA=0.0D0
                  ELSE
                      THETA=DATAN2(W1,W2)
                  END IF
                  IF(THETA.LT.0.0D0) THETA=THETA+(TWOPII)
              END IF
C       INCREMENT THE DATA ITEM COUNTER
              DATCNT=DATCNT+1
              IDATA=IDATA+1
C
              II=0
              DO I=COUNT1,COUNT2
C       FILL THE COLLUMN ARRAY
                  IF(CFF(I).EQ.1) THEN
                      II=II+1
C       CALCULATE COLUMN VECTOR
C       FUNCTION FF5 RETURNS THE CORRECT VALUE
C       FOR THE ITH ENTRY IN THE COLUMN VECTOR
C       WITHOUT THE GIVEN DEPENDENT VARIABLE VALUE
C       OF THE WEIGHT FUNCTION FACTORED IN. THIS IS
C       FOR THE STANDARD ABERRATION POLY. I RANGES FROM COUNT1
C       TO COUNT2
C       MAX VALUE FOR TYPE 5 SURFACE IS 48
                      TERM= FF5(RHO,THETA,I)
                      TERM=TERM*W3*W4
                      CCOL(II)=CCOL(II)+TERM
                      JJ=0
                      DO J=COUNT1,COUNT2
C       DO THE APPROPRIATE ROW FOR COLUMN I
                          IF(CFF(J).EQ.1) THEN
                              JJ=JJ+1
                              TERM= FF5(RHO,THETA,I)*FF5(RHO,THETA,J)
                              ACCUM(II,JJ)=ACCUM(II,JJ)+(W4*TERM)
                          ELSE
C               CFF 0, GO TO NEXT ELEMENT
                          END IF
                      END DO
                  ELSE
                  END IF
              END DO
              COUNT=II
C
C       ALL VALUES CALCULATED FOR THE CURRENT "DATA"
C       STATEMENT.
C
              GO TO 3500
          ELSE
          END IF
          IF(F23.EQ.1.AND.STYPE.EQ.3.0D0.OR.
     1    F23.EQ.2.AND.SSURF.EQ.3.0D0.OR.F23.EQ.2.AND.SSURF.EQ.10) THEN
C
C       PROCEED WITH FITTING A TYPE 3 TYPE/SURFACE
C       SET F24=0, NO DATA READY TO EVALUATE UNTIL A FIT IS DONE
              F24=0
C       DATA NOW EXISTS TO FIT
              F25=1
              RHO=DSQRT((W1**2)+(W2**2))
              IF(DABS(W1).GE.DABS(((1.0D35)*W2))) THEN
                  IF(W1.GE.0.0D0) THETA=PII/2.0D0
                  IF(W1.LT.0.0D0) THETA=(3.0D0*PII)/2.0D0
              ELSE
                  IF(DABS(W1).LE.1.0D-15.AND.DABS(W2).LE.1.0D-15) THEN
                      THETA=0.0D0
                  ELSE
                      THETA=DATAN2(W1,W2)
                  END IF
                  IF(THETA.LT.0.0D0) THETA=THETA+(TWOPII)
              END IF
C       INCREMENT THE DATA ITEM COUNTER
              DATCNT=DATCNT+1
              IDATA=IDATA+1
C
              II=0
              DO 310 I=COUNT1,COUNT2
C       FILL THE COLLUMN ARRAY
                  IF(CFF(I).EQ.1) THEN
                      II=II+1
C       CALCULATE COLUMN VECTOR
C       FUNCTION FF3 RETURNS THE CORRECT VALUE
C       FOR THE ITH ENTRY IN THE COLUMN VECTOR
C       WITHOUT THE GIVEN DEPENDENT VARIABLE VALUE
C       OF THE WEIGHT FUNCTION FACTORED IN. THIS IS
C       FOR THE STANDARD ZERNIKE. I RANGES FROM COUNT1
C       TO COUNT2
C       MAX VALUE FOR TYPE 3 SURFACE IS 37
                      TERM= FF3(RHO,THETA,I)
                      TERM=TERM*W3*W4
                      CCOL(II)=CCOL(II)+TERM
                      JJ=0
                      DO 320 J=COUNT1,COUNT2
C       DO THE APPROPRIATE ROW FOR COLUMN I
                          IF(CFF(J).EQ.1) THEN
                              JJ=JJ+1
                              TERM= FF3(RHO,THETA,I)*FF3(RHO,THETA,J)
                              ACCUM(II,JJ)=ACCUM(II,JJ)+(W4*TERM)
                          ELSE
C               CFF 0, GO TO NEXT ELEMENT
                          END IF
 320                  CONTINUE
                  ELSE
                  END IF
 310          CONTINUE
              COUNT=II
C
C       ALL VALUES CALCULATED FOR THE CURRENT "DATA"
C       STATEMENT.
C
              GO TO 3500
          ELSE
          END IF
          IF(F23.EQ.1.AND.STYPE.EQ.4.0D0.OR.
     1    F23.EQ.2.AND.SSURF.EQ.7.0D0.OR.
     1    F23.EQ.2.AND.SSURF.EQ.8.0D0) THEN
C
C       PROCEED WITH FITTING A TYPE 4 FUNCTIONAL FORM OR A
C     TYPE 7 OR 8 SPECIAL SURFACE
C       SET F24=0, NO DATA READY TO EVALUATE UNTIL A FIT IS DONE
              F24=0
C       DATA NOW EXISTS TO FIT
              F25=1
              XX=W2
              YY=W1
C       INCREMENT THE DATA ITEM COUNTER
              DATCNT=DATCNT+1
              IDATA=IDATA+1
C
              II=0
              DO I=COUNT1,COUNT2
C       FILL THE COLLUMN ARRAY
                  IF(CFF(I).EQ.1) THEN
                      II=II+1
C       CALCULATE COLUMN VECTOR
C       FUNCTION FF4 RETURNS THE CORRECT VALUE
C       FOR THE ITH ENTRY IN THE COLUMN VECTOR
C       WITHOUT THE GIVEN DEPENDENT VARIABLE VALUE
C       OF THE WEIGHT FUNCTION FACTORED IN. THIS IS
C       FOR THE STANDARD ZERNIKE. I RANGES FROM COUNT1
C       TO COUNT2
C       MAX VALUE IS 91
                      TERM= FF4(XX,YY,I)
                      TERM=TERM*W3*W4
                      CCOL(II)=CCOL(II)+TERM
                      JJ=0
                      DO J=COUNT1,COUNT2
C       DO THE APPROPRIATE ROW FOR COLUMN I
                          IF(CFF(J).EQ.1) THEN
                              JJ=JJ+1
                              TERM= FF4(XX,YY,I)*FF4(XX,YY,J)
                              ACCUM(II,JJ)=ACCUM(II,JJ)+(W4*TERM)
                          ELSE
C               CFF 0, GO TO NEXT ELEMENT
                          END IF
                      END DO
                  ELSE
                  END IF
              END DO
              COUNT=II
C
C       ALL VALUES CALCULATED FOR THE CURRENT "DATA"
C       STATEMENT.
C
              GO TO 3500
          ELSE
          END IF
          IF(F23.EQ.1) TYPE=INT(STYPE)
          IF(F23.EQ.2) TYPE=INT(SSURF)
          WRITE(OUTLYNE,*)'TYPE ',TYPE,' FITTING NOT YET SUPPORTED'
          CALL SHOWIT(1)
          OUTLYNE='RETURNED TO CMD LEVEL'
          CALL SHOWIT(1)
          CALL MACFAL
          F9=0
          F1=1
          RETURN
 3500     CONTINUE
C       ADD THE CURRENT DATA TO THE DATA SAVING ARRAY
          IF(IDATA.LT.MAXSPS) THEN
              DDATA(1,IDATA)=W1
              DDATA(2,IDATA)=W2
              DDATA(3,IDATA)=W3
              DDATA(4,IDATA)=W4
          ELSE
C       NO MORE DATA ALLOWED
              OUTLYNE=
     1        'WARNING: NO MORE DATA IS BEING SAVED IN THE "DATA" ARRAY'
              CALL SHOWIT(1)
          END IF
          RETURN
      END
C SUB FTEVAL.FOR
      SUBROUTINE FTEVAL
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE FTEVAL. THIS IS THE SUBROUTINE WHICH
C       HANDLES THE "EVAL" COMMAND FROM THE SPFIT PROGRAM LEVEL.
C
          EXTERNAL FF2,FF3,FF4,FF5
C
          INTEGER
     6    DATCNT,TYPE,COUNT2,COUNT,I
C
          COMMON/PRSIZE/COUNT
C
          COMMON/NUMDAT/DATCNT
C
          REAL*8
     1    TERM,X(1:96),RHO,THETA,FF2,FF3,FF4,XX,YY,FF5
C
          REAL*8  STYPE,SSURF
C
          COMMON/SOLU/X
C
          COMMON/ST/STYPE
C
          COMMON/SSS/SSURF
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
C
C       W1 IS Y VALUE
C       W2 IS X VALUE
C
          IF(SST.EQ.1.OR.SQ.EQ.1.OR.S5.EQ.1.OR.S4.EQ.1.OR.
     1    S3.EQ.1) THEN
              OUTLYNE=
     1        '"EVAL" ONLY TAKES NUMERIC WORD #1 AND #2 INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          ELSE
C       INPUT FORM OK SO FAR
          END IF
          IF(DF1.EQ.1.AND.DF2.EQ.1) THEN
              OUTLYNE=
     1        '"EVAL" COMMAND REQUIRES SOME EXPLICIT NUMERIC INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          ELSE
C       EXPLICIT INPUT GIVEN, CONTINUE
          END IF
C
          IF(DF1.EQ.1) THEN
              W1=0.0D0
              DF1=0
          ELSE
          END IF
          IF(DF2.EQ.1) THEN
              W2=0.0D0
              DF2=0
          ELSE
          END IF
C
C       CHECK FOR ALL ZERO COEF FLAGS
C
C       COUNT2 IS NUMBER OF HIGHEST COEF
          COUNT2=0
          DO 100 I=1,96
              IF(CFF(I).EQ.1) THEN
                  GO TO 105
              ELSE
              END IF
 100      CONTINUE
 105      COUNT=0
          DO 111 I=1,96
              IF(CFF(I).EQ.1) THEN
                  COUNT2=I
                  COUNT=COUNT+1
              ELSE
              END IF
 111      CONTINUE
C
          IF(COUNT2.EQ.0) THEN
              OUTLYNE='ALL COEFFICIENT FLAGS ARE ZERO'
              CALL SHOWIT(1)
              OUTLYNE='NO EVALUATION POSSIBLE'
              CALL SHOWIT(1)
              OUTLYNE='RETURNED TO CMD LEVEL'
              CALL SHOWIT(1)
              CALL MACFAL
              F9=0
              F1=1
              RETURN
          ELSE
C       COUNT2 NOT ZERO, PROCEED
          END IF
C
          IF(F23.EQ.1.AND.STYPE.EQ.1.0D0.OR.
     1    F23.EQ.2.AND.SSURF.EQ.1.0D0.OR.
     1    F23.EQ.2.AND.SSURF.EQ.6.0D0) THEN
C
C       PROCEED WITH EVALUATION
              TERM=0.0D0
              RHO=DSQRT((W1**2)+(W2**2))
              DO 20 I=1,48
                  IF(CFF(I).EQ.1) THEN
                      TERM=TERM+(X(I)*(RHO**(I-9)))
                  ELSE
                  END IF
 20           CONTINUE
              GO TO 2500
          ELSE
          END IF
C
          IF(F23.EQ.1.AND.STYPE.EQ.2.0D0.OR.
     1    F23.EQ.2.AND.SSURF.EQ.2.0D0.OR.
     1    F23.EQ.2.AND.SSURF.EQ.9.0D0) THEN
C
C       PROCEED WITH EVALUATION
              TERM=0.0D0
              RHO=DSQRT((W1**2)+(W2**2))
              IF(DABS(W1).GE.DABS(((1.0D35)*W2))) THEN
                  IF(W1.GE.0.0D0) THETA=PII/2.0D0
                  IF(W1.LT.0.0D0) THETA=(3.0D0*PII)/2.0D0
              ELSE
                  IF(DABS(W1).LE.1.0D-15.AND.DABS(W2).LE.1.0D-15) THEN
                      THETA=0.0D0
                  ELSE
                      THETA=DATAN2(W1,W2)
                  END IF
                  IF(THETA.LT.0.0D0) THETA=THETA+(TWOPII)
              END IF
              DO I=1,66
                  IF(CFF(I).EQ.1) THEN
                      TERM=TERM+(X(I)*FF2(RHO,THETA,I))
                  ELSE
                  END IF
              END DO
              GO TO 2500
          ELSE
          END IF
          IF(F23.EQ.1.AND.STYPE.EQ.5.0D0.OR.
     1    F23.EQ.2.AND.SSURF.EQ.14.0D0.OR.F23.EQ.2.AND.
     2    SSURF.EQ.15.0D0) THEN
C
C       PROCEED WITH EVALUATION
              TERM=0.0D0
              RHO=DSQRT((W1**2)+(W2**2))
              IF(DABS(W1).GE.DABS(((1.0D35)*W2))) THEN
                  IF(W1.GE.0.0D0) THETA=PII/2.0D0
                  IF(W1.LT.0.0D0) THETA=(3.0D0*PII)/2.0D0
              ELSE
                  IF(DABS(W1).LE.1.0D-15.AND.DABS(W2).LE.1.0D-15) THEN
                      THETA=0.0D0
                  ELSE
                      THETA=DATAN2(W1,W2)
                  END IF
                  IF(THETA.LT.0.0D0) THETA=THETA+(TWOPII)
              END IF
              DO I=1,48
                  IF(CFF(I).EQ.1) THEN
                      TERM=TERM+(X(I)*FF5(RHO,THETA,I))
                  ELSE
                  END IF
              END DO
              GO TO 2500
          ELSE
          END IF
          IF(F23.EQ.1.AND.STYPE.EQ.3.0D0.OR.
     1    F23.EQ.2.AND.SSURF.EQ.3.0D0.OR.
     1    F23.EQ.2.AND.SSURF.EQ.10.0D0) THEN
C
C       PROCEED WITH EVALUATION
              TERM=0.0D0
              RHO=DSQRT((W1**2)+(W2**2))
              IF(DABS(W1).GE.DABS(((1.0D35)*W2))) THEN
                  IF(W1.GE.0.0D0) THETA=PII/2.0D0
                  IF(W1.LT.0.0D0) THETA=(3.0D0*PII)/2.0D0
              ELSE
                  IF(DABS(W1).LE.1.0D-15.AND.DABS(W2).LE.1.0D-15) THEN
                      THETA=0.0D0
                  ELSE
                      THETA=DATAN2(W1,W2)
                  END IF
                  IF(THETA.LT.0.0D0) THETA=THETA+(TWOPII)
              END IF
              DO I=1,37
                  IF(CFF(I).EQ.1) THEN
                      TERM=TERM+(X(I)*FF3(RHO,THETA,I))
                  ELSE
                  END IF
              END DO
              GO TO 2500
          ELSE
          END IF
          IF(F23.EQ.1.AND.STYPE.EQ.4.0D0.OR.
     1    F23.EQ.2.AND.SSURF.EQ.7.0D0.OR.
     1    F23.EQ.2.AND.SSURF.EQ.8.0D0) THEN
C
C       PROCEED WITH EVALUATION
              TERM=0.0D0
              XX=W2
              YY=W1
              DO I=1,91
                  IF(CFF(I).EQ.1) THEN
                      TERM=TERM+(X(I)*FF4(XX,YY,I))
                  ELSE
                  END IF
              END DO
              GO TO 2500
          ELSE
          END IF
          IF(F23.EQ.1) TYPE=INT(STYPE)
          IF(F23.EQ.2) TYPE=INT(SSURF)
          WRITE(OUTLYNE,*)'TYPE ',TYPE,' FITTING NOT YET SUPPORTED'
          CALL SHOWIT(1)
          OUTLYNE='RETURNED TO CMD LEVEL'
          CALL SHOWIT(1)
          F9=0
          F1=1
          CALL MACFAL
          RETURN
C
C       HERE IS WHERE OUTPUT IS DONE FOR ALL EVALUATIONS
C
 2500     REG(40)=REG(9)
          REG(9)=TERM
          IF(F23.EQ.1) TYPE=INT(STYPE)
          IF(F23.EQ.2) TYPE=INT(SSURF)
          WRITE(OUTLYNE,1011)
          CALL SHOWIT(0)
 1011     FORMAT(1X)
          IF(F23.EQ.1) WRITE(OUTLYNE,1101) TYPE
          IF(F23.EQ.2) WRITE(OUTLYNE,1102) TYPE
          CALL SHOWIT(0)
          WRITE(OUTLYNE,1110) W1
          CALL SHOWIT(0)
          WRITE(OUTLYNE,1105) W2
          CALL SHOWIT(0)
          WRITE(OUTLYNE,1111) TERM
          CALL SHOWIT(0)
 1101     FORMAT('FOR A TYPE ',I3,' DATA FIT:')
 1102     FORMAT('FOR A TYPE ',I3,' SURFACE FIT:')
 1110     FORMAT('WITH Y = ',G18.10)
 1105     FORMAT(' AND X = ',G18.10)
 1111     FORMAT('THE FUNCTIONAL VALUE = ',G18.10)
C
          RETURN
      END
C SUB FTLIST.FOR
      SUBROUTINE FTLIST
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE FTLIST. THIS IS THE SUBROUTINE WHICH
C       HANDLES THE "LIST" COMMAND FROM THE SPFIT PROGRAM LEVEL.
C
          EXTERNAL FF2,FF3,FF4,FF5
C
          INTEGER
     6    DATCNT,TYPE,COUNT2,COUNT,
     7    IDATA,I,II,III,IIII,IIIII
C
          COMMON/PRSIZE/COUNT
C
          COMMON/NUMDAT/DATCNT
C
          REAL*8 TERM,X(1:96),RHO,FF2,FF3,FF4,XX,YY,FF5
C
          COMMON/SPIDAT/IDATA
C
          REAL*8  STYPE,SSURF
C
          COMMON/SOLU/X
C
          COMMON/ST/STYPE
C
          COMMON/SSS/SSURF
C
          REAL*8 THETA
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
C
          IF(SST.EQ.1.OR.SQ.EQ.1.OR.SN.EQ.1) THEN
              OUTLYNE=
     1        '"LIST" TAKES NO EXPLICIT INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C       CHECK FOR ALL ZERO COEF FLAGS
C
C       COUNT2 IS NUMBER OF HIGHEST COEF
          COUNT2=0
          DO 100 I=1,96
              IF(CFF(I).EQ.1) THEN
                  GO TO 105
              ELSE
              END IF
 100      CONTINUE
 105      COUNT=0
          DO 111 I=1,96
              IF(CFF(I).EQ.1) THEN
                  COUNT2=I
                  COUNT=COUNT+1
              ELSE
              END IF
 111      CONTINUE
C
          IF(COUNT2.EQ.0) THEN
              OUTLYNE='ALL COEFFICIENT FLAGS ARE ZERO'
              CALL SHOWIT(1)
              OUTLYNE='NO LISTING POSSIBLE'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
          IF(F23.EQ.1.AND.STYPE.EQ.1.0D0.OR.
     1    F23.EQ.2.AND.SSURF.EQ.1.0D0.OR.
     1    F23.EQ.2.AND.SSURF.EQ.6.0D0) THEN
C
C       PROCEED WITH LISTING
C
C       WRITE THE HEADING THEN THE DATA
              WRITE(OUTLYNE,1106)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,1105)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,1106)
              CALL SHOWIT(0)
 1106         FORMAT(1X)
 1105         FORMAT('   Y-VALUE     ',1X,'   X-VALUE     ',
     1        1X,' INPUT F(Y,X)  ',1X,
     2        ' FITTED F(X,Y) ',1X,'FITTING ERROR')
C
              DO 3500 III=1,IDATA
                  II=0
                  TERM=0.0D0
                  RHO=DSQRT((DDATA(1,III)**2)+(DDATA(2,III)**2))
                  DO 20 I=1,96
                      IF(CFF(I).EQ.0) GO TO 20
                      II=II+1
                      TERM=TERM+(X(I)*(RHO**(I-9)))
 20               CONTINUE
C       DO A LINE OF OUTPUT
                  WRITE(OUTLYNE,1110) DDATA(1,III),DDATA(2,III),DDATA(3,III),
     1            TERM,(DDATA(3,III)-TERM)
                  CALL SHOWIT(0)
 3500         CONTINUE
              RETURN
          ELSE
          END IF
          IF(F23.EQ.1.AND.STYPE.EQ.2.0D0.OR.
     1    F23.EQ.2.AND.SSURF.EQ.2.0D0.OR.
     1    F23.EQ.2.AND.SSURF.EQ.9.0D0) THEN
C
C       PROCEED WITH LISTING
C
C       WRITE THE HEADING THEN THE DATA
              WRITE(OUTLYNE,1106)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,1105)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,1106)
              CALL SHOWIT(0)
C
              DO 4500 IIII=1,IDATA
C       PROCEED WITH EVALUATION
                  II=0
                  TERM=0.0D0
                  RHO=DSQRT((DDATA(1,IIII)**2)+(DDATA(2,IIII)**2))
                  IF(DABS(DDATA(1,IIII)).GE.
     1            DABS(((1.0D35)*DDATA(2,IIII)))) THEN
                      IF(DDATA(1,IIII).GE.0.0D0) THETA=PII/2.0D0
                      IF(DDATA(1,IIII).LT.0.0D0) THETA=(3.0D0*PII)/2.0D0
                  ELSE
                      IF(DABS(DDATA(1,IIII)).LE.1.0D-15.AND.
     1                DABS(DDATA(2,IIII)).LE.1.0D-15) THEN
                          THETA=0.0D0
                      ELSE
                          THETA=DATAN2(DDATA(1,IIII),DDATA(2,IIII))
                      END IF
                      IF(THETA.LT.0.0D0) THETA=THETA+(TWOPII)
                  END IF
                  DO 2121 I=1,66
                      IF(CFF(I).EQ.1) THEN
                          TERM=TERM+(X(I)*FF2(RHO,THETA,I))
                      ELSE
                      END IF
 2121             CONTINUE
C       DO A LINE OF OUTPUT
                  WRITE(OUTLYNE,1110) DDATA(1,IIII),DDATA(2,IIII),
     1            DDATA(3,IIII),TERM,(DDATA(3,IIII)-TERM)
                  CALL SHOWIT(0)
 4500         CONTINUE
              RETURN
          ELSE
          END IF
          IF(F23.EQ.1.AND.STYPE.EQ.5.0D0.OR.
     1    F23.EQ.2.AND.SSURF.EQ.14.0D0.OR.F23.EQ.2.AND.
     2    SSURF.EQ.15.0D0) THEN
C
C       PROCEED WITH LISTING
C
C       WRITE THE HEADING THEN THE DATA
              WRITE(OUTLYNE,1106)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,1105)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,1106)
              CALL SHOWIT(0)
C
              DO IIII=1,IDATA
C       PROCEED WITH EVALUATION
                  II=0
                  TERM=0.0D0
                  RHO=DSQRT((DDATA(1,IIII)**2)+(DDATA(2,IIII)**2))
                  IF(DABS(DDATA(1,IIII)).GE.
     1            DABS(((1.0D35)*DDATA(2,IIII)))) THEN
                      IF(DDATA(1,IIII).GE.0.0D0) THETA=PII/2.0D0
                      IF(DDATA(1,IIII).LT.0.0D0) THETA=(3.0D0*PII)/2.0D0
                  ELSE
                      IF(DABS(DDATA(1,IIII)).LE.1.0D-15.AND.
     1                DABS(DDATA(2,IIII)).LE.1.0D-15) THEN
                          THETA=0.0D0
                      ELSE
                          THETA=DATAN2(DDATA(1,IIII),DDATA(2,IIII))
                      END IF
                      IF(THETA.LT.0.0D0) THETA=THETA+(TWOPII)
                  END IF
                  DO I=1,48
                      IF(CFF(I).EQ.1) THEN
                          TERM=TERM+(X(I)*FF5(RHO,THETA,I))
                      ELSE
                      END IF
                  END DO
C       DO A LINE OF OUTPUT
                  WRITE(OUTLYNE,1110) DDATA(1,IIII),DDATA(2,IIII),
     1            DDATA(3,IIII),TERM,(DDATA(3,IIII)-TERM)
                  CALL SHOWIT(0)
              END DO
              RETURN
          ELSE
          END IF
          IF(F23.EQ.1.AND.STYPE.EQ.3.0D0.OR.
     1    F23.EQ.2.AND.SSURF.EQ.3.0D0.OR.
     1    F23.EQ.2.AND.SSURF.EQ.10.0D0) THEN
C
C       PROCEED WITH LISTING
C
C       WRITE THE HEADING THEN THE DATA
              WRITE(OUTLYNE,1106)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,1105)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,1106)
              CALL SHOWIT(0)
C
              DO 5500 IIIII=1,IDATA
C       PROCEED WITH EVALUATION
                  II=0
                  TERM=0.0D0
                  RHO=DSQRT((DDATA(1,IIIII)**2)+(DDATA(2,IIIII)**2))
                  IF(DABS(DDATA(1,IIIII)).GE.
     1            DABS(((1.0D35)*DDATA(2,IIIII)))) THEN
                      IF(DDATA(1,IIIII).GE.0.0D0) THETA=PII/2.0D0
                      IF(DDATA(1,IIIII).LT.0.0D0) THETA=(3.0D0*PII)/2.0D0
                  ELSE
                      IF(DABS(DDATA(1,IIIII)).LE.1.0D-15.AND.
     1                DABS(DDATA(2,IIIII)).LE.1.0D-15) THEN
                          THETA=0.0D0
                      ELSE
                          THETA=DATAN2(DDATA(1,IIIII),DDATA(2,IIIII))
                      END IF
                      IF(THETA.LT.0.0D0) THETA=THETA+(TWOPII)
                  END IF
                  DO 2222 I=1,37
                      IF(CFF(I).EQ.1) THEN
                          TERM=TERM+(X(I)*FF3(RHO,THETA,I))
                      ELSE
                      END IF
 2222             CONTINUE
C       DO A LINE OF OUTPUT
                  WRITE(OUTLYNE,1110) DDATA(1,IIIII),DDATA(2,IIIII),
     1            DDATA(3,IIIII),TERM,(DDATA(3,IIIII)-TERM)
                  CALL SHOWIT(0)
 5500         CONTINUE
              RETURN
          ELSE
          END IF
          IF(F23.EQ.1.AND.STYPE.EQ.4.0D0.OR.
     1    F23.EQ.2.AND.SSURF.EQ.7.0D0.OR.
     1    F23.EQ.2.AND.SSURF.EQ.8.0D0) THEN
C
C       PROCEED WITH LISTING
C
C       WRITE THE HEADING THEN THE DATA
              WRITE(OUTLYNE,1106)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,1105)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,1106)
              CALL SHOWIT(0)
C
              DO IIIII=1,IDATA
C       PROCEED WITH EVALUATION
                  II=0
                  TERM=0.0D0
                  XX=DDATA(2,IIIII)
                  YY=DDATA(1,IIIII)
                  DO I=1,91
                      IF(CFF(I).EQ.1) THEN
                          TERM=TERM+(X(I)*FF4(XX,YY,I))
                      ELSE
                      END IF
                  END DO
C       DO A LINE OF OUTPUT
                  WRITE(OUTLYNE,1110) DDATA(1,IIIII),DDATA(2,IIIII),
     1            DDATA(3,IIIII),TERM,(DDATA(3,IIIII)-TERM)
                  CALL SHOWIT(0)
              END DO
              RETURN
          ELSE
          END IF
C
          IF(F23.EQ.1) TYPE=INT(STYPE)
          IF(F23.EQ.2) TYPE=INT(SSURF)
          WRITE(OUTLYNE,*)'TYPE ',TYPE,' FITTING NOT YET SUPPORTED'
          CALL SHOWIT(1)
          OUTLYNE='RETURNED TO CMD LEVEL'
          CALL SHOWIT(1)
          CALL MACFAL
          F9=0
          F1=1
          RETURN
C
C       HERE IS WHERE OUTPUT IS DONE FOR ALL EVALUATIONS
C
 1110     FORMAT(D15.8,1X,D15.8,1X,D15.8,1X,D15.8,1X,D15.8)
C
      END
C SUB RDDATA.FOR
      SUBROUTINE RDDATA
C
          IMPLICIT NONE
C
          EXTERNAL FF2,FF3,FF4,FF5
C
C       THIS IS SUBROUTINE RDDATA. THIS IS THE SUBROUTINE WHICH
C       HANDLES THE "READ" COMMAND FROM THE SPFIT PROGRAM LEVEL.
C
          CHARACTER DDDD*4
C
          LOGICAL EXISTD
C
          INTEGER COUNT1
     6    ,DATCNT,TYPE,COUNT2,COUNT,IDATA,I,II,III
     7    ,J,JJ
C
          COMMON/PRSIZE/COUNT
C
          COMMON/NUMDAT/DATCNT
C
          REAL*8 ACCUM(1:96,1:96),DWW1,DWW2,DWW3,DWW4,
     1    TERM,CCOL(1:96),Z,FF2,FF3,RHO,THETA,FF4,XX,YY,FF5
C
          COMMON/ACDATA/ACCUM,CCOL
C
          REAL*8  STYPE,SSURF
C
          COMMON/SPIDAT/IDATA
C
          COMMON/ST/STYPE
C
          COMMON/SSS/SSURF
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
C
          IF(SST.EQ.1.OR.SQ.EQ.1.OR.SN.EQ.1) THEN
              OUTLYNE=
     1        '"READ" TAKES NO EXPLICIT INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          ELSE
C       INPUT FORM OK SO FAR
          END IF
C
C       READ CAUSES THE FILE DATA.DAT TO BE READ AS A SERIES OF DATA
C       INPUT STATEMENTS. THE DATA.DAT RECORD FORMAT LIST DIRECTED
C
C       WHERE FIELDS ARE:
C       "DATA"
C       Y-COORDINATE
C       X-COORDINATE
C       FUNCTIONAL VALUE
C       WEIGTHING FACTOR
C
C       ENTRIES MAY BE SEPARATED BY A SPACE OR A COMMA
C
C       READS ARE COMPLETED WHEN AN EOF IS DETECTED BY THE
C       INPUT STATEMENT. BAD DATA IN THE DATA.DAT FILE MAY
C       CAUSE THE PROGRAM TO BOMB.
C
          EXISTD=.FALSE.
          INQUIRE(FILE='DATA.DAT',EXIST=EXISTD)
          IF(.NOT.EXISTD) THEN
C       CAN'T DO THE READ OPERATION
              OUTLYNE='NO DATA.DAT FILE EXISTS TO BE "READ"'
              CALL SHOWIT(1)
              OUTLYNE=
     1        'RETURNED TO CMD LEVEL'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          ELSE
C       INITIALIZE THE DDATA ARRAY
              DDATA(1,1:MAXSPS)=0.0D0
              IDATA=0
              DATCNT=0
C       CHECK FOR ALL ZERO COEF FLAGS
C
              COUNT1=0
              COUNT2=0
              DO 100 I=1,96
                  IF(CFF(I).EQ.1) THEN
                      COUNT1=I
                      GO TO 105
                  ELSE
                  END IF
 100          CONTINUE
 105          DO 1111 I=1,96
                  IF(CFF(I).EQ.1) COUNT2=I
 1111         CONTINUE
              IF(COUNT2.EQ.0) THEN
                  OUTLYNE='ALL COEFFICIENT FLAGS ARE ZERO'
                  CALL SHOWIT(1)
                  OUTLYNE='NO FITTING POSSIBLE'
                  CALL SHOWIT(1)
                  OUTLYNE='RETURNED TO CMD LEVEL'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  F9=0
                  F1=1
                  RETURN
              ELSE
C       COUNT2 NOT ZERO, PROCEED
              END IF
          END IF
          CCOL(1:96)=0.0D0
          ACCUM(1:96,1:96)=0.0D0
          OPEN(UNIT=18,ACCESS='SEQUENTIAL',BLANK='NULL'
     1    ,FILE='DATA.DAT'
     2    ,STATUS='UNKNOWN')
          DO 3500 II=1,MAXSPS
              READ(UNIT=18,FMT=*,ERR=114,END=113) DDDD,DWW1,DWW2,DWW3,DWW4
C
C                       TYPE 1 FITTING
C
              IF(F23.EQ.1.AND.STYPE.EQ.1.0D0.OR.
     1        F23.EQ.2.AND.SSURF.EQ.1.0D0.OR.
     1        F23.EQ.2.AND.SSURF.EQ.6.0D0) THEN
C
C       PROCEED WITH FITTING
C       FILL THE ACCUM ARRAY
                  Z=DSQRT((DWW1**2)+(DWW2**2))
C       INCREMENT THE DATA ITEM COUNTER
                  DATCNT=DATCNT+1
                  IDATA=IDATA+1
C       PROCEED
                  III=0
                  DO 10 I=COUNT1,COUNT2
C       FILL THE COLLUMN ARRAY
                      IF(CFF(I).EQ.1) THEN
                          III=III+1
C       CALCULATE COLUMN VECTOR
                          TERM= Z**(I-9)
                          TERM=TERM*DWW3*DWW4
                          CCOL(III)=CCOL(III)+TERM
                          JJ=0
                          DO 20 J=COUNT1,COUNT2
C       DO THE APPROPRIATE ROW FOR COLUMN I
                              IF(CFF(J).EQ.1) THEN
                                  JJ=JJ+1
                                  TERM= Z**(J-9+I-9)
                                  ACCUM(III,JJ)=ACCUM(III,JJ)+(DWW4*TERM)
                              ELSE
C               CFF 0, GO TO NEXT ELEMENT
                              END IF
 20                       CONTINUE
                      ELSE
                      END IF
 10               CONTINUE
                  COUNT=III
C
C       ALL VALUES CALCULATED FOR THE CURRENT "DATA"
C       STATEMENT.
C       READ NEXT ITEM IN DATA.DAT BY GOING TO 3501
                  GO TO 3501
              ELSE
C       NOT TYPE 1 FITTING
              END IF
C
C                       NOT TYPE 1 FITTING
C       ************************************************************
              IF(F23.EQ.1.AND.STYPE.EQ.2.0D0.OR.
     1        F23.EQ.2.AND.SSURF.EQ.2.0D0.OR.
     1        F23.EQ.2.AND.SSURF.EQ.9.0D0) THEN
C
C       PROCEED WITH FITTING A TYPE 2 TYPE/SURFACE
C       SET F24=0, NO DATA READY TO EVALUATE UNTIL A FIT IS DONE
                  F24=0
C       DATA NOW EXISTS TO FIT
                  F25=1
                  RHO=DSQRT((DWW1**2)+(DWW2**2))
                  IF(DABS(DWW1).GE.DABS(((1.0D35)*DWW2))) THEN
                      IF(DWW1.GE.0.0D0) THETA=PII/2.0D0
                      IF(DWW1.LT.0.0D0) THETA=(3.0D0*PII)/2.0D0
                  ELSE
                      IF(DABS(DWW1).LE.1.0D-15.AND.
     1                DABS(DWW2).LE.1.0D-15) THEN
                          THETA=0.0D0
                      ELSE
                          THETA=DATAN2(DWW1,DWW2)
                      END IF
                      IF(THETA.LT.0.0D0) THETA=THETA+(TWOPII)
                  END IF
C       INCREMENT THE DATA ITEM COUNTER
                  DATCNT=DATCNT+1
                  IDATA=IDATA+1
C
                  III=0
                  DO 210 I=COUNT1,COUNT2
C       FILL THE COLLUMN ARRAY
                      IF(CFF(I).EQ.1) THEN
                          III=III+1
C       CALCULATE COLUMN VECTOR
C       FUNCTION FF2 RETURNS THE CORRECT VALUE
C       FOR THE ITH ENTRY IN THE COLUMN VECTOR
C       WITHOUT THE GIVEN DEPENDENT VARIABLE VALUE
C       OF THE WEIGHT FUNCTION FACTORED IN. THIS IS
C       FOR THE STANDARD ZERNIKE. I RANGES FROM COUNT1
C       TO COUNT2
C       MAX VALUE FOR TYPE 2 SURFACE IS 66
                          TERM= FF2(RHO,THETA,I)
                          TERM=TERM*DWW3*DWW4
                          CCOL(III)=CCOL(III)+TERM
                          JJ=0
                          DO 220 J=COUNT1,COUNT2
C       DO THE APPROPRIATE ROW FOR COLUMN I
                              IF(CFF(J).EQ.1) THEN
                                  JJ=JJ+1
                                  TERM= FF2(RHO,THETA,I)*FF2(RHO,THETA,J)
                                  ACCUM(III,JJ)=ACCUM(III,JJ)+(DWW4*TERM)
                              ELSE
C               CFF 0, GO TO NEXT ELEMENT
                              END IF
 220                      CONTINUE
                      ELSE
                      END IF
 210              CONTINUE
                  COUNT=III
C
C       ALL VALUES CALCULATED FOR THE CURRENT "DATA"
C       STATEMENT.
C
                  GO TO 3501
              ELSE
              END IF
              IF(F23.EQ.1.AND.STYPE.EQ.5.0D0.OR.
     1        F23.EQ.2.AND.SSURF.EQ.14.0D0.OR.F23.EQ.2.AND.SSURF.EQ.15.0D0)
     2        THEN
C
C       PROCEED WITH FITTING A TYPE 5 TYPE/SURFACE
C       SET F24=0, NO DATA READY TO EVALUATE UNTIL A FIT IS DONE
                  F24=0
C       DATA NOW EXISTS TO FIT
                  F25=1
                  RHO=DSQRT((DWW1**2)+(DWW2**2))
                  IF(DABS(DWW1).GE.DABS(((1.0D35)*DWW2))) THEN
                      IF(DWW1.GE.0.0D0) THETA=PII/2.0D0
                      IF(DWW1.LT.0.0D0) THETA=(3.0D0*PII)/2.0D0
                  ELSE
                      IF(DABS(DWW1).LE.1.0D-15.AND.
     1                DABS(DWW2).LE.1.0D-15) THEN
                          THETA=0.0D0
                      ELSE
                          THETA=DATAN2(DWW1,DWW2)
                      END IF
                      IF(THETA.LT.0.0D0) THETA=THETA+(TWOPII)
                  END IF
C       INCREMENT THE DATA ITEM COUNTER
                  DATCNT=DATCNT+1
                  IDATA=IDATA+1
C
                  III=0
                  DO I=COUNT1,COUNT2
C       FILL THE COLLUMN ARRAY
                      IF(CFF(I).EQ.1) THEN
                          III=III+1
C       CALCULATE COLUMN VECTOR
C       FUNCTION FF5 RETURNS THE CORRECT VALUE
C       FOR THE ITH ENTRY IN THE COLUMN VECTOR
C       WITHOUT THE GIVEN DEPENDENT VARIABLE VALUE
C       OF THE WEIGHT FUNCTION FACTORED IN. THIS IS
C       FOR THE STANDARD ABERRATION POLY. I RANGES FROM COUNT1
C       TO COUNT2
C       MAX VALUE FOR TYPE 5 SURFACE IS 48
                          TERM= FF5(RHO,THETA,I)
                          TERM=TERM*DWW3*DWW4
                          CCOL(III)=CCOL(III)+TERM
                          JJ=0
                          DO J=COUNT1,COUNT2
C       DO THE APPROPRIATE ROW FOR COLUMN I
                              IF(CFF(J).EQ.1) THEN
                                  JJ=JJ+1
                                  TERM= FF5(RHO,THETA,I)*FF5(RHO,THETA,J)
                                  ACCUM(III,JJ)=ACCUM(III,JJ)+(DWW4*TERM)
                              ELSE
C               CFF 0, GO TO NEXT ELEMENT
                              END IF
                          END DO
                      ELSE
                      END IF
                  END DO
                  COUNT=III
C
C       ALL VALUES CALCULATED FOR THE CURRENT "DATA"
C       STATEMENT.
C
                  GO TO 3501
              ELSE
              END IF
              IF(F23.EQ.1.AND.STYPE.EQ.3.0D0.OR.
     1        F23.EQ.2.AND.SSURF.EQ.3.0D0.OR.
     1        F23.EQ.2.AND.SSURF.EQ.10.0D0) THEN
C
C       PROCEED WITH FITTING A TYPE 3 TYPE/SURFACE
C       SET F24=0, NO DATA READY TO EVALUATE UNTIL A FIT IS DONE
                  F24=0
C       DATA NOW EXISTS TO FIT
                  F25=1
                  RHO=DSQRT((DWW1**2)+(DWW2**2))
                  IF(DABS(DWW1).GE.DABS(((1.0D35)*DWW2))) THEN
                      IF(DWW1.GE.0.0D0) THETA=PII/2.0D0
                      IF(DWW1.LT.0.0D0) THETA=(3.0D0*PII)/2.0D0
                  ELSE
                      IF(DABS(DWW1).LE.1.0D-15.AND.
     1                DABS(DWW2).LE.1.0D-15) THEN
                          THETA=0.0D0
                      ELSE
                          THETA=DATAN2(DWW1,DWW2)
                      END IF
                      IF(THETA.LT.0.0D0) THETA=THETA+(TWOPII)
                  END IF
C       INCREMENT THE DATA ITEM COUNTER
                  DATCNT=DATCNT+1
                  IDATA=IDATA+1
C
                  III=0
                  DO 310 I=COUNT1,COUNT2
C       FILL THE COLLUMN ARRAY
                      IF(CFF(I).EQ.1) THEN
                          III=III+1
C       CALCULATE COLUMN VECTOR
C       FUNCTION FF3 RETURNS THE CORRECT VALUE
C       FOR THE ITH ENTRY IN THE COLUMN VECTOR
C       WITHOUT THE GIVEN DEPENDENT VARIABLE VALUE
C       OF THE WEIGHT FUNCTION FACTORED IN. THIS IS
C       FOR THE STANDARD ZERNIKE. I RANGES FROM COUNT1
C       TO COUNT2
C       MAX VALUE FOR TYPE 3 SURFACE IS 37
                          TERM= FF3(RHO,THETA,I)
                          TERM=TERM*DWW3*DWW4
                          CCOL(III)=CCOL(III)+TERM
                          JJ=0
                          DO 320 J=COUNT1,COUNT2
C       DO THE APPROPRIATE ROW FOR COLUMN I
                              IF(CFF(J).EQ.1) THEN
                                  JJ=JJ+1
                                  TERM= FF3(RHO,THETA,I)*FF3(RHO,THETA,J)
                                  ACCUM(III,JJ)=ACCUM(III,JJ)+(DWW4*TERM)
                              ELSE
C               CFF 0, GO TO NEXT ELEMENT
                              END IF
 320                      CONTINUE
                      ELSE
                      END IF
 310              CONTINUE
                  COUNT=III
C
C       ALL VALUES CALCULATED FOR THE CURRENT "DATA"
C       STATEMENT.
C
                  GO TO 3501
              ELSE
              END IF
              IF(F23.EQ.1.AND.STYPE.EQ.4.0D0.OR.
     1        F23.EQ.2.AND.SSURF.EQ.7.0D0.OR.
     1        F23.EQ.2.AND.SSURF.EQ.8.0D0) THEN
C
C       SET F24=0, NO DATA READY TO EVALUATE UNTIL A FIT IS DONE
                  F24=0
C       DATA NOW EXISTS TO FIT
                  F25=1
                  XX=DWW2
                  YY=DWW1
C       INCREMENT THE DATA ITEM COUNTER
                  DATCNT=DATCNT+1
                  IDATA=IDATA+1
C
                  III=0
                  DO I=COUNT1,COUNT2
C       FILL THE COLLUMN ARRAY
                      IF(CFF(I).EQ.1) THEN
                          III=III+1
C       CALCULATE COLUMN VECTOR
C       FUNCTION FF4 RETURNS THE CORRECT VALUE
C       FOR THE ITH ENTRY IN THE COLUMN VECTOR
C       WITHOUT THE GIVEN DEPENDENT VARIABLE VALUE
C       OF THE WEIGHT FUNCTION FACTORED IN.
C       I RANGES FROM COUNT1
C       TO COUNT2
                          TERM= FF4(XX,YY,I)
                          TERM=TERM*DWW3*DWW4
                          CCOL(III)=CCOL(III)+TERM
                          JJ=0
                          DO J=COUNT1,COUNT2
C       DO THE APPROPRIATE ROW FOR COLUMN I
                              IF(CFF(J).EQ.1) THEN
                                  JJ=JJ+1
                                  TERM= FF4(XX,YY,I)*FF4(XX,YY,J)
                                  ACCUM(III,JJ)=ACCUM(III,JJ)+(DWW4*TERM)
                              ELSE
C               CFF 0, GO TO NEXT ELEMENT
                              END IF
                          END DO
                      ELSE
                      END IF
                  END DO
                  COUNT=III
C
C       ALL VALUES CALCULATED FOR THE CURRENT "DATA"
C       STATEMENT.
C
                  GO TO 3501
              ELSE
              END IF
C       ************************************************************
              IF(F23.EQ.1) TYPE=INT(STYPE)
              IF(F23.EQ.2) TYPE=INT(SSURF)
              WRITE(OUTLYNE,*)'TYPE ',TYPE,' FITTING NOT YET SUPPORTED'
              CALL SHOWIT(1)
              OUTLYNE='RETURNED TO CMD LEVEL'
              CALL SHOWIT(1)
              CALL MACFAL
              F9=0
              F1=1
              CALL CLOSE_FILE(18,1)
              RETURN
 3501         CONTINUE
              DDATA(1,II)=DWW1
              DDATA(2,II)=DWW2
              DDATA(3,II)=DWW3
              DDATA(4,II)=DWW4
              IDATA=II
              DATCNT=II
              IF(IDATA.LT.MAXSPS) THEN
              ELSE
C       NO MORE DATA ALLOWED
                  OUTLYNE=
     1            'WARNING: NO MORE DATA IS BEING SAVED IN THE "DATA" ARRAY'
                  CALL SHOWIT(1)
              END IF
 3500     CONTINUE
 113      CONTINUE
          CALL CLOSE_FILE(18,1)
C       FINISHED READING DATA.DAT CORRECTLY
          F24=0
          F25=1
          CALL CLOSE_FILE(18,1)
          RETURN
C       EOF FOUND, READY TO RETURN TO SPFIT2
 114      CONTINUE
          OUTLYNE=
     1    'READ ERROR IN READING "DATA.DAT" FILE'
          CALL SHOWIT(1)
          OUTLYNE=
     1    'RETURNED TO CMD LEVEL'
          CALL SHOWIT(1)
          CALL MACFAL
          RETURN
      END
C SUB FF2.FOR
      FUNCTION FF2(RHO,THETA,FI)
C
          IMPLICIT NONE
C
          REAL*8 FN1,FN2,FN3,FN4,FN5,FN6,FN7,FN8,FN9
     3    ,FN10,FN11,FN12,FN13,FN14,FN15,FN16,FN17,FN18,FN19,FN20,FN21
     4    ,FN22,FN23,FN24,FN25,FN26,FN27,FN28,FN29,FN30,RHO,THETA,FF2
     4    ,FN31,FN32,FN33,FN34,FN35,FN36,FN37,FN38,FN39,FN40
     4    ,FN41,FN42,FN43,FN44,FN45,FN46,FN47,FN48,FN49,FN50
     4    ,FN51,FN52,FN53,FN54,FN55,FN56,FN57,FN58,FN59,FN60
     4    ,FN61,FN62,FN63,FN64,FN65,FN66
     5    ,R,A
C
          INTEGER FI
C
C       FUNCTIONS FOR TYPE 2 SURFACE (ZERNIKE)
          FN1(R,A)=1.0D0
          FN2(R,A)=R*DCOS(A)
          FN3(R,A)=R*DSIN(A)
          FN4(R,A)=(R**2)*DCOS(2.0D0*A)
          FN5(R,A)=2.0D0*(R**2)-1.0D0
          FN6(R,A)=(R**2)*DSIN(2.0D0*A)
          FN7(R,A)=(R**3)*DCOS(3.0D0*A)
          FN8(R,A)=((3.0D0*(R**3))-2.0D0*R)*DCOS(A)
          FN9(R,A)=((3.0D0*(R**3))-2.0D0*R)*DSIN(A)
          FN10(R,A)=(R**3)*DSIN(3.0D0*A)
          FN11(R,A)=(R**4)*DCOS(4.0D0*A)
          FN12(R,A)=((4.0D0*(R**4))-(3.0D0*(R**2)))*DCOS(2.0D0*A)
          FN13(R,A)=(6.0D0*(R**4))-(6.0D0*(R**2))+1.0D0
          FN14(R,A)=((4.0D0*(R**4))-(3.0D0*(R**2)))*DSIN(2.0D0*A)
          FN15(R,A)=(R**4)*DSIN(4.0D0*A)
          FN16(R,A)=(R**5)*DCOS(5.0D0*A)
          FN17(R,A)=((5.0D0*(R**5))-(4.0D0*(R**3)))*DCOS(3.0D0*A)
          FN18(R,A)=((10.0D0*(R**5))-(12.0D0*(R**3))+(3.0D0*R))*DCOS(A)
          FN19(R,A)=((10.0D0*(R**5))-(12.0D0*(R**3))+(3.0D0*R))*DSIN(A)
          FN20(R,A)=((5.0D0*(R**5))-(4.0D0*(R**3)))*DSIN(3.0D0*A)
          FN21(R,A)=(R**5)*DSIN(5.0D0*A)
          FN22(R,A)=(R**6)*DCOS(6.0D0*A)
          FN23(R,A)=((6.0D0*(R**6))-(5.0D0*(R**4)))*DCOS(4.0D0*A)
          FN24(R,A)=((15.0D0*(R**6))-(20.0D0*(R**4))+(6.0D0*(R**2)))*
     1    DCOS(2.0D0*A)
          FN25(R,A)=(20.0D0*(R**6))-(30.0D0*(R**4))+(12.0D0*(R**2))-1.0D0
          FN26(R,A)=((15.0D0*(R**6))-(20.0D0*(R**4))+(6.0D0*(R**2)))*
     1    DSIN(2.0D0*A)
          FN27(R,A)=((6.0D0*(R**6))-(5.0D0*(R**4)))*DSIN(4.0D0*A)
          FN28(R,A)=(R**6)*DSIN(6.0D0*A)
C
          FN29(R,A)=(R**7)*DCOS(7.0D0*A)
          FN30(R,A)=((7.0D0*(R**7))-(6.0D0*(R**5)))*DCOS(5.0D0*A)
          FN31(R,A)=((21.0D0*(R**7))-(30.0D0*(R**5))+(10.0D0*(R**3)))*
     1    DCOS(3.0D0*A)
          FN32(R,A)=((35.0D0*(R**7))-(60.0D0*(R**5))+(30.0D0*(R**3))
     2    -(4.0D0*(R)))*DCOS(A)
          FN33(R,A)=((35.0D0*(R**7))-(60.0D0*(R**5))+(30.0D0*(R**3))
     2    -(4.0D0*(R)))*DSIN(A)
          FN34(R,A)=((21.0D0*(R**7))-(30.0D0*(R**5))+(10.0D0*(R**3)))*
     1    DSIN(3.0D0*A)
          FN35(R,A)=((7.0D0*(R**7))-(6.0D0*(R**5)))*DSIN(5.0D0*A)
          FN36(R,A)=(R**7)*DSIN(7.0D0*A)
C
          FN37(R,A)=(R**8)*DCOS(8.0D0*A)
          FN38(R,A)=((8.0D0*(R**8))-(7.0D0*(R**6)))*DCOS(6.0D0*A)
          FN39(R,A)=((28.0D0*(R**8))-(42.0D0*(R**6))+(15.0D0*(R**4)))*
     1    DCOS(4.0D0*A)
          FN40(R,A)=((56.0D0*(R**8))-(105.0D0*(R**6))+(60.0D0*(R**4))
     2    -(10.0D0*(R**2)))*DCOS(2.0D0*A)
          FN41(R,A)=((70.0D0*(R**8))-(140.0D0*(R**6))+(90.0D0*(R**4))
     2    -(20.0D0*(R**2))+1.0D0)
          FN42(R,A)=((56.0D0*(R**8))-(105.0D0*(R**6))+(60.0D0*(R**4))
     2    -(10.0D0*(R**2)))*DSIN(2.0D0*A)
          FN43(R,A)=((28.0D0*(R**8))-(42.0D0*(R**6))+(15.0D0*(R**4)))*
     1    DSIN(4.0D0*A)
          FN44(R,A)=((8.0D0*(R**8))-(7.0D0*(R**6)))*DSIN(6.0D0*A)
          FN45(R,A)=(R**8)*DSIN(8.0D0*A)
C
          FN46(R,A)=(R**9)*DCOS(9.0D0*A)
          FN47(R,A)=((9.0D0*(R**9))-(8.0D0*(R**7)))*DCOS(7.0D0*A)
          FN48(R,A)=((36.0D0*(R**9))-(56.0D0*(R**7))+(21.0D0*(R**5)))*
     1    DCOS(5.0D0*A)
          FN49(R,A)=((84.0D0*(R**9))-(168.0D0*(R**7))+(105.0D0*(R**5))
     2    -(20.0D0*(R**3)))*DCOS(3.0D0*A)
          FN50(R,A)=((126.0D0*(R**9))-(280.0D0*(R**7))+(210.0D0*(R**5))
     2    -(60.0D0*(R**3))+(5.0D0*R))*DCOS(A)
          FN51(R,A)=((126.0D0*(R**9))-(280.0D0*(R**7))+(210.0D0*(R**5))
     2    -(60.0D0*(R**3))+(5.0D0*R))*DSIN(A)
          FN52(R,A)=((84.0D0*(R**9))-(168.0D0*(R**7))+(105.0D0*(R**5))
     2    -(20.0D0*(R**3)))*DSIN(3.0D0*A)
          FN53(R,A)=((36.0D0*(R**9))-(56.0D0*(R**7))+(21.0D0*(R**5)))*
     1    DSIN(5.0D0*A)
          FN54(R,A)=((9.0D0*(R**9))-(8.0D0*(R**7)))*DSIN(7.0D0*A)
          FN55(R,A)=(R**9)*DSIN(9.0D0*A)
C
          FN56(R,A)=(R**10)*DCOS(10.0D0*A)
          FN57(R,A)=((10.0D0*(R**10))-(9.0D0*(R**8)))*DCOS(8.0D0*A)
          FN58(R,A)=((45.0D0*(R**10))-(72.0D0*(R**8))+(28.0D0*(R**6)))*
     1    DCOS(6.0D0*A)
          FN59(R,A)=((120.0D0*(R**10))-(252.0D0*(R**8))+(168.0D0*(R**6))
     2    -(35.0D0*(R**4)))*DCOS(4.0D0*A)
          FN60(R,A)=((210.0D0*(R**10))-(540.0D0*(R**8))+(420.0D0*(R**6))
     2    -(140.0D0*(R**4))+(15.0D0*(R**2)))*DCOS(2.0D0*A)
          FN61(R,A)=(252.0D0*(R**10))-(630.0D0*(R**8))+(560.0D0*(R**6))
     2    -(210.0D0*(R**4))+(30.0D0*(R**2))-1.0D0
          FN62(R,A)=((210.0D0*(R**10))-(540.0D0*(R**8))+(420.0D0*(R**6))
     2    -(140.0D0*(R**4))+(15.0D0*(R**2)))*DSIN(2.0D0*A)
          FN63(R,A)=((120.0D0*(R**10))-(252.0D0*(R**8))+(168.0D0*(R**6))
     2    -(35.0D0*(R**4)))*DSIN(4.0D0*A)
          FN64(R,A)=((45.0D0*(R**10))-(72.0D0*(R**8))+(28.0D0*(R**6)))*
     1    DSIN(6.0D0*A)
          FN65(R,A)=((10.0D0*(R**10))-(9.0D0*(R**8)))*DSIN(8.0D0*A)
          FN66(R,A)=(R**10)*DSIN(10.0D0*A)
          IF(FI.EQ.1) FF2=FN1(RHO,THETA)
          IF(FI.EQ.2) FF2=FN2(RHO,THETA)
          IF(FI.EQ.3) FF2=FN3(RHO,THETA)
          IF(FI.EQ.4) FF2=FN4(RHO,THETA)
          IF(FI.EQ.5) FF2=FN5(RHO,THETA)
          IF(FI.EQ.6) FF2=FN6(RHO,THETA)
          IF(FI.EQ.7) FF2=FN7(RHO,THETA)
          IF(FI.EQ.8) FF2=FN8(RHO,THETA)
          IF(FI.EQ.9) FF2=FN9(RHO,THETA)
          IF(FI.EQ.10) FF2=FN10(RHO,THETA)
          IF(FI.EQ.11) FF2=FN11(RHO,THETA)
          IF(FI.EQ.12) FF2=FN12(RHO,THETA)
          IF(FI.EQ.13) FF2=FN13(RHO,THETA)
          IF(FI.EQ.14) FF2=FN14(RHO,THETA)
          IF(FI.EQ.15) FF2=FN15(RHO,THETA)
          IF(FI.EQ.16) FF2=FN16(RHO,THETA)
          IF(FI.EQ.17) FF2=FN17(RHO,THETA)
          IF(FI.EQ.18) FF2=FN18(RHO,THETA)
          IF(FI.EQ.19) FF2=FN19(RHO,THETA)
          IF(FI.EQ.20) FF2=FN20(RHO,THETA)
          IF(FI.EQ.21) FF2=FN21(RHO,THETA)
          IF(FI.EQ.22) FF2=FN22(RHO,THETA)
          IF(FI.EQ.23) FF2=FN23(RHO,THETA)
          IF(FI.EQ.24) FF2=FN24(RHO,THETA)
          IF(FI.EQ.25) FF2=FN25(RHO,THETA)
          IF(FI.EQ.26) FF2=FN26(RHO,THETA)
          IF(FI.EQ.27) FF2=FN27(RHO,THETA)
          IF(FI.EQ.28) FF2=FN28(RHO,THETA)
          IF(FI.EQ.29) FF2=FN29(RHO,THETA)
          IF(FI.EQ.30) FF2=FN30(RHO,THETA)
          IF(FI.EQ.31) FF2=FN31(RHO,THETA)
          IF(FI.EQ.32) FF2=FN32(RHO,THETA)
          IF(FI.EQ.33) FF2=FN33(RHO,THETA)
          IF(FI.EQ.34) FF2=FN34(RHO,THETA)
          IF(FI.EQ.35) FF2=FN35(RHO,THETA)
          IF(FI.EQ.36) FF2=FN36(RHO,THETA)
          IF(FI.EQ.37) FF2=FN37(RHO,THETA)
          IF(FI.EQ.38) FF2=FN38(RHO,THETA)
          IF(FI.EQ.39) FF2=FN39(RHO,THETA)
          IF(FI.EQ.40) FF2=FN40(RHO,THETA)
          IF(FI.EQ.41) FF2=FN41(RHO,THETA)
          IF(FI.EQ.42) FF2=FN42(RHO,THETA)
          IF(FI.EQ.43) FF2=FN43(RHO,THETA)
          IF(FI.EQ.44) FF2=FN44(RHO,THETA)
          IF(FI.EQ.45) FF2=FN45(RHO,THETA)
          IF(FI.EQ.46) FF2=FN46(RHO,THETA)
          IF(FI.EQ.47) FF2=FN47(RHO,THETA)
          IF(FI.EQ.48) FF2=FN48(RHO,THETA)
          IF(FI.EQ.49) FF2=FN49(RHO,THETA)
          IF(FI.EQ.50) FF2=FN50(RHO,THETA)
          IF(FI.EQ.51) FF2=FN51(RHO,THETA)
          IF(FI.EQ.52) FF2=FN52(RHO,THETA)
          IF(FI.EQ.53) FF2=FN53(RHO,THETA)
          IF(FI.EQ.54) FF2=FN54(RHO,THETA)
          IF(FI.EQ.55) FF2=FN55(RHO,THETA)
          IF(FI.EQ.56) FF2=FN56(RHO,THETA)
          IF(FI.EQ.57) FF2=FN57(RHO,THETA)
          IF(FI.EQ.58) FF2=FN58(RHO,THETA)
          IF(FI.EQ.59) FF2=FN59(RHO,THETA)
          IF(FI.EQ.60) FF2=FN60(RHO,THETA)
          IF(FI.EQ.61) FF2=FN61(RHO,THETA)
          IF(FI.EQ.62) FF2=FN62(RHO,THETA)
          IF(FI.EQ.63) FF2=FN63(RHO,THETA)
          IF(FI.EQ.64) FF2=FN64(RHO,THETA)
          IF(FI.EQ.65) FF2=FN65(RHO,THETA)
          IF(FI.EQ.66) FF2=FN66(RHO,THETA)
          RETURN
      END
C SUB FF3.FOR
      FUNCTION FF3(RHO,THETA,FI)
C
          IMPLICIT NONE
C
          REAL*8 FFN1,FFN2,FFN3,FFN4,
     1    FFN5,FFN6,FFN7,FFN8,FFN9
     3    ,FFN10,FFN11,FFN12,FFN13,FFN14,FFN15,FFN16,FFN17,FFN18,
     1    FFN19,FFN20,FFN21
     4    ,FFN22,FFN23,FFN24,FFN25,FFN26,FFN27,FFN28,FFN29,FFN30
     4    ,FFN31,FFN32,FFN33,FFN34,FFN35,FFN36,FFN37
     5    ,RHO,THETA,FF3,R,A
C
          INTEGER FI
C
C       FUNCTIONS FOR TYPE 3 SURFACE, FRINGE ZERNIKEIS
          FFN1(R,A)=1.0D0+(0.0D0*R)
          FFN2(R,A)=R*DCOS(A)
          FFN3(R,A)=R*DSIN(A)
          FFN4(R,A)=2.0D0*(R**2)-1.0D0
          FFN5(R,A)=(R**2)*DCOS(2.0D0*A)
          FFN6(R,A)=(R**2)*DSIN(2.0D0*A)
          FFN7(R,A)=((3.0D0*(R**3))-2.0D0*R)*DCOS(A)
          FFN8(R,A)=((3.0D0*(R**3))-2.0D0*R)*DSIN(A)
          FFN9(R,A)=(6.0D0*(R**4))-(6.0D0*(R**2))+1.0D0
          FFN10(R,A)=(R**3)*DCOS(3.0D0*A)
          FFN11(R,A)=(R**3)*DSIN(3.0D0*A)
          FFN12(R,A)=((4.0D0*(R**4))-(3.0D0*(R**2)))*DCOS(2.0D0*A)
          FFN13(R,A)=((4.0D0*(R**4))-(3.0D0*(R**2)))*DSIN(2.0D0*A)
          FFN14(R,A)=((10.0D0*(R**5))-(12.0D0*(R**3))+(3.0D0*R))*DCOS(A)
          FFN15(R,A)=((10.0D0*(R**5))-(12.0D0*(R**3))+(3.0D0*R))*DSIN(A)
          FFN16(R,A)=(20.0D0*(R**6))-(30.0D0*(R**4))+(12.0D0*(R**2))-1.0D0
          FFN17(R,A)=(R**4)*DCOS(4.0D0*A)
          FFN18(R,A)=(R**4)*DSIN(4.0D0*A)
          FFN19(R,A)=((5.0D0*(R**5))-(4.0D0*(R**3)))*DCOS(3.0D0*A)
          FFN20(R,A)=((5.0D0*(R**5))-(4.0D0*(R**3)))*DSIN(3.0D0*A)
          FFN21(R,A)=((15.0D0*(R**6))-(20.0D0*(R**4))+(6.0D0*(R**2)))*
     1    DCOS(2.0D0*A)
          FFN22(R,A)=((15.0D0*(R**6))-(20.0D0*(R**4))+(6.0D0*(R**2)))*
     1    DSIN(2.0D0*A)
          FFN23(R,A)=((35.0D0*(R**7))-(60.0D0*(R**5))+(30.0D0*(R**3))
     1    -(4.0D0*R))*DCOS(A)
          FFN24(R,A)=((35.0D0*(R**7))-(60.0D0*(R**5))+(30.0D0*(R**3))
     1    -(4.0D0*R))*DSIN(A)
          FFN25(R,A)=(70.0D0*(R**8))-(140.0D0*(R**6))+(90.0D0*(R**4))
     1    -(20.0D0*(R**2))+(1.0D0)
          FFN26(R,A)=(R**5)*DCOS(5.0D0*A)
          FFN27(R,A)=(R**5)*DSIN(5.0D0*A)
          FFN28(R,A)=((6.0D0*(R**6))-(5.0D0*(R**4)))*DCOS(4.0D0*A)
          FFN29(R,A)=((6.0D0*(R**6))-(5.0D0*(R**4)))*DSIN(4.0D0*A)
          FFN30(R,A)=((21.0D0*(R**7))-(30.0D0*(R**5))+(10.0D0*(R**3)))
     1    *DCOS(3.0D0*A)
          FFN31(R,A)=((21.0D0*(R**7))-(30.0D0*(R**5))+(10.0D0*(R**3)))
     1    *DSIN(3.0D0*A)
          FFN32(R,A)=((56.0D0*(R**8))-(105.0D0*(R**6))+(60.0D0*(R**4))
     1    -(10.0D0*(R**2)))*DCOS(2.0D0*A)
          FFN33(R,A)=((56.0D0*(R**8))-(105.0D0*(R**6))+(60.0D0*(R**4))
     1    -(10.0D0*(R**2)))*DSIN(2.0D0*A)
          FFN34(R,A)=((126.0D0*(R**9))-(280.0D0*(R**7))+(210.0D0*(R**5))
     1    -(60.0D0*(R**3))+(5.0D0*R))*DCOS(A)
          FFN35(R,A)=((126.0D0*(R**9))-(280.0D0*(R**7))+(210.0D0*(R**5))
     1    -(60.0D0*(R**3))+(5.0D0*R))*DSIN(A)
          FFN36(R,A)=(252.0D0*(R**10))-(630.0D0*(R**8))+(560.0D0*(R**6))
     1    -(210.0D0*(R**4))+(30.0D0*(R**2))-1.0D0
          FFN37(R,A)=(924.0D0*(R**12))-(2772.0D0*(R**10))+
     1    (3150.0D0*(R**8))
     2    -(1680.0D0*(R**6))+(420.0D0*(R**4))-(42.0D0*(R**2))+1.0D0
C
          IF(FI.EQ.1) FF3=FFN1(RHO,THETA)
          IF(FI.EQ.2) FF3=FFN2(RHO,THETA)
          IF(FI.EQ.3) FF3=FFN3(RHO,THETA)
          IF(FI.EQ.4) FF3=FFN4(RHO,THETA)
          IF(FI.EQ.5) FF3=FFN5(RHO,THETA)
          IF(FI.EQ.6) FF3=FFN6(RHO,THETA)
          IF(FI.EQ.7) FF3=FFN7(RHO,THETA)
          IF(FI.EQ.8) FF3=FFN8(RHO,THETA)
          IF(FI.EQ.9) FF3=FFN9(RHO,THETA)
          IF(FI.EQ.10) FF3=FFN10(RHO,THETA)
          IF(FI.EQ.11) FF3=FFN11(RHO,THETA)
          IF(FI.EQ.12) FF3=FFN12(RHO,THETA)
          IF(FI.EQ.13) FF3=FFN13(RHO,THETA)
          IF(FI.EQ.14) FF3=FFN14(RHO,THETA)
          IF(FI.EQ.15) FF3=FFN15(RHO,THETA)
          IF(FI.EQ.16) FF3=FFN16(RHO,THETA)
          IF(FI.EQ.17) FF3=FFN17(RHO,THETA)
          IF(FI.EQ.18) FF3=FFN18(RHO,THETA)
          IF(FI.EQ.19) FF3=FFN19(RHO,THETA)
          IF(FI.EQ.20) FF3=FFN20(RHO,THETA)
          IF(FI.EQ.21) FF3=FFN21(RHO,THETA)
          IF(FI.EQ.22) FF3=FFN22(RHO,THETA)
          IF(FI.EQ.23) FF3=FFN23(RHO,THETA)
          IF(FI.EQ.24) FF3=FFN24(RHO,THETA)
          IF(FI.EQ.25) FF3=FFN25(RHO,THETA)
          IF(FI.EQ.26) FF3=FFN26(RHO,THETA)
          IF(FI.EQ.27) FF3=FFN27(RHO,THETA)
          IF(FI.EQ.28) FF3=FFN28(RHO,THETA)
          IF(FI.EQ.29) FF3=FFN29(RHO,THETA)
          IF(FI.EQ.30) FF3=FFN30(RHO,THETA)
          IF(FI.EQ.31) FF3=FFN31(RHO,THETA)
          IF(FI.EQ.32) FF3=FFN32(RHO,THETA)
          IF(FI.EQ.33) FF3=FFN33(RHO,THETA)
          IF(FI.EQ.34) FF3=FFN34(RHO,THETA)
          IF(FI.EQ.35) FF3=FFN35(RHO,THETA)
          IF(FI.EQ.36) FF3=FFN36(RHO,THETA)
          IF(FI.EQ.37) FF3=FFN37(RHO,THETA)
          RETURN
      END
C SUB FF4.FOR
      FUNCTION FF4(X,Y,FI)
C
          IMPLICIT NONE
C
          REAL*8 FFN1,FFN2,FFN3,FFN4,FFN40,FFN41,FFN42,FFN43,
     1    FFN5,FFN6,FFN7,FFN8,FFN9,FFN44,FFN45,FFN46,FFN47,FFN48,FFN49,
     3    FFN10,FFN11,FFN12,FFN13,FFN14,FFN15,FFN16,FFN17,FFN18,FFN57,
     1    FFN19,FFN20,FFN21,FFN50,FFN51,FFN52,FFN53,FFN54,FFN55,FFN56,
     4    FFN22,FFN23,FFN24,FFN25,FFN26,FFN27,FFN28,FFN29,FFN30,FFN58,
     4    FFN31,FFN32,FFN33,FFN34,FFN35,FFN36,FFN37,FFN38,FFN39,FFN59,
     5    X,Y,FF4,XX,YY,FFN60,FFN61,FFN62,FFN63,FFN64,FFN65,FFN66,FFN67,
     5    FFN68,FFN69,FFN70,FFN71,FFN72,FFN73,FFN74,FFN75,FFN76,FFN77,
     5    FFN78,FFN79,FFN80,FFN81,FFN82,FFN83,FFN84,FFN85,FFN86,FFN87,
     5    FFN88,FFN89,FFN90,FFN91
C
          INTEGER FI
C
C       FUNCTIONS FOR TYPE 4 SURFACE, RECTANGULAR POLYNOMIAL
C
          FFN1(XX,YY)=1.0D0
          FFN2(XX,YY)=XX
          FFN3(XX,YY)=YY
          FFN4(XX,YY)=XX**2
          FFN5(XX,YY)=XX*YY
          FFN6(XX,YY)=YY**2
          FFN7(XX,YY)=XX**3
          FFN8(XX,YY)=(XX**2)*(YY)
          FFN9(XX,YY)=(XX)*(YY**2)
          FFN10(XX,YY)=YY**3
          FFN11(XX,YY)=XX**4
          FFN12(XX,YY)=(XX**3)*(YY)
          FFN13(XX,YY)=(XX**2)*(YY**2)
          FFN14(XX,YY)=(XX)*(YY**3)
          FFN15(XX,YY)=(YY**4)
          FFN16(XX,YY)=(XX**5)
          FFN17(XX,YY)=(XX**4)*(YY)
          FFN18(XX,YY)=(XX**3)*(YY**2)
          FFN19(XX,YY)=(XX**2)*(YY**3)
          FFN20(XX,YY)=(XX)*(YY**4)
          FFN21(XX,YY)=(YY**5)
          FFN22(XX,YY)=(XX**6)
          FFN23(XX,YY)=(XX**5)*(YY)
          FFN24(XX,YY)=(XX**4)*(YY**2)
          FFN25(XX,YY)=(XX**3)*(YY**3)
          FFN26(XX,YY)=(XX**2)*(YY**4)
          FFN27(XX,YY)=(XX)*(YY**5)
          FFN28(XX,YY)=(YY**6)
          FFN29(XX,YY)=(XX**7)
          FFN30(XX,YY)=(XX**6)*(YY)
          FFN31(XX,YY)=(XX**5)*(YY**2)
          FFN32(XX,YY)=(XX**4)*(YY**3)
          FFN33(XX,YY)=(XX**3)*(YY**4)
          FFN34(XX,YY)=(XX**2)*(YY**5)
          FFN35(XX,YY)=(XX)*(YY**6)
          FFN36(XX,YY)=(YY**7)
          FFN37(XX,YY)=(XX**8)
          FFN38(XX,YY)=(XX**7)*(YY)
          FFN39(XX,YY)=(XX**6)*(YY**2)
          FFN40(XX,YY)=(XX**5)*(YY**3)
          FFN41(XX,YY)=(XX**4)*(YY**4)
          FFN42(XX,YY)=(XX**3)*(YY**5)
          FFN43(XX,YY)=(XX**2)*(YY**6)
          FFN44(XX,YY)=(XX)*(YY**7)
          FFN45(XX,YY)=(YY**8)
          FFN46(XX,YY)=(XX**9)
          FFN47(XX,YY)=(XX**8)*(YY)
          FFN48(XX,YY)=(XX**7)*(YY**2)
          FFN49(XX,YY)=(XX**6)*(YY**3)
          FFN50(XX,YY)=(XX**5)*(YY**4)
          FFN51(XX,YY)=(XX**4)*(YY**5)
          FFN52(XX,YY)=(XX**3)*(YY**6)
          FFN53(XX,YY)=(XX**2)*(YY**7)
          FFN54(XX,YY)=(XX)*(YY**8)
          FFN55(XX,YY)=(YY**9)
          FFN56(XX,YY)=(XX**10)
          FFN57(XX,YY)=(XX**9)*(YY)
          FFN58(XX,YY)=(XX**8)*(YY**2)
          FFN59(XX,YY)=(XX**7)*(YY**3)
          FFN60(XX,YY)=(XX**6)*(YY**4)
          FFN61(XX,YY)=(XX**5)*(YY**5)
          FFN62(XX,YY)=(XX**4)*(YY**6)
          FFN63(XX,YY)=(XX**3)*(YY**7)
          FFN64(XX,YY)=(XX**2)*(YY**8)
          FFN65(XX,YY)=(XX)*(YY**9)
          FFN66(XX,YY)=(YY**10)
          FFN67(XX,YY)=(XX**11)
          FFN68(XX,YY)=(XX**10)*(YY)
          FFN69(XX,YY)=(XX**9)*(YY**2)
          FFN70(XX,YY)=(XX**8)*(YY**3)
          FFN71(XX,YY)=(XX**7)*(YY**4)
          FFN72(XX,YY)=(XX**6)*(YY**5)
          FFN73(XX,YY)=(XX**5)*(YY**6)
          FFN74(XX,YY)=(XX**4)*(YY**7)
          FFN75(XX,YY)=(XX**3)*(YY**8)
          FFN76(XX,YY)=(XX**2)*(YY**9)
          FFN77(XX,YY)=(XX)*(YY**10)
          FFN78(XX,YY)=(YY**11)
          FFN79(XX,YY)=(XX**12)
          FFN80(XX,YY)=(XX**11)*(YY)
          FFN81(XX,YY)=(XX**10)*(YY**2)
          FFN82(XX,YY)=(XX**9)*(YY**3)
          FFN83(XX,YY)=(XX**8)*(YY**4)
          FFN84(XX,YY)=(XX**7)*(YY**5)
          FFN85(XX,YY)=(XX**6)*(YY**6)
          FFN86(XX,YY)=(XX**5)*(YY**7)
          FFN87(XX,YY)=(XX**4)*(YY**8)
          FFN88(XX,YY)=(XX**3)*(YY**9)
          FFN89(XX,YY)=(XX**2)*(YY**10)
          FFN90(XX,YY)=(XX)*(YY**11)
          FFN91(XX,YY)=(YY**12)
C
          IF(FI.EQ.1) FF4=FFN1(X,Y)
          IF(FI.EQ.2) FF4=FFN2(X,Y)
          IF(FI.EQ.3) FF4=FFN3(X,Y)
          IF(FI.EQ.4) FF4=FFN4(X,Y)
          IF(FI.EQ.5) FF4=FFN5(X,Y)
          IF(FI.EQ.6) FF4=FFN6(X,Y)
          IF(FI.EQ.7) FF4=FFN7(X,Y)
          IF(FI.EQ.8) FF4=FFN8(X,Y)
          IF(FI.EQ.9) FF4=FFN9(X,Y)
          IF(FI.EQ.10) FF4=FFN10(X,Y)
          IF(FI.EQ.11) FF4=FFN11(X,Y)
          IF(FI.EQ.12) FF4=FFN12(X,Y)
          IF(FI.EQ.13) FF4=FFN13(X,Y)
          IF(FI.EQ.14) FF4=FFN14(X,Y)
          IF(FI.EQ.15) FF4=FFN15(X,Y)
          IF(FI.EQ.16) FF4=FFN16(X,Y)
          IF(FI.EQ.17) FF4=FFN17(X,Y)
          IF(FI.EQ.18) FF4=FFN18(X,Y)
          IF(FI.EQ.19) FF4=FFN19(X,Y)
          IF(FI.EQ.20) FF4=FFN20(X,Y)
          IF(FI.EQ.21) FF4=FFN21(X,Y)
          IF(FI.EQ.22) FF4=FFN22(X,Y)
          IF(FI.EQ.23) FF4=FFN23(X,Y)
          IF(FI.EQ.24) FF4=FFN24(X,Y)
          IF(FI.EQ.25) FF4=FFN25(X,Y)
          IF(FI.EQ.26) FF4=FFN26(X,Y)
          IF(FI.EQ.27) FF4=FFN27(X,Y)
          IF(FI.EQ.28) FF4=FFN28(X,Y)
          IF(FI.EQ.29) FF4=FFN29(X,Y)
          IF(FI.EQ.30) FF4=FFN30(X,Y)
          IF(FI.EQ.31) FF4=FFN31(X,Y)
          IF(FI.EQ.32) FF4=FFN32(X,Y)
          IF(FI.EQ.33) FF4=FFN33(X,Y)
          IF(FI.EQ.34) FF4=FFN34(X,Y)
          IF(FI.EQ.35) FF4=FFN35(X,Y)
          IF(FI.EQ.36) FF4=FFN36(X,Y)
          IF(FI.EQ.37) FF4=FFN37(X,Y)
          IF(FI.EQ.38) FF4=FFN38(X,Y)
          IF(FI.EQ.39) FF4=FFN39(X,Y)
          IF(FI.EQ.40) FF4=FFN40(X,Y)
          IF(FI.EQ.41) FF4=FFN41(X,Y)
          IF(FI.EQ.42) FF4=FFN42(X,Y)
          IF(FI.EQ.43) FF4=FFN43(X,Y)
          IF(FI.EQ.44) FF4=FFN44(X,Y)
          IF(FI.EQ.45) FF4=FFN45(X,Y)
          IF(FI.EQ.46) FF4=FFN46(X,Y)
          IF(FI.EQ.47) FF4=FFN47(X,Y)
          IF(FI.EQ.48) FF4=FFN48(X,Y)
          IF(FI.EQ.49) FF4=FFN49(X,Y)
          IF(FI.EQ.50) FF4=FFN50(X,Y)
          IF(FI.EQ.51) FF4=FFN51(X,Y)
          IF(FI.EQ.52) FF4=FFN52(X,Y)
          IF(FI.EQ.53) FF4=FFN53(X,Y)
          IF(FI.EQ.54) FF4=FFN54(X,Y)
          IF(FI.EQ.55) FF4=FFN55(X,Y)
          IF(FI.EQ.56) FF4=FFN56(X,Y)
          IF(FI.EQ.57) FF4=FFN57(X,Y)
          IF(FI.EQ.58) FF4=FFN58(X,Y)
          IF(FI.EQ.59) FF4=FFN59(X,Y)
          IF(FI.EQ.60) FF4=FFN60(X,Y)
          IF(FI.EQ.61) FF4=FFN61(X,Y)
          IF(FI.EQ.62) FF4=FFN62(X,Y)
          IF(FI.EQ.63) FF4=FFN63(X,Y)
          IF(FI.EQ.64) FF4=FFN64(X,Y)
          IF(FI.EQ.65) FF4=FFN65(X,Y)
          IF(FI.EQ.66) FF4=FFN66(X,Y)
          IF(FI.EQ.67) FF4=FFN67(X,Y)
          IF(FI.EQ.68) FF4=FFN68(X,Y)
          IF(FI.EQ.69) FF4=FFN69(X,Y)
          IF(FI.EQ.70) FF4=FFN70(X,Y)
          IF(FI.EQ.71) FF4=FFN71(X,Y)
          IF(FI.EQ.72) FF4=FFN72(X,Y)
          IF(FI.EQ.73) FF4=FFN73(X,Y)
          IF(FI.EQ.74) FF4=FFN74(X,Y)
          IF(FI.EQ.75) FF4=FFN75(X,Y)
          IF(FI.EQ.76) FF4=FFN76(X,Y)
          IF(FI.EQ.77) FF4=FFN77(X,Y)
          IF(FI.EQ.78) FF4=FFN78(X,Y)
          IF(FI.EQ.79) FF4=FFN79(X,Y)
          IF(FI.EQ.80) FF4=FFN80(X,Y)
          IF(FI.EQ.81) FF4=FFN81(X,Y)
          IF(FI.EQ.82) FF4=FFN82(X,Y)
          IF(FI.EQ.83) FF4=FFN83(X,Y)
          IF(FI.EQ.84) FF4=FFN84(X,Y)
          IF(FI.EQ.85) FF4=FFN85(X,Y)
          IF(FI.EQ.86) FF4=FFN86(X,Y)
          IF(FI.EQ.87) FF4=FFN87(X,Y)
          IF(FI.EQ.88) FF4=FFN88(X,Y)
          IF(FI.EQ.89) FF4=FFN89(X,Y)
          IF(FI.EQ.90) FF4=FFN90(X,Y)
          IF(FI.EQ.91) FF4=FFN91(X,Y)
          RETURN
      END
C SUB FF5.FOR
      FUNCTION FF5(RHO,THETA,FI)
C
          IMPLICIT NONE
C
          REAL*8 FFN1,FFN2,FFN3,FFN4,FFN43,FFN44,FFN45,
     1    FFN5,FFN6,FFN7,FFN8,FFN9,FFN38,FFN39,FFN40,FFN41,FFN42
     3    ,FFN10,FFN11,FFN12,FFN13,FFN14,FFN15,FFN16,FFN17,FFN18,
     1    FFN19,FFN20,FFN21,FFN46,FFN47,FFN48
     4    ,FFN22,FFN23,FFN24,FFN25,FFN26,FFN27,FFN28,FFN29,FFN30
     4    ,FFN31,FFN32,FFN33,FFN34,FFN35,FFN36,FFN37
     5    ,RHO,THETA,FF5,R,A
C
          INTEGER FI
C
C       FUNCTIONS FOR TYPE 5 SURFACE, FRINGE ZERNIKES
          FFN1(R,A)=1.0D0
          FFN2(R,A)=R*DCOS(A)
          FFN3(R,A)=R*DSIN(A)
          FFN4(R,A)=R**2
          FFN5(R,A)=(R**2)*DCOS(2.0D0*A)
          FFN6(R,A)=(R**2)*DSIN(2.0D0*A)
          FFN7(R,A)=(R**3)*DCOS(A)
          FFN8(R,A)=(R**3)*DSIN(A)
          FFN9(R,A)=(R**3)*DCOS(3.0D0*A)
          FFN10(R,A)=(R**3)*DSIN(3.0D0*A)
          FFN11(R,A)=R**4
          FFN12(R,A)=(R**4)*DCOS(2.0D0*A)
          FFN13(R,A)=(R**4)*DSIN(2.0D0*A)
          FFN14(R,A)=(R**4)*DCOS(4.0D0*A)
          FFN15(R,A)=(R**4)*DSIN(4.0D0*A)
          FFN16(R,A)=(R**5)*DCOS(A)
          FFN17(R,A)=(R**5)*DSIN(A)
          FFN18(R,A)=(R**5)*DCOS(3.0D0*A)
          FFN19(R,A)=(R**5)*DSIN(3.0D0*A)
          FFN20(R,A)=(R**5)*DCOS(5.0D0*A)
          FFN21(R,A)=(R**5)*DSIN(5.0D0*A)
          FFN22(R,A)=(R**6)
          FFN23(R,A)=(R**8)
          FFN24(R,A)=(R**10)
          FFN25(R,A)=(R**6)*DCOS(2.0D0*A)
          FFN26(R,A)=(R**6)*DSIN(2.0D0*A)
          FFN27(R,A)=(R**6)*DCOS(4.0D0*A)
          FFN28(R,A)=(R**6)*DSIN(4.0D0*A)
          FFN29(R,A)=(R**6)*DCOS(6.0D0*A)
          FFN30(R,A)=(R**6)*DSIN(6.0D0*A)
          FFN31(R,A)=(R**7)*DCOS(A)
          FFN32(R,A)=(R**7)*DSIN(A)
          FFN33(R,A)=(R**7)*DCOS(3.0D0*A)
          FFN34(R,A)=(R**7)*DSIN(3.0D0*A)
          FFN35(R,A)=(R**7)*DCOS(5.0D0*A)
          FFN36(R,A)=(R**7)*DSIN(5.0D0*A)
          FFN37(R,A)=(R**7)*DCOS(7.0D0*A)
          FFN38(R,A)=(R**7)*DSIN(7.0D0*A)
          FFN39(R,A)=(R**8)*DCOS(2.0D0*A)
          FFN40(R,A)=(R**8)*DSIN(2.0D0*A)
          FFN41(R,A)=(R**8)*DCOS(4.0D0*A)
          FFN42(R,A)=(R**8)*DSIN(4.0D0*A)
          FFN43(R,A)=(R**8)*DCOS(6.0D0*A)
          FFN44(R,A)=(R**8)*DSIN(6.0D0*A)
          FFN45(R,A)=(R**8)*DCOS(8.0D0*A)
          FFN46(R,A)=(R**8)*DSIN(8.0D0*A)
          FFN47(R,A)=R**12
          FFN48(R,A)=R**14
C
          IF(FI.EQ.1) FF5=FFN1(RHO,THETA)
          IF(FI.EQ.2) FF5=FFN2(RHO,THETA)
          IF(FI.EQ.3) FF5=FFN3(RHO,THETA)
          IF(FI.EQ.4) FF5=FFN4(RHO,THETA)
          IF(FI.EQ.5) FF5=FFN5(RHO,THETA)
          IF(FI.EQ.6) FF5=FFN6(RHO,THETA)
          IF(FI.EQ.7) FF5=FFN7(RHO,THETA)
          IF(FI.EQ.8) FF5=FFN8(RHO,THETA)
          IF(FI.EQ.9) FF5=FFN9(RHO,THETA)
          IF(FI.EQ.10) FF5=FFN10(RHO,THETA)
          IF(FI.EQ.11) FF5=FFN11(RHO,THETA)
          IF(FI.EQ.12) FF5=FFN12(RHO,THETA)
          IF(FI.EQ.13) FF5=FFN13(RHO,THETA)
          IF(FI.EQ.14) FF5=FFN14(RHO,THETA)
          IF(FI.EQ.15) FF5=FFN15(RHO,THETA)
          IF(FI.EQ.16) FF5=FFN16(RHO,THETA)
          IF(FI.EQ.17) FF5=FFN17(RHO,THETA)
          IF(FI.EQ.18) FF5=FFN18(RHO,THETA)
          IF(FI.EQ.19) FF5=FFN19(RHO,THETA)
          IF(FI.EQ.20) FF5=FFN20(RHO,THETA)
          IF(FI.EQ.21) FF5=FFN21(RHO,THETA)
          IF(FI.EQ.22) FF5=FFN22(RHO,THETA)
          IF(FI.EQ.23) FF5=FFN23(RHO,THETA)
          IF(FI.EQ.24) FF5=FFN24(RHO,THETA)
          IF(FI.EQ.25) FF5=FFN25(RHO,THETA)
          IF(FI.EQ.26) FF5=FFN26(RHO,THETA)
          IF(FI.EQ.27) FF5=FFN27(RHO,THETA)
          IF(FI.EQ.28) FF5=FFN28(RHO,THETA)
          IF(FI.EQ.29) FF5=FFN29(RHO,THETA)
          IF(FI.EQ.30) FF5=FFN30(RHO,THETA)
          IF(FI.EQ.31) FF5=FFN31(RHO,THETA)
          IF(FI.EQ.32) FF5=FFN32(RHO,THETA)
          IF(FI.EQ.33) FF5=FFN33(RHO,THETA)
          IF(FI.EQ.34) FF5=FFN34(RHO,THETA)
          IF(FI.EQ.35) FF5=FFN35(RHO,THETA)
          IF(FI.EQ.36) FF5=FFN36(RHO,THETA)
          IF(FI.EQ.37) FF5=FFN37(RHO,THETA)
          IF(FI.EQ.38) FF5=FFN38(RHO,THETA)
          IF(FI.EQ.39) FF5=FFN39(RHO,THETA)
          IF(FI.EQ.40) FF5=FFN40(RHO,THETA)
          IF(FI.EQ.41) FF5=FFN41(RHO,THETA)
          IF(FI.EQ.42) FF5=FFN42(RHO,THETA)
          IF(FI.EQ.43) FF5=FFN43(RHO,THETA)
          IF(FI.EQ.44) FF5=FFN44(RHO,THETA)
          IF(FI.EQ.45) FF5=FFN45(RHO,THETA)
          IF(FI.EQ.46) FF5=FFN46(RHO,THETA)
          IF(FI.EQ.47) FF5=FFN47(RHO,THETA)
          IF(FI.EQ.48) FF5=FFN48(RHO,THETA)
          RETURN
      END
C SUB LFCOEFS.FOR
      SUBROUTINE LFCOEFS
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE LFCOEFS. THIS IS THE SUBROUTINE WHICH
C       HANDLES THE "LISTCOEF" COMMAND FROM THE SPFIT PROGRAM LEVEL.
C
          INTEGER I
C
          REAL*8 STYPE,SSURF
C
          COMMON/ST/STYPE
C
          COMMON/SSS/SSURF
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
C
          IF(SST.EQ.1.OR.SQ.EQ.1.OR.SN.EQ.1)
     1    THEN
              OUTLYNE='"LISTCOEF" TAKES NO EXPLICIT INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          ELSE
C       PROCEED
          END IF
C
C       CHECK THAT TYPE OR SURF WAS ENTERED
          IF(F23.EQ.1) THEN
C       OUTPUT TYPE COEFFICIENTS
              WRITE(OUTLYNE,1212)
              CALL SHOWIT(0)
 1212         FORMAT(1X)
              WRITE(OUTLYNE,120)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,119) INT(STYPE)
              CALL SHOWIT(0)
 119          FORMAT('FITTING TYPE = ',I2)
 120          FORMAT('THE COEFFICIENTS ARE:')
              DO 10 I=1,96
                  WRITE(OUTLYNE,110) I,CFTYPE(I)
                  CALL SHOWIT(0)
 10           CONTINUE
 110          FORMAT('COEF # ',I3,3X,'COEF VALUE = ',D23.15)
              RETURN
          ELSE
C       PROCEED
          END IF
          IF(F23.EQ.2) THEN
C       OUTPUT SURFACE COEFFICIENTS
              WRITE(OUTLYNE,1212)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,129) SURF,INT(SSURF)
              CALL SHOWIT(0)
 129          FORMAT(
     1        'FOR SURFACE NUMBER = ',I3,' WITH SURFACE TYPE = ',I2)
              WRITE(OUTLYNE,130)
              CALL SHOWIT(0)
 130          FORMAT('THE COEFFICIENTS ARE:')
              DO 20 I=1,96
                  WRITE(OUTLYNE,110) I,FTFL01(SURF,I)
                  CALL SHOWIT(0)
 20           CONTINUE
              RETURN
          ELSE
C       PROCEED
          END IF
      END
C SUB FCOEFS.FOR
      SUBROUTINE FCOEFS
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE FCOEFS. THIS IS THE SUBROUTINE WHICH
C       HANDLES THE "COEFS" COMMAND FROM THE SPFIT PROGRAM LEVEL.
C
          INTEGER I
C
          REAL*8 STYPE,SSURF
C
          COMMON/ST/STYPE
C
          COMMON/SSS/SSURF
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
C
          IF(SST.EQ.1.OR.SQ.EQ.1.OR.SN.EQ.1)
     1    THEN
              OUTLYNE='"COEFS" TAKES NO EXPLICIT INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          ELSE
C       PROCEED
          END IF
C
C       OUTPUT TYPE COEFFICIENTS
          WRITE(OUTLYNE,1212)
          CALL SHOWIT(0)
 1212     FORMAT(1X)
          WRITE(OUTLYNE,120)
          CALL SHOWIT(0)
          WRITE(OUTLYNE,119) INT(STYPE)
          CALL SHOWIT(0)
 119      FORMAT('FITTING TYPE = ',I2)
 120      FORMAT('THE COEFFICIENTS ARE:')
          DO 10 I=1,96,4
              WRITE(OUTLYNE,110)
     1        CFTYPE(I),CFTYPE(I+1),CFTYPE(I+2),CFTYPE(I+3)
              CALL SHOWIT(0)
 10       CONTINUE
 110      FORMAT(D17.10,',',D17.10,',',D17.10,',',D17.10)
          RETURN
      END
C SUB CLEARREG.FOR
      SUBROUTINE CLEARREG
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE CLEARREG. THIS IS THE SUBROUTINE WHICH
C       HANDLES THE "CLEARREG"
C
          INTEGER I,J,K
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datsub.inc'
C
          IF(SST.EQ.1.OR.SQ.EQ.1.OR.S3.EQ.1.OR.
     1    S4.EQ.1.OR.S5.EQ.1) THEN
              OUTLYNE=
     1        '"CLEARREG" ONLY TAKES NUMERIC WORD #1 AND #2 INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          J=INT(W1)
          K=INT(W2)
          IF(DF1.EQ.1) J=1
          IF(DF2.EQ.1) K=MAXREG
          IF(J.GE.K) THEN
              OUTLYNE=
     1        'NUMERIC WORD #1 MUST BE LESS THAN NUMERIC WORD #2 INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(J.LT.1) THEN
              OUTLYNE=
     1        'NUMERIC WORD #1 MUST BE GREATER THAN ZERO'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(K.GT.MAXREG) THEN
              WRITE(OUTLYNE,*)
     1        'NUMERIC WORD #2 MUST BE LESS THAN ',MAXREG
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C       INPUT FORM OK SO FAR
C       ZERO THE COEFS
          DO I=J,K
              GPREG(I)=0.0D0
          END DO
          WRITE(OUTLYNE,*)
     1    'GENERAL PURPOSE STRORAGE REGISTERS ',J,' THROUGH ',K
          CALL SHOWIT(1)
          OUTLYNE=
     1    'HAVE BEEN RESET TO 0.0'
          CALL SHOWIT(1)
          RETURN
      END
