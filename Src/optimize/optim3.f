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

C       THIRD SET OF OPTIMIZATION ROUTINES

C SUB CVRBL1.FOR
      SUBROUTINE CVRBL1
C
          IMPLICIT NONE
C
          INCLUDE 'datsub.inc'
          INCLUDE 'datmai.inc'
C
!      LOGICAL YES
C
!      INTEGER I,J
C
C       THIS IS SUBROUTINE CVRBL1. THIS IS THE SUBROUTINE WHICH
C       INITIALLY STARTS THE SETUP OF A NEW COMP VARIABLE SUBFILE
C       THE CMD LEVEL IS DISABLED AND FLAG F52 IS SET TO 1.
C
          IF(SQ.EQ.1.OR.SST.EQ.1.OR.SN.EQ.1) THEN
              WRITE(OUTLYNE,*)
     1        '"',WC,'" TAKES NO EXPLICIT INPUT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          F1=0
          F52=1
C
C       SET THE COUNTER TO THE TOP OF THE VARIABLE ARRAY STRUCTURE.
C       (VARIABLE VBCNT)
          VBCNT=0
          ISCOMP(1:MAXCMP)=.FALSE.
          VARABL(1:MAXCMP,1:17)=0.0D0
C
C       THE MAXIMUM NUMBER OF COMP VARIABLES ALLOWABLE
C       IS SET IN PROGRAM.FOR WITH THE VALUE MAXCMP WHICH IS
C       PASSED IN A COMMON IN THE INCLUDE FILE DATSUB.FOR
C
          RETURN
      END


C SUB VRBL1.FOR
      SUBROUTINE VRBL1
C
          IMPLICIT NONE
C
          INCLUDE 'datsub.inc'
          INCLUDE 'datmai.inc'
C
!      INTEGER I,J
C
C       THIS IS SUBROUTINE VRBL1. THIS IS THE SUBROUTINE WHICH
C       INITIALLY STARTS THE SETUP OF A NEW VARIABLE SUBFILE
C       THE CMD LEVEL IS DISABLED AND FLAG F29 IS SET TO 1.
C
          IF(SQ.EQ.1.OR.SST.EQ.1.OR.SN.EQ.1) THEN
              WRITE(OUTLYNE,*)
     1        '"',WC,'" TAKES NO EXPLICIT INPUT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          F1=0
          F29=1
C
C       SET THE COUNTER TO THE TOP OF THE VARIABLE ARRAY STRUCTURE.
C       (VARIABLE VBCNT)
          VBCNT=0
          TVBCNT=0
C
C       THE MAXIMUM NUMBER OF VARIABLES ALLOWABLE
C       IS SET IN PROGRAM.FOR WITH THE VALUE MAXVB WHICH IS
C       PASSED IN A COMMON IN THE INCLUDE FILE DATSUB.FOR
C
C       THE ARRAY VARIABLE STORES VARIABLE INFORMATION
C       IT IS PASSED IN COMMON IN THE INCLUDE FILE DATSUB.FOR
C
C                       SET VBCFG=1
          VBCFG=1
C     THIS MAKES CFG 1 THE DEFAULT CONFIG FOR VARIABLES
C
          RETURN
      END


C SUB TVRBL1.FOR
      SUBROUTINE TVRBL1
C
          IMPLICIT NONE
C
          INCLUDE 'datsub.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
!      INTEGER I,J
C
C       THIS IS SUBROUTINE TVRBL1. THIS IS THE SUBROUTINE WHICH
C       INITIALLY STARTS THE SETUP OF A NEW TOLERANCE VARIABLE SUBFILE
C       THE CMD LEVEL IS DISABLED AND FLAG F51 IS SET TO 1.
C
          IF(SQ.EQ.1.OR.SST.EQ.1.OR.SN.EQ.1) THEN
              WRITE(OUTLYNE,*)
     1        '"',WC,'" TAKES NO EXPLICIT INPUT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          F1=0
          F51=1
C
C       SET THE COUNTER TO THE TOP OF THE TVAR ARRAY STRUCTURE.
C       (VARIABLE TVBCNT)
          TVBCNT=0
          VBCNT=0
C     ZERO THE TOLER ARRAY
          TOLER(1:60,0:MAXSUR)=0.0D0
          ASTILTXP=0.0D0
          ASTILTYP=0.0D0
          ASTILTZP=0.0D0
          BSTILTXP=0.0D0
          BSTILTYP=0.0D0
          BSTILTZP=0.0D0
          GSTILTXP=0.0D0
          GSTILTYP=0.0D0
          GSTILTZP=0.0D0
          ABTILTXP=0.0D0
          ABTILTYP=0.0D0
          ABTILTZP=0.0D0
          BBTILTXP=0.0D0
          BBTILTYP=0.0D0
          BBTILTZP=0.0D0
          GBTILTXP=0.0D0
          GBTILTYP=0.0D0
          GBTILTZP=0.0D0
C
C       THE MAXIMUM NUMBER OF TVARIABLES ALLOWABLE
C       IS SET TO VALUE MAXTVB. MAXTVB IS
C       PASSED IN A COMMON IN THE INCLUDE FILE DATSUB.FOR
C
C       THE ARRAY VARIABLE STORES VARIABLE INFORMATION
C       IT IS PASSED IN COMMON IN THE INCLUDE FILE DATSUB.FOR
C
          RETURN
      END


C     SUB UCVRBL1.FOR
      SUBROUTINE UCVRBL1
C
          IMPLICIT NONE
C
          INCLUDE 'datsub.inc'
          INCLUDE 'datmai.inc'
C
C       THIS IS SUBROUTINE UCVRBL1. THIS IS THE SUBROUTINE WHICH
C       STARTS AN UPDATE COMPENSATION VARIABLE SESSION.
C       THE CMD LEVEL IS DISABLED AND FLAG F52 IS SET TO 2.
C
          IF(SST.EQ.1.OR.SN.EQ.1) THEN
              WRITE(OUTLYNE,*)
     1        '"UPDATE COMPVAR" TAKES NO EXPLICIT INPUT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          F1=0
          F52=2
C
C       ANY OTHER ACTION TO BE TAKEN DURING RAYSET UPDATE IS HANDLED
C       FROM WITHIN SUBROUTINE CVARBLL.FOR.
C
          RETURN
      END


C     SUB UVRBL1.FOR
      SUBROUTINE UVRBL1
C
          IMPLICIT NONE
C
          INCLUDE 'datsub.inc'
          INCLUDE 'datmai.inc'
C
C       THIS IS SUBROUTINE UVRBL1. THIS IS THE SUBROUTINE WHICH
C       STARTS AN UPDATE VARIABLE SESSION.
C       THE CMD LEVEL IS DISABLED AND FLAG F29 IS SET TO 2.
C
          IF(SST.EQ.1.OR.SN.EQ.1) THEN
              WRITE(OUTLYNE,*)
     1        '"UPDATE VARIABLE" TAKES NO EXPLICIT INPUT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          F1=0
          F29=2
C
C                       SET VBCFG=1
          VBCFG=1
C     THIS MAKES CFG 1 THE DEFAULT CONFIG FOR VARIABLES
C
C       ANY OTHER ACTION TO BE TAKEN DURING RAYSET UPDATE IS HANDLED
C       FROM WITHIN SUBROUTINE VARBLL.FOR.
C
          RETURN
      END


C     SUB TUVRBL1.FOR
      SUBROUTINE TUVRBL1
C
          IMPLICIT NONE
C
          INCLUDE 'datsub.inc'
          INCLUDE 'datmai.inc'
C
C       THIS IS SUBROUTINE TUVRBL1. THIS IS THE SUBROUTINE WHICH
C       STARTS AN UPDATE TOLERANCE VARIABLE SESSION.
C       THE CMD LEVEL IS DISABLED AND FLAG F29 IS SET TO 2.
C
          IF(SST.EQ.1.OR.SN.EQ.1) THEN
              WRITE(OUTLYNE,*)
     1        '"UPDATE TVAR" TAKES NO EXPLICIT INPUT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          F1=0
          F51=2
C
C       ANY OTHER ACTION TO BE TAKEN DURING RAYSET UPDATE IS HANDLED
C       FROM WITHIN SUBROUTINE TVARBLL.FOR.
C
          RETURN
      END


C     SUB UTOPER.FOR
      SUBROUTINE UTOPER
C
          IMPLICIT NONE
C
          INCLUDE 'datsub.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
C       THIS IS SUBROUTINE UTOPER. THIS IS THE SUBROUTINE WHICH
C       STARTS AN UPDATE TOPER SESSION.
C       THE CMD LEVEL IS DISABLED AND FLAG F53 IS SET TO 2.
C
          IF(SST.EQ.1.OR.SN.EQ.1) THEN
              WRITE(OUTLYNE,*)
     1        '"UPDATE TOPER" TAKES NO EXPLICIT INPUT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          F1=0
          F53=2
          CORMOD=1
          RETURN
      END


C     SUB UFOCRIT.FOR
      SUBROUTINE UFOCRIT
C
          IMPLICIT NONE
C
          INCLUDE 'datsub.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
C       THIS IS SUBROUTINE UFOCRIT. THIS IS THE SUBROUTINE WHICH
C       STARTS AN UPDATE FOCRIT SESSION.
C       THE CMD LEVEL IS DISABLED AND FLAG F54 IS SET TO 2.
C
          IF(SST.EQ.1.OR.SN.EQ.1) THEN
              WRITE(OUTLYNE,*)
     1        '"UPDATE FOCRIT" TAKES NO EXPLICIT INPUT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          F1=0
          F54=2
          CORMOD=1
          RETURN
      END
      
      
C SUB UMERIT.FOR
      SUBROUTINE UMERIT
C
          IMPLICIT NONE
C
          INCLUDE 'datsub.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
C       THIS IS SUBROUTINE UMERIT. THIS IS THE SUBROUTINE WHICH
C       STARTS AN UPDATE MERIT SESSION.
C       THE CMD LEVEL IS DISABLED AND FLAG F27 IS SET TO 2.
C
          IF(SST.EQ.1.OR.SN.EQ.1) THEN
              WRITE(OUTLYNE,*)
     1        '"UPDATE MERIT" TAKES NO EXPLICIT INPUT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          F1=0
          F27=2
          CORMOD=1
          MODEFLAG=.FALSE.
C
          RETURN
      END



C SUB ROBB.FOR
      SUBROUTINE ROBB
C
          IMPLICIT NONE
C
          INTEGER IROBB,RMAX,I,VTYPE,ALTYPE,VADD,VCFG
C
          CHARACTER AV1*23
C
          INTEGER VCN,ALLOERR
C
          REAL*8 ROBBB,ROBDD,OLDX,VSAVE,OROBFMT
     1    ,V1,FMT1,DELOLDX
C
          INTEGER ISURF
C
          REAL*8 NEWDEFVAL
C
          COMMON/DEFVALCOM/NEWDEFVAL
C
          DIMENSION OLDX(:),DELOLDX(:),VSAVE(:)
C
          ALLOCATABLE :: OLDX,DELOLDX,VSAVE
C
          COMMON/CAUX1/V1,AV1
C
          LOGICAL ERR1,ERR2
C
          INCLUDE 'datsub.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          INCLUDE 'datcfg.inc'
          VCN=VBCNT
          DEALLOCATE(OLDX,DELOLDX,VSAVE,STAT=ALLOERR)
          ALLOCATE(OLDX(VCN),DELOLDX(VCN),VSAVE(VCN),STAT=ALLOERR)
C
C       THIS IS SUBROUTINE ROBB.FOR THIS IS THE SUBROUTINE WHICH
C       HANDLES CMD LEVEL COMMANDS "ROBB"
C
C       "ROBB" EITHER TAKES OPTINAL NW1 AND NW2
          IF(STI.EQ.1) THEN
              WRITE(OUTLYNE,*)
     1        '"ROBB" PERFORMS CONVERGENCE ACCELERATION'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              DEALLOCATE(OLDX,DELOLDX,VSAVE,STAT=ALLOERR)
              RETURN
          END IF
          IF(SQ.EQ.1.OR.SST.EQ.1) THEN
              WRITE(OUTLYNE,*)
     1        '"ROBB" TAKES NO QUALIFIER OR STRING INPUT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              DEALLOCATE(OLDX,DELOLDX,VSAVE,STAT=ALLOERR)
              RETURN
          END IF
          IF(S4.EQ.1.OR.S5.EQ.1) THEN
              WRITE(OUTLYNE,*)
     1        '"ROBB" TAKES NO NUMERIC WORD #4 OR #5 INPUT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              DEALLOCATE(OLDX,DELOLDX,VSAVE,STAT=ALLOERR)
              RETURN
          END IF
          IF(DF3.EQ.0.AND.W3.LT.5.0D0) THEN
              WRITE(OUTLYNE,*)
     1        '"ROBB" REQUIRES "rmax" TO BE GREATER THAN OR EQUAL TO 5'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              DEALLOCATE(OLDX,DELOLDX,VSAVE,STAT=ALLOERR)
              RETURN
          END IF
          IF(DF3.EQ.1) W3=100.0D0
          RMAX=INT(W3)
C
          IF(KILOPT) THEN
              OUTLYNE='SOME OPERANDS ARE NOT CALCULABLE'
              CALL SHOWIT(1)
              OUTLYNE='NO ACCELERATION IS POSSIBLE'
              CALL SHOWIT(1)
              DEALLOCATE(OLDX,DELOLDX,VSAVE,STAT=ALLOERR)
              RETURN
          END IF
C
          IF(DF1.EQ.1) W1=1.0D0
          IF(DF2.EQ.1) W2=1.1D0
          ROBBB=W1
          ROBDD=W2
C
C       PROCEED WITH ACTION FOR COMMAND
          IF(.NOT.SOLEXT) THEN
              WRITE(OUTLYNE,*)'NO SOLUTION VECTOR EXISTS'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'NO ACTION TAKEN'
              CALL SHOWIT(1)
              CALL MACFAL
              DEALLOCATE(OLDX,DELOLDX,VSAVE,STAT=ALLOERR)
              RETURN
          END IF
C       PROCEED WITH ACTION FOR COMMAND
          IF(VBCNT.EQ.0) THEN
              WRITE(OUTLYNE,*)'THE VARIABLE SUBFILE IS EMPTY'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'NO VARIABLE DATA VALUES EXISTS TO RESTORE'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'NO ACTION TAKEN'
              CALL SHOWIT(1)
              CALL MACFAL
              DEALLOCATE(OLDX,DELOLDX,VSAVE,STAT=ALLOERR)
              RETURN
          END IF
          IF(OPCNT.EQ.0) THEN
              WRITE(OUTLYNE,*)'THE MERIT SUBFILE IS EMPTY'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'NO OPERAND DATA VALUES EXISTS'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'NO ACTION TAKEN'
              CALL SHOWIT(1)
              CALL MACFAL
              DEALLOCATE(OLDX,DELOLDX,VSAVE,STAT=ALLOERR)
              RETURN
          END IF

C     THERE IS A SOLUTION TO ACCELERATE, DO IT
C
C     SAVE THE CURRENT FMT
          OROBFMT=FMTFMT
C
C     CALCULATE AND STORE THE LAST OLD SOLUTION VECTOR
          DO I=1,VBCNT
              OLDX(I)=VARABL(I,4)-VARABL(I,5)
          END DO
C     NOW START SCALING AND APPLYING THE CHANGE TO THE SOLUTION VECTOR
C     TO THE CURRENT LENS WHICH ALREADY HAS THE EXISTING SOLUTION
C     VECTOR IN IT.
          IROBB=0

 30       FORMAT('  I  SCALE FACTOR   CURRENT FMT')
          WRITE(OUTLYNE,30)
          CALL SHOWIT(0)
 1        CONTINUE
          IROBB=IROBB+1
          IF(IROBB.GT.RMAX) THEN
              WRITE(OUTLYNE,98) RMAX
              CALL SHOWIT(1)
98            FORMAT('MAXIMUM NUMBER OF "ROBB" CYCLES, ',I5,
     1        ' ,HAS BEEN REACHED')
              RETURN
          END IF
          ROBBB=ROBBB*ROBDD
          DO I=1,VBCNT
              DELOLDX(I)=(ROBBB*OLDX(I))-OLDX(I)
          END DO
          DO I=1,VBCNT
C     REMEMBER THE CURRENT VARIABLE VALUE BEFORE SCALING
              VSAVE(I)=VARABL(I,4)
C     CALCULATE THE NEW VARIABLE VALUE AFTER SCALING
              VARABL(I,4)=VSAVE(I)+DELOLDX(I)
C
C     BOUNDS CHECKER
              IF(VARABL(I,4).LT.VARABL(I,9)) THEN
                  VARABL(I,4)=VARABL(I,9)
                  WRITE(OUTLYNE,*)
     1            'WARNING: '
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)
     1            'VARIABLE # ',I,' HAS REACHED ITS LOWER BOUNDARY VALUE'
                  CALL SHOWIT(1)
              END IF
              IF(VARABL(I,4).GT.VARABL(I,10)) THEN
                  VARABL(I,4)=VARABL(I,10)
                  WRITE(OUTLYNE,*)
     1            'WARNING: '
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)
     1            'VARIABLE # ',I,' HAS REACHED ITS UPPER BOUNDARY VALUE'
                  CALL SHOWIT(1)
              END IF
              VARABL(I,6)=VARABL(I,4)-VARABL(I,5)
C
          END DO
C
C     CHANGE THE LENS
C**********************************************************************
C     NOW APPLY THE CHANGE VECTOR TO THE LENS
          DO I=1,VBCNT
C     FIRST, IF THE VARIABLE IS A CONFIG 1 VARIABLE
C     NEXT IF THE VARIABLE IS IN A CONFIG OTHER THAN 1
              IF(VARABL(I,2).EQ.1.0D0) THEN
C     NON-CONFIGS VARIABLE MEANING CONFIG 1
C     THIS IS A LENS LEVEL VARIABLE CHANGE
C     GET THE DATA TYPE NUMBER OF THE VARIABLE
                  VTYPE=INT(VARABL(I,1))
C                          CURVATURE
                  IF(VTYPE.EQ.2.OR.VTYPE.EQ.1) THEN
C     SURFACE CURVATURE
C     NEW VALUE IS:
                      V1=VARABL(I,4)
                      ALENS(1,INT(VARABL(I,3)))=V1
C                       CURVATURE DONE
                  END IF
                  IF(VTYPE.EQ.10.OR.VTYPE.EQ.9) THEN
C     SURFACE TORIC CURVATURE
C     NEW VALUE IS:
                      V1=VARABL(I,4)
                      ALENS(24,INT(VARABL(I,3)))=V1
C                       TORIC CURVATURE DONE
                  END IF
                  IF(VTYPE.GE.3.AND.VTYPE.LE.8) THEN
                      IF(VTYPE.EQ.3) ALTYPE=3
                      IF(VTYPE.EQ.4) ALTYPE=2
                      IF(VTYPE.EQ.5) ALTYPE=4
                      IF(VTYPE.EQ.6) ALTYPE=5
                      IF(VTYPE.EQ.7) ALTYPE=6
                      IF(VTYPE.EQ.8) ALTYPE=7
C     NEW VALUE IS:
                      V1=VARABL(I,4)
                      ALENS(ALTYPE,INT(VARABL(I,3)))=V1
C                       THESE VARIABLES DONE
                  END IF
                  IF(VTYPE.GE.11.AND.VTYPE.LE.25.OR.VTYPE.EQ.75.OR.VTYPE.GE.
     1            124.AND.VTYPE.LE.149) THEN
                      IF(VTYPE.EQ.11) ALTYPE=41
                      IF(VTYPE.EQ.12) ALTYPE=37
                      IF(VTYPE.EQ.13) ALTYPE=38
                      IF(VTYPE.EQ.14) ALTYPE=39
                      IF(VTYPE.EQ.15) ALTYPE=40
                      IF(VTYPE.EQ.16) ALTYPE=118
                      IF(VTYPE.EQ.17) ALTYPE=119
                      IF(VTYPE.EQ.18) ALTYPE=120
                      IF(VTYPE.EQ.19) ALTYPE=114
                      IF(VTYPE.EQ.20) ALTYPE=115
                      IF(VTYPE.EQ.21) ALTYPE=46
                      IF(VTYPE.EQ.22) ALTYPE=47
                      IF(VTYPE.EQ.23) ALTYPE=48
                      IF(VTYPE.EQ.24) ALTYPE=49
                      IF(VTYPE.EQ.25) ALTYPE=50
                      IF(VTYPE.EQ.75) ALTYPE=43
                      IF(VTYPE.EQ.124) ALTYPE=71
                      IF(VTYPE.EQ.125) ALTYPE=72
                      IF(VTYPE.EQ.126) ALTYPE=73
                      IF(VTYPE.EQ.127) ALTYPE=74
                      IF(VTYPE.EQ.128) ALTYPE=75
                      IF(VTYPE.EQ.129) ALTYPE=81
                      IF(VTYPE.EQ.130) ALTYPE=82
                      IF(VTYPE.EQ.131) ALTYPE=83
                      IF(VTYPE.EQ.132) ALTYPE=84
                      IF(VTYPE.EQ.133) ALTYPE=85
                      IF(VTYPE.EQ.134) ALTYPE=116
                      IF(VTYPE.EQ.135) ALTYPE=86
                      IF(VTYPE.EQ.136) ALTYPE=87
                      IF(VTYPE.EQ.137) ALTYPE=78
                      IF(VTYPE.EQ.138) ALTYPE=79
                      IF(VTYPE.EQ.139) ALTYPE=80
                      IF(VTYPE.EQ.140) ALTYPE=89
                      IF(VTYPE.EQ.141) ALTYPE=11
                      IF(VTYPE.EQ.142) ALTYPE=10
                      IF(VTYPE.EQ.143) ALTYPE=90
                      IF(VTYPE.EQ.144) ALTYPE=91
                      IF(VTYPE.EQ.145) ALTYPE=92
                      IF(VTYPE.EQ.146) ALTYPE=93
                      IF(VTYPE.EQ.147) ALTYPE=94
                      IF(VTYPE.EQ.148) ALTYPE=95
                      IF(VTYPE.EQ.149) ALTYPE=98
C     NEW VALUE IS:
                      V1=VARABL(I,4)
                      ALENS(ALTYPE,INT(VARABL(I,3)))=V1
                  END IF
                  IF(VTYPE.EQ.150) THEN
C     NEW VALUE IS:
                      V1=VARABL(I,4)
                      GPREG(INT(VARABL(I,3)))=V1
                  END IF
                  IF(VTYPE.GE.250.AND.VTYPE.LE.4218) THEN
                      ALTYPE=VTYPE-249
C     NEW VALUE IS:
                      V1=VARABL(I,4)
C     RESET THE APPRORIATE ARRAY VALUE IN THE DEFORMABLE SURFACE
                      ISURF=INT(VARABL(I,3))
                      DEFGR1=ALENS(103,ISURF)
                      DEFGR2=ALENS(104,ISURF)
                      DEFGR3=ALENS(105,ISURF)
                      DEFGR4=ALENS(106,ISURF)
                      DEFGR5=ALENS(107,ISURF)
                      DEFGR6=0.0D0
                      DEFGR7=ALENS(109,ISURF)
                      DEFGR8=0.0D0
                      ACTNUM=ALTYPE
                      NEWDEFVAL=V1
                      ERR1=.FALSE.
                      ERR2=.FALSE.
                      CALL DEFGRIDS(6,ISURF,ERR1,ERR2)
                  END IF
                  IF(VTYPE.GE.27.AND.VTYPE.LE.74) THEN
C     SPECIAL SURFACE COEFFICIENTS
C     NEW VALUE IS:
                      V1=VARABL(I,4)
                      FTFL01((VTYPE-26),INT(VARABL(I,3)))=V1
C                       SPECIAL SURFACE COEFICIENTS DONE
                  END IF
                  IF(VTYPE.GE.76.AND.VTYPE.LE.123) THEN
C     SPECIAL SURFACE COEFFICIENTS
C     NEW VALUE IS:
                      V1=VARABL(I,4)
                      FTFL01((VTYPE-27),INT(VARABL(I,3)))=V1
C                       SPECIAL SURFACE COEFICIENTS DONE
                  END IF
C
              ELSE
C     CONFIGS VARIABLE
C     VCFG IS THE CONFIG NUMBER
                  VCFG=INT(VARABL(I,2))
C     VTYPE IS THE VARIABLE TYPE NUMBER AS USED IN THE VARIABLES ARRAYS
                  VTYPE=INT(VARABL(I,1))
C
C     FOR VARIABLE I, APPLY THE SPECIFIED CHANGE TO THE SPECIFIED
C     CONFIG
C
C     THE NEW VARAIBLE VALUE IS JUST
                  V1=VARABL(I,4)
C     CONVERT THIS VALUE TO A CHARACTER*23 CHARACTER VARIABLE
                  VADD=INT(VARABL(I,14))
                  IF(CFADD(VADD,1).EQ.1.AND.V1.NE.0.0D0) V1=1.0D0/V1
                  IF(CFADD(VADD,1).EQ.9.AND.V1.NE.0.0D0) V1=1.0D0/V1
                  CALL AUXNTA
C
C     THE POSITION IN THE CFADD,CFVAL AND CFCHAR ARRAYS WHERE THIS
C     VARIABLE IS FOUND IS:
C
                  IF(CFADD(VADD,1).GE.27.AND.CFADD(VADD,1).LE.74.OR.
     1            CFADD(VADD,1).GE.76.AND.CFADD(VADD,1).LE.123
     1            .OR.CFADD(VADD,1).EQ.141) THEN
                      CFVAL(VADD,2)=V1
                      CFCHAR(VADD,2)=AV1
C     NOW UPDATE THE CONFIG ARRAY
                      CONFG(CFADD(VADD,3),CFADD(VADD,9))(CFADD(VADD,6):CFADD(VADD,7))
     1                =AV1(1:23)
                  ELSE
                      CFVAL(VADD,1)=V1
                      CFCHAR(VADD,1)=AV1
C     NOW UPDATE THE CONFIG ARRAY
                      CONFG(CFADD(VADD,3),CFADD(VADD,9))(CFADD(VADD,4):CFADD(VADD,5))
     1                =AV1(1:23)
                  END IF
C     NOW LOOK UP WHERE THIS CHARACTER REPRESENTATION OF THE NEW VALUE
C     SHOULD BE STUFFED INTO THE CONFIG ARRAYS CONFG AND
C     STUFF IT THERE.
C
C     NOW LOOP BACK AND REPEAT FOR THE NEXT VARIABLE.
C     FINISHED WITH A CONFIG VARIABLE
              END IF
C     LOOP TO NEXT VARIABL
          END DO
C     UPDATE THE LENS OR THE PERMANENT LENS WILL BE ******!
          CALL FIXDEFORMFILE
          F6=1
          F1=0
          F22=0
          LNSTYP=2
          CALL LNSEOS
C**********************************************************************
C     EVALUATE OPREANDS AND FMT
          OPCALC_TYPE=3
          CALL OPCALC
          IF(F28.EQ.0) DEALLOCATE(OLDX,DELOLDX,VSAVE,STAT=ALLOERR)
          IF(F28.EQ.0) RETURN
          CALL OPLOAD
          IF(F28.EQ.0) DEALLOCATE(OLDX,DELOLDX,VSAVE,STAT=ALLOERR)
          IF(F28.EQ.0) RETURN
          FMT1=0.0D0
          DO I=1,OPCNT
              IF(OPERND(I,19).EQ.0.0D0) FMT1=FMT1+(OPERND(I,14)**2)
          END DO
C     NOW IS THE NEW FMT LESS THAN THE OLD FMT
          IF(FMT1.LE.OROBFMT) THEN
C     WE IMPROVED THE LENS, LETS TRY IT AGAIN
 10           FORMAT(I3,2X,G13.6,2X,G13.6)
              WRITE(OUTLYNE,10) IROBB,ROBBB,FMT1
              CALL SHOWIT(0)
              OROBFMT=FMT1
              GO TO 1
          ELSE
              WRITE(OUTLYNE,10) IROBB,ROBBB,FMT1
              CALL SHOWIT(0)
              WRITE(OUTLYNE,20)
              CALL SHOWIT(0)
 20           FORMAT('RESTORING TO THE LAST SOLUTION VECTOR')
C     RESTORE THE LENS TO THE LAST SET OF VARIABLES
          END IF
          DO I=1,VBCNT
C     GO BACK TO THE LAST VARIABLE VALUES
              VARABL(I,4)=VSAVE(I)
C
C     BOUNDS CHECKER
              IF(VARABL(I,4).LT.VARABL(I,9)) THEN
                  VARABL(I,4)=VARABL(I,9)
                  WRITE(OUTLYNE,*)
     1            'WARNING: '
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)
     1            'VARIABLE # ',I,' HAS REACHED ITS LOWER BOUNDARY VALUE'
                  CALL SHOWIT(1)
              END IF
              IF(VARABL(I,4).GT.VARABL(I,10)) THEN
                  VARABL(I,4)=VARABL(I,10)
                  WRITE(OUTLYNE,*)
     1            'WARNING: '
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)
     1            'VARIABLE # ',I,' HAS REACHED ITS UPPER BOUNDARY VALUE'
                  CALL SHOWIT(1)
              END IF
              VARABL(I,6)=VARABL(I,4)-VARABL(I,5)
C
          END DO
C
C     CHANGE THE LENS (RESTORE IT)
C**********************************************************************
          DO I=1,VBCNT
C     FIRST, IF THE VARIABLE IS A CONFIG 1 VARIABLE
C     NEXT IF THE VARIABLE IS IN A CONFIG OTHER THAN 1
              IF(VARABL(I,2).EQ.1.0D0) THEN
C     NON-CONFIGS VARIABLE MEANING CONFIG 1
C     THIS IS A LENS LEVEL VARIABLE CHANGE
C     GET THE DATA TYPE NUMBER OF THE VARIABLE
                  VTYPE=INT(VARABL(I,1))
C                          CURVATURE
                  IF(VTYPE.EQ.2.OR.VTYPE.EQ.1) THEN
C     SURFACE CURVATURE
C     NEW VALUE IS:
                      V1=VARABL(I,4)
                      ALENS(1,INT(VARABL(I,3)))=V1
C                       CURVATURE DONE
                  END IF
                  IF(VTYPE.EQ.10.OR.VTYPE.EQ.9) THEN
C     SURFACE TORIC CURVATURE
C     NEW VALUE IS:
                      V1=VARABL(I,4)
                      ALENS(24,INT(VARABL(I,3)))=V1
C                       TORIC CURVATURE DONE
                  END IF
                  IF(VTYPE.GE.3.AND.VTYPE.LE.8) THEN
                      IF(VTYPE.EQ.3) ALTYPE=3
                      IF(VTYPE.EQ.4) ALTYPE=2
                      IF(VTYPE.EQ.5) ALTYPE=4
                      IF(VTYPE.EQ.6) ALTYPE=5
                      IF(VTYPE.EQ.7) ALTYPE=6
                      IF(VTYPE.EQ.8) ALTYPE=7
C     NEW VALUE IS:
                      V1=VARABL(I,4)
                      ALENS(ALTYPE,INT(VARABL(I,3)))=V1
C                       THESE VARIABLES DONE
                  END IF
                  IF(VTYPE.GE.11.AND.VTYPE.LE.25.OR.VTYPE.EQ.75.OR.VTYPE.GE.
     1            124.AND.VTYPE.LE.149) THEN
                      IF(VTYPE.EQ.11) ALTYPE=41
                      IF(VTYPE.EQ.12) ALTYPE=37
                      IF(VTYPE.EQ.13) ALTYPE=38
                      IF(VTYPE.EQ.14) ALTYPE=39
                      IF(VTYPE.EQ.15) ALTYPE=40
                      IF(VTYPE.EQ.16) ALTYPE=118
                      IF(VTYPE.EQ.17) ALTYPE=119
                      IF(VTYPE.EQ.18) ALTYPE=120
                      IF(VTYPE.EQ.19) ALTYPE=114
                      IF(VTYPE.EQ.20) ALTYPE=115
                      IF(VTYPE.EQ.21) ALTYPE=46
                      IF(VTYPE.EQ.22) ALTYPE=47
                      IF(VTYPE.EQ.23) ALTYPE=48
                      IF(VTYPE.EQ.24) ALTYPE=49
                      IF(VTYPE.EQ.25) ALTYPE=50
                      IF(VTYPE.EQ.75) ALTYPE=43
                      IF(VTYPE.EQ.124) ALTYPE=71
                      IF(VTYPE.EQ.125) ALTYPE=72
                      IF(VTYPE.EQ.126) ALTYPE=73
                      IF(VTYPE.EQ.127) ALTYPE=74
                      IF(VTYPE.EQ.128) ALTYPE=75
                      IF(VTYPE.EQ.129) ALTYPE=81
                      IF(VTYPE.EQ.130) ALTYPE=82
                      IF(VTYPE.EQ.131) ALTYPE=83
                      IF(VTYPE.EQ.132) ALTYPE=84
                      IF(VTYPE.EQ.133) ALTYPE=85
                      IF(VTYPE.EQ.134) ALTYPE=116
                      IF(VTYPE.EQ.135) ALTYPE=86
                      IF(VTYPE.EQ.136) ALTYPE=87
                      IF(VTYPE.EQ.137) ALTYPE=78
                      IF(VTYPE.EQ.138) ALTYPE=79
                      IF(VTYPE.EQ.139) ALTYPE=80
                      IF(VTYPE.EQ.140) ALTYPE=89
                      IF(VTYPE.EQ.141) ALTYPE=11
                      IF(VTYPE.EQ.142) ALTYPE=10
                      IF(VTYPE.EQ.143) ALTYPE=90
                      IF(VTYPE.EQ.144) ALTYPE=91
                      IF(VTYPE.EQ.145) ALTYPE=92
                      IF(VTYPE.EQ.146) ALTYPE=93
                      IF(VTYPE.EQ.147) ALTYPE=94
                      IF(VTYPE.EQ.148) ALTYPE=95
                      IF(VTYPE.EQ.149) ALTYPE=98
C     NEW VALUE IS:
                      V1=VARABL(I,4)
                      ALENS(ALTYPE,INT(VARABL(I,3)))=V1
                  END IF
                  IF(VTYPE.EQ.150) THEN
C     NEW VALUE IS:
                      V1=VARABL(I,4)
                      GPREG(INT(VARABL(I,3)))=V1
                  END IF
                  IF(VTYPE.GE.250.AND.VTYPE.LE.4218) THEN
                      ALTYPE=VTYPE-249
C     NEW VALUE IS:
                      V1=VARABL(I,4)
C     RESET THE APPRORIATE ARRAY VALUE IN THE DEFORMABLE SURFACE
                      ISURF=INT(VARABL(I,3))
                      DEFGR1=ALENS(103,ISURF)
                      DEFGR2=ALENS(104,ISURF)
                      DEFGR3=ALENS(105,ISURF)
                      DEFGR4=ALENS(106,ISURF)
                      DEFGR5=ALENS(107,ISURF)
                      DEFGR6=0.0D0
                      DEFGR7=ALENS(109,ISURF)
                      DEFGR8=0.0D0
                      ACTNUM=ALTYPE
                      NEWDEFVAL=V1
                      ERR1=.FALSE.
                      ERR2=.FALSE.
                      CALL DEFGRIDS(6,ISURF,ERR1,ERR2)
                  END IF
                  IF(VTYPE.GE.27.AND.VTYPE.LE.74) THEN
C     SPECIAL SURFACE COEFFICIENTS
C     NEW VALUE IS:
                      V1=VARABL(I,4)
                      FTFL01((VTYPE-26),INT(VARABL(I,3)))=V1
C                       SPECIAL SURFACE COEFICIENTS DONE
                  END IF
                  IF(VTYPE.GE.76.AND.VTYPE.LE.123) THEN
C     SPECIAL SURFACE COEFFICIENTS
C     NEW VALUE IS:
                      V1=VARABL(I,4)
                      FTFL01((VTYPE-27),INT(VARABL(I,3)))=V1
C                       SPECIAL SURFACE COEFICIENTS DONE
                  END IF
C
              ELSE
C     CONFIGS VARIABLE
C     VCFG IS THE CONFIG NUMBER
                  VCFG=INT(VARABL(I,2))
C     VTYPE IS THE VARIABLE TYPE NUMBER AS USED IN THE VARIABLES ARRAYS
                  VTYPE=INT(VARABL(I,1))
C
C     FOR VARIABLE I, APPLY THE SPECIFIED CHANGE TO THE SPECIFIED
C     CONFIG
C
C     THE NEW VARAIBLE VALUE IS JUST
                  V1=VARABL(I,4)
C     CONVERT THIS VALUE TO A CHARACTER*23 CHARACTER VARIABLE
                  VADD=INT(VARABL(I,14))
                  IF(CFADD(VADD,1).EQ.1.AND.V1.NE.0.0D0) V1=1.0D0/V1
                  IF(CFADD(VADD,1).EQ.9.AND.V1.NE.0.0D0) V1=1.0D0/V1
                  CALL AUXNTA
C
C     THE POSITION IN THE CFADD,CFVAL AND CFCHAR ARRAYS WHERE THIS
C     VARIABLE IS FOUND IS:
C
                  IF(CFADD(VADD,1).GE.27.AND.CFADD(VADD,1).LE.74.OR.
     1            CFADD(VADD,1).GE.76.AND.CFADD(VADD,1).LE.123
     1            .OR.CFADD(VADD,1).EQ.141) THEN
                      CFVAL(VADD,2)=V1
                      CFCHAR(VADD,2)=AV1
C     NOW UPDATE THE CONFIG ARRAY
                      CONFG(CFADD(VADD,3),CFADD(VADD,9))(CFADD(VADD,6):CFADD(VADD,7))
     1                =AV1(1:23)
                  ELSE
                      CFVAL(VADD,1)=V1
                      CFCHAR(VADD,1)=AV1
C     NOW UPDATE THE CONFIG ARRAY
                      CONFG(CFADD(VADD,3),CFADD(VADD,9))(CFADD(VADD,4):CFADD(VADD,5))
     1                =AV1(1:23)
                  END IF
C     NOW LOOK UP WHERE THIS CHARACTER REPRESENTATION OF THE NEW VALUE
C     SHOULD BE STUFFED INTO THE CONFIG ARRAYS CONFG AND
C     STUFF IT THERE.
C
C     NOW LOOP BACK AND REPEAT FOR THE NEXT VARIABLE.
C     FINISHED WITH A CONFIG VARIABLE
              END IF
C     LOOP TO NEXT VARIABL
          END DO
C     UPDATE THE LENS OR THE PERMANENT LENS WILL BE ******!
          CALL FIXDEFORMFILE
          F6=1
          F1=0
          F22=0
          LNSTYP=2
          CALL LNSEOS
C
C     RE-EVALUATE THE OPERANDS AND DISPLAY
C     THE NEW FIGURE OF MERIT, THE OLD FIGURE OF MERIT  AND ITS CHANGE
          SAVE_KDP(4)=SAVEINPT(4)
          F28=1
          MSG=.FALSE.
          OPTMES=.FALSE.
          WC='FMT'
          WQ='        '
          SQ=0
          SN=0
          SST=0
          WS=' '
          DF1=1
          S1=0
          S2=0
          S3=0
          S4=0
          S5=0
          DF2=1
          DF3=1
          DF4=1
          DF5=1
          STI=0
          CALL FMT2
          REST_KDP(4)=RESTINPT(4)
          DEALLOCATE(OLDX,DELOLDX,VSAVE,STAT=ALLOERR)
          RETURN
C       ALL DONE
      END
C SUB RESTMIN.FOR
      SUBROUTINE RESTMIN
C
          IMPLICIT NONE
C
          INTEGER IROBB,RMAX,I,VTYPE,ALTYPE,VADD,VCFG
C
          CHARACTER AV1*23
C
          INTEGER VCN,ALLOERR
C
          REAL*8 ROBBB,ROBDD,OLDX,VSAVE,OROBFMT
     1    ,V1,FMT1,DELOLDX
C
          INTEGER ISURF
C
          REAL*8 NEWDEFVAL
C
          COMMON/DEFVALCOM/NEWDEFVAL
C
          DIMENSION OLDX(:),DELOLDX(:),VSAVE(:)
C
          ALLOCATABLE :: OLDX,DELOLDX,VSAVE
C
          COMMON/CAUX1/V1,AV1
C
          LOGICAL ERR1,ERR2
C
          INCLUDE 'datsub.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          INCLUDE 'datcfg.inc'
          VCN=VBCNT
          DEALLOCATE(OLDX,DELOLDX,VSAVE,STAT=ALLOERR)
          ALLOCATE(OLDX(VCN),DELOLDX(VCN),VSAVE(VCN),STAT=ALLOERR)
C
C
          IF(STI.EQ.1) THEN
              WRITE(OUTLYNE,*)
     1        '"RESTORE MIN LOOKS ALONG THE CURRENT SOLUTION VECTOR'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)
     1        'FOR A MINIMUM SOLUTION'
              CALL SHOWIT(1)
              DEALLOCATE(OLDX,DELOLDX,VSAVE,STAT=ALLOERR)
              RETURN
          END IF
          IF(SN.EQ.1.OR.SST.EQ.1) THEN
              WRITE(OUTLYNE,*)
     1        '"RESTORE MIN TAKES NO NUMERIC OR STRING INPUT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              DEALLOCATE(OLDX,DELOLDX,VSAVE,STAT=ALLOERR)
              RETURN
          END IF
          RMAX=500
C
          IF(KILOPT) THEN
              OUTLYNE='SOME OPERANDS ARE NOT CALCULABLE'
              CALL SHOWIT(1)
              OUTLYNE='NO ACTION IS POSSIBLE'
              CALL SHOWIT(1)
              DEALLOCATE(OLDX,DELOLDX,VSAVE,STAT=ALLOERR)
              RETURN
          END IF
C
          ROBBB=1.0D0
          ROBDD=1.1D0
C
C       PROCEED WITH ACTION FOR COMMAND
          IF(.NOT.SOLEXT) THEN
              WRITE(OUTLYNE,*)'NO SOLUTION VECTOR EXISTS'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'NO ACTION TAKEN'
              CALL SHOWIT(1)
              CALL MACFAL
              DEALLOCATE(OLDX,DELOLDX,VSAVE,STAT=ALLOERR)
              RETURN
          END IF
C       PROCEED WITH ACTION FOR COMMAND
          IF(VBCNT.EQ.0) THEN
              WRITE(OUTLYNE,*)'THE VARIABLE SUBFILE IS EMPTY'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'NO VARIABLE DATA VALUES EXISTS TO RESTORE'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'NO ACTION TAKEN'
              CALL SHOWIT(1)
              CALL MACFAL
              DEALLOCATE(OLDX,DELOLDX,VSAVE,STAT=ALLOERR)
              RETURN
          END IF
          IF(OPCNT.EQ.0) THEN
              WRITE(OUTLYNE,*)'THE MERIT SUBFILE IS EMPTY'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'NO OPERAND DATA VALUES EXISTS'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'NO ACTION TAKEN'
              CALL SHOWIT(1)
              CALL MACFAL
              DEALLOCATE(OLDX,DELOLDX,VSAVE,STAT=ALLOERR)
              RETURN
          END IF

C     THERE IS A SOLUTION TO ACCELERATE, DO IT
C
C     SAVE THE CURRENT FMT
          OROBFMT=FMTFMT
C
C     CALCULATE AND STORE THE LAST OLD SOLUTION VECTOR
          DO I=1,VBCNT
              OLDX(I)=VARABL(I,4)-VARABL(I,5)
          END DO
C     NOW START SCALING AND APPLYING THE CHANGE TO THE SOLUTION VECTOR
C     TO THE CURRENT LENS WHICH ALREADY HAS THE EXISTING SOLUTION
C     VECTOR IN IT.
          IROBB=0

 1        CONTINUE
          IROBB=IROBB+1
          IF(IROBB.GT.RMAX) THEN
              WRITE(OUTLYNE,98) RMAX
              CALL SHOWIT(1)
98            FORMAT('MAXIMUM NUMBER CYCLES, ',I5,
     1        ' ,HAS BEEN REACHED')
              WRITE(OUTLYNE,99)
              CALL SHOWIT(1)
99            FORMAT('ISSUE ANOTHER "RESTORE MIN"')
              RETURN
          END IF
          ROBBB=ROBBB*ROBDD
          DO I=1,VBCNT
              DELOLDX(I)=(ROBBB*OLDX(I))-OLDX(I)
          END DO
          DO I=1,VBCNT
C     REMEMBER THE CURRENT VARIABLE VALUE BEFORE SCALING
              VSAVE(I)=VARABL(I,4)
C     CALCULATE THE NEW VARIABLE VALUE AFTER SCALING
              VARABL(I,4)=VSAVE(I)+DELOLDX(I)
C
C     BOUNDS CHECKER
              IF(VARABL(I,4).LT.VARABL(I,9)) THEN
                  VARABL(I,4)=VARABL(I,9)
                  WRITE(OUTLYNE,*)
     1            'WARNING: '
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)
     1            'VARIABLE # ',I,' HAS REACHED ITS LOWER BOUNDARY VALUE'
                  CALL SHOWIT(1)
              END IF
              IF(VARABL(I,4).GT.VARABL(I,10)) THEN
                  VARABL(I,4)=VARABL(I,10)
                  WRITE(OUTLYNE,*)
     1            'WARNING: '
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)
     1            'VARIABLE # ',I,' HAS REACHED ITS UPPER BOUNDARY VALUE'
                  CALL SHOWIT(1)
              END IF
              VARABL(I,6)=VARABL(I,4)-VARABL(I,5)
C
          END DO
C
C     CHANGE THE LENS
C**********************************************************************
C     NOW APPLY THE CHANGE VECTOR TO THE LENS
          DO I=1,VBCNT
C     FIRST, IF THE VARIABLE IS A CONFIG 1 VARIABLE
C     NEXT IF THE VARIABLE IS IN A CONFIG OTHER THAN 1
              IF(VARABL(I,2).EQ.1.0D0) THEN
C     NON-CONFIGS VARIABLE MEANING CONFIG 1
C     THIS IS A LENS LEVEL VARIABLE CHANGE
C     GET THE DATA TYPE NUMBER OF THE VARIABLE
                  VTYPE=INT(VARABL(I,1))
C                          CURVATURE
                  IF(VTYPE.EQ.2.OR.VTYPE.EQ.1) THEN
C     SURFACE CURVATURE
C     NEW VALUE IS:
                      V1=VARABL(I,4)
                      ALENS(1,INT(VARABL(I,3)))=V1
C                       CURVATURE DONE
                  END IF
                  IF(VTYPE.EQ.10.OR.VTYPE.EQ.9) THEN
C     SURFACE TORIC CURVATURE
C     NEW VALUE IS:
                      V1=VARABL(I,4)
                      ALENS(24,INT(VARABL(I,3)))=V1
C                       TORIC CURVATURE DONE
                  END IF
                  IF(VTYPE.GE.3.AND.VTYPE.LE.8) THEN
                      IF(VTYPE.EQ.3) ALTYPE=3
                      IF(VTYPE.EQ.4) ALTYPE=2
                      IF(VTYPE.EQ.5) ALTYPE=4
                      IF(VTYPE.EQ.6) ALTYPE=5
                      IF(VTYPE.EQ.7) ALTYPE=6
                      IF(VTYPE.EQ.8) ALTYPE=7
C     NEW VALUE IS:
                      V1=VARABL(I,4)
                      ALENS(ALTYPE,INT(VARABL(I,3)))=V1
C                       THESE VARIABLES DONE
                  END IF
                  IF(VTYPE.GE.11.AND.VTYPE.LE.25.OR.VTYPE.EQ.75.OR.VTYPE.GE.
     1            124.AND.VTYPE.LE.149) THEN
                      IF(VTYPE.EQ.11) ALTYPE=41
                      IF(VTYPE.EQ.12) ALTYPE=37
                      IF(VTYPE.EQ.13) ALTYPE=38
                      IF(VTYPE.EQ.14) ALTYPE=39
                      IF(VTYPE.EQ.15) ALTYPE=40
                      IF(VTYPE.EQ.16) ALTYPE=118
                      IF(VTYPE.EQ.17) ALTYPE=119
                      IF(VTYPE.EQ.18) ALTYPE=120
                      IF(VTYPE.EQ.19) ALTYPE=114
                      IF(VTYPE.EQ.20) ALTYPE=115
                      IF(VTYPE.EQ.21) ALTYPE=46
                      IF(VTYPE.EQ.22) ALTYPE=47
                      IF(VTYPE.EQ.23) ALTYPE=48
                      IF(VTYPE.EQ.24) ALTYPE=49
                      IF(VTYPE.EQ.25) ALTYPE=50
                      IF(VTYPE.EQ.75) ALTYPE=43
                      IF(VTYPE.EQ.124) ALTYPE=71
                      IF(VTYPE.EQ.125) ALTYPE=72
                      IF(VTYPE.EQ.126) ALTYPE=73
                      IF(VTYPE.EQ.127) ALTYPE=74
                      IF(VTYPE.EQ.128) ALTYPE=75
                      IF(VTYPE.EQ.129) ALTYPE=81
                      IF(VTYPE.EQ.130) ALTYPE=82
                      IF(VTYPE.EQ.131) ALTYPE=83
                      IF(VTYPE.EQ.132) ALTYPE=84
                      IF(VTYPE.EQ.133) ALTYPE=85
                      IF(VTYPE.EQ.134) ALTYPE=116
                      IF(VTYPE.EQ.135) ALTYPE=86
                      IF(VTYPE.EQ.136) ALTYPE=87
                      IF(VTYPE.EQ.137) ALTYPE=78
                      IF(VTYPE.EQ.138) ALTYPE=79
                      IF(VTYPE.EQ.139) ALTYPE=80
                      IF(VTYPE.EQ.140) ALTYPE=89
                      IF(VTYPE.EQ.141) ALTYPE=11
                      IF(VTYPE.EQ.142) ALTYPE=10
                      IF(VTYPE.EQ.143) ALTYPE=90
                      IF(VTYPE.EQ.144) ALTYPE=91
                      IF(VTYPE.EQ.145) ALTYPE=92
                      IF(VTYPE.EQ.146) ALTYPE=93
                      IF(VTYPE.EQ.147) ALTYPE=94
                      IF(VTYPE.EQ.148) ALTYPE=95
                      IF(VTYPE.EQ.149) ALTYPE=98
C     NEW VALUE IS:
                      V1=VARABL(I,4)
                      ALENS(ALTYPE,INT(VARABL(I,3)))=V1
                  END IF
                  IF(VTYPE.EQ.150) THEN
C     NEW VALUE IS:
                      V1=VARABL(I,4)
                      GPREG(INT(VARABL(I,3)))=V1
                  END IF
                  IF(VTYPE.GE.250.AND.VTYPE.LE.4218) THEN
                      ALTYPE=VTYPE-249
C     NEW VALUE IS:
                      V1=VARABL(I,4)
C     RESET THE APPRORIATE ARRAY VALUE IN THE DEFORMABLE SURFACE
                      ISURF=INT(VARABL(I,3))
                      DEFGR1=ALENS(103,ISURF)
                      DEFGR2=ALENS(104,ISURF)
                      DEFGR3=ALENS(105,ISURF)
                      DEFGR4=ALENS(106,ISURF)
                      DEFGR5=ALENS(107,ISURF)
                      DEFGR6=0.0D0
                      DEFGR7=ALENS(109,ISURF)
                      DEFGR8=0.0D0
                      ACTNUM=ALTYPE
                      NEWDEFVAL=V1
                      ERR1=.FALSE.
                      ERR2=.FALSE.
                      CALL DEFGRIDS(6,ISURF,ERR1,ERR2)
                  END IF
                  IF(VTYPE.GE.27.AND.VTYPE.LE.74) THEN
C     SPECIAL SURFACE COEFFICIENTS
C     NEW VALUE IS:
                      V1=VARABL(I,4)
                      FTFL01((VTYPE-26),INT(VARABL(I,3)))=V1
C                       SPECIAL SURFACE COEFICIENTS DONE
                  END IF
                  IF(VTYPE.GE.76.AND.VTYPE.LE.123) THEN
C     SPECIAL SURFACE COEFFICIENTS
C     NEW VALUE IS:
                      V1=VARABL(I,4)
                      FTFL01((VTYPE-27),INT(VARABL(I,3)))=V1
C                       SPECIAL SURFACE COEFICIENTS DONE
                  END IF
C
              ELSE
C     CONFIGS VARIABLE
C     VCFG IS THE CONFIG NUMBER
                  VCFG=INT(VARABL(I,2))
C     VTYPE IS THE VARIABLE TYPE NUMBER AS USED IN THE VARIABLES ARRAYS
                  VTYPE=INT(VARABL(I,1))
C
C     FOR VARIABLE I, APPLY THE SPECIFIED CHANGE TO THE SPECIFIED
C     CONFIG
C
C     THE NEW VARAIBLE VALUE IS JUST
                  V1=VARABL(I,4)
C     CONVERT THIS VALUE TO A CHARACTER*23 CHARACTER VARIABLE
                  VADD=INT(VARABL(I,14))
                  IF(CFADD(VADD,1).EQ.1.AND.V1.NE.0.0D0) V1=1.0D0/V1
                  IF(CFADD(VADD,1).EQ.9.AND.V1.NE.0.0D0) V1=1.0D0/V1
                  CALL AUXNTA
C
C     THE POSITION IN THE CFADD,CFVAL AND CFCHAR ARRAYS WHERE THIS
C     VARIABLE IS FOUND IS:
C
                  IF(CFADD(VADD,1).GE.27.AND.CFADD(VADD,1).LE.74.OR.
     1            CFADD(VADD,1).GE.76.AND.CFADD(VADD,1).LE.123
     1            .OR.CFADD(VADD,1).EQ.141) THEN
                      CFVAL(VADD,2)=V1
                      CFCHAR(VADD,2)=AV1
C     NOW UPDATE THE CONFIG ARRAY
                      CONFG(CFADD(VADD,3),CFADD(VADD,9))(CFADD(VADD,6):CFADD(VADD,7))
     1                =AV1(1:23)
                  ELSE
                      CFVAL(VADD,1)=V1
                      CFCHAR(VADD,1)=AV1
C     NOW UPDATE THE CONFIG ARRAY
                      CONFG(CFADD(VADD,3),CFADD(VADD,9))(CFADD(VADD,4):CFADD(VADD,5))
     1                =AV1(1:23)
                  END IF
C     NOW LOOK UP WHERE THIS CHARACTER REPRESENTATION OF THE NEW VALUE
C     SHOULD BE STUFFED INTO THE CONFIG ARRAYS CONFG AND
C     STUFF IT THERE.
C
C     NOW LOOP BACK AND REPEAT FOR THE NEXT VARIABLE.
C     FINISHED WITH A CONFIG VARIABLE
              END IF
C     LOOP TO NEXT VARIABL
          END DO
C     UPDATE THE LENS OR THE PERMANENT LENS WILL BE ******!
          CALL FIXDEFORMFILE
          F6=1
          F1=0
          F22=0
          LNSTYP=2
          CALL LNSEOS
C**********************************************************************
C     EVALUATE OPREANDS AND FMT
          OPCALC_TYPE=3
          CALL OPCALC
          IF(F28.EQ.0) DEALLOCATE(OLDX,DELOLDX,VSAVE,STAT=ALLOERR)
          IF(F28.EQ.0) RETURN
          CALL OPLOAD
          IF(F28.EQ.0) DEALLOCATE(OLDX,DELOLDX,VSAVE,STAT=ALLOERR)
          IF(F28.EQ.0) RETURN
          FMT1=0.0D0
          DO I=1,OPCNT
              IF(OPERND(I,19).EQ.0.0D0) FMT1=FMT1+(OPERND(I,14)**2)
          END DO
C     NOW IS THE NEW FMT LESS THAN THE OLD FMT
          IF(FMT1.LE.OROBFMT) THEN
C     WE IMPROVED THE LENS, LETS TRY IT AGAIN
              OROBFMT=FMT1
              GO TO 1
          ELSE
C     RESTORE THE LENS TO THE LAST SET OF VARIABLES
          END IF
          DO I=1,VBCNT
C     GO BACK TO THE LAST VARIABLE VALUES
              VARABL(I,4)=VSAVE(I)
C
C     BOUNDS CHECKER
              IF(VARABL(I,4).LT.VARABL(I,9)) THEN
                  VARABL(I,4)=VARABL(I,9)
                  WRITE(OUTLYNE,*)
     1            'WARNING: '
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)
     1            'VARIABLE # ',I,' HAS REACHED ITS LOWER BOUNDARY VALUE'
                  CALL SHOWIT(1)
              END IF
              IF(VARABL(I,4).GT.VARABL(I,10)) THEN
                  VARABL(I,4)=VARABL(I,10)
                  WRITE(OUTLYNE,*)
     1            'WARNING: '
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)
     1            'VARIABLE # ',I,' HAS REACHED ITS UPPER BOUNDARY VALUE'
                  CALL SHOWIT(1)
              END IF
              VARABL(I,6)=VARABL(I,4)-VARABL(I,5)
C
          END DO
C
C     CHANGE THE LENS (RESTORE IT)
C**********************************************************************
          DO I=1,VBCNT
C     FIRST, IF THE VARIABLE IS A CONFIG 1 VARIABLE
C     NEXT IF THE VARIABLE IS IN A CONFIG OTHER THAN 1
              IF(VARABL(I,2).EQ.1.0D0) THEN
C     NON-CONFIGS VARIABLE MEANING CONFIG 1
C     THIS IS A LENS LEVEL VARIABLE CHANGE
C     GET THE DATA TYPE NUMBER OF THE VARIABLE
                  VTYPE=INT(VARABL(I,1))
C                          CURVATURE
                  IF(VTYPE.EQ.2.OR.VTYPE.EQ.1) THEN
C     SURFACE CURVATURE
C     NEW VALUE IS:
                      V1=VARABL(I,4)
                      ALENS(1,INT(VARABL(I,3)))=V1
C                       CURVATURE DONE
                  END IF
                  IF(VTYPE.EQ.10.OR.VTYPE.EQ.9) THEN
C     SURFACE TORIC CURVATURE
C     NEW VALUE IS:
                      V1=VARABL(I,4)
                      ALENS(24,INT(VARABL(I,3)))=V1
C                       TORIC CURVATURE DONE
                  END IF
                  IF(VTYPE.GE.3.AND.VTYPE.LE.8) THEN
                      IF(VTYPE.EQ.3) ALTYPE=3
                      IF(VTYPE.EQ.4) ALTYPE=2
                      IF(VTYPE.EQ.5) ALTYPE=4
                      IF(VTYPE.EQ.6) ALTYPE=5
                      IF(VTYPE.EQ.7) ALTYPE=6
                      IF(VTYPE.EQ.8) ALTYPE=7
C     NEW VALUE IS:
                      V1=VARABL(I,4)
                      ALENS(ALTYPE,INT(VARABL(I,3)))=V1
C                       THESE VARIABLES DONE
                  END IF
                  IF(VTYPE.GE.11.AND.VTYPE.LE.25.OR.VTYPE.EQ.75.OR.VTYPE.GE.
     1            124.AND.VTYPE.LE.149) THEN
                      IF(VTYPE.EQ.11) ALTYPE=41
                      IF(VTYPE.EQ.12) ALTYPE=37
                      IF(VTYPE.EQ.13) ALTYPE=38
                      IF(VTYPE.EQ.14) ALTYPE=39
                      IF(VTYPE.EQ.15) ALTYPE=40
                      IF(VTYPE.EQ.16) ALTYPE=118
                      IF(VTYPE.EQ.17) ALTYPE=119
                      IF(VTYPE.EQ.18) ALTYPE=120
                      IF(VTYPE.EQ.19) ALTYPE=114
                      IF(VTYPE.EQ.20) ALTYPE=115
                      IF(VTYPE.EQ.21) ALTYPE=46
                      IF(VTYPE.EQ.22) ALTYPE=47
                      IF(VTYPE.EQ.23) ALTYPE=48
                      IF(VTYPE.EQ.24) ALTYPE=49
                      IF(VTYPE.EQ.25) ALTYPE=50
                      IF(VTYPE.EQ.75) ALTYPE=43
                      IF(VTYPE.EQ.124) ALTYPE=71
                      IF(VTYPE.EQ.125) ALTYPE=72
                      IF(VTYPE.EQ.126) ALTYPE=73
                      IF(VTYPE.EQ.127) ALTYPE=74
                      IF(VTYPE.EQ.128) ALTYPE=75
                      IF(VTYPE.EQ.129) ALTYPE=81
                      IF(VTYPE.EQ.130) ALTYPE=82
                      IF(VTYPE.EQ.131) ALTYPE=83
                      IF(VTYPE.EQ.132) ALTYPE=84
                      IF(VTYPE.EQ.133) ALTYPE=85
                      IF(VTYPE.EQ.134) ALTYPE=116
                      IF(VTYPE.EQ.135) ALTYPE=86
                      IF(VTYPE.EQ.136) ALTYPE=87
                      IF(VTYPE.EQ.137) ALTYPE=78
                      IF(VTYPE.EQ.138) ALTYPE=79
                      IF(VTYPE.EQ.139) ALTYPE=80
                      IF(VTYPE.EQ.140) ALTYPE=89
                      IF(VTYPE.EQ.141) ALTYPE=11
                      IF(VTYPE.EQ.142) ALTYPE=10
                      IF(VTYPE.EQ.143) ALTYPE=90
                      IF(VTYPE.EQ.144) ALTYPE=91
                      IF(VTYPE.EQ.145) ALTYPE=92
                      IF(VTYPE.EQ.146) ALTYPE=93
                      IF(VTYPE.EQ.147) ALTYPE=94
                      IF(VTYPE.EQ.148) ALTYPE=95
                      IF(VTYPE.EQ.149) ALTYPE=98
C     NEW VALUE IS:
                      V1=VARABL(I,4)
                      ALENS(ALTYPE,INT(VARABL(I,3)))=V1
                  END IF
                  IF(VTYPE.EQ.150) THEN
C     NEW VALUE IS:
                      V1=VARABL(I,4)
                      GPREG(INT(VARABL(I,3)))=V1
                  END IF
                  IF(VTYPE.GE.250.AND.VTYPE.LE.4218) THEN
                      ALTYPE=VTYPE-249
C     NEW VALUE IS:
                      V1=VARABL(I,4)
C     RESET THE APPRORIATE ARRAY VALUE IN THE DEFORMABLE SURFACE
                      ISURF=INT(VARABL(I,3))
                      DEFGR1=ALENS(103,ISURF)
                      DEFGR2=ALENS(104,ISURF)
                      DEFGR3=ALENS(105,ISURF)
                      DEFGR4=ALENS(106,ISURF)
                      DEFGR5=ALENS(107,ISURF)
                      DEFGR6=0.0D0
                      DEFGR7=ALENS(109,ISURF)
                      DEFGR8=0.0D0
                      ACTNUM=ALTYPE
                      NEWDEFVAL=V1
                      ERR1=.FALSE.
                      ERR2=.FALSE.
                      CALL DEFGRIDS(6,ISURF,ERR1,ERR2)
                  END IF
                  IF(VTYPE.GE.27.AND.VTYPE.LE.74) THEN
C     SPECIAL SURFACE COEFFICIENTS
C     NEW VALUE IS:
                      V1=VARABL(I,4)
                      FTFL01((VTYPE-26),INT(VARABL(I,3)))=V1
C                       SPECIAL SURFACE COEFICIENTS DONE
                  END IF
                  IF(VTYPE.GE.76.AND.VTYPE.LE.123) THEN
C     SPECIAL SURFACE COEFFICIENTS
C     NEW VALUE IS:
                      V1=VARABL(I,4)
                      FTFL01((VTYPE-27),INT(VARABL(I,3)))=V1
C                       SPECIAL SURFACE COEFICIENTS DONE
                  END IF
C
              ELSE
C     CONFIGS VARIABLE
C     VCFG IS THE CONFIG NUMBER
                  VCFG=INT(VARABL(I,2))
C     VTYPE IS THE VARIABLE TYPE NUMBER AS USED IN THE VARIABLES ARRAYS
                  VTYPE=INT(VARABL(I,1))
C
C     FOR VARIABLE I, APPLY THE SPECIFIED CHANGE TO THE SPECIFIED
C     CONFIG
C
C     THE NEW VARAIBLE VALUE IS JUST
                  V1=VARABL(I,4)
C     CONVERT THIS VALUE TO A CHARACTER*23 CHARACTER VARIABLE
                  VADD=INT(VARABL(I,14))
                  IF(CFADD(VADD,1).EQ.1.AND.V1.NE.0.0D0) V1=1.0D0/V1
                  IF(CFADD(VADD,1).EQ.9.AND.V1.NE.0.0D0) V1=1.0D0/V1
                  CALL AUXNTA
C
C     THE POSITION IN THE CFADD,CFVAL AND CFCHAR ARRAYS WHERE THIS
C     VARIABLE IS FOUND IS:
C
                  IF(CFADD(VADD,1).GE.27.AND.CFADD(VADD,1).LE.74.OR.
     1            CFADD(VADD,1).GE.76.AND.CFADD(VADD,1).LE.123
     1            .OR.CFADD(VADD,1).EQ.141) THEN
                      CFVAL(VADD,2)=V1
                      CFCHAR(VADD,2)=AV1
C     NOW UPDATE THE CONFIG ARRAY
                      CONFG(CFADD(VADD,3),CFADD(VADD,9))(CFADD(VADD,6):CFADD(VADD,7))
     1                =AV1(1:23)
                  ELSE
                      CFVAL(VADD,1)=V1
                      CFCHAR(VADD,1)=AV1
C     NOW UPDATE THE CONFIG ARRAY
                      CONFG(CFADD(VADD,3),CFADD(VADD,9))(CFADD(VADD,4):CFADD(VADD,5))
     1                =AV1(1:23)
                  END IF
C     NOW LOOK UP WHERE THIS CHARACTER REPRESENTATION OF THE NEW VALUE
C     SHOULD BE STUFFED INTO THE CONFIG ARRAYS CONFG AND
C     STUFF IT THERE.
C
C     NOW LOOP BACK AND REPEAT FOR THE NEXT VARIABLE.
C     FINISHED WITH A CONFIG VARIABLE
              END IF
C     LOOP TO NEXT VARIABL
          END DO
C     UPDATE THE LENS OR THE PERMANENT LENS WILL BE ******!
          CALL FIXDEFORMFILE
          F6=1
          F1=0
          F22=0
          LNSTYP=2
          CALL LNSEOS
C
C     RE-EVALUATE THE OPERANDS AND DISPLAY
C     THE NEW FIGURE OF MERIT, THE OLD FIGURE OF MERIT  AND ITS CHANGE
          SAVE_KDP(4)=SAVEINPT(4)
          F28=1
          MSG=.FALSE.
          OPTMES=.FALSE.
          WC='FMT'
          WQ='        '
          SQ=0
          SN=0
          SST=0
          WS=' '
          DF1=1
          S1=0
          S2=0
          S3=0
          S4=0
          S5=0
          DF2=1
          DF3=1
          DF4=1
          DF5=1
          STI=0
          CALL FMT2
          REST_KDP(4)=RESTINPT(4)
          DEALLOCATE(OLDX,DELOLDX,VSAVE,STAT=ALLOERR)
          RETURN
C       ALL DONE
      END


C SUB SOLVIT.FOR
      SUBROUTINE SOLVIT(IID,JJD,MDERIV,SILENT)

          USE SVDSUB
      
          IMPLICIT NONE
          CHARACTER OOLDWQ*8
          LOGICAL ITDER,SILENT
          INTEGER SSN,SM,NP2,MP,N,J,I,L,M,VTYPE,ALTYPE,VADD,VCFG
     1    ,VN1,MAXCNT,VN,ALLOERR,IID,JJD
          INTEGER ISURF
          REAL*8 NEWDEFVAL

          COMMON/DEFVALCOM/NEWDEFVAL
          REAL*8 X(1:100000),WT,V1
          DIMENSION WT(:)
          ALLOCATABLE :: WT

          CHARACTER AV1*23
          COMMON/SVD1/SM,SSN,MP,NP2
          COMMON /SVD2/ X
          COMMON/CAUX1/V1,AV1
          REAL*8 MDERIV
          DIMENSION MDERIV(1:IID,1:JJD)

          LOGICAL ERR1,ERR2

          INCLUDE 'datsub.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          INCLUDE 'datcfg.inc'
          INCLUDE 'datmac.inc'

          REAL*8 W,V,BTB,BTG,DIAGSUM,WMAX,WMIN
          DIMENSION W(:),V(:,:),BTB(:,:),BTG(:)
          ALLOCATABLE :: W,V,BTB,BTG
          VN=IID
          VN1=IID
          DEALLOCATE(WT,W,V,BTB,BTG,STAT=ALLOERR)
          ALLOCATE (W(VN),V(VN,VN),
     1    WT(VN1),BTB(VN1,VN1),BTG(VN1),STAT=ALLOERR)
          W(1:VN)=0.0D0
          WT(1:VN)=0.0D0
          BTG(1:VN)=0.0D0
          V(1:VN,1:VN)=0.0D0
          BTB(1:VN,1:VN)=0.0D0
          OOLDWQ='        '
          ITDER=.FALSE.
C
          IF(WQ.NE.'D'.AND.WQ.NE.'DIR') THEN
C
C     JUST ITER OR ITER,1
C     THIS IS SUBROUTINE SOLVIT. THIS IS THE SUBROUTINE WHICH
C     HANDLES THE SOLUTION TO THE DAMPED LEAST SQUARES PROBLEM.
C     FORM (BTB+KC)
C     FIRST JUST BTB
C     THIS IS THE MATRIX PRODUCT OF THE TRANSPOSE OF THE DERIV MATRIX
C     TIMES THE DERIV MATRIX ITSELF
C     L IS THE FIRST INDEX OF BTB AND M IS THE SECOND
C     BTB IS SQUARE WITH DIMENSION VBCNT
C
              IF(VBCNT.GE.OPCNT) MAXCNT=VBCNT
              IF(VBCNT.LT.OPCNT) MAXCNT=OPCNT
C
C       ZERO OUT TOO SMALL DERIVATIVES
C
              DO I=1,MAXCNT
                  DO M=1,MAXCNT
                      IF(DABS(MDERIV(I,M)).LT.ONTOL) MDERIV(I,M)=0.0D0
                  END DO
              END DO
C
              DO L=1,MAXCNT
                  DO M=1,MAXCNT
                      DO I=1,MAXCNT
                          BTB(L,M)=BTB(L,M)+(MDERIV(I,L)*MDERIV(I,M))
                      END DO
                  END DO
              END DO
C     NOW KC IS A DIAGONAL TERM ADDED TO THE DIAGONAL ELEMENTS OF
C     BTB
C     NOTE IF THE VARIABLE WEIGHTS ARE ALL 1.0D0 (DEFAULT)
C     KC BECOMES KI, THE P FACTOR TIMES THE IDENTITY MATRIX.
              DO I=1,VBCNT
                  IF(VARABL(I,7).NE.0.0D0) WT(I)=1.0D0/VARABL(I,7)
                  IF(VARABL(I,7).EQ.0.0D0) WT(I)=0.0D0
              END DO
              IF(DMP.EQ.6) THEN
                  DIAGSUM=0.0D0
                  DO I=1,MAXCNT
                      DIAGSUM=DIAGSUM+DABS(BTB(I,I))
                  END DO
                  IF(DIAGSUM.EQ.0.0D0) THEN
                      WRITE(OUTLYNE,*) 'THE DIAGONAL OF BTB IS ZERO'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*) 'DAMPING WILL HAVE NO EFFECT'
                      CALL SHOWIT(1)
                  END IF
              END IF

              DO I=1,MAXCNT
                  IF(DMP.EQ.6)
     1            BTB(I,I)=BTB(I,I)*(1.0D0+(1000.0D0*PFAC*WT(I)))
              END DO

C     NOW FORM BTG
              DO L=1,VBCNT
                  DO I=1,OPCNT
                      BTG(L)=BTG(L)+(MDERIV(I,L)*(-OPERND(I,14)))
                  END DO
              END DO
C
C     NOW SOLVE THE PROBLEM WITH SVD AND THEN APPLY THE SOLUTION TO
C     THE LENS
C
              MP=MAXCNT
              NP2=MAXCNT
              N=MAXCNT
              M=MAXCNT
C       DO SINGULAR VALUE DECOMPOSITION
              SM=M
              SSN=N
              CALL SVDCMP(BTB,SM,SSN,MP,NP2,W,V)
              M=SM
              N=SSN
              WMAX=-1.0D300
              DO J=1,N
                  IF(W(J).GT.WMAX) WMAX=W(J)
              END DO
              WMIN=WMAX*SINGTOL
              DO J=1,N
                  IF(W(J).LT.WMIN) W(J)=0.0D0
              END DO
C
C       SOLVE LINEAR EQUATION
C
              SM=M
              SSN=N
              CALL SVBKSB(BTB,W,V,SM,SSN,VN1,VN,BTG,X)
              M=SM
              N=SSN
C
C
C     NOW APPLY THE SOLUTION VECTOR X(I) TO THE VARABL ARRAY AND TO THE LENS
C     THE ITH SOLUTION VECTOR COMPONENT APPLIES TO THE ITH VARIABLE
              LCVLCV=0.0D0
              DO I=1,VBCNT

C     THE CURRENT VARIABLE VALUE BECOMES THE PREVIOUS VARIABLE VALUE
                  VARABL(I,5)=VARABL(I,4)
C
C     THE NEW SOLUTION VECTOR ADDED TO THE PREVIOS VARIABLE VALUE
C     BECOMES THE NEW SOLUTION VECTOR
C     VALUE
                  LCVLCV=LCVLCV+DABS(((X(I)))/(DINMUL*VARABL(I,8)))
                  VARABL(I,4)=VARABL(I,4)+(X(I))
C
                  IF(VARABL(I,1).EQ.1.0D0) THEN
                      IF(VARABL(I,9).LT.THMINLIM) VARABL(I,9)=THMINLIM
                      IF(VARABL(I,10).GT.THMAXLIM) VARABL(I,10)=THMAXLIM
                  END IF
                  IF(VARABL(I,1).EQ.2.0D0.OR.VARABL(I,1).EQ.10.0D0) THEN
                      IF(RDNEGLIM.EQ.0.0D0) RDNEGLIM=-1.0D-20
                      IF(RDPOSLIM.EQ.0.0D0) RDPOSLIM=1.0D-20
                      IF(VARABL(I,9).LT.(1.0D0/RDNEGLIM)) VARABL(I,9)=1.0D0/RDNEGLIM
                      IF(VARABL(I,10).GT.(1.0D0/RDPOSLIM)) VARABL(I,10)=1.0D0/RDPOSLIM
                  END IF
C     BOUNDS CHECKER
                  IF(VARABL(I,4).LT.VARABL(I,9)) THEN
                      VARABL(I,4)=VARABL(I,9)
                      WRITE(OUTLYNE,*)
     1                'WARNING: '
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'VARIABLE # ',I,' HAS REACHED ITS LOWER BOUNDARY VALUE'
                      CALL SHOWIT(1)
                  END IF
                  IF(VARABL(I,4).GT.VARABL(I,10)) THEN
                      VARABL(I,4)=VARABL(I,10)
                      WRITE(OUTLYNE,*)
     1                'WARNING: '
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'VARIABLE # ',I,' HAS REACHED ITS UPPER BOUNDARY VALUE'
                      CALL SHOWIT(1)
                  END IF
C
C
C     THE VARIABLE CHANGE IS (THE NEW CURRENT VALUE-OLD LAST CURRENT VALUE)
                  VARABL(I,6)=VARABL(I,4)-VARABL(I,5)
C
C     DINCRS ARE ONLY CHANGED BY HAND AND BY THE ITER ADJUST COMMAND
C
              END DO
              LCVLCV=(LCVLCV)/DBLE(VBCNT)
C
C     NOW APPLY THE CHANGE VECTOR TO THE LENS
C**********************************************************************
              DO I=1,VBCNT

C     FIRST, IF THE VARIABLE IS A CONFIG 1 VARIABLE
C     NEXT IF THE VARIABLE IS IN A CONFIG OTHER THAN 1
                  IF(VARABL(I,2).EQ.1.0D0) THEN
C     NON-CONFIGS VARIABLE MEANING CONFIG 1
C     THIS IS A LENS LEVEL VARIABLE CHANGE
C     GET THE DATA TYPE NUMBER OF THE VARIABLE
                      VTYPE=INT(VARABL(I,1))
C                          CURVATURE
                      IF(VTYPE.EQ.2.OR.VTYPE.EQ.1) THEN
C     SURFACE CURVATURE
C     NEW VALUE IS:
                          V1=VARABL(I,4)
                          ALENS(1,INT(VARABL(I,3)))=V1
C                       CURVATURE DONE
                      END IF
                      IF(VTYPE.EQ.10.OR.VTYPE.EQ.9) THEN
C     SURFACE TORIC CURVATURE
C     NEW VALUE IS:
                          V1=VARABL(I,4)
                          ALENS(24,INT(VARABL(I,3)))=V1
C                       TORIC CURVATURE DONE
                      END IF
                      IF(VTYPE.GE.3.AND.VTYPE.LE.8) THEN
                          IF(VTYPE.EQ.3) ALTYPE=3
                          IF(VTYPE.EQ.4) ALTYPE=2
                          IF(VTYPE.EQ.5) ALTYPE=4
                          IF(VTYPE.EQ.6) ALTYPE=5
                          IF(VTYPE.EQ.7) ALTYPE=6
                          IF(VTYPE.EQ.8) ALTYPE=7
C     NEW VALUE IS:
                          V1=VARABL(I,4)
                          ALENS(ALTYPE,INT(VARABL(I,3)))=V1
C                       THESE VARIABLES DONE
                      END IF
                      IF(VTYPE.GE.11.AND.VTYPE.LE.25.OR.VTYPE.EQ.75.OR.VTYPE.GE.
     1                124.AND.VTYPE.LE.149) THEN
                          IF(VTYPE.EQ.11) ALTYPE=41
                          IF(VTYPE.EQ.12) ALTYPE=37
                          IF(VTYPE.EQ.13) ALTYPE=38
                          IF(VTYPE.EQ.14) ALTYPE=39
                          IF(VTYPE.EQ.15) ALTYPE=40
                          IF(VTYPE.EQ.16) ALTYPE=118
                          IF(VTYPE.EQ.17) ALTYPE=119
                          IF(VTYPE.EQ.18) ALTYPE=120
                          IF(VTYPE.EQ.19) ALTYPE=114
                          IF(VTYPE.EQ.20) ALTYPE=115
                          IF(VTYPE.EQ.21) ALTYPE=46
                          IF(VTYPE.EQ.22) ALTYPE=47
                          IF(VTYPE.EQ.23) ALTYPE=48
                          IF(VTYPE.EQ.24) ALTYPE=49
                          IF(VTYPE.EQ.25) ALTYPE=50
                          IF(VTYPE.EQ.75) ALTYPE=43
                          IF(VTYPE.EQ.124) ALTYPE=71
                          IF(VTYPE.EQ.125) ALTYPE=72
                          IF(VTYPE.EQ.126) ALTYPE=73
                          IF(VTYPE.EQ.127) ALTYPE=74
                          IF(VTYPE.EQ.128) ALTYPE=75
                          IF(VTYPE.EQ.129) ALTYPE=81
                          IF(VTYPE.EQ.130) ALTYPE=82
                          IF(VTYPE.EQ.131) ALTYPE=83
                          IF(VTYPE.EQ.132) ALTYPE=84
                          IF(VTYPE.EQ.133) ALTYPE=85
                          IF(VTYPE.EQ.134) ALTYPE=116
                          IF(VTYPE.EQ.135) ALTYPE=86
                          IF(VTYPE.EQ.136) ALTYPE=87
                          IF(VTYPE.EQ.137) ALTYPE=78
                          IF(VTYPE.EQ.138) ALTYPE=79
                          IF(VTYPE.EQ.139) ALTYPE=80
                          IF(VTYPE.EQ.140) ALTYPE=89
                          IF(VTYPE.EQ.141) ALTYPE=11
                          IF(VTYPE.EQ.142) ALTYPE=10
                          IF(VTYPE.EQ.143) ALTYPE=90
                          IF(VTYPE.EQ.144) ALTYPE=91
                          IF(VTYPE.EQ.145) ALTYPE=92
                          IF(VTYPE.EQ.146) ALTYPE=93
                          IF(VTYPE.EQ.147) ALTYPE=94
                          IF(VTYPE.EQ.148) ALTYPE=95
                          IF(VTYPE.EQ.149) ALTYPE=98
C     NEW VALUE IS:
                          V1=VARABL(I,4)
                          ALENS(ALTYPE,INT(VARABL(I,3)))=V1
                      END IF
                      IF(VTYPE.EQ.150) THEN
C     NEW VALUE IS:
                          V1=VARABL(I,4)
                          GPREG(INT(VARABL(I,3)))=V1
                      END IF
                      IF(VTYPE.GE.250.AND.VTYPE.LE.4218) THEN
                          ALTYPE=VTYPE-249
C     NEW VALUE IS:
                          V1=VARABL(I,4)
C     RESET THE APPRORIATE ARRAY VALUE IN THE DEFORMABLE SURFACE
                          ISURF=INT(VARABL(I,3))
                          DEFGR1=ALENS(103,ISURF)
                          DEFGR2=ALENS(104,ISURF)
                          DEFGR3=ALENS(105,ISURF)
                          DEFGR4=ALENS(106,ISURF)
                          DEFGR5=ALENS(107,ISURF)
                          DEFGR6=0.0D0
                          DEFGR7=ALENS(109,ISURF)
                          DEFGR8=0.0D0
                          ACTNUM=ALTYPE
                          NEWDEFVAL=V1
                          ERR1=.FALSE.
                          ERR2=.FALSE.
                          CALL DEFGRIDS(6,ISURF,ERR1,ERR2)
                      END IF
                      IF(VTYPE.GE.27.AND.VTYPE.LE.74) THEN
C     SPECIAL SURFACE COEFFICIENTS
C     NEW VALUE IS:
                          V1=VARABL(I,4)
                          FTFL01((VTYPE-26),INT(VARABL(I,3)))=V1
C                       SPECIAL SURFACE COEFICIENTS DONE
                      END IF
                      IF(VTYPE.GE.76.AND.VTYPE.LE.123) THEN
C     SPECIAL SURFACE COEFFICIENTS
C     NEW VALUE IS:
                          V1=VARABL(I,4)
                          FTFL01((VTYPE-27),INT(VARABL(I,3)))=V1
C                       SPECIAL SURFACE COEFICIENTS DONE
                      END IF
C
                  ELSE
C     CONFIGS VARIABLE
C     VCFG IS THE CONFIG NUMBER
                      VCFG=INT(VARABL(I,2))
C     VTYPE IS THE VARIABLE TYPE NUMBER AS USED IN THE VARIABLES ARRAYS
                      VTYPE=INT(VARABL(I,1))
C
C     FOR VARIABLE I, APPLY THE SPECIFIED CHANGE TO THE SPECIFIED
C     CONFIG
C
C     THE NEW VARAIBLE VALUE IS JUST
                      V1=VARABL(I,4)
C     CONVERT THIS VALUE TO A CHARACTER*23 CHARACTER VARIABLE
                      VADD=INT(VARABL(I,14))
                      IF(CFADD(VADD,1).EQ.1.AND.V1.NE.0.0D0) V1=1.0D0/V1
                      IF(CFADD(VADD,1).EQ.9.AND.V1.NE.0.0D0) V1=1.0D0/V1
                      CALL AUXNTA
C
C     THE POSITION IN THE CFADD,CFVAL AND CFCHAR ARRAYS WHERE THIS
C     VARIABLE IS FOUND IS:
C
                      IF(CFADD(VADD,1).GE.27.AND.CFADD(VADD,1).LE.74.OR.
     1                CFADD(VADD,1).GE.76.AND.CFADD(VADD,1).LE.123
     1                .OR.CFADD(VADD,1).EQ.141) THEN
                          CFVAL(VADD,2)=V1
                          CFCHAR(VADD,2)=AV1
C     NOW UPDATE THE CONFIG ARRAY
                          CONFG(CFADD(VADD,3),CFADD(VADD,9))(CFADD(VADD,6):CFADD(VADD,7))
     1                    =AV1(1:23)
                      ELSE
                          CFVAL(VADD,1)=V1
                          CFCHAR(VADD,1)=AV1
C     NOW UPDATE THE CONFIG ARRAY
                          CONFG(CFADD(VADD,3),CFADD(VADD,9))(CFADD(VADD,4):CFADD(VADD,5))
     1                    =AV1(1:23)
                      END IF
C     NOW LOOK UP WHERE THIS CHARACTER REPRESENTATION OF THE NEW VALUE
C     SHOULD BE STUFFED INTO THE CONFIG ARRAYS CONFG AND
C     STUFF IT THERE.
C
C     NOW LOOP BACK AND REPEAT FOR THE NEXT VARIABLE.
C     FINISHED WITH A CONFIG VARIABLE
                  END IF
C     LOOP TO NEXT VARIABL
              END DO
C     UPDATE THE LENS OR THE PERMANENT LENS WILL BE ******!
              CALL FIXDEFORMFILE
              F6=1
              F1=0
              F22=0
              LNSTYP=2
              CALL LNSEOS
C**********************************************************************
C     AND RE-EVALUATE THE OPERANDS AND DISPLAY
C     THE NEW FIGURE OF MERIT, THE OLD FIGURE OF MERIT  AND ITS CHANGE
              SAVE_KDP(4)=SAVEINPT(4)
              F28=1
              MSG=.FALSE.
              OPTMES=.FALSE.
              WC='FMT'
              WQ='        '
              SQ=0
              SN=0
              SST=0
              WS=' '
              DF1=1
              DF2=1
              DF3=1
              DF4=1
              DF5=1
              STI=0
              SN=0
              S1=0
              S2=0
              S3=0
              S4=0
              S5=0
              W1=0.0D0
              IF(.NOT.SILENT) CALL FMT2
              IF(SILENT) CALL FMT3
              REST_KDP(4)=RESTINPT(4)
              DEALLOCATE(WT,W,V,BTB,BTG,STAT=ALLOERR)
              RETURN
          END IF
          IF(WQ.EQ.'D'.OR.WQ.EQ.'DIR') THEN
C
C     THIS IS ITER DIR.
C     FORM (A) STORED IN THE SPOT FOR BTB TO SAVE MEMORY ONLY
C     THIS IS JUST THE UNWEIGHTED DERIVATIVE MATRIX
C
              BTB(1:OPCNT,1:VBCNT)=MDERIV(1:OPCNT,1:VBCNT)
C     NOW FORM F STORED IN THE SPOT FOR BTG TO SAVE MEMORY ONLY
C     THIS IS JUST THE UNWEIGHTED DESIRED CHANGES TO THE OPERANDS
              BTG(1:OPCNT)=-OPERND(1:OPCNT,14)
C
C     NOW SOLVE THE PROBLEM WITH SVD AND THEN APPLY THE SOLUTION TO
C     THE LENS
C
              MP=VBCNT
              NP2=VBCNT
              N=VBCNT
              M=VBCNT
C       DO SINGULAR VALUE DECOMPOSITION
              SM=M
              SSN=N
              CALL SVDCMP(BTB,SM,SSN,MP,NP2,W,V)
              M=SM
              N=SSN
              WMAX=-1.0D300
              DO J=1,N
                  IF(W(J).GT.WMAX) WMAX=W(J)
              END DO
              WMIN=WMAX*SINGTOL
              DO J=1,N
                  IF(W(J).LT.WMIN) W(J)=0.0D0
              END DO
C
C       SOLVE LINEAR EQUATION
C
              SM=M
              SSN=N
              CALL SVBKSB(BTB,W,V,SM,SSN,VN1,VN,BTG,X)
              M=SM
              N=SSN
C
C     NOW APPLY THE SOLUTION VECTOR X(I) TO THE VARABL ARRAY AND TO THE LENS
C     THE ITH SOLUTION VECTOR COMPONENT APPLIES TO THE ITH VARIABLE
              LCVLCV=0.0D0
C
              DO I=1,VBCNT
C
C     THE CURRENT VARIABLE VALUE BECOMES THE PREVIOUS VARIABLE VALUE
                  VARABL(I,5)=VARABL(I,4)
C
C     THE NEW SOLUTION VECTOR ADDED TO THE PREVIOS VARIABLE VALUE
C     BECOMES THE NEW SOLUTION VECTOR
C     VALUE
                  VARABL(I,4)=VARABL(I,4)+(X(I))
                  LCVLCV=LCVLCV+DABS(((X(I)))/(DINMUL*VARABL(I,8)))
C
C     BOUNDS CHECKER
                  IF(VARABL(I,4).LT.VARABL(I,9)) THEN
                      VARABL(I,4)=VARABL(I,9)
                      WRITE(OUTLYNE,*)
     1                'WARNING: '
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'VARIABLE # ',I,' HAS REACHED ITS LOWER BOUNDARY VALUE!!!!!!!'
                      CALL SHOWIT(1)
                  END IF
                  IF(VARABL(I,4).GT.VARABL(I,10)) THEN
                      VARABL(I,4)=VARABL(I,10)
                      WRITE(OUTLYNE,*)
     1                'WARNING: '
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'VARIABLE # ',I,' HAS REACHED ITS UPPER BOUNDARY VALUE'
                      CALL SHOWIT(1)
                  END IF
C
C
C     THE VARIABLE CHANGE IS (THE NEW CURRENT VALUE-OLD LAST CURRENT VALUE)
                  VARABL(I,6)=VARABL(I,4)-VARABL(I,5)
C
C     DINCRS ARE ONLY CHANGED BY HAND AND BY THE ITER ADJUST COMMAND
C
              END DO
              LCVLCV=DSQRT(LCVLCV)/DBLE(VBCNT)
C
C     NOW APPLY THE CHANGE VECTOR TO THE LENS
C**********************************************************************
              DO I=1,VBCNT
C     FIRST, IF THE VARIABLE IS A CONFIG 1 VARIABLE
C     NEXT IF THE VARIABLE IS IN A CONFIG OTHER THAN 1
                  IF(VARABL(I,2).EQ.1.0D0) THEN
C     NON-CONFIGS VARIABLE MEANING CONFIG 1
C     THIS IS A LENS LEVEL VARIABLE CHANGE
C     GET THE DATA TYPE NUMBER OF THE VARIABLE
                      VTYPE=INT(VARABL(I,1))
C                          CURVATURE
                      IF(VTYPE.EQ.2.OR.VTYPE.EQ.1) THEN
C     SURFACE CURVATURE
C     NEW VALUE IS:
                          V1=VARABL(I,4)
                          ALENS(1,INT(VARABL(I,3)))=V1
C                       CURVATURE DONE
                      END IF
                      IF(VTYPE.EQ.10.OR.VTYPE.EQ.9) THEN
C     SURFACE TORIC CURVATURE
C     NEW VALUE IS:
                          V1=VARABL(I,4)
                          ALENS(24,INT(VARABL(I,3)))=V1
C                       TORIC CURVATURE DONE
                      END IF
                      IF(VTYPE.GE.3.AND.VTYPE.LE.8) THEN
                          IF(VTYPE.EQ.3) ALTYPE=3
                          IF(VTYPE.EQ.4) ALTYPE=2
                          IF(VTYPE.EQ.5) ALTYPE=4
                          IF(VTYPE.EQ.6) ALTYPE=5
                          IF(VTYPE.EQ.7) ALTYPE=6
                          IF(VTYPE.EQ.8) ALTYPE=7
C     NEW VALUE IS:
                          V1=VARABL(I,4)
                          ALENS(ALTYPE,INT(VARABL(I,3)))=V1
C                       THESE VARIABLES DONE
                      END IF
                      IF(VTYPE.GE.11.AND.VTYPE.LE.25.OR.VTYPE.EQ.75.OR.VTYPE.GE.
     1                124.AND.VTYPE.LE.149) THEN
                          IF(VTYPE.EQ.11) ALTYPE=41
                          IF(VTYPE.EQ.12) ALTYPE=37
                          IF(VTYPE.EQ.13) ALTYPE=38
                          IF(VTYPE.EQ.14) ALTYPE=39
                          IF(VTYPE.EQ.15) ALTYPE=40
                          IF(VTYPE.EQ.16) ALTYPE=118
                          IF(VTYPE.EQ.17) ALTYPE=119
                          IF(VTYPE.EQ.18) ALTYPE=120
                          IF(VTYPE.EQ.19) ALTYPE=114
                          IF(VTYPE.EQ.20) ALTYPE=115
                          IF(VTYPE.EQ.21) ALTYPE=46
                          IF(VTYPE.EQ.22) ALTYPE=47
                          IF(VTYPE.EQ.23) ALTYPE=48
                          IF(VTYPE.EQ.24) ALTYPE=49
                          IF(VTYPE.EQ.25) ALTYPE=50
                          IF(VTYPE.EQ.75) ALTYPE=43
                          IF(VTYPE.EQ.124) ALTYPE=71
                          IF(VTYPE.EQ.125) ALTYPE=72
                          IF(VTYPE.EQ.126) ALTYPE=73
                          IF(VTYPE.EQ.127) ALTYPE=74
                          IF(VTYPE.EQ.128) ALTYPE=75
                          IF(VTYPE.EQ.129) ALTYPE=81
                          IF(VTYPE.EQ.130) ALTYPE=82
                          IF(VTYPE.EQ.131) ALTYPE=83
                          IF(VTYPE.EQ.132) ALTYPE=84
                          IF(VTYPE.EQ.133) ALTYPE=85
                          IF(VTYPE.EQ.134) ALTYPE=116
                          IF(VTYPE.EQ.135) ALTYPE=86
                          IF(VTYPE.EQ.136) ALTYPE=87
                          IF(VTYPE.EQ.137) ALTYPE=78
                          IF(VTYPE.EQ.138) ALTYPE=79
                          IF(VTYPE.EQ.139) ALTYPE=80
                          IF(VTYPE.EQ.140) ALTYPE=89
                          IF(VTYPE.EQ.141) ALTYPE=11
                          IF(VTYPE.EQ.142) ALTYPE=10
                          IF(VTYPE.EQ.143) ALTYPE=90
                          IF(VTYPE.EQ.144) ALTYPE=91
                          IF(VTYPE.EQ.145) ALTYPE=92
                          IF(VTYPE.EQ.146) ALTYPE=93
                          IF(VTYPE.EQ.147) ALTYPE=94
                          IF(VTYPE.EQ.148) ALTYPE=95
                          IF(VTYPE.EQ.149) ALTYPE=98
C     NEW VALUE IS:
                          V1=VARABL(I,4)
                          ALENS(ALTYPE,INT(VARABL(I,3)))=V1
                      END IF
                      IF(VTYPE.EQ.150) THEN
C     NEW VALUE IS:
                          V1=VARABL(I,4)
                          GPREG(INT(VARABL(I,3)))=V1
                      END IF
                      IF(VTYPE.GE.250.AND.VTYPE.LE.4218) THEN
                          ALTYPE=VTYPE-249
C     NEW VALUE IS:
                          V1=VARABL(I,4)
C     RESET THE APPRORIATE ARRAY VALUE IN THE DEFORMABLE SURFACE
                          ISURF=INT(VARABL(I,3))
                          DEFGR1=ALENS(103,ISURF)
                          DEFGR2=ALENS(104,ISURF)
                          DEFGR3=ALENS(105,ISURF)
                          DEFGR4=ALENS(106,ISURF)
                          DEFGR5=ALENS(107,ISURF)
                          DEFGR6=0.0D0
                          DEFGR7=ALENS(109,ISURF)
                          DEFGR8=0.0D0
                          ACTNUM=ALTYPE
                          NEWDEFVAL=V1
                          ERR1=.FALSE.
                          ERR2=.FALSE.
                          CALL DEFGRIDS(6,ISURF,ERR1,ERR2)
                      END IF
                      IF(VTYPE.GE.27.AND.VTYPE.LE.74) THEN
C     SPECIAL SURFACE COEFFICIENTS
C     NEW VALUE IS:
                          V1=VARABL(I,4)
                          FTFL01((VTYPE-26),INT(VARABL(I,3)))=V1
C                       SPECIAL SURFACE COEFICIENTS DONE
                      END IF
                      IF(VTYPE.GE.76.AND.VTYPE.LE.123) THEN
C     SPECIAL SURFACE COEFFICIENTS
C     NEW VALUE IS:
                          V1=VARABL(I,4)
                          FTFL01((VTYPE-27),INT(VARABL(I,3)))=V1
C                       SPECIAL SURFACE COEFICIENTS DONE
                      END IF
C
                  ELSE
C     CONFIGS VARIABLE
C     VCFG IS THE CONFIG NUMBER
                      VCFG=INT(VARABL(I,2))
C     VTYPE IS THE VARIABLE TYPE NUMBER AS USED IN THE VARIABLES ARRAYS
                      VTYPE=INT(VARABL(I,1))
C
C     FOR VARIABLE I, APPLY THE SPECIFIED CHANGE TO THE SPECIFIED
C     CONFIG
C
C     THE NEW VARAIBLE VALUE IS JUST
                      V1=VARABL(I,4)
C     CONVERT THIS VALUE TO A CHARACTER*23 CHARACTER VARIABLE
                      VADD=INT(VARABL(I,14))
                      IF(CFADD(VADD,1).EQ.1.AND.V1.NE.0.0D0) V1=1.0D0/V1
                      IF(CFADD(VADD,1).EQ.9.AND.V1.NE.0.0D0) V1=1.0D0/V1
                      CALL AUXNTA
C
C     THE POSITION IN THE CFADD,CFVAL AND CFCHAR ARRAYS WHERE THIS
C     VARIABLE IS FOUND IS:
C
                      IF(CFADD(VADD,1).GE.27.AND.CFADD(VADD,1).LE.74.OR.
     1                CFADD(VADD,1).GE.76.AND.CFADD(VADD,1).LE.123
     1                .OR.CFADD(VADD,1).EQ.141) THEN
                          CFVAL(VADD,2)=V1
                          CFCHAR(VADD,2)=AV1
C     NOW UPDATE THE CONFIG ARRAY
                          CONFG(CFADD(VADD,3),CFADD(VADD,9))(CFADD(VADD,6):CFADD(VADD,7))
     1                    =AV1(1:23)
                      ELSE
                          CFVAL(VADD,1)=V1
                          CFCHAR(VADD,1)=AV1
C     NOW UPDATE THE CONFIG ARRAY
                          CONFG(CFADD(VADD,3),CFADD(VADD,9))(CFADD(VADD,4):CFADD(VADD,5))
     1                    =AV1(1:23)
                      END IF
C     NOW LOOK UP WHERE THIS CHARACTER REPRESENTATION OF THE NEW VALUE
C     SHOULD BE STUFFED INTO THE CONFIG ARRAYS CONFG AND
C     STUFF IT THERE.
C
C     NOW LOOP BACK AND REPEAT FOR THE NEXT VARIABLE.
C     FINISHED WITH A CONFIG VARIABLE
                  END IF
C     LOOP TO NEXT VARIABL
              END DO
C     UPDATE THE LENS OR THE PERMANENT LENS WILL BE ******!
              CALL FIXDEFORMFILE
              F6=1
              F1=0
              F22=0
              LNSTYP=2
              CALL LNSEOS
C**********************************************************************
C     AND RE-EVALUATE THE OPERANDS AND DISPLAY
C     THE NEW FIGURE OF MERIT, THE OLD FIGURE OF MERIT  AND ITS CHANGE
              SAVE_KDP(4)=SAVEINPT(4)
              F28=1
              MSG=.FALSE.
              OPTMES=.FALSE.
              WC='FMT'
              WQ='        '
              SQ=0
              SN=0
              SST=0
              WS=' '
              DF1=1
              DF2=1
              DF3=1
              DF4=1
              DF5=1
              STI=0
              SN=0
              S1=0
              S2=0
              S3=0
              S4=0
              S5=0
              W1=0.0D0
              IF (.NOT. SILENT) THEN
                 CALL FMT2
              ELSE
                 CALL FMT3
              END IF
              REST_KDP(4)=RESTINPT(4)
              DEALLOCATE(WT,W,V,BTB,BTG,STAT=ALLOERR)
              RETURN
          END IF
      END
C SUB SOLVIT_POWELL.FOR
      SUBROUTINE SOLVIT_POWELL(IID,JJD,MDERIV)
          USE GLOBALS
          USE SVDSUB

          IMPLICIT NONE
          CHARACTER OOLDWQ*8
          LOGICAL ITDER
          INTEGER SSN,SM,NP2,MP,N,J,I,L,M,VTYPE,ALTYPE,VADD,VCFG
     1    ,VN1,MAXCNT,VN,ALLOERR,IID,JJD
          INTEGER ISURF,IVB,IVBB
          REAL*8 NEWDEFVAL
          COMMON/DEFVALCOM/NEWDEFVAL

          REAL*8 X(1:100000),WT,V1

          DIMENSION WT(:)
          ALLOCATABLE :: WT

          CHARACTER AV1*23
          COMMON/SVD1/SM,SSN,MP,NP2
          COMMON/SVD2/X
          COMMON/CAUX1/V1,AV1

          REAL*8 MDERIV
          DIMENSION MDERIV(1:IID,1:JJD)

          LOGICAL ERR1,ERR2

          INCLUDE 'datsub.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          INCLUDE 'datcfg.inc'
          INCLUDE 'datmac.inc'

          REAL*8 W,V,BTB,BTG,WMAX,WMIN
          DIMENSION W(:),V(:,:),BTB(:,:),BTG(:)
          ALLOCATABLE :: W,V,BTB,BTG
          VN=IID
          VN1=IID
          DEALLOCATE(WT,W,V,BTB,BTG,STAT=ALLOERR)
          ALLOCATE (W(VN),V(VN,VN),
     1    WT(VN1),BTB(VN1,VN1),BTG(VN1),STAT=ALLOERR)
          W(1:VN)=0.0D0
          WT(1:VN)=0.0D0
          BTG(1:VN)=0.0D0
          V(1:VN,1:VN)=0.0D0
          BTB(1:VN,1:VN)=0.0D0
          OOLDWQ='        '
          ITDER=.FALSE.
C
C THIS IS SUBROUTINE SOLVIT_POWELL. THIS IS THE SUBROUTINE WHICH
C HANDLES THE SOLUTION TO THE ITER POWELL PROBLEM
C     FORM (BTB+KC)
C     FIRST JUST BTB
C     THIS IS THE MATRIX PRODUCT OF THE TRANSPOSE OF THE DERIV MATRIX
C     TIMES THE DERIV MATRIX ITSELF
C     L IS THE FIRST INDEX OF BTB AND M IS THE SECOND
C     BTB IS SQUARE WITH DIMENSION VBCNT
C
C     MAKE OUR SHUFFLE LIST OF RANDOMLY CHOSEN VARIABLE NUMBERS
          N=VBCNT
          DEALLOCATE (DARR,STAT=ALLOERR)
          ALLOCATE (DARR(1:N,1:2),STAT=ALLOERR)
          CALL SHUFFLE(N,DARR)
C
          IF(VBCNT.GE.OPCNT) MAXCNT=VBCNT
          IF(VBCNT.LT.OPCNT) MAXCNT=OPCNT
C
C       ZERO OUT TOO SMALL DERIVATIVES
C
          DO I=1,MAXCNT
              DO M=1,MAXCNT
                  IF(DABS(MDERIV(I,M)).LT.ONTOL) MDERIV(I,M)=0.0D0
              END DO
          END DO
C
          DO L=1,MAXCNT
              DO M=1,MAXCNT
                  DO I=1,MAXCNT
                      BTB(L,M)=BTB(L,M)+(MDERIV(I,L)*MDERIV(I,M))
                  END DO
              END DO
          END DO
C
C                       WE DO THIS ONCE FOR EACH VARIABLE
          DO IVBB=1,VBCNT
              IVB=INT(DARR(IVBB,2))
C
C     NO DAMPING IS USED IN ITER POWELL

C     NOW FORM BTG
              L=IVB
              DO I=1,OPCNT
                  BTG(L)=BTG(L)+(MDERIV(I,L)*(-OPERND(I,14)))
              END DO
C
C     NOW SOLVE THE PROBLEM WITH SVD AND THEN APPLY THE SOLUTION TO
C     THE LENS
C
              MP=MAXCNT
              NP2=MAXCNT
              N=MAXCNT
              M=MAXCNT
C       DO SINGULAR VALUE DECOMPOSITION
              SM=M
              SSN=N
              CALL SVDCMP(BTB,SM,SSN,MP,NP2,W,V)
              M=SM
              N=SSN
              WMAX=-1.0D300
              DO J=1,N
                  IF(W(J).GT.WMAX) WMAX=W(J)
              END DO
              WMIN=WMAX*SINGTOL
              DO J=1,N
                  IF(W(J).LT.WMIN) W(J)=0.0D0
              END DO
C
C       SOLVE LINEAR EQUATION
C
              SM=M
              SSN=N
              CALL SVBKSB(BTB,W,V,SM,SSN,VN1,VN,BTG,X)
              M=SM
              N=SSN
C
C
C     NOW APPLY THE SOLUTION VECTOR X(I) TO THE VARABL ARRAY AND TO THE LENS
C     THE ITH SOLUTION VECTOR COMPONENT APPLIES TO THE ITH VARIABLE
              LCVLCV=0.0D0
              I=IVB
C
C     THE CURRENT VARIABLE VALUE BECOMES THE PREVIOUS VARIABLE VALUE
              VARABL(I,5)=VARABL(I,4)
C
C     THE NEW SOLUTION VECTOR ADDED TO THE PREVIOS VARIABLE VALUE
C     BECOMES THE NEW SOLUTION VECTOR
C     VALUE
              LCVLCV=LCVLCV+DABS(((X(I)))/(DINMUL*VARABL(I,8)))
              VARABL(I,4)=VARABL(I,4)+(X(I))
C
              IF(VARABL(I,1).EQ.1.0D0) THEN
                  IF(VARABL(I,9).LT.THMINLIM) VARABL(I,9)=THMINLIM
                  IF(VARABL(I,10).GT.THMAXLIM) VARABL(I,10)=THMAXLIM
              END IF
              IF(VARABL(I,1).EQ.2.0D0.OR.VARABL(I,1).EQ.10.0D0) THEN
                  IF(RDNEGLIM.EQ.0.0D0) RDNEGLIM=-1.0D-20
                  IF(RDPOSLIM.EQ.0.0D0) RDPOSLIM=1.0D-20
                  IF(VARABL(I,9).LT.(1.0D0/RDNEGLIM)) VARABL(I,9)=1.0D0/RDNEGLIM
                  IF(VARABL(I,10).GT.(1.0D0/RDPOSLIM)) VARABL(I,10)=1.0D0/RDPOSLIM
              END IF
C     BOUNDS CHECKER
              IF(VARABL(I,4).LT.VARABL(I,9)) THEN
                  VARABL(I,4)=VARABL(I,9)
                  WRITE(OUTLYNE,*)
     1            'WARNING: '
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)
     1            'VARIABLE # ',I,' HAS REACHED ITS LOWER BOUNDARY VALUE'
                  CALL SHOWIT(1)
              END IF
              IF(VARABL(I,4).GT.VARABL(I,10)) THEN
                  VARABL(I,4)=VARABL(I,10)
                  WRITE(OUTLYNE,*)
     1            'WARNING: '
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)
     1            'VARIABLE # ',I,' HAS REACHED ITS UPPER BOUNDARY VALUE'
                  CALL SHOWIT(1)
              END IF
C
C
C     THE VARIABLE CHANGE IS (THE NEW CURRENT VALUE-OLD LAST CURRENT VALUE)
              VARABL(I,6)=VARABL(I,4)-VARABL(I,5)
C
C     DINCRS ARE ONLY CHANGED BY HAND AND BY THE ITER ADJUST COMMAND
C
              LCVLCV=(LCVLCV)/DBLE(VBCNT)
C
C     NOW APPLY THE CHANGE VECTOR TO THE LENS
C**********************************************************************
              I=IVB
C     FIRST, IF THE VARIABLE IS A CONFIG 1 VARIABLE
C     NEXT IF THE VARIABLE IS IN A CONFIG OTHER THAN 1
              IF(VARABL(I,2).EQ.1.0D0) THEN
C     NON-CONFIGS VARIABLE MEANING CONFIG 1
C     THIS IS A LENS LEVEL VARIABLE CHANGE
C     GET THE DATA TYPE NUMBER OF THE VARIABLE
                  VTYPE=INT(VARABL(I,1))
C                          CURVATURE
                  IF(VTYPE.EQ.2.OR.VTYPE.EQ.1) THEN
C     SURFACE CURVATURE
C     NEW VALUE IS:
                      V1=VARABL(I,4)
                      ALENS(1,INT(VARABL(I,3)))=V1
C                       CURVATURE DONE
                  END IF
                  IF(VTYPE.EQ.10.OR.VTYPE.EQ.9) THEN
C     SURFACE TORIC CURVATURE
C     NEW VALUE IS:
                      V1=VARABL(I,4)
                      ALENS(24,INT(VARABL(I,3)))=V1
C                       TORIC CURVATURE DONE
                  END IF
                  IF(VTYPE.GE.3.AND.VTYPE.LE.8) THEN
                      IF(VTYPE.EQ.3) ALTYPE=3
                      IF(VTYPE.EQ.4) ALTYPE=2
                      IF(VTYPE.EQ.5) ALTYPE=4
                      IF(VTYPE.EQ.6) ALTYPE=5
                      IF(VTYPE.EQ.7) ALTYPE=6
                      IF(VTYPE.EQ.8) ALTYPE=7
C     NEW VALUE IS:
                      V1=VARABL(I,4)
                      ALENS(ALTYPE,INT(VARABL(I,3)))=V1
C                       THESE VARIABLES DONE
                  END IF
                  IF(VTYPE.GE.11.AND.VTYPE.LE.25.OR.VTYPE.EQ.75.OR.VTYPE.GE.
     1            124.AND.VTYPE.LE.149) THEN
                      IF(VTYPE.EQ.11) ALTYPE=41
                      IF(VTYPE.EQ.12) ALTYPE=37
                      IF(VTYPE.EQ.13) ALTYPE=38
                      IF(VTYPE.EQ.14) ALTYPE=39
                      IF(VTYPE.EQ.15) ALTYPE=40
                      IF(VTYPE.EQ.16) ALTYPE=118
                      IF(VTYPE.EQ.17) ALTYPE=119
                      IF(VTYPE.EQ.18) ALTYPE=120
                      IF(VTYPE.EQ.19) ALTYPE=114
                      IF(VTYPE.EQ.20) ALTYPE=115
                      IF(VTYPE.EQ.21) ALTYPE=46
                      IF(VTYPE.EQ.22) ALTYPE=47
                      IF(VTYPE.EQ.23) ALTYPE=48
                      IF(VTYPE.EQ.24) ALTYPE=49
                      IF(VTYPE.EQ.25) ALTYPE=50
                      IF(VTYPE.EQ.75) ALTYPE=43
                      IF(VTYPE.EQ.124) ALTYPE=71
                      IF(VTYPE.EQ.125) ALTYPE=72
                      IF(VTYPE.EQ.126) ALTYPE=73
                      IF(VTYPE.EQ.127) ALTYPE=74
                      IF(VTYPE.EQ.128) ALTYPE=75
                      IF(VTYPE.EQ.129) ALTYPE=81
                      IF(VTYPE.EQ.130) ALTYPE=82
                      IF(VTYPE.EQ.131) ALTYPE=83
                      IF(VTYPE.EQ.132) ALTYPE=84
                      IF(VTYPE.EQ.133) ALTYPE=85
                      IF(VTYPE.EQ.134) ALTYPE=116
                      IF(VTYPE.EQ.135) ALTYPE=86
                      IF(VTYPE.EQ.136) ALTYPE=87
                      IF(VTYPE.EQ.137) ALTYPE=78
                      IF(VTYPE.EQ.138) ALTYPE=79
                      IF(VTYPE.EQ.139) ALTYPE=80
                      IF(VTYPE.EQ.140) ALTYPE=89
                      IF(VTYPE.EQ.141) ALTYPE=11
                      IF(VTYPE.EQ.142) ALTYPE=10
                      IF(VTYPE.EQ.143) ALTYPE=90
                      IF(VTYPE.EQ.144) ALTYPE=91
                      IF(VTYPE.EQ.145) ALTYPE=92
                      IF(VTYPE.EQ.146) ALTYPE=93
                      IF(VTYPE.EQ.147) ALTYPE=94
                      IF(VTYPE.EQ.148) ALTYPE=95
                      IF(VTYPE.EQ.149) ALTYPE=98
C     NEW VALUE IS:
                      V1=VARABL(I,4)
                      ALENS(ALTYPE,INT(VARABL(I,3)))=V1
                  END IF
                  IF(VTYPE.EQ.150) THEN
C     NEW VALUE IS:
                      V1=VARABL(I,4)
                      GPREG(INT(VARABL(I,3)))=V1
                  END IF
                  IF(VTYPE.GE.250.AND.VTYPE.LE.4218) THEN
                      ALTYPE=VTYPE-249
C     NEW VALUE IS:
                      V1=VARABL(I,4)
C     RESET THE APPRORIATE ARRAY VALUE IN THE DEFORMABLE SURFACE
                      ISURF=INT(VARABL(I,3))
                      DEFGR1=ALENS(103,ISURF)
                      DEFGR2=ALENS(104,ISURF)
                      DEFGR3=ALENS(105,ISURF)
                      DEFGR4=ALENS(106,ISURF)
                      DEFGR5=ALENS(107,ISURF)
                      DEFGR6=0.0D0
                      DEFGR7=ALENS(109,ISURF)
                      DEFGR8=0.0D0
                      ACTNUM=ALTYPE
                      NEWDEFVAL=V1
                      ERR1=.FALSE.
                      ERR2=.FALSE.
                      CALL DEFGRIDS(6,ISURF,ERR1,ERR2)
                  END IF
                  IF(VTYPE.GE.27.AND.VTYPE.LE.74) THEN
C     SPECIAL SURFACE COEFFICIENTS
C     NEW VALUE IS:
                      V1=VARABL(I,4)
                      FTFL01((VTYPE-26),INT(VARABL(I,3)))=V1
C                       SPECIAL SURFACE COEFICIENTS DONE
                  END IF
                  IF(VTYPE.GE.76.AND.VTYPE.LE.123) THEN
C     SPECIAL SURFACE COEFFICIENTS
C     NEW VALUE IS:
                      V1=VARABL(I,4)
                      FTFL01((VTYPE-27),INT(VARABL(I,3)))=V1
C                       SPECIAL SURFACE COEFICIENTS DONE
                  END IF
C
              ELSE
C     CONFIGS VARIABLE
C     VCFG IS THE CONFIG NUMBER
                  VCFG=INT(VARABL(I,2))
C     VTYPE IS THE VARIABLE TYPE NUMBER AS USED IN THE VARIABLES ARRAYS
                  VTYPE=INT(VARABL(I,1))
C
C     FOR VARIABLE I, APPLY THE SPECIFIED CHANGE TO THE SPECIFIED
C     CONFIG
C
C     THE NEW VARAIBLE VALUE IS JUST
                  V1=VARABL(I,4)
C     CONVERT THIS VALUE TO A CHARACTER*23 CHARACTER VARIABLE
                  VADD=INT(VARABL(I,14))
                  IF(CFADD(VADD,1).EQ.1.AND.V1.NE.0.0D0) V1=1.0D0/V1
                  IF(CFADD(VADD,1).EQ.9.AND.V1.NE.0.0D0) V1=1.0D0/V1
                  CALL AUXNTA
C
C     THE POSITION IN THE CFADD,CFVAL AND CFCHAR ARRAYS WHERE THIS
C     VARIABLE IS FOUND IS:
C
                  IF(CFADD(VADD,1).GE.27.AND.CFADD(VADD,1).LE.74.OR.
     1            CFADD(VADD,1).GE.76.AND.CFADD(VADD,1).LE.123
     1            .OR.CFADD(VADD,1).EQ.141) THEN
                      CFVAL(VADD,2)=V1
                      CFCHAR(VADD,2)=AV1
C     NOW UPDATE THE CONFIG ARRAY
                      CONFG(CFADD(VADD,3),CFADD(VADD,9))(CFADD(VADD,6):CFADD(VADD,7))
     1                =AV1(1:23)
                  ELSE
                      CFVAL(VADD,1)=V1
                      CFCHAR(VADD,1)=AV1
C     NOW UPDATE THE CONFIG ARRAY
                      CONFG(CFADD(VADD,3),CFADD(VADD,9))(CFADD(VADD,4):CFADD(VADD,5))
     1                =AV1(1:23)
                  END IF
C     NOW LOOK UP WHERE THIS CHARACTER REPRESENTATION OF THE NEW VALUE
C     SHOULD BE STUFFED INTO THE CONFIG ARRAYS CONFG AND
C     STUFF IT THERE.
C
C     NOW LOOP BACK AND REPEAT FOR THE NEXT VARIABLE.
C     FINISHED WITH A CONFIG VARIABLE
              END IF
C     LOOP TO NEXT VARIABL
C     WRITE(OUTLYNE,*)'VARIABLE ',IVB
C     CALL SHOWIT(1)
          END DO
C     UPDATE THE LENS OR THE PERMANENT LENS WILL BE ******!
          CALL FIXDEFORMFILE
          F6=1
          F1=0
          F22=0
          LNSTYP=2
          CALL LNSEOS
C**********************************************************************
C     AND RE-EVALUATE THE OPERANDS AND DISPLAY
C     THE NEW FIGURE OF MERIT, THE OLD FIGURE OF MERIT  AND ITS CHANGE
          SAVE_KDP(4)=SAVEINPT(4)
          F28=1
          MSG=.FALSE.
          OPTMES=.FALSE.
          WC='FMT'
          WQ='        '
          SQ=0
          SN=0
          SST=0
          WS=' '
          DF1=1
          DF2=1
          DF3=1
          DF4=1
          DF5=1
          STI=0
          SN=0
          S1=0
          S2=0
          S3=0
          S4=0
          S5=0
          W1=0.0D0
          CALL FMT3
          SOLEXT=.TRUE.
          REST_KDP(4)=RESTINPT(4)
          DEALLOCATE(WT,W,V,BTB,BTG,STAT=ALLOERR)
          RETURN
      END
