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

C
C     GENERAL MATH ROUTINES STORED HERE
C
      SUBROUTINE SORT_JK(N,RA,ARA)
          IMPLICIT NONE
          REAL*8 RA,RRA
          CHARACTER ARRA1*13,ARRA2*13,ARA*13
          INTEGER N,L,IR,J,I
          DIMENSION RA(N),ARA(2,N)
          L=N/2+1
          IR=N
 10       CONTINUE
          IF(L.GT.1) THEN
              L=L-1
              RRA=RA(L)
              ARRA1=ARA(1,L)
              ARRA2=ARA(2,L)
          ELSE
              RRA=RA(IR)
              ARRA1=ARA(1,IR)
              ARRA2=ARA(2,IR)
              RA(IR)=RA(1)
              ARA(1,IR)=ARA(1,1)
              ARA(2,IR)=ARA(2,1)
              IR=IR-1
              IF(IR.EQ.1) THEN
                  RA(1)=RRA
                  ARA(1,1)=ARRA1
                  ARA(2,1)=ARRA2
                  RETURN
              END IF
          END IF
          I=L
          J=L+L
 20       IF(J.LE.IR) THEN
              IF(J.LT.IR) THEN
                  IF(RA(J).LT.RA(J+1)) J=J+1
              END IF
              IF(RRA.LT.RA(J)) THEN
                  RA(I)=RA(J)
                  ARA(1,I)=ARA(1,J)
                  ARA(2,I)=ARA(2,J)
                  I=J
                  J=J+1
              ELSE
                  J=IR+1
              END IF
              GO TO 20
          END IF
          RA(I)=RRA
          ARA(1,I)=ARRA1
          ARA(2,I)=ARRA2
          GO TO 10
      END


      SUBROUTINE SHUFFLE(N,ARR)
C     THIS IS USED TO SET UP A RANDOMLY SORTED LIST OF N
C     DOUBLE PRECISION REPRESENTATIONS OF N INTEGER VALUES
C     RANGING FROM 1 TO N. iT WAS ADDED ON 3/13/2006 TO SUPPORT
C     A NEW WAY TO DO ITER POWELL.
          IMPLICIT NONE
          INTEGER n,i,ier
          REAL*8 RANDMM
          EXTERNAL RANDMM
          REAL*8 ARR(n,1:2)
          INCLUDE 'datmai.inc'
          
C     LOAD ARR(I,2) WITH DOUBLE PRECISION REPRESENTATIONS
C     OF I AND LOAD RANDOM DOUBLE PRECISION NUMBERS INTO
C     ARR(I,1)
          DO I=1,N
              ARR(I,2)=DBLE(I)
              ARR(I,1)=RANDMM()
           END DO
           
C     NOW SORT ARRAY ARR BY THE VALUES IN ARR(I,1)
          call sortdmat(ARR,N,2,1,ier)
          if (ier > 0) then
             outlyne = 'Error (shuffle): failed call to sortdmat'
             call showit(1)
             call macfal
          end if
      END


      SUBROUTINE RANDSET
          IMPLICIT NONE

          call random_seed() ! new sequence for Fortran RNG

          RETURN
      END      

      
C     SUB RANDGET.FOR
      SUBROUTINE RANDGET(RESULT)

          IMPLICIT NONE
          REAL*8 RESULT

          call random_number(RESULT) ! call Fortran RNG
          
          RETURN
      END
      
      
      FUNCTION GASDEV()
C     RETURNS A ZERO MEAN AND A UNIT VALUE FOR THE ONE-SIGMA VARIANCE

          USE RANDOM          
          REAL*8 GASDEV

          GASDEV = drand_normal()

          RETURN
      END

C     SUB RANDMM.FOR
      FUNCTION RANDMM()
          IMPLICIT NONE
          REAL*8 RANDMM
          REAL*8 RANDX
          REAL*8 GASDEV

          INCLUDE 'datmai.inc'

          CALL RANDGET(RANDX)
          RANDMM = RANDX
          
          REG(40) = REG(9)
          REG(10) = GASDEV() ! normal random # in reg(10)
          RETURN
      END


      SUBROUTINE CROSS_PRODUCT(XP,YP,ZP,X1,Y1,Z1,X2,Y2,Z2)
          IMPLICIT NONE
          DOUBLE PRECISION X1,Y1,Z1,X2,Y2,Z2,XP,YP,ZP
          XP=(Y1*Z2)-(Z1*Y2)
          YP=(Z1*X2)-(X1*Z2)
          ZP=(X1*Y2)-(Y1*X2)
          RETURN
      END

      
      SUBROUTINE DOT_PRODUCT(DP,X1,Y1,Z1,X2,Y2,Z2)
          IMPLICIT NONE
          DOUBLE PRECISION X1,Y1,Z1,X2,Y2,Z2,DP
          DP=(X1*X2)+(Y1*Y2)+(Z1*Z2)
          RETURN
      END
      
