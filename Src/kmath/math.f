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



      SUBROUTINE DARRAY_sort(n,arr)
c     sorts a 2-dimensional real*8 array by the value stored
c     in the first column
          INTEGER n,M,NSTACK
          REAL*8 arr(n,1:2)
          PARAMETER (M=7,NSTACK=50)
          INTEGER i,ir,j,jstack,k,l,istack(NSTACK)
          REAL*8 a,temp,b,temp2
          INCLUDE "datmai.inc"

          jstack=0
          l=1
          ir=n
1         if(ir-l.lt.M)then
              do 12 j=l+1,ir
                  a=arr(j,1)
                  b=arr(j,2)
                  do 11 i=j-1,1,-1
                      if(arr(i,1).le.a)goto 2
                      arr(i+1,1)=arr(i,1)
                      arr(i+1,2)=arr(i,2)
11                continue
                  i=0
2                 arr(i+1,1)=a
                  arr(i+1,2)=b
12            continue
              if(jstack.eq.0)return
              ir=istack(jstack)
              l=istack(jstack-1)
              jstack=jstack-2
          else
              k=(l+ir)/2
              temp=arr(k,1)
              temp2=arr(k,2)
              arr(k,1)=arr(l+1,1)
              arr(k,2)=arr(l+1,2)
              arr(l+1,1)=temp
              arr(l+1,2)=temp2
              if(arr(l+1,1).gt.arr(ir,1))then
                  temp=arr(l+1,1)
                  temp2=arr(l+1,2)
                  arr(l+1,1)=arr(ir,1)
                  arr(l+1,2)=arr(ir,2)
                  arr(ir,1)=temp
                  arr(ir,2)=temp2
              endif
              if(arr(l,1).gt.arr(ir,1))then
                  temp=arr(l,1)
                  temp2=arr(l,2)
                  arr(l,1)=arr(ir,1)
                  arr(l,2)=arr(ir,2)
                  arr(ir,1)=temp
                  arr(ir,2)=temp2
              endif
              if(arr(l+1,1).gt.arr(l,1))then
                  temp=arr(l+1,1)
                  temp2=arr(l+1,2)
                  arr(l+1,1)=arr(l,1)
                  arr(l+1,2)=arr(l,2)
                  arr(l,1)=temp
                  arr(l,2)=temp2
              endif
              i=l+1
              j=ir
              a=arr(l,1)
              b=arr(l,2)
3             continue
              i=i+1
              if(arr(i,1).lt.a)goto 3
4             continue
              j=j-1
              if(arr(j,1).gt.a)goto 4
              if(j.lt.i)goto 5
              temp=arr(i,1)
              temp2=arr(i,2)
              arr(i,1)=arr(j,1)
              arr(i,2)=arr(j,2)
              arr(j,1)=temp
              arr(j,2)=temp2
              goto 3
5             arr(l,1)=arr(j,1)
              arr(l,2)=arr(j,2)
              arr(j,1)=a
              arr(j,2)=b
              jstack=jstack+2

              if(jstack.gt.NSTACK)THEN
                  WRITE(OUTLYNE,*) 'NSTACK too small in sort'
              END IF

              if(ir-i+1.ge.j-l)then
                  istack(jstack)=ir
                  istack(jstack-1)=i
                  ir=j-1
              else
                  istack(jstack)=j-1
                  istack(jstack-1)=l
                  l=i
              endif
          endif
          goto 1
      END



      SUBROUTINE SHUFFLE(N,ARR)
C     THIS IS USED TO SET UP A RANDOMLY SORTED LIST OF N
C     DOUBLE PRECISION REPRESENTATIONS OF N INTEGER VALUES
C     RANGING FROM 1 TO N. iT WAS ADDED ON 3/13/2006 TO SUPPORT
C     A NEW WAY TO DO ITER POWELL.
          IMPLICIT NONE
          INTEGER n,i
          REAL*8 RANDMM
          EXTERNAL RANDMM
          REAL*8 arr(n,1:2)
          INCLUDE 'datmai.inc'
C     LOAD ARR(I,2) WITH DOUBLE PRECISION REPRESENTATIONS
C     OF I
C     AND LOAD RANDOM DOUBLE PRECISION NUMBERS INTO
C     ARR(I,1)
          DO I=1,N
              ARR(I,2)=DBLE(I)
              ARR(I,1)=RANDMM()
          END DO
C     NOW SORT ARRAY ARR BY THE VALUES IN ARR(I,2)
          CALL DARRAY_SORT(N,ARR)
          DO I=1,N
C     WRITE(OUTLYNE,*) I,ARR(I,1),ARR(I,2)
C     CALL SHOWIT(1)
          END DO
      END
C SUB RANDSET.FOR
C
      SUBROUTINE RANDSET
C
          IMPLICIT NONE
          INTEGER N
C
          REAL RANRAN,RESLT
C
          EXTERNAL RANRAN
C
          N=-1
          RESLT=RANRAN(-1)
C
          RETURN
      END
C SUB MYNEWSEED.FOR
C
      SUBROUTINE MYNEWSEED
C
          IMPLICIT NONE

          INCLUDE 'datmai.inc'
C
          REAL*8 D,DD
          INTEGER ID
          REAL RESLT
          REAL MSEED
          COMMON/SEEDER/MSEED
C
          IF(STI.EQ.1) THEN
              OUTLYNE= '"SEED" SETS THE RANDOM NUMBER SEED TO NUMERIC WORD #1'
              CALL SHOWIT(1)
              RESLT=(MSEED-1618033.)/1000.
              IF(ABS(RESLT).LE.0.0001) THEN
                  WRITE(OUTLYNE,20)
                  CALL SHOWIT(1)
 20               FORMAT('RANDOM NUMBER GENERATOR SEED IS THE PROGRAM DEFAULT')
              ELSE
                  WRITE(OUTLYNE,10) RESLT
                  CALL SHOWIT(1)
              END IF
 10           FORMAT('THE CURRENT USER-SPECIFIED SEED = ',E15.7)
              RETURN
          END IF
          IF(SQ.EQ.1.OR.SST.EQ.1.OR.S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1
     1    .OR.S5.EQ.1) THEN
              OUTLYNE= '"SEED" ONLY TAKES NUMERIC WORD  #1 INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(DF1.EQ.1) THEN
              OUTLYNE= '"SEED" REQUIRES EXPLICIT NUMERIC WORD  #1 INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(W1.LT.0.001D0.OR.W1.GT.1.0D0) THEN
              OUTLYNE=
     1        '"SEED" INPUT MUST NOT BE LESS THAN 0.001'
              CALL SHOWIT(1)
              OUTLYNE=
     1        '"AND NOT GREATER THAN 1.0'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          D=W1*100000.0D0
          ID=INT(D)
          DD=D-DBLE(ID)
          D=(W1*100000.0D0)-DD
C
          MSEED=(1618033.)+SNGL(D)
C
          RETURN
      END
C SUB RANDGET.FOR
C
      SUBROUTINE RANDGET(RESULT)
C
          IMPLICIT NONE
C
          REAL*8 RESULT
          INTEGER N
C
          REAL RANRAN
C
          EXTERNAL RANRAN
C
          N=1
          RESULT=DBLE(RANRAN(1))
C
          RETURN
      END
      FUNCTION RANRAN(MY_IDUM)
          IMPLICIT NONE
          INTEGER MY_IDUM,IDUM
          REAL MBIG,MZ
          REAL RANRAN,FAC
          REAL MSEED
          COMMON/SEEDER/MSEED
          PARAMETER (MBIG=4000000.,MZ=0.,FAC=1./MBIG)
          INTEGER I,IFF,II,INEXT,INEXTP,K
          REAL MJ,MK,MA(55)
          SAVE IFF,INEXT,INEXTP,MA
          DATA IFF /0/
          INCLUDE 'datmai.inc'
          IDUM=MY_IDUM
          IF(IDUM.LT.0.OR.IFF.EQ.0) THEN
              IFF=1
              MJ=MSEED-IABS(IDUM)
              MJ=MOD(MJ,MBIG)
              MA(55)=MJ
              MK=1
              DO I=1,54
                  II=MOD(21*I,55)
                  MA(II)=MK
                  MK=MJ-MK
                  IF(MK.LT.MZ) MK=MK+MBIG
                  MJ=MA(II)
              END DO
              DO K=1,4
                  DO I=1,55
                      MA(I)=MA(I)-MA(1+MOD(I+30,55))
                      IF(MA(I).LT.MZ) MA(I)=MA(I)+MBIG
                  END DO
              END DO
              INEXT=0
              INEXTP=31
              IDUM=1
              RANRAN=0.0
              RETURN
          ELSE
          END IF
          INEXT=INEXT+1
          IF(INEXT.EQ.56) INEXT=1
          INEXTP=INEXTP+1
          IF(INEXTP.EQ.56) INEXTP=1
          MJ=MA(INEXT)-MA(INEXTP)
          IF(MJ.LT.MZ) MJ=MJ+MBIG
          MA(INEXT)=MJ
          RANRAN=MJ*FAC
          RETURN
      END
      FUNCTION GASDEV()
C     RETURNS A ZERO MEAN AND A UNIT VALUE FOR THE ONE-SIGMA VARIANCE
          IMPLICIT NONE
          REAL GASDEV
          REAL*8 RESLT
          INTEGER ISET
          REAL FAC,GSET,RSQ,V1,V2
          SAVE ISET,GSET
          DATA ISET/0/
          IF(ISET.EQ.0) THEN
 1            CALL RANDGET(RESLT)
              V1=2.*REAL(RESLT)-1.
              CALL RANDGET(RESLT)
              V2=2.*REAL(RESLT)-1.
              RSQ=(V1**2)+(V2**2)
              IF(RSQ.GE.1..OR.RSQ.EQ.0) GO TO 1
              FAC=SQRT(-2*LOG(RSQ)/RSQ)
              GSET=V1*FAC
              GASDEV=V2*FAC
              ISET=1
          ELSE
              GASDEV=GSET
              ISET=0
          END IF
          RETURN
      END
C SUB RANDMM.FOR
      FUNCTION RANDMM()
          IMPLICIT NONE
          REAL*8 RANDMM
          REAL*8 RANDX
          REAL GASDEV
          INTEGER MY_IDUM
          EXTERNAL GASDEV
          INCLUDE 'datmai.inc'
          MY_IDUM=-1
          IF(WQ.EQ.' '.OR.WQ.EQ.'ACC'.OR.WQ.EQ.'X')
     1    MY_IDUM=1
          CALL RANDGET(RANDX)
          RANDMM=RANDX
          REG(40)=REG(9)
C     PUT NORMAL RANDOM NUMBER IN REG(10)
          REG(10)=GASDEV()
          RETURN
      END
