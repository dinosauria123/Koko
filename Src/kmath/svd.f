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

C SUB SVDCMPB.FOR
C
C       SVD ROUTINE FROM NUMERICAL RECIPIES
C       USED IN PUPIL MAPPING
C
      SUBROUTINE SVDCMPB(A,W,V,NN)
C
          IMPLICIT NONE
C
          INTEGER NP,MP,M,N,I,J,K,L,ITS,NM,NN
C
          REAL*8 G,SCALE,ANORM,S
     1    ,F,H,C,Z,X,Y
C
          REAL*8 A,W,V,RV1(1:15)
          DIMENSION A(NN,NN),W(NN),V(NN,NN)
C
          INCLUDE 'datmai.inc'
C
          N=15
          M=15
          NP=15
          MP=15
          G=0.0D0
          SCALE=0.0D0
          ANORM=0.0D0
          I=0
          K=0
          J=0
          ITS=0
          NM=0
          ITS=0
          NM=0
          W(1:NP)=0.0D0
          V(1:NP,1:NP)=0.0D0
          RV1(1:NP)=0.0D0
          DO 25 I=1,N
              L=I+1
              RV1(I)=SCALE*G
              G=0.0D0
              S=0.0D0
              SCALE=0.0D0
              IF (I.LE.M) THEN
                  DO 11 K=I,M
                      SCALE=SCALE+DABS(A(K,I))
11                CONTINUE
                  IF (SCALE.NE.0.0D0) THEN
                      DO 12 K=I,M
                          A(K,I)=A(K,I)/SCALE
                          S=S+A(K,I)*A(K,I)
12                    CONTINUE
                      F=A(I,I)
                      IF(S.LT.0.0D0) S=0.0D0
                      G=-DSIGN(DSQRT(S),F)
                      H=F*G-S
                      A(I,I)=F-G
                      IF (I.NE.N) THEN
                          DO 15 J=L,N
                              S=0.0D0
                              DO 13 K=I,M
                                  S=S+A(K,I)*A(K,J)
13                            CONTINUE
                              F=S/H
                              DO 14 K=I,M
                                  A(K,J)=A(K,J)+F*A(K,I)
14                            CONTINUE
15                        CONTINUE
                      ENDIF
                      DO 16 K= I,M
                          A(K,I)=SCALE*A(K,I)
16                    CONTINUE
                  ENDIF
              ENDIF
              W(I)=SCALE *G
              G=0.0D0
              S=0.0D0
              SCALE=0.0D0
              IF ((I.LE.M).AND.(I.NE.N)) THEN
                  DO 17 K=L,N
                      SCALE=SCALE+DABS(A(I,K))
17                CONTINUE
                  IF (SCALE.NE.0.0D0) THEN
                      DO 18 K=L,N
                          A(I,K)=A(I,K)/SCALE
                          S=S+A(I,K)*A(I,K)
18                    CONTINUE
                      F=A(I,L)
                      IF(S.LT.0.0D0) S=0.0D0
                      G=-DSIGN(DSQRT(S),F)
                      H=F*G-S
                      A(I,L)=F-G
                      DO 19 K=L,N
                          RV1(K)=A(I,K)/H
19                    CONTINUE
                      IF (I.NE.M) THEN
                          DO 23 J=L,M
                              S=0.0D0
                              DO 21 K=L,N
                                  S=S+A(J,K)*A(I,K)
21                            CONTINUE
                              DO 22 K=L,N
                                  A(J,K)=A(J,K)+S*RV1(K)
22                            CONTINUE
23                        CONTINUE
                      ENDIF
                      DO 24 K=L,N
                          A(I,K)=SCALE*A(I,K)
24                    CONTINUE
                  ENDIF
              ENDIF
              ANORM=DMAX1(ANORM,(DABS(W(I))+DABS(RV1(I))))
25        CONTINUE
          DO 32 I=N,1,-1
              IF (I.LT.N) THEN
                  IF (G.NE.0.0D0) THEN
                      DO 26 J=L,N
                          V(J,I)=(A(I,J)/A(I,L))/G
26                    CONTINUE
                      DO 29 J=L,N
                          S=0.0D0
                          DO 27 K=L,N
                              S=S+A(I,K)*V(K,J)
27                        CONTINUE
                          DO 28 K=L,N
                              V(K,J)=V(K,J)+S*V(K,I)
28                        CONTINUE
29                    CONTINUE
                  ENDIF
                  DO 31 J=L,N
                      V(I,J)=0.0D0
                      V(J,I)=0.0D0
31                CONTINUE
              ENDIF
              V(I,I)=1.0D0
              G=RV1(I)
              L=I
32        CONTINUE
          DO 39 I=N,1,-1
              L=I+1
              G=W(I)
              IF (I.LT.N) THEN
                  DO 33 J=L,N
                      A(I,J)=0.0D0
33                CONTINUE
              ENDIF
              IF (G.NE.0.0D0) THEN
                  G=1.0D0/G
                  IF (I.NE.N) THEN
                      DO 36 J=L,N
                          S=0.0D0
                          DO 34 K=L,M
                              S=S+A(K,I)*A(K,J)
34                        CONTINUE
                          F=(S/A(I,I))*G
                          DO 35 K=I,M
                              A(K,J)=A(K,J)+F*A(K,I)
35                        CONTINUE
36                    CONTINUE
                  ENDIF
                  DO 37 J=I,M
                      A(J,I)=A(J,I)*G
37                CONTINUE
              ELSE
                  DO 38 J= I,M
                      A(J,I)=0.0D0
38                CONTINUE
              ENDIF
              A(I,I)=A(I,I)+1.0D0
39        CONTINUE
          DO 49 K=N,1,-1
              DO 48 ITS=1,30
                  DO 41 L=K,1,-1
                      NM=L-1
                      IF ((DABS(RV1(L))+ANORM).EQ.(ANORM))  GO TO 2
                      IF ((DABS(W(NM))+ANORM).EQ.(ANORM))  GO TO 1
41                CONTINUE
1                 C=0.0D0
                  S=1.0D0
                  DO 43 I=L,K
                      F=S*RV1(I)
                      IF ((DABS(F)+ANORM).NE.(ANORM)) THEN
                          G=W(I)
                          H=DSQRT(F*F+G*G)
                          W(I)=H
                          H=1.0D0/H
                          C= (G*H)
                          S=-(F*H)
                          DO 42 J=1,M
                              Y=A(J,NM)
                              Z=A(J,I)
                              A(J,NM)=(Y*C)+(Z*S)
                              A(J,I)=-(Y*S)+(Z*C)
42                        CONTINUE
                      ENDIF
43                CONTINUE
2                 Z=W(K)
                  IF (L.EQ.K) THEN
                      IF (Z.LT.0.0D0) THEN
                          W(K)=-Z
                          DO 44 J=1,N
                              V(J,K)=-V(J,K)
44                        CONTINUE
                      ENDIF
                      GO TO 3
                  ENDIF
                  X=W(L)
                  NM=K-1
                  Y=W(NM)
                  G=RV1(NM)
                  H=RV1(K)
                  F=((Y-Z)*(Y+Z)+(G-H)*(G+H))/(2.0D0*H*Y)
                  G=DSQRT(F*F+1.0D0)
                  F=((X-Z)*(X+Z)+H*((Y/(F+DSIGN(G,F)))-H))/X
                  C=1.0D0
                  S=1.0D0
                  DO 47 J=L,NM
                      I=J+1
                      G=RV1(I)
                      Y=W(I)
                      H=S*G
                      G=C*G
                      Z=DSQRT(F*F+H*H)
                      RV1(J)=Z
                      C=F/Z
                      S=H/Z
                      F= (X*C)+(G*S)
                      G=-(X*S)+(G*C)
                      H=Y*S
                      Y=Y*C
                      DO 45 NM=1,N
                          X=V(NM,J)
                          Z=V(NM,I)
                          V(NM,J)= (X*C)+(Z*S)
                          V(NM,I)=-(X*S)+(Z*C)
45                    CONTINUE
                      Z=DSQRT(F*F+H*H)
                      W(J)=Z
                      IF (Z.NE.0.0D0) THEN
                          Z=1.0D0/Z
                          C=F*Z
                          S=H*Z
                      ENDIF
                      F= (C*G)+(S*Y)
                      X=-(S*G)+(C*Y)
                      DO 46 NM=1,M
                          Y=A(NM,J)
                          Z=A(NM,I)
                          A(NM,J)= (Y*C)+(Z*S)
                          A(NM,I)=-(Y*S)+(Z*C)
46                    CONTINUE
47                CONTINUE
                  RV1(L)=0.0D0
                  RV1(K)=F
                  W(K)=X
48            CONTINUE
3             CONTINUE
49        CONTINUE
          RETURN
      END
C SUB SVDCMP.FOR
C
C       SVD ROUTINE FROM NUMERICAL RECIPIES
C       USED TO REMOVE TILT AND FOCUS FROM A CAPFN
C
      SUBROUTINE SVDCMP(A,M,N,MP,NP,W,V)
C
          IMPLICIT NONE
C
          INTEGER NMAX,NP,MP,M,N,I,J,K,L,ITS,NM
C
          REAL*8 A,W,V,G,SCALE,ANORM,RV1,S
     1    ,F,H,C,Z,X,Y
C
          INCLUDE 'datmai.inc'
C
          PARAMETER (NMAX=2000)
          DIMENSION A(MP,NP),W(NP),V(NP,NP),RV1(NMAX)
          G=0.0D0
          SCALE=0.0D0
          ANORM=0.0D0
          I=0
          K=0
          J=0
          ITS=0
          NM=0
          W(1:NP)=0.0D0
          V(1:NP,1:NP)=0.0D0
          RV1(1:NMAX)=0.0D0
          DO 25 I=1,N
              L=I+1
              RV1(I)=SCALE*G
              G=0.0D0
              S=0.0D0
              SCALE=0.0D0
              IF (I.LE.M) THEN
                  DO 11 K=I,M
                      SCALE=SCALE+DABS(A(K,I))
11                CONTINUE
                  IF (SCALE.NE.0.0D0) THEN
                      DO 12 K=I,M
                          A(K,I)=A(K,I)/SCALE
                          S=S+A(K,I)*A(K,I)
12                    CONTINUE
                      F=A(I,I)
                      IF(S.LT.0.0D0) S=0.0D0
                      G=-DSIGN(DSQRT(S),F)
                      H=F*G-S
                      A(I,I)=F-G
                      IF (I.NE.N) THEN
                          DO 15 J=L,N
                              S=0.0D0
                              DO 13 K=I,M
                                  S=S+A(K,I)*A(K,J)
13                            CONTINUE
                              F=S/H
                              DO 14 K=I,M
                                  A(K,J)=A(K,J)+F*A(K,I)
14                            CONTINUE
15                        CONTINUE
                      ENDIF
                      DO 16 K= I,M
                          A(K,I)=SCALE*A(K,I)
16                    CONTINUE
                  ENDIF
              ENDIF
              W(I)=SCALE *G
              G=0.0D0
              S=0.0D0
              SCALE=0.0D0
              IF ((I.LE.M).AND.(I.NE.N)) THEN
                  DO 17 K=L,N
                      SCALE=SCALE+DABS(A(I,K))
17                CONTINUE
                  IF (SCALE.NE.0.0D0) THEN
                      DO 18 K=L,N
                          A(I,K)=A(I,K)/SCALE
                          S=S+A(I,K)*A(I,K)
18                    CONTINUE
                      F=A(I,L)
                      G=-DSIGN(DSQRT(S),F)
                      H=F*G-S
                      A(I,L)=F-G
                      DO 19 K=L,N
                          RV1(K)=A(I,K)/H
19                    CONTINUE
                      IF (I.NE.M) THEN
                          DO 23 J=L,M
                              S=0.0D0
                              DO 21 K=L,N
                                  S=S+A(J,K)*A(I,K)
21                            CONTINUE
                              DO 22 K=L,N
                                  A(J,K)=A(J,K)+S*RV1(K)
22                            CONTINUE
23                        CONTINUE
                      ENDIF
                      DO 24 K=L,N
                          A(I,K)=SCALE*A(I,K)
24                    CONTINUE
                  ENDIF
              ENDIF
              ANORM=DMAX1(ANORM,(DABS(W(I))+DABS(RV1(I))))
25        CONTINUE
          DO 32 I=N,1,-1
              IF (I.LT.N) THEN
                  IF (G.NE.0.0D0) THEN
                      DO 26 J=L,N
                          V(J,I)=(A(I,J)/A(I,L))/G
26                    CONTINUE
                      DO 29 J=L,N
                          S=0.0D0
                          DO 27 K=L,N
                              S=S+A(I,K)*V(K,J)
27                        CONTINUE
                          DO 28 K=L,N
                              V(K,J)=V(K,J)+S*V(K,I)
28                        CONTINUE
29                    CONTINUE
                  ENDIF
                  DO 31 J=L,N
                      V(I,J)=0.0D0
                      V(J,I)=0.0D0
31                CONTINUE
              ENDIF
              V(I,I)=1.0D0
              G=RV1(I)
              L=I
32        CONTINUE
          DO 39 I=N,1,-1
              L=I+1
              G=W(I)
              IF (I.LT.N) THEN
                  DO 33 J=L,N
                      A(I,J)=0.0D0
33                CONTINUE
              ENDIF
              IF (G.NE.0.0D0) THEN
                  G=1.0D0/G
                  IF (I.NE.N) THEN
                      DO 36 J=L,N
                          S=0.0D0
                          DO 34 K=L,M
                              S=S+A(K,I)*A(K,J)
34                        CONTINUE
                          F=(S/A(I,I))*G
                          DO 35 K=I,M
                              A(K,J)=A(K,J)+F*A(K,I)
35                        CONTINUE
36                    CONTINUE
                  ENDIF
                  DO 37 J=I,M
                      A(J,I)=A(J,I)*G
37                CONTINUE
              ELSE
                  DO 38 J= I,M
                      A(J,I)=0.0D0
38                CONTINUE
              ENDIF
              A(I,I)=A(I,I)+1.0D0
39        CONTINUE
          DO 49 K=N,1,-1
              DO 48 ITS=1,30
                  DO 41 L=K,1,-1
                      NM=L-1
                      IF ((DABS(RV1(L))+ANORM).EQ.(ANORM))  GO TO 2
                      IF ((DABS(W(NM))+ANORM).EQ.(ANORM))  GO TO 1
41                CONTINUE
1                 C=0.0D0
                  S=1.0D0
                  DO 43 I=L,K
                      F=S*RV1(I)
                      IF ((DABS(F)+ANORM).NE.(ANORM)) THEN
                          G=W(I)
                          H=DSQRT(F*F+G*G)
                          W(I)=H
                          H=1.0D0/H
                          C= (G*H)
                          S=-(F*H)
                          DO 42 J=1,M
                              Y=A(J,NM)
                              Z=A(J,I)
                              A(J,NM)=(Y*C)+(Z*S)
                              A(J,I)=-(Y*S)+(Z*C)
42                        CONTINUE
                      ENDIF
43                CONTINUE
2                 Z=W(K)
                  IF (L.EQ.K) THEN
                      IF (Z.LT.0.0D0) THEN
                          W(K)=-Z
                          DO 44 J=1,N
                              V(J,K)=-V(J,K)
44                        CONTINUE
                      ENDIF
                      GO TO 3
                  ENDIF
                  X=W(L)
                  NM=K-1
                  Y=W(NM)
                  G=RV1(NM)
                  H=RV1(K)
                  F=((Y-Z)*(Y+Z)+(G-H)*(G+H))/(2.0D0*H*Y)
                  G=DSQRT(F*F+1.0D0)
                  F=((X-Z)*(X+Z)+H*((Y/(F+DSIGN(G,F)))-H))/X
                  C=1.0D0
                  S=1.0D0
                  DO 47 J=L,NM
                      I=J+1
                      G=RV1(I)
                      Y=W(I)
                      H=S*G
                      G=C*G
                      Z=DSQRT(F*F+H*H)
                      RV1(J)=Z
                      C=F/Z
                      S=H/Z
                      F= (X*C)+(G*S)
                      G=-(X*S)+(G*C)
                      H=Y*S
                      Y=Y*C
                      DO 45 NM=1,N
                          X=V(NM,J)
                          Z=V(NM,I)
                          V(NM,J)= (X*C)+(Z*S)
                          V(NM,I)=-(X*S)+(Z*C)
45                    CONTINUE
                      Z=DSQRT(F*F+H*H)
                      W(J)=Z
                      IF (Z.NE.0.0D0) THEN
                          Z=1.0D0/Z
                          C=F*Z
                          S=H*Z
                      ENDIF
                      F= (C*G)+(S*Y)
                      X=-(S*G)+(C*Y)
                      DO 46 NM=1,M
                          Y=A(NM,J)
                          Z=A(NM,I)
                          A(NM,J)= (Y*C)+(Z*S)
                          A(NM,I)=-(Y*S)+(Z*C)
46                    CONTINUE
47                CONTINUE
                  RV1(L)=0.0D0
                  RV1(K)=F
                  W(K)=X
48            CONTINUE
3             CONTINUE
49        CONTINUE
          RETURN
      END
C SUB SVBKSBB.FOR
C
C     SUBROUTINE FROM NUMERICAL RECIPIES FOR SVD
C       USED IN PUPIL MAPPING
C
      SUBROUTINE SVBKSBB(U,V,B,W,NN)
C
          IMPLICIT NONE
C
          INTEGER NP,MP,M,J,I,JJ,N,NN
C
          REAL*8 S
C
          REAL*8
     1     X(1:15),TMP(1:15)
C
          REAL*8 U,W,
     1    V,B
          DIMENSION U(NN,NN),W(NN),
     1    V(NN,NN),B(NN)
C
          COMMON/SVDB2/X
C
          S=0.0D0
          J=0
          I=0
          JJ=0
          NP=15
          MP=15
          N=15
          M=15
          X(1:NP)=0.0D0
          TMP(1:NP)=0.0D0
          DO 12 J=1,N
              S=0.0D0
              IF(W(J).NE.0.0D0) THEN
                  DO 11 I=1,M
                      S=S+U(I,J)*B(I)
11                CONTINUE
                  S=S/W(J)
              END IF
              TMP(J)=S
12        CONTINUE
          DO 14 J=1,N
              S=0.0D0
              DO 13 JJ=1,N
                  S=S+V(J,JJ)*TMP(JJ)
13            CONTINUE
              X(J)=S
14        CONTINUE
          RETURN
      END
C SUB SVBKSB.FOR
C
C     SUBROUTINE FROM NUMERICAL RECIPIES FOR SVD
C       USED TO REMOVE TILT AND FOCUS FROM A CAPFN
C
      SUBROUTINE SVBKSB(U,W,V,M,N,MP,NP,B,X)
C
          IMPLICIT NONE
C
          INTEGER NP,MP,NMAX,M,J,I,JJ,N
C
          REAL*8 U,W,V,B,X,S,TMP
C
          PARAMETER (NMAX=2000)
          DIMENSION U(MP,NP),W(NP),V(NP,NP),B(MP),X(NP),TMP(NMAX)
C
          S=0.0D0
          J=0
          I=0
          JJ=0
          X(1:NP)=0.0D0
          TMP(1:NMAX)=0.0D0
          DO 12 J=1,N
              S=0.0D0
              IF(W(J).NE.0.0D0) THEN
                  DO 11 I=1,M
                      S=S+U(I,J)*B(I)
11                CONTINUE
                  S=S/W(J)
              END IF
              TMP(J)=S
12        CONTINUE
          DO 14 J=1,N
              S=0.0D0
              DO 13 JJ=1,N
                  S=S+V(J,JJ)*TMP(JJ)
13            CONTINUE
              X(J)=S
14        CONTINUE
          RETURN
      END
C SUB SVBKSBA.FOR
C
C     SUBROUTINE FROM NUMERICAL RECIPIES FOR SVD
C
      SUBROUTINE SVBKSBA(W,V,VN,VN1,BTB,BTG)
C
          IMPLICIT NONE
C
          INTEGER NP2,MP,NMAX,SM,J,I,JJ,ALLOERR,SSN,VN,VN1
C
          REAL*8 W,BTB,BTG
     1    ,V,X(1:100000),S,TMP
          DIMENSION W(VN),V(VN,VN),BTB(VN1,VN1),BTG(VN1),TMP(:)
C
          ALLOCATABLE :: TMP
C
          COMMON/SVD1/SM,SSN,MP,NP2
C
          COMMON/SVD2/X
C
          NMAX=VN
          DEALLOCATE(TMP,STAT=ALLOERR)
          ALLOCATE(TMP(VN),STAT=ALLOERR)
          TMP(1:VN)=0.0D0
C
          S=0.0D0
          J=0
          I=0
          JJ=0
          DO 12 J=1,SSN
              IF(W(J).NE.0.0D0) THEN
                  S=0.0D0
                  DO 11 I=1,SM
                      S=S+BTB(I,J)*BTG(I)
11                CONTINUE
                  S=S/W(J)
              END IF
              TMP(J)=S
12        CONTINUE
          DO 14 J=1,SSN
              S=0.0D0
              DO 13 JJ=1,SSN
                  S=S+V(J,JJ)*TMP(JJ)
13            CONTINUE
              X(J)=S
14        CONTINUE
          DEALLOCATE(TMP,STAT=ALLOERR)
          RETURN
      END
C SUB SVDCMPA.FOR
C
C       SVD ROUTINE FROM NUMERICAL RECIPIES
C
      SUBROUTINE SVDCMPA(W,V,VN,BTB)
C
          IMPLICIT NONE

          DIMENSION W(VN),V(VN,VN),BTB(VN,VN),RV1(:)
C
          INTEGER NP2,MP,SM,SSN,VN,I,J,ALLOERR,K,L,ITS,NM
C
          REAL*8 W,BTB,PYTHAG,V,G,SCALE1,ANORM,S,RV1
     1    ,F,H,C,Z,X,Y

          EXTERNAL PYTHAG


C
          ALLOCATABLE :: RV1
C
          COMMON/SVD1/SM,SSN,MP,NP2
C
          INCLUDE 'datmai.inc'
C
          DEALLOCATE(RV1,STAT=ALLOERR)
          ALLOCATE(RV1(VN),STAT=ALLOERR)
          RV1(1:VN)=0.0D0
C
          G=0.0D0
          SCALE1=0.0D0
          ANORM=0.0D0
          I=0
          K=0
          J=0
          ITS=0
          NM=0
          DO 25 I=1,SSN
              L=I+1
              RV1(I)=SCALE1*G
              G=0.0D0
              S=0.0D0
              SCALE1=0.0D0
              IF (I.LE.SM) THEN
                  DO 11 K=I,SM
                      SCALE1=SCALE1+DABS(BTB(K,I))
11                CONTINUE
                  IF (SCALE1.NE.0.0D0) THEN
                      DO 12 K=I,SM
                          BTB(K,I)=BTB(K,I)/SCALE1
                          S=S+BTB(K,I)*BTB(K,I)
12                    CONTINUE
                      F=BTB(I,I)
                      IF(S.LT.0.0D0) S=0.0D0
                      G=-DSIGN(DSQRT(S),F)
                      H=F*G-S
                      BTB(I,I)=F-G
                      DO 15 J=L,SSN
                          S=0.0D0
                          DO 13 K=I,SM
                              S=S+BTB(K,I)*BTB(K,J)
13                        CONTINUE
                          F=S/H
                          DO 14 K=I,SM
                              BTB(K,J)=BTB(K,J)+F*BTB(K,I)
14                        CONTINUE
15                    CONTINUE
                      DO 16 K= I,SM
                          BTB(K,I)=SCALE1*BTB(K,I)
16                    CONTINUE
                  ENDIF
              ENDIF
              W(I)=SCALE1*G
              G=0.0D0
              S=0.0D0
              SCALE1=0.0D0
              IF ((I.LE.SM).AND.(I.NE.SSN)) THEN
                  DO 17 K=L,SSN
                      SCALE1=SCALE1+DABS(BTB(I,K))
17                CONTINUE
                  IF (SCALE1.NE.0.0D0) THEN
                      DO 18 K=L,SSN
                          BTB(I,K)=BTB(I,K)/SCALE1
                          S=S+BTB(I,K)*BTB(I,K)
18                    CONTINUE
                      F=BTB(I,L)
                      IF(S.LT.0.0D0) S=0.0D0
                      G=-DSIGN(DSQRT(S),F)
                      H=F*G-S
                      BTB(I,L)=F-G
                      DO 19 K=L,SSN
                          RV1(K)=BTB(I,K)/H
19                    CONTINUE
                      DO 23 J=L,SM
                          S=0.0D0
                          DO 21 K=L,SSN
                              S=S+BTB(J,K)*BTB(I,K)
21                        CONTINUE
                          DO 22 K=L,SSN
                              BTB(J,K)=BTB(J,K)+S*RV1(K)
22                        CONTINUE
23                    CONTINUE
                      DO 24 K=L,SSN
                          BTB(I,K)=SCALE1*BTB(I,K)
24                    CONTINUE
                  ENDIF
              ENDIF
              ANORM=DMAX1(ANORM,(DABS(W(I))+DABS(RV1(I))))
25        CONTINUE
          DO 32 I=SSN,1,-1
              IF (I.LT.SSN) THEN
                  IF (G.NE.0.0D0) THEN
                      DO 26 J=L,SSN
                          V(J,I)=(BTB(I,J)/BTB(I,L))/G
26                    CONTINUE
                      DO 29 J=L,SSN
                          S=0.0D0
                          DO 27 K=L,SSN
                              S=S+BTB(I,K)*V(K,J)
27                        CONTINUE
                          DO 28 K=L,SSN
                              V(K,J)=V(K,J)+S*V(K,I)
28                        CONTINUE
29                    CONTINUE
                  ENDIF
                  DO 31 J=L,SSN
                      V(I,J)=0.0D0
                      V(J,I)=0.0D0
31                CONTINUE
              ENDIF
              V(I,I)=1.0D0
              G=RV1(I)
              L=I
32        CONTINUE
          DO 39 I=MIN(SM,SSN),1,-1
              L=I+1
              G=W(I)
              DO 33 J=L,SSN
                  BTB(I,J)=0.0D0
33            CONTINUE
              IF (G.NE.0.0D0) THEN
                  G=1.0D0/G
                  DO 36 J=L,SSN
                      S=0.0D0
                      DO 34 K=L,SM
                          S=S+BTB(K,I)*BTB(K,J)
34                    CONTINUE
                      F=(S/BTB(I,I))*G
                      DO 35 K=I,SM
                          BTB(K,J)=BTB(K,J)+F*BTB(K,I)
35                    CONTINUE
36                CONTINUE
                  DO 37 J=I,SM
                      BTB(J,I)=BTB(J,I)*G
37                CONTINUE
              ELSE
                  DO 38 J= I,SM
                      BTB(J,I)=0.0D0
38                CONTINUE
              ENDIF
              BTB(I,I)=BTB(I,I)+1.0D0
39        CONTINUE
          DO 49 K=SSN,1,-1
              DO 48 ITS=1,30
                  DO 41 L=K,1,-1
                      NM=L-1
                      IF ((DABS(RV1(L))+ANORM).EQ.(ANORM))  GO TO 2
                      IF ((DABS(W(NM))+ANORM).EQ.(ANORM))  GO TO 1
41                CONTINUE
1                 C=0.0D0
                  S=1.0D0
                  DO 43 I=L,K
                      F=S*RV1(I)
                      RV1(I)=C*RV1(I)
                      IF ((DABS(F)+ANORM).NE.(ANORM)) GO TO 2
                      G=W(I)
                      H=PYTHAG(F,G)
                      W(I)=H
                      H=1.0D0/H
                      C= (G*H)
                      S=-(F*H)
                      DO 42 J=1,SM
                          Y=BTB(J,NM)
                          Z=BTB(J,I)
                          BTB(J,NM)=(Y*C)+(Z*S)
                          BTB(J,I)=-(Y*S)+(Z*C)
42                    CONTINUE
43                CONTINUE
2                 Z=W(K)
                  IF (L.EQ.K) THEN
                      IF (Z.LT.0.0D0) THEN
                          W(K)=-Z
                          DO 44 J=1,SSN
                              V(J,K)=-V(J,K)
44                        CONTINUE
                      ENDIF
                      GO TO 3
                  ENDIF
C
                  IF(ITS.EQ.30) THEN
                      WRITE(OUTLYNE,*) 'NO CONVERGENCE IN SVDCMPA'
                      CALL SHOWIT(1)
                      CALL MACPAUSE
                  END IF
C
                  X=W(L)
                  NM=K-1
                  Y=W(NM)
                  G=RV1(NM)
                  H=RV1(K)
                  F=((Y-Z)*(Y+Z)+(G-H)*(G+H))/(2.0D0*H*Y)
                  G=PYTHAG(F,1.0D0)
                  F=((X-Z)*(X+Z)+H*((Y/(F+DSIGN(G,F)))-H))/X
                  C=1.0D0
                  S=1.0D0
                  DO 47 J=L,NM
                      I=J+1
                      G=RV1(I)
                      Y=W(I)
                      H=S*G
                      G=C*G
                      Z=PYTHAG(F,H)
                      RV1(J)=Z
                      C=F/Z
                      S=H/Z
                      F= (X*C)+(G*S)
                      G=-(X*S)+(G*C)
                      H=Y*S
                      Y=Y*C
                      DO 45 NM=1,SSN
                          X=V(NM,J)
                          Z=V(NM,I)
                          V(NM,J)= (X*C)+(Z*S)
                          V(NM,I)=-(X*S)+(Z*C)
45                    CONTINUE
                      Z=PYTHAG(F,H)
                      W(J)=Z
                      IF (Z.NE.0.0D0) THEN
                          Z=1.0D0/Z
                          C=F*Z
                          S=H*Z
                      ENDIF
                      F= (C*G)+(S*Y)
                      X=-(S*G)+(C*Y)
                      DO 46 NM=1,SM
                          Y=BTB(NM,J)
                          Z=BTB(NM,I)
                          BTB(NM,J)= (Y*C)+(Z*S)
                          BTB(NM,I)=-(Y*S)+(Z*C)
46                    CONTINUE
47                CONTINUE
                  RV1(L)=0.0D0
                  RV1(K)=F
                  W(K)=X
48            CONTINUE
3             CONTINUE
49        CONTINUE
          DEALLOCATE(RV1,STAT=ALLOERR)
          RETURN
      END



      FUNCTION PYTHAG(A,B)
          REAL*8 A,B,PYTHAG,ABSA,ABSB
          ABSA=DABS(A)
          ABSB=DABS(B)
          IF(ABSA.GT.ABSB) THEN
              PYTHAG=ABSA*DSQRT(1.0D0+(ABSB/ABSA)**2)
          ELSE
              IF(ABSB.EQ.0.0D0) THEN
                  PYTHAG=0.0D0
              ELSE
                  PYTHAG=ABSB*DSQRT(1.0D0+(ABSA/ABSB)**2)
              END IF
          END IF
          RETURN
      END
