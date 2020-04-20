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

C       SECOND SET OF PARAXIAL ROUTINES GO HERE

C SUB AB357.FOR
      SUBROUTINE AB357
C
C       NOTE: THIS SUBROUTINE WILL REPLACE SA3CAL.FOR
C       AND ADD 5TH ORDER ABERRATION CALCULATIONS AS WELL AS
C       SA7. THESE EQUATIONS HAVE BEEN MODIFIED TO PROPERLY
C       CALCULATE ABERRATIONS USING FLAT SURFACES WITH A
C       SECOND ORDER TERM (SCHMIDT CORRECTOR PLATES ETC.)
C
C       THIS SUBROUTINE CALCULATES THE 3RD AND 5TH ORDER
C       SIDEL ABERRATION COEFICIENTS (AND SA7) AND LOADS THEM
C       INTO MAB3(1:11,0:MAXSURF) AND MAB57(1:20,0:MAXSURF)
C       THE SURFACE SUMS ARE CALCULATED ON THE FLY
C       WHEN A PARTICULAR PRINT OUT IS REQUESTED USING
C       THE CMD LEVEL COMMANDS MAB3,MAB5 OR MABX5.
C       LENS MODE IS TAKEN INTO ACCOUNT
C       MAB3(1:5,1:MAXSURF) CONTAINS THE MARGINAL COEFFICIENTS
C       MAB3(6:10,0:MAXSURF) CONTAINS THE CHIEF RAY COEFFICIENTS
C       CHIEF RAY COEFFICIENTS ARE CALCULATED ONLY FOR
C       THIRD ORDER ABERRATIONS. THIRD ORDER ABERRATIONS CONSIST
C       OF A SPHERICAL PART AND A PART CONTRIBUTED BY THE CONIC
C       CONSTANT AND THE 4TH ORDER ASPHERIC TERM.
C
C       THE SURFACE CONTRIBUTIONS OF 5TH AND 7TH ORDER ARE KEPT
C       SEPARATELY IN ARRAY SAB57 AND PASSED VIA COMMON/SAB
C
C       THIS IS DONE FOR THE
C       (Y-Z PLANE) AND XZ PLANE
C
C       ALGORITHM DESCRIBED IN LENSSTORE.DOC
C       AND IN MATTHEW RIMMER'S THESIS U OF R 1963
C
          IMPLICIT NONE
C
          INTEGER IAB,CW,SF,I
C
          COMMON/PAS357/CW,IAB
C
          REAL*8 INV,SI,SIBAR,C1,C1BAR,K,N
     1    ,J_NP,TRAN1,TRAN2,TRAN4,TRAN5,TRAN6
C
          REAL*8 C2,C3,C21,C22,CF16,
     1    C31,C32,C33,C34,B,AB,F,AF,C,AC,E,AE,PIE,APIE,PIECV,
     2    C2BAR,BBAR,FBAR,CBAR,EBAR,WI,ABBAR,AFBAR,CV,CC,
     3    ACBAR,AEBAR,X73,X74,X75,X76,X77,X78,X42,X82,XBAR42,XBAR82,
     4    S1P2,S2P,S3P2,S4P2,S5P,S6P2,S1Q2,L,LBAR,M,A2S1P,AS2P,A2S3P,
     5    A2S4P,AS5P,A2S6P,AS1Q,JO,JOBAR,ALPHA,BETA,GAMMA,LAMBDA,MU,NU,
     6    A2S1PU,AS2PU,A2S3PU,A2S4PU,AS5PU,A2S6PU,A2S1QU,SLH,ASLH
C
          REAL*8 B71,B72,GAMMA1,GAMMA2,GAMMA3,G32,G33,G34,
     1    G36,G37,L3,D3,D32,D33,D34,D35,D36,D37,D38,D39,D40,D41,D31,
     2    BPRIME,FPRIME,CPRIME,EPRIME,PPRIME,BBPRIM,FBPRIM,CBPRIM,
     3    EBPRIM,PBPRIM,TRAN,B5PRIM,EB5PRM,G31
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
          IF(IAB.EQ.1) THEN
C
C       THE CONTROL WAVELENGTH NUMBER IS STORED IN
C       SYSTEM1(11)
              IF(CW.GE.1.AND.CW.LE.5)
     1        CW= CW+45
              IF(CW.GE.6.AND.CW.LE.10)
     1        CW= CW+65
C
C       REFRACTIVE INDICES ARE IN ALENS(46,SURF) TO
C       ALENS(50,SURF)
C
C       INDICES ARE ADDRESSED IN THE FOLLOWING MANNER
C       INDEX AT:
C                CW IS ALENS(CW,I)
C       WHERE I IS THE SURFACE NUMBER
C
C       CALCULATE THE OPTICAL INVARIANT
C
              SF=INT(SYSTEM1(20))
              INV=-((PXTRAY(5,SF)*ALENS(CW,(SF-1))*PXTRAY(2,(SF-1)))-
     1        (PXTRAY(1,SF)*ALENS(CW,(SF-1))*PXTRAY(6,(SF-1))))
              IF(DABS(INV).LE.1.0D-10) THEN
                  CALL MACFAL
                  RETURN
              END IF
C
              DO 10 I=0,INT(SYSTEM1(20))
                  IF(ALENS(1,I).EQ.0.0D0.AND.ALENS(43,I).NE.0.0D0) THEN
                      CV=ALENS(43,I)/2.0D0
                      CC=-1.0D0
                  ELSE
                      CV=ALENS(1,I)
                      CC=ALENS(2,I)

!        write(6,*) 'CV,CC==',CV,CC,I
                  END IF
C
C       AT THE OBJECT ALL IS 0.0D0
                  IF(I.EQ.0) THEN
                      GO TO 10
                  ELSE
C       I NOT OBJECT, PROCEED
                  END IF
C       CALCULATE BASIC CONSTANTS SI,SIBAR,
C       C1I AND C1IBAR
C
                  N=ALENS(CW,(I-1))
                  J_NP=ALENS(CW,I)
                  K=N/J_NP

                  SI=N*(K-1.0D0)*PXTRAY(1,I)*(PXTRAY(3,I)+PXTRAY(2,I))
                  SIBAR=N*(K-1.0D0)*PXTRAY(5,I)*(PXTRAY(7,I)+PXTRAY(6,I))
C
C       CALCULATE C1,C2,C3,C1BAR AND C2BAR FOR ASPHERIC TERMS
C
C       CHECK FOR X-TORICS

                  IF(ALENS(23,I).NE.2.0D0) THEN
C       SURFACE IS NOT X-TORIC
                      C1=(8.0D0*ALENS(4,I))+(CC*(CV**3))
                      C21=((CV**3)*CC*(CC
     1                +2.0D0))-(2.0D0*C1)
                      C22=(0.25D0*(CV**2)*C21)+(4.0D0*ALENS(5,I))
                      C2=3.0D0*C22
                      CF16=16.0D0*ALENS(6,I)
                      C31=(CC**2)+(3.0D0*CC)+3.0D0
                      C32=((CV**3)*CC*C31)-(3.0D0*C1)
                      C33=(5.0D0*(CV**2)*C32)-(12.0D0*C2)
                      C34=(-6.0D0*CV*(C1**2))+((CV**2)*C33)
                      C3=CF16+(0.125D0*C34)
                  ELSE
C       SURFACE IS X-TORIC
                      C1=(8.0D0*((ALENS(37,I)**2)*ALENS(4,I))+
     1                (ALENS(41,I)*(ALENS(24,I)**3)))
                      C21=((ALENS(24,I)**3)*ALENS(41,I)*(ALENS(41,I)
     1                +2.0D0))-(2.0D0*C1)
                      C22=(0.25D0*(ALENS(24,I)**2)*C21)+(4.0D0*(ALENS(5,I)*
     1                (ALENS(38,I)**3)))
                      C2=3.0D0*C22
                      CF16=16.0D0*ALENS(6,I)*(ALENS(39,I)**4)
                      C31=(ALENS(41,I)**2)+(3.0D0*ALENS(41,I))+3.0D0
                      C32=((ALENS(24,I)**3)*ALENS(41,I)*C31)-(3.0D0*C1)
                      C33=(5.0D0*(ALENS(24,I)**2)*C32)-(12.0D0*C2)
                      C34=(-6.0D0*ALENS(24,I)*(C1**2))+((ALENS(24,I)**2)*C33)
                      C3=CF16+(0.125D0*C34)
                  END IF
                  C1BAR=C1*(N-J_NP)
                  C2BAR=C2*(N-J_NP)
C
C       NOW C1,C2,C3 C1BAR AND C2BAR ARE DONE
C
C       NOW: THIRD ORDER ABERRATIONS INCLUDING ASPHERIC TERM
C       THIRD ORDER SPHERICAL (SA3) SURFACE
C       CONTRIBUTION (SURFACE I)
C
                  B=(SI*(PXTRAY(3,I)**2))
                  AB=(C1BAR*(PXTRAY(1,I)**4))
                  MAB3(1,I)=B+AB

                  IF(DABS(MAB3(1,I)).LT.1.0D-15) MAB3(1,I)=0.0D0
C
C       THIRD ORDER COMMA (CMA3) SURFACE
C       CONTRIBUTION (SURFACE I)
C
                  F=(SI*PXTRAY(3,I)*PXTRAY(7,I))
                  AF=(C1BAR*((PXTRAY(1,I)**3)*PXTRAY(5,I)))
                  MAB3(2,I)=F+AF
                  IF(DABS(MAB3(2,I)).LT.1.0D-15) MAB3(2,I)=0.0D0
C
C       THIRD ORDER ASTIGMATISM (AST3) SURFACE CONTRIBUTION
C       AT SURFACE I
C
                  C=SI*((PXTRAY(7,I))**2)
                  AC=(C1BAR*((PXTRAY(1,I)**2)*(PXTRAY(5,I)**2)))
                  MAB3(3,I)=C+AC
                  IF(DABS(MAB3(3,I)).LT.1.0D-15) MAB3(3,I)=0.0D0
C
C       THIRD ORDER DISTORTION (DIST3)
C
                  E=(SIBAR*PXTRAY(3,I)*PXTRAY(7,I))+
     1            (INV*(K-1.0D0)*PXTRAY(7,I))*(PXTRAY(6,I)
     1            +PXTRAY(6,(I-1)))
                  AE=(C1BAR*PXTRAY(1,I)*(PXTRAY(5,I)**3))
                  MAB3(4,I)=E+AE
                  IF(DABS(MAB3(4,I)).LT.1.0D-15) MAB3(4,I)=0.0D0
C
C       THIRD ORDER PETZVAL SUM
C       CHECK FOR X-TORICS
                  IF(ALENS(23,I).NE.2.0D0) THEN
C       NOT X-TORIC
                      PIE=(((INV**2)*(CV*(K-1.0D0)))/N)
                      PIECV=PIE/(INV**2)
                      APIE=0.0D0
                      MAB3(11,I)=PIECV
                      MAB3(5,I)=PIE+APIE

                      IF(DABS(MAB3(5,I)).LT.1.0D-15) MAB3(5,I)=0.0D0
                  ELSE
C       X-TORIC
                      PIE=(((INV**2)*(ALENS(24,I)*(K-1.0D0)))/N)
                      PIECV=PIE/(INV**2)
                      APIE=0.0D0
                      MAB3(11,I)=PIECV
                      MAB3(5,I)=PIE+APIE
                      IF(DABS(MAB3(5,I)).LT.1.0D-15) MAB3(5,I)=0.0D0
                  END IF
C
C       NOW FOR THE 3RD ORDER EXIT PUPIL ABERRATIONS
C
C       THIRD ORDER SPHERICAL (SA3) SURFACE
C       CONTRIBUTION (SURFACE I) (EXIT PUPIL)
C
                  BBAR=(SIBAR*(PXTRAY(7,I)**2))
                  ABBAR=(C1BAR*(PXTRAY(5,I)**4))
                  MAB3(6,I)=BBAR+ABBAR
                  IF(DABS(MAB3(6,I)).LT.1.0D-15) MAB3(6,I)=0.0D0
C
C       THIRD ORDER COMMA (CMA3) SURFACE
C       CONTRIBUTION (SURFACE I) (EXIT PUPIL)
C
                  FBAR=(SIBAR*(PXTRAY(7,I)*PXTRAY(3,I)))
                  AFBAR=(C1BAR*((PXTRAY(5,I)**3)*PXTRAY(1,I)))
                  MAB3(7,I)=FBAR+AFBAR
                  IF(DABS(MAB3(7,I)).LT.1.0D-15) MAB3(7,I)=0.0D0
C
C       THIRD ORDER ASTIGMATISM (AST3) SURFACE CONTRIBUTION
C       AT SURFACE I (EXIT PUPIL)
C
                  CBAR=(SIBAR*(PXTRAY(3,I)**2))
                  ACBAR=(C1BAR*((PXTRAY(5,I)**2)*(PXTRAY(1,I)**2)))
                  MAB3(8,I)=CBAR+ACBAR
                  IF(DABS(MAB3(8,I)).LT.1.0D-15) MAB3(8,I)=0.0D0
C
C       THIRD ORDER DISTORTION (DIST3)
C
                  EBAR=(SI*PXTRAY(3,I)*PXTRAY(7,I))-
     1            (INV*(K-1.0D0)*PXTRAY(3,I)*(PXTRAY(2,I)+
     1            PXTRAY(2,(I-1))))
                  AEBAR=(C1BAR*PXTRAY(5,I)*(PXTRAY(1,I)**3))
                  MAB3(9,I)=EBAR+AEBAR
                  IF(DABS(MAB3(9,I)).LT.1.0D-15) MAB3(9,I)=0.0D0
C
C       THIRD ORDER PETZVAL SUM
C
                  MAB3(10,I)=MAB3(5,I)
C       WI
                  WI=0.125D0*(((PXTRAY(3,I)**2)+(PXTRAY(4,I)**2)+
     1            (PXTRAY(2,I)**2)-(3.0D0*((PXTRAY(2,(I-1)))**2))))
C       X73
                  X73=((3.0D0*PXTRAY(3,I)*PXTRAY(4,I))+
     1            (2.0D0*(PXTRAY(2,I)**2))-(3.0D0*
     1            ((PXTRAY(2,(I-1)))**2)))
C       X74
                  X74=(3.0D0*PXTRAY(3,I)*PXTRAY(8,I))+
     1            (2.0D0*PXTRAY(2,I)*PXTRAY(6,I))-
     2            (3.0D0*PXTRAY(2,(I-1))*PXTRAY(6,(I-1)))
C       X75
                  X75=((3.0D0*PXTRAY(7,I)*PXTRAY(8,I))+
     1            (2.0D0*((PXTRAY(6,I))**2))-(3.0D0*((PXTRAY(6,(I-1)))**2)))
C       X76
                  X76=(PXTRAY(3,I)*((3.0D0*PXTRAY(2,(I-1)))-PXTRAY(2,I)))
C       X77
                  X77=PXTRAY(7,I)*((2.0D0*PXTRAY(2,(I-1)))-PXTRAY(2,I))+
     1            (PXTRAY(3,I)*PXTRAY(6,(I-1)))
C       X78
                  X78=PXTRAY(7,I)*((3.0D0*PXTRAY(6,(I-1)))-PXTRAY(6,I))
C       X42
                  X42=(((PXTRAY(5,I)*PXTRAY(3,I))*(PXTRAY(7,I)
     1            -PXTRAY(6,(I-1))))+
     1            ((PXTRAY(1,I)*PXTRAY(7,I))*(PXTRAY(6,I)+PXTRAY(6,(I-1)))))
C       X82
                  X82=(((PXTRAY(5,I)*PXTRAY(2,(I-1)))*
     1            (PXTRAY(7,I)-PXTRAY(6,(I-1))))-
     2            ((PXTRAY(1,I)*PXTRAY(8,I))*(PXTRAY(6,I)+PXTRAY(6,(I-1)))))
C       XBAR42
                  XBAR42=(((PXTRAY(1,I)*PXTRAY(7,I))*
     1            (PXTRAY(3,I)-PXTRAY(2,(I-1))))+
     1            ((PXTRAY(5,I)*PXTRAY(3,I))*(PXTRAY(2,I)+PXTRAY(2,(I-1)))))
C       XBAR82
                  XBAR82=(((PXTRAY(1,I)*PXTRAY(6,(I-1)))*
     1            (PXTRAY(3,I)-PXTRAY(2,(I-1))))-
     2            ((PXTRAY(5,I)*PXTRAY(4,I))*(PXTRAY(2,I)+PXTRAY(2,(I-1)))))
C
C       NOW FOR THE ADDITIONAL TERMS FOR THE 5TH ORDER ABERRATIONS
C       AND SEVENTH ORDER SPHERICAL
C
C       THE S(HAT) TERMS USED FOR THE SHERICAL PART OF THE SURFACE
C       CONTRIBUTIONS
C
C       S1P2
                  S1P2=3.0D0*WI*SI*PXTRAY(3,I)
C       S2P
                  S2P=(0.25D0*SI)*((PXTRAY(7,I)*X73)+(PXTRAY(3,I)*X74)-
     1            (PXTRAY(6,I)*X76)-(PXTRAY(2,I)*X77))
C       S3P2
                  S3P2=(0.25D0*N*(K-1.0D0))*((X42*X73)+(X76*X82)+
     1            (PXTRAY(1,I)*(PXTRAY(3,I)+PXTRAY(2,I))*
     2            ((PXTRAY(3,I)*X75)-(PXTRAY(2,I)*X78))))
C       S4P2
                  S4P2=SI*((PXTRAY(7,I)*X74)-(PXTRAY(6,I)*X77))
C       S5P
                  S5P=(0.25D0*N*(K-1.0D0))*
     1            ((X42*X74)+(X77*X82)+
     2            (PXTRAY(1,I)*(PXTRAY(3,I)+PXTRAY(2,I)))*
     3            ((PXTRAY(7,I)*X75)-(PXTRAY(6,I)*X78)))
C       S6P2
                  S6P2=(0.25D0*N*(K-1.0D0))*((X42*X75)+(X78*X82))
C       S1Q2
                  S1Q2=(0.25D0*N*(K-1.0D0))*((XBAR42*X73)+(X76*XBAR82))

C
C       NOW CALCULATE L,LBAR, AND M
C
                  L=0.25D0*((3.0D0*PXTRAY(4,I))+(2.0D0*((2.0D0*K)
     1            -1.0D0)*PXTRAY(2,I)))
                  LBAR=0.25D0*((3.0D0*PXTRAY(8,I))+(2.0D0*((2.0D0*K)
     1            -1.0D0)*PXTRAY(6,I)))
                  M=(K*INV)/J_NP
C
C       NOW THE ASPHERIC S(HAT) TERMS
C
C       A2S1P
                  A2S1P=C1BAR*(PXTRAY(1,I)**4)*L
C       AS2P
                  AS2P=(2.0D0*AF*L)+(0.5D0*C1BAR*M*(PXTRAY(1,I)**3))
C       A2S3P
                  A2S3P=(2.0D0*AC*L)+(C1BAR*M*PXTRAY(5,I)*(PXTRAY(1,I)**2))
C       A2S4P
                  A2S4P=2.0D0*A2S3P
C       AS5P
                  AS5P=(2.0D0*AE*L)+((3.0D0/2.0D0)*M*PXTRAY(1,I)*
     1            C1BAR*(PXTRAY(5,I)**2))
                  A2S6P=(ABBAR*L)+(C1BAR*M*(PXTRAY(5,I)**3))
C       AS1Q
                  AS1Q=(AB*LBAR)-(C1BAR*M*(PXTRAY(1,I)**3))
C       C
C       NOW JO,JOBAR,ALPHA,BETA,GAMMA,LAMBDA,MU,NU
C
C       CHECK FOR X-TORICS
                  IF(ALENS(23,I).NE.2.0D0) THEN
C
                      JO=(C2BAR*PXTRAY(1,I))-(0.25D0*CV*C1BAR*
     1                ((3.0D0*PXTRAY(4,I))-(5.0D0*PXTRAY(2,I))))
                      JOBAR=(C2BAR*PXTRAY(5,I))-(0.25D0*CV*C1BAR*
     1                ((3.0D0*PXTRAY(8,I))-(5.0D0*PXTRAY(6,I))))
C
                  ELSE
C       X-TORIC PRESENT
                      JO=(C2BAR*PXTRAY(1,I))-(0.25D0*ALENS(24,I)*C1BAR*
     1                ((3.0D0*PXTRAY(4,I))-(5.0D0*PXTRAY(2,I))))
                      JOBAR=(C2BAR*PXTRAY(5,I))-(0.25D0*ALENS(24,I)*C1BAR*
     1                ((3.0D0*PXTRAY(8,I))-(5.0D0*PXTRAY(6,I))))
                  END IF
C
                  ALPHA=(0.5D0*((PXTRAY(2,I)**2)+(3.0D0*PXTRAY(3,I)
     1            *PXTRAY(4,I))-
     1            (2.0D0*PXTRAY(3,I)*PXTRAY(2,I))))
C
                  BETA=(PXTRAY(2,I)*PXTRAY(6,I))+(3.0D0*
     1            PXTRAY(3,I)*PXTRAY(8,I))-
     1            (PXTRAY(3,I)*PXTRAY(6,I))-(PXTRAY(7,I)*PXTRAY(2,I))
C
                  GAMMA=0.5D0*((PXTRAY(6,I)**2)+(3.0D0*
     1            PXTRAY(7,I)*PXTRAY(8,I))-
     1            (2.0D0*PXTRAY(7,I)*PXTRAY(6,I)))
C
                  LAMBDA=(ALPHA*C1BAR)+(JO*PXTRAY(1,I))
C
                  MU=(BETA*C1BAR)+(2.0D0*JO*PXTRAY(5,I))
C
                  NU=(PXTRAY(1,I)*GAMMA*C1BAR)+(JO*(PXTRAY(5,I)**2))
C
C       NOW FOR THE (U) TERMS
C
C       A2S1PU
                  A2S1PU=(PXTRAY(1,I)**3)*LAMBDA
C       AS2PU
                  AS2PU=((PXTRAY(1,I)**2)*PXTRAY(5,I)*LAMBDA)+
     1            (0.5D0*MU*(PXTRAY(1,I)**3))
C       A2S3PU
                  A2S3PU=(PXTRAY(1,I)*(PXTRAY(5,I)**2)*LAMBDA)+
     1            ((PXTRAY(1,I)**2)*NU)
C       A2S4PU
                  A2S4PU=(2.0D0*MU*PXTRAY(5,I)*(PXTRAY(1,I)**2))
C       AS5PU
                  AS5PU=(0.5D0*MU*PXTRAY(1,I)*(PXTRAY(5,I)**2))+
     1            (PXTRAY(1,I)*PXTRAY(5,I)*NU)
C       A2S6PU
                  A2S6PU=(PXTRAY(5,I)**2)*NU
C       A2S1QU
                  A2S1QU=(PXTRAY(1,I)**2)*((ALPHA*C1BAR*PXTRAY(5,I))+
     1            (JOBAR*(PXTRAY(1,I)**2)))
C
C
C       5TH ORDER INTRINSIC SURFACE CONTRIBUTIONS
C       SPHERICAL AND ASPHERIC
C
C       B5SLASH+AB5SLASH
C
                  SLH=PXTRAY(3,I)*S1P2
                  ASLH=(PXTRAY(3,I)*A2S1P)+(PXTRAY(1,I)*A2S1PU)
                  MAB57(1,I)=SLH+ASLH
C       WE NEED TO KEEP A PERMANENT COPY FOR SA7 CALC
C       KEEP IT IN MAB57(15,I)
                  MAB57(15,I)=MAB57(1,I)
C
C       F1SLASH+AF1SLASH
C
                  SLH=(PXTRAY(7,I)*S1P2)+(PXTRAY(3,I)*S2P)
                  ASLH=(PXTRAY(7,I)*A2S1P)+(PXTRAY(5,I)*A2S1PU)+
     1            (PXTRAY(3,I)*AS2P)+(PXTRAY(1,I)*AS2PU)
                  MAB57(2,I)=SLH+ASLH
C       WE NEED TO KEEP A PERMANENT COPY FOR SA7 CALC
C       KEEP IT IN MAB57(16,I)
                  MAB57(16,I)=MAB57(2,I)
C
C       F2SLASH+AF2SLASH
C
                  SLH=PXTRAY(3,I)*S2P
                  ASLH=(PXTRAY(3,I)*AS2P)+(PXTRAY(1,I)*AS2PU)
                  MAB57(3,I)=SLH+ASLH
C       WE NEED TO KEEP A PERMANENT COPY FOR SA7 CALC
C       KEEP IT IN MAB57(17,I)
                  MAB57(17,I)=MAB57(3,I)
C
C       M1SLASH+AM1SLASH
C
                  SLH=(2.0D0*PXTRAY(7,I)*S2P)
                  ASLH=2.0D0*((PXTRAY(7,I)*AS2P)+(PXTRAY(5,I)*AS2PU))
                  MAB57(4,I)=SLH+ASLH
C
C       M2SLASH+AM2SLASH
C
                  SLH=PXTRAY(3,I)*S3P2
                  ASLH=(PXTRAY(3,I)*A2S3P)+(PXTRAY(1,I)*A2S3PU)
                  MAB57(5,I)=SLH+ASLH
C
C       M3SLASH+AM3SLASH
C
                  SLH=PXTRAY(3,I)*S4P2
                  ASLH=(PXTRAY(3,I)*A2S4P)+(PXTRAY(1,I)*A2S4PU)
                  MAB57(6,I)=SLH+ASLH
C
C       N1SLASH+AN1SLASH
C
                  SLH=PXTRAY(7,I)*S3P2
                  ASLH=(PXTRAY(7,I)*A2S3P)+(PXTRAY(5,I)*A2S3PU)
                  MAB57(7,I)=SLH+ASLH
C
C       N2SLASH+AN2SLASH
C
                  SLH=(PXTRAY(7,I)*S4P2)+(2.0D0*PXTRAY(3,I)*S5P)
                  ASLH=(PXTRAY(7,I)*A2S4P)+(PXTRAY(5,I)*A2S4PU)+
     1            2.0D0*((PXTRAY(3,I)*AS5P)+(PXTRAY(1,I)*AS5PU))
                  MAB57(8,I)=SLH+ASLH
C
C       N3SLASH+AN3SLASH
C
                  SLH=PXTRAY(3,I)*S5P
                  ASLH=(PXTRAY(3,I)*AS5P)+(PXTRAY(1,I)*AS5PU)
                  MAB57(9,I)=SLH+ASLH
C
C       C5SLASH+AC5SLASH
C
                  SLH=0.5D0*(PXTRAY(7,I)*S5P)
                  ASLH=0.5D0*((PXTRAY(7,I)*AS5P)+(PXTRAY(5,I)*AS5PU))
                  MAB57(10,I)=SLH+ASLH
C
C       PI5SLASH+API5SLASH
C
                  SLH=(PXTRAY(3,I)*S6P2)-(0.5D0*PXTRAY(7,I)*S5P)
                  ASLH=(PXTRAY(3,I)*A2S6P)+(PXTRAY(1,I)*A2S6PU)
     1            -0.5D0*((PXTRAY(7,I)*AS5P)+(PXTRAY(5,I)*AS5PU))
                  MAB57(11,I)=SLH+ASLH
C
C       E5SLASH+AE5SLSH
C
                  SLH=PXTRAY(7,I)*S6P2
                  ASLH=(PXTRAY(7,I)*A2S6P)+(PXTRAY(5,I)*A2S6PU)
                  MAB57(12,I)=SLH+ASLH
C
C       E5BARSLASH+AE5BARSLASH
C
                  SLH=PXTRAY(3,I)*S1Q2
                  ASLH=(PXTRAY(3,I)*AS1Q)+(PXTRAY(1,I)*A2S1QU)
                  MAB57(13,I)=SLH+ASLH
C
C       AND SEVENTH ORDER SPHERICAL
C
C       B7SLASH+AB7SLASH
C
                  B71=((0.125D0*SI*PXTRAY(3,I))*((2.0D0*PXTRAY(2,I))
     1            -(5.0D0*PXTRAY(2,(I-1)))))/N
                  IF(ALENS(23,I).NE.2.0D0) THEN
C       NO X-TORIC
                      B72=(10.0D0*(WI**2))+(B71*CV)
                  ELSE
C       X-TORIC
                      B72=(10.0D0*(WI**2))+(B71*ALENS(24,I))
                  END IF
                  SLH=(SI*(PXTRAY(3,I)**2))*B72
C NOW FOR THE ASPHERIC PART
C
C       GAMMA1
                  GAMMA1=C1*(PXTRAY(1,I)**2)
C       CHECK IF X-TORIC
                  IF(ALENS(23,I).NE.2.0D0) THEN
                      GAMMA2=(PXTRAY(1,I)**3)*((C2*PXTRAY(1,I))+
     1                (0.25D0*CV*C1*(PXTRAY(3,I)+
     1                (3.0D0*PXTRAY(2,(I-1))))))
                  ELSE
                      GAMMA2=(PXTRAY(1,I)**3)*((C2*PXTRAY(1,I))+
     1                (0.25D0*ALENS(24,I)*C1*(PXTRAY(3,I)+
     1                (3.0D0*PXTRAY(2,(I-1))))))
                  END IF
                  G32=C3*(PXTRAY(1,I)**2)
                  G33=((C2*PXTRAY(1,I)*(PXTRAY(3,I)+
     1            (5.0D0*PXTRAY(2,(I-1)))))/3.0D0)
C       CHECK FOR X-TORIC
                  IF(ALENS(23,I).NE.2.0D0)THEN
                      G34=CV
                  ELSE
                      G34=ALENS(24,I)
                  END IF
                  G36=(0.125D0*C1)*
     1            ((PXTRAY(3,I)*(PXTRAY(3,I)+(5.0D0*PXTRAY(2,(I-1)))))-
     2            (PXTRAY(2,(I-1))*(PXTRAY(3,I)-(5.0D0*PXTRAY(2,(I-1))))))
                  G37=.25D0*(C1**2)*PXTRAY(1,I)*PXTRAY(3,I)
                  G31=G32+(G33*G34)+((G34**2)*G36)+G37
                  GAMMA3=(PXTRAY(1,I)**4)*G31
C       NOW GAMMA 1,2 AND 3 ARE DONE
CC
C       NOW CALCULATE L3 AND D3
C
C       L3
                  L3=(PXTRAY(1,I)*GAMMA3*(N-J_NP))+0.5D0*
     1            ((GAMMA1*(S1P2+A2S1P))+(GAMMA2*SI*PXTRAY(3,I)))
C
C       D3
                  D32=((C2*(PXTRAY(1,I)**2))/6.0D0)
                  D33=((4.0D0*PXTRAY(4,I))+(3.0D0*PXTRAY(2,I)
     1            *((2.0D0*K)-1.0D0)))
                  D34=0.125D0*C1
                  D35=GAMMA1*PXTRAY(1,I)*(1.0D0+(2.0D0*K*(K-1.0D0)))
C       CHECK FOR X-TORIC
                  IF(ALENS(23,I).NE.2.0D0) THEN
                      D36=CV*PXTRAY(1,I)
                      D37=4.0D0*CV*PXTRAY(1,I)*(PXTRAY(3,I)+
     1                PXTRAY(2,(I-1)))
                  ELSE
C       X-TORIC
                      D36=ALENS(24,I)*PXTRAY(1,I)
                      D37=4.0D0*ALENS(24,I)*PXTRAY(1,I)*(PXTRAY(3,I)+
     1                PXTRAY(2,(I-1)))
                  END IF
                  D38=PXTRAY(4,I)*(5.0D0*((2.0D0*PXTRAY(2,I))+PXTRAY(3,I))
     1            +PXTRAY(4,I))
                  D39=K*PXTRAY(2,I)
                  D40=PXTRAY(4,I)*(4.0D0*((2.0D0*PXTRAY(2,I))+PXTRAY(3,I)+
     1            PXTRAY(4,I))+PXTRAY(3,I))
                  D41=D39*((3.0D0*(PXTRAY(3,I)**2))-(10.0D0
     1            *(PXTRAY(2,(I-1))**2))+
     1            D40)
                  D31=(D32*D33)+D34*(D35+D36*(D37+D38)+D41)
                  D3=(N-J_NP)*(PXTRAY(1,I)**4)*D31
C
C       NOW L3 AND D3 ARE DONE
C
C       ASLH
                  ASLH=(PXTRAY(3,I)*D3)+(PXTRAY(1,I)*L3)
C
                  MAB57(14,I)=SLH+ASLH
 10           CONTINUE
C
C       BEFORE CALCULATING THE TRANSFERED PLUS SURFACE CONTIBUTIONS,
C       STORE THE SURFACE CONTRIBUTIONS IN THE ARRAY SAB57
C       THIRD ORDER IS ALWAYS JUST SURFACE CONTRIBUTIONS BY DEFAULT.
C
              I=INT(SYSTEM1(20))
              SAB57(1:20,0:I)=MAB57(1:20,0:I)
C       NOW THE SURFACE CONTRIBUTIONS ARE AVAILABLE LATER.
C
C
C       NOW ALL OF THE INTRINSIC SURFACE CONTRIBUTIONS HAVE BEEN
C       CALCULATED
C       NOW CALCULATE THE COMPLETE ABERRATION COEFICIENTS
C       FOR EACH SURFACE
C
C       THIRD ORDER ALREADY ARE COMPLETE AS THEY HAVE NO TRANSFERRED
C       PART
C       FIFTH ORDER, START AT SURF 1 AS ALL SURFACE 0 COEFFS ARE
C       ALWAYS ZERO
C       SET ALL PRIMED VARIABLES TO 0.0
              BPRIME=0.0D0
              FPRIME=0.0D0
              CPRIME=0.0D0
              EPRIME=0.0D0
              PPRIME=0.0D0
              BBPRIM=0.0D0
              FBPRIM=0.0D0
              CBPRIM=0.0D0
              EBPRIM=0.0D0
              PPRIME=0.0D0
              B5PRIM=0.0D0
              EB5PRM=0.0D0
              DO 100 I=1,INT(SYSTEM1(20))
C       AT SURFACE I
C       CALCULATE BPRIME,FPRIME,CPRIME,EPRIME,PPRIME, ETC.
C
C       NOTE: ALL SLASH TERMS NOW INCLUDE THE ASPHERIC
C       CONTRIBUTION.
C
                  BPRIME=BPRIME+MAB3(1,(I-1))
                  FPRIME=FPRIME+MAB3(2,(I-1))
                  CPRIME=CPRIME+MAB3(3,(I-1))
                  EPRIME=EPRIME+MAB3(4,(I-1))
                  PPRIME=PPRIME+MAB3(5,(I-1))
                  BBPRIM=BBPRIM+MAB3(6,(I-1))
                  FBPRIM=FBPRIM+MAB3(7,(I-1))
                  CBPRIM=CBPRIM+MAB3(8,(I-1))
                  EBPRIM=EBPRIM+MAB3(9,(I-1))
                  PBPRIM=PBPRIM+MAB3(10,(I-1))
                  B5PRIM=B5PRIM+MAB57(1,(I-1))
                  EB5PRM=EB5PRM+MAB57(13,(I-1))

C       B5=B5SLASH+(3/2INV)*(BPRIME*F-EBARPRIME*B)
C       B5SLASH IS IN MAB57(1,I)
C       WHEN DONE B5 WILL REPLACE B5SLASH
                  TRAN=(BPRIME*MAB3(2,I))-(EBPRIM*MAB3(1,I))
                  MAB57(1,I)=MAB57(1,I)+((3.0D0/(2.0D0*INV))*TRAN)
                  IF(DABS(MAB57(1,I)).LT.1.0D-15) MAB57(1,I)=0.0D0
C
C       F1=F1SLASH+(1/2INV)*
C       [B'(PI+4C)+(5F'-4EBAR')*F-(2PI'+5CBAR')*B)
C       F1SLASH IS IN MAB57(2,I)
C       WHEN DONE F1 WILL REPLACE F1SLASH
                  TRAN=(BPRIME*(MAB3(5,I)+(4.0D0*MAB3(3,I))))+
     1            (((5.0D0*FPRIME)-(4.0D0*EBPRIM))*MAB3(2,I))-
     2            (((2.0D0*PPRIME)+(5.0D0*CBPRIM))*MAB3(1,I))
                  MAB57(2,I)=MAB57(2,I)+((1.0D0/(2.0D0*INV))*TRAN)
                  IF(DABS(MAB57(2,I)).LT.1.0D-15) MAB57(2,I)=0.0D0
C
C       F2=F2SLASH+(1/2INV)*
C       [B'(PI+2C)+2(2F'-EBAR')*F-(PI'+4CBAR')*B)
C       F2SLASH IS IN MAB57(3,I)
C       WHEN DONE F2 WILL REPLACE F2SLASH
                  TRAN=(BPRIME*(MAB3(5,I)+(2.0D0*MAB3(3,I))))+
     1            (2.0D0*(((2.0D0*FPRIME)-(1.0D0*EBPRIM))*MAB3(2,I)))-
     2            (((1.0D0*PPRIME)+(4.0D0*CBPRIM))*MAB3(1,I))
                  MAB57(3,I)=MAB57(3,I)+((1.0D0/(2.0D0*INV))*TRAN)
                  IF(DABS(MAB57(3,I)).LT.1.0D-15) MAB57(3,I)=0.0D0
C
C       M1=M1SLASH+(1/INV)*
C       [B'E+(4F'-EBAR')C+(C'-4CBAR'-2PI')F-FBAR'B]
C       M1SLASH IS IN MAB57(4,I)
C       WHEN DONE M1 WILL REPLACE M1SLASH
                  TRAN=(BPRIME*MAB3(4,I))+
     1            (MAB3(3,I)*((4.0D0*FPRIME)-EBPRIM))+
     2            ((CPRIME-(4.0D0*CBPRIM)-(2.0D0*PPRIME))*MAB3(2,I))-
     3            (FBPRIM*MAB3(1,I))
                  MAB57(4,I)=MAB57(4,I)+((1.0D0/INV)*TRAN)
                  IF(DABS(MAB57(4,I)).LT.1.0D-15) MAB57(4,I)=0.0D0
C
C       M2=M2SLASH+(1/2*INV)*
C       [B'E+(2F'-EBAR')(PI+C)+(3C'-2CBAR'+PI')F-3FBAR'B]
C       M2SLASH IS IN MAB57(5,I)
C       WHEN DONE M2 WILL REPLACE M2SLASH
                  TRAN=(BPRIME*MAB3(4,I))+
     1            ((MAB3(3,I)+MAB3(5,I))*((2.0D0*FPRIME)-EBPRIM))+
     2            (((3.0D0*CPRIME)-(2.0D0*CBPRIM)+PPRIME)*MAB3(2,I))-
     3            (3.0D0*FBPRIM*MAB3(1,I))
                  MAB57(5,I)=MAB57(5,I)+((1.0D0/(2.0D0*INV))*TRAN)
                  IF(DABS(MAB57(5,I)).LT.1.0D-15) MAB57(5,I)=0.0D0
C
C       M3=M3SLASH+(2/INV)*
C       [F'(2C+PI)+(C'-2CBAR')F-FBAR'B]
C       M3SLASH IS IN MAB57(6,I)
C       WHEN DONE M3 WILL REPLACE M3SLASH
                  TRAN=(FPRIME*((2.0D0*MAB3(3,I))+MAB3(5,I)))+
     1            ((CPRIME-(2.0D0*CBPRIM))*MAB3(2,I))-(FBPRIM*MAB3(1,I))
                  MAB57(6,I)=MAB57(6,I)+((2.0D0/(1.0D0*INV))*TRAN)
                  IF(DABS(MAB57(6,I)).LT.1.0D-15) MAB57(6,I)=0.0D0
C
C       N1=N1SLASH+(1/2INV)*
C       [3F'E-(PI'+CBAR')(PI+C)+2(C'-CBAR')C+(E'-2FBAR')F-BBAR'*B]
C       N1SLASH IS IN MAB57(7,I)
C       WHEN DONE N1 WILL REPLACE N1SLASH
                  TRAN=(3.0D0*FPRIME*MAB3(4,I))-
     1            ((PPRIME+CBPRIM)*(MAB3(5,I)+MAB3(3,I)))+
     2            (2.0D0*(CPRIME-CBPRIM)*MAB3(3,I))+
     3            (((EPRIME-(2.0D0*FBPRIM))*MAB3(2,I))-(BBPRIM*MAB3(1,I)))
                  MAB57(7,I)=MAB57(7,I)+((1.0D0/(2.0D0*INV))*TRAN)
                  IF(DABS(MAB57(7,I)).LT.1.0D-15) MAB57(7,I)=0.0D0
C
C       N2=N2SLASH+(1/INV)*
C       [3F'E+(PI'-CBAR'+3C')(PI+3C)-(C'+PI')C+(E'-8FBAR')F-BBAR'*B]
C       N2SLASH IS IN MAB57(8,I)
C       WHEN DONE N2 WILL REPLACE N2SLASH
                  TRAN=(3.0D0*FPRIME*MAB3(4,I))+
     1            ((PPRIME-CBPRIM+(3.0D0*CPRIME))*(MAB3(5,I)+(3.0D0
     1            *MAB3(3,I))))-
     2            (1.0D0*(CPRIME+PPRIME)*MAB3(3,I))+
     3            ((EPRIME-(8.0D0*FBPRIM))*MAB3(2,I))-(BBPRIM*MAB3(1,I))
                  MAB57(8,I)=MAB57(8,I)+((1.0D0/(1.0D0*INV))*TRAN)
                  IF(DABS(MAB57(8,I)).LT.1.0D-15) MAB57(8,I)=0.0D0
C
C       N3=N3SLASH+(1/2INV)*
C       [F'E+(PI'-CBAR'+3C')(PI+C)+(C'+PI')C+(E'-4FBAR')F-BBAR'*B]
C       N3SLASH IS IN MAB57(9,I)
C       WHEN DONE N3 WILL REPLACE N3SLASH
                  TRAN=(FPRIME*MAB3(4,I))+
     1            ((PPRIME-CBPRIM+(3.0D0*CPRIME))*(MAB3(5,I)+(MAB3(3,I))))+
     2            ((CPRIME+PPRIME)*MAB3(3,I))+
     3            ((EPRIME-(4.0D0*FBPRIM))*MAB3(2,I))-(BBPRIM*MAB3(1,I))
                  MAB57(9,I)=MAB57(9,I)+((1.0D0/(2.0D0*INV))*TRAN)
                  IF(DABS(MAB57(9,I)).LT.1.0D-15) MAB57(9,I)=0.0D0
C
C       C5=C5SLASH+(1/4INV)*
C       [(4C'+PI')E-FBAR'PI+2(E'-2FBAR')C-2BBAR'F]
C       C5SLASH IS IN MAB57(10,I)
C       WHEN DONE C5 WILL REPLACE C5SLASH
                  TRAN=(((4.0D0*CPRIME)+PPRIME)*(MAB3(4,I)))-
     1            (FBPRIM*MAB3(5,I))+
     2            (2.0D0*(EPRIME-(2.0D0*FBPRIM))*MAB3(3,I))-
     3            (2.0D0*BBPRIM*MAB3(2,I))
                  MAB57(10,I)=MAB57(10,I)+((1.0D0/(4.0D0*INV))*TRAN)
                  IF(DABS(MAB57(10,I)).LT.1.0D-15) MAB57(10,I)=0.0D0
C
C       PI5=PI5SLASH+(1/4INV)*
C       [(PI'-2C')E+(4E'-FBAR')PI+2(E'+FBAR')C-2BBAR'F]
C       PI5SLASH IS IN MAB57(11,I)
C       WHEN DONE PI5 WILL REPLACE PI5SLASH
                  TRAN=((PPRIME-(2.0D0*CPRIME))*MAB3(4,I))+
     1            (((4.0D0*EPRIME)-FBPRIM)*MAB3(5,I))+
     2            (2.0D0*(EPRIME+FBPRIM)*MAB3(3,I))-(2.0D0*
     1            BBPRIM*MAB3(2,I))
                  MAB57(11,I)=MAB57(11,I)+((1.0D0/(4.0D0*INV))*TRAN)
                  IF(DABS(MAB57(11,I)).LT.1.0D-15) MAB57(11,I)=0.0D0
C
C       E5=E5SLASH+(1/2INV)*
C       [3E'E-BBAR'(PI+3C)]
C       E5SLASH IS IN MAB57(12,I)
C       WHEN DONE E5 WILL REPLACE E5SLASH
                  TRAN=(3.0D0*EPRIME*MAB3(4,I))-
     1            (BBPRIM*(MAB3(5,I)+(3.0D0*MAB3(3,I))))
                  MAB57(12,I)=MAB57(12,I)+((1.0D0/(2.0D0*INV))*TRAN)
                  IF(DABS(MAB57(12,I)).LT.1.0D-15) MAB57(12,I)=0.0D0
C
C       E5BAR=E5BARSLASH+(1/2INV)*
C       [B'(PI+3CBAR)-3EBAR'EBAR)]
C       E5BARSLASH IS IN MAB57(13,I)
C       WHEN DONE E5BAR WILL REPLACE E5BARSLASH
                  TRAN=(BPRIME*(MAB3(5,I)+(3.0D0*MAB3(8,I))))-
     1            (3.0D0*EBPRIM*MAB3(9,I))
                  MAB57(13,I)=MAB57(13,I)+((1.0D0/(2.0D0*INV))*TRAN)
                  IF(DABS(MAB57(13,I)).LT.1.0D-15) MAB57(13,I)=0.0D0
C       NOW SEVENTH ORDER SPHERICAL
C
C       NOW THE SEVENTH ORDER ABERATION FOR SURFACE I
C       IS:
C       B7=B7SLASH+(1/2INV)*
C       [(1/2INV)*(B'**2)(PI+3C)+3(B5'-(1/INV)B'EBAR')F+
C       3((1/2INV)(EBAR'**2)-EBAR5')B+
C       B'(F1SLH+F2SLH)-
C       5EBAR'B5SLH]
                  TRAN1=((0.5D0/INV)*(BPRIME**2)*(MAB3(5,I)+(3.0D0
     1            *MAB3(3,I))))
                  TRAN2=(3.0D0*(B5PRIM-((1.0D0/INV)*BPRIME*EBPRIM))
     1            *MAB3(2,I))
                  TRAN4=(3.0D0*(((0.5D0/INV)*(EBPRIM**2))-EB5PRM)*MAB3(1,I))
                  TRAN5=(BPRIME*(MAB57(16,I)+MAB57(17,I)))
                  TRAN6=(5.0D0*EBPRIM*MAB57(15,I))
                  TRAN=TRAN1+TRAN2+TRAN4+TRAN5-TRAN6
                  MAB57(14,I)=MAB57(14,I)+((0.5D0/INV)*TRAN)
                  IF(DABS(MAB57(14,I)).LT.1.0D-15) MAB57(14,I)=0.0D0
 100          CONTINUE
C
              RETURN
          ELSE
C       IAB NOT 1
          END IF
          IF(IAB.EQ.2) THEN
C       THE CONTROL WAVELENGTH NUMBER IS STORED IN
C       SYSTEM1(11)
              IF(CW.GE.1.AND.CW.LE.5)
     1        CW= CW+45
              IF(CW.GE.6.AND.CW.LE.10)
     1        CW= CW+65
C
C       REFRACTIVE INDICES ARE IN ALENS(46,SURF) TO
C       ALENS(50,SURF)
C
C       INDICES ARE ADDRESSED IN THE FOLLOWING MANNER
C       INDEX AT:
C                CW IS ALENS(CW,I)
C       WHERE I IS THE SURFACE NUMBER
C
C       CALCULATE THE OPTICAL INVARIANT
C
              SF=INT(SYSTEM1(20))
              INV=-((PXTRAX(5,SF)*ALENS(CW,(SF-1))*PXTRAX(2,(SF-1)))-
     1        (PXTRAX(1,SF)*ALENS(CW,(SF-1))*PXTRAX(6,(SF-1))))
              IF(DABS(INV).LE.1.0D-10) THEN
                  CALL MACFAL
                  RETURN
              END IF
C
              DO 15 I=0,INT(SYSTEM1(20))
                  IF(ALENS(1,I).EQ.0.0D0.AND.ALENS(43,I).NE.0.0D0) THEN
                      CV=ALENS(43,I)/2.0D0
                      CC=-1.0D0
                  ELSE
                      CV=ALENS(1,I)
                      CC=ALENS(2,I)
                  END IF
C
C       AT THE OBJECT ALL IS 0.0
                  IF(I.EQ.0) THEN
                      GO TO 15
                  ELSE
C       I NOT OBJECT, PROCEED
                  END IF
C       CALCULATE BASIC CONSTANTS SI,SIBAR,
C       C1I AND C1IBAR
C
                  N=ALENS(CW,(I-1))
                  J_NP=ALENS(CW,I)
                  K=N/J_NP
                  SI=N*(K-1.0D0)*PXTRAX(1,I)*(PXTRAX(3,I)+PXTRAX(2,I))
                  SIBAR=N*(K-1.0D0)*PXTRAX(5,I)*(PXTRAX(7,I)+PXTRAX(6,I))
C
C       CALCULATE C1,C2,C3,C1BAR AND C2BAR FOR ASPHERIC TERMS
C
C       CHECK FOR Y-TORICS
                  IF(ALENS(23,I).NE.1.0D0) THEN
C       SURFACE IS NOT Y-TORIC
                      C1=(8.0D0*ALENS(4,I))+(CC*(CV**3))
                      C21=((CV**3)*CC*(CC+2.0D0))
     1                -(2.0D0*C1)
                      C22=(0.25D0*(CV**2)*C21)+(4.0D0*ALENS(5,I))
                      C2=3.0D0*C22
                      CF16=16.0D0*ALENS(6,I)
                      C31=(CC**2)+(3.0D0*CC)+3.0D0
                      C32=((CV**3)*CC*C31)-(3.0D0*C1)
                      C33=(5.0D0*(CV**2)*C32)-(12.0D0*C2)
                      C34=(-6.0D0*CV*(C1**2))+((CV**2)*C33)
                      C3=CF16+(0.125D0*C34)
                  ELSE
C       SURFACE IS Y-TORIC
                      C1=(8.0D0*((ALENS(37,I)**2)*ALENS(4,I))+
     1                (ALENS(41,I)*(ALENS(24,I)**3)))
                      C21=((ALENS(24,I)**3)*ALENS(41,I)*(ALENS(41,I)+2.0D0))
     1                -(2.0D0*C1)
                      C22=(0.25D0*(ALENS(24,I)**2)*C21)+(4.0D0*(ALENS(5,I)*
     1                (ALENS(38,I)**3)))
                      C2=3.0D0*C22
                      CF16=16.0D0*ALENS(6,I)*(ALENS(39,I)**4)
                      C31=(ALENS(41,I)**2)+(3.0D0*ALENS(41,I))+3.0D0
                      C32=((ALENS(24,I)**3)*ALENS(41,I)*C31)-(3.0D0*C1)
                      C33=(5.0D0*(ALENS(24,I)**2)*C32)-(12.0D0*C2)
                      C34=(-6.0D0*ALENS(24,I)*(C1**2))+((ALENS(24,I)**2)*C33)
                      C3=CF16+(0.125D0*C34)
                  END IF
                  C1BAR=C1*(N-J_NP)
                  C2BAR=C2*(N-J_NP)
C
C       NOW C1,C2,C3 C1BAR AND C2BAR ARE DONE
C
C       NOW: THIRD ORDER ABERRATIONS INCLUDING ASPHERIC TERM
C       THIRD ORDER SPHERICAL (SA3) SURFACE
C       CONTRIBUTION (SURFACE I)
C
                  B=(SI*(PXTRAX(3,I)**2))
                  AB=(C1BAR*(PXTRAX(1,I)**4))
                  XMAB3(1,I)=B+AB
                  IF(DABS(XMAB3(1,I)).LT.1.0D-15) XMAB3(1,I)=0.0D0
C
C       THIRD ORDER COMMA (CMA3) SURFACE
C       CONTRIBUTION (SURFACE I)
C
                  F=(SI*PXTRAX(3,I)*PXTRAX(7,I))
                  AF=(C1BAR*((PXTRAX(1,I)**3)*PXTRAX(5,I)))
                  XMAB3(2,I)=F+AF
                  IF(DABS(XMAB3(2,I)).LT.1.0D-15) XMAB3(2,I)=0.0D0
C
C       THIRD ORDER ASTIGMATISM (AST3) SURFACE CONTRIBUTION
C       AT SURFACE I
C
                  C=SI*((PXTRAX(7,I))**2)
                  AC=(C1BAR*((PXTRAX(1,I)**2)*(PXTRAX(5,I)**2)))
                  XMAB3(3,I)=C+AC
                  IF(DABS(XMAB3(3,I)).LT.1.0D-15) XMAB3(3,I)=0.0D0
C       THIRD ORDER DISTORTION (DIST3)
C
                  E=(SIBAR*PXTRAX(3,I)*PXTRAX(7,I))+
     1            (INV*(K-1.0D0)*PXTRAX(7,I))*(PXTRAX(6,I)+PXTRAX(6,(I-1)))
                  AE=(C1BAR*PXTRAX(1,I)*(PXTRAX(5,I)**3))
                  XMAB3(4,I)=E+AE
                  IF(DABS(XMAB3(4,I)).LT.1.0D-15) XMAB3(4,I)=0.0D0
C
C       THIRD ORDER PETZVAL SUM
C       CHECK FOR Y-TORICS
                  IF(ALENS(23,I).NE.1.0D0) THEN
C       NOT Y-TORIC
                      PIE=(((INV**2)*(CV*(K-1.0D0)))/N)
                      PIECV=PIE/(INV**2)
                      APIE=0.0D0
                      XMAB3(11,I)=PIECV
                      XMAB3(5,I)=PIE+APIE
                      IF(DABS(XMAB3(5,I)).LT.1.0D-15) XMAB3(5,I)=0.0D0
                  ELSE
C       Y-TORIC
                      PIE=(((INV**2)*(ALENS(24,I)*(K-1.0D0)))/N)
                      PIECV=PIE/(INV**2)
                      APIE=0.0D0
                      XMAB3(11,I)=PIECV
                      XMAB3(5,I)=PIE+APIE
                      IF(DABS(XMAB3(5,I)).LT.1.0D-15) XMAB3(5,I)=0.0D0
                  END IF
C
C       NOW FOR THE 3RD ORDER EXIT PUPIL ABERRATIONS
C
C       THIRD ORDER SPHERICAL (SA3) SURFACE
C       CONTRIBUTION (SURFACE I) (EXIT PUPIL)
C
                  BBAR=(SIBAR*(PXTRAX(7,I)**2))
                  ABBAR=(C1BAR*(PXTRAX(5,I)**4))
                  XMAB3(6,I)=BBAR+ABBAR
                  IF(DABS(XMAB3(6,I)).LT.1.0D-15) XMAB3(6,I)=0.0D0
C
C       THIRD ORDER COMMA (CMA3) SURFACE
C       CONTRIBUTION (SURFACE I) (EXIT PUPIL)
C
                  FBAR=(SIBAR*(PXTRAX(7,I)*PXTRAX(3,I)))
                  AFBAR=(C1BAR*((PXTRAX(5,I)**3)*PXTRAX(1,I)))
                  XMAB3(7,I)=FBAR+AFBAR
                  IF(DABS(XMAB3(7,I)).LT.1.0D-15) XMAB3(7,I)=0.0D0
C
C       THIRD ORDER ASTIGMATISM (AST3) SURFACE CONTRIBUTION
C       AT SURFACE I (EXIT PUPIL)
C
                  CBAR=(SIBAR*(PXTRAX(3,I)**2))
                  ACBAR=(C1BAR*((PXTRAX(5,I)**2)*(PXTRAX(1,I)**2)))
                  XMAB3(8,I)=CBAR+ACBAR
                  IF(DABS(XMAB3(8,I)).LT.1.0D-15) XMAB3(8,I)=0.0D0
C
C       THIRD ORDER DISTORTION (DIST3)
C
                  EBAR=(SI*PXTRAX(3,I)*PXTRAX(7,I))-
     1            (INV*(K-1.0D0)*PXTRAX(3,I)*(PXTRAX(2,I)+PXTRAX(2,(I-1))))
                  AEBAR=(C1BAR*PXTRAX(5,I)*(PXTRAX(1,I)**3))
                  XMAB3(9,I)=EBAR+AEBAR
                  IF(DABS(XMAB3(9,I)).LT.1.0D-15) XMAB3(9,I)=0.0D0
C
C       THIRD ORDER PETZVAL SUM
C
                  XMAB3(10,I)=XMAB3(5,I)
C       WI
                  WI=0.125D0*(((PXTRAX(3,I)**2)+(PXTRAX(4,I)**2)+
     1            (PXTRAX(2,I)**2)-(3.0D0*((PXTRAX(2,(I-1)))**2))))
C       X73
                  X73=((3.0D0*PXTRAX(3,I)*PXTRAX(4,I))+
     1            (2.0D0*(PXTRAX(2,I)**2))-(3.0D0*((PXTRAX(2,(I-1)))**2)))
C       X74
                  X74=(3.0D0*PXTRAX(3,I)*PXTRAX(8,I))+
     1            (2.0D0*PXTRAX(2,I)*PXTRAX(6,I))-
     2            (3.0D0*PXTRAX(2,(I-1))*PXTRAX(6,(I-1)))
C       X75
                  X75=((3.0D0*PXTRAX(7,I)*PXTRAX(8,I))+
     1            (2.0D0*((PXTRAX(6,I))**2))-(3.0D0*((PXTRAX(6,(I-1)))**2)))
C       X76
                  X76=(PXTRAX(3,I)*((3.0D0*PXTRAX(2,(I-1)))-PXTRAX(2,I)))
C       X77
                  X77=PXTRAX(7,I)*((2.0D0*PXTRAX(2,(I-1)))-PXTRAX(2,I))+
     1            (PXTRAX(3,I)*PXTRAX(6,(I-1)))
C       X78
                  X78=PXTRAX(7,I)*((3.0D0*PXTRAX(6,(I-1)))-PXTRAX(6,I))
C       X42
                  X42=(((PXTRAX(5,I)*PXTRAX(3,I))*(PXTRAX(7,I)-PXTRAX(6,(I-1))))+
     1            ((PXTRAX(1,I)*PXTRAX(7,I))*(PXTRAX(6,I)+PXTRAX(6,(I-1)))))
C       X82
                  X82=(((PXTRAX(5,I)*PXTRAX(2,(I-1)))*
     1            (PXTRAX(7,I)-PXTRAX(6,(I-1))))-
     2            ((PXTRAX(1,I)*PXTRAX(8,I))*(PXTRAX(6,I)+PXTRAX(6,(I-1)))))
C       XBAR42
                  XBAR42=(((PXTRAX(1,I)*PXTRAX(7,I))*
     1            (PXTRAX(3,I)-PXTRAX(2,(I-1))))+
     1            ((PXTRAX(5,I)*PXTRAX(3,I))*(PXTRAX(2,I)+PXTRAX(2,(I-1)))))
C       XBAR82
                  XBAR82=(((PXTRAX(1,I)*PXTRAX(6,(I-1)))*
     1            (PXTRAX(3,I)-PXTRAX(2,(I-1))))-
     2            ((PXTRAX(5,I)*PXTRAX(4,I))*(PXTRAX(2,I)+PXTRAX(2,(I-1)))))
C
C       NOW FOR THE ADDITIONAL TERMS FOR THE 5TH ORDER ABERRATIONS
C       AND SEVENTH ORDER SPHERICAL
C
C       THE S(HAT) TERMS USED FOR THE SHERICAL PART OF THE SURFACE
C       CONTRIBUTIONS
C
C       S1P2
                  S1P2=3.0D0*WI*SI*PXTRAX(3,I)
C       S2P
                  S2P=(0.25D0*SI)*((PXTRAX(7,I)*X73)+(PXTRAX(3,I)*X74)-
     1            (PXTRAX(6,I)*X76)-(PXTRAX(2,I)*X77))
C       S3P2
                  S3P2=(0.25D0*N*(K-1.0D0))*((X42*X73)+(X76*X82)+
     1            (PXTRAX(1,I)*(PXTRAX(3,I)+PXTRAX(2,I))*
     2            ((PXTRAX(3,I)*X75)-(PXTRAX(2,I)*X78))))
C       S4P2
                  S4P2=SI*((PXTRAX(7,I)*X74)-(PXTRAX(6,I)*X77))
C       S5P
                  S5P=(0.25D0*N*(K-1.0D0))*
     1            ((X42*X74)+(X77*X82)+
     2            (PXTRAX(1,I)*(PXTRAX(3,I)+PXTRAX(2,I)))*
     3            ((PXTRAX(7,I)*X75)-(PXTRAX(6,I)*X78)))
C       S6P2
                  S6P2=(0.25D0*N*(K-1.0D0))*((X42*X75)+(X78*X82))
C       S1Q2
                  S1Q2=(0.25D0*N*(K-1.0D0))*((XBAR42*X73)+(X76*XBAR82))

C
C       NOW CALCULATE L,LBAR, AND M
C
                  L=0.25D0*((3.0D0*PXTRAX(4,I))+(2.0D0*((2.0D0*K)
     1            -1.0D0)*PXTRAX(2,I)))
                  LBAR=0.25D0*((3.0D0*PXTRAX(8,I))+(2.0D0*((2.0D0*K)
     1            -1.0D0)*PXTRAX(6,I)))
                  M=(K*INV)/J_NP
C
C       NOW THE ASPHERIC S(HAT) TERMS
C
C       A2S1P
                  A2S1P=C1BAR*(PXTRAX(1,I)**4)*L
C       AS2P
                  AS2P=(2.0D0*AF*L)+(0.5D0*C1BAR*M*(PXTRAX(1,I)**3))
C       A2S3P
                  A2S3P=(2.0D0*AC*L)+(C1BAR*M*PXTRAX(5,I)*(PXTRAX(1,I)**2))
C       A2S4P
                  A2S4P=2.0D0*A2S3P
C       AS5P
                  AS5P=(2.0D0*AE*L)+((3.0D0/2.0D0)*M*PXTRAX(1,I)*
     1            C1BAR*(PXTRAX(5,I)**2))
                  A2S6P=(ABBAR*L)+(C1BAR*M*(PXTRAX(5,I)**3))
C       AS1Q
                  AS1Q=(AB*LBAR)-(C1BAR*M*(PXTRAX(1,I)**3))
C       C
C       NOW JO,JOBAR,ALPHA,BETA,GAMMA,LAMBDA,MU,NU
C
C       CHECK FOR Y-TORICS
                  IF(ALENS(23,I).NE.1.0D0) THEN
C
                      JO=(C2BAR*PXTRAX(1,I))-(0.25D0*CV*C1BAR*
     1                ((3.0D0*PXTRAX(4,I))-(5.0D0*PXTRAX(2,I))))
                      JOBAR=(C2BAR*PXTRAX(5,I))-(0.25D0*CV*C1BAR*
     1                ((3.0D0*PXTRAX(8,I))-(5.0D0*PXTRAX(6,I))))
C
                  ELSE
C       Y-TORIC PRESENT
                      JO=(C2BAR*PXTRAX(1,I))-(0.25D0*ALENS(24,I)*C1BAR*
     1                ((3.0D0*PXTRAX(4,I))-(5.0D0*PXTRAX(2,I))))
                      JOBAR=(C2BAR*PXTRAX(5,I))-(0.25D0*ALENS(24,I)*C1BAR*
     1                ((3.0D0*PXTRAX(8,I))-(5.0D0*PXTRAX(6,I))))
                  END IF
C
                  ALPHA=(0.5D0*((PXTRAX(2,I)**2)+(3.0D0*
     1            PXTRAX(3,I)*PXTRAX(4,I))-
     1            (2.0D0*PXTRAX(3,I)*PXTRAX(2,I))))
C
                  BETA=(PXTRAX(2,I)*PXTRAX(6,I))+(3.0D0*PXTRAX(3,I)
     1            *PXTRAX(8,I))-
     1            (PXTRAX(3,I)*PXTRAX(6,I))-(PXTRAX(7,I)*PXTRAX(2,I))
C
                  GAMMA=0.5D0*((PXTRAX(6,I)**2)+(3.0D0*PXTRAX(7,I)
     1            *PXTRAX(8,I))-
     1            (2.0D0*PXTRAX(7,I)*PXTRAX(6,I)))
C
                  LAMBDA=(ALPHA*C1BAR)+(JO*PXTRAX(1,I))
C
                  MU=(BETA*C1BAR)+(2.0D0*JO*PXTRAX(5,I))
C
                  NU=(PXTRAX(1,I)*GAMMA*C1BAR)+(JO*(PXTRAX(5,I)**2))
C
C       NOW FOR THE (U) TERMS
C
C       A2S1PU
                  A2S1PU=(PXTRAX(1,I)**3)*LAMBDA
C       AS2PU
                  AS2PU=((PXTRAX(1,I)**2)*PXTRAX(5,I)*LAMBDA)+
     1            (0.5D0*MU*(PXTRAX(1,I)**3))
C       A2S3PU
                  A2S3PU=(PXTRAX(1,I)*(PXTRAX(5,I)**2)*LAMBDA)+
     1            ((PXTRAX(1,I)**2)*NU)
C       A2S4PU
                  A2S4PU=(2.0D0*MU*PXTRAX(5,I)*(PXTRAX(1,I)**2))
C       AS5PU
                  AS5PU=(0.5D0*MU*PXTRAX(1,I)*(PXTRAX(5,I)**2))+
     1            (PXTRAX(1,I)*PXTRAX(5,I)*NU)
C       A2S6PU
                  A2S6PU=(PXTRAX(5,I)**2)*NU
C       A2S1QU
                  A2S1QU=(PXTRAX(1,I)**2)*((ALPHA*C1BAR*PXTRAX(5,I))+
     1            (JOBAR*(PXTRAX(1,I)**2)))
C
C
C       5TH ORDER INTRINSIC SURFACE CONTRIBUTIONS
C       SPHERICAL AND ASPHERIC
C
C       B5SLASH+AB5SLASH
C
                  SLH=PXTRAX(3,I)*S1P2
                  ASLH=(PXTRAX(3,I)*A2S1P)+(PXTRAX(1,I)*A2S1PU)
                  XMAB57(1,I)=SLH+ASLH
C       WE NEED TO KEEP A PERMANENT COPY FOR SA7 CALC
C       KEEP IT IN XMAB57(15,I)
                  XMAB57(15,I)=XMAB57(1,I)
C
C       F1SLASH+AF1SLASH
C
                  SLH=(PXTRAX(7,I)*S1P2)+(PXTRAX(3,I)*S2P)
                  ASLH=(PXTRAX(7,I)*A2S1P)+(PXTRAX(5,I)*A2S1PU)+
     1            (PXTRAX(3,I)*AS2P)+(PXTRAX(1,I)*AS2PU)
                  XMAB57(2,I)=SLH+ASLH
C       WE NEED TO KEEP A PERMANENT COPY FOR SA7 CALC
C       KEEP IT IN XMAB57(16,I)
                  XMAB57(16,I)=XMAB57(2,I)
C
C       F2SLASH+AF2SLASH
C
                  SLH=PXTRAX(3,I)*S2P
                  ASLH=(PXTRAX(3,I)*AS2P)+(PXTRAX(1,I)*AS2PU)
                  XMAB57(3,I)=SLH+ASLH
C       WE NEED TO KEEP A PERMANENT COPY FOR SA7 CALC
C       KEEP IT IN XMAB57(17,I)
                  XMAB57(17,I)=XMAB57(3,I)
C
C       M1SLASH+AM1SLASH
C
                  SLH=(2.0D0*PXTRAX(7,I)*S2P)
                  ASLH=2.0D0*((PXTRAX(7,I)*AS2P)+(PXTRAX(5,I)*AS2PU))
                  XMAB57(4,I)=SLH+ASLH
C
C       M2SLASH+AM2SLASH
C
                  SLH=PXTRAX(3,I)*S3P2
                  ASLH=(PXTRAX(3,I)*A2S3P)+(PXTRAX(1,I)*A2S3PU)
                  XMAB57(5,I)=SLH+ASLH
C
C       M3SLASH+AM3SLASH
C
                  SLH=PXTRAX(3,I)*S4P2
                  ASLH=(PXTRAX(3,I)*A2S4P)+(PXTRAX(1,I)*A2S4PU)
                  XMAB57(6,I)=SLH+ASLH
C
C       N1SLASH+AN1SLASH
C
                  SLH=PXTRAX(7,I)*S3P2
                  ASLH=(PXTRAX(7,I)*A2S3P)+(PXTRAX(5,I)*A2S3PU)
                  XMAB57(7,I)=SLH+ASLH
C
C       N2SLASH+AN2SLASH
C
                  SLH=(PXTRAX(7,I)*S4P2)+(2.0D0*PXTRAX(3,I)*S5P)
                  ASLH=(PXTRAX(7,I)*A2S4P)+(PXTRAX(5,I)*A2S4PU)+
     1            2.0D0*((PXTRAX(3,I)*AS5P)+(PXTRAX(1,I)*AS5PU))
                  XMAB57(8,I)=SLH+ASLH
C
C       N3SLASH+AN3SLASH
C
                  SLH=PXTRAX(3,I)*S5P
                  ASLH=(PXTRAX(3,I)*AS5P)+(PXTRAX(1,I)*AS5PU)
                  XMAB57(9,I)=SLH+ASLH
C
C       C5SLASH+AC5SLASH
C
                  SLH=0.5D0*(PXTRAX(7,I)*S5P)
                  ASLH=0.5D0*((PXTRAX(7,I)*AS5P)+(PXTRAX(5,I)*AS5PU))
                  XMAB57(10,I)=SLH+ASLH
C
C       PI5SLASH+API5SLASH
C
                  SLH=(PXTRAX(3,I)*S6P2)-(0.5D0*PXTRAX(7,I)*S5P)
                  ASLH=(PXTRAX(3,I)*A2S6P)+(PXTRAX(1,I)*A2S6PU)
     1            -0.5D0*((PXTRAX(7,I)*AS5P)+(PXTRAX(5,I)*AS5PU))
                  XMAB57(11,I)=SLH+ASLH
C
C       E5SLASH+AE5SLSH
C
                  SLH=PXTRAX(7,I)*S6P2
                  ASLH=(PXTRAX(7,I)*A2S6P)+(PXTRAX(5,I)*A2S6PU)
                  XMAB57(12,I)=SLH+ASLH
C
C       E5BARSLASH+AE5BARSLASH
C
                  SLH=PXTRAX(3,I)*S1Q2
                  ASLH=(PXTRAX(3,I)*AS1Q)+(PXTRAX(1,I)*A2S1QU)
                  XMAB57(13,I)=SLH+ASLH
C
C       AND SEVENTH ORDER SPHERICAL
C
C       B7SLASH+AB7SLASH
C
                  B71=((0.125D0*SI*PXTRAX(3,I))*((2.0D0*PXTRAX(2,I))
     1            -(5.0D0*PXTRAX(2,(I-1)))))/N
                  IF(ALENS(23,I).NE.1.0D0) THEN
C       NO Y-TORIC
                      B72=(10.0D0*(WI**2))+(B71*CV)
                  ELSE
C       Y-TORIC
                      B72=(10.0D0*(WI**2))+(B71*ALENS(24,I))
                  END IF
                  SLH=(SI*(PXTRAX(3,I)**2))*B72
C NOW FOR THE ASPHERIC PART
C
C       GAMMA1
                  GAMMA1=C1*(PXTRAX(1,I)**2)
C       CHECK IF Y-TORIC
                  IF(ALENS(23,I).NE.1.0D0) THEN
                      GAMMA2=(PXTRAX(1,I)**3)*((C2*PXTRAX(1,I))+
     1                (0.25D0*CV*C1*(PXTRAX(3,I)+(3.0D0*
     1                PXTRAX(2,(I-1))))))
                  ELSE
                      GAMMA2=(PXTRAX(1,I)**3)*((C2*PXTRAX(1,I))+
     1                (0.25D0*ALENS(24,I)*C1*(PXTRAX(3,I)+(3.0D0*
     1                PXTRAX(2,(I-1))))))
                  END IF
                  G32=C3*(PXTRAX(1,I)**2)
                  G33=((C2*PXTRAX(1,I)*(PXTRAX(3,I)+
     1            (5.0D0*PXTRAX(2,(I-1)))))/3.0D0)
C       CHECK FOR Y-TORIC
                  IF(ALENS(23,I).NE.1.0D0)THEN
                      G34=CV
                  ELSE
                      G34=ALENS(24,I)
                  END IF
                  G36=(0.125D0*C1)*
     1            ((PXTRAX(3,I)*(PXTRAX(3,I)+(5.0D0*PXTRAX(2,(I-1)))))-
     2            (PXTRAX(2,(I-1))*(PXTRAX(3,I)-(5.0D0*PXTRAX(2,(I-1))))))
                  G37=0.25D0*(C1**2)*PXTRAX(1,I)*PXTRAX(3,I)
                  G31=G32+(G33*G34)+((G34**2)*G36)+G37
                  GAMMA3=(PXTRAX(1,I)**4)*G31
C       NOW GAMMA 1,2 AND 3 ARE DONE
C
C       NOW CALCULATE L3 AND D3
C
C       L3
                  L3=(PXTRAX(1,I)*GAMMA3*(N-J_NP))+0.5D0*
     1            ((GAMMA1*(S1P2+A2S1P))+(GAMMA2*SI*PXTRAX(3,I)))
C
C       D3
                  D32=((C2*(PXTRAX(1,I)**2))/6.0D0)
                  D33=((4.0D0*PXTRAX(4,I))+(3.0D0*PXTRAX(2,I)*
     1            ((2.0D0*K)-1.0D0)))
                  D34=0.125D0*C1
                  D35=GAMMA1*PXTRAX(1,I)*(1.0D0+(2.0D0*K*(K-1.0D0)))
C       CHECK FOR Y-TORIC
                  IF(ALENS(23,I).NE.1.0D0) THEN
                      D36=CV*PXTRAX(1,I)
                      D37=4.0D0*CV*PXTRAX(1,I)*(PXTRAX(3,I)+
     1                PXTRAX(2,(I-1)))
                  ELSE
C       Y-TORIC
                      D36=ALENS(24,I)*PXTRAX(1,I)
                      D37=4.0D0*ALENS(24,I)*PXTRAX(1,I)*(PXTRAX(3,I)+
     1                PXTRAX(2,(I-1)))
                  END IF
                  D38=PXTRAX(4,I)*(5.0D0*((2.0D0*PXTRAX(2,I))+PXTRAX(3,I))
     1            +PXTRAX(4,I))
                  D39=K*PXTRAX(2,I)
                  D40=PXTRAX(4,I)*(4.0D0*((2.0D0*PXTRAX(2,I))+PXTRAX(3,I)+
     1            PXTRAX(4,I))+PXTRAX(3,I))
                  D41=D39*((3.0D0*(PXTRAX(3,I)**2))-(10.0D0
     1            *(PXTRAX(2,(I-1))**2))+
     1            D40)
                  D31=(D32*D33)+D34*(D35+D36*(D37+D38)+D41)
                  D3=(N-J_NP)*(PXTRAX(1,I)**4)*D31
C
C       NOW L3 AND D3 ARE DONE
C
C       ASLH
                  ASLH=(PXTRAX(3,I)*D3)+(PXTRAX(1,I)*L3)
C
                  XMAB57(14,I)=SLH+ASLH
 15           CONTINUE
C
C       BEFORE CALCULATING THE TRANSFERED PLUS SURFACE CONTIBUTIONS,
C       STORE THE SURFACE CONTRIBUTIONS IN THE ARRAY XSAB57
C       THIRD ORDER IS ALWAYS JUST SURFACE CONTRIBUTIONS BY DEFAULT.
C
              I=INT(SYSTEM1(20))
              XSAB57(1:20,0:I)=XMAB57(1:20,0:I)
C       NOW THE SURFACE CONTRIBUTIONS ARE AVAILABLE LATER.
C
C
C       NOW ALL OF THE INTRINSIC SURFACE CONTRIBUTIONS HAVE BEEN
C       CALCULATED
C       NOW CALCULATE THE COMPLETE ABERRATION COEFICIENTS
C       FOR EACH SURFACE
C
C       THIRD ORDER ALREADY ARE COMPLETE AS THEY HAVE NO TRANSFERRED
C       PART
C       FIFTH ORDER, START AT SURF 1 AS ALL SURFACE 0 COEFFS ARE
C       ALWAYS ZERO
C       SET ALL PRIMED VARIABLES TO 0.0
              BPRIME=0.0D0
              FPRIME=0.0D0
              CPRIME=0.0D0
              EPRIME=0.0D0
              PPRIME=0.0D0
              BBPRIM=0.0D0
              FBPRIM=0.0D0
              CBPRIM=0.0D0
              EBPRIM=0.0D0
              PPRIME=0.0D0
              B5PRIM=0.0D0
              EB5PRM=0.0D0
              DO 105 I=1,INT(SYSTEM1(20))
C       AT SURFACE I
C       CALCULATE BPRIME,FPRIME,CPRIME,EPRIME,PPRIME, ETC.
C
C       NOTE: ALL SLASH TERMS NOW INCLUDE THE ASPHERIC
C       CONTRIBUTION.
C
                  BPRIME=BPRIME+XMAB3(1,(I-1))
                  FPRIME=FPRIME+XMAB3(2,(I-1))
                  CPRIME=CPRIME+XMAB3(3,(I-1))
                  EPRIME=EPRIME+XMAB3(4,(I-1))
                  PPRIME=PPRIME+XMAB3(5,(I-1))
                  BBPRIM=BBPRIM+XMAB3(6,(I-1))
                  FBPRIM=FBPRIM+XMAB3(7,(I-1))
                  CBPRIM=CBPRIM+XMAB3(8,(I-1))
                  EBPRIM=EBPRIM+XMAB3(9,(I-1))
                  PBPRIM=PBPRIM+XMAB3(10,(I-1))
                  B5PRIM=B5PRIM+XMAB57(1,(I-1))
                  EB5PRM=EB5PRM+XMAB57(13,(I-1))

C       B5=B5SLASH+(3/2INV)*(BPRIME*F-EBARPRIME*B)
C       B5SLASH IS IN XMAB57(1,I)
C       WHEN DONE B5 WILL REPLACE B5SLASH
                  TRAN=(BPRIME*XMAB3(2,I))-(EBPRIM*XMAB3(1,I))
                  XMAB57(1,I)=XMAB57(1,I)+((3.0D0/(2.0D0*INV))*TRAN)
                  IF(DABS(XMAB57(1,I)).LT.1.0D-15) XMAB57(1,I)=0.0D0
C
C       F1=F1SLASH+(1/2INV)*
C       [B'(PI+4C)+(5F'-4EBAR')*F-(2PI'+5CBAR')*B)
C       F1SLASH IS IN XMAB57(2,I)
C       WHEN DONE F1 WILL REPLACE F1SLASH
                  TRAN=(BPRIME*(XMAB3(5,I)+(4.0D0*XMAB3(3,I))))+
     1            (((5.0D0*FPRIME)-(4.0D0*EBPRIM))*XMAB3(2,I))-
     2            (((2.0D0*PPRIME)+(5.0D0*CBPRIM))*XMAB3(1,I))
                  XMAB57(2,I)=XMAB57(2,I)+((1.0D0/(2.0D0*INV))*TRAN)
                  IF(DABS(XMAB57(2,I)).LT.1.0D-15) XMAB57(2,I)=0.0D0
C
C       F2=F2SLASH+(1/2INV)*
C       [B'(PI+2C)+2(2F'-EBAR')*F-(PI'+4CBAR')*B)
C       F2SLASH IS IN XMAB57(3,I)
C       WHEN DONE F2 WILL REPLACE F2SLASH
                  TRAN=(BPRIME*(XMAB3(5,I)+(2.0D0*XMAB3(3,I))))+
     1            (2.0D0*(((2.0D0*FPRIME)-(1.0D0*EBPRIM))*XMAB3(2,I)))-
     2            (((1.0D0*PPRIME)+(4.0D0*CBPRIM))*XMAB3(1,I))
                  XMAB57(3,I)=XMAB57(3,I)+((1.0D0/(2.0D0*INV))*TRAN)
                  IF(DABS(XMAB57(3,I)).LT.1.0D-15) XMAB57(3,I)=0.0D0
C
C       M1=M1SLASH+(1/INV)*
C       [B'E+(4F'-EBAR')C+(C'-4CBAR'-2PI')F-FBAR'B]
C       M1SLASH IS IN XMAB57(4,I)
C       WHEN DONE M1 WILL REPLACE M1SLASH
                  TRAN=(BPRIME*XMAB3(4,I))+
     1            (XMAB3(3,I)*((4.0D0*FPRIME)-EBPRIM))+
     2            ((CPRIME-(4.0D0*CBPRIM)-(2.0D0*PPRIME))*XMAB3(2,I))-
     3            (FBPRIM*XMAB3(1,I))
                  XMAB57(4,I)=XMAB57(4,I)+((1.0D0/INV)*TRAN)
                  IF(DABS(XMAB57(4,I)).LT.1.0D-15) XMAB57(4,I)=0.0D0
C
C       M2=M2SLASH+(1/2*INV)*
C       [B'E+(2F'-EBAR')(PI+C)+(3C'-2CBAR'+PI')F-3FBAR'B]
C       M2SLASH IS IN XMAB57(5,I)
C       WHEN DONE M2 WILL REPLACE M2SLASH
                  TRAN=(BPRIME*XMAB3(4,I))+
     1            ((XMAB3(3,I)+XMAB3(5,I))*((2.0D0*FPRIME)-EBPRIM))+
     2            (((3.0D0*CPRIME)-(2.0D0*CBPRIM)+PPRIME)*XMAB3(2,I))-
     3            (3.0D0*FBPRIM*XMAB3(1,I))
                  XMAB57(5,I)=XMAB57(5,I)+((1.0D0/(2.0D0*INV))*TRAN)
                  IF(DABS(XMAB57(5,I)).LT.1.0D-15) XMAB57(5,I)=0.0D0
C
C       M3=M3SLASH+(2/INV)*
C       [F'(2C+PI)+(C'-2CBAR')F-FBAR'B]
C       M3SLASH IS IN XMAB57(6,I)
C       WHEN DONE M3 WILL REPLACE M3SLASH
                  TRAN=(FPRIME*((2.0D0*XMAB3(3,I))+XMAB3(5,I)))+
     1            ((CPRIME-(2.0D0*CBPRIM))*XMAB3(2,I))-(FBPRIM*XMAB3(1,I))
                  XMAB57(6,I)=XMAB57(6,I)+((2.0D0/(1.0D0*INV))*TRAN)
                  IF(DABS(XMAB57(6,I)).LT.1.0D-15) XMAB57(6,I)=0.0D0
C
C       N1=N1SLASH+(1/2INV)*
C       [3F'E-(PI'+CBAR')(PI+C)+2(C'-CBAR')C+(E'-2FBAR')F-BBAR'*B]
C       N1SLASH IS IN XMAB57(7,I)
C       WHEN DONE N1 WILL REPLACE N1SLASH
                  TRAN=(3.0D0*FPRIME*XMAB3(4,I))-
     1            ((PPRIME+CBPRIM)*(XMAB3(5,I)+XMAB3(3,I)))+
     2            (2.0D0*(CPRIME-CBPRIM)*XMAB3(3,I))+
     3            (((EPRIME-(2.0D0*FBPRIM))*XMAB3(2,I))-(BBPRIM*XMAB3(1,I)))
                  XMAB57(7,I)=XMAB57(7,I)+((1.0D0/(2.0D0*INV))*TRAN)
                  IF(DABS(XMAB57(7,I)).LT.1.0D-15) XMAB57(7,I)=0.0D0
C
C       N2=N2SLASH+(1/INV)*
C       [3F'E+(PI'-CBAR'+3C')(PI+3C)-(C'+PI')C+(E'-8FBAR')F-BBAR'*B]
C       N2SLASH IS IN XMAB57(8,I)
C       WHEN DONE N2 WILL REPLACE N2SLASH
                  TRAN=(3.0D0*FPRIME*XMAB3(4,I))+
     1            ((PPRIME-CBPRIM+(3.0D0*CPRIME))*(XMAB3(5,I)+(3.0D0
     1            *XMAB3(3,I))))-
     2            (1.0D0*(CPRIME+PPRIME)*XMAB3(3,I))+
     3            ((EPRIME-(8.0D0*FBPRIM))*XMAB3(2,I))-(BBPRIM*XMAB3(1,I))
                  XMAB57(8,I)=XMAB57(8,I)+((1.0D0/(1.0D0*INV))*TRAN)
                  IF(DABS(XMAB57(8,I)).LT.1.0D-15) XMAB57(8,I)=0.0D0
C
C       N3=N3SLASH+(1/2INV)*
C       [F'E+(PI'-CBAR'+3C')(PI+C)+(C'+PI')C+(E'-4FBAR')F-BBAR'*B]
C       N3SLASH IS IN XMAB57(9,I)
C       WHEN DONE N3 WILL REPLACE N3SLASH
                  TRAN=(FPRIME*XMAB3(4,I))+
     1            ((PPRIME-CBPRIM+(3.0D0*CPRIME))*(XMAB3(5,I)+(XMAB3(3,I))))+
     2            ((CPRIME+PPRIME)*XMAB3(3,I))+
     3            ((EPRIME-(4.0D0*FBPRIM))*XMAB3(2,I))-(BBPRIM*XMAB3(1,I))
                  XMAB57(9,I)=XMAB57(9,I)+((1.0D0/(2.0D0*INV))*TRAN)
                  IF(DABS(XMAB57(9,I)).LT.1.0D-15) XMAB57(9,I)=0.0D0
C
C       C5=C5SLASH+(1/4INV)*
C       [(4C'+PI')E-FBAR'PI+2(E'-2FBAR')C-2BBAR'F]
C       C5SLASH IS IN XMAB57(10,I)
C       WHEN DONE C5 WILL REPLACE C5SLASH
                  TRAN=(((4.0D0*CPRIME)+PPRIME)*(XMAB3(4,I)))-
     1            (FBPRIM*XMAB3(5,I))+
     2            (2.0D0*(EPRIME-(2.0D0*FBPRIM))*XMAB3(3,I))-
     3            (2.0D0*BBPRIM*XMAB3(2,I))
                  XMAB57(10,I)=XMAB57(10,I)+((1.0D0/(4.0D0*INV))*TRAN)
                  IF(DABS(XMAB57(10,I)).LT.1.0D-15) XMAB57(10,I)=0.0D0
C
C       PI5=PI5SLASH+(1/4INV)*
C       [(PI'-2C')E+(4E'-FBAR')PI+2(E'+FBAR')C-2BBAR'F]
C       PI5SLASH IS IN XMAB57(11,I)
C       WHEN DONE PI5 WILL REPLACE PI5SLASH
                  TRAN=((PPRIME-(2.0D0*CPRIME))*XMAB3(4,I))+
     1            (((4.0D0*EPRIME)-FBPRIM)*XMAB3(5,I))+
     2            (2.0D0*(EPRIME+FBPRIM)*XMAB3(3,I))-(2.0D0
     3            *BBPRIM*XMAB3(2,I))
                  XMAB57(11,I)=XMAB57(11,I)+((1.0D0/(4.0D0*INV))*TRAN)
                  IF(DABS(XMAB57(11,I)).LT.1.0D-15) XMAB57(11,I)=0.0D0
C
C       E5=E5SLASH+(1/2INV)*
C       [3E'E-BBAR'(PI+3C)]
C       E5SLASH IS IN XMAB57(12,I)
C       WHEN DONE E5 WILL REPLACE E5SLASH
                  TRAN=(3.0D0*EPRIME*XMAB3(4,I))-
     1            (BBPRIM*(XMAB3(5,I)+(3.0D0*XMAB3(3,I))))
                  XMAB57(12,I)=XMAB57(12,I)+((1.0D0/(2.0D0*INV))*TRAN)
                  IF(DABS(XMAB57(12,I)).LT.1.0D-15) XMAB57(12,I)=0.0D0
C
C       E5BAR=E5BARSLASH+(1/2INV)*
C       [B'(PI+3CBAR)-3EBAR'EBAR)]
C       E5BARSLASH IS IN XMAB57(13,I)
C       WHEN DONE E5BAR WILL REPLACE E5BARSLASH
                  TRAN=(BPRIME*(XMAB3(5,I)+(3.0D0*XMAB3(8,I))))-
     1            (3.0D0*EBPRIM*XMAB3(9,I))
                  XMAB57(13,I)=XMAB57(13,I)+((1.0D0/(2.0D0*INV))*TRAN)
                  IF(DABS(XMAB57(13,I)).LT.1.0D-15) XMAB57(13,I)=0.0D0
C       NOW SEVENTH ORDER SPHERICAL
C
C       NOW THE SEVENTH ORDER ABERATION FOR SURFACE I
C       IS:
C       B7=B7SLASH+(1/2INV)*
C       [(1/2INV)*(B'**2)(PI+3C)+3(B5'-(1/INV)B'EBAR')F+
C       3((1/2INV)(EBAR'**2)-EBAR5')B+
C       B'(F1SLH+F2SLH)-
C       5EBAR'B5SLH]
                  TRAN1=((0.5D0/INV)*(BPRIME**2)*(XMAB3(5,I)+
     1            (3.0D0*XMAB3(3,I))))
                  TRAN2=(3.0D0*(B5PRIM-((1.0D0/INV)*BPRIME*EBPRIM))
     1            *XMAB3(2,I))
                  TRAN4=(3.0D0*(((0.5D0/INV)*
     1            (EBPRIM**2))-EB5PRM)*XMAB3(1,I))
                  TRAN5=(BPRIME*(XMAB57(16,I)+XMAB57(17,I)))
                  TRAN6=(5.0D0*EBPRIM*XMAB57(15,I))
                  TRAN=TRAN1+TRAN2+TRAN4+TRAN5-TRAN6
                  XMAB57(14,I)=XMAB57(14,I)+((0.5D0/INV)*TRAN)
                  IF(DABS(XMAB57(14,I)).LT.1.0D-15) XMAB57(14,I)=0.0D0
 105          CONTINUE
C
              RETURN
          ELSE
C       IAB NOT 2
          END IF
          RETURN
      END
C SUB A357I.FOR
      SUBROUTINE A357I
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE A357I. THIS SUBROUTINE IMPLEMENTS
C       THE SA357I AND SXA357I (INTRINSIC SURFACE
C       COEFFICIENTS AND THEIR SUMS) CMD LEVEL COMMAND
C
C       THE SUMS FOR 5TH AND 7TH ORDER DO'NT RELATE TO
C       REAL ABERRATIONS WHICH HAVE A TRANSFERED PART.
C
          INTEGERSF,CW,I
C
          REAL*8 C1,C2,C3
C
          REAL*8 INV,
     1    C1T,C2T,C3T
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
C
C       THE SA357I AND XSA357I COMMAND ACCEPTS QUALIFIER OR NUMERIC
C       INPUT IN AN EITHER OR MODE. IT DOES NOT ACCEPT STING
C       INPUT. THE VALID QUALIFIERS ARE "ALL" AND "OBJ" AND "OB"
C       "I","IM", AND "IMAGE"
C       THE VALID NUMERIC INPUT IS THE SURFACE NUMBER FOR
C       WHICH IT IS DESIRED TO PRODUCE OUTPUT.
C
          CALL PRTRC
C
          IF(SQ.EQ.1.AND.S1.EQ.1) THEN
              IF(WC.EQ.'SA357I') THEN
                  OUTLYNE='"SA357I" TAKES EITHER QUALIFIER OR NUMERIC INPUT"'
                  CALL SHOWIT(1)
                  OUTLYNE='BUT NOT BOTH'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(WC.EQ.'XSA357I') THEN
                  OUTLYNE=
     1            '"XSA357I" TAKES EITHER QUALIFIER OR NUMERIC INPUT"'
                  CALL SHOWIT(1)
                  OUTLYNE='BUT NOT BOTH'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
          IF(SST.EQ.1.OR.S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
              IF(WC.EQ.'SA357I') THEN
                  OUTLYNE=
     1            '"SA357I" TAKES NO STRING OR NUMERIC WORD #2 THROUGH #5 INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(WC.EQ.'XSA357I') THEN
                  OUTLYNE=
     1            '"XSA357I" TAKES NO STRING OR NUMERIC WORD #2 THROUGH #5 INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
          IF(SYSTEM1(20).EQ.0.0) THEN
              OUTLYNE='LENS SYSTEM HAS NO SURFACES'
              CALL SHOWIT(1)
              OUTLYNE='NO PARAXIAL,3RD, 5TH OR 7TH  ORDER DATA EXISTS'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
          SF=INT(SYSTEM1(20))
          IF(INT(SYSTEM1(11)).GE.1.AND.INT(SYSTEM1(11)).LE.5) THEN
              CW=INT(SYSTEM1(11))+45
          END IF
          IF(INT(SYSTEM1(11)).GE.6.AND.INT(SYSTEM1(11)).LE.10) THEN
              CW=INT(SYSTEM1(11))+65
          END IF
          INV=1.0D0
          IF(SYSTEM1(30).EQ.1.0D0) THEN
C       MODE IS FOCAL
              IF(WC.EQ.'SA357I')
     1        INV=-2.0*ALENS(CW,(SF-1))*PXTRAY(2,(SF-1))
              IF(WC.EQ.'XSA357I')
     1        INV=-2.0*ALENS(CW,(SF-1))*PXTRAX(2,(SF-1))
              IF(DABS(INV).LE.1.0D-10) THEN
                  CALL MACFAL
                  RETURN
              END IF
          ELSE
          END IF
          IF(SYSTEM1(30).EQ.3.0D0) THEN
C       MODE IS AFOCAL
              IF(WC.EQ.'SA357I')
     1        INV= 2.0*ALENS(CW,(SF-1))*PXTRAY(1,SF)
              IF(WC.EQ.'XSA357I')
     1        INV= 2.0*ALENS(CW,(SF-1))*PXTRAX(1,SF)
              IF(DABS(INV).LE.1.0D-10) THEN
                  CALL MACFAL
                  RETURN
              END IF
          ELSE
          END IF
          IF(INV.EQ.0.0D0) THEN
              OUTLYNE=
     1        'THE LENS (MODE) IS NOT CONSISTENT WITH PARAXIAL VALUES'
              CALL SHOWIT(1)
              OUTLYNE=
     1        'ABERRATIONS ARE NOT CALCULABLE'
              CALL SHOWIT(1)
              IF(SYSTEM1(30).EQ.1.0D0)
     1        OUTLYNE='CHANGE FROM "MODE FOCAL" TO "MODE AFOCAL"'
              IF(SYSTEM1(30).EQ.3.0D0)
     1        OUTLYNE='CHANGE FROM "MODE AFOCAL" TO "MODE FOCAL"'
              CALL SHOWIT(1)
              OUTLYNE='THEN RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SQ.EQ.1.AND.WQ.EQ.'ALL') THEN
              SF=INT(SYSTEM1(20))
              IF(WC.EQ.'SA357I')WRITE(OUTLYNE,5001) INT(F12)
              IF(WC.EQ.'XSA357I')WRITE(OUTLYNE,6001) INT(F12)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,4999)
              CALL SHOWIT(0)
              IF(SYSTEM1(30).EQ.1.0) WRITE(OUTLYNE,5501)
              IF(SYSTEM1(30).EQ.2.0) WRITE(OUTLYNE,5502)
              IF(SYSTEM1(30).EQ.3.0) WRITE(OUTLYNE,5503)
              IF(SYSTEM1(30).EQ.4.0) WRITE(OUTLYNE,5504)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,2501)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,5000)
              CALL SHOWIT(0)
              DO 10 I=0,SF
                  IF(SYSTEM1(30).EQ.1.0.OR.
     1                    SYSTEM1(30).EQ.3.0) THEN
C               FOCAL OR AFOCAL, CONVERT
                      IF(WC.EQ.'SA357I') THEN
                          C1=MAB3(1,I)/INV
                          C2=SAB57(1,I)/INV
                          C3=SAB57(14,I)/INV
                      ELSE
                      END IF
                      IF(WC.EQ.'XSA357I') THEN
                          C1=XMAB3(1,I)/INV
                          C2=XSAB57(1,I)/INV
                          C3=XSAB57(14,I)/INV
                      ELSE
                      END IF
                      WRITE(OUTLYNE,2000)I,C1,C2,C3
                      CALL SHOWIT(0)
                      GO TO 10
                  ELSE
                  END IF
                  IF(SYSTEM1(30).EQ.2.0.OR.
     1                    SYSTEM1(30).EQ.4.0) THEN
C               UFOCAL OR UAFOCAL, DON'T CONVERT
                      IF(WC.EQ.'SA357I') THEN
                          C1=MAB3(1,I)
                          C2=SAB57(1,I)
                          C3=SAB57(14,I)
                      ELSE
                      END IF
                      IF(WC.EQ.'XSA357I') THEN
                          C1=XMAB3(1,I)
                          C2=XSAB57(1,I)
                          C3=XSAB57(14,I)
                      ELSE
                      END IF
                      WRITE(OUTLYNE,2000)I,C1,C2,C3
                      CALL SHOWIT(0)
                  ELSE
                  END IF
 10           CONTINUE
C       NOW HANDEL THE CALCULATION AND PRINTING OF THE
C       TOTALS
C       IN ANY CASE, THE SURFACE CONTRIBUTIONS MUST BE SUMMED
              C1T=0.0
              C2T=0.0
              C3T=0.0
              DO 20 I=0,SF
                  IF(WC.EQ.'SA357I') THEN
                      C1T=C1T+MAB3(1,I)
                      C2T=C2T+SAB57(1,I)
                      C3T=C3T+SAB57(14,I)
                  ELSE
                  END IF
                  IF(WC.EQ.'XSA357I') THEN
                      C1T=C1T+XMAB3(1,I)
                      C2T=C2T+XSAB57(1,I)
                      C3T=C3T+XSAB57(14,I)
                  ELSE
                  END IF
 20           CONTINUE
C       NOW FOR CONVERSIONS
C       MODE UFOCAL
              IF(SYSTEM1(30).EQ.2.0) THEN
C       MODE IS UFOCAL, NO CONVERSIONS
                  GO TO 25
              ELSE
              END IF
C       MODE UAFOCAL
              IF(SYSTEM1(30).EQ.4.0) THEN
C       MODE IS UAFOCAL, NO CONVERSIONS
                  GO TO 25
              ELSE
              END IF
C       MODE FOCAL
              IF(SYSTEM1(30).EQ.1.0.OR.SYSTEM1(30).EQ.3.0) THEN
C       MODE IS FOCAL OR AFOCAL, CONVERT SUMS
C       CHROMATIC ABERRATION
                  C1T=C1T/INV
                  C2T=C2T/INV
                  C3T=C3T/INV
                  GO TO 25
              ELSE
C       NO CONVERSION REQUIRED
              END IF
 25           CONTINUE
C       PRINT TOTALS WITH LABELING
              WRITE(OUTLYNE,998)
              CALL SHOWIT(0)
              C1=C1T
              C2=C2T
              C3=C3T
              WRITE(OUTLYNE,2001) C1,C2,C3
              CALL SHOWIT(0)
              RETURN
          ELSE
C       QUALIFIER NOT "ALL"
          END IF
          IF(SQ.EQ.1.AND.WQ.EQ.'OBJ'.OR.SQ.EQ.1.AND.WQ.EQ.'OB'.OR.
     1    SQ.EQ.1.AND.WQ.EQ.'I'.OR.SQ.EQ.1.AND.WQ.EQ.'IM'.OR.
     2    SQ.EQ.1.AND.WQ.EQ.'IMAGE') THEN
              IF(WQ.EQ.'OB'.OR.WQ.EQ.'OBJ') SF=0
              IF(WQ.EQ.'I'.OR.WQ.EQ.'IM'.OR.WQ.EQ.'IMAGE')
     1        SF=INT(SYSTEM1(20))
              IF(SYSTEM1(30).EQ.1.0.OR.
     1                SYSTEM1(30).EQ.3.0) THEN
C               FOCAL OR AFOCAL, CONVERT
                  IF(WC.EQ.'SA357I') THEN
                      C1=MAB3(1,SF)/INV
                      C2=SAB57(1,SF)/INV
                      C3=SAB57(14,SF)/INV
                  ELSE
                  END IF
                  IF(WC.EQ.'XSA357I') THEN
                      C1=XMAB3(1,SF)/INV
                      C2=XSAB57(1,SF)/INV
                      C3=XSAB57(14,SF)/INV
                  ELSE
                  END IF
                  IF(HEADIN) WRITE(OUTLYNE,5000)
                  IF(HEADIN) CALL SHOWIT(0)
                  WRITE(OUTLYNE,2000)SF,C1,C2,C3
                  CALL SHOWIT(0)
                  RETURN
              ELSE
              END IF
              IF(SYSTEM1(30).EQ.2.0.OR.
     1                SYSTEM1(30).EQ.4.0) THEN
C               UFOCAL OR UAFOCAL, DON'T CONVERT
                  IF(WC.EQ.'SA357I') THEN
                      C1=MAB3(1,SF)
                      C2=SAB57(1,SF)
                      C3=SAB57(14,SF)
                  ELSE
                  END IF
                  IF(WC.EQ.'XSA357I') THEN
                      C1=XMAB3(1,SF)
                      C2=XSAB57(1,SF)
                      C3=XSAB57(14,SF)
                  ELSE
                  END IF
                  IF(HEADIN) WRITE(OUTLYNE,5000)
                  IF(HEADIN) CALL SHOWIT(0)
                  WRITE(OUTLYNE,2000)SF,C1,C2,C3
                  CALL SHOWIT(0)
                  RETURN
              ELSE
              END IF
          ELSE
          END IF
          IF(SQ.EQ.1.AND.WQ.NE.'OBJ'.OR.SQ.EQ.1.AND.
     1    WQ.NE.'ALL'.OR.SQ.EQ.1.AND.WQ.NE.'OB'.OR.SQ.EQ.1.AND.
     2    WQ.NE.'I'.OR.SQ.EQ.1.AND.WQ.NE.'IM'.OR.SQ.EQ.1.AND.
     3    WQ.NE.'IMAGE') THEN
              OUTLYNE='INVALID QUALIFIER WORD'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SQ.EQ.0.AND.DF1.EQ.1.OR.SQ.EQ.1.AND.WQ.EQ.'IM'.OR.
     1    SQ.EQ.1.AND.WQ.EQ.'I'.OR.SQ.EQ.1.AND.WQ.EQ.'IMAGE') THEN
              SF=INT(SYSTEM1(20))
C       OUTPUT SYSTEM TOTALS
C
C       THE SURFACE CONTRIBUTIONS MUST BE SUMMED
              C1T=0.0
              C2T=0.0
              C3T=0.0
              DO 220 I=0,SF
                  IF(WC.EQ.'SA357I') THEN
                      C1T=C1T+MAB3(1,I)
                      C2T=C2T+SAB57(1,I)
                      C3T=C3T+SAB57(14,I)
                  ELSE
                  END IF
                  IF(WC.EQ.'XSA357I') THEN
                      C1T=C1T+XMAB3(1,I)
                      C2T=C2T+XSAB57(1,I)
                      C3T=C3T+XSAB57(14,I)
                  ELSE
                  END IF
 220          CONTINUE
C NOW FOR CONVERSIONS
C       MODE UFOCAL
              IF(SYSTEM1(30).EQ.2.0) THEN
C       MODE IS UFOCAL, NO CONVERSIONS
                  GO TO 250
              ELSE
              END IF
C       MODE UAFOCAL
              IF(SYSTEM1(30).EQ.4.0) THEN
C       MODE IS UAFOCAL, NO CONVERSIONS
                  GO TO 250
              ELSE
              END IF
C       MODE FOCAL
              IF(SYSTEM1(30).EQ.1.0.OR.SYSTEM1(30).EQ.3.0) THEN
C       MODE IS FOCAL OR AFOCAL, CONVERT SUMS TO TRANSVERSE
C       CHROMATIC ABERRATION
                  C1T=C1T/INV
                  C2T=C2T/INV
                  C3T=C3T/INV
                  GO TO 250
              ELSE
              END IF
 250          CONTINUE
              C1=C1T
              C2=C2T
              C3=C3T
C       PRINT TOTALS WITH OUT LABELING
              IF(HEADIN) WRITE(OUTLYNE,5000)
              IF(HEADIN) CALL SHOWIT(0)
              WRITE(OUTLYNE,2001) C1,C2,C3
              CALL SHOWIT(0)
              RETURN
          ELSE
C       NOT IMAGE SURFACE
          END IF
C
          IF(SQ.EQ.0.AND.DF1.NE.1) THEN
              I=INT(W1)
              SF=INT(SYSTEM1(20))
              IF(I.GT.SF.OR.I.LT.0) THEN
                  OUTLYNE='SURFACE NUMBER BEYOND LEGAL RANGE'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(SYSTEM1(30).EQ.1.0.OR.
     1                SYSTEM1(30).EQ.3.0) THEN
C               FOCAL OR AFOCAL, CONVERT
                  IF(WC.EQ.'SA357I') THEN
                      C1=MAB3(1,I)/INV
                      C2=SAB57(1,I)/INV
                      C3=SAB57(14,I)/INV
                  ELSE
                  END IF
                  IF(WC.EQ.'XSA357I') THEN
                      C1=XMAB3(1,I)/INV
                      C2=XSAB57(1,I)/INV
                      C3=XSAB57(14,I)/INV
                  ELSE
                  END IF
                  IF(HEADIN) WRITE(OUTLYNE,5000)
                  IF(HEADIN) CALL SHOWIT(0)
                  WRITE(OUTLYNE,2000)I,C1,C2,C3
                  CALL SHOWIT(0)
                  RETURN
              ELSE
              END IF
              IF(SYSTEM1(30).EQ.2.0.OR.
     1                SYSTEM1(30).EQ.4.0) THEN
C               UFOCAL OR UAFOCAL, DON'T CONVERT
                  IF(WC.EQ.'SA357I') THEN
                      C1=MAB3(1,I)
                      C2=SAB57(1,I)
                      C3=SAB57(14,I)
                  ELSE
                  END IF
                  IF(WC.EQ.'XSA357I') THEN
                      C1=XMAB3(1,I)
                      C2=XSAB57(1,I)
                      C3=XSAB57(14,I)
                  ELSE
                  END IF
                  IF(HEADIN) WRITE(OUTLYNE,5000)
                  IF(HEADIN) CALL SHOWIT(0)
                  WRITE(OUTLYNE,2000)I,C1,C2,C3
                  CALL SHOWIT(0)
              ELSE
              END IF
              RETURN
          ELSE
          END IF
! 1500   FORMAT(I3,2X,G12.5,2X,G12.5,2X,G12.5)
 2000     FORMAT(I3,2X,G12.5,2X,G12.5,2X,G12.5)
 2001     FORMAT(G12.5,2X,G12.5,2X,G12.5)
 5000     FORMAT('SURF',5X,'SA3 ',10X,'SA5 ',10X,'SA7')
 5001     FORMAT(
     1    '(Y-Z) PLANE SPHERICAL ABERRATION (3RD,5TH,7TH) CONTRIBUTIONS'
     1    ,' - (CFG #',I2,')')
 6001     FORMAT(
     1    '(X-Z) PLANE SPHERICAL ABERRATION (3RD,5TH,7TH) CONTRIBUTIONS'
     1    ,' - (CFG #',I2,')')
 4999     FORMAT(
     1    'WARNING: THESE ARE THE (INTRINSIC) SURFACE TERMS')
 5501     FORMAT('TRANSVERSE - WITH FINAL SURFACE CONVERSION')
 5502     FORMAT('TRANSVERSE - WITHOUT FINAL SURFACE CONVERSION')
 5503     FORMAT('ANGULAR - WITH FINAL SURFACE CONVERSION')
 5504     FORMAT('ANGULAR - WITHOUT FINAL SURFACE CONVERSION')
 2501     FORMAT(1X)
 998      FORMAT('SYSTEM TOTALS ARE:')
      END
C SUB AB5I.FOR
      SUBROUTINE AB5I
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE AB5I. THIS SUBROUTINE IMPLEMENTS
C       THE MAB5I AND XMAB5I CMD LEVEL COMMAND
C
          INTEGER I,SF,CW
C
          REAL*8 C1,C2,C3,C4,C5,C1T,C2T,C3T,C4T,C5T
C
          REAL*8 INV
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
C
          CALL PRTRC
C
C       THE MAB5I AND XMAB5I COMMAND ACCEPTS QUALIFIER OR NUMERIC
C       INPUT IN AN EITHER OR MODE. IT DOES NOT ACCEPT STING
C       INPUT. THE VALID QUALIFIERS ARE "ALL" AND "OBJ" AND "OB"
C       "I","IM", AND "IMAGE"
C       THE VALID NUMERIC INPUT IS THE SURFACE NUMBER FOR
C       WHICH IT IS DESIRED TO PRODUCE OUTPUT.
C
          IF(SQ.EQ.1.AND.S1.EQ.1) THEN
              IF(WC.EQ.'MAB5I') THEN
                  OUTLYNE='"MAB5I" TAKES EITHER QUALIFIER OR NUMERIC INPUT"'
                  CALL SHOWIT(1)
                  OUTLYNE='BUT NOT BOTH'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(WC.EQ.'XMAB5I') THEN
                  OUTLYNE='"XMAB5I" TAKES EITHER QUALIFIER OR NUMERIC INPUT"'
                  CALL SHOWIT(1)
                  OUTLYNE='BUT NOT BOTH'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
          IF(SYSTEM1(20).EQ.0.0) THEN
              OUTLYNE='LENS SYSTEM HAS NO SURFACES'
              CALL SHOWIT(1)
              OUTLYNE='NO PARAXIAL OR FIFTH ORDER DATA EXISTS'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
          SF=INT(SYSTEM1(20))
          IF(INT(SYSTEM1(11)).GE.1.AND.INT(SYSTEM1(11)).LE.5) THEN
              CW=INT(SYSTEM1(11))+45
          END IF
          IF(INT(SYSTEM1(11)).GE.6.AND.INT(SYSTEM1(11)).LE.10) THEN
              CW=INT(SYSTEM1(11))+65
          END IF
          INV=1.0D0
          IF(SYSTEM1(30).EQ.1.0D0) THEN
C       MODE IS FOCAL
              IF(WC.EQ.'MAB5I')
     1        INV=-2.0*ALENS(CW,(SF-1))*PXTRAY(2,(SF-1))
              IF(WC.EQ.'XMAB5I')
     1        INV=-2.0*ALENS(CW,(SF-1))*PXTRAX(2,(SF-1))
              IF(DABS(INV).LE.1.0D-10) THEN
                  CALL MACFAL
                  RETURN
              END IF
          ELSE
          END IF
          IF(SYSTEM1(30).EQ.3.0D0) THEN
C       MODE IS AFOCAL
              IF(WC.EQ.'MAB5I')
     1        INV= 2.0*ALENS(CW,(SF-1))*PXTRAY(1,SF)
              IF(WC.EQ.'XMAB5I')
     1        INV= 2.0*ALENS(CW,(SF-1))*PXTRAX(1,SF)
              IF(DABS(INV).LE.1.0D-10) THEN
                  CALL MACFAL
                  RETURN
              END IF
          ELSE
          END IF
          IF(INV.EQ.0.0D0) THEN
              OUTLYNE=
     1        'THE LENS (MODE) IS NOT CONSISTENT WITH PARAXIAL VALUES'
              CALL SHOWIT(1)
              OUTLYNE=
     1        'ABERRATIONS ARE NOT CALCULABLE'
              CALL SHOWIT(1)
              IF(SYSTEM1(30).EQ.1.0D0)
     1        OUTLYNE='CHANGE FROM "MODE FOCAL" TO "MODE AFOCAL"'
              IF(SYSTEM1(30).EQ.3.0D0)
     1        OUTLYNE='CHANGE FROM "MODE AFOCAL" TO "MODE FOCAL"'
              CALL SHOWIT(1)
              OUTLYNE='THEN RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SQ.EQ.1.AND.WQ.EQ.'ALL') THEN
              SF=INT(SYSTEM1(20))
              WRITE(OUTLYNE,5001) INT(F12)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,4999)
              CALL SHOWIT(0)
              IF(SYSTEM1(30).EQ.1.0) WRITE(OUTLYNE,5501)
              IF(SYSTEM1(30).EQ.2.0) WRITE(OUTLYNE,5502)
              IF(SYSTEM1(30).EQ.3.0) WRITE(OUTLYNE,5503)
              IF(SYSTEM1(30).EQ.4.0) WRITE(OUTLYNE,5504)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,2501)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,5000)
              CALL SHOWIT(0)
              DO 10 I=0,SF
                  IF(SYSTEM1(30).EQ.1.0.OR.
     1                    SYSTEM1(30).EQ.3.0) THEN
C               FOCAL OR AFOCAL, CONVERT
                      IF(WC.EQ.'MAB5I') THEN
                          C1=SAB57(1,I)/INV
                          C2=(SAB57(2,I)+SAB57(3,I))/INV
                          C3=SAB57(10,I)/INV
                          C4=SAB57(12,I)/INV
                          C5=SAB57(11,I)/INV
                      ELSE
                      END IF
                      IF(WC.EQ.'XMAB5I') THEN
                          C1=XSAB57(1,I)/INV
                          C2=(XSAB57(2,I)+XSAB57(3,I))/INV
                          C3=XSAB57(10,I)/INV
                          C4=XSAB57(12,I)/INV
                          C5=XSAB57(11,I)/INV
                      ELSE
                      END IF
                      WRITE(OUTLYNE,2000)I,C1,C2,C3,C4,C5
                      CALL SHOWIT(0)
                      GO TO 10
                  ELSE
                  END IF
                  IF(SYSTEM1(30).EQ.2.0.OR.
     1                    SYSTEM1(30).EQ.4.0) THEN
C               UFOCAL OR UAFOCAL, DON'T CONVERT
                      IF(WC.EQ.'MAB5I') THEN
                          C1=SAB57(1,I)
                          C2=SAB57(2,I)+SAB57(3,I)
                          C3=SAB57(10,I)
                          C4=SAB57(12,I)
                          C5=SAB57(11,I)
                      ELSE
                      END IF
                      IF(WC.EQ.'XMAB5I') THEN
                          C1=XSAB57(1,I)
                          C2=XSAB57(2,I)+XSAB57(3,I)
                          C3=XSAB57(10,I)
                          C4=XSAB57(12,I)
                          C5=XSAB57(11,I)
                      ELSE
                      END IF
                      WRITE(OUTLYNE,2000)I,C1,C2,C3,C4,C5
                      CALL SHOWIT(0)
                  ELSE
                  END IF
 10           CONTINUE
C       NOW HANDEL THE CALCULATION AND PRINTING OF THE
C       TOTALS
C       IN ANY CASE, THE SURFACE CONTRIBUTIONS MUST BE SUMMED
              C1T=0.0
              C2T=0.0
              C3T=0.0
              C4T=0.0
              C5T=0.0
              DO 20 I=0,SF
                  IF(WC.EQ.'MAB5I') THEN
                      C1T=C1T+SAB57(1,I)
                      C2T=C2T+SAB57(2,I)+SAB57(3,I)
                      C3T=C3T+SAB57(10,I)
                      C4T=C4T+SAB57(12,I)
                      C5T=C5T+SAB57(11,I)
                  ELSE
                  END IF
                  IF(WC.EQ.'XMAB5I') THEN
                      C1T=C1T+XSAB57(1,I)
                      C2T=C2T+XSAB57(2,I)+XSAB57(3,I)
                      C3T=C3T+XSAB57(10,I)
                      C4T=C4T+XSAB57(12,I)
                      C5T=C5T+XSAB57(11,I)
                  ELSE
                  END IF
 20           CONTINUE
C       NOW FOR CONVERSIONS
C       MODE UFOCAL
              IF(SYSTEM1(30).EQ.2.0) THEN
C       MODE IS UFOCAL, NO CONVERSIONS
                  GO TO 25
              ELSE
              END IF
C       MODE UAFOCAL
              IF(SYSTEM1(30).EQ.4.0) THEN
C       MODE IS UAFOCAL, NO CONVERSIONS
                  GO TO 25
              ELSE
              END IF
C       MODE FOCAL
              IF(SYSTEM1(30).EQ.1.0.OR.SYSTEM1(30).EQ.3.0) THEN
C       MODE IS FOCAL OR AFOCAL, CONVERT SUMS
C       CHROMATIC ABERRATION
                  C1T=C1T/INV
                  C2T=C2T/INV
                  C3T=C3T/INV
                  C4T=C4T/INV
                  C5T=C5T/INV
                  GO TO 25
              ELSE
C       NO CONVERSION REQUIRED
              END IF
 25           CONTINUE
C       PRINT TOTALS WITH LABELING
              WRITE(OUTLYNE,998)
              CALL SHOWIT(0)
              C1=C1T
              C2=C2T
              C3=C3T
              C4=C4T
              C5=C5T
              WRITE(OUTLYNE,2001) C1,C2,C3,C4,C5
              CALL SHOWIT(0)
              RETURN
          ELSE
C       QUALIFIER NOT "ALL"
          END IF
          IF(SQ.EQ.1.AND.WQ.EQ.'OBJ'.OR.SQ.EQ.1.AND.WQ.EQ.'OB'.OR.
     1    SQ.EQ.1.AND.WQ.EQ.'I'.OR.SQ.EQ.1.AND.WQ.EQ.'IM'.OR.
     2    SQ.EQ.1.AND.WQ.EQ.'IMAGE') THEN
              IF(WQ.EQ.'OB'.OR.WQ.EQ.'OBJ') SF=0
              IF(WQ.EQ.'I'.OR.WQ.EQ.'IM'.OR.WQ.EQ.'IMAGE')
     1        SF=INT(SYSTEM1(20))
              IF(SYSTEM1(30).EQ.1.0.OR.
     1                SYSTEM1(30).EQ.3.0) THEN
C               FOCAL OR AFOCAL, CONVERT
                  IF(WC.EQ.'MAB5I') THEN
                      C1=SAB57(1,SF)/INV
                      C2=(SAB57(2,SF)+SAB57(3,SF))/INV
                      C3=SAB57(10,SF)/INV
                      C4=SAB57(12,SF)/INV
                      C5=SAB57(11,SF)/INV
                  ELSE
                  END IF
                  IF(WC.EQ.'XMAB5I') THEN
                      C1=XSAB57(1,SF)/INV
                      C2=(XSAB57(2,SF)+XSAB57(3,SF))/INV
                      C3=XSAB57(10,SF)/INV
                      C4=XSAB57(12,SF)/INV
                      C5=XSAB57(11,SF)/INV
                  ELSE
                  END IF
                  WRITE(OUTLYNE,5000)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,2000)SF,C1,C2,C3,C4,C5
                  CALL SHOWIT(0)
                  RETURN
              ELSE
              END IF
              IF(SYSTEM1(30).EQ.2.0.OR.
     1                SYSTEM1(30).EQ.4.0) THEN
C               UFOCAL OR UAFOCAL, DON'T CONVERT
                  IF(WC.EQ.'MAB5I') THEN
                      C1=SAB57(1,SF)
                      C2=SAB57(2,SF)+SAB57(3,SF)
                      C3=SAB57(10,SF)
                      C4=SAB57(12,SF)
                      C5=SAB57(11,SF)
                  ELSE
                  END IF
                  IF(WC.EQ.'XMAB5I') THEN
                      C1=XSAB57(1,SF)
                      C2=XSAB57(2,SF)+XSAB57(3,SF)
                      C3=XSAB57(10,SF)
                      C4=XSAB57(12,SF)
                      C5=XSAB57(11,SF)
                  ELSE
                  END IF
                  WRITE(OUTLYNE,5000)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,2000)SF,C1,C2,C3,C4,C5
                  CALL SHOWIT(0)
                  RETURN
              ELSE
              END IF
          ELSE
          END IF
          IF(SQ.EQ.1.AND.WQ.NE.'OBJ'.OR.SQ.EQ.1.AND.
     1    WQ.NE.'ALL'.OR.SQ.EQ.1.AND.WQ.NE.'OB'.OR.SQ.EQ.1.AND.
     2    WQ.NE.'I'.OR.SQ.EQ.1.AND.WQ.NE.'IM'.OR.SQ.EQ.1.AND.
     3    WQ.NE.'IMAGE') THEN
              OUTLYNE='INVALID QUALIFIER WORD'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SQ.EQ.0.AND.DF1.EQ.1.OR.SQ.EQ.1.AND.WQ.EQ.'IM'.OR.
     1    SQ.EQ.1.AND.WQ.EQ.'I'.OR.SQ.EQ.1.AND.WQ.EQ.'IMAGE') THEN
              SF=INT(SYSTEM1(20))
C       OUTPUT SYSTEM TOTALS
C
C       THE SURFACE CONTRIBUTIONS MUST BE SUMMED
              C1T=0.0
              C2T=0.0
              C3T=0.0
              C4T=0.0
              C5T=0.0
              DO 220 I=0,SF
                  IF(WC.EQ.'MAB5I') THEN
                      C1T=C1T+SAB57(1,I)
                      C2T=C2T+SAB57(2,I)+SAB57(3,I)
                      C3T=C3T+SAB57(10,I)
                      C4T=C4T+SAB57(12,I)
                      C5T=C5T+SAB57(11,I)
                  ELSE
                  END IF
                  IF(WC.EQ.'XMAB5I') THEN
                      C1T=C1T+XSAB57(1,I)
                      C2T=C2T+XSAB57(2,I)+XSAB57(3,I)
                      C3T=C3T+XSAB57(10,I)
                      C4T=C4T+XSAB57(12,I)
                      C5T=C5T+XSAB57(11,I)
                  ELSE
                  END IF
 220          CONTINUE
C NOW FOR CONVERSIONS
C       MODE UFOCAL
              IF(SYSTEM1(30).EQ.2.0) THEN
C       MODE IS UFOCAL, NO CONVERSIONS
                  GO TO 250
              ELSE
              END IF
C       MODE UAFOCAL
              IF(SYSTEM1(30).EQ.4.0) THEN
C       MODE IS UAFOCAL, NO CONVERSIONS
                  GO TO 250
              ELSE
              END IF
C       MODE FOCAL
              IF(SYSTEM1(30).EQ.1.0.OR.SYSTEM1(30).EQ.3.0) THEN
C       MODE IS FOCAL OR AFOCAL, CONVERT SUMS TO TRANSVERSE
C       CHROMATIC ABERRATION
                  C1T=C1T/INV
                  C2T=C2T/INV
                  C3T=C3T/INV
                  C4T=C4T/INV
                  C5T=C5T/INV
                  GO TO 250
              ELSE
              END IF
 250          CONTINUE
              C1=C1T
              C2=C2T
              C3=C3T
              C4=C4T
              C5=C5T
C       PRINT TOTALS WITH OUT LABELING
              WRITE(OUTLYNE,5000)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,2001) C1,C2,C3,C4,C5
              CALL SHOWIT(0)
              RETURN
          ELSE
C       NOT IMAGE SURFACE
          END IF
C
          IF(SQ.EQ.0.AND.DF1.NE.1) THEN
              I=INT(W1)
              SF=INT(SYSTEM1(20))
              IF(I.GT.SF.OR.I.LT.0) THEN
                  OUTLYNE='SURFACE NUMBER BEYOND LEGAL RANGE'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(SYSTEM1(30).EQ.1.0.OR.
     1                SYSTEM1(30).EQ.3.0) THEN
C               FOCAL OR AFOCAL, CONVERT
                  IF(WC.EQ.'MAB5I') THEN
                      C1=SAB57(1,I)/INV
                      C2=(SAB57(2,I)+SAB57(3,I))/INV
                      C3=SAB57(10,I)/INV
                      C4=SAB57(12,I)/INV
                      C5=SAB57(11,I)/INV
                  ELSE
                  END IF
                  IF(WC.EQ.'XMAB5I') THEN
                      C1=XSAB57(1,I)/INV
                      C2=(XSAB57(2,I)+XSAB57(3,I))/INV
                      C3=XSAB57(10,I)/INV
                      C4=XSAB57(12,I)/INV
                      C5=XSAB57(11,I)/INV
                  ELSE
                  END IF
                  WRITE(OUTLYNE,5000)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,2000)I,C1,C2,C3,C4,C5
                  CALL SHOWIT(0)
                  RETURN
              ELSE
              END IF
              IF(SYSTEM1(30).EQ.2.0.OR.
     1                SYSTEM1(30).EQ.4.0) THEN
C               UFOCAL OR UAFOCAL, DON'T CONVERT
                  IF(WC.EQ.'MAB5I') THEN
                      C1=SAB57(1,I)
                      C2=SAB57(2,I)+SAB57(3,I)
                      C3=SAB57(10,I)
                      C4=SAB57(12,I)
                      C5=SAB57(11,I)
                  ELSE
                  END IF
                  IF(WC.EQ.'XMAB5I') THEN
                      C1=XSAB57(1,I)
                      C2=XSAB57(2,I)+XSAB57(3,I)
                      C3=XSAB57(10,I)
                      C4=XSAB57(12,I)
                      C5=XSAB57(11,I)
                  ELSE
                  END IF
                  WRITE(OUTLYNE,5000)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,2000)I,C1,C2,C3,C4,C5
                  CALL SHOWIT(0)
              ELSE
              END IF
              RETURN
          ELSE
          END IF
! 1500   FORMAT(I3,2X,G12.5,2X,G12.5,2X,G12.5,2X,G12.5,
!     1  2X,G12.5)
 2000     FORMAT(I3,2X,G12.5,2X,G12.5,2X,G12.5,2X,G12.5,
     1    2X,G12.5)
 2001     FORMAT(5X,G12.5,2X,G12.5,2X,G12.5,2X,G12.5,2X,G12.5)
 5000     FORMAT('SURF',5X,'SA5 ',10X,'CMA5',10X,'AST5',
     1    10X,'DIS5',10X,'PTZ5')
 5001     FORMAT('(Y-Z) PLANE FIFTH ORDER ABERRATION CONTRIBUTIONS'
     1    ,' - (CFG #',I2,')')
! 6001   FORMAT('(X-Z) PLANE FIFTH ORDER ABERRATION CONTRIBUTIONS'
!     1  ,' - (CFG #',I2,')')
 4999     FORMAT(
     1    'WARNING: THESE ARE INTRINSIC SURFACE CONTRIBUTIONS ONLY!')
 5501     FORMAT('TRANSVERSE - WITH FINAL SURFACE CONVERSION')
 5502     FORMAT('TRANSVERSE - WITHOUT FINAL SURFACE CONVERSION')
 5503     FORMAT('ANGULAR - WITH FINAL SURFACE CONVERSION')
 5504     FORMAT('ANGULAR - WITHOUT FINAL SURFACE CONVERSION')
 2501     FORMAT(1X)
 998      FORMAT('SYSTEM TOTALS ARE:')
      END
C SUB ABX5I.FOR
      SUBROUTINE ABX5I
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE ABX5I. THIS SUBROUTINE IMPLEMENTS
C       THE MABX5I XMABX5I CMD LEVEL COMMAND
C
          INTEGER I,SF,CW
C
          REAL*8 C1,C2,C3,C4,C5,C1T,C2T,C3T,C4T,C5T
C
          REAL*8 INV
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
C
          CALL PRTRC
C
C       THE MABX5I AND XMABX5I COMMAND ACCEPTS QUALIFIER OR NUMERIC
C       INPUT IN AN EITHER OR MODE. IT DOES NOT ACCEPT STING
C       INPUT. THE VALID QUALIFIERS ARE "ALL" AND "OBJ" AND "OB"
C       "I","IM", AND "IMAGE"
C       THE VALID NUMERIC INPUT IS THE SURFACE NUMBER FOR
C       WHICH IT IS DESIRED TO PRODUCE OUTPUT.
C
          IF(SQ.EQ.1.AND.S1.EQ.1) THEN
              IF(WC.EQ.'MABX5I') THEN
                  OUTLYNE='"MABX5I" TAKES EITHER QUALIFIER OR NUMERIC INPUT"'
                  CALL SHOWIT(1)
                  OUTLYNE='BUT NOT BOTH'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(WC.EQ.'XMABX5I') THEN
                  OUTLYNE='"XMABX5I" TAKES EITHER QUALIFIER OR NUMERIC INPUT"'
                  CALL SHOWIT(1)
                  OUTLYNE='BUT NOT BOTH'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
          IF(SST.EQ.1.OR.S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
              IF(WC.EQ.'MABX5I') THEN
                  OUTLYNE=
     1            '"MABX5I" TAKES NO STRING OR NUMERIC WORD #2 THROUGH #5 INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(WC.EQ.'XMABX5I') THEN
                  OUTLYNE=
     1            '"XMABX5I" TAKES NO STRING OR NUMERIC WORD #2 THROUGH #5 INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
          IF(SYSTEM1(20).EQ.0.0) THEN
              OUTLYNE='LENS SYSTEM HAS NO SURFACES'
              CALL SHOWIT(1)
              OUTLYNE='NO PARAXIAL OR FIFTH ORDER DATA EXISTS'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
          SF=INT(SYSTEM1(20))
          IF(INT(SYSTEM1(11)).GE.1.AND.INT(SYSTEM1(11)).LE.5) THEN
              CW=INT(SYSTEM1(11))+45
          END IF
          IF(INT(SYSTEM1(11)).GE.6.AND.INT(SYSTEM1(11)).LE.10) THEN
              CW=INT(SYSTEM1(11))+65
          END IF
          INV=1.0D0
          IF(SYSTEM1(30).EQ.1.0D0) THEN
C       MODE IS FOCAL
              IF(WC.EQ.'MABX5I')
     1        INV=-2.0*ALENS(CW,(SF-1))*PXTRAY(2,(SF-1))
              IF(WC.EQ.'XMABX5I')
     1        INV=-2.0*ALENS(CW,(SF-1))*PXTRAX(2,(SF-1))
              IF(DABS(INV).LE.1.0D-10) THEN
                  CALL MACFAL
                  RETURN
              END IF
          ELSE
          END IF
          IF(SYSTEM1(30).EQ.3.0D0) THEN
C       MODE IS AFOCAL
              IF(WC.EQ.'MABX5I')
     1        INV= 2.0*ALENS(CW,(SF-1))*PXTRAY(1,SF)
              IF(WC.EQ.'XMABX5I')
     1        INV= 2.0*ALENS(CW,(SF-1))*PXTRAX(1,SF)
              IF(DABS(INV).LE.1.0D-10) THEN
                  CALL MACFAL
                  RETURN
              END IF
          ELSE
          END IF
          IF(INV.EQ.0.0D0) THEN
              OUTLYNE=
     1        'THE LENS (MODE) IS NOT CONSISTENT WITH PARAXIAL VALUES'
              CALL SHOWIT(1)
              OUTLYNE=
     1        'ABERRATIONS ARE NOT CALCULABLE'
              CALL SHOWIT(1)
              IF(SYSTEM1(30).EQ.1.0D0)
     1        OUTLYNE='CHANGE FROM "MODE FOCAL" TO "MODE AFOCAL"'
              IF(SYSTEM1(30).EQ.3.0D0)
     1        OUTLYNE='CHANGE FROM "MODE AFOCAL" TO "MODE FOCAL"'
              CALL SHOWIT(1)
              OUTLYNE='THEN RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SQ.EQ.1.AND.WQ.EQ.'ALL') THEN
              SF=INT(SYSTEM1(20))
              IF(WC.EQ.'MABX5I')WRITE(OUTLYNE,5001) INT(F12)
              IF(WC.EQ.'XMABX5I')WRITE(OUTLYNE,6001) INT(F12)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,4999)
              CALL SHOWIT(0)
              IF(SYSTEM1(30).EQ.1.0) WRITE(OUTLYNE,5501)
              IF(SYSTEM1(30).EQ.2.0) WRITE(OUTLYNE,5502)
              IF(SYSTEM1(30).EQ.3.0) WRITE(OUTLYNE,5503)
              IF(SYSTEM1(30).EQ.4.0) WRITE(OUTLYNE,5504)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,2501)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,5000)
              CALL SHOWIT(0)
              DO 10 I=0,SF
                  IF(SYSTEM1(30).EQ.1.0.OR.
     1                    SYSTEM1(30).EQ.3.0) THEN
C               FOCAL OR AFOCAL, CONVERT
                      IF(WC.EQ.'MABX5I') THEN
                          C1=(SAB57(4,I)+SAB57(5,I)+SAB57(6,I))/INV
                          C2=SAB57(5,I)/INV
                          C3=(SAB57(7,I)+SAB57(8,I))/INV
                          C4=(SAB57(11,I)+(5.0*SAB57(10,I)))/INV
                          C5=(SAB57(11,I)+SAB57(10,I))/INV
                      ELSE
                      END IF
                      IF(WC.EQ.'XMABX5I') THEN
                          C1=(XSAB57(4,I)+XSAB57(5,I)+XSAB57(6,I))/INV
                          C2=XSAB57(5,I)/INV
                          C3=(XSAB57(7,I)+XSAB57(8,I))/INV
                          C4=(XSAB57(11,I)+(5.0*XSAB57(10,I)))/INV
                          C5=(XSAB57(11,I)+XSAB57(10,I))/INV
                      ELSE
                      END IF
                      WRITE(OUTLYNE,2000)I,C1,C2,C3,C4,C5
                      CALL SHOWIT(0)
                      GO TO 10
                  ELSE
                  END IF
                  IF(SYSTEM1(30).EQ.2.0.OR.
     1                    SYSTEM1(30).EQ.4.0) THEN
C               UFOCAL OR UAFOCAL, DON'T CONVERT
                      IF(WC.EQ.'MABX5I') THEN
                          C1=SAB57(4,I)+SAB57(5,I)+SAB57(6,I)
                          C2=SAB57(5,I)
                          C3=SAB57(7,I)+SAB57(8,I)
                          C4=SAB57(11,I)+(SAB57(10,I)*5.0)
                          C5=SAB57(11,I)+SAB57(10,I)
                      ELSE
                      END IF
                      IF(WC.EQ.'XMABX5I') THEN
                          C1=XSAB57(4,I)+XSAB57(5,I)+XSAB57(6,I)
                          C2=XSAB57(5,I)
                          C3=XSAB57(7,I)+XSAB57(8,I)
                          C4=XSAB57(11,I)+(XSAB57(10,I)*5.0)
                          C5=XSAB57(11,I)+XSAB57(10,I)
                      ELSE
                      END IF
                      WRITE(OUTLYNE,2000)I,C1,C2,C3,C4,C5
                      CALL SHOWIT(0)
                  ELSE
                  END IF
 10           CONTINUE
C       NOW HANDEL THE CALCULATION AND PRINTING OF THE
C       TOTALS
C       IN ANY CASE, THE SURFACE CONTRIBUTIONS MUST BE SUMMED
              C1T=0.0
              C2T=0.0
              C3T=0.0
              C4T=0.0
              C5T=0.0
              DO 20 I=0,SF
                  IF(WC.EQ.'MABX5I') THEN
                      C1T=C1T+SAB57(4,I)+SAB57(5,I)+SAB57(6,I)
                      C2T=C2T+SAB57(5,I)
                      C3T=C3T+SAB57(7,I)+SAB57(8,I)
                      C4T=C4T+SAB57(11,I)+(SAB57(10,I)*5.0)
                      C5T=C5T+SAB57(10,I)+SAB57(11,I)
                  ELSE
                  END IF
                  IF(WC.EQ.'XMABX5I') THEN
                      C1T=C1T+XSAB57(4,I)+XSAB57(5,I)+XSAB57(6,I)
                      C2T=C2T+XSAB57(5,I)
                      C3T=C3T+XSAB57(7,I)+XSAB57(8,I)
                      C4T=C4T+XSAB57(11,I)+(XSAB57(10,I)*5.0)
                      C5T=C5T+XSAB57(10,I)+XSAB57(11,I)
                  ELSE
                  END IF
 20           CONTINUE
C       NOW FOR CONVERSIONS
C       MODE UFOCAL
              IF(SYSTEM1(30).EQ.2.0) THEN
C       MODE IS UFOCAL, NO CONVERSIONS
                  GO TO 25
              ELSE
              END IF
C       MODE UAFOCAL
              IF(SYSTEM1(30).EQ.4.0) THEN
C       MODE IS UAFOCAL, NO CONVERSIONS
                  GO TO 25
              ELSE
              END IF
C       MODE FOCAL
              IF(SYSTEM1(30).EQ.1.0.OR.SYSTEM1(30).EQ.3.0) THEN
C       MODE IS FOCAL OR AFOCAL, CONVERT SUMS
C       CHROMATIC ABERRATION
                  C1T=C1T/INV
                  C2T=C2T/INV
                  C3T=C3T/INV
                  C4T=C4T/INV
                  C5T=C5T/INV
                  GO TO 25
              ELSE
C       NO CONVERSION REQUIRED
              END IF
C       PRINT TOTALS WITH LABELING
 25           CONTINUE
              WRITE(OUTLYNE,998)
              CALL SHOWIT(0)
              C1=C1T
              C2=C2T
              C3=C3T
              C4=C4T
              C5=C5T
              WRITE(OUTLYNE,2001) C1,C2,C3,C4,C5
              CALL SHOWIT(0)
              RETURN
          ELSE
C       QUALIFIER NOT "ALL"
          END IF
          IF(SQ.EQ.1.AND.WQ.EQ.'OBJ'.OR.SQ.EQ.1.AND.WQ.EQ.'OB'.OR.
     1    SQ.EQ.1.AND.WQ.EQ.'I'.OR.SQ.EQ.1.AND.WQ.EQ.'IM'.OR.
     2    SQ.EQ.1.AND.WQ.EQ.'IMAGE') THEN
              IF(WQ.EQ.'OB'.OR.WQ.EQ.'OBJ') SF=0
              IF(WQ.EQ.'I'.OR.WQ.EQ.'IM'.OR.WQ.EQ.'IMAGE')
     1        SF=INT(SYSTEM1(20))
              IF(SYSTEM1(30).EQ.1.0.OR.
     1                SYSTEM1(30).EQ.3.0) THEN
C               FOCAL OR AFOCAL, CONVERT
                  IF(WC.EQ.'MABX5I') THEN
                      C1=(SAB57(4,SF)+SAB57(5,SF)+SAB57(6,SF))/INV
                      C2=(SAB57(5,SF))/INV
                      C3=(SAB57(7,SF)+SAB57(8,SF))/INV
                      C4=(SAB57(10,SF)+(5.0*SAB57(11,SF)))/INV
                      C5=(SAB57(10,SF)+SAB57(11,SF))/INV
                  ELSE
                  END IF
                  IF(WC.EQ.'XMABX5I') THEN
                      C1=(XSAB57(4,SF)+XSAB57(5,SF)+XSAB57(6,SF))/INV
                      C2=(XSAB57(5,SF))/INV
                      C3=(XSAB57(7,SF)+XSAB57(8,SF))/INV
                      C4=(XSAB57(10,SF)+(5.0*XSAB57(11,SF)))/INV
                      C5=(XSAB57(10,SF)+XSAB57(11,SF))/INV
                  ELSE
                  END IF
                  IF(HEADIN) WRITE(OUTLYNE,5000)
                  IF(HEADIN) CALL SHOWIT(0)
                  WRITE(OUTLYNE,2000)SF,C1,C2,C3,C4,C5
                  CALL SHOWIT(0)
                  RETURN
              ELSE
              END IF
              IF(SYSTEM1(30).EQ.2.0.OR.
     1                SYSTEM1(30).EQ.4.0) THEN
C               UFOCAL OR UAFOCAL, DON'T CONVERT
                  IF(WC.EQ.'MABX5I') THEN
                      C1=SAB57(4,SF)+SAB57(5,SF)+SAB57(6,SF)
                      C2=SAB57(5,SF)
                      C3=SAB57(7,SF)+SAB57(8,SF)
                      C4=SAB57(10,SF)+(5.0*SAB57(11,SF))
                      C5=SAB57(10,SF)+SAB57(11,SF)
                  ELSE
                  END IF
                  IF(WC.EQ.'XMABX5I') THEN
                      C1=XSAB57(4,SF)+XSAB57(5,SF)+XSAB57(6,SF)
                      C2=XSAB57(5,SF)
                      C3=XSAB57(7,SF)+XSAB57(8,SF)
                      C4=XSAB57(10,SF)+(5.0*XSAB57(11,SF))
                      C5=XSAB57(10,SF)+XSAB57(11,SF)
                  ELSE
                  END IF
                  IF(HEADIN) WRITE(OUTLYNE,5000)
                  IF(HEADIN) CALL SHOWIT(0)
                  WRITE(OUTLYNE,2000)SF,C1,C2,C3,C4,C5
                  CALL SHOWIT(0)
                  RETURN
              ELSE
              END IF
          ELSE
          END IF
          IF(SQ.EQ.1.AND.WQ.NE.'OBJ'.OR.SQ.EQ.1.AND.
     1    WQ.NE.'ALL'.OR.SQ.EQ.1.AND.WQ.NE.'OB'.OR.SQ.EQ.1.AND.
     2    WQ.NE.'I'.OR.SQ.EQ.1.AND.WQ.NE.'IM'.OR.SQ.EQ.1.AND.
     3    WQ.NE.'IMAGE') THEN
              OUTLYNE='INVALID QUALIFIER WORD'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SQ.EQ.0.AND.DF1.EQ.1.OR.SQ.EQ.1.AND.WQ.EQ.'IM'.OR.
     1    SQ.EQ.1.AND.WQ.EQ.'I'.OR.SQ.EQ.1.AND.WQ.EQ.'IMAGE') THEN
              SF=INT(SYSTEM1(20))
C       OUTPUT SYSTEM TOTALS
C
C       THE SURFACE CONTRIBUTIONS MUST BE SUMMED
              C1T=0.0
              C2T=0.0
              C3T=0.0
              C4T=0.0
              C5T=0.0
              DO 220 I=0,SF
                  IF(WC.EQ.'MABX5I') THEN
                      C1T=C1T+SAB57(4,I)+SAB57(5,I)+SAB57(6,I)
                      C2T=C2T+SAB57(5,I)
                      C3T=C3T+SAB57(7,I)+SAB57(8,I)
                      C4T=C4T+SAB57(10,I)+(5.0*SAB57(11,I))
                      C5T=C5T+SAB57(10,I)+SAB57(11,I)
                  ELSE
                  END IF
                  IF(WC.EQ.'XMABX5I') THEN
                      C1T=C1T+XSAB57(4,I)+XSAB57(5,I)+XSAB57(6,I)
                      C2T=C2T+XSAB57(5,I)
                      C3T=C3T+XSAB57(7,I)+XSAB57(8,I)
                      C4T=C4T+XSAB57(10,I)+(5.0*XSAB57(11,I))
                      C5T=C5T+XSAB57(10,I)+XSAB57(11,I)
                  ELSE
                  END IF
 220          CONTINUE
C NOW FOR CONVERSIONS
C       MODE UFOCAL
              IF(SYSTEM1(30).EQ.2.0) THEN
C       MODE IS UFOCAL, NO CONVERSIONS
                  GO TO 250
              ELSE
              END IF
C       MODE UAFOCAL
              IF(SYSTEM1(30).EQ.4.0) THEN
C       MODE IS UAFOCAL, NO CONVERSIONS
                  GO TO 250
              ELSE
              END IF
C       MODE FOCAL
              IF(SYSTEM1(30).EQ.1.0.OR.SYSTEM1(30).EQ.3.0) THEN
C       MODE IS FOCAL OR AFOCAL, CONVERT SUMS TO TRANSVERSE
C       CHROMATIC ABERRATION
                  C1T=C1T/INV
                  C2T=C2T/INV
                  C3T=C3T/INV
                  C4T=C4T/INV
                  C5T=C5T/INV
                  GO TO 250
              ELSE
              END IF
 250          CONTINUE
              C1=C1T
              C2=C2T
              C3=C3T
              C4=C4T
              C5=C5T
C       PRINT TOTALS WITH OUT LABELING
              IF(HEADIN) WRITE(OUTLYNE,5000)
              IF(HEADIN) CALL SHOWIT(0)
              WRITE(OUTLYNE,2001) C1,C2,C3,C4,C5
              CALL SHOWIT(0)
              RETURN
          ELSE
C       NOT IMAGE SURFACE
          END IF
C
          IF(SQ.EQ.0.AND.DF1.NE.1) THEN
              I=INT(W1)
              SF=INT(SYSTEM1(20))
              IF(I.GT.SF.OR.I.LT.0) THEN
                  OUTLYNE='SURFACE NUMBER BEYOND LEGAL RANGE'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(SYSTEM1(30).EQ.1.0.OR.
     1                SYSTEM1(30).EQ.3.0) THEN
C               FOCAL OR AFOCAL, CONVERT
                  IF(WC.EQ.'MABX5I') THEN
                      C1=(SAB57(4,I)+SAB57(5,I)+SAB57(6,I))/INV
                      C2=SAB57(5,I)/INV
                      C3=(SAB57(7,I)+SAB57(8,I))/INV
                      C4=(SAB57(10,I)+(SAB57(11,I)*5.0))/INV
                      C5=(SAB57(10,I)+SAB57(11,I))/INV
                  ELSE
                  END IF
                  IF(WC.EQ.'XMABX5I') THEN
                      C1=(XSAB57(4,I)+XSAB57(5,I)+XSAB57(6,I))/INV
                      C2=XSAB57(5,I)/INV
                      C3=(XSAB57(7,I)+XSAB57(8,I))/INV
                      C4=(XSAB57(10,I)+(XSAB57(11,I)*5.0))/INV
                      C5=(XSAB57(10,I)+XSAB57(11,I))/INV
                  ELSE
                  END IF
                  IF(HEADIN) WRITE(OUTLYNE,5000)
                  IF(HEADIN) CALL SHOWIT(0)
                  WRITE(OUTLYNE,2000)I,C1,C2,C3,C4,C5
                  CALL SHOWIT(0)
                  RETURN
              ELSE
              END IF
              IF(SYSTEM1(30).EQ.2.0.OR.
     1                SYSTEM1(30).EQ.4.0) THEN
C               UFOCAL OR UAFOCAL, DON'T CONVERT
                  IF(WC.EQ.'MABX5I') THEN
                      C1=SAB57(4,I)+SAB57(5,I)+SAB57(6,I)
                      C2=SAB57(5,I)
                      C3=SAB57(7,I)+SAB57(8,I)
                      C4=SAB57(10,I)+(5.0*SAB57(11,I))
                      C5=SAB57(10,I)+SAB57(11,I)
                  ELSE
                  END IF
                  IF(WC.EQ.'XMABX5I') THEN
                      C1=XSAB57(4,I)+XSAB57(5,I)+XSAB57(6,I)
                      C2=XSAB57(5,I)
                      C3=XSAB57(7,I)+XSAB57(8,I)
                      C4=XSAB57(10,I)+(5.0*XSAB57(11,I))
                      C5=XSAB57(10,I)+XSAB57(11,I)
                  ELSE
                  END IF
                  IF(HEADIN) WRITE(OUTLYNE,5000)
                  IF(HEADIN) CALL SHOWIT(0)
                  WRITE(OUTLYNE,2000)I,C1,C2,C3,C4,C5
                  CALL SHOWIT(0)
              ELSE
              END IF
              RETURN
          ELSE
          END IF
! 1500   FORMAT(I3,2X,G12.5,2X,G12.5,2X,G12.5,2X,G12.5,
!     1  2X,G12.5)
 2000     FORMAT(I3,2X,G12.5,2X,G12.5,2X,G12.5,2X,G12.5,
     1    2X,G12.5)
 2001     FORMAT(5X,G12.5,2X,G12.5,2X,G12.5,2X,G12.5,2X,G12.5)
 5000     FORMAT('SURF',5X,'TOBSA',9X,'SOBSA',9X,'ELCMA',
     1    9X,'TAS ',10X,'SAS')
 4999     FORMAT(
     1    'WARNING: THESE ARE INTRINSIC SURFACE COEFFICIENTS ONLY!')
 5001     FORMAT(
     1    '(Y-Z) PLANE FIFTH ORDER (EXTENDED) ABERRATION CONTRIBUTIONS'
     1    ,' - (CFG #',I2,')')
 6001     FORMAT(
     1    '(X-Z) PLANE FIFTH ORDER (EXTENDED) ABERRATION CONTRIBUTIONS'
     1    ,' - (CFG #',I2,')')
 5501     FORMAT('TRANSVERSE - WITH FINAL SURFACE CONVERSION')
 5502     FORMAT('TRANSVERSE - WITHOUT FINAL SURFACE CONVERSION')
 5503     FORMAT('ANGULAR - WITH FINAL SURFACE CONVERSION')
 5504     FORMAT('ANGULAR - WITHOUT FINAL SURFACE CONVERSION')
 2501     FORMAT(1X)
 998      FORMAT('SYSTEM TOTALS ARE:')
      END
C SUB MMAB3.FOR
      SUBROUTINE MMAB3
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE MMAB3. THIS SUBROUTINE IMPLEMENTS
C       THE MAB3 (THIRD ORDER ABERRATION) PRINTOUT AT THE
C       CMD LEVEL AND IS A MODEL FOR MAB5,MABX5 ETC..
C
          INTEGER I,SF,CW
C
          REAL*8 C1,C2,C3,C4,C5,C1T,C2T,C3T,C4T,C5T
C
          REAL*8 INV
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
C
          CALL PRTRC
C
C       THE MAB3 AND XMAB3 COMMAND ACCEPTS QUALIFIER OR NUMERIC
C       INPUT IN AN EITHER OR MODE. IT DOES NOT ACCEPT STING
C       INPUT. THE VALID QUALIFIERS ARE "ALL" AND "OBJ" AND "OB"
C       "I","IM", AND "IMAGE"
C       THE VALID NUMERIC INPUT IS THE SURFACE NUMBER FOR
C       WHICH IT IS DESIRED TO PRODUCE OUTPUT.
C
          IF(SQ.EQ.1.AND.S1.EQ.1) THEN
              IF(WC.EQ.'MAB3') THEN
                  OUTLYNE='"MAB3" TAKES EITHER QUALIFIER OR NUMERIC INPUT"'
                  CALL SHOWIT(1)
                  OUTLYNE='BUT NOT BOTH'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(WC.EQ.'XMAB3') THEN
                  OUTLYNE='"XMAB3" TAKES EITHER QUALIFIER OR NUMERIC INPUT"'
                  CALL SHOWIT(1)
                  OUTLYNE='BUT NOT BOTH'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
          IF(SST.EQ.1.OR.S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
              IF(WC.EQ.'MAB3') THEN
                  OUTLYNE=
     1            '"MAB3" TAKES NO STRING OR NUMERIC WORD #2 THROUGH #5 INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(WC.EQ.'XMAB3') THEN
                  OUTLYNE=
     1            '"XMAB3" TAKES NO STRING OR NUMERIC WORD #2 THROUGH #5 INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
          IF(SYSTEM1(20).EQ.0.0D0) THEN
              OUTLYNE='LENS SYSTEM HAS NO SURFACES'
              CALL SHOWIT(1)
              OUTLYNE='NO PARAXIAL OR THIRD ORDER DATA EXISTS'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
          SF=INT(SYSTEM1(20))
          IF(INT(SYSTEM1(11)).GE.1.AND.INT(SYSTEM1(11)).LE.5) THEN
              CW=INT(SYSTEM1(11))+45
          END IF
          IF(INT(SYSTEM1(11)).GE.6.AND.INT(SYSTEM1(11)).LE.10) THEN
              CW=INT(SYSTEM1(11))+65
          END IF
          INV=1.0D0
          IF(SYSTEM1(30).EQ.1.0D0) THEN
C       MODE IS FOCAL
              IF(WC.EQ.'MAB3')
     1        INV=-2.0*ALENS(CW,(SF-1))*PXTRAY(2,(SF-1))
              IF(WC.EQ.'XMAB3')
     1        INV=-2.0*ALENS(CW,(SF-1))*PXTRAX(2,(SF-1))
              IF(DABS(INV).LE.1.0D-10) THEN
                  CALL MACFAL
                  RETURN
              END IF
          ELSE
          END IF
          IF(SYSTEM1(30).EQ.3.0D0) THEN
C       MODE IS AFOCAL
              IF(WC.EQ.'MAB3')
     1        INV= 2.0*ALENS(CW,(SF-1))*PXTRAY(1,SF)
              IF(WC.EQ.'XMAB3')
     1        INV= 2.0*ALENS(CW,(SF-1))*PXTRAX(1,SF)
              IF(DABS(INV).LE.1.0D-10) THEN
                  CALL MACFAL
                  RETURN
              END IF
          ELSE
          END IF
          IF(INV.EQ.0.0D0) THEN
              OUTLYNE=
     1        'THE LENS (MODE) IS NOT CONSISTENT WITH PARAXIAL VALUES'
              CALL SHOWIT(1)
              OUTLYNE=
     1        'ABERRATIONS ARE NOT CALCULABLE'
              CALL SHOWIT(1)
              IF(SYSTEM1(30).EQ.1.0D0)
     1        OUTLYNE='CHANGE FROM "MODE FOCAL" TO "MODE AFOCAL"'
              IF(SYSTEM1(30).EQ.3.0D0)
     1        OUTLYNE='CHANGE FROM "MODE AFOCAL" TO "MODE FOCAL"'
              CALL SHOWIT(1)
              OUTLYNE='THEN RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SQ.EQ.1.AND.WQ.EQ.'ALL') THEN
              SF=INT(SYSTEM1(20))
              IF(WC.EQ.'MAB3') WRITE(OUTLYNE,5001)
              IF(WC.EQ.'XMAB3') WRITE(OUTLYNE,6001)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,5002) INT(F12)
              CALL SHOWIT(0)
              IF(SYSTEM1(30).EQ.1.0) WRITE(OUTLYNE,5501)
              IF(SYSTEM1(30).EQ.2.0) WRITE(OUTLYNE,5502)
              IF(SYSTEM1(30).EQ.3.0) WRITE(OUTLYNE,5503)
              IF(SYSTEM1(30).EQ.4.0) WRITE(OUTLYNE,5504)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,2501)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,5000)
              CALL SHOWIT(0)
              DO 10 I=0,SF
                  IF(SYSTEM1(30).EQ.1.0.OR.
     1                    SYSTEM1(30).EQ.3.0) THEN
C               FOCAL OR AFOCAL, CONVERT
                      IF(WC.EQ.'MAB3') THEN
                          C1=MAB3(1,I)/INV
                          C2=(3.0D0*MAB3(2,I))/INV
                          C3=MAB3(3,I)/INV
                          C4=MAB3(4,I)/INV
                          C5=MAB3(5,I)/INV
                      ELSE
                      END IF
                      IF(WC.EQ.'XMAB3') THEN
                          C1=XMAB3(1,I)/INV
                          C2=(3.0D0*XMAB3(2,I))/INV
                          C3=XMAB3(3,I)/INV
                          C4=XMAB3(4,I)/INV
                          C5=XMAB3(5,I)/INV
                      ELSE
                      END IF
                      WRITE(OUTLYNE,2000)I,C1,C2,C3,C4,C5
                      CALL SHOWIT(0)
                      GO TO 10
                  ELSE
                  END IF
                  IF(SYSTEM1(30).EQ.2.0.OR.
     1                    SYSTEM1(30).EQ.4.0) THEN
C               UFOCAL OR UAFOCAL, DON'T CONVERT
                      IF(WC.EQ.'MAB3') THEN
                          C1=MAB3(1,I)
                          C2=3.0D0*MAB3(2,I)
                          C3=MAB3(3,I)
                          C4=MAB3(4,I)
                          C5=MAB3(5,I)
                      ELSE
                      END IF
                      IF(WC.EQ.'XMAB3') THEN
                          C1=XMAB3(1,I)
                          C2=3.0D0*XMAB3(2,I)
                          C3=XMAB3(3,I)
                          C4=XMAB3(4,I)
                          C5=XMAB3(5,I)
                      ELSE
                      END IF
                      WRITE(OUTLYNE,2000)I,C1,C2,C3,C4,C5
                      CALL SHOWIT(0)
                  ELSE
                  END IF
 10           CONTINUE
C       NOW HANDEL THE CALCULATION AND PRINTING OF THE
C       TOTALS
C       IN ANY CASE, THE SURFACE CONTRIBUTIONS MUST BE SUMMED
              C1T=0.0
              C2T=0.0
              C3T=0.0
              C4T=0.0
              C5T=0.0
              DO 20 I=0,SF
                  IF(WC.EQ.'MAB3') THEN
                      C1T=C1T+MAB3(1,I)
                      C2T=C2T+(3.0D0*MAB3(2,I))
                      C3T=C3T+MAB3(3,I)
                      C4T=C4T+MAB3(4,I)
                      C5T=C5T+MAB3(5,I)
                  ELSE
                  END IF
                  IF(WC.EQ.'XMAB3') THEN
                      C1T=C1T+XMAB3(1,I)
                      C2T=C2T+(3.0D0*XMAB3(2,I))
                      C3T=C3T+XMAB3(3,I)
                      C4T=C4T+XMAB3(4,I)
                      C5T=C5T+XMAB3(5,I)
                  ELSE
                  END IF
 20           CONTINUE
C       NOW FOR CONVERSIONS
C       MODE UFOCAL
              IF(SYSTEM1(30).EQ.2.0) THEN
C       MODE IS UFOCAL, NO CONVERSIONS
                  GO TO 25
              ELSE
              END IF
C       MODE UAFOCAL
              IF(SYSTEM1(30).EQ.4.0) THEN
C       MODE IS UAFOCAL, NO CONVERSIONS
                  GO TO 25
              ELSE
              END IF
C       MODE FOCAL
              IF(SYSTEM1(30).EQ.1.0.OR.SYSTEM1(30).EQ.3.0) THEN
C       MODE IS FOCAL OR AFOCAL, CONVERT SUMS
C       CHROMATIC ABERRATION
                  C1T=C1T/INV
                  C2T=C2T/INV
                  C3T=C3T/INV
                  C4T=C4T/INV
                  C5T=C5T/INV
                  GO TO 25
              ELSE
C       NO CONVERSION REQUIRED
              END IF
C       PRINT TOTALS WITH LABELING
 25           CONTINUE
              WRITE(OUTLYNE,998)
              CALL SHOWIT(0)
              C1=C1T
              C2=C2T
              C3=C3T
              C4=C4T
              C5=C5T
              WRITE(OUTLYNE,2001) C1,C2,C3,C4,C5
              CALL SHOWIT(0)
              RETURN
          ELSE
C       QUALIFIER NOT "ALL"
          END IF
          IF(SQ.EQ.1.AND.WQ.EQ.'OBJ'.OR.SQ.EQ.1.AND.WQ.EQ.'OB'.OR.
     1    SQ.EQ.1.AND.WQ.EQ.'I'.OR.SQ.EQ.1.AND.WQ.EQ.'IM'.OR.
     2    SQ.EQ.1.AND.WQ.EQ.'IMAGE') THEN
              IF(WQ.EQ.'OB'.OR.WQ.EQ.'OBJ') SF=0
              IF(WQ.EQ.'I'.OR.WQ.EQ.'IM'.OR.WQ.EQ.'IMAGE')
     1        SF=INT(SYSTEM1(20))
              IF(SYSTEM1(30).EQ.1.0.OR.
     1                SYSTEM1(30).EQ.3.0) THEN
C               FOCAL OR AFOCAL, CONVERT
                  IF(WC.EQ.'MAB3') THEN
                      C1=MAB3(1,SF)/INV
                      C2=(3.0D0*MAB3(2,SF))/INV
                      C3=MAB3(3,SF)/INV
                      C4=MAB3(4,SF)/INV
                      C5=MAB3(5,SF)/INV
                  ELSE
                  END IF
                  IF(WC.EQ.'XMAB3') THEN
                      C1=XMAB3(1,SF)/INV
                      C2=(3.0D0*XMAB3(2,SF))/INV
                      C3=XMAB3(3,SF)/INV
                      C4=XMAB3(4,SF)/INV
                      C5=XMAB3(5,SF)/INV
                  ELSE
                  END IF
                  IF(HEADIN) WRITE(OUTLYNE,5000)
                  IF(HEADIN) CALL SHOWIT(0)
                  WRITE(OUTLYNE,2000)SF,C1,C2,C3,C4,C5
                  CALL SHOWIT(0)
                  RETURN
              ELSE
              END IF
              IF(SYSTEM1(30).EQ.2.0.OR.
     1                SYSTEM1(30).EQ.4.0) THEN
C               UFOCAL OR UAFOCAL, DON'T CONVERT
                  IF(WC.EQ.'MAB3') THEN
                      C1=MAB3(1,SF)
                      C2=3.0D0*MAB3(2,SF)
                      C3=MAB3(3,SF)
                      C4=MAB3(4,SF)
                      C5=MAB3(5,SF)
                  ELSE
                  END IF
                  IF(WC.EQ.'XMAB3') THEN
                      C1=XMAB3(1,SF)
                      C2=3.0D0*XMAB3(2,SF)
                      C3=XMAB3(3,SF)
                      C4=XMAB3(4,SF)
                      C5=XMAB3(5,SF)
                  ELSE
                  END IF
                  IF(HEADIN) WRITE(OUTLYNE,5000)
                  IF(HEADIN) CALL SHOWIT(0)
                  WRITE(OUTLYNE,2000)SF,C1,C2,C3,C4,C5
                  CALL SHOWIT(0)
                  RETURN
              ELSE
              END IF
          ELSE
          END IF
          IF(SQ.EQ.1.AND.WQ.NE.'OBJ'.OR.SQ.EQ.1.AND.
     1    WQ.NE.'ALL'.OR.SQ.EQ.1.AND.WQ.NE.'OB'.OR.SQ.EQ.1.AND.
     2    WQ.NE.'I'.OR.SQ.EQ.1.AND.WQ.NE.'IM'.OR.SQ.EQ.1.AND.
     3    WQ.NE.'IMAGE') THEN
              OUTLYNE='INVALID QUALIFIER WORD'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SQ.EQ.0.AND.DF1.EQ.1.OR.SQ.EQ.1.AND.WQ.EQ.'IM'.OR.
     1    SQ.EQ.1.AND.WQ.EQ.'I'.OR.SQ.EQ.1.AND.WQ.EQ.'IMAGE') THEN
              SF=INT(SYSTEM1(20))
C       OUTPUT SYSTEM TOTALS
C
C       THE SURFACE CONTRIBUTIONS MUST BE SUMMED
              C1T=0.0
              C2T=0.0
              C3T=0.0
              C4T=0.0
              C5T=0.0
              DO 220 I=0,SF
                  IF(WC.EQ.'MAB3') THEN
                      C1T=C1T+MAB3(1,I)
                      C2T=C2T+(3.0D0*MAB3(2,I))
                      C3T=C3T+MAB3(3,I)
                      C4T=C4T+MAB3(4,I)
                      C5T=C5T+MAB3(5,I)
                  ELSE
                  END IF
                  IF(WC.EQ.'XMAB3') THEN
                      C1T=C1T+XMAB3(1,I)
                      C2T=C2T+(3.0D0*XMAB3(2,I))
                      C3T=C3T+XMAB3(3,I)
                      C4T=C4T+XMAB3(4,I)
                      C5T=C5T+XMAB3(5,I)
                  ELSE
                  END IF
 220          CONTINUE
C NOW FOR CONVERSIONS
C       MODE UFOCAL
              IF(SYSTEM1(30).EQ.2.0) THEN
C       MODE IS UFOCAL, NO CONVERSIONS
                  GO TO 250
              ELSE
              END IF
C       MODE UAFOCAL
              IF(SYSTEM1(30).EQ.4.0) THEN
C       MODE IS UAFOCAL, NO CONVERSIONS
                  GO TO 250
              ELSE
              END IF
C       MODE FOCAL
              IF(SYSTEM1(30).EQ.1.0.OR.SYSTEM1(30).EQ.3.0) THEN
C       MODE IS FOCAL OR AFOCAL, CONVERT SUMS TO TRANSVERSE
C       CHROMATIC ABERRATION
                  C1T=C1T/INV
                  C2T=C2T/INV
                  C3T=C3T/INV
                  C4T=C4T/INV
                  C5T=C5T/INV
                  GO TO 250
              ELSE
              END IF
 250          CONTINUE
              C1=C1T
              C2=C2T
              C3=C3T
              C4=C4T
              C5=C5T
C       PRINT TOTALS WITH OUT LABELING
              IF(HEADIN) WRITE(OUTLYNE,5000)
              IF(HEADIN) CALL SHOWIT(0)
              WRITE(OUTLYNE,2001) C1,C2,C3,C4,C5
              CALL SHOWIT(0)
              RETURN
          ELSE
C       NOT IMAGE SURFACE
          END IF
C
          IF(SQ.EQ.0.AND.DF1.NE.1) THEN
              I=INT(W1)
              SF=INT(SYSTEM1(20))
              IF(I.GT.SF.OR.I.LT.0) THEN
                  OUTLYNE='SURFACE NUMBER BEYOND LEGAL RANGE'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(SYSTEM1(30).EQ.1.0.OR.
     1                SYSTEM1(30).EQ.3.0) THEN
C               FOCAL OR AFOCAL, CONVERT
                  IF(WC.EQ.'MAB3') THEN
                      C1=MAB3(1,I)/INV
                      C2=(3.0D0*MAB3(2,I))/INV
                      C3=MAB3(3,I)/INV
                      C4=MAB3(4,I)/INV
                      C5=MAB3(5,I)/INV
                  ELSE
                  END IF
                  IF(WC.EQ.'XMAB3') THEN
                      C1=XMAB3(1,I)/INV
                      C2=(3.0D0*XMAB3(2,I))/INV
                      C3=XMAB3(3,I)/INV
                      C4=XMAB3(4,I)/INV
                      C5=XMAB3(5,I)/INV
                  ELSE
                  END IF
                  IF(HEADIN) WRITE(OUTLYNE,5000)
                  IF(HEADIN) CALL SHOWIT(0)
                  WRITE(OUTLYNE,2000)I,C1,C2,C3,C4,C5
                  CALL SHOWIT(0)
                  RETURN
              ELSE
              END IF
              IF(SYSTEM1(30).EQ.2.0.OR.
     1                SYSTEM1(30).EQ.4.0) THEN
C               UFOCAL OR UAFOCAL, DON'T CONVERT
                  IF(WC.EQ.'MAB3') THEN
                      C1=MAB3(1,I)/INV
                      C2=(3.0D0*MAB3(2,I))/INV
                      C3=MAB3(3,I)/INV
                      C4=MAB3(4,I)/INV
                      C5=MAB3(5,I)/INV
                  ELSE
                  END IF
                  IF(WC.EQ.'XMAB3') THEN
                      C1=XMAB3(1,I)/INV
                      C2=(3.0D0*XMAB3(2,I))/INV
                      C3=XMAB3(3,I)/INV
                      C4=XMAB3(4,I)/INV
                      C5=XMAB3(5,I)/INV
                  ELSE
                  END IF
                  IF(HEADIN) WRITE(OUTLYNE,5000)
                  IF(HEADIN) CALL SHOWIT(0)
                  WRITE(OUTLYNE,2000)I,C1,C2,C3,C4,C5
                  CALL SHOWIT(0)
              ELSE
              END IF
              RETURN
          ELSE
          END IF
! 1500   FORMAT(I3,2X,G12.5,2X,G12.5,2X,G12.5,2X,G12.5,
!     1  2X,G12.5)
 2000     FORMAT(I3,2X,G12.5,2X,G12.5,2X,G12.5,2X,G12.5,
     1    2X,G12.5)
 2001     FORMAT(5X,G12.5,2X,G12.5,2X,G12.5,2X,G12.5,2X,G12.5)
 5000     FORMAT('SURF',5X,'SA3 ',10X,'CMA3',10X,'AST3',
     1    10X,'DIS3',10X,'PTZ3')
 6001     FORMAT('(X-Z) PLANE, THIRD ORDER')
 5001     FORMAT('(Y-Z), PLANE THIRD ORDER')
 5002     FORMAT('ABERRATION CONTRIBUTIONS'
     1    ,' - (CFG #',I2,')')
 5501     FORMAT('TRANSVERSE - WITH FINAL SURFACE CONVERSION')
 5502     FORMAT('TRANSVERSE - WITHOUT FINAL SURFACE CONVERSION')
 5503     FORMAT('ANGULAR - WITH FINAL SURFACE CONVERSION')
 5504     FORMAT('ANGULAR - WITHOUT FINAL SURFACE CONVERSION')
 2501     FORMAT(1X)
 998      FORMAT('SYSTEM TOTALS ARE:')
      END
C SUB MMAB5.FOR
      SUBROUTINE MMAB5
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE MMAB5. THIS SUBROUTINE IMPLEMENTS
C       THE MAB5 AND XMAB5 CMD LEVEL COMMAND
          INTEGER I,SF,CW
C
          REAL*8 C1,C2,C3,C4,C5,C1T,C2T,C3T,C4T,C5T
C
          REAL*8 INV
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
C
          CALL PRTRC
C
C       THE MAB5 AND XMAB5 COMMAND ACCEPTS QUALIFIER OR NUMERIC
C       INPUT IN AN EITHER OR MODE. IT DOES NOT ACCEPT STING
C       INPUT. THE VALID QUALIFIERS ARE "ALL" AND "OBJ" AND "OB"
C       "I","IM", AND "IMAGE"
C       THE VALID NUMERIC INPUT IS THE SURFACE NUMBER FOR
C       WHICH IT IS DESIRED TO PRODUCE OUTPUT.
C
          IF(SQ.EQ.1.AND.S1.EQ.1) THEN
              IF(WC.EQ.'MAB5') THEN
                  OUTLYNE='"MAB5" TAKES EITHER QUALIFIER OR NUMERIC INPUT"'
                  CALL SHOWIT(1)
                  OUTLYNE='BUT NOT BOTH'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(WC.EQ.'XMAB5') THEN
                  OUTLYNE='"XMAB5" TAKES EITHER QUALIFIER OR NUMERIC INPUT"'
                  CALL SHOWIT(1)
                  OUTLYNE='BUT NOT BOTH'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
          IF(SST.EQ.1.OR.S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
              IF(WC.EQ.'MAB5') THEN
                  OUTLYNE=
     1            '"MAB5" TAKES NO STRING OR NUMERIC WORD #2 THROUGH #5 INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(WC.EQ.'XMAB5') THEN
                  OUTLYNE=
     1            '"XMAB5" TAKES NO STRING OR NUMERIC WORD #2 THROUGH #5 INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
          IF(SYSTEM1(20).EQ.0.0) THEN
              OUTLYNE='LENS SYSTEM HAS NO SURFACES'
              CALL SHOWIT(1)
              OUTLYNE='NO PARAXIAL OR FIFTH ORDER DATA EXISTS'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
          SF=INT(SYSTEM1(20))
          IF(INT(SYSTEM1(11)).GE.1.AND.INT(SYSTEM1(11)).LE.5) THEN
              CW=INT(SYSTEM1(11))+45
          END IF
          IF(INT(SYSTEM1(11)).GE.6.AND.INT(SYSTEM1(11)).LE.10) THEN
              CW=INT(SYSTEM1(11))+65
          END IF
          INV=1.0D0
          IF(SYSTEM1(30).EQ.1.0D0) THEN
C       MODE IS FOCAL
              IF(WC.EQ.'MAB5')
     1        INV=-2.0*ALENS(CW,(SF-1))*PXTRAY(2,(SF-1))
              IF(WC.EQ.'XMAB5')
     1        INV=-2.0*ALENS(CW,(SF-1))*PXTRAX(2,(SF-1))
              IF(DABS(INV).LE.1.0D-10) THEN
                  CALL MACFAL
                  RETURN
              END IF
          ELSE
          END IF
          IF(SYSTEM1(30).EQ.3.0D0) THEN
C       MODE IS AFOCAL
              IF(WC.EQ.'MAB5')
     1        INV= 2.0*ALENS(CW,(SF-1))*PXTRAY(1,SF)
              IF(WC.EQ.'XMAB5')
     1        INV= 2.0*ALENS(CW,(SF-1))*PXTRAX(1,SF)
              IF(DABS(INV).LE.1.0D-10) THEN
                  CALL MACFAL
                  RETURN
              END IF
          ELSE
          END IF
          IF(INV.EQ.0.0D0) THEN
              OUTLYNE=
     1        'THE LENS (MODE) IS NOT CONSISTENT WITH PARAXIAL VALUES'
              CALL SHOWIT(1)
              OUTLYNE=
     1        'ABERRATIONS ARE NOT CALCULABLE'
              CALL SHOWIT(1)
              IF(SYSTEM1(30).EQ.1.0D0)
     1        OUTLYNE='CHANGE FROM "MODE FOCAL" TO "MODE AFOCAL"'
              IF(SYSTEM1(30).EQ.3.0D0)
     1        OUTLYNE='CHANGE FROM "MODE AFOCAL" TO "MODE FOCAL"'
              CALL SHOWIT(1)
              OUTLYNE='THEN RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SQ.EQ.1.AND.WQ.EQ.'ALL') THEN
              SF=INT(SYSTEM1(20))
              IF(WC.EQ.'MAB5')WRITE(OUTLYNE,5001)
              IF(WC.EQ.'XMAB5')WRITE(OUTLYNE,6001)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,5002) INT(F12)
              CALL SHOWIT(0)
              IF(SYSTEM1(30).EQ.1.0) WRITE(OUTLYNE,5501)
              IF(SYSTEM1(30).EQ.2.0) WRITE(OUTLYNE,5502)
              IF(SYSTEM1(30).EQ.3.0) WRITE(OUTLYNE,5503)
              IF(SYSTEM1(30).EQ.4.0) WRITE(OUTLYNE,5504)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,2501)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,5000)
              CALL SHOWIT(0)
              DO 10 I=0,SF
                  IF(SYSTEM1(30).EQ.1.0.OR.
     1                    SYSTEM1(30).EQ.3.0) THEN
C               FOCAL OR AFOCAL, CONVERT
                      IF(WC.EQ.'MAB5') THEN
                          C1=MAB57(1,I)/INV
                          C2=(MAB57(2,I)+MAB57(3,I))/INV
                          C3=MAB57(10,I)/INV
                          C4=MAB57(12,I)/INV
                          C5=MAB57(11,I)/INV
                      ELSE
                      END IF
                      IF(WC.EQ.'XMAB5') THEN
                          C1=XMAB57(1,I)/INV
                          C2=(XMAB57(2,I)+XMAB57(3,I))/INV
                          C3=XMAB57(10,I)/INV
                          C4=XMAB57(12,I)/INV
                          C5=XMAB57(11,I)/INV
                      ELSE
                      END IF
                      WRITE(OUTLYNE,2000)I,C1,C2,C3,C4,C5
                      CALL SHOWIT(0)
                      GO TO 10
                  ELSE
                  END IF
                  IF(SYSTEM1(30).EQ.2.0.OR.
     1                    SYSTEM1(30).EQ.4.0) THEN
C               UFOCAL OR UAFOCAL, DON'T CONVERT
                      IF(WC.EQ.'MAB5') THEN
                          C1=MAB57(1,I)
                          C2=(MAB57(2,I)+MAB57(3,I))
                          C3=MAB57(10,I)
                          C4=MAB57(12,I)
                          C5=MAB57(11,I)
                      ELSE
                      END IF
                      IF(WC.EQ.'XMAB5') THEN
                          C1=XMAB57(1,I)
                          C2=(XMAB57(2,I)+XMAB57(3,I))
                          C3=XMAB57(10,I)
                          C4=XMAB57(12,I)
                          C5=XMAB57(11,I)
                      ELSE
                      END IF
                      WRITE(OUTLYNE,2000)I,C1,C2,C3,C4,C5
                      CALL SHOWIT(0)
                  ELSE
                  END IF
 10           CONTINUE
C       NOW HANDEL THE CALCULATION AND PRINTING OF THE
C       TOTALS
C       IN ANY CASE, THE SURFACE CONTRIBUTIONS MUST BE SUMMED
              C1T=0.0
              C2T=0.0
              C3T=0.0
              C4T=0.0
              C5T=0.0
              DO 20 I=0,SF
                  IF(WC.EQ.'MAB5') THEN
                      C1T=C1T+MAB57(1,I)
                      C2T=C2T+MAB57(2,I)+MAB57(3,I)
                      C3T=C3T+MAB57(10,I)
                      C4T=C4T+MAB57(12,I)
                      C5T=C5T+MAB57(11,I)
                  ELSE
                  END IF
                  IF(WC.EQ.'XMAB5') THEN
                      C1T=C1T+XMAB57(1,I)
                      C2T=C2T+XMAB57(2,I)+MAB57(3,I)
                      C3T=C3T+XMAB57(10,I)
                      C4T=C4T+XMAB57(12,I)
                      C5T=C5T+XMAB57(11,I)
                  ELSE
                  END IF
 20           CONTINUE
C       NOW FOR CONVERSIONS
C       MODE UFOCAL
              IF(SYSTEM1(30).EQ.2.0) THEN
C       MODE IS UFOCAL, NO CONVERSIONS
                  GO TO 25
              ELSE
              END IF
C       MODE UAFOCAL
              IF(SYSTEM1(30).EQ.4.0) THEN
C       MODE IS UAFOCAL, NO CONVERSIONS
                  GO TO 25
              ELSE
              END IF
C       MODE FOCAL
              IF(SYSTEM1(30).EQ.1.0.OR.SYSTEM1(30).EQ.3.0) THEN
C       MODE IS FOCAL OR AFOCAL, CONVERT SUMS
C       CHROMATIC ABERRATION
                  C1T=C1T/INV
                  C2T=C2T/INV
                  C3T=C3T/INV
                  C4T=C4T/INV
                  C5T=C5T/INV
                  GO TO 25
              ELSE
C       NO CONVERSION REQUIRED
              END IF
 25           CONTINUE
C       PRINT TOTALS WITH LABELING
              WRITE(OUTLYNE,998)
              CALL SHOWIT(0)
              C1=C1T
              C2=C2T
              C3=C3T
              C4=C4T
              C5=C5T
              WRITE(OUTLYNE,2001) C1,C2,C3,C4,C5
              CALL SHOWIT(0)
              RETURN
          ELSE
C       QUALIFIER NOT "ALL"
          END IF
          IF(SQ.EQ.1.AND.WQ.EQ.'OBJ'.OR.SQ.EQ.1.AND.WQ.EQ.'OB'.OR.
     1    SQ.EQ.1.AND.WQ.EQ.'I'.OR.SQ.EQ.1.AND.WQ.EQ.'IM'.OR.
     2    SQ.EQ.1.AND.WQ.EQ.'IMAGE') THEN
              IF(WQ.EQ.'OB'.OR.WQ.EQ.'OBJ') SF=0
              IF(WQ.EQ.'I'.OR.WQ.EQ.'IM'.OR.WQ.EQ.'IMAGE')
     1        SF=INT(SYSTEM1(20))
              IF(SYSTEM1(30).EQ.1.0.OR.
     1                SYSTEM1(30).EQ.3.0) THEN
C               FOCAL OR AFOCAL, CONVERT
                  IF(WC.EQ.'MAB5') THEN
                      C1=MAB57(1,SF)/INV
                      C2=(MAB57(2,SF)+MAB57(3,SF))/INV
                      C3=MAB57(10,SF)/INV
                      C4=MAB57(12,SF)/INV
                      C5=MAB57(11,SF)/INV
                  ELSE
                  END IF
                  IF(WC.EQ.'XMAB5') THEN
                      C1=XMAB57(1,SF)/INV
                      C2=(XMAB57(2,SF)+XMAB57(3,SF))/INV
                      C3=XMAB57(10,SF)/INV
                      C4=XMAB57(12,SF)/INV
                      C5=XMAB57(11,SF)/INV
                  ELSE
                  END IF
                  IF(HEADIN) WRITE(OUTLYNE,5000)
                  IF(HEADIN) CALL SHOWIT(0)
                  WRITE(OUTLYNE,2000)SF,C1,C2,C3,C4,C5
                  CALL SHOWIT(0)
                  RETURN
              ELSE
              END IF
              IF(SYSTEM1(30).EQ.2.0.OR.
     1                SYSTEM1(30).EQ.4.0) THEN
C               UFOCAL OR UAFOCAL, DON'T CONVERT
                  IF(WC.EQ.'MAB5') THEN
                      C1=MAB57(1,SF)
                      C2=(MAB57(2,SF)+MAB57(3,SF))
                      C3=MAB57(10,SF)
                      C4=MAB57(12,SF)
                      C5=MAB57(11,SF)
                  ELSE
                  END IF
                  IF(WC.EQ.'XMAB5') THEN
                      C1=XMAB57(1,SF)
                      C2=(XMAB57(2,SF)+XMAB57(3,SF))
                      C3=XMAB57(10,SF)
                      C4=XMAB57(12,SF)
                      C5=XMAB57(11,SF)
                  ELSE
                  END IF
                  IF(HEADIN) WRITE(OUTLYNE,5000)
                  IF(HEADIN) CALL SHOWIT(0)
                  WRITE(OUTLYNE,2000)SF,C1,C2,C3,C4,C5
                  CALL SHOWIT(0)
                  RETURN
              ELSE
              END IF
          ELSE
          END IF
          IF(SQ.EQ.1.AND.WQ.NE.'OBJ'.OR.SQ.EQ.1.AND.
     1    WQ.NE.'ALL'.OR.SQ.EQ.1.AND.WQ.NE.'OB'.OR.SQ.EQ.1.AND.
     2    WQ.NE.'I'.OR.SQ.EQ.1.AND.WQ.NE.'IM'.OR.SQ.EQ.1.AND.
     3    WQ.NE.'IMAGE') THEN
              OUTLYNE='INVALID QUALIFIER WORD'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SQ.EQ.0.AND.DF1.EQ.1.OR.SQ.EQ.1.AND.WQ.EQ.'IM'.OR.
     1    SQ.EQ.1.AND.WQ.EQ.'I'.OR.SQ.EQ.1.AND.WQ.EQ.'IMAGE') THEN
              SF=INT(SYSTEM1(20))
C       OUTPUT SYSTEM TOTALS
C
C       THE SURFACE CONTRIBUTIONS MUST BE SUMMED
              C1T=0.0
              C2T=0.0
              C3T=0.0
              C4T=0.0
              C5T=0.0
              DO 220 I=0,SF
                  IF(WC.EQ.'MAB5') THEN
                      C1T=C1T+MAB57(1,I)
                      C2T=C2T+MAB57(2,I)+MAB57(3,I)
                      C3T=C3T+MAB57(10,I)
                      C4T=C4T+MAB57(12,I)
                      C5T=C5T+MAB57(11,I)
                  ELSE
                  END IF
                  IF(WC.EQ.'XMAB5') THEN
                      C1T=C1T+XMAB57(1,I)
                      C2T=C2T+XMAB57(2,I)+XMAB57(3,I)
                      C3T=C3T+XMAB57(10,I)
                      C4T=C4T+XMAB57(12,I)
                      C5T=C5T+XMAB57(11,I)
                  ELSE
                  END IF
 220          CONTINUE
C NOW FOR CONVERSIONS
C       MODE UFOCAL
              IF(SYSTEM1(30).EQ.2.0) THEN
C       MODE IS UFOCAL, NO CONVERSIONS
                  GO TO 250
              ELSE
              END IF
C       MODE UAFOCAL
              IF(SYSTEM1(30).EQ.4.0) THEN
C       MODE IS UAFOCAL, NO CONVERSIONS
                  GO TO 250
              ELSE
              END IF
C       MODE FOCAL
              IF(SYSTEM1(30).EQ.1.0.OR.SYSTEM1(30).EQ.3.0) THEN
C       MODE IS FOCAL OR AFOCAL, CONVERT SUMS TO TRANSVERSE
C       CHROMATIC ABERRATION
                  C1T=C1T/INV
                  C2T=C2T/INV
                  C3T=C3T/INV
                  C4T=C4T/INV
                  C5T=C5T/INV
                  GO TO 250
              ELSE
              END IF
 250          CONTINUE
              C1=C1T
              C2=C2T
              C3=C3T
              C4=C4T
              C5=C5T
C       PRINT TOTALS WITH OUT LABELING
              IF(HEADIN) WRITE(OUTLYNE,5000)
              IF(HEADIN) CALL SHOWIT(0)
              WRITE(OUTLYNE,2001) C1,C2,C3,C4,C5
              CALL SHOWIT(0)
              RETURN
          ELSE
C       NOT IMAGE SURFACE
          END IF
C
          IF(SQ.EQ.0.AND.DF1.NE.1) THEN
              I=INT(W1)
              SF=INT(SYSTEM1(20))
              IF(I.GT.SF.OR.I.LT.0) THEN
                  OUTLYNE='SURFACE NUMBER BEYOND LEGAL RANGE'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(SYSTEM1(30).EQ.1.0.OR.
     1                SYSTEM1(30).EQ.3.0) THEN
C               FOCAL OR AFOCAL, CONVERT
                  IF(WC.EQ.'MAB5') THEN
                      C1=MAB57(1,I)/INV
                      C2=(MAB57(2,I)+MAB57(3,I))/INV
                      C3=MAB57(10,I)/INV
                      C4=MAB57(12,I)/INV
                      C5=MAB57(11,I)/INV
                  ELSE
                  END IF
                  IF(WC.EQ.'XMAB5') THEN
                      C1=XMAB57(1,I)/INV
                      C2=(XMAB57(2,I)+XMAB57(3,I))/INV
                      C3=XMAB57(10,I)/INV
                      C4=XMAB57(12,I)/INV
                      C5=XMAB57(11,I)/INV
                  ELSE
                  END IF
                  IF(HEADIN) WRITE(OUTLYNE,5000)
                  IF(HEADIN) CALL SHOWIT(0)
                  WRITE(OUTLYNE,2000)I,C1,C2,C3,C4,C5
                  CALL SHOWIT(0)
                  RETURN
              ELSE
              END IF
              IF(SYSTEM1(30).EQ.2.0.OR.
     1                SYSTEM1(30).EQ.4.0) THEN
C               UFOCAL OR UAFOCAL, DON'T CONVERT
                  IF(WC.EQ.'MAB5') THEN
                      C1=MAB57(1,I)
                      C2=(MAB57(2,I)+MAB57(3,I))
                      C3=MAB57(10,I)
                      C4=MAB57(12,I)
                      C5=MAB57(11,I)
                  ELSE
                  END IF
                  IF(WC.EQ.'XMAB5') THEN
                      C1=XMAB57(1,I)
                      C2=(XMAB57(2,I)+XMAB57(3,I))
                      C3=XMAB57(10,I)
                      C4=XMAB57(12,I)
                      C5=XMAB57(11,I)
                  ELSE
                  END IF
                  IF(HEADIN) WRITE(OUTLYNE,5000)
                  IF(HEADIN) CALL SHOWIT(0)
                  WRITE(OUTLYNE,2000)I,C1,C2,C3,C4,C5
                  CALL SHOWIT(0)
              ELSE
              END IF
              RETURN
          ELSE
          END IF
! 1500   FORMAT(I3,2X,G12.5,2X,G12.5,2X,G12.5,2X,G12.5,
!     1  2X,G12.5)
 2000     FORMAT(I3,2X,G12.5,2X,G12.5,2X,G12.5,2X,G12.5,
     1    2X,G12.5)
 2001     FORMAT(5X,G12.5,2X,G12.5,2X,G12.5,2X,G12.5,2X,G12.5)
 5000     FORMAT('SURF',5X,'SA5 ',10X,'CMA5',10X,'AST5',
     1    10X,'DIS5',10X,'PTZ5')
 5001     FORMAT('(Y-Z) PLANE, FIFTH ORDER')
 6001     FORMAT('(X-Z) PLANE, FIFTH ORDER')
 5002     FORMAT('ABERRATION CONTRIBUTIONS'
     1    ,' - (CFG #',I2,')')
 5501     FORMAT('TRANSVERSE - WITH FINAL SURFACE CONVERSION')
 5502     FORMAT('TRANSVERSE - WITHOUT FINAL SURFACE CONVERSION')
 5503     FORMAT('ANGULAR - WITH FINAL SURFACE CONVERSION')
 5504     FORMAT('ANGULAR - WITHOUT FINAL SURFACE CONVERSION')
 2501     FORMAT(1X)
 998      FORMAT('SYSTEM TOTALS ARE:')
      END
C SUB MMABX5.FOR
      SUBROUTINE MMABX5
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE MMABX5. THIS SUBROUTINE IMPLEMENTS
C       THE MABX5 AND XMABX5 CMD LEVEL COMMAND
          INTEGER I,SF,CW
C
          REAL*8 C1,C2,C3,C4,C5,C1T,C2T,C3T,C4T,C5T
C
          REAL*8 INV
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
C
          CALL PRTRC
C
C       THE MABX5 AND XMABX5 COMMAND ACCEPTS QUALIFIER OR NUMERIC
C       INPUT IN AN EITHER OR MODE. IT DOES NOT ACCEPT STING
C       INPUT. THE VALID QUALIFIERS ARE "ALL" AND "OBJ" AND "OB"
C       "I","IM", AND "IMAGE"
C       THE VALID NUMERIC INPUT IS THE SURFACE NUMBER FOR
C       WHICH IT IS DESIRED TO PRODUCE OUTPUT.
C
          IF(SQ.EQ.1.AND.S1.EQ.1) THEN
              IF(WC.EQ.'MABX5') THEN
                  OUTLYNE='"MABX5" TAKES EITHER QUALIFIER OR NUMERIC INPUT"'
                  CALL SHOWIT(1)
                  OUTLYNE='BUT NOT BOTH'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(WC.EQ.'XMABX5') THEN
                  OUTLYNE='"XMABX5" TAKES EITHER QUALIFIER OR NUMERIC INPUT"'
                  CALL SHOWIT(1)
                  OUTLYNE='BUT NOT BOTH'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
          IF(SST.EQ.1.OR.S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
              IF(WC.EQ.'MABX5') THEN
                  OUTLYNE=
     1            '"MABX5" TAKES NO STRING OR NUMERIC WORD #2 THROUGH #5 INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(WC.EQ.'XMABX5') THEN
                  OUTLYNE=
     1            '"XMABX5" TAKES NO STRING OR NUMERIC WORD #2 THROUGH #5 INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
          IF(SYSTEM1(20).EQ.0.0) THEN
              OUTLYNE='LENS SYSTEM HAS NO SURFACES'
              CALL SHOWIT(1)
              OUTLYNE='NO PARAXIAL OR FIFTH ORDER DATA EXISTS'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
          SF=INT(SYSTEM1(20))
          IF(INT(SYSTEM1(11)).GE.1.AND.INT(SYSTEM1(11)).LE.5) THEN
              CW=INT(SYSTEM1(11))+45
          END IF
          IF(INT(SYSTEM1(11)).GE.6.AND.INT(SYSTEM1(11)).LE.10) THEN
              CW=INT(SYSTEM1(11))+65
          END IF
          INV=1.0D0
          IF(SYSTEM1(30).EQ.1.0D0) THEN
C       MODE IS FOCAL
              IF(WC.EQ.'MABX5')
     1        INV=-2.0*ALENS(CW,(SF-1))*PXTRAY(2,(SF-1))
              IF(WC.EQ.'XMABX5')
     1        INV=-2.0*ALENS(CW,(SF-1))*PXTRAX(2,(SF-1))
              IF(DABS(INV).LE.1.0D-10) THEN
                  CALL MACFAL
                  RETURN
              END IF
          ELSE
          END IF
          IF(SYSTEM1(30).EQ.3.0D0) THEN
C       MODE IS AFOCAL
              IF(WC.EQ.'MABX5')
     1        INV= 2.0*ALENS(CW,(SF-1))*PXTRAY(1,SF)
              IF(WC.EQ.'XMABX5')
     1        INV= 2.0*ALENS(CW,(SF-1))*PXTRAX(1,SF)
              IF(DABS(INV).LE.1.0D-10) THEN
                  CALL MACFAL
                  RETURN
              END IF
          ELSE
          END IF
          IF(INV.EQ.0.0D0) THEN
              OUTLYNE=
     1        'THE LENS (MODE) IS NOT CONSISTENT WITH PARAXIAL VALUES'
              CALL SHOWIT(1)
              OUTLYNE=
     1        'ABERRATIONS ARE NOT CALCULABLE'
              CALL SHOWIT(1)
              IF(SYSTEM1(30).EQ.1.0D0)
     1        OUTLYNE='CHANGE FROM "MODE FOCAL" TO "MODE AFOCAL"'
              IF(SYSTEM1(30).EQ.3.0D0)
     1        OUTLYNE='CHANGE FROM "MODE AFOCAL" TO "MODE FOCAL"'
              CALL SHOWIT(1)
              OUTLYNE='THEN RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SQ.EQ.1.AND.WQ.EQ.'ALL') THEN
              SF=INT(SYSTEM1(20))
              IF(WC.EQ.'MABX5')WRITE(OUTLYNE,5001)
              IF(WC.EQ.'XMABX5')WRITE(OUTLYNE,6001)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,5002) INT(F12)
              CALL SHOWIT(0)
              IF(SYSTEM1(30).EQ.1.0) WRITE(OUTLYNE,5501)
              IF(SYSTEM1(30).EQ.2.0) WRITE(OUTLYNE,5502)
              IF(SYSTEM1(30).EQ.3.0) WRITE(OUTLYNE,5503)
              IF(SYSTEM1(30).EQ.4.0) WRITE(OUTLYNE,5504)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,2501)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,5000)
              CALL SHOWIT(0)
              DO 10 I=0,SF
                  IF(SYSTEM1(30).EQ.1.0.OR.
     1                    SYSTEM1(30).EQ.3.0) THEN
C               FOCAL OR AFOCAL, CONVERT
                      IF(WC.EQ.'MABX5') THEN
                          C1=(MAB57(4,I)+MAB57(5,I)+MAB57(6,I))/INV
                          C2=MAB57(5,I)/INV
                          C3=(MAB57(7,I)+MAB57(8,I))/INV
                          C4=(MAB57(11,I)+(5.0*MAB57(10,I)))/INV
                          C5=(MAB57(11,I)+MAB57(10,I))/INV
                      ELSE
                      END IF
                      IF(WC.EQ.'XMABX5') THEN
                          C1=(XMAB57(4,I)+XMAB57(5,I)+XMAB57(6,I))/INV
                          C2=XMAB57(5,I)/INV
                          C3=(XMAB57(7,I)+XMAB57(8,I))/INV
                          C4=(XMAB57(11,I)+(5.0*XMAB57(10,I)))/INV
                          C5=(XMAB57(11,I)+XMAB57(10,I))/INV
                      ELSE
                      END IF
                      WRITE(OUTLYNE,2000)I,C1,C2,C3,C4,C5
                      CALL SHOWIT(0)
                      GO TO 10
                  ELSE
                  END IF
                  IF(SYSTEM1(30).EQ.2.0.OR.
     1                    SYSTEM1(30).EQ.4.0) THEN
C               UFOCAL OR UAFOCAL, DON'T CONVERT
                      IF(WC.EQ.'MABX5') THEN
                          C1=MAB57(4,I)+MAB57(5,I)+MAB57(6,I)
                          C2=MAB57(5,I)
                          C3=MAB57(7,I)+MAB57(8,I)
                          C4=MAB57(11,I)+(MAB57(10,I)*5.0)
                          C5=MAB57(11,I)+MAB57(10,I)
                      ELSE
                      END IF
                      IF(WC.EQ.'XMABX5') THEN
                          C1=XMAB57(4,I)+XMAB57(5,I)+XMAB57(6,I)
                          C2=XMAB57(5,I)
                          C3=XMAB57(7,I)+XMAB57(8,I)
                          C4=XMAB57(11,I)+(XMAB57(10,I)*5.0)
                          C5=XMAB57(11,I)+XMAB57(10,I)
                      ELSE
                      END IF
                      WRITE(OUTLYNE,2000)I,C1,C2,C3,C4,C5
                      CALL SHOWIT(0)
                  ELSE
                  END IF
 10           CONTINUE
C       NOW HANDEL THE CALCULATION AND PRINTING OF THE
C       TOTALS
C       IN ANY CASE, THE SURFACE CONTRIBUTIONS MUST BE SUMMED
              C1T=0.0
              C2T=0.0
              C3T=0.0
              C4T=0.0
              C5T=0.0
              DO 20 I=0,SF
                  IF(WC.EQ.'MABX5') THEN
                      C1T=C1T+MAB57(4,I)+MAB57(5,I)+MAB57(6,I)
                      C2T=C2T+MAB57(5,I)
                      C3T=C3T+MAB57(7,I)+MAB57(8,I)
                      C4T=C4T+MAB57(11,I)+(MAB57(10,I)*5.0)
                      C5T=C5T+MAB57(10,I)+MAB57(11,I)
                  ELSE
                  END IF
                  IF(WC.EQ.'XMABX5') THEN
                      C1T=C1T+XMAB57(4,I)+XMAB57(5,I)+XMAB57(6,I)
                      C2T=C2T+XMAB57(5,I)
                      C3T=C3T+XMAB57(7,I)+XMAB57(8,I)
                      C4T=C4T+XMAB57(11,I)+(XMAB57(10,I)*5.0)
                      C5T=C5T+XMAB57(10,I)+XMAB57(11,I)
                  ELSE
                  END IF
 20           CONTINUE
C       NOW FOR CONVERSIONS
C       MODE UFOCAL
              IF(SYSTEM1(30).EQ.2.0) THEN
C       MODE IS UFOCAL, NO CONVERSIONS
                  GO TO 25
              ELSE
              END IF
C       MODE UAFOCAL
              IF(SYSTEM1(30).EQ.4.0) THEN
C       MODE IS UAFOCAL, NO CONVERSIONS
                  GO TO 25
              ELSE
              END IF
C       MODE FOCAL
              IF(SYSTEM1(30).EQ.1.0.OR.SYSTEM1(30).EQ.3.0) THEN
C       MODE IS FOCAL OR AFOCAL, CONVERT SUMS
C       CHROMATIC ABERRATION
                  C1T=C1T/INV
                  C2T=C2T/INV
                  C3T=C3T/INV
                  C4T=C4T/INV
                  C5T=C5T/INV
                  GO TO 25
              ELSE
C       NO CONVERSION REQUIRED
              END IF
C       PRINT TOTALS WITH LABELING
 25           CONTINUE
              WRITE(OUTLYNE,998)
              CALL SHOWIT(0)
              C1=C1T
              C2=C2T
              C3=C3T
              C4=C4T
              C5=C5T
              WRITE(OUTLYNE,2001) C1,C2,C3,C4,C5
              CALL SHOWIT(0)
              RETURN
          ELSE
C       QUALIFIER NOT "ALL"
          END IF
          IF(SQ.EQ.1.AND.WQ.EQ.'OBJ'.OR.SQ.EQ.1.AND.WQ.EQ.'OB'.OR.
     1    SQ.EQ.1.AND.WQ.EQ.'I'.OR.SQ.EQ.1.AND.WQ.EQ.'IM'.OR.
     2    SQ.EQ.1.AND.WQ.EQ.'IMAGE') THEN
              IF(WQ.EQ.'OB'.OR.WQ.EQ.'OBJ') SF=0
              IF(WQ.EQ.'I'.OR.WQ.EQ.'IM'.OR.WQ.EQ.'IMAGE')
     1        SF=INT(SYSTEM1(20))
              IF(SYSTEM1(30).EQ.1.0.OR.
     1                SYSTEM1(30).EQ.3.0) THEN
C               FOCAL OR AFOCAL, CONVERT
                  IF(WC.EQ.'MABX5') THEN
                      C1=(MAB57(4,SF)+MAB57(5,SF)+MAB57(6,SF))/INV
                      C2=(MAB57(5,SF))/INV
                      C3=(MAB57(7,SF)+MAB57(8,SF))/INV
                      C4=(MAB57(10,SF)+(5.0*MAB57(11,SF)))/INV
                      C5=(MAB57(10,SF)+MAB57(11,SF))/INV
                  ELSE
                  END IF
                  IF(WC.EQ.'XMABX5') THEN
                      C1=(XMAB57(4,SF)+XMAB57(5,SF)+XMAB57(6,SF))/INV
                      C2=(XMAB57(5,SF))/INV
                      C3=(XMAB57(7,SF)+XMAB57(8,SF))/INV
                      C4=(XMAB57(10,SF)+(5.0*XMAB57(11,SF)))/INV
                      C5=(XMAB57(10,SF)+XMAB57(11,SF))/INV
                  ELSE
                  END IF
                  IF(HEADIN) WRITE(OUTLYNE,5000)
                  IF(HEADIN) CALL SHOWIT(0)
                  WRITE(OUTLYNE,2000)SF,C1,C2,C3,C4,C5
                  CALL SHOWIT(0)
                  RETURN
              ELSE
              END IF
              IF(SYSTEM1(30).EQ.2.0.OR.
     1                SYSTEM1(30).EQ.4.0) THEN
C               UFOCAL OR UAFOCAL, DON'T CONVERT
                  IF(WC.EQ.'MABX5') THEN
                      C1=MAB57(4,SF)+MAB57(5,SF)+MAB57(6,SF)
                      C2=MAB57(5,SF)
                      C3=MAB57(7,SF)+MAB57(8,SF)
                      C4=MAB57(10,SF)+(5.0*MAB57(11,SF))
                      C5=MAB57(10,SF)+MAB57(11,SF)
                  ELSE
                  END IF
                  IF(WC.EQ.'XMABX5') THEN
                      C1=XMAB57(4,SF)+XMAB57(5,SF)+XMAB57(6,SF)
                      C2=XMAB57(5,SF)
                      C3=XMAB57(7,SF)+XMAB57(8,SF)
                      C4=XMAB57(10,SF)+(5.0*XMAB57(11,SF))
                      C5=XMAB57(10,SF)+XMAB57(11,SF)
                  ELSE
                  END IF
                  IF(HEADIN) WRITE(OUTLYNE,5000)
                  IF(HEADIN) CALL SHOWIT(0)
                  WRITE(OUTLYNE,2000)SF,C1,C2,C3,C4,C5
                  CALL SHOWIT(0)
                  RETURN
              ELSE
              END IF
          ELSE
          END IF
          IF(SQ.EQ.1.AND.WQ.NE.'OBJ'.OR.SQ.EQ.1.AND.
     1    WQ.NE.'ALL'.OR.SQ.EQ.1.AND.WQ.NE.'OB'.OR.SQ.EQ.1.AND.
     2    WQ.NE.'I'.OR.SQ.EQ.1.AND.WQ.NE.'IM'.OR.SQ.EQ.1.AND.
     3    WQ.NE.'IMAGE') THEN
              OUTLYNE='INVALID QUALIFIER WORD'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SQ.EQ.0.AND.DF1.EQ.1.OR.SQ.EQ.1.AND.WQ.EQ.'IM'.OR.
     1    SQ.EQ.1.AND.WQ.EQ.'I'.OR.SQ.EQ.1.AND.WQ.EQ.'IMAGE') THEN
              SF=INT(SYSTEM1(20))
C       OUTPUT SYSTEM TOTALS
C
C       THE SURFACE CONTRIBUTIONS MUST BE SUMMED
              C1T=0.0
              C2T=0.0
              C3T=0.0
              C4T=0.0
              C5T=0.0
              DO 220 I=0,SF
                  IF(WC.EQ.'MABX5') THEN
                      C1T=C1T+MAB57(4,I)+MAB57(5,I)+MAB57(6,I)
                      C2T=C2T+MAB57(5,I)
                      C3T=C3T+MAB57(7,I)+MAB57(8,I)
                      C4T=C4T+MAB57(10,I)+(5.0*MAB57(11,I))
                      C5T=C5T+MAB57(10,I)+MAB57(11,I)
                  ELSE
                  END IF
                  IF(WC.EQ.'XMABX5') THEN
                      C1T=C1T+XMAB57(4,I)+XMAB57(5,I)+XMAB57(6,I)
                      C2T=C2T+XMAB57(5,I)
                      C3T=C3T+XMAB57(7,I)+XMAB57(8,I)
                      C4T=C4T+XMAB57(10,I)+(5.0*XMAB57(11,I))
                      C5T=C5T+XMAB57(10,I)+XMAB57(11,I)
                  ELSE
                  END IF
 220          CONTINUE
C NOW FOR CONVERSIONS
C       MODE UFOCAL
              IF(SYSTEM1(30).EQ.2.0) THEN
C       MODE IS UFOCAL, NO CONVERSIONS
                  GO TO 250
              ELSE
              END IF
C       MODE UAFOCAL
              IF(SYSTEM1(30).EQ.4.0) THEN
C       MODE IS UAFOCAL, NO CONVERSIONS
                  GO TO 250
              ELSE
              END IF
C       MODE FOCAL
              IF(SYSTEM1(30).EQ.1.0.OR.SYSTEM1(30).EQ.3.0) THEN
C       MODE IS FOCAL OR AFOCAL, CONVERT SUMS TO TRANSVERSE
C       CHROMATIC ABERRATION
                  C1T=C1T/INV
                  C2T=C2T/INV
                  C3T=C3T/INV
                  C4T=C4T/INV
                  C5T=C5T/INV
                  GO TO 250
              ELSE
              END IF
 250          CONTINUE
              C1=C1T
              C2=C2T
              C3=C3T
              C4=C4T
              C5=C5T
C       PRINT TOTALS WITH OUT LABELING
              IF(HEADIN) WRITE(OUTLYNE,5000)
              IF(HEADIN) CALL SHOWIT(0)
              WRITE(OUTLYNE,2001) C1,C2,C3,C4,C5
              CALL SHOWIT(0)
              RETURN
          ELSE
C       NOT IMAGE SURFACE
          END IF
C
          IF(SQ.EQ.0.AND.DF1.NE.1) THEN
              I=INT(W1)
              SF=INT(SYSTEM1(20))
              IF(I.GT.SF.OR.I.LT.0) THEN
                  OUTLYNE='SURFACE NUMBER BEYOND LEGAL RANGE'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(SYSTEM1(30).EQ.1.0.OR.
     1                SYSTEM1(30).EQ.3.0) THEN
C               FOCAL OR AFOCAL, CONVERT
                  IF(WC.EQ.'MABX5') THEN
                      C1=(MAB57(4,I)+MAB57(5,I)+MAB57(6,I))/INV
                      C2=MAB57(5,I)/INV
                      C3=(MAB57(7,I)+MAB57(8,I))/INV
                      C4=(MAB57(10,I)+(MAB57(11,I)*5.0))/INV
                      C5=(MAB57(10,I)+MAB57(11,I))/INV
                  ELSE
                  END IF
                  IF(WC.EQ.'XMABX5') THEN
                      C1=(XMAB57(4,I)+XMAB57(5,I)+XMAB57(6,I))/INV
                      C2=XMAB57(5,I)/INV
                      C3=(XMAB57(7,I)+XMAB57(8,I))/INV
                      C4=(XMAB57(10,I)+(XMAB57(11,I)*5.0))/INV
                      C5=(XMAB57(10,I)+XMAB57(11,I))/INV
                  ELSE
                  END IF
                  IF(HEADIN) WRITE(OUTLYNE,5000)
                  IF(HEADIN) CALL SHOWIT(0)
                  WRITE(OUTLYNE,2000)I,C1,C2,C3,C4,C5
                  CALL SHOWIT(0)
                  RETURN
              ELSE
              END IF
              IF(SYSTEM1(30).EQ.2.0.OR.
     1                SYSTEM1(30).EQ.4.0) THEN
C               UFOCAL OR UAFOCAL, DON'T CONVERT
                  IF(WC.EQ.'MABX5') THEN
                      C1=MAB57(4,I)+MAB57(5,I)+MAB57(6,I)
                      C2=MAB57(5,I)
                      C3=MAB57(7,I)+MAB57(8,I)
                      C4=MAB57(10,I)+(5.0*MAB57(11,I))
                      C5=MAB57(10,I)+MAB57(11,I)
                  ELSE
                  END IF
                  IF(WC.EQ.'XMABX5') THEN
                      C1=XMAB57(4,I)+XMAB57(5,I)+XMAB57(6,I)
                      C2=XMAB57(5,I)
                      C3=XMAB57(7,I)+XMAB57(8,I)
                      C4=XMAB57(10,I)+(5.0*XMAB57(11,I))
                      C5=XMAB57(10,I)+XMAB57(11,I)
                  ELSE
                  END IF
                  IF(HEADIN) WRITE(OUTLYNE,5000)
                  IF(HEADIN) CALL SHOWIT(0)
                  WRITE(OUTLYNE,2000)I,C1,C2,C3,C4,C5
                  CALL SHOWIT(0)
              ELSE
              END IF
              RETURN
          ELSE
          END IF
! 1500   FORMAT(I3,2X,G12.5,2X,G12.5,2X,G12.5,2X,G12.5,
!     1  2X,G12.5)
 2000     FORMAT(I3,2X,G12.5,2X,G12.5,2X,G12.5,2X,G12.5,
     1    2X,G12.5)
 2001     FORMAT(5X,G12.5,2X,G12.5,2X,G12.5,2X,G12.5,2X,G12.5)
 5000     FORMAT('SURF',5X,'TOBSA',9X,'SOBSA',9X,'ELCMA',
     1    9X,'TAS ',10X,'SAS')
 5001     FORMAT(
     1    '(Y-Z), PLANE FIFTH ORDER (EXTENDED)')
 6001     FORMAT(
     1    '(X-Z) PLANE, FIFTH ORDER (EXTENDED)')
 5002     FORMAT('ABERRATION CONTRIBUTIONS'
     1    ,' - (CFG #',I2,')')
 5501     FORMAT('TRANSVERSE - WITH FINAL SURFACE CONVERSION')
 5502     FORMAT('TRANSVERSE - WITHOUT FINAL SURFACE CONVERSION')
 5503     FORMAT('ANGULAR - WITH FINAL SURFACE CONVERSION')
 5504     FORMAT('ANGULAR - WITHOUT FINAL SURFACE CONVERSION')
 2501     FORMAT(1X)
 998      FORMAT('SYSTEM TOTALS ARE:')
      END
C SUB MMABP3.FOR
      SUBROUTINE MMABP3
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE MMABP3. THIS SUBROUTINE IMPLEMENTS
C       THE MABP3 AND XMABP3(THIRD ORDER EXIT PUPIL ABERRATION)
C       PRINTOUT AT THE
C
          INTEGER I,SF,CW
C
          REAL*8 C1,C2,C3,C4,C5,C1T,C2T,C3T,C4T,C5T
C
          REAL*8 INV
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
C
          CALL PRTRC
C
C       THE MABP3 COMMAND ACCEPTS QUALIFIER OR NUMERIC
C       INPUT IN AN EITHER OR MODE. IT DOES NOT ACCEPT STING
C       INPUT. THE VALID QUALIFIERS ARE "ALL" AND "OBJ" AND "OB"
C       "I","IM", AND "IMAGE"
C       THE VALID NUMERIC INPUT IS THE SURFACE NUMBER FOR
C       WHICH IT IS DESIRED TO PRODUCE OUTPUT.
C
          IF(SQ.EQ.1.AND.S1.EQ.1) THEN
              IF(WC.EQ.'MABP3') THEN
                  OUTLYNE='"MABP3" TAKES EITHER QUALIFIER OR NUMERIC INPUT"'
                  CALL SHOWIT(1)
                  OUTLYNE='BUT NOT BOTH'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(WC.EQ.'XMABP3') THEN
                  OUTLYNE='"XMABP3" TAKES EITHER QUALIFIER OR NUMERIC INPUT"'
                  CALL SHOWIT(1)
                  OUTLYNE='BUT NOT BOTH'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
          IF(SST.EQ.1.OR.S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
              IF(WC.EQ.'MABP3') THEN
                  OUTLYNE=
     1            '"MABP3" TAKES NO STRING OR NUMERIC WORD #2 THROUGH #5 INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(WC.EQ.'XMABP3') THEN
                  OUTLYNE=
     1            '"XMABP3" TAKES NO STRING OR NUMERIC WORD #2 THROUGH #5 INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
          IF(SYSTEM1(20).EQ.0.0) THEN
              OUTLYNE='LENS SYSTEM HAS NO SURFACES'
              CALL SHOWIT(1)
              OUTLYNE='NO PARAXIAL OR THIRD ORDER DATA EXISTS'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
          SF=INT(SYSTEM1(20))
          IF(INT(SYSTEM1(11)).GE.1.AND.INT(SYSTEM1(11)).LE.5) THEN
              CW=INT(SYSTEM1(11))+45
          END IF
          IF(INT(SYSTEM1(11)).GE.6.AND.INT(SYSTEM1(11)).LE.10) THEN
              CW=INT(SYSTEM1(11))+65
          END IF
          INV=1.0D0
          IF(SYSTEM1(30).EQ.1.0D0) THEN
C       MODE IS FOCAL
              IF(WC.EQ.'MABP3')
     1        INV=-2.0*ALENS(CW,(SF-1))*PXTRAY(2,(SF-1))
              IF(WC.EQ.'XMABP3')
     1        INV=-2.0*ALENS(CW,(SF-1))*PXTRAX(2,(SF-1))
              IF(DABS(INV).LE.1.0D-10) THEN
                  CALL MACFAL
                  RETURN
              END IF
          ELSE
          END IF
          IF(SYSTEM1(30).EQ.3.0D0) THEN
C       MODE IS AFOCAL
              IF(WC.EQ.'MABP3')
     1        INV= 2.0*ALENS(CW,(SF-1))*PXTRAY(1,SF)
              IF(WC.EQ.'XMABP3')
     1        INV= 2.0*ALENS(CW,(SF-1))*PXTRAX(1,SF)
              IF(DABS(INV).LE.1.0D-10) THEN
                  CALL MACFAL
                  RETURN
              END IF
          ELSE
          END IF
          IF(INV.EQ.0.0D0) THEN
              OUTLYNE=
     1        'THE LENS (MODE) IS NOT CONSISTENT WITH PARAXIAL VALUES'
              CALL SHOWIT(1)
              OUTLYNE=
     1        'ABERRATIONS ARE NOT CALCULABLE'
              CALL SHOWIT(1)
              IF(SYSTEM1(30).EQ.1.0D0)
     1        OUTLYNE='CHANGE FROM "MODE FOCAL" TO "MODE AFOCAL"'
              IF(SYSTEM1(30).EQ.3.0D0)
     1        OUTLYNE='CHANGE FROM "MODE AFOCAL" TO "MODE FOCAL"'
              CALL SHOWIT(1)
              OUTLYNE='THEN RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SQ.EQ.1.AND.WQ.EQ.'ALL') THEN
              SF=INT(SYSTEM1(20))
              IF(WC.EQ.'MABP3')WRITE(OUTLYNE,5001)
              IF(WC.EQ.'XMABP3')WRITE(OUTLYNE,6001)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,5002) INT(F12)
              CALL SHOWIT(0)
              IF(SYSTEM1(30).EQ.1.0) WRITE(OUTLYNE,5501)
              IF(SYSTEM1(30).EQ.2.0) WRITE(OUTLYNE,5502)
              IF(SYSTEM1(30).EQ.3.0) WRITE(OUTLYNE,5503)
              IF(SYSTEM1(30).EQ.4.0) WRITE(OUTLYNE,5504)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,2501)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,5000)
              CALL SHOWIT(0)
              DO 10 I=0,SF
                  IF(SYSTEM1(30).EQ.1.0.OR.
     1                    SYSTEM1(30).EQ.3.0) THEN
C               FOCAL OR AFOCAL, CONVERT
                      IF(WC.EQ.'MABP3') THEN
                          C1=MAB3(6,I)/INV
                          C2=(3.0D0*MAB3(7,I))/INV
                          C3=MAB3(8,I)/INV
                          C4=MAB3(9,I)/INV
                          C5=MAB3(10,I)/INV
                      ELSE
                      END IF
                      IF(WC.EQ.'XMABP3') THEN
                          C1=XMAB3(6,I)/INV
                          C2=(3.0D0*XMAB3(7,I))/INV
                          C3=XMAB3(8,I)/INV
                          C4=XMAB3(9,I)/INV
                          C5=XMAB3(10,I)/INV
                      ELSE
                      END IF
                      WRITE(OUTLYNE,2000)I,C1,C2,C3,C4,C5
                      CALL SHOWIT(0)
                      GO TO 10
                  ELSE
                  END IF
                  IF(SYSTEM1(30).EQ.2.0.OR.
     1                    SYSTEM1(30).EQ.4.0) THEN
C               UFOCAL OR UAFOCAL, DON'T CONVERT
                      IF(WC.EQ.'MABP3') THEN
                          C1=MAB3(6,I)
                          C2=(3.0D0*MAB3(7,I))
                          C3=MAB3(8,I)
                          C4=MAB3(9,I)
                          C5=MAB3(10,I)
                      ELSE
                      END IF
                      IF(WC.EQ.'XMABP3') THEN
                          C1=XMAB3(6,I)
                          C2=(3.0D0*XMAB3(7,I))
                          C3=XMAB3(8,I)
                          C4=XMAB3(9,I)
                          C5=XMAB3(10,I)
                      ELSE
                      END IF
                      WRITE(OUTLYNE,2000)I,C1,C2,C3,C4,C5
                      CALL SHOWIT(0)
                  ELSE
                  END IF
 10           CONTINUE
C       NOW HANDEL THE CALCULATION AND PRINTING OF THE
C       TOTALS
C       IN ANY CASE, THE SURFACE CONTRIBUTIONS MUST BE SUMMED
              C1T=0.0
              C2T=0.0
              C3T=0.0
              C4T=0.0
              C5T=0.0
              DO 20 I=0,SF
                  IF(WC.EQ.'MABP3') THEN
                      C1T=C1T+MAB3(6,I)
                      C2T=C2T+(3.0D0*MAB3(7,I))
                      C3T=C3T+MAB3(8,I)
                      C4T=C4T+MAB3(9,I)
                      C5T=C5T+MAB3(10,I)
                  ELSE
                  END IF
                  IF(WC.EQ.'XMABP3') THEN
                      C1T=C1T+XMAB3(6,I)
                      C2T=C2T+(3.0D0*XMAB3(7,I))
                      C3T=C3T+XMAB3(8,I)
                      C4T=C4T+XMAB3(9,I)
                      C5T=C5T+XMAB3(10,I)
                  ELSE
                  END IF
 20           CONTINUE
C       NOW FOR CONVERSIONS
C       MODE UFOCAL
              IF(SYSTEM1(30).EQ.2.0) THEN
C       MODE IS UFOCAL, NO CONVERSIONS
                  GO TO 25
              ELSE
              END IF
C       MODE UAFOCAL
              IF(SYSTEM1(30).EQ.4.0) THEN
C       MODE IS UAFOCAL, NO CONVERSIONS
                  GO TO 25
              ELSE
              END IF
C       MODE FOCAL
              IF(SYSTEM1(30).EQ.1.0.OR.SYSTEM1(30).EQ.3.0) THEN
C       MODE IS FOCAL OR AFOCAL, CONVERT SUMS
C       CHROMATIC ABERRATION
                  C1T=C1T/INV
                  C2T=C2T/INV
                  C3T=C3T/INV
                  C4T=C4T/INV
                  C5T=C5T/INV
                  GO TO 25
              ELSE
C       NO CONVERSION REQUIRED
              END IF
C       PRINT TOTALS WITH LABELING
 25           CONTINUE
              WRITE(OUTLYNE,998)
              CALL SHOWIT(0)
              C1=C1T
              C2=C2T
              C3=C3T
              C4=C4T
              C5=C5T
              WRITE(OUTLYNE,2001) C1,C2,C3,C4,C5
              CALL SHOWIT(0)
              RETURN
          ELSE
C       QUALIFIER NOT "ALL"
          END IF
          IF(SQ.EQ.1.AND.WQ.EQ.'OBJ'.OR.SQ.EQ.1.AND.WQ.EQ.'OB'.OR.
     1    SQ.EQ.1.AND.WQ.EQ.'I'.OR.SQ.EQ.1.AND.WQ.EQ.'IM'.OR.
     2    SQ.EQ.1.AND.WQ.EQ.'IMAGE') THEN
              IF(WQ.EQ.'OB'.OR.WQ.EQ.'OBJ') SF=0
              IF(WQ.EQ.'I'.OR.WQ.EQ.'IM'.OR.WQ.EQ.'IMAGE')
     1        SF=INT(SYSTEM1(20))
              IF(SYSTEM1(30).EQ.1.0.OR.
     1                SYSTEM1(30).EQ.3.0) THEN
C               FOCAL OR AFOCAL, CONVERT
                  IF(WC.EQ.'MABP3') THEN
                      C1=MAB3(6,SF)/INV
                      C2=(3.0D0*MAB3(7,SF))/INV
                      C3=MAB3(8,SF)/INV
                      C4=MAB3(9,SF)/INV
                      C5=MAB3(10,SF)/INV
                  ELSE
                  END IF
                  IF(WC.EQ.'XMABP3') THEN
                      C1=XMAB3(6,SF)/INV
                      C2=(3.0D0*XMAB3(7,SF))/INV
                      C3=XMAB3(8,SF)/INV
                      C4=XMAB3(9,SF)/INV
                      C5=XMAB3(10,SF)/INV
                  ELSE
                  END IF
                  IF(HEADIN) WRITE(OUTLYNE,5000)
                  IF(HEADIN) CALL SHOWIT(0)
                  WRITE(OUTLYNE,2000)SF,C1,C2,C3,C4,C5
                  CALL SHOWIT(0)
                  RETURN
              ELSE
              END IF
              IF(SYSTEM1(30).EQ.2.0.OR.
     1                SYSTEM1(30).EQ.4.0) THEN
C               UFOCAL OR UAFOCAL, DON'T CONVERT
                  IF(WC.EQ.'MABP3') THEN
                      C1=MAB3(6,SF)
                      C2=3.0D0*MAB3(7,SF)
                      C3=MAB3(8,SF)
                      C4=MAB3(9,SF)
                      C5=MAB3(10,SF)
                  ELSE
                  END IF
                  IF(WC.EQ.'XMABP3') THEN
                      C1=XMAB3(6,SF)
                      C2=3.0D0*XMAB3(7,SF)
                      C3=XMAB3(8,SF)
                      C4=XMAB3(9,SF)
                      C5=XMAB3(10,SF)
                  ELSE
                  END IF
                  IF(HEADIN) WRITE(OUTLYNE,5000)
                  IF(HEADIN) CALL SHOWIT(0)
                  WRITE(OUTLYNE,2000)SF,C1,C2,C3,C4,C5
                  CALL SHOWIT(0)
                  RETURN
              ELSE
              END IF
          ELSE
          END IF
          IF(SQ.EQ.1.AND.WQ.NE.'OBJ'.OR.SQ.EQ.1.AND.
     1    WQ.NE.'ALL'.OR.SQ.EQ.1.AND.WQ.NE.'OB'.OR.SQ.EQ.1.AND.
     2    WQ.NE.'I'.OR.SQ.EQ.1.AND.WQ.NE.'IM'.OR.SQ.EQ.1.AND.
     3    WQ.NE.'IMAGE') THEN
              OUTLYNE='INVALID QUALIFIER WORD'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SQ.EQ.0.AND.DF1.EQ.1.OR.SQ.EQ.1.AND.WQ.EQ.'IM'.OR.
     1    SQ.EQ.1.AND.WQ.EQ.'I'.OR.SQ.EQ.1.AND.WQ.EQ.'IMAGE') THEN
              SF=INT(SYSTEM1(20))
C       OUTPUT SYSTEM TOTALS
C
C       THE SURFACE CONTRIBUTIONS MUST BE SUMMED
              C1T=0.0
              C2T=0.0
              C3T=0.0
              C4T=0.0
              C5T=0.0
              DO 220 I=0,SF
                  IF(WC.EQ.'MABP3') THEN
                      C1T=C1T+MAB3(6,I)
                      C2T=C2T+(3.0D0*MAB3(7,I))
                      C3T=C3T+MAB3(8,I)
                      C4T=C4T+MAB3(9,I)
                      C5T=C5T+MAB3(10,I)
                  ELSE
                  END IF
                  IF(WC.EQ.'XMABP3') THEN
                      C1T=C1T+XMAB3(6,I)
                      C2T=C2T+(3.0D0*XMAB3(7,I))
                      C3T=C3T+XMAB3(8,I)
                      C4T=C4T+XMAB3(9,I)
                      C5T=C5T+XMAB3(10,I)
                  ELSE
                  END IF
 220          CONTINUE
C NOW FOR CONVERSIONS
C       MODE UFOCAL
              IF(SYSTEM1(30).EQ.2.0) THEN
C       MODE IS UFOCAL, NO CONVERSIONS
                  GO TO 250
              ELSE
              END IF
C       MODE UAFOCAL
              IF(SYSTEM1(30).EQ.4.0) THEN
C       MODE IS UAFOCAL, NO CONVERSIONS
                  GO TO 250
              ELSE
              END IF
C       MODE FOCAL
              IF(SYSTEM1(30).EQ.1.0.OR.SYSTEM1(30).EQ.3.0) THEN
C       MODE IS FOCAL OR AFOCAL, CONVERT SUMS TO TRANSVERSE
C       CHROMATIC ABERRATION
                  C1T=C1T/INV
                  C2T=C2T/INV
                  C3T=C3T/INV
                  C4T=C4T/INV
                  C5T=C5T/INV
                  GO TO 250
              ELSE
              END IF
 250          CONTINUE
              C1=C1T
              C2=C2T
              C3=C3T
              C4=C4T
              C5=C5T
C       PRINT TOTALS WITH OUT LABELING
              IF(HEADIN) WRITE(OUTLYNE,5000)
              IF(HEADIN) CALL SHOWIT(0)
              WRITE(OUTLYNE,2001) C1,C2,C3,C4,C5
              CALL SHOWIT(0)
              RETURN
          ELSE
C       NOT IMAGE SURFACE
          END IF
C
          IF(SQ.EQ.0.AND.DF1.NE.1) THEN
              I=INT(W1)
              SF=INT(SYSTEM1(20))
              IF(I.GT.SF.OR.I.LT.0) THEN
                  OUTLYNE='SURFACE NUMBER BEYOND LEGAL RANGE'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(SYSTEM1(30).EQ.1.0.OR.
     1                SYSTEM1(30).EQ.3.0) THEN
C               FOCAL OR AFOCAL, CONVERT
                  IF(WC.EQ.'MABP3') THEN
                      C1=MAB3(6,I)/INV
                      C2=(3.0D0*MAB3(7,I))/INV
                      C3=MAB3(8,I)/INV
                      C4=MAB3(9,I)/INV
                      C5=MAB3(10,I)/INV
                  ELSE
                  END IF
                  IF(WC.EQ.'XMABP3') THEN
                      C1=XMAB3(6,I)/INV
                      C2=(3.0D0*XMAB3(7,I))/INV
                      C3=XMAB3(8,I)/INV
                      C4=XMAB3(9,I)/INV
                      C5=XMAB3(10,I)/INV
                  ELSE
                  END IF
                  IF(HEADIN) WRITE(OUTLYNE,5000)
                  IF(HEADIN) CALL SHOWIT(0)
                  WRITE(OUTLYNE,2000)I,C1,C2,C3,C4,C5
                  CALL SHOWIT(0)
                  RETURN
              ELSE
              END IF
              IF(SYSTEM1(30).EQ.2.0.OR.
     1                SYSTEM1(30).EQ.4.0) THEN
C               UFOCAL OR UAFOCAL, DON'T CONVERT
                  IF(WC.EQ.'MABP3') THEN
                      C1=MAB3(6,I)
                      C2=3.0D0*MAB3(7,I)
                      C3=MAB3(8,I)
                      C4=MAB3(9,I)
                      C5=MAB3(10,I)
                  ELSE
                  END IF
                  IF(WC.EQ.'XMABP3') THEN
                      C1=XMAB3(6,I)
                      C2=3.0D0*XMAB3(7,I)
                      C3=XMAB3(8,I)
                      C4=XMAB3(9,I)
                      C5=XMAB3(10,I)
                  ELSE
                  END IF
                  IF(HEADIN) WRITE(OUTLYNE,5000)
                  IF(HEADIN) CALL SHOWIT(0)
                  WRITE(OUTLYNE,2000)I,C1,C2,C3,C4,C5
                  CALL SHOWIT(0)
              ELSE
              END IF
              RETURN
          ELSE
          END IF
! 1500   FORMAT(I3,2X,G12.5,2X,G12.5,2X,G12.5,2X,G12.5,
!     1  2X,G12.5)
 2000     FORMAT(I3,2X,G12.5,2X,G12.5,2X,G12.5,2X,G12.5,
     1    2X,G12.5)
 2001     FORMAT(5X,G12.5,2X,G12.5,2X,G12.5,2X,G12.5,2X,G12.5)
 5000     FORMAT('SURF',5X,'PSA3 ',9X,'PCMA3',9X,'PAST3',
     1    9X,'PDIS3',9X,'PPTZ3')
 5001     FORMAT(
     1    '(Y-Z) PLANE, EXIT PUPIL THIRD ORDER')
 6001     FORMAT(
     1    '(X-Z) PLANE, THIRD ORDER EXIT PUPIL')
 5002     FORMAT('ABERRATION CONTRIBUTIONS'
     1    ,' - (CFG #',I2,')')
 5501     FORMAT('TRANSVERSE - WITH FINAL SURFACE CONVERSION')
 5502     FORMAT('TRANSVERSE - WITHOUT FINAL SURFACE CONVERSION')
 5503     FORMAT('ANGULAR - WITH FINAL SURFACE CONVERSION')
 5504     FORMAT('ANGULAR - WITHOUT FINAL SURFACE CONVERSION')
 2501     FORMAT(1X)
 998      FORMAT('SYSTEM TOTALS ARE:')
      END
C SUB SA357.FOR
      SUBROUTINE SA357
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE SA357. THIS SUBROUTINE IMPLEMENTS
C       THE SA357 AND XSA357 CMD LEVEL COMMAND
          INTEGER I,SF,CW
C
          REAL*8 C1,C2,C3
C
          REAL*8
     4    INV,C1T,C2T,C3T
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
C
          CALL PRTRC
C
C       THE SA357 AND XSA357 COMMAND ACCEPTS QUALIFIER OR NUMERIC
C       INPUT IN AN EITHER OR MODE. IT DOES NOT ACCEPT STING
C       INPUT. THE VALID QUALIFIERS ARE "ALL" AND "OBJ" AND "OB"
C       "I","IM", AND "IMAGE"
C       THE VALID NUMERIC INPUT IS THE SURFACE NUMBER FOR
C       WHICH IT IS DESIRED TO PRODUCE OUTPUT.
C
          IF(SQ.EQ.1.AND.S1.EQ.1) THEN
              IF(WC.EQ.'SA357') THEN
                  OUTLYNE='"SA357" TAKES EITHER QUALIFIER OR NUMERIC INPUT"'
                  CALL SHOWIT(1)
                  OUTLYNE='BUT NOT BOTH'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(WC.EQ.'XSA357') THEN
                  OUTLYNE='"XSA357" TAKES EITHER QUALIFIER OR NUMERIC INPUT"'
                  CALL SHOWIT(1)
                  OUTLYNE='BUT NOT BOTH'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
          IF(SST.EQ.1.OR.S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
              IF(WC.EQ.'SA357') THEN
                  OUTLYNE=
     1            '"SA357" TAKES NO STRING OR NUMERIC WORD #2 THROUGH #5 INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(WC.EQ.'XSA357') THEN
                  OUTLYNE=
     1            '"XSA357" TAKES NO STRING OR NUMERIC WORD #2 THROUGH #5 INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
          IF(SYSTEM1(20).EQ.0.0) THEN
              OUTLYNE='LENS SYSTEM HAS NO SURFACES'
              CALL SHOWIT(1)
              OUTLYNE='NO PARAXIAL,3RD, 5TH OR 7TH  ORDER DATA EXISTS'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
          SF=INT(SYSTEM1(20))
          IF(INT(SYSTEM1(11)).GE.1.AND.INT(SYSTEM1(11)).LE.5) THEN
              CW=INT(SYSTEM1(11))+45
          END IF
          IF(INT(SYSTEM1(11)).GE.6.AND.INT(SYSTEM1(11)).LE.10) THEN
              CW=INT(SYSTEM1(11))+65
          END IF
          INV=1.0D0
          IF(SYSTEM1(30).EQ.1.0D0) THEN
C       MODE IS FOCAL
              IF(WC.EQ.'SA357')
     1        INV=-2.0*ALENS(CW,(SF-1))*PXTRAY(2,(SF-1))
              IF(WC.EQ.'XSA357')
     1        INV=-2.0*ALENS(CW,(SF-1))*PXTRAX(2,(SF-1))
              IF(DABS(INV).LE.1.0D-10) THEN
                  CALL MACFAL
                  RETURN
              END IF
          ELSE
          END IF
          IF(SYSTEM1(30).EQ.3.0D0) THEN
C       MODE IS AFOCAL
              IF(WC.EQ.'SA357')
     1        INV= 2.0*ALENS(CW,(SF-1))*PXTRAY(1,SF)
              IF(WC.EQ.'XSA357')
     1        INV= 2.0*ALENS(CW,(SF-1))*PXTRAX(1,SF)
              IF(DABS(INV).LE.1.0D-10) THEN
                  CALL MACFAL
                  RETURN
              END IF
          ELSE
          END IF
          IF(INV.EQ.0.0D0) THEN
              OUTLYNE=
     1        'THE LENS (MODE) IS NOT CONSISTENT WITH PARAXIAL VALUES'
              CALL SHOWIT(1)
              OUTLYNE=
     1        'ABERRATIONS ARE NOT CALCULABLE'
              CALL SHOWIT(1)
              IF(SYSTEM1(30).EQ.1.0D0)
     1        OUTLYNE='CHANGE FROM "MODE FOCAL" TO "MODE AFOCAL"'
              IF(SYSTEM1(30).EQ.3.0D0)
     1        OUTLYNE='CHANGE FROM "MODE AFOCAL" TO "MODE FOCAL"'
              CALL SHOWIT(1)
              OUTLYNE='THEN RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SQ.EQ.1.AND.WQ.EQ.'ALL') THEN
              SF=INT(SYSTEM1(20))
              IF(WC.EQ.'SA357')WRITE(OUTLYNE,5001)
              IF(WC.EQ.'XSA357')WRITE(OUTLYNE,6001)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,5002) INT(F12)
              CALL SHOWIT(0)
              IF(SYSTEM1(30).EQ.1.0) WRITE(OUTLYNE,5501)
              IF(SYSTEM1(30).EQ.2.0) WRITE(OUTLYNE,5502)
              IF(SYSTEM1(30).EQ.3.0) WRITE(OUTLYNE,5503)
              IF(SYSTEM1(30).EQ.4.0) WRITE(OUTLYNE,5504)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,2501)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,5000)
              CALL SHOWIT(0)
              DO 10 I=0,SF
                  IF(SYSTEM1(30).EQ.1.0.OR.
     1                    SYSTEM1(30).EQ.3.0) THEN
C               FOCAL OR AFOCAL, CONVERT
                      IF(WC.EQ.'SA357') THEN
                          C1=MAB3(1,I)/INV
                          C2=MAB57(1,I)/INV
                          C3=MAB57(14,I)/INV
                      ELSE
                      END IF
                      IF(WC.EQ.'XSA357') THEN
                          C1=XMAB3(1,I)/INV
                          C2=XMAB57(1,I)/INV
                          C3=XMAB57(14,I)/INV
                      ELSE
                      END IF
                      WRITE(OUTLYNE,2000)I,C1,C2,C3
                      CALL SHOWIT(0)
                      GO TO 10
                  ELSE
                  END IF
                  IF(SYSTEM1(30).EQ.2.0.OR.
     1                    SYSTEM1(30).EQ.4.0) THEN
C               UFOCAL OR UAFOCAL, DON'T CONVERT
                      IF(WC.EQ.'SA357') THEN
                          C1=MAB3(1,I)
                          C2=MAB57(1,I)
                          C3=MAB57(14,I)
                      ELSE
                      END IF
                      IF(WC.EQ.'XSA357') THEN
                          C1=XMAB3(1,I)
                          C2=XMAB57(1,I)
                          C3=XMAB57(14,I)
                      ELSE
                      END IF
                      WRITE(OUTLYNE,2000)I,C1,C2,C3
                      CALL SHOWIT(0)
                  ELSE
                  END IF
 10           CONTINUE
C       NOW HANDEL THE CALCULATION AND PRINTING OF THE
C       TOTALS
C       IN ANY CASE, THE SURFACE CONTRIBUTIONS MUST BE SUMMED
              C1T=0.0
              C2T=0.0
              C3T=0.0
              DO 20 I=0,SF
                  IF(WC.EQ.'SA357') THEN
                      C1T=C1T+MAB3(1,I)
                      C2T=C2T+MAB57(1,I)
                      C3T=C3T+MAB57(14,I)
                  ELSE
                  END IF
                  IF(WC.EQ.'XSA357') THEN
                      C1T=C1T+XMAB3(1,I)
                      C2T=C2T+XMAB57(1,I)
                      C3T=C3T+XMAB57(14,I)
                  ELSE
                  END IF
 20           CONTINUE
C       NOW FOR CONVERSIONS
C       MODE UFOCAL
              IF(SYSTEM1(30).EQ.2.0) THEN
C       MODE IS UFOCAL, NO CONVERSIONS
                  GO TO 25
              ELSE
              END IF
C       MODE UAFOCAL
              IF(SYSTEM1(30).EQ.4.0) THEN
C       MODE IS UAFOCAL, NO CONVERSIONS
                  GO TO 25
              ELSE
              END IF
C       MODE FOCAL
              IF(SYSTEM1(30).EQ.1.0.OR.SYSTEM1(30).EQ.3.0) THEN
C       MODE IS FOCAL OR AFOCAL, CONVERT SUMS
C       CHROMATIC ABERRATION
                  C1T=C1T/INV
                  C2T=C2T/INV
                  C3T=C3T/INV
                  GO TO 25
              ELSE
C       NO CONVERSION REQUIRED
              END IF
C       PRINT TOTALS WITH LABELING
 25           CONTINUE
              WRITE(OUTLYNE,998)
              CALL SHOWIT(0)
              C1=C1T
              C2=C2T
              C3=C3T
              WRITE(OUTLYNE,2001) C1,C2,C3
              CALL SHOWIT(0)
              RETURN
          ELSE
C       QUALIFIER NOT "ALL"
          END IF
          IF(SQ.EQ.1.AND.WQ.EQ.'OBJ'.OR.SQ.EQ.1.AND.WQ.EQ.'OB'.OR.
     1    SQ.EQ.1.AND.WQ.EQ.'I'.OR.SQ.EQ.1.AND.WQ.EQ.'IM'.OR.
     2    SQ.EQ.1.AND.WQ.EQ.'IMAGE') THEN
              IF(WQ.EQ.'OB'.OR.WQ.EQ.'OBJ') SF=0
              IF(WQ.EQ.'I'.OR.WQ.EQ.'IM'.OR.WQ.EQ.'IMAGE')
     1        SF=INT(SYSTEM1(20))
              IF(SYSTEM1(30).EQ.1.0.OR.
     1                SYSTEM1(30).EQ.3.0) THEN
C               FOCAL OR AFOCAL, CONVERT
                  IF(WC.EQ.'SA357') THEN
                      C1=MAB3(1,SF)/INV
                      C2=MAB57(1,SF)/INV
                      C3=MAB57(14,SF)/INV
                  ELSE
                  END IF
                  IF(WC.EQ.'XSA357') THEN
                      C1=XMAB3(1,SF)/INV
                      C2=XMAB57(1,SF)/INV
                      C3=XMAB57(14,SF)/INV
                  ELSE
                  END IF
                  IF(HEADIN) WRITE(OUTLYNE,5000)
                  IF(HEADIN) CALL SHOWIT(0)
                  WRITE(OUTLYNE,2000)SF,C1,C2,C3
                  CALL SHOWIT(0)
                  RETURN
              ELSE
              END IF
              IF(SYSTEM1(30).EQ.2.0.OR.
     1                SYSTEM1(30).EQ.4.0) THEN
C               UFOCAL OR UAFOCAL, DON'T CONVERT
                  IF(WC.EQ.'SA357') THEN
                      C1=MAB3(1,SF)
                      C2=MAB57(1,SF)
                      C3=MAB57(14,SF)
                  ELSE
                  END IF
                  IF(WC.EQ.'XSA357') THEN
                      C1=XMAB3(1,SF)
                      C2=XMAB57(1,SF)
                      C3=XMAB57(14,SF)
                  ELSE
                  END IF
                  IF(HEADIN) WRITE(OUTLYNE,5000)
                  IF(HEADIN) CALL SHOWIT(0)
                  WRITE(OUTLYNE,2000)SF,C1,C2,C3
                  CALL SHOWIT(0)
                  RETURN
              ELSE
              END IF
          ELSE
          END IF
          IF(SQ.EQ.1.AND.WQ.NE.'OBJ'.OR.SQ.EQ.1.AND.
     1    WQ.NE.'ALL'.OR.SQ.EQ.1.AND.WQ.NE.'OB'.OR.SQ.EQ.1.AND.
     2    WQ.NE.'I'.OR.SQ.EQ.1.AND.WQ.NE.'IM'.OR.SQ.EQ.1.AND.
     3    WQ.NE.'IMAGE') THEN
              OUTLYNE='INVALID QUALIFIER WORD'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SQ.EQ.0.AND.DF1.EQ.1.OR.SQ.EQ.1.AND.WQ.EQ.'IM'.OR.
     1    SQ.EQ.1.AND.WQ.EQ.'I'.OR.SQ.EQ.1.AND.WQ.EQ.'IMAGE') THEN
              SF=INT(SYSTEM1(20))
C       OUTPUT SYSTEM TOTALS
C
C       THE SURFACE CONTRIBUTIONS MUST BE SUMMED
              C1T=0.0
              C2T=0.0
              C3T=0.0
              DO 220 I=0,SF
                  IF(WC.EQ.'SA357') THEN
                      C1T=C1T+MAB3(1,I)
                      C2T=C2T+MAB57(1,I)
                      C3T=C3T+MAB57(14,I)
                  ELSE
                  END IF
                  IF(WC.EQ.'XSA357') THEN
                      C1T=C1T+XMAB3(1,I)
                      C2T=C2T+XMAB57(1,I)
                      C3T=C3T+XMAB57(14,I)
                  ELSE
                  END IF
 220          CONTINUE
C NOW FOR CONVERSIONS
C       MODE UFOCAL
              IF(SYSTEM1(30).EQ.2.0) THEN
C       MODE IS UFOCAL, NO CONVERSIONS
                  GO TO 250
              ELSE
              END IF
C       MODE UAFOCAL
              IF(SYSTEM1(30).EQ.4.0) THEN
C       MODE IS UAFOCAL, NO CONVERSIONS
                  GO TO 250
              ELSE
              END IF
C       MODE FOCAL
              IF(SYSTEM1(30).EQ.1.0.OR.SYSTEM1(30).EQ.3.0) THEN
C       MODE IS FOCAL OR AFOCAL, CONVERT SUMS TO TRANSVERSE
C       CHROMATIC ABERRATION
                  C1T=C1T/INV
                  C2T=C2T/INV
                  C3T=C3T/INV
                  GO TO 250
              ELSE
              END IF
 250          CONTINUE
              C1=C1T
              C2=C2T
              C3=C3T
C       PRINT TOTALS WITH OUT LABELING
              IF(HEADIN) WRITE(OUTLYNE,5000)
              IF(HEADIN) CALL SHOWIT(0)
              WRITE(OUTLYNE,2001) C1,C2,C3
              CALL SHOWIT(0)
              RETURN
          ELSE
C       NOT IMAGE SURFACE
          END IF
C
          IF(SQ.EQ.0.AND.DF1.NE.1) THEN
              I=INT(W1)
              SF=INT(SYSTEM1(20))
              IF(I.GT.SF.OR.I.LT.0) THEN
                  OUTLYNE='SURFACE NUMBER BEYOND LEGAL RANGE'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(SYSTEM1(30).EQ.1.0.OR.
     1                SYSTEM1(30).EQ.3.0) THEN
C               FOCAL OR AFOCAL, CONVERT
                  IF(WC.EQ.'SA357') THEN
                      C1=MAB3(1,I)/INV
                      C2=MAB57(1,I)/INV
                      C3=MAB57(14,I)/INV
                  ELSE
                  END IF
                  IF(WC.EQ.'XSA357') THEN
                      C1=XMAB3(1,I)/INV
                      C2=XMAB57(1,I)/INV
                      C3=XMAB57(14,I)/INV
                  ELSE
                  END IF
                  IF(HEADIN) WRITE(OUTLYNE,5000)
                  IF(HEADIN) CALL SHOWIT(0)
                  WRITE(OUTLYNE,2000)I,C1,C2,C3
                  CALL SHOWIT(0)
                  RETURN
              ELSE
              END IF
              IF(SYSTEM1(30).EQ.2.0.OR.
     1                SYSTEM1(30).EQ.4.0) THEN
C               UFOCAL OR UAFOCAL, DON'T CONVERT
                  IF(WC.EQ.'SA357') THEN
                      C1=MAB3(1,I)
                      C2=MAB57(1,I)
                      C3=MAB57(14,I)
                  ELSE
                  END IF
                  IF(WC.EQ.'XSA357') THEN
                      C1=XMAB3(1,I)
                      C2=XMAB57(1,I)
                      C3=XMAB57(14,I)
                  ELSE
                  END IF
                  IF(HEADIN) WRITE(OUTLYNE,5000)
                  IF(HEADIN) CALL SHOWIT(0)
                  WRITE(OUTLYNE,2000)I,C1,C2,C3
                  CALL SHOWIT(0)
              ELSE
              END IF
              RETURN
          ELSE
          END IF
! 1500   FORMAT(I3,2X,G12.5,2X,G12.5,2X,G12.5)
 2000     FORMAT(I3,2X,G12.5,2X,G12.5,2X,G12.5)
 2001     FORMAT(5X,G12.5,2X,G12.5,2X,G12.5)
 5000     FORMAT('SURF',5X,'SA3 ',10X,'SA5 ',10X,'SA7')
 5001     FORMAT('(Y-Z), PLANE SPHERICAL ABERRATION')
 6001     FORMAT('(X-Z), PLANE SPHERICAL ABERRATION')
 5002     FORMAT('(3RD,5TH,7TH) CONTRIBUTIONS'
     1    ,' - (CFG #',I2,')')
 5501     FORMAT('TRANSVERSE - WITH FINAL SURFACE CONVERSION')
 5502     FORMAT('TRANSVERSE - WITHOUT FINAL SURFACE CONVERSION')
 5503     FORMAT('ANGULAR - WITH FINAL SURFACE CONVERSION')
 5504     FORMAT('ANGULAR - WITHOUT FINAL SURFACE CONVERSION')
 2501     FORMAT(1X)
 998      FORMAT('SYSTEM TOTALS ARE:')
      END
