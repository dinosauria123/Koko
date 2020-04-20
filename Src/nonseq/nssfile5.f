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

C       FIFTH COLLECTION OF NSS FILES

      SUBROUTINE NSS_LIST_VERTEX
          USE NSSMOD
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE NSSVERT.INC. THIS SUBROUTINE IMPLEMENTS
C       CREATION OF GLOBAL NSSVERTEX DATA FOR THE NSS DATABASE
C
          INTEGER J,I
C
          REAL*8 A,AN,A11,A12,A13,A21,A22,A23,A31,A32,A33
     1    ,B11,B12,B13,B21,B22,B23,B31,B32,B33
     2    ,C11,C12,C13,C21,C22,C23,C31,C32,C33
     3    ,LX,LY,LZ,MX,MY,MZ,NX,NY,NZ,TX,TY,TZ
     4    ,TLX,TLY,TLZ,TMX,TMY,TMZ,TNX,TNY,TNZ
     5    ,TTLX,TTLY,TTLZ,TTMX,TTMY,TTMZ,TTNX,TTNY,TTNZ
     6    ,AEEA,BEEB,CEEC,XEEX,YEEY,ZEEZ,JLX1,JMX1,JNX1,JLY1,JMY1,JNY1
     7    ,JLZ1,JMZ1,JNZ1

C
          INCLUDE 'datmai.inc'
C
C       ROTATION MATRICIES FUNCTIONS
C
C       A11
          A11(AN)=1.0D0
C       A12
          A12(AN)=0.0D0
C       A13
          A13(AN)=0.0D0
C       A21
          A21(AN)=0.0D0
C       A22
          A22(AN)=DCOS(AN)
C       A23
          A23(AN)=-DSIN(AN)
C       A31
          A31(AN)=0.0D0
C       A32
          A32(AN)=DSIN(AN)
C       A33
          A33(AN)=DCOS(AN)
C       B11
          B11(AN)=DCOS(AN)
C       B12
          B12(AN)=0.0D0
C       B13
          B13(AN)=DSIN(AN)
C       B21
          B21(AN)=0.0D0
C       B22
          B22(AN)=1.0D0
C       B23
          B23(AN)=0.0D0
C       B31
          B31(AN)=-DSIN(AN)
C       B32
          B32(AN)=0.0D0
C       B33
          B33(AN)=DCOS(AN)
C       C11
          C11(AN)=DCOS(AN)
C       C12
          C12(AN)=DSIN(AN)
C       C13
          C13(AN)=0.0D0
C       C21
          C21(AN)=-DSIN(AN)
C       C22
          C22(AN)=DCOS(AN)
C       C23
          C23(AN)=0.0D0
C       C31
          C31(AN)=0.0D0
C       C32
          C32(AN)=0.0D0
C       C33
          C33(AN)=1.0D0

          DO J=1,MAXS
              NSSVERTEX(1,J)=0.0D0
              NSSVERTEX(2,J)=0.0D0
              NSSVERTEX(3,J)=0.0D0
C       THE DIRECTION COSINE OF THE LOCAL X-AXIS IS:
              TX=0.0D0
              TY=0.0D0
              TZ=0.0D0
              LX=1.0D0
              MX=0.0D0
              NX=0.0D0
C       THE DIRECTION COSINE OF THE LOCAL Y-AXIS IS:
              LY=0.0D0
              MY=1.0D0
              NY=0.0D0
C       THE DIRECTION COSINE OF THE LOCAL Z-AXIS IS:
              LZ=0.0D0
              MZ=0.0D0
              NZ=1.0D0
C
              XEEX=0.0D0
              YEEY=0.0D0
              ZEEZ=0.0D0
C
              NSSVERTEX(0,J)=0.0D0
              IF(NSSALENS(100,J).EQ.1.0D0) THEN
C
C       IS TRANSFORMATION REFERENCED TO ANOTHER SURFACE
C       INSTEAD OF TO THE GLOBAL ORIGIN
                  NSSNEST(0:200)=-1
                  INEST=0
                  IF(NSSALENS(37,J).NE.-1.0D0) THEN
                      NSSNEST(1)=INT(NSSALENS(37,J))
                      INEST=1
                      DO I=1,199
                          IF(NSSALENS(37,NSSNEST(I)).NE.-1.0D0) THEN
                              NSSNEST(I+1)=INT(NSSALENS(37,NSSNEST(I)))
                              INEST=I+1
                          ELSE
                              EXIT
                          END IF
                      END DO
                  END IF
                  IF(NSSALENS(37,J).NE.-1.0D0) THEN
C       NOW PERFORM THE REFERENCED
C       WE HAVE A VALID SURFACE TO TRANSFORM
                      NSSVERTEX(0,J)=1.0D0
                      XEEX=NSSALENS(34,INT(NSSALENS(37,J)))
                      YEEY=NSSALENS(35,INT(NSSALENS(37,J)))
                      ZEEZ=NSSALENS(36,INT(NSSALENS(37,J)))
C
C NEG SIGNS ARE USED BECAUSE THE TRANSFORMS WERE ORIGINALLY DESIGNED
C       FOR CODE-V ANGLE SIGN CONVENTIONS FOR ALPHA AND BETA WHICH ARE
C       LEFT HAND AND NOT RIGHT HAND POSITIVE
                      AEEA=-NSSALENS(40,INT(NSSALENS(37,J)))
                      BEEB=-NSSALENS(41,INT(NSSALENS(37,J)))
                      CEEC= NSSALENS(42,INT(NSSALENS(37,J)))
C
                      TX=XEEX
                      TY=YEEY
                      TZ=ZEEZ
C       THE DIRECTION COSINE OF THE LOCAL X-AXIS IS:
                      TLX=LX
                      TMX=MX
                      TNX=NX
C       THE DIRECTION COSINE OF THE LOCAL Y-AXIS IS:
                      TLY=LY
                      TMY=MY
                      TNY=NY
C       THE DIRECTION COSINE OF THE LOCAL Z-AXIS IS:
                      TLZ=LZ
                      TMZ=MZ
                      TNZ=NZ
                      A=-CEEC*(PII/180.0D0)
                      JLX1=(TLX*C11(A))+(TMX*C12(A))+(TNX*C13(A))
                      JMX1=(TLX*C21(A))+(TMX*C22(A))+(TNX*C23(A))
                      JNX1=(TLX*C31(A))+(TMX*C32(A))+(TNX*C33(A))
                      JLY1=(TLY*C11(A))+(TMY*C12(A))+(TNY*C13(A))
                      JMY1=(TLY*C21(A))+(TMY*C22(A))+(TNY*C23(A))
                      JNY1=(TLY*C31(A))+(TMY*C32(A))+(TNY*C33(A))
                      JLZ1=(TLZ*C11(A))+(TMZ*C12(A))+(TNZ*C13(A))
                      JMZ1=(TLZ*C21(A))+(TMZ*C22(A))+(TNZ*C23(A))
                      JNZ1=(TLZ*C31(A))+(TMZ*C32(A))+(TNZ*C33(A))
                      TLX=JLX1
                      TMX=JMX1
                      TNX=JNX1
                      TLY=JLY1
                      TMY=JMY1
                      TNY=JNY1
                      TLZ=JLZ1
                      TMZ=JMZ1
                      TNZ=JNZ1
                      A=-BEEB*(PII/180.0D0)
                      JLX1=(TLX*B11(A))+(TMX*B12(A))+(TNX*B13(A))
                      JMX1=(TLX*B21(A))+(TMX*B22(A))+(TNX*B23(A))
                      JNX1=(TLX*B31(A))+(TMX*B32(A))+(TNX*B33(A))
                      JLY1=(TLY*B11(A))+(TMY*B12(A))+(TNY*B13(A))
                      JMY1=(TLY*B21(A))+(TMY*B22(A))+(TNY*B23(A))
                      JNY1=(TLY*B31(A))+(TMY*B32(A))+(TNY*B33(A))
                      JLZ1=(TLZ*B11(A))+(TMZ*B12(A))+(TNZ*B13(A))
                      JMZ1=(TLZ*B21(A))+(TMZ*B22(A))+(TNZ*B23(A))
                      JNZ1=(TLZ*B31(A))+(TMZ*B32(A))+(TNZ*B33(A))
                      TLX=JLX1
                      TMX=JMX1
                      TNX=JNX1
                      TLY=JLY1
                      TMY=JMY1
                      TNY=JNY1
                      TLZ=JLZ1
                      TMZ=JMZ1
                      TNZ=JNZ1
                      A=-AEEA*(PII/180.0D0)
                      JLX1=(TLX*A11(A))+(TMX*A12(A))+(TNX*A13(A))
                      JMX1=(TLX*A21(A))+(TMX*A22(A))+(TNX*A23(A))
                      JNX1=(TLX*A31(A))+(TMX*A32(A))+(TNX*A33(A))
                      JLY1=(TLY*A11(A))+(TMY*A12(A))+(TNY*A13(A))
                      JMY1=(TLY*A21(A))+(TMY*A22(A))+(TNY*A23(A))
                      JNY1=(TLY*A31(A))+(TMY*A32(A))+(TNY*A33(A))
                      JLZ1=(TLZ*A11(A))+(TMZ*A12(A))+(TNZ*A13(A))
                      JMZ1=(TLZ*A21(A))+(TMZ*A22(A))+(TNZ*A23(A))
                      JNZ1=(TLZ*A31(A))+(TMZ*A32(A))+(TNZ*A33(A))
                      TLX=JLX1
                      TMX=JMX1
                      TNX=JNX1
                      TLY=JLY1
                      TMY=JMY1
                      TNY=JNY1
                      TLZ=JLZ1
                      TMZ=JMZ1
                      TNZ=JNZ1
C     NOW THE DIR COSINES ARE IN THE LOCAL COORDINATES SYSTEM OF
C     SURFACE I. TRANSFORM THEM TO THE GLOBAL COORDINATE SYSTEM
                      TTLX=LX
                      TTMX=MX
                      TTNX=NX
                      TTLY=LY
                      TTMY=MY
                      TTNY=NY
                      TTLZ=LZ
                      TTMZ=MZ
                      TTNZ=NZ
                      LX=(TLX*TTLX)+(TMX*TTLY)+(TNX*TTLZ)
                      MX=(TLX*TTMX)+(TMX*TTMY)+(TNX*TTMZ)
                      NX=(TLX*TTNX)+(TMX*TTNY)+(TNX*TTNZ)
                      LY=(TLY*TTLX)+(TMY*TTLY)+(TNY*TTLZ)
                      MY=(TLY*TTMX)+(TMY*TTMY)+(TNY*TTMZ)
                      NY=(TLY*TTNX)+(TMY*TTNY)+(TNY*TTNZ)
                      LZ=(TLZ*TTLX)+(TMZ*TTLY)+(TNZ*TTLZ)
                      MZ=(TLZ*TTMX)+(TMZ*TTMY)+(TNZ*TTMZ)
                      NZ=(TLZ*TTNX)+(TMZ*TTNY)+(TNZ*TTNZ)
                  END IF
C       NOW PERFORM THE TRANSFORMATION ATTACHED TO THIS SURFACE
C       WE HAVE A VALID SURFACE TO TRANSFORM
                  NSSVERTEX(0,J)=1.0D0
                  XEEX=NSSALENS(34,J)
                  YEEY=NSSALENS(35,J)
                  ZEEZ=NSSALENS(36,J)
C
C NEG SIGNS ARE USED BECAUSE THE TRANSFORMS WERE ORIGINALLY DESIGNED
C       FOR CODE-V ANGLE SIGN CONVENTIONS FOR ALPHA AND BETA WHICH ARE
C       LEFT HAND AND NOT RIGHT HAND POSITIVE
                  AEEA=-NSSALENS(40,J)
                  BEEB=-NSSALENS(41,J)
                  CEEC=NSSALENS(42,J)

                  TX=NSSVERTEX(1,J)+(ZEEZ*LZ)+(XEEX*LX)+(YEEY*LY)
                  TY=NSSVERTEX(2,J)+(ZEEZ*MZ)+(XEEX*MX)+(YEEY*MY)
                  TZ=NSSVERTEX(3,J)+(ZEEZ*NZ)+(XEEX*NX)+(YEEY*NY)
C
                  NSSVERTEX(1,J)=TX
                  NSSVERTEX(2,J)=TY
                  NSSVERTEX(3,J)=TZ
C       THE DIRECTION COSINE OF THE LOCAL X-AXIS IS:
                  TLX=LX
                  TMX=MX
                  TNX=NX
C       THE DIRECTION COSINE OF THE LOCAL Y-AXIS IS:
                  TLY=LY
                  TMY=MY
                  TNY=NY
C       THE DIRECTION COSINE OF THE LOCAL Z-AXIS IS:
                  TLZ=LZ
                  TMZ=MZ
                  TNZ=NZ
                  A=-CEEC*(PII/180.0D0)
                  JLX1=(TLX*C11(A))+(TMX*C12(A))+(TNX*C13(A))
                  JMX1=(TLX*C21(A))+(TMX*C22(A))+(TNX*C23(A))
                  JNX1=(TLX*C31(A))+(TMX*C32(A))+(TNX*C33(A))
                  JLY1=(TLY*C11(A))+(TMY*C12(A))+(TNY*C13(A))
                  JMY1=(TLY*C21(A))+(TMY*C22(A))+(TNY*C23(A))
                  JNY1=(TLY*C31(A))+(TMY*C32(A))+(TNY*C33(A))
                  JLZ1=(TLZ*C11(A))+(TMZ*C12(A))+(TNZ*C13(A))
                  JMZ1=(TLZ*C21(A))+(TMZ*C22(A))+(TNZ*C23(A))
                  JNZ1=(TLZ*C31(A))+(TMZ*C32(A))+(TNZ*C33(A))
                  TLX=JLX1
                  TMX=JMX1
                  TNX=JNX1
                  TLY=JLY1
                  TMY=JMY1
                  TNY=JNY1
                  TLZ=JLZ1
                  TMZ=JMZ1
                  TNZ=JNZ1
                  A=-BEEB*(PII/180.0D0)
                  JLX1=(TLX*B11(A))+(TMX*B12(A))+(TNX*B13(A))
                  JMX1=(TLX*B21(A))+(TMX*B22(A))+(TNX*B23(A))
                  JNX1=(TLX*B31(A))+(TMX*B32(A))+(TNX*B33(A))
                  JLY1=(TLY*B11(A))+(TMY*B12(A))+(TNY*B13(A))
                  JMY1=(TLY*B21(A))+(TMY*B22(A))+(TNY*B23(A))
                  JNY1=(TLY*B31(A))+(TMY*B32(A))+(TNY*B33(A))
                  JLZ1=(TLZ*B11(A))+(TMZ*B12(A))+(TNZ*B13(A))
                  JMZ1=(TLZ*B21(A))+(TMZ*B22(A))+(TNZ*B23(A))
                  JNZ1=(TLZ*B31(A))+(TMZ*B32(A))+(TNZ*B33(A))
                  TLX=JLX1
                  TMX=JMX1
                  TNX=JNX1
                  TLY=JLY1
                  TMY=JMY1
                  TNY=JNY1
                  TLZ=JLZ1
                  TMZ=JMZ1
                  TNZ=JNZ1
                  A=-AEEA*(PII/180.0D0)
                  JLX1=(TLX*A11(A))+(TMX*A12(A))+(TNX*A13(A))
                  JMX1=(TLX*A21(A))+(TMX*A22(A))+(TNX*A23(A))
                  JNX1=(TLX*A31(A))+(TMX*A32(A))+(TNX*A33(A))
                  JLY1=(TLY*A11(A))+(TMY*A12(A))+(TNY*A13(A))
                  JMY1=(TLY*A21(A))+(TMY*A22(A))+(TNY*A23(A))
                  JNY1=(TLY*A31(A))+(TMY*A32(A))+(TNY*A33(A))
                  JLZ1=(TLZ*A11(A))+(TMZ*A12(A))+(TNZ*A13(A))
                  JMZ1=(TLZ*A21(A))+(TMZ*A22(A))+(TNZ*A23(A))
                  JNZ1=(TLZ*A31(A))+(TMZ*A32(A))+(TNZ*A33(A))
                  TLX=JLX1
                  TMX=JMX1
                  TNX=JNX1
                  TLY=JLY1
                  TMY=JMY1
                  TNY=JNY1
                  TLZ=JLZ1
                  TMZ=JMZ1
                  TNZ=JNZ1
                  NSSVERTEX(4,J)=TLX
                  NSSVERTEX(5,J)=TMX
                  NSSVERTEX(6,J)=TNX
                  NSSVERTEX(7,J)=TLY
                  NSSVERTEX(8,J)=TMY
                  NSSVERTEX(9,J)=TNY
                  NSSVERTEX(10,J)=TLZ
                  NSSVERTEX(11,J)=TMZ
                  NSSVERTEX(12,J)=TNZ
              ELSE
C       NOT A USED SURFACE NUMBER IN THIS OBJECT
              END IF
          END DO
C
          DO J=1,MAXS
              IF(NSSVERTEX(0,J).EQ.1.0D0) THEN
                  WRITE(OUTLYNE,10) J
                  CALL SHOWIT(0)
 10               FORMAT('GLOBAL VERTEX DATA FOR SURFACE :',I6)
                  WRITE(OUTLYNE,20) NSSVERTEX(1,J),NSSVERTEX(2,J)
     1            ,NSSVERTEX(3,J)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,20) NSSVERTEX(4,J),NSSVERTEX(5,J)
     1            ,NSSVERTEX(6,J)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,20) NSSVERTEX(7,J),NSSVERTEX(8,J)
     1            ,NSSVERTEX(9,J)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,20) NSSVERTEX(10,J),NSSVERTEX(11,J)
     1            ,NSSVERTEX(12,J)
                  CALL SHOWIT(0)
 20               FORMAT(D23.15,5X,D23.15,5X,D23.15)
              END IF
          END DO
          RETURN
      END


      SUBROUTINE NSS_VERTEX_SILENT
          USE NSSMOD
C
          IMPLICIT NONE
C
          INTEGER J,I
C
          REAL*8 A,AN,A11,A12,A13,A21,A22,A23,A31,A32,A33
     1    ,B11,B12,B13,B21,B22,B23,B31,B32,B33
     2    ,C11,C12,C13,C21,C22,C23,C31,C32,C33
     3    ,LX,LY,LZ,MX,MY,MZ,NX,NY,NZ,TX,TY,TZ
     4    ,TLX,TLY,TLZ,TMX,TMY,TMZ,TNX,TNY,TNZ
     5    ,TTLX,TTLY,TTLZ,TTMX,TTMY,TTMZ,TTNX,TTNY,TTNZ
     6    ,AEEA,BEEB,CEEC,XEEX,YEEY,ZEEZ,JLX1,JMX1,JNX1,JLY1,JMY1,JNY1
     7    ,JLZ1,JMZ1,JNZ1

C
          INCLUDE 'datmai.inc'
C
C       ROTATION MATRICIES FUNCTIONS
C
C       A11
          A11(AN)=1.0D0
C       A12
          A12(AN)=0.0D0
C       A13
          A13(AN)=0.0D0
C       A21
          A21(AN)=0.0D0
C       A22
          A22(AN)=DCOS(AN)
C       A23
          A23(AN)=-DSIN(AN)
C       A31
          A31(AN)=0.0D0
C       A32
          A32(AN)=DSIN(AN)
C       A33
          A33(AN)=DCOS(AN)
C       B11
          B11(AN)=DCOS(AN)
C       B12
          B12(AN)=0.0D0
C       B13
          B13(AN)=DSIN(AN)
C       B21
          B21(AN)=0.0D0
C       B22
          B22(AN)=1.0D0
C       B23
          B23(AN)=0.0D0
C       B31
          B31(AN)=-DSIN(AN)
C       B32
          B32(AN)=0.0D0
C       B33
          B33(AN)=DCOS(AN)
C       C11
          C11(AN)=DCOS(AN)
C       C12
          C12(AN)=DSIN(AN)
C       C13
          C13(AN)=0.0D0
C       C21
          C21(AN)=-DSIN(AN)
C       C22
          C22(AN)=DCOS(AN)
C       C23
          C23(AN)=0.0D0
C       C31
          C31(AN)=0.0D0
C       C32
          C32(AN)=0.0D0
C       C33
          C33(AN)=1.0D0
C
C
          DO J=1,MAXS
              NSSVERTEX(1,J)=0.0D0
              NSSVERTEX(2,J)=0.0D0
              NSSVERTEX(3,J)=0.0D0
C       THE DIRECTION COSINE OF THE LOCAL X-AXIS IS:
              TX=0.0D0
              TY=0.0D0
              TZ=0.0D0
              LX=1.0D0
              MX=0.0D0
              NX=0.0D0
C       THE DIRECTION COSINE OF THE LOCAL Y-AXIS IS:
              LY=0.0D0
              MY=1.0D0
              NY=0.0D0
C       THE DIRECTION COSINE OF THE LOCAL Z-AXIS IS:
              LZ=0.0D0
              MZ=0.0D0
              NZ=1.0D0
              NSSVERTEX(0,J)=0.0D0
              IF(NSSALENS(100,J).EQ.1.0D0) THEN
C       WE HAVE A VALID SURFACE TO TRANSFORM
C       IS TRANSFORMATION REFERENCED TO ANOTHER SURFACE
C       INSTEAD OF TO THE GLOBAL ORIGIN
                  NSSNEST(0:200)=-1
                  INEST=0
                  IF(NSSALENS(37,J).NE.-1.0D0) THEN
                      NSSNEST(1)=INT(NSSALENS(37,J))
                      INEST=1
                      DO I=1,199
                          IF(NSSALENS(37,NSSNEST(I)).NE.-1.0D0) THEN
                              NSSNEST(I+1)=INT(NSSALENS(37,NSSNEST(I)))
                              INEST=I+1
                          ELSE
                              EXIT
                          END IF
                      END DO
                  END IF
                  IF(NSSALENS(37,J).NE.-1.0D0) THEN
C       NOW PERFORM THE REFERENCED
C       WE HAVE A VALID SURFACE TO TRANSFORM
                      NSSVERTEX(0,J)=1.0D0
                      XEEX=NSSALENS(34,INT(NSSALENS(37,J)))
                      YEEY=NSSALENS(35,INT(NSSALENS(37,J)))
                      ZEEZ=NSSALENS(36,INT(NSSALENS(37,J)))
C
C NEG SIGNS ARE USED BECAUSE THE TRANSFORMS WERE ORIGINALLY DESIGNED
C       FOR CODE-V ANGLE SIGN CONVENTIONS FOR ALPHA AND BETA WHICH ARE
C       LEFT HAND AND NOT RIGHT HAND POSITIVE
C
                      AEEA=-NSSALENS(40,INT(NSSALENS(37,J)))
                      BEEB=-NSSALENS(41,INT(NSSALENS(37,J)))
                      CEEC=NSSALENS(42,INT(NSSALENS(37,J)))
                      TX=XEEX
                      TY=YEEY
                      TZ=ZEEZ
C       THE DIRECTION COSINE OF THE LOCAL X-AXIS IS:
                      TLX=1.0D0
                      TMX=0.0D0
                      TNX=0.0D0
C       THE DIRECTION COSINE OF THE LOCAL Y-AXIS IS:
                      TLY=0.0D0
                      TMY=1.0D0
                      TNY=0.0D0
C       THE DIRECTION COSINE OF THE LOCAL Z-AXIS IS:
                      TLZ=0.0D0
                      TMZ=0.0D0
                      TNZ=1.0D0
                      A=-CEEC*(PII/180.0D0)
                      JLX1=(TLX*C11(A))+(TMX*C12(A))+(TNX*C13(A))
                      JMX1=(TLX*C21(A))+(TMX*C22(A))+(TNX*C23(A))
                      JNX1=(TLX*C31(A))+(TMX*C32(A))+(TNX*C33(A))
                      JLY1=(TLY*C11(A))+(TMY*C12(A))+(TNY*C13(A))
                      JMY1=(TLY*C21(A))+(TMY*C22(A))+(TNY*C23(A))
                      JNY1=(TLY*C31(A))+(TMY*C32(A))+(TNY*C33(A))
                      JLZ1=(TLZ*C11(A))+(TMZ*C12(A))+(TNZ*C13(A))
                      JMZ1=(TLZ*C21(A))+(TMZ*C22(A))+(TNZ*C23(A))
                      JNZ1=(TLZ*C31(A))+(TMZ*C32(A))+(TNZ*C33(A))
                      TLX=JLX1
                      TMX=JMX1
                      TNX=JNX1
                      TLY=JLY1
                      TMY=JMY1
                      TNY=JNY1
                      TLZ=JLZ1
                      TMZ=JMZ1
                      TNZ=JNZ1
                      A=-BEEB*(PII/180.0D0)
                      JLX1=(TLX*B11(A))+(TMX*B12(A))+(TNX*B13(A))
                      JMX1=(TLX*B21(A))+(TMX*B22(A))+(TNX*B23(A))
                      JNX1=(TLX*B31(A))+(TMX*B32(A))+(TNX*B33(A))
                      JLY1=(TLY*B11(A))+(TMY*B12(A))+(TNY*B13(A))
                      JMY1=(TLY*B21(A))+(TMY*B22(A))+(TNY*B23(A))
                      JNY1=(TLY*B31(A))+(TMY*B32(A))+(TNY*B33(A))
                      JLZ1=(TLZ*B11(A))+(TMZ*B12(A))+(TNZ*B13(A))
                      JMZ1=(TLZ*B21(A))+(TMZ*B22(A))+(TNZ*B23(A))
                      JNZ1=(TLZ*B31(A))+(TMZ*B32(A))+(TNZ*B33(A))
                      TLX=JLX1
                      TMX=JMX1
                      TNX=JNX1
                      TLY=JLY1
                      TMY=JMY1
                      TNY=JNY1
                      TLZ=JLZ1
                      TMZ=JMZ1
                      TNZ=JNZ1
                      A=-AEEA*(PII/180.0D0)
                      JLX1=(TLX*A11(A))+(TMX*A12(A))+(TNX*A13(A))
                      JMX1=(TLX*A21(A))+(TMX*A22(A))+(TNX*A23(A))
                      JNX1=(TLX*A31(A))+(TMX*A32(A))+(TNX*A33(A))
                      JLY1=(TLY*A11(A))+(TMY*A12(A))+(TNY*A13(A))
                      JMY1=(TLY*A21(A))+(TMY*A22(A))+(TNY*A23(A))
                      JNY1=(TLY*A31(A))+(TMY*A32(A))+(TNY*A33(A))
                      JLZ1=(TLZ*A11(A))+(TMZ*A12(A))+(TNZ*A13(A))
                      JMZ1=(TLZ*A21(A))+(TMZ*A22(A))+(TNZ*A23(A))
                      JNZ1=(TLZ*A31(A))+(TMZ*A32(A))+(TNZ*A33(A))
                      TLX=JLX1
                      TMX=JMX1
                      TNX=JNX1
                      TLY=JLY1
                      TMY=JMY1
                      TNY=JNY1
                      TLZ=JLZ1
                      TMZ=JMZ1
                      TNZ=JNZ1
C     NOW THE DIR COSINES ARE IN THE LOCAL COORDINATES SYSTEM OF
C     SURFACE I. TRANSFORM THEM TO THE GLOBAL COORDINATE SYSTEM
                      TTLX=LX
                      TTMX=MX
                      TTNX=NX
                      TTLY=LY
                      TTMY=MY
                      TTNY=NY
                      TTLZ=LZ
                      TTMZ=MZ
                      TTNZ=NZ
                      LX=(TLX*TTLX)+(TMX*TTLY)+(TNX*TTLZ)
                      MX=(TLX*TTMX)+(TMX*TTMY)+(TNX*TTMZ)
                      NX=(TLX*TTNX)+(TMX*TTNY)+(TNX*TTNZ)
                      LY=(TLY*TTLX)+(TMY*TTLY)+(TNY*TTLZ)
                      MY=(TLY*TTMX)+(TMY*TTMY)+(TNY*TTMZ)
                      NY=(TLY*TTNX)+(TMY*TTNY)+(TNY*TTNZ)
                      LZ=(TLZ*TTLX)+(TMZ*TTLY)+(TNZ*TTLZ)
                      MZ=(TLZ*TTMX)+(TMZ*TTMY)+(TNZ*TTMZ)
                      NZ=(TLZ*TTNX)+(TMZ*TTNY)+(TNZ*TTNZ)
                  END IF
C       NOW PERFORM THE TRANSFORMATION ATTACHED TO THIS SURFACE
C       WE HAVE A VALID SURFACE TO TRANSFORM
                  NSSVERTEX(0,J)=1.0D0
                  XEEX=NSSALENS(34,J)
                  YEEY=NSSALENS(35,J)
                  ZEEZ=NSSALENS(36,J)
C
C NEG SIGNS ARE USED BECAUSE THE TRANSFORMS WERE ORIGINALLY DESIGNED
C       FOR CODE-V ANGLE SIGN CONVENTIONS FOR ALPHA AND BETA WHICH ARE
C       LEFT HAND AND NOT RIGHT HAND POSITIVE
C
                  AEEA=-NSSALENS(40,J)
                  BEEB=-NSSALENS(41,J)
                  CEEC=NSSALENS(42,J)

                  TX=TX+(ZEEZ*LZ)+(XEEX*LX)+(YEEY*LY)
                  TY=TY+(ZEEZ*MZ)+(XEEX*MX)+(YEEY*MY)
                  TZ=TZ+(ZEEZ*NZ)+(XEEX*NX)+(YEEY*NY)
C
                  NSSVERTEX(1,J)=TX
                  NSSVERTEX(2,J)=TY
                  NSSVERTEX(3,J)=TZ
C       THE DIRECTION COSINE OF THE LOCAL X-AXIS IS:
                  TLX=LX
                  TMX=MX
                  TNX=NX
C       THE DIRECTION COSINE OF THE LOCAL Y-AXIS IS:
                  TLY=LY
                  TMY=MY
                  TNY=NY
C       THE DIRECTION COSINE OF THE LOCAL Z-AXIS IS:
                  TLZ=LZ
                  TMZ=MZ
                  TNZ=NZ
                  A=-CEEC*(PII/180.0D0)
                  JLX1=(TLX*C11(A))+(TMX*C12(A))+(TNX*C13(A))
                  JMX1=(TLX*C21(A))+(TMX*C22(A))+(TNX*C23(A))
                  JNX1=(TLX*C31(A))+(TMX*C32(A))+(TNX*C33(A))
                  JLY1=(TLY*C11(A))+(TMY*C12(A))+(TNY*C13(A))
                  JMY1=(TLY*C21(A))+(TMY*C22(A))+(TNY*C23(A))
                  JNY1=(TLY*C31(A))+(TMY*C32(A))+(TNY*C33(A))
                  JLZ1=(TLZ*C11(A))+(TMZ*C12(A))+(TNZ*C13(A))
                  JMZ1=(TLZ*C21(A))+(TMZ*C22(A))+(TNZ*C23(A))
                  JNZ1=(TLZ*C31(A))+(TMZ*C32(A))+(TNZ*C33(A))
                  TLX=JLX1
                  TMX=JMX1
                  TNX=JNX1
                  TLY=JLY1
                  TMY=JMY1
                  TNY=JNY1
                  TLZ=JLZ1
                  TMZ=JMZ1
                  TNZ=JNZ1
                  A=-BEEB*(PII/180.0D0)
                  JLX1=(TLX*B11(A))+(TMX*B12(A))+(TNX*B13(A))
                  JMX1=(TLX*B21(A))+(TMX*B22(A))+(TNX*B23(A))
                  JNX1=(TLX*B31(A))+(TMX*B32(A))+(TNX*B33(A))
                  JLY1=(TLY*B11(A))+(TMY*B12(A))+(TNY*B13(A))
                  JMY1=(TLY*B21(A))+(TMY*B22(A))+(TNY*B23(A))
                  JNY1=(TLY*B31(A))+(TMY*B32(A))+(TNY*B33(A))
                  JLZ1=(TLZ*B11(A))+(TMZ*B12(A))+(TNZ*B13(A))
                  JMZ1=(TLZ*B21(A))+(TMZ*B22(A))+(TNZ*B23(A))
                  JNZ1=(TLZ*B31(A))+(TMZ*B32(A))+(TNZ*B33(A))
                  TLX=JLX1
                  TMX=JMX1
                  TNX=JNX1
                  TLY=JLY1
                  TMY=JMY1
                  TNY=JNY1
                  TLZ=JLZ1
                  TMZ=JMZ1
                  TNZ=JNZ1
                  A=-AEEA*(PII/180.0D0)
                  JLX1=(TLX*A11(A))+(TMX*A12(A))+(TNX*A13(A))
                  JMX1=(TLX*A21(A))+(TMX*A22(A))+(TNX*A23(A))
                  JNX1=(TLX*A31(A))+(TMX*A32(A))+(TNX*A33(A))
                  JLY1=(TLY*A11(A))+(TMY*A12(A))+(TNY*A13(A))
                  JMY1=(TLY*A21(A))+(TMY*A22(A))+(TNY*A23(A))
                  JNY1=(TLY*A31(A))+(TMY*A32(A))+(TNY*A33(A))
                  JLZ1=(TLZ*A11(A))+(TMZ*A12(A))+(TNZ*A13(A))
                  JMZ1=(TLZ*A21(A))+(TMZ*A22(A))+(TNZ*A23(A))
                  JNZ1=(TLZ*A31(A))+(TMZ*A32(A))+(TNZ*A33(A))
                  TLX=JLX1
                  TMX=JMX1
                  TNX=JNX1
                  TLY=JLY1
                  TMY=JMY1
                  TNY=JNY1
                  TLZ=JLZ1
                  TMZ=JMZ1
                  TNZ=JNZ1
                  NSSVERTEX(4,J)=TLX
                  NSSVERTEX(5,J)=TMX
                  NSSVERTEX(6,J)=TNX
                  NSSVERTEX(7,J)=TLY
                  NSSVERTEX(8,J)=TMY
                  NSSVERTEX(9,J)=TNY
                  NSSVERTEX(10,J)=TLZ
                  NSSVERTEX(11,J)=TMZ
                  NSSVERTEX(12,J)=TNZ
              ELSE
C       NOT A USED SURFACE NUMBER IN THIS OBJECT
              END IF
          END DO
          RETURN
      END


      SUBROUTINE NSS_TRANSF(J)
          USE NSSMOD
C
C       THIS ROUTINE CONVERTS GLOBAL RAY DATA INTO THE COORDINATE SYSTEM OF
C       SURFACE J
C
C       THIS OPERATES ON RAY DATA BEFORE A SURFACE INTERACTION
C
C
          IMPLICIT NONE
C
          REAL*8 JK_AA,A11,A12,A13,A21,A22,A23,A31,A32,A33
     1    ,B11,B12,B13,B21,B22,B23,B31,B32,B33
     2    ,C11,C12,C13,C21,C22,C23,C31,C32,C33,
     3    A,B,C,DX,DY,DZ,AN,X1,Y1,Z1,L1,M1,N1
          REAL*8 X,Y,Z,L,M,N,XL,XM,XN,YL,YM,YN
          REAL*8 XP,YP,ZP,LP,MP,NP,XLP,XMP,XNP,YLP,YMP,YNP
C
          INTEGER J,I
C
          INCLUDE 'datmai.inc'
C
C       ROTATION MATRICIES FUNCTIONS
C
C       A11
          A11(AN)=1.0D0
C       A12
          A12(AN)=0.0D0
C       A13
          A13(AN)=0.0D0
C       A21
          A21(AN)=0.0D0
C       A22
          A22(AN)=DCOS(AN)
C       A23
          A23(AN)=-DSIN(AN)
C       A31
          A31(AN)=0.0D0
C       A32
          A32(AN)=DSIN(AN)
C       A33
          A33(AN)=DCOS(AN)
C       B11
          B11(AN)=DCOS(AN)
C       B12
          B12(AN)=0.0D0
C       B13
          B13(AN)=DSIN(AN)
C       B21
          B21(AN)=0.0D0
C       B22
          B22(AN)=1.0D0
C       B23
          B23(AN)=0.0D0
C       B31
          B31(AN)=-DSIN(AN)
C       B32
          B32(AN)=0.0D0
C       B33
          B33(AN)=DCOS(AN)
C       C11
          C11(AN)=DCOS(AN)
C       C12
          C12(AN)=DSIN(AN)
C       C13
          C13(AN)=0.0D0
C       C21
          C21(AN)=-DSIN(AN)
C       C22
          C22(AN)=DCOS(AN)
C       C23
          C23(AN)=0.0D0
C       C31
          C31(AN)=0.0D0
C       C32
          C32(AN)=0.0D0
C       C33
          C33(AN)=1.0D0
C
          X=NSS_GRAY(1)
          Y=NSS_GRAY(2)
          Z=NSS_GRAY(3)
          L=NSS_GRAY(4)
          M=NSS_GRAY(5)
          N=NSS_GRAY(6)
          XL=NSS_GRAY(7)
          XM=NSS_GRAY(8)
          XN=NSS_GRAY(9)
          YL=NSS_GRAY(10)
          YM=NSS_GRAY(11)
          YN=NSS_GRAY(12)
C
C NEG SIGNS ARE USED BECAUSE THE TRANSFORMS WERE ORIGINALLY DESIGNED
C       FOR CODE-V ANGLE SIGN CONVENTIONS FOR ALPHA AND BETA WHICH ARE
C       LEFT HAND AND NOT RIGHT HAND POSITIVE
C       IS TRANSFORMATION REFERENCED TO ANOTHER SURFACE
C       INSTEAD OF TO THE GLOBAL ORIGIN
          NSSNEST(0:200)=-1
          INEST=0
          IF(NSSALENS(37,J).NE.-1.0D0) THEN
              NSSNEST(1)=INT(NSSALENS(37,J))
              INEST=1
              DO I=1,199
                  IF(NSSALENS(37,NSSNEST(I)).NE.-1.0D0) THEN
                      NSSNEST(I+1)=INT(NSSALENS(37,NSSNEST(I)))
                      INEST=I+1
                  ELSE
                      EXIT
                  END IF
              END DO
          END IF
          IF(NSSALENS(37,J).NE.-1.0D0) THEN
C       NOW PERFORM THE REFERENCED
C       WE HAVE A VALID SURFACE TO TRANSFORM
              A=-NSSALENS(40,INT(NSSALENS(37,J)))
              B=-NSSALENS(41,INT(NSSALENS(37,J)))
              C= NSSALENS(42,INT(NSSALENS(37,J)))
              DX=NSSALENS(34,INT(NSSALENS(37,J)))
              DY=NSSALENS(35,INT(NSSALENS(37,J)))
              DZ=NSSALENS(36,INT(NSSALENS(37,J)))
C
C     RESOLVE DECENTER AND TILT
C
              XP=X-DX
              YP=Y-DY
              ZP=Z-DZ
              LP=L
              MP=M
              NP=N
              XLP=XL
              XMP=XM
              XNP=XN
              YLP=YL
              YMP=YM
              YNP=YN
C
C       NOW ROTATE THE SUCKER!
              IF(A.NE.0.0D0) THEN
                  JK_AA=A*(PII/180.0D0)
                  X1=(XP*A11(JK_AA))+(YP*A12(JK_AA))+(ZP*A13(JK_AA))
                  Y1=(XP*A21(JK_AA))+(YP*A22(JK_AA))+(ZP*A23(JK_AA))
                  Z1=(XP*A31(JK_AA))+(YP*A32(JK_AA))+(ZP*A33(JK_AA))
                  XP=X1
                  YP=Y1
                  ZP=Z1
                  L1=(LP*A11(JK_AA))+(MP*A12(JK_AA))+(NP*A13(JK_AA))
                  M1=(LP*A21(JK_AA))+(MP*A22(JK_AA))+(NP*A23(JK_AA))
                  N1=(LP*A31(JK_AA))+(MP*A32(JK_AA))+(NP*A33(JK_AA))
                  LP=L1
                  MP=M1
                  NP=N1
                  L1=(XLP*A11(JK_AA))+(XMP*A12(JK_AA))+(XNP*A13(JK_AA))
                  M1=(XLP*A21(JK_AA))+(XMP*A22(JK_AA))+(XNP*A23(JK_AA))
                  N1=(XLP*A31(JK_AA))+(XMP*A32(JK_AA))+(XNP*A33(JK_AA))
                  XLP=L1
                  XMP=M1
                  XNP=N1
                  L1=(YLP*A11(JK_AA))+(YMP*A12(JK_AA))+(YNP*A13(JK_AA))
                  M1=(YLP*A21(JK_AA))+(YMP*A22(JK_AA))+(YNP*A23(JK_AA))
                  N1=(YLP*A31(JK_AA))+(YMP*A32(JK_AA))+(YNP*A33(JK_AA))
                  YLP=L1
                  YMP=M1
                  YNP=N1
              END IF
              IF(B.NE.0.0D0) THEN
                  JK_AA=B*(PII/180.0D0)
                  X1=(XP*B11(JK_AA))+(YP*B12(JK_AA))+(ZP*B13(JK_AA))
                  Y1=(XP*B21(JK_AA))+(YP*B22(JK_AA))+(ZP*B23(JK_AA))
                  Z1=(XP*B31(JK_AA))+(YP*B32(JK_AA))+(ZP*B33(JK_AA))
                  XP=X1
                  YP=Y1
                  ZP=Z1
                  L1=(LP*B11(JK_AA))+(MP*B12(JK_AA))+(NP*B13(JK_AA))
                  M1=(LP*B21(JK_AA))+(MP*B22(JK_AA))+(NP*B23(JK_AA))
                  N1=(LP*B31(JK_AA))+(MP*B32(JK_AA))+(NP*B33(JK_AA))
                  LP=L1
                  MP=M1
                  NP=N1
                  L1=(XLP*B11(JK_AA))+(XMP*B12(JK_AA))+(XNP*B13(JK_AA))
                  M1=(XLP*B21(JK_AA))+(XMP*B22(JK_AA))+(XNP*B23(JK_AA))
                  N1=(XLP*B31(JK_AA))+(XMP*B32(JK_AA))+(XNP*B33(JK_AA))
                  XLP=L1
                  XMP=M1
                  XNP=N1
                  L1=(YLP*B11(JK_AA))+(YMP*B12(JK_AA))+(YNP*B13(JK_AA))
                  M1=(YLP*B21(JK_AA))+(YMP*B22(JK_AA))+(YNP*B23(JK_AA))
                  N1=(YLP*B31(JK_AA))+(YMP*B32(JK_AA))+(YNP*B33(JK_AA))
                  YLP=L1
                  YMP=M1
                  YNP=N1
              END IF
              IF(C.NE.0.0D0) THEN
                  JK_AA=C*(PII/180.0D0)
                  X1=(XP*C11(JK_AA))+(YP*C12(JK_AA))+(ZP*C13(JK_AA))
                  Y1=(XP*C21(JK_AA))+(YP*C22(JK_AA))+(ZP*C23(JK_AA))
                  Z1=(XP*C31(JK_AA))+(YP*C32(JK_AA))+(ZP*C33(JK_AA))
                  XP=X1
                  YP=Y1
                  ZP=Z1
                  L1=(LP*C11(JK_AA))+(MP*C12(JK_AA))+(NP*C13(JK_AA))
                  M1=(LP*C21(JK_AA))+(MP*C22(JK_AA))+(NP*C23(JK_AA))
                  N1=(LP*C31(JK_AA))+(MP*C32(JK_AA))+(NP*C33(JK_AA))
                  LP=L1
                  MP=M1
                  NP=N1
                  L1=(XLP*C11(JK_AA))+(XMP*C12(JK_AA))+(XNP*C13(JK_AA))
                  M1=(XLP*C21(JK_AA))+(XMP*C22(JK_AA))+(XNP*C23(JK_AA))
                  N1=(XLP*C31(JK_AA))+(XMP*C32(JK_AA))+(XNP*C33(JK_AA))
                  XLP=L1
                  XMP=M1
                  XNP=N1
                  L1=(YLP*C11(JK_AA))+(YMP*C12(JK_AA))+(YNP*C13(JK_AA))
                  M1=(YLP*C21(JK_AA))+(YMP*C22(JK_AA))+(YNP*C23(JK_AA))
                  N1=(YLP*C31(JK_AA))+(YMP*C32(JK_AA))+(YNP*C33(JK_AA))
                  YLP=L1
                  YMP=M1
                  YNP=N1
              END IF
              X=XP
              Y=YP
              Z=ZP
              L=LP
              M=MP
              N=NP
              XL=XLP
              XM=XMP
              XN=XNP
              YL=YLP
              YM=YMP
              YN=YNP
          END IF
C       NOW PERFORM THE TRANSFORMATION ATTACHED TO THIS SURFACE
C       WE HAVE A VALID SURFACE TO TRANSFORM
C
          A=-NSSALENS(40,J)
          B=-NSSALENS(41,J)
          C= NSSALENS(42,J)
          DX=NSSALENS(34,J)
          DY=NSSALENS(35,J)
          DZ=NSSALENS(36,J)
C
C     RESOLVE DECENTER AND TILT
C
          XP=X-DX
          YP=Y-DY
          ZP=Z-DZ
          LP=L
          MP=M
          NP=N
          XLP=XL
          XMP=XM
          XNP=XN
          YLP=YL
          YMP=YM
          YNP=YN
C
C       NOW ROTATE THE SUCKER!
          IF(A.NE.0.0D0) THEN
              JK_AA=A*(PII/180.0D0)
              X1=(XP*A11(JK_AA))+(YP*A12(JK_AA))+(ZP*A13(JK_AA))
              Y1=(XP*A21(JK_AA))+(YP*A22(JK_AA))+(ZP*A23(JK_AA))
              Z1=(XP*A31(JK_AA))+(YP*A32(JK_AA))+(ZP*A33(JK_AA))
              XP=X1
              YP=Y1
              ZP=Z1
              L1=(LP*A11(JK_AA))+(MP*A12(JK_AA))+(NP*A13(JK_AA))
              M1=(LP*A21(JK_AA))+(MP*A22(JK_AA))+(NP*A23(JK_AA))
              N1=(LP*A31(JK_AA))+(MP*A32(JK_AA))+(NP*A33(JK_AA))
              LP=L1
              MP=M1
              NP=N1
              L1=(XLP*A11(JK_AA))+(XMP*A12(JK_AA))+(XNP*A13(JK_AA))
              M1=(XLP*A21(JK_AA))+(XMP*A22(JK_AA))+(XNP*A23(JK_AA))
              N1=(XLP*A31(JK_AA))+(XMP*A32(JK_AA))+(XNP*A33(JK_AA))
              XLP=L1
              XMP=M1
              XNP=N1
              L1=(YLP*A11(JK_AA))+(YMP*A12(JK_AA))+(YNP*A13(JK_AA))
              M1=(YLP*A21(JK_AA))+(YMP*A22(JK_AA))+(YNP*A23(JK_AA))
              N1=(YLP*A31(JK_AA))+(YMP*A32(JK_AA))+(YNP*A33(JK_AA))
              YLP=L1
              YMP=M1
              YNP=N1
          END IF
          IF(B.NE.0.0D0) THEN
              JK_AA=B*(PII/180.0D0)
              X1=(XP*B11(JK_AA))+(YP*B12(JK_AA))+(ZP*B13(JK_AA))
              Y1=(XP*B21(JK_AA))+(YP*B22(JK_AA))+(ZP*B23(JK_AA))
              Z1=(XP*B31(JK_AA))+(YP*B32(JK_AA))+(ZP*B33(JK_AA))
              XP=X1
              YP=Y1
              ZP=Z1
              L1=(LP*B11(JK_AA))+(MP*B12(JK_AA))+(NP*B13(JK_AA))
              M1=(LP*B21(JK_AA))+(MP*B22(JK_AA))+(NP*B23(JK_AA))
              N1=(LP*B31(JK_AA))+(MP*B32(JK_AA))+(NP*B33(JK_AA))
              LP=L1
              MP=M1
              NP=N1
              L1=(XLP*B11(JK_AA))+(XMP*B12(JK_AA))+(XNP*B13(JK_AA))
              M1=(XLP*B21(JK_AA))+(XMP*B22(JK_AA))+(XNP*B23(JK_AA))
              N1=(XLP*B31(JK_AA))+(XMP*B32(JK_AA))+(XNP*B33(JK_AA))
              XLP=L1
              XMP=M1
              XNP=N1
              L1=(YLP*B11(JK_AA))+(YMP*B12(JK_AA))+(YNP*B13(JK_AA))
              M1=(YLP*B21(JK_AA))+(YMP*B22(JK_AA))+(YNP*B23(JK_AA))
              N1=(YLP*B31(JK_AA))+(YMP*B32(JK_AA))+(YNP*B33(JK_AA))
              YLP=L1
              YMP=M1
              YNP=N1
          END IF
          IF(C.NE.0.0D0) THEN
              JK_AA=C*(PII/180.0D0)
              X1=(XP*C11(JK_AA))+(YP*C12(JK_AA))+(ZP*C13(JK_AA))
              Y1=(XP*C21(JK_AA))+(YP*C22(JK_AA))+(ZP*C23(JK_AA))
              Z1=(XP*C31(JK_AA))+(YP*C32(JK_AA))+(ZP*C33(JK_AA))
              XP=X1
              YP=Y1
              ZP=Z1
              L1=(LP*C11(JK_AA))+(MP*C12(JK_AA))+(NP*C13(JK_AA))
              M1=(LP*C21(JK_AA))+(MP*C22(JK_AA))+(NP*C23(JK_AA))
              N1=(LP*C31(JK_AA))+(MP*C32(JK_AA))+(NP*C33(JK_AA))
              LP=L1
              MP=M1
              NP=N1
              L1=(XLP*C11(JK_AA))+(XMP*C12(JK_AA))+(XNP*C13(JK_AA))
              M1=(XLP*C21(JK_AA))+(XMP*C22(JK_AA))+(XNP*C23(JK_AA))
              N1=(XLP*C31(JK_AA))+(XMP*C32(JK_AA))+(XNP*C33(JK_AA))
              XLP=L1
              XMP=M1
              XNP=N1
              L1=(YLP*C11(JK_AA))+(YMP*C12(JK_AA))+(YNP*C13(JK_AA))
              M1=(YLP*C21(JK_AA))+(YMP*C22(JK_AA))+(YNP*C23(JK_AA))
              N1=(YLP*C31(JK_AA))+(YMP*C32(JK_AA))+(YNP*C33(JK_AA))
              YLP=L1
              YMP=M1
              YNP=N1
          END IF
          NSS_LRAY(1)=XP
          NSS_LRAY(2)=YP
          NSS_LRAY(3)=ZP
          NSS_LRAY(4)=LP
          NSS_LRAY(5)=MP
          NSS_LRAY(6)=NP
          NSS_LRAY(7)=XLP
          NSS_LRAY(8)=XMP
          NSS_LRAY(9)=XNP
          NSS_LRAY(10)=YLP
          NSS_LRAY(11)=YMP
          NSS_LRAY(12)=YNP
          RETURN
      END
      SUBROUTINE NSS_TRANSFP(J)
          USE NSSMOD
C
C       THIS ROUTINE CONVERTS GLOBAL RAY DATA INTO THE COORDINATE SYSTEM OF
C       SURFACE J
C
C       THIS OPERATES ON RAY DATA AFTER A SURFACE INTERACTION
C
C
          IMPLICIT NONE
C
          REAL*8 JK_AA,A11,A12,A13,A21,A22,A23,A31,A32,A33
     1    ,B11,B12,B13,B21,B22,B23,B31,B32,B33
     2    ,C11,C12,C13,C21,C22,C23,C31,C32,C33,
     3    A,B,C,DX,DY,DZ,AN,X1,Y1,Z1,L1,M1,N1
          REAL*8 X,Y,Z,L,M,N,XL,XM,XN,YL,YM,YN
          REAL*8 XP,YP,ZP,LP,MP,NP,XLP,XMP,XNP,YLP,YMP,YNP
C
          INTEGER J,I
C
          INCLUDE 'datmai.inc'
C
C       ROTATION MATRICIES FUNCTIONS
C
C       A11
          A11(AN)=1.0D0
C       A12
          A12(AN)=0.0D0
C       A13
          A13(AN)=0.0D0
C       A21
          A21(AN)=0.0D0
C       A22
          A22(AN)=DCOS(AN)
C       A23
          A23(AN)=-DSIN(AN)
C       A31
          A31(AN)=0.0D0
C       A32
          A32(AN)=DSIN(AN)
C       A33
          A33(AN)=DCOS(AN)
C       B11
          B11(AN)=DCOS(AN)
C       B12
          B12(AN)=0.0D0
C       B13
          B13(AN)=DSIN(AN)
C       B21
          B21(AN)=0.0D0
C       B22
          B22(AN)=1.0D0
C       B23
          B23(AN)=0.0D0
C       B31
          B31(AN)=-DSIN(AN)
C       B32
          B32(AN)=0.0D0
C       B33
          B33(AN)=DCOS(AN)
C       C11
          C11(AN)=DCOS(AN)
C       C12
          C12(AN)=DSIN(AN)
C       C13
          C13(AN)=0.0D0
C       C21
          C21(AN)=-DSIN(AN)
C       C22
          C22(AN)=DCOS(AN)
C       C23
          C23(AN)=0.0D0
C       C31
          C31(AN)=0.0D0
C       C32
          C32(AN)=0.0D0
C       C33
          C33(AN)=1.0D0
C
C NEG SIGNS ARE USED BECAUSE THE TRANSFORMS WERE ORIGINALLY DESIGNED
C       FOR CODE-V ANGLE SIGN CONVENTIONS FOR ALPHA AND BETA WHICH ARE
C       LEFT HAND AND NOT RIGHT HAND POSITIVE
C       IS TRANSFORMATION REFERENCED TO ANOTHER SURFACE
C       INSTEAD OF TO THE GLOBAL ORIGIN
          X=NSS_GRAYP(1)
          Y=NSS_GRAYP(2)
          Z=NSS_GRAYP(3)
          L=NSS_GRAYP(4)
          M=NSS_GRAYP(5)
          N=NSS_GRAYP(6)
          XL=NSS_GRAYP(7)
          XM=NSS_GRAYP(8)
          XN=NSS_GRAYP(9)
          YL=NSS_GRAYP(10)
          YM=NSS_GRAYP(11)
          YN=NSS_GRAYP(12)
          NSSNEST(0:200)=-1
          INEST=0
          IF(NSSALENS(37,J).NE.-1.0D0) THEN
              NSSNEST(1)=INT(NSSALENS(37,J))
              INEST=1
              DO I=1,199
                  IF(NSSALENS(37,NSSNEST(I)).NE.-1.0D0) THEN
                      NSSNEST(I+1)=INT(NSSALENS(37,NSSNEST(I)))
                      INEST=I+1
                  ELSE
                      EXIT
                  END IF
              END DO
          END IF
          IF(NSSALENS(37,J).NE.-1.0D0) THEN
C       NOW PERFORM THE REFERENCED
C       WE HAVE A VALID SURFACE TO TRANSFORM
              A=-NSSALENS(40,INT(NSSALENS(37,J)))
              B=-NSSALENS(41,INT(NSSALENS(37,J)))
              C= NSSALENS(42,INT(NSSALENS(37,J)))
              DX=NSSALENS(34,INT(NSSALENS(37,J)))
              DY=NSSALENS(35,INT(NSSALENS(37,J)))
              DZ=NSSALENS(36,INT(NSSALENS(37,J)))
C
C     RESOLVE DECENTER AND TILT
C
              XP=X-DX
              YP=Y-DY
              ZP=Z-DZ
              LP=L
              MP=M
              NP=N
              XLP=XL
              XMP=XM
              XNP=XN
              YLP=YL
              YMP=YM
              YNP=YN
C
C       NOW ROTATE THE SUCKER!
              IF(A.NE.0.0D0) THEN
                  JK_AA=A*(PII/180.0D0)
                  X1=(XP*A11(JK_AA))+(YP*A12(JK_AA))+(ZP*A13(JK_AA))
                  Y1=(XP*A21(JK_AA))+(YP*A22(JK_AA))+(ZP*A23(JK_AA))
                  Z1=(XP*A31(JK_AA))+(YP*A32(JK_AA))+(ZP*A33(JK_AA))
                  XP=X1
                  YP=Y1
                  ZP=Z1
                  L1=(LP*A11(JK_AA))+(MP*A12(JK_AA))+(NP*A13(JK_AA))
                  M1=(LP*A21(JK_AA))+(MP*A22(JK_AA))+(NP*A23(JK_AA))
                  N1=(LP*A31(JK_AA))+(MP*A32(JK_AA))+(NP*A33(JK_AA))
                  LP=L1
                  MP=M1
                  NP=N1
                  L1=(XLP*A11(JK_AA))+(XMP*A12(JK_AA))+(XNP*A13(JK_AA))
                  M1=(XLP*A21(JK_AA))+(XMP*A22(JK_AA))+(XNP*A23(JK_AA))
                  N1=(XLP*A31(JK_AA))+(XMP*A32(JK_AA))+(XNP*A33(JK_AA))
                  XLP=L1
                  XMP=M1
                  XNP=N1
                  L1=(YLP*A11(JK_AA))+(YMP*A12(JK_AA))+(YNP*A13(JK_AA))
                  M1=(YLP*A21(JK_AA))+(YMP*A22(JK_AA))+(YNP*A23(JK_AA))
                  N1=(YLP*A31(JK_AA))+(YMP*A32(JK_AA))+(YNP*A33(JK_AA))
                  YLP=L1
                  YMP=M1
                  YNP=N1
              END IF
              IF(B.NE.0.0D0) THEN
                  JK_AA=B*(PII/180.0D0)
                  X1=(XP*B11(JK_AA))+(YP*B12(JK_AA))+(ZP*B13(JK_AA))
                  Y1=(XP*B21(JK_AA))+(YP*B22(JK_AA))+(ZP*B23(JK_AA))
                  Z1=(XP*B31(JK_AA))+(YP*B32(JK_AA))+(ZP*B33(JK_AA))
                  XP=X1
                  YP=Y1
                  ZP=Z1
                  L1=(LP*B11(JK_AA))+(MP*B12(JK_AA))+(NP*B13(JK_AA))
                  M1=(LP*B21(JK_AA))+(MP*B22(JK_AA))+(NP*B23(JK_AA))
                  N1=(LP*B31(JK_AA))+(MP*B32(JK_AA))+(NP*B33(JK_AA))
                  LP=L1
                  MP=M1
                  NP=N1
                  L1=(XLP*B11(JK_AA))+(XMP*B12(JK_AA))+(XNP*B13(JK_AA))
                  M1=(XLP*B21(JK_AA))+(XMP*B22(JK_AA))+(XNP*B23(JK_AA))
                  N1=(XLP*B31(JK_AA))+(XMP*B32(JK_AA))+(XNP*B33(JK_AA))
                  XLP=L1
                  XMP=M1
                  XNP=N1
                  L1=(YLP*B11(JK_AA))+(YMP*B12(JK_AA))+(YNP*B13(JK_AA))
                  M1=(YLP*B21(JK_AA))+(YMP*B22(JK_AA))+(YNP*B23(JK_AA))
                  N1=(YLP*B31(JK_AA))+(YMP*B32(JK_AA))+(YNP*B33(JK_AA))
                  YLP=L1
                  YMP=M1
                  YNP=N1
              END IF
              IF(C.NE.0.0D0) THEN
                  JK_AA=C*(PII/180.0D0)
                  X1=(XP*C11(JK_AA))+(YP*C12(JK_AA))+(ZP*C13(JK_AA))
                  Y1=(XP*C21(JK_AA))+(YP*C22(JK_AA))+(ZP*C23(JK_AA))
                  Z1=(XP*C31(JK_AA))+(YP*C32(JK_AA))+(ZP*C33(JK_AA))
                  XP=X1
                  YP=Y1
                  ZP=Z1
                  L1=(LP*C11(JK_AA))+(MP*C12(JK_AA))+(NP*C13(JK_AA))
                  M1=(LP*C21(JK_AA))+(MP*C22(JK_AA))+(NP*C23(JK_AA))
                  N1=(LP*C31(JK_AA))+(MP*C32(JK_AA))+(NP*C33(JK_AA))
                  LP=L1
                  MP=M1
                  NP=N1
                  L1=(XLP*C11(JK_AA))+(XMP*C12(JK_AA))+(XNP*C13(JK_AA))
                  M1=(XLP*C21(JK_AA))+(XMP*C22(JK_AA))+(XNP*C23(JK_AA))
                  N1=(XLP*C31(JK_AA))+(XMP*C32(JK_AA))+(XNP*C33(JK_AA))
                  XLP=L1
                  XMP=M1
                  XNP=N1
                  L1=(YLP*C11(JK_AA))+(YMP*C12(JK_AA))+(YNP*C13(JK_AA))
                  M1=(YLP*C21(JK_AA))+(YMP*C22(JK_AA))+(YNP*C23(JK_AA))
                  N1=(YLP*C31(JK_AA))+(YMP*C32(JK_AA))+(YNP*C33(JK_AA))
                  YLP=L1
                  YMP=M1
                  YNP=N1
              END IF
              X=XP
              Y=YP
              Z=ZP
              L=LP
              M=MP
              N=NP
              XL=XLP
              XM=XMP
              XN=XNP
              YL=YLP
              YM=YMP
              YN=YNP
          END IF
C       NOW PERFORM THE TRANSFORMATION ATTACHED TO THIS SURFACE
C       WE HAVE A VALID SURFACE TO TRANSFORM
C
          A=-NSSALENS(40,J)
          B=-NSSALENS(41,J)
          C= NSSALENS(42,J)
          DX=NSSALENS(34,J)
          DY=NSSALENS(35,J)
          DZ=NSSALENS(36,J)
C
C     RESOLVE DECENTER AND TILT
C
          XP=X-DX
          YP=Y-DY
          ZP=Z-DZ
          LP=L
          MP=M
          NP=N
          XLP=XL
          XMP=XM
          XNP=XN
          YLP=YL
          YMP=YM
          YNP=YN
C
C       NOW ROTATE THE SUCKER!
          IF(A.NE.0.0D0) THEN
              JK_AA=A*(PII/180.0D0)
              X1=(XP*A11(JK_AA))+(YP*A12(JK_AA))+(ZP*A13(JK_AA))
              Y1=(XP*A21(JK_AA))+(YP*A22(JK_AA))+(ZP*A23(JK_AA))
              Z1=(XP*A31(JK_AA))+(YP*A32(JK_AA))+(ZP*A33(JK_AA))
              XP=X1
              YP=Y1
              ZP=Z1
              L1=(LP*A11(JK_AA))+(MP*A12(JK_AA))+(NP*A13(JK_AA))
              M1=(LP*A21(JK_AA))+(MP*A22(JK_AA))+(NP*A23(JK_AA))
              N1=(LP*A31(JK_AA))+(MP*A32(JK_AA))+(NP*A33(JK_AA))
              LP=L1
              MP=M1
              NP=N1
              L1=(XLP*A11(JK_AA))+(XMP*A12(JK_AA))+(XNP*A13(JK_AA))
              M1=(XLP*A21(JK_AA))+(XMP*A22(JK_AA))+(XNP*A23(JK_AA))
              N1=(XLP*A31(JK_AA))+(XMP*A32(JK_AA))+(XNP*A33(JK_AA))
              XLP=L1
              XMP=M1
              XNP=N1
              L1=(YLP*A11(JK_AA))+(YMP*A12(JK_AA))+(YNP*A13(JK_AA))
              M1=(YLP*A21(JK_AA))+(YMP*A22(JK_AA))+(YNP*A23(JK_AA))
              N1=(YLP*A31(JK_AA))+(YMP*A32(JK_AA))+(YNP*A33(JK_AA))
              YLP=L1
              YMP=M1
              YNP=N1
          END IF
          IF(B.NE.0.0D0) THEN
              JK_AA=B*(PII/180.0D0)
              X1=(XP*B11(JK_AA))+(YP*B12(JK_AA))+(ZP*B13(JK_AA))
              Y1=(XP*B21(JK_AA))+(YP*B22(JK_AA))+(ZP*B23(JK_AA))
              Z1=(XP*B31(JK_AA))+(YP*B32(JK_AA))+(ZP*B33(JK_AA))
              XP=X1
              YP=Y1
              ZP=Z1
              L1=(LP*B11(JK_AA))+(MP*B12(JK_AA))+(NP*B13(JK_AA))
              M1=(LP*B21(JK_AA))+(MP*B22(JK_AA))+(NP*B23(JK_AA))
              N1=(LP*B31(JK_AA))+(MP*B32(JK_AA))+(NP*B33(JK_AA))
              LP=L1
              MP=M1
              NP=N1
              L1=(XLP*B11(JK_AA))+(XMP*B12(JK_AA))+(XNP*B13(JK_AA))
              M1=(XLP*B21(JK_AA))+(XMP*B22(JK_AA))+(XNP*B23(JK_AA))
              N1=(XLP*B31(JK_AA))+(XMP*B32(JK_AA))+(XNP*B33(JK_AA))
              XLP=L1
              XMP=M1
              XNP=N1
              L1=(YLP*B11(JK_AA))+(YMP*B12(JK_AA))+(YNP*B13(JK_AA))
              M1=(YLP*B21(JK_AA))+(YMP*B22(JK_AA))+(YNP*B23(JK_AA))
              N1=(YLP*B31(JK_AA))+(YMP*B32(JK_AA))+(YNP*B33(JK_AA))
              YLP=L1
              YMP=M1
              YNP=N1
          END IF
          IF(C.NE.0.0D0) THEN
              JK_AA=C*(PII/180.0D0)
              X1=(XP*C11(JK_AA))+(YP*C12(JK_AA))+(ZP*C13(JK_AA))
              Y1=(XP*C21(JK_AA))+(YP*C22(JK_AA))+(ZP*C23(JK_AA))
              Z1=(XP*C31(JK_AA))+(YP*C32(JK_AA))+(ZP*C33(JK_AA))
              XP=X1
              YP=Y1
              ZP=Z1
              L1=(LP*C11(JK_AA))+(MP*C12(JK_AA))+(NP*C13(JK_AA))
              M1=(LP*C21(JK_AA))+(MP*C22(JK_AA))+(NP*C23(JK_AA))
              N1=(LP*C31(JK_AA))+(MP*C32(JK_AA))+(NP*C33(JK_AA))
              LP=L1
              MP=M1
              NP=N1
              L1=(XLP*C11(JK_AA))+(XMP*C12(JK_AA))+(XNP*C13(JK_AA))
              M1=(XLP*C21(JK_AA))+(XMP*C22(JK_AA))+(XNP*C23(JK_AA))
              N1=(XLP*C31(JK_AA))+(XMP*C32(JK_AA))+(XNP*C33(JK_AA))
              XLP=L1
              XMP=M1
              XNP=N1
              L1=(YLP*C11(JK_AA))+(YMP*C12(JK_AA))+(YNP*C13(JK_AA))
              M1=(YLP*C21(JK_AA))+(YMP*C22(JK_AA))+(YNP*C23(JK_AA))
              N1=(YLP*C31(JK_AA))+(YMP*C32(JK_AA))+(YNP*C33(JK_AA))
              YLP=L1
              YMP=M1
              YNP=N1
          END IF
          NSS_LRAYP(1)=XP
          NSS_LRAYP(2)=YP
          NSS_LRAYP(3)=ZP
          NSS_LRAYP(4)=LP
          NSS_LRAYP(5)=MP
          NSS_LRAYP(6)=NP
          NSS_LRAYP(7)=XLP
          NSS_LRAYP(8)=XMP
          NSS_LRAYP(9)=XNP
          NSS_LRAYP(10)=YLP
          NSS_LRAYP(11)=YMP
          NSS_LRAYP(12)=YNP
          RETURN
      END
      SUBROUTINE NSS_TRANSF_BACK(J)
          USE NSSMOD
C
C       THIS ROUTINE CONVERTS LOCAL RAY DATA AT OBJECT I AND SURFACE J INTO
C       GLOBAL COORDINATE RAY DATA.
C
C       THIS OPERATES ON RAY DATA BEFORE A SURFACE INTERACTION
C
C
          IMPLICIT NONE
C
          REAL*8 JK_AA,A11,A12,A13,A21,A22,A23,A31,A32,A33
     1    ,B11,B12,B13,B21,B22,B23,B31,B32,B33
     2    ,C11,C12,C13,C21,C22,C23,C31,C32,C33,
     3    A,B,C,DX,DY,DZ,AN,X1,Y1,Z1,L1,M1,N1
          REAL*8 X,Y,Z,L,M,N,XL,XM,XN,YL,YM,YN
          REAL*8 XP,YP,ZP,LP,MP,NP,XLP,XMP,XNP,YLP,YMP,YNP
C
          INTEGER J,I
C
          INCLUDE 'datmai.inc'
C
C       ROTATION MATRICIES FUNCTIONS
C
C       A11
          A11(AN)=1.0D0
C       A12
          A12(AN)=0.0D0
C       A13
          A13(AN)=0.0D0
C       A21
          A21(AN)=0.0D0
C       A22
          A22(AN)=DCOS(AN)
C       A23
          A23(AN)=-DSIN(AN)
C       A31
          A31(AN)=0.0D0
C       A32
          A32(AN)=DSIN(AN)
C       A33
          A33(AN)=DCOS(AN)
C       B11
          B11(AN)=DCOS(AN)
C       B12
          B12(AN)=0.0D0
C       B13
          B13(AN)=DSIN(AN)
C       B21
          B21(AN)=0.0D0
C       B22
          B22(AN)=1.0D0
C       B23
          B23(AN)=0.0D0
C       B31
          B31(AN)=-DSIN(AN)
C       B32
          B32(AN)=0.0D0
C       B33
          B33(AN)=DCOS(AN)
C       C11
          C11(AN)=DCOS(AN)
C       C12
          C12(AN)=DSIN(AN)
C       C13
          C13(AN)=0.0D0
C       C21
          C21(AN)=-DSIN(AN)
C       C22
          C22(AN)=DCOS(AN)
C       C23
          C23(AN)=0.0D0
C       C31
          C31(AN)=0.0D0
C       C32
          C32(AN)=0.0D0
C       C33
          C33(AN)=1.0D0
C
C NEG SIGNS ARE USED BECAUSE THE TRANSFORMS WERE ORIGINALLY DESIGNED
C       FOR CODE-V ANGLE SIGN CONVENTIONS FOR ALPHA AND BETA WHICH ARE
C       LEFT HAND AND NOT RIGHT HAND POSITIVE
C       NOW PERFORM THE TRANSFORMATION ATTACHED TO THIS SURFACE
C       WE HAVE A VALID SURFACE TO TRANSFORM
C
          A=-NSSALENS(40,J)
          B=-NSSALENS(41,J)
          C= NSSALENS(42,J)
          DX=NSSALENS(34,J)
          DY=NSSALENS(35,J)
          DZ=NSSALENS(36,J)
          X=NSS_LRAY(1)
          Y=NSS_LRAY(2)
          Z=NSS_LRAY(3)
          L=NSS_LRAY(4)
          M=NSS_LRAY(5)
          N=NSS_LRAY(6)
          XL=NSS_LRAY(7)
          XM=NSS_LRAY(8)
          XN=NSS_LRAY(9)
          YL=NSS_LRAY(10)
          YM=NSS_LRAY(11)
          YN=NSS_LRAY(12)
C
C     RESOLVE TILT AND DECENTER
C
          XP=X
          YP=Y
          ZP=Z
          LP=L
          MP=M
          NP=N
          XLP=XL
          XMP=XM
          XNP=XN
          YLP=YL
          YMP=YM
          YNP=YN
C
C       NOW ROTATE THE SUCKER!
          IF(C.NE.0.0D0) THEN
              JK_AA=-C*(PII/180.0D0)
              X1=(XP*C11(JK_AA))+(YP*C12(JK_AA))+(ZP*C13(JK_AA))
              Y1=(XP*C21(JK_AA))+(YP*C22(JK_AA))+(ZP*C23(JK_AA))
              Z1=(XP*C31(JK_AA))+(YP*C32(JK_AA))+(ZP*C33(JK_AA))
              XP=X1
              YP=Y1
              ZP=Z1
              L1=(LP*C11(JK_AA))+(MP*C12(JK_AA))+(NP*C13(JK_AA))
              M1=(LP*C21(JK_AA))+(MP*C22(JK_AA))+(NP*C23(JK_AA))
              N1=(LP*C31(JK_AA))+(MP*C32(JK_AA))+(NP*C33(JK_AA))
              LP=L1
              MP=M1
              NP=N1
              L1=(XLP*C11(JK_AA))+(XMP*C12(JK_AA))+(XNP*C13(JK_AA))
              M1=(XLP*C21(JK_AA))+(XMP*C22(JK_AA))+(XNP*C23(JK_AA))
              N1=(XLP*C31(JK_AA))+(XMP*C32(JK_AA))+(XNP*C33(JK_AA))
              XLP=L1
              XMP=M1
              XNP=N1
              L1=(YLP*C11(JK_AA))+(YMP*C12(JK_AA))+(YNP*C13(JK_AA))
              M1=(YLP*C21(JK_AA))+(YMP*C22(JK_AA))+(YNP*C23(JK_AA))
              N1=(YLP*C31(JK_AA))+(YMP*C32(JK_AA))+(YNP*C33(JK_AA))
              YLP=L1
              YMP=M1
              YNP=N1
          END IF
          IF(B.NE.0.0D0) THEN
              JK_AA=-B*(PII/180.0D0)
              X1=(XP*B11(JK_AA))+(YP*B12(JK_AA))+(ZP*B13(JK_AA))
              Y1=(XP*B21(JK_AA))+(YP*B22(JK_AA))+(ZP*B23(JK_AA))
              Z1=(XP*B31(JK_AA))+(YP*B32(JK_AA))+(ZP*B33(JK_AA))
              XP=X1
              YP=Y1
              ZP=Z1
              L1=(LP*B11(JK_AA))+(MP*B12(JK_AA))+(NP*B13(JK_AA))
              M1=(LP*B21(JK_AA))+(MP*B22(JK_AA))+(NP*B23(JK_AA))
              N1=(LP*B31(JK_AA))+(MP*B32(JK_AA))+(NP*B33(JK_AA))
              LP=L1
              MP=M1
              NP=N1
              L1=(XLP*B11(JK_AA))+(XMP*B12(JK_AA))+(XNP*B13(JK_AA))
              M1=(XLP*B21(JK_AA))+(XMP*B22(JK_AA))+(XNP*B23(JK_AA))
              N1=(XLP*B31(JK_AA))+(XMP*B32(JK_AA))+(XNP*B33(JK_AA))
              XLP=L1
              XMP=M1
              XNP=N1
              L1=(YLP*B11(JK_AA))+(YMP*B12(JK_AA))+(YNP*B13(JK_AA))
              M1=(YLP*B21(JK_AA))+(YMP*B22(JK_AA))+(YNP*B23(JK_AA))
              N1=(YLP*B31(JK_AA))+(YMP*B32(JK_AA))+(YNP*B33(JK_AA))
              YLP=L1
              YMP=M1
              YNP=N1
          END IF
          IF(A.NE.0.0D0) THEN
              JK_AA=-A*(PII/180.0D0)
              X1=(XP*A11(JK_AA))+(YP*A12(JK_AA))+(ZP*A13(JK_AA))
              Y1=(XP*A21(JK_AA))+(YP*A22(JK_AA))+(ZP*A23(JK_AA))
              Z1=(XP*A31(JK_AA))+(YP*A32(JK_AA))+(ZP*A33(JK_AA))
              XP=X1
              YP=Y1
              ZP=Z1
              L1=(LP*A11(JK_AA))+(MP*A12(JK_AA))+(NP*A13(JK_AA))
              M1=(LP*A21(JK_AA))+(MP*A22(JK_AA))+(NP*A23(JK_AA))
              N1=(LP*A31(JK_AA))+(MP*A32(JK_AA))+(NP*A33(JK_AA))
              LP=L1
              MP=M1
              NP=N1
              L1=(XLP*A11(JK_AA))+(XMP*A12(JK_AA))+(XNP*A13(JK_AA))
              M1=(XLP*A21(JK_AA))+(XMP*A22(JK_AA))+(XNP*A23(JK_AA))
              N1=(XLP*A31(JK_AA))+(XMP*A32(JK_AA))+(XNP*A33(JK_AA))
              XLP=L1
              XMP=M1
              XNP=N1
              L1=(YLP*A11(JK_AA))+(YMP*A12(JK_AA))+(YNP*A13(JK_AA))
              M1=(YLP*A21(JK_AA))+(YMP*A22(JK_AA))+(YNP*A23(JK_AA))
              N1=(YLP*A31(JK_AA))+(YMP*A32(JK_AA))+(YNP*A33(JK_AA))
              YLP=L1
              YMP=M1
              YNP=N1
          END IF
          XP=XP+DX
          YP=YP+DY
          ZP=ZP+DZ
          NSS_GRAY(1)=XP
          NSS_GRAY(2)=YP
          NSS_GRAY(3)=ZP
          NSS_GRAY(4)=LP
          NSS_GRAY(5)=MP
          NSS_GRAY(6)=NP
          NSS_GRAY(7)=XLP
          NSS_GRAY(8)=XMP
          NSS_GRAY(9)=XNP
          NSS_GRAY(10)=YLP
          NSS_GRAY(11)=YMP
          NSS_GRAY(12)=YNP
C       IS TRANSFORMATION REFERENCED TO ANOTHER SURFACE
C       INSTEAD OF TO THE GLOBAL ORIGIN
          NSSNEST(0:200)=-1
          INEST=0
          IF(NSSALENS(37,J).NE.-1.0D0) THEN
              NSSNEST(1)=INT(NSSALENS(37,J))
              INEST=1
              DO I=1,199
                  IF(NSSALENS(37,NSSNEST(I)).NE.-1.0D0) THEN
                      NSSNEST(I+1)=INT(NSSALENS(37,NSSNEST(I)))
                      INEST=I+1
                  ELSE
                      EXIT
                  END IF
              END DO
          END IF
          IF(NSSALENS(37,J).NE.-1.0D0) THEN
C       NOW PERFORM THE REFERENCED
C       WE HAVE A VALID SURFACE TO TRANSFORM
              A=-NSSALENS(40,INT(NSSALENS(37,J)))
              B=-NSSALENS(41,INT(NSSALENS(37,J)))
              C= NSSALENS(42,INT(NSSALENS(37,J)))
              DX=NSSALENS(34,INT(NSSALENS(37,J)))
              DY=NSSALENS(35,INT(NSSALENS(37,J)))
              DZ=NSSALENS(36,INT(NSSALENS(37,J)))
              X=NSS_GRAY(1)
              Y=NSS_GRAY(2)
              Z=NSS_GRAY(3)
              L=NSS_GRAY(4)
              M=NSS_GRAY(5)
              N=NSS_GRAY(6)
              XL=NSS_GRAY(7)
              XM=NSS_GRAY(8)
              XN=NSS_GRAY(9)
              YL=NSS_GRAY(10)
              YM=NSS_GRAY(11)
              YN=NSS_GRAY(12)
C
C     RESOLVE TILT AND DECENTER
C
              XP=X
              YP=Y
              ZP=Z
              LP=L
              MP=M
              NP=N
              XLP=XL
              XMP=XM
              XNP=XN
              YLP=YL
              YMP=YM
              YNP=YN
C
C       NOW ROTATE THE SUCKER!
              IF(C.NE.0.0D0) THEN
                  JK_AA=-C*(PII/180.0D0)
                  X1=(XP*C11(JK_AA))+(YP*C12(JK_AA))+(ZP*C13(JK_AA))
                  Y1=(XP*C21(JK_AA))+(YP*C22(JK_AA))+(ZP*C23(JK_AA))
                  Z1=(XP*C31(JK_AA))+(YP*C32(JK_AA))+(ZP*C33(JK_AA))
                  XP=X1
                  YP=Y1
                  ZP=Z1
                  L1=(LP*C11(JK_AA))+(MP*C12(JK_AA))+(NP*C13(JK_AA))
                  M1=(LP*C21(JK_AA))+(MP*C22(JK_AA))+(NP*C23(JK_AA))
                  N1=(LP*C31(JK_AA))+(MP*C32(JK_AA))+(NP*C33(JK_AA))
                  LP=L1
                  MP=M1
                  NP=N1
                  L1=(XLP*C11(JK_AA))+(XMP*C12(JK_AA))+(XNP*C13(JK_AA))
                  M1=(XLP*C21(JK_AA))+(XMP*C22(JK_AA))+(XNP*C23(JK_AA))
                  N1=(XLP*C31(JK_AA))+(XMP*C32(JK_AA))+(XNP*C33(JK_AA))
                  XLP=L1
                  XMP=M1
                  XNP=N1
                  L1=(YLP*C11(JK_AA))+(YMP*C12(JK_AA))+(YNP*C13(JK_AA))
                  M1=(YLP*C21(JK_AA))+(YMP*C22(JK_AA))+(YNP*C23(JK_AA))
                  N1=(YLP*C31(JK_AA))+(YMP*C32(JK_AA))+(YNP*C33(JK_AA))
                  YLP=L1
                  YMP=M1
                  YNP=N1
              END IF
              IF(B.NE.0.0D0) THEN
                  JK_AA=-B*(PII/180.0D0)
                  X1=(XP*B11(JK_AA))+(YP*B12(JK_AA))+(ZP*B13(JK_AA))
                  Y1=(XP*B21(JK_AA))+(YP*B22(JK_AA))+(ZP*B23(JK_AA))
                  Z1=(XP*B31(JK_AA))+(YP*B32(JK_AA))+(ZP*B33(JK_AA))
                  XP=X1
                  YP=Y1
                  ZP=Z1
                  L1=(LP*B11(JK_AA))+(MP*B12(JK_AA))+(NP*B13(JK_AA))
                  M1=(LP*B21(JK_AA))+(MP*B22(JK_AA))+(NP*B23(JK_AA))
                  N1=(LP*B31(JK_AA))+(MP*B32(JK_AA))+(NP*B33(JK_AA))
                  LP=L1
                  MP=M1
                  NP=N1
                  L1=(XLP*B11(JK_AA))+(XMP*B12(JK_AA))+(XNP*B13(JK_AA))
                  M1=(XLP*B21(JK_AA))+(XMP*B22(JK_AA))+(XNP*B23(JK_AA))
                  N1=(XLP*B31(JK_AA))+(XMP*B32(JK_AA))+(XNP*B33(JK_AA))
                  XLP=L1
                  XMP=M1
                  XNP=N1
                  L1=(YLP*B11(JK_AA))+(YMP*B12(JK_AA))+(YNP*B13(JK_AA))
                  M1=(YLP*B21(JK_AA))+(YMP*B22(JK_AA))+(YNP*B23(JK_AA))
                  N1=(YLP*B31(JK_AA))+(YMP*B32(JK_AA))+(YNP*B33(JK_AA))
                  YLP=L1
                  YMP=M1
                  YNP=N1
              END IF
              IF(A.NE.0.0D0) THEN
                  JK_AA=-A*(PII/180.0D0)
                  X1=(XP*A11(JK_AA))+(YP*A12(JK_AA))+(ZP*A13(JK_AA))
                  Y1=(XP*A21(JK_AA))+(YP*A22(JK_AA))+(ZP*A23(JK_AA))
                  Z1=(XP*A31(JK_AA))+(YP*A32(JK_AA))+(ZP*A33(JK_AA))
                  XP=X1
                  YP=Y1
                  ZP=Z1
                  L1=(LP*A11(JK_AA))+(MP*A12(JK_AA))+(NP*A13(JK_AA))
                  M1=(LP*A21(JK_AA))+(MP*A22(JK_AA))+(NP*A23(JK_AA))
                  N1=(LP*A31(JK_AA))+(MP*A32(JK_AA))+(NP*A33(JK_AA))
                  LP=L1
                  MP=M1
                  NP=N1
                  L1=(XLP*A11(JK_AA))+(XMP*A12(JK_AA))+(XNP*A13(JK_AA))
                  M1=(XLP*A21(JK_AA))+(XMP*A22(JK_AA))+(XNP*A23(JK_AA))
                  N1=(XLP*A31(JK_AA))+(XMP*A32(JK_AA))+(XNP*A33(JK_AA))
                  XLP=L1
                  XMP=M1
                  XNP=N1
                  L1=(YLP*A11(JK_AA))+(YMP*A12(JK_AA))+(YNP*A13(JK_AA))
                  M1=(YLP*A21(JK_AA))+(YMP*A22(JK_AA))+(YNP*A23(JK_AA))
                  N1=(YLP*A31(JK_AA))+(YMP*A32(JK_AA))+(YNP*A33(JK_AA))
                  YLP=L1
                  YMP=M1
                  YNP=N1
              END IF
              XP=XP+DX
              YP=YP+DY
              ZP=ZP+DZ
              NSS_GRAY(1)=XP
              NSS_GRAY(2)=YP
              NSS_GRAY(3)=ZP
              NSS_GRAY(4)=LP
              NSS_GRAY(5)=MP
              NSS_GRAY(6)=NP
              NSS_GRAY(7)=XLP
              NSS_GRAY(8)=XMP
              NSS_GRAY(9)=XNP
              NSS_GRAY(10)=YLP
              NSS_GRAY(11)=YMP
              NSS_GRAY(12)=YNP
          END IF
          RETURN
      END
      SUBROUTINE NSS_TRANSFP_BACK(J)
          USE NSSMOD
C
C       THIS ROUTINE CONVERTS LOCAL RAY DATA AT OBJECT I AND SURFACE J INTO
C       GLOBAL COORDINATE RAY DATA.
C
C       THIS OPERATES ON RAY DATA AFTER A SURFACE INTERACTION
C
C
C
          IMPLICIT NONE
C
          REAL*8 JK_AA,A11,A12,A13,A21,A22,A23,A31,A32,A33
     1    ,B11,B12,B13,B21,B22,B23,B31,B32,B33
     2    ,C11,C12,C13,C21,C22,C23,C31,C32,C33,
     3    A,B,C,DX,DY,DZ,AN,X1,Y1,Z1,L1,M1,N1
          REAL*8 X,Y,Z,L,M,N,XL,XM,XN,YL,YM,YN
          REAL*8 XP,YP,ZP,LP,MP,NP,XLP,XMP,XNP,YLP,YMP,YNP
C
          INTEGER J,I
C
          INCLUDE 'datmai.inc'
C
C       ROTATION MATRICIES FUNCTIONS
C
C       A11
          A11(AN)=1.0D0
C       A12
          A12(AN)=0.0D0
C       A13
          A13(AN)=0.0D0
C       A21
          A21(AN)=0.0D0
C       A22
          A22(AN)=DCOS(AN)
C       A23
          A23(AN)=-DSIN(AN)
C       A31
          A31(AN)=0.0D0
C       A32
          A32(AN)=DSIN(AN)
C       A33
          A33(AN)=DCOS(AN)
C       B11
          B11(AN)=DCOS(AN)
C       B12
          B12(AN)=0.0D0
C       B13
          B13(AN)=DSIN(AN)
C       B21
          B21(AN)=0.0D0
C       B22
          B22(AN)=1.0D0
C       B23
          B23(AN)=0.0D0
C       B31
          B31(AN)=-DSIN(AN)
C       B32
          B32(AN)=0.0D0
C       B33
          B33(AN)=DCOS(AN)
C       C11
          C11(AN)=DCOS(AN)
C       C12
          C12(AN)=DSIN(AN)
C       C13
          C13(AN)=0.0D0
C       C21
          C21(AN)=-DSIN(AN)
C       C22
          C22(AN)=DCOS(AN)
C       C23
          C23(AN)=0.0D0
C       C31
          C31(AN)=0.0D0
C       C32
          C32(AN)=0.0D0
C       C33
          C33(AN)=1.0D0
C
C NEG SIGNS ARE USED BECAUSE THE TRANSFORMS WERE ORIGINALLY DESIGNED
C       FOR CODE-V ANGLE SIGN CONVENTIONS FOR ALPHA AND BETA WHICH ARE
C       LEFT HAND AND NOT RIGHT HAND POSITIVE
C       NOW PERFORM THE TRANSFORMATION ATTACHED TO THIS SURFACE
C       WE HAVE A VALID SURFACE TO TRANSFORM
C
          A=-NSSALENS(40,J)
          B=-NSSALENS(41,J)
          C= NSSALENS(42,J)
          DX=NSSALENS(34,J)
          DY=NSSALENS(35,J)
          DZ=NSSALENS(36,J)
          X=NSS_LRAYP(1)
          Y=NSS_LRAYP(2)
          Z=NSS_LRAYP(3)
          L=NSS_LRAYP(4)
          M=NSS_LRAYP(5)
          N=NSS_LRAYP(6)
          XL=NSS_LRAYP(7)
          XM=NSS_LRAYP(8)
          XN=NSS_LRAYP(9)
          YL=NSS_LRAYP(10)
          YM=NSS_LRAYP(11)
          YN=NSS_LRAYP(12)
C
C     RESOLVE TILT AND DECENTER
C
          XP=X
          YP=Y
          ZP=Z
          LP=L
          MP=M
          NP=N
          XLP=XL
          XMP=XM
          XNP=XN
          YLP=YL
          YMP=YM
          YNP=YN
C
C       NOW ROTATE THE SUCKER!
          IF(C.NE.0.0D0) THEN
              JK_AA=-C*(PII/180.0D0)
              X1=(XP*C11(JK_AA))+(YP*C12(JK_AA))+(ZP*C13(JK_AA))
              Y1=(XP*C21(JK_AA))+(YP*C22(JK_AA))+(ZP*C23(JK_AA))
              Z1=(XP*C31(JK_AA))+(YP*C32(JK_AA))+(ZP*C33(JK_AA))
              XP=X1
              YP=Y1
              ZP=Z1
              L1=(LP*C11(JK_AA))+(MP*C12(JK_AA))+(NP*C13(JK_AA))
              M1=(LP*C21(JK_AA))+(MP*C22(JK_AA))+(NP*C23(JK_AA))
              N1=(LP*C31(JK_AA))+(MP*C32(JK_AA))+(NP*C33(JK_AA))
              LP=L1
              MP=M1
              NP=N1
              L1=(XLP*C11(JK_AA))+(XMP*C12(JK_AA))+(XNP*C13(JK_AA))
              M1=(XLP*C21(JK_AA))+(XMP*C22(JK_AA))+(XNP*C23(JK_AA))
              N1=(XLP*C31(JK_AA))+(XMP*C32(JK_AA))+(XNP*C33(JK_AA))
              XLP=L1
              XMP=M1
              XNP=N1
              L1=(YLP*C11(JK_AA))+(YMP*C12(JK_AA))+(YNP*C13(JK_AA))
              M1=(YLP*C21(JK_AA))+(YMP*C22(JK_AA))+(YNP*C23(JK_AA))
              N1=(YLP*C31(JK_AA))+(YMP*C32(JK_AA))+(YNP*C33(JK_AA))
              YLP=L1
              YMP=M1
              YNP=N1
          END IF
          IF(B.NE.0.0D0) THEN
              JK_AA=-B*(PII/180.0D0)
              X1=(XP*B11(JK_AA))+(YP*B12(JK_AA))+(ZP*B13(JK_AA))
              Y1=(XP*B21(JK_AA))+(YP*B22(JK_AA))+(ZP*B23(JK_AA))
              Z1=(XP*B31(JK_AA))+(YP*B32(JK_AA))+(ZP*B33(JK_AA))
              XP=X1
              YP=Y1
              ZP=Z1
              L1=(LP*B11(JK_AA))+(MP*B12(JK_AA))+(NP*B13(JK_AA))
              M1=(LP*B21(JK_AA))+(MP*B22(JK_AA))+(NP*B23(JK_AA))
              N1=(LP*B31(JK_AA))+(MP*B32(JK_AA))+(NP*B33(JK_AA))
              LP=L1
              MP=M1
              NP=N1
              L1=(XLP*B11(JK_AA))+(XMP*B12(JK_AA))+(XNP*B13(JK_AA))
              M1=(XLP*B21(JK_AA))+(XMP*B22(JK_AA))+(XNP*B23(JK_AA))
              N1=(XLP*B31(JK_AA))+(XMP*B32(JK_AA))+(XNP*B33(JK_AA))
              XLP=L1
              XMP=M1
              XNP=N1
              L1=(YLP*B11(JK_AA))+(YMP*B12(JK_AA))+(YNP*B13(JK_AA))
              M1=(YLP*B21(JK_AA))+(YMP*B22(JK_AA))+(YNP*B23(JK_AA))
              N1=(YLP*B31(JK_AA))+(YMP*B32(JK_AA))+(YNP*B33(JK_AA))
              YLP=L1
              YMP=M1
              YNP=N1
          END IF
          IF(A.NE.0.0D0) THEN
              JK_AA=-A*(PII/180.0D0)
              X1=(XP*A11(JK_AA))+(YP*A12(JK_AA))+(ZP*A13(JK_AA))
              Y1=(XP*A21(JK_AA))+(YP*A22(JK_AA))+(ZP*A23(JK_AA))
              Z1=(XP*A31(JK_AA))+(YP*A32(JK_AA))+(ZP*A33(JK_AA))
              XP=X1
              YP=Y1
              ZP=Z1
              L1=(LP*A11(JK_AA))+(MP*A12(JK_AA))+(NP*A13(JK_AA))
              M1=(LP*A21(JK_AA))+(MP*A22(JK_AA))+(NP*A23(JK_AA))
              N1=(LP*A31(JK_AA))+(MP*A32(JK_AA))+(NP*A33(JK_AA))
              LP=L1
              MP=M1
              NP=N1
              L1=(XLP*A11(JK_AA))+(XMP*A12(JK_AA))+(XNP*A13(JK_AA))
              M1=(XLP*A21(JK_AA))+(XMP*A22(JK_AA))+(XNP*A23(JK_AA))
              N1=(XLP*A31(JK_AA))+(XMP*A32(JK_AA))+(XNP*A33(JK_AA))
              XLP=L1
              XMP=M1
              XNP=N1
              L1=(YLP*A11(JK_AA))+(YMP*A12(JK_AA))+(YNP*A13(JK_AA))
              M1=(YLP*A21(JK_AA))+(YMP*A22(JK_AA))+(YNP*A23(JK_AA))
              N1=(YLP*A31(JK_AA))+(YMP*A32(JK_AA))+(YNP*A33(JK_AA))
              YLP=L1
              YMP=M1
              YNP=N1
          END IF
          XP=XP+DX
          YP=YP+DY
          ZP=ZP+DZ
          NSS_GRAYP(1)=XP
          NSS_GRAYP(2)=YP
          NSS_GRAYP(3)=ZP
          NSS_GRAYP(4)=LP
          NSS_GRAYP(5)=MP
          NSS_GRAYP(6)=NP
          NSS_GRAYP(7)=XLP
          NSS_GRAYP(8)=XMP
          NSS_GRAYP(9)=XNP
          NSS_GRAYP(10)=YLP
          NSS_GRAYP(11)=YMP
          NSS_GRAYP(12)=YNP
          NSSNEST(0:200)=-1
          INEST=0
          IF(NSSALENS(37,J).NE.-1.0D0) THEN
              NSSNEST(1)=INT(NSSALENS(37,J))
              INEST=1
              DO I=1,199
                  IF(NSSALENS(37,NSSNEST(I)).NE.-1.0D0) THEN
                      NSSNEST(I+1)=INT(NSSALENS(37,NSSNEST(I)))
                      INEST=I+1
                  ELSE
                      EXIT
                  END IF
              END DO
          END IF
          IF(NSSALENS(37,J).NE.-1.0D0) THEN
C       NOW PERFORM THE REFERENCED
C       WE HAVE A VALID SURFACE TO TRANSFORM
              A=-NSSALENS(40,INT(NSSALENS(37,J)))
              B=-NSSALENS(41,INT(NSSALENS(37,J)))
              C= NSSALENS(42,INT(NSSALENS(37,J)))
              DX=NSSALENS(34,INT(NSSALENS(37,J)))
              DY=NSSALENS(35,INT(NSSALENS(37,J)))
              DZ=NSSALENS(36,INT(NSSALENS(37,J)))
              X=NSS_GRAYP(1)
              Y=NSS_GRAYP(2)
              Z=NSS_GRAYP(3)
              L=NSS_GRAYP(4)
              M=NSS_GRAYP(5)
              N=NSS_GRAYP(6)
              XL=NSS_GRAYP(7)
              XM=NSS_GRAYP(8)
              XN=NSS_GRAYP(9)
              YL=NSS_GRAYP(10)
              YM=NSS_GRAYP(11)
              YN=NSS_GRAYP(12)
C
C     RESOLVE TILT AND DECENTER
C
              XP=X
              YP=Y
              ZP=Z
              LP=L
              MP=M
              NP=N
              XLP=XL
              XMP=XM
              XNP=XN
              YLP=YL
              YMP=YM
              YNP=YN
C
C       NOW ROTATE THE SUCKER!
              IF(C.NE.0.0D0) THEN
                  JK_AA=-C*(PII/180.0D0)
                  X1=(XP*C11(JK_AA))+(YP*C12(JK_AA))+(ZP*C13(JK_AA))
                  Y1=(XP*C21(JK_AA))+(YP*C22(JK_AA))+(ZP*C23(JK_AA))
                  Z1=(XP*C31(JK_AA))+(YP*C32(JK_AA))+(ZP*C33(JK_AA))
                  XP=X1
                  YP=Y1
                  ZP=Z1
                  L1=(LP*C11(JK_AA))+(MP*C12(JK_AA))+(NP*C13(JK_AA))
                  M1=(LP*C21(JK_AA))+(MP*C22(JK_AA))+(NP*C23(JK_AA))
                  N1=(LP*C31(JK_AA))+(MP*C32(JK_AA))+(NP*C33(JK_AA))
                  LP=L1
                  MP=M1
                  NP=N1
                  L1=(XLP*C11(JK_AA))+(XMP*C12(JK_AA))+(XNP*C13(JK_AA))
                  M1=(XLP*C21(JK_AA))+(XMP*C22(JK_AA))+(XNP*C23(JK_AA))
                  N1=(XLP*C31(JK_AA))+(XMP*C32(JK_AA))+(XNP*C33(JK_AA))
                  XLP=L1
                  XMP=M1
                  XNP=N1
                  L1=(YLP*C11(JK_AA))+(YMP*C12(JK_AA))+(YNP*C13(JK_AA))
                  M1=(YLP*C21(JK_AA))+(YMP*C22(JK_AA))+(YNP*C23(JK_AA))
                  N1=(YLP*C31(JK_AA))+(YMP*C32(JK_AA))+(YNP*C33(JK_AA))
                  YLP=L1
                  YMP=M1
                  YNP=N1
              END IF
              IF(B.NE.0.0D0) THEN
                  JK_AA=-B*(PII/180.0D0)
                  X1=(XP*B11(JK_AA))+(YP*B12(JK_AA))+(ZP*B13(JK_AA))
                  Y1=(XP*B21(JK_AA))+(YP*B22(JK_AA))+(ZP*B23(JK_AA))
                  Z1=(XP*B31(JK_AA))+(YP*B32(JK_AA))+(ZP*B33(JK_AA))
                  XP=X1
                  YP=Y1
                  ZP=Z1
                  L1=(LP*B11(JK_AA))+(MP*B12(JK_AA))+(NP*B13(JK_AA))
                  M1=(LP*B21(JK_AA))+(MP*B22(JK_AA))+(NP*B23(JK_AA))
                  N1=(LP*B31(JK_AA))+(MP*B32(JK_AA))+(NP*B33(JK_AA))
                  LP=L1
                  MP=M1
                  NP=N1
                  L1=(XLP*B11(JK_AA))+(XMP*B12(JK_AA))+(XNP*B13(JK_AA))
                  M1=(XLP*B21(JK_AA))+(XMP*B22(JK_AA))+(XNP*B23(JK_AA))
                  N1=(XLP*B31(JK_AA))+(XMP*B32(JK_AA))+(XNP*B33(JK_AA))
                  XLP=L1
                  XMP=M1
                  XNP=N1
                  L1=(YLP*B11(JK_AA))+(YMP*B12(JK_AA))+(YNP*B13(JK_AA))
                  M1=(YLP*B21(JK_AA))+(YMP*B22(JK_AA))+(YNP*B23(JK_AA))
                  N1=(YLP*B31(JK_AA))+(YMP*B32(JK_AA))+(YNP*B33(JK_AA))
                  YLP=L1
                  YMP=M1
                  YNP=N1
              END IF
              IF(A.NE.0.0D0) THEN
                  JK_AA=-A*(PII/180.0D0)
                  X1=(XP*A11(JK_AA))+(YP*A12(JK_AA))+(ZP*A13(JK_AA))
                  Y1=(XP*A21(JK_AA))+(YP*A22(JK_AA))+(ZP*A23(JK_AA))
                  Z1=(XP*A31(JK_AA))+(YP*A32(JK_AA))+(ZP*A33(JK_AA))
                  XP=X1
                  YP=Y1
                  ZP=Z1
                  L1=(LP*A11(JK_AA))+(MP*A12(JK_AA))+(NP*A13(JK_AA))
                  M1=(LP*A21(JK_AA))+(MP*A22(JK_AA))+(NP*A23(JK_AA))
                  N1=(LP*A31(JK_AA))+(MP*A32(JK_AA))+(NP*A33(JK_AA))
                  LP=L1
                  MP=M1
                  NP=N1
                  L1=(XLP*A11(JK_AA))+(XMP*A12(JK_AA))+(XNP*A13(JK_AA))
                  M1=(XLP*A21(JK_AA))+(XMP*A22(JK_AA))+(XNP*A23(JK_AA))
                  N1=(XLP*A31(JK_AA))+(XMP*A32(JK_AA))+(XNP*A33(JK_AA))
                  XLP=L1
                  XMP=M1
                  XNP=N1
                  L1=(YLP*A11(JK_AA))+(YMP*A12(JK_AA))+(YNP*A13(JK_AA))
                  M1=(YLP*A21(JK_AA))+(YMP*A22(JK_AA))+(YNP*A23(JK_AA))
                  N1=(YLP*A31(JK_AA))+(YMP*A32(JK_AA))+(YNP*A33(JK_AA))
                  YLP=L1
                  YMP=M1
                  YNP=N1
              END IF
              XP=XP+DX
              YP=YP+DY
              ZP=ZP+DZ
              NSS_GRAYP(1)=XP
              NSS_GRAYP(2)=YP
              NSS_GRAYP(3)=ZP
              NSS_GRAYP(4)=LP
              NSS_GRAYP(5)=MP
              NSS_GRAYP(6)=NP
              NSS_GRAYP(7)=XLP
              NSS_GRAYP(8)=XMP
              NSS_GRAYP(9)=XNP
              NSS_GRAYP(10)=YLP
              NSS_GRAYP(11)=YMP
              NSS_GRAYP(12)=YNP
          END IF
          RETURN
      END
      SUBROUTINE LOCAL_TO_GLOBAL
     1(GX0,GY0,GZ0,GL0,GM0,GN0,GX,GY,GZ,GL,GM,GN,J)
          USE NSSMOD
C
          IMPLICIT NONE
C
          REAL*8 JK_AA,A11,A12,A13,A21,A22,A23,A31,A32,A33
     1    ,B11,B12,B13,B21,B22,B23,B31,B32,B33
     2    ,C11,C12,C13,C21,C22,C23,C31,C32,C33,
     3    A,B,C,DX,DY,DZ,AN,X1,Y1,Z1,L1,M1,N1
          REAL*8 X,Y,Z,L,M,N
          REAL*8 GX0,GY0,GZ0,GL0,GM0,GN0,GX,GY,GZ,GL,GM,GN
          REAL*8 XP,YP,ZP,LP,MP,NP
C
          INTEGER J,I
C
          INCLUDE 'datmai.inc'
C
C       ROTATION MATRICIES FUNCTIONS
C
C       A11
          A11(AN)=1.0D0
C       A12
          A12(AN)=0.0D0
C       A13
          A13(AN)=0.0D0
C       A21
          A21(AN)=0.0D0
C       A22
          A22(AN)=DCOS(AN)
C       A23
          A23(AN)=-DSIN(AN)
C       A31
          A31(AN)=0.0D0
C       A32
          A32(AN)=DSIN(AN)
C       A33
          A33(AN)=DCOS(AN)
C       B11
          B11(AN)=DCOS(AN)
C       B12
          B12(AN)=0.0D0
C       B13
          B13(AN)=DSIN(AN)
C       B21
          B21(AN)=0.0D0
C       B22
          B22(AN)=1.0D0
C       B23
          B23(AN)=0.0D0
C       B31
          B31(AN)=-DSIN(AN)
C       B32
          B32(AN)=0.0D0
C       B33
          B33(AN)=DCOS(AN)
C       C11
          C11(AN)=DCOS(AN)
C       C12
          C12(AN)=DSIN(AN)
C       C13
          C13(AN)=0.0D0
C       C21
          C21(AN)=-DSIN(AN)
C       C22
          C22(AN)=DCOS(AN)
C       C23
          C23(AN)=0.0D0
C       C31
          C31(AN)=0.0D0
C       C32
          C32(AN)=0.0D0
C       C33
          C33(AN)=1.0D0
C
C       NOW PERFORM THE TRANSFORMATION ATTACHED TO THIS SURFACE
C       WE HAVE A VALID SURFACE TO TRANSFORM
          A=-NSSALENS(40,J)
          B=-NSSALENS(41,J)
          C= NSSALENS(42,J)
          DX=NSSALENS(34,J)
          DY=NSSALENS(35,J)
          DZ=NSSALENS(36,J)
          X=GX0
          Y=GY0
          Z=GZ0
          L=GL0
          M=GM0
          N=GN0
C
          XP=X
          YP=Y
          ZP=Z
          LP=L
          MP=M
          NP=N
C
          IF(C.NE.0.0D0) THEN
              JK_AA=-C*(PII/180.0D0)
              X1=(XP*C11(JK_AA))+(YP*C12(JK_AA))+(ZP*C13(JK_AA))
              Y1=(XP*C21(JK_AA))+(YP*C22(JK_AA))+(ZP*C23(JK_AA))
              Z1=(XP*C31(JK_AA))+(YP*C32(JK_AA))+(ZP*C33(JK_AA))
              L1=(LP*C11(JK_AA))+(MP*C12(JK_AA))+(NP*C13(JK_AA))
              M1=(LP*C21(JK_AA))+(MP*C22(JK_AA))+(NP*C23(JK_AA))
              N1=(LP*C31(JK_AA))+(MP*C32(JK_AA))+(NP*C33(JK_AA))
              XP=X1
              YP=Y1
              ZP=Z1
              LP=L1
              MP=M1
              NP=N1
          END IF
          IF(B.NE.0.0D0) THEN
              JK_AA=-B*(PII/180.0D0)
              X1=(XP*B11(JK_AA))+(YP*B12(JK_AA))+(ZP*B13(JK_AA))
              Y1=(XP*B21(JK_AA))+(YP*B22(JK_AA))+(ZP*B23(JK_AA))
              Z1=(XP*B31(JK_AA))+(YP*B32(JK_AA))+(ZP*B33(JK_AA))
              L1=(LP*B11(JK_AA))+(MP*B12(JK_AA))+(NP*B13(JK_AA))
              M1=(LP*B21(JK_AA))+(MP*B22(JK_AA))+(NP*B23(JK_AA))
              N1=(LP*B31(JK_AA))+(MP*B32(JK_AA))+(NP*B33(JK_AA))
              XP=X1
              YP=Y1
              ZP=Z1
              LP=L1
              MP=M1
              NP=N1
          END IF
          IF(A.NE.0.0D0) THEN
              JK_AA=-A*(PII/180.0D0)
              X1=(XP*A11(JK_AA))+(YP*A12(JK_AA))+(ZP*A13(JK_AA))
              Y1=(XP*A21(JK_AA))+(YP*A22(JK_AA))+(ZP*A23(JK_AA))
              Z1=(XP*A31(JK_AA))+(YP*A32(JK_AA))+(ZP*A33(JK_AA))
              L1=(LP*A11(JK_AA))+(MP*A12(JK_AA))+(NP*A13(JK_AA))
              M1=(LP*A21(JK_AA))+(MP*A22(JK_AA))+(NP*A23(JK_AA))
              N1=(LP*A31(JK_AA))+(MP*A32(JK_AA))+(NP*A33(JK_AA))
              XP=X1
              YP=Y1
              ZP=Z1
              LP=L1
              MP=M1
              NP=N1
          END IF
          XP=XP+DX
          YP=YP+DY
          ZP=ZP+DZ
          GX0=XP
          GY0=YP
          GZ0=ZP
          GL0=LP
          GM0=MP
          GN0=NP
C
C       NEXT
C
          X=GX
          Y=GY
          Z=GZ
          L=GL
          M=GM
          N=GN
C
C     RESOLVE TILT AND DECENTER
C
          XP=X
          YP=Y
          ZP=Z
          LP=L
          MP=M
          NP=N
C
          IF(C.NE.0.0D0) THEN
              JK_AA=-C*(PII/180.0D0)
              X1=(XP*C11(JK_AA))+(YP*C12(JK_AA))+(ZP*C13(JK_AA))
              Y1=(XP*C21(JK_AA))+(YP*C22(JK_AA))+(ZP*C23(JK_AA))
              Z1=(XP*C31(JK_AA))+(YP*C32(JK_AA))+(ZP*C33(JK_AA))
              L1=(LP*C11(JK_AA))+(MP*C12(JK_AA))+(NP*C13(JK_AA))
              M1=(LP*C21(JK_AA))+(MP*C22(JK_AA))+(NP*C23(JK_AA))
              N1=(LP*C31(JK_AA))+(MP*C32(JK_AA))+(NP*C33(JK_AA))
              XP=X1
              YP=Y1
              ZP=Z1
              LP=L1
              MP=M1
              NP=N1
          END IF
          IF(B.NE.0.0D0) THEN
              JK_AA=-B*(PII/180.0D0)
              X1=(XP*B11(JK_AA))+(YP*B12(JK_AA))+(ZP*B13(JK_AA))
              Y1=(XP*B21(JK_AA))+(YP*B22(JK_AA))+(ZP*B23(JK_AA))
              Z1=(XP*B31(JK_AA))+(YP*B32(JK_AA))+(ZP*B33(JK_AA))
              L1=(LP*B11(JK_AA))+(MP*B12(JK_AA))+(NP*B13(JK_AA))
              M1=(LP*B21(JK_AA))+(MP*B22(JK_AA))+(NP*B23(JK_AA))
              N1=(LP*B31(JK_AA))+(MP*B32(JK_AA))+(NP*B33(JK_AA))
              XP=X1
              YP=Y1
              ZP=Z1
              LP=L1
              MP=M1
              NP=N1
          END IF
          IF(A.NE.0.0D0) THEN
              JK_AA=-A*(PII/180.0D0)
              X1=(XP*A11(JK_AA))+(YP*A12(JK_AA))+(ZP*A13(JK_AA))
              Y1=(XP*A21(JK_AA))+(YP*A22(JK_AA))+(ZP*A23(JK_AA))
              Z1=(XP*A31(JK_AA))+(YP*A32(JK_AA))+(ZP*A33(JK_AA))
              L1=(LP*A11(JK_AA))+(MP*A12(JK_AA))+(NP*A13(JK_AA))
              M1=(LP*A21(JK_AA))+(MP*A22(JK_AA))+(NP*A23(JK_AA))
              N1=(LP*A31(JK_AA))+(MP*A32(JK_AA))+(NP*A33(JK_AA))
              XP=X1
              YP=Y1
              ZP=Z1
              LP=L1
              MP=M1
              NP=N1
          END IF
          XP=XP+DX
          YP=YP+DY
          ZP=ZP+DZ
          GX=XP
          GY=YP
          GZ=ZP
          GL=LP
          GM=MP
          GN=NP
C       IS TRANSFORMATION REFERENCED TO ANOTHER SURFACE
C       INSTEAD OF TO THE GLOBAL ORIGIN
          NSSNEST(0:200)=-1
          INEST=0
          IF(NSSALENS(37,J).NE.-1.0D0) THEN
              NSSNEST(1)=INT(NSSALENS(37,J))
              INEST=1
              DO I=1,199
                  IF(NSSALENS(37,NSSNEST(I)).NE.-1.0D0) THEN
                      NSSNEST(I+1)=INT(NSSALENS(37,NSSNEST(I)))
                      INEST=I+1
                  ELSE
                      EXIT
                  END IF
              END DO
          END IF
          IF(NSSALENS(37,J).NE.-1.0D0) THEN
C       NOW PERFORM THE REFERENCED
C       WE HAVE A VALID SURFACE TO TRANSFORM
              A=-NSSALENS(40,INT(NSSALENS(37,J)))
              B=-NSSALENS(41,INT(NSSALENS(37,J)))
              C= NSSALENS(42,INT(NSSALENS(37,J)))
              DX=NSSALENS(34,INT(NSSALENS(37,J)))
              DY=NSSALENS(35,INT(NSSALENS(37,J)))
              DZ=NSSALENS(36,INT(NSSALENS(37,J)))
              X=GX0
              Y=GY0
              Z=GZ0
              L=GL0
              M=GM0
              N=GN0
              XP=X
              YP=Y
              ZP=Z
              LP=L
              MP=M
              NP=N
C
              IF(C.NE.0.0D0) THEN
                  JK_AA=-C*(PII/180.0D0)
                  X1=(XP*C11(JK_AA))+(YP*C12(JK_AA))+(ZP*C13(JK_AA))
                  Y1=(XP*C21(JK_AA))+(YP*C22(JK_AA))+(ZP*C23(JK_AA))
                  Z1=(XP*C31(JK_AA))+(YP*C32(JK_AA))+(ZP*C33(JK_AA))
                  L1=(LP*C11(JK_AA))+(MP*C12(JK_AA))+(NP*C13(JK_AA))
                  M1=(LP*C21(JK_AA))+(MP*C22(JK_AA))+(NP*C23(JK_AA))
                  N1=(LP*C31(JK_AA))+(MP*C32(JK_AA))+(NP*C33(JK_AA))
                  XP=X1
                  YP=Y1
                  ZP=Z1
                  LP=L1
                  MP=M1
                  NP=N1
              END IF
              IF(B.NE.0.0D0) THEN
                  JK_AA=-B*(PII/180.0D0)
                  X1=(XP*B11(JK_AA))+(YP*B12(JK_AA))+(ZP*B13(JK_AA))
                  Y1=(XP*B21(JK_AA))+(YP*B22(JK_AA))+(ZP*B23(JK_AA))
                  Z1=(XP*B31(JK_AA))+(YP*B32(JK_AA))+(ZP*B33(JK_AA))
                  L1=(LP*B11(JK_AA))+(MP*B12(JK_AA))+(NP*B13(JK_AA))
                  M1=(LP*B21(JK_AA))+(MP*B22(JK_AA))+(NP*B23(JK_AA))
                  N1=(LP*B31(JK_AA))+(MP*B32(JK_AA))+(NP*B33(JK_AA))
                  XP=X1
                  YP=Y1
                  ZP=Z1
                  LP=L1
                  MP=M1
                  NP=N1
              END IF
              IF(A.NE.0.0D0) THEN
                  JK_AA=-A*(PII/180.0D0)
                  X1=(XP*A11(JK_AA))+(YP*A12(JK_AA))+(ZP*A13(JK_AA))
                  Y1=(XP*A21(JK_AA))+(YP*A22(JK_AA))+(ZP*A23(JK_AA))
                  Z1=(XP*A31(JK_AA))+(YP*A32(JK_AA))+(ZP*A33(JK_AA))
                  L1=(LP*A11(JK_AA))+(MP*A12(JK_AA))+(NP*A13(JK_AA))
                  M1=(LP*A21(JK_AA))+(MP*A22(JK_AA))+(NP*A23(JK_AA))
                  N1=(LP*A31(JK_AA))+(MP*A32(JK_AA))+(NP*A33(JK_AA))
                  XP=X1
                  YP=Y1
                  ZP=Z1
                  LP=L1
                  MP=M1
                  NP=N1
              END IF
              XP=XP+DX
              YP=YP+DY
              ZP=ZP+DZ
              GX0=XP
              GY0=YP
              GZ0=ZP
              GL0=LP
              GM0=MP
              GN0=NP
C
C       NEXT
C
              X=GX
              Y=GY
              Z=GZ
              L=GL
              M=GM
              N=GN
C
              XP=X
              YP=Y
              ZP=Z
              LP=L
              MP=M
              NP=N
C
              IF(C.NE.0.0D0) THEN
                  JK_AA=-C*(PII/180.0D0)
                  X1=(XP*C11(JK_AA))+(YP*C12(JK_AA))+(ZP*C13(JK_AA))
                  Y1=(XP*C21(JK_AA))+(YP*C22(JK_AA))+(ZP*C23(JK_AA))
                  Z1=(XP*C31(JK_AA))+(YP*C32(JK_AA))+(ZP*C33(JK_AA))
                  L1=(LP*C11(JK_AA))+(MP*C12(JK_AA))+(NP*C13(JK_AA))
                  M1=(LP*C21(JK_AA))+(MP*C22(JK_AA))+(NP*C23(JK_AA))
                  N1=(LP*C31(JK_AA))+(MP*C32(JK_AA))+(NP*C33(JK_AA))
                  XP=X1
                  YP=Y1
                  ZP=Z1
                  LP=L1
                  MP=M1
                  NP=N1
              END IF
              IF(B.NE.0.0D0) THEN
                  JK_AA=-B*(PII/180.0D0)
                  X1=(XP*B11(JK_AA))+(YP*B12(JK_AA))+(ZP*B13(JK_AA))
                  Y1=(XP*B21(JK_AA))+(YP*B22(JK_AA))+(ZP*B23(JK_AA))
                  Z1=(XP*B31(JK_AA))+(YP*B32(JK_AA))+(ZP*B33(JK_AA))
                  L1=(LP*B11(JK_AA))+(MP*B12(JK_AA))+(NP*B13(JK_AA))
                  M1=(LP*B21(JK_AA))+(MP*B22(JK_AA))+(NP*B23(JK_AA))
                  N1=(LP*B31(JK_AA))+(MP*B32(JK_AA))+(NP*B33(JK_AA))
                  XP=X1
                  YP=Y1
                  ZP=Z1
                  LP=L1
                  MP=M1
                  NP=N1
              END IF
              IF(A.NE.0.0D0) THEN
                  JK_AA=-A*(PII/180.0D0)
                  X1=(XP*A11(JK_AA))+(YP*A12(JK_AA))+(ZP*A13(JK_AA))
                  Y1=(XP*A21(JK_AA))+(YP*A22(JK_AA))+(ZP*A23(JK_AA))
                  Z1=(XP*A31(JK_AA))+(YP*A32(JK_AA))+(ZP*A33(JK_AA))
                  L1=(LP*A11(JK_AA))+(MP*A12(JK_AA))+(NP*A13(JK_AA))
                  M1=(LP*A21(JK_AA))+(MP*A22(JK_AA))+(NP*A23(JK_AA))
                  N1=(LP*A31(JK_AA))+(MP*A32(JK_AA))+(NP*A33(JK_AA))
                  XP=X1
                  YP=Y1
                  ZP=Z1
                  LP=L1
                  MP=M1
                  NP=N1
              END IF
              XP=XP+DX
              YP=YP+DY
              ZP=ZP+DZ

              GX=XP
              GY=YP
              GZ=ZP
              GL=LP
              GM=MP
              GN=NP
          END IF
          RETURN
      END
C SUB NSSORINT.FOR
      SUBROUTINE NSSORINT(V1,V2)
          USE NSSMOD
C
          IMPLICIT NONE
C
C       THIS ROUTINE DOES THE NSSORINT COMMAND
C
!        INTEGER I
C
          REAL*8 LVAL,MVAL,NVAL,MAG,SINA,COSA,SINF,COSF
          REAL*8 V1,V2
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
C
C     MAKE SURE YOU HAVE VERTEX DATA WITHOUT OFFSETS AND SET TO
C     THE FIRST SURFACE WITHOUT AN INFINITE THICKNESS
          CALL NSS_VERTEX_SILENT
C     VERTEX DATA EXISTS
C     NOW GET THE DIRECTION COSINES OF THE LOCAL Z-AXIS OF SURFACE
C     DESIGNATED BY V1
          LVAL=NSSVERTEX(10,INT(V1))
          MVAL=NSSVERTEX(11,INT(V1))
          NVAL=NSSVERTEX(12,INT(V1))
          IF(V2.NE.1) THEN
              LVAL=-LVAL
              MVAL=-MVAL
              NVAL=-NVAL
          END IF
          MAG=DSQRT((LVAL**2)+(MVAL**2)+(NVAL**2))
          LOOKX=LVAL/MAG
          LOOKY=MVAL/MAG
          LOOKZ=NVAL/MAG
C       CALCULATE VIEALF AND VIEPHI
          SINA=LOOKY
          COSA=DSQRT((LOOKX**2)+(LOOKZ**2))
          IF(DABS(SINA).LE.1.0D-15.AND.DABS(COSA).LE.1.0D-15) THEN
              VIEALF=0.0D0
          ELSE
              VIEALF=DATAN2(SINA,COSA)
          END IF
          VIEALF=(180.0D0/PII)*VIEALF
          IF(VIEALF.GT.0.0D0) THEN
              IF(VIEALF.GT.90.0D0) VIEALF=180.0D0-VIEALF
              GO TO 1000
          ELSE
          END IF
          IF(VIEALF.LT.0.0D0) THEN
              IF(VIEALF.LT.-90.0D0) VIEALF=-180.0D0-VIEALF
          ELSE
          END IF
 1000     CONTINUE
          IF(DABS(COSA).GT.1.0D-10) THEN
              SINF=LOOKX/COSA
              COSF=LOOKZ/COSA
              IF(DABS(SINF).LE.1.0D-15.AND.DABS(COSF).LE.1.0D-15) THEN
                  VIEPHI=0.0D0
              ELSE
                  VIEPHI=DATAN2(SINF,COSF)
              END IF
              VIEPHI=(180.0D0/PII)*VIEPHI
              IF(VIEPHI.LT.0.0D0) VIEPHI=360.0D0+VIEPHI
          ELSE
              VIEPHI=270.0D0
          END IF
          RETURN
      END
      SUBROUTINE NSS_FNDTRANSF(J)
          USE NSSMOD
C
C       THIS ROUTINE CONVERTS GLOBAL RAY DATA INTO THE COORDINATE SYSTEM OF
C       SURFACE J FOR THE SURFACE SEARCH
C
C       THIS OPERATES ON RAY DATA BEFORE A SURFACE INTERACTION
C
C
          IMPLICIT NONE
C
          REAL*8 JK_AA,A11,A12,A13,A21,A22,A23,A31,A32,A33
     1    ,B11,B12,B13,B21,B22,B23,B31,B32,B33
     2    ,C11,C12,C13,C21,C22,C23,C31,C32,C33,
     3    A,B,C,DX,DY,DZ,AN,X1,Y1,Z1,L1,M1,N1
          REAL*8 X,Y,Z,L,M,N
          REAL*8 XP,YP,ZP,LP,MP,NP
C
          INTEGER J,I
C
          INCLUDE 'datmai.inc'
C
C       ROTATION MATRICIES FUNCTIONS
C
C       A11
          A11(AN)=1.0D0
C       A12
          A12(AN)=0.0D0
C       A13
          A13(AN)=0.0D0
C       A21
          A21(AN)=0.0D0
C       A22
          A22(AN)=DCOS(AN)
C       A23
          A23(AN)=-DSIN(AN)
C       A31
          A31(AN)=0.0D0
C       A32
          A32(AN)=DSIN(AN)
C       A33
          A33(AN)=DCOS(AN)
C       B11
          B11(AN)=DCOS(AN)
C       B12
          B12(AN)=0.0D0
C       B13
          B13(AN)=DSIN(AN)
C       B21
          B21(AN)=0.0D0
C       B22
          B22(AN)=1.0D0
C       B23
          B23(AN)=0.0D0
C       B31
          B31(AN)=-DSIN(AN)
C       B32
          B32(AN)=0.0D0
C       B33
          B33(AN)=DCOS(AN)
C       C11
          C11(AN)=DCOS(AN)
C       C12
          C12(AN)=DSIN(AN)
C       C13
          C13(AN)=0.0D0
C       C21
          C21(AN)=-DSIN(AN)
C       C22
          C22(AN)=DCOS(AN)
C       C23
          C23(AN)=0.0D0
C       C31
          C31(AN)=0.0D0
C       C32
          C32(AN)=0.0D0
C       C33
          C33(AN)=1.0D0
C
C NEG SIGNS ARE USED BECAUSE THE TRANSFORMS WERE ORIGINALLY DESIGNED
C       FOR CODE-V ANGLE SIGN CONVENTIONS FOR ALPHA AND BETA WHICH ARE
C       LEFT HAND AND NOT RIGHT HAND POSITIVE
C
C       IS TRANSFORMATION REFERENCED TO ANOTHER SURFACE
C       INSTEAD OF TO THE GLOBAL ORIGIN
          X=GRRX
          Y=GRRY
          Z=GRRZ
          L=GRRL
          M=GRRM
          N=GRRN
          NSSNEST(0:200)=-1
          INEST=0
          IF(NSSALENS(37,J).NE.-1.0D0) THEN
              NSSNEST(1)=INT(NSSALENS(37,J))
              INEST=1
              DO I=1,199
                  IF(NSSALENS(37,NSSNEST(I)).NE.-1.0D0) THEN
                      NSSNEST(I+1)=INT(NSSALENS(37,NSSNEST(I)))
                      INEST=I+1
                  ELSE
                      EXIT
                  END IF
              END DO
          END IF
          IF(NSSALENS(37,J).NE.-1.0D0) THEN
C       NOW PERFORM THE REFERENCED
C       WE HAVE A VALID SURFACE TO TRANSFORM
              A=-NSSALENS(40,INT(NSSALENS(37,J)))
              B=-NSSALENS(41,INT(NSSALENS(37,J)))
              C= NSSALENS(42,INT(NSSALENS(37,J)))
              DX=NSSALENS(34,INT(NSSALENS(37,J)))
              DY=NSSALENS(35,INT(NSSALENS(37,J)))
              DZ=NSSALENS(36,INT(NSSALENS(37,J)))
C
              XP=X-DX
              YP=Y-DY
              ZP=Z-DZ
              LP=L
              MP=M
              NP=N
C
C       NOW ROTATE THE SUCKER!
              IF(A.NE.0.0D0) THEN
                  JK_AA=A*(PII/180.0D0)
                  X1=(XP*A11(JK_AA))+(YP*A12(JK_AA))+(ZP*A13(JK_AA))
                  Y1=(XP*A21(JK_AA))+(YP*A22(JK_AA))+(ZP*A23(JK_AA))
                  Z1=(XP*A31(JK_AA))+(YP*A32(JK_AA))+(ZP*A33(JK_AA))
                  XP=X1
                  YP=Y1
                  ZP=Z1
                  L1=(LP*A11(JK_AA))+(MP*A12(JK_AA))+(NP*A13(JK_AA))
                  M1=(LP*A21(JK_AA))+(MP*A22(JK_AA))+(NP*A23(JK_AA))
                  N1=(LP*A31(JK_AA))+(MP*A32(JK_AA))+(NP*A33(JK_AA))
                  LP=L1
                  MP=M1
                  NP=N1
              END IF
              IF(B.NE.0.0D0) THEN
                  JK_AA=B*(PII/180.0D0)
                  X1=(XP*B11(JK_AA))+(YP*B12(JK_AA))+(ZP*B13(JK_AA))
                  Y1=(XP*B21(JK_AA))+(YP*B22(JK_AA))+(ZP*B23(JK_AA))
                  Z1=(XP*B31(JK_AA))+(YP*B32(JK_AA))+(ZP*B33(JK_AA))
                  XP=X1
                  YP=Y1
                  ZP=Z1
                  L1=(LP*B11(JK_AA))+(MP*B12(JK_AA))+(NP*B13(JK_AA))
                  M1=(LP*B21(JK_AA))+(MP*B22(JK_AA))+(NP*B23(JK_AA))
                  N1=(LP*B31(JK_AA))+(MP*B32(JK_AA))+(NP*B33(JK_AA))
                  LP=L1
                  MP=M1
                  NP=N1
              END IF
              IF(C.NE.0.0D0) THEN
                  JK_AA=C*(PII/180.0D0)
                  X1=(XP*C11(JK_AA))+(YP*C12(JK_AA))+(ZP*C13(JK_AA))
                  Y1=(XP*C21(JK_AA))+(YP*C22(JK_AA))+(ZP*C23(JK_AA))
                  Z1=(XP*C31(JK_AA))+(YP*C32(JK_AA))+(ZP*C33(JK_AA))
                  XP=X1
                  YP=Y1
                  ZP=Z1
                  L1=(LP*C11(JK_AA))+(MP*C12(JK_AA))+(NP*C13(JK_AA))
                  M1=(LP*C21(JK_AA))+(MP*C22(JK_AA))+(NP*C23(JK_AA))
                  N1=(LP*C31(JK_AA))+(MP*C32(JK_AA))+(NP*C33(JK_AA))
                  LP=L1
                  MP=M1
                  NP=N1
              END IF
              X=XP
              Y=YP
              Z=ZP
              L=LP
              M=MP
              N=NP
          END IF
C       NOW PERFORM THE TRANSFORMATION ATTACHED TO THIS SURFACE
C       WE HAVE A VALID SURFACE TO TRANSFORM
C
          A=-NSSALENS(40,J)
          B=-NSSALENS(41,J)
          C= NSSALENS(42,J)
          DX=NSSALENS(34,J)
          DY=NSSALENS(35,J)
          DZ=NSSALENS(36,J)
C
          XP=X-DX
          YP=Y-DY
          ZP=Z-DZ
          LP=L
          MP=M
          NP=N
C
C       NOW ROTATE THE SUCKER!
          IF(A.NE.0.0D0) THEN
              JK_AA=A*(PII/180.0D0)
              X1=(XP*A11(JK_AA))+(YP*A12(JK_AA))+(ZP*A13(JK_AA))
              Y1=(XP*A21(JK_AA))+(YP*A22(JK_AA))+(ZP*A23(JK_AA))
              Z1=(XP*A31(JK_AA))+(YP*A32(JK_AA))+(ZP*A33(JK_AA))
              XP=X1
              YP=Y1
              ZP=Z1
              L1=(LP*A11(JK_AA))+(MP*A12(JK_AA))+(NP*A13(JK_AA))
              M1=(LP*A21(JK_AA))+(MP*A22(JK_AA))+(NP*A23(JK_AA))
              N1=(LP*A31(JK_AA))+(MP*A32(JK_AA))+(NP*A33(JK_AA))
              LP=L1
              MP=M1
              NP=N1
          END IF
          IF(B.NE.0.0D0) THEN
              JK_AA=B*(PII/180.0D0)
              X1=(XP*B11(JK_AA))+(YP*B12(JK_AA))+(ZP*B13(JK_AA))
              Y1=(XP*B21(JK_AA))+(YP*B22(JK_AA))+(ZP*B23(JK_AA))
              Z1=(XP*B31(JK_AA))+(YP*B32(JK_AA))+(ZP*B33(JK_AA))
              XP=X1
              YP=Y1
              ZP=Z1
              L1=(LP*B11(JK_AA))+(MP*B12(JK_AA))+(NP*B13(JK_AA))
              M1=(LP*B21(JK_AA))+(MP*B22(JK_AA))+(NP*B23(JK_AA))
              N1=(LP*B31(JK_AA))+(MP*B32(JK_AA))+(NP*B33(JK_AA))
              LP=L1
              MP=M1
              NP=N1
          END IF
          IF(C.NE.0.0D0) THEN
              JK_AA=C*(PII/180.0D0)
              X1=(XP*C11(JK_AA))+(YP*C12(JK_AA))+(ZP*C13(JK_AA))
              Y1=(XP*C21(JK_AA))+(YP*C22(JK_AA))+(ZP*C23(JK_AA))
              Z1=(XP*C31(JK_AA))+(YP*C32(JK_AA))+(ZP*C33(JK_AA))
              XP=X1
              YP=Y1
              ZP=Z1
              L1=(LP*C11(JK_AA))+(MP*C12(JK_AA))+(NP*C13(JK_AA))
              M1=(LP*C21(JK_AA))+(MP*C22(JK_AA))+(NP*C23(JK_AA))
              N1=(LP*C31(JK_AA))+(MP*C32(JK_AA))+(NP*C33(JK_AA))
              LP=L1
              MP=M1
              NP=N1
          END IF
          LRRX=XP
          LRRY=YP
          LRRZ=ZP
          LRRL=LP
          LRRM=MP
          LRRN=NP
          RETURN
      END
      SUBROUTINE OXOYRXRY_LOCAL_TO_GLOBAL(OX,OY,RX,RY,GOX,GOY,GRX,GRY)
          USE NSSMOD
C
C       THIS ROUTINE CONVERTS LOCAL STARTING OBJECT AND REFERENCE X AND Y COORDINATES
C       INTO GLOBAL DATA
C
C       THIS OPERATES ON RAY DATA BEFORE A SURFACE INTERACTION
C
C
          IMPLICIT NONE
C
          REAL*8 JK_AA,A11,A12,A13,A21,A22,A23,A31,A32,A33
     1    ,B11,B12,B13,B21,B22,B23,B31,B32,B33
     2    ,C11,C12,C13,C21,C22,C23,C31,C32,C33,
     3    A,B,AN,X1,Y1,L1,M1,OX,OY,RX,RY,GOX,GOY,GRX,GRY
          REAL*8 X,Y,L,M
          REAL*8 XP,YP,LP,MP
C
!      INTEGER J,I
C
          INCLUDE 'datmai.inc'
C
C       ROTATION MATRICIES FUNCTIONS
C
C       A11
          A11(AN)=1.0D0
C       A12
          A12(AN)=0.0D0
C       A13
          A13(AN)=0.0D0
C       A21
          A21(AN)=0.0D0
C       A22
          A22(AN)=DCOS(AN)
C       A23
          A23(AN)=-DSIN(AN)
C       A31
          A31(AN)=0.0D0
C       A32
          A32(AN)=DSIN(AN)
C       A33
          A33(AN)=DCOS(AN)
C       B11
          B11(AN)=DCOS(AN)
C       B12
          B12(AN)=0.0D0
C       B13
          B13(AN)=DSIN(AN)
C       B21
          B21(AN)=0.0D0
C       B22
          B22(AN)=1.0D0
C       B23
          B23(AN)=0.0D0
C       B31
          B31(AN)=-DSIN(AN)
C       B32
          B32(AN)=0.0D0
C       B33
          B33(AN)=DCOS(AN)
C       C11
          C11(AN)=DCOS(AN)
C       C12
          C12(AN)=DSIN(AN)
C       C13
          C13(AN)=0.0D0
C       C21
          C21(AN)=-DSIN(AN)
C       C22
          C22(AN)=DCOS(AN)
C       C23
          C23(AN)=0.0D0
C       C31
          C31(AN)=0.0D0
C       C32
          C32(AN)=0.0D0
C       C33
          C33(AN)=1.0D0
C
C NEG SIGNS ARE USED BECAUSE THE TRANSFORMS WERE ORIGINALLY DESIGNED
C       FOR CODE-V ANGLE SIGN CONVENTIONS FOR ALPHA AND BETA WHICH ARE
C       LEFT HAND AND NOT RIGHT HAND POSITIVE
C       NOW PERFORM THE TRANSFORMATION ATTACHED TO THIS SURFACE
C       WE HAVE A VALID SURFACE TO TRANSFORM
C
          A=-OALPHA
          B=-OBETA
          X=OX
          Y=OY
          L=RX
          M=RY
C
C     RESOLVE TILT AND DECENTER
C
          XP=X
          YP=Y
          LP=L
          MP=M
C
C       NOW ROTATE THE SUCKER!
          IF(B.NE.0.0D0) THEN
              JK_AA=-B*(PII/180.0D0)
              X1=(XP*B11(JK_AA))+(YP*B12(JK_AA))
              Y1=(XP*B21(JK_AA))+(YP*B22(JK_AA))
              XP=X1
              YP=Y1
              L1=(LP*B11(JK_AA))+(MP*B12(JK_AA))
              M1=(LP*B21(JK_AA))+(MP*B22(JK_AA))
              LP=L1
              MP=M1
          END IF
          IF(A.NE.0.0D0) THEN
              JK_AA=-A*(PII/180.0D0)
              X1=(XP*A11(JK_AA))+(YP*A12(JK_AA))
              Y1=(XP*A21(JK_AA))+(YP*A22(JK_AA))
              XP=X1
              YP=Y1
              L1=(LP*A11(JK_AA))+(MP*A12(JK_AA))
              M1=(LP*A21(JK_AA))+(MP*A22(JK_AA))
              LP=L1
              MP=M1
          END IF
          GOX=XP
          GOY=YP
          GRX=LP
          GRY=MP
          RETURN
      END
