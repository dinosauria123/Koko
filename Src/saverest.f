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

      FUNCTION SAVEINPT(I)
C
          IMPLICIT NONE
C
C     THIS ROUTINE SAVES THE EXISTING INPUT LINE
C     SO THAT A NEW LINE MAY BE CREATED AND SENT DIRECTLY TO THE
C     PROGRAM COMMAND PROCESSOR IN PROCES.FOR
C
C
C     THE ROUTINE RESTIO.FOR IS THE INVERSE OF THIS ROUTINE
C     USED TO RESTORE ALL AS IT WAS BEFORE ENTERING THE TWILIGHT ZONE
C
          LOGICAL SAVEINPT
          LOGICAL SAVE_KDP(1:200)
          COMMON/KDPSAVE/SAVE_KDP
          INTEGER KDPN,I
          PARAMETER (KDPN=200)
C
          CHARACTER WC*8,WQ*8,WS*80
C
          COMMON/CNTLCH/WC,WQ,WS
C
          INTEGER SB1,SB2,SC1,SC2,SQ,SST,S1,S2,S3,S4,S5,
     1    DF1,DF2,DF3,DF4,DF5,SN,STI
C
          REAL*8 W1,W2,W3,W4,W5
C
          COMMON/CNTLNM/W1,W2,W3,W4,W5,SB1,SB2,SC1,SC2,SQ,SST,
     1    S1,S2,S3,S4,S5,DF1,DF2,DF3,DF4,DF5,SN
C
          COMMON/KUERRY/STI
C     PREVIOUS STUFF(USED TO REMEMBER PREVIOUS VALUES IN SPECIAL CASES)
C
          CHARACTER KDP_WC(0:KDPN)*8,KDP_WQ(0:KDPN)*8,KDP_WS(0:KDPN)*80
C
          COMMON/KDP_CNTLCH/KDP_WC,KDP_WQ,KDP_WS
C
          INTEGER KDP_SB1(0:KDPN),KDP_SB2(0:KDPN),KDP_SC1(0:KDPN)
     1    ,KDP_SC2(0:KDPN),KDP_SQ(0:KDPN),KDP_SST(KDPN),KDP_S1(0:KDPN),
     2    KDP_S2(0:KDPN),
     3    KDP_S3(0:KDPN),KDP_S4(0:KDPN),KDP_S5(KDPN),
     4    KDP_DF1(0:KDPN),KDP_DF2(0:KDPN),KDP_DF3(0:KDPN),KDP_DF4(0:KDPN),
     5    KDP_DF5(0:KDPN),KDP_SN(0:KDPN),KDP_STI(0:KDPN)
C
          REAL*8 KDP_W1(0:KDPN),KDP_W2(0:KDPN),KDP_W3(0:KDPN),
     1    KDP_W4(0:KDPN),KDP_W5(0:KDPN)
C
          COMMON/KDP_CNTLNM/KDP_W1,KDP_W2,KDP_W3,KDP_W4,KDP_W5,KDP_SB1
     1    ,KDP_SB2,KDP_SC1,
     2    KDP_SC2,KDP_SQ,KDP_SST,
     3    KDP_S1,KDP_S2,KDP_S3,KDP_S4,KDP_S5,KDP_DF1,KDP_DF2,KDP_DF3,
     4    KDP_DF4,KDP_DF5,KDP_SN
C
          COMMON/KDP_QUERRY/KDP_STI
C
          KDP_WC(I)=WC
          KDP_WQ(I)=WQ
          KDP_WS(I)=WS
          KDP_SB1(I)=SB1
          KDP_SB2(I)=SB2
          KDP_SC1(I)=SC1
          KDP_SC2(I)=SC2
          KDP_SQ(I)=SQ
          KDP_SST(I)=SST
          KDP_S1(I)=S1
          KDP_S2(I)=S2
          KDP_S3(I)=S3
          KDP_S4(I)=S4
          KDP_S5(I)=S5
          KDP_SN(I)=SN
          KDP_DF1(I)=DF1
          KDP_DF2(I)=DF2
          KDP_DF3(I)=DF3
          KDP_DF4(I)=DF4
          KDP_DF5(I)=DF5
          KDP_W1(I)=W1
          KDP_W2(I)=W2
          KDP_W3(I)=W3
          KDP_W4(I)=W4
          KDP_W5(I)=W5
          KDP_STI(I)=STI
          SAVEINPT=.TRUE.
          RETURN
      END
      FUNCTION RESTINPT(I)
C
          IMPLICIT NONE
C
C     THIS ROUTINE RESTORES THE EXISTING INPUT LINE
C     SO THAT A NEW LINE MAY BE CREATED AND SENT DIRECTLY TO THE
C     PROGRAM COMMAND PROCESSOR IN PROCES.FOR
C
C
C     THE ROUTINE RESTIO.FOR IS THE INVERSE OF THIS ROUTINE
C     USED TO RESTORE ALL AS IT WAS BEFORE ENTERING THE TWILIGHT ZONE
C
          LOGICAL RESTINPT
          INTEGER KDPN,I
          PARAMETER (KDPN=200)
          LOGICAL REST_KDP(1:200)
          COMMON/KDPREST/REST_KDP
C
          CHARACTER WC*8,WQ*8,WS*80
C
          COMMON/CNTLCH/WC,WQ,WS
C
          INTEGER SB1,SB2,SC1,SC2,SQ,SST,S1,S2,S3,S4,S5,
     1    DF1,DF2,DF3,DF4,DF5,SN,STI
C
          REAL*8 W1,W2,W3,W4,W5
C
          COMMON/CNTLNM/W1,W2,W3,W4,W5,SB1,SB2,SC1,SC2,SQ,SST,
     1    S1,S2,S3,S4,S5,DF1,DF2,DF3,DF4,DF5,SN
C
          COMMON/KUERRY/STI
C     PREVIOUS STUFF(USED TO REMEMBER PREVIOUS VALUES IN SPECIAL CASES)
C
          CHARACTER KDP_WC(0:KDPN)*8,KDP_WQ(0:KDPN)*8,KDP_WS(0:KDPN)*80
C
          COMMON/KDP_CNTLCH/KDP_WC,KDP_WQ,KDP_WS
C
          INTEGER KDP_SB1(0:KDPN),KDP_SB2(0:KDPN),KDP_SC1(0:KDPN)
     1    ,KDP_SC2(0:KDPN),KDP_SQ(0:KDPN),KDP_SST(KDPN),KDP_S1(0:KDPN),
     2    KDP_S2(0:KDPN),
     3    KDP_S3(0:KDPN),KDP_S4(0:KDPN),KDP_S5(KDPN),
     4    KDP_DF1(0:KDPN),KDP_DF2(0:KDPN),KDP_DF3(0:KDPN),KDP_DF4(0:KDPN),
     5    KDP_DF5(0:KDPN),KDP_SN(0:KDPN),KDP_STI(0:KDPN)
C
          REAL*8 KDP_W1(0:KDPN),KDP_W2(0:KDPN),KDP_W3(0:KDPN),
     1    KDP_W4(0:KDPN),KDP_W5(0:KDPN)
C
          COMMON/KDP_CNTLNM/KDP_W1,KDP_W2,KDP_W3,KDP_W4,KDP_W5,KDP_SB1
     1    ,KDP_SB2,KDP_SC1,
     2    KDP_SC2,KDP_SQ,KDP_SST,
     3    KDP_S1,KDP_S2,KDP_S3,KDP_S4,KDP_S5,KDP_DF1,KDP_DF2,KDP_DF3,
     4    KDP_DF4,KDP_DF5,KDP_SN
C
          COMMON/KDP_QUERRY/KDP_STI
C
          WC=KDP_WC(I)
          WQ=KDP_WQ(I)
          WS=KDP_WS(I)
          SB1=KDP_SB1(I)
          SB2=KDP_SB2(I)
          SC1=KDP_SC1(I)
          SC2=KDP_SC2(I)
          SQ=KDP_SQ(I)
          SST=KDP_SST(I)
          S1=KDP_S1(I)
          S2=KDP_S2(I)
          S3=KDP_S3(I)
          S4=KDP_S4(I)
          S5=KDP_S5(I)
          SN=KDP_SN(I)
          DF1=KDP_DF1(I)
          DF2=KDP_DF2(I)
          DF3=KDP_DF3(I)
          DF4=KDP_DF4(I)
          DF5=KDP_DF5(I)
          W1=KDP_W1(I)
          W2=KDP_W2(I)
          W3=KDP_W3(I)
          W4=KDP_W4(I)
          W5=KDP_W5(I)
          STI=KDP_STI(I)
          RESTINPT=.TRUE.
          RETURN
      END
