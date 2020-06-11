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

C       FIRST GROUP OF SPECIAL SURFACE AND FITTING FILES

C SUB SPSUP.FOR
      SUBROUTINE SPSUP
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE SPSUP. THIS IS THE SUBROUTINE WHICH
C       INITIALLY STARTS THE SETUP OF SPSRF UPDATE FOR SPECIAL
C       SURFACE DEFINITION.
C       THE CMD LEVEL IS DISABLED AND FLAG F8 IS SET TO 1.
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
C
          IF(SST.EQ.1.OR.SN.EQ.1) THEN
              OUTLYNE='"UPDATE SPSRF" TAKES NO EXPLICIT INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          F1=0
          F8=1
C       INITIALIZE REFERENCE RAY LOGICAL
C
          IF(RAYCLEAR) THEN
              REFEXT=.FALSE.
              SPDEXT=.FALSE.
              GSPDEXT=.FALSE.
              FOBYES=.FALSE.
              NULL=.FALSE.
              RAYEXT=.FALSE.
              POLEXT=.FALSE.
              FAIL=.TRUE.
          END IF
C
C       IF F15=1 LEAVE F12 AS IT IS, IF F15=0 AND
C       F12 NOT 1, SET F12 TO 1
          IF(F15.EQ.0.AND.F12.NE.1) THEN
              F12=1
C       DON'T DO ANYTHING
          END IF
C
C
C       SPSRF TAG ON A SURFACE IS AT ALENS(34,SURF)
C       IT IS 0.0 FOR NO SPSRF DATA AND EQUAL TO THE
C       FLOATING POINT REP. OF THE SURFACE TYPE FOR
C       SPSSRF DATA PRESENT. (I.E. TYPE 9 YIELDS
C       ALENS(34,SURF)=9.0 IF VALUE IS POSITIVE,
C       SPSRF IS ON, IF NEGATIVE IT IS OFF.
C
C
C       ANY OTHER ACTION TO BE TAKEN DURING SPSRF UPDATE IS HANDLED FROM
C       WITHIN SUBROUTINE SPSUP2.
C
          RETURN
      END
C SUB SPSRF2.FOR
      SUBROUTINE SPSRF2()
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE SPSRF2. THIS IS THE SUBROUTINE WHICH
C       HANDLES THE SPSRF  INPUT AND UPDATE INPUT AFTER INITIALIZATION BY
C       SUBROUTINES SPSUP AND SPSIN.
C
!      INTEGER ITP
C     ITP 1 FOR INPUT AND 2 FOR UPDATE
C
          LOGICAL CEE
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
C
          IF(WC.EQ.'M'.OR.WC.EQ.'C') THEN
              CALL MESCOM
              RETURN
          ELSE
          END IF
C
C       FIRST HANDLE EOS (END OF SUBFILE) IN THIS CASE THE SPSRF
C       SUBFILE.
C
          IF(WC.EQ.'EOS') THEN
              IF(SQ.EQ.1.OR.SST.EQ.1.OR.SN.EQ.1) THEN
                  OUTLYNE='"EOS" TAKES NO EXPLICIT INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  RETURN
              END IF
C
              CALL SPSEOS
              RETURN
          ELSE
          END IF
C       DON'T TERMINATE THE INPUT ROUTINE. ACCEPT INPUT DATA
C       AT THE DESIGNATED SURFACE.
C       THE OBJECT SURFACE WILL BE THE DEFAULT.
C       HERE IS WHERE THE BRANCHING TO SUBROUTINES WHICH HANDLE
C       INPUT TO SPSRF.
C
C
C       FILE LENSSTORE.DOC CONTAINS INFORMATION ON THE
C       SPECIFIC STORAGE LOCATIONS FOR LENS DATA ITEMS.
C
C       COMMAND (SPDEL) AT 'SPSRF' LEVEL
          IF(WC.EQ.'SPDEL') THEN
              CALL SPDEL
              RETURN
          ELSE
          END IF
C       COMMANDS (C1 THROUGH C96)
          CEE=.FALSE.
          IF(WC.EQ.'C1') CEE=.TRUE.
          IF(WC.EQ.'C2') CEE=.TRUE.
          IF(WC.EQ.'C3') CEE=.TRUE.
          IF(WC.EQ.'C4') CEE=.TRUE.
          IF(WC.EQ.'C5') CEE=.TRUE.
          IF(WC.EQ.'C6') CEE=.TRUE.
          IF(WC.EQ.'C7') CEE=.TRUE.
          IF(WC.EQ.'C8') CEE=.TRUE.
          IF(WC.EQ.'C9') CEE=.TRUE.
          IF(WC.EQ.'C10') CEE=.TRUE.
          IF(WC.EQ.'C11') CEE=.TRUE.
          IF(WC.EQ.'C12') CEE=.TRUE.
          IF(WC.EQ.'C13') CEE=.TRUE.
          IF(WC.EQ.'C14') CEE=.TRUE.
          IF(WC.EQ.'C15') CEE=.TRUE.
          IF(WC.EQ.'C16') CEE=.TRUE.
          IF(WC.EQ.'C17') CEE=.TRUE.
          IF(WC.EQ.'C18') CEE=.TRUE.
          IF(WC.EQ.'C19') CEE=.TRUE.
          IF(WC.EQ.'C20') CEE=.TRUE.
          IF(WC.EQ.'C21') CEE=.TRUE.
          IF(WC.EQ.'C22') CEE=.TRUE.
          IF(WC.EQ.'C23') CEE=.TRUE.
          IF(WC.EQ.'C24') CEE=.TRUE.
          IF(WC.EQ.'C25') CEE=.TRUE.
          IF(WC.EQ.'C26') CEE=.TRUE.
          IF(WC.EQ.'C27') CEE=.TRUE.
          IF(WC.EQ.'C28') CEE=.TRUE.
          IF(WC.EQ.'C29') CEE=.TRUE.
          IF(WC.EQ.'C30') CEE=.TRUE.
          IF(WC.EQ.'C31') CEE=.TRUE.
          IF(WC.EQ.'C32') CEE=.TRUE.
          IF(WC.EQ.'C33') CEE=.TRUE.
          IF(WC.EQ.'C34') CEE=.TRUE.
          IF(WC.EQ.'C35') CEE=.TRUE.
          IF(WC.EQ.'C36') CEE=.TRUE.
          IF(WC.EQ.'C37') CEE=.TRUE.
          IF(WC.EQ.'C38') CEE=.TRUE.
          IF(WC.EQ.'C39') CEE=.TRUE.
          IF(WC.EQ.'C40') CEE=.TRUE.
          IF(WC.EQ.'C41') CEE=.TRUE.
          IF(WC.EQ.'C42') CEE=.TRUE.
          IF(WC.EQ.'C43') CEE=.TRUE.
          IF(WC.EQ.'C44') CEE=.TRUE.
          IF(WC.EQ.'C45') CEE=.TRUE.
          IF(WC.EQ.'C46') CEE=.TRUE.
          IF(WC.EQ.'C47') CEE=.TRUE.
          IF(WC.EQ.'C48') CEE=.TRUE.
          IF(WC.EQ.'C49') CEE=.TRUE.
          IF(WC.EQ.'C50') CEE=.TRUE.
          IF(WC.EQ.'C51') CEE=.TRUE.
          IF(WC.EQ.'C52') CEE=.TRUE.
          IF(WC.EQ.'C53') CEE=.TRUE.
          IF(WC.EQ.'C54') CEE=.TRUE.
          IF(WC.EQ.'C55') CEE=.TRUE.
          IF(WC.EQ.'C56') CEE=.TRUE.
          IF(WC.EQ.'C57') CEE=.TRUE.
          IF(WC.EQ.'C58') CEE=.TRUE.
          IF(WC.EQ.'C59') CEE=.TRUE.
          IF(WC.EQ.'C60') CEE=.TRUE.
          IF(WC.EQ.'C61') CEE=.TRUE.
          IF(WC.EQ.'C62') CEE=.TRUE.
          IF(WC.EQ.'C63') CEE=.TRUE.
          IF(WC.EQ.'C64') CEE=.TRUE.
          IF(WC.EQ.'C65') CEE=.TRUE.
          IF(WC.EQ.'C66') CEE=.TRUE.
          IF(WC.EQ.'C67') CEE=.TRUE.
          IF(WC.EQ.'C68') CEE=.TRUE.
          IF(WC.EQ.'C69') CEE=.TRUE.
          IF(WC.EQ.'C70') CEE=.TRUE.
          IF(WC.EQ.'C71') CEE=.TRUE.
          IF(WC.EQ.'C72') CEE=.TRUE.
          IF(WC.EQ.'C73') CEE=.TRUE.
          IF(WC.EQ.'C74') CEE=.TRUE.
          IF(WC.EQ.'C75') CEE=.TRUE.
          IF(WC.EQ.'C76') CEE=.TRUE.
          IF(WC.EQ.'C77') CEE=.TRUE.
          IF(WC.EQ.'C78') CEE=.TRUE.
          IF(WC.EQ.'C79') CEE=.TRUE.
          IF(WC.EQ.'C80') CEE=.TRUE.
          IF(WC.EQ.'C81') CEE=.TRUE.
          IF(WC.EQ.'C82') CEE=.TRUE.
          IF(WC.EQ.'C83') CEE=.TRUE.
          IF(WC.EQ.'C84') CEE=.TRUE.
          IF(WC.EQ.'C85') CEE=.TRUE.
          IF(WC.EQ.'C86') CEE=.TRUE.
          IF(WC.EQ.'C87') CEE=.TRUE.
          IF(WC.EQ.'C88') CEE=.TRUE.
          IF(WC.EQ.'C89') CEE=.TRUE.
          IF(WC.EQ.'C90') CEE=.TRUE.
          IF(WC.EQ.'C91') CEE=.TRUE.
          IF(WC.EQ.'C92') CEE=.TRUE.
          IF(WC.EQ.'C93') CEE=.TRUE.
          IF(WC.EQ.'C94') CEE=.TRUE.
          IF(WC.EQ.'C95') CEE=.TRUE.
          IF(WC.EQ.'C96') CEE=.TRUE.
          IF(CEE) THEN
              CALL SPCOEF()
              RETURN
          ELSE
          END IF
C       COMMAND (SPECIAL OR GENL)
          IF(WC.EQ.'SPECIAL'.OR.WC.EQ.'GENL') THEN
              CALL SPSPEC()
              RETURN
          ELSE
          END IF
C       COMMAND (SPSRF ON AND SPSRF OFF)
          IF(WC.EQ.'SPSRF'.AND.WQ.EQ.'ON'.OR.
     1    WC.EQ.'SPSRF'.AND.WQ.EQ.'OFF') THEN
              CALL SPONOF
              RETURN
          ELSE
          END IF
          RETURN
      END
C SUB SPSTAT.FOR
      SUBROUTINE SPSTAT
C     USED FOR FIELD AVERAGED SPOT DIAGRAM ANALYSIS IN CONJUNCTION
C     WITH SPDSAVE OR SPDADD
C
          IMPLICIT NONE
C
          CHARACTER UN*11
C
          LOGICAL EXIS32,OPEN32
C
          INTEGER J,I,K,NUMT1,N,NUMT2,NUMT3,NUMT4,NUMT5
     6    ,NUMT6,NUMT7,NUMT8,NUMT9,NUMT10
C
          REAL*8 TOT,W,SCENTX,SCENTY,
     1    APFAC,DELTA,MSS,AMSS,MSSX,MSSY,AMSSX,AMSSY,
     2    JA,JB,DS9,DS12
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datspd.inc'
          INCLUDE 'datsp1.inc'
C
          JA=COS_A_ANG
          JB=COS_B_ANG
C
          IF(SYSTEM1(30).EQ.1.0D0.OR.SYSTEM1(30).EQ.2.0D0) THEN
C       MODE FOCAL
              IF(STI.EQ.1) THEN
                  OUTLYNE='"SPDSTATS" DOES STATISTICS ON SUMMED SPOTS'
                  CALL SHOWIT(1)
                  RETURN
              END IF
              IF(S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
                  OUTLYNE='"SPDSTATS" TAKES NO'
                  CALL SHOWIT(1)
                  OUTLYNE='NUMERIC WORD #2 THROUGH #5 INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C
              IF(DF1.EQ.1) W1=0.0D0
          ELSE
C       MODE AFOCAL OR UAFOCAL
              IF(SST.EQ.1) THEN
                  OUTLYNE='"SPDSTATS" TAKES NO STRING INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              DELTA=0.0D0
          END IF
C
          IF(SQ.EQ.0) WQ='SPOTS'
          DO I=8,1,-1
              IF(WQ(I:I).NE.' ') THEN
                  N=I
                  GO TO 10
              END IF
          END DO
 10       CONTINUE
C
C       STATS
          EXIS32=.FALSE.
          INQUIRE(FILE=LIBSPO//WQ(1:N)//'.DAT',EXIST=EXIS32)
          IF(.NOT.EXIS32) THEN
C       NO FILE EXISTS, ERROR AND STOP
              OUTLYNE='NO '//WQ(1:N)//'.DAT FILE EXISTS TO ANALYZE'
              CALL SHOWIT(1)
              OUTLYNE='NO ACTION TAKEN'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          SPDEXT=.TRUE.
C       SPOTS.DAT EXISTS, IF NOT OPEN, OPEN IT
          OPEN32=.FALSE.
          INQUIRE(FILE=LIBSPO//WQ(1:N)//'.DAT',OPENED=OPEN32)
          IF(.NOT.OPEN32) THEN
              OPEN(UNIT=32,ACCESS='DIRECT',FILE=LIBSPO//WQ(1:N)//'.DAT',
     1        FORM='UNFORMATTED',RECL=(110*NRECL),STATUS='UNKNOWN')
          ELSE
C       IT WAS OPENED, PROCEED
          END IF
C
          READ(UNIT=32,REC=1) K
C     DEALLOCATE DSPOTT
          CALL SPOTIT(1)
C     ALLOCATE DSPOTT
          NDSPOTT=K+10
          CALL SPOTIT(2)
C       K IS THE TOTAL NUMBER OF FILE ENTRIES
          TOT=0.0D0
          NUMTOT=0
          NUMT1=0
          NUMT2=0
          NUMT3=0
          NUMT4=0
          NUMT5=0
          NUMT6=0
          NUMT7=0
          NUMT8=0
          NUMT9=0
          NUMT10=0
          SPA=0.0D0
          SPB=0.0D0
          SPC=0.0D0
          SPD=0.0D0
          AFSPB=0.0D0
          AFSPD=0.0D0
          SCENTX=0.0D0
          SCENTY=0.0D0
          DO I=2,K
              READ (UNIT=32,REC=I)
     1        DSPOT(1),DSPOT(2),DSPOT(3),DSPOT(4),
     1        DSPOT(5),DSPOT(6),DSPOT(7),DSPOT(8),
     1        DSPOT(9),DSPOT(10),DSPOT(11),DSPOT(12),
     1        DSPOT(13),DSPOT(14),DSPOT(15),DSPOT(16),
     1        DSPOT(13),DSPOT(14),DSPOT(15),DSPOT(16),
     1        DSPOT(17),DSPOT(18),DSPOT(19),DSPOT(20),
     1        DSPOT(21),DSPOT(22),DSPOT(23),DSPOT(24)
     6        ,DSPOT(25),DSPOT(26),DSPOT(27),DSPOT(28),DSPOT(29)
     6        ,DSPOT(30),DSPOT(31),DSPOT(32),DSPOT(33),DSPOT(34),DSPOT(35)
     6        ,DSPOT(36),DSPOT(37),DSPOT(38),DSPOT(39),DSPOT(40),DSPOT(41)
     6        ,DSPOT(42),DSPOT(43),DSPOT(44),DSPOT(45),DSPOT(46),DSPOT(47)
     6        ,DSPOT(48),DSPOT(49),DSPOT(50)
              J=I-1
C     LOAD DSPOT(*) INTO DSPOTT(*,J)
              ID=J
              CALL SPOTIT(3)
              DS9=DSPOT(16)
              DS12=DSPOT(12)
C       DO FIRST SET OF STATISTICS
C       CHECK IF RAYCOD IS ZERO
C       NO RAYS ARE STORED IF SPTWT WAS ZERO SO DON'T CHECK IT AGAIN
              IF(DSPOT(7).EQ.0.0D0) THEN
C       RAY DID NOT FAIL
                  APFAC=DSPOT(11)
                  TOT=TOT+(DS12*APFAC)
C       NUMTOT IS NUMBER OF PASSED RAYS
                  NUMTOT=NUMTOT+1
C       AT EACH WAVELENGTH THE PASSED RAYS ARE:
                  IF(INT(DS9).EQ.1) NUMT1=NUMT1+1
                  IF(INT(DS9).EQ.2) NUMT2=NUMT2+1
                  IF(INT(DS9).EQ.3) NUMT3=NUMT3+1
                  IF(INT(DS9).EQ.4) NUMT4=NUMT4+1
                  IF(INT(DS9).EQ.5) NUMT5=NUMT5+1
                  IF(INT(DS9).EQ.6) NUMT6=NUMT6+1
                  IF(INT(DS9).EQ.7) NUMT7=NUMT7+1
                  IF(INT(DS9).EQ.8) NUMT8=NUMT8+1
                  IF(INT(DS9).EQ.9) NUMT9=NUMT9+1
                  IF(INT(DS9).EQ.10) NUMT10=NUMT10+1
C       NOW THE CENTROID
                  SPA=SPA+(DS12*DSPOT(1)*APFAC)
                  SPC=SPC+(DS12*DSPOT(2)*APFAC)
                  SPB=SPB+(DS12*DTAN(DSPOT(9))*APFAC)
                  SPD=SPD+(DS12*DTAN(DSPOT(10))*APFAC)
                  AFSPB=AFSPB+(DS12*(DSPOT(9))*APFAC)
                  AFSPD=AFSPD+(DS12*(DSPOT(10))*APFAC)
              ELSE
C       RAY FAILED
              END IF
          END DO
          W=TOT
          SPA=SPA/W
          SPC=SPC/W
          SPB=SPB/W
          SPD=SPD/W
          AFSPB=AFSPB/W
          AFSPD=AFSPD/W
          IF(SYSTEM1(30).EQ.1.0D0.OR.SYSTEM1(30).EQ.2.0D0) THEN
C       MODE FOCAL OR UFOCAL
              SCENTX=SPA
              SCENTY=SPC
          ELSE
C       MODE AFOCAL OR UAFOCAL
              SCENTX=AFSPB
              SCENTY=AFSPD
          END IF
          LA=0.0D0
          LC=0.0D0
          LB=0.0D0
          LD=0.0D0
          AFLB=0.0D0
          AFLD=0.0D0
          SSSP=0.0D0
          SSSQ=0.0D0
          SSSR=0.0D0
          ASSSP=0.0D0
          ASSSQ=0.0D0
          ASSSR=0.0D0
          SSSPX=0.0D0
          SSSQX=0.0D0
          SSSRX=0.0D0
          ASSSPX=0.0D0
          ASSSQX=0.0D0
          ASSSRX=0.0D0
          SSSPY=0.0D0
          SSSQY=0.0D0
          SSSRY=0.0D0
          ASSSPY=0.0D0
          ASSSQY=0.0D0
          ASSSRY=0.0D0
          DO I=1,K-1
C     LOAD DSPOT(*) INTO DSPOTT(*,I)
              ID=I
              CALL SPOTIT(3)
              DS9=DSPOT(16)
              DS12=DSPOT(12)
              IF(DSPOT(7).EQ.0.0D0) THEN
                  APFAC=DSPOT(11)
                  LA=DSPOT(1)
                  LC=DSPOT(2)
                  LB=DTAN(DSPOT(9))
                  LD=DTAN(DSPOT(10))
                  AFLB=DSPOT(9)
                  AFLD=DSPOT(10)

C     FOR FOCAL SYSTEMS
C     THE P TERM
                  SSSP=SSSP+
     1            (DS12*(((LA-SPA)**2)+((LC-SPC)**2))*APFAC)
C     THE Q TERM
                  SSSQ=SSSQ+
     1            (DS12*((LA*LB)-(LA*SPB)-(LB*SPA)
     2            +(SPA*SPB))*APFAC)+
     1            (DS12*((LC*LD)-(LC*SPD)-(LD*SPC)
     2            +(SPC*SPD))*APFAC)
C     THE R TERM
                  SSSR=SSSR+
     1            (DS12*(((LB-SPB)**2))*APFAC)+
     1            (DS12*(((LD-SPD)**2))*APFAC)
C
C     FOR AFOCAL SYSTEMS
C     THE P TERM
                  ASSSP=ASSSP+
     1            (DS12*(((AFLB-AFSPB)**2))*APFAC)+
     1            (DS12*(((AFLD-AFSPD)**2))*APFAC)
C     THE Q TERM   (IDENTICALLY ZERO)
                  ASSSQ=0.0D0
C     THE R TERM   (IDENTICALLY ZERO)
                  ASSSR=0.0D0
C
C       X ONLY
                  SSSPX=SSSPX+
     1            (DS12*(((LA-SPA)**2))*APFAC)
                  SSSQX=SSSQX+
     1            (DS12*((LA*LB)-(LA*SPB)-(LB*SPA)
     2            +(SPA*SPB))*APFAC)
                  SSSRX=SSSRX+
     1            (DS12*(((LB-SPB)**2))*APFAC)
C     AFOCAL
                  ASSSPX=ASSSPX+
     1            (DS12*(((AFLB-AFSPB)**2))*APFAC)
                  ASSSQX=0.0D0
                  ASSSRX=0.0D0
C       Y ONLY
                  SSSPY=SSSPY+
     1            (DS12*(((LC-SPC)**2))*APFAC)
                  SSSQY=SSSQY+
     1            (DS12*((LC*LD)
     2            -(LC*SPD)-(LD*SPC)+(SPC*SPD))*APFAC)
                  SSSRY=SSSRY+
     1            (DS12*(+((LD-SPD)**2))*APFAC)
C     AFOCAL
                  ASSSPY=ASSSPY+
     1            (DS12*(+((AFLD-AFSPD)**2))*APFAC)
                  ASSSQY=0.0D0
                  ASSSRY=0.0D0
              ELSE
C       RAY FAILED
              END IF
          END DO
          DELTA=W1
          MSS=SSSP+(2.0D0*DELTA*SSSQ)+((DELTA**2)*SSSR)
          AMSS=ASSSP
          MSSX=SSSPX+(2.0D0*DELTA*SSSQX)+((DELTA**2)*SSSRX)
          AMSSX=ASSSPX
          MSSY=SSSPY+(2.0D0*DELTA*SSSQY)+((DELTA**2)*SSSRY)
          AMSSY=ASSSPY
          IF(SYSTEM1(30).EQ.1.0D0.OR.SYSTEM1(30).EQ.2.0D0) THEN
C       FOCAL, DO FOCUS SHIFT CALCS
              IF(DABS(SSSRX).LT.1.0D-15.OR.DABS(SSSQX).GT.1.0D+15) THEN
                  IF(SSSRX.GE.0.0D0.AND.SSSQX.GE.0.0D0) FCSFTX=-1.0D35
                  IF(SSSRX.GE.0.0D0.AND.SSSQX.LE.0.0D0) FCSFTX=1.0D35
                  IF(SSSRX.LE.0.0D0.AND.SSSQX.GE.0.0D0) FCSFTX=1.0D35
              ELSE
                  FCSFTX=-SSSQX/SSSRX
              END IF
              IF(DABS(SSSRY).LT.1.0D-15.OR.DABS(SSSQY).GT.1.0D+15) THEN
                  IF(SSSRY.GE.0.0D0.AND.SSSQY.GE.0.0D0) FCSFTY=-1.0D35
                  IF(SSSRY.GE.0.0D0.AND.SSSQY.LE.0.0D0) FCSFTY=1.0D35
                  IF(SSSRY.LE.0.0D0.AND.SSSQY.GE.0.0D0) FCSFTY=1.0D35
                  FCSFTY=1.0D35
              ELSE
                  FCSFTY=-SSSQY/SSSRY
              END IF
              FCSFT=(FCSFTX+FCSFTY)/2.0D0
          ELSE
C       MODE AFOCAL, NO FOCUS SHIFTS
              FCSFT=0.0D0
              FCSFTY=0.0D0
              FCSFTX=0.0D0
          END IF
          IF(DABS(SCENTX).LT.1.0D-15) SCENTX=0.0D0
          IF(DABS(SCENTY).LT.1.0D-15) SCENTY=0.0D0
          IF(SYSTEM1(6).EQ.1.0) UN='INCH'
          IF(SYSTEM1(6).EQ.2.0) UN='CENTIMETER'
          IF(SYSTEM1(6).EQ.3.0) UN='MILLIMETER'
          IF(SYSTEM1(6).EQ.4.0) UN='METER'
          IF(SYSTEM1(30).EQ.1.0D0.OR.SYSTEM1(30).EQ.2.0D0) THEN
              RMSX=2.0D0*(DSQRT(MSSX/W))
              RMSY=2.0D0*(DSQRT(MSSY/W))
              RMS=(RMSX+RMSY)/2.0D0
          ELSE
              RMSX=2.0D0*(DSQRT(AMSSX/W))
              RMSY=2.0D0*(DSQRT(AMSSY/W))
              RMS=(RMSX+RMSY)/2.0D0
          END IF
          IF(SYSTEM1(30).LT.3.0D0) THEN
              RMSX=RMSX/JB
              RMSY=RMSY/JA
              RMS=(RMSX+RMSY)/2.0D0
          END IF
          REG(40)=REG(9)
          REG(11)=RMSY
          REG(10)=RMSX
          REG(9)=RMS
          CALL CLOSE_FILE(32,1)
 114      FORMAT('MULTIPLE FIELD SPOT DIAGRAM SUMMARY')
 214      FORMAT('MULTIPLE FIELD SPOT DIAGRAM SUMMARY')
 321      FORMAT('FOR A FOCUS SHIFT   = ',G17.10,1X,A11)
 115      FORMAT('RMS SPOT SIZE   (X) = ',G17.10,1X,A11)
 116      FORMAT('RMS SPOT SIZE   (Y) = ',G17.10,1X,A11)
 108      FORMAT('RMS SPOT DIAMETER   = ',G17.10,1X,A11)
 215      FORMAT('RMS SPOT SIZE   (X) = ',G17.10,1X,'RADIANS')
 216      FORMAT('RMS SPOT SIZE   (Y) = ',G17.10,1X,'RADIANS')
 208      FORMAT('RMS SPOT DIAMETER   = ',G17.10,1X,'RADIANS')
 101      FORMAT('X-CENTROID POSITION = ',G17.10,1X,A11)
 102      FORMAT('Y-CENTROID POSITION = ',G17.10,1X,A11)
 201      FORMAT('X-CENTROID (ANGLE)  = ',G17.10,1X,'RADIANS')
 202      FORMAT('Y-CENTROID (ANGLE)  = ',G17.10,1X,'RADIANS')
 105      FORMAT('# RAYS ATTEMPTED    = ',I10,' AT ALL WAVELENGTHS')
 106      FORMAT('# RAYS SUMMED       = ',I10,' AT ALL WAVELENGTHS')
 221      FORMAT(
     1    'FOCUS SHIFT FOR BEST RMS SPOT LENGTH (X) = ',G17.10,1X,A11)
 222      FORMAT(
     1    'FOCUS SHIFT FOR BEST RMS SPOT LENGTH (Y) = ',G17.10,1X,A11)
 220      FORMAT(
     1    'FOCUS SHIFT FOR BEST RMS SPOT DIAMETER   = ',G17.10,1X,A11)
C       DO THE PRINT OUT
          IF(SYSTEM1(30).EQ.1.0D0.OR.SYSTEM1(30).EQ.2.0D0) THEN
C       MODE FOCAL
              WRITE(OUTLYNE,114)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,321) W1,UN
              CALL SHOWIT(0)
              WRITE(OUTLYNE,108) RMS,UN
              CALL SHOWIT(0)
              WRITE(OUTLYNE,115) RMSX,UN
              CALL SHOWIT(0)
              WRITE(OUTLYNE,116) RMSY,UN
              CALL SHOWIT(0)
              WRITE(OUTLYNE,101) SCENTX,UN
              CALL SHOWIT(0)
              WRITE(OUTLYNE,102) SCENTY,UN
              CALL SHOWIT(0)
              WRITE(OUTLYNE,105) K-1
              CALL SHOWIT(0)
              WRITE(OUTLYNE,106) NUMTOT
              CALL SHOWIT(0)
              IF(W1.EQ.0.0D0) THEN
                  WRITE(OUTLYNE,220) FCSFT
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,221) FCSFTX
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,222) FCSFTY
                  CALL SHOWIT(0)
              ELSE
              END IF
          ELSE
C       MODE AFOCAL
              WRITE(OUTLYNE,214)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,208) RMS
              CALL SHOWIT(0)
              WRITE(OUTLYNE,215) RMSX
              CALL SHOWIT(0)
              WRITE(OUTLYNE,216) RMSY
              CALL SHOWIT(0)
              WRITE(OUTLYNE,201) SCENTX
              CALL SHOWIT(0)
              WRITE(OUTLYNE,202) SCENTY
              CALL SHOWIT(0)
              WRITE(OUTLYNE,105) K-1
              CALL SHOWIT(0)
              WRITE(OUTLYNE,106) NUMTOT
              CALL SHOWIT(0)
          END IF
C     DEALLOCATE DSPOTT
          CALL SPOTIT(1)
          RETURN
      END
C SUB SPSIN.FOR
      SUBROUTINE SPSIN
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE SPSIN. THIS IS THE SUBROUTINE WHICH
C       INITIALLY STARTS THE SETUP OF SPSRF INPUT FOR SPECIAL
C       SURFACE DEFINITION.
C       THE CMD LEVEL IS DISABLED AND FLAG F7 IS SET TO 1.
C
          INTEGER I
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
C
          IF(SST.EQ.1.OR.SN.EQ.1.OR.SQ.EQ.1) THEN
              OUTLYNE='"SPSRF" TAKES NO EXPLICIT INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          F1=0
          F7=1
C       INITIALIZE REFERENCE RAY LOGICAL
          IF(RAYCLEAR) THEN
              REFEXT=.FALSE.
              SPDEXT=.FALSE.
              GSPDEXT=.FALSE.
              CPFNEXT=.FALSE.
              CALL DELPSF
              FOBYES=.FALSE.
              NULL=.FALSE.
              RAYEXT=.FALSE.
              POLEXT=.FALSE.
              FAIL=.TRUE.
          END IF
C
C
C       SPSRF TAG ON A SURFACE IS AT ALENS(34,SURF)
C       IT IS 0.0 FOR NO SPSRF DATA AND EQUAL TO THE
C       FLOATING POINT REP. OF THE SURFACE TYPE FOR
C       SPSSRF DATA PRESENT. (I.E. TYPE 9 YIELDS
C       ALENS(34,SURF)=9.0 IF VALUE IS POSITIVE,
C       SPSRF IS ON, IF NEGATIVE IT IS OFF.
C
C       INITIALIZE ALL SPECIAL SURFACE DATA

C
          I=INT(SYSTEM1(20))
          ALENS(34,1:I)=0.0D0
C
C
C       ANY OTHER ACTION TO BE TAKEN DURING SPSRF INPUT IS HANDLED FROM
C       WITHIN SUBROUTINE SPSIN2.
C
          RETURN
      END
C SUB SPSPEC.FOR
      SUBROUTINE SPSPEC()
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE SPSPEC. THIS IS THE SUBROUTINE WHICH
C       HANDLES THE "SPECIAL" COMMAND FROM SPSRF OR UPDATE
C       SPSRF LEVEL
C
!        INTEGER ITP
          LOGICAL DM
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datsub.inc'
C
          IF(STI.NE.1) THEN
              IF(SST.EQ.1.OR.SQ.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1
     1        .OR.S5.EQ.1) THEN
                  OUTLYNE=
     1            '" SPECIAL " ONLY TAKES NUMERIC WORD #1 AND #2 INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER DATA'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(DF1.EQ.1.OR.DF2.EQ.1)THEN
                  OUTLYNE=
     1            '" SPECIAL " REQUIRES EXPLICIT NUMERIC WORD #1 AND #2 INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C
              IF(W1.EQ.0.0) THEN
                  OUTLYNE='SPECIAL SURFACE TYPE NOT VALID ON OBJECT SURFACE'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C
              IF(ALENS(103,INT(W1)).EQ.1.0D0) THEN
                  OUTLYNE=
     1            'A DEFORMABLE SURFACE MAY NOT BE DEFINED AS A SPECIAL SURFACE'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C     ARRAY LENS NOT ALLOWED
              IF(ALENS(133,INT(W1)).NE.0.0D0) THEN
                  OUTLYNE=
     1            'AN ARRAY SURFACE MAY NOT BE DEFINED AS A SPECIAL SURFACE'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          ELSE
C     STI=1
              OUTLYNE='"SPECIAL ?" HAS NO MEANING'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C       FLAG THE SPECIAL SURFACE TYPE
          IF(INT(DABS(W2)).GT.24) THEN
              OUTLYNE='SURFACE "TYPE" BEYOND CURRENTLY DEFINED BOUNDS'
              CALL SHOWIT(1)
              OUTLYNE=
     1        'CURRENTLY SUPPORTED SPECIAL SUFACE TYPES ARE 1 THROUGH 24'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C       SURFACE NUMBER CHECK
          IF(W1.LT.0.0D0) W1=SYSTEM1(20)+W1
          IF(INT((W1)).LT.1.OR.INT((W1)).GT.
     1    INT(SYSTEM1(20))) THEN
              OUTLYNE='SURFACE NUMBER BEYOND LEGAL RANGE'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C     CHECK FOR INVALID SURFACE TYPES FOR A TYPE 18 SURFACE
          IF(INT(DABS(W2)).EQ.18) THEN
              IF(ALENS(23,INT(W1)).NE.0.0D0.OR.ALENS(8,INT(W1)).NE.0.0D0
     1        .OR.ALENS(1,INT(W1)).EQ.0.0D0.OR.
     2        GLANAM(INT(W1),2).NE.'REFL         '
     3        .OR.GLANAM(INT(W1),1).NE.'             '.AND.
     4        GLANAM(INT(W1),2).EQ.'REFL         ') THEN
C     SURFACE NOT A SIMPLE MIRROR AND SPHERICAL OR CONIC
                  OUTLYNE='TYPE 18 SURFACE MUST BE A SIMPLE SPHERE OR CONIC'
                  CALL SHOWIT(1)
                  OUTLYNE='AND MUST ALSO BE A MIRROR WITH GLASS TYPE "REFL"'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
C     CHECK FOR INVALID SURFACE TYPES FOR A TYPE 6,7,9,10,11,15,20 SURFACE
          IF(INT(DABS(W2)).EQ.6.OR.
     1    INT(DABS(W2)).EQ.7.OR.
     1    INT(DABS(W2)).EQ.9.OR.
     1    INT(DABS(W2)).EQ.10.OR.
     1    INT(DABS(W2)).EQ.11.OR.
     1    INT(DABS(W2)).EQ.15.OR.
     1    INT(DABS(W2)).EQ.20) THEN
              DM=.FALSE.
              IF(GLANAM(INT(W1),1).EQ.GLANAM((INT(W1)-1),1).AND.
     1        GLANAM(INT(W1),2).EQ.GLANAM((INT(W1)-1),2).AND.
     2        GLANAM(INT(W1),2).NE.'REFL'.AND.
     2        GLANAM(INT(W1),2).NE.'REFLTIR'.AND.
     2        GLANAM(INT(W1),2).NE.'REFLTIRO'.AND.
     2        GLANAM(INT(W1),2).NE.'IDEAL'.AND.
     3        GLANAM(INT(W1),2).NE.'PERFECT      ') DM=.TRUE.
              IF(GLANAM(INT(W1),1).EQ.'MODEL        ') DM=.FALSE.
              IF(GLANAM(INT(W1)-1,1).EQ.'MODEL        ') DM=.FALSE.
              IF(ALENS(46,INT(W1)).EQ.ALENS(46,INT(W1)-1).AND.
     1           ALENS(47,INT(W1)).EQ.ALENS(47,INT(W1)-1).AND.
     1           ALENS(48,INT(W1)).EQ.ALENS(48,INT(W1)-1).AND.
     1           ALENS(49,INT(W1)).EQ.ALENS(49,INT(W1)-1).AND.
     1           ALENS(71,INT(W1)).EQ.ALENS(71,INT(W1)-1).AND.
     1           ALENS(72,INT(W1)).EQ.ALENS(72,INT(W1)-1).AND.
     1           ALENS(73,INT(W1)).EQ.ALENS(73,INT(W1)-1).AND.
     1           ALENS(74,INT(W1)).EQ.ALENS(74,INT(W1)-1).AND.
     1           ALENS(75,INT(W1)).EQ.ALENS(75,INT(W1)-1).AND.
     1           ALENS(50,INT(W1)).EQ.ALENS(50,INT(W1)-1)) DM=.TRUE.
              IF(GLANAM(INT(W1),1).EQ.'MODEL        '.AND.
     1        GLANAM(INT(W1)-1,1).EQ.'MODEL        '.AND.
     1        GLANAM(INT(W1)-1,2).EQ.GLANAM(INT(W1),2)) THEN
                  DM=.FALSE.
                  IF(ALENS(86,INT(W1)).EQ.ALENS(86,INT(W1)-1).AND.
     1               ALENS(87,INT(W1)).EQ.ALENS(87,INT(W1)-1).AND.
     1               ALENS(89,INT(W1)).EQ.ALENS(89,INT(W1)-1)) DM=.TRUE.
              END IF
              IF(ALENS(68,INT(W1)).EQ.1.0D0.AND.DM)
     1        DM=.FALSE.
              IF(ALENS(68,INT(W1)).EQ.0.0D0.AND..NOT.DM)
     1        DM=.TRUE.
C
              IF(ALENS(23,INT(W1)).NE.0.0D0.OR.ALENS(8,INT(W1)).NE.0.0D0
     1        .OR.ALENS(1,INT(W1)).NE.0.0D0.OR..NOT.DM) THEN
C     SURFACE NOT A PLANO DUMMY
                  WRITE(OUTLYNE,*) 'TYPE ',INT(DABS(W2)),
     1            ' SURFACE MUST BE A PLANO-DUMMY SURFACE'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
C     CHECK FOR INVALID SURFACE TYPES FOR A TYPE 23 SURFACE
          IF(INT(DABS(W2)).EQ.23) THEN
              IF(ALENS(23,INT(W1)).NE.0.0D0) THEN
C     SURFACE NOT A ROTATIONALLY SYMMETRIC SURFACE
                  OUTLYNE='TYPE 23 SURFACE MUST BE ROTATIONALLY'
                  CALL SHOWIT(1)
                  OUTLYNE= 'SYMMETRIC SURFACES'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
          IF(INT(DABS(W2)).EQ.24) THEN
              IF(ALENS(25,INT(W1)).EQ.2.0D0
     1        .OR.ALENS(25,INT(W1)).EQ.3.0D0 ) THEN
C     SURFACE CAN'T HAVE A TILT AUTO ON IT
                  OUTLYNE='TYPE 24 SURFACE MAY NOT HAVE TILT AUTO OR TILT AUTOM'
                  CALL SHOWIT(1)
                  OUTLYNE= 'ASSIGNED TO IT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
C
C       SET SURFACE TYPE IN ALENS.
C
          IF(OPTMES) THEN
              IF(IN.EQ.5) THEN
                  WRITE(OUTLYNE,*)
     1              'SURFACE ',INT(W1),' NOW DEFINED AS A TYPE ',INT(W2),' SURFACE'
                  CALL SHOWIT(1)
              END IF
          END IF
          ALENS(34,INT(W1))=W2
C
C       FLUSH THE CURRENT COEFFICIENT STORAGE
C
          FTFL01(1:96,INT(W1))=0.0D0
C     SET DEFAULTS FOR TYPE 12
          IF(INT(W2).EQ.12) THEN
              FTFL01(1,INT(W1))=0.0D0
              IF(SYSTEM1(11).LE.5.0D0)
     1        FTFL01(2,INT(W1))=SYSTEM1(INT(SYSTEM1(11)))
              IF(SYSTEM1(11).GT.5.0D0)
     1        FTFL01(2,INT(W1))=SYSTEM1(65+INT(SYSTEM1(11)))
              FTFL01(3,INT(W1))=0.0D0
              FTFL01(4,INT(W1))=0.0D0
              FTFL01(5,INT(W1))=0.0D0
              FTFL01(6,INT(W1))=1.0D0
              FTFL01(7,INT(W1))=0.0D0
              FTFL01(8,INT(W1))=0.0D0
              FTFL01(9,INT(W1))=0.0D0
              FTFL01(10,INT(W1))=1.0D0
              FTFL01(11,INT(W1))=0.0D0
          END IF
          IF(INT(W2).EQ.13) THEN
C     SET DEFAULTS FOR TYPE 13
              FTFL01(1,INT(W1))=0.0D0
              IF(SYSTEM1(11).LE.5.0D0)
     1        FTFL01(2,INT(W1))=SYSTEM1(INT(SYSTEM1(11)))
              IF(SYSTEM1(11).GT.5.0D0)
     1        FTFL01(2,INT(W1))=SYSTEM1(65+INT(SYSTEM1(11)))
              FTFL01(3,INT(W1))=0.0D0
              FTFL01(4,INT(W1))=0.0D0
              FTFL01(5,INT(W1))=0.0D0
              FTFL01(6,INT(W1))=1.0D0
              FTFL01(7,INT(W1))=1.0D0
              FTFL01(8,INT(W1))=0.0D0
              FTFL01(9,INT(W1))=0.0D0
              FTFL01(10,INT(W1))=0.0D0
              FTFL01(11,INT(W1))=1.0D0
              FTFL01(12,INT(W1))=1.0D0
              FTFL01(13,INT(W1))=2.0D0
              FTFL01(14,INT(W1))=3.0D0
          END IF
          IF(INT(W2).EQ.19) THEN
C     SET DEFAULTS FOR TYPE 19
              FTFL01(1,INT(W1))=0.0D0
              FTFL01(2,INT(W1))=5.0D0
              FTFL01(3,INT(W1))=1.0D0
              FTFL01(4,INT(W1))=1.0D0
          END IF
          IF(INT(W2).EQ.20) THEN
C     SET DEFAULTS FOR TYPE 20
              FTFL01(1,INT(W1))=0.0D0
              FTFL01(2,INT(W1))=5.0D0
              FTFL01(3,INT(W1))=1.0D0
              FTFL01(4,INT(W1))=0.0D0
              FTFL01(5,INT(W1))=1.0D0
          END IF
          IF(INT(W2).EQ.22) THEN
C     SET DEFAULTS FOR TYPE 22
              FTFL01(1,INT(W1))=0.0D0
              FTFL01(2,INT(W1))=5.0D0
              FTFL01(3,INT(W1))=1.0D0
              FTFL01(4,INT(W1))=1.0D0
          END IF
          IF(INT(W2).EQ.24) THEN
C     SET DEFAULTS FOR TYPE 24
              FTFL01(1,INT(W1))=1.0D0
              FTFL01(2,INT(W1))=1.0D0
              FTFL01(3,INT(W1))=0.0D0
              FTFL01(4,INT(W1))=0.0D0
          END IF
C
C       SURFACE NOW CORRECT TYPE WITH ZERO VALUED COEFS
C
          RETURN
      END
C SUB SPSEOS.FOR
      SUBROUTINE SPSEOS
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE SPSEOS. THIS IS THE SUBROUTINE WHICH
C       HANDLES THE EXIT FROM THE SPSRF OR UPDATE SPSRF
C       PROGRAM LEVEL TO THE CMD
C       LEVEL.
C
          INTEGER III,IIII
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
          F7=0
          F8=0
C
C
C       UPDATE THE PERMANENT SPSRF DATA IF F12=1
C
C***************************************************************************
          IF(F12.EQ.1) THEN
C       SINCE ALENS(34,I) MGHT HAVE BEEN CHANGED
              III=INT(SYSTEM1(20))
              FT01P(1:96,0:III)=FTFL01(1:96,0:III)
              IIII=LSIZ
              ALENP(1:IIII,0:III)=ALENS(1:IIII,0:III)
C
          ELSE
C       F12 NOT 1, PROCEED
          END IF
C
C       NOW CALL LNSEOS
          F1=0
          F6=1
          F22=0
          LNSTYP=1
          CALL LNSEOS
          RETURN
      END
C SUB SPONOF.FOR
      SUBROUTINE SPONOF
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE SPONOF. THIS IS THE SUBROUTINE WHICH
C       HANDLES THE "SPSRF ON" AND "SPSRF OFF"
C       COMMAND FROM CMD LEVEL
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
C
C       SPSRF ON TAKES NO STRING DATA
C
          IF(SST.EQ.1.OR.S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
              WRITE(OUTLYNE,*)
     1        '" ',WC,' ',WQ,' " ONLY TAKES NUMERIC WORD #1 INPUT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(DF1.EQ.1) THEN
              WRITE(OUTLYNE,*)
     1        '" ',WC,' ',WQ,' " REQUIRES EXPLICIT NUMERIC WORD #1 INPUT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C       PROCEED WITH SPSRF ON/OFF
C
          IF(W1.EQ.0.0) THEN
              WRITE(OUTLYNE,*)
     1        'SPECIAL SURFACE TYPE NOT VALID ON OBJECT SURFACE'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(ALENS(34,INT(W1)).EQ.0.0D0) THEN
              WRITE(OUTLYNE,*)
     1        'WARNING: SURFACE ',INT(W1),' NOT A SPECIAL SURFACE TYPE'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(WQ.EQ.'ON'.AND.ALENS(34,INT(W1)).LT.0.0D0) THEN
              ALENS(34,INT(W1))=-ALENS(34,INT(W1))
          END IF
          IF(WQ.EQ.'OFF'.AND.ALENS(34,INT(W1)).GT.0.0D0) THEN
              ALENS(34,INT(W1))=-ALENS(34,INT(W1))
          END IF
          RETURN
      END
C SUB SPFIT2.FOR
      SUBROUTINE SPFIT2
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE SPFIT2. THIS IS THE SUBROUTINE WHICH
C       HANDLES THE SPFIT AFTER INITIALIZATION BY
C       SUBROUTINE SPFIT.
C
          INTEGER IDATA
C
          REAL*8 STYPE

          CHARACTER FITGLASS*8
C
          COMMON/GLASSFIT/FITGLASS
C
          COMMON/SPIDAT/IDATA
C
          COMMON/ST/STYPE
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
C
C       CHECK FOR VALID COMMANDS. (BLANK AND ? DONE IN
C       CONTRO.FOR)
C
          IF(WC.NE.'EOS'.AND.WC.NE.'EVAL'.AND.WC.NE.'COEFS'
     1    .AND.WC.NE.'FIT'.AND.WC.NE.'READ'.AND.WC.NE.'COEF'
     2    .AND.WC.NE.'TYPE'.AND.WC.NE.'SURF'.AND.WC.NE.'FITGLASS'
     3    .AND.WC.NE.'C'.AND.WC.NE.'LISTCOEF'.AND.WC.NE.'GDATA'.AND.
     4    WC.NE.'M'.AND.WC.NE.'DATA'.AND.WC.NE.'LIST') THEN
              OUTLYNE='INVALID SPFIT COMMAND'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(WC.EQ.'M'.OR.WC.EQ.'C') THEN
              CALL MESCOM
              RETURN
          END IF
C
C       FIRST HANDLE EOS (END OF SUBFILE) IN THIS CASE THE SPFIT
C       SUBFILE.
C
          IF(WC.EQ.'EOS') THEN
              IF(SQ.EQ.1.OR.SST.EQ.1.OR.SN.EQ.1) THEN
                  OUTLYNE='"EOS" TAKES NO EXPLICIT INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C
              CALL FITEOS
              RETURN
          END IF
C
C       HERE IS WHERE THE BRANCHING TO SUBROUTINES WHICH HANDLE
C       INPUT TO SPFIT.
C
C
C       FILE LENSSTORE.DOC CONTAINS INFORMATION ON THE
C       SPECIFIC STORAGE LOCATIONS FOR LENS DATA ITEMS.
C
C       COMMAND (SURF)
          IF(WC.EQ.'SURF') THEN
              CALL FITSUR
              RETURN
          END IF
C       COMMAND (TYPE)
          IF(WC.EQ.'TYPE') THEN
              CALL FITTYP
              RETURN
          END IF
C       COMMAND (COEF)
          IF(WC.EQ.'COEF') THEN
              CALL FITCFF
              RETURN
          END IF
C       COMMAND (DATA)
C       CHECK IF TYPE OR SURF SET
          IF(F23.EQ.0) THEN
              OUTLYNE='"TYPE" OR "SURF" MUST BE ENTERED BEFORE'
              CALL SHOWIT(1)
              OUTLYNE='DATA MAY BE ENTERED'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(WC.EQ.'DATA'.OR.WC.EQ.'GDATA') THEN
              CALL FITDAT
              RETURN
          END IF
C       COMMAND (READ)
          IF(WC.EQ.'READ') THEN
              IF(F23.EQ.0) THEN
                  OUTLYNE='"TYPE" OR "SURF" MUST BE ENTERED BEFORE'
                  CALL SHOWIT(1)
                  OUTLYNE='DATA MAY BE "READ"'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              CALL RDDATA
              RETURN
          END IF
C       COMMAND (FIT)
          IF(WC.EQ.'FIT'.OR.WC.EQ.'FITGLASS') THEN
              IF(F25.EQ.0) THEN
                  OUTLYNE='NO ACCUMULATED DATA EXISTS FOR FITTING'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              CALL FITFIT
              IF(WC.EQ.'FITGLASS') THEN
C     OPEN THE USER.DAT FILE AND ADD AN ENTRY
                  OPEN(UNIT=36,ACCESS='APPEND',FILE=trim(HOME)//'USER.DAT',
     1            RECL=132,STATUS='UNKNOWN')
                  WRITE(UNIT=36,FMT=*)
     1            FITGLASS,
     2            CFTYPE(9),CFTYPE(11),CFTYPE(7),CFTYPE(5),CFTYPE(3),CFTYPE(1)
                  CALL CLOSE_FILE(36,1)
              END IF
              RETURN
          ELSE
          END IF
C       COMMAND (COEFS)
          IF(WC.EQ.'COEFS') THEN
              IF(F24.EQ.0) THEN
                  OUTLYNE='FITTED DATA DOES NOT EXIST'
                  CALL SHOWIT(1)
                  OUTLYNE='COEFFICIENTS NOT YET AVAILABLE'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              CALL FCOEFS
              RETURN
          END IF
C       COMMAND (LISTCOEF)
          IF(WC.EQ.'LISTCOEF') THEN
              IF(F24.EQ.0) THEN
                  OUTLYNE='FITTED DATA DOES NOT EXIST'
                  CALL SHOWIT(1)
                  OUTLYNE='COEFFICIENTS NOT YET AVAILABLE'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              CALL LFCOEFS
              RETURN
          END IF
C       COMMAND (EVAL)
          IF(WC.EQ.'EVAL') THEN
              IF(F24.EQ.0) THEN
                  OUTLYNE='FITTED DATA DOES NOT EXIST FOR EVALUATION'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              CALL FTEVAL
              RETURN
          END IF
C       COMMAND (LIST)
          IF(WC.EQ.'LIST') THEN
              IF(F24.EQ.0) THEN
                  OUTLYNE='FITTED DATA DOES NOT EXIST FOR LISTING'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              CALL FTLIST
              RETURN
          END IF
          RETURN
      END
C SUB SPFIT.FOR
      SUBROUTINE SPFIT
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE SPFIT. THIS IS THE SUBROUTINE WHICH
C       INITIALLY STARTS (SPFIT) FITTING ROUTINE
C       THE CMD LEVEL IS DISABLED AND FLAG F9 IS SET TO 1.
C
          INTEGER DATCNT,IDATA,I
C
          COMMON/NUMDAT/DATCNT
C
          REAL*8 ACCUM(1:96,1:96),
     1    CCOL(1:96),STYPE,SSURF
C
          COMMON/SPIDAT/IDATA
C
          COMMON/ST/STYPE
C
          COMMON/SSS/SSURF
C
          COMMON/ACDATA/ACCUM,CCOL
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
          IF(SST.EQ.1.OR.SN.EQ.1.OR.SQ.EQ.1) THEN
              OUTLYNE='"SPFIT" TAKES NO EXPLICIT INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          F1=0
          F9=1
C       INITIALIZE THE DATA SAVING ARRAY DDATA
          I=MAXSPS
          DDATA(1:4,1:I)=0.0D0
          IDATA=0
C       SET STYPE AND SSURF T0 -1.0
          STYPE=-1.0D0
          SSURF=-1.0D0
C       SET CFF FLAGS TO ZERO
          CFF(1:96)=0
C       ZERO THE ACCUM AND CCOL ARRAYS
          CCOL(1:96)=0.0D0
          ACCUM(1:96,1:96)=0.0D0
C
C       INITIALIZE THE DATA COUNTER TO ZERO
          DATCNT=0
C
C       THE DEFUALT SURFACE NUMBER IS SET HERE AT -99
          SURF=-99
C       SSURF=-1.0 FOR THIS AND WAS SET ABOVE.
C
C       THIS IS USED TO DIFFERENTIATE BETWEEN A FIT TO A REAL SURFACE
C       AND A SIMPLE DATA FIT WITH NOT LENS SURFACE INTERACTION
C
C       ANY OTHER ACTION TO BE TAKEN DURING SPFIT IS HANDLED FROM
C       WITHIN SUBROUTINE SPFIT2.
C
          RETURN
      END
C SUB SPECIN.FOR
      SUBROUTINE SPECIN
C
          IMPLICIT NONE
C
C       THIS SUBROUTINE IS CALLED TO ENTER THE SPECT LEVEL
C       COMMANDS AT THE SPECTR LEVEL
C
          CHARACTER TNAME*8,NM*8,FN*10,
     1    TTIME*8,DDATE*10,TTTIM*8,DDDAT*10
C
          LOGICAL TORR,EXISJK
C
          COMMON/TTOORR/TORR
C
          COMMON/STRNGR/DDDAT,TTTIM,NM,FN

          INTEGER OCC,NUM_JK,
     3    POINTS,NF,I,J,
     4    CTAB,
     5    NTOTAL,CNTR
C
          REAL*8 LAMB1,LAMB2,
     1    CUMULT(1:1001,1:3),TABLE(1:1001,1:3),DATA1,DATA2
C
          COMMON/CUM/CUMULT
C
          COMMON/CURNAM/TNAME
C
          COMMON/TABL/TABLE,CTAB
C
          COMMON/CNTER/CNTR
C
          COMMON/WAVEL/LAMB1,LAMB2,NTOTAL
C
          INCLUDE 'datmai.inc'
C
          IF(WC.EQ.'M'.OR.WC.EQ.'C') THEN
              CALL MESCOM
              RETURN
          END IF
C       WHAT IF F18 = 1
          IF(F18.EQ.1) THEN
C       ONLY THE COMMANDS "DATA" AND "ENDTABLE" ARE VALID
              IF(WC.NE.'DATA'.AND.WC.NE.'ENDTABLE') THEN
                  WRITE(OUTLYNE,*)
     1            'ONLY THE "DATA" AND "ENDTABLE" COMMANDS ARE VALID AT THIS LEVEL'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C       WC IS DATA OR END
C       WC=END
              IF(WC.EQ.'ENDTABLE') THEN
                  IF(SST.EQ.1.OR.SQ.EQ.1.OR.SN.EQ.1) THEN
                      WRITE(OUTLYNE,*)'"END" TAKES NO EXPLICIT INPUT'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  F18=0
                  F19=1
C
C       DO STORAGE OF TABLE FILE TO DISK
C
                  CALL TFILE(TNAME)
                  WRITE(OUTLYNE,*)'EXITING THE "TABLE" INPUT LEVEL OF "SPECT"'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'READY FOR SPECT LEVEL INPUT'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C       DOES WC=DATA
              IF(WC.EQ.'DATA') THEN
C
C       IF W3, W4 AND W5 INPUT, PRINT MESSAGE
                  IF(SST.EQ.1.OR.SQ.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
                      WRITE(OUTLYNE,*)
     1                '"DATA" ONLY TAKES NUMERIC WORD #1 AND #2 INPUT'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  IF(DF1.EQ.1.OR.DF2.EQ.1) THEN
                      WRITE(OUTLYNE,*)
     1                '"DATA" REQUIRES EXPLICIT NUMERIC WORD 1 AND 2 INPUT'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
C       COUNTER WAS SET TO 1 BY TABLE
C
C       CHECK FOR <ZERO LAMBDAS
                  IF(W1.LT.0.0D0) THEN
                      WRITE(OUTLYNE,*)
     1                'A NEGATIVE WAVELENGTH VALUE IS NOT ALLOWED'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
C       CHECK THAT FIRST WAVELENGTH ENTRY IS GE LAMB1
C       CHECK THAT EACH ENTRY HAS LAMBDA GREATER THAN
C       LAST ENTRY
                  IF(W1.LT.LAMB1.OR.W1.GT.LAMB2) THEN
                      WRITE(OUTLYNE,*)'WAVELENGTH ENTRY OUTSIDE BAND DEFINED BY'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'"WAVLN" OR "START" COMMAND'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  IF(CNTR.GE.2) THEN
                      IF(W1.LE.TABLE((CNTR-1),2)) THEN
                          WRITE(OUTLYNE,*)'WAVLENGTH ENTRY NOT IN ASSENDING ORDER'
                          CALL SHOWIT(1)
                          WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                          CALL SHOWIT(1)
                          CALL MACFAL
                          RETURN
                      END IF
                  END IF
C
                  CTAB=CNTR
                  TABLE(CNTR,1)=DBLE(CNTR)
                  TABLE(CNTR,2)=W1
                  TABLE(CNTR,3)=W2
                  IF((CNTR+1).GT.NTOTAL) THEN
                      WRITE(OUTLYNE,*)'TABLE MEMORY AREA FULL'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'NO ADDITIONAL TABLE INPUT ALLOWED'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'USE "ENDTABLE" COMMAND EXIT TABLE INPUT LEVEL'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  CNTR=CNTR+1

C
                  RETURN
              END IF
          END IF
          IF(WC.EQ.'DATA'.OR.WC.EQ.'ENDTABLE') THEN
              WRITE(OUTLYNE,*)'SPECT-TABLE INPUT COMMANDS NOT AVAILABLE'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'OUTSIDE TABLE INPUT LEVEL'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'COMMAND IGNORED'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
C       THE "TABLE" COMMAND
C
C       THE  F18 IS SET TO 1 FOR TABLE INPUT

          IF(WC.EQ.'TABLE') THEN
              IF(SST.EQ.1.OR.SN.EQ.1) THEN
                  WRITE(OUTLYNE,*)'"TABLE" TAKES NO STRING OR NUMERIC INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(SQ.EQ.0.OR.WQ.EQ.'        ') THEN
                  WRITE(OUTLYNE,*)'TABLE NAME MISSING OR BLANK'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND WITH A TABLE NAME'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              CNTR=1
              TNAME=WQ
              F19=0
              F18=1
              RETURN
          END IF
C
          IF(WC.EQ.'EOS') THEN
              IF(SQ.EQ.1.OR.SST.EQ.1.OR.SN.EQ.1) THEN
                  WRITE(OUTLYNE,*)'"EOS" TAKES NO EXPLICIT INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C                       SET F1=1 AND F17=0
              IF(F1.NE.1) F1=1
              F17=0
              NTOTAL=-99
              WRITE(OUTLYNE,*) 'RETURNED TO CMD LEVEL'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
C       "START" COMMAND
          IF(WC.EQ.'START') THEN
              IF(SQ.EQ.1.OR.SST.EQ.1.OR.SN.EQ.1) THEN
                  WRITE(OUTLYNE,*)'"START" TAKES NO ADDITIONAL INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              CALL SSTART
              RETURN
          END IF
C       "DELETE" COMMAND
          IF(WC.EQ.'DELETE') THEN
              IF(SST.EQ.1.OR.S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
                  WRITE(OUTLYNE,*)
     1            '"DELETE" ONLY TAKES QUALIFIER OR NUMERIC WORD #1 INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(SQ.EQ.1.AND.DF1.EQ.0) THEN
                  WRITE(OUTLYNE,*)
     1            '"DELETE" TAKES EITHER QUALIFIER OR NUMERIC WORD #1 INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'BUT NOT BOTH.'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(SQ.EQ.0.AND.DF1.EQ.1) THEN
                  WRITE(OUTLYNE,*)
     1            '"DELETE" REQUIRES EITHER QUALIFIER OR NUMERIC WORD #1 INPUT.'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              CALL DELETE
              RETURN
          END IF
C       "DIRECT" OR "DIR" COMMAND
          IF(WC.EQ.'DIRECT'.OR.WC.EQ.'DIR') THEN
              IF(SQ.EQ.1.OR.SST.EQ.1.OR.S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1
     1        .OR.S5.EQ.1) THEN
                  WRITE(OUTLYNE,*)'"DIRECT" ONLY TAKES NUMERIC WORD #1 INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              CALL TABDIR
              RETURN
          END IF
C
C       "WAVLN" COMMAND
          IF(WC.EQ.'WAVLN') THEN
C       DO DEFAULT VALUES
              IF(DF1.EQ.1) THEN
                  DF1=0
                  W1=0.05
              END IF
C       SET LAMB1
              LAMB1=W1
              IF(DF2.EQ.1) THEN
                  DF2=0
                  W2=20.0
              END IF
C       SET LAMB2
              LAMB2=W2
              IF(DF3.EQ.1) THEN
                  DF3=0
                  W3=1001.0
              END IF
              NTOTAL=INT(W3)
              IF(W1.LT.0.0D0.OR.W2.LT.0.0D0) THEN
                  WRITE(OUTLYNE,*)'A NEGATIVE WAVELENGTH VALUE IS NOT ALLOWED'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(SQ.EQ.1.OR.SST.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
                  WRITE(OUTLYNE,*)'"WAVLN" TAKES NO NUMERIC WORD #4 OR #5 INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C       CHECK THAT NW1 AND NW2 AND NW3 ARE NOT NEGATIVE
              IF(W1.LT.0.0.OR.W2.LT.0.0.OR.W3.LT.0.0) THEN
                  WRITE(OUTLYNE,*)'NUMERIC INPUT TO "WAVLN" MAY NOT BE NEGATIVE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C       CHECK THAT W3 GE 2
              IF(W3.LT.2.0) THEN
                  WRITE(OUTLYNE,*)
     1            'THE NUMBER OF POINTS MUST BE GREATER THAN OR EQUAL TO 2.'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C       CHECK THAT NW1 IS LT NW2
              IF(W1.GE.W2) THEN
                  WRITE(OUTLYNE,*)
     1            'NUMERIC WORD 2 MUST BE GREATER THAN NUMERIC WORD 1'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C       CHECK THAT NW3 IS GE 1 AND LE 1001
              IF(W3.LT.1.0.OR.W3.GT.1001.0) THEN
                  WRITE(OUTLYNE,*)'NUMERIC WORD 3 IS OUTSIDE RANGE OF 1 TO 1001'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C       THERE IS NO CALL TO A WAVLN SUBROUTINE. ALL WORK
C       WAS DONE FOR THE WAVLN COMMAND HERE BUT CALL SSTART
C
              CALL SSTART
C
              RETURN
          END IF
C       "GETFILE" COMMAND
          IF(WC.EQ.'GETFILE') THEN
              IF(SST.EQ.1) THEN
                  WRITE(OUTLYNE,*)
     1            '"GETFILE" ONLY TAKES QUALIFIER AND NUMERIC WORD INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(SQ.EQ.0) THEN
                  WRITE(OUTLYNE,*)
     1            '"GETFILE" REQUIRES A SPECT FILE NAME AS THE QUALIFIER WORD'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C       DEFAULTS
              IF(DF1.EQ.1) W1=1.0D0
              IF(DF2.EQ.1) W2=0.0D0
              IF(DF3.EQ.1) W3=1.0D0
              IF(DF4.EQ.1) W4=0.0D0
              IF(DF5.EQ.1) W5=1.0D0
C
              IF(W5.NE.DBLE(INT(W5)))THEN
                  WRITE(OUTLYNE,*)
     1            'ONLY AN INTEGER EXPONENT IS ALLOWED IN "GETFILE"'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              CALL GFILE
              TNAME=NM
              RETURN
          END IF
C       "BLACKBDY" COMMAND
          IF(WC.EQ.'BLACKBDY') THEN
              IF(SST.EQ.1.OR.S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
                  WRITE(OUTLYNE,*)
     1            '"BLACKBDY" ONLY TAKES QUALIFIER AND NUMERIC WORD #1 INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(SQ.EQ.0) THEN
                  WQ='SRE'
              END IF
              IF(WQ.NE.' '.AND.WQ.NE.'SRE'.AND.WQ.NE.'SRPE') THEN
                  WRITE(OUTLYNE,*)
     1            'INVALID QUALIFIER WORD USED WITH "BLACKBDY"'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C       DEFAULTS
              IF(DF1.EQ.1) W1=300.0D0
C
              IF(W1.LT.0.0D0) THEN
                  WRITE(OUTLYNE,*)
     1            'A NEGATIVE TEMPERATURE VALUE IS NOT ALLOWED'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C
              CALL BLCKBD
              RETURN
          END IF
C       "PHOTOPIC" COMMAND
          IF(WC.EQ.'PHOTOPIC') THEN
              IF(SST.EQ.1.OR.SN.EQ.1.OR.SQ.EQ.1) THEN
                  WRITE(OUTLYNE,*)
     1            '"PHOTOPIC" TAKES NO ADDITIONAL INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C
              CALL PHOTOPIC
              RETURN
          END IF
C       "SCOTOPIC" COMMAND
          IF(WC.EQ.'SCOTOPIC') THEN
              IF(SST.EQ.1.OR.SN.EQ.1.OR.SQ.EQ.1) THEN
                  WRITE(OUTLYNE,*)
     1            '"SCOTOPIC" TAKES NO ADDITIONAL INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C
              CALL SCOTOPIC
              RETURN
          END IF
C       "SPRINT" COMMAND
          IF(WC.EQ.'PRINT') THEN
              IF(SST.EQ.1) THEN
                  WRITE(OUTLYNE,*)
     1            '"SPRINT" ONLY TAKES QUALIFIER AND NUMERIC WORD INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C       DEFAULTS
              IF(DF1.EQ.1) W1=1.0D0
              IF(DF2.EQ.1) W2=0.0D0
              IF(DF3.EQ.1) W3=1.0D0
              IF(DF4.EQ.1) W4=0.0D0
              IF(DF5.EQ.1) W5=1.0D0
C
              IF(W5.NE.DBLE(INT(W5)))THEN
                  WRITE(OUTLYNE,*)'ONLY AN INTEGER EXPONENT IS ALLOWED IN "SPRINT"'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C
              CALL PPRINT
              RETURN
          END IF
C
C       CUME COMMAND
          IF(WC.EQ.'CUME') THEN
              IF(SQ.EQ.1.OR.SST.EQ.1.OR.SN.EQ.1) THEN
                  WRITE(OUTLYNE,*)'"CUME" TAKES NO ADDITIONAL INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              CALL CUME
              RETURN
          END IF
C       WFACTOR COMMAND
          IF(WC.EQ.'WFACTOR') THEN
              IF(SQ.EQ.1.OR.SST.EQ.1) THEN
                  WRITE(OUTLYNE,*)
     1            '"WFACTOR" TAKES NO QULIFIER OR STRING INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
                  WRITE(OUTLYNE,*)
     1            '"WFACTOR" ONLY TAKES NUMERIC WORD #1 INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(W1.LT.1.0D0.OR.W1.GT.10.0D0) THEN
                  WRITE(OUTLYNE,*)
     1            'NUMBER OF WAVELENGTHS MUST BE 1 TO 10'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              NUM_JK=INT(W1)
              CALL WFACTOR(NUM_JK)
              RETURN
          END IF
C       WORK COMMAND
          IF(WC.EQ.'WORK') THEN
              IF(SQ.EQ.1.OR.SST.EQ.1.OR.SN.EQ.1) THEN
                  WRITE(OUTLYNE,*)'"WORK" TAKES NO ADDITIONAL INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              CALL PWORK
              RETURN
          END IF
C       "LIST" COMMAND
          IF(WC.EQ.'LIST') THEN
              IF(SST.EQ.1.OR.S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
                  WRITE(OUTLYNE,*)
     1            '"LIST" ONLY TAKES QUALIFIER OR NUMERIC WORD #1 INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(SQ.EQ.1.AND.DF1.EQ.0) THEN
                  WRITE(OUTLYNE,*)
     1            '"LIST" TAKES EITHER QUALIFIER OR NUMERIC WORD 1 INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'BUT NOT BOTH'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
          IF(WC.EQ.'LIST'.AND.SQ.EQ.0.AND.DF1.EQ.1) THEN
              IF(F19.EQ.0) THEN
                  WRITE(OUTLYNE,*)'NO TABLE DATA IN MEMORY TO PRINT'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              ELSE
                  CALL PTABLE
                  RETURN
              END IF
          END IF
          IF(WC.EQ.'LIST'.AND.SQ.EQ.1.AND.DF1.EQ.1) THEN
C       SEARCH FOR FILE NAMED WQ AND IF FOUND, LOAD IT, THEN
C       PROCEED.
C       DATA STORED IN TRA.DAT IS:
C               FILE NAME (TNAME) CHARACTER *8
C               DATA1 STARTING WAVELENGTH (DOUBLE PRES.)
C               DATA2 ENDIG WAVLENGTH (DOUBLE PRES.)
C               POINTS - NUMBER OF DATA POINTS (INTEGER)
C               OCC- OCCUPANCY FLAG, 0=EMPTY, 1- FULL (INTEGER)
C               DDATE-DATE FILED
C               TTIME=TIME FILED
C       FIND THE DESIRED FILE
C       ***************************************************************
              EXISJK=.FALSE.
              INQUIRE(FILE=LIBTRA//'TRA.DAT',EXIST=EXISJK)
              IF(.NOT.EXISJK) THEN
                  WRITE(OUTLYNE,*)'"TABLE FILES DO NOT YET EXIST'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'TO INITIALIZE IT, USE "ITF" AND "PROCEED"'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C       ***************************************************************
              OPEN(UNIT=35,ACCESS='DIRECT',FILE=LIBTRA//'TRA.DAT',
     1        FORM='UNFORMATTED',RECL=(50*NRECL),STATUS='UNKNOWN')
C
              DO 201 I=1,999
                  READ(UNIT=35,REC=I) NM,DATA1,DATA2,POINTS,OCC,DDATE,TTIME
                  TTTIM=TTIME
                  DDDAT=DDATE
                  IF(NM.EQ.WQ) THEN
C       FOUND FILE
                      TNAME=WQ
                      GO TO 301
                  END IF
 201          CONTINUE
C       IF YOU GOT HERE, FILE DID NOT EXIST
              WRITE(OUTLYNE,*)'FILE ',WQ,' DOES NOT EXIST'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL CLOSE_FILE(35,1)
              CALL MACFAL
              RETURN
 301          CONTINUE
C       LOAD FILE INTO TABLE ARRAY
C
              NF=I
              CALL TRAFIL(NF,FN)
C
C       OPEN AND READ FROM FILE
              OPEN(UNIT=35,ACCESS='DIRECT',FILE=LIBTRA//FN,
     1        FORM='UNFORMATTED',RECL=(80*NRECL),STATUS='UNKNOWN')
C
              CTAB=POINTS
              DO 211 J=1,POINTS
                  READ(UNIT=35,REC=J) TABLE(J,1),TABLE(J,2),TABLE(J,3)
 211          CONTINUE
              CALL CLOSE_FILE(35,1)
              F19=1
              CALL PTABLE
              RETURN
          END IF
          IF(WC.EQ.'LIST'.AND.SQ.EQ.0.AND.DF1.EQ.0) THEN
C       LIST FILE NUMBER INT(W1) IF NOT EMPTY
C       FIND THE DESIRED FILE
C       ***************************************************************
              EXISJK=.FALSE.
              INQUIRE(FILE=LIBTRA//'TRA.DAT',EXIST=EXISJK)
              IF(.NOT.EXISJK) THEN
                  WRITE(OUTLYNE,*)'"TABLE FILES DO NOT YET EXIST'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'TO INITIALIZE IT, USE "ITF" AND "PROCEED"'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C       ***************************************************************
              OPEN(UNIT=35,ACCESS='DIRECT',FILE=LIBTRA//'TRA.DAT',
     1        FORM='UNFORMATTED',RECL=(50*NRECL),STATUS='UNKNOWN')
              READ(UNIT=35,REC=INT(W1))
     1        NM,DATA1,DATA2,POINTS,OCC,DDATE,TTIME
              DDDAT=DDATE
              TTTIM=TTIME
              IF(OCC.EQ.0) THEN
C       FILE EMPTY, PRINT MESSAGE AND RETURN
                  WRITE(OUTLYNE,*)'FILE # ',INT(W1),' IS EMPTY'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL CLOSE_FILE(35,1)
                  CALL MACFAL
                  RETURN
              ELSE
                  CALL CLOSE_FILE(35,1)
                  TNAME=NM
              END IF
C       LOAD FILE INTO TABLE ARRAY
C
              NF=INT(W1)
              CALL TRAFIL(NF,FN)
C
C       OPEN AND READ FROM FILE
              OPEN(UNIT=35,ACCESS='DIRECT',FILE=LIBTRA//FN,
     1        FORM='UNFORMATTED',RECL=(80*NRECL),STATUS='UNKNOWN')
C
              CTAB=POINTS
              DO 212 J=1,POINTS
                  READ(UNIT=35,REC=J) TABLE(J,1),TABLE(J,2),TABLE(J,3)
 212          CONTINUE
              CALL CLOSE_FILE(35,1)
              F19=1
              CALL PTABLE
              RETURN
          END IF
C       "PLOTR AND "PLOTT" COMMAND
          IF(WC.EQ.'PLOTR'.OR.WC.EQ.'PLOTT') THEN
              IF(SST.EQ.1) THEN
                  WRITE(OUTLYNE,*)
     1            '"PLOTR" AND "PLOTT" TAKE NO STRING INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(SQ.EQ.1.AND.DF1.EQ.0) THEN
                  WRITE(OUTLYNE,*)
     1            '"PLOTR" AND "PLOTT" TAKE EITHER QUALIFIER OR ',
     1            'NUMERIC WORD 1 INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'BUT NOT BOTH'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
          IF(WC.EQ.'PLOTR'.AND.SQ.EQ.0.AND.DF1.EQ.1.OR.
     1    WC.EQ.'PLOTT'.AND.SQ.EQ.0.AND.DF1.EQ.1) THEN
              WRITE(OUTLYNE,*)
     1        '"PLOTR" AND "PLOTT" REQUIRES EITHER QUALIFIER OR ',
     1        'NUMERIC WORD 1 INPUT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(WC.EQ.'PLOTT'.AND.SQ.EQ.1.OR.
     1    WC.EQ.'PLOTR'.AND.SQ.EQ.1) THEN
C       SEARCH FOR FILE NAMED WQ AND IF FOUND, LOAD IT, THEN
C       PROCEED.
C       DATA STORED IN TRA.DAT IS:
C               FILE NAME (TNAME) CHARACTER *8
C               DATA1 STARTING WAVELENGTH (DOUBLE PRES.)
C               DATA2 ENDIG WAVLENGTH (DOUBLE PRES.)
C               POINTS - NUMBER OF DATA POINTS (INTEGER)
C               OCC- OCCUPANCY FLAG, 0=EMPTY, 1- FULL (INTEGER)
C               DDATE-DATE FILED
C               TTIME=TIME FILED
C       FIND THE DESIRED FILE
C       ***************************************************************
              EXISJK=.FALSE.
              INQUIRE(FILE=LIBTRA//'TRA.DAT',EXIST=EXISJK)
              IF(.NOT.EXISJK) THEN
                  WRITE(OUTLYNE,*)'"TABLE FILES DO NOT YET EXIST'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'TO INITIALIZE IT, USE "ITF" AND "PROCEED"'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C       ***************************************************************
              OPEN(UNIT=35,ACCESS='DIRECT',FILE=LIBTRA//'TRA.DAT',
     1        FORM='UNFORMATTED',RECL=(50*NRECL),STATUS='UNKNOWN')
C
              DO 2012 I=1,999
                  READ(UNIT=35,REC=I) NM,DATA1,DATA2,POINTS,OCC,DDATE,TTIME
                  DDDAT=DDATE
                  TTTIM=TTIME
                  IF(NM.EQ.WQ) THEN
C       FOUND FILE
                      TNAME=WQ
                      GO TO 3012
                  END IF
 2012         CONTINUE
C       IF YOU GOT HERE, FILE DID NOT EXIST
              WRITE(OUTLYNE,*)'FILE ',WQ,' DOES NOT EXIST'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL CLOSE_FILE(35,1)
              CALL MACFAL
              RETURN

C       LOAD FILE INTO TABLE ARRAY
C
 3012         NF=I
              CALL TRAFIL(NF,FN)
C
C       OPEN AND READ FROM FILE
              OPEN(UNIT=35,ACCESS='DIRECT',FILE=LIBTRA//FN,
     1        FORM='UNFORMATTED',RECL=(80*NRECL),STATUS='UNKNOWN')
C
              CTAB=POINTS
              DO 2112 J=1,POINTS
                  READ(UNIT=35,REC=J) TABLE(J,1),TABLE(J,2),TABLE(J,3)
 2112         CONTINUE
              CALL CLOSE_FILE(35,1)
              F19=1
              IF(WC.EQ.'PLOTR') TORR=.FALSE.
              IF(WC.EQ.'PLOTT') TORR=.TRUE.
              CALL PLTABL
              RETURN
          END IF
          IF(WC.EQ.'PLOTR'.AND.DF1.EQ.0.OR.
     1    WC.EQ.'PLOTT'.AND.DF1.EQ.0) THEN
C       PLOT FILE NUMBER INT(W1) IF NOT EMPTY
C       FIND THE DESIRED FILE
C       ***************************************************************
              EXISJK=.FALSE.
              INQUIRE(FILE=LIBTRA//'TRA.DAT',EXIST=EXISJK)
              IF(.NOT.EXISJK) THEN
                  WRITE(OUTLYNE,*)'"TABLE FILES DO NOT YET EXIST'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'TO INITIALIZE IT, USE "ITF" AND "PROCEED"'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C       ***************************************************************
              OPEN(UNIT=35,ACCESS='DIRECT',FILE=LIBTRA//'TRA.DAT',
     1        FORM='UNFORMATTED',RECL=(50*NRECL),STATUS='UNKNOWN')
              READ(UNIT=35,REC=INT(W1))
     1        NM,DATA1,DATA2,POINTS,OCC,DDATE,TTIME
              DDDAT=DDATE
              TTTIM=TTIME
              IF(OCC.EQ.0) THEN
C       FILE EMPTY, PRINT MESSAGE AND RETURN
                  WRITE(OUTLYNE,*)'FILE # ',INT(W1),' IS EMPTY'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL CLOSE_FILE(35,1)
                  CALL MACFAL
                  RETURN
              ELSE
                  CALL CLOSE_FILE(35,1)
                  TNAME=NM
              END IF
C       LOAD FILE INTO TABLE ARRAY
C
              NF=INT(W1)
              CALL TRAFIL(NF,FN)
C
C       OPEN AND READ FROM FILE
              OPEN(UNIT=35,ACCESS='DIRECT',FILE=LIBTRA//FN,
     1        FORM='UNFORMATTED',RECL=(80*NRECL),STATUS='UNKNOWN')
C
              CTAB=POINTS
              DO 2122 J=1,POINTS
                  READ(UNIT=35,REC=J) TABLE(J,1),TABLE(J,2),TABLE(J,3)
 2122         CONTINUE
              CALL CLOSE_FILE(35,1)
              F19=1
              IF(WC.EQ.'PLOTR') TORR=.FALSE.
              IF(WC.EQ.'PLOTT') TORR=.TRUE.
              CALL PLTABL
              RETURN
          END IF
C       "PUNCH" COMMAND
          IF(WC.EQ.'PUNCH') THEN
              IF(SST.EQ.1.OR.S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
                  WRITE(OUTLYNE,*)
     1            '"PUNCH" ONLY TAKES QUALIFIER OR NUMERIC WORD #1 INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(SQ.EQ.1.AND.DF1.EQ.0) THEN
                  WRITE(OUTLYNE,*)
     1            '"PUNCH" TAKES EITHER QUALIFIER OR NUMERIC WORD 1 INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'BUT NOT BOTH'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
          IF(WC.EQ.'PUNCH'.AND.SQ.EQ.0.AND.DF1.EQ.1) THEN
              IF(F19.EQ.0) THEN
                  WRITE(OUTLYNE,*)'NO TABLE DATA IN MEMORY TO PRINT'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              ELSE
                  CALL PUTAB
                  RETURN
              END IF
          END IF
          IF(WC.EQ.'PUNCH'.AND.SQ.EQ.1.AND.DF1.EQ.1) THEN
C       SEARCH FOR FILE NAMED WQ AND IF FOUND, LOAD IT, THEN
C       PROCEED.
C       DATA STORED IN TRA.DAT IS:
C               FILE NAME (TNAME) CHARACTER *8
C               DATA1 STARTING WAVELENGTH (DOUBLE PRES.)
C               DATA2 ENDIG WAVLENGTH (DOUBLE PRES.)
C               POINTS - NUMBER OF DATA POINTS (INTEGER)
C               OCC- OCCUPANCY FLAG, 0=EMPTY, 1- FULL (INTEGER)
C               DDATE-DATE FILED
C               TTIME=TIME FILED
C       FIND THE DESIRED FILE
C       ***************************************************************
              EXISJK=.FALSE.
              INQUIRE(FILE=LIBTRA//'TRA.DAT',EXIST=EXISJK)
              IF(.NOT.EXISJK) THEN
                  WRITE(OUTLYNE,*)'"TABLE FILES DO NOT YET EXIST'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'TO INITIALIZE IT, USE "ITF" AND "PROCEED"'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C       ***************************************************************
              OPEN(UNIT=35,ACCESS='DIRECT',FILE=LIBTRA//'TRA.DAT',
     1        FORM='UNFORMATTED',RECL=(50*NRECL),STATUS='UNKNOWN')
C
              DO 2011 I=1,999
                  READ(UNIT=35,REC=I) NM,DATA1,DATA2,POINTS,OCC,DDATE,TTIME
                  DDDAT=DDATE
                  TTTIM=TTIME
                  IF(NM.EQ.WQ) THEN
C       FOUND FILE
                      TNAME=WQ
                      GO TO 3011
                  END IF
 2011         CONTINUE
C       IF YOU GOT HERE, FILE DID NOT EXIST
              WRITE(OUTLYNE,*)'FILE ',WQ,' DOES NOT EXIST'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL CLOSE_FILE(35,1)
              CALL MACFAL
              RETURN
 3011         CONTINUE
C       LOAD FILE INTO TABLE ARRAY
C
              NF=I
              CALL TRAFIL(NF,FN)
C
C       OPEN AND READ FROM FILE
              OPEN(UNIT=35,ACCESS='DIRECT',FILE=LIBTRA//FN,
     1        FORM='UNFORMATTED',RECL=(80*NRECL),STATUS='UNKNOWN')
C
              CTAB=POINTS
              DO 2111 J=1,POINTS
                  READ(UNIT=35,REC=J) TABLE(J,1),TABLE(J,2),TABLE(J,3)
 2111         CONTINUE
              CALL CLOSE_FILE(35,1)
              F19=1
              CALL PUTAB
              RETURN
          END IF
          IF(WC.EQ.'PUNCH'.AND.SQ.EQ.0.AND.DF1.EQ.0) THEN
C       LIST FILE NUMBER INT(W1) IF NOT EMPTY
C       FIND THE DESIRED FILE
C       ***************************************************************
              EXISJK=.FALSE.
              INQUIRE(FILE=LIBTRA//'TRA.DAT',EXIST=EXISJK)
              IF(.NOT.EXISJK) THEN
                  WRITE(OUTLYNE,*)'"TABLE FILES DO NOT YET EXIST'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'TO INITIALIZE IT, USE "ITF" AND "PROCEED"'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C       ***************************************************************
              OPEN(UNIT=35,ACCESS='DIRECT',FILE=LIBTRA//'TRA.DAT',
     1        FORM='UNFORMATTED',RECL=(50*NRECL),STATUS='UNKNOWN')
              READ(UNIT=35,REC=INT(W1))
     1        NM,DATA1,DATA2,POINTS,OCC,DDATE,TTIME
              DDDAT=DDATE
              TTTIM=TTIME
              IF(OCC.EQ.0) THEN
C       FILE EMPTY, PRINT MESSAGE AND RETURN
                  WRITE(OUTLYNE,*)'FILE # ',INT(W1),' IS EMPTY'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL CLOSE_FILE(35,1)
                  CALL MACFAL
                  RETURN
              ELSE
                  CALL CLOSE_FILE(35,1)
                  TNAME=NM
              END IF
C       LOAD FILE INTO TABLE ARRAY
C
              NF=INT(W1)
              CALL TRAFIL(NF,FN)
C
C       OPEN AND READ FROM FILE
              OPEN(UNIT=35,ACCESS='DIRECT',FILE=LIBTRA//FN,
     1        FORM='UNFORMATTED',RECL=(80*NRECL),STATUS='UNKNOWN')
C
              CTAB=POINTS
              DO 2121 J=1,POINTS
                  READ(UNIT=35,REC=J) TABLE(J,1),TABLE(J,2),TABLE(J,3)
 2121         CONTINUE
              CALL CLOSE_FILE(35,1)
              F19=1
              CALL PUTAB
              RETURN
          END IF
C       "NAME" COMMAND
          IF(WC.EQ.'NAME') THEN
              IF(SST.EQ.1.OR.SN.EQ.1) THEN
                  WRITE(OUTLYNE,*)'"NAME" ONLY TAKES QUALIFIER WORD INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
          IF(WC.EQ.'NAME'.AND.SQ.EQ.0) THEN
              IF(F19.EQ.0) THEN
                  WRITE(OUTLYNE,*)'NO TABLE DATA IN MEMORY TO PRINT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              ELSE
                  WRITE(OUTLYNE,1200) TNAME
                  CALL SHOWIT(0)
 1200             FORMAT('THE CURRENT SPECT TABLE FILE IN MEMORY IS ( '
     1            ,A8,' )')
                  RETURN
              END IF
          END IF
C       "FILE" COMMAND
          IF(WC.EQ.'FILE') THEN
              IF(SST.EQ.1.OR.SN.EQ.1) THEN
                  WRITE(OUTLYNE,*)'"FILE" ONLY TAKES QUALIFIER WORD INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
          IF(WC.EQ.'FILE'.AND.SQ.EQ.0) THEN
              WRITE(OUTLYNE,*)'"FILE" REQUIRES A FILE NAME QUALIFIER'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(WC.EQ.'FILE'.AND.SQ.EQ.1) THEN
C       PROCEED.
C       CHECK IF TABLE MEMORY DATA EXISTS
              IF(F19.EQ.0) THEN
                  WRITE(OUTLYNE,*)'NO TABLE DATA IN MEMORY TO FILE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C
              TNAME=WQ
              CALL TFILE(TNAME)
              RETURN
          END IF
C       "PUT" COMMAND
          IF(WC.EQ.'PUT') THEN
              IF(SST.EQ.1.OR.SN.EQ.1) THEN
                  WRITE(OUTLYNE,*)'"PUT" ONLY TAKES QUALIFIER WORD INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
          IF(WC.EQ.'PUT'.AND.SQ.EQ.0) THEN
              WRITE(OUTLYNE,*)'"PUT" REQUIRES A FILE NAME QUALIFIER'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(WC.EQ.'PUT'.AND.SQ.EQ.1) THEN
C       PROCEED.
              CALL PFILE
              RETURN
          END IF
C       "RENAME" COMMAND
          IF(WC.EQ.'RENAME') THEN
              IF(SN.EQ.1) THEN
                  WRITE(OUTLYNE,*)'"RENAME" TAKES NO NUMERIC INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
          IF(WC.EQ.'RENAME'.AND.SQ.EQ.0.OR.WC.EQ.'RENAME'
     1    .AND.SST.EQ.0) THEN
              WRITE(OUTLYNE,*)
     1        '"RENAME" REQUIRES BOTH QUALIFIER AND STRING INPUT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(WC.EQ.'RENAME'.AND.SQ.EQ.1.AND.SST.EQ.1) THEN
C       SEARCH FOR FILE NAMED WQ AND IF FOUND, CHANGE ITS NAME
C       TO WS(1:8), THEN
C       PROCEED.
C       FIND THE DESIRED FILE
C       ***************************************************************
              EXISJK=.FALSE.
              INQUIRE(FILE=LIBTRA//'TRA.DAT',EXIST=EXISJK)
              IF(.NOT.EXISJK) THEN
                  WRITE(OUTLYNE,*)'"TABLE FILES DO NOT YET EXIST'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'TO INITIALIZE IT, USE "ITF" AND "PROCEED"'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C       ***************************************************************
              OPEN(UNIT=35,ACCESS='DIRECT',FILE=LIBTRA//'TRA.DAT',
     1        FORM='UNFORMATTED',RECL=(50*NRECL),STATUS='UNKNOWN')
              DO 611 I=1,999
                  READ(UNIT=35,REC=I) NM,DATA1,DATA2,POINTS,OCC,DDATE,TTIME
                  DDDAT=DDATE
                  TTTIM=TTIME
                  IF(NM.EQ.WS(1:8)) THEN
C       FOUND FILE WHICH HAS RENAMING NAME ALREADY
C       DON'T PROCEED WITH RENAMING
                      WRITE(OUTLYNE,*)'RENAMING NAME ALREADY IN USE.'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'PICK A NEW NAME AND RE-ISSUE COMMAND.'
                      CALL SHOWIT(1)
                      CALL CLOSE_FILE(35,1)
                      CALL MACFAL
                      RETURN
                  END IF
 611          CONTINUE
              DO 601 I=1,999
                  READ(UNIT=35,REC=I) NM,DATA1,DATA2,POINTS,OCC,DDATE,TTIME
                  DDDAT=DDATE
                  TTTIM=TTIME
                  IF(NM.EQ.WQ) THEN
C       FOUND FILE TO RENAME
                      NM=WS(1:8)
                      WRITE(UNIT=35,REC=I) NM,DATA1,DATA2,POINTS,OCC,DDATE,TTIME
                      WRITE(OUTLYNE,*)'FILE ( ',WQ,' ) RENAMED AS ( ',WS(1:8),' )'
                      CALL SHOWIT(1)
                      TNAME=WS(1:8)
                      CALL CLOSE_FILE(35,1)
                      RETURN
                  END IF
 601          CONTINUE
C       IF YOU GOT HERE, FILE DID NOT EXIST
              WRITE(OUTLYNE,*)'FILE ',WQ,' DOES NOT EXIST'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              CALL CLOSE_FILE(35,1)
              RETURN
          END IF
C       "DROP" COMMAND
          IF(WC.EQ.'DROP') THEN
              IF(SST.EQ.1.OR.S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
                  WRITE(OUTLYNE,*)
     1            '"DROP" ONLY TAKES QUALIFIER AND NUMERIC WORD #1 INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(SQ.EQ.0) THEN
                  WRITE(OUTLYNE,*)
     1            '"DROP" REQUIRES AN EXPLICIT FILE NAME QUALIFIER WORD'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(DF1.EQ.1) THEN
                  WRITE(OUTLYNE,*)
     1            '"DROP" REQUIRES AN EXPLICIT NUMERIC WORD #1 ENTRY TO DELETE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              CALL DROP
              RETURN
          END IF
C       "INTER" COMMAND
          IF(WC.EQ.'INTER') THEN
              IF(SST.EQ.1.OR.S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
                  WRITE(OUTLYNE,*)
     1            '"INTER" ONLY TAKES QUALIFIER AND NUMERIC WORD #1 INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(SQ.EQ.0) THEN
                  WRITE(OUTLYNE,*)
     1            '"INTER" REQUIRES AN EXPLICIT FILE NAME QUALIFIER WORD'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(W1.LT.0.0D0) THEN
                  WRITE(OUTLYNE,*)
     1            'INTERPOLATION TO OR BELOW ZERO WAVELENGTH IS NOT ALLOWED'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(DF1.EQ.1) THEN
                  WRITE(OUTLYNE,*)
     1            '"INTER" REQUIRES AN EXPLICIT NUMERIC WORD #1 WAVELENGTH VALUE.'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              CALL INTER
              RETURN
          END IF
C
          IF(WC.EQ.'INT') THEN
              IF(SQ.EQ.1.OR.SST.EQ.1.OR.SN.EQ.1) THEN
                  WRITE(OUTLYNE,*)'"INT" TAKES NO ADDITIONAL INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C       INTEGRATE THE CUMULATIVE AREA USING SIMPSON'S RULE
C       AND PLACE THE VALUE OF THE INTEGRAL IN THE ACCUMULATOR
C       (THE X OR ACC REGISTER). LASTX WILL BE UPDATED.
C
              CALL INTSMP
C
              RETURN
          END IF
C       "INSERT" COMMAND
          IF(WC.EQ.'INSERT') THEN
              IF(SST.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
                  WRITE(OUTLYNE,*)
     1            '"INSERT" ONLY TAKES QULIFIER AND NUMERIC WORD #1 AND #2 INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(SQ.EQ.0) THEN
                  WRITE(OUTLYNE,*)
     1            '"INSERT" REQUIRES AN EXPLICIT FILE NAME QULIFIER WORD'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(W1.LT.0.0D0.OR.W2.LT.0.0D0) THEN
                  WRITE(OUTLYNE,*)
     1            'A NEGATIVE WAVELENGTH VALUE IS NOT ALLOWED'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(DF1.EQ.1.OR.DF2.EQ.1) THEN
                  WRITE(OUTLYNE,*)
     1            '"INSERT" REQUIRES EXPLICIT NUMERIC WORD #1 AND #2 INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              CALL SINSERT
              RETURN
          END IF
C                       RETURN
      END
      SUBROUTINE PHOTOPIC
          IMPLICIT NONE
          INCLUDE 'datmai.inc'
C     SAVE I/O
          SAVE_KDP(1)=SAVEINPT(1)
          INPUT='TABLE PHOTOPIC'
          CALL PROCES
          INPUT='DATA .380 0.0000'
          CALL PROCES
          INPUT='DATA .385 0.0001'
          CALL PROCES
          INPUT='DATA .390 0.0001'
          CALL PROCES
          INPUT='DATA .395 0.0002'
          CALL PROCES
          INPUT='DATA .400 0.0004'
          CALL PROCES
          INPUT='DATA .405 0.0006'
          CALL PROCES
          INPUT='DATA .410 0.0012'
          CALL PROCES
          INPUT='DATA .415 0.0022'
          CALL PROCES
          INPUT='DATA .420 0.0040'
          CALL PROCES
          INPUT='DATA .425 0.0073'
          CALL PROCES
          INPUT='DATA .430 0.0116'
          CALL PROCES
          INPUT='DATA .435 0.0168'
          CALL PROCES
          INPUT='DATA .440 0.0230'
          CALL PROCES
          INPUT='DATA .445 0.0298'
          CALL PROCES
          INPUT='DATA .450 0.0380'
          CALL PROCES
          INPUT='DATA .455 0.0480'
          CALL PROCES
          INPUT='DATA .460 0.0600'
          CALL PROCES
          INPUT='DATA .465 0.0739'
          CALL PROCES
          INPUT='DATA .470 0.0910'
          CALL PROCES
          INPUT='DATA .475 0.1126'
          CALL PROCES
          INPUT='DATA .480 0.1390'
          CALL PROCES
          INPUT='DATA .485 0.1693'
          CALL PROCES
          INPUT='DATA .490 0.2080'
          CALL PROCES
          INPUT='DATA .495 0.2586'
          CALL PROCES
          INPUT='DATA .500 0.3230'
          CALL PROCES
          INPUT='DATA .505 0.4073'
          CALL PROCES
          INPUT='DATA .510 0.5030'
          CALL PROCES
          INPUT='DATA .515 0.6082'
          CALL PROCES
          INPUT='DATA .520 0.7100'
          CALL PROCES
          INPUT='DATA .525 0.7932'
          CALL PROCES
          INPUT='DATA .530 0.8620'
          CALL PROCES
          INPUT='DATA .535 0.9149'
          CALL PROCES
          INPUT='DATA .540 0.9540'
          CALL PROCES
          INPUT='DATA .545 0.9803'
          CALL PROCES
          INPUT='DATA .550 0.9950'
          CALL PROCES
          INPUT='DATA .555 1.0002'
          CALL PROCES
          INPUT='DATA .560 0.9950'
          CALL PROCES
          INPUT='DATA .565 0.9786'
          CALL PROCES
          INPUT='DATA .570 0.9520'
          CALL PROCES
          INPUT='DATA .575 0.9154'
          CALL PROCES
          INPUT='DATA .580 0.8700'
          CALL PROCES
          INPUT='DATA .585 0.8163'
          CALL PROCES
          INPUT='DATA .590 0.7570'
          CALL PROCES
          INPUT='DATA .595 0.6949'
          CALL PROCES
          INPUT='DATA .600 0.6310'
          CALL PROCES
          INPUT='DATA .605 0.5668'
          CALL PROCES
          INPUT='DATA .610 0.5030'
          CALL PROCES
          INPUT='DATA .615 0.4412'
          CALL PROCES
          INPUT='DATA .620 0.3810'
          CALL PROCES
          INPUT='DATA .625 0.3210'
          CALL PROCES
          INPUT='DATA .630 0.2650'
          CALL PROCES
          INPUT='DATA .635 0.2170'
          CALL PROCES
          INPUT='DATA .640 0.1750'
          CALL PROCES
          INPUT='DATA .645 0.1382'
          CALL PROCES
          INPUT='DATA .650 0.1070'
          CALL PROCES
          INPUT='DATA .655 0.0816'
          CALL PROCES
          INPUT='DATA .660 0.0610'
          CALL PROCES
          INPUT='DATA .665 0.0446'
          CALL PROCES
          INPUT='DATA .670 0.0320'
          CALL PROCES
          INPUT='DATA .675 0.0232'
          CALL PROCES
          INPUT='DATA .680 0.0170'
          CALL PROCES
          INPUT='DATA .685 0.0119'
          CALL PROCES
          INPUT='DATA .690 0.0082'
          CALL PROCES
          INPUT='DATA .695 0.0057'
          CALL PROCES
          INPUT='DATA .700 0.0041'
          CALL PROCES
          INPUT='DATA .705 0.0029'
          CALL PROCES
          INPUT='DATA .710 0.0021'
          CALL PROCES
          INPUT='DATA .715 0.0015'
          CALL PROCES
          INPUT='DATA .720 0.0010'
          CALL PROCES
          INPUT='DATA .725 0.0007'
          CALL PROCES
          INPUT='DATA .730 0.0005'
          CALL PROCES
          INPUT='DATA .735 0.0004'
          CALL PROCES
          INPUT='DATA .740 0.0003'
          CALL PROCES
          INPUT='DATA .745 0.0002'
          CALL PROCES
          INPUT='DATA .750 0.0001'
          CALL PROCES
          INPUT='DATA .755 0.0001'
          CALL PROCES
          INPUT='DATA .760 0.0001'
          CALL PROCES
          INPUT='DATA .765 0.0000'
          CALL PROCES
          INPUT='DATA .770 0.0000'
          CALL PROCES
          INPUT='DATA .775 0.0000'
          CALL PROCES
          INPUT='DATA .780 0.0000'
          CALL PROCES
          INPUT='ENDTABLE'
          CALL PROCES
C     RETORE I/O
          REST_KDP(1)=RESTINPT(1)
          RETURN
      END
      SUBROUTINE SCOTOPIC
          IMPLICIT NONE
          INCLUDE 'datmai.inc'
C     SAVE I/O
          SAVE_KDP(1)=SAVEINPT(1)
          INPUT='TABLE SCOTOPIC'
          CALL PROCES
          INPUT='DATA .380 0.00059'
          CALL PROCES
          INPUT='DATA .385 0.00111'
          CALL PROCES
          INPUT='DATA .390 0.00221'
          CALL PROCES
          INPUT='DATA .395 0.00453'
          CALL PROCES
          INPUT='DATA .400 0.00929'
          CALL PROCES
          INPUT='DATA .405 0.01850'
          CALL PROCES
          INPUT='DATA .410 0.03484'
          CALL PROCES
          INPUT='DATA .415 0.0604'
          CALL PROCES
          INPUT='DATA .420 0.0966'
          CALL PROCES
          INPUT='DATA .425 0.1436'
          CALL PROCES
          INPUT='DATA .430 0.1998'
          CALL PROCES
          INPUT='DATA .435 0.2625'
          CALL PROCES
          INPUT='DATA .440 0.3281'
          CALL PROCES
          INPUT='DATA .445 0.3931'
          CALL PROCES
          INPUT='DATA .450 0.4550'
          CALL PROCES
          INPUT='DATA .455 0.5129'
          CALL PROCES
          INPUT='DATA .460 0.5672'
          CALL PROCES
          INPUT='DATA .465 0.6205'
          CALL PROCES
          INPUT='DATA .470 0.6756'
          CALL PROCES
          INPUT='DATA .475 0.7337'
          CALL PROCES
          INPUT='DATA .480 0.7930'
          CALL PROCES
          INPUT='DATA .485 0.8509'
          CALL PROCES
          INPUT='DATA .490 0.9043'
          CALL PROCES
          INPUT='DATA .495 0.9491'
          CALL PROCES
          INPUT='DATA .500 0.9817'
          CALL PROCES
          INPUT='DATA .505 0.9984'
          CALL PROCES
          INPUT='DATA .510 0.9966'
          CALL PROCES
          INPUT='DATA .515 0.9750'
          CALL PROCES
          INPUT='DATA .520 0.9352'
          CALL PROCES
          INPUT='DATA .525 0.8796'
          CALL PROCES
          INPUT='DATA .530 0.8110'
          CALL PROCES
          INPUT='DATA .535 0.7332'
          CALL PROCES
          INPUT='DATA .540 0.6497'
          CALL PROCES
          INPUT='DATA .545 0.5644'
          CALL PROCES
          INPUT='DATA .550 0.4808'
          CALL PROCES
          INPUT='DATA .555 0.4015'
          CALL PROCES
          INPUT='DATA .560 0.3288'
          CALL PROCES
          INPUT='DATA .565 0.2639'
          CALL PROCES
          INPUT='DATA .570 0.2076'
          CALL PROCES
          INPUT='DATA .575 0.1602'
          CALL PROCES
          INPUT='DATA .580 0.1212'
          CALL PROCES
          INPUT='DATA .585 0.0899'
          CALL PROCES
          INPUT='DATA .590 0.0655'
          CALL PROCES
          INPUT='DATA .595 0.0469'
          CALL PROCES
          INPUT='DATA .600 0.03325'
          CALL PROCES
          INPUT='DATA .605 0.02312'
          CALL PROCES
          INPUT='DATA .610 0.01593'
          CALL PROCES
          INPUT='DATA .615 0.01088'
          CALL PROCES
          INPUT='DATA .620 0.00737'
          CALL PROCES
          INPUT='DATA .625 0.00497'
          CALL PROCES
          INPUT='DATA .630 0.003335'
          CALL PROCES
          INPUT='DATA .635 0.002235'
          CALL PROCES
          INPUT='DATA .640 0.001497'
          CALL PROCES
          INPUT='DATA .645 0.001005'
          CALL PROCES
          INPUT='DATA .650 0.000677'
          CALL PROCES
          INPUT='DATA .655 0.000459'
          CALL PROCES
          INPUT='DATA .660 0.0003129'
          CALL PROCES
          INPUT='DATA .665 0.0002147'
          CALL PROCES
          INPUT='DATA .670 0.0001480'
          CALL PROCES
          INPUT='DATA .675 0.0001026'
          CALL PROCES
          INPUT='DATA .680 0.0000716'
          CALL PROCES
          INPUT='DATA .685 0.0000502'
          CALL PROCES
          INPUT='DATA .690 0.00003533'
          CALL PROCES
          INPUT='DATA .695 0.00002502'
          CALL PROCES
          INPUT='DATA .700 0.00001780'
          CALL PROCES
          INPUT='DATA .705 0.00001273'
          CALL PROCES
          INPUT='DATA .710 0.00000914'
          CALL PROCES
          INPUT='DATA .715 0.00000660'
          CALL PROCES
          INPUT='DATA .720 0.00000478'
          CALL PROCES
          INPUT='DATA .725 0.000003482'
          CALL PROCES
          INPUT='DATA .730 0.000002546'
          CALL PROCES
          INPUT='DATA .735 0.000001870'
          CALL PROCES
          INPUT='DATA .740 0.000001379'
          CALL PROCES
          INPUT='DATA .745 0.000001022'
          CALL PROCES
          INPUT='DATA .750 0.000000760'
          CALL PROCES
          INPUT='DATA .755 0.000000567'
          CALL PROCES
          INPUT='DATA .760 0.000000425'
          CALL PROCES
          INPUT='DATA .765 0.000000320'
          CALL PROCES
          INPUT='DATA .770 0.000000241'
          CALL PROCES
          INPUT='DATA .775 0.000000183'
          CALL PROCES
          INPUT='DATA .780 0.000000139'
          CALL PROCES
          INPUT='ENDTABLE'
          CALL PROCES
C     RETORE I/O
          REST_KDP(1)=RESTINPT(1)
          RETURN
      END
C SUB SPDEL.FOR
      SUBROUTINE SPDEL
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE SPDEL. THIS IS THE SUBROUTINE WHICH
C       HANDLES THE "DEL"
C       COMMAND FROM SPSRF OR UPDATE SPSRF LEVEL
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
C
          IF(SST.EQ.1.OR.SQ.EQ.1.OR.S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1
     1    .OR.S5.EQ.1) THEN
              OUTLYNE='" '//WC//' " ONLY TAKES NUMERIC WORD #1 INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER DATA'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(DF1.EQ.1) THEN
              OUTLYNE=
     1        '" '//WC//' " REQUIRES EXPLICIT NUMERIC WORD #1 INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C       PROCEED WITH SPSRF DEL
C
          IF(W1.EQ.0.0) THEN
              OUTLYNE='SPECIAL SURFACE TYPE NOT VALID ON OBJECT SURFACE'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(W1.LT.0.0D0) W1=SYSTEM1(20)+W1
          IF(INT(W1).LT.1.OR.INT(W1).GT.INT(SYSTEM1(20))) THEN
              OUTLYNE=
     1        'SURFACE NUMBER BEYOND LEGAL RANGE'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(ALENS(34,INT(W1)).EQ.0.0D0) THEN
              WRITE(OUTLYNE,*)
     1          'WARNING: SURFACE ',INT(W1),' NOT A SPECIAL SURFACE TYPE'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              RETURN
          END IF
          IF(ALENS(34,INT(W1)).NE.0.0D0) THEN
              ALENS(34,INT(W1))=0.0D0
              WRITE(OUTLYNE,*)
     1          'SPECIAL SURFACE DEFINITION REMOVED FROM SURFACE ',INT(W1)
              CALL SHOWIT(1)
          END IF
          RETURN
      END
