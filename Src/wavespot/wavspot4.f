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

C       FOURTH FILE OF CAPFN/SPOT ROUTINES

C SUB SPFAIL.FOR
      SUBROUTINE SPFAIL
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE SPFAIL.FOR.
C
          INTEGER NS1,NS2,NIGNORE
C
          INTEGER I
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datspd.inc'
          INCLUDE 'datsp1.inc'
C
          IF(SQ.EQ.1.OR.SST.EQ.1) THEN
              OUTLYNE=
     1        '"FAIL" OR "FAILACC" TAKE NO QUALIFIER OR STRING INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C       DEFAULT NUMERIC INPUT
          IF(DF1.EQ.1) W1=DBLE(NEWOBJ)
          IF(DF1.EQ.1.AND.DF2.EQ.1) W2=DBLE(NEWIMG)
          IF(DF1.EQ.0.AND.DF2.EQ.1) W2=W1
          IF(DF1.EQ.1) DF1=0
          IF(DF2.EQ.1) DF2=0
          IF(S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1)THEN
              OUTLYNE=
     1        '"FAIL" OR "FAILACC"'
              CALL SHOWIT(1)
              OUTLYNE='TAKES NO NUMERIC WORD #3 THROUGH #5 INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
C       CHECK FOR VALID SURFACE NUMBERS
          IF(INT(W1).LT.NEWOBJ) THEN
              OUTLYNE=
     1        'NUMERIC WORD #1 SURFACE NUMBER BEYOND LEGAL BOUNDS'
              CALL SHOWIT(1)
              OUTLYNE=
     1        'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              RETURN
          END IF
          IF(DF2.EQ.0) THEN
              IF(INT(W2).GT.NEWIMG) THEN
                  OUTLYNE=
     1            'NUMERIC WORD #2 SURFACE NUMBER BEYOND LEGAL BOUNDS'
                  CALL SHOWIT(1)
                  OUTLYNE=
     1            'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  RETURN
              END IF
              IF(INT(W2).LT.INT(W1)) THEN
                  OUTLYNE=
     1            'SURFACE NUMBERS OUT OF ORDER FOR THE CURRENT COMMAND'
                  CALL SHOWIT(1)
                  OUTLYNE=
     1            'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  RETURN
              END IF
          ELSE
C       W2 IS DEFAULT AND NOT USED
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
C
C       THE NUMBER OF RAYS WHICH REPRESENT 100% OF THE ENERGY
C       EQUAL THE NUMBER OF RAYS TRACED (IWIW) TIMES THE MUNBER
C       OF NON-ZERO SPECTRAL WEIGHTS.
C
          NFAIL=0
          NIGNORE=0
          NS1=INT(W1)
          NS2=INT(W2)
          DO I=2,ITOT
C     LOAD DSPOT(*) WITH DSPOTT(*,ID)
              ID=I-1
              CALL SPOTIT(4)
              IF(DSPOT(7).NE.0.0D0.AND.DSPOT(36).NE.0.0D0) THEN
                  NIGNORE=NIGNORE+1
              END IF
              IF(DSPOT(7).NE.0.0D0.AND.DSPOT(36).EQ.0.0D0) THEN
C     FOUND FAILED RAY
                  IF(INT(DSPOT(8)).GE.NS1.AND.INT(DSPOT(8))
     1            .LE.NS2) NFAIL=NFAIL+1
              ELSE
C       RAY FAILED, DONT DO STATS
              END IF
          END DO
C
          IF(WC.EQ.'FAILACC') THEN
C       NOW SET THE GENERAL PURPOSE REGISTER #1 TO THE
C       REAL*8 REPRESENTATION OF THE TOTAL NUMBER OF FAILED
C       RAYS
              GPREG(1)=DBLE(NFAIL)
              GPREG(2)=DBLE(NIGNORE)
          ELSE
          END IF
 105      FORMAT('            AT SURFACE = ',2X,I3)
 106      FORMAT('          FROM SURFACE = ',2X,I3)
 107      FORMAT('            TO SURFACE = ',2X,I3)
 108      FORMAT(' NUMBER OF FAILED RAYS = ',I10)
! 109    FORMAT('NUMBER OF IGNORED RAYS = ',I10)
          IF(WC.EQ.'FAIL') THEN
C       DO THE PRINT OUT
              IF(W1.EQ.W2) THEN
C       JUST ONE SURFACE
                  WRITE(OUTLYNE,105) NS1
                  CALL SHOWIT(0)
              ELSE
C       BOTH SURFACES
                  WRITE(OUTLYNE,106) NS1
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,107) NS2
                  CALL SHOWIT(0)
              END IF
              WRITE(OUTLYNE,108) NFAIL
              CALL SHOWIT(0)
              WRITE(OUTLYNE,108) NIGNORE
              CALL SHOWIT(0)
          ELSE
          END IF
          RETURN
      END
C SUB SPDQAL.FOR

      SUBROUTINE SPDQAL
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE SPDQAL DOES SPD WITH QUALIFIER WORDS.
C     VALID QUALIFIERS ARE MOVE
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datspd.inc'
          INCLUDE 'datsp1.inc'
C
          IF(WC.EQ.'FAIL'.OR.WC.EQ.'FAILACC') THEN
              CALL SPFAIL
              RETURN
          ELSE
          END IF
          IF(WC.EQ.'SPD') THEN
              IF(.NOT.SPDEXT) THEN
                  OUTLYNE='NO SPOT DIAGRAM EXISTS'
                  CALL SHOWIT(1)
                  OUTLYNE='USE "FOB" AND "SPD (GRID SIZE)" TO GENERATE'
                  CALL SHOWIT(1)
                  OUTLYNE='A SPOT DIAGRAM, THEN RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
          IF(WQ.EQ.'MOVE'.OR.WQ.EQ.'MOVEACC') THEN
              CALL SPMOVE
              RETURN
          ELSE
          END IF
          IF(WC.EQ.'SPDSAVE'.OR.WC.EQ.'SPDADD') THEN
              CALL SPSAVE
              RETURN
          ELSE
          END IF
          IF(WC.EQ.'SPDSTATS') THEN
              CALL SPSTAT
              RETURN
          ELSE
          END IF
          RETURN
      END
C SUB RSPH.FOR
      SUBROUTINE RSPH
C
          IMPLICIT NONE
C
C       THIS CONTROLS THE OPERATION OF THE "RSPH (CHIEF or NOTILT or BEST)
C       COMMAND
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          INCLUDE 'datspd.inc'
C
          IF(STI.EQ.1) THEN
              IF(REFLOC.EQ.1) THEN
C     CHIEF IS SET
 100              FORMAT(
     1            'SPOT DIAGRAM REFERNCE SPHERE CENTER LIES ON THE CHIEF RAY')
                  WRITE(OUTLYNE,100)
                  CALL SHOWIT(0)
              END IF
              IF(REFLOC.EQ.3) THEN
C     CENT IS SET
 300              FORMAT(
     1            'SPOT DIAGRAM REFERENCE SPHERE CENTER REMOVES TILT')
                  WRITE(OUTLYNE,300)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,305)DLLX
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,306)DLLY
                  CALL SHOWIT(0)
              END IF
              IF(REFLOC.EQ.4) THEN
C     CENT IS SET
 400              FORMAT(
     1            'SPOT DIAGRAM REFERENCE SPHERE CENTER REMOVES TILT AND FOCUS')
                  WRITE(OUTLYNE,400)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,305)DLLX
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,306)DLLY
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,307)DLLZ
                  CALL SHOWIT(0)
              END IF
              RETURN
          END IF
          IF(SST.EQ.1.OR.SN.EQ.1) THEN
              WRITE(OUTLYNE,*)'"RSPH" TAKES NO STRING OR NUMERIC INPUT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              RETURN
          END IF
          IF(SQ.EQ.1) THEN
              IF(WQ.NE.'CHIEF'.AND.WQ.NE.'NOTILT'.AND.
     1        WQ.NE.'BEST') THEN
                  WRITE(OUTLYNE,*)'INVALID QUALIFIER USED WITH "RSPH"'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  RETURN
              END IF
          END IF
          IF(SQ.EQ.0.AND.STI.EQ.0) THEN
              WRITE(OUTLYNE,*)'"RSPH" REQUIRES EXPLICIT QUALIFIER INPUT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              RETURN
          END IF
          IF(WQ.EQ.'CHIEF') THEN
              REFLOC=1
              DLLX=0.0D0
              DLLY=0.0D0
              DLLZ=0.0D0
 301          FORMAT(
     1        'SPOT DIAGRAM REFERENCE SPHERE CENTER WILL LIE ON THE CHIEF RAY')
              WRITE(OUTLYNE,301)
              CALL SHOWIT(0)
              RETURN
          END IF
          IF(WQ.EQ.'NOTILT') THEN
              REFLOC=3
              DLLX=0.0D0
              DLLY=0.0D0
              DLLZ=0.0D0
 601          FORMAT(
     1        'SPOT DIAGRAM REFERENCE SPHERE CENTER REMOVES TILT')
              WRITE(OUTLYNE,601)
              CALL SHOWIT(0)
              RETURN
          END IF
          IF(WQ.EQ.'BEST') THEN
              REFLOC=4
              DLLX=0.0D0
              DLLY=0.0D0
              DLLZ=0.0D0
 701          FORMAT(
     1        'SPOT DIAGRAM REFERENCE SPHERE CENTER REMOVES TILT AND FOCUS')
              WRITE(OUTLYNE,701)
              CALL SHOWIT(0)
              RETURN
          END IF
          RETURN
 305      FORMAT('CURRENT REF. SPHERE X-DISPLACEMENT = ',G18.10)
 306      FORMAT('CURRENT REF. SPHERE Y-DISPLACEMENT = ',G18.10)
 307      FORMAT('CURRENT REF. SPHERE Z-DISPLACEMENT = ',G18.10)
      END


      SUBROUTINE REDSUM(LARGER,M1,N1,IIRED)
          USE GLOBALS
          IMPLICIT NONE
          INTEGER I,M1,N1,IIRED
!      LOGICAL OPEN62
          INCLUDE 'datmai.inc'
          INCLUDE 'datspd.inc'
          INCLUDE 'datsp1.inc'
C
          REAL*8 LARGER
          DIMENSION LARGER(M1,N1)
C
          IRED=IIRED
          IRED=IRED-1
          DO I=1,IRED
              LARGER(I+1,1)=LARGER(I,1)+LARGER(I+1,1)
          END DO
          IRED=IRED+1
          DO I=2,IRED
              REDDAT1(I)=LARGER(I-1,1)
              REDDAT2(I)=LARGER(I-1,2)
          END DO
          IIRED=IRED
          RETURN
      END


      SUBROUTINE REDSUMS(LARGER,M1,N1)
          USE GLOBALS
          IMPLICIT NONE
          INTEGER I,J,M1,N1,ALLOERR
          REAL*8 MAX50,LEN,MAXX,MAXY,MAXI
!      LOGICAL OPEN62
          INCLUDE 'datmai.inc'
          INCLUDE 'datspd.inc'
          INCLUDE 'datsp1.inc'
C
          REAL*8 LARGER,RED2,COUNTIT
C
          DIMENSION LARGER(M1,N1),COUNTIT(:,:)
          ALLOCATABLE :: COUNTIT
          DEALLOCATE(COUNTIT,STAT=ALLOERR)
C
          DO I=2,IRED
              LARGER(I-1,1)=REDDAT1(I)
              LARGER(I-1,2)=REDDAT2(I)
              LARGER(I-1,3)=REDDAT3(I)
          END DO
C     THE LARGER ARRAY IS IN STORAGE
C     WHAT IS THE LARGEST X AND Y VALUE
          MAXX=-1.0D+300
          MAXY=-1.0D+300
C
          IRED=IRED-1
          DEALLOCATE(COUNTIT,STAT=ALLOERR)
          ALLOCATE(COUNTIT(IRED,2),STAT=ALLOERR)
          I=IRED
          COUNTIT(1:I,1:2)=0.0D0
          RED2=0.0D0
          DO I=1,IRED
              IF(DABS(LARGER(I,2)).GE.MAXX) MAXX=DABS(LARGER(I,2))
              IF(DABS(LARGER(I,3)).GE.MAXY) MAXY=DABS(LARGER(I,3))
          END DO
C
          IF(MAXX.GE.MAXY) MAXI=MAXX
          IF(MAXX.LT.MAXY) MAXI=MAXY
C     MAXI IS THE SEMI-DIMENSION OF THE SQUARE WHICH ENSQUARES
C     ALL THE ENERGY. DIVIDE IT INTO IRED PARTS
          MAX50=MAXI/DBLE(IRED)
C
          LEN=0.0D0
          DO I=1,IRED
              IF(LEN.EQ.0.0D0) COUNTIT(I,1)=0.0D0
              IF(LEN.EQ.0.0D0) COUNTIT(I,2)=0.0D0
              IF(LEN.GT.0.0D0) THEN
                  DO J=1,IRED
C     CALCULATE THE AMOUNT OF ENERGY INSIDE IT
                      IF(DABS(LARGER(J,2)).LE.LEN.AND.DABS(LARGER(J,3)).LE.LEN) THEN
C     POINT IS INSIDE THE CURRENT SQUARE
                          COUNTIT(I,2)=2.0D0*LEN
                          COUNTIT(I,1)=COUNTIT(I,1)+LARGER(J,1)
                      END IF
                  END DO
              END IF
              LEN=LEN+MAX50
          END DO
          IRED=IRED+1
          DO I=2,IRED
              REDDAT1(I)=COUNTIT(I-1,1)
              REDDAT2(I)=COUNTIT(I-1,2)
              REDDAT3(I)=COUNTIT(I-1,3)
          END DO
          DEALLOCATE(COUNTIT,STAT=ALLOERR)
          RETURN
      END


      SUBROUTINE REDSOR(N,LARGER,M1,N1,IIRED)
          USE GLOBALS
          IMPLICIT NONE
          REAL*8 RRA,RRB
!      LOGICAL OPEN62
          INTEGER L,N,J,IR,I,M1,N1,IIRED
          INCLUDE 'datmai.inc'
          INCLUDE 'datspd.inc'
          INCLUDE 'datsp1.inc'
C
          REAL*8 LARGER
C
          DIMENSION LARGER(M1,N1)
C
          DO I=2,IRED
              LARGER(I-1,1)=REDDAT1(I)
              LARGER(I-1,2)=REDDAT2(I)
          END DO
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
                  LARGER(1,1)=RRB
                  LARGER(1,2)=RRA
                  IIRED=IRED
                  RETURN
              ENDIF
          ENDIF
          I=L
          J=L+L
20        IF(J.LE.IR)THEN
              IF(J.LT.IR)THEN
                  IF(LARGER(J,2).LT.LARGER(J+1,2))J=J+1
              ENDIF
              IF(RRA.LT.LARGER(J,2))THEN
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


      SUBROUTINE REDPAK(LARGER,M1,N1,IIRED)
          IMPLICIT NONE
          INTEGER I,M1,N1,IIRED,JRED,J
!      REAL*8 RED1,RED2,REDP1,REDP2
!      LOGICAL OPEN62
          INCLUDE 'datmai.inc'
          INCLUDE 'datspd.inc'
          INCLUDE 'datsp1.inc'
C
C       LARGER(I,1) = ENERGY
C       LARGER(I,2) = RADIAL POSITION
C
          REAL*8 LARGER,LARGER2
          DIMENSION LARGER(M1,N1),LARGER2(M1,N1)
          LARGER2(1:M1,1:N1)=0.0D0
C
          IRED=IIRED
          J=1
          DO I=1,IRED
              IF(LARGER(I,1).NE.0.0D0) THEN
                  LARGER2(J,1:2)=LARGER(I,1:2)
                  J=J+1
              END IF
          END DO
          JRED=J
          LARGER(1:M1,1:N1)=0.0D0
          LARGER(1:JRED,1:N1)=LARGER2(1:JRED,1:N1)
          IF(JRED.LT.IRED) THEN
              LARGER(JRED+1:IRED,1:N1)=0.0D0
          END IF
C
          IRED=JRED
C
          JRED=IRED
          DO I=1,IRED-1
              IF(LARGER(I+1,2).EQ.LARGER(I,2)) THEN
                  JRED=JRED-1
                  LARGER(I,1)=LARGER(I,1)+LARGER(I+1,1)
                  DO J=I,JRED
                      IF(J+2.LE.JRED) THEN
                          LARGER(J+1,1)=LARGER(J+2,1)
                          LARGER(J+2,2)=LARGER(J+2,2)
                      END IF
                  END DO
              END IF
          END DO
          IIRED=IRED
          RETURN
C
      END


C SUB PSFWRTE.FOR
      SUBROUTINE PSFWRTE
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE PSFWRT.FOR. THIS SUBROUTINE CONTROLS
C     PSF FILE OUTPUT
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          INCLUDE 'datspd.inc'
C
C       CHECK FOR STRING INPUT
          IF(SST.EQ.1.OR.SN.EQ.1) THEN
              OUTLYNE='"PSFWRITE" TAKES NO STRING OR NUMERIC INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(STI.EQ.0) THEN
              IF(WQ.NE.'ON'.AND.WQ.NE.'OFF') THEN
                  OUTLYNE='"ON", "YES","OFF" AND "NO"'
                  CALL SHOWIT(1)
                  OUTLYNE=' ARE THE ONLY VALID QUALIFIERS'
                  CALL SHOWIT(1)
                  OUTLYNE='USED WITH "PSFWRITE"'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C     STI=1
          END IF
          IF(STI.EQ.1) THEN
              IF(.NOT.PSFWRITE) WRITE(OUTLYNE,10)
              IF(PSFWRITE) WRITE(OUTLYNE,11)
              CALL SHOWIT(0)
 10           FORMAT('PSF FILE OUTPUT IS CURRENTLY TURNED "OFF"')
 11           FORMAT('PSF FILE OUTPUT IS CURRENTLY TURNED "ON"')
              CALL SHOWIT(0)
              RETURN
          END IF
          IF(WQ.EQ.'ON') THEN
              PSFWRITE=.TRUE.
              RETURN
          END IF
          IF(WQ.EQ.'OFF') THEN
              PSFWRITE=.FALSE.
              RETURN
          END IF
          RETURN
      END


C SUB PSFBN.FOR
      SUBROUTINE PSFBN
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE PSFBN.FOR. THIS SUBROUTINE CONTROLS
C     PSF BINARY FILE OUTPUT
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          INCLUDE 'datspd.inc'
C
C       CHECK FOR STRING INPUT
          IF(SST.EQ.1.OR.SN.EQ.1) THEN
              OUTLYNE='"PSFBIN" TAKES NO STRING OR NUMERIC INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(STI.EQ.0) THEN
              IF(WQ.NE.'ON'.AND.WQ.NE.'OFF') THEN
                  OUTLYNE='"ON", "YES","OFF" AND "NO"'
                  CALL SHOWIT(1)
                  OUTLYNE=' ARE THE ONLY VALID QUALIFIERS'
                  CALL SHOWIT(1)
                  OUTLYNE='USED WITH "PSFBIN"'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C     STI=1
          END IF
          IF(STI.EQ.1) THEN
              IF(.NOT.PSFBIN) WRITE(OUTLYNE,10)
              IF(PSFBIN) WRITE(OUTLYNE,11)
              CALL SHOWIT(0)
 10           FORMAT('PSF BINARY FILE OUTPUT IS CURRENTLY TURNED "OFF"')
 11           FORMAT('PSF BINARY FILE OUTPUT IS CURRENTLY TURNED "ON"')
              CALL SHOWIT(0)
              RETURN
          END IF
          IF(WQ.EQ.'ON') THEN
              PSFBIN=.TRUE.
              RETURN
          END IF
          IF(WQ.EQ.'OFF') THEN
              PSFBIN=.FALSE.
              RETURN
          END IF
          RETURN
      END


C SUB PSFPLT.FOR
      SUBROUTINE PSFPLT
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE PSFPLT.FOR. THIS SUBROUTINE CONTROLS
C     PSF PLOT OUTPUT
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          INCLUDE 'datspd.inc'
C
C       CHECK FOR STRING INPUT
          IF(SST.EQ.1.OR.SN.EQ.1) THEN
              OUTLYNE='"PSFPLOT" TAKES NO STRING OR NUMERIC INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(STI.EQ.0) THEN
              IF(WQ.NE.'ON'.AND.WQ.NE.'OFF'.AND.WQ.NE.'HCONLY') THEN
                  OUTLYNE='"ON", "YES","OFF", "NO" AND "HCONLY"'
                  CALL SHOWIT(1)
                  OUTLYNE=' ARE THE ONLY VALID QUALIFIERS'
                  CALL SHOWIT(1)
                  OUTLYNE='USED WITH "PSFPLOT"'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C     STI=1
          END IF
          IF(STI.EQ.1) THEN
              IF(.NOT.PSFPLOT) WRITE(OUTLYNE,10)
              IF(PSFPLOT) WRITE(OUTLYNE,11)
              IF(PSFHC) WRITE(OUTLYNE,12)
              CALL SHOWIT(0)
 10           FORMAT('PSF PLOT OUTPUT IS CURRENTLY TURNED "OFF"')
 11           FORMAT('PSF PLOT OUTPUT IS CURRENTLY TURNED "ON"')
 12           FORMAT('PSF PLOT OUTPUT IS "HARD COPY ONLY"')
              CALL SHOWIT(0)
              RETURN
          END IF
          IF(WQ.EQ.'ON') THEN
              PSFPLOT=.TRUE.
              PSFHC=.FALSE.
              RETURN
          END IF
          IF(WQ.EQ.'OFF') THEN
              PSFPLOT=.FALSE.
              PSFHC=.FALSE.
              RETURN
          END IF
          IF(WQ.EQ.'HCONLY') THEN
              PSFPLOT=.TRUE.
              PSFHC=.TRUE.
              RETURN
          END IF
          RETURN
      END


C SUB PSFROT.FOR
      SUBROUTINE PSFROT
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE PSFROT.FOR. THIS SUBROUTINE CONTROLS
C     PSF PLOT ROTATION OUTPUT
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          INCLUDE 'datspd.inc'
C
C       CHECK FOR STRING INPUT
          IF(SST.EQ.1.OR.SN.EQ.1) THEN
              OUTLYNE='"PSFROT" TAKES NO STRING OR NUMERIC INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(STI.EQ.0) THEN
              IF(WQ.NE.'ON'.AND.WQ.NE.'OFF') THEN
                  OUTLYNE='"ON", "YES","OFF" AND "NO"'
                  CALL SHOWIT(1)
                  OUTLYNE=' ARE THE ONLY VALID QUALIFIERS'
                  CALL SHOWIT(1)
                  OUTLYNE='USED WITH "PSFROT"'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C     STI=1
          END IF
          IF(STI.EQ.1) THEN
              IF(.NOT.ROTPSF) WRITE(OUTLYNE,10)
              IF(ROTPSF) WRITE(OUTLYNE,11)
              CALL SHOWIT(0)
 10           FORMAT('PSF PLOT ROTATION IS CURRENTLY TURNED "OFF"')
 11           FORMAT('PSF PLOT ROTATION IS CURRENTLY TURNED "ON"')
              CALL SHOWIT(0)
              RETURN
          END IF
          IF(WQ.EQ.'ON') THEN
              ROTPSF=.TRUE.
              RETURN
          END IF
          IF(WQ.EQ.'OFF') THEN
              ROTPSF=.FALSE.
              RETURN
          END IF
          RETURN
      END


C SUB CAPFROT.FOR
      SUBROUTINE CAPFNROT
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE CAPFNROT.FOR. THIS SUBROUTINE CONTROLS
C     CAPFN PLOT ROTATION OUTPUT
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          INCLUDE 'datspd.inc'
C
C       CHECK FOR STRING INPUT
          IF(SST.EQ.1.OR.SN.EQ.1) THEN
              OUTLYNE='"CAPFNROT" TAKES NO STRING OR NUMERIC INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(STI.EQ.0) THEN
              IF(WQ.NE.'ON'.AND.WQ.NE.'OFF') THEN
                  OUTLYNE='"ON", "YES","OFF" AND "NO"'
                  CALL SHOWIT(1)
                  OUTLYNE=' ARE THE ONLY VALID QUALIFIERS'
                  CALL SHOWIT(1)
                  OUTLYNE='USED WITH "CAPFNROT"'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C     STI=1
          END IF
          IF(STI.EQ.1) THEN
              IF(.NOT.ROTCAPFN) WRITE(OUTLYNE,10)
              IF(ROTCAPFN) WRITE(OUTLYNE,11)
              CALL SHOWIT(0)
 10           FORMAT('CAPFN PLOT ROTATION IS CURRENTLY TURNED "OFF"')
 11           FORMAT('CAPFN PLOT ROTATION IS CURRENTLY TURNED "ON"')
              CALL SHOWIT(0)
              RETURN
          END IF
          IF(WQ.EQ.'ON') THEN
              ROTCAPFN=.TRUE.
              RETURN
          END IF
          IF(WQ.EQ.'OFF') THEN
              ROTCAPFN=.FALSE.
              RETURN
          END IF
          RETURN
      END


C SUB SAGFLROT.FOR
      SUBROUTINE SSAGFLROT
C
          IMPLICIT NONE
C
          LOGICAL ROTSAGFL
C
          COMMON/SAGFLROT/ROTSAGFL
C
C       THIS IS SUBROUTINE SAGFLROT.FOR. THIS SUBROUTINE CONTROLS
C     SAG FILE PLOT ROTATION OUTPUT
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
C       CHECK FOR STRING INPUT
          IF(SST.EQ.1.OR.SN.EQ.1) THEN
              OUTLYNE='"SAGFLROT" TAKES NO STRING OR NUMERIC INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(STI.EQ.0) THEN
              IF(WQ.NE.'ON'.AND.WQ.NE.'OFF') THEN
                  OUTLYNE='"ON", "YES","OFF" AND "NO"'
                  CALL SHOWIT(1)
                  OUTLYNE=' ARE THE ONLY VALID QUALIFIERS'
                  CALL SHOWIT(1)
                  OUTLYNE='USED WITH "SAGFLROT"'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C     STI=1
          END IF
          IF(STI.EQ.1) THEN
              IF(.NOT.ROTSAGFL) WRITE(OUTLYNE,10)
              IF(ROTSAGFL) WRITE(OUTLYNE,11)
              CALL SHOWIT(0)
 10           FORMAT('SAG.DAT PLOT ROTATION IS CURRENTLY TURNED "OFF"')
 11           FORMAT('SAG.DAT PLOT ROTATION IS CURRENTLY TURNED "ON"')
              CALL SHOWIT(0)
              RETURN
          END IF
          IF(WQ.EQ.'ON') THEN
              ROTSAGFL=.TRUE.
              RETURN
          END IF
          IF(WQ.EQ.'OFF') THEN
              ROTSAGFL=.FALSE.
              RETURN
          END IF
          RETURN
      END


C SUB PSFLINLOG.FOR

      SUBROUTINE PSFLINLOG
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE PSFLINLOG (DOES PSFLIN AND PSFLOG COMMANDS)
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datspd.inc'
C
          IF(STI.EQ.1) THEN
              IF(PSFLIN.EQ.1) THEN
                  OUTLYNE='CURRENT "PSFLIN" IS CURRENTLY IN EFFECT'
                  CALL SHOWIT(1)
                  RETURN
              END IF
              IF(PSFLIN.EQ.0) THEN
                  OUTLYNE='CURRENT "PSFLOG" IS CURRENTLY IN EFFECT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'THE PSF WILL BE SPREAD OVER ',PSFLOG,' DECADES'
                  CALL SHOWIT(1)
                  RETURN
              END IF
          END IF
C
          IF(SST.EQ.1.OR.SQ.EQ.1) THEN
              OUTLYNE=WC(1:6)//' TAKES NO STRING OR QUALIFIER INPUT'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SN.EQ.1.AND.WC.EQ.'PSFLIN') THEN
              OUTLYNE=WC(1:6)//' TAKES NO NUMERIC INPUT'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(WC.EQ.'PSFLOG') THEN
              IF(S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
                  OUTLYNE=WC(1:6)//' TAKES ONLY TAKES NUMERIC WORD #1 INPUT'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(DF1.EQ.1) THEN
                  DF1=0
                  S1=1
                  SN=1
                  W1=2.0D0
              END IF
          END IF
          IF(W1.LE.0.0D0.AND.WC.EQ.'PSFLOG') THEN
              OUTLYNE=WC(1:6)//' TAKES REQUIRES POSITIVE NUMERIC WORD #1 INPUT'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF

          IF(WC.EQ.'PSFLIN') PSFLIN=1
          IF(WC.EQ.'PSFLOG') PSFLIN=0
          IF(WC.EQ.'PSFLOG') PSFLOG=INT(W1)
          RETURN
      END


C SUB PSFTAGG.FOR

      SUBROUTINE PSFTAGG
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE PSFTAGG (DOES PSFTAG AND PSFLI COMMANDS)
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datspd.inc'
C
          IF(WC.EQ.'PSFTAG') THEN
              IF(STI.EQ.1) THEN
                  OUTLYNE='CURRENT "PSFTAG" IS SET TO : '//PSFTAG
                  CALL SHOWIT(1)
                  RETURN
              END IF
              IF(SN.EQ.1.OR.SQ.EQ.1) THEN
                  OUTLYNE='"PSFTAG" ONLY TAKES STRING INPUT'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(SST.EQ.0) THEN
                  OUTLYNE='"PSFTAG" REQUIRES EXPLICIT STRING INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='TWELVE (12) CHARACTERS OF INPUT ARE REGONIZED'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(SST.EQ.1) THEN
                  PSFTAG=WS(1:12)
                  RETURN
              END IF
          END IF
C
          IF(WC.EQ.'PSFLI') THEN
              IF(STI.EQ.1) THEN
                  OUTLYNE='CURRENT "PSFLI" IS SET TO : '
                  CALL SHOWIT(1)
                  OUTLYNE=PSFLI(1:80)
                  CALL SHOWIT(1)
                  RETURN
              END IF
              IF(SN.EQ.1.OR.SQ.EQ.1) THEN
                  OUTLYNE='"PSFLI" ONLY TAKES STRING INPUT'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(SST.EQ.0) THEN
                  OUTLYNE='"PSFLI" REQUIRES EXPLICIT STRING INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='EIGHTY (80) CHARACTERS OF INPUT ARE REGONIZED'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(SST.EQ.1) THEN
                  PSFLI=WS(1:80)
                  RETURN
              END IF
          END IF
          RETURN
      END


C SUB CAPFNTAGG.FOR

      SUBROUTINE CAPFNTAGG
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE CAPFNTAGG (DOES CAPFNTAG AND CAPFNLI COMMANDS)
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datspd.inc'
C
          IF(WC.EQ.'CAPFNTAG') THEN
              IF(STI.EQ.1) THEN
                  OUTLYNE='CURRENT "CAPFNTAG" IS SET TO : '//CAPFNTAG
                  CALL SHOWIT(1)
                  RETURN
              END IF
              IF(SN.EQ.1.OR.SQ.EQ.1) THEN
                  OUTLYNE='"CAPFNTAG" ONLY TAKES STRING INPUT'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(SST.EQ.0) THEN
                  OUTLYNE='"CAPFNTAG" REQUIRES EXPLICIT STRING INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='TWELVE (12) CHARACTERS OF INPUT ARE REGONIZED'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(SST.EQ.1) THEN
                  CAPFNTAG=WS(1:12)
                  RETURN
              END IF
          END IF
C
          IF(WC.EQ.'CAPFNLI') THEN
              IF(STI.EQ.1) THEN
                  OUTLYNE='CURRENT "CAPFNLI" IS SET TO : '
                  CALL SHOWIT(1)
                  OUTLYNE=CAPFNLI(1:80)
                  CALL SHOWIT(1)
                  RETURN
              END IF
              IF(SN.EQ.1.OR.SQ.EQ.1) THEN
                  OUTLYNE='"CAPFNLI" ONLY TAKES STRING INPUT'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(SST.EQ.0) THEN
                  OUTLYNE='"CAPFNLI" REQUIRES EXPLICIT STRING INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='EIGHTY (80) CHARACTERS OF INPUT ARE REGONIZED'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(SST.EQ.1) THEN
                  CAPFNLI=WS(1:80)
                  RETURN
              END IF
          END IF
          RETURN
      END
C SUB PSFSPOT.FOR
      SUBROUTINE PSFSPOT(IRRER,MMM)
          USE GLOBALS
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE PSFSPOT
C       IT CREATES A SPOT DIAGRAM FILE FOR THE PSF.DAT FILE
C
          CHARACTER UNITS*2
C
          REAL*8 EXTENT,SPACING,PSFXCENT,PSFYCENT,CRAYX,CRAYY
     1    ,X,Y
C
          INTEGER MMM,IMM,INN,I,J,II,ALLOERR,IDUM,JDUM
C
          LOGICAL EXIS51,OPEN51,IRRER
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datspd.inc'
          INCLUDE 'datsp1.inc'
!      CALL IOSDELETEFILE('SPDPSF.DAT')
          OPEN(UNIT=53,ACCESS='SEQUENTIAL',FILE='SPDPSF.DAT',
     1    FORM='FORMATTED',STATUS='UNKNOWN')
C
C     DOES THE PSF FILE EXIST WITH AN EXISTING PSF
          EXIS51=.FALSE.
          INQUIRE(FILE=trim(HOME)//'PSF.DAT',EXIST=EXIS51)
          IF(EXIS51) THEN
C     DO ANALYSIS
              OPEN51=.FALSE.
              INQUIRE(FILE=trim(HOME)//'PSF.DAT',OPENED=OPEN51)
              IF(.NOT.OPEN51) THEN
C     OPEN FILE
                  OPEN(UNIT=51,ACCESS='SEQUENTIAL',BLANK='NULL'
     1              ,FORM='FORMATTED',FILE=trim(HOME)//'PSF.DAT'
     2              ,STATUS='UNKNOWN')
                  REWIND(UNIT=51)
              ELSE
                  REWIND(UNIT=51)
              END IF
C     READ SOME STUFF
              READ(51,1001) PSFTAG
 1001         FORMAT(A12)
              READ(51,1002) PSFLI
 1002         FORMAT(A78)
              READ(51,1003) MMM,PGR
 1003         FORMAT(I10,1X,I10)
              READ(51,1004) UNITS
 1004         FORMAT(A2)
              READ(51,1008) EXTENT,SPACING
              READ(51,1008) PSFXCENT,PSFYCENT
              READ(51,1008) CRAYX,CRAYY
 1008         FORMAT(E15.8,1x,E15.8)
 1005         FORMAT(2I4,I8)
! 1015 FORMAT(G23.15,1X,G23.15,1X,G23.15)
! 1006 FORMAT(I4,G23.15)
C     READPSF ARRAYS
C
C     MMM IS ODD SO
              IMM=(MMM-1)/2
              INN=(MMM-1)/2
              WRITE(53,1016) (PGR**2)
              DEALLOCATE(SPDPSF1,SPDPSF2,SPDPSF3,STAT=ALLOERR)
              ALLOCATE(SPDPSF1(MMM**2),SPDPSF2(MMM**2),SPDPSF3(MMM**2),
     1        STAT=ALLOERR)
 1016         FORMAT(I8)
              II=1
              Y=-(DBLE(INN)*DBLE(SPACING))
              DO J=1,(MMM)
                  X=-(DBLE(IMM)*DBLE(SPACING))
                  DO I=1,(MMM)
                      SPDPSF1(II)=X
                      SPDPSF2(II)=Y
                      READ(51,1005) IDUM,JDUM,IPSF3(II)
                      SPDPSF3(II)=DBLE(IPSF3(II))
                      II=II+1
                      X=X+DBLE(SPACING)
                  END DO
                  Y=Y+DBLE(SPACING)
              END DO
              CALL CLOSE_FILE(51,1)
              CALL CLOSE_FILE(53,1)
              IRRER=.FALSE.
          ELSE
              IRRER=.TRUE.
C     NO ANALYSIS CAN BE DONE, NO PSF.DAT EXISTS
              RETURN
          END IF
          RETURN
      END
C SUB PIXAR.FOR

      SUBROUTINE PIXAR
          USE GLOBALS
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE PIXAR.FOR.
C     CALLED BY CMDER FOR COMMANDS PIXEL AND CENTROID
C
          CHARACTER EEOOFF*4,UNITS*2
C
          REAL*8 EXTENT,SPACING,PSFXCENT,PSFYCENT,XCEN,YCEN
     1    ,XLSF,YLSF,PSF,DDUMMY,SHIFTER,SUMMERIT,XCENVAL,XCNT,YCNT
     2    ,SUMMX,SUMMY,TRUTH,YCENVAL,XRMS,YRMS,XMEEN,YMEEN,XSUMM,YSUMM
     3    ,XSSUMM,YSSUMM
C
          REAL*8 JXSUM,JYSUM,XSUM,YSUM,XDUM,YDUM
     1    ,CRAYX,CRAYY,JKSUMX1,JKSUMY1,JKSUMX2,JKSUMY2,DEVX,DEVY,SUMMINT
     2    ,XTOT1,YTOT1,TOT1
C
          INTEGER PPGR,IPSF,JJJ,I,J,K,SHIFT,PIX2,ISHIFT,JSHIFT
     1    ,SSHIFT,SHIFTY,PMMM,IP,JP,IQ,JQ,IR,JR,PGG
C
          DIMENSION XLSF(:),YLSF(:),XCEN(:),YCEN(:),XDUM(:),YDUM(:)
     1    ,PSF(:,:),IPSF(:,:),SHIFTER(:,:),SUMMERIT(:,:),
     2    SUMMX(:),SUMMY(:)
C
          INTEGER MMM,PIXAR1,PIXAR2,DUMMY,DUMIND,JJ,PRG,II
C
          COMMON/TOYSTORY/PIXAR1,PIXAR2
C
          ALLOCATABLE :: SHIFTER,IPSF,PSF,XLSF,YLSF,XCEN,YCEN,XDUM,YDUM
     1    ,SUMMERIT,SUMMX,SUMMY
C
          INTEGER ALLOERR,IDUM,JDUM
C
          LOGICAL EXIS51,OPEN51
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datspd.inc'
          INCLUDE 'datsp1.inc'
          PRG=TGR-1
C
C     WC='PIXEL'
          IF(WC.EQ.'PIXEL') THEN
              IF(STI.EQ.1) THEN
                  OUTLYNE=
     1            '"PIXEL" SETS UP PARAMETERS FOR PIXEL LINEARITY ANALYSIS'
                  CALL SHOWIT(1)
                  OUTLYNE=
     1            'NUMERIC WORD #1 IS THE NUMBER OF SUB-PIXELS PER PIXEL'
                  CALL SHOWIT(1)
                  OUTLYNE=
     1            'NUMERIC WORD #2 IS THE NUMBER OF PIXELS IN THE WINDOW'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)
     1            'CURRENT NUMBER OF SUB-PIXELS PER PIXEL IS : ',PIXAR1
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)
     1            '   CURRENT NUMBER PIXELS IN THE WINDOW IS : ',PIXAR2
                  CALL SHOWIT(1)
                  RETURN
              END IF
              IF(SYSTEM1(30).GE.3.0D0) THEN
                  OUTLYNE=
     1            '"PIXEL" IS NOT OPERATIONAL WITH AFOCAL SYSTEMS'
                  CALL SHOWIT(1)
                  OUTLYNE='NO ACTION TAKEN'
                  CALL SHOWIT(1)
                  CALL MACFAL
              END IF
              IF(SST.EQ.1.OR.SQ.EQ.1) THEN
                  OUTLYNE=
     1            '"PIXEL" TAKES NO STRING OR QUALIFIER WORD INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
                  OUTLYNE=
     1            '"PIXEL" ONLY TAKES NUMERIC WORD #1 AND #2 INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(DF1.EQ.1.OR.DF2.EQ.1) THEN
                  OUTLYNE=
     1            '"PIXEL" REQUIRES EXPLICIT NUMERIC WORD #1 AND #2 INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(W1.LE.0.0D0) THEN
                  OUTLYNE=
     1            'NUMERIC WORD #1 MUST BE A POSITIVE INTEGER'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(((DBLE(INT(W1/2.0D0))*2.0D0)-W1).EQ.0.0D0) THEN
                  OUTLYNE=
     1            'NUMERIC WORD #1 MUST BE AN ODD INTEGER'
                  CALL SHOWIT(1)
                  OUTLYNE=
     1            'IT MAY NOT BE AN EVEN INTEGER'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(W2.LT.1.0D0) THEN
                  OUTLYNE=
     1            'NUMERIC WORD #2 MUST BE A POSITIVE INTEGER'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(W1.LE.W2) THEN
                  OUTLYNE=
     1            'NUMERIC WORD #1 MUST BE GREATER THAN NUMERIC WORD #2'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              PIXAR1=INT(W1)
              PIXAR2=INT(W2)
              RETURN
          END IF
C
C     WC='CENTROID'
          IF(WC.EQ.'CENTROID') THEN
              IF(PIXAR1.EQ.0.0D0.OR.PIXAR2.EQ.0.0D0) THEN
                  OUTLYNE='THE NUMBER OF SUB-PIXELS AND PIXELS MUST BE SET USING'
                  CALL SHOWIT(1)
                  OUTLYNE='THE "PIXEL" COMMAND BEFORE A PIXEL LINEARITY ANALYSIS'
                  CALL SHOWIT(1)
                  OUTLYNE='CAN BE PERFORMED'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(STI.EQ.1) THEN
                  OUTLYNE=
     1            '"CENTROID" PERFORMS A COMPLETE LINEARITY ANALYSIS'
                  CALL SHOWIT(1)
                  RETURN
              END IF
              IF(SST.EQ.1.OR.SN.EQ.1) THEN
                  OUTLYNE=
     1            '"CENTROID" TAKES NO NUMERIC OR STRING INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(SYSTEM1(30).GE.3.0D0) THEN
                  OUTLYNE=
     1            '"CENTROID" IS NOT OPERATIONAL WITH AFOCAL SYSTEMS'
                  CALL SHOWIT(1)
                  OUTLYNE='NO ACTION TAKEN'
                  CALL SHOWIT(1)
                  CALL MACFAL
              END IF
              IF(SQ.EQ.1.AND.WQ.NE.'LSF'.AND.WQ.NE.'SLSF'.AND.WQ.NE.'PSF'
     1        .AND.WQ.NE.'SPSF') THEN
                  OUTLYNE=
     1            '"CENTROID" ONLY TAKES "LSF", "PSF", "SLSF" AND "SPSF"'
                  CALL SHOWIT(1)
                  OUTLYNE='AS QUALIFIER WORDS'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C
C     DO CENTROIDING
C     DOES THE PSF FILE EXIST WITH AN EXISTING PSF
              EXIS51=.FALSE.
              IF(WQ(1:1).NE.'S'.OR.SQ.EQ.0)
     1        INQUIRE(FILE=trim(HOME)//'PSF.DAT',EXIST=EXIS51)
              IF(WQ(1:1).EQ.'S') INQUIRE(FILE='SPSF.DAT',EXIST=EXIS51)
              IF(EXIS51) THEN
C     DO ANALYSIS
                  OPEN51=.FALSE.
                  IF(SQ.EQ.0.OR.WQ.EQ.'PSF'.OR.WQ.EQ.'LSF')
     1            INQUIRE(FILE=trim(HOME)//'PSF.DAT',OPENED=OPEN51)
                  IF(WQ(1:1).EQ.'S')INQUIRE(FILE='SPSF.DAT',OPENED=OPEN51)
                  IF(.NOT.OPEN51) THEN
C     OPEN FILE
                      IF(SQ.EQ.0.OR.WQ.EQ.'PSF'.OR.WQ.EQ.'LSF')
     1                OPEN(UNIT=51,ACCESS='SEQUENTIAL',BLANK='NULL'
     1                  ,FORM='FORMATTED',FILE=trim(HOME)//'PSF.DAT'
     2                  ,STATUS='UNKNOWN')
                      IF(WQ(1:1).EQ.'S') OPEN(UNIT=51,ACCESS='SEQUENTIAL',BLANK='NULL'
     1                  ,FORM='FORMATTED',FILE=trim(HOME)//'SPSF.DAT'
     2                  ,STATUS='UNKNOWN')
                      REWIND(UNIT=51)
                  ELSE
                      REWIND(UNIT=51)
                  END IF
C     NOW ALLOCATE AND LOAD ARRAYS
C     READ SOME STUFF
                  READ(51,1001) PSFTAG
 1001             FORMAT(A12)
                  READ(51,1002) PSFLI
 1002             FORMAT(A78)
                  READ(51,1003) MMM,PGR
                  PPGR=(PGR-1)/2
                  PMMM=(MMM-1)/2
 1003             FORMAT(I10,1X,I10)
                  READ(51,1004) UNITS
 1004             FORMAT(A2)
                  READ(51,1008) EXTENT,SPACING
                  READ(51,1008) PSFXCENT,PSFYCENT
                  READ(51,1008) CRAYX,CRAYY
 1008             FORMAT(E15.8,1x,E15.8)
 1005             FORMAT(2I4,I8)
 1006             FORMAT(I4,G23.15)
                  IF(WQ.EQ.'LSF'.OR.WQ.EQ.'SLSF'.OR.SQ.EQ.0) THEN
                      DEALLOCATE(XLSF,YLSF,STAT=ALLOERR)
                      ALLOCATE(XLSF(-PPGR:PPGR),
     1                YLSF(-PPGR:PPGR),STAT=ALLOERR)
                  END IF
                  IF(WQ.EQ.'PSF'.OR.WQ.EQ.'SPSF') THEN
                      DEALLOCATE(PSF,IPSF,STAT=ALLOERR)
                      ALLOCATE(IPSF(-PMMM:PMMM,-PMMM:PMMM),
     1                PSF(-PMMM:PMMM,-PMMM:PMMM),STAT=ALLOERR)
                  END IF
C     LOAD PSF/LSF ARRAYS
                  DEALLOCATE(SPDPSF1,SPDPSF2,SPDPSF3,STAT=ALLOERR)
                  ALLOCATE(SPDPSF1(MMM**2),SPDPSF2(MMM**2),SPDPSF3(MMM**2),
     1            STAT=ALLOERR)
                  II=1
                  DO J=-PMMM,PMMM
                      DO I=-PMMM,PMMM
                          READ(51,1005) IDUM,JDUM,IPSF3(II)
                          IF(WQ.EQ.'SPSF'.OR.WQ.EQ.'PSF') THEN

                              PSF(I,J)=DBLE(IPSF3(II))
                              II=II+1
                          END IF
                      END DO
                  END DO
C     READ IN THE XLSF
                  DO I=-PPGR,PPGR
                      IF(WQ.EQ.'SPSF'.OR.WQ.EQ.'PSF') THEN
                          READ(51,1006) DUMMY,DDUMMY
                      ELSE
                          READ(51,1006) DUMMY,XLSF(I)
                      END IF
                  END DO
C     READ IN THE YLSF
                  DO I=-PPGR,PPGR
                      IF(WQ.EQ.'SPSF'.OR.WQ.EQ.'PSF') THEN
                          READ(51,1006) DUMMY,DDUMMY
                      ELSE
                          READ(51,1006) DUMMY,YLSF(I)
                      END IF
                  END DO
C     NEXT LINE BETTER BE EOF
                  READ(51,*) EEOOFF
C     LSF ARRAYS LOADED
                  REWIND(UNIT=51)
                  CALL CLOSE_FILE(51,1)
C     FINISHED CENTROID ANALYSIS
                  IF(WQ.EQ.'PSF'.OR.WQ.EQ.'SPSF') THEN
C******************************************************************************
C     PSF CENTROIDING
                      IF(PGR.LT.((PIXAR1*PIXAR2)+PIXAR1)) THEN
                          OUTLYNE='THE CURRENT PSF WINDOW DEFINED BY THE PGR VALUE'
                          CALL SHOWIT(1)
                          OUTLYNE='IS TOO SMALL FOR THE CURRENTLY DEFINED PIXEL VALUES'
                          CALL SHOWIT(1)
                          OUTLYNE='NO CENTROID LINEARITY ANALYSIS CAN BE PERFORMED'
                          CALL SHOWIT(1)
                          CALL MACFAL
                          DEALLOCATE(XLSF,YLSF,XCEN,YCEN,IPSF,PSF,SHIFTER,STAT=ALLOERR)
                          RETURN
                      END IF
                      IF(((DBLE(PIXAR2)/2.0D0)-DBLE(INT(DBLE(PIXAR2)/2.0D0)))
     1                .NE.0.0D0) THEN
C     PIXAR2 ODD
                          PIX2=1
                          PGG=((PIXAR1*PIXAR2)-1)/2
                      ELSE
C     PIXAR2 EVEN
                          PIX2=0
                          PGG=(PIXAR1*PIXAR2)/2
                      END IF
                      SSHIFT=PIXAR1
                      IF(PIX2.EQ.1) SHIFTY=(SSHIFT-1)/2
                      IF(PIX2.EQ.0) SHIFTY=(SSHIFT+1)/2
                      XMEEN=0.0D0
                      YMEEN=0.0D0
                      XRMS=0.0D0
                      XSUMM=0.0D0
                      JXSUM=0.0D0
                      JYSUM=0.0D0
                      YSUMM=0.0D0
                      XSSUMM=0.0D0
                      YSSUMM=0.0D0
                      YRMS=0.0D0
                      XCNT=0.0D0
                      YCNT=0.0D0
C
                      DEALLOCATE(SUMMERIT,STAT=ALLOERR)
                      ALLOCATE(SUMMERIT(PIXAR2,PIXAR2),STAT=ALLOERR)
                      IF(PIX2.EQ.1) THEN
                          DEALLOCATE(SHIFTER,STAT=ALLOERR)
                          ALLOCATE(SHIFTER(-PGG:PGG,-PGG:PGG),STAT=ALLOERR)
                          DO ISHIFT=-SHIFTY,SHIFTY
                              DO JSHIFT=-SHIFTY,SHIFTY
C     A NEW SSHIFT VALUE, ZERO OUT THE SUMMERIT ARRAY
                                  DO JQ=1,PIXAR2
                                      DO IQ=1,PIXAR2
                                          SUMMERIT(IQ,JQ)=0.0D0
                                      END DO
                                  END DO
C     INCREMENT THE SHIFT COUNTER
C     LOAD THE SHIFTER ARRAY
C     ODD NUMBER OF PIXELS
                                  DO JP=-PGG,PGG
                                      DO IP=-PGG,PGG
                                          SHIFTER(IP,JP)=PSF(IP-ISHIFT,JP-JSHIFT)
                                      END DO
                                  END DO
C     NOW DO ANALYSIS ON THE SHIFTER ARRAY
C     AT PIXEL JQ,IQ
                                  DO JQ=1,PIXAR2
                                      DO IQ=1,PIXAR2
C     NEW PIXEL, ZERO THE SUMMINT VARIABLE
                                          SUMMINT=0.0D0
C     SUMMINT IS AN INTERMEDIATE SUMMATION VALUE
                                          DO JR=1,PIXAR1
                                              DO IR=1,PIXAR1
                                                  SUMMINT=SUMMINT+
     1                                            SHIFTER(-PGG-1+((IQ-1)*PIXAR1)+IR,-PGG-1+((JQ-1)*PIXAR1)+JR)
                                              END DO
                                          END DO

C     SUMMER IS THE ENERGY SUM OF THE IQ,JQ TH PIXEL
                                          SUMMERIT(IQ,JQ)=SUMMINT
                                      END DO
                                  END DO
C     EVERYTIME WE GET HERE WE HAVE A NEW CENTROID VALUES TO CALCULATE
C     FOR A NEW SHIFT POSITION
C
C     CALCULATE CENTROID POSITIONS
C
C
C     X CENTROID, SUMM ALL THE Y VALUES
                                  DEALLOCATE(SUMMX,SUMMY,STAT=ALLOERR)
                                  ALLOCATE(SUMMX(PIXAR2),SUMMY(PIXAR2),STAT=ALLOERR)
                                  DO IQ=1,PIXAR2
                                      SUMMX(IQ)=0.0D0
                                      SUMMY(IQ)=0.0D0
                                  END DO
                                  DO IQ=1,PIXAR2
                                      DO JQ=1,PIXAR2
                                          SUMMX(IQ)=SUMMX(IQ)+(SUMMERIT(IQ,JQ))
                                      END DO
                                  END DO
                                  XTOT1=0.0D0
                                  TOT1=0.0D0
                                  JXSUM=0.0D0
                                  DO IQ=1,PIXAR2
                                      XTOT1=XTOT1+SUMMX(IQ)
                                      TOT1=TOT1+DBLE(IQ)
                                      JXSUM=JXSUM+(DBLE(IQ)*SUMMX(IQ))
                                  END DO
                                  TRUTH=PSFXCENT+(DBLE(ISHIFT)/DBLE(PIXAR1))
                                  XCENVAL=(JXSUM/XTOT1)-(TOT1/DBLE(PIXAR2))
     1                            -TRUTH
                                  XSUMM=XSUMM+XCENVAL
                                  XSSUMM=XSSUMM+(XCENVAL**2)
                                  XCNT=XCNT+1.0D0
C
C
C     Y CENTROID, SUMM ALL THE X VALUES
                                  DEALLOCATE(SUMMX,SUMMY,STAT=ALLOERR)
                                  ALLOCATE(SUMMX(PIXAR2),SUMMY(PIXAR2),STAT=ALLOERR)
                                  DO IQ=1,PIXAR2
                                      SUMMX(IQ)=0.0D0
                                      SUMMY(IQ)=0.0D0
                                  END DO
                                  DO JQ=1,PIXAR2
                                      DO IQ=1,PIXAR2
                                          SUMMY(JQ)=SUMMY(JQ)+(SUMMERIT(IQ,JQ))
                                      END DO
                                  END DO
                                  YTOT1=0.0D0
                                  TOT1=0.0D0
                                  JYSUM=0.0D0
                                  DO JQ=1,PIXAR2
                                      YTOT1=YTOT1+SUMMY(JQ)
                                      TOT1=TOT1+DBLE(JQ)
                                      JYSUM=JYSUM+(DBLE(JQ)*SUMMY(JQ))
                                  END DO
C     NUMBER OF PIXELS IS ODD
                                  TRUTH=PSFYCENT+(DBLE(JSHIFT)/DBLE(PIXAR1))
                                  YCENVAL=(JYSUM/YTOT1)-(TOT1/DBLE(PIXAR2))
     1                            -TRUTH
                                  YSUMM=YSUMM+(YCENVAL)
                                  YSSUMM=YSSUMM+((YCENVAL)**2)
                                  YCNT=YCNT+1
                                  DEALLOCATE(SUMMX,SUMMY,STAT=ALLOERR)
C
                              END DO
                          END DO
                      ELSE
C     PIX2 = 0
                          DEALLOCATE(SHIFTER,STAT=ALLOERR)
                          ALLOCATE(SHIFTER(-PGG:PGG-1,-PGG:PGG-1),STAT=ALLOERR)
                          DO ISHIFT=-SHIFTY,SHIFTY-1
                              DO JSHIFT=-SHIFTY,SHIFTY-1
C     A NEW SSHIFT VALUE, ZERO OUT THE SUMMERIT ARRAY
                                  DO JQ=1,PIXAR2
                                      DO IQ=1,PIXAR2
                                          SUMMERIT(IQ,JQ)=0.0D0
                                      END DO
                                  END DO
C     INCREMENT THE SHIFT COUNTER
C     LOAD THE SHIFTER ARRAY
                                  DO JP=-PGG,PGG-1
                                      DO IP=-PGG,PGG-1
                                          SHIFTER(IP,JP)=PSF(IP-ISHIFT,JP-JSHIFT)
C                       END IF
                                      END DO
                                  END DO
C     NOW DO ANALYSIS ON THE SHIFTER ARRAY
C     AT PIXEL JQ,IQ
                                  DO JQ=1,PIXAR2
                                      DO IQ=1,PIXAR2
C     NEW PIXEL, ZERO THE SUMMINT VARIABLE
                                          SUMMINT=0.0D0
C     SUMMINT IS AN INTERMEDIATE SUMMATION VALUE
                                          DO JR=1,PIXAR1
                                              DO IR=1,PIXAR1
                                                  SUMMINT=SUMMINT+
     1                                            SHIFTER(-PGG-1+((IQ-1)*PIXAR1)+IR,-PGG-1+((JQ-1)*PIXAR1)+JR)
                                              END DO
                                          END DO

C     SUMMER IS THE ENERGY SUM OF THE IQ,JQ TH PIXEL
                                          SUMMERIT(IQ,JQ)=SUMMINT
                                      END DO
                                  END DO
C     EVERYTIME WE GET HERE WE HAVE A NEW CENTROID VALUES TO CALCULATE
C     FOR A NEW SHIFT POSITION
C
C     CALCULATE CENTROID POSITIONS
C
C
C     X CENTROID, SUMM ALL THE Y VALUES
                                  DEALLOCATE(SUMMX,SUMMY,STAT=ALLOERR)
                                  ALLOCATE(SUMMX(PIXAR2),SUMMY(PIXAR2),STAT=ALLOERR)
                                  DO IQ=1,PIXAR2
                                      SUMMX(IQ)=0.0D0
                                  END DO
                                  DO IQ=1,PIXAR2
                                      DO JQ=1,PIXAR2
                                          SUMMX(IQ)=SUMMX(IQ)+(SUMMERIT(IQ,JQ))
                                      END DO
                                  END DO
                                  XTOT1=0.0D0
                                  TOT1=0.0D0
                                  JXSUM=0.0D0
                                  DO IQ=1,PIXAR2
                                      XTOT1=XTOT1+SUMMX(IQ)
                                      TOT1=TOT1+DBLE(IQ)
                                      JXSUM=JXSUM+(DBLE(IQ)*SUMMX(IQ))
                                  END DO
                                  IF(ISHIFT.LT.0)
     1                            TRUTH=PSFXCENT+
     1                            ((DBLE(ISHIFT)+0.5D0)/DBLE(PIXAR1))
                                  IF(ISHIFT.GE.0)
     1                            TRUTH=PSFXCENT+
     1                            ((DBLE(ISHIFT+1)-0.5D0)/DBLE(PIXAR1))
                                  XCENVAL=(JXSUM/XTOT1)-(TOT1/DBLE(PIXAR2))
     1                            -TRUTH
                                  XSUMM=XSUMM+XCENVAL
                                  XSSUMM=XSSUMM+(XCENVAL**2)
                                  XCNT=XCNT+1.0D0
C
C
C     Y CENTROID, SUMM ALL THE X VALUES
                                  DEALLOCATE(SUMMX,SUMMY,STAT=ALLOERR)
                                  ALLOCATE(SUMMX(PIXAR2),SUMMY(PIXAR2),STAT=ALLOERR)
                                  DO IQ=1,PIXAR2
                                      SUMMY(IQ)=0.0D0
                                  END DO
                                  DO JQ=1,PIXAR2
                                      DO IQ=1,PIXAR2
                                          SUMMY(JQ)=SUMMY(JQ)+(SUMMERIT(IQ,JQ))
                                      END DO
                                  END DO
                                  YTOT1=0.0D0
                                  TOT1=0.0D0
                                  JYSUM=0.0D0
                                  DO JQ=1,PIXAR2
                                      YTOT1=YTOT1+SUMMY(JQ)
                                      TOT1=TOT1+DBLE(JQ)
                                      JYSUM=JYSUM+(DBLE(JQ)*SUMMY(JQ))
                                  END DO
                                  IF(JSHIFT.LT.0)
     1                            TRUTH=PSFYCENT+
     1                            ((DBLE(JSHIFT)+0.5D0)/DBLE(PIXAR1))
                                  IF(JSHIFT.GE.0)
     1                            TRUTH=PSFYCENT+
     1                            ((DBLE(JSHIFT+1)-0.5D0)/DBLE(PIXAR1))
                                  YCENVAL=(JYSUM/YTOT1)-(TOT1/DBLE(PIXAR2))
     1                            -TRUTH
                                  IF(JSHIFT.EQ.-SHIFTY.OR.JSHIFT.EQ.SHIFTY-1) THEN
                                      YSUMM=YSUMM+(YCENVAL*0.5D0)
                                      YSSUMM=YSSUMM+((YCENVAL*0.5D0)**2)
                                  ELSE
                                      YSUMM=YSUMM+YCENVAL
                                      YSSUMM=YSSUMM+(YCENVAL**2)
                                  END IF
                                  YCNT=YCNT+1
                                  DEALLOCATE(SUMMX,SUMMY,STAT=ALLOERR)
C
                              END DO
                          END DO
                      END IF
C     NOW PROCESS TO GET MEAN AND RMS CENTROIDS
C     IN X AND Y
                      XMEEN=XSUMM/XCNT
                      YMEEN=YSUMM/YCNT
                      XRMS=DSQRT(
     1                (XSSUMM-((XSUMM**2)/XCNT))
     2                /(XCNT-1.0D0)
     3                )
                      YRMS=DSQRT(
     1                (YSSUMM-((YSUMM**2)/YCNT))
     2                /(YCNT-1.0D0)
     3                )
  3                   FORMAT('POINT SPREAD FUNCTION CENTROID ANALYSIS')
  6                   FORMAT('LINE SPREAD FUNCTION CENTROID ANALYSIS')
  4                   FORMAT('FROM PSF.DAT FILE')
  5                   FORMAT('FROM SPSF.DAT FILE')
  1                   FORMAT('    NUMBER OF PIXELS = ',I3)
  2                   FORMAT('NUMBER OF SUB-PIXELS = ',I3)
 10                   FORMAT('CENTROID      MEAN X-POSITION (IN PIXELS) = ',G13.6)
 20                   FORMAT('CENTROID      MEAN Y-POSITION (IN PIXELS) = ',G13.6)
 30                   FORMAT('CENTROID RMS X-CENTROID ERROR (IN PIXELS) = ',G13.6)
 40                   FORMAT('CENTROID RMS Y-CENTROID ERROR (IN PIXELS) = ',G13.6)
                      IF(WQ.EQ.'PSF'.OR.WQ.EQ.'SPSF') WRITE(OUTLYNE,3)
                      IF(WQ.EQ.'LSF'.OR.WQ.EQ.'SLSF'.OR.SQ.EQ.0) WRITE(OUTLYNE,6)
                      CALL SHOWIT(0)
                      IF(WQ.EQ.'PSF'.OR.WQ.EQ.'LSF'.OR.SQ.EQ.0) WRITE(OUTLYNE,4)
                      IF(WQ.EQ.'SPSF'.OR.WQ.EQ.'SLSF') WRITE(OUTLYNE,5)
                      CALL SHOWIT(0)
                      WRITE(OUTLYNE,1) PIXAR2
                      CALL SHOWIT(0)
                      WRITE(OUTLYNE,2) PIXAR1
                      CALL SHOWIT(0)
                      WRITE(OUTLYNE,10) XMEEN
                      CALL SHOWIT(0)
                      WRITE(OUTLYNE,20) YMEEN
                      CALL SHOWIT(0)
                      WRITE(OUTLYNE,30) XRMS
                      CALL SHOWIT(0)
                      WRITE(OUTLYNE,40) YRMS
                      CALL SHOWIT(0)
C
C     FINISHED
                      DEALLOCATE(SUMMERIT,STAT=ALLOERR)
C
C******************************************************************************
                  ELSE
C     LSF CENTROIDING
                      IF(((DBLE(PIXAR2)/2.0D0)-DBLE(INT(DBLE(PIXAR2)/2.0D0)))
     1                .NE.0.0D0) THEN
C     PIXAR2 ODD
                          PIX2=1
                      ELSE
C     PIXAR2 EVEN
                          PIX2=0
                      END IF
C
C     SET UP THE SHIFT VALUE
                      SHIFT=PIXAR1
                      IF(PGR.LT.((PIXAR1*PIXAR2)+PIXAR1)) THEN
                          OUTLYNE='THE CURRENT PSF WINDOW DEFINED BY THE PGR VALUE'
                          CALL SHOWIT(1)
                          OUTLYNE='IS TOO SMALL FOR THE CURRENTLY DEFINED PIXEL VALUES'
                          CALL SHOWIT(1)
                          OUTLYNE='NO CENTROID LINEARITY ANALYSIS CAN BE PERFORMED'
                          CALL SHOWIT(1)
                          CALL MACFAL
                          DEALLOCATE(XLSF,YLSF,XCEN,YCEN,STAT=ALLOERR)
                          RETURN
                      END IF
C
C     ALLOCATE THE DUMMY ARRAYS FOR CENTROIDING
                      DEALLOCATE(XDUM,YDUM,STAT=ALLOERR)
                      ALLOCATE(XDUM(1:(PIXAR1*PIXAR2)),
     1                YDUM(1:(PIXAR1*PIXAR2)),STAT=ALLOERR)
C     CALCULATE CENTROIDS BASED ON THE ARITHMETIC MEAN ALGORITHM
C     AND THE CURRENT DEFINED WINDOW
C
C**********************************************************************
                      IF(PIX2.EQ.1) THEN
C     NUMBER OF PIXELS IS ODD (ODD NUMBER OF PIXELS)
C**********************************************************************
                          DUMIND=((PIXAR1*PIXAR2)-1)/2
                          DEALLOCATE(XCEN,YCEN,STAT=ALLOERR)
                          ALLOCATE(XCEN(1:SHIFT),YCEN(1:SHIFT),STAT=ALLOERR)
C
                          JKSUMX1=0.0D0
                          JKSUMY1=0.0D0
                          JKSUMX2=0.0D0
                          JKSUMY2=0.0D0
                          DO I=1,SHIFT
C     BEFORE WE DO A SHIFT POSITION, ZERO THE SUMMING VARIABLES
C     ZERO THE CENTROID ARRAYS AND LOAD THE XDUM AND YDUM ARRAYS
                              XCEN(I)=0.0D0
                              YCEN(I)=0.0D0
                              JXSUM=0.0D0
                              JYSUM=0.0D0
                              XTOT1=0.0D0
                              YTOT1=0.0D0
                              TOT1=0.0D0
C
                              JJ=0
                              DO JJJ=-DUMIND,DUMIND
                                  JJ=JJ+1
                                  XDUM(JJ)=XLSF(JJJ-((SHIFT-1)/2)-1+I)
                                  YDUM(JJ)=YLSF(JJJ-((SHIFT-1)/2)-1+I)
                              END DO
C

                              DO J=1,PIXAR2
C     WE ARE NOW DOING A CENTROID SUMMATION OVER PIXEL J
                                  XSUM=0.0D0
                                  YSUM=0.0D0

                                  DO K=1,PIXAR1

C     HERE WE SUM THE VALUES OF PIXAR1 SUB-PIXELS OF PIXEL J
                                      XSUM=XSUM+XDUM(K+((J-1)*PIXAR1))
                                      YSUM=YSUM+YDUM(K+((J-1)*PIXAR1))
                                  END DO

C     HERE WE FORM THE WEIGHTED SUMMATIONS FOR CENTROID CALCULATIONS
                                  JXSUM=JXSUM+(DBLE(J)*XSUM)
                                  JYSUM=JYSUM+(DBLE(J)*YSUM)
                                  TOT1=TOT1+DBLE(J)
                                  XTOT1=XTOT1+XSUM
                                  YTOT1=YTOT1+YSUM
                              END DO

                              XCEN(I)=(JXSUM/XTOT1)-(TOT1/DBLE(PIXAR2))
     1                        -(((DBLE(SHIFT-1)/2.0D0)+1.0D0-DBLE(I))/(DBLE(SHIFT)))
     1                        -PSFXCENT
C
                              YCEN(I)=(JYSUM/YTOT1)-(TOT1/DBLE(PIXAR2))
     1                        -(((DBLE(SHIFT-1)/2.0D0)+1.0D0-DBLE(I))/(DBLE(SHIFT)))
     1                        -PSFYCENT

                              WRITE(OUTLYNE,2009) I,XCEN(I),YCEN(I)
                              CALL SHOWIT(0)
 2009                         FORMAT(I2,1X,G13.6,1X,G13.6)
                              JKSUMX1=JKSUMX1+(XCEN(I))
                              JKSUMY1=JKSUMY1+(YCEN(I))
                              JKSUMX2=JKSUMX2+(XCEN(I)**2)
                              JKSUMY2=JKSUMY2+(YCEN(I)**2)
                          END DO
                          DEVX=DSQRT(
     1                    (JKSUMX2-((JKSUMX1**2)/DBLE(PIXAR1)))
     2                    /(DBLE(PIXAR1)-1.0D0)
     3                    )
                          DEVY=DSQRT(
     1                    (JKSUMY2-((JKSUMY1**2)/DBLE(PIXAR1)))
     2                    /(DBLE(PIXAR1)-1.0D0)
     3                    )
                          IF(WQ.EQ.'PSF'.OR.WQ.EQ.'SPSF') WRITE(OUTLYNE,3)
                          IF(WQ.EQ.'LSF'.OR.WQ.EQ.'SLSF'.OR.SQ.EQ.0) WRITE(OUTLYNE,6)
                          CALL SHOWIT(0)
                          IF(WQ.EQ.'PSF'.OR.WQ.EQ.'LSF'.OR.SQ.EQ.0) WRITE(OUTLYNE,4)
                          IF(WQ.EQ.'SPSF'.OR.WQ.EQ.'SLSF') WRITE(OUTLYNE,5)
                          CALL SHOWIT(0)
                          WRITE(OUTLYNE,1) PIXAR2
                          CALL SHOWIT(0)
                          WRITE(OUTLYNE,2) PIXAR1
                          CALL SHOWIT(0)
                          WRITE(OUTLYNE,10) JKSUMX1/DBLE(PIXAR1)
                          CALL SHOWIT(0)
                          WRITE(OUTLYNE,20) JKSUMY1/DBLE(PIXAR1)
                          CALL SHOWIT(0)
                          WRITE(OUTLYNE,30) DEVX
                          CALL SHOWIT(0)
                          WRITE(OUTLYNE,40) DEVY
                          CALL SHOWIT(0)
                      END IF
C**********************************************************************
                      IF(PIX2.EQ.0) THEN
C     NUMBER OF PIXELS IS EVEN
C**********************************************************************
                          DUMIND=((PIXAR1*PIXAR2))/2
                          DEALLOCATE(XCEN,YCEN,STAT=ALLOERR)
                          ALLOCATE(XCEN(1:SHIFT+1),YCEN(1:SHIFT+1),STAT=ALLOERR)
C
                          JKSUMX1=0.0D0
                          JKSUMY1=0.0D0
                          JKSUMX2=0.0D0
                          JKSUMY2=0.0D0
                          DO I=1,SHIFT+1
C     BEFORE WE DO A SHIFT POSITION, ZERO THE SUMMING VARIABLES
C     ZERO THE CENTROID ARRAYS AND LOAD THE XDUM AND YDUM ARRAYS
                              XCEN(I)=0.0D0
                              YCEN(I)=0.0D0
                              JXSUM=0.0D0
                              JYSUM=0.0D0
                              XTOT1=0.0D0
                              YTOT1=0.0D0
                              TOT1=0.0D0
C
                              JJ=0
                              DO JJJ=-DUMIND,DUMIND-1
                                  JJ=JJ+1
                                  XDUM(JJ)=XLSF(JJJ+((SHIFT+1)/2)-I+1)
                                  YDUM(JJ)=YLSF(JJJ+((SHIFT+1)/2)-I+1)
                              END DO
C
                              DO J=1,PIXAR2
C     WE ARE NOW DOING A CENTROID SUMMATION OVER PIXEL J
                                  XSUM=0.0D0
                                  YSUM=0.0D0

                                  DO K=1,PIXAR1

C     HERE WE SUM THE VALUES OF PIXAR1 SUB-PIXELS OF PIXEL J
                                      XSUM=XSUM+XDUM(K+((J-1)*PIXAR1))
                                      YSUM=YSUM+YDUM(K+((J-1)*PIXAR1))
                                  END DO

C     HERE WE FORM THE WEIGHTED SUMMATIONS FOR CENTROID CALCULATIONS
                                  JXSUM=JXSUM+(DBLE(J)*XSUM)
                                  JYSUM=JYSUM+(DBLE(J)*YSUM)
                                  TOT1=TOT1+DBLE(J)
                                  XTOT1=XTOT1+XSUM
                                  YTOT1=YTOT1+YSUM
                              END DO

                              XCEN(I)=(JXSUM/XTOT1)-(TOT1/DBLE(PIXAR2))
     1                        +(((DBLE(SHIFT)/2.0D0)-DBLE(I-1))
     1                        /DBLE(SHIFT))
     2                        -PSFXCENT
C
                              YCEN(I)=(JYSUM/YTOT1)-(TOT1/DBLE(PIXAR2))
     1                        +(((DBLE(SHIFT)/2.0D0)-DBLE(I-1))
     1                        /DBLE(SHIFT))
     2                        -PSFYCENT

                              WRITE(OUTLYNE,2009) I,XCEN(I),YCEN(I)
                              CALL SHOWIT(0)
                              JKSUMX1=JKSUMX1+(XCEN(I))
                              JKSUMY1=JKSUMY1+(YCEN(I))
                              JKSUMX2=JKSUMX2+(XCEN(I)**2)
                              JKSUMY2=JKSUMY2+(YCEN(I)**2)
                          END DO
                          DEVX=DSQRT(
     1                    (JKSUMX2-((JKSUMX1**2)/DBLE(PIXAR1)))
     2                    /(DBLE(PIXAR1)-1.0D0)
     3                    )
                          DEVY=DSQRT(
     1                    (JKSUMY2-((JKSUMY1**2)/DBLE(PIXAR1)))
     2                    /(DBLE(PIXAR1)-1.0D0)
     3                    )
                          IF(WQ.EQ.'PSF'.OR.WQ.EQ.'SPSF') WRITE(OUTLYNE,3)
                          IF(WQ.EQ.'LSF'.OR.WQ.EQ.'SLSF'.OR.SQ.EQ.0) WRITE(OUTLYNE,6)
                          CALL SHOWIT(0)
                          IF(WQ.EQ.'PSF'.OR.WQ.EQ.'LSF'.OR.SQ.EQ.0) WRITE(OUTLYNE,4)
                          IF(WQ.EQ.'SPSF'.OR.WQ.EQ.'SLSF') WRITE(OUTLYNE,5)
                          CALL SHOWIT(0)
                          WRITE(OUTLYNE,1) PIXAR2
                          CALL SHOWIT(0)
                          WRITE(OUTLYNE,2) PIXAR1
                          CALL SHOWIT(0)
                          WRITE(OUTLYNE,10) JKSUMX1/DBLE(PIXAR1)
                          CALL SHOWIT(0)
                          WRITE(OUTLYNE,20) JKSUMY1/DBLE(PIXAR1)
                          CALL SHOWIT(0)
                          WRITE(OUTLYNE,30) DEVX
                          CALL SHOWIT(0)
                          WRITE(OUTLYNE,40) DEVY
                          CALL SHOWIT(0)
                      END IF
C
                      DEALLOCATE(SHIFTER,IPSF,PSF,XLSF,YLSF,XCEN,YCEN,
     1                XDUM,YDUM,SUMMERIT,STAT=ALLOERR)
                      RETURN
                  END IF
              ELSE
C     NO ANALYSIS CAN BE DONE
                  OUTLYNE=
     1            'NO CENTROID ANALYSIS CAN BE PERFORMED.'
                  CALL SHOWIT(1)
                  IF(SQ.EQ.0) OUTLYNE=
     1            '"CENTROID" REQUIRES A PSF.DAT FILE TO EXIST'
                  IF(WQ.EQ.'S') OUTLYNE=
     1            '"CENTROID" REQUIRES AN SPSF.DAT FILE TO EXIST'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  DEALLOCATE(SHIFTER,IPSF,PSF,XLSF,YLSF,XCEN,YCEN,
     1            XDUM,YDUM,SUMMERIT,STAT=ALLOERR)
                  RETURN
              END IF
          END IF
      END
C SUB GETPSF.FOR

      SUBROUTINE GETPSF(IGET,JGET,V9,IRRER,IRRER2)
          USE GLOBALS
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE GETPSF
C     CALLED BY CMDER FOR GET PSF
C
          CHARACTER EEOOFF*4,UNITS*2
C
          REAL*8 EXTENT,SPACING,PSFXCENT,PSFYCENT
     1    ,PSF,DDUMMY,V9
C
          REAL*8 TPSF,PEAKER,CRAYX,CRAYY
C
          INTEGER PPGR,IPSF,I,J,IGET,JGET
     1    ,L,M,IID,JJD
C
          DIMENSION PSF(:,:),IPSF(:,:),TPSF(:,:)
C
          INTEGER MMM,PIXAR1,PIXAR2,DUMMY,II
C
          COMMON/TOYSTORY/PIXAR1,PIXAR2
C
          ALLOCATABLE :: IPSF,PSF,TPSF
C
          INTEGER ALLOERR
C
          LOGICAL EXIS51,OPEN51,IRRER,IRRER2
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datspd.inc'
          INCLUDE 'datsp1.inc'
C
C
C     DOES THE PSF FILE EXIST WITH AN EXISTING PSF
          EXIS51=.FALSE.
          INQUIRE(FILE=trim(HOME)//'PSF.DAT',EXIST=EXIS51)
          IF(EXIS51) THEN
C     DO ANALYSIS
              OPEN51=.FALSE.
              INQUIRE(FILE=trim(HOME)//'PSF.DAT',OPENED=OPEN51)
              IF(.NOT.OPEN51) THEN
C     OPEN FILE
                  OPEN(UNIT=51,ACCESS='SEQUENTIAL',BLANK='NULL'
     1              ,FORM='FORMATTED',FILE=trim(HOME)//'PSF.DAT'
     2              ,STATUS='UNKNOWN')
                  REWIND(UNIT=51)
              ELSE
                  REWIND(UNIT=51)
              END IF
C     NOW ALLOCATE AND LOAD ARRAYS
C     READ SOME STUFF
              II=1
              READ(51,1001) PSFTAG
 1001         FORMAT(A12)
              READ(51,1002) PSFLI
 1002         FORMAT(A78)
              READ(51,1003) MMM,PGR
              PPGR=(PGR-1)/2
              IRRER2=.FALSE.
              IF(IGET.LT.-PPGR.OR.IGET.GT.PPGR.OR.
     1        JGET.LT.-PPGR.OR.JGET.GT.PPGR) THEN
                  IRRER2=.TRUE.
                  RETURN
              END IF
 1003         FORMAT(I10,1X,I10)
              READ(51,1004) UNITS
 1004         FORMAT(A2)
              READ(51,1008) EXTENT,SPACING
              READ(51,1008) PSFXCENT,PSFYCENT
              READ(51,1008) CRAYX,CRAYY
 1008         FORMAT(E15.8,1x,E15.8)
 1005         FORMAT(2I4,I8)
 1006         FORMAT(I4,G23.15)
              DEALLOCATE(PSF,IPSF,TPSF,STAT=ALLOERR)
              ALLOCATE(IPSF(1:MMM,1:MMM),TPSF(-PPGR:PPGR,-PPGR:PPGR),
     1        PSF(1:MMM,1:MMM),STAT=ALLOERR)
C     LOAD PSF/LSF ARRAYS
              II=1
              DO JJD=1,MMM
                  DO IID=1,MMM
                      READ(51,1005) I,J,IPSF3(II)
                      II=II+1
                  END DO
              END DO
C
              II=1
              DO J=1,MMM
                  DO I=1,MMM
                      PSF(I,J)=DBLE(IPSF3(II))
                      II=II+1
                  END DO
              END DO
C
              M=-PPGR
              DO J=-(PPGR)+((MMM+2)/2),(PPGR)+((MMM+2)/2)
                  L=-PPGR
                  DO I=-(PPGR)+((MMM+2)/2),(PPGR)+((MMM+2)/2)
                      TPSF(L,M)=PSF(I,J)
                      L=L+1
                  END DO
                  M=M+1
              END DO
C     READ IN THE XLSF
              DO I=-PPGR,PPGR
                  READ(51,1006) DUMMY,DDUMMY
              END DO
C     READ IN THE YLSF
              DO I=-PPGR,PPGR
                  READ(51,1006) DUMMY,DDUMMY
              END DO
              READ(51,*) PEAKER
C     NEXT LINE BETTER BE EOF
              READ(51,*) EEOOFF
C     LSF ARRAYS LOADED
              REWIND(UNIT=51)
              CALL CLOSE_FILE(51,1)
              IRRER=.FALSE.
              V9=TPSF(IGET,JGET)*PEAKER/32676.0D0
          ELSE
              IRRER=.TRUE.
C     NO ANALYSIS CAN BE DONE, NO PSF.DAT EXISTS
              DEALLOCATE(PSF,IPSF,STAT=ALLOERR)
              RETURN
          END IF
          RETURN
      END
C SUB GETPSFSUM.FOR

      SUBROUTINE GETPSFSUM(V9,IRRER)
          USE GLOBALS
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE GETPSFSUM
C
          CHARACTER EEOOFF*4,UNITS*2
C
          REAL*8 EXTENT,SPACING,PSFXCENT,PSFYCENT
     1    ,PSF,DDUMMY,V9
C
          REAL*8 TPSF,PEAKER,CRAYX,CRAYY
C
          INTEGER PPGR,IPSF,I,J,L,M,IID,JJD
C
          DIMENSION PSF(:,:),IPSF(:,:),TPSF(:,:)
C
          INTEGER MMM,PIXAR1,PIXAR2,DUMMY,II
C
          COMMON/TOYSTORY/PIXAR1,PIXAR2
C
          ALLOCATABLE :: IPSF,PSF,TPSF
C
          INTEGER ALLOERR
C
          LOGICAL EXIS51,OPEN51,IRRER
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datspd.inc'
          INCLUDE 'datsp1.inc'
C
C
C     DOES THE PSF FILE EXIST WITH AN EXISTING PSF
          EXIS51=.FALSE.
          INQUIRE(FILE=trim(HOME)//'PSF.DAT',EXIST=EXIS51)
          IF(EXIS51) THEN
C     DO ANALYSIS
              OPEN51=.FALSE.
              INQUIRE(FILE=trim(HOME)//'PSF.DAT',OPENED=OPEN51)
              IF(.NOT.OPEN51) THEN
C     OPEN FILE
                  OPEN(UNIT=51,ACCESS='SEQUENTIAL',BLANK='NULL'
     1              ,FORM='FORMATTED',FILE=trim(HOME)//'PSF.DAT'
     2              ,STATUS='UNKNOWN')
                  REWIND(UNIT=51)
              ELSE
                  REWIND(UNIT=51)
              END IF
C     NOW ALLOCATE AND LOAD ARRAYS
C     READ SOME STUFF
              II=1
              READ(51,1001) PSFTAG
 1001         FORMAT(A12)
              READ(51,1002) PSFLI
 1002         FORMAT(A78)
              READ(51,1003) MMM,PGR
              PPGR=(PGR-1)/2
 1003         FORMAT(I10,1X,I10)
              READ(51,1004) UNITS
 1004         FORMAT(A2)
              READ(51,1008) EXTENT,SPACING
              READ(51,1008) PSFXCENT,PSFYCENT
              READ(51,1008) CRAYX,CRAYY
 1008         FORMAT(E15.8,1x,E15.8)
 1005         FORMAT(2I4,I8)
 1006         FORMAT(I4,G23.15)
              DEALLOCATE(PSF,IPSF,TPSF,STAT=ALLOERR)
              ALLOCATE(IPSF(1:MMM,1:MMM),TPSF(-PPGR:PPGR,-PPGR:PPGR),
     1        PSF(1:MMM,1:MMM),STAT=ALLOERR)
C     LOAD PSF/LSF ARRAYS
              II=1
              DO JJD=1,MMM
                  DO IID=1,MMM
                      READ(51,1005) I,J,IPSF3(II)
                      II=II+1
                  END DO
              END DO
C
              II=1
              DO J=1,MMM
                  DO I=1,MMM
                      PSF(I,J)=DBLE(IPSF3(II))
                      II=II+1
                  END DO
              END DO
C
              M=-PPGR
              DO J=-(PPGR)+((MMM+2)/2),(PPGR)+((MMM+2)/2)
                  L=-PPGR
                  DO I=-(PPGR)+((MMM+2)/2),(PPGR)+((MMM+2)/2)
                      TPSF(L,M)=PSF(I,J)
                      L=L+1
                  END DO
                  M=M+1
              END DO
C     READ IN THE XLSF
              DO I=-PPGR,PPGR
                  READ(51,1006) DUMMY,DDUMMY
              END DO
C     READ IN THE YLSF
              DO I=-PPGR,PPGR
                  READ(51,1006) DUMMY,DDUMMY
              END DO
              READ(51,*) PEAKER
C     NEXT LINE BETTER BE EOF
              READ(51,*) EEOOFF
C     LSF ARRAYS LOADED
              REWIND(UNIT=51)
              CALL CLOSE_FILE(51,1)
              IRRER=.FALSE.
              V9=0.0D0
              DO J=-PPGR,PPGR
                  DO I=-PPGR,PPGR
                      V9=V9+TPSF(I,J)*PEAKER/32676.0D0
                  END DO
              END DO
          ELSE
              IRRER=.TRUE.
C     NO ANALYSIS CAN BE DONE, NO PSF.DAT EXISTS
              DEALLOCATE(PSF,IPSF,STAT=ALLOERR)
              RETURN
          END IF
          RETURN
C
      END
C SUB GETPSFFWHM.FOR

      SUBROUTINE GETPSFFWHM(V9,IRRER,K)
          USE GLOBALS
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE GETPSFSUM
C
          CHARACTER EEOOFF*4,UNITS*2
C
          REAL*8 EXTENT,SPACING,PSFXCENT,PSFYCENT
     1    ,PSF,DDUMMYX,DDUMMYY,V9,V8
C
          REAL*8 TPSF,PEAKER,CRAYX,CRAYY
C
          INTEGER PPGR,IPSF,I,J,K
     1    ,L,M,IID,JJD
C
          DIMENSION PSF(:,:),IPSF(:,:),TPSF(:,:),DUMMYX(:),DDUMMYX(:)
     1    ,DUMMYY(:),DDUMMYY(:)
C
          INTEGER MMM,PIXAR1,PIXAR2,DUMMYX,II,DUMMYY
C
          COMMON/TOYSTORY/PIXAR1,PIXAR2
C
          ALLOCATABLE :: IPSF,PSF,TPSF,DUMMYX,DUMMYY,DDUMMYX,DDUMMYY
C
          INTEGER ALLOERR
C
          LOGICAL EXIS51,OPEN51,IRRER
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datspd.inc'
          INCLUDE 'datsp1.inc'
C
C
C     DOES THE PSF FILE EXIST WITH AN EXISTING PSF
          EXIS51=.FALSE.
          INQUIRE(FILE=trim(HOME)//'PSF.DAT',EXIST=EXIS51)
          IF(EXIS51) THEN
C     DO ANALYSIS
              OPEN51=.FALSE.
              INQUIRE(FILE=trim(HOME)//'PSF.DAT',OPENED=OPEN51)
              IF(.NOT.OPEN51) THEN
C     OPEN FILE
                  OPEN(UNIT=51,ACCESS='SEQUENTIAL',BLANK='NULL'
     1              ,FORM='FORMATTED',FILE=trim(HOME)//'PSF.DAT'
     2              ,STATUS='UNKNOWN')
                  REWIND(UNIT=51)
              ELSE
                  REWIND(UNIT=51)
              END IF
C     NOW ALLOCATE AND LOAD ARRAYS
C     READ SOME STUFF
              II=1
              READ(51,1001) PSFTAG
 1001         FORMAT(A12)
              READ(51,1002) PSFLI
 1002         FORMAT(A78)
              READ(51,1003) MMM,PGR
              PPGR=(PGR-1)/2
 1003         FORMAT(I10,1X,I10)
              READ(51,1004) UNITS
 1004         FORMAT(A2)
              READ(51,1008) EXTENT,SPACING
              READ(51,1008) PSFXCENT,PSFYCENT
              READ(51,1008) CRAYX,CRAYY
 1008         FORMAT(E15.8,1x,E15.8)
 1005         FORMAT(2I4,I8)
 1006         FORMAT(I4,G23.15)
              DEALLOCATE(PSF,IPSF,TPSF,STAT=ALLOERR)
              ALLOCATE(IPSF(1:MMM,1:MMM),TPSF(-PPGR:PPGR,-PPGR:PPGR),
     1        PSF(1:MMM,1:MMM),DUMMYX(1:PGR),DDUMMYX(1:PGR),
     1        DUMMYY(1:PGR),DDUMMYY(1:PGR),
     1        STAT=ALLOERR)
C     LOAD PSF/LSF ARRAYS
              II=1
              DO JJD=1,MMM
                  DO IID=1,MMM
                      READ(51,1005) I,J,IPSF3(II)
                      II=II+1
                  END DO
              END DO
C
              II=1
              DO J=1,MMM
                  DO I=1,MMM
                      PSF(I,J)=DBLE(IPSF3(II))
                      II=II+1
                  END DO
              END DO
C
              M=-PPGR
              DO J=-(PPGR)+((MMM+2)/2),(PPGR)+((MMM+2)/2)
                  L=-PPGR
                  DO I=-(PPGR)+((MMM+2)/2),(PPGR)+((MMM+2)/2)
                      TPSF(L,M)=PSF(I,J)
                      L=L+1
                  END DO
                  M=M+1
              END DO
C     READ IN THE XLSF
              DO I=1,PGR
                  READ(51,1006) DUMMYX(I),DDUMMYX(I)
              END DO
C     READ IN THE YLSF
              DO I=1,PGR
                  READ(51,1006) DUMMYY(I),DDUMMYY(I)
              END DO
              READ(51,*) PEAKER
C     NEXT LINE BETTER BE EOF
              READ(51,*) EEOOFF
C     LSF ARRAYS LOADED
              REWIND(UNIT=51)
              CALL CLOSE_FILE(51,1)
              IRRER=.FALSE.
              V9=0.0D0
              IF(K.EQ.1) THEN
C       X
                  DO I=2,PGR
                      IF(DDUMMYX(I-1).LE.0.5D0.AND.DDUMMYX(I).GE.0.5D0) THEN
                          V8=(0.5D0-DDUMMYX(I-1))/(DDUMMYX(I)-DDUMMYX(I-1))
                          V8=DBLE(I-1)+(V9*(DBLE(I-(I-1))))
                      END IF
                  END DO
                  DO I=2,PGR
                      IF(DDUMMYX(I-1).GE.0.5D0.AND.DDUMMYX(I).LE.0.5D0) THEN
                          V9=(DDUMMYX(I-1)-0.5D0)/(DDUMMYX(I-1)-DDUMMYX(I))
                          V9=DBLE(I-1)+(V9*(DBLE(I-(I-1))))
                      END IF
                  END DO
                  V9=DABS(V9-V8)*GRI
              ELSE
C       Y
C       X
                  DO I=2,PGR
                      IF(DDUMMYY(I-1).LE.0.5D0.AND.DDUMMYY(I).GE.0.5D0) THEN
                          V8=(0.5D0-DDUMMYY(I-1))/(DDUMMYY(I)-DDUMMYY(I-1))
                          V8=DBLE(I-1)+(V9*(DBLE(I-(I-1))))
                      END IF
                  END DO
                  DO I=2,PGR
                      IF(DDUMMYY(I-1).GE.0.5D0.AND.DDUMMYY(I).LE.0.5D0) THEN
                          V9=(DDUMMYY(I-1)-0.5D0)/(DDUMMYY(I-1)-DDUMMYY(I))
                          V9=DBLE(I-1)+(V9*(DBLE(I-(I-1))))
                      END IF
                  END DO
                  V9=DABS(V9-V8)*GRI
              END IF





          ELSE
              IRRER=.TRUE.
C     NO ANALYSIS CAN BE DONE, NO PSF.DAT EXISTS
              DEALLOCATE(PSF,IPSF,DUMMYX,DDUMMYX,DUMMYY,DDUMMYY,STAT=ALLOERR)
              RETURN
          END IF
C
      END
C SUB STREAK.FOR

      SUBROUTINE STREAK
          USE GLOBALS
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE STREAK.FOR.
C     CALLED BY CMDER FOR COMMAND STREAK
C
          REAL XPLT,YPLT,FPLT,EXTENT
     1    ,SPACING,CNTX,CNTY
          DIMENSION XPLT(:),YPLT(:),FPLT(:,:)
C
          ALLOCATABLE :: XPLT,YPLT,FPLT
C
          COMMON/PSFPLOTER/EXTENT,SPACING,CNTX,CNTY
C
          CHARACTER EEOOFF*4,UNITS*2
C
          REAL*8 SPACER,EXTENT1,SPACING1
C
          REAL*8 CRAYX,CRAYY,SCENX,SCENY,PSFYCENT,PSFXCENT,DUMMY2,
     2    IV,FTOT,FSUM,IIII,PSF,PEAKER,SPSF
C
          INTEGER IPSF,I,J,K,KKSTART,
     1    KKSTOP,L,III,JJJ,MMM,PCOUNT,PRG,II
C
          DIMENSION PSF(:,:),SPSF(:,:),SCENX(:),SCENY(:),IPSF(:,:)
C
          INTEGER DUMMY,SPGR,IDUM
C
          ALLOCATABLE :: PSF,SPSF,SCENX,SCENY,IPSF
C
          INTEGER ALLOERR
C
          LOGICAL EXIS52,OPEN52
          LOGICAL EXIS51,OPEN51
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datspd.inc'
          INCLUDE 'datsp1.inc'
          PRG=TGR-1
C
          IF(WQ.NE.'X'.AND.WQ.NE.'Y'.AND.WQ.NE.'XY'.AND.WQ.NE.'WRITE'
     1    .AND.WQ.NE.'PLOT'.AND.STI.NE.1) THEN
              OUTLYNE='INVALID QUALIFIER USED WITH "STREAK"'
              CALL SHOWIT(1)
              OUTLYNE='"STREAK" REQUIRES "X", "Y", "XY", "WRITE" OR "PLOT"'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(WQ.EQ.'WRITE'.OR.WQ.EQ.'PLOT') THEN
              IF(WS(1:2).NE.'ON'.AND.WS(1:3).NE.'OFF'.AND.STI.NE.1) THEN
                  OUTLYNE='"STREAK PLOT" AND "STREAK WRITE" REQUIRE STRING INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='"ON" OR "OFF"'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
C     IF WQ EQ 'PLOT' OR 'WRITE'
          IF(WQ.EQ.'PLOT'.OR.WQ.EQ.'WRITE') THEN
              IF(WQ.EQ.'PLOT') THEN
                  IF(SN.EQ.1) THEN
                      OUTLYNE='"STREAK PLOT" TAKES NO NUMERIC INPUT"'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  IF(STI.EQ.1) THEN
                      OUTLYNE='"STREAK PLOT" TURNS STREAKED PSF PLOTTING "ON" OR "OFF"'
                      CALL SHOWIT(1)
                      IF(STKPLT) OUTLYNE='"STREAK PSF PLOTTING IS CURRENTLY "ON"'
                      IF(.NOT.STKPLT) OUTLYNE='"STREAK PSF PLOTTING IS CURRENTLY "OFF"'
                      RETURN
                  END IF
                  IF(WS(1:2).EQ.'ON')  STKPLT=.TRUE.
                  IF(WS(1:3).EQ.'OFF') STKPLT=.FALSE.
                  RETURN
              END IF
C
              IF(WQ.EQ.'WRITE') THEN
                  IF(SN.EQ.1) THEN
                      OUTLYNE='"STREAK WRITE" TAKES NO NUMERIC INPUT"'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  IF(STI.EQ.1) THEN
                      OUTLYNE=
     1                '"STREAK WRITE" TURNS STREAKED PSF PLOTTING "ON" OR "OFF"'
                      CALL SHOWIT(1)
                      IF(STKWRT) OUTLYNE='"STREAK PSF WRITING IS CURRENTLY "ON"'
                      IF(.NOT.STKWRT) OUTLYNE='"STREAK PSF WRITING IS CURRENTLY "OFF"'
                      CALL SHOWIT(1)
                      RETURN
                  END IF
                  IF(WS(1:2).EQ.'ON')  STKWRT=.TRUE.
                  IF(WS(1:3).EQ.'OFF') STKWRT=.FALSE.
                  RETURN
              END IF
C
          ELSE
C     NOT STREAK PLOT OR STREAK WRITE
          END IF
C
C
          IF(STI.EQ.1) THEN
              OUTLYNE=
     1        '"STREAK" STREAKS THE STORED PSF IN THE X OR Y DIRECTION'
              CALL SHOWIT(1)
              OUTLYNE=
     1        'QUALIFIER WORD IS "X" OR "Y" FOR THE STREAK DIRECTION'
              CALL SHOWIT(1)
              OUTLYNE=
     1        'NUMERIC WORD #1 IS THE NUMBER OF "GRI" UNITS TO STREAK'
              CALL SHOWIT(1)
              RETURN
          END IF
C
          IF(SST.EQ.1) THEN
              OUTLYNE='"STREAK" OTHER THAN WITH "PLOT" OR "WRITE"'
              CALL SHOWIT(1)
              OUTLYNE='TAKES NO STRING INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SQ.EQ.0) THEN
              OUTLYNE='"STREAK" REQUIRES EXPLICIT QUALIFIER WORD INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(S1.EQ.0) THEN
              OUTLYNE='"STREAK" REQUIRES EXPLICIT NUMERIC WORD #1 INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
              OUTLYNE='"STREAK" ONLY TAKES NUMERIC WORDS #1 AND #2 INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SQ.EQ.1.AND.WQ.NE.'X'.AND.WQ.NE.'Y'.AND.WQ.NE.'XY') THEN
              OUTLYNE=
     1        '"STREAK" REQUIRES "X", "Y" OR "XY" AS QUALIFIER INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(W1.LT.0.0D0) THEN
              OUTLYNE=
     1        'NUMERIC WORD #1 MAY NOT BE NEGATIVE'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
          STREAKIT=INT(W1)
C
C     DOES THE PSF FILE EXIST WITH AN EXISTING PSF
          EXIS51=.FALSE.
          INQUIRE(FILE=trim(HOME)//'PSF.DAT',EXIST=EXIS51)
          IF(EXIS51) THEN
C     DO ANALYSIS
              OPEN51=.FALSE.
              INQUIRE(FILE=trim(HOME)//'PSF.DAT',OPENED=OPEN51)
              IF(.NOT.OPEN51) THEN
C     OPEN FILE
                  OPEN(UNIT=51,ACCESS='SEQUENTIAL',BLANK='NULL'
     1              ,FORM='FORMATTED',FILE=trim(HOME)//'PSF.DAT'
     2              ,STATUS='UNKNOWN')
                  REWIND(UNIT=51)
              ELSE
                  REWIND(UNIT=51)
              END IF
C     NOW ALLOCATE AND LOAD ARRAYS
C     READ SOME STUFF
              READ(51,1001) PSFTAG
 1001         FORMAT(A12)
              READ(51,1002) PSFLI
 1002         FORMAT(A78)
              READ(51,1003) MMM,PGR
              SPGR=PRG+INT(STREAKIT*2.0D0)
 1003         FORMAT(I10,1X,I10)
              READ(51,9004) UNITS
 9004         FORMAT(A2)
              READ(51,1008) EXTENT1,SPACING1
              READ(51,1008) PSFXCENT,PSFYCENT
              READ(51,1008) CRAYX,CRAYY
 1008         FORMAT(E15.8,1x,E15.8)
 9005         FORMAT(2I4,I8)
 9006         FORMAT(I4,G23.15)
              DEALLOCATE(PSF,SPSF,SCENX,SCENY,IPSF,STAT=ALLOERR)
              ALLOCATE(PSF(PRG,PRG),SPSF(SPGR,SPGR)
     1        ,SCENX(SPGR)
     2        ,SCENY(SPGR),IPSF(PRG,PRG),STAT=ALLOERR)
              III=SPGR
              SCENX(1:III)=0.0D0
              SCENY(1:III)=0.0D0
C     ZERO THE ADD ARRAY
              I=SPGR
              J=SPGR
              SPSF(1:J,1:I)=0.0D0
              III=PRG
              JJJ=PRG
              PSF(1:III,1:JJJ)=0.0D0
              IPSF(1:III,1:JJJ)=0
C
C     LOAD PSF ARRAY
              II=1
              DO I=1,PRG
                  DO J=1,PRG
                      READ(51,9005) IDUM,IDUM,IPSF3(II)
                      PSF(J,I)=DBLE(IPSF3(II))
                      II=II+1
                  END DO
              END DO
C     READ IN THE XLSF
              DO I=1,PGR
                  READ(51,9006) DUMMY,DUMMY2
              END DO
C     READ IN THE YLSF
              DO I=1,PGR
                  READ(51,9006) DUMMY,DUMMY2
              END DO
C     NEXT LINE BETTER BE EOF
              READ(51,*) EEOOFF
C     LSF ARRAYS LOADED
              REWIND(UNIT=51)
              CALL CLOSE_FILE(51,1)
C     PSF ARRAY LOADED.
C     NOW ADD PSF TO SPSF TO BUILD THE STREAKED PSF
              IF(WQ.EQ.'X'.OR.WQ.EQ.'Y') THEN
                  IF(WQ.EQ.'X') THEN
                      L=INT(STREAKIT)
                      DO K=0,INT(STREAKIT)*2
                          DO I=1,PRG
                              DO J=1,PRG
                                  SPSF(K+J,L+I)=SPSF(K+J,L+I)+PSF(J,I)
                              END DO
                          END DO
                      END DO
                  END IF
                  IF(WQ.EQ.'Y') THEN
                      K=INT(STREAKIT)
                      DO L=0,INT(STREAKIT)*2
                          DO I=1,PRG
                              DO J=1,PRG
                                  SPSF(K+J,L+I)=SPSF(K+J,L+I)+PSF(J,I)
                              END DO
                          END DO
                      END DO
                  END IF
C
              END IF
C
              IF(WQ.EQ.'XY') THEN
                  KKSTART=0
                  KKSTOP=SPGR
                  DO K=0,INT(STREAKIT)*2
                      DO I=1,PRG
                          DO J=1,PRG
                              SPSF(K+J,K+I)=SPSF(K+J,K+I)+PSF(J,I)
                          END DO
                      END DO
                  END DO
              END IF
              PEAKER=-1.0D300
              DO JJJ=1,SPGR
                  DO III=1,SPGR
                      IF(SPSF(III,JJJ).GT.PEAKER) PEAKER=SPSF(III,JJJ)
                  END DO
              END DO
              DO JJJ=1,SPGR
                  DO III=1,SPGR
                      IF(PEAKER.NE.0.0D0) SPSF(III,JJJ)=DNINT((SPSF(III,JJJ)/PEAKER)
     1                *32767.0D0)
                      IF(PEAKER.EQ.0.0D0) SPSF(III,JJJ)=DNINT(SPSF(III,JJJ))
                  END DO
              END DO
C     CALCULATE THE TRUE SPSF X-CENTROID POSITION
              PSFXCENT=0.0D0
              IIII=0.0D0
              IV=0.0D0
              FTOT=0.0D0
              DO III=1,SPGR
                  IIII=IIII+1.0D0
                  IV=IV+IIII
                  FSUM=0.0D0
                  DO JJJ=1,SPGR
                      FSUM=FSUM+SPSF(III,JJJ)
                  END DO
                  FTOT=FTOT+FSUM
                  SCENX(III)=FSUM
                  PSFXCENT=PSFXCENT+(IIII*FSUM)
              END DO
              PSFXCENT=-((PSFXCENT/FTOT)-(IV/DBLE(SPGR)))
C
C     CALCULATE THE TRUE PSF Y-CENTROID POSITION
              PSFYCENT=0.0D0
              IIII=0.0D0
              IV=0.0D0
              FTOT=0.0D0
              DO JJJ=1,SPGR
                  IIII=IIII+1.0D0
                  IV=IV+IIII
                  FSUM=0.0D0
                  DO III=1,SPGR
                      FSUM=FSUM+SPSF(III,JJJ)
                  END DO
                  FTOT=FTOT+FSUM
                  SCENY(JJJ)=FSUM
                  PSFYCENT=PSFYCENT+(IIII*FSUM)
              END DO
              PSFYCENT=-((PSFYCENT/FTOT)-(IV/DBLE(SPGR)))
C
C     NOW DO THE WRITING OF THE STREAKED PSF
              IF(.NOT.STKWRT) THEN
                  EXIS52=.FALSE.
                  OPEN52=.FALSE.
                  INQUIRE(FILE=trim(HOME)//'SPSF.DAT',EXIST=EXIS52)
                  INQUIRE(FILE=trim(HOME)//'SPSF.DAT',OPENED=OPEN52)
                  IF(OPEN52.AND.EXIS52) CALL CLOSE_FILE(52,0)
                  EXIS52=.FALSE.
                  INQUIRE(FILE=trim(HOME)//'SPSF.DAT',EXIST=EXIS52)
                  IF(EXIS52) THEN
                      OPEN(UNIT=52,ACCESS='SEQUENTIAL',BLANK='NULL'
     1                  ,FORM='FORMATTED',FILE=trim(HOME)//'SPSF.DAT'
     2                  ,STATUS='UNKNOWN')
                      CALL CLOSE_FILE(52,1)
                  END IF
              END IF
              IF(STKWRT) THEN
C     WRITE FILE
                  EXIS52=.FALSE.
                  OPEN52=.FALSE.
                  INQUIRE(FILE=trim(HOME)//'SPSF.DAT',EXIST=EXIS52)
                  INQUIRE(FILE=trim(HOME)//'SPSF.DAT',OPENED=OPEN52)
                  IF(OPEN52.AND.EXIS52) CALL CLOSE_FILE(52,0)
                  EXIS52=.FALSE.
                  INQUIRE(FILE=trim(HOME)//'SPSF.DAT',EXIST=EXIS52)
                  IF(EXIS52) THEN
                      OPEN(UNIT=52,ACCESS='SEQUENTIAL',BLANK='NULL'
     1                  ,FORM='FORMATTED',FILE=trim(HOME)//'SPSF.DAT'
     2                  ,STATUS='UNKNOWN')
                      CALL CLOSE_FILE(52,0)
                  END IF
C     OPEN NEW FILE
                  OPEN(UNIT=52,ACCESS='SEQUENTIAL',BLANK='NULL'
     1              ,FORM='FORMATTED',FILE=trim(HOME)//'SPSF.DAT'
     2              ,STATUS='UNKNOWN')
C     WRITE FILE HEADER
                  WRITE(52,9001)
 9001             FORMAT('STKPSF.DAT')
                  WRITE(52,9002) INT(STREAKIT)
 9002             FORMAT('CURRENT PSF.DAT DATA STREAKED BY ',I6,' SUB-PIXELS')
                  WRITE(52,1003) SPGR,PGR
                  IF(SYSTEM1(6).EQ.1.0D0) WRITE(52,1004)
                  IF(SYSTEM1(6).EQ.2.0D0) WRITE(52,1005)
                  IF(SYSTEM1(6).EQ.3.0D0) WRITE(52,1006)
                  IF(SYSTEM1(6).EQ.4.0D0) WRITE(52,1007)
 1004             FORMAT('IN')
 1005             FORMAT('CM')
 1006             FORMAT('MM')
 1007             FORMAT('M')
                  WRITE(52,1008) EXTENT1,SPACING1
                  WRITE(52,1008) PSFXCENT*DBLE(SPACING1),PSFYCENT*DBLE(SPACING1)
                  WRITE(52,1008) CRAYX,CRAYY
                  DO JJJ=1,SPGR
                      DO III=1,SPGR
                          WRITE(52,2000) III,JJJ,INT(DNINT(SPSF(III,JJJ)))
                      END DO
                  END DO
 2000             FORMAT(2I4,I8)
 3000             FORMAT(I4,G23.15)
                  PEAKER=-1.0D300
                  DO III=1,PGR
                      IF(SCENX(((SPGR-PGR)/2)+III).GT.PEAKER) PEAKER=
     1                SCENX(((SPGR-PGR)/2)+III)
                  END DO
                  DO III=1,PGR
                      WRITE(52,3000) III,
     1                (SCENX(((SPGR-PGR)/2)+III)/PEAKER)*1.0D0
                  END DO
                  PEAKER=-1.0D300
                  DO III=1,PGR
                      IF(SCENY(((SPGR-PGR)/2)+III).GT.PEAKER) PEAKER=
     1                SCENY(((SPGR-PGR)/2)+III)
                  END DO
                  DO III=1,PGR
                      WRITE(52,3000) III,
     1                (SCENY(((SPGR-PGR)/2)+III)/PEAKER)*1.0D0
                  END DO
                  WRITE(52,1009)
 1009             FORMAT('EOF')
              ELSE
C     DON'T WRITE THE FILE
              END IF
C     NOW DO THE PLOTTING OF THE STREAKED PSF
              IF(STKPLT) THEN
C     PLOT THE STREAKED FILE
C
                  DEALLOCATE(XPLT,YPLT,FPLT,STAT=ALLOERR)
                  ALLOCATE(
     1            XPLT(PGR),YPLT(PGR),FPLT(PGR,PGR),STAT=ALLOERR)
                  DO III=1,PGR
                      XPLT(III)=0.0
                      YPLT(III)=0.0
                      DO JJJ=1,PGR
                          FPLT(JJJ,III)=0.0
                      END DO
                  END DO
C     SET UP THE XPLT AND YPLT ARRAYS TO SPGRXSPGR
                  YPLT(1)=-1.0
                  XPLT(1)=-1.0
                  SPACER=2.0D0/DBLE(PGR-1)
                  DO III=2,PGR
                      XPLT(III)=XPLT(III-1)+SNGL(SPACER)
                      YPLT(III)=YPLT(III-1)+SNGL(SPACER)
                  END DO
                  DO III=1,PGR
                      XPLT(III)=XPLT(III)*0.95
                      YPLT(III)=YPLT(III)*0.95
                  END DO
C
                  DO III=1,PGR
                      DO JJJ=1,PGR
                          FPLT(III,JJJ)=
     1                    SNGL(SPSF(((SPGR-PGR)/2)+III,((SPGR-PGR)/2)+JJJ))
                      END DO
                  END DO
C                       END IF
C
C     PLOT FUNCTION
                  PCOUNT=PGR
                  IF(PSFEXT) THEN
                      IF(S2.EQ.0) CALL FFT2(PCOUNT,XPLT,YPLT,FPLT,0)
                      IF(S2.EQ.1) CALL FFT2(PCOUNT,XPLT,YPLT,FPLT,1)
                  END IF
              ELSE
C     DON'T PLOT THE FILE
              END IF
C     FINISHED STREAK ANALYSIS
          ELSE
              OUTLYNE=
     1        'NO PSF STREAKING CAN BE PERFORMED.'
              CALL SHOWIT(1)
              OUTLYNE=
     1        '"STREAK" REQUIRES A PSF AND A PSF.DAT FILE TO EXIST'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
          DEALLOCATE(IPSF,PSF,SPSF,SCENX,
     1    SCENY,XPLT,YPLT,FPLT,STAT=ALLOERR)
          CALL CLOSE_FILE(51,1)
          CALL CLOSE_FILE(52,1)
          RETURN
      END
C SUB OLDLSF.FOR
      SUBROUTINE OLDLSF
C
          IMPLICIT NONE
C
C     CALLED BY CMDER FOR COMMAND OLSF
C     THESE DISPLAY EXISTING GEOMETRICAL LINE SPREAD FUNCTIONS
C
          REAL*8 IL1,IL2,INTEN,IX1,IX2,IY1,IY2,USDEL,NEWW1
     2    ,SPDELX,SPDELY,LSF1,LSF2,LSFP1,LSFP2,MTHETA,DELZ
C
          INTEGER I,NOPNTS
C
          LOGICAL OPEN66
C
          CHARACTER J_UN*13
C
          LOGICAL EXTGLSF,LSFCENT
C
          COMMON/GLSFEXT/EXTGLSF,LSFCENT
C
          REAL GDTAP(1:102),GDTAV(1:102)
C
          INTEGER IE,ENNL
C
          COMMON/GLSFPASS/ENNL,GDTAP,GDTAV,MTHETA,SPDELX,SPDELY,DELZ
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datspd.inc'
          INCLUDE 'datsp1.inc'
          EXTGLSF=.FALSE.
          LSFCENT=.FALSE.
          ENNL=0
C     STRING INPUT
          IF(SST.EQ.1) THEN
              OUTLYNE=
     1        '"OLSF", "OLSF (ACC)", "OLSF (CACC) AND "OLSF (CENT)"'
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
     1            '"INVALID QUALIFIER USED WITH "OLSF"'
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
     1            '"OLSF AND "OLSF CENT"'
                  CALL SHOWIT(1)
                  OUTLYNE=
     1            'REQUIRE NUMERIC WORD 1 INPUT GREATER THAN 2.0'
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
     1            '"OLSF (ACC)" AND "OLSF (CACC)",'
                  CALL SHOWIT(1)
                  OUTLYNE='REQUIRE EXPLICIT NUMERIC WORD #1 INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
C     NUMERIC WORDS 2 TO 5
          IF(DF2.EQ.0.OR.DF3.EQ.0.OR.DF4.EQ.0.OR.DF5.EQ.0) THEN
              OUTLYNE='"OLSF" TAKES NO NUMERIC WORD 2 THROUGH 5 INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
          IF(.NOT.LSF) THEN
              OUTLYNE=
     1        'NO CURRENT LINE SPREAD FUNCTION EXISTS'
              CALL SHOWIT(1)
              OUTLYNE='NO ACTION TAKEN'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
C     INTERPOLATE AND OUTPUT THE DATA REQUESTED
          OPEN66=.FALSE.
          INQUIRE(FILE=LIBSPO//'LSF.DAT',OPENED=OPEN66)
          IF(.NOT.OPEN66)
     1      OPEN(UNIT=66,ACCESS='DIRECT',FILE=LIBSPO//'LSF.DAT',
     1      FORM='UNFORMATTED',RECL=(10*NRECL),STATUS='UNKNOWN')
C
C     NOW DO APPROPRIATE OUTPUT BASED UPON COMMAND
C     FIRST, IF WQ IS ACC OR CACC, NO OUTPUT, JUST CALCULATE THE
C     INTENSITY FOR THE REQUESTED IMAGE POSITION (+ OR -)
          IF(WQ.EQ.'ACC'.OR.WQ.EQ.'CACC') THEN
C
              INTEN=0.0D0
C     W1 IS THE POSITION FOR WHICH WE WISH TO KNOW THE INTENSITY
              READ(UNIT=66,REC=1) ILSF
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
C
              REG(40)=REG(9)
              REG(9)=INTEN
              CALL CLOSE_FILE(66,1)
              RETURN
          ELSE
C     WQ NOT ACC OR CACC
          END IF
          IF(WQ.NE.'ACC'.AND.WQ.NE.'CACC') THEN
C     NOW INTERPOLATE THE VALUES AND DISPLAY THEM
C
              IF(WQ.NE.'ACC'.AND.WQ.NE.'CACC') THEN
                  WRITE(OUTLYNE,20)
                  CALL SHOWIT(0)
 20               FORMAT('LINE SPREAD FUNCTION')
              ELSE
              END IF
C     PRINT ORIENTATION OF KNIFE EDGE
              WRITE(OUTLYNE,30) MTHETA*(180.0D0/PII)
              CALL SHOWIT(0)
 30           FORMAT('SCAN ORIENTATION = ',F7.2,' DEGREE(S)')
C
              IF(WQ.EQ.'CENT'.AND.SYSTEM1(30).LT.3.0D0) THEN
                  IF(SYSTEM1(6).EQ.1.0D0) J_UN='INCH(S)'
                  IF(SYSTEM1(6).EQ.2.0D0) J_UN='CENTIMETER(S)'
                  IF(SYSTEM1(6).EQ.3.0D0) J_UN='MILLIMETER(S)'
                  IF(SYSTEM1(6).EQ.4.0D0) J_UN='METER(S)'
                  IF(W3.NE.0.0D0) WRITE(OUTLYNE,40) W3,J_UN
                  IF(W3.NE.0.0D0) CALL SHOWIT(0)
 40               FORMAT('APPLIED DEFOCUS (Z-DIRECTION) = ',G13.6,1X,A13)
              ELSE
              END IF
              IF(WQ.NE.'CENT'.AND.SYSTEM1(30).LT.3.0D0) THEN
                  IF(SYSTEM1(6).EQ.1.0D0) J_UN='INCH(S)'
                  IF(SYSTEM1(6).EQ.2.0D0) J_UN='CENTIMETER(S)'
                  IF(SYSTEM1(6).EQ.3.0D0) J_UN='MILLIMETER(S)'
                  IF(SYSTEM1(6).EQ.4.0D0) J_UN='METER(S)'
                  IF(W2.NE.0.0D0) WRITE(OUTLYNE,41) W2,J_UN
                  IF(W2.NE.0.0D0) CALL SHOWIT(0)
 41               FORMAT('APPLIED X-OFFSET = ',G13.6,1X,A13)
                  IF(W3.NE.0.0D0) WRITE(OUTLYNE,42) W3,J_UN
                  IF(W3.NE.0.0D0) CALL SHOWIT(0)
 42               FORMAT('APPLIED Y-OFFSET = ',G13.6,1X,A13)
                  IF(W3.NE.0.0D0) WRITE(OUTLYNE,40) W4,J_UN
                  IF(W3.NE.0.0D0) CALL SHOWIT(0)
              ELSE
              END IF
              IF(WC.EQ.'CENT') THEN
                  IF(SYSTEM1(6).EQ.1.0D0) J_UN='INCH(S)'
                  IF(SYSTEM1(6).EQ.2.0D0) J_UN='CENTIMETER(S)'
                  IF(SYSTEM1(6).EQ.3.0D0) J_UN='MILLIMETER(S)'
                  IF(SYSTEM1(6).EQ.4.0D0) J_UN='METER(S)'
                  WRITE(OUTLYNE,50) CENTX,J_UN
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,60) CENTX,J_UN
                  CALL SHOWIT(0)
 50               FORMAT('CENTROID X-COORDINATE = ',G13.6,1X,A13)
 60               FORMAT('CENTROID Y-COORDINATE = ',G13.6,1X,A13)
              ELSE
              END IF
              IF(WC.NE.'CENT') THEN
                  IF(SYSTEM1(6).EQ.1.0D0) J_UN='INCH(S)'
                  IF(SYSTEM1(6).EQ.2.0D0) J_UN='CENTIMETER(S)'
                  IF(SYSTEM1(6).EQ.3.0D0) J_UN='MILLIMETER(S)'
                  IF(SYSTEM1(6).EQ.4.0D0) J_UN='METER(S)'
                  WRITE(OUTLYNE,70) REFRY(1,NEWIMG),J_UN
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,80) REFRY(2,NEWIMG),J_UN
                  CALL SHOWIT(0)
 70               FORMAT('CHIEF RAY X-COORDINATE = ',G13.6,1X,A13)
 80               FORMAT('CHIEF RAY Y-COORDINATE = ',G13.6,1X,A13)
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
              WRITE(OUTLYNE,902) J_UN
              CALL SHOWIT(0)
 902          FORMAT('RELATIVE IMAGE INTENSITY',4X,'   POSITION    ',1X,A13)
C
C     W1 SPECIFIES THE NUMBER OF DATA POINTS
C     SO THERE WILL BE (ROUNDED TO THE NEAREST INTEGER)
              IF(DF1.EQ.1) THEN
                  NOPNTS=21
              ELSE
                  NOPNTS=INT(W1)
              END IF
              READ(UNIT=66,REC=1) ILSF
              READ(UNIT=66,REC=2) LSF1,LSF2
              READ(UNIT=66,REC=ILSF) LSFP1,LSFP2
              USDEL=DABS((LSFP2-LSF2)/DBLE(NOPNTS-1))
              NEWW1=LSF2
              NP=NOPNTS+1
              IE=-1
 987          CONTINUE
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
              NEWW1=NEWW1+USDEL
              INTEN=0.0D0
              GO TO 987
 988          CONTINUE
              LSF=.TRUE.
 903          FORMAT(5X,G14.6,13X,G14.6)
          ELSE
C     WQ = TO ACC OR CACC
          END IF
C
C     INTENSITY CALCULATIONS ARE COMPLETE
C
          CALL CLOSE_FILE(66,1)
          RETURN
      END
C SUB FFA.FOR
      FUNCTION FFA()
C
          IMPLICIT NONE
C
          REAL*8 FN1,FN2,FN3,FN4,FN5,FN6,FN7,FN8,FN9
     3    ,FN10,X,Y,FFA,FN11,FN12,FN13,FN14,FN15
C
          INTEGER I
C
          COMMON/FFFA/X,Y,I
C
C       FUNCTIONS FOR RECTANGULAR FUNCTION FIT
          FN1(X,Y)=1.0D0
C
          FN2(X,Y)=X
          FN3(X,Y)=Y
C
          FN4(X,Y)=X**2
          FN5(X,Y)=X*Y
          FN6(X,Y)=Y**2
C
          FN7(X,Y)=X**3
          FN8(X,Y)=(X**2)*Y
          FN9(X,Y)=X*(Y**2)
          FN10(X,Y)=Y**3
C
          FN11(X,Y)=X**4
          FN12(X,Y)=(X**3)*Y
          FN13(X,Y)=(X**2)*(Y**2)
          FN14(X,Y)=X*(Y**3)
          FN15(X,Y)=Y**4
C
          IF(I.EQ.1) FFA=FN1(X,Y)
          IF(I.EQ.2) FFA=FN2(X,Y)
          IF(I.EQ.3) FFA=FN3(X,Y)
          IF(I.EQ.4) FFA=FN4(X,Y)
          IF(I.EQ.5) FFA=FN5(X,Y)
          IF(I.EQ.6) FFA=FN6(X,Y)
          IF(I.EQ.7) FFA=FN7(X,Y)
          IF(I.EQ.8) FFA=FN8(X,Y)
          IF(I.EQ.9) FFA=FN9(X,Y)
          IF(I.EQ.10) FFA=FN10(X,Y)
          IF(I.EQ.11) FFA=FN11(X,Y)
          IF(I.EQ.12) FFA=FN12(X,Y)
          IF(I.EQ.13) FFA=FN13(X,Y)
          IF(I.EQ.14) FFA=FN14(X,Y)
          IF(I.EQ.15) FFA=FN15(X,Y)
          RETURN
      END
C SUB FFAX.FOR
      FUNCTION FFAX()
C
          IMPLICIT NONE
C
          REAL*8 FN1,FN2,FN3,FN4,FN5,FN6,FN7,FN8,FN9
     3    ,FN10,X,Y,FFAX,FN11,FN12,FN13,FN14,FN15
C
          COMMON/FFAAER/X,Y
          COMMON/FFAIER/I
C
          INTEGER I
C
C       FUNCTIONS FOR RECTANGULAR FUNCTION FIT
          FN1(X,Y)=0.0D0
C
          FN2(X,Y)=1.0D0
          FN3(X,Y)=0.0D0
C
          FN4(X,Y)=X*2.0D0
          FN5(X,Y)=Y
          FN6(X,Y)=0.0D0
C
          FN7(X,Y)=3.0*(X**2)
          FN8(X,Y)=2.0D0*X*Y
          FN9(X,Y)=(Y**2)
          FN10(X,Y)=0.0D0
C
          FN11(X,Y)=4.0D0*(X**3)
          FN12(X,Y)=3.0D0*(X**2)*Y
          FN13(X,Y)=2.0D0*X*(Y**2)
          FN14(X,Y)=Y**3
          FN15(X,Y)=0.0D0
C
          IF(I.EQ.1) FFAX=FN1(X,Y)
          IF(I.EQ.2) FFAX=FN2(X,Y)
          IF(I.EQ.3) FFAX=FN3(X,Y)
          IF(I.EQ.4) FFAX=FN4(X,Y)
          IF(I.EQ.5) FFAX=FN5(X,Y)
          IF(I.EQ.6) FFAX=FN6(X,Y)
          IF(I.EQ.7) FFAX=FN7(X,Y)
          IF(I.EQ.8) FFAX=FN8(X,Y)
          IF(I.EQ.9) FFAX=FN9(X,Y)
          IF(I.EQ.10) FFAX=FN10(X,Y)
          IF(I.EQ.11) FFAX=FN11(X,Y)
          IF(I.EQ.12) FFAX=FN12(X,Y)
          IF(I.EQ.13) FFAX=FN13(X,Y)
          IF(I.EQ.14) FFAX=FN14(X,Y)
          IF(I.EQ.15) FFAX=FN15(X,Y)
          RETURN
      END
C SUB FFAY.FOR
      FUNCTION FFAY()
C
          IMPLICIT NONE
C
          REAL*8 FN1,FN2,FN3,FN4,FN5,FN6,FN7,FN8,FN9
     3    ,FN10,X,Y,FFAY,FN11,FN12,FN13,FN14,FN15
C
          INTEGER I
C
          COMMON/FFAAER/X,Y
          COMMON/FFAIER/I
C
C       FUNCTIONS FOR RECTANGULAR FUNCTION FIT
          FN1(X,Y)=0.0D0
C
          FN2(X,Y)=0.0D0
          FN3(X,Y)=1.0D0
C
          FN4(X,Y)=0.0D0
          FN5(X,Y)=X
          FN6(X,Y)=2.0D0*Y
C
          FN7(X,Y)=0.0D0
          FN8(X,Y)=(X**2)
          FN9(X,Y)=2.0D0*X*Y
          FN10(X,Y)=3.0D0*(Y**2)
C
          FN11(X,Y)=0.0D0
          FN12(X,Y)=X**3
          FN13(X,Y)=2.0D0*(X**2)*Y
          FN14(X,Y)=3.0D0*X*(Y**2)
          FN15(X,Y)=4.0D0*(Y**3)
C
          IF(I.EQ.1) FFAY=FN1(X,Y)
          IF(I.EQ.2) FFAY=FN2(X,Y)
          IF(I.EQ.3) FFAY=FN3(X,Y)
          IF(I.EQ.4) FFAY=FN4(X,Y)
          IF(I.EQ.5) FFAY=FN5(X,Y)
          IF(I.EQ.6) FFAY=FN6(X,Y)
          IF(I.EQ.7) FFAY=FN7(X,Y)
          IF(I.EQ.8) FFAY=FN8(X,Y)
          IF(I.EQ.9) FFAY=FN9(X,Y)
          IF(I.EQ.10) FFAY=FN10(X,Y)
          IF(I.EQ.11) FFAY=FN11(X,Y)
          IF(I.EQ.12) FFAY=FN12(X,Y)
          IF(I.EQ.13) FFAY=FN13(X,Y)
          IF(I.EQ.14) FFAY=FN14(X,Y)
          IF(I.EQ.15) FFAY=FN15(X,Y)
          RETURN
      END
C SUB ENEXFT.FOR
      SUBROUTINE ENEXFT(NNTOT)
          USE GLOBALS
          USE SVDSUB
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE ENEXFT. THIS IS THE SUBROUTINE WHICH
C       HANDLES THE MAPPING FUNCTION FITTING FROM THE ENTRANCE
C       TO THE EXIT SURFACE
C
C     THE DAREA1 FACTOR IS THE AREA IN THE ENTRANCE SURFACE WHICH IS
C     REPRESENTED BY A UNIT AREA IN THE REFERENCE SURFACE. SINCE
C     WE TRACE RAYS AT EQUAL INTERVALS IN THE REFERENCE SURFACE,
C     DAREA1 IS THE FACTOR WHICH WE MUST MULTIPLY THE ENERGY TERM
C     BY IN THE REFERENCE SURFACE SO THAT IT REPRESENTS THE CORRECT
C     AMOUNT OF COLLECTED ENERGY.
C
C     IF DAREA1 WAS 1.5, THEN A UNIT AREA IN THE REFERENCE SURFACE
C     WAS REPRESENTED BY AN AREA OF 1.5 IN THE ENTRANCE SURFACE
C     AN COLLECTED 1.5 NORMALIZED UNITS OF ENERGY.
C
C     THE DAREA2 FACTOR IS THE AREA IN THE EXIT PUPIL SURFACE WHICH IS
C     REPRESENTED BY A UNIT AREA IN THE REFERENCE SURFACE. SINCE
C     WE WISH TO REPRESENT RAYS TRACED OVER EQUAL AREAS IN THE
C     EXIT PUPIL SURFACE, THEN IF 1 AREA UNIT IN THE REFERENCE
C     SURFACE REPRESENTS DAREA2 UNITS IN THE EXIT PUPIL SURFACE,
C     WE MUST DIVIDE THE ENERGY TERM BY DAREA2.
C
C     IF DAREA2 WAS 1.5, THEN A UNIT AREA IN THE EXIT PUPIL SURFACE
C     WAS REPRESENTED BY AN AREA OF 1/1.5 UNITS IN THE REFERNCE
C     SURFACE. THIS IS THE INVERSE OF THE DAREA1 DEFINITION
C
          INTEGER NNTOT
C
          DOUBLE PRECISION SOLCOR1,WTOL,FX,FY,TERM11,
     2    X(1:15),AAVAL,IAVAL,ABVAL,IBVAL
     3    ,ZAR,ZBR,RADD,SOLCOR2
C
          INTEGER FI
C
          COMMON/FFFA/FX,FY,FI
C
          INTEGER FA_I,I,J,II
C
          DOUBLE PRECISION ZA1,ZB1,RFX1,RFY1,TERM,FFA,
     1    TERMA,TERMB,TERM1
     2    ,FFAX,FFAY,DA1,DB1,DAREA1,DA2,DB2,DAREA2,
     3    ZA2,ZB2
     4    ,RFX2,RFY2,FA_X,FA_Y
C
          DOUBLE PRECISION
     1    HITER1A,HITER1B,HITER1C,HITER1D,HITER1E,HITER1F,HITER1G
     2    ,HITER1H,HITER1I,HITER1J,HITER1K
          COMMON/HITME1/
     1    HITER1A,HITER1B,HITER1C,HITER1D,HITER1E,HITER1F,HITER1G
     2    ,HITER1H,HITER1I,HITER1J,HITER1K
C
          COMMON/FFAAER/FA_X,FA_Y
          COMMON/FFAIER/FA_I
C
          EXTERNAL FFA,FFAX,FFAY
C
          COMMON/SVDB2/X,WTOL

          DOUBLE PRECISION W,V,U,
     2    B,CFA1,CFB1
     3    ,CFA2,CFB2,
     1    RACUMA1,
     1    RCCOLA1,RACUMB1,RCCOLB1,
     2    RACUMA2,
     3    RCCOLA2,RACUMB2,RCCOLB2
          DIMENSION W(15),V(15,15),U(15,15),
     2    B(15),CFA1(15),CFB1(15)
     3    ,CFA2(15),CFB2(15),
     1    RACUMA1(15,15),
     1    RCCOLA1(15),RACUMB1(15,15),RCCOLB1(15),
     2    RACUMA2(15,15),
     3    RCCOLA2(15),RACUMB2(15,15),RCCOLB2(15)
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datspd.inc'
          INCLUDE 'datsp1.inc'
          AAVAL=0.0D0
          IAVAL=0.0D0
          ABVAL=0.0D0
          IBVAL=0.0D0
          DO I=1,15
              W(I)=0.0D0
              B(I)=0.0D0
              CFA1(I)=0.0D0
              CFA2(I)=0.0D0
              CFB1(I)=0.0D0
              CFB2(I)=0.0D0
              RCCOLA1(I)=0.0D0
              RCCOLA2(I)=0.0D0
              RCCOLB1(I)=0.0D0
              RCCOLB2(I)=0.0D0
              DO J=1,15
                  V(I,J)=0.0D0
                  U(I,J)=0.0D0
                  RACUMA1(I,J)=0.0D0
                  RACUMA2(I,J)=0.0D0
                  RACUMB1(I,J)=0.0D0
                  RACUMB2(I,J)=0.0D0
              END DO
          END DO
          SOLCOR1=0.0D0
          SOLCOR2=0.0D0
          DAREA1=0.0D0
          DAREA2=0.0D0
C
C       SETUP THE CALCULATION FOR THE 15 Ai AND Bi COEFFICIENTS
C       PROCEED WITH FITTING
C       RFX1=(ASSIGN A RAY X-HT FROM THE SPOT DIAG AT REF SURF)
C     DSPOTT(5)
C       RFY1=(ASSIGN A RAY Y-HT FROM THE SPOT DIAG AT REF SURF)
C     DSPOTT(6)
C       ZA1=(ASSIGN A RAY X-HT FROM THE SPOT DIAG AT SURF NEWOBJ+1)
C     DSPOTT(14)
C       ZB1=(ASSIGN A RAY Y-HT FROM THE SPOT DIAG AT SURF NEWOBJ+1)
C     DSPOTT(15)
C
C     ALSO
C
C       SETUP THE CALCULATION FOR THE 15 Ai AND Bi COEFFICIENTS
C       PROCEED WITH FITTING
C       RFX2=(ASSIGN A RAY X-HT FROM THE SPOT DIAG AT REF SURF)
C     DSPOTT(5)
C       RFY2=(ASSIGN A RAY Y-HT FROM THE SPOT DIAG AT REF SURF)
C     DSPOTT(6)
C       ZA2=(ASSIGN A RAY X-HT AT THE TANGENT PLANE TO THE EXIT PUPIL)
C       ZB2=(ASSIGN A RAY Y-HT AT THE TANGENT PLANE TO THE EXIT PUPIL)
C
          DO II=1,NNTOT-1
              IF(DSPOTT(7,II).EQ.0.0D0.AND.DSPOTT(12,II).NE.0.0D0) THEN
C     RAY DID NOT FAIL
C     TREAT X AND Y COORDINATES AS THEY PROJECT ALONG THE LOCAL Z-AXIS
C     ON TO THE XY-PLANE
                  RFX1=DSPOTT(5,II)-REFRY(1,NEWREF)
                  RFY1=DSPOTT(6,II)-REFRY(2,NEWREF)
                  ZA1=DSPOTT(14,II)-REFRY(1,NEWOBJ+1)
                  ZB1=DSPOTT(15,II)-REFRY(1,NEWOBJ+1)
C     TREAT X AND Y COORDINATES AS THEY PROJECT ALONG THE LOCAL Z-AXIS
C     ON TO THE XY-PLANE
                  RFX2=DSPOTT(5,II)-REFRY(1,NEWREF)
                  RFY2=DSPOTT(6,II)-REFRY(2,NEWREF)
C     HITEX1 RETURNS THE X AND Y RAY HEIGHTS IN THE TANGENT PLANE
C     TO THE REFERENCE SPHERE, THE X AND Y COORDINATES OF THE
C     REFERENCE RAY IN THAT PLANE AND THE RADIUS OF THE REFERENCE SPHERE
C     ZAR,ZBR AND RADD ARE USED LATER FOR THE CONVERTION TO SOLID
C     ANGLE AT THE EXIT PUPIL.
C     THESE ARE AT A PLANE ALREADY
                  HITER1A=ZA2
                  HITER1B=ZB2
                  HITER1C=ZAR
                  HITER1D=ZBR
                  HITER1E=RADD
                  HITER1F=DSPOTT(1,II)
                  HITER1G=DSPOTT(2,II)
                  HITER1H=DSPOTT(3,II)
                  HITER1I=DSPOTT(22,II)
                  HITER1J=DSPOTT(23,II)
                  HITER1K=DSPOTT(24,II)
                  CALL HITEX1
                  ZA2=HITER1A
                  ZB2=HITER1B
                  ZAR=HITER1C
                  ZBR=HITER1D
                  RADD=HITER1E
                  DO I=1,15
C       CALCULATE COLUMN VECTOR
C       FUNCTION FFA RETURNS THE CORRECT VALUE
C       FOR THE ITH ENTRY IN THE COLUMN VECTOR
C       I RANGES FROM 1 TO 15
                      FX=RFX1
                      FY=RFY1
                      FI=I
                      TERM= FFA()
                      TERMA=TERM*ZA1
                      TERMB=TERM*ZB1
                      RCCOLA1(I)=RCCOLA1(I)+TERMA
                      RCCOLB1(I)=RCCOLB1(I)+TERMB
                      TERM1= FFA()
C       DO THE APPROPRIATE ROW FOR COLUMN I
                      FI=1
                      TERM11= TERM1*FFA()
                      RACUMA1(I,1)=RACUMA1(I,1)+TERM11
                      RACUMB1(I,1)=RACUMB1(I,1)+TERM11
                      FI=2
                      TERM11= TERM1*FFA()
                      RACUMA1(I,2)=RACUMA1(I,2)+TERM11
                      RACUMB1(I,2)=RACUMB1(I,2)+TERM11
                      FI=3
                      TERM11= TERM1*FFA()
                      RACUMA1(I,3)=RACUMA1(I,3)+TERM11
                      RACUMB1(I,3)=RACUMB1(I,3)+TERM11
                      FI=4
                      TERM11= TERM1*FFA()
                      RACUMA1(I,4)=RACUMA1(I,4)+TERM11
                      RACUMB1(I,4)=RACUMB1(I,4)+TERM11
                      FI=5
                      TERM11= TERM1*FFA()
                      RACUMA1(I,5)=RACUMA1(I,5)+TERM11
                      RACUMB1(I,5)=RACUMB1(I,5)+TERM11
                      FI=6
                      TERM11= TERM1*FFA()
                      RACUMA1(I,6)=RACUMA1(I,6)+TERM11
                      RACUMB1(I,6)=RACUMB1(I,6)+TERM11
                      FI=7
                      TERM11= TERM1*FFA()
                      RACUMA1(I,7)=RACUMA1(I,7)+TERM11
                      RACUMB1(I,7)=RACUMB1(I,7)+TERM11
                      FI=8
                      TERM11= TERM1*FFA()
                      RACUMA1(I,8)=RACUMA1(I,8)+TERM11
                      RACUMB1(I,8)=RACUMB1(I,8)+TERM11
                      FI=9
                      TERM11= TERM1*FFA()
                      RACUMA1(I,9)=RACUMA1(I,9)+TERM11
                      RACUMB1(I,9)=RACUMB1(I,9)+TERM11
                      FI=10
                      TERM11= TERM1*FFA()
                      RACUMA1(I,10)=RACUMA1(I,10)+TERM11
                      RACUMB1(I,10)=RACUMB1(I,10)+TERM11
                      FI=11
                      TERM11= TERM1*FFA()
                      RACUMA1(I,11)=RACUMA1(I,11)+TERM11
                      RACUMB1(I,11)=RACUMB1(I,11)+TERM11
                      FI=12
                      TERM11= TERM1*FFA()
                      RACUMA1(I,12)=RACUMA1(I,12)+TERM11
                      RACUMB1(I,12)=RACUMB1(I,12)+TERM11
                      FI=13
                      TERM11= TERM1*FFA()
                      RACUMA1(I,13)=RACUMA1(I,13)+TERM11
                      RACUMB1(I,13)=RACUMB1(I,13)+TERM11
                      FI=14
                      TERM11= TERM1*FFA()
                      RACUMA1(I,14)=RACUMA1(I,14)+TERM11
                      RACUMB1(I,14)=RACUMB1(I,14)+TERM11
                      FI=15
                      TERM11= TERM1*FFA()
                      RACUMA1(I,15)=RACUMA1(I,15)+TERM11
                      RACUMB1(I,15)=RACUMB1(I,15)+TERM11
                  END DO
C
                  DO I=1,15
C       CALCULATE COLUMN VECTOR
C       FUNCTION FFA RETURNS THE CORRECT VALUE
C       FOR THE ITH ENTRY IN THE COLUMN VECTOR
C       I RANGES FROM 1 TO 15
                      FX=RFX2
                      FY=RFY2
                      FI=I
                      TERM= FFA()
                      TERMA=TERM*ZA2
                      TERMB=TERM*ZB2
                      RCCOLA2(I)=RCCOLA2(I)+TERMA
                      RCCOLB2(I)=RCCOLB2(I)+TERMB
                      TERM1= FFA()
C       DO THE APPROPRIATE ROW FOR COLUMN I
                      FI=1
                      TERM11= TERM1*FFA()
                      RACUMA2(I,1)=RACUMA2(I,1)+TERM11
                      RACUMB2(I,1)=RACUMB2(I,1)+TERM11
                      FI=2
                      TERM11= TERM1*FFA()
                      RACUMA2(I,2)=RACUMA2(I,2)+TERM11
                      RACUMB2(I,2)=RACUMB2(I,2)+TERM11
                      FI=3
                      TERM11= TERM1*FFA()
                      RACUMA2(I,3)=RACUMA2(I,3)+TERM11
                      RACUMB2(I,3)=RACUMB2(I,3)+TERM11
                      FI=4
                      TERM11= TERM1*FFA()
                      RACUMA2(I,4)=RACUMA2(I,4)+TERM11
                      RACUMB2(I,4)=RACUMB2(I,4)+TERM11
                      FI=5
                      TERM11= TERM1*FFA()
                      RACUMA2(I,5)=RACUMA2(I,5)+TERM11
                      RACUMB2(I,5)=RACUMB2(I,5)+TERM11
                      FI=6
                      TERM11= TERM1*FFA()
                      RACUMA2(I,6)=RACUMA2(I,6)+TERM11
                      RACUMB2(I,6)=RACUMB2(I,6)+TERM11
                      FI=7
                      TERM11= TERM1*FFA()
                      RACUMA2(I,7)=RACUMA2(I,7)+TERM11
                      RACUMB2(I,7)=RACUMB2(I,7)+TERM11
                      FI=8
                      TERM11= TERM1*FFA()
                      RACUMA2(I,8)=RACUMA2(I,8)+TERM11
                      RACUMB2(I,8)=RACUMB2(I,8)+TERM11
                      FI=9
                      TERM11= TERM1*FFA()
                      RACUMA2(I,9)=RACUMA2(I,9)+TERM11
                      RACUMB2(I,9)=RACUMB2(I,9)+TERM11
                      FI=10
                      TERM11= TERM1*FFA()
                      RACUMA2(I,10)=RACUMA2(I,10)+TERM11
                      RACUMB2(I,10)=RACUMB2(I,10)+TERM11
                      FI=11
                      TERM11= TERM1*FFA()
                      RACUMA2(I,11)=RACUMA2(I,11)+TERM11
                      RACUMB2(I,11)=RACUMB2(I,11)+TERM11
                      FI=12
                      TERM11= TERM1*FFA()
                      RACUMA2(I,12)=RACUMA2(I,12)+TERM11
                      RACUMB2(I,12)=RACUMB2(I,12)+TERM11
                      FI=13
                      TERM11= TERM1*FFA()
                      RACUMA2(I,13)=RACUMA2(I,13)+TERM11
                      RACUMB2(I,13)=RACUMB2(I,13)+TERM11
                      FI=14
                      TERM11= TERM1*FFA()
                      RACUMA2(I,14)=RACUMA2(I,14)+TERM11
                      RACUMB2(I,14)=RACUMB2(I,14)+TERM11
                      FI=15
                      TERM11= TERM1*FFA()
                      RACUMA2(I,15)=RACUMA2(I,15)+TERM11
                      RACUMB2(I,15)=RACUMB2(I,15)+TERM11
                  END DO
C
C     RAY FAILED, GO TO NEXT RAY
              END IF
          END DO
C
C     NOW CALCULATE ALL THE Ai AND Bi COEFFICIENTS
C
C     FIRST DETERMINE THE Ai COEFFICIENTS
          DO I=1,15
              B(I)=RCCOLA1(I)
              J=1
              U(I,J)=RACUMA1(I,J)
              J=2
              U(I,J)=RACUMA1(I,J)
              J=3
              U(I,J)=RACUMA1(I,J)
              J=4
              U(I,J)=RACUMA1(I,J)
              J=5
              U(I,J)=RACUMA1(I,J)
              J=6
              U(I,J)=RACUMA1(I,J)
              J=7
              U(I,J)=RACUMA1(I,J)
              J=8
              U(I,J)=RACUMA1(I,J)
              J=9
              U(I,J)=RACUMA1(I,J)
              J=10
              U(I,J)=RACUMA1(I,J)
              J=11
              U(I,J)=RACUMA1(I,J)
              J=12
              U(I,J)=RACUMA1(I,J)
              J=13
              U(I,J)=RACUMA1(I,J)
              J=14
              U(I,J)=RACUMA1(I,J)
              J=15
              U(I,J)=RACUMA1(I,J)
          END DO
C       DO SINGULAR VALUE DECOMPOSITION
C
          CALL SVDCMPB(U,W,V,15)
C
C       SOLVE LINEAR EQUATION
C
          WTOL=0.0D0
          CALL SVBKSBB(U,W,V,15,B,X)
C
          I=1
          CFA1(I)=X(I)
          I=2
          CFA1(I)=X(I)
          I=3
          CFA1(I)=X(I)
          I=4
          CFA1(I)=X(I)
          I=5
          CFA1(I)=X(I)
          I=6
          CFA1(I)=X(I)
          I=7
          CFA1(I)=X(I)
          I=8
          CFA1(I)=X(I)
          I=9
          CFA1(I)=X(I)
          I=10
          CFA1(I)=X(I)
          I=11
          CFA1(I)=X(I)
          I=12
          CFA1(I)=X(I)
          I=13
          CFA1(I)=X(I)
          I=14
          CFA1(I)=X(I)
          I=15
          CFA1(I)=X(I)
C     Ai DONE, NOW DO Bi
          DO I=1,15
              B(I)=RCCOLB1(I)
              J=1
              U(I,J)=RACUMB1(I,J)
              J=2
              U(I,J)=RACUMB1(I,J)
              J=3
              U(I,J)=RACUMB1(I,J)
              J=4
              U(I,J)=RACUMB1(I,J)
              J=5
              U(I,J)=RACUMB1(I,J)
              J=6
              U(I,J)=RACUMB1(I,J)
              J=7
              U(I,J)=RACUMB1(I,J)
              J=8
              U(I,J)=RACUMB1(I,J)
              J=9
              U(I,J)=RACUMB1(I,J)
              J=10
              U(I,J)=RACUMB1(I,J)
              J=11
              U(I,J)=RACUMB1(I,J)
              J=12
              U(I,J)=RACUMB1(I,J)
              J=13
              U(I,J)=RACUMB1(I,J)
              J=14
              U(I,J)=RACUMB1(I,J)
              J=15
              U(I,J)=RACUMB1(I,J)
          END DO
C       DO SINGULAR VALUE DECOMPOSITION
C
          CALL SVDCMPB(U,W,V,15)
C
C       SOLVE LINEAR EQUATION
C
          WTOL=0.0D0
          CALL SVBKSBB(U,W,V,15,B,X)
C
          I=1
          CFB1(I)=X(I)
          I=2
          CFB1(I)=X(I)
          I=3
          CFB1(I)=X(I)
          I=4
          CFB1(I)=X(I)
          I=5
          CFB1(I)=X(I)
          I=6
          CFB1(I)=X(I)
          I=7
          CFB1(I)=X(I)
          I=8
          CFB1(I)=X(I)
          I=9
          CFB1(I)=X(I)
          I=10
          CFB1(I)=X(I)
          I=11
          CFB1(I)=X(I)
          I=12
          CFB1(I)=X(I)
          I=13
          CFB1(I)=X(I)
          I=14
          CFB1(I)=X(I)
          I=15
          CFB1(I)=X(I)
C     FIRST DETERMINE THE Ai COEFFICIENTS
          DO I=1,15
              B(I)=RCCOLA2(I)
              J=1
              U(I,J)=RACUMA2(I,J)
              J=2
              U(I,J)=RACUMA2(I,J)
              J=3
              U(I,J)=RACUMA2(I,J)
              J=4
              U(I,J)=RACUMA2(I,J)
              J=5
              U(I,J)=RACUMA2(I,J)
              J=6
              U(I,J)=RACUMA2(I,J)
              J=7
              U(I,J)=RACUMA2(I,J)
              J=8
              U(I,J)=RACUMA2(I,J)
              J=9
              U(I,J)=RACUMA2(I,J)
              J=10
              U(I,J)=RACUMA2(I,J)
              J=11
              U(I,J)=RACUMA2(I,J)
              J=12
              U(I,J)=RACUMA2(I,J)
              J=13
              U(I,J)=RACUMA2(I,J)
              J=14
              U(I,J)=RACUMA2(I,J)
              J=15
              U(I,J)=RACUMA2(I,J)
          END DO
C       DO SINGULAR VALUE DECOMPOSITION
C
          CALL SVDCMPB(U,W,V,15)
C
C       SOLVE LINEAR EQUATION
C
          WTOL=0.0D0
          CALL SVBKSBB(U,W,V,15,B,X)
C
          I=1
          CFA2(I)=X(I)
          I=2
          CFA2(I)=X(I)
          I=3
          CFA2(I)=X(I)
          I=4
          CFA2(I)=X(I)
          I=5
          CFA2(I)=X(I)
          I=6
          CFA2(I)=X(I)
          I=7
          CFA2(I)=X(I)
          I=8
          CFA2(I)=X(I)
          I=9
          CFA2(I)=X(I)
          I=10
          CFA2(I)=X(I)
          I=11
          CFA2(I)=X(I)
          I=12
          CFA2(I)=X(I)
          I=13
          CFA2(I)=X(I)
          I=14
          CFA2(I)=X(I)
          I=15
          CFA2(I)=X(I)
C     Ai DONE, NOW DO Bi
          DO I=1,15
              B(I)=RCCOLB2(I)
              J=1
              U(I,J)=RACUMB2(I,J)
              J=2
              U(I,J)=RACUMB2(I,J)
              J=3
              U(I,J)=RACUMB2(I,J)
              J=4
              U(I,J)=RACUMB2(I,J)
              J=5
              U(I,J)=RACUMB2(I,J)
              J=6
              U(I,J)=RACUMB2(I,J)
              J=7
              U(I,J)=RACUMB2(I,J)
              J=8
              U(I,J)=RACUMB2(I,J)
              J=9
              U(I,J)=RACUMB2(I,J)
              J=10
              U(I,J)=RACUMB2(I,J)
              J=11
              U(I,J)=RACUMB2(I,J)
              J=12
              U(I,J)=RACUMB2(I,J)
              J=13
              U(I,J)=RACUMB2(I,J)
              J=14
              U(I,J)=RACUMB2(I,J)
              J=15
              U(I,J)=RACUMB2(I,J)
          END DO
C       DO SINGULAR VALUE DECOMPOSITION
C
          CALL SVDCMPB(U,W,V,15)
C
C       SOLVE LINEAR EQUATION
C
          WTOL=0.0D0
          CALL SVBKSBB(U,W,V,15,B,X)
C
          I=1
          CFB2(I)=X(I)
          I=2
          CFB2(I)=X(I)
          I=3
          CFB2(I)=X(I)
          I=4
          CFB2(I)=X(I)
          I=5
          CFB2(I)=X(I)
          I=6
          CFB2(I)=X(I)
          I=7
          CFB2(I)=X(I)
          I=8
          CFB2(I)=X(I)
          I=9
          CFB2(I)=X(I)
          I=10
          CFB2(I)=X(I)
          I=11
          CFB2(I)=X(I)
          I=12
          CFB2(I)=X(I)
          I=13
          CFB2(I)=X(I)
          I=14
          CFB2(I)=X(I)
          I=15
          CFB2(I)=X(I)
C
C     NOW ALL THE COEFFICIENTS HAVE BEEN CALCULATED
C
C     dAREA1=(dZA1)*(dZB1)
C
C     DO THIS FOR EVERY RAY IN THE SPOT DIAGRAM
C
          DO II=1,NNTOT-1
              IF(DSPOTT(7,II).EQ.0.0D0.AND.DSPOTT(12,II).NE.0.0D0) THEN
C     RAY DID NOT FAIL
                  RFX1=DSPOTT(5,II)-REFRY(1,NEWREF)
                  RFY1=DSPOTT(6,II)-REFRY(2,NEWREF)
                  DA1=0.0D0
                  DB1=0.0D0
                  FA_X=RFX1
                  FA_Y=RFY1
                  FA_I=1
                  DA1=DA1+(CFA1(I)*FFAX())
                  DB1=DB1+(CFB1(I)*FFAY())
                  FA_I=2
                  DA1=DA1+(CFA1(I)*FFAX())
                  DB1=DB1+(CFB1(I)*FFAY())
                  FA_I=3
                  DA1=DA1+(CFA1(I)*FFAX())
                  DB1=DB1+(CFB1(I)*FFAY())
                  FA_I=4
                  DA1=DA1+(CFA1(I)*FFAX())
                  DB1=DB1+(CFB1(I)*FFAY())
                  FA_I=5
                  DA1=DA1+(CFA1(I)*FFAX())
                  DB1=DB1+(CFB1(I)*FFAY())
                  FA_I=6
                  DA1=DA1+(CFA1(I)*FFAX())
                  DB1=DB1+(CFB1(I)*FFAY())
                  FA_I=7
                  DA1=DA1+(CFA1(I)*FFAX())
                  DB1=DB1+(CFB1(I)*FFAY())
                  FA_I=8
                  DA1=DA1+(CFA1(I)*FFAX())
                  DB1=DB1+(CFB1(I)*FFAY())
                  FA_I=9
                  DA1=DA1+(CFA1(I)*FFAX())
                  DB1=DB1+(CFB1(I)*FFAY())
                  FA_I=10
                  DA1=DA1+(CFA1(I)*FFAX())
                  DB1=DB1+(CFB1(I)*FFAY())
                  FA_I=11
                  DA1=DA1+(CFA1(I)*FFAX())
                  DB1=DB1+(CFB1(I)*FFAY())
                  FA_I=12
                  DA1=DA1+(CFA1(I)*FFAX())
                  DB1=DB1+(CFB1(I)*FFAY())
                  FA_I=13
                  DA1=DA1+(CFA1(I)*FFAX())
                  DB1=DB1+(CFB1(I)*FFAY())
                  FA_I=14
                  DA1=DA1+(CFA1(I)*FFAX())
                  DB1=DB1+(CFB1(I)*FFAY())
                  FA_I=15
                  DA1=DA1+(CFA1(I)*FFAX())
                  DB1=DB1+(CFB1(I)*FFAY())
                  DAREA1=DA1*DB1
C
                  RFX2=DSPOTT(5,II)-REFRY(1,NEWREF)
                  RFY2=DSPOTT(6,II)-REFRY(2,NEWREF)
                  DA2=0.0D0
                  DB2=0.0D0
                  FA_I=1
                  FA_X=RFX2
                  FA_Y=RFY2
                  DA2=DA2+(CFA2(I)*FFAX())
                  DB2=DB2+(CFB2(I)*FFAY())
                  FA_I=2
                  DA2=DA2+(CFA2(I)*FFAX())
                  DB2=DB2+(CFB2(I)*FFAY())
                  FA_I=3
                  DA2=DA2+(CFA2(I)*FFAX())
                  DB2=DB2+(CFB2(I)*FFAY())
                  FA_I=4
                  DA2=DA2+(CFA2(I)*FFAX())
                  DB2=DB2+(CFB2(I)*FFAY())
                  FA_I=5
                  DA2=DA2+(CFA2(I)*FFAX())
                  DB2=DB2+(CFB2(I)*FFAY())
                  FA_I=6
                  DA2=DA2+(CFA2(I)*FFAX())
                  DB2=DB2+(CFB2(I)*FFAY())
                  FA_I=7
                  DA2=DA2+(CFA2(I)*FFAX())
                  DB2=DB2+(CFB2(I)*FFAY())
                  FA_I=8
                  DA2=DA2+(CFA2(I)*FFAX())
                  DB2=DB2+(CFB2(I)*FFAY())
                  FA_I=9
                  DA2=DA2+(CFA2(I)*FFAX())
                  DB2=DB2+(CFB2(I)*FFAY())
                  FA_I=10
                  DA2=DA2+(CFA2(I)*FFAX())
                  DB2=DB2+(CFB2(I)*FFAY())
                  FA_I=11
                  DA2=DA2+(CFA2(I)*FFAX())
                  DB2=DB2+(CFB2(I)*FFAY())
                  FA_I=12
                  DA2=DA2+(CFA2(I)*FFAX())
                  DB2=DB2+(CFB2(I)*FFAY())
                  FA_I=13
                  DA2=DA2+(CFA2(I)*FFAX())
                  DB2=DB2+(CFB2(I)*FFAY())
                  FA_I=14
                  DA2=DA2+(CFA2(I)*FFAX())
                  DB2=DB2+(CFB2(I)*FFAY())
                  FA_I=15
                  DA2=DA2+(CFA2(I)*FFAX())
                  DB2=DB2+(CFB2(I)*FFAY())
                  DAREA2=DA2*DB2
C
C     HERE WE ADJUST THE RAY ENERGY TERM BY MULTIPLYING
C     THE EXISTING RAY ENERGY TERM BY DAREA1 AND DIVIDING BY DAREA2
                  IF(DAREA2.NE.0.0D0) THEN
                      AAVAL=AAVAL+DABS(DAREA1/DAREA2)
                      IAVAL=IAVAL+1.0D0
                  ELSE
                      AAVAL=AAVAL+1.0D0
                      IAVAL=IAVAL+1.0D0
                  END IF
                  IF(DAREA2.NE.0.0D0)
     1            DSPOTT(12,II)=DSPOTT(12,II)*DABS(DAREA1/DAREA2)
C
C     NOW APPLY THE SOLID ANGLE CORRECTION TERM IF THE DISTANCE
C     ALONG THE CHIEF RAY FROM SURFACE NEWOBJ TO NEWOBJ+1 HAS AN
C     ABSOLUTE MAGNITUDE LESS THAN 1.0D10
C     IF THIS TEST IS NOT MET, THEN ALL AREAS IN THE TANGENT PLANE
C     TO SURFACE NEWOBJ+1 REPRESENT EQUAL SOLID ANGLES AND THEY
C     NEED NOT BE COUNTED.
C     CORRECTED SO SOLCOR1=COS(THETA) 1/10/94
C     SOLCOR1 IS THE AMOUNT OF SOLID ANGLE REPRESENTED BY ONE UNIT OF
C     AREA IN THE TANGENT PLANE OF THE NEWOBJ+1 SURFACE
C     THE EXACT OFF AXIS POSITION OF THE OBJECT POINT IS IGNORED
                  SOLCOR1=DABS((REFRY(19,NEWOBJ+1)*DSPOTT(19,II))+
     1            (REFRY(20,NEWOBJ+1)*DSPOTT(20,II))+
     2            (REFRY(21,NEWOBJ+1)*DSPOTT(21,II)))
                  DSPOTT(12,II)=DSPOTT(12,II)*SOLCOR1
C
C     NOW APPLY THE SOLID ANGLE CORRECTION TERM IF THE DISTANCE
C     ALONG THE CHIEF RAY FROM EXIT PUPIL TO NEWIMG HAS AN
C     ABSOLUTE MAGNITUDE LESS THAN 1.0D10
C     IF IT IS GREATER AND THIS TEST IS NOT MET, ALL CORRECTIONS
C     ARE ASSUMED THE SAME AND THUS NOT NEEDED.
C
C     SOLCOR2 IN THIS CASE IS THE SOLID ANGLE REPRESENTED BY A
C     UNIT AREA IN THE TANGENT PLANE OF THE EXIT PUPIL. SINCE
C     THE ENERGY TERM IS NOW ADJUSTED FOR UNIFORM AREAS IN THE
C     EXIT PUPIL WHICH ARE NOT UNIFORM SOLID ANGLES, WE NEED TO
C     DIVIDE THE ENERGY TERM BY THIS SOLCOR2
C
C     CORRECTED SO SOLCOR2=COS(THETA) ON 1/10/94
C
C     SOLCOR2 MULTIPLIES DAREA2 BUT NOW DAREA2 IS IN THE
C     DENOMINATOR SO WE DIVIDE
                  SOLCOR2=DABS((REFRY(19,NEWIMG-1)*DSPOTT(22,II))+
     1            (REFRY(20,NEWIMG-1)*DSPOTT(23,II))+
     2            (REFRY(21,NEWIMG-1)*DSPOTT(24,II)))
                  IF(SOLCOR2.NE.0.0D0)
     1            DSPOTT(12,II)=DSPOTT(12,II)/SOLCOR2
                  IF(SOLCOR2.NE.0.0D0) THEN
                      ABVAL=ABVAL+DABS(SOLCOR1/SOLCOR2)
                      IBVAL=IBVAL+1.0D0
                  ELSE
                      ABVAL=ABVAL+1.0D0
                      IBVAL=IBVAL+1.0D0
                  END IF
C
C     RAY FAILED, PROCESS NEXT RAY
              END IF
          END DO
C     MAKE SURE THE AVERAGE VALUE OF THE FIRST CORRECTION TERM IS 1.0
          AAVAL=AAVAL/IAVAL
          ABVAL=ABVAL/IBVAL
          DO II=1,NNTOT-1
              DSPOTT(12,II)=DSPOTT(12,II)/(AAVAL*ABVAL)
          END DO
C
          RETURN
      END


C SUB PSFINT.FOR

      SUBROUTINE PSFINT
          USE GLOBALS
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE PSFINT.FOR.
C     CALLED BY CMDER FOR COMMANDS PSFINT AND PSFINTS
C
          CHARACTER EEOOFF*4,UNITS*2
C
          REAL*8 EXTENT1,SPACING1
C
          REAL*8 CRAYX,CRAYY,PSFYCENT,PSFXCENT,DUMMY2,IPS,
     2    PSF,PEAKER,SPSF,SSPSF
C
          INTEGER IPSF,I,J,Q,III,JJJ,MMM,UNITTT,II
C
          DIMENSION PSF(:,:),SPSF(:,:),IPSF(:,:),SSPSF(:,:)
C
          INTEGER DUMMY,IDUM,JDUM
C
          ALLOCATABLE :: PSF,SPSF,IPSF,SSPSF
C
          INTEGER ALLOERR
C
          LOGICAL EXIS52,OPEN52
          LOGICAL EXIS51,OPEN51
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datspd.inc'
          INCLUDE 'datsp1.inc'
C
          IF(STI.EQ.1) THEN
              IF(WC.EQ.'PSFINT')
     1        OUTLYNE='"PSFINT" INTERPOLATES THE CURRENT PSF TO AN'
              IF(WC.EQ.'PSFINTS')
     1        OUTLYNE='"PSFINTS" INTERPOLATES THE CURRENT STREAKED PSF TO AN'
              CALL SHOWIT(1)
              OUTLYNE='EVEN, (PGR+1)x(PGR+1) GRID'
              CALL SHOWIT(1)
              RETURN
          END IF
          IF(SST.EQ.1.OR.SN.EQ.1.OR.SQ.EQ.1) THEN
              IF(WC.EQ.'PSFINT')
     1        OUTLYNE='"PSFINT" TAKES NO ADDITIONAL INPUT'
              IF(WC.EQ.'PSFINTS')
     1        OUTLYNE='"PSFINTS" TAKES NO ADDITIONAL INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
          IF(WC.EQ.'PSFINT') THEN
C     DOES THE PSF FILE EXIST WITH AN EXISTING PSF
              EXIS51=.FALSE.
              INQUIRE(FILE=trim(HOME)//'PSF.DAT',EXIST=EXIS51)
              IF(EXIS51) THEN
C     DO ANALYSIS
                  OPEN51=.FALSE.
                  INQUIRE(FILE=trim(HOME)//'PSF.DAT',OPENED=OPEN51)
                  IF(.NOT.OPEN51) THEN
C     OPEN FILE
                      OPEN(UNIT=51,ACCESS='SEQUENTIAL',BLANK='NULL'
     1                  ,FORM='FORMATTED',FILE=trim(HOME)//'PSF.DAT'
     2                  ,STATUS='UNKNOWN')
                      REWIND(UNIT=51)
                      UNITTT=51
                  ELSE
                      REWIND(UNIT=51)
                      UNITTT=51
                  END IF
              ELSE
C     NO FILE, RETURN
                  OUTLYNE='FILE "PSF.DAT" DOES NOT EXITS TO BE ITERPOLATED'
                  CALL SHOWIT(1)
                  OUTLYNE='NO ACTION TAKEN'
                  CALL SHOWIT(1)
                  CALL MACFAL

              END IF
          END IF
          IF(WC.EQ.'PSFINTS') THEN
C     DOES THE SPSF FILE EXIST WITH AN EXISTING PSF
              EXIS52=.FALSE.
              INQUIRE(FILE=trim(HOME)//'SPSF.DAT',EXIST=EXIS52)
              IF(EXIS52) THEN
C     DO ANALYSIS
                  OPEN52=.FALSE.
                  INQUIRE(FILE=trim(HOME)//'SPSF.DAT',OPENED=OPEN52)
                  IF(.NOT.OPEN52) THEN
C     OPEN FILE
                      OPEN(UNIT=52,ACCESS='SEQUENTIAL',BLANK='NULL'
     1                  ,FORM='FORMATTED',FILE=trim(HOME)//'SPSF.DAT'
     2                  ,STATUS='UNKNOWN')
                      REWIND(UNIT=52)
                      UNITTT=52
                  ELSE
                      REWIND(UNIT=52)
                      UNITTT=52
                  END IF
              ELSE
C     NO FILE, RETURN
                  OUTLYNE='FILE "SPSF.DAT" DOES NOT EXITS TO BE ITERPOLATED'
                  CALL SHOWIT(1)
                  OUTLYNE='NO ACTION TAKEN'
                  CALL SHOWIT(1)
                  CALL MACFAL

              END IF
          END IF
C     NOW ALLOCATE AND LOAD ARRAYS
C     READ SOME STUFF
          READ(51,1001) PSFTAG
 1001     FORMAT(A12)
          READ(51,1002) PSFLI
 1002     FORMAT(A78)
          READ(51,1003) MMM,PGR
 1003     FORMAT(I10,1X,I10)
          READ(51,9004) UNITS
 9004     FORMAT(A2)
          READ(51,1008) EXTENT1,SPACING1
          READ(51,1008) PSFXCENT,PSFYCENT
          READ(51,1008) CRAYX,CRAYY
 1008     FORMAT(E15.8,1x,E15.8)
 9005     FORMAT(2I4,I8)
 9006     FORMAT(I4,G23.15)
          DEALLOCATE(PSF,SPSF,IPSF,SSPSF,STAT=ALLOERR)
          ALLOCATE(PSF(MMM,MMM),SPSF(MMM-1,MMM-1),
     2    SSPSF(PGR+1,PGR+1),IPSF(MMM,MMM),STAT=ALLOERR)
C     ZERO THE ADD ARRAY
          I=MMM-1
          J=MMM-1
          SPSF(1:J,1:I)=0.0D0
          III=MMM
          JJJ=MMM
          PSF(1:III,1:JJJ)=0.0D0
          IPSF(1:III,1:JJJ)=0
C
C     LOAD PSF ARRAY
          II=1
          DO I=1,MMM
              DO J=1,MMM
                  READ(51,9005) IDUM,JDUM,IPS
                  PSF(J,I)=DBLE(IPS)
                  II=II+1
              END DO
          END DO
C     READ IN THE XLSF
          DO I=1,PGR
              READ(51,9006) DUMMY,DUMMY2
          END DO
C     READ IN THE YLSF
          DO I=1,PGR
              READ(51,9006) DUMMY,DUMMY2
          END DO
C     NEXT LINE BETTER BE EOF
          READ(51,*) EEOOFF
C     LSF ARRAYS LOADED
          REWIND(UNIT=UNITTT)
          CALL CLOSE_FILE(UNITTT,1)
C     PSF ARRAY LOADED.

C     NOW INTERPOLATE PSF TO SPSF TO BUILD THE INTERPOLATED PSF
          DO I=1,MMM-1
              DO J=1,MMM-1
                  SPSF(I,J)=(PSF(I,J)+PSF(I+1,J)+PSF(I,J+1)+PSF(I+1,J+1))/4.0D0
              END DO
          END DO
          PEAKER=-1.0D300
          DO JJJ=1,MMM-1
              DO III=1,MMM-1
                  IF(SPSF(III,JJJ).GT.PEAKER) PEAKER=SPSF(III,JJJ)
              END DO
          END DO
          DO JJJ=1,MMM-1
              DO III=1,MMM-1
                  IF(PEAKER.NE.0.0D0) SPSF(III,JJJ)=DNINT((SPSF(III,JJJ)/PEAKER)
     1            *32767.0D0)
                  IF(PEAKER.EQ.0.0D0) SPSF(III,JJJ)=DNINT(SPSF(III,JJJ))
              END DO
          END DO
C     NOW LOAD UP TH SSPSF ARRAY
          DO III=1,PGR+1
              DO JJJ=1,PGR+1
                  SSPSF(III,JJJ)=0.0D0
              END DO
          END DO
C
          IF((PGR+1).GT.(MMM-1)) THEN
C     TARGET ARRAY LARGER THAN SOURCE ARRAY
              Q=((PGR+1)-(MMM-1))/2
              DO III=1,MMM-1
                  DO JJJ=1,MMM-1
                      SSPSF(III+Q,JJJ+Q)=SPSF(III,JJJ)
                  END DO
              END DO
          END IF
          IF((PGR+1).LE.(MMM-1)) THEN
C     TARGET ARRAY SMALLER OR EQUAL TO SOURCE ARRAY
              Q=((PGR+1)-(MMM-1))/2
              DO III=1,PGR+1
                  DO JJJ=1,PGR+1
                      SSPSF(III,JJJ)=SPSF(III+Q,JJJ+Q)
                  END DO
              END DO
          END IF
C     MAKE SURE INPUT FILES ARE CLOSED
          CALL CLOSE_FILE(UNITTT,1)
C
C     OPEN WRITE FILE
          EXIS52=.FALSE.
          OPEN52=.FALSE.
          INQUIRE(FILE=trim(HOME)//'PSFINT.DAT',EXIST=EXIS52)
          INQUIRE(FILE=trim(HOME)//'PSFINT.DAT',OPENED=OPEN52)
          IF(OPEN52.AND.EXIS52) CALL CLOSE_FILE(52,0)
          EXIS52=.FALSE.
          INQUIRE(FILE=trim(HOME)//'SPSF.DAT',EXIST=EXIS52)
          IF(EXIS52) THEN
              OPEN(UNIT=52,ACCESS='SEQUENTIAL',BLANK='NULL'
     1          ,FORM='FORMATTED',FILE=trim(HOME)//'PSFINT.DAT'
     2          ,STATUS='UNKNOWN')
              CALL CLOSE_FILE(52,0)
          END IF
C     OPEN NEW FILE
          OPEN(UNIT=52,ACCESS='SEQUENTIAL',BLANK='NULL'
     1    ,FORM='FORMATTED',FILE=trim(HOME)//'PSFINT.DAT'
     2    ,STATUS='UNKNOWN')
C     WRITE FILE HEADER
          WRITE(52,9001)
 9001     FORMAT('PSFINT.DAT')
          WRITE(52,9002) PGR+1
 9002     FORMAT('PSF INTERPOLATED TO AN NxN GRID WITH N = ',I4)
          IF(SYSTEM1(6).EQ.1.0D0) WRITE(52,1004)
          IF(SYSTEM1(6).EQ.2.0D0) WRITE(52,1005)
          IF(SYSTEM1(6).EQ.3.0D0) WRITE(52,1006)
          IF(SYSTEM1(6).EQ.4.0D0) WRITE(52,1007)
 1004     FORMAT('IN')
 1005     FORMAT('CM')
 1006     FORMAT('MM')
 1007     FORMAT('M')
          WRITE(52,1008) (SPACING1*DBLE(PGR)),SPACING1
          DO JJJ=1,PGR+1
              DO III=1,PGR+1
                  WRITE(52,2000) III,JJJ,
     1            INT(DNINT(SSPSF(III,JJJ)))
              END DO
          END DO
 2000     FORMAT(2I4,I8)
          WRITE(52,1009)
 1009     FORMAT('EOF')
C
          DEALLOCATE(PSF,SSPSF,SPSF,IPSF,STAT=ALLOERR)
          CALL CLOSE_FILE(51,1)
          CALL CLOSE_FILE(52,1)
          RETURN
      END
