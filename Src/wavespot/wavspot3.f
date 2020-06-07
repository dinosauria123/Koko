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

C       THIRD FILE OF CAPFN/SPOT ROUTINES

C SUB SPOT.FOR

      SUBROUTINE SPOT
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE SPOT.FOR. THIS DOES ALL SPOT DIAGRAMS.
C
          LOGICAL SYES,OPMAP,ERRORR,FIXWL
C
          INTEGER TYPEOLD,OLDRNUMBR
C
          REAL*8 ELMAX
          COMMON/ELEV/ELMAX
          REAL*8 ZZEMAX,ZZEMIN
          COMMON/KENMOOR/ZZEMIN,ZZEMAX
C
          COMMON/OPOPMP/OPMAP
C
          COMMON/OLDTYP/TYPEOLD,OLDRNUMBR
C
          LOGICAL NOCOBSPSF
          COMMON/PSFCOBS/NOCOBSPSF
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datspd.inc'
          INCLUDE 'datsp1.inc'

          INTEGER RAYMAX,IWV
C
          LOGICAL REFERR
C
C
C     THE ENTRIES IN THE SPOT FILES ARE:
C
C     DSPOT#(J)
C               # IS 1 TO 5 FOR THE 5 WAVELENGTHS
C               J GOES FROM 1 TO 17
C               J=1  IS X RAY POS RAYRAY(1,NEWIMG)
C               J=2  IS Y RAY POS RAYRAY(2,NEWIMG)
C               J=3  IS Z RAY POS RAYRAY(3,NEWIMG)
C               J=4  OPDW
C               J=5  X COORD OF RAY AT NEWREF
C               J=6  Y COORD OF RAY AT NEWREF
C               J=7  IS RAY CODE 1
C               J=8  IS RAY CODE 2
C               J=9  XZ SLOPE ANGLE(RADIANS) OF RAY
C               J=10 YZ SLOPE ANGLE(RADIANS) OF RAY
C               J=11 IS THE RAY APERTURE APODIZATION TERM
C               J=12 RAY ENERGY TERM (11*SPT)
C               J=13 OPTICAL PATH LENGTH FROM NEWOBJ OR NEWOBJ+1 TO NEWIMG
C               J=14 X RAY COORD AT NEWOBJ+1
C               J=15 Y RAY COORD AT NEWOBJ+1
C               J=16 WAVELENGTH NUMBER
C               J=17 SPECTRAL WEIGHT
C               J=18 Z RAY COORD AT NEWOBJ+1
C               J=19 RAYRAY(19,NEWOBJ+1)
C               J=20 RAYRAY(20,NEWOBJ+1)
C               J=21 RAYRAY(21,NEWOBJ+1)
C               J=22 RAYRAY(19,NEWIMG)
C               J=23 RAYRAY(20,NEWIMG)
C               J=24 RAYRAY(21,NEWIMG)
C               J=25 RAYRAY(9,NEWIMG) RAY ANGLE OF INCIDENCE AT FINAL SURFACE
C               J=26 RAYRAY(10,NEWIMG) RAY ANGLE OF REFRACTION
C               J=27 RAYRAY(1,NEWIMG-1)
C               J=28 RAYRAY(2,NEWIMG-1)
C               J=29 RAYRAY(3,NEWIMG-1)
C               J=30 RAYRAY(19,NEWIMG-1)
C               J=31 RAYRAY(20,NEWIMG-1)
C               J=32 RAYRAY(21,NEWIMG-1)
C               J=33 USED FOR POLYCHROMATIC RMSOPD
C               J=34 RAY RAY ENERGY IGNORING ADJUSTMENT DUE TO PUPIL DISTORTION
C               BUT INCLUES APODIZATION AND COATING LOSSES
C               J=35 SPECTRAL WEIGHT FOR THAT WAVELENGTH
C               J=36 IS ZERO IF AIMED RAY IS INSIDE REF SURF CLAP
C               AND 1 IF IT IS OUTSIDE REF SURF CLAP
C               J=37 IS RAY INTENSITY WHICH IS J=12 TIMES COSI (RAYRAY(9,NEWIMG))
C               AND 1 IF IT IS OUTSIDE REF SURF CLAP
C               J=38 IS THE CHIEF RAY ANGLE OF INCIDENCE AT NEWIMG
C               J=39 IS THE RAY ANGLE OF INCIDENCE AT NEWIMG
C               DSPOT(40)=IX (REFERENCE SURFACE POINTER IN X
C               DSPOT(41)=NDEL1 REFERENCE SURFACE DELTA IN X
C               DSPOT(42)=IX (REFERENCE SURFACE POINTER IN Y
C               DSPOT(43)=NDEL1 REFERENCE SURFACE DELTA IN Y
C               J=44 IS THE ANGLE OF INCIDENCE AT THE REFERENCE SURFACE FOR THE CHIEF RAY
C               J=45 X AT SPDSURF
C               J=46 Y AT SPDSURF
C               J=47 Z AT SPDSURF
C               J=48 L AT SPDSURF
C               J=49 M AT SPDSURF
C               J=50 N AT SPDSURF
C               J=51 COSI AT SPDSURF
C
          REFERR=.FALSE.
C       WAS A FOB ISSUED
          IF(WC.EQ.'ISPD'.OR.WC.EQ.'ISPDA') THEN
              IF(.NOT.FOBYES) THEN
                  OUTLYNE='NO "FOB" EXISTS TO USE IN THE ILLUMINATION SPOT ANALYSIS'
                  CALL SHOWIT(1)
                  IF(WC.EQ.'ISPD')
     1            OUTLYNE='ISSUE A "FOB" BEFORE ISSUING "ISPD"'
                  IF(WC.EQ.'ISPDA')
     1            OUTLYNE='ISSUE A "FOB" BEFORE ISSUING "ISPDA"'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(STI.EQ.1) THEN
                  IF(WC.EQ.'ISPD')THEN
                      OUTLYNE='"ISPD" PERFORMS AN ILLUMINATION SPOT DIAGRAM RAY TRACE'
                      CALL SHOWIT(1)
                      OUTLYNE='RAYS WILL BE FIRED AT SPATIAL COORDINATES ON SURFACE #1'
                      CALL SHOWIT(1)
                  END IF
                  IF(WC.EQ.'ISPDA')THEN
                      OUTLYNE='"ISPDA" PERFORMS AN ILLUMINATION SPOT DIAGRAM RAY TRACE'
                      CALL SHOWIT(1)
                      OUTLYNE='RAY WILL BE FIRED IN A RANDOM ANGULAR PATTERN'
                      CALL SHOWIT(1)
                  END IF
              END IF
              IF(SST.EQ.1.OR.SQ.EQ.1) THEN
                  IF(WC.EQ.'ISPD')
     1            OUTLYNE='"ISPD" TAKES NO STRING OR QUALIFIER INPUT'
                  IF(WC.EQ.'ISPDA')
     1            OUTLYNE='"ISPDA" TAKES NO STRING OR QUALIFIER INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
                  IF(WC.EQ.'ISPD')
     1            OUTLYNE='"ISPD" TAKES NO NUMERIC WORD #3, #4 OR #5 INPUT'
                  IF(WC.EQ.'ISPDA')
     1            OUTLYNE='"ISPDA" TAKES NO NUMERIC WORD #3, #4 OR #5 INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(WC.EQ.'ISPD'.AND.S2.EQ.1) THEN
                  IF(WC.EQ.'ISPD')
     1            OUTLYNE='"ISPD" TAKES NO NUMERIC WORD #2 INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(W1.LE.1.0D0.AND.DF1.EQ.0) THEN
                  IF(WC.EQ.'ISPD')
     1            OUTLYNE='"ISPD" REQUIRES POSITIVE NUMERIC WORD #1 INPUT'
                  IF(WC.EQ.'ISPDA')
     1            OUTLYNE='"ISPDA" REQUIRES POSITIVE NUMERIC WORD #1 INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='TO BE GREATER THAN OR EQUAL TO 1'
                  CALL SHOWIT(1)
                  OUTLYNE='W1=NUMBER OF RAYS TO TRACE'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(W2.LE.0.0D0.AND.WC.EQ.'ISPDA') THEN
                  OUTLYNE='"ISPDA" REQUIRES POSITIVE NUMERIC WORD #2 INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='W2=MAXIMUM CONE HALF-ANGLE'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(W2.GT.180.0D0.AND.WC.EQ.'ISPDA') THEN
                  OUTLYNE='MAX ELEVATION ANGLE INPUT FOR "ISPDA" IS 180 DEGREES'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(DF1.EQ.1) W1=1000.0D0
              IF(DF2.EQ.1.AND.WC.EQ.'ISPDA') W2=90.0D0

              RAYMAX=INT(W1)
              ELMAX=W2
C     SET SPOT TYPE TO RANDOM
              OLDRNUMBR=RNUMBR
              RNUMBR=RAYMAX
              TYPEOLD=SPDTYPE
              SPDTYPE=3
              SPOTSSI=.FALSE.
              DETER=.FALSE.
              DETCENT=.FALSE.
              DETCHIEF=.TRUE.
              SPDCENT=.FALSE.
              SPDCHIEF=.TRUE.
              DETTYP=0
              DETDIAM=0.0D0
              DETTX=0.0D0
              DETTY=0.0D0
              DETDELX=0.0D0
              DETDELY=0.0D0
              DETTHETA=0.0D0
              SPDSSIVAL=0.0D0
C
              ZZEMIN=1.0D+300
              ZZEMAX=-1.0D+300
              IF(DF1.EQ.1.OR.WC(1:1).EQ.'I') THEN
                  OLDIF=LDIF
                  LDIF=.FALSE.
                  CALL SPOT1(1)
                  LDIF=OLDIF
              ELSE
                  OLDSP(1) =SYSTEM1(31)
                  OLDSP(2) =SYSTEM1(32)
                  OLDSP(3) =SYSTEM1(33)
                  OLDSP(4) =SYSTEM1(34)
                  OLDSP(5) =SYSTEM1(35)
                  OLDSP(6) =SYSTEM1(76)
                  OLDSP(7) =SYSTEM1(77)
                  OLDSP(8) =SYSTEM1(78)
                  OLDSP(9) =SYSTEM1(79)
                  OLDSP(10)=SYSTEM1(80)
                  SYSTEM1(31:35)=0.0D0
                  SYSTEM1(76:80)=0.0D0
                  IF(INT(W1).EQ.1)  SYSTEM1(31)=1.0D0
                  IF(INT(W1).EQ.2)  SYSTEM1(32)=1.0D0
                  IF(INT(W1).EQ.3)  SYSTEM1(33)=1.0D0
                  IF(INT(W1).EQ.4)  SYSTEM1(34)=1.0D0
                  IF(INT(W1).EQ.5)  SYSTEM1(35)=1.0D0
                  IF(INT(W1).EQ.6)  SYSTEM1(76)=1.0D0
                  IF(INT(W1).EQ.7)  SYSTEM1(77)=1.0D0
                  IF(INT(W1).EQ.8)  SYSTEM1(78)=1.0D0
                  IF(INT(W1).EQ.9)  SYSTEM1(79)=1.0D0
                  IF(INT(W1).EQ.10) SYSTEM1(80)=1.0D0
                  OLDIF=LDIF
                  LDIF=.FALSE.
                  CALL SPOT1(1)
                  LDIF=OLDIF
                  SYSTEM1(31)=OLDSP(1)
                  SYSTEM1(32)=OLDSP(2)
                  SYSTEM1(33)=OLDSP(3)
                  SYSTEM1(34)=OLDSP(4)
                  SYSTEM1(35)=OLDSP(5)
                  SYSTEM1(76)=OLDSP(6)
                  SYSTEM1(77)=OLDSP(7)
                  SYSTEM1(78)=OLDSP(8)
                  SYSTEM1(79)=OLDSP(9)
                  SYSTEM1(80)=OLDSP(10)
              END IF
              SPDTYPE=TYPEOLD
              RNUMBR=OLDRNUMBR
              RETURN
          END IF
          IF(WC.EQ.'SPD') THEN
              IF(.NOT.FOBYES.AND.SQ.EQ.0.OR..NOT.FOBYES.AND.WQ.EQ.'ACC'
     1        .OR..NOT.FOBYES.AND.
     2        WQ.EQ.'ISTAT'.OR.
     3        .NOT.FOBYES.AND.WQ.EQ.'IPSTAT') THEN
                  OUTLYNE='NO "FOB" EXISTS TO USE IN THE SPOT ANALYSIS'
                  CALL SHOWIT(1)
                  OUTLYNE='ISSUE A "FOB" BEFORE ISSUING "SPD" COMMANDS'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
          IF(WC.EQ.'PSF') THEN
              IF(.NOT.FOBYES.AND.SQ.EQ.0
     1        .OR..NOT.FOBYES.AND.WQ.EQ.'PERFECT'
     1        .OR..NOT.FOBYES.AND.WQ.EQ.'PERFNOOB') THEN
                  OUTLYNE='NO "FOB" EXISTS TO BE USE'
                  CALL SHOWIT(1)
                  OUTLYNE='ISSUE A "FOB" BEFORE ISSUING THE "PSF" COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
          IF(WC.EQ.'PUPIL') THEN
              IF(.NOT.FOBYES.AND.SQ.EQ.0
     1        .OR..NOT.FOBYES.AND.WQ.EQ.'PERFECT'
     1        .OR..NOT.FOBYES.AND.WQ.EQ.'PERFNOOB') THEN
                  OUTLYNE='NO "FOB" EXISTS TO BE USE'
                  CALL SHOWIT(1)
                  OUTLYNE='ISSUE A "FOB" BEFORE ISSUING THE "PUPIL" COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
          IF(WC.EQ.'CAPFN') THEN
              IF(.NOT.FOBYES.AND.SQ.EQ.0
     1        .OR..NOT.FOBYES.AND.WQ.EQ.'PERFECT'
     1        .OR..NOT.FOBYES.AND.WQ.EQ.'SILENT'
     1        .OR..NOT.FOBYES.AND.WQ.EQ.'NOSCALE') THEN
                  OUTLYNE='NO "FOB" EXISTS TO BE USE'
                  CALL SHOWIT(1)
                  OUTLYNE='ISSUE A "FOB" BEFORE ISSUING THE "CAPFN" COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
C       WAS FOB NULL USED
C
C                       "NO QUALIFIER"
C                        ACC
C                        MOVE
C                        MOVEACC
C       ***************************************************************
C       FOR FIELD AVERAGING SPOTS COMMAND WORDS ARE:
C                       SPDSAVE (SAVES TO SPOTS.DAT IN BINARY
C       AND ERASES LAST COPY OF DESIGNATED FILE OR SPOTS.DAT)
C                       SPDADD (ADDS TO SPOTS.DAT IN BINARY
C     OR OTHER DESIGNATED FILE
C                       SPDSTATS (CALCULATES ALL STATISTICS FOR
C       DATA IN SPOTS.DAT OR OTHER DESIGNATED FILE)
C       ****************************************************************
C
C       CHECK FOR CORRECT QUALIFIER WORDS
          IF(WC.EQ.'SPD') THEN
              SYES=.FALSE.
              IF(WQ.EQ.' ')       SYES=.TRUE.
              IF(WQ.EQ.'ACC')     SYES=.TRUE.
              IF(WQ.EQ.'MOVE')    SYES=.TRUE.
              IF(WQ.EQ.'MOVEACC') SYES=.TRUE.
              IF(WQ.EQ.'ISTAT')   SYES=.TRUE.
              IF(WQ.EQ.'IPSTAT')  SYES=.TRUE.
          END IF
          IF(WC.EQ.'CAPFN') THEN
              SYES=.FALSE.
              IF(WQ.EQ.' ') SYES=.TRUE.
              IF(WQ.EQ.'PERFECT') SYES=.TRUE.
              IF(WQ.EQ.'NOSCALE') SYES=.TRUE.
              IF(WQ.EQ.'SILENT') SYES=.TRUE.
          END IF
          IF(WC.EQ.'PSF'.OR.WC.EQ.'PUPIL') THEN
              SYES=.FALSE.
              IF(WQ.EQ.' ') SYES=.TRUE.
              IF(WQ.EQ.'PERFECT') SYES=.TRUE.
              IF(WQ.EQ.'PERFNOOB') SYES=.TRUE.
              IF(WQ.EQ.'PERFNOOB') NOOB=.TRUE.
              IF(WQ.NE.'PERFNOOB') NOOB=.FALSE.
          END IF
          IF(WC.EQ.'SPD'.AND..NOT.SYES) THEN
              OUTLYNE=
     1        'INVALID QUALIFIER WORD USED WITH "SPD" COMMAND'
              CALL SHOWIT(1)
              OUTLYNE=
     1        'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(WC.EQ.'CAPFN'.AND..NOT.SYES) THEN
              OUTLYNE=
     1        'INVALID QUALIFIER WORD USED WITH "CAPFN" COMMAND'
              CALL SHOWIT(1)
              OUTLYNE=
     1        'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(WC.EQ.'CAPFN'.AND.STI.EQ.1) THEN
              OUTLYNE=
     1        '"CAPFN" CREATES A COMPLEX APERTURE FUNCTION'
              CALL SHOWIT(1)
              RETURN
          END IF
          IF(WC.EQ.'PSF'.AND..NOT.SYES) THEN
              OUTLYNE=
     1        'INVALID QUALIFIER WORD USED WITH "PSF" COMMAND'
              CALL SHOWIT(1)
              OUTLYNE=
     1        'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(WC.EQ.'PUPIL'.AND..NOT.SYES) THEN
              OUTLYNE=
     1        'INVALID QUALIFIER WORD USED WITH "PUPIL" COMMAND'
              CALL SHOWIT(1)
              OUTLYNE=
     1        'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(WC.EQ.'PSF'.AND.STI.EQ.1) THEN
              OUTLYNE=
     1        '"PSF" CREATES A DIFFRACTION POINT SPREAD FUNCTION'
              CALL SHOWIT(1)
              RETURN
          END IF
          IF(WC.EQ.'PSF'.AND.STI.EQ.1) THEN
              OUTLYNE=
     1        '"PSF" CREATES A PUPIL FUNCTION IN FOE MEMORY'
              CALL SHOWIT(1)
              RETURN
          END IF
          IF(WC.EQ.'SPD'.AND.STI.EQ.1) THEN
              OUTLYNE=
     1        '"SPD" CREATES A SPOT DIAGRAM'
              CALL SHOWIT(1)
              RETURN
          END IF
          IF(WC.EQ.'CAPFN'.AND.SST.EQ.1) THEN
              OUTLYNE=
     1        '"CAPFN" TAKES NO STRING INPUT'
              CALL SHOWIT(1)
              OUTLYNE=
     1        'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(WC.EQ.'PSF'.AND.SST.EQ.1) THEN
              OUTLYNE=
     1        '"PSF" TAKES NO STRING INPUT'
              CALL SHOWIT(1)
              OUTLYNE=
     1        'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(WC.EQ.'PUPIL'.AND.SST.EQ.1) THEN
              OUTLYNE=
     1        '"PUPIL" TAKES NO STRING INPUT'
              CALL SHOWIT(1)
              OUTLYNE=
     1        'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(WC.EQ.'PSF') THEN
              IF(S4.EQ.1.OR.S5.EQ.1) THEN
                  OUTLYNE=
     1            '"PSF" TAKES NO NUMERIC WORD #4 TO #5 INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE=
     1            'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
          IF(WC.EQ.'PUPIL') THEN
              IF(S4.EQ.1.OR.S5.EQ.1) THEN
                  OUTLYNE=
     1            '"PUPIL" TAKES NO NUMERIC WORD #4 TO #5 INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE=
     1            'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
          IF(WC.EQ.'SPD'.AND.SST.EQ.1) THEN
              OUTLYNE=
     1        '"SPD" TAKES NO STRING INPUT'
              CALL SHOWIT(1)
              OUTLYNE=
     1        'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
C       CALL THE CORRECT ROUTINE
          IF(WC.EQ.'CAPFN'.AND.WQ.NE.'PERFECT') THEN
C     OPMAP FALSE MEANS NO OPD MAP FITTING COEFFICIENTS EXIST
              OPMAP=.FALSE.
              NRDFACTOR=1.0D0
              OLDIF=LDIF
              LDIF=.FALSE.
              CALL COMPAP(REFERR,1)
              LDIF=OLDIF
              RETURN
          ELSE
C                       NOT CAPFN
          END IF
          IF(WC.EQ.'CAPFN'.AND.WQ.EQ.'PERFECT') THEN
C     OPMAP FALSE MEANS NO OPD MAP FITTING COEFFICIENTS EXIST
C                      OPMAP=.FALSE.
              NRDFACTOR=1.0D0
              PERFECT=.TRUE.
              CALL COMPAP(REFERR,1)
              PERFECT=.FALSE.
              RETURN
          ELSE
C                       NOT CAPFN PERFECT
          END IF
          IF(WC.EQ.'PSF'.AND.WQ.NE.'PERFECT'.AND.
     1    WC.EQ.'PSF'.AND.WQ.NE.'PERFNOOB') THEN
C     OPMAP FALSE MEANS NO OPD MAP FITTING COEFFICIENTS EXIST
              NOOB=.FALSE.
              FIXWL=.FALSE.
              IF(DF2.EQ.0) THEN
                  FIXWL=.TRUE.
                  CALL FIXWV(W2)
              END IF
              CPFNEXT=.FALSE.
              IF(.NOT.CPFNEXT) THEN
                  OPMAP=.FALSE.
                  SAVE_KDP(1)=SAVEINPT(1)
                  W1=DABS(DBLE(NRD))
                  DF1=0
                  S1=1
                  SN=1
                  REST_KDP(1)=RESTINPT(1)
                  ERRORR=.FALSE.
                  NRDFACTOR=1.0D0
                  IF(GRIFLG.EQ.1) CALL NRDCALC(ERRORR)
                  IF(ERRORR) THEN
                      WRITE(OUTLYNE,*) 'UNREALISTIC NRD VALUES'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*) 'PSF CANNOT PROCEED'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  OLDIF=LDIF
                  LDIF=.FALSE.
                  CALL COMPAP(REFERR,1)
                  LDIF=OLDIF
                  IF(REFERR) RETURN
              ELSE
              END IF
              IF(DF2.EQ.0) IWV=INT(W2)
              IF(IWV.LT.1.OR.IWV.GT.11) IWV=11
              IF(DF3.EQ.1) W3=1.0D0
              CALL DOPSF
              IF(FIXWL) CALL UNFIXWV
              IF(FIXWL) FIXWL=.FALSE.
              CALL CLOSE_FILE(51,1)
              RETURN
          ELSE
C                       NOT PSF
          END IF
          IF(WC.EQ.'PSF'.AND.WQ.EQ.'PERFECT'.OR.
     1    WC.EQ.'PSF'.AND.WQ.EQ.'PERFNOOB') THEN
C     OPMAP FALSE MEANS NO OPD MAP FITTING COEFFICIENTS EXIST
C                      OPMAP=.FALSE.
              FIXWL=.FALSE.
              IF(DF2.EQ.0) THEN
                  FIXWL=.TRUE.
                  CALL FIXWV(W2)
              END IF
              NOOB=.TRUE.
              SAVE_KDP(1)=SAVEINPT(1)
              W1=DABS(DBLE(NRD))
              DF1=0
              S1=1
              SN=1
              REST_KDP(1)=RESTINPT(1)
              ERRORR=.FALSE.
              NRDFACTOR=1.0D0
              IF(GRIFLG.EQ.1) CALL NRDCALC(ERRORR)
              IF(ERRORR) THEN
                  WRITE(OUTLYNE,*) 'UNREALISTIC NRD VALUES'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*) 'PSF CANNOT PROCEED'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              NOCOBSPSF=.FALSE.
              IF(WQ.EQ.'PERFNOOB') NOCOBSPSF=.TRUE.
              IF(WQ.EQ.'PERFNOOB') NOOB=.TRUE.
              IF(WQ.NE.'PERFNOOB') NOOB=.FALSE.
              PERFECT=.TRUE.
              CALL COMPAP(REFERR,1)
              PERFECT=.FALSE.
              IF(DF2.EQ.0) IWV=INT(W2)
              IF(IWV.LT.1.OR.IWV.GT.11) IWV=11
              IF(DF3.EQ.1) W3=1.0D0
              CALL DOPSF
              NOCOBSPSF=.FALSE.
              IF(FIXWL) CALL UNFIXWV
              IF(FIXWL) FIXWL=.FALSE.
              CALL CLOSE_FILE(51,1)
              RETURN
          ELSE
C                       NOT PSF PERFECT
          END IF
          IF(WC.EQ.'PUPIL'.AND.WQ.NE.'PERFECT'.AND.
     1    WC.EQ.'PUPIL'.AND.WQ.NE.'PERFNOOB') THEN
C     OPMAP FALSE MEANS NO OPD MAP FITTING COEFFICIENTS EXIST
              NOOB=.FALSE.
              FIXWL=.FALSE.
              IF(DF2.EQ.0) THEN
                  FIXWL=.TRUE.
                  CALL FIXWV(W2)
              END IF
              CPFNEXT=.FALSE.
              IF(.NOT.CPFNEXT) THEN
                  OPMAP=.FALSE.
                  SAVE_KDP(1)=SAVEINPT(1)
                  W1=DABS(DBLE(NRD))
                  DF1=0
                  S1=1
                  SN=1
                  REST_KDP(1)=RESTINPT(1)
                  ERRORR=.FALSE.
                  NRDFACTOR=1.0D0
                  IF(GRIFLG.EQ.1) CALL NRDCALC(ERRORR)
                  IF(ERRORR) THEN
                      WRITE(OUTLYNE,*) 'UNREALISTIC NRD VALUES'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*) '"PUPIL" CANNOT PROCEED'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  OLDIF=LDIF
                  LDIF=.FALSE.
                  CALL COMPAP(REFERR,1)
                  LDIF=OLDIF
                  IF(REFERR) RETURN
              ELSE
              END IF
              IF(DF2.EQ.0) IWV=INT(W2)
              IF(IWV.LT.1.OR.IWV.GT.11) IWV=11
              IF(DF3.EQ.1) W3=1.0D0
              CALL DOPUPIL(2)
              WRITE(OUTLYNE,*) 'PUPIL FUNCTION CREATED'
              CALL SHOWIT(0)
              IF(FIXWL) CALL UNFIXWV
              IF(FIXWL) FIXWL=.FALSE.
              CALL CLOSE_FILE(51,1)
              RETURN
          ELSE
C                       NOT PUPIL
          END IF
          IF(WC.EQ.'PUPIL'.AND.WQ.EQ.'PERFECT'.OR.
     1    WC.EQ.'PUPIL'.AND.WQ.EQ.'PERFNOOB') THEN
C     OPMAP FALSE MEANS NO OPD MAP FITTING COEFFICIENTS EXIST
C                      OPMAP=.FALSE.
              FIXWL=.FALSE.
              IF(DF2.EQ.0) THEN
                  FIXWL=.TRUE.
                  CALL FIXWV(W2)
              END IF
              NOOB=.TRUE.
              SAVE_KDP(1)=SAVEINPT(1)
              W1=DABS(DBLE(NRD))
              DF1=0
              S1=1
              SN=1
              REST_KDP(1)=RESTINPT(1)
              ERRORR=.FALSE.
              NRDFACTOR=1.0D0
              IF(GRIFLG.EQ.1) CALL NRDCALC(ERRORR)
              IF(ERRORR) THEN
                  WRITE(OUTLYNE,*) 'UNREALISTIC NRD VALUES'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*) '"PUPIL" CANNOT PROCEED'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              NOCOBSPSF=.FALSE.
              IF(WQ.EQ.'PERFNOOB') NOCOBSPSF=.TRUE.
              IF(WQ.EQ.'PERFNOOB') NOOB=.TRUE.
              IF(WQ.NE.'PERFNOOB') NOOB=.FALSE.
              PERFECT=.TRUE.
              CALL COMPAP(REFERR,1)
              PERFECT=.FALSE.
              IF(DF2.EQ.0) IWV=INT(W2)
              IF(IWV.LT.1.OR.IWV.GT.11) IWV=11
              IF(DF3.EQ.1) W3=1.0D0
              CALL DOPUPIL
              NOCOBSPSF=.FALSE.
              IF(FIXWL) CALL UNFIXWV
              IF(FIXWL) FIXWL=.FALSE.
              CALL CLOSE_FILE(51,1)
              RETURN
          ELSE
C                       NOT PUPIL PERFECT
          END IF
C       CALL THE CORRECT ROUTINE
          IF(WC.EQ.'SPD') THEN
              IF(WQ.EQ.' '.OR.WQ.EQ.'ACC'
     1        .OR.WQ.EQ.'ISTAT'.OR.WQ.EQ.'IPSTAT') THEN
                  SPOTSSI=.FALSE.
                  DETER=.FALSE.
                  DETCENT=.FALSE.
                  DETCHIEF=.TRUE.
                  SPDCENT=.FALSE.
                  SPDCHIEF=.TRUE.
                  DETTYP=0
                  DETDIAM=0.0D0
                  DETTX=0.0D0
                  DETTY=0.0D0
                  DETDELX=0.0D0
                  DETDELY=0.0D0
                  DETTHETA=0.0D0
                  SPDSSIVAL=0.0D0
                  ZZEMIN=1.0D+300
                  ZZEMAX=-1.0D+300
                  IF(DF1.EQ.1) THEN
                      OLDIF=LDIF
                      LDIF=.FALSE.
                      CALL SPOT1(1)
                      LDIF=OLDIF
                  ELSE
                      OLDSP(1) =SYSTEM1(31)
                      OLDSP(2) =SYSTEM1(32)
                      OLDSP(3) =SYSTEM1(33)
                      OLDSP(4) =SYSTEM1(34)
                      OLDSP(5) =SYSTEM1(35)
                      OLDSP(6) =SYSTEM1(76)
                      OLDSP(7) =SYSTEM1(77)
                      OLDSP(8) =SYSTEM1(78)
                      OLDSP(9) =SYSTEM1(79)
                      OLDSP(10)=SYSTEM1(80)
                      SYSTEM1(31:35)=0.0D0
                      SYSTEM1(76:80)=0.0D0
                      IF(INT(W1).EQ.1)  SYSTEM1(31)=1.0D0
                      IF(INT(W1).EQ.2)  SYSTEM1(32)=1.0D0
                      IF(INT(W1).EQ.3)  SYSTEM1(33)=1.0D0
                      IF(INT(W1).EQ.4)  SYSTEM1(34)=1.0D0
                      IF(INT(W1).EQ.5)  SYSTEM1(35)=1.0D0
                      IF(INT(W1).EQ.6)  SYSTEM1(76)=1.0D0
                      IF(INT(W1).EQ.7)  SYSTEM1(77)=1.0D0
                      IF(INT(W1).EQ.8)  SYSTEM1(78)=1.0D0
                      IF(INT(W1).EQ.9)  SYSTEM1(79)=1.0D0
                      IF(INT(W1).EQ.10) SYSTEM1(80)=1.0D0
                      OLDIF=LDIF
                      LDIF=.FALSE.
                      CALL SPOT1(1)
                      LDIF=OLDIF
                      SYSTEM1(31)=OLDSP(1)
                      SYSTEM1(32)=OLDSP(2)
                      SYSTEM1(33)=OLDSP(3)
                      SYSTEM1(34)=OLDSP(4)
                      SYSTEM1(35)=OLDSP(5)
                      SYSTEM1(76)=OLDSP(6)
                      SYSTEM1(77)=OLDSP(7)
                      SYSTEM1(78)=OLDSP(8)
                      SYSTEM1(79)=OLDSP(9)
                      SYSTEM1(80)=OLDSP(10)
                  END IF
              ELSE
                  CALL SPDQAL
              END IF
              RETURN
          ELSE
C     NOT SPD
          END IF
          IF(WC.EQ.'FAIL'.OR.WC.EQ.'FAILACC') THEN
C     OPMAP FALSE MEANS NO OPD MAP FITTING COEFFICIENTS EXIST
              CALL SPDQAL
          END IF
          IF(WC.EQ.'SPDADD'.OR.WC.EQ.'SPDSAVE') THEN
              CALL SPDQAL
          END IF
          IF(WC.EQ.'SPDSTATS') THEN
              CALL SPDQAL
          END IF
C
          RETURN
      END
C SUB ISTAT.FOR
      SUBROUTINE ISTAT(J,STARANG,ENDANG,DELANG,NSTEP)
C
          IMPLICIT NONE
C
          INTEGER K,I,TOTI,J,TOTII,NSTEP,BUCKET(1:100)
C
          REAL*8 STAR,ENDD,ANGVAL,STARANG,ENDANG,DELANG
     1    ,YOBP,XOBP
C
          LOGICAL EXTGMTF1,EXTGMTF2
C
          COMMON/GMTFEXT/EXTGMTF1,EXTGMTF2
C
          CHARACTER STUNI*9
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datspd.inc'
          INCLUDE 'datsp1.inc'
C
C
          EXTGMTF1=.FALSE.
          EXTGMTF2=.FALSE.
C     CLEAR THE BUCKET
          BUCKET(1:100)=0
C     CALCULATE OBJECT POINT
C
C     FIELD OF VIEW DATA
C
          IF(DABS(ALENS(3,NEWOBJ)).GE.1.0D10) THEN
C     ANGULAR
              XOBP=SYSTEM1(23)*LFOB(2)
              YOBP=SYSTEM1(21)*LFOB(1)
              STUNI='DEGREE(S)'
          ELSE
              XOBP=SYSTEM1(16)*LFOB(2)
              YOBP=SYSTEM1(14)*LFOB(1)
              IF(SYSTEM1(6).EQ.1.0D0) STUNI='INCH     '
              IF(SYSTEM1(6).EQ.2.0D0) STUNI='CM       '
              IF(SYSTEM1(6).EQ.3.0D0) STUNI='MM       '
              IF(SYSTEM1(6).EQ.4.0D0) STUNI='M        '
          END IF
C
 100      FORMAT('CALCULATING ANGLE OF INCIDENCE STATISTICS')
 200      FORMAT(
     1    'CALCULATING ANGLE OF REFRACTION, REFLECTION AND DIFFRACTION',
     2    ' STATISTICS')
          IF(J.EQ.1) WRITE(OUTLYNE,100)
          CALL SHOWIT(0)
          IF(J.EQ.2) WRITE(OUTLYNE,200)
          CALL SHOWIT(0)
          TOTI=0
          TOTII=0
          DO I=2,ITOT
C     LOAD DSPOT(*) WITH DSPOTT(*,ID)
              ID=I-1
              CALL SPOTIT(4)
              IF(DSPOT(12).NE.0.0D0.AND.DSPOT(7).EQ.0.0D0) THEN
C     NON-FAILED, NON-ZERO ENERGY RAY
                  TOTI=TOTI+1
                  IF(J.EQ.1) ANGVAL=DABS(DACOS(DABS(DSPOT(25))))
                  IF(J.EQ.2) ANGVAL=DABS(DACOS(DABS(DSPOT(26))))
C     CONVERT TO DEGREES
                  ANGVAL=ANGVAL*180.0D0/PII
C     WHICH BUCKET TO STORE TO
                  DO K=1,NSTEP
                      STAR=STARANG+(DBLE(K-1)*DELANG)
                      ENDD=STARANG+(DBLE(K)*DELANG)
                      IF(K.NE.NSTEP) THEN
                          IF(ANGVAL.GE.STAR.AND.ANGVAL.LT.ENDD) THEN
                              BUCKET(K)=BUCKET(K)+1
                              TOTII=TOTII+1
                              GO TO 1
                          END IF
                      ELSE
C       K=NSTEP, INCLUDE END POINT
                          IF(ANGVAL.GE.STAR.AND.ANGVAL.LE.ENDD) THEN
                              BUCKET(K)=BUCKET(K)+1
                              TOTII=TOTII+1
                              GO TO 1
                          END IF
                      END IF
                  END DO

              END IF
C       WEIGHT 0, SKIP RAY
 1            CONTINUE
          END DO
C     NOW DO OUTPUT
 300      FORMAT('ANGLE OF INCIDENCE STATISTICS')
 400      FORMAT(
     1    'ANGLE OF REFRACTION, REFLECTION AND DIFFRACTION STATISTICS')
 402      FORMAT(7X,'FROM',11X, 'TO',8X,'SUCCESSFUL RAYS')
 406      FORMAT(5X,'DEGREES(S)',5X, 'DEGREE(S)')
 403      FORMAT(
     1    'SUCCESSFUL RAYS TRACED = ',I10)
 404      FORMAT(
     1    'SUCCESSFUL RAYS IN SPECIFIED RANGE = ',I10)
 407      FORMAT(
     1    'SPECIFIED ANGULAR RANGE = ',G15.8,'DEGREE(S)')
 408      FORMAT(
     1    'STATISTICS CALCULATED FOR SURFACE # = ',I4)
 409      FORMAT(
     1    'Y-OBJECT POINT = ',G15.8,1X,A9)
 410      FORMAT(
     1    'X-OBJECT POINT = ',G15.8,1X,A9)
 405      FORMAT(2X,G15.8,2X,G15.8,2X,I8)
          IF(J.EQ.1)  WRITE(OUTLYNE,300)
          IF(J.EQ.1)  CALL SHOWIT(0)
          IF(J.EQ.2) WRITE(OUTLYNE,400)
          IF(J.EQ.2)  CALL SHOWIT(0)
          WRITE(OUTLYNE,403) TOTI
          CALL SHOWIT(0)
          WRITE(OUTLYNE,404) TOTII
          CALL SHOWIT(0)
          WRITE(OUTLYNE,407) (ENDANG-STARANG)
          CALL SHOWIT(0)
          WRITE(OUTLYNE,408) NEWIMG
          CALL SHOWIT(0)
          WRITE(OUTLYNE,409) YOBP,STUNI
          CALL SHOWIT(0)
          WRITE(OUTLYNE,410) XOBP,STUNI
          CALL SHOWIT(0)
          WRITE(OUTLYNE,402)
          CALL SHOWIT(0)
          WRITE(OUTLYNE,406)
          CALL SHOWIT(0)
          DO I=1,NSTEP
              STAR=STARANG+(DBLE(I-1)*DELANG)
              ENDD=STARANG+(DBLE(I)*DELANG)
              WRITE(OUTLYNE,405) STAR,ENDD,BUCKET(I)
              CALL SHOWIT(0)
          END DO
          RETURN
      END
C SUB SPOPD1.FOR
      SUBROUTINE SPOPD1
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE SPOPD1.FOR.
C     IT HANDLES THE FIRST OPD CALCULATION FOR SPOT DIAGRAMS
C
!        REAL*8 WAV
C
!      LOGICAL OLDEXP
C
          INTEGER JJ,J
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datspd.inc'
          INTEGER WWVN,WWRF
          IF(INT(CURLAM).EQ.1) WWVN=46
          IF(INT(CURLAM).EQ.2) WWVN=47
          IF(INT(CURLAM).EQ.3) WWVN=48
          IF(INT(CURLAM).EQ.4) WWVN=49
          IF(INT(CURLAM).EQ.5) WWVN=50
          IF(INT(CURLAM).EQ.6) WWVN=71
          IF(INT(CURLAM).EQ.7) WWVN=72
          IF(INT(CURLAM).EQ.8) WWVN=73
          IF(INT(CURLAM).EQ.9) WWVN=74
          IF(INT(CURLAM).EQ.10) WWRF=75
          IF(INT(LFOB(4)).EQ.1) WWRF=46
          IF(INT(LFOB(4)).EQ.2) WWRF=47
          IF(INT(LFOB(4)).EQ.3) WWRF=48
          IF(INT(LFOB(4)).EQ.4) WWRF=49
          IF(INT(LFOB(4)).EQ.5) WWRF=50
          IF(INT(LFOB(4)).EQ.6) WWRF=71
          IF(INT(LFOB(4)).EQ.7) WWRF=72
          IF(INT(LFOB(4)).EQ.8) WWRF=73
          IF(INT(LFOB(4)).EQ.9) WWRF=74
          IF(INT(LFOB(4)).EQ.10) WWRF=75
C
C       OPD
          IF(RAYEXT.AND.REFEXT) THEN
              OOPD=0.0D0
              IF(DABS(ALENS(3,NEWOBJ)).GE.1.0D10) JJ=NEWOBJ+2
              IF(DABS(ALENS(3,NEWOBJ)).LT.1.0D10) JJ=NEWOBJ+1
              DO J=JJ,NEWIMG
                  OOPD=OOPD+RAYRAY(7,J)
     1            -(REFRY(7,J)*(ALENS(WWVN,J-1)/ALENS(WWRF,J-1)))
              END DO
              RETURN
          ELSE
C     RAY FAILED
              OOPD=0.0D0
          END IF
          RETURN
      END
C SUB SPOPD2.FOR
      SUBROUTINE SPOPD2(REFERR,TPT)
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE SPOPD2.FOR.
C     IT HANDLES THE SECOND OPD CALCULATION FOR SPOT DIAGRAMS
C
          REAL*8 LFOBW,WAV
C
          LOGICAL OLDEXP,REFERR
C
          INTEGER TPT
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datspd.inc'
C
          INTEGER WWVN,WWRF
          IF(INT(CURLAM).EQ.1) WWVN=46
          IF(INT(CURLAM).EQ.2) WWVN=47
          IF(INT(CURLAM).EQ.3) WWVN=48
          IF(INT(CURLAM).EQ.4) WWVN=49
          IF(INT(CURLAM).EQ.5) WWVN=50
          IF(INT(CURLAM).EQ.6) WWVN=71
          IF(INT(CURLAM).EQ.7) WWVN=72
          IF(INT(CURLAM).EQ.8) WWVN=73
          IF(INT(CURLAM).EQ.9) WWVN=74
          IF(INT(CURLAM).EQ.10) WWRF=75
          IF(INT(LFOB(4)).EQ.1) WWRF=46
          IF(INT(LFOB(4)).EQ.2) WWRF=47
          IF(INT(LFOB(4)).EQ.3) WWRF=48
          IF(INT(LFOB(4)).EQ.4) WWRF=49
          IF(INT(LFOB(4)).EQ.5) WWRF=50
          IF(INT(LFOB(4)).EQ.6) WWRF=71
          IF(INT(LFOB(4)).EQ.7) WWRF=72
          IF(INT(LFOB(4)).EQ.8) WWRF=73
          IF(INT(LFOB(4)).EQ.9) WWRF=74
          IF(INT(LFOB(4)).EQ.10) WWRF=75
C
C       OPD
          RCOR=0.0D0
          OCOR=0.0D0
          IF(SYSTEM1(30).EQ.1.0D0.OR.SYSTEM1(30).EQ.2.0D0) THEN
C       MODE FOCAL
C               RCOR=0.0D0
C               OCOR=0.0D0
              CALL FOPDS
C       CALCULATE THEN APPLY ADJUSTMENT FOR THE BEGINNING AND ENDING
C       REFERENCE SPHERES.
              OOPD=OOPD-(OCOR*ALENS(WWVN,NEWOBJ))
     1        +(RCOR*ALENS(WWVN,NEWOBJ))
              RCOR=0.0D0
              OCOR=0.0D0
              IF(SYSTEM1(30).LE.2.0D0) THEN
C     REFLOC=1 = CHIEF RAY
                  IF(REFLOC.EQ.1) CENCEN=.FALSE.
                  IF(REFLOC.EQ.3) CENCEN=.FALSE.
                  IF(REFLOC.EQ.4) CENCEN=.FALSE.
                  IF(REFLOC.EQ.2) CENCEN=.FALSE.
              ELSE
                  CENCEN=.FALSE.
              END IF
              OLDEXP=EXPAUT
              EXPAUT=.TRUE.
              CALL LOPDS(REFERR,TPT)
              IF(REFERR) RETURN
              CENCEN=.FALSE.
              EXPAUT=OLDEXP
              OOPD=OOPD-(OCOR*ALENS(WWVN,NEWIMG-1))+
     1        (RCOR*ALENS(WWVN,NEWIMG-1))
          ELSE
C       MODE AFOCAL
C               RCOR=0.0D0
C               OCOR=0.0D0
              CALL FOPDS
C       CALCULATE THEN APPLY ADJUSTMENT FOR THE BEGINNING AND ENDING
C       REFERENCE SPHERES.
              OOPD=OOPD-(OCOR*ALENS(WWVN,NEWOBJ))
     1        +(RCOR*ALENS(WWVN,NEWOBJ))
              RCOR=0.0D0
              OCOR=0.0D0
C     REFLOC=1= CHIEF RAY
              IF(SYSTEM1(30).LE.2.0D0) THEN
                  IF(REFLOC.EQ.1) CENCEN=.FALSE.
                  IF(REFLOC.EQ.3) CENCEN=.FALSE.
                  IF(REFLOC.EQ.4) CENCEN=.FALSE.
                  IF(REFLOC.EQ.2) CENCEN=.FALSE.
              ELSE
                  CENCEN=.FALSE.
              END IF
              OLDEXP=EXPAUT
              EXPAUT=.TRUE.
              CALL LOPDS(REFERR,TPT)
              IF(REFERR) RETURN
              EXPAUT=OLDEXP
              CENCEN=.FALSE.
              OOPD=OOPD-(OCOR*ALENS(WWVN,NEWIMG-1))
     1        +(RCOR*ALENS(WWVN,NEWIMG-1))
          END IF
          IF(INT(CURLAM).GE.1.AND.INT(CURLAM).LE.5) THEN
              LFOBW=SYSTEM1(INT(CURLAM))
          END IF
          IF(INT(CURLAM).GE.6.AND.INT(CURLAM).LE.10) THEN
              LFOBW=SYSTEM1(INT(CURLAM)+65)
          END IF
          IF(SYSTEM1(6).EQ.1.0D0) WAV=LFOBW*
     1    ((1.0D-3)/(25.4D0))
          IF(SYSTEM1(6).EQ.2.0D0) WAV=LFOBW*(1.0D-4)
          IF(SYSTEM1(6).EQ.3.0D0) WAV=LFOBW*(1.0D-3)
          IF(SYSTEM1(6).EQ.4.0D0) WAV=LFOBW*(1.0D-6)
          OOPD=-OOPD
          IF(REVSTR) OOPD=-OOPD
          OPDW=OOPD/WAV
          IF(DABS(OPDW).LT.1.0D-4) THEN
              OPDW=0.0D0
              OOPD=0.0D0
          END IF
          RETURN
      END
      SUBROUTINE SLOPES
C
          REAL*8 X,Y
C
          COMMON/SLOPY/X,Y
C
          INCLUDE 'datmai.inc'
C
C     RAY SLOPES
          IF(X.GT.(PII/2.0D0).AND.X.LE.PII) X=-(PII-X)
          IF(X.GT.(PII).AND.X.LE.((3.0D0*PII)/2.0D0)) X=-(PII-X)
          IF(X.GT.((3.0D0*PII)/2.0D0)) X=X-(TWOPII)
          IF(Y.GT.(PII/2.0D0).AND.Y.LE.PII) Y=-(PII-Y)
          IF(Y.GT.(PII).AND.Y.LE.((3.0D0*PII)/2.0D0)) Y=-(PII-Y)
          IF(Y.GT.((3.0D0*PII)/2.0D0)) Y=Y-(TWOPII)
          RETURN
      END
      SUBROUTINE SPOTIT(I)
          USE GLOBALS
          IMPLICIT NONE
          INTEGER ALLOERR,I
          INCLUDE 'datsp1.inc'
          INCLUDE 'datmai.inc'
          IF(I.EQ.1) THEN
C     DEALLOCATE THE DSPOTT ARRAY
              DEALLOCATE (DSPOTT,STAT=ALLOERR)
              RETURN
          END IF
          IF(I.EQ.2) THEN
C     ALLOCATE THE DSPOTT ARRAY
              DEALLOCATE (DSPOTT,STAT=ALLOERR)
              ALLOCATE (DSPOTT(1:60,1:NDSPOTT),STAT=ALLOERR)
              DSPOTT(1:60,1:NDSPOTT)=0.0D0
              RETURN
          END IF
          IF(I.EQ.3) THEN
C     LOAD THE DSPOTT ARRAY WITH DSPOT DATA
              DSPOTT(1:60,ID)=0.0D0
              DSPOTT(1:60,ID)=(DSPOT(1:60))
              RETURN
          END IF
          IF(I.EQ.4) THEN
C     LOAD THE DSPOT(*) ARRAY WITH DSPOTT(ID,*) DATA
              DSPOT(1:60)=0.0D0
              DSPOT(1:60)=(DSPOTT(1:60,ID))
              RETURN
          END IF
          IF(I.EQ.5) THEN
C     LOAD THE DSPOTT ARRAY, ELEMENT ID
              DSPOTT(1:60,ID)=0.0D0
              RETURN
          END IF
      END


C SUB GSPOT.FOR
      SUBROUTINE GSPOT
C
C     GREYS OPERAND
C
          IMPLICIT NONE
C
          REAL*8 SPT,V1,VALUE
     1    ,JA,JB,RRAD,RRANG,RPNT
     1    ,VXLO,VXHI,VYLO,VYHI,SPOTTY
C
          DIMENSION SPOTTY(:)
C
          ALLOCATABLE :: SPOTTY
C
          INTEGER WWRF,WWVN,ALLOERR
C
          LOGICAL TCLPRF,SPDTRA
C
          INTEGER SPDCD1,SPDCD2,JJ,JJJ,K
C
          COMMON/SPRA1/SPDTRA
          COMMON/SPRA2/SPDCD1,SPDCD2
C
          COMMON/SLOPY/X,Y

          REAL*8 LEN,OPDWT,GREYOP
C
          COMMON/WTOPD/OPDWT,GREYOP
C
C       THIS IS SUBROUTINE GSPOT.FOR.
C     IT HANDLES THE GREYS OPERAND
C
!        INTEGER KIK,KIKI
C
          REAL*8 X,Y
C
!        REAL*8 MSS
C
          INTEGER I,J,IWL
C
          LOGICAL FOBB0,FOBB0X,FOBB0Y
C
          COMMON/FFOOBB/FOBB0,FOBB0X,FOBB0Y
C
C     THE FOLLOWING VARIABLES TRACK THE REFERENCE SPHERE RADIUS
C     AND THE "OTHER RAY INTERSECTION POINTS WITH THIS REFERENCE
C     SPHERE
C
          REAL*8 SIZES,SLOPES,XSPH,YSPH,ZSPH,RADSPH
C
          COMMON/REFSPR/XSPH,YSPH,ZSPH,RADSPH
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datspd.inc'
          INCLUDE 'datsp1.inc'
          JA=COS_A_ANG
          JB=COS_B_ANG
C
C     SPOTTY(1)=DX OR DXA
C     SPOTTY(2)=DY OR DYA
C     SPOTTY(3)=RAY ENERGY TERM
C     SPOTTY(4)=OPD(lens units)
C     SPOTTY(5)=WAVELENGTH NUMBER
C     SPOTTY(6)=DXDX OR DXADX
C     SPOTTY(7)=DXAY OR DXADY
C     SPOTTY(8)=DYDX OR DYADX
C     SPOTTY(9)=DYDY OR DYADY
C     SPOTTY(10)=
C     SPOTTY(11)=
C     SPOTTY(12)=
C     SPOTTY(13)=
C     SPOTTY(14)=
C     SPOTTY(15)=
C
          DEALLOCATE (SPOTTY,STAT=ALLOERR)
          ALLOCATE (SPOTTY(1:15),STAT=ALLOERR)
C
          GRASET=.FALSE.
C
          GREYOP=0.0D0
C
          GSPDEXT=.FALSE.
C
C     CHECK IF ALL SPECTRAL WEIGHTS ARE ZERO
C
          IF(SYSTEM1(31).LE.0.0D0.AND.SYSTEM1(32).LE.0.0D0
     1    .AND.SYSTEM1(33).LE.0.0D0.AND.SYSTEM1(34).LE.0.0D0
     2    .AND.SYSTEM1(35).LE.0.0D0
     1    .AND.SYSTEM1(76).LE.0.0D0.AND.SYSTEM1(77).LE.0.0D0
     1    .AND.SYSTEM1(78).LE.0.0D0.AND.SYSTEM1(79).LE.0.0D0
     1    .AND.SYSTEM1(80).LE.0.0D0) THEN
C     NO SPOT CAN EXIST, RETURN
              GSPDEXT=.FALSE.
              DEALLOCATE(SPOTTY,STAT=ALLOERR)
              RETURN
          ELSE
C
C       NOT ALL SPECTRAL WEIGHTS ZERO, CALCULATE NUMBER OF RAYS MAXIMUM
C       PER WAVELENGTH ASSUMING SPECTRAL WEIGHTS NOT ZERO
C
              IW=0
              IWIW=0
              DO I=0,int(OPRINGTOT)
                  IWIW=IWIW+OPRINGPNT(I)
              END DO
          END IF
          LSF=.FALSE.
C
C       THE LAST FOB INPUT VALUES ARE STORED IN CHLFOB AND
C       THE ARRAY LFOB(1:7).
C
C       NOTE: DSPOT#(12) IS A RAW, UN-NORMALIZED
C       RAY ENERGY TERM BUT IS LATER (WHEN SPA IS CALC.) NORMALIZED
C       TO A FRACTIONAL ENERGY TERM BY DIVISION BY THE WEIGHTING
C       TERM TOT.
C     DOES THE NEWREF SURFACE HAVE A CLAP ON IT ?
          TCLPRF=.FALSE.
          IF(ALENS(9,NEWREF).EQ.0.0D0.OR.ALENS(127,NEWREF).NE.0.0D0) THEN
C     RING SPOT
C     ASSIGN A TEMPORARY CIRCULAR CLAP
              ALENS(9,NEWREF)=1.0D0
              IF(PXTRAY(1,NEWREF).GT.PXTRAX(1,NEWREF)) THEN
                  ALENS(10,NEWREF)=PXTRAY(1,NEWREF)
                  ALENS(11,NEWREF)=ALENS(10,NEWREF)
              ELSE
                  ALENS(10,NEWREF)=PXTRAX(1,NEWREF)
                  ALENS(11,NEWREF)=ALENS(10,NEWREF)
              END IF
              ALENS(12,NEWREF)=0.0D0
              ALENS(13,NEWREF)=0.0D0
              ALENS(14,NEWREF)=0.0D0
              ALENS(15,NEWREF)=0.0D0
              TCLPRF=.TRUE.
C     THERE WAS A CLEAR APERTURE, USE ITS VALUES
          END IF
C
C       IWL COUNTS THROUGH THE 10 WAVELENGTH NUMBERS
          I=0
          DO IWL=OLDLAMM,OLDLAMM
              IF(IWL.GE.1.AND.IWL.LE.5)  SPT=SYSTEM1(30+IWL)
              IF(IWL.GE.6.AND.IWL.LE.10) SPT=SYSTEM1(75+IWL-5)
              IF(SPT.GT.0.0D0) THEN
C     ONLY TRACE RAYS FOR NON-ZERO SPECTRAL WEIGHTS
C       TRACE RAYS AT THAT WAVELENGTH
C
C     SPOT WAS RING
C     DO RING TRACE HERE
C     OPRINGRAD WAS INPUT AS A FRACTIONAL REFERENCE SURFACE COORDINATE
C     AND IS STORED AS SUCH. IF THE REF AP HTS ARE DIFFERENT,
C     RAY AIMING WILL BE IN AN ELLIPTICAL RATHER THAN A CIRCULAR RING
C     BECAUSE OF THE FRACTIONAL RAY AIMING IN RRAY.FOR
                  VXLO=-1.0D0
                  VXHI=1.0D0
                  IF(LVIG) CALL VIGCAL(100,VXLO,VXHI,1)
                  VYLO=-1.0D0
                  VYHI=1.0D0
                  IF(LVIG) CALL VIGCAL(100,VYLO,VYHI,2)
                  DO JJ=0,int(OPRINGTOT)
C     THIS LOOPS FOR THE RINGS
                      IF(JJ.EQ.0) RRAD=0.0D0
                      IF(JJ.EQ.0) RPNT=1
                      IF(JJ.NE.0) RRAD=OPRINGRAD(JJ)
                      IF(JJ.NE.0) RPNT=OPRINGPNT(JJ)
C     THIS LOOPS THROUGH THE POINTS IN EACH RING
                      DO K=1,INT(RPNT)
                          IF(JJ.EQ.0) RRANG=0.0D0
                          IF(JJ.NE.0) THEN
                              RRANG=((TWOPII)/360.0D0)*OPRINGANG(JJ)
     1                        +(DBLE(K-1)*((TWOPII)/DBLE(OPRINGPNT(JJ))))
                          END IF
                          WW1=RRAD*DSIN(RRANG)
                          WW2=RRAD*DCOS(RRANG)
                          I=I+1
C
C       THE CALL TO RRAY HAS INPUTS:
C               QUALIFIER
                          WWQ='CAOB'
C               WW3
                          WW3=IWL
                          WVN=IWL
C               CACOCH IS SET TO 1 FOR SPOT DIAGRAMS
                          CACOCH = 1
                          SPDTRA=.TRUE.
                          MSG = .FALSE.
                          STOPP=0
C
C THE NEWOBJ,NEWREF AND NEWIMG ARE SET BY FOB
C       TRACE RAY AND RETURN
                          DSPOT(1)=0.0D0
                          DSPOT(2)=0.0D0
                          DSPOT(7)=0.0D0
                          DSPOT(9)=0.0D0
                          DSPOT(10)=0.0D0
                          DSPOT(11)=0.0D0
                          DSPOT(12)=0.0D0
                          DSPOT(17)=0.0D0
                          IF(WW1.GT.0.0D0) WW1=DABS(WW1)*VYHI
                          IF(WW1.LT.0.0D0) WW1=DABS(WW1)*VYLO
                          IF(WW1.EQ.0.0D0) WW1=0.0D0
                          IF(WW2.GT.0.0D0) WW2=DABS(WW2)*VXHI
                          IF(WW2.LT.0.0D0) WW2=DABS(WW2)*VXLO
                          IF(WW2.EQ.0.0D0) WW2=0.0D0
                          SAVE_KDP(1)=SAVEINPT(1)
                          WQ='CAOB    '
                          SQ=1
                          SST=0
                          DF1=0
                          DF2=0
                          DF3=0
                          DF4=1
                          DF5=1
                          S1=1
                          S2=1
                          S3=1
                          S4=0
                          S5=0
                          SN=1
                          W1=WW1
                          W2=WW2
                          W3=WW3
                          W4=0.0D0
                          W5=0.0D0
                          WC='RAY     '
                          NOCOAT=.FALSE.
                          GRASET=.FALSE.
                          DXFSET=.FALSE.
                          CALL RRAY
                          CACOCH=0
                          SPDCD1=RAYCOD(1)
                          SPDCD2=RAYCOD(2)
                          REST_KDP(1)=RESTINPT(1)
                          DSPOT(7)=DBLE(SPDCD1)
                          IF(DSPOT(7).NE.0.0D0) GO TO 1942
                          SPDTRA=.FALSE.
                          DSPOT(1)=RAYRAY(1,NEWIMG)
                          DSPOT(2)=RAYRAY(2,NEWIMG)
                          DSPOT(11)=RAYRAY(25,NEWIMG)
C
C
C       NOW CALCULATE THE RAY ENERGY TERM
C     THIS IS SPECTRAL WEIGHT TIMES APODIZATION FACTOR FOR THIS RAY
C
                          DSPOT(12)=(DSPOT(11))
                          IF(DSPOT(7).NE.0.0D0) DSPOT(12)=0.0D0
                          DSPOT(37)=DSPOT(12)*RAYRAY(9,NEWIMG)
                          DSPOT(38)=REFRY(9,NEWIMG)
                          DSPOT(39)=RAYRAY(9,NEWIMG)
                          DSPOT(44)=RAYRAY(9,NEWREF)
C
                          IF(IWL.GE.1.AND.IWL.LE.5)
     1                    DSPOT(17)=SYSTEM1(30+IWL)
                          IF(IWL.GE.6.AND.IWL.LE.10)
     1                    DSPOT(17)=SYSTEM1(75+IWL-5)
C     WRITE DATA TO THE ARRAY INSTEAD OF TO A FILE
                          OOPD=0.0D0
                          IF(INT(LFOB(4)).EQ.1) WWRF=46
                          IF(INT(LFOB(4)).EQ.2) WWRF=47
                          IF(INT(LFOB(4)).EQ.3) WWRF=48
                          IF(INT(LFOB(4)).EQ.4) WWRF=49
                          IF(INT(LFOB(4)).EQ.5) WWRF=50
                          IF(INT(LFOB(4)).EQ.6) WWRF=71
                          IF(INT(LFOB(4)).EQ.7) WWRF=72
                          IF(INT(LFOB(4)).EQ.8) WWRF=73
                          IF(INT(LFOB(4)).EQ.9) WWRF=74
                          IF(INT(LFOB(4)).EQ.10) WWRF=75
                          IF(IWL.EQ.1) WWVN=46
                          IF(IWL.EQ.2) WWVN=47
                          IF(IWL.EQ.3) WWVN=48
                          IF(IWL.EQ.4) WWVN=49
                          IF(IWL.EQ.5) WWVN=50
                          IF(IWL.EQ.6) WWVN=71
                          IF(IWL.EQ.7) WWVN=72
                          IF(IWL.EQ.8) WWVN=73
                          IF(IWL.EQ.9) WWVN=74
                          IF(IWL.EQ.10) WWVN=75
                          IF(RAYEXT.AND.REFEXT) THEN
                              LEN=0.0D0
                              RCOR=0.0D0
                              OCOR=0.0D0
                              IF(DABS(ALENS(3,NEWOBJ)).GE.1.0D10) JJJ=NEWOBJ+2
                              IF(DABS(ALENS(3,NEWOBJ)).LT.1.0D10) JJJ=NEWOBJ+1
                              DO J=JJJ,NEWIMG
                                  LEN=LEN+RAYRAY(7,J)
     1                            -(REFRY(7,J)*(ALENS(WWVN,J-1)/ALENS(WWRF,J-1)))
                              END DO
                              IF(SYSTEM1(30).EQ.1.0D0.OR.SYSTEM1(30).EQ.2.0D0) THEN
C       MODE FOCAL
                                  RCOR=0.0D0
                                  OCOR=0.0D0
                                  CALL FOPD
                                  LEN=LEN-(OCOR*ALENS(WWVN,NEWOBJ))
     1                            +(RCOR*ALENS(WWVN,NEWOBJ))
                                  RCOR=0.0D0
                                  OCOR=0.0D0
                                  CENCEN=.FALSE.
                                  CALL LOPD
                                  LEN=LEN-(OCOR*ALENS(WWVN,NEWIMG-1))
     1                            +(RCOR*ALENS(WWVN,NEWIMG-1))
                              ELSE
C       MODE AFOCAL
                                  RCOR=0.0D0
                                  OCOR=0.0D0
                                  CALL FOPD
                                  LEN=LEN-(OCOR*ALENS(WWVN,NEWOBJ))
     1                            +(RCOR*ALENS(WWVN,NEWOBJ))
                                  RCOR=0.0D0
                                  OCOR=0.0D0
                                  CENCEN=.FALSE.
                                  CALL LOPD
                                  LEN=LEN-(OCOR*ALENS(WWVN,NEWIMG-1))
     1                            +(RCOR*ALENS(WWVN,NEWIMG-1))
                              END IF
                              OOPD=LEN
                          ELSE
C       NO RAY EXITS OR NO REF RAY
                              OOPD=0.0D0
                          END IF
                          SPOTTY(4)=OOPD
                          SPOTTY(5)=DBLE(IWL)
 1942                     IF(SYSTEM1(30).LE.2.0D0) THEN
C     FOCAL
                              SPOTTY(1)=(DSPOT(1)-REFRY(1,NEWIMG))/JB
                              SPOTTY(2)=(DSPOT(2)-REFRY(2,NEWIMG))/JA
                              SPOTTY(3)=DSPOT(12)
                              SPOTTY(6) = (DIFF(1,NEWIMG)-RAYRAY(1,NEWIMG))/(DELX*JB)
                              SPOTTY(7) = (DIFF(7,NEWIMG)-RAYRAY(1,NEWIMG))/(DELY*JB)
                              SPOTTY(8) = (DIFF(2,NEWIMG)-RAYRAY(2,NEWIMG))/(DELX*JA)
                              SPOTTY(9) = (DIFF(8,NEWIMG)-RAYRAY(2,NEWIMG))/(DELY*JA)
                          ELSE
C     AFOCAL
                              VALUE=RAYRAY(11,NEWIMG)
     1                        -REFRY(11,NEWIMG)
                              IF(REAL(VALUE).GT.REAL(PII)) VALUE=VALUE-(TWOPII)
                              IF(REAL(VALUE).EQ.REAL(TWOPII)) VALUE=0.0D0
                              SPOTTY(1)=VALUE
                              VALUE=RAYRAY(12,NEWIMG)
     1                        -REFRY(12,NEWIMG)
                              IF(REAL(VALUE).GT.REAL(PII)) VALUE=VALUE-(TWOPII)
                              IF(REAL(VALUE).EQ.REAL(TWOPII)) VALUE=0.0D0
                              SPOTTY(2)=VALUE
                              SPOTTY(3)=DSPOT(12)
                              IF(DABS(DIFF(4,NEWIMG)).GE.
     1                        (1.0D35*DABS(DIFF(6,NEWIMG)))) THEN
                                  IF(REAL(DIFF(4,NEWIMG)).GE.0.0) V1=PII/2.0D0
                                  IF(REAL(DIFF(4,NEWIMG)).LT.0.0) V1=(3.0D0*PII)/2.0D0
                              ELSE
                                  IF(DABS(DIFF(4,NEWIMG)).EQ.0.0D0.AND.
     1                            DABS(DIFF(6,NEWIMG)).EQ.0.0D0) THEN
                                      V1=0.0D0
                                  ELSE
                                      V1=DATAN2(DIFF(4,NEWIMG),DIFF(6,NEWIMG))
                                  END IF
                                  IF(REAL(V1).LT.0.0) V1=V1+(TWOPII)
                              END IF
                              VALUE=(V1-RAYRAY(11,NEWIMG))
                              IF(REAL(VALUE).GT.REAL(PII)) VALUE=VALUE-(TWOPII)
                              IF(REAL(VALUE).EQ.REAL(TWOPII)) VALUE=0.0D0
                              SPOTTY(6)=VALUE/DELX

                              IF(DABS(DIFF(10,NEWIMG)).GE.
     1                        (1.0D35*DABS(DIFF(12,NEWIMG)))) THEN
                                  IF(REAL(DIFF(10,NEWIMG)).GE.0.0) V1=PII/2.0D0
                                  IF(REAL(DIFF(10,NEWIMG)).LT.0.0) V1=(3.0D0*PII)/2.0D0
                              ELSE
                                  IF(DABS(DIFF(10,NEWIMG)).EQ.0.0D0.AND.
     1                            DABS(DIFF(12,NEWIMG)).EQ.0.0D0) THEN
                                      V1=0.0D0
                                  ELSE
                                      V1=DATAN2(DIFF(10,NEWIMG),DIFF(12,NEWIMG))
                                  END IF
                                  IF(REAL(V1).LT.0.0) V1=V1+(TWOPII)
                              END IF
                              VALUE=(V1-RAYRAY(11,NEWIMG))
                              IF(REAL(VALUE).GT.REAL(PII)) VALUE=VALUE-(TWOPII)
                              IF(REAL(VALUE).EQ.REAL(TWOPII)) VALUE=0.0D0
                              SPOTTY(7)=VALUE/DELY

                              IF(DABS(DIFF(5,NEWIMG)).GE.
     1                        (1.0D35*DABS(DIFF(6,NEWIMG)))) THEN
                                  IF(REAL(DIFF(5,NEWIMG)).GE.0.0) V1=PII/2.0D0
                                  IF(REAL(DIFF(5,NEWIMG)).LT.0.0) V1=(3.0D0*PII)/2.0D0
                              ELSE
                                  IF(DABS(DIFF(5,NEWIMG)).EQ.0.0D0.AND.
     1                            DABS(DIFF(6,NEWIMG)).EQ.0.0D0) THEN
                                      V1=0.0D0
                                  ELSE
                                      V1=DATAN2(DIFF(5,NEWIMG),DIFF(6,NEWIMG))
                                  END IF
                                  IF(REAL(V1).LT.0.0) V1=V1+(TWOPII)
                              END IF
                              VALUE=(V1-RAYRAY(12,NEWIMG))
                              IF(REAL(VALUE).GT.REAL(PII)) VALUE=VALUE-(TWOPII)
                              IF(REAL(VALUE).EQ.REAL(TWOPII)) VALUE=0.0D0
                              SPOTTY(8)=VALUE/DELX

                              IF(DABS(DIFF(11,NEWIMG)).GE.
     1                        (1.0D35*DABS(DIFF(12,NEWIMG)))) THEN
                                  IF(REAL(DIFF(11,NEWIMG)).GE.0.0) V1=PII/2.0D0
                                  IF(REAL(DIFF(11,NEWIMG)).LT.0.0) V1=(3.0D0*PII)/2.0D0
                              ELSE
                                  IF(DABS(DIFF(11,NEWIMG)).EQ.0.0D0.AND.
     1                            DABS(DIFF(12,NEWIMG)).EQ.0.0D0) THEN
                                      V1=0.0D0
                                  ELSE
                                      V1=DATAN2(DIFF(11,NEWIMG),DIFF(12,NEWIMG))
                                  END IF
                                  IF(REAL(V1).LT.0.0) V1=V1+(TWOPII)
                              END IF
                              VALUE=(V1-RAYRAY(12,NEWIMG))
                              IF(REAL(VALUE).GT.REAL(PII)) VALUE=VALUE-(TWOPII)
                              IF(REAL(VALUE).EQ.(TWOPII)) VALUE=0.0D0
                              SPOTTY(9)=VALUE/DELY
                          END IF
                          SIZES=DABS(SPOTTY(3))*(DABS(SPOTTY(1))+DABS(SPOTTY(2)))
                          SLOPES=DABS(SPOTTY(3))*(DABS(SPOTTY(6))+DABS(SPOTTY(7))
     1                    +DABS(SPOTTY(8))+DABS(SPOTTY(9)))
                          GREYOP=GREYOP+SIZES+SLOPES+(OPDWT*OOPD)
                      END DO
                  END DO
C       SPECTRAL WEIGHT WAS ZERO, GO TO NEXT WAVELENGTH
C     NO FILE OUTPUT WAS DONE
              END IF
          END DO
C     WRITE THE TOTAL # OF RECORDS IN RECORD #1
          ITOT=I
C
C     ALL THE RAYS HAVE BEEN TRACED, NOW DO THE STATISTICS AND
C     OUTPUT.
          IF(TCLPRF) THEN
C     REMOVE TEMP CLAP ON NEWREF
              TCLPRF=.FALSE.
              ALENS(9:15,NEWREF)=0.0D0
          END IF
C
C
          IF(ITOT.EQ.0) THEN
              SPDEXT=.FALSE.
              GSPDEXT=.FALSE.
              REFEXT=.FALSE.
              GREYOP=0.0D0
              CALL MACFAL
              DEALLOCATE(SPOTTY,STAT=ALLOERR)
              RETURN
          END IF
          GSPDEXT=.TRUE.
          DEALLOCATE(SPOTTY,STAT=ALLOERR)
          RETURN
      END
C SUB SPMOVE.FOR
      SUBROUTINE SPMOVE
C
          IMPLICIT NONE
C
          CHARACTER UN*11
C
C       THIS IS SUBROUTINE SPMOVE.FOR.
C
          REAL*8 SPT,TOT,W,JK_S
     3    ,MSSX,MSSY,JA,JB
C
          REAL*8
     2    SPDELL,MSS,DELTA,APFAC
C
          INTEGER I,J,NUMT1,NUMT2,NUMT3,NUMT4,
     1    NUMT5,NUMT6,NUMT7,NUMT8,NUMT9,NUMT10
C
          LOGICAL FOBB0
     1    ,FOBB0X,FOBB0Y
C
          COMMON/FFOOBB/FOBB0,FOBB0X,FOBB0Y
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datspd.inc'
          INCLUDE 'datsp1.inc'
C
          JA=COS_A_ANG
          JB=COS_B_ANG
C
          IF(SYSTEM1(30).EQ.3.0D0.OR.SYSTEM1(30).EQ.4.0D0) THEN
              OUTLYNE='"SPD (MOVE OR MOVEACC)"'
              CALL SHOWIT(1)
              OUTLYNE='ONLY WORKS FOR MODES FOCAL AND UFAOCAL'
              CALL SHOWIT(1)
              OUTLYNE='NO ACTION TAKEN'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SST.EQ.1) THEN
              OUTLYNE='"SPD (MOVE OR MOVEACC)" TAKES NO STRING INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C       DEFAULT NUMERIC INPUT, ALL DEFAULTS ARE ZERO
          IF(DF1.EQ.1) THEN
              OUTLYNE=
     1        '"SPD (MOVE OR MOVEACC)" REQUIERS EXPLICIT NUMERIC'
              CALL SHOWIT(1)
              OUTLYNE='WORD #1 INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1)THEN
              OUTLYNE=
     1        '"SPD (MOVE OR MOVEACC)"'
              CALL SHOWIT(1)
              OUTLYNE='TAKES NO NUMERIC WORD #2 THROUGH #5 INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
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
          DELTA=W1
          GO TO 999
C     THIS IS THE RE-ENTRY POINT IF SPMOVE NEEDS TO BE CALLED
C     DURING A GEOMETRICAL RADIAL ENERGY DISTRIBUTION CALCUALTION
C     OR A GEOMETRICAL LINE SPREAD FUNCTION
        ENTRY SPMV(SPDELL)
C
          DELTA=SPDELL
 999      CONTINUE
C
C       THE NUMBER OF RAYS WHICH REPRESENT 100% OF THE ENERGY
C       EQUAL THE NUMBER OF RAYS TRACED (IWIW) TIMES THE MUNBER
C       OF NON-ZERO SPECTRAL WEIGHTS.
C
C       TOT IS THE WEIGTHED TOTAL
C       NUMTOT IS THE RAW TOTAL OF NON-FAILED RAYS
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
          DO I=2,ITOT
C     LOAD DSPOT(*) WITH DSPOTT(*,ID)
              ID=I-1
              CALL SPOTIT(4)
C     LOAD DSPOTT FROM DSPOT
              J=I-1
              ID=J
              CALL SPOTIT(3)
              IF(DSPOT(17).NE.0.0D0) THEN
                  IF(DSPOT(7).EQ.0.0D0) THEN
                      APFAC=DSPOT(11)
                      TOT=TOT+(DSPOT(17)*APFAC)
                      NUMTOT=NUMTOT+1
                      IF(DSPOT(16).EQ.1.0D0) NUMT1=NUMT1+1
                      IF(DSPOT(16).EQ.2.0D0) NUMT2=NUMT2+1
                      IF(DSPOT(16).EQ.3.0D0) NUMT3=NUMT3+1
                      IF(DSPOT(16).EQ.4.0D0) NUMT4=NUMT4+1
                      IF(DSPOT(16).EQ.5.0D0) NUMT5=NUMT5+1
                      IF(DSPOT(16).EQ.6.0D0) NUMT6=NUMT6+1
                      IF(DSPOT(16).EQ.7.0D0) NUMT7=NUMT7+1
                      IF(DSPOT(16).EQ.8.0D0) NUMT8=NUMT8+1
                      IF(DSPOT(16).EQ.9.0D0) NUMT9=NUMT9+1
                      IF(DSPOT(16).EQ.10.0D0) NUMT10=NUMT10+1
                  ELSE
C       RAY FAILED, DONT DO STATS
                  END IF
              ELSE
C       SPECTRAL WEIGHT 0
              END IF
          END DO
C
          IF(NUMTOT.EQ.0.0D0) THEN
              IF(WQ.NE.'ACC') THEN
                  OUTLYNE='NO RAYS COULD GET THROUGH THE SYSTEM'
                  CALL SHOWIT(1)
                  OUTLYNE='NO SPOT DIAGRAM WAS CREATED'
                  CALL SHOWIT(1)
C     DEALLOCATE DSPOTT
                  CALL SPOTIT(1)
              END IF
              SPDEXT=.FALSE.
              CALL MACFAL
              RETURN
          END IF
          SPA=0.0D0
          SPC=0.0D0
          SPB=0.0D0
          SPD=0.0D0
          DO I=1,ITOT-1
C     LOAD DSPOTT(I,*) INTO DSPOT(*)
              ID=I
              CALL SPOTIT(4)
              IF(DSPOT(17).NE.0.0D0) THEN
                  IF(DSPOT(7).EQ.0.0D0) THEN
                      APFAC=DSPOT(11)
                      SPA=SPA+(DSPOT(17)*DSPOT(1)*APFAC)
                      SPC=SPC+(DSPOT(17)*DSPOT(2)*APFAC)
                      SPB=SPB+(DSPOT(17)*DTAN(DSPOT(9))*APFAC)
                      SPD=SPD+(DSPOT(17)*DTAN(DSPOT(10))*APFAC)
                  ELSE
C     RAY FAILED
                  END IF
              ELSE
C     WEIGHT 0
              END IF
          END DO
C       W IS THE NORMALIZING FACTOR FOR THE SPOT CALCULATIONS
          W=TOT
          SPA=SPA/W
          SPC=SPC/W
          SPB=SPB/W
          SPD=SPD/W
          CENTX=SPA+(SPB*DELTA)
          CENTY=SPC+(SPD*DELTA)
          IF(DABS(CENTX).LT.1.0D-15) CENTX=0.0D0
          IF(DABS(CENTY).LT.1.0D-15) CENTY=0.0D0
C
C       NOW CALCULATE THE ROOT MEAN SQUARE SPOT SIZE
          LA=0.0D0
          LC=0.0D0
          LB=0.0D0
          LD=0.0D0
          SSSP=0.0D0
          SSSQ=0.0D0
          SSSR=0.0D0
          SSSPX=0.0D0
          SSSQX=0.0D0
          SSSRX=0.0D0
          SSSPY=0.0D0
          SSSQY=0.0D0
          SSSRY=0.0D0
          MSS=0.0D0
          MSSX=0.0D0
          MSSY=0.0D0
          RMS=0.0D0
          RMSX=0.0D0
          RMSY=0.0D0
          DO I=1,ITOT-1
C     LOAD DSPOTT(I,*) INTO DSPOT(*)
              ID=I
              CALL SPOTIT(4)
              IF(DSPOT(17).NE.0.0D0) THEN
                  IF(DSPOT(7).EQ.0.0D0) THEN
                      APFAC=DSPOT(11)
                      LA=DSPOT(1)
                      LC=DSPOT(2)
                      LB=DTAN(DSPOT(9))
                      LD=DTAN(DSPOT(10))
                      SPT=DSPOT(17)
                      J=INT(DSPOT(16))
C       X ONLY
                      IF(J.GE.1.AND.J.LE.5) JK_S=SYSTEM1(30+J)
                      IF(J.GE.6.AND.J.LE.10) JK_S=SYSTEM1(75+J-5)
                      SSSPX=SSSPX+
     1                (JK_S*(((LA-SPA)**2))*APFAC)
                      SSSQX=SSSQX+
     1                (JK_S*((LA*LB)-(LA*SPB)-(LB*SPA)
     2                +(SPA*SPB))*APFAC)
                      SSSRX=SSSRX+
     1                (JK_S*(((LB-SPB)**2))*APFAC)
C       Y ONLY
                      SSSPY=SSSPY+
     1                (JK_S*(((LC-SPC)**2))*APFAC)
                      SSSQY=SSSQY+
     1                (JK_S*((LC*LD)
     2                -(LC*SPD)-(LD*SPC)+(SPC*SPD))*APFAC)
                      SSSRY=SSSRY+
     1                (JK_S*(((LD-SPD)**2))*APFAC)
                  ELSE
                  END IF
              ELSE
              END IF
          END DO
          MSSX=SSSPX+(2.0D0*DELTA*SSSQX)+((DELTA**2)*SSSRX)
          MSSY=SSSPY+(2.0D0*DELTA*SSSQY)+((DELTA**2)*SSSRY)
          MSS=(MSSX+MSSY)/2.0D0
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
          IF(SYSTEM1(6).EQ.1.0) UN='INCHES     '
          IF(SYSTEM1(6).EQ.2.0) UN='CENTIMETERS'
          IF(SYSTEM1(6).EQ.3.0) UN='MILLIMETERS'
          IF(SYSTEM1(6).EQ.4.0) UN='METERS'
          RMSX=2.0D0*(DSQRT(MSSX/W))
          RMSY=2.0D0*(DSQRT(MSSY/W))
          RMS=(RMSX+RMSY)/2.0D0
          IF(WQ.EQ.'MOVEACC') THEN
C       NOW SET THE GENERAL PURPOSE REGISTERS
C       THESE CAN BE USED AS WELL AS GET FEATURES WHICH
C       RE-PROCESS THE CURRENT SPOT DIAGRAM.
              REG(40)=REG(9)
              IF(SYSTEM1(30).LT.3.0D0) THEN
                  REG(11)=RMSY/JA
                  REG(10)=RMSX/JB
              ELSE
                  REG(11)=RMSY
                  REG(10)=RMSX
              END IF
              REG(9)=DSQRT((RMSX**2)+(RMSY**2))
          ELSE
          END IF
 114      FORMAT('FOCUS SHIFTED SPOT DIAGRAM SUMMARY')
 321      FORMAT('FOR A FOCUS SHIFT   = ',G17.10,1X,A11)
 115      FORMAT('RMS SPOT SIZE   (X) = ',G17.10,1X,A11)
 116      FORMAT('RMS SPOT SIZE   (Y) = ',G17.10,1X,A11)
 108      FORMAT('RMS SPOT DIAMETER   = ',G17.10,1X,A11)
 101      FORMAT('X-CENTROID POSITION = ',G17.10,1X,A11)
 102      FORMAT('Y-CENTROID POSITION = ',G17.10,1X,A11)
 103      FORMAT('CHIEF RAY X-COORD.  = ',G17.10,1X,A11)
 104      FORMAT('CHIEF RAY Y-COORD.  = ',G17.10,1X,A11)
 105      FORMAT('# RAYS ATTEMPTED    = ',I5,' AT EACH WAVELENGTH')
 106      FORMAT('# RAYS SUCCEDED     = ',I5,' AT ALL WAVELENGTHS')
 107      FORMAT(
     1    '"FOB NULL" WAS USED, CHIEF RAY DATA MAY BE SUSPECT')
 221      FORMAT(
     1    'FOCUS SHIFT FOR BEST RMS SPOT LENGTH (X) = ',G17.10,1X,A11)
 222      FORMAT(
     1    'FOCUS SHIFT FOR BEST RMS SPOT LENGTH (Y) = ',G17.10,1X,A11)
 220      FORMAT(
     1    'FOCUS SHIFT FOR BEST RMS SPOT DIAMETER   = ',G17.10,1X,A11)
          IF(WQ.EQ.'MOVE') THEN
C       DO THE PRINT OUT

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

              WRITE(OUTLYNE,101) CENTX,UN
              CALL SHOWIT(0)

              WRITE(OUTLYNE,102) CENTY,UN
              CALL SHOWIT(0)

              WRITE(OUTLYNE,103) REFRY(1,NEWIMG)+
     1        (DTAN(REFRY(11,NEWIMG))*DELTA),UN
              CALL SHOWIT(0)

              WRITE(OUTLYNE,104) REFRY(2,NEWIMG)+
     1        (DTAN(REFRY(12,NEWIMG))*DELTA),UN
              CALL SHOWIT(0)

              IF(NULL) WRITE(OUTLYNE,107)
              CALL SHOWIT(0)

              WRITE(OUTLYNE,105) IWIW
              CALL SHOWIT(0)

              WRITE(OUTLYNE,106) NUMTOT
              CALL SHOWIT(0)
C
 2110         FORMAT('AT WAVELENGTH # 1,  # RAYS SUCEEDED = ',I10)
 2111         FORMAT('AT WAVELENGTH # 2,  # RAYS SUCEEDED = ',I10)
 2112         FORMAT('AT WAVELENGTH # 3,  # RAYS SUCEEDED = ',I10)
 2113         FORMAT('AT WAVELENGTH # 4,  # RAYS SUCEEDED = ',I10)
 2114         FORMAT('AT WAVELENGTH # 5,  # RAYS SUCEEDED = ',I10)
 2115         FORMAT('AT WAVELENGTH # 6,  # RAYS SUCEEDED = ',I10)
 2116         FORMAT('AT WAVELENGTH # 7,  # RAYS SUCEEDED = ',I10)
 2117         FORMAT('AT WAVELENGTH # 8,  # RAYS SUCEEDED = ',I10)
 2118         FORMAT('AT WAVELENGTH # 9,  # RAYS SUCEEDED = ',I10)
 2119         FORMAT('AT WAVELENGTH # 10, # RAYS SUCEEDED = ',I10)

              WRITE(OUTLYNE,2110) NUMT1
              CALL SHOWIT(0)
              WRITE(OUTLYNE,2111) NUMT2
              CALL SHOWIT(0)
              WRITE(OUTLYNE,2112) NUMT3
              CALL SHOWIT(0)
              WRITE(OUTLYNE,2113) NUMT4
              CALL SHOWIT(0)
              WRITE(OUTLYNE,2114) NUMT5
              CALL SHOWIT(0)
              WRITE(OUTLYNE,2115) NUMT6
              CALL SHOWIT(0)
              WRITE(OUTLYNE,2116) NUMT7
              CALL SHOWIT(0)
              WRITE(OUTLYNE,2117) NUMT8
              CALL SHOWIT(0)
              WRITE(OUTLYNE,2118) NUMT9
              CALL SHOWIT(0)
              WRITE(OUTLYNE,2119) NUMT10
              CALL SHOWIT(0)

              IF(W1.EQ.0.0D0) THEN
                  WRITE(OUTLYNE,220) FCSFT
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,221) FCSFTX
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,222) FCSFTY
                  CALL SHOWIT(0)
              END IF
          END IF
          RETURN
      END
      SUBROUTINE FIXWV(W2)
          IMPLICIT NONE
          INCLUDE 'datlen.inc'
          REAL*8 OLDWT(1:10),W2
          COMMON/OLDSTUFFWV/OLDWT
          OLDWT(1)=SYSTEM1(31)
          OLDWT(2)=SYSTEM1(32)
          OLDWT(3)=SYSTEM1(33)
          OLDWT(4)=SYSTEM1(34)
          OLDWT(5)=SYSTEM1(35)
          OLDWT(6)=SYSTEM1(76)
          OLDWT(7)=SYSTEM1(77)
          OLDWT(8)=SYSTEM1(78)
          OLDWT(9)=SYSTEM1(79)
          OLDWT(10)=SYSTEM1(80)
          IF(W2.GE.1.0D0.AND.W2.LE.10.0D0) THEN
              SYSTEM1(31)=0.0D0
              SYSTEM1(32)=0.0D0
              SYSTEM1(33)=0.0D0
              SYSTEM1(34)=0.0D0
              SYSTEM1(35)=0.0D0
              SYSTEM1(76)=0.0D0
              SYSTEM1(77)=0.0D0
              SYSTEM1(78)=0.0D0
              SYSTEM1(79)=0.0D0
              SYSTEM1(80)=0.0D0
          END IF
          IF(INT(W2).EQ.1) SYSTEM1(31)=1.0D0
          IF(INT(W2).EQ.2) SYSTEM1(32)=1.0D0
          IF(INT(W2).EQ.3) SYSTEM1(33)=1.0D0
          IF(INT(W2).EQ.4) SYSTEM1(34)=1.0D0
          IF(INT(W2).EQ.5) SYSTEM1(35)=1.0D0
          IF(INT(W2).EQ.6) SYSTEM1(76)=1.0D0
          IF(INT(W2).EQ.7) SYSTEM1(77)=1.0D0
          IF(INT(W2).EQ.8) SYSTEM1(78)=1.0D0
          IF(INT(W2).EQ.9) SYSTEM1(79)=1.0D0
          IF(INT(W2).EQ.10) SYSTEM1(80)=1.0D0
          RETURN
      END
      SUBROUTINE UNFIXWV
          IMPLICIT NONE
          INCLUDE 'datlen.inc'
          REAL*8 OLDWT(1:10)
          COMMON/OLDSTUFFWV/OLDWT
          SYSTEM1(31)=OLDWT(1)
          SYSTEM1(32)=OLDWT(2)
          SYSTEM1(33)=OLDWT(3)
          SYSTEM1(34)=OLDWT(4)
          SYSTEM1(35)=OLDWT(5)
          SYSTEM1(76)=OLDWT(6)
          SYSTEM1(77)=OLDWT(7)
          SYSTEM1(78)=OLDWT(8)
          SYSTEM1(79)=OLDWT(9)
          SYSTEM1(80)=OLDWT(10)
          RETURN
      END
      SUBROUTINE REVERSECA
          IMPLICIT NONE
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          INTEGER SPDCD1,SPDCD2
          COMMON/SPRA2/SPDCD1,SPDCD2
C       IF CURRENT RAY FAILS AT SURFACE I, SET SPDCD1 AND SPDCD2
C       AND RETURN
C     DON'T CHECK ON OBJECT OR IMAGE SURFACES.
          DO R_I=NEWIMG-1,NEWOBJ+1,-1
              MSG=.FALSE.
C       CALL CLAP CHECKING ROUTINE
              R_X=RAYRAY(1,R_I)
              R_Y=RAYRAY(2,R_I)
              R_Z=RAYRAY(3,R_I)
              CALL CACHEK(0.0D0,0.0D0,0.0D0,0)
              IF(RAYCOD(1).NE.0) THEN
                  SPDCD1=RAYCOD(1)
                  SPDCD2=RAYCOD(2)
                  RETURN
              END IF
          END DO
          RETURN
      END


C SUB SPOT1.FOR
      SUBROUTINE SPOT1(TPT)
          USE GLOBALS
C
          IMPLICIT NONE
C
          CHARACTER UN*11
C
          REAL*8 SPT,SPT1,SPT2,SPT3,SPT4,
     1    SPT5,RRAD,RRANG,RPNT,RANDX,SPT6,SPT7,SPT8,SPT9
     1    ,RINGTOT1,SPT10,OSPA,OSPB,OSPC,OSPD,OAFSPB,OAFSPD
     3    ,RESLT
C
          REAL*8 ELMAX
C
          COMMON/ELEV/ELMAX
C
          COMMON/SPTWTS/SPT1,SPT2,SPT3,SPT4,SPT5
     1    ,SPT6,SPT7,SPT8,SPT9,SPT10
C
          LOGICAL TCLPRF,SPDTRA
C
          INTEGER NSTEP,LSTIMG,TPT,SPDCD1,SPDCD2,JJ,K
C
          COMMON/SPRA1/SPDTRA
          COMMON/SPRA2/SPDCD1,SPDCD2
C
          COMMON/SLOPY/X,Y
          LOGICAL CAFORWARD
          COMMON/FORWARDCA/CAFORWARD
C
C
C     IF TPT=1 REGULAR SPOT
C     IF TPT=2 OPTIM/TOLR SPOT
C
C       THIS IS SUBROUTINE SPOT1.FOR.
C     IT HANDLES THE OPERATION OF THE COMMANDS:
C
C               SPD
C               SPD ACC
C
C     THESE COMMANDS ONLY TAKE NUMERIC WORD #1,2,3,4
C               SPD ISTAT
C               SPD IPSTAT
C
C     THESE COMMANDS ONLY TAKE NUMERIC WORD #1
C
C     NUMERIC WORD #1 IS TH NUMBER OF RAYS ACROSS A RECTANGULAR GRID
C     NUMERIC WORD #2 IS FFLAG USED TO SET THE DIRECTION OF CLEAR APERTURE
C       AND OBSCURATION CHECKING
C
C
!        INTEGER KIK,KIKI
C
          REAL*8 NSTART1,NSTOP1,NDEL1,NSTART2,NSTOP2,NDEL2,
     2    TOT,W,AMSS,IX,IY,XUP,YUP,XLO,YLO
     3    ,X,Y,MSSX,MSSY,AMSSX,AMSSY,MAXX,MAXY,APODX2,APODR2
C
          COMMON/BIGGY/XUP,YUP,XLO,YLO,MAXX,MAXY
C
          REAL*8 STARANG,ENDANG,DELANG
     2    ,FRAC,FRACA,FRACB,MSS,APFAC
C
          INTEGER I,IWIW2,IWL,NUMT1,NUMT2,NUMT3,NUMT4,
     1    NUMT5,NUMT6,NUMT7,NUMT8,NUMT9,NUMT10,TEMPHOLDER
C
          LOGICAL ODD,FOBB0
     1    ,FOBB0X,FOBB0Y,IMFLAG
C
          COMMON/FFOOBB/FOBB0,FOBB0X,FOBB0Y
C
C     THE FOLLOWING VARIABLES TRACK THE REFERENCE SPHERE RADIUS
C     AND THE "OTHER RAY INTERSECTION POINTS WITH THIS REFERENCE
C     SPHERE
C
          REAL*8 JA,JB,XSPH,YSPH,ZSPH,RADSPH
C
          COMMON/REFSPR/XSPH,YSPH,ZSPH,RADSPH
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datspd.inc'
          INCLUDE 'datsp1.inc'
          IF(DF2.EQ.1) CAFORWARD=.TRUE.
          IF(DF2.EQ.0) CAFORWARD=.FALSE.
C
          JA=COS_A_ANG
          JB=COS_B_ANG
C
          GRASET=.FALSE.
C
          IMFLAG=.FALSE.
C
          XUP=-1.0D10
          YUP=-1.0D10
          XLO=1.0D10
          YLO=1.0D10
C
          IF(STI.EQ.1.AND.TPT.EQ.1) THEN
              OUTLYNE=
     1        '"SPD" IS THE SPOT DIAGRAM COMMAND'
              CALL SHOWIT(1)
              RETURN
          END IF
C
          IF(WC.NE.'ISPD'.AND.WC.NE.'ISPDA') THEN
              IF(WQ.NE.'ISTAT'.AND.WQ.NE.'IPSTAT'.AND.TPT.EQ.1) THEN
                  IF(SST.EQ.1) THEN
                      OUTLYNE=
     1                '"SPD (ACC) TAKES NO STRING INPUT'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
C       NO STRINGS PRESENT, PROCEED
                  END IF
                  IF(S4.EQ.1.OR.S5.EQ.1) THEN
                      OUTLYNE=
     1                '"SPD (ACC)" TAKES NO NUMERIC WORD #4 OR #5 INPUT'
                      CALL SHOWIT(1)
                      OUTLYNE=
     1                'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  IF(WC.EQ.'SPD') THEN
                      IF(DF3.EQ.1) THEN
                          DF3=0
                          S3=1
                          SPDSURF=NEWIMG
                      ELSE
C       DF3=0
                          IF(INT(W3).LT.NEWOBJ.OR.INT(W3).GT.NEWIMG) THEN
                              WRITE(OUTLYNE,*) 'SPD SURFACE BEYOND LEGAL BOUNDS, NW3'
                              CALL SHOWIT(1)
                              CALL MACFAL
                              RETURN
                          ELSE
                              SPDSURF=INT(W3)
                          END IF
                      END IF
                  END IF
                  IF(S1.EQ.1) THEN
                      IF(W1.NE.1.0D0.AND.W1.NE.2.0D0.AND.W1.NE.3.0D0.AND.W1.NE.4.0D0
     1                .AND.W1.NE.5.0D0.AND.W1.NE.6.0D0.AND.W1.NE.7.0D0.AND.W1.NE.8.0D0
     2                .AND.W1.NE.9.0D0.AND.W1.NE.10.0D0) THEN
                          OUTLYNE=
     1                    'INVALID WAVELENGTH NUMBER ISSUED WITH THE LAST "SPD" COMMAND'
                          CALL SHOWIT(1)
                          OUTLYNE=
     1                    'RE-ENTER COMMAND'
                          CALL SHOWIT(1)
                          CALL MACFAL
                          RETURN
                      END IF
                  END IF
              END IF
              IF(WQ.EQ.'ISTAT'.AND.TPT.EQ.1.OR.WQ.EQ.'IPSTAT'.AND.TPT.EQ.1) THEN
                  IF(S5.EQ.1.OR.SST.EQ.1) THEN
                      OUTLYNE=
     1                '"SPD (ISTAT OR IPSTAT) TAKES NO STRING OR NUMERIC WORD #5 INPUT'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
C       NO STRINGS PRESENT, PROCEED
                  END IF
C
C     CHECK FOR VALID RANGES AND DO DEFAULTS
C     NW1
                  IF(DF1.EQ.1) W1=SYSTEM1(20)
                  IF(INT(W1).LE.0.OR.W1.GT.SYSTEM1(20)) THEN
                      OUTLYNE=
     1                'SURFACE NUMBER BEYOND LEGAL BOUNDS'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  ELSE
                      LSTIMG=NEWIMG
                      IMFLAG=.TRUE.
                      NEWIMG=INT(W1)
                  END IF
C     NW2 AND NW3
                  IF(DF2.EQ.1) W2=0.0D0
                  IF(DF3.EQ.1) W3=90.0D0
                  IF(W2.GE.W3) THEN
                      OUTLYNE=
     1                'STARTING ANGLE MUST BE LESS THAN ENDING ANGLE'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  ELSE
                      STARANG=W2
                      ENDANG=W3
                  END IF
C     NW4
                  IF(DF4.EQ.1) W4=(DABS(W3)-DABS(W2))/10.0D0
                  IF(DABS(W4).GE.(DABS(W3)-DABS(W2))) THEN
                      OUTLYNE=
     1                'ANGLE INCREMENT TOO SMALL FOR SPECIFIED ANGLE RANGE'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  ELSE
C     NUMBER OF STEPS IS
                      NSTEP=IDNINT((DABS(W3)-DABS(W2))/W4)
                      IF(NSTEP.GT.100) THEN
                          OUTLYNE=
     1                    'THE MAXIMUM NUMBER OF STEPS IS 100'
                          CALL SHOWIT(1)
                          OUTLYNE=
     1                    'ANGLE INCREMENT YIELDS MORE THAN 100 STEPS'
                          CALL SHOWIT(1)
                          OUTLYNE='RE-ENTER COMMAND'
                          CALL SHOWIT(1)
                          CALL MACFAL
                          RETURN
                      END IF
                      DELANG=(DABS(W3)-DABS(W2))/DBLE(NSTEP)
                  END IF
              END IF

          END IF
C
          SPDEXT=.FALSE.
C
C     CHECK IF ALL SPECTRAL WEIGHTS ARE ZERO
C
          IF(SYSTEM1(31).LE.0.0D0.AND.SYSTEM1(32).LE.0.0D0
     1    .AND.SYSTEM1(33).LE.0.0D0.AND.SYSTEM1(34).LE.0.0D0
     2    .AND.SYSTEM1(35).LE.0.0D0
     1    .AND.SYSTEM1(76).LE.0.0D0.AND.SYSTEM1(77).LE.0.0D0
     1    .AND.SYSTEM1(78).LE.0.0D0.AND.SYSTEM1(79).LE.0.0D0
     1    .AND.SYSTEM1(80).LE.0.0D0) THEN
C
              IF(TPT.EQ.1) THEN
                  OUTLYNE='ALL SPECTRAL WEIGHTS ARE ZERO'
                  CALL SHOWIT(1)
                  OUTLYNE='NO SPOT DIAGRAM WAS CREATED'
                  CALL SHOWIT(1)
              END IF
              SPDEXT=.FALSE.
              CALL MACFAL
              RETURN
          ELSE
C
C       NOT ALL SPECTRAL WEIGHTS ZERO, CALCULATE NUMBER OF RAYS MAXIMUM
C       PER WAVELENGTH ASSUMING SPECTRAL WEIGHTS NOT ZERO
C
C      OPEN FOR INPUT
C
              SPT1=SYSTEM1(31)
              SPT2=SYSTEM1(32)
              SPT3=SYSTEM1(33)
              SPT4=SYSTEM1(34)
              SPT5=SYSTEM1(35)
              SPT6=SYSTEM1(76)
              SPT7=SYSTEM1(77)
              SPT8=SYSTEM1(78)
              SPT9=SYSTEM1(79)
              SPT10=SYSTEM1(80)
          END IF
C
          IF(TPT.EQ.1) THEN
              IF(SPDTYPE.EQ.1) THEN
                  IW=NRECT
                  IWIW=NRECT**2
              END IF
              IF(SPDTYPE.EQ.2) THEN
                  IW=0
                  IWIW=0
                  DO I=0,RINGTOT
                      IWIW=IWIW+RINGPNT(I)
                  END DO
              END IF
              IF(SPDTYPE.EQ.3) THEN
                  IW=0
                  IWIW=RNUMBR
              END IF
          END IF
          IF(TPT.EQ.2) THEN
              IF(OPSPDTYPE.EQ.1) THEN
                  IW=OPNRECT
                  IWIW=OPNRECT**2
              END IF
              IF(OPSPDTYPE.EQ.2) THEN
                  IW=0
                  IWIW=0
                  DO I=0,int(OPRINGTOT)
                      IWIW=IWIW+OPRINGPNT(I)
                  END DO
              END IF
              IF(OPSPDTYPE.EQ.3) THEN
                  IW=0
                  IWIW=int(OPRNUMBR)
              END IF
          END IF
          SPT1=SYSTEM1(31)
          SPT2=SYSTEM1(32)
          SPT3=SYSTEM1(33)
          SPT4=SYSTEM1(34)
          SPT5=SYSTEM1(35)
          SPT6=SYSTEM1(76)
          SPT7=SYSTEM1(77)
          SPT8=SYSTEM1(78)
          SPT9=SYSTEM1(79)
          SPT10=SYSTEM1(80)
          TEMPHOLDER=0
          IF(SPT1.NE.0.0D0) TEMPHOLDER=TEMPHOLDER+1
          IF(SPT2.NE.0.0D0) TEMPHOLDER=TEMPHOLDER+1
          IF(SPT3.NE.0.0D0) TEMPHOLDER=TEMPHOLDER+1
          IF(SPT4.NE.0.0D0) TEMPHOLDER=TEMPHOLDER+1
          IF(SPT5.NE.0.0D0) TEMPHOLDER=TEMPHOLDER+1
          IF(SPT6.NE.0.0D0) TEMPHOLDER=TEMPHOLDER+1
          IF(SPT7.NE.0.0D0) TEMPHOLDER=TEMPHOLDER+1
          IF(SPT8.NE.0.0D0) TEMPHOLDER=TEMPHOLDER+1
          IF(SPT9.NE.0.0D0) TEMPHOLDER=TEMPHOLDER+1
          IF(SPT10.NE.0.0D0) TEMPHOLDER=TEMPHOLDER+1
          IWIW2=IWIW*TEMPHOLDER
C     DEALLOCATE DSPOTT
          CALL SPOTIT(1)
C     ALLOCATE DSPOTT
          NDSPOTT=IWIW2
          CALL SPOTIT(2)
          LSF=.FALSE.
C
C       THE LAST FOB INPUT VALUES ARE STORED IN CHLFOB AND
C       THE ARRAY LFOB(1:7).
C
C       NOTE: DSPOT#(12) IS A RAW, UN-NORMALIZED
C       RAY ENERGY TERM BUT IS LATER (WHEN SPA IS CALC.) NORMALIZED
C       TO A FRACTIONAL ENERGY TERM BY DIVISION BY THE WEIGHTING
C       TERM TOT.
          IF(ITRACE) TCLPRF=.FALSE.
          IF(.NOT.ITRACE) THEN
C     DOES THE NEWREF SURFACE HAVE A CLAP ON IT ?
              TCLPRF=.FALSE.
              IF(TPT.EQ.1) THEN
C
                  IF(ALENS(9,NEWREF).EQ.0.0D0.OR.ALENS(127,NEWREF).NE.0.0D0) THEN
                      IF(SPDTYPE.EQ.1) THEN
C     RECT SPOT
C     ASSIGN A TEMPORARY CIRCULAR CLAP
                          ALENS(9,NEWREF)=1.0D0
                          IF(PXTRAY(1,NEWREF).GT.PXTRAX(1,NEWREF)) THEN
                              ALENS(10,NEWREF)=PXTRAY(1,NEWREF)
                              ALENS(11,NEWREF)=ALENS(10,NEWREF)
                          ELSE
                              ALENS(10,NEWREF)=PXTRAX(1,NEWREF)
                              ALENS(11,NEWREF)=ALENS(10,NEWREF)
                          END IF
                          ALENS(12,NEWREF)=0.0D0
                          ALENS(13,NEWREF)=0.0D0
                          ALENS(14,NEWREF)=0.0D0
                          ALENS(15,NEWREF)=0.0D0
                          TCLPRF=.TRUE.
C
                          IF(WQ.NE.'ACC') THEN
                              IF(F28.EQ.1.OR.F31.EQ.1) MSGSPD=.FALSE.
                              IF(MSGSPD)WRITE(OUTLYNE,301) PXTRAX(1,NEWREF),PXTRAY(1,NEWREF)
                              IF(MSGSPD) CALL SHOWIT(0)
                              IF(MSGSPD)WRITE(OUTLYNE,*)
     1                        '(X x Y), HAS BEEN ASSIGNED TO THE REFERENCE SURFACE'
                              IF(MSGSPD) CALL SHOWIT(0)
                              IF(MSGSPD)WRITE(OUTLYNE,*)
     1                        'FOR THE CURRENT SPOT DIAGRAM CALCULATIONS'
                              IF(MSGSPD) CALL SHOWIT(0)
 301                          FORMAT('TEMPORARY CIRCULAR CLEAR APERTURE = '
     1                         ,G11.4,' X ',G11.4)
                          END IF
C     SPDTYPE NOT 1 FOR RECT
                      END IF
                      IF(SPDTYPE.EQ.2) THEN
C     RING SPOT
C     ASSIGN A TEMPORARY CIRCULAR CLAP
                          ALENS(9,NEWREF)=1.0D0
                          IF(PXTRAY(1,NEWREF).GT.PXTRAX(1,NEWREF)) THEN
                              ALENS(10,NEWREF)=PXTRAY(1,NEWREF)
                              ALENS(11,NEWREF)=ALENS(10,NEWREF)
                          ELSE
                              ALENS(10,NEWREF)=PXTRAX(1,NEWREF)
                              ALENS(11,NEWREF)=ALENS(10,NEWREF)
                          END IF
                          ALENS(12,NEWREF)=0.0D0
                          ALENS(13,NEWREF)=0.0D0
                          ALENS(14,NEWREF)=0.0D0
                          ALENS(15,NEWREF)=0.0D0
                          TCLPRF=.TRUE.
C
                          IF(WQ.NE.'ACC') THEN
                              IF(F28.EQ.1.OR.F31.EQ.1) MSGSPD=.FALSE.
                              IF(MSGSPD)WRITE(OUTLYNE,3010) PXTRAX(1,NEWREF),PXTRAY(1,NEWREF)
                              IF(MSGSPD) CALL SHOWIT(0)
                              IF(MSGSPD)WRITE(OUTLYNE,*)
     1                        '(X x Y), HAS BEEN ASSIGNED TO THE REFERENCE SURFACE'
                              IF(MSGSPD) CALL SHOWIT(0)
                              IF(MSGSPD)WRITE(OUTLYNE,*)
     1                        'FOR THE CURRENT SPOT DIAGRAM CALCULATIONS'
                              IF(MSGSPD) CALL SHOWIT(0)
 3010                         FORMAT('TEMPORARY CIRCULAR CLEAR APERTURE = '
     1                         ,G11.4,' X ',G11.4)
                          END IF
C     SPDTYPE NOT 2 FOR RING
                      END IF
                      IF(SPDTYPE.EQ.3) THEN
C     RAND SPOT
C     ASSIGN A TEMPORARY CIRCULAR CLAP
                          ALENS(9,NEWREF)=1.0D0
                          IF(PXTRAY(1,NEWREF).GT.PXTRAX(1,NEWREF)) THEN
                              ALENS(10,NEWREF)=PXTRAY(1,NEWREF)
                              ALENS(11,NEWREF)=ALENS(10,NEWREF)
                          ELSE
                              ALENS(10,NEWREF)=PXTRAX(1,NEWREF)
                              ALENS(11,NEWREF)=ALENS(10,NEWREF)
                          END IF
                          ALENS(12,NEWREF)=0.0D0
                          ALENS(13,NEWREF)=0.0D0
                          ALENS(14,NEWREF)=0.0D0
                          ALENS(15,NEWREF)=0.0D0
                          TCLPRF=.TRUE.
C
                          IF(WQ.NE.'ACC') THEN
                              IF(F28.EQ.1.OR.F31.EQ.1) MSGSPD=.FALSE.
                              IF(MSGSPD)WRITE(OUTLYNE,301) PXTRAX(1,NEWREF),PXTRAY(1,NEWREF)
                              IF(MSGSPD) CALL SHOWIT(0)
                              IF(MSGSPD)WRITE(OUTLYNE,*)
     1                        '(X x Y), HAS BEEN ASSIGNED TO THE REFERENCE SURFACE'
                              IF(MSGSPD) CALL SHOWIT(0)
                              IF(MSGSPD)WRITE(OUTLYNE,*)
     1                        'FOR THE CURRENT SPOT DIAGRAM CALCULATIONS'
                              IF(MSGSPD) CALL SHOWIT(0)
                          END IF
C     SPDTYPE NOT 3 FOR RAND
                      END IF
C     THERE WAS A CLEAR APERTURE, USE ITS VALUES
                  END IF
              END IF
C
              IF(TPT.EQ.2) THEN
                  IF(ALENS(9,NEWREF).EQ.0.0D0.OR.ALENS(127,NEWREF).NE.0.0D0) THEN
                      IF(OPSPDTYPE.EQ.1) THEN
C     RECT SPOT
C     ASSIGN A TEMPORARY CIRCULAR CLAP
                          ALENS(9,NEWREF)=1.0D0
                          IF(PXTRAY(1,NEWREF).GT.PXTRAX(1,NEWREF)) THEN
                              ALENS(10,NEWREF)=PXTRAY(1,NEWREF)
                              ALENS(11,NEWREF)=ALENS(10,NEWREF)
                          ELSE
                              ALENS(10,NEWREF)=PXTRAX(1,NEWREF)
                              ALENS(11,NEWREF)=ALENS(10,NEWREF)
                          END IF
                          ALENS(12,NEWREF)=0.0D0
                          ALENS(13,NEWREF)=0.0D0
                          ALENS(14,NEWREF)=0.0D0
                          ALENS(15,NEWREF)=0.0D0
                          TCLPRF=.TRUE.
C     OPSPDTYPE NOT 1 FOR RECT
                      END IF
                      IF(OPSPDTYPE.EQ.2) THEN
C     RING SPOT
C     ASSIGN A TEMPORARY CIRCULAR CLAP
                          ALENS(9,NEWREF)=1.0D0
                          IF(PXTRAY(1,NEWREF).GT.PXTRAX(1,NEWREF)) THEN
                              ALENS(10,NEWREF)=PXTRAY(1,NEWREF)
                              ALENS(11,NEWREF)=ALENS(10,NEWREF)
                          ELSE
                              ALENS(10,NEWREF)=PXTRAX(1,NEWREF)
                              ALENS(11,NEWREF)=ALENS(10,NEWREF)
                          END IF
                          ALENS(12,NEWREF)=0.0D0
                          ALENS(13,NEWREF)=0.0D0
                          ALENS(14,NEWREF)=0.0D0
                          ALENS(15,NEWREF)=0.0D0
                          TCLPRF=.TRUE.
C     OPSPDTYPE NOT 2 FOR RING
                      END IF
                      IF(OPSPDTYPE.EQ.3) THEN
C     RAND SPOT
C     ASSIGN A TEMPORARY CIRCULAR CLAP
                          ALENS(9,NEWREF)=1.0D0
                          IF(PXTRAY(1,NEWREF).GT.PXTRAX(1,NEWREF)) THEN
                              ALENS(10,NEWREF)=PXTRAY(1,NEWREF)
                              ALENS(11,NEWREF)=ALENS(10,NEWREF)
                          ELSE
                              ALENS(10,NEWREF)=PXTRAX(1,NEWREF)
                              ALENS(11,NEWREF)=ALENS(10,NEWREF)
                          END IF
                          ALENS(12,NEWREF)=0.0D0
                          ALENS(13,NEWREF)=0.0D0
                          ALENS(14,NEWREF)=0.0D0
                          ALENS(15,NEWREF)=0.0D0
                          TCLPRF=.TRUE.
C     OPSPDTYPE NOT 3 FOR RAND
                      END IF
C     THERE WAS A CLEAR APERTURE, USE ITS VALUES
                  END IF
              END IF
C
C     DECIDE HOW TO HANDLE ODD VERSUS EVEN GRIDS AS FAR AS RAY
C     SPACINGS ARE CONCERNED (ONLY FOR RECT TYPE SPOT)
C
              IF(TPT.EQ.1) THEN
                  IF(SPDTYPE.EQ.1) THEN
C     RECT
C       IS IW ODD OR EVEN
                      FRACA=DABS(DBLE(IW)/2.0D0)
                      FRACB=DABS(DBLE(INT(DBLE(IW)/2.0D0)))
                      FRAC=FRACA-FRACB
                      IF(FRAC.GT.0.0D0) THEN
                          ODD=.TRUE.
                      ELSE
                          ODD=.FALSE.
                      END IF
C     RING OR RANNUM
                  END IF
              END IF
              IF(TPT.EQ.2) THEN
                  IF(OPSPDTYPE.EQ.1) THEN
C     RECT
C       IS IW ODD OR EVEN
                      FRACA=DABS(DBLE(IW)/2.0D0)
                      FRACB=DABS(DBLE(INT(DBLE(IW)/2.0D0)))
                      FRAC=FRACA-FRACB
                      IF(FRAC.GT.0.0D0) THEN
                          ODD=.TRUE.
                      ELSE
                          ODD=.FALSE.
                      END IF
C     RING OR RANNUM
                  END IF
              END IF
          ELSE
C     NOT ITRACE
          END IF
C
C       IWL COUNTS THROUGH THE 10 WAVELENGTH NUMBERS
          I=1
          DO IWL=1,10
              IF(IWL.GE.1.AND.IWL.LE.5) SPT=SYSTEM1(30+IWL)
              IF(IWL.GE.6.AND.IWL.LE.10) SPT=SYSTEM1(75+IWL-5)
              IF(SPT.GT.0.0D0) THEN
C     ONLY TRACE RAYS FOR NON-ZERO SPECTRAL WEIGHTS
C       TRACE RAYS AT THAT WAVELENGTH
C
                  IF(TPT.EQ.1) THEN
                      IF(SPDTYPE.EQ.1) THEN
C     RECT
                          IF(ODD) THEN
                              NSTART1 = -DBLE(NRECT)/2.0D0
                              NDEL1   = DBLE(NRECT)/(DBLE(NRECT)-1.0D0)
                              NSTOP1  =  (DBLE(NRECT)/2.0D0)+(0.0001D0*NDEL1)
                              NSTART2 = -DBLE(NRECT)/2.0D0
                              NDEL2   = DBLE(NRECT)/(DBLE(NRECT)-1.0D0)
                              NSTOP2  =  (DBLE(NRECT)/2.0D0)+(0.0001D0*NDEL2)
                          ELSE
C       NRECT IS EVEN
                              NSTART1 = -DBLE(NRECT)/2.0D0
                              NDEL1   =DBLE(NRECT)/(DBLE(NRECT)-1.0D0)
                              NSTOP1  =  (DBLE(NRECT)/2.0D0)+(0.0001D0*NDEL1)
                              NSTART2 = -DBLE(NRECT)/2.0D0
                              NDEL2   =DBLE(NRECT)/(DBLE(NRECT)-1.0D0)
                              NSTOP2  =  (DBLE(NRECT)/2.0D0)+(0.0001D0*NDEL2)
                          END IF
C     NOT RECT
                      END IF
                      IF(SPDTYPE.EQ.3) THEN
C     RANNUM
                          NSTART1 = 1.0D0
                          NDEL1   = 1.0D0
                          NSTOP1  = DBLE(RNUMBR)+0.0001D0
                          NSTART2 = 1.0D0
                          NDEL2   = 1.0D0
                          NSTOP2  = 1.0001D0
C     NOT RANNUM
                      END IF
                      IF(SPDTYPE.EQ.2) THEN
C     RING STRUCTURE
C     TOTAL NUMBER OF RINGS IS RINGTOT WHICH MAY BE MAX = 50
C     ARRAY RINGRAD(I) CONTAINS DAT FOR RINGS RADIUS
C     I COUNTS THE RING
C     ARRAY RINGPNT(I) HAS DATA FOR POINTS ON RING I
C     ARRAY RINGANG(I) HAS DATA FOR ANGLE OFFSET ON RING I
C     RINGRAD(1:50),RINGPNT(1:50),RINGANG(1:50),RINGTOT
                      END IF
                  END IF
                  IF(TPT.EQ.2) THEN
                      IF(OPSPDTYPE.EQ.1) THEN
C     RECT
                          IF(ODD) THEN
                              NSTART1 = -DBLE(OPNRECT)/2.0D0
                              NDEL1   = DBLE(OPNRECT)/(DBLE(OPNRECT)-1.0D0)
                              NSTOP1  =  (DBLE(OPNRECT)/2.0D0)+(0.0001D0*NDEL1)
                              NSTART2 = -DBLE(OPNRECT)/2.0D0
                              NDEL2   = DBLE(OPNRECT)/(DBLE(OPNRECT)-1.0D0)
                              NSTOP2  =  (DBLE(OPNRECT)/2.0D0)+(0.0001D0*NDEL2)
                          ELSE
C       NRECT IS EVEN
                              NSTART1 = -DBLE(OPNRECT)/2.0D0
                              NDEL1   =DBLE(OPNRECT)/(DBLE(OPNRECT)-1.0D0)
                              NSTOP1  =  (DBLE(OPNRECT)/2.0D0)+(0.0001D0*NDEL1)
                              NSTART2 = -DBLE(OPNRECT)/2.0D0
                              NDEL2   =DBLE(OPNRECT)/(DBLE(OPNRECT)-1.0D0)
                              NSTOP2  =  (DBLE(OPNRECT)/2.0D0)+(0.0001D0*NDEL2)
                          END IF
C     NOT RECT
                      END IF
                      IF(OPSPDTYPE.EQ.3) THEN
C     RANNUM
                          NSTART1 = 1.0D0
                          NDEL1   = 1.0D0
                          NSTOP1  = DBLE(OPRNUMBR)+0.0001D0
                          NSTART2 = 1.0D0
                          NDEL2   = 1.0D0
                          NSTOP2  = 1.0001D0
C     NOT RANNUM
                      END IF
                      IF(OPSPDTYPE.EQ.2) THEN
C     RING STRUCTURE
C     TOTAL NUMBER OF RINGS IS RINGTOT WHICH MAY BE MAX = 50
C     ARRAY OPRINGRAD(I) CONTAINS DAT FOR RINGS RADIUS
C     I COUNTS THE RING
C     ARRAY OPRINGPNT(I) HAS DATA FOR POINTS ON RING I
C     ARRAY OPRINGANG(I) HAS DATA FOR ANGLE OFFSET ON RING I
C     OPRINGRAD(1:50),OPRINGPNT(1:50),OPRINGANG(1:50),OPRINGTOT
                      END IF
                  END IF
                  IF(SPT.NE.0.0D0) THEN
                      IF(WQ.NE.'ACC'.AND.TPT.EQ.1) THEN
                          IF(F28.EQ.0.AND.F31.EQ.0) WRITE(OUTLYNE,200)IWIW,IWL
                          IF(F28.EQ.0.AND.F31.EQ.0) CALL SHOWIT(0)
                      END IF
                  END IF
 200              FORMAT('TRACING ',I10,' RAYS AT WAVELENGTH ',I1)
C
                  IF(SPDTYPE.NE.2.AND.TPT.EQ.1.OR.OPSPDTYPE.NE.2.AND.TPT.EQ.2) THEN
C     SPOT IS RECT OR RANNUM
C
                      DO IY=NSTART1,NSTOP1,NDEL1
                          DO IX=NSTART2,NSTOP2,NDEL2
                              I=I+1
C       THE CALL TO RAYTRA2 HAS INPUTS:
C               QUALIFIER
                              IF(CAFORWARD) WWQ='CAOB'
                              IF(.NOT.CAFORWARD) WWQ='        '
                              IF(SPDTYPE.EQ.1.AND.TPT.EQ.1.OR.OPSPDTYPE.EQ.1.AND.TPT.EQ.2) THEN
C     RECT
C       WW1 IS Y THE RELATIVE APERTURE HT OF THE RAY
C               WW1
                                  IF(TPT.EQ.1) WW1=((2.0D0/DBLE(NRECT))*((IY)))
                                  IF(TPT.EQ.2) WW1=((2.0D0/DBLE(OPNRECT))*((IY)))
C       WW2 IS X THE RELATIVE APERTURE HT OF THE RAY
C               WW2
                                  IF(TPT.EQ.1) WW2=((2.0D0/DBLE(NRECT))*((IX)))
                                  IF(TPT.EQ.2) WW2=((2.0D0/DBLE(OPNRECT))*((IX)))
                              END IF
                              IF(SPDTYPE.EQ.3.AND.TPT.EQ.1.OR.OPSPDTYPE.EQ.3.AND.TPT.EQ.2) THEN
C     RANDOM SPOT
C
C       NOW CALCULATE PSEUDO-RANDOM NUMBER
C
C       WW1 IS Y THE RELATIVE APERTURE HT OF THE RAY
                                  IF(.NOT.ITRACE) CALL RANDGET(RESLT)
                                  IF(.NOT.ITRACE) RANDX=(2.0D0*DBLE(RESLT))-1.0D0
                                  IF(ITRACE) CALL RANDGET(RESLT)
                                  IF(ITRACE) RANDX=DBLE(RESLT)
C               WW1
                                  WW1=RANDX
C       WW2 IS X THE RELATIVE APERTURE HT OF THE RAY
                                  IF(.NOT.ITRACE) CALL RANDGET(RESLT)
                                  IF(.NOT.ITRACE) RANDX=(2.0D0*DBLE(RESLT))-1.0D0
                                  IF(ITRACE) CALL RANDGET(RESLT)
                                  IF(ITRACE) RANDX=DBLE(RESLT)
C               WW2
                                  WW2=RANDX
                              END IF
C               WW3
                              WW3=IWL
                              WVN=IWL
                              IF(ITRACE) THEN
C     CONVERT WW1 AND WW2 TO EL AND AZ ANGLES
C     RANDOM EL FROM 0 TO ELMAX
C     RANDOM AZ FROM 0 TO 360
                                  WW1=WW1*ELMAX
                                  WW2=WW2*360.0D0
                              END IF
C               CACOCH IS SET TO 1 FOR SPOT DIAGRAMS
                              IF(CAFORWARD) CACOCH = 1
                              IF(.NOT.CAFORWARD) CACOCH = 0
                              SPDTRA=.TRUE.
                              MSG = .FALSE.
                              STOPP=0
C
C THE NEWOBJ,NEWREF AND NEWIMG ARE SET BY FOB
C       TRACE RAY AND RETURN
                              DSPOT(1:60)=0.0D0
                              DSPOT(40)=IX
                              DSPOT(41)=NDEL1
                              DSPOT(42)=IY
                              DSPOT(43)=NDEL2
                              WW4=1.0D0
                              NOCOAT=.FALSE.
                              GRASET=.FALSE.
                              DXFSET=.FALSE.
                              CALL RAYTRA2
                              SPDCD1=RAYCOD(1)
                              SPDCD2=RAYCOD(2)
                              IF(.NOT.CAFORWARD) CALL REVERSECA
                              DSPOT(36)=0.0D0
                              IF(REFMISS) THEN
                                  DSPOT(36)=1.0D0
                              END IF
                              CACOCH=0
                              DSPOT(7)=DBLE(SPDCD1)
                              DSPOT(8)=DBLE(SPDCD2)
                              IF(DSPOT(7).NE.0.0D0) THEN
                                  GO TO 1941
                              END IF
                              SPDTRA=.FALSE.
                              DSPOT(1)=RAYRAY(1,NEWIMG)
                              DSPOT(2)=RAYRAY(2,NEWIMG)
                              DSPOT(3)=RAYRAY(3,NEWIMG)
                              DSPOT(45)=RAYRAY(1,SPDSURF)
                              DSPOT(46)=RAYRAY(2,SPDSURF)
                              DSPOT(47)=RAYRAY(3,SPDSURF)
                              DSPOT(48)=RAYRAY(4,SPDSURF)
                              DSPOT(49)=RAYRAY(5,SPDSURF)
                              DSPOT(50)=RAYRAY(6,SPDSURF)
                              DSPOT(51)=RAYRAY(9,SPDSURF)
C     RAY SLOPES
                              X=RAYRAY(11,NEWIMG)
                              Y=RAYRAY(12,NEWIMG)
                              CALL SLOPES
                              DSPOT(9)=X
                              DSPOT(10)=Y
C
                              DSPOT(11)=RAYRAY(25,NEWIMG)
C
                              IF(APODGAUSS) THEN
                                  APODX2=-DLOG(10.0D0**(-DABS(APODDBLOSS)/10.0D0))
                                  APODR2=(WW1**2)+(WW2**2)
                                  DSPOT(11)=DSPOT(11)*DEXP(-APODX2*APODR2)
                              END IF
C       NOW CALCULATE THE RAY ENERGY TERM
C     THIS IS SPECTRAL WEIGHT TIMES APODIZATION FACTOR FOR THIS RAY
C
                              DSPOT(12)=(SPT*DSPOT(11))
                              IF(DSPOT(7).NE.0.0D0) DSPOT(12)=0.0D0
                              DSPOT(37)=DSPOT(12)*RAYRAY(9,NEWIMG)
                              DSPOT(38)=REFRY(9,NEWIMG)
                              DSPOT(39)=RAYRAY(9,NEWIMG)
                              DSPOT(44)=RAYRAY(9,NEWREF)
                              IF(DSPOT(12).NE.0.0D0) THEN
                                  IF(SYSTEM1(30).LE.2.0D0) THEN
                                      IF(DSPOT(1).GT.XUP) XUP=DSPOT(1)
                                      IF(DSPOT(2).GT.YUP) YUP=DSPOT(2)
                                      IF(DSPOT(1).LT.XLO) XLO=DSPOT(1)
                                      IF(DSPOT(2).LT.YLO) YLO=DSPOT(2)
                                  ELSE
                                      IF(DSPOT(9).GT.XUP) XUP=DSPOT(9)
                                      IF(DSPOT(10).GT.YUP) YUP=DSPOT(10)
                                      IF(DSPOT(9).LT.XLO) XLO=DSPOT(9)
                                      IF(DSPOT(10).LT.YLO) YLO=DSPOT(10)
                                  END IF
                              END IF
C       KEEP TRACK OF THE TOTAL OPL ALONE THE RAY FROM NEWOBJ TO NEWI
                              DSPOT(13)=RAYRAY(22,NEWIMG)
C               J=14 X RAY COORD AT TANGENT PLANE OF NEWOBJ+1
                              DSPOT(14)=RAYRAY(1,NEWOBJ+1)
C               J=15 Y RAY COORD AT TANGENT PLANE OF NEWOBJ+1
                              DSPOT(15)=RAYRAY(2,NEWOBJ+1)
C               J=5 X COORD OF RAY AT NEWREF
                              DSPOT(5)=RAYRAY(1,NEWREF)
C               J=6 Y COORD OF RAY AT NEWREF
                              DSPOT(6)=RAYRAY(2,NEWREF)
C
C
                              OOPD=0.0D0
                              DSPOT(4)=OOPD
                              DSPOT(16)=DBLE(IWL)
                              IF(IWL.GE.1.AND.IWL.LE.5)
     1                        DSPOT(17)=SYSTEM1(30+IWL)
                              IF(IWL.GE.6.AND.IWL.LE.10)
     1                        DSPOT(17)=SYSTEM1(75+IWL-5)
                              DSPOT(18)=RAYRAY(3,NEWOBJ+1)
                              DSPOT(19)=RAYRAY(19,NEWOBJ+1)
                              DSPOT(20)=RAYRAY(20,NEWOBJ+1)
                              DSPOT(21)=RAYRAY(21,NEWOBJ+1)
                              DSPOT(22)=RAYRAY(19,NEWIMG)
                              DSPOT(23)=RAYRAY(20,NEWIMG)
                              DSPOT(24)=RAYRAY(21,NEWIMG)
                              DSPOT(25)=RAYRAY(9,NEWIMG)
                              DSPOT(26)=RAYRAY(10,NEWIMG)
                              DSPOT(27)=RAYRAY(1,NEWIMG-1)
                              DSPOT(28)=RAYRAY(2,NEWIMG-1)
                              DSPOT(29)=RAYRAY(3,NEWIMG-1)
                              DSPOT(30)=RAYRAY(19,NEWIMG-1)
                              DSPOT(31)=RAYRAY(20,NEWIMG-1)
                              DSPOT(32)=RAYRAY(21,NEWIMG-1)
C
 1941                         CONTINUE
C     LOAD THE DSOPT ARRAY INTO DSPOTT(ID,*)
                              ID=I-1
                              CALL SPOTIT(3)
                          END DO
                      END DO
                  ELSE
C     SPOT WAS RING
C     DO RING TRACE HERE
C     RINGRAD WAS INPUT AS A FRACTIONAL REFERENCE SURFACE COORDINATE
C     AND IS STORED AS SUCH. IF THE REF AP HTS ARE DIFFERENT,
C     RAY AIMING WILL BE IN AN ELLIPTICAL RATHER THAN A CIRCULAR RING
C     BECAUSE OF THE FRACTIONAL RAY AIMING IN RAYTRA2.FOR
                      IF(TPT.EQ.1) RINGTOT1=RINGTOT
                      IF(TPT.EQ.2) RINGTOT1=OPRINGTOT
                      DO JJ=0,INT(RINGTOT1)
C     THIS LOOPS FOR THE RINGS
                          IF(JJ.EQ.0) RRAD=0.0D0
                          IF(JJ.EQ.0) RPNT=1
                          IF(JJ.NE.0.AND.TPT.EQ.1) RRAD=RINGRAD(JJ)
                          IF(JJ.NE.0.AND.TPT.EQ.2) RRAD=OPRINGRAD(JJ)
                          IF(JJ.NE.0.AND.TPT.EQ.1) RPNT=RINGPNT(JJ)
                          IF(JJ.NE.0.AND.TPT.EQ.2) RPNT=OPRINGPNT(JJ)
C     THIS LOOPS THROUGH THE POINTS IN EACH RING
                          DO K=1,INT(RPNT)
                              IF(JJ.EQ.0) RRANG=0.0D0
                              IF(JJ.NE.0) THEN
                                  IF(TPT.EQ.1) RRANG=((TWOPII)/360.0D0)*RINGANG(JJ)
     1                            +(DBLE(K-1)*((TWOPII)/DBLE(RINGPNT(JJ))))
                                  IF(TPT.EQ.2) RRANG=((TWOPII)/360.0D0)*OPRINGANG(JJ)
     1                            +(DBLE(K-1)*((TWOPII)/DBLE(OPRINGPNT(JJ))))
                              END IF
                              WW1=RRAD*DSIN(RRANG)
                              WW2=RRAD*DCOS(RRANG)
                              I=I+1
C
C       THE CALL TO RAYTRA2 HAS INPUTS:
C               QUALIFIER
                              IF(CAFORWARD) WWQ='CAOB'
                              IF(.NOT.CAFORWARD) WWQ='        '
C               WW3
                              WW3=IWL
                              WVN=IWL
C               CACOCH IS SET TO 1 FOR SPOT DIAGRAMS
                              IF(CAFORWARD) CACOCH = 1
                              IF(.NOT.CAFORWARD) CACOCH = 0
                              SPDTRA=.TRUE.
                              MSG = .FALSE.
                              STOPP=0
C
C THE NEWOBJ,NEWREF AND NEWIMG ARE SET BY FOB
C       TRACE RAY AND RETURN
                              DSPOT(1:60)=0.0D0
                              WW4=1.0D0
                              NOCOAT=.FALSE.
                              GRASET=.FALSE.
                              DXFSET=.FALSE.
                              CALL RAYTRA2
                              SPDCD1=RAYCOD(1)
                              SPDCD2=RAYCOD(2)
                              IF(.NOT.CAFORWARD) CALL REVERSECA
                              CACOCH=0
                              DSPOT(7)=DBLE(SPDCD1)
                              DSPOT(8)=DBLE(SPDCD2)
                              IF(DSPOT(7).NE.0.0D0) GO TO 1942
                              SPDTRA=.FALSE.
                              DSPOT(1)=RAYRAY(1,NEWIMG)
                              DSPOT(2)=RAYRAY(2,NEWIMG)
                              DSPOT(3)=RAYRAY(3,NEWIMG)
                              DSPOT(45)=RAYRAY(1,SPDSURF)
                              DSPOT(46)=RAYRAY(2,SPDSURF)
                              DSPOT(47)=RAYRAY(3,SPDSURF)
                              DSPOT(48)=RAYRAY(4,SPDSURF)
                              DSPOT(49)=RAYRAY(5,SPDSURF)
                              DSPOT(50)=RAYRAY(6,SPDSURF)
                              DSPOT(51)=RAYRAY(9,SPDSURF)
C     RAY SLOPES
                              X=RAYRAY(11,NEWIMG)
                              Y=RAYRAY(12,NEWIMG)
                              CALL SLOPES
                              DSPOT(9)=X
                              DSPOT(10)=Y
C
                              DSPOT(11)=RAYRAY(25,NEWIMG)
C
                              IF(APODGAUSS) THEN
                                  APODX2=-DLOG(10.0D0**(-DABS(APODDBLOSS)/10.0D0))
                                  APODR2=(WW1**2)+(WW2**2)
                                  DSPOT(11)=DSPOT(11)*DEXP(-APODX2*APODR2)
                              END IF
C       NOW CALCULATE THE RAY ENERGY TERM
C     THIS IS SPECTRAL WEIGHT TIMES APODIZATION FACTOR FOR THIS RAY
C
                              DSPOT(12)=(SPT*DSPOT(11))
                              IF(DSPOT(7).NE.0.0D0) DSPOT(12)=0.0D0
                              DSPOT(37)=DSPOT(12)*RAYRAY(9,NEWIMG)
                              DSPOT(38)=REFRY(9,NEWIMG)
                              DSPOT(39)=RAYRAY(9,NEWIMG)
                              DSPOT(44)=RAYRAY(9,NEWREF)
                              IF(DSPOT(12).NE.0.0D0) THEN
                                  IF(SYSTEM1(30).LE.2.0D0) THEN
                                      IF(DSPOT(1).GT.XUP) XUP=DSPOT(1)
                                      IF(DSPOT(2).GT.YUP) YUP=DSPOT(2)
                                      IF(DSPOT(1).LT.XLO) XLO=DSPOT(1)
                                      IF(DSPOT(2).LT.YLO) YLO=DSPOT(2)
                                  ELSE
                                      IF(DSPOT(9).GT.XUP) XUP=DSPOT(9)
                                      IF(DSPOT(10).GT.YUP) YUP=DSPOT(10)
                                      IF(DSPOT(9).LT.XLO) XLO=DSPOT(9)
                                      IF(DSPOT(10).LT.YLO) YLO=DSPOT(10)
                                  END IF
                              END IF
C       KEEP TRACK OF THE TOTAL OPL ALONE THE RAY FROM NEWOBJ TO NEWI
                              DSPOT(13)=RAYRAY(22,NEWIMG)
C               J=14 X RAY COORD AT TANGENT PLANE OF NEWOBJ+1
                              DSPOT(14)=RAYRAY(1,NEWOBJ+1)
C               J=15 Y RAY COORD AT TANGENT PLANE OF NEWOBJ+1
                              DSPOT(15)=RAYRAY(2,NEWOBJ+1)
C               J=5 X COORD OF RAY AT NEWREF
                              DSPOT(5)=RAYRAY(1,NEWREF)
C               J=6 Y COORD OF RAY AT NEWREF
                              DSPOT(6)=RAYRAY(2,NEWREF)
C
                              OOPD=0.0D0
                              DSPOT(4)=OOPD
                              DSPOT(16)=DBLE(IWL)
                              IF(IWL.GE.1.AND.IWL.LE.5)
     1                        DSPOT(17)=SYSTEM1(30+IWL)
                              IF(IWL.GE.6.AND.IWL.LE.10)
     1                        DSPOT(17)=SYSTEM1(75+IWL-5)
                              DSPOT(18)=RAYRAY(3,NEWOBJ+1)
                              DSPOT(19)=RAYRAY(19,NEWOBJ+1)
                              DSPOT(20)=RAYRAY(20,NEWOBJ+1)
                              DSPOT(21)=RAYRAY(21,NEWOBJ+1)
                              DSPOT(22)=RAYRAY(19,NEWIMG)
                              DSPOT(23)=RAYRAY(20,NEWIMG)
                              DSPOT(24)=RAYRAY(21,NEWIMG)
                              DSPOT(25)=RAYRAY(9,NEWIMG)
                              DSPOT(26)=RAYRAY(10,NEWIMG)
                              DSPOT(27)=RAYRAY(1,NEWIMG-1)
                              DSPOT(28)=RAYRAY(2,NEWIMG-1)
                              DSPOT(29)=RAYRAY(3,NEWIMG-1)
                              DSPOT(30)=RAYRAY(19,NEWIMG-1)
                              DSPOT(31)=RAYRAY(20,NEWIMG-1)
                              DSPOT(32)=RAYRAY(21,NEWIMG-1)
C
C     GET RID OF REDICULOUSLY SMALL VALUES
 1942                         CONTINUE
C     LOAD THE DSOPT ARRAY INTO DSPOTT(ID,*)
                              ID=I-1
                              CALL SPOTIT(3)
                          END DO
                      END DO
                  END IF
              ELSE
C       SPECTRAL WEIGHT WAS ZERO, GO TO NEXT WAVELENGTH
C     NO FILE OUTPUT WAS DONE
              END IF
          END DO
C     WRITE THE TOTAL # OF RECORDS IN RECORD #1
          ITOT=I
C
C     ALL THE RAYS HAVE BEEN TRACED, NOW DO THE STATISTICS AND
C     OUTPUT.
          IF(WQ.NE.'ACC'.AND.TPT.EQ.1) THEN
              IF(F28.EQ.0.AND.F31.EQ.0) THEN
                  IF(MSGSPD)WRITE(OUTLYNE,*)'SPOT DIAGRAM RAY TRACING COMPLETED'
                  IF(MSGSPD) CALL SHOWIT(0)
              END IF
          END IF
          IF(TCLPRF) THEN
C     REMOVE TEMP CLAP ON NEWREF
              TCLPRF=.FALSE.
              ALENS(9:15,NEWREF)=0.0D0
              IF(WQ.NE.'ACC'.AND.TPT.EQ.1) THEN
                  IF(F28.EQ.1.OR.F31.EQ.1) MSGSPD=.FALSE.
                  IF(MSGSPD)WRITE(OUTLYNE,301) PXTRAX(1,NEWREF),PXTRAY(1,NEWREF)
                  IF(MSGSPD) CALL SHOWIT(0)
                  IF(MSGSPD)WRITE(OUTLYNE,*)
     1            'HAS BEEN REMOVED FROM THE REFERENCE SURFACE'
                  IF(MSGSPD) CALL SHOWIT(0)
                  IF(MSGSPD)WRITE(OUTLYNE,*)
     1            'FOR THE CURRENT SPOT DIAGRAM CALCULATIONS'
                  IF(MSGSPD) CALL SHOWIT(0)
              END IF
          END IF
          IF(WQ.NE.'ACC'.AND.TPT.EQ.1) THEN
              IF(MSGSPD)WRITE(OUTLYNE,*)
     1        'CALCULATING SPOT DIAGRAM STATISTICS...'
              IF(MSGSPD) CALL SHOWIT(0)
          END IF
C
C     NOW DO THE CENTX AND CENTY CALCULATIONS
C
          SPA=0.0D0
          SPB=0.0D0
          SPC=0.0D0
          SPD=0.0D0
          AFSPB=0.0D0
          AFSPD=0.0D0
C       W IS THE NORMALIZING FACTOR FOR THE SPOT CALCULATIONS
C       TOT IS THE WEIGTHED TOTAL
C       NUMTOT IS THE RAW TOTAL OF NON-FAILED RAYS
          TOT=0.0D0
          RTOT=0.0D0
          IRTOT=0.0D0
          INTTOT=0.0D0
          INTOT2=0.0D0
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
          DO I=1,ITOT-1
C     LOAD DSPOTT(I,*) INTO DSPOT(*)
              ID=I
              CALL SPOTIT(4)
              SPT=DSPOT(17)
              APFAC=DSPOT(11)
C     SPA IS A WEIGHTED SUMMATION OF THE X COORDINATE OF A RAY
C     IN THE SPOT DIAGRAM, WEIGHTED BY THE SPECTRAL WEIGHT
C     AND THE APODIZATION FACTOR
C     SPC IS FOR THE Y COMPONENT
C     SPB IS THE WEIGHTED X SLOPE (IN XZ PLANE)
C     SPD IS THE WEIGHTED Y SLOPE (IN YZ PLANE)
C     AFSPB IS THE WEIGHTED X SLOPE ANGLE (IN XZ PLANE)
C     AFSPD IS THE WEIGHTED Y SLOPE ANGLE (IN YZ PLANE)
C
              IF(SPT.NE.0.0D0) THEN
                  IF(DSPOT(7).EQ.0.0D0) THEN
                      APFAC=DSPOT(11)
                      SPA=SPA+(SPT*DSPOT(1)*APFAC)
                      SPC=SPC+(SPT*DSPOT(2)*APFAC)
                      SPB=SPB+(SPT*DTAN(DSPOT(9))*APFAC)
                      SPD=SPD+(SPT*DTAN(DSPOT(10))*APFAC)
                      AFSPB=AFSPB+(SPT*(DSPOT(9))*APFAC)
                      AFSPD=AFSPD+(SPT*(DSPOT(10))*APFAC)
C     RAY FAILED
                  END IF
C     SPT = 0
              END IF
              IF(DSPOT(12).NE.0.0D0) THEN
                  IF(DSPOT(7).EQ.0.0D0) THEN
                      TOT=TOT+(DSPOT(12))
                      NUMTOT=NUMTOT+1
                      IRTOT=IRTOT+1.0D0
                      INTTOT=INTTOT+DSPOT(12)
                      INTOT2=INTOT2+DSPOT(12)
                      RTOT=DBLE(NUMTOT)
                      IF(DSPOT(16).EQ.1.0D0) NUMT1=NUMT1+1
                      IF(DSPOT(16).EQ.2.0D0) NUMT2=NUMT2+1
                      IF(DSPOT(16).EQ.3.0D0) NUMT3=NUMT3+1
                      IF(DSPOT(16).EQ.4.0D0) NUMT4=NUMT4+1
                      IF(DSPOT(16).EQ.5.0D0) NUMT5=NUMT5+1
                      IF(DSPOT(16).EQ.6.0D0) NUMT6=NUMT6+1
                      IF(DSPOT(16).EQ.7.0D0) NUMT7=NUMT7+1
                      IF(DSPOT(16).EQ.8.0D0) NUMT8=NUMT8+1
                      IF(DSPOT(16).EQ.9.0D0) NUMT9=NUMT9+1
                      IF(DSPOT(16).EQ.10.0D0) NUMT10=NUMT10+1
C       RAY FAILED, DONT DO STATS
                  END IF
C       WEIGHT 0
              END IF
          END DO
          IF(NUMTOT.EQ.0.0D0) THEN
              IF(WQ.NE.'ACC') THEN
                  OUTLYNE='NO RAYS COULD GET THROUGH THE SYSTEM'
                  CALL SHOWIT(1)
                  OUTLYNE='NO SPOT DIAGRAM WAS CREATED'
                  CALL SHOWIT(1)
C     DEALLOCATE DSPOTT
                  CALL SPOTIT(1)
              END IF
              SPDEXT=.FALSE.
              CALL MACFAL
              RETURN
          END IF
          W=TOT
          SPA=SPA/W
          SPC=SPC/W
          SPB=SPB/W
          SPD=SPD/W
          AFSPB=AFSPB/W
          AFSPD=AFSPD/W
          OSPA=SPA
          OSPC=SPC
          OSPB=SPB
          OSPD=SPD
          OAFSPB=AFSPB
          OAFSPD=AFSPD
C     NOW SPA,SPB,SPC,SPD,AFSPB AND AFSPD ARE NORMALIZED
C
C     CALCULATE CENTROID LOCATIONS
          IF(SYSTEM1(30).EQ.1.0D0.OR.SYSTEM1(30).EQ.2.0D0) THEN
C     FOCAL SYSTEMS
              CENTX=SPA
              CENTY=SPC
          ELSE
C     AFOCAL SYSTEMS
              CENTX=AFSPB
              CENTY=AFSPD
          END IF
C     GET RID OF REDICULOIUSLY SMALL VALUES
          IF(DABS(CENTX).LT.1.0D-10) CENTX=0.0D0
          IF(DABS(CENTY).LT.1.0D-10) CENTY=0.0D0
C
C     REDEFINE SP VARS FOR CHIEF RAY
          SPA=REFRY(1,NEWIMG)
          SPC=REFRY(2,NEWIMG)
C     RAY SLOPES
          X=REFRY(11,NEWIMG)
          Y=REFRY(12,NEWIMG)
          CALL SLOPES
          AFSPB=X
          AFSPD=Y
          SPB=DTAN(X)
          SPD=DTAN(Y)
C
C       TOT IS THE SUM OF ALL THE ENERGY FOR THE NON-FAILED RAYS
C
          SPDEXT=.TRUE.
          IF(STATSP.OR.TPT.EQ.2) THEN
C     PROCEED WITH FULL STATS
C
C       NOW CALCULATE THE RSS SPOT SIZE
C     INITIALIZE VALIABLES
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
              MSS=0.0D0
              AMSS=0.0D0
              MSSX=0.0D0
              AMSSX=0.0D0
              MSSY=0.0D0
              AMSSY=0.0D0
              RSS=0.0D0
              RSSX=0.0D0
              RSSY=0.0D0
              DO I=1,ITOT-1
C     LOAD DSPOTT(I,*) INTO DSPOT(*)
                  ID=I
                  CALL SPOTIT(4)
                  SPT=DSPOT(17)
                  IF(SPT.NE.0.0D0) THEN
                      IF(DSPOT(7).EQ.0.0D0) THEN
                          APFAC=DSPOT(11)
                          LA=DSPOT(1)
                          LC=DSPOT(2)
                          LB=DTAN(DSPOT(9))
                          LD=DTAN(DSPOT(10))
                          AFLB=DSPOT(9)
                          AFLD=DSPOT(10)
C     FOR FOCAL SYSTEMS
C
C     FOCAL SYSTEM
C       X ONLY
                          SSSPX=SSSPX+
     1                    (SPT*(((LA-SPA)**2))*APFAC)
C     AFOCAL SYSTEM
C       X ONLY
                          ASSSPX=ASSSPX+
     1                    (SPT*(((AFLB-AFSPB)**2))*APFAC)
C     FOCAL SYSTEM
C       Y ONLY
                          SSSPY=SSSPY+
     1                    (SPT*(((LC-SPC)**2))*APFAC)
C     AFOCAL SYSTEM
C       Y ONLY
                          ASSSPY=ASSSPY+
     1                    (SPT*(+((AFLD-AFSPD)**2))*APFAC)
                      END IF
                  END IF
              END DO
              MSS=SSSP
              AMSS=ASSSP
              MSSX=SSSPX
              AMSSX=ASSSPX
              MSSY=SSSPY
              AMSSY=ASSSPY
              IF(SYSTEM1(30).EQ.1.0D0.OR.SYSTEM1(30).EQ.2.0D0) THEN
                  RSSX=2.0D0*(DSQRT(MSSX/W))
                  RSSY=2.0D0*(DSQRT(MSSY/W))
                  RSS=(RSSX+RSSY)/2.0D0
              ELSE
                  RSSX=2.0D0*(DSQRT(AMSSX/W))
                  RSSY=2.0D0*(DSQRT(AMSSY/W))
                  RSS=(RSSX+RSSY)/2.0D0
              END IF
C     RESET SP VARS FOR CENT
              SPA=OSPA
              SPC=OSPC
              SPB=OSPB
              SPD=OSPD
              AFSPB=OAFSPB
              AFSPD=OAFSPD
C       NOW CALCULATE THE ROOT MEAN SQUARE SPOT SIZE
C     INITIALIZE VALIABLES
C     A,B,C AND D TERMS
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
              MSS=0.0D0
              AMSS=0.0D0
              MSSX=0.0D0
              AMSSX=0.0D0
              MSSY=0.0D0
              AMSSY=0.0D0
              RMS=0.0D0
              RMSX=0.0D0
              RMSY=0.0D0
              DO I=1,ITOT-1
C     LOAD DSPOTT(I,*) INTO DSPOT(*)
                  ID=I
                  CALL SPOTIT(4)
                  SPT=DSPOT(17)
                  IF(SPT.NE.0.0D0) THEN
                      IF(DSPOT(7).EQ.0.0D0) THEN
                          APFAC=DSPOT(11)
                          LA=DSPOT(1)
                          LC=DSPOT(2)
                          LB=DTAN(DSPOT(9))
                          LD=DTAN(DSPOT(10))
                          AFLB=DSPOT(9)
                          AFLD=DSPOT(10)
C     FOR FOCAL SYSTEMS
C
C     FOCAL SYSTEM
C       X ONLY
                          SSSPX=SSSPX+
     1                    (SPT*(((LA-SPA)**2))*APFAC)
                          SSSQX=SSSQX+
     1                    (SPT*((LA*LB)-(LA*SPB)-(LB*SPA)
     2                    +(SPA*SPB))*APFAC)
                          SSSRX=SSSRX+
     1                    (SPT*(((LB-SPB)**2))*APFAC)
C     AFOCAL SYSTEM
C       X ONLY
                          ASSSPX=ASSSPX+
     1                    (SPT*(((AFLB-AFSPB)**2))*APFAC)
                          ASSSQX=0.0D0
                          ASSSRX=0.0D0
C     FOCAL SYSTEM
C       Y ONLY
                          SSSPY=SSSPY+
     1                    (SPT*(((LC-SPC)**2))*APFAC)
                          SSSQY=SSSQY+
     1                    (SPT*((LC*LD)
     2                    -(LC*SPD)-(LD*SPC)+(SPC*SPD))*APFAC)
                          SSSRY=SSSRY+
     1                    (SPT*(((LD-SPD)**2))*APFAC)
C     AFOCAL SYSTEM
C       Y ONLY
                          ASSSPY=ASSSPY+
     1                    (SPT*(+((AFLD-AFSPD)**2))*APFAC)
                          ASSSQY=0.0D0
                          ASSSRY=0.0D0
                      END IF
                  END IF
              END DO
              MSS=SSSP
              AMSS=ASSSP
              MSSX=SSSPX
              AMSSX=ASSSPX
              MSSY=SSSPY
              AMSSY=ASSSPY
              IF(SYSTEM1(30).EQ.1.0D0.OR.SYSTEM1(30).EQ.2.0D0) THEN
C     FOCAL SYSTEMS ONLY
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
C     AFOCAL, NO MOVE TO BEST SPOT SIZE
                  FCSFT=0.0D0
                  FCSFTX=0.0D0
                  FCSFTY=0.0D0
              END IF
              IF(SYSTEM1(6).EQ.1.0) UN='INCHES     '
              IF(SYSTEM1(6).EQ.2.0) UN='CENTIMETERS'
              IF(SYSTEM1(6).EQ.3.0) UN='MILLIMETERS'
              IF(SYSTEM1(6).EQ.4.0) UN='METERS'
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
              MAXX=DABS(XUP-XLO)
              MAXY=DABS(YUP-YLO)
          ELSE
C     ONLY MINIMUM STATS WERE DONE
              SAVE_KDP(1)=SAVEINPT(1)
              INPUT='SPD MOVEACC 0'
              CALL PROCES
              REST_KDP(1)=RESTINPT(1)
          END IF
 114      FORMAT('SPOT DIAGRAM SUMMARY')
 115      FORMAT('RMS SPOT SIZE   (X) = ',G17.10,1X,A11)
 116      FORMAT('RMS SPOT SIZE   (Y) = ',G17.10,1X,A11)
 108      FORMAT('RMS SPOT DIAMETER   = ',G17.10,1X,A11)
 101      FORMAT('X-CENTROID POSITION = ',G17.10,1X,A11)
 102      FORMAT('Y-CENTROID POSITION = ',G17.10,1X,A11)
 103      FORMAT('CHIEF RAY X-COORD.  = ',G17.10,1X,A11)
 104      FORMAT('CHIEF RAY Y-COORD.  = ',G17.10,1X,A11)
 203      FORMAT('CHIEF RAY X-SLOPE   = ',G17.10,1X,'RADIANS')
 204      FORMAT('CHIEF RAY Y-SLOPE   = ',G17.10,1X,'RADIANS')
 105      FORMAT('# RAYS ATTEMPTED    = ',I10,' AT EACH WAVELENGTH')
 106      FORMAT('# RAYS SUCCEDED     = ',I10,' AT ALL WAVELENGTHS')
 1106     FORMAT('% TRANSMISSION      = ',G12.5)
 221      FORMAT(
     1    'FOCUS SHIFT FOR BEST RMS SPOT LENGTH (X) = ',G17.10,1X,A11)
 222      FORMAT(
     1    'FOCUS SHIFT FOR BEST RMS SPOT LENGTH (Y) = ',G17.10,1X,A11)
 220      FORMAT(
     1    'FOCUS SHIFT FOR BEST RMS SPOT DIAMETER   = ',G17.10,1X,A11)
 107      FORMAT(
     1    '"FOB NULL" WAS USED, CHIEF RAY DATA MAY BE SUSPECT')
C     NOW DO THE RAY FAILURE STATISTICS
          IF(TPT.NE.2) CALL FALRAY
C     NOW DO PRINTOUTS AS REQUIRED
C
          IF(TPT.EQ.1) THEN
C
              IF(SQ.EQ.0) THEN
C       DO THE PRINT OUT
                  IF(SYSTEM1(30).EQ.3.0D0.OR.SYSTEM1(30).EQ.4.0D0) THEN
C       MODE AFOCAL

                      IF(MSGSPD)WRITE(OUTLYNE,114)
                      IF(MSGSPD) CALL SHOWIT(0)

                      IF(STATSP.AND.MSGSPD) WRITE(OUTLYNE,208) RMS
                      IF(STATSP.AND.MSGSPD) CALL SHOWIT(0)

                      IF(STATSP.AND.MSGSPD) WRITE(OUTLYNE,215) RMSX
                      IF(STATSP.AND.MSGSPD) CALL SHOWIT(0)

                      IF(STATSP.AND.MSGSPD) WRITE(OUTLYNE,216) RMSY
                      IF(STATSP.AND.MSGSPD) CALL SHOWIT(0)
 215                  FORMAT('RMS SPOT SIZE   (X) = ',G17.10,1X,'RADIANS')
 216                  FORMAT('RMS SPOT SIZE   (Y) = ',G17.10,1X,'RADIANS')
 208                  FORMAT('RMS SPOT DIAMETER   = ',G17.10,1X,'RADIANS')

                      IF(MSGSPD)WRITE(OUTLYNE,201) CENTX
                      IF(MSGSPD) CALL SHOWIT(0)

                      IF(MSGSPD)WRITE(OUTLYNE,202) CENTY
                      IF(MSGSPD) CALL SHOWIT(0)
 201                  FORMAT('X-CENTROID (ANGLE)  = ',G17.10,1X,'RADIANS')
 202                  FORMAT('Y-CENTROID (ANGLE)  = ',G17.10,1X,'RADIANS')
C       AFOCAL

                      IF(MSGSPD)WRITE(OUTLYNE,203) REFRY(11,NEWIMG)
                      IF(MSGSPD) CALL SHOWIT(0)

                      IF(MSGSPD)WRITE(OUTLYNE,204) REFRY(12,NEWIMG)
                      IF(MSGSPD) CALL SHOWIT(0)

                      IF(NULL.AND.MSGSPD) WRITE(OUTLYNE,107)
                      IF(NULL.AND.MSGSPD) CALL SHOWIT(0)

                      IF(MSGSPD)WRITE(OUTLYNE,105) IWIW
                      IF(MSGSPD) CALL SHOWIT(0)

                      SPDTRANS=(TOT/DBLE(NUMTOT))*100.0D0
C
                      IF(MSGSPD)WRITE(OUTLYNE,1106) (TOT/DBLE(NUMTOT))*100.0D0
                      IF(MSGSPD) CALL SHOWIT(0)
                      IF(MSGSPD)WRITE(OUTLYNE,106) NUMTOT
                      IF(MSGSPD) CALL SHOWIT(0)
 2110                 FORMAT('AT WAVELENGTH # = 1,  # RAYS SUCCEDED = ',I10)
 2111                 FORMAT('AT WAVELENGTH # = 2,  # RAYS SUCCEDED = ',I10)
 2112                 FORMAT('AT WAVELENGTH # = 3,  # RAYS SUCCEDED = ',I10)
 2113                 FORMAT('AT WAVELENGTH # = 4,  # RAYS SUCCEDED = ',I10)
 2114                 FORMAT('AT WAVELENGTH # = 5,  # RAYS SUCCEDED = ',I10)
 2115                 FORMAT('AT WAVELENGTH # = 6,  # RAYS SUCCEDED = ',I10)
 2116                 FORMAT('AT WAVELENGTH # = 7,  # RAYS SUCCEDED = ',I10)
 2117                 FORMAT('AT WAVELENGTH # = 8,  # RAYS SUCCEDED = ',I10)
 2118                 FORMAT('AT WAVELENGTH # = 9,  # RAYS SUCCEDED = ',I10)
 2119                 FORMAT('AT WAVELENGTH # = 10, # RAYS SUCCEDED = ',I10)

                      IF(MSGSPD)WRITE(OUTLYNE,2110) NUMT1
                      IF(MSGSPD) CALL SHOWIT(0)
                      IF(MSGSPD)WRITE(OUTLYNE,2111) NUMT2
                      IF(MSGSPD) CALL SHOWIT(0)
                      IF(MSGSPD)WRITE(OUTLYNE,2112) NUMT3
                      IF(MSGSPD) CALL SHOWIT(0)
                      IF(MSGSPD)WRITE(OUTLYNE,2113) NUMT4
                      IF(MSGSPD) CALL SHOWIT(0)
                      IF(MSGSPD)WRITE(OUTLYNE,2114) NUMT5
                      IF(MSGSPD) CALL SHOWIT(0)
                      IF(MSGSPD)WRITE(OUTLYNE,2115) NUMT6
                      IF(MSGSPD) CALL SHOWIT(0)
                      IF(MSGSPD)WRITE(OUTLYNE,2116) NUMT7
                      IF(MSGSPD) CALL SHOWIT(0)
                      IF(MSGSPD)WRITE(OUTLYNE,2117) NUMT8
                      IF(MSGSPD) CALL SHOWIT(0)
                      IF(MSGSPD)WRITE(OUTLYNE,2118) NUMT9
                      IF(MSGSPD) CALL SHOWIT(0)
                      IF(MSGSPD)WRITE(OUTLYNE,2119) NUMT10
                      IF(MSGSPD) CALL SHOWIT(0)

                  ELSE
C       FOCAL

                      IF(MSGSPD)WRITE(OUTLYNE,114)
                      IF(MSGSPD) CALL SHOWIT(0)

                      IF(STATSP.AND.MSGSPD) WRITE(OUTLYNE,108) RMS,UN
                      IF(STATSP.AND.MSGSPD) CALL SHOWIT(0)

                      IF(STATSP.AND.MSGSPD) WRITE(OUTLYNE,115) RMSX,UN
                      IF(STATSP.AND.MSGSPD) CALL SHOWIT(0)

                      IF(STATSP.AND.MSGSPD) WRITE(OUTLYNE,116) RMSY,UN
                      IF(STATSP.AND.MSGSPD) CALL SHOWIT(0)

                      IF(MSGSPD)WRITE(OUTLYNE,101) CENTX,UN
                      IF(MSGSPD) CALL SHOWIT(0)

                      IF(MSGSPD)WRITE(OUTLYNE,102) CENTY,UN
                      IF(MSGSPD) CALL SHOWIT(0)

                      IF(MSGSPD)WRITE(OUTLYNE,103) REFRY(1,NEWIMG),UN
                      IF(MSGSPD) CALL SHOWIT(0)

                      IF(MSGSPD)WRITE(OUTLYNE,104) REFRY(2,NEWIMG),UN
                      IF(MSGSPD) CALL SHOWIT(0)

                      IF(NULL.AND.MSGSPD) WRITE(OUTLYNE,107)
                      IF(NULL.AND.MSGSPD) CALL SHOWIT(0)

                      IF(MSGSPD)WRITE(OUTLYNE,105) IWIW
                      IF(MSGSPD) CALL SHOWIT(0)
C
                      SPDTRANS=(TOT/DBLE(NUMTOT))*100.0D0
C

                      IF(MSGSPD)WRITE(OUTLYNE,1106) (TOT/DBLE(NUMTOT))*100.0D0
                      IF(MSGSPD) CALL SHOWIT(0)
                      IF(MSGSPD)WRITE(OUTLYNE,106) NUMTOT
                      IF(MSGSPD) CALL SHOWIT(0)

                      IF(MSGSPD)WRITE(OUTLYNE,2110) NUMT1
                      IF(MSGSPD) CALL SHOWIT(0)
                      IF(MSGSPD)WRITE(OUTLYNE,2111) NUMT2
                      IF(MSGSPD) CALL SHOWIT(0)
                      IF(MSGSPD)WRITE(OUTLYNE,2112) NUMT3
                      IF(MSGSPD) CALL SHOWIT(0)
                      IF(MSGSPD)WRITE(OUTLYNE,2113) NUMT4
                      IF(MSGSPD) CALL SHOWIT(0)
                      IF(MSGSPD)WRITE(OUTLYNE,2114) NUMT5
                      IF(MSGSPD) CALL SHOWIT(0)
                      IF(MSGSPD)WRITE(OUTLYNE,2115) NUMT6
                      IF(MSGSPD) CALL SHOWIT(0)
                      IF(MSGSPD)WRITE(OUTLYNE,2116) NUMT7
                      IF(MSGSPD) CALL SHOWIT(0)
                      IF(MSGSPD)WRITE(OUTLYNE,2117) NUMT8
                      IF(MSGSPD) CALL SHOWIT(0)
                      IF(MSGSPD)WRITE(OUTLYNE,2118) NUMT9
                      IF(MSGSPD) CALL SHOWIT(0)
                      IF(MSGSPD)WRITE(OUTLYNE,2119) NUMT10
                      IF(MSGSPD) CALL SHOWIT(0)

                      IF(STATSP.AND.MSGSPD) WRITE(OUTLYNE,220) FCSFT
                      IF(STATSP.AND.MSGSPD) CALL SHOWIT(0)

                      IF(STATSP.AND.MSGSPD) WRITE(OUTLYNE,221) FCSFTX
                      IF(STATSP.AND.MSGSPD) CALL SHOWIT(0)

                      IF(STATSP.AND.MSGSPD) WRITE(OUTLYNE,222) FCSFTY
                      IF(STATSP.AND.MSGSPD) CALL SHOWIT(0)
                  END IF
C WQ IS ACC, NO OUTPUT
              END IF
C
C     IF WQ EQ ISTAT OR IPSTAT, CALL THE ISTAT SUBROUTINE
              IF(WQ.EQ.'ISTAT'.OR.WQ.EQ.'IPSTAT') THEN
                  IF(WQ.EQ.'ISTAT')  CALL ISTAT(1,STARANG,ENDANG,DELANG,NSTEP)
                  IF(WQ.EQ.'IPSTAT') CALL ISTAT(2,STARANG,ENDANG,DELANG,NSTEP)
                  IF(IMFLAG) THEN
                      IMFLAG=.FALSE.
                      NEWIMG=LSTIMG
                  END IF
              END IF
          END IF
          RETURN
      END
