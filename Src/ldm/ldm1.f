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

C       FIRST FILE FOR LENS DATABASE MANAGER FILES

C SUB LENNS.FOR
      SUBROUTINE LENNS
          USE GLOBALS
C
          IMPLICIT NONE
C
!        INTEGER I,K,J
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datcfg.inc'
          REAL*8 XFOBB0,YFOBB0,ZFOBB0
C
          COMMON/OHFOB/XFOBB0,YFOBB0,ZFOBB0
C
C       THIS IS SUBROUTINE LENNS. THIS IS THE SUBROUTINE WHICH
C       INITIALLY STARTS THE SETUP OF A NEW LENS INTO THE ARRAY
C       ALENS. THE CMD LEVEL IS DISABLED AND FLAG F5 IS SET TO 1.
C
          XFOBB0=0.0D0
          YFOBB0=0.0D0
          ZFOBB0=0.0D0
          NUMHITS(0:MAXSUR)=1
C
          IF(SQ.EQ.1.OR.SN.EQ.1) THEN
              OUTLYNE='"LENS" TAKES NO EXPLICIT INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          MFG(1:75)='Manufacturer Name'
          CATNUM(1:75)='Lens Catalog Number'
          F1=0
          F5=1
C       SET GLASS CATALOG FLAG TO 1
          F22=1
C       INITIALIZE THE REFERECE RAY LOGICAL TO FALSE
C       AND NULL TO FALSE
C
          CALL AUTOFF
C
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
C       ARRAY SYSTEM STORES NUMERIC DATA NOT SPECIFIC TO A
C       PARTICULAR SURFACE.
          LBL(0)(1:80)=' '
          ALENS(1:LSIZ,0)=0.0D0
          SOLVE(1:9,0)=0.0D0
          PIKUP(1:6,0,1:PSIZ)=0.0D0
          MULTCLAP(1:1000,1:3,0)=0.0D0
          MULTCOBS(1:1000,1:3,0)=0.0D0
C
          SYSTEM1(1:SSIZ)=0.0D0
C
C       SYSTEM1(31) TO SYSTEM1(35) REPRESENT THE SPTWT VALUES
C       WHICH DEFAULT TO 1.0 AT LENS START UP.
C
          SYSTEM1(31:35)=1.0D0
C
C       THE VALUES OF THE REFRACTIVE INDICES FOR THE 5 POSSIBLE
C       WAVELENGTHS ARE STORED FOR THE JTH SURFACE OF THE LENS
C       IN ALENS(46,J),ALENS(47,J),ALENS(48,J),ALENS(49,J) AND
C       ALENS(50,J). THESE ARRAY ELEMENTS ALWAYS START WITH VALUES
C     AND 71 TO 75 FOR WAVELENGTHS 6 TO 10
C       OF 1.0 STORED IN THEM REPRESENTING THE INDICES OF THE DEFAULT
C       "AIR" MATERIAL. SO DO ELEMENTS 71 TO 75
C
C       THE ARRAY GLANAM STORES GLASS NAMES ASSOCIATED WITH
C       EACH SURFACE WHEN APPROPRIATE. COLUMN 1 STORES THE
C       MANUFACTURER NAME (CATALOG NAME) AND COLUMN 2 STORES
C       THE SPECIFIC GLASS DESIGNATOR (LIKE BK7).
C
C       INITIALIZE THE LENS TITLE AND ITS CONTINUATION AS WELL.
C
          IF(SST.EQ.1) THEN
              LI=WS
          ELSE
              LI='NEW LENS'
          END IF
          LIC(1)=CNULL
          LIC(2)=CNULL
          LIC(3)=CNULL
          LIC(4)=CNULL
          LICCNT=1
          INNI=CNULL
          LLTYPE=CNULL
C
C       INITIALIZE THE LENS SURFACE POINTER TO 0
C
          SURF=0
C       EACH AND EVERY TIME THE VARIABLE SURF IS CHANGED,
C       A FLOATING POINT REPRESENTATION IS STORED IN SYSTEM1(20)
          SYSTEM1(20)=DBLE(SURF)
C
C       SET DEFAULT WAVLENGTHS TO:
C
C       INITIALIZE MULTIPLE FIELDS
          CFLDCNT=0
          CFLDTYPE=0
          CFLDS(1:2,1:10)=0.0D0
C
C                       WV1=0.58756
C                       WV2=0.48613
C                       WV3=0.65627
C                       WV4=0.43584
C                       WV5=0.70652
C
C       WV1 THROUGH WV5 ARE STORED IN SYSTEM1(1) THROUGH
C       SYSTEM1(5) IN THE SYSTEM ARRAY.
C
C       SET THE DEFAULT WAVELENGTH VALUES.
C
          SYSTEM1(1)=0.58756D0
          SYSTEM1(2)=0.48613D0
          SYSTEM1(3)=0.65627D0
          SYSTEM1(4)=0.43584D0
          SYSTEM1(5)=0.70652D0
C     SET WAVELENGTHS 6 TO 10 TO ZERO AND WEIGHTS TO ZERO
          SYSTEM1(71:80)=0.0D0
          SYSTEM1(111)=0.58756D0
          SYSTEM1(112)=0.48613D0
          SYSTEM1(113)=0.65627D0
          SYSTEM1(114)=0.43584D0
          SYSTEM1(115)=0.70652D0
C     SET WAVELENGTHS 6 TO 10 TO ZERO AND WEIGHTS TO ZERO
          SYSTEM1(116:120)=0.0D0
C
C       THE PROGRAM DEFAULT UNITS ARE INCHES. THE UNIT CODE IS:
C
C                       INCHES=SYSTEM1(6)=1
C                       CM    =SYSTEM1(6)=2
C                       MM    =SYSTEM1(6)=3
C
          SYSTEM1(6)=1.0D0
C
C       SET THE DEFAULT VALUES FOR THE PRIMARY WAVELENGTH PAIRS
C       STORED IN SYSTEM1(7) AND SYSTEM1(8).
C
C       THESE PERTAIN TO THE COMMAND PCW.
          SYSTEM1(7)=2.0D0
          SYSTEM1(8)=3.0D0
C
C       SET THE DEFAULT VALUES FOR THE SECONDARY WAVELENGTH PAIRS
C       STORED IN SYSTEM1(9) AND SYSTEM1(10).
C
C       THESE PERTAIN TO THE COMMAND SCW.
          SYSTEM1(9)=2.0D0
          SYSTEM1(10)=1.0D0
C
C
C       SET THE DEFAULT VALUE FOR THE CONTROL WAVELENGTH
C       STORED IN SYSTEM1(11).
C
C       THESE PERTAIN TO THE COMMAND SCW.
          SYSTEM1(11)=1.0D0
C
C       SET THE DEFAULT VALUE FOR THE SAY AND SAX TO 1.0
C       STORED IN SYSTEM1(12) AND SYSTEM1(13).
C
C       THESE PERTAIN TO THE COMMANDS SAY AND SAX AND WRX,WRY.
          SYSTEM1(64)=0.0D0
          SYSTEM1(67)=0.0D0
          SYSTEM1(12)=1.0D0
          SYSTEM1(13)=1.0D0
          SYSTEM1(83)=1.0D0
          SYSTEM1(84)=1.0D0
          SYSTEM1(85:86)=1.0D0
          SYSTEM1(87:88)=0.001D0
C     SET  RAY AIMING ON
          SYSTEM1(62)=1.0D0
C     SET TEL OFF
          SYSTEM1(63)=0.0D0
C     SET AIMAPL OFF
          SYSTEM1(70)=0.0D0
C
C       SET THE DEFAULT VALUES FOR SCY AND SCX.
C       EACH HAS TWO VALUES. SCY HAS Y0 AND Y1
C       SCX HAS X0 AND X1.
C       THE DEFAULT VALUES ARE Y0=1.0,Y1=0.0
C       X0=1.0,X1=0.0. THESE FOUR VALUES ARE STORED IN SYSTEM
C       14,15,16,AND 17
C
          SYSTEM1(14)=1.0D0
          SYSTEM1(15)=0.0D0
          SYSTEM1(16)=1.0D0
          SYSTEM1(17)=0.0D0
C       NOTE:
C       THE ALTERNATE FORMS OF SCY AND SCX ARE THE ANGULAR FORMS
C       SCY FANG AND SCX FANG.
C       IF THEY ARE USED FOR INPUT,
C       THEY USE THEIR INPUT VALUES PLUS LENS DATA FOR SURFACES
C       0 (OBJECT) AND 1 INORDER TO CALCULATE SCY,Y0 Y1 AND
C       SCX,X0 X1.
C
C       IF THESE ALTERNATE ANGULAR FORMS ARE USED, SYSTEM1(18)
C       FOR SCY FANG AND SYSTEM1(19) FOR SCX FANG ARE SET TO 1.0.
C       OTHERWISE THEY REMAIN SET TO 0.0 AS A DEFAULT.
C
          SYSTEM1(18:19)=0.0D0
C
C       DEFAULT FOR IMAGE SURFACE NUMBER IS 1.0
          SYSTEM1(20)=1.0D0
C
C       THE DEFAULT VALUES FOR SCY FANG AND SCX FANG AUTOMATICALLY
C       ARE ASSIGNED FROM LENS DATA AND SCY AND SCX VALUES AT "EOS".
C       DURING LENS INPUT THE DEFAULT VALUES ARE 0.0 AND ARE STORED
C       SYSTEM1(21 THROUGH24)
          SYSTEM1(21:24)=0.0D0
C
C       SYSTEM1(49) TRACKS IF SCX/SCX FANG OR SAX HAVE BEEN ENTERED
C       EXPLICITLY.
C       IT IS ASSUMED THAT THEY HAVE NOT IN WHICH CASE
C       SYSTEM1(49)=0.0, PUPIL AND FIELD ARE CIRCULAR
C
          SYSTEM1(49)=0.0D0
C
C       ALL SURFACES ARE ASSUMED TO BE FLAT,SPHERICAL OR CONIC
C               FOR ASPHERICS  -  ALENS(8,SURF)=1.0
C
C       THE DEFAULT REFERENCE SURFACE IS SURFACE 1
C       TRACKER BY THE VALUE IN SYSTEM1(25)
C
          SYSTEM1(25)=1.0D0
C
C       THE DEFAULT APERTURE STOP CONDITION IS "NO"
C       AND IS INDICATED BY THE VALUE IN SYSTEM1(26) OF -99
C       THIS IS THE DEFAULT. WHEN ASTOP NO IS ISSUED, SYSTEM1(26)
C       IS ALWAYS SET TO THIS DEFAULT VALUE, OTHERWISE IT IS
C       SET EQUAL TO THE FLOATING POINT REPRESENTATION OF THE SURFACE
C       NUMBER WHICH IS TO BE THE APERTURE STOP
C
          SYSTEM1(26)=-99.0D0
C
C       THE ASTOP ADJUSTMENT IS TRACKED IN SYSTEM1(27)
C                       DEFAULT IS NO ADJUSTMENT = 0.0
C       EN ADJUST = SYSTEM1(27)=1.0
C       EX ADJUSTMENT = SYSTEM1(27)=-1.0
C
          SYSTEM1(27)=0.0D0
C
C
C       THE LENS EVALUATION MODE (FOCAL,UFOCAL,AFOCAL OR UAFOCAL)
C       STATUS IS STORED IN SYSTEM1(30) AS 1.0,2.0,3.0 OR 4.0
C       RESPECTIVELY. FOCAL IS THE DEFAULT MODE SET HERE.
C
          SYSTEM1(30)=1.0D0
C
C       SET SYSTEM1(50) TO ITS DEFAULT OF 1.00
C
          SYSTEM1(50)=1.00D0
C       SET SYSTEM1(56) TO ITS DEFAULT OF 1.00
C
          SYSTEM1(56)=1.00D0
C
C
C       NOTE:
C       ACTUAL OUTPUT OF THE "CURRENT" LENS TO LENSTEXT.DATA IS
C       HANDELED DURING THE EXI OF EXIT ROUTINE.
C
C       ANY OTHER ACTION TO BE TAKEN DURING LENS INPUT IS HANDLED FROM
C       WITHIN SUBROUTINE LENIN.
C
C       INITIALIZE THE ARRAY THAT TRACKS THE EXISTENCE OF
C       ENTRIES IN THE CONFIG SUBFILE
          CFGCNT(2:MAXCFG)=0
          RETURN
      END
C SUB XYGAMA.FOR
      SUBROUTINE XYGAMA
C
          IMPLICIT NONE
C
C       THIS ROUTINE DOES THE PLOT XSHIFT,YSHIFT AND GAMMA COMMANDS
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
          IF(STI.EQ.0) THEN
C       CHECK SYNTAX
              IF(SST.EQ.1) THEN
                  OUTLYNE=
     1            '"PLOT "'//WQ(1:6)//'" TAKES NO STRING INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
                  OUTLYNE=
     1            '"PLOT "'//WQ(1:6)//
     1            '" TAKES NO NUMERIC WORD #2 THROUGH #5 INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(DF1.EQ.1) THEN
                  OUTLYNE=
     1            '"PLOT "'//WQ(1:6)//
     1            ' REQUIRES EXPLICIT NUMERIC WORD #1 INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C
              IF(WQ.EQ.'XSHIFT') PXSHFT=INT(W1)
              IF(WQ.EQ.'XSHIFT') LORIENT=.FALSE.
              IF(WQ.EQ.'YSHIFT') PYSHFT=INT(W1)
              IF(WQ.EQ.'YSHIFT') LORIENT=.FALSE.
              IF(WQ.EQ.'GAMMA') PGAMMA=INT(W1)
 67           IF(PGAMMA.GE.360) THEN
                  PGAMMA=PGAMMA-360
                  IF(PGAMMA.GE.360) GO TO 67
              ELSE
C     PROCEED
              END IF
 76           IF(PGAMMA.LE.-360) THEN
                  PGAMMA=PGAMMA+360
                  IF(PGAMMA.LE.360) GO TO 76
              ELSE
C     PROCEED
              END IF
              RETURN
          ELSE
C     STI=1
          END IF
          IF(STI.EQ.1) THEN
              IF(WQ.EQ."XSHIFT") WRITE(OUTLYNE,801) PXSHFT
              IF(WQ.EQ."YSHIFT") WRITE(OUTLYNE,802) PYSHFT
              IF(WQ.EQ."GAMMA") WRITE(OUTLYNE,803) PGAMMA
              CALL SHOWIT(1)
 801          FORMAT('THE CURRENT "XSHIFT" VALUE IS ',I6)
 802          FORMAT('THE CURRENT "YSHIFT" VALUE IS ',I6)
 803          FORMAT('THE CURRENT "GAMMA" VALUE IS ',I4,' DEGREES')
              RETURN
          END IF
          RETURN
      END
C SUB ULQUER.FOR
      SUBROUTINE ULQUER
C
          IMPLICIT NONE
C
C       THIS SUBROUTINE DISPLAYS THE CURRENT VALUE OF A LENS
C       SYSTEM PARAMETER FROM WITHIN THE UPDATE LENS
C       LEVEL IN RESPONSE TO A COMMAND NAME FOLLOWED BY A ?
C
          REAL*8 VALUE1,VALUE2,VALUE3,VALUE4,VALUE5,VALUE6
C
          INTEGER V1,V2,V3,V4,V5,V6,VA1,VA1WS1,VA1WS2,VA1WS3,VA1WS4
     1    ,MAT,REPEAT,I
C
          CHARACTER VAL*80,VALWS1*80,VALWS2*80,VALWS3*80
     1    ,VALWS4*80
C
          INCLUDE 'datcfg.inc'
          INCLUDE 'datmac.inc'
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
C
          VALUE1=0.0D0
          VALUE2=0.0D0
          VALUE3=0.0D0
          VALUE4=0.0D0
          VALUE5=0.0D0
          VALUE6=0.0D0
          VAL=AA//AA//AA//AA
          VALWS1=AA//AA//AA//AA
          VALWS2=AA//AA//AA//AA
          VALWS3=AA//AA//AA//AA
          VALWS4=AA//AA//AA//AA
          V1=0
          V2=0
          V3=0
          V4=0
          V5=0
          V6=0
          VA1=0
          VA1WS1=0
          VA1WS2=0
          VA1WS3=0
          VA1WS4=0
          REPEAT=0
C
          MAT=0
          IF(WC.EQ.'AIR') MAT=1
          IF(WC.EQ.'REFL') MAT=1
          IF(WC.EQ.'REFLTIR') MAT=1
          IF(WC.EQ.'REFLTIRO') MAT=1
          IF(WC.EQ.'PERFECT') MAT=1
          IF(WC.EQ.'IDEAL') MAT=1
          IF(WC.EQ.'GLA') MAT=1
          IF(WC.EQ.'GLASS') MAT=1
          IF(WC.EQ.'MODEL') MAT=1
          IF(WC.EQ.'OHARA') MAT=1
          IF(WC.EQ.'HOYA') MAT=1
          IF(WC.EQ.'HIKARI') MAT=1
          IF(WC.EQ.'SCHOTT') MAT=1
          IF(WC.EQ.'SCH2000') MAT=1
          IF(WC.EQ.'CHANCE') MAT=1
          IF(WC.EQ.'CORNIN') MAT=1
          IF(WC.EQ.'USER') MAT=1
          IF(WC.EQ.'RADHARD') MAT=1
          IF(WC.EQ.'GLCAT') MAT=1
          IF(WC.EQ.'MATL') MAT=1
          IF(WC.EQ.'RUSSIAN') MAT=1
          IF(WC.NE.'IDEAL') THEN
              IF(MAT.EQ.1) THEN
                  VA1WS1=1
                  VALWS1=GLANAM(SURF,1)//GLANAM(SURF,2)
                  VAL='CURRENT SURFACE MATERIAL TYPE IS:'
                  VA1=1
                  IF(WC.EQ.'MODEL') THEN
                      V1=1
                      VALUE1=ALENS(86,SURF)
                      V2=1
                      VALUE2=ALENS(87,SURF)
                      V3=1
                      VALUE3=ALENS(89,SURF)
                  END IF
                  GO TO 200
              ELSE
              END IF
              IF(WC.EQ.'N1'.OR.WC.EQ.'N2'.OR.WC.EQ.'N3'.OR.WC.EQ.'N4'
     1        .OR.WC.EQ.'N5'.OR.WC.EQ.'N6'.OR.WC.EQ.'N7'.OR.WC.EQ.'N8'
     2        .OR.WC.EQ.'N9'.OR.WC.EQ.'N10') THEN
                  IF(WC.EQ.'N1') THEN
                      VA1=1
                      VAL='THE REFRACTIVE INDEX FOR WAVELENGTH #1 IS:'
                      V1=1
                      VALUE1=ALENS(46,SURF)
                      GO TO 200
                  ELSE
                  END IF
                  IF(WC.EQ.'N2') THEN
                      VA1=1
                      VAL='THE REFRACTIVE INDEX FOR WAVELENGTH #2 IS:'
                      V1=1
                      VALUE1=ALENS(47,SURF)
                      GO TO 200
                  ELSE
                  END IF
                  IF(WC.EQ.'N3') THEN
                      VA1=1
                      VAL='THE REFRACTIVE INDEX FOR WAVELENGTH #3 IS:'
                      V1=1
                      VALUE1=ALENS(48,SURF)
                      GO TO 200
                  ELSE
                  END IF
                  IF(WC.EQ.'N4') THEN
                      VA1=1
                      VAL='THE REFRACTIVE INDEX FOR WAVELENGTH #4 IS:'
                      V1=1
                      VALUE1=ALENS(49,SURF)
                      GO TO 200
                  ELSE
                  END IF
                  IF(WC.EQ.'N5') THEN
                      VA1=1
                      VAL='THE REFRACTIVE INDEX FOR WAVELENGTH #5 IS:'
                      V1=1
                      VALUE1=ALENS(50,SURF)
                      GO TO 200
                  ELSE
                  END IF
                  IF(WC.EQ.'N6') THEN
                      VA1=1
                      VAL='THE REFRACTIVE INDEX FOR WAVELENGTH #6 IS:'
                      V1=1
                      VALUE1=ALENS(71,SURF)
                      GO TO 200
                  ELSE
                  END IF
                  IF(WC.EQ.'N7') THEN
                      VA1=1
                      VAL='THE REFRACTIVE INDEX FOR WAVELENGTH #7 IS:'
                      V1=1
                      VALUE1=ALENS(72,SURF)
                      GO TO 200
                  ELSE
                  END IF
                  IF(WC.EQ.'N8') THEN
                      VA1=1
                      VAL='THE REFRACTIVE INDEX FOR WAVELENGTH #8 IS:'
                      V1=1
                      VALUE1=ALENS(73,SURF)
                      GO TO 200
                  ELSE
                  END IF
                  IF(WC.EQ.'N9') THEN
                      VA1=1
                      VAL='THE REFRACTIVE INDEX FOR WAVELENGTH #9 IS:'
                      V1=1
                      VALUE1=ALENS(74,SURF)
                      GO TO 200
                  ELSE
                  END IF
                  IF(WC.EQ.'N10') THEN
                      VA1=1
                      VAL='THE REFRACTIVE INDEX FOR WAVELENGTH #10 IS:'
                      V1=1
                      VALUE1=ALENS(75,SURF)
                      GO TO 200
                  ELSE
                  END IF
              ELSE
              END IF
              IF(WC.EQ.'INDEX'.OR.WC.EQ.'VNUM'.OR.WC.EQ.'DPART') THEN
                  IF(GLANAM(SURF,1) .NE.'MODEL') THEN
                      OUTLYNE='NO "INDEX", "VNUM" OR "DPART" VALUES EXIST'
                      CALL SHOWIT(1)
                      OUTLYNE='FOR NON-"MODEL" GLASSES'
                      CALL SHOWIT(1)
                  ELSE
                      IF(WC.EQ.'INDEX') THEN
                          VA1=1
                          VAL='THE "INDEX" VALUE IS:'
                          V1=1
                          VALUE1=ALENS(86,SURF)
                          GO TO 200
                      END IF
                      IF(WC.EQ.'VNUM') THEN
                          VA1=1
                          VAL='THE "VNUM" VALUE IS:'
                          V1=1
                          VALUE1=ALENS(87,SURF)
                          GO TO 200
                      END IF
                      IF(WC.EQ.'DPART') THEN
                          VA1=1
                          VAL='THE "DPART" VALUE IS:'
                          V1=1
                          VALUE1=ALENS(89,SURF)
                          GO TO 200
                      END IF
                  END IF
              END IF
          ELSE
C     WC IS 'IDEAL'
              IF(MAT.EQ.1) THEN
                  VA1WS1=1
                  V1=1
                  VALWS1=GLANAM(SURF,1)//GLANAM(SURF,2)
                  VAL='CURRENT SURFACE MATERIAL TYPE IS:'
                  VALUE1=ALENS(121,SURF)
                  VA1=1
              ELSE
              END IF
          END IF
          IF(WC.EQ.'UNITS') THEN
              IF(SYSTEM1(6).EQ.1.0D0) VALWS1='INCHES'
              IF(SYSTEM1(6).EQ.2.0D0) VALWS1='CM    '
              IF(SYSTEM1(6).EQ.3.0D0) VALWS1='MM    '
              IF(SYSTEM1(6).EQ.4.0D0) VALWS1='METERS'
              VA1WS1=1
              VAL='CURRENT SYSTEM UNITS ARE:'
              VA1=1
              GO TO 200
          ELSE
          END IF
          IF(WC.EQ.'REAL'.OR.WC.EQ.'PARAX') THEN
              IF(ALENS(124,SURF).EQ.0.0D0) VALWS1='REAL'
              IF(ALENS(124,SURF).EQ.1.0D0) VALWS1='PARAX'
              VA1WS1=1
              VAL='SURFACE TYPE IS:'
              VA1=1
              GO TO 200
          ELSE
          END IF
          IF(WC.EQ.'LBL'.OR.WC.EQ.'LABEL') THEN
              VA1=1
              VAL=LBL(SURF)
              IF(ALENS(44,SURF).EQ.0.0D0)
     1        VAL='"THERE IS NO LABEL FOR THE CURRENT SURFACE"'
              GO TO 200
          ELSE
          END IF
          IF(WC.EQ.'LI') THEN
              VA1=1
              VAL='THE CURRENT LENS IDENTIFIER IS:'
              VA1WS1=1
              VALWS1=LI
              IF(LI.EQ.AA) VALWS1='"LI IS ALL BLANK"'
              VA1WS1=1
              GO TO 200
          ELSE
          END IF
          IF(WC.EQ.'MFG') THEN
              VA1=1
              VAL='THE CURRENT MFG IS:'
              VA1WS1=1
              VALWS1=MFG
              IF(MFG.EQ.AA) VALWS1='"MFG IS ALL BLANK"'
              VA1WS1=1
              GO TO 200
          ELSE
          END IF
          IF(WC.EQ.'CATNUM') THEN
              VA1=1
              VAL='THE CURRENT CATNUM IS:'
              VA1WS1=1
              VALWS1=CATNUM
              IF(CATNUM.EQ.AA) VALWS1='"CATNUM IS ALL BLANK"'
              VA1WS1=1
              GO TO 200
          ELSE
          END IF
          IF(WC.EQ.'INI') THEN
              VA1=1
              VAL='THE CURRENT DESIGNER IDENTIFIER IS:'
              VA1WS1=1
              VALWS1=INNI
              IF(INNI.EQ.AA) VALWS1='"INI IS ALL BLANK"'
              VA1WS1=1
              GO TO 200
          ELSE
          END IF
          IF(WC.EQ.'LTYPE') THEN
              VA1=1
              VAL='THE CURRENT LENS TYPE IDENTIFIER IS:'
              VA1WS1=1
              VALWS1=LLTYPE(1:5)
              IF(INNI.EQ.AA) VALWS1='"LTYPE IS ALL BLANK"'
              VA1WS1=1
              GO TO 200
          ELSE
          END IF
          IF(WC.EQ.'LIC') THEN
              VAL='THE CURRENT LENS IDENTIFIER CONTINUATION IS:'
              VA1=1
              VALWS1=LIC(1)
              IF(LIC(1).EQ.AA) VALWS1='"LIC (LINE 1) IS ALL BLANK"'
              VA1WS1=1
              VALWS2=LIC(2)
              IF(LIC(2).EQ.AA) VALWS2='"LIC (LINE 2) IS ALL BLANK"'
              VA1WS2=1
              VALWS3=LIC(3)
              IF(LIC(3).EQ.AA) VALWS3='"LIC (LINE 3) IS ALL BLANK"'
              VA1WS3=1
              VALWS4=LIC(4)
              IF(LIC(4).EQ.AA) VALWS4='"LIC (LINE 4) IS ALL BLANK"'
              VA1WS4=1
              GO TO 200
          ELSE
          END IF
          IF(WC.EQ.'REFS') THEN
              VA1=1
              VAL=
     1        'CURRENT REFERENCE SURFACE NUMBER AND ORIENTATION ANGLE(DEG) ARE:'
              V1=1
              VALUE1=SYSTEM1(25)
              V2=1
              VALUE2=SYSTEM1(59)
              GO TO 200
          ELSE
          END IF
C
          IF(WC.EQ.'WRY'.OR.WC.EQ.'WRX'.OR.WC.EQ.'BDX'.OR.
     1    WC.EQ.'BDY') THEN
              VA1=1
              IF(WC.EQ.'WRY') VAL='CURRENT "WRY" VALUE IS:'
              IF(WC.EQ.'WRX') VAL='CURRENT "WRX" VALUE IS:'
              IF(WC.EQ.'BDY') VAL='CURRENT "BDY" VALUE IS:'
              IF(WC.EQ.'BDX') VAL='CURRENT "BDX" VALUE IS:'
              V1=1
              IF(WC.EQ.'WRX') VALUE1=SYSTEM1(85)
              IF(WC.EQ.'WRY') VALUE1=SYSTEM1(86)
              IF(WC.EQ.'BDX') VALUE1=SYSTEM1(87)
              IF(WC.EQ.'BDY') VALUE1=SYSTEM1(88)
              GO TO 200
          ELSE
          END IF
          IF(WC.EQ.'SAY'.OR.WC.EQ.'SAX') THEN
              IF(SYSTEM1(64).NE.0.0D0) THEN
                  WRITE(OUTLYNE,*)'"'//WC(1:3)//'" HAS NOT BEEN EXPLICITLY SET'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'BUT IS HELD WITH AN "NAO(X OR Y)" ASSIGNMENT'
                  CALL SHOWIT(1)
              ELSE
              END IF
              IF(SYSTEM1(67).NE.0.0D0) THEN
                  WRITE(OUTLYNE,*)'"'//WC(1:3)//'" HAS NOT BEEN EXPLICITLY SET'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'BUT IS HELD WITH AN "FNO(X OR Y)" ASSIGNMENT'
                  CALL SHOWIT(1)
              ELSE
              END IF
              IF(SYSTEM1(83).NE.0.0D0) THEN
                  WRITE(OUTLYNE,*)'"SAY" IS CURRENTLY FLOATING'
                  CALL SHOWIT(1)
              ELSE
              END IF
              IF(SYSTEM1(84).NE.0.0D0) THEN
                  WRITE(OUTLYNE,*)'"SAX" IS CURRENTLY FLOATING'
                  CALL SHOWIT(1)
              ELSE
              END IF
              VA1=1
              IF(WC.EQ.'SAY') VAL='CURRENT "SAY" VALUE IS:'
              IF(WC.EQ.'SAX') VAL='CURRENT "SAX" VALUE IS:'
              V1=1
              IF(WC.EQ.'SAY') VALUE1=SYSTEM1(12)
              IF(WC.EQ.'SAX') VALUE1=SYSTEM1(13)
              GO TO 200
          ELSE
          END IF
          IF(WC.EQ.'NAOY'.OR.WC.EQ.'NAOX') THEN
              IF(SYSTEM1(64).NE.1.0D0) THEN
                  WRITE(OUTLYNE,*)'NOTE:'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'"'//WC(1:4)//'" HAS NOT BEEN EXPLICITLY SET'
                  CALL SHOWIT(1)
                  IF(INT(SYSTEM1(11)).GE.1.AND.INT(SYSTEM1(11)).LE.6) THEN
                      SYSTEM1(65)=(ALENS(45+INT(SYSTEM1(11)),0)*SYSTEM1(12))/
     1                DSQRT((ALENS(3,0)**2)+(SYSTEM1(12)**2))
                      SYSTEM1(66)=(ALENS(45+INT(SYSTEM1(11)),0)*SYSTEM1(13))/
     1                DSQRT((ALENS(3,0)**2)+(SYSTEM1(13)**2))
                      SYSTEM1(83)=0.0D0
                      SYSTEM1(84)=0.0D0
                  END IF
                  IF(INT(SYSTEM1(11)).GE.6.AND.INT(SYSTEM1(11)).LE.10) THEN
                      SYSTEM1(65)=(ALENS(70-5+INT(SYSTEM1(11)),0)*SYSTEM1(12))/
     1                DSQRT((ALENS(3,0)**2)+(SYSTEM1(12)**2))
                      SYSTEM1(66)=(ALENS(70-5+INT(SYSTEM1(11)),0)*SYSTEM1(13))/
     1                DSQRT((ALENS(3,0)**2)+(SYSTEM1(13)**2))
                      SYSTEM1(83)=0.0D0
                      SYSTEM1(84)=0.0D0
                  END IF
              ELSE
              END IF
              VA1=1
              IF(WC.EQ.'NAOY') VAL='CURRENT "NAOY" VALUE IS:'
              IF(WC.EQ.'NAOX') VAL='CURRENT "NAOX" VALUE IS:'
              V1=1
              IF(WC.EQ.'NAOY') VALUE1=SYSTEM1(65)
              IF(WC.EQ.'NAOX') VALUE1=SYSTEM1(66)
              GO TO 200
          ELSE
          END IF
          IF(WC.EQ.'FNOY'.OR.WC.EQ.'FNOX') THEN
              IF(SYSTEM1(67).NE.1.0D0) THEN
                  WRITE(OUTLYNE,*)'NOTE:'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'"'//WC(1:4)//'" HAS NOT BEEN EXPLICITLY SET'
                  CALL SHOWIT(1)
                  SYSTEM1(68)=1.0D0/((2.0D0*SYSTEM1(12))/ALENS(3,0))
                  SYSTEM1(69)=1.0D0/((2.0D0*SYSTEM1(13))/ALENS(3,0))
                  SYSTEM1(83)=0.0D0
                  SYSTEM1(84)=0.0D0
              ELSE
              END IF
              VA1=1
              IF(WC.EQ.'FNOY') VAL='CURRENT "FNOY" VALUE IS:'
              IF(WC.EQ.'FNOX') VAL='CURRENT "FNOX" VALUE IS:'
              V1=1
              IF(WC.EQ.'FNOY') VALUE1=SYSTEM1(68)
              IF(WC.EQ.'FNOX') VALUE1=SYSTEM1(69)
              GO TO 200
          ELSE
          END IF
          IF(WC.EQ.'SCY'.OR.WC.EQ.'SCX') THEN
              VA1=1
              IF(WC.EQ.'SCY'.AND.SYSTEM1(18).EQ.0.0D0)
     1        VAL='CURRENT "SCY" VALUES ARE:'
              IF(WC.EQ.'SCY'.AND.SYSTEM1(18).EQ.1.0D0)
     1        VAL='CURRENT "SCY FANG" VALUES ARE:'
              IF(WC.EQ.'SCX'.AND.SYSTEM1(19).EQ.0.0D0)
     1        VAL='CURRENT "SCX" VALUES ARE:'
              IF(WC.EQ.'SCX'.AND.SYSTEM1(19).EQ.1.0D0)
     1        VAL='CURRENT "SCX FANG" VALUES ARE:'
              V1=1
              V2=1
              IF(WC.EQ.'SCY'.AND.SYSTEM1(18).EQ.0.0D0) THEN
                  VALUE1=SYSTEM1(14)
                  VALUE2=SYSTEM1(15)
              ELSE
              END IF
              IF(WC.EQ.'SCY'.AND.SYSTEM1(18).EQ.1.0D0) THEN
                  VALUE1=SYSTEM1(21)
                  VALUE2=SYSTEM1(22)
              ELSE
              END IF
              IF(WC.EQ.'SCX'.AND.SYSTEM1(19).EQ.0.0D0) THEN
                  VALUE1=SYSTEM1(16)
                  VALUE2=SYSTEM1(17)
              ELSE
              END IF
              IF(WC.EQ.'SCX'.AND.SYSTEM1(19).EQ.1.0D0) THEN
                  VALUE1=SYSTEM1(23)
                  VALUE2=SYSTEM1(24)
              ELSE
              END IF
              GO TO 200
          ELSE
          END IF
          IF(WC.EQ.'PXIM'.OR.WC.EQ.'PYIM') THEN
              VA1=1
              IF(WC.EQ.'PXIM'.AND.SYSTEM1(94).EQ.0.0D0)
     1        VAL='CURRENT "PXIM" VALUES ARE:'
              IF(WC.EQ.'PXIM'.AND.SYSTEM1(94).EQ.1.0D0)
     1        VAL='CURRENT "PXIM FANG" VALUES ARE:'
              IF(WC.EQ.'PYIM'.AND.SYSTEM1(95).EQ.0.0D0)
     1        VAL='CURRENT "PYIM" VALUES ARE:'
              IF(WC.EQ.'PYIM'.AND.SYSTEM1(95).EQ.1.0D0)
     1        VAL='CURRENT "PYIM FANG" VALUES ARE:'
              V1=1
              IF(WC.EQ.'PXIM') THEN
                  VALUE1=SYSTEM1(92)
              ELSE
              END IF
              IF(WC.EQ.'PYIM') THEN
                  VALUE1=SYSTEM1(93)
              ELSE
              END IF
              GO TO 200
          ELSE
          END IF
          IF(WC.EQ.'RXIM'.OR.WC.EQ.'RYIM') THEN
              VA1=1
              IF(WC.EQ.'RXIM'.AND.SYSTEM1(98).EQ.0.0D0)
     1        VAL='CURRENT "RXIM" VALUES ARE:'
              IF(WC.EQ.'RXIM'.AND.SYSTEM1(98).EQ.1.0D0)
     1        VAL='CURRENT "RXIM FANG" VALUES ARE:'
              IF(WC.EQ.'RYIM'.AND.SYSTEM1(99).EQ.0.0D0)
     1        VAL='CURRENT "RYIM" VALUES ARE:'
              IF(WC.EQ.'RXIM'.AND.SYSTEM1(99).EQ.1.0D0)
     1        VAL='CURRENT "RYIM FANG" VALUES ARE:'
              V1=1
              IF(WC.EQ.'RXIM') THEN
                  VALUE1=SYSTEM1(96)
              ELSE
              END IF
              IF(WC.EQ.'RYIM') THEN
                  VALUE1=SYSTEM1(97)
              ELSE
              END IF
              GO TO 200
          ELSE
          END IF
          IF(WC.EQ.'ASTOP') THEN
              VA1=1
              VAL='CURRENT APERTURE STOP SURFACE NUMBER IS:'
              V1=1
              VALUE1=SYSTEM1(26)
              GO TO 200
          ELSE
          END IF
          IF(WC.EQ.'AUTOFUNC') THEN
              VA1=1
              VAL='CURRENT AUTOFUNC FUNCTION NUMBER IS:'
              V1=1
              VALUE1=SYSTEM1(91)
              GO TO 200
          ELSE
          END IF
          IF(WC.EQ.'DEFORM') THEN
              IF(ALENS(103,SURF).EQ.0.0D0) THEN
                  VA1=1
                  VAL='CURRENT SURFACE IS NOT A DEFORMABLE SURFACE'
              ELSE
                  VA1=1
                  VAL='CURRENT SURFACE IS A DEFORMABLE SURFACE'
                  V1=1
                  VALUE1=ALENS(104,SURF)
                  V3=1
                  VALUE2=ALENS(105,SURF)
                  V4=1
                  VALUE2=ALENS(105,SURF)
              END IF
              GO TO 200
          ELSE
          END IF
          IF(WC.EQ.'SPTWT') THEN
              VA1=1
              VAL='CURRENT SPECTRAL WEIGHTING FACTORS (1-5) ARE:'
              V1=1
              VALUE1=SYSTEM1(31)
              V2=1
              VALUE2=SYSTEM1(32)
              V3=1
              VALUE3=SYSTEM1(33)
              V4=1
              VALUE4=SYSTEM1(34)
              V5=1
              VALUE5=SYSTEM1(35)
              GO TO 200
          ELSE
          END IF
          IF(WC.EQ.'SPTWT2') THEN
              VA1=1
              VAL='CURRENT SPECTRAL WEIGHTING FACTORS (6-10) ARE:'
              V1=1
              VALUE1=SYSTEM1(76)
              V2=1
              VALUE2=SYSTEM1(77)
              V3=1
              VALUE3=SYSTEM1(78)
              V4=1
              VALUE4=SYSTEM1(79)
              V5=1
              VALUE5=SYSTEM1(80)
              GO TO 200
          ELSE
          END IF
          IF(WC.EQ.'CW') THEN
              VA1=1
              VAL='CURRENT CONTROL WAVELENGTH NUMBER IS:'
              V1=1
              VALUE1=SYSTEM1(11)
              GO TO 200
          ELSE
          END IF
          IF(WC.EQ.'PCW') THEN
              VA1=1
              VAL='CURRENT PRIMARY WAVELENGTH PAIR NUMBERS ARE:'
              V1=1
              VALUE1=SYSTEM1(7)
              V2=1
              VALUE2=SYSTEM1(8)
              GO TO 200
          ELSE
          END IF
          IF(WC.EQ.'SCW') THEN
              VA1=1
              VAL='CURRENT SECONDARY WAVELENGTH PAIR NUMBERS ARE:'
              V1=1
              VALUE1=SYSTEM1(9)
              V1=2
              VALUE1=SYSTEM1(10)
              GO TO 200
          ELSE
          END IF
          IF(WC.EQ.'MODE') THEN
              VA1=1
              IF(SYSTEM1(30).EQ.1) VAL='CURRENT MODE IS "FOCAL"'
              IF(SYSTEM1(30).EQ.2) VAL='CURRENT MODE IS "UFOCAL"'
              IF(SYSTEM1(30).EQ.3) VAL='CURRENT MODE IS "AFOCAL"'
              IF(SYSTEM1(30).EQ.4) VAL='CURRENT MODE IS "UAFOCAL"'
              GO TO 200
          ELSE
          END IF
          IF(WC.EQ.'CV') THEN
              VA1=1
              VAL='CURRENT SURFACE CURVATURE IS:'
              V1=1
              VALUE1=ALENS(1,SURF)
              GO TO 200
          ELSE
          END IF
          IF(WC.EQ.'INR') THEN
              VA1=1
              VAL='CURRENT SURFACE "INR" VALUE IS:'
              V1=1
              VALUE1=ALENS(76,SURF)
              GO TO 200
          ELSE
          END IF
          IF(WC.EQ.'CVTOR') THEN
              VA1=1
              VAL='CURRENT SURFACE TORIC CURVATURE IS:'
              V1=1
              VALUE1=ALENS(24,SURF)
              GO TO 200
          ELSE
          END IF
          IF(WC.EQ.'TH') THEN
              VA1=1
              VAL='CURRENT SURFACE THICKNESS IS:'
              V1=1
              VALUE1=ALENS(3,SURF)
              GO TO 200
          ELSE
          END IF
          IF(WC.EQ.'THM') THEN
              VA1=1
              VAL='CURRENT MIRROR THICKNESS IS:'
              V1=1
              VALUE1=ALENS(110,SURF)
              GO TO 200
          ELSE
          END IF
          IF(WC.EQ.'PRICE') THEN
              VA1=1
              VAL='CURRENT PRICE PER UNIT MASS IS:'
              V1=1
              VALUE1=ALENS(111,SURF)
              GO TO 200
          ELSE
          END IF
          IF(WC.EQ.'AC') THEN
              VA1=1
              VAL='CURRENT 2ND ORDER ASPHERIC SURFACE COEFFICIENT IS:'
              V1=1
              VALUE1=ALENS(43,SURF)
              GO TO 200
          ELSE
          END IF
          IF(WC.EQ.'AD') THEN
              VA1=1
              VAL='CURRENT 4TH ORDER ASPHERIC SURFACE COEFFICIENT IS:'
              V1=1
              VALUE1=ALENS(4,SURF)
              GO TO 200
          ELSE
          END IF
          IF(WC.EQ.'AE') THEN
              VA1=1
              VAL='CURRENT 6TH ORDER ASPHERIC SURFACE COEFFICIENT IS:'
              V1=1
              VALUE1=ALENS(5,SURF)
              GO TO 200
          ELSE
          END IF
          IF(WC.EQ.'AF') THEN
              VA1=1
              VAL='CURRENT 8TH ORDER ASPHERIC SURFACE COEFFICIENT IS:'
              V1=1
              VALUE1=ALENS(6,SURF)
              GO TO 200
          ELSE
          END IF
          IF(WC.EQ.'AG') THEN
              VA1=1
              VAL='CURRENT 10TH ORDER ASPHERIC SURFACE COEFFICIENT IS:'
              V1=1
              VALUE1=ALENS(7,SURF)
              GO TO 200
          ELSE
          END IF
          IF(WC.EQ.'AH') THEN
              VA1=1
              VAL='CURRENT 12TH ORDER ASPHERIC SURFACE COEFFICIENT IS:'
              V1=1
              VALUE1=ALENS(81,SURF)
              GO TO 200
          ELSE
          END IF
          IF(WC.EQ.'AI') THEN
              VA1=1
              VAL='CURRENT 14TH ORDER ASPHERIC SURFACE COEFFICIENT IS:'
              V1=1
              VALUE1=ALENS(82,SURF)
              GO TO 200
          ELSE
          END IF
          IF(WC.EQ.'AJ') THEN
              VA1=1
              VAL='CURRENT 16TH ORDER ASPHERIC SURFACE COEFFICIENT IS:'
              V1=1
              VALUE1=ALENS(83,SURF)
              GO TO 200
          ELSE
          END IF
          IF(WC.EQ.'AK') THEN
              VA1=1
              VAL='CURRENT 18TH ORDER ASPHERIC SURFACE COEFFICIENT IS:'
              V1=1
              VALUE1=ALENS(84,SURF)
              GO TO 200
          ELSE
          END IF
          IF(WC.EQ.'AL') THEN
              VA1=1
              VAL='CURRENT 20TH ORDER ASPHERIC SURFACE COEFFICIENT IS:'
              V1=1
              VALUE1=ALENS(85,SURF)
              GO TO 200
          ELSE
          END IF
          IF(WC.EQ.'ARRAY'.AND.ALENS(133,SURF).EQ.0.0D0) THEN
              VA1=1
              VAL=
     1        'SURFACE IS CURRENTLY NOT AN ARRAY LENS'
              GO TO 200
          ELSE
          END IF
          IF(WC.EQ.'ARRAY'.AND.ALENS(133,SURF).EQ.-1.0D0) THEN
              VA1=1
              VAL=
     1        'CURRENT ODD ARRAY SURFACE VALUES'
              VA1WS1=1
              VALWS1='(DX AND DY) ARE:'
              V1=1
              VALUE1=ALENS(131,SURF)
              V2=1
              VALUE2=ALENS(132,SURF)
              GO TO 200
          ELSE
          END IF
          IF(WC.EQ.'ARRAY'.AND.ALENS(133,SURF).EQ.1.0D0) THEN
              VA1=1
              VAL=
     1        'CURRENT EVEN ARRAY SURFACE VALUES'
              VA1WS1=1
              VALWS1='(DX AND DY) ARE:'
              V1=1
              VALUE1=ALENS(131,SURF)
              V2=1
              VALUE2=ALENS(132,SURF)
              GO TO 200
          ELSE
          END IF
          IF(WC.EQ.'ASPH') THEN
              VA1=1
              VAL=
     1        'CURRENT ASPHERIC SURFACE COEFFICIENTS'
              VA1WS1=1
              VALWS1='(AD, AE, AF, AG AND AC) ARE:'
              V1=1
              VALUE1=ALENS(4,SURF)
              V2=1
              VALUE2=ALENS(5,SURF)
              V3=1
              VALUE3=ALENS(6,SURF)
              V4=1
              VALUE4=ALENS(7,SURF)
              V5=1
              VALUE5=ALENS(43,SURF)
              GO TO 200
          ELSE
          END IF
          IF(WC.EQ.'ASPH2') THEN
              VA1=1
              VAL=
     1        'CURRENT ASPHERIC SURFACE COEFFICIENTS'
              VA1WS1=1
              VALWS1='(AH, AI, AJ, AK AND AL) ARE:'
              V1=1
              VALUE1=ALENS(81,SURF)
              V2=1
              VALUE2=ALENS(82,SURF)
              V3=1
              VALUE3=ALENS(83,SURF)
              V4=1
              VALUE4=ALENS(84,SURF)
              V5=1
              VALUE5=ALENS(85,SURF)
              GO TO 200
          ELSE
          END IF
          IF(WC.EQ.'TASPH') THEN
              VA1=1
              VAL=
     1        'CURRENT ANAMORPHIC ASPHERIC SURFACE COEFFICIENTS'
              VA1WS1=1
              VALWS1='(ADTOR, AETOR, AFTOR AND AGTOR) ARE:'
              V1=1
              VALUE1=ALENS(37,SURF)
              V2=1
              VALUE2=ALENS(38,SURF)
              V3=1
              VALUE3=ALENS(39,SURF)
              V4=1
              VALUE4=ALENS(40,SURF)
              GO TO 200
          ELSE
          END IF
          IF(WC.EQ.'ADTOR') THEN
              VA1=1
              VAL=
     1        'CURRENT 4TH ORDER ANAMORPHIC ASPHERIC SURFACE COEFFICIENT IS:'
              V1=1
              VALUE1=ALENS(37,SURF)
              GO TO 200
          ELSE
          END IF
          IF(WC.EQ.'AETOR') THEN
              VA1=1
              VAL=
     1        'CURRENT 6TH ORDER ANAMORPHIC ASPHERIC SURFACE COEFFICIENT IS:'
              V1=1
              VALUE1=ALENS(38,SURF)
              GO TO 200
          ELSE
          END IF
          IF(WC.EQ.'AFTOR') THEN
              VA1=1
              VAL=
     1        'CURRENT 8TH ORDER ANAMORPHIC ASPHERIC SURFACE COEFFICIENT IS:'
              V1=1
              VALUE1=ALENS(39,SURF)
              GO TO 200
          ELSE
          END IF
          IF(WC.EQ.'AGTOR') THEN
              VA1=1
              VAL=
     1        'CURRENT 10TH ORDER ANAMORPHIC ASPHERIC SURFACE COEFFICIENT IS:'
              V1=1
              VALUE1=ALENS(40,SURF)
              GO TO 200
          ELSE
          END IF
          IF(WC.EQ.'CC') THEN
              VA1=1
              VAL='SURFACE CONIC CONSTANT IS:'
              V1=1
              VALUE1=ALENS(2,SURF)
              GO TO 200
          ELSE
          END IF
          IF(WC.EQ.'CCTOR') THEN
              VA1=1
              VAL='CURRENT SURFACE ANAMORPHIC CONIC CONSTANT IS:'
              V1=1
              VALUE1=ALENS(41,SURF)
              GO TO 200
          ELSE
          END IF
          IF(WC.EQ.'GDX') THEN
              VA1=1
              VAL='SURFACE GLOBAL X-DECENTER IS:'
              V1=1
              VALUE1=ALENS(90,SURF)
              GO TO 200
          ELSE
          END IF
          IF(WC.EQ.'GDY') THEN
              VA1=1
              VAL='SURFACE GLOBAL Y-DECENTER IS:'
              V1=1
              VALUE1=ALENS(91,SURF)
              GO TO 200
          ELSE
          END IF
          IF(WC.EQ.'GDZ') THEN
              VA1=1
              VAL='SURFACE GLOBAL Z-DECENTER IS:'
              V1=1
              VALUE1=ALENS(92,SURF)
              GO TO 200
          ELSE
          END IF
          IF(WC.EQ.'GALPHA') THEN
              VA1=1
              VAL='SURFACE GLOBAL ALPHA TILT ANGLE IN DEGREES IS:'
              V1=1
              VALUE1=ALENS(93,SURF)
              GO TO 200
          ELSE
          END IF
          IF(WC.EQ.'GBETA') THEN
              VA1=1
              VAL='SURFACE GLOBAL BETA TILT ANGLE IN DEGREES IS:'
              V1=1
              VALUE1=ALENS(94,SURF)
              GO TO 200
          ELSE
          END IF
          IF(WC.EQ.'GGAMMA') THEN
              VA1=1
              VAL='SURFACE GLOBAL GAMMA TILT ANGLE IN DEGREES IS:'
              V1=1
              VALUE1=ALENS(95,SURF)
              GO TO 200
          ELSE
          END IF
          IF(WC.EQ.'ALPHA') THEN
              VA1=1
              VAL='SURFACE ALPHA TILT ANGLE IN DEGREES IS:'
              V1=1
              VALUE1=ALENS(118,SURF)
              GO TO 200
          ELSE
          END IF
          IF(WC.EQ.'BETA') THEN
              VA1=1
              VAL='SURFACE BETA TILT ANGLE IN DEGREES IS:'
              V1=1
              VALUE1=ALENS(119,SURF)
              GO TO 200
          ELSE
          END IF
          IF(WC.EQ.'GAMMA') THEN
              VA1=1
              VAL='SURFACE GAMMA TILT ANGLE IN DEGREES IS:'
              V1=1
              VALUE1=ALENS(120,SURF)
              GO TO 200
          ELSE
          END IF
          IF(WC.EQ.'TILT'.OR.WC.EQ.'RTILT') THEN
              IF(ALENS(25,SURF).EQ.0.0D0) THEN
                  VA1=1
                  VAL='SURFACE HAS NO TILT OR RTILT VALUES'
                  GO TO 200
              ELSE
              END IF
              VA1WS1=1
              VALWS1='SURFACE TILTS (ALPHA, BETA AND GAMMA) IN DEGREES ARE:'
              VA1=1
              VAL='SURFACE IS NOT CURRENTLY TILTED'
              IF(ALENS(25,SURF).EQ.1.0D0)
     1        VAL='CURRENT SURFACE HAS A "TILT" DEFINED ON IT'
              IF(ALENS(25,SURF).EQ.-1.0D0)
     1        VAL='CURRENT SURFACE HAS AN "RTILT" DEFINED ON IT'
              IF(ALENS(25,SURF).EQ.2.0D0)
     1        VAL='CURRENT SURFACE HAS A "TILT AUTO" DEFINED ON IT'
              IF(ALENS(25,SURF).EQ.3.0D0)
     1        VAL='CURRENT SURFACE HAS A "TILT AUTOM" DEFINED ON IT'
              IF(ALENS(25,SURF).EQ.4.0D0)
     1        VAL='CURRENT SURFACE HAS A "TILT BEN" DEFINED ON IT'
              IF(ALENS(25,SURF).EQ.5.0D0)
     1        VAL='CURRENT SURFACE HAS A "TILT DAR" DEFINED ON IT'
              IF(ALENS(25,SURF).EQ.7.0D0)
     1        VAL='CURRENT SURFACE HAS A "TILT REV" DEFINED ON IT'
              IF(ALENS(25,SURF).EQ.6.0D0.OR.ALENS(25,SURF).EQ.1.0D0.AND.
     1        ALENS(77,SURF).EQ.1.0D0)
     1        VAL='CURRENT SURFACE HAS A "TILT RET" DEFINED ON IT'
              IF(ALENS(25,SURF).EQ.6.0D0.OR.ALENS(25,SURF).EQ.1.0D0.AND.
     1        ALENS(77,SURF).EQ.1.0D0) THEN
                  V1=1
                  VALUE1=ALENS(70,SURF)
              ELSE
                  V1=1
                  VALUE1=ALENS(118,SURF)
                  V2=1
                  VALUE2=ALENS(119,SURF)
                  V3=1
                  VALUE3=ALENS(120,SURF)
              END IF
              GO TO 200
          ELSE
          END IF
          IF(WC.EQ.'XD') THEN
              VA1=1
              VAL='CURRENT SURFACE X-DECENTRATION IS:'
              V1=1
              VALUE1=ALENS(114,SURF)
              GO TO 200
          ELSE
          END IF
          IF(WC.EQ.'ZD') THEN
              VA1=1
              VAL='CURRENT SURFACE Z-DECENTRATION IS:'
              V1=1
              VALUE1=ALENS(116,SURF)
              GO TO 200
          ELSE
          END IF
          IF(WC.EQ.'YD') THEN
              VA1=1
              VAL='CURRENT SURFACE Y-DECENTRATION IS:'
              V1=1
              VALUE1=ALENS(115,SURF)
              GO TO 200
          ELSE
          END IF
          IF(WC.EQ.'DEC') THEN
              VA1=1
              VAL='CURRENT SURFACE Y, X AND Z DECENTRATIONS ARE:'
              V1=1
              VALUE1=ALENS(115,SURF)
              V2=1
              VALUE2=ALENS(114,SURF)
              V3=1
              VALUE3=ALENS(116,SURF)
              GO TO 200
          ELSE
          END IF
          IF(WC.EQ.'PIVOT') THEN
              VA1=1
              VAL='CURRENT SURFACE ALTERNATE PIVOT DEFINITIONS ARE:'
              V1=1
              VALUE1=ALENS(78,SURF)
              V2=1
              VALUE2=ALENS(79,SURF)
              V3=1
              VALUE3=ALENS(80,SURF)
              GO TO 200
          ELSE
          END IF
          IF(WC.EQ.'PIVX') THEN
              VA1=1
              VAL='CURRENT SURFACE X-PIVOT POSITION IS:'
              V1=1
              VALUE1=ALENS(78,SURF)
              GO TO 200
          ELSE
          END IF
          IF(WC.EQ.'PIVY') THEN
              VA1=1
              VAL='CURRENT SURFACE Y-PIVOT POSITION IS:'
              V1=1
              VALUE1=ALENS(79,SURF)
              GO TO 200
          ELSE
          END IF
          IF(WC.EQ.'RD') THEN
              VA1=1
              VAL='CURRENT SURFACE RADIUS OF CURVATURE IS:'
              IF(DABS(ALENS(1,SURF)).LT.1D-30) THEN
                  V1=1
                  VALUE1=0.0D0
              ELSE
                  V1=1
                  VALUE1=1.0D0/ALENS(1,SURF)
              END IF
              GO TO 200
          ELSE
          END IF
          IF(WC.EQ.'XTORIC'.OR.WC.EQ.'YTORIC') THEN
              VA1=1
              VAL='THE SURFACE IS CURRENTLY:'
              IF(ALENS(23,SURF).EQ.0.0D0)
     1        VALWS1='NOT DEFINED AS A TORIC'
              IF(ALENS(23,SURF).EQ.1.0D0)
     1        VALWS1='DEFINED AS A Y-TORIC'
              IF(ALENS(23,SURF).EQ.2.0D0)
     1        VALWS1='DEFINED AS AN X-TORIC'
              VA1WS1=1
              GO TO 200
          ELSE
          END IF
          IF(WC.EQ.'GRT') THEN
              VA1=1
              VAL='THE SURFACE IS CURRENTLY:'
              IF(ALENS(96,SURF).EQ.0.0D0)
     1        VALWS1='NOT A LINEAR DIFFRATION GRATING'
              IF(ALENS(96,SURF).EQ.1.0D0)
     1        VALWS1='A LINEAR DIFFRATION GRATING'
              GO TO 200
          ELSE
          END IF
          IF(WC.EQ.'GRO') THEN
              IF(ALENS(96,SURF).EQ.0.0D0) THEN
                  VA1=1
                  VAL='THE SURFACE NOT A LINEAR DIFFRATION GRATING:'
              ELSE
                  V1=1
                  VALUE1=ALENS(97,SURF)
              END IF
              GO TO 200
          ELSE
          END IF
          IF(WC.EQ.'GRS') THEN
              IF(ALENS(96,SURF).EQ.0.0D0) THEN
                  VA1=1
                  VAL='THE SURFACE NOT A LINEAR DIFFRATION GRATING:'
              ELSE
                  V1=1
                  VALUE1=ALENS(98,SURF)
              END IF
              GO TO 200
          ELSE
          END IF
          IF(WC.EQ.'GRX') THEN
              IF(ALENS(96,SURF).EQ.0.0D0) THEN
                  VA1=1
                  VAL='THE SURFACE NOT A LINEAR DIFFRATION GRATING:'
              ELSE
                  V1=1
                  VALUE1=ALENS(99,SURF)
              END IF
              GO TO 200
          ELSE
          END IF
          IF(WC.EQ.'GRY') THEN
              IF(ALENS(96,SURF).EQ.0.0D0) THEN
                  VA1=1
                  VAL='THE SURFACE NOT A LINEAR DIFFRATION GRATING:'
              ELSE
                  V1=1
                  VALUE1=ALENS(100,SURF)
              END IF
              GO TO 200
          ELSE
          END IF
          IF(WC.EQ.'GRZ') THEN
              IF(ALENS(96,SURF).EQ.0.0D0) THEN
                  VA1=1
                  VAL='THE SURFACE NOT A LINEAR DIFFRATION GRATING:'
              ELSE
                  V1=1
                  VALUE1=ALENS(101,SURF)
              END IF
              GO TO 200
          ELSE
          END IF
          IF(WC.EQ.'SPGR') THEN
              V1=1
              VALUE1=ALENS(102,SURF)
              GO TO 200
          ELSE
          END IF
          IF(WC.EQ.'RDTOR') THEN
              VA1=1
              VAL='CURRENT SURFACE TORIC RADIUS OF CURVATURE IS:'
              IF(DABS(ALENS(24,SURF)).LT.1D-30) THEN
                  V1=1
                  VALUE1=0.0D0
              ELSE
                  V1=1
                  VALUE1=1.0D0/ALENS(24,SURF)
              END IF
              GO TO 200
          ELSE
          END IF
          IF(WC.EQ.'CCR') THEN
              IF(ALENS(126,SURF).EQ.2.0D0) THEN
                  VA1=1
                  VAL='CORNER CUBE DATA IS:'
                  V1=1
                  VALUE1=ALENS(138,SURF)
                  V1=2
                  VALUE1=ALENS(139,SURF)
                  V1=3
                  VALUE1=ALENS(140,SURF)
                  V1=4
                  VALUE1=ALENS(141,SURF)
              ELSE
                  VA1=1
                  VAL='SURFACE IS NOT A CORNER CUBE'
              END IF
              GO TO 200
          ELSE
          END IF
          IF(WC.EQ.'ROO') THEN
              IF(ALENS(126,SURF).EQ.1.0D0) THEN
                  VA1=1
                  VAL='ROOF DATA IS:'
                  V1=1
                  VALUE1=ALENS(138,SURF)
                  V1=2
                  VALUE1=ALENS(139,SURF)
              ELSE
                  VA1=1
                  VAL='SURFACE IS NOT A ROOF SURFACE'
              END IF
              GO TO 200
          ELSE
          END IF
          IF(WC.EQ.'RAYERROR') THEN
              VA1=1
              VAL='RANDOM SURFACE RAY ERROR IS:'
              V1=1
              VALUE1=ALENS(144,SURF)
              GO TO 200
          ELSE
          END IF
          IF(WC.EQ.'COATING') THEN
              VA1=1
              VAL='CURRENT SURFACE COATING NUMBER IS:'
              V1=1
              VALUE1=ALENS(112,SURF)
              GO TO 200
          ELSE
          END IF
          IF(WC.EQ.'CHG') THEN
              VA1=1
              VAL='"CHG" MOVES THE SURFACE POINTER TO A NEW SURFACE'
              GO TO 200
          ELSE
          END IF
          IF(WC.EQ.'DEL') THEN
              VA1=1
              VAL='"DEL" DELETES A SURFACE'
              GO TO 200
          ELSE
          END IF
          IF(WC.EQ.'INS') THEN
              VA1=1
              VAL='"INS" INSERTS A NEW SURFACE'
              GO TO 200
          ELSE
          END IF
          IF(WC.EQ.'CLAPD') THEN
              VA1=1
              VAL='"CLAPD" DELETES A SURFACE CLEAR APERTURE DEFINITION'
              GO TO 200
          ELSE
          END IF
          IF(WC.EQ.'COBSD') THEN
              VA1=1
              VAL='"COBSD" DELETES A SURFACE OBSCURATION DEFINITION'
              GO TO 200
          ELSE
          END IF
          IF(WC.EQ.'FOOTBLOK') THEN
              VA1=1
              IF(ALENS(58,SURF).EQ.0.0D0)
     1        VAL='CURRENT SURFACE DOES NOT HAVE A "FOOTBLOK" DEFINITION ON IT'
              IF(ALENS(58,SURF).EQ.1.0D0)
     1        VAL='CURRENT SURFACE HAS A "FOOTBLOK" DEFINITION ON IT'
              GO TO 200
          ELSE
          END IF
          IF(WC.EQ.'PIVAXIS') THEN
              VA1=1
              IF(ALENS(113,SURF).EQ.0.0D0)
     1        VAL='CURRENT SURFACE HAS "PIVAXIS" SET TO "LOCAL"'
              IF(ALENS(113,SURF).EQ.1.0D0)
     1        VAL='CURRENT SURFACE HAS "PIVAXIS" SET TO "NORMAL"'
              GO TO 200
          ELSE
          END IF
          IF(WC.EQ.'NODUM') THEN
              VA1=1
              IF(ALENS(68,SURF).EQ.0.0D0)
     1        VAL='SURFACE NOT FORCED TO DUMMY'
              IF(ALENS(68,SURF).EQ.1.0D0)
     1        VAL='SURFACE FORCED TO DUMMY'
              GO TO 200
          ELSE
          END IF
          IF(WC.EQ.'TSD') THEN
              VA1=1
              VAL='"TSD" WILL DELETE A THICKNESS SOLVE'
              GO TO 200
          ELSE
          END IF
          IF(WC.EQ.'GRTD') THEN
              VA1=1
              VAL='"GRTD" WILL DELETE A DIFFRACTION GRATING'
              GO TO 200
          ELSE
          END IF
          IF(WC.EQ.'CSD') THEN
              VA1=1
              VAL='"CSD" WILL DELETE A XZ AND YZ-PLANE CURVATURE SOLVES'
              GO TO 200
          ELSE
          END IF
          IF(WC.EQ.'INRD') THEN
              VA1=1
              VAL='"INRD" WILL DELETE AN EXPLICIT "INR" ASSIGNMENT'
              GO TO 200
          ELSE
          END IF
          IF(WC.EQ.'CSDX') THEN
              VA1=1
              VAL='"CSDX" WILL DELETE AN XZ-PLANE CURVATURE SOLVE'
              GO TO 200
          ELSE
          END IF
          IF(WC.EQ.'CSDY') THEN
              VA1=1
              VAL='"CSDY" WILL DELETE A YZ-PLANE CURVATURE SOLVE'
              GO TO 200
          ELSE
          END IF
          IF(WC.EQ.'ARRAYD') THEN
              VA1=1
              VAL='"ARRAYD" WILL DELETE AN "ARRAY" DEFINITION'
              GO TO 200
          ELSE
          END IF
          IF(WC.EQ.'TASPHD') THEN
              VA1=1
              VAL='"TASPHD" WILL DELETE AN "TASPH" DEFINITION'
              GO TO 200
          ELSE
          END IF
          IF(WC.EQ.'ASPHD') THEN
              VA1=1
              VAL='"ASPHD" WILL DELETE AN "ASPH" DEFINITION'
              GO TO 200
          ELSE
          END IF
          IF(WC.EQ.'TILTD') THEN
              VA1=1
              VAL='"TILTD" WILL DELETE A "TILT" DEFINITION'
              GO TO 200
          ELSE
          END IF
          IF(WC.EQ.'TORD') THEN
              VA1=1
              VAL='"TORD" WILL DELETE A TORIC TYPE DEFINITION'
              GO TO 200
          ELSE
          END IF
          IF(WC.EQ.'PIKD') THEN
              VA1=1
              VAL='"PIKD" WILL DELETE A SPECIFIED PIKUP DEFINITION'
              GO TO 200
          ELSE
          END IF
C       WV
          IF(WC.EQ.'WV') THEN
              VA1=1
              VAL='CURRENT OPTICAL SYSTEM WAVELENGTHS (1-5) ARE:'
              V1=1
              VALUE1=SYSTEM1(1)
              V2=1
              VALUE2=SYSTEM1(2)
              V3=1
              VALUE3=SYSTEM1(3)
              V4=1
              VALUE4=SYSTEM1(4)
              V5=1
              VALUE5=SYSTEM1(5)
              GO TO 200
          ELSE
          END IF
          IF(WC.EQ.'WV2') THEN
              VA1=1
              VAL='CURRENT OPTICAL SYSTEM WAVELENGTHS (6-10) ARE:'
              V1=1
              VALUE1=SYSTEM1(71)
              V2=1
              VALUE2=SYSTEM1(72)
              V3=1
              VALUE3=SYSTEM1(73)
              V4=1
              VALUE4=SYSTEM1(74)
              V5=1
              VALUE5=SYSTEM1(75)
              GO TO 200
          ELSE
          END IF
          IF(WC.EQ.'SPIDER') THEN
              IF(ALENS(134,SURF).EQ.0.0D0) THEN
                  VA1=1
                  VAL='NO SPIDER IS DEFINED ON THIS SURFACE'
              ELSE
                  VA1=1
                  VAL='CURRENT SPIDER VALUES ARE:'
                  V1=1
                  VALUE1=ALENS(135,SURF)
                  V2=1
                  VALUE2=ALENS(136,SURF)
                  V3=1
                  VALUE3=ALENS(137,SURF)
                  V4=1
                  VALUE4=ALENS(138,SURF)
                  GO TO 200
              END IF
          ELSE
          END IF
          IF(WC.EQ.'MULTCLAP') THEN
              OUTLYNE='"MULTCLAP" QUERRY IS NOT AVAILABLE. USE "CAOB" FROM THE'
              CALL SHOWIT(1)
              OUTLYNE='CMD PROGRAM LEVEL'
              CALL SHOWIT(1)
              CALL MACFAL
              GO TO 200
          END IF
          IF(WC.EQ.'MULTCCOBS') THEN
              OUTLYNE='"MULTCOBS" QUERRY IS NOT AVAILABLE. USE "CAOB" FROM THE'
              CALL SHOWIT(1)
              OUTLYNE='CMD PROGRAM LEVEL'
              CALL SHOWIT(1)
              CALL MACFAL
              GO TO 200
          END IF
          IF(WC.EQ.'CLAP') THEN
              IF(DABS(ALENS(9,SURF)).EQ.0.0D0) THEN
                  ALENS(51:57,SURF)=0.0D0
                  WRITE(OUTLYNE,*)
     1            'NO "CLEAR APERTURE DATA" ON CURRENT SURFACE"'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  GO TO 200
              ELSE
                  VA1=1
                  IF(ALENS(9,SURF).EQ.1.0D0)
     1            VAL='CURRENT SURFACE "CLAP" VALUES ARE:'
                  IF(ALENS(9,SURF).EQ.2.0D0)
     1            VAL='CURRENT SURFACE "CLAP RECT" VALUES ARE:'
                  IF(ALENS(9,SURF).EQ.3.0D0)
     1            VAL='CURRENT SURFACE"CLAP ELIP" VALUES ARE:'
                  IF(ALENS(9,SURF).EQ.4.0D0)
     1            VAL='CURRENT SURFACE "CLAP RCTK" VALUES ARE:'
                  IF(ALENS(9,SURF).EQ.5.0D0)
     1            VAL='CURRENT SURFACE "CLAP POLY" VALUES ARE:'
                  IF(ALENS(9,SURF).EQ.6.0D0)
     1            VAL='CURRENT SURFACE "CLAP IPOLY" VALUES ARE:'
                  V1=1
                  VALUE1=ALENS(10,SURF)
                  V2=1
                  VALUE2=ALENS(11,SURF)
                  V3=1
                  VALUE3=ALENS(12,SURF)
                  V4=1
                  VALUE4=ALENS(13,SURF)
                  V5=1
                  VALUE5=ALENS(14,SURF)
                  V6=1
                  VALUE6=ALENS(15,SURF)
                  GO TO 200
              END IF
          ELSE
C       NOT CLAP
          END IF
          IF(WC.EQ.'CLAP') THEN
              IF(DABS(ALENS(51,SURF)).EQ.0.0D0) THEN
                  WRITE(OUTLYNE,*)
     1            'NO "CLEAR APERTURE ERASE DATA" ON CURRENT SURFACE"'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  GO TO 200
              ELSE
                  VA1=1
                  IF(ALENS(51,SURF).EQ.1.0D0)
     1            VAL='CURRENT SURFACE "CLAP ERASE" VALUES ARE:'
                  IF(ALENS(51,SURF).EQ.2.0D0)
     1            VAL='CURRENT SURFACE "CLAP RECTE" VALUES ARE:'
                  IF(ALENS(51,SURF).EQ.3.0D0)
     1            VAL='CURRENT SURFACE "CLAP ELIPE" VALUES ARE:'
                  IF(ALENS(51,SURF).EQ.4.0D0)
     1            VAL='CURRENT SURFACE "CLAP RCTKE" VALUES ARE:'
                  IF(ALENS(51,SURF).EQ.5.0D0)
     1            VAL='CURRENT SURFACE "CLAP POLYE" VALUES ARE:'
                  IF(ALENS(51,SURF).EQ.6.0D0)
     1            VAL='CURRENT SURFACE "CLAP IPOLYE" VALUES ARE:'
                  V1=1
                  VALUE1=ALENS(52,SURF)
                  V2=1
                  VALUE2=ALENS(53,SURF)
                  V3=1
                  VALUE3=ALENS(54,SURF)
                  V4=1
                  VALUE4=ALENS(55,SURF)
                  V5=1
                  VALUE5=ALENS(56,SURF)
                  V6=1
                  VALUE6=ALENS(57,SURF)
                  GO TO 200
              END IF
          ELSE
C       NOT CLAP
          END IF
          IF(WC.EQ.'COBS') THEN
              IF(DABS(ALENS(16,SURF)).EQ.0.0D0) THEN
                  ALENS(61:67,SURF)=0.0D0
                  WRITE(OUTLYNE,*)
     1            'NO "OBSCURATION DATA" ON CURRENT SURFACE"'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  GO TO 200
              ELSE
                  VA1=1
                  IF(ALENS(16,SURF).EQ.1.0D0)
     1            VAL='CURRENT SURFACE "COBS" VALUES ARE:'
                  IF(ALENS(16,SURF).EQ.2.0D0)
     1            VAL='CURRENT SURFACE "COBS RECT" VALUES ARE:'
                  IF(ALENS(16,SURF).EQ.3.0D0)
     1            VAL='CURRENT SURFACE "COBS ELIP" VALUES ARE:'
                  IF(ALENS(16,SURF).EQ.4.0D0)
     1            VAL='CURRENT SURFACE "COBS RCTK" VALUES ARE:'
                  IF(ALENS(16,SURF).EQ.5.0D0)
     1            VAL='CURRENT SURFACE "COBS POLY" VALUES ARE:'
                  IF(ALENS(16,SURF).EQ.6.0D0)
     1            VAL='CURRENT SURFACE "COBS IPOLY" VALUES ARE:'
                  V1=1
                  VALUE1=ALENS(17,SURF)
                  V2=1
                  VALUE2=ALENS(18,SURF)
                  V3=1
                  VALUE3=ALENS(19,SURF)
                  V4=1
                  VALUE4=ALENS(20,SURF)
                  V5=1
                  VALUE5=ALENS(21,SURF)
                  V6=1
                  VALUE6=ALENS(22,SURF)
                  GO TO 200
              END IF
          ELSE
C       NOT COBS
          END IF
          IF(WC.EQ.'COBS') THEN
              IF(DABS(ALENS(61,SURF)).EQ.0.0D0) THEN
                  WRITE(OUTLYNE,*)
     1            'NO "OBSCURATION ERASE DATA" ON CURRENT SURFACE"'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  GO TO 200
              ELSE
                  VA1=1
                  IF(ALENS(61,SURF).EQ.1.0D0)
     1            VAL='CURRENT SURFACE "COBS ERASE" VALUES ARE:'
                  IF(ALENS(61,SURF).EQ.2.0D0)
     1            VAL='CURRENT SURFACE "COBS RECTE" VALUES ARE:'
                  IF(ALENS(61,SURF).EQ.3.0D0)
     1            VAL='CURRENT SURFACE "COBS ELIPE" VALUES ARE:'
                  IF(ALENS(61,SURF).EQ.4.0D0)
     1            VAL='CURRENT SURFACE "COBS RCTKE" VALUES ARE:'
                  IF(ALENS(61,SURF).EQ.5.0D0)
     1            VAL='CURRENT SURFACE "COBS POLYE" VALUES ARE:'
                  IF(ALENS(61,SURF).EQ.6.0D0)
     1            VAL='CURRENT SURFACE "COBS IPOLYE" VALUES ARE:'
                  V1=1
                  VALUE1=ALENS(62,SURF)
                  V2=1
                  VALUE2=ALENS(63,SURF)
                  V3=1
                  VALUE3=ALENS(64,SURF)
                  V4=1
                  VALUE4=ALENS(65,SURF)
                  V5=1
                  VALUE5=ALENS(66,SURF)
                  V6=1
                  VALUE6=ALENS(67,SURF)
                  GO TO 200
              END IF
          ELSE
C       NOT COBS
          END IF
C       SOLVES
          IF(WC.EQ.'PY'.OR.WC.EQ.'PX'.OR.WC.EQ.'PCY'.OR.WC.EQ.'PCX'
     1    .OR.WC.EQ.'CAY'.OR.WC.EQ.'CAX'.OR.WC.EQ.'PUY'.OR.WC.EQ.'PUX'
     2    .OR.WC.EQ.'PUCY'.OR.WC.EQ.'PUCX'.OR.WC.EQ.'COCY'.OR.WC.EQ.
     3    'COCX'.OR.WC.EQ.'APY'.OR.WC.EQ.'APX'.OR.WC.EQ.'APCY'.OR.
     4    WC.EQ.'APCX'.OR.WC.EQ.'PIY'.OR.WC.EQ.'PIX'.OR.WC.EQ.
     5    'PICY'.OR.WC.EQ.'PICX') THEN
C
              IF(SOLVE(6,SURF).EQ.1.0D0) THEN
                  VA1=1
                  VAL='CURRENT SURFACE SOLVE IS A "PY" SOLVE WITH TARGET VALUE:'
                  V1=1
                  VALUE1=SOLVE(7,SURF)
                  GO TO 200
              ELSE
              END IF
              IF(SOLVE(6,SURF).EQ.2.0D0) THEN
                  VA1=1
                  VAL='CURRENT SURFACE SOLVE IS A "PCY" SOLVE WITH TARGET VALUE:'
                  V1=1
                  VALUE1=SOLVE(7,SURF)
                  GO TO 200
              ELSE
              END IF
              IF(SOLVE(6,SURF).EQ.3.0D0) THEN
                  VA1=1
                  VAL='CURRENT SURFACE SOLVE IS A "CAY" SOLVE WITH TARGET VALUE:'
                  V1=1
                  VALUE1=SOLVE(7,SURF)
                  GO TO 200
              ELSE
              END IF
C
              IF(SOLVE(4,SURF).EQ.4.0D0) THEN
                  VA1=1
                  VAL='CURRENT SURFACE SOLVE IS A "PX" SOLVE WITH TARGET VALUE:'
                  V1=1
                  VALUE1=SOLVE(3,SURF)
                  GO TO 200
              ELSE
              END IF
              IF(SOLVE(4,SURF).EQ.5.0D0) THEN
                  VA1=1
                  VAL='CURRENT SURFACE SOLVE IS A "PCX" SOLVE WITH TARGET VALUE:'
                  V1=1
                  VALUE1=SOLVE(3,SURF)
                  GO TO 200
              ELSE
              END IF
              IF(SOLVE(4,SURF).EQ.6.0D0) THEN
                  VA1=1
                  VAL='CURRENT SURFACE SOLVE IS A "CAX" SOLVE WITH TARGET VALUE:'
                  V1=1
                  VALUE1=SOLVE(3,SURF)
                  GO TO 200
              ELSE
              END IF
C
              IF(SOLVE(8,SURF).EQ.1.0D0) THEN
                  VA1=1
                  VAL='CURRENT SURFACE SOLVE IS AN "APY" SOLVE'
                  GO TO 200
              ELSE
              END IF
              IF(SOLVE(8,SURF).EQ.2.0D0) THEN
                  VA1=1
                  VAL='CURRENT SURFACE SOLVE IS A "PIY" SOLVE WITH TARGET VALUE:'
                  V1=1
                  VALUE1=SOLVE(9,SURF)
                  GO TO 200
              ELSE
              END IF
              IF(SOLVE(8,SURF).EQ.3.0D0) THEN
                  VA1=1
                  VAL='CURRENT SURFACE SOLVE IS A "PUY" SOLVE WITH TARGET VALUE:'
                  V1=1
                  VALUE1=SOLVE(9,SURF)
                  GO TO 200
              ELSE
              END IF
              IF(SOLVE(8,SURF).EQ.4.0D0) THEN
                  VA1=1
                  VAL='CURRENT SURFACE SOLVE IS AN "APCY" SOLVE'
                  GO TO 200
              ELSE
              END IF
              IF(SOLVE(8,SURF).EQ.5.0D0) THEN
                  VA1=1
                  VAL='CURRENT SURFACE SOLVE IS A "PICY" SOLVE WITH TARGET VALUE:'
                  V1=1
                  VALUE1=SOLVE(9,SURF)
                  GO TO 200
              ELSE
              END IF
              IF(SOLVE(8,SURF).EQ.6.0D0) THEN
                  VA1=1
                  VAL='CURRENT SURFACE SOLVE IS A "PUCY" SOLVE WITH TARGET VALUE:'
                  V1=1
                  VALUE1=SOLVE(9,SURF)
                  GO TO 200
              ELSE
              END IF
              IF(SOLVE(8,SURF).EQ.7.0D0) THEN
                  VA1=1
                  VAL='CURRENT SURFACE SOLVE IS A "COCY" SOLVE TO SURFACE #:'
                  V1=1
                  VALUE1=SOLVE(9,SURF)
                  GO TO 200
              ELSE
              END IF
C
              IF(SOLVE(2,SURF).EQ.8.0D0) THEN
                  VA1=1
                  VAL='CURRENT SURFACE SOLVE IS AN "APX" SOLVE'
                  GO TO 200
              ELSE
              END IF
              IF(SOLVE(2,SURF).EQ.9.0D0) THEN
                  VA1=1
                  VAL='CURRENT SURFACE SOLVE IS A "PIX" SOLVE WITH TARGET VALUE:'
                  V1=1
                  VALUE1=SOLVE(1,SURF)
                  GO TO 200
              ELSE
              END IF
              IF(SOLVE(2,SURF).EQ.10.0D0) THEN
                  VA1=1
                  VAL='CURRENT SURFACE SOLVE IS A "PUX" SOLVE WITH TARGET VALUE:'
                  V1=1
                  VALUE1=SOLVE(1,SURF)
                  GO TO 200
              ELSE
              END IF
              IF(SOLVE(2,SURF).EQ.11.0D0) THEN
                  VA1=1
                  VAL='CURRENT SURFACE SOLVE IS AN "APCX" SOLVE'
                  GO TO 200
              ELSE
              END IF
              IF(SOLVE(2,SURF).EQ.12.0D0) THEN
                  VA1=1
                  VAL='CURRENT SURFACE SOLVE IS A "PICX" SOLVE WITH TARGET VALUE:'
                  V1=1
                  VALUE1=SOLVE(1,SURF)
                  GO TO 200
              ELSE
              END IF
              IF(SOLVE(2,SURF).EQ.13.0D0) THEN
                  VA1=1
                  VAL='CURRENT SURFACE SOLVE IS A "PUCX" SOLVE WITH TARGET VALUE:'
                  V1=1
                  VALUE1=SOLVE(1,SURF)
                  GO TO 200
              ELSE
              END IF
              IF(SOLVE(2,SURF).EQ.14.0D0) THEN
                  VA1=1
                  VAL='CURRENT SURFACE SOLVE IS A "COCX" SOLVE TO SURFACE #:'
                  V1=1
                  VALUE1=SOLVE(1,SURF)
                  GO TO 200
              ELSE
              END IF
              IF(SOLVE(6,SURF).EQ.0.0D0.AND.SOLVE(4,SURF).EQ.0.0D0
     1        .AND.SOLVE(8,SURF).EQ.0.0D0.AND.SOLVE(2,SURF).EQ.0.0D0)THEN
                  VA1=1
                  VAL='CURRENT SURFACE HAS NO SOLVES'
                  GO TO 200
              ELSE
              END IF
          ELSE
C       NOT A SOLVE
          END IF
 300      CONTINUE
          IF(WC.EQ.'PIKUP') THEN
C       PIKUPS HERE
              DO I=1,PSIZ
                  IF(PIKUP(1,SURF,I).NE.0.0D0) REPEAT=REPEAT+1
              END DO
              IF(REPEAT.EQ.0) THEN
                  VA1=1
                  VAL='CURRENT SURFACE HAS NO PIKUPS DEFINED ON IT'
                  GO TO 200
              ELSE
C       THERE ARE PIKUPS, TAKE APPROPRIATE ACTION
                  DO I=1,PSIZ
                      IF(PIKUP(1,SURF,I).NE.0.0D0) THEN
C
                          IF(I.EQ.1) THEN
                              VA1=1
                              VAL='CURRENT SURFACE HAS AN "RD" PIKUP ON IT WITH VALUES:'
                              V1=1
                              V2=1
                              V3=1
                              V4=1
                              V5=1
                              VALUE1=PIKUP(2,SURF,I)
                              VALUE2=PIKUP(3,SURF,I)
                              VALUE3=PIKUP(4,SURF,I)
                              VALUE4=PIKUP(5,SURF,I)
                              VALUE5=PIKUP(6,SURF,I)
                              GO TO 200
                          ELSE
C       PROCEED TO NEXT PIKUP TYPE
                          END IF
C
                          IF(I.EQ.2) THEN
                              VA1=1
                              VAL='CURRENT SURFACE HAS AN "CV" PIKUP ON IT WITH VALUES:'
                              V1=1
                              V2=1
                              V3=1
                              V4=1
                              V5=1
                              VALUE1=PIKUP(2,SURF,I)
                              VALUE2=PIKUP(3,SURF,I)
                              VALUE3=PIKUP(4,SURF,I)
                              VALUE4=PIKUP(5,SURF,I)
                              VALUE5=PIKUP(6,SURF,I)
                              GO TO 200
                          ELSE
C       PROCEED TO NEXT PIKUP TYPE
                          END IF
C
                          IF(I.EQ.3) THEN
                              VA1=1
                              VAL='CURRENT SURFACE HAS AN "TH" PIKUP ON IT WITH VALUES:'
                              V1=1
                              V2=1
                              V3=1
                              V4=1
                              V5=1
                              VALUE1=PIKUP(2,SURF,I)
                              VALUE2=PIKUP(3,SURF,I)
                              VALUE3=PIKUP(4,SURF,I)
                              VALUE4=PIKUP(5,SURF,I)
                              VALUE5=PIKUP(6,SURF,I)
                              GO TO 200
                          ELSE
C       PROCEED TO NEXT PIKUP TYPE
                          END IF
C
                          IF(I.EQ.32) THEN
                              VA1=1
                              VAL='CURRENT SURFACE HAS AN "THOAL" PIKUP ON IT WITH VALUES:'
                              V1=1
                              V2=1
                              V3=1
                              V4=1
                              V5=1
                              VALUE1=PIKUP(2,SURF,I)
                              VALUE2=PIKUP(3,SURF,I)
                              VALUE3=PIKUP(4,SURF,I)
                              VALUE4=PIKUP(5,SURF,I)
                              VALUE5=PIKUP(6,SURF,I)
                              GO TO 200
                          ELSE
C       PROCEED TO NEXT PIKUP TYPE
                          END IF
C
                          IF(I.EQ.4) THEN
                              VA1=1
                              VAL='CURRENT SURFACE HAS AN "CC" PIKUP ON IT WITH VALUES:'
                              V1=1
                              V2=1
                              V3=1
                              V4=1
                              V5=1
                              VALUE1=PIKUP(2,SURF,I)
                              VALUE2=PIKUP(3,SURF,I)
                              VALUE3=PIKUP(4,SURF,I)
                              VALUE4=PIKUP(5,SURF,I)
                              VALUE5=PIKUP(6,SURF,I)
                              GO TO 200
                          ELSE
C       PROCEED TO NEXT PIKUP TYPE
                          END IF
C
                          IF(I.EQ.5) THEN
                              VA1=1
                              VAL='CURRENT SURFACE HAS AN "AD" PIKUP ON IT WITH VALUES:'
                              V1=1
                              V2=1
                              V3=1
                              V4=1
                              V5=1
                              VALUE1=PIKUP(2,SURF,I)
                              VALUE2=PIKUP(3,SURF,I)
                              VALUE3=PIKUP(4,SURF,I)
                              VALUE4=PIKUP(5,SURF,I)
                              VALUE5=PIKUP(6,SURF,I)
                              GO TO 200
                          ELSE
C       PROCEED TO NEXT PIKUP TYPE
                          END IF
C
                          IF(I.EQ.6) THEN
                              VA1=1
                              VAL='CURRENT SURFACE HAS AN "AE" PIKUP ON IT WITH VALUES:'
                              V1=1
                              V2=1
                              V3=1
                              V4=1
                              V5=1
                              VALUE1=PIKUP(2,SURF,I)
                              VALUE2=PIKUP(3,SURF,I)
                              VALUE3=PIKUP(4,SURF,I)
                              VALUE4=PIKUP(5,SURF,I)
                              VALUE5=PIKUP(6,SURF,I)
                              GO TO 200
                          ELSE
C       PROCEED TO NEXT PIKUP TYPE
                          END IF
C
                          IF(I.EQ.7) THEN
                              VA1=1
                              VAL='CURRENT SURFACE HAS AN "AF" PIKUP ON IT WITH VALUES:'
                              V1=1
                              V2=1
                              V3=1
                              V4=1
                              V5=1
                              VALUE1=PIKUP(2,SURF,I)
                              VALUE2=PIKUP(3,SURF,I)
                              VALUE3=PIKUP(4,SURF,I)
                              VALUE4=PIKUP(5,SURF,I)
                              VALUE5=PIKUP(6,SURF,I)
                              GO TO 200
                          ELSE
C       PROCEED TO NEXT PIKUP TYPE
                          END IF
C
                          IF(I.EQ.8) THEN
                              VA1=1
                              VAL='CURRENT SURFACE HAS AN "AG" PIKUP ON IT WITH VALUES:'
                              V1=1
                              V2=1
                              V3=1
                              V4=1
                              V5=1
                              VALUE1=PIKUP(2,SURF,I)
                              VALUE2=PIKUP(3,SURF,I)
                              VALUE3=PIKUP(4,SURF,I)
                              VALUE4=PIKUP(5,SURF,I)
                              VALUE5=PIKUP(6,SURF,I)
                              GO TO 200
                          ELSE
C       PROCEED TO NEXT PIKUP TYPE
                          END IF
C
                          IF(I.EQ.9) THEN
                              VA1=1
                              VAL='CURRENT SURFACE HAS AN "CVTOR" PIKUP ON IT WITH VALUES:'
                              V1=1
                              V2=1
                              V3=1
                              V4=1
                              V5=1
                              VALUE1=PIKUP(2,SURF,I)
                              VALUE2=PIKUP(3,SURF,I)
                              VALUE3=PIKUP(4,SURF,I)
                              VALUE4=PIKUP(5,SURF,I)
                              VALUE5=PIKUP(6,SURF,I)
                              GO TO 200
                          ELSE
C       PROCEED TO NEXT PIKUP TYPE
                          END IF
C
                          IF(I.EQ.10) THEN
                              VA1=1
                              VAL='CURRENT SURFACE HAS AN "RDTOR" PIKUP ON IT WITH VALUES:'
                              V1=1
                              V2=1
                              V3=1
                              V4=1
                              V5=1
                              VALUE1=PIKUP(2,SURF,I)
                              VALUE2=PIKUP(3,SURF,I)
                              VALUE3=PIKUP(4,SURF,I)
                              VALUE4=PIKUP(5,SURF,I)
                              VALUE5=PIKUP(6,SURF,I)
                              GO TO 200
                          ELSE
C       PROCEED TO NEXT PIKUP TYPE
                          END IF
C
                          IF(I.EQ.11) THEN
                              VA1=1
                              VAL='CURRENT SURFACE HAS AN "PRO" PIKUP ON IT WITH VALUES:'
                              V1=1
                              V2=1
                              V3=1
                              V4=1
                              V5=1
                              VALUE1=PIKUP(2,SURF,I)
                              VALUE2=PIKUP(3,SURF,I)
                              VALUE3=PIKUP(4,SURF,I)
                              VALUE4=PIKUP(5,SURF,I)
                              VALUE5=PIKUP(6,SURF,I)
                              GO TO 200
                          ELSE
C       PROCEED TO NEXT PIKUP TYPE
                          END IF
C
                          IF(I.EQ.12) THEN
                              VA1=1
                              VAL='CURRENT SURFACE HAS AN "NPRO" PIKUP ON IT WITH VALUES:'
                              V1=1
                              V2=1
                              V3=1
                              V4=1
                              V5=1
                              VALUE1=PIKUP(2,SURF,I)
                              VALUE2=PIKUP(3,SURF,I)
                              VALUE3=PIKUP(4,SURF,I)
                              VALUE4=PIKUP(5,SURF,I)
                              VALUE5=PIKUP(6,SURF,I)
                              GO TO 200
                          ELSE
C       PROCEED TO NEXT PIKUP TYPE
                          END IF
C
                          IF(I.EQ.13) THEN
                              VA1=1
                              VAL='CURRENT SURFACE HAS AN "YD" PIKUP ON IT WITH VALUES:'
                              V1=1
                              V2=1
                              V3=1
                              V4=1
                              V5=1
                              VALUE1=PIKUP(2,SURF,I)
                              VALUE2=PIKUP(3,SURF,I)
                              VALUE3=PIKUP(4,SURF,I)
                              VALUE4=PIKUP(5,SURF,I)
                              VALUE5=PIKUP(6,SURF,I)
                              GO TO 200
                          ELSE
C       PROCEED TO NEXT PIKUP TYPE
                          END IF
C
                          IF(I.EQ.14) THEN
                              VA1=1
                              VAL='CURRENT SURFACE HAS AN "XD" PIKUP ON IT WITH VALUES:'
                              V1=1
                              V2=1
                              V3=1
                              V4=1
                              V5=1
                              VALUE1=PIKUP(2,SURF,I)
                              VALUE2=PIKUP(3,SURF,I)
                              VALUE3=PIKUP(4,SURF,I)
                              VALUE4=PIKUP(5,SURF,I)
                              VALUE5=PIKUP(6,SURF,I)
                              GO TO 200
                          ELSE
C       PROCEED TO NEXT PIKUP TYPE
                          END IF
C
                          IF(I.EQ.15) THEN
                              VA1=1
                              VAL='CURRENT SURFACE HAS AN "ALPHA" PIKUP ON IT WITH VALUES:'
                              V1=1
                              V2=1
                              V3=1
                              V4=1
                              V5=1
                              VALUE1=PIKUP(2,SURF,I)
                              VALUE2=PIKUP(3,SURF,I)
                              VALUE3=PIKUP(4,SURF,I)
                              VALUE4=PIKUP(5,SURF,I)
                              VALUE5=PIKUP(6,SURF,I)
                              GO TO 200
                          ELSE
C       PROCEED TO NEXT PIKUP TYPE
                          END IF
C
                          IF(I.EQ.16) THEN
                              VA1=1
                              VAL='CURRENT SURFACE HAS AN "BETA" PIKUP ON IT WITH VALUES:'
                              V1=1
                              V2=1
                              V3=1
                              V4=1
                              V5=1
                              VALUE1=PIKUP(2,SURF,I)
                              VALUE2=PIKUP(3,SURF,I)
                              VALUE3=PIKUP(4,SURF,I)
                              VALUE4=PIKUP(5,SURF,I)
                              VALUE5=PIKUP(6,SURF,I)
                              GO TO 200
                          ELSE
C       PROCEED TO NEXT PIKUP TYPE
                          END IF
C
                          IF(I.EQ.17) THEN
                              VA1=1
                              VAL='CURRENT SURFACE HAS AN "GAMMA" PIKUP ON IT WITH VALUES:'
                              V1=1
                              V2=1
                              V3=1
                              V4=1
                              V5=1
                              VALUE1=PIKUP(2,SURF,I)
                              VALUE2=PIKUP(3,SURF,I)
                              VALUE3=PIKUP(4,SURF,I)
                              VALUE4=PIKUP(5,SURF,I)
                              VALUE5=PIKUP(6,SURF,I)
                              GO TO 200
                          ELSE
C       PROCEED TO NEXT PIKUP TYPE
                          END IF
C
                          IF(I.EQ.18) THEN
                              VA1=1
                              VAL='CURRENT SURFACE HAS AN "CLAP" PIKUP ON IT WITH VALUES:'
                              V1=1
                              V2=1
                              V3=1
                              V4=1
                              V5=1
                              VALUE1=PIKUP(2,SURF,I)
                              VALUE2=PIKUP(3,SURF,I)
                              VALUE3=PIKUP(4,SURF,I)
                              VALUE4=PIKUP(5,SURF,I)
                              VALUE5=PIKUP(6,SURF,I)
                              GO TO 200
                          ELSE
C       PROCEED TO NEXT PIKUP TYPE
                          END IF
C
                          IF(I.EQ.19) THEN
                              VA1=1
                              VAL='CURRENT SURFACE HAS AN "COBS" PIKUP ON IT WITH VALUES:'
                              V1=1
                              V2=1
                              V3=1
                              V4=1
                              V5=1
                              VALUE1=PIKUP(2,SURF,I)
                              VALUE2=PIKUP(3,SURF,I)
                              VALUE3=PIKUP(4,SURF,I)
                              VALUE4=PIKUP(5,SURF,I)
                              VALUE5=PIKUP(6,SURF,I)
                              GO TO 200
                          ELSE
C       PROCEED TO NEXT PIKUP TYPE
                          END IF
C
                          IF(I.EQ.20) THEN
                              VA1=1
                              VAL='CURRENT SURFACE HAS AN "GLASS" PIKUP ON IT WITH VALUES:'
                              V1=1
                              V2=1
                              V3=1
                              V4=1
                              V5=1
                              VALUE1=PIKUP(2,SURF,I)
                              VALUE2=PIKUP(3,SURF,I)
                              VALUE3=PIKUP(4,SURF,I)
                              VALUE4=PIKUP(5,SURF,I)
                              VALUE5=PIKUP(6,SURF,I)
                              GO TO 200
                          ELSE
C       PROCEED TO NEXT PIKUP TYPE
                          END IF
C
                          IF(I.EQ.21) THEN
                              VA1=1
                              VAL='CURRENT SURFACE HAS AN "CCTOR" PIKUP ON IT WITH VALUES:'
                              V1=1
                              V2=1
                              V3=1
                              V4=1
                              V5=1
                              VALUE1=PIKUP(2,SURF,I)
                              VALUE2=PIKUP(3,SURF,I)
                              VALUE3=PIKUP(4,SURF,I)
                              VALUE4=PIKUP(5,SURF,I)
                              VALUE5=PIKUP(6,SURF,I)
                              GO TO 200
                          ELSE
C       PROCEED TO NEXT PIKUP TYPE
                          END IF
C
                          IF(I.EQ.22) THEN
                              VA1=1
                              VAL='CURRENT SURFACE HAS AN "ADTOR" PIKUP ON IT WITH VALUES:'
                              V1=1
                              V2=1
                              V3=1
                              V4=1
                              V5=1
                              VALUE1=PIKUP(2,SURF,I)
                              VALUE2=PIKUP(3,SURF,I)
                              VALUE3=PIKUP(4,SURF,I)
                              VALUE4=PIKUP(5,SURF,I)
                              VALUE5=PIKUP(6,SURF,I)
                              GO TO 200
                          ELSE
C       PROCEED TO NEXT PIKUP TYPE
                          END IF
C
                          IF(I.EQ.23) THEN
                              VA1=1
                              VAL='CURRENT SURFACE HAS AN "AETOR" PIKUP ON IT WITH VALUES:'
                              V1=1
                              V2=1
                              V3=1
                              V4=1
                              V5=1
                              VALUE1=PIKUP(2,SURF,I)
                              VALUE2=PIKUP(3,SURF,I)
                              VALUE3=PIKUP(4,SURF,I)
                              VALUE4=PIKUP(5,SURF,I)
                              VALUE5=PIKUP(6,SURF,I)
                              GO TO 200
                          ELSE
C       PROCEED TO NEXT PIKUP TYPE
                          END IF
C
                          IF(I.EQ.24) THEN
                              VA1=1
                              VAL='CURRENT SURFACE HAS AN "AFTOR" PIKUP ON IT WITH VALUES:'
                              V1=1
                              V2=1
                              V3=1
                              V4=1
                              V5=1
                              VALUE1=PIKUP(2,SURF,I)
                              VALUE2=PIKUP(3,SURF,I)
                              VALUE3=PIKUP(4,SURF,I)
                              VALUE4=PIKUP(5,SURF,I)
                              VALUE5=PIKUP(6,SURF,I)
                              GO TO 200
                          ELSE
C       PROCEED TO NEXT PIKUP TYPE
                          END IF
C
                          IF(I.EQ.25) THEN
                              VA1=1
                              VAL='CURRENT SURFACE HAS AN "AGTOR" PIKUP ON IT WITH VALUES:'
                              V1=1
                              V2=1
                              V3=1
                              V4=1
                              V5=1
                              VALUE1=PIKUP(2,SURF,I)
                              VALUE2=PIKUP(3,SURF,I)
                              VALUE3=PIKUP(4,SURF,I)
                              VALUE4=PIKUP(5,SURF,I)
                              VALUE5=PIKUP(6,SURF,I)
                              GO TO 200
                          ELSE
C       PROCEED TO NEXT PIKUP TYPE
                          END IF
C
                          IF(I.EQ.26) THEN
                              VA1=1
                              VAL='CURRENT SURFACE HAS AN "AC" PIKUP ON IT WITH VALUES:'
                              V1=1
                              V2=1
                              V3=1
                              V4=1
                              V5=1
                              VALUE1=PIKUP(2,SURF,I)
                              VALUE2=PIKUP(3,SURF,I)
                              VALUE3=PIKUP(4,SURF,I)
                              VALUE4=PIKUP(5,SURF,I)
                              VALUE5=PIKUP(6,SURF,I)
                              GO TO 200
                          ELSE
C       PROCEED TO NEXT PIKUP TYPE
                          END IF
C
                          IF(I.GT.26) THEN
C                 DO NOTHING
                          ELSE
                          END IF
                      ELSE
C       PROCEED BY INCREMENTING I BY 1
                      END IF
                  END DO
              END IF
          ELSE
C       WC NOT PIKUP
          END IF
          RETURN
C
 200      CALL LQAS(VALUE1,VALUE2,VALUE3,VALUE4,VALUE5,VAL,
     1    V1,V2,V3,V4,V5,VA1,V6,VALUE6,VA1WS1,VA1WS2,VA1WS3,VA1WS4
     2    ,VALWS1,VALWS2,VALWS3,VALWS4,REPEAT)
C
          IF(REPEAT.NE.0) GO TO 300
          RETURN
      END
C SUB ULENNS.FOR
      SUBROUTINE ULENNS
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE ULENNS. THIS IS THE SUBROUTINE WHICH
C       INITIALLY STARTS THE LENS  UPDATE OR CHANGE PROCEDURE.
C       THE CMD LEVEL IS DISABLED AND FLAG F6 IS SET TO 1.
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
C
          IF(SN.EQ.1.OR.SST.EQ.1) THEN
              OUTLYNE=
     1        '"UPDATE LENS" TAKES NO NUMERIC OR STRING INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
          F1=0
          F6=1
C       INITIALIZE REFERENCE RAY LOGICAL TO .FALSE.
C       AND NULL TO FALSE
C
          IF(RAYCLEAR) THEN
              REFEXT=.FALSE.
              CPFNEXT=.FALSE.
              SPDEXT=.FALSE.
              GSPDEXT=.FALSE.
              CALL DELPSF
              FOBYES=.FALSE.
              NULL=.FALSE.
              RAYEXT=.FALSE.
              POLEXT=.FALSE.
              FAIL=.TRUE.
          END IF
C
C       SET SURF=0
          SURF=0
          IF(F15.EQ.0.AND.F12.NE.1) THEN
C       NOT AT CONFIGS OR UPDATE CONFIGS, ONLY CHANGE CFG 1
C
C       NOW COPY THE PERMANENT LENS BACK TO THE CURRENT LENS
C       DATA

              CALL PTOC
C
C               FINISHED RETURNING TO CFG 1
C
              F12=1
C
C       F12 MAY BE ANY VALUE FROM 2 TO 100
          END IF
          RETURN
      END
C SUB THSOLV.FOR
      SUBROUTINE THSOLV
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE THSOLV WHICH IMPLEMENTS THE
C       THICKNESS SOLVES AND DELETIONS
C       COMMAND AT THE LENS INPUT AND UPDATE LENS LEVEL.
C
          INTEGER PIKCNT,I,SF
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
          IF(WC.EQ.'PY'.OR.WC.EQ.'PX'.OR.WC.EQ.'PCY'.OR.WC.EQ.'PCX'
     1    .OR.WC.EQ.'CAY'.OR.WC.EQ.'CAX') THEN
C       COMMANDS WHICH MIGHT HAVE QUERRY
              IF(STI.EQ.1) THEN
C
                  IF(WC.EQ.'PY') THEN
                      IF(SOLVE(6,SURF).EQ.1.0D0)
     1                WRITE(OUTLYNE,101)SOLVE(7,SURF),SURF
 101                  FORMAT('"PY" = ',G23.15,' AT SURFACE #',I3)
                  END IF
C
                  IF(WC.EQ.'PX') THEN
                      IF(SOLVE(4,SURF).EQ.4.0D0)
     1                WRITE(OUTLYNE,102)SOLVE(3,SURF),SURF
 102                  FORMAT('"PX" = ',G23.15,' AT SURFACE #',I3)
                  END IF
C
                  IF(WC.EQ.'PCY') THEN
                      IF(SOLVE(6,SURF).EQ.2.0D0)
     1                WRITE(OUTLYNE,103)SOLVE(7,SURF),SURF
 103                  FORMAT('"PCY" = ',G23.15,' AT SURFACE #',I3)
                  END IF
C
                  IF(WC.EQ.'PCX') THEN
                      IF(SOLVE(4,SURF).EQ.5.0D0)
     1                WRITE(OUTLYNE,104)SOLVE(3,SURF),SURF
 104                  FORMAT('"PCX" = ',G23.15,' AT SURFACE #',I3)
                  END IF
C
                  IF(WC.EQ.'CAY') THEN
                      IF(SOLVE(6,SURF).EQ.3.0D0)
     1                WRITE(OUTLYNE,105)SOLVE(7,SURF),SURF
 105                  FORMAT('"CAY" = ',G23.15,' AT SURFACE #',I3)
                  END IF
C
                  IF(WC.EQ.'CAX') THEN
                      IF(SOLVE(4,SURF).EQ.6.0D0)
     1                WRITE(OUTLYNE,106)SOLVE(3,SURF),SURF
 106                  FORMAT('"CAX" = ',G23.15,' AT SURFACE #',I3)
                  END IF
                  CALL SHOWIT(0)
C
                  RETURN
              END IF
          END IF
C
C               CHECK FOR PRESENCE OF QUALIFIER OR STRING INPUT
C               PRINT ERROR AND RETURN IF DISCOVERED.
C
          IF(WC.NE.'TSD') THEN
              IF(SQ.EQ.1.OR.SST.EQ.1.OR.S2.EQ.1.OR.S3.EQ.1.OR.
     1        S4.EQ.1.OR.S5.EQ.1) THEN
C
                  IF(WC.EQ.'PY') THEN
                      OUTLYNE='"PY" ONLY ACCEPTS NUMERIC WORD #1 INPUT'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  IF(WC.EQ.'PX') THEN
                      OUTLYNE='"PX" ONLY ACCEPTS NUMERIC WORD #1 INPUT'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  IF(WC.EQ.'PCY') THEN
                      OUTLYNE='"PCY" ONLY ACCEPTS NUMERIC WORD #1 INPUT'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  IF(WC.EQ.'PCX') THEN
                      OUTLYNE='"PCX" ONLY ACCEPTS NUMERIC WORD #1 INPUT'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  IF(WC.EQ.'CAY') THEN
                      OUTLYNE='"CAY" ONLY ACCEPTS NUMERIC WORD #1 INPUT'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  IF(WC.EQ.'CAX') THEN
                      OUTLYNE='"CAX" ONLY ACCEPTS NUMERIC WORD #1 INPUT'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
C
              IF(DF1.EQ.1) W1=0.0D0
C
C               WE ARE AT LENS INPUT OR LENS UPDATE LEVEL
C
C       CHECK THAT THICKNESS SOLVES NOT ASSIGNED TO OBJECT SURF
C
              IF(WC.EQ.'PCY'.OR.WC.EQ.'PCX') THEN
                  IF(SURF.LT.1.AND.SYSTEM1(26).EQ.-99.0D0.OR.SURF.LT.
     1            INT(SYSTEM1(26)).AND.SYSTEM1(26).NE.-99.0D0)THEN
                      OUTLYNE='             CHIEF RAY SOLVES ARE NOT ALLOWED'
                      CALL SHOWIT(1)
                      OUTLYNE='                       OBJECT SURFACE'
                      CALL SHOWIT(1)
                      OUTLYNE='                            OR'
                      CALL SHOWIT(1)
                      OUTLYNE='              BEFORE THE APERTURE STOP SURFACE.'
                      CALL SHOWIT(1)
                      OUTLYNE='                      RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
C
              IF(WC.EQ.'PY') SOLVE(6,SURF)=1.0D0
              IF(WC.EQ.'PCY') SOLVE(6,SURF)=2.0D0
              IF(WC.EQ.'CAY') SOLVE(6,SURF)=3.0D0
              IF(WC.EQ.'PY'.OR.WC.EQ.'PCY'.OR.WC.EQ.'CAY') THEN
                  SOLVE(7,SURF)=W1
                  SOLVE(4,SURF)=0.0D0
                  SOLVE(3,SURF)=0.0D0
              ELSE
              END IF
C
C       CHECK FOR TORICS, NO XZ SOLVES GO ON NON-TORICS
C
              IF(WC.EQ.'PX'.OR.WC.EQ.'PCX'.OR.WC.EQ.'CAX') THEN
                  IF(ALENS(23,SURF).EQ.0.0D0) THEN
C       CAN'T DO SOLVE,SURF NOT TORIC
                      OUTLYNE=
     1                'XZ THICHNESS SOLVES VALID ONLY FOR TORIC SURFACES'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'XZ THICKNESS SOLVE NOT ASSIGNED FOR SURFACE',SURF
                      CALL SHOWIT(1)
                      SOLVE(4,SURF)=0.0D0
                      SOLVE(3,SURF)=0.0D0
                  ELSE
C       SURFACE IS TORIC
C
                      IF(WC.EQ.'PX') SOLVE(4,SURF)=4.0D0
                      IF(WC.EQ.'PCX') SOLVE(4,SURF)=5.0D0
                      IF(WC.EQ.'CAX') SOLVE(4,SURF)=6.0D0
                      IF(WC.EQ.'PX'.OR.WC.EQ.'PCX'.OR.WC.EQ.'CAX') THEN
                          SOLVE(3,SURF)=W1
                          SOLVE(6,SURF)=0.0D0
                          SOLVE(7,SURF)=0.0D0
                      ELSE
                      END IF
                  END IF
              ELSE
              END IF
C
C       CALCULATE THE CORRECT VALUE OF ALENS(33,SURF)
C
              ALENS(33,SURF)=0.0D0
              IF(SOLVE(6,SURF).GT.0.0D0) ALENS(33,SURF)=ALENS(33,SURF)+1.0D0
              IF(SOLVE(4,SURF).GT.0.0D0) ALENS(33,SURF)=ALENS(33,SURF)+0.1D0
              IF(SOLVE(8,SURF).GT.0.0D0) ALENS(33,SURF)=ALENS(33,SURF)+2.0D0
              IF(SOLVE(2,SURF).GT.0.0D0) ALENS(33,SURF)=ALENS(33,SURF)+0.2D0

C       IF THERE IS A THICKNESS PIKUP ON THIS SURFACE,
C       DELETE IT.
C       CHECK FOR ANY PIKUPS
              IF(ALENS(32,SURF).EQ.0.0D0) THEN
C       NO PIKUPS, DON'T DO ANYTHING
              ELSE
C       THERE ARE PIKUPS. ARE THERE ANY THICKNESS PIKUPS.
                  IF(PIKUP(1,SURF,3).EQ.0.0D0.AND.PIKUP(1,SURF,32).EQ.0.0D0) THEN
C       NO THICKNESS PIKUPS
                  ELSE
C
C       THERE IS A THICKNESS PIKUP,DELETE IT AND IF THERE ARE NO
C       OTHER PIKUPS THEN SET THE PIKUP INDICATOR ALENS(32,SURF) TO
C       ZERO.
C
                      IF(PIKUP(1,SURF,3).NE.0.0D0) THEN
                          PIKUP(1:6,SURF,3)=0.0D0
                      END IF
                      IF(PIKUP(1,SURF,32).NE.0.0D0) THEN
                          PIKUP(1:6,SURF,32)=0.0D0
                      END IF
                      WRITE(OUTLYNE,*)'SURFACE',SURF,' :THICKNESS PIKUP DELETED'
                      CALL SHOWIT(1)
C
C       NOW CHECK FOR OTHER PIKUPS. IF NOT FOUND,SET
C       ALENS(32,SURF) TO ZERO,ELSE LEAVE IT AT 1.0D0
C
                      PIKCNT=0
                      DO 15 I=1,PSIZ
                          IF(PIKUP(1,SURF,I).NE.0.0D0) THEN
                              PIKCNT=PIKCNT+1
                          ELSE
                          END IF
 15                   CONTINUE
                      IF(PIKCNT.EQ.0) ALENS(32,SURF)=0.0D0
C       THERE ARE NOW ONE LESS PIKUPS THAN BEFORE THE SOLVE
C       WAS DEFINED.
                  END IF
              END IF
              RETURN
          END IF
C
          IF(WC.EQ.'TSD') THEN
              IF(SST.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
                  OUTLYNE=
     1            '"TSD" TAKES NO STRING OR QUALIFIER'
                  CALL SHOWIT(1)
                  OUTLYNE=
     1            'OR NUMERIC WORD #3 THROUGH #5 INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(SQ.EQ.1.AND.SN.EQ.1) THEN
                  OUTLYNE=
     1            '"TSD" TAKES EITHER QUALIFIER OR NUMERIC INPUT, NOT BOTH'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(SQ.EQ.1.AND.WQ.NE.'ALL     ') THEN
                  OUTLYNE=
     1            '"TSD" ONLY ACCEPTS "ALL" AS A VAILID QUALIFIER'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(WQ.EQ.'ALL') THEN
                  W1=0.0D0
                  W2=SYSTEM1(20)
                  S1=1
                  S2=1
                  DF1=0
                  DF2=0
                  SN=1
              END IF
              IF(SQ.EQ.0.AND.DF1.EQ.1.AND.DF2.EQ.1) THEN
                  W1=DBLE(SURF)
                  W2=DBLE(SURF)
                  S1=1
                  S2=1
                  DF1=0
                  DF2=0
                  SN=1
              END IF
              IF(DF1.EQ.1.AND.DF2.EQ.0.OR.DF1.EQ.0.AND.DF2.EQ.1) THEN
                  OUTLYNE=
     1            '"TSD" USES EITHER TWO OR ZERO NUMERIC WORDS OR QUALIFIER INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(INT(W1).LT.0) THEN
                  OUTLYNE=
     1            'STARTING SURFACE NUMBER MUST BE GREATER THAN OR EQUAL TO 0'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(INT(W2).GT.INT(SYSTEM1(20))) THEN
                  WRITE(OUTLYNE,*)
     1            'ENDING SURFACE NUMBER MUST BE LESS THAN OR EQUAL TO ',
     1            INT(SYSTEM1(20))
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(W1.GT.W2) THEN
                  OUTLYNE=
     1            'THE ENDING SURFACE # MUST BE GREATER THAN OR EQUAL TO#'
                  CALL SHOWIT(1)
                  OUTLYNE=
     1            'THE STARTING SURFACE #'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              DO SF=INT(W1),INT(W2)
                  IF(SF.EQ.0) THEN
                      OUTLYNE='OBJECT SURFACE NEVER HAS SOLVES'
                      CALL SHOWIT(1)
                      GO TO 900
                  END IF
C
                  IF(SOLVE(6,SF).NE.0.0D0.OR.
     1            SOLVE(4,SF).NE.0.0D0.OR.SOLVE(6,SF).NE.0.0D0
     2            ) THEN
                      SOLVE(3:7,SF)=0.0D0
                      WRITE(OUTLYNE,*)
     1                'SURFACE',SF,' :EXISTING THICKNESS SOLVE DELETED'
                      CALL SHOWIT(1)
                  ELSE
                      WRITE(OUTLYNE,*)
     1                'SURFACE',SF,' :NO THICKNESS SOLVE TO DELETE'
                      CALL SHOWIT(1)
                  END IF
C
C       RE-COMPUTE ALENS(33,SF)
C
                  ALENS(33,SF)=0.0D0
                  IF(SOLVE(6,SF).GT.0.0D0) ALENS(33,SF)=ALENS(33,SF)+1.0D0
                  IF(SOLVE(4,SF).GT.0.0D0) ALENS(33,SF)=ALENS(33,SF)+0.1D0
                  IF(SOLVE(8,SF).GT.0.0D0) ALENS(33,SF)=ALENS(33,SF)+2.0D0
                  IF(SOLVE(2,SF).GT.0.0D0) ALENS(33,SF)=ALENS(33,SF)+0.2D0
 900              CONTINUE
              END DO
              RETURN
          ELSE
C       NOT TSD
          END IF
          RETURN
      END



C SUB THERMGAS.FOR
      SUBROUTINE THERMGAS
C
          IMPLICIT NONE
C
          INTEGER I
C
          LOGICAL GONOGO
C
          REAL*8 DSGN,DUMA,DUMB,DUML,LM1,LA1,LA2,LA3
     1    ,LA4,LA5,A,B,LA6,LA7,LA8,LA9,LA10
C
          EXTERNAL DSGN
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
C       LM1
          LM1(DUMA,DUMB,DUML)=DUMA*(1.0D0+(DUMB/(DUML**2)))
c
          IF(SST.EQ.1) THEN
              WRITE(OUTLYNE,*)'"THERM" TAKES NO STRING INPUT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
          IF(STI.EQ.1) THEN
              WRITE(OUTLYNE,*)'NO ADDITIONAL INFORMATION'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
          IF(S5.EQ.1.AND.WQ.NE.'GAS'.OR.S4.EQ.1.AND.WQ.NE.'GAS') THEN
              WRITE(OUTLYNE,*)
     1        '"THERM GAS" TAKES NO NUMERIC WORD #4 OR #5 INPUT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
          IF(S5.EQ.1) THEN
              WRITE(OUTLYNE,*)'"THERM" TAKES NO NUMERIC WORD #5 INPUT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
          IF(DF1.EQ.1) W1=0.0D0
          IF(DF2.EQ.1) W2=SYSTEM1(20)
          IF(WQ.EQ.'GAS') THEN
              IF(DF3.EQ.1.OR.DF4.EQ.1) THEN
                  WRITE(OUTLYNE,*)
     1            '"THERM GAS" REQUIRES EXPLICIT NUMERIC WORDS #3 AND #4'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          ELSE
              IF(DF3.EQ.1) THEN
                  WRITE(OUTLYNE,*)'"THERM" REQUIRES EXPLICIT NUMERIC WORD #3'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
          IF(W1.LT.0.0D0) THEN
              WRITE(OUTLYNE,*)'STARTING SURFACE NUMBER MUST BE 0 OR GREATER'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(W2.GT.SYSTEM1(20)) THEN
              WRITE(OUTLYNE,*)
     1        'ENDING SURFACE NUMBER MUST BE LESS THAN',INT(SYSTEM1(20)+1.0D0)
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(W2.LT.W1) THEN
              WRITE(OUTLYNE,*)
     1        'ERROR:'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)
     1        'ENDING SURFACE NUMBER LESS THAN STARTING SURFACE NUMBER'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
C DO THE THERM OPERATION
          IF(WQ.EQ.'GAS') THEN
              IF(INT(W1).EQ.INT(W2)) THEN
                  I=INT(W1)
                  CALL CHKGLS(GONOGO,I)
                  IF(.NOT.GONOGO) THEN
                      RETURN
                  END IF
                  IF(GLANAM(I,2).EQ.'REFL         '.OR.
     1            GLANAM(I,2).EQ.'REFLTIRO     '.OR.
     2            GLANAM(I,2).EQ.'REFLTIR      ') THEN
                      WRITE(OUTLYNE,*)
     1                '"THERM GAS" DID NOT MODIFY "REFL" SURFACE # ',I
                      CALL SHOWIT(1)
                      RETURN
                  END IF
                  IF(GLANAM(I,2).EQ.'PERFECT      ') THEN
                      WRITE(OUTLYNE,*)
     1                '"THERM GAS" DID NOT MODIFY "PERFECT" SURFACE # ',I
                      CALL SHOWIT(1)
                      RETURN
                  END IF
                  IF(GLANAM(I,2).EQ.'IDEAL        ') THEN
                      WRITE(OUTLYNE,*)
     1                '"THERM GAS" DID NOT MODIFY "IDEAL" SURFACE # ',I
                      CALL SHOWIT(1)
                      RETURN
                  END IF
                  IF(GLANAM(I,2).NE.'REFL         '.AND.
     1            GLANAM(I,2).NE.'PERFECT      '.AND.
     1            GLANAM(I,2).NE.'REFLTIR      '.AND.
     1            GLANAM(I,2).NE.'REFLTIRO     '.AND.
     1            GLANAM(I,2).NE.'IDEAL        ') THEN
                      IF(DABS(ALENS(46,I)).LE.1.1D0.AND.DABS(ALENS(47,I)).LE.1.1D0
     1                .OR.DABS(ALENS(48,I)).LE.1.1D0.AND.DABS(ALENS(49,I)).LE.1.1D0
     2                .AND.DABS(ALENS(50,I)).LE.1.1D0) THEN
                          GLANAM(I,1)='GLASS      '
                          IF(SYSTEM1(1).NE.0.0D0)
     1                    ALENS(46,I)=ALENS(46,I)+(DSGN(ALENS(46,I))*W3*W4)
                          IF(SYSTEM1(2).NE.0.0D0)
     1                    ALENS(47,I)=ALENS(47,I)+(DSGN(ALENS(47,I))*W3*W4)
                          IF(SYSTEM1(3).NE.0.0D0)
     1                    ALENS(48,I)=ALENS(48,I)+(DSGN(ALENS(48,I))*W3*W4)
                          IF(SYSTEM1(4).NE.0.0D0)
     1                    ALENS(49,I)=ALENS(49,I)+(DSGN(ALENS(49,I))*W3*W4)
                          IF(SYSTEM1(5).NE.0.0D0)
     1                    ALENS(50,I)=ALENS(50,I)+(DSGN(ALENS(50,I))*W3*W4)
                          GLANAM(I,1)(1:13)='GLASS'
                          GLANAM(I,2)(1:13)='GAS'
                      END IF
                      IF(DABS(ALENS(71,I)).LE.1.1D0.AND.DABS(ALENS(72,I)).LE.1.1D0
     1                .OR.DABS(ALENS(73,I)).LE.1.1D0.AND.DABS(ALENS(74,I)).LE.1.1D0
     2                .AND.DABS(ALENS(75,I)).LE.1.1D0) THEN
                          IF(SYSTEM1(71).NE.0.0D0)
     1                    ALENS(71,I)=ALENS(71,I)+(DSGN(ALENS(71,I))*W3*W4)
                          IF(SYSTEM1(72).NE.0.0D0)
     1                    ALENS(72,I)=ALENS(72,I)+(DSGN(ALENS(72,I))*W3*W4)
                          IF(SYSTEM1(73).NE.0.0D0)
     1                    ALENS(73,I)=ALENS(73,I)+(DSGN(ALENS(73,I))*W3*W4)
                          IF(SYSTEM1(74).NE.0.0D0)
     1                    ALENS(74,I)=ALENS(74,I)+(DSGN(ALENS(74,I))*W3*W4)
                          IF(SYSTEM1(75).NE.0.0D0)
     1                    ALENS(75,I)=ALENS(75,I)+(DSGN(ALENS(75,I))*W3*W4)
                          GLANAM(I,1)(1:13)='GLASS'
                          GLANAM(I,2)(1:13)='GAS'
                      END IF
                  END IF
              ELSE
                  DO I=INT(W1),INT(W2)
                      CALL CHKGLS(GONOGO,I)
                      IF(.NOT.GONOGO) THEN
                      ELSE
                          IF(GLANAM(I,2).EQ.'REFL         '.OR.
     1                    GLANAM(I,2).EQ.'REFLTIR      '.OR.
     2                    GLANAM(I,2).EQ.'REFLTIRO     ') THEN
                              WRITE(OUTLYNE,*)
     1                        '"THERM GAS" DID NOT MODIFY "REFL" SURFACE # ',I
                              CALL SHOWIT(1)
                              WRITE(OUTLYNE,*)'NO ACTION TAKEN'
                              CALL SHOWIT(1)
                          END IF
                          IF(GLANAM(I,2).EQ.'PERFECT      ') THEN
                              WRITE(OUTLYNE,*)
     1                        '"THERM GAS" DID NOT MODIFY "PERFECT" SURFACE # ',I
                              CALL SHOWIT(1)
                              WRITE(OUTLYNE,*)'NO ACTION TAKEN'
                              CALL SHOWIT(1)
                          END IF
                          IF(GLANAM(I,2).EQ.'IDEAL        ') THEN
                              WRITE(OUTLYNE,*)
     1                        '"THERM GAS" DID NOT MODIFY "IDEAL" SURFACE # ',I
                              CALL SHOWIT(1)
                              WRITE(OUTLYNE,*)'NO ACTION TAKEN'
                              CALL SHOWIT(1)
                          END IF
                          IF(GLANAM(I,2).NE.'REFL         '.AND.
     1                    GLANAM(I,2).NE.'PERFECT      '.AND.
     1                    GLANAM(I,2).NE.'REFLTIRO     '.AND.
     1                    GLANAM(I,2).NE.'REFLTIR      '.AND.
     1                    GLANAM(I,2).NE.'IDEAL        ') THEN
                              IF(DABS(ALENS(46,I)).LE.1.1D0.AND.DABS(ALENS(47,I)).LE.1.1D0
     1                        .OR.DABS(ALENS(48,I)).LE.1.1D0.AND.DABS(ALENS(49,I)).LE.1.1D0
     2                        .AND.DABS(ALENS(50,I)).LE.1.1D0) THEN
                                  GLANAM(I,1)='GLASS      '
                                  IF(SYSTEM1(1).NE.0.0D0)
     1                            ALENS(46,I)=ALENS(46,I)+(DSGN(ALENS(46,I))*W3*W4)
                                  IF(SYSTEM1(2).NE.0.0D0)
     1                            ALENS(47,I)=ALENS(47,I)+(DSGN(ALENS(47,I))*W3*W4)
                                  IF(SYSTEM1(3).NE.0.0D0)
     1                            ALENS(48,I)=ALENS(48,I)+(DSGN(ALENS(48,I))*W3*W4)
                                  IF(SYSTEM1(4).NE.0.0D0)
     1                            ALENS(49,I)=ALENS(49,I)+(DSGN(ALENS(49,I))*W3*W4)
                                  IF(SYSTEM1(5).NE.0.0D0)
     1                            ALENS(50,I)=ALENS(50,I)+(DSGN(ALENS(50,I))*W3*W4)
                                  GLANAM(I,1)(1:13)='GLASS'
                                  GLANAM(I,2)(1:13)='GAS'
                              END IF
                              IF(DABS(ALENS(71,I)).LE.1.1D0.AND.DABS(ALENS(72,I)).LE.1.1D0
     1                        .OR.DABS(ALENS(73,I)).LE.1.1D0.AND.DABS(ALENS(74,I)).LE.1.1D0
     2                        .AND.DABS(ALENS(75,I)).LE.1.1D0) THEN
                                  IF(SYSTEM1(71).NE.0.0D0)
     1                            ALENS(71,I)=ALENS(71,I)+(DSGN(ALENS(71,I))*W3*W4)
                                  IF(SYSTEM1(72).NE.0.0D0)
     1                            ALENS(72,I)=ALENS(72,I)+(DSGN(ALENS(72,I))*W3*W4)
                                  IF(SYSTEM1(73).NE.0.0D0)
     1                            ALENS(73,I)=ALENS(73,I)+(DSGN(ALENS(73,I))*W3*W4)
                                  IF(SYSTEM1(74).NE.0.0D0)
     1                            ALENS(74,I)=ALENS(74,I)+(DSGN(ALENS(74,I))*W3*W4)
                                  IF(SYSTEM1(75).NE.0.0D0)
     1                            ALENS(75,I)=ALENS(75,I)+(DSGN(ALENS(75,I))*W3*W4)
                                  GLANAM(I,1)(1:13)='GLASS'
                                  GLANAM(I,2)(1:13)='GAS'
                              END IF
                          END IF
                      END IF
                  END DO
              END IF
              F1=0
              F6=1
              F22=1
              LNSTYP=1
              CALL LNSEOS
          END IF
          IF(WQ.EQ.'ARGON') THEN
              A=27.92D-5
              B=5.6D-5
          END IF
          IF(WQ.EQ.'NITROGEN') THEN
              A=29.19D-5
              B=7.7D-5
          END IF
          IF(WQ.EQ.'HELIUM') THEN
              A=3.48D-5
              B=2.3D-5
          END IF
          IF(WQ.EQ.'HYDROGEN') THEN
              A=13.6D-5
              B=7.7D-5
          END IF
          IF(WQ.EQ.'OXYGEN') THEN
              A=26.63D-5
              B=5.07D-5
          END IF
          IF(WQ.EQ.'AIR') THEN
              A=28.79D-5
              B=5.67D-5
          END IF
          IF(WQ.EQ.'ETHANE') THEN
              A=73.65D-5
              B=9.08D-5
          END IF
          IF(WQ.EQ.'METHANE') THEN
              A=42.6D-5
              B=14.41D-5
          END IF
C     LA1 TO LA5 ARE THE BASE N-1 VALUES AT THE 5 WAVELENGTHS AT
C     760 MM OF MERCURY. IF THE PRESSURE GOES UP, THE DENSITY GOES UP
C     IN LINEAR PROPORTION AND THE N-1 VALUE SCALES LINEARLY
C
          IF(INT(W1).EQ.INT(W2)) THEN
              I=INT(W1)
              CALL CHKGLS(GONOGO,I)
              IF(.NOT.GONOGO) THEN
                  RETURN
              END IF
              IF(GLANAM(I,2).EQ.'REFL         '.OR.
     1        GLANAM(I,2).EQ.'REFLTIR      '.OR.
     2        GLANAM(I,2).EQ.'REFLTIRO     ') THEN
                  WRITE(OUTLYNE,*)
     1            '"THERM ',WQ,'" DID NOT MODIFY "REFL" SURFACE # ',I
                  CALL SHOWIT(1)
                  RETURN
              END IF
              IF(GLANAM(I,2).EQ.'PERFECT      ') THEN
                  WRITE(OUTLYNE,*)
     1            '"THERM ',WQ,'" DID NOT MODIFY "PERFECT" SURFACE # ',I
                  CALL SHOWIT(1)
                  RETURN
              END IF
              IF(GLANAM(I,2).EQ.'IDEAL        ') THEN
                  WRITE(OUTLYNE,*)
     1            '"THERM ',WQ,'" DID NOT MODIFY "IDEAL" SURFACE # ',I
                  CALL SHOWIT(1)
                  RETURN
              END IF
              IF(GLANAM(I,2).NE.'REFL         '.AND.
     1        GLANAM(I,2).NE.'PERFECT      '.AND.
     1        GLANAM(I,2).NE.'REFLTIR      '.AND.
     1        GLANAM(I,2).NE.'REFLTIRO     '.AND.
     1        GLANAM(I,2).NE.'IDEAL        ') THEN
                  IF(DABS(ALENS(46,I)).LE.1.1D0.AND.DABS(ALENS(47,I)).LE.1.1D0
     1            .OR.DABS(ALENS(48,I)).LE.1.1D0.AND.DABS(ALENS(49,I)).LE.1.1D0
     2            .AND.DABS(ALENS(50,I)).LE.1.1D0.AND.
     1            DABS(ALENS(71,I)).LE.1.1D0.AND.DABS(ALENS(72,I)).LE.1.1D0
     1            .OR.DABS(ALENS(73,I)).LE.1.1D0.AND.DABS(ALENS(74,I)).LE.1.1D0
     2            .AND.DABS(ALENS(75,I)).LE.1.1D0) THEN
                      GLANAM(I,1)='GLASS      '
                      IF(SYSTEM1(1).NE.0.0D0) THEN
                          LA1=LM1(A,B,SYSTEM1(1))
                          ALENS(46,I)=ALENS(46,I)+(((LA1*((293.00)/(W3+273.0D0)))-LA1)
     1                    *DSGN(ALENS(46,I)))
                      END IF
                      IF(SYSTEM1(2).NE.0.0D0) THEN
                          LA2=LM1(A,B,SYSTEM1(2))
                          ALENS(47,I)=ALENS(47,I)+(((LA2*((293.00)/(W3+273.0D0)))-LA2)
     1                    *DSGN(ALENS(47,I)))
                      END IF
                      IF(SYSTEM1(3).NE.0.0D0) THEN
                          LA3=LM1(A,B,SYSTEM1(3))
                          ALENS(48,I)=ALENS(48,I)+(((LA3*((293.00)/(W3+273.0D0)))-LA3)
     1                    *DSGN(ALENS(48,I)))
                      END IF
                      IF(SYSTEM1(4).NE.0.0D0) THEN
                          LA4=LM1(A,B,SYSTEM1(4))
                          ALENS(49,I)=ALENS(49,I)+(((LA4*((293.00)/(W3+273.0D0)))-LA4)
     1                    *DSGN(ALENS(49,I)))
                      END IF
                      IF(SYSTEM1(5).NE.0.0D0) THEN
                          LA5=LM1(A,B,SYSTEM1(5))
                          ALENS(50,I)=ALENS(50,I)+(((LA5*((293.00)/(W3+273.0D0)))-LA5)
     1                    *DSGN(ALENS(50,I)))
                      END IF
                      IF(SYSTEM1(71).NE.0.0D0) THEN
                          LA6=LM1(A,B,SYSTEM1(71))
                          ALENS(71,I)=ALENS(71,I)+(((LA6*((293.00)/(W3+273.0D0)))-LA6)
     1                    *DSGN(ALENS(71,I)))
                      END IF
                      IF(SYSTEM1(72).NE.0.0D0) THEN
                          LA7=LM1(A,B,SYSTEM1(72))
                          ALENS(72,I)=ALENS(72,I)+(((LA7*((293.00)/(W3+273.0D0)))-LA7)
     1                    *DSGN(ALENS(72,I)))
                      END IF
                      IF(SYSTEM1(73).NE.0.0D0) THEN
                          LA8=LM1(A,B,SYSTEM1(73))
                          ALENS(73,I)=ALENS(73,I)+(((LA8*((293.00)/(W3+273.0D0)))-LA8)
     1                    *DSGN(ALENS(73,I)))
                      END IF
                      IF(SYSTEM1(74).NE.0.0D0) THEN
                          LA9=LM1(A,B,SYSTEM1(74))
                          ALENS(74,I)=ALENS(74,I)+(((LA9*((293.00)/(W3+273.0D0)))-LA9)
     1                    *DSGN(ALENS(74,I)))
                      END IF
                      IF(SYSTEM1(75).NE.0.0D0) THEN
                          LA10=LM1(A,B,SYSTEM1(75))
                          ALENS(75,I)=ALENS(75,I)+(((LA10*((293.00)/(W3+273.0D0)))-LA10)
     1                    *DSGN(ALENS(75,I)))
                      END IF
                      GLANAM(1,I)='GLASS'
                      GLANAM(2,I)='WQ'
                      RETURN
                  ELSE
                      RETURN
                  END IF
              END IF
          ELSE
              DO I=INT(W1),INT(W2)
                  CALL CHKGLS(GONOGO,I)
                  IF(.NOT.GONOGO) THEN
                  ELSE
                      IF(GLANAM(I,2).EQ.'REFL         '.OR.
     1                GLANAM(I,2).EQ.'REFLTIR      '.OR.
     2                GLANAM(I,2).EQ.'REFLTIRO     ') THEN
                          WRITE(OUTLYNE,*)
     1                    '"THERM ',WQ,'" DID NOT MODIFY "REFL" SURFACE # ',I
                          CALL SHOWIT(1)
                      END IF
                      IF(GLANAM(I,2).EQ.'PERFECT      ') THEN
                          WRITE(OUTLYNE,*)
     1                    '"THERM ',WQ,'" DID NOT MODIFY "PERFECT" SURFACE # ',I
                          CALL SHOWIT(1)
                      END IF
                      IF(GLANAM(I,2).EQ.'IDEAL        ') THEN
                          WRITE(OUTLYNE,*)
     1                    '"THERM ',WQ,'" DID NOT MODIFY "IDEAL" SURFACE # ',I
                          CALL SHOWIT(1)
                      END IF
                      IF(GLANAM(I,2).NE.'REFL         '.AND.
     1                GLANAM(I,2).NE.'PERFECT      '.AND.
     1                GLANAM(I,2).NE.'REFLTIR      '.AND.
     1                GLANAM(I,2).NE.'REFLTIRO     '.AND.
     1                GLANAM(I,2).NE.'IDEAL        ') THEN
                          IF(DABS(ALENS(46,I)).LE.1.1D0.AND.DABS(ALENS(47,I)).LE.1.1D0
     1                    .OR.DABS(ALENS(48,I)).LE.1.1D0.AND.DABS(ALENS(49,I)).LE.1.1D0
     2                    .AND.DABS(ALENS(50,I)).LE.1.1D0.AND.
     1                    DABS(ALENS(71,I)).LE.1.1D0.AND.DABS(ALENS(72,I)).LE.1.1D0
     1                    .OR.DABS(ALENS(73,I)).LE.1.1D0.AND.DABS(ALENS(74,I)).LE.1.1D0
     2                    .AND.DABS(ALENS(75,I)).LE.1.1D0) THEN
                              GLANAM(I,2)='GLASS      '
                              IF(SYSTEM1(71).NE.0.0D0) THEN
                                  LA1=LM1(A,B,SYSTEM1(1))
                                  ALENS(46,I)=ALENS(46,I)+(((LA1*((293.00)/(W3+273.0D0)))-LA1)
     1                            *DSGN(ALENS(46,I)))
                              END IF
                              IF(SYSTEM1(72).NE.0.0D0) THEN
                                  LA2=LM1(A,B,SYSTEM1(2))
                                  ALENS(47,I)=ALENS(47,I)+(((LA2*((293.00)/(W3+273.0D0)))-LA2)
     1                            *DSGN(ALENS(47,I)))
                              END IF
                              IF(SYSTEM1(73).NE.0.0D0) THEN
                                  LA3=LM1(A,B,SYSTEM1(3))
                                  ALENS(48,I)=ALENS(48,I)+(((LA3*((293.00)/(W3+273.0D0)))-LA3)
     1                            *DSGN(ALENS(48,I)))
                              END IF
                              IF(SYSTEM1(74).NE.0.0D0) THEN
                                  LA4=LM1(A,B,SYSTEM1(4))
                                  ALENS(49,I)=ALENS(49,I)+(((LA4*((293.00)/(W3+273.0D0)))-LA4)
     1                            *DSGN(ALENS(49,I)))
                              END IF
                              IF(SYSTEM1(75).NE.0.0D0) THEN
                                  LA5=LM1(A,B,SYSTEM1(5))
                                  ALENS(50,I)=ALENS(50,I)+(((LA5*((293.00)/(W3+273.0D0)))-LA5)
     1                            *DSGN(ALENS(50,I)))
                              END IF
                              IF(SYSTEM1(71).NE.0.0D0) THEN
                                  LA6=LM1(A,B,SYSTEM1(71))
                                  ALENS(71,I)=ALENS(71,I)+(((LA6*((293.00)/(W3+273.0D0)))-LA6)
     1                            *DSGN(ALENS(71,I)))
                              END IF
                              IF(SYSTEM1(72).NE.0.0D0) THEN
                                  LA7=LM1(A,B,SYSTEM1(72))
                                  ALENS(72,I)=ALENS(72,I)+(((LA7*((293.00)/(W3+273.0D0)))-LA7)
     1                            *DSGN(ALENS(72,I)))
                              END IF
                              IF(SYSTEM1(73).NE.0.0D0) THEN
                                  LA8=LM1(A,B,SYSTEM1(73))
                                  ALENS(73,I)=ALENS(73,I)+(((LA8*((293.00)/(W3+273.0D0)))-LA8)
     1                            *DSGN(ALENS(73,I)))
                              END IF
                              IF(SYSTEM1(74).NE.0.0D0) THEN
                                  LA9=LM1(A,B,SYSTEM1(74))
                                  ALENS(74,I)=ALENS(74,I)+(((LA9*((293.00)/(W3+273.0D0)))-LA9)
     1                            *DSGN(ALENS(74,I)))
                              END IF
                              IF(SYSTEM1(75).NE.0.0D0) THEN
                                  LA10=LM1(A,B,SYSTEM1(75))
                                  ALENS(75,I)=ALENS(75,I)+(((LA10*((293.00)/(W3+273.0D0)))-LA10)
     1                            *DSGN(ALENS(75,I)))
                              END IF
                              GLANAM(I,1)='GLASS'
                              GLANAM(I,2)='WQ'
                          END IF
                      END IF
                  END IF
              END DO
          END IF
          RETURN
      END
      FUNCTION DSGN(A)
          IMPLICIT NONE
          REAL*8 A,DSGN
          IF(A.NE.0.0D0) THEN
              DSGN=A/DABS(A)
          ELSE
              DSGN=1.0D0
          END IF
          RETURN
      END
C SUB SXYD.FOR
      SUBROUTINE SXYD
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE SXYD WHICH IMPLEMENTS THE XD AND YD
C       COMMAND AT THE LENS UPDATE LEVEL.
C
          INTEGER PIKCNT,CT,I
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
C
C               CHECK FOR ADDITIONAL INPUT AND
C               PRINT ERROR AND RETURN IF DISCOVERED.
C
          IF(SST.EQ.1) THEN
              OUTLYNE=
     1        '"'//WQ(1:2)//'" TAKES NO STRING INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1
     1    .OR.S5.EQ.1) THEN
              OUTLYNE=
     1        '"'//WQ(1:2)//'" TAKES NO NUMERIC WORD #2 THROUGH #5 INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SQ.EQ.1.AND.F5.EQ.1) THEN
              OUTLYNE=
     1        '"'//WQ(1:2)//'" TAKES NO QUALIFIER WORD IN LENS INPUT MODE'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(F6.EQ.1) THEN
              IF(WQ.NE.'        '.AND.WQ.NE.'DELT'.AND.WQ.NE.'CENT') THEN
                  OUTLYNE=
     1            'INVALID QUALIFIER WORD USED WITH "'//WQ(1:2)//'"'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
          IF(DF1.EQ.1) THEN
              OUTLYNE=
     1        'REQUIRES EXPLICIT NUMERIC WORD #1 INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
          IF(SURF.EQ.0) THEN
              OUTLYNE='OBJECT SURFACE MAY NOT BE DECENTERED'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
C       WE ARE AT LENS UPDATE LEVEL
C
C               PROCEED
C
C       CHECK FOR PIKUP XD OR YD
C       AND DELETE THEN RESOLVE ALENS(32,SURF)
C
          IF(WC.EQ.'YD') CT=13
          IF(WC.EQ.'XD') CT=14
          IF(WC.EQ.'ZD') CT=33
C
          IF(PIKUP(1,SURF,CT).EQ.1.0D0) THEN
              PIKUP(1:6,SURF,CT)=0.0D0
              ALENS(32,SURF)=ALENS(32,SURF)-1.0D0
              IF(CT.EQ.13)
     1        WRITE(OUTLYNE,*)'SURFACE',SURF,' :PIKUP (XD) DELETED'
              IF(CT.EQ.14)
     1        WRITE(OUTLYNE,*)'SURFACE',SURF,' :PIKUP (YD) DELETED'
              IF(CT.EQ.33)
     1        WRITE(OUTLYNE,*)'SURFACE',SURF,' :PIKUP (ZD) DELETED'
              CALL SHOWIT(1)
          END IF
          PIKCNT=0
          DO 10 I=1,PSIZ
              IF(PIKUP(1,SURF,I).NE.0.0D0) THEN
                  PIKCNT=PIKCNT+1
              ELSE
              END IF
 10       CONTINUE
          IF(PIKCNT.EQ.0) ALENS(32,SURF)=0.0D0
C
C       PROCEED WITH XD OR YD ASSIGNMENT
C
C       DON'T SET DEC FLAG (ALENS(29,SURF) IF YD AND XD AND ZD ARE 0.0D0
C       BUT CLEAR IT.
C
C
          IF(SQ.EQ.0) THEN
              IF(WC.EQ.'YD') ALENS(30,SURF)=W1
              IF(WC.EQ.'XD') ALENS(31,SURF)=W1
              IF(WC.EQ.'ZD') ALENS(69,SURF)=W1
              IF(WC.EQ.'YD') ALENS(115,SURF)=W1
              IF(WC.EQ.'XD') ALENS(114,SURF)=W1
              IF(WC.EQ.'ZD') ALENS(116,SURF)=W1
          END IF
          IF(WQ.EQ.'DELT') THEN
              IF(WC.EQ.'YD') ALENS(30,SURF)=ALENS(30,SURF)+W1
              IF(WC.EQ.'XD') ALENS(31,SURF)=ALENS(31,SURF)+W1
              IF(WC.EQ.'ZD') ALENS(69,SURF)=ALENS(69,SURF)+W1
              IF(WC.EQ.'YD') ALENS(115,SURF)=ALENS(115,SURF)+W1
              IF(WC.EQ.'XD') ALENS(114,SURF)=ALENS(114,SURF)+W1
              IF(WC.EQ.'ZD') ALENS(116,SURF)=ALENS(116,SURF)+W1
          END IF
          IF(WQ.EQ.'CENT') THEN
              IF(WC.EQ.'YD')
     1        ALENS(30,SURF)=ALENS(30,SURF)+(W1*0.01D0*ALENS(30,SURF))
              IF(WC.EQ.'XD')
     1        ALENS(31,SURF)=ALENS(31,SURF)+(W1*0.01D0*ALENS(31,SURF))
              IF(WC.EQ.'ZD')
     1        ALENS(69,SURF)=ALENS(69,SURF)+(W1*0.01D0*ALENS(69,SURF))
              IF(WC.EQ.'YD')
     1        ALENS(30,SURF)=ALENS(115,SURF)+(W1*0.01D0*ALENS(115,SURF))
              IF(WC.EQ.'XD')
     1        ALENS(31,SURF)=ALENS(114,SURF)+(W1*0.01D0*ALENS(114,SURF))
              IF(WC.EQ.'ZD')
     1        ALENS(69,SURF)=ALENS(116,SURF)+(W1*0.01D0*ALENS(116,SURF))
          END IF
C       CHECK ALENS(30,SURF) AND ALENS(31,SURF) AND ALENS(69,SURF) ZEROS
C       IF THEY ARE,CLEAR THE DECENTER FLAG
          IF(ALENS(30,SURF).EQ.0.0D0.AND.ALENS(31,SURF).EQ.0.0D0
     1    .AND.ALENS(69,SURF).EQ.0.0D0
     1    .AND.ALENS(114,SURF).EQ.0.0D0
     1    .AND.ALENS(115,SURF).EQ.0.0D0
     1    .AND.ALENS(116,SURF).EQ.0.0D0) THEN
              ALENS(29,SURF)=0.0D0
          ELSE
C       THE DECENTER FLAG SHOULD BE SET. CHECK IT AND SET IT
C       IF NECESSARY.
C
              IF(ALENS(29,SURF).EQ.0.0D0) ALENS(29,SURF)=1.0D0
          END IF
C
C       NOW IF A SURFACE IS PIKING UP XD FROM THIS SURFACE WE MAY
C       HAVE TO
C       UPDATE THE STATUS OF ALENS(29, ) ON THAT SURFACE TO THE
C       NEW VALUE ON
C       SURF
C
C       RESOLVE THIS
C
          DO 100 I=0,INT(SYSTEM1(20))
              IF(PIKUP(1,I,CT).EQ.1.0D0) THEN
                  IF(INT(PIKUP(2,I,CT)).EQ.SURF) THEN
                      ALENS(29,I)=ALENS(29,SURF)
                  ELSE
                  END IF
              ELSE
              END IF
 100      CONTINUE
C
C       IF THE SURFACE HAS A TILT AUTO THEN REMOVE THE
C       AUTO AND PRINT MESSAGE
C
          IF(ALENS(25,SURF).EQ.2.0D0.OR.ALENS(25,SURF).EQ.
     1    3.0D0) THEN
              ALENS(25,SURF)=1.0D0
              IF(ALENS(25,SURF).EQ.2.0D0) OUTLYNE=
     1        '"AUTO" ADJUST REMOVED FROM SURFACE TILT DEFINITION'
              CALL SHOWIT(1)
              IF(ALENS(25,SURF).EQ.3.0D0) OUTLYNE=
     1        '"AUTOM" ADJUST REMOVED FROM SURFACE TILT DEFINITION'
              CALL SHOWIT(1)
          END IF
          RETURN
      END
C SUB SPIVXYD.FOR
      SUBROUTINE SPIVXYD
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE SPIVXYD WHICH IMPLEMENTS THE PIVX,PIVY AND PIVZ
C       COMMAND AT THE LENS UPDATE LEVEL.
C
          INTEGER PIKCNT,CT,I
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
C
C               CHECK FOR ADDITIONAL INPUT AND
C               PRINT ERROR AND RETURN IF DISCOVERED.
C
          IF(SST.EQ.1) THEN
              OUTLYNE=
     1        '"'//WQ(1:4)//'" TAKES NO STRING INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1
     1    .OR.S5.EQ.1) THEN
              OUTLYNE=
     1        '"'//WQ(1:4)//'" TAKES NO NUMERIC WORD #2 THROUGH #5 INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SQ.EQ.1.AND.F5.EQ.1) THEN
              OUTLYNE=
     1        '"'//WQ(1:4)//'" TAKES NO QUALIFIER WORD IN LENS INPUT MODE'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(F6.EQ.1) THEN
              IF(WQ.NE.'        '.AND.WQ.NE.'DELT'.AND.WQ.NE.'CENT') THEN
                  OUTLYNE=
     1            'INVALID QUALIFIER WORD USED WITH "'//WQ(1:4)//'"'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
          IF(DF1.EQ.1) THEN
              OUTLYNE=
     1        'REQUIRES EXPLICIT NUMERIC WORD #1 INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
          IF(SURF.EQ.0) THEN
              OUTLYNE='OBJECT SURFACE MAY NOT BE HAVE A PIVOT POINT ASSIGNED'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
C       WE ARE AT LENS UPDATE LEVEL
C
C               PROCEED
C
C       CHECK FOR PIKUP PIVX,PIVY,PIVZ
C       AND DELETE THEN RESOLVE ALENS(32,SURF)
C
          IF(WC.EQ.'PIVX') CT=34
          IF(WC.EQ.'PIVY') CT=35
          IF(WC.EQ.'PIVZ') CT=36
C
          IF(PIKUP(1,SURF,CT).EQ.1.0D0) THEN
              PIKUP(1:6,SURF,CT)=0.0D0
              ALENS(32,SURF)=ALENS(32,SURF)-1.0D0
              IF(CT.EQ.34)
     1        WRITE(OUTLYNE,*)'SURFACE',SURF,' :PIKUP (PIVX) DELETED'
              IF(CT.EQ.35)
     1        WRITE(OUTLYNE,*)'SURFACE',SURF,' :PIKUP (PIVY) DELETED'
              IF(CT.EQ.36)
     1        WRITE(OUTLYNE,*)'SURFACE',SURF,' :PIKUP (PIVZ) DELETED'
              CALL SHOWIT(1)
          END IF
          PIKCNT=0
          DO 10 I=1,PSIZ
              IF(PIKUP(1,SURF,I).NE.0.0D0) THEN
                  PIKCNT=PIKCNT+1
              ELSE
              END IF
 10       CONTINUE
          IF(PIKCNT.EQ.0) ALENS(32,SURF)=0.0D0
C
C       PROCEED WITH ASSIGNMENT
C
C
          IF(SQ.EQ.0) THEN
              IF(WC.EQ.'PIVX') ALENS(78,SURF)=W1
              IF(WC.EQ.'PIVY') ALENS(79,SURF)=W1
              IF(WC.EQ.'PIVZ') ALENS(80,SURF)=W1
          END IF
          IF(WQ.EQ.'DELT') THEN
              IF(WC.EQ.'PIVX') ALENS(78,SURF)=ALENS(78,SURF)+W1
              IF(WC.EQ.'PIVY') ALENS(79,SURF)=ALENS(79,SURF)+W1
              IF(WC.EQ.'PIVZ') ALENS(80,SURF)=ALENS(80,SURF)+W1
          END IF
          IF(WQ.EQ.'CENT') THEN
              IF(WC.EQ.'PIVX')
     1        ALENS(78,SURF)=ALENS(78,SURF)+(W1*0.01D0*ALENS(78,SURF))
              IF(WC.EQ.'PIVY')
     1        ALENS(79,SURF)=ALENS(79,SURF)+(W1*0.01D0*ALENS(79,SURF))
              IF(WC.EQ.'PIVZ')
     1        ALENS(80,SURF)=ALENS(80,SURF)+(W1*0.01D0*ALENS(80,SURF))
          END IF
C       CHECK ALENS(78,SURF) AND ALENS(79,SURF) AND ALENS(80,SURF) ZEROS
C       IF THEY ARE,CLEAR THE DECENTER FLAG
          IF(ALENS(78,SURF).EQ.0.0D0.AND.ALENS(79,SURF).EQ.0.0D0
     1    .AND.ALENS(80,SURF).EQ.0.0D0) THEN
              ALENS(59,SURF)=1.0D0
          ELSE
C       THE DECENTER FLAG SHOULD BE SET. CHECK IT AND SET IT
C       IF NECESSARY.
C
              IF(ALENS(59,SURF).EQ.0.0D0) ALENS(59,SURF)=1.0D0
          END IF
C
C       NOW IF A SURFACE IS PIKING UP PIV FROM THIS SURFACE WE MAY
C       HAVE TO
C       UPDATE THE STATUS OF ALENS(59, ) ON THAT SURFACE TO THE
C       NEW VALUE ON
C       SURF
C
C       RESOLVE THIS
C
          DO 100 I=0,INT(SYSTEM1(20))
              IF(PIKUP(1,I,CT).EQ.1.0D0) THEN
                  IF(INT(PIKUP(2,I,CT)).EQ.SURF) THEN
                      ALENS(59,I)=ALENS(59,SURF)
                  ELSE
                  END IF
              ELSE
              END IF
 100      CONTINUE
C
C       IF THE SURFACE HAS A TILT AUTO THEN REMOVE THE
C       AUTO AND PRINT MESSAGE
C
          IF(ALENS(25,SURF).EQ.2.0D0.OR.ALENS(25,SURF).EQ.
     1    3.0D0) THEN
              ALENS(25,SURF)=1.0D0
              IF(ALENS(25,SURF).EQ.2.0D0) OUTLYNE=
     1        '"AUTO" ADJUST REMOVED FROM SURFACE TILT DEFINITION'
              CALL SHOWIT(1)
              IF(ALENS(25,SURF).EQ.3.0D0) OUTLYNE=
     1        '"AUTOM" ADJUST REMOVED FROM SURFACE TILT DEFINITION'
              CALL SHOWIT(1)
          END IF
          RETURN
      END
C SUB SGXYD.FOR
      SUBROUTINE SGXYD
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE SGXYD WHICH IMPLEMENTS GDX, GDY GDZ
C       COMMAND AT THE LENS UPDATE LEVEL.
C
          INTEGER PIKCNT,CT,I
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
C
C               CHECK FOR ADDITIONAL INPUT AND
C               PRINT ERROR AND RETURN IF DISCOVERED.
C
          IF(SST.EQ.1) THEN
              OUTLYNE=
     1        '"'//WQ(1:3)//'" TAKES NO STRING INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1
     1    .OR.S5.EQ.1) THEN
              OUTLYNE=
     1        '"'//WQ(1:3)//'" TAKES NO NUMERIC WORD #2 THROUGH #5 INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SQ.EQ.1.AND.F5.EQ.1) THEN
              OUTLYNE=
     1        '"'//WQ(1:3)//'" TAKES NO QUALIFIER WORD IN LENS INPUT MODE'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(F6.EQ.1) THEN
              IF(WQ.NE.'        '.AND.WQ.NE.'DELT'.AND.WQ.NE.'CENT') THEN
                  OUTLYNE=
     1            'INVALID QUALIFIER WORD USED WITH "'//WQ(1:3)//'"'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
          IF(DF1.EQ.1) THEN
              OUTLYNE=
     1        'REQUIRES EXPLICIT NUMERIC WORD #1 INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
          IF(SURF.EQ.0) THEN
              OUTLYNE='OBJECT SURFACE MAY NOT BE DECENTERED'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
C       WE ARE AT LENS UPDATE LEVEL
C
C               PROCEED
C
C       CHECK FOR PIKUP GDX, GDY OR GDZ
C       AND DELETE THEN RESOLVE ALENS(32,SURF)
C
          IF(WC.EQ.'GDX') CT=37
          IF(WC.EQ.'GDY') CT=38
          IF(WC.EQ.'GDZ') CT=39
C
          IF(PIKUP(1,SURF,CT).EQ.1.0D0) THEN
              PIKUP(1:6,SURF,CT)=0.0D0
              ALENS(32,SURF)=ALENS(32,SURF)-1.0D0
              IF(CT.EQ.37)
     1        WRITE(OUTLYNE,*)'SURFACE',SURF,' :PIKUP (GDX) DELETED'
              IF(CT.EQ.38)
     1        WRITE(OUTLYNE,*)'SURFACE',SURF,' :PIKUP (GDY) DELETED'
              IF(CT.EQ.39)
     1        WRITE(OUTLYNE,*)'SURFACE',SURF,' :PIKUP (GDZ) DELETED'
              CALL SHOWIT(1)
          END IF
          PIKCNT=0
          DO 10 I=1,PSIZ
              IF(PIKUP(1,SURF,I).NE.0.0D0) THEN
                  PIKCNT=PIKCNT+1
              ELSE
              END IF
 10       CONTINUE
          IF(PIKCNT.EQ.0) ALENS(32,SURF)=0.0D0
C
C       PROCEED WITH GDX, GDY OR GDZ ASSIGNMENT
C
C
          IF(ALENS(25,SURF).EQ.6.0D0.OR.ALENS(25,SURF).EQ.1.0D0) THEN
          ELSE
              IF(WC.EQ.'GDX')
     1        OUTLYNE='"GDX" REQUIRES A "TILT" OR A "TILT RET" SURFACE'
              IF(WC.EQ.'GDY')
     1        OUTLYNE='"GDY" REQUIRES A "TILT" OR A "TILT RET" SURFACE'
              IF(WC.EQ.'GDZ')
     1        OUTLYNE='"GDZ" REQUIRES A "TILT" OR A "TILT RET" SURFACE'
              CALL SHOWIT(1)
              OUTLYNE='NO-ACTION TAKEN'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(ALENS(25,SURF).EQ.6.0D0.OR.ALENS(25,SURF).EQ.1.0D0) THEN
C
              IF(SQ.EQ.0) THEN
                  IF(WC.EQ.'GDX') ALENS(90,SURF)=W1
                  IF(WC.EQ.'GDY') ALENS(91,SURF)=W1
                  IF(WC.EQ.'GDZ') ALENS(92,SURF)=W1
              END IF
              IF(WQ.EQ.'DELT') THEN
                  IF(WC.EQ.'GDX') ALENS(90,SURF)=ALENS(90,SURF)+W1
                  IF(WC.EQ.'GDY') ALENS(91,SURF)=ALENS(91,SURF)+W1
                  IF(WC.EQ.'GDZ') ALENS(92,SURF)=ALENS(92,SURF)+W1
              END IF
              IF(WQ.EQ.'CENT') THEN
                  IF(WC.EQ.'GDX')
     1            ALENS(90,SURF)=ALENS(90,SURF)+(W1*0.01D0*ALENS(90,SURF))
                  IF(WC.EQ.'GDY')
     1            ALENS(91,SURF)=ALENS(91,SURF)+(W1*0.01D0*ALENS(91,SURF))
                  IF(WC.EQ.'GDZ')
     1            ALENS(92,SURF)=ALENS(92,SURF)+(W1*0.01D0*ALENS(92,SURF))
              END IF
          END IF
C
C
          RETURN
      END
C SUB SGRAT.FOR
      SUBROUTINE SGRAT
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE SGRAT WHICH IMPLEMENTS GRT,GRO,GRS,GRX,GRY
C       AND GRZ COMMANDS AT THE LENS UPDATE LEVEL.
C
          INTEGER PIKCNT,CT,I
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
C
C               CHECK FOR ADDITIONAL INPUT AND
C               PRINT ERROR AND RETURN IF DISCOVERED.
C
          IF(SST.EQ.1) THEN
              OUTLYNE=
     1        '"'//WQ(1:3)//'" TAKES NO STRING INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(WC.NE.'GRT') THEN
              IF(ALENS(96,SURF).NE.1.0D0) THEN
                  OUTLYNE=
     1            '"'//WQ(1:3)//'" REQUIRES THE SURFACE TO BE A LINEAR GRATING'
                  CALL SHOWIT(1)
                  OUTLYNE='NO ACTION TAKEN'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
          IF(WC.NE.'GRT') THEN
              IF(S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1
     1        .OR.S5.EQ.1) THEN
                  OUTLYNE=
     1            '"'//WQ(1:3)//'" TAKES NO NUMERIC WORD #2 THROUGH #5 INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          ELSE
              IF(SN.EQ.1) THEN
                  OUTLYNE=
     1            '"'//WQ(1:3)//'" TAKES NO NUMERIC INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
          IF(SQ.EQ.1) THEN
              OUTLYNE=
     1        '"'//WQ(1:3)//'" TAKES NO QUALIFIER WORD INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(DF1.EQ.1.AND.WC.NE.'GRT') THEN
              OUTLYNE=
     1        '"'//WQ(1:3)//'" TAKES NO NUMERIC INPUT'
              CALL SHOWIT(1)
              OUTLYNE=
     1        'REQUIRES EXPLICIT NUMERIC WORD #1 INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
          IF(SURF.EQ.0) THEN
              OUTLYNE='OBJECT SURFACE MAY NOT BE A DIFFRACTION GRATING'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(F6.EQ.1.AND.SURF.EQ.INT(SYSTEM1(20))) THEN
              OUTLYNE='FINAL SURFACE MAY NOT BE A DIFFRACTION GRATING'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
C       WE ARE AT LENS UPDATE LEVEL
C
C               PROCEED
C
C       CHECK FOR PIKUP GDX, GDY OR GDZ
C       AND DELETE THEN RESOLVE ALENS(32,SURF)
C
          CT=43
C
          IF(PIKUP(1,SURF,CT).EQ.1.0D0) THEN
              PIKUP(1:6,SURF,CT)=0.0D0
              ALENS(32,SURF)=ALENS(32,SURF)-1.0D0
              WRITE(OUTLYNE,*)'SURFACE',SURF,' :PIKUP (GRT) DELETED'
              CALL SHOWIT(1)
          END IF
          PIKCNT=0
          DO 10 I=1,PSIZ
              IF(PIKUP(1,SURF,I).NE.0.0D0) THEN
                  PIKCNT=PIKCNT+1
              ELSE
              END IF
 10       CONTINUE
          IF(PIKCNT.EQ.0) ALENS(32,SURF)=0.0D0
          IF(WC.EQ.'GRT') THEN
              ALENS(96,SURF)=1.0D0
              ALENS(97:99,SURF)=0.0D0
              ALENS(100,SURF)=1.0D0
              ALENS(101,SURF)=0.0D0
          END IF
          IF(WC.EQ.'GRO') ALENS(97,SURF)=W1
          IF(WC.EQ.'GRS') ALENS(98,SURF)=W1
          IF(WC.EQ.'GRX') ALENS(99,SURF)=W1
          IF(WC.EQ.'GRY') ALENS(100,SURF)=W1
          IF(WC.EQ.'GRZ') ALENS(101,SURF)=W1
C
          RETURN
      END
C SUB SGRTD.FOR
      SUBROUTINE SGRTD
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE SGRTD WHICH IMPLEMENTS GRTD
C
          INTEGER PIKCNT,CT,I
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
C
C               CHECK FOR ADDITIONAL INPUT AND
C               PRINT ERROR AND RETURN IF DISCOVERED.
C
          IF(SST.EQ.1.OR.SQ.EQ.1.OR.SN.EQ.1) THEN
              OUTLYNE=
     1        '"'//WQ(1:4)//'" TAKES NO ADDITIONAL INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(ALENS(96,SURF).NE.1.0D0) THEN
              OUTLYNE=
     1        'NO DIFFRACTION GRATING TO DELETE ON THE CURRENT SURFACE'
              CALL SHOWIT(1)
              OUTLYNE='NO ACTION TAKEN'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
C     DELETE GRATING PIKUPS
C
          CT=43
C
          IF(PIKUP(1,SURF,CT).EQ.1.0D0) THEN
              PIKUP(1:6,SURF,CT)=0.0D0
              ALENS(32,SURF)=ALENS(32,SURF)-1.0D0
              WRITE(OUTLYNE,*)'SURFACE',SURF,' :PIKUP (GRT) DELETED'
              CALL SHOWIT(1)
          END IF
          PIKCNT=0
          DO I=1,PSIZ
              IF(PIKUP(1,SURF,I).NE.0.0D0) THEN
                  PIKCNT=PIKCNT+1
              ELSE
              END IF
          END DO
          IF(PIKCNT.EQ.0) ALENS(32,SURF)=0.0D0
          ALENS(96:101,SURF)=0.0D0
          RETURN
      END
C SUB ABSORB.FOR
      SUBROUTINE ABSORB
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE ABSORB WHICH IMPLEMENTS CMD LEVEL COMMAND ABSORB
C       WHICH MANAGES THE INTERNAL ABSORBTION COEFFICIENT
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
C
C               CHECK FOR ADDITIONAL INPUT AND
C               PRINT ERROR AND RETURN IF DISCOVERED.
C
          IF(SST.EQ.1.OR.SQ.EQ.1) THEN
              OUTLYNE=
     1        '"'//WQ(1:3)//'" TAKES NO QUALIFIER OR STRING INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(STI.EQ.1) THEN
              IF(DF1.EQ.1) THEN
C       DISPLAY ALL COEFS
              ELSE
C       DISPLAY ONLY FOR SURFACE W1
              END IF
          END IF
C       W1 IS SURFACE #
C       W2 IS WAVELENGTH #
C       W3 IS COEFFICIENT VALUE
C       W4 IS REFERENCE THICKNESS IN MM
          IF(DF5.EQ.0) THEN
              WRITE(OUTLYNE,*) '"ABSORB" TAKES NO NUMERIC WORD #5 INPUT'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          RETURN
      END
