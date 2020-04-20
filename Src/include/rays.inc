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
C       SINGLE RAY RAYTRACE DIALOG HANDLER
C
      IF(MESSAGE%WIN.EQ.IDD_RAY) THEN
          CALL WDIALOGSELECT(IDD_RAY)
          SELECT CASE(MESSAGE%VALUE1)
            CASE (IDOK)
C       RADIO BUTTON STATUS
C       RAY AIM POINT SPEC. METHOD
              CALL WDIALOGGETRADIOBUTTON(IDF_RAY1,JSET)
C       RAY ANGLE OUTPUT UNITS
              CALL WDIALOGGETRADIOBUTTON(IDF_ANG1,KSET)
C       RAY DATA LISTING TYPE
              CALL WDIALOGGETRADIOBUTTON(IDF_RLIS1,LSET)
C       DIFFERENTIAL RAY TRACING CHECK BOX
              CALL WDIALOGGETCHECKBOX(IDF_DIFRAY,JSTATE)
C       RAY AIMING CHECK BOX
              CALL WDIALOGGETCHECKBOX(IDF_AIM,KSTATE)
C
C       RAY AIMING ON OR OFF
              IF(KSTATE.EQ.0) INPUT='AIMRAY OFF'
              IF(KSTATE.EQ.1) INPUT='AIMRAY ON'
              CALL PROCES
C
C       DIFFERENTIAL RAYS ON OR OFF
              IF(JSTATE.EQ.0) INPUT='DIFFOB OFF'
              IF(JSTATE.EQ.1) INPUT='AIMRAY ON'
              CALL PROCES
              IF(KSTATE.EQ.0) INPUT='DIFRAY OFF'
              IF(KSTATE.EQ.1) INPUT='DIFRAY ON'
              CALL PROCES
C
C       ANGULAR OUTPUT UNITS
              IF(KSET.EQ.1) INPUT='DEG'
              IF(KSET.EQ.2) INPUT='RAD'
              IF(KSET.EQ.3) INPUT='TANGENT'
              CALL PROCES
C
C       CONSTRUCT THE FOB COMMAND AND ISSUE IT.
              CALL CHIEFTRACE
C
C       CONSTRUCT THE RAY COMMAND AND ISSUE IT.
              CALL GRAYTRACE
C
C       NOW PERFORM REQUESTED OUTPUT
              SELECT CASE (LSET)
                CASE (1)
                  INPUT='PRXYZ ALL'
                  CALL PROCES
                CASE (2)
                  INPUT='PRXYI ALL'
                  CALL PROCES
                CASE (3)
                  INPUT='PRXYIP ALL'
                  CALL PROCES
                CASE (4)
                  INPUT='PRXYD ALL'
                  CALL PROCES
                CASE (5)
                  INPUT='PRR ALL'
                  CALL PROCES
                CASE (6)
                  INPUT='PRLMN ALL'
                  CALL PROCES
                CASE (7)
                  INPUT='PRREF ALL'
                  CALL PROCES
                CASE (8)
                  INPUT='PRDIFFXR ALL'
                  CALL PROCES
                CASE (9)
                  INPUT='PRDIFFYR ALL'
                  CALL PROCES
                CASE (10)
                  INPUT='PRDIFFXM ALL'
                  CALL PROCES
                CASE (11)
                  INPUT='PRDIFFYM ALL'
                  CALL PROCES
                CASE (12)
C       OUTPUT NOTHING
                  OUTLYNE='REQUESTED RAY TRACED'
                  CALL SHOWIT(0)
              END SELECT
C
            CASE(IDCANCEL)
              CALL WDialogHide()
              CALL WDialogUnload()
          END SELECT
      END IF
C
C       SIMPLE FANS
C
C
      IF(MESSAGE%WIN.EQ.IDD_FAN1) THEN
          CALL WDIALOGSELECT(IDD_FAN1)
          SELECT CASE(MESSAGE%VALUE1)
            CASE (IDOK)
C
C       MAX PUPIL HT
              CALL WDIALOGGETDOUBLE(IDF_MIN,DW1)
C
C       MIN PUPIL HT
              CALL WDIALOGGETDOUBLE(IDF_MAX,DW2)
C
C       PLOT FAN SCALE
              CALL WDIALOGGETDOUBLE(IDF_SSI,SSI)
              CALL DTOA23(SSI,ASSI)
C
C       NUMBER OF RAYS IN FAN
              CALL WDIALOGGETINTEGER(IDF_NUM,INUM)
C
C       FAN WAVELENGTH
              CALL WDIALOGGETINTEGER(IDF_WAV,IWAV)
C
C       FAN PRIMARY TYPE
C       1=YFAN
C       2=XFAN
C       3=PFAN
C       4=NFAN
              CALL WDIALOGGETRADIOBUTTON(IDF_FT1,ISET)
C
C       FAN SECONDARY TYPE
C       1=TRANSVERSE
C       2=OPD
C       3=CD
C       4=LA
              CALL WDIALOGGETRADIOBUTTON(IDF_Q1,JSET)
C
C       PLOTTING ON OR OFF
C       0=NO PLOT
C       1=PLOT
              CALL WDIALOGGETCHECKBOX(IDF_PLOT,ISTATE)
C
C       PLOTTING AUTOSCALE FACTOR
C       1=AUTOSCALE
C       0=NO AUTOSCALE
              CALL WDIALOGGETCHECKBOX(IDF_AUTOSSI,KSTATE)
              IF(KSTATE.EQ.1.AND.SSI.EQ.0.0D0) THEN
                  KSTATE=1
                  CALL WDIALOGPUTCHECKBOX(IDF_AUTOSSI,KSTATE)
              END IF
C
C       TRACE THE CORRECT CHIEF RAY
              CALL CHIEFTRACE
C
              IF(ISET.EQ.1) PART1='YFAN '
              IF(ISET.EQ.2) PART1='XFAN '
              IF(ISET.EQ.3) PART1='PFAN '
              IF(ISET.EQ.4) PART1='NFAN '
C
              IF(JSET.EQ.1) PART2='     '
              IF(JSET.EQ.2) PART2='OPD  '
              IF(JSET.EQ.3) PART2='CD   '
              IF(JSET.EQ.4) PART2='LA   '
C
              CALL DTOA23(DW1,AW1)
              CALL DTOA23(DW2,AW2)
              CALL ITOAA(IWAV,AJ)
              CALL ITOA6(INUM,A6)
C
              INPUT=PART1//TRIM(PART2)//','//AW1//','//AW2//','//AJ//','//A6
              CALL PROCES
              IF(ISTATE.EQ.1) THEN
C       PLOTTING
                  IF(KSTATE.EQ.0) THEN
C       USE SCALE
                      INPUT='DRAWFAN,'//ASSI//',1'
                      CALL PROCES
                  ELSE
C       AUTO SCALE
                      INPUT='DRAWFAN,,1'
                      CALL PROCES
                  END IF
                  CALL GRAPHOUTPUT
              END IF
C
            CASE(IDCANCEL)
              CALL WDialogHide()
              CALL WDialogUnload()
          END SELECT
      END IF
C
C       FIELD CURVATURE, ASTIGMATISM AND DISTORTION
C
      IF(MESSAGE%WIN.EQ.IDD_DISAST) THEN
          CALL WDIALOGSELECT(IDD_DISAST)
          SELECT CASE(MESSAGE%VALUE1)
C
            CASE(IDF_FLD)
              CALL WDIALOGGETRADIOBUTTON(IDF_PL1,ISET)
              CALL WDIALOGGETINTEGER(IDF_N1,JSET)
              CALL WDIALOGGETCHECKBOX(IDF_PLOTIT1,KSET)
              IF(JSET.LT.10) JSET=10
              IF(JSET.GT.50) JSET=50
              WRITE(OUTLYNE,*) ' '
              CALL SHOWIT(1)
              IF(ISET.EQ.1) WRITE(INPUT,*) 'FLDCV,0,,',JSET
              IF(ISET.EQ.2) WRITE(INPUT,*) 'FLDCV,90,,',JSET
              CALL PROCES
              IF(KSET.EQ.1) THEN
                  INPUT='PLTFLDCV,,1'
                  CALL PROCES
                  CALL GRAPHOUTPUT
              END IF
C
            CASE(IDF_AST)
              CALL WDIALOGGETRADIOBUTTON(IDF_PL3,ISET)
              CALL WDIALOGGETINTEGER(IDF_N2,JSET)
              CALL WDIALOGGETCHECKBOX(IDF_PLOTIT2,KSET)
              IF(JSET.LT.10) JSET=10
              IF(JSET.GT.50) JSET=50
              WRITE(OUTLYNE,*) ' '
              CALL SHOWIT(1)
              IF(ISET.EQ.1) WRITE(INPUT,*) 'AST,0,,',JSET
              IF(ISET.EQ.2) WRITE(INPUT,*) 'AST,90,,',JSET
              CALL PROCES
              IF(KSET.EQ.1) THEN
                  INPUT='PLTAST,,1'
                  CALL PROCES
                  CALL GRAPHOUTPUT
              END IF
C
            CASE(IDF_DIST)
              CALL WDIALOGGETRADIOBUTTON(IDF_PL5,ISET)
              CALL WDIALOGGETINTEGER(IDF_N3,JSET)
              CALL WDIALOGGETCHECKBOX(IDF_PLOTIT3,KSET)
              CALL WDIALOGGETRADIOBUTTON(IDF_D1,LSET)
              IF(JSET.LT.10) JSET=10
              IF(JSET.GT.50) JSET=50
              IF(LSET.EQ.1) THEN
                  WRITE(OUTLYNE,*) ' '
                  CALL SHOWIT(1)
                  IF(ISET.EQ.1) WRITE(INPUT,*) 'DIST,0,,',JSET
                  IF(ISET.EQ.2) WRITE(INPUT,*) 'DIST,90,,',JSET
                  CALL PROCES
                  IF(KSET.EQ.1) THEN
                      INPUT='PLTDIST,,1'
                      CALL PROCES
                      CALL GRAPHOUTPUT
                  END IF
              END IF
              IF(LSET.EQ.2) THEN
                  WRITE(OUTLYNE,*) ' '
                  CALL SHOWIT(1)
                  IF(ISET.EQ.1) WRITE(INPUT,*) 'FISHDIST,0,,',JSET
                  IF(ISET.EQ.2) WRITE(INPUT,*) 'FISHDIST,90,,',JSET
                  CALL PROCES
                  IF(KSET.EQ.1) THEN
                      INPUT='PLTFDIST,,1'
                      CALL PROCES
                      CALL GRAPHOUTPUT
                  END IF
              END IF
C
            CASE(IDCANCEL)
              CALL WDialogHide()
              CALL WDialogUnload()
          END SELECT
      END IF
C
C       DIFSET
C
      IF(MESSAGE%WIN.EQ.IDD_DIFSET) THEN
          CALL WDIALOGSELECT(IDD_DIFSET)
          SELECT CASE(MESSAGE%VALUE1)
C
            CASE(IDOK)
              CALL WDIALOGGETRADIOBUTTON(IDF_REF1,ISET)
              CALL WDIALOGGETRADIOBUTTON(IDF_EX1,JSET)
              IF(JSET.EQ.1) THEN
                  INPUT='EXPUP AUTO'
                  CALL PROCES
              END IF
              IF(JSET.EQ.2) THEN
                  INPUT='EXPUP NOAUTO'
                  CALL PROCES
              END IF
              IF(ISET.EQ.1) THEN
                  INPUT='RSPH NOTILT'
                  CALL PROCES
              END IF
              IF(ISET.EQ.2) THEN
                  INPUT='RSPH BEST'
                  CALL PROCES
              END IF
              IF(ISET.EQ.3) THEN
                  INPUT='RSPH CHIEF'
                  CALL PROCES
              END IF
              CALL WDialogHide()
              CALL WDialogUnload()

C
            CASE(IDCANCEL)
              CALL WDialogHide()
              CALL WDialogUnload()
          END SELECT
      END IF
C
C       CAPFN
C
      IF(MESSAGE%WIN.EQ.IDD_CAPFN) THEN
          CALL WDIALOGSELECT(IDD_CAPFN)
          SELECT CASE(MESSAGE%VALUE1)
C
            CASE(IDF_CAPFN)
              CALL CHIEFTRACE
              CALL WDIALOGGETINTEGER(IDF_CAPFNNRD,ISET)
C       SET UP THE NRD
              WRITE(INPUT,*) 'CAPFNNRD,',ISET
              CALL PROCES

              IF((DBLE(ISET)/2.0D0)-DBLE(INT(DBLE(ISET)/2.0D0))
     1        .EQ.0.0D0) THEN
C
                  CALL WDIALOGGETRADIOBUTTON(IDF_CAP1,JSET)
                  IF(JSET.EQ.1) THEN
                      INPUT='CAPFN'
                      CALL PROCES
                  END IF
                  IF(JSET.EQ.2) THEN
                      INPUT='CAPFN PERFECT'
                      CALL PROCES
                  END IF
                  IF(JSET.EQ.3) THEN
                      INPUT='CAPFN SILENT'
                      CALL PROCES
                  END IF
              END IF
C
            CASE(IDF_CAPFNOUT)
              IF(.NOT.CPFNEXT) THEN
                  CALL CHIEFTRACE
                  CALL WDIALOGGETRADIOBUTTON(IDF_CAP1,JSET)
                  IF(JSET.EQ.1) THEN
                      INPUT='CAPFN'
                      CALL PROCES
                  END IF
                  IF(JSET.EQ.2) THEN
                      INPUT='CAPFN PERFECT'
                      CALL PROCES
                  END IF
                  IF(JSET.EQ.3) THEN
                      INPUT='CAPFN SILENT'
                      CALL PROCES
                  END IF
              END IF
              INPUT='CAPFNOUT'
              CALL PROCES
C
            CASE(IDF_CAPFNIN)
              IF(.NOT.CPFNEXT) THEN
                  CALL CHIEFTRACE
                  CALL WDIALOGGETRADIOBUTTON(IDF_CAP1,JSET)
                  IF(JSET.EQ.1) THEN
                      INPUT='CAPFN'
                      CALL PROCES
                  END IF
                  IF(JSET.EQ.2) THEN
                      INPUT='CAPFN PERFECT'
                      CALL PROCES
                  END IF
                  IF(JSET.EQ.3) THEN
                      INPUT='CAPFN SILENT'
                      CALL PROCES
                  END IF
                  INPUT='CAPFNOUT'
                  CALL PROCES
              END IF
              INPUT='CAPFNIN'
              CALL PROCES
C
            CASE(IDF_CAPFNADD)
              IF(.NOT.CPFNEXT) THEN
                  CALL CHIEFTRACE
                  CALL WDIALOGGETRADIOBUTTON(IDF_CAP1,JSET)
                  IF(JSET.EQ.1) THEN
                      INPUT='CAPFN'
                      CALL PROCES
                  END IF
                  IF(JSET.EQ.2) THEN
                      INPUT='CAPFN PERFECT'
                      CALL PROCES
                  END IF
                  IF(JSET.EQ.3) THEN
                      INPUT='CAPFN SILENT'
                      CALL PROCES
                  END IF
                  INPUT='CAPFNOUT'
                  CALL PROCES
              END IF
              INPUT='CAPFNADD'
              CALL PROCES
C
            CASE(IDF_CAPFNCLR)
              INPUT='CAPFNCLR'
              CALL PROCES
C
            CASE(IDF_CAPGRID)
              IF(.NOT.CPFNEXT) THEN
                  CALL CHIEFTRACE
                  CALL WDIALOGGETRADIOBUTTON(IDF_CAP1,JSET)
                  IF(JSET.EQ.1) THEN
                      INPUT='CAPFN'
                      CALL PROCES
                  END IF
                  IF(JSET.EQ.2) THEN
                      INPUT='CAPFN PERFECT'
                      CALL PROCES
                  END IF
                  IF(JSET.EQ.3) THEN
                      INPUT='CAPFN SILENT'
                      CALL PROCES
                  END IF
              END IF
              CALL WDIALOGGETINTEGER(IDF_WAVNUM,ISET)
              WRITE(INPUT,*) 'CAPGRID,',ISET
              CALL PROCES
C
            CASE(IDF_WAMAP)
              IF(.NOT.CPFNEXT) THEN
                  CALL CHIEFTRACE
                  CALL WDIALOGGETRADIOBUTTON(IDF_CAP1,JSET)
                  IF(JSET.EQ.1) THEN
                      INPUT='CAPFN'
                      CALL PROCES
                  END IF
                  IF(JSET.EQ.2) THEN
                      INPUT='CAPFN PERFECT'
                      CALL PROCES
                  END IF
                  IF(JSET.EQ.3) THEN
                      INPUT='CAPFN SILENT'
                      CALL PROCES
                  END IF
              END IF
              CALL WDIALOGGETINTEGER(IDF_WAVNUM,ISET)
              WRITE(INPUT,*) 'WAMAP,',ISET
              CALL PROCES
C
            CASE(IDF_AMAP)
              IF(.NOT.CPFNEXT) THEN
                  CALL CHIEFTRACE
                  CALL WDIALOGGETRADIOBUTTON(IDF_CAP1,JSET)
                  IF(JSET.EQ.1) THEN
                      INPUT='CAPFN'
                      CALL PROCES
                  END IF
                  IF(JSET.EQ.2) THEN
                      INPUT='CAPFN PERFECT'
                      CALL PROCES
                  END IF
                  IF(JSET.EQ.3) THEN
                      INPUT='CAPFN SILENT'
                      CALL PROCES
                  END IF
              END IF
              CALL WDIALOGGETINTEGER(IDF_WAVNUM,ISET)
              WRITE(INPUT,*) 'AMAP,',ISET
              CALL PROCES
C
            CASE(IDF_FITZERN)
              IF(.NOT.CPFNEXT) THEN
                  CALL CHIEFTRACE
                  CALL WDIALOGGETRADIOBUTTON(IDF_CAP1,JSET)
                  IF(JSET.EQ.1) THEN
                      INPUT='CAPFN'
                      CALL PROCES
                  END IF
                  IF(JSET.EQ.2) THEN
                      INPUT='CAPFN PERFECT'
                      CALL PROCES
                  END IF
                  IF(JSET.EQ.3) THEN
                      INPUT='CAPFN SILENT'
                      CALL PROCES
                  END IF
              END IF
              CALL WDIALOGGETINTEGER(IDF_WAVFIT,ISET)
              WRITE(INPUT,*) 'FITZERN,',ISET
              CALL PROCES
C
            CASE(IDF_LISTOPD)
              WRITE(INPUT,*) 'LISTOPD'
              CALL PROCES
C
            CASE(IDF_LISTZERN)
              WRITE(INPUT,*) 'LISTZERN'
              CALL PROCES
C
            CASE(IDF_LISTREPT)
              WRITE(INPUT,*) 'LISTREPT'
              CALL PROCES
C
            CASE(IDF_PLOTOPD)
              IF(.NOT.CPFNEXT) THEN
                  CALL CHIEFTRACE
                  CALL WDIALOGGETRADIOBUTTON(IDF_CAP1,JSET)
                  IF(JSET.EQ.1) THEN
                      INPUT='CAPFN'
                      CALL PROCES
                  END IF
                  IF(JSET.EQ.2) THEN
                      INPUT='CAPFN PERFECT'
                      CALL PROCES
                  END IF
                  IF(JSET.EQ.3) THEN
                      INPUT='CAPFN SILENT'
                      CALL PROCES
                  END IF
              END IF
              CALL WDIALOGGETINTEGER(IDF_PLTWAV,ISET)
              CALL WDIALOGGETCHECKBOX(IDF_CAPFNROT,JSET)
              CALL WDIALOGGETDOUBLE(IDF_MAX,DW1)
              CALL WDIALOGGETDOUBLE(IDF_MIN,DW2)
              IF((DW1-DW2).GT.0.0D0) THEN
                  IF(JSET.EQ.0) THEN
                      INPUT='CAPFNROT NO'
                      CALL PROCES
                  END IF
                  IF(JSET.EQ.1) THEN
                      INPUT='CAPFNROT YES'
                      CALL PROCES
                  END IF
                  WRITE(INPUT,*) 'PLOT CAPFNOPD,',ISET,',1,',DW2,',',DW1
                  CALL PROCES
                  CALL GRAPHOUTPUT
              ELSE
                  IF(JSET.EQ.0) THEN
                      INPUT='CAPFNROT NO'
                      CALL PROCES
                  END IF
                  IF(JSET.EQ.1) THEN
                      INPUT='CAPFNROT YES'
                      CALL PROCES
                  END IF
                  WRITE(INPUT,*) 'PLOT CAPFNOPD,',ISET,',1'
                  CALL PROCES
                  CALL GRAPHOUTPUT
              END IF
C
            CASE(IDF_PLOTAPD)
              IF(.NOT.CPFNEXT) THEN
                  CALL CHIEFTRACE
                  CALL WDIALOGGETRADIOBUTTON(IDF_CAP1,JSET)
                  IF(JSET.EQ.1) THEN
                      INPUT='CAPFN'
                      CALL PROCES
                  END IF
                  IF(JSET.EQ.2) THEN
                      INPUT='CAPFN PERFECT'
                      CALL PROCES
                  END IF
                  IF(JSET.EQ.3) THEN
                      INPUT='CAPFN SILENT'
                      CALL PROCES
                  END IF
              END IF
              CALL WDIALOGGETINTEGER(IDF_PLTWAV,ISET)
              CALL WDIALOGGETCHECKBOX(IDF_CAPFNROT,JSET)
              IF(JSET.EQ.0) THEN
                  INPUT='CAPFNROT NO'
                  CALL PROCES
              END IF
              IF(JSET.EQ.1) THEN
                  INPUT='CAPFNROT YES'
                  CALL PROCES
              END IF
              WRITE(INPUT,*) 'PLOT CAPFNAPD,',ISET,',1'
              CALL PROCES
              CALL GRAPHOUTPUT
C
            CASE(IDF_CONOPD)
              IF(.NOT.CPFNEXT) THEN
                  CALL CHIEFTRACE
                  CALL WDIALOGGETRADIOBUTTON(IDF_CAP1,JSET)
                  IF(JSET.EQ.1) THEN
                      INPUT='CAPFN'
                      CALL PROCES
                  END IF
                  IF(JSET.EQ.2) THEN
                      INPUT='CAPFN PERFECT'
                      CALL PROCES
                  END IF
                  IF(JSET.EQ.3) THEN
                      INPUT='CAPFN SILENT'
                      CALL PROCES
                  END IF
              END IF
              CALL WDIALOGGETINTEGER(IDF_PLTWAV,ISET)
              WRITE(INPUT,*) 'PLOTCON CAPFNOPD,',ISET,',1'
              CALL PROCES
              CALL GRAPHOUTPUT
C
            CASE(IDF_CONAPD)
              IF(.NOT.CPFNEXT) THEN
                  CALL CHIEFTRACE
                  CALL WDIALOGGETRADIOBUTTON(IDF_CAP1,JSET)
                  IF(JSET.EQ.1) THEN
                      INPUT='CAPFN'
                      CALL PROCES
                  END IF
                  IF(JSET.EQ.2) THEN
                      INPUT='CAPFN PERFECT'
                      CALL PROCES
                  END IF
                  IF(JSET.EQ.3) THEN
                      INPUT='CAPFN SILENT'
                      CALL PROCES
                  END IF
              END IF
              CALL WDIALOGGETINTEGER(IDF_PLTWAV,ISET)
              WRITE(INPUT,*) 'PLOTCON CAPFNAPD,',ISET,',1'
              CALL PROCES
              CALL GRAPHOUTPUT
C
            CASE(IDCANCEL)
              CALL WDialogHide()
              CALL WDialogUnload()
          END SELECT
      END IF
