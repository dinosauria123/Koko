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

C       SOME OTHER SURFACE PARAMENTER WAS SELECTED
C
C       REMEMBER WHAT SURFACE IS BEING EDITED
      EDIT_SURFACE=CURROW-1
C
      IF(CURCOL.EQ.1) THEN
C       CURVATURE/RADIUS
          IF(RDM)
!     1  CALL WDialogLoad(IDD_SRADIUS)
          IF(.NOT.RDM)
!     1  CALL WDialogLoad(IDD_SCURVATURE)
          CALL CURVATURE_FRESH(EDIT_SURFACE,TEMPCV)
      END IF
C
      IF(CURCOL.EQ.2) THEN
C       THICKNESS
!        Call WDialogLoad(IDD_STHICKNESS)
          CALL THICKNESS_FRESH(EDIT_SURFACE,TEMPTH)
      END IF
C
      IF(CURCOL.EQ.3) THEN
C       SELECTED GLASS
!        Call WDialogLoad(IDD_EDGLASS)
          MNAME27='JUNK                       '
          ONAME27=GLANAM(EDIT_SURFACE,1)//' '//GLANAM(EDIT_SURFACE,2)
          CALL GLASSFRESH(EDIT_SURFACE)
      END IF
C
      IF(CURCOL.EQ.4) THEN
C       SELECTED STOP SURFACE
          OLD26=INT(SYSTEM1(26))
          OLD27=INT(SYSTEM1(27))
          J=EDIT_SURFACE
!        Call WDialogLoad(IDD_STOPSURF)
          CALL ITOAA(J,AJ)
!        Call WDialogPutInteger(IDF_STOPSURFNUM,J)
          IF(OLD26.EQ.J) THEN
C       WE HAVE NOT CHANGED SURFACES
!        IF(OLD27.EQ.0) CALL WDIALOGPUTRADIOBUTTON(IDF_ASTOP1)
!        IF(OLD27.EQ.-1) CALL WDIALOGPUTRADIOBUTTON(IDF_ASTOP2)
!        IF(OLD27.EQ.1) CALL WDIALOGPUTRADIOBUTTON(IDF_ASTOP3)
!        IF(OLD27.EQ.2) CALL WDIALOGPUTRADIOBUTTON(IDF_ASTOP4)
          ELSE
C       NEW ASTOP DEFINITION
              IF(OLD27.EQ.0) CALL WDIALOGPUTRADIOBUTTON(IDF_ASTOP1)
          END IF
!        Call WDialogShow(-1,-1,0,Modeless)
      END IF
C
      IF(CURCOL.EQ.5) THEN
C       SELECTED REFERENCE SURFACE
          OLD25=INT(SYSTEM1(25))
          J=EDIT_SURFACE
!        Call WDialogLoad(IDD_REFSSURF)
          CALL ITOAA(J,AJ)
!        Call WDialogPutInteger(IDF_REFSSURFNUM,J)
!        Call WDialogShow(-1,-1,0,Modeless)
      END IF

      IF(CURCOL.EQ.6) THEN
C       SELECTED CLEAR APERTURES
!        Call WDialogLoad(IDD_CLAPS)
          J=EDIT_SURFACE
          OLD_APETYPE=-1
          APETYPE=-1
          CALL ITOAA(J,AJ)
!        Call WDialogPutInteger(IDF_SURFNUM,J)
          INCLUDE 'FRESHCLAP.INC'
!        Call WDialogShow(-1,-1,0,Modeless)
      END IF
C
C       TILTS AND DECENTERS
C
      IF(CURCOL.EQ.7) THEN
C       SELECTED TILTS AND DECENTERS
!        Call WDialogLoad(IDD_TILTS)
          J=EDIT_SURFACE
          CALL ITOAA(J,AJ)
!        Call WDialogPutInteger(IDF_SURFNUM,J)
          IF(ALENS(25,EDIT_SURFACE).EQ.0.0D0) THEN
!        Call WDialogPutRadioButton(IDF_TILT0)
          END IF
          IF(ALENS(25,EDIT_SURFACE).EQ.1.0D0) THEN
!        Call WDialogPutRadioButton(IDF_TILT1)
          END IF
          IF(ALENS(25,EDIT_SURFACE).EQ.-1.0D0) THEN
!        Call WDialogPutRadioButton(IDF_TILT2)
          END IF
          IF(ALENS(25,EDIT_SURFACE).EQ.2.0D0) THEN
!        Call WDialogPutRadioButton(IDF_TILT3)
          END IF
          IF(ALENS(25,EDIT_SURFACE).EQ.3.0D0) THEN
!        Call WDialogPutRadioButton(IDF_TILT3)
          END IF
          IF(ALENS(25,EDIT_SURFACE).EQ.4.0D0) THEN
!        Call WDialogPutRadioButton(IDF_TILT4)
          END IF
          IF(ALENS(25,EDIT_SURFACE).EQ.5.0D0) THEN
!        Call WDialogPutRadioButton(IDF_TILT5)
          END IF
          IF(ALENS(25,EDIT_SURFACE).EQ.6.0D0) THEN
!        Call WDialogPutRadioButton(IDF_TILT6)
          END IF
          IF(ALENS(25,EDIT_SURFACE).EQ.7.0D0) THEN
!        Call WDialogPutRadioButton(IDF_TILT8)
          END IF
          IF(ALENS(29,EDIT_SURFACE).EQ.1.0D0) THEN
!        Call WDialogPutRadioButton(IDF_TILT7)
          END IF
!        Call WDialogShow(-1,-1,0,Modeless)
      END IF
C
C       TORICS ANS ASPHERICS
C
      IF(CURCOL.EQ.8) THEN
C       SELECTED ASPHERICS AND TORICS
!        Call WDialogLoad(IDD_ASPH)
          J=EDIT_SURFACE
          CALL ITOAA(J,AJ)
          IF(ALENS(1,J).EQ.0.0D0.AND.ALENS(2,J).NE.0.0D0) THEN
              ALENS(2,J)=0.0D0
          END IF
          IF(ALENS(1,J).NE.0.0D0.AND.ALENS(43,J).NE.0.0D0) THEN
              ALENS(43,J)=0.0D0
          END IF
          IF(ALENS(4,J).EQ.0.0D0.AND.ALENS(5,J).EQ.0.0D0.AND.
     1    ALENS(6,J).EQ.0.0D0.AND.ALENS(7,J).EQ.0.0D0.AND.
     1    ALENS(81,J).EQ.0.0D0.AND.ALENS(82,J).EQ.0.0D0.AND.
     1    ALENS(83,J).EQ.0.0D0.AND.ALENS(84,J).EQ.0.0D0.AND.
     1    ALENS(85,J).EQ.0.0D0.AND.ALENS(43,J).EQ.0.0D0) THEN
              ALENS(8,J)=0.0D0
          END IF
!        Call WDialogPutInteger(IDF_SURFNUM,J)
C       LOAD UP VALUES
          IF(ALENS(23,EDIT_SURFACE).EQ.0.0D0)
!     1  CALL WDIALOGPUTRADIOBUTTON(IDF_TR1)
          IF(ALENS(23,EDIT_SURFACE).EQ.1.0D0)
!     1  CALL WDIALOGPUTRADIOBUTTON(IDF_TR2)
          IF(ALENS(23,EDIT_SURFACE).EQ.2.0D0)
!     1  CALL WDIALOGPUTRADIOBUTTON(IDF_TR3)
!        Call WDIALOGPUTDOUBLE(IDF_TORVAL,ALENS(24,EDIT_SURFACE))
!        Call WDIALOGPUTDOUBLE(IDF_CC,ALENS(2,EDIT_SURFACE))
!        Call WDIALOGPUTDOUBLE(IDF_TCC,ALENS(41,EDIT_SURFACE))
!        Call WDIALOGPUTDOUBLE(IDF_2ND,ALENS(43,EDIT_SURFACE))
!        Call WDIALOGPUTDOUBLE(IDF_4TH,ALENS(4,EDIT_SURFACE))
!        Call WDIALOGPUTDOUBLE(IDF_6TH,ALENS(5,EDIT_SURFACE))
!        Call WDIALOGPUTDOUBLE(IDF_8TH,ALENS(6,EDIT_SURFACE))
!        Call WDIALOGPUTDOUBLE(IDF_10TH,ALENS(7,EDIT_SURFACE))
!        Call WDIALOGPUTDOUBLE(IDF_12TH,ALENS(81,EDIT_SURFACE))
!        Call WDIALOGPUTDOUBLE(IDF_14TH,ALENS(82,EDIT_SURFACE))
!        Call WDIALOGPUTDOUBLE(IDF_16TH,ALENS(83,EDIT_SURFACE))
!        Call WDIALOGPUTDOUBLE(IDF_18TH,ALENS(84,EDIT_SURFACE))
!        Call WDIALOGPUTDOUBLE(IDF_20TH,ALENS(85,EDIT_SURFACE))
!        Call WDIALOGPUTDOUBLE(IDF_4THA,ALENS(37,EDIT_SURFACE))
!        Call WDIALOGPUTDOUBLE(IDF_6THA,ALENS(38,EDIT_SURFACE))
!        Call WDIALOGPUTDOUBLE(IDF_8THA,ALENS(39,EDIT_SURFACE))
!        Call WDIALOGPUTDOUBLE(IDF_10THA,ALENS(40,EDIT_SURFACE))
!        Call WDialogShow(-1,-1,0,Modeless)
      END IF
C
C       PICKUPS AND SOLVES
C
      IF(CURCOL.EQ.9) THEN
!        Call WDialogLoad(IDD_PIKSLV)
          CALL PIKFRESH(EDIT_SURFACE)
      END IF
C
C       ARRAY AND GRATING
C
      IF(CURCOL.EQ.10) THEN
!        Call WDialogLoad(IDD_GRTARRAY)
          INCLUDE 'ARRAYGRTFRESH.INC'
      END IF
C
C       SPECIAL SURFACE DATA
C
      IF(CURCOL.EQ.11) THEN
!        Call WDialogLoad(IDD_SPSRF)
          CALL SPSRFFRESH(EDIT_SURFACE)
      END IF
C
C       OTHER
C
      IF(CURCOL.EQ.12) THEN
!        Call WDialogLoad(IDD_OTHER)
          INCLUDE 'OTHERFRESH.INC'
      END IF
C
C       ZERO
C
      IF(CURCOL.EQ.13) THEN
!        Call WDialogLoad(IDD_ZERO)
          INCLUDE 'ZEROFRESH.INC'
      END IF
