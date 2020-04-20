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

C       OTHER
C
!        CALL WDialogSelect(IDD_OTHER)
!        J=EDIT_SURFACE
      CALL ITOAA(J,AJ)
!        Call WDialogPutInteger(IDF_SURFNUM,J)
C
C       PARAX OR REAL
C
!        IF(ALENS(124,EDIT_SURFACE).EQ.0.0D0)
!     1  CALL WDIALOGPUTRADIOBUTTON(IDF_REAL1)
!        IF(ALENS(124,EDIT_SURFACE).NE.0.0D0)
!     1  CALL WDIALOGPUTRADIOBUTTON(IDF_REAL2)
C
C       FOOT PRINT BLOCKING
C
!        IF(ALENS(58,EDIT_SURFACE).EQ.0.0D0)
!     1  CALL WDIALOGPUTRADIOBUTTON(IDF_FP1)
!        IF(ALENS(58,EDIT_SURFACE).NE.0.0D0)
!     1  CALL WDIALOGPUTRADIOBUTTON(IDF_FP2)
C
C       FORCED DUMMY
C
!        IF(ALENS(68,EDIT_SURFACE).EQ.0.0D0)
!     1  CALL WDIALOGPUTRADIOBUTTON(IDF_DUM1)
!        IF(ALENS(68,EDIT_SURFACE).NE.0.0D0)
!     1  CALL WDIALOGPUTRADIOBUTTON(IDF_DUM2)
C
C       INR
C
!        IF(ALENS(143,EDIT_SURFACE).EQ.0.0D0)
!     1  CALL WDIALOGPUTRADIOBUTTON(IDF_INR1)
!        IF(ALENS(143,EDIT_SURFACE).EQ.1.0D0)
!     1  CALL WDIALOGPUTRADIOBUTTON(IDF_INR2)
!        CALL WDIALOGPUTDOUBLE(IDF_INR,ALENS(76,EDIT_SURFACE))
C
C       PRICE, SPGR AND THM
!        CALL WDIALOGPUTDOUBLE(IDF_THM,ALENS(110,EDIT_SURFACE))
!        CALL WDIALOGPUTDOUBLE(IDF_SPGR,ALENS(102,EDIT_SURFACE))
!        CALL WDIALOGPUTDOUBLE(IDF_PRICE,ALENS(111,EDIT_SURFACE))
C
C       LBL
!        CALL WDIALOGPUTSTRING(IDF_LBL,LBL(EDIT_SURFACE)(1:80))
C
C       COATING FILE NUMBER
!        CALL WDIALOGPUTINTEGER(IDF_COATFILENUM,
!     1  INT(ALENS(112,EDIT_SURFACE)))
C
C
C       RAY/SURFACE RANDOM ERROR
!        CALL WDIALOGPUTDOUBLE(IDF_RAYERR,
!     1  ALENS(144,EDIT_SURFACE))
C
!        CALL WDialogShow(-1,-1,0,Modeless)
