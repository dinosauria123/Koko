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

C       THIS CODE TAKES CARE OF THE OPERATION OF THE GLASSP DIALOG
C
!        IF(MESSAGE%WIN.EQ.IDD_GLASSP) THEN
!        CALL WDIALOGSELECT(IDD_GLASSP)
C
C
C
!        SELECT CASE(MESSAGE%VALUE1)
C
!        CASE (IDF_ALLGLASS)
C
C       GENERATE A GLASSP ARRAY
C       LOAD UP THE DISPLAY
!        CALL WDIALOGCLEARFIELD(IDF_N1)
!        CALL WDIALOGCLEARFIELD(IDF_N2)
!        CALL WDIALOGCLEARFIELD(IDF_N3)
!        CALL WDIALOGCLEARFIELD(IDF_N4)
!        CALL WDIALOGCLEARFIELD(IDF_N5)
!        CALL GCATLOAD(CATLIST)
!        CALL WDialogPutMenu(IDF_GLASSP,CATARRAY,CATLIST,1)
!       CALL WDIALOGFIELDSTATE(IDF_GLASSP,1)
      INCLUDE 'GLASSPFRESH.INC'

!         CASE (IDF_ONEGLASS)

!         CALL WDIALOGGETMENU(IDF_GLASSP,IOPTION,CVALUE)
!        IF(IOPTION.NE.0) THEN
!         CALL GCATLOAD2(CATLIST,CVALUE)
!                        DO I=CATLIST,1,-1
!                        CATARRAY(I+1)=CATARRAY(I)
!                        END DO
!                        CATARRAY(1)=CVALUE(1:50)
!                        CATLIST=CATLIST+1
!        CALL WDialogPutMenu(IDF_GLASSP,CATARRAY,CATLIST,1)
!        CALL WDIALOGFIELDSTATE(IDF_GLASSP,1)
!        CALL WDIALOGGETDOUBLE(IDF_WV1,GLSWV(1))
!        CALL WDIALOGGETDOUBLE(IDF_WV2,GLSWV(2))
!        CALL WDIALOGGETDOUBLE(IDF_WV3,GLSWV(3))
!        CALL WDIALOGGETDOUBLE(IDF_WV4,GLSWV(4))
!        CALL WDIALOGGETDOUBLE(IDF_WV5,GLSWV(5))
!        INPUT='OUT NULL'
!        CALL PROCES
!        INPUT=CATARRAY(1)
!        CALL PROCES
!        INPUT='OUT TP'
!        CALL PROCES
!        CALL WDIALOGPUTDOUBLE(IDF_N1,GPREG(1))
!        CALL WDIALOGPUTDOUBLE(IDF_N2,GPREG(2))
!        CALL WDIALOGPUTDOUBLE(IDF_N3,GPREG(3))
!        CALL WDIALOGPUTDOUBLE(IDF_N4,GPREG(4))
!        CALL WDIALOGPUTDOUBLE(IDF_N5,GPREG(5))
      INCLUDE 'GLASSPFRESH.INC'
!                         END IF

!        CASE(IDCANCEL)
!        CALL WDialogHide()
!        CALL WDialogUnload()
!        END SELECT
!                        END IF
