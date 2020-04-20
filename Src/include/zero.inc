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

C       THIS CODE TAKES CARE OF THE OPERATION OF ZEROING
C       LENS SURFACE DATA
C
      IF(COMMAND.EQ.'ZERO') THEN
          J=EDIT_SURFACE
!        CALL WDIALOGSELECT(ZERO)
          SELECT CASE(MESSAGE)

            CASE ('IDOK')
C
C       MAKE LENS CHANGES HERE
C
              INPUT='U L'
              CALL PROCES
              CALL ITOAA(EDIT_SURFACE,AJ)
              INPUT='CHG '//AJ
              CALL PROCES
              INPUT='ZERO'
              CALL PROCES
              INPUT='EOS'
              CALL PROCES
              INPUT='OUT TP'
              CALL PROCES
!        CALL WDialogHide()
!        CALL WDialogUnload()
              INCLUDE 'LOADSHEET.INC'
!        CALL WDialogShow(-1,-1,0,Modeless)
C

            CASE ('IDCANCEL')
!        CALL WDIALOGSELECT(ZERO)
!        CALL WDialogShow(-1,-1,0,Modeless)
!        CALL WDialogHide()
!        CALL WDialogUnload()
              INCLUDE 'LOADSHEET.INC'
!        CALL WDialogShow(-1,-1,0,Modeless)
C
          END SELECT
      END IF
