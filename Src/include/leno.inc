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
      IF(COMMAND.EQ.'LENOCV') THEN
          !       CALL WDIALOGSELECT(LENOCV)
C
!        SELECT CASE(MESSAGE)
C
          !       CASE ('IDOK')
          !       CALL WDIALOGGETSTRING(FILE,FILENAME)
          !       FILENAME=TRIM(FILENAME)
          !       INPUT='OUT FILE '//FILENAME
          !       CALL PROCES
          !       INPUT='LENO CV'
          !       CALL PROCES
          !       INPUT='OUT TP'
          !       CALL PROCES
          WRITE(OUTLYNE,*)
     1    'CURRENT LENS OUTPUT IN CODE-V FORMAT'
          CALL SHOWIT(1)
          WRITE(OUTLYNE,*)
     1    'TO FILE NAMED : '//FILENAME
          CALL SHOWIT(1)
          !       CALL WDialogHide()
          !       CALL WDialogUnload()
C
          !       CASE('IDCANCEL')
          !       CALL WDialogHide()
          !       CALL WDialogUnload()
          !      END SELECT
      END IF
C
      IF(COMMAND.EQ.'LENOACC') THEN
!       CALL WDIALOGSELECT(LENOACC)
C
          SELECT CASE(MESSAGE)
C
            CASE ('IDOK')
!       CALL WDIALOGGETSTRING(FILE,FILENAME)
              FILENAME=TRIM(FILENAME)
              INPUT='OUT FILE '//FILENAME
              CALL PROCES
              INPUT='LENO AC'
              CALL PROCES
              INPUT='OUT TP'
              CALL PROCES
              WRITE(OUTLYNE,*)
     1        'CURRENT LENS OUTPUT IN ACCOS-V FORMAT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)
     1        'TO FILE NAMED : '//FILENAME
              CALL SHOWIT(1)
              !       CALL WDialogHide()
              !       CALL WDialogUnload()
C
            CASE('IDCANCEL')
              !       CALL WDialogHide()
              !       CALL WDialogUnload()
          END SELECT
      END IF
C
      IF(COMMAND.EQ.'IMPORT_CV') THEN
          !       CALL WDIALOGSELECT(IMPORT_CV)
C
          SELECT CASE(MESSAGE)
C
            CASE ('IDOK')
              !       CALL WDIALOGGETSTRING(FILE,FILENAME)
              FILENAME=TRIM(FILENAME)
              INPUT='CV2PRG '//FILENAME
              CALL PROCES
              INPUT='OUT TP'
              CALL PROCES
              WRITE(OUTLYNE,*)
     1        TRIM(FILENAME)//' IMPORTED AS NEW CURRENT LENS'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)
     1        'CHECK CONVERT.ERR FOR ITEMS NOT IMPORTED'
              CALL SHOWIT(1)
              !       CALL WDialogHide()
              !       CALL WDialogUnload()
C
            CASE('IDCANCEL')
              !       CALL WDialogHide()
              !       CALL WDialogUnload()
          END SELECT
      END IF
C
      !       IF(COMMAND.EQ.IMPORT_ZMX) THEN
      !       CALL WDIALOGSELECT(IMPORT_ZMX)
C
      SELECT CASE(MESSAGE)
C
        CASE ('IDOK')
          !       CALL WDIALOGGETSTRING(FILE,FILENAME)
          FILENAME=TRIM(FILENAME)
          INPUT='ZMX2PRG '//HOME//FILENAME
          CALL PROCES
          INPUT='OUT TP'
          CALL PROCES
          WRITE(OUTLYNE,*)
     1    TRIM(FILENAME)//' IMPORTED AS NEW CURRENT LENS'
          CALL SHOWIT(1)
          WRITE(OUTLYNE,*)
     1    'CHECK CONVERT.ERR FOR ITEMS NOT IMPORTED'
          CALL SHOWIT(1)
          !       CALL WDialogHide()
          !       CALL WDialogUnload()
C
        CASE('IDCANCEL')
          !       CALL WDialogHide()
          !       CALL WDialogUnload()
      END SELECT
      END IF
