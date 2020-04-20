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

C       ADVANCED RAY SETTINGS DIALOG HANDLER
C
      IF(COMMAND.EQ.'RAYSETTINGS') THEN
          !       CALL WDIALOGSELECT(RAYSETTINGS)
          SELECT CASE(MESSAGE)
            CASE ('IDOK')
C       RADIO BUTTON STATUS
C       SURTOL
              !       CALL WDIALOGGETDOUBLE(SURTOL,SURTOL)
C       AIMTOL
              !       CALL WDIALOGGETDOUBLE(AIMTOL,AIMTOL)
C       CAIMTOL
              !       CALL WDIALOGGETDOUBLE(CAIMTOL,CAIMTOL)
C       MRAYS
              !       CALL WDIALOGGETINTEGER(MRAYS,MRAYS)
C       NRAITR
              !       CALL WDIALOGGETINTEGER(NRAITR,NRAITR)
C       NEWOBJ
              !       CALL WDIALOGGETINTEGER(NEWOBJ,NEWOBJ)
C       NEWOBJ
              !       CALL WDIALOGGETINTEGER(NEWREF,NEWREF)
C       NEWIMG
              !       CALL WDIALOGGETINTEGER(NEWIMG,NEWIMG)
C       RESET TO DEFAULTS CHECKBOX
              !       CALL WDIALOGGETCHECKBOX(DEFAULTS,ISET)
              IF(ISET.EQ.1) THEN
                  AIMTOL=1.0D-8
                  CAIMTOL=1.0D-3
                  SURTOL=1.0D-8
                  NRAITR=100
                  MRAYS=200
                  !       CALL WDIALOGPUTDOUBLE(SURTOL,SURTOL)
C       MRAYS
                  !       CALL WDIALOGPUTINTEGER(MRAYS,MRAYS)
C       AIMTOL
                  !       CALL WDIALOGPUTDOUBLE(AIMTOL,AIMTOL)
C       CAIMTOL
                  !       CALL WDIALOGPUTDOUBLE(CAIMTOL,CAIMTOL)
C       NRAITR
                  !       CALL WDIALOGPUTINTEGER(NRAITR,NRAITR)
C       RESET TO DEFAULTS CHECKBOX
                  !       CALL WDIALOGGETCHECKBOX(DEFAULTS,ISET)
              END IF
              OUTLYNE='RAY SETTINGS HAVE BEEN ADJUSTED'
              CALL SHOWIT(0)
C
            CASE('IDCANCEL')
              !       CALL WDialogHide()
              !       CALL WDialogUnload()
          END SELECT
      END IF
