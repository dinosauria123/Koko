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

      SUBROUTINE THICKNESS_FRESH(J,TEMPTH)
!        USE WINTERACTER
          IMPLICIT NONE
          INTEGER J
          REAL*8 TEMPTH
          INCLUDE 'resource.inc'
          INCLUDE 'datlen.inc'
          INCLUDE 'datmac.inc'
          INCLUDE 'datmai.inc'
!        CALL WDIALOGSELECT(IDD_STHICKNESS)
          !       CALL WDIALOGPUTINTEGER(IDF_SURFNUM,J)
C
          TEMPTH=ALENS(3,J)
          !       CALL WDIALOGPUTDOUBLE(IDF_VAL,TEMPTH)
          !       CALL WDialogShow(-1,-1,0,Modeless)
          RETURN
      END
