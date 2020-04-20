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

!        Call WDialogPutInteger(IDF_MAXAPE,INT(ALENS(127,EDIT_SURFACE)))
!        Call WDialogPutInteger(IDF_NUMAPE,0)
!        Call WDialogPutInteger(IDF_MAXOBS,INT(ALENS(128,EDIT_SURFACE)))
!        Call WDialogPutInteger(IDF_NUMOBS,0)
      IF(ALENS(9,EDIT_SURFACE).EQ.0.0D0) THEN
          OLD_APETYPE=0
!        Call WDialogPutRadioButton(IDF_APE1)
      END IF
      IF(ALENS(9,EDIT_SURFACE).EQ.1.0D0) THEN
          OLD_APETYPE=1
!        Call WDialogPutRadioButton(IDF_APE2)
      END IF
      IF(ALENS(9,EDIT_SURFACE).EQ.2.0D0) THEN
          OLD_APETYPE=2
!        Call WDialogPutRadioButton(IDF_APE3)
      END IF
      IF(ALENS(9,EDIT_SURFACE).EQ.3.0D0) THEN
          OLD_APETYPE=3
!        Call WDialogPutRadioButton(IDF_APE4)
      END IF
      IF(ALENS(9,EDIT_SURFACE).EQ.4.0D0) THEN
          OLD_APETYPE=4
!        Call WDialogPutRadioButton(IDF_APE5)
      END IF
      IF(ALENS(9,EDIT_SURFACE).EQ.5.0D0) THEN
          OLD_APETYPE=5
!        Call WDialogPutRadioButton(IDF_APE6)
      END IF
C
      IF(ALENS(16,EDIT_SURFACE).EQ.0.0D0) THEN
          OLD_COBSTYPE=0
!        Call WDialogPutRadioButton(IDF_COBS1)
      END IF
      IF(ALENS(16,EDIT_SURFACE).EQ.1.0D0) THEN
          OLD_COBSTYPE=1
!        Call WDialogPutRadioButton(IDF_COBS2)
      END IF
      IF(ALENS(16,EDIT_SURFACE).EQ.2.0D0) THEN
          OLD_COBSTYPE=2
!        Call WDialogPutRadioButton(IDF_COBS3)
      END IF
      IF(ALENS(16,EDIT_SURFACE).EQ.3.0D0) THEN
          OLD_COBSTYPE=3
!        Call WDialogPutRadioButton(IDF_COBS4)
      END IF
      IF(ALENS(16,EDIT_SURFACE).EQ.4.0D0) THEN
          OLD_COBSTYPE=4
!        Call WDialogPutRadioButton(IDF_COBS5)
      END IF
      IF(ALENS(16,EDIT_SURFACE).EQ.5.0D0) THEN
          OLD_COBSTYPE=5
!        Call WDialogPutRadioButton(IDF_COBS6)
      END IF
C
      IF(ALENS(51,EDIT_SURFACE).EQ.0.0D0) THEN
          OLD_EAPETYPE=0
!        Call WDialogPutRadioButton(IDF_EAPE1)
      END IF
      IF(ALENS(51,EDIT_SURFACE).EQ.1.0D0) THEN
          OLD_EAPETYPE=1
!        Call WDialogPutRadioButton(IDF_EAPE2)
      END IF
      IF(ALENS(51,EDIT_SURFACE).EQ.2.0D0) THEN
          OLD_EAPETYPE=2
!        Call WDialogPutRadioButton(IDF_EAPE3)
      END IF
      IF(ALENS(51,EDIT_SURFACE).EQ.3.0D0) THEN
          OLD_EAPETYPE=3
!        Call WDialogPutRadioButton(IDF_EAPE4)
      END IF
      IF(ALENS(51,EDIT_SURFACE).EQ.4.0D0) THEN
          OLD_EAPETYPE=4
!        Call WDialogPutRadioButton(IDF_EAPE5)
      END IF
      IF(ALENS(51,EDIT_SURFACE).EQ.5.0D0) THEN
          OLD_EAPETYPE=5
!        Call WDialogPutRadioButton(IDF_EAPE6)
      END IF
C
      IF(ALENS(61,EDIT_SURFACE).EQ.0.0D0) THEN
          OLD_ ECOBSTYPE=0
!        Call WDialogPutRadioButton(IDF_ECOBS1)
      END IF
      IF(ALENS(61,EDIT_SURFACE).EQ.1.0D0) THEN
          OLD_ ECOBSTYPE=1
!        Call WDialogPutRadioButton(IDF_ECOBS2)
      END IF
      IF(ALENS(61,EDIT_SURFACE).EQ.2.0D0) THEN
          OLD_ ECOBSTYPE=2
!        Call WDialogPutRadioButton(IDF_ECOBS3)
      END IF
      IF(ALENS(61,EDIT_SURFACE).EQ.3.0D0) THEN
          OLD_ ECOBSTYPE=3
!        Call WDialogPutRadioButton(IDF_ECOBS4)
      END IF
      IF(ALENS(61,EDIT_SURFACE).EQ.4.0D0) THEN
          OLD_ ECOBSTYPE=4
!        Call WDialogPutRadioButton(IDF_ECOBS5)
      END IF
      IF(ALENS(61,EDIT_SURFACE).EQ.5.0D0) THEN
          OLD_ ECOBSTYPE=5
!        Call WDialogPutRadioButton(IDF_ECOBS6)
      END IF
