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

C       THIS CODE TAKES CARE OF THE OPERATION OF THE
C       ARRAY LENS AND DIFFRACTION GRATING PARAMETERS
C
      IF(COMMAND.EQ.'GRTARRAY') THEN
          J=EDIT_SURFACE
!        Call WDIALOGSELECT(GRTARRAY)
          SELECT CASE(MESSAGE)

            CASE ('IDOK')
C
C       MAKE LENS CHANGES HERE
C
!        Call WDialogGetRadioButton(GRT1,ISET)
!        Call WDialogGetRadioButton(ARRAY1,JSET)
!        Call WDialogGetRadioButton(EVENODD1,KSET)
C
              IF(ISET.EQ.1) THEN
C       NO GRATING
                  IF(ALENS(96,J).NE.0.0D0) THEN
C       DELETE THE GRATING DEFINITION
                      INPUT='U L'
                      CALL PROCES
                      CALL ITOAA(J,AJ)
                      INPUT='CHG,'//AJ
                      CALL PROCES
                      INPUT='GRTD'
                      CALL PROCES
                      INPUT='EOS'
                      CALL PROCES
                      INPUT='OUT TP'
                      CALL PROCES
                  END IF
              END IF
              IF(ISET.EQ.2) THEN
C       REQUEST GRATING ASSIGNMENT
                  INPUT='U L'
                  CALL PROCES
                  CALL ITOAA(J,AJ)
                  INPUT='CHG,'//AJ
                  CALL PROCES
!        Call WDialogGetDouble(GRO,DW1)
                  CALL DTOA23(DW1,AW1)
!        Call WDialogGetDouble(GRS,DW2)
                  CALL DTOA23(DW2,AW2)
!        Call WDialogGetDouble(GRX,DW3)
                  CALL DTOA23(DW3,AW3)
!        Call WDialogGetDouble(GRY,DW4)
                  CALL DTOA23(DW4,AW4)
!        Call WDialogGetDouble(GRZ,DW5)
                  CALL DTOA23(DW5,AW5)
                  INPUT='GRT'
                  CALL PROCES
                  INPUT='GRO,'//AW1
                  CALL PROCES
                  INPUT='GRS,'//AW2
                  CALL PROCES
                  INPUT='GRX,'//AW3
                  CALL PROCES
                  INPUT='GRY,'//AW4
                  CALL PROCES
                  INPUT='GRZ,'//AW5
                  CALL PROCES
                  INPUT='EOS'
                  CALL PROCES
                  INPUT='OUT TP'
                  CALL PROCES
              END IF
              IF(ISET.EQ.3) THEN
C       REQUEST GRATING DELETTION
                  IF(ALENS(96,J).NE.0.0D0) THEN
C       DELETE THE GRATING DEFINITION
                      INPUT='U L'
                      CALL PROCES
                      CALL ITOAA(J,AJ)
                      INPUT='CHG,'//AJ
                      CALL PROCES
                      INPUT='GRTD'
                      CALL PROCES
                      INPUT='EOS'
                      CALL PROCES
                      INPUT='OUT TP'
                      CALL PROCES
                  END IF
              END IF
C
C       ARRAY LENS
C
              IF(JSET.EQ.1) THEN
C       NO ARRAY DEFINITION
                  IF(ALENS(133,J).NE.0.0D0) THEN
C       DELETE ARRAY LENS DEFINITION
                      INPUT='U L'
                      CALL PROCES
                      CALL ITOAA(J,AJ)
                      INPUT='CHG,'//AJ
                      CALL PROCES
                      INPUT='ARRAYD'
                      CALL PROCES
                      INPUT='EOS'
                      CALL PROCES
                      INPUT='OUT TP'
                      CALL PROCES
                  END IF
              END IF
              IF(JSET.EQ.2) THEN
C       REQUEST ARRAY LENS ASSIGNMENT
                  INPUT='U L'
                  CALL PROCES
                  CALL ITOAA(J,AJ)
                  INPUT='CHG,'//AJ
                  CALL PROCES
!        Call WDialogGetDouble(DX,DW1)
                  CALL DTOA23(DW1,AW1)
!        Call WDialogGetDouble(DY,DW2)
                  CALL DTOA23(DW2,AW2)
                  IF(KSET.EQ.1) THEN
C       ODD ARRAY
                      INPUT='ARRAY ODD,'//AW1//','//AW2
                      CALL PROCES
                  END IF
                  IF(KSET.EQ.2) THEN
C       ODD ARRAY
                      INPUT='ARRAY EVEN,'//AW1//','//AW2
                      CALL PROCES
                  END IF
                  INPUT='EOS'
                  CALL PROCES
                  INPUT='OUT TP'
                  CALL PROCES
              END IF
              IF(JSET.EQ.3) THEN
C       REQUEST ARRAY DEFINITION DELETTION
                  IF(ALENS(133,J).NE.0.0D0) THEN
C       DELETE THE ARRAY LENS DEFINITION
                      INPUT='U L'
                      CALL PROCES
                      CALL ITOAA(J,AJ)
                      INPUT='CHG,'//AJ
                      CALL PROCES
                      INPUT='ARRAYD'
                      CALL PROCES
                      INPUT='EOS'
                      CALL PROCES
                      INPUT='OUT TP'
                      CALL PROCES
                  END IF
              END IF
C
              INCLUDE 'LOADSHEET.INC'
              INCLUDE 'ARRAYGRTFRESH.INC'
!        Call WDIALOGSELECT(GRTARRAY)
!        Call WDialogShow(-1,-1,0,Modeless)

            CASE ('IDCANCEL')
!        Call WDialogShow(-1,-1,0,Modeless)
!        Call WDIALOGSELECT(GRTARRAY)
!        Call WDialogShow(-1,-1,0,Modeless)
!        Call WDialogHide()
!        Call WDialogUnload()
              INCLUDE 'LOADSHEET.INC'
!        Call WDialogShow(-1,-1,0,Modeless)
C
          END SELECT
      END IF
C
