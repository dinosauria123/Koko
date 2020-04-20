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

C       THIS CODE TAKES CARE OF THE OPERATION OF THE OTHER
C       LENS PARAMETERS
C
      IF(COMMAND.EQ.'OTHER') THEN
          J=EDIT_SURFACE
!        Call WDIALOGSELECT(OTHER)
          SELECT CASE(MESSAGE)

            CASE ('IDOK')
C
C       MAKE LENS CHANGES HERE
C
!        Call WDialogGetDouble(IDF_INR,DW1)
!        Call WDialogGetInteger(IDF_COATFILENUM,MSET)
!        Call WDialogGetRadioButton(IDF_REAL1,ISET)
!        Call WDialogGetString(IDF_LBL,STRINGER)
!        Call WDialogGetRadioButton(IDF_FP1,JSET)
!        Call WDialogGetRadioButton(IDF_DUM1,KSET)
!        Call WDialogGetRadioButton(IDF_INR1,LSET)
!        Call WDialogGetDouble(IDF_RAYERR,DW2)
              INPUT='U L'
              CALL PROCES
              CALL ITOAA(EDIT_SURFACE,AJ)
              INPUT='CHG '//AJ
              CALL PROCES
              IF(ISET.EQ.1) INPUT='REAL'
              IF(ISET.EQ.2) INPUT='PARAX'
              CALL PROCES
              IF(JSET.EQ.1) INPUT='FOOTBLOK NO'
              IF(JSET.EQ.2) INPUT='FOOTBLOK YES'
              CALL PROCES
              IF(KSET.EQ.1) INPUT='NODUM NO'
              IF(KSET.EQ.2) INPUT='NODUM YES'
              CALL PROCES
              IF(GLANAM(EDIT_SURFACE,2).EQ.'REFL') THEN
!        Call WDialogGetDouble(IDF_THM,DW1)
                  CALL DTOA23(DW1,AW1)
                  INPUT='THM,'//AW1
                  CALL PROCES
              END IF
!        Call WDialogGetDouble(IDF_SPGR,DW1)
              CALL DTOA23(DW1,AW1)
              INPUT='SPGR,'//AW1
              CALL PROCES
!        Call WDialogGetDouble(IDF_PRICE,DW1)
              CALL DTOA23(DW1,AW1)
              INPUT='PRICE,'//AW1
              CALL PROCES
C
              IF(LSET.EQ.2) THEN
C       EXPLICIT CHANGE
!        Call WDialogGetDouble(IDF_INR,DW1)
                  CALL DTOA23(DW1,AW1)
                  INPUT='INR,'//AW1
                  CALL PROCES
              END IF
              IF(LSET.EQ.1.AND.ALENS(143,EDIT_SURFACE).EQ.1.0D0) THEN
C       WAS EXPLICIT BUT NOW MAKE IT IMPLICIT
                  INPUT='INRD'
                  CALL PROCES
              END IF
C     RANDOM RAY ERROR
              CALL DTOA23(DW2,AW2)
              INPUT='RAYERROR,'//AW2
              CALL PROCES

C       LBL
              INPUT='LBL,'//STRINGER
              CALL PROCES
C
C       COATING FILE NUMBER
              WRITE(INPUT,*) 'COATING,',MSET
              CALL PROCES
C
              INPUT='EOS'
              CALL PROCES
              INPUT='OUT TP'
              CALL PROCES
C
              INCLUDE 'LOADSHEET.INC'
              INCLUDE 'OTHERFRESH.INC'
!        Call WDialogShow(-1,-1,0,Modeless)
!        Call WDIALOGSELECT(OTHER)
!        Call WDialogShow(-1,-1,0,Modeless)
C       CALL WDialogHide()
C       CALL WDialogUnload()
C       INCLUDE 'LOADSHEET.INC'
C       CALL WDialogShow(-1,-1,0,Modeless)

            CASE ('IDCANCEL')
!        Call WDialogShow(-1,-1,0,Modeless)
!        Call WDIALOGSELECT(OTHER)
!        Call WDialogShow(-1,-1,0,Modeless)
!        Call WDialogHide()
!        Call WDialogUnload()
              INCLUDE 'LOADSHEET.INC'
!        Call WDialogShow(-1,-1,0,Modeless)
C
          END SELECT
      END IF
C
