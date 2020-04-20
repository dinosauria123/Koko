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

C       THIS CODE TAKES CARE OF THE OPERATION OF
C       TORICS AND ASPHERICS
C
      IF(COMMAND.EQ.'ASPH') THEN
          J=EDIT_SURFACE
!        CALL WDIALOGSELECT(ASPH)

          SELECT CASE(MESSAGE)

            CASE ('IDOK')
C
C       GET VALUES AND MAKE APPROPRIATE CHANGES
C
C       TORIC PRESENCE
!        CALL WDIALOGGETRADIOBUTTON(TR1,ISET)
C       1 NO TORIC
C       2 YTORIC
C       3 XTORIC

C       TORIC VALUE MODE
!        CALL WDIALOGGETRADIOBUTTON(T1,JSET)
C       1 CURVATURE
C       2 RADIUS
C
              IF(ISET.EQ.1) THEN
C       NONTORIC

C       CONICS
!        CALL WDIALOGGETDOUBLE(CC,DW1)
                  CALL DTOA23(DW1,AW1)
                  IF(ALENS(1,J).NE.0.0D0) THEN
                      INPUT='U L'
                      CALL PROCES
                      J=EDIT_SURFACE
                      CALL ITOAA(J,AJ)
                      INPUT='CHG '//AJ
                      CALL PROCES
                      INPUT='CC '//AW1
                      CALL PROCES
                      INPUT='EOS'
                      CALL PROCES
                  ELSE
C       NO CHANGE
                  END IF
C
C       ASPHERICS
C       2ND
!        CALL WDIALOGGETDOUBLE(2ND,DW1)
                  CALL DTOA23(DW1,AW1)
                  IF(ALENS(1,J).EQ.0.0D0) THEN
                      INPUT='U L'
                      CALL PROCES
                      J=EDIT_SURFACE
                      CALL ITOAA(J,AJ)
                      IF(ALENS(8,J).EQ.0.0D0) THEN
                          INPUT='CHG '//AJ
                          CALL PROCES
                          INPUT='ASPH'
                          CALL PROCES
                      END IF
                      INPUT='CHG '//AJ
                      CALL PROCES
                      INPUT='AC '//AW1
                      CALL PROCES
                      INPUT='EOS'
                      CALL PROCES
                  ELSE
C       NO CHANGE
                  END IF
C
C       4TH
!        CALL WDIALOGGETDOUBLE(4TH,DW1)
                  CALL DTOA23(DW1,AW1)
                  INPUT='U L'
                  CALL PROCES
                  J=EDIT_SURFACE
                  CALL ITOAA(J,AJ)
                  IF(ALENS(8,J).EQ.0.0D0) THEN
                      INPUT='CHG '//AJ
                      CALL PROCES
                      INPUT='ASPH'
                      CALL PROCES
                  END IF
                  INPUT='CHG '//AJ
                  CALL PROCES
                  INPUT='AD '//AW1
                  CALL PROCES
                  INPUT='EOS'
                  CALL PROCES
C
C       6TH
!        CALL WDIALOGGETDOUBLE(6TH,DW1)
                  CALL DTOA23(DW1,AW1)
                  INPUT='U L'
                  CALL PROCES
                  J=EDIT_SURFACE
                  CALL ITOAA(J,AJ)
                  IF(ALENS(8,J).EQ.0.0D0) THEN
                      INPUT='CHG '//AJ
                      CALL PROCES
                      INPUT='ASPH'
                      CALL PROCES
                  END IF
                  INPUT='CHG '//AJ
                  CALL PROCES
                  INPUT='AE '//AW1
                  CALL PROCES
                  INPUT='EOS'
                  CALL PROCES
C
C       8TH
!        CALL WDIALOGGETDOUBLE(8TH,DW1)
                  CALL DTOA23(DW1,AW1)
                  INPUT='U L'
                  CALL PROCES
                  J=EDIT_SURFACE
                  CALL ITOAA(J,AJ)
                  IF(ALENS(8,J).EQ.0.0D0) THEN
                      INPUT='CHG '//AJ
                      CALL PROCES
                      INPUT='ASPH'
                      CALL PROCES
                  END IF
                  INPUT='CHG '//AJ
                  CALL PROCES
                  INPUT='AF '//AW1
                  CALL PROCES
                  INPUT='EOS'
                  CALL PROCES
C
C       10TH
!        CALL WDIALOGGETDOUBLE(10TH,DW1)
                  CALL DTOA23(DW1,AW1)
                  INPUT='U L'
                  CALL PROCES
                  J=EDIT_SURFACE
                  CALL ITOAA(J,AJ)
                  IF(ALENS(8,J).EQ.0.0D0) THEN
                      INPUT='CHG '//AJ
                      CALL PROCES
                      INPUT='ASPH'
                      CALL PROCES
                  END IF
                  INPUT='CHG '//AJ
                  CALL PROCES
                  INPUT='AG '//AW1
                  CALL PROCES
                  INPUT='EOS'
                  CALL PROCES
C
C       12TH
!        CALL WDIALOGGETDOUBLE(12TH,DW1)
                  CALL DTOA23(DW1,AW1)
                  INPUT='U L'
                  CALL PROCES
                  J=EDIT_SURFACE
                  CALL ITOAA(J,AJ)
                  IF(ALENS(8,J).EQ.0.0D0) THEN
                      INPUT='CHG '//AJ
                      CALL PROCES
                      INPUT='ASPH'
                      CALL PROCES
                  END IF
                  INPUT='CHG '//AJ
                  CALL PROCES
                  INPUT='AH '//AW1
                  CALL PROCES
                  INPUT='EOS'
                  CALL PROCES
C
C       14TH
!        CALL WDIALOGGETDOUBLE(14TH,DW1)
                  CALL DTOA23(DW1,AW1)
                  INPUT='U L'
                  CALL PROCES
                  J=EDIT_SURFACE
                  CALL ITOAA(J,AJ)
                  IF(ALENS(8,J).EQ.0.0D0) THEN
                      INPUT='CHG '//AJ
                      CALL PROCES
                      INPUT='ASPH'
                      CALL PROCES
                  END IF
                  INPUT='CHG '//AJ
                  CALL PROCES
                  INPUT='AI '//AW1
                  CALL PROCES
                  INPUT='EOS'
                  CALL PROCES
C
C       16TH
!        CALL WDIALOGGETDOUBLE(16TH,DW1)
                  CALL DTOA23(DW1,AW1)
                  INPUT='U L'
                  CALL PROCES
                  J=EDIT_SURFACE
                  CALL ITOAA(J,AJ)
                  IF(ALENS(8,J).EQ.0.0D0) THEN
                      INPUT='CHG '//AJ
                      CALL PROCES
                      INPUT='ASPH'
                      CALL PROCES
                  END IF
                  INPUT='CHG '//AJ
                  CALL PROCES
                  INPUT='AJ '//AW1
                  CALL PROCES
                  INPUT='EOS'
                  CALL PROCES
C
C       18TH
!        CALL WDIALOGGETDOUBLE(18TH,DW1)
                  CALL DTOA23(DW1,AW1)
                  INPUT='U L'
                  CALL PROCES
                  J=EDIT_SURFACE
                  CALL ITOAA(J,AJ)
                  IF(ALENS(8,J).EQ.0.0D0) THEN
                      INPUT='CHG '//AJ
                      CALL PROCES
                      INPUT='ASPH'
                      CALL PROCES
                  END IF
                  INPUT='CHG '//AJ
                  CALL PROCES
                  INPUT='AK '//AW1
                  CALL PROCES
                  INPUT='EOS'
                  CALL PROCES
C
C       20TH
!        CALL WDIALOGGETDOUBLE(20TH,DW1)
                  CALL DTOA23(DW1,AW1)
                  INPUT='U L'
                  CALL PROCES
                  J=EDIT_SURFACE
                  CALL ITOAA(J,AJ)
                  IF(ALENS(8,J).EQ.0.0D0) THEN
                      INPUT='CHG '//AJ
                      CALL PROCES
                      INPUT='ASPH'
                      CALL PROCES
                  END IF
                  INPUT='CHG '//AJ
                  CALL PROCES
                  INPUT='AL '//AW1
                  CALL PROCES
                  INPUT='EOS'
                  CALL PROCES
              END IF
C
              IF(ISET.EQ.2.OR.ISET.EQ.3) THEN
C       YTORIC
!        CALL WDIALOGGETDOUBLE(TORVAL,DW1)
                  CALL DTOA23(DW1,AW1)
                  INPUT='U L'
                  CALL PROCES
                  J=EDIT_SURFACE
                  CALL ITOAA(J,AJ)
                  INPUT='CHG '//AJ
                  CALL PROCES
                  IF(ISET.EQ.2) INPUT='YTORIC'
                  IF(ISET.EQ.3) INPUT='XTORIC'
                  CALL PROCES
                  IF(JSET.EQ.1) INPUT='RDTOR '//AW1
                  IF(JSET.EQ.2) INPUT='CVTOR '//AW1
                  CALL PROCES
                  INPUT='EOS'
                  CALL PROCES
C
C       TORIC CONICS
!        CALL WDIALOGGETDOUBLE(TCC,DW1)
                  CALL DTOA23(DW1,AW1)
                  IF(ALENS(24,J).NE.0.0D0) THEN
                      INPUT='U L'
                      CALL PROCES
                      J=EDIT_SURFACE
                      CALL ITOAA(J,AJ)
                      INPUT='CHG '//AJ
                      CALL PROCES
                      INPUT='CCTOR '//AW1
                      CALL PROCES
                      INPUT='EOS'
                      CALL PROCES
                  ELSE
C       NO CHANGE
                  END IF
C
C       4THA
!        CALL WDIALOGGETDOUBLE(4THA,DW1)
                  CALL DTOA23(DW1,AW1)
                  IF(ALENS(4,J).NE.0.0D0) THEN
                      INPUT='U L'
                      CALL PROCES
                      J=EDIT_SURFACE
                      CALL ITOAA(J,AJ)
                      IF(ALENS(36,J).EQ.0.0D0) THEN
                          INPUT='CHG '//AJ
                          CALL PROCES
                          INPUT='TASPH'
                          CALL PROCES
                      END IF
                      INPUT='CHG '//AJ
                      CALL PROCES
                      INPUT='ADTOR '//AW1
                      CALL PROCES
                      INPUT='EOS'
                      CALL PROCES
                  ELSE
C       NO CHANGE
                  END IF
C
C       6THA
!        CALL WDIALOGGETDOUBLE(6THA,DW1)
                  CALL DTOA23(DW1,AW1)
                  IF(ALENS(5,J).NE.0.0D0) THEN
                      INPUT='U L'
                      CALL PROCES
                      J=EDIT_SURFACE
                      CALL ITOAA(J,AJ)
                      IF(ALENS(36,J).EQ.0.0D0) THEN
                          INPUT='CHG '//AJ
                          CALL PROCES
                          INPUT='TASPH'
                          CALL PROCES
                      END IF
                      INPUT='CHG '//AJ
                      CALL PROCES
                      INPUT='AETOR '//AW1
                      CALL PROCES
                      INPUT='EOS'
                      CALL PROCES
                  ELSE
C       NO CHANGE
                  END IF
C
C       8THA
!        CALL WDIALOGGETDOUBLE(8THA,DW1)
                  CALL DTOA23(DW1,AW1)
                  IF(ALENS(6,J).NE.0.0D0) THEN
                      INPUT='U L'
                      CALL PROCES
                      J=EDIT_SURFACE
                      CALL ITOAA(J,AJ)
                      IF(ALENS(36,J).EQ.0.0D0) THEN
                          INPUT='CHG '//AJ
                          CALL PROCES
                          INPUT='TASPH'
                          CALL PROCES
                      END IF
                      INPUT='CHG '//AJ
                      CALL PROCES
                      INPUT='AFTOR '//AW1
                      CALL PROCES
                      INPUT='EOS'
                      CALL PROCES
                  ELSE
C       NO CHANGE
                  END IF
C
C       10THA
!        CALL WDIALOGGETDOUBLE(10THA,DW1)
                  CALL DTOA23(DW1,AW1)
                  IF(ALENS(7,J).NE.0.0D0) THEN
                      INPUT='U L'
                      CALL PROCES
                      J=EDIT_SURFACE
                      CALL ITOAA(J,AJ)
                      IF(ALENS(36,J).EQ.0.0D0) THEN
                          INPUT='CHG '//AJ
                          CALL PROCES
                          INPUT='TASPH'
                          CALL PROCES
                      END IF
                      INPUT='CHG '//AJ
                      CALL PROCES
                      INPUT='AGTOR '//AW1
                      CALL PROCES
                      INPUT='EOS'
                      CALL PROCES
                  ELSE
C       NO CHANGE
                  END IF
              END IF
C
!        CALL WDIALOGSELECT(ASPH)
!        CALL WDialogHide()
!        CALL WDialogUnload()
              INCLUDE 'LOADSHEET.INC'
!        CALL WDialogShow(-1,-1,0,Modeless)

            CASE ('IDCANCEL')
!        CALL WDIALOGSELECT(ASPH)
!        CALL WDialogHide()
!        CALL WDialogUnload()
              INCLUDE 'LOADSHEET.INC'
!        CALL WDialogShow(-1,-1,0,Modeless)


            CASE ('TORD')
C       DELETE ANY EXISTING TORICS
              INPUT='U L'
              CALL PROCES
              J=EDIT_SURFACE
              CALL ITOAA(J,AJ)
              INPUT='CHG '//AJ
              CALL PROCES
              INPUT='TORD'
              CALL PROCES
              INPUT='EOS'
              CALL PROCES
!        CALL WDIALOGPUTRADIOBUTTON(TR1)
!        CALL WDIALOGPUTDOUBLE(TORVAL,0.0D0)
              INCLUDE 'LOADSHEET.INC'
!        CALL WDIALOGSELECT(ASPH)
!        CALL WDialogShow(-1,-1,0,Modeless)
C
            CASE ('ASPHD')
C       DELETE ANY ASPHERICS AND CONICS
              INPUT='U L'
              CALL PROCES
              J=EDIT_SURFACE
              CALL ITOAA(J,AJ)
              INPUT='CHG '//AJ
              CALL PROCES
              INPUT='ASPHD'
              CALL PROCES
              INPUT='CC 0.0D0'
              CALL PROCES
              INPUT='EOS'
              CALL PROCES
!        CALL WDIALOGPUTDOUBLE(2ND,0.0D0)
!        CALL WDIALOGPUTDOUBLE(4TH,0.0D0)
!        CALL WDIALOGPUTDOUBLE(6TH,0.0D0)
!        CALL WDIALOGPUTDOUBLE(8TH,0.0D0)
!        CALL WDIALOGPUTDOUBLE(10TH,0.0D0)
!        CALL WDIALOGPUTDOUBLE(12TH,0.0D0)
!        CALL WDIALOGPUTDOUBLE(14TH,0.0D0)
!        CALL WDIALOGPUTDOUBLE(16TH,0.0D0)
!        CALL WDIALOGPUTDOUBLE(18TH,0.0D0)
!        CALL WDIALOGPUTDOUBLE(20TH,0.0D0)
!        CALL WDIALOGPUTDOUBLE(CC,0.0D0)
              INCLUDE 'LOADSHEET.INC'
!        CALL WDIALOGSELECT(ASPH)
!        CALL WDialogShow(-1,-1,0,Modeless)
C
            CASE ('TASPHD')
C       DELETE ANAMORPHIC ASPHERICS AND TORIC CONIC
              INPUT='U L'
              CALL PROCES
              J=EDIT_SURFACE
              CALL ITOAA(J,AJ)
              INPUT='CHG '//AJ
              CALL PROCES
              INPUT='TASPHD'
              CALL PROCES
              INPUT='CCTOR 0.0D0'
              CALL PROCES
              INPUT='EOS'
              CALL PROCES
!        CALL WDIALOGPUTDOUBLE(4THA,0.0D0)
!        CALL WDIALOGPUTDOUBLE(6THA,0.0D0)
!        CALL WDIALOGPUTDOUBLE(8THA,0.0D0)
!        CALL WDIALOGPUTDOUBLE(10THA,0.0D0)
!        CALL WDIALOGPUTDOUBLE(TCC,0.0D0)
              INCLUDE 'LOADSHEET.INC'
!        CALL WDIALOGSELECT(ASPH)
!        CALL WDialogShow(-1,-1,0,Modeless)
C
          END SELECT
C
      END IF
