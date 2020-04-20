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
C       SPECIAL SURFACE DATA
C
      IF(COMMAND.EQ.'SPSRF') THEN
          J=EDIT_SURFACE
!        CALL WDIALOGSELECT(SPSRF)
          SELECT CASE(MESSAGE)

            CASE ('IDOK')
!        CALL WDIALOGGETRADIOBUTTON(B1,ISET)
              CALL ITOAA(J,AJ)
              SELECT CASE (ISET)

                CASE (1)

                  INPUT='U SP'
                  CALL PROCES
                  IF(DABS(ALENS(34,J)).NE.1.0D0) THEN
                      INPUT='SPECIAL,'//AJ//',1'
                      CALL PROCES
                  END IF
                  CALL DOCOEFFS39(J)
                  INPUT='EOS'
                  CALL PROCES

                CASE (2)

                  INPUT='U SP'
                  CALL PROCES
                  IF(DABS(ALENS(34,J)).NE.2.0D0) THEN
                      INPUT='SPECIAL,'//AJ//',2'
                      CALL PROCES
                  END IF
                  CALL DOCOEFFS66(J)
                  INPUT='EOS'
                  CALL PROCES

                CASE (3)

                  INPUT='U SP'
                  CALL PROCES
                  IF(DABS(ALENS(34,J)).NE.3.0D0) THEN
                      INPUT='SPECIAL,'//AJ//',3'
                      CALL PROCES
                  END IF
                  CALL DOCOEFFS37(J)
                  INPUT='EOS'
                  CALL PROCES

                CASE (4)

                  INPUT='U SP'
                  CALL PROCES
                  IF(DABS(ALENS(34,J)).NE.4.0D0) THEN
                      INPUT='SPECIAL,'//AJ//',4'
                      CALL PROCES
                  END IF
                  CALL DOCOEFFS3(J)
                  INPUT='EOS'
                  CALL PROCES

                CASE (5)

                  INPUT='U SP'
                  CALL PROCES
                  IF(DABS(ALENS(34,J)).NE.5.0D0) THEN
                      INPUT='SPECIAL,'//AJ//',5'
                      CALL PROCES
                  END IF
                  CALL DOCOEFFS96(J)
                  INPUT='EOS'
                  CALL PROCES

                CASE (6)

                  INPUT='U SP'
                  CALL PROCES
                  IF(DABS(ALENS(34,J)).NE.6.0D0) THEN
                      INPUT='SPECIAL,'//AJ//',6'
                      CALL PROCES
                  END IF
                  CALL DOCOEFFS39(J)
                  INPUT='EOS'
                  CALL PROCES

                CASE (7)

                  INPUT='U SP'
                  CALL PROCES
                  IF(DABS(ALENS(34,J)).NE.7.0D0) THEN
                      INPUT='SPECIAL,'//AJ//',7'
                      CALL PROCES
                  END IF
                  CALL DOCOEFFS91(J)
                  INPUT='EOS'
                  CALL PROCES

                CASE (8)

                  INPUT='U SP'
                  CALL PROCES
                  IF(DABS(ALENS(34,J)).NE.8.0D0) THEN
                      INPUT='SPECIAL,'//AJ//',8'
                      CALL PROCES
                  END IF
                  CALL DOCOEFFS91(J)
                  INPUT='EOS'
                  CALL PROCES

                CASE (9)

                  INPUT='U SP'
                  CALL PROCES
                  IF(DABS(ALENS(34,J)).NE.9.0D0) THEN
                      INPUT='SPECIAL,'//AJ//',9'
                      CALL PROCES
                  END IF
                  CALL DOCOEFFS66(J)
                  INPUT='EOS'
                  CALL PROCES

                CASE (10)

                  INPUT='U SP'
                  CALL PROCES
                  IF(DABS(ALENS(34,J)).NE.10.0D0) THEN
                      INPUT='SPECIAL,'//AJ//',10'
                      CALL PROCES
                  END IF
                  CALL DOCOEFFS37(J)
                  INPUT='EOS'
                  CALL PROCES

                CASE (11)

                  INPUT='U SP'
                  CALL PROCES
                  IF(DABS(ALENS(34,J)).NE.11.0D0) THEN
                      INPUT='SPECIAL,'//AJ//',11'
                      CALL PROCES
                  END IF
                  CALL DOCOEFFS96(J)
                  INPUT='EOS'
                  CALL PROCES

                CASE (12)

                  INPUT='U SP'
                  CALL PROCES
                  IF(DABS(ALENS(34,J)).NE.12.0D0) THEN
                      INPUT='SPECIAL,'//AJ//',12'
                      CALL PROCES
                      INPUT='C11 0.0D0'
                      CALL PROCES
                  END IF
                  CALL DOCOEFFS11(J)
                  INPUT='EOS'
                  CALL PROCES

                CASE (13)

                  INPUT='U SP'
                  CALL PROCES
                  IF(DABS(ALENS(34,J)).NE.12.0D0) THEN
                      INPUT='SPECIAL,'//AJ//',12'
                      CALL PROCES
                      INPUT='C11 1.0D0'
                      CALL PROCES
                  END IF
                  CALL DOCOEFFS31(J)
                  INPUT='EOS'
                  CALL PROCES

                CASE (14)

                  INPUT='U SP'
                  CALL PROCES
                  IF(DABS(ALENS(34,J)).NE.12.0D0) THEN
                      INPUT='SPECIAL,'//AJ//',12'
                      CALL PROCES
                      INPUT='C11 2.0D0'
                      CALL PROCES
                  END IF
                  CALL DOCOEFFS89(J)
                  INPUT='EOS'
                  CALL PROCES

                CASE (15)

                  INPUT='U SP'
                  CALL PROCES
                  IF(DABS(ALENS(34,J)).NE.12.0D0) THEN
                      INPUT='SPECIAL,'//AJ//',12'
                      CALL PROCES
                      INPUT='C11 3.0D0'
                      CALL PROCES
                  END IF
                  CALL DOCOEFFS89(J)
                  INPUT='EOS'
                  CALL PROCES

                CASE (16)

                  INPUT='U SP'
                  CALL PROCES
                  IF(DABS(ALENS(34,J)).NE.12.0D0) THEN
                      INPUT='SPECIAL,'//AJ//',12'
                      CALL PROCES
                      INPUT='C11 4.0D0'
                      CALL PROCES
                  END IF
                  CALL DOCOEFFS89(J)
                  INPUT='EOS'
                  CALL PROCES

                CASE (17)

                  INPUT='U SP'
                  CALL PROCES
                  IF(DABS(ALENS(34,J)).NE.12.0D0) THEN
                      INPUT='SPECIAL,'//AJ//',12'
                      CALL PROCES
                      INPUT='C11 5.0D0'
                      CALL PROCES
                  END IF
                  CALL DOCOEFFS89(J)
                  INPUT='EOS'
                  CALL PROCES

                CASE (18)

                  INPUT='U SP'
                  CALL PROCES
                  IF(DABS(ALENS(34,J)).NE.12.0D0) THEN
                      INPUT='SPECIAL,'//AJ//',12'
                      CALL PROCES
                      INPUT='C11 6.0D0'
                      CALL PROCES
                  END IF
                  CALL DOCOEFFS31(J)
                  INPUT='EOS'
                  CALL PROCES

                CASE (19)

                  INPUT='U SP'
                  CALL PROCES
                  IF(DABS(ALENS(34,J)).NE.12.0D0) THEN
                      INPUT='SPECIAL,'//AJ//',12'
                      CALL PROCES
                      INPUT='C11 7.0D0'
                      CALL PROCES
                  END IF
                  CALL DOCOEFFS96(J)
                  INPUT='EOS'
                  CALL PROCES

                CASE (20)

                  INPUT='U SP'
                  CALL PROCES
                  IF(DABS(ALENS(34,J)).NE.13.0D0) THEN
                      INPUT='SPECIAL,'//AJ//',13'
                      CALL PROCES
                  END IF
                  CALL DOCOEFFS14(J)
                  INPUT='EOS'
                  CALL PROCES

                CASE (21)

                  INPUT='U SP'
                  CALL PROCES
                  IF(DABS(ALENS(34,J)).NE.14.0D0) THEN
                      INPUT='SPECIAL,'//AJ//',14'
                      CALL PROCES
                  END IF
                  CALL DOCOEFFS48(J)
                  INPUT='EOS'
                  CALL PROCES

                CASE (22)

                  INPUT='U SP'
                  CALL PROCES
                  IF(DABS(ALENS(34,J)).NE.15.0D0) THEN
                      INPUT='SPECIAL,'//AJ//',15'
                      CALL PROCES
                  END IF
                  CALL DOCOEFFS48(J)
                  INPUT='EOS'
                  CALL PROCES

                CASE (23)

                  INPUT='U SP'
                  CALL PROCES
                  IF(DABS(ALENS(34,J)).NE.16.0D0) THEN
                      INPUT='SPECIAL,'//AJ//',16'
                      CALL PROCES
                  END IF
                  CALL DOCOEFFS11(J)
                  INPUT='EOS'
                  CALL PROCES

                CASE (24)

                  INPUT='U SP'
                  CALL PROCES
                  IF(DABS(ALENS(34,J)).NE.17.0D0) THEN
                      INPUT='SPECIAL,'//AJ//',17'
                      CALL PROCES
                  END IF
                  CALL DOCOEFFS96(J)
                  INPUT='EOS'
                  CALL PROCES

                CASE (25)

                  INPUT='U SP'
                  CALL PROCES
                  IF(DABS(ALENS(34,J)).NE.18.0D0) THEN
                      INPUT='SPECIAL,'//AJ//',18'
                      CALL PROCES
                  END IF
                  CALL DOCOEFFS18(J)
                  INPUT='EOS'
                  CALL PROCES

                CASE (26)

                  INPUT='U SP'
                  CALL PROCES
                  IF(DABS(ALENS(34,J)).NE.19.0D0) THEN
                      INPUT='SPECIAL,'//AJ//',19'
                      CALL PROCES
                  END IF
                  CALL DOCOEFFS4(J)
                  INPUT='EOS'
                  CALL PROCES

                CASE (27)

                  INPUT='U SP'
                  CALL PROCES
                  IF(DABS(ALENS(34,J)).NE.20.0D0) THEN
                      INPUT='SPECIAL,'//AJ//',20'
                      CALL PROCES
                  END IF
                  CALL DOCOEFFS5(J)
                  INPUT='EOS'
                  CALL PROCES

                CASE (28)

                  INPUT='U SP'
                  CALL PROCES
                  IF(DABS(ALENS(34,J)).NE.21.0D0) THEN
                      INPUT='SPECIAL,'//AJ//',21'
                      CALL PROCES
                  END IF
                  CALL DOCOEFFS96(J)
                  INPUT='EOS'
                  CALL PROCES

                CASE (29)

                  INPUT='U SP'
                  CALL PROCES
                  IF(DABS(ALENS(34,J)).NE.22.0D0) THEN
                      INPUT='SPECIAL,'//AJ//',22'
                      CALL PROCES
                  END IF
                  CALL DOCOEFFS4(J)
                  INPUT='EOS'
                  CALL PROCES

                CASE (30)

                  INPUT='U SP'
                  CALL PROCES
                  IF(DABS(ALENS(34,J)).NE.23.0D0) THEN
                      INPUT='SPECIAL,'//AJ//',23'
                      CALL PROCES
                  END IF
                  CALL DOCOEFFS96(J)
                  INPUT='EOS'
                  CALL PROCES

              END SELECT
              INCLUDE 'LOADSHEET.INC'
              CALL SPSRFFRESH(J)
!        CALL WDIALOGSELECT(SPSRF)
!        CALL WDialogHide()
!        CALL WDialogUnload()
              INCLUDE 'LOADSHEET.INC'
!        CALL WDialogShow(-1,-1,0,Modeless)

            CASE ('SPSRFON')
              IF(ALENS(34,J).GE.0.0D0) THEN
              ELSE
                  CALL ITOAA(J,AJ)
                  INPUT='SPSRF ON,'//AJ
                  CALL PROCES
                  CALL SPSRFFRESH(J)
              END IF
C
            CASE ('SPSRFOFF')
              IF(ALENS(34,J).LE.0.0D0) THEN
              ELSE
                  CALL ITOAA(J,AJ)
                  INPUT='SPSRF OFF,'//AJ
                  CALL PROCES
                  CALL SPSRFFRESH(J)
              END IF

            CASE ('SPDEL')
              INPUT='U SP'
              CALL PROCES
              CALL ITOAA(J,AJ)
              INPUT='SPDEL,'//AJ
              CALL PROCES
              INPUT='EOS'
              CALL PROCES
!        CALL WDIALOGCLEARFIELD(B1)
!        CALL WDIALOGCLEARFIELD(B2)
!        CALL WDIALOGCLEARFIELD(B3)
!        CALL WDIALOGCLEARFIELD(B4)
!        CALL WDIALOGCLEARFIELD(B5)
!        CALL WDIALOGCLEARFIELD(B6)
!        CALL WDIALOGCLEARFIELD(B7)
!        CALL WDIALOGCLEARFIELD(B8)
!        CALL WDIALOGCLEARFIELD(B9)
!        CALL WDIALOGCLEARFIELD(B10)
!        CALL WDIALOGCLEARFIELD(B11)
!        CALL WDIALOGCLEARFIELD(B12a)
!        CALL WDIALOGCLEARFIELD(B12b)
!        CALL WDIALOGCLEARFIELD(B12c)
!        CALL WDIALOGCLEARFIELD(B12d)
!        CALL WDIALOGCLEARFIELD(B12e)
!        CALL WDIALOGCLEARFIELD(B12f)
!        CALL WDIALOGCLEARFIELD(B12g)
!        CALL WDIALOGCLEARFIELD(B12h)
!        CALL WDIALOGCLEARFIELD(B13)
!        CALL WDIALOGCLEARFIELD(B14)
!        CALL WDIALOGCLEARFIELD(B15)
!        CALL WDIALOGCLEARFIELD(B16)
!        CALL WDIALOGCLEARFIELD(B17)
!        CALL WDIALOGCLEARFIELD(B18)
!        CALL WDIALOGCLEARFIELD(B19)
!        CALL WDIALOGCLEARFIELD(B20)
!        CALL WDIALOGCLEARFIELD(B21)
!        CALL WDIALOGCLEARFIELD(B22)
!        CALL WDIALOGCLEARFIELD(B23)
              DO I=1,96
!        CALL WGRIDCLEARCELL(GRID1,1,I)
!        CALL WGRIDCLEARCELL(GRID1,2,I)
!        CALL WGRIDSTATE(GRID1,1,I)
!        CALL WGRIDSTATE(GRID1,2,I)
              END DO
              CALL SPSRFFRESH(J)

            CASE ('S1')
C       LOAD THE APPROPRIATE COEF DESCRIPTIONS AND VALUES
              IF(DABS(ALENS(34,J)).NE.1.0D0) THEN
                  CALL ITOAA(J,AJ)
                  INPUT='U SP'
                  CALL PROCES
                  INPUT='SPECIAL,'//AJ//',1'
                  CALL PROCES
                  INPUT='EOS'
                  CALL PROCES
              END IF
              IF(.NOT.GUIERROR) THEN
                  CALL LOADGRIDA
                  CALL LOADGRIDB1(J)
!        CALL WDIALOGPUTRADIOBUTTON(B1)
              ELSE
                  GUIERROR=.FALSE.
              END IF

            CASE ('S2')
C       LOAD THE APPROPRIATE COEF DESCRIPTIONS AND VALUES
              IF(DABS(ALENS(34,J)).NE.2.0D0) THEN
                  CALL ITOAA(J,AJ)
                  INPUT='U SP'
                  CALL PROCES
                  INPUT='SPECIAL,'//AJ//',2'
                  CALL PROCES
                  INPUT='EOS'
                  CALL PROCES
              END IF
              IF(.NOT.GUIERROR) THEN
                  CALL LOADGRIDA
                  CALL LOADGRIDB2(J)
!        CALL WDIALOGPUTRADIOBUTTON(B2)
              ELSE
                  GUIERROR=.FALSE.
              END IF

            CASE ('S3')
C       LOAD THE APPROPRIATE COEF DESCRIPTIONS AND VALUES
              IF(DABS(ALENS(34,J)).NE.3.0D0) THEN
                  CALL ITOAA(J,AJ)
                  INPUT='U SP'
                  CALL PROCES
                  INPUT='SPECIAL,'//AJ//',3'
                  CALL PROCES
                  INPUT='EOS'
                  CALL PROCES
              END IF
              IF(.NOT.GUIERROR) THEN
                  CALL LOADGRIDA
                  CALL LOADGRIDB3(J)
!        CALL WDIALOGPUTRADIOBUTTON(B3)
              ELSE
                  GUIERROR=.FALSE.
              END IF

            CASE ('S4')
C       LOAD THE APPROPRIATE COEF DESCRIPTIONS AND VALUES
              IF(DABS(ALENS(34,J)).NE.4.0D0) THEN
                  CALL ITOAA(J,AJ)
                  INPUT='U SP'
                  CALL PROCES
                  INPUT='SPECIAL,'//AJ//',4'
                  CALL PROCES
                  INPUT='EOS'
                  CALL PROCES
              END IF
              IF(.NOT.GUIERROR) THEN
                  CALL LOADGRIDA
                  CALL LOADGRIDB4(J)
!        CALL WDIALOGPUTRADIOBUTTON(B4)
              ELSE
                  GUIERROR=.FALSE.
              END IF

            CASE ('S5')
C       LOAD THE APPROPRIATE COEF DESCRIPTIONS AND VALUES
              IF(DABS(ALENS(34,J)).NE.5.0D0) THEN
                  CALL ITOAA(J,AJ)
                  INPUT='U SP'
                  CALL PROCES
                  INPUT='SPECIAL,'//AJ//',5'
                  CALL PROCES
                  INPUT='EOS'
                  CALL PROCES
              END IF
              IF(.NOT.GUIERROR) THEN
                  CALL LOADGRIDA
                  CALL LOADGRIDB5(J)
!        CALL WDIALOGPUTRADIOBUTTON(B5)
              ELSE
                  GUIERROR=.FALSE.
              END IF

            CASE ('S6')
C       LOAD THE APPROPRIATE COEF DESCRIPTIONS AND VALUES
              IF(DABS(ALENS(34,J)).NE.6.0D0) THEN
                  CALL ITOAA(J,AJ)
                  INPUT='U SP'
                  CALL PROCES
                  INPUT='SPECIAL,'//AJ//',6'
                  CALL PROCES
                  INPUT='EOS'
                  CALL PROCES
              END IF
              IF(.NOT.GUIERROR) THEN
                  CALL LOADGRIDA
                  CALL LOADGRIDB6(J)
!        CALL WDIALOGPUTRADIOBUTTON(B6)
              ELSE
                  GUIERROR=.FALSE.
              END IF

            CASE ('S7')
C       LOAD THE APPROPRIATE COEF DESCRIPTIONS AND VALUES
              IF(DABS(ALENS(34,J)).NE.7.0D0) THEN
                  CALL ITOAA(J,AJ)
                  INPUT='U SP'
                  CALL PROCES
                  INPUT='SPECIAL,'//AJ//',7'
                  CALL PROCES
                  INPUT='EOS'
                  CALL PROCES
              END IF
              IF(.NOT.GUIERROR) THEN
                  CALL LOADGRIDA
                  CALL LOADGRIDB7(J)
!        CALL WDIALOGPUTRADIOBUTTON(B7)
              ELSE
                  GUIERROR=.FALSE.
              END IF

            CASE ('S8')
C       LOAD THE APPROPRIATE COEF DESCRIPTIONS AND VALUES
              IF(DABS(ALENS(34,J)).NE.8.0D0) THEN
                  CALL ITOAA(J,AJ)
                  INPUT='U SP'
                  CALL PROCES
                  INPUT='SPECIAL,'//AJ//',8'
                  CALL PROCES
                  INPUT='EOS'
                  CALL PROCES
              END IF
              IF(.NOT.GUIERROR) THEN
                  CALL LOADGRIDA
                  CALL LOADGRIDB8(J)
!        CALL WDIALOGPUTRADIOBUTTON(B8)
              ELSE
                  GUIERROR=.FALSE.
              END IF

            CASE ('S9')
C       LOAD THE APPROPRIATE COEF DESCRIPTIONS AND VALUES
              IF(DABS(ALENS(34,J)).NE.9.0D0) THEN
                  CALL ITOAA(J,AJ)
                  INPUT='U SP'
                  CALL PROCES
                  INPUT='SPECIAL,'//AJ//',9'
                  CALL PROCES
                  INPUT='EOS'
                  CALL PROCES
              END IF
              IF(.NOT.GUIERROR) THEN
                  CALL LOADGRIDA
                  CALL LOADGRIDB9(J)
!        CALL WDIALOGPUTRADIOBUTTON(B9)
              ELSE
                  GUIERROR=.FALSE.
              END IF

            CASE ('S10')
C       LOAD THE APPROPRIATE COEF DESCRIPTIONS AND VALUES
              IF(DABS(ALENS(34,J)).NE.10.0D0) THEN
                  CALL ITOAA(J,AJ)
                  INPUT='U SP'
                  CALL PROCES
                  INPUT='SPECIAL,'//AJ//',10'
                  CALL PROCES
                  INPUT='EOS'
                  CALL PROCES
              END IF
              IF(.NOT.GUIERROR) THEN
                  CALL LOADGRIDA
                  CALL LOADGRIDB10(J)
!        CALL WDIALOGPUTRADIOBUTTON(B10)
              ELSE
                  GUIERROR=.FALSE.
              END IF

            CASE ('S11')
C       LOAD THE APPROPRIATE COEF DESCRIPTIONS AND VALUES
              IF(DABS(ALENS(34,J)).NE.11.0D0) THEN
                  CALL ITOAA(J,AJ)
                  INPUT='U SP'
                  CALL PROCES
                  INPUT='SPECIAL,'//AJ//',11'
                  CALL PROCES
                  INPUT='EOS'
                  CALL PROCES
              END IF
              IF(.NOT.GUIERROR) THEN
                  CALL LOADGRIDA
                  CALL LOADGRIDB11(J)
!        CALL WDIALOGPUTRADIOBUTTON(B11)
              ELSE
                  GUIERROR=.FALSE.
              END IF

            CASE ('S12a')
C       LOAD THE APPROPRIATE COEF DESCRIPTIONS AND VALUES
              IF(DABS(ALENS(34,J)).NE.12.0D0) THEN
                  CALL ITOAA(J,AJ)
                  INPUT='U SP'
                  CALL PROCES
                  INPUT='SPECIAL,'//AJ//',12'
                  CALL PROCES
                  INPUT='C11,'//AJ//',0'
                  CALL PROCES
                  INPUT='EOS'
                  CALL PROCES
              END IF
              IF(.NOT.GUIERROR) THEN
                  CALL LOADGRIDA
                  CALL LOADGRIDB12a(J)
!        CALL WDIALOGPUTRADIOBUTTON(B12a)
              ELSE
                  GUIERROR=.FALSE.
              END IF

            CASE ('S12b')
C       LOAD THE APPROPRIATE COEF DESCRIPTIONS AND VALUES
              IF(DABS(ALENS(34,J)).NE.12.0D0) THEN
                  CALL ITOAA(J,AJ)
                  INPUT='U SP'
                  CALL PROCES
                  INPUT='SPECIAL,'//AJ//',12'
                  CALL PROCES
                  INPUT='C11,'//AJ//',1'
                  CALL PROCES
                  INPUT='EOS'
                  CALL PROCES
              END IF
              IF(.NOT.GUIERROR) THEN
                  CALL LOADGRIDA
                  CALL LOADGRIDB12b(J)
!        CALL WDIALOGPUTRADIOBUTTON(B12b)
              ELSE
                  GUIERROR=.FALSE.
              END IF

            CASE ('S12c')
C       LOAD THE APPROPRIATE COEF DESCRIPTIONS AND VALUES
              IF(DABS(ALENS(34,J)).NE.12.0D0) THEN
                  CALL ITOAA(J,AJ)
                  INPUT='U SP'
                  CALL PROCES
                  INPUT='SPECIAL,'//AJ//',12'
                  CALL PROCES
                  INPUT='C11,'//AJ//',2'
                  CALL PROCES
                  INPUT='EOS'
                  CALL PROCES
              END IF
              IF(.NOT.GUIERROR) THEN
                  CALL LOADGRIDA
                  CALL LOADGRIDB12c(J)
!        CALL WDIALOGPUTRADIOBUTTON(B12c)
              ELSE
                  GUIERROR=.FALSE.
              END IF

            CASE ('S12d')
C       LOAD THE APPROPRIATE COEF DESCRIPTIONS AND VALUES
              IF(DABS(ALENS(34,J)).NE.12.0D0) THEN
                  CALL ITOAA(J,AJ)
                  INPUT='U SP'
                  CALL PROCES
                  INPUT='SPECIAL,'//AJ//',12'
                  CALL PROCES
                  INPUT='C11,'//AJ//',3'
                  CALL PROCES
                  INPUT='EOS'
                  CALL PROCES
              END IF
              IF(.NOT.GUIERROR) THEN
                  CALL LOADGRIDA
                  CALL LOADGRIDB12d(J)
!        CALL WDIALOGPUTRADIOBUTTON(B12d)
              ELSE
                  GUIERROR=.FALSE.
              END IF

            CASE ('S12e')
C       LOAD THE APPROPRIATE COEF DESCRIPTIONS AND VALUES
              IF(DABS(ALENS(34,J)).NE.12.0D0) THEN
                  CALL ITOAA(J,AJ)
                  INPUT='U SP'
                  CALL PROCES
                  INPUT='SPECIAL,'//AJ//',12'
                  CALL PROCES
                  INPUT='C11,'//AJ//',4'
                  CALL PROCES
                  INPUT='EOS'
                  CALL PROCES
              END IF
              IF(.NOT.GUIERROR) THEN
                  CALL LOADGRIDA
                  CALL LOADGRIDB12e(J)
!        CALL WDIALOGPUTRADIOBUTTON(B12e)
              ELSE
                  GUIERROR=.FALSE.
              END IF

            CASE ('S12f')
C       LOAD THE APPROPRIATE COEF DESCRIPTIONS AND VALUES
              IF(DABS(ALENS(34,J)).NE.12.0D0) THEN
                  CALL ITOAA(J,AJ)
                  INPUT='U SP'
                  CALL PROCES
                  INPUT='SPECIAL,'//AJ//',12'
                  CALL PROCES
                  INPUT='C11,'//AJ//',5'
                  CALL PROCES
                  INPUT='EOS'
                  CALL PROCES
              END IF
              IF(.NOT.GUIERROR) THEN
                  CALL LOADGRIDA
                  CALL LOADGRIDB12f(J)
!        CALL WDIALOGPUTRADIOBUTTON(B12f)
              ELSE
                  GUIERROR=.FALSE.
              END IF

            CASE ('S12g')
C       LOAD THE APPROPRIATE COEF DESCRIPTIONS AND VALUES
              IF(DABS(ALENS(34,J)).NE.12.0D0) THEN
                  CALL ITOAA(J,AJ)
                  INPUT='U SP'
                  CALL PROCES
                  INPUT='SPECIAL,'//AJ//',12'
                  CALL PROCES
                  INPUT='C11,'//AJ//',6'
                  CALL PROCES
                  INPUT='EOS'
                  CALL PROCES
              END IF
              IF(.NOT.GUIERROR) THEN
                  CALL LOADGRIDA
                  CALL LOADGRIDB12g(J)
!        CALL WDIALOGPUTRADIOBUTTON(B12g)
              ELSE
                  GUIERROR=.FALSE.
              END IF

            CASE ('S12h')
C       LOAD THE APPROPRIATE COEF DESCRIPTIONS AND VALUES
              IF(DABS(ALENS(34,J)).NE.12.0D0) THEN
                  CALL ITOAA(J,AJ)
                  INPUT='U SP'
                  CALL PROCES
                  INPUT='SPECIAL,'//AJ//',12'
                  CALL PROCES
                  INPUT='C11,'//AJ//',7'
                  CALL PROCES
                  INPUT='EOS'
                  CALL PROCES
              END IF
              IF(.NOT.GUIERROR) THEN
                  CALL LOADGRIDA
                  CALL LOADGRIDB12h(J)
!        CALL WDIALOGPUTRADIOBUTTON(B12h)
              ELSE
                  GUIERROR=.FALSE.
              END IF


            CASE ('S13')
C       LOAD THE APPROPRIATE COEF DESCRIPTIONS AND VALUES
              IF(DABS(ALENS(34,J)).NE.13.0D0) THEN
                  CALL ITOAA(J,AJ)
                  INPUT='U SP'
                  CALL PROCES
                  INPUT='SPECIAL,'//AJ//',13'
                  CALL PROCES
                  INPUT='EOS'
                  CALL PROCES
              END IF
              IF(.NOT.GUIERROR) THEN
                  CALL LOADGRIDA
                  CALL LOADGRIDB13(J)
!        CALL WDIALOGPUTRADIOBUTTON(B13)
              ELSE
                  GUIERROR=.FALSE.
              END IF

            CASE ('S14')
C       LOAD THE APPROPRIATE COEF DESCRIPTIONS AND VALUES
              IF(DABS(ALENS(34,J)).NE.14.0D0) THEN
                  CALL ITOAA(J,AJ)
                  INPUT='U SP'
                  CALL PROCES
                  INPUT='SPECIAL,'//AJ//',14'
                  CALL PROCES
                  INPUT='EOS'
                  CALL PROCES
              END IF
              IF(.NOT.GUIERROR) THEN
                  CALL LOADGRIDA
                  CALL LOADGRIDB14(J)
!        CALL WDIALOGPUTRADIOBUTTON(B14)
              ELSE
                  GUIERROR=.FALSE.
              END IF

            CASE ('S15')
C       LOAD THE APPROPRIATE COEF DESCRIPTIONS AND VALUES
              IF(DABS(ALENS(34,J)).NE.15.0D0) THEN
                  CALL ITOAA(J,AJ)
                  INPUT='U SP'
                  CALL PROCES
                  INPUT='SPECIAL,'//AJ//',15'
                  CALL PROCES
                  INPUT='EOS'
                  CALL PROCES
              END IF
              IF(.NOT.GUIERROR) THEN
                  CALL LOADGRIDA
                  CALL LOADGRIDB15(J)
!        CALL WDIALOGPUTRADIOBUTTON(B15)
              ELSE
                  GUIERROR=.FALSE.
              END IF

            CASE ('S16')
C       LOAD THE APPROPRIATE COEF DESCRIPTIONS AND VALUES
              IF(DABS(ALENS(34,J)).NE.16.0D0) THEN
                  CALL ITOAA(J,AJ)
                  INPUT='U SP'
                  CALL PROCES
                  INPUT='SPECIAL,'//AJ//',16'
                  CALL PROCES
                  INPUT='EOS'
                  CALL PROCES
              END IF
              IF(.NOT.GUIERROR) THEN
                  CALL LOADGRIDA
                  CALL LOADGRIDB16(J)
!        CALL WDIALOGPUTRADIOBUTTON(B16)
              ELSE
                  GUIERROR=.FALSE.
              END IF

            CASE ('S17')
C       LOAD THE APPROPRIATE COEF DESCRIPTIONS AND VALUES
              IF(DABS(ALENS(34,J)).NE.17.0D0) THEN
                  CALL ITOAA(J,AJ)
                  INPUT='U SP'
                  CALL PROCES
                  INPUT='SPECIAL,'//AJ//',17'
                  CALL PROCES
                  INPUT='EOS'
                  CALL PROCES
              END IF
              IF(.NOT.GUIERROR) THEN
                  CALL LOADGRIDA
                  CALL LOADGRIDB17(J)
!        CALL WDIALOGPUTRADIOBUTTON(B17)
              ELSE
                  GUIERROR=.FALSE.
              END IF

            CASE ('S18')
C       LOAD THE APPROPRIATE COEF DESCRIPTIONS AND VALUES
              IF(DABS(ALENS(34,J)).NE.18.0D0) THEN
                  CALL ITOAA(J,AJ)
                  INPUT='U SP'
                  CALL PROCES
                  INPUT='SPECIAL,'//AJ//',18'
                  CALL PROCES
                  INPUT='EOS'
                  CALL PROCES
              END IF
              IF(.NOT.GUIERROR) THEN
                  CALL LOADGRIDA
                  CALL LOADGRIDB18(J)
!        CALL WDIALOGPUTRADIOBUTTON(B18)
              ELSE
                  GUIERROR=.FALSE.
              END IF

            CASE ('S19')
C       LOAD THE APPROPRIATE COEF DESCRIPTIONS AND VALUES
              IF(DABS(ALENS(34,J)).NE.19.0D0) THEN
                  CALL ITOAA(J,AJ)
                  INPUT='U SP'
                  CALL PROCES
                  INPUT='SPECIAL,'//AJ//',19'
                  CALL PROCES
                  INPUT='EOS'
                  CALL PROCES
              END IF
              IF(.NOT.GUIERROR) THEN
                  CALL LOADGRIDA
                  CALL LOADGRIDB19(J)
!        CALL WDIALOGPUTRADIOBUTTON(B19)
              ELSE
                  GUIERROR=.FALSE.
              END IF

            CASE ('S20')

C       LOAD THE APPROPRIATE COEF DESCRIPTIONS AND VALUES
              IF(DABS(ALENS(34,J)).NE.20.0D0) THEN
                  CALL ITOAA(J,AJ)
                  INPUT='U SP'
                  CALL PROCES
                  INPUT='SPECIAL,'//AJ//',20'
                  CALL PROCES
                  INPUT='EOS'
                  CALL PROCES
              END IF
              IF(.NOT.GUIERROR) THEN
                  CALL LOADGRIDA
                  CALL LOADGRIDB20(J)
!        CALL WDIALOGPUTRADIOBUTTON(B20)
              ELSE
                  GUIERROR=.FALSE.
              END IF

            CASE ('S21')
C       LOAD THE APPROPRIATE COEF DESCRIPTIONS AND VALUES
              IF(DABS(ALENS(34,J)).NE.21.0D0) THEN
                  CALL ITOAA(J,AJ)
                  INPUT='U SP'
                  CALL PROCES
                  INPUT='SPECIAL,'//AJ//',21'
                  CALL PROCES
                  INPUT='EOS'
                  CALL PROCES
              END IF
              IF(.NOT.GUIERROR) THEN
                  CALL LOADGRIDA
                  CALL LOADGRIDB21(J)
!        CALL WDIALOGPUTRADIOBUTTON(B21)
              ELSE
                  GUIERROR=.FALSE.
              END IF

            CASE ('S22')
C       LOAD THE APPROPRIATE COEF DESCRIPTIONS AND VALUES
              IF(DABS(ALENS(34,J)).NE.22.0D0) THEN
                  CALL ITOAA(J,AJ)
                  INPUT='U SP'
                  CALL PROCES
                  INPUT='SPECIAL,'//AJ//',22'
                  CALL PROCES
                  INPUT='EOS'
                  CALL PROCES
              END IF
              IF(.NOT.GUIERROR) THEN
                  CALL LOADGRIDA
                  CALL LOADGRIDB22(J)
!        CALL WDIALOGPUTRADIOBUTTON(B22)
              ELSE
                  GUIERROR=.FALSE.
              END IF

            CASE ('S23')
C       LOAD THE APPROPRIATE COEF DESCRIPTIONS AND VALUES
              IF(DABS(ALENS(34,J)).NE.23.0D0) THEN
                  CALL ITOAA(J,AJ)
                  INPUT='U SP'
                  CALL PROCES
                  INPUT='SPECIAL,'//AJ//',23'
                  CALL PROCES
                  INPUT='EOS'
                  CALL PROCES
              END IF
              IF(.NOT.GUIERROR) THEN
                  CALL LOADGRIDA
                  CALL LOADGRIDB23(J)
!        CALL WDIALOGPUTRADIOBUTTON(B23)
              ELSE
                  GUIERROR=.FALSE.
              END IF
C
          END SELECT
          CALL SPSRFFRESH(J)
      END IF
C
