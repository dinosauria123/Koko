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

C       SECOND GROUP OF SPECIAL SURFACE AND FITTING FILES

C SUB SPCOEF.FOR
      SUBROUTINE SPCOEF()
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE SPCOEF. THIS IS THE SUBROUTINE WHICH
C       HANDLES THE "C1" THHROUGH "C96"
C       COMMANDS FROM SPSRF OR UPDATE SPSRF LEVEL
C
          LOGICAL CNOT
C
          INTEGER PIKCNT,I
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
C
          IF(SST.EQ.1.OR.SQ.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
              OUTLYNE='" '//WC//
     1        ' " ONLY TAKES NUMERIC WORD #1 AND #2 INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER DATA'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(DF1.EQ.1.OR.DF2.EQ.1) THEN
              OUTLYNE=
     1        '" '//WC//
     1        ' " REQUIRES EXPLICIT NUMERIC WORD #1 AND #2 INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C       PROCEED WITH C1 TO C96
C
          IF(W1.EQ.0.0D0) THEN
              OUTLYNE='SPECIAL SURFACE TYPE NOT VALID ON OBJECT SURFACE'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(W1.LT.0.0D0) W1=SYSTEM1(20)+W1
          IF(INT(W1).LT.1.OR.INT(W1).GT.INT(SYSTEM1(20))) THEN
              OUTLYNE=
     1        'SURFACE NUMBER BEYOND LEGAL RANGE'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(ALENS(34,INT(W1)).EQ.0.0D0) THEN
              WRITE(OUTLYNE,*)
     1          'WARNING: SURFACE ',INT(W1),' NOT A SPECIAL SURFACE TYPE'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(ALENS(34,INT(W1)).NE.0.0D0) THEN
C
C       PERFORM REQUIRED ACTION
C
C     CHECK FOR THINGS NOT ALLOWED
              CNOT=.FALSE.
C
              IF(DABS(ALENS(34,INT(W1))).EQ.1.0D0.OR.
     1        DABS(ALENS(34,INT(W1))).EQ.6.0D0) THEN
                  IF(WC.EQ.'C1') CNOT=.TRUE.
                  IF(WC.EQ.'C2') CNOT=.TRUE.
                  IF(WC.EQ.'C3') CNOT=.TRUE.
                  IF(WC.EQ.'C4') CNOT=.TRUE.
                  IF(WC.EQ.'C5') CNOT=.TRUE.
                  IF(WC.EQ.'C6') CNOT=.TRUE.
                  IF(WC.EQ.'C7') CNOT=.TRUE.
                  IF(WC.EQ.'C8') CNOT=.TRUE.
                  IF(WC.EQ.'C49') CNOT=.TRUE.
                  IF(WC.EQ.'C50') CNOT=.TRUE.
                  IF(WC.EQ.'C51') CNOT=.TRUE.
                  IF(WC.EQ.'C52') CNOT=.TRUE.
                  IF(WC.EQ.'C53') CNOT=.TRUE.
                  IF(WC.EQ.'C54') CNOT=.TRUE.
                  IF(WC.EQ.'C55') CNOT=.TRUE.
                  IF(WC.EQ.'C56') CNOT=.TRUE.
                  IF(WC.EQ.'C57') CNOT=.TRUE.
                  IF(WC.EQ.'C58') CNOT=.TRUE.
                  IF(WC.EQ.'C59') CNOT=.TRUE.
                  IF(WC.EQ.'C60') CNOT=.TRUE.
                  IF(WC.EQ.'C61') CNOT=.TRUE.
                  IF(WC.EQ.'C62') CNOT=.TRUE.
                  IF(WC.EQ.'C63') CNOT=.TRUE.
                  IF(WC.EQ.'C64') CNOT=.TRUE.
                  IF(WC.EQ.'C65') CNOT=.TRUE.
                  IF(WC.EQ.'C66') CNOT=.TRUE.
                  IF(WC.EQ.'C67') CNOT=.TRUE.
                  IF(WC.EQ.'C68') CNOT=.TRUE.
                  IF(WC.EQ.'C69') CNOT=.TRUE.
                  IF(WC.EQ.'C70') CNOT=.TRUE.
                  IF(WC.EQ.'C71') CNOT=.TRUE.
                  IF(WC.EQ.'C72') CNOT=.TRUE.
                  IF(WC.EQ.'C73') CNOT=.TRUE.
                  IF(WC.EQ.'C74') CNOT=.TRUE.
                  IF(WC.EQ.'C75') CNOT=.TRUE.
                  IF(WC.EQ.'C76') CNOT=.TRUE.
                  IF(WC.EQ.'C77') CNOT=.TRUE.
                  IF(WC.EQ.'C78') CNOT=.TRUE.
                  IF(WC.EQ.'C79') CNOT=.TRUE.
                  IF(WC.EQ.'C80') CNOT=.TRUE.
                  IF(WC.EQ.'C81') CNOT=.TRUE.
                  IF(WC.EQ.'C82') CNOT=.TRUE.
                  IF(WC.EQ.'C83') CNOT=.TRUE.
                  IF(WC.EQ.'C84') CNOT=.TRUE.
                  IF(WC.EQ.'C85') CNOT=.TRUE.
                  IF(WC.EQ.'C86') CNOT=.TRUE.
                  IF(WC.EQ.'C87') CNOT=.TRUE.
                  IF(WC.EQ.'C88') CNOT=.TRUE.
                  IF(WC.EQ.'C89') CNOT=.TRUE.
                  IF(WC.EQ.'C90') CNOT=.TRUE.
                  IF(WC.EQ.'C91') CNOT=.TRUE.
                  IF(WC.EQ.'C92') CNOT=.TRUE.
                  IF(WC.EQ.'C93') CNOT=.TRUE.
                  IF(WC.EQ.'C94') CNOT=.TRUE.
                  IF(WC.EQ.'C95') CNOT=.TRUE.
                  IF(WC.EQ.'C96') CNOT=.TRUE.
                  IF(CNOT) THEN
                      OUTLYNE=
     1                'SPECIAL SURFACE TYPES 1 AND 6'
                      CALL SHOWIT(1)
                      OUTLYNE=
     1                'ONLY USE COEFFICIENTS C9 THROUGH C48'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
              IF(DABS(ALENS(34,INT(W1))).EQ.24.0D0) THEN
                  IF(WC.EQ.'C5') CNOT=.TRUE.
                  IF(WC.EQ.'C6') CNOT=.TRUE.
                  IF(WC.EQ.'C7') CNOT=.TRUE.
                  IF(WC.EQ.'C8') CNOT=.TRUE.
                  IF(WC.EQ.'C9') CNOT=.TRUE.
                  IF(WC.EQ.'C10') CNOT=.TRUE.
                  IF(WC.EQ.'C11') CNOT=.TRUE.
                  IF(WC.EQ.'C12') CNOT=.TRUE.
                  IF(WC.EQ.'C13') CNOT=.TRUE.
                  IF(WC.EQ.'C14') CNOT=.TRUE.
                  IF(WC.EQ.'C15') CNOT=.TRUE.
                  IF(WC.EQ.'C16') CNOT=.TRUE.
                  IF(WC.EQ.'C17') CNOT=.TRUE.
                  IF(WC.EQ.'C18') CNOT=.TRUE.
                  IF(WC.EQ.'C19') CNOT=.TRUE.
                  IF(WC.EQ.'C20') CNOT=.TRUE.
                  IF(WC.EQ.'C21') CNOT=.TRUE.
                  IF(WC.EQ.'C22') CNOT=.TRUE.
                  IF(WC.EQ.'C23') CNOT=.TRUE.
                  IF(WC.EQ.'C24') CNOT=.TRUE.
                  IF(WC.EQ.'C25') CNOT=.TRUE.
                  IF(WC.EQ.'C26') CNOT=.TRUE.
                  IF(WC.EQ.'C27') CNOT=.TRUE.
                  IF(WC.EQ.'C28') CNOT=.TRUE.
                  IF(WC.EQ.'C29') CNOT=.TRUE.
                  IF(WC.EQ.'C30') CNOT=.TRUE.
                  IF(WC.EQ.'C31') CNOT=.TRUE.
                  IF(WC.EQ.'C32') CNOT=.TRUE.
                  IF(WC.EQ.'C33') CNOT=.TRUE.
                  IF(WC.EQ.'C34') CNOT=.TRUE.
                  IF(WC.EQ.'C35') CNOT=.TRUE.
                  IF(WC.EQ.'C36') CNOT=.TRUE.
                  IF(WC.EQ.'C37') CNOT=.TRUE.
                  IF(WC.EQ.'C38') CNOT=.TRUE.
                  IF(WC.EQ.'C39') CNOT=.TRUE.
                  IF(WC.EQ.'C40') CNOT=.TRUE.
                  IF(WC.EQ.'C41') CNOT=.TRUE.
                  IF(WC.EQ.'C42') CNOT=.TRUE.
                  IF(WC.EQ.'C43') CNOT=.TRUE.
                  IF(WC.EQ.'C44') CNOT=.TRUE.
                  IF(WC.EQ.'C45') CNOT=.TRUE.
                  IF(WC.EQ.'C46') CNOT=.TRUE.
                  IF(WC.EQ.'C47') CNOT=.TRUE.
                  IF(WC.EQ.'C48') CNOT=.TRUE.
                  IF(WC.EQ.'C49') CNOT=.TRUE.
                  IF(WC.EQ.'C50') CNOT=.TRUE.
                  IF(WC.EQ.'C51') CNOT=.TRUE.
                  IF(WC.EQ.'C52') CNOT=.TRUE.
                  IF(WC.EQ.'C53') CNOT=.TRUE.
                  IF(WC.EQ.'C54') CNOT=.TRUE.
                  IF(WC.EQ.'C55') CNOT=.TRUE.
                  IF(WC.EQ.'C56') CNOT=.TRUE.
                  IF(WC.EQ.'C57') CNOT=.TRUE.
                  IF(WC.EQ.'C58') CNOT=.TRUE.
                  IF(WC.EQ.'C59') CNOT=.TRUE.
                  IF(WC.EQ.'C60') CNOT=.TRUE.
                  IF(WC.EQ.'C61') CNOT=.TRUE.
                  IF(WC.EQ.'C62') CNOT=.TRUE.
                  IF(WC.EQ.'C63') CNOT=.TRUE.
                  IF(WC.EQ.'C64') CNOT=.TRUE.
                  IF(WC.EQ.'C65') CNOT=.TRUE.
                  IF(WC.EQ.'C66') CNOT=.TRUE.
                  IF(WC.EQ.'C67') CNOT=.TRUE.
                  IF(WC.EQ.'C68') CNOT=.TRUE.
                  IF(WC.EQ.'C69') CNOT=.TRUE.
                  IF(WC.EQ.'C70') CNOT=.TRUE.
                  IF(WC.EQ.'C71') CNOT=.TRUE.
                  IF(WC.EQ.'C72') CNOT=.TRUE.
                  IF(WC.EQ.'C73') CNOT=.TRUE.
                  IF(WC.EQ.'C74') CNOT=.TRUE.
                  IF(WC.EQ.'C75') CNOT=.TRUE.
                  IF(WC.EQ.'C76') CNOT=.TRUE.
                  IF(WC.EQ.'C77') CNOT=.TRUE.
                  IF(WC.EQ.'C78') CNOT=.TRUE.
                  IF(WC.EQ.'C79') CNOT=.TRUE.
                  IF(WC.EQ.'C80') CNOT=.TRUE.
                  IF(WC.EQ.'C81') CNOT=.TRUE.
                  IF(WC.EQ.'C82') CNOT=.TRUE.
                  IF(WC.EQ.'C83') CNOT=.TRUE.
                  IF(WC.EQ.'C84') CNOT=.TRUE.
                  IF(WC.EQ.'C85') CNOT=.TRUE.
                  IF(WC.EQ.'C86') CNOT=.TRUE.
                  IF(WC.EQ.'C87') CNOT=.TRUE.
                  IF(WC.EQ.'C88') CNOT=.TRUE.
                  IF(WC.EQ.'C89') CNOT=.TRUE.
                  IF(WC.EQ.'C90') CNOT=.TRUE.
                  IF(WC.EQ.'C91') CNOT=.TRUE.
                  IF(WC.EQ.'C92') CNOT=.TRUE.
                  IF(WC.EQ.'C93') CNOT=.TRUE.
                  IF(WC.EQ.'C94') CNOT=.TRUE.
                  IF(WC.EQ.'C95') CNOT=.TRUE.
                  IF(WC.EQ.'C96') CNOT=.TRUE.
                  IF(CNOT) THEN
                      OUTLYNE=
     1                'SPECIAL SURFACE TYPE 24'
                      CALL SHOWIT(1)
                      OUTLYNE=
     1                'ONLY USES COEFFICIENTS C1 THROUGH C4'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
              IF(DABS(ALENS(34,INT(W1))).EQ.2.0D0.OR.
     1        DABS(ALENS(34,INT(W1))).EQ.9.0D0) THEN
                  IF(WC.EQ.'C67') CNOT=.TRUE.
                  IF(WC.EQ.'C68') CNOT=.TRUE.
                  IF(WC.EQ.'C69') CNOT=.TRUE.
                  IF(WC.EQ.'C70') CNOT=.TRUE.
                  IF(WC.EQ.'C71') CNOT=.TRUE.
                  IF(WC.EQ.'C72') CNOT=.TRUE.
                  IF(WC.EQ.'C73') CNOT=.TRUE.
                  IF(WC.EQ.'C74') CNOT=.TRUE.
                  IF(WC.EQ.'C75') CNOT=.TRUE.
                  IF(WC.EQ.'C76') CNOT=.TRUE.
                  IF(WC.EQ.'C77') CNOT=.TRUE.
                  IF(WC.EQ.'C78') CNOT=.TRUE.
                  IF(WC.EQ.'C79') CNOT=.TRUE.
                  IF(WC.EQ.'C80') CNOT=.TRUE.
                  IF(WC.EQ.'C81') CNOT=.TRUE.
                  IF(WC.EQ.'C82') CNOT=.TRUE.
                  IF(WC.EQ.'C83') CNOT=.TRUE.
                  IF(WC.EQ.'C84') CNOT=.TRUE.
                  IF(WC.EQ.'C85') CNOT=.TRUE.
                  IF(WC.EQ.'C86') CNOT=.TRUE.
                  IF(WC.EQ.'C87') CNOT=.TRUE.
                  IF(WC.EQ.'C88') CNOT=.TRUE.
                  IF(WC.EQ.'C89') CNOT=.TRUE.
                  IF(WC.EQ.'C90') CNOT=.TRUE.
                  IF(WC.EQ.'C91') CNOT=.TRUE.
                  IF(WC.EQ.'C92') CNOT=.TRUE.
                  IF(WC.EQ.'C93') CNOT=.TRUE.
                  IF(WC.EQ.'C94') CNOT=.TRUE.
                  IF(WC.EQ.'C95') CNOT=.TRUE.
                  IF(WC.EQ.'C96') CNOT=.TRUE.
                  IF(CNOT) THEN
                      OUTLYNE=
     1                'SPECIAL SURFACE TYPES 2 AND 9'
                      CALL SHOWIT(1)
                      OUTLYNE=
     1                'ONLY USE COEFFICIENTS C1 THROUGH C66'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
              IF(DABS(ALENS(34,INT(W1))).EQ.14.0D0.OR.
     1        DABS(ALENS(34,INT(W1))).EQ.15.0D0) THEN
                  IF(WC.EQ.'C49') CNOT=.TRUE.
                  IF(WC.EQ.'C50') CNOT=.TRUE.
                  IF(WC.EQ.'C51') CNOT=.TRUE.
                  IF(WC.EQ.'C52') CNOT=.TRUE.
                  IF(WC.EQ.'C53') CNOT=.TRUE.
                  IF(WC.EQ.'C54') CNOT=.TRUE.
                  IF(WC.EQ.'C55') CNOT=.TRUE.
                  IF(WC.EQ.'C56') CNOT=.TRUE.
                  IF(WC.EQ.'C57') CNOT=.TRUE.
                  IF(WC.EQ.'C58') CNOT=.TRUE.
                  IF(WC.EQ.'C59') CNOT=.TRUE.
                  IF(WC.EQ.'C60') CNOT=.TRUE.
                  IF(WC.EQ.'C61') CNOT=.TRUE.
                  IF(WC.EQ.'C62') CNOT=.TRUE.
                  IF(WC.EQ.'C63') CNOT=.TRUE.
                  IF(WC.EQ.'C64') CNOT=.TRUE.
                  IF(WC.EQ.'C65') CNOT=.TRUE.
                  IF(WC.EQ.'C66') CNOT=.TRUE.
                  IF(WC.EQ.'C67') CNOT=.TRUE.
                  IF(WC.EQ.'C68') CNOT=.TRUE.
                  IF(WC.EQ.'C69') CNOT=.TRUE.
                  IF(WC.EQ.'C70') CNOT=.TRUE.
                  IF(WC.EQ.'C71') CNOT=.TRUE.
                  IF(WC.EQ.'C72') CNOT=.TRUE.
                  IF(WC.EQ.'C73') CNOT=.TRUE.
                  IF(WC.EQ.'C74') CNOT=.TRUE.
                  IF(WC.EQ.'C75') CNOT=.TRUE.
                  IF(WC.EQ.'C76') CNOT=.TRUE.
                  IF(WC.EQ.'C77') CNOT=.TRUE.
                  IF(WC.EQ.'C78') CNOT=.TRUE.
                  IF(WC.EQ.'C79') CNOT=.TRUE.
                  IF(WC.EQ.'C80') CNOT=.TRUE.
                  IF(WC.EQ.'C81') CNOT=.TRUE.
                  IF(WC.EQ.'C82') CNOT=.TRUE.
                  IF(WC.EQ.'C83') CNOT=.TRUE.
                  IF(WC.EQ.'C84') CNOT=.TRUE.
                  IF(WC.EQ.'C85') CNOT=.TRUE.
                  IF(WC.EQ.'C86') CNOT=.TRUE.
                  IF(WC.EQ.'C87') CNOT=.TRUE.
                  IF(WC.EQ.'C88') CNOT=.TRUE.
                  IF(WC.EQ.'C89') CNOT=.TRUE.
                  IF(WC.EQ.'C90') CNOT=.TRUE.
                  IF(WC.EQ.'C91') CNOT=.TRUE.
                  IF(WC.EQ.'C92') CNOT=.TRUE.
                  IF(WC.EQ.'C93') CNOT=.TRUE.
                  IF(WC.EQ.'C94') CNOT=.TRUE.
                  IF(WC.EQ.'C95') CNOT=.TRUE.
                  IF(WC.EQ.'C96') CNOT=.TRUE.
                  IF(CNOT) THEN
                      OUTLYNE=
     1                'SPECIAL SURFACE TYPES 14 AND 15'
                      CALL SHOWIT(1)
                      OUTLYNE=
     1                'ONLY USE COEFFICIENTS C1 THROUGH C48'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
              IF(DABS(ALENS(34,INT(W1))).EQ.16.0D0) THEN
                  IF(WC.EQ.'C12') CNOT=.TRUE.
                  IF(WC.EQ.'C13') CNOT=.TRUE.
                  IF(WC.EQ.'C14') CNOT=.TRUE.
                  IF(WC.EQ.'C15') CNOT=.TRUE.
                  IF(WC.EQ.'C16') CNOT=.TRUE.
                  IF(WC.EQ.'C17') CNOT=.TRUE.
                  IF(WC.EQ.'C18') CNOT=.TRUE.
                  IF(WC.EQ.'C19') CNOT=.TRUE.
                  IF(WC.EQ.'C20') CNOT=.TRUE.
                  IF(WC.EQ.'C21') CNOT=.TRUE.
                  IF(WC.EQ.'C22') CNOT=.TRUE.
                  IF(WC.EQ.'C23') CNOT=.TRUE.
                  IF(WC.EQ.'C24') CNOT=.TRUE.
                  IF(WC.EQ.'C25') CNOT=.TRUE.
                  IF(WC.EQ.'C26') CNOT=.TRUE.
                  IF(WC.EQ.'C27') CNOT=.TRUE.
                  IF(WC.EQ.'C28') CNOT=.TRUE.
                  IF(WC.EQ.'C29') CNOT=.TRUE.
                  IF(WC.EQ.'C30') CNOT=.TRUE.
                  IF(WC.EQ.'C31') CNOT=.TRUE.
                  IF(WC.EQ.'C32') CNOT=.TRUE.
                  IF(WC.EQ.'C33') CNOT=.TRUE.
                  IF(WC.EQ.'C34') CNOT=.TRUE.
                  IF(WC.EQ.'C35') CNOT=.TRUE.
                  IF(WC.EQ.'C36') CNOT=.TRUE.
                  IF(WC.EQ.'C37') CNOT=.TRUE.
                  IF(WC.EQ.'C38') CNOT=.TRUE.
                  IF(WC.EQ.'C39') CNOT=.TRUE.
                  IF(WC.EQ.'C40') CNOT=.TRUE.
                  IF(WC.EQ.'C41') CNOT=.TRUE.
                  IF(WC.EQ.'C42') CNOT=.TRUE.
                  IF(WC.EQ.'C43') CNOT=.TRUE.
                  IF(WC.EQ.'C44') CNOT=.TRUE.
                  IF(WC.EQ.'C45') CNOT=.TRUE.
                  IF(WC.EQ.'C46') CNOT=.TRUE.
                  IF(WC.EQ.'C47') CNOT=.TRUE.
                  IF(WC.EQ.'C48') CNOT=.TRUE.
                  IF(WC.EQ.'C49') CNOT=.TRUE.
                  IF(WC.EQ.'C50') CNOT=.TRUE.
                  IF(WC.EQ.'C51') CNOT=.TRUE.
                  IF(WC.EQ.'C52') CNOT=.TRUE.
                  IF(WC.EQ.'C53') CNOT=.TRUE.
                  IF(WC.EQ.'C54') CNOT=.TRUE.
                  IF(WC.EQ.'C55') CNOT=.TRUE.
                  IF(WC.EQ.'C56') CNOT=.TRUE.
                  IF(WC.EQ.'C57') CNOT=.TRUE.
                  IF(WC.EQ.'C58') CNOT=.TRUE.
                  IF(WC.EQ.'C59') CNOT=.TRUE.
                  IF(WC.EQ.'C60') CNOT=.TRUE.
                  IF(WC.EQ.'C61') CNOT=.TRUE.
                  IF(WC.EQ.'C62') CNOT=.TRUE.
                  IF(WC.EQ.'C62') CNOT=.TRUE.
                  IF(WC.EQ.'C63') CNOT=.TRUE.
                  IF(WC.EQ.'C64') CNOT=.TRUE.
                  IF(WC.EQ.'C65') CNOT=.TRUE.
                  IF(WC.EQ.'C66') CNOT=.TRUE.
                  IF(WC.EQ.'C67') CNOT=.TRUE.
                  IF(WC.EQ.'C68') CNOT=.TRUE.
                  IF(WC.EQ.'C69') CNOT=.TRUE.
                  IF(WC.EQ.'C70') CNOT=.TRUE.
                  IF(WC.EQ.'C71') CNOT=.TRUE.
                  IF(WC.EQ.'C72') CNOT=.TRUE.
                  IF(WC.EQ.'C73') CNOT=.TRUE.
                  IF(WC.EQ.'C74') CNOT=.TRUE.
                  IF(WC.EQ.'C75') CNOT=.TRUE.
                  IF(WC.EQ.'C76') CNOT=.TRUE.
                  IF(WC.EQ.'C77') CNOT=.TRUE.
                  IF(WC.EQ.'C78') CNOT=.TRUE.
                  IF(WC.EQ.'C79') CNOT=.TRUE.
                  IF(WC.EQ.'C80') CNOT=.TRUE.
                  IF(WC.EQ.'C81') CNOT=.TRUE.
                  IF(WC.EQ.'C82') CNOT=.TRUE.
                  IF(WC.EQ.'C83') CNOT=.TRUE.
                  IF(WC.EQ.'C84') CNOT=.TRUE.
                  IF(WC.EQ.'C85') CNOT=.TRUE.
                  IF(WC.EQ.'C86') CNOT=.TRUE.
                  IF(WC.EQ.'C87') CNOT=.TRUE.
                  IF(WC.EQ.'C88') CNOT=.TRUE.
                  IF(WC.EQ.'C89') CNOT=.TRUE.
                  IF(WC.EQ.'C90') CNOT=.TRUE.
                  IF(WC.EQ.'C91') CNOT=.TRUE.
                  IF(WC.EQ.'C92') CNOT=.TRUE.
                  IF(WC.EQ.'C93') CNOT=.TRUE.
                  IF(WC.EQ.'C94') CNOT=.TRUE.
                  IF(WC.EQ.'C95') CNOT=.TRUE.
                  IF(WC.EQ.'C96') CNOT=.TRUE.
                  IF(CNOT) THEN
                      OUTLYNE=
     1                'SPECIAL SURFACE TYPE 16'
                      CALL SHOWIT(1)
                      OUTLYNE=
     1                'ONLY USES COEFFICIENTS C1 THROUGH C11'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
              IF(DABS(ALENS(34,INT(W1))).EQ.18.0D0) THEN
                  IF(WC.EQ.'C19') CNOT=.TRUE.
                  IF(WC.EQ.'C20') CNOT=.TRUE.
                  IF(WC.EQ.'C21') CNOT=.TRUE.
                  IF(WC.EQ.'C22') CNOT=.TRUE.
                  IF(WC.EQ.'C23') CNOT=.TRUE.
                  IF(WC.EQ.'C24') CNOT=.TRUE.
                  IF(WC.EQ.'C25') CNOT=.TRUE.
                  IF(WC.EQ.'C26') CNOT=.TRUE.
                  IF(WC.EQ.'C27') CNOT=.TRUE.
                  IF(WC.EQ.'C28') CNOT=.TRUE.
                  IF(WC.EQ.'C29') CNOT=.TRUE.
                  IF(WC.EQ.'C30') CNOT=.TRUE.
                  IF(WC.EQ.'C31') CNOT=.TRUE.
                  IF(WC.EQ.'C32') CNOT=.TRUE.
                  IF(WC.EQ.'C33') CNOT=.TRUE.
                  IF(WC.EQ.'C34') CNOT=.TRUE.
                  IF(WC.EQ.'C35') CNOT=.TRUE.
                  IF(WC.EQ.'C36') CNOT=.TRUE.
                  IF(WC.EQ.'C37') CNOT=.TRUE.
                  IF(WC.EQ.'C38') CNOT=.TRUE.
                  IF(WC.EQ.'C39') CNOT=.TRUE.
                  IF(WC.EQ.'C40') CNOT=.TRUE.
                  IF(WC.EQ.'C41') CNOT=.TRUE.
                  IF(WC.EQ.'C42') CNOT=.TRUE.
                  IF(WC.EQ.'C43') CNOT=.TRUE.
                  IF(WC.EQ.'C44') CNOT=.TRUE.
                  IF(WC.EQ.'C45') CNOT=.TRUE.
                  IF(WC.EQ.'C46') CNOT=.TRUE.
                  IF(WC.EQ.'C47') CNOT=.TRUE.
                  IF(WC.EQ.'C48') CNOT=.TRUE.
                  IF(WC.EQ.'C49') CNOT=.TRUE.
                  IF(WC.EQ.'C50') CNOT=.TRUE.
                  IF(WC.EQ.'C51') CNOT=.TRUE.
                  IF(WC.EQ.'C52') CNOT=.TRUE.
                  IF(WC.EQ.'C53') CNOT=.TRUE.
                  IF(WC.EQ.'C54') CNOT=.TRUE.
                  IF(WC.EQ.'C55') CNOT=.TRUE.
                  IF(WC.EQ.'C56') CNOT=.TRUE.
                  IF(WC.EQ.'C57') CNOT=.TRUE.
                  IF(WC.EQ.'C58') CNOT=.TRUE.
                  IF(WC.EQ.'C59') CNOT=.TRUE.
                  IF(WC.EQ.'C60') CNOT=.TRUE.
                  IF(WC.EQ.'C61') CNOT=.TRUE.
                  IF(WC.EQ.'C62') CNOT=.TRUE.
                  IF(WC.EQ.'C62') CNOT=.TRUE.
                  IF(WC.EQ.'C63') CNOT=.TRUE.
                  IF(WC.EQ.'C64') CNOT=.TRUE.
                  IF(WC.EQ.'C65') CNOT=.TRUE.
                  IF(WC.EQ.'C66') CNOT=.TRUE.
                  IF(WC.EQ.'C67') CNOT=.TRUE.
                  IF(WC.EQ.'C68') CNOT=.TRUE.
                  IF(WC.EQ.'C69') CNOT=.TRUE.
                  IF(WC.EQ.'C70') CNOT=.TRUE.
                  IF(WC.EQ.'C71') CNOT=.TRUE.
                  IF(WC.EQ.'C72') CNOT=.TRUE.
                  IF(WC.EQ.'C73') CNOT=.TRUE.
                  IF(WC.EQ.'C74') CNOT=.TRUE.
                  IF(WC.EQ.'C75') CNOT=.TRUE.
                  IF(WC.EQ.'C76') CNOT=.TRUE.
                  IF(WC.EQ.'C77') CNOT=.TRUE.
                  IF(WC.EQ.'C78') CNOT=.TRUE.
                  IF(WC.EQ.'C79') CNOT=.TRUE.
                  IF(WC.EQ.'C80') CNOT=.TRUE.
                  IF(WC.EQ.'C81') CNOT=.TRUE.
                  IF(WC.EQ.'C82') CNOT=.TRUE.
                  IF(WC.EQ.'C83') CNOT=.TRUE.
                  IF(WC.EQ.'C84') CNOT=.TRUE.
                  IF(WC.EQ.'C85') CNOT=.TRUE.
                  IF(WC.EQ.'C86') CNOT=.TRUE.
                  IF(WC.EQ.'C87') CNOT=.TRUE.
                  IF(WC.EQ.'C88') CNOT=.TRUE.
                  IF(WC.EQ.'C89') CNOT=.TRUE.
                  IF(WC.EQ.'C90') CNOT=.TRUE.
                  IF(WC.EQ.'C91') CNOT=.TRUE.
                  IF(WC.EQ.'C92') CNOT=.TRUE.
                  IF(WC.EQ.'C93') CNOT=.TRUE.
                  IF(WC.EQ.'C94') CNOT=.TRUE.
                  IF(WC.EQ.'C95') CNOT=.TRUE.
                  IF(WC.EQ.'C96') CNOT=.TRUE.
                  IF(CNOT) THEN
                      OUTLYNE=
     1                'SPECIAL SURFACE TYPE 18'
                      CALL SHOWIT(1)
                      OUTLYNE=
     1                'ONLY USES COEFFICIENTS C1 THROUGH C18'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
              IF(DABS(ALENS(34,INT(W1))).EQ.3.0D0.OR.
     1        DABS(ALENS(34,INT(W1))).EQ.10.0D0) THEN
                  IF(WC.EQ.'C38') CNOT=.TRUE.
                  IF(WC.EQ.'C39') CNOT=.TRUE.
                  IF(WC.EQ.'C40') CNOT=.TRUE.
                  IF(WC.EQ.'C41') CNOT=.TRUE.
                  IF(WC.EQ.'C42') CNOT=.TRUE.
                  IF(WC.EQ.'C43') CNOT=.TRUE.
                  IF(WC.EQ.'C44') CNOT=.TRUE.
                  IF(WC.EQ.'C45') CNOT=.TRUE.
                  IF(WC.EQ.'C46') CNOT=.TRUE.
                  IF(WC.EQ.'C47') CNOT=.TRUE.
                  IF(WC.EQ.'C48') CNOT=.TRUE.
                  IF(WC.EQ.'C49') CNOT=.TRUE.
                  IF(WC.EQ.'C50') CNOT=.TRUE.
                  IF(WC.EQ.'C51') CNOT=.TRUE.
                  IF(WC.EQ.'C52') CNOT=.TRUE.
                  IF(WC.EQ.'C53') CNOT=.TRUE.
                  IF(WC.EQ.'C54') CNOT=.TRUE.
                  IF(WC.EQ.'C55') CNOT=.TRUE.
                  IF(WC.EQ.'C56') CNOT=.TRUE.
                  IF(WC.EQ.'C57') CNOT=.TRUE.
                  IF(WC.EQ.'C58') CNOT=.TRUE.
                  IF(WC.EQ.'C59') CNOT=.TRUE.
                  IF(WC.EQ.'C60') CNOT=.TRUE.
                  IF(WC.EQ.'C61') CNOT=.TRUE.
                  IF(WC.EQ.'C62') CNOT=.TRUE.
                  IF(WC.EQ.'C63') CNOT=.TRUE.
                  IF(WC.EQ.'C64') CNOT=.TRUE.
                  IF(WC.EQ.'C65') CNOT=.TRUE.
                  IF(WC.EQ.'C66') CNOT=.TRUE.
                  IF(WC.EQ.'C67') CNOT=.TRUE.
                  IF(WC.EQ.'C68') CNOT=.TRUE.
                  IF(WC.EQ.'C69') CNOT=.TRUE.
                  IF(WC.EQ.'C70') CNOT=.TRUE.
                  IF(WC.EQ.'C71') CNOT=.TRUE.
                  IF(WC.EQ.'C72') CNOT=.TRUE.
                  IF(WC.EQ.'C73') CNOT=.TRUE.
                  IF(WC.EQ.'C74') CNOT=.TRUE.
                  IF(WC.EQ.'C75') CNOT=.TRUE.
                  IF(WC.EQ.'C76') CNOT=.TRUE.
                  IF(WC.EQ.'C77') CNOT=.TRUE.
                  IF(WC.EQ.'C78') CNOT=.TRUE.
                  IF(WC.EQ.'C79') CNOT=.TRUE.
                  IF(WC.EQ.'C80') CNOT=.TRUE.
                  IF(WC.EQ.'C81') CNOT=.TRUE.
                  IF(WC.EQ.'C82') CNOT=.TRUE.
                  IF(WC.EQ.'C83') CNOT=.TRUE.
                  IF(WC.EQ.'C84') CNOT=.TRUE.
                  IF(WC.EQ.'C85') CNOT=.TRUE.
                  IF(WC.EQ.'C86') CNOT=.TRUE.
                  IF(WC.EQ.'C87') CNOT=.TRUE.
                  IF(WC.EQ.'C88') CNOT=.TRUE.
                  IF(WC.EQ.'C89') CNOT=.TRUE.
                  IF(WC.EQ.'C90') CNOT=.TRUE.
                  IF(WC.EQ.'C91') CNOT=.TRUE.
                  IF(WC.EQ.'C92') CNOT=.TRUE.
                  IF(WC.EQ.'C93') CNOT=.TRUE.
                  IF(WC.EQ.'C94') CNOT=.TRUE.
                  IF(WC.EQ.'C95') CNOT=.TRUE.
                  IF(WC.EQ.'C96') CNOT=.TRUE.
                  IF(CNOT) THEN
                      OUTLYNE=
     1                'SPECIAL SURFACE TYPES 3 AND 10'
                      CALL SHOWIT(1)
                      OUTLYNE=
     1                'ONLY USE COEFFICIENTS C1 THROUGH C37'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
              IF(DABS(ALENS(34,INT(W1))).EQ.19.0D0) THEN
                  IF(WC.EQ.'C5') CNOT=.TRUE.
                  IF(WC.EQ.'C6') CNOT=.TRUE.
                  IF(WC.EQ.'C7') CNOT=.TRUE.
                  IF(WC.EQ.'C8') CNOT=.TRUE.
                  IF(WC.EQ.'C9') CNOT=.TRUE.
                  IF(WC.EQ.'C10') CNOT=.TRUE.
                  IF(WC.EQ.'C11') CNOT=.TRUE.
                  IF(WC.EQ.'C12') CNOT=.TRUE.
                  IF(WC.EQ.'C13') CNOT=.TRUE.
                  IF(WC.EQ.'C14') CNOT=.TRUE.
                  IF(WC.EQ.'C15') CNOT=.TRUE.
                  IF(WC.EQ.'C16') CNOT=.TRUE.
                  IF(WC.EQ.'C17') CNOT=.TRUE.
                  IF(WC.EQ.'C18') CNOT=.TRUE.
                  IF(WC.EQ.'C19') CNOT=.TRUE.
                  IF(WC.EQ.'C20') CNOT=.TRUE.
                  IF(WC.EQ.'C21') CNOT=.TRUE.
                  IF(WC.EQ.'C22') CNOT=.TRUE.
                  IF(WC.EQ.'C23') CNOT=.TRUE.
                  IF(WC.EQ.'C24') CNOT=.TRUE.
                  IF(WC.EQ.'C25') CNOT=.TRUE.
                  IF(WC.EQ.'C26') CNOT=.TRUE.
                  IF(WC.EQ.'C27') CNOT=.TRUE.
                  IF(WC.EQ.'C28') CNOT=.TRUE.
                  IF(WC.EQ.'C29') CNOT=.TRUE.
                  IF(WC.EQ.'C30') CNOT=.TRUE.
                  IF(WC.EQ.'C31') CNOT=.TRUE.
                  IF(WC.EQ.'C32') CNOT=.TRUE.
                  IF(WC.EQ.'C33') CNOT=.TRUE.
                  IF(WC.EQ.'C34') CNOT=.TRUE.
                  IF(WC.EQ.'C35') CNOT=.TRUE.
                  IF(WC.EQ.'C36') CNOT=.TRUE.
                  IF(WC.EQ.'C37') CNOT=.TRUE.
                  IF(WC.EQ.'C38') CNOT=.TRUE.
                  IF(WC.EQ.'C39') CNOT=.TRUE.
                  IF(WC.EQ.'C40') CNOT=.TRUE.
                  IF(WC.EQ.'C41') CNOT=.TRUE.
                  IF(WC.EQ.'C42') CNOT=.TRUE.
                  IF(WC.EQ.'C43') CNOT=.TRUE.
                  IF(WC.EQ.'C44') CNOT=.TRUE.
                  IF(WC.EQ.'C45') CNOT=.TRUE.
                  IF(WC.EQ.'C46') CNOT=.TRUE.
                  IF(WC.EQ.'C47') CNOT=.TRUE.
                  IF(WC.EQ.'C48') CNOT=.TRUE.
                  IF(WC.EQ.'C49') CNOT=.TRUE.
                  IF(WC.EQ.'C50') CNOT=.TRUE.
                  IF(WC.EQ.'C51') CNOT=.TRUE.
                  IF(WC.EQ.'C52') CNOT=.TRUE.
                  IF(WC.EQ.'C53') CNOT=.TRUE.
                  IF(WC.EQ.'C54') CNOT=.TRUE.
                  IF(WC.EQ.'C55') CNOT=.TRUE.
                  IF(WC.EQ.'C56') CNOT=.TRUE.
                  IF(WC.EQ.'C57') CNOT=.TRUE.
                  IF(WC.EQ.'C58') CNOT=.TRUE.
                  IF(WC.EQ.'C59') CNOT=.TRUE.
                  IF(WC.EQ.'C60') CNOT=.TRUE.
                  IF(WC.EQ.'C61') CNOT=.TRUE.
                  IF(WC.EQ.'C62') CNOT=.TRUE.
                  IF(WC.EQ.'C63') CNOT=.TRUE.
                  IF(WC.EQ.'C64') CNOT=.TRUE.
                  IF(WC.EQ.'C65') CNOT=.TRUE.
                  IF(WC.EQ.'C66') CNOT=.TRUE.
                  IF(WC.EQ.'C67') CNOT=.TRUE.
                  IF(WC.EQ.'C68') CNOT=.TRUE.
                  IF(WC.EQ.'C69') CNOT=.TRUE.
                  IF(WC.EQ.'C70') CNOT=.TRUE.
                  IF(WC.EQ.'C71') CNOT=.TRUE.
                  IF(WC.EQ.'C72') CNOT=.TRUE.
                  IF(WC.EQ.'C73') CNOT=.TRUE.
                  IF(WC.EQ.'C74') CNOT=.TRUE.
                  IF(WC.EQ.'C75') CNOT=.TRUE.
                  IF(WC.EQ.'C76') CNOT=.TRUE.
                  IF(WC.EQ.'C77') CNOT=.TRUE.
                  IF(WC.EQ.'C78') CNOT=.TRUE.
                  IF(WC.EQ.'C79') CNOT=.TRUE.
                  IF(WC.EQ.'C80') CNOT=.TRUE.
                  IF(WC.EQ.'C81') CNOT=.TRUE.
                  IF(WC.EQ.'C82') CNOT=.TRUE.
                  IF(WC.EQ.'C83') CNOT=.TRUE.
                  IF(WC.EQ.'C84') CNOT=.TRUE.
                  IF(WC.EQ.'C85') CNOT=.TRUE.
                  IF(WC.EQ.'C86') CNOT=.TRUE.
                  IF(WC.EQ.'C87') CNOT=.TRUE.
                  IF(WC.EQ.'C88') CNOT=.TRUE.
                  IF(WC.EQ.'C89') CNOT=.TRUE.
                  IF(WC.EQ.'C90') CNOT=.TRUE.
                  IF(WC.EQ.'C91') CNOT=.TRUE.
                  IF(WC.EQ.'C92') CNOT=.TRUE.
                  IF(WC.EQ.'C93') CNOT=.TRUE.
                  IF(WC.EQ.'C94') CNOT=.TRUE.
                  IF(WC.EQ.'C95') CNOT=.TRUE.
                  IF(WC.EQ.'C96') CNOT=.TRUE.
                  IF(CNOT) THEN
                      OUTLYNE=
     1                'SPECIAL SURFACE TYPE 19'
                      CALL SHOWIT(1)
                      OUTLYNE=
     1                'ONLY USE COEFFICIENTS C1 THROUGH C4'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
C     CHECK FOR VALID INPUTS
                  IF(WC.EQ.'C1') THEN
                      IF(W2.LT.0.0D0.OR.W1.GT.99.0D0) THEN
                          OUTLYNE='FOR A TYPE 19 SPECIAL SURFACE'
                          CALL SHOWIT(1)
                          OUTLYNE='"C1" MUST BE SET TO AN INTEGER VALUE BETWEEN'
                          CALL SHOWIT(1)
                          OUTLYNE='1 AND 99'
                          CALL SHOWIT(1)
                          OUTLYNE='RE-ENTER COMMAND'
                          CALL SHOWIT(1)
                      END IF
                  END IF
                  IF(WC.EQ.'C2') THEN
                      IF(W2.LT.5.0D0) THEN
                          OUTLYNE='FOR A TYPE 19 SPECIAL SURFACE'
                          CALL SHOWIT(1)
                          OUTLYNE='"C2" MUST BE SET TO AN INTEGER VALUE GREATER THAN 4'
                          CALL SHOWIT(1)
                          OUTLYNE='RE-ENTER COMMAND'
                          CALL SHOWIT(1)
                      END IF
                  END IF
                  IF(WC.EQ.'C3') THEN
                      IF(W3.LT.0.0D0) THEN
                          OUTLYNE='FOR A TYPE 19 SPECIAL SURFACE'
                          CALL SHOWIT(1)
                          OUTLYNE='"C3" MAY NOT BE SET NEGATIVE'
                          CALL SHOWIT(1)
                          OUTLYNE='RE-ENTER COMMAND'
                          CALL SHOWIT(1)
                      END IF
                  END IF
              END IF
              IF(DABS(ALENS(34,INT(W1))).EQ.20.0D0) THEN
                  IF(WC.EQ.'C6') CNOT=.TRUE.
                  IF(WC.EQ.'C7') CNOT=.TRUE.
                  IF(WC.EQ.'C8') CNOT=.TRUE.
                  IF(WC.EQ.'C9') CNOT=.TRUE.
                  IF(WC.EQ.'C10') CNOT=.TRUE.
                  IF(WC.EQ.'C11') CNOT=.TRUE.
                  IF(WC.EQ.'C12') CNOT=.TRUE.
                  IF(WC.EQ.'C13') CNOT=.TRUE.
                  IF(WC.EQ.'C14') CNOT=.TRUE.
                  IF(WC.EQ.'C15') CNOT=.TRUE.
                  IF(WC.EQ.'C16') CNOT=.TRUE.
                  IF(WC.EQ.'C17') CNOT=.TRUE.
                  IF(WC.EQ.'C18') CNOT=.TRUE.
                  IF(WC.EQ.'C19') CNOT=.TRUE.
                  IF(WC.EQ.'C20') CNOT=.TRUE.
                  IF(WC.EQ.'C21') CNOT=.TRUE.
                  IF(WC.EQ.'C22') CNOT=.TRUE.
                  IF(WC.EQ.'C23') CNOT=.TRUE.
                  IF(WC.EQ.'C24') CNOT=.TRUE.
                  IF(WC.EQ.'C25') CNOT=.TRUE.
                  IF(WC.EQ.'C26') CNOT=.TRUE.
                  IF(WC.EQ.'C27') CNOT=.TRUE.
                  IF(WC.EQ.'C28') CNOT=.TRUE.
                  IF(WC.EQ.'C29') CNOT=.TRUE.
                  IF(WC.EQ.'C30') CNOT=.TRUE.
                  IF(WC.EQ.'C31') CNOT=.TRUE.
                  IF(WC.EQ.'C32') CNOT=.TRUE.
                  IF(WC.EQ.'C33') CNOT=.TRUE.
                  IF(WC.EQ.'C34') CNOT=.TRUE.
                  IF(WC.EQ.'C35') CNOT=.TRUE.
                  IF(WC.EQ.'C36') CNOT=.TRUE.
                  IF(WC.EQ.'C37') CNOT=.TRUE.
                  IF(WC.EQ.'C38') CNOT=.TRUE.
                  IF(WC.EQ.'C39') CNOT=.TRUE.
                  IF(WC.EQ.'C40') CNOT=.TRUE.
                  IF(WC.EQ.'C41') CNOT=.TRUE.
                  IF(WC.EQ.'C42') CNOT=.TRUE.
                  IF(WC.EQ.'C43') CNOT=.TRUE.
                  IF(WC.EQ.'C44') CNOT=.TRUE.
                  IF(WC.EQ.'C45') CNOT=.TRUE.
                  IF(WC.EQ.'C46') CNOT=.TRUE.
                  IF(WC.EQ.'C47') CNOT=.TRUE.
                  IF(WC.EQ.'C48') CNOT=.TRUE.
                  IF(WC.EQ.'C49') CNOT=.TRUE.
                  IF(WC.EQ.'C50') CNOT=.TRUE.
                  IF(WC.EQ.'C51') CNOT=.TRUE.
                  IF(WC.EQ.'C52') CNOT=.TRUE.
                  IF(WC.EQ.'C53') CNOT=.TRUE.
                  IF(WC.EQ.'C54') CNOT=.TRUE.
                  IF(WC.EQ.'C55') CNOT=.TRUE.
                  IF(WC.EQ.'C56') CNOT=.TRUE.
                  IF(WC.EQ.'C57') CNOT=.TRUE.
                  IF(WC.EQ.'C58') CNOT=.TRUE.
                  IF(WC.EQ.'C59') CNOT=.TRUE.
                  IF(WC.EQ.'C60') CNOT=.TRUE.
                  IF(WC.EQ.'C61') CNOT=.TRUE.
                  IF(WC.EQ.'C62') CNOT=.TRUE.
                  IF(WC.EQ.'C63') CNOT=.TRUE.
                  IF(WC.EQ.'C64') CNOT=.TRUE.
                  IF(WC.EQ.'C65') CNOT=.TRUE.
                  IF(WC.EQ.'C66') CNOT=.TRUE.
                  IF(WC.EQ.'C67') CNOT=.TRUE.
                  IF(WC.EQ.'C68') CNOT=.TRUE.
                  IF(WC.EQ.'C69') CNOT=.TRUE.
                  IF(WC.EQ.'C70') CNOT=.TRUE.
                  IF(WC.EQ.'C71') CNOT=.TRUE.
                  IF(WC.EQ.'C72') CNOT=.TRUE.
                  IF(WC.EQ.'C73') CNOT=.TRUE.
                  IF(WC.EQ.'C74') CNOT=.TRUE.
                  IF(WC.EQ.'C75') CNOT=.TRUE.
                  IF(WC.EQ.'C76') CNOT=.TRUE.
                  IF(WC.EQ.'C77') CNOT=.TRUE.
                  IF(WC.EQ.'C78') CNOT=.TRUE.
                  IF(WC.EQ.'C79') CNOT=.TRUE.
                  IF(WC.EQ.'C80') CNOT=.TRUE.
                  IF(WC.EQ.'C81') CNOT=.TRUE.
                  IF(WC.EQ.'C82') CNOT=.TRUE.
                  IF(WC.EQ.'C83') CNOT=.TRUE.
                  IF(WC.EQ.'C84') CNOT=.TRUE.
                  IF(WC.EQ.'C85') CNOT=.TRUE.
                  IF(WC.EQ.'C86') CNOT=.TRUE.
                  IF(WC.EQ.'C87') CNOT=.TRUE.
                  IF(WC.EQ.'C88') CNOT=.TRUE.
                  IF(WC.EQ.'C89') CNOT=.TRUE.
                  IF(WC.EQ.'C90') CNOT=.TRUE.
                  IF(WC.EQ.'C91') CNOT=.TRUE.
                  IF(WC.EQ.'C92') CNOT=.TRUE.
                  IF(WC.EQ.'C93') CNOT=.TRUE.
                  IF(WC.EQ.'C94') CNOT=.TRUE.
                  IF(WC.EQ.'C95') CNOT=.TRUE.
                  IF(WC.EQ.'C96') CNOT=.TRUE.
                  IF(CNOT) THEN
                      OUTLYNE=
     1                'SPECIAL SURFACE TYPE 20'
                      CALL SHOWIT(1)
                      OUTLYNE=
     1                'ONLY USE COEFFICIENTS C1 THROUGH C5'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  IF(WC.EQ.'C1') THEN
                      IF(W2.LT.0.0D0.OR.W1.GT.99.0D0) THEN
                          OUTLYNE='FOR A TYPE 20 SPECIAL SURFACE'
                          CALL SHOWIT(1)
                          OUTLYNE='"C1" MUST BE SET TO AN INTEGER VALUE BETWEEN'
                          CALL SHOWIT(1)
                          OUTLYNE='1 AND 99'
                          CALL SHOWIT(1)
                          OUTLYNE='RE-ENTER COMMAND'
                          CALL SHOWIT(1)
                      END IF
                  END IF
                  IF(WC.EQ.'C2') THEN
                      IF(W2.LT.5.0D0) THEN
                          OUTLYNE='FOR A TYPE 20 SPECIAL SURFACE'
                          CALL SHOWIT(1)
                          OUTLYNE='"C2" MUST BE SET TO AN INTEGER VALUE GREATER THAN 4'
                          CALL SHOWIT(1)
                          OUTLYNE='RE-ENTER COMMAND'
                          CALL SHOWIT(1)
                      END IF
                  END IF
                  IF(WC.EQ.'C3') THEN
                      IF(W3.LT.0.0D0) THEN
                          OUTLYNE='FOR A TYPE 20 SPECIAL SURFACE'
                          CALL SHOWIT(1)
                          OUTLYNE='"C3" MAY NOT BE SET NEGATIVE'
                          CALL SHOWIT(1)
                          OUTLYNE='RE-ENTER COMMAND'
                          CALL SHOWIT(1)
                      END IF
                  END IF
                  IF(WC.EQ.'C4') THEN
                      IF(W2.NE.0.0D0.AND.W2.NE.1.0D0.AND.W2.NE.2.0D0) THEN
                          OUTLYNE='FOR A TYPE 20 SPECIAL SURFACE'
                          CALL SHOWIT(1)
                          OUTLYNE='"C4" CAN ONLY BE SET TO 0, 1 OR 2'
                          CALL SHOWIT(1)
                          OUTLYNE='RE-ENTER COMMAND'
                          CALL SHOWIT(1)
                      END IF
                  END IF
              END IF
              IF(DABS(ALENS(34,INT(W1))).EQ.22.0D0) THEN
                  IF(WC.EQ.'C5') CNOT=.TRUE.
                  IF(WC.EQ.'C6') CNOT=.TRUE.
                  IF(WC.EQ.'C7') CNOT=.TRUE.
                  IF(WC.EQ.'C8') CNOT=.TRUE.
                  IF(WC.EQ.'C9') CNOT=.TRUE.
                  IF(WC.EQ.'C10') CNOT=.TRUE.
                  IF(WC.EQ.'C11') CNOT=.TRUE.
                  IF(WC.EQ.'C12') CNOT=.TRUE.
                  IF(WC.EQ.'C13') CNOT=.TRUE.
                  IF(WC.EQ.'C14') CNOT=.TRUE.
                  IF(WC.EQ.'C15') CNOT=.TRUE.
                  IF(WC.EQ.'C16') CNOT=.TRUE.
                  IF(WC.EQ.'C17') CNOT=.TRUE.
                  IF(WC.EQ.'C18') CNOT=.TRUE.
                  IF(WC.EQ.'C19') CNOT=.TRUE.
                  IF(WC.EQ.'C20') CNOT=.TRUE.
                  IF(WC.EQ.'C21') CNOT=.TRUE.
                  IF(WC.EQ.'C22') CNOT=.TRUE.
                  IF(WC.EQ.'C23') CNOT=.TRUE.
                  IF(WC.EQ.'C24') CNOT=.TRUE.
                  IF(WC.EQ.'C25') CNOT=.TRUE.
                  IF(WC.EQ.'C26') CNOT=.TRUE.
                  IF(WC.EQ.'C27') CNOT=.TRUE.
                  IF(WC.EQ.'C28') CNOT=.TRUE.
                  IF(WC.EQ.'C29') CNOT=.TRUE.
                  IF(WC.EQ.'C30') CNOT=.TRUE.
                  IF(WC.EQ.'C31') CNOT=.TRUE.
                  IF(WC.EQ.'C32') CNOT=.TRUE.
                  IF(WC.EQ.'C33') CNOT=.TRUE.
                  IF(WC.EQ.'C34') CNOT=.TRUE.
                  IF(WC.EQ.'C35') CNOT=.TRUE.
                  IF(WC.EQ.'C36') CNOT=.TRUE.
                  IF(WC.EQ.'C37') CNOT=.TRUE.
                  IF(WC.EQ.'C38') CNOT=.TRUE.
                  IF(WC.EQ.'C39') CNOT=.TRUE.
                  IF(WC.EQ.'C40') CNOT=.TRUE.
                  IF(WC.EQ.'C41') CNOT=.TRUE.
                  IF(WC.EQ.'C42') CNOT=.TRUE.
                  IF(WC.EQ.'C43') CNOT=.TRUE.
                  IF(WC.EQ.'C44') CNOT=.TRUE.
                  IF(WC.EQ.'C45') CNOT=.TRUE.
                  IF(WC.EQ.'C46') CNOT=.TRUE.
                  IF(WC.EQ.'C47') CNOT=.TRUE.
                  IF(WC.EQ.'C48') CNOT=.TRUE.
                  IF(WC.EQ.'C49') CNOT=.TRUE.
                  IF(WC.EQ.'C50') CNOT=.TRUE.
                  IF(WC.EQ.'C51') CNOT=.TRUE.
                  IF(WC.EQ.'C52') CNOT=.TRUE.
                  IF(WC.EQ.'C53') CNOT=.TRUE.
                  IF(WC.EQ.'C54') CNOT=.TRUE.
                  IF(WC.EQ.'C55') CNOT=.TRUE.
                  IF(WC.EQ.'C56') CNOT=.TRUE.
                  IF(WC.EQ.'C57') CNOT=.TRUE.
                  IF(WC.EQ.'C58') CNOT=.TRUE.
                  IF(WC.EQ.'C59') CNOT=.TRUE.
                  IF(WC.EQ.'C60') CNOT=.TRUE.
                  IF(WC.EQ.'C61') CNOT=.TRUE.
                  IF(WC.EQ.'C62') CNOT=.TRUE.
                  IF(WC.EQ.'C63') CNOT=.TRUE.
                  IF(WC.EQ.'C64') CNOT=.TRUE.
                  IF(WC.EQ.'C65') CNOT=.TRUE.
                  IF(WC.EQ.'C66') CNOT=.TRUE.
                  IF(WC.EQ.'C67') CNOT=.TRUE.
                  IF(WC.EQ.'C68') CNOT=.TRUE.
                  IF(WC.EQ.'C69') CNOT=.TRUE.
                  IF(WC.EQ.'C70') CNOT=.TRUE.
                  IF(WC.EQ.'C71') CNOT=.TRUE.
                  IF(WC.EQ.'C72') CNOT=.TRUE.
                  IF(WC.EQ.'C73') CNOT=.TRUE.
                  IF(WC.EQ.'C74') CNOT=.TRUE.
                  IF(WC.EQ.'C75') CNOT=.TRUE.
                  IF(WC.EQ.'C76') CNOT=.TRUE.
                  IF(WC.EQ.'C77') CNOT=.TRUE.
                  IF(WC.EQ.'C78') CNOT=.TRUE.
                  IF(WC.EQ.'C79') CNOT=.TRUE.
                  IF(WC.EQ.'C80') CNOT=.TRUE.
                  IF(WC.EQ.'C81') CNOT=.TRUE.
                  IF(WC.EQ.'C82') CNOT=.TRUE.
                  IF(WC.EQ.'C83') CNOT=.TRUE.
                  IF(WC.EQ.'C84') CNOT=.TRUE.
                  IF(WC.EQ.'C85') CNOT=.TRUE.
                  IF(WC.EQ.'C86') CNOT=.TRUE.
                  IF(WC.EQ.'C87') CNOT=.TRUE.
                  IF(WC.EQ.'C88') CNOT=.TRUE.
                  IF(WC.EQ.'C89') CNOT=.TRUE.
                  IF(WC.EQ.'C90') CNOT=.TRUE.
                  IF(WC.EQ.'C91') CNOT=.TRUE.
                  IF(WC.EQ.'C92') CNOT=.TRUE.
                  IF(WC.EQ.'C93') CNOT=.TRUE.
                  IF(WC.EQ.'C94') CNOT=.TRUE.
                  IF(WC.EQ.'C95') CNOT=.TRUE.
                  IF(WC.EQ.'C96') CNOT=.TRUE.
                  IF(CNOT) THEN
                      OUTLYNE=
     1                'SPECIAL SURFACE TYPE 22'
                      CALL SHOWIT(1)
                      OUTLYNE=
     1                'ONLY USE COEFFICIENTS C1 THROUGH C4'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  IF(WC.EQ.'C1') THEN
                      IF(W2.LT.0.0D0.OR.W1.GT.99.0D0) THEN
                          OUTLYNE='FOR A TYPE 22 SPECIAL SURFACE'
                          CALL SHOWIT(1)
                          OUTLYNE='"C1" MUST BE SET TO AN INTEGER VALUE BETWEEN'
                          CALL SHOWIT(1)
                          OUTLYNE='1 AND 99'
                          CALL SHOWIT(1)
                          OUTLYNE='RE-ENTER COMMAND'
                          CALL SHOWIT(1)
                      END IF
                  END IF
                  IF(WC.EQ.'C2') THEN
                      IF(W2.LT.5.0D0) THEN
                          OUTLYNE='FOR A TYPE 22 SPECIAL SURFACE'
                          CALL SHOWIT(1)
                          OUTLYNE='"C2" MUST BE SET TO AN INTEGER VALUE GREATER THAN 4'
                          CALL SHOWIT(1)
                          OUTLYNE='RE-ENTER COMMAND'
                          CALL SHOWIT(1)
                      END IF
                  END IF
                  IF(WC.EQ.'C3') THEN
                      IF(W3.LT.0.0D0) THEN
                          OUTLYNE='FOR A TYPE 22 SPECIAL SURFACE'
                          CALL SHOWIT(1)
                          OUTLYNE='"C3" MAY NOT BE SET NEGATIVE'
                          CALL SHOWIT(1)
                          OUTLYNE='RE-ENTER COMMAND'
                          CALL SHOWIT(1)
                      END IF
                  END IF
              END IF
              IF(DABS(ALENS(34,INT(W1))).EQ.4.0D0) THEN
                  IF(WC.NE.'C1'.AND.WC.NE.'C2'.AND.WC.NE.'C3'
     1            .AND.WC.NE.'C4'.AND.WC.NE.'C5'.AND.WC.NE.'C6'
     1            .AND.WC.NE.'C7'.AND.WC.NE.'C8'.AND.WC.NE.'C9'
     1            .AND.WC.NE.'C10'.AND.WC.NE.'C11'.AND.WC.NE.'C12'
     1            .AND.WC.NE.'C13'.AND.WC.NE.'C14'.AND.WC.NE.'C15')THEN
                      OUTLYNE=
     1                'SPECIAL SURFACE TYPE 4 ONLY USES COEFFICIENTS C1 THROUGH C15'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
              IF(DABS(ALENS(34,INT(W1))).EQ.7.0D0.OR.
     1        DABS(ALENS(34,INT(W1))).EQ.8.0D0) THEN
                  IF(WC.EQ.'C93') CNOT=.TRUE.
                  IF(WC.EQ.'C94') CNOT=.TRUE.
                  IF(WC.EQ.'C95') CNOT=.TRUE.
                  IF(WC.EQ.'C96') CNOT=.TRUE.
                  IF(CNOT) THEN
                      OUTLYNE=
     1                'SPECIAL SURFACE TYPES 7 AND 8'
                      CALL SHOWIT(1)
                      OUTLYNE=
     1                'ONLY USE COEFFICIENTS C1 THROUGH C91'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
              IF(DABS(ALENS(34,INT(W1))).EQ.12.0D0) THEN
                  IF(WC.EQ.'C90') CNOT=.TRUE.
                  IF(WC.EQ.'C91') CNOT=.TRUE.
                  IF(WC.EQ.'C92') CNOT=.TRUE.
                  IF(WC.EQ.'C93') CNOT=.TRUE.
                  IF(WC.EQ.'C94') CNOT=.TRUE.
                  IF(WC.EQ.'C95') CNOT=.TRUE.
                  IF(WC.EQ.'C96') CNOT=.TRUE.
                  IF(CNOT) THEN
                      OUTLYNE=
     1                'SPECIAL SURFACE TYPE 12'
                      CALL SHOWIT(1)
                      OUTLYNE=
     1                'ONLY USES COEFFICIENTS C1 THROUGH C89'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
              IF(DABS(ALENS(34,INT(W1))).EQ.13.0D0) THEN
                  IF(WC.EQ.'C15') CNOT=.TRUE.
                  IF(WC.EQ.'C16') CNOT=.TRUE.
                  IF(WC.EQ.'C17') CNOT=.TRUE.
                  IF(WC.EQ.'C18') CNOT=.TRUE.
                  IF(WC.EQ.'C19') CNOT=.TRUE.
                  IF(WC.EQ.'C20') CNOT=.TRUE.
                  IF(WC.EQ.'C21') CNOT=.TRUE.
                  IF(WC.EQ.'C22') CNOT=.TRUE.
                  IF(WC.EQ.'C23') CNOT=.TRUE.
                  IF(WC.EQ.'C24') CNOT=.TRUE.
                  IF(WC.EQ.'C25') CNOT=.TRUE.
                  IF(WC.EQ.'C26') CNOT=.TRUE.
                  IF(WC.EQ.'C27') CNOT=.TRUE.
                  IF(WC.EQ.'C28') CNOT=.TRUE.
                  IF(WC.EQ.'C29') CNOT=.TRUE.
                  IF(WC.EQ.'C30') CNOT=.TRUE.
                  IF(WC.EQ.'C31') CNOT=.TRUE.
                  IF(WC.EQ.'C32') CNOT=.TRUE.
                  IF(WC.EQ.'C33') CNOT=.TRUE.
                  IF(WC.EQ.'C34') CNOT=.TRUE.
                  IF(WC.EQ.'C35') CNOT=.TRUE.
                  IF(WC.EQ.'C36') CNOT=.TRUE.
                  IF(WC.EQ.'C37') CNOT=.TRUE.
                  IF(WC.EQ.'C38') CNOT=.TRUE.
                  IF(WC.EQ.'C39') CNOT=.TRUE.
                  IF(WC.EQ.'C40') CNOT=.TRUE.
                  IF(WC.EQ.'C41') CNOT=.TRUE.
                  IF(WC.EQ.'C42') CNOT=.TRUE.
                  IF(WC.EQ.'C43') CNOT=.TRUE.
                  IF(WC.EQ.'C44') CNOT=.TRUE.
                  IF(WC.EQ.'C45') CNOT=.TRUE.
                  IF(WC.EQ.'C46') CNOT=.TRUE.
                  IF(WC.EQ.'C47') CNOT=.TRUE.
                  IF(WC.EQ.'C48') CNOT=.TRUE.
                  IF(WC.EQ.'C49') CNOT=.TRUE.
                  IF(WC.EQ.'C50') CNOT=.TRUE.
                  IF(WC.EQ.'C51') CNOT=.TRUE.
                  IF(WC.EQ.'C52') CNOT=.TRUE.
                  IF(WC.EQ.'C53') CNOT=.TRUE.
                  IF(WC.EQ.'C54') CNOT=.TRUE.
                  IF(WC.EQ.'C55') CNOT=.TRUE.
                  IF(WC.EQ.'C56') CNOT=.TRUE.
                  IF(WC.EQ.'C57') CNOT=.TRUE.
                  IF(WC.EQ.'C58') CNOT=.TRUE.
                  IF(WC.EQ.'C59') CNOT=.TRUE.
                  IF(WC.EQ.'C60') CNOT=.TRUE.
                  IF(WC.EQ.'C61') CNOT=.TRUE.
                  IF(WC.EQ.'C62') CNOT=.TRUE.
                  IF(WC.EQ.'C63') CNOT=.TRUE.
                  IF(WC.EQ.'C64') CNOT=.TRUE.
                  IF(WC.EQ.'C65') CNOT=.TRUE.
                  IF(WC.EQ.'C66') CNOT=.TRUE.
                  IF(WC.EQ.'C67') CNOT=.TRUE.
                  IF(WC.EQ.'C68') CNOT=.TRUE.
                  IF(WC.EQ.'C69') CNOT=.TRUE.
                  IF(WC.EQ.'C70') CNOT=.TRUE.
                  IF(WC.EQ.'C71') CNOT=.TRUE.
                  IF(WC.EQ.'C72') CNOT=.TRUE.
                  IF(WC.EQ.'C73') CNOT=.TRUE.
                  IF(WC.EQ.'C74') CNOT=.TRUE.
                  IF(WC.EQ.'C75') CNOT=.TRUE.
                  IF(WC.EQ.'C76') CNOT=.TRUE.
                  IF(WC.EQ.'C77') CNOT=.TRUE.
                  IF(WC.EQ.'C78') CNOT=.TRUE.
                  IF(WC.EQ.'C79') CNOT=.TRUE.
                  IF(WC.EQ.'C80') CNOT=.TRUE.
                  IF(WC.EQ.'C81') CNOT=.TRUE.
                  IF(WC.EQ.'C82') CNOT=.TRUE.
                  IF(WC.EQ.'C83') CNOT=.TRUE.
                  IF(WC.EQ.'C84') CNOT=.TRUE.
                  IF(WC.EQ.'C85') CNOT=.TRUE.
                  IF(WC.EQ.'C86') CNOT=.TRUE.
                  IF(WC.EQ.'C87') CNOT=.TRUE.
                  IF(WC.EQ.'C88') CNOT=.TRUE.
                  IF(WC.EQ.'C89') CNOT=.TRUE.
                  IF(WC.EQ.'C90') CNOT=.TRUE.
                  IF(WC.EQ.'C91') CNOT=.TRUE.
                  IF(WC.EQ.'C92') CNOT=.TRUE.
                  IF(WC.EQ.'C93') CNOT=.TRUE.
                  IF(WC.EQ.'C94') CNOT=.TRUE.
                  IF(WC.EQ.'C95') CNOT=.TRUE.
                  IF(WC.EQ.'C96') CNOT=.TRUE.
                  IF(CNOT) THEN
                      OUTLYNE=
     1                'SPECIAL SURFACE TYPE 13'
                      CALL SHOWIT(1)
                      OUTLYNE=
     1                'ONLY USES COEFFICIENTS C1 THROUGH C14'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
              IF(WC.EQ.'C1') FTFL01(1,INT(W1))=W2
              IF(WC.EQ.'C2') FTFL01(2,INT(W1))=W2
              IF(WC.EQ.'C3') FTFL01(3,INT(W1))=W2
              IF(WC.EQ.'C4') FTFL01(4,INT(W1))=W2
              IF(WC.EQ.'C5') FTFL01(5,INT(W1))=W2
              IF(WC.EQ.'C6') FTFL01(6,INT(W1))=W2
              IF(WC.EQ.'C7') FTFL01(7,INT(W1))=W2
              IF(WC.EQ.'C8') FTFL01(8,INT(W1))=W2
              IF(WC.EQ.'C9') FTFL01(9,INT(W1))=W2
              IF(WC.EQ.'C10') FTFL01(10,INT(W1))=W2
              IF(WC.EQ.'C11') FTFL01(11,INT(W1))=W2
              IF(WC.EQ.'C12') FTFL01(12,INT(W1))=W2
              IF(WC.EQ.'C13') FTFL01(13,INT(W1))=W2
              IF(WC.EQ.'C14') FTFL01(14,INT(W1))=W2
              IF(WC.EQ.'C15') FTFL01(15,INT(W1))=W2
              IF(WC.EQ.'C16') FTFL01(16,INT(W1))=W2
              IF(WC.EQ.'C17') FTFL01(17,INT(W1))=W2
              IF(WC.EQ.'C18') FTFL01(18,INT(W1))=W2
              IF(WC.EQ.'C19') FTFL01(19,INT(W1))=W2
              IF(WC.EQ.'C20') FTFL01(20,INT(W1))=W2
              IF(WC.EQ.'C21') FTFL01(21,INT(W1))=W2
              IF(WC.EQ.'C22') FTFL01(22,INT(W1))=W2
              IF(WC.EQ.'C23') FTFL01(23,INT(W1))=W2
              IF(WC.EQ.'C24') FTFL01(24,INT(W1))=W2
              IF(WC.EQ.'C25') FTFL01(25,INT(W1))=W2
              IF(WC.EQ.'C26') FTFL01(26,INT(W1))=W2
              IF(WC.EQ.'C27') FTFL01(27,INT(W1))=W2
              IF(WC.EQ.'C28') FTFL01(28,INT(W1))=W2
              IF(WC.EQ.'C29') FTFL01(29,INT(W1))=W2
              IF(WC.EQ.'C30') FTFL01(30,INT(W1))=W2
              IF(WC.EQ.'C31') FTFL01(31,INT(W1))=W2
              IF(WC.EQ.'C32') FTFL01(32,INT(W1))=W2
              IF(WC.EQ.'C33') FTFL01(33,INT(W1))=W2
              IF(WC.EQ.'C34') FTFL01(34,INT(W1))=W2
              IF(WC.EQ.'C35') FTFL01(35,INT(W1))=W2
              IF(WC.EQ.'C36') FTFL01(36,INT(W1))=W2
              IF(WC.EQ.'C37') FTFL01(37,INT(W1))=W2
              IF(WC.EQ.'C38') FTFL01(38,INT(W1))=W2
              IF(WC.EQ.'C39') FTFL01(39,INT(W1))=W2
              IF(WC.EQ.'C40') FTFL01(40,INT(W1))=W2
              IF(WC.EQ.'C41') FTFL01(41,INT(W1))=W2
              IF(WC.EQ.'C42') FTFL01(42,INT(W1))=W2
              IF(WC.EQ.'C43') FTFL01(43,INT(W1))=W2
              IF(WC.EQ.'C44') FTFL01(44,INT(W1))=W2
              IF(WC.EQ.'C45') FTFL01(45,INT(W1))=W2
              IF(WC.EQ.'C46') FTFL01(46,INT(W1))=W2
              IF(WC.EQ.'C47') FTFL01(47,INT(W1))=W2
              IF(WC.EQ.'C48') FTFL01(48,INT(W1))=W2
              IF(WC.EQ.'C49') FTFL01(49,INT(W1))=W2
              IF(WC.EQ.'C50') FTFL01(50,INT(W1))=W2
              IF(WC.EQ.'C51') FTFL01(51,INT(W1))=W2
              IF(WC.EQ.'C52') FTFL01(52,INT(W1))=W2
              IF(WC.EQ.'C53') FTFL01(53,INT(W1))=W2
              IF(WC.EQ.'C54') FTFL01(54,INT(W1))=W2
              IF(WC.EQ.'C55') FTFL01(55,INT(W1))=W2
              IF(WC.EQ.'C56') FTFL01(56,INT(W1))=W2
              IF(WC.EQ.'C57') FTFL01(57,INT(W1))=W2
              IF(WC.EQ.'C58') FTFL01(58,INT(W1))=W2
              IF(WC.EQ.'C59') FTFL01(59,INT(W1))=W2
              IF(WC.EQ.'C60') FTFL01(60,INT(W1))=W2
              IF(WC.EQ.'C61') FTFL01(61,INT(W1))=W2
              IF(WC.EQ.'C62') FTFL01(62,INT(W1))=W2
              IF(WC.EQ.'C63') FTFL01(63,INT(W1))=W2
              IF(WC.EQ.'C64') FTFL01(64,INT(W1))=W2
              IF(WC.EQ.'C65') FTFL01(65,INT(W1))=W2
              IF(WC.EQ.'C66') FTFL01(66,INT(W1))=W2
              IF(WC.EQ.'C67') FTFL01(67,INT(W1))=W2
              IF(WC.EQ.'C68') FTFL01(68,INT(W1))=W2
              IF(WC.EQ.'C69') FTFL01(69,INT(W1))=W2
              IF(WC.EQ.'C70') FTFL01(70,INT(W1))=W2
              IF(WC.EQ.'C71') FTFL01(71,INT(W1))=W2
              IF(WC.EQ.'C72') FTFL01(72,INT(W1))=W2
              IF(WC.EQ.'C73') FTFL01(73,INT(W1))=W2
              IF(WC.EQ.'C74') FTFL01(74,INT(W1))=W2
              IF(WC.EQ.'C75') FTFL01(75,INT(W1))=W2
              IF(WC.EQ.'C76') FTFL01(76,INT(W1))=W2
              IF(WC.EQ.'C77') FTFL01(77,INT(W1))=W2
              IF(WC.EQ.'C78') FTFL01(78,INT(W1))=W2
              IF(WC.EQ.'C79') FTFL01(79,INT(W1))=W2
              IF(WC.EQ.'C80') FTFL01(80,INT(W1))=W2
              IF(WC.EQ.'C81') FTFL01(81,INT(W1))=W2
              IF(WC.EQ.'C82') FTFL01(82,INT(W1))=W2
              IF(WC.EQ.'C83') FTFL01(83,INT(W1))=W2
              IF(WC.EQ.'C84') FTFL01(84,INT(W1))=W2
              IF(WC.EQ.'C85') FTFL01(85,INT(W1))=W2
              IF(WC.EQ.'C86') FTFL01(86,INT(W1))=W2
              IF(WC.EQ.'C87') FTFL01(87,INT(W1))=W2
              IF(WC.EQ.'C88') FTFL01(88,INT(W1))=W2
              IF(WC.EQ.'C89') FTFL01(89,INT(W1))=W2
              IF(WC.EQ.'C90') FTFL01(90,INT(W1))=W2
              IF(WC.EQ.'C91') FTFL01(91,INT(W1))=W2
              IF(WC.EQ.'C92') FTFL01(92,INT(W1))=W2
              IF(WC.EQ.'C93') FTFL01(93,INT(W1))=W2
              IF(WC.EQ.'C94') FTFL01(94,INT(W1))=W2
              IF(WC.EQ.'C95') FTFL01(95,INT(W1))=W2
              IF(WC.EQ.'C96') FTFL01(96,INT(W1))=W2
              IF(PIKUP(1,INT(W1),11).GT.0.0D0) THEN
                  PIKUP(1,INT(W1),11)=0.0D0
                  PIKUP(2,INT(W1),11)=0.0D0
                  PIKUP(3,INT(W1),11)=0.0D0
                  PIKUP(4,INT(W1),11)=0.0D0
                  PIKUP(5,INT(W1),11)=0.0D0
                  PIKUP(6,INT(W1),11)=0.0D0
                  ALENS(32,INT(W1))=ALENS(32,INT(W1))-1.0D0
                  WRITE(OUTLYNE,*)'SURFACE',INT(W1),' :PIKUP (PRO) DELETED'
                  CALL SHOWIT(1)
              END IF
              IF(PIKUP(1,INT(W1),12).GT.0.0D0) THEN
                  PIKUP(1,INT(W1),12)=0.0D0
                  PIKUP(2,INT(W1),12)=0.0D0
                  PIKUP(3,INT(W1),12)=0.0D0
                  PIKUP(4,INT(W1),12)=0.0D0
                  PIKUP(5,INT(W1),12)=0.0D0
                  PIKUP(6,INT(W1),12)=0.0D0
                  ALENS(32,INT(W1))=ALENS(32,INT(W1))-1.0D0
                  WRITE(OUTLYNE,*)'SURFACE',INT(W1),' :PIKUP (NPRO) DELETED'
                  CALL SHOWIT(1)
              END IF
C
C       ARE THERE MORE PIKUPS? IF NOT SET ALENS(32,INT(W1)) TO ZERO.
C
              PIKCNT=0
              DO 10 I=1,PSIZ
                  IF(PIKUP(1,INT(W1),I).NE.0.0D0) THEN
                      PIKCNT=PIKCNT+1
                  ELSE
                  END IF
 10           CONTINUE
C
              IF(PIKCNT.EQ.0) ALENS(32,INT(W1))=0.0D0
          ELSE
C       NO ACTION TAKEN
          END IF
          RETURN
      END
C SUB ZERNREPT.FOR
      SUBROUTINE ZERNREPT
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE ZERNREPT THAT DOES THE ZERNREPT COMMAND
C
          REAL*8 HIGHORD,VCF(1:37),EMN,NV
     1    ,VC1,VC2,VC3,VC4,VC5,VC6,VC7,VC8,VC9
C
          INTEGER I
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
C
C               THIS IS A CMD LEVEL COMMAND ONLY
C
C               CHECK FOR ADDITIONAL INPUT AND
C               PRINT ERROR AND RETURN IF DISCOVERED.
C
          IF(STI.EQ.1) THEN
              WRITE(OUTLYNE,*)
     1        '"ZERNREPT" GENERATES A REPORT FOR 37-TERM FRINGE ZERNIKE'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)
     1        'SURFACES (TYPE 3 AND TYPE 10 SPECIAL SURFACES)'
              CALL SHOWIT(1)
              RETURN
          END IF
          IF(SST.EQ.1.OR.S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1
     1    .OR.SQ.EQ.1) THEN
              WRITE(OUTLYNE,*)
     1        '"ZERNREPT" ONLY TAKES NUMERIC WORD #1 INPUT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(W1.LT.0.0D0.OR.W1.GT.SYSTEM1(20)) THEN
              WRITE(OUTLYNE,*)
     1        'SURFACE NUMBER BEYOND LEGAL RANGE '
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(ALENS(34,INT(W1)).NE.3.0D0.AND.ALENS(34,INT(W1))
     1    .NE.10.0D0) THEN
              WRITE(OUTLYNE,*)
     1        'SPECIFIED SURFACE IS NOT A TYPE 3 OR TYPE 10 SPECIAL SURFACE'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'NO ZERN REPORT WILL BE GENERATED'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          WRITE(OUTLYNE,6)
 6        FORMAT('  ')
          CALL SHOWIT(0)
          WRITE(OUTLYNE,5)
 5        FORMAT('37-TERN FRINGE ZERNIKE REPORT')
          IF(ALENS(34,INT(W1)).EQ.3.0D0) WRITE(OUTLYNE,10) INT(W1)
          IF(ALENS(34,INT(W1)).EQ.10.0D0) WRITE(OUTLYNE,20) INT(W1)
 10       FORMAT('SURFACE ',I3,' IS A ZERNIKE SURFACE DEFORMATION SURFACE')
 20       FORMAT('SURFACE ',I3,' IS A ZERNIKE WAVEFRONT PHASE SURFACE')
          CALL SHOWIT(0)
          IF(ALENS(34,INT(W1)).EQ.3.0D0) WRITE(OUTLYNE,30)
          IF(ALENS(34,INT(W1)).EQ.10.0D0) WRITE(OUTLYNE,40)
 30       FORMAT('RMS SURFACE ERROR, WAVES (AS MEASURED OR FIT)')
 40       FORMAT('RMS WAVEFRONT ERROR, WAVES (AS MEASURED OR FIT)')
          CALL SHOWIT(0)
          NV=0.0D0
          EMN=2.0D0
          VCF(1)=(EMN/(2.0D0*(NV+1.0D0)))*(FTFL01(1,INT(W1))**2)
          NV=1.0D0
          EMN=1.0D0
          VCF(2)=(EMN/(2.0D0*(NV+1.0D0)))*(FTFL01(2,INT(W1))**2)
          NV=1.0D0
          EMN=1.0D0
          VCF(3)=(EMN/(2.0D0*(NV+1.0D0)))*(FTFL01(3,INT(W1))**2)
          NV=2.0D0
          EMN=2.0D0
          VCF(4)=(EMN/(2.0D0*(NV+1.0D0)))*(FTFL01(4,INT(W1))**2)
          NV=2.0D0
          EMN=1.0D0
          VCF(5)=(EMN/(2.0D0*(NV+1.0D0)))*(FTFL01(5,INT(W1))**2)
          NV=2.0D0
          EMN=1.0D0
          VCF(6)=(EMN/(2.0D0*(NV+1.0D0)))*(FTFL01(6,INT(W1))**2)
          NV=3.0D0
          EMN=1.0D0
          VCF(7)=(EMN/(2.0D0*(NV+1.0D0)))*(FTFL01(7,INT(W1))**2)
          NV=3.0D0
          EMN=1.0D0
          VCF(8)=(EMN/(2.0D0*(NV+1.0D0)))*(FTFL01(8,INT(W1))**2)
          NV=4.0D0
          EMN=2.0D0
          VCF(9)=(EMN/(2.0D0*(NV+1.0D0)))*(FTFL01(9,INT(W1))**2)
          NV=3.0D0
          EMN=1.0D0
          VCF(10)=(EMN/(2.0D0*(NV+1.0D0)))*(FTFL01(10,INT(W1))**2)
          NV=3.0D0
          EMN=1.0D0
          VCF(11)=(EMN/(2.0D0*(NV+1.0D0)))*(FTFL01(11,INT(W1))**2)
          NV=4.0D0
          EMN=1.0D0
          VCF(12)=(EMN/(2.0D0*(NV+1.0D0)))*(FTFL01(12,INT(W1))**2)
          NV=4.0D0
          EMN=1.0D0
          VCF(13)=(EMN/(2.0D0*(NV+1.0D0)))*(FTFL01(13,INT(W1))**2)
          NV=5.0D0
          EMN=1.0D0
          VCF(14)=(EMN/(2.0D0*(NV+1.0D0)))*(FTFL01(14,INT(W1))**2)
          NV=5.0D0
          EMN=1.0D0
          VCF(15)=(EMN/(2.0D0*(NV+1.0D0)))*(FTFL01(15,INT(W1))**2)
          NV=6.0D0
          EMN=2.0D0
          VCF(16)=(EMN/(2.0D0*(NV+1.0D0)))*(FTFL01(16,INT(W1))**2)
          NV=4.0D0
          EMN=1.0D0
          VCF(17)=(EMN/(2.0D0*(NV+1.0D0)))*(FTFL01(17,INT(W1))**2)
          NV=4.0D0
          EMN=1.0D0
          VCF(18)=(EMN/(2.0D0*(NV+1.0D0)))*(FTFL01(18,INT(W1))**2)
          NV=5.0D0
          EMN=1.0D0
          VCF(19)=(EMN/(2.0D0*(NV+1.0D0)))*(FTFL01(19,INT(W1))**2)
          NV=5.0D0
          EMN=1.0D0
          VCF(20)=(EMN/(2.0D0*(NV+1.0D0)))*(FTFL01(20,INT(W1))**2)
          NV=6.0D0
          EMN=1.0D0
          VCF(21)=(EMN/(2.0D0*(NV+1.0D0)))*(FTFL01(21,INT(W1))**2)
          NV=6.0D0
          EMN=1.0D0
          VCF(22)=(EMN/(2.0D0*(NV+1.0D0)))*(FTFL01(22,INT(W1))**2)
          NV=7.0D0
          EMN=1.0D0
          VCF(23)=(EMN/(2.0D0*(NV+1.0D0)))*(FTFL01(23,INT(W1))**2)
          NV=7.0D0
          EMN=1.0D0
          VCF(24)=(EMN/(2.0D0*(NV+1.0D0)))*(FTFL01(24,INT(W1))**2)
          NV=8.0D0
          EMN=1.0D0
          VCF(25)=(EMN/(2.0D0*(NV+1.0D0)))*(FTFL01(25,INT(W1))**2)
          NV=5.0D0
          EMN=1.0D0
          VCF(26)=(EMN/(2.0D0*(NV+1.0D0)))*(FTFL01(26,INT(W1))**2)
          NV=5.0D0
          EMN=1.0D0
          VCF(27)=(EMN/(2.0D0*(NV+1.0D0)))*(FTFL01(27,INT(W1))**2)
          NV=6.0D0
          EMN=1.0D0
          VCF(28)=(EMN/(2.0D0*(NV+1.0D0)))*(FTFL01(28,INT(W1))**2)
          NV=6.0D0
          EMN=1.0D0
          VCF(29)=(EMN/(2.0D0*(NV+1.0D0)))*(FTFL01(29,INT(W1))**2)
          NV=7.0D0
          EMN=1.0D0
          VCF(30)=(EMN/(2.0D0*(NV+1.0D0)))*(FTFL01(30,INT(W1))**2)
          NV=7.0D0
          EMN=1.0D0
          VCF(31)=(EMN/(2.0D0*(NV+1.0D0)))*(FTFL01(31,INT(W1))**2)
          NV=8.0D0
          EMN=1.0D0
          VCF(32)=(EMN/(2.0D0*(NV+1.0D0)))*(FTFL01(32,INT(W1))**2)
          NV=8.0D0
          EMN=1.0D0
          VCF(33)=(EMN/(2.0D0*(NV+1.0D0)))*(FTFL01(33,INT(W1))**2)
          NV=9.0D0
          EMN=1.0D0
          VCF(34)=(EMN/(2.0D0*(NV+1.0D0)))*(FTFL01(34,INT(W1))**2)
          NV=9.0D0
          EMN=1.0D0
          VCF(35)=(EMN/(2.0D0*(NV+1.0D0)))*(FTFL01(35,INT(W1))**2)
          NV=10.0D0
          EMN=2.0D0
          VCF(36)=(EMN/(2.0D0*(NV+1.0D0)))*(FTFL01(36,INT(W1))**2)
          NV=12.0D0
          EMN=2.0D0
          VCF(37)=(EMN/(2.0D0*(NV+1.0D0)))*(FTFL01(37,INT(W1))**2)
          VC1=DSQRT(VCF(1))
          VC2=DSQRT(VCF(2))
          VC3=DSQRT(VCF(3))
          VC4=DSQRT(VCF(4))
          VC5=DSQRT(VCF(5))
          VC6=DSQRT(VCF(6))
          VC7=DSQRT(VCF(7))
          VC8=DSQRT(VCF(8))
          VC9=DSQRT(VCF(9))
          WRITE(OUTLYNE,100) VC1
          CALL SHOWIT(0)
 100      FORMAT('             CONSTANT TERM = ',G20.12)
          WRITE(OUTLYNE,200) VC2
          CALL SHOWIT(0)
 200      FORMAT('          X-AXIS TILT TERM = ',G20.12)
          WRITE(OUTLYNE,300) VC3
          CALL SHOWIT(0)
 300      FORMAT('          Y-AXIS TILT TERM = ',G20.12)
          WRITE(OUTLYNE,400) VC4
          CALL SHOWIT(0)
 400      FORMAT('                FOCUS TERM = ',G20.12)
          WRITE(OUTLYNE,500) VC5
          CALL SHOWIT(0)
 500      FORMAT('   0 OR 90 DEG ASTIG. TERM = ',G20.12)
          WRITE(OUTLYNE,600) VC6
          CALL SHOWIT(0)
 600      FORMAT('     +/-45 DEG ASTIG. TERM = ',G20.12)
          WRITE(OUTLYNE,700) VC9
          CALL SHOWIT(0)
 700      FORMAT('  3RD ORDER SPHERICAL TERM = ',G20.12)
          WRITE(OUTLYNE,800) VC8
          CALL SHOWIT(0)
 800      FORMAT('3RD ORDER X-AXIS COMA TERM = ',G20.12)
          WRITE(OUTLYNE,900) VC7
          CALL SHOWIT(0)
 900      FORMAT('3RD ORDER Y-AXIS COMA TERM = ',G20.12)
          HIGHORD=0.0D0
          DO I=10,37
              HIGHORD=HIGHORD+VCF(I)
          END DO
          WRITE(OUTLYNE,1000) DSQRT(HIGHORD)
          CALL SHOWIT(0)
 1000     FORMAT('        HIGHER ORDER TERMS = ',G20.12)
          HIGHORD=0.0D0
          DO I=1,37
              HIGHORD=HIGHORD+VCF(I)
          END DO
          WRITE(OUTLYNE,1100) DSQRT(HIGHORD)
          CALL SHOWIT(0)
 1100     FORMAT('(TOTAL) ALL TERMS INCLUDED = ',G20.12)
          HIGHORD=0.0D0
          DO I=4,37
              HIGHORD=HIGHORD+VCF(I)
          END DO
          WRITE(OUTLYNE,1200) DSQRT(HIGHORD)
          CALL SHOWIT(0)
 1200     FORMAT('(TOTAL) MINUS CONST & TILT = ',G20.12)
          HIGHORD=0.0D0
          DO I=5,37
              HIGHORD=HIGHORD+VCF(I)
          END DO
          WRITE(OUTLYNE,1300)
          CALL SHOWIT(0)
          WRITE(OUTLYNE,1301) DSQRT(HIGHORD)
          CALL SHOWIT(0)
 1300     FORMAT('     (TOTAL) MINUS CONST &')
 1301     FORMAT('              TILT & FOCUS = ',G20.12)
C
          RETURN
      END
C SUB PPRSPR.FOR
      SUBROUTINE PPRSPR
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE PPRSPR WHICH IMPLEMENTS THE PRSPR
C       SPECIAL SURFACE DATA OUTPUT OPTION AT THE CMD LEVEL
C       THE ONLY VALID QUALIFIERS ARE (BLANK) AND "ALL","OB" AND "OBJ"
C       THE NUMERIC ENTRY W1, IS THE SURFACE DESIGNATOR
C       FOR PRINTOUT OF ONE SURFACE'S DATA ONLY.
C
          CHARACTER TYPE*9,TYPE2*80,UN*6,REALL*8
C
          INTEGER SPSCNT,I,JK,ITY
C
          REAL*8 COEF(1:96)
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
          IF(SYSTEM1(6).EQ.1) UN='INCH'
          IF(SYSTEM1(6).EQ.2) UN='CM'
          IF(SYSTEM1(6).EQ.3) UN='MM'
          IF(SYSTEM1(6).EQ.4) UN='METER'
C
C               THIS IS A CMD LEVEL COMMAND ONLY
C
C               CHECK FOR ADDITIONAL INPUT AND
C               PRINT ERROR AND RETURN IF DISCOVERED.
C
          IF(SST.EQ.1.OR.S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
              WRITE(OUTLYNE,*)
     1        '"PRSPR" ONLY TAKES QUALIFIER OR NUMERIC WORD #1 INPUT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SQ.EQ.1.AND.S1.EQ.1) THEN
              WRITE(OUTLYNE,*)
     1        '"PRSPR" TAKES EITHER QUALIFIER OR NUMERIC WORD #1 INPUT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)
     1        'BUT NOT BOTH'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
C       W1 DESIGNATES THE SURFACE FOR WHICH THE SPECIAL SURFACE
C       (SPSRF) DATA IS TO BE OUTPUT.
C       IF THE QUALIFIER "ALL" IS USED, THEN THE SPSRF DATA FOR
C       THE ENTIRE LENS IS PRINTED. IF THE QUALIFIER IS "OB" OR "OBJ"
C       THEN DATA FOR THE OBJECT SURFACE IS PRINTED.
C
          IF(WQ.EQ.'OB'.OR.WQ.EQ.'OBJ') THEN
C
C       THIS IS SAME AS SURFACE ZERO
              S1=1
              DF1=0
              SQ=0
              WQ='        '
              W1=0.0
          END IF
          IF(SQ.EQ.1.AND.WQ.NE.'ALL') THEN
C
C       WE HAVE INVALID QUALIFIER
              WRITE(OUTLYNE,*)'INVALID QUALIFIER INPUT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SQ.EQ.0) THEN
C       HANDEL AN INDIVIDUAL SURFACE INCLUDING "OB" AND "OBJ"
              IF(DF1.EQ.1) W1=DBLE(INT(SYSTEM1(20)))
              SURF=INT(W1)
              IF(SURF.GT.INT(SYSTEM1(20))) THEN
C       WE HAVE INVALID SURFACE #
                  WRITE(OUTLYNE,*)'SURFACE NUMBER BEYOND LEGAL RANGE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C
C       ALENS(34,SURF) CONTAINS THE FLAG FOR SPSRF
C       DATA PRESENT ON A SURFACE
C
              IF(ALENS(34,SURF).EQ.0.0) THEN
C       NO SPSRF DATA, WRITE MESSAGE AND RETURN
                  WRITE(OUTLYNE,110) SURF
                  CALL SHOWIT(0)
                  RETURN
              END IF
              TYPE2=AA//AA//AA//AA
C       THERE IS SPECIAL SURFACE DATA
              IF(DABS(ALENS(34,SURF)).EQ.1.0)
     1        TYPE2='RADIAL POLYNOMIAL SURFACE'
              IF(DABS(ALENS(34,SURF)).EQ.2.0)
     1        TYPE2='30 TERM ZERNIKE POLYNOMIAL SURFACE'
              IF(DABS(ALENS(34,SURF)).EQ.3.0)
     1        TYPE2='37 TERM FRINGE ZERNIKE POLYNOMIAL SURFACE'
              IF(DABS(ALENS(34,SURF)).EQ.4.0)
     1        TYPE2='SINUSOIDAL ERROR SURFACE'
              IF(DABS(ALENS(34,SURF)).EQ.5.0)
     1        TYPE2='USER DEFINED SURFACE #1'
              IF(DABS(ALENS(34,SURF)).EQ.6.0)
     1        TYPE2='RADIAL POLYNOMIAL PHASE SURFACE'
              IF(DABS(ALENS(34,SURF)).EQ.7.0)
     1        TYPE2='RECTANGULAR POLYNOMIAL PHASE SURFACE'
              IF(DABS(ALENS(34,SURF)).EQ.8.0)
     1        TYPE2='RECTANGULAR POLYNOMIAL SURFACE'
              IF(DABS(ALENS(34,SURF)).EQ.9.0)
     1        TYPE2='30 TERM ZERNIKE POLYNOMIAL PHASE SURFACE'
              IF(DABS(ALENS(34,SURF)).EQ.10.0)
     1        TYPE2='37 TERM ZERNIKE POLYNOMIAL PHASE SURFACE'
              IF(DABS(ALENS(34,SURF)).EQ.11.0)
     1        TYPE2='37 USER DEFINED PHASE SURFACE'
              IF(DABS(ALENS(34,SURF)).EQ.13.0D0.AND.F12.EQ.1)
     1        TYPE2='REAL RAY HOE'
              IF(DABS(ALENS(34,SURF)).EQ.12.0D0.AND.FTFL01(11,SURF).EQ.0.0D0)
     1        TYPE2='HOE WITHOUT ADDITIONAL PHASE TERMS'
              IF(DABS(ALENS(34,SURF)).EQ.12.0D0.AND.FTFL01(11,SURF).EQ.1.0D0)
     1        TYPE2='HOE WITH RADIAL POLYNOMIAL PHASE TERMS (R)'
              IF(DABS(ALENS(34,SURF)).EQ.12.0D0.AND.FTFL01(11,SURF).EQ.2.0D0)
     1        TYPE2='HOE WITH RECTANGULAR POLYNOMIAL PHASE TERMS (XY)'
              IF(DABS(ALENS(34,SURF)).EQ.12.0D0.AND.FTFL01(11,SURF).EQ.3.0D0)
     1        TYPE2='HOE WITH RECTANGULAR POLYNOMIAL PHASE TERMS (AXY)'
              IF(DABS(ALENS(34,SURF)).EQ.12.0D0.AND.FTFL01(11,SURF).EQ.4.0D0)
     1        TYPE2='HOE WITH RECTANGULAR POLYNOMIAL PHASE TERMS (XAY)'
              IF(DABS(ALENS(34,SURF)).EQ.12.0D0.AND.FTFL01(11,SURF).EQ.5.0D0)
     1        TYPE2='HOE WITH RECTANGULAR POLYNOMIAL PHASE TERMS (XYA)'
              IF(DABS(ALENS(34,SURF)).EQ.12.0D0.AND.FTFL01(11,SURF).EQ.6.0D0)
     1        TYPE2='HOE WITH RADIAL POLYNOMIAL PHASE TERMS (AR)'
              IF(DABS(ALENS(34,SURF)).EQ.12.0D0.AND.FTFL01(11,SURF).EQ.7.0D0)
     1        TYPE2='HOE WITH USER DEFINED PHASE (MACRO FUNCTION-FUN09)'
              IF(DABS(ALENS(34,SURF)).EQ.14.0)
     1        TYPE2='ABERRATION POLYNOMIAL SURFACE'
              IF(DABS(ALENS(34,SURF)).EQ.15.0)
     1        TYPE2='ABERRATION POLYNOMIAL PHASE SURFACE'
              IF(DABS(ALENS(34,SURF)).EQ.16.0.AND.ALENS(23,SURF).EQ.0.0D0)
     1        TYPE2='FRESNEL-1 SURFACE (ROTATIONALLY SYMMETRIC TYPE)'
              IF(DABS(ALENS(34,SURF)).EQ.16.0.AND.ALENS(23,SURF).EQ.1.0D0)
     1        TYPE2='FRESNEL-1 SURFACE (Y-TORIC CYLINDER TYPE)'
              IF(DABS(ALENS(34,SURF)).EQ.16.0.AND.ALENS(23,SURF).EQ.2.0D0)
     1        TYPE2='FRESNEL-1 SURFACE (X-TORIC CYLINDER TYPE)'
              IF(DABS(ALENS(34,SURF)).EQ.17.0)
     1        TYPE2='USER DEFINED SURFACE #2'
              IF(DABS(ALENS(34,SURF)).EQ.18.0)
     1        TYPE2='GRAZING INCIDENCE SURFACE'
              IF(DABS(ALENS(34,SURF)).EQ.19.0)
     1        TYPE2='GRID APODIZATION SURFACE'
              IF(DABS(ALENS(34,SURF)).EQ.20.0)
     1        TYPE2='GRID PHASE SURFACE'
              IF(DABS(ALENS(34,SURF)).EQ.21.0)
     1        TYPE2='USER DEFINED SUBROUTINE SURFACE'
              IF(DABS(ALENS(34,SURF)).EQ.22.0)
     1        TYPE2='GRID SAG SURFACE'
              IF(DABS(ALENS(34,SURF)).EQ.23.0)
     1        TYPE2='NORMAL SYMMETRIC CUBIC SPLINE SURFACE'
              ITY=INT(DABS(ALENS(34,SURF)))
              IF(ALENS(34,SURF).EQ.1.0)  TYPE='1- ON    '
              IF(ALENS(34,SURF).EQ.2.0)  TYPE='2- ON    '
              IF(ALENS(34,SURF).EQ.-1.0) TYPE='1- OFF   '
              IF(ALENS(34,SURF).EQ.-2.0) TYPE='2- OFF   '
              IF(ALENS(34,SURF).EQ.3.0)  TYPE='3- ON    '
              IF(ALENS(34,SURF).EQ.-3.0) TYPE='3- OFF   '
              IF(ALENS(34,SURF).EQ.4.0)  TYPE='4- ON    '
              IF(ALENS(34,SURF).EQ.-4.0) TYPE='4- OFF   '
              IF(ALENS(34,SURF).EQ.5.0)  TYPE='5- ON    '
              IF(ALENS(34,SURF).EQ.-5.0) TYPE='5- OFF   '
              IF(ALENS(34,SURF).EQ.6.0)  TYPE='6- ON    '
              IF(ALENS(34,SURF).EQ.-6.0) TYPE='6- OFF   '
              IF(ALENS(34,SURF).EQ.7.0)  TYPE='7- ON    '
              IF(ALENS(34,SURF).EQ.-7.0) TYPE='7- OFF   '
              IF(ALENS(34,SURF).EQ.8.0)  TYPE='8- ON    '
              IF(ALENS(34,SURF).EQ.-8.0) TYPE='8- OFF   '
              IF(ALENS(34,SURF).EQ.9.0)  TYPE='9- ON    '
              IF(ALENS(34,SURF).EQ.-9.0) TYPE='9- OFF   '
              IF(ALENS(34,SURF).EQ.10.0)  TYPE='10- ON   '
              IF(ALENS(34,SURF).EQ.-10.0) TYPE='10- OFF  '
              IF(ALENS(34,SURF).EQ.11.0)  TYPE='11- ON   '
              IF(ALENS(34,SURF).EQ.-11.0) TYPE='11- OFF  '
              IF(ALENS(34,SURF).EQ.12.0)  TYPE='12- ON   '
              IF(ALENS(34,SURF).EQ.-12.0) TYPE='12- OFF  '
              IF(ALENS(34,SURF).EQ.13.0)  TYPE='13- ON   '
              IF(ALENS(34,SURF).EQ.-13.0) TYPE='13- OFF  '
              IF(ALENS(34,SURF).EQ.14.0)  TYPE='14- ON   '
              IF(ALENS(34,SURF).EQ.-14.0) TYPE='14- OFF  '
              IF(ALENS(34,SURF).EQ.15.0)  TYPE='15- ON   '
              IF(ALENS(34,SURF).EQ.-15.0) TYPE='15- OFF  '
              IF(ALENS(34,SURF).EQ.16.0)  TYPE='16- ON   '
              IF(ALENS(34,SURF).EQ.-16.0) TYPE='16- OFF  '
              IF(ALENS(34,SURF).EQ.17.0)  TYPE='17- ON   '
              IF(ALENS(34,SURF).EQ.-17.0) TYPE='17- OFF  '
              IF(ALENS(34,SURF).EQ.18.0)  TYPE='18- ON   '
              IF(ALENS(34,SURF).EQ.-18.0) TYPE='18- OFF  '
              IF(ALENS(34,SURF).EQ.19.0)  TYPE='19- ON   '
              IF(ALENS(34,SURF).EQ.-19.0) TYPE='19- OFF  '
              IF(ALENS(34,SURF).EQ.20.0)  TYPE='20- ON   '
              IF(ALENS(34,SURF).EQ.-20.0) TYPE='20- OFF  '
              IF(ALENS(34,SURF).EQ.21.0)  TYPE='21- ON   '
              IF(ALENS(34,SURF).EQ.-21.0) TYPE='21- OFF  '
              IF(ALENS(34,SURF).EQ.22.0)  TYPE='22- ON   '
              IF(ALENS(34,SURF).EQ.-22.0) TYPE='22- OFF  '
              IF(ALENS(34,SURF).EQ.23.0)  TYPE='23- ON   '
              IF(ALENS(34,SURF).EQ.-23.0) TYPE='23- OFF  '
              IF(ALENS(34,SURF).EQ.24.0)  TYPE='24- ON   '
              IF(ALENS(34,SURF).EQ.-24.0) TYPE='24- OFF  '
              IF(DABS(ALENS(34,SURF)).GT.24.0) THEN
                  WRITE(OUTLYNE,*)'SPECIAL SURFACE TYPE NOT RECOGNIZED'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'VALID TYPES RANGE FROM 1 TO 24'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C        SET PRINTING ARRAY COEF TO PROPER VALUES
C
              DO 9000 I=1,96
                  IF(DABS(ALENS(34,SURF)).GE.1.0.AND.
     1            DABS(ALENS(34,SURF)).LE.30.0) THEN
                      COEF(I)=FTFL01(I,SURF)
                  END IF
 9000         CONTINUE
              IF(DABS(ALENS(34,SURF)).NE.13.0D0.OR.
     1        DABS(ALENS(34,SURF)).EQ.13.0D0.AND.F12.EQ.1) THEN
                  WRITE(OUTLYNE,101) SURF,TYPE
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,102) TYPE2(1:79)
                  CALL SHOWIT(0)
              END IF
              IF(ITY.EQ.1.OR.ITY.EQ.6) THEN
                  WRITE(OUTLYNE,400) COEF(9),COEF(10),COEF(11),
     1            COEF(12)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,500) COEF(13),COEF(14),COEF(15),
     1            COEF(16)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,600) COEF(17),COEF(18),COEF(19),
     1            COEF(20)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,700) COEF(21),COEF(22),COEF(23),
     1            COEF(24)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,701) COEF(25),COEF(26),COEF(27),
     1            COEF(28)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,702) COEF(29),COEF(30),COEF(31),
     1            COEF(32)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,703) COEF(33),COEF(34),COEF(35),
     1            COEF(36)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,7041) COEF(37),COEF(38),COEF(39)
                  CALL SHOWIT(0)
              END IF
              IF(ITY.EQ.2.OR.ITY.EQ.9) THEN
                  WRITE(OUTLYNE,200) COEF(1),COEF(2),COEF(3),
     1            COEF(4)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,300) COEF(5),COEF(6),COEF(7),
     1            COEF(8)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,400) COEF(9),COEF(10),COEF(11),
     1            COEF(12)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,500) COEF(13),COEF(14),COEF(15),
     1            COEF(16)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,600) COEF(17),COEF(18),COEF(19),
     1            COEF(20)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,700) COEF(21),COEF(22),COEF(23),
     1            COEF(24)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,701) COEF(25),COEF(26),COEF(27),
     1            COEF(28)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,702) COEF(29),COEF(30),COEF(31),
     1            COEF(32)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,703) COEF(33),COEF(34),COEF(35),
     1            COEF(36)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,704) COEF(37),COEF(38),COEF(39),
     1            COEF(40)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,705) COEF(41),COEF(42),COEF(43),
     1            COEF(44)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,706) COEF(45),COEF(46),COEF(47),
     1            COEF(48)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,707) COEF(49),COEF(50),COEF(51),
     1            COEF(52)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,708) COEF(53),COEF(54),COEF(55),
     1            COEF(56)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,709) COEF(57),COEF(58),COEF(59),
     1            COEF(60)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,710) COEF(61),COEF(62),COEF(63),
     1            COEF(64)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,7004) COEF(65),COEF(66)
                  CALL SHOWIT(0)
              END IF
              IF(ITY.EQ.3.OR.ITY.EQ.10) THEN

                  WRITE(OUTLYNE,200) COEF(1),COEF(2),COEF(3),
     1            COEF(4)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,300) COEF(5),COEF(6),COEF(7),
     1            COEF(8)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,400) COEF(9),COEF(10),COEF(11),
     1            COEF(12)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,500) COEF(13),COEF(14),COEF(15),
     1            COEF(16)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,600) COEF(17),COEF(18),COEF(19),
     1            COEF(20)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,700) COEF(21),COEF(22),COEF(23),
     1            COEF(24)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,701) COEF(25),COEF(26),COEF(27),
     1            COEF(28)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,702) COEF(29),COEF(30),COEF(31),
     1            COEF(32)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,703) COEF(33),COEF(34),COEF(35),
     1            COEF(36)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,7042) COEF(37)
                  CALL SHOWIT(0)
              END IF
              IF(ITY.EQ.4) THEN
                  WRITE(OUTLYNE,31001) COEF(1)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,31002) COEF(2),UN
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,31003) COEF(3),UN
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,32001) COEF(4)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,32002) COEF(5),UN
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,32003) COEF(6),UN
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,33001) COEF(7)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,33002) COEF(8),UN
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,33003) COEF(9),UN
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,34001) COEF(10)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,34002) COEF(11),UN
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,34003) COEF(12),UN
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,35001) COEF(13)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,35002) COEF(14),UN
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,35003) COEF(15),UN
                  CALL SHOWIT(0)
              END IF
              IF(ITY.EQ.18) THEN

                  WRITE(OUTLYNE,5001) COEF(1),UN
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,5002) COEF(2),UN
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,5003) COEF(3),UN
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,5004) COEF(4),COEF(5),COEF(6),COEF(7)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,5005) COEF(8),COEF(9),COEF(10),COEF(11)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,5006) COEF(12),COEF(13),COEF(14),COEF(15)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,5007) COEF(16),COEF(17),COEF(18)
                  CALL SHOWIT(0)
              END IF
              IF(ITY.EQ.19) THEN

                  IF(COEF(1).NE.0.0D0)WRITE(OUTLYNE,8001) INT(COEF(1))
                  IF(COEF(1).EQ.0.0D0)WRITE(OUTLYNE,8006)
 8001             FORMAT('GRID SOURCE FILE # (APGRIDxx.DAT), xx = ',I2)
 8006             FORMAT('NO APODIZATION SOURCE GRID FILE HAS BEEN SPECIFIED')
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,8002) INT(COEF(2))
 8002             FORMAT('APODIZATION GRID DIMENSION NxN, N = ',I5)
                  CALL SHOWIT(0)

                  COEF(3)=FTFL01(3,SURF)
                  WRITE(OUTLYNE,8003) COEF(3)
 8003             FORMAT('GRID CLEAR APERTURE(RADIUS) = ',G15.8,1X,A6)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,9030) COEF(4)
 9030             FORMAT('APODIZATION SCALE FACTOR = ',G15.8)
                  CALL SHOWIT(0)

              END IF
              IF(ITY.EQ.22) THEN

                  IF(COEF(1).NE.0.0D0)WRITE(OUTLYNE,8101) INT(COEF(1))
                  IF(COEF(1).EQ.0.0D0)WRITE(OUTLYNE,8106)
 8101             FORMAT('GRID SAG FILE # (SGGRIDxx.DAT), xx = ',I2)
 8106             FORMAT('NO SAG SOURCE GRID FILE HAS BEEN SPECIFIED')
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,8102) INT(COEF(2))
 8102             FORMAT('SAG GRID DIMENSION NxN, N = ',I5)
                  CALL SHOWIT(0)

                  COEF(3)=FTFL01(3,SURF)
                  WRITE(OUTLYNE,8103) COEF(3)
 8103             FORMAT('GRID CLEAR APERTURE(RADIUS) = ',G15.8,1X,A6)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,9050) COEF(4)
 9050             FORMAT('SAG SCALE FACTOR = ',G15.8)
                  CALL SHOWIT(0)
              END IF
              IF(ITY.EQ.23) THEN
C     NORMAL CUBIC SPLINE

                  WRITE(OUTLYNE,200) COEF(1),COEF(2),COEF(3),COEF(4)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,300) COEF(5),COEF(6),COEF(7),COEF(8)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,400) COEF(9),COEF(10),COEF(11),COEF(12)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,500) COEF(13),COEF(14),COEF(15),COEF(16)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,600) COEF(17),COEF(18),COEF(19),COEF(20)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,700) COEF(21),COEF(22),COEF(23),COEF(24)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,701) COEF(25),COEF(26),COEF(27),COEF(28)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,702) COEF(29),COEF(30),COEF(31),COEF(32)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,703) COEF(33),COEF(34),COEF(35),COEF(36)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,704) COEF(37),COEF(38),COEF(39),COEF(40)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,705) COEF(41),COEF(42),COEF(43),COEF(44)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,706) COEF(45),COEF(46),COEF(47),COEF(48)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,707) COEF(49),COEF(50),COEF(51),COEF(52)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,708) COEF(53),COEF(54),COEF(55),COEF(56)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,709) COEF(57),COEF(58),COEF(59),COEF(60)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,710) COEF(61),COEF(62),COEF(63),COEF(64)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,711) COEF(65),COEF(66),COEF(67),COEF(68)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,712) COEF(69),COEF(70),COEF(71),COEF(72)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,713) COEF(73),COEF(74),COEF(75),COEF(76)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,714) COEF(77),COEF(78),COEF(79),COEF(80)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,715) COEF(81),COEF(82),COEF(83),COEF(84)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,716) COEF(84),COEF(85),COEF(86),COEF(87)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,717) COEF(89),COEF(90),COEF(91),COEF(92)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,718) COEF(93),COEF(94),COEF(95),COEF(96)
                  CALL SHOWIT(0)
              END IF
C
              IF(ITY.EQ.20) THEN

                  IF(COEF(1).NE.0.0D0)WRITE(OUTLYNE,9001) INT(COEF(1))
                  IF(COEF(1).EQ.0.0D0)WRITE(OUTLYNE,9006)
 9001             FORMAT('GRID SOURCE FILE # (PHGRIDxx.DAT), xx = ',I2)
 9006             FORMAT('NO PHASE SOURCE GRID FILE HAS BEEN SPECIFIED')
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,9002) INT(COEF(2))
 9002             FORMAT('PHASE GRID DIMENSION NxN, N = ',I5)
                  CALL SHOWIT(0)

                  COEF(3)=FTFL01(3,SURF)
                  WRITE(OUTLYNE,9003) COEF(3),UN
 9003             FORMAT('GRID CLEAR APERTURE(RADIUS) = ',G15.8,1X,A6)
                  CALL SHOWIT(0)

                  IF(COEF(4).EQ.0.0D0)WRITE(OUTLYNE,9007)
                  IF(COEF(4).EQ.1.0D0)WRITE(OUTLYNE,9008) UN
                  IF(COEF(4).EQ.2.0D0)WRITE(OUTLYNE,9009)
                  IF(COEF(4).GT.3.0D0)WRITE(OUTLYNE,9010)
 9007             FORMAT
     1            ('INPUT PHASE(OPD) IN WAVES AT THE CURRENT REF. WAVELENGTH')
 9008             FORMAT
     1            ('INPUT PHASE(OPD) UNITS ARE CURRENT LENS UNITS = ',A6)
 9009             FORMAT
     1            ('INPUT PHASE(OPD) UNITS ARE MICROMETER')
 9010             FORMAT
     1            ('INVALID UNITS CODE FOUND IN TYPE 20 SPECIAL SURFACE')
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,9040) COEF(5)
 9040             FORMAT('PHASE SCALE FACTOR = ',G15.8)
                  CALL SHOWIT(0)

              END IF
              IF(ITY.EQ.5.OR.ITY.EQ.11) THEN

                  WRITE(OUTLYNE,200) COEF(1),COEF(2),COEF(3),COEF(4)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,300) COEF(5),COEF(6),COEF(7),COEF(8)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,400) COEF(9),COEF(10),COEF(11),COEF(12)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,500) COEF(13),COEF(14),COEF(15),COEF(16)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,600) COEF(17),COEF(18),COEF(19),COEF(20)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,700) COEF(21),COEF(22),COEF(23),COEF(24)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,701) COEF(25),COEF(26),COEF(27),COEF(28)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,702) COEF(29),COEF(30),COEF(31),COEF(32)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,703) COEF(33),COEF(34),COEF(35),COEF(36)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,704) COEF(37),COEF(38),COEF(39),COEF(40)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,705) COEF(41),COEF(42),COEF(43),COEF(44)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,706) COEF(45),COEF(46),COEF(47),COEF(48)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,707) COEF(49),COEF(50),COEF(51),COEF(52)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,708) COEF(53),COEF(54),COEF(55),COEF(56)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,709) COEF(57),COEF(58),COEF(59),COEF(60)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,710) COEF(61),COEF(62),COEF(63),COEF(64)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,711) COEF(65),COEF(66),COEF(67),COEF(68)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,712) COEF(69),COEF(70),COEF(71),COEF(72)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,713) COEF(73),COEF(74),COEF(75),COEF(76)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,714) COEF(77),COEF(78),COEF(79),COEF(80)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,715) COEF(81),COEF(82),COEF(83),COEF(84)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,716) COEF(84),COEF(85),COEF(86),COEF(87)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,717) COEF(89),COEF(90),COEF(91),COEF(92)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,718) COEF(93),COEF(94),COEF(95),COEF(96)
                  CALL SHOWIT(0)
              END IF
              IF(ITY.EQ.21) THEN

                  WRITE(OUTLYNE,200) COEF(1),COEF(2),COEF(3),
     1            COEF(4)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,300) COEF(5),COEF(6),COEF(7),
     1            COEF(8)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,400) COEF(9),COEF(10),COEF(11),
     1            COEF(12)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,500) COEF(13),COEF(14),COEF(15),
     1            COEF(16)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,600) COEF(17),COEF(18),COEF(19),
     1            COEF(20)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,700) COEF(21),COEF(22),COEF(23),
     1            COEF(24)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,701) COEF(25),COEF(26),COEF(27),
     1            COEF(28)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,702) COEF(29),COEF(30),COEF(31),
     1            COEF(32)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,703) COEF(33),COEF(34),COEF(35),
     1            COEF(36)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,704) COEF(37),COEF(38),COEF(39),
     1            COEF(40)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,705) COEF(41),COEF(42),COEF(43),
     1            COEF(44)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,706) COEF(45),COEF(46),COEF(47),
     1            COEF(48)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,707) COEF(49),COEF(50),COEF(51),
     1            COEF(52)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,708) COEF(53),COEF(54),COEF(55),
     1            COEF(56)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,709) COEF(57),COEF(58),COEF(59),
     1            COEF(60)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,710) COEF(61),COEF(62),COEF(63),
     1            COEF(64)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,711) COEF(65),COEF(66),COEF(67),
     1            COEF(68)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,712) COEF(69),COEF(70),COEF(71),
     1            COEF(72)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,713) COEF(73),COEF(74),COEF(75),
     1            COEF(76)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,714) COEF(77),COEF(78),COEF(79),
     1            COEF(80)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,715) COEF(81),COEF(82),COEF(83),
     1            COEF(84)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,716) COEF(84),COEF(85),COEF(86),
     1            COEF(87)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,717) COEF(89),COEF(90),COEF(91),
     1            COEF(92)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,718) COEF(93),COEF(94),COEF(95),
     1            COEF(96)
                  CALL SHOWIT(0)
              END IF
              IF(ITY.EQ.14.OR.ITY.EQ.15) THEN

                  WRITE(OUTLYNE,200) COEF(1),COEF(2),COEF(3),
     1            COEF(4)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,300) COEF(5),COEF(6),COEF(7),
     1            COEF(8)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,400) COEF(9),COEF(10),COEF(11),
     1            COEF(12)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,500) COEF(13),COEF(14),COEF(15),
     1            COEF(16)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,600) COEF(17),COEF(18),COEF(19),
     1            COEF(20)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,700) COEF(21),COEF(22),COEF(23),
     1            COEF(24)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,701) COEF(25),COEF(26),COEF(27),
     1            COEF(28)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,702) COEF(29),COEF(30),COEF(31),
     1            COEF(32)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,703) COEF(33),COEF(34),COEF(35),
     1            COEF(36)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,704) COEF(37),COEF(38),COEF(39),
     1            COEF(40)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,705) COEF(41),COEF(42),COEF(43),
     1            COEF(44)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,706) COEF(45),COEF(46),COEF(47),
     1            COEF(48)
                  CALL SHOWIT(0)
              END IF
              IF(ITY.EQ.16) THEN

                  WRITE(OUTLYNE,200) COEF(1),COEF(2),COEF(3),
     1            COEF(4)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,300) COEF(5),COEF(6),COEF(7),
     1            COEF(8)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,401) COEF(9),COEF(10),COEF(11)
                  CALL SHOWIT(0)

              END IF
              IF(ITY.EQ.7.OR.ITY.EQ.8) THEN

                  WRITE(OUTLYNE,200) COEF(1),COEF(2),COEF(3),
     1            COEF(4)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,300) COEF(5),COEF(6),COEF(7),
     1            COEF(8)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,400) COEF(9),COEF(10),COEF(11),
     1            COEF(12)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,500) COEF(13),COEF(14),COEF(15),
     1            COEF(16)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,600) COEF(17),COEF(18),COEF(19),
     1            COEF(20)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,700) COEF(21),COEF(22),COEF(23),
     1            COEF(24)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,701) COEF(25),COEF(26),COEF(27),
     1            COEF(28)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,702) COEF(29),COEF(30),COEF(31),
     1            COEF(32)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,703) COEF(33),COEF(34),COEF(35),
     1            COEF(36)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,704) COEF(37),COEF(38),COEF(39),
     1            COEF(40)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,705) COEF(41),COEF(42),COEF(43),
     1            COEF(44)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,706) COEF(45),COEF(46),COEF(47),
     1            COEF(48)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,707) COEF(49),COEF(50),COEF(51),
     1            COEF(52)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,708) COEF(53),COEF(54),COEF(55),
     1            COEF(56)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,709) COEF(57),COEF(58),COEF(59),
     1            COEF(60)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,710) COEF(61),COEF(62),COEF(63),
     1            COEF(64)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,711) COEF(65),COEF(66),COEF(67),
     1            COEF(68)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,712) COEF(69),COEF(70),COEF(71),
     1            COEF(72)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,713) COEF(73),COEF(74),COEF(75),
     1            COEF(76)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,714) COEF(77),COEF(78),COEF(79),
     1            COEF(80)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,715) COEF(81),COEF(82),COEF(83),
     1            COEF(84)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,716) COEF(84),COEF(85),COEF(86),
     1            COEF(87)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,7171) COEF(89),COEF(90),COEF(91)
                  CALL SHOWIT(0)
              END IF
              IF(ITY.EQ.13.AND.F12.EQ.1) THEN

                  WRITE(OUTLYNE,2201) COEF(1)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,2202) COEF(2)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,2203) COEF(3),UN
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,2204) COEF(4),UN
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,2205) COEF(5),UN
                  CALL SHOWIT(0)

                  IF(COEF(6).GT.0.0D0) REALL='POS.    '
                  IF(COEF(6).LT.0.0D0) REALL='NEG.    '
                  WRITE(OUTLYNE,2206) INT(COEF(6)),REALL
                  CALL SHOWIT(0)
                  IF(COEF(7).GT.0.0D0) REALL='REAL    '
                  IF(COEF(7).LT.0.0D0) REALL='VIRTUAL '
                  IF(COEF(7).GT.0.0D0) COEF(7)=1.0D0
                  IF(COEF(7).LT.0.0D0) COEF(7)=-1.0D0
                  WRITE(OUTLYNE,2207) INT(COEF(7)),REALL
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,2208) COEF(8),UN
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,2209) COEF(9),UN
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,2210) COEF(10),UN
                  CALL SHOWIT(0)

                  IF(COEF(11).GT.0.0D0) REALL='POS.   '
                  IF(COEF(11).LT.0.0D0) REALL='NEG.   '
                  WRITE(OUTLYNE,2211) INT(COEF(11)),REALL
                  CALL SHOWIT(0)
                  IF(COEF(12).GT.0.0D0) REALL='REAL   '
                  IF(COEF(12).LT.0.0D0) REALL='VIRTUAL'
                  IF(COEF(12).GT.0.0D0) COEF(12)=1.0D0
                  IF(COEF(12).LT.0.0D0) COEF(12)=-1.0D0
                  WRITE(OUTLYNE,2212) INT(COEF(12)),REALL
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,2213) INT(COEF(13))
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,2214) INT(COEF(14))
                  CALL SHOWIT(0)
              END IF
              IF(ITY.EQ.12.AND.COEF(11).EQ.0.0D0) THEN

                  WRITE(OUTLYNE,2001) COEF(1)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,2002) COEF(2)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,2003) COEF(3),UN
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,2004) COEF(4),UN
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,2005) COEF(5),UN
                  CALL SHOWIT(0)

                  IF(COEF(6).GT.0.0D0) REALL='REAL    '
                  IF(COEF(6).LT.0.0D0) REALL='VIRTUAL '
                  IF(COEF(6).GT.0.0D0) COEF(6)=1.0D0
                  IF(COEF(6).LT.0.0D0) COEF(6)=-1.0D0
                  WRITE(OUTLYNE,2006) INT(COEF(6)),REALL
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,2007) COEF(7),UN
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,2008) COEF(8),UN
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,2009) COEF(9),UN
                  CALL SHOWIT(0)

                  IF(COEF(10).GT.0.0D0) REALL='REAL   '
                  IF(COEF(10).LT.0.0D0) REALL='VIRTUAL'
                  IF(COEF(10).GT.0.0D0) COEF(10)=1.0D0
                  IF(COEF(10).LT.0.0D0) COEF(10)=-1.0D0
                  WRITE(OUTLYNE,2010) INT(COEF(10)),REALL
                  CALL SHOWIT(0)
              END IF
              IF(ITY.EQ.12.AND.COEF(11).EQ.6.0D0) THEN

                  WRITE(OUTLYNE,2001) COEF(1)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,2002) COEF(2)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,2003) COEF(3),UN
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,2004) COEF(4),UN
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,2005) COEF(5),UN
                  CALL SHOWIT(0)

                  IF(COEF(6).GT.0.0D0) REALL='REAL    '
                  IF(COEF(6).LT.0.0D0) REALL='VIRTUAL '
                  IF(COEF(6).GT.0.0D0) COEF(6)=1.0D0
                  IF(COEF(6).LT.0.0D0) COEF(6)=-1.0D0
                  WRITE(OUTLYNE,2006) INT(COEF(6)),REALL
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,2007) COEF(7),UN
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,2008) COEF(8),UN
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,2009) COEF(9),UN
                  CALL SHOWIT(0)

                  IF(COEF(10).GT.0.0D0) REALL='REAL   '
                  IF(COEF(10).LT.0.0D0) REALL='VIRTUAL'
                  IF(COEF(10).GT.0.0D0) COEF(10)=1.0D0
                  IF(COEF(10).LT.0.0D0) COEF(10)=-1.0D0
                  WRITE(OUTLYNE,2010) INT(COEF(10)),REALL
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,4001) COEF(11),COEF(12)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,500) COEF(13),COEF(14),COEF(15),
     1            COEF(16)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,600) COEF(17),COEF(18),COEF(19),
     1            COEF(20)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,7001) COEF(21),COEF(22),COEF(23)
     1            ,COEF(24)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,7002) COEF(25),COEF(26),COEF(27)
     1            ,COEF(28)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,7003) COEF(29),COEF(30),COEF(31)
                  CALL SHOWIT(0)
              END IF
              IF(ITY.EQ.12.AND.COEF(11).EQ.1.0D0) THEN

                  WRITE(OUTLYNE,2001) COEF(1)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,2002) COEF(2)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,2003) COEF(3),UN
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,2004) COEF(4),UN
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,2005) COEF(5),UN
                  CALL SHOWIT(0)

                  IF(COEF(6).GT.0.0D0) REALL='REAL    '
                  IF(COEF(6).LT.0.0D0) REALL='VIRTUAL '
                  IF(COEF(6).GT.0.0D0) COEF(6)=1.0D0
                  IF(COEF(6).LT.0.0D0) COEF(6)=-1.0D0
                  WRITE(OUTLYNE,2006) INT(COEF(6)),REALL
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,2007) COEF(7),UN
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,2008) COEF(8),UN
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,2009) COEF(9),UN
                  CALL SHOWIT(0)

                  IF(COEF(10).GT.0.0D0) REALL='REAL   '
                  IF(COEF(10).LT.0.0D0) REALL='VIRTUAL'
                  IF(COEF(10).GT.0.0D0) COEF(10)=1.0D0
                  IF(COEF(10).LT.0.0D0) COEF(10)=-1.0D0
                  WRITE(OUTLYNE,2010) INT(COEF(10)),REALL
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,4001) COEF(11),COEF(12)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,500) COEF(13),COEF(14),COEF(15),
     1            COEF(16)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,600) COEF(17),COEF(18),COEF(19),
     1            COEF(20)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,7005) COEF(21)
                  CALL SHOWIT(0)
              END IF
              IF(ITY.EQ.12.AND.COEF(11).EQ.7.0D0) THEN

                  WRITE(OUTLYNE,2001) COEF(1)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,2002) COEF(2)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,2003) COEF(3),UN
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,2004) COEF(4),UN
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,2005) COEF(5),UN
                  CALL SHOWIT(0)

                  IF(COEF(6).GT.0.0D0) REALL='REAL    '
                  IF(COEF(6).LT.0.0D0) REALL='VIRTUAL '
                  IF(COEF(6).GT.0.0D0) COEF(6)=1.0D0
                  IF(COEF(6).LT.0.0D0) COEF(6)=-1.0D0
                  WRITE(OUTLYNE,2006) INT(COEF(6)),REALL
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,2007) COEF(7),UN
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,2008) COEF(8),UN
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,2009) COEF(9),UN
                  CALL SHOWIT(0)

                  IF(COEF(10).GT.0.0D0) REALL='REAL   '
                  IF(COEF(10).LT.0.0D0) REALL='VIRTUAL'
                  IF(COEF(10).GT.0.0D0) COEF(10)=1.0D0
                  IF(COEF(10).LT.0.0D0) COEF(10)=-1.0D0
                  WRITE(OUTLYNE,2010) INT(COEF(10)),REALL
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,4001) COEF(11),COEF(12)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,500) COEF(13),COEF(14),COEF(15),
     1            COEF(16)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,600) COEF(17),COEF(18),COEF(19),
     1            COEF(20)
                  CALL SHOWIT(0)
                  
                  WRITE(OUTLYNE,700) COEF(21),COEF(22),COEF(23),
     1            COEF(24)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,701) COEF(25),COEF(26),COEF(27),
     1            COEF(28)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,702) COEF(29),COEF(30),COEF(31),
     1            COEF(32)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,703) COEF(33),COEF(34),COEF(35),
     1            COEF(36)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,704) COEF(37),COEF(38),COEF(39),
     1            COEF(40)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,705) COEF(41),COEF(42),COEF(43),
     1            COEF(44)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,706) COEF(45),COEF(46),COEF(47),
     1            COEF(48)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,707) COEF(49),COEF(50),COEF(51),
     1            COEF(52)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,708) COEF(53),COEF(54),COEF(55),
     1            COEF(56)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,709) COEF(57),COEF(58),COEF(59),
     1            COEF(60)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,710) COEF(61),COEF(62),COEF(63),
     1            COEF(64)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,711) COEF(65),COEF(66),COEF(67),
     1            COEF(68)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,712) COEF(69),COEF(70),COEF(71),
     1            COEF(72)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,713) COEF(73),COEF(74),COEF(75),
     1            COEF(76)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,714) COEF(77),COEF(78),COEF(79),
     1            COEF(80)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,715) COEF(81),COEF(82),COEF(83),
     1            COEF(84)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,716) COEF(84),COEF(85),COEF(86),
     1            COEF(87)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,717) COEF(89),COEF(90),COEF(91),
     1            COEF(92)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,718) COEF(93),COEF(94),COEF(95),
     1            COEF(96)
                  CALL SHOWIT(0)
              END IF
              IF(ITY.EQ.12.AND.COEF(11).EQ.2.0D0.OR.
     1        ITY.EQ.12.AND.COEF(11).EQ.3.0D0.OR.
     1        ITY.EQ.12.AND.COEF(11).EQ.4.0D0.OR.
     1        ITY.EQ.12.AND.COEF(11).EQ.5.0D0) THEN

                  WRITE(OUTLYNE,2001) COEF(1)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,2002) COEF(2)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,2003) COEF(3),UN
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,2004) COEF(4),UN
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,2005) COEF(5),UN
                  CALL SHOWIT(0)

                  IF(COEF(6).GT.0.0D0) REALL='REAL    '
                  IF(COEF(6).LT.0.0D0) REALL='VIRTUAL '
                  IF(COEF(6).GT.0.0D0) COEF(6)=1.0D0
                  IF(COEF(6).LT.0.0D0) COEF(6)=-1.0D0
                  WRITE(OUTLYNE,2006) INT(COEF(6)),REALL
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,2007) COEF(7),UN
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,2008) COEF(8),UN
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,2009) COEF(9),UN
                  CALL SHOWIT(0)

                  IF(COEF(10).GT.0.0D0) REALL='REAL   '
                  IF(COEF(10).LT.0.0D0) REALL='VIRTUAL'
                  IF(COEF(10).GT.0.0D0) COEF(10)=1.0D0
                  IF(COEF(10).LT.0.0D0) COEF(10)=-1.0D0
                  WRITE(OUTLYNE,2010) INT(COEF(10)),REALL
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,4001) COEF(11),COEF(12)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,500) COEF(13),COEF(14),COEF(15),
     1            COEF(16)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,600) COEF(17),COEF(18),COEF(19),
     1            COEF(20)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,700) COEF(21),COEF(22),COEF(23),COEF(24)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,701) COEF(25),COEF(26),COEF(27),
     1            COEF(28)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,702) COEF(29),COEF(30),COEF(31),
     1            COEF(32)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,703) COEF(33),COEF(34),COEF(35),
     1            COEF(36)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,704) COEF(37),COEF(38),COEF(39),
     1            COEF(40)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,705) COEF(41),COEF(42),COEF(43),
     1            COEF(44)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,706) COEF(45),COEF(46),COEF(47),
     1            COEF(48)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,707) COEF(49),COEF(50),COEF(51),
     1            COEF(52)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,708) COEF(53),COEF(54),COEF(55),
     1            COEF(56)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,709) COEF(57),COEF(58),COEF(59),
     1            COEF(60)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,710) COEF(61),COEF(62),COEF(63),
     1            COEF(64)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,711) COEF(65),COEF(66),COEF(67),
     1            COEF(68)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,712) COEF(69),COEF(70),COEF(71),
     1            COEF(72)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,713) COEF(73),COEF(74),COEF(75),
     1            COEF(76)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,7141) COEF(77),COEF(78),COEF(79)
                  CALL SHOWIT(0)
              END IF
              RETURN
          ELSE
C*********************************************************************
C       SQ=1 AND ONLY ALLOWED WQ BY NOW IS "ALL"
C       HANDEL THE WHOLE LENS
              SPSCNT=0
C
              DO 16 JK=0,INT(SYSTEM1(20))
                  IF(ALENS(34,JK).NE.0.0) THEN
                      SPSCNT=SPSCNT+1
                  END IF
 16           CONTINUE
              IF(SPSCNT.EQ.0) THEN
                  WRITE(OUTLYNE,100)
                  CALL SHOWIT(0)
                  RETURN
              END IF
              DO JK=0,INT(SYSTEM1(20))
                  IF(DABS(ALENS(34,JK)).EQ.13.0D0.AND.F12.EQ.1.OR.
     1            DABS(ALENS(34,JK)).NE.13.0D0.AND.ALENS(34,JK).NE.0.0D0) THEN
                      WRITE(OUTLYNE,1000)
                      CALL SHOWIT(0)
                  END IF
              END DO
              DO 15 JK=0,INT(SYSTEM1(20))
C
C       SET SPECIAL SURFACE TYPE
C
                  IF(ALENS(34,JK).EQ.0.0) THEN
C       NOT A SPECIAL SURFACE TYPE. JUMP TO 15 AND
C       GO TO THE NEXT SURFACE NUMBER
                      GO TO 15
                  END IF
                  TYPE2=AA//AA//AA//AA
C       THERE IS SPECIAL SURFACE DATA
                  IF(DABS(ALENS(34,JK)).EQ.1.0)
     1            TYPE2='RADIAL POLYNOMIAL SURFACE'
                  IF(DABS(ALENS(34,JK)).EQ.2.0)
     1            TYPE2='30 TERM ZERNIKE POLYNOMIAL SURFACE'
                  IF(DABS(ALENS(34,JK)).EQ.3.0)
     1            TYPE2='37 TERM FRINGE ZERNIKE POLYNOMIAL SURFACE'
                  IF(DABS(ALENS(34,JK)).EQ.4.0)
     1            TYPE2='SINUSOIDAL ERROR SURFACE'
                  IF(DABS(ALENS(34,JK)).EQ.5.0)
     1            TYPE2='USER DEFINED SURFACE #1'
                  IF(DABS(ALENS(34,JK)).EQ.6.0)
     1            TYPE2='RADIAL POLYNOMIAL PHASE SURFACE'
                  IF(DABS(ALENS(34,JK)).EQ.7.0)
     1            TYPE2='RECTANGULAR POLYNOMIAL PHASE SURFACE'
                  IF(DABS(ALENS(34,JK)).EQ.8.0)
     1            TYPE2='RECTANGULAR POLYNOMIAL SURFACE'
                  IF(DABS(ALENS(34,JK)).EQ.9.0)
     1            TYPE2='30 TERM ZERNIKE POLYNOMIAL PHASE SURFACE'
                  IF(DABS(ALENS(34,JK)).EQ.10.0)
     1            TYPE2='37 TERM ZERNIKE POLYNOMIAL PHASE SURFACE'
                  IF(DABS(ALENS(34,JK)).EQ.11.0)
     1            TYPE2='37 USER DEFINED PHASE SURFACE'
                  IF(DABS(ALENS(34,SURF)).EQ.13.0D0.AND.F12.EQ.1)
     1            TYPE2='REAL RAY HOE'
                  IF(DABS(ALENS(34,JK)).EQ.12.0D0.AND.FTFL01(11,JK).EQ.0.0D0)
     1            TYPE2='HOE WITHOUT ADDITIONAL PHASE TERMS'
                  IF(DABS(ALENS(34,JK)).EQ.12.0D0.AND.FTFL01(11,JK).EQ.1.0D0)
     1            TYPE2='HOE WITH RADIAL POLYNOMIAL PHASE TERMS (R)'
                  IF(DABS(ALENS(34,JK)).EQ.12.0D0.AND.FTFL01(11,JK).EQ.2.0D0)
     1            TYPE2='HOE WITH RECTANGULAR POLYNOMIAL PHASE TERMS (XY)'
                  IF(DABS(ALENS(34,JK)).EQ.12.0D0.AND.FTFL01(11,JK).EQ.3.0D0)
     1            TYPE2='HOE WITH RECTANGULAR POLYNOMIAL PHASE TERMS (AXY)'
                  IF(DABS(ALENS(34,JK)).EQ.12.0D0.AND.FTFL01(11,JK).EQ.4.0D0)
     1            TYPE2='HOE WITH RECTANGULAR POLYNOMIAL PHASE TERMS (XAY)'
                  IF(DABS(ALENS(34,JK)).EQ.12.0D0.AND.FTFL01(11,JK).EQ.5.0D0)
     1            TYPE2='HOE WITH RECTANGULAR POLYNOMIAL PHASE TERMS (XYA)'
                  IF(DABS(ALENS(34,JK)).EQ.12.0D0.AND.FTFL01(11,JK).EQ.6.0D0)
     1            TYPE2='HOE WITH RADIAL POLYNOMIAL PHASE TERMS (AR)'
                  IF(DABS(ALENS(34,JK)).EQ.12.0D0.AND.FTFL01(11,SURF).EQ.7.0D0)
     1            TYPE2='HOE WITH USER DEFINED PHASE (MACRO FUNCTION-FUN09)'
                  IF(DABS(ALENS(34,JK)).EQ.14.0)
     1            TYPE2='STANDARD ABERRATION POLYNOMIAL PHASE TERMS'
                  IF(DABS(ALENS(34,JK)).EQ.15.0)
     1            TYPE2='ABERRATION POLYNOMIAL PHASE SURFACE'
                  IF(DABS(ALENS(34,JK)).EQ.16.0)
     1            TYPE2='FRESNEL-1 SURFACE'
                  IF(DABS(ALENS(34,JK)).EQ.17.0)
     1            TYPE2='USER DEFINED SURFACE #2'
                  IF(DABS(ALENS(34,JK)).EQ.18.0)
     1            TYPE2='GRAZING INCIDENCE SURFACE'
                  IF(DABS(ALENS(34,JK)).EQ.19.0)
     1            TYPE2='GRID APODIZATION SURFACE'
                  IF(DABS(ALENS(34,JK)).EQ.20.0)
     1            TYPE2='GRID PHASE SURFACE'
                  IF(DABS(ALENS(34,JK)).EQ.21.0)
     1            TYPE2='USER DEFINED SUBROUTINE SURFACE'
                  IF(DABS(ALENS(34,JK)).EQ.22.0)
     1            TYPE2='GRID SAG SURFACE'
                  IF(DABS(ALENS(34,JK)).EQ.23.0)
     1            TYPE2='NORMAL SYMMETRIC CUBIC SPLINE SURFACE'
                  ITY=INT(DABS(ALENS(34,JK)))
                  IF(ALENS(34,JK).EQ.1.0)  TYPE='1- ON    '
                  IF(ALENS(34,JK).EQ.2.0)  TYPE='2- ON    '
                  IF(ALENS(34,JK).EQ.-1.0) TYPE='1- OFF   '
                  IF(ALENS(34,JK).EQ.-2.0) TYPE='2- OFF   '
                  IF(ALENS(34,JK).EQ.3.0)  TYPE='3- ON    '
                  IF(ALENS(34,JK).EQ.-3.0) TYPE='3- OFF   '
                  IF(ALENS(34,JK).EQ.4.0)  TYPE='4- ON    '
                  IF(ALENS(34,JK).EQ.-4.0) TYPE='4- OFF   '
                  IF(ALENS(34,JK).EQ.5.0)  TYPE='5- ON    '
                  IF(ALENS(34,JK).EQ.-5.0) TYPE='5- OFF   '
                  IF(ALENS(34,JK).EQ.6.0)  TYPE='6- ON    '
                  IF(ALENS(34,JK).EQ.-6.0) TYPE='6- OFF   '
                  IF(ALENS(34,JK).EQ.7.0)  TYPE='7- ON    '
                  IF(ALENS(34,JK).EQ.-7.0) TYPE='7- OFF   '
                  IF(ALENS(34,JK).EQ.8.0)  TYPE='8- ON    '
                  IF(ALENS(34,JK).EQ.-8.0) TYPE='8- OFF   '
                  IF(ALENS(34,JK).EQ.9.0)  TYPE='9- ON    '
                  IF(ALENS(34,JK).EQ.-9.0) TYPE='9- OFF   '
                  IF(ALENS(34,JK).EQ.10.0)  TYPE='10- ON   '
                  IF(ALENS(34,JK).EQ.-10.0) TYPE='10- OFF  '
                  IF(ALENS(34,JK).EQ.11.0)  TYPE='11- ON   '
                  IF(ALENS(34,JK).EQ.-11.0) TYPE='11- OFF  '
                  IF(ALENS(34,JK).EQ.12.0)  TYPE='12- ON   '
                  IF(ALENS(34,JK).EQ.-12.0) TYPE='12- OFF  '
                  IF(ALENS(34,JK).EQ.13.0)  TYPE='13- ON   '
                  IF(ALENS(34,JK).EQ.-13.0) TYPE='13- OFF  '
                  IF(ALENS(34,JK).EQ.14.0)  TYPE='14- ON   '
                  IF(ALENS(34,JK).EQ.-14.0) TYPE='14- OFF  '
                  IF(ALENS(34,JK).EQ.15.0)  TYPE='15- ON   '
                  IF(ALENS(34,JK).EQ.-15.0) TYPE='15- OFF  '
                  IF(ALENS(34,JK).EQ.16.0)  TYPE='16- ON   '
                  IF(ALENS(34,JK).EQ.-16.0) TYPE='16- OFF  '
                  IF(ALENS(34,JK).EQ.17.0)  TYPE='17- ON   '
                  IF(ALENS(34,JK).EQ.-17.0) TYPE='17- OFF  '
                  IF(ALENS(34,JK).EQ.18.0)  TYPE='18- ON   '
                  IF(ALENS(34,JK).EQ.-18.0) TYPE='18- OFF  '
                  IF(ALENS(34,JK).EQ.19.0)  TYPE='19- ON   '
                  IF(ALENS(34,JK).EQ.-19.0) TYPE='19- OFF  '
                  IF(ALENS(34,JK).EQ.20.0)  TYPE='20- ON   '
                  IF(ALENS(34,JK).EQ.-20.0) TYPE='20- OFF  '
                  IF(ALENS(34,JK).EQ.21.0)  TYPE='21- ON   '
                  IF(ALENS(34,JK).EQ.-21.0) TYPE='21- OFF  '
                  IF(ALENS(34,JK).EQ.22.0)  TYPE='22- ON   '
                  IF(ALENS(34,JK).EQ.-22.0) TYPE='22- OFF  '
                  IF(ALENS(34,JK).EQ.23.0)  TYPE='23- ON   '
                  IF(ALENS(34,JK).EQ.-23.0) TYPE='23- OFF  '
                  IF(ALENS(34,JK).EQ.24.0)  TYPE='24- ON   '
                  IF(ALENS(34,JK).EQ.-24.0) TYPE='24- OFF  '
                  IF(DABS(ALENS(34,JK)).GT.24.0) THEN
                      WRITE(OUTLYNE,*)'SPECIAL SURFACE TYPE NOT RECOGNIZED'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'VALID TYPES RANGE FROM 1 TO 24'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
C        SET PRINTING ARRAY COEF TO PROPER VALUES
C
                  DO I=1,96
                      IF(DABS(ALENS(34,JK)).GE.1.0.AND.
     1                DABS(ALENS(34,JK)).LE.30.0) THEN
                          COEF(I)=FTFL01(I,JK)
                      END IF
                  END DO
                  IF(DABS(ALENS(34,JK)).NE.13.0D0.OR.
     1            DABS(ALENS(34,JK)).EQ.13.0D0.AND.F12.EQ.1) THEN
                      WRITE(OUTLYNE,101) JK,TYPE
                      CALL SHOWIT(0)
                  END IF

                  WRITE(OUTLYNE,102) TYPE2(1:79)
                  CALL SHOWIT(0)
                  IF(ITY.EQ.1.OR.ITY.EQ.6) THEN

                      WRITE(OUTLYNE,400) COEF(9),COEF(10),COEF(11),
     1                COEF(12)
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,500) COEF(13),COEF(14),COEF(15),
     1                COEF(16)
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,600) COEF(17),COEF(18),COEF(19),
     1                COEF(20)
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,700) COEF(21),COEF(22),COEF(23),
     1                COEF(24)
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,701) COEF(25),COEF(26),COEF(27),
     1                COEF(28)
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,702) COEF(29),COEF(30),COEF(31),
     1                COEF(32)
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,703) COEF(33),COEF(34),COEF(35),
     1                COEF(36)
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,7041) COEF(37),COEF(38),COEF(39)
                      CALL SHOWIT(0)
                  END IF
                  IF(ITY.EQ.2.OR.ITY.EQ.9) THEN

                      WRITE(OUTLYNE,200) COEF(1),COEF(2),COEF(3),
     1                COEF(4)
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,300) COEF(5),COEF(6),COEF(7),
     1                COEF(8)
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,400) COEF(9),COEF(10),COEF(11),
     1                COEF(12)
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,500) COEF(13),COEF(14),COEF(15),
     1                COEF(16)
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,600) COEF(17),COEF(18),COEF(19),
     1                COEF(20)
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,700) COEF(21),COEF(22),COEF(23),
     1                COEF(24)
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,701) COEF(25),COEF(26),COEF(27),
     1                COEF(28)
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,702) COEF(29),COEF(30),COEF(31),
     1                COEF(32)
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,703) COEF(33),COEF(34),COEF(35),
     1                COEF(36)
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,704) COEF(37),COEF(38),COEF(39),
     1                COEF(40)
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,705) COEF(41),COEF(42),COEF(43),
     1                COEF(44)
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,706) COEF(45),COEF(46),COEF(47),
     1                COEF(48)
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,707) COEF(49),COEF(50),COEF(51),
     1                COEF(52)
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,708) COEF(53),COEF(54),COEF(55),
     1                COEF(56)
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,709) COEF(57),COEF(58),COEF(59),
     1                COEF(60)
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,710) COEF(61),COEF(62),COEF(63),
     1                COEF(64)
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,7004) COEF(65),COEF(66)
                      CALL SHOWIT(0)
                  END IF
                  IF(ITY.EQ.3.OR.ITY.EQ.10) THEN

                      WRITE(OUTLYNE,200) COEF(1),COEF(2),COEF(3),
     1                COEF(4)
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,300) COEF(5),COEF(6),COEF(7),
     1                COEF(8)
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,400) COEF(9),COEF(10),COEF(11),
     1                COEF(12)
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,500) COEF(13),COEF(14),COEF(15),
     1                COEF(16)
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,600) COEF(17),COEF(18),COEF(19),
     1                COEF(20)
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,700) COEF(21),COEF(22),COEF(23),
     1                COEF(24)
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,701) COEF(25),COEF(26),COEF(27),
     1                COEF(28)
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,702) COEF(29),COEF(30),COEF(31),
     1                COEF(32)
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,703) COEF(33),COEF(34),COEF(35),
     1                COEF(36)
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,7042) COEF(37)
                      CALL SHOWIT(0)
                  END IF
                  IF(ITY.EQ.4) THEN
                      WRITE(OUTLYNE,31001) COEF(1)
                      CALL SHOWIT(0)
                      WRITE(OUTLYNE,31002) COEF(2),UN
                      CALL SHOWIT(0)
                      WRITE(OUTLYNE,31003) COEF(3),UN
                      CALL SHOWIT(0)
                      WRITE(OUTLYNE,32001) COEF(4)
                      CALL SHOWIT(0)
                      WRITE(OUTLYNE,32002) COEF(5),UN
                      CALL SHOWIT(0)
                      WRITE(OUTLYNE,32003) COEF(6),UN
                      CALL SHOWIT(0)
                      WRITE(OUTLYNE,33001) COEF(7)
                      CALL SHOWIT(0)
                      WRITE(OUTLYNE,33002) COEF(8),UN
                      CALL SHOWIT(0)
                      WRITE(OUTLYNE,33003) COEF(9),UN
                      CALL SHOWIT(0)
                      WRITE(OUTLYNE,34001) COEF(10)
                      CALL SHOWIT(0)
                      WRITE(OUTLYNE,34002) COEF(11),UN
                      CALL SHOWIT(0)
                      WRITE(OUTLYNE,34003) COEF(12),UN
                      CALL SHOWIT(0)
                      WRITE(OUTLYNE,35001) COEF(13)
                      CALL SHOWIT(0)
                      WRITE(OUTLYNE,35002) COEF(14),UN
                      CALL SHOWIT(0)
                      WRITE(OUTLYNE,35003) COEF(15),UN
                      CALL SHOWIT(0)
                  END IF
                  IF(ITY.EQ.18) THEN

                      WRITE(OUTLYNE,5001) COEF(1),UN
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,5002) COEF(2),UN
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,5003) COEF(3),UN
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,5004) COEF(4),COEF(5),COEF(6),
     1                COEF(7)
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,5005) COEF(8),COEF(9),COEF(10),
     1                COEF(11)
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,5006) COEF(12),COEF(13),COEF(14),
     1                COEF(15)
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,5007) COEF(16),COEF(17),COEF(18)
                      CALL SHOWIT(0)
                  END IF
                  IF(ITY.EQ.19) THEN

                      IF(COEF(1).NE.0.0D0)WRITE(OUTLYNE,8001) INT(COEF(1))
                      IF(COEF(1).EQ.0.0D0)WRITE(OUTLYNE,8006)
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,8002) INT(COEF(2))
                      CALL SHOWIT(0)

                      COEF(3)=FTFL01(3,JK)
                      WRITE(OUTLYNE,8003) COEF(3),UN
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,9030) COEF(4)
                      CALL SHOWIT(0)

                  END IF
                  IF(ITY.EQ.22) THEN

                      IF(COEF(1).NE.0.0D0)WRITE(OUTLYNE,8101) INT(COEF(1))
                      IF(COEF(1).EQ.0.0D0)WRITE(OUTLYNE,8106)
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,8102) INT(COEF(2))
                      CALL SHOWIT(0)

                      COEF(3)=FTFL01(3,JK)
                      WRITE(OUTLYNE,8103) COEF(3),UN
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,9050) COEF(4)
                      CALL SHOWIT(0)

                  END IF
                  IF(ITY.EQ.23) THEN
C     NORMAL CUBIC SPLINE
                      WRITE(OUTLYNE,200) COEF(1),COEF(2),COEF(3),
     1                COEF(4)
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,300) COEF(5),COEF(6),COEF(7),
     1                COEF(8)
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,400) COEF(9),COEF(10),COEF(11),
     1                COEF(12)
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,500) COEF(13),COEF(14),COEF(15),
     1                COEF(16)
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,600) COEF(17),COEF(18),COEF(19),
     1                COEF(20)
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,700) COEF(21),COEF(22),COEF(23),
     1                COEF(24)
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,701) COEF(25),COEF(26),COEF(27),
     1                COEF(28)
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,702) COEF(29),COEF(30),COEF(31),
     1                COEF(32)
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,703) COEF(33),COEF(34),COEF(35),
     1                COEF(36)
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,704) COEF(37),COEF(38),COEF(39),
     1                COEF(40)
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,705) COEF(41),COEF(42),COEF(43),
     1                COEF(44)
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,706) COEF(45),COEF(46),COEF(47),
     1                COEF(48)
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,707) COEF(49),COEF(50),COEF(51),
     1                COEF(52)
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,708) COEF(53),COEF(54),COEF(55),
     1                COEF(56)
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,709) COEF(57),COEF(58),COEF(59),
     1                COEF(60)
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,710) COEF(61),COEF(62),COEF(63),
     1                COEF(64)
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,711) COEF(65),COEF(66),COEF(67),
     1                COEF(68)
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,712) COEF(69),COEF(70),COEF(71),
     1                COEF(72)
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,713) COEF(73),COEF(74),COEF(75),
     1                COEF(76)
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,714) COEF(77),COEF(78),COEF(79),
     1                COEF(80)
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,715) COEF(81),COEF(82),COEF(83),
     1                COEF(84)
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,716) COEF(84),COEF(85),COEF(86),
     1                COEF(87)
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,717) COEF(89),COEF(90),COEF(91),
     1                COEF(92)
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,718) COEF(93),COEF(94),COEF(95),
     1                COEF(96)
                      CALL SHOWIT(0)
                  END IF
                  IF(ITY.EQ.20) THEN

                      IF(COEF(1).NE.0.0D0)WRITE(OUTLYNE,9001) INT(COEF(1))
                      IF(COEF(1).EQ.0.0D0)WRITE(OUTLYNE,9006)
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,9002) INT(COEF(2))
                      CALL SHOWIT(0)

                      COEF(3)=FTFL01(3,JK)
                      WRITE(OUTLYNE,9003) COEF(3)
                      CALL SHOWIT(0)
                      IF(COEF(4).EQ.0.0D0)WRITE(OUTLYNE,9007)
                      IF(COEF(4).EQ.1.0D0)WRITE(OUTLYNE,9008) UN
                      IF(COEF(4).EQ.2.0D0)WRITE(OUTLYNE,9009)
                      IF(COEF(4).EQ.3.0D0)WRITE(OUTLYNE,9010)
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,9040) COEF(5)
                      CALL SHOWIT(0)
                  END IF

                  IF(ITY.EQ.5.OR.ITY.EQ.11) THEN

                      WRITE(OUTLYNE,200) COEF(1),COEF(2),COEF(3),
     1                COEF(4)
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,300) COEF(5),COEF(6),COEF(7),
     1                COEF(8)
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,400) COEF(9),COEF(10),COEF(11),
     1                COEF(12)
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,500) COEF(13),COEF(14),COEF(15),
     1                COEF(16)
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,600) COEF(17),COEF(18),COEF(19),
     1                COEF(20)
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,700) COEF(21),COEF(22),COEF(23),
     1                COEF(24)
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,701) COEF(25),COEF(26),COEF(27),
     1                COEF(28)
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,702) COEF(29),COEF(30),COEF(31),
     1                COEF(32)
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,703) COEF(33),COEF(34),COEF(35),
     1                COEF(36)
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,704) COEF(37),COEF(38),COEF(39),
     1                COEF(40)
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,705) COEF(41),COEF(42),COEF(43),
     1                COEF(44)
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,706) COEF(45),COEF(46),COEF(47),
     1                COEF(48)
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,707) COEF(49),COEF(50),COEF(51),
     1                COEF(52)
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,708) COEF(53),COEF(54),COEF(55),
     1                COEF(56)
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,709) COEF(57),COEF(58),COEF(59),
     1                COEF(60)
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,710) COEF(61),COEF(62),COEF(63),
     1                COEF(64)
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,711) COEF(65),COEF(66),COEF(67),
     1                COEF(68)
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,712) COEF(69),COEF(70),COEF(71),
     1                COEF(72)
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,713) COEF(73),COEF(74),COEF(75),
     1                COEF(76)
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,714) COEF(77),COEF(78),COEF(79),
     1                COEF(80)
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,715) COEF(81),COEF(82),COEF(83),
     1                COEF(84)
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,716) COEF(84),COEF(85),COEF(86),
     1                COEF(87)
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,717) COEF(89),COEF(90),COEF(91),
     1                COEF(92)
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,718) COEF(93),COEF(94),COEF(95),
     1                COEF(96)
                      CALL SHOWIT(0)
                  END IF
                  IF(ITY.EQ.21) THEN

                      WRITE(OUTLYNE,200) COEF(1),COEF(2),COEF(3),
     1                COEF(4)
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,300) COEF(5),COEF(6),COEF(7),
     1                COEF(8)
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,400) COEF(9),COEF(10),COEF(11),
     1                COEF(12)
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,500) COEF(13),COEF(14),COEF(15),
     1                COEF(16)
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,600) COEF(17),COEF(18),COEF(19),
     1                COEF(20)
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,700) COEF(21),COEF(22),COEF(23),
     1                COEF(24)
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,701) COEF(25),COEF(26),COEF(27),
     1                COEF(28)
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,702) COEF(29),COEF(30),COEF(31),
     1                COEF(32)
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,703) COEF(33),COEF(34),COEF(35),
     1                COEF(36)
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,704) COEF(37),COEF(38),COEF(39),
     1                COEF(40)
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,705) COEF(41),COEF(42),COEF(43),
     1                COEF(44)
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,706) COEF(45),COEF(46),COEF(47),
     1                COEF(48)
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,707) COEF(49),COEF(50),COEF(51),
     1                COEF(52)
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,708) COEF(53),COEF(54),COEF(55),
     1                COEF(56)
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,709) COEF(57),COEF(58),COEF(59),
     1                COEF(60)
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,710) COEF(61),COEF(62),COEF(63),
     1                COEF(64)
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,711) COEF(65),COEF(66),COEF(67),
     1                COEF(68)
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,712) COEF(69),COEF(70),COEF(71),
     1                COEF(72)
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,713) COEF(73),COEF(74),COEF(75),
     1                COEF(76)
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,714) COEF(77),COEF(78),COEF(79),
     1                COEF(80)
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,715) COEF(81),COEF(82),COEF(83),
     1                COEF(84)
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,716) COEF(84),COEF(85),COEF(86),
     1                COEF(87)
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,717) COEF(89),COEF(90),COEF(91),
     1                COEF(92)
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,718) COEF(93),COEF(94),COEF(95),
     1                COEF(96)
                      CALL SHOWIT(0)
                  END IF
                  IF(ITY.EQ.14.OR.ITY.EQ.15) THEN

                      WRITE(OUTLYNE,200) COEF(1),COEF(2),COEF(3),
     1                COEF(4)
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,300) COEF(5),COEF(6),COEF(7),
     1                COEF(8)
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,400) COEF(9),COEF(10),COEF(11),
     1                COEF(12)
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,500) COEF(13),COEF(14),COEF(15),
     1                COEF(16)
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,600) COEF(17),COEF(18),COEF(19),
     1                COEF(20)
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,700) COEF(21),COEF(22),COEF(23),
     1                COEF(24)
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,701) COEF(25),COEF(26),COEF(27),
     1                COEF(28)
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,702) COEF(29),COEF(30),COEF(31),
     1                COEF(32)
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,703) COEF(33),COEF(34),COEF(35),
     1                COEF(36)
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,704) COEF(37),COEF(38),COEF(39),
     1                COEF(40)
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,705) COEF(41),COEF(42),COEF(43),
     1                COEF(44)
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,706) COEF(45),COEF(46),COEF(47),
     1                COEF(48)
                      CALL SHOWIT(0)
                  END IF
                  IF(ITY.EQ.16) THEN

                      WRITE(OUTLYNE,200) COEF(1),COEF(2),COEF(3),
     1                COEF(4)
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,300) COEF(5),COEF(6),COEF(7),
     1                COEF(8)
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,401) COEF(9),COEF(10),COEF(11)
                      CALL SHOWIT(0)

                  END IF
                  IF(ITY.EQ.7.OR.ITY.EQ.8) THEN

                      WRITE(OUTLYNE,200) COEF(1),COEF(2),COEF(3),
     1                COEF(4)
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,300) COEF(5),COEF(6),COEF(7),
     1                COEF(8)
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,400) COEF(9),COEF(10),COEF(11),
     1                COEF(12)
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,500) COEF(13),COEF(14),COEF(15),
     1                COEF(16)
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,600) COEF(17),COEF(18),COEF(19),
     1                COEF(20)
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,700) COEF(21),COEF(22),COEF(23),
     1                COEF(24)
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,701) COEF(25),COEF(26),COEF(27),
     1                COEF(28)
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,702) COEF(29),COEF(30),COEF(31),
     1                COEF(32)
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,703) COEF(33),COEF(34),COEF(35),
     1                COEF(36)
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,704) COEF(37),COEF(38),COEF(39),
     1                COEF(40)
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,705) COEF(41),COEF(42),COEF(43),
     1                COEF(44)
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,706) COEF(45),COEF(46),COEF(47),
     1                COEF(48)
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,707) COEF(49),COEF(50),COEF(51),
     1                COEF(52)
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,708) COEF(53),COEF(54),COEF(55),
     1                COEF(56)
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,709) COEF(57),COEF(58),COEF(59),
     1                COEF(60)
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,710) COEF(61),COEF(62),COEF(63),
     1                COEF(64)
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,711) COEF(65),COEF(66),COEF(67),
     1                COEF(68)
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,712) COEF(69),COEF(70),COEF(71),
     1                COEF(72)
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,713) COEF(73),COEF(74),COEF(75),
     1                COEF(76)
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,714) COEF(77),COEF(78),COEF(79),
     1                COEF(80)
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,715) COEF(81),COEF(82),COEF(83),
     1                COEF(84)
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,716) COEF(84),COEF(85),COEF(86),
     1                COEF(87)
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,7171) COEF(89),COEF(90),COEF(91)
                      CALL SHOWIT(0)
                  END IF
                  IF(ITY.EQ.13.AND.F12.EQ.1) THEN

                      WRITE(OUTLYNE,2201) COEF(1)
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,2202) COEF(2)
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,2203) COEF(3),UN
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,2204) COEF(4),UN
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,2205) COEF(5),UN
                      CALL SHOWIT(0)

                      IF(COEF(6).GT.0.0D0) REALL='POS.    '
                      IF(COEF(6).LT.0.0D0) REALL='NEG.    '
                      WRITE(OUTLYNE,2206) INT(COEF(6)),REALL
                      CALL SHOWIT(0)
                      IF(COEF(7).GT.0.0D0) REALL='REAL    '
                      IF(COEF(7).LT.0.0D0) REALL='VIRTUAL '
                      IF(COEF(7).GT.0.0D0) COEF(7)=1.0D0
                      IF(COEF(7).LT.0.0D0) COEF(7)=-1.0D0
                      WRITE(OUTLYNE,2207) INT(COEF(7)),REALL
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,2208) COEF(8),UN
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,2209) COEF(9),UN
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,2210) COEF(10),UN
                      CALL SHOWIT(0)

                      IF(COEF(11).GT.0.0D0) REALL='POS.   '
                      IF(COEF(11).LT.0.0D0) REALL='NEG.   '
                      WRITE(OUTLYNE,2211) INT(COEF(11)),REALL
                      CALL SHOWIT(0)
                      IF(COEF(12).GT.0.0D0) REALL='REAL   '
                      IF(COEF(12).LT.0.0D0) REALL='VIRTUAL'
                      IF(COEF(12).GT.0.0D0) COEF(12)=1.0D0
                      IF(COEF(12).LT.0.0D0) COEF(12)=-1.0D0
                      WRITE(OUTLYNE,2212) INT(COEF(12)),REALL
                      CALL SHOWIT(0)
                      WRITE(OUTLYNE,2213) INT(COEF(13))
                      CALL SHOWIT(0)
                      WRITE(OUTLYNE,2214) INT(COEF(14))
                      CALL SHOWIT(0)
                  END IF
                  IF(ITY.EQ.12.AND.COEF(11).EQ.0.0D0) THEN

                      WRITE(OUTLYNE,2001) COEF(1)
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,2002) COEF(2)
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,2003) COEF(3),UN
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,2004) COEF(4),UN
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,2005) COEF(5),UN
                      CALL SHOWIT(0)

                      IF(COEF(6).GT.0.0D0) REALL='REAL    '
                      IF(COEF(6).LT.0.0D0) REALL='VIRTUAL '
                      IF(COEF(6).GT.0.0D0) COEF(6)=1.0D0
                      IF(COEF(6).LT.0.0D0) COEF(6)=-1.0D0
                      WRITE(OUTLYNE,2006) INT(COEF(6)),REALL
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,2007) COEF(7),UN
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,2008) COEF(8),UN
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,2009) COEF(9),UN
                      CALL SHOWIT(0)

                      IF(COEF(10).GT.0.0D0) REALL='REAL   '
                      IF(COEF(10).LT.0.0D0) REALL='VIRTUAL'
                      IF(COEF(10).GT.0.0D0) COEF(10)=1.0D0
                      IF(COEF(10).LT.0.0D0) COEF(10)=-1.0D0
                      WRITE(OUTLYNE,2010) INT(COEF(10)),REALL
                      CALL SHOWIT(0)
                  END IF
                  IF(ITY.EQ.12.AND.COEF(11).EQ.6.0D0) THEN

                      WRITE(OUTLYNE,2001) COEF(1)
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,2002) COEF(2)
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,2003) COEF(3),UN
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,2004) COEF(4),UN
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,2005) COEF(5),UN
                      CALL SHOWIT(0)

                      IF(COEF(6).GT.0.0D0) REALL='REAL    '
                      IF(COEF(6).LT.0.0D0) REALL='VIRTUAL '
                      IF(COEF(6).GT.0.0D0) COEF(6)=1.0D0
                      IF(COEF(6).LT.0.0D0) COEF(6)=-1.0D0
                      WRITE(OUTLYNE,2006) INT(COEF(6)),REALL
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,2007) COEF(7),UN
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,2008) COEF(8),UN
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,2009) COEF(9),UN
                      CALL SHOWIT(0)

                      IF(COEF(10).GT.0.0D0) REALL='REAL   '
                      IF(COEF(10).LT.0.0D0) REALL='VIRTUAL'
                      IF(COEF(10).GT.0.0D0) COEF(10)=1.0D0
                      IF(COEF(10).LT.0.0D0) COEF(10)=-1.0D0
                      WRITE(OUTLYNE,2010) INT(COEF(10)),REALL
                      CALL SHOWIT(0)
                      WRITE(OUTLYNE,4001) COEF(11),COEF(12)
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,500) COEF(13),COEF(14),COEF(15),
     1                COEF(16)
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,600) COEF(17),COEF(18),COEF(19),
     1                COEF(20)
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,7005) COEF(21)
                      CALL SHOWIT(0)
                  END IF
                  IF(ITY.EQ.12.AND.COEF(11).EQ.1.0D0) THEN

                      WRITE(OUTLYNE,2001) COEF(1)
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,2002) COEF(2)
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,2003) COEF(3),UN
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,2004) COEF(4),UN
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,2005) COEF(5),UN
                      CALL SHOWIT(0)

                      IF(COEF(6).GT.0.0D0) REALL='REAL    '
                      IF(COEF(6).LT.0.0D0) REALL='VIRTUAL '
                      IF(COEF(6).GT.0.0D0) COEF(6)=1.0D0
                      IF(COEF(6).LT.0.0D0) COEF(6)=-1.0D0
                      WRITE(OUTLYNE,2006) INT(COEF(6)),REALL
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,2007) COEF(7),UN
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,2008) COEF(8),UN
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,2009) COEF(9),UN
                      CALL SHOWIT(0)

                      IF(COEF(10).GT.0.0D0) REALL='REAL   '
                      IF(COEF(10).LT.0.0D0) REALL='VIRTUAL'
                      IF(COEF(10).GT.0.0D0) COEF(10)=1.0D0
                      IF(COEF(10).LT.0.0D0) COEF(10)=-1.0D0
                      WRITE(OUTLYNE,2010) INT(COEF(10)),REALL
                      CALL SHOWIT(0)
                      WRITE(OUTLYNE,4001) COEF(11),COEF(12)
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,500) COEF(13),COEF(14),COEF(15),
     1                COEF(16)
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,600) COEF(17),COEF(18),COEF(19),
     1                COEF(20)
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,7005) COEF(21)
                      CALL SHOWIT(0)
                  END IF
                  IF(ITY.EQ.12.AND.COEF(11).EQ.7.0D0) THEN

                      WRITE(OUTLYNE,2001) COEF(1)
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,2002) COEF(2)
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,2003) COEF(3),UN
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,2004) COEF(4),UN
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,2005) COEF(5),UN
                      CALL SHOWIT(0)

                      IF(COEF(6).GT.0.0D0) REALL='REAL    '
                      IF(COEF(6).LT.0.0D0) REALL='VIRTUAL '
                      IF(COEF(6).GT.0.0D0) COEF(6)=1.0D0
                      IF(COEF(6).LT.0.0D0) COEF(6)=-1.0D0
                      WRITE(OUTLYNE,2006) INT(COEF(6)),REALL
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,2007) COEF(7),UN
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,2008) COEF(8),UN
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,2009) COEF(9),UN
                      CALL SHOWIT(0)

                      IF(COEF(10).GT.0.0D0) REALL='REAL   '
                      IF(COEF(10).LT.0.0D0) REALL='VIRTUAL'
                      IF(COEF(10).GT.0.0D0) COEF(10)=1.0D0
                      IF(COEF(10).LT.0.0D0) COEF(10)=-1.0D0
                      WRITE(OUTLYNE,2010) INT(COEF(10)),REALL
                      CALL SHOWIT(0)
                      WRITE(OUTLYNE,4001) COEF(11),COEF(12)
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,500) COEF(13),COEF(14),COEF(15),
     1                COEF(16)
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,600) COEF(17),COEF(18),COEF(19),
     1                COEF(20)
                      CALL SHOWIT(0)
                      
                      WRITE(OUTLYNE,700) COEF(21),COEF(22),COEF(23),
     1                COEF(24)
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,701) COEF(25),COEF(26),COEF(27),
     1                COEF(28)
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,702) COEF(29),COEF(30),COEF(31),
     1                COEF(32)
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,703) COEF(33),COEF(34),COEF(35),
     1                COEF(36)
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,704) COEF(37),COEF(38),COEF(39),
     1                COEF(40)
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,705) COEF(41),COEF(42),COEF(43),
     1                COEF(44)
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,706) COEF(45),COEF(46),COEF(47),
     1                COEF(48)
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,707) COEF(49),COEF(50),COEF(51),
     1                COEF(52)
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,708) COEF(53),COEF(54),COEF(55),
     1                COEF(56)
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,709) COEF(57),COEF(58),COEF(59),
     1                COEF(60)
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,710) COEF(61),COEF(62),COEF(63),
     1                COEF(64)
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,711) COEF(65),COEF(66),COEF(67),
     1                COEF(68)
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,712) COEF(69),COEF(70),COEF(71),
     1                COEF(72)
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,713) COEF(73),COEF(74),COEF(75),
     1                COEF(76)
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,714) COEF(77),COEF(78),COEF(79),
     1                COEF(80)
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,715) COEF(81),COEF(82),COEF(83),
     1                COEF(84)
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,716) COEF(84),COEF(85),COEF(86),
     1                COEF(87)
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,717) COEF(89),COEF(90),COEF(91),
     1                COEF(92)
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,718) COEF(93),COEF(94),COEF(95),
     1                COEF(96)
                      CALL SHOWIT(0)
                  END IF
                  IF(ITY.EQ.12.AND.COEF(11).EQ.2.0D0.OR.
     1            ITY.EQ.12.AND.COEF(11).EQ.3.0D0.OR.
     1            ITY.EQ.12.AND.COEF(11).EQ.4.0D0.OR.
     1            ITY.EQ.12.AND.COEF(11).EQ.5.0D0) THEN

                      WRITE(OUTLYNE,2001) COEF(1)
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,2002) COEF(2)
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,2003) COEF(3),UN
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,2004) COEF(4),UN
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,2005) COEF(5),UN
                      CALL SHOWIT(0)

                      IF(COEF(6).GT.0.0D0) REALL='REAL    '
                      IF(COEF(6).LT.0.0D0) REALL='VIRTUAL '
                      IF(COEF(6).GT.0.0D0) COEF(6)=1.0D0
                      IF(COEF(6).LT.0.0D0) COEF(6)=-1.0D0
                      WRITE(OUTLYNE,2006) INT(COEF(6)),REALL
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,2007) COEF(7),UN
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,2008) COEF(8),UN
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,2009) COEF(9),UN
                      CALL SHOWIT(0)

                      IF(COEF(10).GT.0.0D0) REALL='REAL   '
                      IF(COEF(10).LT.0.0D0) REALL='VIRTUAL'
                      IF(COEF(10).GT.0.0D0) COEF(10)=1.0D0
                      IF(COEF(10).LT.0.0D0) COEF(10)=-1.0D0
                      WRITE(OUTLYNE,2010) INT(COEF(10)),REALL
                      CALL SHOWIT(0)
                      WRITE(OUTLYNE,4001) COEF(11),COEF(12)
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,500) COEF(13),COEF(14),COEF(15),
     1                COEF(16)
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,600) COEF(17),COEF(18),COEF(19),
     1                COEF(20)
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,700) COEF(21),COEF(22),COEF(23),COEF(24)
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,701) COEF(25),COEF(26),COEF(27),
     1                COEF(28)
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,702) COEF(29),COEF(30),COEF(31),
     1                COEF(32)
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,703) COEF(33),COEF(34),COEF(35),
     1                COEF(36)
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,704) COEF(37),COEF(38),COEF(39),
     1                COEF(40)
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,705) COEF(41),COEF(42),COEF(43),
     1                COEF(44)
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,706) COEF(45),COEF(46),COEF(47),
     1                COEF(48)
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,707) COEF(49),COEF(50),COEF(51),
     1                COEF(52)
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,708) COEF(53),COEF(54),COEF(55),
     1                COEF(56)
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,709) COEF(57),COEF(58),COEF(59),
     1                COEF(60)
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,710) COEF(61),COEF(62),COEF(63),
     1                COEF(64)
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,711) COEF(65),COEF(66),COEF(67),
     1                COEF(68)
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,712) COEF(69),COEF(70),COEF(71),
     1                COEF(72)
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,713) COEF(73),COEF(74),COEF(75),
     1                COEF(76)
                      CALL SHOWIT(0)

                      WRITE(OUTLYNE,7141) COEF(77),COEF(78),COEF(79)
                      CALL SHOWIT(0)
                  END IF
15            CONTINUE
          END IF
C
 101      FORMAT('SURFACE =',I3,2X,
     1    'SPECIAL SURFACE TYPE =',1X,A9)
 102      FORMAT(A79)
 200      FORMAT('C(01)=',G12.5,1X,'C(02)=',
     1    G12.5,1X,'C(03)=',G12.5,1X,'C(04)=',G12.5)
 2001     FORMAT('HOE ORDER NUMBER      (C1) = ',G15.8)
 2002     FORMAT('CONST. WAVELENGTH     (C2) = ',G15.8,' MICROMETER')
 2003     FORMAT('X-COORD. SOURCE POINT (C3) = ',G15.8,1X,A6)
 2004     FORMAT('Y-COORD. SOURCE POINT (C4) = ',G15.8,1X,A6)
 2005     FORMAT('Z-COORD. SOURCE POINT (C5) = ',G15.8,1X,A6)
 2006     FORMAT('SOURCE REALITY        (C6) = ',I2,1X,A8)
 2007     FORMAT('X-COORD. REF. POINT   (C7) = ',G15.8,1X,A6)
 2008     FORMAT('Y-COORD. REF. POINT   (C8) = ',G15.8,1X,A6)
 2009     FORMAT('Z-COORD. REF. POINT   (C9) = ',G15.8,1X,A6)
 2010     FORMAT('REFERENCE REALITY    (C10) = ',I2,1X,A8)
C
 2201     FORMAT('HOE ORDER NUMBER      (C1) = ',G15.8)
 2202     FORMAT('CONST. WAVELENGTH     (C2) = ',G15.8,' MICROMETER')
 2203     FORMAT('X-COORD. SOURCE POINT (C3) = ',G15.8,1X,A6)
 2204     FORMAT('Y-COORD. SOURCE POINT (C4) = ',G15.8,1X,A6)
 2205     FORMAT('Z-COORD. SOURCE POINT (C5) = ',G15.8,1X,A6)
 2206     FORMAT('SOURCE BEAM DIR.      (C6) = ',I2,1X,A8)
 2207     FORMAT('SOURCE REALITY        (C7) = ',I2,1X,A8)
 2208     FORMAT('X-COORD. REF. POINT   (C8) = ',G15.8,1X,A6)
 2209     FORMAT('Y-COORD. REF. POINT   (C9) = ',G15.8,1X,A6)
 2210     FORMAT('Z-COORD. REF. POINT  (C10) = ',G15.8,1X,A6)
 2211     FORMAT('REFERENCE BEAM DIR.  (C11) = ',I2,1X,A8)
 2212     FORMAT('REFERENCE REALITY    (C12) = ',I2,1X,A8)
 2213     FORMAT('SOURCE CFG#          (C13) = ',I2,1X,A8)
 2214     FORMAT('REFERENCE CFG#       (C14) = ',I2,1X,A8)
 300      FORMAT('C(05)=',G12.5,1X,'C(06)=',
     1    G12.5,1X,'C(07)=',G12.5,1X,'C(08)=',G12.5)
31001     FORMAT('1ST PEAK TO VALLY ERROR (WAVES)  = ',G15.8)
31002     FORMAT('1ST X-PERIOD (SPATIAL FREQUENCY) = ',G15.8,1X,A6)
31003     FORMAT('1ST Y-PERIOD (SPATIAL FREQUENCY) = ',G15.8,1X,A6)
32001     FORMAT('2ND PEAK TO VALLY ERROR (WAVES)  = ',G15.8)
32002     FORMAT('2ND X-PERIOD (SPATIAL FREQUENCY) = ',G15.8,1X,A6)
32003     FORMAT('2ND Y-PERIOD (SPATIAL FREQUENCY) = ',G15.8,1X,A6)
33001     FORMAT('3RD PEAK TO VALLY ERROR (WAVES)  = ',G15.8)
33002     FORMAT('3RD X-PERIOD (SPATIAL FREQUENCY) = ',G15.8,1X,A6)
33003     FORMAT('3RD Y-PERIOD (SPATIAL FREQUENCY) = ',G15.8,1X,A6)
34001     FORMAT('4TH PEAK TO VALLY ERROR (WAVES)  = ',G15.8)
34002     FORMAT('4TH X-PERIOD (SPATIAL FREQUENCY) = ',G15.8,1X,A6)
34003     FORMAT('4TH Y-PERIOD (SPATIAL FREQUENCY) = ',G15.8,1X,A6)
35001     FORMAT('5TH PEAK TO VALLY ERROR (WAVES)  = ',G15.8)
35002     FORMAT('5TH X-PERIOD (SPATIAL FREQUENCY) = ',G15.8,1X,A6)
35003     FORMAT('5TH Y-PERIOD (SPATIAL FREQUENCY) = ',G15.8,1X,A6)
 5001     FORMAT('FORWARD Z-COORDINATE  = ',G15.8,1X,A6)
 5002     FORMAT('    AFT Z-COORDINATE  = ',G15.8,1X,A6)
 5003     FORMAT('CENTRAL Z-COORDINATE  = ',G15.8,1X,A6)
 5004     FORMAT('F(01)=',G12.5,1X,'F(02)=',
     1    G12.5,1X,'F(03)=',G12.5,1X,'F(04)=',G12.5)
 5005     FORMAT('F(05)=',G12.5,1X,'F(06)=',
     1    G12.5,1X,'F(07)=',G12.5,1X,'F(08)=',G12.5)
 5006     FORMAT('F(09)=',G12.5,1X,'F(10)=',
     1    G12.5,1X,'F(11)=',G12.5,1X,'F(12)=',G12.5)
 5007     FORMAT('F(13)=',G12.5,1X,'F(14)=',
     1    G12.5,1X,'F(15)=',G12.5)
 400      FORMAT('C(09)=',G12.5,1X,'C(10)=',
     1    G12.5,1X,'C(11)=',G12.5,1X,'C(12)=',G12.5)
 401      FORMAT('C(09)=',G12.5,1X,'C(10)=',
     1    G12.5,1X,'C(11)=',G12.5)
 4001     FORMAT('C(11)=',G12.5,1X,'C(12)=',G12.5)
 500      FORMAT('C(13)=',G12.5,1X,'C(14)=',
     1    G12.5,1X,'C(15)=',G12.5,1X,'C(16)=',G12.5)
! 5011  FORMAT('C(09)=',G12.5,1X,'C(10)=',
!     1  G12.5,1X,'C(11)=',G12.5)
 600      FORMAT('C(17)=',G12.5,1X,'C(18)=',
     1    G12.5,1X,'C(19)=',G12.5,1X,'C(20)=',G12.5)
 700      FORMAT('C(21)=',G12.5,1X,'C(22)=',
     1    G12.5,1X,'C(23)=',G12.5,1X,'C(24)=',G12.5)
 7001     FORMAT('C(21)=',G12.5,1X,'C(22)=',
     1    G12.5,1X,'C(23)=',G12.5,1X,'C(24)=',G12.5)
 7002     FORMAT('C(25)=',G12.5,1X,'C(26)=',
     1    G12.5,1X,'C(27)=',G12.5,1X,'C(28)=',G12.5)
 7003     FORMAT('C(29)=',G12.5,1X,'C(30)=',
     1    G12.5,1X,'C(31)=',G12.5)
 7004     FORMAT('C(65)=',G12.5,1X,'C(66)=',
     1    G12.5)
 7005     FORMAT('C(21)=',G12.5)
! 7006   FORMAT('C(32)=',G12.5)
 701      FORMAT('C(25)=',G12.5,1X,'C(26)=',
     1    G12.5,1X,'C(27)=',G12.5,1X,'C(28)=',G12.5)
 702      FORMAT('C(29)=',G12.5,1X,'C(30)=',
     1    G12.5,1X,'C(31)=',G12.5,1X,'C(32)=',G12.5)
! 7021   FORMAT('C(29)=',G12.5,1X,'C(30)=',
!     1  G12.5)
 703      FORMAT('C(33)=',G12.5,1X,'C(34)=',
     1    G12.5,1X,'C(35)=',G12.5,1X,'C(36)=',G12.5)
 704      FORMAT('C(37)=',G12.5,1X,'C(38)=',
     1    G12.5,1X,'C(39)=',G12.5,1X,'C(40)=',G12.5)
 7041     FORMAT('C(37)=',G12.5,1X,'C(38)=',
     1    G12.5,1X,'C(39)=',G12.5)
 7042     FORMAT('C(37)=',G12.5)
 705      FORMAT('C(41)=',G12.5,1X,'C(42)=',
     1    G12.5,1X,'C(43)=',G12.5,1X,'C(44)=',G12.5)
 706      FORMAT('C(45)=',G12.5,1X,'C(46)=',
     1    G12.5,1X,'C(47)=',G12.5,1X,'C(48)=',G12.5)
 707      FORMAT('C(49)=',G12.5,1X,'C(50)=',
     1    G12.5,1X,'C(51)=',G12.5,1X,'C(52)=',G12.5)
 708      FORMAT('C(53)=',G12.5,1X,'C(54)=',
     1    G12.5,1X,'C(55)=',G12.5,1X,'C(56)=',G12.5)
 709      FORMAT('C(57)=',G12.5,1X,'C(58)=',
     1    G12.5,1X,'C(59)=',G12.5,1X,'C(60)=',G12.5)
 710      FORMAT('C(61)=',G12.5,1X,'C(62)=',
     1    G12.5,1X,'C(63)=',G12.5,1X,'C(64)=',G12.5)
 711      FORMAT('C(65)=',G12.5,1X,'C(66)=',
     1    G12.5,1X,'C(67)=',G12.5,1X,'C(68)=',G12.5)
 712      FORMAT('C(69)=',G12.5,1X,'C(70)=',
     1    G12.5,1X,'C(71)=',G12.5,1X,'C(72)=',G12.5)
 713      FORMAT('C(73)=',G12.5,1X,'C(74)=',
     1    G12.5,1X,'C(75)=',G12.5,1X,'C(76)=',G12.5)
 714      FORMAT('C(77)=',G12.5,1X,'C(78)=',
     1    G12.5,1X,'C(79)=',G12.5,1X,'C(80)=',G12.5)
 7141     FORMAT('C(81)=',G12.5,1X,'C(82)=',
     1    G12.5,1X,'C(83)=',G12.5)
 715      FORMAT('C(81)=',G12.5,1X,'C(82)=',
     1    G12.5,1X,'C(83)=',G12.5,1X,'C(84)=',G12.5)
 716      FORMAT('C(85)=',G12.5,1X,'C(86)=',
     1    G12.5,1X,'C(87)=',G12.5,1X,'C(88)=',G12.5)
 717      FORMAT('C(89)=',G12.5,1X,'C(90)=',
     1    G12.5,1X,'C(91)=',G12.5,1X,'C(92)=',G12.5)
 7171     FORMAT('C(89)=',G12.5,1X,'C(90)=',
     1    G12.5,1X,'C(91)=',G12.5)
 718      FORMAT('C(93)=',G12.5,1X,'C(94)=',
     1    G12.5,1X,'C(95)=',G12.5,1X,'C(96)=',G12.5)
 110      FORMAT('SURF',1X,I3,1X,
     1    ':NO SPECIAL SURFACE DATA')
 100      FORMAT('NO SPECIAL SURFACE DATA')
 1000     FORMAT('SPECIAL SURFACE DATA')
          RETURN
      END
