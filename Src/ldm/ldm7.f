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

C       SEVENTH FILE FOR LENS DATABASE MANAGER FILES

C SUB SCHG.FOR
      SUBROUTINE SCHG
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE SCHG WHICH IMPLEMENTS THE CHG
C       COMMANDS AT THE UPDATE LENS LEVEL.
C       THE FIRST NUMERIC WORD IS THE SURFACE TO WHICH WE WISH TO
C       CHANGE SURF TO. VALID VALUES ARE FROM 0 TO THE INTEGER VALUE
C       STORED IN SYSTEM1(20)
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
C               CHECK FOR PRESENCE OF QUALIFIER OR STRING INPUT
C               PRINT ERROR AND RETURN IF DISCOVERED.
C
          IF(SQ.EQ.1.OR.SST.EQ.1.OR.S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1
     1    .OR.S5.EQ.1) THEN
C
              OUTLYNE='"CHG" ONLY TAKES NUMERIC WORD #1 INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(DF1.EQ.1) THEN
              OUTLYNE='"CHG" REQUIRES EXPLICIT NUMERIC WORD #1 INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
          IF(F6.EQ.1) THEN
C
C               WE ARE AT LENS UPDATE LEVEL
C
              IF(W1.LT.0.0D0) W1=SYSTEM1(20)+W1
              IF(W1.LT.0.0D0.OR.W1.GT.SYSTEM1(20)) THEN
C
C       TRYING TO CHANGE A SURFACE NUMBER OUTSIDE THE VALID SURFACE
C       NUMBER RANGE FOR THIS LENS.
C       PRINT ERROR AND RETURN.
                  OUTLYNE='SURFACE NUMBER OUTSIDE VALID RANGE FOR THIS LENS'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              ELSE
C       SURFACE NUMBER WITHIN VALID RANGE.
                  SURF=INT(W1)
                  RETURN
              END IF
          END IF
          RETURN
      END
C SUB SCC.FOR
      SUBROUTINE SCC
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE SCC WHICH IMPLEMENTS THE CC
C        AND CCTOR COMMAND AT THE LENS OF UPDATE LENS LEVEL.
C
          INTEGER PIKCNT,I
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
C               CHECK FOR PRESENCE OF QUALIFIER OR STRING INPUT
C               PRINT ERROR AND RETURN IF DISCOVERED.
C
          IF(SST.EQ.1) THEN
              IF(WC.EQ.'CC')
     1        OUTLYNE='"CC" TAKES NO STRING INPUT'
              IF(WC.EQ.'CCTOR')
     1        OUTLYNE='"CCTOR" TAKES NO STRING INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1
     1    .OR.S5.EQ.1) THEN
              IF(WC.EQ.'CC')
     1        OUTLYNE='"CC" TAKES NO NUMERIC WORD #2 THROUGH #5 INPUT'
              IF(WC.EQ.'CCTOR')
     1        OUTLYNE='"CCTOR" TAKES NO NUMERIC WORD #2 THROUGH #5 INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SQ.EQ.1.AND.F5.EQ.1) THEN
              IF(WC.EQ.'CC')
     1        OUTLYNE='"CC" TAKES NO QUALIFIER WORD IN LENS INPUT MODE'
              IF(WC.EQ.'CCTOR')
     1        OUTLYNE='"CCTOR" TAKES NO QUALIFIER WORD IN LENS INPUT MODE'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(F6.EQ.1) THEN
C
              IF(WQ.NE.'        '.AND.WQ.NE.'DELT'.AND.WQ.NE.'CENT') THEN
                  IF(WC.EQ.'CC')
     1            OUTLYNE='INVALID QUALIFIER WORD USED WITH "CC"'
                  IF(WC.EQ.'CCTOR')
     1            OUTLYNE='INVALID QUALIFIER WORD USED WITH "CCTOR"'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
          IF(DF1.EQ.1)THEN
              IF(WC.EQ.'CC')
     1        OUTLYNE='"CC" REQUIRES EXPLICIT NUMERIC WORD #1 INPUT'
              IF(WC.EQ.'CCTOR')
     1        OUTLYNE='"CCTOR" REQUIRES EXPLICIT NUMERIC WORD #1 INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C               WE ARE AT LENS INPUT OR LENS UPDATE LEVEL
C
          IF(WC.EQ.'CC') THEN
              IF(SQ.EQ.0) ALENS(2,SURF)=W1
              IF(WQ.EQ.'DELT') ALENS(2,SURF)=ALENS(2,SURF)+W1
              IF(WQ.EQ.'CENT')
     1        ALENS(2,SURF)=ALENS(2,SURF)+(W1*0.0D0*ALENS(2,SURF))
C
C       CHECK FOR CC PIKUPS AND DELETE IF FOUND
C
C
C       DELETE THE PIKUP
              IF(PIKUP(1,SURF,4).GT.0.0D0) THEN
                  PIKUP(1:6,SURF,4)=0.0D0
                  ALENS(32,SURF)=ALENS(32,SURF)-1.0D0
                  WRITE(OUTLYNE,*)'SURFACE',SURF,' :PIKUP (CC) DELETED'
                  CALL SHOWIT(1)
              END IF
              IF(PIKUP(1,SURF,11).GT.0.0D0) THEN
                  PIKUP(1:6,SURF,11)=0.0D0
                  ALENS(32,SURF)=ALENS(32,SURF)-1.0D0
                  WRITE(OUTLYNE,*)'SURFACE',SURF,' :PIKUP (PRO) DELETED'
                  CALL SHOWIT(1)
              END IF
              IF(PIKUP(1,SURF,12).GT.0.0D0) THEN
                  PIKUP(1:6,SURF,12)=0.0D0
                  ALENS(32,SURF)=ALENS(32,SURF)-1.0D0
                  WRITE(OUTLYNE,*)'SURFACE',SURF,' :PIKUP (NPRO) DELETED'
                  CALL SHOWIT(1)
              END IF
C
C       ARE THERE MORE PIKUPS? IF NOT SET ALENS(32,SURF) TO ZERO.
C
              PIKCNT=0
              DO 10 I=1,PSIZ
                  IF(PIKUP(1,SURF,I).NE.0.0D0) THEN
                      PIKCNT=PIKCNT+1
                  ELSE
                  END IF
 10           CONTINUE
              IF(PIKCNT.EQ.0) ALENS(32,SURF)=0.0D0
              RETURN
          ELSE
          END IF
          IF(WC.EQ.'CCTOR') THEN
              IF(ALENS(24,SURF).EQ.0.0D0)THEN
                  OUTLYNE='WARNING:'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'FOR SURFACE ',SURF
                  CALL SHOWIT(1)
                  OUTLYNE='"CCTOR" WILL BE IGNORED FOR THIS CVTOR=0 SURFACE'
                  CALL SHOWIT(1)
              END IF
              IF(SURF.EQ.0) THEN
                  OUTLYNE='OBJECT SURFACE MAY NOT BE A TORIC'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C       CHECK FOR TORIC SURFACE
              IF(ALENS(23,SURF).EQ.0.0D0) THEN
                  WRITE(OUTLYNE,*)'SURFACE',SURF,' NOT DEFINED AS ANAMORPHIC'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C
C               WE ARE AT LENS INPUT OR LENS UPDATE LEVEL
C
              IF(SQ.EQ.0) ALENS(41,SURF)=W1
              IF(WQ.EQ.'DELT') ALENS(41,SURF)=ALENS(41,SURF)+W1
              IF(WQ.EQ.'CENT')
     1        ALENS(41,SURF)=ALENS(41,SURF)+(W1*0.0D0*ALENS(41,SURF))
C
C
C       DELETE THE PIKUP
              IF(PIKUP(1,SURF,4).GT.0.0D0) THEN
                  PIKUP(1:6,SURF,21)=0.0D0
                  ALENS(32,SURF)=ALENS(32,SURF)-1.0D0
                  WRITE(OUTLYNE,*)'SURFACE',SURF,' :PIKUP(CC) DELETED'
                  CALL SHOWIT(1)
              END IF
C       DUMP PIKUP PRO AND NPRO IF FOUND
              IF(PIKUP(1,SURF,11).GT.0.0D0) THEN
                  PIKUP(1:6,SURF,11)=0.0D0
                  ALENS(32,SURF)=ALENS(32,SURF)-1.0D0
                  WRITE(OUTLYNE,*)'SURFACE',SURF,' :PIKUP (PRO) DELETED'
                  CALL SHOWIT(1)
              END IF
              IF(PIKUP(1,SURF,12).GT.0.0D0) THEN
                  PIKUP(1:6,SURF,12)=0.0D0
                  ALENS(32,SURF)=ALENS(32,SURF)-1.0D0
                  WRITE(OUTLYNE,*)'SURFACE',SURF,' :PIKUP (NPRO) DELETED'
                  CALL SHOWIT(1)
              END IF
C
C       ARE THERE MORE PIKUPS? IF NOT SET ALENS(32,SURF) TO ZERO.
C
              PIKCNT=0
              DO 101 I=1,PSIZ
                  IF(PIKUP(1,SURF,I).NE.0.0D0) THEN
                      PIKCNT=PIKCNT+1
                  ELSE
                  END IF
 101          CONTINUE
C
              IF(PIKCNT.EQ.0) ALENS(32,SURF)=0.0D0
          ELSE
          END IF
          RETURN
      END
C SUB SCASPC.FOR
      SUBROUTINE SCASPC(SCW1)
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE SCALE WHICH IMPLEMENTS ALL SC
C       AND WSC COMMAND FOR CFG1 SPECIAL SURFACE DATA.
C       SCASPC.FOR WHICH IS CALLED BY SCALEA.FOR
C
          REAL*8 SCW1
C
          INTEGER I,J
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
C
          DO I=0,INT(SYSTEM1(20))
C     TYPE 1 AND 6
              IF(DABS(ALENS(34,I)).EQ.1.0D0.OR.DABS(ALENS(34,I)).EQ.6.0D0) THEN
                  IF(FTFL01(9,I).NE.0.0D0) FTFL01(9,I)= FTFL01(9,I)*SCW1
                  IF(FTFL01(10,I).NE.0.0D0) FTFL01(10,I)=FTFL01(10,I)
                  DO J=11,48
                      IF(FTFL01(J,I).NE.0.0D0) THEN
                          FTFL01(J,I)=FTFL01(J,I)/(SCW1**(J-10))
                      END IF
                  END DO
              END IF
C     TYPE 23
              IF(DABS(ALENS(34,I)).EQ.23.0D0) THEN
                  DO J=2,96
                      FTFL01(J,I)=FTFL01(J,I)*(SCW1)
                  END DO
              END IF
C     TYPE 4
              IF(DABS(ALENS(34,I)).EQ.4.0D0) THEN
                  FTFL01(1,I)=FTFL01(1,I)*SCW1
                  FTFL01(2,I)=FTFL01(2,I)*SCW1
                  FTFL01(3,I)=FTFL01(3,I)*SCW1
                  FTFL01(4,I)=FTFL01(4,I)*SCW1
                  FTFL01(5,I)=FTFL01(5,I)*SCW1
                  FTFL01(6,I)=FTFL01(6,I)*SCW1
                  FTFL01(7,I)=FTFL01(7,I)*SCW1
                  FTFL01(8,I)=FTFL01(8,I)*SCW1
                  FTFL01(9,I)=FTFL01(9,I)*SCW1
                  FTFL01(10,I)=FTFL01(10,I)*SCW1
                  FTFL01(11,I)=FTFL01(11,I)*SCW1
                  FTFL01(12,I)=FTFL01(12,I)*SCW1
                  FTFL01(13,I)=FTFL01(13,I)*SCW1
                  FTFL01(14,I)=FTFL01(14,I)*SCW1
                  FTFL01(15,I)=FTFL01(15,I)*SCW1
              END IF
C     TYPE 7 AND 8
              IF(DABS(ALENS(34,I)).EQ.7.0D0.OR.DABS(ALENS(34,I)).EQ.8.0D0) THEN
                  IF(FTFL01(1,I).NE.0.0D0) FTFL01(1,I)=FTFL01(1,1)*SCW1
                  IF(FTFL01(2,I).NE.0.0D0) FTFL01(2,I)=FTFL01(2,I)
                  IF(FTFL01(3,I).NE.0.0D0) FTFL01(3,I)=FTFL01(3,I)
                  IF(FTFL01(4,I).NE.0.0D0) FTFL01(4,I)=FTFL01(4,I)/(SCW1)
                  IF(FTFL01(5,I).NE.0.0D0) FTFL01(5,I)=FTFL01(5,I)/(SCW1)
                  IF(FTFL01(6,I).NE.0.0D0) FTFL01(6,I)=FTFL01(6,I)/(SCW1)
                  IF(FTFL01(7,I).NE.0.0D0) FTFL01(7,I)=FTFL01(7,I)/(SCW1**2)
                  IF(FTFL01(8,I).NE.0.0D0) FTFL01(8,I)=FTFL01(8,I)/(SCW1**2)
                  IF(FTFL01(9,I).NE.0.0D0) FTFL01(9,I)=FTFL01(9,I)/(SCW1**2)
                  IF(FTFL01(10,I).NE.0.0D0) FTFL01(10,I)=FTFL01(10,I)/(SCW1**2)
                  IF(FTFL01(11,I).NE.0.0D0) FTFL01(11,I)=FTFL01(11,I)/(SCW1**3)
                  IF(FTFL01(12,I).NE.0.0D0) FTFL01(12,I)=FTFL01(12,I)/(SCW1**3)
                  IF(FTFL01(13,I).NE.0.0D0) FTFL01(13,I)=FTFL01(13,I)/(SCW1**3)
                  IF(FTFL01(14,I).NE.0.0D0) FTFL01(14,I)=FTFL01(14,I)/(SCW1**3)
                  IF(FTFL01(15,I).NE.0.0D0) FTFL01(15,I)=FTFL01(15,I)/(SCW1**3)
                  IF(FTFL01(16,I).NE.0.0D0) FTFL01(16,I)=FTFL01(16,I)/(SCW1**4)
                  IF(FTFL01(17,I).NE.0.0D0) FTFL01(17,I)=FTFL01(17,I)/(SCW1**4)
                  IF(FTFL01(18,I).NE.0.0D0) FTFL01(18,I)=FTFL01(18,I)/(SCW1**4)
                  IF(FTFL01(19,I).NE.0.0D0) FTFL01(19,I)=FTFL01(19,I)/(SCW1**4)
                  IF(FTFL01(20,I).NE.0.0D0) FTFL01(20,I)=FTFL01(20,I)/(SCW1**4)
                  IF(FTFL01(21,I).NE.0.0D0) FTFL01(21,I)=FTFL01(21,I)/(SCW1**4)
                  IF(FTFL01(22,I).NE.0.0D0) FTFL01(22,I)=FTFL01(22,I)/(SCW1**5)
                  IF(FTFL01(23,I).NE.0.0D0) FTFL01(23,I)=FTFL01(23,I)/(SCW1**5)
                  IF(FTFL01(24,I).NE.0.0D0) FTFL01(24,I)=FTFL01(24,I)/(SCW1**5)
                  IF(FTFL01(25,I).NE.0.0D0) FTFL01(25,I)=FTFL01(25,I)/(SCW1**5)
                  IF(FTFL01(26,I).NE.0.0D0) FTFL01(26,I)=FTFL01(26,I)/(SCW1**5)
                  IF(FTFL01(27,I).NE.0.0D0) FTFL01(27,I)=FTFL01(27,I)/(SCW1**5)
                  IF(FTFL01(28,I).NE.0.0D0) FTFL01(28,I)=FTFL01(28,I)/(SCW1**5)
                  IF(FTFL01(29,I).NE.0.0D0) FTFL01(29,I)=FTFL01(29,I)/(SCW1**6)
                  IF(FTFL01(30,I).NE.0.0D0) FTFL01(30,I)=FTFL01(30,I)/(SCW1**6)
                  IF(FTFL01(31,I).NE.0.0D0) FTFL01(31,I)=FTFL01(31,I)/(SCW1**6)
                  IF(FTFL01(32,I).NE.0.0D0) FTFL01(32,I)=FTFL01(32,I)/(SCW1**6)
                  IF(FTFL01(33,I).NE.0.0D0) FTFL01(33,I)=FTFL01(33,I)/(SCW1**6)
                  IF(FTFL01(34,I).NE.0.0D0) FTFL01(34,I)=FTFL01(34,I)/(SCW1**6)
                  IF(FTFL01(35,I).NE.0.0D0) FTFL01(35,I)=FTFL01(35,I)/(SCW1**6)
                  IF(FTFL01(36,I).NE.0.0D0) FTFL01(36,I)=FTFL01(36,I)/(SCW1**6)
                  IF(FTFL01(37,I).NE.0.0D0) FTFL01(37,I)=FTFL01(37,I)/(SCW1**7)
                  IF(FTFL01(38,I).NE.0.0D0) FTFL01(38,I)=FTFL01(38,I)/(SCW1**7)
                  IF(FTFL01(39,I).NE.0.0D0) FTFL01(39,I)=FTFL01(39,I)/(SCW1**7)
                  IF(FTFL01(40,I).NE.0.0D0) FTFL01(40,I)=FTFL01(40,I)/(SCW1**7)
                  IF(FTFL01(41,I).NE.0.0D0) FTFL01(41,I)=FTFL01(41,I)/(SCW1**7)
                  IF(FTFL01(42,I).NE.0.0D0) FTFL01(42,I)=FTFL01(42,I)/(SCW1**7)
                  IF(FTFL01(43,I).NE.0.0D0) FTFL01(43,I)=FTFL01(43,I)/(SCW1**7)
                  IF(FTFL01(44,I).NE.0.0D0) FTFL01(44,I)=FTFL01(44,I)/(SCW1**7)
                  IF(FTFL01(45,I).NE.0.0D0) FTFL01(45,I)=FTFL01(45,I)/(SCW1**7)
                  IF(FTFL01(46,I).NE.0.0D0) FTFL01(46,I)=FTFL01(46,I)/(SCW1**8)
                  IF(FTFL01(47,I).NE.0.0D0) FTFL01(47,I)=FTFL01(47,I)/(SCW1**8)
                  IF(FTFL01(48,I).NE.0.0D0) FTFL01(48,I)=FTFL01(48,I)/(SCW1**8)
                  IF(FTFL01(49,I).NE.0.0D0) FTFL01(49,I)=FTFL01(49,I)/(SCW1**8)
                  IF(FTFL01(50,I).NE.0.0D0) FTFL01(50,I)=FTFL01(50,I)/(SCW1**8)
                  IF(FTFL01(51,I).NE.0.0D0) FTFL01(51,I)=FTFL01(51,I)/(SCW1**8)
                  IF(FTFL01(52,I).NE.0.0D0) FTFL01(52,I)=FTFL01(52,I)/(SCW1**8)
                  IF(FTFL01(53,I).NE.0.0D0) FTFL01(53,I)=FTFL01(53,I)/(SCW1**8)
                  IF(FTFL01(54,I).NE.0.0D0) FTFL01(54,I)=FTFL01(54,I)/(SCW1**8)
                  IF(FTFL01(55,I).NE.0.0D0) FTFL01(55,I)=FTFL01(55,I)/(SCW1**8)
                  IF(FTFL01(56,I).NE.0.0D0) FTFL01(56,I)=FTFL01(56,I)/(SCW1**9)
                  IF(FTFL01(57,I).NE.0.0D0) FTFL01(57,I)=FTFL01(57,I)/(SCW1**9)
                  IF(FTFL01(58,I).NE.0.0D0) FTFL01(58,I)=FTFL01(58,I)/(SCW1**9)
                  IF(FTFL01(59,I).NE.0.0D0) FTFL01(59,I)=FTFL01(59,I)/(SCW1**9)
                  IF(FTFL01(60,I).NE.0.0D0) FTFL01(60,I)=FTFL01(60,I)/(SCW1**9)
                  IF(FTFL01(61,I).NE.0.0D0) FTFL01(61,I)=FTFL01(61,I)/(SCW1**9)
                  IF(FTFL01(62,I).NE.0.0D0) FTFL01(62,I)=FTFL01(62,I)/(SCW1**9)
                  IF(FTFL01(63,I).NE.0.0D0) FTFL01(63,I)=FTFL01(63,I)/(SCW1**9)
                  IF(FTFL01(64,I).NE.0.0D0) FTFL01(64,I)=FTFL01(64,I)/(SCW1**9)
                  IF(FTFL01(65,I).NE.0.0D0) FTFL01(65,I)=FTFL01(65,I)/(SCW1**9)
                  IF(FTFL01(66,I).NE.0.0D0) FTFL01(66,I)=FTFL01(66,I)/(SCW1**9)
                  IF(FTFL01(67,I).NE.0.0D0) FTFL01(67,I)=FTFL01(67,I)/(SCW1**10)
                  IF(FTFL01(68,I).NE.0.0D0) FTFL01(68,I)=FTFL01(68,I)/(SCW1**10)
                  IF(FTFL01(69,I).NE.0.0D0) FTFL01(69,I)=FTFL01(69,I)/(SCW1**10)
                  IF(FTFL01(70,I).NE.0.0D0) FTFL01(70,I)=FTFL01(70,I)/(SCW1**10)
                  IF(FTFL01(71,I).NE.0.0D0) FTFL01(71,I)=FTFL01(71,I)/(SCW1**10)
                  IF(FTFL01(72,I).NE.0.0D0) FTFL01(72,I)=FTFL01(72,I)/(SCW1**10)
                  IF(FTFL01(73,I).NE.0.0D0) FTFL01(73,I)=FTFL01(73,I)/(SCW1**10)
                  IF(FTFL01(74,I).NE.0.0D0) FTFL01(74,I)=FTFL01(74,I)/(SCW1**10)
                  IF(FTFL01(75,I).NE.0.0D0) FTFL01(75,I)=FTFL01(75,I)/(SCW1**10)
                  IF(FTFL01(76,I).NE.0.0D0) FTFL01(76,I)=FTFL01(76,I)/(SCW1**10)
                  IF(FTFL01(77,I).NE.0.0D0) FTFL01(77,I)=FTFL01(77,I)/(SCW1**10)
                  IF(FTFL01(78,I).NE.0.0D0) FTFL01(78,I)=FTFL01(78,I)/(SCW1**10)
                  IF(FTFL01(79,I).NE.0.0D0) FTFL01(79,I)=FTFL01(79,I)/(SCW1**11)
                  IF(FTFL01(80,I).NE.0.0D0) FTFL01(80,I)=FTFL01(80,I)/(SCW1**11)
                  IF(FTFL01(81,I).NE.0.0D0) FTFL01(81,I)=FTFL01(81,I)/(SCW1**11)
                  IF(FTFL01(82,I).NE.0.0D0) FTFL01(82,I)=FTFL01(82,I)/(SCW1**11)
                  IF(FTFL01(83,I).NE.0.0D0) FTFL01(83,I)=FTFL01(83,I)/(SCW1**11)
                  IF(FTFL01(84,I).NE.0.0D0) FTFL01(84,I)=FTFL01(84,I)/(SCW1**11)
                  IF(FTFL01(85,I).NE.0.0D0) FTFL01(85,I)=FTFL01(85,I)/(SCW1**11)
                  IF(FTFL01(86,I).NE.0.0D0) FTFL01(86,I)=FTFL01(86,I)/(SCW1**11)
                  IF(FTFL01(87,I).NE.0.0D0) FTFL01(87,I)=FTFL01(87,I)/(SCW1**11)
                  IF(FTFL01(88,I).NE.0.0D0) FTFL01(88,I)=FTFL01(88,I)/(SCW1**11)
                  IF(FTFL01(89,I).NE.0.0D0) FTFL01(89,I)=FTFL01(89,I)/(SCW1**11)
                  IF(FTFL01(90,I).NE.0.0D0) FTFL01(90,I)=FTFL01(90,I)/(SCW1**11)
                  IF(FTFL01(91,I).NE.0.0D0) FTFL01(91,I)=FTFL01(91,I)/(SCW1**11)
              END IF
C     TYPE 12
              IF(DABS(ALENS(34,I)).EQ.12.0D0) THEN
                  FTFL01(1,I)=FTFL01(1,I)*(SCW1/DABS(SCW1))
                  IF(FTFL01(3,I).NE.0.0D0) FTFL01(3,I)=FTFL01(3,I)*SCW1
                  IF(FTFL01(4,I).NE.0.0D0) FTFL01(4,I)=FTFL01(4,I)*SCW1
                  IF(FTFL01(5,I).NE.0.0D0) FTFL01(5,I)=FTFL01(5,I)*SCW1
                  IF(FTFL01(7,I).NE.0.0D0) FTFL01(7,I)=FTFL01(7,I)*SCW1
                  IF(FTFL01(8,I).NE.0.0D0) FTFL01(8,I)=FTFL01(8,I)*SCW1
                  IF(FTFL01(9,I).NE.0.0D0) FTFL01(9,I)=FTFL01(9,I)*SCW1
                  IF(FTFL01(11,I).NE.0.0D0) FTFL01(11,I)=FTFL01(11,I)*SCW1
                  IF(FTFL01(12,I).NE.0.0D0) FTFL01(12,I)=FTFL01(12,I)
                  IF(FTFL01(13,I).NE.0.0D0) FTFL01(13,I)=FTFL01(13,I)/(SCW1)
                  IF(FTFL01(14,I).NE.0.0D0) FTFL01(14,I)=FTFL01(14,I)/(SCW1**2)
                  IF(FTFL01(15,I).NE.0.0D0) FTFL01(15,I)=FTFL01(15,I)/(SCW1**3)
                  IF(FTFL01(16,I).NE.0.0D0) FTFL01(16,I)=FTFL01(16,I)/(SCW1**4)
                  IF(FTFL01(17,I).NE.0.0D0) FTFL01(17,I)=FTFL01(17,I)/(SCW1**5)
                  IF(FTFL01(18,I).NE.0.0D0) FTFL01(18,I)=FTFL01(18,I)/(SCW1**6)
                  IF(FTFL01(19,I).NE.0.0D0) FTFL01(19,I)=FTFL01(19,I)/(SCW1**7)
                  IF(FTFL01(20,I).NE.0.0D0) FTFL01(20,I)=FTFL01(20,I)/(SCW1**8)
                  IF(FTFL01(21,I).NE.0.0D0) FTFL01(21,I)=FTFL01(21,I)/(SCW1**9)
                  IF(FTFL01(22,I).NE.0.0D0) FTFL01(22,I)=FTFL01(22,I)/(SCW1**10)
                  IF(FTFL01(23,I).NE.0.0D0) FTFL01(23,I)=FTFL01(23,I)/(SCW1**11)
              END IF
          END DO
C
          RETURN
      END
C SUB AUTOFUNC.FOR
      SUBROUTINE AUTOFUNC
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE AUTOFUNC WHICH IMPLEMENTS THE AUTOFUNC
C       COMMAND AT THE CMD LEVEL
C
!      INTEGER I,CLCNT
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
C
C     CMD LEVEL
          IF(SST.EQ.0.AND.SN.EQ.0.AND.SQ.EQ.0.OR.STI.EQ.1) THEN
              IF(HEADIN) WRITE(OUTLYNE,401)
              IF(HEADIN) CALL SHOWIT(0)
              IF(SYSTEM1(91).NE.0.0D0) WRITE(OUTLYNE,100)INT(SYSTEM1(91))
              IF(SYSTEM1(91).EQ.0.0D0) WRITE(OUTLYNE,101)
              CALL SHOWIT(0)
              RETURN
          END IF
          IF(SST.EQ.1.OR.SQ.EQ.1.OR.SN.EQ.1) THEN
              OUTLYNE='AT THE CMD LEVEL, "AUTOFUNC" TAKES NO INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
 401      FORMAT(1X)
 100      FORMAT('CURRENT AUTOFUNC FUNCTION NUMBER = ',I2)
 101      FORMAT('NO AUTOFUNC FUNCTION IS CURRENTLY SPECIFIED')
          RETURN
      END
C SUB TTHM.FOR
      SUBROUTINE TTHM
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE TTHM WHICH IMPLEMENTS THE THM
C       COMMANDS
C
          INTEGER I,CLCNT
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
C
C     CMD LEVEL
          IF(SST.EQ.1) THEN
              OUTLYNE='AT THE CMD LEVEL, "THM" TAKES NO STRING INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SQ.EQ.1.AND.S1.EQ.1) THEN
              OUTLYNE=
     1        'AT THE CMD LEVEL, "THM" TAKES EITHER QUALIFIER OR'
              CALL SHOWIT(1)
              OUTLYNE='NUMERIC WORD #1 INPUT BUT NOT BOTH'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
              OUTLYNE=
     1        'AT THE CMD LEVEL,'
              CALL SHOWIT(1)
              OUTLYNE=
     1        '"THM" TAKES NO NUMERIC WORD #2 THROUGH #5 INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C       WHAT IF NO SURFACES EXIST
          IF(SYSTEM1(20).EQ.0.0D0) THEN
              OUTLYNE='NO MIRROR THICKNESS DATA EXISTS'
              CALL SHOWIT(1)
              OUTLYNE='LENS SYSTEM HAS NO SURFACES'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
C       W1 DESIGNATES THE SURFACE FOR WHICH THE NORMAL
C       DATA IS TO BE OUTPUT.
C       IF THE QUALIFIER "ALL" IS USED, THEN THE NORMAL DATA FOR
C       THE ENTIRE LENS IS PRINTED.
C
C       PRINT OUT FOR AN INDIVIDUAL SURFACE
C
          IF(WQ.EQ.'OB'.OR.WQ.EQ.'OBJ') THEN
C       THIS IS THE SAME AS NORMAL,0
              SQ=0
              WQ='        '
              S1=1
              W1=0.0D0
              DF1=0
          ELSE
C       NOT "OB" OR "OBJ"
          END IF
C
          IF(SQ.EQ.0) THEN
              IF(W1.LT.0.0D0) W1=SYSTEM1(20)+W1
              IF(DF1.EQ.1) W1=DBLE(INT(SYSTEM1(20)))
              I=INT(W1)
              SURF=I
              IF(I.GT.INT(SYSTEM1(20)).OR.I.LT.0) THEN
                  OUTLYNE='SURFACE NUMBER BEYOND LEGAL RANGE'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(GLANAM(I,2).NE.'REFL         '.AND.
     1        GLANAM(I,2).NE.'REFLTIRO     '.AND.
     1        GLANAM(I,2).NE.'REFLTIR      ') THEN
                  OUTLYNE='NON-REFLECTIVE SURFACES HAVE NO THM VALUE ASSIGNED'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(HEADIN) WRITE(OUTLYNE,500)
              IF(HEADIN) CALL SHOWIT(0)
              WRITE(OUTLYNE,100)I,DABS(ALENS(110,I))
              CALL SHOWIT(0)
              RETURN
          ELSE
C       THERE WAS A QUALIFIER.
              IF(WQ.NE.'ALL') THEN
                  OUTLYNE='INVALID QUALIFIER WORD'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C
C       CHECK FOR NO DATA
              CLCNT=0
              DO SURF=0,INT(SYSTEM1(20))
                  IF(ALENS(110,SURF).NE.0.0D0) THEN
                      CLCNT=CLCNT+1
                  ELSE
                  END IF
              END DO
              IF(CLCNT.EQ.0) THEN
                  WRITE(OUTLYNE,110)
                  CALL SHOWIT(0)
                  RETURN
              END IF
 110          FORMAT('NO MIRROR THICKNESS DATA')
C
C       PRINT HEADER MESSAGE
              WRITE(OUTLYNE,401)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,400)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,401)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,500)
              CALL SHOWIT(0)
              DO I=0,INT(SYSTEM1(20))

                  IF(GLANAM(I,2).NE.'REFL         '.AND.
     1            GLANAM(I,2).NE.'REFLTIRO     '.AND.
     1            GLANAM(I,2).NE.'REFLTIR      ') THEN
                      WRITE(OUTLYNE,101)I
                      CALL SHOWIT(0)
                      GO TO 200
                  END IF
                  IF(ALENS(110,I).NE.0.0D0) THEN
                      WRITE(OUTLYNE,100)I,DABS(ALENS(110,I))
                      CALL SHOWIT(0)
                  END IF
 200              CONTINUE
              END DO
C
 400          FORMAT('MIRROR THICKNESS VALUES')
 401          FORMAT(1X)
 500          FORMAT('SURF',5X,
     1        '"MIRROR THICKNESS" VALUE (IN CURRENT LENS UNITS)')
 100          FORMAT(I3,11X,G15.8)
 101          FORMAT(I3,11X,'THM NOT VALID ON THIS NON-REFLECTIVE SURFACE')
          END IF
          RETURN
      END
C SUB PPRICE.FOR
      SUBROUTINE PPRICE
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE PPRIC WHICH IMPLEMENTS THE PRICE
C       COMMANDS
C
          INTEGER I,CLCNT
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
C
C     CMD LEVEL
          IF(SST.EQ.1) THEN
              OUTLYNE='AT THE CMD LEVEL, "PRICE" TAKES NO STRING INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SQ.EQ.1.AND.S1.EQ.1) THEN
              OUTLYNE=
     1        'AT THE CMD LEVEL, "PRICE" TAKES EITHER QUALIFIER OR'
              CALL SHOWIT(1)
              OUTLYNE='NUMERIC WORD #1 INPUT BUT NOT BOTH'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
              OUTLYNE=
     1        'AT THE CMD LEVEL,'
              CALL SHOWIT(1)
              OUTLYNE=
     1        '"PRICE" TAKES NO NUMERIC WORD #2 THROUGH #5 INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C       WHAT IF NO SURFACES EXIST
          IF(SYSTEM1(20).EQ.0.0D0) THEN
              OUTLYNE='NO "PRICE" DATA EXISTS'
              CALL SHOWIT(1)
              OUTLYNE='LENS SYSTEM HAS NO SURFACES'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
C       W1 DESIGNATES THE SURFACE FOR WHICH THE NORMAL
C       DATA IS TO BE OUTPUT.
C       IF THE QUALIFIER "ALL" IS USED, THEN THE NORMAL DATA FOR
C       THE ENTIRE LENS IS PRINTED.
C
C       PRINT OUT FOR AN INDIVIDUAL SURFACE
C
          IF(WQ.EQ.'OB'.OR.WQ.EQ.'OBJ') THEN
C       THIS IS THE SAME AS NORMAL,0
              SQ=0
              WQ='        '
              S1=1
              W1=0.0D0
              DF1=0
          ELSE
C       NOT "OB" OR "OBJ"
          END IF
C
          IF(SQ.EQ.0) THEN
              IF(W1.LT.0.0D0) W1=SYSTEM1(20)+W1
              IF(DF1.EQ.1) W1=DBLE(INT(SYSTEM1(20)))
              I=INT(W1)
              SURF=I
              IF(I.GT.INT(SYSTEM1(20)).OR.I.LT.0) THEN
                  OUTLYNE='SURFACE NUMBER BEYOND LEGAL RANGE'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(HEADIN) WRITE(OUTLYNE,500)
              IF(HEADIN) CALL SHOWIT(0)
              WRITE(OUTLYNE,100)I,DABS(ALENS(111,I))
              CALL SHOWIT(0)
              RETURN
          ELSE
C       THERE WAS A QUALIFIER.
              IF(WQ.NE.'ALL') THEN
                  OUTLYNE='INVALID QUALIFIER WORD'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C
C       CHECK FOR NO DATA
              CLCNT=0
              DO SURF=0,INT(SYSTEM1(20))
                  IF(ALENS(111,SURF).NE.0.0D0) THEN
                      CLCNT=CLCNT+1
                  ELSE
                  END IF
              END DO
              IF(CLCNT.EQ.0) THEN
                  WRITE(OUTLYNE,110)
                  CALL SHOWIT(0)
                  RETURN
              END IF
 110          FORMAT('NO PRICE DATA')
C
C       PRINT HEADER MESSAGE
              WRITE(OUTLYNE,401)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,400)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,401)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,500)
              CALL SHOWIT(0)
              DO I=0,INT(SYSTEM1(20))
                  IF(ALENS(111,I).NE.0.0D0) THEN
                      WRITE(OUTLYNE,100)I,DABS(ALENS(111,I))
                      CALL SHOWIT(0)
                  END IF
              END DO
C
 400          FORMAT('PRICE UNIT PER Kg VALUES')
 401          FORMAT(1X)
 500          FORMAT('SURF',5X,'"PRICE" VALUE IN PRICE UNITS)')
 100          FORMAT(I3,11X,F15.4)
          END IF
          RETURN
      END
C SUB INRINR.FOR
      SUBROUTINE INRINR
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE INRINR WHICH IMPLEMENTS THE INR
C       COMMANDS
C
          INTEGER I
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
C
          IF(F1.NE.1) THEN
C     NOT CMD LEVEL
C               CHECK FOR PRESENCE OF QUALIFIER OR STRING INPUT
C               PRINT ERROR AND RETURN IF DISCOVERED.
C
              IF(S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
                  OUTLYNE='"INR" TAKES NO NUMERIC WORD #2 THROUGH #5 INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(SST.EQ.1) THEN
                  OUTLYNE='"INR" TAKES NO STRING INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(SQ.EQ.1.AND.F5.EQ.1) THEN
                  OUTLYNE='"INR" TAKES NO QUALIFIER INPUT AT LENS INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(SQ.EQ.1.AND.F6.EQ.1.AND.WQ.NE.'CENT'.AND.
     1        SQ.EQ.1.AND.F6.EQ.1.AND.WQ.NE.'DELT') THEN
                  OUTLYNE='INVALID QUALIFIER INPUT FOR "INR" AT LENS UPDATE'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(S1.EQ.0) THEN
                  OUTLYNE='"INR" REQUIRES EXPLICIT NUMERIC WORD #1 INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(F6.EQ.1.AND.WQ.EQ.'DELT') THEN
                  ALENS(76,SURF)=ALENS(76,SURF)+(W1)
                  ALENS(143,SURF)=1.0D0
              END IF
              IF(F6.EQ.1.AND.WQ.EQ.'CENT') THEN
                  ALENS(76,SURF)=
     1            ALENS(76,SURF)+(W1*0.01D0*ALENS(76,SURF))
                  ALENS(143,SURF)=1.0D0
              END IF
              IF(F5.EQ.1.OR.F6.EQ.1.AND.SQ.EQ.0) THEN
                  ALENS(76,SURF)=W1
                  ALENS(143,SURF)=1.0D0
              END IF
C
          ELSE
C     CMD LEVEL
              IF(SST.EQ.1) THEN
                  OUTLYNE='AT THE CMD LEVEL, "INR" TAKES NO STRING INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(SQ.EQ.1.AND.S1.EQ.1) THEN
                  OUTLYNE=
     1            'AT THE CMD LEVEL, "INR" TAKES EITHER QUALIFIER OR'
                  CALL SHOWIT(1)
                  OUTLYNE='NUMERIC WORD #1 INPUT BUT NOT BOTH'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
                  OUTLYNE=
     1            'AT THE CMD LEVEL,'
                  CALL SHOWIT(1)
                  OUTLYNE=
     1            '"INR" TAKES NO NUMERIC WORD #2 THROUGH #5 INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C       WHAT IF NO SURFACES EXIST
              IF(SYSTEM1(20).EQ.0.0D0) THEN
                  OUTLYNE='NO "INR" DATA EXISTS'
                  CALL SHOWIT(1)
                  OUTLYNE='LENS SYSTEM HAS NO SURFACES'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C
C       W1 DESIGNATES THE SURFACE FOR WHICH THE NORMAL
C       DATA IS TO BE OUTPUT.
C       IF THE QUALIFIER "ALL" IS USED, THEN THE NORMAL DATA FOR
C       THE ENTIRE LENS IS PRINTED.
C
C       PRINT OUT FOR AN INDIVIDUAL SURFACE
C
              IF(WQ.EQ.'OB'.OR.WQ.EQ.'OBJ') THEN
C       THIS IS THE SAME AS NORMAL,0
                  SQ=0
                  WQ='        '
                  S1=1
                  W1=0.0D0
                  DF1=0
              ELSE
C       NOT "OB" OR "OBJ"
              END IF
C
              IF(SQ.EQ.0) THEN
                  IF(W1.LT.0.0D0) W1=SYSTEM1(20)+W1
                  IF(DF1.EQ.1) W1=DBLE(INT(SYSTEM1(20)))
                  I=INT(W1)
                  SURF=I
                  IF(I.GT.INT(SYSTEM1(20)).OR.I.LT.0) THEN
                      OUTLYNE='SURFACE NUMBER BEYOND LEGAL RANGE'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  IF(HEADIN) WRITE(OUTLYNE,500)
                  IF(HEADIN) CALL SHOWIT(0)
                  WRITE(OUTLYNE,100)I,ALENS(76,I)
                  CALL SHOWIT(0)
                  RETURN
              ELSE
C       THERE WAS A QUALIFIER.
                  IF(WQ.NE.'ALL') THEN
                      OUTLYNE='INVALID QUALIFIER WORD'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
C
C       CHECK FOR NO DATA
C
C       PRINT HEADER MESSAGE
                  WRITE(OUTLYNE,401)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,400)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,402)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,401)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,500)
                  CALL SHOWIT(0)
                  DO I=0,INT(SYSTEM1(20))
                      WRITE(OUTLYNE,100)I,ALENS(76,I)
                      CALL SHOWIT(0)
                  END DO
C
 400              FORMAT('SURFACE "INR" VALUES')
 402              FORMAT
     1            ('"INR" IS THE REFERENCE RADIUS FOR "FRINGE" CALCULATIONS')
 401              FORMAT(1X)
 500              FORMAT('SURF',5X,'"INR" VALUE (IN CURRENT LENS UNITS)')
 100              FORMAT(I3,11X,G15.8)
              END IF
          END IF
          RETURN
      END


C SUB INRINRD.FOR
      SUBROUTINE INRINRD
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE INRINRD WHICH IMPLEMENTS THE INRD
C
          INTEGER SF
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
C               CHECK FOR PRESENCE OF QUALIFIER OR STRING INPUT
C               OR NUMERIC INPUT.
C               PRINT ERROR AND RETURN IF DISCOVERED.
C
          IF(SQ.EQ.1.OR.SST.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
              OUTLYNE=
     1        '"INRD" TAKES NO STRING OR QUALIFIER'
              CALL SHOWIT(1)
              OUTLYNE=
     1        'OR NUMERIC WORD #3 THROUGH #5 INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(DF1.EQ.1.AND.DF2.EQ.1) THEN
              W1=DBLE(SURF)
              W2=DBLE(SURF)
              S1=1
              S2=1
              DF1=0
              DF2=0
          END IF
          IF(DF1.EQ.1.AND.DF2.EQ.0.OR.DF1.EQ.0.AND.DF2.EQ.1) THEN
              OUTLYNE=
     1        '"INRD" USES EITHER TWO OR ZERO NUMERIC WORDS'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(INT(W1).LT.0) THEN
              OUTLYNE=
     1        'STARTING SURFACE NUMBER MUST BE GREATER THAN OR EQUAL TO 0'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(INT(W2).GT.INT(SYSTEM1(20))) THEN
              WRITE(OUTLYNE,*)
     1        'ENDING SURFACE NUMBER MUST BE LESS THAN OR EQUAL TO ',
     1        INT(SYSTEM1(20))
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(W1.GT.W2) THEN
              OUTLYNE=
     1        'THE ENDING SURFACE # MUST BE GREATER THAN OR EQUAL TO#'
              CALL SHOWIT(1)
              OUTLYNE=
     1        'THE STARTING SURFACE #'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          DO SF=INT(W1),INT(W2)
              IF(ALENS(143,SF).EQ.1.0D0) THEN
C     EXPLICIT ASSIGNMENT EXISTS
                  ALENS(76,SF)=0.0D0
                  ALENS(143,SF)=0.0D0
                  WRITE(OUTLYNE,*)
     1            'EXPLICIT "INR" ASSIGNMENT FOR SURFACE',SF,' DELETED'
                  CALL SHOWIT(1)
              ELSE
                  WRITE(OUTLYNE,*)
     1              'EXPLICIT "INR" ASSIGNMENT NOT DEFINED FOR SURFACE'
     1              ,SF
                  CALL SHOWIT(1)
                  CALL MACFAL
              END IF
          END DO
          RETURN
      END
C SUB SPGR.FOR
      SUBROUTINE SPGR
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE SPGR WHICH IMPLEMENTS THE SPGR
C       COMMANDS
C
          INTEGER I
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
C
          IF(F1.NE.1) THEN
C     NOT CMD LEVEL
C               CHECK FOR PRESENCE OF QUALIFIER OR STRING INPUT
C               PRINT ERROR AND RETURN IF DISCOVERED.
C
              IF(S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
                  OUTLYNE='"SPGR" TAKES NO NUMERIC WORD #2 THROUGH #5 INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(SST.EQ.1) THEN
                  OUTLYNE='"SPGR" TAKES NO STRING INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(SQ.EQ.1.AND.F5.EQ.1) THEN
                  OUTLYNE='"SPGR" TAKES NO QUALIFIER INPUT AT LENS INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(SQ.EQ.1.AND.F6.EQ.1.AND.WQ.NE.'CENT'.AND.
     1        SQ.EQ.1.AND.F6.EQ.1.AND.WQ.NE.'DELT') THEN
                  OUTLYNE='INVALID QUALIFIER INPUT FOR "SPGR" AT LENS UPDATE'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(S1.EQ.0) THEN
                  OUTLYNE='"SPCR" REQUIRES EXPLICIT NUMERIC WORD #1 INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(F6.EQ.1.AND.WQ.EQ.'DELT') THEN
                  ALENS(102,SURF)=DABS(ALENS(102,SURF)+(W1))
              END IF
              IF(F6.EQ.1.AND.WQ.EQ.'CENT') THEN
                  ALENS(102,SURF)=
     1            DABS(ALENS(102,SURF)+(W1*0.01D0*ALENS(102,SURF)))
              END IF
              IF(F5.EQ.1.OR.F6.EQ.1.AND.SQ.EQ.0) THEN
                  ALENS(102,SURF)=DABS(W1)
              END IF
C
          ELSE
C     CMD LEVEL
              IF(SST.EQ.1) THEN
                  OUTLYNE='AT THE CMD LEVEL, "SPGR" TAKES NO STRING INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(SQ.EQ.1.AND.S1.EQ.1) THEN
                  OUTLYNE=
     1            'AT THE CMD LEVEL, "SPGR" TAKES EITHER QUALIFIER OR'
                  CALL SHOWIT(1)
                  OUTLYNE='NUMERIC WORD #1 INPUT BUT NOT BOTH'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
                  OUTLYNE=
     1            'AT THE CMD LEVEL,'
                  CALL SHOWIT(1)
                  OUTLYNE=
     1            '"SPGR" TAKES NO NUMERIC WORD #2 THROUGH #5 INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C       WHAT IF NO SURFACES EXIST
              IF(SYSTEM1(20).EQ.0.0D0) THEN
                  OUTLYNE='NO "SPGR" DATA EXISTS'
                  CALL SHOWIT(1)
                  OUTLYNE='LENS SYSTEM HAS NO SURFACES'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C
C       W1 DESIGNATES THE SURFACE FOR WHICH THE NORMAL
C       DATA IS TO BE OUTPUT.
C       IF THE QUALIFIER "ALL" IS USED, THEN THE NORMAL DATA FOR
C       THE ENTIRE LENS IS PRINTED.
C
C       PRINT OUT FOR AN INDIVIDUAL SURFACE
C
              IF(WQ.EQ.'OB'.OR.WQ.EQ.'OBJ') THEN
C       THIS IS THE SAME AS NORMAL,0
                  SQ=0
                  WQ='        '
                  S1=1
                  W1=0.0D0
                  DF1=0
              ELSE
C       NOT "OB" OR "OBJ"
              END IF
C
              IF(SQ.EQ.0) THEN
                  IF(W1.LT.0.0D0) W1=SYSTEM1(20)+W1
                  IF(DF1.EQ.1) W1=DBLE(INT(SYSTEM1(20)))
                  I=INT(W1)
                  SURF=I
                  IF(I.GT.INT(SYSTEM1(20)).OR.I.LT.0) THEN
                      OUTLYNE='SURFACE NUMBER BEYOND LEGAL RANGE'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  IF(HEADIN) WRITE(OUTLYNE,500)
                  IF(HEADIN) CALL SHOWIT(0)
                  WRITE(OUTLYNE,100)I,DABS(ALENS(102,I))
                  CALL SHOWIT(0)
                  RETURN
              ELSE
C       THERE WAS A QUALIFIER.
                  IF(WQ.NE.'ALL') THEN
                      OUTLYNE='INVALID QUALIFIER WORD'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
C
C       CHECK FOR NO DATA
C
C       PRINT HEADER MESSAGE
                  WRITE(OUTLYNE,401)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,400)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,401)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,500)
                  CALL SHOWIT(0)
                  DO I=0,INT(SYSTEM1(20))
                      WRITE(OUTLYNE,100)I,DABS(ALENS(102,I))
                      CALL SHOWIT(0)
                  END DO
C
 400              FORMAT('SURFACE "SPECIFIC GRAVITY (gm/cc)" VALUES')
 401              FORMAT(1X)
 500              FORMAT('SURF',5X,'"SPGR" VALUE (IN CURRENT LENS UNITS)')
 100              FORMAT(I3,11X,G15.8)
              END IF
          END IF
          RETURN
      END
C SUB SCAOB.FOR
      SUBROUTINE SCAOB
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE SCAOB WHICH IMPLEMENTS THE CAOB
C       COMMAND AT THE CMD LEVEL
C       THE ONLY VALID QUALIFIERS ARE (BLANK) AND "ALL","OB" AND "OBJ"
C       THE NUMERIC ENTRY W1, IS THE SURFACE DESIGNATOR
C       FOR PRINTOUT OF ONE SURFACE'S DATA ONLY
C
          CHARACTER CLTYPE*8,COTYPE*8,CLETYPE*8,COETYPE*8
     2    ,ASURF*3,SPCE*1,LINE*80,SP10*10,AAL*10
C
          INTEGER I
     1    ,CLAP,COBS,CLCNT,CLAPE,COBSE
C
          LOGICAL NOHEAD
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
C               THIS IS A CMD LEVEL COMMAND ONLY
C
C               CHECK FOR ADDITIONAL INPUT AND
C               PRINT ERROR AND RETURN IF DISCOVERED.
C
          SP10='          '
          SPCE=' '
          CLAP=0
          CLAPE=0
          COBS=0
          COBSE=0
C
          IF(SST.EQ.1) THEN
              OUTLYNE='"CAOB" TAKES NO STRING INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SQ.EQ.1.AND.S1.EQ.1) THEN
              S1=0
              DF1=0
              W1=0.0D0
              OUTLYNE=
     1        '"CAOB" TAKES EITHER QUALIFIER OR NUMERIC WORD #1 INPUT'
              CALL SHOWIT(1)
              OUTLYNE='BUT NOT BOTH'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C       WHAT IF NO SURFACES EXIST
          IF(SYSTEM1(20).EQ.0.0D0) THEN
              OUTLYNE='NO CLEAR APERTURES OR OBSCURATIONS EXIST'
              CALL SHOWIT(1)
              OUTLYNE='LENS SYSTEM HAS NO SURFACES'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
C       W1 DESIGNATES THE SURFACE FOR WHICH THE CAOB
C       DATA IS TO BE OUTPUT.
C       IF THE QUALIFIER "ALL" IS USED, THEN THE CLEAR APERTURE
C       DATA FOR
C       THE ENTIRE LENS IS PRINTED. IF THE QUALIFIER IS "OB" OR "OBJ"
C       THEN DATA FOR THE OBJECT SURFACE IS PRINTED.
C
          IF(WQ.EQ.'OB'.OR.WQ.EQ.'OBJ') THEN
C
C       THIS IS SAME AS CAOB,0
              S1=1
              DF1=0
              SQ=0
              WQ='        '
              W1=0.0
          ELSE
C       WQ NOT OB OR OBJ
          END IF
          IF(SQ.EQ.1.AND.WQ.NE.'ALL') THEN
C
C       WE HAVE INVALID QUALIFIER
              OUTLYNE='INVALID QUALIFIER INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SQ.EQ.0) THEN
C       HANDEL AN INDIVIDUAL SURFACE INCLUDING "OB" AND "OBJ"
              IF(DF1.EQ.1) W1=DBLE(INT(SYSTEM1(20)))
              SURF=INT(W1)
              IF(SURF.GT.(INT(SYSTEM1(20))).OR.SURF.LT.0) THEN
                  OUTLYNE='SURFACE NUMBER BEYOND LEGAL RANGE'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF

              COBS=0
              COBSE=0
              CLAP=0
              CLAPE=0
              IF(ALENS(9,SURF).NE.0.0D0) CLAP=1
              IF(ALENS(51,SURF).NE.0.0D0) CLAPE=1
              IF(ALENS(9,SURF).EQ.1.0) CLTYPE ='    CIRC'
              IF(ALENS(9,SURF).EQ.2.0) CLTYPE ='    RECT'
              IF(ALENS(9,SURF).EQ.3.0) CLTYPE ='    ELIP'
              IF(ALENS(9,SURF).EQ.4.0) CLTYPE ='    RCTK'
              IF(ALENS(9,SURF).EQ.5.0) CLTYPE ='    POLY'
              IF(ALENS(9,SURF).EQ.6.0) CLTYPE ='   IPOLY'
              IF(ALENS(51,SURF).EQ.1.0) CLETYPE='   ERASE'
              IF(ALENS(51,SURF).EQ.2.0) CLETYPE='   RECTE'
              IF(ALENS(51,SURF).EQ.3.0) CLETYPE='   ELIPE'
              IF(ALENS(51,SURF).EQ.4.0) CLETYPE='   RCTKE'
              IF(ALENS(51,SURF).EQ.5.0) CLETYPE='   POLYE'
              IF(ALENS(51,SURF).EQ.6.0) CLETYPE='  IPOLYE'
              IF(ALENS(16,SURF).NE.0.0D0) COBS=1
              IF(ALENS(61,SURF).NE.0.0D0) COBSE=1
              IF(ALENS(16,SURF).EQ.1.0) COTYPE= 'OB  CIRC'
              IF(ALENS(16,SURF).EQ.2.0) COTYPE= 'OB  RECT'
              IF(ALENS(16,SURF).EQ.3.0) COTYPE= 'OB  ELIP'
              IF(ALENS(16,SURF).EQ.4.0) COTYPE= 'OB  RCTK'
              IF(ALENS(16,SURF).EQ.5.0) COTYPE= 'OB  POLY'
              IF(ALENS(16,SURF).EQ.6.0) COTYPE= 'OB IPOLY'
              IF(ALENS(61,SURF).EQ.1.0) COETYPE='OB ERASE'
              IF(ALENS(61,SURF).EQ.2.0) COETYPE='OB RECTE'
              IF(ALENS(61,SURF).EQ.3.0) COETYPE='OB ELIPE'
              IF(ALENS(61,SURF).EQ.4.0) COETYPE='OB RCTKE'
              IF(ALENS(61,SURF).EQ.5.0) COETYPE='OB POLYE'
              IF(ALENS(61,SURF).EQ.6.0) COETYPE='OBIPOLYE'
C
C       FIRST PRINT CLAP DATA THEN COBS DATA
C               HANDEL NO DATA FOR A SURFACE
              IF(CLAP.EQ.0.AND.COBS.EQ.0) THEN

                  CALL NTOAN1(SURF,ASURF)
                  WRITE(OUTLYNE,100) ASURF(1:3)
                  CALL SHOWIT(0)
                  RETURN
              END IF
C
C                       DO PRINTING NOW
C*****************************************************************
C
C                       HANDEL CLAP DATA
C
              IF(CLAP.EQ.1) THEN
                  CALL NTOAN1(SURF,ASURF)
                  LINE=ASURF(1:3)//SPCE(1:1)//CLTYPE(1:8)//SPCE(1:1)
                  CALL NTOAN2(ALENS(10,SURF),AAL(1:10))
                  LINE=LINE(1:13)//AAL(1:10)//SPCE(1:1)
                  IF(ALENS(11,SURF).EQ.0.0D0) THEN
                      LINE=LINE(1:24)//SP10(1:10)//SPCE(1:1)
                  ELSE
                      CALL NTOAN2(ALENS(11,SURF),AAL(1:10))
                      LINE=LINE(1:24)//AAL(1:10)//SPCE(1:1)
                  END IF
                  IF(ALENS(12,SURF).EQ.0.0D0) THEN
                      LINE=LINE(1:35)//SP10(1:10)//SPCE(1:1)
                  ELSE
                      CALL NTOAN2(ALENS(12,SURF),AAL(1:10))
                      LINE=LINE(1:35)//AAL(1:10)//SPCE(1:1)
                  END IF
                  IF(ALENS(13,SURF).EQ.0.0D0) THEN
                      LINE=LINE(1:46)//SP10(1:10)//SPCE(1:1)
                  ELSE
                      CALL NTOAN2(ALENS(13,SURF),AAL(1:10))
                      LINE=LINE(1:46)//AAL(1:10)//SPCE(1:1)
                  END IF
                  IF(ALENS(14,SURF).EQ.0.0D0) THEN
                      LINE=LINE(1:57)//SP10(1:10)//SPCE(1:1)
                  ELSE
                      CALL NTOAN2(ALENS(14,SURF),AAL(1:10))
                      LINE=LINE(1:57)//AAL(1:10)//SPCE(1:1)
                  END IF
                  IF(ALENS(15,SURF).EQ.0.0D0) THEN
                      LINE=LINE(1:68)//SP10(1:10)
                  ELSE
                      CALL NTOAN2(ALENS(15,SURF),AAL(1:10))
                      LINE=LINE(1:68)//AAL(1:10)
                  END IF

                  IF(HEADIN) THEN
                      WRITE(OUTLYNE,2000)
                      CALL SHOWIT(0)
                      WRITE(OUTLYNE,2001)
                      CALL SHOWIT(0)
                      WRITE(OUTLYNE,3000)
                      CALL SHOWIT(0)
                  END IF

                  WRITE(OUTLYNE,200) LINE(1:79)
                  CALL SHOWIT(0)
              END IF
              IF(CLAPE.EQ.1) THEN
                  CALL NTOAN1(SURF,ASURF)
                  LINE=ASURF(1:3)//SPCE(1:1)//CLETYPE(1:8)//SPCE(1:1)
                  CALL NTOAN2(ALENS(52,SURF),AAL(1:10))
                  LINE=LINE(1:13)//AAL(1:10)//SPCE(1:1)
                  IF(ALENS(53,SURF).EQ.0.0D0) THEN
                      LINE=LINE(1:24)//SP10(1:10)//SPCE(1:1)
                  ELSE
                      CALL NTOAN2(ALENS(53,SURF),AAL(1:10))
                      LINE=LINE(1:24)//AAL(1:10)//SPCE(1:1)
                  END IF
                  IF(ALENS(54,SURF).EQ.0.0D0) THEN
                      LINE=LINE(1:35)//SP10(1:10)//SPCE(1:1)
                  ELSE
                      CALL NTOAN2(ALENS(54,SURF),AAL(1:10))
                      LINE=LINE(1:35)//AAL(1:10)//SPCE(1:1)
                  END IF
                  IF(ALENS(55,SURF).EQ.0.0D0) THEN
                      LINE=LINE(1:46)//SP10(1:10)//SPCE(1:1)
                  ELSE
                      CALL NTOAN2(ALENS(55,SURF),AAL(1:10))
                      LINE=LINE(1:46)//AAL(1:10)//SPCE(1:1)
                  END IF
                  IF(ALENS(56,SURF).EQ.0.0D0) THEN
                      LINE=LINE(1:57)//SP10(1:10)//SPCE(1:1)
                  ELSE
                      CALL NTOAN2(ALENS(56,SURF),AAL(1:10))
                      LINE=LINE(1:57)//AAL(1:10)//SPCE(1:1)
                  END IF
                  IF(ALENS(57,SURF).EQ.0.0D0) THEN
                      LINE=LINE(1:68)//SP10(1:10)
                  ELSE
                      CALL NTOAN2(ALENS(57,SURF),AAL(1:10))
                      LINE=LINE(1:68)//AAL(1:10)
                  END IF
                  IF(HEADIN) THEN
                      WRITE(OUTLYNE,2000)
                      CALL SHOWIT(0)
                      WRITE(OUTLYNE,2001)
                      CALL SHOWIT(0)
                      WRITE(OUTLYNE,3000)
                      CALL SHOWIT(0)
                  END IF
                  WRITE(OUTLYNE,200) LINE(1:79)
                  CALL SHOWIT(0)
              END IF
C       HANDEL COBS DATA
              IF(COBS.EQ.1) THEN
                  CALL NTOAN1(SURF,ASURF)
                  LINE=ASURF(1:3)//SPCE(1:1)//COTYPE(1:8)//SPCE(1:1)
                  CALL NTOAN2(ALENS(17,SURF),AAL)
                  LINE=LINE(1:13)//AAL(1:10)//SPCE(1:1)
C
                  IF(ALENS(18,SURF).EQ.0.0D0) THEN
                      LINE=LINE(1:24)//SP10(1:10)//SPCE(1:1)
                  ELSE
                      CALL NTOAN2(ALENS(18,SURF),AAL(1:10))
                      LINE=LINE(1:24)//AAL(1:10)//SPCE(1:1)
                  END IF
C
                  IF(ALENS(19,SURF).EQ.0.0D0) THEN
                      LINE=LINE(1:35)//SP10(1:10)//SPCE(1:1)
                  ELSE
                      CALL NTOAN2(ALENS(19,SURF),AAL(1:10))
                      LINE=LINE(1:35)//AAL(1:10)//SPCE(1:1)
                  END IF
                  IF(ALENS(20,SURF).EQ.0.0D0) THEN
                      LINE=LINE(1:46)//SP10(1:10)//SPCE(1:1)
                  ELSE
                      CALL NTOAN2(ALENS(20,SURF),AAL(1:10))
                      LINE=LINE(1:46)//AAL(1:10)//SPCE(1:1)
                  END IF
                  IF(ALENS(21,SURF).EQ.0.0D0) THEN
                      LINE=LINE(1:57)//SP10(1:10)//SPCE(1:1)
                  ELSE
                      CALL NTOAN2(ALENS(21,SURF),AAL(1:10))
                      LINE=LINE(1:57)//AAL(1:10)//SPCE(1:1)
                  END IF
                  IF(ALENS(22,SURF).EQ.0.0D0) THEN
                      LINE=LINE(1:68)//SP10(1:10)
                  ELSE
                      CALL NTOAN2(ALENS(22,SURF),AAL(1:10))
                      LINE=LINE(1:68)//AAL(1:10)
                  END IF
                  IF(HEADIN) THEN
                      WRITE(OUTLYNE,2000)
                      CALL SHOWIT(0)
                      WRITE(OUTLYNE,2001)
                      CALL SHOWIT(0)
                      WRITE(OUTLYNE,3000)
                      CALL SHOWIT(0)
                  END IF
                  WRITE(OUTLYNE,300) LINE(1:79)
                  CALL SHOWIT(0)
              END IF
              IF(COBSE.EQ.1) THEN
                  CALL NTOAN1(SURF,ASURF)
                  LINE=ASURF(1:3)//SPCE(1:1)//COETYPE(1:8)//SPCE(1:1)
                  CALL NTOAN2(ALENS(62,SURF),AAL)
                  LINE=LINE(1:13)//AAL(1:10)//SPCE(1:1)
C
                  IF(ALENS(63,SURF).EQ.0.0D0) THEN
                      LINE=LINE(1:24)//SP10(1:10)//SPCE(1:1)
                  ELSE
                      CALL NTOAN2(ALENS(63,SURF),AAL(1:10))
                      LINE=LINE(1:24)//AAL(1:10)//SPCE(1:1)
                  END IF
C
                  IF(ALENS(64,SURF).EQ.0.0D0) THEN
                      LINE=LINE(1:35)//SP10(1:10)//SPCE(1:1)
                  ELSE
                      CALL NTOAN2(ALENS(64,SURF),AAL(1:10))
                      LINE=LINE(1:35)//AAL(1:10)//SPCE(1:1)
                  END IF
                  IF(ALENS(65,SURF).EQ.0.0D0) THEN
                      LINE=LINE(1:46)//SP10(1:10)//SPCE(1:1)
                  ELSE
                      CALL NTOAN2(ALENS(65,SURF),AAL(1:10))
                      LINE=LINE(1:46)//AAL(1:10)//SPCE(1:1)
                  END IF
                  IF(ALENS(66,SURF).EQ.0.0D0) THEN
                      LINE=LINE(1:57)//SP10(1:10)//SPCE(1:1)
                  ELSE
                      CALL NTOAN2(ALENS(66,SURF),AAL(1:10))
                      LINE=LINE(1:57)//AAL(1:10)//SPCE(1:1)
                  END IF
                  IF(ALENS(67,SURF).EQ.0.0D0) THEN
                      LINE=LINE(1:68)//SP10(1:10)
                  ELSE
                      CALL NTOAN2(ALENS(67,SURF),AAL(1:10))
                      LINE=LINE(1:68)//AAL(1:10)
                  END IF
                  IF(HEADIN) THEN
                      WRITE(OUTLYNE,2000)
                      CALL SHOWIT(0)
                      WRITE(OUTLYNE,2001)
                      CALL SHOWIT(0)
                      WRITE(OUTLYNE,3000)
                      CALL SHOWIT(0)
                  END IF
                  WRITE(OUTLYNE,300) LINE(1:79)
                  CALL SHOWIT(0)
              END IF
 2008         FORMAT(I3,4X,I4,5X,G13.6,1X,G13.6,1X,G13.6)
              IF(ALENS(127,SURF).NE.0.0D0) THEN
                  DO I=1,INT(ALENS(127,SURF))
                      IF(HEADIN) THEN
                          WRITE(OUTLYNE,2009)
                          CALL SHOWIT(0)
                          WRITE(OUTLYNE,2010)
                          CALL SHOWIT(0)
                      END IF
                      WRITE(OUTLYNE,2008)
     1                SURF,I,MULTCLAP(I,1,SURF),MULTCLAP(I,2,SURF),
     2                MULTCLAP(I,3,SURF)
                      CALL SHOWIT(1)
                  END DO
              END IF
              IF(ALENS(128,SURF).NE.0.0D0) THEN
                  DO I=1,INT(ALENS(128,SURF))
                      IF(HEADIN) THEN
                          WRITE(OUTLYNE,3009)
                          CALL SHOWIT(0)
                          WRITE(OUTLYNE,2010)
                          CALL SHOWIT(0)
                      END IF
                      WRITE(OUTLYNE,2008)
     1                SURF,I,MULTCOBS(I,1,SURF),MULTCOBS(I,2,SURF),
     2                MULTCOBS(I,3,SURF)
                      CALL SHOWIT(1)
                  END DO
              END IF
              RETURN
          ELSE
C*********************************************************************
C       SQ=1 AND ONLY ALLOWED WQ BY NOW IS "ALL"
C       HANDEL THE WHOLE LENS
              CLCNT=0
C       PRINT HEADING DATA
C
              DO 15 SURF=0,INT(SYSTEM1(20))
                  IF(ALENS(9,SURF).NE.0.0.AND.ALENS(16,SURF).NE.0.0
     1            .OR.ALENS(9,SURF).NE.0.0.OR.ALENS(16,SURF).NE.0.0D0) THEN
                      CLCNT=CLCNT+1
                  ELSE
                  END IF
 15           CONTINUE
              IF(CLCNT.EQ.0) THEN
                  WRITE(OUTLYNE,110)
                  CALL SHOWIT(0)
                  RETURN
              END IF
C       PROCEED WITH PROCESSING
              WRITE(OUTLYNE,1000)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,1500)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,2000)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,2001)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,3000)
              CALL SHOWIT(0)
              DO 5 SURF=0,INT(SYSTEM1(20))
                  CLAP=0
                  CLAPE=0
                  COBS=0
                  COBSE=0
                  IF(ALENS(9,SURF).NE.0.0D0) CLAP=1
                  IF(ALENS(51,SURF).NE.0.0D0) CLAPE=1
                  IF(ALENS(9,SURF).EQ.1.0) CLTYPE ='    CIRC'
                  IF(ALENS(9,SURF).EQ.2.0) CLTYPE ='    RECT'
                  IF(ALENS(9,SURF).EQ.3.0) CLTYPE ='    ELIP'
                  IF(ALENS(9,SURF).EQ.4.0) CLTYPE ='    RCTK'
                  IF(ALENS(9,SURF).EQ.5.0) CLTYPE ='    POLY'
                  IF(ALENS(9,SURF).EQ.6.0) CLTYPE ='   IPOLY'
                  IF(ALENS(51,SURF).EQ.1.0) CLETYPE='   ERASE'
                  IF(ALENS(51,SURF).EQ.2.0) CLETYPE='   RECTE'
                  IF(ALENS(51,SURF).EQ.3.0) CLETYPE='   ELIPE'
                  IF(ALENS(51,SURF).EQ.4.0) CLETYPE='   RCTKE'
                  IF(ALENS(51,SURF).EQ.5.0) CLETYPE='   POLYE'
                  IF(ALENS(51,SURF).EQ.6.0) CLETYPE='  IPOLYE'
                  IF(ALENS(16,SURF).NE.0.0D0) COBS=1
                  IF(ALENS(61,SURF).NE.0.0D0) COBSE=1
                  IF(ALENS(16,SURF).EQ.1.0) COTYPE= 'OB  CIRC'
                  IF(ALENS(16,SURF).EQ.2.0) COTYPE= 'OB  RECT'
                  IF(ALENS(16,SURF).EQ.3.0) COTYPE= 'OB  ELIP'
                  IF(ALENS(16,SURF).EQ.4.0) COTYPE= 'OB  RCTK'
                  IF(ALENS(16,SURF).EQ.5.0) COTYPE= 'OB  POLY'
                  IF(ALENS(16,SURF).EQ.6.0) COTYPE= 'OB IPOLY'
                  IF(ALENS(61,SURF).EQ.1.0) COETYPE='OB ERASE'
                  IF(ALENS(61,SURF).EQ.2.0) COETYPE='OB RECTE'
                  IF(ALENS(61,SURF).EQ.3.0) COETYPE='OB ELIPE'
                  IF(ALENS(61,SURF).EQ.4.0) COETYPE='OB RCTKE'
                  IF(ALENS(61,SURF).EQ.5.0) COETYPE='OB POLYE'
                  IF(ALENS(61,SURF).EQ.6.0) COETYPE='OBIPOLYE'
C
C                       DO PRINTING NOW
C*****************************************************************
C
C                       HANDEL CLAP DATA
C
                  IF(CLAP.EQ.1) THEN
                      CALL NTOAN1(SURF,ASURF)
                      LINE=ASURF(1:3)//SPCE(1:1)//CLTYPE(1:8)//SPCE(1:1)
                      CALL NTOAN2(ALENS(10,SURF),AAL(1:10))
                      LINE=LINE(1:13)//AAL(1:10)//SPCE(1:1)
                      IF(ALENS(11,SURF).EQ.0.0D0) THEN
                          LINE=LINE(1:24)//SP10(1:10)//SPCE(1:1)
                      ELSE
                          CALL NTOAN2(ALENS(11,SURF),AAL(1:10))
                          LINE=LINE(1:24)//AAL(1:10)//SPCE(1:1)
                      END IF
                      IF(ALENS(12,SURF).EQ.0.0D0) THEN
                          LINE=LINE(1:35)//SP10(1:10)//SPCE(1:1)
                      ELSE
                          CALL NTOAN2(ALENS(12,SURF),AAL(1:10))
                          LINE=LINE(1:35)//AAL(1:10)//SPCE(1:1)
                      END IF
                      IF(ALENS(13,SURF).EQ.0.0D0) THEN
                          LINE=LINE(1:46)//SP10(1:10)//SPCE(1:1)
                      ELSE
                          CALL NTOAN2(ALENS(13,SURF),AAL(1:10))
                          LINE=LINE(1:46)//AAL(1:10)//SPCE(1:1)
                      END IF
                      IF(ALENS(14,SURF).EQ.0.0D0) THEN
                          LINE=LINE(1:57)//SP10(1:10)//SPCE(1:1)
                      ELSE
                          CALL NTOAN2(ALENS(14,SURF),AAL(1:10))
                          LINE=LINE(1:57)//AAL(1:10)//SPCE(1:1)
                      END IF
                      IF(ALENS(15,SURF).EQ.0.0D0) THEN
                          LINE=LINE(1:68)//SP10(1:10)
                      ELSE
                          CALL NTOAN2(ALENS(15,SURF),AAL(1:10))
                          LINE=LINE(1:68)//AAL(1:10)
                      END IF
                      WRITE(OUTLYNE,200) LINE(1:79)
                      CALL SHOWIT(0)
                  END IF
                  IF(CLAPE.EQ.1) THEN
                      CALL NTOAN1(SURF,ASURF)
                      LINE=ASURF(1:3)//SPCE(1:1)//CLETYPE(1:8)//SPCE(1:1)
                      CALL NTOAN2(ALENS(52,SURF),AAL(1:10))
                      LINE=LINE(1:13)//AAL(1:10)//SPCE(1:1)
                      IF(ALENS(53,SURF).EQ.0.0D0) THEN
                          LINE=LINE(1:24)//SP10(1:10)//SPCE(1:1)
                      ELSE
                          CALL NTOAN2(ALENS(53,SURF),AAL(1:10))
                          LINE=LINE(1:24)//AAL(1:10)//SPCE(1:1)
                      END IF
                      IF(ALENS(54,SURF).EQ.0.0D0) THEN
                          LINE=LINE(1:35)//SP10(1:10)//SPCE(1:1)
                      ELSE
                          CALL NTOAN2(ALENS(54,SURF),AAL(1:10))
                          LINE=LINE(1:35)//AAL(1:10)//SPCE(1:1)
                      END IF
                      IF(ALENS(55,SURF).EQ.0.0D0) THEN
                          LINE=LINE(1:46)//SP10(1:10)//SPCE(1:1)
                      ELSE
                          CALL NTOAN2(ALENS(55,SURF),AAL(1:10))
                          LINE=LINE(1:46)//AAL(1:10)//SPCE(1:1)
                      END IF
                      IF(ALENS(56,SURF).EQ.0.0D0) THEN
                          LINE=LINE(1:57)//SP10(1:10)//SPCE(1:1)
                      ELSE
                          CALL NTOAN2(ALENS(56,SURF),AAL(1:10))
                          LINE=LINE(1:57)//AAL(1:10)//SPCE(1:1)
                      END IF
                      IF(ALENS(57,SURF).EQ.0.0D0) THEN
                          LINE=LINE(1:68)//SP10(1:10)
                      ELSE
                          CALL NTOAN2(ALENS(57,SURF),AAL(1:10))
                          LINE=LINE(1:68)//AAL(1:10)
                      END IF
                      WRITE(OUTLYNE,200) LINE(1:79)
                      CALL SHOWIT(0)
                  END IF
C       HANDEL COBS DATA
                  IF(COBS.EQ.1) THEN
                      CALL NTOAN1(SURF,ASURF)
                      LINE=ASURF(1:3)//SPCE(1:1)//COTYPE(1:8)//SPCE(1:1)
                      CALL NTOAN2(ALENS(17,SURF),AAL(1:10))
                      LINE=LINE(1:13)//AAL(1:10)//SPCE(1:1)
C
                      IF(ALENS(18,SURF).EQ.0.0D0) THEN
                          LINE=LINE(1:24)//SP10(1:10)//SPCE(1:1)
                      ELSE
                          CALL NTOAN2(ALENS(18,SURF),AAL(1:10))
                          LINE=LINE(1:24)//AAL(1:10)//SPCE(1:1)
                      END IF
C
                      IF(ALENS(19,SURF).EQ.0.0D0) THEN
                          LINE=LINE(1:35)//SP10(1:10)//SPCE(1:1)
                      ELSE
                          CALL NTOAN2(ALENS(19,SURF),AAL(1:10))
                          LINE=LINE(1:35)//AAL(1:10)//SPCE(1:1)
                      END IF
                      IF(ALENS(20,SURF).EQ.0.0D0) THEN
                          LINE=LINE(1:46)//SP10(1:10)//SPCE(1:1)
                      ELSE
                          CALL NTOAN2(ALENS(20,SURF),AAL(1:10))
                          LINE=LINE(1:46)//AAL(1:10)//SPCE(1:1)
                      END IF
                      IF(ALENS(21,SURF).EQ.0.0D0) THEN
                          LINE=LINE(1:57)//SP10(1:10)//SPCE(1:1)
                      ELSE
                          CALL NTOAN2(ALENS(21,SURF),AAL(1:10))
                          LINE=LINE(1:57)//AAL(1:10)//SPCE(1:1)
                      END IF
                      IF(ALENS(22,SURF).EQ.0.0D0) THEN
                          LINE=LINE(1:68)//SP10(1:10)
                      ELSE
                          CALL NTOAN2(ALENS(22,SURF),AAL(1:10))
                          LINE=LINE(1:68)//AAL(1:10)
                      END IF
                      WRITE(OUTLYNE,300) LINE(1:79)
                      CALL SHOWIT(0)
                  END IF
                  IF(COBSE.EQ.1) THEN
                      CALL NTOAN1(SURF,ASURF)
                      LINE=ASURF(1:3)//SPCE(1:1)//COTYPE(1:8)//SPCE(1:1)
                      CALL NTOAN2(ALENS(62,SURF),AAL(1:10))
                      LINE=LINE(1:13)//AAL(1:10)//SPCE(1:1)
C
                      IF(ALENS(63,SURF).EQ.0.0D0) THEN
                          LINE=LINE(1:24)//SP10(1:10)//SPCE(1:1)
                      ELSE
                          CALL NTOAN2(ALENS(63,SURF),AAL(1:10))
                          LINE=LINE(1:24)//AAL(1:10)//SPCE(1:1)
                      END IF
C
                      IF(ALENS(64,SURF).EQ.0.0D0) THEN
                          LINE=LINE(1:35)//SP10(1:10)//SPCE(1:1)
                      ELSE
                          CALL NTOAN2(ALENS(64,SURF),AAL(1:10))
                          LINE=LINE(1:35)//AAL(1:10)//SPCE(1:1)
                      END IF
                      IF(ALENS(65,SURF).EQ.0.0D0) THEN
                          LINE=LINE(1:46)//SP10(1:10)//SPCE(1:1)
                      ELSE
                          CALL NTOAN2(ALENS(65,SURF),AAL(1:10))
                          LINE=LINE(1:46)//AAL(1:10)//SPCE(1:1)
                      END IF
                      IF(ALENS(66,SURF).EQ.0.0D0) THEN
                          LINE=LINE(1:57)//SP10(1:10)//SPCE(1:1)
                      ELSE
                          CALL NTOAN2(ALENS(66,SURF),AAL(1:10))
                          LINE=LINE(1:57)//AAL(1:10)//SPCE(1:1)
                      END IF
                      IF(ALENS(67,SURF).EQ.0.0D0) THEN
                          LINE=LINE(1:68)//SP10(1:10)
                      ELSE
                          CALL NTOAN2(ALENS(67,SURF),AAL(1:10))
                          LINE=LINE(1:68)//AAL(1:10)
                      END IF
                      WRITE(OUTLYNE,300) LINE(1:79)
                      CALL SHOWIT(0)
                  END IF
 5            CONTINUE
          END IF
          NOHEAD=.TRUE.
          DO SURF=1,INT(SYSTEM1(20))
              IF(ALENS(127,SURF).NE.0.0D0) THEN
                  NOHEAD=.FALSE.
              END IF
          END DO
          IF(.NOT.NOHEAD) THEN
              WRITE(OUTLYNE,2009)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,2010)
              CALL SHOWIT(0)
          END IF
          DO SURF=1,INT(SYSTEM1(20))
              IF(ALENS(127,SURF).NE.0.0D0) THEN
                  DO I=1,INT(ALENS(127,SURF))
                      WRITE(OUTLYNE,2008) SURF,I,MULTCLAP(I,1,SURF),MULTCLAP(I,2,SURF),
     1                MULTCLAP(I,3,SURF)
                      CALL SHOWIT(1)
                  END DO
              END IF
          END DO
          NOHEAD=.TRUE.
          DO SURF=1,INT(SYSTEM1(20))
              IF(ALENS(128,SURF).NE.0.0D0) THEN
                  NOHEAD=.FALSE.
              END IF
          END DO
          IF(.NOT.NOHEAD) THEN
              WRITE(OUTLYNE,3009)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,2010)
              CALL SHOWIT(0)
          END IF
          DO SURF=1,INT(SYSTEM1(20))
              IF(ALENS(128,SURF).NE.0.0D0) THEN
                  DO I=1,INT(ALENS(128,SURF))
                      WRITE(OUTLYNE,2008) SURF,I,MULTCOBS(I,1,SURF),MULTCOBS(I,2,SURF),
     1                MULTCOBS(I,3,SURF)
                      CALL SHOWIT(1)
                  END DO
              END IF
          END DO
C
 200      FORMAT(A79)
 300      FORMAT(A79)
C
 100      FORMAT('SURF',1X,A3,1X,
     1    ':NO CLEAR APERTURE OR OBSCURATION DATA')
 110      FORMAT('NO CLEAR APERTURE OR OBSCURATION DATA')
 1000     FORMAT('CLEAR APERTURES AND OBSCURATIONS')
 2000     FORMAT('SURF',4X,'TYPE',3X,'Y-SEMI.',4X,'X-SEMI.',
     1    4X,'Y-DEC',6X,'X-DEC',2X,'CORNER-RADIUS',1X,'TILT(DEG)')
 2009     FORMAT('MULTIPLE CLEAR APERTURE DEFINITIONS')
 3009     FORMAT('MULTIPLE OBSCURATION DEFINITIONS')
 2010     FORMAT('SURF',4X,'MULTI#',3X,'  X  ',8X,'   Y   ',
     1    9X,' GAMMA ')
 2001     FORMAT(14X,'(RADIUS)',3X,'(N-POLY)')
 3000     FORMAT(25X,'(RAD-FLT)',22X,'(DELTA-Z)')
 1500     FORMAT(1X)
          RETURN
      END
C SUB SCALLE.FOR
      SUBROUTINE SCALLE
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE SCALE WHICH IMPLEMENTS THE SC AND WSC
C       COMMANDS AT THE CMD LEVEL.
C
          CHARACTER DUMPY*140
C
          COMMON/LUMPY/DUMPY
C
          INTEGER OLDCFG,I
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          INCLUDE 'datcfg.inc'
C
C       RESET NEWOBJ,NEWREF AND NEWIMG
          CALL RESSUR
C
C       WHAT WAS CURRENT CONFIG NUMBER
          OLDCFG=F12
          IF(OLDCFG.NE.1) THEN
C       CHANGE TO CONFIG 1
              F12=1
              CALL PTOC
C************************************************************
C               FINISHED
C       COPYING PERM LENS TO CURRENT LENS
          ELSE
C       ALREADY AT CFG 1
          END IF
C
C               CHECK FOR ADDITIONAL INPUT AND
C               PRINT ERROR AND RETURN IF DISCOVERED.
C
          IF(SST.EQ.1) THEN
C
              IF(WC.EQ.'SC') THEN
                  OUTLYNE='"SC" ACCEPTS NO STRING INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(WC.EQ.'WSC') THEN
                  OUTLYNE='"WSC" ACCEPTS NO STRING INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
          IF(WC.EQ.'SC'.AND.SQ.EQ.0) THEN
**************************************************
C       SIMPLE SCALE FACTOR
C
              IF(W1.EQ.0.0D0) THEN
                  OUTLYNE=
     1            'ZERO FOR THE SCALE FACTOR IS NOT ALLOWED'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C       DEFAULTS FOR I AND J IN THE INPUT LINE.
C       FINITE CONJUGATE
              IF(DF1.EQ.1) W1=1.0D0
              IF(DF2.EQ.1) W2=NEWOBJ
              IF(DF3.EQ.1) W3=NEWIMG
              IF(DF4.EQ.0.OR.DF5.EQ.0) THEN
                  OUTLYNE=
     1            '"SC" TAKES NO NUMERIC WORD #4 OR #5 INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(W2.LT.NEWOBJ) THEN
                  OUTLYNE='STARTING SURFACE BEYOND LEGAL RANGE'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(W3.GT.NEWIMG) THEN
                  OUTLYNE='ENDING SURFACE BEYOND LEGAL RANGE'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(W2.GE.W3) THEN
                  OUTLYNE=
     1            'STARTING AND ENDING SURFACE NUMBERS OUT OF ORDER'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C       PERFORM SCALING
              CALL SCALEA1
C       STRAIGHT SCALING SCALES LENS SURFACE DATA AND
C       SYSTEM DATA.
          ELSE
C       DON'T SCALE
          END IF
**********************************************************
          IF(WC.EQ.'WSC'.AND.SQ.EQ.0) THEN
**********************************************************
C       SIMPLE WORKING SCALE FACTOR
              IF(W1.EQ.0.0D0) THEN
                  OUTLYNE=
     1            'ZERO FOR THE SCALE FACTOR IS NOT ALLOWED'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C       DEFAULTS FOR I AND J IN THE INPUT LINE.
C       FINITE CONJUGATE
              IF(DF1.EQ.1) W1=1.0D0
              IF(DF2.EQ.1) W2=NEWOBJ
              IF(DF3.EQ.1) W3=NEWIMG
              IF(DF4.EQ.0.OR.DF5.EQ.0) THEN
                  OUTLYNE=
     1            '"WSC" TAKES NO NUMERIC WORD #4 OR #5 INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(W2.LT.NEWOBJ) THEN
                  OUTLYNE='STARTING SURFACE BEYOND LEGAL RANGE'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(W3.GT.NEWIMG) THEN
                  OUTLYNE='ENDING SURFACE BEYOND LEGAL RANGE'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(W2.GE.W3) THEN
                  OUTLYNE=
     1            'STARTING AND ENDING SURFACE NUMBERS OUT OF ORDER'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C       PERFORM SCALING
              CALL SCALEA2
          END IF
******************************************************
C
          IF(WC.EQ.'SC'.AND.SQ.EQ.1) THEN
              IF(WQ.NE.'FY') THEN
******************************************************
                  OUTLYNE='INVALID QUALIFIER USED WITH "SC"'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              ELSE
              END IF
C       WQ MUST BE "FY", SCALE EFL
C
              IF(W1.EQ.0.0D0) THEN
                  OUTLYNE=
     1            'ZERO FOR THE SCALE FACTOR IS NOT ALLOWED'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C       DEFAULTS FOR I AND J IN THE INPUT LINE.
C       FINITE CONJUGATE
              IF(DF1.EQ.1) W1=1.0D0
              IF(DF2.EQ.1) W2=NEWOBJ
              IF(DF3.EQ.1) W3=NEWIMG
              IF(DF4.EQ.0.OR.DF5.EQ.0) THEN
                  OUTLYNE=
     1            '"SC FY" TAKES NO NUMERIC WORD #4 OR #5 INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(W2.LT.NEWOBJ) THEN
                  OUTLYNE='STARTING SURFACE BEYOND LEGAL RANGE'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(W3.GT.NEWIMG) THEN
                  OUTLYNE='ENDING SURFACE BEYOND LEGAL RANGE'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(W2.GE.W3) THEN
                  OUTLYNE=
     1            'STARTING AND ENDING SURFACE NUMBERS OUT OF ORDER'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C       PERFORM SCALING
              CALL SCALEA3
          END IF
C
**********************************************************
          IF(WC.EQ.'WSC'.AND.SQ.EQ.1) THEN
********************************************************
              IF(WQ.NE.'FY') THEN
                  OUTLYNE='INVALID QUALIFIER USED WITH "WSC"'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C       WQ MUST BE "FY", (WORKING) SCALE EFL
C
              IF(W1.EQ.0.0D0) THEN
                  OUTLYNE=
     1            'ZERO FOR THE SCALE FACTOR IS NOT ALLOWED'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C       DEFAULTS FOR I AND J IN THE INPUT LINE.
C       FINITE CONJUGATE
              IF(DF1.EQ.1) W1=1.0D0
              IF(DF2.EQ.1) W2=NEWOBJ
              IF(DF3.EQ.1) W3=NEWIMG
              IF(DF4.EQ.0.OR.DF5.EQ.0) THEN
                  OUTLYNE=
     1            '"WSC FY" TAKES NO NUMERIC WORD #4 OR #5 INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(W2.LT.NEWOBJ) THEN
                  OUTLYNE='STARTING SURFACE BEYOND LEGAL RANGE'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(W3.GT.NEWIMG) THEN
                  OUTLYNE='ENDING SURFACE BEYOND LEGAL RANGE'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(W2.GE.W3) THEN
                  OUTLYNE=
     1            'STARTING AND ENDING SURFACE NUMBERS OUT OF ORDER'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C       PERFORM SCALING
              CALL SCALEA4
          END IF
C
C       COPY CURRENT LENS TO PERM LENS SINCE CHANGES WERE MADE
C       TO CFG 1
C
          CALL CTOP
C************************************************************
C               PERMANENT LENS UPDATED
C       NOW RETURN TO CONFIG OLDCFG
          IF(OLDCFG.NE.1) THEN
              F12=OLDCFG
C
C       NOW THE MAGIC. FOR THE PARTICULAR CFG
C       DESIGNATED BY THE CURRENT VALE OF FLAG F12.
C       DURING THIS PROCESS, FLAG F15 IS SET TO 1
C       TO HELP THE UPDATE LENS AND UPDATE SPSRF
C       LEVELS KNOW HOW TO PROERLY RESPOND
              F15=1
C       READ THE CONTENTS OF
C       CONFG(F12,I)
C        FROM I=1 TO I=CFGCNT(F12)
C       AND ONE BY ONE RUN THE STRING VALUES FOUND BACK
C       THROUGH THE PROCESS SUBROUTINE VIA CFCHG1
              DO 14 I=1,CFGCNT(F12)
                  EE12=CONFG(F12,I)
                  HOLDER=EE12
                  DUMPY=HOLDER(1:140)
                  CALL CFCHG1
 14           CONTINUE
C
C       PROCESS DONE, SET F15 TO 0
              F15=0
          ELSE
C       OLD CONFIG WAS 1, NO NEED TO RETURN TO ANOTHER
          END IF
C NOTE: ALL SCALE CHANGES WERE MADE TO CONFIG DATA
C       FROM CALLS FROM SCALE1 AND SCALE3 TO SUBROUTINE CFSC1
C       AND FROM SCALE2 AND SCALE4 (WORKING SCALE) TO SUBROUTINE
C       CFSC2.
          RETURN
      END
C SUB SCALEA1.FOR
      SUBROUTINE SCALEA1
C
          IMPLICIT NONE
C
C     THIS DOES SC WITHOUT FY
C
          INTEGER I,J,J1,J2
C
          REAL*8 SCW1,M1
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
C
C       STRAIGHT SCALING (SC,FACT,I,J)
C       W1=FACTOR
C       W2=STARTING SURFACE
C       W3=ENDING SURFACE
C       SAY,SAX,SCY,SCX AND WRX,WRY,BDX,BDY STARTING VALUES ALSO SCALED
C       FIRST OPERATE ON THE CURRENT LENS
C
C       SCALE THE NON-SURFACE SPECIFIC DATA
C
C       SAY,SAX,SCY,SCX,SCY FANG,SCX FANG
C       ANY CONFLICT WITH SCY AND SCY FANG WILL BE RESOLVED
C       AT THE END OF SCALING WHEN LNSEOS IS CALLED
C
          IF(SYSTEM1(64).EQ.0.0D0.AND.SYSTEM1(67).EQ.0.0D0) THEN
              SYSTEM1(12)=SYSTEM1(12)*W1
              SYSTEM1(13)=SYSTEM1(13)*W1
          END IF
          SYSTEM1(14)=SYSTEM1(14)*W1
          SYSTEM1(15)=SYSTEM1(15)*W1
          SYSTEM1(16)=SYSTEM1(16)*W1
          SYSTEM1(17)=SYSTEM1(17)*W1
C     FIELD ANGLES DO NOT GET SCALED !
          SYSTEM1(21)=SYSTEM1(21)
          SYSTEM1(23)=SYSTEM1(23)
C
          SYSTEM1(22)=SYSTEM1(22)*W1
          SYSTEM1(24)=SYSTEM1(24)*W1
C
C       REMOVE FNBY HLD OR ER HOLD IF PRESENT
C
          IF(SYSTEM1(44).EQ.-1.0D0.OR.SYSTEM1(45).EQ.-1.0D0)THEN
              OUTLYNE='REMOVING F/# HOLD'
              CALL SHOWIT(1)
              SYSTEM1(44:47)=0.0D0
          END IF
          IF(SYSTEM1(44).EQ.-2.0D0.OR.SYSTEM1(45).EQ.-2.0D0)THEN
              OUTLYNE='REMOVING EXIT PUPIL RADIUS HOLD'
              CALL SHOWIT(1)
              SYSTEM1(44:47)=0.0D0
          END IF

C
C       NOW FOR SURFACE DATA
          J1=INT(W2)
          J2=INT(W3)
          DO I=J1,J2
C       CURVATURE
C       HERE SCALE FACTOR CHANGES THE RADIUS OF CURVATURE
C       FACTOR = 2 INCREASES RADIUS BY 2X OR REDUCES CURVATURE
C       BY FACTOR OF 2X.
C
              ALENS(1,I)=ALENS(1,I)*(1/W1)
C
C       CONIC CONSTANT DOES NOT CHANGE
C
C       THICKNESS
C
              ALENS(3,I)=ALENS(3,I)*W1
C
C       ROOF AND THICKNESS
C
              IF(ALENS(126,I).EQ.1.0D0) THEN
C     ROOF
                  ALENS(138,I)=ALENS(138,I)*W1
                  ALENS(140,I)=ALENS(140,I)*W1
              END IF
              IF(ALENS(126,I).EQ.1.0D0) THEN
C     CCR
                  ALENS(138,I)=ALENS(138,I)*W1
                  ALENS(142,I)=ALENS(142,I)*W1
              END IF
C
C       IDEAL LENS THICKNESS
C
              ALENS(121,I)=ALENS(121,I)*W1
C
C       FOURTH, SIXTH, EIGHTH AND TENTH ORDER ASPHERICS
C       FACTOR OF NX CHANGES THE FOURTH ORDER COEFFICIENT
C       FROM AD TO AD/(NX**3)
C       AND
C       AE TO AE/(NX**5) AND AF TO AF/(NX**7) AND
C       AG TO AG/(NX**9)
C
              ALENS(43,I)=ALENS(43,I)/W1
              ALENS(4,I)=ALENS(4,I)/(W1**3)
              ALENS(5,I)=ALENS(5,I)/(W1**5)
              ALENS(6,I)=ALENS(6,I)/(W1**7)
              ALENS(7,I)=ALENS(7,I)/(W1**9)
              ALENS(81,I)=ALENS(81,I)/(W1**11)
              ALENS(82,I)=ALENS(82,I)/(W1**13)
              ALENS(83,I)=ALENS(83,I)/(W1**15)
              ALENS(84,I)=ALENS(84,I)/(W1**17)
              ALENS(85,I)=ALENS(85,I)/(W1**19)
C
C       NOW THE CLEAR APERTURES AND OBSCURATIONS
C
              M1=DABS(W1)
C     CLAP MULT SCALING
              IF(ALENS(127,I).NE.0.0D0) THEN
                  DO J=1,INT(ALENS(127,I))
                      MULTCLAP(J,1,I)=MULTCLAP(J,1,I)*M1
                      MULTCLAP(J,2,I)=MULTCLAP(J,2,I)*M1
                  END DO
              END IF
C     COBS MULT SCALING
              IF(ALENS(128,I).NE.0.0D0) THEN
                  DO J=1,INT(ALENS(128,I))
                      MULTCOBS(J,1,I)=MULTCOBS(J,1,I)*M1
                      MULTCOBS(J,2,I)=MULTCOBS(J,2,I)*M1
                  END DO
              END IF
C     SPIDER SCALING
              IF(ALENS(134,I).NE.0.0D0) THEN
                  ALENS(136,I)=ALENS(136,I)*M1
                  ALENS(137,I)=ALENS(137,I)*M1
              END IF
C       CIRCULAR
              IF(DABS(ALENS(9,I)).EQ.1.0D0) THEN
                  ALENS(10,I)=ALENS(10,I)*M1
                  ALENS(11,I)=ALENS(11,I)*M1
                  ALENS(12,I)=ALENS(12,I)*M1
                  ALENS(13,I)=ALENS(13,I)*M1
                  ALENS(52,I)=ALENS(52,I)*M1
                  ALENS(54,I)=ALENS(54,I)*M1
                  ALENS(55,I)=ALENS(55,I)*M1
              ELSE
              END IF
              IF(DABS(ALENS(16,I)).EQ.1.0D0) THEN
                  ALENS(17,I)=ALENS(17,I)*M1
                  ALENS(18,I)=ALENS(17,I)*M1
                  ALENS(19,I)=ALENS(19,I)*M1
                  ALENS(20,I)=ALENS(20,I)*M1
                  ALENS(62,I)=ALENS(62,I)*M1
                  ALENS(64,I)=ALENS(64,I)*M1
                  ALENS(65,I)=ALENS(65,I)*M1
              ELSE
              END IF
C       ELLIPTICAL
              IF(DABS(ALENS(9,I)).EQ.3.0D0) THEN
                  ALENS(10,I)=ALENS(10,I)*M1
                  ALENS(11,I)=ALENS(11,I)*M1
                  ALENS(12,I)=ALENS(12,I)*M1
                  ALENS(13,I)=ALENS(13,I)*M1
                  ALENS(52,I)=ALENS(52,I)*M1
                  ALENS(53,I)=ALENS(53,I)*M1
                  ALENS(54,I)=ALENS(54,I)*M1
                  ALENS(55,I)=ALENS(55,I)*M1
              ELSE
              END IF
              IF(DABS(ALENS(16,I)).EQ.3.0D0) THEN
                  ALENS(17,I)=ALENS(17,I)*M1
                  ALENS(18,I)=ALENS(18,I)*M1
                  ALENS(19,I)=ALENS(19,I)*M1
                  ALENS(20,I)=ALENS(20,I)*M1
                  ALENS(62,I)=ALENS(62,I)*M1
                  ALENS(63,I)=ALENS(63,I)*M1
                  ALENS(64,I)=ALENS(64,I)*M1
                  ALENS(65,I)=ALENS(65,I)*M1
              ELSE
              END IF
C       RECTANGULAR
              IF(DABS(ALENS(9,I)).EQ.2.0D0) THEN
                  ALENS(10,I)=ALENS(10,I)*M1
                  ALENS(11,I)=ALENS(11,I)*M1
                  ALENS(12,I)=ALENS(12,I)*M1
                  ALENS(13,I)=ALENS(13,I)*M1
                  ALENS(52,I)=ALENS(52,I)*M1
                  ALENS(53,I)=ALENS(53,I)*M1
                  ALENS(54,I)=ALENS(54,I)*M1
                  ALENS(55,I)=ALENS(55,I)*M1
              ELSE
              END IF
              IF(DABS(ALENS(16,I)).EQ.2.0D0) THEN
                  ALENS(17,I)=ALENS(17,I)*M1
                  ALENS(18,I)=ALENS(18,I)*M1
                  ALENS(19,I)=ALENS(19,I)*M1
                  ALENS(20,I)=ALENS(20,I)*M1
                  ALENS(62,I)=ALENS(62,I)*M1
                  ALENS(63,I)=ALENS(63,I)*M1
                  ALENS(64,I)=ALENS(64,I)*M1
                  ALENS(65,I)=ALENS(65,I)*M1
              ELSE
              END IF
C
C       RACETRACK
              IF(DABS(ALENS(9,I)).EQ.4.0D0) THEN
                  ALENS(10,I)=ALENS(10,I)*M1
                  ALENS(11,I)=ALENS(11,I)*M1
                  ALENS(12,I)=ALENS(12,I)*M1
                  ALENS(13,I)=ALENS(13,I)*M1
                  ALENS(14,I)=ALENS(14,I)*M1
                  ALENS(52,I)=ALENS(52,I)*M1
                  ALENS(53,I)=ALENS(53,I)*W1
                  ALENS(54,I)=ALENS(54,I)*M1
                  ALENS(55,I)=ALENS(55,I)*M1
                  ALENS(56,I)=ALENS(56,I)*M1
              ELSE
              END IF
              IF(DABS(ALENS(16,I)).EQ.4.0D0) THEN
                  ALENS(17,I)=ALENS(17,I)*M1
                  ALENS(18,I)=ALENS(18,I)*M1
                  ALENS(19,I)=ALENS(19,I)*M1
                  ALENS(20,I)=ALENS(20,I)*M1
                  ALENS(21,I)=ALENS(21,I)*M1
                  ALENS(62,I)=ALENS(62,I)*M1
                  ALENS(63,I)=ALENS(63,I)*M1
                  ALENS(64,I)=ALENS(64,I)*M1
                  ALENS(65,I)=ALENS(65,I)*M1
                  ALENS(66,I)=ALENS(66,I)*M1
              ELSE
              END IF
C
C       POLY
              IF(DABS(ALENS(9,I)).EQ.5.0D0) THEN
                  ALENS(10,I)=ALENS(10,I)*M1
                  ALENS(11,I)=ALENS(11,I)
                  ALENS(12,I)=ALENS(12,I)*M1
                  ALENS(13,I)=ALENS(13,I)*M1
                  ALENS(14,I)=ALENS(14,I)*M1
                  ALENS(52,I)=ALENS(52,I)*M1
                  ALENS(53,I)=ALENS(53,I)
                  ALENS(54,I)=ALENS(54,I)*M1
                  ALENS(55,I)=ALENS(55,I)*M1
                  ALENS(56,I)=ALENS(56,I)*M1
              ELSE
              END IF
              IF(DABS(ALENS(16,I)).EQ.5.0D0) THEN
                  ALENS(17,I)=ALENS(17,I)*M1
                  ALENS(18,I)=ALENS(18,I)
                  ALENS(19,I)=ALENS(19,I)*M1
                  ALENS(20,I)=ALENS(20,I)*M1
                  ALENS(21,I)=ALENS(21,I)*M1
                  ALENS(62,I)=ALENS(62,I)*M1
                  ALENS(63,I)=ALENS(63,I)
                  ALENS(64,I)=ALENS(64,I)*M1
                  ALENS(65,I)=ALENS(65,I)*M1
                  ALENS(66,I)=ALENS(66,I)*M1
              ELSE
              END IF
C
C       IPOLY
              IF(DABS(ALENS(9,I)).EQ.6.0D0) THEN
                  ALENS(12,I)=ALENS(12,I)*M1
                  ALENS(13,I)=ALENS(13,I)*M1
                  ALENS(13,I)=ALENS(14,I)*M1
                  ALENS(53,I)=ALENS(54,I)*M1
                  ALENS(54,I)=ALENS(55,I)*M1
                  ALENS(55,I)=ALENS(56,I)*M1
              ELSE
              END IF
              IF(DABS(ALENS(16,I)).EQ.6.0D0) THEN
                  ALENS(18,I)=ALENS(19,I)*M1
                  ALENS(19,I)=ALENS(20,I)*M1
                  ALENS(20,I)=ALENS(21,I)*M1
                  ALENS(63,I)=ALENS(64,I)*M1
                  ALENS(64,I)=ALENS(65,I)*M1
                  ALENS(65,I)=ALENS(66,I)*M1
              ELSE
              END IF
C
C       ALL CLAPS/COBS HAVE BEEN SCALED
C
C       TORIC CURVATURE
C
              ALENS(24,I)=ALENS(24,I)/(W1)
C
C       X,Y AND Z DECENTERS
C
              ALENS(30,I)=ALENS(30,I)*DABS(W1)
              ALENS(31,I)=ALENS(31,I)*DABS(W1)
              ALENS(69,I)=ALENS(69,I)*W1
              ALENS(114,I)=ALENS(114,I)*DABS(W1)
              ALENS(115,I)=ALENS(115,I)*DABS(W1)
              ALENS(116,I)=ALENS(116,I)*W1
              ALENS(90,I)=ALENS(90,I)*DABS(W1)
              ALENS(91,I)=ALENS(91,I)*DABS(W1)
              ALENS(92,I)=ALENS(92,I)*W1
              IF(W1.LT.0.0D0) ALENS(26,I)=-ALENS(26,I)
              IF(W1.LT.0.0D0) ALENS(27,I)=-ALENS(27,I)
              IF(W1.LT.0.0D0) ALENS(28,I)=-ALENS(28,I)
              IF(W1.LT.0.0D0) ALENS(118,I)=-ALENS(118,I)
              IF(W1.LT.0.0D0) ALENS(119,I)=-ALENS(119,I)
              IF(W1.LT.0.0D0) ALENS(120,I)=-ALENS(120,I)
              IF(W1.LT.0.0D0) ALENS(93,I)=-ALENS(93,I)
              IF(W1.LT.0.0D0) ALENS(94,I)=-ALENS(94,I)
              IF(W1.LT.0.0D0) ALENS(95,I)=-ALENS(95,I)

C       X,Y AND Z PIVOTS
C
              ALENS(78,I)=ALENS(78,I)*DABS(W1)
              ALENS(79,I)=ALENS(79,I)*DABS(W1)
              ALENS(80,I)=ALENS(80,I)*W1
C
C       ASPHERIC TORIC COEFFICIENTS
C
              ALENS(37,I)=ALENS(37,I)/(W1**3)
              ALENS(38,I)=ALENS(38,I)/(W1**5)
              ALENS(39,I)=ALENS(39,I)/(W1**7)
              ALENS(40,I)=ALENS(40,I)/(W1**9)
C
C       ALL THE SYSTEM AND ALENS VALUES THAT NEEDED
C       SCALING HAVE BEEN SCALED.
C
C       NOW SOLVE TARGET VALUES.
C
C       ANGULAR TARGETS ARE NOT SCALED
C       DIMENSIONAL TARGETS ARE SCALED
C
              IF(SOLVE(6,I).EQ.1.0D0.OR.SOLVE(6,I)
     1        .EQ.2.0D0.OR.SOLVE(6,I).EQ.3.0D0) THEN
                  SOLVE(7,I)=SOLVE(7,I)*W1
              ELSE
              END IF
              IF(SOLVE(4,I).EQ.4.0.OR.SOLVE(4,I)
     1        .EQ.5.0D0.OR.SOLVE(4,I).EQ.6.0D0) THEN
                  SOLVE(3,I)=SOLVE(3,I)*W1
              ELSE
              END IF
C
C       ALL SOLVE SCALING IS COMPLETE
C
C       PIKUP SCALING
C       IN PIKUP SCALING, THE ONLY VALUE SCALED IS THE
C       ADDITIVE CONSTANT IF NUMERIC WORD 4 IS NOT 1.0
C       RD
              PIKUP(4,I,1)=PIKUP(4,I,1)*W1
C       CV
              PIKUP(4,I,2)=PIKUP(4,I,2)*W1
C       TH
              PIKUP(4,I,3)=PIKUP(4,I,3)*W1
C       THOAL (THE FIFTH TERM IS SCALED CAUSE THERE WERE 2 SURFACES ENTERED)
              PIKUP(5,I,32)=PIKUP(5,I,32)*W1
C       CC
              PIKUP(4,I,4)=PIKUP(4,I,4)*W1
C       AD
              PIKUP(4,I,5)=PIKUP(4,I,5)/(W1**3)
C       AE
              PIKUP(4,I,6)=PIKUP(4,I,6)/(W1**5)
C       AF
              PIKUP(4,I,7)=PIKUP(4,I,7)/(W1**7)
C       AG
              PIKUP(4,I,8)=PIKUP(4,I,8)/(W1**9)
C       CVTOR
              PIKUP(4,I,9)=PIKUP(4,I,9)*W1
C       RDTOR
              PIKUP(4,I,10)=PIKUP(4,I,10)*W1
C       YD
              PIKUP(4,I,13)=PIKUP(4,I,13)*W1
C       XD
              PIKUP(4,I,14)=PIKUP(4,I,14)*W1
C       ALPHA
              PIKUP(4,I,15)=PIKUP(4,I,15)*W1
C       BETA
              PIKUP(4,I,16)=PIKUP(4,I,16)*W1
C       GAMMA
              PIKUP(4,I,17)=PIKUP(4,I,17)*W1
C       CLAP
              PIKUP(4,I,18)=PIKUP(4,I,18)*M1
C       COBS
              PIKUP(4,I,19)=PIKUP(4,I,19)*M1
C       CCTOR
              PIKUP(4,I,20)=PIKUP(4,I,20)*W1
C       ADTOR
              PIKUP(4,I,22)=PIKUP(4,I,22)/(W1**3)
C       AETOR
              PIKUP(4,I,23)=PIKUP(4,I,23)/(W1**5)
C       AFTOR
              PIKUP(4,I,24)=PIKUP(4,I,24)/(W1**7)
C       AGTOR
              PIKUP(4,I,25)=PIKUP(4,I,25)/(W1**9)
C       AC
              PIKUP(4,I,26)=PIKUP(4,I,26)/(W1)
C       AH
              PIKUP(4,I,27)=PIKUP(4,I,27)/(W1**11)
C       AI
              PIKUP(4,I,28)=PIKUP(4,I,28)/(W1**13)
C       AJ
              PIKUP(4,I,29)=PIKUP(4,I,29)/(W1**15)
C       AK
              PIKUP(4,I,30)=PIKUP(4,I,30)/(W1**17)
C       AL
              PIKUP(4,I,31)=PIKUP(4,I,31)/(W1**19)
C
C               NOW ALL THE PIKUPS ARE SCALED
C       PRO,NPRO AND GLASS DON'T SCALE!
          END DO
C
C       NOW MAKE CHANGES TO THE CONFIG DATA
C       WHICH IS APPROPRIATE FOR THE PARTICULAR
C       SCALING REQUESTED.
          CALL CFSC1
C       ALL RESCALING WAS PERFORMED.
C       RESCALING AFFECTS SPECIAL SURFACE DATA
C       AS OF 1/6/94
          SCW1=W1
          CALL SCASPC(SCW1)
          F1=0
          F6=1
          F22=0
          LNSTYP=1
          CALL LNSEOS
          RETURN
      END
C SUB SCALEA2.FOR
      SUBROUTINE SCALEA2
C
          IMPLICIT NONE
C
C     THIS DOES WSC WITHOUT FY
C
          INTEGER I,J,J1,J2
C
          REAL*8 SCW1,M1
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
C
C
C       STRAIGHT SCALING (SC,FACT,I,J)
C       WITHOUT SAY,SCY,SAX OR SCX AFFECTED
C       W1=FACTOR
C       W2=STARTING SURFACE
C       W3=ENDING SURFACE
C       SAY,SAX,SCY,SCX STARTING VALUES NOT SCALED
C       FIRST OPERATE ON THE CURRENT LENS
C
C
C       REMOVE FNBY HLD OR ER HOLD IF PRESENT
C
          IF(SYSTEM1(44).EQ.-1.0D0.OR.SYSTEM1(45).EQ.-1.0D0)THEN
              OUTLYNE='REMOVING F/# HOLD'
              CALL SHOWIT(1)
              SYSTEM1(44:47)=0.0D0
          END IF
          IF(SYSTEM1(44).EQ.-2.0D0.OR.SYSTEM1(45).EQ.-2.0D0)THEN
              OUTLYNE='REMOVING EXIT PUPIL RADIUS HOLD'
              CALL SHOWIT(1)
              SYSTEM1(44:47)=0.0D0
          END IF

C
C       NOW FOR SURFACE DATA
          J1=INT(W2)
          J2=INT(W3)
          DO I=J1,J2
C       CURVATURE
C       HERE SCALE FACTOR CHANGES THE RADIUS OF CURVATURE
C       FACTOR = 2 INCREASES RADIUS BY 2X OR REDUCES CURVATURE
C       BY FACTOR OF 2X.
C
              ALENS(1,I)=ALENS(1,I)*(1/W1)
C
C       CONIC CONSTANT DOES NOT CHANGE
C
C       THICKNESS
C
              ALENS(3,I)=ALENS(3,I)*W1
C
C       ROOF AND THICKNESS
C
              IF(ALENS(126,I).EQ.1.0D0) THEN
C     ROOF
                  ALENS(138,I)=ALENS(138,I)*W1
                  ALENS(140,I)=ALENS(140,I)*W1
              END IF
              IF(ALENS(126,I).EQ.1.0D0) THEN
C     CCR
                  ALENS(138,I)=ALENS(138,I)*W1
                  ALENS(142,I)=ALENS(142,I)*W1
              END IF
C
C       IDEAL LENS THICKNESS
C
              ALENS(121,I)=ALENS(121,I)*W1
C
C       FOURTH, SIXTH, EIGHTH AND TENTH ORDER ASPHERICS
C       FACTOR OF NX CHANGES THE FOURTH ORDER COEFFICIENT
C       FROM AD TO AD/(NX**3)
C       AND
C       AE TO AE/(NX**5) AND AF TO AF/(NX**7) AND
C       AG TO AG/(NX**9)
C
              ALENS(43,I)=ALENS(43,I)/W1
              ALENS(4,I)=ALENS(4,I)/(W1**3)
              ALENS(5,I)=ALENS(5,I)/(W1**5)
              ALENS(6,I)=ALENS(6,I)/(W1**7)
              ALENS(7,I)=ALENS(7,I)/(W1**9)
              ALENS(81,I)=ALENS(81,I)/(W1**11)
              ALENS(82,I)=ALENS(82,I)/(W1**13)
              ALENS(83,I)=ALENS(83,I)/(W1**15)
              ALENS(84,I)=ALENS(84,I)/(W1**17)
              ALENS(85,I)=ALENS(85,I)/(W1**19)
C
C       NOW THE CLEAR APERTURES AND OBSCURATIONS
              M1=DABS(W1)
C
C     CLAP MULT SCALING
              IF(ALENS(127,I).NE.0.0D0) THEN
                  DO J=1,INT(ALENS(127,I))
                      MULTCLAP(J,1,I)=MULTCLAP(J,1,I)*M1
                      MULTCLAP(J,2,I)=MULTCLAP(J,2,I)*M1
                  END DO
              END IF
C     COBS MULT SCALING
              IF(ALENS(128,I).NE.0.0D0) THEN
                  DO J=1,INT(ALENS(128,I))
                      MULTCOBS(J,1,I)=MULTCOBS(J,1,I)*M1
                      MULTCOBS(J,2,I)=MULTCOBS(J,2,I)*M1
                  END DO
              END IF
C     SPIDER SCALING
              IF(ALENS(134,I).NE.0.0D0) THEN
                  ALENS(136,I)=ALENS(136,I)*M1
                  ALENS(137,I)=ALENS(137,I)*M1
              END IF
C       CIRCULAR
              IF(DABS(ALENS(9,I)).EQ.1.0D0) THEN
                  ALENS(10,I)=ALENS(10,I)*M1
                  ALENS(11,I)=ALENS(11,I)*M1
                  ALENS(12,I)=ALENS(12,I)*M1
                  ALENS(13,I)=ALENS(13,I)*M1
                  ALENS(52,I)=ALENS(52,I)*M1
                  ALENS(54,I)=ALENS(54,I)*M1
                  ALENS(55,I)=ALENS(55,I)*M1
              ELSE
              END IF
              IF(DABS(ALENS(16,I)).EQ.1.0D0) THEN
                  ALENS(17,I)=ALENS(17,I)*M1
                  ALENS(18,I)=ALENS(17,I)*M1
                  ALENS(19,I)=ALENS(19,I)*M1
                  ALENS(20,I)=ALENS(20,I)*M1
                  ALENS(62,I)=ALENS(62,I)*M1
                  ALENS(64,I)=ALENS(64,I)*M1
                  ALENS(65,I)=ALENS(65,I)*M1
              ELSE
              END IF
C       RECTANGULAR
              IF(DABS(ALENS(9,I)).EQ.2.0D0) THEN
                  ALENS(10,I)=ALENS(10,I)*M1
                  ALENS(11,I)=ALENS(11,I)*M1
                  ALENS(12,I)=ALENS(12,I)*M1
                  ALENS(13,I)=ALENS(13,I)*M1
                  ALENS(52,I)=ALENS(52,I)*M1
                  ALENS(53,I)=ALENS(53,I)*M1
                  ALENS(54,I)=ALENS(54,I)*M1
                  ALENS(55,I)=ALENS(55,I)*M1
              ELSE
              END IF
              IF(DABS(ALENS(16,I)).EQ.2.0D0) THEN
                  ALENS(17,I)=ALENS(17,I)*M1
                  ALENS(18,I)=ALENS(18,I)*M1
                  ALENS(19,I)=ALENS(19,I)*M1
                  ALENS(20,I)=ALENS(20,I)*M1
                  ALENS(62,I)=ALENS(62,I)*M1
                  ALENS(63,I)=ALENS(63,I)*M1
                  ALENS(64,I)=ALENS(64,I)*M1
                  ALENS(65,I)=ALENS(65,I)*M1
              ELSE
              END IF
C       ELLIPTICAL
              IF(DABS(ALENS(9,I)).EQ.3.0D0) THEN
                  ALENS(10,I)=ALENS(10,I)*M1
                  ALENS(11,I)=ALENS(11,I)*M1
                  ALENS(12,I)=ALENS(12,I)*M1
                  ALENS(13,I)=ALENS(13,I)*M1
                  ALENS(52,I)=ALENS(52,I)*M1
                  ALENS(53,I)=ALENS(53,I)*M1
                  ALENS(54,I)=ALENS(54,I)*M1
                  ALENS(55,I)=ALENS(55,I)*M1
              ELSE
              END IF
              IF(DABS(ALENS(16,I)).EQ.3.0D0) THEN
                  ALENS(17,I)=ALENS(17,I)*M1
                  ALENS(18,I)=ALENS(18,I)*M1
                  ALENS(19,I)=ALENS(19,I)*M1
                  ALENS(20,I)=ALENS(20,I)*M1
                  ALENS(62,I)=ALENS(62,I)*M1
                  ALENS(63,I)=ALENS(63,I)*M1
                  ALENS(64,I)=ALENS(64,I)*M1
                  ALENS(65,I)=ALENS(65,I)*M1
              ELSE
              END IF
C
C       RACETRACK
              IF(DABS(ALENS(9,I)).EQ.4.0D0) THEN
                  ALENS(10,I)=ALENS(10,I)*M1
                  ALENS(11,I)=ALENS(11,I)*M1
                  ALENS(12,I)=ALENS(12,I)*M1
                  ALENS(13,I)=ALENS(13,I)*M1
                  ALENS(14,I)=ALENS(14,I)*M1
                  ALENS(52,I)=ALENS(52,I)*M1
                  ALENS(53,I)=ALENS(53,I)*M1
                  ALENS(54,I)=ALENS(54,I)*M1
                  ALENS(55,I)=ALENS(55,I)*M1
                  ALENS(56,I)=ALENS(56,I)*M1
              ELSE
              END IF
              IF(DABS(ALENS(16,I)).EQ.4.0D0) THEN
                  ALENS(17,I)=ALENS(17,I)*M1
                  ALENS(18,I)=ALENS(18,I)*M1
                  ALENS(19,I)=ALENS(19,I)*M1
                  ALENS(20,I)=ALENS(20,I)*M1
                  ALENS(21,I)=ALENS(21,I)*M1
                  ALENS(62,I)=ALENS(62,I)*M1
                  ALENS(63,I)=ALENS(63,I)*M1
                  ALENS(64,I)=ALENS(64,I)*M1
                  ALENS(65,I)=ALENS(65,I)*M1
                  ALENS(66,I)=ALENS(66,I)*M1
              ELSE
              END IF
C
C       POLY
              IF(DABS(ALENS(9,I)).EQ.5.0D0) THEN
                  ALENS(10,I)=ALENS(10,I)*M1
                  ALENS(11,I)=ALENS(11,I)
                  ALENS(12,I)=ALENS(12,I)*M1
                  ALENS(13,I)=ALENS(13,I)*M1
                  ALENS(14,I)=ALENS(14,I)*M1
                  ALENS(52,I)=ALENS(52,I)*M1
                  ALENS(53,I)=ALENS(53,I)
                  ALENS(54,I)=ALENS(54,I)*M1
                  ALENS(55,I)=ALENS(55,I)*M1
                  ALENS(56,I)=ALENS(56,I)*M1
              ELSE
              END IF
              IF(DABS(ALENS(16,I)).EQ.5.0D0) THEN
                  ALENS(17,I)=ALENS(17,I)*M1
                  ALENS(18,I)=ALENS(18,I)
                  ALENS(19,I)=ALENS(19,I)*M1
                  ALENS(20,I)=ALENS(20,I)*M1
                  ALENS(21,I)=ALENS(21,I)*M1
                  ALENS(62,I)=ALENS(62,I)*M1
                  ALENS(63,I)=ALENS(63,I)
                  ALENS(64,I)=ALENS(64,I)*M1
                  ALENS(65,I)=ALENS(65,I)*M1
                  ALENS(66,I)=ALENS(66,I)*M1
              ELSE
              END IF
C
C       IPOLY
              IF(DABS(ALENS(9,I)).EQ.6.0D0) THEN
                  ALENS(11,I)=ALENS(12,I)*M1
                  ALENS(12,I)=ALENS(13,I)*M1
                  ALENS(13,I)=ALENS(14,I)*M1
                  ALENS(53,I)=ALENS(54,I)*M1
                  ALENS(54,I)=ALENS(55,I)*M1
                  ALENS(55,I)=ALENS(56,I)*M1
              ELSE
              END IF
              IF(DABS(ALENS(16,I)).EQ.6.0D0) THEN
                  ALENS(18,I)=ALENS(19,I)*M1
                  ALENS(19,I)=ALENS(20,I)*M1
                  ALENS(20,I)=ALENS(21,I)*M1
                  ALENS(63,I)=ALENS(64,I)*M1
                  ALENS(64,I)=ALENS(65,I)*M1
                  ALENS(65,I)=ALENS(66,I)*M1
              ELSE
              END IF
C
C       ALL CLAPS/COBS HAVE BEEN SCALED
C
C       TORIC CURVATURE
C
              ALENS(24,I)=ALENS(24,I)/(W1)
C
C       X,Y AND Z DECENTERS
C
              ALENS(30,I)=ALENS(30,I)*DABS(W1)
              ALENS(31,I)=ALENS(31,I)*DABS(W1)
              ALENS(69,I)=ALENS(69,I)*W1
              ALENS(114,I)=ALENS(114,I)*DABS(W1)
              ALENS(115,I)=ALENS(115,I)*DABS(W1)
              ALENS(116,I)=ALENS(116,I)*W1
              ALENS(90,I)=ALENS(90,I)*DABS(W1)
              ALENS(91,I)=ALENS(91,I)*DABS(W1)
              ALENS(92,I)=ALENS(92,I)*W1
              IF(W1.LT.0.0D0) ALENS(26,I)=-ALENS(26,I)
              IF(W1.LT.0.0D0) ALENS(27,I)=-ALENS(27,I)
              IF(W1.LT.0.0D0) ALENS(28,I)=-ALENS(28,I)
              IF(W1.LT.0.0D0) ALENS(118,I)=-ALENS(118,I)
              IF(W1.LT.0.0D0) ALENS(119,I)=-ALENS(119,I)
              IF(W1.LT.0.0D0) ALENS(120,I)=-ALENS(120,I)
              IF(W1.LT.0.0D0) ALENS(93,I)=-ALENS(93,I)
              IF(W1.LT.0.0D0) ALENS(94,I)=-ALENS(94,I)
              IF(W1.LT.0.0D0) ALENS(95,I)=-ALENS(95,I)
C
C       X,Y AND Z PIVOTS
C
              ALENS(78,I)=ALENS(78,I)*DABS(W1)
              ALENS(79,I)=ALENS(79,I)*DABS(W1)
              ALENS(80,I)=ALENS(80,I)*W1
C
C       ASPHERIC TORIC COEFFICIENTS
C
              ALENS(37,I)=ALENS(37,I)/(W1**3)
              ALENS(38,I)=ALENS(38,I)/(W1**5)
              ALENS(39,I)=ALENS(39,I)/(W1**7)
              ALENS(40,I)=ALENS(40,I)/(W1**9)
C
C       ALL THE SYSTEM AND ALENS VALUES THAT NEEDED
C       SCALING HAVE BEEN SCALED.
C
C       NOW SOLVE TARGET VALUES.
C
C       ANGULAR TARGETS ARE NOT SCALED
C       DIMENSIONAL TARGETS ARE SCALED
C
              IF(SOLVE(6,I).EQ.1.0D0.OR.SOLVE(6,I)
     1        .EQ.2.0D0.OR.SOLVE(6,I).EQ.3.0D0) THEN
                  SOLVE(7,I)=SOLVE(7,I)*W1
              ELSE
              END IF
              IF(SOLVE(4,I).EQ.4.0D0.OR.SOLVE(4,I)
     1        .EQ.5.0D0.OR.SOLVE(4,I).EQ.6.0D0) THEN
                  SOLVE(3,I)=SOLVE(3,I)*W1
              ELSE
              END IF
C
C       ALL SOLVE SCALING IS COMPLETE
C
C       PIKUP SCALING
C       IN PIKUP SCALING, THE ONLY VALUE SCALED IS THE
C       ADDITIVE CONSTANT IF NUMERIC WORD 4 IS NOT 1.0
C       RD
              PIKUP(4,I,1)=PIKUP(4,I,1)*W1
C       CV
              PIKUP(4,I,2)=PIKUP(4,I,2)*W1
C       TH
              PIKUP(4,I,3)=PIKUP(4,I,3)*W1
C       THOAL (THE FIFTH TERM IS SCALED CAUSE THERE WERE 2 SURFACES ENTERED)
              PIKUP(5,I,32)=PIKUP(5,I,32)*W1
C       CC
              PIKUP(4,I,4)=PIKUP(4,I,4)*W1
C       AD
              PIKUP(4,I,5)=PIKUP(4,I,5)/(W1**3)
C       AE
              PIKUP(4,I,6)=PIKUP(4,I,6)/(W1**5)
C       AF
              PIKUP(4,I,7)=PIKUP(4,I,7)/(W1**7)
C       AG
              PIKUP(4,I,8)=PIKUP(4,I,8)/(W1**9)
C       CVTOR
              PIKUP(4,I,9)=PIKUP(4,I,9)*W1
C       RDTOR
              PIKUP(4,I,10)=PIKUP(4,I,10)*W1
C       YD
              PIKUP(4,I,13)=PIKUP(4,I,13)*W1
C       XD
              PIKUP(4,I,14)=PIKUP(4,I,14)*W1
C       ALPHA
              PIKUP(4,I,15)=PIKUP(4,I,15)*W1
C       BETA
              PIKUP(4,I,16)=PIKUP(4,I,16)*W1
C       GAMMA
              PIKUP(4,I,17)=PIKUP(4,I,17)*W1
C       CLAP
              PIKUP(4,I,18)=PIKUP(4,I,18)*M1
C       COBS
              PIKUP(4,I,19)=PIKUP(4,I,19)*M1
C       CCTOR
              PIKUP(4,I,20)=PIKUP(4,I,20)*W1
C       ADTOR
              PIKUP(4,I,22)=PIKUP(4,I,22)/(W1**3)
C       AETOR
              PIKUP(4,I,23)=PIKUP(4,I,23)/(W1**5)
C       AFTOR
              PIKUP(4,I,24)=PIKUP(4,I,24)/(W1**7)
C       AGTOR
              PIKUP(4,I,25)=PIKUP(4,I,25)/(W1**9)
C       AC
              PIKUP(4,I,26)=PIKUP(4,I,26)/(W1)
C       AH
              PIKUP(4,I,27)=PIKUP(4,I,27)/(W1**11)
C       AI
              PIKUP(4,I,28)=PIKUP(4,I,28)/(W1**13)
C       AJ
              PIKUP(4,I,29)=PIKUP(4,I,29)/(W1**15)
C       AK
              PIKUP(4,I,30)=PIKUP(4,I,30)/(W1**17)
C       AL
              PIKUP(4,I,31)=PIKUP(4,I,31)/(W1**19)
C
C               NOW ALL THE PIKUPS ARE SCALED
C       PRO,NPRO AND GLASS DON'T SCALE!
          END DO
C
C       NOW MAKE CHANGES TO THE CONFIG DATA
C       WHICH IS APPROPRIATE FOR THE PARTICULAR
C       SCALING REQUESTED.
          CALL CFSC2
C       ALL RESCALING WAS PERFORMED.
C       RESCALING AFFECTS SPECIAL SURFACE DATA
C       AS OF 1/6/94
          SCW1=W1
          CALL SCASPC(SCW1)
          F1=0
          F6=1
          F22=0
          LNSTYP=1
          CALL LNSEOS
          RETURN
      END
C SUB SCALEA3.FOR
      SUBROUTINE SCALEA3
C
          IMPLICIT NONE
C
C     THIS DOES SC WITH FY
C
          INTEGER I,J,J1,J2
C
          REAL*8 EFLY,SCW1,M1
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
C
C
C       SCALING TO A SPECIFIC EFL (SC EFL,EFL,I,J)
C       W1=DESIRED
C       W2=STARTING SURFACE
C       W3=ENDING SURFACE
C       SAY,SAX,SCY,SCX STARTING VALUES ALSO SCALED
C       FIRST OPERATE ON THE CURRENT LENS
C
C       WE MUST FIRST DETERMINE THE ORIGINAL EFL
C       OF THE ELEMENTS FROM SURFACE I TO J
C       THEN THE CORRECT SCALE FACTOR( USED FOR ALL CONFIGS BUT
C       DETERMINED FROM DATA IN CONFIG 1 IS GIVEN BY:
C
C       SCALE FACTOR= (NEW DESIRED EFL)/(ORIGINAL EFL)
C
C       YZ PLANE EFL CALCULATION
C
          IF(W2.GT.0.0D0) THEN
              I=INT(W2)-1
          ELSE
              I=INT(W2)
          END IF
          J=INT(W3)
          EFLY=-(((PXTRAY(2,I)*PXTRAY(5,I+1))-(PXTRAY(1,I+1)
     1    *PXTRAY(6,I)))/
     1    ((PXTRAY(2,I)*PXTRAY(6,J))-(PXTRAY(6,I)*PXTRAY(2,J))))
          IF(EFLY.EQ.0.0D0.OR.DABS(EFLY).GT.1.0D10) THEN
              OUTLYNE='NO EFL CAN BE CALCULATED FOR SPECIFIED SURFACES'
              CALL SHOWIT(1)
              OUTLYNE='NO EFL SCALING CAN BE PERFORMED'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
C       NEW SCALE FACTOR IS W1
C
          W1=W1/(EFLY)
C
C       SCALE THE NON-SURFACE SPECIFIC DATA
C
C       SAY,SAX,SCY,SCX,SCY FANG,SCX FANG
C       ANY CONFLICT WITH SCY AND SCY FANG WILL BE RESOLVED
C       AT THE END OF SCALING WHEN LNSEOS IS CALLED
C
          IF(SYSTEM1(64).EQ.0.0D0.AND.SYSTEM1(67).EQ.0.0D0) THEN
              SYSTEM1(12)=SYSTEM1(12)*W1
              SYSTEM1(13)=SYSTEM1(13)*W1
          END IF
          SYSTEM1(14)=SYSTEM1(14)*W1
          SYSTEM1(15)=SYSTEM1(15)*W1
          SYSTEM1(16)=SYSTEM1(16)*W1
          SYSTEM1(17)=SYSTEM1(17)*W1
C     FIELD ANGLES DO NOT GET SCALED !
          SYSTEM1(21)=SYSTEM1(21)
          SYSTEM1(23)=SYSTEM1(23)
C
          SYSTEM1(22)=SYSTEM1(22)*W1
          SYSTEM1(24)=SYSTEM1(24)*W1
C
C       REMOVE FNBY HLD OR ER HOLD IF PRESENT
C
          IF(SYSTEM1(44).EQ.-1.0D0.OR.SYSTEM1(45).EQ.-1.0D0)THEN
              OUTLYNE='REMOVING F/# HOLD'
              CALL SHOWIT(1)
              SYSTEM1(44:47)=0.0D0
          END IF
          IF(SYSTEM1(44).EQ.-2.0D0.OR.SYSTEM1(45).EQ.-2.0D0)THEN
              OUTLYNE='REMOVING EXIT PUPIL RADIUS HOLD'
              CALL SHOWIT(1)
              SYSTEM1(44:47)=0.0D0
          END IF

C
C       NOW FOR SURFACE DATA
          J1=INT(W2)
          J2=INT(W3)
          DO I=J1,J2
C       CURVATURE
C       HERE SCALE FACTOR CHANGES THE RADIUS OF CURVATURE
C       FACTOR = 2 INCREASES RADIUS BY 2X OR REDUCES CURVATURE
C       BY FACTOR OF 2X.
C
              ALENS(1,I)=ALENS(1,I)*(1/W1)
C
C       CONIC CONSTANT DOES NOT CHANGE
C
C       THICKNESS
C
              ALENS(3,I)=ALENS(3,I)*W1
C
C       ROOF AND THICKNESS
C
              IF(ALENS(126,I).EQ.1.0D0) THEN
C     ROOF
                  ALENS(138,I)=ALENS(138,I)*W1
                  ALENS(140,I)=ALENS(140,I)*W1
              END IF
              IF(ALENS(126,I).EQ.1.0D0) THEN
C     CCR
                  ALENS(138,I)=ALENS(138,I)*W1
                  ALENS(142,I)=ALENS(142,I)*W1
              END IF
C
C       IDEAL LENS THICKNESS
C
              ALENS(121,I)=ALENS(121,I)*W1
C
C       FOURTH, SIXTH, EIGHTH AND TENTH ORDER ASPHERICS
C       FACTOR OF NX CHANGES THE FOURTH ORDER COEFFICIENT
C       FROM AD TO AD/(NX**3)
C       AND
C       AE TO AE/(NX**5) AND AF TO AF/(NX**7) AND
C       AG TO AG/(NX**9)
C
              ALENS(43,I)=ALENS(43,I)/W1
              ALENS(4,I)=ALENS(4,I)/(W1**3)
              ALENS(5,I)=ALENS(5,I)/(W1**5)
              ALENS(6,I)=ALENS(6,I)/(W1**7)
              ALENS(7,I)=ALENS(7,I)/(W1**9)
              ALENS(81,I)=ALENS(81,I)/(W1**11)
              ALENS(82,I)=ALENS(82,I)/(W1**13)
              ALENS(83,I)=ALENS(83,I)/(W1**15)
              ALENS(84,I)=ALENS(84,I)/(W1**17)
              ALENS(85,I)=ALENS(85,I)/(W1**19)
C
C       NOW THE CLEAR APERTURES AND OBSCURATIONS
              M1=DABS(W1)
C
C     CLAP MULT SCALING
              IF(ALENS(127,I).NE.0.0D0) THEN
                  DO J=1,INT(ALENS(127,I))
                      MULTCLAP(J,1,I)=MULTCLAP(J,1,I)*M1
                      MULTCLAP(J,2,I)=MULTCLAP(J,2,I)*M1
                  END DO
              END IF
C     COBS MULT SCALING
              IF(ALENS(128,I).NE.0.0D0) THEN
                  DO J=1,INT(ALENS(128,I))
                      MULTCOBS(J,1,I)=MULTCOBS(J,1,I)*M1
                      MULTCOBS(J,2,I)=MULTCOBS(J,2,I)*M1
                  END DO
              END IF
C     SPIDER SCALING
              IF(ALENS(134,I).NE.0.0D0) THEN
                  ALENS(136,I)=ALENS(136,I)*M1
                  ALENS(137,I)=ALENS(137,I)*M1
              END IF
C       CIRCULAR
              IF(DABS(ALENS(9,I)).EQ.1.0D0) THEN
                  ALENS(10,I)=ALENS(10,I)*M1
                  ALENS(11,I)=ALENS(11,I)*M1
                  ALENS(12,I)=ALENS(12,I)*M1
                  ALENS(13,I)=ALENS(13,I)*M1
                  ALENS(52,I)=ALENS(52,I)*M1
                  ALENS(54,I)=ALENS(54,I)*M1
                  ALENS(55,I)=ALENS(55,I)*M1
              ELSE
              END IF
              IF(DABS(ALENS(16,I)).EQ.1.0D0) THEN
                  ALENS(17,I)=ALENS(17,I)*M1
                  ALENS(18,I)=ALENS(17,I)*M1
                  ALENS(19,I)=ALENS(19,I)*M1
                  ALENS(20,I)=ALENS(20,I)*M1
                  ALENS(62,I)=ALENS(62,I)*M1
                  ALENS(64,I)=ALENS(64,I)*M1
                  ALENS(65,I)=ALENS(65,I)*M1
              ELSE
              END IF
C       RECTANGULAR
              IF(DABS(ALENS(9,I)).EQ.2.0D0) THEN
                  ALENS(10,I)=ALENS(10,I)*M1
                  ALENS(11,I)=ALENS(11,I)*M1
                  ALENS(12,I)=ALENS(12,I)*M1
                  ALENS(13,I)=ALENS(13,I)*M1
                  ALENS(52,I)=ALENS(52,I)*M1
                  ALENS(53,I)=ALENS(53,I)*M1
                  ALENS(54,I)=ALENS(54,I)*M1
                  ALENS(55,I)=ALENS(55,I)*M1
              ELSE
              END IF
              IF(DABS(ALENS(16,I)).EQ.2.0D0) THEN
                  ALENS(17,I)=ALENS(17,I)*M1
                  ALENS(18,I)=ALENS(18,I)*M1
                  ALENS(19,I)=ALENS(19,I)*M1
                  ALENS(20,I)=ALENS(20,I)*M1
                  ALENS(62,I)=ALENS(62,I)*M1
                  ALENS(63,I)=ALENS(63,I)*M1
                  ALENS(64,I)=ALENS(64,I)*M1
                  ALENS(65,I)=ALENS(65,I)*M1
              ELSE
              END IF
C       ELLIPTICAL
              IF(DABS(ALENS(9,I)).EQ.3.0D0) THEN
                  ALENS(10,I)=ALENS(10,I)*M1
                  ALENS(11,I)=ALENS(11,I)*M1
                  ALENS(12,I)=ALENS(12,I)*M1
                  ALENS(13,I)=ALENS(13,I)*M1
                  ALENS(52,I)=ALENS(52,I)*M1
                  ALENS(53,I)=ALENS(53,I)*M1
                  ALENS(54,I)=ALENS(54,I)*M1
                  ALENS(55,I)=ALENS(55,I)*M1
              ELSE
              END IF
              IF(DABS(ALENS(16,I)).EQ.3.0D0) THEN
                  ALENS(17,I)=ALENS(17,I)*M1
                  ALENS(18,I)=ALENS(18,I)*M1
                  ALENS(19,I)=ALENS(19,I)*M1
                  ALENS(20,I)=ALENS(20,I)*M1
                  ALENS(62,I)=ALENS(62,I)*M1
                  ALENS(63,I)=ALENS(63,I)*M1
                  ALENS(64,I)=ALENS(64,I)*M1
                  ALENS(65,I)=ALENS(65,I)*M1
              ELSE
              END IF
C
C       RACETRACK
              IF(DABS(ALENS(9,I)).EQ.4.0D0) THEN
                  ALENS(10,I)=ALENS(10,I)*M1
                  ALENS(11,I)=ALENS(11,I)*M1
                  ALENS(12,I)=ALENS(12,I)*M1
                  ALENS(13,I)=ALENS(13,I)*M1
                  ALENS(14,I)=ALENS(14,I)*M1
                  ALENS(52,I)=ALENS(52,I)*M1
                  ALENS(53,I)=ALENS(53,I)*M1
                  ALENS(54,I)=ALENS(54,I)*M1
                  ALENS(55,I)=ALENS(55,I)*M1
                  ALENS(56,I)=ALENS(56,I)*M1
              ELSE
              END IF
              IF(DABS(ALENS(16,I)).EQ.4.0D0) THEN
                  ALENS(17,I)=ALENS(17,I)*M1
                  ALENS(18,I)=ALENS(18,I)*M1
                  ALENS(19,I)=ALENS(19,I)*M1
                  ALENS(20,I)=ALENS(20,I)*M1
                  ALENS(21,I)=ALENS(21,I)*M1
                  ALENS(62,I)=ALENS(62,I)*M1
                  ALENS(63,I)=ALENS(63,I)*M1
                  ALENS(64,I)=ALENS(64,I)*M1
                  ALENS(65,I)=ALENS(65,I)*M1
                  ALENS(66,I)=ALENS(66,I)*M1
              ELSE
              END IF
C
C       POLY
              IF(DABS(ALENS(9,I)).EQ.5.0D0) THEN
                  ALENS(10,I)=ALENS(10,I)*M1
                  ALENS(11,I)=ALENS(11,I)
                  ALENS(12,I)=ALENS(12,I)*M1
                  ALENS(13,I)=ALENS(13,I)*M1
                  ALENS(14,I)=ALENS(14,I)*M1
                  ALENS(52,I)=ALENS(52,I)*M1
                  ALENS(53,I)=ALENS(53,I)
                  ALENS(54,I)=ALENS(54,I)*M1
                  ALENS(55,I)=ALENS(55,I)*M1
                  ALENS(56,I)=ALENS(56,I)*M1
              ELSE
              END IF
              IF(DABS(ALENS(16,I)).EQ.5.0D0) THEN
                  ALENS(17,I)=ALENS(17,I)*M1
                  ALENS(18,I)=ALENS(18,I)
                  ALENS(19,I)=ALENS(19,I)*M1
                  ALENS(20,I)=ALENS(20,I)*M1
                  ALENS(21,I)=ALENS(21,I)*M1
                  ALENS(62,I)=ALENS(62,I)*M1
                  ALENS(63,I)=ALENS(63,I)
                  ALENS(64,I)=ALENS(64,I)*M1
                  ALENS(65,I)=ALENS(65,I)*M1
                  ALENS(66,I)=ALENS(66,I)*M1
              ELSE
              END IF
C
C       IPOLY
              IF(DABS(ALENS(9,I)).EQ.6.0D0) THEN
                  ALENS(11,I)=ALENS(12,I)*M1
                  ALENS(12,I)=ALENS(13,I)*M1
                  ALENS(13,I)=ALENS(14,I)*M1
                  ALENS(53,I)=ALENS(54,I)*M1
                  ALENS(54,I)=ALENS(55,I)*M1
                  ALENS(55,I)=ALENS(56,I)*M1
              ELSE
              END IF
              IF(DABS(ALENS(16,I)).EQ.6.0D0) THEN
                  ALENS(18,I)=ALENS(19,I)*M1
                  ALENS(19,I)=ALENS(20,I)*M1
                  ALENS(20,I)=ALENS(21,I)*M1
                  ALENS(63,I)=ALENS(64,I)*M1
                  ALENS(64,I)=ALENS(65,I)*M1
                  ALENS(65,I)=ALENS(66,I)*M1
              ELSE
              END IF
C
C       ALL CLAPS/COBS HAVE BEEN SCALED
C
C       TORIC CURVATURE
C
              ALENS(24,I)=ALENS(24,I)/(W1)
C
C       X,Y AND Z DECENTERS
C
              ALENS(30,I)=ALENS(30,I)*DABS(W1)
              ALENS(31,I)=ALENS(31,I)*DABS(W1)
              ALENS(69,I)=ALENS(69,I)*W1
              ALENS(114,I)=ALENS(114,I)*DABS(W1)
              ALENS(115,I)=ALENS(115,I)*DABS(W1)
              ALENS(116,I)=ALENS(116,I)*W1
              ALENS(90,I)=ALENS(90,I)*DABS(W1)
              ALENS(91,I)=ALENS(91,I)*DABS(W1)
              ALENS(92,I)=ALENS(92,I)*W1
              IF(W1.LT.0.0D0) ALENS(26,I)=-ALENS(26,I)
              IF(W1.LT.0.0D0) ALENS(27,I)=-ALENS(27,I)
              IF(W1.LT.0.0D0) ALENS(28,I)=-ALENS(28,I)
              IF(W1.LT.0.0D0) ALENS(118,I)=-ALENS(118,I)
              IF(W1.LT.0.0D0) ALENS(119,I)=-ALENS(119,I)
              IF(W1.LT.0.0D0) ALENS(120,I)=-ALENS(120,I)
              IF(W1.LT.0.0D0) ALENS(93,I)=-ALENS(93,I)
              IF(W1.LT.0.0D0) ALENS(94,I)=-ALENS(94,I)
              IF(W1.LT.0.0D0) ALENS(95,I)=-ALENS(95,I)
C
C       X,Y AND Z PIVOTS
C
              ALENS(78,I)=ALENS(78,I)*DABS(W1)
              ALENS(79,I)=ALENS(79,I)*DABS(W1)
              ALENS(80,I)=ALENS(80,I)*W1
C
C       ASPHERIC TORIC COEFFICIENTS
C
              ALENS(37,I)=ALENS(37,I)/(W1**3)
              ALENS(38,I)=ALENS(38,I)/(W1**5)
              ALENS(39,I)=ALENS(39,I)/(W1**7)
              ALENS(40,I)=ALENS(40,I)/(W1**9)
C
C       ALL THE SYSTEM AND ALENS VALUES THAT NEEDED
C       SCALING HAVE BEEN SCALED.
C
C       NOW SOLVE TARGET VALUES.
C
C       ANGULAR TARGETS ARE NOT SCALED
C       DIMENSIONAL TARGETS ARE SCALED
C
              IF(SOLVE(6,I).EQ.1.0D0.OR.SOLVE(6,I)
     1        .EQ.2.0D0.OR.SOLVE(6,I).EQ.3.0D0) THEN
                  SOLVE(7,I)=SOLVE(7,I)*W1
              ELSE
              END IF
              IF(SOLVE(4,I).EQ.4.0D0.OR.SOLVE(4,I)
     1        .EQ.5.0D0.OR.SOLVE(4,I).EQ.6.0D0) THEN
                  SOLVE(3,I)=SOLVE(3,I)*W1
              ELSE
              END IF
C
C       ALL SOLVE SCALING IS COMPLETE
C
C       PIKUP SCALING
C       IN PIKUP SCALING, THE ONLY VALUE SCALED IS THE
C       ADDITIVE CONSTANT IF NUMERIC WORD 4 IS NOT 1.0
C       RD
              PIKUP(4,I,1)=PIKUP(4,I,1)*W1
C       CV
              PIKUP(4,I,2)=PIKUP(4,I,2)*W1
C       TH
              PIKUP(4,I,3)=PIKUP(4,I,3)*W1
C       THOAL (THE FIFTH TERM IS SCALED CAUSE THERE WERE 2 SURFACES ENTERED)
              PIKUP(5,I,32)=PIKUP(5,I,32)*W1
C       CC
              PIKUP(4,I,4)=PIKUP(4,I,4)*W1
C       AD
              PIKUP(4,I,5)=PIKUP(4,I,5)/(W1**3)
C       AE
              PIKUP(4,I,6)=PIKUP(4,I,6)/(W1**5)
C       AF
              PIKUP(4,I,7)=PIKUP(4,I,7)/(W1**7)
C       AG
              PIKUP(4,I,8)=PIKUP(4,I,8)/(W1**9)
C       CVTOR
              PIKUP(4,I,9)=PIKUP(4,I,9)*W1
C       RDTOR
              PIKUP(4,I,10)=PIKUP(4,I,10)*W1
C       YD
              PIKUP(4,I,13)=PIKUP(4,I,13)*W1
C       XD
              PIKUP(4,I,14)=PIKUP(4,I,14)*W1
C       ALPHA
              PIKUP(4,I,15)=PIKUP(4,I,15)*W1
C       BETA
              PIKUP(4,I,16)=PIKUP(4,I,16)*W1
C       GAMMA
              PIKUP(4,I,17)=PIKUP(4,I,17)*W1
C       CLAP
              PIKUP(4,I,18)=PIKUP(4,I,18)*M1
C       COBS
              PIKUP(4,I,19)=PIKUP(4,I,19)*M1
C       CCTOR
              PIKUP(4,I,20)=PIKUP(4,I,20)*W1
C       ADTOR
              PIKUP(4,I,22)=PIKUP(4,I,22)/(W1**3)
C       AETOR
              PIKUP(4,I,23)=PIKUP(4,I,23)/(W1**5)
C       AFTOR
              PIKUP(4,I,24)=PIKUP(4,I,24)/(W1**7)
C       AGTOR
              PIKUP(4,I,25)=PIKUP(4,I,25)/(W1**9)
C       AC
              PIKUP(4,I,26)=PIKUP(4,I,26)/(W1)
C       AH
              PIKUP(4,I,27)=PIKUP(4,I,27)/(W1**11)
C       AI
              PIKUP(4,I,28)=PIKUP(4,I,28)/(W1**13)
C       AJ
              PIKUP(4,I,29)=PIKUP(4,I,29)/(W1**15)
C       AK
              PIKUP(4,I,30)=PIKUP(4,I,30)/(W1**17)
C       AL
              PIKUP(4,I,31)=PIKUP(4,I,31)/(W1**19)
C
C               NOW ALL THE PIKUPS ARE SCALED
C       PRO,NPRO AND GLASS DON'T SCALE!
          END DO
C
C       NOW MAKE CHANGES TO THE CONFIG DATA
C       WHICH IS APPROPRIATE FOR THE PARTICULAR
C       SCALING REQUESTED.
C               CALL CFSC1
C       ALL RESCALING WAS PERFORMED.
C       RESCALING AFFECTS SPECIAL SURFACE DATA
C       AS OF 1/6/94
          SCW1=W1
          CALL SCASPC(SCW1)
          F1=0
          F6=1
          F22=0
          LNSTYP=1
          CALL LNSEOS
          RETURN
      END
C SUB SCALEA4.FOR
      SUBROUTINE SCALEA4
C
          IMPLICIT NONE
C
C     THIS DOES WSC WITH FY
C
          INTEGER I,J,J1,J2
C
          REAL*8 EFLY,SCW1,M1
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
C
C
C       WORKING SCALING TO A SPECIFIC EFL (SC EFL,EFL,I,J)
C       W1=DESIRED
C       W2=STARTING SURFACE
C       W3=ENDING SURFACE
C       SAY,SAX,SCY,SCX STARTING VALUES NOT SCALED
C       FIRST OPERATE ON THE CURRENT LENS
C
C       WE MUST FIRST DETERMINE THE ORIGINAL EFL
C       OF THE ELEMENTS FROM SURFACE I TO J
C       THEN THE CORRECT SCALE FACTOR( USED FOR ALL CONFIGS BUT
C       DETERMINED FROM DATA IN CONFIG 1 IS GIVEN BY:
C
C       SCALE FACTOR= (NEW DESIRED EFL)/(ORIGINAL EFL)
C
C       YZ PLANE EFL CALCULATION
C
          IF(W2.GT.0.0D0) THEN
              I=INT(W2)-1
          ELSE
              I=INT(W2)
          END IF
          J=INT(W3)
          EFLY=-(((PXTRAY(2,I)*PXTRAY(5,I+1))-(PXTRAY(1,I+1)
     1    *PXTRAY(6,I)))/
     1    ((PXTRAY(2,I)*PXTRAY(6,J))-(PXTRAY(6,I)*PXTRAY(2,J))))
          IF(EFLY.EQ.0.0D0.OR.DABS(EFLY).GT.1.0D10) THEN
              OUTLYNE='NO EFL CAN BE CALCULATED FOR SPECIFIED SURFACES'
              CALL SHOWIT(1)
              OUTLYNE='NO EFL SCALING CAN BE PERFORMED'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
C       NEW SCALE FACTOR IS W1
C
          W1=W1/(EFLY)
C
C       REMOVE FNBY HLD OR ER HOLD IF PRESENT
C
          IF(SYSTEM1(44).EQ.-1.0D0.OR.SYSTEM1(45).EQ.-1.0D0)THEN
              OUTLYNE='REMOVING F/# HOLD'
              CALL SHOWIT(1)
              SYSTEM1(44:47)=0.0D0
          END IF
          IF(SYSTEM1(44).EQ.-2.0D0.OR.SYSTEM1(45).EQ.-2.0D0)THEN
              OUTLYNE='REMOVING EXIT PUPIL RADIUS HOLD'
              CALL SHOWIT(1)
              SYSTEM1(44:47)=0.0D0
          END IF

C
C       NOW FOR SURFACE DATA
          J1=INT(W2)
          J2=INT(W3)
          DO I=J1,J2
C       CURVATURE
C       HERE SCALE FACTOR CHANGES THE RADIUS OF CURVATURE
C       FACTOR = 2 INCREASES RADIUS BY 2X OR REDUCES CURVATURE
C       BY FACTOR OF 2X.
C
              ALENS(1,I)=ALENS(1,I)*(1/W1)
C
C       CONIC CONSTANT DOES NOT CHANGE
C
C       THICKNESS
C
              ALENS(3,I)=ALENS(3,I)*W1
C
C       ROOF AND THICKNESS
C
              IF(ALENS(126,I).EQ.1.0D0) THEN
C     ROOF
                  ALENS(138,I)=ALENS(138,I)*W1
                  ALENS(140,I)=ALENS(140,I)*W1
              END IF
              IF(ALENS(126,I).EQ.1.0D0) THEN
C     CCR
                  ALENS(138,I)=ALENS(138,I)*W1
                  ALENS(142,I)=ALENS(142,I)*W1
              END IF
C
C       IDEAL LENS THICKNESS
C
              ALENS(121,I)=ALENS(121,I)*W1
C
C       FOURTH, SIXTH, EIGHTH AND TENTH ORDER ASPHERICS
C       FACTOR OF NX CHANGES THE FOURTH ORDER COEFFICIENT
C       FROM AD TO AD/(NX**3)
C       AND
C       AE TO AE/(NX**5) AND AF TO AF/(NX**7) AND
C       AG TO AG/(NX**9)
C
              ALENS(43,I)=ALENS(43,I)/W1
              ALENS(4,I)=ALENS(4,I)/(W1**3)
              ALENS(5,I)=ALENS(5,I)/(W1**5)
              ALENS(6,I)=ALENS(6,I)/(W1**7)
              ALENS(7,I)=ALENS(7,I)/(W1**9)
              ALENS(81,I)=ALENS(81,I)/(W1**11)
              ALENS(82,I)=ALENS(82,I)/(W1**13)
              ALENS(83,I)=ALENS(83,I)/(W1**15)
              ALENS(84,I)=ALENS(84,I)/(W1**17)
              ALENS(85,I)=ALENS(85,I)/(W1**19)
C
C       NOW THE CLEAR APERTURES AND OBSCURATIONS
C
              M1=DABS(W1)
C     CLAP MULT SCALING
              IF(ALENS(127,I).NE.0.0D0) THEN
                  DO J=1,INT(ALENS(127,I))
                      MULTCLAP(J,1,I)=MULTCLAP(J,1,I)*M1
                      MULTCLAP(J,2,I)=MULTCLAP(J,2,I)*M1
                  END DO
              END IF
C     COBS MULT SCALING
              IF(ALENS(128,I).NE.0.0D0) THEN
                  DO J=1,INT(ALENS(128,I))
                      MULTCOBS(J,1,I)=MULTCOBS(J,1,I)*M1
                      MULTCOBS(J,2,I)=MULTCOBS(J,2,I)*M1
                  END DO
              END IF
C     SPIDER SCALING
              IF(ALENS(134,I).NE.0.0D0) THEN
                  ALENS(136,I)=ALENS(136,I)*M1
                  ALENS(137,I)=ALENS(137,I)*M1
              END IF
C       CIRCULAR
              IF(DABS(ALENS(9,I)).EQ.1.0D0) THEN
                  ALENS(10,I)=ALENS(10,I)*M1
                  ALENS(11,I)=ALENS(11,I)*M1
                  ALENS(12,I)=ALENS(12,I)*M1
                  ALENS(13,I)=ALENS(13,I)*M1
                  ALENS(52,I)=ALENS(52,I)*M1
                  ALENS(54,I)=ALENS(54,I)*M1
                  ALENS(55,I)=ALENS(55,I)*M1
              ELSE
              END IF
              IF(DABS(ALENS(16,I)).EQ.1.0D0) THEN
                  ALENS(17,I)=ALENS(17,I)*M1
                  ALENS(18,I)=ALENS(17,I)*M1
                  ALENS(19,I)=ALENS(19,I)*M1
                  ALENS(20,I)=ALENS(20,I)*M1
                  ALENS(62,I)=ALENS(62,I)*M1
                  ALENS(64,I)=ALENS(64,I)*M1
                  ALENS(65,I)=ALENS(65,I)*M1
              ELSE
              END IF
C       RECTANGULAR
              IF(DABS(ALENS(9,I)).EQ.2.0D0) THEN
                  ALENS(10,I)=ALENS(10,I)*M1
                  ALENS(11,I)=ALENS(11,I)*M1
                  ALENS(12,I)=ALENS(12,I)*M1
                  ALENS(13,I)=ALENS(13,I)*M1
                  ALENS(52,I)=ALENS(52,I)*M1
                  ALENS(53,I)=ALENS(53,I)*M1
                  ALENS(54,I)=ALENS(54,I)*M1
                  ALENS(55,I)=ALENS(55,I)*M1
              ELSE
              END IF
              IF(DABS(ALENS(16,I)).EQ.2.0D0) THEN
                  ALENS(17,I)=ALENS(17,I)*M1
                  ALENS(18,I)=ALENS(18,I)*M1
                  ALENS(19,I)=ALENS(19,I)*M1
                  ALENS(20,I)=ALENS(20,I)*M1
                  ALENS(62,I)=ALENS(62,I)*M1
                  ALENS(63,I)=ALENS(63,I)*M1
                  ALENS(64,I)=ALENS(64,I)*M1
                  ALENS(65,I)=ALENS(65,I)*M1
              ELSE
              END IF
C       ELLIPTICAL
              IF(DABS(ALENS(9,I)).EQ.3.0D0) THEN
                  ALENS(10,I)=ALENS(10,I)*M1
                  ALENS(11,I)=ALENS(11,I)*M1
                  ALENS(12,I)=ALENS(12,I)*M1
                  ALENS(13,I)=ALENS(13,I)*M1
                  ALENS(52,I)=ALENS(52,I)*M1
                  ALENS(53,I)=ALENS(53,I)*M1
                  ALENS(54,I)=ALENS(54,I)*M1
                  ALENS(55,I)=ALENS(55,I)*M1
              ELSE
              END IF
              IF(DABS(ALENS(16,I)).EQ.3.0D0) THEN
                  ALENS(17,I)=ALENS(17,I)*M1
                  ALENS(18,I)=ALENS(18,I)*M1
                  ALENS(19,I)=ALENS(19,I)*M1
                  ALENS(20,I)=ALENS(20,I)*M1
                  ALENS(62,I)=ALENS(62,I)*M1
                  ALENS(63,I)=ALENS(63,I)*M1
                  ALENS(64,I)=ALENS(64,I)*M1
                  ALENS(65,I)=ALENS(65,I)*M1
              ELSE
              END IF
C
C       RACETRACK
              IF(DABS(ALENS(9,I)).EQ.4.0D0) THEN
                  ALENS(10,I)=ALENS(10,I)*M1
                  ALENS(11,I)=ALENS(11,I)*M1
                  ALENS(12,I)=ALENS(12,I)*M1
                  ALENS(13,I)=ALENS(13,I)*M1
                  ALENS(14,I)=ALENS(14,I)*M1
                  ALENS(52,I)=ALENS(52,I)*M1
                  ALENS(53,I)=ALENS(53,I)*M1
                  ALENS(54,I)=ALENS(54,I)*M1
                  ALENS(55,I)=ALENS(55,I)*M1
                  ALENS(56,I)=ALENS(56,I)*M1
              ELSE
              END IF
              IF(DABS(ALENS(16,I)).EQ.4.0D0) THEN
                  ALENS(17,I)=ALENS(17,I)*M1
                  ALENS(18,I)=ALENS(18,I)*M1
                  ALENS(19,I)=ALENS(19,I)*M1
                  ALENS(20,I)=ALENS(20,I)*M1
                  ALENS(21,I)=ALENS(21,I)*M1
                  ALENS(62,I)=ALENS(62,I)*M1
                  ALENS(63,I)=ALENS(63,I)*M1
                  ALENS(64,I)=ALENS(64,I)*M1
                  ALENS(65,I)=ALENS(65,I)*M1
                  ALENS(66,I)=ALENS(66,I)*M1
              ELSE
              END IF
C
C       POLY
              IF(DABS(ALENS(9,I)).EQ.5.0D0) THEN
                  ALENS(10,I)=ALENS(10,I)*M1
                  ALENS(11,I)=ALENS(11,I)
                  ALENS(12,I)=ALENS(12,I)*M1
                  ALENS(13,I)=ALENS(13,I)*M1
                  ALENS(14,I)=ALENS(14,I)*M1
                  ALENS(52,I)=ALENS(52,I)*M1
                  ALENS(53,I)=ALENS(53,I)
                  ALENS(54,I)=ALENS(54,I)*M1
                  ALENS(55,I)=ALENS(55,I)*M1
                  ALENS(56,I)=ALENS(56,I)*M1
              ELSE
              END IF
              IF(DABS(ALENS(16,I)).EQ.5.0D0) THEN
                  ALENS(17,I)=ALENS(17,I)*M1
                  ALENS(18,I)=ALENS(18,I)
                  ALENS(19,I)=ALENS(19,I)*M1
                  ALENS(20,I)=ALENS(20,I)*M1
                  ALENS(21,I)=ALENS(21,I)*M1
                  ALENS(62,I)=ALENS(62,I)*M1
                  ALENS(63,I)=ALENS(63,I)
                  ALENS(64,I)=ALENS(64,I)*M1
                  ALENS(65,I)=ALENS(65,I)*M1
                  ALENS(66,I)=ALENS(66,I)*M1
              ELSE
              END IF
C
C       IPOLY
              IF(DABS(ALENS(9,I)).EQ.6.0D0) THEN
                  ALENS(11,I)=ALENS(12,I)*M1
                  ALENS(12,I)=ALENS(13,I)*M1
                  ALENS(13,I)=ALENS(14,I)*M1
                  ALENS(53,I)=ALENS(54,I)*M1
                  ALENS(54,I)=ALENS(55,I)*M1
                  ALENS(55,I)=ALENS(56,I)*M1
              ELSE
              END IF
              IF(DABS(ALENS(16,I)).EQ.6.0D0) THEN
                  ALENS(18,I)=ALENS(19,I)*M1
                  ALENS(19,I)=ALENS(20,I)*M1
                  ALENS(20,I)=ALENS(21,I)*M1
                  ALENS(63,I)=ALENS(64,I)*M1
                  ALENS(64,I)=ALENS(65,I)*M1
                  ALENS(65,I)=ALENS(66,I)*M1
              ELSE
              END IF
C
C       ALL CLAPS/COBS HAVE BEEN SCALED
C
C       TORIC CURVATURE
C
              ALENS(24,I)=ALENS(24,I)/(W1)
C
C       X,Y AND Z DECENTERS
C
              ALENS(30,I)=ALENS(30,I)*DABS(W1)
              ALENS(31,I)=ALENS(31,I)*DABS(W1)
              ALENS(69,I)=ALENS(69,I)*W1
              ALENS(114,I)=ALENS(114,I)*DABS(W1)
              ALENS(115,I)=ALENS(115,I)*DABS(W1)
              ALENS(116,I)=ALENS(116,I)*W1
              ALENS(90,I)=ALENS(90,I)*DABS(W1)
              ALENS(91,I)=ALENS(91,I)*DABS(W1)
              ALENS(92,I)=ALENS(92,I)*W1
              IF(W1.LT.0.0D0) ALENS(26,I)=-ALENS(26,I)
              IF(W1.LT.0.0D0) ALENS(27,I)=-ALENS(27,I)
              IF(W1.LT.0.0D0) ALENS(28,I)=-ALENS(28,I)
              IF(W1.LT.0.0D0) ALENS(118,I)=-ALENS(118,I)
              IF(W1.LT.0.0D0) ALENS(119,I)=-ALENS(119,I)
              IF(W1.LT.0.0D0) ALENS(120,I)=-ALENS(120,I)
              IF(W1.LT.0.0D0) ALENS(93,I)=-ALENS(93,I)
              IF(W1.LT.0.0D0) ALENS(94,I)=-ALENS(94,I)
              IF(W1.LT.0.0D0) ALENS(95,I)=-ALENS(95,I)
C
C       X,Y AND Z PIVOTS
C
              ALENS(78,I)=ALENS(78,I)*DABS(W1)
              ALENS(79,I)=ALENS(79,I)*DABS(W1)
              ALENS(80,I)=ALENS(80,I)*W1
C
C       ASPHERIC TORIC COEFFICIENTS
C
              ALENS(37,I)=ALENS(37,I)/(W1**3)
              ALENS(38,I)=ALENS(38,I)/(W1**5)
              ALENS(39,I)=ALENS(39,I)/(W1**7)
              ALENS(40,I)=ALENS(40,I)/(W1**9)
C
C       ALL THE SYSTEM AND ALENS VALUES THAT NEEDED
C       SCALING HAVE BEEN SCALED.
C
C       NOW SOLVE TARGET VALUES.
C
C       ANGULAR TARGETS ARE NOT SCALED
C       DIMENSIONAL TARGETS ARE SCALED
C
              IF(SOLVE(6,I).EQ.1.0D0.OR.SOLVE(6,I)
     1        .EQ.2.0D0.OR.SOLVE(6,I).EQ.3.0D0) THEN
                  SOLVE(7,I)=SOLVE(7,I)*W1
              ELSE
              END IF
              IF(SOLVE(4,I).EQ.4.0D0.OR.SOLVE(4,I)
     1        .EQ.5.0D0.OR.SOLVE(4,I).EQ.6.0D0) THEN
                  SOLVE(3,I)=SOLVE(3,I)*W1
              ELSE
              END IF
C
C       ALL SOLVE SCALING IS COMPLETE
C
C       PIKUP SCALING
C       IN PIKUP SCALING, THE ONLY VALUE SCALED IS THE
C       ADDITIVE CONSTANT IF NUMERIC WORD 4 IS NOT 1.0
C       RD
              PIKUP(4,I,1)=PIKUP(4,I,1)*W1
C       CV
              PIKUP(4,I,2)=PIKUP(4,I,2)*W1
C       TH
              PIKUP(4,I,3)=PIKUP(4,I,3)*W1
C       THOAL (THE FIFTH TERM IS SCALED CAUSE THERE WERE 2 SURFACES ENTERED)
              PIKUP(5,I,32)=PIKUP(5,I,32)*W1
C       CC
              PIKUP(4,I,4)=PIKUP(4,I,4)*W1
C       AD
              PIKUP(4,I,5)=PIKUP(4,I,5)/(W1**3)
C       AE
              PIKUP(4,I,6)=PIKUP(4,I,6)/(W1**5)
C       AF
              PIKUP(4,I,7)=PIKUP(4,I,7)/(W1**7)
C       AG
              PIKUP(4,I,8)=PIKUP(4,I,8)/(W1**9)
C       CVTOR
              PIKUP(4,I,9)=PIKUP(4,I,9)*W1
C       RDTOR
              PIKUP(4,I,10)=PIKUP(4,I,10)*W1
C       YD
              PIKUP(4,I,13)=PIKUP(4,I,13)*W1
C       XD
              PIKUP(4,I,14)=PIKUP(4,I,14)*W1
C       ALPHA
              PIKUP(4,I,15)=PIKUP(4,I,15)*W1
C       BETA
              PIKUP(4,I,16)=PIKUP(4,I,16)*W1
C       GAMMA
              PIKUP(4,I,17)=PIKUP(4,I,17)*W1
C       CLAP
              PIKUP(4,I,18)=PIKUP(4,I,18)*M1
C       COBS
              PIKUP(4,I,19)=PIKUP(4,I,19)*M1
C       CCTOR
              PIKUP(4,I,20)=PIKUP(4,I,20)*W1
C       ADTOR
              PIKUP(4,I,22)=PIKUP(4,I,22)/(W1**3)
C       AETOR
              PIKUP(4,I,23)=PIKUP(4,I,23)/(W1**5)
C       AFTOR
              PIKUP(4,I,24)=PIKUP(4,I,24)/(W1**7)
C       AGTOR
              PIKUP(4,I,25)=PIKUP(4,I,25)/(W1**9)
C       AC
              PIKUP(4,I,26)=PIKUP(4,I,26)/(W1)
C       AH
              PIKUP(4,I,27)=PIKUP(4,I,27)/(W1**11)
C       AI
              PIKUP(4,I,28)=PIKUP(4,I,28)/(W1**13)
C       AJ
              PIKUP(4,I,29)=PIKUP(4,I,29)/(W1**15)
C       AK
              PIKUP(4,I,30)=PIKUP(4,I,30)/(W1**17)
C       AL
              PIKUP(4,I,31)=PIKUP(4,I,31)/(W1**19)
C
C               NOW ALL THE PIKUPS ARE SCALED
C       PRO,NPRO AND GLASS DON'T SCALE!
          END DO
C
C       NOW MAKE CHANGES TO THE CONFIG DATA
C       WHICH IS APPROPRIATE FOR THE PARTICULAR
C       SCALING REQUESTED.
          CALL CFSC2
C       ALL RESCALING WAS PERFORMED.
C       RESCALING AFFECTS SPECIAL SURFACE DATA
C       AS OF 1/6/94
          SCW1=W1
          CALL SCASPC(SCW1)
          F1=0
          F6=1
          F22=0
          LNSTYP=1
          CALL LNSEOS
          RETURN
      END
