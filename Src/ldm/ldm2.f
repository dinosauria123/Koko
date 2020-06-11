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

C SUB THERM.FOR
      SUBROUTINE THERM
C
          IMPLICIT NONE
C
          INTEGER I
C
          LOGICAL GONOGO
C
          REAL*8 FACTOR,DSGN
C
          EXTERNAL DSGN
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
          IF(SST.EQ.1) THEN
              WRITE(OUTLYNE,*)'"THERM" TAKES NO STRING INPUT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
          IF(STI.EQ.1) THEN
              WRITE(OUTLYNE,*)'NO ADDITIONAL INFORMATION'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
          IF(S5.EQ.1) THEN
              WRITE(OUTLYNE,*)'"THERM" TAKES NO NUMERIC WORD #5 INPUT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
          IF(SQ.EQ.0) THEN
              WRITE(OUTLYNE,*)'"THERM" REQUIRES EXPLICIT QUALIFIER INPUT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
          IF(WQ.NE.'SHAPE'.AND.WQ.NE.'GLASS'.AND.WQ.NE.'THICK'
     1    .AND.WQ.NE.'SPACE') THEN
              WRITE(OUTLYNE,*)'INVALID QUALIFIER USED WITH "THERM"'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(DF1.EQ.1) W1=0.0D0
          IF(DF2.EQ.1) W2=SYSTEM1(20)
          IF(DF3.EQ.1.OR.DF4.EQ.1) THEN
              WRITE(OUTLYNE,*)
     1        '"THERM" REQUIRES EXPLICIT NUMERIC WORDS #3 AND #4'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(W1.LT.0.0D0) THEN
              WRITE(OUTLYNE,*)'STARTING SURFACE NUMBER MUST BE 0 OR GREATER'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(W2.GT.SYSTEM1(20)) THEN
              WRITE(OUTLYNE,*)
     1        'ENDING SURFACE NUMBER MUST BE LESS THAN',INT(SYSTEM1(20)+1.0D0)
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(W2.LT.W1) THEN
              WRITE(OUTLYNE,*)
     1        'ERROR:'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)
     1        'ENDING SURFACE NUMBER LESS THAN STARTING SURFACE NUMBER'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
C DO THE THERM OPERATION
          IF(WQ.EQ.'GLASS') THEN
              IF(INT(W1).EQ.INT(W2)) THEN
                  I=INT(W1)
                  GONOGO=.TRUE.
                  CALL CHKGLS(GONOGO,I)
                  IF(.NOT.GONOGO) THEN
                      RETURN
                  END IF
                  IF(GLANAM(I,2).EQ.'REFL         '.OR.
     1            GLANAM(I,2).EQ.'REFLTIRO     '.OR.
     2            GLANAM(I,2).EQ.'REFLTIR      ') THEN
                      WRITE(OUTLYNE,*)
     1                '"THERM ',WQ,'" DID NOT MODIFY "REFL" SURFACE # ',I
                      CALL SHOWIT(1)
                      RETURN
                  END IF
                  IF(GLANAM(I,2).EQ.'PERFECT      ') THEN
                      WRITE(OUTLYNE,*)
     1                '"THERM ',WQ,'" DID NOT MODIFY "PERFECT" SURFACE # ',I
                      CALL SHOWIT(1)
                      RETURN
                  END IF
                  IF(GLANAM(I,2).EQ.'IDEAL        ') THEN
                      WRITE(OUTLYNE,*)
     1                '"THERM ',WQ,'" DID NOT MODIFY "IDEAL" SURFACE # ',I
                      CALL SHOWIT(1)
                      RETURN
                  END IF
                  GONOGO=.TRUE.
                  CALL CHKGLS(GONOGO,I)
                  IF(.NOT.GONOGO) THEN
                  ELSE
                      IF(GLANAM(I,2).NE.'REFL         '.AND.
     1                GLANAM(I,2).NE.'PERFECT      '.AND.
     1                GLANAM(I,2).NE.'REFLTIR      '.AND.
     1                GLANAM(I,2).NE.'REFLTIRO     '.AND.
     1                GLANAM(I,2).NE.'IDEAL        ') THEN
                          IF(DABS(ALENS(46,I)).GT.1.1D0.OR.DABS(ALENS(47,I)).GT.1.1D0
     1                    .OR.DABS(ALENS(48,I)).GT.1.1D0.OR.DABS(ALENS(49,I)).GT.1.1D0
     2                    .OR.DABS(ALENS(50,I)).GT.1.1D0) THEN
                              GLANAM(I,1)='MYGLASS      '
                              IF(DABS(ALENS(46,I)).GT.1.1D0)
     1                        ALENS(46,I)=ALENS(46,I)+(DSGN(ALENS(46,I))*W3*W4)
                              IF(DABS(ALENS(47,I)).GT.1.1D0)
     1                        ALENS(47,I)=ALENS(47,I)+(DSGN(ALENS(46,I))*W3*W4)
                              IF(DABS(ALENS(48,I)).GT.1.1D0)
     1                        ALENS(48,I)=ALENS(48,I)+(DSGN(ALENS(48,I))*W3*W4)
                              IF(DABS(ALENS(49,I)).GT.1.1D0)
     1                        ALENS(49,I)=ALENS(49,I)+(DSGN(ALENS(49,I))*W3*W4)
                              IF(DABS(ALENS(50,I)).GT.1.1D0)
     1                        ALENS(50,I)=ALENS(50,I)+(DSGN(ALENS(50,I))*W3*W4)
                          END IF
                          IF(DABS(ALENS(71,I)).GT.1.1D0.OR.DABS(ALENS(72,I)).GT.1.1D0
     1                    .OR.DABS(ALENS(73,I)).GT.1.1D0.OR.DABS(ALENS(74,I)).GT.1.1D0
     2                    .OR.DABS(ALENS(75,I)).GT.1.1D0) THEN
                              IF(DABS(ALENS(71,I)).GT.1.1D0)
     1                        ALENS(71,I)=ALENS(71,I)+(DSGN(ALENS(71,I))*W3*W4)
                              IF(DABS(ALENS(47,I)).GT.1.1D0)
     1                        ALENS(72,I)=ALENS(72,I)+(DSGN(ALENS(72,I))*W3*W4)
                              IF(DABS(ALENS(48,I)).GT.1.1D0)
     1                        ALENS(73,I)=ALENS(73,I)+(DSGN(ALENS(73,I))*W3*W4)
                              IF(DABS(ALENS(49,I)).GT.1.1D0)
     1                        ALENS(74,I)=ALENS(74,I)+(DSGN(ALENS(74,I))*W3*W4)
                              IF(DABS(ALENS(50,I)).GT.1.1D0)
     1                        ALENS(75,I)=ALENS(75,I)+(DSGN(ALENS(75,I))*W3*W4)
                          END IF
                      END IF
                  END IF
              ELSE
                  DO I=INT(W1),INT(W2)
                      IF(GLANAM(I,2).EQ.'REFL         '.OR.
     1                GLANAM(I,2).EQ.'REFLTIRO     '.OR.
     2                GLANAM(I,2).EQ.'REFLTIR      ') THEN
                          WRITE(OUTLYNE,*)
     1                    '"THERM ',WQ,'" DID NOT MODIFY "REFL" SURFACE # ',I
                          CALL SHOWIT(1)
                      END IF
                      IF(GLANAM(I,2).EQ.'PERFECT      ') THEN
                          WRITE(OUTLYNE,*)
     1                    '"THERM ',WQ,'" DID NOT MODIFY "PERFECT" SURFACE # ',I
                          CALL SHOWIT(1)
                      END IF
                      IF(GLANAM(I,2).EQ.'IDEAL        ') THEN
                          WRITE(OUTLYNE,*)
     1                    '"THERM ',WQ,'" DID NOT MODIFY "IDEAL" SURFACE # ',I
                          CALL SHOWIT(1)
                      END IF
                      IF(GLANAM(I,2).NE.'REFL         '.AND.
     1                GLANAM(I,2).NE.'PERFECT      '.AND.
     1                GLANAM(I,2).NE.'REFLTIR      '.AND.
     1                GLANAM(I,2).NE.'REFLTIRO     '.AND.
     1                GLANAM(I,2).NE.'IDEAL        ') THEN
                          IF(DABS(ALENS(46,I)).GT.1.1D0.OR.DABS(ALENS(47,I)).GT.1.1D0
     1                    .OR.DABS(ALENS(48,I)).GT.1.1D0.OR.DABS(ALENS(49,I)).GT.1.1D0
     2                    .OR.DABS(ALENS(50,I)).GT.1.1D0) THEN
                              GLANAM(I,1)='MYGLASS      '
                              IF(DABS(ALENS(46,I)).GT.1.1D0)
     1                        ALENS(46,I)=ALENS(46,I)+(DSGN(ALENS(46,I))*W3*W4)
                              IF(DABS(ALENS(47,I)).GT.1.1D0)
     1                        ALENS(47,I)=ALENS(47,I)+(DSGN(ALENS(47,I))*W3*W4)
                              IF(DABS(ALENS(48,I)).GT.1.1D0)
     1                        ALENS(48,I)=ALENS(48,I)+(DSGN(ALENS(48,I))*W3*W4)
                              IF(DABS(ALENS(49,I)).GT.1.1D0)
     1                        ALENS(49,I)=ALENS(49,I)+(DSGN(ALENS(49,I))*W3*W4)
                              IF(DABS(ALENS(50,I)).GT.1.1D0)
     1                        ALENS(50,I)=ALENS(50,I)+(DSGN(ALENS(50,I))*W3*W4)
                          END IF
                          IF(DABS(ALENS(71,I)).GT.1.1D0.OR.DABS(ALENS(72,I)).GT.1.1D0
     1                    .OR.DABS(ALENS(73,I)).GT.1.1D0.OR.DABS(ALENS(74,I)).GT.1.1D0
     2                    .OR.DABS(ALENS(75,I)).GT.1.1D0) THEN
                              IF(DABS(ALENS(71,I)).GT.1.1D0)
     1                        ALENS(71,I)=ALENS(71,I)+(DSGN(ALENS(71,I))*W3*W4)
                              IF(DABS(ALENS(72,I)).GT.1.1D0)
     1                        ALENS(72,I)=ALENS(72,I)+(DSGN(ALENS(72,I))*W3*W4)
                              IF(DABS(ALENS(73,I)).GT.1.1D0)
     1                        ALENS(73,I)=ALENS(73,I)+(DSGN(ALENS(73,I))*W3*W4)
                              IF(DABS(ALENS(74,I)).GT.1.1D0)
     1                        ALENS(74,I)=ALENS(74,I)+(DSGN(ALENS(74,I))*W3*W4)
                              IF(DABS(ALENS(75,I)).GT.1.1D0)
     1                        ALENS(75,I)=ALENS(75,I)+(DSGN(ALENS(75,I))*W3*W4)
                          END IF
                      END IF
                  END DO
              END IF
              F1=0
              F6=1
              F22=1
              LNSTYP=1
              CALL LNSEOS
          END IF
          IF(WQ.EQ.'SPACE') THEN
              IF(INT(W1).EQ.INT(W2)) THEN
                  I=INT(W1)
                  GONOGO=.TRUE.
                  CALL CHKTHK(GONOGO,I)
                  IF(.NOT.GONOGO) THEN
                      RETURN
                  END IF
                  IF(DABS(ALENS(46,I)).LE.1.1D0.AND.DABS(ALENS(47,I)).LE.1.1D0
     1            .AND.DABS(ALENS(48,I)).LE.1.1D0.AND.DABS(ALENS(49,I)).LE.1.1D0
     2            .AND.DABS(ALENS(50,I)).LE.1.1D0.AND.
     1            DABS(ALENS(71,I)).LE.1.1D0.AND.DABS(ALENS(72,I)).LE.1.1D0
     1            .AND.DABS(ALENS(73,I)).LE.1.1D0.AND.DABS(ALENS(74,I)).LE.1.1D0
     2            .AND.DABS(ALENS(75,I)).LE.1.1D0) THEN
                      ALENS(3,I)=ALENS(3,I)+(ALENS(3,I)*W3*W4)
                  END IF
              ELSE
                  DO I=INT(W1),INT(W2)
                      GONOGO=.TRUE.
                      CALL CHKTHK(GONOGO,I)
                      IF(.NOT.GONOGO) THEN
                      ELSE
                          IF(DABS(ALENS(46,I)).LE.1.1D0.AND.DABS(ALENS(47,I)).LE.1.1D0
     1                    .AND.DABS(ALENS(48,I)).LE.1.1D0.AND.DABS(ALENS(49,I)).LE.1.1D0
     2                    .AND.DABS(ALENS(50,I)).LE.1.1D0.AND.
     1                    DABS(ALENS(71,I)).LE.1.1D0.AND.DABS(ALENS(72,I)).LE.1.1D0
     1                    .AND.DABS(ALENS(73,I)).LE.1.1D0.AND.DABS(ALENS(74,I)).LE.1.1D0
     2                    .AND.DABS(ALENS(75,I)).LE.1.1D0) THEN
                              ALENS(3,I)=ALENS(3,I)+(ALENS(3,I)*W3*W4)
                          END IF
                      END IF
                  END DO
              END IF
              F1=0
              F6=1
              F22=1
              LNSTYP=1
              CALL LNSEOS
          END IF
          IF(WQ.EQ.'THICK') THEN
              IF(INT(W1).EQ.INT(W2)) THEN
                  I=INT(W1)
                  GONOGO=.TRUE.
                  CALL CHKTHK(GONOGO,I)
                  IF(.NOT.GONOGO) THEN
                      RETURN
                  END IF
                  IF(DABS(ALENS(46,I)).GT.1.1D0.OR.DABS(ALENS(47,I)).GT.1.1D0
     1            .OR.DABS(ALENS(48,I)).GT.1.1D0.OR.DABS(ALENS(49,I)).GT.1.1D0
     2            .OR.DABS(ALENS(50,I)).GT.1.1D0.OR.
     1            DABS(ALENS(71,I)).GT.1.1D0.OR.DABS(ALENS(72,I)).GT.1.1D0
     1            .OR.DABS(ALENS(73,I)).GT.1.1D0.OR.DABS(ALENS(74,I)).GT.1.1D0
     2            .OR.DABS(ALENS(75,I)).GT.1.1D0) THEN
                      ALENS(3,I)=ALENS(3,I)+(ALENS(3,I)*W3*W4)
                  END IF
              ELSE
                  DO I=INT(W1),INT(W2)
                      GONOGO=.TRUE.
                      CALL CHKTHK(GONOGO,I)
                      IF(.NOT.GONOGO) THEN
                      ELSE
                          IF(DABS(ALENS(46,I)).GT.1.1D0.OR.DABS(ALENS(47,I)).GT.1.1D0
     1                    .OR.DABS(ALENS(48,I)).GT.1.1D0.OR.DABS(ALENS(49,I)).GT.1.1D0
     2                    .OR.DABS(ALENS(50,I)).GT.1.1D0.OR.
     1                    DABS(ALENS(71,I)).GT.1.1D0.OR.DABS(ALENS(72,I)).GT.1.1D0
     1                    .OR.DABS(ALENS(73,I)).GT.1.1D0.OR.DABS(ALENS(74,I)).GT.1.1D0
     2                    .OR.DABS(ALENS(75,I)).GT.1.1D0) THEN
                              ALENS(3,I)=ALENS(3,I)+(ALENS(3,I)*W3*W4)
                          END IF
                      END IF
                  END DO
              END IF
              F1=0
              F6=1
              F22=1
              LNSTYP=1
              CALL LNSEOS
          END IF
          IF(WQ.EQ.'SHAPE') THEN
              IF(INT(W1).EQ.INT(W2)) THEN
                  I=INT(W1)
                  GONOGO=.TRUE.
                  CALL CHKCVR(GONOGO,I)
                  IF(.NOT.GONOGO) THEN
                      RETURN
                  END IF
C     CURVATURE IF NOT ZERO
                  FACTOR=1.0D0+(W3*W4)
C     AC
                  ALENS(43,I)=ALENS(43,I)/FACTOR
C     AD
                  ALENS(4,I)=ALENS(4,I)/(FACTOR**3)
C     AE
                  ALENS(5,I)=ALENS(5,I)/(FACTOR**5)
C     AF
                  ALENS(6,I)=ALENS(6,I)/(FACTOR**7)
C     AG
                  ALENS(7,I)=ALENS(7,I)/(FACTOR**9)
C     AH
                  ALENS(81,I)=ALENS(81,I)/(FACTOR**11)
C     AI
                  ALENS(82,I)=ALENS(82,I)/(FACTOR**13)
C     AJ
                  ALENS(83,I)=ALENS(83,I)/(FACTOR**15)
C     AK
                  ALENS(84,I)=ALENS(84,I)/(FACTOR**17)
C     AL
                  ALENS(85,I)=ALENS(85,I)/(FACTOR**19)
C     ADTOR
                  ALENS(37,I)=ALENS(37,I)/(FACTOR**3)
C     AETOR
                  ALENS(38,I)=ALENS(38,I)/(FACTOR**5)
C     AFTOR
                  ALENS(39,I)=ALENS(39,I)/(FACTOR**7)
C     AGTOR
                  ALENS(40,I)=ALENS(40,I)/(FACTOR**9)
C     CV
                  ALENS(1,I)=ALENS(1,I)*(1/FACTOR)
C     CVTOR
                  ALENS(24,I)=ALENS(24,I)*(1/FACTOR)
              ELSE
                  DO I=INT(W1),INT(W2)
                      GONOGO=.TRUE.
                      CALL CHKCVR(GONOGO,I)
                      IF(.NOT.GONOGO) THEN
                      ELSE
                          FACTOR=1.0D0+(W3*W4)
C     AC
                          ALENS(43,I)=ALENS(43,I)/FACTOR
C     AD
                          ALENS(4,I)=ALENS(4,I)/(FACTOR**3)
C     AE
                          ALENS(5,I)=ALENS(5,I)/(FACTOR**5)
C     AF
                          ALENS(6,I)=ALENS(6,I)/(FACTOR**7)
C     AG
                          ALENS(7,I)=ALENS(7,I)/(FACTOR**9)
C     AH
                          ALENS(81,I)=ALENS(81,I)/(FACTOR**11)
C     AI
                          ALENS(82,I)=ALENS(82,I)/(FACTOR**13)
C     AJ
                          ALENS(83,I)=ALENS(83,I)/(FACTOR**15)
C     AK
                          ALENS(84,I)=ALENS(84,I)/(FACTOR**17)
C     AL
                          ALENS(85,I)=ALENS(85,I)/(FACTOR**19)
C     ADTOR
                          ALENS(37,I)=ALENS(37,I)/(FACTOR**3)
C     AETOR
                          ALENS(38,I)=ALENS(38,I)/(FACTOR**5)
C     AFTOR
                          ALENS(39,I)=ALENS(39,I)/(FACTOR**7)
C     AGTOR
                          ALENS(40,I)=ALENS(40,I)/(FACTOR**9)
C     CV
                          ALENS(1,I)=ALENS(1,I)*(1/FACTOR)
C     CVTOR
                          ALENS(24,I)=ALENS(24,I)*(1/FACTOR)
                      END IF
                  END DO
              END IF
              F1=0
              F6=1
              F22=1
              LNSTYP=1
              CALL LNSEOS
          END IF
          RETURN
      END
      SUBROUTINE CHKGLS(GONOGO,I)
          IMPLICIT NONE
          LOGICAL GONOGO
          INTEGER I
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          GONOGO=.TRUE.
          IF(PIKUP(1,I,20).NE.0.0D0) THEN
              GONOGO=.FALSE.
              WRITE(OUTLYNE,*)
     1        '"THERM ',WQ,'" DID NOT MODIFY SURFACE # ',I,' DUE TO GLASS PIKUP'
              CALL SHOWIT(1)
          END IF
          RETURN
      END
      SUBROUTINE CHKGLSP(GONOGO,I)
          IMPLICIT NONE
          LOGICAL GONOGO
          INTEGER I
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          GONOGO=.TRUE.
          IF(PIKUP(1,I,20).NE.0.0D0) THEN
              GONOGO=.FALSE.
              WRITE(OUTLYNE,*)
     1        '"PRES ',WQ,'" DID NOT MODIFY SURFACE # ',I,' DUE TO GLASS PIKUP'
              CALL SHOWIT(1)
          END IF
          RETURN
      END
      SUBROUTINE CHKTHK(GONOGO,I)
          IMPLICIT NONE
          LOGICAL GONOGO,PIKK,SLVV
          INTEGER I
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          PIKK=.FALSE.
          SLVV=.FALSE.
          GONOGO=.TRUE.
          IF(ALENS(33,I).EQ.1.0D0) SLVV=.TRUE.
          IF(ALENS(33,I).EQ.3.0D0) SLVV=.TRUE.
          IF(PIKUP(1,I,3).NE.0.0D0) PIKK=.TRUE.
          IF(PIKUP(1,I,32).NE.0.0D0) PIKK=.TRUE.
          IF(PIKK) THEN
              GONOGO=.FALSE.
              WRITE(OUTLYNE,*)
     1        '"THERM ',WQ,'" DID NOT MODIFY SURFACE # ',I
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)
     1        ' DUE TO "TH" OR "THOAL" PIKUP'
              CALL SHOWIT(1)
          END IF
          IF(SLVV) THEN
              GONOGO=.FALSE.
              WRITE(OUTLYNE,*)
     1        '"THERM ',WQ,'" DID NOT MODIFY SURFACE # ',I,' DUE TO "TH" SOLVE'
              CALL SHOWIT(1)
          END IF
          RETURN
      END
      SUBROUTINE CHKCVR(GONOGO,I)
          IMPLICIT NONE
          LOGICAL GONOGO,PIKK,SLVV
          INTEGER I
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          PIKK=.FALSE.
          SLVV=.FALSE.
          GONOGO=.TRUE.
          IF(ALENS(33,I).EQ.2.0D0) SLVV=.TRUE.
          IF(ALENS(33,I).EQ.3.0D0) SLVV=.TRUE.
          IF(PIKUP(1,I,2).NE.0.0D0) PIKK=.TRUE.
          IF(PIKUP(1,I,1).NE.0.0D0) PIKK=.TRUE.
          IF(PIKUP(1,I,10).NE.0.0D0) PIKK=.TRUE.
          IF(PIKUP(1,I,9).NE.0.0D0) PIKK=.TRUE.
          IF(PIKUP(1,I,4).NE.0.0D0) PIKK=.TRUE.
          IF(PIKUP(1,I,21).NE.0.0D0) PIKK=.TRUE.
          IF(PIKUP(1,I,26).NE.0.0D0) PIKK=.TRUE.
          IF(PIKUP(1,I,5).NE.0.0D0) PIKK=.TRUE.
          IF(PIKUP(1,I,6).NE.0.0D0) PIKK=.TRUE.
          IF(PIKUP(1,I,7).NE.0.0D0) PIKK=.TRUE.
          IF(PIKUP(1,I,8).NE.0.0D0) PIKK=.TRUE.
          IF(PIKUP(1,I,22).NE.0.0D0) PIKK=.TRUE.
          IF(PIKUP(1,I,23).NE.0.0D0) PIKK=.TRUE.
          IF(PIKUP(1,I,24).NE.0.0D0) PIKK=.TRUE.
          IF(PIKUP(1,I,25).NE.0.0D0) PIKK=.TRUE.
          IF(PIKUP(1,I,11).NE.0.0D0) PIKK=.TRUE.
          IF(PIKUP(1,I,12).NE.0.0D0) PIKK=.TRUE.
          IF(PIKK) THEN
              GONOGO=.FALSE.
              WRITE(OUTLYNE,*)
     1        '"THERM ',WQ,'" DID NOT MODIFY SURFACE # ',I,
     1        ' DUE TO A SURFACE SHAPE PIKUP'
              CALL SHOWIT(1)
          END IF
          IF(SLVV) THEN
              GONOGO=.FALSE.
              WRITE(OUTLYNE,*)
     1        '"THERM ',WQ,'" DID NOT MODIFY SURFACE # ',I,
     1        ' DUE TO A CURVATURE SOLVE'
              CALL SHOWIT(1)
          END IF
          RETURN
      END
C SUB TELAIM.FOR
      SUBROUTINE TELAIM
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE TELAIM.FOR. THIS SUBROUTINE CONTROLS
C     TELECENTRIC RAY AIMING
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
C       CHECK FOR STRING INPUT
          IF(SST.EQ.1.OR.SN.EQ.1) THEN
              OUTLYNE='"TEL" TAKES NO STRING OR NUMERIC INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(STI.EQ.0) THEN
              IF(WQ.NE.'ON'.AND.WQ.NE.'OFF') THEN
                  OUTLYNE='"ON", "YES","OFF" AND "NO"'
                  CALL SHOWIT(1)
                  OUTLYNE=' ARE THE ONLY VALID QUALIFIERS'
                  CALL SHOWIT(1)
                  OUTLYNE='USED WITH "TEL"'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C     STI=1
          END IF
          IF(STI.EQ.1) THEN
              IF(SYSTEM1(63).EQ.0.0D0) WRITE(OUTLYNE,10)
              CALL SHOWIT(0)
 10           FORMAT('TELECENTRIC RAY AIMING IS CURRENTLY TURNED "OFF"')
 11           FORMAT('TELECENTRIC RAY AIMING IS CURRENTLY TURNED "ON"')
              IF(SYSTEM1(63).EQ.1.0D0) WRITE(OUTLYNE,11)
              CALL SHOWIT(0)
              RETURN
          END IF
          IF(WQ.EQ.'OFF') THEN
              SYSTEM1(63)=0.0D0
              RETURN
          END IF
          IF(WQ.EQ.'ON') THEN
              IF(DABS(ALENS(3,0)).GE.1.0D10) THEN
                  OUTLYNE=
     1            'TELECENTRIC RAY AIMING MAY NOT BE ACTIVATED BECAUSE THE'
                  CALL SHOWIT(1)
                  OUTLYNE=
     1            'MAGNITUDE OF THE OBJECT DISTANCE IS GREATER THAN OR EQUAL'
                  CALL SHOWIT(1)
                  OUTLYNE=
     1            'TO 1.0D+10'
                  CALL SHOWIT(1)
                  OUTLYNE='NO ACTION TAKEN'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              ELSE
                  SYSTEM1(63)=1.0D0
                  SYSTEM1(62)=0.0D0
                  SYSTEM1(70)=0.0D0
                  NEWOBJ=0
                  NEWIMG=INT(SYSTEM1(20))
C     SHUT OFF REGULAR RAY AIMING
                  SYSTEM1(62)=0.0D0
              END IF
          END IF
          RETURN
      END
C SUB NEARFARNEAR.FOR
      SUBROUTINE NEARFARNEAR
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE NEARFAR.FOR. THIS SUBROUTINE CONTROLS
C     THE CMD LEVEL COMMANDS NEAR AND FAR FOR GOTF AND DOTF UNITS
C     AND WORKS WITH THE O SETTING OF THE SPACE COMMAND
C
          REAL*8 AL
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
          IF(SYSTEM1(6).EQ.1.0D0) AL=DABS(ALENS(3,NEWOBJ))*25.4D0
          IF(SYSTEM1(6).EQ.2.0D0) AL=DABS(ALENS(3,NEWOBJ))*10.0D0
          IF(SYSTEM1(6).EQ.3.0D0) AL=DABS(ALENS(3,NEWOBJ))
          IF(SYSTEM1(6).EQ.4.0D0) AL=DABS(ALENS(3,NEWOBJ))*1000.0D0
C
          IF(STI.EQ.1) THEN
              IF(WC.EQ.'FAR')OUTLYNE='"FAR" SETS UNITS TO LP/MRAD'
              IF(WC.EQ.'NEAR')OUTLYNE='"NEAR" SETS UNITS TO LP/MM'
              CALL SHOWIT(1)
              OUTLYNE='FOR GOTF AND DOTF OPTICAL TRANSFER FUNCTION DISPLAYS'
              CALL SHOWIT(1)
              IF(NEAR_FAR.EQ.0) OUTLYNE='CURRENT SETTING IS "NEAR"'
              IF(NEAR_FAR.EQ.1) OUTLYNE='CURRENT SETTING IS "FAR"'
              CALL SHOWIT(1)
              RETURN
          END IF
          IF(SQ.EQ.1.OR.SN.EQ.1.OR.SST.EQ.1) THEN
              IF(WC.EQ.'FAR')OUTLYNE='"FAR" TAKES NO ADDITIONAL INPUT'
              IF(WC.EQ.'NEAR')OUTLYNE='"NEAR" TAKES NO ADDITIONAL INPUT'
              CALL SHOWIT(1)
              IF(NEAR_FAR.EQ.1) OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(WC.EQ.'NEAR'.AND.AL.GT.1.0D5) THEN
              OUTLYNE='OBJECT DISTANCE GREATER THAN 1.0D+5 MILLIMETERS'
              CALL SHOWIT(1)
              IF(NEAR_FAR.EQ.1) OUTLYNE='"NEAR" NOT ALLOWED'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          ELSE
              NEAR_FAR=0
          END IF
          IF(WC.EQ.'FAR') NEAR_FAR=1
          RETURN
      END
C SUB OVERBOSE.FOR
      SUBROUTINE OVERBOSE
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE OPTMINIT.FOR. THIS SUBROUTINE CONTROLS
C     THE CMD LEVEL COMMAND OPTMINIT
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
C       CHECK FOR STRING INPUT
          IF(SST.EQ.1.OR.SN.EQ.1) THEN
              OUTLYNE='"OVERBOSE" TAKES NO STRING OR NUMERIC INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(STI.EQ.0) THEN
              IF(WQ.NE.'ON'.AND.WQ.NE.'OFF'.AND.SQ.EQ.1) THEN
                  OUTLYNE='"ON", "YES","OFF" AND "NO"'
                  CALL SHOWIT(1)
                  OUTLYNE=' ARE THE ONLY VALID QUALIFIERS'
                  CALL SHOWIT(1)
                  OUTLYNE='USED WITH "OVERBOSE"'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C     STI=1
          END IF
          IF(STI.EQ.1.OR.SQ.EQ.0) THEN
 10           FORMAT('VERBOSE OPTIMIZATION OUTPUT IS CURRENTLY "OFF"')
 11           FORMAT('VERBOSE OPTIMIZATION OUTPUT IS CURRENTLY "ON"')
              IF(SYSTEM1(101).EQ.0.0D0) WRITE(OUTLYNE,10)
              CALL SHOWIT(0)
              IF(SYSTEM1(101).EQ.1.0D0) WRITE(OUTLYNE,11)
              CALL SHOWIT(0)
              RETURN
          END IF
          IF(WQ.EQ.'OFF') THEN
              SYSTEM1(101)=0.0D0
              RETURN
          END IF
          IF(WQ.EQ.'ON') THEN
              SYSTEM1(101)=1.0D0
              RETURN
          END IF
      END
C SUB OPTMINIT.FOR
      SUBROUTINE OPTMINIT
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE OPTMINIT.FOR. THIS SUBROUTINE CONTROLS
C     THE CMD LEVEL COMMAND OPTMINIT
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
C       CHECK FOR STRING INPUT
          IF(SST.EQ.1.OR.SN.EQ.1) THEN
              OUTLYNE='"OPTMINIT" TAKES NO STRING OR NUMERIC INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(STI.EQ.0) THEN
              IF(WQ.NE.'ON'.AND.WQ.NE.'OFF'.AND.SQ.EQ.1) THEN
                  OUTLYNE='"ON", "YES","OFF" AND "NO"'
                  CALL SHOWIT(1)
                  OUTLYNE=' ARE THE ONLY VALID QUALIFIERS'
                  CALL SHOWIT(1)
                  OUTLYNE='USED WITH "OPTMINIT"'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C     STI=1
          END IF
          IF(STI.EQ.1.OR.SQ.EQ.0) THEN
 10           FORMAT('AUTOMATIC OPERAND INITIALIZATION IS CURRENTLY "OFF"')
 11           FORMAT('AUTOMATIC OPERAND INITIALIZATION IS CURRENTLY "ON"')
              IF(OPTM_INIT.EQ.0) WRITE(OUTLYNE,10)
              CALL SHOWIT(0)
              IF(OPTM_INIT.EQ.1) WRITE(OUTLYNE,11)
              CALL SHOWIT(0)
              RETURN
          END IF
          IF(WQ.EQ.'OFF') THEN
              OPTM_INIT=0
              RETURN
          END IF
          IF(WQ.EQ.'ON') THEN
              OPTM_INIT=1
              RETURN
          END IF
      END
C SUB GEOLEICA.FOR
      SUBROUTINE GEOLEICA
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE GEOLEICA.FOR. THIS SUBROUTINE CONTROLS
C       THE COMMAND GEOLEICA
C
          INTEGER I
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          INCLUDE 'datspd.inc'
C
          IF(STI.EQ.1) THEN
              DO I=1,10
                  IF(GLEICA(I)) WRITE(OUTLYNE,10) I
                  IF(.NOT.GLEICA(I)) WRITE(OUTLYNE,11) I
                  CALL SHOWIT(0)
 10               FORMAT('"GEOLEICA" CURRENTLY TURNED "ON" FOR FREQUENCY # ',I2)
 11               FORMAT('"GEOLEICA" CURRENTLY TURNED "OFF" FOR FREQUENCY # ',I2)
              END DO
              RETURN
          END IF
C       CHECK FOR NW1 INPUT
          IF(S1.EQ.0) THEN
              OUTLYNE='"GEOLEICA" REQUIRES EXPLICIT NUMERIC WORD #1'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(INT(W1).NE.1.AND.INT(W1).NE.2.AND.INT(W1).NE.3
     1    .AND.INT(W1).NE.4.AND.INT(W1).NE.5.AND.INT(W1).NE.6
     1    .AND.INT(W1).NE.7.AND.INT(W1).NE.8.AND.INT(W1).NE.9
     1    .AND.INT(W1).NE.10) THEN
              OUTLYNE='"GEOLEICA" REQUIRES 1,2,3,4,5,6,7,8,9 OR 10'
              CALL SHOWIT(1)
              OUTLYNE='AS NUMERIC WORD #1 INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(STI.EQ.0) THEN
              IF(WQ.NE.'ON'.AND.WQ.NE.'OFF') THEN
                  OUTLYNE='"ON", "YES","OFF" AND "NO"'
                  CALL SHOWIT(1)
                  OUTLYNE=' ARE THE ONLY VALID QUALIFIERS'
                  CALL SHOWIT(1)
                  OUTLYNE='USED WITH "TEL"'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C     STI=1
          END IF
C       CHECK FOR STRING INPUT
          IF(SST.EQ.1.OR.S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
              OUTLYNE='"GEOLEICA" TAKES NO STRING OR '
              CALL SHOWIT(1)
              OUTLYNE='NUMERIC WORD #2 THROUGH #5 INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(WQ.EQ.'OFF') THEN
              GLEICA(INT(W1))=.FALSE.
              RETURN
          END IF
          IF(WQ.EQ.'ON') THEN
              GLEICA(INT(W1))=.TRUE.
              RETURN
          END IF
          RETURN
      END
C SUB DIFLEICA.FOR
      SUBROUTINE DIFLEICA
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE DIFLEICA.FOR. THIS SUBROUTINE CONTROLS
C       THE COMMAND DIFLEICA
C
          INTEGER I
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          INCLUDE 'datspd.inc'
C
          IF(STI.EQ.1) THEN
              DO I=1,10
                  IF(DLEICA(I)) WRITE(OUTLYNE,10) I
                  IF(.NOT.DLEICA(I)) WRITE(OUTLYNE,11) I
                  CALL SHOWIT(0)
 10               FORMAT('"DIFLEICA" CURRENTLY TURNED "ON" FOR FREQUENCY # ',I2)
 11               FORMAT('"DIFLEICA" CURRENTLY TURNED "OFF" FOR FREQUENCY # ',I2)
              END DO
              RETURN
          END IF
C       CHECK FOR NW1 INPUT
          IF(S1.EQ.0) THEN
              OUTLYNE='"DIFLEICA" REQUIRES EXPLICIT NUMERIC WORD #1'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(INT(W1).NE.1.AND.INT(W1).NE.2.AND.INT(W1).NE.3
     1    .AND.INT(W1).NE.4.AND.INT(W1).NE.5.AND.INT(W1).NE.6
     1    .AND.INT(W1).NE.7.AND.INT(W1).NE.8.AND.INT(W1).NE.9
     1    .AND.INT(W1).NE.10) THEN
              OUTLYNE='"DIFLEICA" REQUIRES 1,2,3,4,5,6,7,8,9 OR 10'
              CALL SHOWIT(1)
              OUTLYNE='AS NUMERIC WORD #1 INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(STI.EQ.0) THEN
              IF(WQ.NE.'ON'.AND.WQ.NE.'OFF') THEN
                  OUTLYNE='"ON", "YES","OFF" AND "NO"'
                  CALL SHOWIT(1)
                  OUTLYNE=' ARE THE ONLY VALID QUALIFIERS'
                  CALL SHOWIT(1)
                  OUTLYNE='USED WITH "TEL"'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C     STI=1
          END IF
C       CHECK FOR STRING INPUT
          IF(SST.EQ.1.OR.S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
              OUTLYNE='"DIFLEICA" TAKES NO STRING OR '
              CALL SHOWIT(1)
              OUTLYNE='NUMERIC WORD #2 THROUGH #5 INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(WQ.EQ.'OFF') THEN
              DLEICA(INT(W1))=.FALSE.
              RETURN
          END IF
          IF(WQ.EQ.'ON') THEN
              DLEICA(INT(W1))=.TRUE.
              RETURN
          END IF
          RETURN
      END
C SUB SWV.FOR
      SUBROUTINE SWV
C
          IMPLICIT NONE
C
C       THIS SUBROUTINE HANDELS THE ASSIGNMENT OF WAVELENGTHS.
C       SUBROUTINE LENNS SETS THE DEFAULT VALUES OF WV1 TO WV5.
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
C       THIS IS SUBROUTINE SWV WHICH IMPLEMENTS THE WV COMMAND
C       AT THE LENS OF UPDATE LENS LEVEL OR THE WV COMMAND AT
C       THE CMD LEVEL. AT THE CMD LEVEL, NUMERIC INPUT INCLUDED
C       WITH THE WV COMMAND IS IGNORED.
C
          IF(F5.EQ.1.OR.F6.EQ.1) THEN
              IF(STI.EQ.1) THEN
                  WRITE(OUTLYNE,2001)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,3001)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,10001) SYSTEM1(1),SYSTEM1(2),SYSTEM1(3),
     1            SYSTEM1(4),SYSTEM1(5)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,200)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,300)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,1000) SYSTEM1(71),SYSTEM1(72),SYSTEM1(73),
     1            SYSTEM1(74),SYSTEM1(75)
                  CALL SHOWIT(0)
                  RETURN
              ELSE
C       NOT STI
              END IF
          ELSE
C       AT CMD
          END IF
          IF(F1.EQ.1) THEN
C
C               CHECK FOR ADDITIONAL INPUT
C       AND IF FOUND PRINT ERROR MESSAGE.
C
              IF(SQ.EQ.1.OR.SST.EQ.1.OR.SN.EQ.1) THEN
                  OUTLYNE='AT THE CMD LEVEL, "WV" TAKES NO EXPLICIT INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              WRITE(OUTLYNE,2001)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,3001)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,10001) SYSTEM1(1),SYSTEM1(2),SYSTEM1(3),
     1        SYSTEM1(4),SYSTEM1(5)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,200)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,300)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,1000) SYSTEM1(71),SYSTEM1(72),SYSTEM1(73),
     1                  SYSTEM1(74),SYSTEM1(75)
              CALL SHOWIT(0)
              RETURN
          ELSE
C               NOT AT CMD LEVEL
C
              IF(F5.EQ.1.OR.F6.EQ.1) THEN
C               CHECK FOR PRESENCE OF QUALIFIER OR STRING
C       AND IF FOUND PRINT ERROR MESSAGE.
C
                  IF(SQ.EQ.1.OR.SST.EQ.1) THEN
                      OUTLYNE='"WV" COMMAND ONLY ACCEPTS NUNERIC INPUT'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  IF(DF1.EQ.1.AND.DF2.EQ.1.AND.DF3.EQ.1.AND.DF4.EQ.1
     1            .AND.DF5.EQ.1) THEN
                      OUTLYNE='"WV" REQUIRES SOME EXPLICIT NUMERIC INPUT'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
C
C               WE ARE AT LENS INPUT OR LENS UPDATE LEVEL
C
                  IF(DF1.EQ.0) SYSTEM1(1)=W1
                  IF(DF2.EQ.0) SYSTEM1(2)=W2
                  IF(DF3.EQ.0) SYSTEM1(3)=W3
                  IF(DF4.EQ.0) SYSTEM1(4)=W4
                  IF(DF5.EQ.0) SYSTEM1(5)=W5
                  IF(DF1.EQ.0) SYSTEM1(111)=W1
                  IF(DF2.EQ.0) SYSTEM1(112)=W2
                  IF(DF3.EQ.0) SYSTEM1(113)=W3
                  IF(DF4.EQ.0) SYSTEM1(114)=W4
                  IF(DF5.EQ.0) SYSTEM1(115)=W5
                  IF(SYSTEM1(1).EQ.0.0D0) SYSTEM1(31)=0.0D0
                  IF(SYSTEM1(2).EQ.0.0D0) SYSTEM1(32)=0.0D0
                  IF(SYSTEM1(3).EQ.0.0D0) SYSTEM1(33)=0.0D0
                  IF(SYSTEM1(4).EQ.0.0D0) SYSTEM1(34)=0.0D0
                  IF(SYSTEM1(5).EQ.0.0D0) SYSTEM1(35)=0.0D0
                  IF(SYSTEM1(71).EQ.0.0D0) SYSTEM1(76)=0.0D0
                  IF(SYSTEM1(72).EQ.0.0D0) SYSTEM1(77)=0.0D0
                  IF(SYSTEM1(73).EQ.0.0D0) SYSTEM1(78)=0.0D0
                  IF(SYSTEM1(74).EQ.0.0D0) SYSTEM1(79)=0.0D0
                  IF(SYSTEM1(75).EQ.0.0D0) SYSTEM1(80)=0.0D0
                  F22=1
              ELSE
              END IF
          END IF
 2001     FORMAT('CURRENT LENS FILE WAVELENGTHS #1 TO #5 ARE:')
 3001     FORMAT(4X,'WV(1)',9X,'WV(2)',9X,'WV(3)',
     1    9X,'WV(4)',9X,'WV(5)')
10001     FORMAT(G13.7,1X,G13.7,1X,G13.7,1X,G13.7,1X,G13.7)
 200      FORMAT('CURRENT LENS FILE WAVELENGTHS #6 TO #10 ARE:')
 300      FORMAT(4X,'WV(6)',9X,'WV(7)',9X,'WV(8)',
     1    9X,'WV(9)',9X,'WV(10)')
 1000     FORMAT(G13.7,1X,G13.7,1X,G13.7,1X,G13.7,1X,G13.7)
          RETURN
      END
C SUB SWV2.FOR
      SUBROUTINE SWV2
C
          IMPLICIT NONE
C
C       THIS SUBROUTINE HANDELS THE ASSIGNMENT OF WAVELENGTHS.
C       SUBROUTINE LENNS SETS THE DEFAULT VALUES OF WV6 TO WV10.
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
C       THIS IS SUBROUTINE SWV WHICH IMPLEMENTS THE WV COMMAND
C       AT THE LENS OF UPDATE LENS LEVEL OR THE WV COMMAND AT
C       THE CMD LEVEL. AT THE CMD LEVEL, NUMERIC INPUT INCLUDED
C       WITH THE WV COMMAND IS IGNORED.
C
          IF(F5.EQ.1.OR.F6.EQ.1) THEN
              IF(STI.EQ.1) THEN
                  WRITE(OUTLYNE,2001)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,3001)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,10001) SYSTEM1(1),SYSTEM1(2),SYSTEM1(3),
     1            SYSTEM1(4),SYSTEM1(5)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,200)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,300)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,1000) SYSTEM1(71),SYSTEM1(72),SYSTEM1(73),
     1            SYSTEM1(74),SYSTEM1(5)
                  CALL SHOWIT(0)
                  RETURN
              ELSE
C       NOT STI
              END IF
          ELSE
C       AT CMD
          END IF
          IF(F1.EQ.1) THEN
C
C               CHECK FOR ADDITIONAL INPUT
C       AND IF FOUND PRINT ERROR MESSAGE.
C
              IF(SQ.EQ.1.OR.SST.EQ.1.OR.SN.EQ.1) THEN
                  OUTLYNE='AT THE CMD LEVEL, "WV2" TAKES NO EXPLICIT INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              WRITE(OUTLYNE,2001)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,3001)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,10001) SYSTEM1(1),SYSTEM1(2),SYSTEM1(3),
     1        SYSTEM1(4),SYSTEM1(5)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,200)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,300)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,1000) SYSTEM1(71),SYSTEM1(72),SYSTEM1(73),
     1        SYSTEM1(74),SYSTEM1(75)
              CALL SHOWIT(0)
              RETURN
          ELSE
C               NOT AT CMD LEVEL
C
              IF(F5.EQ.1.OR.F6.EQ.1) THEN
C               CHECK FOR PRESENCE OF QUALIFIER OR STRING
C       AND IF FOUND PRINT ERROR MESSAGE.
C
                  IF(SQ.EQ.1.OR.SST.EQ.1) THEN
                      OUTLYNE='"WV2" COMMAND ONLY ACCEPTS NUNERIC INPUT'
                      CALL SHOWIT(0)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(0)
                      CALL MACFAL
                      RETURN
                  END IF
                  IF(DF1.EQ.1.AND.DF2.EQ.1.AND.DF3.EQ.1.AND.DF4.EQ.1
     1            .AND.DF5.EQ.1) THEN
                      OUTLYNE='"WV2" REQUIRES SOME EXPLICIT NUMERIC INPUT'
                      CALL SHOWIT(0)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(0)
                      CALL MACFAL
                      RETURN
                  END IF
C
C               WE ARE AT LENS INPUT OR LENS UPDATE LEVEL
C
                  IF(DF1.EQ.0) SYSTEM1(71)=W1
                  IF(DF2.EQ.0) SYSTEM1(72)=W2
                  IF(DF3.EQ.0) SYSTEM1(73)=W3
                  IF(DF4.EQ.0) SYSTEM1(74)=W4
                  IF(DF5.EQ.0) SYSTEM1(75)=W5
                  IF(DF1.EQ.0) SYSTEM1(116)=W1
                  IF(DF2.EQ.0) SYSTEM1(117)=W2
                  IF(DF3.EQ.0) SYSTEM1(118)=W3
                  IF(DF4.EQ.0) SYSTEM1(119)=W4
                  IF(DF5.EQ.0) SYSTEM1(120)=W5
                  IF(SYSTEM1(1).EQ.0.0D0) SYSTEM1(31)=0.0D0
                  IF(SYSTEM1(2).EQ.0.0D0) SYSTEM1(32)=0.0D0
                  IF(SYSTEM1(3).EQ.0.0D0) SYSTEM1(33)=0.0D0
                  IF(SYSTEM1(4).EQ.0.0D0) SYSTEM1(34)=0.0D0
                  IF(SYSTEM1(5).EQ.0.0D0) SYSTEM1(35)=0.0D0
                  IF(SYSTEM1(71).EQ.0.0D0) SYSTEM1(76)=0.0D0
                  IF(SYSTEM1(72).EQ.0.0D0) SYSTEM1(77)=0.0D0
                  IF(SYSTEM1(73).EQ.0.0D0) SYSTEM1(78)=0.0D0
                  IF(SYSTEM1(74).EQ.0.0D0) SYSTEM1(79)=0.0D0
                  IF(SYSTEM1(75).EQ.0.0D0) SYSTEM1(80)=0.0D0
                  F22=1
              ELSE
              END IF
          END IF
 2001     FORMAT('CURRENT LENS FILE WAVELENGTHS #1 TO #5 ARE:')
 3001     FORMAT(4X,'WV(1)',9X,'WV(2)',9X,'WV(3)',
     1    9X,'WV(4)',9X,'WV(5)')
10001     FORMAT(G13.7,1X,G13.7,1X,G13.7,1X,G13.7,1X,G13.7)
 200      FORMAT('CURRENT LENS FILE WAVELENGTHS #6 TO #10 ARE:')
 300      FORMAT(4X,'WV(6)',9X,'WV(7)',9X,'WV(8)',
     1    9X,'WV(9)',9X,'WV(10)')
 1000     FORMAT(G13.7,1X,G13.7,1X,G13.7,1X,G13.7,1X,G13.7)
          RETURN
      END
C SUB SVSET.FOR
      SUBROUTINE SVSET
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE SVSET .
C       IT RE-SETS ALL THE YZ/XZ-PLANE SOLVE DATUM ACCORDING TO THE
C       CURRENT YZ/YZ-PLANE PARAXIAL DATA
C
          INTEGER ITYPEP,I
C
          COMMON/PTYPE/ITYPEP
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
          IF(ITYPEP.EQ.1) THEN
              DO 10 I=0,((INT(SYSTEM1(20)))-1)
                  IF(SOLVE(6,I).EQ.1.0D0) THEN
C       PY SOLVE
                      SOLVE(7,I)=PXTRAY(1,(I+1))
                  ELSE
C       NOT PY SOLVE
                  END IF
                  IF(SOLVE(6,I).EQ.2.0D0) THEN
C       PCY SOLVE
                      SOLVE(7,I)=PXTRAY(5,(I+1))
                  ELSE
C       NOT PCY SOLVE
                  END IF
                  IF(SOLVE(8,I).EQ.1.0D0) THEN
C       APY SOLVE
                      SOLVE(9,I)=0.0D0
                  ELSE
C       NOT APY SOLVE
                  END IF
                  IF(SOLVE(8,I).EQ.2.0D0) THEN
C       PIY SOLVE
                      SOLVE(9,I)=PXTRAY(3,I)
                  ELSE
C       NOT PIY SOLVE
                  END IF
                  IF(SOLVE(8,I).EQ.3.0D0) THEN
C       PUY SOLVE
                      SOLVE(9,I)=PXTRAY(2,I)
                  ELSE
C       NOT PUY SOLVE
                  END IF
                  IF(SOLVE(8,I).EQ.4.0D0) THEN
C       APCY SOLVE
                      SOLVE(9,I)=0.0D0
                  ELSE
C       NOT APCY SOLVE
                  END IF
                  IF(SOLVE(8,I).EQ.5.0D0) THEN
C       PICY SOLVE
                      SOLVE(9,I)=PXTRAY(7,I)
                  ELSE
C       NOT PICY SOLVE
                  END IF
                  IF(SOLVE(8,I).EQ.6.0D0) THEN
C       PUCY SOLVE
                      SOLVE(9,I)=PXTRAY(6,I)
                  ELSE
C       NOT PUCY SOLVE
                  END IF
 10           CONTINUE
C       ALL SOLVE TARGETS RE-SET
              RETURN
          ELSE
C       ITYPEP NOT 1
          END IF
C
          IF(ITYPEP.EQ.2) THEN
C
              DO 100 I=0,((INT(SYSTEM1(20)))-1)
                  IF(SOLVE(6,I).EQ.1.0D0) THEN
C       PX SOLVE
                      SOLVE(7,I)=PXTRAX(1,(I+1))
                  ELSE
C       NOT PX SOLVE
                  END IF
                  IF(SOLVE(6,I).EQ.2.0D0) THEN
C       PCX SOLVE
                      SOLVE(7,I)=PXTRAX(5,(I+1))
                  ELSE
C       NOT PCX SOLVE
                  END IF
                  IF(SOLVE(2,I).EQ.1.0D0) THEN
C       APX SOLVE
                      SOLVE(1,I)=0.0D0
                  ELSE
C       NOT APX SOLVE
                  END IF
                  IF(SOLVE(2,I).EQ.2.0D0) THEN
C       PIX SOLVE
                      SOLVE(1,I)=PXTRAX(3,I)
                  ELSE
C       NOT PIX SOLVE
                  END IF
                  IF(SOLVE(2,I).EQ.3.0D0) THEN
C       PUX SOLVE
                      SOLVE(1,I)=PXTRAX(2,I)
                  ELSE
C       NOT PUX SOLVE
                  END IF
                  IF(SOLVE(2,I).EQ.4.0D0) THEN
C       APCX SOLVE
                      SOLVE(1,I)=0.0D0
                  ELSE
C       NOT APCX SOLVE
                  END IF
                  IF(SOLVE(2,I).EQ.5.0D0) THEN
C       PICX SOLVE
                      SOLVE(1,I)=PXTRAX(7,I)
                  ELSE
C       NOT PICX SOLVE
                  END IF
                  IF(SOLVE(2,I).EQ.6.0D0) THEN
C       PUCX SOLVE
                      SOLVE(1,I)=PXTRAX(6,I)
                  ELSE
C       NOT PUCX SOLVE
                  END IF
 100          CONTINUE
C       ALL SOLVE TARGETS RE-SET
              RETURN
          ELSE
C       ITYPEP NOT 2
          END IF
          RETURN
      END
C SUB SUNITS.FOR
      SUBROUTINE SUNITS
C
          IMPLICIT NONE
C
C       THIS SUBROUTINE HANDELS THE UNITS COMMAND BOTH AT
C       THE CMD LEVEL AND AT THE LENS AND LENS UPDATE LEVEL.
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'

C
          IF(F5.EQ.1.OR.F6.EQ.1) THEN
              IF(STI.EQ.1) THEN
                  IF(SYSTEM1(6).EQ.1.0D0) THEN
                      WRITE(OUTLYNE,1000)
                      CALL SHOWIT(0)
                  END IF
                  IF(SYSTEM1(6).EQ.2.0D0) THEN
                      WRITE(OUTLYNE,2000)
                      CALL SHOWIT(0)
                  END IF
                  IF(SYSTEM1(6).EQ.3.0D0) THEN
                      WRITE(OUTLYNE,3000)
                      CALL SHOWIT(0)
                  END IF
                  IF(SYSTEM1(6).EQ.4.0D0) THEN
                      WRITE(OUTLYNE,4000)
                      CALL SHOWIT(0)
                  END IF
                  RETURN
              ELSE
C       NOT STI
              END IF
          ELSE
          END IF
          IF(F1.EQ.1) THEN
C
C               CHECK FOR ADDITIONAL INPUT AND
C               PRINT ERROR AND RETURN IF DISCOVERED.
C
              IF(SST.EQ.1.OR.SQ.EQ.1.OR.SN.EQ.1)THEN
                  OUTLYNE='AT THE CMD LEVEL, "UNITS" TAKES NO EXPLICIT INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C       WHAT IF NO SURFACES EXIST
              IF(SYSTEM1(20).EQ.0.0D0) THEN
                  OUTLYNE='UNITS ARE NOT DEFINED'
                  CALL SHOWIT(1)
                  OUTLYNE='LENS SYSTEM HAS NO SURFACES'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(SYSTEM1(6).EQ.1.0D0) THEN
                  WRITE(OUTLYNE,1000)
                  CALL SHOWIT(0)
              END IF
              IF(SYSTEM1(6).EQ.2.0D0) THEN
                  WRITE(OUTLYNE,2000)
                  CALL SHOWIT(0)
              END IF
              IF(SYSTEM1(6).EQ.3.0D0) THEN
                  WRITE(OUTLYNE,3000)
                  CALL SHOWIT(0)
              END IF
              IF(SYSTEM1(6).EQ.4.0D0) THEN
                  WRITE(OUTLYNE,4000)
                  CALL SHOWIT(0)
              END IF
              RETURN
          ELSE
C               NOT AT CMD LEVEL
C
              IF(F5.EQ.1.OR.F6.EQ.1) THEN
C
C
C               CHECK FOR PRESENCE OF STRING OR NUMERIC WORDS
C               PRINT ERROR AND RETURN IF DISCOVERED.
C
                  IF(SST.EQ.1.OR.SN.EQ.1)THEN
                      OUTLYNE=
     1                '"UNITS" COMMAND ONLY TAKES QUALIFIER WORD INPUT'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF

C               WE ARE AT LENS INPUT OR LENS UPDATE LEVEL
C
                  IF(SQ.EQ.0) THEN
                      OUTLYNE='"UNITS" REQUIRES EXPLICIT QUALIFIER WORD INPUT"'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      RETURN
                  END IF
                  IF(WQ.EQ.'IN'.OR.WQ.EQ.'INCH'.OR.WQ.EQ.
     1            'INCHES') SYSTEM1(6)=1.0D0
                  IF(WQ.EQ.'CM') SYSTEM1(6)=2.0D0
                  IF(WQ.EQ.'MM') SYSTEM1(6)=3.0D0
                  IF(WQ.EQ.'M') SYSTEM1(6)=4.0D0
C     2/93 THE ADJUSTMENT OF THE PLOTTED SCALE FACTOR
C
C     THE SCALE FACTOR IS:
                  IF(SYSTEM1(6).EQ.1.0D0) SCFAY=SCFAYP
                  IF(SYSTEM1(6).EQ.1.0D0) SCFAX=SCFAXP
                  IF(SYSTEM1(6).EQ.2.0D0) SCFAY=SCFAYP*2.54D0
                  IF(SYSTEM1(6).EQ.2.0D0) SCFAX=SCFAXP*2.54D0
                  IF(SYSTEM1(6).EQ.3.0D0) SCFAY=SCFAYP*25.4D0
                  IF(SYSTEM1(6).EQ.3.0D0) SCFAX=SCFAXP*25.4D0
                  IF(SYSTEM1(6).EQ.4.0D0) SCFAY=SCFAYP*0.0254
                  IF(SYSTEM1(6).EQ.4.0D0) SCFAX=SCFAXP*0.0254
                  PSIZY=1.0D0/SCFAY
                  PSIZX=1.0D0/SCFAX
C
C
                  IF(WQ.NE.'IN'.AND.WQ.NE.'INCH'.AND.WQ.NE.'INCHES'
     1            .AND.WQ.NE.'CM'.AND.WQ.NE.'MM'.AND.WQ.NE.'M') THEN
                      OUTLYNE='INVALID UNITS DESCRIPTION REQUESTED'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
          END IF
 1000     FORMAT('UNITS = INCH')
 2000     FORMAT('UNITS = CENTIMETER')
 3000     FORMAT('UNITS = MILLIMETER')
 4000     FORMAT('UNITS = METER')
          RETURN
      END
C SUB STORIC.FOR
      SUBROUTINE STORIC
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE STORIC WHICH IMPLEMENTS THE YTORIC OR XTORIC
C       COMMANDS AT THE LENS INPUT AND UPDATE LENS LEVEL.
C       THE TORIC COMMAND MUST BE ISSUED BEFORE TORIC DATA
C       CAN BE ENTERD FOR A SURFACE. WHEN A SURFACE IS CHANGED
C       FROM A Y-TORIC TO AN X-TORIC, ALL SOLVE DATA FOR THAT SURFACE
C       IS DELETED TO PREVENT CATASTROPHIC CHANGES TO THE LENS
C       SYSTEM. A MESSAGE TO THAT EFFECT IS PRINTED. USE PARAXIAL DATA
C       TO SET ANY NEW SOLVES. IN ORDER TO AVOID ANY ARBITRARY ASSIGNMENT OF
C       SOLVES, EXISTING SOLVES ARE DELETED WHEN A NON-TORIC SURFACE IS
C       DEFINED AS A Y-TORIC OR AN X-TORIC.
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
C
          IF(F5.EQ.1.OR.F6.EQ.1) THEN
C       AT LENS OR UPDATE LENS MODE
C
              IF(SURF.EQ.0) THEN
C       AT OBJECT SURFACE
                  OUTLYNE='OBJECT SURFACE MAY NOT BE TORIC'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  RETURN
              END IF
C
C               CHECK FOR PRESENCE OF QUALIFIER OR STRING INPUT
C               PRINT ERROR AND RETURN IF DISCOVERED.
C
              IF(SST.EQ.1.OR.SQ.EQ.1.OR.SN.EQ.1) THEN
                  OUTLYNE='"X-TORIC" AND "Y-TORIC" TAKE NO EXPLICIT INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C
C       IF THE SURFACE WAS NOT A TORIC BEFORE THEN JUST SET IT AS
C       SUCH AND RETURN.
C
              IF(ALENS(23,SURF).EQ.0.0D0) THEN
C       FIX PIKUP PRO AND NPRO
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
                  IF(WC.EQ.'YTORIC') ALENS(23,SURF)=1.0D0
                  IF(WC.EQ.'XTORIC') ALENS(23,SURF)=2.0D0
                  IF(ALENS(43,SURF).NE.0.0D0) THEN
                      ALENS(43,SURF)=0.0D0
                      OUTLYNE='WARNING:'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'FOR SURFACE ',SURF
                      CALL SHOWIT(1)
                      OUTLYNE=
     1                'THE "AC" TERM RESET TO 0.0D0 FOR THIS TORIC SURFACE'
                      CALL SHOWIT(1)
                  END IF
C
C       SET TORIC CURV = TO PROFILE CURVATURE
                  IF(WC.EQ.'XTORIC'.OR.WC.EQ.'YTORIC') ALENS(24,SURF)=
     1            ALENS(1,SURF)
                  IF(ALENS(33,SURF).NE.0.0D0) THEN
C       THERE WERE SOLVES. PRINT MESSAGE THEN DELETE ALL SOLVES FOR
C       SURF.
                      IF(SOLVE(6,SURF).GT.0.0D0)THEN
                          OUTLYNE=
     1                    'YZ PLANE THICKNESS SOLVE DELETED'
                          CALL SHOWIT(1)
                      END IF
                      IF(SOLVE(4,SURF).GT.0.0D0) THEN
                          OUTLYNE=
     1                    'XZ PLANE THICKNESS SOLVE DELETED'
                          CALL SHOWIT(1)
                      END IF
                      IF(SOLVE(8,SURF).GT.0.0D0) THEN
                          OUTLYNE=
     1                    'YZ PLANE THICKNESS SOLVE DELETED'
                          CALL SHOWIT(1)
                      END IF
                      IF(SOLVE(2,SURF).GT.0.0D0) THEN
                          OUTLYNE=
     1                    'XZ PLANE THICKNESS SOLVE DELETED'
                          CALL SHOWIT(1)
                          WRITE(OUTLYNE,*)'SURFACE',SURF,' :NO SOLVES REMAIN'
                          CALL SHOWIT(1)
                      END IF
                      ALENS(33,SURF)=0.0D0
                      SOLVE(0:9,SURF)=0.0D0
                  ELSE
                  END IF
                  RETURN
C
              ELSE
              END IF
C
C       ALENS(23,SURF) SHOULD BE EITHER 1.0D0 OR 2.0D0,ITS NOT ZER0
C
              IF(ALENS(23,SURF).NE.1.0D0.AND.ALENS(23,SURF).NE.2.0D0) THEN
                  OUTLYNE='SERIOUS ERROR IN ASSIGNING TORICS'
                  CALL SHOWIT(1)
                  RETURN
              END IF
C       TORIC TYPE CHANGE OR REDEFINED
C
              IF(WC.EQ.'YTORIC'.AND.ALENS(23,SURF).EQ.1.0D0) THEN
                  WRITE(OUTLYNE,*)'SURFACE',SURF,' ALREADY DEFINED AS Y-TORIC'
                  CALL SHOWIT(1)
                  RETURN
              END IF
              IF(WC.EQ.'XTORIC'.AND.ALENS(23,SURF).EQ.2.0D0) THEN
                  WRITE(OUTLYNE,*)'SURFACE',SURF,' ALREADY DEFINED AS X-TORIC'
                  CALL SHOWIT(1)
                  RETURN
              ELSE
              END IF
              IF(WC.EQ.'YTORIC'.AND.ALENS(23,SURF).EQ.2.0D0) THEN
                  WRITE(OUTLYNE,*)'SURFACE',SURF,' X-TORIC CHANGED TO Y-TORIC'
                  CALL SHOWIT(1)
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
                  ALENS(23,SURF)=1.0D0
                  IF(ALENS(33,SURF).NE.0.0D0) THEN
C       THERE WERE SOLVES. PRINT MESSAGE THEN DELETE ALL SOLVES FOR
C       SURF.
                      IF(SOLVE(6,SURF).GT.0.0D0) THEN
                          OUTLYNE=
     1                    'YZ PLANE THICKNESS SOLVE DELETED'
                          CALL SHOWIT(1)
                      END IF
                      IF(SOLVE(4,SURF).GT.0.0D0) THEN
                          OUTLYNE=
     1                    'XZ PLANE THICKNESS SOLVE DELETED'
                          CALL SHOWIT(1)
                      END IF
                      IF(SOLVE(8,SURF).GT.0.0D0) THEN
                          OUTLYNE=
     1                    'YZ PLANE THICKNESS SOLVE DELETED'
                          CALL SHOWIT(1)
                      END IF
                      IF(SOLVE(2,SURF).GT.0.0D0) THEN
                          OUTLYNE=
     1                    'XZ PLANE THICKNESS SOLVE DELETED'
                          CALL SHOWIT(1)
                          WRITE(OUTLYNE,*)'SURFACE',SURF,' :NO SOLVES REMAIN'
                          CALL SHOWIT(1)
                      END IF
                      ALENS(33,SURF)=0.0D0
                      SOLVE(0:9,SURF)=0.0D0
                  ELSE
                  END IF
                  RETURN
              ELSE
              END IF
              IF(WC.EQ.'XTORIC'.AND.ALENS(23,SURF).EQ.1.0D0) THEN
                  WRITE(OUTLYNE,*)'SURFACE',SURF,' Y-TORIC CHANGED TO X-TORIC'
                  CALL SHOWIT(1)
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
                  ALENS(23,SURF)=2.0D0
                  IF(ALENS(33,SURF).NE.0.0D0) THEN
C       THERE WERE SOLVES. PRINT MESSAGE THEN DELETE ALL SOLVES FOR
C       SURF.
                      IF(SOLVE(6,SURF).GT.0.0D0) THEN
                          OUTLYNE=
     1                    'YZ PLANE THICKNESS SOLVE DELETED'
                          CALL SHOWIT(1)
                      END IF
                      IF(SOLVE(4,SURF).GT.0.0D0) THEN
                          OUTLYNE=
     1                    'XZ PLANE THICKNESS SOLVE DELETED'
                          CALL SHOWIT(1)
                      END IF
                      IF(SOLVE(8,SURF).GT.0.0D0) THEN
                          OUTLYNE=
     1                    'YZ PLANE THICKNESS SOLVE DELETED'
                          CALL SHOWIT(1)
                      END IF
                      IF(SOLVE(2,SURF).GT.0.0D0) THEN
                          OUTLYNE=
     1                    'XZ PLANE THICKNESS SOLVE DELETED'
                          CALL SHOWIT(1)
                          WRITE(OUTLYNE,*)'SURFACE',SURF,' :NO SOLVES REMAIN'
                          CALL SHOWIT(1)
                      END IF
                      ALENS(33,SURF)=0.0D0
                      SOLVE(0:9,SURF)=0.0D0
                  ELSE
                  END IF
                  RETURN
              ELSE
              END IF
          ELSE
          END IF
          RETURN
      END
C SUB STORD.FOR
      SUBROUTINE STORD
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE STORD WHICH IMPLEMENTS THE TORD
C       COMMAND AT THE LENS INPUT AND UPDATE LENS LEVEL.
C
          INTEGER I,SF
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
C
          IF(SQ.EQ.1.OR.SST.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
              OUTLYNE=
     1        '"TORD" TAKES NO STRING OR QUALIFIER'
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
              IF(SURF.EQ.0) THEN
                  OUTLYNE='OBJECT SURFACE IS ALWAYS PLANO'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              ELSE
              END IF
              W1=DBLE(SURF)
              W2=DBLE(SURF)
              S1=1
              S2=1
              DF1=0
              DF2=0
          END IF
          IF(DF1.EQ.1.AND.DF2.EQ.0.OR.DF1.EQ.0.AND.DF2.EQ.1) THEN
              OUTLYNE=
     1        '"TORD" USES EITHER TWO OR ZERO NUMERIC WORDS'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          ELSE
C       PROCEED
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

C               WE ARE AT LENS INPUT OR LENS UPDATE LEVEL
C
C       IF THE SURFACE WAS NOT A TORIC BEFORE THEN JUST PRINT A
C       MESSAGE AND RETURN.
C
              IF(ALENS(23,SF).EQ.0.0D0) THEN
                  WRITE(OUTLYNE,*)'SURFACE',SF,' NOT DEFINED AS TORIC'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              ELSE
C
C       IF THE SURFACE WAS A Y-TORIC THEN GET RID OF ALL XZ PLANE
C       SOLVES. IF X-TORIC, GET RID OF ALL YZ PLANE SOLVES
C       SURFACE IS A TORIC
C
                  IF(ALENS(23,SF).EQ.1.0D0) THEN
C       Y-TORIC,GET RIDE OF XZ PLANE SOLVES, ALL OF THEM
                      IF(SOLVE(4,SF).GT.0.0D0.OR.SOLVE(2,SF).GT.0.0D0
     1                .OR.SOLVE(4,SF).GT.0.0D0.AND.SOLVE(2,SF).GT.0.0D0)
     2                 THEN
                          SOLVE(4,SF)=0.0D0
                          SOLVE(3,SF)=0.0D0
                          SOLVE(2,SF)=0.0D0
                          SOLVE(1,SF)=0.0D0
                          WRITE(OUTLYNE,*)'SURFACE',SF,' :ALL XZ PLANE SOLVES DELETED'
                          CALL SHOWIT(1)
                      END IF
                  END IF
                  IF(ALENS(23,SF).EQ.2.0D0) THEN
C       X-TORIC,GET RIDE OF YZ PLANE SOLVES, ALL OF THEM
                      IF(SOLVE(6,SF).GT.0.0D0.OR.SOLVE(8,SF).GT.0.0D0
     1                .OR.SOLVE(6,SF).GT.0.0D0.AND.SOLVE(8,SF).GT.0.0D0)
     2                 THEN
                          SOLVE(6,SF)=0.0D0
                          SOLVE(7,SF)=0.0D0
                          SOLVE(8,SF)=0.0D0
                          SOLVE(9,SF)=0.0D0
                          WRITE(OUTLYNE,*)'SURFACE',SF,' :ALL YZ PLANE SOLVES DELETED'
                          CALL SHOWIT(1)
                      END IF
C       THEN MAKE ALL EXISTING XZ PLANE SOLVES INTO YZ PLANE SOLVES
                      IF(SOLVE(4,SF).GT.0.0D0.OR.SOLVE(2,SF).GT.0.0D0.OR.
     1                SOLVE(4,SF).GT.0.0D0.AND.SOLVE(2,SF).GT.0.0D0) THEN
                          IF(SOLVE(4,SF).EQ.4.0D0) SOLVE(6,SF)=1.0D0
                          IF(SOLVE(4,SF).EQ.5.0D0) SOLVE(6,SF)=2.0D0
                          IF(SOLVE(4,SF).EQ.6.0D0) SOLVE(6,SF)=3.0D0
                          IF(SOLVE(2,SF).EQ.8.0D0) SOLVE(8,SF)=1.0D0
                          IF(SOLVE(2,SF).EQ.9.0D0) SOLVE(8,SF)=2.0D0
                          IF(SOLVE(2,SF).EQ.10.0D0) SOLVE(8,SF)=3.0D0
                          IF(SOLVE(2,SF).EQ.11.0D0) SOLVE(8,SF)=4.0D0
                          IF(SOLVE(2,SF).EQ.12.0D0) SOLVE(8,SF)=5.0D0
                          IF(SOLVE(2,SF).EQ.13.0D0) SOLVE(8,SF)=6.0D0
                          IF(SOLVE(2,SF).EQ.14.0D0) SOLVE(8,SF)=7.0D0
                          SOLVE(7,SF)=SOLVE(3,SF)
                          SOLVE(9,SF)=SOLVE(1,SF)
C       THEN ERASE THE OLD XZ SOLVES
                          SOLVE(4,SF)=0.0D0
                          SOLVE(3,SF)=0.0D0
                          SOLVE(2,SF)=0.0D0
                          SOLVE(1,SF)=0.0D0
                          WRITE(OUTLYNE,*)'SURFACE',SF,
     1                    ' :ALL XZ PLANE SOLVES CONVERTED TO YZ PLANE SOLVES'
                          CALL SHOWIT(1)
                      END IF
                  END IF
C       RECALCULATE ALENS(33,SF)
                  ALENS(33,SF)=0.0D0
                  IF(SOLVE(6,SF).GT.0.0D0) ALENS(33,SF)=ALENS(33,SF)+1.0D0
                  IF(SOLVE(4,SF).GT.0.0D0) ALENS(33,SF)=ALENS(33,SF)+0.1D0
                  IF(SOLVE(8,SF).GT.0.0D0) ALENS(33,SF)=ALENS(33,SF)+2.0D0
                  IF(SOLVE(2,SF).GT.0.0D0) ALENS(33,SF)=ALENS(33,SF)+0.2D0
C
C       IF THERE ARE RDTOR OR CVTOR PIKUPS ON THIS SURFACE THEY MUST GO
C       ALSO TORIC CONIC AND ASPHERIC PIKUPS AND ASPHERIC TORIC
C       DEFINITIONS.
C
C
                  IF(PIKUP(1,SF,9).EQ.1.0D0) THEN
C       THERE ARE PIKUPS TO REMOVE
                      PIKUP(1:6,SF,9)=0.0D0
                      WRITE(OUTLYNE,*)'SURFACE',SF,' :PIKUP (CVTOR) DELETED'
                      CALL SHOWIT(1)
                      ALENS(32,SF)=ALENS(32,SF)-1.0D0
                  END IF
                  IF(PIKUP(1,SF,10).EQ.1.0D0) THEN
                      PIKUP(1:6,SF,10)=0.0D0
                      WRITE(OUTLYNE,*)'SURFACE',SF,' :PIKUP (RDTOR) DELETED'
                      CALL SHOWIT(1)
                      ALENS(32,SF)=ALENS(32,SF)-1.0D0
                  END IF
                  IF(PIKUP(1,SF,21).EQ.1.0D0) THEN
                      PIKUP(1:6,SF,21)=0.0D0
                      WRITE(OUTLYNE,*)'SURFACE',SF,' :PIKUP (CCTOR) DELETED'
                      CALL SHOWIT(1)
                      ALENS(32,SF)=ALENS(32,SF)-1.0D0
                  END IF
                  IF(PIKUP(1,SF,22).EQ.1.0D0) THEN
                      PIKUP(1:6,SF,22)=0.0D0
                      WRITE(OUTLYNE,*)'SURFACE',SF,' :PIKUP (ADTOR) DELETED'
                      CALL SHOWIT(1)
                      ALENS(32,SF)=ALENS(32,SF)-1.0D0
                  END IF
                  IF(PIKUP(1,SF,23).EQ.1.0D0) THEN
                      PIKUP(1:6,SF,23)=0.0D0
                      WRITE(OUTLYNE,*)'SURFACE',SF,' :PIKUP (AETOR) DELETED'
                      CALL SHOWIT(1)
                      ALENS(32,SF)=ALENS(32,SF)-1.0D0
                  END IF
                  IF(PIKUP(1,SF,24).EQ.1.0D0) THEN
                      PIKUP(1:6,SF,24)=0.0D0
                      WRITE(OUTLYNE,*)'SURFACE',SF,' :PIKUP (AFTOR) DELETED'
                      CALL SHOWIT(1)
                      ALENS(32,SF)=ALENS(32,SF)-1.0D0
                  END IF
                  IF(PIKUP(1,SF,25).EQ.1.0D0) THEN
                      PIKUP(1:6,SF,25)=0.0D0
                      WRITE(OUTLYNE,*)'SURFACE',SF,' :PIKUP (AGTOR) DELETED'
                      CALL SHOWIT(1)
                      ALENS(32,SF)=ALENS(32,SF)-1.0D0
                  END IF
                  IF(PIKUP(1,SF,26).EQ.1.0D0) THEN
                      PIKUP(1:6,SF,26)=0.0D0
                      WRITE(OUTLYNE,*)'SURFACE',SF,' :PIKUP (AC) DELETED'
                      CALL SHOWIT(1)
                      ALENS(32,SF)=ALENS(32,SF)-1.0D0
                  END IF
C       DUMP PIKUP PRO AND NPRO IF FOUND
                  IF(PIKUP(1,SF,11).GT.0.0D0) THEN
                      PIKUP(1:6,SF,11)=0.0D0
                      ALENS(32,SF)=ALENS(32,SF)-1.0D0
                      WRITE(OUTLYNE,*)'SURFACE',SF,' :PIKUP (PRO) DELETED'
                      CALL SHOWIT(1)
                  END IF
                  IF(PIKUP(1,SF,12).GT.0.0D0) THEN
                      PIKUP(1:6,SF,12)=0.0D0
                      ALENS(32,SF)=ALENS(32,SF)-1.0D0
                      WRITE(OUTLYNE,*)'SURFACE',SF,' :PIKUP (NPRO) DELETED'
                      CALL SHOWIT(1)
                  END IF
C
C
C****************************************************************
C       IF ANY SURFACE IN THE LENS IS PIKING UP TORIC DATA FROM
C       THIS SURFACE, THOSE PIKUPS MUST ALSO GO. ALL PIKED
C       UP DATA IS FROZEN AT ITS CURRENT VALUES.
C
                  DO 31 I=0,INT(SYSTEM1(20))

                      IF(PIKUP(2,I,9).EQ.DBLE(SF).AND.PIKUP(1,I,9).NE.0.0D0
     1                .OR.PIKUP(2,I,10).EQ.DBLE(SF).AND.PIKUP(1,I,10).NE.0.0D0
     2                .OR.PIKUP(2,I,21).EQ.DBLE(SF).AND.PIKUP(1,I,21).NE.0.0D0
     3                .OR.PIKUP(2,I,22).EQ.DBLE(SF).AND.PIKUP(1,I,22).NE.0.0D0
     4                .OR.PIKUP(2,I,23).EQ.DBLE(SF).AND.PIKUP(1,I,23).NE.0.0D0
     5                .OR.PIKUP(2,I,24).EQ.DBLE(SF).AND.PIKUP(1,I,24).NE.0.0D0
     6                .OR.PIKUP(2,I,25).EQ.DBLE(SF).AND.PIKUP(1,I,25).NE.0.0D0
     6                .OR.PIKUP(2,I,27).EQ.DBLE(SF).AND.PIKUP(1,I,27).NE.0.0D0)
     7                 THEN

C
C       SURFACE I IS PIKING UP TORIC DATA FROM SURFACE SF
C       DELETE ALL TORIC PIKUPS FROM SURFACE I
C
                          IF(PIKUP(1,I,9).EQ.1.0D0) THEN
                              PIKUP(1:6,I,9)=0.0D0
                              ALENS(32,I)=ALENS(32,I)-1.0D0
                          ELSE
                          END IF
                          IF(PIKUP(1,I,10).EQ.1.0D0) THEN
                              PIKUP(1:6,I,10)=0.0D0
                              ALENS(32,I)=ALENS(32,I)-1.0D0
                          ELSE
                          END IF
                          IF(PIKUP(1,I,21).EQ.1.0D0) THEN
                              PIKUP(1:6,I,21)=0.0D0
                              ALENS(32,I)=ALENS(32,I)-1.0D0
                          ELSE
                          END IF
                          IF(PIKUP(1,I,22).EQ.1.0D0) THEN
                              PIKUP(1:6,I,22)=0.0D0
                              ALENS(32,I)=ALENS(32,I)-1.0D0
                          ELSE
                          END IF
                          IF(PIKUP(1,I,23).EQ.1.0D0) THEN
                              PIKUP(1:6,I,23)=0.0D0
                              ALENS(32,I)=ALENS(32,I)-1.0D0
                          ELSE
                          END IF
                          IF(PIKUP(1,I,24).EQ.1.0D0) THEN
                              PIKUP(1:6,I,24)=0.0D0
                              ALENS(32,I)=ALENS(32,I)-1.0D0
                          ELSE
                          END IF
                          IF(PIKUP(1,I,25).EQ.1.0D0) THEN
                              PIKUP(1:6,I,25)=0.0D0
                              ALENS(32,I)=ALENS(32,I)-1.0D0
                          ELSE
                          END IF
                          IF(PIKUP(1,I,27).EQ.1.0D0) THEN
                              PIKUP(1:6,I,27)=0.0D0
                              ALENS(32,I)=ALENS(32,I)-1.0D0
                          ELSE
                          END IF
                          WRITE(OUTLYNE,*)'SURFACE',I,' :ALL TORIC PIKUPS DELETED'
                          CALL SHOWIT(1)
                          WRITE(OUTLYNE,*)'WHICH REFERENCED SURFACE',SF
                          CALL SHOWIT(1)
                      END IF
C
 31               CONTINUE
C       IF ANY SURFACE IN THE LENS IS PIKING UP PRO/NPRO DATA FROM
C       THIS SURFACE, THOSE PIKUPS MUST ALSO GO. ALL PIKED
C       UP DATA IS FROZEN AT ITS CURRENT VALUES.
C
                  DO 32 I=0,INT(SYSTEM1(20))

                      IF(PIKUP(2,I,11).EQ.DBLE(SF).AND.PIKUP(1,I,11).NE.0.0D0
     1                .OR.PIKUP(2,I,12).EQ.DBLE(SF).AND.PIKUP(1,I,12).NE.0.0D0
     2                )THEN

C
C       SURFACE I IS PIKING UP PRO/NPRO DATA FROM SURFACE SF
C       DELETE ALL PRO/NPRO PIKUPS FROM SURFACE I
C
                          IF(PIKUP(1,I,11).EQ.1.0D0) THEN
                              PIKUP(1:6,I,11)=0.0D0
                              ALENS(32,I)=ALENS(32,I)-1.0D0
                              WRITE(OUTLYNE,*)'SURFACE',I,' :PIKUP (PRO) DELETED'
                              CALL SHOWIT(1)
                          END IF
                          IF(PIKUP(1,I,12).EQ.1.0D0) THEN
                              PIKUP(1:6,I,12)=0.0D0
                              ALENS(32,I)=ALENS(32,I)-1.0D0
                              WRITE(OUTLYNE,*)'SURFACE',I,' :PIKUP (NPRO) DELETED'
                              CALL SHOWIT(1)
                          END IF
C
                      ELSE
C       NO TORIC PIKUPS FOUND REFERENCING SURFACE SF,PROCEED
                      END IF
C
 32               CONTINUE
C
C       NOW DUMP THE TORIC DATA COMPLETELY
C
                  ALENS(23:24,SF)=0.0D0
                  ALENS(36:41,SF)=0.0D0
                  WRITE(OUTLYNE,*)'TORIC DELETED FROM SURFACE',SF
                  CALL SHOWIT(1)
                  RETURN
              END IF
          END DO
          RETURN
      END
C SUB STILTD.FOR
      SUBROUTINE STILTD
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE STILTD WHICH IMPLEMENTS THE TILTD COMMAND
C       AT THE LENS UPDATE LEVEL.
C       THIS REMOVES ALL TILTS BUT NOT DECENTERS ON A SURFACE.
C
          INTEGER PIKCNT,I,J,SF
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
C               CHECK FOR ADDITIONAL INPUT AND
C               PRINT ERROR AND RETURN IF DISCOVERED.
C
          IF(SQ.EQ.1.OR.SST.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
              OUTLYNE=
     1        '"TILTD" TAKES NO STRING OR QUALIFIER'
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
     1        '"TILTD" USES EITHER TWO OR ZERO NUMERIC WORDS'
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
C
              IF(SF.EQ.0.AND.W1.EQ.W2) THEN
                  OUTLYNE='OBJECT SURFACE IS NEVER TILTED'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C
C       WE ARE AT THE LENS UPDATE LEVEL
C
C
C       FIRST SET THE TILT STATUS FLAG ALENS(25,SF) TO ZERO
C       AND SET ALENS(26,SF) TO ALENS(28,SF) TO ZERO
C
              ALENS(25:28,SF)=0.0D0
              ALENS(118:120,SF)=0.0D0
              ALENS(90:95,SF)=0.0D0
              ALENS(77,SF)=0.0D0
C       WHAT IF THE SURFACE HAD ALPHA,BETA OR GAMMA PIKUPS ON IT?
C
              DO I=15,17
                  IF(PIKUP(1,SF,I).NE.0.0D0) THEN
                      PIKUP(1:6,SF,I)=0.0D0
C       FIX THE PIKUP COUNTER
                      ALENS(32,SF)=ALENS(32,SF)-1.0D0
                      IF(I.EQ.15) THEN
                          WRITE(OUTLYNE,*)'SURFACE',SF,' :PIKUP (ALPHA) DELETED'
                          CALL SHOWIT(1)
                      END IF
                      IF(I.EQ.16) THEN
                          WRITE(OUTLYNE,*)'SURFACE',SF,' :PIKUP (BETA) DELETED'
                          CALL SHOWIT(1)
                      END IF
                      IF(I.EQ.17) THEN
                          WRITE(OUTLYNE,*)'SURFACE',SF,' :PIKUP (GAMMA) DELETED'
                          CALL SHOWIT(1)
                      END IF
                  END IF
              END DO
C       WHAT IF THE SURFACE HAD ALPHA,BETA OR GAMMA PIKUPS ON IT?
C
              DO I=37,42
                  IF(PIKUP(1,SF,I).NE.0.0D0) THEN
                      PIKUP(1:6,SF,I)=0.0D0
C       FIX THE PIKUP COUNTER
                      ALENS(32,SF)=ALENS(32,SF)-1.0D0
                      IF(I.EQ.37) THEN
                          WRITE(OUTLYNE,*)'SURFACE',SF,' :PIKUP (GDX) DELETED'
                          CALL SHOWIT(1)
                      END IF
                      IF(I.EQ.38) THEN
                          WRITE(OUTLYNE,*)'SURFACE',SF,' :PIKUP (GDY) DELETED'
                          CALL SHOWIT(1)
                      END IF
                      IF(I.EQ.39) THEN
                          WRITE(OUTLYNE,*)'SURFACE',SF,' :PIKUP (GDZ) DELETED'
                          CALL SHOWIT(1)
                      END IF
                      IF(I.EQ.40) THEN
                          WRITE(OUTLYNE,*)'SURFACE',SF,' :PIKUP (GALPHA) DELETED'
                          CALL SHOWIT(1)
                      END IF
                      IF(I.EQ.41) THEN
                          WRITE(OUTLYNE,*)'SURFACE',SF,' :PIKUP (GBETA) DELETED'
                          CALL SHOWIT(1)
                      END IF
                      IF(I.EQ.42) THEN
                          WRITE(OUTLYNE,*)'SURFACE',SF,' :PIKUP (GGAMMA) DELETED'
                          CALL SHOWIT(1)
                      END IF
                  END IF
              END DO
C
              PIKCNT=0
              DO 503 J=1,PSIZ
                  IF(PIKUP(1,SF,J).NE.0.0D0) THEN
                      PIKCNT=PIKCNT+1
                  ELSE
                  END IF
 503          CONTINUE
              IF(PIKCNT.EQ.0) ALENS(32,SF)=0.0D0

C
C       WHAT IF THIS SURFACE WAS THE TARGET OF AN ALPHA,BETE OR GAMMA PIKUP
C       OR DECENTERS
C               PIKUP(I,J,K) WHERE K IS 15,16, OR 17
C       IF SO THEN THE PIKUP MUST BE DELETED AND THE TILT
C       DATA FROZEN ON THE PIKUP SURFACE AT THEIR CURRENT VALUES.
              DO 300 I=0,INT(SYSTEM1(20))
                  DO J=15,17
                      IF(PIKUP(1,I,J).EQ.1.0D0) THEN
C       DOES IT REFER TO SURFACE SF
                          IF(INT(PIKUP(2,I,J)).EQ.SF) THEN
C       YES IT REFERS TO THE SURFACE SF WHICH IS HAVING ITS TILT
C       DELETED SO GET RIDE OF THE PIKUP
                              PIKUP(1:6,I,J)=0.0D0
                              ALENS(32,I)=ALENS(32,I)-1.0D0
                              IF(J.EQ.15) THEN
                                  WRITE(OUTLYNE,*)'(ALPHA) PIKUP DELETED ON SURFACE',I
                                  CALL SHOWIT(1)
                              END IF
                              IF(J.EQ.16) THEN
                                  WRITE(OUTLYNE,*)'(BETA) PIKUP DELETED ON SURFACE',I
                                  CALL SHOWIT(1)
                              END IF
                              IF(J.EQ.17) THEN
                                  WRITE(OUTLYNE,*)'(GAMMA) PIKUP DELETED ON SURFACE',I
                                  CALL SHOWIT(1)
                              END IF
                          END IF
                      END IF
                  END DO
                  DO J=37,42
                      IF(PIKUP(1,I,J).EQ.1.0D0) THEN
C       DOES IT REFER TO SURFACE SF
                          IF(INT(PIKUP(2,I,J)).EQ.SF) THEN
C       YES IT REFERS TO THE SURFACE SF WHICH IS HAVING ITS TILT
C       DELETED SO GET RIDE OF THE PIKUP
                              PIKUP(1:6,I,J)=0.0D0
                              ALENS(32,I)=ALENS(32,I)-1.0D0
                              IF(J.EQ.37) THEN
                                  WRITE(OUTLYNE,*)'(GDX) PIKUP DELETED ON SURFACE',I
                                  CALL SHOWIT(1)
                              END IF
                              IF(J.EQ.38) THEN
                                  WRITE(OUTLYNE,*)'(GDY) PIKUP DELETED ON SURFACE',I
                                  CALL SHOWIT(1)
                              END IF
                              IF(J.EQ.39) THEN
                                  WRITE(OUTLYNE,*)'(GDZ) PIKUP DELETED ON SURFACE',I
                                  CALL SHOWIT(1)
                              END IF
                              IF(J.EQ.40) THEN
                                  WRITE(OUTLYNE,*)'(GALPHA) PIKUP DELETED ON SURFACE',I
                                  CALL SHOWIT(1)
                              END IF
                              IF(J.EQ.41) THEN
                                  WRITE(OUTLYNE,*)'(GBETA) PIKUP DELETED ON SURFACE',I
                                  CALL SHOWIT(1)
                              END IF
                              IF(J.EQ.42) THEN
                                  WRITE(OUTLYNE,*)'(GGAMMA) PIKUP DELETED ON SURFACE',I
                                  CALL SHOWIT(1)
                              END IF
                          END IF
                      END IF
                  END DO
 300          CONTINUE
C
C       NOW FIX ALL THE ALENS(32,K) IN THE LENS SYSTEM
C
              DO 400 I=0,INT(SYSTEM1(20))
C       CHECK PIKUPS
                  PIKCNT=0
                  DO 401 J=1,PSIZ
                      IF(PIKUP(1,I,J).EQ.1.0D0) THEN
                          PIKCNT=PIKCNT+1
                      ELSE
                      END IF
 401              CONTINUE
                  IF(PIKCNT.EQ.0) ALENS(32,I)=0.0D0
 400          CONTINUE
          END DO
          RETURN
      END


C SUB STILTAD.FOR
      SUBROUTINE STILTAD
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE STILT WHICH IMPLEMENTS THE TILT COMMANDS
C       TILT AUTOD
C       COMMAND AT THE LENS INPUT LEVEL OR THE LENS UPDATE LEVEL.
C
          INTEGER PIKCNT,I,J,SF
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
C
          IF(SST.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
              OUTLYNE= '"TILT AUTOD"'
              CALL SHOWIT(1)
              OUTLYNE=
     1        'TAKES NO STRING OR NUMERIC WORD #3 THROUGH #5 INPUT'
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
              OUTLYNE= '"TILT AUTOD"'
              CALL SHOWIT(1)
              OUTLYNE=
     1        'USES EITHER TWO OR ZERO NUMERIC WORDS'
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
C
              IF(SF.EQ.0) THEN
                  OUTLYNE='OBJECT SURFACE MAY NOT BE TILTED OR DECENTERED'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C
C       WE ARE AT LENS INPUT LEVEL OR LENS UPDATE LEVEL
C
C               CHECK FOR VALID QUALIFIERS, PROCEED
C
C       TILT AUTOD CHANGES TILT AUTO AND TILT AUTOM INTO
C       ORDINARY TILTS.
C                       1.0= TILT AUTOD IF IT WAS 2.0
C                       1.0= TILT AUTOD IF IT WAS 3.0
C
              IF(ALENS(25,SF).NE.2.0D0.AND.ALENS(25,SF)
     1        .NE.3.0D0) THEN
                  WRITE(OUTLYNE,*)'SURFACE',SF,
     1            ' :NOT DEFINED AS TILT AUTO OR TILT AUTOM'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(ALENS(25,SF).EQ.2.0D0) THEN
                  ALENS(25,SF)=1.0D0
              ELSE
              END IF
              IF(ALENS(25,SF).EQ.3.0D0) THEN
                  ALENS(25,SF)=1.0D0
              ELSE
              END IF
C       RESOLVE PIKUPS ALPHA,BETA AND GAMMA IF THE EXIST
C       USING THE TILT COMMAND HAS THE SAME EFFECT AS IF
C       A TILTD COMMAND WERE USED FIRST. HANDLE PIKUPS
C       THE SAME WAY THEY ARE HANDLED IN (STILTD)
C       WHAT IF THE SURFACE HAD ALPHA,BETA OR GAMMA PIKUPS ON IT?
C
              DO 500 I=15,17
                  IF(PIKUP(1,SF,I).NE.0.0D0) THEN
                      PIKUP(1:6,SF,I)=0.0D0
C       FIX THE PIKUP COUNTER
                      ALENS(32,SF)=ALENS(32,SF)-1.0D0
                      IF(I.EQ.15) THEN
                          WRITE(OUTLYNE,*)'SURFACE',SF,' :PIKUP (ALPHA) DELETED'
                          CALL SHOWIT(1)
                      END IF
                      IF(I.EQ.16) THEN
                          WRITE(OUTLYNE,*)'SURFACE',SF,' :PIKUP (BETA) DELETED'
                          CALL SHOWIT(1)
                      END IF
                      IF(I.EQ.17) THEN
                          WRITE(OUTLYNE,*)'SURFACE',SF,' :PIKUP (GAMMA) DELETED'
                          CALL SHOWIT(1)
                      END IF
                  END IF
 500          CONTINUE
C
              PIKCNT=0
              DO 503 J=1,PSIZ
                  IF(PIKUP(1,SF,J).NE.0.0D0) THEN
                      PIKCNT=PIKCNT+1
                  ELSE
                  END IF
 503          CONTINUE
              IF(PIKCNT.EQ.0) ALENS(32,SF)=0.0D0

C
C       WHAT IF THIS SURFACE WAS THE TARGET OF AN ALPHA,BETA OR GAMMA PIKUP
C               PIKUP(I,J,K) WHERE K IS 15,16, OR 17
C       IF SO THEN THE PIKUP MUST BE DELETED AND THE TILT
C       DATA FROZEN ON THE PIKUP SURFACE AT THEIR CURRENT VALUES.
              DO 300 I=0,INT(SYSTEM1(20))
                  DO 301 J=15,17
                      IF(PIKUP(1,I,J).EQ.1.0D0) THEN
C       DOES IT REFER TO SURFACE SF
                          IF(INT(PIKUP(2,I,J)).EQ.SF) THEN
C       YES IT REFERS TO THE SURFACE SF WHICH IS HAVING ITS TILT
C       DELETED SO GET RIDE OF THE PIKUP
                              PIKUP(1:6,I,J)=0.0D0
                              ALENS(32,I)=ALENS(32,I)-1.0D0
                              IF(J.EQ.15) THEN
                                  WRITE(OUTLYNE,*)'(ALPHA) PIKUP DELETED ON SURFACE',I
                                  CALL SHOWIT(1)
                              END IF
                              IF(J.EQ.16) THEN
                                  WRITE(OUTLYNE,*)'(BETA) PIKUP DELETED ON SURFACE',I
                                  CALL SHOWIT(1)
                              END IF
                              IF(J.EQ.17) THEN
                                  WRITE(OUTLYNE,*)'(GAMMA) PIKUP DELETED ON SURFACE',I
                                  CALL SHOWIT(1)
                              END IF
                          END IF
                      END IF
 301              CONTINUE
 300          CONTINUE
C
C       NOW FIX ALL THE ALENS(32,K) IN THE LENS SYSTEM
C
              DO 400 I=0,INT(SYSTEM1(20))
C       CHECK PIKUPS
                  PIKCNT=0
                  DO 401 J=1,PSIZ
                      IF(PIKUP(1,I,J).EQ.1.0D0) THEN
                          PIKCNT=PIKCNT+1
                      ELSE
                      END IF
 401              CONTINUE
                  IF(PIKCNT.EQ.0) ALENS(32,I)=0.0D0
 400          CONTINUE
              RETURN
          END DO
      END
C SUB STH.FOR
      SUBROUTINE STH
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE STH WHICH IMPLEMENTS THE TH
C       COMMAND AT THE LENS INPUT AND UPDATE LENS LEVEL.
C       IF A THICKNESS
C       SOLVE EXISTS ON THE SURFACE, A THICKNESS
C       ASSIGNMENT WILL DELETE THE SOLVE.
C
          INTEGER PIKCNT,I
C
          REAL*8 X00,Y00,OLDX1,OLDY1,OLDTH,TH,SLOPE
     1    ,NEWY1,NEWX1,Y0ANG,X0ANG
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
C
C               CHECK FOR PRESENCE OF QUALIFIER OR STRING INPUT
C               PRINT ERROR AND RETURN IF DISCOVERED.
C
          IF(SST.EQ.1) THEN
              OUTLYNE='"TH" TAKES NO STRING INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1
     1    .OR.S5.EQ.1) THEN
              OUTLYNE='"TH" TAKES NO NUMERIC WORD #2 THROUGH #5 INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SQ.EQ.1.AND.F5.EQ.1) THEN
              OUTLYNE='"TH" TAKES NO QUALIFIER WORD IN LENS INPUT MODE'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(F6.EQ.1) THEN
              IF(WQ.NE.'        '.AND.WQ.NE.'DELT'.AND.WQ.NE.'CENT') THEN
                  OUTLYNE='INVALID QUALIFIER WORD USED WITH "TH"'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
          IF(DF1.EQ.1) THEN
              OUTLYNE='"TH" REQUIRES EXPLICIT NUMERIC WORD #1 INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(F6.EQ.1) THEN
C       IF(ALENS(90,SURF).NE.0.0D0.OR.ALENS(91,SURF).NE.0.0D0.OR.
C    1ALENS(92,SURF).NE.0.0D0.OR.ALENS(93,SURF).NE.0.0D0.OR.
C    2ALENS(94,SURF).NE.0.0D0.OR.ALENS(95,SURF).NE.0.0D0) THEN
C       WRITE(OUTLYNE,*)
C    1'GLOBAL POSITIONING IS IN EFFECT AT THE CURRENT SURFACE'
C       CALL SHOWIT(1)
C       WRITE(OUTLYNE,*)
C    1'THE "TH" COMMAND IS NOT ALLOWED'
C       CALL SHOWIT(1)
C                       CALL MACFAL
C                       RETURN
C                       END IF
C
C       THE CASE OF CHANGING THE THICKNESS OF THE OBJECT SURFACE
C       IF OBJECT SURFACE, REMEMBER OLD VALUE
              IF(SURF.EQ.0.AND.SYSTEM1(26).EQ.-99.0D0) THEN
                  SYSTEM1(55)=ALENS(3,SURF)
                  IF(SQ.EQ.0) ALENS(3,SURF)=W1
                  IF(WQ.EQ.'DELT') ALENS(3,SURF)=ALENS(3,SURF)+W1
                  IF(WQ.EQ.'CENT')
     1            ALENS(3,SURF)=ALENS(3,SURF)+(W1*0.01D0*ALENS(3,SURF))
C       MUST BE NO STOP AS WELL
                  IF(SYSTEM1(51).NE.0.0D0.OR.SYSTEM1(53).NE.0.0D0) THEN
C       RECALCULATE Y1
                      IF(SYSTEM1(18).EQ.0.0D0) THEN
C       CASE OF SCY INPUT Y00
                          Y00=SYSTEM1(14)
                          OLDY1=SYSTEM1(15)
                          OLDTH=SYSTEM1(55)
                          TH=ALENS(3,SURF)
                          SLOPE=(OLDY1-Y00)/OLDTH
                          NEWY1=Y00+(SLOPE*TH)
                          SYSTEM1(15)=NEWY1
                          SYSTEM1(22)=NEWY1
                      ELSE
                      END IF
                      IF(SYSTEM1(18).EQ.1.0D0) THEN
C       CASE OF SCY FANG INPUT Y0ANG
                          Y0ANG=SYSTEM1(21)
                          OLDY1=SYSTEM1(22)
                          OLDTH=SYSTEM1(55)
                          TH=ALENS(3,SURF)
                          Y00=-OLDTH*
     1                    DTAN((PII/180.0D0)*Y0ANG)+OLDY1
                          NEWY1=Y00+(DATAN((PII/180.0D0)*Y0ANG)*TH)
                          SYSTEM1(15)=NEWY1
                          SYSTEM1(22)=NEWY1
                      ELSE
                      END IF
                  ELSE
                  END IF
                  IF(SYSTEM1(52).NE.0.0D0.OR.SYSTEM1(54).NE.0.0D0) THEN
C       RECALCULATE X1
                      IF(SYSTEM1(19).EQ.0.0D0) THEN
C       CASE OF SCX INPUT X00
                          X00=SYSTEM1(16)
                          OLDX1=SYSTEM1(17)
                          OLDTH=SYSTEM1(55)
                          TH=ALENS(3,SURF)
                          SLOPE=(OLDX1-X00)/OLDTH
!        NEWY1=X00+(SLOPE*TH)
                          NEWX1=X00+(SLOPE*TH)            ! Changed by ENDO
                          SYSTEM1(17)=NEWX1
                          SYSTEM1(24)=NEWX1
                      ELSE
                      END IF
                      IF(SYSTEM1(19).EQ.1.0D0) THEN
C       CASE OF SCX FANG INPUT X0ANG
                          X0ANG=SYSTEM1(23)
                          OLDX1=SYSTEM1(24)
                          OLDTH=SYSTEM1(55)
                          TH=ALENS(3,SURF)
                          X00=-OLDTH*
     1                    DTAN((PII/180.0D0)*X0ANG)+OLDX1
                          NEWX1=X00+(DATAN((PII/180.0D0)*X0ANG)*TH)
                          SYSTEM1(17)=NEWX1
                          SYSTEM1(24)=NEWX1
                      ELSE
                      END IF
                  ELSE
                  END IF
              ELSE
C       NOT SURFACE ZERO, PROCEED
              END IF
          ELSE
C       NOT AT UPDATE LENS, NO ACTION TAKEN
          END IF
          IF(SQ.EQ.0) ALENS(3,SURF)=W1
          IF(WQ.EQ.'DELT') ALENS(3,SURF)=ALENS(3,SURF)+W1
          IF(WQ.EQ.'CENT')
     1    ALENS(3,SURF)=ALENS(3,SURF)+(W1*0.01D0*ALENS(3,SURF))
C       CHECK FOR A SOLVE
          IF(SOLVE(6,SURF).NE.0.0D0.OR.SOLVE(4,SURF)
     1    .NE.0.0D0.OR.SOLVE(6,SURF).NE.0.0D0.AND
     1    .SOLVE(4,SURF).NE.0.0D0) THEN
C       THERE IS A THICKNESS SOLVE. REMOVE IT
              SOLVE(6,SURF)=0.0D0
              SOLVE(7,SURF)=0.0D0
              SOLVE(4,SURF)=0.0D0
              SOLVE(3,SURF)=0.0D0
              WRITE(OUTLYNE,*)'SURFACE',SURF,' :THICKNESS SOLVE DELETED'
              CALL SHOWIT(1)
              ALENS(33,SURF)=0.0D0
              IF(SOLVE(6,SURF).GT.0.0D0)ALENS(33,SURF)=ALENS(33,SURF)+1.0D0
              IF(SOLVE(4,SURF).GT.0.0D0)ALENS(33,SURF)=ALENS(33,SURF)+0.1D0
              IF(SOLVE(8,SURF).GT.0.0D0)ALENS(33,SURF)=ALENS(33,SURF)+2.0D0
              IF(SOLVE(2,SURF).GT.0.0D0)ALENS(33,SURF)=ALENS(33,SURF)+0.2D0
          ELSE
C       NO SOLVE TO DELETE
          END IF
C
C       CHECK FOR A THICKNESS PIKUP AND IF FOUND,DELETE IT
C       THE PIKUP INDICATOR IS ALENS(32,SURF)
          IF(ALENS(32,SURF).EQ.0.0D0) THEN
C       NO PIKUPS OF ANY KIND, DONT DO ANYTHING,JUST RETURN
              RETURN
          ELSE
          END IF
C
C               THERE ARE PIKUPS, ARE THERE ANY THICKNESS PIKUPS
C
          IF(PIKUP(1,SURF,3).EQ.0.0D0.AND.PIKUP(1,SURF,32).EQ.0.0D0)THEN
C       NO THICKNESS PIKUP TO DELETE,JUST RETURN
              RETURN
          ELSE
          END IF
C
C       DELETE THICKNESS PIKUP
C
          IF(PIKUP(1,SURF,3).NE.0.0D0) THEN
              PIKUP(1:6,SURF,3)=0.0D0
              ALENS(32,SURF)=ALENS(32,SURF)-1.0D0
          END IF
          IF(PIKUP(1,SURF,32).NE.0.0D0) THEN
              PIKUP(1:6,SURF,32)=0.0D0
              ALENS(32,SURF)=ALENS(32,SURF)-1.0D0
          END IF
C
C       PRINT MESSAGE THAT PIKUP WAS DELETED
          WRITE(OUTLYNE,*)'SURFACE',SURF,' :THICKNESS PIKUP DELETED'
          CALL SHOWIT(1)
C
C       IF THERE ARE NO OTHER PIKUPS THEN SET THE PIKUP
C       INDICATOR TO ZERO, ELSE LEAVE IT AT 1.0D0
C
          PIKCNT=0
          DO 10 I=1,PSIZ
              IF(PIKUP(1,SURF,I).NE.0.0D0) THEN
                  PIKCNT=PIKCNT+1
              ELSE
              END IF
 10       CONTINUE
          IF(PIKCNT.EQ.0) THEN
              ALENS(32,SURF)=0.0D0
          ELSE
C       DON'T DO ANYTHING, JUST RETURN
          END IF

C
          RETURN
      END
C SUB SPRICE.FOR
      SUBROUTINE SPRICE
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE SPRICE WHICH IMPLEMENTS THE PRICE
C       COMMAND AT THE LENS INPUT AND UPDATE LENS LEVEL.
C
!        INTEGER PIKCNT
C
!        REAL*8 X00,Y00,TH,SLOPE
!     1  ,Y0ANG,X0ANG
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
C
C               CHECK FOR PRESENCE OF QUALIFIER OR STRING INPUT
C               PRINT ERROR AND RETURN IF DISCOVERED.
C
          IF(SST.EQ.1) THEN
              OUTLYNE='"PRICE" TAKES NO STRING INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1
     1    .OR.S5.EQ.1) THEN
              OUTLYNE='"PRICE" TAKES NO NUMERIC WORD #2 THROUGH #5 INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SQ.EQ.1.AND.F5.EQ.1) THEN
              OUTLYNE='"PRICE" TAKES NO QUALIFIER WORD IN LENS INPUT MODE'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(F6.EQ.1) THEN
              IF(WQ.NE.'        '.AND.WQ.NE.'DELT'.AND.WQ.NE.'CENT') THEN
                  OUTLYNE='INVALID QUALIFIER WORD USED WITH "PRICE"'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
          IF(DF1.EQ.1) THEN
              OUTLYNE='"PRICE" REQUIRES EXPLICIT NUMERIC WORD #1 INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(F6.EQ.1) THEN
              IF(SQ.EQ.0) ALENS(111,SURF)=W1
              IF(WQ.EQ.'DELT') ALENS(111,SURF)=ALENS(111,SURF)+W1
              IF(WQ.EQ.'CENT')
     1        ALENS(111,SURF)=ALENS(111,SURF)+(W1*0.01D0*ALENS(111,SURF))
          END IF
          IF(F5.EQ.1) THEN
              IF(SQ.EQ.0) ALENS(111,SURF)=W1
          END IF
          RETURN
      END
C SUB STHM.FOR
      SUBROUTINE STHM
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE STHM WHICH IMPLEMENTS THE THM
C       COMMAND AT THE LENS INPUT AND UPDATE LENS LEVEL.
C
          !       INTEGER PIKCNT
C
!        REAL*8 X00,Y00,TH,SLOPE
!     1  ,Y0ANG,X0ANG
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
C
C               CHECK FOR PRESENCE OF QUALIFIER OR STRING INPUT
C               PRINT ERROR AND RETURN IF DISCOVERED.
C
          IF(SST.EQ.1) THEN
              OUTLYNE='"THM" TAKES NO STRING INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1
     1    .OR.S5.EQ.1) THEN
              OUTLYNE='"THM" TAKES NO NUMERIC WORD #2 THROUGH #5 INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SQ.EQ.1.AND.F5.EQ.1) THEN
              OUTLYNE='"THM" TAKES NO QUALIFIER WORD IN LENS INPUT MODE'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(F6.EQ.1) THEN
              IF(WQ.NE.'        '.AND.WQ.NE.'DELT'.AND.WQ.NE.'CENT') THEN
                  OUTLYNE='INVALID QUALIFIER WORD USED WITH "THM"'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
          IF(DF1.EQ.1) THEN
              OUTLYNE='"THM" REQUIRES EXPLICIT NUMERIC WORD #1 INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(F5.EQ.1) THEN
              IF(SQ.EQ.0) ALENS(110,SURF)=W1
          END IF
          IF(F6.EQ.1) THEN
              IF(GLANAM(SURF,1).EQ.'             '.AND.
     1        GLANAM(SURF,2).EQ.'REFL         '.AND.F6.EQ.1) THEN
                  IF(SQ.EQ.0) ALENS(110,SURF)=W1
                  IF(WQ.EQ.'DELT') ALENS(110,SURF)=ALENS(110,SURF)+W1
                  IF(WQ.EQ.'CENT')
     1            ALENS(110,SURF)=ALENS(110,SURF)+(W1*0.01D0*ALENS(110,SURF))
              ELSE
                  OUTLYNE='"THM" REQUIRES A MIRROR SURFACE AT THE UPDATE LENS LEVEL'
                  CALL SHOWIT(1)
                  OUTLYNE='NO ACTION TAKEN'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
          RETURN
      END
C SUB SAUTOFUNC.FOR
      SUBROUTINE SAUTOFUNC
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE SAUTOFUNC WHICH IMPLEMENTS THE AUTOFUNC
C       COMMAND AT THE LENS INPUT AND UPDATE LENS LEVEL.
C
!        INTEGER PIKCNT,I
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
C
C               CHECK FOR PRESENCE OF QUALIFIER OR STRING INPUT
C               PRINT ERROR AND RETURN IF DISCOVERED.
C
          IF(SST.EQ.1.OR.SQ.EQ.1) THEN
              OUTLYNE='"AUTOFUNC" TAKES NO STRING OR QUALIFIER INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1
     1    .OR.S5.EQ.1) THEN
              OUTLYNE='"AUTOFUNC" TAKES NO NUMERIC WORD #2 THROUGH #5 INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(DF1.EQ.1) THEN
              OUTLYNE='"AUTOFUNC" REQUIRES EXPLICIT NUMERIC WORD #1 INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(INT(W1).NE.0.AND.INT(W1).NE.1.AND.INT(W1).NE.2
     1    .AND.INT(W1).NE.3.AND.INT(W1).NE.4.AND.INT(W1).NE.5
     1    .AND.INT(W1).NE.6.AND.INT(W1).NE.7.AND.INT(W1).NE.8
     1    .AND.INT(W1).NE.9.AND.INT(W1).NE.10) THEN
              WRITE(OUTLYNE,*)
     1        '"',WC(1:8),'" REQUIRES EXPLICIT INTEGER NUMERIC WORD #1 INPUT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)
     1        'BETWEEN 0 AND 10'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          SYSTEM1(91)=INT(W1)
          F57=1
          RETURN
      END
C SUB STASPH.FOR
      SUBROUTINE STASPH
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE STASPH WHICH IMPLEMENTS THE TASPH
C       COMMAND AT THE LENS INPUT OR UPDATE LENS LEVEL.
C       THESE ARE THE ASPHERIC COEFS IN THE PLANE PERPENDICULAR
C       TO THOSE SET USING THE "ASPH" COMMAND AND ARE ONLY VALID
C       FOR TORIC SURFACES.
C       THE NUMERIC WORDS W1 W2 W3 AND W4 ARE THE
C       4TH, 6TH, 8TH AND 10TH ORDER ASPHERIC SURFACE PROFILE
C       COEFFICIENTS. THE DEFAULT AT LENS INITIALIZATION IS
C       ALL COEFFICIENTS = 0.0D0. IF THE SURFACE IS SET AS AN ANAMORPHIC
C       ASPHERIC, ALENS(36,SURF) IS SET TO 1.0D0. IF NOT ALENS(36,SURF)
C       IS SET BY DEFAULT TO 0.0D0. THIS IS A
C       LABEL MARKING THE SURFACE AS AN ANAMORPHIC ASPHERIC.
C       ALSO HANDELS ASPH AT CMD LEVEL.
C
          INTEGER I,J
C
          REAL*8 CC,AD,AE,AF,AG
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
          IF (F5.EQ.1.OR.F6.EQ.1) THEN
C
              IF (STI.EQ.1) THEN
                  IF (ALENS(36,SURF).NE.0.0D0) THEN
                      WRITE(OUTLYNE,106) SURF
                      CALL SHOWIT(0)
                      WRITE(OUTLYNE,101)ALENS(37,SURF)
                      CALL SHOWIT(0)
                      WRITE(OUTLYNE,102)ALENS(38,SURF)
                      CALL SHOWIT(0)
                      WRITE(OUTLYNE,103)ALENS(39,SURF)
                      CALL SHOWIT(0)
                      WRITE(OUTLYNE,104)ALENS(40,SURF)
                      CALL SHOWIT(0)
 106                  FORMAT('"TASPH" VALUES AT SURFACE #',I3,' ARE:')
 101                  FORMAT('"ADTOR = "',G23.15)
 102                  FORMAT('"AETOR = "',G23.15)
 103                  FORMAT('"AFTOR = "',G23.15)
 104                  FORMAT('"AGTOR = "',G23.15)
                  ELSE
C       NOT ANAMORPHIC ASPHERIC
                      WRITE(OUTLYNE,305) SURF
                      CALL SHOWIT(0)
                      WRITE(OUTLYNE,306)
                      CALL SHOWIT(0)
                  END IF
 305              FORMAT('SURFACE #',I3,
     1            ' IS NOT ANAMORPHIC ASPHERIC')
 306              FORMAT(
     1            'NO ANAMORPHIC ASPHERIC DEFORMATION TERMS EXIST')
                  RETURN
              ELSE
C       NOT STI
              END IF
C
              IF(SURF.EQ.0) THEN
                  OUTLYNE='OBJECT SURFACE MAY NOT BE ASPHERIC-TORIC'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C
C               CHECK FOR PRESENCE OF QUALIFIER OR STRING INPUT
C               PRINT ERROR AND RETURN IF DISCOVERED.
C
              IF(SST.EQ.1.OR.SQ.EQ.1.OR.DF5.EQ.0) THEN
                  OUTLYNE=
     1            '"TASPH" ONLY TAKES NUMERIC WORD #1 THROUGH #4 INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF

C               WE ARE AT LENS INPUT OR LENS UPDATE LEVEL
C
C       CHECK THAT THE SURFACE IS A TORIC. IF NOT PRINT MESSAGE
C       AND RETURN, ELSE PROCEED.
C
              IF(ALENS(23,SURF).EQ.0.0D0) THEN
                  WRITE(OUTLYNE,*)
     1            'SURFACE',SURF,' MUST BE DEFINED AS A Y-TORIC OR'
                  CALL SHOWIT(1)
                  OUTLYNE='AN X-TORIC BEFORE "TASPH" MAY BE USED.'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(DF1.EQ.1) W1=0.0D0
              IF(DF2.EQ.1) W2=0.0D0
              IF(DF3.EQ.1) W3=0.0D0
              IF(DF4.EQ.1) W4=0.0D0
              IF(DF5.EQ.1) W5=0.0D0
C
              ALENS(36,SURF)=1.0D0
              IF(DF1.EQ.0) ALENS(37,SURF)=W1
              IF(DF2.EQ.0) ALENS(38,SURF)=W2
              IF(DF3.EQ.0) ALENS(39,SURF)=W3
              IF(DF4.EQ.0) ALENS(40,SURF)=W4
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
C               NOTE:
C
C       ANAMORPHIC CONIC CONSTANT IS STORED IN ALENS(41,SURF)
C
C
C
          ELSE
              IF(F1.EQ.1) THEN
C
C               CHECK FOR ADDITIONAL INPUT AND
C               PRINT ERROR AND RETURN IF DISCOVERED.
C
                  IF(SST.EQ.1) THEN
                      OUTLYNE='AT THE CMD LEVEL, "TASPH" TAKES NO STRING INPUT'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  IF(SQ.EQ.1.AND.S1.EQ.1) THEN
                      OUTLYNE=
     1                'AT THE CMD LEVEL, "TASPH" TAKES EITHER QUALIFIER OR'
                      CALL SHOWIT(1)
                      OUTLYNE='NUMERIC WORD #1 INPUT BUT NOT BOTH'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  IF(S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
                      OUTLYNE='AT THE CMD LEVEL,'
                      CALL SHOWIT(1)
                      OUTLYNE=
     1                '"TASPH" TAKES NO NUMERIC WORD #2 THROUGH #5 INPUT'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
C
C       WHAT IF NO SURFACES EXIST
                  IF(SYSTEM1(20).EQ.0.0D0) THEN
                      OUTLYNE='NO ASHERIC-TORICS EXIST'
                      CALL SHOWIT(1)
                      OUTLYNE='LENS SYSTEM HAS NO SURFACES'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
C       W1 DESIGNATES THE SURFACE FOR WHICH THE ANAMORPHIC CONIC
C       OR ASPHERIC
C       DATA IS TO BE OUTPUT.
C       IF THE QUALIFIER "ALL" IS USED, THEN THE ASPHERIC DATA FOR
C       THE ENTIRE LENS IS PRINTED.
C
C       IF ALENS(41,SURF) NOT EQUAL TO 0.0 THEN THERE IS CONIC DATA
C       IF ALENS(36,SURF) NOT EQUAL TO 0.0 THEN THERE IS ASPHERIC DATA
C
C       PRINT OUT FOR AN INDIVIDUAL SURFACE
C
                  IF(WQ.EQ.'OB'.OR.WQ.EQ.'OBJ') THEN
C       THIS IS THE SAME AS TASPH,0
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
                      IF(DF1.EQ.1) W1=DBLE(INT(SYSTEM1(20)))
                      I=INT(W1)
                      IF(I.GT.(INT(SYSTEM1(20))).OR.I.LT.0) THEN
                          OUTLYNE='SURFACE NUMBER BEYOND LEGAL RANGE'
                          CALL SHOWIT(1)
                          OUTLYNE='RE-ENTER COMMAND'
                          CALL SHOWIT(1)
                          CALL MACFAL
                          RETURN
                      END IF
                      IF(ALENS(36,I).NE.0.0D0) THEN
                          CC=ALENS(41,I)
                          AD=ALENS(37,I)
                          AE=ALENS(38,I)
                          AF=ALENS(39,I)
                          AG=ALENS(40,I)
                          IF(HEADIN) WRITE(OUTLYNE,500)
                          IF(HEADIN) CALL SHOWIT(0)
                          WRITE(OUTLYNE,100)I,CC,AD,AE,AF,AG
                          CALL SHOWIT(0)
                          RETURN
                      ELSE
C       ALENS(36,I)=0.0D0
                      END IF
                      IF(ALENS(41,I).NE.0.0D0) THEN
                          CC=ALENS(41,I)
                          IF(HEADIN) WRITE(OUTLYNE,500)
                          IF(HEADIN) CALL SHOWIT(0)
                          WRITE(OUTLYNE,200)I,CC
                          CALL SHOWIT(0)
                          RETURN
                      ELSE
                      END IF
C       NO CONIC OR ASPHERIC DATA FOR THAT SURFACE,RETURN
                      WRITE(OUTLYNE,300) I
                      CALL SHOWIT(0)
                      RETURN
                  ELSE
C       THERE WAS A QUALIFIER.
                      IF(WQ.NE.'ALL') THEN
                          OUTLYNE='INVALID QUALIFIER WORD'
                          CALL SHOWIT(1)
                          OUTLYNE='RE-ENTER COMMAND'
                          CALL SHOWIT(1)
                          RETURN
                      END IF
C
C       CHECK FOR NO DATA
C
                      J=0
                      DO 20 I=0,INT(SYSTEM1(20))
                          IF(ALENS(41,I).NE.0.0D0.AND.ALENS(36,I).NE.0.0D0.OR.
     1                    ALENS(41,I).NE.0.0D0.OR.ALENS(36,I).NE.0.0D0) THEN
                              J=J+1
                          ELSE
                          END IF
 20                   CONTINUE
                      IF(J.EQ.0) THEN
C       WRITE "NO DATA" AND RETURN
                          WRITE(OUTLYNE,310)
                          CALL SHOWIT(0)
                          RETURN
                      ELSE
                      END IF
C
C       THERE WAS DATA, WRITE IT
C
C       PRINT HEADER MESSAGE
                      WRITE(OUTLYNE,400)
                      CALL SHOWIT(0)
                      WRITE(OUTLYNE,401)
                      CALL SHOWIT(0)
                      WRITE(OUTLYNE,500)
                      CALL SHOWIT(0)
C
                      DO 10 I=0,INT(SYSTEM1(20))
                          IF(ALENS(41,I).NE.0.0D0.AND.ALENS(36,I).EQ.0.0D0) THEN
                              CC=ALENS(41,I)
                              WRITE(OUTLYNE,200)I,CC
                              CALL SHOWIT(0)
                          ELSE
                              IF(ALENS(36,I).NE.0.0D0) THEN
                                  CC=ALENS(41,I)
                                  AD=ALENS(37,I)
                                  AE=ALENS(38,I)
                                  AF=ALENS(39,I)
                                  AG=ALENS(40,I)
                                  WRITE(OUTLYNE,100)I,CC,AD,AE,AF,AG
                                  CALL SHOWIT(0)
                              ELSE
                              END IF
                          END IF
 10                   CONTINUE
                      RETURN
                  END IF
              ELSE
              END IF
          END IF
 100      FORMAT(I3,1X,G13.6,1X,G13.6,1X,G13.6,1X,G13.6,
     1    1X,G13.6)
 200      FORMAT(I3,1X,G13.6)
 300      FORMAT('SURF',1X,I3,1X,
     1    ' :NO CONIC OR ASPHERIC ANAMORPHIC DATA')
 310      FORMAT('NO CONIC OR ASPHERIC ANAMORPHIC DATA')
 400      FORMAT('ANAMORPHIC CONIC AND ASPHERIC DATA')
 401      FORMAT(1X)
 500      FORMAT('SURF',3X,'CCTOR',9X,'ADTOR',9X,
     1    'AETOR',9X,
     1    'AFTOR',9X,'AGTOR')
          RETURN
      END
C SUB STILT.FOR
      SUBROUTINE STILT
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE STILT WHICH IMPLEMENTS THE TILT COMMANDS
C       TILT,RTILT,TILT AUTO,TILT AUTOM,TILT BEN, TILT DAR AND TILT RET
C       AND TILT RETD,BEND AND DARD AND REVD
C       COMMAND AT THE LENS INPUT LEVEL OR THE LENS UPDATE LEVEL.
C
          REAL*8 RAL,RBE,RGAM,CGAM,SGAM
C
          INTEGER PIKCNT,I,J,NEXTSURF
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
C
C               CHECK FOR ADDITIONAL INPUT AND
C               PRINT ERROR AND RETURN IF DISCOVERED.
C
          IF(SST.EQ.1) THEN
              OUTLYNE=
     1        '"TILT AND RTILT COMMANDS" TAKE NO STRING INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SURF.EQ.0) THEN
              OUTLYNE='OBJECT SURFACE MAY NOT BE TILTED OR DECENTERED'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(GLANAM(SURF,2).EQ.'PERFECT'.OR.GLANAM(SURF,2).EQ.'IDEAL') THEN
              OUTLYNE='"PERFECT" AND "IDEAL" SURFACES MAY NOT BE TILTED'//
     1          ' OR DECENTERED'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(GLANAM(SURF-1,2).EQ.'PERFECT'.OR.GLANAM(SURF-1,2).EQ.'IDEAL')
     1    THEN
              OUTLYNE='THE LAST SURFACE MAY NOT BE TILTED OR DECENTERED'
              CALL SHOWIT(1)
              OUTLYNE='IF THE PREVIOUS SURFACE WAS "PERFECT" OR "IDEAL"'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
C       WE ARE AT LENS INPUT LEVEL OR LENS UPDATE LEVEL
C
C               CHECK FOR VALID QUALIFIERS, PROCEED
C
          IF(WQ.NE.'AUTO'.AND.WQ.NE.'AUTOM'.AND.WQ.NE.'BEN'.AND.WQ.NE.
     1    'DAR'.AND.WQ.NE.'RET'.AND.WQ.NE.'RETD'.AND.WQ.NE.'BEND'.AND.
     1    WQ.NE.'DARD'.AND.WQ.NE.'REV'.AND.WQ.NE.'REVD'
     1    .AND.SQ.NE.0) THEN
              OUTLYNE='INVALID QUALIFIER INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(WQ.EQ.'BEN'.OR.WQ.EQ.
     1    'DAR'.OR.WQ.EQ.'RET'.OR.WQ.EQ.'REV') THEN
              IF(SURF.EQ.0.OR.SURF.EQ.1) THEN
                  OUTLYNE='"TILT "'//WQ(1:3)//' IS DISALLOWED ON'
                  CALL SHOWIT(1)
                  OUTLYNE='SURFACES 0 AND 1'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
C
C       FIRST SET THE TILT STATUS FLAG ALENS(25,SURF)
C
C                       0.0=NO TILT (DEFAULT VALUE)
C                       1.0= TILT
C                      -1.0= RTILT
C                       2.0= TILT AUTO
C                       3.0= TILT AUTOM
C                       4.0= TILT BEN
C                       5.0= TILT DAR
C                       6.0= TILT RET
C
C       FIRST A STANDARD TILT COMMAND
C
          IF(WQ.EQ.'REV'.AND.SURF.EQ.NEWOBJ+1) THEN
              OUTLYNE='"TILT REV" IS NOT ALLOWED ON THE SURFACE IMMEDIATELY'
              CALL SHOWIT(1)
              OUTLYNE='FOLLOWING THE OBJECT SURFACE'
              CALL SHOWIT(1)
              OUTLYNE='NO ACTION TAKEN'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(WC.EQ.'TILT'.AND.SQ.EQ.0) THEN
              ALENS(25,SURF)=0.0D0
              ALENS(90:95,SURF)=0.0D0
              ALENS(77,SURF)=0.0D0
              IF(DF1.EQ.1)W1=ALENS(26,SURF)
              IF(DF2.EQ.1)W2=ALENS(27,SURF)
              IF(DF3.EQ.1)W3=ALENS(28,SURF)
              ALENS(25,SURF)=1.0D0
              ALENS(26,SURF)=W1
              ALENS(27,SURF)=W2
              ALENS(28,SURF)=W3
              ALENS(118,SURF)=W1
              ALENS(119,SURF)=W2
              ALENS(120,SURF)=W3
          ELSE
          END IF
C
C       THEN AN RTILT
C
          IF(WC.EQ.'RTILT'.AND.SQ.EQ.0) THEN
              ALENS(25,SURF)=0.0D0
              ALENS(90:95,SURF)=0.0D0
              ALENS(77,SURF)=0.0D0
              IF(DF1.EQ.1)W1=ALENS(26,SURF)
              IF(DF2.EQ.1)W2=ALENS(27,SURF)
              IF(DF3.EQ.1)W3=ALENS(28,SURF)
              ALENS(25,SURF)=-1.0D0
              ALENS(26,SURF)=W1
              ALENS(27,SURF)=W2
              ALENS(28,SURF)=W3
              ALENS(118,SURF)=W1
              ALENS(119,SURF)=W2
              ALENS(120,SURF)=W3
          ELSE
          END IF
C       CHECK FOR RTILT WITH QUALIFIER
          IF(WC.EQ.'RTILT'.AND.SQ.EQ.1) THEN
              OUTLYNE=
     1        '"RTILT" TAKES NO QUALIFIER WORD INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
C       THEN A TILT AUTO
C
          IF(WC.EQ.'TILT'.AND.WQ.EQ.'AUTO') THEN
              IF(SURF.LE.INT(SYSTEM1(25)))THEN
                  OUTLYNE=
     1            '"TILT AUTO" NOT ALLOWED BEFORE OR ON THE REFERENCE SURFACE'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(ALENS(34,SURF).EQ.24.0D0)THEN
                  OUTLYNE=
     1            '"TILT AUTO" NOT ALLOWED ON A LENS ARRAY SURFACE'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(DF1.EQ.1)W1=ALENS(26,SURF)
              IF(DF2.EQ.1)W2=ALENS(27,SURF)
              IF(DF3.EQ.1)W3=ALENS(28,SURF)
              ALENS(25,SURF)=0.0D0
              ALENS(90:95,SURF)=0.0D0
              ALENS(77,SURF)=0.0D0
              ALENS(25,SURF)=2.0D0
              ALENS(26,SURF)=W1
              ALENS(27,SURF)=W2
              ALENS(28,SURF)=W3
              ALENS(118,SURF)=W1
              ALENS(119,SURF)=W2
              ALENS(120,SURF)=W3
          ELSE
          END IF
          IF(WC.EQ.'TILT'.AND.WQ.EQ.'BEN') THEN
              IF(SN.EQ.1) THEN
                  IF(DF1.EQ.1)W1=ALENS(26,SURF)
                  IF(DF2.EQ.1)W2=ALENS(27,SURF)
                  ALENS(28,SURF)=0.0D0
                  ALENS(120,SURF)=0.0D0
                  ALENS(25,SURF)=0.0D0
                  ALENS(90:95,SURF)=0.0D0
                  ALENS(77,SURF)=0.0D0
                  ALENS(25,SURF)=4.0D0
                  ALENS(26,SURF)=W1
                  ALENS(27,SURF)=W2
                  ALENS(118,SURF)=W1
                  ALENS(119,SURF)=W2
C     USE THE CODE-V DEFAULT GAMMA ONLY IF IT IS NOT EXPLICITLY INPUT
                  RAL=(PII/180.0D0)*W1
                  RBE=(PII/180.0D0)*W2
                  CGAM=(DCOS(RAL)+DCOS(RBE))/(1.0D0+(DCOS(RAL)*DCOS(RBE)))
                  SGAM=-(DSIN(RAL)*DSIN(RBE))/(1.0D0+(DCOS(RAL)*DCOS(RBE)))
                  IF(CGAM.LT.-1.0D0) CGAM=-1.0D0
                  IF(CGAM.GT.+1.0D0) CGAM=+1.0D0
                  IF(SGAM.GE.0.0D0) RGAM=DABS(DACOS(CGAM))
                  IF(SGAM.LT.0.0D0) RGAM=-DABS(DACOS(CGAM))
                  ALENS(28,SURF)=RGAM*180.0D0/PII
                  ALENS(120,SURF)=RGAM*180.0D0/PII
              ELSE
C       NO NUMERICS, JUST CHANGE TYPE
                  ALENS(25,SURF)=4.0D0
              END IF
          ELSE
          END IF
          IF(WC.EQ.'TILT'.AND.WQ.EQ.'DAR') THEN
              IF(SN.EQ.1) THEN
                  IF(DF1.EQ.1)W1=ALENS(26,SURF)
                  IF(DF2.EQ.1)W2=ALENS(27,SURF)
                  IF(DF3.EQ.1)W3=ALENS(28,SURF)
                  ALENS(25,SURF)=0.0D0
                  ALENS(90:95,SURF)=0.0D0
                  ALENS(77,SURF)=0.0D0
                  ALENS(25,SURF)=5.0D0
                  ALENS(26,SURF)=W1
                  ALENS(27,SURF)=W2
                  ALENS(28,SURF)=W3
                  ALENS(118,SURF)=W1
                  ALENS(119,SURF)=W2
                  ALENS(120,SURF)=W3
              ELSE
C       NO NUMERICS, JUST CHANGE TO DAR
                  ALENS(25,SURF)=5.0D0
              END IF
          ELSE
          END IF
          IF(WC.EQ.'TILT'.AND.WQ.EQ.'REV') THEN
              IF(SN.EQ.1) THEN
                  IF(DF1.EQ.1)W1=ALENS(26,SURF)
                  IF(DF2.EQ.1)W2=ALENS(27,SURF)
                  IF(DF3.EQ.1)W3=ALENS(28,SURF)
                  ALENS(25,SURF)=0.0D0
                  ALENS(90:95,SURF)=0.0D0
                  ALENS(77,SURF)=0.0D0
                  ALENS(25,SURF)=7.0D0
                  ALENS(26,SURF)=W1
                  ALENS(27,SURF)=W2
                  ALENS(28,SURF)=W3
                  ALENS(118,SURF)=W1
                  ALENS(119,SURF)=W2
                  ALENS(120,SURF)=W3
              ELSE
C       NO NUMERICS, JUST CHANGE TYPE
                  ALENS(25,SURF)=7.0D0
              END IF
          END IF
          IF(WC.EQ.'TILT'.AND.WQ.EQ.'RETD') THEN
              IF(SN.EQ.1.OR.SST.EQ.1) THEN
                  OUTLYNE='"TILT RETD" TAKES NO STRING OR NUMERIC INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
          IF(WC.EQ.'TILT'.AND.WQ.EQ.'DARD') THEN
              IF(SN.EQ.1.OR.SST.EQ.1) THEN
                  OUTLYNE='"TILT DARD" TAKES NO STRING OR NUMERIC INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
          IF(WC.EQ.'TILT'.AND.WQ.EQ.'REVD') THEN
              IF(SN.EQ.1.OR.SST.EQ.1) THEN
                  OUTLYNE='"TILT REVD" TAKES NO STRING OR NUMERIC INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
          IF(WC.EQ.'TILT'.AND.WQ.EQ.'BEND') THEN
              IF(SN.EQ.1.OR.SST.EQ.1) THEN
                  OUTLYNE='"TILT BEND" TAKES NO STRING OR NUMERIC INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
          IF(WC.EQ.'TILT'.AND.WQ.EQ.'RET') THEN
              IF(DF1.EQ.1) THEN
                  OUTLYNE='"TILT RET" REQUIRES AN EXPLICIT SURFACE #'
                  CALL SHOWIT(1)
                  OUTLYNE='INPUT FOR NUMERIC WORD #1'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C     NOW CHECK IF IT REFERS TO A PREVIOUS SURFACE
              IF(W1.LT.0.0D0.OR.W1.GT.SYSTEM1(20)) THEN
                  OUTLYNE='INVALID SURFACE NUMBER REFERED TO BY "TILT RET"'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(INT(W1).GE.SURF) THEN
                  OUTLYNE='"TILT RET" MUST REFER TO A PREVIOUS SURFACE'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C
              ALENS(70,SURF)=W1
              ALENS(25:28,SURF)=0.0D0
              ALENS(118:120,SURF)=0.0D0
              ALENS(90:95,SURF)=0.0D0
              ALENS(95,SURF)=0.0D0
              ALENS(77,SURF)=0.0D0
              ALENS(114:116,SURF)=0.0D0
              ALENS(29:31,SURF)=0.0D0
              ALENS(69,SURF)=0.0D0
              ALENS(90:95,SURF)=0.0D0
              ALENS(113:116,SURF)=0.0D0
              ALENS(118:120,SURF)=0.0D0
              ALENS(25,SURF)=6.0D0
          ELSE
          END IF
          IF(WC.EQ.'TILT'.AND.WQ.EQ.'RETD') THEN
              IF(ALENS(25,SURF).EQ.6.0D0.OR.ALENS(25,SURF).EQ.1.0D0.AND.
     1        ALENS(77,SURF).EQ.1.0D0) THEN
                  ALENS(25,SURF)=1.0D0
                  ALENS(77,SURF)=0.0D0
              END IF
          END IF
          IF(WC.EQ.'TILT'.AND.WQ.EQ.'BEND'.AND.
     1    ALENS(25,SURF).EQ.4.0D0) THEN
              NEXTSURF=SURF+1
C     CHANGE CURRENT SURFACE
              SAVE_KDP(1)=SAVEINPT(1)
              WC='CHG'
              SQ=0
              S1=1
              S2=0
              S3=0
              S4=0
              S5=0
              DF1=0
              DF2=1
              DF3=1
              DF4=1
              DF5=1
              W1=DBLE(NEXTSURF)-1.0D0
              SST=0
              SN=1
              CALL SCHG
              REST_KDP(1)=RESTINPT(1)
C     INSERT FOLLOWING SURFACE
              SAVE_KDP(1)=SAVEINPT(1)
              WC='INS'
              SQ=0
              S1=1
              S2=0
              S3=0
              S4=0
              S5=0
              DF1=0
              DF2=1
              DF3=1
              DF4=1
              DF5=1
              W1=DBLE(NEXTSURF)
              SST=0
              SN=1
              CALL SINS
              REST_KDP(1)=RESTINPT(1)
C     CHANGE TILT TYPE ON CURRENT SURFACE TO "TILT"
              ALENS(25,NEXTSURF-1)=1.0D0
              ALENS(25,NEXTSURF)=1.0D0
              ALENS(26,NEXTSURF)=ALENS(26,NEXTSURF-1)
              ALENS(27,NEXTSURF)=ALENS(27,NEXTSURF-1)
              ALENS(28,NEXTSURF)=ALENS(28,NEXTSURF-1)
              ALENS(118,NEXTSURF)=ALENS(118,NEXTSURF-1)
              ALENS(119,NEXTSURF)=ALENS(119,NEXTSURF-1)
              ALENS(120,NEXTSURF)=ALENS(120,NEXTSURF-1)
          END IF
C
C       TILT REVD
C
          IF(WC.EQ.'TILT'.AND.WQ.EQ.'REVD'.AND.
     1    ALENS(25,SURF).EQ.7.0D0) THEN
              NEXTSURF=SURF+1
C     CHANGE CURRENT SURFACE
              SAVE_KDP(1)=SAVEINPT(1)
              WC='CHG'
              SQ=0
              S1=1
              S2=0
              S3=0
              S4=0
              S5=0
              DF1=0
              DF2=1
              DF3=1
              DF4=1
              DF5=1
              W1=DBLE(NEXTSURF)-1.0D0
              SST=0
              SN=1
              CALL SCHG
              REST_KDP(1)=RESTINPT(1)
C     INSERT FOLLOWING SURFACE
              SAVE_KDP(1)=SAVEINPT(1)
              WC='INS'
              SQ=0
              S1=1
              S2=0
              S3=0
              S4=0
              S5=0
              DF1=0
              DF2=1
              DF3=1
              DF4=1
              DF5=1
              W1=DBLE(NEXTSURF)
              SST=0
              SN=1
              CALL SINS
              REST_KDP(1)=RESTINPT(1)
C     CHANGE TILT TYPE ON CURRENT SURFACE TO "TILT"
              ALENS(25,NEXTSURF-1)=0.0D0
              ALENS(25,NEXTSURF)=-1.0D0
              ALENS(26,NEXTSURF)=ALENS(26,NEXTSURF-1)
              ALENS(27,NEXTSURF)=ALENS(27,NEXTSURF-1)
              ALENS(28,NEXTSURF)=ALENS(28,NEXTSURF-1)
              ALENS(118,NEXTSURF)=ALENS(118,NEXTSURF-1)
              ALENS(119,NEXTSURF)=ALENS(119,NEXTSURF-1)
              ALENS(120,NEXTSURF)=ALENS(120,NEXTSURF-1)
              ALENS(26,NEXTSURF-1)=0.0D0
              ALENS(27,NEXTSURF-1)=0.0D0
              ALENS(28,NEXTSURF-1)=0.0D0
              ALENS(118,NEXTSURF-1)=0.0D0
              ALENS(119,NEXTSURF-1)=0.0D0
              ALENS(120,NEXTSURF-1)=0.0D0
              IF(ALENS(29,NEXTSURF-1).NE.0.0D0) THEN
                  ALENS(30,NEXTSURF)=ALENS(30,NEXTSURF-1)
                  ALENS(31,NEXTSURF)=ALENS(31,NEXTSURF-1)
                  ALENS(69,NEXTSURF)=ALENS(69,NEXTSURF-1)
                  ALENS(113,NEXTSURF)=ALENS(113,NEXTSURF-1)
                  ALENS(114,NEXTSURF)=ALENS(114,NEXTSURF-1)
                  ALENS(115,NEXTSURF)=ALENS(115,NEXTSURF-1)
                  ALENS(116,NEXTSURF)=ALENS(116,NEXTSURF-1)
                  ALENS(30,NEXTSURF-1)=0.0D0
                  ALENS(31,NEXTSURF-1)=0.0D0
                  ALENS(69,NEXTSURF-1)=0.0D0
                  ALENS(113,NEXTSURF-1)=0.0D0
                  ALENS(114,NEXTSURF-1)=0.0D0
                  ALENS(115,NEXTSURF-1)=0.0D0
                  ALENS(116,NEXTSURF-1)=0.0D0
              END IF
C     THICKNESSES
              IF(ALENS(33,NEXTSURF-1).EQ.1.0D0.OR.
     1        ALENS(33,NEXTSURF-1).EQ.1.1D0.OR.
     2        ALENS(33,NEXTSURF-1).EQ.0.1D0.OR.
     3        ALENS(33,NEXTSURF-1).EQ.0.3D0.OR.
     4        ALENS(33,NEXTSURF-1).EQ.3.0D0.OR.
     5        ALENS(33,NEXTSURF-1).EQ.3.3D0) THEN
C     THICKNESS SOLVE NEEDS MOVING
                  ALENS(33,NEXTSURF)=ALENS(33,NEXTSURF-1)
                  SOLVE(6,NEXTSURF)=SOLVE(6,NEXTSURF-1)
                  SOLVE(4,NEXTSURF)=SOLVE(4,NEXTSURF-1)
                  SOLVE(7,NEXTSURF)=SOLVE(7,NEXTSURF-1)
                  SOLVE(3,NEXTSURF)=SOLVE(3,NEXTSURF-1)
                  IF(ALENS(33,NEXTSURF-1).EQ.0.1D0) ALENS(33,NEXTSURF-1)=0.0D0
                  IF(ALENS(33,NEXTSURF-1).EQ.1.0D0) ALENS(33,NEXTSURF-1)=0.0D0
                  IF(ALENS(33,NEXTSURF-1).EQ.1.1D0) ALENS(33,NEXTSURF-1)=0.0D0
                  IF(ALENS(33,NEXTSURF-1).EQ.0.3D0) ALENS(33,NEXTSURF-1)=0.2D0
                  IF(ALENS(33,NEXTSURF-1).EQ.3.0D0) ALENS(33,NEXTSURF-1)=2.0D0
                  IF(ALENS(33,NEXTSURF-1).EQ.3.3D0) ALENS(33,NEXTSURF-1)=2.2D0
              END IF
              ALENS(3,NEXTSURF)=ALENS(3,NEXTSURF-1)
              ALENS(3,NEXTSURF-1)=0.0D0
              RETURN
          END IF
          IF(WC.EQ.'TILT'.AND.WQ.EQ.'DARD'.AND.
     1    ALENS(25,SURF).EQ.5.0D0) THEN
              NEXTSURF=SURF+1
C     CHANGE CURRENT SURFACE
              SAVE_KDP(1)=SAVEINPT(1)
              WC='CHG'
              SQ=0
              S1=1
              S2=0
              S3=0
              S4=0
              S5=0
              DF1=0
              DF2=1
              DF3=1
              DF4=1
              DF5=1
              W1=DBLE(NEXTSURF)-1.0D0
              SST=0
              SN=1
              CALL SCHG
              REST_KDP(1)=RESTINPT(1)
C     INSERT FOLLOWING SURFACE
              SAVE_KDP(1)=SAVEINPT(1)
              WC='INS'
              SQ=0
              S1=1
              S2=0
              S3=0
              S4=0
              S5=0
              DF1=0
              DF2=1
              DF3=1
              DF4=1
              DF5=1
              W1=DBLE(NEXTSURF)
              SST=0
              SN=1
              CALL SINS
              REST_KDP(1)=RESTINPT(1)
C     CHANGE TILT TYPE ON CURRENT SURFACE TO "TILT"
              ALENS(25,NEXTSURF-1)=1.0D0
              ALENS(25,NEXTSURF)=-1.0D0
              ALENS(26,NEXTSURF)=ALENS(26,NEXTSURF-1)
              ALENS(27,NEXTSURF)=ALENS(27,NEXTSURF-1)
              ALENS(28,NEXTSURF)=ALENS(28,NEXTSURF-1)
              ALENS(118,NEXTSURF)=ALENS(118,NEXTSURF-1)
              ALENS(119,NEXTSURF)=ALENS(119,NEXTSURF-1)
              ALENS(120,NEXTSURF)=ALENS(120,NEXTSURF-1)
              IF(ALENS(29,NEXTSURF-1).NE.0.0D0) THEN
                  ALENS(30,NEXTSURF)=ALENS(30,NEXTSURF-1)
                  ALENS(31,NEXTSURF)=ALENS(31,NEXTSURF-1)
                  ALENS(69,NEXTSURF)=ALENS(69,NEXTSURF-1)
                  ALENS(113,NEXTSURF)=ALENS(113,NEXTSURF-1)
                  ALENS(114,NEXTSURF)=ALENS(114,NEXTSURF-1)
                  ALENS(115,NEXTSURF)=ALENS(115,NEXTSURF-1)
                  ALENS(116,NEXTSURF)=ALENS(116,NEXTSURF-1)
              END IF
C     THICKNESSES
              IF(ALENS(33,NEXTSURF-1).EQ.1.0D0.OR.
     1        ALENS(33,NEXTSURF-1).EQ.1.1D0.OR.
     2        ALENS(33,NEXTSURF-1).EQ.0.1D0.OR.
     3        ALENS(33,NEXTSURF-1).EQ.0.3D0.OR.
     4        ALENS(33,NEXTSURF-1).EQ.3.0D0.OR.
     5        ALENS(33,NEXTSURF-1).EQ.3.3D0) THEN
C     THICKNESS SOLVE NEEDS MOVING
                  ALENS(33,NEXTSURF)=ALENS(33,NEXTSURF-1)
                  SOLVE(6,NEXTSURF)=SOLVE(6,NEXTSURF-1)
                  SOLVE(4,NEXTSURF)=SOLVE(4,NEXTSURF-1)
                  SOLVE(7,NEXTSURF)=SOLVE(7,NEXTSURF-1)
                  SOLVE(3,NEXTSURF)=SOLVE(3,NEXTSURF-1)
                  IF(ALENS(33,NEXTSURF-1).EQ.0.1D0) ALENS(33,NEXTSURF-1)=0.0D0
                  IF(ALENS(33,NEXTSURF-1).EQ.1.0D0) ALENS(33,NEXTSURF-1)=0.0D0
                  IF(ALENS(33,NEXTSURF-1).EQ.1.1D0) ALENS(33,NEXTSURF-1)=0.0D0
                  IF(ALENS(33,NEXTSURF-1).EQ.0.3D0) ALENS(33,NEXTSURF-1)=0.2D0
                  IF(ALENS(33,NEXTSURF-1).EQ.3.0D0) ALENS(33,NEXTSURF-1)=2.0D0
                  IF(ALENS(33,NEXTSURF-1).EQ.3.3D0) ALENS(33,NEXTSURF-1)=2.2D0
              END IF
              ALENS(3,NEXTSURF)=ALENS(3,NEXTSURF-1)
              ALENS(3,NEXTSURF-1)=0.0D0
              RETURN
          END IF
C
C       THEN A TILT AUTOM
C
          IF(WC.EQ.'TILT'.AND.WQ.EQ.'AUTOM') THEN
              IF(SURF.LE.INT(SYSTEM1(25)))THEN
                  OUTLYNE=
     1            '"TILT AUTOM" NOT ALLOWED BEFORE OR ON THE REFERENCE SURFACE'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  RETURN
              END IF
              IF(ALENS(34,SURF).EQ.24.0D0)THEN
                  OUTLYNE=
     1            '"TILT AUTOM" NOT ALLOWED ON A LENS ARRAY SURFACE'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(DF1.EQ.1)W1=ALENS(26,SURF)
              IF(DF2.EQ.1)W2=ALENS(27,SURF)
              IF(DF3.EQ.1)W3=ALENS(28,SURF)
              ALENS(25,SURF)=3.0D0
              ALENS(26,SURF)=W1
              ALENS(27,SURF)=W2
              ALENS(28,SURF)=W3
              ALENS(118,SURF)=W1
              ALENS(119,SURF)=W2
              ALENS(120,SURF)=W3
          ELSE
          END IF
C
C       RESOLVE PIKUPS ALPHA,BETA AND GAMMA IF THE EXIST
C       USING THE TILT COMMAND HAS THE SAME EFFECT AS IF
C       A TILTD COMMAND WERE USED FIRST. HANDLE PIKUPS
C       THE SAME WAY THEY ARE HANDLED IN (STILTD)
C       WHAT IF THE SURFACE HAD ALPHA,BETA OR GAMMA PIKUPS ON IT?
C
          DO I=15,17
              IF(PIKUP(1,SURF,I).NE.0.0D0) THEN
                  PIKUP(1:6,SURF,I)=0.0D0
C       FIX THE PIKUP COUNTER
                  ALENS(32,SURF)=ALENS(32,SURF)-1.0D0
                  IF(I.EQ.15) THEN
                      WRITE(OUTLYNE,*)'SURFACE',SURF,' :PIKUP (ALPHA) DELETED'
                      CALL SHOWIT(1)
                  END IF
                  IF(I.EQ.16) THEN
                      WRITE(OUTLYNE,*)'SURFACE',SURF,' :PIKUP (BETA) DELETED'
                      CALL SHOWIT(1)
                  END IF
                  IF(I.EQ.17) THEN
                      WRITE(OUTLYNE,*)'SURFACE',SURF,' :PIKUP (GAMMA) DELETED'
                      CALL SHOWIT(1)
                  END IF
              END IF
          END DO
          DO I=37,42
              IF(PIKUP(1,SURF,I).NE.0.0D0) THEN
                  PIKUP(1:6,SURF,I)=0.0D0
C       FIX THE PIKUP COUNTER
                  ALENS(32,SURF)=ALENS(32,SURF)-1.0D0
                  IF(I.EQ.37) THEN
                      WRITE(OUTLYNE,*)'SURFACE',SURF,' :PIKUP (GDX) DELETED'
                      CALL SHOWIT(1)
                  END IF
                  IF(I.EQ.38) THEN
                      WRITE(OUTLYNE,*)'SURFACE',SURF,' :PIKUP (GDY) DELETED'
                      CALL SHOWIT(1)
                  END IF
                  IF(I.EQ.39) THEN
                      WRITE(OUTLYNE,*)'SURFACE',SURF,' :PIKUP (GDZ) DELETED'
                      CALL SHOWIT(1)
                  END IF
                  IF(I.EQ.40) THEN
                      WRITE(OUTLYNE,*)'SURFACE',SURF,' :PIKUP (GALPHA) DELETED'
                      CALL SHOWIT(1)
                  END IF
                  IF(I.EQ.41) THEN
                      WRITE(OUTLYNE,*)'SURFACE',SURF,' :PIKUP (GBETA) DELETED'
                      CALL SHOWIT(1)
                  END IF
                  IF(I.EQ.42) THEN
                      WRITE(OUTLYNE,*)'SURFACE',SURF,' :PIKUP (GGAMMA) DELETED'
                      CALL SHOWIT(1)
                  END IF
              END IF
          END DO
C
          PIKCNT=0
          DO 503 J=1,PSIZ
              IF(PIKUP(1,SURF,J).NE.0.0D0) THEN
                  PIKCNT=PIKCNT+1
              ELSE
              END IF
 503      CONTINUE
          IF(PIKCNT.EQ.0) ALENS(32,SURF)=0.0D0

C
C       WHAT IF THIS SURFACE WAS THE TARGET OF AN ALPHA,BETA OR GAMMA PIKUP
C               PIKUP(I,J,K) WHERE K IS 15,16, OR 17
C       IF SO THEN THE PIKUP MUST BE DELETED AND THE TILT
C       DATA FROZEN ON THE PIKUP SURFACE AT THEIR CURRENT VALUES.
          DO I=0,INT(SYSTEM1(20))
              DO J=15,17
                  IF(PIKUP(1,I,J).EQ.1.0D0) THEN
C       DOES IT REFER TO SURFACE SURF
                      IF(INT(PIKUP(2,I,J)).EQ.SURF) THEN
C       YES IT REFERS TO THE SURFACE SURF WHICH IS HAVING ITS TILT
C       DELETED SO GET RIDE OF THE PIKUP
                          PIKUP(1:6,I,J)=0.0D0
                          ALENS(32,I)=ALENS(32,I)-1.0D0
                          IF(J.EQ.15) THEN
                              WRITE(OUTLYNE,*)'(ALPHA) PIKUP DELETED ON SURFACE',I
                              CALL SHOWIT(1)
                          END IF
                          IF(J.EQ.16) THEN
                              WRITE(OUTLYNE,*)'(BETA) PIKUP DELETED ON SURFACE',I
                              CALL SHOWIT(1)
                          END IF
                          IF(J.EQ.17) THEN
                              WRITE(OUTLYNE,*)'(GAMMA) PIKUP DELETED ON SURFACE',I
                              CALL SHOWIT(1)
                          END IF
                      END IF
                  END IF
              END DO
              DO J=37,42
                  IF(PIKUP(1,I,J).EQ.1.0D0) THEN
C       DOES IT REFER TO SURFACE SURF
                      IF(INT(PIKUP(2,I,J)).EQ.SURF) THEN
C       YES IT REFERS TO THE SURFACE SURF WHICH IS HAVING ITS TILT
C       DELETED SO GET RIDE OF THE PIKUP
                          PIKUP(1:6,I,J)=0.0D0
                          ALENS(32,I)=ALENS(32,I)-1.0D0
                          IF(J.EQ.37) THEN
                              WRITE(OUTLYNE,*)'(GDX) PIKUP DELETED ON SURFACE',I
                              CALL SHOWIT(1)
                          END IF
                          IF(J.EQ.38) THEN
                              WRITE(OUTLYNE,*)'(GDY) PIKUP DELETED ON SURFACE',I
                              CALL SHOWIT(1)
                          END IF
                          IF(J.EQ.39) THEN
                              WRITE(OUTLYNE,*)'(GDZ) PIKUP DELETED ON SURFACE',I
                              CALL SHOWIT(1)
                          END IF
                          IF(J.EQ.40) THEN
                              WRITE(OUTLYNE,*)'(GALPHA) PIKUP DELETED ON SURFACE',I
                              CALL SHOWIT(1)
                          END IF
                          IF(J.EQ.41) THEN
                              WRITE(OUTLYNE,*)'(GBETA) PIKUP DELETED ON SURFACE',I
                              CALL SHOWIT(1)
                          END IF
                          IF(J.EQ.42) THEN
                              WRITE(OUTLYNE,*)'(GGAMMA) PIKUP DELETED ON SURFACE',I
                              CALL SHOWIT(1)
                          END IF
                      END IF
                  END IF
              END DO
          END DO
C
C       NOW FIX ALL THE ALENS(32,K) IN THE LENS SYSTEM
C
          DO 400 I=0,INT(SYSTEM1(20))
C       CHECK PIKUPS
              PIKCNT=0
              DO 401 J=1,PSIZ
                  IF(PIKUP(1,I,J).EQ.1.0D0) THEN
                      PIKCNT=PIKCNT+1
                  ELSE
                  END IF
 401          CONTINUE
              IF(PIKCNT.EQ.0) ALENS(32,I)=0.0D0
 400      CONTINUE
          RETURN
      END
