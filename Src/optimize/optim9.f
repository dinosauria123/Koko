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

C       NINTH SET OF OPTIMIZATION ROUTINES

C SUB TLDMP.FOR
      SUBROUTINE TLDMP
C
          IMPLICIT NONE
C
C     THIS OUTPUTS EXISTING TOLERANCE DEFINITIONS WITH LENO
C
          LOGICAL YES
C
          CHARACTER BF1*1,AI1*1,OPNNMM*3,BWORD*8
C
          INTEGER II,I
C
!      LOGICAL EXISJK
C
          INCLUDE 'datsub.inc'
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datsp1.inc'
          INCLUDE 'datspd.inc'
C
C     COMPENSATOR VARIABLES
          YES=.FALSE.
          DO I=1,MAXCMP
              IF(ISTOP(I)) YES=.TRUE.
              IF(ISCOMP(I)) YES=.TRUE.
              IF(ISCRIT(I)) YES=.TRUE.
          END DO
          IF(YES) THEN
C
C
C     DOING TOLERANCE DEFINITIONS
              IF(OPSPDTYPE.EQ.1) WRITE(OUTLYNE,21)
              IF(OPSPDTYPE.EQ.2) WRITE(OUTLYNE,22)
              IF(OPSPDTYPE.EQ.3) WRITE(OUTLYNE,23)
              CALL SHOWIT(10)
 21           FORMAT('OPSPOT RECT')
 22           FORMAT('OPSPOT RING')
 23           FORMAT('OPSPOT RAND')
              IF(OPSPDTYPE.EQ.1) THEN
                  WRITE(OUTLYNE,30) OPNRECT
                  CALL SHOWIT(10)
 30               FORMAT('OPRECT  ,',I8)
              END IF
              IF(OPSPDTYPE.EQ.2) THEN
                  WRITE(OUTLYNE,24) OPRINGTOT
                  CALL SHOWIT(10)
 24               FORMAT('OPRINGS ,',D23.15)
                  DO I=1,INT(OPRINGTOT)
                      WRITE(OUTLYNE,25) I,OPRINGRAD(I),OPRINGPNT(I),OPRINGANG(I)
                      CALL SHOWIT(10)
 25                   FORMAT('OPRING  ,',I3,',',D23.15,',',I3,',',D23.15)
                  END DO
              END IF
              IF(OPSPDTYPE.EQ.3) THEN
                  WRITE(OUTLYNE,41) OPRNUMBR
                  CALL SHOWIT(10)
 41               FORMAT('OPRANNUM,',D23.15)
              END IF
              WRITE(OUTLYNE,52) OPNRD
              CALL SHOWIT(10)
 52           FORMAT('OPNRD   ,',I8)
              WRITE(OUTLYNE,53) TOLNRD
              CALL SHOWIT(10)
 53           FORMAT('TOLNRD  ,',I8)
          END IF
C
          YES=.FALSE.
          DO I=1,MAXCMP
              IF(ISCOMP(I)) YES=.TRUE.
          END DO
          IF(YES) THEN
C     DO COMP VARAIBLES OUTPUT HERE
              WRITE(OUTLYNE,*)'COMPVAR'
              CALL SHOWIT(10)
              DO I=1,MAXCMP
                  IF(ISCOMP(I)) THEN
                      WRITE(OUTLYNE,11) VARNAM(I),DBLE(I),DBLE(INT(VARABL(I,3)))
                      CALL SHOWIT(10)
 11                   FORMAT(A8,',',D23.15,',',D23.15,',,,,')
 12                   FORMAT(A8,',',D23.15,',',D23.15,',',D23.15,',,,')
 13                   FORMAT(A8,',',D23.15,',',D23.15,',',D23.15,',',D23.15,',,')
 14                   FORMAT(A8,' ','PIVOT',',',D23.15,',',D23.15,',',D23.15,',,,')
                  END IF
              END DO
              WRITE(OUTLYNE,*)'EOS'
              CALL SHOWIT(10)
          END IF
C
C     TOLERANCE VARIABLES
C
          IF(TVBCNT.NE.0) THEN
C     HERE IS WHERE DATA IS STORED
C     DO TOL VARAIBLES OUTPUT HERE
              WRITE(OUTLYNE,*)'TVAR'
              CALL SHOWIT(10)
              DO I=1,TVBCNT
                  II=I+MAXCMP
                  IF(VARNAM(II)(1:5).NE.'STILT'.AND.
     1               VARNAM(II)(1:5).NE.'BTILT'.AND.
     1               VARNAM(II)(1:4).NE.'ROLL'.AND.
     1               VARNAM(II)(1:4).NE.'DISP') THEN
                      WRITE(OUTLYNE,11) VARNAM(II),DBLE(INT(VARABL(II,3))),
     1                VARABL(II,8)
                      CALL SHOWIT(10)
                  END IF
                  IF(VARNAM(II)(1:4).EQ.'DISP') THEN
                      WRITE(OUTLYNE,12) VARNAM(II),DBLE(INT(VARABL(II,3))),
     1                DBLE(INT(VARABL(II,7))),
     1                VARABL(II,8)
                      CALL SHOWIT(10)
                  END IF
                  IF(VARNAM(II)(1:4).EQ.'ROLL') THEN
                      WRITE(OUTLYNE,13) VARNAM(II),DBLE(INT(VARABL(II,3))),
     1                DBLE(INT(VARABL(II,7))),
     1                VARABL(II,8),DBLE(INT(VARABL(II,12)))
                      CALL SHOWIT(10)
                  END IF
                  IF(VARNAM(II)(1:5).EQ.'STILT') THEN
C     DO PIVOT
                      IF(VARABL(II,9).NE.0.0D0.OR.VARABL(II,10).NE.0.0D0.OR.
     1                VARABL(II,11).NE.0.0D0)
     2                WRITE(OUTLYNE,14) VARNAM(II),DBLE(INT(VARABL(II,9)))
     3                ,DBLE(INT(VARABL(II,10))),DBLE(INT(VARABL(II,11)))
                      CALL SHOWIT(10)
C     DO STILT
                      WRITE(OUTLYNE,11) VARNAM(II),DBLE(INT(VARABL(II,3))),
     1                VARABL(II,8)
                      CALL SHOWIT(10)
                  END IF
                  IF(VARNAM(II)(1:5).EQ.'BTILT') THEN
C     DO PIVOT
                      IF(VARABL(II,9).NE.0.0D0.OR.VARABL(II,10).NE.0.0D0.OR.
     1                VARABL(II,11).NE.0.0D0)
     2                WRITE(OUTLYNE,14) VARNAM(II),DBLE(INT(VARABL(II,9)))
     3                ,DBLE(INT(VARABL(II,10))),DBLE(INT(VARABL(II,11)))
                      CALL SHOWIT(10)
C     DO BTILT
                      WRITE(OUTLYNE,12) VARNAM(II),DBLE(INT(VARABL(II,3))),
     1                DBLE(INT(VARABL(II,7))),VARABL(II,8)
                      CALL SHOWIT(10)
                  END IF
              END DO
              WRITE(OUTLYNE,*)'EOS'
              CALL SHOWIT(10)
          END IF
C
          YES=.FALSE.
          DO I=1,MAXFOCRIT
              IF(ISCRIT(I)) YES=.TRUE.
          END DO
C     FOCRITS
          IF(YES) THEN
C     HERE IS WHERE DATA IS STORED
C
C     DO FOCRIT OUTPUT HERE
              WRITE(OUTLYNE,*)'FOCRIT'
              CALL SHOWIT(10)
              DO I=1,MAXFOCRIT
C     THE OPERAND AND FUNCTION NAME AND NW1 AND NW2
                  IF(INT(OPERND(I,1)).EQ.0) BWORD='FUNC00'
                  IF(INT(OPERND(I,1)).EQ.1) BWORD='FUNC01'
                  IF(INT(OPERND(I,1)).EQ.2) BWORD='FUNC02'
                  IF(INT(OPERND(I,1)).EQ.3) BWORD='FUNC03'
                  IF(INT(OPERND(I,1)).EQ.4) BWORD='FUNC04'
                  IF(INT(OPERND(I,1)).EQ.5) BWORD='FUNC05'
                  IF(INT(OPERND(I,1)).EQ.6) BWORD='FUNC06'
                  IF(INT(OPERND(I,1)).EQ.7) BWORD='FUNC07'
                  IF(INT(OPERND(I,1)).EQ.8) BWORD='FUNC08'
                  IF(INT(OPERND(I,1)).EQ.9) BWORD='FUNC09'
                  IF(INT(OPERND(I,1)).EQ.10) BWORD='FUNC10'
                  IF(ISCRIT(I)) THEN
C
                      IF(BWORD.EQ.'FUNC00') THEN
                          IF(OPERND(I,18).EQ.0.0D0.AND.OPERND(I,11).EQ.0.0D0
     1                    .AND.OPERND(I,12).EQ.0.0D0) THEN
                              WRITE(OUTLYNE,101) OPNAM(I),DBLE(I),OPERND(I,8),OPERND(I,9),
     1                        OPERND(I,10)
                              CALL SHOWIT(10)
                          END IF
 101                      FORMAT(A8,',',D23.15,',',D23.15,',',D23.15,',',D23.15,',,')
C
                          IF(OPERND(I,18).EQ.1.0D0.AND.OPERND(I,11).EQ.0.0D0
     1                    .AND.OPERND(I,12).EQ.0.0D0) THEN
                              WRITE(OUTLYNE,102) OPNAM(I),DBLE(I),OPERND(I,9),
     1                        OPERND(I,10)
                              CALL SHOWIT(10)
                          END IF
 102                      FORMAT(A8,',',D23.15,',,',D23.15,',',D23.15,',,')
C
                          IF(OPERND(I,18).EQ.1.0D0.AND.OPERND(I,11).EQ.1.0D0
     1                    .AND.OPERND(I,12).EQ.0.0D0) THEN
                              WRITE(OUTLYNE,103) OPNAM(I),DBLE(I),
     1                        OPERND(I,10)
                              CALL SHOWIT(10)
                          END IF
 103                      FORMAT(A8,',',D23.15,',,,',D23.15,',,')
C
                          IF(OPERND(I,18).EQ.1.0D0.AND.OPERND(I,11).EQ.1.0D0
     1                    .AND.OPERND(I,12).EQ.1.0D0) THEN
                              WRITE(OUTLYNE,104) OPNAM(I),DBLE(I)
                              CALL SHOWIT(10)
                          END IF
 104                      FORMAT(A8,',',D23.15,',,,,,')
C
                          IF(OPERND(I,18).EQ.0.0D0.AND.OPERND(I,11).EQ.1.0D0
     1                    .AND.OPERND(I,12).EQ.0.0D0) THEN
                              WRITE(OUTLYNE,105) OPNAM(I),DBLE(I),OPERND(I,8),
     1                        OPERND(I,10)
                              CALL SHOWIT(10)
                          END IF
 105                      FORMAT(A8,',',D23.15,',',D23.15,',,',D23.15,',,')
C
                          IF(OPERND(I,18).EQ.0.0D0.AND.OPERND(I,11).EQ.0.0D0
     1                    .AND.OPERND(I,12).EQ.1.0D0) THEN
                              WRITE(OUTLYNE,106) OPNAM(I),DBLE(I),OPERND(I,8),OPERND(I,9)
                              CALL SHOWIT(10)
                          END IF
 106                      FORMAT(A8,',',D23.15,',',D23.15,',',D23.15,',,,')
C
                          IF(OPERND(I,18).EQ.0.0D0.AND.OPERND(I,11).EQ.1.0D0
     1                    .AND.OPERND(I,12).EQ.1.0D0) THEN
                              WRITE(OUTLYNE,107) OPNAM(I),DBLE(I),OPERND(I,8)
                              CALL SHOWIT(10)
                          END IF
 107                      FORMAT(A8,',',D23.15,',',D23.15,',,,,')
C
                          IF(OPERND(I,18).EQ.1.0D0.AND.OPERND(I,11).EQ.0.0D0
     1                    .AND.OPERND(I,12).EQ.1.0D0) THEN
                              WRITE(OUTLYNE,108) OPNAM(I),DBLE(I),OPERND(I,9)
                              CALL SHOWIT(10)
                          END IF
 108                      FORMAT(A8,',',D23.15,',,',D23.15,',,,')
                      ELSE
                          IF(OPERND(I,18).EQ.0.0D0.AND.OPERND(I,11).EQ.0.0D0
     1                    .AND.OPERND(I,12).EQ.0.0D0) THEN
                              WRITE(OUTLYNE,1101) BWORD,OPNAM(I),DBLE(I),OPERND(I,8),
     1                        OPERND(I,9),
     1                        OPERND(I,10)
                              CALL SHOWIT(10)
                          END IF
 1101                     FORMAT(A8,' ',A8,',',D23.15,',',D23.15,',',D23.15,','
     1                    ,D23.15,',,')
C
                          IF(OPERND(I,18).EQ.1.0D0.AND.OPERND(I,11).EQ.0.0D0
     1                    .AND.OPERND(I,12).EQ.0.0D0) THEN
                              WRITE(OUTLYNE,1102) BWORD,OPNAM(I),DBLE(I),OPERND(I,9),
     1                        OPERND(I,10)
                              CALL SHOWIT(10)
                          END IF
 1102                     FORMAT(A8,' ',A8,',',D23.15,',,',D23.15,',',D23.15,',,')
C
                          IF(OPERND(I,18).EQ.1.0D0.AND.OPERND(I,11).EQ.1.0D0
     1                    .AND.OPERND(I,12).EQ.0.0D0) THEN
                              WRITE(OUTLYNE,1103) BWORD,OPNAM(I),DBLE(I),
     1                        OPERND(I,10)
                              CALL SHOWIT(10)
                          END IF
 1103                     FORMAT(A8,' ',A8,',',D23.15,',,,',D23.15,',,')
C
                          IF(OPERND(I,18).EQ.1.0D0.AND.OPERND(I,11).EQ.1.0D0
     1                    .AND.OPERND(I,12).EQ.1.0D0) THEN
                              WRITE(OUTLYNE,1104) BWORD,OPNAM(I),DBLE(I)
                              CALL SHOWIT(10)
                          END IF
 1104                     FORMAT(A8,' ',A8,',',D23.15,',,,,,')
C
                          IF(OPERND(I,18).EQ.0.0D0.AND.OPERND(I,11).EQ.1.0D0
     1                    .AND.OPERND(I,12).EQ.0.0D0) THEN
                              WRITE(OUTLYNE,1105) BWORD,OPNAM(I),DBLE(I),OPERND(I,8),
     1                        OPERND(I,10)
                              CALL SHOWIT(10)
                          END IF
 1105                     FORMAT(A8,' ',A8,',',D23.15,',',D23.15,',,',D23.15,',,')
C
                          IF(OPERND(I,18).EQ.0.0D0.AND.OPERND(I,11).EQ.0.0D0
     1                    .AND.OPERND(I,12).EQ.1.0D0) THEN
                              WRITE(OUTLYNE,1106) BWORD,OPNAM(I),DBLE(I),OPERND(I,8),
     1                        OPERND(I,9)
                              CALL SHOWIT(10)
                          END IF
 1106                     FORMAT(A8,' ',A8,',',D23.15,',',D23.15,',',D23.15,',,,')
C
                          IF(OPERND(I,18).EQ.0.0D0.AND.OPERND(I,11).EQ.1.0D0
     1                    .AND.OPERND(I,12).EQ.1.0D0) THEN
                              WRITE(OUTLYNE,1107) BWORD,OPNAM(I),DBLE(I),OPERND(I,8)
                              CALL SHOWIT(10)
                          END IF
 1107                     FORMAT(A8,' ',A8,',',D23.15,',',D23.15,',,,,')
C
                          IF(OPERND(I,18).EQ.1.0D0.AND.OPERND(I,11).EQ.0.0D0
     1                    .AND.OPERND(I,12).EQ.1.0D0) THEN
                              WRITE(OUTLYNE,1108) BWORD,OPNAM(I),DBLE(I),OPERND(I,9)
                              CALL SHOWIT(10)
                          END IF
 1108                     FORMAT(A8,' ',A8,',',D23.15,',,',D23.15,',,,')
                      END IF
C
                  END IF
                  IF(OPERDESC(I)(1:8).NE.'        ') THEN
                      WRITE(BF1,100) I
                      READ(BF1,110) AI1
 100                  FORMAT(I1)
 110                  FORMAT(A1)
                      OPNNMM='OP'//AI1
                      WRITE(OUTLYNE,1701) OPNNMM,OPERDESC(I)(1:69)
                      CALL SHOWIT(10)
                  END IF
 1701             FORMAT('OP_DESC',1x,A3,1X,A69)
              END DO
              WRITE(OUTLYNE,*)'EOS'
              CALL SHOWIT(10)
          END IF
C
C     TOPER
          YES=.FALSE.
          DO II=1,MAXTOP
              I=II+MAXFOCRIT
              IF(ISTOP(II)) YES=.TRUE.
          END DO
          IF(YES) THEN
C     HERE IS WHERE DATA IS STORED
              WRITE(OUTLYNE,*)'TOPER'
              CALL SHOWIT(10)
              DO II=1,MAXTOP
                  I=II+MAXFOCRIT
C     THE OPERAND AND FUNCTION NAME AND NW1 AND NW2
                  IF(INT(OPERND(I,1)).EQ.0) BWORD='FUNC00'
                  IF(INT(OPERND(I,1)).EQ.1) BWORD='FUNC01'
                  IF(INT(OPERND(I,1)).EQ.2) BWORD='FUNC02'
                  IF(INT(OPERND(I,1)).EQ.3) BWORD='FUNC03'
                  IF(INT(OPERND(I,1)).EQ.4) BWORD='FUNC04'
                  IF(INT(OPERND(I,1)).EQ.5) BWORD='FUNC05'
                  IF(INT(OPERND(I,1)).EQ.6) BWORD='FUNC06'
                  IF(INT(OPERND(I,1)).EQ.7) BWORD='FUNC07'
                  IF(INT(OPERND(I,1)).EQ.8) BWORD='FUNC08'
                  IF(INT(OPERND(I,1)).EQ.9) BWORD='FUNC09'
                  IF(INT(OPERND(I,1)).EQ.10) BWORD='FUNC10'
                  IF(ISTOP(II)) THEN
C
                      IF(BWORD.EQ.'FUNC00') THEN
                          IF(OPERND(I,18).EQ.0.0D0.AND.OPERND(I,11).EQ.0.0D0
     1                    .AND.OPERND(I,12).EQ.0.0D0) THEN
                              WRITE(OUTLYNE,9101) OPNAM(I),DBLE(II),OPERND(I,8),OPERND(I,9),
     1                        OPERND(I,10),OPERND(I,20)
                              CALL SHOWIT(10)
 9101                         FORMAT(A8,',',D23.15,',',D23.15,',',D23.15,',',D23.15,
     1                        ',',D23.15)
                          END IF
C
                          IF(OPERND(I,18).EQ.1.0D0.AND.OPERND(I,11).EQ.0.0D0
     1                    .AND.OPERND(I,12).EQ.0.0D0) THEN
                              WRITE(OUTLYNE,9102) OPNAM(I),DBLE(II),OPERND(I,9),
     1                        OPERND(I,10),OPERND(I,20)
                              CALL SHOWIT(10)
 9102                         FORMAT(A8,',',D23.15,',,',D23.15,',',D23.15,',',D23.15)
                          END IF
C
                          IF(OPERND(I,18).EQ.1.0D0.AND.OPERND(I,11).EQ.1.0D0
     1                    .AND.OPERND(I,12).EQ.0.0D0) THEN
                              WRITE(OUTLYNE,9103) OPNAM(I),DBLE(II),
     1                        OPERND(I,10),OPERND(I,20)
                              CALL SHOWIT(10)
 9103                         FORMAT(A8,',',D23.15,',,,',D23.15,',',D23.15)
                          END IF
C
                          IF(OPERND(I,18).EQ.1.0D0.AND.OPERND(I,11).EQ.1.0D0
     1                    .AND.OPERND(I,12).EQ.1.0D0) THEN
                              WRITE(OUTLYNE,9104) OPNAM(I),DBLE(II),OPERND(I,20)
                              CALL SHOWIT(10)
 9104                         FORMAT(A8,',',D23.15,',,,,',D23.15)
                          END IF
C
                          IF(OPERND(I,18).EQ.0.0D0.AND.OPERND(I,11).EQ.1.0D0
     1                    .AND.OPERND(I,12).EQ.0.0D0) THEN
                              WRITE(OUTLYNE,9105) OPNAM(I),DBLE(II),OPERND(I,8),
     1                        OPERND(I,10),OPERND(I,20)
                              CALL SHOWIT(10)
 9105                         FORMAT(A8,',',D23.15,',',D23.15,',,',D23.15,',',D23.15)
                          END IF
C
                          IF(OPERND(I,18).EQ.0.0D0.AND.OPERND(I,11).EQ.0.0D0
     1                    .AND.OPERND(I,12).EQ.1.0D0) THEN
                              WRITE(OUTLYNE,9106) OPNAM(I),DBLE(II),OPERND(I,8),OPERND(I,9)
     1                        ,OPERND(I,20)
                              CALL SHOWIT(10)
 9106                         FORMAT(A8,',',D23.15,',',D23.15,',',D23.15,',,',D23.15)
                          END IF
C
                          IF(OPERND(I,18).EQ.0.0D0.AND.OPERND(I,11).EQ.1.0D0
     1                    .AND.OPERND(I,12).EQ.1.0D0) THEN
                              WRITE(OUTLYNE,9107) OPNAM(I),DBLE(II),OPERND(I,8),OPERND(I,20)
                              CALL SHOWIT(10)
 9107                         FORMAT(A8,',',D23.15,',',D23.15,',,,',D23.15)
                          END IF
C
                          IF(OPERND(I,18).EQ.1.0D0.AND.OPERND(I,11).EQ.0.0D0
     1                    .AND.OPERND(I,12).EQ.1.0D0) THEN
                              WRITE(OUTLYNE,9108) OPNAM(I),DBLE(II),OPERND(I,9),OPERND(I,20)
                              CALL SHOWIT(10)
 9108                         FORMAT(A8,',',D23.15,',,',D23.15,',,',D23.15)
                          END IF
                      ELSE
                          IF(OPERND(I,18).EQ.0.0D0.AND.OPERND(I,11).EQ.0.0D0
     1                    .AND.OPERND(I,12).EQ.0.0D0) THEN
                              WRITE(OUTLYNE,1901) BWORD,OPNAM(I),DBLE(II),OPERND(I,8),
     1                        OPERND(I,9),
     1                        OPERND(I,10),OPERND(I,20)
                              CALL SHOWIT(10)
 1901                         FORMAT(A8,' ',A8,',',D23.15,',',D23.15,',',D23.15,','
     1                        ,D23.15,',',D23.15)
                          END IF
C
                          IF(OPERND(I,18).EQ.1.0D0.AND.OPERND(I,11).EQ.0.0D0
     1                    .AND.OPERND(I,12).EQ.0.0D0) THEN
                              WRITE(OUTLYNE,1902) BWORD,OPNAM(I),DBLE(II),OPERND(I,9),
     1                        OPERND(I,10),OPERND(I,20)
                              CALL SHOWIT(10)
 1902                         FORMAT(A8,' ',A8,',',D23.15,',,',D23.15,',',D23.15,',',D23.15)
                          END IF
C
                          IF(OPERND(I,18).EQ.1.0D0.AND.OPERND(I,11).EQ.1.0D0
     1                    .AND.OPERND(I,12).EQ.0.0D0) THEN
                              WRITE(OUTLYNE,1903) BWORD,OPNAM(I),DBLE(II),
     1                        OPERND(I,10),OPERND(I,20)
                              CALL SHOWIT(10)
 1903                         FORMAT(A8,' ',A8,',',D23.15,',,,',D23.15,',',D23.15)
                          END IF
C
                          IF(OPERND(I,18).EQ.1.0D0.AND.OPERND(I,11).EQ.1.0D0
     1                    .AND.OPERND(I,12).EQ.1.0D0) THEN
                              WRITE(OUTLYNE,1904) BWORD,OPNAM(I),DBLE(II),OPERND(I,20)
                              CALL SHOWIT(10)
 1904                         FORMAT(A8,' ',A8,',',D23.15,',,,,',D23.15)
                          END IF
C
                          IF(OPERND(I,18).EQ.0.0D0.AND.OPERND(I,11).EQ.1.0D0
     1                    .AND.OPERND(I,12).EQ.0.0D0) THEN
                              WRITE(OUTLYNE,1905) BWORD,OPNAM(I),DBLE(II),OPERND(I,8),
     1                        OPERND(I,10),OPERND(I,20)
                              CALL SHOWIT(10)
 1905                         FORMAT(A8,' ',A8,',',D23.15,',',D23.15,',,',D23.15,','
     1                        ,D23.15)
                          END IF
C
                          IF(OPERND(I,18).EQ.0.0D0.AND.OPERND(I,11).EQ.0.0D0
     1                    .AND.OPERND(I,12).EQ.1.0D0) THEN
                              WRITE(OUTLYNE,1906) BWORD,OPNAM(I),DBLE(II),OPERND(I,8),
     1                        OPERND(I,9)
     1                        ,OPERND(I,20)
                              CALL SHOWIT(10)
 1906                         FORMAT(A8,' ',A8,',',D23.15,',',D23.15,',',D23.15,',,'
     1                        ,D23.15)
                          END IF
C
                          IF(OPERND(I,18).EQ.0.0D0.AND.OPERND(I,11).EQ.1.0D0
     1                    .AND.OPERND(I,12).EQ.1.0D0) THEN
                              WRITE(OUTLYNE,1907) BWORD,OPNAM(I),DBLE(II),OPERND(I,8)
     1                        ,OPERND(I,20)
                              CALL SHOWIT(10)
 1907                         FORMAT(A8,' ',A8,',',D23.15,',',D23.15,',,,',D23.15)
                          END IF
C
                          IF(OPERND(I,18).EQ.1.0D0.AND.OPERND(I,11).EQ.0.0D0
     1                    .AND.OPERND(I,12).EQ.1.0D0) THEN
                              WRITE(OUTLYNE,1908) BWORD,OPNAM(I),DBLE(II),OPERND(I,9),
     1                        OPERND(I,20)
                              CALL SHOWIT(10)
 1908                         FORMAT(A8,' ',A8,',',D23.15,',,',D23.15,',,',D23.15)
                          END IF
                      END IF
C
                  END IF
                  IF(OPERDESC(I)(1:8).NE.'        ') THEN
                      WRITE(BF1,100) I-MAXFOCRIT
                      READ(BF1,110) AI1
                      OPNNMM='OP'//AI1
                      WRITE(OUTLYNE,1701) OPNNMM,OPERDESC(I)(1:69)
                      CALL SHOWIT(10)
                  END IF
              END DO
              WRITE(OUTLYNE,*)'EOS'
              CALL SHOWIT(10)
          END IF
          RETURN
      END
C SUB OPDMP.FOR
      SUBROUTINE OPDMP
C
          IMPLICIT NONE
C
C     THIS DOES THE MACDMP COMMAND AT THE CMD LEVEL
C     SYNTAX IS MACDMP (MACNAME), 0(DEFAULT) OR 1 OR 2
C     0 SAVES VARIABLES AND MERIT
C     1 SAVES VARIABLES ONLY
C     2 SAVES MERIT ONLY
C
          CHARACTER OPNNMM*5,AI1*1,BWORD*8
     1    ,AI2*2,AI3*3,BF1*1,BF2*2,BF3*3
C
          INTEGER OLDMODOP,I,OLDCFGOP,OLDCFGVB
C
!      LOGICAL EXISJK
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datmac.inc'
          INCLUDE 'datsp1.inc'
          INCLUDE 'datspd.inc'
          INCLUDE 'datsub.inc'
C
C
C       THIS IS SUBROUTINE OPDMP. THIS IS THE SUBROUTINE WHICH
C       SAVES VARIABLES AND MERIT DEFINITIONS WITH THE LENS IN
C       CURLENS AND IN LENS LIBRARIES
C
C
C     DOING BOTH VARIABLES AND MERIT DEFINITIONS
C
          IF(VBCNT.NE.0) THEN
C     DO VARAIBLES OUTPUT HERE
              OLDCFGVB=1
              WRITE(OUTLYNE,*)'VARIABLE'
              CALL SHOWIT(10)
              DO I=1,VBCNT
C     FIRST THE "CFG" COMMAND AS NEEDED
                  IF(INT(VARABL(I,2)).NE.OLDCFGVB) THEN
                      OLDCFGVB=INT(VARABL(I,2))
                      WRITE(OUTLYNE,10) OLDCFGVB
                      CALL SHOWIT(10)
 10                   FORMAT('CFG,',I2)
                  END IF
C
C     NOW VARIABLES
C
                  WRITE(OUTLYNE,11) VARNAM(I),VARABL(I,3),VARABL(I,7),VARABL(I,8)
     1            ,VARABL(I,9),VARABL(I,10)
                  CALL SHOWIT(11)
 11               FORMAT(A8,',',D15.7,',',D15.7,',',D15.7,',',D15.7,',',D15.7)
              END DO
              WRITE(OUTLYNE,*)'EOS'
              CALL SHOWIT(10)
              OLDCFGVB=1
          END IF
          IF(OPCNT.NE.0) THEN
C     DO MERIT OUTPUT HERE
C     OUTPUT CMD LEVEL SETUP COMMANDS FIRST
              IF(OPSPDTYPE.EQ.1) WRITE(OUTLYNE,21)
              IF(OPSPDTYPE.EQ.2) WRITE(OUTLYNE,22)
              IF(OPSPDTYPE.EQ.3) WRITE(OUTLYNE,23)
              CALL SHOWIT(10)
 21           FORMAT('OPSPOT RECT')
 22           FORMAT('OPSPOT RING')
 23           FORMAT('OPSPOT RAND')
              IF(OPSPDTYPE.EQ.1) THEN
                  WRITE(OUTLYNE,30) OPNRECT
                  CALL SHOWIT(10)
 30               FORMAT('OPRECT  ,',I8)
              END IF
              IF(OPSPDTYPE.EQ.2) THEN
                  WRITE(OUTLYNE,24) OPRINGTOT
                  CALL SHOWIT(10)
 24               FORMAT('OPRINGS ,',D23.15)
                  DO I=1,int(OPRINGTOT)
                      WRITE(OUTLYNE,25) I,OPRINGRAD(I),OPRINGPNT(I),OPRINGANG(I)
                      CALL SHOWIT(10)
 25                   FORMAT('OPRING  ,',I3,',',D23.15,',',I3,',',D23.15)
                  END DO
              END IF
              IF(OPSPDTYPE.EQ.3) THEN
                  WRITE(OUTLYNE,41) OPRNUMBR
                  CALL SHOWIT(10)
 41               FORMAT('OPRANNUM,',D23.15)
              END IF
              WRITE(OUTLYNE,52) OPNRD
              CALL SHOWIT(10)
 52           FORMAT('OPNRD   ,',I8)
              OLDCFGOP=1
              OLDMODOP=1
              WRITE(OUTLYNE,*)'MERIT'
              CALL SHOWIT(10)
              DO I=1,OPCNT
C     FIRST THE "CFG" COMMAND AS NEEDED
                  IF(INT(OPERND(I,16)).NE.OLDCFGOP) THEN
                      OLDCFGOP=INT(OPERND(I,16))
                      WRITE(OUTLYNE,1000) OLDCFGOP
                      CALL SHOWIT(10)
 1000                 FORMAT('CFG,',I2)
                  END IF
C     NEXT THE MODE COMMAND AS NEEDED
                  IF(INT(OPERND(I,13)).NE.OLDMODOP) THEN
                      OLDMODOP=INT(OPERND(I,13))
                      IF(OLDMODOP.EQ.1) WRITE(OUTLYNE,1001)
 1001                 FORMAT('COR')
                      IF(OLDMODOP.EQ.0) WRITE(OUTLYNE,1002)
 1002                 FORMAT('BYP')
                      IF(OLDMODOP.EQ.-2) WRITE(OUTLYNE,1003)
 1003                 FORMAT('GTE')
                      IF(OLDMODOP.EQ.2) WRITE(OUTLYNE,1004)
 1004                 FORMAT('LTE')
                      IF(OLDMODOP.EQ.10) WRITE(OUTLYNE,1005)
 1005                 FORMAT('HLD')
                      CALL SHOWIT(10)
                  END IF
C     NOW OPERANDS
C     THE OPERAND AND FUNCTION NAME AND NW1 AND NW2
                  IF(INT(OPERND(I,1)).EQ.0) BWORD='FUNC00'
                  IF(INT(OPERND(I,1)).EQ.1) BWORD='FUNC01'
                  IF(INT(OPERND(I,1)).EQ.2) BWORD='FUNC02'
                  IF(INT(OPERND(I,1)).EQ.3) BWORD='FUNC03'
                  IF(INT(OPERND(I,1)).EQ.4) BWORD='FUNC04'
                  IF(INT(OPERND(I,1)).EQ.5) BWORD='FUNC05'
                  IF(INT(OPERND(I,1)).EQ.6) BWORD='FUNC06'
                  IF(INT(OPERND(I,1)).EQ.7) BWORD='FUNC07'
                  IF(INT(OPERND(I,1)).EQ.8) BWORD='FUNC08'
                  IF(INT(OPERND(I,1)).EQ.9) BWORD='FUNC09'
                  IF(INT(OPERND(I,1)).EQ.10) BWORD='FUNC10'
                  IF(BWORD.EQ.'FUNC00') THEN
                      IF(OPERND(I,18).EQ.0.0D0.AND.OPERND(I,11).EQ.0.0D0
     1                .AND.OPERND(I,12).EQ.0.0D0) THEN
                          WRITE(OUTLYNE,101) OPNAM(I),OPERND(I,2),OPERND(I,7),OPERND(I,8)
     1                    ,OPERND(I,9),OPERND(I,10)
                          CALL SHOWIT(10)
                      END IF
 101                  FORMAT(A8,',',D23.15,',',D23.15,',',D23.15,',',D23.15,','
     1                ,D23.15)
C
                      IF(OPERND(I,18).EQ.1.0D0.AND.OPERND(I,11).EQ.0.0D0
     1                .AND.OPERND(I,12).EQ.0.0D0) THEN
                          WRITE(OUTLYNE,102) OPNAM(I),OPERND(I,2),OPERND(I,7),
     1                    OPERND(I,9),OPERND(I,10)
                          CALL SHOWIT(10)
                      END IF
 102                  FORMAT(A8,',',D23.15,',',D23.15,',,',D23.15,',',D23.15)
C
                      IF(OPERND(I,18).EQ.1.0D0.AND.OPERND(I,11).EQ.1.0D0
     1                .AND.OPERND(I,12).EQ.0.0D0) THEN
                          WRITE(OUTLYNE,103) OPNAM(I),OPERND(I,2),OPERND(I,7),OPERND(I,10)
                          CALL SHOWIT(10)
                      END IF
 103                  FORMAT(A8,',',D23.15,',',D23.15,',,,',D23.15)
C
                      IF(OPERND(I,18).EQ.1.0D0.AND.OPERND(I,11).EQ.1.0D0
     1                .AND.OPERND(I,12).EQ.1.0D0) THEN
                          WRITE(OUTLYNE,104) OPNAM(I),OPERND(I,2),OPERND(I,7)
                          CALL SHOWIT(10)
                      END IF
 104                  FORMAT(A8,',',D23.15,',',D23.15,',,,,')
C
                      IF(OPERND(I,18).EQ.0.0D0.AND.OPERND(I,11).EQ.1.0D0
     1                .AND.OPERND(I,12).EQ.0.0D0) THEN
                          WRITE(OUTLYNE,105) OPNAM(I),OPERND(I,2),OPERND(I,7),OPERND(I,8)
     1                    ,OPERND(I,10)
                          CALL SHOWIT(10)
                      END IF
 105                  FORMAT(A8,',',D23.15,',',D23.15,',',D23.15,',,',D23.15)
C
                      IF(OPERND(I,18).EQ.0.0D0.AND.OPERND(I,11).EQ.0.0D0
     1                .AND.OPERND(I,12).EQ.1.0D0) THEN
                          WRITE(OUTLYNE,106) OPNAM(I),OPERND(I,2),OPERND(I,7)
     1                    ,OPERND(I,8),OPERND(I,9)
                          CALL SHOWIT(10)
                      END IF
 106                  FORMAT(A8,',',D23.15,',',D23.15,',',D23.15,',',D23.15,',,')
C
                      IF(OPERND(I,18).EQ.0.0D0.AND.OPERND(I,11).EQ.1.0D0
     1                .AND.OPERND(I,12).EQ.1.0D0) THEN
                          WRITE(OUTLYNE,107) OPNAM(I),OPERND(I,2),OPERND(I,7),OPERND(I,8)
                          CALL SHOWIT(10)
                      END IF
 107                  FORMAT(A8,',',D23.15,',',D23.15,',',D23.15,',,,')
C
                      IF(OPERND(I,18).EQ.1.0D0.AND.OPERND(I,11).EQ.0.0D0
     1                .AND.OPERND(I,12).EQ.1.0D0) THEN
                          WRITE(OUTLYNE,108) OPNAM(I),OPERND(I,2),OPERND(I,7),OPERND(I,9)
                          CALL SHOWIT(10)
                      END IF
 108                  FORMAT(A8,',',D23.15,',',D23.15,',,',D23.15,',,')
                  ELSE
                      IF(OPERND(I,18).EQ.0.0D0.AND.OPERND(I,11).EQ.0.0D0
     1                .AND.OPERND(I,12).EQ.0.0D0) THEN
                          WRITE(OUTLYNE,1101) BWORD,OPNAM(I),OPERND(I,2),OPERND(I,7),
     1                    OPERND(I,8),OPERND(I,9),OPERND(I,10)
                          CALL SHOWIT(10)
                      END IF
 1101                 FORMAT(A8,' ',A8,',',D23.15,',',D23.15,',',D23.15,','
     1                ,D23.15,',',D23.15)
C
                      IF(OPERND(I,18).EQ.1.0D0.AND.OPERND(I,11).EQ.0.0D0
     1                .AND.OPERND(I,12).EQ.0.0D0) THEN
                          WRITE(OUTLYNE,1102) BWORD,OPNAM(I),OPERND(I,2),OPERND(I,7),
     1                    OPERND(I,9),OPERND(I,10)
                          CALL SHOWIT(10)
                      END IF
 1102                 FORMAT(A8,' ',A8,',',D23.15,',',D23.15,',,',D23.15,',',D23.15
     1                )
C
                      IF(OPERND(I,18).EQ.1.0D0.AND.OPERND(I,11).EQ.1.0D0
     1                .AND.OPERND(I,12).EQ.0.0D0) THEN
                          WRITE(OUTLYNE,1103) BWORD,OPNAM(I),OPERND(I,2),OPERND(I,7),
     1                    OPERND(I,10)
                          CALL SHOWIT(10)
                      END IF
 1103                 FORMAT(A8,' ',A8,',',D23.15,',',D23.15,',,,',D23.15)
C
                      IF(OPERND(I,18).EQ.1.0D0.AND.OPERND(I,11).EQ.1.0D0
     1                .AND.OPERND(I,12).EQ.1.0D0) THEN
                          WRITE(OUTLYNE,1104) BWORD,OPNAM(I),OPERND(I,2),OPERND(I,7)
                          CALL SHOWIT(10)
                      END IF
 1104                 FORMAT(A8,' ',A8,',',D23.15,',',D23.15,',,,,')
C
                      IF(OPERND(I,18).EQ.0.0D0.AND.OPERND(I,11).EQ.1.0D0
     1                .AND.OPERND(I,12).EQ.0.0D0) THEN
                          WRITE(OUTLYNE,1105) BWORD,OPNAM(I),OPERND(I,2),OPERND(I,7),
     1                    OPERND(I,8),OPERND(I,10)
                          CALL SHOWIT(10)
                      END IF
 1105                 FORMAT(A8,' ',A8,',',D23.15,',',D23.15,',',D23.15,',,',D23.15)
C
                      IF(OPERND(I,18).EQ.0.0D0.AND.OPERND(I,11).EQ.0.0D0
     1                .AND.OPERND(I,12).EQ.1.0D0) THEN
                          WRITE(OUTLYNE,1106) BWORD,OPNAM(I),OPERND(I,2),OPERND(I,7),
     1                    OPERND(I,8),OPERND(I,9)
                          CALL SHOWIT(10)
                      END IF
 1106                 FORMAT(A8,' ',A8,',',D23.15,',',D23.15,',',D23.15,',',D23.15,',,')
C
                      IF(OPERND(I,18).EQ.0.0D0.AND.OPERND(I,11).EQ.1.0D0
     1                .AND.OPERND(I,12).EQ.1.0D0) THEN
                          WRITE(OUTLYNE,1107) BWORD,OPNAM(I),OPERND(I,2),OPERND(I,7),
     1                    OPERND(I,8)
                          CALL SHOWIT(10)
                      END IF
 1107                 FORMAT(A8,' ',A8,',',D23.15,',',D23.15,',',D23.15,',,,')
C
                      IF(OPERND(I,18).EQ.1.0D0.AND.OPERND(I,11).EQ.0.0D0
     1                .AND.OPERND(I,12).EQ.1.0D0) THEN
                          WRITE(OUTLYNE,1108) BWORD,OPERND(I,2),OPERND(I,7),
     1                    OPERND(I,9)
                          CALL SHOWIT(10)
                      END IF
 1108                 FORMAT(A8,' ',A8,',',D23.15,',',D23.15,',,',D23.15,',,')
                  END IF
                  IF(OPERDESC(I)(1:8).NE.'        ') THEN
                      IF(I.LT.10)    WRITE(BF1,100) I
                      IF(I.GE.10.AND.I.LT.100)   WRITE(BF2,200) I
                      IF(I.GE.100.AND.I.LT.10000) WRITE(BF3,300) I
                      IF(I.LT.10)    READ(BF1,110) AI1
                      IF(I.GE.10.AND.I.LT.100)   READ(BF2,210) AI2
                      IF(I.GE.100.AND.I.LT.10000) READ(BF3,310) AI3
 100                  FORMAT(I1)
 200                  FORMAT(I2)
 300                  FORMAT(I3)
 110                  FORMAT(A1)
 210                  FORMAT(A2)
 310                  FORMAT(A3)
                      IF(I.LT.10)   OPNNMM='OP'//AI1//'  '
                      IF(I.GE.10.AND.I.LT.100)  OPNNMM='OP'//AI2//' '
                      IF(I.GE.100.AND.I.LT.1000) OPNNMM='OP'//AI3
                      WRITE(OUTLYNE,1701) OPNNMM,OPERDESC(I)(1:69)
                      CALL SHOWIT(10)
                  END IF
 1701             FORMAT('OP_DESC',1x,A5,1X,A69)
C
              END DO
              WRITE(OUTLYNE,*)'EOS'
              CALL SHOWIT(10)
          END IF
          RETURN
      END
C SUB TVARBLL.FOR
      SUBROUTINE TVARBLL
C
          IMPLICIT NONE
C
          INTEGER VBJK,II,I,J,L,VALT,DFDELTT,VBSURF,VBSURF2
C
          LOGICAL CNOT
C
          REAL*8 DELTTA,ALTRSURF,SYS12,SYS13,REFHTT
          REAL*8 DELTTA1,DELTTA2,DELTTA3
C
          INCLUDE 'datsub.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          INCLUDE 'datcfg.inc'
C
C       THIS IS SUBROUTINE TVARBLL. THIS IS THE SUBROUTINE WHICH
C       HANDLES TVAR INPUT AND TVAR UPDATE COMMANDS AND TVAR
C       OUTPUT COMMANDS AT THE CMD LEVEL
C
C       THE ARRAY VARBLL STORES TVAR INFORMATION
C       IT IS PASSED IN COMMON IN THE INCLUDE FILE DATSUB.FOR
C
C       VARBLL(I,J) WHERE I COUNTS THE NUMBER OF VARIABLE ENTRIES
C       AND J TAKES ON THE FOLLOWING VALUES AND MEANIINGS.
C
C       TVAR USES THE SECOND HALF OF VARBLL(I,J)
C
C       J=1  > 1 THROUGH 164, A VARIABLE TYPE DESIGNATOR
C       J=2  > IS NOT USED
C       J=3  > 0 THROUGH MAXSUR, THE SURFACE NUMBER DESIGNATOR
C       J=4  > VARIABLE CURRENT VALUE
C       J=5  > VARIABLE PREVIOUS VALUE
C       J=6  > LAST VARIABLE CHANGE VALUE
C       J=7  > ENDING SURFACE NUMBER FOR COMPUND VARIABLE
C       J=8  > VARIABLE DELTA VALUE
C       J=9  > X PIVOT FOR COMPOUND VARIABLE
C       J=10 > Y PIVOT FOR COMPOUND VARIABLE
C       J=11 > Z PIVOT FOR COMPOUND VARIABLE
C       J=12 > ALTERNATE ROLL SURFACE
C       J=13 > ORIGINAL VALUE
C       J=14 > IS NOT USED
C       J=15 > IS NOT USED
C       J=16 > IS NOT USED
C       J=17 > IS NOT USED
C
          IF(WC.EQ.'M'.OR.WC.EQ.'C') THEN
              CALL MESCOM
              RETURN
          END IF
C
C       'TVB' OUTPUT VARIABLE DATA FROM INSIDE
C       AND OUTSIDE OF THE VARIABLE SUBFILE VIA SUBROUTINE VBA.FOR.
          IF(WC.EQ.'TVB') THEN
              CALL TVBA
              RETURN
          END IF
C
C       NOW DO CASE OF WC = EOS
C
C***********************************************************************
C       DEAL WITH WC=EOS
          IF(WC.EQ.'EOS') THEN
              IF(SST.EQ.1.OR.SQ.EQ.1.OR.SN.EQ.1)THEN
                  WRITE(OUTLYNE,*)'"EOS" TAKES NO EXPLICIT INPUT'
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(0)
                  CALL MACFAL
                  RETURN
              END IF
C       PROCEED WITH ACTION FOR COMMAND
              F1=1
              F51=0
              IF(TVBCNT.LT.0) THEN
                  WRITE(OUTLYNE,*)'THE TVAR SUBFILE IS EMPTY'
                  CALL SHOWIT(0)
              END IF
              CALL TVARCLN
              F1=1
              F51=0
              RETURN
C       ACTION COMPLETED
          END IF
C
C       EOS DONE
C***********************************************************************
C
C       NOW DO WC=DEL
          IF(WC.EQ.'DEL') THEN
              IF(F51.NE.2) THEN
                  WRITE(OUTLYNE,*)
     1            '"DEL" IS ONLY AVAILABLE FROM THE "UPDATE TVAR" LEVEL'
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,*)'NO ACTION TAKEN'
                  CALL SHOWIT(0)
                  CALL MACFAL
                  RETURN
              END IF
              IF(SST.EQ.1.OR.S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1
     1        ) THEN
                  WRITE(OUTLYNE,*)
     1            '"DEL" ONLY TAKES QUALIFIER AND NUMERIC WORD #1'
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(0)
                  CALL MACFAL
                  RETURN
              END IF
              IF(DF1.EQ.1.OR.SQ.EQ.0) THEN
                  WRITE(OUTLYNE,*)
     1            '"DEL" REQUIRES EXPLICIT QUALIFIER AND NUMERIC WORD #1 INPUT'
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(0)
                  CALL MACFAL
                  RETURN
              END IF
C     CHECK FOR VALID QUALIFIER WORDS AND ASSIGN ASSOCIATED NUMERICAL
C     VALUES
              DELVAL = -1
              IF(WQ.EQ.'RD      ') DELVAL=1
              IF(WQ.EQ.'CV      ') DELVAL=2
              IF(WQ.EQ.'TH      ') DELVAL=3
              IF(WQ.EQ.'CC      ') DELVAL=4
              IF(WQ.EQ.'AD      ') DELVAL=5
              IF(WQ.EQ.'AE      ') DELVAL=6
              IF(WQ.EQ.'AF      ') DELVAL=7
              IF(WQ.EQ.'AG      ') DELVAL=8
              IF(WQ.EQ.'RDTOR   ') DELVAL=9
              IF(WQ.EQ.'CVTOR   ') DELVAL=10
              IF(WQ.EQ.'CCTOR   ') DELVAL=11
              IF(WQ.EQ.'ADTOR   ') DELVAL=12
              IF(WQ.EQ.'AETOR   ') DELVAL=13
              IF(WQ.EQ.'AFTOR   ') DELVAL=14
              IF(WQ.EQ.'AGTOR   ') DELVAL=15
              IF(WQ.EQ.'ALPHA   ') DELVAL=16
              IF(WQ.EQ.'BETA    ') DELVAL=17
              IF(WQ.EQ.'GAMMA   ') DELVAL=18
              IF(WQ.EQ.'XD      ') DELVAL=19
              IF(WQ.EQ.'YD      ') DELVAL=20
              IF(WQ.EQ.'N1      ') DELVAL=21
              IF(WQ.EQ.'N2      ') DELVAL=22
              IF(WQ.EQ.'N3      ') DELVAL=23
              IF(WQ.EQ.'N4      ') DELVAL=24
              IF(WQ.EQ.'N5      ') DELVAL=25
              IF(WQ.EQ.'C1      ') DELVAL=27
              IF(WQ.EQ.'C2      ') DELVAL=28
              IF(WQ.EQ.'C3      ') DELVAL=29
              IF(WQ.EQ.'C4      ') DELVAL=30
              IF(WQ.EQ.'C5      ') DELVAL=31
              IF(WQ.EQ.'C6      ') DELVAL=32
              IF(WQ.EQ.'C7      ') DELVAL=33
              IF(WQ.EQ.'C8      ') DELVAL=34
              IF(WQ.EQ.'C9      ') DELVAL=35
              IF(WQ.EQ.'C10     ') DELVAL=36
              IF(WQ.EQ.'C11     ') DELVAL=37
              IF(WQ.EQ.'C12     ') DELVAL=38
              IF(WQ.EQ.'C13     ') DELVAL=39
              IF(WQ.EQ.'C14     ') DELVAL=40
              IF(WQ.EQ.'C15     ') DELVAL=41
              IF(WQ.EQ.'C16     ') DELVAL=42
              IF(WQ.EQ.'C17     ') DELVAL=43
              IF(WQ.EQ.'C18     ') DELVAL=44
              IF(WQ.EQ.'C19     ') DELVAL=45
              IF(WQ.EQ.'C20     ') DELVAL=46
              IF(WQ.EQ.'C21     ') DELVAL=47
              IF(WQ.EQ.'C22     ') DELVAL=48
              IF(WQ.EQ.'C23     ') DELVAL=49
              IF(WQ.EQ.'C24     ') DELVAL=50
              IF(WQ.EQ.'C25     ') DELVAL=51
              IF(WQ.EQ.'C26     ') DELVAL=52
              IF(WQ.EQ.'C27     ') DELVAL=53
              IF(WQ.EQ.'C28     ') DELVAL=54
              IF(WQ.EQ.'C29     ') DELVAL=55
              IF(WQ.EQ.'C30     ') DELVAL=56
              IF(WQ.EQ.'C31     ') DELVAL=57
              IF(WQ.EQ.'C32     ') DELVAL=58
              IF(WQ.EQ.'C33     ') DELVAL=59
              IF(WQ.EQ.'C34     ') DELVAL=60
              IF(WQ.EQ.'C35     ') DELVAL=61
              IF(WQ.EQ.'C36     ') DELVAL=62
              IF(WQ.EQ.'C37     ') DELVAL=63
              IF(WQ.EQ.'C38     ') DELVAL=64
              IF(WQ.EQ.'C39     ') DELVAL=65
              IF(WQ.EQ.'C40     ') DELVAL=66
              IF(WQ.EQ.'C41     ') DELVAL=67
              IF(WQ.EQ.'C42     ') DELVAL=68
              IF(WQ.EQ.'C43     ') DELVAL=69
              IF(WQ.EQ.'C44     ') DELVAL=70
              IF(WQ.EQ.'C45     ') DELVAL=71
              IF(WQ.EQ.'C46     ') DELVAL=72
              IF(WQ.EQ.'C47     ') DELVAL=73
              IF(WQ.EQ.'C48     ') DELVAL=74
              IF(WQ.EQ.'AC      ') DELVAL=75
              IF(WQ.EQ.'C49     ') DELVAL=76
              IF(WQ.EQ.'C50     ') DELVAL=77
              IF(WQ.EQ.'C51     ') DELVAL=78
              IF(WQ.EQ.'C52     ') DELVAL=79
              IF(WQ.EQ.'C53     ') DELVAL=80
              IF(WQ.EQ.'C54     ') DELVAL=81
              IF(WQ.EQ.'C55     ') DELVAL=82
              IF(WQ.EQ.'C56     ') DELVAL=83
              IF(WQ.EQ.'C57     ') DELVAL=84
              IF(WQ.EQ.'C58     ') DELVAL=85
              IF(WQ.EQ.'C59     ') DELVAL=86
              IF(WQ.EQ.'C60     ') DELVAL=87
              IF(WQ.EQ.'C61     ') DELVAL=88
              IF(WQ.EQ.'C62     ') DELVAL=89
              IF(WQ.EQ.'C63     ') DELVAL=90
              IF(WQ.EQ.'C64     ') DELVAL=91
              IF(WQ.EQ.'C65     ') DELVAL=92
              IF(WQ.EQ.'C66     ') DELVAL=93
              IF(WQ.EQ.'C67     ') DELVAL=94
              IF(WQ.EQ.'C68     ') DELVAL=95
              IF(WQ.EQ.'C69     ') DELVAL=96
              IF(WQ.EQ.'C70     ') DELVAL=97
              IF(WQ.EQ.'C71     ') DELVAL=98
              IF(WQ.EQ.'C72     ') DELVAL=99
              IF(WQ.EQ.'C73     ') DELVAL=100
              IF(WQ.EQ.'C74     ') DELVAL=101
              IF(WQ.EQ.'C75     ') DELVAL=102
              IF(WQ.EQ.'C76     ') DELVAL=103
              IF(WQ.EQ.'C77     ') DELVAL=104
              IF(WQ.EQ.'C78     ') DELVAL=105
              IF(WQ.EQ.'C79     ') DELVAL=106
              IF(WQ.EQ.'C80     ') DELVAL=107
              IF(WQ.EQ.'C81     ') DELVAL=108
              IF(WQ.EQ.'C82     ') DELVAL=109
              IF(WQ.EQ.'C83     ') DELVAL=110
              IF(WQ.EQ.'C84     ') DELVAL=111
              IF(WQ.EQ.'C85     ') DELVAL=112
              IF(WQ.EQ.'C86     ') DELVAL=113
              IF(WQ.EQ.'C87     ') DELVAL=114
              IF(WQ.EQ.'C88     ') DELVAL=115
              IF(WQ.EQ.'C89     ') DELVAL=116
              IF(WQ.EQ.'C90     ') DELVAL=117
              IF(WQ.EQ.'C91     ') DELVAL=118
              IF(WQ.EQ.'C92     ') DELVAL=119
              IF(WQ.EQ.'C93     ') DELVAL=120
              IF(WQ.EQ.'C94     ') DELVAL=121
              IF(WQ.EQ.'C95     ') DELVAL=122
              IF(WQ.EQ.'C96     ') DELVAL=123
              IF(WQ.EQ.'N6      ') DELVAL=124
              IF(WQ.EQ.'N7      ') DELVAL=125
              IF(WQ.EQ.'N8      ') DELVAL=126
              IF(WQ.EQ.'N9      ') DELVAL=127
              IF(WQ.EQ.'N10     ') DELVAL=128
              IF(WQ.EQ.'AH      ') DELVAL=129
              IF(WQ.EQ.'AI      ') DELVAL=130
              IF(WQ.EQ.'AJ      ') DELVAL=131
              IF(WQ.EQ.'AK      ') DELVAL=132
              IF(WQ.EQ.'AL      ') DELVAL=133
              IF(WQ.EQ.'RD_FR   ') DELVAL=134
              IF(WQ.EQ.'CV_FR   ') DELVAL=135
              IF(WQ.EQ.'RDTFR   ') DELVAL=136
              IF(WQ.EQ.'CVTFR   ') DELVAL=137
              IF(WQ.EQ.'ZD      ') DELVAL=138
              IF(WQ.EQ.'INDEX   ') DELVAL=139
              IF(WQ.EQ.'VNUM    ') DELVAL=140
              IF(WQ.EQ.'PIVX    ') DELVAL=141
              IF(WQ.EQ.'PIVY    ') DELVAL=142
              IF(WQ.EQ.'PIVZ    ') DELVAL=143
              IF(WQ.EQ.'DPART   ') DELVAL=144
              IF(WQ.EQ.'CLPX    ') DELVAL=145
              IF(WQ.EQ.'CLPY    ') DELVAL=146
              IF(WQ.EQ.'GDX     ') DELVAL=147
              IF(WQ.EQ.'GDY     ') DELVAL=148
              IF(WQ.EQ.'GDZ     ') DELVAL=149
              IF(WQ.EQ.'GALPHA  ') DELVAL=150
              IF(WQ.EQ.'GBETA   ') DELVAL=151
              IF(WQ.EQ.'GGAMMA  ') DELVAL=152
              IF(WQ.EQ.'GRS     ') DELVAL=153
              IF(WQ.EQ.'DISPX   ') DELVAL=154
              IF(WQ.EQ.'DISPY   ') DELVAL=155
              IF(WQ.EQ.'DISPZ   ') DELVAL=156
              IF(WQ.EQ.'STILTA  ') DELVAL=157
              IF(WQ.EQ.'STILTB  ') DELVAL=158
              IF(WQ.EQ.'STILTG  ') DELVAL=159
              IF(WQ.EQ.'BTILTA  ') DELVAL=160
              IF(WQ.EQ.'BTILTB  ') DELVAL=161
              IF(WQ.EQ.'BTILTG  ') DELVAL=162
              IF(WQ.EQ.'ROLLX   ') DELVAL=163
              IF(WQ.EQ.'ROLLY   ') DELVAL=164
              IF(DELVAL.EQ.-1) THEN
                  WRITE(OUTLYNE,*)
     1            'INVALID QUALIFIER WORD USED WITH "DEL"'
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(0)
                  CALL MACFAL
                  RETURN
              END IF
C     NOW CHECK FOR VALID NUMERIC WORD
              IF(W1.LT.0.0D0) W1=SYSTEM1(20)+W1
              IF(INT(W1).LT.0.OR.INT(W1).GT.INT(SYSTEM1(20))) THEN
                  WRITE(OUTLYNE,*)
     1            'NUMERIC WORD #1 (SURFACE NUMBER) BEYOND LEGAL BOUNDS'
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(0)
                  CALL MACFAL
                  RETURN
              END IF
C     HERE IS WHERE VARIABLE IS DELETED
C     DOES THE DESIGNATED VARIABLE EXIST ?
              J=0
              DO I=1,TVBCNT
                  II=I+MAXCMP
                  IF(INT(VARABL(II,1)).EQ.DELVAL.AND.
     1            INT(VARABL(II,3)).EQ.INT(W1)) THEN
                      J=J+1
                      DELTAG=II
C     FOUND A MATCH, TAG IT FOR DELETION AND GO TO 95 AND DELETE IT
                      GO TO 95
                  END IF
              END DO
 95           IF(J.GT.0) THEN
C     DELETE TAGGED ITEMS
C     DELETE ITEM I=DELTAG
                  IF(TVBCNT.GT.1) THEN
                      DO I=DELTAG,((TVBCNT-1)+MAXCMP)
                          VARNAM(I)=VARNAM(I+1)
                          DO L=1,17
                              VARABL(I,L)=VARABL(I+1,L)
                          END DO
                      END DO
                      TVBCNT=TVBCNT-1
                  ELSE
C     MAKE TVBCNT IT ZERO
                      TVBCNT=0
                  END IF
C     ALL DELETIONS COMPLETED
                  CALL TVARCLN
                  RETURN
              ELSE
                  WRITE(OUTLYNE,*)
     1            'SPECIFIED TVAR WAS NOT IN THE TVAR SUBFILE'
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,*)'NO DELETION PERFORMED'
                  CALL SHOWIT(0)
                  CALL MACFAL
C     ITEM DESIGNATED WAS NOT FOUND
              END IF
              RETURN
C
          END IF
C     START DOING THE VARIABLE NAMES HERE
C FIRST DO STILTA, STILTB, STILTG, BTILTA, BTILTB, BTILTG WITH THE PIVOT
C QUALIFIER
          IF(WC.EQ.'STILTA'.OR.WC.EQ.'STILTB'.OR.WC.EQ.'STILTG'.OR.
     1    WC.EQ.'BTILTA'.OR.WC.EQ.'BTILTB'.OR.WC.EQ.'BTILTG') THEN
              IF(WQ.EQ.'PIVOT') THEN
                  IF(S4.EQ.1.OR.S5.EQ.1) THEN
                      IF(WC.EQ.'STILTA') WRITE(OUTLYNE,*)
     1                  '"STILTA PIVOT" TAKES NO NUMERIC WORD #4 OR #5 INPUT'
                      IF(WC.EQ.'STILTB') WRITE(OUTLYNE,*)
     1                  '"STILTB PIVOT" TAKES NO NUMERIC WORD #4 OR #5 INPUT'
                      IF(WC.EQ.'STILTG') WRITE(OUTLYNE,*)
     1                  '"STILTG PIVOT" TAKES NO NUMERIC WORD #4 OR #5 INPUT'
                      IF(WC.EQ.'BTILTA') WRITE(OUTLYNE,*)
     1                  '"BTILTA PIVOT" TAKES NO NUMERIC WORD #4 OR #5 INPUT'
                      IF(WC.EQ.'BTILTB') WRITE(OUTLYNE,*)
     1                  '"BTILTB PIVOT" TAKES NO NUMERIC WORD #4 OR #5 INPUT'
                      IF(WC.EQ.'BTILTG') WRITE(OUTLYNE,*)
     1                  '"BTILTG PIVOT" TAKES NO NUMERIC WORD #4 OR#5 INPUT'
                      CALL SHOWIT(0)
                      CALL MACFAL
                      RETURN
                  END IF
C     QUALIFIER INPUT OK, PROCESS AND RETURN
                  IF(WC.EQ.'STILTA') THEN
                      ASTILTXP=W1
                      ASTILTYP=W2
                      ASTILTZP=W3
                  END IF
                  IF(WC.EQ.'STILTB') THEN
                      BSTILTXP=W1
                      BSTILTYP=W2
                      BSTILTZP=W3
                  END IF
                  IF(WC.EQ.'STILTG') THEN
                      GSTILTXP=W1
                      GSTILTYP=W2
                      GSTILTZP=W3
                  END IF
                  IF(WC.EQ.'BTILTA') THEN
                      ABTILTXP=W1
                      ABTILTYP=W2
                      ABTILTZP=W3
                  END IF
                  IF(WC.EQ.'BTILTB') THEN
                      BBTILTXP=W1
                      BBTILTYP=W2
                      BBTILTZP=W3
                  END IF
                  IF(WC.EQ.'BTILTG') THEN
                      GBTILTXP=W1
                      GBTILTYP=W2
                      GBTILTZP=W3
                  END IF
                  RETURN
              ELSE
C     WQ NOT PIVOT, PROCEED
              END IF
          ELSE
C     KEEP GOING
          END IF
C
          IF(WC.EQ.'RD      ') VALT=1
          IF(WC.EQ.'CV      ') VALT=2
          IF(WC.EQ.'TH      ') VALT=3
          IF(WC.EQ.'CC      ') VALT=4
          IF(WC.EQ.'AD      ') VALT=5
          IF(WC.EQ.'AE      ') VALT=6
          IF(WC.EQ.'AF      ') VALT=7
          IF(WC.EQ.'AG      ') VALT=8
          IF(WC.EQ.'RDTOR   ') VALT=9
          IF(WC.EQ.'CVTOR   ') VALT=10
          IF(WC.EQ.'CCTOR   ') VALT=11
          IF(WC.EQ.'ADTOR   ') VALT=12
          IF(WC.EQ.'AETOR   ') VALT=13
          IF(WC.EQ.'AFTOR   ') VALT=14
          IF(WC.EQ.'AGTOR   ') VALT=15
          IF(WC.EQ.'ALPHA   ') VALT=16
          IF(WC.EQ.'BETA    ') VALT=17
          IF(WC.EQ.'GAMMA   ') VALT=18
          IF(WC.EQ.'XD      ') VALT=19
          IF(WC.EQ.'YD      ') VALT=20
          IF(WC.EQ.'N1      ') VALT=21
          IF(WC.EQ.'N2      ') VALT=22
          IF(WC.EQ.'N3      ') VALT=23
          IF(WC.EQ.'N4      ') VALT=24
          IF(WC.EQ.'N5      ') VALT=25
          IF(WC.EQ.'C1      ') VALT=27
          IF(WC.EQ.'C2      ') VALT=28
          IF(WC.EQ.'C3      ') VALT=29
          IF(WC.EQ.'C4      ') VALT=30
          IF(WC.EQ.'C5      ') VALT=31
          IF(WC.EQ.'C6      ') VALT=32
          IF(WC.EQ.'C7      ') VALT=33
          IF(WC.EQ.'C8      ') VALT=34
          IF(WC.EQ.'C9      ') VALT=35
          IF(WC.EQ.'C10     ') VALT=36
          IF(WC.EQ.'C11     ') VALT=37
          IF(WC.EQ.'C12     ') VALT=38
          IF(WC.EQ.'C13     ') VALT=39
          IF(WC.EQ.'C14     ') VALT=40
          IF(WC.EQ.'C15     ') VALT=41
          IF(WC.EQ.'C16     ') VALT=42
          IF(WC.EQ.'C17     ') VALT=43
          IF(WC.EQ.'C18     ') VALT=44
          IF(WC.EQ.'C19     ') VALT=45
          IF(WC.EQ.'C20     ') VALT=46
          IF(WC.EQ.'C21     ') VALT=47
          IF(WC.EQ.'C22     ') VALT=48
          IF(WC.EQ.'C23     ') VALT=49
          IF(WC.EQ.'C24     ') VALT=50
          IF(WC.EQ.'C25     ') VALT=51
          IF(WC.EQ.'C26     ') VALT=52
          IF(WC.EQ.'C27     ') VALT=53
          IF(WC.EQ.'C28     ') VALT=54
          IF(WC.EQ.'C29     ') VALT=55
          IF(WC.EQ.'C30     ') VALT=56
          IF(WC.EQ.'C31     ') VALT=57
          IF(WC.EQ.'C32     ') VALT=58
          IF(WC.EQ.'C33     ') VALT=59
          IF(WC.EQ.'C34     ') VALT=60
          IF(WC.EQ.'C35     ') VALT=61
          IF(WC.EQ.'C36     ') VALT=62
          IF(WC.EQ.'C37     ') VALT=63
          IF(WC.EQ.'C38     ') VALT=64
          IF(WC.EQ.'C39     ') VALT=65
          IF(WC.EQ.'C40     ') VALT=66
          IF(WC.EQ.'C41     ') VALT=67
          IF(WC.EQ.'C42     ') VALT=68
          IF(WC.EQ.'C43     ') VALT=69
          IF(WC.EQ.'C44     ') VALT=70
          IF(WC.EQ.'C45     ') VALT=71
          IF(WC.EQ.'C46     ') VALT=72
          IF(WC.EQ.'C47     ') VALT=73
          IF(WC.EQ.'C48     ') VALT=74
          IF(WC.EQ.'AC      ') VALT=75
          IF(WC.EQ.'C49     ') VALT=76
          IF(WC.EQ.'C50     ') VALT=77
          IF(WC.EQ.'C51     ') VALT=78
          IF(WC.EQ.'C52     ') VALT=79
          IF(WC.EQ.'C53     ') VALT=80
          IF(WC.EQ.'C54     ') VALT=81
          IF(WC.EQ.'C55     ') VALT=82
          IF(WC.EQ.'C56     ') VALT=83
          IF(WC.EQ.'C57     ') VALT=84
          IF(WC.EQ.'C58     ') VALT=85
          IF(WC.EQ.'C59     ') VALT=86
          IF(WC.EQ.'C60     ') VALT=87
          IF(WC.EQ.'C61     ') VALT=88
          IF(WC.EQ.'C62     ') VALT=89
          IF(WC.EQ.'C63     ') VALT=90
          IF(WC.EQ.'C64     ') VALT=91
          IF(WC.EQ.'C65     ') VALT=92
          IF(WC.EQ.'C66     ') VALT=93
          IF(WC.EQ.'C67     ') VALT=94
          IF(WC.EQ.'C68     ') VALT=95
          IF(WC.EQ.'C69     ') VALT=96
          IF(WC.EQ.'C70     ') VALT=97
          IF(WC.EQ.'C71     ') VALT=98
          IF(WC.EQ.'C72     ') VALT=99
          IF(WC.EQ.'C73     ') VALT=100
          IF(WC.EQ.'C74     ') VALT=101
          IF(WC.EQ.'C75     ') VALT=102
          IF(WC.EQ.'C76     ') VALT=103
          IF(WC.EQ.'C77     ') VALT=104
          IF(WC.EQ.'C78     ') VALT=105
          IF(WC.EQ.'C79     ') VALT=106
          IF(WC.EQ.'C80     ') VALT=107
          IF(WC.EQ.'C81     ') VALT=108
          IF(WC.EQ.'C82     ') VALT=109
          IF(WC.EQ.'C83     ') VALT=110
          IF(WC.EQ.'C84     ') VALT=111
          IF(WC.EQ.'C85     ') VALT=112
          IF(WC.EQ.'C86     ') VALT=113
          IF(WC.EQ.'C87     ') VALT=114
          IF(WC.EQ.'C88     ') VALT=115
          IF(WC.EQ.'C89     ') VALT=116
          IF(WC.EQ.'C90     ') VALT=117
          IF(WC.EQ.'C91     ') VALT=118
          IF(WC.EQ.'C92     ') VALT=119
          IF(WC.EQ.'C93     ') VALT=120
          IF(WC.EQ.'C94     ') VALT=121
          IF(WC.EQ.'C95     ') VALT=122
          IF(WC.EQ.'C96     ') VALT=123
          IF(WC.EQ.'N6      ') VALT=124
          IF(WC.EQ.'N7      ') VALT=125
          IF(WC.EQ.'N8      ') VALT=126
          IF(WC.EQ.'N9      ') VALT=127
          IF(WC.EQ.'N10     ') VALT=128
          IF(WC.EQ.'AH      ') VALT=129
          IF(WC.EQ.'AI      ') VALT=130
          IF(WC.EQ.'AJ      ') VALT=131
          IF(WC.EQ.'AK      ') VALT=132
          IF(WC.EQ.'AL      ') VALT=133
          IF(WC.EQ.'RD_FR   ') VALT=134
          IF(WC.EQ.'CV_FR   ') VALT=135
          IF(WC.EQ.'RDTFR   ') VALT=136
          IF(WC.EQ.'CVTFR   ') VALT=137
          IF(WC.EQ.'ZD      ') VALT=138
          IF(WC.EQ.'INDEX   ') VALT=139
          IF(WC.EQ.'VNUM    ') VALT=140
          IF(WC.EQ.'PIVX    ') VALT=141
          IF(WC.EQ.'PIVY    ') VALT=142
          IF(WC.EQ.'PIVZ    ') VALT=143
          IF(WC.EQ.'DPART   ') VALT=144
          IF(WC.EQ.'CLPX    ') VALT=145
          IF(WC.EQ.'CLPY    ') VALT=146
          IF(WC.EQ.'GDX     ') VALT=147
          IF(WC.EQ.'GDY     ') VALT=148
          IF(WC.EQ.'GDZ     ') VALT=149
          IF(WC.EQ.'GALPHA  ') VALT=150
          IF(WC.EQ.'GBETA   ') VALT=151
          IF(WC.EQ.'GGAMMA  ') VALT=152
          IF(WC.EQ.'GRS     ') VALT=153
          IF(WC.EQ.'DISPX   ') VALT=154
          IF(WC.EQ.'DISPY   ') VALT=155
          IF(WC.EQ.'DISPZ   ') VALT=156
          IF(WC.EQ.'STILTA  ') VALT=157
          IF(WC.EQ.'STILTB  ') VALT=158
          IF(WC.EQ.'STILTG  ') VALT=159
          IF(WC.EQ.'BTILTA  ') VALT=160
          IF(WC.EQ.'BTILTB  ') VALT=161
          IF(WC.EQ.'BTILTG  ') VALT=162
          IF(WC.EQ.'ROLLX   ') VALT=163
          IF(WC.EQ.'ROLLY   ') VALT=164
          IF(VALT.EQ.-1) THEN
              WRITE(OUTLYNE,*)
     1        'INVALID TVAR NAME'
              CALL SHOWIT(0)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(0)
              CALL MACFAL
              RETURN
          END IF
C
          IF(VALT.LE.153.OR.VALT.GE.157.AND.VALT.LE.159) THEN
              IF(S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
                  WRITE(OUTLYNE,*)
     1            'NUMERIC WORDS #3, #4 AND #5 ARE NOT USED WITH THIS TVAR INPUT'
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,*)
     1            'OR UPDATE'
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(0)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
          IF(VALT.GE.154.AND.VALT.LE.156.OR.
     1    VALT.GE.160.AND.VALT.LE.162) THEN
              IF(S4.EQ.1.OR.S5.EQ.1) THEN
                  WRITE(OUTLYNE,*)
     1            'NUMERIC WORDS #4 AND #5 ARE NOT USED WITH THIS TVAR INPUT'
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,*)
     1            'OR UPDATE'
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(0)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
          IF(VALT.GE.163.AND.VALT.LE.164) THEN
              IF(S5.EQ.1) THEN
                  WRITE(OUTLYNE,*)
     1            'NUMERIC WORD #5 IS NOT USED WITH THIS TVAR INPUT'
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,*)
     1            'OR UPDATE'
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(0)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
          IF(VALT.GE.163.AND.VALT.LE.164) THEN
              IF(DF4.EQ.1) THEN
                  DF4=0
                  S4=1
                  W4=W1
              END IF
              IF(S4.EQ.1) THEN
                  IF(W4.EQ.W1.OR.W4.EQ.W2) THEN
                  ELSE
                      WRITE(OUTLYNE,*)
     1                'FOR "ROLLX" AND "ROLLY", THE ALTERNATE PIVOT SURFACE,'
                      WRITE(OUTLYNE,*)
     1                'NUMERIC WORD #4, MUST BE SET EQUAL TO NUMERIC WORD #1 OR #2'
                      CALL SHOWIT(0)
                      WRITE(OUTLYNE,*)
     1                'OR UPDATE'
                      CALL SHOWIT(0)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(0)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
          END IF
C     NUMERIC WORDS AND DEFAULTS
C     NW1 IS SURFACE NUMBER
          IF(DF1.EQ.1) THEN
              WRITE(OUTLYNE,*)'VARIABLE NAME = ',WC
              CALL SHOWIT(0)
              WRITE(OUTLYNE,*)'REQUIRES EXPLICIT NUMERIC WORD #1 INPUT'
              CALL SHOWIT(0)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(0)
              CALL MACFAL
              RETURN
          END IF
          IF(W1.LT.0.0D0) W1=SYSTEM1(20)+W1
          IF(INT(W1).LT.0.OR.INT(W1).GT.INT(SYSTEM1(20))) THEN
              WRITE(OUTLYNE,*)
     1        'NUMERIC WORD #1 (SURFACE NUMBER) BEYOND LEGAL BOUNDS'
              CALL SHOWIT(0)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(0)
              CALL MACFAL
              RETURN
          ELSE
C     NW1 OK, PROCEED
              VBSURF=INT(W1)
          END IF
          IF(VALT.GE.154.AND.VALT.LE.156.OR.
     1    VALT.GE.160.AND.VALT.LE.164) THEN
C     NW2 IS SECOND SURFACE NUMBER
              IF(DF2.EQ.1) THEN
                  WRITE(OUTLYNE,*)'VARIABLE NAME = ',WC
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,*)'REQUIRES EXPLICIT NUMERIC WORD #2 INPUT'
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(0)
                  CALL MACFAL
                  RETURN
              END IF
              IF(W2.LT.0.0D0) W2=SYSTEM1(20)+W2
              IF(INT(W2).LT.0.OR.INT(W2).GT.INT(SYSTEM1(20)).OR.
     1        INT(W2).LT.INT(W1)) THEN
                  WRITE(OUTLYNE,*)
     1            'NUMERIC WORD #2 (SURFACE NUMBER) BEYOND LEGAL BOUNDS'
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(0)
                  CALL MACFAL
                  RETURN
              ELSE
C     NW1 OK, PROCEED
                  VBSURF2=INT(W2)
              END IF
          END IF
          IF(VALT.LE.153.OR.VALT.GE.157.AND.VALT.LE.159) THEN
C     NW2 IS DELTA VALUE
              DFDELTT=0
              IF(DF2.EQ.1) THEN
                  DFDELTT=1
C
C     CAL MAX REF AP HT FOR ASPHERIC DELTTA SETTINGS
C
C       CLAP IS ON REFERENCE SURFACE
C
C       CIRCULAR CLAP
C
                  IF(ALENS(127,NEWREF).EQ.0.0D0) THEN
                      IF(DABS(ALENS(9,NEWREF)).EQ.1.0D0) THEN
                          SYS12=ALENS(10,NEWREF)
                          SYS13=ALENS(10,NEWREF)
                      END IF
C        RECT CLAP
C
                      IF(DABS(ALENS(9,NEWREF)).EQ.2.0D0) THEN
                          SYS12=ALENS(10,NEWREF)
                          SYS13=ALENS(11,NEWREF)
                      END IF
C        ELIP CLAP
C
                      IF(DABS(ALENS(9,NEWREF)).EQ.3.0D0) THEN
                          SYS12=ALENS(10,NEWREF)
                          SYS13=ALENS(11,NEWREF)
                      END IF
C        RCTK CLAP
C
                      IF(DABS(ALENS(9,NEWREF)).EQ.4.0D0) THEN
                          SYS12=ALENS(10,NEWREF)
                          SYS13=ALENS(11,NEWREF)
                      END IF
C        POLY CLAP
C
                      IF(DABS(ALENS(9,NEWREF)).EQ.5.0D0) THEN
                          SYS12=ALENS(10,NEWREF)
                          SYS13=ALENS(10,NEWREF)
                      END IF
C        IPOLY CLAP
C
                      IF(DABS(ALENS(9,NEWREF)).EQ.6.0D0) THEN
                          SYS12=ALENS(14,NEWREF)
                          SYS13=ALENS(14,NEWREF)
                      END IF
                  END IF
C
C       NO CLAP ON REF SURF.
                  IF(DABS(ALENS(9,NEWREF)).EQ.0.0D0.OR.
     1            ALENS(127,NEWREF).NE.0.0D0) THEN
C       NO CLAP ON REF SURF.
                      SYS13=PXTRAX(1,NEWREF)
                      SYS12=PXTRAY(1,NEWREF)
                  END IF
                  IF(SYS12.GT.SYS13) REFHTT=SYS12
                  IF(SYS12.LE.SYS13) REFHTT=SYS13
C
C     THE DEFAULT DELTTA FOR CV AND CVR MUST BE ADJUSTED
C
C     DEFAULT INPUT
                  IF(WC.EQ.'RD      ') DELTTA=DELTT1
                  IF(WC.EQ.'CV      ') DELTTA=DELTT2
                  IF(WC.EQ.'RD_FR   ') DELTTA=DELTT1A
                  IF(WC.EQ.'CV_FR   ') DELTTA=DELTT2A
                  IF(WC.EQ.'TH      ') DELTTA=DELTT3
                  IF(WC.EQ.'CLPX    ') DELTTA=DELTT3
                  IF(WC.EQ.'CLPY    ') DELTTA=DELTT3
                  IF(WC.EQ.'CC      ') DELTTA=DELTT4
                  IF(REFHTT.GE.1.0D0) THEN
                      IF(WC.EQ.'AD      ') DELTTA=DELTT5A
                      IF(WC.EQ.'AE      ') DELTTA=DELTT6A
                      IF(WC.EQ.'AF      ') DELTTA=DELTT7A
                      IF(WC.EQ.'AG      ') DELTTA=DELTT8A
                      IF(WC.EQ.'AH      ') DELTTA=DELTT9A
                      IF(WC.EQ.'AI      ') DELTTA=DELTT10A
                      IF(WC.EQ.'AJ      ') DELTTA=DELTT11A
                      IF(WC.EQ.'AK      ') DELTTA=DELTT12A
                      IF(WC.EQ.'AL      ') DELTTA=DELTT13A
                      IF(WC.EQ.'AC      ') DELTTA=DELTT14A
                  ELSE
                      IF(WC.EQ.'AD      ') DELTTA=DELTT5B
                      IF(WC.EQ.'AE      ') DELTTA=DELTT6B
                      IF(WC.EQ.'AF      ') DELTTA=DELTT7B
                      IF(WC.EQ.'AG      ') DELTTA=DELTT8B
                      IF(WC.EQ.'AH      ') DELTTA=DELTT9B
                      IF(WC.EQ.'AI      ') DELTTA=DELTT10B
                      IF(WC.EQ.'AJ      ') DELTTA=DELTT11B
                      IF(WC.EQ.'AK      ') DELTTA=DELTT12B
                      IF(WC.EQ.'AL      ') DELTTA=DELTT13B
                      IF(WC.EQ.'AC      ') DELTTA=DELTT14B
                  END IF
                  IF(WC.EQ.'RDTOR   ') DELTTA=DELTT15
                  IF(WC.EQ.'CVTOR   ') DELTTA=DELTT16
                  IF(WC.EQ.'RDTFR   ') DELTTA=DELTT15A
                  IF(WC.EQ.'CVTFR   ') DELTTA=DELTT16A
                  IF(WC.EQ.'CCTOR   ') DELTTA=DELTT17
                  IF(REFHTT.GE.1.0D0) THEN
                      IF(WC.EQ.'ADTOR   ') DELTTA=DELTT18A
                      IF(WC.EQ.'AETOR   ') DELTTA=DELTT19A
                      IF(WC.EQ.'AFTOR   ') DELTTA=DELTT20A
                      IF(WC.EQ.'AGTOR   ') DELTTA=DELTT21A
                  ELSE
                      IF(WC.EQ.'ADTOR   ') DELTTA=DELTT18B
                      IF(WC.EQ.'AETOR   ') DELTTA=DELTT19B
                      IF(WC.EQ.'AFTOR   ') DELTTA=DELTT20B
                      IF(WC.EQ.'AGTOR   ') DELTTA=DELTT21B
                  END IF
                  IF(WC.EQ.'ALPHA   ') DELTTA=DELTT22
                  IF(WC.EQ.'BETA    ') DELTTA=DELTT23
                  IF(WC.EQ.'GAMMA   ') DELTTA=DELTT24
                  IF(WC.EQ.'XD      ') DELTTA=DELTT25
                  IF(WC.EQ.'YD      ') DELTTA=DELTT26
                  IF(WC.EQ.'ZD      ') DELTTA=DELTT29
                  IF(WC.EQ.'GALPHA  ') DELTTA=DELTT22
                  IF(WC.EQ.'GBETA   ') DELTTA=DELTT23
                  IF(WC.EQ.'GGAMMA  ') DELTTA=DELTT24
                  IF(WC.EQ.'GDX     ') DELTTA=DELTT25
                  IF(WC.EQ.'GDY     ') DELTTA=DELTT26
                  IF(WC.EQ.'GDZ     ') DELTTA=DELTT29
                  IF(WC.EQ.'N1      ') DELTTA=DELTT27
                  IF(WC.EQ.'N2      ') DELTTA=DELTT27
                  IF(WC.EQ.'N3      ') DELTTA=DELTT27
                  IF(WC.EQ.'N4      ') DELTTA=DELTT27
                  IF(WC.EQ.'N5      ') DELTTA=DELTT27
                  IF(WC.EQ.'N6      ') DELTTA=DELTT27
                  IF(WC.EQ.'N7      ') DELTTA=DELTT27
                  IF(WC.EQ.'N8      ') DELTTA=DELTT27
                  IF(WC.EQ.'N9      ') DELTTA=DELTT27
                  IF(WC.EQ.'N10     ') DELTTA=DELTT27
                  IF(WC.EQ.'INDEX   ') DELTTA=DELTT27
                  IF(WC.EQ.'VNUM    ') DELTTA=DELTT27
                  IF(WC.EQ.'DPART   ') DELTTA=DELTT27
                  IF(WC.EQ.'C1      ') DELTTA=DELTT28
                  IF(WC.EQ.'C2      ') DELTTA=DELTT28
                  IF(WC.EQ.'C3      ') DELTTA=DELTT28
                  IF(WC.EQ.'C4      ') DELTTA=DELTT28
                  IF(WC.EQ.'C5      ') DELTTA=DELTT28
                  IF(WC.EQ.'C6      ') DELTTA=DELTT28
                  IF(WC.EQ.'C7      ') DELTTA=DELTT28
                  IF(WC.EQ.'C8      ') DELTTA=DELTT28
                  IF(WC.EQ.'C9      ') DELTTA=DELTT28
                  IF(WC.EQ.'C10     ') DELTTA=DELTT28
                  IF(WC.EQ.'C11     ') DELTTA=DELTT28
                  IF(WC.EQ.'C12     ') DELTTA=DELTT28
                  IF(WC.EQ.'C13     ') DELTTA=DELTT28
                  IF(WC.EQ.'C14     ') DELTTA=DELTT28
                  IF(WC.EQ.'C15     ') DELTTA=DELTT28
                  IF(WC.EQ.'C16     ') DELTTA=DELTT28
                  IF(WC.EQ.'C17     ') DELTTA=DELTT28
                  IF(WC.EQ.'C18     ') DELTTA=DELTT28
                  IF(WC.EQ.'C19     ') DELTTA=DELTT28
                  IF(WC.EQ.'C20     ') DELTTA=DELTT28
                  IF(WC.EQ.'C21     ') DELTTA=DELTT28
                  IF(WC.EQ.'C22     ') DELTTA=DELTT28
                  IF(WC.EQ.'C23     ') DELTTA=DELTT28
                  IF(WC.EQ.'C24     ') DELTTA=DELTT28
                  IF(WC.EQ.'C25     ') DELTTA=DELTT28
                  IF(WC.EQ.'C26     ') DELTTA=DELTT28
                  IF(WC.EQ.'C27     ') DELTTA=DELTT28
                  IF(WC.EQ.'C28     ') DELTTA=DELTT28
                  IF(WC.EQ.'C29     ') DELTTA=DELTT28
                  IF(WC.EQ.'C30     ') DELTTA=DELTT28
                  IF(WC.EQ.'C31     ') DELTTA=DELTT28
                  IF(WC.EQ.'C32     ') DELTTA=DELTT28
                  IF(WC.EQ.'C33     ') DELTTA=DELTT28
                  IF(WC.EQ.'C34     ') DELTTA=DELTT28
                  IF(WC.EQ.'C35     ') DELTTA=DELTT28
                  IF(WC.EQ.'C36     ') DELTTA=DELTT28
                  IF(WC.EQ.'C37     ') DELTTA=DELTT28
                  IF(WC.EQ.'C38     ') DELTTA=DELTT28
                  IF(WC.EQ.'C39     ') DELTTA=DELTT28
                  IF(WC.EQ.'C40     ') DELTTA=DELTT28
                  IF(WC.EQ.'C41     ') DELTTA=DELTT28
                  IF(WC.EQ.'C42     ') DELTTA=DELTT28
                  IF(WC.EQ.'C43     ') DELTTA=DELTT28
                  IF(WC.EQ.'C44     ') DELTTA=DELTT28
                  IF(WC.EQ.'C45     ') DELTTA=DELTT28
                  IF(WC.EQ.'C46     ') DELTTA=DELTT28
                  IF(WC.EQ.'C47     ') DELTTA=DELTT28
                  IF(WC.EQ.'C48     ') DELTTA=DELTT28
                  IF(WC.EQ.'C49     ') DELTTA=DELTT28
                  IF(WC.EQ.'C50     ') DELTTA=DELTT28
                  IF(WC.EQ.'C51     ') DELTTA=DELTT28
                  IF(WC.EQ.'C52     ') DELTTA=DELTT28
                  IF(WC.EQ.'C53     ') DELTTA=DELTT28
                  IF(WC.EQ.'C54     ') DELTTA=DELTT28
                  IF(WC.EQ.'C55     ') DELTTA=DELTT28
                  IF(WC.EQ.'C56     ') DELTTA=DELTT28
                  IF(WC.EQ.'C57     ') DELTTA=DELTT28
                  IF(WC.EQ.'C58     ') DELTTA=DELTT28
                  IF(WC.EQ.'C59     ') DELTTA=DELTT28
                  IF(WC.EQ.'C60     ') DELTTA=DELTT28
                  IF(WC.EQ.'C61     ') DELTTA=DELTT28
                  IF(WC.EQ.'C62     ') DELTTA=DELTT28
                  IF(WC.EQ.'C63     ') DELTTA=DELTT28
                  IF(WC.EQ.'C64     ') DELTTA=DELTT28
                  IF(WC.EQ.'C65     ') DELTTA=DELTT28
                  IF(WC.EQ.'C66     ') DELTTA=DELTT28
                  IF(WC.EQ.'C67     ') DELTTA=DELTT28
                  IF(WC.EQ.'C68     ') DELTTA=DELTT28
                  IF(WC.EQ.'C69     ') DELTTA=DELTT28
                  IF(WC.EQ.'C70     ') DELTTA=DELTT28
                  IF(WC.EQ.'C71     ') DELTTA=DELTT28
                  IF(WC.EQ.'C72     ') DELTTA=DELTT28
                  IF(WC.EQ.'C73     ') DELTTA=DELTT28
                  IF(WC.EQ.'C74     ') DELTTA=DELTT28
                  IF(WC.EQ.'C75     ') DELTTA=DELTT28
                  IF(WC.EQ.'C76     ') DELTTA=DELTT28
                  IF(WC.EQ.'C77     ') DELTTA=DELTT28
                  IF(WC.EQ.'C78     ') DELTTA=DELTT28
                  IF(WC.EQ.'C79     ') DELTTA=DELTT28
                  IF(WC.EQ.'C80     ') DELTTA=DELTT28
                  IF(WC.EQ.'C81     ') DELTTA=DELTT28
                  IF(WC.EQ.'C82     ') DELTTA=DELTT28
                  IF(WC.EQ.'C83     ') DELTTA=DELTT28
                  IF(WC.EQ.'C84     ') DELTTA=DELTT28
                  IF(WC.EQ.'C85     ') DELTTA=DELTT28
                  IF(WC.EQ.'C86     ') DELTTA=DELTT28
                  IF(WC.EQ.'C87     ') DELTTA=DELTT28
                  IF(WC.EQ.'C88     ') DELTTA=DELTT28
                  IF(WC.EQ.'C89     ') DELTTA=DELTT28
                  IF(WC.EQ.'C90     ') DELTTA=DELTT28
                  IF(WC.EQ.'C91     ') DELTTA=DELTT28
                  IF(WC.EQ.'C92     ') DELTTA=DELTT28
                  IF(WC.EQ.'C93     ') DELTTA=DELTT28
                  IF(WC.EQ.'C94     ') DELTTA=DELTT28
                  IF(WC.EQ.'C95     ') DELTTA=DELTT28
                  IF(WC.EQ.'C96     ') DELTTA=DELTT28
                  IF(WC.EQ.'DISPX   ') DELTTA1=DELTT25
                  IF(WC.EQ.'DISPY   ') DELTTA2=DELTT26
                  IF(WC.EQ.'DISPZ   ') DELTTA3=DELTT29
                  IF(WC.EQ.'STILTA  ') DELTTA1=DELTT22
                  IF(WC.EQ.'STILTB  ') DELTTA2=DELTT23
                  IF(WC.EQ.'STILTG  ') DELTTA3=DELTT24
                  IF(WC.EQ.'BTILTA  ') DELTTA1=DELTT22
                  IF(WC.EQ.'BTILTB  ') DELTTA2=DELTT23
                  IF(WC.EQ.'BTILTG  ') DELTTA3=DELTT24
                  IF(WC.EQ.'ROLLX   ') DELTTA1=DELTT25
                  IF(WC.EQ.'ROLLY   ') DELTTA2=DELTT26
              ELSE
                  DFDELTT=0
                  DELTTA=W2
              END IF
          END IF
          IF(VALT.GE.154.AND.VALT.LE.156.OR.
     1    VALT.GE.160.AND.VALT.LE.162) THEN
C     NW3 IS DELTA VALUE
              DFDELTT=0
              IF(DF3.EQ.1) THEN
                  DFDELTT=1
C
C     CAL MAX REF AP HT FOR ASPHERIC DELTTA SETTINGS
C
C       CLAP IS ON REFERENCE SURFACE
C
C       CIRCULAR CLAP
C
                  IF(ALENS(127,NEWREF).EQ.0.0D0) THEN
                      IF(DABS(ALENS(9,NEWREF)).EQ.1.0D0) THEN
                          SYS12=ALENS(10,NEWREF)
                          SYS13=ALENS(10,NEWREF)
                      END IF
C        RECT CLAP
C
                      IF(DABS(ALENS(9,NEWREF)).EQ.2.0D0) THEN
                          SYS12=ALENS(10,NEWREF)
                          SYS13=ALENS(11,NEWREF)
                      END IF
C        ELIP CLAP
C
                      IF(DABS(ALENS(9,NEWREF)).EQ.3.0D0) THEN
                          SYS12=ALENS(10,NEWREF)
                          SYS13=ALENS(11,NEWREF)
                      END IF
C        RCTK CLAP
C
                      IF(DABS(ALENS(9,NEWREF)).EQ.4.0D0) THEN
                          SYS12=ALENS(10,NEWREF)
                          SYS13=ALENS(11,NEWREF)
                      END IF
C        POLY CLAP
C
                      IF(DABS(ALENS(9,NEWREF)).EQ.5.0D0) THEN
                          SYS12=ALENS(10,NEWREF)
                          SYS13=ALENS(10,NEWREF)
                      END IF
C        IPOLY CLAP
C
                      IF(DABS(ALENS(9,NEWREF)).EQ.6.0D0) THEN
                          SYS12=ALENS(14,NEWREF)
                          SYS13=ALENS(14,NEWREF)
                      END IF
                  END IF
C
C       NO CLAP ON REF SURF.
                  IF(DABS(ALENS(9,NEWREF)).EQ.0.0D0.OR.
     1            ALENS(127,NEWREF).NE.0.0D0) THEN
C       NO CLAP ON REF SURF.
                      SYS13=PXTRAX(1,NEWREF)
                      SYS12=PXTRAY(1,NEWREF)
                  END IF
                  IF(SYS12.GT.SYS13) REFHTT=SYS12
                  IF(SYS12.LE.SYS13) REFHTT=SYS13
C
C     THE DEFAULT DELTTA FOR CV AND CVR MUST BE ADJUSTED
C
C     DEFAULT INPUT
                  IF(WC.EQ.'RD      ') DELTTA=DELTT1
                  IF(WC.EQ.'CV      ') DELTTA=DELTT2
                  IF(WC.EQ.'RD_FR   ') DELTTA=DELTT1A
                  IF(WC.EQ.'CV_FR   ') DELTTA=DELTT2A
                  IF(WC.EQ.'TH      ') DELTTA=DELTT3
                  IF(WC.EQ.'CLPX    ') DELTTA=DELTT3
                  IF(WC.EQ.'CLPY    ') DELTTA=DELTT3
                  IF(WC.EQ.'CC      ') DELTTA=DELTT4
                  IF(REFHTT.GE.1.0D0) THEN
                      IF(WC.EQ.'AD      ') DELTTA=DELTT5A
                      IF(WC.EQ.'AE      ') DELTTA=DELTT6A
                      IF(WC.EQ.'AF      ') DELTTA=DELTT7A
                      IF(WC.EQ.'AG      ') DELTTA=DELTT8A
                      IF(WC.EQ.'AH      ') DELTTA=DELTT9A
                      IF(WC.EQ.'AI      ') DELTTA=DELTT10A
                      IF(WC.EQ.'AJ      ') DELTTA=DELTT11A
                      IF(WC.EQ.'AK      ') DELTTA=DELTT12A
                      IF(WC.EQ.'AL      ') DELTTA=DELTT13A
                      IF(WC.EQ.'AC      ') DELTTA=DELTT14A
                  ELSE
                      IF(WC.EQ.'AD      ') DELTTA=DELTT5B
                      IF(WC.EQ.'AE      ') DELTTA=DELTT6B
                      IF(WC.EQ.'AF      ') DELTTA=DELTT7B
                      IF(WC.EQ.'AG      ') DELTTA=DELTT8B
                      IF(WC.EQ.'AH      ') DELTTA=DELTT9B
                      IF(WC.EQ.'AI      ') DELTTA=DELTT10B
                      IF(WC.EQ.'AJ      ') DELTTA=DELTT11B
                      IF(WC.EQ.'AK      ') DELTTA=DELTT12B
                      IF(WC.EQ.'AL      ') DELTTA=DELTT13B
                      IF(WC.EQ.'AC      ') DELTTA=DELTT14B
                  END IF
                  IF(WC.EQ.'RDTOR   ') DELTTA=DELTT15
                  IF(WC.EQ.'CVTOR   ') DELTTA=DELTT16
                  IF(WC.EQ.'RDTFR   ') DELTTA=DELTT15A
                  IF(WC.EQ.'CVTFR   ') DELTTA=DELTT16A
                  IF(WC.EQ.'CCTOR   ') DELTTA=DELTT17
                  IF(REFHTT.GE.1.0D0) THEN
                      IF(WC.EQ.'ADTOR   ') DELTTA=DELTT18A
                      IF(WC.EQ.'AETOR   ') DELTTA=DELTT19A
                      IF(WC.EQ.'AFTOR   ') DELTTA=DELTT20A
                      IF(WC.EQ.'AGTOR   ') DELTTA=DELTT21A
                  ELSE
                      IF(WC.EQ.'ADTOR   ') DELTTA=DELTT18B
                      IF(WC.EQ.'AETOR   ') DELTTA=DELTT19B
                      IF(WC.EQ.'AFTOR   ') DELTTA=DELTT20B
                      IF(WC.EQ.'AGTOR   ') DELTTA=DELTT21B
                  END IF
                  IF(WC.EQ.'ALPHA   ') DELTTA=DELTT22
                  IF(WC.EQ.'BETA    ') DELTTA=DELTT23
                  IF(WC.EQ.'GAMMA   ') DELTTA=DELTT24
                  IF(WC.EQ.'XD      ') DELTTA=DELTT25
                  IF(WC.EQ.'YD      ') DELTTA=DELTT26
                  IF(WC.EQ.'ZD      ') DELTTA=DELTT29
                  IF(WC.EQ.'GALPHA  ') DELTTA=DELTT22
                  IF(WC.EQ.'GBETA   ') DELTTA=DELTT23
                  IF(WC.EQ.'GGAMMA  ') DELTTA=DELTT24
                  IF(WC.EQ.'GDX     ') DELTTA=DELTT25
                  IF(WC.EQ.'GDY     ') DELTTA=DELTT26
                  IF(WC.EQ.'GDZ     ') DELTTA=DELTT29
                  IF(WC.EQ.'N1      ') DELTTA=DELTT27
                  IF(WC.EQ.'N2      ') DELTTA=DELTT27
                  IF(WC.EQ.'N3      ') DELTTA=DELTT27
                  IF(WC.EQ.'N4      ') DELTTA=DELTT27
                  IF(WC.EQ.'N5      ') DELTTA=DELTT27
                  IF(WC.EQ.'N6      ') DELTTA=DELTT27
                  IF(WC.EQ.'N7      ') DELTTA=DELTT27
                  IF(WC.EQ.'N8      ') DELTTA=DELTT27
                  IF(WC.EQ.'N9      ') DELTTA=DELTT27
                  IF(WC.EQ.'N10     ') DELTTA=DELTT27
                  IF(WC.EQ.'INDEX   ') DELTTA=DELTT27
                  IF(WC.EQ.'VNUM    ') DELTTA=DELTT27
                  IF(WC.EQ.'DPART   ') DELTTA=DELTT27
                  IF(WC.EQ.'C1      ') DELTTA=DELTT28
                  IF(WC.EQ.'C2      ') DELTTA=DELTT28
                  IF(WC.EQ.'C3      ') DELTTA=DELTT28
                  IF(WC.EQ.'C4      ') DELTTA=DELTT28
                  IF(WC.EQ.'C5      ') DELTTA=DELTT28
                  IF(WC.EQ.'C6      ') DELTTA=DELTT28
                  IF(WC.EQ.'C7      ') DELTTA=DELTT28
                  IF(WC.EQ.'C8      ') DELTTA=DELTT28
                  IF(WC.EQ.'C9      ') DELTTA=DELTT28
                  IF(WC.EQ.'C10     ') DELTTA=DELTT28
                  IF(WC.EQ.'C11     ') DELTTA=DELTT28
                  IF(WC.EQ.'C12     ') DELTTA=DELTT28
                  IF(WC.EQ.'C13     ') DELTTA=DELTT28
                  IF(WC.EQ.'C14     ') DELTTA=DELTT28
                  IF(WC.EQ.'C15     ') DELTTA=DELTT28
                  IF(WC.EQ.'C16     ') DELTTA=DELTT28
                  IF(WC.EQ.'C17     ') DELTTA=DELTT28
                  IF(WC.EQ.'C18     ') DELTTA=DELTT28
                  IF(WC.EQ.'C19     ') DELTTA=DELTT28
                  IF(WC.EQ.'C20     ') DELTTA=DELTT28
                  IF(WC.EQ.'C21     ') DELTTA=DELTT28
                  IF(WC.EQ.'C22     ') DELTTA=DELTT28
                  IF(WC.EQ.'C23     ') DELTTA=DELTT28
                  IF(WC.EQ.'C24     ') DELTTA=DELTT28
                  IF(WC.EQ.'C25     ') DELTTA=DELTT28
                  IF(WC.EQ.'C26     ') DELTTA=DELTT28
                  IF(WC.EQ.'C27     ') DELTTA=DELTT28
                  IF(WC.EQ.'C28     ') DELTTA=DELTT28
                  IF(WC.EQ.'C29     ') DELTTA=DELTT28
                  IF(WC.EQ.'C30     ') DELTTA=DELTT28
                  IF(WC.EQ.'C31     ') DELTTA=DELTT28
                  IF(WC.EQ.'C32     ') DELTTA=DELTT28
                  IF(WC.EQ.'C33     ') DELTTA=DELTT28
                  IF(WC.EQ.'C34     ') DELTTA=DELTT28
                  IF(WC.EQ.'C35     ') DELTTA=DELTT28
                  IF(WC.EQ.'C36     ') DELTTA=DELTT28
                  IF(WC.EQ.'C37     ') DELTTA=DELTT28
                  IF(WC.EQ.'C38     ') DELTTA=DELTT28
                  IF(WC.EQ.'C39     ') DELTTA=DELTT28
                  IF(WC.EQ.'C40     ') DELTTA=DELTT28
                  IF(WC.EQ.'C41     ') DELTTA=DELTT28
                  IF(WC.EQ.'C42     ') DELTTA=DELTT28
                  IF(WC.EQ.'C43     ') DELTTA=DELTT28
                  IF(WC.EQ.'C44     ') DELTTA=DELTT28
                  IF(WC.EQ.'C45     ') DELTTA=DELTT28
                  IF(WC.EQ.'C46     ') DELTTA=DELTT28
                  IF(WC.EQ.'C47     ') DELTTA=DELTT28
                  IF(WC.EQ.'C48     ') DELTTA=DELTT28
                  IF(WC.EQ.'C49     ') DELTTA=DELTT28
                  IF(WC.EQ.'C50     ') DELTTA=DELTT28
                  IF(WC.EQ.'C51     ') DELTTA=DELTT28
                  IF(WC.EQ.'C52     ') DELTTA=DELTT28
                  IF(WC.EQ.'C53     ') DELTTA=DELTT28
                  IF(WC.EQ.'C54     ') DELTTA=DELTT28
                  IF(WC.EQ.'C55     ') DELTTA=DELTT28
                  IF(WC.EQ.'C56     ') DELTTA=DELTT28
                  IF(WC.EQ.'C57     ') DELTTA=DELTT28
                  IF(WC.EQ.'C58     ') DELTTA=DELTT28
                  IF(WC.EQ.'C59     ') DELTTA=DELTT28
                  IF(WC.EQ.'C60     ') DELTTA=DELTT28
                  IF(WC.EQ.'C61     ') DELTTA=DELTT28
                  IF(WC.EQ.'C62     ') DELTTA=DELTT28
                  IF(WC.EQ.'C63     ') DELTTA=DELTT28
                  IF(WC.EQ.'C64     ') DELTTA=DELTT28
                  IF(WC.EQ.'C65     ') DELTTA=DELTT28
                  IF(WC.EQ.'C66     ') DELTTA=DELTT28
                  IF(WC.EQ.'C67     ') DELTTA=DELTT28
                  IF(WC.EQ.'C68     ') DELTTA=DELTT28
                  IF(WC.EQ.'C69     ') DELTTA=DELTT28
                  IF(WC.EQ.'C70     ') DELTTA=DELTT28
                  IF(WC.EQ.'C71     ') DELTTA=DELTT28
                  IF(WC.EQ.'C72     ') DELTTA=DELTT28
                  IF(WC.EQ.'C73     ') DELTTA=DELTT28
                  IF(WC.EQ.'C74     ') DELTTA=DELTT28
                  IF(WC.EQ.'C75     ') DELTTA=DELTT28
                  IF(WC.EQ.'C76     ') DELTTA=DELTT28
                  IF(WC.EQ.'C77     ') DELTTA=DELTT28
                  IF(WC.EQ.'C78     ') DELTTA=DELTT28
                  IF(WC.EQ.'C79     ') DELTTA=DELTT28
                  IF(WC.EQ.'C80     ') DELTTA=DELTT28
                  IF(WC.EQ.'C81     ') DELTTA=DELTT28
                  IF(WC.EQ.'C82     ') DELTTA=DELTT28
                  IF(WC.EQ.'C83     ') DELTTA=DELTT28
                  IF(WC.EQ.'C84     ') DELTTA=DELTT28
                  IF(WC.EQ.'C85     ') DELTTA=DELTT28
                  IF(WC.EQ.'C86     ') DELTTA=DELTT28
                  IF(WC.EQ.'C87     ') DELTTA=DELTT28
                  IF(WC.EQ.'C88     ') DELTTA=DELTT28
                  IF(WC.EQ.'C89     ') DELTTA=DELTT28
                  IF(WC.EQ.'C90     ') DELTTA=DELTT28
                  IF(WC.EQ.'C91     ') DELTTA=DELTT28
                  IF(WC.EQ.'C92     ') DELTTA=DELTT28
                  IF(WC.EQ.'C93     ') DELTTA=DELTT28
                  IF(WC.EQ.'C94     ') DELTTA=DELTT28
                  IF(WC.EQ.'C95     ') DELTTA=DELTT28
                  IF(WC.EQ.'C96     ') DELTTA=DELTT28
                  IF(WC.EQ.'DISPX   ') DELTTA1=DELTT25
                  IF(WC.EQ.'DISPY   ') DELTTA2=DELTT26
                  IF(WC.EQ.'DISPZ   ') DELTTA3=DELTT29
                  IF(WC.EQ.'STILTA  ') DELTTA1=DELTT22
                  IF(WC.EQ.'STILTB  ') DELTTA2=DELTT23
                  IF(WC.EQ.'STILTG  ') DELTTA3=DELTT24
                  IF(WC.EQ.'BTILTA  ') DELTTA1=DELTT22
                  IF(WC.EQ.'BTILTB  ') DELTTA2=DELTT23
                  IF(WC.EQ.'BTILTG  ') DELTTA3=DELTT24
                  IF(WC.EQ.'ROLLX   ') DELTTA1=DELTT25
                  IF(WC.EQ.'ROLLY   ') DELTTA2=DELTT26
              ELSE
                  DFDELTT=0
                  DELTTA=W3
              END IF
          END IF
          IF(VALT.GE.163.AND.VALT.LE.164) THEN
C     NW3 IS DELTA VALUE
              DFDELTT=0
              IF(DF3.EQ.1) THEN
                  DFDELTT=1
C
C     CAL MAX REF AP HT FOR ASPHERIC DELTTA SETTINGS
C
C       CLAP IS ON REFERENCE SURFACE
C
C       CIRCULAR CLAP
C
                  IF(ALENS(127,NEWREF).EQ.0.0D0) THEN
                      IF(DABS(ALENS(9,NEWREF)).EQ.1.0D0) THEN
                          SYS12=ALENS(10,NEWREF)
                          SYS13=ALENS(10,NEWREF)
                      END IF
C        RECT CLAP
C
                      IF(DABS(ALENS(9,NEWREF)).EQ.2.0D0) THEN
                          SYS12=ALENS(10,NEWREF)
                          SYS13=ALENS(11,NEWREF)
                      END IF
C        ELIP CLAP
C
                      IF(DABS(ALENS(9,NEWREF)).EQ.3.0D0) THEN
                          SYS12=ALENS(10,NEWREF)
                          SYS13=ALENS(11,NEWREF)
                      END IF
C        RCTK CLAP
C
                      IF(DABS(ALENS(9,NEWREF)).EQ.4.0D0) THEN
                          SYS12=ALENS(10,NEWREF)
                          SYS13=ALENS(11,NEWREF)
                      END IF
C        POLY CLAP
C
                      IF(DABS(ALENS(9,NEWREF)).EQ.5.0D0) THEN
                          SYS12=ALENS(10,NEWREF)
                          SYS13=ALENS(10,NEWREF)
                      END IF
C        IPOLY CLAP
C
                      IF(DABS(ALENS(9,NEWREF)).EQ.6.0D0) THEN
                          SYS12=ALENS(14,NEWREF)
                          SYS13=ALENS(14,NEWREF)
                      END IF
                  END IF
C
C       NO CLAP ON REF SURF.
                  IF(DABS(ALENS(9,NEWREF)).EQ.0.0D0.OR.
     1            ALENS(127,NEWREF).NE.0.0D0) THEN
C       NO CLAP ON REF SURF.
                      SYS13=PXTRAX(1,NEWREF)
                      SYS12=PXTRAY(1,NEWREF)
                  END IF
                  IF(SYS12.GT.SYS13) REFHTT=SYS12
                  IF(SYS12.LE.SYS13) REFHTT=SYS13
C
C     THE DEFAULT DELTTA FOR CV AND CVR MUST BE ADJUSTED
C
C     DEFAULT INPUT
                  IF(WC.EQ.'RD      ') DELTTA=DELTT1
                  IF(WC.EQ.'CV      ') DELTTA=DELTT2
                  IF(WC.EQ.'RD_FR   ') DELTTA=DELTT1A
                  IF(WC.EQ.'CV_FR   ') DELTTA=DELTT2A
                  IF(WC.EQ.'TH      ') DELTTA=DELTT3
                  IF(WC.EQ.'CLPX    ') DELTTA=DELTT3
                  IF(WC.EQ.'CLPY    ') DELTTA=DELTT3
                  IF(WC.EQ.'CC      ') DELTTA=DELTT4
                  IF(REFHTT.GE.1.0D0) THEN
                      IF(WC.EQ.'AD      ') DELTTA=DELTT5A
                      IF(WC.EQ.'AE      ') DELTTA=DELTT6A
                      IF(WC.EQ.'AF      ') DELTTA=DELTT7A
                      IF(WC.EQ.'AG      ') DELTTA=DELTT8A
                      IF(WC.EQ.'AH      ') DELTTA=DELTT9A
                      IF(WC.EQ.'AI      ') DELTTA=DELTT10A
                      IF(WC.EQ.'AJ      ') DELTTA=DELTT11A
                      IF(WC.EQ.'AK      ') DELTTA=DELTT12A
                      IF(WC.EQ.'AL      ') DELTTA=DELTT13A
                      IF(WC.EQ.'AC      ') DELTTA=DELTT14A
                  ELSE
                      IF(WC.EQ.'AD      ') DELTTA=DELTT5B
                      IF(WC.EQ.'AE      ') DELTTA=DELTT6B
                      IF(WC.EQ.'AF      ') DELTTA=DELTT7B
                      IF(WC.EQ.'AG      ') DELTTA=DELTT8B
                      IF(WC.EQ.'AH      ') DELTTA=DELTT9B
                      IF(WC.EQ.'AI      ') DELTTA=DELTT10B
                      IF(WC.EQ.'AJ      ') DELTTA=DELTT11B
                      IF(WC.EQ.'AK      ') DELTTA=DELTT12B
                      IF(WC.EQ.'AL      ') DELTTA=DELTT13B
                      IF(WC.EQ.'AC      ') DELTTA=DELTT14B
                  END IF
                  IF(WC.EQ.'RDTOR   ') DELTTA=DELTT15
                  IF(WC.EQ.'CVTOR   ') DELTTA=DELTT16
                  IF(WC.EQ.'RDTFR   ') DELTTA=DELTT15A
                  IF(WC.EQ.'CVTFR   ') DELTTA=DELTT16A
                  IF(WC.EQ.'CCTOR   ') DELTTA=DELTT17
                  IF(REFHTT.GE.1.0D0) THEN
                      IF(WC.EQ.'ADTOR   ') DELTTA=DELTT18A
                      IF(WC.EQ.'AETOR   ') DELTTA=DELTT19A
                      IF(WC.EQ.'AFTOR   ') DELTTA=DELTT20A
                      IF(WC.EQ.'AGTOR   ') DELTTA=DELTT21A
                  ELSE
                      IF(WC.EQ.'ADTOR   ') DELTTA=DELTT18B
                      IF(WC.EQ.'AETOR   ') DELTTA=DELTT19B
                      IF(WC.EQ.'AFTOR   ') DELTTA=DELTT20B
                      IF(WC.EQ.'AGTOR   ') DELTTA=DELTT21B
                  END IF
                  IF(WC.EQ.'ALPHA   ') DELTTA=DELTT22
                  IF(WC.EQ.'BETA    ') DELTTA=DELTT23
                  IF(WC.EQ.'GAMMA   ') DELTTA=DELTT24
                  IF(WC.EQ.'XD      ') DELTTA=DELTT25
                  IF(WC.EQ.'YD      ') DELTTA=DELTT26
                  IF(WC.EQ.'ZD      ') DELTTA=DELTT29
                  IF(WC.EQ.'GALPHA  ') DELTTA=DELTT22
                  IF(WC.EQ.'GBETA   ') DELTTA=DELTT23
                  IF(WC.EQ.'GGAMMA  ') DELTTA=DELTT24
                  IF(WC.EQ.'GDX     ') DELTTA=DELTT25
                  IF(WC.EQ.'GDY     ') DELTTA=DELTT26
                  IF(WC.EQ.'GDZ     ') DELTTA=DELTT29
                  IF(WC.EQ.'N1      ') DELTTA=DELTT27
                  IF(WC.EQ.'N2      ') DELTTA=DELTT27
                  IF(WC.EQ.'N3      ') DELTTA=DELTT27
                  IF(WC.EQ.'N4      ') DELTTA=DELTT27
                  IF(WC.EQ.'N5      ') DELTTA=DELTT27
                  IF(WC.EQ.'N6      ') DELTTA=DELTT27
                  IF(WC.EQ.'N7      ') DELTTA=DELTT27
                  IF(WC.EQ.'N8      ') DELTTA=DELTT27
                  IF(WC.EQ.'N9      ') DELTTA=DELTT27
                  IF(WC.EQ.'N10     ') DELTTA=DELTT27
                  IF(WC.EQ.'INDEX   ') DELTTA=DELTT27
                  IF(WC.EQ.'VNUM    ') DELTTA=DELTT27
                  IF(WC.EQ.'DPART   ') DELTTA=DELTT27
                  IF(WC.EQ.'C1      ') DELTTA=DELTT28
                  IF(WC.EQ.'C2      ') DELTTA=DELTT28
                  IF(WC.EQ.'C3      ') DELTTA=DELTT28
                  IF(WC.EQ.'C4      ') DELTTA=DELTT28
                  IF(WC.EQ.'C5      ') DELTTA=DELTT28
                  IF(WC.EQ.'C6      ') DELTTA=DELTT28
                  IF(WC.EQ.'C7      ') DELTTA=DELTT28
                  IF(WC.EQ.'C8      ') DELTTA=DELTT28
                  IF(WC.EQ.'C9      ') DELTTA=DELTT28
                  IF(WC.EQ.'C10     ') DELTTA=DELTT28
                  IF(WC.EQ.'C11     ') DELTTA=DELTT28
                  IF(WC.EQ.'C12     ') DELTTA=DELTT28
                  IF(WC.EQ.'C13     ') DELTTA=DELTT28
                  IF(WC.EQ.'C14     ') DELTTA=DELTT28
                  IF(WC.EQ.'C15     ') DELTTA=DELTT28
                  IF(WC.EQ.'C16     ') DELTTA=DELTT28
                  IF(WC.EQ.'C17     ') DELTTA=DELTT28
                  IF(WC.EQ.'C18     ') DELTTA=DELTT28
                  IF(WC.EQ.'C19     ') DELTTA=DELTT28
                  IF(WC.EQ.'C20     ') DELTTA=DELTT28
                  IF(WC.EQ.'C21     ') DELTTA=DELTT28
                  IF(WC.EQ.'C22     ') DELTTA=DELTT28
                  IF(WC.EQ.'C23     ') DELTTA=DELTT28
                  IF(WC.EQ.'C24     ') DELTTA=DELTT28
                  IF(WC.EQ.'C25     ') DELTTA=DELTT28
                  IF(WC.EQ.'C26     ') DELTTA=DELTT28
                  IF(WC.EQ.'C27     ') DELTTA=DELTT28
                  IF(WC.EQ.'C28     ') DELTTA=DELTT28
                  IF(WC.EQ.'C29     ') DELTTA=DELTT28
                  IF(WC.EQ.'C30     ') DELTTA=DELTT28
                  IF(WC.EQ.'C31     ') DELTTA=DELTT28
                  IF(WC.EQ.'C32     ') DELTTA=DELTT28
                  IF(WC.EQ.'C33     ') DELTTA=DELTT28
                  IF(WC.EQ.'C34     ') DELTTA=DELTT28
                  IF(WC.EQ.'C35     ') DELTTA=DELTT28
                  IF(WC.EQ.'C36     ') DELTTA=DELTT28
                  IF(WC.EQ.'C37     ') DELTTA=DELTT28
                  IF(WC.EQ.'C38     ') DELTTA=DELTT28
                  IF(WC.EQ.'C39     ') DELTTA=DELTT28
                  IF(WC.EQ.'C40     ') DELTTA=DELTT28
                  IF(WC.EQ.'C41     ') DELTTA=DELTT28
                  IF(WC.EQ.'C42     ') DELTTA=DELTT28
                  IF(WC.EQ.'C43     ') DELTTA=DELTT28
                  IF(WC.EQ.'C44     ') DELTTA=DELTT28
                  IF(WC.EQ.'C45     ') DELTTA=DELTT28
                  IF(WC.EQ.'C46     ') DELTTA=DELTT28
                  IF(WC.EQ.'C47     ') DELTTA=DELTT28
                  IF(WC.EQ.'C48     ') DELTTA=DELTT28
                  IF(WC.EQ.'C49     ') DELTTA=DELTT28
                  IF(WC.EQ.'C50     ') DELTTA=DELTT28
                  IF(WC.EQ.'C51     ') DELTTA=DELTT28
                  IF(WC.EQ.'C52     ') DELTTA=DELTT28
                  IF(WC.EQ.'C53     ') DELTTA=DELTT28
                  IF(WC.EQ.'C54     ') DELTTA=DELTT28
                  IF(WC.EQ.'C55     ') DELTTA=DELTT28
                  IF(WC.EQ.'C56     ') DELTTA=DELTT28
                  IF(WC.EQ.'C57     ') DELTTA=DELTT28
                  IF(WC.EQ.'C58     ') DELTTA=DELTT28
                  IF(WC.EQ.'C59     ') DELTTA=DELTT28
                  IF(WC.EQ.'C60     ') DELTTA=DELTT28
                  IF(WC.EQ.'C61     ') DELTTA=DELTT28
                  IF(WC.EQ.'C62     ') DELTTA=DELTT28
                  IF(WC.EQ.'C63     ') DELTTA=DELTT28
                  IF(WC.EQ.'C64     ') DELTTA=DELTT28
                  IF(WC.EQ.'C65     ') DELTTA=DELTT28
                  IF(WC.EQ.'C66     ') DELTTA=DELTT28
                  IF(WC.EQ.'C67     ') DELTTA=DELTT28
                  IF(WC.EQ.'C68     ') DELTTA=DELTT28
                  IF(WC.EQ.'C69     ') DELTTA=DELTT28
                  IF(WC.EQ.'C70     ') DELTTA=DELTT28
                  IF(WC.EQ.'C71     ') DELTTA=DELTT28
                  IF(WC.EQ.'C72     ') DELTTA=DELTT28
                  IF(WC.EQ.'C73     ') DELTTA=DELTT28
                  IF(WC.EQ.'C74     ') DELTTA=DELTT28
                  IF(WC.EQ.'C75     ') DELTTA=DELTT28
                  IF(WC.EQ.'C76     ') DELTTA=DELTT28
                  IF(WC.EQ.'C77     ') DELTTA=DELTT28
                  IF(WC.EQ.'C78     ') DELTTA=DELTT28
                  IF(WC.EQ.'C79     ') DELTTA=DELTT28
                  IF(WC.EQ.'C80     ') DELTTA=DELTT28
                  IF(WC.EQ.'C81     ') DELTTA=DELTT28
                  IF(WC.EQ.'C82     ') DELTTA=DELTT28
                  IF(WC.EQ.'C83     ') DELTTA=DELTT28
                  IF(WC.EQ.'C84     ') DELTTA=DELTT28
                  IF(WC.EQ.'C85     ') DELTTA=DELTT28
                  IF(WC.EQ.'C86     ') DELTTA=DELTT28
                  IF(WC.EQ.'C87     ') DELTTA=DELTT28
                  IF(WC.EQ.'C88     ') DELTTA=DELTT28
                  IF(WC.EQ.'C89     ') DELTTA=DELTT28
                  IF(WC.EQ.'C90     ') DELTTA=DELTT28
                  IF(WC.EQ.'C91     ') DELTTA=DELTT28
                  IF(WC.EQ.'C92     ') DELTTA=DELTT28
                  IF(WC.EQ.'C93     ') DELTTA=DELTT28
                  IF(WC.EQ.'C94     ') DELTTA=DELTT28
                  IF(WC.EQ.'C95     ') DELTTA=DELTT28
                  IF(WC.EQ.'C96     ') DELTTA=DELTT28
                  IF(WC.EQ.'DISPX   ') DELTTA1=DELTT25
                  IF(WC.EQ.'DISPY   ') DELTTA2=DELTT26
                  IF(WC.EQ.'DISPZ   ') DELTTA3=DELTT29
                  IF(WC.EQ.'STILTA  ') DELTTA1=DELTT22
                  IF(WC.EQ.'STILTB  ') DELTTA2=DELTT23
                  IF(WC.EQ.'STILTG  ') DELTTA3=DELTT24
                  IF(WC.EQ.'BTILTA  ') DELTTA1=DELTT22
                  IF(WC.EQ.'BTILTB  ') DELTTA2=DELTT23
                  IF(WC.EQ.'BTILTG  ') DELTTA3=DELTT24
                  IF(WC.EQ.'ROLLX   ') DELTTA1=DELTT25
                  IF(WC.EQ.'ROLLY   ') DELTTA2=DELTT26
              ELSE
                  DFDELTT=0
                  DELTTA=W3
              END IF
              IF(DF4.EQ.1) W4=W1
              ALTRSURF=INT(W4)
          END IF
C
C     NOW BUILD THE VARIABLE ENTRY
C       J=1  > 1 THROUGH 164, A VARIABLE TYPE DESIGNATOR
C       J=2  > IS NOT USED
C       J=3  > 0 THROUGH MAXSUR, THE SURFACE NUMBER DESIGNATOR
C       J=4  > VARIABLE CURRENT VALUE
C       J=5  > VARIABLE PREVIOUS VALUE
C       J=6  > LAST VARIABLE CHANGE VALUE
C       J=7  > ENDING SURFACE NUMBER FOR COMPUND VARIABLE
C       J=8  > VARIABLE DELTA VALUE
C       J=9  > X PIVOT FOR COMPOUND VARIABLE
C       J=10 > Y PIVOT FOR COMPOUND VARIABLE
C       J=11 > Z PIVOT FOR COMPOUND VARIABLE
C       J=12 > IS NOT USED
C       J=13 > ORIGINAL VALUE
C       J=14 > IS NOT USED
C       J=15 > IS NOT USED
C       J=16 > IS NOT USED
C       J=17 > IS NOT USED
C
C     CHECK FOR A POSSIBLE OVERFLOW
          IF(TVBCNT.EQ.MAXTVB) THEN
C     NO MORE TOL VARIABLES
              WRITE(OUTLYNE,*)
     1        'THE MAXIMUM OF ',MAXTVB,' TVARS HAS BEEN REACHED'
              CALL SHOWIT(0)
              WRITE(OUTLYNE,*)'NO MORE MAY BE ENTERED'
              CALL SHOWIT(0)
              CALL MACFAL
              RETURN
          END IF
          IF(VALT.GE.154.AND.VALT.LE.156.OR.VALT.GE.160.AND.
     1    VALT.LE.164) THEN
              VBJK=TVBCNT+MAXCMP
              VARNAM(VBJK+1)=WC
              VARABL(VBJK+1,1) =DBLE(VALT)
              VARABL(VBJK+1,2) =0.0D0
              VARABL(VBJK+1,3) =DBLE(VBSURF)
              VARABL(VBJK+1,4) =0.0D0
              VARABL(VBJK+1,5) =0.0D0
              VARABL(VBJK+1,6) =0.0D0
              VARABL(VBJK+1,7) =DBLE(VBSURF2)
              VARABL(VBJK+1,8) =DELTTA
              VARABL(VBJK+1,9) =0.0D0
              VARABL(VBJK+1,10)=0.0D0
              VARABL(VBJK+1,11)=0.0D0
C     DFDELTT=DF2
              VARABL(VBJK+1,12)=DBLE(DFDELTT)
              VARABL(VBJK+1,13)=0.0D0
              VARABL(VBJK+1,14)=0.0D0
              VARABL(VBJK+1,15)=0.0D0
              VARABL(VBJK+1,16)=0.0D0
              VARABL(VBJK+1,17)=0.0D0
C
C     DO DISPX
              IF(VALT.EQ.154) THEN
                  TOLER(1,VBSURF)=W3
                  TOLER(2,VBSURF)=W2
                  VARABL(VBJK+1,7)=TOLER(2,VBSURF)
                  VARABL(VBJK+1,4)=TOLER(1,VBSURF)
                  VARABL(VBJK+1,5)=TOLER(1,VBSURF)
                  VARABL(VBJK+1,13)=TOLER(1,VBSURF)
                  TVBCNT=TVBCNT+1
                  CALL TVARCLN
                  RETURN
              END IF
C
C     DO DISPY
              IF(VALT.EQ.155) THEN
                  TOLER(3,VBSURF)=W3
                  TOLER(4,VBSURF)=W2
                  VARABL(VBJK+1,7)=TOLER(4,VBSURF)
                  VARABL(VBJK+1,4)=TOLER(3,VBSURF)
                  VARABL(VBJK+1,5)=TOLER(3,VBSURF)
                  VARABL(VBJK+1,13)=TOLER(3,VBSURF)
                  TVBCNT=TVBCNT+1
                  CALL TVARCLN
                  RETURN
              END IF
C
C     DO DISPZ
              IF(VALT.EQ.156) THEN
                  TOLER(5,VBSURF)=W3
                  TOLER(6,VBSURF)=W2
                  VARABL(VBJK+1,7)=TOLER(6,VBSURF)
                  VARABL(VBJK+1,4)=TOLER(5,VBSURF)
                  VARABL(VBJK+1,5)=TOLER(5,VBSURF)
                  VARABL(VBJK+1,13)=TOLER(5,VBSURF)
                  TVBCNT=TVBCNT+1
                  CALL TVARCLN
                  RETURN
              END IF
C
C     DO BTILTA
              IF(VALT.EQ.160) THEN
                  TOLER(19,VBSURF)=W3
                  TOLER(20,VBSURF)=W2
                  TOLER(21,VBSURF)=ABTILTXP
                  TOLER(22,VBSURF)=ABTILTYP
                  TOLER(23,VBSURF)=ABTILTZP
                  ABTILTXP=0.0D0
                  ABTILTYP=0.0D0
                  ABTILTZP=0.0D0
                  VARABL(VBJK+1,7)=TOLER(20,VBSURF)
                  VARABL(VBJK+1,4)=TOLER(19,VBSURF)
                  VARABL(VBJK+1,5)=TOLER(19,VBSURF)
                  VARABL(VBJK+1,13)=TOLER(19,VBSURF)
                  VARABL(VBJK+1,9)=TOLER(21,VBSURF)
                  VARABL(VBJK+1,10)=TOLER(22,VBSURF)
                  VARABL(VBJK+1,11)=TOLER(23,VBSURF)
                  TVBCNT=TVBCNT+1
                  CALL TVARCLN
                  RETURN
              END IF
C     DO BTILTB
              IF(VALT.EQ.161) THEN
                  TOLER(24,VBSURF)=W3
                  TOLER(25,VBSURF)=W2
                  TOLER(26,VBSURF)=BBTILTXP
                  TOLER(27,VBSURF)=BBTILTYP
                  TOLER(28,VBSURF)=BBTILTZP
                  BBTILTXP=0.0D0
                  BBTILTYP=0.0D0
                  BBTILTZP=0.0D0
                  VARABL(VBJK+1,7)=TOLER(25,VBSURF)
                  VARABL(VBJK+1,4)=TOLER(24,VBSURF)
                  VARABL(VBJK+1,5)=TOLER(24,VBSURF)
                  VARABL(VBJK+1,13)=TOLER(24,VBSURF)
                  VARABL(VBJK+1,9)=TOLER(26,VBSURF)
                  VARABL(VBJK+1,10)=TOLER(27,VBSURF)
                  VARABL(VBJK+1,11)=TOLER(28,VBSURF)
                  TVBCNT=TVBCNT+1
                  CALL TVARCLN
                  RETURN
              END IF
C     DO BTILTG
              IF(VALT.EQ.162) THEN
                  TOLER(29,VBSURF)=W3
                  TOLER(30,VBSURF)=W2
                  TOLER(31,VBSURF)=GBTILTXP
                  TOLER(32,VBSURF)=GBTILTYP
                  TOLER(33,VBSURF)=GBTILTZP
                  GBTILTXP=0.0D0
                  GBTILTYP=0.0D0
                  GBTILTZP=0.0D0
                  VARABL(VBJK+1,7)=TOLER(30,VBSURF)
                  VARABL(VBJK+1,4)=TOLER(29,VBSURF)
                  VARABL(VBJK+1,5)=TOLER(29,VBSURF)
                  VARABL(VBJK+1,13)=TOLER(29,VBSURF)
                  VARABL(VBJK+1,9)=TOLER(31,VBSURF)
                  VARABL(VBJK+1,10)=TOLER(32,VBSURF)
                  VARABL(VBJK+1,11)=TOLER(33,VBSURF)
                  TVBCNT=TVBCNT+1
                  CALL TVARCLN
                  RETURN
              END IF
C
C     DO ROLLX
              IF(VALT.EQ.163) THEN
                  TOLER(34,VBSURF)=W3
                  TOLER(35,VBSURF)=W2
                  TOLER(36,VBSURF)=W4
                  VARABL(VBJK+1,7)=TOLER(35,VBSURF)
                  VARABL(VBJK+1,4)=TOLER(34,VBSURF)
                  VARABL(VBJK+1,5)=TOLER(34,VBSURF)
                  VARABL(VBJK+1,13)=TOLER(34,VBSURF)
                  VARABL(VBJK+1,12)=TOLER(36,VBSURF)
                  TVBCNT=TVBCNT+1
                  CALL TVARCLN
                  RETURN
              END IF
C     DO ROLLY
              IF(VALT.EQ.164) THEN
                  TOLER(37,VBSURF)=W3
                  TOLER(38,VBSURF)=W2
                  TOLER(39,VBSURF)=W4
                  VARABL(VBJK+1,7)=TOLER(38,VBSURF)
                  VARABL(VBJK+1,4)=TOLER(37,VBSURF)
                  VARABL(VBJK+1,5)=TOLER(37,VBSURF)
                  VARABL(VBJK+1,13)=TOLER(37,VBSURF)
                  VARABL(VBJK+1,12)=TOLER(39,VBSURF)
                  TVBCNT=TVBCNT+1
                  CALL TVARCLN
                  RETURN
              END IF
          ELSE
C     DROP THROUGH AND DO VARIABLES WITH TWO NUMERIC WORDS
              VBJK=TVBCNT+MAXCMP
              VARNAM(VBJK+1)=WC
              VARABL(VBJK+1,1) =DBLE(VALT)
              VARABL(VBJK+1,2) =0.0D0
              VARABL(VBJK+1,3) =DBLE(VBSURF)
              VARABL(VBJK+1,4) =0.0D0
              VARABL(VBJK+1,5) =0.0D0
              VARABL(VBJK+1,6) =0.0D0
              VARABL(VBJK+1,7) =0.0D0
              VARABL(VBJK+1,8) =DELTTA
              VARABL(VBJK+1,9) =0.0D0
              VARABL(VBJK+1,10)=0.0D0
              VARABL(VBJK+1,11)=0.0D0
C     DFDELTT=DF2
              VARABL(VBJK+1,12)=DBLE(DFDELTT)
              VARABL(VBJK+1,13)=0.0D0
              VARABL(VBJK+1,14)=0.0D0
              VARABL(VBJK+1,15)=0.0D0
              VARABL(VBJK+1,16)=0.0D0
              VARABL(VBJK+1,17)=0.0D0
          END IF
C     WE FIRST CHECK TO SEE IF THE VARIABLE IS BEING
C     CONTROLLED BY A SOLVE. IF IT IS, THE ADDITION
C     OF THIS VARIABLE TO THE VARIABLE SUBFIL WILL BE
C     DISALLOWED AND THE COUNTER WILL NOT BE ADVANCED.
C     IF IT IS NOT CONTROLLED BY A SOLVE, THEN PIKUPS WILL BE
C     CHECKED FOLLOWED BY TILT AUTO ASSIGNMENTS CHECKS AS NECESSARY.
C
 666      FORMAT(
     1    'VARIABLE NAME = ',A8,'AT SURFACE # = ',I3)
C
C     FOR CV.RD,CVTOR,RDTOR, CHECK FOR FLAT SURFACES
          IF(VALT.EQ.1.OR.VALT.EQ.2) THEN
              IF(ALENS(1,VBSURF).EQ.0.0D0) THEN
                  WRITE(OUTLYNE,*)
     1            '"RD" AND "CV" TOLERANCE VARIABLES CAN NOT BE USED'
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,*)
     1            'ON PLANO SURFACES. USE "RD_FR" OR "CV_FR"'
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,*)'VARIABLE ASSIGNMENT WAS NOT MADE'
                  CALL SHOWIT(0)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
          IF(VALT.EQ.134) THEN
              IF(ALENS(1,VBSURF).EQ.0.0D0) THEN
                  WRITE(OUTLYNE,*)
     1            '"RD_FR" TOLERANCE VARIABLE CAN NOT BE USED'
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,*)
     1            'ON PLANO SURFACES. USE "CV_FR"'
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,*)'VARIABLE ASSIGNMENT WAS NOT MADE'
                  CALL SHOWIT(0)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
C     FOR CV.RD,CVTOR,RDTOR, CHECK FOR FLAT SURFACES
          IF(VALT.EQ.9.OR.VALT.EQ.10) THEN
              IF(ALENS(24,VBSURF).EQ.0.0D0) THEN
                  WRITE(OUTLYNE,*)
     1            '"RDTOR" AND "CVTOR" TOLERANCE VARIABLES CAN NOT BE USED'
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,*)
     1            'ON SURFACES WITH 0.0 TORIC CURVATURE. USE "RDTFR" OR "CVTFR"'
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,*)'VARIABLE ASSIGNMENT WAS NOT MADE'
                  CALL SHOWIT(0)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
          IF(VALT.EQ.136) THEN
              IF(ALENS(24,VBSURF).EQ.0.0D0) THEN
                  WRITE(OUTLYNE,*)
     1            '"RDTOR" TOLERANCE VARIABLE CAN NOT BE USED'
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,*)
     1            'ON SURFACES WITH 0.0 TORIC CURVATURE. USE "CVTFR"'
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,*)'VARIABLE ASSIGNMENT WAS NOT MADE'
                  CALL SHOWIT(0)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
C     DO CV AND RD OR RD_FR OR CV_FR
          IF(VALT.EQ.2.OR.VALT.EQ.1.OR.VALT.EQ.134.OR.VALT.EQ.135) THEN
C     IF SURFACE IS NON-TORIC WITH A CURVATURE SOLVE OR THE
C     SURFACE IS X-TORIC WITH AN X CURVATURE SOLVE OR THE
C     SURFACE IS Y-TORIC WITH AN Y CURVATURE SOLVE
C     THEN DISALLOW VARIABLE
              IF(ALENS(23,VBSURF).EQ.0.0D0.AND.ALENS(33,VBSURF).GT.1.0D0
     2        .OR.ALENS(23,VBSURF).EQ.1.0D0.AND.
     3        SOLVE(8,VBSURF).GT.0.0D0.OR.
     2        ALENS(23,VBSURF).EQ.2.0D0.AND.
     3        SOLVE(2,VBSURF).GT.0.0D0) THEN
                  WRITE(OUTLYNE,*)
     1            'A CONFLICTING CURVATURE SOLVE EXISTS ON THIS SURFACE'
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,666) WC,INT(W1)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,*)'VARIABLE ASSIGNMENT WAS NOT MADE'
                  CALL SHOWIT(0)
                  CALL MACFAL
                  RETURN
              END IF
C     PIKUP CHECK
C     IF A CV,RD,PRO OR NPRO PIKUPS EXISTS THEN
              IF(PIKUP(1,VBSURF,1).EQ.1.0D0
     1        .OR.PIKUP(1,VBSURF,2).EQ.1.0D0
     1        .OR.PIKUP(1,VBSURF,11).EQ.1.0D0
     1        .OR.PIKUP(1,VBSURF,12).EQ.1.0D0) THEN
C     A CV OR RD PIKUP EXISTS, DISSALLOW THE VARIABLE ASSIGNMENT
                  WRITE(OUTLYNE,*)
     1            'A CONFLICTING SURFACE SHAPE PIKUP EXISTS ON THIS SURFACE'
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,666) WC,INT(W1)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,*)'VARIABLE ASSIGNMENT WAS NOT MADE'
                  CALL SHOWIT(0)
                  CALL MACFAL
                  RETURN
              ELSE
C     NO CONFLICTING PIKUP OR SOLVES EXIST, ASSIGN THE VARIABLE
C
                  IF(ALENS(1,VBSURF).EQ.0.0D0) THEN
                      IF(VALT.EQ.1) VARABL(VBJK+1,4)=0.0D0
                      IF(VALT.EQ.1) VARABL(VBJK+1,5)=0.0D0
                      IF(VALT.EQ.1) VARABL(VBJK+1,13)=0.0D0
                      IF(VALT.EQ.2) VARABL(VBJK+1,4)=0.0D0
                      IF(VALT.EQ.2) VARABL(VBJK+1,5)=0.0D0
                      IF(VALT.EQ.2) VARABL(VBJK+1,13)=0.0D0
                      IF(VALT.EQ.134) VARABL(VBJK+1,4)=0.0D0
                      IF(VALT.EQ.134) VARABL(VBJK+1,5)=0.0D0
                      IF(VALT.EQ.134) VARABL(VBJK+1,13)=0.0D0
                      IF(VALT.EQ.135) VARABL(VBJK+1,4)=0.0D0
                      IF(VALT.EQ.135) VARABL(VBJK+1,5)=0.0D0
                      IF(VALT.EQ.135) VARABL(VBJK+1,13)=0.0D0
                  ELSE
                      IF(VALT.EQ.1) VARABL(VBJK+1,4)=1.0D0/ALENS(1,VBSURF)
                      IF(VALT.EQ.1) VARABL(VBJK+1,5)=1.0D0/ALENS(1,VBSURF)
                      IF(VALT.EQ.1) VARABL(VBJK+1,13)=1.0D0/ALENS(1,VBSURF)
                      IF(VALT.EQ.2) VARABL(VBJK+1,4)=ALENS(1,VBSURF)
                      IF(VALT.EQ.2) VARABL(VBJK+1,5)=ALENS(1,VBSURF)
                      IF(VALT.EQ.2) VARABL(VBJK+1,13)=ALENS(1,VBSURF)
                      IF(VALT.EQ.134) VARABL(VBJK+1,4)=1.0D0/ALENS(1,VBSURF)
                      IF(VALT.EQ.134) VARABL(VBJK+1,5)=1.0D0/ALENS(1,VBSURF)
                      IF(VALT.EQ.134) VARABL(VBJK+1,13)=1.0D0/ALENS(1,VBSURF)
                      IF(VALT.EQ.135) VARABL(VBJK+1,4)=ALENS(1,VBSURF)
                      IF(VALT.EQ.135) VARABL(VBJK+1,5)=ALENS(1,VBSURF)
                      IF(VALT.EQ.135) VARABL(VBJK+1,13)=ALENS(1,VBSURF)
                  END IF
C
                  TVBCNT=TVBCNT+1
              END IF
              CALL TVARCLN
              RETURN
          END IF
C
C     DO TH TVAR
          IF(VALT.EQ.3) THEN
              IF(VBSURF.EQ.NEWIMG) THEN
                  WRITE(OUTLYNE,999) WC
 999              FORMAT(
     1            'VARIABLE NAME = ',A8,'NOT USABLE AT THE IMAGE SURFACE')
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(ALENS(33,VBSURF).EQ.1.0D0.OR.ALENS(33,VBSURF).EQ.3.0D0) THEN
C     TH SOLVE EXISTS, DISALLOW VARIABLE
                  WRITE(OUTLYNE,*)
     1            'A CONFLICTING THICKNESS SOLVE EXISTS ON THIS SURFACE'
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,666) WC,INT(W1)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,*)'VARIABLE ASSIGNMENT WAS NOT MADE'
                  CALL SHOWIT(0)
                  CALL MACFAL
                  RETURN
              END IF
C     PIKUP CHECK
              IF(PIKUP(1,VBSURF,3).EQ.1.0D0.OR.PIKUP(1,VBSURF,32).EQ.1.0D0)
     1         THEN
C     A THICKNESS PIKUP EXISTS, DISSALLOW THE VARIABLE ASSIGNMENT
                  WRITE(OUTLYNE,*)
     1            'A CONFLICTING THICKNESS PIKUP EXISTS ON THIS SURFACE'
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,666) WC,INT(W1)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,*)'VARIABLE ASSIGNMENT WAS NOT MADE'
                  CALL SHOWIT(0)
                  CALL MACFAL
                  RETURN
              ELSE
C     NO TH PIKUP OR SOLVES EXIST, ASSIGN THE VARIABLE
C
                  VARABL(VBJK+1,4)=ALENS(3,VBSURF)
C
                  VARABL(VBJK+1,5)=ALENS(3,VBSURF)
                  VARABL(VBJK+1,13)=ALENS(3,VBSURF)
                  TVBCNT=TVBCNT+1
              END IF
              CALL TVARCLN
              RETURN
          END IF
C     DO CC
          IF(VALT.EQ.4) THEN
C     PIKUP CHECK
C     IF A CC,PRO OR NPRO PIKUPS EXISTS THEN
              IF(PIKUP(1,VBSURF,4).EQ.1.0D0
     1        .OR.PIKUP(1,VBSURF,11).EQ.1.0D0
     1        .OR.PIKUP(1,VBSURF,12).EQ.1.0D0) THEN
C     A CV OR RD PIKUP EXISTS, DISSALLOW THE VARIABLE ASSIGNMENT
                  WRITE(OUTLYNE,*)
     1            'A CONFLICTING SURFACE SHAPE PIKUP EXISTS ON THIS SURFACE'
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,666) WC,INT(W1)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,*)'VARIABLE ASSIGNMENT WAS NOT MADE'
                  CALL SHOWIT(0)
                  CALL MACFAL
                  RETURN
              ELSE
C     NO CONFLICTING PIKUP EXISTS, ASSIGN THE VARIABLE
C
                  IF(VALT.EQ.4) VARABL(VBJK+1,4)=ALENS(2,VBSURF)
C
                  IF(VALT.EQ.4) VARABL(VBJK+1,5)=ALENS(2,VBSURF)
                  IF(VALT.EQ.4) VARABL(VBJK+1,13)=ALENS(2,VBSURF)
                  TVBCNT=TVBCNT+1
              END IF
              CALL TVARCLN
              RETURN
          END IF
          IF(VALT.EQ.5) THEN
              IF(ALENS(8,VBSURF).NE.1.0D0) ALENS(8,VBSURF)=1.0D0
C     PIKUP CHECK
C     IF A AD,PRO OR NPRO PIKUPS EXISTS THEN
              IF(PIKUP(1,VBSURF,5).EQ.1.0D0
     1        .OR.PIKUP(1,VBSURF,11).EQ.1.0D0
     1        .OR.PIKUP(1,VBSURF,12).EQ.1.0D0) THEN
C     A CONFLICTING SHAPE PIKUP EXISTS, DISSALLOW THE VARIABLE ASSIGNMENT
                  WRITE(OUTLYNE,*)
     1            'A CONFLICTING SURFACE SHAPE PIKUP EXISTS ON THIS SURFACE'
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,666) WC,INT(W1)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,*)'VARIABLE ASSIGNMENT WAS NOT MADE'
                  CALL SHOWIT(0)
                  CALL MACFAL
                  RETURN
              ELSE
C     NO CONFLICTING PIKUP EXISTS, ASSIGN THE VARIABLE
C
                  IF(VALT.EQ.5) VARABL(VBJK+1,4)=ALENS(4,VBSURF)
C
                  IF(VALT.EQ.5) VARABL(VBJK+1,5)=ALENS(4,VBSURF)
                  IF(VALT.EQ.5) VARABL(VBJK+1,13)=ALENS(4,VBSURF)
                  TVBCNT=TVBCNT+1
              END IF
              CALL TVARCLN
              RETURN
          END IF
C
          IF(VALT.EQ.6) THEN
              IF(ALENS(8,VBSURF).NE.1.0D0) ALENS(8,VBSURF)=1.0D0
C     PIKUP CHECK
C     IF A AE,PRO OR NPRO PIKUPS EXISTS THEN
              IF(PIKUP(1,VBSURF,6).EQ.1.0D0
     1        .OR.PIKUP(1,VBSURF,11).EQ.1.0D0
     1        .OR.PIKUP(1,VBSURF,12).EQ.1.0D0) THEN
C     A CONFLICTING SHAPE PIKUP EXISTS, DISSALLOW THE VARIABLE ASSIGNMENT
                  WRITE(OUTLYNE,*)
     1            'A CONFLICTING SURFACE SHAPE PIKUP EXISTS ON THIS SURFACE'
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,666) WC,INT(W1)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,*)'VARIABLE ASSIGNMENT WAS NOT MADE'
                  CALL SHOWIT(0)
                  CALL MACFAL
                  RETURN
              ELSE
C     NO CONFLICTING PIKUP EXISTS, ASSIGN THE VARIABLE
C
                  IF(VALT.EQ.6) VARABL(VBJK+1,4)=ALENS(5,VBSURF)
C
                  IF(VALT.EQ.6) VARABL(VBJK+1,5)=ALENS(5,VBSURF)
                  IF(VALT.EQ.6) VARABL(VBJK+1,13)=ALENS(5,VBSURF)
                  TVBCNT=TVBCNT+1
              END IF
              CALL TVARCLN
              RETURN
          END IF
C
          IF(VALT.EQ.7) THEN
              IF(ALENS(8,VBSURF).NE.1.0D0) ALENS(8,VBSURF)=1.0D0
C     PIKUP CHECK
C     IF A AF,PRO OR NPRO PIKUPS EXISTS THEN
              IF(PIKUP(1,VBSURF,7).EQ.1.0D0
     1        .OR.PIKUP(1,VBSURF,11).EQ.1.0D0
     1        .OR.PIKUP(1,VBSURF,12).EQ.1.0D0) THEN
C     A CONFLICTING SHAPE PIKUP EXISTS, DISSALLOW THE VARIABLE ASSIGNMENT
                  WRITE(OUTLYNE,*)
     1            'A CONFLICTING SURFACE SHAPE PIKUP EXISTS ON THIS SURFACE'
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,666) WC,INT(W1)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,*)'VARIABLE ASSIGNMENT WAS NOT MADE'
                  CALL SHOWIT(0)
                  CALL MACFAL
                  RETURN
              ELSE
C     NO CONFLICTING PIKUP EXISTS, ASSIGN THE VARIABLE
C
                  IF(VALT.EQ.7) VARABL(VBJK+1,4)=ALENS(6,VBSURF)
C
                  IF(VALT.EQ.7) VARABL(VBJK+1,5)=ALENS(6,VBSURF)
                  IF(VALT.EQ.7) VARABL(VBJK+1,13)=ALENS(6,VBSURF)
                  TVBCNT=TVBCNT+1
              END IF
              CALL TVARCLN
              RETURN
          END IF
C
          IF(VALT.EQ.8) THEN
              IF(ALENS(8,VBSURF).NE.1.0D0) ALENS(8,VBSURF)=1.0D0
C     PIKUP CHECK
C     IF A AG,PRO OR NPRO PIKUPS EXISTS THEN
              IF(PIKUP(1,VBSURF,8).EQ.1.0D0
     1        .OR.PIKUP(1,VBSURF,11).EQ.1.0D0
     1        .OR.PIKUP(1,VBSURF,12).EQ.1.0D0) THEN
C     A CONFLICTING SHAPE PIKUP EXISTS, DISSALLOW THE VARIABLE ASSIGNMENT
                  WRITE(OUTLYNE,*)
     1            'A CONFLICTING SURFACE SHAPE PIKUP EXISTS ON THIS SURFACE'
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,666) WC,INT(W1)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,*)'VARIABLE ASSIGNMENT WAS NOT MADE'
                  CALL SHOWIT(0)
                  CALL MACFAL
                  RETURN
              ELSE
C     NO CONFLICTING PIKUP EXISTS, ASSIGN THE VARIABLE
C
                  IF(VALT.EQ.8) VARABL(VBJK+1,4)=ALENS(7,VBSURF)
C
                  IF(VALT.EQ.8) VARABL(VBJK+1,5)=ALENS(7,VBSURF)
                  IF(VALT.EQ.8) VARABL(VBJK+1,13)=ALENS(7,VBSURF)
                  TVBCNT=TVBCNT+1
              END IF
              CALL TVARCLN
              RETURN
          END IF
C
C     DO CVTOR OR RDTOR (OR _FR)
          IF(VALT.EQ.10.OR.VALT.EQ.9.OR.VALT.EQ.136.OR.VALT.EQ.137) THEN
C     THE
C     SURFACE IS X-TORIC WITH AN Y CURVATURE SOLVE OR THE
C     SURFACE IS Y-TORIC WITH AN X CURVATURE SOLVE
C     THEN DISALLOW VARIABLE
              IF(ALENS(23,VBSURF).EQ.1.0D0.AND.
     3        SOLVE(2,VBSURF).GT.0.0D0.OR.
     2        ALENS(23,VBSURF).EQ.2.0D0.AND.
     3        SOLVE(8,VBSURF).GT.0.0D0) THEN
                  WRITE(OUTLYNE,*)
     1            'A CONFLICTING CURVATURE SOLVE EXISTS ON THIS SURFACE'
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,666) WC,INT(W1)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,*)'VARIABLE ASSIGNMENT WAS NOT MADE'
                  CALL SHOWIT(0)
                  CALL MACFAL
                  RETURN
              END IF
C     PIKUP CHECK
C     IF A CV,RD,PRO OR NPRO PIKUPS EXISTS THEN
              IF(PIKUP(1,VBSURF,9).EQ.1.0D0
     1        .OR.PIKUP(1,VBSURF,10).EQ.1.0D0
     1        .OR.PIKUP(1,VBSURF,11).EQ.1.0D0
     1        .OR.PIKUP(1,VBSURF,12).EQ.1.0D0) THEN
C     A CV OR RD PIKUP EXISTS, DISSALLOW THE VARIABLE ASSIGNMENT
                  WRITE(OUTLYNE,*)
     1            'A CONFLICTING SURFACE SHAPE PIKUP EXISTS ON THIS SURFACE'
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,666) WC,INT(W1)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,*)'VARIABLE ASSIGNMENT WAS NOT MADE'
                  CALL SHOWIT(0)
                  CALL MACFAL
                  RETURN
              ELSE
C     NO CONFLICTING PIKUP OR SOLVES EXIST, ASSIGN THE VARIABLE
C
                  IF(ALENS(24,VBSURF).EQ.0.0D0) THEN
C
                      IF(VALT.EQ.9) VARABL(VBJK+1,4)=0.0D0
                      IF(VALT.EQ.9) VARABL(VBJK+1,5)=0.0D0
                      IF(VALT.EQ.9) VARABL(VBJK+1,13)=0.0D0
                      IF(VALT.EQ.10) VARABL(VBJK+1,4)=0.0D0
                      IF(VALT.EQ.10) VARABL(VBJK+1,5)=0.0D0
                      IF(VALT.EQ.10) VARABL(VBJK+1,13)=0.0D0
                      IF(VALT.EQ.136) VARABL(VBJK+1,4)=0.0D0
                      IF(VALT.EQ.136) VARABL(VBJK+1,5)=0.0D0
                      IF(VALT.EQ.136) VARABL(VBJK+1,13)=0.0D0
                      IF(VALT.EQ.137) VARABL(VBJK+1,4)=0.0D0
                      IF(VALT.EQ.137) VARABL(VBJK+1,5)=0.0D0
                      IF(VALT.EQ.137) VARABL(VBJK+1,13)=0.0D0
                  ELSE
C
                      IF(VALT.EQ.9) VARABL(VBJK+1,4)=1.0D0/ALENS(24,VBSURF)
                      IF(VALT.EQ.9) VARABL(VBJK+1,5)=1.0D0/ALENS(24,VBSURF)
                      IF(VALT.EQ.9) VARABL(VBJK+1,13)=1.0D0/ALENS(24,VBSURF)
                      IF(VALT.EQ.10) VARABL(VBJK+1,4)=ALENS(24,VBSURF)
                      IF(VALT.EQ.10) VARABL(VBJK+1,5)=ALENS(24,VBSURF)
                      IF(VALT.EQ.10) VARABL(VBJK+1,13)=ALENS(24,VBSURF)
                      IF(VALT.EQ.136) VARABL(VBJK+1,4)=1.0D0/ALENS(24,VBSURF)
                      IF(VALT.EQ.136) VARABL(VBJK+1,5)=1.0D0/ALENS(24,VBSURF)
                      IF(VALT.EQ.136) VARABL(VBJK+1,13)=1.0D0/ALENS(24,VBSURF)
                      IF(VALT.EQ.137) VARABL(VBJK+1,4)=ALENS(24,VBSURF)
                      IF(VALT.EQ.137) VARABL(VBJK+1,5)=ALENS(24,VBSURF)
                      IF(VALT.EQ.137) VARABL(VBJK+1,13)=ALENS(24,VBSURF)
                  END IF
                  TVBCNT=TVBCNT+1
              END IF
              CALL TVARCLN
              RETURN
          END IF
C
C     DO CCTOR
          IF(VALT.EQ.11) THEN
              IF(ALENS(23,VBSURF).EQ.0.0D0) THEN
                  WRITE(OUTLYNE,*)
     1            'THE SPECIFIED SURFACE IS NOT TOROIDAL'
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,*)'VARIABLE ASSIGNMENT WAS NOT MADE'
                  CALL SHOWIT(0)
                  CALL MACFAL
                  RETURN
              END IF
C     PIKUP CHECK
C     IF A CCTOR,PRO OR NPRO PIKUPS EXISTS THEN
              IF(PIKUP(1,VBSURF,21).EQ.1.0D0
     1        .OR.PIKUP(1,VBSURF,11).EQ.1.0D0
     1        .OR.PIKUP(1,VBSURF,12).EQ.1.0D0) THEN
C     A CV OR RD PIKUP EXISTS, DISSALLOW THE VARIABLE ASSIGNMENT
                  WRITE(OUTLYNE,*)
     1            'A CONFLICTING SURFACE SHAPE PIKUP EXISTS ON THIS SURFACE'
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,666) WC,INT(W1)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,*)'VARIABLE ASSIGNMENT WAS NOT MADE'
                  CALL SHOWIT(0)
                  CALL MACFAL
                  RETURN
              ELSE
C     NO CONFLICTING PIKUP EXISTS, ASSIGN THE VARIABLE
C
                  IF(VALT.EQ.11) VARABL(VBJK+1,4)=ALENS(41,VBSURF)
C
                  IF(VALT.EQ.11) VARABL(VBJK+1,5)=ALENS(41,VBSURF)
                  IF(VALT.EQ.11) VARABL(VBJK+1,13)=ALENS(41,VBSURF)
                  TVBCNT=TVBCNT+1
              END IF
              CALL TVARCLN
              RETURN
          END IF
C
C     DO ADTOR
          IF(VALT.EQ.12) THEN
              IF(ALENS(23,VBSURF).EQ.0.0D0) THEN
                  WRITE(OUTLYNE,*)
     1            'THE SPECIFIED SURFACE IS NOT TOROIDAL'
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,*)'VARIABLE ASSIGNMENT WAS NOT MADE'
                  CALL SHOWIT(0)
                  CALL MACFAL
                  RETURN
              END IF
C     PIKUP CHECK
C     IF A ADT,PRO OR NPRO PIKUPS EXISTS THEN
              IF(PIKUP(1,VBSURF,22).EQ.1.0D0
     1        .OR.PIKUP(1,VBSURF,11).EQ.1.0D0
     1        .OR.PIKUP(1,VBSURF,12).EQ.1.0D0) THEN
C     A CV OR RD PIKUP EXISTS, DISSALLOW THE VARIABLE ASSIGNMENT
                  WRITE(OUTLYNE,*)
     1            'A CONFLICTING SURFACE SHAPE PIKUP EXISTS ON THIS SURFACE'
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,666) WC,INT(W1)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,*)'VARIABLE ASSIGNMENT WAS NOT MADE'
                  CALL SHOWIT(0)
                  CALL MACFAL
                  RETURN
              ELSE
C     NO CONFLICTING PIKUP EXISTS, ASSIGN THE VARIABLE
C
                  IF(VALT.EQ.12) VARABL(VBJK+1,4)=ALENS(37,VBSURF)
C
                  IF(VALT.EQ.12) VARABL(VBJK+1,5)=ALENS(37,VBSURF)
                  IF(VALT.EQ.12) VARABL(VBJK+1,13)=ALENS(37,VBSURF)
                  TVBCNT=TVBCNT+1
              END IF
              CALL TVARCLN
              RETURN
          END IF
C
C     DO AETOR
          IF(VALT.EQ.13) THEN
              IF(ALENS(23,VBSURF).EQ.0.0D0) THEN
                  WRITE(OUTLYNE,*)
     1            'THE SPECIFIED SURFACE IS NOT TOROIDAL'
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,*)'VARIABLE ASSIGNMENT WAS NOT MADE'
                  CALL SHOWIT(0)
                  CALL MACFAL
                  RETURN
              END IF
C     PIKUP CHECK
C     IF A AETOR,PRO OR NPRO PIKUPS EXISTS THEN
              IF(PIKUP(1,VBSURF,23).EQ.1.0D0
     1        .OR.PIKUP(1,VBSURF,11).EQ.1.0D0
     1        .OR.PIKUP(1,VBSURF,12).EQ.1.0D0) THEN
C     A CV OR RD PIKUP EXISTS, DISSALLOW THE VARIABLE ASSIGNMENT
                  WRITE(OUTLYNE,*)
     1            'A CONFLICTING SURFACE SHAPE PIKUP EXISTS ON THIS SURFACE'
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,666) WC,INT(W1)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,*)'VARIABLE ASSIGNMENT WAS NOT MADE'
                  CALL SHOWIT(0)
                  CALL MACFAL
                  RETURN
              ELSE
C     NO CONFLICTING PIKUP EXISTS, ASSIGN THE VARIABLE
C
                  IF(VALT.EQ.13) VARABL(VBJK+1,4)=ALENS(38,VBSURF)
C
                  IF(VALT.EQ.13) VARABL(VBJK+1,5)=ALENS(38,VBSURF)
                  IF(VALT.EQ.13) VARABL(VBJK+1,13)=ALENS(38,VBSURF)
                  TVBCNT=TVBCNT+1
              END IF
              CALL TVARCLN
              RETURN
          END IF
C
C     DO AFTOR
          IF(VALT.EQ.14) THEN
              IF(ALENS(23,VBSURF).EQ.0.0D0) THEN
                  WRITE(OUTLYNE,*)
     1            'THE SPECIFIED SURFACE IS NOT TOROIDAL'
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,*)'VARIABLE ASSIGNMENT WAS NOT MADE'
                  CALL SHOWIT(0)
                  CALL MACFAL
                  RETURN
              END IF
C     PIKUP CHECK
C     IF A AFTOR,PRO OR NPRO PIKUPS EXISTS THEN
              IF(PIKUP(1,VBSURF,24).EQ.1.0D0
     1        .OR.PIKUP(1,VBSURF,11).EQ.1.0D0
     1        .OR.PIKUP(1,VBSURF,12).EQ.1.0D0) THEN
C     A CV OR RD PIKUP EXISTS, DISSALLOW THE VARIABLE ASSIGNMENT
                  WRITE(OUTLYNE,*)
     1            'A CONFLICTING SURFACE SHAPE PIKUP EXISTS ON THIS SURFACE'
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,666) WC,INT(W1)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,*)'VARIABLE ASSIGNMENT WAS NOT MADE'
                  CALL SHOWIT(0)
                  CALL MACFAL
                  RETURN
              ELSE
C     NO CONFLICTING PIKUP EXISTS, ASSIGN THE VARIABLE
C
                  IF(VALT.EQ.14) VARABL(VBJK+1,4)=ALENS(39,VBSURF)
C
                  IF(VALT.EQ.14) VARABL(VBJK+1,5)=ALENS(39,VBSURF)
                  IF(VALT.EQ.14) VARABL(VBJK+1,13)=ALENS(39,VBSURF)
                  TVBCNT=TVBCNT+1
              END IF
              CALL TVARCLN
              RETURN
          END IF
C
C     DO AGTOR
          IF(VALT.EQ.15) THEN
              IF(ALENS(23,VBSURF).EQ.0.0D0) THEN
                  WRITE(OUTLYNE,*)
     1            'THE SPECIFIED SURFACE IS NOT TOROIDAL'
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,*)'VARIABLE ASSIGNMENT WAS NOT MADE'
                  CALL SHOWIT(0)
                  CALL MACFAL
                  RETURN
              END IF
C     PIKUP CHECK
C     IF A AGTOR,PRO OR NPRO PIKUPS EXISTS THEN
              IF(PIKUP(1,VBSURF,25).EQ.1.0D0
     1        .OR.PIKUP(1,VBSURF,11).EQ.1.0D0
     1        .OR.PIKUP(1,VBSURF,12).EQ.1.0D0) THEN
C     A CV OR RD PIKUP EXISTS, DISSALLOW THE VARIABLE ASSIGNMENT
                  WRITE(OUTLYNE,*)
     1            'A CONFLICTING SURFACE SHAPE PIKUP EXISTS ON THIS SURFACE'
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,666) WC,INT(W1)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,*)'VARIABLE ASSIGNMENT WAS NOT MADE'
                  CALL SHOWIT(0)
                  CALL MACFAL
                  RETURN
              ELSE
C     NO CONFLICTING PIKUP EXISTS, ASSIGN THE VARIABLE
C
                  IF(VALT.EQ.15) VARABL(VBJK+1,4)=ALENS(40,VBSURF)
C
                  IF(VALT.EQ.15) VARABL(VBJK+1,5)=ALENS(40,VBSURF)
                  IF(VALT.EQ.15) VARABL(VBJK+1,13)=ALENS(40,VBSURF)
                  TVBCNT=TVBCNT+1
              END IF
              CALL TVARCLN
              RETURN
          END IF
C
C     DO GDX
          IF(VALT.EQ.147) THEN
C     PIKUP CHECK
C     IF AN GDX PIKUPS EXISTS THEN
              IF(PIKUP(1,VBSURF,37).EQ.1.0D0) THEN
C     A CONFLICTING PIKUP EXISTS, DISSALLOW THE VARIABLE ASSIGNMENT
                  WRITE(OUTLYNE,*)
     1            'A CONFLICTING GDX PIKUP EXISTS ON THIS SURFACE'
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,666) WC,INT(W1)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,*)'VARIABLE ASSIGNMENT WAS NOT MADE'
                  CALL SHOWIT(0)
                  CALL MACFAL
                  RETURN
              ELSE
C     NO CONFLICTING PIKUP EXISTS, ASSIGN THE VARIABLE
C
                  IF(VALT.EQ.147) VARABL(VBJK+1,4)=ALENS(90,VBSURF)
                  IF(VALT.EQ.147) VARABL(VBJK+1,5)=ALENS(90,VBSURF)
                  IF(VALT.EQ.147) VARABL(VBJK+1,13)=ALENS(90,VBSURF)
                  TVBCNT=TVBCNT+1
              END IF
              CALL TVARCLN
              RETURN
          END IF
C
C     DO GDY
          IF(VALT.EQ.148) THEN
C     PIKUP CHECK
C     IF AN GDY PIKUPS EXISTS THEN
              IF(PIKUP(1,VBSURF,38).EQ.1.0D0) THEN
C     A CONFLICTING PIKUP EXISTS, DISSALLOW THE VARIABLE ASSIGNMENT
                  WRITE(OUTLYNE,*)
     1            'A CONFLICTING GDY PIKUP EXISTS ON THIS SURFACE'
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,666) WC,INT(W1)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,*)'VARIABLE ASSIGNMENT WAS NOT MADE'
                  CALL SHOWIT(0)
                  CALL MACFAL
                  RETURN
              ELSE
C     NO CONFLICTING PIKUP EXISTS, ASSIGN THE VARIABLE
C
                  IF(VALT.EQ.148) VARABL(VBJK+1,4)=ALENS(91,VBSURF)
                  IF(VALT.EQ.148) VARABL(VBJK+1,5)=ALENS(91,VBSURF)
                  IF(VALT.EQ.148) VARABL(VBJK+1,13)=ALENS(91,VBSURF)
                  TVBCNT=TVBCNT+1
              END IF
              CALL TVARCLN
              RETURN
          END IF
C
C     DO GDZ
          IF(VALT.EQ.148) THEN
C     PIKUP CHECK
C     IF AN GDZ PIKUPS EXISTS THEN
              IF(PIKUP(1,VBSURF,38).EQ.1.0D0) THEN
C     A CONFLICTING PIKUP EXISTS, DISSALLOW THE VARIABLE ASSIGNMENT
                  WRITE(OUTLYNE,*)
     1            'A CONFLICTING GDZ PIKUP EXISTS ON THIS SURFACE'
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,666) WC,INT(W1)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,*)'VARIABLE ASSIGNMENT WAS NOT MADE'
                  CALL SHOWIT(0)
                  CALL MACFAL
                  RETURN
              ELSE
C     NO CONFLICTING PIKUP EXISTS, ASSIGN THE VARIABLE
C
                  IF(VALT.EQ.149) VARABL(VBJK+1,4)=ALENS(92,VBSURF)
                  IF(VALT.EQ.149) VARABL(VBJK+1,5)=ALENS(92,VBSURF)
                  IF(VALT.EQ.149) VARABL(VBJK+1,13)=ALENS(92,VBSURF)
                  TVBCNT=TVBCNT+1
              END IF
              CALL TVARCLN
              RETURN
          END IF
C
C     DO GALPHA
          IF(VALT.EQ.150) THEN
C     PIKUP CHECK
C     IF AN GALPHA PIKUPS EXISTS THEN
              IF(PIKUP(1,VBSURF,40).EQ.1.0D0) THEN
C     A CONFLICTING PIKUP EXISTS, DISSALLOW THE VARIABLE ASSIGNMENT
                  WRITE(OUTLYNE,*)
     1            'A CONFLICTING GALPHA PIKUP EXISTS ON THIS SURFACE'
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,666) WC,INT(W1)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,*)'VARIABLE ASSIGNMENT WAS NOT MADE'
                  CALL SHOWIT(0)
                  CALL MACFAL
                  RETURN
              ELSE
C     NO CONFLICTING PIKUP EXISTS, ASSIGN THE VARIABLE
C
                  IF(VALT.EQ.150) VARABL(VBJK+1,4)=ALENS(93,VBSURF)
                  IF(VALT.EQ.150) VARABL(VBJK+1,5)=ALENS(93,VBSURF)
                  IF(VALT.EQ.150) VARABL(VBJK+1,13)=ALENS(93,VBSURF)
                  TVBCNT=TVBCNT+1
              END IF
              CALL TVARCLN
              RETURN
          END IF
C
C     DO GBETA
          IF(VALT.EQ.151) THEN
C     PIKUP CHECK
C     IF AN GBETA PIKUPS EXISTS THEN
              IF(PIKUP(1,VBSURF,41).EQ.1.0D0) THEN
C     A CONFLICTING PIKUP EXISTS, DISSALLOW THE VARIABLE ASSIGNMENT
                  WRITE(OUTLYNE,*)
     1            'A CONFLICTING GBETA PIKUP EXISTS ON THIS SURFACE'
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,666) WC,INT(W1)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,*)'VARIABLE ASSIGNMENT WAS NOT MADE'
                  CALL SHOWIT(0)
                  CALL MACFAL
                  RETURN
              ELSE
C     NO CONFLICTING PIKUP EXISTS, ASSIGN THE VARIABLE
C
                  IF(VALT.EQ.151) VARABL(VBJK+1,4)=ALENS(94,VBSURF)
                  IF(VALT.EQ.151) VARABL(VBJK+1,5)=ALENS(94,VBSURF)
                  IF(VALT.EQ.151) VARABL(VBJK+1,13)=ALENS(95,VBSURF)
                  TVBCNT=TVBCNT+1
              END IF
              CALL TVARCLN
              RETURN
          END IF
C
C     DO GGAMMA
          IF(VALT.EQ.152) THEN
C     PIKUP CHECK
C     IF AN GGAMMA PIKUPS EXISTS THEN
              IF(PIKUP(1,VBSURF,42).EQ.1.0D0) THEN
C     A CONFLICTING PIKUP EXISTS, DISSALLOW THE VARIABLE ASSIGNMENT
                  WRITE(OUTLYNE,*)
     1            'A CONFLICTING GGAMMA PIKUP EXISTS ON THIS SURFACE'
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,666) WC,INT(W1)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,*)'VARIABLE ASSIGNMENT WAS NOT MADE'
                  CALL SHOWIT(0)
                  CALL MACFAL
                  RETURN
              ELSE
C     NO CONFLICTING PIKUP EXISTS, ASSIGN THE VARIABLE
C
                  IF(VALT.EQ.152) VARABL(VBJK+1,4)=ALENS(95,VBSURF)
                  IF(VALT.EQ.152) VARABL(VBJK+1,5)=ALENS(95,VBSURF)
                  IF(VALT.EQ.152) VARABL(VBJK+1,13)=ALENS(95,VBSURF)
                  TVBCNT=TVBCNT+1
              END IF
              CALL TVARCLN
              RETURN
          END IF
C
C     DO GRS
          IF(VALT.EQ.153) THEN
              IF(ALENS(96,VBSURF).EQ.1.0D0) THEN
              ELSE
                  WRITE(OUTLYNE,*)
     1            'THE SPECIFIED SURFACE REQUIRES A "GRT" DEFINITION'
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,*)'VARIABLE ASSIGNMENT WAS NOT MADE'
                  CALL SHOWIT(0)
                  CALL MACFAL
                  RETURN
              END IF
C     PIKUP CHECK
C     IF AN GRT PIKUPS EXISTS THEN
              IF(PIKUP(1,VBSURF,43).EQ.1.0D0) THEN
C     A CONFLICTING PIKUP EXISTS, DISSALLOW THE VARIABLE ASSIGNMENT
                  WRITE(OUTLYNE,*)
     1            'A CONFLICTING GRT PIKUP EXISTS ON THIS SURFACE'
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,666) WC,INT(W1)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,*)'VARIABLE ASSIGNMENT WAS NOT MADE'
                  CALL SHOWIT(0)
                  CALL MACFAL
                  RETURN
              ELSE
C     NO CONFLICTING PIKUP EXISTS, ASSIGN THE VARIABLE
C
                  IF(VALT.EQ.153) VARABL(VBJK+1,4)=ALENS(98,VBSURF)
                  IF(VALT.EQ.153) VARABL(VBJK+1,5)=ALENS(98,VBSURF)
                  IF(VALT.EQ.153) VARABL(VBJK+1,13)=ALENS(98,VBSURF)
                  TVBCNT=TVBCNT+1
              END IF
              CALL TVARCLN
              RETURN
          END IF
C
C     DO STILTA
          IF(VALT.EQ.157) THEN
              TOLER(7,VBSURF)=W2
              TOLER(8,VBSURF)=ASTILTXP
              TOLER(9,VBSURF)=ASTILTYP
              TOLER(10,VBSURF)=ASTILTZP
              ASTILTXP=0.0D0
              ASTILTYP=0.0D0
              ASTILTZP=0.0D0
              VARABL(VBJK+1,4)=TOLER(7,VBSURF)
              VARABL(VBJK+1,5)=TOLER(7,VBSURF)
              VARABL(VBJK+1,13)=TOLER(7,VBSURF)
              VARABL(VBJK+1,9)=TOLER(8,VBSURF)
              VARABL(VBJK+1,10)=TOLER(9,VBSURF)
              VARABL(VBJK+1,11)=TOLER(10,VBSURF)
              TVBCNT=TVBCNT+1
              CALL TVARCLN
              RETURN
          END IF
C
C     DO STILTB
          IF(VALT.EQ.158) THEN
              TOLER(11,VBSURF)=W2
              TOLER(12,VBSURF)=BSTILTXP
              TOLER(13,VBSURF)=BSTILTYP
              TOLER(14,VBSURF)=BSTILTZP
              BSTILTXP=0.0D0
              BSTILTYP=0.0D0
              BSTILTZP=0.0D0
              VARABL(VBJK+1,4)=TOLER(11,VBSURF)
              VARABL(VBJK+1,5)=TOLER(11,VBSURF)
              VARABL(VBJK+1,13)=TOLER(11,VBSURF)
              VARABL(VBJK+1,9)=TOLER(12,VBSURF)
              VARABL(VBJK+1,10)=TOLER(13,VBSURF)
              VARABL(VBJK+1,11)=TOLER(14,VBSURF)
              TVBCNT=TVBCNT+1
              CALL TVARCLN
              RETURN
          END IF
C
C     DO STILTG
          IF(VALT.EQ.159) THEN
              TOLER(15,VBSURF)=W2
              TOLER(16,VBSURF)=GSTILTXP
              TOLER(17,VBSURF)=GSTILTYP
              TOLER(18,VBSURF)=GSTILTZP
              GSTILTXP=0.0D0
              GSTILTYP=0.0D0
              GSTILTZP=0.0D0
              VARABL(VBJK+1,4)=TOLER(15,VBSURF)
              VARABL(VBJK+1,5)=TOLER(15,VBSURF)
              VARABL(VBJK+1,13)=TOLER(15,VBSURF)
              VARABL(VBJK+1,9)=TOLER(16,VBSURF)
              VARABL(VBJK+1,10)=TOLER(17,VBSURF)
              VARABL(VBJK+1,11)=TOLER(18,VBSURF)
              TVBCNT=TVBCNT+1
              CALL TVARCLN
              RETURN
          END IF
C
C     DO ALPHA
          IF(VALT.EQ.16) THEN
              IF(ALENS(25,VBSURF).EQ.0.0D0) THEN
                  WRITE(OUTLYNE,*)
     1            'THE SPECIFIED SURFACE IS NOT TILTED'
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,*)'VARIABLE ASSIGNMENT WAS NOT MADE'
                  CALL SHOWIT(0)
                  CALL MACFAL
                  RETURN
              END IF
              IF(ALENS(25,VBSURF).EQ.2.0D0.OR.ALENS(25,VBSURF).EQ.3.0D0) THEN
                  WRITE(OUTLYNE,*)
     1            'THE SPECIFIED SURFACE HAS AN AUTOMATIC TILT DEFINITION'
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,*)'VARIABLE ASSIGNMENT WAS NOT MADE'
                  CALL SHOWIT(0)
                  CALL MACFAL
                  RETURN
              END IF
C     PIKUP CHECK
C     IF AN ALPHA PIKUPS EXISTS THEN
              IF(PIKUP(1,VBSURF,15).EQ.1.0D0) THEN
C     A CONFLICTING PIKUP EXISTS, DISSALLOW THE VARIABLE ASSIGNMENT
                  WRITE(OUTLYNE,*)
     1            'A CONFLICTING ALPHA PIKUP EXISTS ON THIS SURFACE'
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,666) WC,INT(W1)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,*)'VARIABLE ASSIGNMENT WAS NOT MADE'
                  CALL SHOWIT(0)
                  CALL MACFAL
                  RETURN
              ELSE
C     NO CONFLICTING PIKUP EXISTS, ASSIGN THE VARIABLE
C
                  IF(VALT.EQ.16) VARABL(VBJK+1,4)=ALENS(118,VBSURF)
C
                  IF(VALT.EQ.16) VARABL(VBJK+1,5)=ALENS(118,VBSURF)
                  IF(VALT.EQ.16) VARABL(VBJK+1,13)=ALENS(118,VBSURF)
                  TVBCNT=TVBCNT+1
              END IF
              CALL TVARCLN
              RETURN
          END IF
C
C     DO BETA
          IF(VALT.EQ.17) THEN
              IF(ALENS(25,VBSURF).EQ.0.0D0) THEN
                  WRITE(OUTLYNE,*)
     1            'THE SPECIFIED SURFACE IS NOT TILTED'
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,*)'VARIABLE ASSIGNMENT WAS NOT MADE'
                  CALL SHOWIT(0)
                  CALL MACFAL
                  RETURN
              END IF
              IF(ALENS(25,VBSURF).EQ.2.0D0.OR.ALENS(25,VBSURF).EQ.3.0D0) THEN
                  WRITE(OUTLYNE,*)
     1            'THE SPECIFIED SURFACE HAS AN AUTOMATIC TILT DEFINITION'
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,*)'VARIABLE ASSIGNMENT WAS NOT MADE'
                  CALL SHOWIT(0)
                  CALL MACFAL
                  RETURN
              END IF
C     PIKUP CHECK
C     IF AN BETA PIKUPS EXISTS THEN
              IF(PIKUP(1,VBSURF,16).EQ.1.0D0) THEN
C     A CONFLICTING PIKUP EXISTS, DISSALLOW THE VARIABLE ASSIGNMENT
                  WRITE(OUTLYNE,*)
     1            'A CONFLICTING BETA PIKUP EXISTS ON THIS SURFACE'
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,666) WC,INT(W1)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,*)'VARIABLE ASSIGNMENT WAS NOT MADE'
                  CALL SHOWIT(0)
                  CALL MACFAL
                  RETURN
              ELSE
C     NO CONFLICTING PIKUP EXISTS, ASSIGN THE VARIABLE
C
                  IF(VALT.EQ.17) VARABL(VBJK+1,4)=ALENS(119,VBSURF)
C
                  IF(VALT.EQ.17) VARABL(VBJK+1,5)=ALENS(119,VBSURF)
                  IF(VALT.EQ.17) VARABL(VBJK+1,13)=ALENS(119,VBSURF)
                  TVBCNT=TVBCNT+1
              END IF
              CALL TVARCLN
              RETURN
          END IF
C
C     DO GAMMA
          IF(VALT.EQ.18) THEN
              IF(ALENS(25,VBSURF).EQ.0.0D0) THEN
                  WRITE(OUTLYNE,*)
     1            'THE SPECIFIED SURFACE IS NOT TILTED'
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,*)'VARIABLE ASSIGNMENT WAS NOT MADE'
                  CALL SHOWIT(0)
                  CALL MACFAL
                  RETURN
              END IF
              IF(ALENS(25,VBSURF).EQ.2.0D0.OR.ALENS(25,VBSURF).EQ.3.0D0) THEN
                  WRITE(OUTLYNE,*)
     1            'THE SPECIFIED SURFACE HAS AN AUTOMATIC TILT DEFINITION'
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,*)'VARIABLE ASSIGNMENT WAS NOT MADE'
                  CALL SHOWIT(0)
                  CALL MACFAL
                  RETURN
              END IF
C     PIKUP CHECK
C     IF AN GAMMA PIKUPS EXISTS THEN
              IF(PIKUP(1,VBSURF,17).EQ.1.0D0) THEN
C     A CONFLICTING PIKUP EXISTS, DISSALLOW THE VARIABLE ASSIGNMENT
                  WRITE(OUTLYNE,*)
     1            'A CONFLICTING GAMMA PIKUP EXISTS ON THIS SURFACE'
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,666) WC,INT(W1)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,*)'VARIABLE ASSIGNMENT WAS NOT MADE'
                  CALL SHOWIT(0)
                  CALL MACFAL
                  RETURN
              ELSE
C     NO CONFLICTING PIKUP EXISTS, ASSIGN THE VARIABLE
C
                  IF(VALT.EQ.18) VARABL(VBJK+1,4)=ALENS(120,VBSURF)
C
                  IF(VALT.EQ.18) VARABL(VBJK+1,5)=ALENS(120,VBSURF)
                  IF(VALT.EQ.18) VARABL(VBJK+1,13)=ALENS(120,VBSURF)
                  TVBCNT=TVBCNT+1
              END IF
              CALL TVARCLN
              RETURN
          END IF
C
C     DO XD
          IF(VALT.EQ.19) THEN
C     PIKUP CHECK
C     IF AN XD PIKUPS EXISTS THEN
              IF(ALENS(25,VBSURF).EQ.2.0D0.OR.ALENS(25,VBSURF).EQ.3.0D0) THEN
                  WRITE(OUTLYNE,*)
     1            'THE SPECIFIED SURFACE HAS AN AUTOMATIC TILT DEFINITION'
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,*)'VARIABLE ASSIGNMENT WAS NOT MADE'
                  CALL SHOWIT(0)
                  CALL MACFAL
                  RETURN
              END IF
              IF(PIKUP(1,VBSURF,14).EQ.1.0D0) THEN
C     A CONFLICTING PIKUP EXISTS, DISSALLOW THE VARIABLE ASSIGNMENT
                  WRITE(OUTLYNE,*)
     1            'A CONFLICTING XD PIKUP EXISTS ON THIS SURFACE'
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,666) WC,INT(W1)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,*)'VARIABLE ASSIGNMENT WAS NOT MADE'
                  CALL SHOWIT(0)
                  CALL MACFAL
                  RETURN
              ELSE
C     NO CONFLICTING PIKUP EXISTS, ASSIGN THE VARIABLE
C
                  IF(VALT.EQ.19) VARABL(VBJK+1,4)=ALENS(114,VBSURF)
C
                  IF(VALT.EQ.19) VARABL(VBJK+1,5)=ALENS(114,VBSURF)
                  IF(VALT.EQ.19) VARABL(VBJK+1,13)=ALENS(114,VBSURF)
                  TVBCNT=TVBCNT+1
              END IF
              CALL TVARCLN
              RETURN
          END IF
C
C
C     DO ZD
          IF(VALT.EQ.138) THEN
C     PIKUP CHECK
C     IF AN XD PIKUPS EXISTS THEN
              IF(ALENS(25,VBSURF).EQ.2.0D0.OR.ALENS(25,VBSURF).EQ.3.0D0) THEN
                  WRITE(OUTLYNE,*)
     1            'THE SPECIFIED SURFACE HAS AN AUTOMATIC TILT DEFINITION'
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,*)'VARIABLE ASSIGNMENT WAS NOT MADE'
                  CALL SHOWIT(0)
                  CALL MACFAL
                  RETURN
              END IF
              IF(PIKUP(1,VBSURF,33).EQ.1.0D0) THEN
C     A CONFLICTING PIKUP EXISTS, DISSALLOW THE VARIABLE ASSIGNMENT
                  WRITE(OUTLYNE,*)
     1            'A CONFLICTING ZD PIKUP EXISTS ON THIS SURFACE'
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,666) WC,INT(W1)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,*)'VARIABLE ASSIGNMENT WAS NOT MADE'
                  CALL SHOWIT(0)
                  CALL MACFAL
                  RETURN
              ELSE
C     NO CONFLICTING PIKUP EXISTS, ASSIGN THE VARIABLE
C
                  IF(VALT.EQ.138) VARABL(VBJK+1,4)=ALENS(116,VBSURF)
C
                  IF(VALT.EQ.138) VARABL(VBJK+1,5)=ALENS(116,VBSURF)
                  IF(VALT.EQ.138) VARABL(VBJK+1,13)=ALENS(116,VBSURF)
                  TVBCNT=TVBCNT+1
              END IF
              CALL TVARCLN
              RETURN
          END IF
C
C
C     DO CLPX
          IF(VALT.EQ.145) THEN
              IF(ALENS(127,VBSURF).NE.0.0D0) THEN
                  WRITE(OUTLYNE,*)
     1            'THE SPECIFIED SURFACE HAS MULTIPLE APERTURES ASSIGNED'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'VARIABLE ASSIGNMENT WAS NOT MADE'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(ALENS(9,VBSURF).EQ.0.0D0) THEN
                  WRITE(OUTLYNE,*)
     1            'THE SPECIFIED SURFACE HAS NO CLEAR APERTURE ASSIGNED'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'VARIABLE ASSIGNMENT WAS NOT MADE'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(ALENS(9,VBSURF).EQ.1.0D0) THEN
                  WRITE(OUTLYNE,*)
     1            'THE SPECIFIED SURFACE HAS A CIRCULAR CLEAR APERTURE ASSIGNED'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'"CLPX" VARIABLE ASSIGNMENT WAS NOT MADE'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C     PIKUP CHECK
C     IF A CLAP PIKUPS EXISTS THEN
              IF(PIKUP(1,VBSURF,18).EQ.1.0D0) THEN
C     A CONFLICTING PIKUP EXISTS, DISSALLOW THE VARIABLE ASSIGNMENT
                  WRITE(OUTLYNE,*)
     1            'A CONFLICTING CLAP PIKUP EXISTS ON THIS SURFACE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,666) WC,INT(W1),VBCFG
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'VARIABLE ASSIGNMENT WAS NOT MADE'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              ELSE
C     NO CONFLICTING PIKUP EXISTS, ASSIGN THE VARIABLE
C
                  IF(VALT.EQ.145) VARABL(VBJK+1,4)=ALENS(11,VBSURF)
                  IF(VALT.EQ.145) VARABL(VBJK+1,5)=ALENS(11,VBSURF)
                  IF(VALT.EQ.145) VARABL(VBJK+1,13)=ALENS(11,VBSURF)
                  TVBCNT=TVBCNT+1
              END IF
              CALL TVARCLN
              RETURN
          END IF
C     DO CLPY
          IF(VALT.EQ.146) THEN
              IF(ALENS(127,VBSURF).NE.0.0D0) THEN
                  WRITE(OUTLYNE,*)
     1            'THE SPECIFIED SURFACE HAS MULTIPLE APERTURES ASSIGNED'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'VARIABLE ASSIGNMENT WAS NOT MADE'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(ALENS(9,VBSURF).EQ.0.0D0) THEN
                  WRITE(OUTLYNE,*)
     1            'THE SPECIFIED SURFACE HAS NO CLEAR APERTURE ASSIGNED'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'VARIABLE ASSIGNMENT WAS NOT MADE'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C     PIKUP CHECK
C     IF A CLAP PIKUPS EXISTS THEN
              IF(PIKUP(1,VBSURF,18).EQ.1.0D0) THEN
C     A CONFLICTING PIKUP EXISTS, DISSALLOW THE VARIABLE ASSIGNMENT
                  WRITE(OUTLYNE,*)
     1            'A CONFLICTING CLAP PIKUP EXISTS ON THIS SURFACE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,666) WC,INT(W1),VBCFG
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'VARIABLE ASSIGNMENT WAS NOT MADE'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              ELSE
C     NO CONFLICTING PIKUP EXISTS, ASSIGN THE VARIABLE
C
                  IF(VALT.EQ.146) VARABL(VBJK+1,4)=ALENS(10,VBSURF)
                  IF(VALT.EQ.146) VARABL(VBJK+1,5)=ALENS(10,VBSURF)
                  IF(VALT.EQ.146) VARABL(VBJK+1,13)=ALENS(10,VBSURF)
                  TVBCNT=TVBCNT+1
              END IF
              CALL TVARCLN
              RETURN
          END IF
C
C     DO PIVX
          IF(VALT.EQ.141) THEN
C     PIKUP CHECK
C     IF AN PIVX PIKUPS EXISTS THEN
              IF(ALENS(25,VBSURF).EQ.2.0D0.OR.ALENS(25,VBSURF).EQ.3.0D0) THEN
                  WRITE(OUTLYNE,*)
     1            'THE SPECIFIED SURFACE HAS AN AUTOMATIC TILT DEFINITION'
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,*)'VARIABLE ASSIGNMENT WAS NOT MADE'
                  CALL SHOWIT(0)
                  CALL MACFAL
                  RETURN
              END IF
              IF(PIKUP(1,VBSURF,33).EQ.1.0D0) THEN
C     A CONFLICTING PIKUP EXISTS, DISSALLOW THE VARIABLE ASSIGNMENT
                  WRITE(OUTLYNE,*)
     1            'A CONFLICTING PIVX PIKUP EXISTS ON THIS SURFACE'
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,666) WC,INT(W1)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,*)'VARIABLE ASSIGNMENT WAS NOT MADE'
                  CALL SHOWIT(0)
                  CALL MACFAL
                  RETURN
              ELSE
C     NO CONFLICTING PIKUP EXISTS, ASSIGN THE VARIABLE
C
                  IF(VALT.EQ.141) VARABL(VBJK+1,4)=ALENS(78,VBSURF)
C
                  IF(VALT.EQ.141) VARABL(VBJK+1,5)=ALENS(78,VBSURF)
                  IF(VALT.EQ.141) VARABL(VBJK+1,13)=ALENS(78,VBSURF)
                  TVBCNT=TVBCNT+1
              END IF
              CALL TVARCLN
              RETURN
          END IF
C
C
C
C     DO PIVY
          IF(VALT.EQ.141) THEN
C     PIKUP CHECK
C     IF AN PIVY PIKUPS EXISTS THEN
              IF(ALENS(25,VBSURF).EQ.2.0D0.OR.ALENS(25,VBSURF).EQ.3.0D0) THEN
                  WRITE(OUTLYNE,*)
     1            'THE SPECIFIED SURFACE HAS AN AUTOMATIC TILT DEFINITION'
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,*)'VARIABLE ASSIGNMENT WAS NOT MADE'
                  CALL SHOWIT(0)
                  CALL MACFAL
                  RETURN
              END IF
              IF(PIKUP(1,VBSURF,35).EQ.1.0D0) THEN
C     A CONFLICTING PIKUP EXISTS, DISSALLOW THE VARIABLE ASSIGNMENT
                  WRITE(OUTLYNE,*)
     1            'A CONFLICTING PIVY PIKUP EXISTS ON THIS SURFACE'
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,666) WC,INT(W1)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,*)'VARIABLE ASSIGNMENT WAS NOT MADE'
                  CALL SHOWIT(0)
                  CALL MACFAL
                  RETURN
              ELSE
C     NO CONFLICTING PIKUP EXISTS, ASSIGN THE VARIABLE
C
                  IF(VALT.EQ.142) VARABL(VBJK+1,4)=ALENS(79,VBSURF)
C
                  IF(VALT.EQ.142) VARABL(VBJK+1,5)=ALENS(79,VBSURF)
                  IF(VALT.EQ.142) VARABL(VBJK+1,13)=ALENS(79,VBSURF)
                  TVBCNT=TVBCNT+1
              END IF
              CALL TVARCLN
              RETURN
          END IF
C
C
C
C     DO PIVZ
          IF(VALT.EQ.143) THEN
C     PIKUP CHECK
C     IF AN PIVZ PIKUPS EXISTS THEN
              IF(ALENS(25,VBSURF).EQ.2.0D0.OR.ALENS(25,VBSURF).EQ.3.0D0) THEN
                  WRITE(OUTLYNE,*)
     1            'THE SPECIFIED SURFACE HAS AN AUTOMATIC TILT DEFINITION'
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,*)'VARIABLE ASSIGNMENT WAS NOT MADE'
                  CALL SHOWIT(0)
                  CALL MACFAL
                  RETURN
              END IF
              IF(PIKUP(1,VBSURF,36).EQ.1.0D0) THEN
C     A CONFLICTING PIKUP EXISTS, DISSALLOW THE VARIABLE ASSIGNMENT
                  WRITE(OUTLYNE,*)
     1            'A CONFLICTING PIVZ PIKUP EXISTS ON THIS SURFACE'
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,666) WC,INT(W1)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,*)'VARIABLE ASSIGNMENT WAS NOT MADE'
                  CALL SHOWIT(0)
                  CALL MACFAL
                  RETURN
              ELSE
C     NO CONFLICTING PIKUP EXISTS, ASSIGN THE VARIABLE
C
                  IF(VALT.EQ.143) VARABL(VBJK+1,4)=ALENS(80,VBSURF)
C
                  IF(VALT.EQ.143) VARABL(VBJK+1,5)=ALENS(80,VBSURF)
                  IF(VALT.EQ.143) VARABL(VBJK+1,13)=ALENS(80,VBSURF)
                  TVBCNT=TVBCNT+1
              END IF
              CALL TVARCLN
              RETURN
          END IF
C
C     DO YD
          IF(VALT.EQ.20) THEN
              IF(ALENS(25,VBSURF).EQ.2.0D0.OR.ALENS(25,VBSURF).EQ.3.0D0) THEN
                  WRITE(OUTLYNE,*)
     1            'THE SPECIFIED SURFACE HAS AN AUTOMATIC TILT DEFINITION'
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,*)'VARIABLE ASSIGNMENT WAS NOT MADE'
                  CALL SHOWIT(0)
                  CALL MACFAL
                  RETURN
              END IF
C     PIKUP CHECK
C     IF AN YD PIKUPS EXISTS THEN
              IF(PIKUP(1,VBSURF,13).EQ.1.0D0) THEN
C     A CONFLICTING PIKUP EXISTS, DISSALLOW THE VARIABLE ASSIGNMENT
                  WRITE(OUTLYNE,*)
     1            'A CONFLICTING YD PIKUP EXISTS ON THIS SURFACE'
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,666) WC,INT(W1)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,*)'VARIABLE ASSIGNMENT WAS NOT MADE'
                  CALL SHOWIT(0)
                  CALL MACFAL
                  RETURN
              ELSE
C     NO CONFLICTING PIKUP EXISTS, ASSIGN THE VARIABLE
C
                  IF(VALT.EQ.20) VARABL(VBJK+1,4)=ALENS(115,VBSURF)
C
                  IF(VALT.EQ.20) VARABL(VBJK+1,5)=ALENS(115,VBSURF)
                  IF(VALT.EQ.20) VARABL(VBJK+1,13)=ALENS(115,VBSURF)
                  TVBCNT=TVBCNT+1
              END IF
              CALL TVARCLN
              RETURN
          END IF
C
C     DO N1
          IF(VALT.EQ.21) THEN
              IF(GLANAM(VBSURF,1)(1:5).NE.'GLASS') THEN
                  WRITE(OUTLYNE,*)
     1            'GLASS MUST BE CONVERTED FROM A CATALOG GLASS'
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,*)
     1            'TO A "GLASS" TYPE OF ENTRY BEFORE INDEX CAN BE A VARIABLE'
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,*) 'NO-ACTION TAKEN'
                  CALL SHOWIT(0)
                  CALL MACFAL
                  RETURN
              END IF
C     PIKUP CHECK
C     IF AN PIKUP GLASS EXISTS THEN
              IF(PIKUP(1,VBSURF,20).EQ.1.0D0) THEN
C     A CONFLICTING PIKUP EXISTS, DISSALLOW THE VARIABLE ASSIGNMENT
                  WRITE(OUTLYNE,*)
     1            'A CONFLICTING GLASS PIKUP EXISTS ON THIS SURFACE'
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,666) WC,INT(W1)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,*)'VARIABLE ASSIGNMENT WAS NOT MADE'
                  CALL SHOWIT(0)
                  CALL MACFAL
                  RETURN
              ELSE
C     NO CONFLICTING PIKUP EXISTS, ASSIGN THE VARIABLE
C
                  IF(VALT.EQ.21) VARABL(VBJK+1,4)=ALENS(46,VBSURF)
C
                  IF(VALT.EQ.21) VARABL(VBJK+1,5)=ALENS(46,VBSURF)
                  IF(VALT.EQ.21) VARABL(VBJK+1,13)=ALENS(46,VBSURF)
                  TVBCNT=TVBCNT+1
              END IF
              CALL TVARCLN
              RETURN
          END IF
C
C     DO N2
          IF(VALT.EQ.22) THEN
              IF(GLANAM(VBSURF,1)(1:5).NE.'GLASS') THEN
                  WRITE(OUTLYNE,*)
     1            'GLASS MUST BE CONVERTED FROM A CATALOG GLASS'
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,*)
     1            'TO A "GLASS" TYPE OF ENTRY BEFORE INDEX CAN BE A VARIABLE'
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,*) 'NO-ACTION TAKEN'
                  CALL SHOWIT(0)
                  CALL MACFAL
                  RETURN
              END IF
C     PIKUP CHECK
C     IF AN PIKUP GLASS EXISTS THEN
              IF(PIKUP(1,VBSURF,20).EQ.1.0D0) THEN
C     A CONFLICTING PIKUP EXISTS, DISSALLOW THE VARIABLE ASSIGNMENT
                  WRITE(OUTLYNE,*)
     1            'A CONFLICTING GLASS PIKUP EXISTS ON THIS SURFACE'
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,666) WC,INT(W1)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,*)'VARIABLE ASSIGNMENT WAS NOT MADE'
                  CALL SHOWIT(0)
                  CALL MACFAL
                  RETURN
              ELSE
C     NO CONFLICTING PIKUP EXISTS, ASSIGN THE VARIABLE
C
                  IF(VALT.EQ.22) VARABL(VBJK+1,4)=ALENS(47,VBSURF)
C
                  IF(VALT.EQ.22) VARABL(VBJK+1,5)=ALENS(47,VBSURF)
                  IF(VALT.EQ.22) VARABL(VBJK+1,13)=ALENS(47,VBSURF)
                  TVBCNT=TVBCNT+1
              END IF
              CALL TVARCLN
              RETURN
          END IF
C
C     DO N3
          IF(VALT.EQ.23) THEN
              IF(GLANAM(VBSURF,1)(1:5).NE.'GLASS') THEN
                  WRITE(OUTLYNE,*)
     1            'GLASS MUST BE CONVERTED FROM A CATALOG GLASS'
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,*)
     1            'TO A "GLASS" TYPE OF ENTRY BEFORE INDEX CAN BE A VARIABLE'
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,*) 'NO-ACTION TAKEN'
                  CALL SHOWIT(0)
                  CALL MACFAL
                  RETURN
              END IF
C     PIKUP CHECK
C     IF AN PIKUP GLASS EXISTS THEN
              IF(PIKUP(1,VBSURF,20).EQ.1.0D0) THEN
C     A CONFLICTING PIKUP EXISTS, DISSALLOW THE VARIABLE ASSIGNMENT
                  WRITE(OUTLYNE,*)
     1            'A CONFLICTING GLASS PIKUP EXISTS ON THIS SURFACE'
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,666) WC,INT(W1)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,*)'VARIABLE ASSIGNMENT WAS NOT MADE'
                  CALL SHOWIT(0)
                  CALL MACFAL
                  RETURN
              ELSE
C     NO CONFLICTING PIKUP EXISTS, ASSIGN THE VARIABLE
C
                  IF(VALT.EQ.23) VARABL(VBJK+1,4)=ALENS(48,VBSURF)
C
                  IF(VALT.EQ.23) VARABL(VBJK+1,5)=ALENS(48,VBSURF)
                  IF(VALT.EQ.23) VARABL(VBJK+1,13)=ALENS(48,VBSURF)
                  TVBCNT=TVBCNT+1
              END IF
              CALL TVARCLN
              RETURN
          END IF
C
C     DO N4
          IF(VALT.EQ.24) THEN
              IF(GLANAM(VBSURF,1)(1:5).NE.'GLASS') THEN
                  WRITE(OUTLYNE,*)
     1            'GLASS MUST BE CONVERTED FROM A CATALOG GLASS'
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,*)
     1            'TO A "GLASS" TYPE OF ENTRY BEFORE INDEX CAN BE A VARIABLE'
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,*) 'NO-ACTION TAKEN'
                  CALL SHOWIT(0)
                  CALL MACFAL
                  RETURN
              END IF
C     PIKUP CHECK
C     IF AN PIKUP GLASS EXISTS THEN
              IF(PIKUP(1,VBSURF,20).EQ.1.0D0) THEN
C     A CONFLICTING PIKUP EXISTS, DISSALLOW THE VARIABLE ASSIGNMENT
                  WRITE(OUTLYNE,*)
     1            'A CONFLICTING GLASS PIKUP EXISTS ON THIS SURFACE'
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,666) WC,INT(W1)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,*)'VARIABLE ASSIGNMENT WAS NOT MADE'
                  CALL SHOWIT(0)
                  CALL MACFAL
                  RETURN
              ELSE
C     NO CONFLICTING PIKUP EXISTS, ASSIGN THE VARIABLE
C
                  IF(VALT.EQ.24) VARABL(VBJK+1,4)=ALENS(49,VBSURF)
C
                  IF(VALT.EQ.24) VARABL(VBJK+1,5)=ALENS(49,VBSURF)
                  IF(VALT.EQ.24) VARABL(VBJK+1,13)=ALENS(49,VBSURF)
                  TVBCNT=TVBCNT+1
              END IF
              CALL TVARCLN
              RETURN
          END IF
C
C     DO N5
          IF(VALT.EQ.25) THEN
              IF(GLANAM(VBSURF,1)(1:5).NE.'GLASS') THEN
                  WRITE(OUTLYNE,*)
     1            'GLASS MUST BE CONVERTED FROM A CATALOG GLASS'
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,*)
     1            'TO A "GLASS" TYPE OF ENTRY BEFORE INDEX CAN BE A VARIABLE'
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,*) 'NO-ACTION TAKEN'
                  CALL SHOWIT(0)
                  CALL MACFAL
                  RETURN
              END IF
C     PIKUP CHECK
C     IF AN PIKUP GLASS EXISTS THEN
              IF(PIKUP(1,VBSURF,20).EQ.1.0D0) THEN
C     A CONFLICTING PIKUP EXISTS, DISSALLOW THE VARIABLE ASSIGNMENT
                  WRITE(OUTLYNE,*)
     1            'A CONFLICTING GLASS PIKUP EXISTS ON THIS SURFACE'
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,666) WC,INT(W1)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,*)'VARIABLE ASSIGNMENT WAS NOT MADE'
                  CALL SHOWIT(0)
                  CALL MACFAL
                  RETURN
              ELSE
C     NO CONFLICTING PIKUP EXISTS, ASSIGN THE VARIABLE
C
                  IF(VALT.EQ.25) VARABL(VBJK+1,4)=ALENS(50,VBSURF)
C
                  IF(VALT.EQ.25) VARABL(VBJK+1,5)=ALENS(50,VBSURF)
                  IF(VALT.EQ.25) VARABL(VBJK+1,13)=ALENS(50,VBSURF)
                  TVBCNT=TVBCNT+1
              END IF
              CALL TVARCLN
              RETURN
          END IF
C
C     DO N6
          IF(VALT.EQ.124) THEN
              IF(GLANAM(VBSURF,1)(1:5).NE.'GLASS') THEN
                  WRITE(OUTLYNE,*)
     1            'GLASS MUST BE CONVERTED FROM A CATALOG GLASS'
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,*)
     1            'TO A "GLASS" TYPE OF ENTRY BEFORE INDEX CAN BE A VARIABLE'
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,*) 'NO-ACTION TAKEN'
                  CALL SHOWIT(0)
                  CALL MACFAL
                  RETURN
              END IF
C     PIKUP CHECK
C     IF AN PIKUP GLASS EXISTS THEN
              IF(PIKUP(1,VBSURF,20).EQ.1.0D0) THEN
C     A CONFLICTING PIKUP EXISTS, DISSALLOW THE VARIABLE ASSIGNMENT
                  WRITE(OUTLYNE,*)
     1            'A CONFLICTING GLASS PIKUP EXISTS ON THIS SURFACE'
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,666) WC,INT(W1)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,*)'VARIABLE ASSIGNMENT WAS NOT MADE'
                  CALL SHOWIT(0)
                  CALL MACFAL
                  RETURN
              ELSE
C     NO CONFLICTING PIKUP EXISTS, ASSIGN THE VARIABLE
C
                  IF(VALT.EQ.124) VARABL(VBJK+1,4)=ALENS(71,VBSURF)
C
                  IF(VALT.EQ.124) VARABL(VBJK+1,5)=ALENS(71,VBSURF)
                  IF(VALT.EQ.124) VARABL(VBJK+1,13)=ALENS(71,VBSURF)
                  TVBCNT=TVBCNT+1
              END IF
              CALL TVARCLN
              RETURN
          END IF
C     DO N7
          IF(VALT.EQ.125) THEN
              IF(GLANAM(VBSURF,1)(1:5).NE.'GLASS') THEN
                  WRITE(OUTLYNE,*)
     1            'GLASS MUST BE CONVERTED FROM A CATALOG GLASS'
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,*)
     1            'TO A "GLASS" TYPE OF ENTRY BEFORE INDEX CAN BE A VARIABLE'
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,*) 'NO-ACTION TAKEN'
                  CALL SHOWIT(0)
                  CALL MACFAL
                  RETURN
              END IF
C     PIKUP CHECK
C     IF AN PIKUP GLASS EXISTS THEN
              IF(PIKUP(1,VBSURF,20).EQ.1.0D0) THEN
C     A CONFLICTING PIKUP EXISTS, DISSALLOW THE VARIABLE ASSIGNMENT
                  WRITE(OUTLYNE,*)
     1            'A CONFLICTING GLASS PIKUP EXISTS ON THIS SURFACE'
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,666) WC,INT(W1)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,*)'VARIABLE ASSIGNMENT WAS NOT MADE'
                  CALL SHOWIT(0)
                  CALL MACFAL
                  RETURN
              ELSE
C     NO CONFLICTING PIKUP EXISTS, ASSIGN THE VARIABLE
C
                  IF(VALT.EQ.125) VARABL(VBJK+1,4)=ALENS(72,VBSURF)
C
                  IF(VALT.EQ.125) VARABL(VBJK+1,5)=ALENS(72,VBSURF)
                  IF(VALT.EQ.125) VARABL(VBJK+1,13)=ALENS(72,VBSURF)
                  TVBCNT=TVBCNT+1
              END IF
              CALL TVARCLN
              RETURN
          END IF
C     DO N8
          IF(VALT.EQ.126) THEN
              IF(GLANAM(VBSURF,1)(1:5).NE.'GLASS') THEN
                  WRITE(OUTLYNE,*)
     1            'GLASS MUST BE CONVERTED FROM A CATALOG GLASS'
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,*)
     1            'TO A "GLASS" TYPE OF ENTRY BEFORE INDEX CAN BE A VARIABLE'
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,*) 'NO-ACTION TAKEN'
                  CALL SHOWIT(0)
                  CALL MACFAL
                  RETURN
              END IF
C     PIKUP CHECK
C     IF AN PIKUP GLASS EXISTS THEN
              IF(PIKUP(1,VBSURF,20).EQ.1.0D0) THEN
C     A CONFLICTING PIKUP EXISTS, DISSALLOW THE VARIABLE ASSIGNMENT
                  WRITE(OUTLYNE,*)
     1            'A CONFLICTING GLASS PIKUP EXISTS ON THIS SURFACE'
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,666) WC,INT(W1)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,*)'VARIABLE ASSIGNMENT WAS NOT MADE'
                  CALL SHOWIT(0)
                  CALL MACFAL
                  RETURN
              ELSE
C     NO CONFLICTING PIKUP EXISTS, ASSIGN THE VARIABLE
C
                  IF(VALT.EQ.126) VARABL(VBJK+1,4)=ALENS(73,VBSURF)
C
                  IF(VALT.EQ.126) VARABL(VBJK+1,5)=ALENS(73,VBSURF)
                  IF(VALT.EQ.126) VARABL(VBJK+1,13)=ALENS(73,VBSURF)
                  TVBCNT=TVBCNT+1
              END IF
              CALL TVARCLN
              RETURN
          END IF
C     DO N9
          IF(VALT.EQ.127) THEN
              IF(GLANAM(VBSURF,1)(1:5).NE.'GLASS') THEN
                  WRITE(OUTLYNE,*)
     1            'GLASS MUST BE CONVERTED FROM A CATALOG GLASS'
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,*)
     1            'TO A "GLASS" TYPE OF ENTRY BEFORE INDEX CAN BE A VARIABLE'
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,*) 'NO-ACTION TAKEN'
                  CALL SHOWIT(0)
                  CALL MACFAL
                  RETURN
              END IF
C     PIKUP CHECK
C     IF AN PIKUP GLASS EXISTS THEN
              IF(PIKUP(1,VBSURF,20).EQ.1.0D0) THEN
C     A CONFLICTING PIKUP EXISTS, DISSALLOW THE VARIABLE ASSIGNMENT
                  WRITE(OUTLYNE,*)
     1            'A CONFLICTING GLASS PIKUP EXISTS ON THIS SURFACE'
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,666) WC,INT(W1)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,*)'VARIABLE ASSIGNMENT WAS NOT MADE'
                  CALL SHOWIT(0)
                  CALL MACFAL
                  RETURN
              ELSE
C     NO CONFLICTING PIKUP EXISTS, ASSIGN THE VARIABLE
C
                  IF(VALT.EQ.127) VARABL(VBJK+1,4)=ALENS(74,VBSURF)
C
                  IF(VALT.EQ.127) VARABL(VBJK+1,5)=ALENS(74,VBSURF)
                  IF(VALT.EQ.127) VARABL(VBJK+1,13)=ALENS(74,VBSURF)
                  TVBCNT=TVBCNT+1
              END IF
              CALL TVARCLN
              RETURN
          END IF
C     DO N10
          IF(VALT.EQ.128) THEN
              IF(GLANAM(VBSURF,1)(1:5).NE.'GLASS') THEN
                  WRITE(OUTLYNE,*)
     1            'GLASS MUST BE CONVERTED FROM A CATALOG GLASS'
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,*)
     1            'TO A "GLASS" TYPE OF ENTRY BEFORE INDEX CAN BE A VARIABLE'
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,*) 'NO-ACTION TAKEN'
                  CALL SHOWIT(0)
                  CALL MACFAL
                  RETURN
              END IF
C     PIKUP CHECK
C     IF AN PIKUP GLASS EXISTS THEN
              IF(PIKUP(1,VBSURF,20).EQ.1.0D0) THEN
C     A CONFLICTING PIKUP EXISTS, DISSALLOW THE VARIABLE ASSIGNMENT
                  WRITE(OUTLYNE,*)
     1            'A CONFLICTING GLASS PIKUP EXISTS ON THIS SURFACE'
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,666) WC,INT(W1)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,*)'VARIABLE ASSIGNMENT WAS NOT MADE'
                  CALL SHOWIT(0)
                  CALL MACFAL
                  RETURN
              ELSE
C     NO CONFLICTING PIKUP EXISTS, ASSIGN THE VARIABLE
C
                  IF(VALT.EQ.128) VARABL(VBJK+1,4)=ALENS(75,VBSURF)
C
                  IF(VALT.EQ.128) VARABL(VBJK+1,5)=ALENS(75,VBSURF)
                  IF(VALT.EQ.128) VARABL(VBJK+1,13)=ALENS(75,VBSURF)
                  TVBCNT=TVBCNT+1
              END IF
              CALL TVARCLN
              RETURN
          END IF
C
C     DO INDEX
          IF(VALT.EQ.139) THEN
              IF(GLANAM(VBSURF,1)(1:5).NE.'MODEL') THEN
                  WRITE(OUTLYNE,*)
     1            'GLASS MUST BE CONVERTED FROM A CATALOG GLASS'
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,*)
     1            'TO A "MODEL" TYPE OF ENTRY BEFORE INDEX CAN BE A VARIABLE'
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,*) 'NO-ACTION TAKEN'
                  CALL SHOWIT(0)
                  CALL MACFAL
                  RETURN
              END IF
C     PIKUP CHECK
C     IF AN PIKUP GLASS EXISTS THEN
              IF(PIKUP(1,VBSURF,20).EQ.1.0D0) THEN
C     A CONFLICTING PIKUP EXISTS, DISSALLOW THE VARIABLE ASSIGNMENT
                  WRITE(OUTLYNE,*)
     1            'A CONFLICTING GLASS PIKUP EXISTS ON THIS SURFACE'
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,666) WC,INT(W1)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,*)'VARIABLE ASSIGNMENT WAS NOT MADE'
                  CALL SHOWIT(0)
                  CALL MACFAL
                  RETURN
              ELSE
C     NO CONFLICTING PIKUP EXISTS, ASSIGN THE VARIABLE
C
                  IF(VALT.EQ.139) VARABL(VBJK+1,4)=ALENS(86,VBSURF)
C
                  IF(VALT.EQ.139) VARABL(VBJK+1,5)=ALENS(86,VBSURF)
                  IF(VALT.EQ.139) VARABL(VBJK+1,13)=ALENS(86,VBSURF)
                  TVBCNT=TVBCNT+1
              END IF
              CALL TVARCLN
              RETURN
          END IF
C
C     DO VNUM
          IF(VALT.EQ.140) THEN
              IF(GLANAM(VBSURF,1)(1:5).NE.'MODEL') THEN
                  WRITE(OUTLYNE,*)
     1            'GLASS MUST BE CONVERTED FROM A CATALOG GLASS'
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,*)
     1            'TO A "MODEL" TYPE OF ENTRY BEFORE INDEX CAN BE A VARIABLE'
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,*) 'NO-ACTION TAKEN'
                  CALL SHOWIT(0)
                  CALL MACFAL
                  RETURN
              END IF
C     PIKUP CHECK
C     IF AN PIKUP GLASS EXISTS THEN
              IF(PIKUP(1,VBSURF,20).EQ.1.0D0) THEN
C     A CONFLICTING PIKUP EXISTS, DISSALLOW THE VARIABLE ASSIGNMENT
                  WRITE(OUTLYNE,*)
     1            'A CONFLICTING GLASS PIKUP EXISTS ON THIS SURFACE'
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,666) WC,INT(W1)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,*)'VARIABLE ASSIGNMENT WAS NOT MADE'
                  CALL SHOWIT(0)
                  CALL MACFAL
                  RETURN
              ELSE
C     NO CONFLICTING PIKUP EXISTS, ASSIGN THE VARIABLE
C
                  IF(VALT.EQ.140) VARABL(VBJK+1,4)=ALENS(87,VBSURF)
C
                  IF(VALT.EQ.140) VARABL(VBJK+1,5)=ALENS(87,VBSURF)
                  IF(VALT.EQ.140) VARABL(VBJK+1,13)=ALENS(87,VBSURF)
                  TVBCNT=TVBCNT+1
              END IF
              CALL TVARCLN
              RETURN
          END IF
C
C
C     DO DPART
          IF(VALT.EQ.144) THEN
              IF(GLANAM(VBSURF,1)(1:5).NE.'MODEL') THEN
                  WRITE(OUTLYNE,*)
     1            'GLASS MUST BE CONVERTED FROM A CATALOG GLASS'
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,*)
     1            'TO A "MODEL" TYPE OF ENTRY BEFORE INDEX CAN BE A VARIABLE'
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,*) 'NO-ACTION TAKEN'
                  CALL SHOWIT(0)
                  CALL MACFAL
                  RETURN
              END IF
C     PIKUP CHECK
C     IF AN PIKUP GLASS EXISTS THEN
              IF(PIKUP(1,VBSURF,20).EQ.1.0D0) THEN
C     A CONFLICTING PIKUP EXISTS, DISSALLOW THE VARIABLE ASSIGNMENT
                  WRITE(OUTLYNE,*)
     1            'A CONFLICTING GLASS PIKUP EXISTS ON THIS SURFACE'
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,666) WC,INT(W1)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,*)'VARIABLE ASSIGNMENT WAS NOT MADE'
                  CALL SHOWIT(0)
                  CALL MACFAL
                  RETURN
              ELSE
C     NO CONFLICTING PIKUP EXISTS, ASSIGN THE VARIABLE
C
                  IF(VALT.EQ.144) VARABL(VBJK+1,4)=ALENS(89,VBSURF)
C
                  IF(VALT.EQ.144) VARABL(VBJK+1,5)=ALENS(89,VBSURF)
                  IF(VALT.EQ.144) VARABL(VBJK+1,13)=ALENS(89,VBSURF)
                  TVBCNT=TVBCNT+1
              END IF
              CALL TVARCLN
              RETURN
          END IF
C
          IF(VALT.EQ.129) THEN
              IF(ALENS(8,VBSURF).NE.1.0D0) ALENS(8,VBSURF)=1.0D0
C     PIKUP CHECK
C     IF A AH,PRO OR NPRO PIKUPS EXISTS THEN
              IF(PIKUP(1,VBSURF,27).EQ.1.0D0
     1        .OR.PIKUP(1,VBSURF,11).EQ.1.0D0
     1        .OR.PIKUP(1,VBSURF,12).EQ.1.0D0) THEN
C     A CONFLICTING SHAPE PIKUP EXISTS, DISSALLOW THE VARIABLE ASSIGNMENT
                  WRITE(OUTLYNE,*)
     1            'A CONFLICTING SURFACE SHAPE PIKUP EXISTS ON THIS SURFACE'
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,666) WC,INT(W1)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,*)'VARIABLE ASSIGNMENT WAS NOT MADE'
                  CALL SHOWIT(0)
                  CALL MACFAL
                  RETURN
              ELSE
C     NO CONFLICTING PIKUP EXISTS, ASSIGN THE VARIABLE
C
                  IF(VALT.EQ.129) VARABL(VBJK+1,4)=ALENS(81,VBSURF)
C
                  IF(VALT.EQ.129) VARABL(VBJK+1,5)=ALENS(81,VBSURF)
                  IF(VALT.EQ.129) VARABL(VBJK+1,13)=ALENS(81,VBSURF)
                  TVBCNT=TVBCNT+1
              END IF
              CALL TVARCLN
              RETURN
          END IF
C
          IF(VALT.EQ.130) THEN
              IF(ALENS(8,VBSURF).NE.1.0D0) ALENS(8,VBSURF)=1.0D0
C     PIKUP CHECK
C     IF A AI,PRO OR NPRO PIKUPS EXISTS THEN
              IF(PIKUP(1,VBSURF,28).EQ.1.0D0
     1        .OR.PIKUP(1,VBSURF,11).EQ.1.0D0
     1        .OR.PIKUP(1,VBSURF,12).EQ.1.0D0) THEN
C     A CONFLICTING SHAPE PIKUP EXISTS, DISSALLOW THE VARIABLE ASSIGNMENT
                  WRITE(OUTLYNE,*)
     1            'A CONFLICTING SURFACE SHAPE PIKUP EXISTS ON THIS SURFACE'
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,666) WC,INT(W1)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,*)'VARIABLE ASSIGNMENT WAS NOT MADE'
                  CALL SHOWIT(0)
                  CALL MACFAL
                  RETURN
              ELSE
C     NO CONFLICTING PIKUP EXISTS, ASSIGN THE VARIABLE
C
                  IF(VALT.EQ.130) VARABL(VBJK+1,4)=ALENS(82,VBSURF)
C
                  IF(VALT.EQ.130) VARABL(VBJK+1,5)=ALENS(82,VBSURF)
                  IF(VALT.EQ.130) VARABL(VBJK+1,13)=ALENS(82,VBSURF)
                  TVBCNT=TVBCNT+1
              END IF
              CALL TVARCLN
              RETURN
          END IF
C
          IF(VALT.EQ.131) THEN
              IF(ALENS(8,VBSURF).NE.1.0D0) ALENS(8,VBSURF)=1.0D0
C     PIKUP CHECK
C     IF A AJ,PRO OR NPRO PIKUPS EXISTS THEN
              IF(PIKUP(1,VBSURF,29).EQ.1.0D0
     1        .OR.PIKUP(1,VBSURF,11).EQ.1.0D0
     1        .OR.PIKUP(1,VBSURF,12).EQ.1.0D0) THEN
C     A CONFLICTING SHAPE PIKUP EXISTS, DISSALLOW THE VARIABLE ASSIGNMENT
                  WRITE(OUTLYNE,*)
     1            'A CONFLICTING SURFACE SHAPE PIKUP EXISTS ON THIS SURFACE'
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,666) WC,INT(W1)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,*)'VARIABLE ASSIGNMENT WAS NOT MADE'
                  CALL SHOWIT(0)
                  CALL MACFAL
                  RETURN
              ELSE
C     NO CONFLICTING PIKUP EXISTS, ASSIGN THE VARIABLE
C
                  IF(VALT.EQ.131) VARABL(VBJK+1,4)=ALENS(83,VBSURF)
C
                  IF(VALT.EQ.131) VARABL(VBJK+1,5)=ALENS(83,VBSURF)
                  IF(VALT.EQ.131) VARABL(VBJK+1,13)=ALENS(83,VBSURF)
                  TVBCNT=TVBCNT+1
              END IF
              CALL TVARCLN
              RETURN
          END IF
C
          IF(VALT.EQ.132) THEN
              IF(ALENS(8,VBSURF).NE.1.0D0) ALENS(8,VBSURF)=1.0D0
C     PIKUP CHECK
C     IF A AK,PRO OR NPRO PIKUPS EXISTS THEN
              IF(PIKUP(1,VBSURF,30).EQ.1.0D0
     1        .OR.PIKUP(1,VBSURF,11).EQ.1.0D0
     1        .OR.PIKUP(1,VBSURF,12).EQ.1.0D0) THEN
C     A CONFLICTING SHAPE PIKUP EXISTS, DISSALLOW THE VARIABLE ASSIGNMENT
                  WRITE(OUTLYNE,*)
     1            'A CONFLICTING SURFACE SHAPE PIKUP EXISTS ON THIS SURFACE'
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,666) WC,INT(W1)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,*)'VARIABLE ASSIGNMENT WAS NOT MADE'
                  CALL SHOWIT(0)
                  CALL MACFAL
                  RETURN
              ELSE
C     NO CONFLICTING PIKUP EXISTS, ASSIGN THE VARIABLE
C
                  IF(VALT.EQ.132) VARABL(VBJK+1,4)=ALENS(84,VBSURF)
C
                  IF(VALT.EQ.132) VARABL(VBJK+1,5)=ALENS(84,VBSURF)
                  IF(VALT.EQ.132) VARABL(VBJK+1,13)=ALENS(84,VBSURF)
                  TVBCNT=TVBCNT+1
              END IF
              CALL TVARCLN
              RETURN
          END IF
C
          IF(VALT.EQ.133) THEN
              IF(ALENS(8,VBSURF).NE.1.0D0) ALENS(8,VBSURF)=1.0D0
C     PIKUP CHECK
C     IF A AL,PRO OR NPRO PIKUPS EXISTS THEN
              IF(PIKUP(1,VBSURF,31).EQ.1.0D0
     1        .OR.PIKUP(1,VBSURF,11).EQ.1.0D0
     1        .OR.PIKUP(1,VBSURF,12).EQ.1.0D0) THEN
C     A CONFLICTING SHAPE PIKUP EXISTS, DISSALLOW THE VARIABLE ASSIGNMENT
                  WRITE(OUTLYNE,*)
     1            'A CONFLICTING SURFACE SHAPE PIKUP EXISTS ON THIS SURFACE'
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,666) WC,INT(W1)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,*)'VARIABLE ASSIGNMENT WAS NOT MADE'
                  CALL SHOWIT(0)
                  CALL MACFAL
                  RETURN
              ELSE
C     NO CONFLICTING PIKUP EXISTS, ASSIGN THE VARIABLE
C
                  IF(VALT.EQ.133) VARABL(VBJK+1,4)=ALENS(85,VBSURF)
C
                  IF(VALT.EQ.133) VARABL(VBJK+1,5)=ALENS(85,VBSURF)
                  IF(VALT.EQ.133) VARABL(VBJK+1,13)=ALENS(85,VBSURF)
                  TVBCNT=TVBCNT+1
              END IF
              CALL TVARCLN
              RETURN
          END IF
C
C
C     NOW DO THE COEFFS FOR CFG1
C
          IF(VALT.GE.27.AND.VALT.LE.74.OR.
     1    VALT.GE.76.AND.VALT.LE.123) THEN
              CNOT=.FALSE.
              IF(DABS(ALENS(34,VBSURF)).EQ.1.0D0.OR.
     1        DABS(ALENS(34,VBSURF)).EQ.6.0D0) THEN
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
                      WRITE(OUTLYNE,*)
     1                'SPECIAL SURFACE TYPES 1 AND 6'
                      CALL SHOWIT(0)
                      WRITE(OUTLYNE,*)
     1                'ONLY USE COEFFICIENTS C9 THROUGH C48'
                      CALL SHOWIT(0)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(0)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
              IF(DABS(ALENS(34,VBSURF)).EQ.2.0D0.OR.
     1        DABS(ALENS(34,VBSURF)).EQ.9.0D0) THEN
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
                      WRITE(OUTLYNE,*)
     1                'SPECIAL SURFACE TYPES 2 AND 9'
                      CALL SHOWIT(0)
                      WRITE(OUTLYNE,*)
     1                'ONLY USE COEFFICIENTS C1 THROUGH C30'
                      CALL SHOWIT(0)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(0)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
              IF(DABS(ALENS(34,VBSURF)).EQ.3.0D0.OR.
     1        DABS(ALENS(34,VBSURF)).EQ.10.0D0) THEN
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
                      WRITE(OUTLYNE,*)
     1                'SPECIAL SURFACE TYPES 3 AND 10'
                      CALL SHOWIT(0)
                      WRITE(OUTLYNE,*)
     1                'ONLY USE COEFFICIENTS C1 THROUGH C37'
                      CALL SHOWIT(0)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(0)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
              IF(DABS(ALENS(34,VBSURF)).EQ.4.0D0) THEN
                  IF(WC.NE.'C1'.AND.WC.NE.'C2'.AND.WC.NE.'C3'
     1            .AND.WC.NE.'C4'.AND.WC.NE.'C5'.AND.WC.NE.'C6'
     1            .AND.WC.NE.'C7'.AND.WC.NE.'C8'.AND.WC.NE.'C9'
     1            .AND.WC.NE.'C10'.AND.WC.NE.'C11'.AND.WC.NE.'C12'
     1            .AND.WC.NE.'C13'.AND.WC.NE.'C14'.AND.WC.NE.'C15') THEN
                      WRITE(OUTLYNE,*)
     1                'SPECIAL SURFACE TYPE 4 ONLY USES COEFFICIENTS C1 THROUGH C15'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
              IF(DABS(ALENS(34,VBSURF)).EQ.18.0D0) THEN
                  IF(WC.NE.'C1'.AND.WC.NE.'C2'.AND.WC.NE.'C3'.AND.WC.NE.'C4'
     1            .AND.WC.NE.'C5'.AND.WC.NE.'C6'.AND.WC.NE.'C7'.AND.
     1            WC.NE.'C8'.AND.WC.NE.'C9'.AND.WC.NE.'C10'.AND.
     1            WC.NE.'C11'.AND.WC.NE.'C12'.AND.WC.NE.'C13'.AND.
     1            WC.NE.'C14'.AND.WC.NE.'C15'.AND.WC.NE.'C16'.AND.
     1            WC.NE.'C17'.AND.WC.NE.'C18') THEN
                      WRITE(OUTLYNE,*)
     1                'SPECIAL SURFACE TYPE 18 ONLY USES COEFFICIENTS C1 THROUGH C18'
                      CALL SHOWIT(0)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(0)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
              IF(DABS(ALENS(34,VBSURF)).EQ.7.0D0.OR.
     1        DABS(ALENS(34,VBSURF)).EQ.8.0D0) THEN
                  IF(WC.EQ.'C92') CNOT=.TRUE.
                  IF(WC.EQ.'C93') CNOT=.TRUE.
                  IF(WC.EQ.'C94') CNOT=.TRUE.
                  IF(WC.EQ.'C95') CNOT=.TRUE.
                  IF(WC.EQ.'C96') CNOT=.TRUE.
                  IF(CNOT) THEN
                      WRITE(OUTLYNE,*)
     1                'SPECIAL SURFACE TYPES 7 AND 8'
                      CALL SHOWIT(0)
                      WRITE(OUTLYNE,*)
     1                'ONLY USE COEFFICIENTS C1 THROUGH C91'
                      CALL SHOWIT(0)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(0)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
              IF(DABS(ALENS(34,VBSURF)).EQ.12.0D0) THEN
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
                      WRITE(OUTLYNE,*)
     1                'SPECIAL SURFACE TYPE 12'
                      CALL SHOWIT(0)
                      WRITE(OUTLYNE,*)
     1                'ONLY USES COEFFICIENTS C1 THROUGH C23'
                      CALL SHOWIT(0)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(0)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
          END IF

C     DO C1 TO 96
          IF(VALT.GE.27.AND.VALT.LE.74) THEN
              IF(ALENS(34,VBSURF).EQ.0.0D0) THEN
                  WRITE(OUTLYNE,*)
     1            'THE SURFACE IS NOT DEFINED AS A SPECIAL SURFACE'
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,*)'VARIABLE ASSIGNMENT WAS NOT MADE'
                  CALL SHOWIT(0)
                  CALL MACFAL
                  RETURN
              END IF
C     PIKUP CHECK
C     IF AN PIKUP GLASS EXISTS THEN
              IF(PIKUP(1,VBSURF,11).EQ.1.0D0
     1        .OR.PIKUP(1,VBSURF,12).EQ.1.0D0) THEN
C     A CONFLICTING PIKUP EXISTS, DISSALLOW THE VARIABLE ASSIGNMENT
                  WRITE(OUTLYNE,*)
     1            'A CONFLICTING SURFACE SHAPE PIKUP EXISTS ON THIS SURFACE'
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,666) WC,INT(W1)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,*)'VARIABLE ASSIGNMENT WAS NOT MADE'
                  CALL SHOWIT(0)
                  CALL MACFAL
                  RETURN
              ELSE
C     NO CONFLICTING PIKUP EXISTS, ASSIGN THE VARIABLE
C
                  IF(VALT.EQ.27) VARABL(VBJK+1,4)=FTFL01(1,VBSURF)
                  IF(VALT.EQ.28) VARABL(VBJK+1,4)=FTFL01(2,VBSURF)
                  IF(VALT.EQ.29) VARABL(VBJK+1,4)=FTFL01(3,VBSURF)
                  IF(VALT.EQ.30) VARABL(VBJK+1,4)=FTFL01(4,VBSURF)
                  IF(VALT.EQ.31) VARABL(VBJK+1,4)=FTFL01(5,VBSURF)
                  IF(VALT.EQ.32) VARABL(VBJK+1,4)=FTFL01(6,VBSURF)
                  IF(VALT.EQ.33) VARABL(VBJK+1,4)=FTFL01(7,VBSURF)
                  IF(VALT.EQ.34) VARABL(VBJK+1,4)=FTFL01(8,VBSURF)
                  IF(VALT.EQ.35) VARABL(VBJK+1,4)=FTFL01(9,VBSURF)
                  IF(VALT.EQ.36) VARABL(VBJK+1,4)=FTFL01(10,VBSURF)
                  IF(VALT.EQ.37) VARABL(VBJK+1,4)=FTFL01(11,VBSURF)
                  IF(VALT.EQ.38) VARABL(VBJK+1,4)=FTFL01(12,VBSURF)
                  IF(VALT.EQ.39) VARABL(VBJK+1,4)=FTFL01(13,VBSURF)
                  IF(VALT.EQ.40) VARABL(VBJK+1,4)=FTFL01(14,VBSURF)
                  IF(VALT.EQ.41) VARABL(VBJK+1,4)=FTFL01(15,VBSURF)
                  IF(VALT.EQ.42) VARABL(VBJK+1,4)=FTFL01(16,VBSURF)
                  IF(VALT.EQ.43) VARABL(VBJK+1,4)=FTFL01(17,VBSURF)
                  IF(VALT.EQ.44) VARABL(VBJK+1,4)=FTFL01(18,VBSURF)
                  IF(VALT.EQ.45) VARABL(VBJK+1,4)=FTFL01(19,VBSURF)
                  IF(VALT.EQ.46) VARABL(VBJK+1,4)=FTFL01(20,VBSURF)
                  IF(VALT.EQ.47) VARABL(VBJK+1,4)=FTFL01(21,VBSURF)
                  IF(VALT.EQ.48) VARABL(VBJK+1,4)=FTFL01(22,VBSURF)
                  IF(VALT.EQ.49) VARABL(VBJK+1,4)=FTFL01(23,VBSURF)
                  IF(VALT.EQ.50) VARABL(VBJK+1,4)=FTFL01(24,VBSURF)
                  IF(VALT.EQ.51) VARABL(VBJK+1,4)=FTFL01(25,VBSURF)
                  IF(VALT.EQ.52) VARABL(VBJK+1,4)=FTFL01(26,VBSURF)
                  IF(VALT.EQ.53) VARABL(VBJK+1,4)=FTFL01(27,VBSURF)
                  IF(VALT.EQ.54) VARABL(VBJK+1,4)=FTFL01(28,VBSURF)
                  IF(VALT.EQ.55) VARABL(VBJK+1,4)=FTFL01(29,VBSURF)
                  IF(VALT.EQ.56) VARABL(VBJK+1,4)=FTFL01(30,VBSURF)
                  IF(VALT.EQ.57) VARABL(VBJK+1,4)=FTFL01(31,VBSURF)
                  IF(VALT.EQ.58) VARABL(VBJK+1,4)=FTFL01(32,VBSURF)
                  IF(VALT.EQ.59) VARABL(VBJK+1,4)=FTFL01(33,VBSURF)
                  IF(VALT.EQ.60) VARABL(VBJK+1,4)=FTFL01(34,VBSURF)
                  IF(VALT.EQ.61) VARABL(VBJK+1,4)=FTFL01(35,VBSURF)
                  IF(VALT.EQ.62) VARABL(VBJK+1,4)=FTFL01(36,VBSURF)
                  IF(VALT.EQ.63) VARABL(VBJK+1,4)=FTFL01(37,VBSURF)
                  IF(VALT.EQ.64) VARABL(VBJK+1,4)=FTFL01(38,VBSURF)
                  IF(VALT.EQ.65) VARABL(VBJK+1,4)=FTFL01(39,VBSURF)
                  IF(VALT.EQ.66) VARABL(VBJK+1,4)=FTFL01(40,VBSURF)
                  IF(VALT.EQ.67) VARABL(VBJK+1,4)=FTFL01(41,VBSURF)
                  IF(VALT.EQ.68) VARABL(VBJK+1,4)=FTFL01(42,VBSURF)
                  IF(VALT.EQ.69) VARABL(VBJK+1,4)=FTFL01(43,VBSURF)
                  IF(VALT.EQ.70) VARABL(VBJK+1,4)=FTFL01(44,VBSURF)
                  IF(VALT.EQ.71) VARABL(VBJK+1,4)=FTFL01(45,VBSURF)
                  IF(VALT.EQ.72) VARABL(VBJK+1,4)=FTFL01(46,VBSURF)
                  IF(VALT.EQ.73) VARABL(VBJK+1,4)=FTFL01(47,VBSURF)
                  IF(VALT.EQ.74) VARABL(VBJK+1,4)=FTFL01(48,VBSURF)
                  IF(VALT.GE.76.AND.VALT.LE.123)
     1            VARABL(VBJK+1,4)=FTFL01(VALT-27,VBSURF)
C
                  IF(VALT.EQ.27) VARABL(VBJK+1,5)=FTFL01(1,VBSURF)
                  IF(VALT.EQ.28) VARABL(VBJK+1,5)=FTFL01(2,VBSURF)
                  IF(VALT.EQ.29) VARABL(VBJK+1,5)=FTFL01(3,VBSURF)
                  IF(VALT.EQ.30) VARABL(VBJK+1,5)=FTFL01(4,VBSURF)
                  IF(VALT.EQ.31) VARABL(VBJK+1,5)=FTFL01(5,VBSURF)
                  IF(VALT.EQ.32) VARABL(VBJK+1,5)=FTFL01(6,VBSURF)
                  IF(VALT.EQ.33) VARABL(VBJK+1,5)=FTFL01(7,VBSURF)
                  IF(VALT.EQ.34) VARABL(VBJK+1,5)=FTFL01(8,VBSURF)
                  IF(VALT.EQ.35) VARABL(VBJK+1,5)=FTFL01(9,VBSURF)
                  IF(VALT.EQ.36) VARABL(VBJK+1,5)=FTFL01(10,VBSURF)
                  IF(VALT.EQ.37) VARABL(VBJK+1,5)=FTFL01(11,VBSURF)
                  IF(VALT.EQ.38) VARABL(VBJK+1,5)=FTFL01(12,VBSURF)
                  IF(VALT.EQ.39) VARABL(VBJK+1,5)=FTFL01(13,VBSURF)
                  IF(VALT.EQ.40) VARABL(VBJK+1,5)=FTFL01(14,VBSURF)
                  IF(VALT.EQ.41) VARABL(VBJK+1,5)=FTFL01(15,VBSURF)
                  IF(VALT.EQ.42) VARABL(VBJK+1,5)=FTFL01(16,VBSURF)
                  IF(VALT.EQ.43) VARABL(VBJK+1,5)=FTFL01(17,VBSURF)
                  IF(VALT.EQ.44) VARABL(VBJK+1,5)=FTFL01(18,VBSURF)
                  IF(VALT.EQ.45) VARABL(VBJK+1,5)=FTFL01(19,VBSURF)
                  IF(VALT.EQ.46) VARABL(VBJK+1,5)=FTFL01(20,VBSURF)
                  IF(VALT.EQ.47) VARABL(VBJK+1,5)=FTFL01(21,VBSURF)
                  IF(VALT.EQ.48) VARABL(VBJK+1,5)=FTFL01(22,VBSURF)
                  IF(VALT.EQ.49) VARABL(VBJK+1,5)=FTFL01(23,VBSURF)
                  IF(VALT.EQ.50) VARABL(VBJK+1,5)=FTFL01(24,VBSURF)
                  IF(VALT.EQ.51) VARABL(VBJK+1,5)=FTFL01(25,VBSURF)
                  IF(VALT.EQ.52) VARABL(VBJK+1,5)=FTFL01(26,VBSURF)
                  IF(VALT.EQ.53) VARABL(VBJK+1,5)=FTFL01(27,VBSURF)
                  IF(VALT.EQ.54) VARABL(VBJK+1,5)=FTFL01(28,VBSURF)
                  IF(VALT.EQ.55) VARABL(VBJK+1,5)=FTFL01(29,VBSURF)
                  IF(VALT.EQ.56) VARABL(VBJK+1,5)=FTFL01(30,VBSURF)
                  IF(VALT.EQ.57) VARABL(VBJK+1,5)=FTFL01(31,VBSURF)
                  IF(VALT.EQ.58) VARABL(VBJK+1,5)=FTFL01(32,VBSURF)
                  IF(VALT.EQ.59) VARABL(VBJK+1,5)=FTFL01(33,VBSURF)
                  IF(VALT.EQ.60) VARABL(VBJK+1,5)=FTFL01(34,VBSURF)
                  IF(VALT.EQ.61) VARABL(VBJK+1,5)=FTFL01(35,VBSURF)
                  IF(VALT.EQ.62) VARABL(VBJK+1,5)=FTFL01(36,VBSURF)
                  IF(VALT.EQ.63) VARABL(VBJK+1,5)=FTFL01(37,VBSURF)
                  IF(VALT.EQ.64) VARABL(VBJK+1,5)=FTFL01(38,VBSURF)
                  IF(VALT.EQ.65) VARABL(VBJK+1,5)=FTFL01(39,VBSURF)
                  IF(VALT.EQ.66) VARABL(VBJK+1,5)=FTFL01(40,VBSURF)
                  IF(VALT.EQ.67) VARABL(VBJK+1,5)=FTFL01(41,VBSURF)
                  IF(VALT.EQ.68) VARABL(VBJK+1,5)=FTFL01(42,VBSURF)
                  IF(VALT.EQ.69) VARABL(VBJK+1,5)=FTFL01(43,VBSURF)
                  IF(VALT.EQ.70) VARABL(VBJK+1,5)=FTFL01(44,VBSURF)
                  IF(VALT.EQ.71) VARABL(VBJK+1,5)=FTFL01(45,VBSURF)
                  IF(VALT.EQ.72) VARABL(VBJK+1,5)=FTFL01(46,VBSURF)
                  IF(VALT.EQ.73) VARABL(VBJK+1,5)=FTFL01(47,VBSURF)
                  IF(VALT.EQ.74) VARABL(VBJK+1,5)=FTFL01(48,VBSURF)
                  IF(VALT.GE.76.AND.VALT.LE.123)
     1            VARABL(VBJK+1,5)=FTFL01(VALT-27,VBSURF)
                  IF(VALT.EQ.27) VARABL(VBJK+1,13)=FTFL01(1,VBSURF)
                  IF(VALT.EQ.28) VARABL(VBJK+1,13)=FTFL01(2,VBSURF)
                  IF(VALT.EQ.29) VARABL(VBJK+1,13)=FTFL01(3,VBSURF)
                  IF(VALT.EQ.30) VARABL(VBJK+1,13)=FTFL01(4,VBSURF)
                  IF(VALT.EQ.31) VARABL(VBJK+1,13)=FTFL01(5,VBSURF)
                  IF(VALT.EQ.32) VARABL(VBJK+1,13)=FTFL01(6,VBSURF)
                  IF(VALT.EQ.33) VARABL(VBJK+1,13)=FTFL01(7,VBSURF)
                  IF(VALT.EQ.34) VARABL(VBJK+1,13)=FTFL01(8,VBSURF)
                  IF(VALT.EQ.35) VARABL(VBJK+1,13)=FTFL01(9,VBSURF)
                  IF(VALT.EQ.36) VARABL(VBJK+1,13)=FTFL01(10,VBSURF)
                  IF(VALT.EQ.37) VARABL(VBJK+1,13)=FTFL01(11,VBSURF)
                  IF(VALT.EQ.38) VARABL(VBJK+1,13)=FTFL01(12,VBSURF)
                  IF(VALT.EQ.39) VARABL(VBJK+1,13)=FTFL01(13,VBSURF)
                  IF(VALT.EQ.40) VARABL(VBJK+1,13)=FTFL01(14,VBSURF)
                  IF(VALT.EQ.41) VARABL(VBJK+1,13)=FTFL01(15,VBSURF)
                  IF(VALT.EQ.42) VARABL(VBJK+1,13)=FTFL01(16,VBSURF)
                  IF(VALT.EQ.43) VARABL(VBJK+1,13)=FTFL01(17,VBSURF)
                  IF(VALT.EQ.44) VARABL(VBJK+1,13)=FTFL01(18,VBSURF)
                  IF(VALT.EQ.45) VARABL(VBJK+1,13)=FTFL01(19,VBSURF)
                  IF(VALT.EQ.46) VARABL(VBJK+1,13)=FTFL01(20,VBSURF)
                  IF(VALT.EQ.47) VARABL(VBJK+1,13)=FTFL01(21,VBSURF)
                  IF(VALT.EQ.48) VARABL(VBJK+1,13)=FTFL01(22,VBSURF)
                  IF(VALT.EQ.49) VARABL(VBJK+1,13)=FTFL01(23,VBSURF)
                  IF(VALT.EQ.50) VARABL(VBJK+1,13)=FTFL01(24,VBSURF)
                  IF(VALT.EQ.51) VARABL(VBJK+1,13)=FTFL01(25,VBSURF)
                  IF(VALT.EQ.52) VARABL(VBJK+1,13)=FTFL01(26,VBSURF)
                  IF(VALT.EQ.53) VARABL(VBJK+1,13)=FTFL01(27,VBSURF)
                  IF(VALT.EQ.54) VARABL(VBJK+1,13)=FTFL01(28,VBSURF)
                  IF(VALT.EQ.55) VARABL(VBJK+1,13)=FTFL01(29,VBSURF)
                  IF(VALT.EQ.56) VARABL(VBJK+1,13)=FTFL01(30,VBSURF)
                  IF(VALT.EQ.57) VARABL(VBJK+1,13)=FTFL01(31,VBSURF)
                  IF(VALT.EQ.58) VARABL(VBJK+1,13)=FTFL01(32,VBSURF)
                  IF(VALT.EQ.59) VARABL(VBJK+1,13)=FTFL01(33,VBSURF)
                  IF(VALT.EQ.60) VARABL(VBJK+1,13)=FTFL01(34,VBSURF)
                  IF(VALT.EQ.61) VARABL(VBJK+1,13)=FTFL01(35,VBSURF)
                  IF(VALT.EQ.62) VARABL(VBJK+1,13)=FTFL01(36,VBSURF)
                  IF(VALT.EQ.63) VARABL(VBJK+1,13)=FTFL01(37,VBSURF)
                  IF(VALT.EQ.64) VARABL(VBJK+1,13)=FTFL01(38,VBSURF)
                  IF(VALT.EQ.65) VARABL(VBJK+1,13)=FTFL01(39,VBSURF)
                  IF(VALT.EQ.66) VARABL(VBJK+1,13)=FTFL01(40,VBSURF)
                  IF(VALT.EQ.67) VARABL(VBJK+1,13)=FTFL01(41,VBSURF)
                  IF(VALT.EQ.68) VARABL(VBJK+1,13)=FTFL01(42,VBSURF)
                  IF(VALT.EQ.69) VARABL(VBJK+1,13)=FTFL01(43,VBSURF)
                  IF(VALT.EQ.70) VARABL(VBJK+1,13)=FTFL01(44,VBSURF)
                  IF(VALT.EQ.71) VARABL(VBJK+1,13)=FTFL01(45,VBSURF)
                  IF(VALT.EQ.72) VARABL(VBJK+1,13)=FTFL01(46,VBSURF)
                  IF(VALT.EQ.73) VARABL(VBJK+1,13)=FTFL01(47,VBSURF)
                  IF(VALT.EQ.74) VARABL(VBJK+1,13)=FTFL01(48,VBSURF)
                  IF(VALT.GE.76.AND.VALT.LE.123)
     1            VARABL(VBJK+1,13)=FTFL01(VALT-27,VBSURF)
                  IF(VALT.EQ.27) VARABL(VBJK+1,11)=1.0D0
                  IF(VALT.EQ.28) VARABL(VBJK+1,11)=2.0D0
                  IF(VALT.EQ.29) VARABL(VBJK+1,11)=3.0D0
                  IF(VALT.EQ.30) VARABL(VBJK+1,11)=4.0D0
                  IF(VALT.EQ.31) VARABL(VBJK+1,11)=5.0D0
                  IF(VALT.EQ.32) VARABL(VBJK+1,11)=6.0D0
                  IF(VALT.EQ.33) VARABL(VBJK+1,11)=7.0D0
                  IF(VALT.EQ.34) VARABL(VBJK+1,11)=8.0D0
                  IF(VALT.EQ.35) VARABL(VBJK+1,11)=9.0D0
                  IF(VALT.EQ.36) VARABL(VBJK+1,11)=10.0D0
                  IF(VALT.EQ.37) VARABL(VBJK+1,11)=11.0D0
                  IF(VALT.EQ.38) VARABL(VBJK+1,11)=12.0D0
                  IF(VALT.EQ.39) VARABL(VBJK+1,11)=13.0D0
                  IF(VALT.EQ.40) VARABL(VBJK+1,11)=14.0D0
                  IF(VALT.EQ.41) VARABL(VBJK+1,11)=15.0D0
                  IF(VALT.EQ.42) VARABL(VBJK+1,11)=16.0D0
                  IF(VALT.EQ.43) VARABL(VBJK+1,11)=17.0D0
                  IF(VALT.EQ.44) VARABL(VBJK+1,11)=18.0D0
                  IF(VALT.EQ.45) VARABL(VBJK+1,11)=19.0D0
                  IF(VALT.EQ.46) VARABL(VBJK+1,11)=20.0D0
                  IF(VALT.EQ.47) VARABL(VBJK+1,11)=21.0D0
                  IF(VALT.EQ.48) VARABL(VBJK+1,11)=22.0D0
                  IF(VALT.EQ.49) VARABL(VBJK+1,11)=23.0D0
                  IF(VALT.EQ.50) VARABL(VBJK+1,11)=24.0D0
                  IF(VALT.EQ.51) VARABL(VBJK+1,11)=25.0D0
                  IF(VALT.EQ.52) VARABL(VBJK+1,11)=26.0D0
                  IF(VALT.EQ.53) VARABL(VBJK+1,11)=27.0D0
                  IF(VALT.EQ.54) VARABL(VBJK+1,11)=28.0D0
                  IF(VALT.EQ.55) VARABL(VBJK+1,11)=29.0D0
                  IF(VALT.EQ.56) VARABL(VBJK+1,11)=30.0D0
                  IF(VALT.EQ.57) VARABL(VBJK+1,11)=31.0D0
                  IF(VALT.EQ.58) VARABL(VBJK+1,11)=32.0D0
                  IF(VALT.EQ.59) VARABL(VBJK+1,11)=33.0D0
                  IF(VALT.EQ.60) VARABL(VBJK+1,11)=34.0D0
                  IF(VALT.EQ.61) VARABL(VBJK+1,11)=35.0D0
                  IF(VALT.EQ.62) VARABL(VBJK+1,11)=36.0D0
                  IF(VALT.EQ.63) VARABL(VBJK+1,11)=37.0D0
                  IF(VALT.EQ.64) VARABL(VBJK+1,11)=38.0D0
                  IF(VALT.EQ.65) VARABL(VBJK+1,11)=39.0D0
                  IF(VALT.EQ.66) VARABL(VBJK+1,11)=40.0D0
                  IF(VALT.EQ.67) VARABL(VBJK+1,11)=41.0D0
                  IF(VALT.EQ.68) VARABL(VBJK+1,11)=42.0D0
                  IF(VALT.EQ.69) VARABL(VBJK+1,11)=43.0D0
                  IF(VALT.EQ.70) VARABL(VBJK+1,11)=44.0D0
                  IF(VALT.EQ.71) VARABL(VBJK+1,11)=45.0D0
                  IF(VALT.EQ.72) VARABL(VBJK+1,11)=46.0D0
                  IF(VALT.EQ.73) VARABL(VBJK+1,11)=47.0D0
                  IF(VALT.EQ.74) VARABL(VBJK+1,11)=48.0D0
                  IF(VALT.GE.76.AND.VALT.LE.123)
     1            VARABL(VBJK+1,11)=DBLE(VALT-26)
C
                  TVBCNT=TVBCNT+1
              END IF
              CALL TVARCLN
              RETURN
          END IF
C
C     DO AC
          IF(VALT.EQ.75) THEN
              IF(ALENS(1,VBSURF).NE.0.0D0) THEN
                  WRITE(OUTLYNE,*)
     1            'THE SPECIFIED SURFACE IS NOT PLANO'
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,*)'VARIABLE ASSIGNMENT WAS NOT MADE'
                  CALL SHOWIT(0)
                  CALL MACFAL
                  RETURN
              END IF
              IF(ALENS(8,VBSURF).NE.1.0D0) ALENS(8,VBSURF)=1.0D0
C     PIKUP CHECK
C     IF A AC,PRO OR NPRO PIKUPS EXISTS THEN
              IF(PIKUP(1,VBSURF,26).EQ.1.0D0
     1        .OR.PIKUP(1,VBSURF,11).EQ.1.0D0
     1        .OR.PIKUP(1,VBSURF,12).EQ.1.0D0) THEN
C     A CV OR RD PIKUP EXISTS, DISSALLOW THE VARIABLE ASSIGNMENT
                  WRITE(OUTLYNE,*)
     1            'A CONFLICTING SURFACE SHAPE PIKUP EXISTS ON THIS SURFACE'
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,666) WC,INT(W1)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,*)'VARIABLE ASSIGNMENT WAS NOT MADE'
                  CALL SHOWIT(0)
                  CALL MACFAL
                  RETURN
              ELSE
C     NO CONFLICTING PIKUP EXISTS, ASSIGN THE VARIABLE
C
                  IF(VALT.EQ.75) VARABL(VBJK+1,4)=ALENS(43,VBSURF)
C
                  IF(VALT.EQ.75) VARABL(VBJK+1,5)=ALENS(43,VBSURF)
                  IF(VALT.EQ.75) VARABL(VBJK+1,13)=ALENS(43,VBSURF)
                  TVBCNT=TVBCNT+1
              END IF
              CALL TVARCLN
              RETURN
          END IF
C
          CALL TVARCLN
          RETURN
C       ALL DONE
      END
C SUB TVCHECK.FOR
      SUBROUTINE TVCHECK
C
          IMPLICIT NONE
C
          CHARACTER VNA*8
C
          INTEGER ALLOERR
C
          REAL*8 CFER,JKVAR
C
          DIMENSION JKVAR(:,:)
C
          ALLOCATABLE :: JKVAR
C
          INTEGER TNUM,II,I
C
          INCLUDE 'datsub.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          INCLUDE 'datcfg.inc'
C
          DEALLOCATE(JKVAR,STAT=ALLOERR)
          ALLOCATE(JKVAR(4000,17),STAT=ALLOERR)
C
          TNUM=TVBCNT
C
C     THIS READS THE CURRENT TVAR LIST AND RE-SUBMITS THE COMMANDS
C     TO THE COMMAND PROCESSOR IF THE LENS HAS BEEN MODIFIED
C     WITH AN UPDATE LENS COMMAND OR AN UPDATE CONFIGS COMMAND
C     FROM THE KEYBOARD.
C
C       VARBLL(I,J) WHERE I COUNTS THE NUMBER OF VARIABLE ENTRIES
C       AND J TAKES ON THE FOLLOWING VALUES AND MEANIINGS.
C
          IF(TVBCNT.EQ.0) DEALLOCATE(JKVAR,STAT=ALLOERR)
          IF(TVBCNT.EQ.0) RETURN
C     THERE WERE VARIABLES, PROCEED RE-ISSUING THE VARIABLES COMMANDS
          LASCFG=INT(SYSTEM1(56))
          DO I=1,TVBCNT
              II=I+MAXCMP
              JKVAR(I,1:17)=VARABL(II,1:17)
          END DO

          SAVE_KDP(1)=SAVEINPT(1)
          INPUT='TVAR'
          CALL PROCES
          REST_KDP(1)=RESTINPT(1)
          CFER=1.0D0
          DO I=1,TNUM
              VNA(1:4)='SNIT'
              IF(JKVAR(I,1).EQ.1.0D0) VNA='RD'
              IF(JKVAR(I,1).EQ.2.0D0) VNA='CV'
              IF(JKVAR(I,1).EQ.3.0D0) VNA='TH'
              IF(JKVAR(I,1).EQ.4.0D0) VNA='CC'
              IF(JKVAR(I,1).EQ.5.0D0) VNA='AD'
              IF(JKVAR(I,1).EQ.6.0D0) VNA='AE'
              IF(JKVAR(I,1).EQ.7.0D0) VNA='AF'
              IF(JKVAR(I,1).EQ.8.0D0) VNA='AG'
              IF(JKVAR(I,1).EQ.9.0D0) VNA='RDTOR'
              IF(JKVAR(I,1).EQ.10.0D0) VNA='CVTOR'
              IF(JKVAR(I,1).EQ.11.0D0) VNA='CCTOR'
              IF(JKVAR(I,1).EQ.12.0D0) VNA='ADTOR'
              IF(JKVAR(I,1).EQ.13.0D0) VNA='AETOR'
              IF(JKVAR(I,1).EQ.14.0D0) VNA='AFTOR'
              IF(JKVAR(I,1).EQ.15.0D0) VNA='AGTOR'
              IF(JKVAR(I,1).EQ.16.0D0) VNA='ALPHA'
              IF(JKVAR(I,1).EQ.17.0D0) VNA='BETA'
              IF(JKVAR(I,1).EQ.18.0D0) VNA='GAMMA'
              IF(JKVAR(I,1).EQ.19.0D0) VNA='XD'
              IF(JKVAR(I,1).EQ.20.0D0) VNA='YD'
              IF(JKVAR(I,1).EQ.21.0D0) VNA='N1'
              IF(JKVAR(I,1).EQ.22.0D0) VNA='N2'
              IF(JKVAR(I,1).EQ.23.0D0) VNA='N3'
              IF(JKVAR(I,1).EQ.24.0D0) VNA='N4'
              IF(JKVAR(I,1).EQ.25.0D0) VNA='N5'
              IF(JKVAR(I,1).EQ.27.0D0) VNA='C1'
              IF(JKVAR(I,1).EQ.28.0D0) VNA='C2'
              IF(JKVAR(I,1).EQ.29.0D0) VNA='C3'
              IF(JKVAR(I,1).EQ.30.0D0) VNA='C4'
              IF(JKVAR(I,1).EQ.31.0D0) VNA='C5'
              IF(JKVAR(I,1).EQ.32.0D0) VNA='C6'
              IF(JKVAR(I,1).EQ.33.0D0) VNA='C7'
              IF(JKVAR(I,1).EQ.34.0D0) VNA='C8'
              IF(JKVAR(I,1).EQ.35.0D0) VNA='C9'
              IF(JKVAR(I,1).EQ.36.0D0) VNA='C10'
              IF(JKVAR(I,1).EQ.37.0D0) VNA='C11'
              IF(JKVAR(I,1).EQ.38.0D0) VNA='C12'
              IF(JKVAR(I,1).EQ.39.0D0) VNA='C13'
              IF(JKVAR(I,1).EQ.40.0D0) VNA='C14'
              IF(JKVAR(I,1).EQ.41.0D0) VNA='C15'
              IF(JKVAR(I,1).EQ.42.0D0) VNA='C16'
              IF(JKVAR(I,1).EQ.43.0D0) VNA='C17'
              IF(JKVAR(I,1).EQ.44.0D0) VNA='C18'
              IF(JKVAR(I,1).EQ.45.0D0) VNA='C19'
              IF(JKVAR(I,1).EQ.46.0D0) VNA='C20'
              IF(JKVAR(I,1).EQ.47.0D0) VNA='C21'
              IF(JKVAR(I,1).EQ.48.0D0) VNA='C22'
              IF(JKVAR(I,1).EQ.49.0D0) VNA='C23'
              IF(JKVAR(I,1).EQ.50.0D0) VNA='C24'
              IF(JKVAR(I,1).EQ.51.0D0) VNA='C25'
              IF(JKVAR(I,1).EQ.52.0D0) VNA='C26'
              IF(JKVAR(I,1).EQ.53.0D0) VNA='C27'
              IF(JKVAR(I,1).EQ.54.0D0) VNA='C28'
              IF(JKVAR(I,1).EQ.55.0D0) VNA='C29'
              IF(JKVAR(I,1).EQ.56.0D0) VNA='C30'
              IF(JKVAR(I,1).EQ.57.0D0) VNA='C31'
              IF(JKVAR(I,1).EQ.58.0D0) VNA='C32'
              IF(JKVAR(I,1).EQ.59.0D0) VNA='C33'
              IF(JKVAR(I,1).EQ.60.0D0) VNA='C34'
              IF(JKVAR(I,1).EQ.61.0D0) VNA='C35'
              IF(JKVAR(I,1).EQ.62.0D0) VNA='C36'
              IF(JKVAR(I,1).EQ.63.0D0) VNA='C37'
              IF(JKVAR(I,1).EQ.64.0D0) VNA='C38'
              IF(JKVAR(I,1).EQ.65.0D0) VNA='C39'
              IF(JKVAR(I,1).EQ.66.0D0) VNA='C40'
              IF(JKVAR(I,1).EQ.67.0D0) VNA='C41'
              IF(JKVAR(I,1).EQ.68.0D0) VNA='C42'
              IF(JKVAR(I,1).EQ.69.0D0) VNA='C43'
              IF(JKVAR(I,1).EQ.70.0D0) VNA='C44'
              IF(JKVAR(I,1).EQ.71.0D0) VNA='C45'
              IF(JKVAR(I,1).EQ.72.0D0) VNA='C46'
              IF(JKVAR(I,1).EQ.73.0D0) VNA='C47'
              IF(JKVAR(I,1).EQ.74.0D0) VNA='C48'
              IF(JKVAR(I,1).EQ.75.0D0) VNA='AC'
              IF(JKVAR(I,1).EQ.76.0D0) VNA='C49'
              IF(JKVAR(I,1).EQ.77.0D0) VNA='C50'
              IF(JKVAR(I,1).EQ.78.0D0) VNA='C51'
              IF(JKVAR(I,1).EQ.79.0D0) VNA='C52'
              IF(JKVAR(I,1).EQ.80.0D0) VNA='C53'
              IF(JKVAR(I,1).EQ.81.0D0) VNA='C54'
              IF(JKVAR(I,1).EQ.82.0D0) VNA='C55'
              IF(JKVAR(I,1).EQ.83.0D0) VNA='C56'
              IF(JKVAR(I,1).EQ.84.0D0) VNA='C57'
              IF(JKVAR(I,1).EQ.85.0D0) VNA='C58'
              IF(JKVAR(I,1).EQ.86.0D0) VNA='C59'
              IF(JKVAR(I,1).EQ.87.0D0) VNA='C60'
              IF(JKVAR(I,1).EQ.88.0D0) VNA='C61'
              IF(JKVAR(I,1).EQ.89.0D0) VNA='C62'
              IF(JKVAR(I,1).EQ.90.0D0) VNA='C63'
              IF(JKVAR(I,1).EQ.91.0D0) VNA='C64'
              IF(JKVAR(I,1).EQ.92.0D0) VNA='C65'
              IF(JKVAR(I,1).EQ.93.0D0) VNA='C66'
              IF(JKVAR(I,1).EQ.94.0D0) VNA='C67'
              IF(JKVAR(I,1).EQ.95.0D0) VNA='C68'
              IF(JKVAR(I,1).EQ.96.0D0) VNA='C69'
              IF(JKVAR(I,1).EQ.97.0D0) VNA='C70'
              IF(JKVAR(I,1).EQ.98.0D0) VNA='C71'
              IF(JKVAR(I,1).EQ.99.0D0) VNA='C72'
              IF(JKVAR(I,1).EQ.100.0D0) VNA='C73'
              IF(JKVAR(I,1).EQ.101.0D0) VNA='C74'
              IF(JKVAR(I,1).EQ.102.0D0) VNA='C75'
              IF(JKVAR(I,1).EQ.103.0D0) VNA='C76'
              IF(JKVAR(I,1).EQ.104.0D0) VNA='C77'
              IF(JKVAR(I,1).EQ.105.0D0) VNA='C78'
              IF(JKVAR(I,1).EQ.106.0D0) VNA='C79'
              IF(JKVAR(I,1).EQ.107.0D0) VNA='C80'
              IF(JKVAR(I,1).EQ.108.0D0) VNA='C81'
              IF(JKVAR(I,1).EQ.109.0D0) VNA='C82'
              IF(JKVAR(I,1).EQ.110.0D0) VNA='C83'
              IF(JKVAR(I,1).EQ.111.0D0) VNA='C84'
              IF(JKVAR(I,1).EQ.112.0D0) VNA='C85'
              IF(JKVAR(I,1).EQ.113.0D0) VNA='C86'
              IF(JKVAR(I,1).EQ.114.0D0) VNA='C87'
              IF(JKVAR(I,1).EQ.115.0D0) VNA='C88'
              IF(JKVAR(I,1).EQ.116.0D0) VNA='C89'
              IF(JKVAR(I,1).EQ.117.0D0) VNA='C90'
              IF(JKVAR(I,1).EQ.118.0D0) VNA='C91'
              IF(JKVAR(I,1).EQ.119.0D0) VNA='C92'
              IF(JKVAR(I,1).EQ.120.0D0) VNA='C93'
              IF(JKVAR(I,1).EQ.121.0D0) VNA='C94'
              IF(JKVAR(I,1).EQ.122.0D0) VNA='C95'
              IF(JKVAR(I,1).EQ.123.0D0) VNA='C96'
              IF(JKVAR(I,1).EQ.124.0D0) VNA='N6'
              IF(JKVAR(I,1).EQ.125.0D0) VNA='N7'
              IF(JKVAR(I,1).EQ.126.0D0) VNA='N8'
              IF(JKVAR(I,1).EQ.127.0D0) VNA='N9'
              IF(JKVAR(I,1).EQ.128.0D0) VNA='N10'
              IF(JKVAR(I,1).EQ.129.0D0) VNA='AH'
              IF(JKVAR(I,1).EQ.131.0D0) VNA='AI'
              IF(JKVAR(I,1).EQ.131.0D0) VNA='AJ'
              IF(JKVAR(I,1).EQ.132.0D0) VNA='AK'
              IF(JKVAR(I,1).EQ.133.0D0) VNA='AL'
              IF(JKVAR(I,1).EQ.134.0D0) VNA='RD_FR'
              IF(JKVAR(I,1).EQ.135.0D0) VNA='CV_FR'
              IF(JKVAR(I,1).EQ.136.0D0) VNA='RDTFR'
              IF(JKVAR(I,1).EQ.137.0D0) VNA='CVTFR'
              IF(JKVAR(I,1).EQ.138.0D0) VNA='ZD   '
              IF(JKVAR(I,1).EQ.139.0D0) VNA='INDEX'
              IF(JKVAR(I,1).EQ.140.0D0) VNA='VNUM '
              IF(JKVAR(I,1).EQ.141.0D0) VNA='PIVX '
              IF(JKVAR(I,1).EQ.142.0D0) VNA='PIVY '
              IF(JKVAR(I,1).EQ.143.0D0) VNA='PIVZ '
              IF(JKVAR(I,1).EQ.144.0D0) VNA='DPART'
              IF(JKVAR(I,1).EQ.145.0D0) VNA='CLPX '
              IF(JKVAR(I,1).EQ.146.0D0) VNA='CLPY '
              IF(JKVAR(I,1).EQ.147.0D0) VNA='GDX  '
              IF(JKVAR(I,1).EQ.148.0D0) VNA='GDY  '
              IF(JKVAR(I,1).EQ.149.0D0) VNA='GDZ  '
              IF(JKVAR(I,1).EQ.150.0D0) VNA='GALPHA'
              IF(JKVAR(I,1).EQ.151.0D0) VNA='GBETA'
              IF(JKVAR(I,1).EQ.152.0D0) VNA='GGAMMA'
              IF(JKVAR(I,1).EQ.153.0D0) VNA='GRS'
              IF(JKVAR(I,1).EQ.154.0D0) VNA='DISPX'
              IF(JKVAR(I,1).EQ.155.0D0) VNA='DISPY'
              IF(JKVAR(I,1).EQ.156.0D0) VNA='DISPZ'
              IF(JKVAR(I,1).EQ.157.0D0) VNA='STILTA'
              IF(JKVAR(I,1).EQ.158.0D0) VNA='STILTB'
              IF(JKVAR(I,1).EQ.159.0D0) VNA='STILTG'
              IF(JKVAR(I,1).EQ.160.0D0) VNA='BTILTA'
              IF(JKVAR(I,1).EQ.161.0D0) VNA='BTILTB'
              IF(JKVAR(I,1).EQ.162.0D0) VNA='BTILTG'
              IF(JKVAR(I,1).EQ.163.0D0) VNA='ROLLX'
              IF(JKVAR(I,1).EQ.164.0D0) VNA='ROLLY'
              IF(VNA(1:4).NE.'SNIT') THEN
                  IF(JKVAR(I,1).LE.153.0D0.OR.JKVAR(I,1).GE.157.0D0.AND.
     1            JKVAR(I,1).LE.159) THEN
C     BUILD A LINES AND ISSUE TO CONTRO
                      SAVE_KDP(1)=SAVEINPT(1)
                      WC=VNA
C     ONE SURFACE NUMBER AND A VALUE
                      W1=JKVAR(I,3)
                      W2=JKVAR(I,8)
                      S1=1
                      S2=1
                      S3=0
                      S4=0
                      S5=0
                      DF1=0
                      DF2=0
                      DF3=1
                      DF4=1
                      DF5=1
                      SN=1
                      SQ=0
                      SST=0
                      SSI=0
                      CALL CONTRO
                      REST_KDP(1)=RESTINPT(1)
                  ELSE
                      SAVE_KDP(1)=SAVEINPT(1)
                      WC=VNA
C     TWO SURFACE NUMBERS AND A VALUE
                      W1=JKVAR(I,3)
                      W2=JKVAR(I,7)
                      W3=JKVAR(I,8)
                      S1=1
                      S2=1
                      S3=1
                      S4=0
                      S5=0
                      DF1=0
                      DF2=0
                      DF3=0
                      DF4=1
                      DF5=1
                      SN=1
                      SQ=0
                      SST=0
                      SSI=0
                      CALL CONTRO
                      REST_KDP(1)=RESTINPT(1)
                  END IF
              END IF
          END DO
          SAVE_KDP(1)=SAVEINPT(1)
          INPUT='EOS'
          CALL PROCES
          REST_KDP(1)=RESTINPT(1)
          F51=0
          DEALLOCATE(JKVAR,STAT=ALLOERR)
          RETURN
      END
