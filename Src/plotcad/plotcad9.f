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

C       NINTH FILE OF PLOT/CAD ROUTINES

C ALL INTERACTER/WINTERACTER CALLS HAVE BEEN MOVED TO ISS.FOR
C
      SUBROUTINE FANCD
C
          IMPLICIT NONE
C
C       THIS ROUTINE DOES THE PLOTTING OF THE CHROMATIC DIFFERENCE LEGEND
C     FOR FAN PLOTTING
C
          CHARACTER NNTT1*9,NNTT2*16,NNTT3*10,VALUE1*10,B*140
C
          CHARACTER NNTT4(1:28)*1,NNTT5*28,NNTT6*16,NNTT7*31
C
          INTEGER II,IB,COLPAS,NT1ANG,NT1SIZ,I,J,K
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          INCLUDE 'dathgr.inc'
C
C     SET LETTER SIZE AND ANGLE
          NT1SIZ=1
          NT1ANG=0
          CALL MY_SETCHARASPECT(1.5,1.5)
C
C     DO THE PLOTTING OF THE LEGEND FOR SCD
          COLPAS=COLR2
          CALL MY_COLTYP(COLPAS)
          CALL MY_PLOT(100,385,0,1,-10,10010,-10,10010)
          CALL MY_PLOT(1100,385,1,1,-10,10010,-10,10010)
          NNTT1=' SCD PAIR'
          COLPAS=COLLBL
          CALL MY_COLTYP(COLPAS)
          CALL MY_JUSTSTRING(1200,325,NNTT1,NT1ANG,NT1SIZ,3)
C
C     DO THE PLOTTING OF THE LEGEND FOR PCD
          COLPAS=COLR1
          CALL MY_COLTYP(COLPAS)
          CALL MY_PLOT(100,560,0,0,-10,10010,-10,10010)
          CALL MY_PLOT(1100,560,1,0,-10,10010,-10,10010)
          NNTT1=' PCD PAIR'
          COLPAS=COLLBL
          CALL MY_COLTYP(COLPAS)
          CALL MY_JUSTSTRING(1200,500,NNTT1,NT1ANG,NT1SIZ,3)
C
C     DO THE PLOTTING OF THE SCD DEFINITION
          IF(SYSTEM1(INT(SYSTEM1(9))).GT.99.9D0) THEN
              WRITE(B,180) REAL(SYSTEM1(INT(SYSTEM1(9))))
              READ(B,200) VALUE1
180           FORMAT(G10.4)
200           FORMAT(A10)
              NNTT2=VALUE1
          ELSE
              WRITE(B,181) REAL(SYSTEM1(INT(SYSTEM1(9))))
              READ(B,200) VALUE1
181           FORMAT(F10.5)
              NNTT2=VALUE1
          END IF
C
          IF(SYSTEM1(INT(SYSTEM1(10))).GT.99.9D0) THEN
              WRITE(B,180) REAL(SYSTEM1(INT(SYSTEM1(10))))
              READ(B,200) VALUE1
              NNTT3=VALUE1
          ELSE
              WRITE(B,181) REAL(SYSTEM1(INT(SYSTEM1(10))))
              READ(B,200) VALUE1
              NNTT3=VALUE1
          END IF
          NNTT4=' '
          NNTT5=' '
C
          NNTT4(1)='S'
          NNTT4(2)='C'
          NNTT4(3)='D'
          NNTT4(4)='@'
          NNTT4(5)='('
          J=5
          DO I=1,10
              IF(NNTT2(I:I).NE.' ') THEN
                  J=J+1
                  NNTT4(J)=NNTT2(I:I)
              ELSE
              END IF
          END DO
          J=J+1
          NNTT4(J)='-'
          DO I=1,10
              IF(NNTT3(I:I).NE.' ') THEN
                  J=J+1
                  NNTT4(J)=NNTT3(I:I)
              ELSE
              END IF
          END DO
          J=J+1
          NNTT4(J)=')'
          DO K=1,J
              NNTT5(K:K)=NNTT4(K)
          END DO
          IB=1
          DO II=28,1,-1
              IF(NNTT5(II:II).NE.' ') THEN
                  IB=II
                  GO TO 281
              END IF
          END DO
 281      CONTINUE
          COLPAS=COLLBL
          CALL MY_COLTYP(COLPAS)
          CALL MY_JUSTSTRING(200,735,NNTT5,NT1ANG,NT1SIZ,3)
          NNTT6='MICROMETER'
          COLPAS=COLLBL
          CALL MY_COLTYP(COLPAS)
          CALL MY_JUSTSTRING(2100,735,NNTT6,NT1ANG,NT1SIZ,3)
C
C     DO THE PLOTTING OF THE PCD DEFINITION
          CALL MY_PLOT(200,900,0,0,-10,10010,-10,10010)
          IF(SYSTEM1(INT(SYSTEM1(7))).GT.99.9D0) THEN
              WRITE(B,180) REAL(SYSTEM1(INT(SYSTEM1(7))))
              READ(B,200) VALUE1
              NNTT2=VALUE1
          ELSE
              WRITE(B,181) REAL(SYSTEM1(INT(SYSTEM1(7))))
              READ(B,200) VALUE1
              NNTT2=VALUE1
          END IF
C
          IF(SYSTEM1(INT(SYSTEM1(8))).GT.99.9D0) THEN
              WRITE(B,180) REAL(SYSTEM1(INT(SYSTEM1(8))))
              READ(B,200) VALUE1
              NNTT3=VALUE1
          ELSE
              WRITE(B,181) REAL(SYSTEM1(INT(SYSTEM1(8))))
              READ(B,200) VALUE1
              NNTT3=VALUE1
          END IF
C
          NNTT4(1)='P'
          NNTT4(2)='C'
          NNTT4(3)='D'
          NNTT4(4)='@'
          NNTT4(5)='('
          J=5
          DO I=1,10
              IF(NNTT2(I:I).NE.' ') THEN
                  J=J+1
                  NNTT4(J)=NNTT2(I:I)
              ELSE
              END IF
          END DO
          J=J+1
          NNTT4(J)='-'
          DO I=1,10
              IF(NNTT3(I:I).NE.' ') THEN
                  J=J+1
                  NNTT4(J)=NNTT3(I:I)
              ELSE
              END IF
          END DO
          J=J+1
          NNTT4(J)=')'
          DO K=1,J
              NNTT5(K:K)=NNTT4(K)
          END DO
          IB=1
          DO II=28,1,-1
              IF(NNTT5(II:II).NE.' ') THEN
                  IB=II
                  GO TO 282
              END IF
          END DO
 282      CONTINUE
          COLPAS=COLLBL
          CALL MY_COLTYP(COLPAS)
          CALL MY_JUSTSTRING(200,900,NNTT5,NT1ANG,NT1SIZ,3)
          NNTT6='MICROMETER'
          COLPAS=COLLBL
          CALL MY_COLTYP(COLPAS)
          CALL MY_JUSTSTRING(2100,900,NNTT6,NT1ANG,NT1SIZ,3)


C     DO THE PLOTTING OF THE TITLE
!                CALL MY_PLOT(1125,6800,1,0,-10,10010,-10,10010)
          NNTT7='PRI./SEC. CHROMATIC DIFFERENCES'
          IB=1
          DO II=31,1,-1
              IF(NNTT7(II:II).NE.' ') THEN
                  IB=II
                  GO TO 311
              END IF
          END DO
 311      CONTINUE
          COLPAS=COLLBL
          CALL MY_COLTYP(COLPAS)
          CALL MY_JUSTSTRING(200,1125,NNTT7,NT1ANG,NT1SIZ,3)
C
C     NOW BOX THE LEGENDS OFF
C
C     LIFT PEN, MOVE TO TOP
          COLPAS=COLFRM
          CALL MY_COLTYP(COLPAS)
          CALL MY_PLOT(0,1325,0,0,-10,10010,-10,10010)
C     DROP PEN, DRAW LINE
          CALL MY_PLOT(3000,1325,1,0,-10,10010,-10,10010)
          CALL MY_PLOT(3000,250,1,0,-10,10010,-10,10010)
C     LIFT PEN, RETURN TO 0,0
          CALL MY_PLOT(0,0,0,0,-10,10010,-10,10010)
C
          IF(.NOT.PLEXIS) PLEXIS=.TRUE.
C
          RETURN
      END
C SUB FFRAME.FOR
      SUBROUTINE FFRAME
          USE GLOBALS
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE NOFRAME.FOR. THIS SUBROUTINE CONTROLS
C     THE FRAME COMMAND CONTROLLING FRAME PLOTTING
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
C       CHECK FOR STRING INPUT
          IF(SST.EQ.1.OR.SN.EQ.1) THEN
              OUTLYNE='"FRAME" TAKES NO STRING OR NUMERIC INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(STI.EQ.0) THEN
              IF(WQ.NE.'ON'.AND.WQ.NE.'OFF') THEN
                  OUTLYNE='"ON", "YES","OFF" AND "NO" ARE THE ONLY'
                  CALL SHOWIT(1)
                  OUTLYNE='VALID QUALIFIERS USED WITH "FRAME"'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C     STI=1
          END IF
          IF(STI.EQ.1) THEN
              IF(.NOT.FRAME) WRITE(OUTLYNE,10)
              IF(FRAME) WRITE(OUTLYNE,11)
              CALL SHOWIT(0)
 10           FORMAT('FRAME IS CURRENTLY "OFF"')
 11           FORMAT('FRAME IS CURRENTLY "ON"')
              RETURN
          END IF
          IF(WQ.EQ.'OFF') THEN
              FRAME=.FALSE.
          END IF
          IF(WQ.EQ.'ON') THEN
              FRAME=.TRUE.
          END IF
          RETURN
      END
