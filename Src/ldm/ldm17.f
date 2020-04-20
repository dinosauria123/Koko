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

C       SENENTEENTH FILE FOR LENS DATABASE MANAGER FILES

C SUB LENOUT.FOR
      SUBROUTINE LENOUT
          USE GLOBALS
C
          IMPLICIT NONE
C
C       IT IS WITHIN THIS SUBROUTINE THAT (LENO) COMMANDS
C       ARE HANDELED. LENO IS LENS OUTPUT IN A FORM
C       WHICH COULD BE THE READ BACK INTO THE PROGRAM VIA AN
C       ASCII FILE AND AFTER READING IN, THIS LENS WOULD
C       BECOME THE NEW CURRENT LENS. LENO REVERSE
C
          INTEGER I,SSTOPI
C
          LOGICAL CAL,RDOUT,ISKDP,ISAC,ISCV,ISREVERSE,NOOPT
C
          COMMON/LENOTYPE/ISKDP,ISAC,ISCV,ISREVERSE
C
          CHARACTER AI4*4
C
          INCLUDE 'datcfg.inc'
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datsub.inc'
          ISKDP=.TRUE.
          ISAC=.FALSE.
          ISCV=.FALSE.
          ISREVERSE=.FALSE.
          IF(WQ.EQ.'AC') ISKDP=.FALSE.
          IF(WQ.EQ.'HEX') ISKDP=.FALSE.
          IF(WQ.EQ.'CV') ISKDP=.FALSE.
          IF(WQ.EQ.'REVERSE') ISKDP=.FALSE.
          IF(WQ.EQ.'AC') ISAC=.TRUE.
          IF(WQ.EQ.'HEX') ISAC=.TRUE.
          IF(WQ.EQ.'CV') ISCV=.TRUE.
          IF(WQ.EQ.'REVERSE') ISREVERSE=.TRUE.

C
C*************************************************************
C       THE LENO COMMAND HAS THE FORMS:
C                               LENO
C                               LENO REVERSE
C                               LENO AC
C                               LENO CV
C*************************************************************
C
C               SIMPLE LENO USES SUBROUTINES
C                       LENHD
C                       LENSF
C                       LENED
C                       PIKSLV
C
C
C       LENO TAKES NO STRING OF NUMERIC INPUT
          IF(SST.EQ.1.OR.SN.EQ.1) THEN
              WRITE(OUTLYNE,*)
     1        '"LENO (QUALIFIER)" TAKES NO STRING OR NUMERIC INPUT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(WQ.EQ.'RD') THEN
              WQ='        '
              SQ=0
              RDOUT=.TRUE.
          ELSE
              RDOUT=.FALSE.
          END IF
          IF(WQ.EQ.'NOOPT') THEN
              WQ='        '
              SQ=0
              NOOPT=.TRUE.
          ELSE
              NOOPT=.FALSE.
          END IF
          IF(ISKDP.OR.ISAC) THEN
C       MUST BE SIMPLE LENO OR LENO AC
              IF(ISKDP) CALL LENHD
              IF(ISAC) CALL LENHDAC
              IF(ISKDP) THEN
                  DO I=0,INT(SYSTEM1(20))
                      CALL LENSF(I,RDOUT)
                  END DO
              END IF
              IF(ISAC) THEN
                  DO I=0,INT(SYSTEM1(20))
                      CALL LENSFAC(I)
                  END DO
              END IF
C
C
              IF(ISAC) CALL LENEDAC
              IF(ISKDP) CALL LENED
C       SHOULD WE CALL PIKSLV AT ALL?
              IF(ISKDP) THEN
C     PLANE LENO ONLY
                  CAL=.FALSE.
                  DO I=0,INT(SYSTEM1(20))
                      IF(ALENS(32,I).NE.0.0D0.OR.ALENS(33,I).NE.0.0D0) CAL=.TRUE.
                  END DO
                  IF(CAL) THEN
C       YES GO TO THE U L CYCLE
                      WRITE(OUTLYNE,10)
                      CALL SHOWIT(10)
 10                   FORMAT('U        L')
                      DO I=0,INT(SYSTEM1(20))
                          IF(ALENS(32,I).NE.0.0D0.OR.ALENS(33,I).NE.0.0D0) THEN
C       SOLVES AND/OR PIKUPS EXITS, CALL PIKSLV
                              WRITE(OUTLYNE,11) DBLE(I)
                              CALL SHOWIT(10)
 11                           FORMAT('CHG     ,',G23.15)
                              CALL PIKSLV(I)
                          END IF
                      END DO
                      WRITE(OUTLYNE,15)
                      CALL SHOWIT(10)
 15                   FORMAT('EOS')
                  END IF
              END IF
              IF(OUT.NE.6.AND.OUT.NE.7) THEN
                  IF(.NOT.NOOPT) THEN
C     TOL AND OPIMIZATION DUMPING IF NEEDED
                      IF(ISKDP) THEN
 100                      FORMAT(A4,1X,D15.8,1X,D15.8,1X,D15.8,1X,I2)
                          IF(RAYY(501).EQ.0.0D0.AND.RAYX(501).EQ.0.0D0)  SSTOPI=500
                          IF(RAYY(501).NE.0.0D0.OR.RAYX(501).NE.0.0D0)   SSTOPI=1000
                          IF(RAYY(1001).NE.0.0D0.OR.RAYX(1001).NE.0.0D0) SSTOPI=1500
                          IF(RAYY(1501).NE.0.0D0.OR.RAYX(1501).NE.0.0D0) SSTOPI=2000
                          IF(RAYY(2001).NE.0.0D0.OR.RAYX(2001).NE.0.0D0) SSTOPI=2500
                          IF(RAYY(2501).NE.0.0D0.OR.RAYX(2501).NE.0.0D0) SSTOPI=3000
                          IF(RAYY(3001).NE.0.0D0.OR.RAYX(3001).NE.0.0D0) SSTOPI=3500
                          IF(RAYY(3501).NE.0.0D0.OR.RAYX(3501).NE.0.0D0) SSTOPI=4000
                          IF(RAYY(4001).NE.0.0D0.OR.RAYX(4001).NE.0.0D0) SSTOPI=4500
                          IF(RAYY(4501).NE.0.0D0.OR.RAYX(4501).NE.0.0D0) SSTOPI=5000
                          WRITE(OUTLYNE,*) 'FLDSRAYS ',DBLE(SSTOPI)
                          CALL SHOWIT(10)
                          DO I=1,200
                              CALL ITOAAA(I,AI4)
                              WRITE(UNIT=OUTLYNE,FMT=100) AI4,FIELDY(I),FIELDX(I),
     1                        FIELDZ(I),INT(FIELDW(I))
                              CALL SHOWIT(11)
                          END DO
 102                      FORMAT(A4,1X,D15.8,1X,D15.8,1X,I2)
                          IF(RAYY(501).EQ.0.0D0.AND.RAYX(501).EQ.0.0D0)  SSTOPI=500
                          IF(RAYY(501).NE.0.0D0.OR.RAYX(501).NE.0.0D0)   SSTOPI=1000
                          IF(RAYY(1001).NE.0.0D0.OR.RAYX(1001).NE.0.0D0) SSTOPI=1500
                          IF(RAYY(1501).NE.0.0D0.OR.RAYX(1501).NE.0.0D0) SSTOPI=2000
                          IF(RAYY(2001).NE.0.0D0.OR.RAYX(2001).NE.0.0D0) SSTOPI=2500
                          IF(RAYY(2501).NE.0.0D0.OR.RAYX(2501).NE.0.0D0) SSTOPI=3000
                          IF(RAYY(3001).NE.0.0D0.OR.RAYX(3001).NE.0.0D0) SSTOPI=3500
                          IF(RAYY(3501).NE.0.0D0.OR.RAYX(3501).NE.0.0D0) SSTOPI=4000
                          IF(RAYY(4001).NE.0.0D0.OR.RAYX(4001).NE.0.0D0) SSTOPI=4500
                          IF(RAYY(4501).NE.0.0D0.OR.RAYX(4501).NE.0.0D0) SSTOPI=5000
                          DO I=1,SSTOPI
                              CALL ITOAAA(I,AI4)
                              WRITE(UNIT=OUTLYNE,FMT=102) AI4,RAYY(I),RAYX(I),
     1                        INT(RAYW(I))
                              CALL SHOWIT(11)
                          END DO
                      END IF
                      CALL OPDMP
                      CALL TLDMP
                  END IF
              END IF
              IF(ISKDP) CALL MULTFLDS
C

C       FINISHED WITH LENO OUTPUT
C
C********************************************************
C
              RETURN
          END IF
          IF(WQ.NE.'REVERSE'.AND.WQ.NE.'CV'.AND.WQ.NE.'AC'.AND.
     1    WQ.NE.'RD'.AND.WQ.NE.'NOOPT'.AND.WQ.NE.'HEX') THEN
              WRITE(OUTLYNE,*)'INVALID QUALIFIER WORD USED WITH "LENO".'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(WQ.EQ.'REVERSE') THEN
C       DO A LENO REVERSE. THIS OPERATION REMOVES
C       ALL PIKUPS, SOLVES, TILTS, DECENTRATIONS, AND ALTERNATE
C       CONFIGURATIONS. THE REVERSED DESIGN MAY NOT BE TRACABLE.
              CALL RLENHD
              DO 20 I=INT(SYSTEM1(20)),0,-1
                  CALL RLENSF(I)
 20           CONTINUE
C       LENO REVERSE COMPLETED.
              CALL RLENED
              RETURN
          END IF
          IF(WQ.EQ.'CV') THEN
C       DO A LENO CV (CODE-V). THIS OPERATION REMOVES
C       ALL PIKUPS, SOLVES, AND ALTERNATE
C       CONFIGURATIONS.
              CALL LENHDCV
              DO I=0,INT(SYSTEM1(20))
                  CALL LENSFCV(I)
              END DO
              OUTLYNE='GO'
              CALL SHOWIT(10)
C       LENO REVERSE COMPLETED.
              RETURN
          END IF
          RETURN
      END
C SUB LENOCSV.FOR
      SUBROUTINE LENOCSV
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          INCLUDE 'datcfg.inc'
          INTEGER I
          REAL*8 RD,TH
          CHARACTER*13 GLNAME
          OPEN(UNIT=84,BLANK='NULL'
     1    ,FORM='FORMATTED',FILE=trim(HOME)//'LENO.CSV'
     2    ,STATUS='UNKNOWN')
          CALL CLOSE_FILE(84,0)
          OPEN(UNIT=84,BLANK='NULL'
     1    ,FORM='FORMATTED',FILE=trim(HOME)//'LENO.CSV'
     2    ,STATUS='UNKNOWN')
C     HERE IS WHERE WE OUTPUT THE LENS DATABASE TO A CSV FILE
C     OUTPUT OF SURF #,RADIUS, THICKNESS, GLASS NAME
          WRITE(84,*)'SURF#,RADIUS,THICKNESS,GLASS NAME'
          DO I=0,INT(SYSTEM1(20))
              IF(ALENS(1,I).EQ.0.0D0) THEN
                  RD=0.0D0
              ELSE
                  RD=1.0D0/ALENS(1,I)
              END IF
              TH=ALENS(3,I)
              GLNAME=GLANAM(I,2)
              WRITE(84,10) I,RD,TH,GLNAME
10            FORMAT(I3,',',D23.15,',',D23.15,',',A13)
          END DO
          CALL CLOSE_FILE(84,1)
          RETURN
      END

C SUB MULTFLDS.FOR
      SUBROUTINE MULTFLDS
C
          IMPLICIT NONE
C
C       SUBROUTINE MULTFLDS HANDELS LENS MULTIPLE FIELD OF VIEW OUTPUT
C
          INTEGER I
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
C
          IF(CFLDCNT.GT.0) THEN
C     MULTIPLE FLDS DEFINED
C
              WRITE(OUTLYNE,1000) DBLE(CFLDCNT)
              CALL SHOWIT(10)
 1000         FORMAT('FLDS MAX ,',G23.15)
              DO I=1,CFLDCNT
                  WRITE(OUTLYNE,2000) DBLE(I),CFLDS(1,I),CFLDS(2,I)
                  CALL SHOWIT(10)
              END DO
 2000         FORMAT('FLDS ,',G23.15,',',G23.15,',',G23.15)
          END IF
          RETURN
      END
C SUB LENED.FOR
      SUBROUTINE LENED
C
          IMPLICIT NONE
C
C       SUBROUTINE LENED HANDEL LENS TRAILING INFO OUTPUT
C       DURING A SIMPLE LENO. ACTS ON THE CURRENT LENS.
C       USED AFTER SURFACE DATA OUTPUT
C
          INTEGER SS,I,J
C
          INCLUDE 'datcfg.inc'
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datspd.inc'
C
C       THE TERMINAL RECORD FOR AND LENO IS EOS
C
          WRITE(OUTLYNE,10)
          CALL SHOWIT(10)
C
C**********************************************************
          IF(SYSTEM1(62).EQ.1.0D0) THEN
C     RAY AIMING YES
              WRITE(OUTLYNE,1000)
              CALL SHOWIT(10)
              WRITE(OUTLYNE,4000) SYSTEM1(81),SYSTEM1(82),SYSTEM1(89)
              CALL SHOWIT(10)
 4000         FORMAT('AIMRAY  OFFSET  ,',G23.15,',',G23.15,',',G23.15)
          ELSE
              IF(SYSTEM1(63).EQ.1.0D0) THEN
C     TEL YES
                  WRITE(OUTLYNE,1002)
                  CALL SHOWIT(10)
              ELSE
C     RAY AIMING OFF
                  WRITE(OUTLYNE,1001)
                  CALL SHOWIT(10)
              END IF
 1000         FORMAT('AIMRAY   ON')
 1001         FORMAT('AIMRAY   OFF')
 1002         FORMAT('TEL      ON')
          END IF
C**********************************************************
C**********************************************************
          IF(SYSTEM1(128).EQ.1.0D0) THEN
C     FLIPREFY
              WRITE(OUTLYNE,4001)
              CALL SHOWIT(10)
 4001         FORMAT('FLIPREFX ON')
          END IF
C**********************************************************
C**********************************************************
          IF(SYSTEM1(129).EQ.1.0D0) THEN
C     FLIPREFY
              WRITE(OUTLYNE,4002)
              CALL SHOWIT(10)
 4002         FORMAT('FLIPREFY ON')
          END IF
C**********************************************************
C**********************************************************
          IF(SYSTEM1(103).EQ.1.0D0) THEN
C     SCREEN ON
              WRITE(OUTLYNE,1003) SYSTEM1(104),SYSTEM1(105),SYSTEM1(106)
     1        ,SYSTEM1(107),SYSTEM1(108)
              CALL SHOWIT(10)
 1003         FORMAT('SCREEN   ON      ,',G23.15,',',G23.15,','
     1         ,G23.15,',',G23.15,',',G23.15)
          END IF
C**********************************************************

C       MODE SETTING

          IF(SYSTEM1(30).EQ.1) WRITE(OUTLYNE,33)
          IF(SYSTEM1(30).EQ.2) WRITE(OUTLYNE,34)
          IF(SYSTEM1(30).EQ.3) WRITE(OUTLYNE,35)
          IF(SYSTEM1(30).EQ.4) WRITE(OUTLYNE,36)
          CALL SHOWIT(10)
 33       FORMAT('MODE     FOCAL')
 34       FORMAT('MODE     UFOCAL')
 35       FORMAT('MODE     AFOCAL')
 36       FORMAT('MODE     UAFOCAL')
C
C       SPTWT

          WRITE(OUTLYNE,3000) SYSTEM1(31),SYSTEM1(32),SYSTEM1(33)
     1    ,SYSTEM1(34),SYSTEM1(35)
          CALL SHOWIT(10)
 3000     FORMAT('SPTWT   ,',G23.15,',',G23.15,',',G23.15,
     1    ',',G23.15,',',G23.15)
C       SPTWT2

 3003     FORMAT('SPTWT2  ,',G23.15,',',G23.15,',',G23.15,
     1    ',',G23.15,',',G23.15)
          WRITE(OUTLYNE,3003) SYSTEM1(76),SYSTEM1(77),SYSTEM1(78)
     1    ,SYSTEM1(79),SYSTEM1(80)
          CALL SHOWIT(10)
C     *****************************************************************
C       NOW IS THERE ANY SPSRF DATA
C
          SS=0
          DO 20 I=0,INT(SYSTEM1(20))
              IF(ALENS(34,I).NE.0.0) THEN
C       FOUND SPECIAL SURFACE DATA
                  SS=SS+1
              END IF
 20       CONTINUE
          IF(SS.NE.0) THEN
C       PRINT SPECIAL SURFACE DATA
              WRITE(OUTLYNE,200)
              CALL SHOWIT(10)
 200          FORMAT('SPSRF')
              DO 21  I=0,INT(SYSTEM1(20))
                  IF(ALENS(34,I).NE.0.0) THEN
                      WRITE(OUTLYNE,210) DBLE(I),ALENS(34,I)
                      CALL SHOWIT(10)
 210                  FORMAT('SPECIAL ,',G23.15,',',G23.15)
C
C       NOW WRITE THE COEFFICIENTS
                      IF(DABS(ALENS(34,I)).GE.1.0.AND.
     1                DABS(ALENS(34,I)).LE.30.0) THEN
                          IF(FTFL01(1,I).NE.0.0D0) WRITE(OUTLYNE,300) DBLE(I),FTFL01(1,I)
                          IF(FTFL01(1,I).NE.0.0D0) CALL SHOWIT(10)
                          IF(FTFL01(2,I).NE.0.0D0) WRITE(OUTLYNE,301) DBLE(I),FTFL01(2,I)
                          IF(FTFL01(2,I).NE.0.0D0) CALL SHOWIT(10)
                          IF(DABS(ALENS(34,I)).NE.12.0D0.AND.DABS(ALENS(34,I)).NE.13.0D0)
     1                     THEN
                              IF(FTFL01(3,I).NE.0.0D0) WRITE(OUTLYNE,302) DBLE(I),FTFL01(3,I)
                              IF(FTFL01(3,I).NE.0.0D0) CALL SHOWIT(10)
                              IF(FTFL01(4,I).NE.0.0D0) WRITE(OUTLYNE,303) DBLE(I),FTFL01(4,I)
                              IF(FTFL01(4,I).NE.0.0D0) CALL SHOWIT(10)
                              IF(FTFL01(5,I).NE.0.0D0) WRITE(OUTLYNE,304) DBLE(I),FTFL01(5,I)
                              IF(FTFL01(5,I).NE.0.0D0) CALL SHOWIT(10)
                          ELSE
                              WRITE(OUTLYNE,302) DBLE(I),FTFL01(3,I)
                              CALL SHOWIT(10)
                              WRITE(OUTLYNE,303) DBLE(I),FTFL01(4,I)
                              CALL SHOWIT(10)
                              WRITE(OUTLYNE,304) DBLE(I),FTFL01(5,I)
                              CALL SHOWIT(10)
                          END IF

                          IF(FTFL01(6,I).NE.0.0D0) WRITE(OUTLYNE,305) DBLE(I),FTFL01(6,I)
                          IF(FTFL01(6,I).NE.0.0D0) CALL SHOWIT(10)

                          IF(DABS(ALENS(34,I)).NE.12.0D0.AND.DABS(ALENS(34,I)).NE.13.0D0)
     1                     THEN
                              IF(FTFL01(7,I).NE.0.0D0) WRITE(OUTLYNE,306) DBLE(I),FTFL01(7,I)
                              IF(FTFL01(7,I).NE.0.0D0) CALL SHOWIT(10)

                              IF(FTFL01(8,I).NE.0.0D0) WRITE(OUTLYNE,307) DBLE(I),FTFL01(8,I)
                              IF(FTFL01(8,I).NE.0.0D0) CALL SHOWIT(10)

                              IF(FTFL01(9,I).NE.0.0D0) WRITE(OUTLYNE,308) DBLE(I),FTFL01(9,I)
                              IF(FTFL01(9,I).NE.0.0D0) CALL SHOWIT(10)
                          ELSE
                              WRITE(OUTLYNE,306) DBLE(I),FTFL01(7,I)
                              CALL SHOWIT(10)

                              WRITE(OUTLYNE,307) DBLE(I),FTFL01(8,I)
                              CALL SHOWIT(10)

                              WRITE(OUTLYNE,308) DBLE(I),FTFL01(9,I)
                              CALL SHOWIT(10)
                          END IF

                          IF(FTFL01(10,I).NE.0.0D0) WRITE(OUTLYNE,309) DBLE(I),FTFL01(10,I)
                          IF(FTFL01(10,I).NE.0.0D0) CALL SHOWIT(10)

                          IF(FTFL01(11,I).NE.0.0D0) WRITE(OUTLYNE,310) DBLE(I),FTFL01(11,I)
                          IF(FTFL01(11,I).NE.0.0D0) CALL SHOWIT(10)

                          IF(FTFL01(12,I).NE.0.0D0) WRITE(OUTLYNE,311) DBLE(I),FTFL01(12,I)
                          IF(FTFL01(12,I).NE.0.0D0) CALL SHOWIT(10)

                          IF(FTFL01(13,I).NE.0.0D0) WRITE(OUTLYNE,312) DBLE(I),FTFL01(13,I)
                          IF(FTFL01(13,I).NE.0.0D0) CALL SHOWIT(10)

                          IF(FTFL01(14,I).NE.0.0D0) WRITE(OUTLYNE,313) DBLE(I),FTFL01(14,I)
                          IF(FTFL01(14,I).NE.0.0D0) CALL SHOWIT(10)

                          IF(FTFL01(15,I).NE.0.0D0) WRITE(OUTLYNE,314) DBLE(I),FTFL01(15,I)
                          IF(FTFL01(15,I).NE.0.0D0) CALL SHOWIT(10)

                          IF(FTFL01(16,I).NE.0.0D0) WRITE(OUTLYNE,315) DBLE(I),FTFL01(16,I)
                          IF(FTFL01(16,I).NE.0.0D0) CALL SHOWIT(10)

                          IF(FTFL01(17,I).NE.0.0D0) WRITE(OUTLYNE,316) DBLE(I),FTFL01(17,I)
                          IF(FTFL01(17,I).NE.0.0D0) CALL SHOWIT(10)

                          IF(FTFL01(18,I).NE.0.0D0) WRITE(OUTLYNE,317) DBLE(I),FTFL01(18,I)
                          IF(FTFL01(18,I).NE.0.0D0) CALL SHOWIT(10)

                          IF(FTFL01(19,I).NE.0.0D0) WRITE(OUTLYNE,318) DBLE(I),FTFL01(19,I)
                          IF(FTFL01(19,I).NE.0.0D0) CALL SHOWIT(10)

                          IF(FTFL01(20,I).NE.0.0D0) WRITE(OUTLYNE,319) DBLE(I),FTFL01(20,I)
                          IF(FTFL01(20,I).NE.0.0D0) CALL SHOWIT(10)

                          IF(FTFL01(21,I).NE.0.0D0) WRITE(OUTLYNE,320) DBLE(I),FTFL01(21,I)
                          IF(FTFL01(21,I).NE.0.0D0) CALL SHOWIT(10)

                          IF(FTFL01(22,I).NE.0.0D0) WRITE(OUTLYNE,321) DBLE(I),FTFL01(22,I)
                          IF(FTFL01(22,I).NE.0.0D0) CALL SHOWIT(10)

                          IF(FTFL01(23,I).NE.0.0D0) WRITE(OUTLYNE,322) DBLE(I),FTFL01(23,I)
                          IF(FTFL01(23,I).NE.0.0D0) CALL SHOWIT(10)

                          IF(FTFL01(24,I).NE.0.0D0) WRITE(OUTLYNE,323) DBLE(I),FTFL01(24,I)
                          IF(FTFL01(24,I).NE.0.0D0) CALL SHOWIT(10)

                          IF(FTFL01(25,I).NE.0.0D0) WRITE(OUTLYNE,324) DBLE(I),FTFL01(25,I)
                          IF(FTFL01(25,I).NE.0.0D0) CALL SHOWIT(10)

                          IF(FTFL01(26,I).NE.0.0D0) WRITE(OUTLYNE,325) DBLE(I),FTFL01(26,I)
                          IF(FTFL01(26,I).NE.0.0D0) CALL SHOWIT(10)

                          IF(FTFL01(27,I).NE.0.0D0) WRITE(OUTLYNE,326) DBLE(I),FTFL01(27,I)
                          IF(FTFL01(27,I).NE.0.0D0) CALL SHOWIT(10)

                          IF(FTFL01(28,I).NE.0.0D0) WRITE(OUTLYNE,327) DBLE(I),FTFL01(28,I)
                          IF(FTFL01(28,I).NE.0.0D0) CALL SHOWIT(10)

                          IF(FTFL01(29,I).NE.0.0D0) WRITE(OUTLYNE,328) DBLE(I),FTFL01(29,I)
                          IF(FTFL01(29,I).NE.0.0D0) CALL SHOWIT(10)

                          IF(FTFL01(30,I).NE.0.0D0) WRITE(OUTLYNE,329) DBLE(I),FTFL01(30,I)
                          IF(FTFL01(30,I).NE.0.0D0) CALL SHOWIT(10)

                          IF(FTFL01(31,I).NE.0.0D0) WRITE(OUTLYNE,330) DBLE(I),FTFL01(31,I)
                          IF(FTFL01(31,I).NE.0.0D0) CALL SHOWIT(10)

                          IF(FTFL01(32,I).NE.0.0D0) WRITE(OUTLYNE,331) DBLE(I),FTFL01(32,I)
                          IF(FTFL01(32,I).NE.0.0D0) CALL SHOWIT(10)

                          IF(FTFL01(33,I).NE.0.0D0) WRITE(OUTLYNE,332) DBLE(I),FTFL01(33,I)
                          IF(FTFL01(33,I).NE.0.0D0) CALL SHOWIT(10)

                          IF(FTFL01(34,I).NE.0.0D0) WRITE(OUTLYNE,333) DBLE(I),FTFL01(34,I)
                          IF(FTFL01(34,I).NE.0.0D0) CALL SHOWIT(10)

                          IF(FTFL01(35,I).NE.0.0D0) WRITE(OUTLYNE,334) DBLE(I),FTFL01(35,I)
                          IF(FTFL01(35,I).NE.0.0D0) CALL SHOWIT(10)

                          IF(FTFL01(36,I).NE.0.0D0) WRITE(OUTLYNE,335) DBLE(I),FTFL01(36,I)
                          IF(FTFL01(36,I).NE.0.0D0) CALL SHOWIT(10)

                          IF(FTFL01(37,I).NE.0.0D0) WRITE(OUTLYNE,336) DBLE(I),FTFL01(37,I)
                          IF(FTFL01(37,I).NE.0.0D0) CALL SHOWIT(10)

                          IF(FTFL01(38,I).NE.0.0D0) WRITE(OUTLYNE,337) DBLE(I),FTFL01(38,I)
                          IF(FTFL01(38,I).NE.0.0D0) CALL SHOWIT(10)

                          IF(FTFL01(39,I).NE.0.0D0) WRITE(OUTLYNE,338) DBLE(I),FTFL01(39,I)
                          IF(FTFL01(39,I).NE.0.0D0) CALL SHOWIT(10)

                          IF(FTFL01(40,I).NE.0.0D0) WRITE(OUTLYNE,339) DBLE(I),FTFL01(40,I)
                          IF(FTFL01(40,I).NE.0.0D0) CALL SHOWIT(10)

                          IF(FTFL01(41,I).NE.0.0D0) WRITE(OUTLYNE,340) DBLE(I),FTFL01(41,I)
                          IF(FTFL01(41,I).NE.0.0D0) CALL SHOWIT(10)

                          IF(FTFL01(42,I).NE.0.0D0) WRITE(OUTLYNE,341) DBLE(I),FTFL01(42,I)
                          IF(FTFL01(42,I).NE.0.0D0) CALL SHOWIT(10)

                          IF(FTFL01(43,I).NE.0.0D0) WRITE(OUTLYNE,342) DBLE(I),FTFL01(43,I)
                          IF(FTFL01(43,I).NE.0.0D0) CALL SHOWIT(10)

                          IF(FTFL01(44,I).NE.0.0D0) WRITE(OUTLYNE,343) DBLE(I),FTFL01(44,I)
                          IF(FTFL01(44,I).NE.0.0D0) CALL SHOWIT(10)

                          IF(FTFL01(45,I).NE.0.0D0) WRITE(OUTLYNE,344) DBLE(I),FTFL01(45,I)
                          IF(FTFL01(45,I).NE.0.0D0) CALL SHOWIT(10)

                          IF(FTFL01(46,I).NE.0.0D0) WRITE(OUTLYNE,345) DBLE(I),FTFL01(46,I)
                          IF(FTFL01(46,I).NE.0.0D0) CALL SHOWIT(10)

                          IF(FTFL01(47,I).NE.0.0D0) WRITE(OUTLYNE,346) DBLE(I),FTFL01(47,I)
                          IF(FTFL01(47,I).NE.0.0D0) CALL SHOWIT(10)

                          IF(FTFL01(48,I).NE.0.0D0) WRITE(OUTLYNE,347) DBLE(I),FTFL01(48,I)
                          IF(FTFL01(48,I).NE.0.0D0) CALL SHOWIT(10)

                          IF(FTFL01(49,I).NE.0.0D0) WRITE(OUTLYNE,348) DBLE(I),FTFL01(49,I)
                          IF(FTFL01(49,I).NE.0.0D0) CALL SHOWIT(10)

                          IF(FTFL01(50,I).NE.0.0D0) WRITE(OUTLYNE,349) DBLE(I),FTFL01(50,I)
                          IF(FTFL01(50,I).NE.0.0D0) CALL SHOWIT(10)

                          IF(FTFL01(51,I).NE.0.0D0) WRITE(OUTLYNE,350) DBLE(I),FTFL01(51,I)
                          IF(FTFL01(51,I).NE.0.0D0) CALL SHOWIT(10)

                          IF(FTFL01(52,I).NE.0.0D0) WRITE(OUTLYNE,351) DBLE(I),FTFL01(52,I)
                          IF(FTFL01(52,I).NE.0.0D0) CALL SHOWIT(10)

                          IF(FTFL01(53,I).NE.0.0D0) WRITE(OUTLYNE,352) DBLE(I),FTFL01(53,I)
                          IF(FTFL01(53,I).NE.0.0D0) CALL SHOWIT(10)

                          IF(FTFL01(54,I).NE.0.0D0) WRITE(OUTLYNE,353) DBLE(I),FTFL01(54,I)
                          IF(FTFL01(54,I).NE.0.0D0) CALL SHOWIT(10)

                          IF(FTFL01(55,I).NE.0.0D0) WRITE(OUTLYNE,354) DBLE(I),FTFL01(55,I)
                          IF(FTFL01(55,I).NE.0.0D0) CALL SHOWIT(10)

                          IF(FTFL01(56,I).NE.0.0D0) WRITE(OUTLYNE,355) DBLE(I),FTFL01(56,I)
                          IF(FTFL01(56,I).NE.0.0D0) CALL SHOWIT(10)

                          IF(FTFL01(57,I).NE.0.0D0) WRITE(OUTLYNE,356) DBLE(I),FTFL01(57,I)
                          IF(FTFL01(57,I).NE.0.0D0) CALL SHOWIT(10)

                          IF(FTFL01(58,I).NE.0.0D0) WRITE(OUTLYNE,357) DBLE(I),FTFL01(58,I)
                          IF(FTFL01(58,I).NE.0.0D0) CALL SHOWIT(10)

                          IF(FTFL01(59,I).NE.0.0D0) WRITE(OUTLYNE,358) DBLE(I),FTFL01(59,I)
                          IF(FTFL01(59,I).NE.0.0D0) CALL SHOWIT(10)

                          IF(FTFL01(60,I).NE.0.0D0) WRITE(OUTLYNE,359) DBLE(I),FTFL01(60,I)
                          IF(FTFL01(60,I).NE.0.0D0) CALL SHOWIT(10)

                          IF(FTFL01(61,I).NE.0.0D0) WRITE(OUTLYNE,360) DBLE(I),FTFL01(61,I)
                          IF(FTFL01(61,I).NE.0.0D0) CALL SHOWIT(10)

                          IF(FTFL01(62,I).NE.0.0D0) WRITE(OUTLYNE,361) DBLE(I),FTFL01(62,I)
                          IF(FTFL01(62,I).NE.0.0D0) CALL SHOWIT(10)

                          IF(FTFL01(63,I).NE.0.0D0) WRITE(OUTLYNE,362) DBLE(I),FTFL01(63,I)
                          IF(FTFL01(63,I).NE.0.0D0) CALL SHOWIT(10)

                          IF(FTFL01(64,I).NE.0.0D0) WRITE(OUTLYNE,363) DBLE(I),FTFL01(64,I)
                          IF(FTFL01(64,I).NE.0.0D0) CALL SHOWIT(10)

                          IF(FTFL01(65,I).NE.0.0D0) WRITE(OUTLYNE,364) DBLE(I),FTFL01(65,I)
                          IF(FTFL01(65,I).NE.0.0D0) CALL SHOWIT(10)

                          IF(FTFL01(66,I).NE.0.0D0) WRITE(OUTLYNE,365) DBLE(I),FTFL01(66,I)
                          IF(FTFL01(66,I).NE.0.0D0) CALL SHOWIT(10)

                          IF(FTFL01(67,I).NE.0.0D0) WRITE(OUTLYNE,366) DBLE(I),FTFL01(67,I)
                          IF(FTFL01(67,I).NE.0.0D0) CALL SHOWIT(10)

                          IF(FTFL01(68,I).NE.0.0D0) WRITE(OUTLYNE,367) DBLE(I),FTFL01(68,I)
                          IF(FTFL01(68,I).NE.0.0D0) CALL SHOWIT(10)

                          IF(FTFL01(69,I).NE.0.0D0) WRITE(OUTLYNE,368) DBLE(I),FTFL01(69,I)
                          IF(FTFL01(69,I).NE.0.0D0) CALL SHOWIT(10)

                          IF(FTFL01(70,I).NE.0.0D0) WRITE(OUTLYNE,369) DBLE(I),FTFL01(70,I)
                          IF(FTFL01(70,I).NE.0.0D0) CALL SHOWIT(10)

                          IF(FTFL01(71,I).NE.0.0D0) WRITE(OUTLYNE,370) DBLE(I),FTFL01(71,I)
                          IF(FTFL01(71,I).NE.0.0D0) CALL SHOWIT(10)

                          IF(FTFL01(72,I).NE.0.0D0) WRITE(OUTLYNE,371) DBLE(I),FTFL01(72,I)
                          IF(FTFL01(72,I).NE.0.0D0) CALL SHOWIT(10)

                          IF(FTFL01(73,I).NE.0.0D0) WRITE(OUTLYNE,372) DBLE(I),FTFL01(73,I)
                          IF(FTFL01(73,I).NE.0.0D0) CALL SHOWIT(10)

                          IF(FTFL01(74,I).NE.0.0D0) WRITE(OUTLYNE,373) DBLE(I),FTFL01(74,I)
                          IF(FTFL01(74,I).NE.0.0D0) CALL SHOWIT(10)

                          IF(FTFL01(75,I).NE.0.0D0) WRITE(OUTLYNE,374) DBLE(I),FTFL01(75,I)
                          IF(FTFL01(75,I).NE.0.0D0) CALL SHOWIT(10)

                          IF(FTFL01(76,I).NE.0.0D0) WRITE(OUTLYNE,375) DBLE(I),FTFL01(76,I)
                          IF(FTFL01(76,I).NE.0.0D0) CALL SHOWIT(10)

                          IF(FTFL01(77,I).NE.0.0D0) WRITE(OUTLYNE,376) DBLE(I),FTFL01(77,I)
                          IF(FTFL01(77,I).NE.0.0D0) CALL SHOWIT(10)

                          IF(FTFL01(78,I).NE.0.0D0) WRITE(OUTLYNE,377) DBLE(I),FTFL01(78,I)
                          IF(FTFL01(78,I).NE.0.0D0) CALL SHOWIT(10)

                          IF(FTFL01(79,I).NE.0.0D0) WRITE(OUTLYNE,378) DBLE(I),FTFL01(79,I)
                          IF(FTFL01(79,I).NE.0.0D0) CALL SHOWIT(10)

                          IF(FTFL01(80,I).NE.0.0D0) WRITE(OUTLYNE,379) DBLE(I),FTFL01(80,I)
                          IF(FTFL01(80,I).NE.0.0D0) CALL SHOWIT(10)

                          IF(FTFL01(81,I).NE.0.0D0) WRITE(OUTLYNE,380) DBLE(I),FTFL01(81,I)
                          IF(FTFL01(81,I).NE.0.0D0) CALL SHOWIT(10)

                          IF(FTFL01(82,I).NE.0.0D0) WRITE(OUTLYNE,381) DBLE(I),FTFL01(82,I)
                          IF(FTFL01(82,I).NE.0.0D0) CALL SHOWIT(10)

                          IF(FTFL01(83,I).NE.0.0D0) WRITE(OUTLYNE,382) DBLE(I),FTFL01(83,I)
                          IF(FTFL01(83,I).NE.0.0D0) CALL SHOWIT(10)

                          IF(FTFL01(84,I).NE.0.0D0) WRITE(OUTLYNE,383) DBLE(I),FTFL01(84,I)
                          IF(FTFL01(84,I).NE.0.0D0) CALL SHOWIT(10)

                          IF(FTFL01(85,I).NE.0.0D0) WRITE(OUTLYNE,384) DBLE(I),FTFL01(85,I)
                          IF(FTFL01(85,I).NE.0.0D0) CALL SHOWIT(10)

                          IF(FTFL01(86,I).NE.0.0D0) WRITE(OUTLYNE,385) DBLE(I),FTFL01(86,I)
                          IF(FTFL01(86,I).NE.0.0D0) CALL SHOWIT(10)

                          IF(FTFL01(87,I).NE.0.0D0) WRITE(OUTLYNE,386) DBLE(I),FTFL01(87,I)
                          IF(FTFL01(87,I).NE.0.0D0) CALL SHOWIT(10)

                          IF(FTFL01(88,I).NE.0.0D0) WRITE(OUTLYNE,387) DBLE(I),FTFL01(88,I)
                          IF(FTFL01(88,I).NE.0.0D0) CALL SHOWIT(10)

                          IF(FTFL01(89,I).NE.0.0D0) WRITE(OUTLYNE,388) DBLE(I),FTFL01(89,I)
                          IF(FTFL01(89,I).NE.0.0D0) CALL SHOWIT(10)

                          IF(FTFL01(90,I).NE.0.0D0) WRITE(OUTLYNE,389) DBLE(I),FTFL01(90,I)
                          IF(FTFL01(90,I).NE.0.0D0) CALL SHOWIT(10)

                          IF(FTFL01(91,I).NE.0.0D0) WRITE(OUTLYNE,390) DBLE(I),FTFL01(91,I)
                          IF(FTFL01(91,I).NE.0.0D0) CALL SHOWIT(10)

                          IF(FTFL01(92,I).NE.0.0D0) WRITE(OUTLYNE,391) DBLE(I),FTFL01(92,I)
                          IF(FTFL01(92,I).NE.0.0D0) CALL SHOWIT(10)

                          IF(FTFL01(93,I).NE.0.0D0) WRITE(OUTLYNE,392) DBLE(I),FTFL01(93,I)
                          IF(FTFL01(93,I).NE.0.0D0) CALL SHOWIT(10)

                          IF(FTFL01(94,I).NE.0.0D0) WRITE(OUTLYNE,393) DBLE(I),FTFL01(94,I)
                          IF(FTFL01(94,I).NE.0.0D0) CALL SHOWIT(10)

                          IF(FTFL01(95,I).NE.0.0D0) WRITE(OUTLYNE,394) DBLE(I),FTFL01(95,I)
                          IF(FTFL01(95,I).NE.0.0D0) CALL SHOWIT(10)

                          IF(FTFL01(96,I).NE.0.0D0) WRITE(OUTLYNE,395) DBLE(I),FTFL01(96,I)
                          IF(FTFL01(96,I).NE.0.0D0) CALL SHOWIT(10)
                      END IF
                  END IF
 21           CONTINUE
              WRITE(OUTLYNE,10)
              CALL SHOWIT(10)
          END IF
 300      FORMAT('C1      ,',G23.15,',',G23.15)
 301      FORMAT('C2      ,',G23.15,',',G23.15)
 302      FORMAT('C3      ,',G23.15,',',G23.15)
 303      FORMAT('C4      ,',G23.15,',',G23.15)
 304      FORMAT('C5      ,',G23.15,',',G23.15)
 305      FORMAT('C6      ,',G23.15,',',G23.15)
 306      FORMAT('C7      ,',G23.15,',',G23.15)
 307      FORMAT('C8      ,',G23.15,',',G23.15)
 308      FORMAT('C9      ,',G23.15,',',G23.15)
 309      FORMAT('C10     ,',G23.15,',',G23.15)
 310      FORMAT('C11     ,',G23.15,',',G23.15)
 311      FORMAT('C12     ,',G23.15,',',G23.15)
 312      FORMAT('C13     ,',G23.15,',',G23.15)
 313      FORMAT('C14     ,',G23.15,',',G23.15)
 314      FORMAT('C15     ,',G23.15,',',G23.15)
 315      FORMAT('C16     ,',G23.15,',',G23.15)
 316      FORMAT('C17     ,',G23.15,',',G23.15)
 317      FORMAT('C18     ,',G23.15,',',G23.15)
 318      FORMAT('C19     ,',G23.15,',',G23.15)
 319      FORMAT('C20     ,',G23.15,',',G23.15)
 320      FORMAT('C21     ,',G23.15,',',G23.15)
 321      FORMAT('C22     ,',G23.15,',',G23.15)
 322      FORMAT('C23     ,',G23.15,',',G23.15)
 323      FORMAT('C24     ,',G23.15,',',G23.15)
 324      FORMAT('C25     ,',G23.15,',',G23.15)
 325      FORMAT('C26     ,',G23.15,',',G23.15)
 326      FORMAT('C27     ,',G23.15,',',G23.15)
 327      FORMAT('C28     ,',G23.15,',',G23.15)
 328      FORMAT('C29     ,',G23.15,',',G23.15)
 329      FORMAT('C30     ,',G23.15,',',G23.15)
 330      FORMAT('C31     ,',G23.15,',',G23.15)
 331      FORMAT('C32     ,',G23.15,',',G23.15)
 332      FORMAT('C33     ,',G23.15,',',G23.15)
 333      FORMAT('C34     ,',G23.15,',',G23.15)
 334      FORMAT('C35     ,',G23.15,',',G23.15)
 335      FORMAT('C36     ,',G23.15,',',G23.15)
 336      FORMAT('C37     ,',G23.15,',',G23.15)
 337      FORMAT('C38     ,',G23.15,',',G23.15)
 338      FORMAT('C39     ,',G23.15,',',G23.15)
 339      FORMAT('C40     ,',G23.15,',',G23.15)
 340      FORMAT('C41     ,',G23.15,',',G23.15)
 341      FORMAT('C42     ,',G23.15,',',G23.15)
 342      FORMAT('C43     ,',G23.15,',',G23.15)
 343      FORMAT('C44     ,',G23.15,',',G23.15)
 344      FORMAT('C45     ,',G23.15,',',G23.15)
 345      FORMAT('C46     ,',G23.15,',',G23.15)
 346      FORMAT('C47     ,',G23.15,',',G23.15)
 347      FORMAT('C48     ,',G23.15,',',G23.15)
 348      FORMAT('C49     ,',G23.15,',',G23.15)
 349      FORMAT('C50     ,',G23.15,',',G23.15)
 350      FORMAT('C51     ,',G23.15,',',G23.15)
 351      FORMAT('C52     ,',G23.15,',',G23.15)
 352      FORMAT('C53     ,',G23.15,',',G23.15)
 353      FORMAT('C54     ,',G23.15,',',G23.15)
 354      FORMAT('C55     ,',G23.15,',',G23.15)
 355      FORMAT('C56     ,',G23.15,',',G23.15)
 356      FORMAT('C57     ,',G23.15,',',G23.15)
 357      FORMAT('C58     ,',G23.15,',',G23.15)
 358      FORMAT('C59     ,',G23.15,',',G23.15)
 359      FORMAT('C60     ,',G23.15,',',G23.15)
 360      FORMAT('C61     ,',G23.15,',',G23.15)
 361      FORMAT('C62     ,',G23.15,',',G23.15)
 362      FORMAT('C63     ,',G23.15,',',G23.15)
 363      FORMAT('C64     ,',G23.15,',',G23.15)
 364      FORMAT('C65     ,',G23.15,',',G23.15)
 365      FORMAT('C66     ,',G23.15,',',G23.15)
 366      FORMAT('C67     ,',G23.15,',',G23.15)
 367      FORMAT('C68     ,',G23.15,',',G23.15)
 368      FORMAT('C69     ,',G23.15,',',G23.15)
 369      FORMAT('C70     ,',G23.15,',',G23.15)
 370      FORMAT('C71     ,',G23.15,',',G23.15)
 371      FORMAT('C72     ,',G23.15,',',G23.15)
 372      FORMAT('C73     ,',G23.15,',',G23.15)
 373      FORMAT('C74     ,',G23.15,',',G23.15)
 374      FORMAT('C75     ,',G23.15,',',G23.15)
 375      FORMAT('C76     ,',G23.15,',',G23.15)
 376      FORMAT('C77     ,',G23.15,',',G23.15)
 377      FORMAT('C78     ,',G23.15,',',G23.15)
 378      FORMAT('C79     ,',G23.15,',',G23.15)
 379      FORMAT('C80     ,',G23.15,',',G23.15)
 380      FORMAT('C81     ,',G23.15,',',G23.15)
 381      FORMAT('C82     ,',G23.15,',',G23.15)
 382      FORMAT('C83     ,',G23.15,',',G23.15)
 383      FORMAT('C84     ,',G23.15,',',G23.15)
 384      FORMAT('C85     ,',G23.15,',',G23.15)
 385      FORMAT('C86     ,',G23.15,',',G23.15)
 386      FORMAT('C87     ,',G23.15,',',G23.15)
 387      FORMAT('C88     ,',G23.15,',',G23.15)
 388      FORMAT('C89     ,',G23.15,',',G23.15)
 389      FORMAT('C90     ,',G23.15,',',G23.15)
 390      FORMAT('C91     ,',G23.15,',',G23.15)
 391      FORMAT('C92     ,',G23.15,',',G23.15)
 392      FORMAT('C93     ,',G23.15,',',G23.15)
 393      FORMAT('C94     ,',G23.15,',',G23.15)
 394      FORMAT('C95     ,',G23.15,',',G23.15)
 395      FORMAT('C96     ,',G23.15,',',G23.15)
C**********************************************************
C
C       IF THE LENS FILE HAS ANY CONFIGURATION DATA
C       DEFINED, WRITE IT OUT
C
C       CHECK EACH CONFIGURATION
C
          SS=0
          DO 100 I=2,MAXCFG
              IF(CFGCNT(I).GT.0) THEN
C       THERE IS DATA FOR CFG I
                  SS=SS+1
              END IF
 100      CONTINUE
          IF(SS.GT.0) THEN
C
C       THERE IS CONFIGS DATA
              WRITE(OUTLYNE,11)
              CALL SHOWIT(10)
 11           FORMAT('CONFIGS')
              DO 110 I=2,MAXCFG
                  IF(CFGCNT(I).GT.0) THEN
                      WRITE(OUTLYNE,13) DBLE(I)
                      CALL SHOWIT(10)
 13                   FORMAT('CFG     ,',G23.15)
C
                      DO 120 J=1,CFGCNT(I)
                          WRITE(OUTLYNE,12) CONFG(I,J)(1:139)
                          CALL SHOWIT(10)
 12                       FORMAT(A139)
 120                  CONTINUE
                  END IF
C       FINISHED WRITTING CURRENT CFG DATA
C       OR CFG HAD NO DATA
C       CHECK FOR OTHER CFG DATA
 110          CONTINUE
C       FINISHED WRITEING CFG DATA, WRITE EOS
              WRITE(OUTLYNE,10)
              CALL SHOWIT(10)
C
C       ALL CFG I DATA WRITEN
 10           FORMAT('EOS')
          END IF
C
C       TGR,NRD,PGR,GRI
          IF(TGR.GT.0.0D0.AND.TGRFLG.NE.0) THEN
              WRITE(OUTLYNE,*)'TGR,',TGR
              CALL SHOWIT(10)
          END IF
          IF(NRD.GT.0.0D0.AND.NRDFLG.NE.0) THEN
              WRITE(OUTLYNE,*)'NRD,',NRD
              CALL SHOWIT(10)
          END IF
          IF(PGR.GT.0.0D0) THEN
              WRITE(OUTLYNE,*)'PGR,',PGR
              CALL SHOWIT(10)
          END IF
          IF(GRI.GT.0.0D0.AND.GRIFLG.NE.0) THEN
              WRITE(OUTLYNE,*)'GRI,',GRI
              CALL SHOWIT(10)
          END IF
C
          RETURN
      END
C SUB RLENED.FOR
      SUBROUTINE RLENED
C
          IMPLICIT NONE
C
C       SUBROUTINE LENED HANDEL LENS TRAILING INFO OUTPUT
C       DURING A LENO REVERSE. ACTS ON THE CURRENT LENS.
C       USED AFTER SURFACE DATA OUTPUT
C
          INTEGER SS,I,J
C
          INCLUDE 'datcfg.inc'
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
C
C       THE TERMINAL RECORD FOR AND LENO IS EOS
C
          WRITE(OUTLYNE,10)
          CALL SHOWIT(10)
 10       FORMAT('EOS')
C
C     RAY AIMING STATUS IS ALWAYS ON FOR LENO REVERSE AND
C     TEL IS ALWAYS "OFF"
          WRITE(OUTLYNE,1000)
          CALL SHOWIT(10)
          CALL SHOWIT(10)
          WRITE(OUTLYNE,4000) SYSTEM1(81),SYSTEM1(82),SYSTEM1(89)
          CALL SHOWIT(10)
 4000     FORMAT('AIMRAY  OFFSET  ,',G23.15,',',G23.15,',',G23.15)
 1000     FORMAT('AIMRAY   ON')
C**********************************************************
          IF(SYSTEM1(128).EQ.1.0D0) THEN
C     FLIPREFY
              WRITE(OUTLYNE,4001)
              CALL SHOWIT(10)
 4001         FORMAT('FLIPREFX ON')
          END IF
C**********************************************************
C**********************************************************
          IF(SYSTEM1(129).EQ.1.0D0) THEN
C     FLIPREFY
              WRITE(OUTLYNE,4002)
              CALL SHOWIT(10)
 4002         FORMAT('FLIPREFY ON')
          END IF
C**********************************************************

C       MODE SETTING
          IF(SYSTEM1(30).EQ.1) WRITE(OUTLYNE,33)
          IF(SYSTEM1(30).EQ.2) WRITE(OUTLYNE,34)
          IF(SYSTEM1(30).EQ.3) WRITE(OUTLYNE,35)
          IF(SYSTEM1(30).EQ.4) WRITE(OUTLYNE,36)
          CALL SHOWIT(10)
 33       FORMAT('MODE     FOCAL')
 34       FORMAT('MODE     UFOCAL')
 35       FORMAT('MODE     AFOCAL')
 36       FORMAT('MODE     UAFOCAL')
C       SPTWT
          WRITE(OUTLYNE,3000) SYSTEM1(31),SYSTEM1(32),SYSTEM1(33)
     1    ,SYSTEM1(34),SYSTEM1(35)
          CALL SHOWIT(10)
 3000     FORMAT('SPTWT   ,',G23.15,',',G23.15,',',G23.15,
     1    ',',G23.15,',',G23.15)
C       SPTWT2
 3003     FORMAT('SPTWT2  ,',G23.15,',',G23.15,',',G23.15,
     1    ',',G23.15,',',G23.15)
          WRITE(OUTLYNE,3003) SYSTEM1(76),SYSTEM1(77),SYSTEM1(78)
     1    ,SYSTEM1(79),SYSTEM1(80)
          CALL SHOWIT(10)
C
C       IF THE LENS FILE HAS ANY CONFIGURATION DATA
C       IGNORE IT
C
C       IS THERE ANY SPSRF DATA
C
          SS=0
          DO 20 I=0,INT(SYSTEM1(20))
              IF(ALENS(34,I).NE.0.0) THEN
C       FOUND SPECIAL SURFACE DATA
                  SS=SS+1
              END IF
 20       CONTINUE
          IF(SS.NE.0) THEN
C       PRINT SPECIAL SURFACE DATA
              WRITE(OUTLYNE,200)
              CALL SHOWIT(10)
 200          FORMAT('SPSRF')
              DO 21  I=INT(SYSTEM1(20)),0
                  J=INT(SYSTEM1(20))-I
                  IF(ALENS(34,I).NE.0.0) THEN
                      WRITE(OUTLYNE,210) DBLE(J),ALENS(34,I)
                      CALL SHOWIT(10)
 210                  FORMAT('SPECIAL ,',G23.15,',',G23.15)
                  END IF
C
C       NOW WRITE THE COEFFICIENTS
C
                  IF(DABS(ALENS(34,I)).GE.1.0.AND.
     1            DABS(ALENS(34,I)).LE.30.0) THEN
                      IF(FTFL01(1,I).NE.0.0D0) WRITE(OUTLYNE,300) DBLE(I),FTFL01(1,I)
                      IF(FTFL01(1,I).NE.0.0D0) CALL SHOWIT(10)
                      IF(FTFL01(2,I).NE.0.0D0) WRITE(OUTLYNE,301) DBLE(I),FTFL01(2,I)
                      IF(FTFL01(2,I).NE.0.0D0) CALL SHOWIT(10)
                      IF(DABS(ALENS(34,I)).NE.12.0D0.AND.DABS(ALENS(34,I)).NE.13.0D0)
     1                 THEN
                          IF(FTFL01(3,I).NE.0.0D0) WRITE(OUTLYNE,302) DBLE(I),FTFL01(3,I)
                          IF(FTFL01(3,I).NE.0.0D0) CALL SHOWIT(10)
                          IF(FTFL01(4,I).NE.0.0D0) WRITE(OUTLYNE,303) DBLE(I),FTFL01(4,I)
                          IF(FTFL01(4,I).NE.0.0D0) CALL SHOWIT(10)
                          IF(FTFL01(5,I).NE.0.0D0) WRITE(OUTLYNE,304) DBLE(I),FTFL01(5,I)
                          IF(FTFL01(5,I).NE.0.0D0) CALL SHOWIT(10)
                      ELSE
                          WRITE(OUTLYNE,302) DBLE(I),FTFL01(3,I)
                          CALL SHOWIT(10)
                          WRITE(OUTLYNE,303) DBLE(I),FTFL01(4,I)
                          CALL SHOWIT(10)
                          WRITE(OUTLYNE,304) DBLE(I),FTFL01(5,I)
                          CALL SHOWIT(10)
                      END IF
                      IF(FTFL01(6,I).NE.0.0D0) WRITE(OUTLYNE,305) DBLE(I),FTFL01(6,I)
                      IF(FTFL01(6,I).NE.0.0D0) CALL SHOWIT(10)
                      IF(DABS(ALENS(34,I)).NE.12.0D0.AND.DABS(ALENS(34,I)).NE.13.0D0)
     1                 THEN
                          IF(FTFL01(7,I).NE.0.0D0) WRITE(OUTLYNE,306) DBLE(I),FTFL01(7,I)
                          IF(FTFL01(7,I).NE.0.0D0) CALL SHOWIT(10)
                          IF(FTFL01(8,I).NE.0.0D0) WRITE(OUTLYNE,307) DBLE(I),FTFL01(8,I)
                          IF(FTFL01(8,I).NE.0.0D0) CALL SHOWIT(10)
                          IF(FTFL01(9,I).NE.0.0D0) WRITE(OUTLYNE,308) DBLE(I),FTFL01(9,I)
                          IF(FTFL01(9,I).NE.0.0D0) CALL SHOWIT(10)
                      ELSE
                          WRITE(OUTLYNE,306) DBLE(I),FTFL01(7,I)
                          CALL SHOWIT(10)
                          WRITE(OUTLYNE,307) DBLE(I),FTFL01(8,I)
                          CALL SHOWIT(10)
                          WRITE(OUTLYNE,308) DBLE(I),FTFL01(9,I)
                          CALL SHOWIT(10)
                          IF(FTFL01(10,I).NE.0.0D0) WRITE(OUTLYNE,309) DBLE(I),FTFL01(10,I)
                          IF(FTFL01(10,I).NE.0.0D0) CALL SHOWIT(10)
                          IF(FTFL01(11,I).NE.0.0D0) WRITE(OUTLYNE,310) DBLE(I),FTFL01(11,I)
                          IF(FTFL01(11,I).NE.0.0D0) CALL SHOWIT(10)
                          IF(FTFL01(12,I).NE.0.0D0) WRITE(OUTLYNE,311) DBLE(I),FTFL01(12,I)
                          IF(FTFL01(12,I).NE.0.0D0) CALL SHOWIT(10)
                          IF(FTFL01(13,I).NE.0.0D0) WRITE(OUTLYNE,312) DBLE(I),FTFL01(13,I)
                          IF(FTFL01(13,I).NE.0.0D0) CALL SHOWIT(10)
                          IF(FTFL01(14,I).NE.0.0D0) WRITE(OUTLYNE,313) DBLE(I),FTFL01(14,I)
                          IF(FTFL01(14,I).NE.0.0D0) CALL SHOWIT(10)
                          IF(FTFL01(15,I).NE.0.0D0) WRITE(OUTLYNE,314) DBLE(I),FTFL01(15,I)
                          IF(FTFL01(15,I).NE.0.0D0) CALL SHOWIT(10)
                          IF(FTFL01(16,I).NE.0.0D0) WRITE(OUTLYNE,315) DBLE(I),FTFL01(16,I)
                          IF(FTFL01(16,I).NE.0.0D0) CALL SHOWIT(10)
                          IF(FTFL01(17,I).NE.0.0D0) WRITE(OUTLYNE,316) DBLE(I),FTFL01(17,I)
                          IF(FTFL01(17,I).NE.0.0D0) CALL SHOWIT(10)
                          IF(FTFL01(18,I).NE.0.0D0) WRITE(OUTLYNE,317) DBLE(I),FTFL01(18,I)
                          IF(FTFL01(18,I).NE.0.0D0) CALL SHOWIT(10)
                          IF(FTFL01(19,I).NE.0.0D0) WRITE(OUTLYNE,318) DBLE(I),FTFL01(19,I)
                          IF(FTFL01(19,I).NE.0.0D0) CALL SHOWIT(10)
                          IF(FTFL01(20,I).NE.0.0D0) WRITE(OUTLYNE,319) DBLE(I),FTFL01(20,I)
                          IF(FTFL01(20,I).NE.0.0D0) CALL SHOWIT(10)
                          IF(FTFL01(21,I).NE.0.0D0) WRITE(OUTLYNE,320) DBLE(I),FTFL01(21,I)
                          IF(FTFL01(21,I).NE.0.0D0) CALL SHOWIT(10)
                          IF(FTFL01(22,I).NE.0.0D0) WRITE(OUTLYNE,321) DBLE(I),FTFL01(22,I)
                          IF(FTFL01(22,I).NE.0.0D0) CALL SHOWIT(10)
                          IF(FTFL01(23,I).NE.0.0D0) WRITE(OUTLYNE,322) DBLE(I),FTFL01(23,I)
                          IF(FTFL01(23,I).NE.0.0D0) CALL SHOWIT(10)
                          IF(FTFL01(24,I).NE.0.0D0) WRITE(OUTLYNE,323) DBLE(I),FTFL01(24,I)
                          IF(FTFL01(24,I).NE.0.0D0) CALL SHOWIT(10)
                          IF(FTFL01(25,I).NE.0.0D0) WRITE(OUTLYNE,324) DBLE(I),FTFL01(25,I)
                          IF(FTFL01(25,I).NE.0.0D0) CALL SHOWIT(10)
                          IF(FTFL01(26,I).NE.0.0D0) WRITE(OUTLYNE,325) DBLE(I),FTFL01(26,I)
                          IF(FTFL01(26,I).NE.0.0D0) CALL SHOWIT(10)
                          IF(FTFL01(27,I).NE.0.0D0) WRITE(OUTLYNE,326) DBLE(I),FTFL01(27,I)
                          IF(FTFL01(27,I).NE.0.0D0) CALL SHOWIT(10)
                          IF(FTFL01(28,I).NE.0.0D0) WRITE(OUTLYNE,327) DBLE(I),FTFL01(28,I)
                          IF(FTFL01(28,I).NE.0.0D0) CALL SHOWIT(10)
                          IF(FTFL01(29,I).NE.0.0D0) WRITE(OUTLYNE,328) DBLE(I),FTFL01(29,I)
                          IF(FTFL01(29,I).NE.0.0D0) CALL SHOWIT(10)
                          IF(FTFL01(30,I).NE.0.0D0) WRITE(OUTLYNE,329) DBLE(I),FTFL01(30,I)
                          IF(FTFL01(30,I).NE.0.0D0) CALL SHOWIT(10)
                          IF(FTFL01(31,I).NE.0.0D0) WRITE(OUTLYNE,330) DBLE(I),FTFL01(31,I)
                          IF(FTFL01(31,I).NE.0.0D0) CALL SHOWIT(10)
                          IF(FTFL01(32,I).NE.0.0D0) WRITE(OUTLYNE,331) DBLE(I),FTFL01(32,I)
                          IF(FTFL01(32,I).NE.0.0D0) CALL SHOWIT(10)
                          IF(FTFL01(33,I).NE.0.0D0) WRITE(OUTLYNE,332) DBLE(I),FTFL01(33,I)
                          IF(FTFL01(33,I).NE.0.0D0) CALL SHOWIT(10)
                          IF(FTFL01(34,I).NE.0.0D0) WRITE(OUTLYNE,333) DBLE(I),FTFL01(34,I)
                          IF(FTFL01(34,I).NE.0.0D0) CALL SHOWIT(10)
                          IF(FTFL01(35,I).NE.0.0D0) WRITE(OUTLYNE,334) DBLE(I),FTFL01(35,I)
                          IF(FTFL01(35,I).NE.0.0D0) CALL SHOWIT(10)
                          IF(FTFL01(36,I).NE.0.0D0) WRITE(OUTLYNE,335) DBLE(I),FTFL01(36,I)
                          IF(FTFL01(36,I).NE.0.0D0) CALL SHOWIT(10)
                          IF(FTFL01(37,I).NE.0.0D0) WRITE(OUTLYNE,336) DBLE(I),FTFL01(37,I)
                          IF(FTFL01(37,I).NE.0.0D0) CALL SHOWIT(10)
                          IF(FTFL01(38,I).NE.0.0D0) WRITE(OUTLYNE,337) DBLE(I),FTFL01(38,I)
                          IF(FTFL01(38,I).NE.0.0D0) CALL SHOWIT(10)
                          IF(FTFL01(39,I).NE.0.0D0) WRITE(OUTLYNE,338) DBLE(I),FTFL01(39,I)
                          IF(FTFL01(39,I).NE.0.0D0) CALL SHOWIT(10)
                          IF(FTFL01(40,I).NE.0.0D0) WRITE(OUTLYNE,339) DBLE(I),FTFL01(40,I)
                          IF(FTFL01(40,I).NE.0.0D0) CALL SHOWIT(10)
                          IF(FTFL01(41,I).NE.0.0D0) WRITE(OUTLYNE,340) DBLE(I),FTFL01(41,I)
                          IF(FTFL01(41,I).NE.0.0D0) CALL SHOWIT(10)
                          IF(FTFL01(42,I).NE.0.0D0) WRITE(OUTLYNE,341) DBLE(I),FTFL01(42,I)
                          IF(FTFL01(42,I).NE.0.0D0) CALL SHOWIT(10)
                          IF(FTFL01(43,I).NE.0.0D0) WRITE(OUTLYNE,342) DBLE(I),FTFL01(43,I)
                          IF(FTFL01(43,I).NE.0.0D0) CALL SHOWIT(10)
                          IF(FTFL01(44,I).NE.0.0D0) WRITE(OUTLYNE,343) DBLE(I),FTFL01(44,I)
                          IF(FTFL01(44,I).NE.0.0D0) CALL SHOWIT(10)
                          IF(FTFL01(45,I).NE.0.0D0) WRITE(OUTLYNE,344) DBLE(I),FTFL01(45,I)
                          IF(FTFL01(45,I).NE.0.0D0) CALL SHOWIT(10)
                          IF(FTFL01(46,I).NE.0.0D0) WRITE(OUTLYNE,345) DBLE(I),FTFL01(46,I)
                          IF(FTFL01(46,I).NE.0.0D0) CALL SHOWIT(10)
                          IF(FTFL01(47,I).NE.0.0D0) WRITE(OUTLYNE,346) DBLE(I),FTFL01(47,I)
                          IF(FTFL01(47,I).NE.0.0D0) CALL SHOWIT(10)
                          IF(FTFL01(48,I).NE.0.0D0) WRITE(OUTLYNE,347) DBLE(I),FTFL01(48,I)
                          IF(FTFL01(48,I).NE.0.0D0) CALL SHOWIT(10)
                          IF(FTFL01(49,I).NE.0.0D0) WRITE(OUTLYNE,348) DBLE(I),FTFL01(49,I)
                          IF(FTFL01(49,I).NE.0.0D0) CALL SHOWIT(10)
                          IF(FTFL01(50,I).NE.0.0D0) WRITE(OUTLYNE,349) DBLE(I),FTFL01(50,I)
                          IF(FTFL01(50,I).NE.0.0D0) CALL SHOWIT(10)
                          IF(FTFL01(51,I).NE.0.0D0) WRITE(OUTLYNE,350) DBLE(I),FTFL01(51,I)
                          IF(FTFL01(51,I).NE.0.0D0) CALL SHOWIT(10)
                          IF(FTFL01(52,I).NE.0.0D0) WRITE(OUTLYNE,351) DBLE(I),FTFL01(52,I)
                          IF(FTFL01(52,I).NE.0.0D0) CALL SHOWIT(10)
                          IF(FTFL01(53,I).NE.0.0D0) WRITE(OUTLYNE,352) DBLE(I),FTFL01(53,I)
                          IF(FTFL01(53,I).NE.0.0D0) CALL SHOWIT(10)
                          IF(FTFL01(54,I).NE.0.0D0) WRITE(OUTLYNE,353) DBLE(I),FTFL01(54,I)
                          IF(FTFL01(54,I).NE.0.0D0) CALL SHOWIT(10)
                          IF(FTFL01(55,I).NE.0.0D0) WRITE(OUTLYNE,354) DBLE(I),FTFL01(55,I)
                          IF(FTFL01(55,I).NE.0.0D0) CALL SHOWIT(10)
                          IF(FTFL01(56,I).NE.0.0D0) WRITE(OUTLYNE,355) DBLE(I),FTFL01(56,I)
                          IF(FTFL01(56,I).NE.0.0D0) CALL SHOWIT(10)
                          IF(FTFL01(57,I).NE.0.0D0) WRITE(OUTLYNE,356) DBLE(I),FTFL01(57,I)
                          IF(FTFL01(57,I).NE.0.0D0) CALL SHOWIT(10)
                          IF(FTFL01(58,I).NE.0.0D0) WRITE(OUTLYNE,357) DBLE(I),FTFL01(58,I)
                          IF(FTFL01(58,I).NE.0.0D0) CALL SHOWIT(10)
                          IF(FTFL01(59,I).NE.0.0D0) WRITE(OUTLYNE,358) DBLE(I),FTFL01(59,I)
                          IF(FTFL01(59,I).NE.0.0D0) CALL SHOWIT(10)
                          IF(FTFL01(60,I).NE.0.0D0) WRITE(OUTLYNE,359) DBLE(I),FTFL01(60,I)
                          IF(FTFL01(60,I).NE.0.0D0) CALL SHOWIT(10)
                          IF(FTFL01(61,I).NE.0.0D0) WRITE(OUTLYNE,360) DBLE(I),FTFL01(61,I)
                          IF(FTFL01(61,I).NE.0.0D0) CALL SHOWIT(10)
                          IF(FTFL01(62,I).NE.0.0D0) WRITE(OUTLYNE,361) DBLE(I),FTFL01(62,I)
                          IF(FTFL01(62,I).NE.0.0D0) CALL SHOWIT(10)
                          IF(FTFL01(63,I).NE.0.0D0) WRITE(OUTLYNE,362) DBLE(I),FTFL01(63,I)
                          IF(FTFL01(63,I).NE.0.0D0) CALL SHOWIT(10)
                          IF(FTFL01(64,I).NE.0.0D0) WRITE(OUTLYNE,363) DBLE(I),FTFL01(64,I)
                          IF(FTFL01(64,I).NE.0.0D0) CALL SHOWIT(10)
                          IF(FTFL01(65,I).NE.0.0D0) WRITE(OUTLYNE,364) DBLE(I),FTFL01(65,I)
                          IF(FTFL01(65,I).NE.0.0D0) CALL SHOWIT(10)
                          IF(FTFL01(66,I).NE.0.0D0) WRITE(OUTLYNE,365) DBLE(I),FTFL01(66,I)
                          IF(FTFL01(66,I).NE.0.0D0) CALL SHOWIT(10)
                          IF(FTFL01(67,I).NE.0.0D0) WRITE(OUTLYNE,366) DBLE(I),FTFL01(67,I)
                          IF(FTFL01(67,I).NE.0.0D0) CALL SHOWIT(10)
                          IF(FTFL01(68,I).NE.0.0D0) WRITE(OUTLYNE,367) DBLE(I),FTFL01(68,I)
                          IF(FTFL01(68,I).NE.0.0D0) CALL SHOWIT(10)
                          IF(FTFL01(69,I).NE.0.0D0) WRITE(OUTLYNE,368) DBLE(I),FTFL01(69,I)
                          IF(FTFL01(69,I).NE.0.0D0) CALL SHOWIT(10)
                          IF(FTFL01(70,I).NE.0.0D0) WRITE(OUTLYNE,369) DBLE(I),FTFL01(70,I)
                          IF(FTFL01(70,I).NE.0.0D0) CALL SHOWIT(10)
                          IF(FTFL01(71,I).NE.0.0D0) WRITE(OUTLYNE,370) DBLE(I),FTFL01(71,I)
                          IF(FTFL01(71,I).NE.0.0D0) CALL SHOWIT(10)
                          IF(FTFL01(72,I).NE.0.0D0) WRITE(OUTLYNE,371) DBLE(I),FTFL01(72,I)
                          IF(FTFL01(72,I).NE.0.0D0) CALL SHOWIT(10)
                          IF(FTFL01(73,I).NE.0.0D0) WRITE(OUTLYNE,372) DBLE(I),FTFL01(73,I)
                          IF(FTFL01(73,I).NE.0.0D0) CALL SHOWIT(10)
                          IF(FTFL01(74,I).NE.0.0D0) WRITE(OUTLYNE,373) DBLE(I),FTFL01(74,I)
                          IF(FTFL01(74,I).NE.0.0D0) CALL SHOWIT(10)
                          IF(FTFL01(75,I).NE.0.0D0) WRITE(OUTLYNE,374) DBLE(I),FTFL01(75,I)
                          IF(FTFL01(75,I).NE.0.0D0) CALL SHOWIT(10)
                          IF(FTFL01(76,I).NE.0.0D0) WRITE(OUTLYNE,375) DBLE(I),FTFL01(76,I)
                          IF(FTFL01(76,I).NE.0.0D0) CALL SHOWIT(10)
                          IF(FTFL01(77,I).NE.0.0D0) WRITE(OUTLYNE,376) DBLE(I),FTFL01(77,I)
                          IF(FTFL01(77,I).NE.0.0D0) CALL SHOWIT(10)
                          IF(FTFL01(78,I).NE.0.0D0) WRITE(OUTLYNE,377) DBLE(I),FTFL01(78,I)
                          IF(FTFL01(78,I).NE.0.0D0) CALL SHOWIT(10)
                          IF(FTFL01(79,I).NE.0.0D0) WRITE(OUTLYNE,378) DBLE(I),FTFL01(79,I)
                          IF(FTFL01(79,I).NE.0.0D0) CALL SHOWIT(10)
                          IF(FTFL01(80,I).NE.0.0D0) WRITE(OUTLYNE,379) DBLE(I),FTFL01(80,I)
                          IF(FTFL01(80,I).NE.0.0D0) CALL SHOWIT(10)
                          IF(FTFL01(81,I).NE.0.0D0) WRITE(OUTLYNE,380) DBLE(I),FTFL01(81,I)
                          IF(FTFL01(81,I).NE.0.0D0) CALL SHOWIT(10)
                          IF(FTFL01(82,I).NE.0.0D0) WRITE(OUTLYNE,381) DBLE(I),FTFL01(82,I)
                          IF(FTFL01(82,I).NE.0.0D0) CALL SHOWIT(10)
                          IF(FTFL01(83,I).NE.0.0D0) WRITE(OUTLYNE,382) DBLE(I),FTFL01(83,I)
                          IF(FTFL01(83,I).NE.0.0D0) CALL SHOWIT(10)
                          IF(FTFL01(84,I).NE.0.0D0) WRITE(OUTLYNE,383) DBLE(I),FTFL01(84,I)
                          IF(FTFL01(84,I).NE.0.0D0) CALL SHOWIT(10)
                          IF(FTFL01(85,I).NE.0.0D0) WRITE(OUTLYNE,384) DBLE(I),FTFL01(85,I)
                          IF(FTFL01(85,I).NE.0.0D0) CALL SHOWIT(10)
                          IF(FTFL01(86,I).NE.0.0D0) WRITE(OUTLYNE,385) DBLE(I),FTFL01(86,I)
                          IF(FTFL01(86,I).NE.0.0D0) CALL SHOWIT(10)
                          IF(FTFL01(87,I).NE.0.0D0) WRITE(OUTLYNE,386) DBLE(I),FTFL01(87,I)
                          IF(FTFL01(87,I).NE.0.0D0) CALL SHOWIT(10)
                          IF(FTFL01(88,I).NE.0.0D0) WRITE(OUTLYNE,387) DBLE(I),FTFL01(88,I)
                          IF(FTFL01(88,I).NE.0.0D0) CALL SHOWIT(10)
                          IF(FTFL01(89,I).NE.0.0D0) WRITE(OUTLYNE,388) DBLE(I),FTFL01(89,I)
                          IF(FTFL01(89,I).NE.0.0D0) CALL SHOWIT(10)
                          IF(FTFL01(90,I).NE.0.0D0) WRITE(OUTLYNE,389) DBLE(I),FTFL01(90,I)
                          IF(FTFL01(90,I).NE.0.0D0) CALL SHOWIT(10)
                          IF(FTFL01(91,I).NE.0.0D0) WRITE(OUTLYNE,390) DBLE(I),FTFL01(91,I)
                          IF(FTFL01(91,I).NE.0.0D0) CALL SHOWIT(10)
                          IF(FTFL01(92,I).NE.0.0D0) WRITE(OUTLYNE,391) DBLE(I),FTFL01(92,I)
                          IF(FTFL01(92,I).NE.0.0D0) CALL SHOWIT(10)
                          IF(FTFL01(93,I).NE.0.0D0) WRITE(OUTLYNE,392) DBLE(I),FTFL01(93,I)
                          IF(FTFL01(93,I).NE.0.0D0) CALL SHOWIT(10)
                          IF(FTFL01(94,I).NE.0.0D0) WRITE(OUTLYNE,393) DBLE(I),FTFL01(94,I)
                          IF(FTFL01(94,I).NE.0.0D0) CALL SHOWIT(10)
                          IF(FTFL01(95,I).NE.0.0D0) WRITE(OUTLYNE,394) DBLE(I),FTFL01(95,I)
                          IF(FTFL01(95,I).NE.0.0D0) CALL SHOWIT(10)
                          IF(FTFL01(96,I).NE.0.0D0) WRITE(OUTLYNE,395) DBLE(I),FTFL01(96,I)
                          IF(FTFL01(96,I).NE.0.0D0) CALL SHOWIT(10)
                      END IF
                  END IF
 21           CONTINUE
              WRITE(OUTLYNE,10)
              CALL SHOWIT(10)
          END IF
 300      FORMAT('C1      ,',G23.15,',',G23.15)
 301      FORMAT('C2      ,',G23.15,',',G23.15)
 302      FORMAT('C3      ,',G23.15,',',G23.15)
 303      FORMAT('C4      ,',G23.15,',',G23.15)
 304      FORMAT('C5      ,',G23.15,',',G23.15)
 305      FORMAT('C6      ,',G23.15,',',G23.15)
 306      FORMAT('C7      ,',G23.15,',',G23.15)
 307      FORMAT('C8      ,',G23.15,',',G23.15)
 308      FORMAT('C9      ,',G23.15,',',G23.15)
 309      FORMAT('C10     ,',G23.15,',',G23.15)
 310      FORMAT('C11     ,',G23.15,',',G23.15)
 311      FORMAT('C12     ,',G23.15,',',G23.15)
 312      FORMAT('C13     ,',G23.15,',',G23.15)
 313      FORMAT('C14     ,',G23.15,',',G23.15)
 314      FORMAT('C15     ,',G23.15,',',G23.15)
 315      FORMAT('C16     ,',G23.15,',',G23.15)
 316      FORMAT('C17     ,',G23.15,',',G23.15)
 317      FORMAT('C18     ,',G23.15,',',G23.15)
 318      FORMAT('C19     ,',G23.15,',',G23.15)
 319      FORMAT('C20     ,',G23.15,',',G23.15)
 320      FORMAT('C21     ,',G23.15,',',G23.15)
 321      FORMAT('C22     ,',G23.15,',',G23.15)
 322      FORMAT('C23     ,',G23.15,',',G23.15)
 323      FORMAT('C24     ,',G23.15,',',G23.15)
 324      FORMAT('C25     ,',G23.15,',',G23.15)
 325      FORMAT('C26     ,',G23.15,',',G23.15)
 326      FORMAT('C27     ,',G23.15,',',G23.15)
 327      FORMAT('C28     ,',G23.15,',',G23.15)
 328      FORMAT('C29     ,',G23.15,',',G23.15)
 329      FORMAT('C30     ,',G23.15,',',G23.15)
 330      FORMAT('C31     ,',G23.15,',',G23.15)
 331      FORMAT('C32     ,',G23.15,',',G23.15)
 332      FORMAT('C33     ,',G23.15,',',G23.15)
 333      FORMAT('C34     ,',G23.15,',',G23.15)
 334      FORMAT('C35     ,',G23.15,',',G23.15)
 335      FORMAT('C36     ,',G23.15,',',G23.15)
 336      FORMAT('C37     ,',G23.15,',',G23.15)
 337      FORMAT('C38     ,',G23.15,',',G23.15)
 338      FORMAT('C39     ,',G23.15,',',G23.15)
 339      FORMAT('C40     ,',G23.15,',',G23.15)
 340      FORMAT('C41     ,',G23.15,',',G23.15)
 341      FORMAT('C42     ,',G23.15,',',G23.15)
 342      FORMAT('C43     ,',G23.15,',',G23.15)
 343      FORMAT('C44     ,',G23.15,',',G23.15)
 344      FORMAT('C45     ,',G23.15,',',G23.15)
 345      FORMAT('C46     ,',G23.15,',',G23.15)
 346      FORMAT('C47     ,',G23.15,',',G23.15)
 347      FORMAT('C48     ,',G23.15,',',G23.15)
 348      FORMAT('C49     ,',G23.15,',',G23.15)
 349      FORMAT('C50     ,',G23.15,',',G23.15)
 350      FORMAT('C51     ,',G23.15,',',G23.15)
 351      FORMAT('C52     ,',G23.15,',',G23.15)
 352      FORMAT('C53     ,',G23.15,',',G23.15)
 353      FORMAT('C54     ,',G23.15,',',G23.15)
 354      FORMAT('C55     ,',G23.15,',',G23.15)
 355      FORMAT('C56     ,',G23.15,',',G23.15)
 356      FORMAT('C57     ,',G23.15,',',G23.15)
 357      FORMAT('C58     ,',G23.15,',',G23.15)
 358      FORMAT('C59     ,',G23.15,',',G23.15)
 359      FORMAT('C60     ,',G23.15,',',G23.15)
 360      FORMAT('C61     ,',G23.15,',',G23.15)
 361      FORMAT('C62     ,',G23.15,',',G23.15)
 362      FORMAT('C63     ,',G23.15,',',G23.15)
 363      FORMAT('C64     ,',G23.15,',',G23.15)
 364      FORMAT('C65     ,',G23.15,',',G23.15)
 365      FORMAT('C66     ,',G23.15,',',G23.15)
 366      FORMAT('C67     ,',G23.15,',',G23.15)
 367      FORMAT('C68     ,',G23.15,',',G23.15)
 368      FORMAT('C69     ,',G23.15,',',G23.15)
 369      FORMAT('C70     ,',G23.15,',',G23.15)
 370      FORMAT('C71     ,',G23.15,',',G23.15)
 371      FORMAT('C72     ,',G23.15,',',G23.15)
 372      FORMAT('C73     ,',G23.15,',',G23.15)
 373      FORMAT('C74     ,',G23.15,',',G23.15)
 374      FORMAT('C75     ,',G23.15,',',G23.15)
 375      FORMAT('C76     ,',G23.15,',',G23.15)
 376      FORMAT('C77     ,',G23.15,',',G23.15)
 377      FORMAT('C78     ,',G23.15,',',G23.15)
 378      FORMAT('C79     ,',G23.15,',',G23.15)
 379      FORMAT('C80     ,',G23.15,',',G23.15)
 380      FORMAT('C81     ,',G23.15,',',G23.15)
 381      FORMAT('C82     ,',G23.15,',',G23.15)
 382      FORMAT('C83     ,',G23.15,',',G23.15)
 383      FORMAT('C84     ,',G23.15,',',G23.15)
 384      FORMAT('C85     ,',G23.15,',',G23.15)
 385      FORMAT('C86     ,',G23.15,',',G23.15)
 386      FORMAT('C87     ,',G23.15,',',G23.15)
 387      FORMAT('C88     ,',G23.15,',',G23.15)
 388      FORMAT('C89     ,',G23.15,',',G23.15)
 389      FORMAT('C90     ,',G23.15,',',G23.15)
 390      FORMAT('C91     ,',G23.15,',',G23.15)
 391      FORMAT('C92     ,',G23.15,',',G23.15)
 392      FORMAT('C93     ,',G23.15,',',G23.15)
 393      FORMAT('C94     ,',G23.15,',',G23.15)
 394      FORMAT('C95     ,',G23.15,',',G23.15)
 395      FORMAT('C96     ,',G23.15,',',G23.15)
C
          RETURN
      END
C SUB DOGTAG.FOR
      SUBROUTINE DOGTAG
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE DOGTAG WHICH CREATES THE IDTAG FOR A LENS
C       THIS CAN'T BE ISSUED FROM THE KEYBOARD. THE COMMAND ONLY
C       WORKS FROM THE TASK LEVEL WHEN INPUT IS NOT 5
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
          !       LOGICAL OPEN9
          CHARACTER NM1*13,NM2*13,NM3*13,NM4*13
          INTEGER K,L,M,N,I
C
C
          IDTAG(1)(1:75)=CNULL
          IDTAG(2)(1:75)=CNULL
          IDTAG(3)(1:75)=CNULL
          IDTAG(4)(1:75)=CNULL
          IDTAG(5)(1:75)=CNULL
          IDTAG(6)(1:75)=CNULL
          IDTAG(7)(1:75)=CNULL
          IDTAG(8)(1:75)=CNULL
          IDTAG(9)(1:75)=CNULL
          IDTAG(10)(1:75)=CNULL
C     NOW COMPUTE THE NEW IDTAG
C
C       LINE ONE IS THE FIRST 75 CHARACTERS OF THE LI
          IDTAG(1)(1:75)=LI(1:75)
C       LINE TWO IS THE FIRST 75 CHARACTERS OF THE MFG
          IDTAG(2)(1:75)=MFG(1:75)
C       LINE THREE IS THE FIRST 75 CHARACTERS OF THE CATNUM
          IDTAG(3)(1:75)=CATNUM(1:75)
C       LINE FOUR IS THE EFL IN G23.15 FORMAT FOLLOWED BY THE NUMBER OF
C       SURFACES NOT COUNTING 0 AND LAST SURFACE IN I3 FORMAT FOLLOWED BY
C       THE OVERALL LENGTH FROM SURFACE 1 TO SURFACE LAST SURF -1 IN G23.15 FORMAT.
          SAVE_KDP(25)=SAVEINPT(25)
          INPUT='FIRD QUIET'
          CALL PROCES
          WRITE(OUTLYNE,*) 'GET OAL 1',INT(SYSTEM1(20))-1
          INPUT(1:75)=OUTLYNE(1:75)
          CALL PROCES
          REST_KDP(25)=RESTINPT(25)
          WRITE(IDTAG(4),100) GPREG(1),INT(SYSTEM1(20)),REG(9)
 100      FORMAT(G23.15,I3,G23.15)
C       LINE FIVE IS THE MODE CODE 1=FOCAL,2=UFOCAL,3=AFOCAL, 4=UAFOCAL IN I1 FOLLOWED BY
C       THE UNITS CODE 1=IN, 2= CM, 3= MM, 4=METERS IN I1 FORMAT FOLLOWED BY THE CONTROL
C       WAVELENGTH IN G23.15,FOLLOWED BY THE PRIMARY WAVELENGTH PAIR EACH IN G23.15 FORMAT
          WRITE(IDTAG(5),101) INT(SYSTEM1(30)),INT(SYSTEM1(6)),
     1    SYSTEM1(INT(SYSTEM1(11))+110)
     2    ,SYSTEM1(INT(SYSTEM1(7))+110),SYSTEM1(INT(SYSTEM1(8))+110)
 101      FORMAT(I1,I1,G23.15,G23.15,G23.15)
C       LINE SIX IS SECONDARY WAVELENGTH PAIR WAVELENGTHS IN G23.15 FORMAT (EACH)
          WRITE(IDTAG(6),102) SYSTEM1(INT(SYSTEM1(9))+110),
     1    SYSTEM1(INT(SYSTEM1(8))+110)
 102      FORMAT(G23.15,G23.15)
C       LINE SEVEN FULL NAME OF THE FIRST NON-AIR, NON-REFLECTIVE GLASS
C       FOLLOWED BY THE SECOND FULL GLASS NAME EACH IN 2(A13) FORMAT
          K=0
          L=0
          M=0
          N=0
          DO I=0,INT(SYSTEM1(20))
              IF(GLANAM(I,1).EQ.'SCHOTT'.OR.GLANAM(I,1).EQ.'HOYA'.OR.
     1        GLANAM(I,1).EQ.'CORNIN'.OR.GLANAM(I,1).EQ.'CHANCE'.OR.
     1        GLANAM(I,1).EQ.'SCH2000'.OR.
     2        GLANAM(I,1).EQ.'HIKARI'.OR.GLANAM(I,1).EQ.'GLCAT'.OR.
     3        GLANAM(I,1).EQ.'MATL'.OR.GLANAM(I,1).EQ.'RUSSIAN'.OR.
     4        GLANAM(I,1).EQ.'GLA'.OR.GLANAM(I,1).EQ.'RADHARD'.OR.
     5        GLANAM(I,1).EQ.'USER'.OR.GLANAM(I,1).EQ.'OHARA') THEN
                  IF(GLANAM(I,2).NE.'AIR'.AND.GLANAM(I,2).NE.'REFL'.AND.
     1            GLANAM(I,2).NE.'REFLTIRO') THEN
                      IF(N.EQ.0) THEN
                          IF(M.EQ.0) THEN
                              IF(L.EQ.0) THEN
                                  IF(K.EQ.0) THEN
                                      K=I
                                      GO TO 10
                                  END IF
                                  L=I
                                  GO TO 10
                              END IF
                              M=I
                              GO TO 10
                          END IF
                          N=I
                          GO TO 10
                      END IF

                  END IF

              END IF
 10           CONTINUE
          END DO
          NM1=GLANAM(K,1)
          NM2=GLANAM(K,2)
          NM3=GLANAM(L,1)
          NM4=GLANAM(L,2)
          IF(NM1.EQ.'AIR') NM1='             '
          IF(NM2.EQ.'AIR') NM2='             '
          IF(NM3.EQ.'AIR') NM3='             '
          IF(NM4.EQ.'AIR') NM4='             '
          WRITE(IDTAG(7),103) NM1,NM2,NM3,NM4

 103      FORMAT(A13,A13,A13,A13)
C       LINE EIGHT FULL NAME OF THE THIRD NON-AIR, NON-REFLECTIVE GLASS
C       FOLLOWED BY THE FOURTH FULL GLASS NAME EACH IN 2(A13) FORMAT
          NM1=GLANAM(M,1)
          NM2=GLANAM(M,2)
          NM3=GLANAM(N,1)
          NM4=GLANAM(N,2)
          IF(NM1.EQ.'AIR') NM1='             '
          IF(NM2.EQ.'AIR') NM2='             '
          IF(NM3.EQ.'AIR') NM3='             '
          IF(NM4.EQ.'AIR') NM4='             '
          WRITE(IDTAG(8),103) NM1,NM2,NM3,NM4
C
C       WRITE(OUTLYNE,*)'1',IDTAG(1)
C       CALL SHOWIT(1)
C       WRITE(OUTLYNE,*)'2',IDTAG(2)
C       CALL SHOWIT(1)
C       WRITE(OUTLYNE,*)'3',IDTAG(3)
C       CALL SHOWIT(1)
C       WRITE(OUTLYNE,*)'4',IDTAG(4)
C       CALL SHOWIT(1)
C       WRITE(OUTLYNE,*)'5',IDTAG(5)
C       CALL SHOWIT(1)
C       WRITE(OUTLYNE,*)'6',IDTAG(6)
C       CALL SHOWIT(1)
C       WRITE(OUTLYNE,*)'7',IDTAG(7)
C       CALL SHOWIT(1)
C       WRITE(OUTLYNE,*)'8',IDTAG(8)
C       CALL SHOWIT(1)
C       WRITE(OUTLYNE,*)'9',IDTAG(9)
C       CALL SHOWIT(1)
C       WRITE(OUTLYNE,*)'10',IDTAG(10)
C       CALL SHOWIT(1)
          RETURN
      END
C SUB RLENHD.FOR
      SUBROUTINE RLENHD
C
          IMPLICIT NONE
C
C       SUBROUTINE LENHD HANDEL LENS HEADER INFO OUTPUT
C       DURING A LENO REVERSE. ACTS ON THE CURRENT LENS
C
          INTEGER I
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
C
C     OVERBOSE
          IF(SYSTEM1(101).EQ.1.0D0)
     1    WRITE(OUTLYNE,*)'OVERBOSE ON'
C
C       OUTPUT THE HEADER COMMAND (LENS)
          WRITE(OUTLYNE,10)
          CALL SHOWIT(10)
C
C       NOW THE LI
          IF(LI(1:20).NE.'                    ')
     1    WRITE(OUTLYNE,20) LI(1:79)
          IF(LI(1:20).NE.'                    ') CALL SHOWIT(10)
C
C       NOW LIC
C
          DO I=1,LICCNT
              IF(LIC(I)(1:20).NE.'                    ') THEN
                  WRITE(OUTLYNE,21) LIC(I)
                  CALL SHOWIT(10)
              END IF
          END DO
C
          IF(INNI(1:20).NE.'                    ') WRITE(OUTLYNE,121) INNI
          IF(INNI(1:20).NE.'                    ') CALL SHOWIT(10)
          IF(LLTYPE(1:5).NE.'     ') WRITE(OUTLYNE,1212) LLTYPE(1:5)
          IF(LLTYPE(1:5).NE.'     ') CALL SHOWIT(10)
C
          IF(INT(SYSTEM1(91)).NE.0) THEN
C       NOW AUTOFUNC
              WRITE(OUTLYNE,50) DBLE(INT(SYSTEM1(91)))
 50           FORMAT('AUTOFUNC,',G23.15)
              CALL SHOWIT(10)
          END IF
C
C       NOW WV
C
          WRITE(OUTLYNE,22)
     1    SYSTEM1(1),SYSTEM1(2),SYSTEM1(3),SYSTEM1(4),SYSTEM1(5)
          CALL SHOWIT(10)
C
C       NOW WV2
C
          WRITE(OUTLYNE,221)
     1    SYSTEM1(71),SYSTEM1(72),SYSTEM1(73),SYSTEM1(74),SYSTEM1(75)
          CALL SHOWIT(10)
C
! 6661   FORMAT('WV      ,',G23.15,',',G23.15)
! 6662   FORMAT('WV2     ,',G23.15,',',G23.15)
! 6663   FORMAT(G23.15,',',G23.15,',',G23.15)
C
C       NOW UNITS
          IF(SYSTEM1(6).EQ.1.0) WRITE(OUTLYNE,23)
          IF(SYSTEM1(6).EQ.2.0) WRITE(OUTLYNE,24)
          IF(SYSTEM1(6).EQ.3.0) WRITE(OUTLYNE,25)
          IF(SYSTEM1(6).EQ.4.0) WRITE(OUTLYNE,33)
          CALL SHOWIT(10)
C
C       NOW PRIMARY WAVELENGTH PAIR
          WRITE(OUTLYNE,26) INT(SYSTEM1(7)),INT(SYSTEM1(8))
          CALL SHOWIT(10)
C
C       SECONDARY WAVELENGTH PAIR
          WRITE(OUTLYNE,27) INT(SYSTEM1(9)),INT(SYSTEM1(10))
          CALL SHOWIT(10)
C
C       CONTROL WAVELENGTH
          WRITE(OUTLYNE,28) INT(SYSTEM1(11))
          CALL SHOWIT(10)
C
C       WRX
          WRITE(OUTLYNE,2992) SYSTEM1(85)
          CALL SHOWIT(10)
 2992     FORMAT('WRX     ,',G23.15)
C
C       WRY
          WRITE(OUTLYNE,2993) SYSTEM1(86)
          CALL SHOWIT(10)
 2993     FORMAT('WRY     ,',G23.15)
C
C       BDX
          IF(SYSTEM1(87).EQ.0.0D0) SYSTEM1(87)=0.001D0
          WRITE(OUTLYNE,2994) SYSTEM1(87)
          CALL SHOWIT(10)
 2994     FORMAT('BDX     ,',G23.15)
C
C
C       BDY
          IF(SYSTEM1(88).EQ.0.0D0) SYSTEM1(88)=0.001D0
          WRITE(OUTLYNE,2995) SYSTEM1(88)
          CALL SHOWIT(10)
 2995     FORMAT('BDY     ,',G23.15)
C
C       SAY, THIS IS THE PY VALUE AT SURFACE SYSTEM1(20)-1
          WRITE(OUTLYNE,29) PXTRAY(1,(INT(SYSTEM1(20)-1.0D0)))
          CALL SHOWIT(10)
C
C       SAX, THIS IS THE PX VALUE AT SURFACE SYSTEM1(20)-1
          WRITE(OUTLYNE,30) PXTRAX(1,(INT(SYSTEM1(20)-1.0D0)))
          CALL SHOWIT(10)
C
C       SCY (SCY FANG IS NOT DIRECTLY PASSED BY LENO)
C       THIS IS THE PCY
          WRITE(OUTLYNE,31) PXTRAY(5,INT(SYSTEM1(20)))
          CALL SHOWIT(10)
C
          IF(PXTRAY(5,INT(SYSTEM1(20))).NE.PXTRAX(5,INT(SYSTEM1(20)))) THEN
C     DO SCX
C       SCX (SCX FANG IS NOT DIRECTLY PASSED BY LENO)
              WRITE(OUTLYNE,32) PXTRAX(5,INT(SYSTEM1(20)))
              CALL SHOWIT(10)
          END IF
          RETURN
C
 10       FORMAT('LENS')
 20       FORMAT('LI,',A79)
 21       FORMAT('LIC,',A79)
 121      FORMAT('INI,',A79)
 1212     FORMAT('LTYPE,',A5)
 22       FORMAT('WV      ,',G23.15,',',G23.15,',',G23.15,',',G23.15
     1    ,',',G23.15)
 221      FORMAT('WV2     ,',G23.15,',',G23.15,',',G23.15,',',G23.15
     1    ,',',G23.15)
 23       FORMAT('UNITS    IN')
 24       FORMAT('UNITS    CM')
 25       FORMAT('UNITS    MM')
 33       FORMAT('UNITS    M ')
 26       FORMAT('PCW     ,',I2,',',I2)
 27       FORMAT('SCW     ,',I2,',',I2)
 28       FORMAT('CW      ,',I2)
 29       FORMAT('SAY     ,',G23.15)
 30       FORMAT('SAX     ,',G23.15)
 31       FORMAT('SCY     ,',G23.15)
 32       FORMAT('SCX     ,',G23.15)
      END
C SUB LENHDCV.FOR
      SUBROUTINE LENHDCV
C
          IMPLICIT NONE
C
C       SUBROUTINE LENHD HANDEL LENS HEADER INFO OUTPUT
C       DURING A LENO CV. ACTS ON THE CURRENT LENS
C
          CHARACTER LIO*80
C
          INTEGER IV
C
          REAL*8 VW(5)
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
C
C       OUTPUT THE HEADER COMMAND (LENS)
C
          WRITE(OUTLYNE,10)
          CALL SHOWIT(10)
          WRITE(OUTLYNE,11)
          CALL SHOWIT(10)
C
C
C       NOW THE LI
          LIO='TIT '//''''//LI(1:73)//''''
C
          IF(LI(1:20).NE.'                    ')
     1    WRITE(OUTLYNE,20) LIO(1:79)
          IF(LI(1:20).NE.'                    ') CALL SHOWIT(10)
C
C       NOW WV
C
          IV=0
          IF(SYSTEM1(1).NE.0.0D0) THEN
              IV=IV+1
              VW(IV)=SYSTEM1(1)*1000.0D0
          END IF
          IF(SYSTEM1(2).NE.0.0D0) THEN
              IV=IV+1
              VW(IV)=SYSTEM1(2)*1000.0D0
          END IF
          IF(SYSTEM1(3).NE.0.0D0) THEN
              IV=IV+1
              VW(IV)=SYSTEM1(3)*1000.0D0
          END IF
          IF(SYSTEM1(4).NE.0.0D0) THEN
              IV=IV+1
              VW(IV)=SYSTEM1(4)*1000.0D0
          END IF
          IF(SYSTEM1(5).NE.0.0D0) THEN
              IV=IV+1
              VW(IV)=SYSTEM1(5)*1000.0D0
          END IF
          IF(IV.EQ.1) THEN
              WRITE(OUTLYNE,41) VW(1)
          END IF
          IF(IV.EQ.2) THEN
              WRITE(OUTLYNE,42) VW(1),VW(2)
          END IF
          IF(IV.EQ.3) THEN
              WRITE(OUTLYNE,43) VW(1),VW(2),VW(3)
          END IF
          IF(IV.EQ.4) THEN
              WRITE(OUTLYNE,44) VW(1),VW(2),VW(3),VW(4)
          END IF
          IF(IV.EQ.5) THEN
              WRITE(OUTLYNE,45) VW(1),VW(2),VW(3),VW(4),VW(5)
          END IF
          CALL SHOWIT(10)
 41       FORMAT('WL ',G15.7)
 42       FORMAT('WL ',G15.7,1X,G15.7)
 43       FORMAT('WL ',G15.7,1X,G15.7,1X,G15.7)
 44       FORMAT('WL ',G15.7,1X,G15.7,1X,G15.7,1X,G15.7)
 45       FORMAT('WL ',G15.7,1X,G15.7,1X,G15.7,1X,G15.7,1X,G15.7)
C
C       NOW SPTWT
C
          IV=0
          IF(SYSTEM1(31).NE.0.0D0) THEN
              IV=IV+1
              VW(IV)=SYSTEM1(31)*100.0D0
          END IF
          IF(SYSTEM1(32).NE.0.0D0) THEN
              IV=IV+1
              VW(IV)=SYSTEM1(32)*100.0D0
          END IF
          IF(SYSTEM1(33).NE.0.0D0) THEN
              IV=IV+1
              VW(IV)=SYSTEM1(33)*100.0D0
          END IF
          IF(SYSTEM1(34).NE.0.0D0) THEN
              IV=IV+1
              VW(IV)=SYSTEM1(34)*100.0D0
          END IF
          IF(SYSTEM1(35).NE.0.0D0) THEN
              IV=IV+1
              VW(IV)=SYSTEM1(35)*100.0D0
          END IF
          IF(IV.EQ.1) THEN
              WRITE(OUTLYNE,61) VW(1)
          END IF
          IF(IV.EQ.2) THEN
              WRITE(OUTLYNE,62) VW(1),VW(2)
          END IF
          IF(IV.EQ.3) THEN
              WRITE(OUTLYNE,63) VW(1),VW(2),VW(3)
          END IF
          IF(IV.EQ.4) THEN
              WRITE(OUTLYNE,64) VW(1),VW(2),VW(3),VW(4)
          END IF
          IF(IV.EQ.5) THEN
              WRITE(OUTLYNE,65) VW(1),VW(2),VW(3),VW(4),VW(5)
          END IF
          CALL SHOWIT(10)
 61       FORMAT('WTW ',G15.7)
 62       FORMAT('WTW ',G15.7,1X,G15.7)
 63       FORMAT('WTW ',G15.7,1X,G15.7,1X,G15.7)
 64       FORMAT('WTW ',G15.7,1X,G15.7,1X,G15.7,1X,G15.7)
 65       FORMAT('WTW ',G15.7,1X,G15.7,1X,G15.7,1X,G15.7,1X,G15.7)
C
C       NOW UNITS
          IF(SYSTEM1(6).EQ.1.0) WRITE(OUTLYNE,23)
          IF(SYSTEM1(6).EQ.2.0) WRITE(OUTLYNE,24)
          IF(SYSTEM1(6).EQ.3.0) WRITE(OUTLYNE,25)
          CALL SHOWIT(10)
C
C       CONTROL WAVELENGTH
          WRITE(OUTLYNE,28) INT(SYSTEM1(11))
          CALL SHOWIT(10)
C
C       SAY

          WRITE(OUTLYNE,29) DABS(2.0D0*SYSTEM1(12))
          CALL SHOWIT(10)
C
          IF(SYSTEM1(94).EQ.0.0D0.AND.SYSTEM1(95).EQ.0.0D0.AND.
     1    SYSTEM1(98).EQ.0.0D0.AND.SYSTEM1(99).EQ.0.0D0) THEN
C     NOT IMAGE SPACE FIELD SPEC
              IF(SYSTEM1(18).NE.1.0D0) THEN
C       SCY (SCY FANG IS NOT DIRECTLY PASSED BY LENO)
                  WRITE(OUTLYNE,31) (0.0D0*SYSTEM1(14)),(0.7D0*SYSTEM1(14)),
     1            SYSTEM1(14)
                  CALL SHOWIT(10)
              ELSE
C     SCY FANG
                  WRITE(OUTLYNE,32) (0.0D0*SYSTEM1(21)),(0.7D0*SYSTEM1(21)),
     1            SYSTEM1(21)
                  CALL SHOWIT(10)
              END IF
          ELSE
C     IMAGE SPACE SPECIFICATION
C     NOT DONE YET
          END IF
C
 10       FORMAT('LENS')
 11       FORMAT('RDM')
 20       FORMAT(A79)
! 22     FORMAT('WL      ,',D15.7,',',D15.7,',',D15.7,',',D15.7
!     1  ,',',D15.7)
 23       FORMAT('DIM I')
 24       FORMAT('DIM C')
 25       FORMAT('DIM M')
 28       FORMAT('REF ',G23.15)
 29       FORMAT('EPD ',G23.15)
 31       FORMAT('YOB ',G23.15,1X,G23.15,1X,G23.15)
 32       FORMAT('YAN ',G23.15,1X,G23.15,1X,G23.15)
C
          RETURN
      END
C SUB LENHD.FOR
      SUBROUTINE LENHD
C
          IMPLICIT NONE
C
C       SUBROUTINE LENHD HANDEL LENS HEADER INFO OUTPUT
C       DURING A SIMPLE LENO. ACTS ON THE CURRENT LENS
C
          INTEGER I
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
C
C     OVERBOSE
          IF(SYSTEM1(101).EQ.1.0D0)
     1    WRITE(OUTLYNE,*)'OVERBOSE ON'
C
C       OUTPUT THE HEADER COMMAND (LENS)
C
          WRITE(OUTLYNE,10)
          CALL SHOWIT(10)
C
C
C       NOW THE LI
C
          IF(LI(1:20).NE.'                    ')
     1    WRITE(OUTLYNE,20) LI(1:79)
          IF(LI(1:20).NE.'                    ') CALL SHOWIT(10)
C
C       NOW LIC
C
          DO I=1,LICCNT
              IF(LIC(I)(1:20).NE.'                    ') THEN
                  WRITE(OUTLYNE,21) LIC(I)
                  CALL SHOWIT(10)
              END IF
          END DO
C
          IF(MFG(1:20).NE.'                    ') THEN
C     MFG
              WRITE(OUTLYNE,8768) MFG
 8768         FORMAT('MFG,',A75)
              CALL SHOWIT(10)
C     CATNUM
              DO I=2,80
                  IF(LI(I:I).EQ.' ') THEN
                      CATNUM=LI(1:I-1)
                      GO TO 8767
                  END IF
              END DO
 8767         CONTINUE
              WRITE(OUTLYNE,8769) CATNUM
 8769         FORMAT('CATNUM,',A75)
              CALL SHOWIT(10)
          END IF
          IF(INNI(1:20).NE.'                    ') WRITE(OUTLYNE,121) INNI
          IF(INNI(1:20).NE.'                    ') CALL SHOWIT(10)
          IF(LLTYPE(1:5).NE.'     ') WRITE(OUTLYNE,1212) LLTYPE(1:5)
          IF(LLTYPE(1:5).NE.'     ') CALL SHOWIT(10)
C
          IF(INT(SYSTEM1(91)).NE.0) THEN
C       NOW AUTOFUNC
C
              WRITE(OUTLYNE,50) DBLE(INT(SYSTEM1(91)))
 50           FORMAT('AUTOFUNC,',G23.15)
              CALL SHOWIT(10)
          END IF
C
C
C       NOW WV
C
          WRITE(OUTLYNE,22)
     1    SYSTEM1(1),SYSTEM1(2),SYSTEM1(3),SYSTEM1(4),SYSTEM1(5)
          CALL SHOWIT(10)
C
C       NOW WV2
C
          WRITE(OUTLYNE,221)
     1    SYSTEM1(71),SYSTEM1(72),SYSTEM1(73),SYSTEM1(74),SYSTEM1(75)
          CALL SHOWIT(10)
C
C       NOW UNITS
          IF(SYSTEM1(6).EQ.1.0) WRITE(OUTLYNE,23)
          IF(SYSTEM1(6).EQ.2.0) WRITE(OUTLYNE,24)
          IF(SYSTEM1(6).EQ.3.0) WRITE(OUTLYNE,25)
          IF(SYSTEM1(6).EQ.4.0) WRITE(OUTLYNE,33)
          CALL SHOWIT(10)
C
C       NOW PRIMARY WAVELENGTH PAIR
          WRITE(OUTLYNE,26) INT(SYSTEM1(7)),INT(SYSTEM1(8))
          CALL SHOWIT(10)
C
C       SECONDARY WAVELENGTH PAIR
          WRITE(OUTLYNE,27) INT(SYSTEM1(9)),INT(SYSTEM1(10))
          CALL SHOWIT(10)
C
C       CONTROL WAVELENGTH
          WRITE(OUTLYNE,28) INT(SYSTEM1(11))
          CALL SHOWIT(10)
C
C       WRX
          WRITE(OUTLYNE,2992) SYSTEM1(85)
          CALL SHOWIT(10)
 2992     FORMAT('WRX     ,',G23.15)
C
C
C       WRY
          WRITE(OUTLYNE,2993) SYSTEM1(86)
          CALL SHOWIT(10)
 2993     FORMAT('WRY     ,',G23.15)
C
C       BDX
          IF(SYSTEM1(87).EQ.0.0D0) SYSTEM1(87)=0.001D0
          WRITE(OUTLYNE,2994) SYSTEM1(87)
          CALL SHOWIT(10)
 2994     FORMAT('BDX     ,',G23.15)
C
C
C       BDY
          IF(SYSTEM1(88).EQ.0.0D0) SYSTEM1(88)=0.001D0
          WRITE(OUTLYNE,2995) SYSTEM1(88)
          CALL SHOWIT(10)
 2995     FORMAT('BDY     ,',G23.15)
C
C
C       SAY
          IF(SYSTEM1(64).EQ.0.0D0.AND.SYSTEM1(67).EQ.0.0D0) THEN
              IF(SYSTEM1(83).EQ.0.0D0) WRITE(OUTLYNE,29) SYSTEM1(12)
              IF(SYSTEM1(83).NE.0.0D0) WRITE(OUTLYNE,292)
              CALL SHOWIT(10)
          ELSE
              IF(SYSTEM1(64).EQ.1.0D0.OR.SYSTEM1(64).EQ.3.0D0)
     1        WRITE(OUTLYNE,2929) SYSTEM1(65)
              IF(SYSTEM1(64).EQ.1.0D0.OR.SYSTEM1(64).EQ.3.0D0)
     1        CALL SHOWIT(10)
              IF(SYSTEM1(67).EQ.1.0D0.OR.SYSTEM1(67).EQ.3.0D0)
     1        WRITE(OUTLYNE,2930) SYSTEM1(68)
              IF(SYSTEM1(67).EQ.1.0D0.OR.SYSTEM1(67).EQ.3.0D0)
     1        CALL SHOWIT(10)
          END IF
C
C       SAX
          IF(SYSTEM1(64).EQ.0.0D0.AND.SYSTEM1(67).EQ.0.0D0) THEN
              IF(SYSTEM1(13).NE.SYSTEM1(12)) THEN
                  IF(SYSTEM1(84).EQ.0.0D0) WRITE(OUTLYNE,30) SYSTEM1(13)
                  IF(SYSTEM1(84).EQ.0.0D0) CALL SHOWIT(10)
                  IF(SYSTEM1(84).NE.0.0D0) WRITE(OUTLYNE,3022)
                  IF(SYSTEM1(84).NE.0.0D0) CALL SHOWIT(10)
              END IF
          ELSE
              IF(SYSTEM1(64).EQ.2.0D0.OR.SYSTEM1(64).EQ.3.0D0)
     1        WRITE(OUTLYNE,3030) SYSTEM1(66)
              IF(SYSTEM1(64).EQ.2.0D0.OR.SYSTEM1(64).EQ.3.0D0)
     1        CALL SHOWIT(10)
              IF(SYSTEM1(67).EQ.2.0D0.OR.SYSTEM1(67).EQ.3.0D0)
     1        WRITE(OUTLYNE,3031) SYSTEM1(69)
              IF(SYSTEM1(67).EQ.2.0D0.OR.SYSTEM1(67).EQ.3.0D0)
     1        CALL SHOWIT(10)
          END IF
C
          IF(SYSTEM1(94).EQ.0.0D0.AND.SYSTEM1(95).EQ.0.0D0.AND.
     1    SYSTEM1(98).EQ.0.0D0.AND.SYSTEM1(99).EQ.0.0D0) THEN
              IF(SYSTEM1(18).NE.1.0D0) THEN
C       SCY (SCY FANG IS NOT DIRECTLY PASSED BY LENO)
                  WRITE(OUTLYNE,31) SYSTEM1(14),SYSTEM1(15)
                  CALL SHOWIT(10)
              ELSE
C     SCY FANG
                  WRITE(OUTLYNE,1319) SYSTEM1(21),SYSTEM1(22)
 1319             FORMAT('SCY FANG,',G23.15,',',G23.15)
                  CALL SHOWIT(10)
              END IF
C
              IF(SYSTEM1(19).NE.1.0D0) THEN
                  IF(SYSTEM1(14).NE.SYSTEM1(16).OR.SYSTEM1(15).NE.SYSTEM1(17)) THEN
C     DO SCX
C       SCX (SCX FANG IS NOT DIRECTLY PASSED BY LENO)
                      WRITE(OUTLYNE,32) SYSTEM1(16),SYSTEM1(17)
                      CALL SHOWIT(10)
                  END IF
              ELSE
C     PASS SCX FANG
                  IF(SYSTEM1(21).NE.SYSTEM1(23).OR.SYSTEM1(22).NE.SYSTEM1(24)) THEN
C     DO SCX FANG
 1320                 FORMAT('SCX FANG,',G23.15,',',G23.15)
                      WRITE(OUTLYNE,1320) SYSTEM1(23),SYSTEM1(24)
                      CALL SHOWIT(10)
                  END IF
              END IF
          ELSE
              IF(SYSTEM1(94).NE.0.0D0) THEN
C     PXIM OR PXIM FANG
                  IF(SYSTEM1(94).EQ.-1.0D0) THEN
                      WRITE(OUTLYNE,331) SYSTEM1(92)
                  ELSE
                      WRITE(OUTLYNE,332) SYSTEM1(92)
                  END IF
                  CALL SHOWIT(10)
              END IF
              IF(SYSTEM1(95).NE.0.0D0) THEN
C     PYIM OR PYIM FANG
                  IF(SYSTEM1(95).EQ.-1.0D0) THEN
                      WRITE(OUTLYNE,341) SYSTEM1(93)
                  ELSE
                      WRITE(OUTLYNE,342) SYSTEM1(93)
                  END IF
                  CALL SHOWIT(10)
              END IF
              IF(SYSTEM1(98).NE.0.0D0) THEN
C     RXIM OR RXIM FANG
                  IF(SYSTEM1(98).EQ.-1.0D0) THEN
                      WRITE(OUTLYNE,351) SYSTEM1(96)
                  ELSE
                      WRITE(OUTLYNE,352) SYSTEM1(96)
                  END IF
                  CALL SHOWIT(10)
              END IF
              IF(SYSTEM1(99).NE.0.0D0) THEN
C     RYIM OR RYIM FANG
                  IF(SYSTEM1(99).EQ.-1.0D0) THEN
                      WRITE(OUTLYNE,361) SYSTEM1(97)
                  ELSE
                      WRITE(OUTLYNE,362) SYSTEM1(97)
                  END IF
                  CALL SHOWIT(10)
              END IF
          END IF
          IF(SYSTEM1(99).NE.0.0D0.AND.SYSTEM1(98).NE.0.0D0.AND.SYSTEM1(100)
     1    .NE.0.0D0) THEN
              WRITE(OUTLYNE,363)
 363          FORMAT('REVRAY ON')
              CALL SHOWIT(10)
          END IF
C
 10       FORMAT('LENS')
 20       FORMAT('LI,',A79)
 21       FORMAT('LIC,',A79)
 121      FORMAT('INI,',A79)
 1212     FORMAT('LTYPE,',A5)
C
! 6661   FORMAT('WV      ,',G23.15,',',G23.15)
! 6662   FORMAT('WV2     ,',G23.15,',',G23.15)
! 6663   FORMAT(G23.15,',',G23.15,',',G23.15)
C
 22       FORMAT('WV      ,',G23.15,',',G23.15,',',G23.15,',',G23.15
     1    ,',',G23.15)
 221      FORMAT('WV2     ,',G23.15,',',G23.15,',',G23.15,',',G23.15
     1    ,',',G23.15)
 23       FORMAT('UNITS    IN')
 24       FORMAT('UNITS    CM')
 25       FORMAT('UNITS    MM')
 33       FORMAT('UNITS    M ')
 26       FORMAT('PCW     ,',I2,',',I2)
 27       FORMAT('SCW     ,',I2,',',I2)
 28       FORMAT('CW      ,',I2)
 29       FORMAT('SAY     ,',G23.15)
 292      FORMAT('SAY      FLOAT    ')
 30       FORMAT('SAX     ,',G23.15)
 3022     FORMAT('SAX      FLOAT   ,')
 2929     FORMAT('NAOY    ,',G23.15)
 3030     FORMAT('NAOX    ,',G23.15)
 2930     FORMAT('FNOY    ,',G23.15)
 3031     FORMAT('FNOX    ,',G23.15)
 31       FORMAT('SCY     ,',G23.15,',',G23.15)
 32       FORMAT('SCX     ,',G23.15,',',G23.15)
 331      FORMAT('PXIM    ',G23.15)
 341      FORMAT('PYIM    ',G23.15)
 351      FORMAT('RXIM    ',G23.15)
 361      FORMAT('RYIM    ',G23.15)
 332      FORMAT('PXIM     FANG    ,',G23.15)
 342      FORMAT('PYIM     FANG    ,',G23.15)
 352      FORMAT('RXIM     FANG    ,',G23.15)
 362      FORMAT('RYIM     FANG    ,',G23.15)
          RETURN
      END
C SUB PIKSLV.FOR
      SUBROUTINE PIKSLV(I)
C
          IMPLICIT NONE
C
C       SUBROUTINE PIKSLV HANDELS LENS SURFACE PIKUPS
C       DURING A LENO AND
C       ACTS ON THE CURRENT LENS, ONLY
C       ONE SURFACE AT A TIME.
C       THE VARIABLE I PASSES THE SURFACE
C       NUMBER OF INTEREST
C
          INTEGER I
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
C
C       THE CURRENT SURFACE IS PASSED WITH I
C       A DUMMY SURFACE HAS AIR AS ITS MATERIAL AND HAS
C       AIR AS THE MATERIAL INFRONT OF IT.
C
 401      FORMAT
     1    ('C THE FOLLOWING PIKUP/SOLVE DATA REFERS TO SURFACE #',I1)
 402      FORMAT
     1    ('C THE FOLLOWING PIKUP/SOLVE DATA REFERS TO SURFACE #',I2)
 403      FORMAT
     1    ('C THE FOLLOWING PIKUP/SOLVE DATA REFERS TO SURFACE #',I3)
C     SURFACE LABEL
          IF(I.LT.10) WRITE(OUTLYNE,401) I
          IF(I.GE.10.AND.I.LT.100) WRITE(OUTLYNE,402) I
          IF(I.GE.100) WRITE(OUTLYNE,403) I
          CALL SHOWIT(10)
C       PIKUPS
C
C       RD PIKUP
          IF(PIKUP(1,I,1).EQ.1.0D0) THEN
              WRITE(OUTLYNE,1) PIKUP(2,I,1),PIKUP(3,I,1),PIKUP(4,I,1)
     1        ,PIKUP(6,I,1)
              CALL SHOWIT(10)
 1            FORMAT('PIKUP    RD      ,',G23.15,',',G23.15,',',G23.15
     1        ,',,',G23.15)
          END IF
C       CV PIKUP
          IF(PIKUP(1,I,2).EQ.1.0D0) THEN
              WRITE(OUTLYNE,2) PIKUP(2,I,2),PIKUP(3,I,2),PIKUP(4,I,2)
     1        ,PIKUP(6,I,2)
              CALL SHOWIT(10)
 2            FORMAT('PIKUP    CV      ,',G23.15,',',G23.15,
     1        ',',G23.15,',,',G23.15)
          END IF
C
C       TH PIKUP
          IF(PIKUP(1,I,3).EQ.1.0D0) THEN
              WRITE(OUTLYNE,3) PIKUP(2,I,3),PIKUP(3,I,3),PIKUP(4,I,3)
     1        ,PIKUP(6,I,3)
              CALL SHOWIT(10)
 3            FORMAT('PIKUP    TH      ,',G23.15,',',G23.15,',',G23.15,
     1        ',,',G23.15)
          END IF
C
C       THOAL PIKUP
          IF(PIKUP(1,I,32).EQ.1.0D0) THEN
              WRITE(OUTLYNE,901) PIKUP(2,I,32),PIKUP(3,I,32),PIKUP(4,I,32)
     1        ,PIKUP(5,I,32),PIKUP(6,I,32)
              CALL SHOWIT(10)
 901          FORMAT('PIKUP    THOAL   ,',G23.15,',',G23.15,',',
     1        G23.15,',',G23.15,',',G23.15)
          END IF
C       PIVX PIKUP
          IF(PIKUP(1,I,34).EQ.1.0D0) THEN
              WRITE(OUTLYNE,902) PIKUP(2,I,34),PIKUP(3,I,34),PIKUP(4,I,34)
     1        ,PIKUP(6,I,34)
              CALL SHOWIT(10)
 902          FORMAT('PIKUP    PIVX    ,',G23.15,',',G23.15,',',
     1        G23.15,',,',G23.15)
          END IF
C       PIVX PIKUP
          IF(PIKUP(1,I,35).EQ.1.0D0) THEN
              WRITE(OUTLYNE,904) PIKUP(2,I,35),PIKUP(3,I,35),PIKUP(4,I,35)
     1        ,PIKUP(6,I,35)
              CALL SHOWIT(10)
 904          FORMAT('PIKUP    PIVY    ,',G23.15,',',G23.15,',',
     1        G23.15,',,',G23.15)
          END IF
C       PIVZ PIKUP
          IF(PIKUP(1,I,36).EQ.1.0D0) THEN
              WRITE(OUTLYNE,906) PIKUP(2,I,36),PIKUP(3,I,36),PIKUP(4,I,36)
     1        ,PIKUP(6,I,36)
              CALL SHOWIT(10)
 906          FORMAT('PIKUP    PIVZ    ,',G23.15,',',G23.15,',',
     1        G23.15,',,',G23.15)
          END IF
C       GDX PIKUP
          IF(PIKUP(1,I,37).EQ.1.0D0) THEN
              WRITE(OUTLYNE,908) PIKUP(2,I,37),PIKUP(3,I,37),PIKUP(4,I,37)
     1        ,PIKUP(6,I,37)
              CALL SHOWIT(10)
 908          FORMAT('PIKUP    GDX    ,',G23.15,',',G23.15,',',
     1        G23.15,',,',G23.15)
          END IF
C       GDY PIKUP
          IF(PIKUP(1,I,38).EQ.1.0D0) THEN
              WRITE(OUTLYNE,910) PIKUP(2,I,38),PIKUP(3,I,38),PIKUP(4,I,38)
     1        ,PIKUP(6,I,38)
              CALL SHOWIT(10)
 910          FORMAT('PIKUP    GDY    ,',G23.15,',',G23.15,',',
     1        G23.15,',,',G23.15)
          END IF
C       GDZ PIKUP
          IF(PIKUP(1,I,39).EQ.1.0D0) THEN
              WRITE(OUTLYNE,912) PIKUP(2,I,39),PIKUP(3,I,39),PIKUP(4,I,39)
     1        ,PIKUP(6,I,39)
              CALL SHOWIT(10)
 912          FORMAT('PIKUP    GDZ     ,',G23.15,',',G23.15,',',
     1        G23.15,',,',G23.15)
          END IF
C       GALPHA PIKUP
          IF(PIKUP(1,I,40).EQ.1.0D0) THEN
              WRITE(OUTLYNE,920) PIKUP(2,I,40),PIKUP(3,I,40),PIKUP(4,I,40)
     1        ,PIKUP(6,I,40)
              CALL SHOWIT(10)
 920          FORMAT('PIKUP    GALPHA  ,',G23.15,',',G23.15,',',
     1        G23.15,',,',G23.15)
          END IF
C       GBETA PIKUP
          IF(PIKUP(1,I,41).EQ.1.0D0) THEN
              WRITE(OUTLYNE,916) PIKUP(2,I,41),PIKUP(3,I,41),PIKUP(4,I,41)
     1        ,PIKUP(6,I,41)
              CALL SHOWIT(10)
 916          FORMAT('PIKUP    GBETA   ,',G23.15,',',G23.15,',',
     1        G23.15,',,',G23.15)
          END IF
C       GGAMMA PIKUP
          IF(PIKUP(1,I,42).EQ.1.0D0) THEN
              WRITE(OUTLYNE,918) PIKUP(2,I,42),PIKUP(3,I,42),PIKUP(4,I,42)
     1        ,PIKUP(6,I,42)
              CALL SHOWIT(10)
 918          FORMAT('PIKUP    GGAMMA  ,',G23.15,',',G23.15,',',
     1        G23.15,',,',G23.15)
          END IF
C       CC PIKUP
          IF(PIKUP(1,I,4).EQ.1.0D0) THEN
              WRITE(OUTLYNE,4) PIKUP(2,I,4),PIKUP(3,I,4),PIKUP(4,I,4)
     1        ,PIKUP(6,I,4)
              CALL SHOWIT(10)
 4            FORMAT('PIKUP    CC      ,',G23.15,',',G23.15,',',G23.15,
     1        ',,',G23.15)
          END IF
C
C       AD PIKUP
          IF(PIKUP(1,I,5).EQ.1.0D0) THEN
              WRITE(OUTLYNE,5) PIKUP(2,I,5),PIKUP(3,I,5),PIKUP(4,I,5)
     1        ,PIKUP(6,I,5)
              CALL SHOWIT(10)
 5            FORMAT('PIKUP    AD      ,',G23.15,',',G23.15,',',G23.15
     1        ,',,',G23.15)
          END IF
C
C       AE PIKUP
          IF(PIKUP(1,I,6).EQ.1.0D0) THEN
              WRITE(OUTLYNE,6) PIKUP(2,I,6),PIKUP(3,I,6),PIKUP(4,I,6)
     1        ,PIKUP(6,I,6)
              CALL SHOWIT(10)
 6            FORMAT('PIKUP    AE      ,',G23.15,',',G23.15,',',G23.15
     1        ,',,',G23.15)
          END IF
C
C       AF PIKUP
          IF(PIKUP(1,I,7).EQ.1.0D0) THEN
              WRITE(OUTLYNE,7) PIKUP(2,I,7),PIKUP(3,I,7),PIKUP(4,I,7)
     1        ,PIKUP(6,I,7)
              CALL SHOWIT(10)
 7            FORMAT('PIKUP    AF      ,',G23.15,',',G23.15,',',G23.15
     1        ,',,',G23.15)
          END IF
C
C       AG PIKUP
          IF(PIKUP(1,I,8).EQ.1.0D0) THEN
              WRITE(OUTLYNE,8) PIKUP(2,I,8),PIKUP(3,I,8),PIKUP(4,I,8)
     1        ,PIKUP(6,I,8)
              CALL SHOWIT(10)
 8            FORMAT('PIKUP    AG      ,',G23.15,',',G23.15,',',G23.15
     1        ,',,',G23.15)
          END IF
C
C       AH PIKUP
          IF(PIKUP(1,I,27).EQ.1.0D0) THEN
              WRITE(OUTLYNE,8) PIKUP(2,I,27),PIKUP(3,I,27),PIKUP(4,I,27)
     1        ,PIKUP(6,I,27)
              CALL SHOWIT(10)
! 801    FORMAT('PIKUP    AH      ,',G23.15,',',G23.15,',',G23.15
!     1,',,',G23.15)
          END IF
C
C       AI PIKUP
          IF(PIKUP(1,I,28).EQ.1.0D0) THEN
              WRITE(OUTLYNE,8) PIKUP(2,I,28) ,PIKUP(3,I,28),PIKUP(4,I,28)
     1        ,PIKUP(6,I,28)
              CALL SHOWIT(10)
! 802    FORMAT('PIKUP    AI      ,',G23.15,',',G23.15,',',G23.15
!     1,',,',G23.15)
          END IF
C
C       AJ PIKUP
          IF(PIKUP(1,I,29).EQ.1.0D0) THEN
              WRITE(OUTLYNE,8) PIKUP(2,I,29),PIKUP(3,I,29),PIKUP(4,I,29)
     1        ,PIKUP(6,I,29)
              CALL SHOWIT(10)
! 803    FORMAT('PIKUP    AJ      ,',G23.15,',',G23.15,',',G23.15
!     1,',,',G23.15)
          END IF
C
C       AK PIKUP
          IF(PIKUP(1,I,30).EQ.1.0D0) THEN
              WRITE(OUTLYNE,8) PIKUP(2,I,30),PIKUP(3,I,30),PIKUP(4,I,30)
     1        ,PIKUP(6,I,30)
              CALL SHOWIT(10)
! 804    FORMAT('PIKUP    AK      ,',G23.15,',',G23.15,',',G23.15
!     1,',,',G23.15)
          END IF
C
C       AL PIKUP
          IF(PIKUP(1,I,31).EQ.1.0D0) THEN
              WRITE(OUTLYNE,8) PIKUP(2,I,31),PIKUP(3,I,31),PIKUP(4,I,31)
     1        ,PIKUP(6,I,31)
              CALL SHOWIT(10)
! 805    FORMAT('PIKUP    AL      ,',G23.15,',',G23.15,',',G23.15
!     1,',,',G23.15)
          END IF
C
C       CVTOR PIKUP
          IF(PIKUP(1,I,9).EQ.1.0D0) THEN
              WRITE(OUTLYNE,9) PIKUP(2,I,9),PIKUP(3,I,9),PIKUP(4,I,9)
     1        ,PIKUP(6,I,9)
              CALL SHOWIT(10)
 9            FORMAT('PIKUP    CVTOR   ,',G23.15,',',G23.15,',',G23.15
     1        ,',,',G23.15)
          END IF
C
C       RDTOR PIKUP
          IF(PIKUP(1,I,10).EQ.1.0D0) THEN
              WRITE(OUTLYNE,10) PIKUP(2,I,10),PIKUP(3,I,10),PIKUP(4,I,10)
     1        ,PIKUP(6,I,10)
              CALL SHOWIT(10)
 10           FORMAT('PIKUP    RDTOR   ,',G23.15,',',G23.15,',',G23.15
     1        ,',,',G23.15)
          END IF
C
C       PRO PIKUP
          IF(PIKUP(1,I,11).EQ.1.0D0) THEN
              WRITE(OUTLYNE,11) PIKUP(2,I,11),PIKUP(6,I,11)
              CALL SHOWIT(10)
 11           FORMAT('PIKUP    PRO     ,',G23.15,',,,,',G23.15)
          END IF
C
C       NPRO PIKUP
          IF(PIKUP(1,I,12).EQ.1.0D0) THEN
              WRITE(OUTLYNE,12) PIKUP(2,I,12),PIKUP(6,I,12)
              CALL SHOWIT(10)
 12           FORMAT('PIKUP    NPRO    ,',G23.15,',,,,',G23.15)
          END IF
C
C       YD PIKUP
          IF(PIKUP(1,I,13).EQ.1.0D0) THEN
              WRITE(OUTLYNE,13) PIKUP(2,I,13),PIKUP(3,I,13),PIKUP(4,I,13)
     1        ,PIKUP(6,I,13)
              CALL SHOWIT(10)
 13           FORMAT('PIKUP    YD      ,',G23.15,',',G23.15,',',G23.15
     1        ,',,',G23.15)
          END IF
C
C       XD PIKUP
          IF(PIKUP(1,I,14).EQ.1.0D0) THEN
              WRITE(OUTLYNE,14) PIKUP(2,I,14),PIKUP(3,I,14),PIKUP(4,I,14)
     1        ,PIKUP(6,I,14)
              CALL SHOWIT(10)
  14          FORMAT('PIKUP    XD      ,',G23.15,',',G23.15,',',G23.15
     1        ,',,',G23.15)
          END IF
C
C
C       ZD PIKUP
          IF(PIKUP(1,I,33).EQ.1.0D0) THEN
              WRITE(OUTLYNE,914) PIKUP(2,I,33),PIKUP(3,I,33),PIKUP(4,I,33)
     1        ,PIKUP(6,I,33)
              CALL SHOWIT(10)
 914          FORMAT('PIKUP    ZD      ,',G23.15,',',G23.15,',',G23.15
     1        ,',,',G23.15)
          END IF
C
C       ALPHA PIKUP
          IF(PIKUP(1,I,15).EQ.1.0D0) THEN
              WRITE(OUTLYNE,15) PIKUP(2,I,15),PIKUP(3,I,15),PIKUP(4,I,15)
     1        ,PIKUP(6,I,15)
              CALL SHOWIT(10)
 15           FORMAT('PIKUP    ALPHA   ,',G23.15,',',G23.15,',',G23.15
     1        ,',,',G23.15)
          END IF
C
C       BETA PIKUP
          IF(PIKUP(1,I,16).EQ.1.0D0) THEN
              WRITE(OUTLYNE,16) PIKUP(2,I,16),PIKUP(3,I,16),PIKUP(4,I,16)
     1        ,PIKUP(6,I,16)
              CALL SHOWIT(10)
 16           FORMAT('PIKUP    BETA    ,',G23.15,',',G23.15,',',G23.15
     1        ,',,',G23.15)
          END IF
C
C       GAMMA PIKUP
          IF(PIKUP(1,I,17).EQ.1.0D0) THEN
              WRITE(OUTLYNE,17) PIKUP(2,I,17),PIKUP(3,I,17),PIKUP(4,I,17)
     1        ,PIKUP(6,I,17)
              CALL SHOWIT(10)
 17           FORMAT('PIKUP    GAMMA   ,',G23.15,',',G23.15,',',G23.15
     1        ,',,',G23.15)
          END IF
C
C       CLAP PIKUP
          IF(PIKUP(1,I,18).EQ.1.0D0) THEN
              WRITE(OUTLYNE,18) PIKUP(2,I,18),PIKUP(6,I,18)
              CALL SHOWIT(10)
 18           FORMAT('PIKUP    CLAP    ,',G23.15,',,,,',G23.15)
          END IF
C
C       COBS PIKUP
          IF(PIKUP(1,I,19).EQ.1.0D0) THEN
              WRITE(OUTLYNE,19) PIKUP(2,I,19),PIKUP(6,I,19)
              CALL SHOWIT(10)
 19           FORMAT('PIKUP    COBS    ,',G23.15,',,,,',G23.15)
          END IF
C
C       GLASS PIKUP
          IF(PIKUP(1,I,20).EQ.1.0D0) THEN
              WRITE(OUTLYNE,20) PIKUP(2,I,20),PIKUP(6,I,20)
              CALL SHOWIT(10)
 20           FORMAT('PIKUP    GLASS   ,',G23.15,',,,,',G23.15)
          END IF
C
C       CCTOR PIKUP
          IF(PIKUP(1,I,21).EQ.1.0D0) THEN
              WRITE(OUTLYNE,21) PIKUP(2,I,21),PIKUP(3,I,21),PIKUP(4,I,21)
     1        ,PIKUP(6,I,21)
              CALL SHOWIT(10)
 21           FORMAT('PIKUP    CCTOR   ,',G23.15,',',G23.15,',',G23.15
     1        ,',,',G23.15)
          END IF
C
C       ADTOR PIKUP
          IF(PIKUP(1,I,22).EQ.1.0D0) THEN
              WRITE(OUTLYNE,22) PIKUP(2,I,22),PIKUP(3,I,22),PIKUP(4,I,22)
     1        ,PIKUP(6,I,22)
              CALL SHOWIT(10)
 22           FORMAT('PIKUP    ADTOR   ,',G23.15,',',G23.15,',',G23.15
     1        ,',,',G23.15)
          END IF
C
C       AETOR PIKUP
          IF(PIKUP(1,I,23).EQ.1.0D0) THEN
              WRITE(OUTLYNE,23) PIKUP(2,I,23),PIKUP(3,I,23),PIKUP(4,I,23)
     1        ,PIKUP(6,I,23)
              CALL SHOWIT(10)
 23           FORMAT('PIKUP    AETOR   ,',G23.15,',',G23.15,',',G23.15
     1        ,',,',G23.15)
          END IF
C
C       AFTOR PIKUP
          IF(PIKUP(1,I,24).EQ.1.0D0) THEN
              WRITE(OUTLYNE,24) PIKUP(2,I,24),PIKUP(3,I,24),PIKUP(4,I,24)
     1        ,PIKUP(6,I,24)
              CALL SHOWIT(10)
 24           FORMAT('PIKUP    AFTOR   ,',G23.15,',',G23.15,',',G23.15
     1        ,',,',G23.15)
          END IF
C
C       AGTOR PIKUP
          IF(PIKUP(1,I,25).EQ.1.0D0) THEN
              WRITE(OUTLYNE,25) PIKUP(2,I,25),PIKUP(3,I,25),PIKUP(4,I,25)
     1        ,PIKUP(6,I,25)
              CALL SHOWIT(10)
 25           FORMAT('PIKUP    AGTOR   ,',G23.15,',',G23.15,',',G23.15
     1        ,',,',G23.15)
          END IF
C
C       AC PIKUP
          IF(PIKUP(1,I,26).EQ.1.0D0) THEN
              WRITE(OUTLYNE,26) PIKUP(2,I,26),PIKUP(3,I,26),PIKUP(4,I,26)
     1        ,PIKUP(6,I,26)
              CALL SHOWIT(10)
 26           FORMAT('PIKUP    AC      ,',G23.15,',',G23.15,',',G23.15
     1        ,',,',G23.15)
          END IF
C
C       AH PIKUP
          IF(PIKUP(1,I,27).EQ.1.0D0) THEN
              WRITE(OUTLYNE,27) PIKUP(2,I,27),PIKUP(3,I,27),PIKUP(4,I,27)
     1        ,PIKUP(6,I,27)
              CALL SHOWIT(10)
 27           FORMAT('PIKUP    AH      ,',G23.15,',',G23.15,',',G23.15
     1        ,',,',G23.15)
          END IF
C
C       AI PIKUP
          IF(PIKUP(1,I,28).EQ.1.0D0) THEN
              WRITE(OUTLYNE,28) PIKUP(2,I,28),PIKUP(3,I,28),PIKUP(4,I,28)
     1        ,PIKUP(6,I,28)
              CALL SHOWIT(10)
 28           FORMAT('PIKUP    AI      ,',G23.15,',',G23.15,',',G23.15
     1        ,',,',G23.15)
          END IF
C
C       AJ PIKUP
          IF(PIKUP(1,I,29).EQ.1.0D0) THEN
              WRITE(OUTLYNE,29) PIKUP(2,I,29),PIKUP(3,I,29),PIKUP(4,I,29)
     1        ,PIKUP(6,I,29)
              CALL SHOWIT(10)
 29           FORMAT('PIKUP    AJ      ,',G23.15,',',G23.15,',',G23.15
     1        ,',,',G23.15)
          END IF
C
C       AK PIKUP
          IF(PIKUP(1,I,30).EQ.1.0D0) THEN
              WRITE(OUTLYNE,30) PIKUP(2,I,30),PIKUP(3,I,30),PIKUP(4,I,30)
     1        ,PIKUP(6,I,30)
              CALL SHOWIT(10)
 30           FORMAT('PIKUP    AK      ,',G23.15,',',G23.15,',',G23.15
     1        ,',,',G23.15)
          END IF
C
C       AL PIKUP
          IF(PIKUP(1,I,31).EQ.1.0D0) THEN
              WRITE(OUTLYNE,31) PIKUP(2,I,31),PIKUP(3,I,31),PIKUP(4,I,31)
     1        ,PIKUP(6,I,31)
              CALL SHOWIT(10)
 31           FORMAT('PIKUP    AL      ,',G23.15,',',G23.15,',',G23.15
     1        ,',,',G23.15)
          END IF
C
C       THOAL PIKUP
          IF(PIKUP(1,I,32).EQ.1.0D0) THEN
              WRITE(OUTLYNE,32) PIKUP(2,I,32),PIKUP(3,I,32),PIKUP(4,I,32)
     1        ,PIKUP(5,I,32),PIKUP(6,I,32)
              CALL SHOWIT(10)
 32           FORMAT('PIKUP    THOAL   ,',G23.15,',',G23.15,',',G23.15
     1        ,',',G23.15,',',G23.15)
          END IF
C
C       ZD PIKUP
          IF(PIKUP(1,I,33).EQ.1.0D0) THEN
              WRITE(OUTLYNE,33) PIKUP(2,I,33),PIKUP(3,I,33),PIKUP(4,I,33)
     1        ,PIKUP(6,I,33)
              CALL SHOWIT(10)
 33           FORMAT('PIKUP    ZD      ,',G23.15,',',G23.15,',',G23.15
     1        ,',,',G23.15)
          END IF
C
C       PIVX PIKUP
          IF(PIKUP(1,I,34).EQ.1.0D0) THEN
              WRITE(OUTLYNE,34) PIKUP(2,I,34),PIKUP(3,I,34),PIKUP(4,I,34)
     1        ,PIKUP(6,I,34)
              CALL SHOWIT(10)
 34           FORMAT('PIKUP    PIVX    ,',G23.15,',',G23.15,',',G23.15
     1        ,',,',G23.15)
          END IF
C
C       PIVY PIKUP
          IF(PIKUP(1,I,35).EQ.1.0D0) THEN
              WRITE(OUTLYNE,35) PIKUP(2,I,35),PIKUP(3,I,35),PIKUP(4,I,35)
     1        ,PIKUP(6,I,35)
              CALL SHOWIT(10)
 35           FORMAT('PIKUP    PIVY    ,',G23.15,',',G23.15,',',G23.15
     1        ,',,',G23.15)
          END IF
C
C       PIVZ PIKUP
          IF(PIKUP(1,I,36).EQ.1.0D0) THEN
              WRITE(OUTLYNE,36) PIKUP(2,I,36),PIKUP(3,I,36),PIKUP(4,I,36)
     1        ,PIKUP(6,I,36)
              CALL SHOWIT(10)
 36           FORMAT('PIKUP    PIVZ    ,',G23.15,',',G23.15,',',G23.15
     1        ,',,',G23.15)
          END IF
C
C       GDX PIKUP
          IF(PIKUP(1,I,37).EQ.1.0D0) THEN
              WRITE(OUTLYNE,37) PIKUP(2,I,37),PIKUP(3,I,37),PIKUP(4,I,37)
     1        ,PIKUP(6,I,37)
              CALL SHOWIT(10)
 37           FORMAT('PIKUP    GDX     ,',G23.15,',',G23.15,',',G23.15
     1        ,',,',G23.15)
          END IF
C
C       GDY PIKUP
          IF(PIKUP(1,I,38).EQ.1.0D0) THEN
              WRITE(OUTLYNE,38) PIKUP(2,I,38),PIKUP(3,I,38),PIKUP(4,I,38)
     1        ,PIKUP(6,I,38)
              CALL SHOWIT(10)
 38           FORMAT('PIKUP    GDY     ,',G23.15,',',G23.15,',',G23.15
     1        ,',,',G23.15)
          END IF
C
C       GDZ PIKUP
          IF(PIKUP(1,I,39).EQ.1.0D0) THEN
              WRITE(OUTLYNE,39) PIKUP(2,I,39),PIKUP(3,I,39),PIKUP(4,I,39)
     1        ,PIKUP(6,I,39)
              CALL SHOWIT(10)
 39           FORMAT('PIKUP    GDZ     ,',G23.15,',',G23.15,',',G23.15
     1        ,',,',G23.15)
          END IF
C
C       GALPHA PIKUP
          IF(PIKUP(1,I,40).EQ.1.0D0) THEN
              WRITE(OUTLYNE,40) PIKUP(2,I,40),PIKUP(3,I,40),PIKUP(4,I,40)
     1        ,PIKUP(6,I,40)
              CALL SHOWIT(10)
 40           FORMAT('PIKUP    GALPHA  ,',G23.15,',',G23.15,',',G23.15
     1        ,',,',G23.15)
          END IF
C
C       GBETA PIKUP
          IF(PIKUP(1,I,41).EQ.1.0D0) THEN
              WRITE(OUTLYNE,41) PIKUP(2,I,41),PIKUP(3,I,41),PIKUP(4,I,41)
     1        ,PIKUP(6,I,41)
              CALL SHOWIT(10)
 41           FORMAT('PIKUP    GBETA   ,',G23.15,',',G23.15,',',G23.15
     1        ,',,',G23.15)
          END IF
C
C       GGAMMA PIKUP
          IF(PIKUP(1,I,42).EQ.1.0D0) THEN
              WRITE(OUTLYNE,42) PIKUP(2,I,42),PIKUP(3,I,42),PIKUP(4,I,42)
     1        ,PIKUP(6,I,42)
              CALL SHOWIT(10)
 42           FORMAT('PIKUP    GGAMMA  ,',G23.15,',',G23.15,',',G23.15
     1        ,',,',G23.15)
          END IF
C
C       GRT PIKUP
          IF(PIKUP(1,I,43).EQ.1.0D0) THEN
              WRITE(OUTLYNE,43) PIKUP(2,I,43),PIKUP(3,I,43),PIKUP(4,I,43)
     1        ,PIKUP(6,I,43)
              CALL SHOWIT(10)
 43           FORMAT('PIKUP    GRT     ,',G23.15,',',G23.15,',',G23.15
     1        ,',,',G23.15)
          END IF
C
C       COATING PIKUP
          IF(PIKUP(1,I,44).EQ.1.0D0) THEN
              WRITE(OUTLYNE,44) PIKUP(2,I,44),PIKUP(6,I,44)
              CALL SHOWIT(10)
 44           FORMAT('PIKUP    COATING ,',G23.15,',,,,',G23.15)
          END IF
C
C       CURVATURE SOLVES
          IF(ALENS(23,I).EQ.0.0D0.OR.ALENS(23,I).EQ.1.0D0) THEN
C       SURFACE IS NOT TORIC OR IS Y-TORIC
C       APY SOLVE
              IF(SOLVE(8,I).EQ.1.0D0) THEN
                  WRITE(OUTLYNE,202)SOLVE(9,I)
                  CALL SHOWIT(10)
 202              FORMAT('APY     ,',G23.15)
              END IF
C       PIY SOLVE
              IF(SOLVE(8,I).EQ.2.0D0) THEN
                  WRITE(OUTLYNE,203)SOLVE(9,I)
                  CALL SHOWIT(10)
 203              FORMAT('PIY     ,',G23.15)
              END IF
C       PUY SOLVE
              IF(SOLVE(8,I).EQ.3.0D0) THEN
                  WRITE(OUTLYNE,204)SOLVE(9,I)
                  CALL SHOWIT(10)
 204              FORMAT('PUY     ,',G23.15)
              END IF
C       APCY SOLVE
              IF(SOLVE(8,I).EQ.4.0D0) THEN
                  WRITE(OUTLYNE,205)SOLVE(9,I)
                  CALL SHOWIT(10)
 205              FORMAT('APCY    ,',G23.15)
              END IF
C       PICY SOLVE
              IF(SOLVE(8,I).EQ.5.0D0) THEN
                  WRITE(OUTLYNE,206)SOLVE(9,I)
                  CALL SHOWIT(10)
 206              FORMAT('PICY    ,',G23.15)
              END IF
C       PUCY SOLVE
              IF(SOLVE(8,I).EQ.6.0D0) THEN
                  WRITE(OUTLYNE,207)SOLVE(9,I)
                  CALL SHOWIT(10)
 207              FORMAT('PUCY    ,',G23.15)
              END IF
C       COCY SOLVE
              IF(SOLVE(8,I).EQ.7.0D0) THEN
                  WRITE(OUTLYNE,208)SOLVE(9,I)
                  CALL SHOWIT(10)
 208              FORMAT('COCY    ,',G23.15)
              END IF
          END IF
          IF(ALENS(23,I).EQ.2.0D0) THEN
C       SURFACE IS X-TORIC
C       APX SOLVE
              IF(SOLVE(2,I).EQ.8.0D0) THEN
                  WRITE(OUTLYNE,209)SOLVE(1,I)
                  CALL SHOWIT(10)
 209              FORMAT('APX     ,',G23.15)
              END IF
C       PIX SOLVE
              IF(SOLVE(2,I).EQ.9.0D0) THEN
                  WRITE(OUTLYNE,210)SOLVE(1,I)
                  CALL SHOWIT(10)
 210              FORMAT('PIX     ,',G23.15)
              END IF
C       PUX SOLVE
              IF(SOLVE(2,I).EQ.10.0D0) THEN
                  WRITE(OUTLYNE,211)SOLVE(1,I)
                  CALL SHOWIT(10)
 211              FORMAT('PUX     ,',G23.15)
              END IF
C       APCX SOLVE
              IF(SOLVE(2,I).EQ.11.0D0) THEN
                  WRITE(OUTLYNE,212)SOLVE(1,I)
                  CALL SHOWIT(10)
 212              FORMAT('APCX    ,',G23.15)
              END IF
C       PICX SOLVE
              IF(SOLVE(2,I).EQ.12.0D0) THEN
                  WRITE(OUTLYNE,213)SOLVE(1,I)
                  CALL SHOWIT(10)
 213              FORMAT('PICX    ,',G23.15)
              END IF
C       PUCX SOLVE
              IF(SOLVE(2,I).EQ.13.0D0) THEN
                  WRITE(OUTLYNE,214)SOLVE(1,I)
                  CALL SHOWIT(10)
 214              FORMAT('PUCX    ,',G23.15)
              END IF
C       COCX SOLVE
              IF(SOLVE(2,I).EQ.14.0D0) THEN
                  WRITE(OUTLYNE,215)SOLVE(1,I)
                  CALL SHOWIT(10)
 215              FORMAT('COCX    ,',G23.15)
              END IF
          END IF
C
C       THICKNESS SOLVES
C
C       PY SOLVE
          IF(SOLVE(6,I).EQ.1.0D0) THEN
              WRITE(OUTLYNE,302)SOLVE(7,I)
              CALL SHOWIT(10)
 302          FORMAT('PY      ,',G23.15)
          END IF
C       PCY SOLVE
          IF(SOLVE(6,I).EQ.2.0D0) THEN
              WRITE(OUTLYNE,303)SOLVE(7,I)
              CALL SHOWIT(10)
 303          FORMAT('PCY     ,',G23.15)
          END IF
C       CAY SOLVE
          IF(SOLVE(6,I).EQ.3.0D0) THEN
              WRITE(OUTLYNE,304)SOLVE(7,I)
              CALL SHOWIT(10)
 304          FORMAT('CAY     ,',G23.15)
          END IF
C
C       PX SOLVE
          IF(SOLVE(4,I).EQ.4.0D0) THEN
              WRITE(OUTLYNE,305)SOLVE(3,I)
              CALL SHOWIT(10)
 305          FORMAT('PX      ,',G23.15)
          END IF
C       PCX SOLVE
          IF(SOLVE(4,I).EQ.5.0D0) THEN
              WRITE(OUTLYNE,306)SOLVE(3,I)
              CALL SHOWIT(10)
 306          FORMAT('PCX     ,',G23.15)
          END IF
C       CAX SOLVE
          IF(SOLVE(4,I).EQ.6.0D0) THEN
              WRITE(OUTLYNE,307)SOLVE(3,I)
              CALL SHOWIT(10)
 307          FORMAT('CAX     ,',G23.15)
          END IF
C       ALL SOLVES AND PIKUPS HAVE BEEN TAKEN CARE OF.
          RETURN
      END
C SUB LENSFCV.FOR
      SUBROUTINE LENSFCV(I)
C
          IMPLICIT NONE
C
C       SUBROUTINE LENSF HANDELS LENS SURFACE INFO OUTPUT
C       DURING A LENO CV AND
C       ACTS ON THE CURRENT LENS, ONLY
C       ONE SURFACE AT A TIME.
C       THE VARIABLE I PASSES THE SURFACE
C       NUMBER OF INTEREST
C
          INTEGER I,J,JA
C
          REAL*8 A,B
C
          CHARACTER GL*27,SBL*79
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
C
C       THE CURRENT SURFACE IS PASSED WITH I
C       A DUMMY SURFACE HAS AIR AS ITS MATERIAL AND HAS
C       AIR AS THE MATERIAL INFRONT OF IT.
C
C       WE NOW HAVE LENO CV, PROCEED
          IF(I.EQ.0.AND.ALENS(3,I).GE.0.0D0) WRITE(OUTLYNE,1)
          IF(I.EQ.0.AND.ALENS(3,I).LT.0.0D0) WRITE(OUTLYNE,119)
          IF(ALENS(23,I).EQ.0.0D0.OR.ALENS(23,I).EQ.1.0D0) THEN
              IF(ALENS(1,I).EQ.0.0D0) A=0.0D0
              IF(ALENS(1,I).NE.0.0D0) A=1.0D0/ALENS(1,I)
          END IF
          IF(ALENS(23,I).EQ.2.0D0) THEN
              IF(ALENS(24,I).EQ.0.0D0) A=0.0D0
              IF(ALENS(24,I).NE.0.0D0) A=1.0D0/ALENS(24,I)
          END IF
          B=ALENS(3,I)
          DO,J=13,1,-1
              IF(GLANAM(I,2)(J:J).NE.' ') THEN
                  JA=J
                  GO TO 666
              END IF
          END DO
 666      CONTINUE
          IF(GLANAM(I,1).EQ.'SCHOTT')
     1    GL=GLANAM(I,2)(1:JA)//'_SCHOTT'
          IF(GLANAM(I,1).EQ.'SCH2000')
     1    GL=GLANAM(I,2)(1:JA)//'_SCH2000'
          IF(GLANAM(I,1).EQ.'OHARA')
     1    GL=GLANAM(I,2)(1:JA)//'_OHARA'
          IF(GLANAM(I,1).EQ.'HOYA')
     1    GL=GLANAM(I,2)(1:JA)//'_HOYA'
          IF(GLANAM(I,1).EQ.'HIKARI')
     1    GL=GLANAM(I,2)(1:JA)//'_HIKARI'
          IF(GLANAM(I,1).EQ.'CORNIN')
     1    GL=GLANAM(I,2)(1:JA)//'_CORNFR'
          IF(GLANAM(I,1).EQ.'CHANCE')
     1    GL=GLANAM(I,2)(1:JA)//'_CHANCE'
          IF(GLANAM(I,2).EQ.'LAST SURFACE')
     1    GL='AIR                        '
          IF(GLANAM(I,1).NE.'SCHOTT'.AND.
     1    GLANAM(I,1).NE.'SCH2000'.AND.
     1    GLANAM(I,1).NE.'OHARA'.AND.
     1    GLANAM(I,1).NE.'HOYA'.AND.
     1    GLANAM(I,1).NE.'HIKARI'.AND.
     1    GLANAM(I,1).NE.'CORNIN'.AND.
     1    GLANAM(I,2).NE.'LAST SURFACE'.AND.
     1    GLANAM(I,1).NE.'CHANCE')
     1    GL=GLANAM(I,2)(1:JA)
          IF(I.GT.0) WRITE(OUTLYNE,2) A,B,GL
          CALL SHOWIT(10)
C     SURFACE LABLE
          IF(ALENS(44,I).EQ.1.0D0) THEN
              SBL='SLB '//''''//LBL(I)(1:72)//''''
              WRITE(OUTLYNE,400) SBL(1:79)
              CALL SHOWIT(10)
          END IF
 400      FORMAT(A79)
 1        FORMAT('S0 0.0 1.0E20 AIR')
 119      FORMAT('S0 0.0 -1.0E20 AIR')
 2        FORMAT('S',1X,G23.15,1X,G23.15,1X,A27)
C
C               GRATING

          IF(ALENS(96,I).EQ.1.0D0) THEN
              WRITE(OUTLYNE,1212)
 1212         FORMAT('GRT')
              CALL SHOWIT(10)
              WRITE(OUTLYNE,1213) ALENS(97,I)
 1213         FORMAT('GRO',G23.15)
              CALL SHOWIT(10)
              WRITE(OUTLYNE,1214) ALENS(98,I)
 1214         FORMAT('GRS',G23.15)
              CALL SHOWIT(10)
              WRITE(OUTLYNE,1215) ALENS(99,I)
 1215         FORMAT('GRX',G23.15)
              CALL SHOWIT(10)
              WRITE(OUTLYNE,1216) ALENS(100,I)
 1216         FORMAT('GRY',G23.15)
              CALL SHOWIT(10)
              WRITE(OUTLYNE,1217) ALENS(101,I)
 1217         FORMAT('GRZ',G23.15)
              CALL SHOWIT(10)
          END IF
C
          IF(ALENS(2,I).NE.0.0) THEN
C               CONIC CONSTANT
              IF(ALENS(2,I).NE.0.0D0.AND.ALENS(23,I).EQ.0.0D0.AND.
     1        ALENS(8,I).EQ.0.0D0) THEN
                  WRITE(OUTLYNE,3)
 3                FORMAT('CONIC')
                  CALL SHOWIT(10)
                  WRITE(OUTLYNE,11) ALENS(2,I)
                  CALL SHOWIT(10)
 11               FORMAT('K ',G23.15)
              END IF
          END IF
          IF(ALENS(23,I).EQ.0.0D0.AND.
     1    ALENS(8,I).EQ.1.0D0) THEN
              WRITE(OUTLYNE,70)
 70           FORMAT('ASP')
              CALL SHOWIT(10)
              IF(ALENS(2,I).NE.0.0D0) THEN
                  WRITE(OUTLYNE,11) ALENS(2,I)
                  CALL SHOWIT(10)
              END IF
          END IF
C               ASPHERICS
          IF(ALENS(8,I).EQ.1.0) THEN
              IF(ALENS(4,I).NE.0.0D0) THEN
                  WRITE(OUTLYNE,5) ALENS(4,I)
                  CALL SHOWIT(10)
 5                FORMAT('A ',G23.15)
              END IF
              IF(ALENS(5,I).NE.0.0D0) THEN
                  WRITE(OUTLYNE,6) ALENS(5,I)
                  CALL SHOWIT(10)
 6                FORMAT('B ',G23.15)
              END IF
              IF(ALENS(6,I).NE.0.0D0) THEN
                  WRITE(OUTLYNE,7) ALENS(6,I)
                  CALL SHOWIT(10)
 7                FORMAT('C ',G23.15)
              END IF
              IF(ALENS(7,I).NE.0.0D0) THEN
                  WRITE(OUTLYNE,8) ALENS(7,I)
                  CALL SHOWIT(10)
 8                FORMAT('D ',G23.15)
              END IF
              IF(ALENS(81,I).NE.0.0D0) THEN
                  WRITE(OUTLYNE,990) ALENS(81,I)
                  CALL SHOWIT(10)
 990              FORMAT('E ',G23.15)
              END IF
              IF(ALENS(82,I).NE.0.0D0) THEN
                  WRITE(OUTLYNE,991) ALENS(82,I)
                  CALL SHOWIT(10)
 991              FORMAT('F ',G23.15)
              END IF
              IF(ALENS(83,I).NE.0.0D0) THEN
                  WRITE(OUTLYNE,992) ALENS(83,I)
                  CALL SHOWIT(10)
 992              FORMAT('G ',G23.15)
              END IF
              IF(ALENS(84,I).NE.0.0D0) THEN
                  WRITE(OUTLYNE,993) ALENS(84,I)
                  CALL SHOWIT(10)
 993              FORMAT('H ',G23.15)
              END IF
              IF(ALENS(85,I).NE.0.0D0) THEN
                  WRITE(OUTLYNE,994) ALENS(85,I)
                  CALL SHOWIT(10)
 994              FORMAT('J ',G23.15)
              END IF
          END IF
C
C       NOW TORICS IF PRESENT
C
          IF(ALENS(23,I).NE.0.0D0) THEN
C     TORICS
              IF(ALENS(2,I).EQ.0.0D0.AND.
     1        ALENS(41,I).EQ.0.0D0.AND.
     1        ALENS(5,I).EQ.0.0D0.AND.
     1        ALENS(6,I).EQ.0.0D0.AND.
     1        ALENS(7,I).EQ.0.0D0.AND.
     1        ALENS(8,I).EQ.0.0D0.AND.
     1        ALENS(37,I).EQ.0.0D0.AND.
     1        ALENS(38,I).EQ.0.0D0.AND.
     1        ALENS(39,I).EQ.0.0D0.AND.
     1        ALENS(40,I).EQ.0.0D0) THEN
C     CYLINDER ONLY
                  WRITE(OUTLYNE,35)
                  CALL SHOWIT(10)
C       TORICS PRESENT
                  IF(ALENS(23,I).EQ.1.0) THEN
C     YTORIC
                      WRITE(OUTLYNE,38) ALENS(24,I)
                      CALL SHOWIT(10)
                  END IF
                  IF(ALENS(23,I).EQ.2.0) THEN
                      WRITE(OUTLYNE,38) ALENS(1,I)
                      CALL SHOWIT(10)
                  END IF
              ELSE
C     ANAMORPHIC ASPHERE
                  WRITE(OUTLYNE,36)
                  CALL SHOWIT(10)
C       TORICS PRESENT
                  IF(ALENS(23,I).EQ.1.0) THEN
C     YTORIC
                      WRITE(OUTLYNE,38) ALENS(24,I)
                      CALL SHOWIT(10)
                      WRITE(OUTLYNE,61) ALENS(2,I)
                      CALL SHOWIT(10)
                      WRITE(OUTLYNE,60) ALENS(41,I)
                      CALL SHOWIT(10)
                      WRITE(OUTLYNE,62) ALENS(4,I)
                      CALL SHOWIT(10)
                      WRITE(OUTLYNE,63) ALENS(5,I)
                      CALL SHOWIT(10)
                      WRITE(OUTLYNE,64) ALENS(6,I)
                      CALL SHOWIT(10)
                      WRITE(OUTLYNE,65) ALENS(7,I)
                      CALL SHOWIT(10)
                      WRITE(OUTLYNE,66) ALENS(37,I)
                      CALL SHOWIT(10)
                      WRITE(OUTLYNE,67) ALENS(38,I)
                      CALL SHOWIT(10)
                      WRITE(OUTLYNE,68) ALENS(39,I)
                      CALL SHOWIT(10)
                      WRITE(OUTLYNE,69) ALENS(40,I)
                      CALL SHOWIT(10)
                  END IF
                  IF(ALENS(23,I).EQ.2.0) THEN
                      CALL SHOWIT(10)
                      WRITE(OUTLYNE,38) ALENS(1,I)
                      CALL SHOWIT(10)
                      WRITE(OUTLYNE,60) ALENS(2,I)
                      CALL SHOWIT(10)
                      WRITE(OUTLYNE,61) ALENS(41,I)
                      CALL SHOWIT(10)
                      WRITE(OUTLYNE,62) ALENS(37,I)
                      CALL SHOWIT(10)
                      WRITE(OUTLYNE,63) ALENS(38,I)
                      CALL SHOWIT(10)
                      WRITE(OUTLYNE,64) ALENS(39,I)
                      CALL SHOWIT(10)
                      WRITE(OUTLYNE,65) ALENS(40,I)
                      CALL SHOWIT(10)
                      WRITE(OUTLYNE,66) ALENS(4,I)
                      CALL SHOWIT(10)
                      WRITE(OUTLYNE,67) ALENS(5,I)
                      CALL SHOWIT(10)
                      WRITE(OUTLYNE,68) ALENS(6,I)
                      CALL SHOWIT(10)
                      WRITE(OUTLYNE,69) ALENS(7,I)
                      CALL SHOWIT(10)
                  END IF
              END IF
          END IF
 35       FORMAT('CYL')
 36       FORMAT('AAS')
 38       FORMAT('CUX',1X,G23.15)
! 37     FORMAT('CUY',1X,G23.15)
 60       FORMAT('KX',1X,G23.15)
 61       FORMAT('KY',1X,G23.15)
 62       FORMAT('AR ',1X,G23.15)
 63       FORMAT('BR ',1X,G23.15)
 64       FORMAT('CR ',1X,G23.15)
 65       FORMAT('DR ',1X,G23.15)
 66       FORMAT('AP ',1X,G23.15)
 67       FORMAT('BP ',1X,G23.15)
 68       FORMAT('CP ',1X,G23.15)
 69       FORMAT('DP ',1X,G23.15)
C

C
C               CLEAR APERTURE
          IF(ALENS(9,I).NE.0.0.OR.
     1    ALENS(16,I).NE.0.0) THEN
C     CLAP OR COBS PRESENT
              IF(ALENS(9,I).EQ.1.0) THEN
C       CIRCULAR CLAP
 14               FORMAT('CIR',1x,G23.15)
                  WRITE(OUTLYNE,14)ALENS(10,I)
                  CALL SHOWIT(10)
 814              FORMAT('ADY',1x,G23.15)
 1814             FORMAT('ADY OBS',1x,G23.15)
                  WRITE(OUTLYNE,814)ALENS(12,I)
                  CALL SHOWIT(10)
 914              FORMAT('ADX',1x,G23.15)
 1914             FORMAT('ADX OBS',1x,G23.15)
                  WRITE(OUTLYNE,914)ALENS(13,I)
                  CALL SHOWIT(10)
              END IF
              IF(ALENS(9,I).EQ.3.0) THEN
C       ELLIPTICAL CLAP
 16               FORMAT('ELY',1X,G23.15)
 816              FORMAT('ELX',1X,G23.15)
                  WRITE(OUTLYNE,16)ALENS(10,I)
                  CALL SHOWIT(10)
                  WRITE(OUTLYNE,816)ALENS(11,I)
                  CALL SHOWIT(10)
                  WRITE(OUTLYNE,814)ALENS(12,I)
                  CALL SHOWIT(10)
                  WRITE(OUTLYNE,914)ALENS(13,I)
                  CALL SHOWIT(10)
                  IF(ALENS(15,I).NE.0.0) THEN
                      WRITE(OUTLYNE,15) ALENS(15,I)
                      CALL SHOWIT(10)
                  END IF
 15               FORMAT('ARO',1X,G23.15)
 1515             FORMAT('ARO OBS',1X,G23.15)
              END IF
              IF(ALENS(9,I).EQ.2.0) THEN
C       RECTANGULAR CLAP
 17               FORMAT('REY',1X,G23.15)
 817              FORMAT('REX',1X,G23.15)
                  WRITE(OUTLYNE,17)ALENS(10,I)
                  CALL SHOWIT(10)
                  WRITE(OUTLYNE,817)ALENS(11,I)
                  CALL SHOWIT(10)
                  WRITE(OUTLYNE,814)ALENS(12,I)
                  CALL SHOWIT(10)
                  WRITE(OUTLYNE,914)ALENS(13,I)
                  CALL SHOWIT(10)
                  IF(ALENS(15,I).NE.0.0) THEN
                      WRITE(OUTLYNE,15) ALENS(15,I)
                      CALL SHOWIT(10)
                  END IF
              END IF
          END IF
C               OBSCURATIONS
          IF(ALENS(16,I).NE.0.0) THEN
              IF(ALENS(16,I).EQ.1.0) THEN
C       CIRCULAR CLAP
 714              FORMAT('CIR OBS',1x,G23.15)
                  WRITE(OUTLYNE,714)ALENS(17,I)
                  CALL SHOWIT(10)
                  IF(ALENS(19,I).NE.0.0D0) THEN
                      WRITE(OUTLYNE,1814)ALENS(19,I)
                      CALL SHOWIT(10)
                  END IF
                  IF(ALENS(20,I).NE.0.0D0) THEN
                      WRITE(OUTLYNE,1914)ALENS(20,I)
                      CALL SHOWIT(10)
                  END IF
              END IF
              IF(ALENS(16,I).EQ.3.0) THEN
C       ELLIPTICAL CLAP
 916              FORMAT('ELY OBS',1X,G23.15)
 1816             FORMAT('ELX OBS',1X,G23.15)
                  WRITE(OUTLYNE,916)ALENS(17,I)
                  CALL SHOWIT(10)
                  WRITE(OUTLYNE,1816)ALENS(18,I)
                  CALL SHOWIT(10)
                  IF(ALENS(19,I).NE.0.0D0) THEN
                      WRITE(OUTLYNE,1814)ALENS(19,I)
                      CALL SHOWIT(10)
                  END IF
                  IF(ALENS(20,I).NE.0.0D0) THEN
                      WRITE(OUTLYNE,1914)ALENS(20,I)
                      CALL SHOWIT(10)
                  END IF
                  IF(ALENS(22,I).NE.0.0) THEN
                      WRITE(OUTLYNE,1515) ALENS(22,I)
                      CALL SHOWIT(10)
                  END IF
              END IF
              IF(ALENS(16,I).EQ.2.0) THEN
C       RECTANGULAR CLAP
 917              FORMAT('REY OBS',1X,G23.15)
 1817             FORMAT('REX OBS',1X,G23.15)
                  WRITE(OUTLYNE,917)ALENS(17,I)
                  CALL SHOWIT(10)
                  WRITE(OUTLYNE,1817)ALENS(18,I)
                  CALL SHOWIT(10)
                  IF(ALENS(19,I).NE.0.0D0) THEN
                      WRITE(OUTLYNE,1814)ALENS(19,I)
                      CALL SHOWIT(10)
                  END IF
                  IF(ALENS(20,I).NE.0.0D0) THEN
                      WRITE(OUTLYNE,1914)ALENS(20,I)
                      CALL SHOWIT(10)
                  END IF
                  IF(ALENS(22,I).NE.0.0) THEN
                      WRITE(OUTLYNE,1515) ALENS(22,I)
                      CALL SHOWIT(10)
                  END IF
              END IF
          END IF
C
C       TILTS AND DECENTRATIONS
          IF(ALENS(29,I).NE.0.0D0) THEN
C       SURFACE DECENTER
              WRITE(OUTLYNE,40) ALENS(114,I)
              CALL SHOWIT(10)
              WRITE(OUTLYNE,41) ALENS(115,I)
              CALL SHOWIT(10)
              WRITE(OUTLYNE,42) ALENS(116,I)
              CALL SHOWIT(10)
          END IF
          IF(ALENS(25,I).NE.0.0D0) THEN
C       TILTS
              IF(ALENS(25,I).EQ.4.0D0) THEN
 51               FORMAT('BEN')
                  WRITE(OUTLYNE,51)
                  CALL SHOWIT(10)
              END IF
              IF(ALENS(25,I).EQ.5.0D0) THEN
 52               FORMAT('DAR')
                  WRITE(OUTLYNE,52)
                  CALL SHOWIT(10)
              END IF
              IF(ALENS(25,I).EQ.7.0D0) THEN
 545              FORMAT('REV')
                  WRITE(OUTLYNE,545)
                  CALL SHOWIT(10)
              END IF
              WRITE(OUTLYNE,43) ALENS(118,I)
              CALL SHOWIT(10)
              WRITE(OUTLYNE,44) ALENS(119,I)
              CALL SHOWIT(10)
              WRITE(OUTLYNE,45) ALENS(120,I)
              CALL SHOWIT(10)
              IF(ALENS(25,I).EQ.6.0D0) THEN
 53               FORMAT('RET ,',G23.15)
                  WRITE(OUTLYNE,53) ALENS(70,I)
                  CALL SHOWIT(10)
              END IF
          END IF
C
 40       FORMAT('XDE',1X,G23.15)
 41       FORMAT('YDE',1X,G23.15)
 42       FORMAT('ZDE',1X,G23.15)
 43       FORMAT('ADE',1X,G23.15)
 44       FORMAT('BDE',1X,G23.15)
 45       FORMAT('CDE',1X,G23.15)
C
C
C       NOW ASTOP AND REF
          IF(I.EQ.INT(SYSTEM1(26))) THEN
C       I IS THE ASTOP SURFACE, IS THERE AN ADJUSTMENT ?
              WRITE(OUTLYNE,95)
 95           FORMAT('STOP')
          END IF
          IF(I.EQ.INT(SYSTEM1(25))) THEN
C       SURFACE SHOULD BE THE REFERENCE SURFACE
              WRITE(OUTLYNE,95)
              CALL SHOWIT(10)
          END IF
C
          RETURN
      END
C SUB LENHDAC.FOR
      SUBROUTINE LENHDAC
C
          IMPLICIT NONE
C
C       SUBROUTINE LENHDAC HANDEL LENS HEADER INFO OUTPUT
C       DURING A LENO AC. ACTS ON THE CURRENT LENS
C
          INTEGER I
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
C
C       OUTPUT THE HEADER COMMAND (LENS)
C

          WRITE(OUTLYNE,10)
          CALL SHOWIT(10)
C
C
C       NOW THE LI
C

          IF(LI(1:20).NE.'                    ')
     1    WRITE(OUTLYNE,20) LI(1:79)
          IF(LI(1:20).NE.'                    ') CALL SHOWIT(10)
C
C       NOW LIC
C
          DO I=1,LICCNT
              IF(LIC(I)(1:20).NE.'                    ') THEN
                  WRITE(OUTLYNE,21) LIC(I)
                  CALL SHOWIT(10)
              END IF
          END DO
C
C       NOW WV
C

          WRITE(OUTLYNE,22)
     1    SYSTEM1(1),SYSTEM1(2),SYSTEM1(3),SYSTEM1(4),SYSTEM1(5)
          CALL SHOWIT(10)
C
C
C       NOW UNITS

          IF(SYSTEM1(6).EQ.1.0) WRITE(OUTLYNE,23)
          IF(SYSTEM1(6).EQ.2.0) WRITE(OUTLYNE,24)
          IF(SYSTEM1(6).EQ.3.0) WRITE(OUTLYNE,25)
          IF(SYSTEM1(6).EQ.4.0) WRITE(OUTLYNE,33)
          CALL SHOWIT(10)
C
C       NOW PRIMARY WAVELENGTH PAIR

          WRITE(OUTLYNE,26) INT(SYSTEM1(7)),INT(SYSTEM1(8))
          CALL SHOWIT(10)
C
C       SECONDARY WAVELENGTH PAIR

          WRITE(OUTLYNE,27) INT(SYSTEM1(9)),INT(SYSTEM1(10))
          CALL SHOWIT(10)
C
C       CONTROL WAVELENGTH

          WRITE(OUTLYNE,28) INT(SYSTEM1(11))
          CALL SHOWIT(10)
C
C
C       SAY

          WRITE(OUTLYNE,29) SYSTEM1(12)
          CALL SHOWIT(10)
C
C       SAX
          IF(SYSTEM1(13).NE.SYSTEM1(12)) THEN
              WRITE(OUTLYNE,29) SYSTEM1(13)
              CALL SHOWIT(10)
              WRITE(OUTLYNE,30) SYSTEM1(13)
              CALL SHOWIT(10)
          END IF
C
C       SCY (SCY FANG IS NOT DIRECTLY PASSED BY LENO)
          WRITE(OUTLYNE,31) SYSTEM1(14),SYSTEM1(15)
          CALL SHOWIT(10)
C
          IF(SYSTEM1(14).NE.SYSTEM1(16).OR.SYSTEM1(15).NE.SYSTEM1(17)) THEN
C     DO SCX
C       SCX (SCX FANG IS NOT DIRECTLY PASSED BY LENO)
              WRITE(OUTLYNE,31) SYSTEM1(16),SYSTEM1(17)
              CALL SHOWIT(10)
              WRITE(OUTLYNE,32) SYSTEM1(16),SYSTEM1(17)
              CALL SHOWIT(10)
          END IF
C
 10       FORMAT('LENS')
 20       FORMAT('LI,',A79)
 21       FORMAT('LIC,',A79)
! 121    FORMAT('INI,',A79)
! 1212   FORMAT('LTYPE,',A5)
C
 22       FORMAT('WV,',E15.7,',',E15.7,',',E15.7,',',E15.7
     1    ,',',E15.7)
 23       FORMAT('UNITS IN')
 24       FORMAT('UNITS CM')
 25       FORMAT('UNITS MM')
 33       FORMAT('UNITS METERS')
 26       FORMAT('PCW,',I2,',',I2)
 27       FORMAT('SCW,',I2,',',I2)
 28       FORMAT('CW,',I2)
 29       FORMAT('SAY,',E15.7)
 30       FORMAT('SAX,',E15.7)
 31       FORMAT('SCY,',E15.7,',',E15.7)
 32       FORMAT('SCX,',E15.7,',',E15.7)
          RETURN
      END
C SUB LENEDAC.FOR
      SUBROUTINE LENEDAC
C
          IMPLICIT NONE
C
C       SUBROUTINE LENED HANDEL LENS TRAILING INFO OUTPUT
C       DURING A LENO AC. ACTS ON THE CURRENT LENS.
C       USED AFTER SURFACE DATA OUTPUT
C
          !       INTEGER SS,I,J
C
          INCLUDE 'datcfg.inc'
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
C
C       THE TERMINAL RECORD FOR AND LENO IS EOS
C
          WRITE(OUTLYNE,10)
          CALL SHOWIT(10)
 10       FORMAT('EOS')
C**********************************************************

C       MODE SETTING
          IF(SYSTEM1(30).EQ.1) WRITE(OUTLYNE,33)
          IF(SYSTEM1(30).EQ.2) WRITE(OUTLYNE,34)
          IF(SYSTEM1(30).EQ.3) WRITE(OUTLYNE,35)
          IF(SYSTEM1(30).EQ.4) WRITE(OUTLYNE,36)
          CALL SHOWIT(10)
 33       FORMAT('MODE FOCAL')
 34       FORMAT('MODE UFOCAL')
 35       FORMAT('MODE AFOCAL')
 36       FORMAT('MODE UAFOCAL')
C
C       SPTWT

          WRITE(OUTLYNE,3000) SYSTEM1(31),SYSTEM1(32),SYSTEM1(33)
     1    ,SYSTEM1(34),SYSTEM1(35)
          CALL SHOWIT(10)
 3000     FORMAT('SPTWT,',E15.7,',',E15.7,',',E15.7,
     1    ',',E15.7,',',E15.7)
          RETURN
      END
C SUB LENSFAC.FOR
      SUBROUTINE LENSFAC(I)
C
          IMPLICIT NONE
C
C       SUBROUTINE LENSF HANDELS LENS SURFACE INFO OUTPUT
C       DURING A LENO AC AND
C       ACTS ON THE CURRENT LENS, ONLY
C       ONE SURFACE AT A TIME.
C       THE VARIABLE I PASSES THE SURFACE
C       NUMBER OF INTEREST
C
          INTEGER I,IG,J
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
C
C       THE CURRENT SURFACE IS PASSED WITH I
C       A DUMMY SURFACE HAS AIR AS ITS MATERIAL AND HAS
C       AIR AS THE MATERIAL INFRONT OF IT.
C
C       WE NOW HAVE LENO AC, PROCEED
C
C               CURVATURE

          WRITE(OUTLYNE,10) ALENS(1,I)
          CALL SHOWIT(10)
 10       FORMAT('CV,',E15.7)
C
C       ALL DONE WITH CURVATURE
C
          IF(ALENS(2,I).NE.0.0) THEN
C               CONIC CONSTANT

              WRITE(OUTLYNE,11) ALENS(2,I)
              CALL SHOWIT(10)
 11           FORMAT('CC,',E15.7)
          END IF

C               THICKNESS

          WRITE(OUTLYNE,12) ALENS(3,(I))
          CALL SHOWIT(10)
 12       FORMAT('TH,',E15.7)
C
C               ASPHERICS
          IF(ALENS(8,I).EQ.1.0) THEN

C     LONG FORM
              WRITE(OUTLYNE,13) ALENS(4,I),ALENS(5,I),
     1        ALENS(6,I),ALENS(7,I)
              CALL SHOWIT(10)
 13           FORMAT('ASPH,',E15.7,',',E15.7,',',
     1        E15.7,',',E15.7)
          END IF
C               CLEAR APERTURE
          IF(ALENS(9,I).NE.0.0D0) THEN
              IF(ALENS(9,I).EQ.1.0) THEN
C       CIRCULAR CLAP

 14               FORMAT('CLAP,',E15.7)
                  WRITE(OUTLYNE,14)ALENS(10,I)
                  CALL SHOWIT(10)
              END IF
              IF(ALENS(9,I).EQ.3.0) THEN
C       ELLIPTICAL CLAP

 16               FORMAT('CLAP ELIP,',E15.7,',',E15.7)
                  WRITE(OUTLYNE,16)ALENS(10,I),ALENS(11,I)
                  CALL SHOWIT(10)
              END IF
              IF(ALENS(9,I).EQ.2.0) THEN
C       RECTANGULAR CLAP
 17               FORMAT('CLAP RECT,',E15.7,',',E15.7)
                  WRITE(OUTLYNE,17)ALENS(10,I),ALENS(11,I)
                  CALL SHOWIT(10)
              END IF
          END IF
C     OBSCURATIONS
          IF(ALENS(16,I).NE.0.0) THEN
              IF(ALENS(16,I).EQ.1.0) THEN
C       CIRCULAR COBS
 25               FORMAT('COBS,',E15.7)
                  WRITE(OUTLYNE,25)ALENS(17,I)
                  CALL SHOWIT(10)
              END IF
              IF(ALENS(16,I).EQ.3.0) THEN
C       ELLIPTICAL COBS
 26               FORMAT('COBS ELIP,',E15.7,',',E15.7)
                  WRITE(OUTLYNE,26)ALENS(17,I),ALENS(18,I)
                  CALL SHOWIT(10)
              END IF
              IF(ALENS(16,I).EQ.2.0) THEN
C       RECTANGULAR COBS
 28               FORMAT('COBS RECT,',E15.7,',',E15.7)
                  WRITE(OUTLYNE,28)ALENS(17,I),ALENS(18,I)
                  CALL SHOWIT(10)
              END IF
          END IF
C
C
C       NOW TORICS IF PRESENT
C
          IF(ALENS(23,I).NE.0.0) THEN
C       TORICS PRESENT
              IF(ALENS(23,I).EQ.1.0) THEN

                  WRITE(OUTLYNE,36)
                  CALL SHOWIT(10)

                  WRITE(OUTLYNE,38) ALENS(24,I)
                  CALL SHOWIT(10)
              END IF
              IF(ALENS(23,I).EQ.2.0) THEN

                  WRITE(OUTLYNE,37)
                  CALL SHOWIT(10)

                  WRITE(OUTLYNE,39) ALENS(24,I)
                  CALL SHOWIT(10)
              END IF
          END IF
 36       FORMAT('YTORIC')
 38       FORMAT('CVX,',E15.7)
 37       FORMAT('XTORIC  ')
 39       FORMAT('CVY,',E15.7)
C
C
C       TILTS AND DECENTRATIONS
          IF(ALENS(29,I).NE.0.0D0) THEN
C       SURFACE DECENTER
              WRITE(OUTLYNE,106) ALENS(115,I),ALENS(114,I)
              CALL SHOWIT(10)
 106          FORMAT('DEC,',E15.7,',',E15.7)
          END IF
          IF(ALENS(25,I).EQ.1.0D0.AND.ALENS(77,I).EQ.0.0D0.OR.
     1    ALENS(25,I).EQ.-1.0D0) THEN
C       TILTS
              IF(ALENS(25,I).EQ.1.0D0.AND.ALENS(77,I).EQ.0.0D0)
     1        WRITE(OUTLYNE,102) ALENS(118,I),ALENS(119,I),ALENS(120,I)
              IF(ALENS(25,I).EQ.-1.0D0)
     1        WRITE(OUTLYNE,103) ALENS(118,I),ALENS(119,I),ALENS(120,I)
              CALL SHOWIT(10)
C
 102          FORMAT('TILT,',E15.7,',',E15.7,',',E15.7)
 103          FORMAT('RTILT,',E15.7,',',E15.7,',',E15.7)
C
          END IF
C
C       NOW ASTOP AND REF
          IF(I.EQ.INT(SYSTEM1(25))) THEN
C       SURFACE SHOULD BE THE REFERENCE SURFACE

              WRITE(OUTLYNE,94)
              CALL SHOWIT(10)
          END IF
          IF(I.EQ.INT(SYSTEM1(26))) THEN
C       I IS THE ASTOP SURFACE, IS THERE AN ADJUSTMENT ?

              IF(SYSTEM1(27).EQ.0.0)  WRITE(OUTLYNE,95)
              IF(SYSTEM1(27).EQ.1.0)  WRITE(OUTLYNE,96)
              IF(SYSTEM1(27).EQ.-1.0) WRITE(OUTLYNE,97)
              IF(SYSTEM1(27).EQ.2.0)  WRITE(OUTLYNE,98)
              IF(SYSTEM1(27).GE.-1.0D0.AND.SYSTEM1(27).LE.2.0D0)
     1        CALL SHOWIT(10)
 94           FORMAT('REFS')
 95           FORMAT('ASTOP')
 96           FORMAT('ASTOP    EN')
 97           FORMAT('ASTOP    EX')
 98           FORMAT('ASTOP    ENEX')
          END IF
C
          IF(GLANAM(I,1).EQ.' '.AND.GLANAM(I,2).EQ.'AIR'.OR.
     1    GLANAM(I,1).EQ.' '.AND.GLANAM(I,2).EQ.'LAST SURFACE') THEN
              WRITE(OUTLYNE,110)
              CALL SHOWIT(10)
              RETURN
          END IF
          IF(GLANAM(I,1).EQ.' '.AND.GLANAM(I,2).EQ.'REFL') THEN
              WRITE(OUTLYNE,120)
              CALL SHOWIT(10)
              RETURN
          END IF
          GLANAM(I,1)='GLA'
          DO J=1,6
              IF(GLANAM(I,2)(J:J).EQ.' ') THEN
                  IG=J
                  GO TO 101
              END IF
          END DO
 101      CONTINUE
          IF(GLANAM(I,1)(1:5).EQ.'GLA') THEN
              IF(IG.EQ.1)
     1        WRITE(OUTLYNE,201)GLANAM(I,1)(1:6),GLANAM(I,2)(1:IG),ALENS(46,I),
     2        ALENS(47,I),ALENS(48,I),ALENS(49,I),ALENS(50,I)
              IF(IG.EQ.2)
     1        WRITE(OUTLYNE,202)GLANAM(I,1)(1:6),GLANAM(I,2)(1:IG),ALENS(46,I),
     2        ALENS(47,I),ALENS(48,I),ALENS(49,I),ALENS(50,I)
              IF(IG.EQ.3)
     1        WRITE(OUTLYNE,203)GLANAM(I,1)(1:6),GLANAM(I,2)(1:IG),ALENS(46,I),
     2        ALENS(47,I),ALENS(48,I),ALENS(49,I),ALENS(50,I)
              IF(IG.EQ.4)
     1        WRITE(OUTLYNE,204)GLANAM(I,1)(1:6),GLANAM(I,2)(1:IG),ALENS(46,I),
     2        ALENS(47,I),ALENS(48,I),ALENS(49,I),ALENS(50,I)
              IF(IG.EQ.5)
     1        WRITE(OUTLYNE,205)GLANAM(I,1)(1:6),GLANAM(I,2)(1:IG),ALENS(46,I),
     2        ALENS(47,I),ALENS(48,I),ALENS(49,I),ALENS(50,I)
              IF(IG.EQ.6)
     1        WRITE(OUTLYNE,206)GLANAM(I,1)(1:6),GLANAM(I,2)(1:IG),ALENS(46,I),
     2        ALENS(47,I),ALENS(48,I),ALENS(49,I),ALENS(50,I)
              CALL SHOWIT(10)
          END IF
 201      FORMAT(A6,' ',A1,',',E15.7,',',E15.7,',',E15.7,',',
     1    E15.7,',',E15.7)
 202      FORMAT(A6,' ',A2,',',E15.7,',',E15.7,',',E15.7,',',
     1    E15.7,',',E15.7)
 203      FORMAT(A6,' ',A3,',',E15.7,',',E15.7,',',E15.7,',',
     1    E15.7,',',E15.7)
 204      FORMAT(A6,' ',A4,',',E15.7,',',E15.7,',',E15.7,',',
     1    E15.7,',',E15.7)
 205      FORMAT(A6,' ',A5,',',E15.7,',',E15.7,',',E15.7,',',
     1    E15.7,',',E15.7)
 206      FORMAT(A6,' ',A6,',',E15.7,',',E15.7,',',E15.7,',',
     1    E15.7,',',E15.7)
 110      FORMAT('AIR')
 120      FORMAT('REFL')
          RETURN
      END
C     LENS SURFACES
C SUB RLENSF.FOR
      SUBROUTINE RLENSF(I)
C
          IMPLICIT NONE
C
C       SUBROUTINE RLENSF HANDELS LENS SURFACE INFO OUTPUT
C       DURING A LENO REVERSE AND
C       ACTS ON THE CURRENT LENS, ONLY
C       ONE SURFACE AT A TIME.
C       THE VARIABLE I PASSES THE SURFACE
C       NUMBER OF INTEREST
C       ALL SOLVES,PIKUPS AND TILTS/DECENTRATIONS ARE IGNORED
C
          INTEGER I,J
          CHARACTER*3 AALL
C
          REAL*8 AVAV
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
C
C       THE CURRENT SURFACE IS PASSED WITH I
C       A DUMMY SURFACE HAS AIR AS ITS MATERIAL AND HAS
C       AIR AS THE MATERIAL INFRONT OF IT.
C
C       WE NOW HAVE LENO REVERSE, PROCEED
C     SURFACE LABEL
          IF(ALENS(44,I).EQ.1.0D0) THEN
              WRITE(OUTLYNE,400) LBL(I)(1:74)
              CALL SHOWIT(10)
 400          FORMAT('LBL ,',A74)
          END IF
C     COATING NUMBER
          WRITE(OUTLYNE,300) DBLE(INT(ALENS(112,I)))
          CALL SHOWIT(10)
 300      FORMAT('COATING ,',G23.15)
C               CURVATURE
          WRITE(OUTLYNE,10) -ALENS(1,I)
          CALL SHOWIT(10)
 10       FORMAT('CV      ,',G23.15)
          IF(ALENS(2,I).NE.0.0) THEN
C               CONIC CONSTANT
              WRITE(OUTLYNE,11) ALENS(2,I)
              CALL SHOWIT(10)
 11           FORMAT('CC      ,',G23.15)
          END IF
C               THICKNESS
          IF(I.NE.0) WRITE(OUTLYNE,12) ALENS(3,(I-1))
          IF(I.NE.0) CALL SHOWIT(10)
          AVAV=0.0D0
          IF(I.EQ.0) WRITE(OUTLYNE,12) AVAV
          IF(I.NE.0) CALL SHOWIT(10)
 12       FORMAT('TH      ,',G23.15)
C
C
C       DEFORM
          IF(ALENS(103,I).EQ.1.0D0) THEN
              IF(ALENS(104,I).EQ.1.0D0) AALL='F01'
              IF(ALENS(104,I).EQ.2.0D0) AALL='F02'
              IF(ALENS(104,I).EQ.3.0D0) AALL='F03'
              IF(ALENS(104,I).EQ.4.0D0) AALL='F04'
              IF(ALENS(104,I).EQ.5.0D0) AALL='F05'
              IF(ALENS(104,I).EQ.6.0D0) AALL='F06'
              IF(ALENS(104,I).EQ.7.0D0) AALL='F07'
              IF(ALENS(104,I).EQ.8.0D0) AALL='F08'
              IF(ALENS(104,I).EQ.9.0D0) AALL='F09'
              IF(ALENS(104,I).EQ.10.0D0) AALL='F10'
              WRITE(OUTLYNE,1218) AALL,ALENS(105,I),ALENS(106,I)
              CALL SHOWIT(10)
 1218         FORMAT('DEFORM   ',A3,1X,G23.15,',',G23.15)
          END IF
C       LENS ARRAY
          IF(ALENS(133,I).NE.0.0D0) THEN
              IF(ALENS(133,I).EQ.-1.0D0)
     1        WRITE(OUTLYNE,1219) ALENS(131,I),ALENS(132,I)
              IF(ALENS(133,I).EQ.1.0D0)
     1        WRITE(OUTLYNE,1220) ALENS(131,I),ALENS(132,I)
              CALL SHOWIT(10)
 1219         FORMAT('ARRAY ODD',G23.15,',',G23.15)
 1220         FORMAT('ARRAY EVEN',G23.15,',',G23.15)
          END IF
C               GRATING

          IF(ALENS(96,I).EQ.1.0D0) THEN
              WRITE(OUTLYNE,1212)
 1212         FORMAT('GRT')
              CALL SHOWIT(10)
              WRITE(OUTLYNE,1213) ALENS(97,I)
 1213         FORMAT('GRO',G23.15)
              CALL SHOWIT(10)
              WRITE(OUTLYNE,1214) ALENS(98,I)
 1214         FORMAT('GRS',G23.15)
              CALL SHOWIT(10)
              WRITE(OUTLYNE,1215) ALENS(99,I)
 1215         FORMAT('GRX',G23.15)
              CALL SHOWIT(10)
              WRITE(OUTLYNE,1216) ALENS(100,I)
 1216         FORMAT('GRY',G23.15)
              CALL SHOWIT(10)
              WRITE(OUTLYNE,1217) ALENS(101,I)
 1217         FORMAT('GRZ',G23.15)
              CALL SHOWIT(10)
          END IF
C
C               ASPHERICS
          IF(ALENS(8,I).EQ.1.0) THEN
C     LONG FORM
              WRITE(OUTLYNE,13) -ALENS(4,I),-ALENS(5,I),
     1        -ALENS(6,I),-ALENS(7,I),-ALENS(43,I)
              CALL SHOWIT(10)
 13           FORMAT('ASPH    ,',G23.15,',',G23.15,',',
     1        G23.15,',',G23.15,',',G23.15)
              WRITE(OUTLYNE,113) -ALENS(81,I),-ALENS(82,I),
     1        -ALENS(83,I),-ALENS(84,I),-ALENS(85,I)
              CALL SHOWIT(10)
 113          FORMAT('ASPH2   ,',G23.15,',',G23.15,',',
     1        G23.15,',',G23.15,',',G23.15)
          END IF
C               CLEAR APERTURE
          IF(ALENS(9,I).EQ.1.0) THEN
C       CIRCULAR CLAP
 14           FORMAT('CLAP    ,',G23.15,',',G23.15,',',G23.15,',',G23.15,
     1        ',',G23.15)
              IF(ALENS(11,I).EQ.0.0D0) ALENS(11,I)=ALENS(10,I)
              WRITE(OUTLYNE,14)ALENS(10,I),ALENS(12,I),ALENS(13,I),ALENS(11,I)
     1        ,ALENS(14,I)
              CALL SHOWIT(10)
          END IF
          IF(ALENS(9,I).EQ.3.0) THEN
C       ELLIPTICAL CLAP
 16           FORMAT('CLAP     ELIP    ,',G23.15,',',G23.15,
     1                  ',',G23.15,',',G23.15)
              WRITE(OUTLYNE,16)ALENS(10,I),ALENS(11,I),ALENS(12,I),
     1        ALENS(13,I)
              CALL SHOWIT(10)
              IF(ALENS(15,I).NE.0.0) THEN
                  WRITE(OUTLYNE,15) ALENS(15,I)
                  CALL SHOWIT(10)
              END IF
 15           FORMAT('CLAP     TILT    ,',G23.15)
          END IF
          IF(ALENS(9,I).EQ.2.0) THEN
C       RECTANGULAR CLAP
 17           FORMAT('CLAP     RECT    ,',G23.15,',',G23.15,
     1                  ',',G23.15,',',G23.15)
              WRITE(OUTLYNE,17)ALENS(10,I),ALENS(11,I),ALENS(12,I),
     1        ALENS(13,I)
              CALL SHOWIT(10)
              IF(ALENS(15,I).NE.0.0) THEN
                  WRITE(OUTLYNE,15) ALENS(15,I)
                  CALL SHOWIT(10)
              END IF
          END IF
          IF(ALENS(9,I).EQ.4.0) THEN
C       RACETRACK CLAP
 18           FORMAT('CLAP     RCTK    ,',G23.15,',',G23.15,',',G23.15,
     1                  ',',G23.15,',',G23.15)
              WRITE(OUTLYNE,18)ALENS(10,I),ALENS(11,I),ALENS(12,I),
     1        ALENS(13,I),ALENS(14,I)
              CALL SHOWIT(10)
              IF(ALENS(15,I).NE.0.0) THEN
                  WRITE(OUTLYNE,15) ALENS(15,I)
                  CALL SHOWIT(10)
              END IF
          END IF
          IF(ALENS(9,I).EQ.5.0) THEN
C       POLY CLAP
 181          FORMAT('CLAP     POLY    ,',G23.15,',',G23.15,',',G23.15,
     1                  ',',G23.15,',',G23.15)
              WRITE(OUTLYNE,181)ALENS(10,I),ALENS(11,I),ALENS(12,I),
     1        ALENS(13,I),ALENS(14,I)
              CALL SHOWIT(10)
              IF(ALENS(15,I).NE.0.0) THEN
                  WRITE(OUTLYNE,15) ALENS(15,I)
                  CALL SHOWIT(10)
              END IF
          END IF
          IF(ALENS(9,I).EQ.6.0) THEN
C       POLY ICLAP
 182          FORMAT('CLAP     IPOLY   ,',G23.15,',',G23.15,',',G23.15,
     1                  ',',G23.15,',',G23.15)
              WRITE(OUTLYNE,182)ALENS(10,I),ALENS(11,I),ALENS(12,I),
     1        ALENS(13,I),ALENS(14,I)
              CALL SHOWIT(10)
              IF(ALENS(15,I).NE.0.0) THEN
                  WRITE(OUTLYNE,15) ALENS(15,I)
                  CALL SHOWIT(10)
              END IF
          END IF
C
C     MULTCLAP
          IF(ALENS(127,I).NE.0.0D0) THEN
              DO J=1,INT(ALENS(127,I))
                  WRITE(OUTLYNE,1818) J,MULTCLAP(J,1,I),MULTCLAP(J,2,I)
     1            ,MULTCLAP(J,3,I)
                  CALL SHOWIT(10)
 1818             FORMAT('MULTCLAP ',I4,',',G23.15,',',G23.15,',',G23.15)
              END DO
          END IF
C
          IF(ALENS(51,I).EQ.1.0) THEN
C       CIRCULAR ERASE CLAP
 20           FORMAT('CLAP     ERASE   ,',G23.15,',',G23.15,',',G23.15)
              WRITE(OUTLYNE,20)ALENS(52,I),ALENS(54,I),ALENS(55,I)
              CALL SHOWIT(10)
          END IF
          IF(ALENS(51,I).EQ.3.0) THEN
C       ELLIPTICAL ERASE CLAP
 21           FORMAT('CLAP     ELIPE   ,',G23.15,',',G23.15,',',G23.15,
     1                  ',',G23.15)
              WRITE(OUTLYNE,21)ALENS(52,I),ALENS(53,I),ALENS(54,I),
     1        ALENS(55,I)
              CALL SHOWIT(10)
              IF(ALENS(57,I).NE.0.0) THEN
                  WRITE(OUTLYNE,15) ALENS(57,I)
                  CALL SHOWIT(10)
              END IF
          END IF
          IF(ALENS(51,I).EQ.2.0) THEN
C       RECTANGULAR ERACE CLAP
 22           FORMAT('CLAP     RECTE   ,',G23.15,',',G23.15,',',
     1                  G23.15,',',G23.15)
              WRITE(OUTLYNE,22)ALENS(52,I),ALENS(53,I),ALENS(54,I),
     1        ALENS(55,I)
              CALL SHOWIT(10)
              IF(ALENS(57,I).NE.0.0) THEN
                  WRITE(OUTLYNE,15) ALENS(57,I)
                  CALL SHOWIT(10)
              END IF
          END IF
          IF(ALENS(51,I).EQ.4.0) THEN
C       RACETRACK ERASE CLAP
 23           FORMAT('CLAP     RCTKE   ,',G23.15,',',G23.15,',',G23.15,
     1                  ',',G23.15,',',G23.15)
              WRITE(OUTLYNE,23)ALENS(52,I),ALENS(53,I),ALENS(54,I),
     1        ALENS(55,I),ALENS(56,I)
              CALL SHOWIT(10)
              IF(ALENS(57,I).NE.0.0) THEN
                  WRITE(OUTLYNE,15) ALENS(57,I)
                  CALL SHOWIT(10)
              END IF
          END IF
          IF(ALENS(51,I).EQ.5.0) THEN
C       POLY ERASE CLAP
 231          FORMAT('CLAP     POLYE   ,',G23.15,',',G23.15,',',G23.15,
     1                  ',',G23.15,',',G23.15)
              WRITE(OUTLYNE,231)ALENS(52,I),ALENS(53,I),ALENS(54,I),
     1        ALENS(55,I),ALENS(56,I)
              CALL SHOWIT(10)
              IF(ALENS(57,I).NE.0.0) THEN
                  WRITE(OUTLYNE,15) ALENS(57,I)
                  CALL SHOWIT(10)
              END IF
          END IF
          IF(ALENS(51,I).EQ.6.0) THEN
C       IPOLY ERASE CLAP
 232          FORMAT('CLAP     IPOLYE  ,',G23.15,',',G23.15,',',G23.15,
     1                  ',',G23.15,',',G23.15)
              WRITE(OUTLYNE,232)ALENS(52,I),ALENS(53,I),ALENS(54,I),
     1        ALENS(55,I),ALENS(56,I)
              CALL SHOWIT(10)
              IF(ALENS(57,I).NE.0.0) THEN
                  WRITE(OUTLYNE,15) ALENS(57,I)
                  CALL SHOWIT(10)
              END IF
          END IF
C               OBSCURATIONS
          IF(ALENS(16,I).NE.0.0) THEN
              IF(ALENS(16,I).EQ.1.0) THEN
C       CIRCULAR COBS
 25               FORMAT('COBS    ,',G23.15,',',G23.15,',',G23.15)
                  WRITE(OUTLYNE,25)ALENS(17,I),ALENS(19,I),ALENS(20,I)
                  CALL SHOWIT(10)
              END IF
              IF(ALENS(16,I).EQ.3.0) THEN
C       ELLIPTICAL COBS
 26               FORMAT('COBS     ELIP    ,',G23.15,',',G23.15,
     1            ',',G23.15,',',G23.15)
                  WRITE(OUTLYNE,26)ALENS(17,I),ALENS(18,I),ALENS(19,I),
     1            ALENS(20,I)
                  CALL SHOWIT(10)
                  IF(ALENS(22,I).NE.0.0) THEN
                      WRITE(OUTLYNE,27) ALENS(22,I)
                      CALL SHOWIT(10)
                  END IF
 27               FORMAT('COBS     TILT    ,',G23.15)
              END IF
              IF(ALENS(16,I).EQ.2.0D0.AND.ALENS(134,I).EQ.0.0D0) THEN
C       RECTANGULAR COBS
 28               FORMAT('COBS     RECT    ,',G23.15,',',G23.15,
     1            ',',G23.15,',',G23.15)
                  WRITE(OUTLYNE,28)ALENS(17,I),ALENS(18,I),ALENS(19,I),
     1            ALENS(20,I)
                  CALL SHOWIT(10)
                  IF(ALENS(22,I).NE.0.0) THEN
                      WRITE(OUTLYNE,27) ALENS(22,I)
                      CALL SHOWIT(10)
                  END IF
              END IF
C       NOT RECTANGULAR COBS,PROCEED
          END IF
          IF(ALENS(16,I).EQ.4.0) THEN
C       RACETRACK COBS
 29           FORMAT('COBS     RCTK    ,',G23.15,',',G23.15,',',G23.15,
     1                  ',',G23.15,',',G23.15)
              WRITE(OUTLYNE,29)ALENS(17,I),ALENS(18,I),ALENS(19,I),
     1        ALENS(20,I),ALENS(21,I)
              CALL SHOWIT(10)
              IF(ALENS(22,I).NE.0.0) THEN
                  WRITE(OUTLYNE,27) ALENS(22,I)
                  CALL SHOWIT(10)
              END IF
          END IF
          IF(ALENS(16,I).EQ.5.0) THEN
C       POLY COBS
 291          FORMAT('COBS     POLY    ,',G23.15,',',G23.15,',',G23.15,
     1                  ',',G23.15,',',G23.15)
              WRITE(OUTLYNE,291)ALENS(17,I),ALENS(18,I),ALENS(19,I),
     1        ALENS(20,I),ALENS(21,I)
              CALL SHOWIT(10)
              IF(ALENS(22,I).NE.0.0) THEN
                  WRITE(OUTLYNE,27) ALENS(22,I)
                  CALL SHOWIT(10)
              END IF
          END IF
          IF(ALENS(16,I).EQ.6.0) THEN
C       IPOLY COBS
 292          FORMAT('COBS     IPOLY   ,',G23.15,',',G23.15,',',G23.15,
     1                  ',',G23.15,',',G23.15)
              WRITE(OUTLYNE,292)ALENS(17,I),ALENS(18,I),ALENS(19,I),
     1        ALENS(20,I),ALENS(21,I)
              CALL SHOWIT(10)
              IF(ALENS(22,I).NE.0.0) THEN
                  WRITE(OUTLYNE,27) ALENS(22,I)
                  CALL SHOWIT(10)
              END IF
          END IF
C
C     MULTCOBS
          IF(ALENS(128,I).NE.0.0D0.AND.ALENS(134,I).EQ.0.0D0) THEN
              DO J=1,INT(ALENS(128,I))
                  WRITE(OUTLYNE,1819) J,MULTCOBS(J,1,I),MULTCOBS(J,2,I)
     1            ,MULTCOBS(J,3,I)
                  CALL SHOWIT(10)
 1819             FORMAT('MULTCOBS ',I4,',',G23.15,',',G23.15,',',G23.15)
              END DO
          END IF
C
C     SPIDER
          IF(ALENS(134,I).NE.0.0D0) THEN
              WRITE(OUTLYNE,1820) ALENS(135,I),ALENS(136,I),ALENS(137,I)
              CALL SHOWIT(10)
 1820         FORMAT('SPIDER ,',G23.15,',',G23.15,',',G23.15)
          END IF
          IF(ALENS(61,I).EQ.1.0) THEN
C       CIRCULAR ERASE COBS
 31           FORMAT('COBS     ERASE   ,',G23.15,',',G23.15,',',G23.15)
              WRITE(OUTLYNE,31)ALENS(62,I),ALENS(64,I),ALENS(65,I)
              CALL SHOWIT(10)
          END IF
          IF(ALENS(61,I).EQ.3.0) THEN
C       ELLIPTICAL ERASE COBS
 32           FORMAT('COBS     ELIPE   ,',G23.15,',',G23.15,',',G23.15,
     1                  ',',G23.15)
              WRITE(OUTLYNE,32)ALENS(62,I),ALENS(63,I),ALENS(64,I),
     1        ALENS(65,I)
              CALL SHOWIT(10)
              IF(ALENS(67,I).NE.0.0) THEN
                  WRITE(OUTLYNE,27) ALENS(67,I)
                  CALL SHOWIT(10)
              END IF
          END IF
          IF(ALENS(61,I).EQ.2.0) THEN
C       RECTANGULAR ERACE COBS
 33           FORMAT('COBS     RECTE   ,',G23.15,',',G23.15,',',
     1                  G23.15,',',G23.15)
              WRITE(OUTLYNE,33)ALENS(62,I),ALENS(63,I),ALENS(64,I),
     1        ALENS(65,I)
              CALL SHOWIT(10)
              IF(ALENS(67,I).NE.0.0) THEN

                  WRITE(OUTLYNE,27) ALENS(67,I)
                  CALL SHOWIT(10)
              END IF
          END IF
          IF(ALENS(61,I).EQ.4.0) THEN
C       RACETRACK ERASE CLAP
 34           FORMAT('COBS     RCTKE   ,',G23.15,',',G23.15,',',G23.15,
     1                  ',',G23.15,',',G23.15)
              WRITE(OUTLYNE,34)ALENS(62,I),ALENS(63,I),ALENS(64,I),
     1        ALENS(65,I),ALENS(66,I)
              CALL SHOWIT(10)
              IF(ALENS(67,I).NE.0.0) THEN

                  WRITE(OUTLYNE,27) ALENS(67,I)
                  CALL SHOWIT(10)
              END IF
          END IF
          IF(ALENS(61,I).EQ.5.0) THEN
C       POLY ERASE CLAP
 341          FORMAT('COBS     POLYE   ,',G23.15,',',G23.15,',',G23.15,
     1                  ',',G23.15,',',G23.15)
              WRITE(OUTLYNE,341)ALENS(62,I),ALENS(63,I),ALENS(64,I),
     1        ALENS(65,I),ALENS(66,I)
              CALL SHOWIT(10)
              IF(ALENS(67,I).NE.0.0) THEN

                  WRITE(OUTLYNE,27) ALENS(67,I)
                  CALL SHOWIT(10)
              END IF
          END IF
          IF(ALENS(61,I).EQ.6.0) THEN
C       IPOLY ERASE CLAP
 342          FORMAT('COBS     IPOLYE  ,',G23.15,',',G23.15,',',G23.15,
     1                  ',',G23.15,',',G23.15)
              WRITE(OUTLYNE,342)ALENS(62,I),ALENS(63,I),ALENS(64,I),
     1        ALENS(65,I),ALENS(66,I)
              CALL SHOWIT(10)
              IF(ALENS(67,I).NE.0.0) THEN

                  WRITE(OUTLYNE,27) ALENS(67,I)
                  CALL SHOWIT(10)
              END IF
          END IF
C
C
C       NOW TORICS IF PRESENT
C
          IF(ALENS(23,I).NE.0.0) THEN
C       TORICS PRESENT
              IF(ALENS(23,I).EQ.1.0) THEN
                  WRITE(OUTLYNE,36)
                  CALL SHOWIT(10)
                  WRITE(OUTLYNE,38) -ALENS(24,I)
                  CALL SHOWIT(10)
              END IF
              IF(ALENS(23,I).EQ.2.0) THEN
                  WRITE(OUTLYNE,37)
                  CALL SHOWIT(10)
                  WRITE(OUTLYNE,39) -ALENS(24,I)
                  CALL SHOWIT(10)
              END IF
          END IF
 36       FORMAT('YTORIC  ')
 38       FORMAT('CVTOR   ,',G23.15)
 37       FORMAT('XTORIC  ')
 39       FORMAT('CVTOR   ,',G23.15)
C
C       ROOF, CORNER CUBE AND RAYERROR SURFACES
          IF(ALENS(126,I).EQ.1.0D0) THEN
              WRITE(OUTLYNE,1121) ALENS(138,I),ALENS(139,I)
              CALL SHOWIT(10)
          END IF
          IF(ALENS(126,I).EQ.2.0D0) THEN
              WRITE(OUTLYNE,1122) ALENS(138,I),ALENS(139,I),ALENS(140,I)
     1        ,ALENS(141,I)
              CALL SHOWIT(10)
          END IF
          WRITE(OUTLYNE,1123) ALENS(144,I)
          CALL SHOWIT(10)
 1121     FORMAT('ROO ',G23.15,',',G23.15)
 1122     FORMAT('CCR ',G23.15,',',G23.15,','
     1    ,G23.15,',',G23.15)
 1123     FORMAT('RAYERROR',G23.15)
C
C       ANAMORPHIC CONIC AND ASPHERIC TERMS
C
          IF(ALENS(41,I).NE.0.0) THEN
              WRITE(OUTLYNE,40) ALENS(41,I)
              CALL SHOWIT(10)
 40           FORMAT('CCTOR   ,',G23.15)
          END IF
          IF(ALENS(36,I).NE.0.0) THEN
C       ANAMORPHIC ASPHERICS
              WRITE(OUTLYNE,41) -ALENS(37,I),-ALENS(38,I),-ALENS(39,I),
     1        -ALENS(40,I)
              CALL SHOWIT(10)
 41           FORMAT('TASPH   ,',G23.15,',',G23.15,',',G23.15,
     1        ',',G23.15)
          END IF
C
C       REAL OR PARAXAIL SURFACE
          IF(ALENS(124,I).EQ.1.0D0) THEN
C       PARAXIAL SURFACE
              WRITE(OUTLYNE,4711)
              CALL SHOWIT(10)
4711          FORMAT('PARAX')
          END IF
C
C       SPECIFIC GRAVITY
          WRITE(OUTLYNE,4701) DABS(ALENS(102,I))
          IF(ALENS(102,I).NE.0.0D0)
     1    CALL SHOWIT(10)
 4701     FORMAT('SPGR,',G23.15)
C
C       MIRROR THICKNESS
          IF(DABS(ALENS(110,I)).GT.0.0D0) THEN
              IF(GLANAM(I,1).EQ.'             '.AND.
     1        GLANAM(I,2).EQ.'REFL         ') THEN
                  WRITE(OUTLYNE,4702) DABS(ALENS(110,I))
                  CALL SHOWIT(10)
              END IF
          END IF
 4702     FORMAT('THM,',G23.15)
C
C       PRICE
          WRITE(OUTLYNE,4703) DABS(ALENS(111,I))
          IF(ALENS(111,I).NE.0.0D0) CALL SHOWIT(10)
 4703     FORMAT('PRICE,',G23.15)
C
C       FOOTBLOK
          IF(ALENS(58,I).EQ.1.0D0) THEN
C       SET FOOTBLOK
              WRITE(OUTLYNE,4747)
              CALL SHOWIT(10)
 4747         FORMAT('FOOTBLOK ON')
          END IF
C
C       PIVAXIS
          IF(ALENS(113,I).EQ.1.0D0) THEN
              WRITE(OUTLYNE,4748)
              CALL SHOWIT(10)
 4748         FORMAT('PIVAXIS NORMAL')
          END IF
C       SURFACE FORCED DUMMY
          IF(ALENS(68,I).EQ.1.0D0) THEN
              WRITE(OUTLYNE,1471)
              CALL SHOWIT(10)
 1471         FORMAT('NODUM YES')
          END IF
          IF(ALENS(143,I).EQ.0.0D0) THEN
C     INR IS EXPLICITLY SET
              WRITE(OUTLYNE,475) ALENS(76,I)
              CALL SHOWIT(10)
 475          FORMAT('INR     ,',G23.15)
          END IF
C       NOW ASTOP AND REF
          IF(I.EQ.INT(SYSTEM1(25))) THEN
C       SURFACE SHOULD BE THE REFERENCE SURFACE
              WRITE(OUTLYNE,94)
              CALL SHOWIT(10)
          END IF
          IF(I.EQ.INT(SYSTEM1(26))) THEN
C       I IS THE ASTOP SURFACE, IS THERE AN ADJUSTMENT ?
              IF(SYSTEM1(27).EQ.0.0)  WRITE(OUTLYNE,95)
              IF(SYSTEM1(27).EQ.1.0)  WRITE(OUTLYNE,96)
              IF(SYSTEM1(27).EQ.-1.0) WRITE(OUTLYNE,97)
              IF(SYSTEM1(27).EQ.2.0)  WRITE(OUTLYNE,98)
              IF(SYSTEM1(27).GE.-1.0D0.AND.SYSTEM1(27).LE.2.0D0)
     1        CALL SHOWIT(10)
 94           FORMAT('REFS')
 95           FORMAT('ASTOP')
 96           FORMAT('ASTOP    EN')
 97           FORMAT('ASTOP    EX')
 98           FORMAT('ASTOP    ENEX')
          END IF
C
          IF(GLANAM(I,1).EQ.' '.AND.GLANAM(I,2).EQ.'REFL') THEN
              WRITE(OUTLYNE,120)
              CALL SHOWIT(10)
              RETURN
          END IF
          IF(GLANAM(I,1).EQ.' '.AND.GLANAM(I,2).EQ.'REFLTIRO') THEN
              WRITE(OUTLYNE,2120)
              CALL SHOWIT(10)
              RETURN
          END IF
          IF(GLANAM(I,1).EQ.' '.AND.GLANAM(I,2).EQ.'REFLTIR') THEN
              WRITE(OUTLYNE,3120)
              CALL SHOWIT(10)
              RETURN
          END IF
          IF(I.NE.0) THEN
              IF(GLANAM(I-1,1).EQ.' '.AND.GLANAM(I-1,2).EQ.'REFL'.OR.
     1        GLANAM(I-1,1).EQ.' '.AND.GLANAM(I-2,2).EQ.'REFLTIRO') THEN
                  IF(DABS(ALENS(46,I-1)).EQ.1.0D0.AND.
     1            DABS(ALENS(47,I-1)).EQ.1.0D0.AND.
     2            DABS(ALENS(48,I-1)).EQ.1.0D0.AND.
     3            DABS(ALENS(49,I-1)).EQ.1.0D0.AND.
     4            DABS(ALENS(50,I-1)).EQ.1.0D0.AND.
     5            DABS(ALENS(71,I-1)).EQ.1.0D0.AND.
     6            DABS(ALENS(72,I-1)).EQ.1.0D0.AND.
     7            DABS(ALENS(73,I-1)).EQ.1.0D0.AND.
     8            DABS(ALENS(74,I-1)).EQ.1.0D0.AND.
     9            DABS(ALENS(75,I-1)).EQ.1.0D0) THEN
C     USE AIR
                      WRITE(OUTLYNE,110)
                      CALL SHOWIT(10)
                  ELSE
C     HERE IS WHERE THE SEARCH GOES. WE ARE NOT AT I=0
C     WE ALREADY KNOW WHAT SITS AT I-1, IT IS REFL
C     WE CAN SEARCH BACK TO WHEN J=0
                      IF(I.GT.1) THEN
                          DO J=I-2,0,-1
C     IS IT ANOTHER REFLECTOR ? GO TO 87
                              IF(GLANAM(J,1).EQ.' '.AND.GLANAM(J,2).EQ.'REFLTIRO') GO TO 87
                              IF(GLANAM(J,1).EQ.' '.AND.GLANAM(J,2).EQ.'REFLTIR') GO TO 87
                              IF(GLANAM(J,1).EQ.' '.AND.GLANAM(J,2).EQ.'REFL') GO TO 87
C
C     WE LOOK FOR GLASSES TO USE AT SURFACE J
C     IF FOUND GOOD GLASS, GO TO 9876
C     IS IT "GLASS" ?
                              IF(GLANAM(J,1).EQ.'GLASS') THEN
                                  WRITE(OUTLYNE,100)GLANAM(J,1)(1:8),GLANAM(J,2)(1:8),
     1                            ALENS(46,J),ALENS(47,J),ALENS(48,J),ALENS(49,J),
     2                            ALENS(50,J)
                                  CALL SHOWIT(10)
                                  WRITE(OUTLYNE,1011) ALENS(71,J)
                                  CALL SHOWIT(10)
                                  WRITE(OUTLYNE,1012) ALENS(72,J)
                                  CALL SHOWIT(10)
                                  WRITE(OUTLYNE,1013) ALENS(73,J)
                                  CALL SHOWIT(10)
                                  WRITE(OUTLYNE,1014) ALENS(74,J)
                                  CALL SHOWIT(10)
                                  WRITE(OUTLYNE,1015) ALENS(75,J)
                                  CALL SHOWIT(10)
                                  GO TO 9876
                              END IF
C     IS IT "MODEL" ?
                              IF(GLANAM(J,1).EQ.'MODEL') THEN
                                  WRITE(OUTLYNE,7676)GLANAM(J,1)(1:8),GLANAM(J,2)(1:8),
     1                            ALENS(86,J),ALENS(87,J),ALENS(89,J)
                                  CALL SHOWIT(10)
                                  GO TO 9876
                              END IF
C     IS IT A CATALOG GLASS ?
                              IF(GLANAM(J,1).NE.'GLASS'.AND.GLANAM(J,1).NE.' '
     1                          .AND.GLANAM(J,2).NE.'AIR'.AND.GLANAM(J,2).NE.
     2                          'REFL'.AND.GLANAM(J,2).NE.'LAST SURFACE'
     3                        .AND.GLANAM(J,1).NE.'MODEL'.AND.
     4                        GLANAM(I,2).NE.'REFLTIRO'.AND.
     5                        GLANAM(I,2).NE.'REFLTIR') THEN
                                  WRITE(OUTLYNE,101)GLANAM(J,1)(1:8),GLANAM(J,2)
                                  CALL SHOWIT(10)
                                  GO TO 9876
                              END IF
 87                           CONTINUE
                          END DO
                      END IF
C     FELL OUT HERE, USE UNKNOWN GLASS
C     NO GOOD REAL GLASS FOUND, USE UNKNOWN
                      WRITE(OUTLYNE,9010)
                      CALL SHOWIT(10)
                      WRITE(OUTLYNE,9011)
                      CALL SHOWIT(10)
                      WRITE(OUTLYNE,9012)
                      CALL SHOWIT(10)
                      WRITE(OUTLYNE,9013)
                      CALL SHOWIT(10)
                      WRITE(OUTLYNE,9014)
                      CALL SHOWIT(10)
                      WRITE(OUTLYNE,9015)
                      CALL SHOWIT(10)
 9010                 FORMAT('GLASS UNKNOWN 1.0 1.0 1.0 1.0 1.0')
 9011                 FORMAT('N6 1.0')
 9012                 FORMAT('N7 1.0')
 9013                 FORMAT('N8 1.0')
 9014                 FORMAT('N9 1.0')
 9015                 FORMAT('N10 1.0')
                  END IF
 9876             CONTINUE
                  RETURN
              END IF
C
              IF(GLANAM(I-1,1).EQ.' '.AND.GLANAM(I-1,2).EQ.'AIR'.OR.
     1        GLANAM(I-1,1).EQ.' '.AND.GLANAM(I-1,2).EQ.'LAST SURFACE') THEN
                  WRITE(OUTLYNE,110)
                  CALL SHOWIT(10)
                  RETURN
              END IF
              IF(GLANAM(I-1,1).EQ.'GLASS') THEN
                  WRITE(OUTLYNE,100)GLANAM(I-1,1)(1:8),GLANAM(I-1,2)(1:8),
     1            ALENS(46,I-1),ALENS(47,I-1),ALENS(48,I-1),ALENS(49,I-1),
     2            ALENS(50,I-1)
                  CALL SHOWIT(10)
! 6676   FORMAT(A8,' ',A8,',',G23.15,',',G23.15,',',G23.15)
 7676             FORMAT(A8,' ',A8,',',G23.15,',',G23.15,',',G23.15)
! 6677   FORMAT(G23.15,',',G23.15)
              END IF
              IF(GLANAM(I-1,1).EQ.'GLASS') THEN
                  WRITE(OUTLYNE,1011) ALENS(71,I-1)
                  CALL SHOWIT(10)
                  WRITE(OUTLYNE,1012) ALENS(72,I-1)
                  CALL SHOWIT(10)
                  WRITE(OUTLYNE,1013) ALENS(73,I-1)
                  CALL SHOWIT(10)
                  WRITE(OUTLYNE,1014) ALENS(74,I-1)
                  CALL SHOWIT(10)
                  WRITE(OUTLYNE,1015) ALENS(75,I-1)
                  CALL SHOWIT(10)
 1011             FORMAT('N6      ,',G23.15)
 1012             FORMAT('N7      ,',G23.15)
 1013             FORMAT('N8      ,',G23.15)
 1014             FORMAT('N9      ,',G23.15)
 1015             FORMAT('N10     ,',G23.15)
                  RETURN
              END IF
              IF(GLANAM(I-1,1).NE.'GLASS'.AND.GLANAM(I-1,1).NE.' '
     1        .AND.GLANAM(I-1,2).NE.'AIR'.AND.GLANAM(I-1,2).NE.
     2        'REFL'.AND.GLANAM(I-1,1).NE.'MODEL'.AND.
     3        GLANAM(I-1,2).NE.'REFLTIRO'.AND.
     4        GLANAM(I-1,2).NE.'REFLTIR')THEN
                  WRITE(OUTLYNE,101)GLANAM(I-1,1)(1:8),GLANAM(I-1,2)
                  CALL SHOWIT(10)
                  RETURN
              END IF
          ELSE
              WRITE(OUTLYNE,110)
              CALL SHOWIT(10)
          END IF
 100      FORMAT(A8,' ',A8,',',G23.15,',',G23.15,',',G23.15,',',
     1    G23.15,',',G23.15)
 101      FORMAT(A8,' ',A13)
 110      FORMAT('AIR')
 120      FORMAT('REFL')
 2120     FORMAT('REFLTIRO')
 3120     FORMAT('REFLTIR')
C
          RETURN
      END
C SUB LENSF.FOR
      SUBROUTINE LENSF(I,RDOUT)
C
          IMPLICIT NONE
C
C       SUBROUTINE LENSF HANDELS LENS SURFACE INFO OUTPUT
C       DURING A LENO AND
C       ACTS ON THE CURRENT LENS, ONLY
C       ONE SURFACE AT A TIME.
C       THE VARIABLE I PASSES THE SURFACE
C       NUMBER OF INTEREST
C
          INTEGER I,J
C
          LOGICAL RDOUT
C
          CHARACTER*3 AALL
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
C
C       THE CURRENT SURFACE IS PASSED WITH I
C       A DUMMY SURFACE HAS AIR AS ITS MATERIAL AND HAS
C       AIR AS THE MATERIAL INFRONT OF IT.
C
C       WE NOW HAVE SIMPLE LENO, PROCEED
C
C
 401      FORMAT('C THE FOLLOWING DATA REFERS TO SURFACE #',I1)
 402      FORMAT('C THE FOLLOWING DATA REFERS TO SURFACE #',I2)
 403      FORMAT('C THE FOLLOWING DATA REFERS TO SURFACE #',I3)
C     SURFACE LABEL
          IF(I.LT.10) WRITE(OUTLYNE,401) I
          IF(I.GE.10.AND.I.LT.100) WRITE(OUTLYNE,402) I
          IF(I.GE.100) WRITE(OUTLYNE,403) I
          CALL SHOWIT(10)
C
          IF(ALENS(44,I).EQ.1.0D0) THEN
              WRITE(OUTLYNE,400) LBL(I)(1:74)
              CALL SHOWIT(10)
 400          FORMAT('LBL ,',A74)
          END IF
C     COATING NUMBER
          WRITE(OUTLYNE,300) DBLE(INT(ALENS(112,I)))
          CALL SHOWIT(10)
 300      FORMAT('COATING ,',G23.15)
C               CURVATURE

          IF(.NOT.RDOUT)WRITE(OUTLYNE,10) ALENS(1,I)
          IF(RDOUT) THEN
              IF(ALENS(1,I).NE.0.0D0)WRITE(OUTLYNE,1010) 1.0D0/ALENS(1,I)
              IF(ALENS(1,I).EQ.0.0D0)WRITE(OUTLYNE,10) ALENS(1,I)
          END IF
          CALL SHOWIT(10)
 10       FORMAT('CV      ,',G23.15)
 1010     FORMAT('RD      ,',G23.15)
C
C       ALL DONE WITH CURVATURE
C
          IF(ALENS(2,I).NE.0.0) THEN
C               CONIC CONSTANT

              WRITE(OUTLYNE,11) ALENS(2,I)
              CALL SHOWIT(10)
 11           FORMAT('CC      ,',G23.15)
          END IF

C               THICKNESS

          WRITE(OUTLYNE,12) ALENS(3,(I))
          CALL SHOWIT(10)
 12       FORMAT('TH      ,',G23.15)
C
C
C       DEFORM
          IF(ALENS(103,I).EQ.1.0D0) THEN
              IF(ALENS(104,I).EQ.1.0D0) AALL='F01'
              IF(ALENS(104,I).EQ.2.0D0) AALL='F02'
              IF(ALENS(104,I).EQ.3.0D0) AALL='F03'
              IF(ALENS(104,I).EQ.4.0D0) AALL='F04'
              IF(ALENS(104,I).EQ.5.0D0) AALL='F05'
              IF(ALENS(104,I).EQ.6.0D0) AALL='F06'
              IF(ALENS(104,I).EQ.7.0D0) AALL='F07'
              IF(ALENS(104,I).EQ.8.0D0) AALL='F08'
              IF(ALENS(104,I).EQ.9.0D0) AALL='F09'
              IF(ALENS(104,I).EQ.10.0D0) AALL='F10'
              WRITE(OUTLYNE,1218) AALL,ALENS(105,I),ALENS(106,I)
              CALL SHOWIT(10)
 1218         FORMAT('DEFORM   ',A3,1X,G23.15,',',G23.15)
          END IF
C       LENS ARRAY
          IF(ALENS(133,I).NE.0.0D0) THEN
              IF(ALENS(133,I).EQ.-1.0D0)
     1        WRITE(OUTLYNE,1219) ALENS(131,I),ALENS(132,I)
              IF(ALENS(133,I).EQ.1.0D0)
     1        WRITE(OUTLYNE,1220) ALENS(131,I),ALENS(132,I)
              CALL SHOWIT(10)
 1219         FORMAT('ARRAY ODD',G23.15,',',G23.15)
 1220         FORMAT('ARRAY EVEN',G23.15,',',G23.15)
          END IF
C               GRATING

          IF(ALENS(96,I).EQ.1.0D0) THEN
              WRITE(OUTLYNE,1212)
 1212         FORMAT('GRT')
              CALL SHOWIT(10)
              WRITE(OUTLYNE,1213) ALENS(97,I)
 1213         FORMAT('GRO',G23.15)
              CALL SHOWIT(10)
              WRITE(OUTLYNE,1214) ALENS(98,I)
 1214         FORMAT('GRS',G23.15)
              CALL SHOWIT(10)
              WRITE(OUTLYNE,1215) ALENS(99,I)
 1215         FORMAT('GRX',G23.15)
              CALL SHOWIT(10)
              WRITE(OUTLYNE,1216) ALENS(100,I)
 1216         FORMAT('GRY',G23.15)
              CALL SHOWIT(10)
              WRITE(OUTLYNE,1217) ALENS(101,I)
 1217         FORMAT('GRZ',G23.15)
              CALL SHOWIT(10)
          END IF
C
C               ASPHERICS
          IF(ALENS(8,I).EQ.1.0) THEN

              WRITE(OUTLYNE,13) ALENS(4,I),ALENS(5,I),
     1        ALENS(6,I),ALENS(7,I),ALENS(43,I)
              CALL SHOWIT(10)
 13           FORMAT('ASPH    ,',G23.15,',',G23.15,',',
     1        G23.15,',',G23.15,',',G23.15)
              WRITE(OUTLYNE,113) ALENS(81,I),ALENS(82,I),
     1        ALENS(83,I),ALENS(84,I),ALENS(85,I)
              CALL SHOWIT(10)
 113          FORMAT('ASPH2   ,',G23.15,',',G23.15,',',
     1        G23.15,',',G23.15,',',G23.15)
          END IF
C               CLEAR APERTURE
          IF(ALENS(9,I).EQ.1.0) THEN
C       CIRCULAR CLAP

              IF(ALENS(11,I).EQ.0.0D0) ALENS(11,I)=ALENS(10,I)
 14           FORMAT('CLAP    ,',G23.15,',',G23.15,',',G23.15,',',G23.15
     1        ,',',G23.15)
              WRITE(OUTLYNE,14)ALENS(10,I),ALENS(12,I),ALENS(13,I),ALENS(11,I)
     1        ,ALENS(14,I)
              CALL SHOWIT(10)
          END IF
          IF(ALENS(9,I).EQ.3.0) THEN
C       ELLIPTICAL CLAP

 16           FORMAT('CLAP     ELIP    ,',G23.15,',',G23.15,
     1                  ',',G23.15,',',G23.15)
              WRITE(OUTLYNE,16)ALENS(10,I),ALENS(11,I),ALENS(12,I),
     1        ALENS(13,I)
              CALL SHOWIT(10)
              IF(ALENS(15,I).NE.0.0) THEN
                  WRITE(OUTLYNE,15) ALENS(15,I)
                  CALL SHOWIT(10)
              END IF
 15           FORMAT('CLAP     TILT    ,',G23.15)
          END IF
          IF(ALENS(9,I).EQ.2.0) THEN
C       RECTANGULAR CLAP
 17           FORMAT('CLAP     RECT    ,',G23.15,',',G23.15,
     1                  ',',G23.15,',',G23.15)
              WRITE(OUTLYNE,17)ALENS(10,I),ALENS(11,I),ALENS(12,I),
     1        ALENS(13,I)
              CALL SHOWIT(10)
              IF(ALENS(15,I).NE.0.0) THEN

                  WRITE(OUTLYNE,15) ALENS(15,I)
                  CALL SHOWIT(10)
              END IF
          END IF
          IF(ALENS(9,I).EQ.4.0) THEN
C       RACETRACK CLAP
 18           FORMAT('CLAP     RCTK    ,',G23.15,',',G23.15,',',G23.15,
     1                  ',',G23.15,',',G23.15)
              WRITE(OUTLYNE,18)ALENS(10,I),ALENS(11,I),ALENS(12,I),
     1        ALENS(13,I),ALENS(14,I)
              CALL SHOWIT(10)
              IF(ALENS(15,I).NE.0.0) THEN

                  WRITE(OUTLYNE,15) ALENS(15,I)
                  CALL SHOWIT(10)
              END IF
          END IF
          IF(ALENS(9,I).EQ.5.0) THEN
C       POLY CLAP
 181          FORMAT('CLAP     POLY    ,',G23.15,',',G23.15,',',G23.15,
     1                  ',',G23.15,',',G23.15)
              WRITE(OUTLYNE,181)ALENS(10,I),ALENS(11,I),ALENS(12,I),
     1        ALENS(13,I),ALENS(14,I)
              CALL SHOWIT(10)
              IF(ALENS(15,I).NE.0.0) THEN

                  WRITE(OUTLYNE,15) ALENS(15,I)
                  CALL SHOWIT(10)
              END IF
          END IF
          IF(ALENS(9,I).EQ.6.0) THEN
C       IPOLY CLAP
 182          FORMAT('CLAP     IPOLY   ,',G23.15,',',G23.15,',',G23.15,
     1                  ',',G23.15,',',G23.15)
              WRITE(OUTLYNE,182)ALENS(10,I),ALENS(11,I),ALENS(12,I),
     1        ALENS(13,I),ALENS(14,I)
              CALL SHOWIT(10)
              IF(ALENS(15,I).NE.0.0) THEN

                  WRITE(OUTLYNE,15) ALENS(15,I)
                  CALL SHOWIT(10)
              END IF
          END IF
C     MULTCLAP
          IF(ALENS(127,I).NE.0.0D0) THEN
              DO J=1,INT(ALENS(127,I))
                  WRITE(OUTLYNE,1818) J,MULTCLAP(J,1,I),MULTCLAP(J,2,I)
     1            ,MULTCLAP(J,3,I)
                  CALL SHOWIT(10)
 1818             FORMAT('MULTCLAP ',I4,',',G23.15,',',G23.15,',',G23.15)
              END DO
          END IF
          IF(ALENS(51,I).EQ.1.0) THEN
C       CIRCULAR ERASE CLAP
 20           FORMAT('CLAP     ERASE   ,',G23.15,',',G23.15,',',G23.15)
              WRITE(OUTLYNE,20)ALENS(52,I),ALENS(54,I),ALENS(55,I)
              CALL SHOWIT(10)
          END IF
          IF(ALENS(51,I).EQ.3.0) THEN
C       ELLIPTICAL ERASE CLAP
 21           FORMAT('CLAP     ELIPE   ,',G23.15,',',G23.15,',',G23.15,
     1                  ',',G23.15)
              WRITE(OUTLYNE,21)ALENS(52,I),ALENS(53,I),ALENS(54,I),
     1        ALENS(55,I)
              CALL SHOWIT(10)
              IF(ALENS(57,I).NE.0.0) THEN

                  WRITE(OUTLYNE,15) ALENS(57,I)
                  CALL SHOWIT(10)
              END IF
          END IF
          IF(ALENS(51,I).EQ.2.0) THEN
C       RECTANGULAR ERASE CLAP
 22           FORMAT('CLAP     RECTE   ,',G23.15,',',G23.15,',',
     1                  G23.15,',',G23.15)
              WRITE(OUTLYNE,22)ALENS(52,I),ALENS(53,I),ALENS(54,I),
     1        ALENS(55,I)
              CALL SHOWIT(10)
              IF(ALENS(57,I).NE.0.0) THEN

                  WRITE(OUTLYNE,15) ALENS(57,I)
                  CALL SHOWIT(10)
              END IF
          END IF
          IF(ALENS(51,I).EQ.4.0) THEN
C       RACETRACK ERASE CLAP
 23           FORMAT('CLAP     RCTKE   ,',G23.15,',',G23.15,',',G23.15,
     1                  ',',G23.15,',',G23.15)
              WRITE(OUTLYNE,23)ALENS(52,I),ALENS(53,I),ALENS(54,I),
     1        ALENS(55,I),ALENS(56,I)
              CALL SHOWIT(10)
              IF(ALENS(57,I).NE.0.0) THEN

                  WRITE(OUTLYNE,15) ALENS(57,I)
                  CALL SHOWIT(10)
              END IF
          END IF
          IF(ALENS(51,I).EQ.5.0) THEN
C       POLY ERASE CLAP
 231          FORMAT('CLAP     POLYE   ,',G23.15,',',G23.15,',',G23.15,
     1                  ',',G23.15,',',G23.15)
              WRITE(OUTLYNE,231)ALENS(52,I),ALENS(53,I),ALENS(54,I),
     1        ALENS(55,I),ALENS(56,I)
              CALL SHOWIT(10)
              IF(ALENS(57,I).NE.0.0) THEN

                  WRITE(OUTLYNE,15) ALENS(57,I)
                  CALL SHOWIT(10)
              END IF
          END IF
          IF(ALENS(51,I).EQ.6.0) THEN
C       IPOLY ERASE CLAP
 232          FORMAT('CLAP     IPOLYE  ,',G23.15,',',G23.15,',',G23.15,
     1                  ',',G23.15,',',G23.15)
              WRITE(OUTLYNE,232)ALENS(52,I),ALENS(53,I),ALENS(54,I),
     1        ALENS(55,I),ALENS(56,I)
              CALL SHOWIT(10)
              IF(ALENS(57,I).NE.0.0) THEN

                  WRITE(OUTLYNE,15) ALENS(57,I)
                  CALL SHOWIT(10)
              END IF
          END IF
C               OBSCURATIONS
          IF(ALENS(16,I).EQ.1.0) THEN
C       CIRCULAR COBS
 25           FORMAT('COBS    ,',G23.15,',',G23.15,',',G23.15)
              WRITE(OUTLYNE,25)ALENS(17,I),ALENS(19,I),ALENS(20,I)
              CALL SHOWIT(10)
          END IF
          IF(ALENS(16,I).EQ.3.0) THEN
C       ELLIPTICAL COBS
 26           FORMAT('COBS     ELIP    ,',G23.15,',',G23.15,
     1                  ',',G23.15,',',G23.15)
              WRITE(OUTLYNE,26)ALENS(17,I),ALENS(18,I),ALENS(19,I),
     1        ALENS(20,I)
              CALL SHOWIT(10)
              IF(ALENS(22,I).NE.0.0) THEN
                  WRITE(OUTLYNE,27) ALENS(22,I)
                  CALL SHOWIT(10)
              END IF
 27           FORMAT('COBS     TILT    ,',G23.15)
          END IF
          IF(ALENS(16,I).EQ.2.0D0.AND.ALENS(134,I).EQ.0.0D0) THEN
C       RECTANGULAR COBS
 28           FORMAT('COBS     RECT    ,',G23.15,',',G23.15,
     1                  ',',G23.15,',',G23.15)
              WRITE(OUTLYNE,28)ALENS(17,I),ALENS(18,I),ALENS(19,I),
     1                  ALENS(20,I)
              CALL SHOWIT(10)
              IF(ALENS(22,I).NE.0.0) THEN
                  WRITE(OUTLYNE,27) ALENS(22,I)
                  CALL SHOWIT(10)
              END IF
          END IF
          IF(ALENS(16,I).EQ.4.0) THEN
C       RACETRACK COBS
 29           FORMAT('COBS     RCTK    ,',G23.15,',',G23.15,',',G23.15,
     1                  ',',G23.15,',',G23.15)
              WRITE(OUTLYNE,29)ALENS(17,I),ALENS(18,I),ALENS(19,I),
     1        ALENS(20,I),ALENS(21,I)
              CALL SHOWIT(10)
              IF(ALENS(22,I).NE.0.0) THEN

                  WRITE(OUTLYNE,27) ALENS(22,I)
                  CALL SHOWIT(10)
              END IF
          END IF
          IF(ALENS(16,I).EQ.5.0) THEN
C       POLY COBS
 291          FORMAT('COBS     POLY    ,',G23.15,',',G23.15,',',G23.15,
     1                  ',',G23.15,',',G23.15)
              WRITE(OUTLYNE,291)ALENS(17,I),ALENS(18,I),ALENS(19,I),
     1        ALENS(20,I),ALENS(21,I)
              CALL SHOWIT(10)
              IF(ALENS(22,I).NE.0.0) THEN

                  WRITE(OUTLYNE,27) ALENS(22,I)
                  CALL SHOWIT(10)
              END IF
          END IF
          IF(ALENS(16,I).EQ.6.0) THEN
C       IPOLY COBS
 292          FORMAT('COBS     IPOLY   ,',G23.15,',',G23.15,',',G23.15,
     1                  ',',G23.15,',',G23.15)
              WRITE(OUTLYNE,292)ALENS(17,I),ALENS(18,I),ALENS(19,I),
     1        ALENS(20,I),ALENS(21,I)
              CALL SHOWIT(10)
              IF(ALENS(22,I).NE.0.0) THEN

                  WRITE(OUTLYNE,27) ALENS(22,I)
                  CALL SHOWIT(10)
              END IF
          END IF
C
C     MULTCOBS
          IF(ALENS(128,I).NE.0.0D0.AND.ALENS(134,I).EQ.0.0D0) THEN
              DO J=1,INT(ALENS(128,I))
                  WRITE(OUTLYNE,1819) J,MULTCOBS(J,1,I),MULTCOBS(J,2,I)
     1            ,MULTCOBS(J,3,I)
                  CALL SHOWIT(10)
 1819             FORMAT('MULTCOBS ',I4,',',G23.15,',',G23.15,',',G23.15)
              END DO
          END IF
C
C     SPIDER
          IF(ALENS(134,I).NE.0.0D0) THEN
              WRITE(OUTLYNE,1820) ALENS(135,I),ALENS(136,I),ALENS(137,I)
              CALL SHOWIT(10)
 1820         FORMAT('SPIDER ,',G23.15,',',G23.15,',',G23.15)
          END IF
          IF(ALENS(61,I).EQ.1.0) THEN
C       CIRCULAR ERASE COBS

 31           FORMAT('COBS     ERASE   ,',G23.15,',',G23.15,',',G23.15)
              WRITE(OUTLYNE,31)ALENS(62,I),ALENS(64,I),ALENS(65,I)
              CALL SHOWIT(10)
          END IF
          IF(ALENS(61,I).EQ.3.0) THEN
C       ELLIPTICAL ERASE COBS

 32           FORMAT('COBS     ELIPE   ,',G23.15,',',G23.15,',',G23.15,
     1                  ',',G23.15)
              WRITE(OUTLYNE,32)ALENS(62,I),ALENS(63,I),ALENS(64,I),
     1        ALENS(65,I)
              CALL SHOWIT(10)
              IF(ALENS(67,I).NE.0.0) THEN

                  WRITE(OUTLYNE,27) ALENS(67,I)
                  CALL SHOWIT(10)
              END IF
          END IF
          IF(ALENS(61,I).EQ.2.0) THEN
C       RECTANGULAR ERACE COBS

 33           FORMAT('COBS     RECTE   ,',G23.15,',',G23.15,',',
     1                  G23.15,',',G23.15)
              WRITE(OUTLYNE,33)ALENS(62,I),ALENS(63,I),ALENS(64,I),
     1        ALENS(65,I)
              CALL SHOWIT(10)
              IF(ALENS(67,I).NE.0.0) THEN

                  WRITE(OUTLYNE,27) ALENS(67,I)
                  CALL SHOWIT(10)
              END IF
          END IF
          IF(ALENS(61,I).EQ.4.0) THEN
C       RACETRACK ERASE CLAP
 34           FORMAT('COBS     RCTKE   ,',G23.15,',',G23.15,',',G23.15,
     1                  ',',G23.15,',',G23.15)
              WRITE(OUTLYNE,34)ALENS(62,I),ALENS(63,I),ALENS(64,I),
     1        ALENS(65,I),ALENS(66,I)
              CALL SHOWIT(10)
              IF(ALENS(67,I).NE.0.0) THEN

                  WRITE(OUTLYNE,27) ALENS(67,I)
                  CALL SHOWIT(10)
              END IF
          END IF
          IF(ALENS(61,I).EQ.5.0) THEN
C       POLY ERASE CLAP
 341          FORMAT('COBS     POLYE   ,',G23.15,',',G23.15,',',G23.15,
     1                  ',',G23.15,',',G23.15)
              WRITE(OUTLYNE,341)ALENS(62,I),ALENS(63,I),ALENS(64,I),
     1        ALENS(65,I),ALENS(66,I)
              CALL SHOWIT(10)
              IF(ALENS(67,I).NE.0.0) THEN

                  WRITE(OUTLYNE,27) ALENS(67,I)
                  CALL SHOWIT(10)
              END IF
          END IF
          IF(ALENS(61,I).EQ.6.0) THEN
C       IPOLY ERASE CLAP
 342          FORMAT('COBS     IPOLYE  ,',G23.15,',',G23.15,',',G23.15,
     1                  ',',G23.15,',',G23.15)
              WRITE(OUTLYNE,342)ALENS(62,I),ALENS(63,I),ALENS(64,I),
     1        ALENS(65,I),ALENS(66,I)
              CALL SHOWIT(10)
              IF(ALENS(67,I).NE.0.0) THEN

                  WRITE(OUTLYNE,27) ALENS(67,I)
                  CALL SHOWIT(10)
              END IF
          END IF
C
C
C       NOW TORICS IF PRESENT
C
          IF(ALENS(23,I).NE.0.0) THEN
C       TORICS PRESENT
              IF(ALENS(23,I).EQ.1.0) THEN

                  WRITE(OUTLYNE,36)
                  CALL SHOWIT(10)

                  IF(.NOT.RDOUT) THEN
                      WRITE(OUTLYNE,38) ALENS(24,I)
                  ELSE
                      IF(ALENS(24,I).NE.0.0D0) WRITE(OUTLYNE,39) 1.0D0/ALENS(24,I)
                      IF(ALENS(24,I).EQ.0.0D0) WRITE(OUTLYNE,38) ALENS(24,I)
                  END IF
                  CALL SHOWIT(10)
              END IF
              IF(ALENS(23,I).EQ.2.0) THEN

                  WRITE(OUTLYNE,37)
                  CALL SHOWIT(10)

                  WRITE(OUTLYNE,38) ALENS(24,I)
                  CALL SHOWIT(10)
              END IF
          END IF
 36       FORMAT('YTORIC  ')
 38       FORMAT('CVTOR   ,',G23.15)
 37       FORMAT('XTORIC  ')
 39       FORMAT('RDTOR   ,',G23.15)
C
C       ROOF, CORNER CUBE AND RAYERROR SURFACES
          IF(ALENS(126,I).EQ.1.0D0) THEN
              WRITE(OUTLYNE,1121) ALENS(138,I),ALENS(139,I)
              CALL SHOWIT(10)
          END IF
          IF(ALENS(126,I).EQ.2.0D0) THEN
              WRITE(OUTLYNE,1122) ALENS(138,I),ALENS(139,I),ALENS(140,I)
     1        ,ALENS(141,I)
              CALL SHOWIT(10)
          END IF
          WRITE(OUTLYNE,1123) ALENS(144,I)
          CALL SHOWIT(10)
 1121     FORMAT('ROO ',G23.15,',',G23.15)
 1122     FORMAT('CCR ',G23.15,',',G23.15,','
     1    ,G23.15,',',G23.15)
 1123     FORMAT('RAYERROR',G23.15)
C
C       ANAMORPHIC CONIC AND ASPHERIC TERMS
C
          IF(ALENS(41,I).NE.0.0) THEN

              WRITE(OUTLYNE,40) ALENS(41,I)
              CALL SHOWIT(10)
 40           FORMAT('CCTOR   ,',G23.15)
          END IF
          IF(ALENS(36,I).NE.0.0) THEN
C       ANAMORPHIC ASPHERICS
              WRITE(OUTLYNE,41) ALENS(37,I),ALENS(38,I),ALENS(39,I),ALENS(40,I)
              CALL SHOWIT(10)
 41           FORMAT('TASPH   ,',G23.15,',',G23.15,',',G23.15,
     1        ',',G23.15)
          END IF
C
C       TILTS AND DECENTRATIONS
          IF(ALENS(29,I).NE.0.0D0) THEN
C       SURFACE DECENTER
              WRITE(OUTLYNE,106) ALENS(115,I),ALENS(114,I),ALENS(116,I)
              CALL SHOWIT(10)
 106          FORMAT('DEC     ,',G23.15,',',G23.15,',',G23.15)
          END IF
C
          IF(ALENS(25,I).NE.0.0D0) THEN
C       TILTS
              IF(ALENS(25,I).EQ.1.0D0.AND.ALENS(77,I).EQ.1.0D0) THEN
                  WRITE(OUTLYNE,1007) ALENS(70,I)
                  CALL SHOWIT(10)
                  IF(ALENS(90,I).NE.0.0D0) THEN
                      WRITE(OUTLYNE,9001) ALENS(90,I)
                      CALL SHOWIT(10)
                  END IF
                  IF(ALENS(91,I).NE.0.0D0) THEN
                      WRITE(OUTLYNE,9002) ALENS(91,I)
                      CALL SHOWIT(10)
                  END IF
                  IF(ALENS(92,I).NE.0.0D0) THEN
                      WRITE(OUTLYNE,9003) ALENS(92,I)
                      CALL SHOWIT(10)
                  END IF
                  IF(ALENS(93,I).NE.0.0D0) THEN
                      WRITE(OUTLYNE,9004) ALENS(93,I)
                      CALL SHOWIT(10)
                  END IF
                  IF(ALENS(94,I).NE.0.0D0) THEN
                      WRITE(OUTLYNE,9005) ALENS(94,I)
                      CALL SHOWIT(10)
                  END IF
                  IF(ALENS(95,I).NE.0.0D0) THEN
                      WRITE(OUTLYNE,9006) ALENS(95,I)
                      CALL SHOWIT(10)
                  END IF
              ELSE
                  IF(ALENS(25,I).EQ.1.0D0)
     1            WRITE(OUTLYNE,102) ALENS(118,I),ALENS(119,I),ALENS(120,I)
                  IF(ALENS(25,I).EQ.-1.0D0)
     1            WRITE(OUTLYNE,103) ALENS(118,I),ALENS(119,I),ALENS(120,I)
                  IF(ALENS(25,I).EQ.2.0D0)
     1            WRITE(OUTLYNE,104) ALENS(118,I),ALENS(119,I),ALENS(120,I)
                  IF(ALENS(25,I).EQ.3.0D0)
     1            WRITE(OUTLYNE,105) ALENS(118,I),ALENS(119,I),ALENS(120,I)
                  IF(ALENS(25,I).EQ.4.0D0)
     1            WRITE(OUTLYNE,1005) ALENS(118,I),ALENS(119,I),ALENS(120,I)
                  IF(ALENS(25,I).EQ.5.0D0)
     1            WRITE(OUTLYNE,1006) ALENS(118,I),ALENS(119,I),ALENS(120,I)
                  IF(ALENS(25,I).EQ.7.0D0)
     1            WRITE(OUTLYNE,7674) ALENS(118,I),ALENS(119,I),ALENS(120,I)
                  CALL SHOWIT(10)
              END IF
C
 102          FORMAT('TILT    ,',G23.15,',',G23.15,',',G23.15)
 103          FORMAT('RTILT   ,',G23.15,',',G23.15,',',G23.15)
 104          FORMAT('TILT     AUTO    ,',G23.15,',',G23.15,',',G23.15)
 105          FORMAT('TILT     AUTOM   ,',G23.15,',',G23.15,',',G23.15)
 1005         FORMAT('TILT     BEN     ,',G23.15,',',G23.15,',',G23.15)
 1006         FORMAT('TILT     DAR     ,',G23.15,',',G23.15,',',G23.15)
 1007         FORMAT('TILT     RET     ,',G23.15,',',G23.15,',',G23.15
     1        ,',',G23.15)
              IF(ALENS(25,I).EQ.1.0D0.AND.ALENS(77,I).EQ.1.0D0) THEN
                  WRITE(OUTLYNE,1007) ALENS(70,I)
                  CALL SHOWIT(10)
                  IF(ALENS(90,I).NE.0.0D0) THEN
                      WRITE(OUTLYNE,9001) ALENS(90,I)
                      CALL SHOWIT(10)
 9001                 FORMAT('GDX     ,',G23.15)
                  END IF
                  IF(ALENS(91,I).NE.0.0D0) THEN
                      WRITE(OUTLYNE,9002) ALENS(91,I)
                      CALL SHOWIT(10)
 9002                 FORMAT('GDY     ,',G23.15)
                  END IF
                  IF(ALENS(92,I).NE.0.0D0) THEN
                      WRITE(OUTLYNE,9003) ALENS(92,I)
                      CALL SHOWIT(10)
 9003                 FORMAT('GDZ     ,',G23.15)
                  END IF
                  IF(ALENS(93,I).NE.0.0D0) THEN
                      WRITE(OUTLYNE,9004) ALENS(93,I)
                      CALL SHOWIT(10)
 9004                 FORMAT('GALPHA  ,',G23.15)
                  END IF
                  IF(ALENS(94,I).NE.0.0D0) THEN
                      WRITE(OUTLYNE,9005) ALENS(94,I)
                      CALL SHOWIT(10)
 9005                 FORMAT('GBETA   ,',G23.15)
                  END IF
                  IF(ALENS(95,I).NE.0.0D0) THEN
                      WRITE(OUTLYNE,9006) ALENS(95,I)
                      CALL SHOWIT(10)
 9006                 FORMAT('GGAMMA  ,',G23.15)
                  END IF
              ELSE
                  IF(ALENS(25,I).EQ.1.0D0)
     1            WRITE(OUTLYNE,6669) ALENS(118,I),ALENS(119,I),ALENS(120,I)
                  IF(ALENS(25,I).EQ.-1.0D0)
     1            WRITE(OUTLYNE,6670) ALENS(118,I),ALENS(119,I),ALENS(120,I)
                  IF(ALENS(25,I).EQ.2.0D0)
     1            WRITE(OUTLYNE,6671) ALENS(118,I),ALENS(119,I),ALENS(120,I)
                  IF(ALENS(25,I).EQ.3.0D0)
     1            WRITE(OUTLYNE,6672) ALENS(118,I),ALENS(119,I),ALENS(120,I)
                  IF(ALENS(25,I).EQ.4.0D0)
     1            WRITE(OUTLYNE,6673) ALENS(118,I),ALENS(119,I),ALENS(120,I)
                  IF(ALENS(25,I).EQ.5.0D0)
     1            WRITE(OUTLYNE,6674) ALENS(118,I),ALENS(119,I),ALENS(120,I)
                  IF(ALENS(25,I).EQ.7.0D0)
     1            WRITE(OUTLYNE,7674) ALENS(118,I),ALENS(119,I),ALENS(120,I)
              END IF
C
 6669         FORMAT('TILT    ,',G23.15,',',G23.15,',',G23.15)
 6670         FORMAT('RTILT   ,',G23.15,',',G23.15,',',G23.15)
 6671         FORMAT('TILT     AUTO    ,',G23.15,',',G23.15,',',G23.15)
 6672         FORMAT('TILT     AUTOM   ,',G23.15,',',G23.15,',',G23.15)
 6673         FORMAT('TILT     BEN     ,',G23.15,',',G23.15,',',G23.15)
 6674         FORMAT('TILT     DAR     ,',G23.15,',',G23.15,',',G23.15)
 7674         FORMAT('TILT     REV     ,',G23.15,',',G23.15,',',G23.15)
          END IF
          IF(ALENS(59,I).NE.0.0D0) THEN
C       SURFACE PIVOT
              WRITE(OUTLYNE,1061) ALENS(78,I),ALENS(79,I),ALENS(80,I)
              CALL SHOWIT(10)
 1061         FORMAT('PIVOT   ,',G23.15,',',G23.15,',',G23.15)
          END IF
C
C
C       REAL OR PARAXAIL SURFACE
          IF(ALENS(124,I).EQ.1.0D0) THEN
C       PARAXIAL SURFACE
              WRITE(OUTLYNE,4711)
              CALL SHOWIT(10)
4711          FORMAT('PARAX')
          END IF
C
C       SPECIFIC GRAVITY
          WRITE(OUTLYNE,4701) DABS(ALENS(102,I))
          IF(ALENS(102,I).NE.0.0D0)
     1    CALL SHOWIT(10)
 4701     FORMAT('SPGR,',G23.15)
C
C       MIRROR THICKNESS
          IF(DABS(ALENS(110,I)).GT.0.0D0) THEN
              IF(GLANAM(I,1).EQ.'             '.AND.
     1        GLANAM(I,2).EQ.'REFL         ') THEN
                  WRITE(OUTLYNE,4702) DABS(ALENS(110,I))
                  CALL SHOWIT(10)
              END IF
          END IF
 4702     FORMAT('THM,',G23.15)
C
C       PRICE
          WRITE(OUTLYNE,4703) DABS(ALENS(111,I))
          IF(ALENS(111,I).NE.0.0D0) CALL SHOWIT(10)
 4703     FORMAT('PRICE,',G23.15)
C
C       FOOTBLOK
          IF(ALENS(58,I).EQ.1.0) THEN
C       SET FOOTBLOK
              WRITE(OUTLYNE,4747)
              CALL SHOWIT(10)
 4747         FORMAT('FOOTBLOK ON')
          END IF
C
C       PIVAXIS
          IF(ALENS(113,I).EQ.1.0D0) THEN
              WRITE(OUTLYNE,4748)
              CALL SHOWIT(10)
 4748         FORMAT('PIVAXIS NORMAL')
          END IF
C       SURFACE FORCED DUMMY
          IF(ALENS(68,I).EQ.1.0D0) THEN
              WRITE(OUTLYNE,1471)
              CALL SHOWIT(10)
 1471         FORMAT('NODUM YES')
          END IF
          IF(ALENS(143,I).EQ.1.0D0) THEN
C     INR IS EXPLICITLY SET
              WRITE(OUTLYNE,475) ALENS(76,I)
              CALL SHOWIT(10)
 475          FORMAT('INR     ,',G23.15)
          END IF
C       NOW ASTOP AND REF
          IF(I.EQ.INT(SYSTEM1(25))) THEN
C       SURFACE SHOULD BE THE REFERENCE SURFACE
              WRITE(OUTLYNE,94)
              CALL SHOWIT(10)
          END IF
          IF(I.EQ.INT(SYSTEM1(26))) THEN
C       I IS THE ASTOP SURFACE, IS THERE AN ADJUSTMENT ?
              IF(SYSTEM1(27).EQ.0.0)  WRITE(OUTLYNE,95)
              IF(SYSTEM1(27).EQ.1.0)  WRITE(OUTLYNE,96)
              IF(SYSTEM1(27).EQ.-1.0) WRITE(OUTLYNE,97)
              IF(SYSTEM1(27).EQ.2.0)  WRITE(OUTLYNE,98)
              IF(SYSTEM1(27).GE.-1.0D0.AND.SYSTEM1(27).LE.2.0D0)
     1        CALL SHOWIT(10)
 94           FORMAT('REFS')
 95           FORMAT('ASTOP')
 96           FORMAT('ASTOP    EN')
 97           FORMAT('ASTOP    EX')
 98           FORMAT('ASTOP    ENEX')
          END IF
C
          IF(GLANAM(I,1).EQ.' '.AND.GLANAM(I,2).EQ.'AIR'.OR.
     1    GLANAM(I,1).EQ.' '.AND.GLANAM(I,2).EQ.'LAST SURFACE') THEN
              WRITE(OUTLYNE,110)
              CALL SHOWIT(10)
              RETURN
          END IF
C
          IF(GLANAM(I,1).EQ.' '.AND.GLANAM(I,2).EQ.'PERFECT') THEN
              WRITE(OUTLYNE,1201)
              CALL SHOWIT(10)
              RETURN
          END IF
C
          IF(GLANAM(I,1).EQ.' '.AND.GLANAM(I,2).EQ.'IDEAL') THEN
              WRITE(OUTLYNE,11201) ALENS(121,I)
              CALL SHOWIT(10)
              RETURN
          END IF
          IF(GLANAM(I,1).EQ.' '.AND.GLANAM(I,2).EQ.'REFL') THEN
              WRITE(OUTLYNE,120)
              CALL SHOWIT(10)
              RETURN
          END IF
          IF(GLANAM(I,1).EQ.' '.AND.GLANAM(I,2).EQ.'REFLTIRO') THEN
              WRITE(OUTLYNE,2120)
              CALL SHOWIT(10)
              RETURN
          END IF
          IF(GLANAM(I,1).EQ.' '.AND.GLANAM(I,2).EQ.'REFLTIR') THEN
              WRITE(OUTLYNE,3120)
              CALL SHOWIT(10)
              RETURN
          END IF
          IF(GLANAM(I,1).EQ.'GLASS') THEN
              WRITE(OUTLYNE,100)GLANAM(I,1)(1:8),GLANAM(I,2)(1:8),ALENS(46,I),
     1        ALENS(47,I),ALENS(48,I),ALENS(49,I),ALENS(50,I)
              CALL SHOWIT(10)
          END IF
          IF(GLANAM(I,1).EQ.'GLASS') THEN
              WRITE(OUTLYNE,1011) ALENS(71,I)
              CALL SHOWIT(10)
              WRITE(OUTLYNE,1012) ALENS(72,I)
              CALL SHOWIT(10)
              WRITE(OUTLYNE,1013) ALENS(73,I)
              CALL SHOWIT(10)
              WRITE(OUTLYNE,1014) ALENS(74,I)
              CALL SHOWIT(10)
              WRITE(OUTLYNE,1015) ALENS(75,I)
              CALL SHOWIT(10)
 1011         FORMAT('N6      ,',G23.15)
 1012         FORMAT('N7      ,',G23.15)
 1013         FORMAT('N8      ,',G23.15)
 1014         FORMAT('N9      ,',G23.15)
 1015         FORMAT('N10     ,',G23.15)
              RETURN
          END IF
C     IS IT "MODEL" ?
          IF(GLANAM(I,1).EQ.'MODEL') THEN
              WRITE(OUTLYNE,7676)GLANAM(I,1)(1:8),GLANAM(I,2)(1:8),
     1        ALENS(86,I),ALENS(87,I),ALENS(89,I)
              CALL SHOWIT(10)
          END IF
          IF(GLANAM(I,1).NE.'GLASS'.AND.GLANAM(I,1).NE.' '
     1    .AND.GLANAM(I,2).NE.'AIR'.AND.GLANAM(I,2).NE.
     2    'REFL'.AND.GLANAM(I,1).NE.'MODEL'.AND.GLANAM(I,2).NE.
     3    'PERFECT'.AND.GLANAM(I,2).NE.'REFLTIRO'.AND.
     3    GLANAM(I,2).NE.'REFLTIR') THEN
              WRITE(OUTLYNE,101)GLANAM(I,1)(1:8),GLANAM(I,2)
              CALL SHOWIT(10)
              RETURN
          END IF
 100      FORMAT(A8,' ',A8,',',G23.15,',',G23.15,',',G23.15,',',
     1    G23.15,',',G23.15)
          !6676   FORMAT(A8,' ',A8,',',G23.15,',',G23.15,',',G23.15)
 7676     FORMAT(A8,' ',A8,',',G23.15,',',G23.15,',',G23.15)
! 6677   FORMAT(G23.15,',',G23.15)
 101      FORMAT(A8,' ',A13)
 110      FORMAT('AIR')
 120      FORMAT('REFL')
 2120     FORMAT('REFLTIRO')
 3120     FORMAT('REFLTIR')
 1201     FORMAT('PERFECT')
11201     FORMAT('IDEAL , ',G23.15)
C
          RETURN
      END
