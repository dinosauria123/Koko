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

C       THIRD COLLECTION OF NSS FILES

C SUB NSSLENO.FOR
      SUBROUTINE NSSLENO
          USE NSSMOD
C
          IMPLICIT NONE
          CHARACTER NSSUNITS*2,STYPE*8
          INTEGER J,K,I
          REAL*8 RD
C
C       THIS ROUTINE OUTPUTS THE CURRENT NSS DATABASE IN A FORM READABLE BY THE
C       PROGRAM

          INCLUDE 'datmai.inc'
C       START NSS DATABASE
          WRITE(OUTLYNE,10)
 10       FORMAT('NSSNEW')
          CALL SHOWIT(10)
C       NSSUNITS
          IF(NSSSYSTEM(21).EQ.1.0D0) NSSUNITS='IN'
          IF(NSSSYSTEM(21).EQ.2.0D0) NSSUNITS='CM'
          IF(NSSSYSTEM(21).EQ.3.0D0) NSSUNITS='MM'
          IF(NSSSYSTEM(21).EQ.4.0D0) NSSUNITS=' M'
          WRITE(OUTLYNE,11) NSSUNITS
          CALL SHOWIT(10)
 11       FORMAT('NSSUNITS ',A2)
C       WAVELENGTHS
          DO I=1,10
              WRITE(OUTLYNE,12) I,NSSSYSTEM(I)
              CALL SHOWIT(10)
          END DO
 12       FORMAT('NSSWV ,',I2,1X,D23.15)
C       SPECTRAL WEIGHTS
          DO I=1,10
              WRITE(OUTLYNE,13) I,NSSSYSTEM(I+10)
              CALL SHOWIT(10)
          END DO
 13       FORMAT('NSSWT ,',I2,1X,D23.15)
          WRITE(OUTLYNE,14) NSSSYSTEM(25)
          CALL SHOWIT(10)
 14       FORMAT('UNIVERSE',',',D23.15)
C       MIN RAY ENERGY
          WRITE(OUTLYNE,15) NSSSYSTEM(26)
          CALL SHOWIT(10)
 15       FORMAT('NSSMINE',D23.15)
C       MAX HAY INTERSECTIONS PER SURFACE
          WRITE(OUTLYNE,16) DBLE(INT(NSSSYSTEM(27)))
          CALL SHOWIT(10)
 16       FORMAT('NSSMHIT',D23.15)
C       RAY SPLITING
          IF(NSSSYSTEM(28).EQ.1.0D0) WRITE(OUTLYNE,17)
          IF(NSSSYSTEM(28).EQ.0.0D0) WRITE(OUTLYNE,18)
          CALL SHOWIT(10)
 17       FORMAT('NSSSPLIT ON')
 18       FORMAT('NSSSPLIT OFF')
C       SOURCE POSITION
          IF(NSSSYSTEM(34).EQ.1.0D0) THEN
              WRITE(OUTLYNE,19) NSSSYSTEM(29),NSSSYSTEM(30),NSSSYSTEM(31)
              CALL SHOWIT(10)
 19           FORMAT('NSSOBJ REAL',',',D23.15,',',D23.15,',',D23.15)
          END IF
          IF(NSSSYSTEM(34).EQ.-1.0D0) THEN
              WRITE(OUTLYNE,191) NSSSYSTEM(29),NSSSYSTEM(30),NSSSYSTEM(31)
              CALL SHOWIT(10)
 191          FORMAT('NSSOBJ VIRTUAL',',',D23.15,',',D23.15,',',D23.15)
          END IF
C       SOURCE GRID DIMENSION
          IF(NSSSYSTEM(33).EQ.0.0D0)
     1    WRITE(OUTLYNE,20) DBLE(INT(NSSSYSTEM(32))),NSSSYSTEM(37)
          IF(NSSSYSTEM(33).EQ.1.0D0)
     1    WRITE(OUTLYNE,202) DBLE(INT(NSSSYSTEM(32))),NSSSYSTEM(37)
          CALL SHOWIT(10)
 20       FORMAT('NSSGRIDS CIRC',',',D23.15,',',D23.15)
 202      FORMAT('NSSGRIDS RECT',',',D23.15,',',D23.15)
C       REFERENCE GRID POSITION
          WRITE(OUTLYNE,195) NSSSYSTEM(40),NSSSYSTEM(41),NSSSYSTEM(42)
          CALL SHOWIT(10)
 195      FORMAT('NSSREF',',',D23.15,',',D23.15,',',D23.15)
C       REFERENCE GRID DIMENSION
          IF(NSSSYSTEM(22).EQ.0.0D0)
     1    WRITE(OUTLYNE,22) DBLE(INT(NSSSYSTEM(36))),NSSSYSTEM(38)
          IF(NSSSYSTEM(22).EQ.1.0D0)
     1    WRITE(OUTLYNE,229) DBLE(INT(NSSSYSTEM(36))),NSSSYSTEM(38)
          CALL SHOWIT(10)
 22       FORMAT('NSSGRIDR CIRC',',',D23.15,',',D23.15)
 229      FORMAT('NSSGRIDR RECT',',',D23.15,',',D23.15)
C       REFERENCE GRID APODIZATION
          WRITE(OUTLYNE,223) NSSSYSTEM(23)
          CALL SHOWIT(10)
 223      FORMAT('NSSAPODR',',',D23.15)
C       MEDIA
          WRITE(OUTLYNE,221) NSSOGLASS(1),NSSOGLASS(2)
          CALL SHOWIT(10)
 221      FORMAT('OBJMEDIA ',A13,1X,A13)
C
C       NOW FOR THE SURFACE DATA
          DO J=1,MAXS
C       FIND OUT IF THE CURRENT SURFACE #
C       IS SUPPOSED TO BE IN THE NSS DATABASE
              IF(NSSALENS(100,J).EQ.1.0D0) THEN
C       OUTPUT SURFACE
                  WRITE(OUTLYNE,87) DBLE(J)
                  CALL SHOWIT(10)
 87               FORMAT('SURFACE',',',D23.15)
C       OUTPUT SURFACE NAME
                  IF(NSSSNAM(J)(1:20).NE.'                    ')THEN
                      WRITE(OUTLYNE,118) NSSSNAM(J)(1:80)
                      CALL SHOWIT(10)
                  END IF
 118              FORMAT('SNAME ',A80)
C       DETECTOR SURFACE DEFINITION
                  IF(NSSSYSTEM(49).EQ.DBLE(J).AND.J.GT.0) THEN
                      WRITE(OUTLYNE,87) DBLE(J)
                      CALL SHOWIT(10)
                      WRITE(OUTLYNE,222)
 222                  FORMAT('NSSDET')
                  END IF
C       OUTPUT SURFACE DATA FOR THIS SURFACE
                  IF(NSSALENS(1,J).EQ.1.0D0)  STYPE='PLANO   '
                  IF(NSSALENS(1,J).EQ.2.0D0)  STYPE='SPHERIC '
                  IF(NSSALENS(1,J).EQ.3.0D0)  STYPE='ANAMORPH'
                  IF(NSSALENS(1,J).EQ.4.0D0)  STYPE='USER    '
                  IF(NSSALENS(1,J).EQ.5.0D0)  STYPE='MEM'
                  IF(NSSALENS(1,J).EQ.6.0D0)  STYPE='TUBE'
                  IF(NSSALENS(1,J).EQ.1.0D0) THEN
                      WRITE(OUTLYNE,100) STYPE,NSSALENS(3,J),NSSALENS(4,J)
     1                ,NSSALENS(5,J),NSSALENS(6,J),NSSALENS(7,J)
                  END IF
                  IF(NSSALENS(1,J).EQ.2.0D0) THEN
                      IF(NSSALENS(3,J).NE.0.0D0) RD=1/NSSALENS(3,J)
                      IF(NSSALENS(3,J).EQ.0.0D0) RD=NSSALENS(3,J)
                      WRITE(OUTLYNE,100) STYPE,RD,NSSALENS(4,J)
     1                ,NSSALENS(5,J),NSSALENS(6,J),NSSALENS(7,J)
                  END IF
                  IF(NSSALENS(1,J).EQ.3.0D0) THEN
                      IF(NSSALENS(3,J).NE.0.0D0) RD=1/NSSALENS(3,J)
                      IF(NSSALENS(3,J).EQ.0.0D0) RD=NSSALENS(3,J)
                      WRITE(OUTLYNE,100) STYPE,RD,NSSALENS(4,J)
     1                ,NSSALENS(5,J),NSSALENS(6,J),NSSALENS(7,J)
                  END IF
                  IF(NSSALENS(1,J).EQ.4.0D0) THEN
                      WRITE(OUTLYNE,100) STYPE,NSSALENS(3,J),NSSALENS(4,J)
     1                ,NSSALENS(5,J),NSSALENS(6,J),NSSALENS(7,J)
                  END IF
                  IF(NSSALENS(1,J).EQ.5.0D0) THEN
                      WRITE(OUTLYNE,100) STYPE,NSSALENS(3,J),NSSALENS(4,J)
     1                ,NSSALENS(5,J),NSSALENS(6,J),NSSALENS(7,J)
                  END IF
                  IF(NSSALENS(1,J).EQ.6.0D0) THEN
                      WRITE(OUTLYNE,1001) STYPE,NSSALENS(10,J),NSSALENS(11,J)
                  END IF
                  CALL SHOWIT(10)
 1001             FORMAT('SPROFILE ',A8,',',D23.15,',',D23.15)
 100              FORMAT('SPROFILE ',A8,',',D23.15,',',D23.15,',',D23.15,
     1            ',',D23.15,',',D23.15)
! 900    FORMAT('SPROFILE ',A8,',',D23.15,',',D23.15,',',D23.15,
!     1  ',',D23.15,',',D23.15)
C       PARAMETER
                  DO K=1,200
                      IF(NSSALENS(K+200,J).NE.0.0D0) THEN
C       OUTPUT A SURFACE PARAMETER
                          WRITE(OUTLYNE,101) K,NSSALENS(K+200,J)
                          CALL SHOWIT(10)
 101                      FORMAT('SPARAM',',',I3,',',D23.15)
                      ELSE
C       PARAMETER IS ZERO
                      END IF
                  END DO

C       OUTPUT LINEAR GRATING DATA FOR THIS SURFACE
                  IF(NSSALENS(146,J).NE.0.0D0) THEN
                      WRITE(OUTLYNE,200) NSSALENS(141,J),NSSALENS(142,J)
     1                ,NSSALENS(143,J),NSSALENS(144,J),NSSALENS(145,J)
                      CALL SHOWIT(10)
 200                  FORMAT('SGRT ',D23.15,',',D23.15,',',D23.15,
     1                ',',D23.15,',',D23.15)
                  ELSE
C       NO GRATING DATA
                  END IF
C NOW CLEAR APERTURES
                  IF(NSSALENS(1,J).NE.6.0D0) THEN
C       NOT IF A TUBE SURFACE
                      IF(NSSALENS(19,J).NE.0.0D0) THEN
                          IF(NSSALENS(19,J).EQ.1.0D0) THEN
                              WRITE(OUTLYNE,102) NSSALENS(20,J),NSSALENS(21,J)
     1                        ,NSSALENS(22,J),NSSALENS(23,J),NSSALENS(24,J)
                              CALL SHOWIT(10)
 102                          FORMAT('SCLAP',',',D23.15,',',D23.15,',',D23.15,',',D23.15
     1                        ,',',D23.15)
                          END IF
                          IF(NSSALENS(19,J).EQ.2.0D0) THEN
                              WRITE(OUTLYNE,103) NSSALENS(20,J),NSSALENS(21,J)
     1                        ,NSSALENS(22,I),NSSALENS(23,J),NSSALENS(24,J)
                              CALL SHOWIT(10)
 103                          FORMAT('SCLAP RECT',',',D23.15,',',D23.15,',',D23.15,',',D23.15
     1                        ,',',D23.15)
                          END IF
                          IF(NSSALENS(19,J).EQ.3.0D0) THEN
                              WRITE(OUTLYNE,104) NSSALENS(20,J),NSSALENS(21,J)
     1                        ,NSSALENS(22,J),NSSALENS(23,J),NSSALENS(24,J)
                              CALL SHOWIT(10)
 104                          FORMAT('SCLAP ELIP',',',D23.15,',',D23.15,',',D23.15,',',D23.15
     1                        ,',',D23.15)
                          END IF
                      END IF
                  END IF
                  IF(NSSALENS(1,J).NE.6.0D0) THEN
C       NOT IF A TUBE SURFACE
C NOW HOLES
                      IF(NSSALENS(25,J).NE.0.0D0) THEN
                          IF(NSSALENS(25,J).EQ.1.0D0) THEN
                              WRITE(OUTLYNE,108) NSSALENS(26,J),NSSALENS(27,J)
     1                        ,NSSALENS(28,J),NSSALENS(29,J),NSSALENS(30,J)
                              CALL SHOWIT(10)
 108                          FORMAT('SHOLE',',',D23.15,',',D23.15,',',D23.15,',',D23.15
     1                        ,',',D23.15)
                          END IF
                          IF(NSSALENS(25,J).EQ.2.0D0) THEN
                              WRITE(OUTLYNE,109) NSSALENS(26,J),NSSALENS(27,J)
     1                        ,NSSALENS(28,J),NSSALENS(29,J),NSSALENS(30,J)
                              CALL SHOWIT(10)
 109                          FORMAT('SHOLE RECT',',',D23.15,',',D23.15,',',D23.15,',',D23.15
     1                        ,',',D23.15)
                          END IF
                          IF(NSSALENS(25,J).EQ.3.0D0) THEN
                              WRITE(OUTLYNE,110) NSSALENS(26,J),NSSALENS(27,J)
     1                        ,NSSALENS(28,J),NSSALENS(29,J),NSSALENS(30,J)
                              CALL SHOWIT(10)
 110                          FORMAT('SHOLE ELIP',',',D23.15,',',D23.15,',',D23.15,',',D23.15
     1                        ,',',D23.15)
                          END IF
                      END IF
                  END IF
C       OUTPUT SURFACE SPOS
                  WRITE(OUTLYNE,111) NSSALENS(34,J),NSSALENS(35,J),
     1            NSSALENS(36,J),INT(NSSALENS(37,J))
                  CALL SHOWIT(10)
 111              FORMAT('SPOS',',',D23.15,',',D23.15,',',D23.15,
     1            ',',I8)
C       OUTPUT SURFACE SROT
                  WRITE(OUTLYNE,112) NSSALENS(40,J),NSSALENS(41,J),
     1            NSSALENS(42,J)
                  CALL SHOWIT(10)
 112              FORMAT('SROT',',',D23.15,',',D23.15,',',D23.15)
C       OUTPUT SURFACE COATING 1
                  WRITE(OUTLYNE,1131) INT(NSSALENS(31,J))
                  CALL SHOWIT(10)
 1131             FORMAT('NSSCOAT1',',',I3)
C       OUTPUT SURFACE COATING 2
                  WRITE(OUTLYNE,1132) INT(NSSALENS(32,J))
                  CALL SHOWIT(10)
 1132             FORMAT('NSSCOAT2',',',I3)
C
                  IF(NSSSYSTEM(39).EQ.1.0D0) WRITE(OUTLYNE,1133)
                  IF(NSSSYSTEM(39).EQ.0.0D0) WRITE(OUTLYNE,1134)
                  CALL SHOWIT(10)
 1133             FORMAT('NSSPOL ON')
 1134             FORMAT('NSSPOL OFF')
                  IF(NSSALENS(8,J).EQ.1.0D0) WRITE(OUTLYNE,2000)
                  IF(NSSALENS(8,J).EQ.2.0D0) WRITE(OUTLYNE,2001)
                  IF(NSSALENS(8,J).EQ.3.0D0) WRITE(OUTLYNE,2002)
                  CALL SHOWIT(10)
                  IF(NSSALENS(9,J).EQ.1.0D0) WRITE(OUTLYNE,3000)
                  IF(NSSALENS(9,J).EQ.2.0D0) WRITE(OUTLYNE,3001)
                  IF(NSSALENS(9,J).EQ.3.0D0) WRITE(OUTLYNE,3002)
                  CALL SHOWIT(10)
 2000             FORMAT('NSSINTER T 1')
 2001             FORMAT('NSSINTER R 1')
 2002             FORMAT('NSSINTER A 1')
 3000             FORMAT('NSSINTER T 2')
 3001             FORMAT('NSSINTER R 2')
 3002             FORMAT('NSSINTER A 2')
C       THE NSSN INDEX COMMAND IF NEEDED.
                  WRITE(OUTLYNE,116) ' 1',NSSALENS(101,J),NSSALENS(111,J)
                  CALL SHOWIT(10)
                  WRITE(OUTLYNE,116) ' 2',NSSALENS(102,J),NSSALENS(112,J)
                  CALL SHOWIT(10)
                  WRITE(OUTLYNE,116) ' 3',NSSALENS(103,J),NSSALENS(113,J)
                  CALL SHOWIT(10)
                  WRITE(OUTLYNE,116) ' 4',NSSALENS(104,J),NSSALENS(114,J)
                  CALL SHOWIT(10)
                  WRITE(OUTLYNE,116) ' 5',NSSALENS(105,J),NSSALENS(115,J)
                  CALL SHOWIT(10)
                  WRITE(OUTLYNE,116) ' 6',NSSALENS(106,J),NSSALENS(116,J)
                  CALL SHOWIT(10)
                  WRITE(OUTLYNE,116) ' 7',NSSALENS(107,J),NSSALENS(117,J)
                  CALL SHOWIT(10)
                  WRITE(OUTLYNE,116) ' 8',NSSALENS(108,J),NSSALENS(118,J)
                  CALL SHOWIT(10)
                  WRITE(OUTLYNE,116) ' 9',NSSALENS(109,J),NSSALENS(119,J)
                  CALL SHOWIT(10)
                  WRITE(OUTLYNE,116) '10',NSSALENS(110,J),NSSALENS(120,J)
                  CALL SHOWIT(10)
 116              FORMAT('NSSN 1',',',A2,',',D23.15,',',D23.15)
                  WRITE(OUTLYNE,117) ' 1',NSSALENS(121,J),NSSALENS(131,J)
                  CALL SHOWIT(10)
                  WRITE(OUTLYNE,117) ' 2',NSSALENS(122,J),NSSALENS(132,J)
                  CALL SHOWIT(10)
                  WRITE(OUTLYNE,117) ' 3',NSSALENS(123,J),NSSALENS(133,J)
                  CALL SHOWIT(10)
                  WRITE(OUTLYNE,117) ' 4',NSSALENS(124,J),NSSALENS(134,J)
                  CALL SHOWIT(10)
                  WRITE(OUTLYNE,117) ' 5',NSSALENS(125,J),NSSALENS(135,J)
                  CALL SHOWIT(10)
                  WRITE(OUTLYNE,117) ' 6',NSSALENS(126,J),NSSALENS(136,J)
                  CALL SHOWIT(10)
                  WRITE(OUTLYNE,117) ' 7',NSSALENS(127,J),NSSALENS(137,J)
                  CALL SHOWIT(10)
                  WRITE(OUTLYNE,117) ' 8',NSSALENS(128,J),NSSALENS(138,J)
                  CALL SHOWIT(10)
                  WRITE(OUTLYNE,117) ' 9',NSSALENS(129,J),NSSALENS(139,J)
                  CALL SHOWIT(10)
                  WRITE(OUTLYNE,117) '10',NSSALENS(130,J),NSSALENS(140,J)
                  CALL SHOWIT(10)
 117              FORMAT('NSSN 2',',',A2,',',D23.15,',',D23.15)
C       OPTICAL MATERIALS FIRST SPACE
                  WRITE(OUTLYNE,114) NSSGLASS1(1,J)(1:8),NSSGLASS1(2,J)
                  CALL SHOWIT(10)
 114              FORMAT('MEDIA1 ',A8,1X,A13)
C       OPTICAL MATERIALS FIRST SPACE
                  WRITE(OUTLYNE,115) NSSGLASS2(1,J)(1:8),NSSGLASS2(2,J)
                  CALL SHOWIT(10)
 115              FORMAT('MEDIA2 ',A8,1X,A13)
C       BOUNDS
                  IF(NSSALENS(1,J).NE.6.0D0) THEN
C       NOT IF A TUBE SURFACE
                      WRITE(OUTLYNE,120) NSSBOUNDS(1,J),NSSBOUNDS(2,J)
                      CALL SHOWIT(10)
                      WRITE(OUTLYNE,121) NSSBOUNDS(3,J),NSSBOUNDS(4,J)
                      CALL SHOWIT(10)
                      WRITE(OUTLYNE,122) NSSBOUNDS(5,J),NSSBOUNDS(6,J)
                      CALL SHOWIT(10)
 120                  FORMAT('SBOUNDX ',D23.15,',',D23.15)
 121                  FORMAT('SBOUNDY ',D23.15,',',D23.15)
 122                  FORMAT('SBOUNDZ ',D23.15,',',D23.15)
                  END IF
C       NOW OUTPUT "LINKAGE" ASSIGNMENTS
C       LINK SPROFILE
                  IF(NSSALENS(44,J).NE.DBLE(J).AND.
     1            NSSALENS(44,J).NE.0.0D0) THEN
 123                  FORMAT('NSSLINK SPROFILE',D23.15)
                      WRITE(OUTLYNE,123) NSSALENS(44,J)
                      CALL SHOWIT(10)
                  END IF
C       LINK SMEDIA
                  IF(NSSALENS(45,J).NE.DBLE(J).AND.
     1            NSSALENS(45,J).NE.0.0D0) THEN
 124                  FORMAT('NSSLINK SMEDIA',D23.15)
                      WRITE(OUTLYNE,124) NSSALENS(45,J)
                      CALL SHOWIT(10)
                  END IF
C       LINK SCOATING
                  IF(NSSALENS(46,J).NE.DBLE(J).AND.
     1            NSSALENS(46,J).NE.0.0D0) THEN
 125                  FORMAT('NSSLINK SCOATING',D23.15)
                      WRITE(OUTLYNE,125) NSSALENS(46,J)
                      CALL SHOWIT(10)
                  END IF
C       LINK SINTERAC
                  IF(NSSALENS(47,J).NE.DBLE(J).AND.
     1            NSSALENS(47,J).NE.0.0D0) THEN
 126                  FORMAT('NSSLINK SINTERAC',D23.15)
                      WRITE(OUTLYNE,126) NSSALENS(47,J)
                      CALL SHOWIT(10)
                  END IF
              ELSE
C       NO OUTPUT FOR THIS SURFACE
              END IF
          END DO
          RETURN
      END
      SUBROUTINE NSSLIST
C       THIS MAKE A HUMAN READABLE LISTING OF THE CURRENT NSS DATABASE
          USE NSSMOD
C
          IMPLICIT NONE
          CHARACTER UNITS*8,STYPE*14
          INTEGER I,J,K,IW1,IW2
C
C       THIS ROUTINE OUTPUTS THE CURRENT NSS DATABASE IN A FORM READABLE BY THE
C       PROGRAM

          INCLUDE 'datmai.inc'
 99       FORMAT(1X)
! 999    FORMAT(2X)
          IF(NSSSYSTEM(21).EQ.1.0D0) UNITS='INCH(ES)'
          IF(NSSSYSTEM(21).EQ.2.0D0) UNITS='CM(S)'
          IF(NSSSYSTEM(21).EQ.3.0D0) UNITS='MM(S)'
          IF(NSSSYSTEM(21).EQ.4.0D0) UNITS='METER(S)'
          IF(S1.EQ.1) THEN
              WRITE(OUTLYNE,9)
 9            FORMAT('CURRENT NSS DATABASE SURFACE LISTING')
              CALL SHOWIT(0)
          END IF
          IF(S1.NE.1) THEN
C       HEADER
              WRITE(OUTLYNE,10)
 10           FORMAT('CURRENT NSS DATABASE LISTING')
              CALL SHOWIT(0)
              WRITE(OUTLYNE,99)
              CALL SHOWIT(0)
C       UNITS
              WRITE(OUTLYNE,11) UNITS
              CALL SHOWIT(0)
 11           FORMAT('LINEAR UNITS = ',A8)
              WRITE(OUTLYNE,99)
              CALL SHOWIT(0)
C       WAVELENGTHS
              DO I=1,10
                  WRITE(OUTLYNE,12) I,NSSSYSTEM(I)
                  CALL SHOWIT(0)
              END DO
 12           FORMAT('WAVELENGTH #',I2,' = 'G15.8,' MICROMETER')
              WRITE(OUTLYNE,99)
              CALL SHOWIT(0)
C       SPECTRAL WEIGHTS
              DO I=1,10
                  WRITE(OUTLYNE,13) I,NSSSYSTEM(I+10)
                  CALL SHOWIT(0)
              END DO
 13           FORMAT('SPECTRAL WT. FOR WAVELENGTH #',I2,' = ',G15.8)
              WRITE(OUTLYNE,99)
              CALL SHOWIT(0)
C       UNIVERSE
              WRITE(OUTLYNE,144) NSSSYSTEM(25)
              CALL SHOWIT(0)
 144          FORMAT('UNIVERSE RADIUS = ',G15.8)
              WRITE(OUTLYNE,99)
              CALL SHOWIT(0)
C       MIN RAY ENERGY
              WRITE(OUTLYNE,15) NSSSYSTEM(26)
              CALL SHOWIT(0)
 15           FORMAT('MIN. FRACTIONAL ENERGY FOR RAYS IN TRACE = ',G15.8)
              WRITE(OUTLYNE,99)
              CALL SHOWIT(0)
C       MAX HAY INTERSECTIONS PER SURFACE
              WRITE(OUTLYNE,16) DBLE(INT(NSSSYSTEM(27)))
              CALL SHOWIT(0)
 16           FORMAT('MAX. RAY/SURFACE INTERSECTIONS PER RAY = ',G15.8)
              WRITE(OUTLYNE,99)
              CALL SHOWIT(0)
C       RAY SPLITING
              IF(NSSSYSTEM(28).EQ.1.0D0) WRITE(OUTLYNE,17)
              IF(NSSSYSTEM(28).EQ.0.0D0) WRITE(OUTLYNE,18)
              CALL SHOWIT(0)
 17           FORMAT('RAY SPLITTING IS "ON"')
 18           FORMAT('RAY SPLITTING IS "OFF"')
              WRITE(OUTLYNE,99)
              CALL SHOWIT(0)
C       SOURCE GRID CLIPPING
              IF(NSSSYSTEM(33).EQ.0.0D0) THEN
                  WRITE(OUTLYNE,338)
              ELSE
                  WRITE(OUTLYNE,339)
              END IF
              CALL SHOWIT(0)
 338          FORMAT('SOURCE GRID CLIPPED CIRCULAR')
 339          FORMAT('SOURCE GRID FULL RECTANGULAR')
C       SOURCE POSITION
              IF(NSSSYSTEM(34).EQ.1.0D0) THEN
                  WRITE(OUTLYNE,19) NSSSYSTEM(29),NSSSYSTEM(30),NSSSYSTEM(31)
                  CALL SHOWIT(0)
 19               FORMAT('REAL SOURCE POSITION (X,Y,Z) = ',G15.8,',',G15.8,
     1            ',',G15.8)
              END IF
              IF(NSSSYSTEM(34).EQ.-1.0D0) THEN
                  WRITE(OUTLYNE,191) NSSSYSTEM(29),NSSSYSTEM(30),NSSSYSTEM(31)
                  CALL SHOWIT(0)
 191              FORMAT('VIRTUAL SOURCE POSITION (X,Y,Z) = ',G15.8,',',G15.8,
     1            ',',G15.8)
              END IF
              WRITE(OUTLYNE,99)
              CALL SHOWIT(0)
C       SOURCE GRID DIMENSION AND SPACING
              WRITE(OUTLYNE,20) INT(NSSSYSTEM(32))
              CALL SHOWIT(0)
 20           FORMAT('SOURCE GRID DIMENSION (NXN), N = ',I6)
              WRITE(OUTLYNE,201) NSSSYSTEM(37)
              CALL SHOWIT(0)
 201          FORMAT('SOURCE GRID SPACING = ',G15.8)
              WRITE(OUTLYNE,99)
              CALL SHOWIT(0)
C       REFERENCE GRID CLIPPING
              IF(NSSSYSTEM(22).EQ.0.0D0) THEN
                  WRITE(OUTLYNE,228)
              ELSE
                  WRITE(OUTLYNE,229)
              END IF
              CALL SHOWIT(0)
 228          FORMAT('REFERENCE GRID CLIPPED CIRCULAR')
 229          FORMAT('REFERENCE GRID FULL RECTANGULAR')
C       REFERENCE GRID POSITION
              WRITE(OUTLYNE,195) NSSSYSTEM(40),NSSSYSTEM(41),NSSSYSTEM(42)
              CALL SHOWIT(0)
 195          FORMAT('REFERENCE POSITION (X,Y,Z) = ',
     1        G15.8,',',G15.8,',',G15.8)
C       REFERENCE GRID DIMENSION
              WRITE(OUTLYNE,22) INT(NSSSYSTEM(36))
              CALL SHOWIT(0)
 22           FORMAT('REFERENCE GRID DIMENSION (NXN), N = ',I6)
              WRITE(OUTLYNE,221) NSSSYSTEM(38)
              CALL SHOWIT(0)
 221          FORMAT('REFERENCE GRID SPACING = ',G15.8)
              WRITE(OUTLYNE,99)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,222) NSSOGLASS(1),NSSOGLASS(2)
              CALL SHOWIT(0)
 222          FORMAT('OBJECT SPACE MATERIAL TYPE IS = ',A13,1X,A13)
              WRITE(OUTLYNE,99)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,224) DETSNUM
              CALL SHOWIT(0)
 224          FORMAT('DETECTOR SURFACE IS LOCATED ON SURFACE # ',I10)
              WRITE(OUTLYNE,99)
              CALL SHOWIT(0)
C
              IF(NSSSYSTEM(39).EQ.0.0D0) WRITE(OUTLYNE,1133)
              IF(NSSSYSTEM(39).EQ.1.0D0) WRITE(OUTLYNE,1134)
              CALL SHOWIT(0)
 1133         FORMAT('NSS POLARIZATION RAY TRACING IS "OFF"')
 1134         FORMAT('NSS POLARIZATION RAY TRACING IS "ON"')
              WRITE(OUTLYNE,99)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,99)
              CALL SHOWIT(0)
          END IF
          IF(WQ.NE.'NOSURF') THEN
              OUTLYNE='NSS SURFACE DATA FOLLOWS:'
              CALL SHOWIT(0)
              IF(S1.EQ.1) THEN
                  IW1=INT(W1)
                  IW2=INT(W1)
              END IF
              IF(S2.EQ.1) IW2=INT(W2)
              IF(IW2.LT.IW1) IW2=IW1
              IF(IW1.LT.1) IW1=0
              IF(IW2.LT.1) IW2=0
              IF(S1.EQ.0.AND.S2.EQ.0) THEN
                  IW1=1
                  IW2=MAXS
              END IF
              DO J=IW1,IW2
C       FIND OUT IF THE CURRENT SURFACE #
C       IS SUPPOSED TO BE IN THE NSS DATABASE
                  IF(NSSALENS(100,J).NE.0.0D0) THEN
C       OUTPUT SURFACE
                      WRITE(OUTLYNE,200)
                      CALL SHOWIT(0)
                      WRITE(OUTLYNE,99)
                      CALL SHOWIT(0)
                      WRITE(OUTLYNE,99)
                      CALL SHOWIT(0)
                      WRITE(OUTLYNE,200)
                      CALL SHOWIT(0)
                      WRITE(OUTLYNE,87) J
                      CALL SHOWIT(0)
                      WRITE(OUTLYNE,200)
                      CALL SHOWIT(0)
 87                   FORMAT('SURFACE NUMBER = ',I6)
C       OUTPUT SURFACE NAME
                      WRITE(OUTLYNE,118) NSSSNAM(J)(1:80)
                      CALL SHOWIT(0)
 118                  FORMAT('SURFACE NAME = ',A80)
C
                      IF(NSSSYSTEM(49).EQ.DBLE(J).AND.J.GT.0) THEN
                          WRITE(OUTLYNE,225)
 225                      FORMAT('THIS SURFACE IS THE DETECTOR SURFACE')
                      END IF
C       OUTPUT SURFACE DATA FOR THIS SURFACE
                      IF(NSSALENS(1,J).EQ.1.0D0)  STYPE='PLANO  '
                      IF(NSSALENS(1,J).EQ.2.0D0)  STYPE='SPHERIC'
                      IF(NSSALENS(1,J).EQ.3.0D0)  STYPE='ANAMORPH'
                      IF(NSSALENS(1,J).EQ.4.0D0)  STYPE='USER'
                      IF(NSSALENS(1,J).EQ.5.0D0)  STYPE='MEM'
                      IF(NSSALENS(1,J).EQ.6.0D0)  STYPE='TUBE'
                      IF(STYPE.EQ.'PLANO') THEN
                          WRITE(OUTLYNE,100) STYPE
                          CALL SHOWIT(0)
                          WRITE(OUTLYNE,1001) NSSALENS(3,J)
                          CALL SHOWIT(0)
                          WRITE(OUTLYNE,1002) NSSALENS(4,J)
                          CALL SHOWIT(0)
                          WRITE(OUTLYNE,1003) NSSALENS(5,J)
                          CALL SHOWIT(0)
                          WRITE(OUTLYNE,1004) NSSALENS(6,J)
                          CALL SHOWIT(0)
                          WRITE(OUTLYNE,1005) NSSALENS(7,J)
                          CALL SHOWIT(0)
                      END IF
                      IF(STYPE.EQ.'SPHERIC') THEN
                          WRITE(OUTLYNE,100) STYPE
                          CALL SHOWIT(0)
                          IF(NSSALENS(3,J).EQ.0.0D0)
     1                    WRITE(OUTLYNE,1001) NSSALENS(3,J)
                          IF(NSSALENS(3,J).NE.0.0D0)
     1                    WRITE(OUTLYNE,1001) 1.0D0/NSSALENS(3,J)
                          CALL SHOWIT(0)
                          WRITE(OUTLYNE,1002) NSSALENS(4,J)
                          CALL SHOWIT(0)
                          WRITE(OUTLYNE,1003) NSSALENS(5,J)
                          CALL SHOWIT(0)
                          WRITE(OUTLYNE,1004) NSSALENS(6,J)
                          CALL SHOWIT(0)
                          WRITE(OUTLYNE,1005) NSSALENS(7,J)
                          CALL SHOWIT(0)
                      END IF
                      IF(STYPE.EQ.'ANAMORPH') THEN
                          WRITE(OUTLYNE,100) STYPE
                          CALL SHOWIT(0)
                          IF(NSSALENS(3,J).EQ.0.0D0)
     1                    WRITE(OUTLYNE,1001) NSSALENS(3,J)
                          IF(NSSALENS(3,J).NE.0.0D0)
     1                    WRITE(OUTLYNE,1001) 1.0D0/NSSALENS(3,J)
                          CALL SHOWIT(0)
                          WRITE(OUTLYNE,1002) NSSALENS(4,J)
                          CALL SHOWIT(0)
                          WRITE(OUTLYNE,1003) NSSALENS(5,J)
                          CALL SHOWIT(0)
                          WRITE(OUTLYNE,1004) NSSALENS(6,J)
                          CALL SHOWIT(0)
                          WRITE(OUTLYNE,1005) NSSALENS(7,J)
                          CALL SHOWIT(0)
                      END IF
                      IF(STYPE.EQ.'USER') THEN
                          WRITE(OUTLYNE,100) STYPE
                          CALL SHOWIT(0)
                          WRITE(OUTLYNE,1201) NSSALENS(3,J)
                          CALL SHOWIT(0)
                          WRITE(OUTLYNE,1202) NSSALENS(4,J)
                          CALL SHOWIT(0)
                          WRITE(OUTLYNE,1203) NSSALENS(5,J)
                          CALL SHOWIT(0)
                          WRITE(OUTLYNE,1204) NSSALENS(6,J)
                          CALL SHOWIT(0)
                          WRITE(OUTLYNE,1205) NSSALENS(7,J)
                          CALL SHOWIT(0)
                      END IF
C       MEM
                      IF(STYPE.EQ.'MEM') THEN
                          WRITE(OUTLYNE,100) STYPE
                          CALL SHOWIT(0)
                          WRITE(OUTLYNE,1101) INT(NSSALENS(3,J))
                          CALL SHOWIT(0)
                          WRITE(OUTLYNE,1102) INT(NSSALENS(4,J))
                          CALL SHOWIT(0)
                          WRITE(OUTLYNE,1103) INT(NSSALENS(5,J))
                          CALL SHOWIT(0)
                          WRITE(OUTLYNE,1104) NSSALENS(6,J)
                          CALL SHOWIT(0)
                          WRITE(OUTLYNE,1105) NSSALENS(7,J)
                          CALL SHOWIT(0)
                      END IF
C       TUBE
                      IF(STYPE.EQ.'TUBE') THEN
                          WRITE(OUTLYNE,100) STYPE
                          CALL SHOWIT(0)
                          WRITE(OUTLYNE,1301) NSSALENS(10,J)
                          CALL SHOWIT(0)
                          WRITE(OUTLYNE,1302) NSSALENS(11,J)
                          CALL SHOWIT(0)
                      END IF
 100                  FORMAT('SURFACE TYPE = ',A14)
 1001                 FORMAT('NUMERIC VALUE #1      (RD OR CV) = ',G15.8)
 1002                 FORMAT('NUMERIC VALUE #2         (CONIC) = ',G15.8)
 1003                 FORMAT('NUMERIC VALUE #3  (4TH ASPHERIC) = ',G15.8)
 1004                 FORMAT('NUMERIC VALUE #4  (6TH ASPHERIC) = ',G15.8)
 1005                 FORMAT('NUMERIC VALUE #5  (8TH ASPHERIC) = ',G15.8)
 1101                 FORMAT('NUMERIC VALUE #1  # MEM ELEMENTS IN X = ',I10)
 1102                 FORMAT('NUMERIC VALUE #2  # MEM ELEMENTS IN Y = ',I10)
 1103                 FORMAT('NUMERIC VALUE #3       MEM DATA FILE# = ',I3)
 1104                 FORMAT('NUMERIC VALUE #4 X-DIM OF MEM ELEMENT = ',G15.8)
 1105                 FORMAT('NUMERIC VALUE #5 Y-DIM OF MEM ELEMENT = ',G15.8)
 1201                 FORMAT('NUMERIC VALUE #1 = ',G15.8)
 1202                 FORMAT('NUMERIC VALUE #2 = ',G15.8)
 1203                 FORMAT('NUMERIC VALUE #3 = ',G15.8)
 1204                 FORMAT('NUMERIC VALUE #4 = ',G15.8)
 1205                 FORMAT('NUMERIC VALUE #5 = ',G15.8)
 1301                 FORMAT('TUBE RADIUS = ',G15.8)
 1302                 FORMAT('TUBE LENGTH = ',G15.8)
C       PARAMETERS
                      DO K=1,200
                          IF(NSSALENS(K+200,J).NE.0.0D0) THEN
C       OUTPUT A SURFACE PARAMETER
                              WRITE(OUTLYNE,101) K,NSSALENS(K+200,J)
                              CALL SHOWIT(0)
 101                          FORMAT('SURFACE PARAM # ',I3,' = ',G15.8)
                          ELSE
C       PARAMETER IS ZERO
                          END IF
                      END DO
C       INTERACTION CODES
                      IF(NSSALENS(8,J).EQ.1.0D0) THEN
                          WRITE(OUTLYNE,1006)
                          CALL SHOWIT(0)
                      END IF
                      IF(NSSALENS(8,J).EQ.2.0D0) THEN
                          WRITE(OUTLYNE,1007)
                          CALL SHOWIT(0)
                      END IF
                      IF(NSSALENS(8,J).EQ.3.0D0) THEN
                          WRITE(OUTLYNE,1008)
                          CALL SHOWIT(0)
                      END IF
                      IF(NSSALENS(9,J).EQ.1.0D0) THEN
                          WRITE(OUTLYNE,1009)
                          CALL SHOWIT(0)
                      END IF
                      IF(NSSALENS(9,J).EQ.2.0D0) THEN
                          WRITE(OUTLYNE,2006)
                          CALL SHOWIT(0)
                      END IF
                      IF(NSSALENS(9,J).EQ.3.0D0) THEN
                          WRITE(OUTLYNE,2007)
                          CALL SHOWIT(0)
                      END IF
 1006                 FORMAT('SPACE#1 SURFACE INTERACTION CODE IS "T"')
 1007                 FORMAT('SPACE#1 SURFACE INTERACTION CODE IS "R"')
 1008                 FORMAT('SPACE#1 SURFACE INTERACTION CODE IS "A"')
 1009                 FORMAT('SPACE#2 SURFACE INTERACTION CODE IS "T"')
 2006                 FORMAT('SPACE#2 SURFACE INTERACTION CODE IS "R"')
 2007                 FORMAT('SPACE#2 SURFACE INTERACTION CODE IS "A"')

C       NOW GRATING
                      IF(NSSALENS(146,J).NE.0.0D0) THEN
                          WRITE(OUTLYNE,2020)
                          CALL SHOWIT(0)
                          WRITE(OUTLYNE,2021)NSSALENS(141,J)
                          CALL SHOWIT(0)
                          WRITE(OUTLYNE,2021)NSSALENS(142,J),UNITS
                          CALL SHOWIT(0)
                          WRITE(OUTLYNE,2021)NSSALENS(143,J)
                          CALL SHOWIT(0)
                          WRITE(OUTLYNE,2021)NSSALENS(144,J)
                          CALL SHOWIT(0)
                          WRITE(OUTLYNE,2021)NSSALENS(145,J)
                          CALL SHOWIT(0)
 2020                     FORMAT('LINEAR DIFFRACTION GRATING DATA')
 2021                     FORMAT('              GRATING ORDER = ',G15.8)
! 2022   FORMAT('            GRATING SPACING = ',G15.8,A8)
! 2023   FORMAT('GEN. PLANE NORMAL L DIR-COS = ',G15.8)
! 2024   FORMAT('GEN. PLANE NORMAL M DIR-COS = ',G15.8)
! 2025   FORMAT('GEN. PLANE NORMAL N DIR-COS = ',G15.8)
                      END IF
                      IF(NSSALENS(1,J).NE.6.0D0) THEN
C       NOT FOR TUBES
C NOW CLEAR APERTURES
                          IF(NSSALENS(19,J).NE.0.0D0) THEN
                              IF(NSSALENS(19,J).EQ.1.0D0) THEN
                                  WRITE(OUTLYNE,102)
                                  CALL SHOWIT(0)
                                  WRITE(OUTLYNE,1021) NSSALENS(20,J),UNITS
                                  CALL SHOWIT(0)
                                  WRITE(OUTLYNE,1022) NSSALENS(21,J),UNITS
                                  CALL SHOWIT(0)
                                  WRITE(OUTLYNE,1023) NSSALENS(22,J),UNITS
                                  CALL SHOWIT(0)
                                  WRITE(OUTLYNE,1024) NSSALENS(23,J),UNITS
                                  CALL SHOWIT(0)
                                  NSSALENS(24,J)=0.0D0
                                  WRITE(OUTLYNE,1025)NSSALENS(24,J)
                                  CALL SHOWIT(0)
 102                              FORMAT('CIRCULAR APERTURE')
 1021                             FORMAT('   X-DIMENSION = ',G15.8,1X,A8)
 1022                             FORMAT('   Y-DIMENSION = ',G15.8,1X,A8)
 1023                             FORMAT('    X-DECENTER = ',G15.8,1X,A8)
 1024                             FORMAT('    Y=DECENTER = ',G15.8,1X,A8)
 1025                             FORMAT('GAMMA ROTATION = ',G15.8,1X,'DEGREES')
                              END IF
                              IF(NSSALENS(19,J).EQ.2.0D0) THEN
                                  WRITE(OUTLYNE,103)
                                  CALL SHOWIT(0)
                                  WRITE(OUTLYNE,1021) NSSALENS(20,J),UNITS
                                  CALL SHOWIT(0)
                                  WRITE(OUTLYNE,1022) NSSALENS(21,J),UNITS
                                  CALL SHOWIT(0)
                                  WRITE(OUTLYNE,1023) NSSALENS(22,J),UNITS
                                  CALL SHOWIT(0)
                                  WRITE(OUTLYNE,1024) NSSALENS(23,J),UNITS
                                  CALL SHOWIT(0)
                                  WRITE(OUTLYNE,1025) NSSALENS(24,J)
                                  CALL SHOWIT(0)
 103                              FORMAT('RECTANGULAR APERTURE')
                              END IF
                              IF(NSSALENS(19,J).EQ.3.0D0) THEN
                                  WRITE(OUTLYNE,104)
                                  CALL SHOWIT(0)
                                  WRITE(OUTLYNE,1021) NSSALENS(20,J),UNITS
                                  CALL SHOWIT(0)
                                  WRITE(OUTLYNE,1022) NSSALENS(21,J),UNITS
                                  CALL SHOWIT(0)
                                  WRITE(OUTLYNE,1023) NSSALENS(22,J),UNITS
                                  CALL SHOWIT(0)
                                  WRITE(OUTLYNE,1024) NSSALENS(23,J),UNITS
                                  CALL SHOWIT(0)
                                  WRITE(OUTLYNE,1025) NSSALENS(24,J)
                                  CALL SHOWIT(0)
 104                              FORMAT('ELLIPTICAL APERTURE')
                              END IF
                          END IF
                      END IF
                      IF(NSSALENS(1,J).NE.6.0D0) THEN
C       NOT FOR TUBES
C NOW HOLES
                          IF(NSSALENS(25,J).NE.0.0D0) THEN
                              IF(NSSALENS(25,J).EQ.1.0D0) THEN
                                  WRITE(OUTLYNE,108)
                                  CALL SHOWIT(0)
                                  WRITE(OUTLYNE,1021) NSSALENS(26,J),UNITS
                                  CALL SHOWIT(0)
                                  WRITE(OUTLYNE,1021) NSSALENS(27,J),UNITS
                                  CALL SHOWIT(0)
                                  WRITE(OUTLYNE,1021) NSSALENS(28,J),UNITS
                                  CALL SHOWIT(0)
                                  WRITE(OUTLYNE,1021) NSSALENS(29,J),UNITS
                                  CALL SHOWIT(0)
                                  WRITE(OUTLYNE,1021) NSSALENS(30,J)
                                  CALL SHOWIT(0)
 108                              FORMAT('CIRCULAR HOLE')
                              END IF
                              IF(NSSALENS(25,J).EQ.2.0D0) THEN
                                  WRITE(OUTLYNE,109)
                                  CALL SHOWIT(0)
                                  WRITE(OUTLYNE,1021)NSSALENS(26,J),UNITS
                                  CALL SHOWIT(0)
                                  WRITE(OUTLYNE,1021)NSSALENS(27,J),UNITS
                                  CALL SHOWIT(0)
                                  WRITE(OUTLYNE,1021)NSSALENS(28,J),UNITS
                                  CALL SHOWIT(0)
                                  WRITE(OUTLYNE,1021)NSSALENS(29,J),UNITS
                                  CALL SHOWIT(0)
                                  WRITE(OUTLYNE,1021)NSSALENS(30,J)
                                  CALL SHOWIT(0)
 109                              FORMAT('RECTANGULAR HOLE')
                              END IF
                              IF(NSSALENS(25,J).EQ.3.0D0) THEN
                                  WRITE(OUTLYNE,110)
                                  CALL SHOWIT(0)
                                  WRITE(OUTLYNE,1021)NSSALENS(26,J),UNITS
                                  CALL SHOWIT(0)
                                  WRITE(OUTLYNE,1021)NSSALENS(27,J),UNITS
                                  CALL SHOWIT(0)
                                  WRITE(OUTLYNE,1021)NSSALENS(28,J),UNITS
                                  CALL SHOWIT(0)
                                  WRITE(OUTLYNE,1021)NSSALENS(29,J),UNITS
                                  CALL SHOWIT(0)
                                  WRITE(OUTLYNE,1021)NSSALENS(30,J)
                                  CALL SHOWIT(0)
 110                              FORMAT('ELLIPTICAL HOLE')
                              END IF
                          END IF
                      END IF
C       OUTPUT SURFACE SPOS
                      WRITE(OUTLYNE,111) NSSALENS(34,J),NSSALENS(35,J),
     1                NSSALENS(36,J)
                      CALL SHOWIT(0)
 111                  FORMAT('SURFACE POS.(X,Y,Z) = ',G15.8
     1                 ,',',G15.8,',',G15.8)
                      WRITE(OUTLYNE,1111) INT(NSSALENS(37,J))
                      CALL SHOWIT(0)
 1111                 FORMAT('SURFACE POS. RELATIVE TO.SURF.# ',I8)
C       OUTPUT SURFACE SROT
                      WRITE(OUTLYNE,112) NSSALENS(40,J),NSSALENS(41,J),
     1                NSSALENS(42,J)
                      CALL SHOWIT(0)
 112                  FORMAT('SURFACE ROT.(A,B,C) = ',G15.8
     1                ,',',G15.8,',',G15.8)
C       OUTPUT SURFACE COATING 1 FILE NUMBER
                      WRITE(OUTLYNE,1131) INT(NSSALENS(31,J))
                      CALL SHOWIT(0)
 1131                 FORMAT('NSS SURFACE COATING 1 FILE NUMBER = ',I3)
C       OUTPUT SURFACE COATING  2 ID NUMBER
                      WRITE(OUTLYNE,1132) INT(NSSALENS(32,J))
                      CALL SHOWIT(0)
 1132                 FORMAT('NSS SURFACE COATING 2 FILE NUMBER = ',I3)
C       OPTICAL MATERIALS FIRST SPACE
                      WRITE(OUTLYNE,114) NSSGLASS1(1,J)(1:8),NSSGLASS1(2,J)
                      CALL SHOWIT(0)
 114                  FORMAT('SURFACE MEDIA1 ',A8,1X,A13)
C       OPTICAL MATERIALS SECOND SPACE
                      WRITE(OUTLYNE,115) NSSGLASS2(1,J)(1:8),NSSGLASS2(2,J)
                      CALL SHOWIT(0)
 115                  FORMAT('SURFACE MEDIA2 ',A8,1X,A13)
 301                  FORMAT('  N (MEDIA1)   ',3X,'  K (MEDIA1)',6X,
     1                '  N (MEDIA2)   ',3X,'  K (MEDIA2)')
 302                  FORMAT(G15.8,3X,G15.8,3X,G15.8,3X,G15.8)
                      WRITE(OUTLYNE,301)
                      CALL SHOWIT(0)
                      WRITE(OUTLYNE,302) NSSALENS(101,J),NSSALENS(111,J),
     1                NSSALENS(121,J),NSSALENS(131,J)
                      CALL SHOWIT(0)
                      WRITE(OUTLYNE,302) NSSALENS(102,J),NSSALENS(112,J),
     1                NSSALENS(122,J),NSSALENS(132,J)
                      CALL SHOWIT(0)
                      WRITE(OUTLYNE,302) NSSALENS(103,J),NSSALENS(113,J),
     1                NSSALENS(123,J),NSSALENS(133,J)
                      CALL SHOWIT(0)
                      WRITE(OUTLYNE,302) NSSALENS(104,J),NSSALENS(114,J),
     1                NSSALENS(124,J),NSSALENS(134,J)
                      CALL SHOWIT(0)
                      WRITE(OUTLYNE,302) NSSALENS(105,J),NSSALENS(115,J),
     1                NSSALENS(125,J),NSSALENS(135,J)
                      CALL SHOWIT(0)
                      WRITE(OUTLYNE,302) NSSALENS(106,J),NSSALENS(116,J),
     1                NSSALENS(126,J),NSSALENS(136,J)
                      CALL SHOWIT(0)
                      WRITE(OUTLYNE,302) NSSALENS(107,J),NSSALENS(117,J),
     1                NSSALENS(127,J),NSSALENS(137,J)
                      CALL SHOWIT(0)
                      WRITE(OUTLYNE,302) NSSALENS(108,J),NSSALENS(118,J),
     1                NSSALENS(128,J),NSSALENS(138,J)
                      CALL SHOWIT(0)
                      WRITE(OUTLYNE,302) NSSALENS(109,J),NSSALENS(119,J),
     1                NSSALENS(129,J),NSSALENS(139,J)
                      CALL SHOWIT(0)
                      WRITE(OUTLYNE,302) NSSALENS(110,J),NSSALENS(120,J),
     1                NSSALENS(130,J),NSSALENS(140,J)
                      CALL SHOWIT(0)
C       BOUNDS
                      IF(NSSALENS(1,J).NE.6.0D0) THEN
C       NOT FOR TUBES
                          WRITE(OUTLYNE,120) NSSBOUNDS(1,J)
                          CALL SHOWIT(0)
                          WRITE(OUTLYNE,121) NSSBOUNDS(2,J)
                          CALL SHOWIT(0)
                          WRITE(OUTLYNE,122) NSSBOUNDS(3,J)
                          CALL SHOWIT(0)
                          WRITE(OUTLYNE,123) NSSBOUNDS(4,J)
                          CALL SHOWIT(0)
                          WRITE(OUTLYNE,124) NSSBOUNDS(5,J)
                          CALL SHOWIT(0)
                          WRITE(OUTLYNE,125) NSSBOUNDS(6,J)
                          CALL SHOWIT(0)
 120                      FORMAT('SURFACE -X BOUND =',G15.8)
 121                      FORMAT('SURFACE +X BOUND =',G15.8)
 122                      FORMAT('SURFACE -Y BOUND =',G15.8)
 123                      FORMAT('SURFACE +Y BOUND =',G15.8)
 124                      FORMAT('SURFACE -Z BOUND =',G15.8)
 125                      FORMAT('SURFACE +Z BOUND =',G15.8)
                      END IF
C       NOW OUTPUT "LINKAGE" ASSIGNMENTS
C       LINK SPROFILE
                      IF(NSSALENS(44,J).NE.DBLE(J).AND.
     1                NSSALENS(44,J).NE.0.0D0) THEN
 1123                     FORMAT('NSSLINK SPROFILE',G15.8)
                          WRITE(OUTLYNE,1123) NSSALENS(44,J)
                          CALL SHOWIT(0)
                      END IF
C       LINK SMEDIA
                      IF(NSSALENS(45,J).NE.DBLE(J).AND.
     1                NSSALENS(45,J).NE.0.0D0) THEN
 1124                     FORMAT('NSSLINK SMEDIA',G15.8)
                          WRITE(OUTLYNE,1124) NSSALENS(45,J)
                          CALL SHOWIT(0)
                      END IF
C       LINK SPROFILE
                      IF(NSSALENS(46,J).NE.DBLE(J).AND.
     1                NSSALENS(46,J).NE.0.0D0) THEN
 1125                     FORMAT('NSSLINK SCOATING',G15.8)
                          WRITE(OUTLYNE,1125) NSSALENS(46,J)
                          CALL SHOWIT(0)
                      END IF
C       LINK SPROFILE
                      IF(NSSALENS(47,J).NE.DBLE(J).AND.
     1                NSSALENS(47,J).NE.0.0D0) THEN
 1126                     FORMAT('NSSLINK SINTERAC',G15.8)
                          WRITE(OUTLYNE,1126) NSSALENS(47,J)
                          CALL SHOWIT(0)
                      END IF
                  ELSE
C       NO OUTPUT FOR THIS SURFACE
                  END IF
              END DO
          END IF
          WRITE(OUTLYNE,200)
          CALL SHOWIT(0)
 200      FORMAT('**********************************************')
          RETURN
      END
