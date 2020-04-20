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

      SUBROUTINE MODEL(I)
          IMPLICIT NONE
C       CONVERTS CATALOG GLASS TO MODEL GLASS
C       AUTOMATICALLY
          INTEGER I,J
          REAL*8 INDEX,ABBE,ND,INDEX0,INDEX1,INDEX2,DPART0
     1    ,DPART1,DPART2,S,DPART,ERROR,LASTERROR,LASTDPART
          CHARACTER AJ*3,AW1*23,AW2*23,AW3*23
          REAL*8 OLDGLSWV(1:10),OGPREG(1:5),DELTA,DPARTN
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
          I=INT(W1)
C        CALCULATE Nd AND Vd
C       SAVE OLD WAVELENGTH VALUES
          IF(GLANAM(I,1).EQ.'SCHOTT'.OR.GLANAM(I,1).EQ.'OHARA'
     1    .OR.GLANAM(I,1).EQ.'HOYA'.OR.GLANAM(I,1).EQ.'CORNIN'
     1    .OR.GLANAM(I,1).EQ.'SCH2000'
     2    .OR.GLANAM(I,1).EQ.'CHANCE'.OR.GLANAM(I,1).EQ.'RADHARD'
     3    .OR.GLANAM(I,1).EQ.'RUSSIAN'.OR.GLANAM(I,1).EQ.'HIKARI') THEN
              OLDGLSWV(1:10)=GLSWV(1:10)
              OGPREG(1:5)=GPREG(1:5)
C       RESET WAVELENGTHS TO THE DEFAULT.
              GLSWV(1)=0.58756D0
              GLSWV(2)=0.48613D0
              GLSWV(3)=0.65627D0
              GLSWV(4)=0.43584D0
              GLSWV(5)=0.70652D0
C       COMPUTE THE 5 REFRACTIVE INDEX VALUES AND STORE THEM
              SAVE_KDP(29)=SAVEINPT(29)
              INPUT=GLANAM(I,1)//',:'//GLANAM(I,2)
              CALL PROCES
              REST_KDP(29)=RESTINPT(29)
              ND=GPREG(1)
              ABBE=(GPREG(1)-1.0D0)/(GPREG(2)-GPREG(3))
              DPART0=0.0D0
              CALL FICT(INDEX,ND,ABBE,DPART0)
              DPART=0.0D0
              LASTDPART=0.0D0
              DELTA=.001D0
              ERROR=1.0D20
              WRITE(OUTLYNE,*) GPREG(1),GPREG(2),GPREG(3)
              CALL SHOWIT(1)
              DO J=1,10
                  LASTERROR=ERROR
                  LASTDPART=DPART
                  DPART0=DPART-DELTA
                  CALL FICT(INDEX,ND,ABBE,DPART0)
                  INDEX0=INDEX
C
                  DPART1=DPART
                  CALL FICT(INDEX,ND,ABBE,DPART1)
                  INDEX1=INDEX
C
                  DPART2=DPART+DELTA
                  CALL FICT(INDEX,ND,ABBE,DPART2)
                  INDEX2=INDEX
                  S=(DPART0-DPART2)/(INDEX0-INDEX2)
                  DPARTN=(S*(ND-INDEX2))-DPART2
                  CALL FICT(INDEX,ND,ABBE,DPARTN)

                  ERROR=ND-INDEX
                  DELTA=DABS(DPARTN-DPART)
                  DPART=DPARTN
                  IF(DABS(ERROR).GT.LASTERROR) THEN
                      DPART=LASTDPART
                      GO TO 10
                  END IF
              END DO
 10           CONTINUE
              SAVE_KDP(29)=SAVEINPT(29)
              INPUT='U L'
              CALL PROCES
              REST_KDP(29)=RESTINPT(29)
              SAVE_KDP(29)=SAVEINPT(29)
              CALL ITOAA(I,AJ)
              INPUT='CHG,'//AJ
              CALL PROCES
              REST_KDP(29)=RESTINPT(29)
              SAVE_KDP(29)=SAVEINPT(29)
              CALL DTOA23(ND,AW1)
              CALL DTOA23(ABBE,AW2)
              CALL DTOA23(DPART,AW3)
              INPUT='MODEL '//GLANAM(I,2)(1:8)
     1        //','//AW1//','//AW2//','//AW3
              SST=0
              CALL PROCES
              REST_KDP(29)=RESTINPT(29)
              SAVE_KDP(29)=SAVEINPT(29)
              INPUT='EOS'
              CALL PROCES
              REST_KDP(29)=RESTINPT(29)
              GLSWV(1:10)=OLDGLSWV(1:10)
              GPREG(1:5)=OGPREG(1:5)
          END IF
          RETURN
      END
