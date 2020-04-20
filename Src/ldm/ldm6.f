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

C       SIXTH FILE FOR LENS DATABASE MANAGER FILES

C SUB SLABEL.FOR
      SUBROUTINE SLABEL
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE SLABEL WHICH IMPLEMENTS THE LBL/LABEL
C       COMMAND
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
C
C               CHECK FOR PRESENCE OF QUALIFIER OR NUMERIC WORDS
C               PRINT ERROR AND RETURN IF DISCOVERED.
C
C
C               WE ARE AT LENS INPUT OR LENS UPDATE LEVEL
C
          IF(WC.EQ.'LBL'.OR.WC.EQ.'LABEL') THEN
              LBL(SURF)(1:80)=WS(1:80)
              IF(WS.NE.CNULL) ALENS(44,SURF)=1.0D0
              IF(WS.EQ.CNULL) ALENS(44,SURF)=0.0D0
          ELSE
C       NOT LABEL OR LBL
          END IF
          RETURN
      END
C SUB SFNO.FOR
      SUBROUTINE SFNO
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE SFNO WHICH IMPLEMENTS THE FNO(X OR Y) COMMAND
C       AT THE LENS OF UPDATE LENS LEVEL OR THE FNO(X OR Y) COMMAND AT
C       THE CMD LEVEL
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
          IF(F1.EQ.1) THEN
C
C               CHECK FOR ADDITIONAL INPUT AND
C               PRINT ERROR AND RETURN IF DISCOVERED.
C
              IF(SQ.EQ.1.OR.SN.EQ.1.OR.SST.EQ.1) THEN
                  OUTLYNE='AT THE CMD LEVEL, "FNOY" AND "FNOX"'
                  CALL SHOWIT(1)
                  OUTLYNE='TAKE NO EXPLICIT INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C
              IF(STI.EQ.1.OR.STI.EQ.0) THEN
                  IF(WC.EQ.'FNOY') THEN
                      IF(SYSTEM1(67).NE.1.0D0.AND.SYSTEM1(67).NE.3.0D0) THEN
                          OUTLYNE='NOTE:'
                          CALL SHOWIT(1)
                          OUTLYNE='"'//WC(1:4)//'" HAS NOT BEEN EXPLICITLY SET'
                          CALL SHOWIT(1)
                          SYSTEM1(68)=1.0D0/((2.0D0*SYSTEM1(12))/ALENS(3,0))
                          SYSTEM1(83)=0.0D0
                          SYSTEM1(84)=0.0D0
                      ELSE
                      END IF
                      WRITE(OUTLYNE,2000) SYSTEM1(68)
                      CALL SHOWIT(0)
                      RETURN
                  END IF
                  IF(WC.EQ.'FNOX') THEN
                      IF(SYSTEM1(67).NE.2.0D0.AND.SYSTEM1(67).NE.3.0D0) THEN
                          OUTLYNE='NOTE:'
                          CALL SHOWIT(1)
                          OUTLYNE='"'//WC(1:4)//'" HAS NOT BEEN EXPLICITLY SET'
                          CALL SHOWIT(1)
                          SYSTEM1(69)=1.0D0/((2.0D0*SYSTEM1(13))/ALENS(3,0))
                          SYSTEM1(83)=0.0D0
                          SYSTEM1(84)=0.0D0
                      END IF
                      WRITE(OUTLYNE,3000) SYSTEM1(69)
                      CALL SHOWIT(0)
                      RETURN
                  END IF
              END IF
          END IF
C
          IF(F5.EQ.1.OR.F6.EQ.1) THEN
              IF(STI.EQ.1) THEN
                  OUTLYNE=
     1            'QUERRY OBJECT F-NUMBER VALUES FROM THE CMD LEVEL WITH THE'
                  CALL SHOWIT(1)
                  OUTLYNE=
     1            '"FNOY" OR "FNOX" COMMANDS'
                  CALL SHOWIT(1)
                  RETURN
              END IF
C
C               CHECK FOR PRESENCE OF QUALIFIER OR STRING INPUT
C               PRINT ERROR AND RETURN IF DISCOVERED.
C
              IF(SST.EQ.1) THEN
C
                  IF(WC.EQ.'FNOY') THEN
                      OUTLYNE='"FNOY" TAKES NO STRING INPUT'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  IF(WC.EQ.'FNOX') THEN
                      OUTLYNE='"FNOX" TAKES NO STRING INPUT'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
              IF(S2.EQ.1.OR.S3.EQ.1
     1        .OR.S4.EQ.1.OR.S5.EQ.1) THEN
C
                  IF(WC.EQ.'FNOY') THEN
                      OUTLYNE='"FNOY" TAKES NO NUMERIC WORD #2 THROUGH #5 INPUT'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  IF(WC.EQ.'FNOX') THEN
                      OUTLYNE='"FNOX" TAKES NO NUMERIC WORD #2 THROUGH #5 INPUT'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
              IF(SQ.EQ.1.AND.F5.EQ.1) THEN
C
                  IF(WC.EQ.'FNOY') THEN
                      OUTLYNE='"FNOY" TAKES NO QUALIFIER WORD IN LENS INPUT MODE'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  IF(WC.EQ.'FNOX') THEN
                      OUTLYNE='"FNOX" TAKES NO QUALIFIER WORD IN LENS INPUT MODE'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
              IF(F6.EQ.1) THEN
                  IF(WQ.NE.'        '.AND.WQ.NE.'DELT'.AND.WQ.NE.'CENT') THEN
C
                      IF(WC.EQ.'FNOY') THEN
                          OUTLYNE='INVALID QUALIFIER WORD USED WITH "FNOY"'
                          CALL SHOWIT(1)
                          OUTLYNE='RE-ENTER COMMAND'
                          CALL SHOWIT(1)
                          CALL MACFAL
                          RETURN
                      END IF
                      IF(WC.EQ.'FNOX') THEN
                          OUTLYNE='INVALID QUALIFIER WORD USED WITH "FNOX"'
                          CALL SHOWIT(1)
                          OUTLYNE='RE-ENTER COMMAND'
                          CALL SHOWIT(1)
                          CALL MACFAL
                          RETURN
                      END IF
                  END IF
              END IF
              IF(DF1.EQ.1) THEN
C
                  IF(WC.EQ.'FNOY') THEN
                      OUTLYNE='"FNOY" REQUIRES EXPLICIT NUMERIC WORD #1 INPUT'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  IF(WC.EQ.'FNOX') THEN
                      OUTLYNE='"FNOX" REQUIRES EXPLICIT NUMERIC WORD #1 INPUT'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
C
C               WE ARE AT LENS INPUT OR LENS UPDATE LEVEL
C
              IF(S1.EQ.0) THEN
                  IF(WC.EQ.'FNOY') THEN
                      WRITE(OUTLYNE,2000) SYSTEM1(68)
                      CALL SHOWIT(0)
                  ELSE
                  END IF
                  IF(WC.EQ.'FNOX') THEN
                      WRITE(OUTLYNE,3000) SYSTEM1(69)
                      CALL SHOWIT(0)
                      IF(SYSTEM1(49).EQ.0.0D0) SYSTEM1(49)=1.0D0
                      IF(SYSTEM1(49).EQ.2.0D0) SYSTEM1(49)=3.0D0
                  ELSE
                  END IF
                  RETURN
              ELSE
                  IF(W1.EQ.0.0D0) THEN
                      IF(WC.EQ.'FNOY') THEN
                          OUTLYNE='"FNOY" MAY NOT BE SET TO ZERO'
                          CALL SHOWIT(1)
                          OUTLYNE='RE-ENTER COMMAND'
                          CALL SHOWIT(1)
                          CALL MACFAL
                          RETURN
                      END IF
                      IF(WC.EQ.'FNOX') THEN
                          OUTLYNE='"FNOX" MAY NOT BE SET TO ZERO'
                          CALL SHOWIT(1)
                          OUTLYNE='RE-ENTER COMMAND'
                          CALL SHOWIT(1)
                          CALL MACFAL
                          RETURN
                      END IF
                      RETURN
                  END IF
                  IF(SQ.EQ.0) THEN
                      IF(WC.EQ.'FNOY') THEN
                          IF(SYSTEM1(67).NE.3.0D0.AND.SYSTEM1(67).NE.1.0D0) THEN
                              IF(SYSTEM1(67).EQ.0.0D0) SYSTEM1(67)=1.0D0
                              IF(SYSTEM1(67).EQ.2.0D0) SYSTEM1(67)=3.0D0
                              SYSTEM1(83)=0.0D0
                              SYSTEM1(84)=0.0D0
                          END IF
                      END IF
                      IF(WC.EQ.'FNOX') THEN
                          IF(SYSTEM1(67).NE.3.0D0.AND.SYSTEM1(67).NE.2.0D0) THEN
                              IF(SYSTEM1(67).EQ.0.0D0) SYSTEM1(67)=2.0D0
                              IF(SYSTEM1(67).EQ.1.0D0) SYSTEM1(67)=3.0D0
                              SYSTEM1(83)=0.0D0
                              SYSTEM1(84)=0.0D0
                          END IF
                      END IF
                      SYSTEM1(64)=0.0D0
                      IF(WC.EQ.'FNOY') SYSTEM1(68)=DABS(W1)
                      IF(WC.EQ.'FNOX') SYSTEM1(69)=DABS(W1)
                  END IF
                  IF(WQ.EQ.'DELT') THEN
                      SYSTEM1(64)=0.0D0
                      IF(WC.EQ.'FNOY') SYSTEM1(68)=SYSTEM1(68)+(W1)
                      IF(WC.EQ.'FNOX') SYSTEM1(69)=SYSTEM1(69)+(W1)
                  END IF
                  IF(WQ.EQ.'CENT') THEN
                      SYSTEM1(64)=0.0D0
                      IF(WC.EQ.'FNOY') SYSTEM1(68)=SYSTEM1(68)+(W1*0.0D0*SYSTEM1(68))
                      IF(WC.EQ.'FNOX') SYSTEM1(69)=SYSTEM1(69)+(W1*0.0D0*SYSTEM1(69))
                  END IF
                  RETURN
              END IF
          END IF
 2000     FORMAT('FNOY=',1X,D23.15)
 3000     FORMAT('FNOX=',1X,D23.15)
          RETURN
      END
C SUB SFNB.FOR
      SUBROUTINE SFNB
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE SFNB WHICH IMPLEMENTS THE FNBY AND FNBX
C       COMMANDS AT THE CMD LEVEL. (ALSO FNBY HLD AND FNBX HLD)
C
          REAL*8 FN,ABSSYS
C
          INTEGER ITYPEP
C
          COMMON/PTYPER/ITYPEP
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
C               CHECK FOR ADDITIONAL INPUT AND
C               PRINT ERROR AND RETURN IF DISCOVERED.
C
          IF(STI.EQ.1) THEN
              OUTLYNE=
     1        'QUERRY F-NUMBER VALUES FROM THE CMD LEVEL WITH THE'
              CALL SHOWIT(1)
              OUTLYNE=
     1        '"SHO" OR "GET" AND "WRITE" COMMANDS'
              CALL SHOWIT(1)
              RETURN
          END IF
          IF(SST.EQ.1.OR.S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
C
              IF(WC.EQ.'FNBY') THEN
                  OUTLYNE=
     1            '"FNBY" TAKES NO STRING OR NUMERIC WORD #2 THROUGH #5 INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(WC.EQ.'FNBX') THEN
                  OUTLYNE=
     1            '"FNBX" TAKES NO STRING OR NUMERIC WORD #2 THROUGH #5 INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
          IF(DF1.EQ.1) THEN
C
              IF(WC.EQ.'FNBY') THEN
                  OUTLYNE=
     1            '"FNBY" REQUIRES EXPLICIT NUMERIC WORD #1 INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(WC.EQ.'FNBX') THEN
                  OUTLYNE=
     1            '"FNBX" REQUIRES EXPLICIT NUMERIC WORD #1 INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(WC.EQ.'FNBX'.AND.SYSTEM1(64).NE.0.0D0) THEN
                  OUTLYNE=
     1            '"FNBX" CAN NOT BE USED WHEN "SAX" IS SPECIFIED BY "NAOX"'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(WC.EQ.'FNBY'.AND.SYSTEM1(64).NE.0.0D0) THEN
                  OUTLYNE=
     1            '"FNBY" CAN NOT BE USED WHEN "SAY" IS SPECIFIED BY "NAOY"'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(WC.EQ.'FNBX'.AND.SYSTEM1(67).NE.0.0D0) THEN
                  OUTLYNE=
     1            '"FNBX" CAN NOT BE USED WHEN "SAX" IS SPECIFIED BY "FNOX"'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(WC.EQ.'FNBY'.AND.SYSTEM1(67).NE.0.0D0) THEN
                  OUTLYNE=
     1            '"FNBY" CAN NOT BE USED WHEN "SAY" IS SPECIFIED BY "FNOY"'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C
          END IF
          IF(SYSTEM1(83).NE.0.0D0.OR.SYSTEM1(84).NE.0.0D0) THEN
              OUTLYNE='SAY OR SAX IS CURRENTLY FLOATING'
              CALL SHOWIT(1)
              OUTLYNE='"FNBY/FNBX" ADJUSTMENT NOT ALLOWED'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SQ.EQ.0.AND.DF1.EQ.1.AND.DF2.EQ.1.AND.DF3.EQ.1.AND.
     1    DF4.EQ.1.AND.DF5.EQ.1) THEN
C       FNBY OR FNBX TYPED IN WITH NO INPUT,PRINTS OUT CURRENT VALUES
              IF(WC.EQ.'FNBY'.AND.SYSTEM1(44).NE.1.0D0.AND.
     1        SYSTEM1(44).NE.-1.0D0) THEN
                  IF(DABS(PXTRAY(2,(INT(SYSTEM1(20))))).LE.1.0D-15) THEN
                      WRITE(OUTLYNE,100)
                      CALL SHOWIT(0)
                      RETURN
                  ELSE
                      FN=-(1.0D0/(2.0D0*PXTRAY(2,(INT(SYSTEM1(20))))))
                  END IF
                  WRITE(OUTLYNE,200)FN
                  CALL SHOWIT(0)
                  RETURN
              END IF
              ABSSYS=DABS(SYSTEM1(44))
              IF(WC.EQ.'FNBY'.AND.ABSSYS.EQ.1.0D0) THEN
                  IF(DABS(PXTRAY(2,(INT(SYSTEM1(20))))).LE.1.0D-15) THEN
                      WRITE(OUTLYNE,100)
                      CALL SHOWIT(0)
                      RETURN
                  ELSE
                      FN=-(1.0D0/(2.0D0*PXTRAY(2,(INT(SYSTEM1(20))))))
                  END IF
                  IF(SYSTEM1(44).GT.0.0D0) THEN
                      WRITE(OUTLYNE,200) FN
                      CALL SHOWIT(0)
                  END IF
                  IF(SYSTEM1(44).LT.0.0D0) THEN
                      WRITE(OUTLYNE,300) SYSTEM1(46)
                      CALL SHOWIT(0)
                  END IF
                  RETURN
              END IF
              IF(WC.EQ.'FNBX'.AND.SYSTEM1(45).NE.1.0D0.AND.
     1        SYSTEM1(45).NE.-1.0D0) THEN
                  IF(DABS(PXTRAX(2,(INT(SYSTEM1(20))))).LE.1.0D-15) THEN
                      WRITE(OUTLYNE,101)
                      CALL SHOWIT(0)
                      RETURN
                  ELSE
                      FN=-(1.0D0/(2.0D0*PXTRAX(2,(INT(SYSTEM1(20))))))
                  END IF
                  WRITE(OUTLYNE,201) FN
                  CALL SHOWIT(0)
                  RETURN
              END IF
              ABSSYS=DABS(SYSTEM1(45))
              IF(WC.EQ.'FNBX'.AND.ABSSYS.EQ.1.0D0) THEN
                  IF(DABS(PXTRAX(2,(INT(SYSTEM1(20))))).LE.1.0D-15) THEN
                      WRITE(OUTLYNE,101)
                      CALL SHOWIT(0)
                      RETURN
                  ELSE
                      FN=-(1.0D0/(2.0D0*PXTRAX(2,(INT(SYSTEM1(20))))))
                  END IF
                  IF(SYSTEM1(45).GT.0.0D0) THEN
                      WRITE(OUTLYNE,201) FN
                      CALL SHOWIT(0)
                  END IF
                  IF(SYSTEM1(45).LT.0.0D0) THEN
                      WRITE(OUTLYNE,301) SYSTEM1(47)
                      CALL SHOWIT(0)
                  END IF
                  RETURN
              ELSE
              END IF
C
          ELSE
C       NUMERIC INPUT AND QUALIFIER INPUT NOT DEFAULT
              IF(WQ.NE.'DEL'.AND.WQ.NE.'HLD'.AND.SQ.NE.0) THEN
                  OUTLYNE='INVALID QUALIFIER INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(WQ.NE.'DEL') THEN
                  IF(WC.EQ.'FNBY') THEN
                      IF(W1.EQ.0.0D0) THEN
                          OUTLYNE=
     1                    'F/0.0 IS MEANINGLESS, ADJUSTMENT NOT PERFORMED'
                          CALL SHOWIT(1)
                          CALL MACFAL
                          RETURN
                      END IF
C       CHECK LENS MODE, MUST BE FOCAL OR UFOCAL
                      IF(SYSTEM1(30).EQ.3.0.OR.SYSTEM1(30).EQ.4.0) THEN
                          IF(WC.EQ.'FNBY') THEN
                              OUTLYNE='"FNBY" REQUIRES THE FOCAL OR UFOCAL MODES'
                              CALL SHOWIT(1)
                              IF(SYSTEM1(30).EQ.3.0) THEN
                                  OUTLYNE='CURRENT MODE IS AFOCAL'
                                  CALL SHOWIT(1)
                              END IF
                              IF(SYSTEM1(30).EQ.4.0) THEN
                                  OUTLYNE='CURRENT MODE IS UAFOCAL'
                                  CALL SHOWIT(1)
                              END IF
                              OUTLYNE='"FNBY" ADJUSTMENT NOT PERFORMED'
                              CALL SHOWIT(1)
                              CALL MACFAL
                              RETURN
                          END IF
                          IF(WC.EQ.'FNBX') THEN
                              OUTLYNE='"FNBX" REQUIRES THE FOCAL OR UFOCAL MODES'
                              CALL SHOWIT(1)
                              IF(SYSTEM1(30).EQ.3.0) THEN
                                  OUTLYNE='CURRENT MODE IS AFOCAL'
                                  CALL SHOWIT(1)
                              END IF
                              IF(SYSTEM1(30).EQ.4.0) THEN
                                  OUTLYNE='CURRENT MODE IS UAFOCAL'
                                  CALL SHOWIT(1)
                              END IF
                              OUTLYNE='"FNBX" ADJUSTMENT NOT PERFORMED'
                              CALL SHOWIT(1)
                              CALL MACFAL
                              RETURN
                          END IF
                      ELSE
                      END IF
C
                      IF(WQ.NE.'HLD') SYSTEM1(44)=1.0D0
                      IF(WQ.EQ.'HLD') SYSTEM1(44)=-1.0D0
                      SYSTEM1(46)=W1
C       CALL THE SUBROUTINE FYADJ TO PERFORM THE F NUMBER ADJUSTMENT
C       DO A PARAXIAL TRACE TO GET STARTING VALUES
                      ITYPEP=1
C       TELECENTRIC STUFF, 11/12/2000
                      IF(SYSTEM1(63).EQ.1.0D0) THEN
                          IF(SYSTEM1(64).EQ.0.0D0.AND.SYSTEM1(67).EQ.0.0D0) THEN
                              OUTLYNE='WHEN "TEL ON" IS SET, NAO OR FNO MUST BE USED'
                              CALL SHOWIT(1)
                              OUTLYNE='TO SPECIFY THE MARGINAL PARAXIAL RAY STARTING'
                              CALL SHOWIT(1)
                              OUTLYNE='VALUES'
                              CALL SHOWIT(1)
                              OUTLYNE='PARAXIAL TRACE STOPPED'
                              CALL SHOWIT(1)
                              CALL MACFAL
                              RETURN
                          ELSE
                              IF(SYSTEM1(64).EQ.1.0D0) THEN
                                  SYSTEM1(12)=ALENS(3,0)*SYSTEM1(65)
                              END IF
                              IF(SYSTEM1(64).EQ.2.0D0) THEN
                                  SYSTEM1(13)=ALENS(3,0)*SYSTEM1(66)
                              END IF
                              IF(SYSTEM1(64).EQ.3.0D0) THEN
                                  SYSTEM1(12)=ALENS(3,0)*SYSTEM1(65)
                                  SYSTEM1(13)=ALENS(3,0)*SYSTEM1(66)
                              END IF
                              IF(SYSTEM1(67).EQ.1.0D0) THEN
                                  SYSTEM1(12)=ALENS(3,0)/(2.0D0*SYSTEM1(68))
                              END IF
                              IF(SYSTEM1(67).EQ.2.0D0) THEN
                                  SYSTEM1(13)=ALENS(3,0)/(2.0D0*SYSTEM1(69))
                              END IF
                              IF(SYSTEM1(67).EQ.3.0D0) THEN
                                  SYSTEM1(12)=ALENS(3,0)/(2.0D0*SYSTEM1(68))
                                  SYSTEM1(13)=ALENS(3,0)/(2.0D0*SYSTEM1(69))
                              END IF
                          END IF
                      END IF
C
                      CALL PRTRA
                      ITYPEP=1
                      CALL FADJ
                      ITYPEP=3
C       TELECENTRIC STUFF, 11/12/2000
                      IF(SYSTEM1(63).EQ.1.0D0) THEN
                          IF(SYSTEM1(64).EQ.0.0D0.AND.SYSTEM1(67).EQ.0.0D0) THEN
                              OUTLYNE='WHEN "TEL ON" IS SET, NAO OR FNO MUST BE USED'
                              CALL SHOWIT(1)
                              OUTLYNE='TO SPECIFY THE MARGINAL PARAXIAL RAY STARTING'
                              CALL SHOWIT(1)
                              OUTLYNE='VALUES'
                              CALL SHOWIT(1)
                              OUTLYNE='PARAXIAL TRACE STOPPED'
                              CALL SHOWIT(1)
                              CALL MACFAL
                              RETURN
                          ELSE
                              IF(SYSTEM1(64).EQ.1.0D0) THEN
                                  SYSTEM1(12)=ALENS(3,0)*SYSTEM1(65)
                              END IF
                              IF(SYSTEM1(64).EQ.2.0D0) THEN
                                  SYSTEM1(13)=ALENS(3,0)*SYSTEM1(66)
                              END IF
                              IF(SYSTEM1(64).EQ.3.0D0) THEN
                                  SYSTEM1(12)=ALENS(3,0)*SYSTEM1(65)
                                  SYSTEM1(13)=ALENS(3,0)*SYSTEM1(66)
                              END IF
                              IF(SYSTEM1(67).EQ.1.0D0) THEN
                                  SYSTEM1(12)=ALENS(3,0)/(2.0D0*SYSTEM1(68))
                              END IF
                              IF(SYSTEM1(67).EQ.2.0D0) THEN
                                  SYSTEM1(13)=ALENS(3,0)/(2.0D0*SYSTEM1(69))
                              END IF
                              IF(SYSTEM1(67).EQ.3.0D0) THEN
                                  SYSTEM1(12)=ALENS(3,0)/(2.0D0*SYSTEM1(68))
                                  SYSTEM1(13)=ALENS(3,0)/(2.0D0*SYSTEM1(69))
                              END IF
                          END IF
                      END IF
C
                      CALL PRTRA
                      RETURN
                  ELSE
C       WC MUST BE 'FNBX'
                      IF(W1.EQ.0.0D0) THEN
                          OUTLYNE=
     1                    'F/0.0 IS MEANINGLESS, ADJUSTMENT NOT PERFORMED'
                          CALL SHOWIT(1)
                          CALL MACFAL
                          RETURN
                      END IF
                      IF(WQ.NE.'HLD') SYSTEM1(45)=1.0D0
                      IF(WQ.EQ.'HLD') SYSTEM1(45)=-1.0D0
                      SYSTEM1(47)=W1
C       CALL SUBROUTINE FXADJ TO ADJUST THE F NUMBER IN THE X PLANE
C       DO A PARAXIAL TRACE TO GET STARTING VALUES.
                      ITYPEP=3
C       TELECENTRIC STUFF, 11/12/2000
                      IF(SYSTEM1(63).EQ.1.0D0) THEN
                          IF(SYSTEM1(64).EQ.0.0D0.AND.SYSTEM1(67).EQ.0.0D0) THEN
                              OUTLYNE='WHEN "TEL ON" IS SET, NAO OR FNO MUST BE USED'
                              CALL SHOWIT(1)
                              OUTLYNE='TO SPECIFY THE MARGINAL PARAXIAL RAY STARTING'
                              CALL SHOWIT(1)
                              OUTLYNE='VALUES'
                              CALL SHOWIT(1)
                              OUTLYNE='PARAXIAL TRACE STOPPED'
                              CALL SHOWIT(1)
                              CALL MACFAL
                              RETURN
                          ELSE
                              IF(SYSTEM1(64).EQ.1.0D0) THEN
                                  SYSTEM1(12)=ALENS(3,0)*SYSTEM1(65)
                              END IF
                              IF(SYSTEM1(64).EQ.2.0D0) THEN
                                  SYSTEM1(13)=ALENS(3,0)*SYSTEM1(66)
                              END IF
                              IF(SYSTEM1(64).EQ.3.0D0) THEN
                                  SYSTEM1(12)=ALENS(3,0)*SYSTEM1(65)
                                  SYSTEM1(13)=ALENS(3,0)*SYSTEM1(66)
                              END IF
                              IF(SYSTEM1(67).EQ.1.0D0) THEN
                                  SYSTEM1(12)=ALENS(3,0)/(2.0D0*SYSTEM1(68))
                              END IF
                              IF(SYSTEM1(67).EQ.2.0D0) THEN
                                  SYSTEM1(13)=ALENS(3,0)/(2.0D0*SYSTEM1(69))
                              END IF
                              IF(SYSTEM1(67).EQ.3.0D0) THEN
                                  SYSTEM1(12)=ALENS(3,0)/(2.0D0*SYSTEM1(68))
                                  SYSTEM1(13)=ALENS(3,0)/(2.0D0*SYSTEM1(69))
                              END IF
                          END IF
                      END IF
C
                      CALL PRTRA
                      ITYPEP=2
                      CALL FADJ
                      ITYPEP=3
C       TELECENTRIC STUFF, 11/12/2000
                      IF(SYSTEM1(63).EQ.1.0D0) THEN
                          IF(SYSTEM1(64).EQ.0.0D0.AND.SYSTEM1(67).EQ.0.0D0) THEN
                              OUTLYNE='WHEN "TEL ON" IS SET, NAO OR FNO MUST BE USED'
                              CALL SHOWIT(1)
                              OUTLYNE='TO SPECIFY THE MARGINAL PARAXIAL RAY STARTING'
                              CALL SHOWIT(1)
                              OUTLYNE='VALUES'
                              CALL SHOWIT(1)
                              OUTLYNE='PARAXIAL TRACE STOPPED'
                              CALL SHOWIT(1)
                              CALL MACFAL
                              RETURN
                          ELSE
                              IF(SYSTEM1(64).EQ.1.0D0) THEN
                                  SYSTEM1(12)=ALENS(3,0)*SYSTEM1(65)
                              END IF
                              IF(SYSTEM1(64).EQ.2.0D0) THEN
                                  SYSTEM1(13)=ALENS(3,0)*SYSTEM1(66)
                              END IF
                              IF(SYSTEM1(64).EQ.3.0D0) THEN
                                  SYSTEM1(12)=ALENS(3,0)*SYSTEM1(65)
                                  SYSTEM1(13)=ALENS(3,0)*SYSTEM1(66)
                              END IF
                              IF(SYSTEM1(67).EQ.1.0D0) THEN
                                  SYSTEM1(12)=ALENS(3,0)/(2.0D0*SYSTEM1(68))
                              END IF
                              IF(SYSTEM1(67).EQ.2.0D0) THEN
                                  SYSTEM1(13)=ALENS(3,0)/(2.0D0*SYSTEM1(69))
                              END IF
                              IF(SYSTEM1(67).EQ.3.0D0) THEN
                                  SYSTEM1(12)=ALENS(3,0)/(2.0D0*SYSTEM1(68))
                                  SYSTEM1(13)=ALENS(3,0)/(2.0D0*SYSTEM1(69))
                              END IF
                          END IF
                      END IF
C
                      CALL PRTRA
                  END IF
                  RETURN
              ELSE
C       WQ IS DEL
                  IF(WC.EQ.'FNBY') THEN
                      ABSSYS=DABS(SYSTEM1(44))
                      IF(ABSSYS.EQ.1.0D0) THEN
                          OUTLYNE='"FNBY" ADJUSTMENT DELETED'
                          CALL SHOWIT(1)
                          SYSTEM1(44)=0.0D0
                          SYSTEM1(46)=0.0D0
                          F1=0
                          F6=1
                          F22=0
                          LNSTYP=1
                          CALL LNSEOS
                          RETURN
                      ELSE
                          OUTLYNE='NO FNBY ADJUSTMENT TO DELETE'
                          CALL SHOWIT(1)
                      END IF
                      CALL MACFAL
                      RETURN
                  ELSE
C       WC WAS FNBX
                      ABSSYS=DABS(SYSTEM1(45))
                      IF(ABSSYS.EQ.1.0D0) THEN
                          OUTLYNE='"FNBX" ADJUSTMENT DELETED'
                          CALL SHOWIT(1)
                          SYSTEM1(45)=0.0D0
                          SYSTEM1(47)=0.0D0
                          F1=0
                          F6=1
                          F22=0
                          LNSTYP=1
                          CALL LNSEOS
                          RETURN
                      ELSE
                          OUTLYNE='NO FNBX ADJUSTMENT TO DELETE'
                          CALL SHOWIT(1)
                      END IF
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
          END IF
C
  100     FORMAT('F-NUMBER (YZ-PLANE) IS INFINITE')
  101     FORMAT('F-NUMBER (YX-PLANE) IS INFINITE')
  200     FORMAT('F-NUMBER (YZ-PLANE) =',G12.5)
  201     FORMAT('F-NUMBER (XZ-PLANE) =',G12.5)
  300     FORMAT('F-NUMBER HOLD (YZ-PLANE) =',G12.5)
  301     FORMAT('F-NUMBER HOLD (XZ-PLANE) =',G12.5)
C
          RETURN
      END
C SUB PIVAXIS.FOR
      SUBROUTINE PIVAXIS
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE PIVAXIS WHICH IMPLEMENTS THE
C       "PIVAXIS"
C       COMMAND AT THE CMD LEVEL.
C
!      INTEGER AMODE
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          IF(STI.EQ.1.OR.SQ.EQ.0) THEN
              IF(ALENS(113,SURF).EQ.0.0D0)
     1        OUTLYNE='"PIVAXIS" IS SET TO "VERTEX"'
              IF(ALENS(113,SURF).EQ.1.0D0)
     1        OUTLYNE='"PIVAXIS" IS SET TO "NORMAL"'
              CALL SHOWIT(0)
              RETURN
          END IF
C
          IF(SST.EQ.1.OR.SN.EQ.1) THEN
              OUTLYNE='"PIVAXIS" TAKES NO STRING OR NUMERIC INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SQ.EQ.1) THEN
              IF(WQ.NE.'VERTEX'.AND.WQ.NE.'NORMAL') THEN
                  OUTLYNE='INVALID QUALIFIER INPUT USED WITH "PIVAXIS"'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
C
          IF(WQ.EQ.'VERTEX')THEN
              ALENS(113,SURF)=0.0D0
          END IF
          IF(WQ.EQ.'NORMAL')THEN
              ALENS(113,SURF)=1.0D0
          END IF
          RETURN
      END
C SUB SER.FOR
      SUBROUTINE SER
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE SER WHICH IMPLEMENTS THE ERY AND ERX
C       COMMANDS AT THE CMD LEVEL.
C
          CHARACTER UNIT*5
C
          REAL*8 ABSSYS,EP
C
          INTEGER ITYPEP
C
          COMMON/PTYPER/ITYPEP
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
          IF(SYSTEM1(6).EQ.1.0) UNIT='IN   '
          IF(SYSTEM1(6).EQ.2.0) UNIT='CM   '
          IF(SYSTEM1(6).EQ.3.0) UNIT='MM'
          IF(SYSTEM1(6).EQ.4.0) UNIT='M    '
C
C               CHECK FOR ADDITIONAL INPUT AND
C               PRINT ERROR AND RETURN IF DISCOVERED.
C
          IF(STI.EQ.1) THEN
              OUTLYNE=
     1        'QUERRY EXIT PUPIL VALUES FROM THE CMD LEVEL WITH THE'
              CALL SHOWIT(1)
              OUTLYNE=
     1        '"SHO" OR "GET" AND "WRITE" COMMANDS'
              CALL SHOWIT(1)
              RETURN
          END IF
          IF(SST.EQ.1.OR.S2.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1.OR.S3.EQ.1) THEN
C
              IF(WC.EQ.'ERY') THEN
                  OUTLYNE=
     1            '"ERY" TAKES NO STRING OR NUMERIC WORD #2 THROUGH #5 INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(WC.EQ.'ERX') THEN
                  OUTLYNE=
     1            '"ERX" TAKES NO STRING OR NUMERIC WORD #2 THROUGH #5 INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
C
          IF(DF1.EQ.1) THEN
C
              IF(WC.EQ.'ERY') THEN
                  OUTLYNE=
     1            '"ERY" REQUIRES EXPLICIT NUMERIC WORD #1 INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(WC.EQ.'ERX') THEN
                  OUTLYNE=
     1            '"ERX" REQUIRES EXPLICIT NUMERIC WORD #1 INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
          IF(WC.EQ.'ERX'.AND.SYSTEM1(64).NE.0.0D0) THEN
              OUTLYNE=
     1        '"ERX" CAN NOT BE USED WHEN "SAX" IS SPECIFIED BY "NAOX"'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(WC.EQ.'ERY'.AND.SYSTEM1(64).NE.0.0D0) THEN
              OUTLYNE=
     1        '"ERY" CAN NOT BE USED WHEN "SAY" IS SPECIFIED BY "NAOY"'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(WC.EQ.'ERX'.AND.SYSTEM1(67).NE.0.0D0) THEN
              OUTLYNE=
     1        '"ERX" CAN NOT BE USED WHEN "SAX" IS SPECIFIED BY "FNOX"'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(WC.EQ.'ERY'.AND.SYSTEM1(67).NE.0.0D0) THEN
              OUTLYNE=
     1        '"ERY" CAN NOT BE USED WHEN "SAY" IS SPECIFIED BY "FNOY"'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SYSTEM1(83).NE.0.0D0.OR.SYSTEM1(84).NE.0.0D0) THEN
              OUTLYNE='SAY OR SAX IS CURRENTLY FLOATING'
              CALL SHOWIT(1)
              OUTLYNE='"ERY/ERX" ADJUSTMENT NOT ALLOWED'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
          IF(SQ.EQ.0.AND.DF1.EQ.1.AND.DF2.EQ.1.AND.DF3.EQ.1.AND.
     1    DF4.EQ.1.AND.DF5.EQ.1) THEN
C       ERY OR ERX TYPED IN WITH NO INPUT,PRINTS OUT CURRENT VALUES
              IF(WC.EQ.'ERY'.AND.SYSTEM1(44).NE.2.0.AND.
     1        SYSTEM1(44).NE.-2.0) THEN
C       CALCULATE EXIT PUPIL RADIUS
                  EP=PXTRAY(1,(INT(SYSTEM1(20))))-
     1            (PXTRAY(2,(INT(SYSTEM1(20))))*(PXTRAY(5,(INT(SYSTEM1(20))))/
     2            PXTRAY(6,(INT(SYSTEM1(20))))))
                  WRITE(OUTLYNE,200) EP,UNIT
                  CALL SHOWIT(0)
                  RETURN
              ELSE
              END IF
              ABSSYS=DABS(SYSTEM1(44))
              IF(WC.EQ.'ERY'.AND.ABSSYS.EQ.2.0) THEN
                  EP=PXTRAY(1,(INT(SYSTEM1(20))))-
     1            (PXTRAY(2,(INT(SYSTEM1(20))))*(PXTRAY(5,(INT(SYSTEM1(20))))/
     2            PXTRAY(6,(INT(SYSTEM1(20))))))
                  IF(SYSTEM1(44).GT.0.0D0) THEN
                      WRITE(OUTLYNE,200) EP,UNIT
                      CALL SHOWIT(0)
                  END IF
                  IF(SYSTEM1(44).LT.0.0D0) THEN
                      WRITE(OUTLYNE,300) SYSTEM1(46),UNIT
                      CALL SHOWIT(0)
                  END IF
                  RETURN
              ELSE
              END IF
              IF(WC.EQ.'ERX'.AND.SYSTEM1(45).NE.2.0.AND.
     1        SYSTEM1(45).NE.-2.0) THEN
                  EP=PXTRAX(1,(INT(SYSTEM1(20))))-
     1            (PXTRAX(2,(INT(SYSTEM1(20))))*(PXTRAX(5,(INT(SYSTEM1(20))))/
     2            PXTRAX(6,(INT(SYSTEM1(20))))))
                  WRITE(OUTLYNE,201) EP,UNIT
                  CALL SHOWIT(0)
                  RETURN
              END IF
              ABSSYS=DABS(SYSTEM1(45))
              IF(WC.EQ.'ERX'.AND.ABSSYS.EQ.2.0) THEN
                  EP=PXTRAX(1,(INT(SYSTEM1(20))))-
     1            (PXTRAX(2,(INT(SYSTEM1(20))))*(PXTRAX(5,(INT(SYSTEM1(20))))
     2            /PXTRAX(6,(INT(SYSTEM1(20))))))
                  IF(SYSTEM1(45).GT.0.0D0) THEN
                      WRITE(OUTLYNE,201) EP,UNIT
                      CALL SHOWIT(0)
                  END IF
                  IF(SYSTEM1(45).LT.0.0D0) THEN
                      WRITE(OUTLYNE,301) SYSTEM1(47),UNIT
                      CALL SHOWIT(0)
                  END IF
                  RETURN
              ELSE
              END IF
C
          ELSE
C       NUMERIC INPUT AND QUALIFIER INPUT NOT DEFAULT
              IF(WQ.NE.'DEL'.AND.WQ.NE.'HLD'.AND.SQ.NE.0) THEN
                  OUTLYNE='INVALID QUALIFIER INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(WQ.NE.'DEL') THEN
                  IF(WC.EQ.'ERY') THEN
                      IF(W1.EQ.0.0D0) THEN
                          OUTLYNE='ZERO DIAMETER EXIT PUPIL RADIUS IS MEANINGLESS'
                          CALL SHOWIT(1)
                          OUTLYNE='EXIT PUPIL ADJUSTMENT NOT PERFORMED'
                          CALL SHOWIT(1)
                          CALL MACFAL
                          RETURN
                      END IF
C       CHECK LENS MODE, MUST BE AFOCAL OR UAFOCAL
                      IF(SYSTEM1(30).EQ.1.0.OR.SYSTEM1(30).EQ.2.0) THEN
                          IF(WC.EQ.'ERY') THEN
                              OUTLYNE='"ERY" REQUIRES THE AFOCAL OR UAFOCAL MODES'
                              CALL SHOWIT(1)
                              IF(SYSTEM1(30).EQ.1.0) THEN
                                  CALL SHOWIT(1)
                                  OUTLYNE='CURRENT MODE IS FOCAL'
                                  CALL SHOWIT(1)
                              END IF
                              IF(SYSTEM1(30).EQ.2.0) THEN
                                  OUTLYNE='CURRENT MODE IS UFOCAL'
                                  CALL SHOWIT(1)
                              END IF
                              OUTLYNE='"ERY" ADJUSTMENT NOT PERFORMED'
                              CALL SHOWIT(1)
                              CALL MACFAL
                              RETURN
                          ELSE
                          END IF
                          IF(WC.EQ.'ERX') THEN
                              OUTLYNE='"ERX" REQUIRES THE AFOCAL OR UAFOCAL MODES'
                              CALL SHOWIT(1)
                              IF(SYSTEM1(30).EQ.1.0) THEN
                                  OUTLYNE='CURRENT MODE IS FOCAL'
                                  CALL SHOWIT(1)
                              END IF
                              IF(SYSTEM1(30).EQ.2.0) THEN
                                  OUTLYNE='CURRENT MODE IS UFOCAL'
                                  CALL SHOWIT(1)
                              END IF
                              OUTLYNE='"ERX" ADJUSTMENT NOT PERFORMED'
                              CALL SHOWIT(1)
                              CALL MACFAL
                              RETURN
                          ELSE
                          END IF
                      ELSE
                      END IF
                      IF(WQ.NE.'HLD') SYSTEM1(44)=2.0
                      IF(WQ.EQ.'HLD') SYSTEM1(44)=-2.0
                      SYSTEM1(46)=W1
C       PERFORM ERY ADJUSTMENT BUT
C       FIRST DO A PARAXIAL TRACE FOR STARTING VALUES
                      ITYPEP=1
C       TELECENTRIC STUFF, 11/12/2000
                      IF(SYSTEM1(63).EQ.1.0D0) THEN
                          IF(SYSTEM1(64).EQ.0.0D0.AND.SYSTEM1(67).EQ.0.0D0) THEN
                              OUTLYNE='WHEN "TEL ON" IS SET, NAO OR FNO MUST BE USED'
                              CALL SHOWIT(1)
                              OUTLYNE='TO SPECIFY THE MARGINAL PARAXIAL RAY STARTING'
                              CALL SHOWIT(1)
                              OUTLYNE='VALUES'
                              CALL SHOWIT(1)
                              OUTLYNE='PARAXIAL TRACE STOPPED'
                              CALL SHOWIT(1)
                              CALL MACFAL
                              RETURN
                          ELSE
                              IF(SYSTEM1(64).EQ.1.0D0) THEN
                                  SYSTEM1(12)=ALENS(3,0)*SYSTEM1(65)
                              END IF
                              IF(SYSTEM1(64).EQ.2.0D0) THEN
                                  SYSTEM1(13)=ALENS(3,0)*SYSTEM1(66)
                              END IF
                              IF(SYSTEM1(64).EQ.3.0D0) THEN
                                  SYSTEM1(12)=ALENS(3,0)*SYSTEM1(65)
                                  SYSTEM1(13)=ALENS(3,0)*SYSTEM1(66)
                              END IF
                              IF(SYSTEM1(67).EQ.1.0D0) THEN
                                  SYSTEM1(12)=ALENS(3,0)/(2.0D0*SYSTEM1(68))
                              END IF
                              IF(SYSTEM1(67).EQ.2.0D0) THEN
                                  SYSTEM1(13)=ALENS(3,0)/(2.0D0*SYSTEM1(69))
                              END IF
                              IF(SYSTEM1(67).EQ.3.0D0) THEN
                                  SYSTEM1(12)=ALENS(3,0)/(2.0D0*SYSTEM1(68))
                                  SYSTEM1(13)=ALENS(3,0)/(2.0D0*SYSTEM1(69))
                              END IF
                          END IF
                      END IF
C
                      CALL PRTRA
                      CALL ERADJ
                      ITYPEP=3
C       TELECENTRIC STUFF, 11/12/2000
                      IF(SYSTEM1(63).EQ.1.0D0) THEN
                          IF(SYSTEM1(64).EQ.0.0D0.AND.SYSTEM1(67).EQ.0.0D0) THEN
                              OUTLYNE='WHEN "TEL ON" IS SET, NAO OR FNO MUST BE USED'
                              CALL SHOWIT(1)
                              OUTLYNE='TO SPECIFY THE MARGINAL PARAXIAL RAY STARTING'
                              CALL SHOWIT(1)
                              OUTLYNE='VALUES'
                              CALL SHOWIT(1)
                              OUTLYNE='PARAXIAL TRACE STOPPED'
                              CALL SHOWIT(1)
                              CALL MACFAL
                              RETURN
                          ELSE
                              IF(SYSTEM1(64).EQ.1.0D0) THEN
                                  SYSTEM1(12)=ALENS(3,0)*SYSTEM1(65)
                              END IF
                              IF(SYSTEM1(64).EQ.2.0D0) THEN
                                  SYSTEM1(13)=ALENS(3,0)*SYSTEM1(66)
                              END IF
                              IF(SYSTEM1(64).EQ.3.0D0) THEN
                                  SYSTEM1(12)=ALENS(3,0)*SYSTEM1(65)
                                  SYSTEM1(13)=ALENS(3,0)*SYSTEM1(66)
                              END IF
                              IF(SYSTEM1(67).EQ.1.0D0) THEN
                                  SYSTEM1(12)=ALENS(3,0)/(2.0D0*SYSTEM1(68))
                              END IF
                              IF(SYSTEM1(67).EQ.2.0D0) THEN
                                  SYSTEM1(13)=ALENS(3,0)/(2.0D0*SYSTEM1(69))
                              END IF
                              IF(SYSTEM1(67).EQ.3.0D0) THEN
                                  SYSTEM1(12)=ALENS(3,0)/(2.0D0*SYSTEM1(68))
                                  SYSTEM1(13)=ALENS(3,0)/(2.0D0*SYSTEM1(69))
                              END IF
                          END IF
                      END IF
C
                      CALL PRTRA
                      RETURN
                  ELSE
C       WC MUST BE 'ERX'
                      IF(W1.EQ.0.0D0) THEN
                          OUTLYNE='ZERO DIAMETER EXIT PUPIL RADIUS IS MEANINGLESS'
                          CALL SHOWIT(1)
                          OUTLYNE='ADJUSTMENT NOT PERFORMED'
                          CALL SHOWIT(1)
                          CALL MACFAL
                          RETURN
                      END IF
C       CHECK LENS MODE, MUST BE AFOCAL OR UAFOCAL
                      IF(WQ.NE.'HLD') SYSTEM1(45)=2.0
                      IF(WQ.EQ.'HLD') SYSTEM1(45)=-2.0
                      SYSTEM1(47)=W1
C               PERFORM ERX ADJUSTMENT
C       FIRST DO A PARAXIAL TRACE FOR STARTING VALUES
                      ITYPEP=3
C       TELECENTRIC STUFF, 11/12/2000
                      IF(SYSTEM1(63).EQ.1.0D0) THEN
                          IF(SYSTEM1(64).EQ.0.0D0.AND.SYSTEM1(67).EQ.0.0D0) THEN
                              OUTLYNE='WHEN "TEL ON" IS SET, NAO OR FNO MUST BE USED'
                              CALL SHOWIT(1)
                              OUTLYNE='TO SPECIFY THE MARGINAL PARAXIAL RAY STARTING'
                              CALL SHOWIT(1)
                              OUTLYNE='VALUES'
                              CALL SHOWIT(1)
                              OUTLYNE='PARAXIAL TRACE STOPPED'
                              CALL SHOWIT(1)
                              CALL MACFAL
                              RETURN
                          ELSE
                              IF(SYSTEM1(64).EQ.1.0D0) THEN
                                  SYSTEM1(12)=ALENS(3,0)*SYSTEM1(65)
                              END IF
                              IF(SYSTEM1(64).EQ.2.0D0) THEN
                                  SYSTEM1(13)=ALENS(3,0)*SYSTEM1(66)
                              END IF
                              IF(SYSTEM1(64).EQ.3.0D0) THEN
                                  SYSTEM1(12)=ALENS(3,0)*SYSTEM1(65)
                                  SYSTEM1(13)=ALENS(3,0)*SYSTEM1(66)
                              END IF
                              IF(SYSTEM1(67).EQ.1.0D0) THEN
                                  SYSTEM1(12)=ALENS(3,0)/(2.0D0*SYSTEM1(68))
                              END IF
                              IF(SYSTEM1(67).EQ.2.0D0) THEN
                                  SYSTEM1(13)=ALENS(3,0)/(2.0D0*SYSTEM1(69))
                              END IF
                              IF(SYSTEM1(67).EQ.3.0D0) THEN
                                  SYSTEM1(12)=ALENS(3,0)/(2.0D0*SYSTEM1(68))
                                  SYSTEM1(13)=ALENS(3,0)/(2.0D0*SYSTEM1(69))
                              END IF
                          END IF
                      END IF
C
                      CALL PRTRA
                      CALL ERADJ
                      ITYPEP=3
C       TELECENTRIC STUFF, 11/12/2000
                      IF(SYSTEM1(63).EQ.1.0D0) THEN
                          IF(SYSTEM1(64).EQ.0.0D0.AND.SYSTEM1(67).EQ.0.0D0) THEN
                              OUTLYNE='WHEN "TEL ON" IS SET, NAO OR FNO MUST BE USED'
                              CALL SHOWIT(1)
                              OUTLYNE='TO SPECIFY THE MARGINAL PARAXIAL RAY STARTING'
                              CALL SHOWIT(1)
                              OUTLYNE='VALUES'
                              CALL SHOWIT(1)
                              OUTLYNE='PARAXIAL TRACE STOPPED'
                              CALL SHOWIT(1)
                              CALL MACFAL
                              RETURN
                          ELSE
                              IF(SYSTEM1(64).EQ.1.0D0) THEN
                                  SYSTEM1(12)=ALENS(3,0)*SYSTEM1(65)
                              END IF
                              IF(SYSTEM1(64).EQ.2.0D0) THEN
                                  SYSTEM1(13)=ALENS(3,0)*SYSTEM1(66)
                              END IF
                              IF(SYSTEM1(64).EQ.3.0D0) THEN
                                  SYSTEM1(12)=ALENS(3,0)*SYSTEM1(65)
                                  SYSTEM1(13)=ALENS(3,0)*SYSTEM1(66)
                              END IF
                              IF(SYSTEM1(67).EQ.1.0D0) THEN
                                  SYSTEM1(12)=ALENS(3,0)/(2.0D0*SYSTEM1(68))
                              END IF
                              IF(SYSTEM1(67).EQ.2.0D0) THEN
                                  SYSTEM1(13)=ALENS(3,0)/(2.0D0*SYSTEM1(69))
                              END IF
                              IF(SYSTEM1(67).EQ.3.0D0) THEN
                                  SYSTEM1(12)=ALENS(3,0)/(2.0D0*SYSTEM1(68))
                                  SYSTEM1(13)=ALENS(3,0)/(2.0D0*SYSTEM1(69))
                              END IF
                          END IF
                      END IF
C
                      CALL PRTRA
                  END IF
                  RETURN
              ELSE
C       WQ IS DEL
                  IF(WC.EQ.'ERY') THEN
                      ABSSYS=DABS(SYSTEM1(44))
                      IF(ABSSYS.EQ.2.0) THEN
                          OUTLYNE='"ERY" ADJUSTMENT DELETED'
                          CALL SHOWIT(1)
                          SYSTEM1(44)=0.0
                          SYSTEM1(46)=0.0
                          F1=0
                          F6=1
                          F22=0
                          LNSTYP=1
                          CALL LNSEOS
                          RETURN
                      ELSE
                          OUTLYNE='NO ERY ADJUSTMENT TO DELETE'
                          CALL SHOWIT(1)
                      END IF
                      CALL MACFAL
                      RETURN
                  ELSE
C       WC WAS ERX
                      ABSSYS=DABS(SYSTEM1(45))
                      IF(ABSSYS.EQ.2.0) THEN
                          OUTLYNE='"ERX" ADJUSTMENT DELETED'
                          CALL SHOWIT(1)
                          SYSTEM1(45)=0.0
                          SYSTEM1(47)=0.0
                          F1=0
                          F6=1
                          F22=0
                          LNSTYP=1
                          CALL LNSEOS
                          RETURN
                      ELSE
                          OUTLYNE='NO ERX ADJUSTMENT TO DELETE'
                          CALL SHOWIT(1)
                      END IF
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
          END IF
C
  200     FORMAT('EXIT PUPIL RADIUS (YZ-PLANE) =',G12.5,1X,A5)
  201     FORMAT('EXIT PUPIL RADIUS (XZ-PLANE) =',G12.5,1X,A5)
  300     FORMAT('EXIT PUPIL RADIUS HOLD (YZ-PLANE) =',G12.5,1X,A5)
  301     FORMAT('EXIT PIPIL RADIUS HOLD (XZ-PLANE) =',G12.5,1X,A5)
C
          RETURN
      END
C SUB SDEFG.FOR
      SUBROUTINE SDEFG
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE SDEFG WHICH IMPLEMENTS THE AC,AD,
C       AE,AF,AG,ADTOR,AETOR,AFTOR AND AGTOR
C       COMMANDS AT THE UPDATE LENS LEVEL.
C
          INTEGER CT,PIKCNT,I
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
C               CHECK FOR PRESENCE OF QUALIFIER OR STRING INPUT
C               PRINT ERROR AND RETURN IF DISCOVERED.
C
          IF(S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1
     1    .OR.S5.EQ.1) THEN
              IF(WC.EQ.'AD'.OR.WC.EQ.'AE'.OR.WC.EQ.'AF'
     1        .OR.WC.EQ.'AG'.OR.WC.EQ.'AC')
     2        OUTLYNE=
     2        'AC,AD,AE,AF AND AG TAKE NO NUMERIC WORD #2 THROUGH #5 INPUT'
              IF(WC.EQ.'AH'.OR.WC.EQ.'AI'.OR.WC.EQ.'AJ'
     1        .OR.WC.EQ.'AK'.OR.WC.EQ.'AL')
     2        OUTLYNE=
     2        'AH,AI,AJ,AK AND AL TAKE NO NUMERIC WORD #2 THROUGH #5 INPUT'
              IF(WC.EQ.'ADTOR'.OR.WC.EQ.'AETOR'.OR.WC.EQ.'AFTOR'
     1        .OR.WC.EQ.'AGTOR')
     2        OUTLYNE=
     3        'ADTOR, AETOR, AFTOR AND AGTOR '//
     3        'TAKE NO NUMERIC WORD #2 THROUGH #5 INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SST.EQ.1) THEN
              IF(WC.EQ.'AD'.OR.WC.EQ.'AE'.OR.WC.EQ.'AF'
     1        .OR.WC.EQ.'AG'.OR.WC.EQ.'AC')
     2        OUTLYNE=
     2        'AC,AD,AE,AF AND AG TAKE NO STRING INPUT'
              IF(WC.EQ.'AH'.OR.WC.EQ.'AI'.OR.WC.EQ.'AJ'
     1        .OR.WC.EQ.'AK'.OR.WC.EQ.'AL')
     2        OUTLYNE=
     2        'AH,AI,AJ,AK AND AL TAKE NO STRING INPUT'
              IF(WC.EQ.'ADTOR'.OR.WC.EQ.'AETOR'.OR.WC.EQ.'AFTOR'
     1        .OR.WC.EQ.'AGTOR')
     2        OUTLYNE=
     3        'ADTOR, AETOR, AFTOR AND AGTOR '//
     3        'TAKE NO STRING INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SQ.EQ.1.AND.F5.EQ.1) THEN
              IF(WC.EQ.'AD'.OR.WC.EQ.'AE'.OR.WC.EQ.'AF'
     1        .OR.WC.EQ.'AG'.OR.WC.EQ.'AC')
     2        OUTLYNE=
     2        'AC,AD,AE,AF AND AG TAKE NO QUALIFIER WORD IN LENS INPUT MODE'
              IF(WC.EQ.'AH'.OR.WC.EQ.'AI'.OR.WC.EQ.'AJ'
     1        .OR.WC.EQ.'AK'.OR.WC.EQ.'AL')
     2        OUTLYNE=
     2        'AH,AI,AJ,AK AND AL TAKE NO QUALIFIER WORD IN LENS INPUT MODE'
              IF(WC.EQ.'ADTOR'.OR.WC.EQ.'AETOR'.OR.WC.EQ.'AFTOR'
     1        .OR.WC.EQ.'AGTOR')
     2        OUTLYNE=
     3        'ADTOR, AETOR, AFTOR AND AGTOR '//
     3        'TAKE NO QUALIFIER WORD IN LENS INPUT MODE'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(F6.EQ.1) THEN
              IF(WQ.NE.'        '.AND.WQ.NE.'CENT'.AND.WQ.NE.'DELT') THEN
                  IF(WC.EQ.'AD'.OR.WC.EQ.'AE'.OR.WC.EQ.'AF'
     1            .OR.WC.EQ.'AG'.OR.WC.EQ.'AC')
     1            OUTLYNE=
     2            'INVALID QUALIFIER WORD USED WITH AC,AD,AE,AF OR AG'
                  IF(WC.EQ.'AH'.OR.WC.EQ.'AI'.OR.WC.EQ.'AJ'
     1            .OR.WC.EQ.'AK'.OR.WC.EQ.'AL')
     1            OUTLYNE=
     2            'INVALID QUALIFIER WORD USED WITH AH,AI,AJ,AK OR AL'
                  IF(WC.EQ.'ADTOR'.OR.WC.EQ.'AETOR'.OR.WC.EQ.'AFTOR'
     1            .OR.WC.EQ.'AGTOR') THEN
                      OUTLYNE=
     3                'INVALID QUALIFIER WORD USED WITH'
                      OUTLYNE=
     3                'ADTOR, AETOR, AFTOR OR AGTOR'
                  END IF
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
          IF(DF1.EQ.1)THEN
              IF(WC.EQ.'AD'.OR.WC.EQ.'AE'.OR.WC.EQ.'AF'
     1        .OR.WC.EQ.'AG'.OR.WC.EQ.'AC')
     2        OUTLYNE=
     2        'AC,AD,AE,AF OR AG REQUIRES EXPLICIT NUMERIC WORD #1 INPUT'
              IF(WC.EQ.'AH'.OR.WC.EQ.'AI'.OR.WC.EQ.'AJ'
     1        .OR.WC.EQ.'AK'.OR.WC.EQ.'AL')
     2        OUTLYNE=
     2        'AH,AI,AJ,AK OR AL REQUIRES EXPLICIT NUMERIC WORD #1 INPUT'
              IF(WC.EQ.'ADTOR'.OR.WC.EQ.'AETOR'.OR.WC.EQ.'AFTOR'
     1        .OR.WC.EQ.'AGTOR') THEN
                  OUTLYNE=
     3            'ADTOR,AETOR,AFTOR OR AGTOR REQUIRES EXPLICIT'
                  OUTLYNE='NUMERIC WORD #1 INPUT'
              END IF
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
C
C               WE ARE AT LENS UPDATE LEVEL
          IF(WC.EQ.'AC') CT=26
          IF(WC.EQ.'AD') CT=5
          IF(WC.EQ.'AE') CT=6
          IF(WC.EQ.'AF') CT=7
          IF(WC.EQ.'AG') CT=8
          IF(WC.EQ.'AH') CT=27
          IF(WC.EQ.'AI') CT=28
          IF(WC.EQ.'AJ') CT=29
          IF(WC.EQ.'AK') CT=30
          IF(WC.EQ.'AL') CT=31
          IF(WC.EQ.'ADTOR') CT=22
          IF(WC.EQ.'AETOR') CT=23
          IF(WC.EQ.'AFTOR') CT=24
          IF(WC.EQ.'AGTOR') CT=25
C
          IF(WC.EQ.'AD'.OR.WC.EQ.'AF'.OR.WC.EQ.'AE'.OR.
     1    WC.EQ.'AG'.OR.WC.EQ.'AC'.OR.WC.EQ.'AH'.OR.WC.EQ.'AI'
     2    .OR.WC.EQ.'AJ'.OR.WC.EQ.'AK'.OR.WC.EQ.'AL') THEN
              IF(ALENS(8,SURF).NE.1.0D0) THEN
C       SURFACE NOT ASPHERIC, SET IT AS SUCH.
                  IF(WC.EQ.'AC') THEN
                      IF(ALENS(1,SURF).NE.0.0D0) THEN
                          OUTLYNE='WARNING:'
                          CALL SHOWIT(1)
                          WRITE(OUTLYNE,*)'FOR SURFACE ',SURF
                          CALL SHOWIT(1)
                          OUTLYNE=
     1                    'THE "AC" TERM WILL BE IGNORED FOR THIS NON-PLANO SURFACE'
                          CALL SHOWIT(1)
                      END IF
                      W5=W1
                      DF1=1
                      DF2=1
                      DF3=1
                      DF4=1
                      DF5=0
                      W1=0.0D0
                      W2=0.0D0
                      W3=0.0D0
                      W4=0.0D0
                      SQ=0
                      SN=1
                      SST=0
                  ELSE
                  END IF
                  IF(WC.EQ.'AD') THEN
                      W1=W1
                      DF1=0
                      DF2=1
                      DF3=1
                      DF4=1
                      DF5=1
                      W2=0.0D0
                      W3=0.0D0
                      W4=0.0D0
                      W5=0.0D0
                      SQ=0
                      SN=1
                      SST=0
                  ELSE
                  END IF
                  IF(WC.EQ.'AE') THEN
                      W2=W1
                      DF1=1
                      DF2=0
                      DF3=1
                      DF4=1
                      DF5=1
                      W1=0.0D0
                      W3=0.0D0
                      W4=0.0D0
                      W5=0.0D0
                      SQ=0
                      SN=1
                      SST=0
                  ELSE
                  END IF
                  IF(WC.EQ.'AF') THEN
                      W3=W1
                      DF1=1
                      DF2=1
                      DF3=0
                      DF4=1
                      DF5=1
                      W1=0.0D0
                      W2=0.0D0
                      W4=0.0D0
                      W5=0.0D0
                      SQ=0
                      SN=1
                      SST=0
                  ELSE
                  END IF
                  IF(WC.EQ.'AG') THEN
                      W4=W1
                      DF1=1
                      DF2=1
                      DF3=1
                      DF4=0
                      DF5=1
                      W1=0.0D0
                      W2=0.0D0
                      W3=0.0D0
                      W5=0.0D0
                      SQ=0
                      SN=1
                      SST=0
                  ELSE
                  END IF
                  IF(WC.EQ.'AH') THEN
                      W1=W1
                      DF1=0
                      DF2=1
                      DF3=1
                      DF4=1
                      DF5=1
                      W2=0.0D0
                      W3=0.0D0
                      W4=0.0D0
                      W5=0.0D0
                      SQ=0
                      SN=1
                      SST=0
                  ELSE
                  END IF
                  IF(WC.EQ.'AI') THEN
                      W2=W1
                      DF1=1
                      DF2=0
                      DF3=1
                      DF4=1
                      DF5=1
                      W1=0.0D0
                      W3=0.0D0
                      W4=0.0D0
                      W5=0.0D0
                      SQ=0
                      SN=1
                      SST=0
                  ELSE
                  END IF
                  IF(WC.EQ.'AJ') THEN
                      W3=W1
                      DF1=1
                      DF2=1
                      DF3=0
                      DF4=1
                      DF5=1
                      W1=0.0D0
                      W2=0.0D0
                      W4=0.0D0
                      W5=0.0D0
                      SQ=0
                      SN=1
                      SST=0
                  ELSE
                  END IF
                  IF(WC.EQ.'AK') THEN
                      W4=W1
                      DF1=1
                      DF2=1
                      DF3=1
                      DF4=0
                      DF5=1
                      W1=0.0D0
                      W2=0.0D0
                      W3=0.0D0
                      W5=0.0D0
                      SQ=0
                      SN=1
                      SST=0
                  ELSE
                  END IF
                  IF(WC.EQ.'AL') THEN
                      W5=W1
                      DF1=1
                      DF2=1
                      DF3=1
                      DF4=1
                      DF5=0
                      W1=0.0D0
                      W2=0.0D0
                      W3=0.0D0
                      W4=0.0D0
                      SQ=0
                      SN=1
                      SST=0
                  ELSE
                  END IF
                  IF(WC.EQ.'AC'.OR.WC.EQ.'AD'.OR.WC.EQ.'AF'.OR.WC.EQ.'AG'.OR.
     1            WC.EQ.'AE')
     2            CALL SASPH
                  IF(WC.EQ.'AH'.OR.WC.EQ.'AI'.OR.WC.EQ.'AJ'.OR.WC.EQ.'AK'.OR.
     1            WC.EQ.'AL')
     2            CALL SASPH
                  RETURN
              ELSE
              END IF
              IF(ALENS(8,SURF).EQ.1.0D0) THEN
                  IF(WC.EQ.'AC') THEN
                      IF(SQ.EQ.0) ALENS(43,SURF)=W1
                      IF(WQ.EQ.'DELT') ALENS(43,SURF)=ALENS(43,SURF)+W1
                      IF(WQ.EQ.'CENT')
     1                ALENS(43,SURF)=ALENS(43,SURF)+(W1*0.01D0*ALENS(43,SURF))
                      IF(ALENS(1,SURF).NE.0.0D0) THEN
                          OUTLYNE='WARNING:'
                          CALL SHOWIT(1)
                          WRITE(OUTLYNE,*)'FOR SURFACE ',SURF
                          CALL SHOWIT(1)
                          OUTLYNE=
     1                    'THE "AC" TERM WILL BE IGNORED FOR THIS NON-PLANO SURFACE'
                          CALL SHOWIT(1)
                      END IF
                  END IF
                  IF(SQ.EQ.0) THEN
                      IF(WC.EQ.'AD') ALENS(4,SURF)=W1
                      IF(WC.EQ.'AE') ALENS(5,SURF)=W1
                      IF(WC.EQ.'AF') ALENS(6,SURF)=W1
                      IF(WC.EQ.'AG') ALENS(7,SURF)=W1
                      IF(WC.EQ.'AH') ALENS(81,SURF)=W1
                      IF(WC.EQ.'AI') ALENS(82,SURF)=W1
                      IF(WC.EQ.'AJ') ALENS(83,SURF)=W1
                      IF(WC.EQ.'AK') ALENS(84,SURF)=W1
                      IF(WC.EQ.'AL') ALENS(85,SURF)=W1
                  END IF
                  IF(WQ.EQ.'DELT') THEN
                      IF(WC.EQ.'AD') ALENS(4,SURF)=ALENS(4,SURF)+W1
                      IF(WC.EQ.'AE') ALENS(5,SURF)=ALENS(5,SURF)+W1
                      IF(WC.EQ.'AF') ALENS(6,SURF)=ALENS(6,SURF)+W1
                      IF(WC.EQ.'AG') ALENS(7,SURF)=ALENS(7,SURF)+W1
                      IF(WC.EQ.'AH') ALENS(81,SURF)=ALENS(81,SURF)+W1
                      IF(WC.EQ.'AI') ALENS(82,SURF)=ALENS(82,SURF)+W1
                      IF(WC.EQ.'AJ') ALENS(83,SURF)=ALENS(83,SURF)+W1
                      IF(WC.EQ.'AK') ALENS(84,SURF)=ALENS(84,SURF)+W1
                      IF(WC.EQ.'AL') ALENS(85,SURF)=ALENS(85,SURF)+W1
                  END IF
                  IF(WQ.EQ.'DELT') THEN
                      IF(WC.EQ.'AD')
     1                ALENS(4,SURF)=ALENS(4,SURF)+(W1*0.01D0*ALENS(4,SURF))
                      IF(WC.EQ.'AE')
     1                ALENS(5,SURF)=ALENS(5,SURF)+(W1*0.01D0*ALENS(5,SURF))
                      IF(WC.EQ.'AF')
     1                ALENS(6,SURF)=ALENS(6,SURF)+(W1*0.01D0*ALENS(6,SURF))
                      IF(WC.EQ.'AG')
     1                ALENS(7,SURF)=ALENS(7,SURF)+(W1*0.01D0*ALENS(7,SURF))
                      IF(WC.EQ.'AH')
     1                ALENS(81,SURF)=ALENS(81,SURF)+(W1*0.01D0*ALENS(81,SURF))
                      IF(WC.EQ.'AI')
     1                ALENS(82,SURF)=ALENS(82,SURF)+(W1*0.01D0*ALENS(82,SURF))
                      IF(WC.EQ.'AJ')
     1                ALENS(83,SURF)=ALENS(83,SURF)+(W1*0.01D0*ALENS(83,SURF))
                      IF(WC.EQ.'AK')
     1                ALENS(84,SURF)=ALENS(84,SURF)+(W1*0.01D0*ALENS(84,SURF))
                      IF(WC.EQ.'AL')
     1                ALENS(85,SURF)=ALENS(85,SURF)+(W1*0.01D0*ALENS(85,SURF))
                  END IF
              ELSE
              END IF
          ELSE
C       NOT AC,AD,AE,AF,AG,AH,AI,AJ,AK OR AL
          END IF
          IF(WC.EQ.'ADTOR'.OR.WC.EQ.'AETOR'.OR.WC.EQ.'AFTOR'
     1    .OR.WC.EQ.'AGTOR') THEN
              IF(ALENS(23,SURF).EQ.0.0D0) THEN
                  WRITE(OUTLYNE,*)'SURFACE',SURF,' NOT A TORIC'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-DEFINE SURFACE TYPE AS X OR Y-TORIC AND THEN'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(ALENS(36,SURF).NE.1.0D0) THEN
                  IF(WC.EQ.'ADTOR') THEN
                      W1=W1
                      W2=0.0D0
                      W3=0.0D0
                      W4=0.0D0
                      DF1=0
                      DF2=1
                      DF3=1
                      DF4=1
                      DF5=1
                      SQ=0
                      SST=0
                  ELSE
                  END IF
                  IF(WC.EQ.'AETOR') THEN
                      W2=W1
                      W1=0.0D0
                      W3=0.0D0
                      W4=0.0D0
                      DF1=1
                      DF2=0
                      DF3=1
                      DF4=1
                      DF5=1
                      SQ=0
                      SST=0
                  ELSE
                  END IF
                  IF(WC.EQ.'AFTOR') THEN
                      W3=W1
                      W1=0.0D0
                      W2=0.0D0
                      W4=0.0D0
                      DF1=1
                      DF2=1
                      DF3=0
                      DF4=1
                      DF5=1
                      SQ=0
                      SST=0
                  ELSE
                  END IF
                  IF(WC.EQ.'AGTOR') THEN
                      W4=W1
                      W1=0.0D0
                      W2=0.0D0
                      W3=0.0D0
                      W4=0.0D0
                      DF1=1
                      DF2=1
                      DF3=1
                      DF4=0
                      DF5=1
                      SQ=0
                      SST=0
                  ELSE
                  END IF
                  CALL STASPH
              ELSE
              END IF
              IF(ALENS(36,SURF).EQ.1.0D0) THEN
                  IF(SQ.EQ.0) THEN
                      IF(WC.EQ.'ADTOR') ALENS(37,SURF)=W1
                      IF(WC.EQ.'AETOR') ALENS(38,SURF)=W1
                      IF(WC.EQ.'AFTOR') ALENS(39,SURF)=W1
                      IF(WC.EQ.'AGTOR') ALENS(40,SURF)=W1
                  END IF
                  IF(WQ.EQ.'DELT') THEN
                      IF(WC.EQ.'ADTOR') ALENS(37,SURF)=ALENS(37,SURF)+W1
                      IF(WC.EQ.'AETOR') ALENS(38,SURF)=ALENS(38,SURF)+W1
                      IF(WC.EQ.'AFTOR') ALENS(39,SURF)=ALENS(39,SURF)+W1
                      IF(WC.EQ.'AGTOR') ALENS(40,SURF)=ALENS(40,SURF)+W1
                  END IF
                  IF(WQ.EQ.'DELT') THEN
                      IF(WC.EQ.'ADTOR')
     1                ALENS(37,SURF)=ALENS(37,SURF)+(W1*0.01D0*ALENS(37,SURF))
                      IF(WC.EQ.'AETOR')
     1                ALENS(38,SURF)=ALENS(38,SURF)+(W1*0.01D0*ALENS(38,SURF))
                      IF(WC.EQ.'AFTOR')
     1                ALENS(39,SURF)=ALENS(39,SURF)+(W1*0.01D0*ALENS(39,SURF))
                      IF(WC.EQ.'AGTOR')
     1                ALENS(40,SURF)=ALENS(40,SURF)+(W1*0.01D0*ALENS(40,SURF))
                  END IF
              ELSE
              END IF
          ELSE
C       NOT ADTOR,AETOR,AFTOR OR AGTOR
          END IF
C
C       IF THERE ARE PIKUPS FOR AD,AE,AF,AG,AH,AI,AJ,AK OR AL
C       THEN DELETE THEM.
C
C       CHECK FOR ANY PIKUPS
C
C       CHECK FOR PIKUP
          IF(PIKUP(1,SURF,CT).EQ.1.0D0)THEN
C       DELETE PIKUP
              PIKUP(1:6,SURF,CT)=0.0D0
              ALENS(32,SURF)=ALENS(32,SURF)-1.0D0
              IF(WC.EQ.'AD'.OR.WC.EQ.'AE'.OR.WC.EQ.'AF'
     1        .OR.WC.EQ.'AG'.OR.WC.EQ.'AC'.OR.WC.EQ.'AH'.OR.WC.EQ.'AI'
     2        .OR.WC.EQ.'AJ'.OR.WC.EQ.'AK'.OR.WC.EQ.'AL')
     2        WRITE(OUTLYNE,*)
     3        'SURFACE',SURF,' : PIKUP (',WC(1:2),') DELETED'
              IF(WC.EQ.'ADTOR'.OR.WC.EQ.'AETOR'.OR.WC.EQ.'AFTOR'
     1        .OR.WC.EQ.'AGTOR')
     2        WRITE(OUTLYNE,*)
     3        'SURFACE',SURF,' : PIKUP (',WC(1:5),') DELETED'
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
C       NOW SEE IF THERE ARE AND REMAINING PIKUPS. IF NOT, SET
C       ALENS(32,SURF) TO ZERO
          END IF
C
          PIKCNT=0
          DO 10 I=1,PSIZ
              IF(PIKUP(1,SURF,I).NE.0.0D0) THEN
                  PIKCNT=PIKCNT+1
              ELSE
              END IF
 10       CONTINUE
C
          IF(PIKCNT.EQ.0) ALENS(32,SURF)=0.0D0
C
          RETURN
      END
C SUB SPIVOT.FOR
      SUBROUTINE SPIVOT
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE SPIVOT WHICH IMPLEMENTS THE PIVOT
C       COMMAND AT THE LENS INPUT LEVEL AND LENS UPDATE LEVEL.
C       THE DEC COMMAND IS:
C
C               PIVOT X Y Z
C
          INTEGER PIKCNT,I
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
C               CHECK FOR ADDITIONAL INPUT AND
C               PRINT ERROR AND RETURN IF DISCOVERED.
C
          IF(SST.EQ.1.OR.SQ.EQ.1) THEN
              OUTLYNE='"PIVOT" TAKES NO QUALIFIER OR STRING INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(S4.EQ.1.OR.S5.EQ.1) THEN
              OUTLYNE='"PIVOT" TAKES NO NUMERIC WORD #4 OR #5 INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(DF1.EQ.1.AND.DF2.EQ.1.AND.DF3.EQ.1) THEN
              OUTLYNE='"PIVOT" REQUIRES SOME EXPLICIT NUMERIC INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
          IF(F5.EQ.1.OR.F6.EQ.1) THEN
              IF(SURF.EQ.0) THEN
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
C               PROCEED
C
C       CHECK FOR PIKUP PIVX IF FOUND REMOVE
C
              IF(PIKUP(1,SURF,34).EQ.1.0D0) THEN
C       DELETE PIKUP PIVX
                  PIKUP(1:6,SURF,34)=0.0D0
                  WRITE(OUTLYNE,*)'SURFACE',SURF,' :PIKUP PIVX DELETED'
                  CALL SHOWIT(1)
              END IF
              IF(PIKUP(1,SURF,35).EQ.1.0D0) THEN
C       DELETE PIKUP PIVY
                  PIKUP(1:6,SURF,35)=0.0D0
                  WRITE(OUTLYNE,*)'SURFACE',SURF,' :PIKUP PIVY DELETED'
                  CALL SHOWIT(1)
              END IF
              IF(PIKUP(1,SURF,36).EQ.1.0D0) THEN
C       DELETE PIKUP PIVZ
                  PIKUP(1:6,SURF,36)=0.0D0
                  WRITE(OUTLYNE,*)'SURFACE',SURF,' :PIKUP PIVZ DELETED'
                  CALL SHOWIT(1)
              END IF
C
C       SET ALENS(32,SURF) IF NO OTHER PIKUPS EXIST
              PIKCNT=0
              DO 10 I=1,PSIZ
                  IF(PIKUP(1,SURF,I).NE.0.0D0) THEN
                      PIKCNT=PIKCNT+1
                  ELSE
                  END IF
 10           CONTINUE
              IF(PIKCNT.EQ.0) ALENS(32,SURF)=0.0D0
C
C       PROCEED WITH PIVOT ASSIGNMENT
C
              IF(W1.EQ.0.0D0.AND.W2.EQ.0.0D0.AND.W3.EQ.0.0D0) THEN
                  IF(DF1.EQ.1) W1=0.0D0
                  IF(DF2.EQ.1) W2=0.0D0
                  IF(DF3.EQ.1) W3=0.0D0
                  ALENS(59,SURF)=1.0D0
                  ALENS(78,SURF)=W1
                  ALENS(79,SURF)=W2
                  ALENS(80,SURF)=W3
                  ALENS(30,SURF)=0.0D0
                  ALENS(31,SURF)=0.0D0
                  ALENS(69,SURF)=0.0D0
                  ALENS(114,SURF)=0.0D0
                  ALENS(115,SURF)=0.0D0
                  ALENS(116,SURF)=0.0D0
              ELSE
                  IF(DF1.EQ.1) W1=0.0D0
                  IF(DF2.EQ.1) W2=0.0D0
                  IF(DF3.EQ.1) W3=0.0D0
                  ALENS(59,SURF)=1.0D0
                  ALENS(78,SURF)=W1
                  ALENS(79,SURF)=W2
                  ALENS(80,SURF)=W3
                  ALENS(30,SURF)=0.0D0
                  ALENS(31,SURF)=0.0D0
                  ALENS(69,SURF)=0.0D0
                  ALENS(114,SURF)=0.0D0
                  ALENS(115,SURF)=0.0D0
                  ALENS(116,SURF)=0.0D0
              END IF
C
C       NOW WHAT IF THIS CURRENT SURFACE IS BEING PIKUP UP WITH A
C       PIKUP YD OR PIKUP XD FROM ANOTHER SURFACE. IF IT IS THEN
C       IF THE CURRENT WORK HERE CHANGED THE STATUS OF ALENS(29,SURF)
C       IT WILL CHANGE THE STATUS ON THE PIKING SURFACE.
C       RESOLVE THIS ISSUE.
C
              DO 100 I=0,INT(SYSTEM1(20))
                  IF(PIKUP(1,I,34).EQ.1.0D0) THEN
                      IF(INT(PIKUP(2,I,34)).EQ.SURF) THEN
C       UPDATE THE ALENS(59) ON THE PIKING SURFACE
                          ALENS(59,I)=ALENS(59,SURF)
                      ELSE
C       DONT DO ANYTHING
                      END IF
                  ELSE
C       NOT RIGHT KIND OF PIKUP,DO NOTING,PROCEED TO NEXT SURFACE
                  END IF
 100          CONTINUE
C
          END IF
          RETURN
      END
C SUB SPIVOTD.FOR
      SUBROUTINE SPIVOTD
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE SPIVOTD WHICH IMPLEMENTS THE PIVOTD
C       COMMAND AT THE LENS INPUT LEVEL AND LENS UPDATE LEVEL.
C               PIVOT X Y Z
C
          INTEGER PIKCNT,I
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
C               CHECK FOR ADDITIONAL INPUT AND
C               PRINT ERROR AND RETURN IF DISCOVERED.
C
          IF(SST.EQ.1.OR.SQ.EQ.1.OR.SN.EQ.1) THEN
              OUTLYNE='"PIVOTD" TAKES NO ADDITIONAL INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
          IF(F5.EQ.1.OR.F6.EQ.1) THEN
              IF(SURF.EQ.0) THEN
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
C               PROCEED
C
C       CHECK FOR PIKUP PIVX IF FOUND REMOVE
C
              IF(PIKUP(1,SURF,34).EQ.1.0D0) THEN
C       DELETE PIKUP PIVX
                  PIKUP(1:6,SURF,34)=0.0D0
                  WRITE(OUTLYNE,*)'SURFACE',SURF,' :PIKUP PIVX DELETED'
                  CALL SHOWIT(1)
              END IF
              IF(PIKUP(1,SURF,35).EQ.1.0D0) THEN
C       DELETE PIKUP PIVY
                  PIKUP(1:6,SURF,35)=0.0D0
                  WRITE(OUTLYNE,*)'SURFACE',SURF,' :PIKUP PIVY DELETED'
                  CALL SHOWIT(1)
              END IF
              IF(PIKUP(1,SURF,36).EQ.1.0D0) THEN
C       DELETE PIKUP PIVZ
                  PIKUP(1:6,SURF,36)=0.0D0
                  WRITE(OUTLYNE,*)'SURFACE',SURF,' :PIKUP PIVZ DELETED'
                  CALL SHOWIT(1)
              END IF
C
C       SET ALENS(32,SURF) IF NO OTHER PIKUPS EXIST
              PIKCNT=0
              DO 10 I=1,PSIZ
                  IF(PIKUP(1,SURF,I).NE.0.0D0) THEN
                      PIKCNT=PIKCNT+1
                  ELSE
                  END IF
 10           CONTINUE
              IF(PIKCNT.EQ.0) ALENS(32,SURF)=0.0D0
C
C       PROCEED WITH PIVOTD
C
              ALENS(59,SURF)=0.0D0
              ALENS(78:79,SURF)=0.0D0
              ALENS(80,SURF)=0.0D0
              ALENS(30:31,SURF)=0.0D0
              ALENS(69,SURF)=0.0D0
              ALENS(114:116,SURF)=0.0D0
C
C       NOW WHAT IF THIS CURRENT SURFACE IS BEING PIKUP UP WITH A
C       PIKUP YD OR PIKUP XD FROM ANOTHER SURFACE. IF IT IS THEN
C       IF THE CURRENT WORK HERE CHANGED THE STATUS OF ALENS(29,SURF)
C       IT WILL CHANGE THE STATUS ON THE PIKING SURFACE.
C       RESOLVE THIS ISSUE.
C
              DO 100 I=0,INT(SYSTEM1(20))
                  IF(PIKUP(1,I,34).EQ.1.0D0) THEN
                      IF(INT(PIKUP(2,I,34)).EQ.SURF) THEN
C       UPDATE THE ALENS(59) ON THE PIKING SURFACE
                          ALENS(59,I)=ALENS(59,SURF)
                      ELSE
C       DONT DO ANYTHING
                      END IF
                  ELSE
C       NOT RIGHT KIND OF PIKUP,DO NOTING,PROCEED TO NEXT SURFACE
                  END IF
 100          CONTINUE
C
          END IF
          RETURN
      END
C SUB SPIVAX.FOR
      SUBROUTINE SPIVAX
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE SPIVAX WHICH IMPLEMENTS THE PIVAXIS
C       COMMAND AT THE LENS INPUT LEVEL AND LENS UPDATE LEVEL.
C       THE COMMAND IS:
C
C               PIVAXIS (LOCAL OR NORMAL)
C
!        INTEGER PIKCNT,I
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
C               CHECK FOR ADDITIONAL INPUT AND
C               PRINT ERROR AND RETURN IF DISCOVERED.
C
          IF(SST.EQ.1.OR.SN.EQ.1) THEN
              OUTLYNE='"PIVAXIS" TAKES NO NUMERIC OR STRING INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SQ.EQ.1.AND.WQ.NE.'LOCAL'.AND.WQ.NE.'NORMAL') THEN
              OUTLYNE=
     1        '"PIVAXIS" TAKES QUALIFIER WORDS "LOCAL" AND "NORMAL" ONLY'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
          IF(SURF.EQ.0) THEN
              OUTLYNE='OBJECT SURFACE MAY NOT BE TILTED OR DECENTERED'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(WQ.EQ.'LOCAL') ALENS(113,SURF)=0.0D0
          IF(WQ.EQ.'NORMAL') ALENS(113,SURF)=1.0D0
          RETURN
      END

C SUB SCW.FOR
      SUBROUTINE SCW
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE SCW WHICH IMPLEMENTS THE CW COMMAND
C       AT THE LENS OF UPDATE LENS LEVEL OR THE CW COMMAND AT
C       THE CMD LEVEL
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'

C
          IF(WC.EQ.'CW') THEN
              IF(F5.EQ.1.OR.F6.EQ.1) THEN
              ELSE
C       AT CMD
              END IF
C
              IF(F1.EQ.1) THEN
C
C               CHECK FOR PRESENCE OF ADDITIONAL INPUT AND
C               PRINT ERROR AND RETURN IF DISCOVERED.
C
                  IF(SQ.EQ.1.OR.SST.EQ.1.OR.SN.EQ.1) THEN
                      OUTLYNE='AT THE CMD LEVEL, "CW" TAKES NO EXPLICIT INPUT'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  WRITE(OUTLYNE,1000)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,2000) INT(SYSTEM1(11))
                  CALL SHOWIT(0)
                  RETURN
              ELSE
C               NOT AT CMD LEVEL
C
                  IF(F5.EQ.1.OR.F6.EQ.1) THEN
C
C               CHECK FOR PRESENCE OF QUALIFIER OR STRING INPUT
C               PRINT ERROR AND RETURN IF DISCOVERED.
C
                      IF(SQ.EQ.1.OR.SST.EQ.1.OR.S2.EQ.1.OR.S3.EQ.1
     1                .OR.S4.EQ.1.OR.S5.EQ.1) THEN
C
                          OUTLYNE='"CW" ONLY ACCEPTS NUMERIC WORD #1 INPUT'
                          CALL SHOWIT(1)
                          OUTLYNE='RE-ENTER COMMAND'
                          CALL SHOWIT(1)
                          CALL MACFAL
                          RETURN
                      END IF
C
                      IF(DF1.EQ.1) THEN
                          OUTLYNE='"CW" REQUIRES EXPLICIT NUMERIC WORD #1 INPUT'
                          CALL SHOWIT(1)
                          OUTLYNE='RE-ENTER COMMAND'
                          CALL SHOWIT(1)
                          CALL MACFAL
                          RETURN
                      END IF
C
C               WE ARE AT LENS INPUT OR LENS UPDATE LEVEL
C
                      IF(S1.EQ.0) THEN
                          WRITE(OUTLYNE,1000)
                          CALL SHOWIT(0)
                          WRITE(OUTLYNE,2000) INT(SYSTEM1(11))
                          CALL SHOWIT(0)
                          RETURN
                      ELSE

                          IF(W1.LT.1.0.OR.W1.GT.10.0D0)THEN
                              OUTLYNE='NUMERIC INPUT TO "CW" OUTSIDE ALLOWED BOUNDS'
                              CALL SHOWIT(1)
                              OUTLYNE='RE-ENTER COMMAND'
                              CALL SHOWIT(1)
                              CALL MACFAL
                              RETURN
                          ELSE
                              SYSTEM1(11)=W1
                              RETURN
                          END IF
                      END IF
                  END IF
              END IF
          END IF

          IF(WC.EQ.'PCW') THEN
              IF(F5.EQ.1.OR.F6.EQ.1) THEN
              ELSE
C       AT CMD
              END IF
              IF(F1.EQ.1) THEN
C
C               CHECK FOR ADDITIONAL INPUT AND
C               PRINT ERROR AND RETURN IF DISCOVERED.
C

                  IF(SN.EQ.1.OR.SQ.EQ.1.OR.SST.EQ.1) THEN
                      OUTLYNE='AT THE CMD LEVEL, "PCW" TAKES NO INPUT'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  WRITE(OUTLYNE,3000)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,4000) INT(SYSTEM1(7)),INT(SYSTEM1(8))
                  CALL SHOWIT(0)
                  RETURN
              ELSE
C               NOT AT CMD LEVEL
C
                  IF(F5.EQ.1.OR.F6.EQ.1) THEN
C
C               CHECK FOR PRESENCE OF QUALIFIER OR NUMERIC WORDS
C               PRINT ERROR AND RETURN IF DISCOVERED.
C
                      IF(SQ.EQ.1.OR.SST.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1
     1                .OR.S5.EQ.1)THEN
C
                          OUTLYNE=
     1                    '"PCW" ONLY ACCEPTS NUMERIC WORD #1 AND #2 INPUT'
                          CALL SHOWIT(1)
                          OUTLYNE='RE-ENTER COMMAND'
                          CALL SHOWIT(1)
                          CALL MACFAL
                          RETURN
                      END IF

                      IF(DF1.EQ.1.OR.DF2.EQ.1) THEN
                          OUTLYNE=
     1                    '"PCW" REQUIRES EXPLICIT NUMERIC WORD #1 AND #2 INPUT'
                          CALL SHOWIT(1)
                          OUTLYNE='RE-ENTER COMMAND'
                          CALL SHOWIT(1)
                          CALL MACFAL
                          RETURN
                      END IF
C
C               WE ARE AT LENS INPUT OR LENS UPDATE LEVEL
C
                      IF(S1.EQ.0.AND.S2.EQ.0) THEN
                          WRITE(OUTLYNE,3000)
                          CALL SHOWIT(0)
                          WRITE(OUTLYNE,4000) INT(SYSTEM1(7)),INT(SYSTEM1(8))
                          CALL SHOWIT(0)
                          RETURN
                      ELSE
                          IF(W1.LT.1.0.OR.W1.GT.10.0.OR.W2.LT.1.0.OR.W2.GT.10.0D0)THEN
                              OUTLYNE='NUMERIC INPUT TO "PCW" OUTSIDE ALLOWED BOUNDS'
                              CALL SHOWIT(1)
                              OUTLYNE='RE-ENTER COMMAND'
                              CALL SHOWIT(1)
                              CALL MACFAL
                              RETURN
                          ELSE
                              SYSTEM1(7)=W1
                              SYSTEM1(8)=W2
                              F22=1
                              RETURN
                          END IF
                      END IF
                  END IF
              END IF
          END IF
          IF(WC.EQ.'SCW') THEN
              IF(F5.EQ.1.OR.F6.EQ.1) THEN
              END IF
              IF(F1.EQ.1) THEN
C               CHECK FOR PRESENCE OF ADDITIONAL INPUT AND
C               PRINT ERROR AND RETURN IF DISCOVERED.
                  IF(SQ.EQ.1.OR.SST.EQ.1.OR.SN.EQ.1) THEN
                      OUTLYNE='AT THE CMD LEVEL, "SCW" TAKES NO INPUT'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  WRITE(OUTLYNE,5000)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,6000) INT(SYSTEM1(9)),INT(SYSTEM1(10))
                  CALL SHOWIT(0)
                  RETURN
              ELSE
C               NOT AT CMD LEVEL
C
                  IF(F5.EQ.1.OR.F6.EQ.1) THEN
C
C               CHECK FOR PRESENCE OF QUALIFIER OR NUMERIC WORDS
C               PRINT ERROR AND RETURN IF DISCOVERED.
C
                      IF(SQ.EQ.1.OR.SST.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1
     1                .OR.S5.EQ.1) THEN
C
                          OUTLYNE=
     1                    '"SCW" ONLY ACCEPTS NUMERIC WORD #1 AND #2 INPUT'
                          CALL SHOWIT(1)
                          OUTLYNE='RE-ENTER COMMAND'
                          CALL SHOWIT(1)
                          CALL MACFAL
                          RETURN
                      END IF
C
                      IF(DF1.EQ.1.OR.DF2.EQ.1) THEN
                          OUTLYNE=
     1                    '"SCW" REQUIRES EXPLICIT NUMERIC WORD #1 AND #2 INPUT'
                          CALL SHOWIT(1)
                          OUTLYNE='RE-ENTER COMMAND'
                          CALL SHOWIT(1)
                          CALL MACFAL
                          RETURN
                      END IF
C
C               WE ARE AT LENS INPUT OR LENS UPDATE LEVEL
C
                      IF(S1.EQ.0.AND.S2.EQ.0) THEN
                          WRITE(OUTLYNE,5000)
                          CALL SHOWIT(0)
                          WRITE(OUTLYNE,6000) INT(SYSTEM1(9)),INT(SYSTEM1(10))
                          CALL SHOWIT(0)
                          RETURN
                      ELSE
                          IF(W1.LT.1.0.OR.W1.GT.10.0.OR.W2.LT.1.0.OR.W2.GT.10.0D0)THEN
                              OUTLYNE=
     1                        'NUMERIC INPUT TO "SCW" OUTSIDE ALLOWED BOUNDS'
                              CALL SHOWIT(1)
                              OUTLYNE='RE-ENTER COMMAND'
                              CALL SHOWIT(1)
                              CALL MACFAL
                              RETURN
                          ELSE
                              SYSTEM1(9)=W1
                              SYSTEM1(10)=W2
                              F22=1
                              RETURN
                          END IF
                      END IF
                  END IF
              END IF
          END IF
C
 1000     FORMAT('CONTROL WAVELENGTH IS WAVLENGTH')
 2000     FORMAT(10X,I2)
 3000     FORMAT('PRIMARY WAVELENGTH PAIRS ARE WAVELENGTHS')
 4000     FORMAT(5X,I2,2X,'AND',2X,I2)
 5000     FORMAT('SECONDARY WAVELENGTH PAIRS ARE WAVELENGTHS')
 6000     FORMAT(5X,I2,2X,'AND',2X,I2)
          RETURN
      END


C SUB SCVR.FOR
      SUBROUTINE SCVR
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE SCVR WHICH IMPLEMENTS THE CVTOR AND RDTOR
C       COMMANDS AT THE UPDATE LENS LEVEL.
C       A YTORIC OR AN XTORIC COMMAND MUST BE ISSUED BEFORE TORIC DATA
C       CAN BE ENTERD FOR A SURFACE.
C
!        INTEGER I
C
          REAL*8 DR,NEWRAD,ARG1,RADIUS,APER,WAVE,WAVER,VAL1,RAD
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
C
          IF(SURF.EQ.0) THEN
              OUTLYNE='OBJECT SURFACE MAY NOT BE A TORIC'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
C               CHECK FOR PRESENCE OF QUALIFIER OR STRING INPUT
C               PRINT ERROR AND RETURN IF DISCOVERED.
          IF(WQ.NE.'DELTFR') THEN
              IF(S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
C
                  IF(WC.EQ.'CVTOR') THEN
                      OUTLYNE='"CVTOR" TAKES NO NUMERIC WORD #3 THROUGH #5 INPUT'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  IF(WC.EQ.'RDTOR') THEN
                      OUTLYNE='"RDTOR" TAKES NO NUMERIC WORD #3 THROUGH #5 INPUT'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  RETURN
              END IF
          ELSE
C DELTFR
              IF(S4.EQ.1.OR.S5.EQ.1) THEN
C
                  IF(WC.EQ.'CVTOR') THEN
                      OUTLYNE='"CVTOR DELTFR" TAKES NO NUMERIC WORD #4 OR #5 INPUT'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  IF(WC.EQ.'RDTOR') THEN
                      OUTLYNE='"RDTOR DELTFR" TAKES NO NUMERIC WORD #4 OR #5 INPUT'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  RETURN
              END IF
              IF(W2.LE.0.0D0.AND.DF2.EQ.0.OR.W3.LE.0.0D0
     1        .AND.DF3.EQ.0) THEN
C
                  IF(WC.EQ.'CVTOR') THEN
                      OUTLYNE=
     1                '"CVTOR DELTFR" REQUIRES POSITIVE NUMERIC WORD #2 AND #3 INPUT'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  IF(WC.EQ.'RDTOR') THEN
                      OUTLYNE=
     1                '"RDTOR DELTFR" REQUIRES POSITIVE NUMERIC WORD #2 AND #3 INPUT'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  RETURN
              END IF
          END IF
          IF(SST.EQ.1) THEN
C
              IF(WC.EQ.'CVTOR') THEN
                  OUTLYNE='"CVTOR" TAKES NO STRING INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(WC.EQ.'RDTOR') THEN
                  OUTLYNE='"RDTOR" TAKES NO STRING INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              RETURN
          END IF
          IF(SQ.EQ.1.AND.F5.EQ.1) THEN
              OUTLYNE=
     1        '"'//WC(1:5)//
     1        '" TAKES NO QUALIFIER INPUT DURING LENS INPUT MODE'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(F6.EQ.1) THEN
              IF(WQ.NE.'        '.AND.WQ.NE.'DELT'.AND.WQ.NE.'CENT'.AND.
     1        WQ.NE.'DELTFR') THEN
                  OUTLYNE=
     1            'INVALID QUALIFIER USED WITH "'//WC(1:5)//'"'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
          IF(DF1.EQ.1) THEN
              OUTLYNE=
     1        '"'//WC(1:5)//'" REQUIRES EXPLICIT NUMERIC WORD #1 INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF

          IF(WC.EQ.'RDTOR'.OR.WC.EQ.'CVTOR') THEN
C
C       IF THE SURFACE WAS NOT A TORIC THEN SAY SO AND RETURN
C
              IF(ALENS(23,SURF).EQ.0.0D0) THEN
                  OUTLYNE='A "YTORIC" OR "XTORIC" COMMAND MUST BE ENTERED'
                  CALL SHOWIT(1)
                  OUTLYNE='PRIOR TO TORIC DATA BEING ENTERED'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C       ALENS(23,SURF) NOT ZERO, MUST BE 1.0 (Y-TORIC)
C       OR 2.0 (X-TORIC).
C
              IF(WC.EQ.'CVTOR') THEN
                  IF(SQ.EQ.0) ALENS(24,SURF)=W1
                  IF(WQ.EQ.'DELT') ALENS(24,SURF)=ALENS(24,SURF)+W1
                  IF(WQ.EQ.'CENT')
     1            ALENS(24,SURF)=ALENS(24,SURF)+(W1*0.01D0*ALENS(24,SURF))
                  IF(WQ.EQ.'DELTFR') THEN
                      IF(DF2.EQ.0) WAVER=W2
                      IF(DF2.EQ.1) WAVER=0.5461D0
                      IF(DF3.EQ.0) APER=W3
C
                      IF(DF3.EQ.1) THEN
                          IF(ALENS(9,SURF).EQ.0.0D0.OR.ALENS(126,SURF).NE.0.0D0) THEN
C     USE LAST PARAXIAL DATA
                              APER=2.0D0*(DABS(PXTRAY(1,SURF))+DABS(PXTRAY(5,SURF)))
                          ELSE
                              IF(ALENS(9,SURF).EQ.1.0D0) THEN
C     USE MIN CLAP DATA WITHOUT DEC OR TILTS
                                  IF(ALENS(10,SURF).GT.ALENS(11,SURF))
     1                            APER=2.0D0*DABS(ALENS(11,SURF))
                                  IF(ALENS(10,SURF).LE.ALENS(11,SURF))
     1                            APER=2.0D0*DABS(ALENS(10,SURF))
                              END IF
                              IF(ALENS(9,SURF).GE.2.0D0.AND.ALENS(9,SURF).LE.4.0D0) THEN
C     USE MAX CLAP DATA WITHOUT DEC OR TILTS
                                  IF(ALENS(10,SURF).GT.ALENS(11,SURF))
     1                            APER=2.0D0*DABS(ALENS(10,SURF))
                                  IF(ALENS(10,SURF).LE.ALENS(11,SURF))
     1                            APER=2.0D0*DABS(ALENS(11,SURF))
                              END IF
                          END IF
                      END IF
C
C       APER=PART DIAMETER IN CURRENT LENS UNITS (REQUIRED)
C       WAVER=WAVELENGTH IN MICRONS (DEFAULT = 0.5461)
                      IF(SYSTEM1(6).EQ.1.0D0) WAVE=(WAVER*1.0D-3)/(25.4D0)
                      IF(SYSTEM1(6).EQ.2.0D0) WAVE=WAVER*1.0D-4
                      IF(SYSTEM1(6).EQ.3.0D0) WAVE=WAVER*1.0D-3
                      IF(SYSTEM1(6).EQ.4.0D0) WAVE=WAVER*1.0D-6
C
                      IF(ALENS(24,SURF).NE.0.0D0) THEN
C     CURVED SURFACE
                          RAD=1.0D0/ALENS(24,SURF)
                          RADIUS=DABS(1.0D0/ALENS(24,SURF))
                          ARG1=(RADIUS**2)-((APER/2.0D0)**2)
                          IF(ARG1.LT.0.0D0) THEN
                              OUTLYNE='RADIUS OR CURVATURE CHANGE CAN NOT BE DETERMINED'
                              CALL SHOWIT(1)
                              OUTLYNE='RE-ENTER COMMAND'
                              CALL SHOWIT(1)
                              CALL MACFAL
                              RETURN
                          END IF
                          IF((DSQRT(ARG1)-RADIUS).EQ.0.0D0) DR=0.0D0
                          IF((DSQRT(ARG1)-RADIUS).NE.0.0D0)
     1                    DR=W1*(DABS((WAVE*DSQRT(ARG1))
     1                    /(2.0D0*(DSQRT(ARG1)-RADIUS))))
                          IF(DR.EQ.0.0D0) ALENS(24,SURF)=ALENS(24,SURF)
                          IF(DR.NE.0.0D0) THEN
                              NEWRAD=RAD+DR
                              IF(NEWRAD.EQ.0.0D0) ALENS(24,SURF)=0.0D0
                              IF(NEWRAD.NE.0.0D0) ALENS(24,SURF)=1.0D0/NEWRAD
                          END IF
                      ELSE
C     SURFACE WAS FLAT
                          NEWRAD=((1.0D0/(W1*WAVE))*((APER/2.0D0)**2))
     1                    +(W1*WAVE/4.0D0)
                          IF(NEWRAD.EQ.0.0D0) ALENS(24,SURF)=0.0D0
                          IF(NEWRAD.NE.0.0D0) ALENS(24,SURF)=1.0D0/NEWRAD
C
                      END IF
C     DONE WITH DELTFR
C
                  END IF
                  RETURN
              ELSE
              END IF
C     WC MUST BE RDTOR
              IF(WQ.EQ.'DELTFR') THEN
                  IF(DF2.EQ.0) WAVER=W2
                  IF(DF2.EQ.1) WAVER=0.5461D0
                  IF(DF3.EQ.0) APER=W3
                  IF(DF3.EQ.1) THEN
                      IF(ALENS(9,SURF).EQ.0.0D0.OR.ALENS(126,SURF).NE.0.0D0) THEN
C     USE LAST PARAXIAL DATA
                          APER=2.0D0*(DABS(PXTRAY(1,SURF))+DABS(PXTRAY(5,SURF)))
                      ELSE
                          IF(ALENS(9,SURF).EQ.1.0D0) THEN
C     USE MIN CLAP DATA WITHOUT DEC OR TILTS
                              IF(ALENS(10,SURF).GT.ALENS(11,SURF))
     1                        APER=2.0D0*DABS(ALENS(11,SURF))
                              IF(ALENS(10,SURF).LE.ALENS(11,SURF))
     1                        APER=2.0D0*DABS(ALENS(10,SURF))
                          END IF
                          IF(ALENS(9,SURF).GE.2.0D0.AND.ALENS(9,SURF).LE.4.0D0) THEN
C     USE MAX CLAP DATA WITHOUT DEC OR TILTS
                              IF(ALENS(10,SURF).GT.ALENS(11,SURF))
     1                        APER=2.0D0*DABS(ALENS(10,SURF))
                              IF(ALENS(10,SURF).LE.ALENS(11,SURF))
     1                        APER=2.0D0*DABS(ALENS(11,SURF))
                          END IF
                      END IF
                  END IF
C
C       APER=PART DIAMETER IN CURRENT LENS UNITS (REQUIRED)
C       WAVER=WAVELENGTH IN MICRONS (DEFAULT = 0.5461)
                  IF(SYSTEM1(6).EQ.1.0D0) WAVE=(WAVER*1.0D-3)/(25.4D0)
                  IF(SYSTEM1(6).EQ.2.0D0) WAVE=WAVER*1.0D-4
                  IF(SYSTEM1(6).EQ.3.0D0) WAVE=WAVER*1.0D-3
                  IF(SYSTEM1(6).EQ.4.0D0) WAVE=WAVER*1.0D-6
                  IF(ALENS(24,SURF).NE.0.0D0) RADIUS=DABS(1.0D0/ALENS(24,SURF))
                  IF(ALENS(24,SURF).NE.0.0D0) RAD=1.0D0/ALENS(24,SURF)
                  IF(ALENS(24,SURF).EQ.0.0D0) RADIUS=0.0D0
C
                  IF(RADIUS.NE.0.0D0) THEN
C     CURVED SURFACE
                      ARG1=(RADIUS**2)-((APER/2.0D0)**2)
                      IF(ARG1.LT.0.0D0) THEN
                          OUTLYNE='RADIUS OR CURVATURE CHANGE CAN NOT BE DETERMINED'
                          CALL SHOWIT(1)
                          OUTLYNE='RE-ENTER COMMAND'
                          CALL SHOWIT(1)
                          CALL MACFAL
                          RETURN
                      END IF
                      IF((DSQRT(ARG1)-RADIUS).EQ.0.0D0) DR=0.0D0
                      IF((DSQRT(ARG1)-RADIUS).NE.0.0D0)
     1                DR=W1*(DABS((WAVE*DSQRT(ARG1))
     1                /(2.0D0*(DSQRT(ARG1)-RADIUS))))
                      IF(DR.EQ.0.0D0) ALENS(24,SURF)=ALENS(24,SURF)
                      IF(DR.NE.0.0D0) THEN
                          NEWRAD=RAD+DR
                          IF(NEWRAD.EQ.0.0D0) ALENS(24,SURF)=0.0D0
                          IF(NEWRAD.NE.0.0D0) ALENS(24,SURF)=1.0D0/NEWRAD
                      END IF
                  ELSE
C     SURFACE WAS FLAT
                      NEWRAD=((1.0D0/(W1*WAVE))*((APER/2.0D0)**2))
     1                +(W1*WAVE/4.0D0)
                      IF(NEWRAD.EQ.0.0D0) ALENS(24,SURF)=0.0D0
                      IF(NEWRAD.NE.0.0D0) ALENS(24,SURF)=1.0D0/NEWRAD
C
                  END IF
C     DONE WITH DELTFR
              END IF
              IF(W1.EQ.0.0D0) THEN
                  IF(SQ.EQ.0) ALENS(24,SURF)=0.0D0
                  IF(WQ.EQ.'DELT') ALENS(24,SURF)=ALENS(24,SURF)
                  IF(WQ.EQ.'CENT') ALENS(24,SURF)=ALENS(24,SURF)
              ELSE
                  IF(SQ.EQ.0) ALENS(24,SURF)=1.0D0/(W1)
                  IF(WQ.EQ.'DELT') THEN
                      IF(ALENS(24,SURF).EQ.0.0D0) VAL1=0.0D0
                      IF(ALENS(24,SURF).NE.0.0D0) VAL1=1.0D0/ALENS(24,SURF)
                      VAL1=VAL1+W1
                      IF(VAL1.EQ.0.0D0) ALENS(24,SURF)=0.0D0
                      IF(VAL1.NE.0.0D0) ALENS(24,SURF)=1.0D0/VAL1
                  END IF
                  IF(WQ.EQ.'CENT') THEN
                      IF(ALENS(24,SURF).EQ.0.0D0) VAL1=0.0D0
                      IF(ALENS(24,SURF).NE.0.0D0) VAL1=1.0D0/ALENS(24,SURF)
                      VAL1=VAL1+(W1*0.01D0*VAL1)
                      IF(VAL1.EQ.0.0D0) ALENS(24,SURF)=0.0D0
                      IF(VAL1.NE.0.0D0) ALENS(24,SURF)=1.0D0/VAL1
                  END IF
              END IF
C
C       IF AN RDTOR OR CVTOR PIKUP EXISTS, GET RID OF IT
C
              IF(PIKUP(1,SURF,9).EQ.1.0D0) THEN
                  ALENS(32,SURF)=ALENS(32,SURF)-1.0D0
                  WRITE(OUTLYNE,*)'SURFACE',SURF,' :(CVTOR) PIKUP DELETED'
                  CALL SHOWIT(1)
                  PIKUP(1:6,SURF,9)=0.0D0
              END IF
              IF(PIKUP(1,SURF,10).EQ.1.0D0) THEN
                  ALENS(32,SURF)=ALENS(32,SURF)-1.0D0
                  WRITE(OUTLYNE,*)'SURFACE',SURF,' :(RDTOR) PIKUP DELETED'
                  CALL SHOWIT(1)
                  PIKUP(1:6,SURF,10)=0.0D0
              END IF
C       NOW CHECK FOR CURVATURE SOLVES AND DELETE THE APPROPRIATE
C       ONES. IF THE SURFACE IS A Y-TORIC, DELETE XZ CURVATURE
C       SOLVES. IF THE SURFACE IS AN X-TORIC DELETE YZ CURVATURE SOLVES
C
              IF(ALENS(23,SURF).EQ.1.0D0) THEN
C       Y-TORIC, ARE THERE XZ CURVATURE SOLVES
                  IF(SOLVE(2,SURF).GT.0.0D0) THEN
C       SOLVE TO DELETE IS FOUND
                      SOLVE(2,SURF)=0.0D0
                      SOLVE(1,SURF)=0.0D0
                      WRITE(OUTLYNE,*)
     1                'SURFACE',SURF,' :XZ PLANE CURVATURE SOLVE DELETED'
                      CALL SHOWIT(1)
                  END IF
              END IF
              IF(ALENS(23,SURF).EQ.2.0D0) THEN
C       X-TORIC, ARE THERE YZ CURVATURE SOLVES
                  IF(SOLVE(8,SURF).GT.0.0D0) THEN
C       SOLVE TO DELETE IS FOUND
                      SOLVE(8,SURF)=0.0D0
                      SOLVE(9,SURF)=0.0D0
                      WRITE(OUTLYNE,*)
     1                'SURFACE',SURF,' :YZ PLANE CURVATURE SOLVE DELETED'
                      CALL SHOWIT(1)
                  END IF
              END IF
C       NOW RE-CALCULATE ALENS(33,SURF) THE SOLVE COUNTER
              ALENS(33,SURF)=0.0D0
              IF(SOLVE(6,SURF).GT.0.0D0) ALENS(33,SURF)=ALENS(33,SURF)+1.0D0
              IF(SOLVE(4,SURF).GT.0.0D0) ALENS(33,SURF)=ALENS(33,SURF)+0.1D0
              IF(SOLVE(8,SURF).GT.0.0D0) ALENS(33,SURF)=ALENS(33,SURF)+2.0D0
              IF(SOLVE(2,SURF).GT.0.0D0) ALENS(33,SURF)=ALENS(33,SURF)+0.2D0
          ELSE
C       NOT RDTOR OR CVTOR
          END IF
          RETURN
      END
C SUB SCV.FOR
      SUBROUTINE SCV
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE SCV WHICH IMPLEMENTS THE CV AND RD
C       COMMANDS AT THE LENS OF UPDATE LENS LEVEL.
C       THE SECOND NUMERIC WORD IS THE CONIC CONSTANT
C       ASSOCIATED WITH THIS SURFACE.
C
          INTEGER PIKCNT,I
C
          REAL*8 VAL1,WAVE,WAVER,APER,RADIUS,DR,RAD
     1    ,NEWRAD,ARG1
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
C
C               CHECK FOR PRESENCE OF QUALIFIER OR STRING INPUT
C               PRINT ERROR AND RETURN IF DISCOVERED.
          IF(WQ.NE.'DELTFR'.AND.WQ.NE.'SAG') THEN
              IF(S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
C
                  IF(WC.EQ.'CV') THEN
                      OUTLYNE='"CV" TAKES NO NUMERIC WORD #3 THROUGH #5 INPUT'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  IF(WC.EQ.'RD') THEN
                      OUTLYNE='"RD" TAKES NO NUMERIC WORD #3 THROUGH #5 INPUT'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  RETURN
              END IF
          ELSE
C DELTFR OR SAG
              IF(WQ.EQ.'DELTFR') THEN
                  IF(S4.EQ.1.OR.S5.EQ.1) THEN
C
                      IF(WC.EQ.'CV') THEN
                          OUTLYNE='"CV DELTFR" TAKES NO NUMERIC WORD #4 OR #5 INPUT'
                          CALL SHOWIT(1)
                          OUTLYNE='RE-ENTER COMMAND'
                          CALL SHOWIT(1)
                          CALL MACFAL
                          RETURN
                      END IF
                      IF(WC.EQ.'RD') THEN
                          OUTLYNE='"RD DELTFR" TAKES NO NUMERIC WORD #4 OR #5 INPUT'
                          CALL SHOWIT(1)
                          OUTLYNE='RE-ENTER COMMAND'
                          CALL SHOWIT(1)
                          CALL MACFAL
                          RETURN
                      END IF
                      RETURN
                  END IF
                  IF(W2.LE.0.0D0.AND.DF2.EQ.0.OR.W3.LE.0.0D0
     1            .AND.DF3.EQ.0) THEN
C
                      IF(WC.EQ.'CV') THEN
                          OUTLYNE=
     1                    '"CV DELTFR" REQUIRES POSITIVE NUMERIC WORD #2 AND #3 INPUT'
                          CALL SHOWIT(1)
                          OUTLYNE='RE-ENTER COMMAND'
                          CALL SHOWIT(1)
                          CALL MACFAL
                          RETURN
                      END IF
                      IF(WC.EQ.'RD') THEN
                          OUTLYNE=
     1                    '"RD DELTFR" REQUIRES POSITIVE NUMERIC WORD #2 AND #3 INPUT'
                          CALL SHOWIT(1)
                          OUTLYNE='RE-ENTER COMMAND'
                          CALL SHOWIT(1)
                          CALL MACFAL
                          RETURN
                      END IF
                      RETURN
                  END IF
              END IF
              IF(WQ.EQ.'SAG') THEN
                  IF(S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
C
                      IF(WC.EQ.'CV') THEN
                          OUTLYNE='"CV SAG" TAKES NO NUMERIC WORD #3, #4 OR #5 INPUT'
                          CALL SHOWIT(1)
                          OUTLYNE='RE-ENTER COMMAND'
                          CALL SHOWIT(1)
                          CALL MACFAL
                          RETURN
                      END IF
                      IF(WC.EQ.'RD') THEN
                          OUTLYNE='"RD SAG" TAKES NO NUMERIC WORD #3, #4 OR #5 INPUT'
                          CALL SHOWIT(1)
                          OUTLYNE='RE-ENTER COMMAND'
                          CALL SHOWIT(1)
                          CALL MACFAL
                          RETURN
                      END IF
                      RETURN
                  END IF
                  IF(W2.LE.0.0D0
     1            .AND.DF2.EQ.0) THEN
C
                      IF(WC.EQ.'CV') THEN
                          OUTLYNE=
     1                    '"CV SAG" REQUIRES POSITIVE NUMERIC WORD #2 INPUT'
                          CALL SHOWIT(1)
                          OUTLYNE='RE-ENTER COMMAND'
                          CALL SHOWIT(1)
                          CALL MACFAL
                          RETURN
                      END IF
                      IF(WC.EQ.'RD') THEN
                          OUTLYNE=
     1                    '"RD SAG" REQUIRES POSITIVE NUMERIC WORD #2 INPUT'
                          CALL SHOWIT(1)
                          OUTLYNE='RE-ENTER COMMAND'
                          CALL SHOWIT(1)
                          CALL MACFAL
                          RETURN
                      END IF
                      RETURN
                  END IF
              END IF
C     NOT DELTFR OR SAG
          END IF
          IF(SST.EQ.1) THEN
C
              IF(WC.EQ.'CV') THEN
                  OUTLYNE='"CV" TAKES NO STRING INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(WC.EQ.'RD') THEN
                  OUTLYNE='"RD" TAKES NO STRING INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              RETURN
          END IF
          IF(F6.EQ.1) THEN
              IF(WQ.NE.'        '.AND.WQ.NE.'CENT'.AND.WQ.NE.'DELT'
     1        .AND.WQ.NE.'DELTFR'.AND.WQ.NE.'SAG') THEN
C
                  IF(WC.EQ.'CV') THEN
                      OUTLYNE='INVALID QUALIFIER USED WITH "CV"'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  IF(WC.EQ.'RD') THEN
                      OUTLYNE='INVALID QUALIFIER USED WITH "RD"'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  RETURN
              END IF
          END IF
          IF(F5.EQ.1) THEN
              IF(SQ.EQ.1) THEN
C
                  IF(WC.EQ.'CV') THEN
                      OUTLYNE='"CV" TAKES NO QUALIFIER WORDS IN LENS INPUT MODE'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  IF(WC.EQ.'RD') THEN
                      OUTLYNE='"RD" TAKES NO QUALIFIER WORDS IN LENS INPUT MODE'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  RETURN
              END IF
          END IF
          IF(DF1.EQ.1) THEN
C
              IF(WC.EQ.'CV') THEN
                  OUTLYNE='"CV" REQUIRES EXPLICIT NUMERIC WORD #1 INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(WC.EQ.'RD') THEN
                  OUTLYNE='"RD" REQUIRES EXPLICIT NUMERIC WORD #1 INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              RETURN
          END IF
C
          IF(WC.EQ.'RD'.AND.SQ.EQ.0.AND.DF2.EQ.0) ALENS(2,SURF)=W2
C
          IF(WC.EQ.'CV') THEN
              IF(SQ.EQ.0) ALENS(1,SURF)=W1
              IF(SQ.EQ.0.AND.DF2.EQ.0) ALENS(2,SURF)=W2
              IF(WQ.EQ.'DELTFR') THEN
                  IF(DF2.EQ.0) WAVER=W2
                  IF(DF2.EQ.1) WAVER=0.5461D0
                  IF(DF3.EQ.0) APER=2.0D0*W3
C
                  IF(DF3.EQ.1) THEN
                      IF(ALENS(9,SURF).EQ.0.0D0.OR.ALENS(127,SURF).NE.0.0D0) THEN
C     USE LAST PARAXIAL DATA
                          APER=2.0D0*(DABS(PXTRAY(1,SURF))+DABS(PXTRAY(5,SURF)))
                      ELSE
                          IF(ALENS(9,SURF).EQ.1.0D0) THEN
C     USE MIN CLAP DATA WITHOUT DEC OR TILTS
                              IF(ALENS(10,SURF).GT.ALENS(11,SURF))
     1                        APER=2.0D0*DABS(ALENS(11,SURF))
                              IF(ALENS(10,SURF).LE.ALENS(11,SURF))
     1                        APER=2.0D0*DABS(ALENS(10,SURF))
                          END IF
                          IF(ALENS(9,SURF).GE.2.0D0.AND.ALENS(9,SURF).LE.4.0D0) THEN
C     USE MAX CLAP DATA WITHOUT DEC OR TILTS
                              IF(ALENS(10,SURF).GT.ALENS(11,SURF))
     1                        APER=2.0D0*DABS(ALENS(10,SURF))
                              IF(ALENS(10,SURF).LE.ALENS(11,SURF))
     1                        APER=2.0D0*DABS(ALENS(11,SURF))
                          END IF
                      END IF
                  END IF
C
C       APER=PART DIAMETER IN CURRENT LENS UNITS (REQUIRED)
C       WAVER=WAVELENGTH IN MICRONS (DEFAULT = 0.5461)
                  IF(SYSTEM1(6).EQ.1.0D0) WAVE=(WAVER*1.0D-3)/(25.4D0)
                  IF(SYSTEM1(6).EQ.2.0D0) WAVE=WAVER*1.0D-4
                  IF(SYSTEM1(6).EQ.3.0D0) WAVE=WAVER*1.0D-3
                  IF(SYSTEM1(6).EQ.4.0D0) WAVE=WAVER*1.0D-6
C
                  IF(ALENS(1,SURF).NE.0.0D0) THEN
C     CURVED SURFACE
                      RADIUS=DABS(1.0D0/ALENS(1,SURF))
                      RAD=1.0D0/ALENS(1,SURF)
                      ARG1=(RADIUS**2)-((APER/2.0D0)**2)
                      IF(ARG1.LT.0.0D0) THEN
                          OUTLYNE='RADIUS OR CURVATURE CHANGE CAN NOT BE DETERMINED'
                          CALL SHOWIT(1)
                          OUTLYNE='RE-ENTER COMMAND'
                          CALL SHOWIT(1)
                          CALL MACFAL
                          RETURN
                      END IF
                      IF((DSQRT(ARG1)-RADIUS).EQ.0.0D0) DR=0.0D0
                      IF((DSQRT(ARG1)-RADIUS).NE.0.0D0)
     1                DR=W1*(DABS((WAVE*DSQRT(ARG1))
     1                /(2.0D0*(DSQRT(ARG1)-RADIUS))))
                      IF(DR.EQ.0.0D0) ALENS(1,SURF)=ALENS(1,SURF)
                      IF(DR.NE.0.0D0) THEN
                          NEWRAD=RAD+DR
                          IF(NEWRAD.EQ.0.0D0) ALENS(1,SURF)=0.0D0
                          IF(NEWRAD.NE.0.0D0) ALENS(1,SURF)=1.0D0/NEWRAD
                      END IF
                  ELSE
C     SURFACE WAS FLAT
                      NEWRAD=((1.0D0/(W1*WAVE))*((APER/2.0D0)**2))
     1                +(W1*WAVE/4.0D0)
                      IF(NEWRAD.EQ.0.0D0) ALENS(1,SURF)=0.0D0
                      IF(NEWRAD.NE.0.0D0) ALENS(1,SURF)=1.0D0/NEWRAD
C
                  END IF
C     DONE WITH DELTFR
C
              END IF
              IF(WQ.EQ.'SAG') THEN
                  IF(DF2.EQ.0) APER=2.0D0*W2
C
                  IF(DF2.EQ.1) THEN
                      IF(ALENS(9,SURF).EQ.0.0D0.AND.ALENS(127,SURF).NE.0.0D0) THEN
C     USE LAST PARAXIAL DATA
                          APER=2.0D0*(DABS(PXTRAY(1,SURF))+DABS(PXTRAY(5,SURF)))
                      ELSE
                          IF(ALENS(9,SURF).EQ.1.0D0) THEN
C     USE MIN CLAP DATA WITHOUT DEC OR TILTS
                              IF(ALENS(10,SURF).GT.ALENS(11,SURF))
     1                        APER=2.0D0*DABS(ALENS(11,SURF))
                              IF(ALENS(10,SURF).LE.ALENS(11,SURF))
     1                        APER=2.0D0*DABS(ALENS(10,SURF))
                          END IF
                          IF(ALENS(9,SURF).GE.2.0D0.AND.ALENS(9,SURF).LE.4.0D0) THEN
C     USE MAX CLAP DATA WITHOUT DEC OR TILTS
                              IF(ALENS(10,SURF).GT.ALENS(11,SURF))
     1                        APER=2.0D0*DABS(ALENS(10,SURF))
                              IF(ALENS(10,SURF).LE.ALENS(11,SURF))
     1                        APER=2.0D0*DABS(ALENS(11,SURF))
                          END IF
                      END IF
                  END IF
C
C       APER=PART DIAMETER IN CURRENT LENS UNITS (REQUIRED)
                  ARG1=(((W1**2)*(ALENS(2,SURF)+1.0D0))+((APER/2.0D0)**2))
                  IF(ARG1.LT.0.0D0) THEN
                      OUTLYNE='RADIUS OR CURVATURE CHANGE CAN NOT BE DETERMINED'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  ALENS(1,SURF)=(2.0D0*W1)/
     1            (((W1**2)*(ALENS(2,SURF)+1.0D0))+((APER/2.0D0)**2))
C     DONE WITH SAG
C
              END IF
              IF(WQ.EQ.'DELT') ALENS(1,SURF)=ALENS(1,SURF)+W1
              IF(WQ.EQ.'CENT')
     1        ALENS(1,SURF)=ALENS(1,SURF)+(W1*0.01D0*ALENS(1,SURF))
          ELSE
          END IF
C     WC MUST BE RD
          IF(WC.EQ.'RD') THEN
              IF(WQ.EQ.'DELTFR') THEN
                  IF(DF2.EQ.0) WAVER=W2
                  IF(DF2.EQ.1) WAVER=0.5461D0
                  IF(DF3.EQ.0) APER=2.0D0*W3
                  IF(DF3.EQ.1) THEN
                      IF(ALENS(9,SURF).EQ.0.0D0.AND.ALENS(127,SURF).NE.0.0D0) THEN
C     USE LAST PARAXIAL DATA
                          APER=2.0D0*(DABS(PXTRAY(1,SURF))+DABS(PXTRAY(5,SURF)))
                      ELSE
                          IF(ALENS(9,SURF).EQ.1.0D0) THEN
C     USE MIN CLAP DATA WITHOUT DEC OR TILTS
                              IF(ALENS(10,SURF).GT.ALENS(11,SURF))
     1                        APER=2.0D0*DABS(ALENS(11,SURF))
                              IF(ALENS(10,SURF).LE.ALENS(11,SURF))
     1                        APER=2.0D0*DABS(ALENS(10,SURF))
                          END IF
                          IF(ALENS(9,SURF).GE.2.0D0.AND.ALENS(9,SURF).LE.4.0D0) THEN
C     USE MAX CLAP DATA WITHOUT DEC OR TILTS
                              IF(ALENS(10,SURF).GT.ALENS(11,SURF))
     1                        APER=2.0D0*DABS(ALENS(10,SURF))
                              IF(ALENS(10,SURF).LE.ALENS(11,SURF))
     1                        APER=2.0D0*DABS(ALENS(11,SURF))
                          END IF
                      END IF
                  END IF
C
C       APER=PART DIAMETER IN CURRENT LENS UNITS (REQUIRED)
C       WAVER=WAVELENGTH IN MICRONS (DEFAULT = 0.5461)
                  IF(SYSTEM1(6).EQ.1.0D0) WAVE=(WAVER*1.0D-3)/(25.4D0)
                  IF(SYSTEM1(6).EQ.2.0D0) WAVE=WAVER*1.0D-4
                  IF(SYSTEM1(6).EQ.3.0D0) WAVE=WAVER*1.0D-3
                  IF(SYSTEM1(6).EQ.4.0D0) WAVE=WAVER*1.0D-6
                  IF(ALENS(1,SURF).NE.0.0D0) RADIUS=DABS(1.0D0/ALENS(1,SURF))
                  IF(ALENS(1,SURF).NE.0.0D0) RAD=1.0D0/ALENS(1,SURF)
                  IF(ALENS(1,SURF).EQ.0.0D0) RADIUS=0.0D0
C
                  IF(RADIUS.NE.0.0D0) THEN
C     CURVED SURFACE
                      ARG1=(RADIUS**2)-((APER/2.0D0)**2)
                      IF(ARG1.LT.0.0D0) THEN
                          OUTLYNE='RADIUS OR CURVATURE CHANGE CAN NOT BE DETERMINED'
                          CALL SHOWIT(1)
                          OUTLYNE='RE-ENTER COMMAND'
                          CALL SHOWIT(1)
                          CALL MACFAL
                          RETURN
                      END IF
                      IF((DSQRT(ARG1)-RADIUS).EQ.0.0D0) DR=0.0D0
                      IF((DSQRT(ARG1)-RADIUS).NE.0.0D0)
     1                DR=W1*(DABS((WAVE*DSQRT(ARG1))
     1                /(2.0D0*(DSQRT(ARG1)-RADIUS))))
                      IF(DR.EQ.0.0D0) ALENS(1,SURF)=ALENS(1,SURF)
                      IF(DR.NE.0.0D0) THEN
                          NEWRAD=RAD+DR
                          IF(NEWRAD.EQ.0.0D0) ALENS(1,SURF)=0.0D0
                          IF(NEWRAD.NE.0.0D0) ALENS(1,SURF)=1.0D0/NEWRAD
                      END IF
                  ELSE
C     SURFACE WAS FLAT
                      NEWRAD=((1.0D0/(W1*WAVE))*((APER/2.0D0)**2))
     1                +(W1*WAVE/4.0D0)
                      IF(NEWRAD.EQ.0.0D0) ALENS(1,SURF)=0.0D0
                      IF(NEWRAD.NE.0.0D0) ALENS(1,SURF)=1.0D0/NEWRAD
C
                  END IF
C     DONE WITH DELTFR
              END IF
              IF(WQ.EQ.'SAG') THEN
                  IF(DF2.EQ.0) APER=2.0D0*W2
                  IF(DF2.EQ.1) THEN
                      IF(ALENS(9,SURF).EQ.0.0D0.OR.ALENS(127,SURF).NE.0.0D0) THEN
C     USE LAST PARAXIAL DATA
                          APER=2.0D0*(DABS(PXTRAY(1,SURF))+DABS(PXTRAY(5,SURF)))
                      ELSE
                          IF(ALENS(9,SURF).EQ.1.0D0) THEN
C     USE MIN CLAP DATA WITHOUT DEC OR TILTS
                              IF(ALENS(10,SURF).GT.ALENS(11,SURF))
     1                        APER=2.0D0*DABS(ALENS(11,SURF))
                              IF(ALENS(10,SURF).LE.ALENS(11,SURF))
     1                        APER=2.0D0*DABS(ALENS(10,SURF))
                          END IF
                          IF(ALENS(9,SURF).GE.2.0D0.AND.ALENS(9,SURF).LE.4.0D0) THEN
C     USE MAX CLAP DATA WITHOUT DEC OR TILTS
                              IF(ALENS(10,SURF).GT.ALENS(11,SURF))
     1                        APER=2.0D0*DABS(ALENS(10,SURF))
                              IF(ALENS(10,SURF).LE.ALENS(11,SURF))
     1                        APER=2.0D0*DABS(ALENS(11,SURF))
                          END IF
                      END IF
                  END IF
C
C       APER=PART DIAMETER IN CURRENT LENS UNITS (REQUIRED)
C
C       APER=PART DIAMETER IN CURRENT LENS UNITS (REQUIRED)
                  ARG1=(((W1**2)*(ALENS(2,SURF)+1.0D0))+((APER/2.0D0)**2))
                  IF(ARG1.LT.0.0D0) THEN
                      OUTLYNE='RADIUS OR CURVATURE CHANGE CAN NOT BE DETERMINED'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  ALENS(1,SURF)=(2.0D0*W1)/
     1            (((W1**2)*(ALENS(2,SURF)+1.0D0))+((APER/2.0D0)**2))
C     DONE WITH SAG
C
              END IF
              IF(W1.EQ.0.0D0) THEN
                  IF(SQ.EQ.0) ALENS(1,SURF)=0.0D0
                  IF(WQ.EQ.'DELT') ALENS(1,SURF)=ALENS(1,SURF)
                  IF(WQ.EQ.'CENT') ALENS(1,SURF)=ALENS(1,SURF)
              ELSE
                  IF(SQ.EQ.0) ALENS(1,SURF)=1.0D0/(W1)
                  IF(WQ.EQ.'DELT') THEN
                      IF(ALENS(1,SURF).EQ.0.0D0) VAL1=0.0D0
                      IF(ALENS(1,SURF).NE.0.0D0) VAL1=1.0D0/ALENS(1,SURF)
                      VAL1=VAL1+W1
                      IF(VAL1.EQ.0.0D0) ALENS(1,SURF)=0.0D0
                      IF(VAL1.NE.0.0D0) ALENS(1,SURF)=1.0D0/VAL1
                  END IF
                  IF(WQ.EQ.'CENT') THEN
                      IF(ALENS(1,SURF).EQ.0.0D0) VAL1=0.0D0
                      IF(ALENS(1,SURF).NE.0.0D0) VAL1=1.0D0/ALENS(1,SURF)
                      VAL1=VAL1+(W1*0.01D0*VAL1)
                      IF(VAL1.EQ.0.0D0) ALENS(1,SURF)=0.0D0
                      IF(VAL1.NE.0.0D0) ALENS(1,SURF)=1.0D0/VAL1
                  END IF
              END IF
          END IF
C
          IF(ALENS(1,SURF).EQ.0.0D0.AND.ALENS(2,SURF).NE.0.0D0) THEN
              OUTLYNE='WARNING:'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'FOR SURFACE ',SURF
              CALL SHOWIT(1)
              OUTLYNE=
     1        'THE CONIC CONSTANT WILL BE IGNORED FOR THIS PLANO SURFACE'
              CALL SHOWIT(1)
          END IF
          IF(ALENS(1,SURF).NE.0.0D0.AND.ALENS(43,SURF).NE.0.0D0) THEN
              ALENS(43,SURF)=0.0D0
              OUTLYNE='WARNING:'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'FOR SURFACE ',SURF
              CALL SHOWIT(1)
              OUTLYNE=
     1        'THE "AC" TERM RESET TO 0.0 FOR THIS NON-PLANO SURFACE'
              CALL SHOWIT(1)
          END IF
C
C       CHECK CURVATURE SOLVES
C IF THE SURFACE IS NOT A TORIC ALENS(23,SURF)=0 THEN DELETE ALL CURVATUVE SOLVES
C       AND RE-CALCULATE ALENS(33,SURF) THE SOLVE TRACKER
C       IF THE SURFACE IS AN X TORIC THEN DELETE ONLY THE XZ PLANE CURVATURE SOLVES
C       IF THE SURFACE IS AN Y TORIC THEN DELETE ONLY THE YZ PLANE CURVATURE SOLVES
C
C       CHECK FOR A TORIC
          IF(ALENS(23,SURF).EQ.0.0D0) THEN
C       NO TORIC, DELETE ALL CURVATURE SOLVE
              IF(SOLVE(8,SURF).NE.0.0D0) THEN
                  SOLVE(8,SURF)=0.0D0
                  SOLVE(9,SURF)=0.0D0
                  WRITE(OUTLYNE,*)'SURFACE',SURF,' :ALL CURVATURE SOLVES DELETED'
                  CALL SHOWIT(1)
                  SOLVE(2,SURF)=0.0D0
                  SOLVE(1,SURF)=0.0D0
              END IF
C       RE-CALCULATE ALENS(33,SURF)
              ALENS(33,SURF)=0.0D0
              IF(SOLVE(6,SURF).GT.0.0D0) ALENS(33,SURF)=ALENS(33,SURF)+1.0D0
              IF(SOLVE(4,SURF).GT.0.0D0) ALENS(33,SURF)=ALENS(33,SURF)+0.1D0
              IF(SOLVE(8,SURF).GT.0.0D0) ALENS(33,SURF)=ALENS(33,SURF)+2.0D0
              IF(SOLVE(2,SURF).GT.0.0D0) ALENS(33,SURF)=ALENS(33,SURF)+0.2D0
          ELSE
C       TORIC
C               CHECK FOR Y-TORIC
              IF(ALENS(23,SURF).EQ.1.0D0) THEN
C       Y-TORIC, DELETE ALL YZ PLANE CURVATURE SOLVES
                  IF(SOLVE(8,SURF).NE.0.0D0) THEN
                      SOLVE(8,SURF)=0.0D0
                      SOLVE(9,SURF)=0.0D0
                      WRITE(OUTLYNE,*)
     1                'SURFACE',SURF,' :YZ PLANE CURVATURE SOLVES DELETED'
                      CALL SHOWIT(1)
C       RE-CALCULATE ALENS(33,SURF)
                      ALENS(33,SURF)=0.0D0
                      IF(SOLVE(6,SURF).GT.0.0D0) ALENS(33,SURF)=ALENS(33,SURF)+1.0D0
                      IF(SOLVE(4,SURF).GT.0.0D0) ALENS(33,SURF)=ALENS(33,SURF)+0.1D0
                      IF(SOLVE(8,SURF).GT.0.0D0) ALENS(33,SURF)=ALENS(33,SURF)+2.0D0
                      IF(SOLVE(2,SURF).GT.0.0D0) ALENS(33,SURF)=ALENS(33,SURF)+0.2D0
                  ELSE
C       NO SOLVES
                  END IF
              ELSE
                  IF(ALENS(23,SURF).EQ.2.0D0) THEN
C       X-TORIC, DELETE ALL XZ PLANE CURVATURE SOLVES
                      IF(SOLVE(2,SURF).NE.0.0D0) THEN
                          SOLVE(2,SURF)=0.0D0
                          SOLVE(1,SURF)=0.0D0
                          WRITE(OUTLYNE,*)
     1                    'SURFACE',SURF,' :XZ PLANE CURVATURE SOLVES DELETED'
                          CALL SHOWIT(1)
C       RE-CALCULATE ALENS(33,SURF)
                          ALENS(33,SURF)=0.0D0
                          IF(SOLVE(6,SURF).GT.0.0D0) ALENS(33,SURF)=ALENS(33,SURF)+1.0D0
                          IF(SOLVE(4,SURF).GT.0.0D0) ALENS(33,SURF)=ALENS(33,SURF)+0.1D0
                          IF(SOLVE(8,SURF).GT.0.0D0) ALENS(33,SURF)=ALENS(33,SURF)+2.0D0
                          IF(SOLVE(2,SURF).GT.0.0D0) ALENS(33,SURF)=ALENS(33,SURF)+0.2D0
                      END IF
                  ELSE
                      OUTLYNE='SERIOUS ERROR IN ASSIGNMENT OF ALENS(23, )'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
          END IF
C
C       CHECK FOR RD OR CV PIKUPS AND DELETE IF FOUND
C
          IF(ALENS(32,SURF).EQ.0.0D0) THEN
C       NO PIKUPS DON'T DO ANYTHING
C
              RETURN
          ELSE
          END IF
C
C       CHECK FOR CV OR RD OR PRO OR NPRO
C       PIKUPS AND DELETE IF FOUND
C
          IF(PIKUP(1,SURF,1).EQ.0.0D0.AND.PIKUP(1,SURF,2).EQ.0.0D0.
     1    AND.PIKUP(1,SURF,11).EQ.0.0D0.AND.PIKUP(1,SURF,12)
     2    .EQ.0.0D0) THEN
C
C       NO CV RD PRO OR NPRO PIKUPS, JUST RETURN
C
              RETURN
          ELSE
          END IF
C
C       DELETE THE PIKUP
          IF(PIKUP(1,SURF,1).GT.0.0D0) THEN
              PIKUP(1:6,SURF,1)=0.0D0
              ALENS(32,SURF)=ALENS(32,SURF)-1.0D0
              WRITE(OUTLYNE,*)'SURFACE',SURF,' :PIKUP (RD) DELETED'
              CALL SHOWIT(1)
          END IF
          IF(PIKUP(1,SURF,2).GT.0.0D0) THEN
              PIKUP(1:6,SURF,2)=0.0D0
              ALENS(32,SURF)=ALENS(32,SURF)-1.0D0
              WRITE(OUTLYNE,*)'SURFACE',SURF,' :PIKUP (CV) DELETED'
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
 10       CONTINUE
C
          IF(PIKCNT.EQ.0) ALENS(32,SURF)=0.0D0
          RETURN
      END
C SUB SINS.FOR
      SUBROUTINE SINS
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE SINS WHICH IMPLEMENTS THE INS
C       COMMANDS AT THE UPDATE LENS LEVEL.
C       THE FIRST NUMERIC WORD IS THE SURFACE WHICH WE WISH TO
C       ADD TO THE LENS. VALID VALUES ARE FROM 1 TO THE INTEGER VALUE
C       STORED IN SYSTEM1(20). FOR EXAMPLE, IF THE COMMAND
C
C                       INS,15 IS ISSUED AND SYSTEM1(20)=25
C       THEN A DUMMY BLANK DEFAULT SURFACE IS ADDED TO ALENS
C       BETWEEN EXISTING SURFACES 14 AND 15. OLD SURFACE
C       15 BECOMES SURFACE 16 AND SYSTEM1(20) IS INCREMENTED BY 1.
C       THE CURRENT SURFACE BECOMES THE SURFACE INSERTER. IF INS
C       IS ISSUED WITH NO NUMERIC INPUT, A DUMMY SURFACE IS ENTERED
C       AHEAD OF THE CURRENT, CURRENT SURFACE. AFTER THE INSERTION
C       THE NEWLY INSERTED SURFACE BECOMES THE NEW CURRENT SURFACE.
C       SOLVE ARRAY AND PIKUPS ALSO UPDATED
C
          INTEGER K,L,I,J,II,III,IV
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datsub.inc'
C
          IF(SQ.EQ.1.OR.SST.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1
     1    .OR.S5.EQ.1) THEN
              OUTLYNE='"INS" ONLY TAKES NUMERIC WORD #1 AND #2 INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(DF1.EQ.1) THEN
              W1=DBLE(SURF)
              DF1=0
          ELSE
          END IF
          IF(DF2.EQ.1) THEN
              W2=1.0D0
              DF2=0
          ELSE
          END IF
          IF(GLANAM(INT(W1)-1,2).EQ.'PERFECT      ') THEN
              OUTLYNE=
     1        'A SURFACE MAY NOT BE INSERTED BEHIND A "PERFECT" SURFACE'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(GLANAM(INT(W1)-1,2).EQ.'IDEAL        ') THEN
              OUTLYNE=
     1        'A SURFACE MAY NOT BE INSERTED BEHIND A "IDEAL" SURFACE'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
C       IS DEFAULT SURFACE ENTRY IN EFFECT?
C
          DO II=1,INT(W2)
C
C       USE THE NUMERIC
C       VALUE OF NUMERIC WORD 1 AS THE LOCATION OF THE SURFACE
C       INSERTION.
              IF(W1.EQ.0.0D0) THEN
                  OUTLYNE=
     1            'SURFACE INSERTION INFRONT OF OBJECT SURF. NOT ALLOWED'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C
              IF(W1.LT.0.0D0) W1=SYSTEM1(20)+W1
              IF(W1.LT.1.0D0.OR.W1.GT.SYSTEM1(20)) THEN
C
C       TRYING TO INSERT A SURFACE NUMBER INFRONT OF THE
C       OBJECT SURFACE OR AFTER THE IMAGE SURFACE. THIS OPERATION
C       IS NOT ALLOWED.
C       PRINT ERROR AND RETURN.
                  OUTLYNE='INVALID LOCATION FOR INSERTED SURFACE'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              ELSE
C       SURFACE NUMBER WITHIN VALID RANGE.
C
C
C       IS SURFACE INSERTED, INFRONT OF REFERENCE SURFACE ?
                  IF(INT(W1).LE.INT(SYSTEM1(25))) THEN
                      SYSTEM1(25)=SYSTEM1(25)+1.0D0
                      NEWREF=INT(SYSTEM1(25))
                  ELSE
C       NOT IN FRONT OF REF SURF
                  END IF
C       IS SURFACE INSERTED, INFRONT OF ASTOP SURFACE ?
                  IF(INT(W1).LE.INT(SYSTEM1(26)).AND.
     1            SYSTEM1(26).NE.-99.0D0) THEN
                      SYSTEM1(26)=SYSTEM1(26)+1.0D0
                  ELSE
C       NOT IN FRONT OF ASTOP SURFOR THERE IS NO ASTOP SURFACE
                  END IF
C
                  SYSTEM1(20)=SYSTEM1(20)+1.0D0
                  K=INT(W1)
                  L=INT(SYSTEM1(20))
                  DO J=(L-1),K,-1
                      ALENS(1:LSIZ,(J+1))=ALENS(1:LSIZ,J)
                  END DO
                  DO J=(L-1),K,-1
                      LBL(J+1)(1:80)=LBL(J)(1:80)
                  END DO
C     NOW SPSRF DATA
                  DO  J=(L-1),K,-1
                      FTFL01(1:96,J+1)=FTFL01(1:96,J)
                  END DO
                  DO J=(L-1),K,-1
                      GLANAM((J+1),1)=GLANAM(J,1)
                      GLANAM((J+1),2)=GLANAM(J,2)
                      SOLVE(0:9,J+1)=SOLVE(0:9,J)
                  END DO
C
C     L IS THE NEW FINAL SURFACE OF THE LENS
                  DO J=0,L
                      DO I=1,PSIZ
                          IF(I.NE.32) THEN
C     NOT THOAL
C       IF THE SURFACE NUMBER OF THE SURFACE BEING INSERTED IS
C       NUMERICALLY LESS THAN OR EQUAL TO SURFACE NUMBER REFERENCED
C       BY THE PIKUP (PIKUP(2,J,I) THEN
                              IF(ALENS(32,J).NE.0.0D0) THEN
C     THERE IS A PIKUP ON SURFACE J
                                  IF(K.LE.INT(PIKUP(2,J-1,I))) THEN
                                      PIKUP(2,J-1,I)=PIKUP(2,J-1,I)+1.0D0
                                  ELSE
C       SURFACE BEING INSERTED AFTER THE SURFACE REFERENCED BY
C       THE PIKUP
                                  END IF
                              END IF
                          ELSE
C     THOAL
                              IF(ALENS(32,J).NE.0.0D0) THEN
C     THERE IS A PIKUP ON SURFACE J
                                  IF(K.LE.INT(PIKUP(2,J-1,I))) THEN
                                      PIKUP(2,J-1,I)=PIKUP(2,J-1,I)+1.0D0
                                  ELSE
C       SURFACE BEING INSERTED AFTER THE SURFACE REFERENCED BY
C       THE PIKUP
                                  END IF
                                  IF(K.LE.INT(PIKUP(3,J-1,I))) THEN
                                      PIKUP(3,J-1,I)=PIKUP(3,J-1,I)+1.0D0
                                  ELSE
C       SURFACE BEING INSERTED AFTER THE SURFACE REFERENCED BY
C       THE PIKUP
                                  END IF
                              END IF

                          END IF
                      END DO
                  END DO
C
C     K IS THE SURFACE BEING INSERTED
                  DO J=(L-1),K,-1
                      PIKUP(1:6,J+1,1:PSIZ)=PIKUP(1:6,J,1:PSIZ)
                  END DO
C
                  SURF=INT(W1)
                  CALL FLUSHNEXT
                  IF(VBCNT.NE.0) THEN
                      DO III=1,VBCNT
                          IF(VARABL(III,3).GE.W1) VARABL(III,3)=VARABL(III,3)+1.0D0
                      END DO
                  END IF
                  IF(TVBCNT.NE.0) THEN
                      DO III=1,TVBCNT
                          IV=III+MAXCMP
                          IF(VARABL(IV,3).GE.W1) VARABL(IV,3)=VARABL(IV,3)+1.0D0
                      END DO
                  END IF
                  IF(CMPCNT.NE.0) THEN
                      DO III=1,CMPCNT
                          IF(VARABL(III,3).GE.W1) VARABL(III,3)=VARABL(III,3)+1.0D0
                      END DO
                  END IF
C     HERE IS WHERE WE CALL CFGFX1.FOR TO FIX POSSIBLE
C     CONFIGS DATA IMPACTED BY THE INSERTION
                  CALL CFGFX1(INT(W1))
C
              END IF
C
C       PRINT INSERTION MESSAGE
              WRITE(OUTLYNE,*)'SURFACE',SURF,' :SURFACE INSERTED'
              CALL SHOWIT(1)
              F22=1
C       NOW SURFACE NUMBER SURF HAS BEEN INSERTED
              DO I=0,INT(SYSTEM1(20))
                  IF(ALENS(25,I).EQ.6.0D0.OR.ALENS(25,I).EQ.1.0D0.AND.
     1            ALENS(77,I).EQ.1) THEN
C       FOUND A TILT RET
                      IF((ALENS(70,I)).GE.SURF) ALENS(70,I)=ALENS(70,I)+1.0D0
                  END IF
              END DO
          END DO

          RETURN
      END
C SUB SDEL.FOR
      SUBROUTINE SDEL
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE SDEL WHICH IMPLEMENTS THE DEL
C       COMMANDS AT THE UPDATE LENS LEVEL.
C       THE FIRST NUMERIC WORD IS THE SURFACE WHICH WE WISH TO
C       DELETE FROM THE LENS. VALID VALUES ARE FROM 1 TO THE INTEGER VALUE
C       STORED IN (SYSTEM1(20) MINUS 1).
C
C       IT IS NOT PERMITTED TO DELETE THE OBJECT OR IMAGE SURFACES.
C
C        FOR EXAMPLE, IF THE COMMAND
C
C                       DEL,15 IS ISSUED AND SYSTEM1(20)=25
C       THEN SURFACE 15 IS DELETE. OLD SURFACE 16 BECOMES NEW 15
C       AND SO FORTH AND SYSTEM1(20) IS DECREMENTED BY 1.0D0. THE CURRENT
C       SURFACE BECOMES THE NEW SURFACE 15.
C       ISSUING DEL WITH NO NUMERIC WORD CAUSES THE CURRENT SURFACE
C       TO BE DELETED IF THE CURRENT SURFACE IS NOT THE OBJECT OR
C       IMMAGE SURFACE. IF THE CURRENT SURFACE IS THE OBJECT SURFACE,
C       AN MESSAGE IS PRINTED STATING THAT THE OBJECT SURFACE MAY NOT
C       BE DELETED. IF THE CURRENT SURFACE IS THE IMAGE SURFACE THEN
C       THE CURRENT SURFACE IS DECREMENTED BY ONE AND THEN THE DELETION
C       IS PERFORMED. THIS MAY BE REPEATED UNTIL ONLY THE OBJECT AND
C       IMAGE SURFACES REMAIN. SOLVE DATA ALSO FIXED.
C
          INTEGER PIKCNT,J,I,K,III,II,SNUMBER,IIII,IV,V
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          INCLUDE 'datsub.inc'
C
C               CHECK FOR PRESENCE OF QUALIFIER OR STRING INPUT
C               PRINT ERROR AND RETURN IF DISCOVERED.
C
          IF(SQ.EQ.1.OR.SST.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1
     1    .OR.S5.EQ.1) THEN
              OUTLYNE='"DEL" ONLY TAKES NUMERIC WORD #1 AND #2 INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
C
C               WE ARE AT LENS UPDATE LEVEL
C
C       IS DEFAULT SURFACE ENTRY IN EFFECT?
C
          IF(DF1.EQ.1) THEN
              W1=DBLE(SURF)
              DF1=0
          ELSE
          END IF
          IF(W1.LT.0.0D0) W1=SYSTEM1(20)+W1
C
          IF(DF2.EQ.1) THEN
              W2=W1
              DF2=0
          ELSE
          END IF
C
          IF(W2.LT.W1) THEN
              OUTLYNE=
     1        'STARTING SURFACE # MAY NOT BE GREATER THAN ENDING SURFACE #'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(W1.LE.0.0D0.OR.W1.GE.SYSTEM1(20).OR.W2.LE.0.0D0.OR.
     1    W2.GE.SYSTEM1(20)) THEN
C
C       TRYING TO DELETE A SURFACE NUMBER LESS THAN OR EQUAL TO THE
C       OBJECT SURFACE OR GREATER THAN OR EQUAL TO THE IMAGE SURFACE
C       IS NOT ALLOWED.
C       PRINT ERROR AND RETURN.
C
              IF(W1.EQ.0.0.OR.W2.EQ.0.0D0) THEN
                  OUTLYNE='OBJECT SURFACE DELETION NOT ALLOWED'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(W1.EQ.SYSTEM1(20).OR.W2.EQ.SYSTEM1(20)) THEN
                  OUTLYNE='IMAGE SURFACE DELETION NOT ALLOWED'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
C
          SNUMBER=INT(W1)
          SURF=INT(W1)
          DO II=1,INT(W2)-INT(W1)+1
C
C       DELETION MESSAGE
              IF(VBCNT.NE.0) THEN
                  DO III=1,VBCNT
                      IF(INT(VARABL(III,3)).EQ.SNUMBER) THEN
C     DELETE AN ENTRY
                          DO IV=III,VBCNT-1
                              VARABL(IV,1:17)=VARABL(IV+1,1:17)
                          END DO
                          VBCNT=VBCNT-1
                          GO TO 20
                      END IF
                      IF(INT(VARABL(III,3)).GT.SNUMBER) THEN
C     MOVE ALL SURF NUMBERS UP
                          DO IIII=1,VBCNT
                              VARABL(IIII,3)=VARABL(IIII,3)-1.0D0
                          END DO
                      END IF
 20                   CONTINUE
                  END DO
              END IF
              IF(TVBCNT.NE.0) THEN
                  DO III=1,TVBCNT
                      V=III+MAXCMP
                      IF(INT(VARABL(V,3)).EQ.SNUMBER) THEN
C     DELETE AN ENTRY
                          DO IV=III,TVBCNT-1
                              V=IV+MAXCMP
                              VARABL(IV,1:17)=VARABL(IV+1,1:17)
                          END DO
                          TVBCNT=TVBCNT-1
                          GO TO 30
                      END IF
                      IF(INT(VARABL(III,3)).GT.SNUMBER) THEN
C     MOVE ALL SURF NUMBERS UP
                          DO IIII=1,TVBCNT
                              VARABL(IIII,3)=VARABL(IIII,3)-1.0D0
                          END DO
                      END IF
 30                   CONTINUE
                  END DO
              END IF
              IF(CMPCNT.NE.0) THEN
                  DO III=1,CMPCNT
                      IF(INT(VARABL(III,3)).EQ.SNUMBER) THEN
C     DELETE AN ENTRY
                          DO IV=III,CMPCNT-1
                              VARABL(IV,1:17)=VARABL(IV+1,1:17)
                          END DO
                          CMPCNT=CMPCNT-1
                          GO TO 40
                      END IF
                      IF(INT(VARABL(III,3)).GT.SNUMBER) THEN
C     MOVE ALL SURF NUMBERS UP
                          DO IIII=1,CMPCNT
                              VARABL(IIII,3)=VARABL(IIII,3)-1.0D0
                          END DO
                      END IF
 40                   CONTINUE
                  END DO
              END IF
              WRITE(OUTLYNE,*)'DELETING SURFACE ',SNUMBER
              CALL SHOWIT(1)
C
C       IS THE SURFACE THE REFERENCE SURFACE ?
              IF(SURF.EQ.INT(SYSTEM1(25))) THEN
                  OUTLYNE='CURRENT REFERENCE SURFACE BEING DELETED'
                  CALL SHOWIT(1)
                  OUTLYNE='REFERENCE SURFACE SHIFTED TO SURFACE 1'
                  CALL SHOWIT(1)
                  SYSTEM1(25)=1.0D0
              END IF
C       IS SURFACE DELETED, INFRONT OF REFERENCE SURFACE ?
              IF(SURF.LT.INT(SYSTEM1(25))) THEN
                  SYSTEM1(25)=SYSTEM1(25)-1.0D0
                  NEWREF=INT(SYSTEM1(25))
              ELSE
C       NOT IN FRONT OF REF SURF
              END IF
C       IS THE SURFACE THE ASTOP SURFACE ?
              IF(SURF.EQ.INT(SYSTEM1(26))) THEN
                  OUTLYNE='CURRENT APERTURE STOP SURFACE BEING DELETED'
                  CALL SHOWIT(1)
                  SYSTEM1(26)=-99.0D0
                  SYSTEM1(27)=0.0D0
              END IF
C       IS SURFACE DELETED, INFRONT OF ASTOP SURFACE ?
              IF(SURF.LT.INT(SYSTEM1(26)).AND.
     1        SYSTEM1(26).NE.-99.0D0) THEN
                  SYSTEM1(26)=SYSTEM1(26)-1.0D0
              ELSE
C       NOT IN FRONT OF ASTOP SURF OR NO ASTOP DEFINED.
              END IF
C
C       DELETE SURFACE
C       REPRESENTED BY SURF,SURF EITHER INPUT ON COMMAND LINE
C       OR SET BY ABOVE DEFAULT PROCEEDURE.
C
C       BEFORE DELETING THE SURFACE,CHECK TO SEE IF THERE ARE
C       ANY SOLVES OR PIKUPS ON THAT SURFACE AND IF THERE
C       ARE ANY, PRINT A MESSAGE THAT EITHER SOLVES OR PIKUPS
C       ARE BEING DELETED WITH THE SURFACE DELETION.
C
C       FIRST SOLVES
C
              IF(ALENS(32,SURF).NE.0.0D0) THEN
                  OUTLYNE=
     1            'PIKUP DATA BEING DELETED WITH SURFACE DELETION'
                  CALL SHOWIT(1)
              END IF
C
C       THEN PIKUPS
C
              IF(ALENS(33,SURF).NE.0.0D0) THEN
                  OUTLYNE=
     1            'SOLVE DATA BEING DELETED WITH SURFACE DELETION'
                  CALL SHOWIT(1)
              END IF
C
C       THEN SPECIAL SURFACE DATA
C
C       NOW WHAT IF THE SURFACE TO BE DELETED IS THE SURFACE
C       FROM WHICH SOMETHING IS BEING PIKED UP FROM. IN THAT CASE
C       THE PIKUP SHOULD BE DELETED WITH THE ASSOCIATED PARAMETER
C       LEFT AT ITS CURRENT VALUE.
C
C       THE ENTIRE LENS MUST BE CHECKED TO SEE IF A PIKUP
C       REFERS TO THE SURFACE TO BE DELETED. THAT PIKUP MUST BE
C       DELETED. IF THAT WAS THE LAST PIKUP ON THAT SURFACE THEN
C       ALENS(32,(FOR THE SURFACE WITH PIKUPS)) IS SET TO ZERO
C
              DO J=0,INT(SYSTEM1(20))
                  IF(ALENS(32,J).NE.0D0) THEN
C       FOUND A SURFACE WITH PIKUPS
                      DO I=1,PSIZ
                          IF(PIKUP(1,J,I).EQ.1) THEN
                              IF(INT(PIKUP(2,J,I)).EQ.SURF) THEN
C       FOUND A PIKUP WHICH REFERENCES A SURFACE TO BE DELETED
C       DELETE THE PIKUP
C       AND PRINT MESSAGE
C
                                  PIKUP(1:6,J,I)=0.0D0
                                  OUTLYNE='PIKUP REFERENCED BY DELETED SURFACE HAS BEEN DELETED'
                                  CALL SHOWIT(1)
                                  OUTLYNE='ASSOCIATED PARAMETER FROZEN AT CURRENT VALUE'
                                  CALL SHOWIT(1)
                              END IF
                          END IF
                      END DO
                  END IF
C       CHECK THE NEXT SURFACE
              END DO
C
C               NOW FIX THE STATUS OF ALENS(32,K) FOR ALL
C       SURFACES K=0 TO INT(SYSTEM1(20))
C
              DO 405 K=0,INT(SYSTEM1(20))
                  IF(ALENS(32,K).NE.0.0D0) THEN
C       THE MIGHT BE PIKUPS
C       SHOULD THE STATUS BE CHANGED
                      PIKCNT=0
                      DO 406 I=1,PSIZ
                          IF(PIKUP(1,K,I).NE.0.0D0) THEN
                              PIKCNT=PIKCNT+1
                          ELSE
C       PROCEED
                          END IF
 406                  CONTINUE
C       IF NO PIKUPS FOUND, CHANGE STATUS
                      IF(PIKCNT.EQ.0) ALENS(32,K)=0.0D0
C       PROCEED TO CHECK THE NEXT SURFACE FOR PIKUPS TO BE
C       STATUS RESOLVED
                  ELSE
                  END IF
 405          CONTINUE
C
              IF(ALENS(34,SURF).NE.0.0D0) THEN
                  OUTLYNE=
     1            'SPECIAL SURFACE DATA BEING DELETED WITH SURFACE DELETION'
                  CALL SHOWIT(1)
              END IF
C
              K=INT(SYSTEM1(20))-1
              DO J=SURF,K
                  ALENS(1:LSIZ,J)=ALENS(1:LSIZ,(J+1))
              END DO
C
              DO I=SURF,K
                  LBL(I)(1:80)=LBL(I+1)(1:80)
              END DO
C
              DO J=SURF,K
                  FTFL01(1:96,J)=FTFL01(1:96,(J+1))
              END DO
              DO J=SURF,K
                  GLANAM(J,1)=GLANAM((J+1),1)
                  GLANAM(J,2)=GLANAM((J+1),2)
                  SOLVE(0:9,J) = SOLVE(0:9,(J+1))
              END DO
C     THE FIRST SURFACE IN THE LOOP IS THE SURFACE 0
C     SNUMBER IS THE CURRENT SURFACE
              DO J=0,K
                  DO I=1,PSIZ
                      IF(I.NE.32) THEN
C     NOT THOAL
C       IF THE SURFACE NUMBER OF THE SURFACE BEING DELETED IS
C       NUMERICALLY LESS THAN OR EQUAL TO SURFACE NUMBER REFERENCED
C       BY THE PIKUP (PIKUP(2,J+1,I)
                          IF(SURF.LE.INT(PIKUP(2,J+1,I)))
     1                    PIKUP(2,J+1,I)=PIKUP(2,J+1,I)-1.0D0
                      ELSE
C     THOAL
                          IF(SURF.LE.INT(PIKUP(2,J+1,I)))
     1                    PIKUP(2,J+1,I)=PIKUP(2,J+1,I)-1.0D0
                          IF(SURF.LE.INT(PIKUP(3,J+1,I)))
     1                    PIKUP(3,J+1,I)=PIKUP(3,J+1,I)-1.0D0
                      END IF
                  END DO
              END DO
              DO J=SURF,K
                  PIKUP(1:6,J,1:PSIZ)=PIKUP(1:6,J+1,1:PSIZ)
              END DO
C
C       DECREMENT THE IMAGE SURFACE NUMBER COUNTER SYSTEM1(20) BY 1.0D0
              IF(SYSTEM1(20).GT.0.0D0) THEN
                  SYSTEM1(20)=(SYSTEM1(20)-1.0D0)
              ELSE
                  SYSTEM1(20)=0.0D0
              END IF
C
C       IF THE CURRENT SURFACE IS THE IMAGE SURFACE,
C       PRINT WARNING MESSAGE
C
              IF(SURF.EQ.INT(SYSTEM1(20)))THEN
                  OUTLYNE='CURRENT SURFACE IS NOW THE IMAGE SURFACE'
                  CALL SHOWIT(1)
              END IF
              F22=1
C     HERE IS WHERE WE CALL CFGFX2.FOR  AND CFGFX3.FOR TO FIX POSSIBLE
C     CONFIGS DATA IMPACTED BY THE DELETION
              CALL CFGFX2(SURF)
              CALL CFGFX3(SURF)
C     SURF WAS DELETED
              DO I=0,INT(SYSTEM1(20))
                  IF(ALENS(25,I).EQ.6.0D0.OR.ALENS(25,I).EQ.1.0D0.AND.
     1            ALENS(77,I).EQ.1) THEN
C       FOUND A TILT RET
                      IF((ALENS(70,I)).GE.SURF) ALENS(70,I)=ALENS(70,I)-1.0D0
                  END IF
              END DO
          END DO
          RETURN
      END
C SUB SDEC.FOR
      SUBROUTINE SDEC
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE SDEC WHICH IMPLEMENTS THE DEC
C       COMMAND AT THE LENS INPUT LEVEL AND LENS UPDATE LEVEL.
C       THE DEC COMMAND IS:
C
C               DEC, YD XD ZD
C
          INTEGER PIKCNT,I
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
C               CHECK FOR ADDITIONAL INPUT AND
C               PRINT ERROR AND RETURN IF DISCOVERED.
C
          IF(SST.EQ.1.OR.SQ.EQ.1) THEN
              OUTLYNE='"DEC" TAKES NO QUALIFIER OR STRING INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(S4.EQ.1.OR.S5.EQ.1) THEN
              OUTLYNE='"DEC" TAKES NO NUMERIC WORD #4 OR #5 INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(DF1.EQ.1.AND.DF2.EQ.1.AND.DF3.EQ.1) THEN
              OUTLYNE='"DEC" REQUIRES SOME EXPLICIT NUMERIC INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
          IF(F5.EQ.1.OR.F6.EQ.1) THEN
              IF(SURF.EQ.0) THEN
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
C               PROCEED
C
C       DON'T SET DEC FLAG (ALENS(29,SURF) IF YD AND XD ARE BOTH 0.0D0
C       BUT CLEAR IT.
C
C       CHECK FOR PIKUP YD OR PIKUP XD OR ZD AND IF FOUND REMOVE
C
              IF(PIKUP(1,SURF,13).EQ.1.0D0) THEN
C       DELETE PIKUP YD
                  PIKUP(1:6,SURF,13)=0.0D0
                  WRITE(OUTLYNE,*)'SURFACE',SURF,' :PIKUP (YD) DELETED'
                  CALL SHOWIT(1)
              END IF
              IF(PIKUP(1,SURF,33).EQ.1.0D0) THEN
C       DELETE PIKUP YD
                  PIKUP(1:6,SURF,33)=0.0D0
                  WRITE(OUTLYNE,*)'SURFACE',SURF,' :PIKUP (ZD) DELETED'
                  CALL SHOWIT(1)
              END IF
              IF(PIKUP(1,SURF,14).EQ.1.0D0) THEN
C       DELETE PIKUP XD
                  PIKUP(1:6,SURF,14)=0.0D0
                  WRITE(OUTLYNE,*)'SURFACE',SURF,' :PIKUP (XD) DELETED'
                  CALL SHOWIT(1)
              END IF
C
C       SET ALENS(32,SURF) IF NO OTHER PIKUPS EXIST
              PIKCNT=0
              DO 10 I=1,PSIZ
                  IF(PIKUP(1,SURF,I).NE.0.0D0) THEN
                      PIKCNT=PIKCNT+1
                  ELSE
                  END IF
 10           CONTINUE
              IF(PIKCNT.EQ.0) ALENS(32,SURF)=0.0D0
C
C       PROCEED WITH DEC ASSIGNMENT
C
              IF(W1.EQ.0.0D0.AND.W2.EQ.0.0D0.AND.W3.EQ.0.0D0) THEN
                  IF(DF1.EQ.1) W1=0.0D0
                  IF(DF2.EQ.1) W2=0.0D0
                  IF(DF3.EQ.1) W3=0.0D0
                  ALENS(29,SURF)=0.0D0
                  ALENS(30,SURF)=W1
                  ALENS(31,SURF)=W2
                  ALENS(69,SURF)=W3
                  ALENS(115,SURF)=W1
                  ALENS(114,SURF)=W2
                  ALENS(116,SURF)=W3
              ELSE
C       NOT BOTH W1 AND W2 ARE ZERO, PROCEED
C       SET DEC FLAG AND VALUES.
                  IF(DF1.EQ.1) W1=ALENS(30,SURF)
                  IF(DF2.EQ.1) W2=ALENS(31,SURF)
                  IF(DF3.EQ.1) W3=ALENS(69,SURF)
                  ALENS(29,SURF)=1.0D0
                  ALENS(30,SURF)=W1
                  ALENS(31,SURF)=W2
                  ALENS(69,SURF)=W3
                  ALENS(115,SURF)=W1
                  ALENS(114,SURF)=W2
                  ALENS(116,SURF)=W3
              END IF
C
C       NOW WHAT IF THIS CURRENT SURFACE IS BEING PIKUP UP WITH A
C       PIKUP YD OR PIKUP XD FROM ANOTHER SURFACE. IF IT IS THEN
C       IF THE CURRENT WORK HERE CHANGED THE STATUS OF ALENS(29,SURF)
C       IT WILL CHANGE THE STATUS ON THE PIKING SURFACE.
C       RESOLVE THIS ISSUE.
C
              DO 100 I=0,INT(SYSTEM1(20))
                  IF(PIKUP(1,I,13).EQ.1.0D0.OR.
     1            PIKUP(1,I,14).EQ.1.0D0.OR.PIKUP(1,I,33).EQ.1.0D0.OR.
     2            PIKUP(1,I,13).EQ.1.0D0.AND.PIKUP(1,I,14).EQ.1.0D0
     2            .AND.PIKUP(1,I,33).EQ.1.0D0) THEN
                      IF(INT(PIKUP(2,I,13)).EQ.SURF.OR.
     1                INT(PIKUP(2,I,14)).EQ.SURF.OR.INT(PIKUP(2,I,33)).EQ.
     2                SURF) THEN
C       UPDATE THE ALENS(29) ON THE PIKING SURFACE
                          ALENS(29,I)=ALENS(29,SURF)
                      ELSE
C       DONT DO ANYTHING
                      END IF
                  ELSE
C       NOT RIGHT KIND OF PIKUP,DO NOTING,PROCEED TO NEXT SURFACE
                  END IF
 100          CONTINUE
C
C       IF THE SURFACE HAS A TILT AUTO THEN REMOVE THE
C       AUTO AND PRINT MESSAGE
C
              IF(ALENS(25,SURF).EQ.2.0D0.OR.ALENS(25,SURF).EQ.
     1        3.0D0) THEN
                  ALENS(25,SURF)=1.0D0
                  IF(ALENS(25,SURF).EQ.2.0D0) OUTLYNE=
     1            '"AUTO" ADJUST REMOVED FROM SURFACE TILT DEFINITION'
                  IF(ALENS(25,SURF).EQ.3.0D0) OUTLYNE=
     1            '"AUTOM" ADJUST REMOVED FROM SURFACE TILT DEFINITION'
                  CALL SHOWIT(1)
              END IF
          END IF
          RETURN
      END
