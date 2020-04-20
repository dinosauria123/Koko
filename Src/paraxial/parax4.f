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

C       FOURTH SET OF PARAXIAL ROUTINES GO HERE

C SUB FADJ.FOR
      SUBROUTINE FADJ
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE FADJ. THIS IS THE SUBROUTINE
C       WHICH PERFORMS THE FNBY/X ADJUSTMENT
C
          INTEGER ITYPEP
C
          COMMON/PTYPER/ITYPEP
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
          IF(ITYPEP.EQ.1) THEN
C       FIRST CHECK FOR PUY=0 AT IMAGE PLANE
              IF(DABS(PXTRAY(2,(INT(SYSTEM1(20))))).LE.1.0D-15) THEN
                  OUTLYNE='FINAL "PUY" VALUE IS ZERO, F-NUBER HAS NO MEANING'
                  CALL SHOWIT(1)
                  OUTLYNE='"FNBY" ADJUSTMENT NOT PERFORMED AND WILL BE REMOVED'
                  CALL SHOWIT(1)
                  SYSTEM1(44)=0.0D0
                  SYSTEM1(46)=0.0D0
                  RETURN
              ELSE
C       PUY(FINAL) NOT ZERO, PROCEED
              END IF
C       NOW, THE CURRENT FNUMBER IS JUST 1.0/(2.0*PUY(FINAL))
C       THE DESIRED F-NUMBER IS STORED IN SYSTEM1(46)
C       IF ALL SOLVED ARE SHUT OFF, THE NEW F-NUMBER
C       IS REALIZED BY SCALING THE SAY VALUE BY:
C
C       CURRENT F-NUMBER/NEW F-NUMBER
C
              SYSTEM1(12)=SYSTEM1(12)*(-(1.0D0/(2.0D0*
     1        PXTRAY(2,(INT(SYSTEM1(20))))))/SYSTEM1(46))
C
C       NOW WE PERFORM A PARAXIAL RAY TRACE WITHOUT AND SOLVES
C
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
                          SYSTEM1(13)=ALENS(3,0)*SYSTEM1(66)
                      END IF
                      IF(SYSTEM1(67).EQ.1.0D0) THEN
                          SYSTEM1(12)=ALENS(3,0)/(2.0D0*SYSTEM1(68))
                          SYSTEM1(13)=ALENS(3,0)/(2.0D0*SYSTEM1(69))
                      END IF
                  END IF
              END IF
C
              CALL TR
C
C       NOW THE PARAXIAL SOLVE DATA ARRAYS ARE LOADED WITH THE CORRECT
C       SOLVELESS DATA. NOW RE-SET ALL SOLVE TARGET VALUES
C
C       WHEN THE RETURN OCCURS, THE RETURN WILL GO TO LNSEOS IN
C       THE CASE OF HLDS OR TO SFNBY.FOR OR SER.FOR. LNSEOS.FOR
C       WILL AGAIN PERFORM THE TRACE FIXING THE EXIT PUPIL DISTACES AND
C       CALCULATING CHROMATIC AND OTHER VALUES.
C       A CALL TO PRTRA1 AND PRTRA2 IS PLACED IN SER.FOR AND SFNB.FOR
C       TO PERFORM A SIMILAR FUNCTION.
C       SVYSET RESETS ALL YZ-PLANE PARAXIAL SOLVES USING CURRENT YZ-PLANE
C       PARAXIAL DATA.
              CALL SVSET
              RETURN
          ELSE
C       ITYPEP NOT 1
          END IF
          IF(ITYPEP.EQ.2) THEN
C
C       FIRST CHECK FOR PUX=0 AT IMAGE PLANE
              IF(DABS(PXTRAX(2,(INT(SYSTEM1(20))))).LE.1.0D-15) THEN
                  OUTLYNE='FINAL "PUX" VALUE IS ZERO, F-NUBER HAS NO MEANING'
                  CALL SHOWIT(1)
                  OUTLYNE='"FNBX" ADJUSTMENT NOT PERFORMED AND WILL BE REMOVED'
                  CALL SHOWIT(1)
                  SYSTEM1(45)=0.0D0
                  SYSTEM1(47)=0.0D0
                  RETURN
              ELSE
C       PUX(FINAL) NOT ZERO, PROCEED
              END IF
C       NOW, THE CURRENT FNUMBER IS JUST 1.0/(2.0*PUX(FINAL))
C       THE DESIRED F-NUMBER IS STORED IN SYSTEM1(47)
C       IF ALL SOLVED ARE SHUT OFF, THE NEW F-NUMBER
C       IS REALIZED BY SCALING THE SAX VALUE BY:
C
C       CURRENT F-NUMBER/NEW F-NUMBER
C
              SYSTEM1(13)=SYSTEM1(13)*(-(1.0D0/(2.0D0*
     1        PXTRAX(2,(INT(SYSTEM1(20))))))/SYSTEM1(47))
C
C       NOW WE PERFORM A PARAXIAL RAY TRACE WITHOUT AND SOLVES
C
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
                          SYSTEM1(13)=ALENS(3,0)*SYSTEM1(66)
                      END IF
                      IF(SYSTEM1(67).EQ.1.0D0) THEN
                          SYSTEM1(12)=ALENS(3,0)/(2.0D0*SYSTEM1(68))
                          SYSTEM1(13)=ALENS(3,0)/(2.0D0*SYSTEM1(69))
                      END IF
                  END IF
              END IF
C
              CALL TR
C
C       NOW THE PARAXIAL SOLVE DATA ARRAYS ARE LOADED WITH THE CORRECT
C       SOLVELESS DATA. NOW RE-SET ALL SOLVE TARGET VALUE
C
C       WHEN THE RETURN OCCURS, THE RETURN WILL GO TO LNSEOS IN
C       THE CASE OF HLDS OR TO SFNB.FOR OR SER.FOR. LNSEOS.FOR
C       WILL AGAIN PERFORM THE TRACE FIXING THE EXIT PUPIL DISTACES AND
C       CALCULATING CHROMATIC AND OTHER VALUES.
C       A CALL TO PRTRA1 AND PRTRA2 IS PLACED IN SER.FOR AND SFNB.FOR
C       TO PERFORM A SIMILAR FUNCTION.
C       SVXSET RESETS ALL XZ-PLANE PARAXIAL SOLVES USING CURRENT XZ-PLANE
C       PARAXIAL DATA.
              CALL SVSET
              RETURN
          ELSE
C       ITYPEP NOT 2
          END IF
          RETURN
      END
C SUB G357.FOR
      SUBROUTINE G357
C
          IMPLICIT NONE
C
C       SUBROUTINE GET SERVES TO GET THE 3RD, 5TH AND 7TH
C       ORDER ABERRATIONS, THEIR TOTALS AND THEIR CHROMATIC
C       DIFFERENCES. IT IS CALLED BY GET.FOR AND RETURNS THE
C       REAL*8
C
          REAL*8 VALUE1,INV,V,INTV,W1A,W1B,V1
C
          LOGICAL NEG
C
          INTEGER SF,CW,I,NUM5
C
          COMMON/GV/VALUE1,NUM5
C
          COMMON/LGV/NEG
C
          INCLUDE 'datcfg.inc'
          INCLUDE 'datmac.inc'
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
C
          CALL PRTRB
          CALL PRTRC
          CALL PRTRD
C
          W1A=W1
          W1B=W1
          IF(NEG) THEN
              IF(W1.NE.0.0D0) THEN
                  W1A=W1
                  W1B=W1-1.0D0
              END IF
          END IF
C       GET IMDISX AND IMDISY

          IF(WQ.EQ.'IMDISY') THEN
C       VALUE1 IS DISTACE TO FOCUS
              IF(PXTRAY(2,INT(W1B)).NE.0.0D0) THEN
                  VALUE1=((-PXTRAY(1,INT(W1A)))/(PXTRAY(2,INT(W1B))))
              ELSE
                  VALUE1=1.0D300
              END IF
              RETURN
          END IF
          IF(WQ.EQ.'IMDISX') THEN
C       VALUE1 IS DISTACE TO FOCUS
              IF(PXTRAX(2,INT(W1B)).NE.0.0D0) THEN
                  VALUE1=((-PXTRAX(1,INT(W1A)))/(PXTRAX(2,INT(W1B))))
              ELSE
                  VALUE1=1.0D300
              END IF
              RETURN
          END IF
C
C       GET CHFIM
          IF(WQ.EQ.'CHFIMY') THEN
C       V1 IS DISTACE TO FOCUS, VALUE IS GAUSSIAN IMAGE HEIGHT THERE.
              V1=((-PXTRAY(1,INT(W1A)))/(PXTRAY(2,INT(W1B))))
              VALUE1=(V1*PXTRAY(6,INT(W1B)))+PXTRAY(5,INT(W1A))
              RETURN
          END IF
          IF(WQ.EQ.'CHFIMX') THEN
C       V1 IS DISTACE TO FOCUS, VALUE IS GAUSSIAN IMAGE HEIGHT THERE.
              V1=((-PXTRAX(1,INT(W1A)))/(PXTRAX(2,INT(W1B))))
              VALUE1=(V1*PXTRAX(6,INT(W1B)))+PXTRAX(5,INT(W1A))
              RETURN
          END IF
C
C
C       GET PUPDIS(X OR Y)
          IF(WQ.EQ.'PUPDISY') THEN
C       VALUE1 IS DISTANCE TO PUPIL
              VALUE1=((-PXTRAY(5,INT(W1A)))/(PXTRAY(6,INT(W1B))))
              RETURN
          END IF
          IF(WQ.EQ.'PUPDISX') THEN
C       VALUE1 IS DISTANCE TO PUPIL
              VALUE1=((-PXTRAX(5,INT(W1A)))/(PXTRAX(6,INT(W1B))))
              RETURN
          END IF
C
C       GET PUPDIA(X OR Y)
          IF(WQ.EQ.'PUPDIAY') THEN
C       V1 IS DISTANCE TO PUPIL, VALUE IS MARG RAY HEIGHT THERE.
              V1=((-PXTRAY(5,INT(W1A)))/(PXTRAY(6,INT(W1B))))
              VALUE1=2.0D0*(V1*PXTRAY(2,INT(W1B)))+PXTRAY(1,INT(W1A))
              RETURN
          END IF
          IF(WQ.EQ.'PUPDIAX') THEN
C       V1 IS DISTANCE TO PUPIL, VALUE IS MARG RAY HEIGHT THERE.
              V1=((-PXTRAX(5,INT(W1A)))/(PXTRAX(6,INT(W1B))))
              VALUE1=2.0D0*(V1*PXTRAX(2,INT(W1B)))+PXTRAX(1,INT(W1A))
              RETURN
          END IF
C
          VALUE1=0.0D0
          SF=INT(SYSTEM1(20))
          IF(INT(SYSTEM1(11)).GE.1.AND.INT(SYSTEM1(11)).LE.5) THEN
              CW=INT(SYSTEM1(11))+45
          END IF
          IF(INT(SYSTEM1(11)).GE.6.AND.INT(SYSTEM1(11)).LE.10) THEN
              CW=INT(SYSTEM1(11))+65
          END IF
          INTV=1.0D0
          IF(SYSTEM1(30).EQ.1.0D0.OR.SYSTEM1(30).EQ.3.0D0) THEN
C       CALCULATE INTV
              IF(WQ.EQ.'PACX'.OR.WQ.EQ.'PLCX'.OR.WQ.EQ.'SACX'
     1        .OR.WQ.EQ.'SLCX') THEN
                  INTV=((PXTRAX(5,SF)*ALENS(CW,(SF-1))*PXTRAX(2,(SF-1)))-
     1            (PXTRAX(1,SF)*ALENS(CW,(SF-1))*PXTRAX(6,(SF-1))))
              END IF
              IF(WQ.EQ.'PACY'.OR.WQ.EQ.'PLCY'.OR.WQ.EQ.'SACY'
     1        .OR.WQ.EQ.'SLCY') THEN
                  INTV=((PXTRAY(5,SF)*ALENS(CW,(SF-1))*PXTRAY(2,(SF-1)))-
     1            (PXTRAY(1,SF)*ALENS(CW,(SF-1))*PXTRAY(6,(SF-1))))
              END IF
          END IF
C
          INV=1.0D0
          IF(SYSTEM1(30).EQ.1.0D0) THEN
C       MODE IS FOCAL
              IF(WQ(1:1).NE.'X')
     1        INV=-2.0*ALENS(CW,(SF-1))*PXTRAY(2,(SF-1))
              IF(WQ(1:1).EQ.'X')
     1        INV=-2.0*ALENS(CW,(SF-1))*PXTRAX(2,(SF-1))
          END IF
          IF(SYSTEM1(30).EQ.3.0D0) THEN
C       MODE IS AFOCAL
              IF(WQ(1:1).NE.'X')
     1        INV= 2.0*ALENS(CW,(SF-1))*PXTRAY(1,SF)
              IF(WQ(1:1).EQ.'X')
     1        INV= 2.0*ALENS(CW,(SF-1))*PXTRAX(1,SF)
          END IF
          IF(INV.EQ.0.0D0) THEN
              WRITE(OUTLYNE,*)
     1        'THE LENS (MODE) IS NOT CONSISTENT WITH PARAXIAL VALUE'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)
     1        'ABERRATIONS ARE NOT CALCULABLE'
              CALL SHOWIT(1)
              IF(SYSTEM1(30).EQ.1.0D0)
     1        WRITE(OUTLYNE,*)'CHANGE FROM "MODE FOCAL" TO "MODE AFOCAL"'
              IF(SYSTEM1(30).EQ.3.0D0)
     1        WRITE(OUTLYNE,*)'CHANGE FROM "MODE AFOCAL" TO "MODE FOCAL"'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'THEN RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(DF1.EQ.0) THEN
C       A SPECIFIC SURFACE HAS BEEN REQUESTED
C       3RD, 5TH AND 7TH ORDER ABERRATIONS
              IF(WQ.EQ.'SA3')       VALUE1=MAB3(1,INT(W1))
              IF(WQ.EQ.'XSA3')      VALUE1=XMAB3(1,INT(W1))
              IF(WQ.EQ.'CMA3')      VALUE1=3.0D0*MAB3(2,INT(W1))
              IF(WQ.EQ.'XCMA3')     VALUE1=3.0D0*XMAB3(2,INT(W1))
              IF(WQ.EQ.'AST3')      VALUE1=MAB3(3,INT(W1))
              IF(WQ.EQ.'XAST3')     VALUE1=XMAB3(3,INT(W1))
              IF(WQ.EQ.'DIS3')      VALUE1=MAB3(4,INT(W1))
              IF(WQ.EQ.'XDIS3')     VALUE1=XMAB3(4,INT(W1))
              IF(WQ.EQ.'PTZ3')      VALUE1=MAB3(5,INT(W1))
              IF(WQ.EQ.'XPTZ3')     VALUE1=XMAB3(5,INT(W1))
              IF(WQ.EQ.'PTZCV')     VALUE1=MAB3(11,INT(W1))
              IF(WQ.EQ.'XPTZCV')    VALUE1=XMAB3(11,INT(W1))
              IF(WQ.EQ.'SA5')       VALUE1=MAB57(1,INT(W1))
              IF(WQ.EQ.'XSA5')      VALUE1=XMAB57(1,INT(W1))
              IF(WQ.EQ.'CMA5')      VALUE1=MAB57(2,INT(W1))+MAB57(3,INT(W1))
              IF(WQ.EQ.'XCMA5')     VALUE1=XMAB57(2,INT(W1))+XMAB57(3,INT(W1))
              IF(WQ.EQ.'AST5')      VALUE1=MAB57(10,INT(W1))
              IF(WQ.EQ.'XAST5')     VALUE1=XMAB57(10,INT(W1))
              IF(WQ.EQ.'DIS5')      VALUE1=MAB57(12,INT(W1))
              IF(WQ.EQ.'XDIS5')     VALUE1=XMAB57(12,INT(W1))
              IF(WQ.EQ.'PTZ5')      VALUE1=MAB57(11,INT(W1))
              IF(WQ.EQ.'XPTZ5')     VALUE1=XMAB57(11,INT(W1))
              IF(WQ.EQ.'TOBSA')     VALUE1=MAB57(4,INT(W1))+
     1        MAB57(5,INT(W1))+MAB57(6,INT(W1))
              IF(WQ.EQ.'XTOBSA')    VALUE1=XMAB57(4,INT(W1))+
     1        XMAB57(5,INT(W1))+XMAB57(6,INT(W1))
              IF(WQ.EQ.'SOBSA')     VALUE1=MAB57(5,INT(W1))
              IF(WQ.EQ.'XSOBSA')    VALUE1=XMAB57(5,INT(W1))
              IF(WQ.EQ.'ELCMA')     VALUE1=MAB57(7,INT(W1))+
     1        MAB57(8,INT(W1))
              IF(WQ.EQ.'XELCMA')    VALUE1=XMAB57(7,INT(W1))+
     1        XMAB57(8,INT(W1))
              IF(WQ.EQ.'TAS')       VALUE1=MAB57(10,INT(W1))+
     1        (5.0*MAB57(11,INT(W1)))
              IF(WQ.EQ.'XTAS')      VALUE1=XMAB57(10,INT(W1))+
     1        (5.0*XMAB57(11,INT(W1)))
              IF(WQ.EQ.'SAS')       VALUE1=MAB57(10,INT(W1))+
     1        MAB57(11,INT(W1))
              IF(WQ.EQ.'XSAS')      VALUE1=XMAB57(10,INT(W1))+
     1        XMAB57(11,INT(W1))
              IF(WQ.EQ.'SA7')       VALUE1=MAB57(14,INT(W1))
              IF(WQ.EQ.'XSA7')      VALUE1=XMAB57(14,INT(W1))
C       PRIMARY CHROMATIC ABERRATION DIFFERENCES
              IF(WQ.EQ.'SA3P')       VALUE1=PDF3(1,INT(W1))
              IF(WQ.EQ.'XSA3P')      VALUE1=XPDF3(1,INT(W1))
              IF(WQ.EQ.'CMA3P')      VALUE1=3.0D0*PDF3(2,INT(W1))
              IF(WQ.EQ.'XCMA3P')     VALUE1=3.0D0*XPDF3(2,INT(W1))
              IF(WQ.EQ.'AST3P')      VALUE1=PDF3(3,INT(W1))
              IF(WQ.EQ.'XAST3P')     VALUE1=XPDF3(3,INT(W1))
              IF(WQ.EQ.'DIS3P')      VALUE1=PDF3(4,INT(W1))
              IF(WQ.EQ.'XDIS3P')     VALUE1=XPDF3(4,INT(W1))
              IF(WQ.EQ.'PTZ3P')      VALUE1=PDF3(5,INT(W1))
              IF(WQ.EQ.'XPTZ3P')     VALUE1=XPDF3(5,INT(W1))
              IF(WQ.EQ.'SA5P')       VALUE1=PDF57(1,INT(W1))
              IF(WQ.EQ.'XSA5P')      VALUE1=XPDF57(1,INT(W1))
              IF(WQ.EQ.'CMA5P')      VALUE1=PDF57(2,INT(W1))+PDF57(3,INT(W1))
              IF(WQ.EQ.'XCMA5P')    VALUE1=XPDF57(2,INT(W1))+XPDF57(3,INT(W1))
              IF(WQ.EQ.'AST5P')      VALUE1=PDF57(10,INT(W1))
              IF(WQ.EQ.'XAST5P')     VALUE1=XPDF57(10,INT(W1))
              IF(WQ.EQ.'DIS5P')      VALUE1=PDF57(12,INT(W1))
              IF(WQ.EQ.'XDIS5P')     VALUE1=XPDF57(12,INT(W1))
              IF(WQ.EQ.'PTZ5P')      VALUE1=PDF57(11,INT(W1))
              IF(WQ.EQ.'XPTZ5P')     VALUE1=XPDF57(11,INT(W1))
              IF(WQ.EQ.'TOBSAP')     VALUE1=PDF57(4,INT(W1))+
     1        PDF57(5,INT(W1))+PDF57(6,INT(W1))
              IF(WQ.EQ.'XTOBSAP')    VALUE1=XPDF57(4,INT(W1))+
     1        XPDF57(5,INT(W1))+XPDF57(6,INT(W1))
              IF(WQ.EQ.'SOBSAP')     VALUE1=PDF57(5,INT(W1))
              IF(WQ.EQ.'XSOBSAP')    VALUE1=XPDF57(5,INT(W1))
              IF(WQ.EQ.'ELCMAP')     VALUE1=PDF57(7,INT(W1))+
     1        PDF57(8,INT(W1))
              IF(WQ.EQ.'XELCMAP')    VALUE1=XPDF57(7,INT(W1))+
     1        XPDF57(8,INT(W1))
              IF(WQ.EQ.'TASP')       VALUE1=PDF57(10,INT(W1))+
     1        (5.0*PDF57(11,INT(W1)))
              IF(WQ.EQ.'XTASP')      VALUE1=XPDF57(10,INT(W1))+
     1        (5.0*XPDF57(11,INT(W1)))
              IF(WQ.EQ.'SASP')       VALUE1=PDF57(10,INT(W1))+
     1        PDF57(11,INT(W1))
              IF(WQ.EQ.'XSASP')      VALUE1=XPDF57(10,INT(W1))+
     1        XPDF57(11,INT(W1))
              IF(WQ.EQ.'SA7P')       VALUE1=PDF57(14,INT(W1))
              IF(WQ.EQ.'XSA7P')      VALUE1=XPDF57(14,INT(W1))
C       SECONDARY CHROMATIC ABERRATION DIFFERENCES
              IF(WQ.EQ.'SA3S')       VALUE1=SDF3(1,INT(W1))
              IF(WQ.EQ.'XSA3S')      VALUE1=XSDF3(1,INT(W1))
              IF(WQ.EQ.'CMA3S')      VALUE1=3.0D0*SDF3(2,INT(W1))
              IF(WQ.EQ.'XCMA3S')     VALUE1=3.0D0*XSDF3(2,INT(W1))
              IF(WQ.EQ.'AST3S')      VALUE1=SDF3(3,INT(W1))
              IF(WQ.EQ.'XAST3S')     VALUE1=XSDF3(3,INT(W1))
              IF(WQ.EQ.'DIS3S')      VALUE1=SDF3(4,INT(W1))
              IF(WQ.EQ.'XDIS3S')     VALUE1=XSDF3(4,INT(W1))
              IF(WQ.EQ.'PTZ3S')      VALUE1=SDF3(5,INT(W1))
              IF(WQ.EQ.'XPTZ3S')     VALUE1=XSDF3(5,INT(W1))
              IF(WQ.EQ.'SA5S')       VALUE1=SDF57(1,INT(W1))
              IF(WQ.EQ.'XSA5S')      VALUE1=XSDF57(1,INT(W1))
              IF(WQ.EQ.'CMA5S')      VALUE1=SDF57(2,INT(W1))+SDF57(3,INT(W1))
              IF(WQ.EQ.'XCMA5S')    VALUE1=XSDF57(2,INT(W1))+XSDF57(3,INT(W1))
              IF(WQ.EQ.'AST5S')      VALUE1=SDF57(10,INT(W1))
              IF(WQ.EQ.'XAST5S')     VALUE1=XSDF57(10,INT(W1))
              IF(WQ.EQ.'DIS5S')      VALUE1=SDF57(12,INT(W1))
              IF(WQ.EQ.'XDIS5S')     VALUE1=XSDF57(12,INT(W1))
              IF(WQ.EQ.'PTZ5S')      VALUE1=SDF57(11,INT(W1))
              IF(WQ.EQ.'XPTZ5S')     VALUE1=XSDF57(11,INT(W1))
              IF(WQ.EQ.'TOBSAS')     VALUE1=SDF57(4,INT(W1))+
     1        SDF57(5,INT(W1))+SDF57(6,INT(W1))
              IF(WQ.EQ.'XTOBSAS')    VALUE1=XSDF57(4,INT(W1))+
     1        XSDF57(5,INT(W1))+XSDF57(6,INT(W1))
              IF(WQ.EQ.'SOBSAS')     VALUE1=SDF57(5,INT(W1))
              IF(WQ.EQ.'XSOBSAS')    VALUE1=XSDF57(5,INT(W1))
              IF(WQ.EQ.'ELCMAS')     VALUE1=SDF57(7,INT(W1))+
     1        SDF57(8,INT(W1))
              IF(WQ.EQ.'XELCMAS')    VALUE1=XSDF57(7,INT(W1))+
     1        XSDF57(8,INT(W1))
              IF(WQ.EQ.'TASS')       VALUE1=SDF57(10,INT(W1))+
     1        (5.0*SDF57(11,INT(W1)))
              IF(WQ.EQ.'XTASS')      VALUE1=XSDF57(10,INT(W1))+
     1        (5.0*XSDF57(11,INT(W1)))
              IF(WQ.EQ.'SASS')       VALUE1=SDF57(10,INT(W1))+
     1        SDF57(11,INT(W1))
              IF(WQ.EQ.'XSASS')      VALUE1=XSDF57(10,INT(W1))+
     1        XSDF57(11,INT(W1))
              IF(WQ.EQ.'SA7S')       VALUE1=SDF57(14,INT(W1))
              IF(WQ.EQ.'XSA7S')      VALUE1=XSDF57(14,INT(W1))
C       5TH AND 7TH ORDER INTRINSIC SURFACE ABERRATIONS
              IF(WQ.EQ.'SA5I')       VALUE1=SAB57(1,INT(W1))
              IF(WQ.EQ.'XSA5I')      VALUE1=XSAB57(1,INT(W1))
              IF(WQ.EQ.'CMA5I')      VALUE1=SAB57(2,INT(W1))+SAB57(3,INT(W1))
              IF(WQ.EQ.'XCMA5I')    VALUE1=XSAB57(2,INT(W1))+XSAB57(3,INT(W1))
              IF(WQ.EQ.'AST5I')      VALUE1=SAB57(10,INT(W1))
              IF(WQ.EQ.'XAST5I')     VALUE1=XSAB57(10,INT(W1))
              IF(WQ.EQ.'DIS5I')      VALUE1=SAB57(12,INT(W1))
              IF(WQ.EQ.'XDIS5I')     VALUE1=XSAB57(12,INT(W1))
              IF(WQ.EQ.'PTZ5I')      VALUE1=SAB57(11,INT(W1))
              IF(WQ.EQ.'XPTZ5I')     VALUE1=XSAB57(11,INT(W1))
              IF(WQ.EQ.'TOBSAI')     VALUE1=SAB57(4,INT(W1))+
     1        SAB57(5,INT(W1))+SAB57(6,INT(W1))
              IF(WQ.EQ.'XTOBSAI')    VALUE1=XSAB57(4,INT(W1))+
     1        XSAB57(5,INT(W1))+XSAB57(6,INT(W1))
              IF(WQ.EQ.'SOBSAI')     VALUE1=SAB57(5,INT(W1))
              IF(WQ.EQ.'XSOBSAI')    VALUE1=XSAB57(5,INT(W1))
              IF(WQ.EQ.'ELCMAI')     VALUE1=SAB57(7,INT(W1))+
     1        SAB57(8,INT(W1))
              IF(WQ.EQ.'XELCMAI')    VALUE1=XSAB57(7,INT(W1))+
     1        XSAB57(8,INT(W1))
              IF(WQ.EQ.'TASI')       VALUE1=SAB57(10,INT(W1))+
     1        (5.0*SAB57(11,INT(W1)))
              IF(WQ.EQ.'XTASI')      VALUE1=XSAB57(10,INT(W1))+
     1        (5.0*XSAB57(11,INT(W1)))
              IF(WQ.EQ.'SASI')       VALUE1=SAB57(10,INT(W1))+
     1        SAB57(11,INT(W1))
              IF(WQ.EQ.'XSASI')      VALUE1=XSAB57(10,INT(W1))+
     1        XSAB57(11,INT(W1))
              IF(WQ.EQ.'SA7I')       VALUE1=SAB57(14,INT(W1))
              IF(WQ.EQ.'XSA7I')      VALUE1=XSAB57(14,INT(W1))
C       3RD ORDER PUPIL ABERRATIONS
              IF(WQ.EQ.'PSA3')       VALUE1=MAB3(6,INT(W1))
              IF(WQ.EQ.'XPSA3')      VALUE1=XMAB3(6,INT(W1))
              IF(WQ.EQ.'PCMA3')      VALUE1=3.0D0*MAB3(7,INT(W1))
              IF(WQ.EQ.'XPCMA3')     VALUE1=3.0D0*XMAB3(7,INT(W1))
              IF(WQ.EQ.'PAST3')      VALUE1=MAB3(8,INT(W1))
              IF(WQ.EQ.'XPAST3')     VALUE1=XMAB3(8,INT(W1))
              IF(WQ.EQ.'PDIS3')      VALUE1=MAB3(9,INT(W1))
              IF(WQ.EQ.'XPDIS3')     VALUE1=XMAB3(9,INT(W1))
              IF(WQ.EQ.'PPTZ3')      VALUE1=MAB3(10,INT(W1))
              IF(WQ.EQ.'XPPTZ3')     VALUE1=XMAB3(10,INT(W1))
C       3RD ORDER PUPIL ABERRATION PRIMARY CHROMATIC DIFFERENCES
              IF(WQ.EQ.'PSA3P')       VALUE1=PDF3(6,INT(W1))
              IF(WQ.EQ.'XPSA3P')      VALUE1=XPDF3(6,INT(W1))
              IF(WQ.EQ.'PCMA3P')      VALUE1=3.0D0*PDF3(7,INT(W1))
              IF(WQ.EQ.'XPCMA3P')     VALUE1=3.0D0*XPDF3(7,INT(W1))
              IF(WQ.EQ.'PAST3P')      VALUE1=PDF3(8,INT(W1))
              IF(WQ.EQ.'XPAST3P')     VALUE1=XPDF3(8,INT(W1))
              IF(WQ.EQ.'PDIS3P')      VALUE1=PDF3(9,INT(W1))
              IF(WQ.EQ.'XPDIS3P')     VALUE1=XPDF3(9,INT(W1))
              IF(WQ.EQ.'PPTZ3P')      VALUE1=PDF3(10,INT(W1))
              IF(WQ.EQ.'XPPTZ3P')     VALUE1=XPDF3(10,INT(W1))
C       3RD ORDER PUPIL ABERRATION SECONDARY CHROMATIC DIFFERENCES
              IF(WQ.EQ.'PSA3S')       VALUE1=SDF3(6,INT(W1))
              IF(WQ.EQ.'XPSA3S')      VALUE1=XSDF3(6,INT(W1))
              IF(WQ.EQ.'PCMA3S')      VALUE1=3.0D0*SDF3(7,INT(W1))
              IF(WQ.EQ.'XPCMA3S')     VALUE1=3.0D0*XSDF3(7,INT(W1))
              IF(WQ.EQ.'PAST3S')      VALUE1=SDF3(8,INT(W1))
              IF(WQ.EQ.'XPAST3S')     VALUE1=XSDF3(8,INT(W1))
              IF(WQ.EQ.'PDIS3S')      VALUE1=SDF3(9,INT(W1))
              IF(WQ.EQ.'XPDIS3S')     VALUE1=XSDF3(9,INT(W1))
              IF(WQ.EQ.'PPTZ3S')      VALUE1=SDF3(10,INT(W1))
              IF(WQ.EQ.'XPPTZ3S')     VALUE1=XSDF3(10,INT(W1))
              IF(WQ.EQ.'PTZCV'.OR.WQ.EQ.'XPTZCV') THEN
                  VALUE1=VALUE1
              ELSE
                  VALUE1=VALUE1/INV
              END IF

              IF(SYSTEM1(30).LE.2.0D0) THEN
                  IF(WQ.EQ.'PACY') THEN
                      VALUE1=COLORY(1,INT(W1))/INTV
                  END IF
                  IF(WQ.EQ.'PLCY') THEN
                      VALUE1=COLORY(2,INT(W1))/INTV
                  END IF
                  IF(WQ.EQ.'SACY') THEN
                      VALUE1=COLORY(3,INT(W1))/INTV
                  END IF
                  IF(WQ.EQ.'SLCY') THEN
                      VALUE1=COLORY(4,INT(W1))/INTV
                  END IF
                  IF(WQ.EQ.'PACX') THEN
                      VALUE1=COLORX(1,INT(W1))/INTV
                  END IF
                  IF(WQ.EQ.'PLCX') THEN
                      VALUE1=COLORX(2,INT(W1))/INTV
                  END IF
                  IF(WQ.EQ.'SACX') THEN
                      VALUE1=COLORX(3,INT(W1))/INTV
                  END IF
                  IF(WQ.EQ.'SLCX') THEN
                      VALUE1=COLORX(4,INT(W1))/INTV
                  END IF
              ELSE
                  IF(WQ.EQ.'PACY') THEN
                      VALUE1=COLORY(5,INT(W1))/INTV
                  END IF
                  IF(WQ.EQ.'PLCY') THEN
                      VALUE1=COLORY(6,INT(W1))/INTV
                  END IF
                  IF(WQ.EQ.'SACY') THEN
                      VALUE1=COLORY(7,INT(W1))/INTV
                  END IF
                  IF(WQ.EQ.'SLCY') THEN
                      VALUE1=COLORY(8,INT(W1))/INTV
                  END IF
                  IF(WQ.EQ.'PACX') THEN
                      VALUE1=COLORX(5,INT(W1))/INTV
                  END IF
                  IF(WQ.EQ.'PLCX') THEN
                      VALUE1=COLORX(6,INT(W1))/INTV
                  END IF
                  IF(WQ.EQ.'SACX') THEN
                      VALUE1=COLORX(7,INT(W1))/INTV
                  END IF
                  IF(WQ.EQ.'SLCX') THEN
                      VALUE1=COLORX(8,INT(W1))/INTV
                  END IF
              END IF
          ELSE
C       DO THE TOTALS FOR ALL SURFACES
C       3RD, 5TH AND 7TH ORDER ABERRATIONS
              V=0.0D0
              IF(WQ.EQ.'SA3') THEN
                  DO I=0,INT(SYSTEM1(20))
                      V=V+MAB3(1,I)
                  END DO
              END IF
              IF(WQ.EQ.'XSA3') THEN
                  DO I=0,INT(SYSTEM1(20))
                      V=V+XMAB3(1,I)
                  END DO
              END IF
              IF(WQ.EQ.'CMA3') THEN
                  DO I=0,INT(SYSTEM1(20))
                      V=V+(3.0D0*MAB3(2,I))
                  END DO
              END IF
              IF(WQ.EQ.'XCMA3') THEN
                  DO I=0,INT(SYSTEM1(20))
                      V=V+(3.0D0*XMAB3(2,I))
                  END DO
              END IF
              IF(WQ.EQ.'AST3') THEN
                  DO I=0,INT(SYSTEM1(20))
                      V=V+MAB3(3,I)
                  END DO
              END IF
              IF(WQ.EQ.'XAST3') THEN
                  DO I=0,INT(SYSTEM1(20))
                      V=V+XMAB3(3,I)
                  END DO
              END IF
              IF(WQ.EQ.'DIS3') THEN
                  DO I=0,INT(SYSTEM1(20))
                      V=V+MAB3(4,I)
                  END DO
              END IF
              IF(WQ.EQ.'XDIS3') THEN
                  DO I=0,INT(SYSTEM1(20))
                      V=V+XMAB3(4,I)
                  END DO
              END IF
              IF(WQ.EQ.'PTZ3') THEN
                  DO I=0,INT(SYSTEM1(20))
                      V=V+MAB3(5,I)
                  END DO
              END IF
              IF(WQ.EQ.'XPTZ3') THEN
                  DO I=0,INT(SYSTEM1(20))
                      V=V+XMAB3(5,I)
                  END DO
              END IF
              IF(WQ.EQ.'PTZCV') THEN
                  DO I=0,INT(SYSTEM1(20))
                      V=V+MAB3(11,I)
                  END DO
                  RETURN
              END IF
              IF(WQ.EQ.'XPTZCV') THEN
                  DO I=0,INT(SYSTEM1(20))
                      V=V+XMAB3(11,I)
                  END DO
                  RETURN
              END IF
              IF(WQ.EQ.'SA5') THEN
                  DO I=0,INT(SYSTEM1(20))
                      V=V+MAB57(1,I)
                  END DO
              END IF
              IF(WQ.EQ.'XSA5') THEN
                  DO I=0,INT(SYSTEM1(20))
                      V=V+XMAB57(1,I)
                  END DO
              END IF
              IF(WQ.EQ.'CMA5') THEN
                  DO I=0,INT(SYSTEM1(20))
                      V=V+MAB57(2,I)+MAB57(3,I)
                  END DO
              END IF
              IF(WQ.EQ.'XCMA5') THEN
                  DO I=0,INT(SYSTEM1(20))
                      V=V+XMAB57(2,I)+XMAB57(3,I)
                  END DO
              END IF
              IF(WQ.EQ.'AST5') THEN
                  DO I=0,INT(SYSTEM1(20))
                      V=V+MAB57(10,I)
                  END DO
              END IF
              IF(WQ.EQ.'XAST5') THEN
                  DO I=0,INT(SYSTEM1(20))
                      V=V+XMAB57(10,I)
                  END DO
              END IF
              IF(WQ.EQ.'DIS5') THEN
                  DO I=0,INT(SYSTEM1(20))
                      V=V+MAB57(12,I)
                  END DO
              END IF
              IF(WQ.EQ.'XDIS5') THEN
                  DO I=0,INT(SYSTEM1(20))
                      V=V+XMAB57(12,I)
                  END DO
              END IF
              IF(WQ.EQ.'PTZ5') THEN
                  DO I=0,INT(SYSTEM1(20))
                      V=V+MAB57(11,I)
                  END DO
              END IF
              IF(WQ.EQ.'XPTZ5') THEN
                  DO I=0,INT(SYSTEM1(20))
                      V=V+XMAB57(11,I)
                  END DO
              END IF
              IF(WQ.EQ.'TOBSA') THEN
                  DO I=0,INT(SYSTEM1(20))
                      V=V+MAB57(4,I)+
     1                MAB57(5,I)+MAB57(6,I)
                  END DO
              END IF
              IF(WQ.EQ.'XTOBSA') THEN
                  DO I=0,INT(SYSTEM1(20))
                      V=V+XMAB57(4,I)+
     1                XMAB57(5,I)+XMAB57(6,I)
                  END DO
              END IF
              IF(WQ.EQ.'SOBSA') THEN
                  DO I=0,INT(SYSTEM1(20))
                      V=V+MAB57(5,I)
                  END DO
              END IF
              IF(WQ.EQ.'XSOBSA') THEN
                  DO I=0,INT(SYSTEM1(20))
                      V=V+XMAB57(5,I)
                  END DO
              END IF
              IF(WQ.EQ.'ELCMA') THEN
                  DO I=0,INT(SYSTEM1(20))
                      V=V+MAB57(7,I)+
     1                MAB57(8,I)
                  END DO
              END IF
              IF(WQ.EQ.'XELCMA') THEN
                  DO I=0,INT(SYSTEM1(20))
                      V=V+XMAB57(7,I)+
     1                XMAB57(8,I)
                  END DO
              END IF
              IF(WQ.EQ.'TAS') THEN
                  DO I=0,INT(SYSTEM1(20))
                      V=V+MAB57(10,I)+
     1                (5.0*MAB57(11,I))
                  END DO
              END IF
              IF(WQ.EQ.'XTAS') THEN
                  DO I=0,INT(SYSTEM1(20))
                      V=V+XMAB57(10,I)+
     1                (5.0*XMAB57(11,I))
                  END DO
              END IF
              IF(WQ.EQ.'SAS') THEN
                  DO I=0,INT(SYSTEM1(20))
                      V=V+MAB57(10,I)+
     1                MAB57(11,I)
                  END DO
              END IF
              IF(WQ.EQ.'XSAS') THEN
                  DO I=0,INT(SYSTEM1(20))
                      V=V+XMAB57(10,I)+
     1                XMAB57(11,I)
                  END DO
              END IF
              IF(WQ.EQ.'SA7') THEN
                  DO I=0,INT(SYSTEM1(20))
                      V=V+MAB57(14,I)
                  END DO
              END IF
              IF(WQ.EQ.'XSA7') THEN
                  DO I=0,INT(SYSTEM1(20))
                      V=V+XMAB57(14,I)
                  END DO
              END IF
C       PRIMARY CHROMATIC ABERRATION DIFFERENCES
              IF(WQ.EQ.'SA3P') THEN
                  DO I=0,INT(SYSTEM1(20))
                      V=V+PDF3(1,I)
                  END DO
              END IF
              IF(WQ.EQ.'XSA3P') THEN
                  DO I=0,INT(SYSTEM1(20))
                      V=V+XPDF3(1,I)
                  END DO
              END IF
              IF(WQ.EQ.'CMA3P') THEN
                  DO I=0,INT(SYSTEM1(20))
                      V=V+(3.0D0*PDF3(2,I))
                  END DO
              END IF
              IF(WQ.EQ.'XCMA3P') THEN
                  DO I=0,INT(SYSTEM1(20))
                      V=V+(3.0D0*XPDF3(2,I))
                  END DO
              END IF
              IF(WQ.EQ.'AST3P') THEN
                  DO I=0,INT(SYSTEM1(20))
                      V=V+PDF3(3,I)
                  END DO
              END IF
              IF(WQ.EQ.'XAST3P') THEN
                  DO I=0,INT(SYSTEM1(20))
                      V=V+XPDF3(3,I)
                  END DO
              END IF
              IF(WQ.EQ.'DIS3P') THEN
                  DO I=0,INT(SYSTEM1(20))
                      V=V+PDF3(4,I)
                  END DO
              END IF
              IF(WQ.EQ.'XDIS3P') THEN
                  DO I=0,INT(SYSTEM1(20))
                      V=V+XPDF3(4,I)
                  END DO
              END IF
              IF(WQ.EQ.'PTZ3P') THEN
                  DO I=0,INT(SYSTEM1(20))
                      V=V+PDF3(5,I)
                  END DO
              END IF
              IF(WQ.EQ.'XPTZ3P') THEN
                  DO I=0,INT(SYSTEM1(20))
                      V=V+XPDF3(5,I)
                  END DO
              END IF
              IF(WQ.EQ.'SA5P') THEN
                  DO I=0,INT(SYSTEM1(20))
                      V=V+PDF57(1,I)
                  END DO
              END IF
              IF(WQ.EQ.'XSA5P') THEN
                  DO I=0,INT(SYSTEM1(20))
                      V=V+XPDF57(1,I)
                  END DO
              END IF
              IF(WQ.EQ.'CMA5P') THEN
                  DO I=0,INT(SYSTEM1(20))
                      V=V+PDF57(2,I)+PDF57(3,I)
                  END DO
              END IF
              IF(WQ.EQ.'XCMA5P') THEN
                  DO I=0,INT(SYSTEM1(20))
                      V=V+XPDF57(2,I)+XPDF57(3,I)
                  END DO
              END IF
              IF(WQ.EQ.'AST5P') THEN
                  DO I=0,INT(SYSTEM1(20))
                      V=V+PDF57(10,I)
                  END DO
              END IF
              IF(WQ.EQ.'XAST5P') THEN
                  DO I=0,INT(SYSTEM1(20))
                      V=V+XPDF57(10,I)
                  END DO
              END IF
              IF(WQ.EQ.'DIS5P') THEN
                  DO I=0,INT(SYSTEM1(20))
                      V=V+PDF57(12,I)
                  END DO
              END IF
              IF(WQ.EQ.'XDIS5P') THEN
                  DO I=0,INT(SYSTEM1(20))
                      V=V+XPDF57(12,I)
                  END DO
              END IF
              IF(WQ.EQ.'PTZ5P') THEN
                  DO I=0,INT(SYSTEM1(20))
                      V=V+PDF57(11,I)
                  END DO
              END IF
              IF(WQ.EQ.'XPTZ5P') THEN
                  DO I=0,INT(SYSTEM1(20))
                      V=V+XPDF57(11,I)
                  END DO
              END IF
              IF(WQ.EQ.'TOBSAP') THEN
                  DO I=0,INT(SYSTEM1(20))
                      V=V+PDF57(4,I)+
     1                PDF57(5,I)+PDF57(6,I)
                  END DO
              END IF
              IF(WQ.EQ.'XTOBSAP') THEN
                  DO I=0,INT(SYSTEM1(20))
                      V=V+XPDF57(4,I)+
     1                XPDF57(5,I)+XPDF57(6,I)
                  END DO
              END IF
              IF(WQ.EQ.'SOBSAP') THEN
                  DO I=0,INT(SYSTEM1(20))
                      V=V+PDF57(5,I)
                  END DO
              END IF
              IF(WQ.EQ.'XSOBSAP') THEN
                  DO I=0,INT(SYSTEM1(20))
                      V=V+XPDF57(5,I)
                  END DO
              END IF
              IF(WQ.EQ.'ELCMAP') THEN
                  DO I=0,INT(SYSTEM1(20))
                      V=V+PDF57(7,I)+
     1                PDF57(8,I)
                  END DO
              END IF
              IF(WQ.EQ.'XELCMAP') THEN
                  DO I=0,INT(SYSTEM1(20))
                      V=V+XPDF57(7,I)+
     1                XPDF57(8,I)
                  END DO
              END IF
              IF(WQ.EQ.'TASP') THEN
                  DO I=0,INT(SYSTEM1(20))
                      V=V+PDF57(10,I)+
     1                (5.0*PDF57(11,I))
                  END DO
              END IF
              IF(WQ.EQ.'XTASP') THEN
                  DO I=0,INT(SYSTEM1(20))
                      V=V+XPDF57(10,I)+
     1                (5.0*XPDF57(11,I))
                  END DO
              END IF
              IF(WQ.EQ.'SASP') THEN
                  DO I=0,INT(SYSTEM1(20))
                      V=V+PDF57(10,I)+
     1                PDF57(11,I)
                  END DO
              END IF
              IF(WQ.EQ.'XSASP') THEN
                  DO I=0,INT(SYSTEM1(20))
                      V=V+XPDF57(10,I)+
     1                XPDF57(11,I)
                  END DO
              END IF
              IF(WQ.EQ.'SA7P') THEN
                  DO I=0,INT(SYSTEM1(20))
                      V=V+PDF57(14,I)
                  END DO
              END IF
              IF(WQ.EQ.'XSA7P') THEN
                  DO I=0,INT(SYSTEM1(20))
                      V=V+XPDF57(14,I)
                  END DO
              END IF
C       SECONDARY CHROMATIC ABERRATION DIFFERENCES
              IF(WQ.EQ.'SA3S') THEN
                  DO I=0,INT(SYSTEM1(20))
                      V=V+SDF3(1,I)
                  END DO
              END IF
              IF(WQ.EQ.'XSA3S') THEN
                  DO I=0,INT(SYSTEM1(20))
                      V=V+XSDF3(1,I)
                  END DO
              END IF
              IF(WQ.EQ.'CMA3S') THEN
                  DO I=0,INT(SYSTEM1(20))
                      V=V+(3.0D0*SDF3(2,I))
                  END DO
              END IF
              IF(WQ.EQ.'XCMA3S') THEN
                  DO I=0,INT(SYSTEM1(20))
                      V=V+(3.0D0*XSDF3(2,I))
                  END DO
              END IF
              IF(WQ.EQ.'AST3S') THEN
                  DO I=0,INT(SYSTEM1(20))
                      V=V+SDF3(3,I)
                  END DO
              END IF
              IF(WQ.EQ.'XAST3S') THEN
                  DO I=0,INT(SYSTEM1(20))
                      V=V+XSDF3(3,I)
                  END DO
              END IF
              IF(WQ.EQ.'DIS3S') THEN
                  DO I=0,INT(SYSTEM1(20))
                      V=V+SDF3(4,I)
                  END DO
              END IF
              IF(WQ.EQ.'XDIS3S') THEN
                  DO I=0,INT(SYSTEM1(20))
                      V=V+XSDF3(4,I)
                  END DO
              END IF
              IF(WQ.EQ.'PTZ3S') THEN
                  DO I=0,INT(SYSTEM1(20))
                      V=V+SDF3(5,I)
                  END DO
              END IF
              IF(WQ.EQ.'XPTZ3S') THEN
                  DO I=0,INT(SYSTEM1(20))
                      V=V+XSDF3(5,I)
                  END DO
              END IF
              IF(WQ.EQ.'SA5S') THEN
                  DO I=0,INT(SYSTEM1(20))
                      V=V+SDF57(1,I)
                  END DO
              END IF
              IF(WQ.EQ.'XSA5S') THEN
                  DO I=0,INT(SYSTEM1(20))
                      V=V+XSDF57(1,I)
                  END DO
              END IF
              IF(WQ.EQ.'CMA5S') THEN
                  DO I=0,INT(SYSTEM1(20))
                      V=V+SDF57(2,I)+SDF57(3,I)
                  END DO
              END IF
              IF(WQ.EQ.'XCMA5S') THEN
                  DO I=0,INT(SYSTEM1(20))
                      V=V+XSDF57(2,I)+XSDF57(3,I)
                  END DO
              END IF
              IF(WQ.EQ.'AST5S') THEN
                  DO I=0,INT(SYSTEM1(20))
                      V=V+SDF57(10,I)
                  END DO
              END IF
              IF(WQ.EQ.'XAST5S') THEN
                  DO I=0,INT(SYSTEM1(20))
                      V=V+XSDF57(10,I)
                  END DO
              END IF
              IF(WQ.EQ.'DIS5S') THEN
                  DO I=0,INT(SYSTEM1(20))
                      V=V+SDF57(12,I)
                  END DO
              END IF
              IF(WQ.EQ.'XDIS5S') THEN
                  DO I=0,INT(SYSTEM1(20))
                      V=V+XSDF57(12,I)
                  END DO
              END IF
              IF(WQ.EQ.'PTZ5S') THEN
                  DO I=0,INT(SYSTEM1(20))
                      V=V+SDF57(11,I)
                  END DO
              END IF
              IF(WQ.EQ.'XPTZ5S') THEN
                  DO I=0,INT(SYSTEM1(20))
                      V=V+XSDF57(11,I)
                  END DO
              END IF
              IF(WQ.EQ.'TOBSAS') THEN
                  DO I=0,INT(SYSTEM1(20))
                      V=V+SDF57(4,I)+
     1                SDF57(5,I)+SDF57(6,I)
                  END DO
              END IF
              IF(WQ.EQ.'XTOBSAS') THEN
                  DO I=0,INT(SYSTEM1(20))
                      V=V+XSDF57(4,I)+
     1                XSDF57(5,I)+XSDF57(6,I)
                  END DO
              END IF
              IF(WQ.EQ.'SOBSAS') THEN
                  DO I=0,INT(SYSTEM1(20))
                      V=V+SDF57(5,I)
                  END DO
              END IF
              IF(WQ.EQ.'XSOBSAS') THEN
                  DO I=0,INT(SYSTEM1(20))
                      V=V+XSDF57(5,I)
                  END DO
              END IF
              IF(WQ.EQ.'ELCMAS') THEN
                  DO I=0,INT(SYSTEM1(20))
                      V=V+SDF57(7,I)+
     1                SDF57(8,I)
                  END DO
              END IF
              IF(WQ.EQ.'XELCMAS') THEN
                  DO I=0,INT(SYSTEM1(20))
                      V=V+XSDF57(7,I)+
     1                XSDF57(8,I)
                  END DO
              END IF
              IF(WQ.EQ.'TASS') THEN
                  DO I=0,INT(SYSTEM1(20))
                      V=V+SDF57(10,I)+
     1                (5.0*SDF57(11,I))
                  END DO
              END IF
              IF(WQ.EQ.'XTASS') THEN
                  DO I=0,INT(SYSTEM1(20))
                      V=V+XSDF57(10,I)+
     1                (5.0*XSDF57(11,I))
                  END DO
              END IF
              IF(WQ.EQ.'SASS') THEN
                  DO I=0,INT(SYSTEM1(20))
                      V=V+SDF57(10,I)+
     1                SDF57(11,I)
                  END DO
              END IF
              IF(WQ.EQ.'XSASS') THEN
                  DO I=0,INT(SYSTEM1(20))
                      V=V+XSDF57(10,I)+
     1                XSDF57(11,I)
                  END DO
              END IF
              IF(WQ.EQ.'SA7S') THEN
                  DO I=0,INT(SYSTEM1(20))
                      V=V+SDF57(14,I)
                  END DO
              END IF
              IF(WQ.EQ.'XSA7S') THEN
                  DO I=0,INT(SYSTEM1(20))
                      V=V+XSDF57(14,I)
                  END DO
              END IF
C       5TH AND 7TH ORDER INTRINSIC SURFACE ABERRATIONS
              IF(WQ.EQ.'SA5I') THEN
                  DO I=0,INT(SYSTEM1(20))
                      V=V+SAB57(1,I)
                  END DO
              END IF
              IF(WQ.EQ.'XSA5I') THEN
                  DO I=0,INT(SYSTEM1(20))
                      V=V+XSAB57(1,I)
                  END DO
              END IF
              IF(WQ.EQ.'CMA5I') THEN
                  DO I=0,INT(SYSTEM1(20))
                      V=V+SAB57(2,I)+SAB57(3,I)
                  END DO
              END IF
              IF(WQ.EQ.'XCMA5I') THEN
                  DO I=0,INT(SYSTEM1(20))
                      V=V+XSAB57(2,I)+XSAB57(3,I)
                  END DO
              END IF
              IF(WQ.EQ.'AST5I') THEN
                  DO I=0,INT(SYSTEM1(20))
                      V=V+SAB57(10,I)
                  END DO
              END IF
              IF(WQ.EQ.'XAST5I') THEN
                  DO I=0,INT(SYSTEM1(20))
                      V=V+XSAB57(10,I)
                  END DO
              END IF
              IF(WQ.EQ.'DIS5I') THEN
                  DO I=0,INT(SYSTEM1(20))
                      V=V+SAB57(12,I)
                  END DO
              END IF
              IF(WQ.EQ.'XDIS5I') THEN
                  DO I=0,INT(SYSTEM1(20))
                      V=V+XSAB57(12,I)
                  END DO
              END IF
              IF(WQ.EQ.'PTZ5I') THEN
                  DO I=0,INT(SYSTEM1(20))
                      V=V+SAB57(11,I)
                  END DO
              END IF
              IF(WQ.EQ.'XPTZ5I') THEN
                  DO I=0,INT(SYSTEM1(20))
                      V=V+XSAB57(11,I)
                  END DO
              END IF
              IF(WQ.EQ.'TOBSAI') THEN
                  DO I=0,INT(SYSTEM1(20))
                      V=V+SAB57(4,I)+
     1                SAB57(5,I)+SAB57(6,I)
                  END DO
              END IF
              IF(WQ.EQ.'XTOBSAI') THEN
                  DO I=0,INT(SYSTEM1(20))
                      V=V+XSAB57(4,I)+
     1                XSAB57(5,I)+XSAB57(6,I)
                  END DO
              END IF
              IF(WQ.EQ.'SOBSAI') THEN
                  DO I=0,INT(SYSTEM1(20))
                      V=V+SAB57(5,I)
                  END DO
              END IF
              IF(WQ.EQ.'XSOBSAI') THEN
                  DO I=0,INT(SYSTEM1(20))
                      V=V+XSAB57(5,I)
                  END DO
              END IF
              IF(WQ.EQ.'ELCMAI') THEN
                  DO I=0,INT(SYSTEM1(20))
                      V=V+SAB57(7,I)+
     1                SAB57(8,I)
                  END DO
              END IF
              IF(WQ.EQ.'XELCMAI') THEN
                  DO I=0,INT(SYSTEM1(20))
                      V=V+XSAB57(7,I)+
     1                XSAB57(8,I)
                  END DO
              END IF
              IF(WQ.EQ.'TASI') THEN
                  DO I=0,INT(SYSTEM1(20))
                      V=V+SAB57(10,I)+
     1                (5.0*SAB57(11,I))
                  END DO
              END IF
              IF(WQ.EQ.'XTASI') THEN
                  DO I=0,INT(SYSTEM1(20))
                      V=V+XSAB57(10,I)+
     1                (5.0*XSAB57(11,I))
                  END DO
              END IF
              IF(WQ.EQ.'SASI') THEN
                  DO I=0,INT(SYSTEM1(20))
                      V=V+SAB57(10,I)+
     1                SAB57(11,I)
                  END DO
              END IF
              IF(WQ.EQ.'XSASI') THEN
                  DO I=0,INT(SYSTEM1(20))
                      V=V+XSAB57(10,I)+
     1                XSAB57(11,I)
                  END DO
              END IF
              IF(WQ.EQ.'SA7I') THEN
                  DO I=0,INT(SYSTEM1(20))
                      V=V+SAB57(14,I)
                  END DO
              END IF
              IF(WQ.EQ.'XSA7I') THEN
                  DO I=0,INT(SYSTEM1(20))
                      V=V+XSAB57(14,I)
                  END DO
              END IF
C       3RD ORDER PUPIL ABERRATIONS
              IF(WQ.EQ.'PSA3') THEN
                  DO I=0,INT(SYSTEM1(20))
                      V=V+MAB3(6,I)
                  END DO
              END IF
              IF(WQ.EQ.'XPSA3') THEN
                  DO I=0,INT(SYSTEM1(20))
                      V=V+XMAB3(6,I)
                  END DO
              END IF
              IF(WQ.EQ.'PCMA3') THEN
                  DO I=0,INT(SYSTEM1(20))
                      V=V+(3.0D0*MAB3(7,I))
                  END DO
              END IF
              IF(WQ.EQ.'XPCMA3') THEN
                  DO I=0,INT(SYSTEM1(20))
                      V=V+(3.0D0*XMAB3(7,I))
                  END DO
              END IF
              IF(WQ.EQ.'PAST3') THEN
                  DO I=0,INT(SYSTEM1(20))
                      V=V+MAB3(8,I)
                  END DO
              END IF
              IF(WQ.EQ.'XPAST3') THEN
                  DO I=0,INT(SYSTEM1(20))
                      V=V+XMAB3(8,I)
                  END DO
              END IF
              IF(WQ.EQ.'PDIS3') THEN
                  DO I=0,INT(SYSTEM1(20))
                      V=V+MAB3(9,I)
                  END DO
              END IF
              IF(WQ.EQ.'XPDIS3') THEN
                  DO I=0,INT(SYSTEM1(20))
                      V=V+XMAB3(9,I)
                  END DO
              END IF
              IF(WQ.EQ.'PPTZ3') THEN
                  DO I=0,INT(SYSTEM1(20))
                      V=V+MAB3(10,I)
                  END DO
              END IF
              IF(WQ.EQ.'XPPTZ3') THEN
                  DO I=0,INT(SYSTEM1(20))
                      V=V+XMAB3(10,I)
                  END DO
              END IF
C       3RD ORDER PUPIL ABERRATION PRIMARY CHROMATIC DIFFERENCES
              IF(WQ.EQ.'PSA3P') THEN
                  DO I=0,INT(SYSTEM1(20))
                      V=V+PDF3(6,I)
                  END DO
              END IF
              IF(WQ.EQ.'XPSA3P') THEN
                  DO I=0,INT(SYSTEM1(20))
                      V=V+XPDF3(6,I)
                  END DO
              END IF
              IF(WQ.EQ.'PCMA3P') THEN
                  DO I=0,INT(SYSTEM1(20))
                      V=V+(3.0D0*PDF3(7,I))
                  END DO
              END IF
              IF(WQ.EQ.'XPCMA3P') THEN
                  DO I=0,INT(SYSTEM1(20))
                      V=V+(3.0D0*XPDF3(7,I))
                  END DO
              END IF
              IF(WQ.EQ.'PAST3P') THEN
                  DO I=0,INT(SYSTEM1(20))
                      V=V+PDF3(8,I)
                  END DO
              END IF
              IF(WQ.EQ.'XPAST3P') THEN
                  DO I=0,INT(SYSTEM1(20))
                      V=V+XPDF3(8,I)
                  END DO
              END IF
              IF(WQ.EQ.'PDIS3P') THEN
                  DO I=0,INT(SYSTEM1(20))
                      V=V+PDF3(9,I)
                  END DO
              END IF
              IF(WQ.EQ.'XPDIS3P') THEN
                  DO I=0,INT(SYSTEM1(20))
                      V=V+XPDF3(9,I)
                  END DO
              END IF
              IF(WQ.EQ.'PPTZ3P') THEN
                  DO I=0,INT(SYSTEM1(20))
                      V=V+PDF3(10,I)
                  END DO
              END IF
              IF(WQ.EQ.'XPPTZ3P') THEN
                  DO I=0,INT(SYSTEM1(20))
                      V=V+XPDF3(10,I)
                  END DO
              END IF
C       3RD ORDER PUPIL ABERRATION SECONDARY CHROMATIC DIFFERENCES
              IF(WQ.EQ.'PSA3S') THEN
                  DO I=0,INT(SYSTEM1(20))
                      V=V+SDF3(6,I)
                  END DO
              END IF
              IF(WQ.EQ.'XPSA3S') THEN
                  DO I=0,INT(SYSTEM1(20))
                      V=V+XSDF3(6,I)
                  END DO
              END IF
              IF(WQ.EQ.'PCMA3S') THEN
                  DO I=0,INT(SYSTEM1(20))
                      V=V+(3.0D0*SDF3(7,I))
                  END DO
              END IF
              IF(WQ.EQ.'XPCMA3S') THEN
                  DO I=0,INT(SYSTEM1(20))
                      V=V+(3.0D0*XSDF3(7,I))
                  END DO
              END IF
              IF(WQ.EQ.'PAST3S') THEN
                  DO I=0,INT(SYSTEM1(20))
                      V=V+SDF3(8,I)
                  END DO
              END IF
              IF(WQ.EQ.'XPAST3S') THEN
                  DO I=0,INT(SYSTEM1(20))
                      V=V+XSDF3(8,I)
                  END DO
              END IF
              IF(WQ.EQ.'PDIS3S') THEN
                  DO I=0,INT(SYSTEM1(20))
                      V=V+SDF3(9,I)
                  END DO
              END IF
              IF(WQ.EQ.'XPDIS3S') THEN
                  DO I=0,INT(SYSTEM1(20))
                      V=V+XSDF3(9,I)
                  END DO
              END IF
              IF(WQ.EQ.'PPTZ3S') THEN
                  DO I=0,INT(SYSTEM1(20))
                      V=V+SDF3(10,I)
                  END DO
              END IF
              IF(WQ.EQ.'XPPTZ3S') THEN
                  DO I=0,INT(SYSTEM1(20))
                      V=V+XSDF3(10,I)
                  END DO
              END IF
              IF(WQ.EQ.'PTZCV'.OR.WQ.EQ.'XPTZCV') THEN
                  VALUE1=V
              ELSE
                  VALUE1=V/INV
              END IF
              IF(SYSTEM1(30).LE.2.0D0) THEN
                  IF(WQ.EQ.'PACY') THEN
                      DO I=0,INT(SYSTEM1(20))
                          V=V+COLORY(1,I)
                      END DO
                      VALUE1=V/INTV
                  END IF
                  IF(WQ.EQ.'PLCY') THEN
                      DO I=0,INT(SYSTEM1(20))
                          V=V+COLORY(2,I)
                      END DO
                      VALUE1=V/INTV
                  END IF
                  IF(WQ.EQ.'SACY') THEN
                      DO I=0,INT(SYSTEM1(20))
                          V=V+COLORY(3,I)
                      END DO
                      VALUE1=V/INTV
                  END IF
                  IF(WQ.EQ.'SLCY') THEN
                      DO I=0,INT(SYSTEM1(20))
                          V=V+COLORY(4,I)
                      END DO
                      VALUE1=V/INTV
                  END IF
                  IF(WQ.EQ.'PACX') THEN
                      DO I=0,INT(SYSTEM1(20))
                          V=V+COLORX(1,I)
                      END DO
                      VALUE1=V/INTV
                  END IF
                  IF(WQ.EQ.'PLCX') THEN
                      DO I=0,INT(SYSTEM1(20))
                          V=V+COLORX(2,I)
                      END DO
                      VALUE1=V/INTV
                  END IF
                  IF(WQ.EQ.'SACX') THEN
                      DO I=0,INT(SYSTEM1(20))
                          V=V+COLORX(3,I)
                      END DO
                      VALUE1=V/INTV
                  END IF
                  IF(WQ.EQ.'SLCX') THEN
                      DO I=0,INT(SYSTEM1(20))
                          V=V+COLORX(4,I)
                      END DO
                      VALUE1=V/INTV
                  END IF
              ELSE
                  IF(WQ.EQ.'PACY') THEN
                      DO I=0,INT(SYSTEM1(20))
                          V=V+COLORY(5,I)
                      END DO
                      VALUE1=V/INTV
                  END IF
                  IF(WQ.EQ.'PLCY') THEN
                      DO I=0,INT(SYSTEM1(20))
                          V=V+COLORY(6,I)
                      END DO
                      VALUE1=V/INTV
                  END IF
                  IF(WQ.EQ.'SACY') THEN
                      DO I=0,INT(SYSTEM1(20))
                          V=V+COLORY(7,I)
                      END DO
                      VALUE1=V/INTV
                  END IF
                  IF(WQ.EQ.'SLCY') THEN
                      DO I=0,INT(SYSTEM1(20))
                          V=V+COLORY(8,I)
                      END DO
                      VALUE1=V/INTV
                  END IF
                  IF(WQ.EQ.'PACX') THEN
                      DO I=0,INT(SYSTEM1(20))
                          V=V+COLORX(5,I)
                      END DO
                      VALUE1=V/INTV
                  END IF
                  IF(WQ.EQ.'PLCX') THEN
                      DO I=0,INT(SYSTEM1(20))
                          V=V+COLORX(6,I)
                      END DO
                      VALUE1=V/INTV
                  END IF
                  IF(WQ.EQ.'SACX') THEN
                      DO I=0,INT(SYSTEM1(20))
                          V=V+COLORX(7,I)
                      END DO
                      VALUE1=V/INTV
                  END IF
                  IF(WQ.EQ.'SLCX') THEN
                      DO I=0,INT(SYSTEM1(20))
                          V=V+COLORX(8,I)
                      END DO
                      VALUE1=V/INTV
                  END IF
              END IF
              RETURN
          END IF
      END
