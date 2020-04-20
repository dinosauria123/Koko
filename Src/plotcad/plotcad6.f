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

C       SIXTH FILE OF PLOT/CAD ROUTINES

C SUB PFANCAP
      SUBROUTINE PFANCAP
C
          IMPLICIT NONE
C
C       THIS ROUTINE DRAWS THE USER-DEFINED FAN CAPTION
C       UP TO 40 CHARACTERS
C       PLOTTING
C
          CHARACTER CAP*40
C
          LOGICAL FANEXT
C
          COMMON/FANEXI/FANEXT
          INTEGER COLPAS,NT1ANG,NT1SIZ,XPOS,YPOS
C
          INTEGER XTENT,YTENT,CLIP
C
          COMMON/USEFAN/XTENT,YTENT,CLIP
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          INCLUDE 'dathgr.inc'
          IF(STI.EQ.1) THEN
              OUTLYNE=
     1        '"PFANCAP" PLOTS A USER-DEFINED FAN CAPTION'
              CALL SHOWIT(1)
              OUTLYNE=
     1        'UP TO 40 CHARACTERS LONG)'
              CALL SHOWIT(1)
              RETURN
          END IF
          IF(SN.EQ.1) THEN
              OUTLYNE=
     1        '"PFANCAP" TAKES NO NUMERIC INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SQ.EQ.1) THEN
              OUTLYNE=
     1        '"PFANCAP" TAKES NO QUALIFIER INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          CAP='                                        '
          CAP=WS(1:40)
          XPOS=FANAXX-(XTENT/2)
          YPOS=FANAXY-(YTENT/2)-150
          NT1ANG=0
          NT1SIZ=1
          COLPAS=COLLBL
          CALL MY_COLTYP(COLPAS)
          CALL MY_SETCHARASPECT(1.5,1.5)
          CALL MY_JUSTSTRING(XPOS,YPOS,CAP,NT1ANG,NT1SIZ,3)
          RETURN
      END
C SUB PFANLBL
      SUBROUTINE PFANLBL
C
          IMPLICIT NONE
C
C       THIS ROUTINE DRAWS THE USER-DEFINED Y-LABLES ON USER-DEFINED FANS
C     THIS ALSO DOES THE +1.0 AND -1.0 X-AXIS LABLES
C       PLOTTING
C
          CHARACTER XLBL*4,YLBL*21
C
          LOGICAL FANEXT
C
          COMMON/FANEXI/FANEXT
          INTEGER COLPAS,NT1ANG,NT1SIZ,XPOS,YPOS,I,II
C
          INTEGER XTENT,YTENT,CLIP
C
          COMMON/USEFAN/XTENT,YTENT,CLIP
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          INCLUDE 'dathgr.inc'
          IF(STI.EQ.1) THEN
              OUTLYNE=
     1        '"PFANLBL" PLOTS A USER-DEFINED Y-AXIS LABEL'
              CALL SHOWIT(1)
              OUTLYNE=
     1        'UP TO 21 CHARACTERS LONG)'
              CALL SHOWIT(1)
              OUTLYNE=
     1        'AND THE X-AXIS RELATIVE APERTURE LABLES'
              CALL SHOWIT(1)
              RETURN
          END IF
          IF(SN.EQ.1) THEN
              OUTLYNE=
     1        '"PFANLBL" TAKES NO NUMERIC INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SQ.EQ.1) THEN
              OUTLYNE=
     1        '"PFANLBL" TAKES NO QUALIFIER INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          YLBL(1:21)='                     '
          YLBL(1:21)=WS(1:21)
          II=0
          DO I=21,1,-1
              IF(YLBL(I:I).NE.' ') THEN
                  II=I
                  GO TO 10
              END IF
          END DO
 10       CONTINUE
          XPOS=FANAXX
          YPOS=FANAXY+(YTENT/2)+100
          NT1ANG=0
          NT1SIZ=1
          COLPAS=COLLBL
          CALL MY_COLTYP(COLPAS)
          CALL MY_SETCHARASPECT(1.5,1.5)
          CALL MY_JUSTSTRING(XPOS,YPOS,YLBL(1:II),NT1ANG,NT1SIZ,2)
          XLBL='-1.0'
          XPOS=FANAXX-(XTENT/2)-350
          YPOS=FANAXY-20
          NT1ANG=0
          NT1SIZ=1
          COLPAS=COLLBL
          CALL MY_COLTYP(COLPAS)
          CALL MY_JUSTSTRING(XPOS,YPOS,XLBL,NT1ANG,NT1SIZ,3)
          XLBL='+1.0'
          XPOS=FANAXX+(XTENT/2)+50
          YPOS=FANAXY-20
          NT1ANG=0
          NT1SIZ=1
          COLPAS=COLLBL
          CALL MY_COLTYP(COLPAS)
          CALL MY_JUSTSTRING(XPOS,YPOS,XLBL,NT1ANG,NT1SIZ,3)
          RETURN
      END
C SUB PFANAXIS.FOR
      SUBROUTINE PFANAXIS
C
          IMPLICIT NONE
C
C      PLOT USER DEFINED FAN AXIS AT X=W1, Y=W2 IF FANEXT IS TRUE
C     X EXTENT WILL BE W3 (DEFAULT 2500 UNITS)
C     Y EXTENT WILL BE W4 (DEFAULT 2000 UNITS)
C     W5=0 CLIPPING IN BOX (DEFAULT)
C     W5 NOT 0, NO CLIPPING
C
          INTEGER IX,IY,IPST,COLPAS
C
          LOGICAL FANEXT
C
          INTEGER XTENT,YTENT,CLIP
C
          COMMON/USEFAN/XTENT,YTENT,CLIP
C
          COMMON/FANEXI/FANEXT
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          INCLUDE 'dathgr.inc'
C
          IF(STI.EQ.1) THEN
              OUTLYNE=
     1        '"PFANAXIS" PLOTS A USER-DEFINED FAN AXIS'
              CALL SHOWIT(1)
              OUTLYNE='CURRENT NUMERIC WORDS IN EFFECT ARE:'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*) FANAXX,FANAXY,(XTENT/2),(YTENT/2),CLIP
              CALL SHOWIT(1)
              RETURN
          END IF
          IF(SST.EQ.1.OR.SQ.EQ.1) THEN
              OUTLYNE=
     1        '"PFANAXIS" TAKES NO QUALIFIER OR STRING INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(DF1.EQ.1) THEN
              W1=5000.0D0
              DF1=0
              S1=1
              SN=1
          END IF
          IF(DF2.EQ.1) THEN
              W2=3500.0D0
              DF2=0
              S2=1
              SN=1
          END IF
          IF(DF3.EQ.1) THEN
              W3=2500.0D0
              DF3=0
              S3=1
              SN=1
          END IF
          IF(DF4.EQ.1) THEN
              W4=2000.0D0
              DF4=0
              S5=1
              SN=1
          END IF
          IF(DF5.EQ.1) THEN
              W5=0.0D0
              DF5=0
              S5=1
              SN=1
          END IF
          XTENT=INT(W3)
          YTENT=INT(W4)
          CLIP=INT(W5)
          IF(CLIP.NE.0) CLIP=1
          XXMIN=0
          XXMAX=10000
          YYMIN=0
          YYMAX=7000
          FANAXX=INT(W1)
          FANAXY=INT(W2)
          COLPAS=COLAXS
          CALL MY_COLTYP(COLPAS)
          IX=FANAXX-(XTENT/2)
          IY=FANAXY
          IPST=0
          CALL MY_PLOT(IX,IY,IPST,LNTYPE,XXMIN,XXMAX,YYMIN,YYMAX)
          IX=FANAXX+(XTENT/2)
          IY=FANAXY
          IPST=1
          CALL MY_PLOT(IX,IY,IPST,LNTYPE,XXMIN,XXMAX,YYMIN,YYMAX)
          IX=FANAXX
          IY=FANAXY-(YTENT/2)
          IPST=0
          CALL MY_PLOT(IX,IY,IPST,LNTYPE,XXMIN,XXMAX,YYMIN,YYMAX)
          IX=FANAXX
          IY=FANAXY+(YTENT/2)
          IPST=1
          CALL MY_PLOT(IX,IY,IPST,LNTYPE,XXMIN,XXMAX,YYMIN,YYMAX)
          IX=0
          IY=0
          IPST=0
          CALL MY_PLOT(IX,IY,IPST,LNTYPE,XXMIN,XXMAX,YYMIN,YYMAX)
          PLEXIS=.TRUE.
          RETURN
      END
C SUB PFANSSI.FOR
      SUBROUTINE PFANSSI
C
          IMPLICIT NONE
C
C       USER DEFINED FAN SCALE FACTOR
C
          LOGICAL FANEXT
C
          COMMON/FANEXI/FANEXT
C
          INTEGER XTENT,YTENT,CLIP
C
          COMMON/USEFAN/XTENT,YTENT,CLIP
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          INCLUDE 'dathgr.inc'
C
          IF(STI.EQ.1) THEN
              OUTLYNE=
     1        '"PFANSSI" SETS AN EXPLICIT USER-DEFINED FAN PLOT SCALE FACTOR'
              CALL SHOWIT(1)
              IF(.NOT.FSSIFLG) THEN
                  OUTLYNE=
     1            'USER-DEFINED FAN PLOT SCALE FACTOR SET TO AUTOMATIC'
                  CALL SHOWIT(1)
                  RETURN
              END IF
              IF(FSSIFLG) THEN
                  WRITE(OUTLYNE,*)
     1            'USER-DEFINED FAN PLOT SCALE FACTOR = ',FSSI
                  CALL SHOWIT(1)
                  RETURN
              END IF
          END IF
          IF(SST.EQ.1.OR.SQ.EQ.1) THEN
              OUTLYNE=
     1        '"PFANSSI" TAKES NO QUALIFIER OR STRING INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
              OUTLYNE=
     1        '"PFANSSI" ONLY TAKES NUMERIC WORD #1 INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(S1.EQ.0) THEN
              OUTLYNE=
     1        '"PFANSSI" REQUIRES EXPLICIT NUMERIC WORD #1 INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(W1.LE.0.0D0) THEN
              OUTLYNE=
     1        '"PFANSSI" HAS ALREADY BEEN SET'
              CALL SHOWIT(1)
              OUTLYNE=
     1        '"PFANSSI" MUST BE SET TO A POSITIVE, NON-ZERO VALUE'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
          ELSE
              FSSIFLG=.TRUE.
              FSSI=W1/2.0D0
          END IF
          RETURN
      END
C SUB PFANCOMP.FOR
      SUBROUTINE PFANCOMP
C
          IMPLICIT NONE
C
C       USER DEFINED FAN PLOTTER AT FANAXX,FANAXY
C
          INTEGER ICOMP,JCOMP,KCOMP
C
          LOGICAL FANEXT
C
          COMMON/FANEXI/FANEXT
C
          INTEGER XTENT,YTENT,CLIP
C
          COMMON/USEFAN/XTENT,YTENT,CLIP
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          INCLUDE 'dathgr.inc'
C
          IF(STI.EQ.1) THEN
              OUTLYNE=
     1        '"PFANCOMP" PLOTS USER-DEFINED FAN COMPONENT 1,2,3 OR 4'
              CALL SHOWIT(1)
              RETURN
          END IF
          IF(SST.EQ.1.OR.SQ.EQ.1) THEN
              OUTLYNE=
     1        '"PFANCOMP" TAKES NO QUALIFIER OR STRING INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(S4.EQ.1.OR.S5.EQ.1) THEN
              OUTLYNE=
     1        '"PFANCOMP" ONLY TAKES NUMERIC WORDS #1, #2 AND #3 INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(S1.EQ.0) THEN
              OUTLYNE=
     1        '"PFANCOMP" REQUIRES EXPLICIT NUMERIC WORD #1 INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(W1.NE.1.0D0.AND.W1.NE.2.0D0.AND.
     1    W1.NE.3.0D0.AND.W1.NE.4.0D0) THEN
              OUTLYNE=
     1        '"PFANCOMP" REQUIRES EITHER "1","2","3" OR "4"'
              CALL SHOWIT(1)
              OUTLYNE=
     1        'AS NUMERIC INPUT ONLY'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(.NOT.REFEXT) THEN
              CALL SHOWIT(1)
              OUTLYNE=
     1        'NO FAN EXISTS, CURRENT USER-DEFINED FAN PLOT ABORTED'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          ELSE
          END IF
          IF(.NOT.FANEXT) THEN
              CALL SHOWIT(1)
              OUTLYNE=
     1        'NO FAN EXISTS, CURRENT USER-DEFINED FAN PLOT ABORTED'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          ELSE
          END IF
          IF(W1.GT.1.0D0.AND.QALTYP.EQ.1) THEN
              OUTLYNE=
     1        '"1" IS THE ONLY VALID "PFANCOMP"'
              CALL SHOWIT(1)
              OUTLYNE=
     1        'ARGUMENT FOR OPD FANS'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(W1.GT.2.0D0.AND.QALTYP.EQ.0) THEN
              OUTLYNE=
     1        '"1" OR "2" ARE THE ONLY VALID "PFANCOMP"'
              CALL SHOWIT(1)
              OUTLYNE=
     1        'ARGUMENTS FOR DX/DY/DXA/DYA FAN TYPES'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(DF2.EQ.1) W2=1.0D0
          IF(DF3.EQ.1) W3=0.0D0
          JCOMP=15
          KCOMP=0
          ICOMP=INT(W1)
          KCOMP=INT(W3)
          IF(INT(W2).EQ.1)  JCOMP=15
          IF(INT(W2).EQ.2)  JCOMP=12
          IF(INT(W2).EQ.3)  JCOMP=2
          IF(INT(W2).EQ.4)  JCOMP=3
          IF(INT(W2).EQ.5)  JCOMP=4
          IF(INT(W2).EQ.6)  JCOMP=5
          IF(INT(W2).EQ.7)  JCOMP=6
          IF(INT(W2).EQ.8)  JCOMP=7
          IF(INT(W2).EQ.9)  JCOMP=8
          IF(INT(W2).EQ.10) JCOMP=9
          CALL PLFAN3(ICOMP)
          CALL FANDO1(JCOMP,KCOMP)
          JCOMP=15
          KCOMP=0
          RETURN
      END


C SUB PENMV1A(X,Y,PST)
      SUBROUTINE PENMV1A(IX,IY,IPST)
C
          IMPLICIT NONE
C
C       THIS ROUTINE DOES MOVES THE PLOTTER PEN FOR PLOT RAY
C
          INTEGER IX,IY,IPST
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          INCLUDE 'dathgr.inc'
          CALL MY_PLOT(IX,IY,IPST,LNTYPE,XXMIN,XXMAX,YYMIN,YYMAX)
          RETURN
      END


C SUB PENMV2(X,Y,PST)
      SUBROUTINE PENMV2(IX,IY,IPST)
C
          IMPLICIT NONE
C
C       THIS ROUTINE DOES MOVES THE PLOTTER PEN FOR PLOT RAY WITH NO BOUNDS
C
          INTEGER IX,IY,IPST
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          INCLUDE 'dathgr.inc'
          XXMIN=-1000000
          XXMAX=1000000
          YYMIN=-1000000
          YYMAX=1000000
          CALL MY_PLOT(IX,IY,IPST,LNTYPE,XXMIN,XXMAX,YYMIN,YYMAX)
          RETURN
      END


C SUB PENMV1(X,Y,PST)
      SUBROUTINE PENMV1(IX,IY,IPST)
C
          IMPLICIT NONE
C
C       THIS ROUTINE DOES MOVES THE PLOTTER PEN FOR PLOT RAY
C
          INTEGER IX,IY,IPST
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          INCLUDE 'dathgr.inc'
C       SET THE OLD VALUES EQUAL TO THE PREVIOUS NEW VALUES
          POLDX=PNEWX
          POLDY=PNEWY
C       SET THE CURRENT VALUES BEFORE CORRECTION
          PCURX=DBLE(IX)
          PCURY=DBLE(IY)
          IF((PCURX-POLDX).NE.0.0D0) THEN
          END IF
C       TEST IF LINE SHOULD BE DASHED
          IF(FIXUP) THEN
              IF(IX.LT.0) THEN
                  CALL FIXUP1(IX,IY)
                  GO TO 10
              END IF
              IF(IX.GT.10000) THEN
                  CALL FIXUP2(IX,IY)
                  GO TO 10
              END IF
              IF(IY.LT.0) THEN
                  CALL FIXUP3(IX,IY)
                  GO TO 10
              END IF
              IF(IY.GT.7000) THEN
                  CALL FIXUP4(IX,IY)
                  GO TO 10
              END IF
          ELSE
              FIXUP=.TRUE.
          END IF
C       SET THE CURRENT VALUES AFTER CORRECTION
          PCURX=DBLE(IX)
          PCURY=DBLE(IY)
 10       CALL MY_PLOT(IX,IY,IPST,LNTYPE,XXMIN,XXMAX,YYMIN,YYMAX)
C       SET THE NEW "NEW" VALUES
          PNEWX=DBLE(IX)
          PNEWY=DBLE(IY)
          RETURN
      END


      SUBROUTINE FIXUP1(IX,IY)
C       IX IS LESS THAN 0
          IMPLICIT NONE
          INCLUDE 'dathgr.inc'
          LOGICAL VERTICAL,HORIZONTAL
          INTEGER IX,IY
          REAL*8 DELX,DELY,SLOPE
          INCLUDE 'datmai.inc'
          VERTICAL=.FALSE.
          HORIZONTAL=.FALSE.
          DELX=PCURX-POLDX
          DELY=PCURY-POLDY
          IF(DELY.EQ.0.0D0) HORIZONTAL=.TRUE.
          IF(DELX.EQ.0.0D0) VERTICAL=.TRUE.
          IF(VERTICAL) THEN
C       LINE WAS VERTICAL
              IF(PCURY.GT.7000.0D0) PNEWY=7000.0D0
              IF(PCURY.LT.0.0D0) PNEWY=0.0D0
              PNEWX=0.0D0
              IX=INT(PNEWX)
              IY=INT(PNEWY)
              RETURN
          END IF
          IF(HORIZONTAL) THEN
C       LINE WAS HORIZONTAL
              IF(PCURY.GT.7000.0D0) PNEWY=7000.0D0
              IF(PCURY.LT.0.0D0) PNEWY=0.0D0
              PNEWX=0.0D0
              IX=INT(PNEWX)
              IY=INT(PNEWY)
              RETURN
          END IF
          SLOPE=DELY/DELX
          PNEWX=0.0D0
          PNEWY=(SLOPE*(PNEWX-POLDX))+POLDY
          IX=INT(PNEWX)
          IY=INT(PNEWY)
          RETURN
      END
      SUBROUTINE FIXUP2(IX,IY)
C       IX IS GREATER THAN 10000
          IMPLICIT NONE
          INCLUDE 'dathgr.inc'
          LOGICAL VERTICAL,HORIZONTAL
          INTEGER IX,IY
          REAL*8 DELX,DELY,SLOPE
          VERTICAL=.FALSE.
          HORIZONTAL=.FALSE.
          DELX=PCURX-POLDX
          DELY=PCURY-POLDY
          IF(DELY.EQ.0.0D0) HORIZONTAL=.TRUE.
          IF(DELX.EQ.0.0D0) VERTICAL=.TRUE.
          IF(VERTICAL) THEN
C       LINE WAS VERTICAL
              IF(PCURY.GT.7000.0D0) PNEWY=7000.0D0
              IF(PCURY.LT.0.0D0) PNEWY=0.0D0
              PNEWX=10000.0D0
              IX=INT(PNEWX)
              IY=INT(PNEWY)
              RETURN
          END IF
          IF(HORIZONTAL) THEN
C       LINE WAS HORIZONTAL
              IF(PCURY.GT.7000.0D0) PNEWY=7000.0D0
              IF(PCURY.LT.0.0D0) PNEWY=0.0D0
              PNEWX=10000.0D0
              IX=INT(PNEWX)
              IY=INT(PNEWY)
              RETURN
          END IF
C       NOT VERTICAL
          SLOPE=DELY/DELX
          PNEWX=10000.0D0
          PNEWY=(SLOPE*(PNEWX-POLDX))+POLDY
          IX=INT(PNEWX)
          IY=INT(PNEWY)
          RETURN
      END
      SUBROUTINE FIXUP3(IX,IY)
C       IY IS LESS THAN 0
          IMPLICIT NONE
          INCLUDE 'dathgr.inc'
          LOGICAL VERTICAL,HORIZONTAL
          INTEGER IX,IY
          REAL*8 DELX,DELY,SLOPE
          VERTICAL=.FALSE.
          HORIZONTAL=.FALSE.
          DELX=PCURX-POLDX
          DELY=PCURY-POLDY
          IF(DELX.EQ.0.0D0) VERTICAL=.TRUE.
          IF(DELY.EQ.0.0D0) HORIZONTAL=.TRUE.
          IF(VERTICAL) THEN
C       LINE WAS VERTICAL
              PNEWY=0.0D0
              PNEWX=PCURX
              IX=INT(PNEWX)
              IY=INT(PNEWY)
              RETURN
          END IF
          IF(HORIZONTAL) THEN
C       LINE WAS HORIZONTAL
              PNEWY=0.0D0
              PNEWX=PCURX
              IX=INT(PNEWX)
              IY=INT(PNEWY)
              RETURN
          END IF
C       NOT VERTICAL OR HORIZONTAL
          SLOPE=DELY/DELX
          PNEWY=0.0D0
          PNEWX=((PNEWY-POLDY)/SLOPE)+POLDX
          IX=INT(PNEWX)
          IY=INT(PNEWY)
          RETURN
      END
      SUBROUTINE FIXUP4(IX,IY)
C       IY IS GREATER THAN 7000
          IMPLICIT NONE
          INCLUDE 'dathgr.inc'
          LOGICAL VERTICAL,HORIZONTAL
          INTEGER IX,IY
          REAL*8 DELX,DELY,SLOPE
          VERTICAL=.FALSE.
          HORIZONTAL=.FALSE.
          DELX=PCURX-POLDX
          DELY=PCURY-POLDY
          IF(DELX.EQ.0.0D0) VERTICAL=.TRUE.
          IF(DELY.EQ.0.0D0) HORIZONTAL=.TRUE.
          IF(VERTICAL) THEN
C       LINE WAS VERTICAL
              PNEWY=7000.0D0
              PNEWX=PCURX
              IX=INT(PNEWX)
              IY=INT(PNEWY)
              RETURN
          END IF
          IF(HORIZONTAL) THEN
C       LINE WAS HORIZONTAL
              PNEWY=7000.0D0
              PNEWX=PCURX
              IX=INT(PNEWX)
              IY=INT(PNEWY)
              RETURN
          END IF
C       NOT VERTICAL OR HORIZONTAL
          SLOPE=DELY/DELX
          PNEWY=7000.0D0
          PNEWX=((PNEWY-POLDY)/SLOPE)+POLDX
          IX=INT(PNEWX)
          IY=INT(PNEWY)
          RETURN
      END
C SUB PENMV.FOR
      SUBROUTINE PENMV
C
          IMPLICIT NONE
C
C       THIS ROUTINE DOES MOVES THE PLOTTER PEN
          INTEGER XPEN1,YPEN1,PENSTA1
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          INCLUDE 'dathgr.inc'
C
          IF(PENSTA.EQ.1) THEN
              XPEN1=XPEN
              YPEN1=YPEN
              PENSTA1=PENSTA
              CALL MY_PLOT(XPEN1,YPEN1,PENSTA1,LNTYPE,-10,10010,-10,7010)
              RETURN
          ELSE
          END IF
          IF(PENSTA.EQ.2) THEN
              XPEN1=XPEN
              YPEN1=YPEN
              PENSTA1=1
              CALL MY_PLOT(XPEN1,YPEN1,PENSTA1,LNTYPE,-10,10010,-10,7010)
              RETURN
          ELSE
          END IF
          IF(PENSTA.EQ.3) THEN
              XPEN1=XPEN
              YPEN1=YPEN
              PENSTA1=0
              CALL MY_PLOT(XPEN1,YPEN1,PENSTA1,LNTYPE,-10,10010,-10,7010)
              RETURN
          ELSE
          END IF
          RETURN
      END

      SUBROUTINE DODRAWING
C
          IMPLICIT NONE
C
          REAL*8 CV1,CV2,TMPVAL,DSCFAC,EFL,BFL,FFL,PP1,PP2,DLPF1,DLPF2
C
          CHARACTER ADSCFAC*3,LINEOF*72,FLNM*19,UN*6,G1*3,G2*3
C
          INTEGER YSTEP,K,I,ICNT,YPOS,COLPAS,NT1ANG,NT1SIZ,GLSCD1,GLSCD2
C
          CHARACTER*12 DRAWVL1,SVAL
C
          CHARACTER DRAWVL2*9
C
          LOGICAL OPEN56,EXIS56,DERROR
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          INCLUDE 'datpts.inc'
C
          IF(SYSTEM1(6).EQ.1.0D0) UN='INCHES'
          IF(SYSTEM1(6).EQ.2.0D0) UN='CM    '
          IF(SYSTEM1(6).EQ.3.0D0) UN='MM    '
          IF(SYSTEM1(6).EQ.4.0D0) UN='METERS'
          YSTEP=120
C
          IF(GLANAM(DRAWSURF,2).EQ.'AIR          '.OR.
     1    GLANAM(DRAWSURF,2).EQ.'REFL         '.OR.
     1    GLANAM(DRAWSURF,2).EQ.'REFLTIRO     '.OR.
     1    GLANAM(DRAWSURF,2).EQ.'REFLTIR      '.OR.
     1    GLANAM(DRAWSURF,2).EQ.'PERFECT      '.OR.
     1    GLANAM(DRAWSURF,2).EQ.'IDEAL        '.OR.
     1    GLANAM(DRAWSURF,1).EQ.'MYGLASS      '.OR.
     1    GLANAM(DRAWSURF,1).EQ.'MODEL        '.OR.
     1    GLANAM(DRAWSURF,1).EQ.'USER         ') THEN
              OUTLYNE=
     1        '"PARTDRAW" ONLY DRAWS LENSES WITH VALID CATALOG GLASSES'
              CALL SHOWIT(1)
              OUTLYNE=
     1        'NO ACTION TAKEN'
              CALL SHOWIT(1)
              PARTISDRAWING=.FALSE.
              CALL MACFAL
              RETURN
          END IF
          IF(GLANAM(DRAWSURF,1).NE.'SCHOTT'.AND.
     1    GLANAM(DRAWSURF,1).NE.'OHARA'.AND.
     1    GLANAM(DRAWSURF,1).NE.'SCH2000'.AND.
     1    GLANAM(DRAWSURF,1).NE.'HOYA'.AND.
     1    GLANAM(DRAWSURF,1).NE.'HIKARI'.AND.
     1    GLANAM(DRAWSURF,1).NE.'CORNIN'.AND.
     1    GLANAM(DRAWSURF,1).NE.'CHANCE'.AND.
     1    GLANAM(DRAWSURF,1).NE.'RADHARD') THEN
C     SET WAVELENGTH TO THE CONTROL WAVELENGTH
              WAVEL1=SYSTEM1(INT(SYSTEM1(11)))
          END IF
C
C     SET FLAT/NOT FLAT LOGICALS
          IF(ALENS(1,DRAWSURF).EQ.0.0D0) FLAT1=.TRUE.
          IF(ALENS(1,DRAWSURF).NE.0.0D0) FLAT1=.FALSE.
          IF(ALENS(1,DRAWSURF+1).EQ.0.0D0) FLAT2=.TRUE.
          IF(ALENS(1,DRAWSURF+1).NE.0.0D0) FLAT2=.FALSE.
C
C     CALCULATE EFL,BFL,FFL,PP1,PP2 AT REFERENCE WAVELENGTH
          SAVE_KDP(1)=SAVEINPT(1)
          WC='FIRD'
          WQ='QUIET'
          SQ=1
          DF1=0
          DF2=0
          DF3=0
          DF4=1
          DF5=1
          S1=1
          S2=1
          S3=1
          S4=0
          S5=0
          W1=DBLE(DRAWSURF)
          W2=DBLE(DRAWSURF+1)
          W3=WAVEL1
          W4=0.0D0
          W5=0.0D0
          STI=0
          SST=0
          SN=1
          CALL FIRD
          EFL=GPREG(1)
          BFL=GPREG(2)
          FFL=GPREG(3)
          PP1=GPREG(4)
          PP2=GPREG(5)
          REST_KDP(1)=RESTINPT(1)
C
          IF(.NOT.FLAT1) THEN
              SAVE_KDP(1)=SAVEINPT(1)
              STI=0
              SST=0
              SQ=1
              WQ='ACC'
              WC='DR/FR'
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
              W1=1.0D0/ALENS(1,DRAWSURF)
              W2=FNGDIA1
              W3=WAVEL1
              W4=0.0D0
              W5=0.0D0
              CALL DLRPFR
              DLPF1=REG(9)
              REST_KDP(1)=RESTINPT(1)
          END IF
          IF(.NOT.FLAT2) THEN
              SAVE_KDP(1)=SAVEINPT(1)
              STI=0
              SST=0
              SQ=1
              WQ='ACC'
              WC='DR/FR'
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
              W1=1.0D0/ALENS(1,DRAWSURF+1)
              W2=FNGDIA2
              W3=WAVEL1
              W4=0.0D0
              W5=0.0D0
              CALL DLRPFR
              DLPF2=REG(9)
              REST_KDP(1)=RESTINPT(1)
          END IF
          IF(FLAT1) THEN
              SAVE_KDP(1)=SAVEINPT(1)
              STI=0
              SST=0
              SQ=1
              WQ='ACC'
              WC='OUTFLAT'
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
              W1=FNGDIA1
              W2=WAVEL1
              W3=0.0D0
              W4=0.0D0
              W5=0.0D0
              CALL OUTFLT
              DLPF1=REG(9)
              REST_KDP(1)=RESTINPT(1)
          END IF
          IF(FLAT2) THEN
              SAVE_KDP(1)=SAVEINPT(1)
              STI=0
              SST=0
              SQ=1
              WQ='ACC'
              WC='OUTFLAT'
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
              W1=FNGDIA2
              W2=WAVEL1
              W3=0.0D0
              W4=0.0D0
              W5=0.0D0
              CALL OUTFLT
              DLPF2=REG(9)
              REST_KDP(1)=RESTINPT(1)
          END IF
C
C     START THE DRAWING
          SAVE_KDP(1)=SAVEINPT(1)
          INPUT='PLOT NEW'
          CALL PROCES
          REST_KDP(1)=RESTINPT(1)
          PLEXIS=.TRUE.
C
C
C     DO A FRAME 7 BY 10 INCHES
          COLPAS=COLFRM
          CALL MY_COLTYP(COLPAS)
          CALL MY_PLOT(0,0,0,0,-500,10500,-500,7500)
C     DROP PEN, DRAW BOX
          CALL MY_PLOT(10000,0,1,0,-500,10500,-500,7500)
          CALL MY_PLOT(10000,7000,1,0,-500,10500,-500,7500)
          CALL MY_PLOT(0,7000,1,0,-500,10500,-500,7500)
          CALL MY_PLOT(0,0,1,0,-500,10500,-500,7500)
C     RIGHT BOTTOM BOXES
          CALL MY_PLOT(4200,0,0,0,-500,10500,-500,7500)
          CALL MY_PLOT(4200,1650,1,0,-500,10500,-500,7500)
          CALL MY_PLOT(10000,1650,1,0,-500,10500,-500,7500)
          CALL MY_PLOT(4200,1350,0,0,-500,10500,-500,7500)
          CALL MY_PLOT(10000,1350,1,0,-500,10500,-500,7500)
          CALL MY_PLOT(4200,1150,0,0,-500,10500,-500,7500)
          CALL MY_PLOT(10000,1150,1,0,-500,10500,-500,7500)
          CALL MY_PLOT(5700,0,0,0,-500,10500,-500,7500)
          CALL MY_PLOT(5700,1150,1,0,-500,10500,-500,7500)
          CALL MY_PLOT(6700,0,0,0,-500,10500,-500,7500)
          CALL MY_PLOT(6700,1150,1,0,-500,10500,-500,7500)
          CALL MY_PLOT(7900,0,0,0,-500,10500,-500,7500)
          CALL MY_PLOT(7900,1150,1,0,-500,10500,-500,7500)
          CALL MY_PLOT(7900,150,0,0,-500,10500,-500,7500)
          CALL MY_PLOT(10000,150,1,0,-500,10500,-500,7500)
          CALL MY_PLOT(8600,0,0,0,-500,10500,-500,7500)
          CALL MY_PLOT(8600,150,0,0,-500,10500,-500,7500)
          CALL MY_PLOT(9300,0,0,0,-500,10500,-500,7500)
          CALL MY_PLOT(9300,150,0,0,-500,10500,-500,7500)
C
          CALL MY_PLOT(7900,450,0,0,-500,10500,-500,7500)
          CALL MY_PLOT(10000,450,1,0,-500,10500,-500,7500)
          CALL MY_PLOT(8200,150,0,0,-500,10500,-500,7500)
          CALL MY_PLOT(8200,450,1,0,-500,10500,-500,7500)
          CALL MY_PLOT(8920,0,0,0,-500,10500,-500,7500)
          CALL MY_PLOT(8920,450,1,0,-500,10500,-500,7500)
C
          CALL MY_PLOT(6700,225,0,0,-500,10500,-500,7500)
          CALL MY_PLOT(7900,225,1,0,-500,10500,-500,7500)
          CALL MY_PLOT(6700,450,0,0,-500,10500,-500,7500)
          CALL MY_PLOT(7900,450,1,0,-500,10500,-500,7500)
          CALL MY_PLOT(5700,675,0,0,-500,10500,-500,7500)
          CALL MY_PLOT(7900,675,1,0,-500,10500,-500,7500)
          CALL MY_PLOT(6700,900,0,0,-500,10500,-500,7500)
          CALL MY_PLOT(7900,900,1,0,-500,10500,-500,7500)
C
          CALL MY_PLOT(7900,900,0,0,-500,10500,-500,7500)
          CALL MY_PLOT(10000,900,1,0,-500,10500,-500,7500)
C
          CALL MY_PLOT(4200,200,0,0,-500,10500,-500,7500)
          CALL MY_PLOT(5700,200,1,0,-500,10500,-500,7500)
          CALL MY_PLOT(4200,450,0,0,-500,10500,-500,7500)
          CALL MY_PLOT(5700,450,1,0,-500,10500,-500,7500)
          CALL MY_PLOT(4950,200,0,0,-500,10500,-500,7500)
          CALL MY_PLOT(4950,450,1,0,-500,10500,-500,7500)
C
          CALL MY_PLOT(4500,1350,0,0,-500,10500,-500,7500)
          CALL MY_PLOT(4500,1650,1,0,-500,10500,-500,7500)
          CALL MY_PLOT(5000,1350,0,0,-500,10500,-500,7500)
          CALL MY_PLOT(5000,1650,1,0,-500,10500,-500,7500)
          CALL MY_PLOT(6500,1350,0,0,-500,10500,-500,7500)
          CALL MY_PLOT(6500,1650,1,0,-500,10500,-500,7500)
          CALL MY_PLOT(9500,1350,0,0,-500,10500,-500,7500)
          CALL MY_PLOT(9500,1650,1,0,-500,10500,-500,7500)
C
C     OPTICAL AXIS LINE WITH BOX AND -A-
          CALL MY_PLOT(9300,4375,0,0,-500,10500,-500,7500)
          CALL MY_PLOT(5000,4375,1,0,-500,10500,-500,7500)
          CALL MY_PLOT(8250,4375,0,0,-500,10500,-500,7500)
          CALL MY_PLOT(8250,4240,1,0,-500,10500,-500,7500)
          CALL MY_PLOT(8550,4240,1,0,-500,10500,-500,7500)
          CALL MY_PLOT(8550,4375,1,0,-500,10500,-500,7500)
C     LINE CENTER IS AT X=7150, Y=4375
          NT1SIZ=1
          NT1ANG=0
          CALL MY_SETFONT(1,0)
          CALL MY_SETCHARASPECT(1.0,1.0)
          COLPAS=COLFRM
          CALL MY_COLTYP(COLPAS)
          CALL MY_JUSTSTRING(8400,4285,'-A-',NT1ANG,NT1SIZ,2)
C
C
C     LINE 1, PERMANENT BOXED DATA
          NT1SIZ=1
          NT1ANG=0
          CALL MY_SETFONT(1,0)
          COLPAS=COLFRM
          CALL MY_COLTYP(COLPAS)
          CALL MY_JUSTSTRING(4350,1525,'QTY',NT1ANG,NT1SIZ,2)
          CALL MY_JUSTSTRING(5700,1525,'PART NO. OR',NT1ANG,NT1SIZ,2)
          CALL MY_JUSTSTRING(8000,1525,'NOMENCLATURE OR',NT1ANG,NT1SIZ,2)
          CALL MY_JUSTSTRING(9750,1525,'ITEM OR',NT1ANG,NT1SIZ,2)
C
C     LINE 1-1/2, PERMANENT BOXED DATA
          CALL MY_JUSTSTRING(4725,1482,'ID CODE',NT1ANG,NT1SIZ,2)
C
C     LINE 2, PERMANENT BOXED DATA
          CALL MY_JUSTSTRING(4350,1400,'REQD',NT1ANG,NT1SIZ,2)
          CALL MY_JUSTSTRING(5700,1400,'IDENTIFYING NO.',NT1ANG,NT1SIZ,2)
          CALL MY_JUSTSTRING(8000,1400,'DESCRIPTION',NT1ANG,NT1SIZ,2)
          CALL MY_JUSTSTRING(9750,1400,'FIND NO.',NT1ANG,NT1SIZ,2)
C
C     LINE 3, PERMANENT BOXED DATA
          CALL MY_JUSTSTRING(7063,1210,'PARTS LIST',NT1ANG,NT1SIZ,2)
C
C     LINE 4, PERMANENT BOXED DATA
          CALL MY_JUSTSTRING(6720,1035,'CONTRACT:',NT1ANG,NT1SIZ,3)
C
C     LINE 5, PERMANENT BOXED DATA
          CALL MY_JUSTSTRING(6720,785,'DR',NT1ANG,NT1SIZ,3)
C
C     LINE 6, PERMANENT BOXED DATA
          CALL MY_JUSTSTRING(6720,560,'CHK',NT1ANG,NT1SIZ,3)
          CALL MY_JUSTSTRING(5720,570,'MATERIAL',NT1ANG,NT1SIZ,3)
C
C     LINE 7, PERMANENT BOXED DATA
          CALL MY_JUSTSTRING(6720,340,'CHK',NT1ANG,NT1SIZ,3)
          CALL MY_JUSTSTRING(8030,340,'SIZE',NT1ANG,NT1SIZ,2)
          CALL MY_JUSTSTRING(8510,340,'ID CODE',NT1ANG,NT1SIZ,2)
          CALL MY_JUSTSTRING(8945,340,'DWG NO.',NT1ANG,NT1SIZ,3)
C
C     LINE 7-1/2, PERMANENT BOXED DATA
          CALL MY_JUSTSTRING(8030,220,'A',NT1ANG,NT1SIZ,2)

C     LINE 8, PERMANENT BOXED DATA
          CALL MY_JUSTSTRING(6720,110,'APPD',NT1ANG,NT1SIZ,3)
C
C     LINE 9, PERMANENT BOXED DATA
          CALL MY_JUSTSTRING(6175,320,'SEE NOTE 1',NT1ANG,NT1SIZ,2)
C
C     LINE 10, PERMANENT BOXED DATA
          CALL MY_JUSTSTRING(7930,45,'SCALE:',NT1ANG,NT1SIZ,3)
          CALL MY_JUSTSTRING(8955,45,'SHEET',NT1ANG,NT1SIZ,3)
          IF(DABS(POD1).GT.(3.0D0/2.0D0*DABS(ALENS(3,DRAWSURF)))) THEN
              TMPVAL=DABS(POD1)
              IF(SYSTEM1(6).EQ.1.0D0) TMPVAL=TMPVAL*1000.0D0
              IF(SYSTEM1(6).EQ.2.0D0) TMPVAL=TMPVAL*1000.0D0/2.54D0
              IF(SYSTEM1(6).EQ.3.0D0) TMPVAL=TMPVAL*1000.0D0/25.4D0
              IF(SYSTEM1(6).EQ.4.0D0) TMPVAL=TMPVAL*1.0D6/25.4
              TMPVAL=TMPVAL/3000.0D0
          ELSE
              TMPVAL=DABS(ALENS(3,DRAWSURF))
              IF(SYSTEM1(6).EQ.1.0D0) TMPVAL=TMPVAL*1000.0D0
              IF(SYSTEM1(6).EQ.2.0D0) TMPVAL=TMPVAL*1000.0D0/2.54D0
              IF(SYSTEM1(6).EQ.3.0D0) TMPVAL=TMPVAL*1000.0D0/25.4D0
              IF(SYSTEM1(6).EQ.4.0D0) TMPVAL=TMPVAL*1.0D6/25.4
              TMPVAL=TMPVAL/2000.0D0
          END IF
          IF(TMPVAL.EQ.0.0D0) DSCFAC=1.0D0
          IF(TMPVAL.GE.1.0D0) DSCFAC=DINT(TMPVAL)
          IF(TMPVAL.LT.1.0D0.AND.TMPVAL.GT.0.0D0)
     1    DSCFAC=DINT(1.0D0/TMPVAL)
          CALL I3TOA3(INT(DSCFAC),ADSCFAC)
          IF(TMPVAL.GT.1.0D0) SVAL(1:7)='1 /'//ADSCFAC//' X'
          IF(TMPVAL.LT.1.0D0) SVAL(1:7)=ADSCFAC//' X  '
          CALL MY_JUSTSTRING(8280,45,SVAL(1:7),NT1ANG,NT1SIZ,3)
C
C     LINE 11, PERMANENT BOXED DATA
          CALL MY_JUSTSTRING(4600,300,'NEXT ASSY',NT1ANG,NT1SIZ,2)
          CALL MY_JUSTSTRING(5300,300,'USED ON',NT1ANG,NT1SIZ,2)
C
C     LINE 12, PERMANENT BOXED DATA
          CALL MY_JUSTSTRING(4950,70,'APPLICATION',NT1ANG,NT1SIZ,2)
C
C     LINE 13, PERMANENT BOXED DATA (UNITS)
          IF(SYSTEM1(6).EQ.1.0D0)
     1    CALL MY_JUSTSTRING(5725,1050,'UNITS: INCHES',NT1ANG,NT1SIZ,3)
          IF(SYSTEM1(6).EQ.2.0D0)
     1    CALL MY_JUSTSTRING(5725,1050,'UNITS: CM',NT1ANG,NT1SIZ,3)
          IF(SYSTEM1(6).EQ.3.0D0)
     1    CALL MY_JUSTSTRING(5725,1050,'UNITS: MM',NT1ANG,NT1SIZ,3)
          IF(SYSTEM1(6).EQ.4.0D0)
     1    CALL MY_JUSTSTRING(5725,1050,'UNITS: METERS',NT1ANG,NT1SIZ,3)
          CALL MY_JUSTSTRING(5725,925,'ALL ANGLES:',NT1ANG,NT1SIZ,3)
          CALL MY_JUSTSTRING(5725,800,'(DEG,MIN,SEC)',NT1ANG,NT1SIZ,3)

C     LINE 14, TITLE
          CALL MY_JUSTSTRING(8030,990,DTITLE,NT1ANG,NT1SIZ,3)
C
C     LINE 15, CONAME
          CALL MY_JUSTSTRING(8030,665,CONAME,NT1ANG,NT1SIZ,3)
C
C     LINE 16, DWGNO
          CALL MY_JUSTSTRING(8945,215,DWGNO,NT1ANG,NT1SIZ,3)
C
C     UPPER RIGHT REVISIONS BOX
          CALL MY_PLOT(4500,6850,0,0,-500,10500,-500,7500)
          CALL MY_PLOT(10000,6850,1,0,-500,10500,-500,7500)
          CALL MY_PLOT(10000,6650,1,0,-500,10500,-500,7500)
          CALL MY_PLOT(4500,6650,1,0,-500,10500,-500,7500)
          CALL MY_PLOT(4500,6850,1,0,-500,10500,-500,7500)
C
          CALL MY_PLOT(5100,6850,1,0,-500,10500,-500,7500)
          CALL MY_PLOT(5100,6650,1,0,-500,10500,-500,7500)
          CALL MY_PLOT(5100,6850,1,0,-500,10500,-500,7500)
C
          CALL MY_PLOT(5900,6850,1,0,-500,10500,-500,7500)
          CALL MY_PLOT(5900,6650,1,0,-500,10500,-500,7500)
          CALL MY_PLOT(5900,7000,1,0,-500,10500,-500,7500)
          CALL MY_PLOT(5900,6850,1,0,-500,10500,-500,7500)
C
          CALL MY_PLOT(6400,6850,1,0,-500,10500,-500,7500)
          CALL MY_PLOT(6400,6650,1,0,-500,10500,-500,7500)
          CALL MY_PLOT(6400,6850,1,0,-500,10500,-500,7500)
C
          CALL MY_PLOT(8900,6850,1,0,-500,10500,-500,7500)
          CALL MY_PLOT(8900,6650,1,0,-500,10500,-500,7500)
          CALL MY_PLOT(8900,6850,1,0,-500,10500,-500,7500)
C
          CALL MY_PLOT(9450,6850,1,0,-500,10500,-500,7500)
          CALL MY_PLOT(9450,6650,1,0,-500,10500,-500,7500)
          CALL MY_PLOT(9450,6850,1,0,-500,10500,-500,7500)
C
C
C     REVISIONS
          CALL MY_JUSTSTRING(7600,6885,'REVISIONS',NT1ANG,NT1SIZ,3)
C
C     EFF
          CALL MY_JUSTSTRING(4825,6725,'EFF',NT1ANG,NT1SIZ,2)
C
C     AUTHORITY
          CALL MY_JUSTSTRING(5500,6725,'AUTHORITY',NT1ANG,NT1SIZ,2)
C
C     LTR
          CALL MY_JUSTSTRING(6150,6725,'LTR',NT1ANG,NT1SIZ,2)
C
C     DESCRIPTION
          CALL MY_JUSTSTRING(7300,6725,'DESCRIPTION',NT1ANG,NT1SIZ,2)
C
C     DATE
          CALL MY_JUSTSTRING(9150,6725,'DATE',NT1ANG,NT1SIZ,2)
C
C     APPROV
          CALL MY_JUSTSTRING(9700,6725,'APPROV',NT1ANG,NT1SIZ,2)

C
C     UPPER LEFT BOXES
          CALL MY_PLOT(0, 6650,0,0,-500,10500,-500,7500)
          CALL MY_PLOT(4400,6650,1,0,-500,10500,-500,7500)
          CALL MY_PLOT(4400,6450,1,0,-500,10500,-500,7500)
          CALL MY_PLOT(0,6450,1,0,-500,10500,-500,7500)
          CALL MY_PLOT(0,6250,1,0,-500,10500,-500,7500)
          CALL MY_PLOT(4400,6250,1,0,-500,10500,-500,7500)
          CALL MY_PLOT(4400,6650,1,0,-500,10500,-500,7500)

          CALL MY_PLOT(300,6250,0,0,-500,10500,-500,7500)
          CALL MY_PLOT(300,6650,1,0,-500,10500,-500,7500)
          CALL MY_PLOT(1050,6250,0,0,-500,10500,-500,7500)
          CALL MY_PLOT(1050,6650,1,0,-500,10500,-500,7500)
          CALL MY_PLOT(1775,6250,0,0,-500,10500,-500,7500)
          CALL MY_PLOT(1775,6650,1,0,-500,10500,-500,7500)
          CALL MY_PLOT(2275,6250,0,0,-500,10500,-500,7500)
          CALL MY_PLOT(2275,6650,1,0,-500,10500,-500,7500)
          CALL MY_PLOT(2775,6250,0,0,-500,10500,-500,7500)
          CALL MY_PLOT(2775,6650,1,0,-500,10500,-500,7500)
          CALL MY_PLOT(3275,6250,0,0,-500,10500,-500,7500)
          CALL MY_PLOT(3275,6650,1,0,-500,10500,-500,7500)
          CALL MY_PLOT(3775,6250,0,0,-500,10500,-500,7500)
          CALL MY_PLOT(3775,6650,1,0,-500,10500,-500,7500)
C
C     RADIUS
          CALL MY_JUSTSTRING(700,6725,'RADIUS',NT1ANG,NT1SIZ,3)
C
C     RAD TOL
          CALL MY_JUSTSTRING(1400,6850,'RADIUS',NT1ANG,NT1SIZ,3)
          CALL MY_JUSTSTRING(1400,6725,CHAR(177)//
     1    'TOLR.',NT1ANG,NT1SIZ,2,3)
C
C     RAD TOL/FR

          CALL MY_JUSTSTRING(2000,6850,'RAD TOL',NT1ANG,NT1SIZ,3)
          CALL MY_JUSTSTRING(2000,6725,CHAR(177)//
     1    'FRINGE',NT1ANG,NT1SIZ,2,3)
C
C     IRREG FRINGE

          CALL MY_JUSTSTRING(2500,6850,'IRRG',NT1ANG,NT1SIZ,2)
          CALL MY_JUSTSTRING(2500,6725,CHAR(177)//
     1    'FRINGE',NT1ANG,NT1SIZ,2,3)
C
C     SAG TO FLAT

          CALL MY_JUSTSTRING(3025,6850,'SAG TO',NT1ANG,NT1SIZ,2)
          CALL MY_JUSTSTRING(3025,6725,'FLAT',NT1ANG,NT1SIZ,2)
C
C     SAG TOL
          CALL MY_JUSTSTRING(3500,6850,'SAG',NT1ANG,NT1SIZ,2)
          CALL MY_JUSTSTRING(3500,6725,CHAR(177)//
     1    'TOLR.',NT1ANG,NT1SIZ,2,3)
C
C     PERP TOL
          CALL MY_JUSTSTRING(4000,6850,'NORM.',NT1ANG,NT1SIZ,2)
          CALL MY_JUSTSTRING(4000,6725,CHAR(177)//
     1    'TOLR.',NT1ANG,NT1SIZ,2,3)
C
C     R1
          CALL MY_JUSTSTRING(125,6525,'R1',NT1ANG,NT1SIZ,2)
C
C     R2
          CALL MY_JUSTSTRING(125,6325,'R2',NT1ANG,NT1SIZ,2)
C
          CALL MY_SETCHARASPECT(1.0,1.0)
C     RAD1 VALUE
          IF(ALENS(1,DRAWSURF).NE.0.0D0)
     1    SVAL=DRAWVL1(1.0D0/DABS(ALENS(1,DRAWSURF)))
          IF(ALENS(1,DRAWSURF).LT.0.0D0)

     1    CALL MY_JUSTSTRING(275,6525,SVAL//'CV',NT1ANG,NT1SIZ,3)
          IF(ALENS(1,DRAWSURF).GT.0.0D0)
     1    CALL MY_JUSTSTRING(275,6525,SVAL//'CX',NT1ANG,NT1SIZ,3)
          IF(ALENS(1,DRAWSURF).EQ.0.0D0)
     1    CALL MY_JUSTSTRING(600,6525,'INF.',NT1ANG,NT1SIZ,2)
C
C     RADTOL1 VALUE
          IF(.NOT.FLAT1) THEN
              IF(FRINTOL1) THEN
                  RADTOL1=RADTOL1*DLPF1
              END IF
              SVAL=DRAWVL1(DABS(RADTOL1))

              CALL MY_JUSTSTRING(1075,6525,SVAL,NT1ANG,NT1SIZ,3)
          ELSE
              CALL MY_PLOT(1050,6450,0,0,-500,10500,-500,7500)
              CALL MY_PLOT(1775,6650,1,0,-500,10500,-500,7500)
          END IF
C
C     RADTOL1 VALUE/FRINGES
          IF(.NOT.FLAT1) THEN
              RADTOL1=RADTOL1/DLPF1
              SVAL=DRAWVL1(DABS(RADTOL1))
              CALL MY_JUSTSTRING(1575,6525,SVAL,NT1ANG,NT1SIZ,3)
          ELSE
              CALL MY_PLOT(1775,6450,0,0,-500,10500,-500,7500)
              CALL MY_PLOT(2275,6650,1,0,-500,10500,-500,7500)
          END IF
C
C     FRNG1 VALUE
          SVAL=DRAWVL1(FRNG1)

          CALL MY_JUSTSTRING(2050,6525,SVAL,NT1ANG,NT1SIZ,3)
          CV1=ALENS(1,DRAWSURF)
          CV2=ALENS(1,DRAWSURF+1)
C
C     SAG TO FLAT 1 VALUE
          IF(PODA2.LT.PODA1.AND.CV1.NE.0.0D0) THEN
              SVAL=DRAWVL1(PODA2)

              CALL MY_JUSTSTRING(2525,6525,SVAL,NT1ANG,NT1SIZ,3)
          ELSE
              CALL MY_PLOT(2775,6450,0,0,-500,10500,-500,7500)
              CALL MY_PLOT(3275,6650,1,0,-500,10500,-500,7500)
          END IF
C
          IF(PODA2.LT.PODA1.AND.CV1.NE.0.0D0) THEN
C     SAGTOL1
              SVAL=DRAWVL1(SAGTL1)

              CALL MY_JUSTSTRING(3025,6525,SVAL,NT1ANG,NT1SIZ,3)
          ELSE
              CALL MY_PLOT(3275,6450,0,0,-500,10500,-500,7500)
              CALL MY_PLOT(3775,6650,1,0,-500,10500,-500,7500)
          END IF
C
C     PRPNTL1
          IF(PODA2.LT.PODA1.OR.FLAT1) THEN
              SVAL(1:9)=DRAWVL2(DABS(PRPNTL1))
              CALL MY_JUSTSTRING(4050,6525,SVAL(1:9)//'SEC'
     1        ,NT1ANG,NT1SIZ,2,3)
          ELSE
              CALL MY_PLOT(3775,6450,0,0,-500,10500,-500,7500)
              CALL MY_PLOT(4400,6650,1,0,-500,10500,-500,7500)
          END IF
C
C
C     RAD2 VALUE
          IF(ALENS(1,DRAWSURF+1).NE.0.0D0)
     1    SVAL=DRAWVL1(1.0D0/DABS(ALENS(1,DRAWSURF+1)))
          IF(ALENS(1,DRAWSURF+1).LT.0.0D0)
     1    CALL MY_JUSTSTRING(325,6325,SVAL//' CX',NT1ANG,NT1SIZ,3)
          IF(ALENS(1,DRAWSURF+1).GT.0.0D0)
     1    CALL MY_JUSTSTRING(325,6325,SVAL//' CV',NT1ANG,NT1SIZ,3)
          IF(ALENS(1,DRAWSURF+1).EQ.0.0D0)
     1    CALL MY_JUSTSTRING(650,6325,'INF.',NT1ANG,NT1SIZ,2)
C
C     RADTOL2 VALUE
          IF(.NOT.FLAT2) THEN
              IF(FRINTOL2) THEN
                  RADTOL2=RADTOL2*DLPF2
              END IF
              SVAL=DRAWVL1(DABS(RADTOL2))
              CALL MY_JUSTSTRING(1075,6325,SVAL,NT1ANG,NT1SIZ,3,3)
          ELSE
              CALL MY_PLOT(1050,6250,0,0,-500,10500,-500,7500)
              CALL MY_PLOT(1775,6450,1,0,-500,10500,-500,7500)
          END IF
C
C     RADTOL2 VALUE/FRINGES
          IF(.NOT.FLAT2) THEN
              RADTOL2=RADTOL2/DLPF2
              SVAL=DRAWVL1(DABS(RADTOL2))
              CALL MY_JUSTSTRING(1575,6325,SVAL,NT1ANG,NT1SIZ,3,3)
          ELSE
              CALL MY_PLOT(1775,6250,0,0,-500,10500,-500,7500)
              CALL MY_PLOT(2275,6450,1,0,-500,10500,-500,7500)
          END IF
C
C
C     FRNG2 VALUE
          SVAL=DRAWVL1(FRNG2)
          CALL MY_JUSTSTRING(2050,6325,SVAL,NT1ANG,NT1SIZ,3,3)
C
C     SAG TO FLAT 2 VALUE
          IF(PODB2.LT.PODB1.AND.CV2.NE.0.0D0) THEN
              SVAL=DRAWVL1(PODB2)
              CALL MY_JUSTSTRING(2525,6325,SVAL,NT1ANG,NT1SIZ,3,3)
          ELSE
              CALL MY_PLOT(2775,6250,0,0,-500,10500,-500,7500)
              CALL MY_PLOT(3275,6450,1,0,-500,10500,-500,7500)
          END IF
          IF(PODB2.LT.PODB1.AND.CV2.NE.0.0D0) THEN
C     SAGTOL2
              SVAL=DRAWVL1(SAGTL2)
              CALL MY_JUSTSTRING(3025,6325,SVAL,NT1ANG,NT1SIZ,3,3)
          ELSE
              CALL MY_PLOT(3275,6250,0,0,-500,10500,-500,7500)
              CALL MY_PLOT(3775,6450,1,0,-500,10500,-500,7500)
          END IF
C
C     PRPNTL2
          IF(PODB2.LT.PODB1.OR.FLAT2) THEN
              SVAL(1:9)=DRAWVL2(DABS(PRPNTL2))
              CALL MY_JUSTSTRING(4050,6325,SVAL(1:9)//'SEC',NT1ANG,

     1        NT1SIZ,2)
          ELSE
              CALL MY_PLOT(3775,6250,0,0,-500,10500,-500,7500)
              CALL MY_PLOT(4400,6450,1,0,-500,10500,-500,7500)
          END IF
C
C     DIAMETER BOXES
          CALL MY_PLOT(0,5750,0,0,-500,10500,-500,7500)
          CALL MY_PLOT(2800,5750,1,0,-500,10500,-500,7500)
          CALL MY_PLOT(2800,5550,1,0,-500,10500,-500,7500)
          CALL MY_PLOT(0,5550,1,0,-500,10500,-500,7500)

          CALL MY_PLOT(800,5750,0,0,-500,10500,-500,7500)
          CALL MY_PLOT(800,5550,1,0,-500,10500,-500,7500)
          CALL MY_PLOT(1425,5750,0,0,-500,10500,-500,7500)
          CALL MY_PLOT(1425,5550,1,0,-500,10500,-500,7500)
          CALL MY_PLOT(2125,5750,0,0,-500,10500,-500,7500)
          CALL MY_PLOT(2125,5550,1,0,-500,10500,-500,7500)
C
          CALL MY_SETCHARASPECT(1.0,1.0)
C     OUTER DIA

          CALL MY_JUSTSTRING(425,5825,'OUTER DIA.',NT1ANG,NT1SIZ,2)
C     DIA TOL
          CALL MY_JUSTSTRING(1100,5950,'DIA.',NT1ANG,NT1SIZ,2)
          CALL MY_JUSTSTRING(1100,5825,CHAR(177)//
     1    'TOLR.',NT1ANG,NT1SIZ,2)
C     CENTER THICKNESS
          CALL MY_JUSTSTRING(1800,5950,'CENTER',NT1ANG,NT1SIZ,3)
          CALL MY_JUSTSTRING(1800,5825,'THICKNESS',NT1ANG,NT1SIZ,3)
C     THI TOL
          CALL MY_JUSTSTRING(2375,5950,'THI.',NT1ANG,NT1SIZ,2)
          CALL MY_JUSTSTRING(2375,5825,CHAR(177)//
     1    'TOLR.',NT1ANG,NT1SIZ,2)
C
          CALL MY_SETCHARASPECT(1.0,1.0)
C     OUTER DIA VAL
          SVAL=DRAWVL1(POD1)

          CALL MY_JUSTSTRING(25,5625,SVAL,NT1ANG,NT1SIZ,3)
C     DIA TOL VALUE
          SVAL=DRAWVL1(DIATOL)
          CALL MY_JUSTSTRING(625,5625,SVAL,NT1ANG,NT1SIZ,3)
C     CENTER THICKNESS VALUE
          SVAL=DRAWVL1(DABS(ALENS(3,DRAWSURF)))
          CALL MY_JUSTSTRING(1300,5625,SVAL,NT1ANG,NT1SIZ,3)
C     CENTER THICKNESS TOL VALUE
          SVAL=DRAWVL1(THITOL)
          CALL MY_JUSTSTRING(1925,5625,SVAL,NT1ANG,NT1SIZ,3)
C
          CALL MY_SETCHARASPECT(1.0,1.0)
C     UNLESS OTHERWISE NOTE:
          CALL MY_JUSTSTRING(50,5200,'UNLESS OTHERWISE NOTED:'

     1    ,NT1ANG,NT1SIZ,3)
          YPOS=5150
C     DO THE NOTES
          DO I=1,25
C     I=1
C     NOTE 1
              IF(I.EQ.1) THEN
                  FLNM='NOTES\NOTE1D.DAT'
                  IF(GLANAM(DRAWSURF,2).EQ.'IRTRAN2   ') FLNM='NOTES\NOTE1A.DAT'
                  IF(GLANAM(DRAWSURF,2).EQ.'ZNS       ') FLNM='NOTES\NOTE1A.DAT'
                  IF(GLANAM(DRAWSURF,2).EQ.'GERMPC    ') FLNM='NOTES\NOTE1B.DAT'
                  IF(GLANAM(DRAWSURF,2).EQ.'ZNSE      ') FLNM='NOTES\NOTE1C.DAT'
                  IF(GLANAM(DRAWSURF,2).EQ.'IRTRAN4   ') FLNM='NOTES\NOTE1C.DAT'
                  IF(GLANAM(DRAWSURF,2).EQ.'AMTIR1    ') FLNM='NOTES\NOTE1E.DAT'
                  IF(GLANAM(DRAWSURF,1).EQ.'SCHOTT       '.OR.
     1            GLANAM(DRAWSURF,1).EQ.'SCH2000      '.OR.
     1            GLANAM(DRAWSURF,1).EQ.'HOYA         '.OR.
     1            GLANAM(DRAWSURF,1).EQ.'HIKARI       '.OR.
     2            GLANAM(DRAWSURF,1).EQ.'OHARA        '.OR.
     3            GLANAM(DRAWSURF,1).EQ.'CHANCE       '.OR.
     4            GLANAM(DRAWSURF,1).EQ.'CORNIN       '.OR.
     5            GLANAM(DRAWSURF,1).EQ.'RADHARD      ') FLNM='NOTES\NOTE1F.DAT'
                  EXIS56=.FALSE.
                  OPEN56=.FALSE.
                  INQUIRE(FILE=trim(HOME)//'NOTES/NOTE1F.DAT',EXIST=EXIS56)
                  INQUIRE(FILE=trim(HOME)//'NOTES/NOTE1F.DAT',OPENED=OPEN56)
                  IF(OPEN56) CALL CLOSE_FILE(56,1)
                  OPEN(UNIT=56,ACCESS='SEQUENTIAL',BLANK='NULL'
     1            ,FORM='FORMATTED',FILE=FLNM
     2            ,STATUS='UNKNOWN')
                  READ(56,*) ICNT
                  DO K=1,ICNT
                      READ(56,100) LINEOF(1:72)
100                   FORMAT(A72)
                      IF(FLNM.EQ.trim(HOME)//'NOTES/NOTE1F.DAT'.AND.K.EQ.1) THEN
                          IF(GLSCD1.EQ.0) GLSCD1=INT(ALENS(122,DRAWSURF))
                          IF(GLSCD2.EQ.0) GLSCD2=INT(ALENS(123,DRAWSURF))
                          CALL GLSCON(G1,GLSCD1)
                          CALL GLSCON(G2,GLSCD2)
                          LINEOF(1:72)=LINEOF(1:32)//' '//G1//'-'//G2//' '//LINEOF(33:63)
                      END IF
                      YPOS=YPOS-YSTEP
                      CALL MY_JUSTSTRING(50,YPOS,LINEOF(1:72)
     1                ,NT1ANG,NT1SIZ,3)
                  END DO
                  CALL CLOSE_FILE(56,1)
              ELSE
C     NOT NOTE 1 SERIES
              END IF
C
C     I=2 TO 8
C     NOTE 2 TO 8
              IF(I.GE.2.AND.I.LE.8) THEN
                  IF(I.EQ.2) FLNM='NOTES\NOTE2.DAT   '
                  IF(I.EQ.3) FLNM='NOTES\NOTE3.DAT   '
                  IF(I.EQ.4) FLNM='NOTES\NOTE4.DAT   '
                  IF(I.EQ.5) FLNM='NOTES\NOTE5.DAT   '
                  IF(I.EQ.6.AND.FLAT1.AND.FLAT2) THEN
                      FLNM=trim(HOME)//'NOTES/NOTE6A.DAT   '
                  END IF
                  IF(I.EQ.6.AND.FLAT1.AND..NOT.FLAT2) THEN
                      FLNM=trim(HOME)//'NOTES/NOTE6B.DAT   '
                  END IF
                  IF(I.EQ.6.AND..NOT.FLAT1.AND.FLAT2) THEN
                      FLNM=trim(HOME)//'NOTES/NOTE6C.DAT   '
                  END IF
                  IF(I.EQ.6.AND..NOT.FLAT1.AND..NOT.FLAT2) THEN
                      FLNM=trim(HOME)//'NOTES/NOTE6D.DAT   '
                  END IF
                  IF(I.EQ.7) FLNM=trim(HOME)//'NOTES/NOTE7.DAT   '
                  IF(I.EQ.8) FLNM=trim(HOME)//'NOTES/NOTE8.DAT   '
                  EXIS56=.FALSE.
                  OPEN56=.FALSE.
                  INQUIRE(FILE=FLNM,EXIST=EXIS56)
                  INQUIRE(FILE=FLNM,OPENED=OPEN56)
                  IF(EXIS56.AND.OPEN56) CALL CLOSE_FILE(56,1)
                  IF(EXIS56) THEN
                      OPEN(UNIT=56,ACCESS='SEQUENTIAL',BLANK='NULL'
     1                ,FORM='FORMATTED',FILE=FLNM
     2                ,STATUS='UNKNOWN')
                      READ(56,*) ICNT
                      DO K=1,ICNT
 20                       FORMAT(G13.6)
 21                       FORMAT(G11.4)
                          READ(56,100) LINEOF(1:72)
                          IF(I.EQ.4) THEN
                              IF(K.EQ.1) THEN
                                  WRITE(OUTLYNE,20) CLP1
                                  LINEOF(1:72)=LINEOF(1:30)//' '//OUTLYNE(1:41)
                              END IF
                              IF(K.EQ.2) THEN
                                  WRITE(OUTLYNE,20) CLP2
                                  LINEOF(1:72)=LINEOF(1:30)//' '//OUTLYNE(1:41)
                              END IF
                          END IF
                          IF(I.EQ.5) THEN
                              LINEOF(1:72)=LINEOF(1:38)//' '//SURQUAL
                          END IF
                          IF(I.EQ.8) THEN
                              IF(K.EQ.1) THEN
                                  WRITE(OUTLYNE,20) CENTIR
                                  LINEOF(1:72)=LINEOF(1:37)//' '//OUTLYNE(1:34)
                              END IF
                              IF(K.EQ.2) THEN
                                  IF(CLP1.GE.CLP2) WRITE(OUTLYNE,20) CLP1
                                  IF(CLP1.LT.CLP2) WRITE(OUTLYNE,20) CLP2
                                  LINEOF(1:72)=LINEOF(1:24)//' '//OUTLYNE(1:47)
                              END IF
                          END IF
C
                          IF(I.EQ.6.AND.FLAT1.AND.FLAT2) THEN
C     BOTH SURFACES FLAT
                              IF(K.EQ.4) THEN
                                  WRITE(OUTLYNE,21) WAVEL1
                                  LINEOF(1:72)=LINEOF(1:59)//' '//OUTLYNE(1:11)
                              END IF
                          END IF
                          IF(I.EQ.6.AND.FLAT1.AND..NOT.FLAT2) THEN
C     FLAT/NOT FLAT
                              IF(K.EQ.3) THEN
                                  WRITE(OUTLYNE,21) WAVEL1
                                  LINEOF(1:72)=LINEOF(1:59)//' '//OUTLYNE(1:11)
                              END IF
                              IF(K.EQ.12) THEN
                                  WRITE(OUTLYNE,20) CLP1
                                  LINEOF(1:72)=LINEOF(1:20)//' '//OUTLYNE(1:13)
                              END IF
                              IF(K.EQ.15) THEN
                                  WRITE(OUTLYNE,20) DLPF2
                                  LINEOF(1:72)=LINEOF(1:20)//' '//OUTLYNE(1:13)
                              END IF
                              IF(K.EQ.16) THEN
                                  WRITE(OUTLYNE,20) CLP2
                                  LINEOF(1:72)=LINEOF(1:20)//' '//OUTLYNE(1:13)
                              END IF
                          END IF
                          IF(I.EQ.6.AND..NOT.FLAT1.AND.FLAT2) THEN
C     NOT FLAT/FLAT
                              IF(K.EQ.3) THEN
                                  WRITE(OUTLYNE,21) WAVEL1
                                  LINEOF(1:72)=LINEOF(1:59)//' '//OUTLYNE(1:11)
                              END IF
                              IF(K.EQ.11) THEN
                                  WRITE(OUTLYNE,20) DLPF1
                                  LINEOF(1:72)=LINEOF(1:20)//' '//OUTLYNE(1:13)
                              END IF
                              IF(K.EQ.12) THEN
                                  WRITE(OUTLYNE,20) CLP1
                                  LINEOF(1:72)=LINEOF(1:20)//' '//OUTLYNE(1:13)
                              END IF
                              IF(K.EQ.16) THEN
                                  WRITE(OUTLYNE,20) CLP2
                                  LINEOF(1:72)=LINEOF(1:20)//' '//OUTLYNE(1:13)
                              END IF
                          END IF
                          IF(I.EQ.6.AND..NOT.FLAT1.AND..NOT.FLAT2) THEN
C     NOT FLAT/NOT FLAT
                              IF(K.EQ.3) THEN
                                  WRITE(OUTLYNE,21) WAVEL1
                                  LINEOF(1:72)=LINEOF(1:59)//' '//OUTLYNE(1:11)
                              END IF
                              IF(K.EQ.11) THEN
                                  WRITE(OUTLYNE,20) DLPF1
                                  LINEOF(1:72)=LINEOF(1:20)//' '//OUTLYNE(1:13)
                              END IF
                              IF(K.EQ.12) THEN
                                  WRITE(OUTLYNE,20) CLP1
                                  LINEOF(1:72)=LINEOF(1:20)//' '//OUTLYNE(1:13)
                              END IF
                              IF(K.EQ.15) THEN
                                  WRITE(OUTLYNE,20) DLPF2
                                  LINEOF(1:72)=LINEOF(1:20)//' '//OUTLYNE(1:13)
                              END IF
                              IF(K.EQ.16) THEN
                                  WRITE(OUTLYNE,20) CLP2
                                  LINEOF(1:72)=LINEOF(1:20)//' '//OUTLYNE(1:13)
                              END IF
                          END IF
C
                          YPOS=YPOS-YSTEP
                          CALL MY_JUSTSTRING(50,YPOS,LINEOF(1:72)
     1                    ,NT1ANG,NT1SIZ,3)
                      END DO
                      CALL CLOSE_FILE(56,1)
                  END IF
              END IF
C
C     I=9 TO 25
C     NOTE 9 TO 25
              IF(I.GE.9.AND.I.LE.25) THEN
                  IF(I.EQ.9)  FLNM=trim(HOME)//'NOTES/NOTE9.DAT   '
                  IF(I.EQ.10) FLNM=trim(HOME)//'NOTES/NOTE10.DAT  '
                  IF(I.EQ.11) FLNM=trim(HOME)//'NOTES/NOTE11.DAT  '
                  IF(I.EQ.12) FLNM=trim(HOME)//'NOTES/NOTE12.DAT  '
                  IF(I.EQ.13) FLNM=trim(HOME)//'NOTES/NOTE13.DAT  '
                  IF(I.EQ.14) FLNM=trim(HOME)//'NOTES/NOTE14.DAT  '
                  IF(I.EQ.15) FLNM=trim(HOME)//'NOTES/NOTE15.DAT  '
                  IF(I.EQ.16) FLNM=trim(HOME)//'NOTES/NOTE16.DAT  '
                  IF(I.EQ.17) FLNM=trim(HOME)//'NOTES/NOTE17.DAT  '
                  IF(I.EQ.18) FLNM=trim(HOME)//'NOTES/NOTE18.DAT  '
                  IF(I.EQ.19) FLNM=trim(HOME)//'NOTES/NOTE19.DAT  '
                  IF(I.EQ.20) FLNM=trim(HOME)//'NOTES/NOTE20.DAT  '
                  IF(I.EQ.21) FLNM=trim(HOME)//'NOTES/NOTE21.DAT  '
                  IF(I.EQ.22) FLNM=trim(HOME)//'NOTES/NOTE22.DAT  '
                  IF(I.EQ.23) FLNM=trim(HOME)//'NOTES/NOTE23.DAT  '
                  IF(I.EQ.24) FLNM=trim(HOME)//'NOTES/NOTE24.DAT  '
                  IF(I.EQ.25) FLNM=trim(HOME)//'NOTES/NOTE25.DAT  '
                  EXIS56=.FALSE.
                  OPEN56=.FALSE.
                  INQUIRE(FILE=FLNM,EXIST=EXIS56)
                  INQUIRE(FILE=FLNM,OPENED=OPEN56)
                  IF(EXIS56.AND.OPEN56) CALL CLOSE_FILE(56,1)
                  IF(EXIS56) THEN
                      OPEN(UNIT=56,ACCESS='SEQUENTIAL',BLANK='NULL'
     1                ,FORM='FORMATTED',FILE=FLNM
     2                ,STATUS='UNKNOWN')
                      READ(56,*) ICNT
                      DO K=1,ICNT
                          READ(56,100) LINEOF(1:72)
                          IF(I.EQ.9) THEN
                              WRITE(OUTLYNE,20) BRKEDG
                              LINEOF(1:72)=LINEOF(1:33)//' '//OUTLYNE(1:38)
                          END IF
                          IF(I.EQ.10) THEN
                              LINEOF(1:72)=LINEOF(1:29)//' '//SURCOAT(1:20)//
     1                        '                       '
                          END IF
                          IF(I.EQ.11) THEN
                              IF(K.EQ.1) THEN
                                  WRITE(OUTLYNE,21) WAVEL1
                                  LINEOF(1:72)=LINEOF(1:45)//' '//OUTLYNE(1:36)
                              END IF
                              IF(K.EQ.2) THEN
                                  WRITE(OUTLYNE,20) EFL
                                  LINEOF(1:72)=LINEOF(1:20)//' '//OUTLYNE(1:13)//' '//UN
     1                            //'                                     '
                              END IF
                              IF(K.EQ.3) THEN
                                  WRITE(OUTLYNE,20) BFL
                                  LINEOF(1:72)=LINEOF(1:20)//' '//OUTLYNE(1:13)//' '//UN
     1                            //'                                     '
                              END IF
                              IF(K.EQ.4) THEN
                                  WRITE(OUTLYNE,20) FFL
                                  LINEOF(1:72)=LINEOF(1:20)//' '//OUTLYNE(1:13)//' '//UN
     1                            //'                                     '
                              END IF
                          END IF
                          YPOS=YPOS-YSTEP
                          CALL MY_JUSTSTRING(50,YPOS,LINEOF(1:72)
     1                    ,NT1ANG,NT1SIZ,3)
                      END DO
                      CALL CLOSE_FILE(56,1)
                  END IF
              END IF
C
          END DO
C
C     NOW DRAW THE LENS IN PLACE
          CALL DRAWLENS(DRAWSURF,DSCFAC,PODA1,PODA2,
     1    PODB1,PODB2,DERROR)
          IF(DERROR) THEN
              OUTLYNE=
     1        'SAG CALCULATION ERROR OCCURED, PART DRAWING ABORTED'
              CALL SHOWIT(1)
              PARTISDRAWING=.FALSE.
              CALL MACFAL
              RETURN
          END IF
C
          RETURN
      END


      SUBROUTINE DRAW_INITIALIZE
          IMPLICIT NONE
C
          INTEGER I,J,GLSCD1,GLSCD2
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          INCLUDE 'datpts.inc'
          PARTISDRAWING=.FALSE.
          TEST1=0.0D0
          TEST2=0.0D0
          PODA1=0.0D0
          PODA2=0.0D0
          PODB1=0.0D0
          PODB2=0.0D0
C
C     FIX DEFAULT VALUES
C
C     OD, THE OUTSIDE DIAMETER AND FLAT DIAMETER VALUES
C     FOR SURFACES DRAWSURF AND DRAWSURF+1
          DO I=1,2
              IF(I.EQ.1) J=DRAWSURF
              IF(I.EQ.2) J=DRAWSURF+1
              IF(ALENS(9,J).EQ.0.0D0.OR.
     1        ALENS(9,J).EQ.2.0D0.OR.
     2        ALENS(9,J).EQ.3.0D0.OR.
     3        ALENS(9,J).EQ.4.0D0.OR.
     4        ALENS(9,J).EQ.5.0D0) THEN
                  TEST1=DABS(PXTRAY(1,J))+DABS(PXTRAY(5,J))
                  TEST2=DABS(PXTRAX(1,J))+DABS(PXTRAX(5,J))
                  IF(TEST1.GE.TEST2) THEN
                      IF(I.EQ.1) THEN
                          PODA1=TEST1
                          PODA2=TEST1
                      ELSE
                          PODB1=TEST1
                          PODB2=TEST1
                      END IF
                  ELSE
                      IF(I.EQ.1) THEN
                          PODA1=TEST2
                          PODA2=TEST2
                      ELSE
                          PODB1=TEST2
                          PODB2=TEST2
                      END IF
                  END IF
              ELSE
C     MUST BE CIRCULAR
                  IF(I.EQ.1) THEN
                      PODA1=DABS(ALENS(10,J))
                      PODA2=DABS(ALENS(11,J))
                  ELSE
                      PODB1=DABS(ALENS(10,J))
                      PODB2=DABS(ALENS(11,J))
                  END IF
              END IF
          END DO
          IF(PODA1.GE.PODB1) POD1=PODA1
          IF(PODA1.LT.PODB1) POD1=PODB1
          IF(PODA2.GE.PODB2) POD2=PODA2
          IF(PODA2.LT.PODB2) POD2=PODB2
          POD1=2.0D0*POD1
          POD2=2.0D0*POD2
C
C     DIATOL
          IF(SYSTEM1(6).EQ.1.0D0) DIATOL=0.002D0
          IF(SYSTEM1(6).EQ.2.0D0) DIATOL=0.002D0*2.54D0
          IF(SYSTEM1(6).EQ.3.0D0) DIATOL=0.002D0*25.4D0
          IF(SYSTEM1(6).EQ.4.0D0) DIATOL=(0.002D0*25.4D0)/1000.0D0
C
C     RADTOL
          IF(ALENS(1,DRAWSURF).EQ.0.0D0)   RADTOL1=4.0D0
          IF(ALENS(1,DRAWSURF+1).EQ.0.0D0) RADTOL2=4.0D0
          IF(ALENS(1,DRAWSURF).NE.0.0D0)
     1    RADTOL1=0.001D0/ALENS(1,DRAWSURF)
          IF(ALENS(1,DRAWSURF+1).NE.0.0D0)
     1    RADTOL2=0.001D0/ALENS(1,DRAWSURF+1)
          FRINTOL1=.FALSE.
          FRINTOL2=.FALSE.
C
C     IRREGULARITY FRINGES
          FRNG1=1.0D0
          FRNG2=1.0D0
C
C     THICKNESS TOLERANCE
          IF(SYSTEM1(6).EQ.1.0D0) THITOL=0.002D0
          IF(SYSTEM1(6).EQ.2.0D0) THITOL=0.002D0*2.54D0
          IF(SYSTEM1(6).EQ.3.0D0) THITOL=0.002D0*25.4D0
          IF(SYSTEM1(6).EQ.4.0D0) THITOL=(0.002D0*25.4D0)/1000.0D0
C
C     CLEARAP FOR NOTES ONLY
          IF(.NOT.CLP1SET) THEN
              IF(SYSTEM1(6).EQ.1.0D0) CLP1=(2.0D0*PODA1)-0.08D0
              IF(SYSTEM1(6).EQ.2.0D0) CLP1=(2.0D0*PODA1)-(0.08D0*2.54D0)
              IF(SYSTEM1(6).EQ.3.0D0) CLP1=(2.0D0*PODA1)-(0.08D0*25.4D0)
              IF(SYSTEM1(6).EQ.4.0D0) CLP1=(2.0D0*PODA1)-
     1        ((0.08D0*25.4D0)/1000.0D0)
          END IF
          IF(.NOT.CLP2SET) THEN
              IF(SYSTEM1(6).EQ.1.0D0) CLP2=(2.0D0*PODB1)-0.08D0
              IF(SYSTEM1(6).EQ.2.0D0) CLP2=(2.0D0*PODB1)-(0.08D0*2.54D0)
              IF(SYSTEM1(6).EQ.3.0D0) CLP2=(2.0D0*PODB1)-(0.08D0*25.4D0)
              IF(SYSTEM1(6).EQ.4.0D0) CLP2=(2.0D0*PODB1)-
     1        ((0.08D0*25.4D0)/1000.0D0)
          END IF
C
C     SURFQUAL
          SURQUAL='(NOT SPECIFIED)     '
C
C     FRINGE DIAMETER
          FNGDIA1=CLP1
          FNGDIA2=CLP2
C
C     DEV FOR CENTERING
          CENTIR=0.002D0
C
C     EDGE BREAK
          IF(SYSTEM1(6).EQ.1.0D0) BRKEDG=0.002D0
          IF(SYSTEM1(6).EQ.2.0D0) BRKEDG=0.002D0*2.54D0
          IF(SYSTEM1(6).EQ.3.0D0) BRKEDG=0.002D0*25.4D0
          IF(SYSTEM1(6).EQ.4.0D0) BRKEDG=(0.002D0*25.4D0)/1000.0D0
C
C     COAT
          SURCOAT='(UNCOATED)          '
C
C     SAGTOL
          IF(SYSTEM1(6).EQ.1.0D0) SAGTL1=(0.002D0)
          IF(SYSTEM1(6).EQ.2.0D0) SAGTL1=(0.002D0*2.54D0)
          IF(SYSTEM1(6).EQ.3.0D0) SAGTL1=(0.002D0*25.4D0)
          IF(SYSTEM1(6).EQ.4.0D0) SAGTL1=((0.002D0*25.4D0)/1000.0D0)
          IF(SYSTEM1(6).EQ.1.0D0) SAGTL2=(0.002D0)
          IF(SYSTEM1(6).EQ.2.0D0) SAGTL2=(0.002D0*2.54D0)
          IF(SYSTEM1(6).EQ.3.0D0) SAGTL2=(0.002D0*25.4D0)
          IF(SYSTEM1(6).EQ.4.0D0) SAGTL2=((0.002D0*25.4D0)/1000.0D0)
C
C     PRPNTL
          PRPNTL1=1.0D0
          PRPNTL2=1.0D0
C
C     TITLE
          DTITLE='                                '
C
C     DWGNO
          DWGNO='            '
C
C     CONAME
          CONAME='                                '
C
C     SURMATL
          SURMATL='             '
C
C     WAVEL
          WAVEL1=0.5461D0
C
          GLSCD1=INT(ALENS(122,DRAWSURF))
          GLSCD2=INT(ALENS(123,DRAWSURF))
          PARTISDRAWING=.TRUE.
C
          RETURN
      END


C SUB DRAWVL1.FOR
      FUNCTION DRAWVL1(VAL)
C
          IMPLICIT NONE
C
          REAL*8 VAL
C
          INTEGER I
C
          CHARACTER B*140,DRAWVL1*12
C
          WRITE(B,100) SNGL(VAL)
          READ(B,110) DRAWVL1
 100      FORMAT(F12.5)
 110      FORMAT(A12)
          DO I=1,12
              IF(DRAWVL1(1:1).EQ.' ')DRAWVL1(1:12)=DRAWVL1(1:11)//' '
          END DO
          RETURN
      END
C SUB DRAWVL2.FOR
      FUNCTION DRAWVL2(VAL)
C
          IMPLICIT NONE
C
          REAL*8 VAL
C
          INTEGER I
C
          CHARACTER B*140,DRAWVL2*9
C
          WRITE(B,100) SNGL(VAL)
          READ(B,110) DRAWVL2
 100      FORMAT(F9.3)
 110      FORMAT(A9)
          DO I=1,9
              IF(DRAWVL2(1:1).EQ.' ')DRAWVL2(1:9)=DRAWVL2(1:8)//' '
          END DO
          RETURN
      END
C SUB GLSCON.FOR
      SUBROUTINE GLSCON(AVAL,IVAL)
C
          IMPLICIT NONE
C
          INTEGER IVAL
C
          CHARACTER B*140,AVAL*3
C
          WRITE(B,100) IVAL
          READ(B,110)  AVAL
 100      FORMAT(I3)
 110      FORMAT(A3)
          RETURN
      END


      SUBROUTINE DRAWLENS(DRAWSF,SCALE_FACTOR,
     1OD1,ODFLAT1,OD2,ODFLAT2,ERROR)
          IMPLICIT NONE
C
C     INCOMMING OD VALUES ARE RADII
C
          INTEGER COLPAS,DRAWSF,ALLOERR,I,J,K
          REAL*8 DZ1,DZ2,SCALE_FACTOR,OD1,ODFLAT1,OD2,ODFLAT2,OD
          REAL*8 T1,T2,SAGODFLT1,SAGODFLT2,SAGVAL,ODSTEP,Y,X,TH,Z
          REAL*8 CNX,CNY,XV,YV,PX1,PX2,NX1,NX2,PY1,PY2,NY1,NY2
          REAL*8 PXX1,PXX2,PYY1,PYY2,SAGOD1,SAGOD2,CV1,CV2
          REAL*8 NXX1,NXX2,NYY1,NYY2
          LOGICAL ERROR
          DIMENSION SAGVAL(:,:)
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datpts.inc'
          ALLOCATABLE :: SAGVAL
          ALLOCATE (SAGVAL(1:2,1:180),STAT=ALLOERR)
          CV1=ALENS(1,DRAWSF)
          CV2=ALENS(1,DRAWSF+1)
C
C     AXIAL THICKNESS IS:
          IF(SYSTEM1(6).EQ.1.0D0) TH=ALENS(3,DRAWSF)
          IF(SYSTEM1(6).EQ.2.0D0) TH=ALENS(3,DRAWSF)/2.54D0
          IF(SYSTEM1(6).EQ.3.0D0) TH=ALENS(3,DRAWSF)/25.4D0
          IF(SYSTEM1(6).EQ.4.0D0) TH=1000.0D0*ALENS(3,DRAWSF)/25.4D0
C
C     EDGE OFFSETS
          DZ1=ALENS(14,DRAWSF)
          DZ2=ALENS(14,DRAWSF+1)
          IF(SYSTEM1(6).EQ.1) DZ1=DZ1
          IF(SYSTEM1(6).EQ.2) DZ1=DZ1/2.54D0
          IF(SYSTEM1(6).EQ.3) DZ1=DZ1/25.4D0
          IF(SYSTEM1(6).EQ.4) DZ1=1000.0D0*DZ1/25.4D0
          IF(SYSTEM1(6).EQ.1) DZ2=DZ2
          IF(SYSTEM1(6).EQ.2) DZ2=DZ2/2.54D0
          IF(SYSTEM1(6).EQ.3) DZ2=DZ2/25.4D0
          IF(SYSTEM1(6).EQ.4) DZ2=1000.0D0*DZ2/25.4D0
C
C     PART OD
          IF(OD1.GE.OD2) OD2=OD1
          IF(OD1.LT.OD2) OD1=OD2
          OD=OD1
C     SAG AT OD
          X=0.0D0
          Y=OD
          ERROR=.FALSE.
          CALL SAGRET(DRAWSF,X,Y,Z,ERROR)
          IF(ERROR) RETURN
          IF(SYSTEM1(6).EQ.1.0D0) SAGOD1=Z
          IF(SYSTEM1(6).EQ.2.0D0) SAGOD1=Z/2.54D0
          IF(SYSTEM1(6).EQ.3.0D0) SAGOD1=Z/25.4D0
          IF(SYSTEM1(6).EQ.4.0D0) SAGOD1=1000.0D0*Z/25.4D0
          X=0.0D0
          Y=OD
          ERROR=.FALSE.
          CALL SAGRET(DRAWSF+1,X,Y,Z,ERROR)
          IF(ERROR) RETURN
          IF(SYSTEM1(6).EQ.1.0D0) SAGOD2=Z
          IF(SYSTEM1(6).EQ.2.0D0) SAGOD2=Z/2.54D0
          IF(SYSTEM1(6).EQ.3.0D0) SAGOD2=Z/25.4D0
          IF(SYSTEM1(6).EQ.4.0D0) SAGOD2=1000.0D0*Z/25.4D0
C
C     SAG AT RADIUS TO FLAT
          X=0.0D0
          Y=ODFLAT1
          ERROR=.FALSE.
          CALL SAGRET(DRAWSF,X,Y,Z,ERROR)
          IF(ERROR) RETURN
          IF(SYSTEM1(6).EQ.1.0D0) SAGODFLT1=Z
          IF(SYSTEM1(6).EQ.2.0D0) SAGODFLT1=Z/2.54D0
          IF(SYSTEM1(6).EQ.3.0D0) SAGODFLT1=Z/25.4D0
          IF(SYSTEM1(6).EQ.4.0D0) SAGODFLT1=1000.0D0*Z/25.4D0
          Y=ODFLAT2
          ERROR=.FALSE.
          CALL SAGRET(DRAWSF+1,X,Y,Z,ERROR)
          IF(ERROR) RETURN
          IF(SYSTEM1(6).EQ.1.0D0) SAGODFLT2=Z
          IF(SYSTEM1(6).EQ.2.0D0) SAGODFLT2=Z/2.54D0
          IF(SYSTEM1(6).EQ.3.0D0) SAGODFLT2=Z/25.4D0
          IF(SYSTEM1(6).EQ.4.0D0) SAGODFLT2=1000.0D0*Z/25.4D0
C
C     LOAD SAG ARRAYS WITH DATA REFERENCED TO EACH SURFACE VERTEX
          DO I=1,2
              IF(I.EQ.1) J=DRAWSF
              IF(I.EQ.2) J=DRAWSF+1
              ODSTEP=OD/179.0D0
              X=0.0D0
              Y=0.0D0
              DO K=1,180
                  ERROR=.FALSE.
                  CALL SAGRET(J,X,Y,Z,ERROR)
                  IF(ERROR) RETURN
                  IF(SYSTEM1(6).EQ.1.0D0) SAGVAL(I,K)=Z
                  IF(SYSTEM1(6).EQ.2.0D0) SAGVAL(I,K)=Z/2.54D0
                  IF(SYSTEM1(6).EQ.3.0D0) SAGVAL(I,K)=Z/25.4D0
                  IF(SYSTEM1(6).EQ.4.0D0) SAGVAL(I,K)=1000.0D0*Z/25.4D0
                  Y=Y+ODSTEP
              END DO
          END DO
          IF(SYSTEM1(6).EQ.1.0D0)           OD=OD
          IF(SYSTEM1(6).EQ.1.0D0)           OD1=OD1
          IF(SYSTEM1(6).EQ.1.0D0)           OD2=OD2
          IF(SYSTEM1(6).EQ.1.0D0) ODFLAT1=ODFLAT1
          IF(SYSTEM1(6).EQ.1.0D0) ODFLAT2=ODFLAT2
          IF(SYSTEM1(6).EQ.1.0D0)   ODSTEP=ODSTEP
          IF(SYSTEM1(6).EQ.2.0D0)           OD=OD/2.54D0
          IF(SYSTEM1(6).EQ.2.0D0)           OD1=OD1/2.54D0
          IF(SYSTEM1(6).EQ.2.0D0)           OD2=OD2/2.54D0
          IF(SYSTEM1(6).EQ.2.0D0) ODFLAT1=ODFLAT1/2.54D0
          IF(SYSTEM1(6).EQ.2.0D0) ODFLAT2=ODFLAT2/2.54D0
          IF(SYSTEM1(6).EQ.2.0D0)   ODSTEP=ODSTEP/2.54D0
          IF(SYSTEM1(6).EQ.3.0D0)           OD=OD/25.4D0
          IF(SYSTEM1(6).EQ.3.0D0)           OD1=OD1/25.4D0
          IF(SYSTEM1(6).EQ.3.0D0)           OD2=OD2/25.4D0
          IF(SYSTEM1(6).EQ.3.0D0) ODFLAT1=ODFLAT1/25.4D0
          IF(SYSTEM1(6).EQ.3.0D0) ODFLAT2=ODFLAT2/25.4D0
          IF(SYSTEM1(6).EQ.3.0D0)   ODSTEP=ODSTEP/25.4D0
          IF(SYSTEM1(6).EQ.4.0D0)           OD=1000.0D0*OD/25.4D0
          IF(SYSTEM1(6).EQ.4.0D0)           OD1=1000.0D0*OD1/25.4D0
          IF(SYSTEM1(6).EQ.4.0D0)           OD2=1000.0D0*OD2/25.4D0
          IF(SYSTEM1(6).EQ.4.0D0) ODFLAT1=1000.0D0*ODFLAT1/25.4D0
          IF(SYSTEM1(6).EQ.4.0D0) ODFLAT2=1000.0D0*ODFLAT2/25.4D0
          IF(SYSTEM1(6).EQ.4.0D0)   ODSTEP=1000.0D0*ODSTEP/25.4D0
C     WE NOW HAVE SURFACE SAGS AND SAGS AT THE FLATS AND DZ1 AND DZ2
C     AND THICKNESS IN LENS UNITS
C     NOW SHIFT VALUES SO AS TO BE REFERENCED TO THE PART CENTER
C     BY SUBTRACTING TH/2 TO FIRST SET OF VALUES AND BY ADDING TH/2
C     TO THE SECOND SET OF VALUES
          SAGODFLT1=SAGODFLT1-(TH/2.0D0)
          SAGODFLT2=SAGODFLT2+(TH/2.0D0)
          SAGOD1=SAGOD1-(TH/2.0D0)
          SAGOD2=SAGOD2+(TH/2.0D0)
          DO K=1,180
              SAGVAL(1,K)=SAGVAL(1,K)-(TH/2.0D0)
              SAGVAL(2,K)=SAGVAL(2,K)+(TH/2.0D0)
          END DO
C     DZ1 AND DZ2 DON'T NEED TO BE SHIFTED AS THEY ARE DELTA VALUES
C
C     NOW APPLY THE SCALING FACTOR, SCALE_FACTOR
          SAGODFLT1=SAGODFLT1*SCALE_FACTOR
          SAGODFLT2=SAGODFLT2*SCALE_FACTOR
          DZ1=DZ1*SCALE_FACTOR
          SAGOD1=SAGOD1*SCALE_FACTOR
          DZ2=DZ2*SCALE_FACTOR
          SAGOD2=SAGOD2*SCALE_FACTOR
          T1=-(TH/2.0D0)*SCALE_FACTOR
          T2=(TH/2.0D0)*SCALE_FACTOR
          ODSTEP=ODSTEP*SCALE_FACTOR
          OD=OD*SCALE_FACTOR
          ODFLAT1=ODFLAT1*SCALE_FACTOR
          ODFLAT2=ODFLAT2*SCALE_FACTOR

          DO I=1,2
              DO K=1,180
                  SAGVAL(I,K)=SAGVAL(I,K)*SCALE_FACTOR
              END DO
          END DO
C     NOW PLOT THE PICTURE OF THE LENS
C     LENS CENTER IS AT X=7150, Y=4375
          CNX=7150
          CNY=4375
          COLPAS=COLFRM
          CALL MY_COLTYP(COLPAS)
C
C     GO TO FIRST VERTEX WITH PEN UP
          YV=CNY
          XV=(1000.0D0*SAGVAL(1,1))+DBLE(CNX)
          CALL MY_PLOT(INT(XV),INT(YV),0,0,-500,10500,-500,7500)

          DO K=2,180
              YV=(1000.0D0*((DBLE(K)*ODSTEP)))+DBLE(CNY)
              XV=(1000.0D0*SAGVAL(1,K))+DBLE(CNX)
              IF(K.EQ.180) THEN
                  YV=(1000.0D0*OD)+DBLE(CNY)
                  XV=(1000.0D0*SAGODFLT1)+DBLE(CNX)
              END IF
              IF(K.EQ.90) THEN
                  PXX1=XV
                  PYY1=YV
              END IF
              IF(INT(XV).LT.INT((1000.0D0*SAGODFLT1)+CNX).AND.
     1        CV1.LE.0.0D0.OR.INT(YV).GE.(INT(ODFLAT1*1000.0D0)+CNY)
     1        .AND.CV1.LE.0.0D0) THEN
                  XV=(1000.0D0*SAGODFLT1)+DBLE(CNX)
                  YV=(1000.0D0*ODFLAT1)+DBLE(CNY)
                  CALL MY_PLOT(INT(XV),INT(YV),1,0,-500,10500,-500,7500)

                  XV=(1000.0D0*(SAGODFLT1+DZ1))+DBLE(CNX)
                  YV=(1000.0D0*OD)+DBLE(CNY)
                  CALL MY_PLOT(INT(XV),INT(YV),1,0,-500,10500,-500,7500)

                  GO TO 101
              ELSE
                  CALL MY_PLOT(INT(XV),INT(YV),1,0,-500,10500,-500,7500)

              END IF
          END DO
101       CONTINUE
          PX1=XV
          PY1=YV
C
C     GO TO FIRST VERTEX WITH PEN UP
          YV=CNY
          XV=(1000.0D0*SAGVAL(1,1))+DBLE(CNX)
          CALL MY_PLOT(INT(XV),INT(YV),0,0,-500,10500,-500,7500)

          DO K=2,180
              YV=(-1000.0D0*((DBLE(K)*ODSTEP)))+DBLE(CNY)
              XV=(1000.0D0*SAGVAL(1,K))+DBLE(CNX)
              IF(K.EQ.180) THEN
                  YV=(-1000.0D0*ODFLAT1)+DBLE(CNY)
                  XV=(1000.0D0*SAGODFLT1)+DBLE(CNX)
              END IF
              IF(K.EQ.90) THEN
                  NXX1=XV
                  NYY1=YV
              END IF
              IF(INT(XV).LT.INT((1000.0D0*SAGODFLT1)+CNX).AND.
     1        CV1.LE.0.0D0.OR.INT(YV).LE.(-INT(ODFLAT2*1000.0D0)+CNY)
     1        .AND.CV1.LE.0.0D0) THEN
                  XV=(1000.0D0*SAGODFLT1)+DBLE(CNX)
                  YV=(-1000.0D0*ODFLAT1)+DBLE(CNY)
                  CALL MY_PLOT(INT(XV),INT(YV),1,0,-500,10500,-500,7500)

                  XV=(1000.0D0*(SAGODFLT1+DZ1))+DBLE(CNX)
                  YV=(-1000.0D0*OD)+DBLE(CNY)
                  CALL MY_PLOT(INT(XV),INT(YV),1,0,-500,10500,-500,7500)

                  GO TO 201
              ELSE
                  CALL MY_PLOT(INT(XV),INT(YV),1,0,-500,10500,-500,7500)

              END IF
          END DO
201       CONTINUE
          NX1=XV
          NY1=YV
C
C     GO TO SECOND VERTEX WITH PEN UP
          YV=CNY
          XV=(1000.0D0*SAGVAL(2,1))+DBLE(CNX)
          CALL MY_PLOT(INT(XV),INT(YV),0,0,-500,10500,-500,7500)

          DO K=2,180
              YV=(1000.0D0*((DBLE(K)*ODSTEP)))+DBLE(CNY)
              XV=(1000.0D0*SAGVAL(2,K))+DBLE(CNX)
              IF(K.EQ.180) THEN
                  YV=(1000.0D0*ODFLAT2)+DBLE(CNY)
                  XV=(1000.0D0*SAGODFLT2)+DBLE(CNX)
              END IF
              IF(K.EQ.90) THEN
                  PXX2=XV
                  PYY2=YV
              END IF
              IF(INT(XV).GT.INT((1000.0D0*SAGODFLT2)+CNX).AND.
     1        CV2.GE.0.0D0.OR.INT(YV).GE.(INT(ODFLAT2*1000.0D0)+CNY)
     1        .AND.CV2.GE.0.0D0) THEN
                  XV=(1000.0D0*SAGODFLT2)+DBLE(CNX)
                  YV=(1000.0D0*ODFLAT2)+DBLE(CNY)
                  CALL MY_PLOT(INT(XV),INT(YV),1,0,-500,10500,-500,7500)

                  XV=(1000.0D0*(SAGODFLT2+DZ2))+DBLE(CNX)
                  YV=(1000.0D0*OD)+DBLE(CNY)
                  CALL MY_PLOT(INT(XV),INT(YV),1,0,-500,10500,-500,7500)

                  GO TO 301
              ELSE
                  CALL MY_PLOT(INT(XV),INT(YV),1,0,-500,10500,-500,7500)

              END IF
          END DO
301       CONTINUE
          PX2=XV
          PY2=YV
C
C     GO TO SECOND VERTEX WITH PEN UP
          YV=CNY
          XV=(1000.0D0*SAGVAL(2,1))+DBLE(CNX)
          CALL MY_PLOT(INT(XV),INT(YV),0,0,-500,10500,-500,7500)

          DO K=2,180
              YV=(-1000.0D0*((DBLE(K)*ODSTEP)))+DBLE(CNY)
              XV=(1000.0D0*SAGVAL(2,K))+DBLE(CNX)
              IF(K.EQ.180) THEN
                  YV=(-1000.0D0*ODFLAT2)+DBLE(CNY)
                  XV=(1000.0D0*SAGODFLT2)+DBLE(CNX)
              END IF
              IF(K.EQ.90) THEN
                  NXX2=XV
                  NYY2=YV
              END IF
              IF(INT(XV).GT.INT((1000.0D0*SAGODFLT2)+CNX).AND.
     1        CV2.GE.0.0D0.OR.INT(YV).LE.(-INT(ODFLAT2*1000.0D0)+CNY)
     1        .AND.CV2.GE.0.0D0) THEN
                  XV=(1000.0D0*SAGODFLT2)+DBLE(CNX)
                  YV=(-1000.0D0*ODFLAT2)+DBLE(CNY)
                  CALL MY_PLOT(INT(XV),INT(YV),1,0,-500,10500,-500,7500)

                  XV=(1000.0D0*(SAGODFLT2+DZ2))+DBLE(CNX)
                  YV=(-1000.0D0*OD)+DBLE(CNY)
                  CALL MY_PLOT(INT(XV),INT(YV),1,0,-500,10500,-500,7500)

                  GO TO 401
              ELSE
                  CALL MY_PLOT(INT(XV),INT(YV),1,0,-500,10500,-500,7500)

              END IF
          END DO
401       CONTINUE
          NX2=XV
          NY2=YV
C
C     DRAW LENS EDGES
          CALL MY_PLOT(INT(PX1),INT(PY1),0,0,-500,10500,-500,7500)
          CALL MY_PLOT(INT(PX2),INT(PY2),1,0,-500,10500,-500,7500)
          CALL MY_PLOT(INT(NX1),INT(NY1),0,0,-500,10500,-500,7500)
          CALL MY_PLOT(INT(NX2),INT(NY2),1,0,-500,10500,-500,7500)
C
          CALL MY_SETCHARASPECT(1.0,1.0)
C     R1 AND "P" NOTES
          CALL MY_PLOT(INT(PXX1),INT(PYY1),0,0,-500,10500,-500,7500)
          CALL MY_PLOT(INT(PXX1)-100,INT(PYY1)+100,
     1    1,0,-500,10500,-500,7500)
          CALL MY_PLOT(INT(PXX1)-500,INT(PYY1)+100,
     1    1,0,-500,10500,-500,7500)
          CALL MY_JUSTSTRING(INT(PXX1)-650
     1    ,INT(PYY1)+50,'"P"',1,1,2)
C
          CALL MY_PLOT(INT(NXX1),INT(NYY1),0,0,-500,10500,-500,7500)
          CALL MY_PLOT(INT(NXX1)-100,INT(NYY1)-100,
     1    1,0,-500,10500,-500,7500)
          CALL MY_PLOT(INT(NXX1)-500,INT(NYY1)-100,
     1    1,0,-500,10500,-500,7500)
          CALL MY_JUSTSTRING(INT(NXX1)-600
     1    ,INT(NYY1)-150,' R1 ',1,1,2)
          CALL MY_JUSTSTRING(INT(NXX1)-600
     1    ,INT(NYY1)-300,'(SURFACE 1)',1,1,2)
C
C     R2 AND "P" NOTES
          CALL MY_PLOT(INT(PXX2),INT(PYY2),0,0,-500,10500,-500,7500)
          CALL MY_PLOT(INT(PXX2)+100,INT(PYY2)+100,
     1    1,0,-500,10500,-500,7500)
          CALL MY_PLOT(INT(PXX2)+500,INT(PYY2)+100,
     1    1,0,-500,10500,-500,7500)
          CALL MY_JUSTSTRING(INT(PXX2)+650
     1    ,INT(PYY2)+50,'"P"',1,1,2)

C
          CALL MY_PLOT(INT(NXX2),INT(NYY2),0,0,-500,10500,-500,7500)
          CALL MY_PLOT(INT(NXX2)+100,INT(NYY2)-100,
     1    1,0,-500,10500,-500,7500)
          CALL MY_PLOT(INT(NXX2)+500,INT(NYY2)-100,
     1    1,0,-500,10500,-500,7500)
          CALL MY_JUSTSTRING(INT(NXX2)+600
     1    ,INT(NYY2)-150,' R2 ',1,1,2,3)
          CALL MY_JUSTSTRING(INT(NXX2)+600
     1    ,INT(NYY2)-300,'(SURFACE 2)',1,1,2)

          DEALLOCATE (SAGVAL,STAT=ALLOERR)
          RETURN
      END


      SUBROUTINE PART_DRAW
          IMPLICIT NONE
C
          INTEGER N,I,GLSCD1,GLSCD2
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          INCLUDE 'datpts.inc'
C
          IF(WC.EQ.'PARTDRAW') THEN
              IF(SQ.EQ.0) THEN
                  SINGLET=.TRUE.
                  DOUBLET=.FALSE.
                  NOTEPAGE=.FALSE.
              END IF
              IF(WQ.EQ.'SINGLET') THEN
                  SINGLET=.TRUE.
                  DOUBLET=.FALSE.
                  NOTEPAGE=.FALSE.
              END IF
              IF(WQ.EQ.'DOUBLET') THEN
                  SINGLET=.FALSE.
                  DOUBLET=.TRUE.
                  NOTEPAGE=.FALSE.
              END IF
              IF(WQ.EQ.'NOTEPAGE') THEN
                  SINGLET=.FALSE.
                  DOUBLET=.FALSE.
                  NOTEPAGE=.TRUE.
              END IF
              IF(SQ.EQ.1.AND.WQ.NE.'SINGLET'.AND.WQ.NE.'DOUBLET'.AND.
     1        WQ.NE.'NOTEPAGE') THEN
                  OUTLYNE=
     1            '"PARTDRAW" ONLY TAKES "SINGLET", "DOUBLET" OR "NOTEPAGE" AS'
                  CALL SHOWIT(1)
                  OUTLYNE=
     1            'QUALIFIER INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE=
     1            'NO ACTION TAKEN'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF

          N=8
          DO I=8,1,-1
              IF(WC(I:I).NE.' ') THEN
                  N=I
                  GO TO 10
              END IF
          END DO
10        CONTINUE
C
          IF(STI.NE.1) THEN
C
C     QUALIFIER CHECK
              IF(SQ.EQ.1) THEN
                  OUTLYNE=
     1            '"'//WC(1:N)//'" TAKES NO QUALIFIER INPUT'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C
C     NUMERIC WORD #3,4 OR 5 CHECK
              IF(S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
                  OUTLYNE=
     1            '"'//WC(1:N)//'" TAKES NO NUMERIC WORD #3, #4 OR #5 INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE=
     1            'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C
C     NUMERIC WORD #2 CHECK
              IF(WC.EQ.'PARTDRAW'.OR.WC.EQ.'DIATOL'.OR.WC.EQ.'THITOL'
     1        .OR.WC.EQ.'WAVEL'.OR.
     2        WC.EQ.'BRKEDG'.OR.WC.EQ.'CENTIR') THEN
                  IF(S2.EQ.1) THEN
                      OUTLYNE=
     1                '"'//WC(1:N)//'" TAKES NO NUMERIC WORD #2 INPUT'
                      CALL SHOWIT(1)
                      OUTLYNE=
     1                'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
C
C     STRING INPUT CHECK
              IF(WC.EQ.'SURFQUAL'.OR.WC.EQ.'SURFCOAT'.OR.WC.EQ.'TITLE'
     1        .OR.WC.EQ.'DWGNO'.OR.WC.EQ.'CONAME'.OR.WC.EQ.'SURFMATL') THEN
              ELSE
                  IF(SST.EQ.1) THEN
                      OUTLYNE=
     1                '"'//WC(1:N)//'" TAKES NO STRING INPUT'
                      CALL SHOWIT(1)
                      OUTLYNE=
     1                'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
          ELSE
C     STI=1
          END IF
C
C     PARTDRAW
          IF(WC.EQ.'PARTDRAW') THEN
              IF(STI.EQ.1) THEN
                  OUTLYNE=
     1            '"PARTDRAW" INITIALIZES THE LENS DRAWING FEATURE'
                  CALL SHOWIT(1)
                  RETURN
              END IF
              IF(S1.EQ.0) THEN
                  OUTLYNE=
     1            '"PARTDRAW" REQUIRES EXPLICIT NUMERIC WORD #1 INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE=
     1            'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(W1.GE.SYSTEM1(20).OR.W1.LE.0.0D0) THEN
                  OUTLYNE=
     1            'INVALID SURFACE NUMBER ISSUED WITH "PARTDRAW"'
                  CALL SHOWIT(1)
                  OUTLYNE=
     1            'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              DRAWSURF=INT(W1)
              CALL DRAW_INITIALIZE
              RETURN
          END IF
C
C     DIAMETER TOLERANCE
          IF(WC.EQ.'DIATOL  ') THEN
              IF(.NOT.PARTISDRAWING) THEN
                  OUTLYNE='"PARTDRAW" MUST BE ISSUED FIRST TO INITIALIZE'
                  CALL SHOWIT(1)
                  OUTLYNE='PART DRAWING PARAMETERS'
                  CALL SHOWIT(1)
                  OUTLYNE=
     1            'NO ACTION TAKEN'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(DF1.EQ.0) DIATOL=W1
              RETURN
          END IF
C
C     RADIUS TOLERANCE
          IF(WC.EQ.'RADTOL  ') THEN
              IF(.NOT.PARTISDRAWING) THEN
                  OUTLYNE='"PARTDRAW" MUST BE ISSUED FIRST TO INITIALIZE'
                  CALL SHOWIT(1)
                  OUTLYNE='PART DRAWING PARAMETERS'
                  CALL SHOWIT(1)
                  OUTLYNE=
     1            'NO ACTION TAKEN'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(DF1.EQ.0) RADTOL1=W1
              IF(DF1.EQ.0) FRINTOL1=.FALSE.
              IF(DF2.EQ.0) RADTOL2=W1
              IF(DF2.EQ.0) FRINTOL2=.FALSE.
              RETURN
          END IF
C
C     RADIUS TOLERANCE (FRINGES)
          IF(WC.EQ.'RADTLF  ') THEN
              IF(.NOT.PARTISDRAWING) THEN
                  OUTLYNE='"PARTDRAW" MUST BE ISSUED FIRST TO INITIALIZE'
                  CALL SHOWIT(1)
                  OUTLYNE='PART DRAWING PARAMETERS'
                  CALL SHOWIT(1)
                  OUTLYNE=
     1            'NO ACTION TAKEN'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(DF1.EQ.0) RADTOL1=W1
              IF(DF1.EQ.0) FRINTOL1=.TRUE.
              IF(DF2.EQ.0) RADTOL2=W1
              IF(DF2.EQ.0) FRINTOL2=.TRUE.
              RETURN
          END IF
C
C     IRREGULARITY IN FRINGES
          IF(WC.EQ.'FRNG    ') THEN
              IF(.NOT.PARTISDRAWING) THEN
                  OUTLYNE='"PARTDRAW" MUST BE ISSUED FIRST TO INITIALIZE'
                  CALL SHOWIT(1)
                  OUTLYNE='PART DRAWING PARAMETERS'
                  CALL SHOWIT(1)
                  OUTLYNE=
     1            'NO ACTION TAKEN'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(DF1.EQ.0) FRNG1=W1
              IF(DF2.EQ.0) FRNG2=W2
              IF(DF2.EQ.1) FRNG2=W1
              RETURN
          END IF
C
C     THICKNESS TOLERANCE
          IF(WC.EQ.'THITOL  ') THEN
              IF(.NOT.PARTISDRAWING) THEN
                  OUTLYNE='"PARTDRAW" MUST BE ISSUED FIRST TO INITIALIZE'
                  CALL SHOWIT(1)
                  OUTLYNE='PART DRAWING PARAMETERS'
                  CALL SHOWIT(1)
                  OUTLYNE=
     1            'NO ACTION TAKEN'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(DF1.EQ.0) THITOL=W1
              RETURN
          END IF
C
C     CLEAR APERUTRE FOR NOTES
          IF(WC.EQ.'CLERAP  ') THEN
              IF(.NOT.PARTISDRAWING) THEN
                  OUTLYNE='"PARTDRAW" MUST BE ISSUED FIRST TO INITIALIZE'
                  CALL SHOWIT(1)
                  OUTLYNE='PART DRAWING PARAMETERS'
                  CALL SHOWIT(1)
                  OUTLYNE=
     1            'NO ACTION TAKEN'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              CLP1SET=.FALSE.
              CLP2SET=.FALSE.
              IF(DF1.EQ.0) CLP1=W1
              IF(DF1.EQ.0) CLP1SET=.TRUE.
              IF(DF2.EQ.0) CLP2=W2
              IF(DF1.EQ.0.AND.DF2.EQ.1) CLP2=W1
              IF(DF1.EQ.0.AND.DF2.EQ.1) DF2=0
              IF(DF2.EQ.0) CLP2SET=.TRUE.
              RETURN
          END IF
C
C     SURFACE QUALITY
          IF(WC.EQ.'SURFQUAL') THEN
              IF(.NOT.PARTISDRAWING) THEN
                  OUTLYNE='"PARTDRAW" MUST BE ISSUED FIRST TO INITIALIZE'
                  CALL SHOWIT(1)
                  OUTLYNE='PART DRAWING PARAMETERS'
                  CALL SHOWIT(1)
                  OUTLYNE=
     1            'NO ACTION TAKEN'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(SST.EQ.1) SURQUAL=WS(1:20)
              RETURN
          END IF
C
C     SURFACE COATING
          IF(WC.EQ.'SURFCOAT') THEN
              IF(.NOT.PARTISDRAWING) THEN
                  OUTLYNE='"PARTDRAW" MUST BE ISSUED FIRST TO INITIALIZE'
                  CALL SHOWIT(1)
                  OUTLYNE='PART DRAWING PARAMETERS'
                  CALL SHOWIT(1)
                  OUTLYNE=
     1            'NO ACTION TAKEN'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(SST.EQ.1) SURCOAT=WS(1:20)
              RETURN
          END IF
C
C     SURFACE MATERIAL
          IF(WC.EQ.'SURFMATL') THEN
              IF(.NOT.PARTISDRAWING) THEN
                  OUTLYNE='"PARTDRAW" MUST BE ISSUED FIRST TO INITIALIZE'
                  CALL SHOWIT(1)
                  OUTLYNE='PART DRAWING PARAMETERS'
                  CALL SHOWIT(1)
                  OUTLYNE=
     1            'NO ACTION TAKEN'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(SST.EQ.1) SURMATL=WS(1:13)
              RETURN
          END IF
C
C     TITLE
          IF(WC.EQ.'TITLE') THEN
              IF(.NOT.PARTISDRAWING) THEN
                  OUTLYNE='"PARTDRAW" MUST BE ISSUED FIRST TO INITIALIZE'
                  CALL SHOWIT(1)
                  OUTLYNE='PART DRAWING PARAMETERS'
                  CALL SHOWIT(1)
                  OUTLYNE=
     1            'NO ACTION TAKEN'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(SST.EQ.1) DTITLE=WS(1:32)
              RETURN
          END IF
C
C     DRAWING NUMBER
          IF(WC.EQ.'DWGNO') THEN
              IF(.NOT.PARTISDRAWING) THEN
                  OUTLYNE='"PARTDRAW" MUST BE ISSUED FIRST TO INITIALIZE'
                  CALL SHOWIT(1)
                  OUTLYNE='PART DRAWING PARAMETERS'
                  CALL SHOWIT(1)
                  OUTLYNE=
     1            'NO ACTION TAKEN'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(SST.EQ.1) DWGNO=WS(1:12)
              RETURN
          END IF
C
C     COMPANY NAME
          IF(WC.EQ.'CONAME') THEN
              IF(.NOT.PARTISDRAWING) THEN
                  OUTLYNE='"PARTDRAW" MUST BE ISSUED FIRST TO INITIALIZE'
                  CALL SHOWIT(1)
                  OUTLYNE='PART DRAWING PARAMETERS'
                  CALL SHOWIT(1)
                  OUTLYNE=
     1            'NO ACTION TAKEN'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(SST.EQ.1) CONAME=WS(1:32)
              RETURN
          END IF
C
C     FRINGE DIAMETER
          IF(WC.EQ.'FNGDIA  ') THEN
              IF(.NOT.PARTISDRAWING) THEN
                  OUTLYNE='"PARTDRAW" MUST BE ISSUED FIRST TO INITIALIZE'
                  CALL SHOWIT(1)
                  OUTLYNE='PART DRAWING PARAMETERS'
                  CALL SHOWIT(1)
                  OUTLYNE=
     1            'NO ACTION TAKEN'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(DF1.EQ.0) FNGDIA1=W1
              IF(DF2.EQ.0) FNGDIA2=W2
              IF(DF2.EQ.1) FNGDIA2=W1
              RETURN
          END IF
C
C     CENTIR
          IF(WC.EQ.'CENTIR  ') THEN
              IF(.NOT.PARTISDRAWING) THEN
                  OUTLYNE='"PARTDRAW" MUST BE ISSUED FIRST TO INITIALIZE'
                  CALL SHOWIT(1)
                  OUTLYNE='PART DRAWING PARAMETERS'
                  CALL SHOWIT(1)
                  OUTLYNE=
     1            'NO ACTION TAKEN'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(DF1.EQ.0) CENTIR=W1
              RETURN
          END IF
C
C     BREAK EDGE FACE WIDTH
          IF(WC.EQ.'BRKEDG  ') THEN
              IF(.NOT.PARTISDRAWING) THEN
                  OUTLYNE='"PARTDRAW" MUST BE ISSUED FIRST TO INITIALIZE'
                  CALL SHOWIT(1)
                  OUTLYNE='PART DRAWING PARAMETERS'
                  CALL SHOWIT(1)
                  OUTLYNE=
     1            'NO ACTION TAKEN'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(DF1.EQ.0) BRKEDG=W1
              RETURN
          END IF
C
C     SAG TOLERANCES
          IF(WC.EQ.'SAGTOL  ') THEN
              IF(.NOT.PARTISDRAWING) THEN
                  OUTLYNE='"PARTDRAW" MUST BE ISSUED FIRST TO INITIALIZE'
                  CALL SHOWIT(1)
                  OUTLYNE='PART DRAWING PARAMETERS'
                  CALL SHOWIT(1)
                  OUTLYNE=
     1            'NO ACTION TAKEN'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(DF1.EQ.0) SAGTL1=W1
              IF(DF2.EQ.0) SAGTL2=W2
              IF(DF2.EQ.1) SAGTL2=W1
              RETURN
          END IF
C
C     TOL OF FLAT PERPENDICULATITY TO AXIS
          IF(WC.EQ.'PRPNTL  ') THEN
              IF(.NOT.PARTISDRAWING) THEN
                  OUTLYNE='"PARTDRAW" MUST BE ISSUED FIRST TO INITIALIZE'
                  CALL SHOWIT(1)
                  OUTLYNE='PART DRAWING PARAMETERS'
                  CALL SHOWIT(1)
                  OUTLYNE=
     1            'NO ACTION TAKEN'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(DF1.EQ.0) PRPNTL1=W1
              IF(DF2.EQ.0) PRPNTL2=W2
              IF(DF2.EQ.1) PRPNTL2=W1
              RETURN
          END IF
C
C     FRINGE WAVELENGTH
          IF(WC.EQ.'WAVEL   ') THEN
              IF(.NOT.PARTISDRAWING) THEN
                  OUTLYNE='"PARTDRAW" MUST BE ISSUED FIRST TO INITIALIZE'
                  CALL SHOWIT(1)
                  OUTLYNE='PART DRAWING PARAMETERS'
                  CALL SHOWIT(1)
                  OUTLYNE=
     1            'NO ACTION TAKEN'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(DF1.EQ.0) WAVEL1=W1
              RETURN
          END IF
C
C     GLASS CODE (3 DIGITS EACH)
          IF(WC.EQ.'GLSCD   ') THEN
              IF(.NOT.PARTISDRAWING) THEN
                  OUTLYNE='"PARTDRAW" MUST BE ISSUED FIRST TO INITIALIZE'
                  CALL SHOWIT(1)
                  OUTLYNE='PART DRAWING PARAMETERS'
                  CALL SHOWIT(1)
                  OUTLYNE=
     1            'NO ACTION TAKEN'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(DF1.EQ.0) GLSCD1=INT(W1)
              IF(DF2.EQ.0) GLSCD2=INT(W2)
              RETURN
          END IF
C
          IF(WC.EQ.'PARTGO  ') THEN
              IF(.NOT.PARTISDRAWING) THEN
                  OUTLYNE='"PARTDRAW" MUST BE ISSUED FIRST TO INITIALIZE'
                  CALL SHOWIT(1)
                  OUTLYNE='PART DRAWING PARAMETERS'
                  CALL SHOWIT(1)
                  OUTLYNE=
     1            'NO ACTION TAKEN'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              CALL DODRAWING
              RETURN
          END IF
          IF(WC.EQ.'PARTQUIT') THEN
              IF(.NOT.PARTISDRAWING) THEN
                  OUTLYNE='"PARTDRAW" WAS NEVER ISSUED,'
                  CALL SHOWIT(1)
                  OUTLYNE='NO PART DRAWING EXISTS TO QUIT'
                  CALL SHOWIT(1)
                  OUTLYNE=
     1            'NO ACTION TAKEN'
                  CALL SHOWIT(1)
                  PARTISDRAWING=.FALSE.
                  CALL MACFAL
                  RETURN
              END IF
              RETURN
          END IF
      END
C SUB GREYSPOT.FOR
      SUBROUTINE GREYSPOT
C
          IMPLICIT NONE
C
C       THIS ROUTINE DOES THE GREYSPOT COMMAND
C
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          INTEGER IW1
          SAVE IW1
C
          IF(STI.EQ.1) THEN
              WRITE(OUTLYNE,800) GRSPT
              CALL SHOWIT(1)
 800          FORMAT('"GREYSPOT" IS CURRENTLY SET = ',I2)
              RETURN
          ELSE
          END IF
C
C       CHECK SYNTAX
          IF(SQ.EQ.1.OR.SST.EQ.1) THEN
              OUTLYNE=
     1        '"GREYSPOT" TAKES NO QUALIFIER OR STRING INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
              OUTLYNE=
     1        '"GREYSPOT" ONLY TAKES NUMERIC WORDS #1 INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(DF1.EQ.1) THEN
              OUTLYNE=
     1        '"GREYSPOT" REQUIRES EXPLICIT NUMERIC WORD #1 INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(W1.LT.1.0D0.OR.W1.GT.4) THEN
              OUTLYNE=
     1        'NUMERIC WORD #1, CAN ONLY BE 1, 2, 3 OR 4'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IW1=INT(W1)
C
          IF(W1.EQ.1.0D0) THEN
              SAVE_KDP(1)=SAVEINPT(1)
C     RESET I/O TO SAFE VALUES
              INPUT='OPSPOT RING'
              CALL PROCES
              INPUT='OPRINGS 1'
              CALL PROCES
              INPUT='OPRING 1 .707 8'
              CALL PROCES
              REST_KDP(1)=RESTINPT(1)
          END IF
          IF(W1.EQ.2.0D0) THEN
              SAVE_KDP(1)=SAVEINPT(1)
C     RESET I/O TO SAFE VALUES
              INPUT='OPSPOT RING'
              CALL PROCES
              INPUT='OPRINGS 2'
              CALL PROCES
              INPUT='OPRING 1 .4 8'
              CALL PROCES
              INPUT='OPRING 2 .866 8'
              CALL PROCES
              REST_KDP(1)=RESTINPT(1)
          END IF
          IF(W1.EQ.3.0D0) THEN
              SAVE_KDP(1)=SAVEINPT(1)
C     RESET I/O TO SAFE VALUES
              INPUT='OPSPOT RING'
              CALL PROCES
              INPUT='OPRINGS 3'
              CALL PROCES
              INPUT='OPRING 1 .4 8'
              CALL PROCES
              INPUT='OPRING 2 .707 8'
              CALL PROCES
              INPUT='OPRING 3 .866 8'
              CALL PROCES
              REST_KDP(1)=RESTINPT(1)
          END IF
          IF(W1.EQ.4.0D0) THEN
              SAVE_KDP(1)=SAVEINPT(1)
C     RESET I/O TO SAFE VALUES
              INPUT='OPSPOT RING'
              CALL PROCES
              INPUT='OPRINGS 4'
              CALL PROCES
              INPUT='OPRING 1 .4 8'
              CALL PROCES
              INPUT='OPRING 2 .707 8'
              CALL PROCES
              INPUT='OPRING 3 .866 8'
              CALL PROCES
              INPUT='OPRING 4 1.0 8'
              CALL PROCES
              REST_KDP(1)=RESTINPT(1)
          END IF
          GRSPT=IW1
          RETURN
      END
C SUB VIEOFF.FOR
      SUBROUTINE VIEOFF
C
C       THIS IS SUBROUTINE VIEOFF. THIS IS THE SUBROUTINE WHICH
C       HANDLES CMD LEVEL COMMAND "VIEOFF"
C
          IMPLICIT NONE
C
          REAL*8 VIEROT
C
          INTEGER VIEXOF,VIEYOF
C
          COMMON/OFFVIE/VIEXOF,VIEYOF,VIEROT
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
          IF(STI.EQ.1.OR.SQ.EQ.0.AND.SN.EQ.0) THEN
              OUTLYNE=
     1        '"VIEOFF" SETS UP OFFSETS AND A ROTATION FOR "VIE"'
              CALL SHOWIT(1)
              OUTLYNE='CURRENT FIELD SETTINGS ARE:'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'X-OFFSET = ',VIEXOF
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'Y-OFFSET = ',VIEYOF
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'ROTATION = ',VIEROT,' DEGREES'
              CALL SHOWIT(1)
              RETURN
          END IF
          IF(SST.EQ.1.OR.SQ.EQ.1) THEN
              OUTLYNE='"VIEOFF" TAKES NO STRING OR QUALIFIER INPUT'
              CALL SHOWIT(1)
              OUTLYNE= 'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(S4.EQ.1.OR.S5.EQ.1) THEN
              OUTLYNE='"VIEOFF" TAKES NO NUMERIC WORD #4 OR #5 INPUT'
              CALL SHOWIT(1)
              OUTLYNE= 'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          VIEXOF=INT(W1)
          VIEYOF=INT(W2)
          VIEROT=INT(W3)
          RETURN
C       ALL DONE
      END
C SUB VIEVIG.FOR
      SUBROUTINE VIEVIG
C
C       THIS IS SUBROUTINE VIEVIG. THIS IS THE SUBROUTINE WHICH
C       HANDLES CMD LEVEL COMMAND "VIEVIG"
C
          IMPLICIT NONE
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
          IF(STI.EQ.1.OR.SQ.EQ.0.AND.SN.EQ.0.AND.SST.EQ.0) THEN
              OUTLYNE=
     1        '"VIEVIG" SETS VIGNETTING IN "VIE" PLOTS TO "ON" OR "OFF"'
              CALL SHOWIT(1)
              IF(VIGOFF) OUTLYNE='CURRENT SETTING IS "OFF"'
              IF(.NOT.VIGOFF) OUTLYNE='CURRENT SETTING IS "ON"'
              CALL SHOWIT(1)
              RETURN
          END IF
          IF(SST.EQ.1.OR.SN.EQ.1) THEN
              OUTLYNE='"VIEVIG" TAKES NO STRING OR NUMERIC INPUT'
              CALL SHOWIT(1)
              OUTLYNE= 'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(WQ.NE.'ON'.AND.WQ.NE.'OFF'.AND.SQ.EQ.1) THEN
              OUTLYNE='"VIEVIG" INPUT MAY ONLY BE "ON" OR "OFF"'
              CALL SHOWIT(1)
              OUTLYNE= 'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(WQ.EQ.'OFF') VIGOFF=.TRUE.
          IF(WQ.EQ.'ON') VIGOFF=.FALSE.
          RETURN
C       ALL DONE
      END
C SUB VIEOVER.FOR
      SUBROUTINE VIEOVER
C
C       THIS IS SUBROUTINE VIEOVER. THIS IS THE SUBROUTINE WHICH
C       HANDLES CMD LEVEL COMMAND "VIEOVER"
C
          IMPLICIT NONE
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
          IF(STI.EQ.1) THEN
              OUTLYNE=
     1        '"VIEOVER" SETS VIE OVERLAY "ON" FOR THE NEXT "VIE"'
              CALL SHOWIT(1)
              RETURN
          END IF
          IF(SST.EQ.1.OR.SN.EQ.1.OR.SQ.EQ.1) THEN
              OUTLYNE='"VIEOVER" TAKES NO INPUT'
              CALL SHOWIT(1)
              OUTLYNE= 'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          VIEOVERLAY=.TRUE.
          RETURN
      END
C SUB VIESYM.FOR
      SUBROUTINE VIESYM
C
C       THIS IS SUBROUTINE VIESYM. THIS IS THE SUBROUTINE WHICH
C       HANDLES CMD LEVEL COMMAND "VIESYM"
C
          IMPLICIT NONE
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
          IF(STI.EQ.1.OR.SQ.EQ.0.AND.SN.EQ.0.AND.SST.EQ.0) THEN
              OUTLYNE=
     1        '"VIESYM" SETS RAY SYMMETRY IN "VIE" PLOTS TO "ON" OR "OFF"'
              CALL SHOWIT(1)
              IF(.NOT.VSYM) OUTLYNE='CURRENT SETTING IS "OFF"'
              IF(VSYM) OUTLYNE='CURRENT SETTING IS "ON"'
              CALL SHOWIT(1)
              RETURN
          END IF
          IF(SST.EQ.1.OR.SN.EQ.1) THEN
              OUTLYNE='"VIESYM" TAKES NO STRING OR NUMERIC INPUT'
              CALL SHOWIT(1)
              OUTLYNE= 'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(WQ.NE.'ON'.AND.WQ.NE.'OFF'.AND.SQ.EQ.1) THEN
              OUTLYNE='"VIESYM" INPUT MAY ONLY BE "ON" OR "OFF"'
              CALL SHOWIT(1)
              OUTLYNE= 'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(WQ.EQ.'ON') VSYM=.TRUE.
          IF(WQ.EQ.'OFF') VSYM=.FALSE.
          RETURN
C       ALL DONE
      END
C SUB VIGGER.FOR
      SUBROUTINE VIGGER
C
          IMPLICIT NONE
C
C       THIS ROUTINE DOES THE VIG ON OR OFF SETTING VIA THE VIG COMMAND
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
          IF(STI.EQ.1) THEN
              IF(LVIG) WRITE(OUTLYNE,100)
              IF(.NOT.LVIG) WRITE(OUTLYNE,200)
              CALL SHOWIT(0)
 100          FORMAT(
     1        'THE VIGNETTING OPTION IS CURRENTLY "ACTIVE"')
 200          FORMAT(
     1        'THE VIGNETTING OPTION IS CURRENTLY "NOT" ACTIVE')
              RETURN
          END IF
C       CHECK FOR STRING INPUT
          IF(SST.EQ.1.OR.SN.EQ.1) THEN
              OUTLYNE='"VIG" TAKES NO STRING OR NUMERICAL INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C       QUALIFIERS
          IF(SQ.EQ.1.AND.WQ.NE.'ON'.AND.WQ.NE.'OFF') THEN
              OUTLYNE='"ON" AND "OFF" ARE THE ONLY VALID QUALIFIERS'
              CALL SHOWIT(1)
              OUTLYNE='USED WITH "VIG"'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SQ.EQ.0) THEN
              OUTLYNE='"VIG" REQUIRES EXPLICIT QUALIFIER INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(WQ.EQ.'ON') LVIG=.TRUE.
          IF(WQ.EQ.'OFF') LVIG=.FALSE.
          RETURN
      END
      SUBROUTINE TSTPLATE
          IMPLICIT NONE
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          IF(STI.EQ.1.OR.SST.EQ.0.AND.SN.EQ.0.AND.WQ.EQ.'LIST') THEN
              OUTLYNE='THE CURRENTLY INSTALLED TESTPLATE LISTS ARE:'
              CALL SHOWIT(0)
              OUTLYNE=' '
              CALL SHOWIT(0)
              OUTLYNE='"APPLIEDO" FOR APPLIED OPTICS INC.'
              CALL SHOWIT(0)
              OUTLYNE='"BERNOPTS" FOR BERN OPTICS INC.'
              CALL SHOWIT(0)
              OUTLYNE='"BRIGHTEN" FOR BRIGHTEN OPTICS LTD.'
              CALL SHOWIT(0)
              OUTLYNE='"COASTALO" FOR COASTAL OPTICAL SYSTEMS INC.'
              CALL SHOWIT(0)
              OUTLYNE='"COMPUTER" FOR COMPUTER OPTICS INC.'
              CALL SHOWIT(0)
              OUTLYNE='"CONINENT" FOR CONTINENTAL OPTICAL CORP.'
              CALL SHOWIT(0)
              OUTLYNE='"EASTMANK" FOR EASTMAN KODAK COMPANY'
              CALL SHOWIT(0)
              OUTLYNE='"HAROLDJN" FOR HAROLD JOHNSON OPTICAL LABORATORIES INC.'
              CALL SHOWIT(0)
              OUTLYNE='"JANOSTEC" FOR JANOS TECHNOLOGY INC.'
              CALL SHOWIT(0)
              OUTLYNE='"JLWOODOP" FOR J. L. WOOD OPTICAL SYSTEMS'
              CALL SHOWIT(0)
              OUTLYNE='"KREISCHR" FOR KREISCHER OPTICS LTD.'
              CALL SHOWIT(0)
              OUTLYNE='"LIEBMANN" FOR LIEBMANN OPTICAL COMPANY'
              CALL SHOWIT(0)
              OUTLYNE='"LIGHTNIN" FOR LIGHTNING OPTICAL CORPORATION'
              CALL SHOWIT(0)
              OUTLYNE='"MEDIVISN" FOR MEDIVISION OPTICS'
              CALL SHOWIT(0)
              OUTLYNE='"MELLESGR" FOR MELLES GRIOT OPTICAL SYSTEMS'
              CALL SHOWIT(0)
              OUTLYNE='"MODELOPT" FOR MODEL OPTICS INC.'
              CALL SHOWIT(0)
              OUTLYNE='"OPT4RESH" FOR OPTICS FOR RESEARCH'
              CALL SHOWIT(0)
              OUTLYNE='"OPTICOMP" FOR OPTICAL COMPONENTS INC.'
              CALL SHOWIT(0)
              OUTLYNE='"OPTIGFAB" FOR OPTICAL GLASS FABRICATION'
              CALL SHOWIT(0)
              OUTLYNE='"OPTIMAXS" FOR OPTIMAX SYSTEMS INC.'
              CALL SHOWIT(0)
              OUTLYNE='"ROCKYMTN" FOR ROCKY MOUNTAIN INSTRUMENTS'
              CALL SHOWIT(0)
              OUTLYNE='"RODENSTK" FOR RODENSTOCK PROZISIONSOPTIK GMBH'
              CALL SHOWIT(0)
              OUTLYNE='"SILLOPTS" FOR SILL OPTICS GMBH'
              CALL SHOWIT(0)
              OUTLYNE='"SPECIALO" FOR SPECIAL OPTICS'
              CALL SHOWIT(0)
              OUTLYNE='"SPINDLER" FOR SPINDLER & HOYER INC.'
              CALL SHOWIT(0)
              OUTLYNE='"SVGTINLY" FOR SVG-TINSLEY DIVISION'
              CALL SHOWIT(0)
              OUTLYNE='"TOWEROPT" FOR TOWER OPTICAL CORP.'
              CALL SHOWIT(0)
              OUTLYNE='"TROPELCO" FOR TROPEL CORPORATION'
              CALL SHOWIT(0)
              OUTLYNE='"TUCSONOP" FOR TUCSON OPTICAL RESEARCH CORP.'
              CALL SHOWIT(0)
              OUTLYNE='"TWOSIXOP" FOR II-VI CORP.'
              CALL SHOWIT(0)
              RETURN
          END IF
          IF(SST.EQ.1.OR.SN.EQ.1) THEN
              OUTLYNE='"TPLATE" TAKES NO STRING OR NUMERIC INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(WQ.NE.'APPLIEDO'.AND.WQ.NE.'BERNOPTS'.AND.WQ.NE.'BRIGHTEN'
     1    .AND.WQ.NE.'COASTALO'.AND.WQ.NE.'COMPUTER'.AND.WQ.NE.'CONINENT'
     2    .AND.WQ.NE.'HAROLDJN'.AND.WQ.NE.'JANOSTEC'.AND.WQ.NE.'JLWOODOP'
     3    .AND.WQ.NE.'EASTMANK'.AND.WQ.NE.'KREISCHR'.AND.WQ.NE.'LIEBMANN'
     4    .AND.WQ.NE.'LIGHTNIN'.AND.WQ.NE.'MEDIVISN'.AND.WQ.NE.'MELLESGR'
     5    .AND.WQ.NE.'MODELOPT'.AND.WQ.NE.'OPTICOMP'.AND.WQ.NE.'OPTIGFAB'
     6    .AND.WQ.NE.'OPT4RESH'.AND.WQ.NE.'OPTIMAXS'.AND.WQ.NE.'ROCKYMTN'
     7    .AND.WQ.NE.'RODENSTK'.AND.WQ.NE.'SILLOPT2'.AND.WQ.NE.'SPECIALO'
     8    .AND.WQ.NE.'SPINDLER'.AND.WQ.NE.'SVGTINLY'.AND.WQ.NE.'TROPELCO'
     9    .AND.WQ.NE.'TUCSONOP'.AND.WQ.NE.'TOWEROPT'.AND.WQ.NE.'LIST')
     1    THEN
              OUTLYNE='"TPLATE" ONLY TAKES:'
              CALL SHOWIT(1)
              OUTLYNE='"LIST" TO SHOW ALL TESTPLATE LISTS INSTALLED'
              CALL SHOWIT(1)
              OUTLYNE='OR'
              CALL SHOWIT(1)
              OUTLYNE='"APPLIEDO" FOR APPLIED OPTICS INC.'
              CALL SHOWIT(1)
              OUTLYNE='"BERNOPTS" FOR BERN OPTICS INC.'
              CALL SHOWIT(1)
              OUTLYNE='"BRIGHTEN" FOR BRIGHTEN OPTICS LTD.'
              CALL SHOWIT(1)
              OUTLYNE='"COASTALO" FOR COASTAL OPTICAL SYSTEMS INC.'
              CALL SHOWIT(1)
              OUTLYNE='"COMPUTER" FOR COMPUTER OPTICS INC.'
              CALL SHOWIT(1)
              OUTLYNE='"CONINENT" FOR CONTINENTAL OPTICAL CORP.'
              CALL SHOWIT(1)
              OUTLYNE='"EASTMANK" FOR EASTMAN KODAK COMPANY'
              CALL SHOWIT(1)
              OUTLYNE='"HAROLDJN" FOR HAROLD JOHNSON OPTICAL LABORATORIES INC.'
              CALL SHOWIT(1)
              OUTLYNE='"JANOSTEC" FOR JANOS TECHNOLOGY INC.'
              CALL SHOWIT(1)
              OUTLYNE='"JLWOODOP" FOR J. L. WOOD OPTICAL SYSTEMS'
              CALL SHOWIT(1)
              OUTLYNE='"KREISCHR" FOR KREISCHER OPTICS LTD.'
              CALL SHOWIT(1)
              OUTLYNE='"LIEBMANN" FOR LIEBMANN OPTICAL COMPANY'
              CALL SHOWIT(1)
              OUTLYNE='"LIGHTNIN" FOR LIGHTNING OPTICAL CORPORATION'
              CALL SHOWIT(1)
              OUTLYNE='"MEDIVISN" FOR MEDIVISION OPTICS'
              CALL SHOWIT(1)
              OUTLYNE='"MELLESGR" FOR MELLES GRIOT OPTICAL SYSTEMS'
              CALL SHOWIT(1)
              OUTLYNE='"MODELOPT" FOR MODEL OPTICS INC.'
              CALL SHOWIT(1)
              OUTLYNE='"OPT4RESH" FOR OPTICS FOR RESEARCH'
              CALL SHOWIT(1)
              OUTLYNE='"OPTICOMP" FOR OPTICAL COMPONENTS INC.'
              CALL SHOWIT(1)
              OUTLYNE='"OPTIGFAB" FOR OPTICAL GLASS FABRICATION'
              CALL SHOWIT(1)
              OUTLYNE='"OPTIMAXS" FOR OPTIMAX SYSTEMS INC.'
              CALL SHOWIT(1)
              OUTLYNE='"ROCKYMTN" FOR ROCKY MOUNTAIN INSTRUMENTS'
              CALL SHOWIT(1)
              OUTLYNE='"RODENSTK" FOR RODENSTOCK PROZISIONSOPTIK GMBH'
              CALL SHOWIT(1)
              OUTLYNE='"SILLOPTS" FOR SILL OPTICS GMBH'
              CALL SHOWIT(1)
              OUTLYNE='"SPECIALO" FOR SPECIAL OPTICS'
              CALL SHOWIT(1)
              OUTLYNE='"SPINDLER" FOR SPINDLER & HOYER INC.'
              CALL SHOWIT(1)
              OUTLYNE='"SVGTINLY" FOR SVG-TINSLEY DIVISION'
              CALL SHOWIT(1)
              OUTLYNE='"TOWEROPT" FOR TOWER OPTICAL CORP.'
              CALL SHOWIT(1)
              OUTLYNE='"TROPELCO" FOR TROPEL CORPORATION'
              CALL SHOWIT(1)
              OUTLYNE='"TUCSONOP" FOR TUCSON OPTICAL RESEARCH CORP.'
              CALL SHOWIT(1)
              OUTLYNE='"TWOSIXOP" FOR II-VI CORP.'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(WQ.EQ.'APPLIEDO') CALL OPENTSTP('TPLATE/APPLIEDO.TPL')
          IF(WQ.EQ.'BERNOPTS') CALL OPENTSTP('TPLATE/BERNOPTS.TPL')
          IF(WQ.EQ.'BRIGHTEN') CALL OPENTSTP('TPLATE/BRIGHTEN.TPL')
          IF(WQ.EQ.'COASTALO') CALL OPENTSTP('TPLATE/COASTALO.TPL')
          IF(WQ.EQ.'COMPUTER') CALL OPENTSTP('TPLATE/COMPUTER.TPL')
          IF(WQ.EQ.'CONINENT') CALL OPENTSTP('TPLATE/COMINENT.TPL')
          IF(WQ.EQ.'HAROLDJN') CALL OPENTSTP('TPLATE/HAROLDJN.TPL')
          IF(WQ.EQ.'JANOSTEC') CALL OPENTSTP('TPLATE/JANOSTEC.TPL')
          IF(WQ.EQ.'JLWOODOP') CALL OPENTSTP('TPLATE/JLWOODOP.TPL')
          IF(WQ.EQ.'EASTMANK') CALL OPENTSTP('TPLATE/EASTMANK.TPL')
          IF(WQ.EQ.'KREISCHR') CALL OPENTSTP('TPLATE/KREISCHR.TPL')
          IF(WQ.EQ.'LIEBMANN') CALL OPENTSTP('TPLATE/LIEBMANN.TPL')
          IF(WQ.EQ.'LIGHTNIN') CALL OPENTSTP('TPLATE/LIGHTNIN.TPL')
          IF(WQ.EQ.'MEDIVISN') CALL OPENTSTP('TPLATE/MEDIVISN.TPL')
          IF(WQ.EQ.'MELLESGR') CALL OPENTSTP('TPLATE/MELLESGR.TPL')
          IF(WQ.EQ.'MODELOPT') CALL OPENTSTP('TPLATE/MODELOPT.TPL')
          IF(WQ.EQ.'OPTICOMP') CALL OPENTSTP('TPLATE/OPTICOMP.TPL')
          IF(WQ.EQ.'OPTIGFAB') CALL OPENTSTP('TPLATE/OPTIGFAB.TPL')
          IF(WQ.EQ.'OPT4RESH') CALL OPENTSTP('TPLATE/OPT4RESH.TPL')
          IF(WQ.EQ.'OPTIMAXS') CALL OPENTSTP('TPLATE/OPTIMAXS.TPL')
          IF(WQ.EQ.'ROCKYMTN') CALL OPENTSTP('TPLATE/ROCKYMTN.TPL')
          IF(WQ.EQ.'RODENSTK') CALL OPENTSTP('TPLATE/RODENSTK.TPL')
          IF(WQ.EQ.'SILLOPTS') CALL OPENTSTP('TPLATE/SILLOPTS.TPL')
          IF(WQ.EQ.'SPECIALO') CALL OPENTSTP('TPLATE/SPECIALO.TPL')
          IF(WQ.EQ.'SPINDLER') CALL OPENTSTP('TPLATE/SPINDLER.TPL')
          IF(WQ.EQ.'SVGTINLY') CALL OPENTSTP('TPLATE/SVGTINLY.TPL')
          IF(WQ.EQ.'TROPELCO') CALL OPENTSTP('TPLATE/TROPELCO.TPL')
          IF(WQ.EQ.'TUCSONOP') CALL OPENTSTP('TPLATE/TUCSONOP.TPL')
          IF(WQ.EQ.'TOWEROPT') CALL OPENTSTP('TPLATE/TOWEROPT.TPL')
          IF(WQ.EQ.'TWOSIXOP') CALL OPENTSTP('TPLATE/TWOSIXOP.TPL')
          RETURN
      END
      SUBROUTINE TEST_PLATE_IT
          IMPLICIT NONE
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          INTEGER SFNUMBER
          REAL*8 TPVALUE
          CHARACTER INLINE*40
          IF(STI.EQ.1.AND.WC.EQ.'TESTRD') THEN
              OUTLYNE='"TESTRD" ASSIGNS A TEST PLATE RADIUS AND REMOVES ALL'
              CALL SHOWIT(0)
              OUTLYNE='SOLVES, PIKUPS AND VARIABLES WHICH WOULD CONFLICT WITH'
              CALL SHOWIT(0)
              OUTLYNE='THAT ASSIGNMENT'
              CALL SHOWIT(0)
              RETURN
          END IF
          IF(STI.EQ.1.AND.WC.EQ.'TESTCYL') THEN
              OUTLYNE=
     1        '"TESTCYL" ASSIGNS A TEST PLATE CYLINDER RADIUS AND REMOVES ALL'
              CALL SHOWIT(0)
              OUTLYNE='SOLVES, PIKUPS AND VARIABLES WHICH WOULD CONFLICT WITH'
              CALL SHOWIT(0)
              OUTLYNE='THAT ASSIGNMENT'
              CALL SHOWIT(0)
              RETURN
          END IF
          IF(SST.EQ.1.OR.SQ.EQ.1) THEN
              OUTLYNE=
     1        '"TESTRD" AND "TESTCYL" TAKE NO QUALIFIER OR STRING INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
              OUTLYNE=
     1        '"TESTRD" AND "TESTCYL" TAKE NO NUMERIC WORD #3, #4 OR #5 INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(S1.EQ.0.OR.S2.EQ.0) THEN
              OUTLYNE=
     1        '"TESTRD" AND "TESTCYL" REQUIRE EXPLICIT NUMERIC WORD'
              CALL SHOWIT(1)
              OUTLYNE=
     1        '#1 AND #2 INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(W1.LT.0.0D0.OR.W1.GT.SYSTEM1(20)) THEN
              OUTLYNE=
     1        'INVALID SURFACE NUMBER (NUMERIC WORD #1) ISSUED WITH A'
              CALL SHOWIT(1)
              OUTLYNE=
     1        '"TESTRD" OR "TESTCYL" COMMAND'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(WC.EQ.'TESTCYL'.AND.ALENS(24,INT(W1)).EQ.0.0D0) THEN
              OUTLYNE=
     1        '"TESTCYL" COMMAND REQUIRES THAT THE SURFACE BE DEFINED AS'
              CALL SHOWIT(1)
              OUTLYNE=
     1        'A TORIC SURFACE'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C     MAKE CHANGES TO THE SURFACE
          SFNUMBER=INT(W1)
          TPVALUE=W2
          IF(SYSTEM1(6).EQ.1.0D0) TPVALUE=TPVALUE/25.4D0
          IF(SYSTEM1(6).EQ.2.0D0) TPVALUE=TPVALUE/10.0D0
          IF(SYSTEM1(6).EQ.3.0D0) TPVALUE=TPVALUE
          IF(SYSTEM1(6).EQ.4.0D0) TPVALUE=TPVALUE/1000.0D0
          IF(WC.EQ.'TESTRD') THEN
C     DELETE CURVATURE SOLVES
              SAVE_KDP(1)=SAVEINPT(1)
              INPUT = 'U L'
              CALL PROCES
              WRITE(INLINE,10) SFNUMBER
10            FORMAT('CHG ',I3)
              INPUT(1:40)=INLINE
              CALL PROCES
              INPUT='CSD'
              CALL PROCES
              INPUT='PIKD RD'
              CALL PROCES
              INPUT='PIKD CV'
              CALL PROCES
              INPUT='PIKD PRO'
              CALL PROCES
              INPUT='PIKD NPRO'
              CALL PROCES
              INPUT='EOS'
              CALL PROCES
              INPUT='U VB'
              CALL PROCES
              WRITE(INLINE,11) SFNUMBER
11            FORMAT('DEL CV ',I3)
              INPUT(1:40)=INLINE
              CALL PROCES
              INPUT='EOS'
              CALL PROCES
              INPUT='U L'
              CALL PROCES
              WRITE(INLINE,10) SFNUMBER
              INPUT(1:40)=INLINE
              CALL PROCES
              WRITE(INLINE,12) TPVALUE
12            FORMAT('RD ',D23.15)
              INPUT(1:40)=INLINE
              CALL PROCES
              INPUT='EOS'
              CALL PROCES
              REST_KDP(1)=RESTINPT(1)
              WRITE(OUTLYNE,51) TPVALUE
51            FORMAT('TEST PLATE RADIUS = ',D23.15)
              CALL SHOWIT(1)
              WRITE(OUTLYNE,41) SFNUMBER
              CALL SHOWIT(1)
          END IF
          IF(WC.EQ.'TESTCYL') THEN
C     DELETE CURVATURE SOLVES
              SAVE_KDP(1)=SAVEINPT(1)
              INPUT = 'U L'
              CALL PROCES
              WRITE(INLINE,10) SFNUMBER
              INPUT(1:40)=INLINE
              CALL PROCES
              INPUT='CSD'
              CALL PROCES
              INPUT='PIKD RDTOR'
              CALL PROCES
              INPUT='PIKD CVTOR'
              CALL PROCES
              INPUT='PIKD PRO'
              CALL PROCES
              INPUT='PIKD NPRO'
              CALL PROCES
              INPUT='EOS'
              CALL PROCES
              INPUT='U VB'
              CALL PROCES
              WRITE(INLINE,21) SFNUMBER
21            FORMAT('DEL CVTOR ',I3)
              INPUT(1:40)=INLINE
              CALL PROCES
              INPUT='EOS'
              INPUT='U L'
              CALL PROCES
              WRITE(INLINE,10) SFNUMBER
              INPUT(1:40)=INLINE
              CALL PROCES
              WRITE(INLINE,22) TPVALUE
22            FORMAT('RDTOR ',D23.15)
              INPUT(1:40)=INLINE
              CALL PROCES
              INPUT='EOS'
              CALL PROCES
              REST_KDP(1)=RESTINPT(1)
              WRITE(OUTLYNE,31) TPVALUE
31            FORMAT('TEST PLATE CYLINDER RADIUS = ',D23.15)
              CALL SHOWIT(1)
              WRITE(OUTLYNE,41) SFNUMBER
41            FORMAT('HAS BEEN ASSIGNED TO SURFACE ',I3)
              CALL SHOWIT(1)
          END IF
          RETURN
      END
C SUB SPDSSI.FOR
      SUBROUTINE SPDSSI
C
          IMPLICIT NONE
C
C     THIS DOES THE SPDSSI COMMAND AT THE CMD LEVEL
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datspd.inc'
C
          IF(STI.EQ.1) THEN
              WRITE(OUTLYNE,*)
     1        'CURRENT SPOT DIAGRAM PLOTTING SSI = ',SPDSSIVAL
              CALL SHOWIT(1)
              IF(.NOT.SPOTSSI)
     1        WRITE(OUTLYNE,*) 'SPOT SSI WILL BE AUTOMATICALLY SET'
              CALL SHOWIT(1)
              RETURN
          END IF
          IF(SST.EQ.1.OR.S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1.OR.
     1    SQ.EQ.1) THEN
              WRITE(OUTLYNE,*)
     1        '"SPDSSI" ONLY TAKES NUMERIC WORD #1 INPUT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(DF1.EQ.1) THEN
              WRITE(OUTLYNE,*)
     1        '"SPDSSI" REQUIRES EXPLICIT NUMERIC WORD #1 INPUT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
          IF(W1.LE.0.0D0) THEN
              WRITE(OUTLYNE,*)
     1        'SPOT DIAGRAM PLOTTING SSI VALUE MUST BE GREATER THAN ZERO'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(DF1.EQ.0) THEN
              SPOTSSI=.TRUE.
              SPDSSIVAL=W1
          END IF
          RETURN
      END
C SUB DETECTOR.FOR
      SUBROUTINE DETECTOR
C
          IMPLICIT NONE
C
C     THIS DOES THE DET COMMAND AT THE CMD LEVEL
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datspd.inc'
C
          IF(STI.EQ.1) THEN
              WRITE(OUTLYNE,*)
     1        'CURRENT SPOT DIAGRAM DETECTOR SETTINGS:'
              CALL SHOWIT(1)
              IF(DETER.AND.DETTYP.NE.0) THEN
                  WRITE(OUTLYNE,*) 'DETECTOR WILL BE DRAWN'
                  CALL SHOWIT(1)
                  IF(DETTYP.EQ.1) WRITE(OUTLYNE,*) 'DETECTOR IS CIRCULAR'
                  IF(DETTYP.EQ.2) WRITE(OUTLYNE,*) 'DETECTOR IS RECTANGULAR'
                  CALL SHOWIT(1)
                  IF(DETTYP.EQ.1) THEN
                      WRITE(OUTLYNE,*) 'DETECTOR DIAMETER       = ',DETDIAM,' UNITS'
                      CALL SHOWIT(1)
                      IF(DETCENT) WRITE(OUTLYNE,*) 'DETECTOR CENTERED ON SPOT CENTROID'
                      IF(DETCHIEF)WRITE(OUTLYNE,*) 'DETECTOR CENTERED ON CHIEF RAY'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*) 'WITH DETECTOR X-OFFSET  = ',DETDELX,' UNITS'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*) 'WITH DETECTOR Y-OFFSET  = ',DETDELY,' UNITS'
                      CALL SHOWIT(1)
                  ELSE
                      WRITE(OUTLYNE,*) 'DETECTOR X-DIMENSION    = ',DETTX,' UNITS'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*) 'DETECTOR Y-DIMENSION    = ',DETTY,' UNITS'
                      CALL SHOWIT(1)
                      IF(DETCENT) WRITE(OUTLYNE,*) 'DETECTOR CENTERED ON SPOT CENTROID'
                      IF(DETCHIEF)WRITE(OUTLYNE,*) 'DETECTOR CENTERED ON CHIEF RAY'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*) 'WITH DETECTOR X-OFFSET  = ',DETDELX,' UNITS'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*) 'WITH DETECTOR Y-OFFSET  = ',DETDELY,' UNITS'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*) 'WITH THETA ANGLE OFFSET = ',DETTHETA,' DEGREES'
                      CALL SHOWIT(1)
                  END IF
              ELSE
                  WRITE(OUTLYNE,*) 'DETECTOR WILL NOT BE DRAWN'
                  CALL SHOWIT(1)
              END IF
              RETURN
          END IF
C     STRING CHECK
          IF(SST.EQ.1) THEN
              WRITE(OUTLYNE,*)
     1        '"DET" TAKES NO INPUT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C     QUALIFIER CHECK
          IF(SQ.EQ.0) THEN
              WRITE(OUTLYNE,*)
     1        '"DET" REQUIRES EXPLICIT QUALIFIER WORD INPUT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SQ.EQ.1) THEN
              IF(WQ.NE.'ON'.AND.WQ.NE.'OFF'.AND.WQ.NE.'CENT'.AND.WQ.NE.
     1        'CHIEF'.AND.WQ.NE.'CIRC'.AND.WQ.NE.'RECT') THEN
                  WRITE(OUTLYNE,*)
     1            'INVALID QUALIFIER WORD INPUT USED WITH "DET"'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
          IF(WQ.EQ.'ON'.OR.WQ.EQ.'OFF'.OR.WQ.EQ.'CENT'.OR.WQ.EQ.'CHIEF')
     1    THEN
              IF(SN.EQ.1) THEN
                  WRITE(OUTLYNE,*)
     1            'THIS FORM OF "DET" TAKES NO NUMERIC INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
          IF(WQ.EQ.'ON') THEN
              DETER=.TRUE.
              RETURN
          END IF
          IF(WQ.EQ.'OFF') THEN
              DETER=.FALSE.
              RETURN
          END IF
          IF(WQ.EQ.'CENT') THEN
              DETCENT=.TRUE.
              DETCHIEF=.FALSE.
              RETURN
          END IF
          IF(WQ.EQ.'CHIEF') THEN
              DETCENT=.FALSE.
              DETCHIEF=.TRUE.
              RETURN
          END IF
          IF(WQ.EQ.'CIRC') THEN
              IF(DF1.EQ.1)THEN
                  WRITE(OUTLYNE,*)
     1            '"DET CIRC" REQUIRES EXPLICIT NUMERIC WORD #1 INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(DF4.EQ.0.OR.DF5.EQ.0)THEN
                  WRITE(OUTLYNE,*)
     1            '"DET CIRC" TAKES NO NUMERIC WORD #4 OR #5 INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(W1.LE.0.0D0) THEN
                  WRITE(OUTLYNE,*)
     1            '"DET CIRC" REQUIRES POSITIVE NUMERIC WORD #1 INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              DETDIAM=W1
              IF(DF2.EQ.1) W2=0.0D0
              IF(DF3.EQ.1) W3=0.0D0
              DETDELX=W2
              DETDELY=W3
              DETTYP=1
              RETURN
          END IF
          IF(WQ.EQ.'RECT') THEN
              IF(DF1.EQ.1.OR.DF2.EQ.1)THEN
                  WRITE(OUTLYNE,*)
     1            '"DET RECT" REQUIRES EXPLICIT NUMERIC WORD #1 AND #2 INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(W1.LE.0.0D0.OR.W2.LE.0.0D0) THEN
                  WRITE(OUTLYNE,*)
     1            '"DET RECT" REQUIRES POSITIVE NUMERIC WORD #1 AND #2 INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(W5.LT.0.0D0.OR.W1.GT.360.0D0) THEN
                  WRITE(OUTLYNE,*)
     1            'THETA (NW5) MUST BE IN THE RANGE 0 TO 360 DEGREES'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              DETTX=W1
              DETTY=W2
              IF(DF3.EQ.1) W3=0.0D0
              IF(DF4.EQ.1) W4=0.0D0
              IF(DF5.EQ.1) W5=0.0D0
              DETDELX=W3
              DETDELY=W4
              DETTHETA=W5
              DETTYP=2
              RETURN
          END IF
C
          RETURN
      END
C SUB ORIENT.FOR
      SUBROUTINE ORIENT
C
          IMPLICIT NONE
C
C       THIS ROUTINE DOES THE ORIENT COMMAND
C
          INTEGER I,ORSF
C
          COMMON/SFOR/ORSF
C
          REAL*8 LVAL,MVAL,NVAL,MAG,SINA,COSA,SINF,COSF
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
          IF(WC.EQ.'ORIENT') THEN
C
C
              IF(STI.EQ.1) THEN
                  WRITE(OUTLYNE,800)
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,801)
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,802)
                  CALL SHOWIT(1)
 800              FORMAT('"ORIENT" SETS THE "LOOK VECTOR" PARALLEL')
 801              FORMAT('"TO THE LOCAL Z-AXIS OF THE SURFACE DESIGNATED')
 802              FORMAT('"BY NUMERIC WORD #1')
                  RETURN
              ELSE
              END IF
C       CHECK SYNTAX
              IF(SQ.EQ.1.OR.SST.EQ.1) THEN
                  OUTLYNE=
     1            '"ORIENT" TAKES NO QUALIFIER OR STRING INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
                  OUTLYNE=
     1            '"ORIENT" ONLY TAKES NUMERIC WORDS #1 AND #2 INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(DF1.EQ.1) THEN
                  OUTLYNE=
     1            '"ORIENT" REQUIRES EXPLICIT NUMERIC WORD #1 INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(W1.LT.0.0D0.OR.W1.GT.SYSTEM1(20)) THEN
                  OUTLYNE=
     1            'NUMERIC WORD #1, SURFACE #, BEYOND LEGAL RANGE'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C
C     MAKE SURE YOU HAVE VERTEX DATA WITHOUT OFFSETS AND SET TO
C     THE FIRST SURFACE WITHOUT AN INFINITE THICKNESS
              GLSURF=-99
              DO I=NEWIMG,NEWOBJ,-1
                  IF(DABS(ALENS(3,I)).LE.1.0D10) GLSURF=I
              END DO
              IF(GLSURF.EQ.-99) THEN
                  GLOBE=.FALSE.
                  OUTLYNE='ALL SURFACES WERE OF INFINITE THICKNESS'
                  CALL SHOWIT(1)
                  OUTLYNE='"ORIENT" DID NOT CHANGE THE "LOOK VECTOR"'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              GLOBE=.TRUE.
              OFFX=0.0D0
              OFFY=0.0D0
              OFFZ=0.0D0
              OFFA=0.0D0
              OFFB=0.0D0
              OFFC=0.0D0
C     RESET NEWIMG,NEWOBJ AND NEWREF
              CALL RESSUR
              CALL GLVERT
              CALL OLDSUR
              GLOBE=.FALSE.
C     VERTEX DATA EXISTS
C     NOW GET THE DIRECTION COSINES OF THE LOCAL Z-AXIS OF SURFACE
C     DESIGNATED BY W1
              ORSF=INT(W1)
              LVAL=VERTEX(10,INT(W1))
              MVAL=VERTEX(11,INT(W1))
              NVAL=VERTEX(12,INT(W1))
              MAG=DSQRT((LVAL**2)+(MVAL**2)+(NVAL**2))
              LOOKX=LVAL/MAG
              LOOKY=MVAL/MAG
              LOOKZ=NVAL/MAG
C       CALCULATE VIEALF AND VIEPHI
              SINA=LOOKY
              COSA=DSQRT((LOOKX**2)+(LOOKZ**2))
              IF(DABS(SINA).LE.1.0D-15.AND.DABS(COSA).LE.1.0D-15) THEN
                  VIEALF=0.0D0
              ELSE
                  VIEALF=DATAN2(SINA,COSA)
              END IF
              VIEALF=(180.0D0/PII)*VIEALF
              IF(VIEALF.GT.0.0D0) THEN
                  IF(VIEALF.GT.90.0D0) VIEALF=180.0D0-VIEALF
                  GO TO 1000
              ELSE
              END IF
              IF(VIEALF.LT.0.0D0) THEN
                  IF(VIEALF.LT.-90.0D0) VIEALF=-180.0D0-VIEALF
              ELSE
              END IF
 1000         CONTINUE
              IF(DABS(COSA).GT.1.0D-10) THEN
                  SINF=LOOKX/COSA
                  COSF=LOOKZ/COSA
                  IF(DABS(SINF).LE.1.0D-15.AND.DABS(COSF).LE.1.0D-15) THEN
                      VIEPHI=0.0D0
                  ELSE
                      VIEPHI=DATAN2(SINF,COSF)
                  END IF
                  VIEPHI=(180.0D0/PII)*VIEPHI
                  IF(VIEPHI.LT.0.0D0) VIEPHI=360.0D0+VIEPHI
              ELSE
                  VIEPHI=270.0D0
              END IF
              IF(DF2.EQ.0) THEN
                  LORIENT=.TRUE.
                  SORIENT=INT(W1)
              ELSE
                  LORIENT=.FALSE.
              END IF
              RETURN
          END IF
          IF(WC.EQ.'NORIENT') THEN
C
C       CHECK SYNTAX
              IF(SQ.EQ.1.OR.SST.EQ.1) THEN
                  OUTLYNE=
     1            '"NORIENT" TAKES NO QUALIFIER OR STRING INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
                  OUTLYNE=
     1            '"NORIENT" ONLY TAKES NUMERIC WORDS #1 AND #2 INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(DF1.EQ.1) THEN
                  OUTLYNE=
     1            '"NORIENT" REQUIRES EXPLICIT NUMERIC WORD #1 INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C
              IF(STI.EQ.1) THEN
                  WRITE(OUTLYNE,1800)
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,1801)
                  CALL SHOWIT(1)
 1800             FORMAT('"NORIENT" SETS THE "LOOK VECTOR" ANTI-PARALLEL')
 1801             FORMAT('"TO THE LOCAL Z-AXIS OF THE SURFACE DESIGNATED')
                  RETURN
              ELSE
              END IF
C
C     MAKE SURE YOU HAVE VERTEX DATA WITHOUT OFFSETS AND SET TO
C     THE FIRST SURFACE WITHOUT AN INFINITE THICKNESS
              GLSURF=-99
              DO I=NEWIMG,NEWOBJ,-1
                  IF(DABS(ALENS(3,I)).LE.1.0D10) GLSURF=I
              END DO
              IF(GLSURF.EQ.-99) THEN
                  GLOBE=.FALSE.
                  OUTLYNE='ALL SURFACES WERE OF INFINITE THICKNESS'
                  CALL SHOWIT(1)
                  OUTLYNE='"ORIENT" DID NOT CHANGE THE "LOOK VECTOR"'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              GLOBE=.TRUE.
              OFFX=0.0D0
              OFFY=0.0D0
              OFFZ=0.0D0
              OFFA=0.0D0
              OFFB=0.0D0
              OFFC=0.0D0
C     RESET NEWIMG,NEWOBJ AND NEWREF
              CALL RESSUR
              CALL GLVERT
              CALL OLDSUR
              GLOBE=.FALSE.
C     VERTEX DATA EXISTS
C     NOW GET THE DIRECTION COSINES OF THE LOCAL Z-AXIS OF SURFACE
C     DESIGNATED BY W1
              ORSF=INT(W1)
              LVAL=VERTEX(10,INT(W1))
              MVAL=VERTEX(11,INT(W1))
              NVAL=VERTEX(12,INT(W1))
              MAG=DSQRT((LVAL**2)+(MVAL**2)+(NVAL**2))
              LOOKX=-LVAL/MAG
              LOOKY=-MVAL/MAG
              LOOKZ=-NVAL/MAG
C       CALCULATE VIEALF AND VIEPHI
              SINA=LOOKY
              COSA=DSQRT((LOOKX**2)+(LOOKZ**2))
              IF(DABS(SINA).LE.1.0D-15.AND.DABS(COSA).LE.1.0D-15) THEN
                  VIEALF=0.0D0
              ELSE
                  VIEALF=DATAN2(SINA,COSA)
              END IF
              VIEALF=(180.0D0/PII)*VIEALF
              IF(VIEALF.GT.0.0D0) THEN
                  IF(VIEALF.GT.90.0D0) VIEALF=180.0D0-VIEALF
                  GO TO 1001
              ELSE
              END IF
              IF(VIEALF.LT.0.0D0) THEN
                  IF(VIEALF.LT.-90.0D0) VIEALF=-180.0D0-VIEALF
              ELSE
              END IF
 1001         CONTINUE
              IF(DABS(COSA).GT.1.0D-10) THEN
                  SINF=LOOKX/COSA
                  COSF=LOOKZ/COSA
                  IF(DABS(SINF).LE.1.0D-15.AND.DABS(COSF).LE.1.0D-15) THEN
                      VIEPHI=0.0D0
                  ELSE
                      VIEPHI=DATAN2(SINF,COSF)
                  END IF
                  VIEPHI=(180.0D0/PII)*VIEPHI
                  IF(VIEPHI.LT.0.0D0) VIEPHI=360.0D0+VIEPHI
              ELSE
                  VIEPHI=270.0D0
              END IF
              IF(DF2.EQ.0) THEN
                  LORIENT=.TRUE.
                  SORIENT=INT(W1)
              ELSE
                  LORIENT=.FALSE.
              END IF
              RETURN
          END IF
      END
C SUB ORSHIFT.FOR
      SUBROUTINE ORSHIFT
C
          IMPLICIT NONE
C
C     THIS DOES THE AUTOMATIC SHIFT CALCS FOR ORIENT
C
          REAL*8 X,Y,Z,XN,YN,ZN,ROT1X,ROT1Z,ROT2Y
     1    ,ROT2Z,AX,AY,AZ,AALF,APHI,
     2    VIEPH,VIEAL
C
          INTEGER I
C
          REAL*8 XVAL,YVAL,ZVAL
C
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
C     LOOK.VIEW TRANSFORMS

          ROT1X(AX,AZ,APHI)=((AX*DCOS(APHI))-(AZ*DSIN(APHI)))
          ROT1Z(AX,AZ,APHI)=((AX*DSIN(APHI))+(AZ*DCOS(APHI)))
C
          ROT2Z(AZ,AY,AALF)=((AZ*DCOS(AALF))+(AY*DSIN(AALF)))
          ROT2Y(AZ,AY,AALF)=((-AZ*DSIN(AALF))+(AY*DCOS(AALF)))
C
          X=0.0D0
          Y=0.0D0
C
          VIEPH=(PII/180.0D0)*VIEPHI
          VIEAL=(PII/180.0D0)*VIEALF
C
C     MAKE SURE YOU HAVE VERTEX DATA WITHOUT OFFSETS AND SET TO
C     THE FIRST SURFACE WITHOUT AN INFINITE THICKNESS
          GLSURF=-99
          DO I=NEWIMG,NEWOBJ,-1
              IF(DABS(ALENS(3,I)).LE.1.0D10) GLSURF=I
          END DO
          IF(GLSURF.EQ.-99) THEN
              GLOBE=.FALSE.
              OUTLYNE='ALL SURFACES WERE OF INFINITE THICKNESS'
              CALL SHOWIT(1)
              OUTLYNE='NO AUTOMATIC SHIFT CALCULATIONS WERE MADE'
              CALL SHOWIT(1)
              LORIENT=.FALSE.
              CALL MACFAL
              RETURN
          END IF
          GLOBE=.TRUE.
          OFFX=0.0D0
          OFFY=0.0D0
          OFFZ=0.0D0
          OFFA=0.0D0
          OFFB=0.0D0
          OFFC=0.0D0
C     RESET NEWIMG,NEWOBJ AND NEWREF
          CALL RESSUR
          CALL GLVERT
          CALL OLDSUR
          GLOBE=.FALSE.
C     ORIGIN OF ORIENT SURFACE IS
          XVAL=VERTEX(1,SORIENT)+ALENS(13,SORIENT)
          YVAL=VERTEX(2,SORIENT)+ALENS(12,SORIENT)
          ZVAL=VERTEX(3,SORIENT)
C
          X=XVAL
          Y=YVAL
          Z=ZVAL
          X=X-XROT
          Y=Y-YROT
          Z=Z-ZROT
          XN=ROT1X(X,Z,VIEPH)
          YN=Y
          ZN=ROT1Z(X,Z,VIEPH)
          X=XN
          Y=YN
          Z=ZN
C
          ZN=ROT2Z(Z,Y,VIEAL)
          YN=ROT2Y(Z,Y,VIEAL)
          XN=X
          XVAL=XN
          YVAL=YN
          ZVAL=ZN
C
C
C     RIGHT NOW,COORDINATES ARE IN WORLD COORDINATES
C     CONVERT THEM TO DEVICE INDEPENDENT GRAPHICS COORDINATES IN
C     TWO STEPS.
C
C     THE WORLD X PLOTS TO THE PLOTTER X
C     THE WORLD Y PLOTS TO THE PLOTTER Y
C
C     STEP 1: CONVERT USING AN APPROPRIATE SCALE FACTOR
C               CALCULATING AN APPROPRIATE FACTOR IF NECESSARY
          XVAL=(XVAL/SCFAX)*1000.0D0
          YVAL=(YVAL/SCFAY)*1000.0D0

C     THESE ARE THE NEW SHIFTS
          PXSHFT= -INT(XVAL)
          PYSHFT= -INT(YVAL)
C
          RETURN
      END


C SUB VIGCAL.FOR
      SUBROUTINE VIGCAL(N,VLO,VHI,XY)
C
C     XY=1 FOR XZ
C     XY=2 FOR YZ
C     N IS NUMBER OF POINTS FROM -1 TO 1
C
          IMPLICIT NONE
C
          REAL*8 VHI,VLO
C
          INTEGER N,XY,ACOD,OACOD,I
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
C     FIRST LOWER VALUE
C     SET ACOD TO FAILURE
          ACOD=0
          DO I=-N,N,1
              IF(XY.EQ.1) THEN
                  WW1=0.0D0
                  WW2=DBLE(I)/DBLE(N)
              END IF
              IF(XY.EQ.2) THEN
                  WW1=DBLE(I)/DBLE(N)
                  WW2=0.0D0
              END IF
              WW3=SYSTEM1(11)
              WVN=SYSTEM1(11)
C               TRACE RAY
              MSG=.FALSE.
              CALL QTRA1(.FALSE.)
              OACOD=ACOD
              IF(RAYCOD(1).EQ.0.OR.RAYCOD(1).EQ.7)  ACOD=1
              IF(RAYCOD(1).NE.0.AND.RAYCOD(1).NE.7) ACOD=0
C
              IF(OACOD.EQ.0.AND.ACOD.EQ.1) THEN
                  VLO=DBLE(I)/DBLE(N)
                  GO TO 10
              END IF
          END DO
C     IF WE ARE HERE, THERE IS NO VIGNETTING OR CONPLETE
C     VIGNETTING AT THE LOWER END
          IF(ACOD.EQ.1) THEN
              VLO=-1.0D0
          END IF
          IF(ACOD.EQ.0) THEN
              VLO=0.0D0
          END IF
 10       CONTINUE
C     NOW UPPER VALUE
          ACOD=0
          DO I=N,-N,-1
              IF(XY.EQ.1) THEN
                  WW1=0.0D0
                  WW2=DBLE(I)/DBLE(N)
              END IF
              IF(XY.EQ.2) THEN
                  WW2=0.0D0
                  WW1=DBLE(I)/DBLE(N)
              END IF
              WW3=SYSTEM1(11)
              WVN=SYSTEM1(11)
C               TRACE RAY
              MSG=.FALSE.
              CALL QTRA1(.FALSE.)
              OACOD=ACOD
              IF(RAYCOD(1).EQ.0.OR.RAYCOD(1).EQ.7)  ACOD=1
              IF(RAYCOD(1).NE.0.AND.RAYCOD(1).NE.7) ACOD=0
C
              IF(OACOD.EQ.0.AND.ACOD.EQ.1) THEN
                  VHI=DBLE(I)/DBLE(N)
                  GO TO 20
              END IF
          END DO
C     IF WE ARE HERE, THERE IS NO AT THE UPPER END OR COMPLETE
C     VIGNETTING
          IF(ACOD.EQ.1) THEN
              VHI=1.0D0
          END IF
          IF(ACOD.EQ.0) THEN
              VHI=0.0D0
          END IF
 20       CONTINUE
          IF(VHI.LE.VLO) THEN
C       BAD SHIT HAPPENED, RESET TO DEFAULTS AND HOPE FOR THE BEST
              VHI=1.0D0
              VLO=-1.0D0
          ELSE
C       NO CORRECTION
          END IF

          RETURN
      END
