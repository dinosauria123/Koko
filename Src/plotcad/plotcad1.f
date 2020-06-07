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

C       FIRST FILE OF PLOT/CAD ROUTINES

C SUB PPLOTT.FOR
      SUBROUTINE PPLOTT
          USE NSSMOD
C
          IMPLICIT NONE
C
C       THIS PROGRAM CONTROLS THE ALL PLOTTING PROCEDURES.
C       ALL THE PLOTTING COMMAND ARE EXECUTED FROM HERE.
C
          LOGICAL PLQ,FANEXT,EXIS94,OPEN94,EXIS92,EXIS103,OPEN103
C
          INTEGER CACOCHVIE
C
          REAL UPPER,LOWER
C
          REAL*8 SFI,MDX,MDY,GAMGAM,V1,V2
C
          CHARACTER NAME*40
C
          INTEGER N,I,J
C
          COMMON/FANEXI/FANEXT
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          INCLUDE 'dathgr.inc'
C
          IF(WC.EQ.'PLOT') THEN
              PLQ=.FALSE.
              IF(WC.EQ.'PNOTE')       PLQ=.TRUE.
              IF(WQ.EQ.'UXAXIS')      PLQ=.TRUE.
              IF(WQ.EQ.'UYAXIS')      PLQ=.TRUE.
              IF(WQ.EQ.'UXAXRNG')     PLQ=.TRUE.
              IF(WQ.EQ.'UYAXRNG')     PLQ=.TRUE.
              IF(WQ.EQ.'UXAXISLB')    PLQ=.TRUE.
              IF(WQ.EQ.'UYAXISLB')    PLQ=.TRUE.
              IF(WQ.EQ.'UPLOT')       PLQ=.TRUE.
              IF(WQ.EQ.'UYLINE')      PLQ=.TRUE.
              IF(WQ.EQ.'UXLINE')      PLQ=.TRUE.
              IF(WQ.EQ.'END')         PLQ=.TRUE.
              IF(WQ.EQ.'LI')          PLQ=.TRUE.
              IF(WQ.EQ.'LBL')         PLQ=.TRUE.
              IF(WQ.EQ.'NOLBL')       PLQ=.TRUE.
              IF(WQ.EQ.'FIGURE')      PLQ=.TRUE.
              IF(WQ.EQ.'NOLI')        PLQ=.TRUE.
              IF(WQ.EQ.'AXIS')        PLQ=.TRUE.
              IF(WQ.EQ.'NSSSCALE')    PLQ=.TRUE.
              IF(WQ.EQ.'SCALE')       PLQ=.TRUE.
              IF(WQ.EQ.'NOSCALE')     PLQ=.TRUE.
              IF(WQ.EQ.'YESCALE')     PLQ=.TRUE.
              IF(WQ.EQ.'SIZE')        PLQ=.TRUE.
              IF(WQ.EQ.'NOSIZE')      PLQ=.TRUE.
              IF(WQ.EQ.'YESIZE')      PLQ=.TRUE.
              IF(WQ.EQ.'ORIGIN')      PLQ=.TRUE.
              IF(WQ.EQ.'LSTYLE')      PLQ=.TRUE.
              IF(WQ.EQ.'SYMBOL')      PLQ=.TRUE.
              IF(WQ.EQ.'NEW')         PLQ=.TRUE.
              IF(WQ.EQ.'NAME')        PLQ=.TRUE.
              IF(WQ.EQ.'PEN')         PLQ=.TRUE.
              IF(WC.EQ.'DRAW')        PLQ=.TRUE.
              IF(WC.EQ.'LISTDRAW')    PLQ=.TRUE.
              IF(WC.EQ.'DRAWFAN')     PLQ=.TRUE.
              IF(WC.EQ.'PLT_FAN')     PLQ=.TRUE.
              IF(WC.EQ.'GRAOUT')      PLQ=.TRUE.
              IF(WQ.EQ.'LOOK')        PLQ=.TRUE.
              IF(WQ.EQ.'YESLOOK')     PLQ=.TRUE.
              IF(WQ.EQ.'NOLOOK')      PLQ=.TRUE.
              IF(WQ.EQ.'VIEW')        PLQ=.TRUE.
              IF(WQ.EQ.'YESVIEW')     PLQ=.TRUE.
              IF(WQ.EQ.'NOVIEW')      PLQ=.TRUE.
              IF(WQ.EQ.'DASH')        PLQ=.TRUE.
              IF(WQ.EQ.'NODASH')      PLQ=.TRUE.
              IF(WQ.EQ.'CHNOTE')      PLQ=.TRUE.
              IF(WQ.EQ.'NOTE')        PLQ=.TRUE.
              IF(WQ.EQ.'ACC')         PLQ=.TRUE.
              IF(WQ.EQ.'RAY')         PLQ=.TRUE.
              IF(WQ.EQ.'RAYS')        PLQ=.TRUE.
              IF(WQ.EQ.'RAYSCO')      PLQ=.TRUE.
              IF(WQ.EQ.'PROF')        PLQ=.TRUE.
              IF(WQ.EQ.'PROFX')       PLQ=.TRUE.
              IF(WQ.EQ.'PROFY')       PLQ=.TRUE.
              IF(WQ.EQ.'EDGEX')       PLQ=.TRUE.
              IF(WQ.EQ.'EDGEY')       PLQ=.TRUE.
              IF(WQ.EQ.'CLAP')        PLQ=.TRUE.
              IF(WQ.EQ.'COBS')        PLQ=.TRUE.
              IF(WQ.EQ.'MIRROR')      PLQ=.TRUE.
              IF(WQ.EQ.'LEFT')        PLQ=.TRUE.
              IF(WQ.EQ.'RIGHT')       PLQ=.TRUE.
              IF(WQ.EQ.'CENTER')      PLQ=.TRUE.
              IF(WQ.EQ.'GAMMA')       PLQ=.TRUE.
              IF(WQ.EQ.'XSHIFT')      PLQ=.TRUE.
              IF(WQ.EQ.'YSHIFT')      PLQ=.TRUE.
              IF(WC.EQ.'PLTXFAN')     PLQ=.TRUE.
              IF(WC.EQ.'PLTYFAN')     PLQ=.TRUE.
              IF(WC.EQ.'PLTNFAN')     PLQ=.TRUE.
              IF(WC.EQ.'PLTPFAN')     PLQ=.TRUE.
              IF(WC.EQ.'PLTXYFAN')    PLQ=.TRUE.
              IF(WC.EQ.'PLTYXFAN')    PLQ=.TRUE.
              IF(WC.EQ.'PLOTFANS')    PLQ=.TRUE.
              IF(WC.EQ.'ORIENT')      PLQ=.TRUE.
              IF(WC.EQ.'NORIENT')     PLQ=.TRUE.
              IF(WQ.EQ.'FRAME')       PLQ=.TRUE.
              IF(WQ.EQ.'FOOT')        PLQ=.TRUE.
              IF(WQ.EQ.'RHFOOT')      PLQ=.TRUE.
              IF(WQ.EQ.'VERTLINE')    PLQ=.TRUE.
              IF(WC.EQ.'PFANAXIS')    PLQ=.TRUE.
              IF(WC.EQ.'PFANLBL')     PLQ=.TRUE.
              IF(WC.EQ.'PFANCOMP')    PLQ=.TRUE.
              IF(WC.EQ.'PFANCAP')     PLQ=.TRUE.
              IF(WC.EQ.'PFANSSI')     PLQ=.TRUE.
              IF(WQ.EQ.'NSSSURFS')    PLQ=.TRUE.
              IF(WQ.EQ.'NSSSURF')     PLQ=.TRUE.
              IF(WQ.EQ.'NSSRAYS')     PLQ=.TRUE.
              IF(WQ.EQ.'NSSSPOT')     PLQ=.TRUE.
              IF(WQ.EQ.'LINE')        PLQ=.TRUE.
              IF(WQ.EQ.'PMRAYX')        PLQ=.TRUE.
              IF(WQ.EQ.'PMRAYY')        PLQ=.TRUE.
              IF(WQ.EQ.'PCRAYX')        PLQ=.TRUE.
              IF(WQ.EQ.'PCRAYY')        PLQ=.TRUE.
              IF(.NOT.PLQ) THEN
                  OUTLYNE='INVALID QUALIFIER USED WITH "PLOT"'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
C
C       PLOT NEW
          IF(WC.EQ.'PLOT'.AND.WQ.EQ.'NEW') THEN
C       INITIALIZE PLOTTING
C       CHECK SYNTAX
              IF(SN.EQ.1
     1        .OR.SST.EQ.1.AND.STI.NE.1) THEN
                  OUTLYNE='"PLOT NEW" TAKES NO ADDITIONAL INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(STI.EQ.1) THEN
                  OUTLYNE='"PLOT NEW ?" HAS NO MEANING'
                  CALL SHOWIT(1)
                  RETURN
              END IF
              CALL PLTDEV
              RETURN
          END IF
          IF(WC.EQ.'DRAWFAN') GO TO 654
          IF(WC.EQ.'PLT_FAN') GO TO 654
          IF(WQ.NE.'NEW'
     1    .OR.WC.EQ.'ORIENT'.OR.WC.EQ.'NORIENT'.OR.WC.EQ.'PNOTE') THEN
              IF(DEVTYP.NE.1) THEN
                  OUTLYNE=
     1            '"PLOT NEW" MUST BE ISSUED BEFORE PLOTTING CAN PROCEED'
                  CALL SHOWIT(1)
                  OUTLYNE=
     1            'NO ACTION TAKEN'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
 654      CONTINUE
C
          IF(WC.EQ.'PLOT'.AND.WQ.EQ.'UXAXIS') THEN
C       UXAXIS
              LOWER=REAL(W1)
              UPPER=REAL(W2)
              IF(DF3.EQ.0) UUDX=REAL(W3)
              IF(UUDX.EQ.0.0) UUDX=5.0
              CALL PLOTUXAXIS(LOWER,UPPER)
          END IF
C
          IF(WC.EQ.'PLOT'.AND.WQ.EQ.'UYAXIS') THEN
C       UYAXIS
              LOWER=REAL(W1)
              UPPER=REAL(W2)
              IF(DF1.EQ.0) UUDY=REAL(W3)
              IF(UUDY.EQ.0.0) UUDY=10.0
              CALL PLOTUYAXIS(LOWER,UPPER)
          END IF
C
          IF(WC.EQ.'PLOT'.AND.WQ.EQ.'UXAXRNG') THEN
C       UXAXIS
              LOWER=REAL(W1)
              UPPER=REAL(W2)
              CALL PLOTUXAXRNG(LOWER,UPPER)
          END IF
C
          IF(WC.EQ.'PLOT'.AND.WQ.EQ.'UYAXRNG') THEN
C       UYAXIS
              LOWER=REAL(W1)
              UPPER=REAL(W2)
              CALL PLOTUYAXRNG(LOWER,UPPER)
          END IF
C
          IF(WC.EQ.'PLOT'.AND.WQ.EQ.'UXAXISLB') THEN
C       UXAXISLB
              NAME(1:40)=WS(1:40)
              DO I=40,1,-1
                  IF(NAME(I:I).NE.' ') THEN
                      N=I
                      GO TO 10
                  END IF
              END DO
 10           CONTINUE

              CALL PLOTUXAXISLB(NAME,N)
          END IF
C
          IF(WC.EQ.'PLOT'.AND.WQ.EQ.'UYAXISLB') THEN
C       UYAXISLB
              NAME(1:40)=WS(1:40)
              DO I=40,1,-1
                  IF(NAME(I:I).NE.' ') THEN
                      N=I
                      GO TO 11
                  END IF
              END DO
 11           CONTINUE

              CALL PLOTUYAXISLB(NAME,N)
          END IF
C
          IF(WC.EQ.'PLOT'.AND.WQ.EQ.'UPLOT') THEN
              CALL PLOTUFUNC
          END IF
          IF(WC.EQ.'PLOT'.AND.WQ.EQ.'UYLINE') THEN
              CALL PLOTUFUNCY
          END IF
          IF(WC.EQ.'PLOT'.AND.WQ.EQ.'UXLINE') THEN
              CALL PLOTUFUNCX
          END IF
C
C       PLI
          IF(WC.EQ.'PLOT'.AND.WQ.EQ.'NAME') CALL PLTNAM
C
C       PLOT FRAME
          IF(WC.EQ.'PLOT'.AND.WQ.EQ.'FRAME') CALL PLTFRM
C
C       PLOT END
          IF(WC.EQ.'PLOT'.AND.WQ.EQ.'END') CALL PLTSTP
C
C       LISTDRAW
          IF(WC.EQ.'LISTDRAW') CALL LDRAW
C
C       DRAW
          IF(WC.EQ.'DRAW') CALL PDRAW

C       DRAWFAN
          IF(WC.EQ.'DRAWFAN') CALL DRAWFAN
C       PLT_FAN
          IF(WC.EQ.'PLT_FAN') CALL PLT_FAN
C
C       GRAOUT
          IF(WC.EQ.'GRAOUT') THEN
              CALL GRAOUT
          END IF
C
C       PLOT LI
          IF(WC.EQ.'PLOT'.AND.WQ.EQ.'LI') CALL PLTLI
C
C       PNOTE
          IF(WC.EQ.'PNOTE') CALL MAKEPNOTE
C
C       PLOT LBL
          IF(WC.EQ.'PLOT'.AND.WQ.EQ.'LBL') CALL PLTLBLL
C
C       PLOT NOLBL
          IF(WC.EQ.'PLOT'.AND.WQ.EQ.'NOLBL') CALL PLTLBLL
C
C       PLOT FIGURE
          IF(WC.EQ.'PLOT'.AND.WQ.EQ.'FIGURE') CALL PLTFIG
C
C       PLOT NOLI
          IF(WC.EQ.'PLOT'.AND.WQ.EQ.'NOLI') CALL PLTLI
C
C       PLOT AXIS
          IF(WC.EQ.'PLOT'.AND.WQ.EQ.'AXIS') CALL PLTAX
C
C       PLOT SCALE, NOSCALE AND YESCALE
          IF(WC.EQ.'PLOT'.AND.WQ.EQ.'SCALE') CALL PLTSZ
          IF(WC.EQ.'PLOT'.AND.WQ.EQ.'NOSCALE') CALL PLTSZ
          IF(WC.EQ.'PLOT'.AND.WQ.EQ.'YESCALE') CALL PLTSZ
C
C       PLOT NSSSCALE
          IF(WC.EQ.'PLOT'.AND.WQ.EQ.'NSSSCALE') CALL NSSPLTSZ
C
C       PLOT SIZE, NOSIZE AND YESIZE
          IF(WC.EQ.'PLOT'.AND.WQ.EQ.'SIZE') CALL PLTSCL
          IF(WC.EQ.'PLOT'.AND.WQ.EQ.'NOSIZE') CALL PLTSCL
          IF(WC.EQ.'PLOT'.AND.WQ.EQ.'YESIZE') CALL PLTSCL
C
C       PLOT ORIGIN
          IF(WC.EQ.'PLOT'.AND.WQ.EQ.'ORIGIN') CALL PLTORG
C
C       PLOT SYMBOL
          IF(WC.EQ.'PLOT'.AND.WQ.EQ.'SYMBOL') CALL PLTSYM
C
C       LSTYLE
          IF(WQ.EQ.'LSTYLE') CALL PLTSTL
C
C       LOOK
          IF(WQ.EQ.'LOOK'.OR.WQ.EQ.'YESLOOK'.OR.WQ.EQ.'NOLOOK')
     1    CALL PLTLOK
C
C       VIEW
          IF(WQ.EQ.'VIEW'.OR.WQ.EQ.'YESVIEW'.OR.WQ.EQ.'NOVIEW')
     1    CALL PLTVIE
C
C       PEN
          IF(WQ.EQ.'PEN') CALL PLTPEN
C
C       DASH
          IF(WQ.EQ.'DASH') CALL PLTDSH
C
C       NODASH
          IF(WQ.EQ.'NODASH') CALL PLTDSH
C
C       NOTE
          IF(WQ.EQ.'NOTE') CALL PLNOTE
C
C       NOTE
          IF(WQ.EQ.'CHNOTE') CALL CHSIZE
C
C       ACC
          IF(WQ.EQ.'ACC') CALL PLTACC
C
C       RAY
          IF(WQ.EQ.'RAY') CALL PLTRAE
C
C       RAYS
          IF(WQ.EQ.'RAYS') THEN
              CACOCHVIE=0
              IF(.NOT.VIGOFF) CACOCHVIE=1
              F34=1
              MSG=.FALSE.
              CALL PLTRAYS(CACOCHVIE)
              F34=0
              MSG=.TRUE.
              CACOCH=0
              CACOCHVIE=0
              RETURN
          END IF
C       RAYSCO
          IF(WQ.EQ.'RAYSCO') THEN
              VIGOFF=.TRUE.
              CACOCH=1
              CACOCHVIE=1
              F34=1
              MSG=.FALSE.
              CALL PLTRAYS(CACOCHVIE)
              F34=0
              MSG=.TRUE.
              CACOCH=0
              CACOCHVIE=0
              RETURN
          END IF
C
C       PMRAYX
          IF(WQ.EQ.'PMRAYX') THEN
              SAVE_KDP(1)=SAVEINPT(1)
              INPUT='OUT NULL'
              CALL PROCES
              INPUT='GPXTY ALL'
              CALL PROCES
              INPUT='OUT TP'
              CALL PROCES
              REST_KDP(1)=RESTINPT(1)
              IF(GPRAYEXT) THEN
                  CALL PLTPRAE(1)
                  CALL PLTPRAE(2)
              ELSE
                  WRITE(OUTLYNE,*) 'NO GENERALIZED PARAXIAL RAYS COULD BE TRACED'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*) 'NO GENERALIZED PARAXIAL RAYS EXISTS TO PLOT'
                  CALL SHOWIT(1)
              END IF
          END IF
C
C       PMRAYY
          IF(WQ.EQ.'PMRAYY') THEN
              SAVE_KDP(1)=SAVEINPT(1)
              INPUT='OUT NULL'
              CALL PROCES
              INPUT='GPXTY ALL'
              CALL PROCES
              INPUT='OUT TP'
              CALL PROCES
              REST_KDP(1)=RESTINPT(1)
              IF(GPRAYEXT) THEN
                  CALL PLTPRAE(3)
                  CALL PLTPRAE(4)
              ELSE
                  WRITE(OUTLYNE,*) 'NO GENERALIZED PARAXIAL RAYS COULD BE TRACED'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*) 'NO GENERALIZED PARAXIAL RAYS EXISTS TO PLOT'
                  CALL SHOWIT(1)
              END IF
          END IF
C
C       PCRAYX
          IF(WQ.EQ.'PCRAYX') THEN
              SAVE_KDP(1)=SAVEINPT(1)
              INPUT='OUT NULL'
              CALL PROCES
              INPUT='GPXTY ALL'
              CALL PROCES
              INPUT='OUT TP'
              CALL PROCES
              REST_KDP(1)=RESTINPT(1)
              IF(GPRAYEXT) THEN
                  CALL PLTPRAE(5)
                  CALL PLTPRAE(6)
              ELSE
                  WRITE(OUTLYNE,*) 'NO GENERALIZED PARAXIAL RAYS COULD BE TRACED'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*) 'NO GENERALIZED PARAXIAL RAYS EXISTS TO PLOT'
                  CALL SHOWIT(1)
              END IF
          END IF
C
C       PCRAYY
          IF(WQ.EQ.'PCRAYY') THEN
              SAVE_KDP(1)=SAVEINPT(1)
              INPUT='OUT NULL'
              CALL PROCES
              INPUT='GPXTY ALL'
              CALL PROCES
              INPUT='OUT TP'
              CALL PROCES
              REST_KDP(1)=RESTINPT(1)
              IF(GPRAYEXT) THEN
                  CALL PLTPRAE(7)
                  CALL PLTPRAE(8)
              ELSE
                  WRITE(OUTLYNE,*) 'NO GENERALIZED PARAXIAL RAYS COULD BE TRACED'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*) 'NO GENERALIZED PARAXIAL RAYS EXISTS TO PLOT'
                  CALL SHOWIT(1)
              END IF
          END IF

C
C       LINE
          IF(WQ.EQ.'LINE') THEN
              IF(NORAYPLOT) THEN
                  OUTLYNE=
     1            'AT LEAST ONE RAY MUST BE PLOTTED BEFORE 3D-LINES MAY BE PLOTTED'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              ELSE
                  CALL PLTLINE
              END IF
          END IF
C
C       VERTLINE
          IF(WQ.EQ.'VERTLINE') CALL VERTLINE
C
C       FOOT
          IF(WQ.EQ.'FOOT') THEN
              call settwocolors2
C
              OPEN94=.FALSE.
              EXIS94=.FALSE.
              INQUIRE(FILE=trim(HOME)//'FOOT1.DAT',OPENED=OPEN94)
              INQUIRE(FILE=trim(HOME)//'FOOT1.DAT',EXIST=EXIS94)
              IF(.NOT.EXIS94) THEN
                  OUTLYNE=
     1            'NO FOOT PRINT DATA EXISTS TO PLOT WITH "PLOT FOOT"'
                  CALL SHOWIT(1)
                  OUTLYNE='NO ACTION TAKEN'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(OPEN94) CALL CLOSE_FILE(94,1)
C     MAKE SURE YOU HAVE VERTEX DATA WITHOUT OFFSETS AND SET TO
C     THE FIRST SURFACE WITHOUT AN INFINITE THICKNESS
              GLSURF=-99
              DO I=NEWIMG,0,-1
                  IF(DABS(ALENS(3,I)).LE.1.0D10) GLSURF=I
              END DO
              IF(GLSURF.EQ.-99) THEN
                  GLOBE=.FALSE.
                  OUTLYNE='ALL SURFACES WERE OF INFINITE THICKNESS'
                  CALL SHOWIT(1)
                  OUTLYNE='NO OPTICAL SYSTEM PLOT COULD BE MADE'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              CALL PLTFOOT
              RETURN
          END IF
C       RHFOOT
          IF(WQ.EQ.'RHFOOT') THEN
C
              OPEN103=.FALSE.
              EXIS103=.FALSE.
              INQUIRE(FILE=trim(HOME)//'RHFOOT.DAT',OPENED=OPEN103)
              INQUIRE(FILE=trim(HOME)//'RHFOOT.DAT',EXIST=EXIS103)
              IF(.NOT.EXIS103) THEN
                  OUTLYNE=
     1            'NO RHFOOT PRINT DATA EXISTS TO PLOT WITH "PLOT RHFOOT"'
                  CALL SHOWIT(1)
                  OUTLYNE='NO ACTION TAKEN'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(OPEN103) CALL CLOSE_FILE(103,1)
C     MAKE SURE YOU HAVE VERTEX DATA WITHOUT OFFSETS AND SET TO
C     THE FIRST SURFACE WITHOUT AN INFINITE THICKNESS
              CALL PLTRHFOOT
              RETURN
          END IF
C
C       NSSSURFS
          IF(WQ.EQ.'NSSSURFS') THEN
              IF(NEXISTN) THEN
                  call setfourcolors
                  CALL PLTNSSSFS
                  RETURN
              ELSE
                  OUTLYNE=
     1            'NO NSS DATABASE EXISTS, NO NSS SURFACES CAN BE PLOTTED'
                  CALL MACFAL
                  RETURN
              END IF
          END IF
C
C       NSSSURF
          IF(WQ.EQ.'NSSSURF') THEN
              IF(NEXISTN) THEN
                  CALL PLTNSSSF
                  RETURN
              ELSE
                  OUTLYNE=
     1            'NO NSS DATABASE EXISTS, NO NSS SURFACES CAN BE PLOTTED'
                  CALL MACFAL
                  RETURN
              END IF
          END IF
C
C       NSSRAYS
          IF(WQ.EQ.'NSSRAYS') THEN
              EXIS92=.FALSE.
              INQUIRE(FILE=trim(HOME)//'NSSHT.DAT',EXIST=EXIS92)
              IF(NEXISTN.AND.EXIS92) THEN
                  CALL PLTNSSRY
                  CALL CLOSE_FILE(92,1)
                  RETURN
              ELSE
                  IF(.NOT.NEXISTN) THEN
                      OUTLYNE=
     1                'NO NSS DATABASE EXISTS, NO NSS RAYS CAN BE PLOTTED'
                      CALL SHOWIT(1)
                  END IF
                  IF(.NOT.EXIS92) THEN
                      OUTLYNE=
     1                'NO NSS RAY HISTORY FILE EXISTS, NO NSS RAYS CAN BE PLOTTED'
                      CALL SHOWIT(1)
                  END IF
                  CALL MACFAL
                  RETURN
              END IF
          END IF
C
C       NSSSPOT
          IF(WQ.EQ.'NSSSPOT') THEN
              IF(.NOT.NSSSPOTEXIST) THEN
                  OUTLYNE=
     1            'NO NSS SPOT DIAGRAM EXISTS, NO NSS SPOTS CAN BE PLOTTED'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              EXIS92=.FALSE.
              INQUIRE(FILE=trim(HOME)//'NSSHT.DAT',EXIST=EXIS92)
              V1=DBLE(NSSSPOTSURF)
              V2=W1
              IF(DF1.EQ.1) V2=1.0D0
              V2=W1
              IF(INT(V2).LT.1) V2=1.0D0
              CALL PLTNSSSPOTS(V1,V2)
              CALL CLOSE_FILE(92,1)
              RETURN
          END IF
C
C       PROFX
          IF(WQ.EQ.'PROFX') THEN
              DF3=0
              S3=1
              W3=0.0D0
              WQ='PROF'
          END IF
C
C       PROFY
          IF(WQ.EQ.'PROFY') THEN
              DF3=0
              S3=1
              W3=90.0D0
              WQ='PROF'
          END IF
C
C       PROF
          IF(WQ.EQ.'PROF') THEN
              CALL PLTPRO1
              RETURN
          END IF
C
C       EDGEX OR EDGEY
          IF(WQ.EQ.'EDGEX'.OR.WQ.EQ.'EDGEY') CALL PLTEDG
C
C       CLAP

          IF(WQ.EQ.'CLAP') THEN
C
C       CHECK SYNTAX
              IF(SST.EQ.1) THEN
                  OUTLYNE=
     1            '"PLOT CLAP" TAKES NO STRING INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(S4.EQ.1) THEN
                  OUTLYNE=
     1            '"PLOT CLAP" ONLY TAKES NUMERIC WORDS #1, #2, #3 AND #5 INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(DF3.EQ.0.AND.W3.LE.0.0D0.OR.
     1        DF3.EQ.0.AND.W3.GT.1.0D0) THEN
                  OUTLYNE=
     1            'THE SCALING FACTOR IN NUMERIC WORD #3 MUST BE GREATER'
                  CALL SHOWIT(1)
                  OUTLYNE=
     1            'THAN ZERO AND LESS THAN OR EQUAL TO 1.0'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(STI.EQ.1) THEN
                  WRITE(OUTLYNE,800)
                  CALL SHOWIT(1)
 800              FORMAT('QUERRY (?) HAS NO MEANING WITH "PLOT CLAP"')
                  RETURN
              ELSE
              END IF

              IF(DF1.EQ.0.AND.W1.LT.0.0D0) W1=SYSTEM1(20)+W1
              IF(DF2.EQ.0.AND.W2.LT.0.0D0) W2=SYSTEM1(20)+W2
              IF(DF3.EQ.1) W3=1.0D0
C       DEFAULT VALUES
              IF(DF1.EQ.1) THEN
                  IF(DABS(ALENS(3,0)).GT.1.0D10) THEN
                      W1=DBLE(1)
                  ELSE
                      W1=DBLE(0)
                  END IF
              ELSE
C       DF1 NOT 1, W1 EXPLICITLY ENTERED
              END IF
              STASUR=INT(W1)
              IF(DF2.EQ.1) THEN
                  W2=DBLE(NEWIMG)
              ELSE
C       DF2 NOT 1, W2 EXPLICITLY ENTERED
              END IF
              STPSUR=INT(W2)
              IF(INT(W1).LT.0) THEN
C       INVALID NUMERIC WORD #1
                  OUTLYNE=
     1            'SURFACE NUMBER (NUMERIC WORD #1) IS BEYOND LEGAL RANGE'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(INT(W2).GT.NEWIMG) THEN
C       INVALID NUMERIC WORD #2
                  OUTLYNE=
     1            'SURFACE NUMBER (NUMERIC WORD #2) IS BEYOND LEGAL RANGE'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(INT(W2).LT.INT(W1)) THEN
C       W2 LESS THAN OR EQUAL TO W1
                  OUTLYNE=
     1            'NUMERIC WORD #2 MAY NOT BE LESS THAN NUMERIC WORD #1'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              SFI=W3
              IF(SFI.EQ.0.0D0) SFI=1.0D0
              IF(MSG) THEN
                  OUTLYNE='GENERATING SURFACE CLEAR APERTURE PLOTTING DATA...'
                  CALL SHOWIT(1)
              END IF
              MDX=0.0D0
              MDY=0.0D0
              DO I=STASUR,STPSUR
                  IF(ALENS(127,I).NE.0.0D0) THEN
                      DO J=1,INT(ALENS(127,I))
                          MDX=MULTCLAP(J,1,I)
                          MDY=MULTCLAP(J,2,I)
                          GAMGAM=MULTCLAP(J,3,I)
                          CALL PLTCLP(1,I,SFI,MDX,MDY,GAMGAM)
                      END DO
                  ELSE
                      CALL PLTCLP(1,I,SFI,0.0D0,0.0D0,0.0D0)
                  END IF
                  IF(ALENS(127,I).NE.0.0D0) THEN
                      DO J=1,INT(ALENS(127,I))
                          MDX=MULTCLAP(J,1,I)
                          MDY=MULTCLAP(J,2,I)
                          GAMGAM=MULTCLAP(J,3,I)
                          CALL PLTCLP(2,I,SFI,MDX,MDY,GAMGAM)
                      END DO
                  ELSE
                      CALL PLTCLP(2,I,SFI,0.0D0,0.0D0,0.0D0)
                  END IF
              END DO
          END IF
C
C       COBS
          IF(WQ.EQ.'COBS') THEN
C       CHECK SYNTAX
              IF(SST.EQ.1) THEN
                  OUTLYNE=
     1            '"PLOT COBS" TAKES NO STRING INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
                  OUTLYNE=
     1            '"PLOT COBS" ONLY TAKES NUMERIC WORDS #1 AND #2 INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(STI.EQ.1) THEN
                  WRITE(OUTLYNE,801)
                  CALL SHOWIT(1)
 801              FORMAT('QUERRY (?) HAS NO MEANING WITH "PLOT COBS"')
                  RETURN
              ELSE
              END IF

              IF(DF1.EQ.0.AND.W1.LT.0.0D0) W1=SYSTEM1(20)+W1
              IF(DF2.EQ.0.AND.W2.LT.0.0D0) W2=SYSTEM1(20)+W2
C       DEFAULT VALUES
              IF(DF1.EQ.1) THEN
                  IF(DABS(ALENS(3,0)).GT.1.0D10) THEN
                      W1=DBLE(1)
                  ELSE
                      W1=DBLE(0)
                  END IF
              ELSE
C       DF1 NOT 1, W1 EXPLICITLY ENTERED
              END IF
              STASUR=INT(W1)
              IF(DF2.EQ.1) THEN
                  W2=DBLE(NEWIMG)
              ELSE
C       DF2 NOT 1, W2 EXPLICITLY ENTERED
              END IF
              STPSUR=INT(W2)
              IF(INT(W1).LT.0) THEN
C       INVALID NUMERIC WORD #1
                  OUTLYNE=
     1            'SURFACE NUMBER (NUMERIC WORD #1) IS BEYOND LEGAL RANGE'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(INT(W2).GT.NEWIMG) THEN
C       INVALID NUMERIC WORD #2
                  OUTLYNE=
     1            'SURFACE NUMBER (NUMERIC WORD #2) IS BEYOND LEGAL RANGE'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(INT(W2).LT.INT(W1)) THEN
C       W2 LESS THAN OR EQUAL TO W1
                  OUTLYNE=
     1            'NUMERIC WORD #2 MAY NOT BE LESS THAN NUMERIC WORD #1'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(MSG) THEN
                  OUTLYNE='GENERATING SURFACE OBSCURATION PLOTTING DATA...'
                  CALL SHOWIT(1)
              END IF
              MDX=0.0D0
              MDY=0.0D0
              DO I=STASUR,STPSUR
                  IF(ALENS(128,I).NE.0.0D0) THEN
                      DO J=1,INT(ALENS(128,I))
                          MDX=MULTCOBS(J,1,I)
                          MDY=MULTCOBS(J,2,I)
                          GAMGAM=MULTCOBS(J,3,I)
                          CALL PLTCOB(I,MDX,MDY,GAMGAM)
                      END DO
                  ELSE
                      CALL PLTCOB(I,0.0D0,0.0D0,0.0D0)
                  END IF
              END DO
          END IF
C
C       LEFT, RIGHT OR CENTER
          IF(WQ.EQ.'LEFT'.OR.WQ.EQ.'RIGHT'.OR.WQ.EQ.'CENTER') CALL PLTJUS
C
C       XSHIFT,YSHIFT,GAMMA
          IF(WQ.EQ.'XSHIFT'.OR.WQ.EQ.'YSHIFT'.OR.WQ.EQ.'GAMMA')
     1    CALL XYGAMA
C
C     PLTXFAN, PLTYFAN, PLTPFAN, PLTNFAN AND PLOTFANS
          IF(WC.EQ.'PLTPFAN'.OR.WC.EQ.'PLTXFAN'.OR.WC.EQ.'PLTXYFAN'
     1    .OR.WC.EQ.'PLTYFAN'.OR.WC.EQ.'PLTNFAN'.OR.WC.EQ.'PLTYXFAN'
     2    .OR.WC.EQ.'PLOTFANS') THEN
C     KILL OFF FANEXT UNLESS WC IS "PLOTFANS" AND WQ IS "GO"
              IF(WC.EQ.'PLOTFANS'.AND.WQ.EQ.'GO') THEN
C     LEAVE FANEXT ALONE
              ELSE
                  FANEXT=.FALSE.
              END IF
              OLDIF=LDIF
              LDIF=.FALSE.
              MSG=.FALSE.
              CALL PLFANS
C     NOW KILL OFF THE FANEXT
              FANEXT=.FALSE.
              LDIF=OLDIF
          ELSE
          END IF
C     PFANAXIS
          IF(WC.EQ.'PFANAXIS') THEN
              CALL PFANAXIS
          ELSE
          END IF
C     PFANLBL
          IF(WC.EQ.'PFANLBL') THEN
              CALL PFANLBL
          ELSE
          END IF
C     PFANCAP
          IF(WC.EQ.'PFANCAP') THEN
              CALL PFANCAP
          ELSE
          END IF
C     PFANSSI
          IF(WC.EQ.'PFANSSI') THEN
              CALL PFANSSI
          ELSE
          END IF
C     PFANCOMP
          IF(WC.EQ.'PFANCOMP') THEN
              CALL PFANCOMP
          ELSE
          END IF
C     ORIENT
          IF(WC.EQ.'ORIENT'.OR.WC.EQ.'NORIENT') THEN
              CALL ORIENT
          ELSE
          END IF
          RETURN
      END


C SUB STAMPER.FOR
C
      SUBROUTINE STAMPER
          IMPLICIT NONE
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          IF(STI.EQ.1) THEN
              IF(WC.EQ.'STAMPD') THEN
                  OUTLYNE=
     1            '"STAMPD" TURNS DATE STAMPING "ON" AND "OFF"'
                  CALL SHOWIT(1)
                  IF(STMPD) THEN
                      OUTLYNE=
     1                'DATE STAMPING IS CURRENTLY "ON"'
                      CALL SHOWIT(1)
                  END IF
                  IF(.NOT.STMPD) THEN
                      OUTLYNE=
     1                'DATE STAMPING IS CURRENTLY "OFF"'
                      CALL SHOWIT(1)
                  END IF
                  RETURN
              END IF
              IF(WC.EQ.'STAMPT') THEN
                  OUTLYNE=
     1            '"STAMPT" TURNS TIME STAMPING "ON" AND "OFF"'
                  CALL SHOWIT(1)
                  IF(STMPT) THEN
                      OUTLYNE=
     1                'TIME STAMPING IS CURRENTLY "ON"'
                      CALL SHOWIT(1)
                  END IF
                  IF(.NOT.STMPT) THEN
                      OUTLYNE=
     1                'TIME STAMPING IS CURRENTLY "OFF"'
                      CALL SHOWIT(1)
                  END IF
                  RETURN
              END IF
          END IF
C
          IF(SST.EQ.1.OR.SN.EQ.1.OR.WQ.NE.'ON'.AND.WQ.NE.'OFF') THEN
              IF(WC.EQ.'STAMPD') THEN
                  OUTLYNE=
     1            '"STAMPD" ONLY TAKES "ON" AND "OFF" QUALIFIER INPUT'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(WC.EQ.'STAMPT') THEN
                  OUTLYNE=
     1            '"STAMPT" ONLY TAKES "ON" AND "OFF" QUALIFIER INPUT'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
          IF(WC.EQ.'STAMPT'.AND.WQ.EQ.'ON') STMPT=.TRUE.
          IF(WC.EQ.'STAMPT'.AND.WQ.EQ.'OFF') STMPT=.FALSE.
          IF(WC.EQ.'STAMPD'.AND.WQ.EQ.'ON') STMPD=.TRUE.
          IF(WC.EQ.'STAMPD'.AND.WQ.EQ.'OFF') STMPD=.FALSE.
          RETURN
      END


C SUB PLTVIE.FOR
      SUBROUTINE PLTVIE
C
          IMPLICIT NONE
C
C       THIS ROUTINE DOES THE "PLOT VIEW" COMMAND AT THE CMD LEVEL
C
          REAL*8 FEE,ALF,RF,RA
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
C       PLOT VIEW
C
          IF(WC.EQ.'PLOT'.AND.WQ.EQ.'VIEW') THEN
C       CHECK SYNTAX
C
              IF(STI.EQ.1) THEN
                  RA=VIEALF
                  RF=VIEPHI
                  IF(DABS(RA).LT.1.0D-10) RA=0.0D0
                  IF(DABS(RF).LT.1.0D-10) RF=0.0D0
                  WRITE(OUTLYNE,100)
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,200)RA
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,300)RF
                  CALL SHOWIT(1)
                  RETURN
              END IF
C
              IF(SST.EQ.1) THEN
                  OUTLYNE=
     1            '"PLOT VIEW" TAKES NO STRING INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C
              IF(DF1.EQ.1.OR.DF2.EQ.1) THEN
                  OUTLYNE=
     1            '"PLOT VIEW" REQUIRES EXPLICIT NUMERIC WORD #1 AND #2 INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(S5.EQ.1) THEN
                  OUTLYNE=
     1            '"PLOT VIEW" TAKES NO NUMERIC WORD #5 INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(DF3.EQ.1) W3=0.0D0
              IF(DF4.EQ.1) W4=3500.0D0
              VIEALF=W1
              VIEPHI=W2
C       CALCULATE LOOKX,LOOKY AND LOOKZ
              ALF=(PII/180.0D0)*(VIEALF)
              FEE=(PII/180.0D0)*(VIEPHI)
              LOOKX=DCOS(ALF)*DSIN(FEE)
              LOOKY=DSIN(ALF)
              LOOKZ=DCOS(ALF)*DCOS(FEE)
          ELSE
C       NOT PLOT VIEW
          END IF
C
C       PLOT YESVIEW
C
          IF(WQ.EQ.'YESVIEW') THEN
C       CHECK SYNTAX
              IF(STI.EQ.0) THEN
                  IF(SN.EQ.1.OR.SST.EQ.1) THEN
                      OUTLYNE=
     1                '"PLOT YESVIEW" TAKES NO STRING OR NUMERIC INPUT'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
              DRAVUE=.TRUE.
              DRALOK=.FALSE.
              CALL DOVUE
          END IF
C
C       PLOT NOVIEW
C
          IF(WQ.EQ.'NOVIEW') THEN
C       CHECK SYNTAX
              IF(STI.EQ.0) THEN
                  IF(SN.EQ.1.OR.SST.EQ.1) THEN
                      OUTLYNE=
     1                '"PLOT NOVIEW" TAKES NO STRING OR NUMERIC INPUT'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
              DRAVUE=.FALSE.
              DRALOK=.FALSE.
          END IF
          RETURN
100       FORMAT('CURRENT "PLOT VIEW" VALUES ARE:')
200       FORMAT('  ELEVATION ANGLE (ALPHA) = ',D15.8)
300       FORMAT('      AZIMUTH ANGLE (PHI) = ',D15.8)
      END


C SUB PLTSZ.FOR
      SUBROUTINE PLTSZ
C
          IMPLICIT NONE
C
C       THIS ROUTINE DOES THE PLOT SCALE, PLOT NOSCALE AND PLOT YESCALE
C       COMMANDS AT THE CMD LEVEL
C
          CHARACTER PU1*11,PU2*6
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
C       SCALE FACTORS IN PLOTTING
C
C       PLOT SCALE
C
          IF(WQ.EQ.'SCALE') THEN
C       CHECK SYNTAX
C
              IF(S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
                  OUTLYNE=
     1            '"PLOT SCALE" TAKES NO NUMERIC WORD #3, #4 OR #5 INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(SST.EQ.1) THEN
                  OUTLYNE=
     1            '"PLOT SCALE" TAKES NO STRING INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(STI.EQ.0) THEN
                  IF(DF1.EQ.1.OR.DF2.EQ.1) THEN
                      OUTLYNE=
     1                '"PLOT SCALE REQUIRES EXPLICIT NUMERIC WORD #1 AND #2 INPUT'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
              IF(STI.EQ.1) THEN
                  IF(SYSTEM1(6).EQ.1.0D0) PU2='INCHES'
                  IF(SYSTEM1(6).EQ.2.0D0) PU1='CENTIMETERS'
                  IF(SYSTEM1(6).EQ.3.0D0) PU1='MILLIMETERS'
                  IF(SYSTEM1(6).EQ.4.0D0) PU2='METERS'
                  WRITE(OUTLYNE,189) PSIZXP
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,179) PSIZYP
                  CALL SHOWIT(1)
                  RETURN
              END IF

 179          FORMAT('Y-SCALE FACTOR = ',G15.8,1X,A11,
     1        ' OF FULL SIZE')
 189          FORMAT('X-SCALE FACTOR = ',G15.8,1X,A11,
     1        ' OF FULL SIZE')
              AUTSL=.FALSE.
C       CHECK FOR ZERO VALUES AND DIS-ALLOW
              IF(W1.LE.0.0D0.OR.W2.LE.0.0D0) THEN
                  OUTLYNE='"PLOT SCALE" REQUIRES NON-ZERO, POSTIVE INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              SCFAXP=1.0D0/W1
              SCFAYP=1.0D0/W2
              PSIZXP=W1
              PSIZYP=W2
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
              PLSZ=.TRUE.
              PLSC=.FALSE.
              RETURN
          ELSE
          END IF
C
C       PLOT NOSCALE
C
          IF(WQ.EQ.'NOSCALE') THEN
C       CHECK SYNTAX
              IF(SST.EQ.1.OR.SN.EQ.1) THEN
                  OUTLYNE=
     1            '"PLOT NOSCALE" TAKES NO NUMERIC OR STRING INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(STI.EQ.1) THEN
                  IF(DRASZZ) THEN
                      WRITE(OUTLYNE,379)
                      CALL SHOWIT(1)
                  ELSE
                      WRITE(OUTLYNE,479)
                      CALL SHOWIT(1)
                  END IF
              END IF
 379          FORMAT('SCALE FACTORS WILL BE SHOWN ON PLOTS')
 479          FORMAT('SCALE FACTORS WILL NOT BE SHOWN ON PLOTS')
              PLSC=.FALSE.
              DRASCL=.FALSE.
              DRASZZ=.FALSE.
              RETURN
          ELSE
          END IF
C
C       PLOT YESCALE
C
          IF(WC.EQ.'PLOT'.AND.WQ.EQ.'YESCALE') THEN
C       CHECK SYNTAX
              IF(SST.EQ.1.OR.SN.EQ.1) THEN
                  OUTLYNE=
     1            '"PLOT YESCALE" TAKES NO NUMERIC OR STRING INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(STI.EQ.1) THEN
                  IF(DRASZZ) THEN
                      WRITE(OUTLYNE,379)
                      CALL SHOWIT(1)
                  ELSE
                      WRITE(OUTLYNE,479)
                      CALL SHOWIT(1)
                  END IF
                  RETURN
              END IF
              PLSZ=.TRUE.
              DRASZZ=.TRUE.
              DRASCL=.FALSE.
              CALL DOSZ
              RETURN
          ELSE
          END IF
          RETURN
      END
C SUB PLTSTP.FOR
      SUBROUTINE PLTSTP
C
          IMPLICIT NONE
C
C       THIS ROUTINE DOES "PLOT END" AT THE CMD AND SPECT LEVELS
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
          IF(WC.EQ.'PLOT'.AND.WQ.EQ.'END') THEN
C       STOP PLOTTING
C       CHECK SYNTAX
              IF(SST.EQ.1.OR.SN.EQ.1) THEN
                  OUTLYNE='"PLOT END" TAKES NO STRING OR NUMERIC INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(DEVTYP.NE.1) THEN
                  OUTLYNE='WARNING: NO PLOTFILE EXISTS TO CLOSE'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              CALL PSTOP
          END IF
          RETURN
      END
C ALL INTERACTER/WINTERACTER CALLS HAVE BEEN MOVED TO ISS.FOR
C
C SUB PLTSTL.FOR
      SUBROUTINE PLTSTL
C
          IMPLICIT NONE
C
C       THIE ROUTINE DOES THE PLOT LSTYLE COMMAND AT THE CMD LEVEL
C
          CHARACTER LSTY*19
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
C       LSTYLE
C
          IF(WQ.EQ.'LSTYLE') THEN
C       CHECK SYNTAX
              IF(SST.EQ.1) THEN
                  OUTLYNE=
     1            '"PLOT LSTYLE" TAKES NO STRING INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
                  OUTLYNE=
     1            '"PLOT LSTYLE" ONLY TAKES NUMERIC WORD #1 INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(DF1.EQ.1.AND.STI.EQ.0) THEN
                  OUTLYNE=
     1            '"PLOT LSTYLE" REQUIRES EXPLICIT NUMERIC WORD #1 INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(STI.EQ.1) THEN
                  IF(LNTYPE.EQ.0) LSTY='TYPE 0'
                  IF(LNTYPE.EQ.1) LSTY='TYPE 1'
                  IF(LNTYPE.EQ.2) LSTY='TYPE 2'
                  IF(LNTYPE.EQ.3) LSTY='TYPE 3'
                  IF(LNTYPE.EQ.4) LSTY='TYPE 4'
                  IF(LNTYPE.EQ.5) LSTY='TYPE 5'
                  IF(LNTYPE.EQ.6) LSTY='TYPE 6'
                  IF(LNTYPE.EQ.7) LSTY='TYPE 7'
                  IF(LNTYPE.EQ.8) LSTY='TYPE 8'
                  IF(LNTYPE.EQ.8) LSTY='TYPE 9'
                  WRITE(OUTLYNE,875) LSTY,LNTYPE
                  CALL SHOWIT(1)
 875              FORMAT('CURRENT LINE STYLE IS ',A19,1X,
     1            'LINE STYLE NUMBER = ',I1)
                  RETURN
              END IF
              IF(INT(W1).LT.0.OR.INT(W1).GT.9) THEN
                  OUTLYNE=
     1            'VALID LINE STYLE NUMBERS RANGE FROM 0 TO 9'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              LNTYPE=INT(W1)
              RETURN
          END IF
          RETURN
      END


C SUB PLTSPD.FOR
      SUBROUTINE PLTSPD
C
          IMPLICIT NONE
C
C       THIS PROGRAM CONTROLS THE "PLTSPD" COMMAND
C
          REAL*8 VIEROT
C
          INTEGER DFLAG,VIEXOF,VIEYOF
C
          COMMON/OFFVIE/VIEXOF,VIEYOF,VIEROT
C
!      INTEGER VS1,VS2,VS3
C
          LOGICAL EXIS32
C
!      CHARACTER VIEWQ*8
C
!      REAL*8 VIEW1,VIEW2,VIEW3
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datspd.inc'
          INCLUDE 'datsp1.inc'
          INCLUDE 'datlen.inc'
C
          IF(STI.EQ.1) THEN
              OUTLYNE= '"PLTSPD" PLOTS THE CURRENT SPOT DIAGRAM'
              CALL SHOWIT(1)
              IF(WQ.NE.'ZERO') THEN
                  IF(SPDCHIEF)
     1            OUTLYNE= 'CURRENT CENTERING IS AT THE CHIEF RAY'
                  IF(SPDCENT)
     1            OUTLYNE= 'CURRENT CENTERING IS AT THE SPOT CENTROID'
                  CALL SHOWIT(1)
              ELSE
                  OUTLYNE= 'CURRENT CENTERING IS AT THE X=0, Y=0'
                  CALL SHOWIT(1)
              END IF
              OUTLYNE= 'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SQ.EQ.1.AND.WQ.NE.'CENT'.AND.WQ.NE.'CHIEF'.AND.WQ.NE.
     1    'SUM'.AND.WQ.NE.'ZERO') THEN
              OUTLYNE= 'INVALID QUALIFIER USED WITH "PLTSPD"'
              CALL SHOWIT(1)
              OUTLYNE= 'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SST.EQ.1) THEN
              OUTLYNE= '"PLTSPD" TAKES NO STRING INPUT'
              CALL SHOWIT(1)
              OUTLYNE= 'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
              OUTLYNE= '"PLTSPD" TAKES NO NUMERIC WORD #2 THROUGH #5 INPUT'
              CALL SHOWIT(1)
              OUTLYNE= 'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(DF1.EQ.1.OR.DF1.EQ.0.AND.W1.EQ.0.0D0) DFLAG=0
          IF(DF1.EQ.0.AND.W1.NE.0.0D0) DFLAG=1
C
C     QUALIFIER WORDS:
C         CENT
C         CHIEF
C         SUM
C
C
          IF(SQ.EQ.0) THEN
              WQ='CHIEF   '
              SPDCHIEF=.TRUE.
              SPDCENT=.FALSE.
          END IF
          IF(SQ.EQ.1.AND.WQ.EQ.'CHIEF') THEN
              SPDCHIEF=.TRUE.
              SPDCENT=.FALSE.
          END IF
          IF(SQ.EQ.1.AND.WQ.EQ.'CENT'.OR.
     1    SQ.EQ.1.AND.WQ.EQ.'SUM') THEN
              SPDCHIEF=.FALSE.
              SPDCENT=.TRUE.
          END IF
          IF(WQ.EQ.'CENT'.OR.WQ.EQ.'CHIEF'.OR.WQ.EQ.'ZERO') THEN
              IF(.NOT.SPDEXT) THEN
                  OUTLYNE= 'NO SPOT DIAGRAM EXISTS TO BE PLOTTED'
                  CALL SHOWIT(1)
                  OUTLYNE= 'NO ACTION TAKEN'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
          IF(WQ.EQ.'SUM') THEN
              EXIS32=.FALSE.
              INQUIRE(FILE=LIBSPO//'SPOTS.DAT',EXIST=EXIS32)
              IF(.NOT.EXIS32) THEN
                  OUTLYNE= 'NO SUMMED SPOT DIAGRAM EXISTS TO BE PLOTTED'
                  CALL SHOWIT(1)
                  OUTLYNE= 'NO ACTION TAKEN'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
C     DO A PLOT NEW
          DEVTYP=1
          LOOKY=0.0D0
          LOOKX=-1.0D0
          LOOKZ=0.0D0
          CALL PLTDEV
          GRASET=.TRUE.
          CALL PLOTSPD
          call setonecolors

          IF(DFLAG.EQ.0) THEN
              SAVE_KDP(1)=SAVEINPT(1)
              INPUT='DRAW'
              CALL PROCES
              REST_KDP(1)=RESTINPT(1)
          END IF
          RETURN
      END


C SUB PLTSCL.FOR
      SUBROUTINE PLTSCL
C
          IMPLICIT NONE
C
C       THIS ROUTINE DOES THE PLOT SIZE, PLOT NOSIZE AND PLOT YESIZE
C       COMMANDS AT THE CMD LEVEL
C
          CHARACTER PU1*11,PU2*6
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
C       SIZE FACTORS IN PLOTTING
C
C       PLOT SIZE
C
          IF(WQ.EQ.'SIZE') THEN
C       CHECK SYNTAX
C
              IF(S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
                  OUTLYNE=
     1            '"PLOT SIZE" TAKES NO NUMERIC WORD #3, #4 OR #5 INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(SST.EQ.1) THEN
                  OUTLYNE=
     1            '"PLOT SIZE" TAKES NO STRING INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(STI.EQ.0) THEN
                  IF(DF1.EQ.1.OR.DF2.EQ.1) THEN
                      OUTLYNE=
     1                '"PLOT SIZE" REQUIRES EXPLICIT NUMERIC WORD #1 AND #2 INPUT'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
              IF(STI.EQ.1) THEN
                  IF(SYSTEM1(6).EQ.1.0D0) PU2='INCHES'
                  IF(SYSTEM1(6).EQ.2.0D0) PU1='CENTIMETERS'
                  IF(SYSTEM1(6).EQ.3.0D0) PU1='MILLIMETERS'
                  IF(SYSTEM1(6).EQ.4.0D0) PU2='METERS'
                  WRITE(OUTLYNE,189) SCFAXP,PU2
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,179) SCFAYP,PU2
                  CALL SHOWIT(1)
                  RETURN
              END IF

 179          FORMAT('Y-SIZE FACTOR = ',G15.8,1X,A11,
     1        ' PER PLOTTED INCH')
 189          FORMAT('X-SIZE FACTOR = ',G15.8,1X,A11,
     1        ' PER PLOTTED INCH')
              AUTSL=.FALSE.
C       CHECK FOR ZERO VALUES AND DIS-ALLOW
              IF(W1.LE.0.0D0.OR.W2.LE.0.0D0) THEN
                  OUTLYNE='"PLOT SIZE" REQUIRES NON-ZERO, POSTIVE INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              SCFAXP=W1
              SCFAYP=W2
              PSIZXP=1.0D0/W1
              PSIZYP=1.0D0/W2
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
              PLSC=.TRUE.
              PLSZ=.FALSE.
              CALL DOSZ
              RETURN
          ELSE
          END IF
C
C       PLOT NOSIZE
C
          IF(WQ.EQ.'NOSIZE') THEN
C       CHECK SYNTAX
              IF(SST.EQ.1.OR.SN.EQ.1) THEN
                  OUTLYNE=
     1            '"PLOT NOSIZE" TAKES NO NUMERIC OR STRING INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(STI.EQ.1) THEN
                  IF(DRASCL) THEN
                      WRITE(OUTLYNE,379)
                      CALL SHOWIT(1)
                  ELSE
                      WRITE(OUTLYNE,479)
                      CALL SHOWIT(1)
                  END IF
              END IF
 379          FORMAT('SIZE FACTORS WILL BE SHOWN ON PLOTS')
 479          FORMAT('SIZE FACTORS WILL NOT BE SHOWN ON PLOTS')
              DRASCL=.FALSE.
              DRASZZ=.FALSE.
              RETURN
          ELSE
          END IF
C
C       PLOT YESIZE
C
          IF(WC.EQ.'PLOT'.AND.WQ.EQ.'YESIZE') THEN
C       CHECK SYNTAX
              IF(SST.EQ.1.OR.SN.EQ.1) THEN
                  OUTLYNE=
     1            '"PLOT YESIZE" TAKES NO NUMERIC OR STRING INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(STI.EQ.1) THEN
                  IF(DRASCL) THEN
                      WRITE(OUTLYNE,379)
                      CALL SHOWIT(1)
                  ELSE
                      WRITE(OUTLYNE,479)
                      CALL SHOWIT(1)
                  END IF
                  RETURN
              END IF
              DRASCL=.TRUE.
              CALL DOSC
              DRASZZ=.FALSE.
              RETURN
          END IF
          RETURN
      END


C SUB PLTSC5.FOR
      SUBROUTINE PLTSC5(XMINI,XMAXI,YMINI,YMAXI,JJ
     1,CLPDAT,M1,M2,M3,M4,M5)
C
          IMPLICIT NONE
C
C       THIS ROUTINE DOES THE AUTO SCALE FACTOR FOR THE
C       PLOT FOOT COMMAND
C
          REAL*8 ROT1X,ROT1Z,ROT2Y,
     1    ROT2Z,AX,AY,AZ,AALF,APHI,YMAXI,YMINI,XMAXI,XMINI,MAXI,MINI
     2    ,VIEPH,VIEAL,X1,X2,X3,X4,Y1,Y2,Y3,Y4,TMINI,TMAXI,TMAXIX,TMAXIY
     3    ,XMINI1,XMINI2,YMINI1,YMINI2,XMAXI1,XMAXI2,YMAXI1,YMAXI2
C
          INTEGER I,ORSF,SFNUMFT,JJ,M1,M2,M3,M4,M5
C
          REAL CLPDAT
          DIMENSION CLPDAT(M1:M2,M3,M1:M4,M5)
C
          COMMON/FOOTNUM/SFNUMFT
C
          COMMON/SFOR/ORSF
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
          VIEPH=(PII/180.0D0)*VIEPHI
          VIEAL=(PII/180.0D0)*VIEALF
C
          JJ=1
C
C     AFTER THIS TRANSFORMATION, THE WORLD X COORDINATE IS PLOTTED IN THE
C     HORIZONTAL X-AXIS OF THE DISPLAY AND THE Y WORLD COORDINATE IS PLOTTED
C     IN THE VERTICAL Y-AXIS OF THE DISPLAY AFTER A CONVERSION TO
C     DEVICE INDEPENDENT COORDINATES ARE MADE.
C
C     CASE OF SFNUMFT
C     KEY OFF CLAP VALUES
          IF(AUTSL) THEN
C     AUTOSCALING IS THE DEFAULT, DOES IT NEED TO BE CALCULATED ?
              IF(.NOT.ASCFLG) THEN
                  I=SFNUMFT
                  X1=CLPDAT(0,1,I,JJ)
                  Y1=CLPDAT(0,2,I,JJ)
                  X2=CLPDAT(180,1,I,JJ)
                  Y2=CLPDAT(180,2,I,JJ)
                  X3=CLPDAT(90,1,I,JJ)
                  Y3=CLPDAT(90,2,I,JJ)
                  X4=CLPDAT(270,1,I,JJ)
                  Y4=CLPDAT(270,2,I,JJ)
                  IF(X1.LT.X2) THEN
                      XMINI1=X1
                      XMAXI1=X2
                  ELSE
                      XMINI1=X2
                      XMAXI1=X1
                  END IF
                  IF(Y1.LT.Y2) THEN
                      YMINI1=Y1
                      YMAXI1=Y2
                  ELSE
                      YMINI1=Y2
                      YMAXI1=Y1
                  END IF
C
                  IF(X3.LT.X4) THEN
                      XMINI2=X3
                      XMAXI2=X4
                  ELSE
                      XMINI2=Y4
                      XMAXI2=Y3
                  END IF
                  IF(Y3.LT.Y4) THEN
                      YMINI2=Y3
                      YMAXI2=Y4
                  ELSE
                      YMINI2=Y4
                      YMAXI2=Y3
                  END IF
                  IF(XMINI1.LT.XMINI2) THEN
                      XMINI=XMINI1
                  ELSE
                      XMINI=XMINI2
                  END IF
                  IF(XMAXI1.LT.XMAXI2) THEN
                      XMAXI=XMAXI1
                  ELSE
                      XMAXI=XMAXI2
                  END IF
                  IF(YMINI1.LT.YMINI2) THEN
                      YMINI=YMINI1
                  ELSE
                      YMINI=YMINI2
                  END IF
                  IF(YMAXI1.LT.YMAXI2) THEN
                      YMAXI=YMAXI1
                  ELSE
                      YMAXI=YMAXI2
                  END IF
C     THE XMAXI AND XMINI AND YMAXI AND YMINI VALUES HAVE BEEN CALCULATED
C     CALCULATE THE AUTOMATIC SCALE FACTOR AND REMEMBER IT.
                  IF((DABS(XMAXI-XMINI)).GE.(DABS(YMAXI-YMINI))) THEN
                      MAXI=XMAXI
                      MINI=XMINI
                  ELSE
                      MAXI=YMAXI
                      MINI=YMINI
                  END IF
                  IF(MAXI.EQ.0.0D0.AND.MINI.EQ.0.0D0) THEN
                      DO I=STASUR,STPSUR
                          IF(ALENS(9,I).EQ.0.0D0.OR.ALENS(9,I).EQ.5.0D0) THEN
                              TMAXIX=DABS(PXTRAX(1,I))+DABS(PXTRAX(5,I))
                              TMAXIY=DABS(PXTRAY(1,I))+DABS(PXTRAY(5,I))
                              TMAXI=TMAXIX
                              IF(TMAXIY.GT.TMAXIX) TMAXI=TMAXIY
                          ELSE
                              IF(ALENS(9,I).EQ.1.0D0) THEN
                                  TMAXIY=DABS(ALENS(10,I))+DABS(ALENS(12,I))
                                  TMAXIX=DABS(ALENS(10,I))+DABS(ALENS(13,I))
                                  TMAXI=TMAXIX
                                  IF(TMAXIY.GT.TMAXIX) TMAXI=TMAXIY
                                  TMINI=-TMAXI
                                  IF(TMAXI.GT.MAXI) MAXI=TMAXI
                                  IF(TMINI.LT.MAXI) MINI=TMINI
                              END IF
                              IF(ALENS(9,I).EQ.6.0D0) THEN
                                  TMAXIY=DABS(ALENS(11,I))+DABS(ALENS(12,I))
                                  TMAXIX=DABS(ALENS(11,I))+DABS(ALENS(13,I))
                                  TMAXI=TMAXIX
                                  IF(TMAXIY.GT.TMAXIX) TMAXI=TMAXIY
                                  TMINI=-TMAXI
                                  IF(TMAXI.GT.MAXI) MAXI=TMAXI
                                  IF(TMINI.LT.MAXI) MINI=TMINI
                              END IF
                              IF(ALENS(9,I).GE.2.0D0.AND.ALENS(9,I).LE.4.0D0) THEN
                                  TMAXIY=DABS(ALENS(10,I))+DABS(ALENS(12,I))
                                  TMAXIX=DABS(ALENS(11,I))+DABS(ALENS(13,I))
                                  TMAXI=TMAXIX
                                  IF(TMAXIY.GT.TMAXIX) TMAXI=TMAXIY
                                  TMINI=-TMAXI
                                  IF(TMAXI.GT.MAXI) MAXI=TMAXI
                                  IF(TMINI.LT.MAXI) MINI=TMINI
                              END IF
                          END IF
                      END DO
                  END IF
C     WE NOW HAVE THE MAXI AND MINI FOR THE SCALE FACTOR CALCULATION
C     NOW CALCULATE THE AUTO SCALE FACTOR AND SET APPROPRIATE FLAGS
C
                  SCFAY=DABS(MAXI-MINI)/6.0D0
                  SCFAX=SCFAY
                  PSIZY=1.0D0/SCFAY
                  PSIZX=1.0D0/SCFAX
                  IF(SYSTEM1(6).EQ.1.0D0) SCFAYP=SCFAY
                  IF(SYSTEM1(6).EQ.1.0D0) SCFAXP=SCFAX
                  IF(SYSTEM1(6).EQ.2.0D0) SCFAYP=SCFAY/2.54D0
                  IF(SYSTEM1(6).EQ.2.0D0) SCFAXP=SCFAX/2.54D0
                  IF(SYSTEM1(6).EQ.3.0D0) SCFAYP=SCFAY/25.4D0
                  IF(SYSTEM1(6).EQ.3.0D0) SCFAXP=SCFAX/25.4D0
                  IF(SYSTEM1(6).EQ.4.0D0) SCFAYP=SCFAY/0.0254
                  IF(SYSTEM1(6).EQ.4.0D0) SCFAXP=SCFAX/0.0254
                  PSIZYP=1.0D0/SCFAYP
                  PSIZXP=1.0D0/SCFAXP
                  IF(SCFAY.EQ.0.0D0.OR.SCFAX.EQ.0.0D0) THEN
                      OUTLYNE=
     1                'ERROR: AUTOMATIC SCALE FACTOR IS ZERO'
                      CALL SHOWIT(1)
                      OUTLYNE=
     1                'EXPLICITLY ISSUE A "PLOT SCALE" OR "PLOT SIZE" COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
C     DON'T DO ANOTHER PLOT SCALE
                  AUTSL=.FALSE.
C     AUTOSCALE HAS BEEN CALCULATED
                  ASCFLG=.TRUE.
              ELSE
C     AUTO SCALE FACTOR HAS ALREADY BEEN CALCULATED, KEEP GOING
              END IF
          ELSE
C     A NON-AUTO SCALE OR SIZE FACTOR EXISTS, USE IT
C     NOW DO THE PLOT RANGE
              IF(RCL.EQ.1.OR.RCL.EQ.2.OR.RCL.EQ.3) THEN
C     DO THIS TO CALCULATE RANGE FOR PLOT JUSTIFICATION
                  I=SFNUMFT
                  X1=CLPDAT(0,1,I,JJ)
                  Y1=CLPDAT(0,2,I,JJ)
                  X2=CLPDAT(180,1,I,JJ)
                  Y2=CLPDAT(180,2,I,JJ)
                  X3=CLPDAT(90,1,I,JJ)
                  Y3=CLPDAT(90,2,I,JJ)
                  X4=CLPDAT(270,1,I,JJ)
                  Y4=CLPDAT(270,2,I,JJ)
                  IF(X1.LT.X2) THEN
                      XMINI1=X1
                      XMAXI1=X2
                  ELSE
                      XMINI1=X2
                      XMAXI1=X1
                  END IF
                  IF(Y1.LT.Y2) THEN
                      YMINI1=Y1
                      YMAXI1=Y2
                  ELSE
                      YMINI1=Y2
                      YMAXI1=Y1
                  END IF
C
                  IF(X3.LT.X4) THEN
                      XMINI2=X3
                      XMAXI2=X4
                  ELSE
                      XMINI2=Y4
                      XMAXI2=Y3
                  END IF
                  IF(Y3.LT.Y4) THEN
                      YMINI2=Y3
                      YMAXI2=Y4
                  ELSE
                      YMINI2=Y4
                      YMAXI2=Y3
                  END IF
                  IF(XMINI1.LT.XMINI2) THEN
                      XMINI=XMINI1
                  ELSE
                      XMINI=XMINI2
                  END IF
                  IF(XMAXI1.LT.XMAXI2) THEN
                      XMAXI=XMAXI1
                  ELSE
                      XMAXI=XMAXI2
                  END IF
                  IF(YMINI1.LT.YMINI2) THEN
                      YMINI=YMINI1
                  ELSE
                      YMINI=YMINI2
                  END IF
                  IF(YMAXI1.LT.YMAXI2) THEN
                      YMAXI=YMAXI1
                  ELSE
                      YMAXI=YMAXI2
                  END IF
              ELSE
C     RANGE IS OK AS IT IS
              END IF
          END IF
C
          RETURN
      END
C SUB PLTSC4.FOR
      SUBROUTINE PLTSC4(XMINI,XMAXI,YMINI,YMAXI,JJ
     1,CLPDAT,M1,M2,M3,M4,M5)
C
          IMPLICIT NONE
C
C       THIS ROUTINE DOES THE AUTO SCALE FACTOR FOR THE
C       PLOT COBS COMMAND
C
          REAL*8 X,Y,Z,XN,YN,ZN,ROT1X,ROT1Z,ROT2Y,
     1    ROT2Z,AX,AY,AZ,AALF,APHI,YMAXI,YMINI,XMAXI,XMINI,MAXI,MINI
     2    ,VIEPH,VIEAL,X1,X2,X3,X4,Y1,Y2,Y3,Y4,TMAXI,TMINI,TMAXIX,TMAXIY
     3    ,XMINI1,XMINI2,YMINI1,YMINI2,XMAXI1,XMAXI2,YMAXI1,YMAXI2
C
          INTEGER JJ,I,M1,M2,M3,M4,M5
C
          REAL CLPDAT
          DIMENSION CLPDAT(M1:M2,M3,M1:M4,M5)
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
          VIEPH=(PII/180.0D0)*VIEPHI
          VIEAL=(PII/180.0D0)*VIEALF
C
C     AFTER THIS TRANSFORMATION, THE WORLD X COORDINATE IS PLOTTED IN THE
C     HORIZONTAL X-AXIS OF THE DISPLAY AND THE Y WORLD COORDINATE IS PLOTTED
C     IN THE VERTICAL Y-AXIS OF THE DISPLAY AFTER A CONVERSION TO
C     DEVICE INDEPENDENT COORDINATES ARE MADE.
C
          IF(STASUR.NE.STPSUR) THEN
C     FIRST CHECK THE SCALING
              IF(AUTSL) THEN
C     AUTOSCALING IS THE DEFAULT, DOES IT NEED TO BE CALCULATED ?
                  IF(.NOT.ASCFLG) THEN
C     AUTOSCALE FACTOR HAS NOT BEEN CALCULATED
C     PROCEED
C     CALCULATE AN AUTO SCALE FACTOR BASED ON THE CURRENTLY
C     REQUESTED RAY PLOTTING DATA SO NO DATA RANGE EXCEEDS
C     6000 DEVICE INDEPENDENT UNITS. THIS KEEPS A GAMMA ROTATION
C     AT THE SCREEN FROM THE FIFTH NUMERIC WORD OF PLOT LOOK OR
C     PLOT VIEW FROM FORCING VALUES OUTSIDE THE PLOTTING WINDOW.
C     DETERMINE THE STARTING AND ENDING WORLD COORDINATES IN THE
C     SCREEN X AND Y DIRECTIONS.
C     WE ARE PLOTTING FROM STASUR TO STPSUR. CALCULATE THE MAX
C     Y AND Z CCORDINATES OF THE VERTEX AFTER APPLYING THE LOOK/VIEW
C     TRANSFORMATIONS. FIND THE LARGEST AND SMALLEST (ALGEBRAICALLY)
C     OF THESE VALUES.
                      DO I=STASUR,STPSUR
C
                          X=VERTEX(1,I)
                          Y=VERTEX(2,I)
                          Z=VERTEX(3,I)
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
                          X=XN
                          Y=YN
                          Z=ZN
                          IF(I.EQ.STASUR) THEN
                              YMAXI=Y
                              YMINI=Y
                              XMAXI=X
                              XMINI=X
                          ELSE
C     NOT FIRST CYCLE, DO SOME TESTING
                              IF(Y.GT.YMAXI) YMAXI=Y
                              IF(Y.LT.YMINI) YMINI=Y
                              IF(X.GT.XMAXI) XMAXI=X
                              IF(X.LT.XMINI) XMINI=X
C                        DONE WITH THE TESTING
C
                          END IF
                      END DO
C     THE XMAXI AND XMINI AND YMAXI AND YMINI VALUES HAVE BEEN CALCULATED
C     CALCULATE THE AUTOMATIC SCALE FACTOR AND REMEMBER IT.
                      IF((DABS(XMAXI-XMINI)).GE.(DABS(YMAXI-YMINI))) THEN
                          MAXI=XMAXI
                          MINI=XMINI
                      ELSE
                          MAXI=YMAXI
                          MINI=YMINI
                      END IF
                      IF(MAXI.EQ.0.0D0.AND.MINI.EQ.0.0D0) THEN
                          DO I=STASUR,STPSUR
                              IF(ALENS(9,I).EQ.0.0D0.OR.ALENS(9,I).EQ.5.0D0) THEN
                                  TMAXIX=DABS(PXTRAX(1,I))+DABS(PXTRAX(5,I))
                                  TMAXIY=DABS(PXTRAY(1,I))+DABS(PXTRAY(5,I))
                                  TMAXI=TMAXIX
                                  IF(TMAXIY.GT.TMAXIX) TMAXI=TMAXIY
                              ELSE
                                  IF(ALENS(9,I).EQ.1.0D0) THEN
                                      TMAXIY=DABS(ALENS(10,I))+DABS(ALENS(12,I))
                                      TMAXIX=DABS(ALENS(10,I))+DABS(ALENS(13,I))
                                      TMAXI=TMAXIX
                                      IF(TMAXIY.GT.TMAXIX) TMAXI=TMAXIY
                                      TMINI=-TMAXI
                                      IF(TMAXI.GT.MAXI) MAXI=TMAXI
                                      IF(TMINI.LT.MAXI) MINI=TMINI
                                  END IF
                                  IF(ALENS(9,I).EQ.6.0D0) THEN
                                      TMAXIY=DABS(ALENS(11,I))+DABS(ALENS(12,I))
                                      TMAXIX=DABS(ALENS(11,I))+DABS(ALENS(13,I))
                                      TMAXI=TMAXIX
                                      IF(TMAXIY.GT.TMAXIX) TMAXI=TMAXIY
                                      TMINI=-TMAXI
                                      IF(TMAXI.GT.MAXI) MAXI=TMAXI
                                      IF(TMINI.LT.MAXI) MINI=TMINI
                                  END IF
                                  IF(ALENS(9,I).GE.2.0D0.AND.ALENS(9,I).LE.4.0D0) THEN
                                      TMAXIY=DABS(ALENS(10,I))+DABS(ALENS(12,I))
                                      TMAXIX=DABS(ALENS(11,I))+DABS(ALENS(13,I))
                                      TMAXI=TMAXIX
                                      IF(TMAXIY.GT.TMAXIX) TMAXI=TMAXIY
                                      TMINI=-TMAXI
                                      IF(TMAXI.GT.MAXI) MAXI=TMAXI
                                      IF(TMINI.LT.MAXI) MINI=TMINI
                                  END IF
                              END IF
                          END DO
                      END IF
C     WE NOW HAVE THE MAXI AND MINI FOR THE SCALE FACTOR CALCULATION
C     NOW CALCULATE THE AUTO SCALE FACTOR AND SET APPROPRIATE FLAGS
                      IF(MAXI.EQ.0.0D0.AND.MINI.EQ.0.0D0) THEN
                          DO I=STASUR,STPSUR
                              TMAXI=DABS(ALENS(10,I))
                              TMINI=-DABS(ALENS(10,I))
                              IF(TMAXI.GT.MAXI) MAXI=TMAXI
                              IF(TMINI.LT.MAXI) MINI=TMINI
                          END DO
                      END IF
                      IF(MAXI.EQ.0.0D0.AND.MINI.EQ.0.0D0) THEN
                          DO I=STASUR,STPSUR
                              TMAXI=DABS(PXTRAY(1,I))+DABS(PXTRAY(5,I))
                              TMINI=-TMAXI
                              IF(TMAXI.GT.MAXI) MAXI=TMAXI
                              IF(TMINI.LT.MAXI) MINI=TMINI
                          END DO
                      END IF
C
                      SCFAY=DABS(MAXI-MINI)/6.0D0
                      SCFAX=DABS(MAXI-MINI)/6.0D0
                      PSIZY=1.0D0/SCFAY
                      PSIZX=1.0D0/SCFAX
                      IF(SYSTEM1(6).EQ.1.0D0) SCFAYP=SCFAY
                      IF(SYSTEM1(6).EQ.1.0D0) SCFAXP=SCFAX
                      IF(SYSTEM1(6).EQ.2.0D0) SCFAYP=SCFAY/2.54D0
                      IF(SYSTEM1(6).EQ.2.0D0) SCFAXP=SCFAX/2.54D0
                      IF(SYSTEM1(6).EQ.3.0D0) SCFAYP=SCFAY/25.4D0
                      IF(SYSTEM1(6).EQ.3.0D0) SCFAXP=SCFAX/25.4D0
                      IF(SYSTEM1(6).EQ.4.0D0) SCFAYP=SCFAY/0.0254D0
                      IF(SYSTEM1(6).EQ.4.0D0) SCFAXP=SCFAX/0.0254D0
                      PSIZYP=1.0D0/SCFAYP
                      PSIZXP=1.0D0/SCFAXP
                      IF(SCFAY.EQ.0.0D0.OR.SCFAX.EQ.0.0D0) THEN
                          OUTLYNE=
     1                    'ERROR: AUTOMATIC SCALE FACTOR IS ZERO'
                          CALL SHOWIT(1)
                          OUTLYNE=
     1                    'EXPLICITLY ISSUE A "PLOT SCALE" OR "PLOT SIZE" COMMAND'
                          CALL SHOWIT(1)
                          CALL MACFAL
                          RETURN
                      END IF
C     DON'T DO ANOTHER PLOT SCALE
                      AUTSL=.FALSE.
C     AUTOSCALE HAS BEEN CALCULATED
                      ASCFLG=.TRUE.
                  ELSE
C     AUTO SCALE FACTOR HAS ALREADY BEEN CALCULATED, KEEP GOING
                  END IF
              ELSE
C     A NON-AUTO SCALE OR SIZE FACTOR EXISTS, USE IT
                  IF(RCL.EQ.1.OR.RCL.EQ.2.OR.RCL.EQ.3) THEN
C     DO THIS TO CALCULATE RANGE FOR PLOT JUSTIFICATION
                      DO I=STASUR,STPSUR
                          X=VERTEX(1,I)
                          Y=VERTEX(2,I)
                          Z=VERTEX(3,I)
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
                          X=XN
                          Y=YN
                          Z=ZN
                          IF(I.EQ.STASUR) THEN
                              YMAXI=Y
                              YMINI=Y
                              XMAXI=X
                              XMINI=X
                          ELSE
C     NOT FIRST CYCLE, DO SOME TESTING
                              IF(Y.GT.YMAXI) YMAXI=Y
                              IF(Y.LT.YMINI) YMINI=Y
                              IF(X.GT.XMAXI) XMAXI=X
                              IF(X.LT.XMINI) XMINI=X
C                        DONE WITH THE TESTING
C
                          END IF
                      END DO
                  ELSE
C     DON'T NEED TO CALCULATE VALUES FOR RANGE
                  END IF
              END IF
          ELSE
C     CASE OF STASUR=STPSUR
              STASUR=STPSUR
C     STASUR=STPSUR, KEY OFF COBS VALUES
              IF(AUTSL) THEN
C     AUTOSCALING IS THE DEFAULT, DOES IT NEED TO BE CALCULATED ?
                  IF(.NOT.ASCFLG) THEN
                      I=STASUR
                      X1=CLPDAT(0,1,I,JJ)
                      Y1=CLPDAT(0,2,I,JJ)
                      X2=CLPDAT(180,1,I,JJ)
                      Y2=CLPDAT(180,2,I,JJ)
                      X3=CLPDAT(90,1,I,JJ)
                      Y3=CLPDAT(90,2,I,JJ)
                      X4=CLPDAT(270,1,I,JJ)
                      Y4=CLPDAT(270,2,I,JJ)
                      IF(X1.LT.X2) THEN
                          XMINI1=X1
                          XMAXI1=X2
                      ELSE
                          XMINI1=X2
                          XMAXI1=X1
                      END IF
                      IF(Y1.LT.Y2) THEN
                          YMINI1=Y1
                          YMAXI1=Y2
                      ELSE
                          YMINI1=Y2
                          YMAXI1=Y1
                      END IF
C
                      IF(X3.LT.X4) THEN
                          XMINI2=X3
                          XMAXI2=X4
                      ELSE
                          XMINI2=Y4
                          XMAXI2=Y3
                      END IF
                      IF(Y3.LT.Y4) THEN
                          YMINI2=Y3
                          YMAXI2=Y4
                      ELSE
                          YMINI2=Y4
                          YMAXI2=Y3
                      END IF
                      IF(XMINI1.LT.XMINI2) THEN
                          XMINI=XMINI1
                      ELSE
                          XMINI=XMINI2
                      END IF
                      IF(XMAXI1.LT.XMAXI2) THEN
                          XMAXI=XMAXI1
                      ELSE
                          XMAXI=XMAXI2
                      END IF
                      IF(YMINI1.LT.YMINI2) THEN
                          YMINI=YMINI1
                      ELSE
                          YMINI=YMINI2
                      END IF
                      IF(YMAXI1.LT.YMAXI2) THEN
                          YMAXI=YMAXI1
                      ELSE
                          YMAXI=YMAXI2
                      END IF
C     THE XMAXI AND XMINI AND YMAXI AND YMINI VALUES HAVE BEEN CALCULATED
C     CALCULATE THE AUTOMATIC SCALE FACTOR AND REMEMBER IT.
                      IF((DABS(XMAXI-XMINI)).GE.(DABS(YMAXI-YMINI))) THEN
                          MAXI=XMAXI
                          MINI=XMINI
                      ELSE
                          MAXI=YMAXI
                          MINI=YMINI
                      END IF
                      IF(MAXI.EQ.0.0D0.AND.MINI.EQ.0.0D0) THEN
                          DO I=STASUR,STPSUR
                              IF(ALENS(9,I).EQ.0.0D0.OR.ALENS(9,I).EQ.5.0D0) THEN
                                  TMAXIX=DABS(PXTRAX(1,I))+DABS(PXTRAX(5,I))
                                  TMAXIY=DABS(PXTRAY(1,I))+DABS(PXTRAY(5,I))
                                  TMAXI=TMAXIX
                                  IF(TMAXIY.GT.TMAXIX) TMAXI=TMAXIY
                              ELSE
                                  IF(ALENS(9,I).EQ.1.0D0) THEN
                                      TMAXIY=DABS(ALENS(10,I))+DABS(ALENS(12,I))
                                      TMAXIX=DABS(ALENS(10,I))+DABS(ALENS(13,I))
                                      TMAXI=TMAXIX
                                      IF(TMAXIY.GT.TMAXIX) TMAXI=TMAXIY
                                      TMINI=-TMAXI
                                      IF(TMAXI.GT.MAXI) MAXI=TMAXI
                                      IF(TMINI.LT.MAXI) MINI=TMINI
                                  END IF
                                  IF(ALENS(9,I).EQ.6.0D0) THEN
                                      TMAXIY=DABS(ALENS(11,I))+DABS(ALENS(12,I))
                                      TMAXIX=DABS(ALENS(11,I))+DABS(ALENS(13,I))
                                      TMAXI=TMAXIX
                                      IF(TMAXIY.GT.TMAXIX) TMAXI=TMAXIY
                                      TMINI=-TMAXI
                                      IF(TMAXI.GT.MAXI) MAXI=TMAXI
                                      IF(TMINI.LT.MAXI) MINI=TMINI
                                  END IF
                                  IF(ALENS(9,I).GE.2.0D0.AND.ALENS(9,I).LE.4.0D0) THEN
                                      TMAXIY=DABS(ALENS(10,I))+DABS(ALENS(12,I))
                                      TMAXIX=DABS(ALENS(11,I))+DABS(ALENS(13,I))
                                      TMAXI=TMAXIX
                                      IF(TMAXIY.GT.TMAXIX) TMAXI=TMAXIY
                                      TMINI=-TMAXI
                                      IF(TMAXI.GT.MAXI) MAXI=TMAXI
                                      IF(TMINI.LT.MAXI) MINI=TMINI
                                  END IF
                              END IF
                          END DO
                      END IF
C     WE NOW HAVE THE MAXI AND MINI FOR THE SCALE FACTOR CALCULATION
C     NOW CALCULATE THE AUTO SCALE FACTOR AND SET APPROPRIATE FLAGS
                      IF(MAXI.EQ.0.0D0.AND.MINI.EQ.0.0D0) THEN
                          DO I=STASUR,STPSUR
                              TMAXI=DABS(ALENS(10,I))
                              TMINI=-DABS(ALENS(10,I))
                              IF(TMAXI.GT.MAXI) MAXI=TMAXI
                              IF(TMINI.LT.MAXI) MINI=TMINI
                          END DO
                      END IF
                      IF(MAXI.EQ.0.0D0.AND.MINI.EQ.0.0D0) THEN
                          DO I=STASUR,STPSUR
                              TMAXI=DABS(PXTRAY(1,I))+DABS(PXTRAY(5,I))
                              TMINI=-TMAXI
                              IF(TMAXI.GT.MAXI) MAXI=TMAXI
                              IF(TMINI.LT.MAXI) MINI=TMINI
                          END DO
                      END IF
C
                      SCFAY=DABS(MAXI-MINI)/6.0D0
                      SCFAX=SCFAY
                      PSIZY=1.0D0/SCFAY
                      PSIZX=1.0D0/SCFAX
                      IF(SYSTEM1(6).EQ.1.0D0) SCFAYP=SCFAY
                      IF(SYSTEM1(6).EQ.1.0D0) SCFAXP=SCFAX
                      IF(SYSTEM1(6).EQ.2.0D0) SCFAYP=SCFAY/2.54D0
                      IF(SYSTEM1(6).EQ.2.0D0) SCFAXP=SCFAX/2.54D0
                      IF(SYSTEM1(6).EQ.3.0D0) SCFAYP=SCFAY/25.4D0
                      IF(SYSTEM1(6).EQ.3.0D0) SCFAXP=SCFAX/25.4D0
                      IF(SYSTEM1(6).EQ.4.0D0) SCFAYP=SCFAY/0.0254
                      IF(SYSTEM1(6).EQ.4.0D0) SCFAXP=SCFAX/0.0254
                      PSIZYP=1.0D0/SCFAYP
                      PSIZXP=1.0D0/SCFAXP
                      IF(SCFAY.EQ.0.0D0.OR.SCFAX.EQ.0.0D0) THEN
                          OUTLYNE=
     1                    'ERROR: AUTOMATIC SCALE FACTOR IS ZERO'
                          CALL SHOWIT(1)
                          OUTLYNE=
     1                    'EXPLICITLY ISSUE A "PLOT SCALE" OR "PLOT SIZE" COMMAND'
                          CALL SHOWIT(1)
                          CALL MACFAL
                          RETURN
                      END IF
C     DON'T DO ANOTHER PLOT SCALE
                      AUTSL=.FALSE.
C     AUTOSCALE HAS BEEN CALCULATED
                      ASCFLG=.TRUE.
                  ELSE
C     AUTO SCALE FACTOR HAS ALREADY BEEN CALCULATED, KEEP GOING
                  END IF
              ELSE
C     A NON-AUTO SCALE OR SIZE FACTOR EXISTS, USE IT
C     NOW DO THE PLOT RANGE
                  IF(RCL.EQ.1.OR.RCL.EQ.2.OR.RCL.EQ.3) THEN
C     DO THIS TO CALCULATE RANGE FOR PLOT JUSTIFICATION
                      I=STASUR
                      X1=CLPDAT(0,1,I,JJ)
                      Y1=CLPDAT(0,2,I,JJ)
                      X2=CLPDAT(180,1,I,JJ)
                      Y2=CLPDAT(180,2,I,JJ)
                      X3=CLPDAT(90,1,I,JJ)
                      Y3=CLPDAT(90,2,I,JJ)
                      X4=CLPDAT(270,1,I,JJ)
                      Y4=CLPDAT(270,2,I,JJ)
                      IF(X1.LT.X2) THEN
                          XMINI1=X1
                          XMAXI1=X2
                      ELSE
                          XMINI1=X2
                          XMAXI1=X1
                      END IF
                      IF(Y1.LT.Y2) THEN
                          YMINI1=Y1
                          YMAXI1=Y2
                      ELSE
                          YMINI1=Y2
                          YMAXI1=Y1
                      END IF
C
                      IF(X3.LT.X4) THEN
                          XMINI2=X3
                          XMAXI2=X4
                      ELSE
                          XMINI2=Y4
                          XMAXI2=Y3
                      END IF
                      IF(Y3.LT.Y4) THEN
                          YMINI2=Y3
                          YMAXI2=Y4
                      ELSE
                          YMINI2=Y4
                          YMAXI2=Y3
                      END IF
                      IF(XMINI1.LT.XMINI2) THEN
                          XMINI=XMINI1
                      ELSE
                          XMINI=XMINI2
                      END IF
                      IF(XMAXI1.LT.XMAXI2) THEN
                          XMAXI=XMAXI1
                      ELSE
                          XMAXI=XMAXI2
                      END IF
                      IF(YMINI1.LT.YMINI2) THEN
                          YMINI=YMINI1
                      ELSE
                          YMINI=YMINI2
                      END IF
                      IF(YMAXI1.LT.YMAXI2) THEN
                          YMAXI=YMAXI1
                      ELSE
                          YMAXI=YMAXI2
                      END IF
                  ELSE
C     RANGE IS OK AS IT IS
                  END IF
              END IF
          END IF
C
          RETURN
      END
C SUB PLTSC3.FOR
      SUBROUTINE PLTSC3(XMINI,XMAXI,YMINI,YMAXI,JJ
     1,CLPDAT,M1,M2,M3,M4,M5)
C
          IMPLICIT NONE
C
C       THIS ROUTINE DOES THE AUTO SCALE FACTOR FOR THE
C       PLOT CLAP COMMAND
C
          REAL*8 X,Y,Z,XN,YN,ZN,ROT1X,ROT1Z,ROT2Y,TMAXI,TMINI,
     1    ROT2Z,AX,AY,AZ,AALF,APHI,YMAXI,YMINI,XMAXI,XMINI,MAXI,MINI
     2    ,VIEPH,VIEAL,X1,X2,X3,X4,Y1,Y2,Y3,Y4,TMAXIX,TMAXIY
     3    ,XMINI1,XMINI2,YMINI1,YMINI2,XMAXI1,XMAXI2,YMAXI1,YMAXI2
C
          INTEGER I,JJ,M1,M2,M3,M4,M5
C
          REAL CLPDAT
          DIMENSION CLPDAT(M1:M2,M3,M1:M4,M5)
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
          VIEPH=(PII/180.0D0)*VIEPHI
          VIEAL=(PII/180.0D0)*VIEALF
C
C     AFTER THIS TRANSFORMATION, THE WORLD X COORDINATE IS PLOTTED IN THE
C     HORIZONTAL X-AXIS OF THE DISPLAY AND THE Y WORLD COORDINATE IS PLOTTED
C     IN THE VERTICAL Y-AXIS OF THE DISPLAY AFTER A CONVERSION TO
C     DEVICE INDEPENDENT COORDINATES ARE MADE.
C
C     FIRST CHECK THE SCALING
          IF(AUTSL) THEN
C     AUTOSCALING IS THE DEFAULT, DOES IT NEED TO BE CALCULATED ?
              IF(.NOT.ASCFLG) THEN
C     AUTOSCALE FACTOR HAS NOT BEEN CALCULATED
C     PROCEED
C     CALCULATE AN AUTO SCALE FACTOR
                  DO I=STASUR,STPSUR
                      X=VERTEX(1,I)
                      Y=VERTEX(2,I)
                      Z=VERTEX(3,I)
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
                      X=XN
                      Y=YN
                      Z=ZN
                      IF(I.EQ.STASUR) THEN
                          YMAXI=Y
                          YMINI=Y
                          XMAXI=X
                          XMINI=X
                      ELSE
C     NOT FIRST CYCLE, DO SOME TESTING
                          IF(Y.GT.YMAXI) YMAXI=Y
                          IF(Y.LT.YMINI) YMINI=Y
                          IF(X.GT.XMAXI) XMAXI=X
                          IF(X.LT.XMINI) XMINI=X
C                        DONE WITH THE TESTING
                      END IF
                  END DO
C     THE XMAXI AND XMINI AND YMAXI AND YMINI VALUES HAVE BEEN CALCULATED
C     CALCULATE THE AUTOMATIC SCALE FACTOR AND REMEMBER IT.
                  IF((DABS(XMAXI-XMINI)).GE.(DABS(YMAXI-YMINI))) THEN
                      MAXI=XMAXI
                      MINI=XMINI
                      IF(MAXI.EQ.0.0D0.AND.MINI.EQ.0.0D0) THEN
                          DO I=STASUR,STPSUR
                              IF(ALENS(9,I).EQ.0.0D0.OR.ALENS(9,I).EQ.5.0D0) THEN
                                  TMAXIX=DABS(PXTRAX(1,I))+DABS(PXTRAX(5,I))
                                  TMAXIY=DABS(PXTRAY(1,I))+DABS(PXTRAY(5,I))
                                  TMAXI=TMAXIX
                                  IF(TMAXIY.GT.TMAXIX) TMAXI=TMAXIY
                              ELSE
                                  IF(ALENS(9,I).EQ.1.0D0) THEN
                                      TMAXIY=DABS(ALENS(10,I))+DABS(ALENS(12,I))
                                      TMAXIX=DABS(ALENS(10,I))+DABS(ALENS(13,I))
                                      TMAXI=TMAXIX
                                      IF(TMAXIY.GT.TMAXIX) TMAXI=TMAXIY
                                      TMINI=-TMAXI
                                      IF(TMAXI.GT.MAXI) MAXI=TMAXI
                                      IF(TMINI.LT.MAXI) MINI=TMINI
                                  END IF
                                  IF(ALENS(9,I).EQ.6.0D0) THEN
                                      TMAXIY=DABS(ALENS(11,I))+DABS(ALENS(12,I))
                                      TMAXIX=DABS(ALENS(11,I))+DABS(ALENS(13,I))
                                      TMAXI=TMAXIX
                                      IF(TMAXIY.GT.TMAXIX) TMAXI=TMAXIY
                                      TMINI=-TMAXI
                                      IF(TMAXI.GT.MAXI) MAXI=TMAXI
                                      IF(TMINI.LT.MAXI) MINI=TMINI
                                  END IF
                                  IF(ALENS(9,I).GE.2.0D0.AND.ALENS(9,I).LE.4.0D0) THEN
                                      TMAXIY=DABS(ALENS(10,I))+DABS(ALENS(12,I))
                                      TMAXIX=DABS(ALENS(11,I))+DABS(ALENS(13,I))
                                      TMAXI=TMAXIX
                                      IF(TMAXIY.GT.TMAXIX) TMAXI=TMAXIY
                                      TMINI=-TMAXI
                                      IF(TMAXI.GT.MAXI) MAXI=TMAXI
                                      IF(TMINI.LT.MAXI) MINI=TMINI
                                  END IF
                              END IF
                          END DO
                      END IF
C     WE NOW HAVE THE MAXI AND MINI FOR THE SCALE FACTOR CALCULATION
C     NOW CALCULATE THE AUTO SCALE FACTOR AND SET APPROPRIATE FLAGS
C
                      SCFAY=DABS(MAXI-MINI)/6.0D0
                      SCFAX=DABS(MAXI-MINI)/6.0D0
                      PSIZY=1.0D0/SCFAY
                      PSIZX=1.0D0/SCFAX
                      IF(SYSTEM1(6).EQ.1.0D0) SCFAYP=SCFAY
                      IF(SYSTEM1(6).EQ.1.0D0) SCFAXP=SCFAX
                      IF(SYSTEM1(6).EQ.2.0D0) SCFAYP=SCFAY/2.54D0
                      IF(SYSTEM1(6).EQ.2.0D0) SCFAXP=SCFAX/2.54D0
                      IF(SYSTEM1(6).EQ.3.0D0) SCFAYP=SCFAY/25.4D0
                      IF(SYSTEM1(6).EQ.3.0D0) SCFAXP=SCFAX/25.4D0
                      IF(SYSTEM1(6).EQ.4.0D0) SCFAYP=SCFAY/0.0254D0
                      IF(SYSTEM1(6).EQ.4.0D0) SCFAXP=SCFAX/0.0254D0
                      PSIZYP=1.0D0/SCFAYP
                      PSIZXP=1.0D0/SCFAXP
                      IF(SCFAY.EQ.0.0D0.OR.SCFAX.EQ.0.0D0) THEN
                          OUTLYNE=
     1                    'ERROR: AUTOMATIC SCALE FACTOR IS ZERO'
                          CALL SHOWIT(1)
                          OUTLYNE=
     1                    'EXPLICITLY ISSUE A "PLOT SCALE" OR "PLOT SIZE" COMMAND'
                          CALL SHOWIT(1)
                          CALL MACFAL
                          RETURN
                      END IF
C     DON'T DO ANOTHER PLOT SCALE
                      AUTSL=.FALSE.
C     AUTOSCALE HAS BEEN CALCULATED
                      ASCFLG=.TRUE.
                  ELSE
C     AUTO SCALE FACTOR HAS ALREADY BEEN CALCULATED, KEEP GOING
                  END IF
              ELSE
C     A NON-AUTO SCALE OR SIZE FACTOR EXISTS, USE IT
                  IF(RCL.EQ.1.OR.RCL.EQ.2.OR.RCL.EQ.3) THEN
C     DO THIS TO CALCULATE RANGE FOR PLOT JUSTIFICATION
                      DO I=STASUR,STPSUR
                          X=VERTEX(1,I)
                          Y=VERTEX(2,I)
                          Z=VERTEX(3,I)
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
                          X=XN
                          Y=YN
                          Z=ZN
                          IF(I.EQ.STASUR) THEN
                              YMAXI=Y
                              YMINI=Y
                              XMAXI=X
                              XMINI=X
                          ELSE
C     NOT FIRST CYCLE, DO SOME TESTING
                              IF(Y.GT.YMAXI) YMAXI=Y
                              IF(Y.LT.YMINI) YMINI=Y
                              IF(X.GT.XMAXI) XMAXI=X
                              IF(X.LT.XMINI) XMINI=X
C                        DONE WITH THE TESTING
C
                          END IF
                      END DO
                  ELSE
C     DON'T NEED TO CALCULATE VALUES FOR RANGE
                  END IF
              END IF
          ELSE
C     CASE OF STASUR=STPSUR
              STASUR=STPSUR
C     STASUR=STPSUR, KEY OFF CLAP VALUES
              IF(AUTSL) THEN
C     AUTOSCALING IS THE DEFAULT, DOES IT NEED TO BE CALCULATED ?
                  IF(.NOT.ASCFLG) THEN
                      I=STASUR
                      X1=CLPDAT(0,1,I,JJ)
                      Y1=CLPDAT(0,2,I,JJ)
                      X2=CLPDAT(180,1,I,JJ)
                      Y2=CLPDAT(180,2,I,JJ)
                      X3=CLPDAT(90,1,I,JJ)
                      Y3=CLPDAT(90,2,I,JJ)
                      X4=CLPDAT(270,1,I,JJ)
                      Y4=CLPDAT(270,2,I,JJ)
                      IF(X1.LT.X2) THEN
                          XMINI1=X1
                          XMAXI1=X2
                      ELSE
                          XMINI1=X2
                          XMAXI1=X1
                      END IF
                      IF(Y1.LT.Y2) THEN
                          YMINI1=Y1
                          YMAXI1=Y2
                      ELSE
                          YMINI1=Y2
                          YMAXI1=Y1
                      END IF
C
                      IF(X3.LT.X4) THEN
                          XMINI2=X3
                          XMAXI2=X4
                      ELSE
                          XMINI2=Y4
                          XMAXI2=Y3
                      END IF
                      IF(Y3.LT.Y4) THEN
                          YMINI2=Y3
                          YMAXI2=Y4
                      ELSE
                          YMINI2=Y4
                          YMAXI2=Y3
                      END IF
                      IF(XMINI1.LT.XMINI2) THEN
                          XMINI=XMINI1
                      ELSE
                          XMINI=XMINI2
                      END IF
                      IF(XMAXI1.LT.XMAXI2) THEN
                          XMAXI=XMAXI1
                      ELSE
                          XMAXI=XMAXI2
                      END IF
                      IF(YMINI1.LT.YMINI2) THEN
                          YMINI=YMINI1
                      ELSE
                          YMINI=YMINI2
                      END IF
                      IF(YMAXI1.LT.YMAXI2) THEN
                          YMAXI=YMAXI1
                      ELSE
                          YMAXI=YMAXI2
                      END IF
C     THE XMAXI AND XMINI AND YMAXI AND YMINI VALUES HAVE BEEN CALCULATED
C     CALCULATE THE AUTOMATIC SCALE FACTOR AND REMEMBER IT.
                      IF((DABS(XMAXI-XMINI)).GE.(DABS(YMAXI-YMINI))) THEN
                          MAXI=XMAXI
                          MINI=XMINI
                      ELSE
                          MAXI=YMAXI
                          MINI=YMINI
                      END IF
                      IF(MAXI.EQ.0.0D0.AND.MINI.EQ.0.0D0) THEN
                          DO I=STASUR,STPSUR
                              IF(ALENS(9,I).EQ.0.0D0.OR.ALENS(9,I).EQ.5.0D0) THEN
                                  TMAXIX=DABS(PXTRAX(1,I))+DABS(PXTRAX(5,I))
                                  TMAXIY=DABS(PXTRAY(1,I))+DABS(PXTRAY(5,I))
                                  TMAXI=TMAXIX
                                  IF(TMAXIY.GT.TMAXIX) TMAXI=TMAXIY
                              ELSE
                                  IF(ALENS(9,I).EQ.1.0D0) THEN
                                      TMAXIY=DABS(ALENS(10,I))+DABS(ALENS(12,I))
                                      TMAXIX=DABS(ALENS(10,I))+DABS(ALENS(13,I))
                                      TMAXI=TMAXIX
                                      IF(TMAXIY.GT.TMAXIX) TMAXI=TMAXIY
                                      TMINI=-TMAXI
                                      IF(TMAXI.GT.MAXI) MAXI=TMAXI
                                      IF(TMINI.LT.MAXI) MINI=TMINI
                                  END IF
                                  IF(ALENS(9,I).EQ.6.0D0) THEN
                                      TMAXIY=DABS(ALENS(11,I))+DABS(ALENS(12,I))
                                      TMAXIX=DABS(ALENS(11,I))+DABS(ALENS(13,I))
                                      TMAXI=TMAXIX
                                      IF(TMAXIY.GT.TMAXIX) TMAXI=TMAXIY
                                      TMINI=-TMAXI
                                      IF(TMAXI.GT.MAXI) MAXI=TMAXI
                                      IF(TMINI.LT.MAXI) MINI=TMINI
                                  END IF
                                  IF(ALENS(9,I).GE.2.0D0.AND.ALENS(9,I).LE.4.0D0) THEN
                                      TMAXIY=DABS(ALENS(10,I))+DABS(ALENS(12,I))
                                      TMAXIX=DABS(ALENS(11,I))+DABS(ALENS(13,I))
                                      TMAXI=TMAXIX
                                      IF(TMAXIY.GT.TMAXIX) TMAXI=TMAXIY
                                      TMINI=-TMAXI
                                      IF(TMAXI.GT.MAXI) MAXI=TMAXI
                                      IF(TMINI.LT.MAXI) MINI=TMINI
                                  END IF
                              END IF
                          END DO
                      END IF
C     WE NOW HAVE THE MAXI AND MINI FOR THE SCALE FACTOR CALCULATION
C     NOW CALCULATE THE AUTO SCALE FACTOR AND SET APPROPRIATE FLAGS
C
                      SCFAY=DABS(MAXI-MINI)/6.0D0
                      SCFAX=SCFAY
                      PSIZY=1.0D0/SCFAY
                      PSIZX=1.0D0/SCFAX
                      IF(SYSTEM1(6).EQ.1.0D0) SCFAYP=SCFAY
                      IF(SYSTEM1(6).EQ.1.0D0) SCFAXP=SCFAX
                      IF(SYSTEM1(6).EQ.2.0D0) SCFAYP=SCFAY/2.54D0
                      IF(SYSTEM1(6).EQ.2.0D0) SCFAXP=SCFAX/2.54D0
                      IF(SYSTEM1(6).EQ.3.0D0) SCFAYP=SCFAY/25.4D0
                      IF(SYSTEM1(6).EQ.3.0D0) SCFAXP=SCFAX/25.4D0
                      IF(SYSTEM1(6).EQ.4.0D0) SCFAYP=SCFAY/0.0254
                      IF(SYSTEM1(6).EQ.4.0D0) SCFAXP=SCFAX/0.0254
                      PSIZYP=1.0D0/SCFAYP
                      PSIZXP=1.0D0/SCFAXP
                      IF(SCFAY.EQ.0.0D0.OR.SCFAX.EQ.0.0D0) THEN
                          OUTLYNE=
     1                    'ERROR: AUTOMATIC SCALE FACTOR IS ZERO'
                          CALL SHOWIT(1)
                          OUTLYNE=
     1                    'EXPLICITLY ISSUE A "PLOT SCALE" OR "PLOT SIZE" COMMAND'
                          CALL SHOWIT(1)
                          CALL MACFAL
                          RETURN
                      END IF
C     DON'T DO ANOTHER PLOT SCALE
                      AUTSL=.FALSE.
C     AUTOSCALE HAS BEEN CALCULATED
                      ASCFLG=.TRUE.
                  ELSE
C     AUTO SCALE FACTOR HAS ALREADY BEEN CALCULATED, KEEP GOING
                  END IF
              ELSE
C     A NON-AUTO SCALE OR SIZE FACTOR EXISTS, USE IT
C     NOW DO THE PLOT RANGE
                  IF(RCL.EQ.1.OR.RCL.EQ.2.OR.RCL.EQ.3) THEN
C     DO THIS TO CALCULATE RANGE FOR PLOT JUSTIFICATION
                      I=STASUR
                      X1=CLPDAT(0,1,I,JJ)
                      Y1=CLPDAT(0,2,I,JJ)
                      X2=CLPDAT(180,1,I,JJ)
                      Y2=CLPDAT(180,2,I,JJ)
                      X3=CLPDAT(90,1,I,JJ)
                      Y3=CLPDAT(90,2,I,JJ)
                      X4=CLPDAT(270,1,I,JJ)
                      Y4=CLPDAT(270,2,I,JJ)
                      IF(X1.LT.X2) THEN
                          XMINI1=X1
                          XMAXI1=X2
                      ELSE
                          XMINI1=X2
                          XMAXI1=X1
                      END IF
                      IF(Y1.LT.Y2) THEN
                          YMINI1=Y1
                          YMAXI1=Y2
                      ELSE
                          YMINI1=Y2
                          YMAXI1=Y1
                      END IF
C
                      IF(X3.LT.X4) THEN
                          XMINI2=X3
                          XMAXI2=X4
                      ELSE
                          XMINI2=Y4
                          XMAXI2=Y3
                      END IF
                      IF(Y3.LT.Y4) THEN
                          YMINI2=Y3
                          YMAXI2=Y4
                      ELSE
                          YMINI2=Y4
                          YMAXI2=Y3
                      END IF
                      IF(XMINI1.LT.XMINI2) THEN
                          XMINI=XMINI1
                      ELSE
                          XMINI=XMINI2
                      END IF
                      IF(XMAXI1.LT.XMAXI2) THEN
                          XMAXI=XMAXI1
                      ELSE
                          XMAXI=XMAXI2
                      END IF
                      IF(YMINI1.LT.YMINI2) THEN
                          YMINI=YMINI1
                      ELSE
                          YMINI=YMINI2
                      END IF
                      IF(YMAXI1.LT.YMAXI2) THEN
                          YMAXI=YMAXI1
                      ELSE
                          YMAXI=YMAXI2
                      END IF
                  ELSE
C     RANGE IS OK AS IT IS
                  END IF
              END IF
          END IF
C
          RETURN
      END
C SUB PLTSC2.FOR
      SUBROUTINE PLTSC2(XMINI,XMAXI,YMINI,YMAXI,PRO,M1,M2,M3,M4)
C
          IMPLICIT NONE
C
C       THIS ROUTINE DOES THE AUTO SCALE FACTOR FOR THE
C       PLOT PROFX/PROFY AND PROF COMMANDS
C
          REAL*8 X,Y,Z,XN,YN,ZN,ROT1X,ROT1Z,ROT2Y,XBASE,YBASE,
     1    ROT2Z,AX,AY,AZ,AALF,APHI,YMAXI,YMINI,XMAXI,XMINI,MAXI,MINI
     2    ,VIEPH,VIEAL,X1,X2,Y1,Y2,PRO,TMAXI,TMINI,TMAXIX,TMAXIY
C
          INTEGER I,M1,M2,M3,M4
C
          DIMENSION PRO(M1,M2,M3:M4)
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
          VIEPH=(PII/180.0D0)*VIEPHI
          VIEAL=(PII/180.0D0)*VIEALF
C
C     AFTER THIS TRANSFORMATION, THE WORLD X COORDINATE IS PLOTTED IN THE
C     HORIZONTAL X-AXIS OF THE DISPLAY AND THE Y WORLD COORDINATE IS PLOTTED
C     IN THE VERTICAL Y-AXIS OF THE DISPLAY AFTER A CONVERSION TO
C     DEVICE INDEPENDENT COORDINATES ARE MADE.
C
          IF(STASUR.NE.STPSUR) THEN
C     FIRST CHECK THE SCALING
              IF(AUTSL) THEN
C     AUTOSCALING IS THE DEFAULT, DOES IT NEED TO BE CALCULATED ?
                  IF(.NOT.ASCFLG) THEN
C     AUTOSCALE FACTOR HAS NOT BEEN CALCULATED
C     PROCEED
C     CALCULATE AN AUTO SCALE FACTOR BASED ON THE CURRENTLY
C     REQUESTED RAY PLOTTING DATA SO NO DATA RANGE EXCEEDS
C     6000 DEVICE INDEPENDENT UNITS. THIS KEEPS A GAMMA ROTATION
C     AT THE SCREEN FROM THE FIFTH NUMERIC WORD OF PLOT LOOK OR
C     PLOT VIEW FROM FORCING VALUES OUTSIDE THE PLOTTING WINDOW.
C     DETERMINE THE STARTING AND ENDING WORLD COORDINATES IN THE
C     SCREEN X AND Y DIRECTIONS.
C     WE ARE PLOTTING FROM STASUR TO STPSUR. CALCULATE THE MAX
C     Y AND Z CCORDINATES OF THE VERTEX AFTER APPLYING THE LOOK/VIEW
C     TRANSFORMATIONS. FIND THE LARGEST AND SMALLEST (ALGEBRAICALLY)
C     OF THESE VALUES.
                      DO I=STASUR,STPSUR
C
                          X=VERTEX(1,I)
                          Y=VERTEX(2,I)
                          Z=VERTEX(3,I)
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
                          X=XN
                          Y=YN
                          Z=ZN
                          IF(I.EQ.STASUR) THEN
                              YMAXI=Y
                              YMINI=Y
                              XMAXI=X
                              XMINI=X
                          ELSE
C     NOT FIRST CYCLE, DO SOME TESTING
                              IF(Y.GT.YMAXI) YMAXI=Y
                              IF(Y.LT.YMINI) YMINI=Y
                              IF(X.GT.XMAXI) XMAXI=X
                              IF(X.LT.XMINI) XMINI=X
C                        DONE WITH THE TESTING
C
                          END IF
                      END DO
C     THE XMAXI AND XMINI AND YMAXI AND YMINI VALUES HAVE BEEN CALCULATED
C     CALCULATE THE AUTOMATIC SCALE FACTOR AND REMEMBER IT.
                      IF((DABS(XMAXI-XMINI)).GE.(DABS(YMAXI-YMINI))) THEN
                          MAXI=XMAXI
                          MINI=XMINI
                      ELSE
                          MAXI=YMAXI
                          MINI=YMINI
                      END IF
                      IF(MAXI.EQ.0.0D0.AND.MINI.EQ.0.0D0) THEN
                          DO I=STASUR,STPSUR
                              IF(ALENS(9,I).EQ.0.0D0.OR.ALENS(9,I).EQ.5.0D0) THEN
                                  TMAXIX=DABS(PXTRAX(1,I))+DABS(PXTRAX(5,I))
                                  TMAXIY=DABS(PXTRAY(1,I))+DABS(PXTRAY(5,I))
                                  TMAXI=TMAXIX
                                  IF(TMAXIY.GT.TMAXIX) TMAXI=TMAXIY
                              ELSE
                                  IF(ALENS(9,I).EQ.1.0D0) THEN
                                      TMAXIY=DABS(ALENS(10,I))+DABS(ALENS(12,I))
                                      TMAXIX=DABS(ALENS(10,I))+DABS(ALENS(13,I))
                                      TMAXI=TMAXIX
                                      IF(TMAXIY.GT.TMAXIX) TMAXI=TMAXIY
                                      TMINI=-TMAXI
                                      IF(TMAXI.GT.MAXI) MAXI=TMAXI
                                      IF(TMINI.LT.MAXI) MINI=TMINI
                                  END IF
                                  IF(ALENS(9,I).EQ.6.0D0) THEN
                                      TMAXIY=DABS(ALENS(11,I))+DABS(ALENS(12,I))
                                      TMAXIX=DABS(ALENS(11,I))+DABS(ALENS(13,I))
                                      TMAXI=TMAXIX
                                      IF(TMAXIY.GT.TMAXIX) TMAXI=TMAXIY
                                      TMINI=-TMAXI
                                      IF(TMAXI.GT.MAXI) MAXI=TMAXI
                                      IF(TMINI.LT.MAXI) MINI=TMINI
                                  END IF
                                  IF(ALENS(9,I).GE.2.0D0.AND.ALENS(9,I).LE.4.0D0) THEN
                                      TMAXIY=DABS(ALENS(10,I))+DABS(ALENS(12,I))
                                      TMAXIX=DABS(ALENS(11,I))+DABS(ALENS(13,I))
                                      TMAXI=TMAXIX
                                      IF(TMAXIY.GT.TMAXIX) TMAXI=TMAXIY
                                      TMINI=-TMAXI
                                      IF(TMAXI.GT.MAXI) MAXI=TMAXI
                                      IF(TMINI.LT.MAXI) MINI=TMINI
                                  END IF
                              END IF
                          END DO
                      END IF
C     WE NOW HAVE THE MAXI AND MINI FOR THE SCALE FACTOR CALCULATION
C     NOW CALCULATE THE AUTO SCALE FACTOR AND SET APPROPRIATE FLAGS
C
                      SCFAY=DABS(MAXI-MINI)/6.0D0
                      SCFAX=DABS(MAXI-MINI)/6.0D0
                      PSIZY=1.0D0/SCFAY
                      PSIZX=1.0D0/SCFAX
                      IF(SYSTEM1(6).EQ.1.0D0) SCFAYP=SCFAY
                      IF(SYSTEM1(6).EQ.1.0D0) SCFAXP=SCFAX
                      IF(SYSTEM1(6).EQ.2.0D0) SCFAYP=SCFAY/2.54D0
                      IF(SYSTEM1(6).EQ.2.0D0) SCFAXP=SCFAX/2.54D0
                      IF(SYSTEM1(6).EQ.3.0D0) SCFAYP=SCFAY/25.4D0
                      IF(SYSTEM1(6).EQ.3.0D0) SCFAXP=SCFAX/25.4D0
                      IF(SYSTEM1(6).EQ.4.0D0) SCFAYP=SCFAY/0.0254D0
                      IF(SYSTEM1(6).EQ.4.0D0) SCFAXP=SCFAX/0.0254D0
                      PSIZYP=1.0D0/SCFAYP
                      PSIZXP=1.0D0/SCFAXP
                      IF(SCFAY.EQ.0.0D0.OR.SCFAX.EQ.0.0D0) THEN
                          OUTLYNE=
     1                    'ERROR: AUTOMATIC SCALE FACTOR IS ZERO'
                          CALL SHOWIT(1)
                          OUTLYNE=
     1                    'EXPLICITLY ISSUE A "PLOT SCALE" OR "PLOT SIZE" COMMAND'
                          CALL SHOWIT(1)
                          CALL MACFAL
                          RETURN
                      END IF
C     DON'T DO ANOTHER PLOT SCALE
                      AUTSL=.FALSE.
C     AUTOSCALE HAS BEEN CALCULATED
                      ASCFLG=.TRUE.
                  ELSE
C     AUTO SCALE FACTOR HAS ALREADY BEEN CALCULATED, KEEP GOING
                  END IF
              ELSE
C     A NON-AUTO SCALE OR SIZE FACTOR EXISTS, USE IT
                  IF(RCL.EQ.1.OR.RCL.EQ.2.OR.RCL.EQ.3) THEN
C     DO THIS TO CALCULATE RANGE FOR PLOT JUSTIFICATION
                      DO I=STASUR,STPSUR
                          X=VERTEX(1,I)
                          Y=VERTEX(2,I)
                          Z=VERTEX(3,I)
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
                          X=XN
                          Y=YN
                          Z=ZN
                          IF(I.EQ.STASUR) THEN
                              YMAXI=Y
                              YMINI=Y
                              XMAXI=X
                              XMINI=X
                          ELSE
C     NOT FIRST CYCLE, DO SOME TESTING
                              IF(Y.GT.YMAXI) YMAXI=Y
                              IF(Y.LT.YMINI) YMINI=Y
                              IF(X.GT.XMAXI) XMAXI=X
                              IF(X.LT.XMINI) XMINI=X
C                        DONE WITH THE TESTING
C
                          END IF
                      END DO
                  ELSE
C     DON'T NEED TO CALCULATE VALUES FOR RANGE
                  END IF
              END IF
          ELSE
C     CASE OF STASUR=STPSUR
              STASUR=STPSUR
C     STASUR=STPSUR, KEY OFF PROFILE VALUES
              IF(AUTSL) THEN
C     AUTOSCALING IS THE DEFAULT, DOES IT NEED TO BE CALCULATED ?
                  IF(.NOT.ASCFLG) THEN
                      I=STASUR
                      X1=PRO(1,1,I)
                      Y1=PRO(1,2,I)
                      X2=PRO(90,1,I)
                      Y2=PRO(90,2,I)
                      IF(X1.LT.X2) THEN
                          XMINI=X1
                          XMAXI=X2
                      ELSE
                          XMINI=X2
                          XMAXI=X1
                      END IF
                      IF(Y1.LT.Y2) THEN
                          YMINI=Y1
                          YMAXI=Y2
                      ELSE
                          YMINI=Y2
                          YMAXI=Y1
                      END IF
C     THE XMAXI AND XMINI AND YMAXI AND YMINI VALUES HAVE BEEN CALCULATED
C     CALCULATE THE AUTOMATIC SCALE FACTOR AND REMEMBER IT.
                      IF((DABS(XMAXI-XMINI)).GE.(DABS(YMAXI-YMINI))) THEN
                          MAXI=XMAXI
                          MINI=XMINI
                      ELSE
                          MAXI=YMAXI
                          MINI=YMINI
                      END IF
                      IF(MAXI.EQ.0.0D0.AND.MINI.EQ.0.0D0) THEN
                          DO I=STASUR,STPSUR
                              IF(ALENS(9,I).EQ.0.0D0.OR.ALENS(9,I).EQ.5.0D0) THEN
                                  TMAXIX=DABS(PXTRAX(1,I))+DABS(PXTRAX(5,I))
                                  TMAXIY=DABS(PXTRAY(1,I))+DABS(PXTRAY(5,I))
                                  TMAXI=TMAXIX
                                  IF(TMAXIY.GT.TMAXIX) TMAXI=TMAXIY
                              ELSE
                                  IF(ALENS(9,I).EQ.1.0D0) THEN
                                      TMAXIY=DABS(ALENS(10,I))+DABS(ALENS(12,I))
                                      TMAXIX=DABS(ALENS(10,I))+DABS(ALENS(13,I))
                                      TMAXI=TMAXIX
                                      IF(TMAXIY.GT.TMAXIX) TMAXI=TMAXIY
                                      TMINI=-TMAXI
                                      IF(TMAXI.GT.MAXI) MAXI=TMAXI
                                      IF(TMINI.LT.MAXI) MINI=TMINI
                                  END IF
                                  IF(ALENS(9,I).EQ.6.0D0) THEN
                                      TMAXIY=DABS(ALENS(11,I))+DABS(ALENS(12,I))
                                      TMAXIX=DABS(ALENS(11,I))+DABS(ALENS(13,I))
                                      TMAXI=TMAXIX
                                      IF(TMAXIY.GT.TMAXIX) TMAXI=TMAXIY
                                      TMINI=-TMAXI
                                      IF(TMAXI.GT.MAXI) MAXI=TMAXI
                                      IF(TMINI.LT.MAXI) MINI=TMINI
                                  END IF
                                  IF(ALENS(9,I).GE.2.0D0.AND.ALENS(9,I).LE.4.0D0) THEN
                                      TMAXIY=DABS(ALENS(10,I))+DABS(ALENS(12,I))
                                      TMAXIX=DABS(ALENS(11,I))+DABS(ALENS(13,I))
                                      TMAXI=TMAXIX
                                      IF(TMAXIY.GT.TMAXIX) TMAXI=TMAXIY
                                      TMINI=-TMAXI
                                      IF(TMAXI.GT.MAXI) MAXI=TMAXI
                                      IF(TMINI.LT.MAXI) MINI=TMINI
                                  END IF
                              END IF
                          END DO
                      END IF
C     WE NOW HAVE THE MAXI AND MINI FOR THE SCALE FACTOR CALCULATION
C     NOW CALCULATE THE AUTO SCALE FACTOR AND SET APPROPRIATE FLAGS
C
                      SCFAY=DABS(MAXI-MINI)/6.0D0
                      SCFAX=SCFAY
                      PSIZY=1.0D0/SCFAY
                      PSIZX=1.0D0/SCFAX
                      IF(SYSTEM1(6).EQ.1.0D0) SCFAYP=SCFAY
                      IF(SYSTEM1(6).EQ.1.0D0) SCFAXP=SCFAX
                      IF(SYSTEM1(6).EQ.2.0D0) SCFAYP=SCFAY/2.54D0
                      IF(SYSTEM1(6).EQ.2.0D0) SCFAXP=SCFAX/2.54D0
                      IF(SYSTEM1(6).EQ.3.0D0) SCFAYP=SCFAY/25.4D0
                      IF(SYSTEM1(6).EQ.3.0D0) SCFAXP=SCFAX/25.4D0
                      IF(SYSTEM1(6).EQ.4.0D0) SCFAYP=SCFAY/0.0254
                      IF(SYSTEM1(6).EQ.4.0D0) SCFAXP=SCFAX/0.0254
                      PSIZYP=1.0D0/SCFAYP
                      PSIZXP=1.0D0/SCFAXP
                      IF(SCFAY.EQ.0.0D0.OR.SCFAX.EQ.0.0D0) THEN
                          OUTLYNE=
     1                    'ERROR: AUTOMATIC SCALE FACTOR IS ZERO'
                          CALL SHOWIT(1)
                          OUTLYNE=
     1                    'EXPLICITLY ISSUE A "PLOT SCALE" OR "PLOT SIZE" COMMAND'
                          CALL SHOWIT(1)
                          CALL MACFAL
                          RETURN
                      END IF
C     DON'T DO ANOTHER PLOT SCALE
                      AUTSL=.FALSE.
C     AUTOSCALE HAS BEEN CALCULATED
                      ASCFLG=.TRUE.
                  ELSE
C     AUTO SCALE FACTOR HAS ALREADY BEEN CALCULATED, KEEP GOING
                  END IF
              ELSE
C     A NON-AUTO SCALE OR SIZE FACTOR EXISTS, USE IT
C     NOW DO THE PLOT RANGE
                  IF(RCL.EQ.1.OR.RCL.EQ.2.OR.RCL.EQ.3) THEN
C     DO THIS TO CALCULATE RANGE FOR PLOT JUSTIFICATION
                      I=STASUR
                      X1=PRO(1,1,I)
                      Y1=PRO(1,2,I)
                      X2=PRO(90,1,I)
                      Y2=PRO(90,2,I)
                      XBASE=X1
                      YBASE=Y1
                      IF(X2.GT.XBASE) XBASE=X2
                      IF(Y2.GT.YBASE) YBASE=Y2
                      XMAXI=XBASE
                      YMAXI=YBASE
                      XBASE=X1
                      YBASE=X1
                      IF(X2.LE.XBASE) XBASE=X2
                      IF(Y2.LE.YBASE) YBASE=Y2
                      XMINI=XBASE
                      YMINI=YBASE
                  ELSE
C     RANGE IS OK AS IT IS
                  END IF
              END IF
          END IF

C
          RETURN
      END
C SUB PLTSC1.FOR
      SUBROUTINE PLTSC1(XMINI,XMAXI,YMINI,YMAXI)
C
          IMPLICIT NONE
C
C       THIS ROUTINE DOES THE AUTO SCALE FACTOR FOR PLOT RAY
C     AND PLOT EDGEX/EDGEY COMMANDS
C
          REAL*8 X,Y,Z,XN,YN,ZN,ROT1X,ROT1Z,ROT2Y,
     1    ROT2Z,AX,AY,AZ,AALF,APHI,YMAXI,YMINI,XMAXI,XMINI,MAXI,MINI
     2    ,VIEPH,VIEAL,TMAXI,TMINI,TMAXIX,TMAXIY
C
          INTEGER I
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
          VIEPH=(PII/180.0D0)*VIEPHI
          VIEAL=(PII/180.0D0)*VIEALF
C
C     FIRST CHECK THE SCALING
          IF(AUTSL) THEN
C     AUTOSCALING IS THE DEFAULT, DOES IT NEED TO BE CALCULATED ?
              IF(.NOT.ASCFLG) THEN
C     AUTOSCALE FACTOR HAS NOT BEEN CALCULATED
C     PROCEED
C     CALCULATE AN AUTO SCALE FACTOR BASED ON THE CURRENTLY
C     REQUESTED RAY PLOTTING DATA SO NO DATA RANGE EXCEEDS
C     6000 DEVICE INDEPENDENT UNITS. THIS KEEPS A GAMMA ROTATION
C     AT THE SCREEN FROM THE FIFTH NUMERIC WORD OF PLOT LOOK OR
C     PLOT VIEW FROM FORCING VALUES OUTSIDE THE PLOTTING WINDOW.
C     DETERMINE THE STARTING AND ENDING WORLD COORDINATES IN THE
C     SCREEN X AND Y DIRECTIONS.
C     WE ARE PLOTTING FROM STASUR TO STPSUR. CALCULATE THE MAX
C     Y AND Z CCORDINATES OF THE VERTEX AFTER APPLYING THE LOOK/VIEW
C     TRANSFORMATIONS. FIND THE LARGEST AND SMALLEST (ALGEBRAICALLY)
C     OF THESE VALUES.
                  DO I=STASUR,STPSUR
C
                      X=VERTEX(1,I)
                      Y=VERTEX(2,I)
                      Z=VERTEX(3,I)
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
                      X=XN
                      Y=YN
                      Z=ZN
                      IF(I.EQ.STASUR) THEN
                          YMAXI=Y
                          YMINI=Y
                          XMAXI=X
                          XMINI=X
                      ELSE
C     NOT FIRST CYCLE, DO SOME TESTING
                          IF(Y.GT.YMAXI) YMAXI=Y
                          IF(Y.LT.YMINI) YMINI=Y
                          IF(X.GT.XMAXI) XMAXI=X
                          IF(X.LT.XMINI) XMINI=X
C                        DONE WITH THE TESTING
C
                      END IF
                  END DO
C     THE XMAXI AND XMINI AND YMAXI AND YMINI VALUES HAVE BEEN CALCULATED
C     CALCULATE THE AUTOMATIC SCALE FACTOR AND REMEMBER IT.
                  IF((DABS(XMAXI-XMINI)).GE.(DABS(YMAXI-YMINI))) THEN
                      MAXI=XMAXI
                      MINI=XMINI
                  ELSE
                      MAXI=YMAXI
                      MINI=YMINI
                  END IF
                  IF(MAXI.EQ.0.0D0.AND.MINI.EQ.0.0D0) THEN
                      DO I=STASUR,STPSUR
                          IF(ALENS(9,I).EQ.0.0D0.OR.ALENS(9,I).EQ.5.0D0) THEN
                              TMAXIX=DABS(PXTRAX(1,I))+DABS(PXTRAX(5,I))
                              TMAXIY=DABS(PXTRAY(1,I))+DABS(PXTRAY(5,I))
                              TMAXI=TMAXIX
                              IF(TMAXIY.GT.TMAXIX) TMAXI=TMAXIY
                          ELSE
                              IF(ALENS(9,I).EQ.1.0D0) THEN
                                  TMAXIY=DABS(ALENS(10,I))+DABS(ALENS(12,I))
                                  TMAXIX=DABS(ALENS(10,I))+DABS(ALENS(13,I))
                                  TMAXI=TMAXIX
                                  IF(TMAXIY.GT.TMAXIX) TMAXI=TMAXIY
                                  TMINI=-TMAXI
                                  IF(TMAXI.GT.MAXI) MAXI=TMAXI
                                  IF(TMINI.LT.MAXI) MINI=TMINI
                              END IF
                              IF(ALENS(9,I).EQ.6.0D0) THEN
                                  TMAXIY=DABS(ALENS(11,I))+DABS(ALENS(12,I))
                                  TMAXIX=DABS(ALENS(11,I))+DABS(ALENS(13,I))
                                  TMAXI=TMAXIX
                                  IF(TMAXIY.GT.TMAXIX) TMAXI=TMAXIY
                                  TMINI=-TMAXI
                                  IF(TMAXI.GT.MAXI) MAXI=TMAXI
                                  IF(TMINI.LT.MAXI) MINI=TMINI
                              END IF
                              IF(ALENS(9,I).GE.2.0D0.AND.ALENS(9,I).LE.4.0D0) THEN
                                  TMAXIY=DABS(ALENS(10,I))+DABS(ALENS(12,I))
                                  TMAXIX=DABS(ALENS(11,I))+DABS(ALENS(13,I))
                                  TMAXI=TMAXIX
                                  IF(TMAXIY.GT.TMAXIX) TMAXI=TMAXIY
                                  TMINI=-TMAXI
                                  IF(TMAXI.GT.MAXI) MAXI=TMAXI
                                  IF(TMINI.LT.MAXI) MINI=TMINI
                              END IF
                          END IF
                      END DO
                  END IF
C     WE NOW HAVE THE MAXI AND MINI FOR THE SCALE FACTOR CALCULATION
C     NOW CALCULATE THE AUTO SCALE FACTOR AND SET APPROPRIATE FLAGS
C
                  SCFAY=DABS(MAXI-MINI)/6.0D0
                  SCFAX=DABS(MAXI-MINI)/6.0D0
                  PSIZY=1.0D0/SCFAY
                  PSIZX=1.0D0/SCFAX
                  IF(SYSTEM1(6).EQ.1.0D0) SCFAYP=SCFAY
                  IF(SYSTEM1(6).EQ.1.0D0) SCFAXP=SCFAX
                  IF(SYSTEM1(6).EQ.2.0D0) SCFAYP=SCFAY/2.54D0
                  IF(SYSTEM1(6).EQ.2.0D0) SCFAXP=SCFAX/2.54D0
                  IF(SYSTEM1(6).EQ.3.0D0) SCFAYP=SCFAY/25.4D0
                  IF(SYSTEM1(6).EQ.3.0D0) SCFAXP=SCFAX/25.4D0
                  IF(SYSTEM1(6).EQ.4.0D0) SCFAYP=SCFAY/0.0254D0
                  IF(SYSTEM1(6).EQ.4.0D0) SCFAXP=SCFAX/0.0254D0
                  PSIZYP=1.0D0/SCFAYP
                  PSIZXP=1.0D0/SCFAXP
                  IF(SCFAY.EQ.0.0D0.OR.SCFAX.EQ.0.0D0) THEN
                      OUTLYNE=
     1                'ERROR: AUTOMATIC SCALE FACTOR IS ZERO'
                      CALL SHOWIT(1)
                      OUTLYNE=
     1                'EXPLICITLY ISSUE A "PLOT SCALE" OR "PLOT SIZE" COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
C     SET FLAGS TO LOCK IN THESE VALUES
C     DON'T DO ANOTHER PLOT SCALE
                  AUTSL=.FALSE.
C     AUTOSCALE HAS BEEN CALCULATED
                  ASCFLG=.TRUE.
              ELSE
C     AUTO SCALE FACTOR HAS ALREADY BEEN CALCULATED, KEEP GOING
              END IF
          ELSE
C     A NON-AUTO SCALE OR SIZE FACTOR EXISTS, USE IT
              IF(RCL.EQ.1.OR.RCL.EQ.2.OR.RCL.EQ.3) THEN
C     DO THIS TO CALCULATE RANGE FOR PLOT JUSTIFICATION
                  DO I=STASUR,STPSUR
                      X=VERTEX(1,I)
                      Y=VERTEX(2,I)
                      Z=VERTEX(3,I)
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
                      X=XN
                      Y=YN
                      Z=ZN
                      IF(I.EQ.STASUR) THEN
                          YMAXI=Y
                          YMINI=Y
                          XMAXI=X
                          XMINI=X
                      ELSE
C     NOT FIRST CYCLE, DO SOME TESTING
                          IF(Y.GT.YMAXI) YMAXI=Y
                          IF(Y.LT.YMINI) YMINI=Y
                          IF(X.GT.XMAXI) XMAXI=X
                          IF(X.LT.XMINI) XMINI=X
C                        DONE WITH THE TESTING
C
                      END IF
                  END DO
              ELSE
C     DON'T NEED TO CALCULATE VALUES FOR RANGE
              END IF
          END IF
C
          RETURN
      END
C SUB PLTSAG.FOR
      SUBROUTINE PLT_SAGFILE
C
          IMPLICIT NONE
C
C       THIS PROGRAM CONTROLS THE PLOT SAGFILE COMMAND

C
          INTEGER I,ALLOERR,N,NNN,ISURF,NSTAR2
C
          REAL*8 SAGARRAY1,SAGARRAY2,SAGARRAY3
C
          REAL*8 XMAX,XMIN,ZMAX,ZMIN,X,Y,SAG
C
          DIMENSION SAGARRAY1(:),SAGARRAY2(:),SAGARRAY3(:)
C
          ALLOCATABLE :: SAGARRAY1,SAGARRAY2,SAGARRAY3
C
          LOGICAL EXIS90
C
          LOGICAL ROTSAGFL
C
          COMMON/SAGFLROT/ROTSAGFL
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          INCLUDE 'datmac.inc'
C
          DEALLOCATE(SAGARRAY1,SAGARRAY2,SAGARRAY3,STAT=ALLOERR)
C
          IF(STI.EQ.1) THEN
              OUTLYNE= '"PLOT SAGFILE" PLOTS THE EXISTING SAG.DAT FILE'
              CALL SHOWIT(1)
              OUTLYNE= 'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              RETURN
          END IF
          IF(SST.EQ.1) THEN
              OUTLYNE= '"PLOT SAGFILE" TAKES NO STRING INPUT'
              CALL SHOWIT(1)
              OUTLYNE= 'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
              OUTLYNE= '"PLOT SAGFILE" ONLY TAKES NUMERIC WORD #1'
              CALL SHOWIT(1)
              OUTLYNE= 'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(DF1.EQ.1) W1=0.0D0
C
C     OPEN AND LOAD THE APPROPRIATE SAG DATA AND THEN PLOT IT
          EXIS90=.FALSE.
          INQUIRE(FILE=trim(HOME)//'SAG.DAT',EXIST=EXIS90)
          IF(.NOT.EXIS90) THEN
              OUTLYNE=
     1        'NO SAG.DAT FILE EXISTS TO PLOT'
              CALL SHOWIT(1)
              OUTLYNE='NO ACTION TAKEN'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C     FILE EXISTS
          CALL CLOSE_FILE(90,1)
          OPEN(UNIT=90,FILE=trim(HOME)//'SAG.DAT',STATUS='UNKNOWN')
          READ(90,END=777,FMT=*,ERR=778) ISURF,NSTAR2
          DO I=1,99999
              READ(90,END=777,FMT=*,ERR=778) X,Y,SAG
C     N IS THE NUMBER OF GOOD DATA POINTS
              N=I
          END DO
          GO TO 777
 778      CONTINUE
          OUTLYNE=
     1    'SAG.DAT FILE CONTAINS ERRORS AND CAN NOT BE PLOTTED'
          CALL SHOWIT(1)
          OUTLYNE='NO ACTION TAKEN'
          CALL SHOWIT(1)
          CALL MACFAL
          RETURN
 777      CONTINUE
          REWIND (UNIT=90)
          DEALLOCATE(SAGARRAY1,SAGARRAY2,SAGARRAY3
     1    ,STAT=ALLOERR)
          ALLOCATE(SAGARRAY1(1:N),SAGARRAY2(1:N),SAGARRAY3(1:N)
     1    ,STAT=ALLOERR)
          XMAX=-1.0D30
          XMIN=1.0D30
          READ(90,*) ISURF
          DO I=1,N
              READ(90,*) SAGARRAY1(I),SAGARRAY2(I),SAGARRAY3(I)

              IF(SAGARRAY1(I).GT.XMAX) XMAX=SAGARRAY1(I)
              IF(SAGARRAY2(I).GT.XMAX) XMAX=SAGARRAY2(I)
              IF(SAGARRAY1(I).LT.XMIN) XMIN=SAGARRAY1(I)
              IF(SAGARRAY2(I).LT.XMIN) XMIN=SAGARRAY2(I)
          END DO

          ZMAX=-1.0D30
          ZMIN=1.0D30
          DO I=1,N
              IF(SAGARRAY3(I).GT.ZMAX) ZMAX=SAGARRAY3(I)
              IF(SAGARRAY3(I).LT.ZMIN) ZMIN=SAGARRAY3(I)
          END DO
          DO I=1,N
              SAGARRAY3(I)=SAGARRAY3(I)-ZMIN
          END DO
C
C     NOW ARRAYS ARE LOADED AND RANGES ARE COMPUTED, CONTINUE
C
          NNN=INT(DSQRT(DBLE(N)))
C
          CALL SAGPLOT(SAGARRAY1,SAGARRAY2,
     1    SAGARRAY3,XMIN,XMAX,ZMIN,ZMAX,NNN,N,ISURF)
          RETURN
      END
C SUB SAGPLOT.FOR
      SUBROUTINE SAGPLOT(SAGARRAY1,SAGARRAY2,SAGARRAY3,XMIN
     1,XMAX,ZMIN,ZMAX,NNN,N,ISURF)
C
          IMPLICIT NONE
C
!        LOGICAL ITSFLAT
C
          INTEGER I,N,NNN,II,J,JJ,ISURF
          INTEGER ALLOERR
C
          REAL*8 SAGARRAY1,SAGARRAY2,SAGARRAY3,XMIN,XMAX
     1    ,ZMIN,ZMAX,XNORM,ZNORM
C
          REAL XPLT,YPLT,FPLT
C
          DIMENSION FPLT(:,:),XPLT(:),YPLT(:)
          DIMENSION SAGARRAY1(1:N),SAGARRAY2(1:N),SAGARRAY3(1:N)
C
          ALLOCATABLE :: FPLT,YPLT,XPLT
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
          DEALLOCATE(XPLT,YPLT,FPLT,STAT=ALLOERR)
          ALLOCATE(XPLT(NNN),YPLT(NNN),FPLT(NNN,NNN),
     1    STAT=ALLOERR)
          FPLT(1:NNN,1:NNN)=0.0
          XNORM=0.5D0
          IF((XMAX-XMIN).NE.0.0D0) XNORM=(XMAX-XMIN)/2.0D0
          ZNORM=1.0D0
          IF((ZMAX-ZMIN).NE.0.0D0) ZNORM=ZMAX-ZMIN
          NNN=INT(DSQRT(DBLE(N)))
          DO I=1,NNN
              XPLT(I)=SNGL(SAGARRAY1(I)/(XNORM))
          END DO
          II=1
          JJ=1
          DO I=1,NNN
              YPLT(II)=SNGL(SAGARRAY2(JJ)/(XNORM))
              II=II+1
              JJ=JJ+NNN
          END DO
          II=1
          DO J=1,NNN
              DO I=1,NNN
                  FPLT(I,J)=SNGL(SAGARRAY3(II)/(ZNORM))
                  II=II+1
              END DO
          END DO
C
          CALL FFT4(NNN,XPLT,YPLT,FPLT,XMIN,XMAX,ZMIN,ZMAX,ISURF)
          DEALLOCATE(XPLT,YPLT,FPLT,STAT=ALLOERR)
          RETURN
      END
      SUBROUTINE FFT4(NNN,XPLT,YPLT,FPLT,XMIN,XMAX,ZMIN,ZMAX,ISURF)
          IMPLICIT NONE
!      REAL SPACER
          INTEGER ROT,I,J,ALLOERR,NNN,ISURF
C
          REAL*8 XMIN,XMAX,ZMIN,ZMAX
C
          REAL FPLT,XPLT,YPLT,FTF1
C
          LOGICAL ROTSAGFL
C
          COMMON/SAGFLROT/ROTSAGFL
C
          DIMENSION FTF1(:,:),FPLT(NNN,NNN),XPLT(NNN),YPLT(NNN)
          ALLOCATABLE :: FTF1
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          INCLUDE 'dathgr.inc'
          DEALLOCATE(FTF1,STAT=ALLOERR)
          ALLOCATE(FTF1(NNN,NNN),STAT=ALLOERR)
          ROT=0
          IF(ROTSAGFL) ROT=90
C
          DO I=1,NNN
              DO J=1,NNN
                  FTF1(I,J)=FPLT(NNN+1-I,J)
              END DO
          END DO
          DO J=1,NNN
              DO I=1,NNN
                  FPLT(I,J)=FTF1(I,J)
              END DO
          END DO
C     WE NEED TO REFLECT IN THE Y-DIRECTION
          DO I=1,NNN
              DO J=1,NNN
                  FTF1(I,J)=FPLT(I,NNN+1-J)
              END DO
          END DO
          DO J=1,NNN
              DO I=1,NNN
                  FPLT(I,J)=FTF1(I,J)
              END DO
          END DO
          IF(ROT.EQ.90) THEN
              DO J=1,NNN
                  DO I=1,NNN
                      FTF1(I,J)=FPLT(NNN+1-J,I)
                  END DO
              END DO
              DO J=1,NNN
                  DO I=1,NNN
                      FPLT(I,J)=FTF1(I,J)
                  END DO
              END DO
          END IF
          DO I=1,NNN
              DO J=1,NNN
                  FPLT(I,J)=(FPLT(I,J))
              END DO
          END DO
          DO I=1,NNN
              DO J=1,NNN
                  FPLT(I,J)=FPLT(I,J)
              END DO
          END DO
          DO I=1,NNN
              XPLT(I)=((XPLT(I))*1500.0)+1500.0
              YPLT(I)=((YPLT(I))*1500.0)+1500.0
          END DO
          DO I=1,NNN
              DO J=1,NNN
                  FPLT(I,J)=(FPLT(I,J))*2000.0
              END DO
          END DO
C
C     NOW FUNCTION GOES FROM 0 TO 2000 AND XPLT AND YPLT GO FROM
C     0 TO +2000 EACH
C
C     THE PLOT WILL BE SEEN IN ORTHOGRAPHIC PROJECTION, XPLT ACROSS THE SCREEN,
C     YPLT INTO THE SCREEN AT 45 DEG EL AND AZ AND FPLT UP ON THE SCREEN
C
          CALL PLOTSAGFL(NNN,XPLT,YPLT,FPLT,ROT
     1    ,XMIN,XMAX,ZMIN,ZMAX,ISURF)
C
          DEALLOCATE(FTF1,STAT=ALLOERR)
          RETURN
      END
      SUBROUTINE PLOTSAGFL(NNN,XPLT,YPLT,FPLT
     1,ROT,XMIN,XMAX,ZMIN,ZMAX,ISURF)
          USE GLOBALS
          IMPLICIT NONE
C     F IS THE FUNTION FPLT(X,Y),XPLT AND YPLT ARE THE POINT COORDINATES
C     NNN=NUMBER OF POINTS IN THE CAPFN FILE
C
          CHARACTER B*80,UNN*9,BLNOTE*80,BL20*20,NNTT1*100,CRANGE*15
     1    ,TMY*8,DTY*10,CCRANGE*11,ICRANGE*3
C
          INTEGER NNN,ROT,NT1ANG,NT1SIZ,ALLOERR,IRANGE
     1    ,K,II,IIX,IIY,IIXM1,IIYM1,IPN,H,ISURF

          REAL*8 XTEST1,XTEST2,YTEST1,YTEST2,SLOPE
     1    ,XMIN,XMAX,ZMIN,ZMAX
C
          REAL FPLT,XPLT,YPLT
C
          DIMENSION XPLT(NNN),YPLT(NNN),FPLT(NNN,NNN),
     1    H(:)
C
!      LOGICAL ERROR1
C
          INTEGER COLPAS
C
          INTEGER I,J
          REAL*8 XV,YV,RANGE1
          REAL*8 XX,YY,XY2
          DIMENSION XX(:),YY(:),XY2(:)
          REAL F_JK,X_JK,Y_JK
          DIMENSION F_JK(:,:),X_JK(:),Y_JK(:)
          ALLOCATABLE :: XX,YY,F_JK,X_JK,Y_JK,XY2,H
          INCLUDE 'datmai.inc'
          INCLUDE 'dathgr.inc'
          INCLUDE 'datlen.inc'
C
C     DO THE PLOTTING OF THE EXTENT
          IF(SYSTEM1(6).EQ.1.0D0) UNN='inch     '
          IF(SYSTEM1(6).EQ.2.0D0) UNN='cm       '
          IF(SYSTEM1(6).EQ.3.0D0) UNN='mm       '
          IF(SYSTEM1(6).EQ.4.0D0) UNN='meter    '
C     UNITS ARE NOW SET
C
          DEALLOCATE(XX,YY,F_JK,X_JK,Y_JK,XY2,H,STAT=ALLOERR)
          ALLOCATE(XX(NNN),YY(NNN),XY2(NNN),
     1    F_JK(NNN,NNN),X_JK(NNN),Y_JK(NNN),H(2740:7260),
     2    STAT=ALLOERR)
          H(2740:7260)=-10000
          I=NNN
          X_JK(1:I)=XPLT(1:I)
          Y_JK(1:I)=YPLT(1:I)
          XX(1:I)=0.0
          YY(1:I)=0.0
          J=NNN
          F_JK(1:I,1:J)=FPLT(1:I,1:J)
          BL20='                    '
          BLNOTE=BL20//BL20//BL20//BL20
C
C     GENERATE GRAPHIC
C     DO A PLOT NEW
          DEVTYP=1
          LOOKY=0.0D0
          LOOKX=-1.0D0
          LOOKZ=0.0D0
C     GENERATE GRAPHIC
          CALL PLTDEV
          GRASET=.TRUE.
          PLEXIS=.TRUE.
          NT1SIZ=1
          NT1ANG=0
          CALL MY_SETCHARASPECT(1.5,1.5)
          COLPAS=COLFRM
          CALL MY_COLTYP(COLPAS)
C
C     PLOT THE FUNCTION RIGHT HERE
C     STARTING AT -Y, PLOT A ROW FROM -X TO X
C     SET THE STARTING POINT
          IIX=INT(2750.0+XPLT(1))
          IIY=INT(2000.0+FPLT(1,1))
          CALL MY_PLOT(IIX,IIY,0,0,-10,10010,-10,7010)
          IPN=1

          DO J=1,NNN
C     LOAD THE XX AND YY ARRAYS FOR A SPECIFIC J VALUE
              DO K=1,NNN
                  XX(K)=XPLT(K)
                  YY(K)=FPLT(K,J)
              END DO
C     NOW PLOTTING STEPS A INTEGER 10 AND THERE ARE GOING TO BE
C     100 POINTS PER LINE
              DO I=1,3000
                  XV=DBLE(I-1)
                  YV=0.0D0
                  DO II=1,NNN-1
                      XTEST1=XX(II)
                      XTEST2=XX(II+1)
                      YTEST1=YY(II)
                      YTEST2=YY(II+1)
                      IF(XV.GE.XTEST1.AND.XV.LE.XTEST2) THEN
C     CALC A VALUE FOR YV AND RETURN
                          IF((XTEST2-XTEST1).NE.0.0D0) THEN
                              SLOPE=(YTEST2-YTEST1)/(XTEST2-XTEST1)
                              YV=(SLOPE*(XV-XTEST1))+YTEST1
                          ELSE
C     SLOPE IS VERTICAL, TAKE THE AVERAGE Y VALUE
                              YV=(YTEST2+YTEST1)/2.0D0
                          END IF
                          GO TO 2001
                      END IF
                  END DO
 2001             CONTINUE
                  IIXM1=IIX
                  IIYM1=IIY
                  IIX=INT((2750+XV)+(0.5*YPLT(J)))
                  IIY=INT((2000+YV)+(0.5*YPLT(J)))
C     IIX IS IN THE RANGE 2750 TO 7250
                  IPN=0
                  IF((IIY).GT.H(IIX)) THEN
                      IF(IPN.EQ.0.AND.I.NE.1.AND.IIYM1.LT.IIY.AND.H(IIX).GT.-10000
     1                .AND.IIYM1.LT.H(IIXM1))
     1                CALL MY_PLOT(IIX,H(IIX),0,0,-10,10010,-10,7010)
                      H(IIX)=(IIY)
                      IPN=1
                  ELSE
C     IIY LESS THAN OR EQUAL TO H(IIX)
                      IPN=0
                  END IF
                  IF(I.EQ.1) IPN=0
                  CALL MY_PLOT(IIX,IIY,IPN,0,-10,10010,-10,7010)
                  IF(IPN.EQ.0.AND.I.EQ.1) IPN=1
              END DO
              IPN=0
          END DO

C
C
          IF(ROT.EQ.90) THEN
C     XAXIS

              CALL MY_PLOT(2550,2300,0,0,0,10000,0,7000)
              CALL MY_PLOT(4250,4000,1,0,0,10000,0,7000)

C     XAXIS LABEL
              CALL MY_JUSTSTRING(2250,1500,'+X'
     1        ,0,2,3)
C     YAXIS

              CALL MY_PLOT(2750,2000,0,0,0,10000,0,7000)
              CALL MY_PLOT(6032,2000,1,0,0,10000,0,7000)

C     YAXIS LABEL
              CALL MY_JUSTSTRING(6182,1910,'+Y'
     1        ,0,2,3)
          END IF
          IF(ROT.EQ.0) THEN
C     YAXIS

              CALL MY_PLOT(5750,2000,0,0,0,10000,0,7000)
              CALL MY_PLOT(7450,3700,1,0,0,10000,0,7000)

C     YAXIS LABEL
              CALL MY_JUSTSTRING(7550,3800,'+Y'
     1        ,0,2,3)
C     XAXIS

              CALL MY_PLOT(2750,2000,0,0,0,10000,0,7000)
              CALL MY_PLOT(6032,2000,1,0,0,10000,0,7000)

C     XAXIS LABEL
              CALL MY_JUSTSTRING(6182,1910,'+X'
     1        ,0,2,3)
          END IF
C     ZAXIS

          CALL MY_PLOT(7250,3500,0,0,0,10000,0,7000)
          CALL MY_PLOT(7250,5500,1,0,0,10000,0,7000)

C
C     Z AXIS TIC MARKS, OPD

          CALL MY_PLOT(7250,3500,0,0,0,10000,0,7000)
          CALL MY_PLOT(7450,3500,1,0,0,10000,0,7000)

C
          RANGE1=ZMIN
          WRITE(B,102) RANGE1
          READ(B,202) CCRANGE
          NNTT1=CCRANGE//' '//UNN
          CALL MY_JUSTSTRING(7550,3460,NNTT1(1:21),0,1,3)

          CALL MY_PLOT(7250,3700,0,0,0,10000,0,7000)
          CALL MY_PLOT(7350,3700,1,0,0,10000,0,7000)
          CALL MY_PLOT(7250,3900,0,0,0,10000,0,7000)
          CALL MY_PLOT(7350,3900,1,0,0,10000,0,7000)
          CALL MY_PLOT(7250,4100,0,0,0,10000,0,7000)
          CALL MY_PLOT(7350,4100,1,0,0,10000,0,7000)
          CALL MY_PLOT(7250,4300,0,0,0,10000,0,7000)
          CALL MY_PLOT(7350,4300,1,0,0,10000,0,7000)
          CALL MY_PLOT(7250,4500,0,0,0,10000,0,7000)
          CALL MY_PLOT(7450,4500,1,0,0,10000,0,7000)

          RANGE1=(ZMAX+ZMIN)/2.0D0
          WRITE(B,102) RANGE1
          READ(B,202) CCRANGE
          NNTT1=CCRANGE//' '//UNN
          CALL MY_JUSTSTRING(7550,4460,NNTT1(1:21),0,1,3,3)

          CALL MY_PLOT(7250,4700,0,0,0,10000,0,7000)
          CALL MY_PLOT(7350,4700,1,0,0,10000,0,7000)
          CALL MY_PLOT(7250,4900,0,0,0,10000,0,7000)
          CALL MY_PLOT(7350,4900,1,0,0,10000,0,7000)
          CALL MY_PLOT(7250,5100,0,0,0,10000,0,7000)
          CALL MY_PLOT(7350,5100,1,0,0,10000,0,7000)
          CALL MY_PLOT(7250,5300,0,0,0,10000,0,7000)
          CALL MY_PLOT(7350,5300,1,0,0,10000,0,7000)
          CALL MY_PLOT(7250,5500,0,0,0,10000,0,7000)
          CALL MY_PLOT(7450,5500,1,0,0,10000,0,7000)

          RANGE1=ZMAX
          WRITE(B,102) RANGE1
          READ(B,202) CCRANGE
          NNTT1=CCRANGE//' '//UNN
          CALL MY_JUSTSTRING(7550,5460,NNTT1(1:21),0,1,3)
C     Z AXIS LABLE
          IRANGE=SURF
          WRITE(B,103) IRANGE
          READ(B,203) ICRANGE
          CALL MY_JUSTSTRING(6500,5750,' (Surface Sag.)'
     2    ,0,1,3,3)
C
          CALL PLOTBOX
C
C     NOW FOR PLOT ANNOTATION
          COLPAS=COLLBL
          CALL MY_COLTYP(COLPAS)
C     DO THE PLOTTING OF THE LENS IDENTIFIER
C     AT X=200, Y=500
          IF(STMPT) CALL MYTIME(TMY)
          IF(STMPD) CALL MYDATE(DTY)
          IF(.NOT.STMPT.AND..NOT.STMPD) NNTT1=LI
          IF(STMPT.AND.STMPD) NNTT1=TMY//' '//DTY//LI
          IF(STMPT.AND..NOT.STMPD) NNTT1=TMY//' '//LI
          IF(.NOT.STMPT.AND.STMPD) NNTT1=DTY//LI
          IF(NNTT1.NE.BLNOTE) THEN
              CALL MY_JUSTSTRING(200,650,NNTT1(1:80),NT1ANG,NT1SIZ,3)
          ELSE
C     LI BLANK, NOT ACTION
          END IF
          IRANGE=ISURF
          WRITE(B,103) IRANGE
          READ(B,203) ICRANGE
          IF(ROT.EQ.0) CALL MY_JUSTSTRING(200,300,
     1    'Surface Sag. Plot, Surface No. '//ICRANGE,
     1     0,2,3)
          IF(ROT.EQ.90) CALL MY_JUSTSTRING(200,300,
     1    'Surface Sag. Plot-ROTATED, Surface No. '//ICRANGE,
     1     0,2,3)
C
C
C     NOW WRITE = "VALUE" UNN
          RANGE1=DBLE((XMAX-XMIN))
          WRITE(B,101) RANGE1
          READ(B,200) CRANGE
          NNTT1='SURFACE EXTENT = '//CRANGE//'x'
     1    //CRANGE//' '//UNN
C
          CALL MY_JUSTSTRING(200,1200,NNTT1(1:59),NT1ANG,NT1SIZ,3)
C
          IF(DF2.EQ.1) THEN
              SAVE_KDP(1)=SAVEINPT(1)
              INPUT='DRAW'
              CALL PROCES
              REST_KDP(1)=RESTINPT(1)
          END IF
101       FORMAT(1PG15.8)
102       FORMAT(1F11.4)
200       FORMAT(A15)
202       FORMAT(A11)
103       FORMAT(I3)
203       FORMAT(A3)
          DEALLOCATE(XX,YY,X_JK,Y_JK,F_JK,XY2,H,STAT=ALLOERR)
          RETURN
      END
