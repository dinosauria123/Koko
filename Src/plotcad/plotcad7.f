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

C       SEVENTH FILE OF PLOT/CAD ROUTINES

C SUB ROT4.FOR
      SUBROUTINE ROT4(JJ,CLPDAT,M1,M2,M3,M4,M5)
C
          IMPLICIT NONE
C
C       THIS ROUTINE DETERMINES THE CENTER OF ROTATION FOR PLOT LOOK
C     AND PLOT VIEW FOR THE PLOT COBS COMMAND
C
C     IF SEVERAL SURFACES ARE ASKED FOR, ROTATION POINTS ARE
C     CALCULATED USING GLOBAL VERTEX DATA. FOR ONE SURFACE,
C     THE ROTATION POINT IS SET AT THE CENTER ON THE X AND Y
C     SURFACE COBS.
C
          REAL*8 X,Y,Z,X1,Y1,Z1,X2,Y2,Z2,X3,Y3,Z3,X4,Y4,Z4
     3    ,XMAXIX,XMINIX,YMAXIY,YMINIY,ZMAXIZ,ZMINIZ,XA,XB,YA,YB,ZA,ZB
C
          INTEGER JJ,I,M1,M2,M3,M4,M5
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
          REAL*8 CLPDAT
          DIMENSION CLPDAT(M1:M2,M3,M1:M4,M5)
C
          STASUR=INT(W1)
          STPSUR=INT(W2)
          IF(STASUR.NE.STPSUR) THEN
              IF(.NOT.ROTSET) THEN
                  DO I=STASUR,STPSUR
                      X=VERTEX(1,I)
                      Y=VERTEX(2,I)
                      Z=VERTEX(3,I)
                      IF(I.EQ.STASUR) THEN
                          XMINIX=X
                          XMAXIX=X
                          YMINIY=Y
                          YMAXIY=Y
                          ZMINIZ=Z
                          ZMAXIZ=Z
                      ELSE
                      END IF
                      IF(X.LE.XMINIX) XMINIX=X
                      IF(X.GT.XMAXIX) XMAXIX=X
                      IF(Y.LE.YMINIY) YMINIY=Y
                      IF(Y.GT.YMAXIY) YMAXIY=Y
                      IF(Z.LE.ZMINIZ) ZMINIZ=Z
                      IF(Z.GT.ZMAXIZ) ZMAXIZ=Z
                  END DO
                  XROT=(XMAXIX+XMINIX)/2.0D0
                  YROT=(YMAXIY+YMINIY)/2.0D0
                  ZROT=(ZMAXIZ+ZMINIZ)/2.0D0
                  ROTSET=.TRUE.
              ELSE
C     ROTSET IS TRUE AND XROT,YROT AND ZROT HAVE ALREADY BEEN DONE
              END IF
          ELSE
C     STASUR SAME AS STPSUR, KEY OFF COBS END POINTS
              IF(.NOT.ROTSET) THEN
                  I=STASUR
C
                  X1=CLPDAT(0,1,I,JJ)
                  Y1=CLPDAT(0,2,I,JJ)
                  Z1=CLPDAT(0,3,I,JJ)
                  X2=CLPDAT(180,1,I,JJ)
                  Y2=CLPDAT(180,2,I,JJ)
                  Z2=CLPDAT(180,3,I,JJ)
                  XA=(X2+X1)/2.0D0
                  YA=(Y2+Y1)/2.0D0
                  ZA=(Z2+Z1)/2.0D0
                  X3=(CLPDAT(22,1,I,JJ)+CLPDAT(90,1,I,JJ))/2.0D0
                  Y3=(CLPDAT(22,2,I,JJ)+CLPDAT(90,2,I,JJ))/2.0D0
                  Z3=(CLPDAT(22,3,I,JJ)+CLPDAT(90,3,I,JJ))/2.0D0
                  X4=(CLPDAT(62,1,I,JJ)+CLPDAT(270,1,I,JJ))/2.0D0
                  Y4=(CLPDAT(62,2,I,JJ)+CLPDAT(270,2,I,JJ))/2.0D0
                  Z4=(CLPDAT(62,3,I,JJ)+CLPDAT(270,3,I,JJ))/2.0D0
                  XB=(X4+X3)/2.0D0
                  YB=(Y4+Y3)/2.0D0
                  ZB=(Z4+Z3)/2.0D0
                  XROT=(XB+XA)/2.0D0
                  YROT=(YB+YA)/2.0D0
                  ZROT=(ZB+ZA)/2.0D0
C
                  ROTSET=.TRUE.
              ELSE
C     ROTSET IS TRUE AND XROT,YROT AND ZROT HAVE ALREADY BEEN DONE
              END IF
          END IF
          RETURN
      END
C SUB ROT3.FOR
      SUBROUTINE ROT3(JJ,CLPDAT,M1,M2,M3,M4,M5)
C
          IMPLICIT NONE
C
C       THIS ROUTINE DETERMINES THE CENTER OF ROTATION FOR PLOT LOOK
C     AND PLOT VIEW FOR THE PLOT CLAP COMMAND
C
C     IF SEVERAL SURFACES ARE ASKED FOR, ROTATION POINTS ARE
C     CALCULATED USING GLOBAL VERTEX DATA. FOR ONE SURFACE,
C     THE ROTATION POINT IS SET AT THE CENTER ON THE X AND Y
C     SURFACE CLAPS.
C
          REAL*8 X,Y,Z,X1,Y1,Z1,X2,Y2,Z2,X3,Y3,Z3,X4,Y4,Z4
     3    ,XMAXIX,XMINIX,YMAXIY,YMINIY,ZMAXIZ,ZMINIZ,XA,XB,YA,YB,ZA,ZB
C
          INTEGER I,JJ,M1,M2,M3,M4,M5
C
          REAL*8 CLPDAT
          DIMENSION CLPDAT(M1:M2,M3,M1:M4,M5)
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
          STASUR=INT(W1)
          STPSUR=INT(W2)
          IF(STASUR.NE.STPSUR) THEN
              IF(.NOT.ROTSET) THEN
                  DO I=STASUR,STPSUR
                      X=VERTEX(1,I)
                      Y=VERTEX(2,I)
                      Z=VERTEX(3,I)
                      IF(I.EQ.STASUR) THEN
                          XMINIX=X
                          XMAXIX=X
                          YMINIY=Y
                          YMAXIY=Y
                          ZMINIZ=Z
                          ZMAXIZ=Z
                      ELSE
                      END IF
                      IF(X.LE.XMINIX) XMINIX=X
                      IF(X.GT.XMAXIX) XMAXIX=X
                      IF(Y.LE.YMINIY) YMINIY=Y
                      IF(Y.GT.YMAXIY) YMAXIY=Y
                      IF(Z.LE.ZMINIZ) ZMINIZ=Z
                      IF(Z.GT.ZMAXIZ) ZMAXIZ=Z
                  END DO
                  XROT=(XMAXIX+XMINIX)/2.0D0
                  YROT=(YMAXIY+YMINIY)/2.0D0
                  ZROT=(ZMAXIZ+ZMINIZ)/2.0D0
                  ROTSET=.TRUE.
              ELSE
C     ROTSET IS TRUE AND XROT,YROT AND ZROT HAVE ALREADY BEEN DONE
              END IF
          ELSE
C     STASUR SAME AS STPSUR, KEY OFF CLAP END POINTS
              IF(.NOT.ROTSET) THEN
                  I=STASUR
C
                  X1=CLPDAT(0,1,I,JJ)
                  Y1=CLPDAT(0,2,I,JJ)
                  Z1=CLPDAT(0,3,I,JJ)
                  X2=CLPDAT(180,1,I,JJ)
                  Y2=CLPDAT(180,2,I,JJ)
                  Z2=CLPDAT(180,3,I,JJ)
                  XA=(X2+X1)/2.0D0
                  YA=(Y2+Y1)/2.0D0
                  ZA=(Z2+Z1)/2.0D0
                  X3=(CLPDAT(22,1,I,JJ)+CLPDAT(90,1,I,JJ))/2.0D0
                  Y3=(CLPDAT(22,2,I,JJ)+CLPDAT(90,2,I,JJ))/2.0D0
                  Z3=(CLPDAT(22,3,I,JJ)+CLPDAT(90,3,I,JJ))/2.0D0
                  X4=(CLPDAT(62,1,I,JJ)+CLPDAT(270,1,I,JJ))/2.0D0
                  Y4=(CLPDAT(62,2,I,JJ)+CLPDAT(270,2,I,JJ))/2.0D0
                  Z4=(CLPDAT(62,3,I,JJ)+CLPDAT(270,3,I,JJ))/2.0D0
                  XB=(X4+X3)/2.0D0
                  YB=(Y4+Y3)/2.0D0
                  ZB=(Z4+Z3)/2.0D0
                  XROT=(XB+XA)/2.0D0
                  YROT=(YB+YA)/2.0D0
                  ZROT=(ZB+ZA)/2.0D0
C
                  ROTSET=.TRUE.
              ELSE
C     ROTSET IS TRUE AND XROT,YROT AND ZROT HAVE ALREADY BEEN DONE
              END IF
          END IF
          RETURN
      END


C SUB RIMS.FOR
      SUBROUTINE RIMS
C
          IMPLICIT NONE
C
C       THIS PROGRAM CONTROLS THE "FANS" COMMAND
C
          REAL*8 F1X,F1Y,F2X,F2Y,F3X,F3Y
C
          COMMON/FANFOB/F1X,F1Y,F2X,F2Y,F3X,F3Y
C
          CHARACTER RIMWQ*8
C
          REAL*8 RIMW1
C
          INTEGER DFLAG
C
          LOGICAL FANEXT,RIM
C
          COMMON/RINSHT/RIM
C
          COMMON/FANEXI/FANEXT
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          INCLUDE 'datmac.inc'
C
          FANEXT=.FALSE.
          RIM=.TRUE.
          SAVE_KDP(1)=SAVEINPT(1)
C
          CALL PLTRST
C
C     W1 IS FOR SSI
C
C     QUALIFIER WORDS:
C         YFAN
C         XFAN
C         NFAN
C         PFAN
C         XYFAN
C         YXFAN
C         YOPD
C         XOPD
C         NOPD
C         POPD
C         XYOPD
C         YLA
C         XLA
C         NLA
C         PLA
C         XYLA
C         YXLA
C         YCD
C         XCD
C         NCD
C         PCD
C         XYCD
C         YXCD
C
          IF(SQ.EQ.0) THEN
              SQ=1
              WQ='XYFAN   '
          END IF
          IF(WQ.NE.'YFAN'.AND.WQ.NE.'XFAN'.AND.WQ.NE.'YOPD'.AND.
     1    WQ.NE.'XOPD'.AND.WQ.NE.'XCD'.AND.WQ.NE.'YCD'.AND.
     1    WQ.NE.'XLA'.AND.WQ.NE.'YLA'.AND.
     1    WQ.NE.'NFAN'.AND.WQ.NE.'PFAN'.AND.WQ.NE.'NOPD'.AND.
     1    WQ.NE.'POPD'.AND.WQ.NE.'NCD'.AND.WQ.NE.'PCD'.AND.
     1    WQ.NE.'NLA'.AND.WQ.NE.'PLA'.AND.
     1    WQ.NE.'XYFAN'.AND.WQ.NE.'YXFAN'.AND.
     1    WQ.NE.'XYOPD'.AND.
     1    WQ.NE.'XYCD'.AND.
     1    WQ.NE.'YXCD'.AND.
     1    WQ.NE.'XYLA'.AND.WQ.NE.'YXLA') THEN
              OUTLYNE= 'FOR "FANS"'
              CALL SHOWIT(1)
              OUTLYNE='THE ONLY VALID QUALIFIER WORDS ARE:'
              CALL SHOWIT(1)
              OUTLYNE='"XFAN"  - FOR X-RAY,  "YFAN"  - FOR Y-RAY  (FANS)'
              CALL SHOWIT(1)
              OUTLYNE='"XYFAN" - FOR XY-RAY, "YXFAN" - FOR YX-RAY (FANS)'
              CALL SHOWIT(1)
              OUTLYNE='"NFAN"  - FOR N-RAY,  "PFAN"  - FOR P-RAY  (FANS)'
              CALL SHOWIT(1)
              OUTLYNE='"XOPD"  - FOR X-OPD,  "YOPD"  - FOR Y-OPD  (FANS)'
              CALL SHOWIT(1)
              OUTLYNE='"XYOPD" - FOR XY-OPD                       (FANS)'
              CALL SHOWIT(1)
              OUTLYNE='"NOPD"  - FOR N-OPD,  "POPD"  - FOR P-OPD  (FANS)'
              CALL SHOWIT(1)
              OUTLYNE='"XCD"   - FOR X-CD,   "YCD"   - FOR Y-CD   (FANS)'
              CALL SHOWIT(1)
              OUTLYNE='"XYCD"  - FOR XY-CD,  "YXCD"  - FOR YX-CD  (FANS)'
              CALL SHOWIT(1)
              OUTLYNE='"NCD"   - FOR N-CD,   "PCD"   - FOR P-CD   (FANS)'
              CALL SHOWIT(1)
              OUTLYNE='"XLA"   - FOR X-LA,   "YLA"   - FOR Y-LA   (FANS)'
              CALL SHOWIT(1)
              OUTLYNE='"XYLA"  - FOR XY-LA,  "YXLA"  - FOR YX-LA  (FANS)'
              CALL SHOWIT(1)
              OUTLYNE='"NLA"   - FOR N-LA,   "PLA"   - FOR P-LA   (FANS)'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(STI.EQ.1) THEN
              OUTLYNE= '"FANS" PERFOMS AUTOMATED FAN PLOTTING'
              CALL SHOWIT(1)
              RETURN
          END IF
          IF(S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
              OUTLYNE= '"FANS" TAKES NO NUMERIC WORD #3 THROUGH #5 INPUT'
              CALL SHOWIT(1)
              OUTLYNE= 'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(DF1.EQ.0.AND.W1.LE.0.0D0) THEN
              OUTLYNE= 'THE "SSI" VALUE MUST BE GREATER THAN 0.0'
              CALL SHOWIT(1)
              OUTLYNE= 'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C     DO A PLOT NEW
          FANEXT=.FALSE.
          RIM=.TRUE.
          SAVE_KDP(1)=SAVEINPT(1)
C
          CALL PLTRST
C
C
C
          FANEXT=.FALSE.
          RIM=.TRUE.
          DEVTYP=1
          LOOKY=0.0D0
          LOOKX=-1.0D0
          LOOKZ=0.0D0
          OUTLYNE='RAY FAN DATA BEING GENERATED'
          CALL SHOWIT(1)
          OUTLYNE='PLEASE WAIT...'
          CALL SHOWIT(1)
          CALL PLTDEV

          IF(DF1.EQ.1) SSI=0.0D0
          IF(DF1.EQ.1) SSIFLG=.TRUE.
          IF(DF1.EQ.0) SSI=W1/2.0D0
          IF(DF1.EQ.0) SSIFLG=.FALSE.
          IF(DF2.EQ.1.OR.DF2.EQ.0.AND.W2.EQ.0.0D0) DFLAG=0
          IF(DF2.EQ.0.AND.W2.NE.0.0D0) DFLAG=1
C     SSI DONE
          RIMWQ=WQ
          RIMW1=W1
          RIM=.TRUE.
          GRASET=.TRUE.
C
C     SET UP 3 FANS
C
          FANNUM=3
          YFOB1=F1Y
          YFOB2=F2Y
          YFOB3=F3Y
          XFOB1=F1X
          XFOB2=F2X
          XFOB3=F3X
C
C     THREE FANS SET UP
C
C     NOW SET REFERENCE WAVELENGTH
C
          REFWV=INT(SYSTEM1(11))
          REST_KDP(1)=RESTINPT(1)
C
C     SET TYPES
          SAVE_KDP(1)=SAVEINPT(1)
          IF(RIMWQ.EQ.'XFAN') INPUT='PLTXFAN'
          IF(RIMWQ.EQ.'YFAN') INPUT='PLTYFAN'
          IF(RIMWQ.EQ.'NFAN') INPUT='PLTNFAN'
          IF(RIMWQ.EQ.'PFAN') INPUT='PLTPFAN'
          IF(RIMWQ.EQ.'XYFAN') INPUT='PLTXYFAN'
          IF(RIMWQ.EQ.'YXFAN') INPUT='PLTYXFAN'
          IF(RIMWQ.EQ.'XOPD') INPUT='PLTXFAN OPD'
          IF(RIMWQ.EQ.'YOPD') INPUT='PLTYFAN OPD'
          IF(RIMWQ.EQ.'NOPD') INPUT='PLTNFAN OPD'
          IF(RIMWQ.EQ.'POPD') INPUT='PLTPFAN OPD'
          IF(RIMWQ.EQ.'XYOPD') INPUT='PLTXYFAN OPD'
          IF(RIMWQ.EQ.'XCD') INPUT='PLTXFAN CD'
          IF(RIMWQ.EQ.'YCD') INPUT='PLTYFAN CD'
          IF(RIMWQ.EQ.'NCD') INPUT='PLTNFAN CD'
          IF(RIMWQ.EQ.'PCD') INPUT='PLTPFAN CD'
          IF(RIMWQ.EQ.'XYCD') INPUT='PLTXYFAN CD'
          IF(RIMWQ.EQ.'YXCD') INPUT='PLTYXFAN CD'
          IF(RIMWQ.EQ.'XLA') INPUT='PLTXFAN LA'
          IF(RIMWQ.EQ.'YLA') INPUT='PLTYFAN LA'
          IF(RIMWQ.EQ.'NLA') INPUT='PLTNFAN LA'
          IF(RIMWQ.EQ.'PLA') INPUT='PLTPFAN LA'
          IF(RIMWQ.EQ.'XYLA') INPUT='PLTXYFAN LA'
          IF(RIMWQ.EQ.'YXLA') INPUT='PLTYXFAN LA'
          CALL PROCES
          REST_KDP(1)=RESTINPT(1)
C
C     DO A PLOTFANS GO

          FANEXT=.FALSE.
          RIM=.TRUE.
          MSG=.FALSE.
          SAVE_KDP(1)=SAVEINPT(1)
          INPUT='PLOTFANS GO'
          CALL PROCES
          REST_KDP(1)=RESTINPT(1)
          MSG=.TRUE.
C
C     DO A DRAW
C
          if (RIMWQ.eq.'XCD'.or.RIMWQ.eq.'YCD'.or.RIMWQ.eq.'XYCD'
     &    .or.RIMWQ.eq.'YXCD'.or. RIMWQ.eq.'NCD'.or. RIMWQ.eq.'PCD') then
              call settwocolors
          else
              call setthreecolors
          end if

          IF(DFLAG.EQ.0) THEN
              SAVE_KDP(1)=SAVEINPT(1)
              INPUT='DRAW'
              CALL PROCES
              REST_KDP(1)=RESTINPT(1)


          END IF
          RETURN
      END


C SUB CHSIZE.FOR
      SUBROUTINE CHSIZE
C
          IMPLICIT NONE
C
C       THIS ROUTINE DOES THE PLOT CHNOTE, PLOT CHSYM AND
C       AND PLOT CHLAB COMMANDS AT THE CMD LEVEL
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
          IF(WQ.EQ.'CHNOTE') THEN
C       CHECK SYNTAX
              IF(SST.EQ.1) THEN
                  OUTLYNE=
     1            '"PLOT CHNOTE" TAKES NO STRING INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
                  OUTLYNE=
     1            '"PLOT CHNOTE" TAKES NO NUMERIC WORD #3, #4 OR #5 INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(STI.EQ.1) THEN
                  WRITE(OUTLYNE,100)NTSIZ
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,200)NTANG
                  CALL SHOWIT(1)
                  RETURN
              END IF
C       ASSIGN VALUES
              IF(DF1.EQ.1.AND.DF2.EQ.1) THEN
                  OUTLYNE=
     1            '"PLOT CHNOTE" REQUIRES SOME EXPLICIT NUMERIC'
                  CALL SHOWIT(1)
                  OUTLYNE=
     1            'WORD #1 OR #2 INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(DF1.EQ.1) W1=1.0D0
              IF(DF2.EQ.1) W2=0.0D0
              IF(INT(W1).LT.1.OR.INT(W1).GT.9) THEN
                  OUTLYNE=
     1            'NOTE SIZE VALUES CAN ONLY BE 1,2,3,4,5,6,7,8 AND 9'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(DABS(W2).GT.360.0D0) THEN
                  OUTLYNE=
     1            'NOTE ANGLES CAN NOT BE GREATER THAN 360.0 DEGREES'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              NTSIZ=INT(W1)
              NTANG=INT(W2)
          ELSE
C       NOT PLOT CHNOTE
          END IF
C
C
          IF(WQ.EQ.'CHSYM') THEN
C       CHECK SYNTAX
              IF(SST.EQ.1) THEN
                  OUTLYNE=
     1            '"PLOT CHSYM" TAKES NO STRING INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
                  OUTLYNE=
     1            '"PLOT CHSYM" TAKES NO NUMERIC WORD #3, #4 OR #5 INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(STI.EQ.1) THEN
                  WRITE(OUTLYNE,300)SYMSIZ
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,400)SYMANG
                  CALL SHOWIT(1)
                  RETURN
              END IF
C       ASSIGN VALUES
              IF(DF1.EQ.1.AND.DF2.EQ.1) THEN
                  OUTLYNE=
     1            '"PLOT CHSYM" REQUIRES SOME EXPLICIT NUMERIC'
                  CALL SHOWIT(1)
                  OUTLYNE=
     1            'WORD #1 OR #2 INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(DF1.EQ.1) W1=1.0D0
              IF(DF2.EQ.1) W2=0.0D0
              IF(INT(W1).LT.1.OR.INT(W1).GT.9) THEN
                  OUTLYNE=
     1            'SYMBOL SIZE VALUES CAN ONLY BE 1,2,3,4,5,6,7,8 AND 9'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(DABS(W2).GT.360.0D0) THEN
                  OUTLYNE=
     1            'SYMBOL ANGLES CAN NOT BE GREATER THAN 360.0 DEGREES'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              SYMSIZ=INT(W1)
              SYMANG=INT(W2)
          END IF
C
          IF(WQ.EQ.'CHLAB') THEN
C       CHECK SYNTAX
              IF(SST.EQ.1) THEN
                  OUTLYNE=
     1            '"PLOT CHLAB" TAKES NO STRING INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
                  OUTLYNE=
     1            '"PLOT CHLAB" TAKES NO NUMERIC WORD #3, #4 OR #5 INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(STI.EQ.1) THEN
                  WRITE(OUTLYNE,500)LABSIZ
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,600)LABANG
                  CALL SHOWIT(1)
                  RETURN
              END IF
C       ASSIGN VALUES
              IF(DF1.EQ.1.AND.DF2.EQ.1) THEN
                  OUTLYNE=
     1            '"PLOT CHLAB" REQUIRES SOME EXPLICIT NUMERIC'
                  CALL SHOWIT(1)
                  OUTLYNE=
     1            'WORD #1 OR #2 INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(DF1.EQ.1) W1=1.0D0
              IF(DF2.EQ.1) W2=0.0D0
              IF(INT(W1).LT.1.OR.INT(W1).GT.9) THEN
                  OUTLYNE=
     1            'LABEL SIZE VALUES CAN ONLY BE 1,2,3,4,5,6,7,8 AND 9'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(DABS(W2).GT.360.0D0) THEN
                  OUTLYNE=
     1            'LABEL ANGLES CAN NOT BE GREATER THAN 360.0 DEGREES'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              LABSIZ=INT(W1)
              LABANG=INT(W2)
          END IF
C
 100      FORMAT('CURRENT NOTE SIZE  IS = ',I1)
 200      FORMAT('CURRENT NOTE ANGLE IS = ',I4)
 300      FORMAT('CURRENT SYMBOL SIZE  IS = ',I1)
 400      FORMAT('CURRENT SYMBOL ANGLE IS = ',I4)
 500      FORMAT('CURRENT LABEL SIZE  IS = ',I1)
 600      FORMAT('CURRENT LABEL ANGLE IS = ',I4)
          RETURN
      END
C SUB CAO1.FOR
C
C     THIS ROUTINE IS CALLED BY PLTCLP
C
      SUBROUTINE CAO1(X,Y,ANGLE,I,AN2,JJ,XID,YID,IIRUN,CLPTYPE,ZDELZ
     1,MULTX,MULTY,GAMGAM)
C
C     THIS SUBROUTINE CALCULATES THE X AND Y CLAP LIMIT VALUES
C     FOR SURFACE III WHICH LIE ON A LINE WHICH EXTENDS FROM THE
C     SURFACE VERTEX OUT TO THE CLAP EDGE. "ANGLE" IS MEASURED
C     COUNTER-CLOCKWISE FROM THE X AXIS TO THE Y AXIS OF THE LOCAL
C     SURFACE VERTEX COORDINATE SYSTEM. 0 DEGREES POINTS IN THE +X
C     DIRECTION. "ANGLE" IS PASSED TO THIS ROUTINE IN RADIAN MEASURE.
C     JJ IS ALWAYS 1 EXCEPT IF THE INNER CIRCULAR BOUNDRY OF A
C     TYPE 18 GRAZING INCIDENCE SURFACE MUST BE DRAWN.
C
          IMPLICIT NONE
C
          INTEGER JJ,CAFLG,I,IIRUN,J,CLPTYPE
C
          LOGICAL DOIT,ISAIR,ISAIR2,POSDIR,OLDIN,NEWIN,ISITIN,ISITINI
C
          EXTERNAL ISAIR,ISAIR2,ISITIN,ISITINI
C
          REAL*8 X,Y,GAM,THETA1,THETA2,THETA3,THETA4,XID,YID,GAMGAM
     1    ,A,B,RAD,XR,YR,ANGLE,AN2,XCOR1,XCOR2,XCOR3,XCOR4,DA,DB,ZDELZ
     2    ,YCOR1,YCOR2,YCOR3,YCOR4,A1TEST,A2TEST,QTEST,Q,JK_AA,JK_BB
     3    ,XA,YA,XB,YB,M,RADIUS,CBX,CBY,SGNB,CEE,ZEE,KAPPA,RHO2,RHO
     2    ,JK_CC,CLPLCX(0:499),CLPLCY(0:499),XOLDX,YOLDY,MULTX,MULTY
C
          COMMON/CLPLOC/CLPLCX,CLPLCY
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
C
          ZDELZ=0.0D0
C
C       SET FLAG CAFLG
          CAFLG=INT(ALENS(9,I))
          GAM=0.0D0
C
C       CAFLG HAS BEEN INITIALIZED
C
C       CAFLG=0 NO CLAP, USE PARAXIAL DATA
          IF(ALENS(34,I).NE.18.0D0) THEN
C     NOT TYPE 18
              IF(CAFLG.EQ.0) THEN
C
C     COORDINATES FOR THE SPECIFIED ANGLE ARE TAKEN FROM THE YZ
C     PLANE PARAXIAL DATA. CLAP TILT OR DECENTRATION APPLIES
                  RAD=DABS(PXTRAY(1,I))+DABS(PXTRAY(5,I))
                  X=(DCOS(ANGLE)*RAD)+CLPLCX(I)
                  Y=(DSIN(ANGLE)*RAD)+CLPLCY(I)
                  XID=(DCOS(ANGLE)*RAD)+CLPLCX(I)
                  YID=(DSIN(ANGLE)*RAD)+CLPLCY(I)
              ELSE
C       CAFLG NOT 0
              END IF
C
C       CAFLG=1 CIRCULAR CLAP
              IF(CAFLG.EQ.1) THEN
                  IF(IIRUN.EQ.1) RAD=DABS(ALENS(10,I))
                  IF(IIRUN.EQ.2) RAD=DABS(ALENS(11,I))
                  IF(CLPTYPE.EQ.1) THEN
                      X=(DCOS(ANGLE)*RAD)+ALENS(13,I)+MULTX
                      Y=(DSIN(ANGLE)*RAD)+ALENS(12,I)+MULTY
                      XID=(DCOS(ANGLE)*RAD)+ALENS(13,I)+MULTX
                      YID=(DSIN(ANGLE)*RAD)+ALENS(12,I)+MULTY
                  ELSE
                      X=(DCOS(ANGLE)*DABS(ALENS(11,I)))+ALENS(13,I)+MULTX
                      Y=(DSIN(ANGLE)*DABS(ALENS(11,I)))+ALENS(12,I)+MULTY
                      XID=(DCOS(ANGLE)*DABS(ALENS(11,I)))+ALENS(13,I)+MULTX
                      YID=(DSIN(ANGLE)*DABS(ALENS(11,I)))+ALENS(12,I)+MULTY
                  END IF
              ELSE
C       CAFLG NOT 1
              END IF
C       CAFLG=5, CLAP POLY
              IF(CAFLG.EQ.5) THEN
                  OLDIN=.TRUE.
                  NEWIN=.TRUE.
                  XOLDX=0.0D0
                  YOLDY=0.0D0
                  X=0.0D0
                  Y=0.0D0
                  DO J=1,99999

                      XOLDX=X
                      YOLDY=Y
                      X=(DBLE(J)*0.01D0*ALENS(10,I))*DCOS(ANGLE)
                      Y=(DBLE(J)*0.01D0*ALENS(10,I))*DSIN(ANGLE)
                      NEWIN=ISITIN(X,Y,I)
                      X=X+ALENS(13,I)+MULTX
                      Y=Y+ALENS(12,I)+MULTY
                      IF(OLDIN.AND..NOT.NEWIN) THEN
                          X=(X+XOLDX)/2.0D0
                          Y=(Y+YOLDY)/2.0D0
                          XID=X
                          YID=Y
                          GO TO 10
                      ELSE
                      END IF
                  END DO
              ELSE
C     CAFLG NOT 5
              END IF
C       CAFLG=6, CLAP IPOLY
              IF(CAFLG.EQ.6) THEN
                  OLDIN=.TRUE.
                  NEWIN=.TRUE.
                  XOLDX=0.0D0
                  YOLDY=0.0D0
                  X=0.0D0
                  Y=0.0D0
                  DO J=1,99999

                      XOLDX=X
                      YOLDY=Y
                      X=(DBLE(J)*0.01D0*ALENS(14,I))*DCOS(ANGLE)
                      Y=(DBLE(J)*0.01D0*ALENS(14,I))*DSIN(ANGLE)
                      NEWIN=ISITINI(X,Y,I)
                      X=X+ALENS(13,I)+MULTX
                      Y=Y+ALENS(12,I)+MULTY
                      IF(OLDIN.AND..NOT.NEWIN) THEN
                          X=(X+XOLDX)/2.0D0
                          Y=(Y+YOLDY)/2.0D0
                          XID=X
                          YID=Y
                          GO TO 10
                      ELSE
                      END IF
                  END DO
              ELSE
C     CAFLG NOT 6
              END IF
 10           CONTINUE
C       CAFLG=2, CLAP RECT
              IF(CAFLG.EQ.2) THEN
C     CORNER 1 HAS COORDINATES
                  XCOR1=+DABS(ALENS(11,I))
                  YCOR1=+DABS(ALENS(10,I))
                  IF(DABS(YCOR1).LE.1.0D-15.AND.DABS(XCOR1).LE.1.0D-15) THEN
                      THETA1=0.0D0
                  ELSE
                      THETA1=DATAN2(YCOR1,XCOR1)
                  END IF
                  IF(THETA1.LT.0.0D0) THETA1=THETA1+(TWOPII)
C     CORNER 2 HAS COORDINATES
                  XCOR2=-DABS(ALENS(11,I))
                  YCOR2=+DABS(ALENS(10,I))
                  IF(DABS(YCOR2).LE.1.0D-15.AND.DABS(XCOR2).LE.1.0D-15) THEN
                      THETA2=0.0D0
                  ELSE
                      THETA2=DATAN2(YCOR2,XCOR2)
                  END IF
                  IF(THETA2.LT.0.0D0) THETA2=THETA2+(TWOPII)
C     CORNER 3 HAS COORDINATES
                  XCOR3=-DABS(ALENS(11,I))
                  YCOR3=-DABS(ALENS(10,I))
                  IF(DABS(YCOR3).LE.1.0D-15.AND.DABS(XCOR3).LE.1.0D-15) THEN
                      THETA3=0.0D0
                  ELSE
                      THETA3=DATAN2(YCOR3,XCOR3)
                  END IF
                  IF(THETA3.LT.0.0D0) THETA3=THETA3+(TWOPII)
C     CORNER 4 HAS COORDINATES
                  XCOR4=+DABS(ALENS(11,I))
                  YCOR4=-DABS(ALENS(10,I))
                  IF(DABS(YCOR4).LE.1.0D-15.AND.DABS(XCOR4).LE.1.0D-15) THEN
                      THETA4=0.0D0
                  ELSE
                      THETA4=DATAN2(YCOR4,XCOR4)
                  END IF
                  IF(THETA4.LT.0.0D0) THETA4=THETA4+(TWOPII)
C
                  IF(ANGLE.GE.0.0D0.AND.ANGLE.LE.THETA1) THEN
                      X=DABS(ALENS(11,I))
                      Y=X*DTAN(ANGLE)
                      XID=DABS(ALENS(11,I))
                      YID=X*DTAN(ANGLE)
                  ELSE
                  END IF
                  IF(ANGLE.GT.THETA1.AND.ANGLE.LE.THETA2) THEN
                      Y=DABS(ALENS(10,I))
                      X=Y*DTAN((PII/2.0D0)-ANGLE)
                      YID=DABS(ALENS(10,I))
                      XID=Y*DTAN((PII/2.0D0)-ANGLE)
                  ELSE
                  END IF
                  IF(ANGLE.GT.THETA2.AND.ANGLE.LE.THETA3) THEN
                      X=-DABS(ALENS(11,I))
                      Y=X*DTAN(ANGLE)
                      XID=-DABS(ALENS(11,I))
                      YID=X*DTAN(ANGLE)
                  ELSE
                  END IF
                  IF(ANGLE.GT.THETA3.AND.ANGLE.LE.THETA4) THEN
                      Y=-DABS(ALENS(10,I))
                      X=Y*DTAN((PII/2.0D0)-ANGLE)
                      YID=-DABS(ALENS(10,I))
                      XID=Y*DTAN((PII/2.0D0)-ANGLE)
                  ELSE
                  END IF
                  IF(ANGLE.GT.THETA4.AND.ANGLE.LE.(TWOPII)) THEN
                      X=DABS(ALENS(11,I))
                      Y=X*DTAN(ANGLE)
                      XID=DABS(ALENS(11,I))
                      YID=X*DTAN(ANGLE)
                  ELSE
                  END IF

                  IF(ANGLE.LE.THETA1.AND.AN2.GE.THETA1) THEN
                      X=XCOR1
                      Y=YCOR1
                      XID=XCOR1
                      YID=YCOR1
                  ELSE
C     POINT NOT AT CORNER1
                  END IF
                  IF(ANGLE.LE.THETA2.AND.AN2.GE.THETA2) THEN
                      X=XCOR2
                      Y=YCOR2
                      XID=XCOR2
                      YID=YCOR2
                  ELSE
C     POINT NOT AT CORNER2
                  END IF
                  IF(ANGLE.LE.THETA3.AND.AN2.GE.THETA3) THEN
                      X=XCOR3
                      Y=YCOR3
                      XID=XCOR3
                      YID=YCOR3
                  ELSE
C     POINT NOT AT CORNER3
                  END IF
                  IF(ANGLE.LE.THETA4.AND.AN2.GE.THETA4) THEN
                      X=XCOR4
                      Y=YCOR4
                      XID=XCOR4
                      YID=YCOR4
                  ELSE
C     POINT NOT AT CORNER4
                  END IF
C
C     APPLY THE CLAP TILT ANGLE AND THE CLAP DECENTRATIONS
                  GAM=(PII/180.0D0)*(ALENS(15,I)+GAMGAM)
                  XR=(X*DCOS(GAM))-(Y*DSIN(GAM))
                  YR=(Y*DCOS(GAM))+(X*DSIN(GAM))
                  X=XR+ALENS(13,I)+MULTX
                  Y=YR+ALENS(12,I)+MULTY
                  XID=XR+ALENS(13,I)+MULTX
                  YID=YR+ALENS(12,I)+MULTY
C
              ELSE
C     CAFLG NOT 2
              END IF
C
C       CAFLG=3, ELLIPTICAL CLAP
              IF(CAFLG.EQ.3) THEN
C       X-SEMI-MAJOR AXIS IS ALENS(11,I)
C       Y-SEMI-MAJOR AXIS IS ALENS(10,I)
                  A=(ALENS(11,I))
                  B=(ALENS(10,I))
                  RAD=((A**2)*(B**2))/
     1            (((B**2)*((DCOS(ANGLE))**2))+((A**2)*((DSIN(ANGLE)**2))))
                  RAD=DSQRT(RAD)
                  X=RAD*DCOS(ANGLE)
                  Y=RAD*DSIN(ANGLE)
                  XID=RAD*DCOS(ANGLE)
                  YID=RAD*DSIN(ANGLE)
C
C     APPLY THE CLAP TILT ANGLE AND THE CLAP DECENTRATIONS
                  GAM=(PII/180.0D0)*(ALENS(15,I)+GAMGAM)
                  XR=(X*DCOS(GAM))-(Y*DSIN(GAM))
                  YR=(Y*DCOS(GAM))+(X*DSIN(GAM))
                  X=XR+ALENS(13,I)+MULTX
                  Y=YR+ALENS(12,I)+MULTY
                  XID=XR+ALENS(13,I)+MULTX
                  YID=YR+ALENS(12,I)+MULTY
C
              ELSE
C     CAFLG NOT 3
              END IF
C       CAFLG=4, CLAP RCTK
              IF(CAFLG.EQ.4) THEN
C     CORNER 1 HAS COORDINATES
                  XCOR1=+DABS(ALENS(11,I))
                  YCOR1=+DABS(ALENS(10,I))
                  IF(DABS(YCOR1).LE.1.0D-15.AND.DABS(XCOR1).LE.1.0D-15) THEN
                      THETA1=0.0D0
                  ELSE
                      THETA1=DATAN2(YCOR1,XCOR1)
                  END IF
                  IF(THETA1.LT.0.0D0) THETA1=THETA1+(TWOPII)
C     CORNER 2 HAS COORDINATES
                  XCOR2=-DABS(ALENS(11,I))
                  YCOR2=+DABS(ALENS(10,I))
                  IF(DABS(YCOR2).LE.1.0D-15.AND.DABS(XCOR2).LE.1.0D-15) THEN
                      THETA2=0.0D0
                  ELSE
                      THETA2=DATAN2(YCOR2,XCOR2)
                  END IF
                  IF(THETA2.LT.0.0D0) THETA2=THETA2+(TWOPII)
C     CORNER 3 HAS COORDINATES
                  XCOR3=-DABS(ALENS(11,I))
                  YCOR3=-DABS(ALENS(10,I))
                  IF(DABS(YCOR3).LE.1.0D-15.AND.DABS(XCOR3).LE.1.0D-15) THEN
                      THETA3=0.0D0
                  ELSE
                      THETA3=DATAN2(YCOR3,XCOR3)
                  END IF
                  IF(THETA3.LT.0.0D0) THETA3=THETA3+(TWOPII)
C     CORNER 4 HAS COORDINATES
                  XCOR4=+DABS(ALENS(11,I))
                  YCOR4=-DABS(ALENS(10,I))
                  IF(DABS(YCOR4).LE.1.0D-15.AND.DABS(XCOR4).LE.1.0D-15) THEN
                      THETA4=0.0D0
                  ELSE
                      THETA4=DATAN2(YCOR4,XCOR4)
                  END IF
                  IF(THETA4.LT.0.0D0) THETA4=THETA4+(TWOPII)
C
                  IF(ANGLE.GE.0.0D0.AND.ANGLE.LE.THETA1) THEN
                      X=DABS(ALENS(11,I))
                      Y=X*DTAN(ANGLE)
                      XID=DABS(ALENS(11,I))
                      YID=X*DTAN(ANGLE)
                  ELSE
                  END IF
                  IF(ANGLE.GT.THETA1.AND.ANGLE.LE.THETA2) THEN
                      Y=DABS(ALENS(10,I))
                      X=Y*DTAN((PII/2.0D0)-ANGLE)
                      YID=DABS(ALENS(10,I))
                      XID=Y*DTAN((PII/2.0D0)-ANGLE)
                  ELSE
                  END IF
                  IF(ANGLE.GT.THETA2.AND.ANGLE.LE.THETA3) THEN
                      X=-DABS(ALENS(11,I))
                      Y=X*DTAN(ANGLE)
                      XID=-DABS(ALENS(11,I))
                      YID=X*DTAN(ANGLE)
                  ELSE
                  END IF
                  IF(ANGLE.GT.THETA3.AND.ANGLE.LE.THETA4) THEN
                      Y=-DABS(ALENS(10,I))
                      X=Y*DTAN((PII/2.0D0)-ANGLE)
                      YID=-DABS(ALENS(10,I))
                      XID=Y*DTAN((PII/2.0D0)-ANGLE)
                  ELSE
                  END IF
                  IF(ANGLE.GT.THETA4.AND.ANGLE.LE.(TWOPII)) THEN
                      X=DABS(ALENS(11,I))
                      Y=X*DTAN(ANGLE)
                      XID=DABS(ALENS(11,I))
                      YID=X*DTAN(ANGLE)
                  ELSE
                  END IF
C
C     HERE, THE RACETRACK HAS BEEN DRAWN AS A RECTANGLE. NOW
C     DOES THIS POINT NEED TO BE MODIFIED DUE TO A RACETRACK
C     CORNER. THIS MODIFICATION IS DONE FOR:
C
C     ANGLES FROM 0 TO 90
C     THEN FROM 90 TO 180
C     THEN FROM 180 TO 270
C     THEN FROM 270 TO 360
                  RADIUS=DABS(ALENS(14,I))
C
C     THESE ARE FOUR AREAS OF ADJUSTMENT
C
C     AREA 1
C
                  IF(ANGLE.GE.0.0D0.AND.ANGLE.LE.(PII/2.0D0)) THEN
                      IF(DABS(YCOR1-RADIUS).LE.1.0D-15.AND.
     1                DABS(XCOR1).LE.1.0D-15) THEN
                          A1TEST=0.0D0
                      ELSE
                          A1TEST=DATAN2((YCOR1-RADIUS),XCOR1)
                      END IF
                      IF(DABS(YCOR1).LE.1.0D-15.AND.
     1                DABS(XCOR1-RADIUS).LE.1.0D-15) THEN
                          A2TEST=0.0D0
                      ELSE
                          A2TEST=DATAN2(YCOR1,(XCOR1-RADIUS))
                      END IF
                      IF(A2TEST.LE.0.0D0) A2TEST=A2TEST+(TWOPII)
                      IF(ANGLE.GE.A1TEST.AND.ANGLE.LE.A2TEST) THEN
C     CALCULATE A NEW SET OF X AND Y VALUES
C     CENTER OF CIRCLE WITH RESPECT TO CENTER OF THE CLAP
                          CBX=XCOR1-RADIUS
                          CBY=YCOR1-RADIUS
C     SLOPE OF LINE FROM CLAP CENTER
                          M=DTAN(ANGLE)
                          JK_AA=1.0D0+(M**2)
                          JK_BB=-(2.0D0*CBX)-(2.0D0*CBY*M)
                          JK_CC=(CBX**2)+(CBY**2)-(RADIUS**2)
                          QTEST=(JK_BB**2)-(4.0D0*JK_AA*JK_CC)
                          IF(QTEST.GE.0.0D0) THEN
C     SOLUTION EXISTS, ADJUSTMENT IS NECESSARY
                              IF(JK_AA.EQ.0.0D0) THEN
C     ONLY ONE SOLUTION EXISTS
                                  X=-JK_CC/JK_BB
                                  Y=M*X
                                  XID=-JK_CC/JK_BB
                                  YID=M*X
                              ELSE
C     MORE THAN ONE SOLUTION EXISTS
                                  SGNB=JK_BB/DABS(JK_BB)
                                  Q=-0.5*(JK_BB+(SGNB*DSQRT(QTEST)))
                                  XA=Q/JK_AA
                                  XB=JK_CC/Q
                                  YA=M*XA
                                  YB=M*XB
                                  DA=(XA**2)+(YA**2)
                                  DB=(XB**2)+(YB**2)
                                  IF(DA.GE.DB) THEN
                                      X=XA
                                      Y=YA
                                      XID=XA
                                      YID=YA
                                  ELSE
                                      X=XB
                                      Y=YB
                                      XID=XB
                                      YID=YB
                                  END IF
                              END IF
                          ELSE
C     NO SOLUTION(S) EXISTS, NO ADJUSTMENT IS NECESSARY
                          END IF
                      ELSE
C     NO CALCULATION NEEDED
                      END IF
                  ELSE
C     NO CORNER VALUES NEEDED TO BE CALCULATED
                  END IF
C
C     AREA 2
C
                  IF(ANGLE.GE.(PII/2.0D0).AND.ANGLE.LE.PII) THEN
                      IF(DABS(YCOR2).LE.1.0D-15.AND.
     1                DABS(XCOR2+RADIUS).LE.1.0D-15) THEN
                          A1TEST=0.0D0
                      ELSE
                          A1TEST=DATAN2(YCOR2,(XCOR2+RADIUS))
                      END IF
                      IF(DABS(YCOR2-RADIUS).LE.1.0D-15.AND.
     1                DABS(XCOR2).LE.1.0D-15) THEN
                          A2TEST=0.0D0
                      ELSE
                          A2TEST=DATAN2((YCOR2-RADIUS),XCOR2)
                      END IF
                      IF(A1TEST.LE.0.0D0) A1TEST=A1TEST+(TWOPII)
                      IF(A2TEST.LE.0.0D0) A2TEST=A2TEST+(TWOPII)
                      IF(ANGLE.GE.A1TEST.AND.ANGLE.LE.A2TEST) THEN
C     CALCULATE A NEW SET OF X AND Y VALUES
C     CENTER OF CIRCLE WITH RESPECT TO CENTER OF THE CLAP
                          CBX=XCOR2+RADIUS
                          CBY=YCOR2-RADIUS
C     SLOPE OF LINE FROM CLAP CENTER
                          M=DTAN(ANGLE)
                          JK_AA=1.0D0+(M**2)
                          JK_BB=-(2.0D0*CBX)-(2.0D0*CBY*M)
                          JK_CC=(CBX**2)+(CBY**2)-(RADIUS**2)
                          QTEST=(JK_BB**2)-(4.0D0*JK_AA*JK_CC)
                          IF(QTEST.GE.0.0D0) THEN
C     SOLUTION EXISTS, ADJUSTMENT IS NECESSARY
                              IF(JK_AA.EQ.0.0D0) THEN
C     ONLY ONE SOLUTION EXISTS
                                  X=-JK_CC/JK_BB
                                  Y=M*X
                                  XID=-JK_CC/JK_BB
                                  YID=M*X
                              ELSE
C     MORE THAN ONE SOLUTION EXISTS
                                  SGNB=JK_BB/DABS(JK_BB)
                                  Q=-0.5*(JK_BB+(SGNB*DSQRT(QTEST)))
                                  XA=Q/JK_AA
                                  XB=JK_CC/Q
                                  YA=M*XA
                                  YB=M*XB
                                  DA=(XA**2)+(YA**2)
                                  DB=(XB**2)+(YB**2)
                                  IF(DA.GE.DB) THEN
                                      X=XA
                                      Y=YA
                                      XID=XA
                                      YID=YA
                                  ELSE
                                      X=XB
                                      Y=YB
                                      XID=XB
                                      YID=YB
                                  END IF
                              END IF
                          ELSE
C     NO SOLUTION(S) EXISTS, NO ADJUSTMENT IS NECESSARY
                          END IF
                      ELSE
C     NO CALCULATION NEEDED
                      END IF
                  ELSE
C     NO CORNER VALUES NEEDED TO BE CALCULATED
                  END IF
C
C     AREA 3
C
                  IF(ANGLE.GE.PII.AND.ANGLE.LE.((3.0D0*PII)/2.0D0)) THEN
                      IF(DABS(YCOR3+RADIUS).LE.1.0D-15.AND.
     1                DABS(XCOR3).LE.1.0D-15) THEN
                          A1TEST=0.0D0
                      ELSE
                          A1TEST=DATAN2((YCOR3+RADIUS),XCOR3)
                      END IF
                      IF(DABS(YCOR3).LE.1.0D-15.AND.
     1                DABS(XCOR3+RADIUS).LE.1.0D-15) THEN
                          A2TEST=0.0D0
                      ELSE
                          A2TEST=DATAN2(YCOR3,(XCOR3+RADIUS))
                      END IF
                      IF(A1TEST.LE.0.0D0) A1TEST=A1TEST+(TWOPII)
                      IF(A2TEST.LE.0.0D0) A2TEST=A2TEST+(TWOPII)
                      IF(ANGLE.GE.A1TEST.AND.ANGLE.LE.A2TEST) THEN
C     CALCULATE A NEW SET OF X AND Y VALUES
C     CENTER OF CIRCLE WITH RESPECT TO CENTER OF THE CLAP
                          CBX=XCOR3+RADIUS
                          CBY=YCOR3+RADIUS
C     SLOPE OF LINE FROM CLAP CENTER
                          M=DTAN(ANGLE)
                          JK_AA=1.0D0+(M**2)
                          JK_BB=-(2.0D0*CBX)-(2.0D0*CBY*M)
                          JK_CC=(CBX**2)+(CBY**2)-(RADIUS**2)
                          QTEST=(JK_BB**2)-(4.0D0*JK_AA*JK_CC)
                          IF(QTEST.GE.0.0D0) THEN
C     SOLUTION EXISTS, ADJUSTMENT IS NECESSARY
                              IF(JK_AA.EQ.0.0D0) THEN
C     ONLY ONE SOLUTION EXISTS
                                  X=-JK_CC/JK_BB
                                  Y=M*X
                                  XID=-JK_CC/JK_BB
                                  YID=M*X
                              ELSE
C     MORE THAN ONE SOLUTION EXISTS
                                  SGNB=JK_BB/DABS(JK_BB)
                                  Q=-0.5*(JK_BB+(SGNB*DSQRT(QTEST)))
                                  XA=Q/JK_AA
                                  XB=JK_CC/Q
                                  YA=M*XA
                                  YB=M*XB
                                  DA=(XA**2)+(YA**2)
                                  DB=(XB**2)+(YB**2)
                                  IF(DA.GE.DB) THEN
                                      X=XA
                                      Y=YA
                                      XID=XA
                                      YID=YA
                                  ELSE
                                      X=XB
                                      Y=YB
                                      XID=XB
                                      YID=YB
                                  END IF
                              END IF
                          ELSE
C     NO SOLUTION(S) EXISTS, NO ADJUSTMENT IS NECESSARY
                          END IF
                      ELSE
C     NO CALCULATION NEEDED
                      END IF
                  ELSE
C     NO CORNER VALUES NEEDED TO BE CALCULATED
                  END IF
C
C     AREA 4
C
                  IF(ANGLE.GE.((3.0D0*PII)/2.0D0)) THEN
                      IF(DABS(YCOR4).LE.1.0D-15.AND.
     1                DABS(XCOR4-RADIUS).LE.1.0D-15) THEN
                          A1TEST=0.0D0
                      ELSE
                          A1TEST=DATAN2(YCOR4,(XCOR4-RADIUS))
                      END IF
                      IF(DABS(YCOR4+RADIUS).LE.1.0D-15.AND.
     1                DABS(XCOR4).LE.1.0D-15) THEN
                          A2TEST=0.0D0
                      ELSE
                          A2TEST=DATAN2((YCOR4+RADIUS),XCOR4)
                      END IF
                      IF(A1TEST.LE.0.0D0) A1TEST=A1TEST+(TWOPII)
                      IF(A2TEST.LE.0.0D0) A2TEST=A2TEST+(TWOPII)
                      IF(A2TEST.LE.PII)   A2TEST=A2TEST+(TWOPII)
                      IF(ANGLE.GE.A1TEST.AND.ANGLE.LE.A2TEST) THEN
C     CALCULATE A NEW SET OF X AND Y VALUES
C     CENTER OF CIRCLE WITH RESPECT TO CENTER OF THE CLAP
                          CBX=XCOR4-RADIUS
                          CBY=YCOR4+RADIUS
C     SLOPE OF LINE FROM CLAP CENTER
                          M=DTAN(ANGLE)
                          JK_AA=1.0D0+(M**2)
                          JK_BB=-(2.0D0*CBX)-(2.0D0*CBY*M)
                          JK_CC=(CBX**2)+(CBY**2)-(RADIUS**2)
                          QTEST=(JK_BB**2)-(4.0D0*JK_AA*JK_CC)
                          IF(QTEST.GE.0.0D0) THEN
C     SOLUTION EXISTS, ADJUSTMENT IS NECESSARY
                              IF(JK_AA.EQ.0.0D0) THEN
C     ONLY ONE SOLUTION EXISTS
                                  X=-JK_CC/JK_BB
                                  Y=M*X
                                  XID=-JK_CC/JK_BB
                                  YID=M*X
                              ELSE
C     MORE THAN ONE SOLUTION EXISTS
                                  SGNB=JK_BB/DABS(JK_BB)
                                  Q=-0.5*(JK_BB+(SGNB*DSQRT(QTEST)))
                                  XA=Q/JK_AA
                                  XB=JK_CC/Q
                                  YA=M*XA
                                  YB=M*XB
                                  DA=(XA**2)+(YA**2)
                                  DB=(XB**2)+(YB**2)
                                  IF(DA.GE.DB) THEN
                                      X=XA
                                      Y=YA
                                      XID=XA
                                      YID=YA
                                  ELSE
                                      X=XB
                                      Y=YB
                                      XID=XB
                                      YID=YB
                                  END IF
                              END IF
                          ELSE
C     NO SOLUTION(S) EXISTS, NO ADJUSTMENT IS NECESSARY
                          END IF
                      ELSE
C     NO CALCULATION NEEDED
                      END IF
                  ELSE
C     NO CORNER VALUES NEEDED TO BE CALCULATED
                  END IF
C
C     APPLY THE CLAP TILT ANGLE AND THE CLAP DECENTRATIONS
                  GAM=(PII/180.0D0)*(ALENS(15,I)+GAMGAM)
                  XR=(X*DCOS(GAM))-(Y*DSIN(GAM))
                  YR=(Y*DCOS(GAM))+(X*DSIN(GAM))
                  X=XR+ALENS(13,I)+MULTX
                  Y=YR+ALENS(12,I)+MULTY
                  XID=XR+ALENS(13,I)+MULTX
                  YID=YR+ALENS(12,I)+MULTY
C
              ELSE
C     CAFLG NOT 4
              END IF
C       CAFLG=1 CIRCULAR CLAP
              IF(CAFLG.EQ.1) THEN
C
                  IF(IIRUN.EQ.1) RAD=DABS(ALENS(10,I))
                  IF(IIRUN.EQ.2) RAD=DABS(ALENS(11,I))
C
C     CLAP DEC MUST BE 0
                  IF((ALENS(12,I)+MULTX).EQ.0.0D0.AND.
     1            (ALENS(13,I)+MULTY).EQ.0.0D0) THEN
                      IF(ALENS(34,I).NE.18.0D0) THEN
C
C     WE HAVE A CIRCULAR CLAP. IS THE SURFACE ONE WHICH COULD HAVE A
C     FLAT ON IT. ONLY IF SURFACE IS CONCAVE TO AIR.
C
C     IF SURFACE I IS NOT AIR AND I+1 IS AIR
C     THEN IF CV>0 THERE MAY BE A FLAT TO DO
C
                          DOIT=.FALSE.
                          DOIT=ISAIR(I,POSDIR)
                          IF(DOIT) THEN
                              IF(ALENS(1,I).GT.0.0D0.AND.POSDIR.OR.
     1                        ALENS(1,I).LT.0.0D0.AND..NOT.POSDIR) THEN
C     ADJUST POINT
                                  RAD=DABS(ALENS(11,I))
                                  IF(ALENS(1,I).GT.0.0D0) ZDELZ=-DABS(ALENS(14,I))
                                  IF(ALENS(1,I).LT.0.0D0) ZDELZ=DABS(ALENS(14,I))
                              END IF
                          END IF
C
C     IF SURFACE I IS AIR AIR AND I+1 IS NOT AIR
C     THEN IF CV<0 THERE MAY BE A FLAT TO DO
C
                          DOIT=.FALSE.
                          DOIT=ISAIR2(I,POSDIR)
                          IF(DOIT) THEN
                              IF(ALENS(1,I).LT.0.0D0.AND.POSDIR.OR.
     1                        ALENS(1,I).GT.0.0D0.AND..NOT.POSDIR) THEN
C     ADJUST POINT
                                  RAD=DABS(ALENS(11,I))
                                  IF(ALENS(1,I).GT.0.0D0) ZDELZ=-DABS(ALENS(14,I))
                                  IF(ALENS(1,I).LT.0.0D0) ZDELZ=DABS(ALENS(14,I))
                              END IF
                          END IF
                      END IF
                      XID=(DCOS(ANGLE)*RAD)
                      YID=(DSIN(ANGLE)*RAD)
                  ELSE
C     DEC CLAP, NO FLAT CALC
                  END IF
              ELSE
C       CAFLG NOT 1
              END IF
          ELSE
C     TYPE 18 SPECIAL SURFACE AND IT IS "ON"
C     CALCULATE THE RADIUS OF THE CLAP
C     CEE IS THE CURVATURE CV (ALENS(1,I))
              CEE=ALENS(1,I)
C     KAPPA IS THE CONIC CONSTANT CC (ALENS(2,I))
              KAPPA=ALENS(2,I)
C
C     ZEE IS THE Z POSITION FOR WHICH A RHO IS DESIRED
C     FIRST THE FORE OR FRONT POSITION
              IF(JJ.EQ.1) ZEE=FTFL01(1,I)
              IF(JJ.EQ.2) ZEE=FTFL01(2,I)
              RHO2=(((2.0D0*ZEE)-(CEE*(KAPPA+1.0D0)*(ZEE**2)))/CEE)
              IF(RHO2.LT.0.0D0) THEN
                  IF(JJ.EQ.1)
     1            OUTLYNE=
     1            'UN-REALISTIC FRONT Z-POSITION IN TYPE 18 SPECIAL SURFACE'
                  IF(JJ.EQ.2)
     1            OUTLYNE=
     1            'UN-REALISTIC REAR Z-POSITION IN TYPE 18 SPECIAL SURFACE'
                  CALL SHOWIT(1)
                  OUTLYNE='NO CLAP COULD BE DRAWN'
                  CALL SHOWIT(1)
                  RETURN
              END IF
              RHO=DSQRT(RHO2)
              X=(DCOS(ANGLE)*RHO)
              Y=(DSIN(ANGLE)*RHO)
              XID=(DCOS(ANGLE)*RHO)
              YID=(DSIN(ANGLE)*RHO)
          END IF
          RETURN
      END
C SUB CAO3.FOR
C
C     THIS ROUTINE IS CALLED BY PLTFOOT
C
      SUBROUTINE CAO3(X,Y,ANGLE,I,AN2,JJ,XID,YID,MULTX,MULTY)
C
C     THIS SUBROUTINE CALCULATES THE X AND Y CLAP LIMIT VALUES
C     FOR SURFACE III WHICH LIE ON A LINE WHICH EXTENDS FROM THE
C     SURFACE VERTEX OUT TO THE CLAP EDGE. "ANGLE" IS MEASURED
C     COUNTER-CLOCKWISE FROM THE X AXIS TO THE Y AXIS OF THE LOCAL
C     SURFACE VERTEX COORDINATE SYSTEM. 0 DEGREES POINTS IN THE +X
C     DIRECTION. "ANGLE" IS PASSED TO THIS ROUTINE IN RADIAN MEASURE.
C     JJ IS ALWAYS 1 EXCEPT IF THE INNER CIRCULAR BOUNDRY OF A
C     TYPE 18 GRAZING INCIDENCE SURFACE MUST BE DRAWN.
C
          IMPLICIT NONE
C
          INTEGER JJ,CAFLG,I,J
C
          LOGICAL DOIT,ISAIR,ISAIR2,POSDIR,OLDIN,NEWIN,ISITIN,ISITINI
C
          EXTERNAL ISAIR,ISAIR2,ISITIN,ISITINI
C
          REAL*8 X,Y,GAM,THETA1,THETA2,THETA3,THETA4,XID,YID
     1    ,A,B,RAD,XR,YR,ANGLE,AN2,XCOR1,XCOR2,XCOR3,XCOR4,DA,DB
     2    ,YCOR1,YCOR2,YCOR3,YCOR4,A1TEST,A2TEST,QTEST,Q,JK_AA,JK_BB
     3    ,XA,YA,XB,YB,M,RADIUS,CBX,CBY,SGNB,CEE,ZEE,KAPPA,RHO2,RHO
     2    ,JK_CC,CLPLCX(0:499),CLPLCY(0:499),XOLDX,YOLDY,MULTX,MULTY
C
          COMMON/CLPLOC/CLPLCX,CLPLCY
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
C
C       SET FLAG CAFLG
          CAFLG=INT(ALENS(9,I))
          GAM=0.0D0
C
C       CAFLG HAS BEEN INITIALIZED
C
C       CAFLG=0 NO CLAP, USE PARAXIAL DATA
          IF(ALENS(34,I).NE.18.0D0) THEN
C     NOT TYPE 18
              IF(CAFLG.EQ.0.OR.ALENS(127,I).NE.0.0D0) THEN
C
C     COORDINATES FOR THE SPECIFIED ANGLE ARE TAKEN FROM THE YZ
C     PLANE PARAXIAL DATA. CLAP TILT OR DECENTRATION APPLIES
                  RAD=DABS(PXTRAY(1,I))+DABS(PXTRAY(5,I))
                  X=(DCOS(ANGLE)*RAD)+CLPLCX(I)
                  Y=(DSIN(ANGLE)*RAD)+CLPLCY(I)
                  XID=(DCOS(ANGLE)*RAD)+CLPLCX(I)
                  YID=(DSIN(ANGLE)*RAD)+CLPLCY(I)
              ELSE
C       CAFLG NOT 0
              END IF
C
C       CAFLG=1 CIRCULAR CLAP
              IF(CAFLG.EQ.1.AND.ALENS(127,I).EQ.0.0D0) THEN
                  IF(ALENS(10,I).LE.ALENS(11,I))  RAD=DABS(ALENS(10,I))
                  IF(ALENS(10,I).GT.ALENS(11,I))  RAD=DABS(ALENS(11,I))
                  X=(DCOS(ANGLE)*RAD)+ALENS(13,I)+MULTX
                  Y=(DSIN(ANGLE)*RAD)+ALENS(12,I)+MULTY
                  XID=(DCOS(ANGLE)*RAD)+ALENS(13,I)+MULTX
                  YID=(DSIN(ANGLE)*RAD)+ALENS(12,I)+MULTY
              ELSE
C       CAFLG NOT 1
              END IF
C       CAFLG=5, CLAP POLY
              IF(CAFLG.EQ.5.AND.ALENS(127,I).EQ.0.0D0) THEN
                  OLDIN=.TRUE.
                  NEWIN=.TRUE.
                  XOLDX=0.0D0
                  YOLDY=0.0D0
                  X=0.0D0
                  Y=0.0D0
                  DO J=1,99999

                      XOLDX=X
                      YOLDY=Y
                      X=(DBLE(J)*0.01D0*ALENS(10,I))*DCOS(ANGLE)
                      Y=(DBLE(J)*0.01D0*ALENS(10,I))*DSIN(ANGLE)
                      NEWIN=ISITIN(X,Y,I)
                      X=X+ALENS(13,I)+MULTX
                      Y=Y+ALENS(12,I)+MULTY
                      IF(OLDIN.AND..NOT.NEWIN) THEN
                          X=(X+XOLDX)/2.0D0
                          Y=(Y+YOLDY)/2.0D0
                          XID=X
                          YID=Y
                          GO TO 10
                      ELSE
                      END IF
                  END DO
              ELSE
C     CAFLG NOT 5
              END IF
C       CAFLG=6, CLAP IPOLY
              IF(CAFLG.EQ.6) THEN
                  OLDIN=.TRUE.
                  NEWIN=.TRUE.
                  XOLDX=0.0D0
                  YOLDY=0.0D0
                  X=0.0D0
                  Y=0.0D0
                  DO J=1,99999

                      XOLDX=X
                      YOLDY=Y
                      X=(DBLE(J)*0.01D0*ALENS(14,I))*DCOS(ANGLE)
                      Y=(DBLE(J)*0.01D0*ALENS(14,I))*DSIN(ANGLE)
                      NEWIN=ISITINI(X,Y,I)
                      X=X+ALENS(13,I)
                      Y=Y+ALENS(12,I)
                      IF(OLDIN.AND..NOT.NEWIN) THEN
                          X=(X+XOLDX)/2.0D0
                          Y=(Y+YOLDY)/2.0D0
                          XID=X
                          YID=Y
                          GO TO 10
                      END IF
                  END DO
              ELSE
C     CAFLG NOT 6
              END IF
 10           CONTINUE
C       CAFLG=2, CLAP RECT
              IF(CAFLG.EQ.2.AND.ALENS(127,I).EQ.0.0D0) THEN
C     CORNER 1 HAS COORDINATES
                  XCOR1=+DABS(ALENS(11,I))
                  YCOR1=+DABS(ALENS(10,I))
                  IF(DABS(YCOR1).LE.1.0D-15.AND.
     1            DABS(XCOR1).LE.1.0D-15) THEN
                      THETA1=0.0D0
                  ELSE
                      THETA1=DATAN2(YCOR1,XCOR1)
                  END IF
                  IF(THETA1.LT.0.0D0) THETA1=THETA1+(TWOPII)
C     CORNER 2 HAS COORDINATES
                  XCOR2=-DABS(ALENS(11,I))
                  YCOR2=+DABS(ALENS(10,I))
                  IF(DABS(YCOR2).LE.1.0D-15.AND.
     1            DABS(XCOR2).LE.1.0D-15) THEN
                      THETA2=0.0D0
                  ELSE
                      THETA2=DATAN2(YCOR2,XCOR2)
                  END IF
                  IF(THETA2.LT.0.0D0) THETA2=THETA2+(TWOPII)
C     CORNER 3 HAS COORDINATES
                  XCOR3=-DABS(ALENS(11,I))
                  YCOR3=-DABS(ALENS(10,I))
                  IF(DABS(YCOR3).LE.1.0D-15.AND.
     1            DABS(XCOR3).LE.1.0D-15) THEN
                      THETA3=0.0D0
                  ELSE
                      THETA3=DATAN2(YCOR3,XCOR3)
                  END IF
                  IF(THETA3.LT.0.0D0) THETA3=THETA3+(TWOPII)
C     CORNER 4 HAS COORDINATES
                  XCOR4=+DABS(ALENS(11,I))
                  YCOR4=-DABS(ALENS(10,I))
                  IF(DABS(YCOR4).LE.1.0D-15.AND.
     1            DABS(XCOR4).LE.1.0D-15) THEN
                      THETA4=0.0D0
                  ELSE
                      THETA4=DATAN2(YCOR4,XCOR4)
                  END IF
                  IF(THETA4.LT.0.0D0) THETA4=THETA4+(TWOPII)
C
                  IF(ANGLE.GE.0.0D0.AND.ANGLE.LE.THETA1) THEN
                      X=DABS(ALENS(11,I))
                      Y=X*DTAN(ANGLE)
                      XID=DABS(ALENS(11,I))
                      YID=X*DTAN(ANGLE)
                  ELSE
                  END IF
                  IF(ANGLE.GT.THETA1.AND.ANGLE.LE.THETA2) THEN
                      Y=DABS(ALENS(10,I))
                      X=Y*DTAN((PII/2.0D0)-ANGLE)
                      YID=DABS(ALENS(10,I))
                      XID=Y*DTAN((PII/2.0D0)-ANGLE)
                  ELSE
                  END IF
                  IF(ANGLE.GT.THETA2.AND.ANGLE.LE.THETA3) THEN
                      X=-DABS(ALENS(11,I))
                      Y=X*DTAN(ANGLE)
                      XID=-DABS(ALENS(11,I))
                      YID=X*DTAN(ANGLE)
                  ELSE
                  END IF
                  IF(ANGLE.GT.THETA3.AND.ANGLE.LE.THETA4) THEN
                      Y=-DABS(ALENS(10,I))
                      X=Y*DTAN((PII/2.0D0)-ANGLE)
                      YID=-DABS(ALENS(10,I))
                      XID=Y*DTAN((PII/2.0D0)-ANGLE)
                  ELSE
                  END IF
                  IF(ANGLE.GT.THETA4.AND.ANGLE.LE.(TWOPII)) THEN
                      X=DABS(ALENS(11,I))
                      Y=X*DTAN(ANGLE)
                      XID=DABS(ALENS(11,I))
                      YID=X*DTAN(ANGLE)
                  ELSE
                  END IF

                  IF(ANGLE.LE.THETA1.AND.AN2.GE.THETA1) THEN
                      X=XCOR1
                      Y=YCOR1
                      XID=XCOR1
                      YID=YCOR1
                  ELSE
C     POINT NOT AT CORNER1
                  END IF
                  IF(ANGLE.LE.THETA2.AND.AN2.GE.THETA2) THEN
                      X=XCOR2
                      Y=YCOR2
                      XID=XCOR2
                      YID=YCOR2
                  ELSE
C     POINT NOT AT CORNER2
                  END IF
                  IF(ANGLE.LE.THETA3.AND.AN2.GE.THETA3) THEN
                      X=XCOR3
                      Y=YCOR3
                      XID=XCOR3
                      YID=YCOR3
                  ELSE
C     POINT NOT AT CORNER3
                  END IF
                  IF(ANGLE.LE.THETA4.AND.AN2.GE.THETA4) THEN
                      X=XCOR4
                      Y=YCOR4
                      XID=XCOR4
                      YID=YCOR4
                  ELSE
C     POINT NOT AT CORNER4
                  END IF
C
C     APPLY THE CLAP TILT ANGLE AND THE CLAP DECENTRATIONS
                  GAM=(PII/180.0D0)*ALENS(15,I)
                  XR=(X*DCOS(GAM))-(Y*DSIN(GAM))
                  YR=(Y*DCOS(GAM))+(X*DSIN(GAM))
                  X=XR+ALENS(13,I)+MULTX
                  Y=YR+ALENS(12,I)+MULTY
                  XID=XR+ALENS(13,I)+MULTX
                  YID=YR+ALENS(12,I)+MULTY
C
              ELSE
C     CAFLG NOT 2
              END IF
C
C       CAFLG=3, ELLIPTICAL CLAP
              IF(CAFLG.EQ.3.AND.ALENS(127,I).EQ.0.0D0) THEN
C       X-SEMI-MAJOR AXIS IS ALENS(11,I)
C       Y-SEMI-MAJOR AXIS IS ALENS(10,I)
                  A=(ALENS(11,I))
                  B=(ALENS(10,I))
                  RAD=((A**2)*(B**2))/
     1            (((B**2)*((DCOS(ANGLE))**2))+((A**2)*((DSIN(ANGLE)**2))))
                  RAD=DSQRT(RAD)
                  X=RAD*DCOS(ANGLE)
                  Y=RAD*DSIN(ANGLE)
                  XID=RAD*DCOS(ANGLE)
                  YID=RAD*DSIN(ANGLE)
C
C     APPLY THE CLAP TILT ANGLE AND THE CLAP DECENTRATIONS
                  GAM=(PII/180.0D0)*ALENS(15,I)
                  XR=(X*DCOS(GAM))-(Y*DSIN(GAM))
                  YR=(Y*DCOS(GAM))+(X*DSIN(GAM))
                  X=XR+ALENS(13,I)+MULTX
                  Y=YR+ALENS(12,I)+MULTY
                  XID=XR+ALENS(13,I)+MULTX
                  YID=YR+ALENS(12,I)+MULTY
C
              ELSE
C     CAFLG NOT 3
              END IF
C       CAFLG=4, CLAP RCTK
              IF(CAFLG.EQ.4.AND.ALENS(127,I).EQ.0.0D0) THEN
C     CORNER 1 HAS COORDINATES
                  XCOR1=+DABS(ALENS(11,I))
                  YCOR1=+DABS(ALENS(10,I))
                  IF(DABS(YCOR1).LE.1.0D-15.AND.
     1            DABS(XCOR1).LE.1.0D-15) THEN
                      THETA1=0.0D0
                  ELSE
                      THETA1=DATAN2(YCOR1,XCOR1)
                  END IF
                  IF(THETA1.LT.0.0D0) THETA1=THETA1+(TWOPII)
C     CORNER 2 HAS COORDINATES
                  XCOR2=-DABS(ALENS(11,I))
                  YCOR2=+DABS(ALENS(10,I))
                  IF(DABS(YCOR2).LE.1.0D-15.AND.
     1            DABS(XCOR2).LE.1.0D-15) THEN
                      THETA2=0.0D0
                  ELSE
                      THETA2=DATAN2(YCOR2,XCOR2)
                  END IF
                  IF(THETA2.LT.0.0D0) THETA2=THETA2+(TWOPII)
C     CORNER 3 HAS COORDINATES
                  XCOR3=-DABS(ALENS(11,I))
                  YCOR3=-DABS(ALENS(10,I))
                  IF(DABS(YCOR3).LE.1.0D-15.AND.
     1            DABS(XCOR3).LE.1.0D-15) THEN
                      THETA3=0.0D0
                  ELSE
                      THETA3=DATAN2(YCOR3,XCOR3)
                  END IF
                  IF(THETA3.LT.0.0D0) THETA3=THETA3+(TWOPII)
C     CORNER 4 HAS COORDINATES
                  XCOR4=+DABS(ALENS(11,I))
                  YCOR4=-DABS(ALENS(10,I))
                  IF(DABS(YCOR4).LE.1.0D-15.AND.
     1            DABS(XCOR4).LE.1.0D-15) THEN
                      THETA4=0.0D0
                  ELSE
                      THETA4=DATAN2(YCOR4,XCOR4)
                  END IF
                  IF(THETA4.LT.0.0D0) THETA4=THETA4+(TWOPII)
C
                  IF(ANGLE.GE.0.0D0.AND.ANGLE.LE.THETA1) THEN
                      X=DABS(ALENS(11,I))
                      Y=X*DTAN(ANGLE)
                      XID=DABS(ALENS(11,I))
                      YID=X*DTAN(ANGLE)
                  ELSE
                  END IF
                  IF(ANGLE.GT.THETA1.AND.ANGLE.LE.THETA2) THEN
                      Y=DABS(ALENS(10,I))
                      X=Y*DTAN((PII/2.0D0)-ANGLE)
                      YID=DABS(ALENS(10,I))
                      XID=Y*DTAN((PII/2.0D0)-ANGLE)
                  ELSE
                  END IF
                  IF(ANGLE.GT.THETA2.AND.ANGLE.LE.THETA3) THEN
                      X=-DABS(ALENS(11,I))
                      Y=X*DTAN(ANGLE)
                      XID=-DABS(ALENS(11,I))
                      YID=X*DTAN(ANGLE)
                  ELSE
                  END IF
                  IF(ANGLE.GT.THETA3.AND.ANGLE.LE.THETA4) THEN
                      Y=-DABS(ALENS(10,I))
                      X=Y*DTAN((PII/2.0D0)-ANGLE)
                      YID=-DABS(ALENS(10,I))
                      XID=Y*DTAN((PII/2.0D0)-ANGLE)
                  ELSE
                  END IF
                  IF(ANGLE.GT.THETA4.AND.ANGLE.LE.(TWOPII)) THEN
                      X=DABS(ALENS(11,I))
                      Y=X*DTAN(ANGLE)
                      XID=DABS(ALENS(11,I))
                      YID=X*DTAN(ANGLE)
                  ELSE
                  END IF
C
C     HERE, THE RACETRACK HAS BEEN DRAWN AS A RECTANGLE. NOW
C     DOES THIS POINT NEED TO BE MODIFIED DUE TO A RACETRACK
C     CORNER. THIS MODIFICATION IS DONE FOR:
C
C     ANGLES FROM 0 TO 90
C     THEN FROM 90 TO 180
C     THEN FROM 180 TO 270
C     THEN FROM 270 TO 360
                  RADIUS=DABS(ALENS(14,I))
C
C     THESE ARE FOUR AREAS OF ADJUSTMENT
C
C     AREA 1
C
                  IF(ANGLE.GE.0.0D0.AND.ANGLE.LE.(PII/2.0D0)) THEN
                      IF(DABS(YCOR1-RADIUS).LE.1.0D-15.AND.
     1                DABS(XCOR1).LE.1.0D-15) THEN
                          A1TEST=0.0D0
                      ELSE
                          A1TEST=DATAN2((YCOR1-RADIUS),XCOR1)
                      END IF
                      IF(DABS(YCOR1).LE.1.0D-15.AND.
     1                DABS(XCOR1-RADIUS).LE.1.0D-15) THEN
                          A2TEST=0.0D0
                      ELSE
                          A2TEST=DATAN2(YCOR1,(XCOR1-RADIUS))
                      END IF
                      IF(A2TEST.LE.0.0D0) A2TEST=A2TEST+(TWOPII)
                      IF(ANGLE.GE.A1TEST.AND.ANGLE.LE.A2TEST) THEN
C     CALCULATE A NEW SET OF X AND Y VALUES
C     CENTER OF CIRCLE WITH RESPECT TO CENTER OF THE CLAP
                          CBX=XCOR1-RADIUS
                          CBY=YCOR1-RADIUS
C     SLOPE OF LINE FROM CLAP CENTER
                          M=DTAN(ANGLE)
                          JK_AA=1.0D0+(M**2)
                          JK_BB=-(2.0D0*CBX)-(2.0D0*CBY*M)
                          JK_CC=(CBX**2)+(CBY**2)-(RADIUS**2)
                          QTEST=(JK_BB**2)-(4.0D0*JK_AA*JK_CC)
                          IF(QTEST.GE.0.0D0) THEN
C     SOLUTION EXISTS, ADJUSTMENT IS NECESSARY
                              IF(JK_AA.EQ.0.0D0) THEN
C     ONLY ONE SOLUTION EXISTS
                                  X=-JK_CC/JK_BB
                                  Y=M*X
                                  XID=-JK_CC/JK_BB
                                  YID=M*X
                              ELSE
C     MORE THAN ONE SOLUTION EXISTS
                                  SGNB=JK_BB/DABS(JK_BB)
                                  Q=-0.5*(JK_BB+(SGNB*DSQRT(QTEST)))
                                  XA=Q/JK_AA
                                  XB=JK_CC/Q
                                  YA=M*XA
                                  YB=M*XB
                                  DA=(XA**2)+(YA**2)
                                  DB=(XB**2)+(YB**2)
                                  IF(DA.GE.DB) THEN
                                      X=XA
                                      Y=YA
                                      XID=XA
                                      YID=YA
                                  ELSE
                                      X=XB
                                      Y=YB
                                      XID=XB
                                      YID=YB
                                  END IF
                              END IF
                          ELSE
C     NO SOLUTION(S) EXISTS, NO ADJUSTMENT IS NECESSARY
                          END IF
                      ELSE
C     NO CALCULATION NEEDED
                      END IF
                  ELSE
C     NO CORNER VALUES NEEDED TO BE CALCULATED
                  END IF
C
C     AREA 2
C
                  IF(ANGLE.GE.(PII/2.0D0).AND.ANGLE.LE.PII) THEN
                      IF(DABS(YCOR2).LE.1.0D-15.AND.
     1                DABS(XCOR2+RADIUS).LE.1.0D-15) THEN
                          A1TEST=0.0D0
                      ELSE
                          A1TEST=DATAN2(YCOR2,(XCOR2+RADIUS))
                      END IF
                      IF(DABS(YCOR2-RADIUS).LE.1.0D-15.AND.
     1                DABS(XCOR2).LE.1.0D-15) THEN
                          A2TEST=0.0D0
                      ELSE
                          A2TEST=DATAN2((YCOR2-RADIUS),XCOR2)
                      END IF
                      IF(A1TEST.LE.0.0D0) A1TEST=A1TEST+(TWOPII)
                      IF(A2TEST.LE.0.0D0) A2TEST=A2TEST+(TWOPII)
                      IF(ANGLE.GE.A1TEST.AND.ANGLE.LE.A2TEST) THEN
C     CALCULATE A NEW SET OF X AND Y VALUES
C     CENTER OF CIRCLE WITH RESPECT TO CENTER OF THE CLAP
                          CBX=XCOR2+RADIUS
                          CBY=YCOR2-RADIUS
C     SLOPE OF LINE FROM CLAP CENTER
                          M=DTAN(ANGLE)
                          JK_AA=1.0D0+(M**2)
                          JK_BB=-(2.0D0*CBX)-(2.0D0*CBY*M)
                          JK_CC=(CBX**2)+(CBY**2)-(RADIUS**2)
                          QTEST=(JK_BB**2)-(4.0D0*JK_AA*JK_CC)
                          IF(QTEST.GE.0.0D0) THEN
C     SOLUTION EXISTS, ADJUSTMENT IS NECESSARY
                              IF(JK_AA.EQ.0.0D0) THEN
C     ONLY ONE SOLUTION EXISTS
                                  X=-JK_CC/JK_BB
                                  Y=M*X
                                  XID=-JK_CC/JK_BB
                                  YID=M*X
                              ELSE
C     MORE THAN ONE SOLUTION EXISTS
                                  SGNB=JK_BB/DABS(JK_BB)
                                  Q=-0.5*(JK_BB+(SGNB*DSQRT(QTEST)))
                                  XA=Q/JK_AA
                                  XB=JK_CC/Q
                                  YA=M*XA
                                  YB=M*XB
                                  DA=(XA**2)+(YA**2)
                                  DB=(XB**2)+(YB**2)
                                  IF(DA.GE.DB) THEN
                                      X=XA
                                      Y=YA
                                      XID=XA
                                      YID=YA
                                  ELSE
                                      X=XB
                                      Y=YB
                                      XID=XB
                                      YID=YB
                                  END IF
                              END IF
                          ELSE
C     NO SOLUTION(S) EXISTS, NO ADJUSTMENT IS NECESSARY
                          END IF
                      ELSE
C     NO CALCULATION NEEDED
                      END IF
                  ELSE
C     NO CORNER VALUES NEEDED TO BE CALCULATED
                  END IF
C
C     AREA 3
C
                  IF(ANGLE.GE.PII.AND.ANGLE.LE.((3.0D0*PII)/2.0D0)) THEN
                      IF(DABS(YCOR3+RADIUS).LE.1.0D-15.AND.
     1                DABS(XCOR3).LE.1.0D-15) THEN
                          A1TEST=0.0D0
                      ELSE
                          A1TEST=DATAN2((YCOR3+RADIUS),XCOR3)
                      END IF
                      IF(DABS(YCOR3).LE.1.0D-15.AND.
     1                DABS(XCOR3+RADIUS).LE.1.0D-15) THEN
                          A2TEST=0.0D0
                      ELSE
                          A2TEST=DATAN2(YCOR3,(XCOR3+RADIUS))
                      END IF
                      IF(A1TEST.LE.0.0D0) A1TEST=A1TEST+(TWOPII)
                      IF(A2TEST.LE.0.0D0) A2TEST=A2TEST+(TWOPII)
                      IF(ANGLE.GE.A1TEST.AND.ANGLE.LE.A2TEST) THEN
C     CALCULATE A NEW SET OF X AND Y VALUES
C     CENTER OF CIRCLE WITH RESPECT TO CENTER OF THE CLAP
                          CBX=XCOR3+RADIUS
                          CBY=YCOR3+RADIUS
C     SLOPE OF LINE FROM CLAP CENTER
                          M=DTAN(ANGLE)
                          JK_AA=1.0D0+(M**2)
                          JK_BB=-(2.0D0*CBX)-(2.0D0*CBY*M)
                          JK_CC=(CBX**2)+(CBY**2)-(RADIUS**2)
                          QTEST=(JK_BB**2)-(4.0D0*JK_AA*JK_CC)
                          IF(QTEST.GE.0.0D0) THEN
C     SOLUTION EXISTS, ADJUSTMENT IS NECESSARY
                              IF(JK_AA.EQ.0.0D0) THEN
C     ONLY ONE SOLUTION EXISTS
                                  X=-JK_CC/JK_BB
                                  Y=M*X
                                  XID=-JK_CC/JK_BB
                                  YID=M*X
                              ELSE
C     MORE THAN ONE SOLUTION EXISTS
                                  SGNB=JK_BB/DABS(JK_BB)
                                  Q=-0.5*(JK_BB+(SGNB*DSQRT(QTEST)))
                                  XA=Q/JK_AA
                                  XB=JK_CC/Q
                                  YA=M*XA
                                  YB=M*XB
                                  DA=(XA**2)+(YA**2)
                                  DB=(XB**2)+(YB**2)
                                  IF(DA.GE.DB) THEN
                                      X=XA
                                      Y=YA
                                      XID=XA
                                      YID=YA
                                  ELSE
                                      X=XB
                                      Y=YB
                                      XID=XB
                                      YID=YB
                                  END IF
                              END IF
                          ELSE
C     NO SOLUTION(S) EXISTS, NO ADJUSTMENT IS NECESSARY
                          END IF
                      ELSE
C     NO CALCULATION NEEDED
                      END IF
                  ELSE
C     NO CORNER VALUES NEEDED TO BE CALCULATED
                  END IF
C
C     AREA 4
C
                  IF(ANGLE.GE.((3.0D0*PII)/2.0D0)) THEN
                      IF(DABS(YCOR4).LE.1.0D-15.AND.
     1                DABS(XCOR4-RADIUS).LE.1.0D-15) THEN
                          A1TEST=0.0D0
                      ELSE
                          A1TEST=DATAN2(YCOR4,(XCOR4-RADIUS))
                      END IF
                      IF(DABS(YCOR4+RADIUS).LE.1.0D-15.AND.
     1                DABS(XCOR4).LE.1.0D-15) THEN
                          A2TEST=0.0D0
                      ELSE
                          A2TEST=DATAN2((YCOR4+RADIUS),XCOR4)
                      END IF
                      IF(A1TEST.LE.0.0D0) A1TEST=A1TEST+(TWOPII)
                      IF(A2TEST.LE.0.0D0) A2TEST=A2TEST+(TWOPII)
                      IF(A2TEST.LE.PII)   A2TEST=A2TEST+(TWOPII)
                      IF(ANGLE.GE.A1TEST.AND.ANGLE.LE.A2TEST) THEN
C     CALCULATE A NEW SET OF X AND Y VALUES
C     CENTER OF CIRCLE WITH RESPECT TO CENTER OF THE CLAP
                          CBX=XCOR4-RADIUS
                          CBY=YCOR4+RADIUS
C     SLOPE OF LINE FROM CLAP CENTER
                          M=DTAN(ANGLE)
                          JK_AA=1.0D0+(M**2)
                          JK_BB=-(2.0D0*CBX)-(2.0D0*CBY*M)
                          JK_CC=(CBX**2)+(CBY**2)-(RADIUS**2)
                          QTEST=(JK_BB**2)-(4.0D0*JK_AA*JK_CC)
                          IF(QTEST.GE.0.0D0) THEN
C     SOLUTION EXISTS, ADJUSTMENT IS NECESSARY
                              IF(JK_AA.EQ.0.0D0) THEN
C     ONLY ONE SOLUTION EXISTS
                                  X=-JK_CC/JK_BB
                                  Y=M*X
                                  XID=-JK_CC/JK_BB
                                  YID=M*X
                              ELSE
C     MORE THAN ONE SOLUTION EXISTS
                                  SGNB=JK_BB/DABS(JK_BB)
                                  Q=-0.5*(JK_BB+(SGNB*DSQRT(QTEST)))
                                  XA=Q/JK_AA
                                  XB=JK_CC/Q
                                  YA=M*XA
                                  YB=M*XB
                                  DA=(XA**2)+(YA**2)
                                  DB=(XB**2)+(YB**2)
                                  IF(DA.GE.DB) THEN
                                      X=XA
                                      Y=YA
                                      XID=XA
                                      YID=YA
                                  ELSE
                                      X=XB
                                      Y=YB
                                      XID=XB
                                      YID=YB
                                  END IF
                              END IF
                          ELSE
C     NO SOLUTION(S) EXISTS, NO ADJUSTMENT IS NECESSARY
                          END IF
                      ELSE
C     NO CALCULATION NEEDED
                      END IF
                  ELSE
C     NO CORNER VALUES NEEDED TO BE CALCULATED
                  END IF
C
C     APPLY THE CLAP TILT ANGLE AND THE CLAP DECENTRATIONS
                  GAM=(PII/180.0D0)*ALENS(15,I)
                  XR=(X*DCOS(GAM))-(Y*DSIN(GAM))
                  YR=(Y*DCOS(GAM))+(X*DSIN(GAM))
                  X=XR+ALENS(13,I)+MULTX
                  Y=YR+ALENS(12,I)+MULTY
                  XID=XR+ALENS(13,I)+MULTX
                  YID=YR+ALENS(12,I)+MULTY
C
              ELSE
C     CAFLG NOT 4
              END IF
C       CAFLG=1 CIRCULAR CLAP
              IF(CAFLG.EQ.1.AND.ALENS(127,I).EQ.0.0D0) THEN
C
                  IF(ALENS(10,I).LE.ALENS(11,I))  RAD=DABS(ALENS(10,I))
                  IF(ALENS(10,I).GT.ALENS(11,I))  RAD=DABS(ALENS(11,I))
C
C     CLAP DEC MUST BE 0
                  IF((ALENS(12,I)+MULTX).EQ.0.0D0.AND.
     1            (ALENS(13,I)+MULTY).EQ.0.0D0) THEN
                      IF(ALENS(34,I).NE.18.0D0) THEN
C
C     WE HAVE A CIRCULAR CLAP. IS THE SURFACE ONE WHICH COULD HAVE A
C     FLAT ON IT. ONLY IF SURFACE IS CONCAVE TO AIR.
C
C     IF SURFACE I IS NOT AIR AND I+1 IS AIR
C     THEN IF CV>0 THERE MAY BE A FLAT TO DO
C
                          DOIT=.FALSE.
                          DOIT=ISAIR(I,POSDIR)
                          IF(DOIT) THEN
                              IF(ALENS(1,I).GT.0.0D0.AND.POSDIR.OR.
     1                        ALENS(1,I).LT.0.0D0.AND..NOT.POSDIR) THEN
C     ADJUST POINT
                                  RAD=DABS(ALENS(11,I))
                              END IF
                          END IF
C
C     IF SURFACE I IS AIR AIR AND I+1 IS NOT AIR
C     THEN IF CV<0 THERE MAY BE A FLAT TO DO
C
                          DOIT=.FALSE.
                          DOIT=ISAIR2(I,POSDIR)
                          IF(DOIT) THEN
                              IF(ALENS(1,I).LT.0.0D0.AND.POSDIR.OR.
     1                        ALENS(1,I).GT.0.0D0.AND..NOT.POSDIR) THEN
C     ADJUST POINT
                                  RAD=DABS(ALENS(11,I))
                              END IF
                          END IF
                      END IF
                  ELSE
C     DEC CLAP, NO FLAT CALC
                  END IF
                  XID=(DCOS(ANGLE)*RAD)
                  YID=(DSIN(ANGLE)*RAD)
              ELSE
C       CAFLG NOT 1
              END IF
          ELSE
C     TYPE 18 SPECIAL SURFACE AND IT IS "ON"
C     CALCULATE THE RADIUS OF THE CLAP
C     CEE IS THE CURVATURE CV (ALENS(1,I))
              CEE=ALENS(1,I)
C     KAPPA IS THE CONIC CONSTANT CC (ALENS(2,I))
              KAPPA=ALENS(2,I)
C
C     ZEE IS THE Z POSITION FOR WHICH A RHO IS DESIRED
C     FIRST THE FORE OR FRONT POSITION
              IF(JJ.EQ.1) ZEE=FTFL01(1,I)
              IF(JJ.EQ.2) ZEE=FTFL01(2,I)
              RHO2=(((2.0D0*ZEE)-(CEE*(KAPPA+1.0D0)*(ZEE**2)))/CEE)
              IF(RHO2.LT.0.0D0) THEN
                  IF(JJ.EQ.1)
     1            OUTLYNE=
     1            'UN-REALISTIC FRONT Z-POSITION IN TYPE 18 SPECIAL SURFACE'
                  IF(JJ.EQ.2)
     1            OUTLYNE=
     1            'UN-REALISTIC REAR Z-POSITION IN TYPE 18 SPECIAL SURFACE'
                  CALL SHOWIT(1)
                  OUTLYNE='NO CLAP COULD BE DRAWN'
                  CALL SHOWIT(1)
                  RETURN
              END IF
              RHO=DSQRT(RHO2)
              X=(DCOS(ANGLE)*RHO)
              Y=(DSIN(ANGLE)*RHO)
              XID=(DCOS(ANGLE)*RHO)
              YID=(DSIN(ANGLE)*RHO)
          END IF
          RETURN
      END
C SUB CAO2.FOR
C
C     THIS ROUTINE IS CALLED BY PLTCOB
C
      SUBROUTINE CAO2(X,Y,ANGLE,I,AN2,MDX,MDY,GAMGAM)
C
C     THIS SUBROUTINE CALCULATES THE X AND Y COBS LIMIT VALUES
C     FOR SURFACE III WHICH LIE ON A LINE WHICH EXTENDS FROM THE
C     SURFACE VERTEX OUT TO THE COBS EDGE. "ANGLE" IS MEASURED
C     COUNTER-CLOCKWISE FROM THE X AXIS TO THE Y AXIS OF THE LOCAL
C     SURFACE VERTEX COORDINATE SYSTEM. 0 DEGREES POINTS IN THE +X
C     DIRECTION. "ANGLE" IS PASSED TO THIS ROUTINE IN RADIAN MEASURE.
C
          IMPLICIT NONE
C
          INTEGER COFLG,I,J
C
          LOGICAL OLDIN,NEWIN,ISITIN2,ISITIN2I
C
          EXTERNAL ISITIN2,ISITIN2I
C
          REAL*8 X,Y,GAM,THETA1,THETA2,THETA3,THETA4,MDX,MDY,GAMGAM
     1    ,A,B,RAD,XR,YR,ANGLE,AN2,XCOR1,XCOR2,XCOR3,XCOR4,XOLDX,YOLDY
     2    ,YCOR1,YCOR2,YCOR3,YCOR4,JK_AA,JK_BB,RADIUS,CBX,CBY
     3    ,JK_CC,QTEST,Q,SGNB,XA,XB,YA,YB,A1TEST,A2TEST,M,DA,DB
!     4,XID,YID
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
C
C       SET FLAG COFLG
          COFLG=INT(ALENS(16,I))
          GAM=0.0D0
C
C       COFLG HAS BEEN INITIALIZED
C
C       COFLG=0 NO COBS, NOTHING TO DRAW, JUST RETURN
          IF(COFLG.EQ.0) THEN
              RETURN
          ELSE
C       COFLG NOT 0
          END IF
C
C       COFLG=1 CIRCULAR COBS
          IF(COFLG.EQ.1) THEN
              RAD=DABS(ALENS(17,I))
              X=(DCOS(ANGLE)*RAD)+ALENS(20,I)+MDX
              Y=(DSIN(ANGLE)*RAD)+ALENS(19,I)+MDY
          ELSE
C       COFLG NOT 1
          END IF
C       C0FLG=5, CLAP POLY
          IF(COFLG.EQ.5) THEN
              OLDIN=.TRUE.
              NEWIN=.TRUE.
              XOLDX=0.0D0
              YOLDY=0.0D0
              X=0.0D0
              Y=0.0D0
              DO J=1,99999

                  XOLDX=X
                  YOLDY=Y
                  X=(DBLE(J)*0.005D0*ALENS(10,I))*DCOS(ANGLE)
                  Y=(DBLE(J)*0.005D0*ALENS(10,I))*DSIN(ANGLE)
                  NEWIN=ISITIN2(X,Y,I)
                  X=X+ALENS(20,I)+MDX
                  Y=Y+ALENS(19,I)+MDY
                  IF(OLDIN.AND..NOT.NEWIN) THEN
                      X=(X+XOLDX)/2.0D0
                      Y=(Y+YOLDY)/2.0D0
                      GO TO 10
                  ELSE
                  END IF
              END DO
          ELSE
C     COFLG NOT 5
          END IF
C       C0FLG=6, COBS IPOLY
          IF(COFLG.EQ.6) THEN
              OLDIN=.TRUE.
              NEWIN=.TRUE.
              XOLDX=0.0D0
              YOLDY=0.0D0
              X=0.0D0
              Y=0.0D0
              DO J=1,99999

                  XOLDX=X
                  YOLDY=Y
                  X=(DBLE(J)*0.005D0*ALENS(14,I))*DCOS(ANGLE)
                  Y=(DBLE(J)*0.005D0*ALENS(14,I))*DSIN(ANGLE)
                  NEWIN=ISITIN2I(X,Y,I)
                  X=X+ALENS(20,I)+MDX
                  Y=Y+ALENS(19,I)+MDY
                  IF(OLDIN.AND..NOT.NEWIN) THEN
                      X=(X+XOLDX)/2.0D0
                      Y=(Y+YOLDY)/2.0D0
                      GO TO 10
                  ELSE
                  END IF
              END DO
          ELSE
C     COFLG NOT 6
          END IF
 10       CONTINUE
C       COFLG=2, COBS RECT
          IF(COFLG.EQ.2) THEN
C     CORNER 1 HAS COORDINATES
              XCOR1=+DABS(ALENS(18,I))
              YCOR1=+DABS(ALENS(17,I))
              IF(DABS(YCOR1).LE.1.0D-15.AND.
     1        DABS(XCOR1).LE.1.0D-15) THEN
                  THETA1=0.0D0
              ELSE
                  THETA1=DATAN2(YCOR1,XCOR1)
              END IF
              IF(THETA1.LT.0.0D0) THETA1=THETA1+(TWOPII)
C     CORNER 2 HAS COORDINATES
              XCOR2=-DABS(ALENS(18,I))
              YCOR2=+DABS(ALENS(17,I))
              IF(DABS(YCOR2).LE.1.0D-15.AND.
     1        DABS(XCOR2).LE.1.0D-15) THEN
                  THETA2=0.0D0
              ELSE
                  THETA2=DATAN2(YCOR2,XCOR2)
              END IF
              IF(THETA2.LT.0.0D0) THETA2=THETA2+(TWOPII)
C     CORNER 3 HAS COORDINATES
              XCOR3=-DABS(ALENS(18,I))
              YCOR3=-DABS(ALENS(17,I))
              IF(DABS(YCOR3).LE.1.0D-15.AND.
     1        DABS(XCOR3).LE.1.0D-15) THEN
                  THETA3=0.0D0
              ELSE
                  THETA3=DATAN2(YCOR3,XCOR3)
              END IF
              IF(THETA3.LT.0.0D0) THETA3=THETA3+(TWOPII)
C     CORNER 4 HAS COORDINATES
              XCOR4=+DABS(ALENS(18,I))
              YCOR4=-DABS(ALENS(17,I))
              IF(DABS(YCOR4).LE.1.0D-15.AND.
     1        DABS(XCOR4).LE.1.0D-15) THEN
                  THETA4=0.0D0
              ELSE
                  THETA4=DATAN2(YCOR4,XCOR4)
              END IF
              IF(THETA4.LT.0.0D0) THETA4=THETA4+(TWOPII)
C
              IF(ANGLE.GE.0.0D0.AND.ANGLE.LE.THETA1) THEN
                  X=DABS(ALENS(18,I))
                  Y=X*DTAN(ANGLE)
              ELSE
              END IF
              IF(ANGLE.GT.THETA1.AND.ANGLE.LE.THETA2) THEN
                  Y=DABS(ALENS(17,I))
                  X=Y*DTAN((PII/2.0D0)-ANGLE)
              ELSE
              END IF
              IF(ANGLE.GT.THETA2.AND.ANGLE.LE.THETA3) THEN
                  X=-DABS(ALENS(18,I))
                  Y=X*DTAN(ANGLE)
              ELSE
              END IF
              IF(ANGLE.GT.THETA3.AND.ANGLE.LE.THETA4) THEN
                  Y=-DABS(ALENS(17,I))
                  X=Y*DTAN((PII/2.0D0)-ANGLE)
              ELSE
              END IF
              IF(ANGLE.GT.THETA4.AND.ANGLE.LE.(TWOPII)) THEN
                  X=DABS(ALENS(18,I))
                  Y=X*DTAN(ANGLE)
              ELSE
              END IF
              IF(ANGLE.LE.THETA1.AND.AN2.GE.THETA1) THEN
                  X=XCOR1
                  Y=YCOR1
              ELSE
C     POINT NOT AT CORNER1
              END IF
              IF(ANGLE.LE.THETA2.AND.AN2.GE.THETA2) THEN
                  X=XCOR2
                  Y=YCOR2
              ELSE
C     POINT NOT AT CORNER2
              END IF
              IF(ANGLE.LE.THETA3.AND.AN2.GE.THETA3) THEN
                  X=XCOR3
                  Y=YCOR3
              ELSE
C     POINT NOT AT CORNER3
              END IF
              IF(ANGLE.LE.THETA4.AND.AN2.GE.THETA4) THEN
                  X=XCOR4
                  Y=YCOR4
              ELSE
C     POINT NOT AT CORNER4
              END IF
C
C     APPLY THE COBS TILT ANGLE AND THE COBS DECENTRATIONS
              GAM=(PII/180.0D0)*(ALENS(22,I)+GAMGAM)
              XR=(X*DCOS(GAM))-(Y*DSIN(GAM))
              YR=(Y*DCOS(GAM))+(X*DSIN(GAM))
              X=XR+ALENS(20,I)+MDX
              Y=YR+ALENS(19,I)+MDY
C
          ELSE
C     COFLG NOT 2
          END IF
C
C       COFLG=3, ELLIPTICAL COBS
          IF(COFLG.EQ.3) THEN
C       X-SEMI-MAJOR AXIS IS ALENS(11,I)
C       Y-SEMI-MAJOR AXIS IS ALENS(10,I)
              A=(ALENS(18,I))
              B=(ALENS(17,I))
              RAD=((A**2)*(B**2))/
     1        (((B**2)*((DCOS(ANGLE))**2))+((A**2)*((DSIN(ANGLE)**2))))
              RAD=DSQRT(RAD)
              X=RAD*DCOS(ANGLE)
              Y=RAD*DSIN(ANGLE)
C
C     APPLY THE COBS TILT ANGLE AND THE COBS DECENTRATIONS
              GAM=(PII/180.0D0)*(ALENS(22,I)+GAMGAM)
              XR=(X*DCOS(GAM))-(Y*DSIN(GAM))
              YR=(Y*DCOS(GAM))+(X*DSIN(GAM))
              X=XR+ALENS(20,I)+MDX
              Y=YR+ALENS(19,I)+MDY
C
          ELSE
C     COFLG NOT 3
          END IF
C       COFLG=4, COBS RCTK
          IF(COFLG.EQ.4) THEN
C     CORNER 1 HAS COORDINATES
              XCOR1=+DABS(ALENS(18,I))
              YCOR1=+DABS(ALENS(17,I))
              IF(DABS(YCOR1).LE.1.0D-15.AND.
     1        DABS(XCOR1).LE.1.0D-15) THEN
                  THETA1=0.0D0
              ELSE
                  THETA1=DATAN2(YCOR1,XCOR1)
              END IF
              IF(THETA1.LT.0.0D0) THETA1=THETA1+(TWOPII)
C     CORNER 2 HAS COORDINATES
              XCOR2=-DABS(ALENS(18,I))
              YCOR2=+DABS(ALENS(17,I))
              IF(DABS(YCOR2).LE.1.0D-15.AND.
     1        DABS(XCOR2).LE.1.0D-15) THEN
                  THETA2=0.0D0
              ELSE
                  THETA2=DATAN2(YCOR2,XCOR2)
              END IF
              IF(THETA2.LT.0.0D0) THETA2=THETA2+(TWOPII)
C     CORNER 3 HAS COORDINATES
              XCOR3=-DABS(ALENS(18,I))
              YCOR3=-DABS(ALENS(17,I))
              IF(DABS(YCOR3).LE.1.0D-15.AND.
     1        DABS(XCOR3).LE.1.0D-15) THEN
                  THETA3=0.0D0
              ELSE
                  THETA3=DATAN2(YCOR3,XCOR3)
              END IF
              IF(THETA3.LT.0.0D0) THETA3=THETA3+(TWOPII)
C     CORNER 4 HAS COORDINATES
              XCOR4=+DABS(ALENS(18,I))
              YCOR4=-DABS(ALENS(17,I))
              IF(DABS(YCOR4).LE.1.0D-15.AND.
     1        DABS(XCOR4).LE.1.0D-15) THEN
                  THETA4=0.0D0
              ELSE
                  THETA4=DATAN2(YCOR4,XCOR4)
              END IF
              IF(THETA4.LT.0.0D0) THETA4=THETA4+(TWOPII)
C
              IF(ANGLE.GE.0.0D0.AND.ANGLE.LE.THETA1) THEN
                  X=DABS(ALENS(18,I))
                  Y=X*DTAN(ANGLE)
              ELSE
              END IF
              IF(ANGLE.GT.THETA1.AND.ANGLE.LE.THETA2) THEN
                  Y=DABS(ALENS(17,I))
                  X=Y*DTAN((PII/2.0D0)-ANGLE)
              ELSE
              END IF
              IF(ANGLE.GT.THETA2.AND.ANGLE.LE.THETA3) THEN
                  X=-DABS(ALENS(18,I))
                  Y=X*DTAN(ANGLE)
              ELSE
              END IF
              IF(ANGLE.GT.THETA3.AND.ANGLE.LE.THETA4) THEN
                  Y=-DABS(ALENS(17,I))
                  X=Y*DTAN((PII/2.0D0)-ANGLE)
              ELSE
              END IF
              IF(ANGLE.GT.THETA4.AND.ANGLE.LE.(TWOPII)) THEN
                  X=DABS(ALENS(18,I))
                  Y=X*DTAN(ANGLE)
              ELSE
              END IF
              IF(ANGLE.LE.THETA1.AND.AN2.GE.THETA1) THEN
                  X=XCOR1
                  Y=YCOR1
              ELSE
C     POINT NOT AT CORNER1
              END IF
              IF(ANGLE.LE.THETA2.AND.AN2.GE.THETA2) THEN
                  X=XCOR2
                  Y=YCOR2
              ELSE
C     POINT NOT AT CORNER2
              END IF
              IF(ANGLE.LE.THETA3.AND.AN2.GE.THETA3) THEN
                  X=XCOR3
                  Y=YCOR3
              ELSE
C     POINT NOT AT CORNER3
              END IF
              IF(ANGLE.LE.THETA4.AND.AN2.GE.THETA4) THEN
                  X=XCOR4
                  Y=YCOR4
              ELSE
C     POINT NOT AT CORNER4
              END IF
C
C     HERE, THE RACETRACK HAS BEEN DRAWN AS A RECTANGLE. NOW
C     DOES THIS POINT NEED TO BE MODIFIED DUE TO A RACETRACK
C     CORNER. THIS MODIFICATION IS DONE FOR:
C
C     ANGLES FROM 0 TO 90
C     THEN FROM 90 TO 180
C     THEN FROM 180 TO 270
C     THEN FROM 270 TO 360
              RADIUS=DABS(ALENS(21,I))
C
C     THESE ARE FOUR AREAS OF ADJUSTMENT
C
C     AREA 1
C
              IF(ANGLE.GE.0.0D0.AND.ANGLE.LE.(PII/2.0D0)) THEN
                  IF(DABS(YCOR1-RADIUS).LE.1.0D-15.AND.
     1            DABS(XCOR1).LE.1.0D-15) THEN
                      A1TEST=0.0D0
                  ELSE
                      A1TEST=DATAN2((YCOR1-RADIUS),XCOR1)
                  END IF
                  IF(DABS(YCOR1).LE.1.0D-15.AND.
     1            DABS(XCOR1-RADIUS).LE.1.0D-15) THEN
                      A2TEST=0.0D0
                  ELSE
                      A2TEST=DATAN2(YCOR1,(XCOR1-RADIUS))
                  END IF
                  IF(A2TEST.LE.0.0D0) A2TEST=A2TEST+(TWOPII)
                  IF(ANGLE.GE.A1TEST.AND.ANGLE.LE.A2TEST) THEN
C     CALCULATE A NEW SET OF X AND Y VALUES
C     CENTER OF CIRCLE WITH RESPECT TO CENTER OF THE CLAP
                      CBX=XCOR1-RADIUS
                      CBY=YCOR1-RADIUS
C     SLOPE OF LINE FROM CLAP CENTER
                      M=DTAN(ANGLE)
                      JK_AA=1.0D0+(M**2)
                      JK_BB=-(2.0D0*CBX)-(2.0D0*CBY*M)
                      JK_CC=(CBX**2)+(CBY**2)-(RADIUS**2)
                      QTEST=(JK_BB**2)-(4.0D0*JK_AA*JK_CC)
                      IF(QTEST.GE.0.0D0) THEN
C     SOLUTION EXISTS, ADJUSTMENT IS NECESSARY
                          IF(JK_AA.EQ.0.0D0) THEN
C     ONLY ONE SOLUTION EXISTS
                              X=-JK_CC/JK_BB
                              Y=M*X
                          ELSE
C     MORE THAN ONE SOLUTION EXISTS
                              SGNB=JK_BB/DABS(JK_BB)
                              Q=-0.5*(JK_BB+(SGNB*DSQRT(QTEST)))
                              XA=Q/JK_AA
                              XB=JK_CC/Q
                              YA=M*XA
                              YB=M*XB
                              DA=(XA**2)+(YA**2)
                              DB=(XB**2)+(YB**2)
                              IF(DA.GE.DB) THEN
                                  X=XA
                                  Y=YA
                              ELSE
                                  X=XB
                                  Y=YB
                              END IF
                          END IF
                      ELSE
C     NO SOLUTION(S) EXISTS, NO ADJUSTMENT IS NECESSARY
                      END IF
                  ELSE
C     NO CALCULATION NEEDED
                  END IF
              ELSE
C     NO CORNER VALUES NEEDED TO BE CALCULATED
              END IF
C
C     AREA 2
C
              IF(ANGLE.GE.(PII/2.0D0).AND.ANGLE.LE.PII) THEN
                  IF(DABS(YCOR2).LE.1.0D-15.AND.
     1            DABS(XCOR2+RADIUS).LE.1.0D-15) THEN
                      A1TEST=0.0D0
                  ELSE
                      A1TEST=DATAN2(YCOR2,(XCOR2+RADIUS))
                  END IF
                  IF(DABS(YCOR2-RADIUS).LE.1.0D-15.AND.
     1            DABS(XCOR2).LE.1.0D-15) THEN
                      A2TEST=0.0D0
                  ELSE
                      A2TEST=DATAN2((YCOR2-RADIUS),XCOR2)
                  END IF
                  IF(A1TEST.LE.0.0D0) A1TEST=A1TEST+(TWOPII)
                  IF(A2TEST.LE.0.0D0) A2TEST=A2TEST+(TWOPII)
                  IF(ANGLE.GE.A1TEST.AND.ANGLE.LE.A2TEST) THEN
C     CALCULATE A NEW SET OF X AND Y VALUES
C     CENTER OF CIRCLE WITH RESPECT TO CENTER OF THE CLAP
                      CBX=XCOR2+RADIUS
                      CBY=YCOR2-RADIUS
C     SLOPE OF LINE FROM CLAP CENTER
                      M=DTAN(ANGLE)
                      JK_AA=1.0D0+(M**2)
                      JK_BB=-(2.0D0*CBX)-(2.0D0*CBY*M)
                      JK_CC=(CBX**2)+(CBY**2)-(RADIUS**2)
                      QTEST=(JK_BB**2)-(4.0D0*JK_AA*JK_CC)
                      IF(QTEST.GE.0.0D0) THEN
C     SOLUTION EXISTS, ADJUSTMENT IS NECESSARY
                          IF(JK_AA.EQ.0.0D0) THEN
C     ONLY ONE SOLUTION EXISTS
                              X=-JK_CC/JK_BB
                              Y=M*X
                          ELSE
C     MORE THAN ONE SOLUTION EXISTS
                              SGNB=JK_BB/DABS(JK_BB)
                              Q=-0.5*(JK_BB+(SGNB*DSQRT(QTEST)))
                              XA=Q/JK_AA
                              XB=JK_CC/Q
                              YA=M*XA
                              YB=M*XB
                              DA=(XA**2)+(YA**2)
                              DB=(XB**2)+(YB**2)
                              IF(DA.GE.DB) THEN
                                  X=XA
                                  Y=YA
                              ELSE
                                  X=XB
                                  Y=YB
                              END IF
                          END IF
                      ELSE
C     NO SOLUTION(S) EXISTS, NO ADJUSTMENT IS NECESSARY
                      END IF
                  ELSE
C     NO CALCULATION NEEDED
                  END IF
              ELSE
C     NO CORNER VALUES NEEDED TO BE CALCULATED
              END IF
C
C     AREA 3
C
              IF(ANGLE.GE.PII.AND.ANGLE.LE.((3.0D0*PII)/2.0D0)) THEN
                  IF(DABS(YCOR3+RADIUS).LE.1.0D-15.AND.
     1            DABS(XCOR3).LE.1.0D-15) THEN
                      A1TEST=0.0D0
                  ELSE
                      A1TEST=DATAN2((YCOR3+RADIUS),XCOR3)
                  END IF
                  IF(DABS(YCOR3).LE.1.0D-15.AND.
     1            DABS(XCOR3+RADIUS).LE.1.0D-15) THEN
                      A2TEST=0.0D0
                  ELSE
                      A2TEST=DATAN2(YCOR3,(XCOR3+RADIUS))
                  END IF
                  IF(A1TEST.LE.0.0D0) A1TEST=A1TEST+(TWOPII)
                  IF(A2TEST.LE.0.0D0) A2TEST=A2TEST+(TWOPII)
                  IF(ANGLE.GE.A1TEST.AND.ANGLE.LE.A2TEST) THEN
C     CALCULATE A NEW SET OF X AND Y VALUES
C     CENTER OF CIRCLE WITH RESPECT TO CENTER OF THE CLAP
                      CBX=XCOR3+RADIUS
                      CBY=YCOR3+RADIUS
C     SLOPE OF LINE FROM CLAP CENTER
                      M=DTAN(ANGLE)
                      JK_AA=1.0D0+(M**2)
                      JK_BB=-(2.0D0*CBX)-(2.0D0*CBY*M)
                      JK_CC=(CBX**2)+(CBY**2)-(RADIUS**2)
                      QTEST=(JK_BB**2)-(4.0D0*JK_AA*JK_CC)
                      IF(QTEST.GE.0.0D0) THEN
C     SOLUTION EXISTS, ADJUSTMENT IS NECESSARY
                          IF(JK_AA.EQ.0.0D0) THEN
C     ONLY ONE SOLUTION EXISTS
                              X=-JK_CC/JK_BB
                              Y=M*X
                          ELSE
C     MORE THAN ONE SOLUTION EXISTS
                              SGNB=JK_BB/DABS(JK_BB)
                              Q=-0.5*(JK_BB+(SGNB*DSQRT(QTEST)))
                              XA=Q/JK_AA
                              XB=JK_CC/Q
                              YA=M*XA
                              YB=M*XB
                              DA=(XA**2)+(YA**2)
                              DB=(XB**2)+(YB**2)
                              IF(DA.GE.DB) THEN
                                  X=XA
                                  Y=YA
                              ELSE
                                  X=XB
                                  Y=YB
                              END IF
                          END IF
                      ELSE
C     NO SOLUTION(S) EXISTS, NO ADJUSTMENT IS NECESSARY
                      END IF
                  ELSE
C     NO CALCULATION NEEDED
                  END IF
              ELSE
C     NO CORNER VALUES NEEDED TO BE CALCULATED
              END IF
C
C     AREA 4
C
              IF(ANGLE.GE.((3.0D0*PII)/2.0D0)) THEN
                  IF(DABS(YCOR4).LE.1.0D-15.AND.
     1            DABS(XCOR4-RADIUS).LE.1.0D-15) THEN
                      A1TEST=0.0D0
                  ELSE
                      A1TEST=DATAN2(YCOR4,(XCOR4-RADIUS))
                  END IF
                  IF(DABS(YCOR4+RADIUS).LE.1.0D-15.AND.
     1            DABS(XCOR4).LE.1.0D-15) THEN
                      A2TEST=0.0D0
                  ELSE
                      A2TEST=DATAN2((YCOR4+RADIUS),XCOR4)
                  END IF
                  IF(A1TEST.LE.0.0D0) A1TEST=A1TEST+(TWOPII)
                  IF(A2TEST.LE.0.0D0) A2TEST=A2TEST+(TWOPII)
                  IF(A2TEST.LE.PII)   A2TEST=A2TEST+(TWOPII)
                  IF(ANGLE.GE.A1TEST.AND.ANGLE.LE.A2TEST) THEN
C     CALCULATE A NEW SET OF X AND Y VALUES
C     CENTER OF CIRCLE WITH RESPECT TO CENTER OF THE CLAP
                      CBX=XCOR4-RADIUS
                      CBY=YCOR4+RADIUS
C     SLOPE OF LINE FROM CLAP CENTER
                      M=DTAN(ANGLE)
                      JK_AA=1.0D0+(M**2)
                      JK_BB=-(2.0D0*CBX)-(2.0D0*CBY*M)
                      JK_CC=(CBX**2)+(CBY**2)-(RADIUS**2)
                      QTEST=(JK_BB**2)-(4.0D0*JK_AA*JK_CC)
                      IF(QTEST.GE.0.0D0) THEN
C     SOLUTION EXISTS, ADJUSTMENT IS NECESSARY
                          IF(JK_AA.EQ.0.0D0) THEN
C     ONLY ONE SOLUTION EXISTS
                              X=-JK_CC/JK_BB
                              Y=M*X
                          ELSE
C     MORE THAN ONE SOLUTION EXISTS
                              SGNB=JK_BB/DABS(JK_BB)
                              Q=-0.5*(JK_BB+(SGNB*DSQRT(QTEST)))
                              XA=Q/JK_AA
                              XB=JK_CC/Q
                              YA=M*XA
                              YB=M*XB
                              DA=(XA**2)+(YA**2)
                              DB=(XB**2)+(YB**2)
                              IF(DA.GE.DB) THEN
                                  X=XA
                                  Y=YA
                              ELSE
                                  X=XB
                                  Y=YB
                              END IF
                          END IF
                      ELSE
C     NO SOLUTION(S) EXISTS, NO ADJUSTMENT IS NECESSARY
                      END IF
                  ELSE
C     NO CALCULATION NEEDED
                  END IF
              ELSE
C     NO CORNER VALUES NEEDED TO BE CALCULATED
              END IF
C
C     APPLY THE COBS TILT ANGLE AND THE COBS DECENTRATIONS
              GAM=(PII/180.0D0)*(ALENS(22,I)+GAMGAM)
              XR=(X*DCOS(GAM))-(Y*DSIN(GAM))
              YR=(Y*DCOS(GAM))+(X*DSIN(GAM))
              X=XR+ALENS(20,I)+MDX
              Y=YR+ALENS(19,I)+MDY
C
          ELSE
C     COFLG NOT 4
          END IF
          RETURN
      END
C SUB CAOJK.FOR
C
      SUBROUTINE CAOJK(YMIN,XMIN,YMAX,XMAX,
     1YMINO,XMINO,YMAXO,XMAXO,CAFLG,
     2COFLG,I,
     3YMIN2,XMIN2,YMAX2,XMAX2,THETA,ZDELZ)
C
C     THIS ROUTINE GETS THE SAGS OF LIMITING POINTS AROUND A SURFACE
C     CLAP OR COBS AND RECOGNIZES THE EXISTENCE OF FLATS ON CONCAVE
C     SURFACES.
C
          IMPLICIT NONE
C
          INTEGER CAFLG,COFLG,I,J
C
          LOGICAL ISITIN,DOIT,ISAIR,ISAIR2,POSDIR,OLDIN,NEWIN,ISITINI
C
          EXTERNAL ISAIR,ISAIR2,ISITIN,ISITINI
C
          REAL*8 X,Y,XMIN,YMIN,XMAX,YMAX,XOLDX,YOLDY,ZDELZ
     1    ,XMINO,YMINO,XMAXO,YMAXO,GAM,RHO2,RHO
     2    ,CLPLCX(0:499),CLPLCY(0:499),KAPPA,CEE,ZEE,PEEPEE,
     3    YMIN2,XMIN2,YMAX2,XMAX2,THETA,THETA2
C
          COMMON/CLPLOC/CLPLCX,CLPLCY
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
C
          ZDELZ=0.0D0
C
          THETA2=THETA+PII
C
C       SET FLAGS CAFLG AND COFLG
          CAFLG=INT(ALENS(9,I))
          COFLG=INT(ALENS(16,I))
          GAM=0.0D0
          IF(ALENS(34,I).NE.18.0D0) THEN
C     NOT TYPE 18 SPECIAL SURFACE
C
C       CAFLG AND COFLG HAVE BEEN SET
C
C       CAFLG=0 NO CLAP, USE PARAXIAL DATA
C
              IF(CAFLG.EQ.0) THEN
C
C     COORDINATES OF THE END POINTS FOR PROF
                  XMAX=(((DABS(PXTRAX(1,I))+DABS(PXTRAX(5,I))))*DCOS(THETA))
     1            +CLPLCX(I)
                  YMAX=(((DABS(PXTRAY(1,I))+DABS(PXTRAY(5,I))))*DSIN(THETA))
     1            +CLPLCY(I)
                  XMIN=(((DABS(PXTRAX(1,I))+DABS(PXTRAX(5,I))))*DCOS(THETA2))
     1            +CLPLCX(I)
                  YMIN=(((DABS(PXTRAY(1,I))+DABS(PXTRAY(5,I))))*DSIN(THETA2))
     1            +CLPLCY(I)
C
                  XMIN2=XMIN
                  YMIN2=YMIN
                  XMAX2=XMAX
                  YMAX2=YMAX
              ELSE
C       CAFLG NOT 0
              END IF
C
C       CAFLG=1 CIRCULAR CLAP
              IF(CAFLG.EQ.1) THEN
C
                  PEEPEE=DABS(ALENS(10,I))
C
C       RIGHT POINT
                  XMAX=((PEEPEE)*DCOS(THETA))+ALENS(13,I)
                  YMAX=((PEEPEE)*DSIN(THETA))+ALENS(12,I)
                  XMIN=((PEEPEE)*DCOS(THETA2))+ALENS(13,I)
                  YMIN=((PEEPEE)*DSIN(THETA2))+ALENS(12,I)
                  XMAX2=XMAX
                  YMAX2=YMAX
                  XMIN2=XMIN
                  YMIN2=YMIN
              ELSE
C       CAFLG NOT 1
              END IF
C
C       CAFLG=2,3,OR 4
              IF(CAFLG.EQ.2.OR.CAFLG.EQ.3.OR.CAFLG.EQ.4) THEN
C
                  GAM=(PII/180.0D0)*ALENS(15,I)
                  XMAX=(ALENS(11,I))*DCOS(THETA)
                  YMAX=(ALENS(10,I))*DSIN(THETA)
                  X=(XMAX*DCOS(GAM))-(YMAX*DSIN(GAM))
                  Y=(YMAX*DCOS(GAM))+(XMAX*DSIN(GAM))
                  XMAX=X+ALENS(13,I)
                  YMAX=Y+ALENS(12,I)
                  XMAX2=X+ALENS(13,I)
                  YMAX2=Y+ALENS(12,I)
                  XMIN=(ALENS(11,I))*DCOS(THETA2)
                  YMIN=(ALENS(10,I))*DSIN(THETA2)
                  X=(XMIN*DCOS(GAM))-(YMIN*DSIN(GAM))
                  Y=(YMIN*DCOS(GAM))+(XMIN*DSIN(GAM))
                  XMIN=X+ALENS(13,I)
                  YMIN=Y+ALENS(12,I)
                  XMIN2=X+ALENS(13,I)
                  YMIN2=Y+ALENS(12,I)
              ELSE
C     CAFLG NOT 2,3 OR 4
              END IF
C       CAFLG=5
              IF(CAFLG.EQ.5) THEN
C   THETA IS THE ANGLE OF THE PROFILE LINE IN THE
C   LOCAL COORDINATE SYSTEM OF THE SURFACE
C   THE COORDINATES ON THIS LINE ARE
                  OLDIN=.TRUE.
                  NEWIN=.TRUE.
                  XOLDX=0.0D0
                  YOLDY=0.0D0
                  X=0.0D0
                  Y=0.0D0
                  DO J=1,99999

                      XOLDX=X
                      YOLDY=Y
                      X=(DBLE(J)*0.01D0*ALENS(10,I))*DCOS(THETA)
                      Y=(DBLE(J)*0.01D0*ALENS(10,I))*DSIN(THETA)
                      NEWIN=ISITIN(X,Y,I)
                      IF(OLDIN.AND..NOT.NEWIN) THEN
                          X=(X+XOLDX)/2.0D0
                          Y=(Y+YOLDY)/2.0D0
                          X=X+ALENS(13,I)
                          Y=Y+ALENS(12,I)
                          XMAX=X
                          YMAX=Y
                          XMAX2=X
                          YMAX2=Y
                          GO TO 10
                      ELSE
                      END IF
                  END DO
 10               CONTINUE
                  OLDIN=.TRUE.
                  NEWIN=.TRUE.
                  XOLDX=0.0D0
                  YOLDY=0.0D0
                  X=0.0D0
                  Y=0.0D0
                  DO J=1,99999

                      XOLDX=X
                      YOLDY=Y
                      X=-(DBLE(J)*0.01D0*ALENS(10,I))*DCOS(THETA)
                      Y=-(DBLE(J)*0.01D0*ALENS(10,I))*DSIN(THETA)
                      NEWIN=ISITIN(X,Y,I)
                      IF(OLDIN.AND..NOT.NEWIN) THEN
                          X=(X+XOLDX)/2.0D0
                          Y=(Y+YOLDY)/2.0D0
                          X=X+ALENS(13,I)
                          Y=Y+ALENS(12,I)
                          XMIN=X
                          YMIN=Y
                          XMIN2=X
                          YMIN2=Y
                          GO TO 20
                      ELSE
                      END IF
                  END DO
              ELSE
C     CAFLG NOT 5
              END IF
C       CAFLG=6
              IF(CAFLG.EQ.6) THEN
C   THETA IS THE ANGLE OF THE PROFILE LINE IN THE
C   LOCAL COORDINATE SYSTEM OF THE SURFACE
C   THE COORDINATES ON THIS LINE ARE
                  OLDIN=.TRUE.
                  NEWIN=.TRUE.
                  XOLDX=0.0D0
                  YOLDY=0.0D0
                  X=0.0D0
                  Y=0.0D0
                  DO J=1,99999

                      XOLDX=X
                      YOLDY=Y
                      X=(DBLE(J)*0.01D0*ALENS(10,I))*DCOS(THETA)
                      Y=(DBLE(J)*0.01D0*ALENS(10,I))*DSIN(THETA)
                      NEWIN=ISITIN(X,Y,I)
                      IF(OLDIN.AND..NOT.NEWIN) THEN
                          X=(X+XOLDX)/2.0D0
                          Y=(Y+YOLDY)/2.0D0
                          X=X+ALENS(13,I)
                          Y=Y+ALENS(12,I)
                          XMAX=X
                          YMAX=Y
                          XMAX2=X
                          YMAX2=Y
                          GO TO 11
                      ELSE
                      END IF
                  END DO
 11               CONTINUE
                  OLDIN=.TRUE.
                  NEWIN=.TRUE.
                  XOLDX=0.0D0
                  YOLDY=0.0D0
                  X=0.0D0
                  Y=0.0D0
                  DO J=1,99999

                      XOLDX=X
                      YOLDY=Y
                      X=-(DBLE(J)*0.01D0*ALENS(10,I))*DCOS(THETA)
                      Y=-(DBLE(J)*0.01D0*ALENS(10,I))*DSIN(THETA)
                      NEWIN=ISITIN(X,Y,I)
                      IF(OLDIN.AND..NOT.NEWIN) THEN
                          X=(X+XOLDX)/2.0D0
                          Y=(Y+YOLDY)/2.0D0
                          X=X+ALENS(13,I)
                          Y=Y+ALENS(12,I)
                          XMIN=X
                          YMIN=Y
                          XMIN2=X
                          YMIN2=Y
                          GO TO 20
                      ELSE
                      END IF
                  END DO
              ELSE
C     CAFLG NOT 5
              END IF
 20           CONTINUE
C       CAFLG=1 CIRCULAR CLAP
              IF(ALENS(13,I).EQ.0.0D0.AND.ALENS(12,I).EQ.0.0D0) THEN
                  IF(CAFLG.EQ.1) THEN
C
                      PEEPEE=DABS(ALENS(10,I))
C
C     CLAP DEC MUST BE 0
C
C     WE HAVE A CIRCULAR CLAP. IS THE SURFACE ONE WHICH COULD HAVE A
C     FLAT ON IT. ONLY IF SURFACE IS CONCAVE TO AIR.
C
C     IF SURFACE I IS NOT AIR AND I+1 IS AIR
C     THEN IF CV>0 THERE MAY BE A FLAT TO DO
C     AS LONG AS SPECIAL SURFACE 18 NOT PRESENT
C
C
                      DOIT=.FALSE.
C     CHECK FOR A FLAT, ELSE DON'T
C
                      DOIT=ISAIR(I,POSDIR)
                      IF(DOIT) THEN
                          IF(ALENS(1,I).GT.0.0D0.AND.POSDIR.OR.
     1                    ALENS(1,I).LT.0.0D0.AND..NOT.POSDIR) THEN
C     ADJUST POINT
                              PEEPEE=DABS(ALENS(11,I))
                              IF(ALENS(1,I).GT.0.0D0) ZDELZ=-DABS(ALENS(14,I))
                              IF(ALENS(1,I).LT.0.0D0) ZDELZ=DABS(ALENS(14,I))
                          END IF
                      END IF
C
C     IF SURFACE I IS NOT AIR AND I+1 IS AIR
C     THEN IF CV<0 THERE MAY BE A FLAT TO DO
                      DOIT=.FALSE.
                      DOIT=ISAIR2(I,POSDIR)
                      IF(DOIT) THEN
                          IF(ALENS(1,I).LT.0.0D0.AND.POSDIR.OR.
     1                    ALENS(1,I).GT.0.0D0.AND..NOT.POSDIR) THEN
C     ADJUST POINT
                              PEEPEE=DABS(ALENS(11,I))
                              IF(ALENS(1,I).GT.0.0D0) ZDELZ=-DABS(ALENS(14,I))
                              IF(ALENS(1,I).LT.0.0D0) ZDELZ=DABS(ALENS(14,I))
                          END IF
                      END IF
C
                      XMAX2=(PEEPEE)*DCOS(THETA)
                      YMAX2=(PEEPEE)*DSIN(THETA)
                      XMIN2=(PEEPEE)*DCOS(THETA2)
                      YMIN2=(PEEPEE)*DSIN(THETA2)
                  ELSE
C       CAFLG NOT 1
                  END IF
              ELSE
C     DEC CLAP, NO FLAT CALC
                  XMAX2=XMAX
                  YMAX2=YMAX
                  XMIN2=XMIN
                  YMIN2=YMIN
              END IF
C       COFLG=0 NO COBS
              IF(COFLG.EQ.0) THEN
                  XMINO=0.0D0
                  YMINO=0.0D0
                  XMAXO=0.0D0
                  YMAXO=0.0D0
                  RETURN
              ELSE
C     MUST BE SOME COBS, CONTINUE
              END IF
C       COFLG=1 CIRCULAR COBS
              IF(COFLG.EQ.1) THEN
C       RIGHT POINT
                  XMAXO=((ALENS(17,I))*DCOS(THETA))
                  YMAXO=((ALENS(17,I))*DSIN(THETA))
                  XMINO=((ALENS(17,I))*DCOS(THETA2))
                  YMINO=((ALENS(17,I))*DSIN(THETA2))
                  XMAXO=XMAXO+ALENS(20,I)
                  XMINO=XMINO+ALENS(20,I)
                  YMAXO=YMAXO+ALENS(19,I)
                  YMINO=YMINO+ALENS(19,I)
              ELSE
C       COFLG NOT 1
              END IF
C
C       COFLG=2,3,OR 4
              IF(COFLG.EQ.2.OR.COFLG.EQ.3.OR.COFLG.EQ.4) THEN
C
                  GAM=(PII/180.0D0)*ALENS(22,I)
                  XMAXO=(ALENS(18,I))*DCOS(THETA)
                  YMAXO=(ALENS(17,I))*DSIN(THETA)
                  X=(XMAXO*DCOS(GAM))-(YMAXO*DSIN(GAM))
                  Y=(YMAXO*DCOS(GAM))+(XMAXO*DSIN(GAM))
                  XMAXO=X+ALENS(20,I)
                  YMAXO=Y+ALENS(19,I)
                  XMINO=(ALENS(18,I))*DCOS(THETA2)
                  YMINO=(ALENS(17,I))*DSIN(THETA2)
                  X=(XMINO*DCOS(GAM))-(YMINO*DSIN(GAM))
                  Y=(YMINO*DCOS(GAM))+(XMINO*DSIN(GAM))
                  XMINO=X+ALENS(20,I)
                  YMINO=Y+ALENS(19,I)
              ELSE
C     COFLG NOT 2,3 OR 4
              END IF
C     NOW FOR SOME SURFACE TYPES, THERE IS A PREDICTABLE
C     EXTENT BEYOND WHICH THE SAG OF THE SURFACE WILL NOT
C     BE A REAL NUMBER. IF THE LIMITS CALCULATED ABOVE EXCEED
C     THESE LIMITS, THEN ADJUST THE LIMITS BEFORE PROCEEDING.
          ELSE
C     TYPE 18 SPECIAL SURFACE AND IT IS "ON"
C     CEE IS THE CURVATURE CV (ALENS(1,I))
              CEE=ALENS(1,I)
C     KAPPA IS THE CONIC CONSTANT CC (ALENS(2,I))
              KAPPA=ALENS(2,I)
C
C     ZEE IS THE Z POSITION FOR WHICH A RHO IS DESIRED
C     FIRST THE FORE OR FRONT POSITION
              ZEE=FTFL01(1,I)
              RHO2=(((2.0D0*ZEE)-(CEE*(KAPPA+1.0D0)*(ZEE**2)))/CEE)
              IF(RHO2.LT.0.0D0) THEN
                  OUTLYNE=
     1            'UN-REALISTIC FRONT Z-POSITION IN TYPE 18 SPECIAL SURFACE'
                  CALL SHOWIT(1)
                  OUTLYNE='NO PROFILE COULD BE DRAWN'
                  CALL SHOWIT(1)
                  XMIN=0.0D0
                  YMIN=0.0D0
                  XMAX=0.0D0
                  YMAX=0.0D0
                  XMINO=0.0D0
                  YMINO=0.0D0
                  XMAXO=0.0D0
                  YMAXO=0.0D0
                  RETURN
              END IF
              RHO=DSQRT(RHO2)
              XMAX=RHO*DCOS(THETA)
              YMAX=RHO*DSIN(THETA)
              XMIN=RHO*DCOS(THETA2)
              YMIN=RHO*DSIN(THETA2)
C     THEN THE AFT OR BACK POSITION
              ZEE=FTFL01(2,I)
              RHO2=(((2.0D0*ZEE)-(CEE*(KAPPA+1.0D0)*(ZEE**2)))/CEE)
              IF(RHO2.LT.0.0D0) THEN
                  OUTLYNE=
     1            'UN-REALISTIC REAR Z-POSITION IN TYPE 18 SPECIAL SURFACE'
                  CALL SHOWIT(1)
                  OUTLYNE='NO PROFILE COULD BE DRAWN'
                  CALL SHOWIT(1)
                  XMIN=0.0D0
                  YMIN=0.0D0
                  XMAX=0.0D0
                  YMAX=0.0D0
                  XMINO=0.0D0
                  YMINO=0.0D0
                  XMAXO=0.0D0
                  YMAXO=0.0D0
                  RETURN
              END IF
              RHO=DSQRT(RHO2)
              XMAXO=RHO*DCOS(THETA)
              YMAXO=RHO*DSIN(THETA)
              XMINO=RHO*DCOS(THETA2)
              YMINO=RHO*DSIN(THETA2)
          END IF
          RETURN
      END
C SUB ROT1.FOR
      SUBROUTINE ROT1
C
          IMPLICIT NONE
C
C       THIS ROUTINE DETERMINES THE CENTER OF ROTATION FOR PLOT LOOK
C     AND PLOT VIEW FOR THE PLOT RAY, EDGEY AND EDGEX COMMANDS
C
          REAL*8 X,Y,Z
     3    ,XMAXIX,XMINIX,YMAXIY,YMINIY,ZMAXIZ,ZMINIZ
C
          INTEGER I
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
          STASUR=INT(W1)
          STPSUR=INT(W2)
          IF(.NOT.ROTSET) THEN
              DO I=STASUR,STPSUR
                  X=VERTEX(1,I)
                  Y=VERTEX(2,I)
                  Z=VERTEX(3,I)
                  IF(I.EQ.STASUR) THEN
                      XMINIX=X
                      XMAXIX=X
                      YMINIY=Y
                      YMAXIY=Y
                      ZMINIZ=Z
                      ZMAXIZ=Z
                  ELSE
                  END IF
                  IF(X.LE.XMINIX) XMINIX=X
                  IF(X.GT.XMAXIX) XMAXIX=X
                  IF(Y.LE.YMINIY) YMINIY=Y
                  IF(Y.GT.YMAXIY) YMAXIY=Y
                  IF(Z.LE.ZMINIZ) ZMINIZ=Z
                  IF(Z.GT.ZMAXIZ) ZMAXIZ=Z
              END DO
              XROT=(XMAXIX+XMINIX)/2.0D0
              YROT=(YMAXIY+YMINIY)/2.0D0
              ZROT=(ZMAXIZ+ZMINIZ)/2.0D0
              ROTSET=.TRUE.
          ELSE
          END IF
          RETURN
      END
C SUB ROT2.FOR
      SUBROUTINE ROT2(PRO,M1,M2,M3,M4)
C
          IMPLICIT NONE
C
C       THIS ROUTINE DETERMINES THE CENTER OF ROTATION FOR PLOT LOOK
C     AND PLOT VIEW FOR THE PLOT PROFX/PROFY COMMANDS
C
C     IF SEVERAL SURFACES ARE ASKED FOR, ROTATION POINTS ARE
C     CALCULATED USING GLOBAL VERTEX DATA. FOR ONE SURFACE,
C     THE ROTATION POINT IS SET AT THE CENTER ON THE X AND Y
C     SURFACE PROFILES.
C
          REAL*8 X,Y,Z,X1,Y1,Z1,X2,Y2,Z2
     1    ,XMAXIX,XMINIX,YMAXIY,YMINIY,ZMAXIZ,ZMINIZ,XA,YA,ZA,PRO
C
          INTEGER I,M1,M2,M3,M4
C
          DIMENSION PRO(M1,M2,M3:M4)
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
          STASUR=INT(W1)
          STPSUR=INT(W2)
          IF(STASUR.NE.STPSUR) THEN
              IF(.NOT.ROTSET) THEN
                  DO I=STASUR,STPSUR
                      X=VERTEX(1,I)
                      Y=VERTEX(2,I)
                      Z=VERTEX(3,I)
                      IF(I.EQ.STASUR) THEN
                          XMINIX=X
                          XMAXIX=X
                          YMINIY=Y
                          YMAXIY=Y
                          ZMINIZ=Z
                          ZMAXIZ=Z
                      ELSE
                      END IF
                      IF(X.LE.XMINIX) XMINIX=X
                      IF(X.GT.XMAXIX) XMAXIX=X
                      IF(Y.LE.YMINIY) YMINIY=Y
                      IF(Y.GT.YMAXIY) YMAXIY=Y
                      IF(Z.LE.ZMINIZ) ZMINIZ=Z
                      IF(Z.GT.ZMAXIZ) ZMAXIZ=Z
                  END DO
                  XROT=(XMAXIX+XMINIX)/2.0D0
                  YROT=(YMAXIY+YMINIY)/2.0D0
                  ZROT=(ZMAXIZ+ZMINIZ)/2.0D0
                  ROTSET=.TRUE.
              ELSE
C     ROTSET IS TRUE AND XROT,YROT AND ZROT HAVE ALREADY BEEN DONE
              END IF
          ELSE
C     STASUR SAME AS STPSUR, KEY OFF PROFILE END POINTS
              IF(.NOT.ROTSET) THEN
                  I=STASUR
C
                  X1=PRO(1,1,I)
                  Y1=PRO(1,2,I)
                  Z1=PRO(1,3,I)
                  X2=PRO(90,1,I)
                  Y2=PRO(90,2,I)
                  Z2=PRO(90,3,I)
                  XA=(X2+X1)/2.0D0
                  YA=(Y2+Y1)/2.0D0
                  ZA=(Z2+Z1)/2.0D0
                  XROT=XA
                  YROT=YA
                  ZROT=ZA
                  ROTSET=.TRUE.
              ELSE
C     ROTSET IS TRUE AND XROT,YROT AND ZROT HAVE ALREADY BEEN DONE
              END IF
          END IF
          RETURN
      END


      FUNCTION ISAIR(I,POSDIR)
          IMPLICIT NONE
          LOGICAL A1,A2,ISAIR,POSDIR
          INTEGER I
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
          A1=.TRUE.
C     A1 TRUE MEANS AIR
          IF(DABS(ALENS(46,I-1)).GT.1.0D0) A1=.FALSE.
          IF(DABS(ALENS(47,I-1)).GT.1.0D0) A1=.FALSE.
          IF(DABS(ALENS(48,I-1)).GT.1.0D0) A1=.FALSE.
          IF(DABS(ALENS(49,I-1)).GT.1.0D0) A1=.FALSE.
          IF(DABS(ALENS(50,I-1)).GT.1.0D0) A1=.FALSE.
          IF(DABS(ALENS(71,I-1)).GT.1.0D0) A1=.FALSE.
          IF(DABS(ALENS(72,I-1)).GT.1.0D0) A1=.FALSE.
          IF(DABS(ALENS(73,I-1)).GT.1.0D0) A1=.FALSE.
          IF(DABS(ALENS(74,I-1)).GT.1.0D0) A1=.FALSE.
          IF(DABS(ALENS(75,I-1)).GT.1.0D0) A1=.FALSE.
          A2=.TRUE.
C     A2 TRUE MEANS AIR
          IF(DABS(ALENS(46,I)).GT.1.0D0) A2=.FALSE.
          IF(DABS(ALENS(47,I)).GT.1.0D0) A2=.FALSE.
          IF(DABS(ALENS(48,I)).GT.1.0D0) A2=.FALSE.
          IF(DABS(ALENS(49,I)).GT.1.0D0) A2=.FALSE.
          IF(DABS(ALENS(50,I)).GT.1.0D0) A2=.FALSE.
          IF(DABS(ALENS(71,I)).GT.1.0D0) A2=.FALSE.
          IF(DABS(ALENS(72,I)).GT.1.0D0) A2=.FALSE.
          IF(DABS(ALENS(73,I)).GT.1.0D0) A2=.FALSE.
          IF(DABS(ALENS(74,I)).GT.1.0D0) A2=.FALSE.
          IF(DABS(ALENS(75,I)).GT.1.0D0) A2=.FALSE.
          ISAIR=.FALSE.
          IF(.NOT.A1.AND.A2) ISAIR=.TRUE.
C
          POSDIR=.TRUE.
          IF((ALENS(46,I)).LT.0.0D0) POSDIR=.FALSE.
          IF((ALENS(47,I)).LT.0.0D0) POSDIR=.FALSE.
          IF((ALENS(48,I)).LT.0.0D0) POSDIR=.FALSE.
          IF((ALENS(49,I)).LT.0.0D0) POSDIR=.FALSE.
          IF((ALENS(50,I)).LT.0.0D0) POSDIR=.FALSE.
          IF((ALENS(71,I)).LT.0.0D0) POSDIR=.FALSE.
          IF((ALENS(72,I)).LT.0.0D0) POSDIR=.FALSE.
          IF((ALENS(73,I)).LT.0.0D0) POSDIR=.FALSE.
          IF((ALENS(74,I)).LT.0.0D0) POSDIR=.FALSE.
          IF((ALENS(75,I)).LT.0.0D0) POSDIR=.FALSE.
          RETURN
      END
      FUNCTION ISAIR2(I,POSDIR)
          IMPLICIT NONE
          LOGICAL A1,A2,ISAIR2,POSDIR
          INTEGER I
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
          A1=.TRUE.
          IF(DABS(ALENS(46,I-1)).GT.1.0D0) A1=.FALSE.
          IF(DABS(ALENS(47,I-1)).GT.1.0D0) A1=.FALSE.
          IF(DABS(ALENS(48,I-1)).GT.1.0D0) A1=.FALSE.
          IF(DABS(ALENS(49,I-1)).GT.1.0D0) A1=.FALSE.
          IF(DABS(ALENS(50,I-1)).GT.1.0D0) A1=.FALSE.
          IF(DABS(ALENS(71,I-1)).GT.1.0D0) A1=.FALSE.
          IF(DABS(ALENS(72,I-1)).GT.1.0D0) A1=.FALSE.
          IF(DABS(ALENS(73,I-1)).GT.1.0D0) A1=.FALSE.
          IF(DABS(ALENS(74,I-1)).GT.1.0D0) A1=.FALSE.
          IF(DABS(ALENS(75,I-1)).GT.1.0D0) A1=.FALSE.
          A2=.TRUE.
          IF(DABS(ALENS(46,I)).GT.1.0D0) A2=.FALSE.
          IF(DABS(ALENS(47,I)).GT.1.0D0) A2=.FALSE.
          IF(DABS(ALENS(48,I)).GT.1.0D0) A2=.FALSE.
          IF(DABS(ALENS(49,I)).GT.1.0D0) A2=.FALSE.
          IF(DABS(ALENS(50,I)).GT.1.0D0) A2=.FALSE.
          IF(DABS(ALENS(71,I)).GT.1.0D0) A2=.FALSE.
          IF(DABS(ALENS(72,I)).GT.1.0D0) A2=.FALSE.
          IF(DABS(ALENS(73,I)).GT.1.0D0) A2=.FALSE.
          IF(DABS(ALENS(74,I)).GT.1.0D0) A2=.FALSE.
          IF(DABS(ALENS(75,I)).GT.1.0D0) A2=.FALSE.
          ISAIR2=.FALSE.
          IF(A1.AND..NOT.A2) ISAIR2=.TRUE.
C
          POSDIR=.TRUE.
          IF((ALENS(46,I)).LT.0.0D0) POSDIR=.FALSE.
          IF((ALENS(47,I)).LT.0.0D0) POSDIR=.FALSE.
          IF((ALENS(48,I)).LT.0.0D0) POSDIR=.FALSE.
          IF((ALENS(49,I)).LT.0.0D0) POSDIR=.FALSE.
          IF((ALENS(50,I)).LT.0.0D0) POSDIR=.FALSE.
          IF((ALENS(71,I)).LT.0.0D0) POSDIR=.FALSE.
          IF((ALENS(72,I)).LT.0.0D0) POSDIR=.FALSE.
          IF((ALENS(73,I)).LT.0.0D0) POSDIR=.FALSE.
          IF((ALENS(74,I)).LT.0.0D0) POSDIR=.FALSE.
          IF((ALENS(75,I)).LT.0.0D0) POSDIR=.FALSE.
          RETURN
      END
C SUB CAO.FOR
C
      SUBROUTINE CAO(YLFT,XLFT,YRHT,XRHT,XTOP,YTOP,XBOT,YBOT,
     1YLFTO,XLFTO,YRHTO,XRHTO,XTOPO,YTOPO,XBOTO,YBOTO,CAFLG,
     2COFLG,I,
     3YLFT2,XLFT2,YRHT2,XRHT2,XTOP2,YTOP2,XBOT2,YBOT2,ZDELZ)
C
C     THIS ROUTINE GETS THE SAGS OF LIMITING POINTS AROUND A SURFACE
C     CLAP OR COBS AND RECOGNIZES THE EXISTENCE OF FLATS ON CONCAVE
C     SURFACES.
C
          IMPLICIT NONE
C
          INTEGER CAFLG,COFLG,I
C
          LOGICAL DOIT,ISAIR,ISAIR2,POSDIR
C
          EXTERNAL ISAIR,ISAIR2
C
          REAL*8 X,Y,XLFT,YLFT,XRHT,YRHT,XTOP,YTOP,XBOT,YBOT
     1    ,XLFTO,YLFTO,XRHTO,YRHTO,XTOPO,YTOPO,XBOTO,YBOTO,GAM,RHO2,RHO
     2    ,CLPLCX(0:499),CLPLCY(0:499),KAPPA,CEE,ZEE,PEEPEE,
     3    YLFT2,XLFT2,YRHT2,XRHT2,XTOP2,YTOP2,XBOT2,YBOT2,ZDELZ
C
          COMMON/CLPLOC/CLPLCX,CLPLCY
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
          ZDELZ=0.0D0
C
C       SET FLAGS CAFLG AND COFLG
          CAFLG=INT(ALENS(9,I))
          COFLG=INT(ALENS(16,I))
          GAM=0.0D0
          IF(ALENS(34,I).NE.18.0D0) THEN
C     NOT TYPE 18 SPECIAL SURFACE
C
C       CAFLG AND COFLG HAVE BEEN SET
C
C       CAFLG=0 NO CLAP, USE PARAXIAL DATA
C
              IF(CAFLG.EQ.0) THEN
C
C     COORDINATES OF THE END POINTS FOR PROFX
C     RIGHT POINT
                  XRHT=((DABS(PXTRAX(1,I))+DABS(PXTRAX(5,I))))+CLPLCX(I)
                  YRHT=0.0D0+CLPLCY(I)
C     LEFT POINT
                  XLFT=(-(DABS(PXTRAX(1,I))+DABS(PXTRAX(5,I))))+CLPLCX(I)
                  YLFT=0.0D0+CLPLCY(I)
C
C     COORDINATES OF THE END POINTS FOR PROFY
C     UPPER POINT
                  XTOP=0.0D0+CLPLCX(I)
                  YTOP=( (DABS(PXTRAY(1,I))+DABS(PXTRAY(5,I))))+CLPLCY(I)
C     BOTTOM POINT
                  XBOT=0.0D0+CLPLCX(I)
                  YBOT=(-(DABS(PXTRAY(1,I))+DABS(PXTRAY(5,I))))+CLPLCY(I)
                  XTOP2=XTOP
                  YTOP2=YTOP
                  XBOT2=XBOT
                  YBOT2=YBOT
                  XRHT2=XRHT
                  YRHT2=YRHT
                  XLFT2=XLFT
                  YLFT2=YLFT
              ELSE
C       CAFLG NOT 0
              END IF
C
C       CAFLG=1 CIRCULAR CLAP
              IF(CAFLG.EQ.1) THEN
C
                  PEEPEE=DABS(ALENS(10,I))
C
C       RIGHT POINT
                  XRHT=(PEEPEE)+ALENS(13,I)
                  YRHT=0.0D0+ALENS(12,I)
C       LEFT POINT
                  XLFT=(-PEEPEE)+ALENS(13,I)
                  YLFT=0.0D0+ALENS(12,I)
C       TOP POINT
                  XTOP=0.0D0+ALENS(13,I)
                  YTOP=(PEEPEE)+ALENS(12,I)
C       BOTTOM BOINT
                  XBOT=0.0D0+ALENS(13,I)
                  YBOT=(-PEEPEE)+ALENS(12,I)
              ELSE
C       CAFLG NOT 1
              END IF
C
C       CAFLG=2
              IF(CAFLG.EQ.2) THEN
C
                  GAM=(PII/180.0D0)*ALENS(15,I)
C       RIGHT POINT
                  XRHT=( ALENS(11,I))
                  YRHT=0.0D0
                  X=(XRHT*DCOS(GAM))-(YRHT*DSIN(GAM))
                  Y=(YRHT*DCOS(GAM))+(XRHT*DSIN(GAM))
                  XRHT=X+ALENS(13,I)
                  YRHT=Y+ALENS(12,I)
                  XRHT2=X+ALENS(13,I)
                  YRHT2=Y+ALENS(12,I)
C       LEFT POINT
                  XLFT=(-ALENS(11,I))
                  YLFT=0.0D0
                  X=(XLFT*DCOS(GAM))-(YLFT*DSIN(GAM))
                  Y=(YLFT*DCOS(GAM))+(XLFT*DSIN(GAM))
                  XLFT=X+ALENS(13,I)
                  YLFT=Y+ALENS(12,I)
                  XLFT2=X+ALENS(13,I)
                  YLFT2=Y+ALENS(12,I)
C       TOP POINT
                  XTOP=0.0D0
                  YTOP=( ALENS(10,I))
                  X=(XTOP*DCOS(GAM))-(YTOP*DSIN(GAM))
                  Y=(YTOP*DCOS(GAM))+(XTOP*DSIN(GAM))
                  XTOP=X+ALENS(13,I)
                  YTOP=Y+ALENS(12,I)
                  XTOP2=X+ALENS(13,I)
                  YTOP2=Y+ALENS(12,I)
C       BOTTOM POINT
                  XBOT=0.0D0
                  YBOT=(-ALENS(10,I))
                  X=(XBOT*DCOS(GAM))-(YBOT*DSIN(GAM))
                  Y=(YBOT*DCOS(GAM))+(XBOT*DSIN(GAM))
                  XBOT=X+ALENS(13,I)
                  YBOT=Y+ALENS(12,I)
                  XBOT2=X+ALENS(13,I)
                  YBOT2=Y+ALENS(12,I)
              ELSE
C     CAFLG NOT 2
              END IF
C       CAFLG=3,OR 4
              IF(CAFLG.EQ.3.OR.CAFLG.EQ.4) THEN
C
                  GAM=(PII/180.0D0)*ALENS(15,I)
C       RIGHT POINT
                  XRHT=( ALENS(11,I))
                  YRHT=0.0D0
                  X=(XRHT*DCOS(GAM))-(YRHT*DSIN(GAM))
                  Y=(YRHT*DCOS(GAM))+(XRHT*DSIN(GAM))
                  XRHT=X+ALENS(13,I)
                  YRHT=Y+ALENS(12,I)
                  XRHT2=X+ALENS(13,I)
                  YRHT2=Y+ALENS(12,I)
C       LEFT POINT
                  XLFT=(-ALENS(11,I))
                  YLFT=0.0D0
                  X=(XLFT*DCOS(GAM))-(YLFT*DSIN(GAM))
                  Y=(YLFT*DCOS(GAM))+(XLFT*DSIN(GAM))
                  XLFT=X+ALENS(13,I)
                  YLFT=Y+ALENS(12,I)
                  XLFT2=X+ALENS(13,I)
                  YLFT2=Y+ALENS(12,I)
C       TOP POINT
                  XTOP=0.0D0
                  YTOP=( ALENS(10,I))
                  X=(XTOP*DCOS(GAM))-(YTOP*DSIN(GAM))
                  Y=(YTOP*DCOS(GAM))+(XTOP*DSIN(GAM))
                  XTOP=X+ALENS(13,I)
                  YTOP=Y+ALENS(12,I)
                  XTOP2=X+ALENS(13,I)
                  YTOP2=Y+ALENS(12,I)
C       BOTTOM POINT
                  XBOT=0.0D0
                  YBOT=(-ALENS(10,I))
                  X=(XBOT*DCOS(GAM))-(YBOT*DSIN(GAM))
                  Y=(YBOT*DCOS(GAM))+(XBOT*DSIN(GAM))
                  XBOT=X+ALENS(13,I)
                  YBOT=Y+ALENS(12,I)
                  XBOT2=X+ALENS(13,I)
                  YBOT2=Y+ALENS(12,I)
              ELSE
C     CAFLG NOT 2,3 OR 4
              END IF
C       CAFLG=1 CIRCULAR CLAP
              IF(CAFLG.EQ.1) THEN
C
                  PEEPEE=DABS(ALENS(10,I))
C
C     CLAP DEC MUST BE 0
                  IF(ALENS(12,I).EQ.0.0D0.AND.ALENS(13,I).EQ.0.0D0) THEN
C
C     WE HAVE A CIRCULAR CLAP. IS THE SURFACE ONE WHICH COULD HAVE A
C     FLAT ON IT. ONLY IF SURFACE IS CONCAVE TO AIR.
C
C     IF SURFACE I IS NOT AIR AND I+1 IS AIR
C     THEN IF CV>0 THERE MAY BE A FLAT TO DO
C     AS LONG AS SPECIAL SURFACE 18 NOT PRESENT
C
C
                      DOIT=.FALSE.
                      DOIT=ISAIR(I,POSDIR)
                      IF(DOIT) THEN
                          IF(ALENS(1,I).GT.0.0D0.AND.POSDIR.OR.
     1                    ALENS(1,I).LT.0.0D0.AND..NOT.POSDIR) THEN
C     ADJUST POINT
                              PEEPEE=DABS(ALENS(11,I))
                              IF(ALENS(1,I).GT.0.0D0) ZDELZ=-DABS(ALENS(14,I))
                              IF(ALENS(1,I).LT.0.0D0) ZDELZ=DABS(ALENS(14,I))
                          END IF
                      END IF
C
C     IF SURFACE I IS NOT AIR AND I+1 IS AIR
C     THEN IF CV<0 THERE MAY BE A FLAT TO DO
                      DOIT=.FALSE.
                      DOIT=ISAIR2(I,POSDIR)
                      IF(DOIT) THEN
                          IF(ALENS(1,I).LT.0.0D0.AND.POSDIR.OR.
     1                    ALENS(1,I).GT.0.0D0.AND..NOT.POSDIR) THEN
C     ADJUST POINT
                              PEEPEE=DABS(ALENS(11,I))
                              IF(ALENS(1,I).GT.0.0D0) ZDELZ=-DABS(ALENS(14,I))
                              IF(ALENS(1,I).LT.0.0D0) ZDELZ=DABS(ALENS(14,I))
                          END IF
                      END IF
                  ELSE
C     DEC CLAP, NO FLAT CALC
                  END IF
C
C       RIGHT POINT
                  XRHT2=(PEEPEE)
                  YRHT2=0.0D0
C       LEFT POINT
                  XLFT2=(-PEEPEE)
                  YLFT2=0.0D0
C       TOP POINT
                  XTOP2=0.0D0
                  YTOP2=(PEEPEE)
C       BOTTOM BOINT
                  XBOT2=0.0D0
                  YBOT2=(-PEEPEE)
              ELSE
C       CAFLG NOT 1
              END IF
C       COFLG=0 NO COBS
              IF(COFLG.EQ.0) THEN
                  XLFTO=0.0D0
                  YLFTO=0.0D0
                  XRHTO=0.0D0
                  YRHTO=0.0D0
                  XBOTO=0.0D0
                  YBOTO=0.0D0
                  XTOPO=0.0D0
                  YTOPO=0.0D0
                  RETURN
              ELSE
C     MUST BE SOME COBS, CONTINUE
              END IF
C       COFLG=1 CIRCULAR COBS
              IF(COFLG.EQ.1) THEN
C       RIGHT POINT
                  XRHTO=( ALENS(17,I))+ALENS(20,I)
                  YRHTO=0.0D0
C       LEFT POINT
                  XLFTO=(-ALENS(17,I))+ALENS(20,I)
                  YLFTO=0.0D0
C       TOP POINT
                  XTOPO=0.0D0
                  YTOPO=( ALENS(17,I))+ALENS(19,I)
C       BOTTOM BOINT
                  XBOTO=0.0D0
                  YBOTO=(-ALENS(17,I))+ALENS(19,I)
              ELSE
C       COFLG NOT 1
              END IF
C
C       COFLG=2,3,OR 4
              IF(COFLG.EQ.2.OR.COFLG.EQ.3.OR.COFLG.EQ.4) THEN
C
                  GAM=(PII/180.0D0)*ALENS(22,I)
C       RIGHT POINT
                  XRHTO=( ALENS(18,I))
                  YRHTO=0.0D0
                  X=(XRHTO*DCOS(GAM))-(YRHTO*DSIN(GAM))
                  Y=(YRHTO*DCOS(GAM))+(XRHTO*DSIN(GAM))
                  XRHTO=X+ALENS(20,I)
                  YRHTO=Y+ALENS(19,I)
C       LEFT POINT
                  XLFTO=(-ALENS(18,I))
                  YLFTO=0.0D0
                  X=(XLFTO*DCOS(GAM))-(YLFTO*DSIN(GAM))
                  Y=(YLFTO*DCOS(GAM))+(XLFTO*DSIN(GAM))
                  XLFTO=X+ALENS(20,I)
                  YLFTO=Y+ALENS(19,I)
C       TOP POINT
                  XTOPO=0.0D0
                  YTOPO=( ALENS(17,I))
                  X=(XTOPO*DCOS(GAM))-(YTOPO*DSIN(GAM))
                  Y=(YTOPO*DCOS(GAM))+(XTOPO*DSIN(GAM))
                  XTOPO=X+ALENS(20,I)
                  YTOPO=Y+ALENS(19,I)
C       BOTTOM POINT
                  XBOTO=0.0D0
                  YBOTO=(-ALENS(17,I))
                  X=(XBOTO*DCOS(GAM))-(YBOTO*DSIN(GAM))
                  Y=(YBOTO*DCOS(GAM))+(XBOTO*DSIN(GAM))
                  XBOTO=X+ALENS(20,I)
                  YBOTO=Y+ALENS(19,I)
              ELSE
C     COFLG NOT 2,3 OR 4
              END IF
C     NOW FOR SOME SURFACE TYPES, THERE IS A PREDICTABLE
C     EXTENT BEYOND WHICH THE SAG OF THE SURFACE WILL NOT
C     BE A REAL NUMBER. IF THE LIMITS CALCULATED ABOVE EXCEED
C     THESE LIMITS, THEN ADJUST THE LIMITS BEFORE PROCEEDING.
          ELSE
C     TYPE 18 SPECIAL SURFACE AND IT IS "ON"
C     CEE IS THE CURVATURE CV (ALENS(1,I))
              CEE=ALENS(1,I)
C     KAPPA IS THE CONIC CONSTANT CC (ALENS(2,I))
              KAPPA=ALENS(2,I)
C
C     ZEE IS THE Z POSITION FOR WHICH A RHO IS DESIRED
C     FIRST THE FORE OR FRONT POSITION
              ZEE=FTFL01(1,I)
              RHO2=(((2.0D0*ZEE)-(CEE*(KAPPA+1.0D0)*(ZEE**2)))/CEE)
              IF(RHO2.LT.0.0D0) THEN
                  OUTLYNE=
     1            'UN-REALISTIC FRONT Z-POSITION IN TYPE 18 SPECIAL SURFACE'
                  CALL SHOWIT(1)
                  OUTLYNE='NO PROFILE COULD BE DRAWN'
                  CALL SHOWIT(1)
                  XLFT=0.0D0
                  YLFT=0.0D0
                  XRHT=0.0D0
                  YRHT=0.0D0
                  XTOP=0.0D0
                  YTOP=0.0D0
                  XBOT=0.0D0
                  YBOT=0.0D0
                  XLFTO=0.0D0
                  YLFTO=0.0D0
                  XRHTO=0.0D0
                  YRHTO=0.0D0
                  XTOPO=0.0D0
                  YTOPO=0.0D0
                  XBOTO=0.0D0
                  YBOTO=0.0D0
                  RETURN
              END IF
              RHO=DSQRT(RHO2)
              XLFT=-RHO
              YLFT=0.0D0
              XRHT=RHO
              YRHT=0.0D0
              XTOP=0.0D0
              YTOP=RHO
              XBOT=0.0D0
              YBOT=-RHO
C     THEN THE AFT OR BACK POSITION
              ZEE=FTFL01(2,I)
              RHO2=(((2.0D0*ZEE)-(CEE*(KAPPA+1.0D0)*(ZEE**2)))/CEE)
              IF(RHO2.LT.0.0D0) THEN
                  OUTLYNE=
     1            'UN-REALISTIC REAR Z-POSITION IN TYPE 18 SPECIAL SURFACE'
                  CALL SHOWIT(1)
                  OUTLYNE='NO PROFILE COULD BE DRAWN'
                  CALL SHOWIT(1)
                  XLFT=0.0D0
                  YLFT=0.0D0
                  XRHT=0.0D0
                  YRHT=0.0D0
                  XTOP=0.0D0
                  YTOP=0.0D0
                  XBOT=0.0D0
                  YBOT=0.0D0
                  XLFTO=0.0D0
                  YLFTO=0.0D0
                  XRHTO=0.0D0
                  YRHTO=0.0D0
                  XTOPO=0.0D0
                  YTOPO=0.0D0
                  XBOTO=0.0D0
                  YBOTO=0.0D0
                  RETURN
              END IF
              RHO=DSQRT(RHO2)
              XLFTO=-RHO
              YLFTO=0.0D0
              XRHTO=RHO
              YRHTO=0.0D0
              XTOPO=0.0D0
              YTOPO=RHO
              XBOTO=0.0D0
              YBOTO=-RHO
          END IF
          RETURN
      END
C SUB CAO5.FOR
C
      SUBROUTINE CAO5(X1,X2,X3,X4,Y1,Y2,Y3,Y4,I)
C
C     DOES EDGES WHEN I AND I+1 HAVE RECTANGULAR CLAPS
C
          IMPLICIT NONE
C
          INTEGER I
C
          REAL*8 X,Y,X1,X2,X3,X4,Y1,Y2,Y3,Y4
     1    ,GAM

C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
C
C       SET FLAGS CAFLG AND COFLG. CAFLG2 TO SUPPORT RECT CLAPS ON I AND I+1
          GAM=0.0D0
C
          GAM=(PII/180.0D0)*ALENS(15,I)
C       X1,Y1
          X1=ALENS(11,I)
          Y1=ALENS(10,I)
          X=(X1*DCOS(GAM))-(Y1*DSIN(GAM))
          Y=(Y1*DCOS(GAM))+(X1*DSIN(GAM))
          X1=X+ALENS(13,I)
          Y1=Y+ALENS(12,I)
C       X2,Y2
          X2=-ALENS(11,I)
          Y2=ALENS(10,I)
          X=(X2*DCOS(GAM))-(Y2*DSIN(GAM))
          Y=(Y2*DCOS(GAM))+(X2*DSIN(GAM))
          X2=X+ALENS(13,I)
          Y2=Y+ALENS(12,I)
C       X3,Y3
          X3=-ALENS(11,I)
          Y3=-ALENS(10,I)
          X=(X3*DCOS(GAM))-(Y3*DSIN(GAM))
          Y=(Y3*DCOS(GAM))+(X3*DSIN(GAM))
          X3=X+ALENS(13,I)
          Y3=Y+ALENS(12,I)
C       X4,Y4
          X4=ALENS(11,I)
          Y4=-ALENS(10,I)
          X=(X4*DCOS(GAM))-(Y4*DSIN(GAM))
          Y=(Y4*DCOS(GAM))+(X4*DSIN(GAM))
          X4=X+ALENS(13,I)
          Y4=Y+ALENS(12,I)
          RETURN
      END
C SUB PLTCLP.FOR
      SUBROUTINE PLTCLP(CLPTYPE,SURFACEI,SFI,MDX,MDY,GAMGAM)
          USE GLOBALS
C
          IMPLICIT NONE
C
C       THIS ROUTINE DOES THE PLOT CLAP COMMAND
C
          REAL*8 X,Y,Z,XN,YN,ZN,ROT1X,ROT1Z,ROT2Y,MDX,MDY,GAMGAM
     1    ,ROT2Z,AX,AY,AZ,AALF,APHI,YMAXI,YMINI,XMAXI,XMINI
     2    ,XNEW,YNEW,LKG,VIEPH,VIEAL,Z1,ANGLE,AN2,ZDELZ
     3    ,X00,Y00,Z0,LX0,LY0,LZ0,ZCORR,SFI
     4    ,X1,Y1,MX0,MY0,MZ0,NX0,NY0,NZ0,XID,YID
C
          INTEGER KKK,JJSTOP,J,IK,III,NO,CLRR,JJ,ALLOERR,IIRUN,CLPTYPE
C
          INTEGER M1,M2,M3,M4,M5,IX,IY,I,II,IPST,POINT(1:2,1:3)
C
          INTEGER SURFACEI
C
          INTEGER COLPAS
C
          LOGICAL SECPLT(0:499),NOPLOT
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
          REAL*8 CLPDAT
          DIMENSION CLPDAT(:,:,:,:)
          ALLOCATABLE :: CLPDAT
C
C     LOOK.VIEW TRANSFORMS

          ROT1X(AX,AZ,APHI)=((AX*DCOS(APHI))-(AZ*DSIN(APHI)))
          ROT1Z(AX,AZ,APHI)=((AX*DSIN(APHI))+(AZ*DCOS(APHI)))
C
          ROT2Z(AZ,AY,AALF)=((AZ*DCOS(AALF))+(AY*DSIN(AALF)))
          ROT2Y(AZ,AY,AALF)=((-AZ*DSIN(AALF))+(AY*DCOS(AALF)))
C
          II=SURFACEI
          NOPLOT=.FALSE.
          IF(DF5.EQ.0) NOPLOT=.TRUE.
C
          DO KKK=1,2
C         KKK IS USED TO PERFORM MIRROR BACKING PLOTS WHEN KKK=2
              M1=0
              M2=360
              M3=3
              M4=INT(SYSTEM1(20))
              M5=2
              DEALLOCATE(CLPDAT,STAT=ALLOERR)
              ALLOCATE(CLPDAT(M1:M2,M3,M1:M4,M5),STAT=ALLOERR)
C
              VIEPH=(PII/180.0D0)*VIEPHI
              VIEAL=(PII/180.0D0)*VIEALF
C
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
                  DEALLOCATE(CLPDAT,STAT=ALLOERR)
                  RETURN
              END IF
              GLOBE=.TRUE.
              OFFX=0.0D0
              OFFY=0.0D0
              OFFZ=0.0D0
              OFFA=0.0D0
              OFFB=0.0D0
              OFFC=0.0D0
              CALL GLVERT
              GLOBE=.FALSE.
C
              DO IIRUN=1,2
                  CLPDAT(0:360,1:3,0:M4,1:2)=0.0
                  X=0.0D0
                  Y=0.0D0
                  XID=0.0D0
                  YID=0.0D0
C
C       ALL INPUT IS OK, KEEP GOING
C     THE ARRAY CONTAINING SURFACE CLAP DATA IS:
C     CLPDAT(0:360,1:3,0:MAXSUR,1:2)
C
C     THE FIRST DIMENSION IS FOR THE DATA POINT NUMBER
C     THE SECOND DIMENSION IS FOR THE X,Y AND Z COORDINATES OR THE POINT
C     THE THIRD IS THE SURFACE NUMBER
C
C     WE NEED TO LOAD THE ARRAY BEFORE PLOTTING
C
C     THE PROCEDURE IS:
C
C     CYCLE THROUGH ALL THE SURFACES
C
                  IF(ALENS(34,II).EQ.18.0D0) JJSTOP=2
                  IF(ALENS(34,II).NE.18.0D0) JJSTOP=1
                  DO JJ=1,JJSTOP
C
C     1. WE WILL CLOCK AROUND THE CLEAR APERTURE FROM THE LOCAL +X
C     TOWARD THE LOCAL +Y AXIS,
C     4.0 DEGREE INCREMENTS AS MEASURED
C     BY AN OBSERVER AT THE SURFACE VERTEX, IN THE LOCAL COORDINATE
C     SYSTEM OF THE SURFACE, WITH THE OBSERVER FACING THE -Z AXIS
C     DIRECTION
C
                      DO J=0,360
                          ANGLE=(DBLE(J)*PII)/180.0D0
                          AN2=(DBLE(J+1)*PII)/180.0D0
C     NOW ALONG THIS ANGLED LINE, WHAT ARE THE X AND Y COORDINATES
C     OF THE CLEAR APERTURE
                          III=II
C
                          CALL CAO1(X,Y,ANGLE,III,AN2,JJ,XID,YID,IIRUN,CLPTYPE,ZDELZ
     1                    ,MDX,MDY,GAMGAM)
C
C     THE RETURNED X AND Y ARE WHERE THE SAG IS TO BE CALCULATED
C
C     2. USE APPROPRIATE CALLS TO THE SAGPLT.FOR ROUTINE
C               TO CALCULATE THE SAG AND MAKE
C               CERTAIN THE SIGN IS CORRECT FOR A LOCAL Z COORDINATE
C
C
C     CALLS TO SAGPLT GO HERE
                          III=II
                          XID=XID*SFI
                          YID=YID*SFI
                          CALL SAGPLT(III,XID,YID,Z,NO)
                          IF(CLPTYPE.EQ.1) Z=Z+ZDELZ
C
C     ASSIGN ARRAY VALUES BASED ON J VALUE
                          CLPDAT(J,1,II,JJ)=X*SFI
                          CLPDAT(J,2,II,JJ)=Y*SFI
C
                          SECPLT(II)=.FALSE.
                          IF(KKK.EQ.2) THEN
                              IF(ALENS(110,II).NE.0.0D0) THEN
                                  ZCORR=DABS(ALENS(110,II))
                                  IF(ALENS(46,II).LT.0.0D0) Z=Z+ZCORR
                                  IF(ALENS(46,II).GT.0.0D0) Z=Z-ZCORR
                              END IF
                              SECPLT(II)=.TRUE.
                          ELSE
                              SECPLT(II)=.FALSE.
                          END IF
                          CLPDAT(J,3,II,JJ)=Z
C
C               CYCLE THROUGH THE NEXT DATA PAIR
                      END DO
C               CYCLE THROUGH THE NEXT JJ
                  END DO
C
C     3. THE ARRAYS NOW HAVE LOCAL X,Y AND Z VALUES STORED IN THEM
C     CONVERT THE LOCAL X ANY Y CLAPS TO GLOBAL NUMBERS
C     GLOBAL VERTEX DATA IS
                  IF(ALENS(34,II).EQ.18.0D0) JJSTOP=2
                  IF(ALENS(34,II).NE.18.0D0) JJSTOP=1
                  DO JJ=1,JJSTOP
                      DO I=0,360
                          X00=VERTEX(1,II)
                          Y00=VERTEX(2,II)
                          Z0=VERTEX(3,II)
                          LX0=VERTEX(4,II)
                          MX0=VERTEX(5,II)
                          NX0=VERTEX(6,II)
                          LY0=VERTEX(7,II)
                          MY0=VERTEX(8,II)
                          NY0=VERTEX(9,II)
                          LZ0=VERTEX(10,II)
                          MZ0=VERTEX(11,II)
                          NZ0=VERTEX(12,II)
                          X=CLPDAT(I,1,II,JJ)
                          Y=CLPDAT(I,2,II,JJ)
                          Z=CLPDAT(I,3,II,JJ)
C
                          X1=X00+((LX0*(X))+(LY0*(Y))
     1                    +(LZ0*(Z)))
                          Y1=Y00+((MX0*(X))+(MY0*(Y))
     1                    +(MZ0*(Z)))
                          Z1=Z0+((NX0*(X))+(NY0*(Y))
     1                    +(NZ0*(Z)))
                          CLPDAT(I,1,II,JJ)=X1
                          CLPDAT(I,2,II,JJ)=Y1
                          CLPDAT(I,3,II,JJ)=Z1
                      END DO
                  END DO
C
C     4. NOW DETERMINE THE BEST PLACE FOR THE ROTATION POINT FOR
C               PLOT LOOK/VIEW
C
C     ROT 3 IS LIKE ROT2 BUT KEYS OFF CLAP DATA NOT PROF DATA
                  CALL ROT3(JJ,CLPDAT,M1,M2,M3,M4,M5)
C
C     5.  CONVERT THE GLOBAL X AND Y CLAP VALUES
C               USING THE LOOK/VIEW VALUES
                  IF(ALENS(34,II).EQ.18.0D0) JJSTOP=2
                  IF(ALENS(34,II).NE.18.0D0) JJSTOP=1
                  DO JJ=1,JJSTOP
                      DO I=0,360
                          X=CLPDAT(I,1,II,JJ)
                          Y=CLPDAT(I,2,II,JJ)
                          Z=CLPDAT(I,3,II,JJ)
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
                          CLPDAT(I,1,II,JJ)=XN
                          CLPDAT(I,2,II,JJ)=YN
                          CLPDAT(I,3,II,JJ)=ZN
                      END DO
                  END DO
C
C     THE ARRAYS NOW HAVE GLOBAL SURFACE CLAP DATA IN THEM
C
C     6.IF NEEDED, DETERMINE SCALE FACTORS AND PLOT RANGE
C
C     PLTSC3 IS LIKE PLTSC2 BUT KEYS OFF CLAP DATA NOT PROF DATA
C
                  CALL PLTSC3(XMINI,XMAXI,YMINI,YMAXI,JJ,CLPDAT,M1,M2,M3,M4,M5)
C
C     7.CYCLE THROUGH THE THE ARRAYS, APPLY SCALE FACTORS
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
C
                  IF(ALENS(34,II).EQ.18.0D0) JJSTOP=2
                  IF(ALENS(34,II).NE.18.0D0) JJSTOP=1
                  DO JJ=1,JJSTOP
                      DO I=0,360
                          CLPDAT(I,1,II,JJ)=(CLPDAT(I,1,II,JJ)/SCFAX)*1000.0D0
                          CLPDAT(I,2,II,JJ)=(CLPDAT(I,2,II,JJ)/SCFAY)*1000.0D0
                      END DO
                  END DO
C
C     8. APPLY THE XSHIFT AND YSHIFT VALUES
                  DO I=0,360
                      IF(ALENS(34,II).EQ.18.0D0) JJSTOP=2
                      IF(ALENS(34,II).NE.18.0D0) JJSTOP=1
                      DO JJ=1,JJSTOP
                          IF(LORIENT) CALL ORSHIFT
                          CLPDAT(I,1,II,JJ)=CLPDAT(I,1,II,JJ)+DBLE(PXSHFT)
                          CLPDAT(I,2,II,JJ)=CLPDAT(I,2,II,JJ)+3500.0D0+DBLE(PYSHFT)
                      END DO
                  END DO
C
C     9. SET THE PLOT JUSTIFICATION IF NEEDED
C     IF THERE ARE X-OFFSETS (JUSTIFICATION) TO APPLY, DO THEM HERE.
C
C     NOW
                  IF(RCL.EQ.1.OR.RCL.EQ.-1) THEN
                      JUSOFF=500.0D0-((XMINI/SCFAX)*1000.0D0)
                      RCL=-1
                  ELSE
                  END IF
                  IF(RCL.EQ.2.OR.RCL.EQ.-2) THEN
                      RCL=-2
                      JUSOFF=5000.0D0
                  ELSE
                  END IF
                  IF(RCL.EQ.3.OR.RCL.EQ.-3) THEN
                      JUSOFF=9500.0D0-((XMAXI/SCFAX)*1000.0D0)
                      RCL=-3
                  ELSE
                  END IF
C
                  DO I=0,360
                      IF(ALENS(34,II).EQ.18.0D0) JJSTOP=2
                      IF(ALENS(34,II).NE.18.0D0) JJSTOP=1
                      DO JJ=1,JJSTOP
                          CLPDAT(I,1,II,JJ)=CLPDAT(I,1,II,JJ)+JUSOFF
                      END DO
                  END DO
C     9. PLOT GAMMA
C     IF THERE IS A NON-ZERO GAMMA SPECIFIED IN PLOT LOOK OR PLOT VIEW THEN
C     ROTATE IN GAMMA ON THE SCREEN ABOUT THE CENTER OF THE SCREEN
C     WHICH HAS COORDINATES X=5000.0D0,Y=3500.0D0
C
C     FIRST SHIFT THE COORDINATE ORIGIN TO THE CENTER OF THE DISPLAY
C
                  DO I=0,360
                      IF(ALENS(34,II).EQ.18.0D0) JJSTOP=2
                      IF(ALENS(34,II).NE.18.0D0) JJSTOP=1
                      DO JJ=1,JJSTOP
                          CLPDAT(I,1,II,JJ)=CLPDAT(I,1,II,JJ)-5000.0D0
                          CLPDAT(I,2,II,JJ)=CLPDAT(I,2,II,JJ)-3500.0D0
                      END DO
                  END DO
C     THE SCREEN COORDINATE IN REAL*8 IN THE SHIFTED COORDINATE
C     FRAME IS NOW X AND Y

                  IF(DBLE(PGAMMA).NE.0.0D0) THEN
                      LKG=(PII/180.0D0)*DBLE(PGAMMA)

                      DO I=0,360
                          IF(ALENS(34,II).EQ.18.0D0) JJSTOP=2
                          IF(ALENS(34,II).NE.18.0D0) JJSTOP=1
                          DO JJ=1,JJSTOP
                              X=CLPDAT(I,1,II,JJ)
                              Y=CLPDAT(I,2,II,JJ)
                              XNEW=((X*DCOS(LKG))-(Y*DSIN(LKG)))
                              YNEW=((X*DSIN(LKG))+(Y*DCOS(LKG)))
                              CLPDAT(I,1,II,JJ)=XNEW
                              CLPDAT(I,2,II,JJ)=YNEW
                          END DO
                      END DO
                  END IF
C     THE ROTATION IS DONE, NOW SHIFT THE ORIGIN BACK TO THE BOTTOM
C     LEFT HAND CORNER
                  DO I=0,360
                      IF(ALENS(34,II).EQ.18.0D0) JJSTOP=2
                      IF(ALENS(34,II).NE.18.0D0) JJSTOP=1
                      DO JJ=1,JJSTOP
                          CLPDAT(I,1,II,JJ)=CLPDAT(I,1,II,JJ)+5000.0D0
                          CLPDAT(I,2,II,JJ)=CLPDAT(I,2,II,JJ)+3500.0D0
                      END DO
                  END DO
C
                  IF(ALENS(34,II).EQ.18.0D0) JJSTOP=2
                  IF(ALENS(34,II).NE.18.0D0) JJSTOP=1
                  DO JJ=1,JJSTOP
C     NOW DRAW THE CLAP AT SURFACE I
C     WITH THE PEN UP, GO TO THE STARTING PLOT POSITION
                      IF(.NOT.PLEXIS) PLEXIS=.TRUE.
                      DO J=0,360
C     PUT INSTRUCTIONS IN P1ARAY TO DROP PEN AND DRAW
                          IF(J.EQ.0) IPST=0
                          IF(J.NE.0) IPST=1
                          IF(CLPDAT(J,1,II,JJ).GT.1.0D6) CLPDAT(J,1,II,JJ)=1.0D6
                          IF(CLPDAT(J,2,II,JJ).GT.1.0D6) CLPDAT(J,2,II,JJ)=1.0D6
                          IF(CLPDAT(J,1,II,JJ).LT.-1.0D6) CLPDAT(J,1,II,JJ)=-1.0D6
                          IF(CLPDAT(J,2,II,JJ).LT.-1.0D6) CLPDAT(J,2,II,JJ)=-1.0D6
                          IX=INT(CLPDAT(J,1,II,JJ))
                          IY=INT(CLPDAT(J,2,II,JJ))
                          P1ARAY(J,1,1)=IX
                          P1ARAY(J,2,1)=IY
                          P1ARAY(J,3,1)=IPST

                          IF(.NOT.PLEXIS) PLEXIS=.TRUE.
                      END DO
C     FINISHED WITH THAT CLAP, LIFT PEN
                      IPST=0
C     NOW ISSUE THE PLOTTING COMMANDS STORED IN THE P1ARAY ARRAY
C
C     LINE TYPE SETTING
                      COLPAS=COLCLP
                      CALL MY_COLTYP(COLPAS)
                      OLLNTP=LNTYPE
                      LNTYPE=0
C     DASH, SOLID OR INVISIBLE
                      CLRR=0
                      IF(DUMMMY(II).AND.ALENS(9,II).EQ.0.0D0) CLRR=-1
                      IF(DUMMMY(II).AND.ALENS(9,II).NE.0.0D0) THEN
                          IF(DASHH) LNTYPE=2
                      ELSE
C     LEAVE LINE ALONE
                      END IF
                      IF(CLRR.NE.-1) THEN
                          FIXUP=.FALSE.
C
                          DO IK=0,360
                              IF(IK.EQ.360) P1ARAY(IK,3,1)=0
                              IF(IK.GT.0) THEN
                                  IF(P1ARAY(IK-1,1,1).LE.0.OR.P1ARAY(IK-1,2,1).LE.0) THEN
                                      P1ARAY(IK,3,1)=0
                                  END IF
                                  IF(P1ARAY(IK-1,1,1).GE.10000.OR.P1ARAY(IK-1,2,1).GE.7000) THEN
                                      P1ARAY(IK,3,1)=0
                                  END IF
                                  IF(P1ARAY(IK,1,1).LE.0.OR.P1ARAY(IK,2,1).LE.0) THEN
                                      P1ARAY(IK,3,1)=0
                                  END IF
                                  IF(P1ARAY(IK,1,1).GE.10000.OR.P1ARAY(IK,2,1).GE.7000) THEN
                                      P1ARAY(IK,3,1)=0
                                  END IF
                              END IF
                              IF(KKK.EQ.1.AND.IK.EQ.0) THEN
                                  POINT(1,1)=P1ARAY(IK,1,1)
                                  POINT(1,2)=P1ARAY(IK,2,1)
                                  POINT(1,3)=P1ARAY(IK,3,1)

                              END IF
                              IF(KKK.EQ.1.AND.IK.EQ.360) THEN
                                  POINT(2,1)=P1ARAY(IK,1,1)
                                  POINT(2,2)=P1ARAY(IK,2,1)
                                  POINT(2,3)=P1ARAY(IK,3,1)

                              END IF

!     Draw lens edge

                              IF(KKK.EQ.2.AND.SECPLT(II)) THEN
                                  IF(.NOT.NOPLOT.OR.NOPLOT.AND.ALENS(9,II).NE.0.0D0)
!     1CALL PENMV1(P1ARAY(IK,1,1),P1ARAY(IK,2,1),P1ARAY(IK,3,1))
     &                            call drawdatasave(P1ARAY(IK,1,1),P1ARAY(IK,2,1),P1ARAY(IK,3,1),
     &                            3)
                              END IF
                              IF(KKK.EQ.1) THEN
                                  IF(.NOT.NOPLOT.OR.NOPLOT.AND.ALENS(9,II).NE.0.0D0)
!     1CALL PENMV1(P1ARAY(IK,1,1),P1ARAY(IK,2,1),P1ARAY(IK,3,1))
     &                            call drawdatasave(P1ARAY(IK,1,1),P1ARAY(IK,2,1),P1ARAY(IK,3,1),
     &                            3)

                              END IF
                          END DO
                      ELSE
                          CLRR=0
                      END IF
                      LNTYPE=OLLNTP
                  END DO

C     THIS LAST END DO ID FOR THE IIRUN LOOP
              END DO
C     DO THE NEXT KKK LOOP FOR MIRROR BACKS
          END DO
C
C     HERE WE DO THE PLOT LI AND PLOT AXIS DRAWING
          IF(.NOT.VIGFLG.AND.PLTVIG) THEN
              CALL VIGSHO
              VIGFLG=.TRUE.
          ELSE
          END IF
          DEALLOCATE(CLPDAT,STAT=ALLOERR)
          LNTYPE=0
          RETURN
      END


      FUNCTION ISITIN(X,Y,I)
          IMPLICIT NONE
          REAL*8 GAM,ANGLE,XR,YR,X,Y,XRD,YRD
          LOGICAL INS,INSID1,ISITIN
          EXTERNAL INSID1
          INTEGER III,I
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
          ANGLE=0.0D0
          DO III=1,INT(ALENS(11,I))
              XT(III)=ALENS(10,I)*DCOS(ANGLE+(PII/2.0D0))
              YT(III)=ALENS(10,I)*DSIN(ANGLE+(PII/2.0D0))
              ANGLE=ANGLE+((TWOPII)/ALENS(11,I))
          END DO
          XRD=X
          YRD=Y
C
          GAM=(PII/180.0D0)*ALENS(15,I)
          XR=(XRD*DCOS(GAM))+(YRD*DSIN(GAM))
          YR=(YRD*DCOS(GAM))-(XRD*DSIN(GAM))
C
c        XR=XR-(ALENS(13,I)+MULTX)
c        YR=YR-(ALENS(12,I)+MULTY)
C
C       ARE THE POINTS XR AND YR ON THE POLYGON OR OUTSIDE
          X0=XR
          Y0=YR
          NP=INT(ALENS(11,I))
          INS=INSID1()
          IF(INS) THEN
              ISITIN=.TRUE.
          ELSE
              ISITIN=.FALSE.
          END IF
          RETURN
      END


      FUNCTION ISITIN2(X,Y,I)
          IMPLICIT NONE
          REAL*8 GAM,ANGLE,XR,YR,X,Y,XRD,YRD
          LOGICAL INS,INSID2,ISITIN2
          EXTERNAL INSID2
          INTEGER III,I
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
          ANGLE=0.0D0
          DO III=1,INT(ALENS(18,I))
              XT(III)=ALENS(17,I)*DCOS(ANGLE+(PII/2.0D0))
              YT(III)=ALENS(17,I)*DSIN(ANGLE+(PII/2.0D0))
              ANGLE=ANGLE+((TWOPII)/ALENS(18,I))
          END DO
          XRD=X
          YRD=Y
C
          GAM=(PII/180.0D0)*ALENS(22,I)
          XR=(XRD*DCOS(GAM))+(YRD*DSIN(GAM))
          YR=(YRD*DCOS(GAM))-(XRD*DSIN(GAM))
C
          XR=XR-ALENS(20,I)
          YR=YR-ALENS(19,I)
C
C       ARE THE POINTS XR AND YR ON THE POLYGON OR OUTSIDE
          X0=XR
          Y0=YR
          NP=INT(ALENS(18,I))
          INS=INSID2()
          IF(INS) THEN
              ISITIN2=.TRUE.
          ELSE
              ISITIN2=.FALSE.
          END IF
          RETURN
      END


      FUNCTION ISITINI(X,Y,I)
          IMPLICIT NONE
          REAL*8 GAM,XR,YR,X,Y,XRD,YRD
          LOGICAL INS,INSID1,ISITINI
          EXTERNAL INSID1
          INTEGER III,I
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
          DO III=1,INT(ALENS(11,I))
              XT(III)=IPOLYX(III,I,1)
              YT(III)=IPOLYY(III,I,1)
          END DO
          XRD=X
          YRD=Y
C
          GAM=(PII/180.0D0)*ALENS(15,I)
          XR=(XRD*DCOS(GAM))+(YRD*DSIN(GAM))
          YR=(YRD*DCOS(GAM))-(XRD*DSIN(GAM))
C
C
C       ARE THE POINTS XR AND YR ON THE POLYGON OR OUTSIDE
          X0=XR
          Y0=YR
          NP=INT(ALENS(11,I))
          INS=INSID1()
          IF(INS) THEN
              ISITINI=.TRUE.
          ELSE
              ISITINI=.FALSE.
          END IF
          RETURN
      END


      FUNCTION ISITIN2I(X,Y,I)
          IMPLICIT NONE
          REAL*8 GAM,XR,YR,X,Y,XRD,YRD
          LOGICAL INS,INSID2,ISITIN2I
          EXTERNAL INSID2
          INTEGER III,I
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
          DO III=1,INT(ALENS(18,I))
              XT(III)=IPOLYX(III,I,3)
              YT(III)=IPOLYY(III,I,3)
          END DO
          XRD=X
          YRD=Y
C
          GAM=(PII/180.0D0)*ALENS(22,I)
          XR=(XRD*DCOS(GAM))+(YRD*DSIN(GAM))
          YR=(YRD*DCOS(GAM))-(XRD*DSIN(GAM))
C
          XR=XR-ALENS(20,I)
          YR=YR-ALENS(19,I)
C
C       ARE THE POINTS XR AND YR ON THE POLYGON OR OUTSIDE
          X0=XR
          Y0=YR
          NP=INT(ALENS(18,I))
          INS=INSID2()
          IF(INS) THEN
              ISITIN2I=.TRUE.
          ELSE
              ISITIN2I=.FALSE.
          END IF
          RETURN
      END

