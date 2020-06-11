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

C       FIFTH FILE OF PLOT/CAD ROUTINES

C SUB MAKEPNOTE.FOR
      SUBROUTINE MAKEPNOTE
C
          IMPLICIT NONE
C
C       THIS ROUTINE DOES THE "PNOTE" COMMAND AT THE CMD LEVEL
C
          CHARACTER BL20*20,BLNOTE*80
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          INCLUDE 'dathgr.inc'
C
          BL20='                    '
          BLNOTE=BL20//BL20//BL20//BL20
C
C       SET PLOT NAME
C       CHECK SYNTAX
          IF(SN.EQ.1.OR.SQ.EQ.1) THEN
              OUTLYNE='"PNOTE" TAKES NO QUALIFIER OR NUMERIC INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          ELSE
          END IF
C       STI
          IF(STI.EQ.1) THEN
              IF(PLOTNOTE.EQ.BLNOTE) THEN
                  OUTLYNE='THE CURRENT PLOT NOTE IS BLANK'
                  CALL SHOWIT(1)
                  OUTLYNE='AND WILL NOT BE PLOTTED'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              ELSE
C       PRINT OUT THE CURRENT PLOT NOTE
                  WRITE(OUTLYNE,15)
                  CALL SHOWIT(0)
 15               FORMAT('THE FOLLOWING IS THE CURRENT PLOT NOTE:')
                  WRITE(OUTLYNE,16) PLOTNOTE(1:79)
                  CALL SHOWIT(0)
 16               FORMAT(A79)
              END IF
              RETURN
          END IF
          PLOTNOTE(1:80)=WS(1:80)
          RETURN
      END
C SUB PLNOTE.FOR
      SUBROUTINE PLNOTE
C
          IMPLICIT NONE
C
C       THIS ROUTINE DOES THE "PLOT NOTE" COMMAND AT THE CMD LEVEL
C
          CHARACTER BL20*20,BLNOTE*80,NNTT*80
C
          INTEGER COLPAS,IIX,IIY,IB,I
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          INCLUDE 'dathgr.inc'
C
          BL20='                    '
          BLNOTE=BL20//BL20//BL20//BL20
C
C       SET PLOT NAME
C       CHECK SYNTAX
          IF(W3.NE.0.0D0.OR.W4.NE.0.0D0.OR.W5.NE.0.0D0.OR.SST.EQ.1) THEN
              OUTLYNE='"PLOT NOTE" TAKES NO STRING OR '
              CALL SHOWIT(1)
              OUTLYNE='NUMERIC WORD #3, #4 OR #5 INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          ELSE
          END IF
          IF(S1.EQ.0.OR.S2.EQ.0) THEN
              OUTLYNE=
     1        '"PLOT NOTE" REQUIRES EXPLICIT NUMERIC WORD #1 AND #2 INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          ELSE
          END IF
C       STI
          IF(STI.EQ.1) THEN
              IF(PLOTNOTE.EQ.BLNOTE) THEN
                  OUTLYNE='THE CURRENT PLOT NOTE IS BLANK'
                  CALL SHOWIT(1)
                  OUTLYNE='AND WILL NOT BE PLOTTED'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              ELSE
C       PRINT OUT THE CURRENT PLOT NOTE
                  WRITE(OUTLYNE,15)
                  CALL SHOWIT(0)
 15               FORMAT('THE FOLLOWING IS THE CURRENT PLOT NOTE:')
                  WRITE(OUTLYNE,16) PLOTNOTE(1:79)
                  CALL SHOWIT(0)
 16               FORMAT(A79)
              END IF
              RETURN
          ELSE
C       EXPLICT INPUT EXISTS, PLOT IT
          END IF
          NNTT=PLOTNOTE(1:80)
C       DO THE PLOTTING OF THE LENS NOTE AT THE CURRENT
C       PLOT POSITION
          IB=1
          DO I=80,1,-1
              IF(NNTT(I:I).NE.' ') THEN
                  IB=I
                  GO TO 100
              END IF
          END DO
 100      CONTINUE
          COLPAS=COLLBL
          CALL MY_COLTYP(COLPAS)
          IIX=INT(W1)
          IIY=INT(W2)
          IF(.NOT.PLEXIS) PLEXIS=.TRUE.
          CALL MY_JUSTSTRING(IIX,IIY,NNTT(1:IB),NTANG,NTSIZ,3)
C
          RETURN
      END


C SUB PLLIB.FOR
      SUBROUTINE PLLIB
C
          IMPLICIT NONE
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
C
C                PLOT LIBRARY COMMANDS
          IF(SQ.EQ.1.AND.
     1    WQ.NE.'P'.AND.WQ.NE.'PUT'.AND.WQ.NE.'DEL'.AND.WQ.NE.'GET'
     1    )THEN
              OUTLYNE=
     1        'INVALID QUALIFIER WORD USED WITH THE "PLIB" COMMAND'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              RETURN
          END IF
          IF(SQ.EQ.0) THEN
              OUTLYNE=
     1        '"PLIB" REQUIRES EXPLICIT QUALIFIER WORD INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              RETURN
          END IF
          IF(WQ.EQ.'GET') THEN
              IF(SST.EQ.1.OR.S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
                  OUTLYNE=
     1            '"PLIB GET" TAKES NO STRING OR NUMERIC WORD #2,#3,#4 OR #5 INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  RETURN
              END IF
          END IF
          IF(WQ.EQ.'PUT') THEN
              IF(SST.EQ.1.OR.S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
                  OUTLYNE=
     1            '"PLIB PUT" TAKES NO STRING OR NUMERIC WORD #2,#3,#4 OR #5 INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  RETURN
              END IF
          END IF
          IF(WQ.EQ.'DEL') THEN
              IF(SST.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
                  OUTLYNE=
     1            '"PLIB DEL" TAKES NO STRING OR NUMERIC WORD #3,#4 OR #5 INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  RETURN
              END IF
          END IF
          IF(WQ.EQ.'P') THEN
              IF(SST.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
                  OUTLYNE=
     1            '"PLIB P" TAKES NO STRING OR NUMERIC WORD #3,#4 OR #5 INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  RETURN
              END IF
          END IF
          IF(WQ.EQ.'P'.OR.
     1    WQ.EQ.'PUT'.OR.
     2    WQ.EQ.'DEL'.OR.
     3    WQ.EQ.'GET') THEN
              CALL PLIBRY
              RETURN
          ELSE
          END IF
          RETURN
      END


C SUB PLFANS.FOR
      SUBROUTINE PLFANS
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE PLFANS.FOR. THIS SUBROUTINE IMPLEMENTS
C       THE PLOT COMMANDS PLTYFAN, PLTXFAN, PLTNFAN, PLTPFAN
C       AND PLOTFANS
C       IT IS CALLED BY PPLOTT.FOR.
C
          CHARACTER UNI*11,A1VAL1*5,A2VAL2*3,AWV1*31,AWV2*31
     1    ,AWV3*31,AWV4*31,AWV5*31,AXF1*23,AXF2*23,AXF3*23,AYF1*23,
     2    AYF2*23,AYF3*23,AWV6*31,AWV7*31,AWV8*31,AWV9*31,AWV10*32
C
          INTEGER IFA,I

          REAL*8 LPWP1,LPWP2,LCW,LSWP1,LSWP2
C
          LOGICAL FOBB0
     1    ,FOBB0X,FOBB0Y,ABORT,FANEXT
C
          COMMON/FANEXI/FANEXT
C
          COMMON/FFOOBB/FOBB0,FOBB0X,FOBB0Y
C
          INTEGER JK_WAV(1:10)
C
          COMMON/WAVER/JK_WAV
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
          A1VAL1='     '
          A2VAL2='   '
          FFAANN(1:3,1:21,1:6,1:10)=0.0D0
C
C       CHECK FOR STRING INPUT
          IF(SST.EQ.1) THEN
              OUTLYNE='"'//WC(1:8)//'" TAKES NO STRING INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          ELSE
C       PROCEED
          END IF
C     FOR WC=PLOTFANS
          IF(WC.EQ.'PLOTFANS') THEN
C       CHECK FOR VALID QUALIFIER INPUT
              IF(SQ.EQ.1) THEN
                  IF(WQ.NE.'SSI'.AND.WQ.NE.'YFOB'.AND.WQ.NE.'XFOB'.AND.
     1            WQ.NE.'OFFSET'.AND.WQ.NE.'WV'.AND.WQ.NE.'GO'.AND.
     1            WQ.NE.'RESET'.AND.WQ.NE.'REFWV'.AND.WQ.NE.'NEWOBJ'.AND.
     1            WQ.NE.'NEWREF'.AND.WQ.NE.'NEWIMG'.AND.WQ.NE.'WV2') THEN
                      OUTLYNE=
     1                  '"'//WC(1:8)//'" ENTERED WITH INVALID QUALIFIER INPUT'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  ELSE
C       QUALIFIERS ARE VALID, PROCEED
                  END IF
              ELSE
                  OUTLYNE=
     1              '"'//WC(1:8)//'" REQUIRES EXPLICIT QUALIFIER INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
C       NO QUALIFIERS
              END IF
          ELSE
C     WC NOT PLOTFAN
          END IF
C
C     FOR WC= PLTXFAN,PLTYFAN,PLTPFAN AND PLTNFAN
          IF(WC.EQ.'PLTXFAN'.OR.WC.EQ.'PLTYFAN'.OR.WC.EQ.'PLTPFAN'
     1    .OR.WC.EQ.'PLTNFAN') THEN
C       CHECK FOR VALID QUALIFIER INPUT
              IF(SQ.EQ.1) THEN
                  IF(WQ.NE.'OPD'.AND.WQ.NE.'CD'.AND.WQ.NE.'LA') THEN
                      OUTLYNE=
     1                  '"'//WC(1:8)//'" ENTERED WITH INVALID QUALIFIER INPUT'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  ELSE
C       QUALIFIERS ARE VALID, PROCEED
                  END IF
              ELSE
C       NO QUALIFIERS, WHICH IN THIS CASE IS OK
              END IF
          ELSE
C     NOT PLTYFAN,PLTXFAN,PLTPFAN OR PLTNFAN
          END IF
C
          IF(STI.EQ.0) THEN
C
C     CHECK FOR VALID NUMERIC INPUT NEXT
              IF(WC.EQ.'PLOTFANS'.AND.WQ.EQ.'SSI') THEN
                  IF(DF1.EQ.1) THEN
                      OUTLYNE=
     1                  '"PLOTFANS SSI" REQUIRES EXPLICIT NUMERIC WORD #1 INPUT'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  ELSE
                  END IF
                  IF(S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
                      OUTLYNE=
     1                  '"PLOTFANS SSI" ONLY TAKES NUMERIC WORD #1 INPUT'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  ELSE
                  END IF
              ELSE
C     NOT PLOTFANS SSI
              END IF
C
              IF(WC.EQ.'PLOTFANS'.AND.WQ.EQ.'OFFSET') THEN
                  IF(DF1.EQ.1) THEN
                      OUTLYNE=
     1                  '"PLOTFANS OFFSET" REQUIRES EXPLICIT NUMERIC WORD #1 INPUT'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  ELSE
                  END IF
                  IF(S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
                      OUTLYNE=
     1                '"PLOTFANS OFFSET" ONLY TAKES NUMERIC WORD #1 INPUT'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  ELSE
                  END IF
              ELSE
C     NOT PLOTFANS OFFSET
              END IF
C
              IF(WC.EQ.'PLOTFANS'.AND.WQ.EQ.'GO') THEN
                  IF(SN.EQ.1) THEN
                      OUTLYNE=
     1                  '"PLOTFANS GO" TAKES NO EXPLICIT NUMERIC INPUT'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  ELSE
                  END IF
              ELSE
C     NOT PLOTFANS GO
              END IF

              IF(WC.EQ.'PLOTFANS'.AND.WQ.EQ.'RESET') THEN
                  IF(SN.EQ.1) THEN
                      OUTLYNE=
     1                '"PLOTFANS RESET" TAKES NO EXPLICIT NUMERIC INPUT'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  ELSE
                  END IF
              ELSE
C     NOT PLOTFANS RESET
              END IF
C
              IF(WC.EQ.'PLOTFANS'.AND.WQ.EQ.'REFWV') THEN
                  IF(DF1.EQ.1.AND.DF2.EQ.1.AND.DF3.EQ.1.AND.DF4.EQ.1.AND.
     1            DF5.EQ.1) THEN
                      OUTLYNE=
     1                '"PLOTFANS REFWV" REQUIRES EXPLICIT NUMERIC WORD #1 INPUT'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  ELSE
                  END IF
                  IF(DF4.EQ.0.OR.DF5.EQ.0.OR.DF3.EQ.0.OR.DF2.EQ.0) THEN
                      OUTLYNE=
     1                '"PLOTFANS REFWV" TAKES NO NUMERIC WORD #2 THROUGH #5 INPUT'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  ELSE
                  END IF
              ELSE
C     NOT PLOTFANS REFWV
              END IF
              IF(WC.EQ.'PLOTFANS'.AND.WQ.EQ.'NEWOBJ') THEN
                  IF(DF1.EQ.1.AND.DF2.EQ.1.AND.DF3.EQ.1.AND.DF4.EQ.1.AND.
     1            DF5.EQ.1) THEN
                      OUTLYNE=
     1                '"PLOTFANS NEWOBJ" REQUIRES EXPLICIT NUMERIC WORD #1 INPUT'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  ELSE
                  END IF
                  IF(DF4.EQ.0.OR.DF5.EQ.0.OR.DF3.EQ.0.OR.DF2.EQ.0) THEN
                      OUTLYNE=
     1                '"PLOTFANS NEWOBJ" TAKES NO NUMERIC WORD #2 THROUGH #5 INPUT'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  ELSE
                  END IF
              ELSE
C     NOT PLOTFANS NEWOBJ
              END IF
              IF(WC.EQ.'PLOTFANS'.AND.WQ.EQ.'NEWREF') THEN
                  IF(DF1.EQ.1.AND.DF2.EQ.1.AND.DF3.EQ.1.AND.DF4.EQ.1.AND.
     1            DF5.EQ.1) THEN
                      OUTLYNE=
     1                '"PLOTFANS NEWREF" REQUIRES EXPLICIT NUMERIC WORD #1 INPUT'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  ELSE
                  END IF
                  IF(DF4.EQ.0.OR.DF5.EQ.0.OR.DF3.EQ.0.OR.DF2.EQ.0) THEN
                      OUTLYNE=
     1                '"PLOTFANS NEWREF" TAKES NO NUMERIC WORD #2 THROUGH #5 INPUT'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  ELSE
                  END IF
              ELSE
C     NOT PLOTFANS NEWREF
              END IF
              IF(WC.EQ.'PLOTFANS'.AND.WQ.EQ.'NEWIMG') THEN
                  IF(DF1.EQ.1.AND.DF2.EQ.1.AND.DF3.EQ.1.AND.DF4.EQ.1.AND.
     1            DF5.EQ.1) THEN
                      OUTLYNE=
     1                '"PLOTFANS NEWIMG" REQUIRES EXPLICIT NUMERIC WORD #1 INPUT'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  ELSE
                  END IF
                  IF(DF4.EQ.0.OR.DF5.EQ.0.OR.DF3.EQ.0.OR.DF2.EQ.0) THEN
                      OUTLYNE=
     1                '"PLOTFANS NEWIMG" TAKES NO NUMERIC WORD #2 THROUGH #5 INPUT'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  ELSE
                  END IF
              ELSE
C     NOT PLOTFANS NEWIMG
              END IF
C
              IF(WC.EQ.'PLOTFANS'.AND.WQ.EQ.'YFOB') THEN
                  IF(DF1.EQ.1.AND.DF2.EQ.1.AND.DF3.EQ.1.AND.DF4.EQ.1.AND.
     1            DF5.EQ.1) THEN
                      OUTLYNE=
     1                '"PLOTFANS YFOB" REQUIRES SOME EXPLICIT NUMERIC INPUT'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  ELSE
                  END IF
                  IF(DF4.EQ.0.OR.DF5.EQ.0) THEN
                      OUTLYNE=
     1                '"PLOTFANS YFOB" TAKES NO NUMERIC WORD #4 OR #5 INPUT'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  ELSE
                  END IF
              ELSE
C     NOT PLOTFANS YFOB
              END IF
C
              IF(WC.EQ.'PLOTFANS'.AND.WQ.EQ.'XFOB') THEN
                  IF(DF1.EQ.1.AND.DF2.EQ.1.AND.DF3.EQ.1.AND.DF4.EQ.1.AND.
     1            DF5.EQ.1) THEN
                      OUTLYNE=
     1                '"PLOTFANS XFOB" REQUIRES SOME EXPLICIT NUMERIC INPUT'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  ELSE
                  END IF
                  IF(DF4.EQ.0.OR.DF5.EQ.0) THEN
                      OUTLYNE=
     1                '"PLOTFANS XFOB" TAKES NO NUMERIC WORD #4 OR #5 INPUT'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  ELSE
                  END IF
              ELSE
C     NOT PLOTFANS XFOB
              END IF
C
              IF(WC.EQ.'PLOTFANS'.AND.WQ.EQ.'WV') THEN
                  IF(DF1.EQ.1.AND.DF2.EQ.1.AND.DF3.EQ.1.AND.DF4.EQ.1.AND.
     1            DF5.EQ.1) THEN
                      OUTLYNE=
     1                '"PLOTFANS WV" REQUIRES SOME EXPLICIT NUMERIC INPUT'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  ELSE
                  END IF
              ELSE
C     NOT PLOTFANS WV
              END IF
              IF(WC.EQ.'PLOTFANS'.AND.WQ.EQ.'WV2') THEN
                  IF(DF1.EQ.1.AND.DF2.EQ.1.AND.DF3.EQ.1.AND.DF4.EQ.1.AND.
     1            DF5.EQ.1) THEN
                      OUTLYNE=
     1                '"PLOTFANS WV2" REQUIRES SOME EXPLICIT NUMERIC INPUT'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  ELSE
                  END IF
              ELSE
C     NOT PLOTFANS WV2
              END IF
C
              IF(WC.EQ.'PLTXFAN') THEN
                  IF(SN.EQ.1) THEN
                      OUTLYNE=
     1                '"PLTXFAN" TAKES NO EXPLICIT NUMERIC INPUT'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  ELSE
                  END IF
              ELSE
C     NOT PLTXFAN
              END IF
C
C
              IF(WC.EQ.'PLTYFAN') THEN
                  IF(SN.EQ.1) THEN
                      OUTLYNE=
     1                '"PLTYFAN" TAKES NO EXPLICIT NUMERIC INPUT'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  ELSE
                  END IF
              ELSE
C     NOT PLTYFAN
              END IF
C
C
              IF(WC.EQ.'PLTNFAN') THEN
                  IF(SN.EQ.1) THEN
                      OUTLYNE=
     1                '"PLTNFAN" TAKES NO EXPLICIT NUMERIC INPUT'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  ELSE
                  END IF
              ELSE
C     NOT PLTNFAN
              END IF
C
C
              IF(WC.EQ.'PLTPFAN') THEN
                  IF(SN.EQ.1) THEN
                      OUTLYNE=
     1                '"PLTPFAN" TAKES NO EXPLICIT NUMERIC INPUT'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  ELSE
                  END IF
              ELSE
C     NOT PLTPFAN
              END IF
C
C     MOST SYNTAX CHECKING IS DONE, NOW DO INDIVIDUAL COMMANDS
C
C     NOW PLOTFANS RESET
              IF(WC.EQ.'PLOTFANS'.AND.WQ.EQ.'RESET') THEN
C
C     RESTORE THE DEFAULT WAVELENGTH SETTINGS
                  FANWV1=.FALSE.
                  FANWV2=.FALSE.
                  FANWV3=.FALSE.
                  FANWV4=.FALSE.
                  FANWV5=.FALSE.
                  FANWV6=.FALSE.
                  FANWV7=.FALSE.
                  FANWV8=.FALSE.
                  FANWV9=.FALSE.
                  FANWV10=.FALSE.
                  I=0
                  IF(SYSTEM1(31).GT.0.0D0) THEN
                      FANWV1=.TRUE.
                      I=I+1
                      IF(I.GT.10) GO TO 900
                      JK_WAV(I)=1
                  END IF
                  IF(SYSTEM1(32).GT.0.0D0) THEN
                      FANWV2=.TRUE.
                      I=I+1
                      IF(I.GT.10) GO TO 900
                      JK_WAV(I)=2
                  END IF
                  IF(SYSTEM1(33).GT.0.0D0) THEN
                      FANWV3=.TRUE.
                      I=I+1
                      IF(I.GT.10) GO TO 900
                      JK_WAV(I)=3
                  END IF
                  IF(SYSTEM1(34).GT.0.0D0) THEN
                      FANWV4=.TRUE.
                      I=I+1
                      IF(I.GT.10) GO TO 900
                      JK_WAV(I)=4
                  END IF
                  IF(SYSTEM1(35).GT.0.0D0) THEN
                      FANWV5=.TRUE.
                      I=I+1
                      IF(I.GT.10) GO TO 900
                      JK_WAV(I)=5
                  END IF
                  IF(SYSTEM1(76).GT.0.0D0) THEN
                      FANWV6=.TRUE.
                      I=I+1
                      IF(I.GT.10) GO TO 900
                      JK_WAV(I)=6
                  END IF
                  IF(SYSTEM1(77).GT.0.0D0) THEN
                      FANWV7=.TRUE.
                      I=I+1
                      IF(I.GT.10) GO TO 900
                      JK_WAV(I)=7
                  END IF
                  IF(SYSTEM1(78).GT.0.0D0) THEN
                      FANWV8=.TRUE.
                      I=I+1
                      IF(I.GT.10) GO TO 900
                      JK_WAV(I)=8
                  END IF
                  IF(SYSTEM1(79).GT.0.0D0) THEN
                      FANWV9=.TRUE.
                      I=I+1
                      IF(I.GT.10) GO TO 900
                      JK_WAV(I)=9
                  END IF
                  IF(SYSTEM1(80).GT.0.0D0) THEN
                      FANWV10=.TRUE.
                      I=I+1
                      IF(I.GT.10) GO TO 900
                      JK_WAV(I)=10
                  END IF
 900              CONTINUE
C
C     RESTORE THE OFFSET  (FRACTIONAL APERTURE HT., UNITLESS)
                  FANOFF=0.0D0
C
C     RESTORE THE DEFAULT FAN TYPE
                  FANTYP=5
C     YFAN FANTYP=1
C     XFAN FANTYP=2
C     NFAN FANTYP=3
C     PFAN FANTYP=4
C     XYFAN FANTYP=5
C     YXFAN FANTYP=6
C
C     RESTORE FAN QUALIFIER TYPE
                  QALTYP=0
C     (NONE) QALTYP=0
C     OPD    QALTYP=1
C     CD     QALTYP=2
C     LA     QALTYP=3
C
C     RESTORE FAN REFWV VALUES
                  REFWV=INT(SYSTEM1(11))
C
C     RESTORE IMG,REF AND OBJ SURF NUMBERS
                  FANNOB=0
                  FANNRF=INT(SYSTEM1(25))
                  FANNIM=INT(SYSTEM1(20))
C
C     RESTORE FAN YFOB VALUES
                  YFOB1=0.0
                  YFOB2=0.0
                  YFOB3=0.0
C
C     RESTORE FAN XFOB VALUES
                  XFOB1=0.0
                  XFOB2=0.0
                  XFOB3=0.0
C
C     RESTORE SSI AUTO FLAG AND THE SSI DEFAULT VALUE
                  SSIFLG=.TRUE.
                  SSI=0.0D0
C
C     RESTORE MAXFAN TO THE DEFAULT VALUE
                  MAXFAN=20
                  STEPJP=125.0D0
C
C     RESTORE THE COUNT FOR THE NUMBER OF FANS ON A PAGE
                  FANNUM=1
                  FANNM1=3
                  FANNM2=3
C
C     (THIS IS INTERNALLY SET BY THE PROGRAM, IT CAN'T BE
C     SET EXPLICITLY BY THE USER)
C
C     ALL DEFAULT VALUES HAVE BEEN RESET
                  OUTLYNE=
     1              'DEFAULT FAN PLOTTING VALUES HAVE BEEN RESTORED'
                  CALL SHOWIT(1)
C
                  RETURN
              ELSE
              END IF
C
C     PLOTFANS WV
              IF(WC.EQ.'PLOTFANS'.AND.WQ.EQ.'WV') THEN
                  IF(DF1.EQ.0) THEN
                      IF(DABS(W1).LT.1.0D0.OR.DABS(W1).GT.10.0D0) THEN
C     VALUE OUT OF RANGE
                          OUTLYNE=
     1                    'WAVELENGTH NUMBER INPUT VALUES MUST BE:'
                          CALL SHOWIT(1)
                          OUTLYNE=
     1                    '+/-1, +/-2, +/-3, +/-4, +/-5'
                          CALL SHOWIT(1)
                          OUTLYNE=
     1                    '+/-6, +/-7, +/-8, +/-9 OR +/-10'
                          CALL SHOWIT(1)
                          OUTLYNE='RE-ENTER COMMAND'
                          CALL SHOWIT(1)
                          CALL MACFAL
                          RETURN
                      ELSE
                      END IF
                  ELSE
                  END IF
                  IF(DF2.EQ.0) THEN
                      IF(DABS(W2).LT.1.0D0.OR.DABS(W2).GT.10.0D0) THEN
C     VALUE OUT OF RANGE
                          OUTLYNE=
     1                      'WAVELENGTH NUMBER INPUT VALUES MUST BE:'
                          CALL SHOWIT(1)
                          OUTLYNE=
     1                      '+/-1, +/-2, +/-3, +/-4, +/-5'
                          CALL SHOWIT(1)
                          OUTLYNE=
     1                      '+/-6, +/-7, +/-8, +/-9 OR +/-10'
                          CALL SHOWIT(1)
                          OUTLYNE='RE-ENTER COMMAND'
                          CALL SHOWIT(1)
                          CALL MACFAL
                          RETURN
                      ELSE
                      END IF
                  ELSE
                  END IF
                  IF(DF3.EQ.0) THEN
                      IF(DABS(W3).LT.1.0D0.OR.DABS(W3).GT.10.0D0) THEN
C     VALUE OUT OF RANGE
                          OUTLYNE=
     1                      'WAVELENGTH NUMBER INPUT VALUES MUST BE:'
                          CALL SHOWIT(1)
                          OUTLYNE=
     1                      '+/-1, +/-2, +/-3, +/-4, +/-5'
                          CALL SHOWIT(1)
                          OUTLYNE=
     1                      '+/-6, +/-7, +/-8, +/-9 OR +/-10'
                          CALL SHOWIT(1)
                          OUTLYNE='RE-ENTER COMMAND'
                          CALL SHOWIT(1)
                          CALL MACFAL
                          RETURN
                      ELSE
                      END IF
                  ELSE
                  END IF
                  IF(DF4.EQ.0) THEN
                      IF(DABS(W4).LT.1.0D0.OR.DABS(W4).GT.10.0D0) THEN
C     VALUE OUT OF RANGE
                          OUTLYNE=
     1                      'WAVELENGTH NUMBER INPUT VALUES MUST BE:'
                          CALL SHOWIT(1)
                          OUTLYNE=
     1                      '+/-1, +/-2, +/-3, +/-4, +/-5'
                          CALL SHOWIT(1)
                          OUTLYNE=
     1                      '+/-6, +/-7, +/-8, +/-9 OR +/-10'
                          CALL SHOWIT(1)
                          OUTLYNE='RE-ENTER COMMAND'
                          CALL SHOWIT(1)
                          CALL MACFAL
                          RETURN
                      ELSE
                      END IF
                  ELSE
                  END IF
                  IF(DF5.EQ.0) THEN
                      IF(DABS(W5).LT.1.0D0.OR.DABS(W5).GT.10.0D0) THEN
C     VALUE OUT OF RANGE
                          OUTLYNE=
     1                      'WAVELENGTH NUMBER INPUT VALUES MUST BE:'
                          CALL SHOWIT(1)
                          OUTLYNE=
     1                      '+/-1, +/-2, +/-3, +/-4, +/-5'
                          CALL SHOWIT(1)
                          OUTLYNE=
     1                      '+/-6, +/-7, +/-8, +/-9 OR +/-10'
                          CALL SHOWIT(1)
                          OUTLYNE='RE-ENTER COMMAND'
                          CALL SHOWIT(1)
                          CALL MACFAL
                          RETURN
                      ELSE
                      END IF
                  ELSE
                  END IF
C
                  IF(DF1.EQ.0) THEN
                      IF(W1.EQ.1.0D0) FANWV1=.TRUE.
                      IF(W1.EQ.2.0D0) FANWV2=.TRUE.
                      IF(W1.EQ.3.0D0) FANWV3=.TRUE.
                      IF(W1.EQ.4.0D0) FANWV4=.TRUE.
                      IF(W1.EQ.5.0D0) FANWV5=.TRUE.
                      IF(W1.EQ.6.0D0) FANWV6=.TRUE.
                      IF(W1.EQ.7.0D0) FANWV7=.TRUE.
                      IF(W1.EQ.8.0D0) FANWV8=.TRUE.
                      IF(W1.EQ.9.0D0) FANWV9=.TRUE.
                      IF(W1.EQ.10.0D0) FANWV10=.TRUE.
                      IF(W1.EQ.-1.0D0) FANWV1=.FALSE.
                      IF(W1.EQ.-2.0D0) FANWV2=.FALSE.
                      IF(W1.EQ.-3.0D0) FANWV3=.FALSE.
                      IF(W1.EQ.-4.0D0) FANWV4=.FALSE.
                      IF(W1.EQ.-5.0D0) FANWV5=.FALSE.
                      IF(W1.EQ.-6.0D0) FANWV6=.FALSE.
                      IF(W1.EQ.-7.0D0) FANWV7=.FALSE.
                      IF(W1.EQ.-8.0D0) FANWV8=.FALSE.
                      IF(W1.EQ.-9.0D0) FANWV9=.FALSE.
                      IF(W1.EQ.-10.0D0) FANWV10=.FALSE.
                      IF(W1.EQ.1.0D0) JK_WAV(1)=1
                      IF(W1.EQ.2.0D0) JK_WAV(1)=2
                      IF(W1.EQ.3.0D0) JK_WAV(1)=3
                      IF(W1.EQ.4.0D0) JK_WAV(1)=4
                      IF(W1.EQ.5.0D0) JK_WAV(1)=5
                      IF(W1.EQ.6.0D0) JK_WAV(1)=6
                      IF(W1.EQ.7.0D0) JK_WAV(1)=7
                      IF(W1.EQ.8.0D0) JK_WAV(1)=8
                      IF(W1.EQ.9.0D0) JK_WAV(1)=9
                      IF(W1.EQ.10.0D0) JK_WAV(1)=10
                      IF(W1.EQ.-1.0D0) JK_WAV(1)=0
                      IF(W1.EQ.-2.0D0) JK_WAV(1)=0
                      IF(W1.EQ.-3.0D0) JK_WAV(1)=0
                      IF(W1.EQ.-4.0D0) JK_WAV(1)=0
                      IF(W1.EQ.-5.0D0) JK_WAV(1)=0
                      IF(W1.EQ.-6.0D0) JK_WAV(1)=0
                      IF(W1.EQ.-7.0D0) JK_WAV(1)=0
                      IF(W1.EQ.-8.0D0) JK_WAV(1)=0
                      IF(W1.EQ.-9.0D0) JK_WAV(1)=0
                      IF(W1.EQ.-10.0D0) JK_WAV(1)=0
                  ELSE
C     DON'T DO ANYTHING
                  END IF
                  IF(DF2.EQ.0) THEN
                      IF(W2.EQ.1.0D0) FANWV1=.TRUE.
                      IF(W2.EQ.2.0D0) FANWV2=.TRUE.
                      IF(W2.EQ.3.0D0) FANWV3=.TRUE.
                      IF(W2.EQ.4.0D0) FANWV4=.TRUE.
                      IF(W2.EQ.5.0D0) FANWV5=.TRUE.
                      IF(W2.EQ.6.0D0) FANWV6=.TRUE.
                      IF(W2.EQ.7.0D0) FANWV7=.TRUE.
                      IF(W2.EQ.8.0D0) FANWV8=.TRUE.
                      IF(W2.EQ.9.0D0) FANWV9=.TRUE.
                      IF(W2.EQ.10.0D0) FANWV10=.TRUE.
                      IF(W2.EQ.-1.0D0) FANWV1=.FALSE.
                      IF(W2.EQ.-2.0D0) FANWV2=.FALSE.
                      IF(W2.EQ.-3.0D0) FANWV3=.FALSE.
                      IF(W2.EQ.-4.0D0) FANWV4=.FALSE.
                      IF(W2.EQ.-5.0D0) FANWV5=.FALSE.
                      IF(W2.EQ.-6.0D0) FANWV6=.FALSE.
                      IF(W2.EQ.-7.0D0) FANWV7=.FALSE.
                      IF(W2.EQ.-8.0D0) FANWV8=.FALSE.
                      IF(W2.EQ.-9.0D0) FANWV9=.FALSE.
                      IF(W2.EQ.-10.0D0) FANWV10=.FALSE.
                      IF(W2.EQ.1.0D0) JK_WAV(2)=1
                      IF(W2.EQ.2.0D0) JK_WAV(2)=2
                      IF(W2.EQ.3.0D0) JK_WAV(2)=3
                      IF(W2.EQ.4.0D0) JK_WAV(2)=4
                      IF(W2.EQ.5.0D0) JK_WAV(2)=5
                      IF(W2.EQ.6.0D0) JK_WAV(2)=6
                      IF(W2.EQ.7.0D0) JK_WAV(2)=7
                      IF(W2.EQ.8.0D0) JK_WAV(2)=8
                      IF(W2.EQ.9.0D0) JK_WAV(2)=9
                      IF(W2.EQ.10.0D0) JK_WAV(2)=10
                      IF(W2.EQ.-1.0D0) JK_WAV(2)=0
                      IF(W2.EQ.-2.0D0) JK_WAV(2)=0
                      IF(W2.EQ.-3.0D0) JK_WAV(2)=0
                      IF(W2.EQ.-4.0D0) JK_WAV(2)=0
                      IF(W2.EQ.-5.0D0) JK_WAV(2)=0
                      IF(W2.EQ.-6.0D0) JK_WAV(2)=0
                      IF(W2.EQ.-7.0D0) JK_WAV(2)=0
                      IF(W2.EQ.-8.0D0) JK_WAV(2)=0
                      IF(W2.EQ.-9.0D0) JK_WAV(2)=0
                      IF(W2.EQ.-10.0D0) JK_WAV(2)=0
                  ELSE
C     DON'T DO ANYTHING
                  END IF
                  IF(DF3.EQ.0) THEN
                      IF(W3.EQ.1.0D0) FANWV1=.TRUE.
                      IF(W3.EQ.2.0D0) FANWV2=.TRUE.
                      IF(W3.EQ.3.0D0) FANWV3=.TRUE.
                      IF(W3.EQ.4.0D0) FANWV4=.TRUE.
                      IF(W3.EQ.5.0D0) FANWV5=.TRUE.
                      IF(W3.EQ.6.0D0) FANWV6=.TRUE.
                      IF(W3.EQ.7.0D0) FANWV7=.TRUE.
                      IF(W3.EQ.8.0D0) FANWV8=.TRUE.
                      IF(W3.EQ.9.0D0) FANWV9=.TRUE.
                      IF(W3.EQ.10.0D0) FANWV10=.TRUE.
                      IF(W3.EQ.-1.0D0) FANWV1=.FALSE.
                      IF(W3.EQ.-2.0D0) FANWV2=.FALSE.
                      IF(W3.EQ.-3.0D0) FANWV3=.FALSE.
                      IF(W3.EQ.-4.0D0) FANWV4=.FALSE.
                      IF(W3.EQ.-5.0D0) FANWV5=.FALSE.
                      IF(W3.EQ.-6.0D0) FANWV6=.FALSE.
                      IF(W3.EQ.-7.0D0) FANWV7=.FALSE.
                      IF(W3.EQ.-8.0D0) FANWV8=.FALSE.
                      IF(W3.EQ.-9.0D0) FANWV9=.FALSE.
                      IF(W3.EQ.-10.0D0) FANWV10=.FALSE.
                      IF(W3.EQ.1.0D0) JK_WAV(3)=1
                      IF(W3.EQ.2.0D0) JK_WAV(3)=2
                      IF(W3.EQ.3.0D0) JK_WAV(3)=3
                      IF(W3.EQ.4.0D0) JK_WAV(3)=4
                      IF(W3.EQ.5.0D0) JK_WAV(3)=5
                      IF(W3.EQ.6.0D0) JK_WAV(3)=6
                      IF(W3.EQ.7.0D0) JK_WAV(3)=7
                      IF(W3.EQ.8.0D0) JK_WAV(3)=8
                      IF(W3.EQ.9.0D0) JK_WAV(3)=9
                      IF(W3.EQ.10.0D0) JK_WAV(3)=10
                      IF(W3.EQ.-1.0D0) JK_WAV(3)=0
                      IF(W3.EQ.-2.0D0) JK_WAV(3)=0
                      IF(W3.EQ.-3.0D0) JK_WAV(3)=0
                      IF(W3.EQ.-4.0D0) JK_WAV(3)=0
                      IF(W3.EQ.-5.0D0) JK_WAV(3)=0
                      IF(W3.EQ.-6.0D0) JK_WAV(3)=0
                      IF(W3.EQ.-7.0D0) JK_WAV(3)=0
                      IF(W3.EQ.-8.0D0) JK_WAV(3)=0
                      IF(W3.EQ.-9.0D0) JK_WAV(3)=0
                      IF(W3.EQ.-10.0D0) JK_WAV(3)=0
                  ELSE
C     DON'T DO ANYTHING
                  END IF
                  IF(DF4.EQ.0) THEN
                      IF(W4.EQ.1.0D0) FANWV1=.TRUE.
                      IF(W4.EQ.2.0D0) FANWV2=.TRUE.
                      IF(W4.EQ.3.0D0) FANWV3=.TRUE.
                      IF(W4.EQ.4.0D0) FANWV4=.TRUE.
                      IF(W4.EQ.5.0D0) FANWV5=.TRUE.
                      IF(W4.EQ.6.0D0) FANWV6=.TRUE.
                      IF(W4.EQ.7.0D0) FANWV7=.TRUE.
                      IF(W4.EQ.8.0D0) FANWV8=.TRUE.
                      IF(W4.EQ.9.0D0) FANWV9=.TRUE.
                      IF(W4.EQ.10.0D0) FANWV10=.TRUE.
                      IF(W4.EQ.-1.0D0) FANWV1=.FALSE.
                      IF(W4.EQ.-2.0D0) FANWV2=.FALSE.
                      IF(W4.EQ.-3.0D0) FANWV3=.FALSE.
                      IF(W4.EQ.-4.0D0) FANWV4=.FALSE.
                      IF(W4.EQ.-5.0D0) FANWV5=.FALSE.
                      IF(W4.EQ.-6.0D0) FANWV6=.FALSE.
                      IF(W4.EQ.-7.0D0) FANWV7=.FALSE.
                      IF(W4.EQ.-8.0D0) FANWV8=.FALSE.
                      IF(W4.EQ.-9.0D0) FANWV9=.FALSE.
                      IF(W4.EQ.-10.0D0) FANWV10=.FALSE.
                      IF(W4.EQ.1.0D0) JK_WAV(4)=1
                      IF(W4.EQ.2.0D0) JK_WAV(4)=2
                      IF(W4.EQ.3.0D0) JK_WAV(4)=3
                      IF(W4.EQ.4.0D0) JK_WAV(4)=4
                      IF(W4.EQ.5.0D0) JK_WAV(4)=5
                      IF(W4.EQ.6.0D0) JK_WAV(4)=6
                      IF(W4.EQ.7.0D0) JK_WAV(4)=7
                      IF(W4.EQ.8.0D0) JK_WAV(4)=8
                      IF(W4.EQ.9.0D0) JK_WAV(4)=9
                      IF(W4.EQ.10.0D0) JK_WAV(4)=10
                      IF(W4.EQ.-1.0D0) JK_WAV(4)=0
                      IF(W4.EQ.-2.0D0) JK_WAV(4)=0
                      IF(W4.EQ.-3.0D0) JK_WAV(4)=0
                      IF(W4.EQ.-4.0D0) JK_WAV(4)=0
                      IF(W4.EQ.-5.0D0) JK_WAV(4)=0
                      IF(W4.EQ.-6.0D0) JK_WAV(4)=0
                      IF(W4.EQ.-7.0D0) JK_WAV(4)=0
                      IF(W4.EQ.-8.0D0) JK_WAV(4)=0
                      IF(W4.EQ.-9.0D0) JK_WAV(4)=0
                      IF(W4.EQ.-10.0D0) JK_WAV(4)=0
                  ELSE
C     DON'T DO ANYTHING
                  END IF
                  IF(DF5.EQ.0) THEN
                      IF(W5.EQ.1.0D0) FANWV1=.TRUE.
                      IF(W5.EQ.2.0D0) FANWV2=.TRUE.
                      IF(W5.EQ.3.0D0) FANWV3=.TRUE.
                      IF(W5.EQ.4.0D0) FANWV4=.TRUE.
                      IF(W5.EQ.5.0D0) FANWV5=.TRUE.
                      IF(W5.EQ.6.0D0) FANWV6=.TRUE.
                      IF(W5.EQ.7.0D0) FANWV7=.TRUE.
                      IF(W5.EQ.8.0D0) FANWV8=.TRUE.
                      IF(W5.EQ.9.0D0) FANWV9=.TRUE.
                      IF(W5.EQ.10.0D0) FANWV10=.TRUE.
                      IF(W5.EQ.-1.0D0) FANWV1=.FALSE.
                      IF(W5.EQ.-2.0D0) FANWV2=.FALSE.
                      IF(W5.EQ.-3.0D0) FANWV3=.FALSE.
                      IF(W5.EQ.-4.0D0) FANWV4=.FALSE.
                      IF(W5.EQ.-5.0D0) FANWV5=.FALSE.
                      IF(W5.EQ.-6.0D0) FANWV6=.FALSE.
                      IF(W5.EQ.-7.0D0) FANWV7=.FALSE.
                      IF(W5.EQ.-8.0D0) FANWV8=.FALSE.
                      IF(W5.EQ.-9.0D0) FANWV9=.FALSE.
                      IF(W5.EQ.-10.0D0) FANWV10=.FALSE.
                      IF(W5.EQ.1.0D0) JK_WAV(5)=1
                      IF(W5.EQ.2.0D0) JK_WAV(5)=2
                      IF(W5.EQ.3.0D0) JK_WAV(5)=3
                      IF(W5.EQ.4.0D0) JK_WAV(5)=4
                      IF(W5.EQ.5.0D0) JK_WAV(5)=5
                      IF(W5.EQ.6.0D0) JK_WAV(5)=6
                      IF(W5.EQ.7.0D0) JK_WAV(5)=7
                      IF(W5.EQ.8.0D0) JK_WAV(5)=8
                      IF(W5.EQ.9.0D0) JK_WAV(5)=9
                      IF(W5.EQ.10.0D0) JK_WAV(5)=10
                      IF(W5.EQ.-1.0D0) JK_WAV(5)=0
                      IF(W5.EQ.-2.0D0) JK_WAV(5)=0
                      IF(W5.EQ.-3.0D0) JK_WAV(5)=0
                      IF(W5.EQ.-4.0D0) JK_WAV(5)=0
                      IF(W5.EQ.-5.0D0) JK_WAV(5)=0
                      IF(W5.EQ.-6.0D0) JK_WAV(5)=0
                      IF(W5.EQ.-7.0D0) JK_WAV(5)=0
                      IF(W5.EQ.-8.0D0) JK_WAV(5)=0
                      IF(W5.EQ.-9.0D0) JK_WAV(5)=0
                      IF(W5.EQ.-10.0D0) JK_WAV(5)=0
                  ELSE
C     DON'T DO ANYTHING
                  END IF
                  RETURN
              ELSE
              END IF
C     PLOTFANS WV2
              IF(WC.EQ.'PLOTFANS'.AND.WQ.EQ.'WV2') THEN
                  IF(DF1.EQ.0) THEN
                      IF(DABS(W1).LT.1.0D0.OR.DABS(W1).GT.10.0D0) THEN
C     VALUE OUT OF RANGE
                          OUTLYNE=
     1                    'WAVELENGTH NUMBER INPUT VALUES MUST BE:'
                          CALL SHOWIT(1)
                          OUTLYNE=
     1                      '+/-1, +/-2, +/-3, +/-4, +/-5'
                          CALL SHOWIT(1)
                          OUTLYNE=
     1                      '+/-6, +/-7, +/-8, +/-9 OR +/-10'
                          CALL SHOWIT(1)
                          OUTLYNE='RE-ENTER COMMAND'
                          CALL SHOWIT(1)
                          CALL MACFAL
                          RETURN
                      ELSE
                      END IF
                  ELSE
                  END IF
                  IF(DF2.EQ.0) THEN
                      IF(DABS(W2).LT.1.0D0.OR.DABS(W2).GT.10.0D0) THEN
C     VALUE OUT OF RANGE
                          OUTLYNE=
     1                      'WAVELENGTH NUMBER INPUT VALUES MUST BE:'
                          CALL SHOWIT(1)
                          OUTLYNE=
     1                      '+/-1, +/-2, +/-3, +/-4, +/-5'
                          CALL SHOWIT(1)
                          OUTLYNE=
     1                      '+/-6, +/-7, +/-8, +/-9 OR +/-10'
                          CALL SHOWIT(1)
                          OUTLYNE='RE-ENTER COMMAND'
                          CALL SHOWIT(1)
                          CALL MACFAL
                          RETURN
                      ELSE
                      END IF
                  ELSE
                  END IF
                  IF(DF3.EQ.0) THEN
                      IF(DABS(W3).LT.1.0D0.OR.DABS(W3).GT.10.0D0) THEN
C     VALUE OUT OF RANGE
                          OUTLYNE=
     1                      'WAVELENGTH NUMBER INPUT VALUES MUST BE:'
                          CALL SHOWIT(1)
                          OUTLYNE=
     1                      '+/-1, +/-2, +/-3, +/-4, +/-5'
                          CALL SHOWIT(1)
                          OUTLYNE=
     1                      '+/-6, +/-7, +/-8, +/-9 OR +/-10'
                          CALL SHOWIT(1)
                          OUTLYNE='RE-ENTER COMMAND'
                          CALL SHOWIT(1)
                          CALL MACFAL
                          RETURN
                      ELSE
                      END IF
                  ELSE
                  END IF
                  IF(DF4.EQ.0) THEN
                      IF(DABS(W4).LT.1.0D0.OR.DABS(W4).GT.10.0D0) THEN
C     VALUE OUT OF RANGE
                          OUTLYNE=
     1                      'WAVELENGTH NUMBER INPUT VALUES MUST BE:'
                          CALL SHOWIT(1)
                          OUTLYNE=
     1                      '+/-1, +/-2, +/-3, +/-4, +/-5'
                          CALL SHOWIT(1)
                          OUTLYNE=
     1                      '+/-6, +/-7, +/-8, +/-9 OR +/-10'
                          CALL SHOWIT(1)
                          OUTLYNE='RE-ENTER COMMAND'
                          CALL SHOWIT(1)
                          CALL MACFAL
                          RETURN
                      ELSE
                      END IF
                  ELSE
                  END IF
                  IF(DF5.EQ.0) THEN
                      IF(DABS(W5).LT.1.0D0.OR.DABS(W5).GT.10.0D0) THEN
C     VALUE OUT OF RANGE
                          OUTLYNE=
     1                      'WAVELENGTH NUMBER INPUT VALUES MUST BE:'
                          CALL SHOWIT(1)
                          OUTLYNE=
     1                      '+/-1, +/-2, +/-3, +/-4, +/-5'
                          CALL SHOWIT(1)
                          OUTLYNE=
     1                      '+/-6, +/-7, +/-8, +/-9 OR +/-10'
                          CALL SHOWIT(1)
                          OUTLYNE='RE-ENTER COMMAND'
                          CALL SHOWIT(1)
                          CALL MACFAL
                          RETURN
                      ELSE
                      END IF
                  ELSE
                  END IF
C
                  IF(DF1.EQ.0) THEN
                      IF(W1.EQ.1.0D0) FANWV1=.TRUE.
                      IF(W1.EQ.2.0D0) FANWV2=.TRUE.
                      IF(W1.EQ.3.0D0) FANWV3=.TRUE.
                      IF(W1.EQ.4.0D0) FANWV4=.TRUE.
                      IF(W1.EQ.5.0D0) FANWV5=.TRUE.
                      IF(W1.EQ.6.0D0) FANWV6=.TRUE.
                      IF(W1.EQ.7.0D0) FANWV7=.TRUE.
                      IF(W1.EQ.8.0D0) FANWV8=.TRUE.
                      IF(W1.EQ.9.0D0) FANWV9=.TRUE.
                      IF(W1.EQ.10.0D0) FANWV10=.TRUE.
                      IF(W1.EQ.-1.0D0) FANWV1=.FALSE.
                      IF(W1.EQ.-2.0D0) FANWV2=.FALSE.
                      IF(W1.EQ.-3.0D0) FANWV3=.FALSE.
                      IF(W1.EQ.-4.0D0) FANWV4=.FALSE.
                      IF(W1.EQ.-5.0D0) FANWV5=.FALSE.
                      IF(W1.EQ.-6.0D0) FANWV6=.FALSE.
                      IF(W1.EQ.-7.0D0) FANWV7=.FALSE.
                      IF(W1.EQ.-8.0D0) FANWV8=.FALSE.
                      IF(W1.EQ.-9.0D0) FANWV9=.FALSE.
                      IF(W1.EQ.-10.0D0) FANWV10=.FALSE.
                      IF(W1.EQ.1.0D0) JK_WAV(6)=1
                      IF(W1.EQ.2.0D0) JK_WAV(6)=2
                      IF(W1.EQ.3.0D0) JK_WAV(6)=3
                      IF(W1.EQ.4.0D0) JK_WAV(6)=4
                      IF(W1.EQ.5.0D0) JK_WAV(6)=5
                      IF(W1.EQ.6.0D0) JK_WAV(6)=6
                      IF(W1.EQ.7.0D0) JK_WAV(6)=7
                      IF(W1.EQ.8.0D0) JK_WAV(6)=8
                      IF(W1.EQ.9.0D0) JK_WAV(6)=9
                      IF(W1.EQ.10.0D0) JK_WAV(6)=10
                      IF(W1.EQ.-1.0D0) JK_WAV(6)=0
                      IF(W1.EQ.-2.0D0) JK_WAV(6)=0
                      IF(W1.EQ.-3.0D0) JK_WAV(6)=0
                      IF(W1.EQ.-4.0D0) JK_WAV(6)=0
                      IF(W1.EQ.-5.0D0) JK_WAV(6)=0
                      IF(W1.EQ.-6.0D0) JK_WAV(6)=0
                      IF(W1.EQ.-7.0D0) JK_WAV(6)=0
                      IF(W1.EQ.-8.0D0) JK_WAV(6)=0
                      IF(W1.EQ.-9.0D0) JK_WAV(6)=0
                      IF(W1.EQ.-10.0D0) JK_WAV(6)=0
                  ELSE
C     DON'T DO ANYTHING
                  END IF
                  IF(DF2.EQ.0) THEN
                      IF(W2.EQ.1.0D0) FANWV1=.TRUE.
                      IF(W2.EQ.2.0D0) FANWV2=.TRUE.
                      IF(W2.EQ.3.0D0) FANWV3=.TRUE.
                      IF(W2.EQ.4.0D0) FANWV4=.TRUE.
                      IF(W2.EQ.5.0D0) FANWV5=.TRUE.
                      IF(W2.EQ.6.0D0) FANWV6=.TRUE.
                      IF(W2.EQ.7.0D0) FANWV7=.TRUE.
                      IF(W2.EQ.8.0D0) FANWV8=.TRUE.
                      IF(W2.EQ.9.0D0) FANWV9=.TRUE.
                      IF(W2.EQ.10.0D0) FANWV10=.TRUE.
                      IF(W2.EQ.-1.0D0) FANWV1=.FALSE.
                      IF(W2.EQ.-2.0D0) FANWV2=.FALSE.
                      IF(W2.EQ.-3.0D0) FANWV3=.FALSE.
                      IF(W2.EQ.-4.0D0) FANWV4=.FALSE.
                      IF(W2.EQ.-5.0D0) FANWV5=.FALSE.
                      IF(W2.EQ.-6.0D0) FANWV6=.FALSE.
                      IF(W2.EQ.-7.0D0) FANWV7=.FALSE.
                      IF(W2.EQ.-8.0D0) FANWV8=.FALSE.
                      IF(W2.EQ.-9.0D0) FANWV9=.FALSE.
                      IF(W2.EQ.-10.0D0) FANWV10=.FALSE.
                      IF(W2.EQ.1.0D0) JK_WAV(7)=1
                      IF(W2.EQ.2.0D0) JK_WAV(7)=2
                      IF(W2.EQ.3.0D0) JK_WAV(7)=3
                      IF(W2.EQ.4.0D0) JK_WAV(7)=4
                      IF(W2.EQ.5.0D0) JK_WAV(7)=5
                      IF(W2.EQ.6.0D0) JK_WAV(7)=6
                      IF(W2.EQ.7.0D0) JK_WAV(7)=7
                      IF(W2.EQ.8.0D0) JK_WAV(7)=8
                      IF(W2.EQ.9.0D0) JK_WAV(7)=9
                      IF(W2.EQ.10.0D0) JK_WAV(7)=10
                      IF(W2.EQ.-1.0D0) JK_WAV(7)=0
                      IF(W2.EQ.-2.0D0) JK_WAV(7)=0
                      IF(W2.EQ.-3.0D0) JK_WAV(7)=0
                      IF(W2.EQ.-4.0D0) JK_WAV(7)=0
                      IF(W2.EQ.-5.0D0) JK_WAV(7)=0
                      IF(W2.EQ.-6.0D0) JK_WAV(7)=0
                      IF(W2.EQ.-7.0D0) JK_WAV(7)=0
                      IF(W2.EQ.-8.0D0) JK_WAV(7)=0
                      IF(W2.EQ.-9.0D0) JK_WAV(7)=0
                      IF(W2.EQ.-10.0D0) JK_WAV(7)=0
                  ELSE
C     DON'T DO ANYTHING
                  END IF
                  IF(DF3.EQ.0) THEN
                      IF(W3.EQ.1.0D0) FANWV1=.TRUE.
                      IF(W3.EQ.2.0D0) FANWV2=.TRUE.
                      IF(W3.EQ.3.0D0) FANWV3=.TRUE.
                      IF(W3.EQ.4.0D0) FANWV4=.TRUE.
                      IF(W3.EQ.5.0D0) FANWV5=.TRUE.
                      IF(W3.EQ.6.0D0) FANWV6=.TRUE.
                      IF(W3.EQ.7.0D0) FANWV7=.TRUE.
                      IF(W3.EQ.8.0D0) FANWV8=.TRUE.
                      IF(W3.EQ.9.0D0) FANWV9=.TRUE.
                      IF(W3.EQ.10.0D0) FANWV10=.TRUE.
                      IF(W3.EQ.-1.0D0) FANWV1=.FALSE.
                      IF(W3.EQ.-2.0D0) FANWV2=.FALSE.
                      IF(W3.EQ.-3.0D0) FANWV3=.FALSE.
                      IF(W3.EQ.-4.0D0) FANWV4=.FALSE.
                      IF(W3.EQ.-5.0D0) FANWV5=.FALSE.
                      IF(W3.EQ.-6.0D0) FANWV6=.FALSE.
                      IF(W3.EQ.-7.0D0) FANWV7=.FALSE.
                      IF(W3.EQ.-8.0D0) FANWV8=.FALSE.
                      IF(W3.EQ.-9.0D0) FANWV9=.FALSE.
                      IF(W3.EQ.-10.0D0) FANWV10=.FALSE.
                      IF(W3.EQ.1.0D0) JK_WAV(8)=1
                      IF(W3.EQ.2.0D0) JK_WAV(8)=2
                      IF(W3.EQ.3.0D0) JK_WAV(8)=3
                      IF(W3.EQ.4.0D0) JK_WAV(8)=4
                      IF(W3.EQ.5.0D0) JK_WAV(8)=5
                      IF(W3.EQ.6.0D0) JK_WAV(8)=6
                      IF(W3.EQ.7.0D0) JK_WAV(8)=7
                      IF(W3.EQ.8.0D0) JK_WAV(8)=8
                      IF(W3.EQ.9.0D0) JK_WAV(8)=9
                      IF(W3.EQ.10.0D0) JK_WAV(8)=10
                      IF(W3.EQ.-1.0D0) JK_WAV(8)=0
                      IF(W3.EQ.-2.0D0) JK_WAV(8)=0
                      IF(W3.EQ.-3.0D0) JK_WAV(8)=0
                      IF(W3.EQ.-4.0D0) JK_WAV(8)=0
                      IF(W3.EQ.-5.0D0) JK_WAV(8)=0
                      IF(W3.EQ.-6.0D0) JK_WAV(8)=0
                      IF(W3.EQ.-7.0D0) JK_WAV(8)=0
                      IF(W3.EQ.-8.0D0) JK_WAV(8)=0
                      IF(W3.EQ.-9.0D0) JK_WAV(8)=0
                      IF(W3.EQ.-10.0D0) JK_WAV(8)=0
                  ELSE
C     DON'T DO ANYTHING
                  END IF
                  IF(DF4.EQ.0) THEN
                      IF(W4.EQ.1.0D0) FANWV1=.TRUE.
                      IF(W4.EQ.2.0D0) FANWV2=.TRUE.
                      IF(W4.EQ.3.0D0) FANWV3=.TRUE.
                      IF(W4.EQ.4.0D0) FANWV4=.TRUE.
                      IF(W4.EQ.5.0D0) FANWV5=.TRUE.
                      IF(W4.EQ.6.0D0) FANWV6=.TRUE.
                      IF(W4.EQ.7.0D0) FANWV7=.TRUE.
                      IF(W4.EQ.8.0D0) FANWV8=.TRUE.
                      IF(W4.EQ.9.0D0) FANWV9=.TRUE.
                      IF(W4.EQ.10.0D0) FANWV10=.TRUE.
                      IF(W4.EQ.-1.0D0) FANWV1=.FALSE.
                      IF(W4.EQ.-2.0D0) FANWV2=.FALSE.
                      IF(W4.EQ.-3.0D0) FANWV3=.FALSE.
                      IF(W4.EQ.-4.0D0) FANWV4=.FALSE.
                      IF(W4.EQ.-5.0D0) FANWV5=.FALSE.
                      IF(W4.EQ.-6.0D0) FANWV6=.FALSE.
                      IF(W4.EQ.-7.0D0) FANWV7=.FALSE.
                      IF(W4.EQ.-8.0D0) FANWV8=.FALSE.
                      IF(W4.EQ.-9.0D0) FANWV9=.FALSE.
                      IF(W4.EQ.-10.0D0) FANWV10=.FALSE.
                      IF(W4.EQ.1.0D0) JK_WAV(9)=1
                      IF(W4.EQ.2.0D0) JK_WAV(9)=2
                      IF(W4.EQ.3.0D0) JK_WAV(9)=3
                      IF(W4.EQ.4.0D0) JK_WAV(9)=4
                      IF(W4.EQ.5.0D0) JK_WAV(9)=5
                      IF(W4.EQ.6.0D0) JK_WAV(9)=6
                      IF(W4.EQ.7.0D0) JK_WAV(9)=7
                      IF(W4.EQ.8.0D0) JK_WAV(9)=8
                      IF(W4.EQ.9.0D0) JK_WAV(9)=9
                      IF(W4.EQ.10.0D0) JK_WAV(9)=10
                      IF(W4.EQ.-1.0D0) JK_WAV(9)=0
                      IF(W4.EQ.-2.0D0) JK_WAV(9)=0
                      IF(W4.EQ.-3.0D0) JK_WAV(9)=0
                      IF(W4.EQ.-4.0D0) JK_WAV(9)=0
                      IF(W4.EQ.-5.0D0) JK_WAV(9)=0
                      IF(W4.EQ.-6.0D0) JK_WAV(9)=0
                      IF(W4.EQ.-7.0D0) JK_WAV(9)=0
                      IF(W4.EQ.-8.0D0) JK_WAV(9)=0
                      IF(W4.EQ.-9.0D0) JK_WAV(9)=0
                      IF(W4.EQ.-10.0D0) JK_WAV(9)=0
                  ELSE
C     DON'T DO ANYTHING
                  END IF
                  IF(DF5.EQ.0) THEN
                      IF(W5.EQ.1.0D0) FANWV1=.TRUE.
                      IF(W5.EQ.2.0D0) FANWV2=.TRUE.
                      IF(W5.EQ.3.0D0) FANWV3=.TRUE.
                      IF(W5.EQ.4.0D0) FANWV4=.TRUE.
                      IF(W5.EQ.5.0D0) FANWV5=.TRUE.
                      IF(W5.EQ.6.0D0) FANWV6=.TRUE.
                      IF(W5.EQ.7.0D0) FANWV7=.TRUE.
                      IF(W5.EQ.8.0D0) FANWV8=.TRUE.
                      IF(W5.EQ.9.0D0) FANWV9=.TRUE.
                      IF(W5.EQ.10.0D0) FANWV10=.TRUE.
                      IF(W5.EQ.-1.0D0) FANWV1=.FALSE.
                      IF(W5.EQ.-2.0D0) FANWV2=.FALSE.
                      IF(W5.EQ.-3.0D0) FANWV3=.FALSE.
                      IF(W5.EQ.-4.0D0) FANWV4=.FALSE.
                      IF(W5.EQ.-5.0D0) FANWV5=.FALSE.
                      IF(W5.EQ.-6.0D0) FANWV6=.FALSE.
                      IF(W5.EQ.-7.0D0) FANWV7=.FALSE.
                      IF(W5.EQ.-8.0D0) FANWV8=.FALSE.
                      IF(W5.EQ.-9.0D0) FANWV9=.FALSE.
                      IF(W5.EQ.-10.0D0) FANWV10=.FALSE.
                      IF(W5.EQ.1.0D0) JK_WAV(10)=1
                      IF(W5.EQ.2.0D0) JK_WAV(10)=2
                      IF(W5.EQ.3.0D0) JK_WAV(10)=3
                      IF(W5.EQ.4.0D0) JK_WAV(10)=4
                      IF(W5.EQ.5.0D0) JK_WAV(10)=5
                      IF(W5.EQ.6.0D0) JK_WAV(10)=6
                      IF(W5.EQ.7.0D0) JK_WAV(10)=7
                      IF(W5.EQ.8.0D0) JK_WAV(10)=8
                      IF(W5.EQ.9.0D0) JK_WAV(10)=9
                      IF(W5.EQ.10.0D0) JK_WAV(10)=10
                      IF(W5.EQ.-1.0D0) JK_WAV(10)=0
                      IF(W5.EQ.-2.0D0) JK_WAV(10)=0
                      IF(W5.EQ.-3.0D0) JK_WAV(10)=0
                      IF(W5.EQ.-4.0D0) JK_WAV(10)=0
                      IF(W5.EQ.-5.0D0) JK_WAV(10)=0
                      IF(W5.EQ.-6.0D0) JK_WAV(10)=0
                      IF(W5.EQ.-7.0D0) JK_WAV(10)=0
                      IF(W5.EQ.-8.0D0) JK_WAV(10)=0
                      IF(W5.EQ.-9.0D0) JK_WAV(10)=0
                      IF(W5.EQ.-10.0D0) JK_WAV(10)=0
                  ELSE
C     DON'T DO ANYTHING
                  END IF
                  RETURN
              ELSE
              END IF
C
C     PLOTFANS SSI
              IF(WC.EQ.'PLOTFANS'.AND.WQ.EQ.'SSI') THEN
                  IF(W1.LE.0.0D0) THEN
                      OUTLYNE=
     1                'THE "SSI" VALUE MUST BE GREATER THAN ZERO'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  ELSE
                      SSI=W1/2.0D0
                      SSIFLG=.FALSE.
                  END IF
                  RETURN
              ELSE
              END IF
C     PLOTFANS OFFSET
              IF(WC.EQ.'PLOTFANS'.AND.WQ.EQ.'OFFSET') THEN
C     THE OFFSET IS A FRACTIONAL APERTURE DIMMENSION
                  FANOFF=W1
                  RETURN
              ELSE
              END IF
C
C     PLOTFANS REFWV
              IF(WC.EQ.'PLOTFANS'.AND.WQ.EQ.'REFWV') THEN
                  IF(W1.NE.1.0D0.AND.W1.NE.2.0D0.AND.W1.NE.3.0D0.AND.W1.NE.4.0D0
     1            .AND.W1.NE.5.0D0.AND.W1.NE.6.0D0.AND.W1.NE.7.0D0.AND.W1.NE.8.0D0
     1            .AND.W1.NE.9.0D0.AND.W1.NE.10.0D0) THEN
                      OUTLYNE=
     1                  'REFERENCE WAVELENGTH NUMBER INPUT VALUE MUST BE:'
                      CALL SHOWIT(1)
                      OUTLYNE=
     1                  '1, 2, 3, 4, 5, 6, 7, 8, 9 0R 10'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  ELSE
                      REFWV=INT(W1)
                  END IF
                  RETURN
              ELSE
              END IF

C     PLOTFANS NEWREF,NEWIMG AND NEWOBJ GO HERE
C
C     PLOTFANS NEWOBJ
              IF(WC.EQ.'PLOTFANS'.AND.WQ.EQ.'NEWOBJ') THEN
                  IF(W1.LT.0.0D0.OR.W1.GE.SYSTEM1(20).OR.W1.GE.SYSTEM1(25)
     1            .OR.W1.GE.DBLE(NEWREF).OR.W1.GE.DBLE(NEWIMG)) THEN
                      OUTLYNE=
     1                  'NEW OBJECT SURFACE LOCATION IS BEYOND LEGAL BOUNDS'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  ELSE
                      OLDOBJ=NEWOBJ
                      NEWOBJ=INT(W1)
                  END IF
                  RETURN
              ELSE
              END IF
C
C     PLOTFANS NEWREF
              IF(WC.EQ.'PLOTFANS'.AND.WQ.EQ.'NEWREF') THEN
                  IF(W1.LE.0.0D0.OR.W1.LE.DBLE(NEWOBJ).OR.W1.GE.SYSTEM1(20)
     1            .OR.W1.GE.DBLE(NEWIMG)) THEN
                      OUTLYNE=
     1                'NEW REFERENCE SURFACE LOCATION IS BEYOND LEGAL BOUNDS'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  ELSE
                      OLDREF=NEWREF
                      NEWREF=INT(W1)
                  END IF
                  RETURN
              ELSE
              END IF
C
C     PLOTFANS NEWIMG
              IF(WC.EQ.'PLOTFANS'.AND.WQ.EQ.'NEWIMG') THEN
                  IF(W1.LE.0.0D0.OR.W1.LE.DBLE(NEWOBJ).OR.W1.LE.SYSTEM1(25)
     1            .OR.W1.LE.DBLE(NEWREF).OR.W1.GT.SYSTEM1(20)) THEN
                      OUTLYNE=
     1                'NEW IMAGE SURFACE LOCATION IS BEYOND LEGAL BOUNDS'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  ELSE
                      OLDIMG=NEWIMG
                      NEWIMG=INT(W1)
                  END IF
                  RETURN
              ELSE
              END IF
C
C     PLOTFANS YFOB
              IF(WC.EQ.'PLOTFANS'.AND.WQ.EQ.'YFOB') THEN
                  YFOB1=W1
                  YFOB2=W2
                  YFOB3=W3
                  FANNM1=3
                  IF(DF3.EQ.1) THEN
                      FANNM1=FANNM1-1
                  ELSE
                      GO TO 1
                  END IF
                  IF(DF2.EQ.1) THEN
                      FANNM1=FANNM1-1
                  ELSE
                      GO TO 1
                  END IF
 1                CONTINUE
                  IF(FANNM1.LE.FANNM2) THEN
                      FANNM2=FANNM1
                      FANNUM=FANNM2
                  ELSE
                      FANNM1=FANNM2
                      FANNUM=FANNM2
                  END IF
C     YFOBS AND FANNUM ARE SET
                  RETURN
              ELSE
              END IF
C
C     PLOTFANS XFOB
              IF(WC.EQ.'PLOTFANS'.AND.WQ.EQ.'XFOB') THEN
                  XFOB1=W1
                  XFOB2=W2
                  XFOB3=W3
                  FANNM2=3
                  IF(DF3.EQ.1) THEN
                      FANNM2=FANNM2-1
                  ELSE
                      GO TO 2
                  END IF
                  IF(DF2.EQ.1) THEN
                      FANNM2=FANNM2-1
                  ELSE
                      GO TO 2
                  END IF
 2                CONTINUE
                  IF(FANNM1.LE.FANNM2) THEN
                      FANNM2=FANNM1
                      FANNUM=FANNM2
                  ELSE
                      FANNM1=FANNM2
                      FANNUM=FANNM2
                  END IF
C     YFOBS AND FANNUM ARE SET
                  RETURN
              ELSE
              END IF

C
C     PLTYFAN (QUALIFIER)
              IF(WC.EQ.'PLTYFAN') THEN
                  FANTYP=1
                  IF(WQ.EQ.' ') QALTYP=0
                  IF(WQ.EQ.'OPD') QALTYP=1
                  IF(WQ.EQ.'CD')  QALTYP=2
                  IF(WQ.EQ.'LA')  QALTYP=3
              ELSE
              END IF
C
C     PLTXFAN (QUALIFIER)
              IF(WC.EQ.'PLTXFAN') THEN
                  FANTYP=2
                  IF(WQ.EQ.' ') QALTYP=0
                  IF(WQ.EQ.'OPD') QALTYP=1
                  IF(WQ.EQ.'CD')  QALTYP=2
                  IF(WQ.EQ.'LA')  QALTYP=3
              ELSE
              END IF
C
C     PLTNFAN (QUALIFIER)
              IF(WC.EQ.'PLTNFAN') THEN
                  FANTYP=3
                  IF(WQ.EQ.' ') QALTYP=0
                  IF(WQ.EQ.'OPD') QALTYP=1
                  IF(WQ.EQ.'CD')  QALTYP=2
                  IF(WQ.EQ.'LA')  QALTYP=3
              ELSE
              END IF
C
C     PLTPFAN (QUALIFIER)
              IF(WC.EQ.'PLTPFAN') THEN
                  FANTYP=4
                  IF(WQ.EQ.' ') QALTYP=0
                  IF(WQ.EQ.'OPD') QALTYP=1
                  IF(WQ.EQ.'CD')  QALTYP=2
                  IF(WQ.EQ.'LA')  QALTYP=3
              ELSE
              END IF
C
C     PLTPFAN (QUALIFIER)
              IF(WC.EQ.'PLTXYFAN') THEN
                  FANTYP=5
                  IF(WQ.EQ.' ') QALTYP=0
                  IF(WQ.EQ.'OPD') QALTYP=1
                  IF(WQ.EQ.'CD')  QALTYP=2
                  IF(WQ.EQ.'LA')  QALTYP=3
              ELSE
              END IF
C
C     PLTPFAN (QUALIFIER)
              IF(WC.EQ.'PLTYXFAN') THEN
                  FANTYP=6
                  IF(WQ.EQ.' ') QALTYP=0
                  IF(WQ.EQ.'OPD') QALTYP=1
                  IF(WQ.EQ.'CD')  QALTYP=2
                  IF(WQ.EQ.'LA')  QALTYP=3
              ELSE
              END IF
          ELSE
C     STI = 1
          END IF
C
C     NOW DO THE WORK FOR STI = 1 (QUERRY)
          IF(STI.EQ.1) THEN
              IF(WC.EQ.'PLTXFAN'.OR.WC.EQ.'PLTXFAN'.OR.WC.EQ.'PLTNFAN'.OR.
     1         WC.EQ.'PLTPFAN') THEN
                  IF(FANTYP.EQ.1) A1VAL1=' YFAN'
                  IF(FANTYP.EQ.2) A1VAL1=' XFAN'
                  IF(FANTYP.EQ.3) A1VAL1=' NFAN'
                  IF(FANTYP.EQ.4) A1VAL1=' PFAN'
                  IF(FANTYP.EQ.5) A1VAL1='XYFAN'
                  IF(FANTYP.EQ.6) A1VAL1='YXFAN'
                  IF(QALTYP.EQ.0) A2VAL2=' '
                  IF(QALTYP.EQ.1) A2VAL2='OPD'
                  IF(QALTYP.EQ.2) A2VAL2='CD'
                  IF(QALTYP.EQ.3) A2VAL2='LA'
                  IF(QALTYP.EQ.0) WRITE(OUTLYNE,20) A1VAL1
                  CALL SHOWIT(3)
 20               FORMAT('CURRENT PLOTTED FAN WILL BE "',A5,'"')
                  IF(QALTYP.EQ.1) WRITE(OUTLYNE,21) A1VAL1,A2VAL2
                  CALL SHOWIT(3)
 21               FORMAT('CURRENT PLOTTED FAN WILL BE "',A5,1X,A3,'"')
                  IF(QALTYP.EQ.2) WRITE(OUTLYNE,22) A1VAL1,A2VAL2
                  CALL SHOWIT(3)
                  IF(QALTYP.EQ.3) WRITE(OUTLYNE,22) A1VAL1,A2VAL2
                  CALL SHOWIT(3)
 22               FORMAT('CURRENT PLOTTED FAN WILL BE "',A5,1X,A2,'"')
                  RETURN
              ELSE
C     NOT PLTYFAN, PLTXFAN, PLTNFAN OR PLTPFAN
              END IF
C
              IF(WC.EQ.'PLOTFANS') THEN
                  IF(WQ.EQ.'GO') THEN
                      WRITE(OUTLYNE,23)
                      CALL SHOWIT(3)
 23                   FORMAT(
     1                '"PLOTFANS GO" CAUSES A FAN TO BE PLOTTED')
                      RETURN
                  ELSE
                  END IF


                  IF(WQ.EQ.'RESET') THEN
                      WRITE(OUTLYNE,24)
                      CALL SHOWIT(3)
 24                   FORMAT(
     1                '"PLOTFANS RESET" RESETS FAN PLOTTING DEFAULT VALUES')
                      RETURN
                  ELSE
                  END IF
                  IF(WQ.EQ.'WV') THEN
                      AWV1= '                               '
                      AWV2= '                               '
                      AWV3= '                               '
                      AWV4= '                               '
                      AWV5= '                               '
                      AWV6= '                               '
                      AWV7= '                               '
                      AWV8= '                               '
                      AWV9= '                               '
                      AWV10= '                                '
                      IF(FANWV1) AWV1='WAVELENGTH 1 FANS WILL BE DRAWN'
                      IF(FANWV2) AWV2='WAVELENGTH 2 FANS WILL BE DRAWN'
                      IF(FANWV3) AWV3='WAVELENGTH 3 FANS WILL BE DRAWN'
                      IF(FANWV4) AWV4='WAVELENGTH 4 FANS WILL BE DRAWN'
                      IF(FANWV5) AWV5='WAVELENGTH 5 FANS WILL BE DRAWN'
                      IF(FANWV6) AWV6='WAVELENGTH 6 FANS WILL BE DRAWN'
                      IF(FANWV7) AWV7='WAVELENGTH 7 FANS WILL BE DRAWN'
                      IF(FANWV8) AWV8='WAVELENGTH 8 FANS WILL BE DRAWN'
                      IF(FANWV9) AWV9='WAVELENGTH 9 FANS WILL BE DRAWN'
                      IF(FANWV10) AWV10='WAVELENGTH 10 FANS WILL BE DRAWN'
                      IF(FANWV1) WRITE(OUTLYNE,25) AWV1
                      CALL SHOWIT(3)
                      IF(FANWV2) WRITE(OUTLYNE,25) AWV2
                      CALL SHOWIT(3)
                      IF(FANWV3) WRITE(OUTLYNE,25) AWV3
                      CALL SHOWIT(3)
                      IF(FANWV4) WRITE(OUTLYNE,25) AWV4
                      CALL SHOWIT(3)
                      IF(FANWV5) WRITE(OUTLYNE,25) AWV5
                      CALL SHOWIT(3)
                      IF(FANWV6) WRITE(OUTLYNE,25) AWV6
                      CALL SHOWIT(3)
                      IF(FANWV7) WRITE(OUTLYNE,25) AWV7
                      CALL SHOWIT(3)
                      IF(FANWV8) WRITE(OUTLYNE,25) AWV8
                      CALL SHOWIT(3)
                      IF(FANWV9) WRITE(OUTLYNE,25) AWV9
                      CALL SHOWIT(3)
                      IF(FANWV10) WRITE(OUTLYNE,251) AWV10
                      CALL SHOWIT(3)
 25                   FORMAT(A31)
 251                  FORMAT(A32)
                      RETURN
                  ELSE
                  END IF
                  IF(WQ.EQ.'OFFSET') THEN
                      WRITE(OUTLYNE,26) FANOFF
                      CALL SHOWIT(3)
 26                   FORMAT('CURRENT FAN OFFSET = ',D23.15)
                      RETURN
                  ELSE
                  END IF
                  IF(WQ.EQ.'SSI') THEN
                      IF(SSIFLG) THEN
                          WRITE(OUTLYNE,27)
                          CALL SHOWIT(3)
 27                       FORMAT(
     1                    'SSI WILL BE DETERMINED AUTOMATICALLY DURING FAN PLOTTING')
                          RETURN
                      ELSE
                          WRITE(OUTLYNE,28) SSI
                          CALL SHOWIT(3)
 28                       FORMAT('CURRENT FAN SSI VALUE = ',D23.15)
                      END IF
                      RETURN
                  ELSE
                  END IF
                  IF(WQ.EQ.'XFOB') THEN
                      AXF1 = '"XFOB" FOR FAN #1 IS = '
                      AXF2 = '"XFOB" FOR FAN #2 IS = '
                      AXF3 = '"XFOB" FOR FAN #3 IS = '
                      IF(FANNUM.GE.1) WRITE(OUTLYNE,29) AXF1,XFOB1
                      CALL SHOWIT(3)
                      IF(FANNUM.GE.2) WRITE(OUTLYNE,29) AXF2,XFOB2
                      CALL SHOWIT(3)
                      IF(FANNUM.EQ.3) WRITE(OUTLYNE,29) AXF3,XFOB3
                      CALL SHOWIT(3)
 29                   FORMAT(A23,D23.15)
                      RETURN
                  ELSE
                  END IF
                  IF(WQ.EQ.'YFOB') THEN
                      AYF1 = '"YFOB" FOR FAN #1 IS = '
                      AYF2 = '"YFOB" FOR FAN #2 IS = '
                      AYF3 = '"YFOB" FOR FAN #3 IS = '
                      IF(FANNUM.GE.1) WRITE(OUTLYNE,30) AYF1,YFOB1
                      CALL SHOWIT(3)
                      IF(FANNUM.GE.2) WRITE(OUTLYNE,30) AYF2,YFOB2
                      CALL SHOWIT(3)
                      IF(FANNUM.EQ.3) WRITE(OUTLYNE,30) AYF3,YFOB3
                      CALL SHOWIT(3)
 30                   FORMAT(A23,D23.15)
                      RETURN
                  ELSE
                  END IF
              ELSE
C     NOT PLOTFANS
              END IF
C
              IF(WQ.EQ.'REFWV') THEN
                  WRITE(OUTLYNE,31) REFWV
                  CALL SHOWIT(3)
 31               FORMAT(
     1            'CURRENT FAN PLOTTING REFERENCE WAVELENGTH NUMBER = ',I3)
                  RETURN
              ELSE
              END IF
C
              IF(WQ.EQ.'NEWOBJ') THEN
                  WRITE(OUTLYNE,32) NEWOBJ
                  CALL SHOWIT(3)
 32               FORMAT(
     1            'CURRENT OBJECT SURFACE NUMBER FOR FAN PLOTTING = ',I3)
                  RETURN
              ELSE
              END IF
C
              IF(WQ.EQ.'NEWREF') THEN
                  WRITE(OUTLYNE,33) NEWREF
                  CALL SHOWIT(3)
 33               FORMAT(
     1            'CURRENT REFERENCE SURFACE NUMBER FOR FAN PLOTTING = ',I3)
                  RETURN
              ELSE
              END IF
C
              IF(WQ.EQ.'NEWIMG') THEN
                  WRITE(OUTLYNE,34) NEWIMG
                  CALL SHOWIT(3)
 34               FORMAT(
     1            'CURRENT IMAGE SURFACE NUMBER FOR FAN PLOTTING = ',I3)
                  RETURN
              ELSE
              END IF
C
          ELSE
C     NOT QUERY, CONTINUE
          END IF
C
C     HERE IS WHERE THE ACTUAL PLOT GENERATION TAKES PLACE
C
          IF(WC.EQ.'PLOTFANS'.AND.WQ.EQ.'GO') THEN
C
              IF(.NOT.FANWV1.AND..NOT.FANWV2.AND..NOT.FANWV3
     1        .AND..NOT.FANWV4.AND..NOT.FANWV5.AND..NOT.FANWV6
     1        .AND..NOT.FANWV7.AND..NOT.FANWV8.AND..NOT.FANWV9
     1        .AND..NOT.FANWV10) THEN
                  OUTLYNE='ALL WAVELENGTHS WERE SHUT OFF'
                  CALL SHOWIT(1)
                  OUTLYNE='NO FANS COULD BE TRACED'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              ELSE
              END IF
C
C     GENERATE PLOT DATA AND RETURN
C
C     NOW CALCULATE THE FAN DATA AND THEN THE SCALE FACTOR
              ABORT=.FALSE.
C     ZERO THE FFAANN
C     I COUNTS THE NUMBER OF FANS ON THE PLOT. 1, 2 OR 3
C     FAN I=1 IS THE BOTTOM FAN AT FIELD POSITION 1
C     II COUNTS THE 55 RAYS IN THE FAN II=1 IS NEG LIMIT,
C     II = 21 IS TH POS SIDE
C     III COUNTS THE 6 DATA ITEMS IN THE FAN
C     IIII COUNTS THE 10 WAVELENGTH ENTRIES IN THE FAN
C     FFAANN IS PLOT POINTS
C
C     TRANSVERSE FANS THE (QALTYP=0)
C                 III=1 IS TRANSVERS X ABERRATION
C                 III=2 IS TRANSVERS Y ABERRATION
C                 III=3 IS NOT USED
C                 III=4 IS NOT USED
C                 III=5 IS FLAG1 (1=PLOT,0=DON'T PLOT)
C                 III=6 IS FLAG2 (1=PLOT,0=DON'T PLOT)
C     OPD FANS (QALTYP=1)
C                 III=1 OPD IN WAVES AT REF WAVELENGTH
C                 III=2 IS NOT USED
C                 III=3 IS NOT USED
C                 III=4 IS NOT USED
C                 III=5 IS FLAG1 (1=PLOT,0=DON'T PLOT)
C                 III=6 IS FLAG2 (1=PLOT,0=DON'T PLOT)
C     CHROMATIC DIFFERENCE FANS (QALTYP=2)
C                 III=1 XZ PRIMARY CHROMATIC PAIR
C                 III=2 YZ PRIMARY CHROMATIC PAIR
C                 III=3 XZ SECONDARY CHROMATIC PAIR
C                 III=4 YZ SECONDARY CHROMATIC PAIR
C                 III=5 IS FLAG1 (1=PLOT,0=DON'T PLOT)
C                 III=6 IS FLAG2 (1=PLOT,0=DON'T PLOT)
C     CHROMATIC LONGITUDINAL (QALTYP=3)
C                 III=1 LAX
C                 III=2 LAY
C                 III=3 IS NOT USED
C                 III=4 IS NOT USED
C                 III=5 IS FLAG1 (1=PLOT,0=DON'T PLOT)
C                 III=6 IS FLAG2 (1=PLOT,0=DON'T PLOT)
C
C     CALCULATE ALL THE FFAANN
C     OR (PLFAN2 FOR FANTYPs 5 AND 6)
              IF(FANTYP.LE.4) CALL PLFAN1(ABORT,IFA)
              IF(FANTYP.GT.4) CALL PLFAN2(ABORT,IFA)
              IF(ABORT) THEN
C     ALL ERROR MESSAGES DONE IN PLFAN1 OR PLFAN2
                  WRITE(OUTLYNE,37) IFA
 37               FORMAT(
     1            'FOR FIELD OF VIEW NUMBER ',I1)
                  CALL SHOWIT(1)
                  OUTLYNE=
     1            'A CHIEF RAY FAILURE HAS CAUSED THE CURRENT FAN PLOT TO ABORT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              ELSE
C     FANS ARE OK, DO REST OF PLOT
              END IF
C
C     SET UP A CHARACTER VALUE FOR THE SYSTEM UNITS
              IF(SYSTEM1(6).EQ.1.0D0) UNI='INCH      '
              IF(SYSTEM1(6).EQ.2.0D0) UNI='CENTIMETER'
              IF(SYSTEM1(6).EQ.3.0D0) UNI='MILLIMETER'
              IF(SYSTEM1(6).EQ.4.0D0) UNI='METER     '

              LCW=SYSTEM1(INT(SYSTEM1(11)))
              LPWP1=SYSTEM1(INT(SYSTEM1(7)))
              LPWP2=SYSTEM1(INT(SYSTEM1(8)))
              LSWP1=SYSTEM1(INT(SYSTEM1(9)))
              LSWP2=SYSTEM1(INT(SYSTEM1(10)))
C
C     PLOT THE FAN DATA AND THE SCALE FACTOR DATA AND CAPTION
              CALL FANDO0
C     PLOT THE BOX AROUND THE PLOT
              CALL FRMBOX
C     PLOT THE LENS IDENTIFIER IF IT IS NOT BLANK
              CALL FANLI
C     PLOT THE WAVELENGTH LEGEND
              IF(QALTYP.NE.2) CALL FANWV
C
C     PLOT THE CHROMATIC DIFFERENCE LEGEND
              IF(QALTYP.EQ.2) CALL FANCD
C
C     PLOT THE REFERENCE WAVELENGTH LEGEND
              CALL FANRWV
C     PLOT THE NAME OF THE TYPE OF PLOT AND THE UNITS OF THE ABERRATION
C     PLOTTED
              CALL FANTP
C     THAT'S IT, WE ARE DONE
C     BEFORE LEAVING, RESTORE THE OLD REF,IMG AND OBJ SURFACES
              CALL OLDSUR
              RETURN
          ELSE
C     NOT 'GO'
          END IF
      END

C SUB PLFAN2.FOR

      SUBROUTINE PLFAN2(ABORT,IFA)
C
          IMPLICIT NONE
C
C     DOES FAN CALCULATIONS FOR FAN PLOTTING (FANTYP 5 6)
C
          INTEGER J,JJ,IX,I,JJJ,K,KKK,IFA
     1    ,ICOUNT,KFNN,FANWAV,JK_WVN,WWRF
C
          INTEGER JK_WAV(1:10)
C
          COMMON/WAVER/JK_WAV
C
          REAL*8 COSARG,
     1    OFFSET,XI,XX1,YY1,LLR,MMR,NNR,LLP,MMP,NNP,
     2    XXDIF,YYDIF,OOPD,OPDW,
     3    WAV,RRDIF,DIF1,DIF2,DIF3,DIF4,PW11,PW12,PW21,PW22,
     4    SW11,SW12,SW21,SW22,LAX,LAY,DX,DY,DTY,DTX,XX2,YY2
     5    ,JA,JB
C
          LOGICAL FOBB0,FANEXT
     1    ,FOBB0X,FOBB0Y,ABORT
C
          COMMON/FANEXI/FANEXT
C
          COMMON/FANNER/FANWAV
C
          COMMON/FFOOBB/FOBB0,FOBB0X,FOBB0Y
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          INTEGER WWVN
          JA=COS_A_ANG
          JB=COS_B_ANG
          IF(INT(LFOB(4)).EQ.1) WWRF=46
          IF(INT(LFOB(4)).EQ.2) WWRF=47
          IF(INT(LFOB(4)).EQ.3) WWRF=48
          IF(INT(LFOB(4)).EQ.4) WWRF=49
          IF(INT(LFOB(4)).EQ.5) WWRF=50
          IF(INT(LFOB(4)).EQ.6) WWRF=71
          IF(INT(LFOB(4)).EQ.7) WWRF=72
          IF(INT(LFOB(4)).EQ.8) WWRF=73
          IF(INT(LFOB(4)).EQ.9) WWRF=74
          IF(INT(LFOB(4)).EQ.10) WWRF=75
          IF(FANEXT) THEN
              IF(FANWAV.EQ.1) WWVN=46
              IF(FANWAV.EQ.2) WWVN=47
              IF(FANWAV.EQ.3) WWVN=48
              IF(FANWAV.EQ.4) WWVN=49
              IF(FANWAV.EQ.5) WWVN=50
              IF(FANWAV.EQ.6) WWVN=71
              IF(FANWAV.EQ.7) WWVN=72
              IF(FANWAV.EQ.8) WWVN=73
              IF(FANWAV.EQ.9) WWVN=74
              IF(FANWAV.EQ.10) WWVN=75
          END IF
C
          IF(QALTYP.EQ.0) KKK=10
          IF(QALTYP.EQ.1) KKK=10
          IF(QALTYP.EQ.2) KKK=1
          IF(QALTYP.EQ.3) KKK=10
          IF(FANEXT) KKK=1
          IF(FANEXT) K=1
          DO K=1,KKK
              IF(QALTYP.NE.2) THEN
C     NOT CD FANS
                  IF(K.EQ.1.AND.JK_WAV(1).EQ.0.OR.
     1            K.EQ.2.AND.JK_WAV(2).EQ.0.OR.
     1            K.EQ.3.AND.JK_WAV(3).EQ.0.OR.
     1            K.EQ.4.AND.JK_WAV(4).EQ.0.OR.
     1            K.EQ.5.AND.JK_WAV(5).EQ.0.OR.
     1            K.EQ.6.AND.JK_WAV(6).EQ.0.OR.
     1            K.EQ.7.AND.JK_WAV(7).EQ.0.OR.
     1            K.EQ.8.AND.JK_WAV(8).EQ.0.OR.
     1            K.EQ.9.AND.JK_WAV(9).EQ.0.OR.
     1            K.EQ.10.AND.JK_WAV(10).EQ.0) THEN
                      FFAANN(1:3,1:21,1:4,K)=0.0D0
                      FFAANN(1:3,1:21,5:6,K)=1.0D0
C     SPECTRAL WEIGHT IS ZERO, SKIP THE CALCULATIONS
                      GO TO 99999
                  ELSE
C     WE WANT TO TRACE RAYS
C     SET WW3
                      IF(FANEXT) THEN
                          WW3=DBLE(JK_WAV(1))
                          IF(FANWAV.EQ.1) FANWV1=.TRUE.
                          IF(FANWAV.EQ.2) FANWV2=.TRUE.
                          IF(FANWAV.EQ.3) FANWV3=.TRUE.
                          IF(FANWAV.EQ.4) FANWV4=.TRUE.
                          IF(FANWAV.EQ.5) FANWV5=.TRUE.
                          IF(FANWAV.EQ.6) FANWV6=.TRUE.
                          IF(FANWAV.EQ.7) FANWV7=.TRUE.
                          IF(FANWAV.EQ.8) FANWV8=.TRUE.
                          IF(FANWAV.EQ.9) FANWV9=.TRUE.
                          IF(FANWAV.EQ.10) FANWV10=.TRUE.
                      ELSE
                          WW3=DBLE(JK_WAV(K))
                          IF(JK_WAV(K).EQ.1) WWVN=46
                          IF(JK_WAV(K).EQ.2) WWVN=47
                          IF(JK_WAV(K).EQ.3) WWVN=48
                          IF(JK_WAV(K).EQ.4) WWVN=49
                          IF(JK_WAV(K).EQ.5) WWVN=50
                          IF(JK_WAV(K).EQ.6) WWVN=71
                          IF(JK_WAV(K).EQ.7) WWVN=72
                          IF(JK_WAV(K).EQ.8) WWVN=73
                          IF(JK_WAV(K).EQ.9) WWVN=74
                          IF(JK_WAV(K).EQ.10) WWVN=75
                      END IF
                      JK_WVN=INT(WW3)
                  END IF
              ELSE
C     CD FANS, KKK IS ALWAYS 1
                  WW3=DBLE(REFWV)
                  JK_WVN=INT(WW3)
              END IF
C
              DO I=1,FANNUM
                  KFNN=I
C     TRACE THE GUT RAY, IF OK KEEP GOING, IF NOT RETURN
C     WITH ABORT SET TO TRUE AND PRINT ERROR MESSAGE
C
                  SAVE_KDP(1)=SAVEINPT(1)
C     RESET THE INPUT WORDS AND CALL FFOB
                  WC='FOB     '
                  WQ='        '
                  SQ=0
                  S1=1
                  S2=1
                  S3=1
                  S4=1
                  S5=0
                  DF1=0
                  DF2=0
                  DF3=0
                  DF4=0
                  DF5=1
                  IF(I.EQ.1) THEN
                      W1=YFOB1
                      W2=XFOB1
                      W3=0.0D0
                      W4=DBLE(REFWV)
                      W5=0.0D0
                  END IF
                  IF(I.EQ.2) THEN
                      W1=YFOB2
                      W2=XFOB2
                      W3=0.0D0
                      W4=DBLE(REFWV)
                      W5=0.0D0
                  END IF
                  IF(I.EQ.3) THEN
                      W1=YFOB3
                      W2=XFOB3
                      W3=0.0D0
                      W4=DBLE(REFWV)
                      W5=0.0D0
                  END IF
C     SAVE THE CURRENT OBJ,REF AND IMAGE SURFACE NUMBERS
                  CALL RESSUR
C     SET VALUES FOR THE FANS
                  NEWOBJ=FANNOB
                  NEWREF=FANNRF
                  NEWIMG=FANNIM
                  CALL FFOB
                  JA=COS_A_ANG
                  JB=COS_B_ANG
                  IF(INT(LFOB(4)).EQ.1) WWRF=46
                  IF(INT(LFOB(4)).EQ.2) WWRF=47
                  IF(INT(LFOB(4)).EQ.3) WWRF=48
                  IF(INT(LFOB(4)).EQ.4) WWRF=49
                  IF(INT(LFOB(4)).EQ.5) WWRF=50
                  IF(INT(LFOB(4)).EQ.6) WWRF=71
                  IF(INT(LFOB(4)).EQ.7) WWRF=72
                  IF(INT(LFOB(4)).EQ.8) WWRF=73
                  IF(INT(LFOB(4)).EQ.9) WWRF=74
                  IF(INT(LFOB(4)).EQ.10) WWRF=75
C     RESTORE THE SAVED OBJ,REF AND IMAGE SURFACE NUMBERS
                  CALL OLDSUR
C     RESTORE THE OLD COMMAND LINE
                  REST_KDP(1)=RESTINPT(1)
                  IF(.NOT.REFEXT) THEN
                      ABORT=.TRUE.
                      IFA=I
                      RETURN
                  ELSE
                      ABORT=.FALSE.
C     PROCEED WITH FAN
                  END IF
C
C     ALL IS OK WITH THE GUT RAY, TRACE THE FAN.
C       CLEAR APERTURE/OBSCURATION CHECKING IS SET
C       TO "ON" BY SETTING CACOCH=1.
C
                  CACOCH=1
C
C       THE ACTUAL COORDINATES OF THE RAYS TO BE TRACED (AT
C       THE REFERENCE SURFACE) ARE DETERMINED IN RAYTRA. ALL THAT NEEDS
C       BE SENT TO RAYTRA IS THE RELATIVE APERTURE POSITIONS.
C       CIRCULAR PUPIL COORDINATES ARE RESOLVED IN RAYTRA.FOR.
C
C       IF PUPIL IS SET TO RECT, TEMPORARILY RESET IT TO CIRCULAR
C       AS FANS ARE ONLY DEFINED FOR A CIRCULAR PUPIL DEFINITION
C       DUE TO THE PRESENCE OF PFAN AND NFAN. IN THIS TEMPORARY
C       REDEFINITION, WE DON'T CHANGE THE VALUE OF SAX OR A
C       CLAPX ON THE REFERENCE SURFACE, WE JUST DON'T USE IT.
C
C
C     FAN LIMITS ARE -1 TO 1 IN 1/(MAXFAN/2.0D0)
C     STEPS FOR MAXFAN+1 TOTAL POINTS
                  OFFSET=FANOFF
C
C     NOW DO FAN TRACES
C
                  DO ICOUNT=1,2
C
                      DO IX=-INT(DBLE(MAXFAN)/2.0D0),INT(DBLE(MAXFAN)/2.0D0),1
                          XI=DBLE(IX)/(DBLE(MAXFAN)/2.0D0)
C     SET THE OFFSET
                          OFFSET=FANOFF
C
C     DO THE SETUP FOR THE TYPE 5 6 7 AND 8 FANS
C
                          IF(FANTYP.EQ.5.OR.FANTYP.EQ.6) THEN
C       XYFAN OR YXFAN
C       XFAN
C       RELATIVE X COORDINATE IS:
                              XX1=XI
C       RELATIVE Y COORDINATE IS:
                              YY1=OFFSET
C       YFAN
C       RELATIVE X COORDINATE IS:
                              XX2=OFFSET
C       RELATIVE Y COORDINATE IS:
                              YY2=XI
                          END IF
C
C       NOW CALL RAYTRA TO TRACE THE SPECIFIC RAY
                          WWQ='        '
                          IF(ICOUNT.EQ.1) THEN
                              WW1=YY1
                              WW2=XX1
                          END IF
                          IF(ICOUNT.EQ.2) THEN
                              WW1=YY2
                              WW2=XX2
                          END IF
C
                          IF(QALTYP.EQ.0) THEN
C     JUST DY/DX OR DYA/DXA
                              WW3=JK_WVN
                              WVN=WW3
                              RAYCOD(1)=0
                              RAYCOD(2)=-1
                              MSG=.FALSE.
                              WW4=1.0D0
                              NOCOAT=.TRUE.
                              GRASET=.FALSE.
                              DXFSET=.FALSE.
                              CALL RAYTRA
                              MSG=.TRUE.
                              IF(SYSTEM1(30).EQ.1.0D0.OR.SYSTEM1(30).EQ.2.0D0) THEN
C       MODE FOCAL
                                  XXDIF=(RAYRAY(1,NEWIMG)-REFRY(1,NEWIMG))/JB
                                  YYDIF=(RAYRAY(2,NEWIMG)-REFRY(2,NEWIMG))/JA
                                  XXDIF=XXDIF/JB
                                  YYDIF=YYDIF/JA
                                  RRDIF=DSQRT((XXDIF**2)+(YYDIF**2))
                              ELSE
C       MOD AFOCAL
                                  XXDIF=RAYRAY(11,NEWIMG)
     1                            -REFRY(11,NEWIMG)
                                  YYDIF=RAYRAY(12,NEWIMG)
     1                            -REFRY(12,NEWIMG)
                                  IF((XXDIF).GT.(PII)) XXDIF=XXDIF-(TWOPII)
                                  IF((YYDIF).GT.(PII)) YYDIF=YYDIF-(TWOPII)
                                  IF((XXDIF).LT.(-PII)) XXDIF=XXDIF+(TWOPII)
                                  IF((YYDIF).LT.(-PII)) YYDIF=YYDIF+(TWOPII)
                                  IF(ABS(XXDIF).EQ.ABS(TWOPII)) XXDIF=0.0D0
                                  IF(ABS(YYDIF).EQ.ABS(TWOPII)) YYDIF=0.0D0
                                  IF((XXDIF).LT.(-TWOPII)) XXDIF=XXDIF+(TWOPII)
                                  IF((YYDIF).LT.(-TWOPII)) YYDIF=YYDIF+(TWOPII)

                                  COSARG=((RAYRAY(4,NEWIMG)*REFRY(4,NEWIMG))+
     1                            (RAYRAY(5,NEWIMG)*REFRY(5,NEWIMG))+
     1                            (RAYRAY(6,NEWIMG)*REFRY(6,NEWIMG)))
                                  IF(COSARG.LT.0.0D0) COSARG=-COSARG
                                  IF(COSARG.GT.1.0D0) COSARG=1.0D0
                                  RRDIF=DACOS(COSARG)
                                  IF((RRDIF).GT.(PII)) RRDIF=RRDIF-(TWOPII)
                                  IF((RRDIF).LT.(-PII)) RRDIF=RRDIF+(TWOPII)
                                  IF(ABS(RRDIF).EQ.ABS(TWOPII)) RRDIF=0.0D0
                                  IF((RRDIF).LT.(-TWOPII)) RRDIF=RRDIF+(TWOPII)
                              END IF
C
                              IF(FANTYP.EQ.5.OR.FANTYP.EQ.6) THEN
                                  IF(RAYCOD(1).EQ.0.0D0) THEN
                                      IF(DABS(XXDIF).LT.1.0D-7) XXDIF=0.0D0
                                      IF(DABS(YYDIF).LT.1.0D-7) YYDIF=0.0D0
                                      JJJ=IX+INT(DBLE(MAXFAN)/2.0D0)+1
C     JJJ GOES FROM 1 TO MAXFAN+1
                                      IF(FANTYP.EQ.5) THEN
                                          IF(ICOUNT.EQ.1) FFAANN(I,JJJ,1,K)=XXDIF
                                          IF(ICOUNT.EQ.2) FFAANN(I,JJJ,2,K)=YYDIF
                                          FFAANN(I,JJJ,3,K)=0.0D0
                                          FFAANN(I,JJJ,4,K)=0.0D0
                                          IF(ICOUNT.EQ.1) FFAANN(I,JJJ,5,K)=1.0D0
                                          IF(ICOUNT.EQ.2) FFAANN(I,JJJ,6,K)=1.0D0
                                      END IF
                                      IF(FANTYP.EQ.6) THEN
                                          IF(ICOUNT.EQ.1) FFAANN(I,JJJ,1,K)=YYDIF
                                          IF(ICOUNT.EQ.2) FFAANN(I,JJJ,2,K)=XXDIF
                                          FFAANN(I,JJJ,3,K)=0.0D0
                                          FFAANN(I,JJJ,4,K)=0.0D0
                                          IF(ICOUNT.EQ.1) FFAANN(I,JJJ,5,K)=1.0D0
                                          IF(ICOUNT.EQ.2) FFAANN(I,JJJ,6,K)=1.0D0
                                      END IF
                                  ELSE
C     RAY FAILED
                                      JJJ=IX+INT(DBLE(MAXFAN)/2.0D0)+1
C     JJJ GOES FROM 1 TO MAXFAN+1
                                      IF(ICOUNT.EQ.1) FFAANN(I,JJJ,1,K)=0.0D0
                                      IF(ICOUNT.EQ.2) FFAANN(I,JJJ,2,K)=0.0D0
                                      FFAANN(I,JJJ,3,K)=0.0D0
                                      FFAANN(I,JJJ,4,K)=0.0D0
                                      IF(ICOUNT.EQ.1) FFAANN(I,JJJ,5,K)=0.0D0
                                      IF(ICOUNT.EQ.2) FFAANN(I,JJJ,6,K)=0.0D0
                                  END IF
                              ELSE
                              END IF
                          ELSE
C       QALTYP NOT 0
                          END IF
C
                          IF(QALTYP.EQ.1) THEN
C       OPD
                              WW3=JK_WVN
                              WVN=WW3
                              RAYCOD(1)=0
                              RAYCOD(2)=-1
                              MSG=.FALSE.
                              WW4=1.0D0
                              NOCOAT=.TRUE.
                              GRASET=.FALSE.
                              DXFSET=.FALSE.
                              CALL RAYTRA
                              MSG=.TRUE.
                              IF(RAYEXT) THEN
                                  OOPD=0.0D0
                                  RCOR=0.0D0
                                  OCOR=0.0D0
                                  IF(DABS(ALENS(3,NEWOBJ)).GE.1.0D10) JJ=NEWOBJ+2
                                  IF(DABS(ALENS(3,NEWOBJ)).LT.1.0D10) JJ=NEWOBJ+1
                                  DO J=JJ,NEWIMG
                                      OOPD=OOPD+RAYRAY(7,J)
     1                                -(REFRY(7,J)*(ALENS(WWVN,J-1)/ALENS(WWRF,J-1)))
                                  END DO
                                  IF(SYSTEM1(30).EQ.1.0D0.OR.SYSTEM1(30).EQ.2.0D0) THEN
C       MODE FOCAL
C               RCOR=0.0D0
C               OCOR=0.0D0
                                      CALL FOPD
C       CALCULATE THEN APPLY ADJUSTMENT FOR THE BEGINNING AND ENDING
C       REFERENCE SPHERES.
                                      OOPD=OOPD-(OCOR*ALENS(WWVN,NEWOBJ))
     1                                +(RCOR*ALENS(WWVN,NEWOBJ))
                                      RCOR=0.0D0
                                      OCOR=0.0D0
                                      CENCEN=.FALSE.
                                      CALL LOPD
                                      OOPD=OOPD-(OCOR*ALENS(WWVN,NEWIMG-1))+
     1                                (RCOR*ALENS(WWVN,NEWIMG-1))
                                  ELSE
C       MODE AFOCAL
C               RCOR=0.0D0
C               OCOR=0.0D0
                                      CALL FOPD
C       CALCULATE THEN APPLY ADJUSTMENT FOR THE BEGINNING AND ENDING
C       REFERENCE SPHERES.
                                      OOPD=OOPD-(OCOR*ALENS(WWVN,NEWOBJ))
     1                                +(RCOR*ALENS(WWVN,NEWOBJ))
                                      RCOR=0.0D0
                                      OCOR=0.0D0
                                      CENCEN=.FALSE.
                                      CALL LOPD
                                      OOPD=OOPD-(OCOR*ALENS(WWVN,NEWIMG-1))
     1                                +(RCOR*ALENS(WWVN,NEWIMG-1))
                                  END IF
                                  IF(SYSTEM1(6).EQ.1.0D0) WAV=SYSTEM1(INT(WW3))*
     1                            ((1.0D-3)/(25.4D0))
                                  IF(SYSTEM1(6).EQ.2.0D0) WAV=SYSTEM1(INT(WW3))*(1.0D-4)
                                  IF(SYSTEM1(6).EQ.3.0D0) WAV=SYSTEM1(INT(WW3))*(1.0D-3)
                                  IF(SYSTEM1(6).EQ.4.0D0) WAV=SYSTEM1(INT(WW3))*(1.0D-6)
                                  OOPD=-OOPD
                                  IF(REVSTR) OOPD=-OOPD
                                  OPDW=OOPD/WAV
                              ELSE
C     RAY FAILED
                                  OOPD=0.0D0
                                  OPDW=0.0D0
                              END IF
C
                              IF(RAYCOD(1).EQ.0.0D0) THEN
C       IF OPD IN WAVES IS LESS THAN 0.0001 WAVES, SET OPDS TO ZERO
                                  IF(DABS(OPDW).LT.1.0D-7) THEN
                                      OOPD=0.0D0
                                      OPDW=0.0D0
                                  ELSE
C       NOT ZERO
                                  END IF
                                  JJJ=IX+INT(DBLE(MAXFAN)/2.0D0)+1
                                  IF(ICOUNT.EQ.1) FFAANN(I,JJJ,1,K)=OPDW
                                  IF(ICOUNT.EQ.2) FFAANN(I,JJJ,2,K)=OPDW
                                  FFAANN(I,JJJ,3,K)=0.0D0
                                  FFAANN(I,JJJ,4,K)=0.0D0
                                  IF(ICOUNT.EQ.1) FFAANN(I,JJJ,5,K)=1.0D0
                                  IF(ICOUNT.EQ.2) FFAANN(I,JJJ,6,K)=1.0D0
                              ELSE
                                  JJJ=IX+INT(DBLE(MAXFAN)/2.0D0)+1
                                  IF(ICOUNT.EQ.1) FFAANN(I,JJJ,1,K)=0.0D0
                                  IF(ICOUNT.EQ.2) FFAANN(I,JJJ,2,K)=0.0D0
                                  FFAANN(I,JJJ,3,K)=0.0D0
                                  FFAANN(I,JJJ,4,K)=0.0D0
                                  IF(ICOUNT.EQ.1) FFAANN(I,JJJ,5,K)=0.0D0
                                  IF(ICOUNT.EQ.2) FFAANN(I,JJJ,6,K)=0.0D0
                              END IF
                          ELSE
C       NOT (OPD)
                          END IF
C
                          IF(QALTYP.EQ.2) THEN
C       CD
                              RAYCOD(1)=0
                              RAYCOD(2)=-1
                              WW3=SYSTEM1(7)
                              WVN=WW3
                              MSG=.FALSE.
                              WW4=1.0D0
                              NOCOAT=.TRUE.
                              GRASET=.FALSE.
                              DXFSET=.FALSE.
                              CALL RAYTRA
                              MSG=.TRUE.
                              IF(SYSTEM1(30).EQ.1.0D0.OR.SYSTEM1(30).EQ.2.0D0) THEN
C       MODE FOCAL
                                  PW11=RAYRAY(1,NEWIMG)
                                  PW12=RAYRAY(2,NEWIMG)
                              ELSE
C       MODE AFOCAL
                                  PW11=RAYRAY(11,NEWIMG)
                                  PW12=RAYRAY(12,NEWIMG)
                              END IF
                              WW3=SYSTEM1(8)
                              WVN=WW3
                              MSG=.FALSE.
                              WW4=1.0D0
                              NOCOAT=.TRUE.
                              GRASET=.FALSE.
                              DXFSET=.FALSE.
                              CALL RAYTRA
                              MSG=.TRUE.
                              IF(SYSTEM1(30).EQ.1.0D0.OR.SYSTEM1(30).EQ.2.0D0) THEN
C       MODE FOCAL
                                  PW21=RAYRAY(1,NEWIMG)
                                  PW22=RAYRAY(2,NEWIMG)
                              ELSE
C       MODE AFOCAL
                                  PW21=RAYRAY(11,NEWIMG)
                                  PW22=RAYRAY(12,NEWIMG)
                              END IF
                              WW3=SYSTEM1(9)
                              WVN=WW3
                              MSG=.FALSE.
                              WW4=1.0D0
                              NOCOAT=.TRUE.
                              GRASET=.FALSE.
                              DXFSET=.FALSE.
                              CALL RAYTRA
                              MSG=.TRUE.
                              IF(SYSTEM1(30).EQ.1.0D0.OR.SYSTEM1(30).EQ.2.0D0) THEN
C       MODE FOCAL
                                  SW11=RAYRAY(1,NEWIMG)
                                  SW12=RAYRAY(2,NEWIMG)
                              ELSE
C       MODE AFOCAL
                                  SW11=RAYRAY(11,NEWIMG)
                                  SW12=RAYRAY(12,NEWIMG)
                              END IF
                              WW3=SYSTEM1(10)
                              WVN=WW3
                              MSG=.FALSE.
                              WW4=1.0D0
                              NOCOAT=.TRUE.
                              GRASET=.FALSE.
                              DXFSET=.FALSE.
                              CALL RAYTRA
                              MSG=.TRUE.
                              IF(SYSTEM1(30).EQ.1.0D0.OR.SYSTEM1(30).EQ.2.0D0) THEN
C       MODE FOCAL
                                  SW21=RAYRAY(1,NEWIMG)
                                  SW22=RAYRAY(2,NEWIMG)
                              ELSE
C       MODE AFOCAL
                                  SW21=RAYRAY(11,NEWIMG)
                                  SW22=RAYRAY(12,NEWIMG)
                              END IF
C       PRIMARY PAIR
C       X-VALUE
                              DIF1=PW11-PW21
                              IF(SYSTEM1(30).GT.2.0D0) THEN
C     AFOCAL
                                  IF((DIF1).GT.(PII)) DIF1=DIF1-(TWOPII)
                                  IF((DIF1).LT.(-PII)) DIF1=DIF1+(TWOPII)
                                  IF(ABS(DIF1).EQ.ABS(TWOPII)) DIF1=0.0D0
                                  IF((DIF1).LT.(-TWOPII)) DIF1=DIF1+(TWOPII)
                              END IF
C       Y-VALUE
                              DIF2=PW12-PW22
                              IF(SYSTEM1(30).GT.2.0D0) THEN
C     AFOCAL
                                  IF((DIF2).GT.(PII)) DIF2=DIF2-(TWOPII)
                                  IF((DIF2).LT.(-PII)) DIF2=DIF2+(TWOPII)
                                  IF(ABS(DIF2).EQ.ABS(TWOPII)) DIF2=0.0D0
                                  IF((DIF2).LT.(-TWOPII)) DIF2=DIF2+(TWOPII)
                              END IF
C       SECONDARY PAIR
C       X-VALUE
                              DIF3=SW11-SW21
                              IF(SYSTEM1(30).GT.2.0D0) THEN
C     AFOCAL
                                  IF((DIF3).GT.(PII)) DIF3=DIF3-(TWOPII)
                                  IF((DIF3).LT.(-PII)) DIF3=DIF3+(TWOPII)
                                  IF(ABS(DIF3).EQ.ABS(TWOPII)) DIF3=0.0D0
                                  IF((DIF3).LT.(-TWOPII)) DIF3=DIF3+(TWOPII)
                              END IF
C       Y-VALUE
                              DIF4=SW12-SW22
                              IF(SYSTEM1(30).GT.2.0D0) THEN
C     AFOCAL
                                  IF((DIF4).GT.(PII)) DIF4=DIF4-(TWOPII)
                                  IF((DIF4).LT.(-PII)) DIF4=DIF4+(TWOPII)
                                  IF(ABS(DIF4).EQ.ABS(TWOPII)) DIF4=0.0D0
                                  IF((DIF4).LT.(-TWOPII)) DIF4=DIF4+(TWOPII)
                              END IF
                              IF(FANTYP.EQ.5.OR.FANTYP.EQ.6) THEN
C     X OR Y FAN
                                  IF(FANTYP.EQ.5) THEN
                                      IF(RAYCOD(1).EQ.0.0D0) THEN
                                          JJJ=IX+INT(DBLE(MAXFAN)/2.0D0)+1
                                          IF(ICOUNT.EQ.1) FFAANN(I,JJJ,1,K)=DIF1
                                          IF(ICOUNT.EQ.2) FFAANN(I,JJJ,2,K)=DIF2
                                          IF(ICOUNT.EQ.1) FFAANN(I,JJJ,3,K)=DIF3
                                          IF(ICOUNT.EQ.2) FFAANN(I,JJJ,4,K)=DIF4
                                          IF(ICOUNT.EQ.1) FFAANN(I,JJJ,5,K)=1.0D0
                                          IF(ICOUNT.EQ.2) FFAANN(I,JJJ,6,K)=1.0D0
                                      ELSE
                                          JJJ=IX+INT(DBLE(MAXFAN)/2.0D0)+1
                                          IF(ICOUNT.EQ.1) FFAANN(I,JJJ,1,K)=0.0D0
                                          IF(ICOUNT.EQ.2) FFAANN(I,JJJ,2,K)=0.0D0
                                          IF(ICOUNT.EQ.1) FFAANN(I,JJJ,3,K)=0.0D0
                                          IF(ICOUNT.EQ.2) FFAANN(I,JJJ,4,K)=0.0D0
                                          IF(ICOUNT.EQ.1) FFAANN(I,JJJ,5,K)=0.0D0
                                          IF(ICOUNT.EQ.2) FFAANN(I,JJJ,6,K)=0.0D0
                                      END IF
                                  END IF
                                  IF(FANTYP.EQ.6) THEN
                                      IF(RAYCOD(1).EQ.0.0D0) THEN
                                          JJJ=IX+INT(DBLE(MAXFAN)/2.0D0)+1
                                          IF(ICOUNT.EQ.1) FFAANN(I,JJJ,1,K)=DIF2
                                          IF(ICOUNT.EQ.2) FFAANN(I,JJJ,2,K)=DIF1
                                          IF(ICOUNT.EQ.1) FFAANN(I,JJJ,3,K)=DIF4
                                          IF(ICOUNT.EQ.2) FFAANN(I,JJJ,4,K)=DIF3
                                          IF(ICOUNT.EQ.1) FFAANN(I,JJJ,5,K)=1.0D0
                                          IF(ICOUNT.EQ.2) FFAANN(I,JJJ,6,K)=1.0D0
                                      ELSE
                                          JJJ=IX+INT(DBLE(MAXFAN)/2.0D0)+1
                                          IF(ICOUNT.EQ.1) FFAANN(I,JJJ,1,K)=0.0D0
                                          IF(ICOUNT.EQ.2) FFAANN(I,JJJ,2,K)=0.0D0
                                          IF(ICOUNT.EQ.1) FFAANN(I,JJJ,3,K)=0.0D0
                                          IF(ICOUNT.EQ.2) FFAANN(I,JJJ,4,K)=0.0D0
                                          IF(ICOUNT.EQ.1) FFAANN(I,JJJ,5,K)=0.0D0
                                          IF(ICOUNT.EQ.2) FFAANN(I,JJJ,6,K)=0.0D0
                                      END IF
                                  END IF
                              ELSE
                              END IF
                          ELSE
C       NOT CD
                          END IF
C
                          IF(QALTYP.EQ.3) THEN
                              WW3=JK_WVN
                              WVN=WW3
                              RAYCOD(1)=0
                              RAYCOD(2)=-1
                              IF(ICOUNT.EQ.1) THEN
C     XFAN
                                  IF(REAL(WW2).LT.0.001.AND.WW2.GT.0.0D0)WW2=0.001D0
                                  IF(REAL(WW2).GT.-0.001.AND.WW2.LT.0.0D0)WW2=-0.001D0
                                  IF(REAL(WW2).EQ.0.0) WW2=.0001D0
                              END IF
                              IF(ICOUNT.EQ.2) THEN
C     YFAN
                                  IF(REAL(WW1).LT.0.001.AND.WW1.GT.0.0D0)WW1=0.001D0
                                  IF(REAL(WW1).GT.-0.001.AND.WW1.LT.0.0D0)WW1=-0.001D0
                                  IF(REAL(WW1).EQ.0.0) WW1=.0001D0
                              END IF
                              MSG=.FALSE.
                              WW4=1.0D0
                              NOCOAT=.TRUE.
                              GRASET=.FALSE.
                              DXFSET=.FALSE.
                              CALL RAYTRA
                              MSG=.TRUE.
C       CALCULATION OF LA IN XZ AND YZ PLANE.
                              DX=(RAYRAY(1,NEWIMG)-REFRY(1,NEWIMG))/JB
                              DY=(RAYRAY(2,NEWIMG)-REFRY(2,NEWIMG))/JA
                              LLR=RAYRAY(4,NEWIMG)
                              MMR=RAYRAY(5,NEWIMG)
                              NNR=RAYRAY(6,NEWIMG)
                              LLP=REFRY(4,NEWIMG)
                              MMP=REFRY(5,NEWIMG)
                              NNP=REFRY(6,NEWIMG)
                              IF(FANTYP.EQ.5.OR.FANTYP.EQ.6) THEN
                                  DTX=((LLR/NNR)-(LLP/NNP))
                                  DTY=((MMR/NNR)-(MMP/NNP))
                              END IF
                              IF(DX.EQ.0.0D0.OR.DTX.EQ.0.0D0) LAX=0.0D0
                              IF(DX.NE.0.0D0.AND.DTX.NE.0.0D0) LAX=DX/DTX
                              IF(DY.EQ.0.0D0.OR.DTY.EQ.0.0D0) LAY=0.0D0
                              IF(DY.NE.0.0D0.AND.DTY.NE.0.0D0) LAY=DY/DTY
                              IF(FANTYP.EQ.5) THEN
                                  IF(RAYCOD(1).EQ.0.0D0) THEN
                                      JJJ=IX+INT(DBLE(MAXFAN)/2.0D0)+1
                                      IF(ICOUNT.EQ.1) FFAANN(I,JJJ,1,K)=LAX
                                      IF(ICOUNT.EQ.2) FFAANN(I,JJJ,2,K)=LAY
                                      FFAANN(I,JJJ,3,K)=0.0D0
                                      FFAANN(I,JJJ,4,K)=0.0D0
                                      IF(ICOUNT.EQ.1) FFAANN(I,JJJ,5,K)=1.0D0
                                      IF(ICOUNT.EQ.2) FFAANN(I,JJJ,6,K)=1.0D0
                                  ELSE
                                      JJJ=IX+INT(DBLE(MAXFAN)/2.0D0)+1
                                      IF(ICOUNT.EQ.1) FFAANN(I,JJJ,1,K)=0.0D0
                                      IF(ICOUNT.EQ.2) FFAANN(I,JJJ,2,K)=0.0D0
                                      FFAANN(I,JJJ,3,K)=0.0D0
                                      FFAANN(I,JJJ,4,K)=0.0D0
                                      IF(ICOUNT.EQ.1) FFAANN(I,JJJ,5,K)=0.0D0
                                      IF(ICOUNT.EQ.2) FFAANN(I,JJJ,6,K)=0.0D0
                                  END IF
                              END IF
                              IF(FANTYP.EQ.6) THEN
                                  IF(RAYCOD(1).EQ.0.0D0) THEN
                                      JJJ=IX+INT(DBLE(MAXFAN)/2.0D0)+1
                                      IF(ICOUNT.EQ.1) FFAANN(I,JJJ,1,K)=LAX
                                      IF(ICOUNT.EQ.2) FFAANN(I,JJJ,2,K)=LAY
                                      FFAANN(I,JJJ,3,K)=0.0D0
                                      FFAANN(I,JJJ,4,K)=0.0D0
                                      IF(ICOUNT.EQ.1) FFAANN(I,JJJ,5,K)=1.0D0
                                      IF(ICOUNT.EQ.2) FFAANN(I,JJJ,6,K)=1.0D0
                                  ELSE
                                      JJJ=IX+INT(DBLE(MAXFAN)/2.0D0)+1
                                      IF(ICOUNT.EQ.1) FFAANN(I,JJJ,1,K)=0.0D0
                                      IF(ICOUNT.EQ.2) FFAANN(I,JJJ,2,K)=0.0D0
                                      FFAANN(I,JJJ,3,K)=0.0D0
                                      FFAANN(I,JJJ,4,K)=0.0D0
                                      IF(ICOUNT.EQ.1) FFAANN(I,JJJ,5,K)=0.0D0
                                      IF(ICOUNT.EQ.2) FFAANN(I,JJJ,6,K)=0.0D0
                                  END IF
                              END IF
C
                          ELSE
C       NOT LA
                          END IF
C
C     THIS IS THE END OF THE FAN LOOP
                      END DO
C     THIS IS THE END OF THE LOOP THAT TRACKS THE LEFT OR RIGHT FAN
C     BY THE ICOUNT VARIABLE
                  END DO
C
C     THIS IS THE END OF THE FIELD OF VIEW LOOP
              END DO
C     THIS IS THE END OF THE WAVELENGTH LOOP
99999         CONTINUE
          END DO
          RETURN
      END


C SUB PLFAN1.FOR
      SUBROUTINE PLFAN1(ABORT,IFA)
C
          IMPLICIT NONE
C
C     DOES FAN CALCULATIONS FOR FAN PLOTTING (FANTYP 1 2 3 4)
C
          INTEGER J,JJ,IX,I,JJJ,K,KKK,IFA
     1    ,KFNN,FANWAV,JK_WVN,WWRF
C
          INTEGER JK_WAV(1:10)
C
          COMMON/WAVER/JK_WAV
C
          REAL*8 COSARG,XTEMP,YTEMP,TEMP1,TEMP2,TEMP3,TEMP4,
     1    OFFSET,XI,XX1,YY1,LLR,MMR,NNR,LLP,MMP,NNP,
     2    XXDIF,YYDIF,OOPD,OPDW,
     3    WAV,RRDIF,DIF1,DIF2,DIF3,DIF4,PW11,PW12,PW21,PW22,
     4    SW11,SW12,SW21,SW22,LAX,LAY,DX,DY,DTY,DTX,
     5    JA,JB
C
          LOGICAL FOBB0,FANEXT
     1    ,FOBB0X,FOBB0Y,ABORT
C
          COMMON/FANEXI/FANEXT
C
          COMMON/FANNER/FANWAV
C
          COMMON/FFOOBB/FOBB0,FOBB0X,FOBB0Y
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          INTEGER WWVN
          IF(INT(LFOB(4)).EQ.1) WWRF=46
          IF(INT(LFOB(4)).EQ.2) WWRF=47
          IF(INT(LFOB(4)).EQ.3) WWRF=48
          IF(INT(LFOB(4)).EQ.4) WWRF=49
          IF(INT(LFOB(4)).EQ.5) WWRF=50
          IF(INT(LFOB(4)).EQ.6) WWRF=71
          IF(INT(LFOB(4)).EQ.7) WWRF=72
          IF(INT(LFOB(4)).EQ.8) WWRF=73
          IF(INT(LFOB(4)).EQ.9) WWRF=74
          IF(INT(LFOB(4)).EQ.10) WWRF=75
          IF(FANEXT) THEN
              IF(FANWAV.EQ.1) WWVN=46
              IF(FANWAV.EQ.2) WWVN=47
              IF(FANWAV.EQ.3) WWVN=48
              IF(FANWAV.EQ.4) WWVN=49
              IF(FANWAV.EQ.5) WWVN=50
              IF(FANWAV.EQ.6) WWVN=71
              IF(FANWAV.EQ.7) WWVN=72
              IF(FANWAV.EQ.8) WWVN=73
              IF(FANWAV.EQ.9) WWVN=74
              IF(FANWAV.EQ.10) WWVN=75
          END IF
C
          IF(QALTYP.EQ.0) KKK=10
          IF(QALTYP.EQ.1) KKK=10
          IF(QALTYP.EQ.2) KKK=1
          IF(QALTYP.EQ.3) KKK=10
          IF(FANEXT) KKK=1
          IF(FANEXT) K=1
          DO K=1,KKK
              IF(QALTYP.NE.2) THEN
C     NOT CD FANS
                  IF(K.EQ.1.AND.JK_WAV(1).EQ.0.OR.
     1            K.EQ.2.AND.JK_WAV(2).EQ.0.OR.
     1            K.EQ.3.AND.JK_WAV(3).EQ.0.OR.
     1            K.EQ.4.AND.JK_WAV(4).EQ.0.OR.
     1            K.EQ.5.AND.JK_WAV(5).EQ.0.OR.
     1            K.EQ.6.AND.JK_WAV(6).EQ.0.OR.
     1            K.EQ.7.AND.JK_WAV(7).EQ.0.OR.
     1            K.EQ.8.AND.JK_WAV(8).EQ.0.OR.
     1            K.EQ.9.AND.JK_WAV(9).EQ.0.OR.
     1            K.EQ.10.AND.JK_WAV(10).EQ.0) THEN
                      DO I=1,3
                          DO JJJ=1,21
                              FFAANN(I,JJJ,1,K)=0.0D0
                              FFAANN(I,JJJ,2,K)=0.0D0
                              FFAANN(I,JJJ,3,K)=0.0D0
                              FFAANN(I,JJJ,4,K)=0.0D0
                              FFAANN(I,JJJ,5,K)=1.0D0
                              FFAANN(I,JJJ,6,K)=1.0D0
                          END DO
                      END DO
C     SPECTRAL WEIGHT IS ZERO, SKIP THE CALCULATIONS
                      GO TO 99999
                  ELSE
C     WE WANT TO TRACE RAYS
C     SET WW3
                      IF(FANEXT) THEN
                          WW3=DBLE(JK_WAV(1))
                          IF(FANWAV.EQ.1) FANWV1=.TRUE.
                          IF(FANWAV.EQ.2) FANWV2=.TRUE.
                          IF(FANWAV.EQ.3) FANWV3=.TRUE.
                          IF(FANWAV.EQ.4) FANWV4=.TRUE.
                          IF(FANWAV.EQ.5) FANWV5=.TRUE.
                          IF(FANWAV.EQ.6) FANWV6=.TRUE.
                          IF(FANWAV.EQ.7) FANWV7=.TRUE.
                          IF(FANWAV.EQ.8) FANWV8=.TRUE.
                          IF(FANWAV.EQ.9) FANWV9=.TRUE.
                          IF(FANWAV.EQ.10) FANWV10=.TRUE.
                      ELSE
                          WW3=DBLE(JK_WAV(K))
                          IF(JK_WAV(K).EQ.1) WWVN=46
                          IF(JK_WAV(K).EQ.2) WWVN=47
                          IF(JK_WAV(K).EQ.3) WWVN=48
                          IF(JK_WAV(K).EQ.4) WWVN=49
                          IF(JK_WAV(K).EQ.5) WWVN=50
                          IF(JK_WAV(K).EQ.6) WWVN=71
                          IF(JK_WAV(K).EQ.7) WWVN=72
                          IF(JK_WAV(K).EQ.8) WWVN=73
                          IF(JK_WAV(K).EQ.9) WWVN=74
                          IF(JK_WAV(K).EQ.10) WWVN=75
                      END IF
                      JK_WVN=INT(WW3)
                  END IF
              ELSE
C     CD FANS, KKK IS ALWAYS 1
                  WW3=DBLE(REFWV)
                  JK_WVN=INT(WW3)
              END IF
C
              DO I=1,FANNUM
                  KFNN=I
C     TRACE THE GUT RAY, IF OK KEEP GOING, IF NOT RETURN
C     WITH ABORT SET TO TRUE AND PRINT ERROR MESSAGE
C
                  SAVE_KDP(1)=SAVEINPT(1)
C     RESET THE INPUT WORDS AND CALL FFOB
                  WC='FOB'
                  WQ='        '
                  SQ=0
                  S1=1
                  S2=1
                  S3=1
                  S4=1
                  S5=0
                  DF1=0
                  DF2=0
                  DF3=0
                  DF4=0
                  DF5=1
                  IF(I.EQ.1) THEN
                      W1=YFOB1
                      W2=XFOB1
                      W3=0.0D0
                      W4=DBLE(REFWV)
                      W5=0.0D0
                  ELSE
                  END IF
                  IF(I.EQ.2) THEN
                      W1=YFOB2
                      W2=XFOB2
                      W3=0.0D0
                      W4=DBLE(REFWV)
                      W5=0.0D0
                  ELSE
                  END IF
                  IF(I.EQ.3) THEN
                      W1=YFOB3
                      W2=XFOB3
                      W3=0.0D0
                      W4=DBLE(REFWV)
                      W5=0.0D0
                  ELSE
                  END IF
C     SAVE THE CURRENT OBJ,REF AND IMAGE SURFACE NUMBERS
                  CALL RESSUR
C     SET VALUES FOR THE FANS
                  NEWOBJ=FANNOB
                  NEWREF=FANNRF
                  NEWIMG=FANNIM
                  CALL FFOB
                  JA=COS_A_ANG
                  JB=COS_B_ANG
                  IF(INT(LFOB(4)).EQ.1) WWRF=46
                  IF(INT(LFOB(4)).EQ.2) WWRF=47
                  IF(INT(LFOB(4)).EQ.3) WWRF=48
                  IF(INT(LFOB(4)).EQ.4) WWRF=49
                  IF(INT(LFOB(4)).EQ.5) WWRF=50
                  IF(INT(LFOB(4)).EQ.6) WWRF=71
                  IF(INT(LFOB(4)).EQ.7) WWRF=72
                  IF(INT(LFOB(4)).EQ.8) WWRF=73
                  IF(INT(LFOB(4)).EQ.9) WWRF=74
                  IF(INT(LFOB(4)).EQ.10) WWRF=75
C     RESTORE THE SAVED OBJ,REF AND IMAGE SURFACE NUMBERS
                  CALL OLDSUR
C     RESTORE THE OLD COMMAND LINE
                  REST_KDP(1)=RESTINPT(1)
                  IF(.NOT.REFEXT) THEN
                      ABORT=.TRUE.
                      IFA=I
                      RETURN
                  ELSE
                      ABORT=.FALSE.
C     PROCEED WITH FAN
                  END IF
C
C     ALL IS OK WITH THE GUT RAY, TRACE THE FAN.
C       CLEAR APERTURE/OBSCURATION CHECKING IS SET
C       TO "ON" BY SETTING CACOCH=1.
C
                  CACOCH=1
C
C       THE ACTUAL COORDINATES OF THE RAYS TO BE TRACED (AT
C       THE REFERENCE SURFACE) ARE DETERMINED IN RAYTRA. ALL THAT NEEDS
C       BE SENT TO RAYTRA IS THE RELATIVE APERTURE POSITIONS.
C       CIRCULAR PUPIL COORDINATES ARE RESOLVED IN RAYTRA.FOR.
C
C       IF PUPIL IS SET TO RECT, TEMPORARILY RESET IT TO CIRCULAR
C       AS FANS ARE ONLY DEFINED FOR A CIRCULAR PUPIL DEFINITION
C       DUE TO THE PRESENCE OF PFAN AND NFAN. IN THIS TEMPORARY
C       REDEFINITION, WE DON'T CHANGE THE VALUE OF SAX OR A
C       CLAPX ON THE REFERENCE SURFACE, WE JUST DON'T USE IT.
C
C
C     FAN LIMITS ARE -1 TO 1 IN 1/(MAXFAN/2.0D0)
C     STEPS FOR MAXFAN+1 TOTAL POINTS
                  OFFSET=FANOFF
C
C     NOW DO FAN TRACES
C
                  DO IX=-INT(DBLE(MAXFAN)/2.0D0),INT(DBLE(MAXFAN)/2.0D0),1
                      XI=DBLE(IX)/(DBLE(MAXFAN)/2.0D0)
C     SET THE OFFSET
                      OFFSET=FANOFF
C
C     DO THE SETUP FOR THE EIGHT TYPES OF FANS
C
                      IF(FANTYP.EQ.1) THEN
C       YFAN
C       RELATIVE X COORDINATE IS:
                          XX1=OFFSET
C       RELATIVE Y COORDINATE IS:
                          YY1=XI
                      END IF
                      IF(FANTYP.EQ.2) THEN
C       XFAN
C       RELATIVE X COORDINATE IS:
                          XX1=XI
C       RELATIVE Y COORDINATE IS:
                          YY1=OFFSET
                      END IF
                      IF(FANTYP.EQ.3) THEN
C       NFAN
C       RELATIVE X COORDINATE IS:
                          XX1=(XI-OFFSET)/(DSQRT(2.0D0))
C       RELATIVE Y COORDINATE IS:
                          YY1=(XI+OFFSET)/(DSQRT(2.0D0))
                      END IF
                      IF(FANTYP.EQ.4) THEN
C       PFAN
C       RELATIVE X COORDINATE IS:
                          XX1=(XI+OFFSET)/(DSQRT(2.0D0))
C       RELATIVE Y COORDINATE IS:
                          YY1=-(XI-OFFSET)/(DSQRT(2.0D0))
                      END IF
C
C       NOW CALL RAYTRA TO TRACE THE SPECIFIC RAY
                      WWQ='        '
                      WW1=YY1
                      WW2=XX1
C
                      IF(QALTYP.EQ.0) THEN
C     JUST DY/DX OR DYA/DXA
                          WW3=JK_WVN
                          WVN=WW3
                          RAYCOD(1)=0
                          RAYCOD(2)=-1
                          MSG=.FALSE.
                          F58=1
                          NOCOAT=.TRUE.
                          GRASET=.FALSE.
                          DXFSET=.FALSE.
                          CALL RAYTRA
                          F58=0
                          MSG=.TRUE.
                          IF(SYSTEM1(30).EQ.1.0D0.OR.SYSTEM1(30).EQ.2.0D0) THEN
C       MODE FOCAL
                              XXDIF=(RAYRAY(1,NEWIMG)-REFRY(1,NEWIMG))/JB
                              YYDIF=(RAYRAY(2,NEWIMG)-REFRY(2,NEWIMG))/JA
                              RRDIF=DSQRT((XXDIF**2)+(YYDIF**2))
                          ELSE
C       MOD AFOCAL
                              XXDIF=RAYRAY(11,NEWIMG)
     1                        -REFRY(11,NEWIMG)
                              YYDIF=RAYRAY(12,NEWIMG)
     1                        -REFRY(12,NEWIMG)
                              IF((XXDIF).GT.(PII)) XXDIF=XXDIF-(TWOPII)
                              IF((YYDIF).GT.(PII)) YYDIF=YYDIF-(TWOPII)
                              IF((XXDIF).LT.(-PII)) XXDIF=XXDIF+(TWOPII)
                              IF((YYDIF).LT.(-PII)) YYDIF=YYDIF+(TWOPII)
                              IF(ABS(XXDIF).EQ.ABS(TWOPII)) XXDIF=0.0D0
                              IF(ABS(YYDIF).EQ.ABS(TWOPII)) YYDIF=0.0D0
                              IF((XXDIF).LT.(-TWOPII)) XXDIF=XXDIF+(TWOPII)
                              IF((YYDIF).LT.(-TWOPII)) YYDIF=YYDIF+(TWOPII)

                              COSARG=((RAYRAY(4,NEWIMG)*REFRY(4,NEWIMG))+
     1                        (RAYRAY(5,NEWIMG)*REFRY(5,NEWIMG))+
     1                        (RAYRAY(6,NEWIMG)*REFRY(6,NEWIMG)))
                              IF(COSARG.LT.0.0D0) COSARG=-COSARG
                              IF(COSARG.GT.1.0D0) COSARG=1.0D0
                              RRDIF=DACOS(COSARG)
                              IF((RRDIF).GT.(PII)) RRDIF=RRDIF-(TWOPII)
                              IF((RRDIF).LT.(-PII)) RRDIF=RRDIF+(TWOPII)
                              IF(ABS(RRDIF).EQ.ABS(TWOPII)) RRDIF=0.0D0
                              IF((RRDIF).LT.(-TWOPII)) RRDIF=RRDIF+(TWOPII)
                          END IF
C
                          IF(FANTYP.EQ.1.OR.FANTYP.EQ.2) THEN
                              IF(RAYCOD(1).EQ.0.0D0) THEN
                                  IF(DABS(XXDIF).LT.1.0D-7) XXDIF=0.0D0
                                  IF(DABS(YYDIF).LT.1.0D-7) YYDIF=0.0D0
                                  JJJ=IX+INT(DBLE(MAXFAN)/2.0D0)+1
C     JJJ GOES FROM 1 TO MAXFAN+1
                                  FFAANN(I,JJJ,1,K)=XXDIF
                                  FFAANN(I,JJJ,2,K)=YYDIF
                                  FFAANN(I,JJJ,3,K)=0.0D0
                                  FFAANN(I,JJJ,4,K)=0.0D0
                                  FFAANN(I,JJJ,5,K)=1.0D0
                                  FFAANN(I,JJJ,6,K)=1.0D0
                              ELSE
C     RAY FAILED
                                  JJJ=IX+INT(DBLE(MAXFAN)/2.0D0)+1
C     JJJ GOES FROM 1 TO MAXFAN+1
                                  FFAANN(I,JJJ,1,K)=0.0D0
                                  FFAANN(I,JJJ,2,K)=0.0D0
                                  FFAANN(I,JJJ,3,K)=0.0D0
                                  FFAANN(I,JJJ,4,K)=0.0D0
                                  FFAANN(I,JJJ,5,K)=0.0D0
                                  FFAANN(I,JJJ,6,K)=0.0D0
                              END IF
                          ELSE
C       NFAN OR PFAN
                              IF(DABS(XXDIF).LT.1.0D-7) XXDIF=0.0D0
                              IF(DABS(YYDIF).LT.1.0D-7) YYDIF=0.0D0
                              IF(FANTYP.EQ.3) THEN
C     NFAN
                                  XTEMP=(XXDIF*DCOS(-PII/4.0D0))+(YYDIF*DSIN(-PII/4.0D0))
                                  YTEMP=(YYDIF*DCOS(-PII/4.0D0))-(XXDIF*DSIN(-PII/4.0D0))
                                  XXDIF=YTEMP
                                  YYDIF=XTEMP
                              END IF
                              IF(FANTYP.EQ.4) THEN
C     PFAN
                                  XTEMP=(XXDIF*DCOS(PII/4.0D0))+(YYDIF*DSIN(PII/4.0D0))
                                  YTEMP=(YYDIF*DCOS(PII/4.0D0))-(XXDIF*DSIN(PII/4.0D0))
                                  XXDIF=-XTEMP
                                  YYDIF=-YTEMP
                              END IF
                              IF(RAYCOD(1).EQ.0.0D0) THEN
                                  IF(DABS(XXDIF).LT.1.0D-7) XXDIF=0.0D0
                                  IF(DABS(YYDIF).LT.1.0D-7) YYDIF=0.0D0
                                  JJJ=IX+INT(DBLE(MAXFAN)/2.0D0)+1
C     JJJ GOES FROM 1 TO MAXFAN+1
                                  FFAANN(I,JJJ,1,K)=XXDIF
                                  FFAANN(I,JJJ,2,K)=YYDIF
                                  FFAANN(I,JJJ,3,K)=0.0D0
                                  FFAANN(I,JJJ,4,K)=0.0D0
                                  FFAANN(I,JJJ,5,K)=1.0D0
                                  FFAANN(I,JJJ,6,K)=1.0D0
                              ELSE
                                  JJJ=IX+INT(DBLE(MAXFAN)/2.0D0)+1
C     JJJ GOES FROM 1 TO MAXFAN+1
                                  FFAANN(I,JJJ,1,K)=0.0D0
                                  FFAANN(I,JJJ,2,K)=0.0D0
                                  FFAANN(I,JJJ,3,K)=0.0D0
                                  FFAANN(I,JJJ,4,K)=0.0D0
                                  FFAANN(I,JJJ,5,K)=0.0D0
                                  FFAANN(I,JJJ,6,K)=0.0D0
                              END IF
                          END IF
                      ELSE
C       QALTYP NOT 0
                      END IF
C
                      IF(QALTYP.EQ.1) THEN
C       OPD
                          WW3=JK_WVN
                          WVN=WW3
                          RAYCOD(1)=0
                          RAYCOD(2)=-1
                          MSG=.FALSE.
                          WW4=1.0D0
                          F58=1
                          NOCOAT=.TRUE.
                          GRASET=.FALSE.
                          DXFSET=.FALSE.
                          CALL RAYTRA
                          F58=0
                          MSG=.TRUE.
                          IF(RAYEXT) THEN
                              OOPD=0.0D0
                              RCOR=0.0D0
                              OCOR=0.0D0
                              IF(DABS(ALENS(3,NEWOBJ)).GE.1.0D10) JJ=NEWOBJ+2
                              IF(DABS(ALENS(3,NEWOBJ)).LT.1.0D10) JJ=NEWOBJ+1
                              DO J=JJ,NEWIMG
                                  OOPD=OOPD+RAYRAY(7,J)
     1                            -(REFRY(7,J)*(ALENS(WWVN,J-1)/ALENS(WWRF,J-1)))
                              END DO
                              IF(SYSTEM1(30).EQ.1.0D0.OR.SYSTEM1(30).EQ.2.0D0) THEN
C       MODE FOCAL
C               RCOR=0.0D0
C               OCOR=0.0D0
                                  CALL FOPD
C       CALCULATE THEN APPLY ADJUSTMENT FOR THE BEGINNING AND ENDING
C       REFERENCE SPHERES.
                                  OOPD=OOPD-(OCOR*ALENS(WWVN,NEWOBJ))
     1                            +(RCOR*ALENS(WWVN,NEWOBJ))
                                  RCOR=0.0D0
                                  OCOR=0.0D0
                                  CENCEN=.FALSE.
                                  CALL LOPD
                                  OOPD=OOPD-(OCOR*ALENS(WWVN,NEWIMG-1))+
     1                            (RCOR*ALENS(WWVN,NEWIMG-1))
                              ELSE
C       MODE AFOCAL
C               RCOR=0.0D0
C               OCOR=0.0D0
                                  CALL FOPD
C       CALCULATE THEN APPLY ADJUSTMENT FOR THE BEGINNING AND ENDING
C       REFERENCE SPHERES.
                                  OOPD=OOPD-(OCOR*ALENS(WWVN,NEWOBJ))
     1                            +(RCOR*ALENS(WWVN,NEWOBJ))
                                  RCOR=0.0D0
                                  OCOR=0.0D0
                                  CENCEN=.FALSE.
                                  CALL LOPD
                                  OOPD=OOPD-(OCOR*ALENS(WWVN,NEWIMG-1))
     1                            +(RCOR*ALENS(WWVN,NEWIMG-1))
                              END IF
                              IF(SYSTEM1(6).EQ.1.0D0) WAV=SYSTEM1(INT(WW3))*
     1                        ((1.0D-3)/(25.4D0))
                              IF(SYSTEM1(6).EQ.2.0D0) WAV=SYSTEM1(INT(WW3))*(1.0D-4)
                              IF(SYSTEM1(6).EQ.3.0D0) WAV=SYSTEM1(INT(WW3))*(1.0D-3)
                              IF(SYSTEM1(6).EQ.4.0D0) WAV=SYSTEM1(INT(WW3))*(1.0D-6)
                              OOPD=-OOPD
                              IF(REVSTR) OOPD=-OOPD
                              OPDW=OOPD/WAV
                          ELSE
C     RAY FAILED
                              OOPD=0.0D0
                              OPDW=0.0D0
                          END IF
C
                          IF(RAYCOD(1).EQ.0.0D0) THEN
C       IF OPD IN WAVES IS LESS THAN 0.0001 WAVES, SET OPDS TO ZERO
                              IF(DABS(OPDW).LT.1.0D-7) THEN
                                  OOPD=0.0D0
                                  OPDW=0.0D0
                              ELSE
C       NOT ZERO
                              END IF
                              JJJ=IX+INT(DBLE(MAXFAN)/2.0D0)+1
                              FFAANN(I,JJJ,1,K)=OPDW
                              FFAANN(I,JJJ,2,K)=0.0D0
                              FFAANN(I,JJJ,3,K)=0.0D0
                              FFAANN(I,JJJ,4,K)=0.0D0
                              FFAANN(I,JJJ,5,K)=1.0D0
                              FFAANN(I,JJJ,6,K)=1.0D0
                          ELSE
                              JJJ=IX+INT(DBLE(MAXFAN)/2.0D0)+1
                              FFAANN(I,JJJ,1,K)=0.0D0
                              FFAANN(I,JJJ,2,K)=0.0D0
                              FFAANN(I,JJJ,3,K)=0.0D0
                              FFAANN(I,JJJ,4,K)=0.0D0
                              FFAANN(I,JJJ,5,K)=0.0D0
                              FFAANN(I,JJJ,6,K)=0.0D0
                          END IF
                      ELSE
C       NOT (OPD)
                      END IF
C
                      IF(QALTYP.EQ.2) THEN
C       CD
                          RAYCOD(1)=0
                          RAYCOD(2)=-1
                          WW3=SYSTEM1(7)
                          WVN=WW3
                          MSG=.FALSE.
                          WW4=1.0D0
                          F58=1
                          NOCOAT=.TRUE.
                          GRASET=.FALSE.
                          DXFSET=.FALSE.
                          CALL RAYTRA
                          F58=0
                          MSG=.TRUE.
                          IF(SYSTEM1(30).EQ.1.0D0.OR.SYSTEM1(30).EQ.2.0D0) THEN
C       MODE FOCAL
                              PW11=RAYRAY(1,NEWIMG)
                              PW12=RAYRAY(2,NEWIMG)
                          ELSE
C       MODE AFOCAL
                              PW11=RAYRAY(11,NEWIMG)
                              PW12=RAYRAY(12,NEWIMG)
                          END IF
                          WW3=SYSTEM1(8)
                          WVN=WW3
                          MSG=.FALSE.
                          WW4=1.0D0
                          F58=1
                          NOCOAT=.TRUE.
                          GRASET=.FALSE.
                          DXFSET=.FALSE.
                          CALL RAYTRA
                          F58=0
                          MSG=.TRUE.
                          IF(SYSTEM1(30).EQ.1.0D0.OR.SYSTEM1(30).EQ.2.0D0) THEN
C       MODE FOCAL
                              PW21=RAYRAY(1,NEWIMG)
                              PW22=RAYRAY(2,NEWIMG)
                          ELSE
C       MODE AFOCAL
                              PW21=RAYRAY(11,NEWIMG)
                              PW22=RAYRAY(12,NEWIMG)
                          END IF
                          WW3=SYSTEM1(9)
                          WVN=WW3
                          MSG=.FALSE.
                          WW4=1.0D0
                          F58=1
                          NOCOAT=.TRUE.
                          GRASET=.FALSE.
                          DXFSET=.FALSE.
                          CALL RAYTRA
                          F58=0
                          MSG=.TRUE.
                          IF(SYSTEM1(30).EQ.1.0D0.OR.SYSTEM1(30).EQ.2.0D0) THEN
C       MODE FOCAL
                              SW11=RAYRAY(1,NEWIMG)
                              SW12=RAYRAY(2,NEWIMG)
                          ELSE
C       MODE AFOCAL
                              SW11=RAYRAY(11,NEWIMG)
                              SW12=RAYRAY(12,NEWIMG)
                          END IF
                          WW3=SYSTEM1(10)
                          WVN=WW3
                          MSG=.FALSE.
                          WW4=1.0D0
                          F58=1
                          NOCOAT=.TRUE.
                          GRASET=.FALSE.
                          DXFSET=.FALSE.
                          CALL RAYTRA
                          F58=0
                          MSG=.TRUE.
                          IF(SYSTEM1(30).EQ.1.0D0.OR.SYSTEM1(30).EQ.2.0D0) THEN
C       MODE FOCAL
                              SW21=RAYRAY(1,NEWIMG)
                              SW22=RAYRAY(2,NEWIMG)
                          ELSE
C       MODE AFOCAL
                              SW21=RAYRAY(11,NEWIMG)
                              SW22=RAYRAY(12,NEWIMG)
                          END IF
C       PRIMARY PAIR
C       X-VALUE
                          DIF1=PW11-PW21
                          IF(SYSTEM1(30).GT.2.0D0) THEN
C     AFOCAL
                              IF((DIF1).GT.(PII)) DIF1=DIF1-(TWOPII)
                              IF((DIF1).LT.(-PII)) DIF1=DIF1+(TWOPII)
                              IF(ABS(DIF1).EQ.ABS(TWOPII)) DIF1=0.0D0
                              IF((DIF1).LT.(-TWOPII)) DIF1=DIF1+(TWOPII)
                          END IF
C       Y-VALUE
                          DIF2=PW12-PW22
                          IF(SYSTEM1(30).GT.2.0D0) THEN
C     AFOCAL
                              IF((DIF2).GT.(PII)) DIF2=DIF2-(TWOPII)
                              IF((DIF2).LT.(-PII)) DIF2=DIF2+(TWOPII)
                              IF(ABS(DIF2).EQ.ABS(TWOPII)) DIF2=0.0D0
                              IF((DIF2).LT.(-TWOPII)) DIF2=DIF2+(TWOPII)
                          END IF
C       SECONDARY PAIR
C       X-VALUE
                          DIF3=SW11-SW21
                          IF(SYSTEM1(30).GT.2.0D0) THEN
C     AFOCAL
                              IF((DIF3).GT.(PII)) DIF3=DIF3-(TWOPII)
                              IF((DIF3).LT.(-PII)) DIF3=DIF3+(TWOPII)
                              IF(ABS(DIF3).EQ.ABS(TWOPII)) DIF3=0.0D0
                              IF((DIF3).LT.(-TWOPII)) DIF3=DIF3+(TWOPII)
                          END IF
C       Y-VALUE
                          DIF4=SW12-SW22
                          IF(SYSTEM1(30).GT.2.0D0) THEN
C     AFOCAL
                              IF((DIF4).GT.(PII)) DIF4=DIF4-(TWOPII)
                              IF((DIF4).LT.(-PII)) DIF4=DIF4+(TWOPII)
                              IF(ABS(DIF4).EQ.ABS(TWOPII)) DIF4=0.0D0
                              IF((DIF4).LT.(-TWOPII)) DIF4=DIF4+(TWOPII)
                          END IF
                          IF(FANTYP.EQ.1.OR.FANTYP.EQ.2) THEN
C     X OR Y FAN
                              IF(RAYCOD(1).EQ.0.0D0) THEN
                                  JJJ=IX+INT(DBLE(MAXFAN)/2.0D0)+1
                                  FFAANN(I,JJJ,1,K)=DIF1
                                  FFAANN(I,JJJ,2,K)=DIF2
                                  FFAANN(I,JJJ,3,K)=DIF3
                                  FFAANN(I,JJJ,4,K)=DIF4
                                  FFAANN(I,JJJ,5,K)=1.0D0
                                  FFAANN(I,JJJ,6,K)=1.0D0
                              ELSE
                                  JJJ=IX+INT(DBLE(MAXFAN)/2.0D0)+1
                                  FFAANN(I,JJJ,1,K)=0.0D0
                                  FFAANN(I,JJJ,2,K)=0.0D0
                                  FFAANN(I,JJJ,3,K)=0.0D0
                                  FFAANN(I,JJJ,4,K)=0.0D0
                                  FFAANN(I,JJJ,5,K)=0.0D0
                                  FFAANN(I,JJJ,6,K)=0.0D0
                              END IF
                          ELSE
C     N OR P FAN
                              IF(FANTYP.EQ.3) THEN
C     NFAN
                                  TEMP1=(DIF1*DCOS(-PII/4.0D0))+(DIF2*DSIN(-PII/4.0D0))
                                  TEMP2=(DIF2*DCOS(-PII/4.0D0))-(DIF1*DSIN(-PII/4.0D0))
                                  TEMP3=(DIF3*DCOS(-PII/4.0D0))+(DIF4*DSIN(-PII/4.0D0))
                                  TEMP4=(DIF4*DCOS(-PII/4.0D0))-(DIF3*DSIN(-PII/4.0D0))
                                  DIF1=TEMP2
                                  DIF2=TEMP1
                                  DIF3=TEMP4
                                  DIF4=TEMP3
                              END IF
                              IF(FANTYP.EQ.4) THEN
C     PFAN
                                  TEMP1=(DIF1*DCOS(PII/4.0D0))+(DIF2*DSIN(PII/4.0D0))
                                  TEMP2=(DIF2*DCOS(PII/4.0D0))-(DIF1*DSIN(PII/4.0D0))
                                  TEMP3=(DIF3*DCOS(PII/4.0D0))+(DIF4*DSIN(PII/4.0D0))
                                  TEMP4=(DIF4*DCOS(PII/4.0D0))-(DIF3*DSIN(PII/4.0D0))
                                  DIF1=-TEMP1
                                  DIF2=-TEMP2
                                  DIF3=-TEMP3
                                  DIF4=-TEMP4
                              END IF
                              IF(RAYCOD(1).EQ.0.0D0) THEN
                                  JJJ=IX+INT(DBLE(MAXFAN)/2.0D0)+1
                                  FFAANN(I,JJJ,1,K)=DIF1
                                  FFAANN(I,JJJ,2,K)=DIF2
                                  FFAANN(I,JJJ,3,K)=DIF3
                                  FFAANN(I,JJJ,4,K)=DIF4
                                  FFAANN(I,JJJ,5,K)=1.0D0
                                  FFAANN(I,JJJ,6,K)=1.0D0
                              ELSE
                                  JJJ=IX+INT(DBLE(MAXFAN)/2.0D0)+1
                                  FFAANN(I,JJJ,1,K)=0.0D0
                                  FFAANN(I,JJJ,2,K)=0.0D0
                                  FFAANN(I,JJJ,3,K)=0.0D0
                                  FFAANN(I,JJJ,4,K)=0.0D0
                                  FFAANN(I,JJJ,5,K)=0.0D0
                                  FFAANN(I,JJJ,6,K)=0.0D0
                              END IF
                          END IF
                      ELSE
C       NOT CD
                      END IF
C
                      IF(QALTYP.EQ.3) THEN
                          WW3=JK_WVN
                          WVN=WW3
                          RAYCOD(1)=0
                          RAYCOD(2)=-1
                          IF(FANTYP.EQ.1) THEN
                              IF(REAL(WW1).LT.0.001.AND.WW1.GT.0.0D0)WW1=0.001D0
                              IF(REAL(WW1).GT.-0.001.AND.WW1.LT.0.0D0)WW1=-0.001D0
                              IF(REAL(WW1).EQ.0.0) WW1=.01D0
                          END IF
                          IF(FANTYP.EQ.2) THEN
                              IF(REAL(WW2).LT.0.001.AND.WW2.GT.0.0D0)WW2=0.001D0
                              IF(REAL(WW2).GT.-0.001.AND.WW2.LT.0.0D0)WW2=-0.001D0
                              IF(REAL(WW2).EQ.0.0) WW2=.01D0
                          END IF
                          IF(FANTYP.EQ.3.OR.FANTYP.EQ.4) THEN
                              IF(REAL(WW1).LT.0.001.AND.WW1.GT.0.0D0)WW1=0.001D0
                              IF(REAL(WW2).LT.0.001.AND.WW2.GT.0.0D0)WW2=0.001D0
                              IF(REAL(WW1).GT.-0.001.AND.WW1.LT.0.0D0)WW1=-0.001D0
                              IF(REAL(WW2).GT.-0.01.AND.WW2.LT.0.0D0)WW2=-0.001D0
                              IF(REAL(WW1).EQ.0.0) WW1=.0001D0
                              IF(REAL(WW2).EQ.0.0) WW2=.0001D0
                          END IF
                          MSG=.FALSE.
                          WW4=1.0D0
                          F58=1
                          NOCOAT=.TRUE.
                          GRASET=.FALSE.
                          DXFSET=.FALSE.
                          CALL RAYTRA
                          F58=0
                          MSG=.TRUE.
C       CALCULATION OF LA IN XZ AND YZ PLANE.
                          DX=(RAYRAY(1,NEWIMG)-REFRY(1,NEWIMG))/JB
                          DY=(RAYRAY(2,NEWIMG)-REFRY(2,NEWIMG))/JA
                          LLR=RAYRAY(4,NEWIMG)
                          MMR=RAYRAY(5,NEWIMG)
                          NNR=RAYRAY(6,NEWIMG)
                          LLP=REFRY(4,NEWIMG)
                          MMP=REFRY(5,NEWIMG)
                          NNP=REFRY(6,NEWIMG)
                          IF(FANTYP.EQ.1.OR.FANTYP.EQ.2) THEN
                              DTX=((LLR/NNR)-(LLP/NNP))
                              DTY=((MMR/NNR)-(MMP/NNP))
                          END IF
                          IF(FANTYP.EQ.3) THEN
C     NFAN
                              TEMP1=(DX*DCOS(-PII/4.0D0))+(DY*DSIN(-PII/4.0D0))
                              TEMP2=(DY*DCOS(-PII/4.0D0))-(DX*DSIN(-PII/4.0D0))
                              DX=TEMP1
                              DY=TEMP2
                              TEMP3=((LLR*DCOS(-PII/4.0D0))+(MMR*DSIN(-PII/4.0D0)))
                              TEMP4=((MMR*DCOS(-PII/4.0D0))-(LLR*DSIN(-PII/4.0D0)))
                              LLR=TEMP3
                              MMR=TEMP4
                              TEMP3=((LLP*DCOS(-PII/4.0D0))+(MMP*DSIN(-PII/4.0D0)))
                              TEMP4=((MMP*DCOS(-PII/4.0D0))-(LLP*DSIN(-PII/4.0D0)))
                              LLP=TEMP3
                              MMP=TEMP4
                              DTX=((LLR/NNR)-(LLP/NNP))
                              DTY=((MMR/NNR)-(MMP/NNP))
                          END IF
                          IF(FANTYP.EQ.4) THEN
C     PFAN
                              TEMP1=(DX*DCOS(PII/4.0D0))+(DY*DSIN(PII/4.0D0))
                              TEMP2=(DY*DCOS(PII/4.0D0))-(DX*DSIN(PII/4.0D0))
                              DX=TEMP1
                              DY=TEMP2
                              TEMP3=((LLR*DCOS(PII/4.0D0))+(MMR*DSIN(PII/4.0D0)))
                              TEMP4=((MMR*DCOS(PII/4.0D0))-(LLR*DSIN(PII/4.0D0)))
                              LLR=TEMP3
                              MMR=TEMP4
                              TEMP3=((LLP*DCOS(PII/4.0D0))+(MMP*DSIN(PII/4.0D0)))
                              TEMP4=((MMP*DCOS(PII/4.0D0))-(LLP*DSIN(PII/4.0D0)))
                              LLP=TEMP3
                              MMP=TEMP4
                              DTX=((LLR/NNR)-(LLP/NNP))
                              DTY=((MMR/NNR)-(MMP/NNP))
                          END IF
                          IF(DX.EQ.0.0D0.OR.DTX.EQ.0.0D0) LAX=0.0D0
                          IF(DX.NE.0.0D0.AND.DTX.NE.0.0D0) LAX=DX/DTX
                          IF(DY.EQ.0.0D0.OR.DTY.EQ.0.0D0) LAY=0.0D0
                          IF(DY.NE.0.0D0.AND.DTY.NE.0.0D0) LAY=DY/DTY
                          IF(RAYCOD(1).EQ.0.0D0) THEN
                              JJJ=IX+INT(DBLE(MAXFAN)/2.0D0)+1
                              IF(FANTYP.EQ.3) THEN
                                  FFAANN(I,JJJ,1,K)=LAY
                                  FFAANN(I,JJJ,2,K)=LAX
                                  FFAANN(I,JJJ,3,K)=0.0D0
                                  FFAANN(I,JJJ,4,K)=0.0D0
                                  FFAANN(I,JJJ,5,K)=1.0D0
                                  FFAANN(I,JJJ,6,K)=1.0D0
                              ELSE
C     FANTYP WAS 1, 2 OR 4
                                  FFAANN(I,JJJ,1,K)=LAX
                                  FFAANN(I,JJJ,2,K)=LAY
                                  FFAANN(I,JJJ,3,K)=0.0D0
                                  FFAANN(I,JJJ,4,K)=0.0D0
                                  FFAANN(I,JJJ,5,K)=1.0D0
                                  FFAANN(I,JJJ,6,K)=1.0D0
                              END IF
                          ELSE
                              JJJ=IX+INT(DBLE(MAXFAN)/2.0D0)+1
                              FFAANN(I,JJJ,1,K)=0.0D0
                              FFAANN(I,JJJ,2,K)=0.0D0
                              FFAANN(I,JJJ,3,K)=0.0D0
                              FFAANN(I,JJJ,4,K)=0.0D0
                              FFAANN(I,JJJ,5,K)=0.0D0
                              FFAANN(I,JJJ,6,K)=0.0D0
                          END IF
C
                      ELSE
C       NOT LA
                      END IF
C
C     THIS IS THE END OF THE FAN LOOP
                  END DO
C
C     THIS IS THE END OF THE FIELD OF VIEW LOOP
              END DO
C     THIS IS THE END OF THE WAVELENGTH LOOP
99999         CONTINUE
          END DO
          RETURN
      END


C SUB PLFAN3.FOR
      SUBROUTINE PLFAN3(ICOMP)
C
          IMPLICIT NONE
C
C     DOES FAN CALCULATIONS FOR USER-DEFINED FAN PLOTTING
C
          INTEGER J,JJ,IX,JJJ,FANWAV,WWRF
C
          INTEGER JK_WAV(1:10),WWVN,ICOMP
C
          COMMON/WAVER/JK_WAV
C
          INTEGER XTENT,YTENT,CLIP
C
          COMMON/USEFAN/XTENT,YTENT,CLIP
C
          REAL*8 COSARG,XTEMP,YTEMP,TEMP1,TEMP2,TEMP3,TEMP4,
     1    OFFSET,XI,XX1,YY1,LLR,MMR,NNR,LLP,MMP,NNP,
     2    XXDIF,YYDIF,OOPD,OPDW,
     3    WAV,RRDIF,DIF1,DIF2,DIF3,DIF4,PW11,PW12,PW21,PW22,
     4    SW11,SW12,SW21,SW22,LAX,LAY,DX,DY,DTY,DTX,
     5    JA,JB
C
          LOGICAL FOBB0,FANEXT
     1    ,FOBB0X,FOBB0Y
C
          COMMON/FANEXI/FANEXT
C
          COMMON/FANNER/FANWAV
C
          COMMON/FFOOBB/FOBB0,FOBB0X,FOBB0Y
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          IF(INT(LFOB(4)).EQ.1) WWRF=46
          IF(INT(LFOB(4)).EQ.2) WWRF=47
          IF(INT(LFOB(4)).EQ.3) WWRF=48
          IF(INT(LFOB(4)).EQ.4) WWRF=49
          IF(INT(LFOB(4)).EQ.5) WWRF=50
          IF(INT(LFOB(4)).EQ.6) WWRF=71
          IF(INT(LFOB(4)).EQ.7) WWRF=72
          IF(INT(LFOB(4)).EQ.8) WWRF=73
          IF(INT(LFOB(4)).EQ.9) WWRF=74
          IF(INT(LFOB(4)).EQ.10) WWRF=75
          IF(FANWAV.EQ.1) WWVN=46
          IF(FANWAV.EQ.2) WWVN=47
          IF(FANWAV.EQ.3) WWVN=48
          IF(FANWAV.EQ.4) WWVN=49
          IF(FANWAV.EQ.5) WWVN=50
          IF(FANWAV.EQ.6) WWVN=71
          IF(FANWAV.EQ.7) WWVN=72
          IF(FANWAV.EQ.8) WWVN=73
          IF(FANWAV.EQ.9) WWVN=74
          IF(FANWAV.EQ.10) WWVN=75
C
          DO JJJ=1,21
              FFAANN(1,JJJ,1,1)=0.0D0
              FFAANN(1,JJJ,5,1)=1.0D0
          END DO
C
C     TRACE THE GUT RAY, IF OK KEEP GOING, IF NOT RETURN
C     WITH ABORT SET TO TRUE AND PRINT ERROR MESSAGE
C
          SAVE_KDP(1)=SAVEINPT(1)
C     RESET THE INPUT WORDS AND CALL FFOB
          WC='FOB'
          WQ='        '
          SQ=0
          S1=1
          S2=1
          S3=1
          S4=1
          S5=0
          DF1=0
          DF2=0
          DF3=0
          DF4=0
          DF5=1
          W1=LFOB(1)
          W2=LFOB(2)
          W3=LFOB(3)
          W4=LFOB(4)
          W5=0.0D0
          CALL FFOB
          JA=COS_A_ANG
          JB=COS_B_ANG
C     RESTORE THE OLD COMMAND LINE
          REST_KDP(1)=RESTINPT(1)
C
C     ALL IS OK WITH THE GUT RAY, TRACE THE FAN.
C       CLEAR APERTURE/OBSCURATION CHECKING IS SET
C       TO "ON" BY SETTING CACOCH=1.
C
          CACOCH=1
C
C       THE ACTUAL COORDINATES OF THE RAYS TO BE TRACED (AT
C       THE REFERENCE SURFACE) ARE DETERMINED IN RAYTRA. ALL THAT NEEDS
C       BE SENT TO RAYTRA IS THE RELATIVE APERTURE POSITIONS.
C       CIRCULAR PUPIL COORDINATES ARE RESOLVED IN RAYTRA.FOR.
C
C       IF PUPIL IS SET TO RECT, TEMPORARILY RESET IT TO CIRCULAR
C       AS FANS ARE ONLY DEFINED FOR A CIRCULAR PUPIL DEFINITION
C       DUE TO THE PRESENCE OF PFAN AND NFAN. IN THIS TEMPORARY
C       REDEFINITION, WE DON'T CHANGE THE VALUE OF SAX OR A
C       CLAPX ON THE REFERENCE SURFACE, WE JUST DON'T USE IT.
C
C
C     FAN LIMITS ARE -1 TO 1 IN 1/(MAXFAN/2.0D0)
C     STEPS FOR MAXFAN+1 TOTAL POINTS
          OFFSET=FANOFF
C
C     NOW DO FAN TRACES
C
          DO IX=-INT(DBLE(MAXFAN)/2.0D0),INT(DBLE(MAXFAN)/2.0D0),1
              XI=DBLE(IX)/(DBLE(MAXFAN)/2.0D0)
C     SET THE OFFSET
              OFFSET=FANOFF
C
C     DO THE SETUP FOR TYPES OF FANS
C
              IF(FANTYP.EQ.1) THEN
C       YFAN
C       RELATIVE X COORDINATE IS:
                  XX1=OFFSET
C       RELATIVE Y COORDINATE IS:
                  YY1=XI
              END IF
              IF(FANTYP.EQ.2) THEN
C       XFAN
C       RELATIVE X COORDINATE IS:
                  XX1=XI
C       RELATIVE Y COORDINATE IS:
                  YY1=OFFSET
              END IF
              IF(FANTYP.EQ.3) THEN
C       NFAN
C       RELATIVE X COORDINATE IS:
                  XX1=(XI-OFFSET)/(DSQRT(2.0D0))
C       RELATIVE Y COORDINATE IS:
                  YY1=(XI+OFFSET)/(DSQRT(2.0D0))
              END IF
              IF(FANTYP.EQ.4) THEN
C       PFAN
C       RELATIVE X COORDINATE IS:
                  XX1=(XI+OFFSET)/(DSQRT(2.0D0))
C       RELATIVE Y COORDINATE IS:
                  YY1=-(XI-OFFSET)/(DSQRT(2.0D0))
              END IF
C
C       NOW CALL RAYTRA TO TRACE THE SPECIFIC RAY
              WWQ='        '
              WW1=YY1
              WW2=XX1
C
              IF(QALTYP.EQ.0) THEN
C     JUST DY/DX OR DYA/DXA
                  WW3=DBLE(FANWAV)
                  WVN=WW3
                  RAYCOD(1)=0
                  RAYCOD(2)=-1
                  MSG=.FALSE.
                  F58=1
                  NOCOAT=.TRUE.
                  GRASET=.FALSE.
                  DXFSET=.FALSE.
                  CALL RAYTRA
                  F58=0
                  MSG=.TRUE.
                  IF(SYSTEM1(30).EQ.1.0D0.OR.SYSTEM1(30).EQ.2.0D0) THEN
C       MODE FOCAL
                      XXDIF=(RAYRAY(1,NEWIMG)-REFRY(1,NEWIMG))/JB
                      YYDIF=(RAYRAY(2,NEWIMG)-REFRY(2,NEWIMG))/JA
                      RRDIF=DSQRT((XXDIF**2)+(YYDIF**2))
                  ELSE
C       MOD AFOCAL
                      XXDIF=RAYRAY(11,NEWIMG)
     1                -REFRY(11,NEWIMG)
                      YYDIF=RAYRAY(12,NEWIMG)
     1                -REFRY(12,NEWIMG)
                      IF((XXDIF).GT.(PII)) XXDIF=XXDIF-(TWOPII)
                      IF((YYDIF).GT.(PII)) YYDIF=YYDIF-(TWOPII)
                      IF((XXDIF).LT.(-PII)) XXDIF=XXDIF+(TWOPII)
                      IF((YYDIF).LT.(-PII)) YYDIF=YYDIF+(TWOPII)
                      IF(ABS(XXDIF).EQ.ABS(TWOPII)) XXDIF=0.0D0
                      IF(ABS(YYDIF).EQ.ABS(TWOPII)) YYDIF=0.0D0
                      IF((XXDIF).LT.(-TWOPII)) XXDIF=XXDIF+(TWOPII)
                      IF((YYDIF).LT.(-TWOPII)) YYDIF=YYDIF+(TWOPII)

                      COSARG=((RAYRAY(4,NEWIMG)*REFRY(4,NEWIMG))+
     1                (RAYRAY(5,NEWIMG)*REFRY(5,NEWIMG))+
     1                (RAYRAY(6,NEWIMG)*REFRY(6,NEWIMG)))
                      IF(COSARG.LT.0.0D0) COSARG=-COSARG
                      IF(COSARG.GT.1.0D0) COSARG=1.0D0
                      RRDIF=DACOS(COSARG)
                      IF((RRDIF).GT.(PII)) RRDIF=RRDIF-(TWOPII)
                      IF((RRDIF).LT.(-PII)) RRDIF=RRDIF+(TWOPII)
                      IF(ABS(RRDIF).EQ.ABS(TWOPII)) RRDIF=0.0D0
                      IF((RRDIF).LT.(-TWOPII)) RRDIF=RRDIF+(TWOPII)
                  END IF
C
                  IF(FANTYP.EQ.1.OR.FANTYP.EQ.2) THEN
                      IF(RAYCOD(1).EQ.0.0D0) THEN
                          IF(DABS(XXDIF).LT.1.0D-7) XXDIF=0.0D0
                          IF(DABS(YYDIF).LT.1.0D-7) YYDIF=0.0D0
                          JJJ=IX+INT(DBLE(MAXFAN)/2.0D0)+1
C     JJJ GOES FROM 1 TO MAXFAN+1
                          IF(ICOMP.EQ.1) FFAANN(1,JJJ,1,1)=XXDIF
                          IF(ICOMP.EQ.2) FFAANN(1,JJJ,1,1)=YYDIF
                          FFAANN(1,JJJ,5,1)=1.0D0
                      ELSE
                          JJJ=IX+INT(DBLE(MAXFAN)/2.0D0)+1
C     JJJ GOES FROM 1 TO MAXFAN+1
                          FFAANN(1,JJJ,1,1)=0.0D0
                          FFAANN(1,JJJ,5,1)=0.0D0
                      END IF
                  ELSE
C       NFAN OR PFAN
                      IF(DABS(XXDIF).LT.1.0D-7) XXDIF=0.0D0
                      IF(DABS(YYDIF).LT.1.0D-7) YYDIF=0.0D0
                      IF(FANTYP.EQ.3) THEN
C     NFAN
                          XTEMP=(XXDIF*DCOS(-PII/4.0D0))+(YYDIF*DSIN(-PII/4.0D0))
                          YTEMP=(YYDIF*DCOS(-PII/4.0D0))-(XXDIF*DSIN(-PII/4.0D0))
                          XXDIF=YTEMP
                          YYDIF=XTEMP
                      END IF
                      IF(FANTYP.EQ.4) THEN
C     PFAN
                          XTEMP=(XXDIF*DCOS(PII/4.0D0))+(YYDIF*DSIN(PII/4.0D0))
                          YTEMP=(YYDIF*DCOS(PII/4.0D0))-(XXDIF*DSIN(PII/4.0D0))
                          XXDIF=-XTEMP
                          YYDIF=-YTEMP
                      END IF
                      IF(RAYCOD(1).EQ.0.0D0) THEN
                          IF(DABS(XXDIF).LT.1.0D-7) XXDIF=0.0D0
                          IF(DABS(YYDIF).LT.1.0D-7) YYDIF=0.0D0
                          JJJ=IX+INT(DBLE(MAXFAN)/2.0D0)+1
C     JJJ GOES FROM 1 TO MAXFAN+1
                          IF(ICOMP.EQ.1) FFAANN(1,JJJ,1,1)=XXDIF
                          IF(ICOMP.EQ.2) FFAANN(1,JJJ,1,1)=YYDIF
                          FFAANN(1,JJJ,5,1)=1.0D0
                      ELSE
                          JJJ=IX+INT(DBLE(MAXFAN)/2.0D0)+1
C     JJJ GOES FROM 1 TO MAXFAN+1
                          FFAANN(1,JJJ,1,1)=0.0D0
                          FFAANN(1,JJJ,5,1)=0.0D0
                      END IF
                  END IF
              ELSE
C       QALTYP NOT 0
              END IF
C
              IF(QALTYP.EQ.1) THEN
C       OPD
                  WW3=DBLE(FANWAV)
                  WVN=WW3
                  RAYCOD(1)=0
                  RAYCOD(2)=-1
                  MSG=.FALSE.
                  WW4=1.0D0
                  F58=1
                  NOCOAT=.TRUE.
                  GRASET=.FALSE.
                  DXFSET=.FALSE.
                  CALL RAYTRA
                  F58=0
                  MSG=.TRUE.
                  IF(RAYEXT) THEN
                      OOPD=0.0D0
                      RCOR=0.0D0
                      OCOR=0.0D0
                      IF(DABS(ALENS(3,NEWOBJ)).GE.1.0D10) JJ=NEWOBJ+2
                      IF(DABS(ALENS(3,NEWOBJ)).LT.1.0D10) JJ=NEWOBJ+1
                      DO J=JJ,NEWIMG
                          OOPD=OOPD+RAYRAY(7,J)
     1                    -(REFRY(7,J)*(ALENS(WWVN,J-1)/ALENS(WWRF,J-1)))
                      END DO
                      IF(SYSTEM1(30).EQ.1.0D0.OR.SYSTEM1(30).EQ.2.0D0) THEN
C       MODE FOCAL
C               RCOR=0.0D0
C               OCOR=0.0D0
                          CALL FOPD
C       CALCULATE THEN APPLY ADJUSTMENT FOR THE BEGINNING AND ENDING
C       REFERENCE SPHERES.
                          OOPD=OOPD-(OCOR*ALENS(WWVN,NEWOBJ))
     1                    +(RCOR*ALENS(WWVN,NEWOBJ))
                          RCOR=0.0D0
                          OCOR=0.0D0
                          CENCEN=.FALSE.
                          CALL LOPD
                          OOPD=OOPD-(OCOR*ALENS(WWVN,NEWIMG-1))+
     1                    (RCOR*ALENS(WWVN,NEWIMG-1))
                      ELSE
C       MODE AFOCAL
C               RCOR=0.0D0
C               OCOR=0.0D0
                          CALL FOPD
C       CALCULATE THEN APPLY ADJUSTMENT FOR THE BEGINNING AND ENDING
C       REFERENCE SPHERES.
                          OOPD=OOPD-(OCOR*ALENS(WWVN,NEWOBJ))
     1                    +(RCOR*ALENS(WWVN,NEWOBJ))
                          RCOR=0.0D0
                          OCOR=0.0D0
                          CENCEN=.FALSE.
                          CALL LOPD
                          OOPD=OOPD-(OCOR*ALENS(WWVN,NEWIMG-1))
     1                    +(RCOR*ALENS(WWVN,NEWIMG-1))
                      END IF
                      IF(SYSTEM1(6).EQ.1.0D0) WAV=SYSTEM1(INT(WW3))*
     1                ((1.0D-3)/(25.4D0))
                      IF(SYSTEM1(6).EQ.2.0D0) WAV=SYSTEM1(INT(WW3))*(1.0D-4)
                      IF(SYSTEM1(6).EQ.3.0D0) WAV=SYSTEM1(INT(WW3))*(1.0D-3)
                      IF(SYSTEM1(6).EQ.4.0D0) WAV=SYSTEM1(INT(WW3))*(1.0D-6)
                      OOPD=-OOPD
                      IF(REVSTR) OOPD=-OOPD
                      OPDW=OOPD/WAV
                  ELSE
C     RAY FAILED
                      OOPD=0.0D0
                      OPDW=0.0D0
                  END IF
C
                  IF(RAYCOD(1).EQ.0.0D0) THEN
C       IF OPD IN WAVES IS LESS THAN 0.0001 WAVES, SET OPDS TO ZERO
                      IF(DABS(OPDW).LT.1.0D-7) THEN
                          OOPD=0.0D0
                          OPDW=0.0D0
                      ELSE
C       NOT ZERO
                      END IF
                      JJJ=IX+INT(DBLE(MAXFAN)/2.0D0)+1
                      FFAANN(1,JJJ,1,1)=OPDW
                  ELSE
                      JJJ=IX+INT(DBLE(MAXFAN)/2.0D0)+1
                      FFAANN(1,JJJ,1,1)=0.0D0
                  END IF
              ELSE
C       NOT (OPD)
              END IF
C
              IF(QALTYP.EQ.2) THEN
C       CD
                  RAYCOD(1)=0
                  RAYCOD(2)=-1
                  WW3=SYSTEM1(7)
                  WVN=WW3
                  MSG=.FALSE.
                  WW4=1.0D0
                  F58=1
                  NOCOAT=.TRUE.
                  GRASET=.FALSE.
                  DXFSET=.FALSE.
                  CALL RAYTRA
                  F58=0
                  MSG=.TRUE.
                  IF(SYSTEM1(30).EQ.1.0D0.OR.SYSTEM1(30).EQ.2.0D0) THEN
C       MODE FOCAL
                      PW11=RAYRAY(1,NEWIMG)
                      PW12=RAYRAY(2,NEWIMG)
                  ELSE
C       MODE AFOCAL
                      PW11=RAYRAY(11,NEWIMG)
                      PW12=RAYRAY(12,NEWIMG)
                  END IF
                  WW3=SYSTEM1(8)
                  WVN=WW3
                  MSG=.FALSE.
                  WW4=1.0D0
                  F58=1
                  NOCOAT=.TRUE.
                  GRASET=.FALSE.
                  DXFSET=.FALSE.
                  CALL RAYTRA
                  F58=0
                  MSG=.TRUE.
                  IF(SYSTEM1(30).EQ.1.0D0.OR.SYSTEM1(30).EQ.2.0D0) THEN
C       MODE FOCAL
                      PW21=RAYRAY(1,NEWIMG)
                      PW22=RAYRAY(2,NEWIMG)
                  ELSE
C       MODE AFOCAL
                      PW21=RAYRAY(11,NEWIMG)
                      PW22=RAYRAY(12,NEWIMG)
                  END IF
                  WW3=SYSTEM1(9)
                  WVN=WW3
                  MSG=.FALSE.
                  WW4=1.0D0
                  F58=1
                  NOCOAT=.TRUE.
                  GRASET=.FALSE.
                  DXFSET=.FALSE.
                  CALL RAYTRA
                  F58=0
                  MSG=.TRUE.
                  IF(SYSTEM1(30).EQ.1.0D0.OR.SYSTEM1(30).EQ.2.0D0) THEN
C       MODE FOCAL
                      SW11=RAYRAY(1,NEWIMG)
                      SW12=RAYRAY(2,NEWIMG)
                  ELSE
C       MODE AFOCAL
                      SW11=RAYRAY(11,NEWIMG)
                      SW12=RAYRAY(12,NEWIMG)
                  END IF
                  WW3=SYSTEM1(10)
                  WVN=WW3
                  MSG=.FALSE.
                  WW4=1.0D0
                  F58=1
                  NOCOAT=.TRUE.
                  GRASET=.FALSE.
                  DXFSET=.FALSE.
                  CALL RAYTRA
                  F58=0
                  MSG=.TRUE.
                  IF(SYSTEM1(30).EQ.1.0D0.OR.SYSTEM1(30).EQ.2.0D0) THEN
C       MODE FOCAL
                      SW21=RAYRAY(1,NEWIMG)
                      SW22=RAYRAY(2,NEWIMG)
                  ELSE
C       MODE AFOCAL
                      SW21=RAYRAY(11,NEWIMG)
                      SW22=RAYRAY(12,NEWIMG)
                  END IF
C       PRIMARY PAIR
C       X-VALUE
                  DIF1=PW11-PW21
                  IF(SYSTEM1(30).GT.2.0D0) THEN
C     AFOCAL
                      IF((DIF1).GT.(PII)) DIF1=DIF1-(TWOPII)
                      IF((DIF1).LT.(-PII)) DIF1=DIF1+(TWOPII)
                      IF(ABS(DIF1).EQ.ABS(TWOPII)) DIF1=0.0D0
                      IF((DIF1).LT.(-TWOPII)) DIF1=DIF1+(TWOPII)
                  END IF
C       Y-VALUE
                  DIF2=PW12-PW22
                  IF(SYSTEM1(30).GT.2.0D0) THEN
C     AFOCAL
                      IF((DIF2).GT.(PII)) DIF2=DIF2-(TWOPII)
                      IF((DIF2).LT.(-PII)) DIF2=DIF2+(TWOPII)
                      IF(ABS(DIF2).EQ.ABS(TWOPII)) DIF2=0.0D0
                      IF((DIF2).LT.(-TWOPII)) DIF2=DIF2+(TWOPII)
                  END IF
C       SECONDARY PAIR
C       X-VALUE
                  DIF3=SW11-SW21
                  IF(SYSTEM1(30).GT.2.0D0) THEN
C     AFOCAL
                      IF((DIF3).GT.(PII)) DIF3=DIF3-(TWOPII)
                      IF((DIF3).LT.(-PII)) DIF3=DIF3+(TWOPII)
                      IF(ABS(DIF3).EQ.ABS(TWOPII)) DIF3=0.0D0
                      IF((DIF3).LT.(-TWOPII)) DIF3=DIF3+(TWOPII)
                  END IF
C       Y-VALUE
                  DIF4=SW12-SW22
                  IF(SYSTEM1(30).GT.2.0D0) THEN
C     AFOCAL
                      IF((DIF4).GT.(PII)) DIF4=DIF4-(TWOPII)
                      IF((DIF4).LT.(-PII)) DIF4=DIF4+(TWOPII)
                      IF(ABS(DIF4).EQ.ABS(TWOPII)) DIF4=0.0D0
                      IF((DIF4).LT.(-TWOPII)) DIF4=DIF4+(TWOPII)
                  END IF
                  IF(FANTYP.EQ.1.OR.FANTYP.EQ.2) THEN
C     X OR Y FAN
                      IF(RAYCOD(1).EQ.0.0D0) THEN
                          JJJ=IX+INT(DBLE(MAXFAN)/2.0D0)+1
                          IF(ICOMP.EQ.1) FFAANN(1,JJJ,1,1)=DIF1
                          IF(ICOMP.EQ.2) FFAANN(1,JJJ,1,1)=DIF2
                          IF(ICOMP.EQ.3) FFAANN(1,JJJ,1,1)=DIF3
                          IF(ICOMP.EQ.4) FFAANN(1,JJJ,1,1)=DIF4
                          FFAANN(1,JJJ,5,1)=1.0D0
                      ELSE
                          JJJ=IX+INT(DBLE(MAXFAN)/2.0D0)+1
                          FFAANN(1,JJJ,1,1)=0.0D0
                          FFAANN(1,JJJ,5,1)=0.0D0
                      END IF
                  ELSE
C     N OR P FAN
                      IF(FANTYP.EQ.3) THEN
C     NFAN
                          TEMP1=(DIF1*DCOS(-PII/4.0D0))+(DIF2*DSIN(-PII/4.0D0))
                          TEMP2=(DIF2*DCOS(-PII/4.0D0))-(DIF1*DSIN(-PII/4.0D0))
                          TEMP3=(DIF3*DCOS(-PII/4.0D0))+(DIF4*DSIN(-PII/4.0D0))
                          TEMP4=(DIF4*DCOS(-PII/4.0D0))-(DIF3*DSIN(-PII/4.0D0))
                          DIF1=TEMP2
                          DIF2=TEMP1
                          DIF3=TEMP4
                          DIF4=TEMP3
                      END IF
                      IF(FANTYP.EQ.4) THEN
C     PFAN
                          TEMP1=(DIF1*DCOS(PII/4.0D0))+(DIF2*DSIN(PII/4.0D0))
                          TEMP2=(DIF2*DCOS(PII/4.0D0))-(DIF1*DSIN(PII/4.0D0))
                          TEMP3=(DIF3*DCOS(PII/4.0D0))+(DIF4*DSIN(PII/4.0D0))
                          TEMP4=(DIF4*DCOS(PII/4.0D0))-(DIF3*DSIN(PII/4.0D0))
                          DIF1=-TEMP1
                          DIF2=-TEMP2
                          DIF3=-TEMP3
                          DIF4=-TEMP4
                      END IF
                      IF(RAYCOD(1).EQ.0.0D0) THEN
                          JJJ=IX+INT(DBLE(MAXFAN)/2.0D0)+1
                          IF(ICOMP.EQ.1) FFAANN(1,JJJ,1,1)=DIF1
                          IF(ICOMP.EQ.2) FFAANN(1,JJJ,1,1)=DIF2
                          IF(ICOMP.EQ.3) FFAANN(1,JJJ,1,1)=DIF3
                          IF(ICOMP.EQ.4) FFAANN(1,JJJ,1,1)=DIF4
                          FFAANN(1,JJJ,5,1)=1.0D0
                      ELSE
                          JJJ=IX+INT(DBLE(MAXFAN)/2.0D0)+1
                          FFAANN(1,JJJ,1,1)=0.0D0
                          FFAANN(1,JJJ,5,1)=0.0D0
                      END IF
                  END IF
              ELSE
C       NOT CD
              END IF
C
              IF(QALTYP.EQ.3) THEN
                  WW3=DBLE(FANWAV)
                  WVN=WW3
                  RAYCOD(1)=0
                  RAYCOD(2)=-1
                  IF(FANTYP.EQ.1) THEN
                      IF(REAL(WW1).LT.0.001.AND.WW1.GT.0.0D0)WW1=0.001D0
                      IF(REAL(WW1).GT.-0.001.AND.WW1.LT.0.0D0)WW1=-0.001D0
                      IF(REAL(WW1).EQ.0.0) WW1=.01D0
                  END IF
                  IF(FANTYP.EQ.2) THEN
                      IF(REAL(WW2).LT.0.001.AND.WW2.GT.0.0D0)WW2=0.001D0
                      IF(REAL(WW2).GT.-0.001.AND.WW2.LT.0.0D0)WW2=-0.001D0
                      IF(REAL(WW2).EQ.0.0) WW2=.01D0
                  END IF
                  IF(FANTYP.EQ.3.OR.FANTYP.EQ.4) THEN
                      IF(REAL(WW1).LT.0.001.AND.WW1.GT.0.0D0)WW1=0.001D0
                      IF(REAL(WW2).LT.0.001.AND.WW2.GT.0.0D0)WW2=0.001D0
                      IF(REAL(WW1).GT.-0.001.AND.WW1.LT.0.0D0)WW1=-0.001D0
                      IF(REAL(WW2).GT.-0.01.AND.WW2.LT.0.0D0)WW2=-0.001D0
                      IF(REAL(WW1).EQ.0.0) WW1=.0001D0
                      IF(REAL(WW2).EQ.0.0) WW2=.0001D0
                  END IF
                  MSG=.FALSE.
                  WW4=1.0D0
                  F58=1
                  NOCOAT=.TRUE.
                  GRASET=.FALSE.
                  DXFSET=.FALSE.
                  CALL RAYTRA
                  F58=0
                  MSG=.TRUE.
C       CALCULATION OF LA IN XZ AND YZ PLANE.
                  DX=(RAYRAY(1,NEWIMG)-REFRY(1,NEWIMG))/JB
                  DY=(RAYRAY(2,NEWIMG)-REFRY(2,NEWIMG))/JA
                  LLR=RAYRAY(4,NEWIMG)
                  MMR=RAYRAY(5,NEWIMG)
                  NNR=RAYRAY(6,NEWIMG)
                  LLP=REFRY(4,NEWIMG)
                  MMP=REFRY(5,NEWIMG)
                  NNP=REFRY(6,NEWIMG)
                  IF(FANTYP.EQ.1.OR.FANTYP.EQ.2) THEN
                      DTX=((LLR/NNR)-(LLP/NNP))
                      DTY=((MMR/NNR)-(MMP/NNP))
                  END IF
                  IF(FANTYP.EQ.3) THEN
C     NFAN
                      TEMP1=(DX*DCOS(-PII/4.0D0))+(DY*DSIN(-PII/4.0D0))
                      TEMP2=(DY*DCOS(-PII/4.0D0))-(DX*DSIN(-PII/4.0D0))
                      DX=TEMP1
                      DY=TEMP2
                      TEMP3=((LLR*DCOS(-PII/4.0D0))+(MMR*DSIN(-PII/4.0D0)))
                      TEMP4=((MMR*DCOS(-PII/4.0D0))-(LLR*DSIN(-PII/4.0D0)))
                      LLR=TEMP3
                      MMR=TEMP4
                      TEMP3=((LLP*DCOS(-PII/4.0D0))+(MMP*DSIN(-PII/4.0D0)))
                      TEMP4=((MMP*DCOS(-PII/4.0D0))-(LLP*DSIN(-PII/4.0D0)))
                      LLP=TEMP3
                      MMP=TEMP4
                      DTX=((LLR/NNR)-(LLP/NNP))
                      DTY=((MMR/NNR)-(MMP/NNP))
                  END IF
                  IF(FANTYP.EQ.4) THEN
C     PFAN
                      TEMP1=(DX*DCOS(PII/4.0D0))+(DY*DSIN(PII/4.0D0))
                      TEMP2=(DY*DCOS(PII/4.0D0))-(DX*DSIN(PII/4.0D0))
                      DX=TEMP1
                      DY=TEMP2
                      TEMP3=((LLR*DCOS(PII/4.0D0))+(MMR*DSIN(PII/4.0D0)))
                      TEMP4=((MMR*DCOS(PII/4.0D0))-(LLR*DSIN(PII/4.0D0)))
                      LLR=TEMP3
                      MMR=TEMP4
                      TEMP3=((LLP*DCOS(PII/4.0D0))+(MMP*DSIN(PII/4.0D0)))
                      TEMP4=((MMP*DCOS(PII/4.0D0))-(LLP*DSIN(PII/4.0D0)))
                      LLP=TEMP3
                      MMP=TEMP4
                      DTX=((LLR/NNR)-(LLP/NNP))
                      DTY=((MMR/NNR)-(MMP/NNP))
                  END IF
                  IF(DX.EQ.0.0D0.OR.DTX.EQ.0.0D0) LAX=0.0D0
                  IF(DX.NE.0.0D0.AND.DTX.NE.0.0D0) LAX=DX/DTX
                  IF(DY.EQ.0.0D0.OR.DTY.EQ.0.0D0) LAY=0.0D0
                  IF(DY.NE.0.0D0.AND.DTY.NE.0.0D0) LAY=DY/DTY
                  IF(RAYCOD(1).EQ.0.0D0) THEN
                      JJJ=IX+INT(DBLE(MAXFAN)/2.0D0)+1
                      IF(ICOMP.EQ.1) FFAANN(1,JJJ,1,1)=LAX
                      IF(ICOMP.EQ.2) FFAANN(1,JJJ,1,1)=LAY
                      FFAANN(1,JJJ,5,1)=1.0D0
                  ELSE
                      JJJ=IX+INT(DBLE(MAXFAN)/2.0D0)+1
                      FFAANN(1,JJJ,1,1)=0.0D0
                      FFAANN(1,JJJ,5,1)=0.0D0
                  END IF
C
              ELSE
C       NOT LA
              END IF
          END DO
          RETURN
      END

      SUBROUTINE FANWV
C
          IMPLICIT NONE
C
C       THIS ROUTINE DOES THE PLOTTING OF THE WAVELENGTH LEGEND
C     FOR FAN PLOTTING
C
          CHARACTER NNTT1*8,NNTT2*9,VALUE*8,NNTT3*10,VALUE1*10,B*140
C
          INTEGER FAN1,FAN2,FAN3,FAN4,FAN5,I,FANWAV,FANLAST
     1    ,COLPAS,NT1SIZ,FAN6,FAN7,FAN8,FAN9,FAN10
C
          REAL*8 WAVE1,WAVE2,WAVE3,WAVE4,WAVE5
     1    ,WAVE6,WAVE7,WAVE8,WAVE9,WAVE10
C
          COMMON/FANNER/FANWAV
C
          LOGICAL FANEXT
C
          COMMON/FANEXI/FANEXT
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          INCLUDE 'dathgr.inc'
C
          WAVE1=0.0D0
          WAVE2=0.0D0
          WAVE3=0.0D0
          WAVE4=0.0D0
          WAVE5=0.0D0
          WAVE6=0.0D0
          WAVE7=0.0D0
          WAVE8=0.0D0
          WAVE9=0.0D0
          WAVE10=0.0D0
          NT1SIZ=1
C
!      CALL MY_SETCHARASPECT(1.5,1.5)
!      CALL MY_SETFONT(1,0)
C
          FANLAST=0
C
          IF(FANEXT) THEN
              FAN2=0
              FAN3=0
              FAN4=0
              FAN5=0
              FAN6=0
              FAN7=0
              FAN8=0
              FAN9=0
              FAN10=0
              IF(FANWAV.EQ.1) THEN
                  WAVE1=SYSTEM1(1)
                  FAN1=1
              END IF
              IF(FANWAV.EQ.2) THEN
                  WAVE1=SYSTEM1(2)
                  FAN1=2
              END IF
              IF(FANWAV.EQ.3) THEN
                  WAVE1=SYSTEM1(3)
                  FAN1=3
              END IF
              IF(FANWAV.EQ.4) THEN
                  WAVE1=SYSTEM1(4)
                  FAN1=4
              END IF
              IF(FANWAV.EQ.5) THEN
                  WAVE1=SYSTEM1(5)
                  FAN1=5
              END IF
              IF(FANWAV.EQ.6) THEN
                  WAVE1=SYSTEM1(71)
                  FAN1=6
              END IF
              IF(FANWAV.EQ.7) THEN
                  WAVE1=SYSTEM1(72)
                  FAN1=7
              END IF
              IF(FANWAV.EQ.8) THEN
                  WAVE1=SYSTEM1(73)
                  FAN1=8
              END IF
              IF(FANWAV.EQ.9) THEN
                  WAVE1=SYSTEM1(74)
                  FAN1=9
              END IF
              IF(FANWAV.EQ.10) THEN
                  WAVE1=SYSTEM1(75)
                  FAN1=10
              END IF
          ELSE
              FAN1=0
              FAN2=0
              FAN3=0
              FAN4=0
              FAN5=0
              FAN6=0
              FAN7=0
              FAN8=0
              FAN9=0
              FAN10=0
C     SCAN FOR THE 10 WAVELENGTHS TO USE
              DO I=1,10
                  IF(I.EQ.1.AND.FANWV1) THEN
C     FANWV1 IS THE FIRST FAN
                      FAN1=1
                      FANLAST=FAN1
                      WAVE1=SYSTEM1(1)
                      GO TO 10
                  END IF
                  IF(I.EQ.2.AND.FANWV2) THEN
C     FANWV2 IS THE FIRST FAN
                      FAN1=2
                      FANLAST=FAN1
                      WAVE1=SYSTEM1(2)
                      GO TO 10
                  END IF
                  IF(I.EQ.3.AND.FANWV3) THEN
C     FANWV3 IS THE FIRST FAN
                      FAN1=3
                      FANLAST=FAN1
                      WAVE1=SYSTEM1(3)
                      GO TO 10
                  END IF
                  IF(I.EQ.4.AND.FANWV4) THEN
C     FANWV4 IS THE FIRST FAN
                      FAN1=4
                      FANLAST=FAN1
                      WAVE1=SYSTEM1(4)
                      GO TO 10
                  END IF
                  IF(I.EQ.5.AND.FANWV5) THEN
C     FANWV5 IS THE FIRST FAN
                      FAN1=5
                      FANLAST=FAN1
                      WAVE1=SYSTEM1(5)
                      GO TO 10
                  END IF
                  IF(I.EQ.6.AND.FANWV6) THEN
C     FANWV6 IS THE FIRST FAN
                      FAN1=6
                      FANLAST=FAN1
                      WAVE1=SYSTEM1(71)
                      GO TO 10
                  END IF
                  IF(I.EQ.7.AND.FANWV7) THEN
C     FANWV7 IS THE FIRST FAN
                      FAN1=7
                      FANLAST=FAN1
                      WAVE1=SYSTEM1(72)
                      GO TO 10
                  END IF
                  IF(I.EQ.8.AND.FANWV8) THEN
C     FANWV8 IS THE FIRST FAN
                      FAN1=8
                      FANLAST=FAN1
                      WAVE1=SYSTEM1(73)
                      GO TO 10
                  END IF
                  IF(I.EQ.9.AND.FANWV9) THEN
C     FANWV2 IS THE FIRST FAN
                      FAN1=9
                      FANLAST=FAN1
                      WAVE1=SYSTEM1(74)
                      GO TO 10
                  END IF
                  IF(I.EQ.10.AND.FANWV10) THEN
C     FANWV10 IS THE FIRST FAN
                      FAN1=10
                      FANLAST=FAN1
                      WAVE1=SYSTEM1(75)
                      GO TO 10
                  END IF
              END DO
 10           CONTINUE
              DO I=FANLAST+1,10
                  IF(I.EQ.1.AND.FANWV1) THEN
C     FANWV1 IS THE SECOND FAN
                      FAN2=1
                      FANLAST=FAN2
                      WAVE2=SYSTEM1(1)
                      GO TO 20
                  END IF
                  IF(I.EQ.2.AND.FANWV2) THEN
C     FANWV2 IS THE SECOND FAN
                      FAN2=2
                      FANLAST=FAN2
                      WAVE2=SYSTEM1(2)
                      GO TO 20
                  END IF
                  IF(I.EQ.3.AND.FANWV3) THEN
C     FANWV3 IS THE SECOND FAN
                      FAN2=3
                      FANLAST=FAN2
                      WAVE2=SYSTEM1(3)
                      GO TO 20
                  END IF
                  IF(I.EQ.4.AND.FANWV4) THEN
C     FANWV1 IS THE SECOND FAN
                      FAN2=4
                      FANLAST=FAN2
                      WAVE2=SYSTEM1(4)
                      GO TO 20
                  END IF
                  IF(I.EQ.5.AND.FANWV5) THEN
C     FANWV5 IS THE SECOND FAN
                      FAN2=5
                      FANLAST=FAN2
                      WAVE2=SYSTEM1(5)
                      GO TO 20
                  END IF
                  IF(I.EQ.6.AND.FANWV6) THEN
C     FANWV1 IS THE SECOND FAN
                      FAN2=6
                      FANLAST=FAN2
                      WAVE2=SYSTEM1(71)
                      GO TO 20
                  END IF
                  IF(I.EQ.7.AND.FANWV7) THEN
C     FANWV1 IS THE SECOND FAN
                      FAN2=7
                      FANLAST=FAN2
                      WAVE2=SYSTEM1(72)
                      GO TO 20
                  END IF
                  IF(I.EQ.8.AND.FANWV8) THEN
C     FANWV8 IS THE SECOND FAN
                      FAN2=8
                      FANLAST=FAN2
                      WAVE2=SYSTEM1(73)
                      GO TO 20
                  END IF
                  IF(I.EQ.9.AND.FANWV9) THEN
C     FANWV9 IS THE SECOND FAN
                      FAN2=9
                      FANLAST=FAN2
                      WAVE2=SYSTEM1(74)
                      GO TO 20
                  END IF
                  IF(I.EQ.10.AND.FANWV10) THEN
C     FANWV10 IS THE DECOND FAN
                      FAN2=10
                      FANLAST=FAN2
                      WAVE2=SYSTEM1(75)
                      GO TO 20
                  END IF
              END DO
 20           CONTINUE
              DO I=FANLAST+1,10
                  IF(I.EQ.1.AND.FANWV1) THEN
                      FAN3=1
                      FANLAST=FAN3
                      WAVE3=SYSTEM1(1)
                      GO TO 30
                  END IF
                  IF(I.EQ.2.AND.FANWV2) THEN
                      FAN3=2
                      FANLAST=FAN3
                      WAVE3=SYSTEM1(2)
                      GO TO 30
                  END IF
                  IF(I.EQ.3.AND.FANWV3) THEN
                      FAN3=3
                      FANLAST=FAN3
                      WAVE3=SYSTEM1(3)
                      GO TO 30
                  END IF
                  IF(I.EQ.4.AND.FANWV4) THEN
                      FAN3=4
                      FANLAST=FAN3
                      WAVE3=SYSTEM1(4)
                      GO TO 30
                  END IF
                  IF(I.EQ.5.AND.FANWV5) THEN
                      FAN3=5
                      FANLAST=FAN3
                      WAVE3=SYSTEM1(5)
                      GO TO 30
                  END IF
                  IF(I.EQ.6.AND.FANWV6) THEN
                      FAN3=6
                      FANLAST=FAN3
                      WAVE3=SYSTEM1(71)
                      GO TO 30
                  END IF
                  IF(I.EQ.7.AND.FANWV7) THEN
                      FAN3=7
                      FANLAST=FAN3
                      WAVE3=SYSTEM1(72)
                      GO TO 30
                  END IF
                  IF(I.EQ.8.AND.FANWV8) THEN
                      FAN3=8
                      FANLAST=FAN3
                      WAVE3=SYSTEM1(73)
                      GO TO 30
                  END IF
                  IF(I.EQ.9.AND.FANWV9) THEN
                      FAN3=9
                      FANLAST=FAN3
                      WAVE3=SYSTEM1(74)
                      GO TO 30
                  END IF
                  IF(I.EQ.10.AND.FANWV10) THEN
                      FAN3=10
                      FANLAST=FAN3
                      WAVE3=SYSTEM1(75)
                      GO TO 30
                  END IF
              END DO
 30           CONTINUE
              DO I=FANLAST+1,10
                  IF(I.EQ.1.AND.FANWV1) THEN
                      FAN4=1
                      FANLAST=FAN4
                      WAVE4=SYSTEM1(1)
                      GO TO 40
                  END IF
                  IF(I.EQ.2.AND.FANWV2) THEN
                      FAN4=2
                      FANLAST=FAN4
                      WAVE4=SYSTEM1(2)
                      GO TO 40
                  END IF
                  IF(I.EQ.3.AND.FANWV3) THEN
                      FAN4=3
                      FANLAST=FAN4
                      WAVE4=SYSTEM1(3)
                      GO TO 40
                  END IF
                  IF(I.EQ.4.AND.FANWV4) THEN
                      FAN4=4
                      FANLAST=FAN4
                      WAVE4=SYSTEM1(4)
                      GO TO 40
                  END IF
                  IF(I.EQ.5.AND.FANWV5) THEN
                      FAN4=5
                      FANLAST=FAN4
                      WAVE4=SYSTEM1(5)
                      GO TO 40
                  END IF
                  IF(I.EQ.6.AND.FANWV6) THEN
                      FAN4=6
                      FANLAST=FAN4
                      WAVE4=SYSTEM1(71)
                      GO TO 40
                  END IF
                  IF(I.EQ.7.AND.FANWV7) THEN
                      FAN4=7
                      FANLAST=FAN4
                      WAVE4=SYSTEM1(72)
                      GO TO 40
                  END IF
                  IF(I.EQ.8.AND.FANWV8) THEN
                      FAN4=8
                      FANLAST=FAN4
                      WAVE4=SYSTEM1(73)
                      GO TO 40
                  END IF
                  IF(I.EQ.9.AND.FANWV9) THEN
                      FAN4=9
                      FANLAST=FAN4
                      WAVE4=SYSTEM1(74)
                      GO TO 40
                  END IF
                  IF(I.EQ.10.AND.FANWV10) THEN
                      FAN4=10
                      FANLAST=FAN4
                      WAVE4=SYSTEM1(75)
                      GO TO 40
                  END IF
              END DO
 40           CONTINUE
              DO I=FANLAST+1,10
                  IF(I.EQ.1.AND.FANWV1) THEN
                      FAN5=1
                      FANLAST=FAN5
                      WAVE5=SYSTEM1(1)
                      GO TO 50
                  END IF
                  IF(I.EQ.2.AND.FANWV2) THEN
                      FAN5=2
                      FANLAST=FAN5
                      WAVE5=SYSTEM1(2)
                      GO TO 50
                  END IF
                  IF(I.EQ.3.AND.FANWV3) THEN
                      FAN5=3
                      FANLAST=FAN5
                      WAVE5=SYSTEM1(3)
                      GO TO 50
                  END IF
                  IF(I.EQ.4.AND.FANWV4) THEN
                      FAN5=4
                      FANLAST=FAN5
                      WAVE5=SYSTEM1(4)
                      GO TO 50
                  END IF
                  IF(I.EQ.5.AND.FANWV5) THEN
                      FAN5=5
                      FANLAST=FAN5
                      WAVE5=SYSTEM1(5)
                      GO TO 50
                  END IF
                  IF(I.EQ.6.AND.FANWV6) THEN
                      FAN5=6
                      FANLAST=FAN5
                      WAVE5=SYSTEM1(71)
                      GO TO 50
                  END IF
                  IF(I.EQ.7.AND.FANWV7) THEN
                      FAN5=7
                      FANLAST=FAN5
                      WAVE5=SYSTEM1(72)
                      GO TO 50
                  END IF
                  IF(I.EQ.8.AND.FANWV8) THEN
                      FAN5=8
                      FANLAST=FAN5
                      WAVE5=SYSTEM1(73)
                      GO TO 50
                  END IF
                  IF(I.EQ.9.AND.FANWV9) THEN
                      FAN5=9
                      FANLAST=FAN5
                      WAVE5=SYSTEM1(74)
                      GO TO 50
                  END IF
                  IF(I.EQ.10.AND.FANWV10) THEN
                      FAN5=10
                      FANLAST=FAN5
                      WAVE5=SYSTEM1(75)
                      GO TO 50
                  END IF
              END DO
 50           CONTINUE
              DO I=FANLAST+1,10
                  IF(I.EQ.1.AND.FANWV1) THEN
                      FAN6=1
                      FANLAST=FAN6
                      WAVE6=SYSTEM1(1)
                      GO TO 60
                  END IF
                  IF(I.EQ.2.AND.FANWV2) THEN
                      FAN6=2
                      FANLAST=FAN6
                      WAVE6=SYSTEM1(2)
                      GO TO 60
                  END IF
                  IF(I.EQ.3.AND.FANWV3) THEN
                      FAN6=3
                      FANLAST=FAN6
                      WAVE6=SYSTEM1(3)
                      GO TO 60
                  END IF
                  IF(I.EQ.4.AND.FANWV4) THEN
                      FAN6=4
                      FANLAST=FAN6
                      WAVE6=SYSTEM1(4)
                      GO TO 60
                  END IF
                  IF(I.EQ.5.AND.FANWV5) THEN
                      FAN6=5
                      FANLAST=FAN6
                      WAVE6=SYSTEM1(5)
                      GO TO 60
                  END IF
                  IF(I.EQ.6.AND.FANWV6) THEN
                      FAN6=6
                      FANLAST=FAN6
                      WAVE6=SYSTEM1(71)
                      GO TO 60
                  END IF
                  IF(I.EQ.7.AND.FANWV7) THEN
                      FAN6=7
                      FANLAST=FAN6
                      WAVE6=SYSTEM1(72)
                      GO TO 60
                  END IF
                  IF(I.EQ.8.AND.FANWV8) THEN
                      FAN6=8
                      FANLAST=FAN6
                      WAVE6=SYSTEM1(73)
                      GO TO 60
                  END IF
                  IF(I.EQ.9.AND.FANWV9) THEN
                      FAN6=9
                      FANLAST=FAN6
                      WAVE6=SYSTEM1(74)
                      GO TO 60
                  END IF
                  IF(I.EQ.10.AND.FANWV10) THEN
                      FAN6=10
                      FANLAST=FAN6
                      WAVE6=SYSTEM1(75)
                      GO TO 60
                  END IF
              END DO
 60           CONTINUE
              DO I=FANLAST+1,10
                  IF(I.EQ.1.AND.FANWV1) THEN
                      FAN7=1
                      FANLAST=FAN7
                      WAVE7=SYSTEM1(1)
                      GO TO 70
                  END IF
                  IF(I.EQ.2.AND.FANWV2) THEN
                      FAN7=2
                      FANLAST=FAN7
                      WAVE7=SYSTEM1(2)
                      GO TO 70
                  END IF
                  IF(I.EQ.3.AND.FANWV3) THEN
                      FAN7=3
                      FANLAST=FAN7
                      WAVE7=SYSTEM1(3)
                      GO TO 70
                  END IF
                  IF(I.EQ.4.AND.FANWV4) THEN
                      FAN7=4
                      FANLAST=FAN7
                      WAVE7=SYSTEM1(4)
                      GO TO 70
                  END IF
                  IF(I.EQ.5.AND.FANWV5) THEN
                      FAN7=5
                      FANLAST=FAN7
                      WAVE7=SYSTEM1(5)
                      GO TO 70
                  END IF
                  IF(I.EQ.6.AND.FANWV6) THEN
                      FAN7=6
                      FANLAST=FAN7
                      WAVE7=SYSTEM1(71)
                      GO TO 70
                  END IF
                  IF(I.EQ.7.AND.FANWV7) THEN
                      FAN7=7
                      FANLAST=FAN7
                      WAVE7=SYSTEM1(72)
                      GO TO 70
                  END IF
                  IF(I.EQ.8.AND.FANWV8) THEN
                      FAN7=8
                      FANLAST=FAN7
                      WAVE7=SYSTEM1(73)
                      GO TO 70
                  END IF
                  IF(I.EQ.9.AND.FANWV9) THEN
                      FAN7=9
                      FANLAST=FAN7
                      WAVE7=SYSTEM1(74)
                      GO TO 70
                  END IF
                  IF(I.EQ.10.AND.FANWV10) THEN
                      FAN7=10
                      FANLAST=FAN7
                      WAVE7=SYSTEM1(75)
                      GO TO 70
                  END IF
              END DO
 70           CONTINUE
              DO I=FANLAST+1,10
                  IF(I.EQ.1.AND.FANWV1) THEN
                      FAN8=1
                      FANLAST=FAN8
                      WAVE8=SYSTEM1(1)
                      GO TO 80
                  END IF
                  IF(I.EQ.2.AND.FANWV2) THEN
                      FAN8=2
                      FANLAST=FAN8
                      WAVE8=SYSTEM1(2)
                      GO TO 80
                  END IF
                  IF(I.EQ.3.AND.FANWV3) THEN
                      FAN8=3
                      FANLAST=FAN8
                      WAVE8=SYSTEM1(3)
                      GO TO 80
                  END IF
                  IF(I.EQ.4.AND.FANWV4) THEN
                      FAN8=4
                      FANLAST=FAN8
                      WAVE8=SYSTEM1(4)
                      GO TO 80
                  END IF
                  IF(I.EQ.5.AND.FANWV5) THEN
                      FAN8=5
                      FANLAST=FAN8
                      WAVE8=SYSTEM1(5)
                      GO TO 80
                  END IF
                  IF(I.EQ.6.AND.FANWV6) THEN
                      FAN8=6
                      FANLAST=FAN8
                      WAVE8=SYSTEM1(71)
                      GO TO 80
                  END IF
                  IF(I.EQ.7.AND.FANWV7) THEN
                      FAN8=7
                      FANLAST=FAN8
                      WAVE8=SYSTEM1(72)
                      GO TO 80
                  END IF
                  IF(I.EQ.8.AND.FANWV8) THEN
                      FAN8=8
                      FANLAST=FAN8
                      WAVE8=SYSTEM1(73)
                      GO TO 80
                  END IF
                  IF(I.EQ.9.AND.FANWV9) THEN
                      FAN8=9
                      FANLAST=FAN8
                      WAVE8=SYSTEM1(74)
                      GO TO 80
                  END IF
                  IF(I.EQ.10.AND.FANWV10) THEN
                      FAN8=10
                      FANLAST=FAN8
                      WAVE8=SYSTEM1(75)
                      GO TO 80
                  END IF
              END DO
 80           CONTINUE
              DO I=FANLAST+1,10
                  IF(I.EQ.1.AND.FANWV1) THEN
                      FAN9=1
                      FANLAST=FAN9
                      WAVE9=SYSTEM1(1)
                      GO TO 90
                  END IF
                  IF(I.EQ.2.AND.FANWV2) THEN
                      FAN9=2
                      FANLAST=FAN9
                      WAVE9=SYSTEM1(2)
                      GO TO 90
                  END IF
                  IF(I.EQ.3.AND.FANWV3) THEN
                      FAN9=3
                      FANLAST=FAN9
                      WAVE9=SYSTEM1(3)
                      GO TO 90
                  END IF
                  IF(I.EQ.4.AND.FANWV4) THEN
                      FAN9=4
                      FANLAST=FAN9
                      WAVE9=SYSTEM1(4)
                      GO TO 90
                  END IF
                  IF(I.EQ.5.AND.FANWV5) THEN
                      FAN9=5
                      FANLAST=FAN9
                      WAVE9=SYSTEM1(5)
                      GO TO 90
                  END IF
                  IF(I.EQ.6.AND.FANWV6) THEN
                      FAN9=6
                      FANLAST=FAN9
                      WAVE9=SYSTEM1(71)
                      GO TO 90
                  END IF
                  IF(I.EQ.7.AND.FANWV7) THEN
                      FAN9=7
                      FANLAST=FAN9
                      WAVE9=SYSTEM1(72)
                      GO TO 90
                  END IF
                  IF(I.EQ.8.AND.FANWV8) THEN
                      FAN9=8
                      FANLAST=FAN9
                      WAVE9=SYSTEM1(73)
                      GO TO 90
                  END IF
                  IF(I.EQ.9.AND.FANWV9) THEN
                      FAN9=9
                      FANLAST=FAN9
                      WAVE9=SYSTEM1(74)
                      GO TO 90
                  END IF
                  IF(I.EQ.10.AND.FANWV10) THEN
                      FAN9=10
                      FANLAST=FAN9
                      WAVE9=SYSTEM1(75)
                      GO TO 90
                  END IF
              END DO
 90           CONTINUE
              DO I=FANLAST+1,10
                  IF(I.EQ.1.AND.FANWV1) THEN
                      FAN10=1
                      FANLAST=FAN10
                      WAVE10=SYSTEM1(1)
                      GO TO 100
                  END IF
                  IF(I.EQ.2.AND.FANWV2) THEN
                      FAN10=2
                      FANLAST=FAN10
                      WAVE10=SYSTEM1(2)
                      GO TO 100
                  END IF
                  IF(I.EQ.3.AND.FANWV3) THEN
                      FAN10=3
                      FANLAST=FAN10
                      WAVE10=SYSTEM1(3)
                      GO TO 100
                  END IF
                  IF(I.EQ.4.AND.FANWV4) THEN
                      FAN10=4
                      FANLAST=FAN10
                      WAVE10=SYSTEM1(4)
                      GO TO 100
                  END IF
                  IF(I.EQ.5.AND.FANWV5) THEN
                      FAN10=5
                      FANLAST=FAN10
                      WAVE10=SYSTEM1(5)
                      GO TO 100
                  END IF
                  IF(I.EQ.6.AND.FANWV6) THEN
                      FAN10=6
                      FANLAST=FAN10
                      WAVE10=SYSTEM1(71)
                      GO TO 100
                  END IF
                  IF(I.EQ.7.AND.FANWV7) THEN
                      FAN10=7
                      FANLAST=FAN10
                      WAVE10=SYSTEM1(72)
                      GO TO 100
                  END IF
                  IF(I.EQ.8.AND.FANWV8) THEN
                      FAN10=8
                      FANLAST=FAN10
                      WAVE10=SYSTEM1(73)
                      GO TO 100
                  END IF
                  IF(I.EQ.9.AND.FANWV9) THEN
                      FAN10=9
                      FANLAST=FAN10
                      WAVE10=SYSTEM1(74)
                      GO TO 100
                  END IF
                  IF(I.EQ.10.AND.FANWV10) THEN
                      FAN10=10
                      FANLAST=FAN10
                      WAVE10=SYSTEM1(75)
                      GO TO 100
                  END IF
              END DO
          END IF
 100      CONTINUE

          IF(FAN5.NE.0) THEN
C     DO THE PLOTTING OF THE LEGEND FOR WAVELENGTH 5
C     DRAW THE WV5 LINE
              COLPAS=COLR5
              CALL MY_COLTYP(COLPAS)
              CALL MY_PLOT(100,410,0,4,-10,10010,-10,7010)
              CALL MY_PLOT(600,410,1,4,-10,10010,-10,7010)
              CALL MY_PLOT(700,350,0,4,-10,10010,-10,7010)
              IF(WAVE5.GT.99.9D0) THEN
                  WRITE(B,180)REAL(WAVE5)
                  READ(B,200) VALUE1
180               FORMAT(G10.4)
200               FORMAT(A10)
                  NNTT3=VALUE1
                  COLPAS=COLLBL
                  CALL MY_COLTYP(COLPAS)
                  CALL MY_SETFONT(1,0)
                  CALL MY_SETCHARASPECT(1.0,1.0)
                  CALL MY_JUSTSTRING(700,350,NNTT3(1:10),0,NT1SIZ,3)
              ELSE
                  WRITE(B,181)REAL(WAVE5)
                  READ(B,201) VALUE
181               FORMAT(F8.5)
201               FORMAT(A8)
                  NNTT1=VALUE
                  COLPAS=COLLBL
                  CALL MY_COLTYP(COLPAS)
                  CALL MY_SETFONT(1,0)
                  CALL MY_SETCHARASPECT(1.0,1.0)
                  CALL MY_JUSTSTRING(700,350,NNTT1(1:8),0,NT1SIZ,3)
              END IF
              CALL MY_SETCHARASPECT(1.5,1.5)
              CALL MY_SETFONT(2,0)
              NNTT2=" um"  ! micrometer **should be greek letter*** ???
              COLPAS=COLLBL
              CALL MY_COLTYP(COLPAS)
              CALL MY_JUSTSTRING(1250,350,NNTT2(1:3),0,NT1SIZ,3)
              CALL MY_SETFONT(1,0)
              CALL MY_SETCHARASPECT(1.0,1.0)
          END IF
          IF(FAN4.NE.0) THEN
C     DO THE PLOTTING OF THE LEGEND FOR WAVELENGTH 4
C     DRAW THE WV4 LINE
              COLPAS=COLR4
              CALL MY_COLTYP(COLPAS)
              CALL MY_PLOT(100,610,0,3,-10,10010,-10,7010)
              CALL MY_PLOT(600,610,1,3,-10,10010,-10,7010)
              CALL MY_PLOT(700,550,0,3,-10,10010,-10,7010)
              IF(WAVE4.GT.99.9D0) THEN
                  WRITE(B,180)REAL(WAVE4)
                  READ(B,200) VALUE1
                  NNTT3=VALUE1
                  COLPAS=COLLBL
                  CALL MY_COLTYP(COLPAS)
                  CALL MY_SETFONT(1,0)
                  CALL MY_SETCHARASPECT(1.0,1.0)
                  CALL MY_JUSTSTRING(700,550,NNTT3(1:10),0,NT1SIZ,3)
              ELSE
                  WRITE(B,181)REAL(WAVE4)
                  READ(B,201) VALUE
                  NNTT1=VALUE
                  COLPAS=COLLBL
                  CALL MY_COLTYP(COLPAS)
                  CALL MY_SETFONT(1,0)
                  CALL MY_SETCHARASPECT(1.0,1.0)
                  CALL MY_JUSTSTRING(700,550,NNTT1(1:8),0,NT1SIZ,3)
              END IF
              CALL MY_SETCHARASPECT(1.5,1.5)
              CALL MY_SETFONT(2,0)
              NNTT2=" um"
              COLPAS=COLLBL
              CALL MY_COLTYP(COLPAS)
              CALL MY_JUSTSTRING(1250,550,NNTT2(1:3),0,NT1SIZ,3)
              CALL MY_SETFONT(1,0)
              CALL MY_SETCHARASPECT(1.0,1.0)
          END IF
          IF(FAN3.NE.0) THEN
C     DO THE PLOTTING OF THE LEGEND FOR WAVELENGTH 3
C     DRAW THE WV3 LINE
!     Magenta legend line at bottom left
              COLPAS=COLR3
              CALL MY_COLTYP(COLPAS)
              CALL MY_PLOT(100,810,0,2,-10,10010,-10,7010)
              CALL MY_PLOT(600,810,1,2,-10,10010,-10,7010)
              CALL MY_PLOT(700,750,0,2,-10,10010,-10,7010)
              IF(WAVE3.GT.99.9D0) THEN
                  WRITE(B,180)REAL(WAVE3)
                  READ(B,200) VALUE1
                  NNTT3=VALUE1
                  COLPAS=COLLBL
                  CALL MY_COLTYP(COLPAS)
                  CALL MY_SETFONT(1,0)
                  CALL MY_SETCHARASPECT(1.0,1.0)
                  CALL MY_JUSTSTRING(700,750,NNTT3(1:10),0,NT1SIZ,3)
              ELSE
                  WRITE(B,181)REAL(WAVE3)
                  READ(B,201) VALUE
                  NNTT1=VALUE
                  COLPAS=COLLBL
                  CALL MY_COLTYP(COLPAS)
                  CALL MY_SETFONT(1,0)
                  CALL MY_SETCHARASPECT(1.0,1.0)
                  CALL MY_JUSTSTRING(700,750,NNTT1(1:8),0,NT1SIZ,3)
              END IF
              CALL MY_SETCHARASPECT(1.5,1.5)
              CALL MY_SETFONT(2,0)
              NNTT2=" um"
              COLPAS=COLLBL
              CALL MY_COLTYP(COLPAS)
              CALL MY_JUSTSTRING(1250,750,NNTT2(1:3),0,NT1SIZ,3)
              CALL MY_SETFONT(1,0)
              CALL MY_SETCHARASPECT(1.0,1.0)
          END IF
          IF(FAN2.NE.0) THEN
C     DO THE PLOTTING OF THE LEGEND FOR WAVELENGTH 2
C     DRAW THE WV2 LINE
!     Yellow legend line at bottom left
              COLPAS=COLR2
              CALL MY_COLTYP(COLPAS)
              CALL MY_PLOT(100,1010,0,1,-10,10010,-10,7010)
              CALL MY_PLOT(600,1010,1,1,-10,10010,-10,7010)
              CALL MY_PLOT(700,950,0,1,-10,10010,-10,7010)
              IF(WAVE2.GT.99.9D0) THEN
                  WRITE(B,180)REAL(WAVE2)
                  READ(B,200) VALUE1
                  NNTT3=VALUE1
                  COLPAS=COLLBL
                  CALL MY_COLTYP(COLPAS)
                  CALL MY_SETFONT(1,0)
                  CALL MY_SETCHARASPECT(1.0,1.0)
                  CALL MY_JUSTSTRING(700,950,NNTT3(1:10),0,NT1SIZ,3)
              ELSE
                  WRITE(B,181)REAL(WAVE2)
                  READ(B,201) VALUE
                  NNTT1=VALUE
                  COLPAS=COLLBL
                  CALL MY_COLTYP(COLPAS)
                  CALL MY_SETFONT(1,0)
                  CALL MY_SETCHARASPECT(1.0,1.0)
                  CALL MY_JUSTSTRING(700,950,NNTT1(1:8),0,NT1SIZ,3)
              END IF
              CALL MY_SETCHARASPECT(1.5,1.5)
              CALL MY_SETFONT(2,0)
              NNTT2=" um"
              COLPAS=COLLBL
              CALL MY_COLTYP(COLPAS)
              CALL MY_JUSTSTRING(1250,950,NNTT2(1:3),0,NT1SIZ,3)
              CALL MY_SETFONT(1,0)
              CALL MY_SETCHARASPECT(1.0,1.0)
          END IF
          IF(FAN1.NE.0) THEN
C     DO THE PLOTTING OF THE LEGEND FOR WAVELENGTH 1
C     DRAW THE WV1 LINE
!     Black legend line at bottom left
              COLPAS=COLR1
              CALL MY_COLTYP(COLPAS)
              CALL MY_PLOT(100,1210,0,0,-10,10010,-10,7010)
              CALL MY_PLOT(600,1210,1,0,-10,10010,-10,7010)
              CALL MY_PLOT(700,1150,0,0,-10,10010,-10,7010)
              IF(WAVE1.GT.99.9D0) THEN
                  WRITE(B,180) REAL(WAVE1)
                  READ(B,200) VALUE1
                  NNTT3=VALUE1
                  COLPAS=COLLBL
                  CALL MY_COLTYP(COLPAS)
                  CALL MY_SETFONT(1,0)
                  CALL MY_SETCHARASPECT(1.0,1.0)
                  CALL MY_JUSTSTRING(700,1150,NNTT3(1:10),0,NT1SIZ,3)
              ELSE
                  WRITE(B,181) REAL(WAVE1)
                  READ(B,201) VALUE
                  NNTT1=VALUE
                  COLPAS=COLLBL
                  CALL MY_COLTYP(COLPAS)
                  CALL MY_SETFONT(1,0)
                  CALL MY_SETCHARASPECT(1.0,1.0)
                  CALL MY_JUSTSTRING(700,1150,NNTT1(1:8),0,NT1SIZ,3)
              END IF
              CALL MY_SETCHARASPECT(1.5,1.5)
              CALL MY_SETFONT(2,0)
              NNTT2=" um"
              COLPAS=COLLBL
              CALL MY_COLTYP(COLPAS)
              CALL MY_JUSTSTRING(1250,1150,NNTT2(1:3),0,NT1SIZ,3)
              CALL MY_SETFONT(1,0)
              CALL MY_SETCHARASPECT(1.0,1.0)
          END IF
          IF(FAN10.NE.0) THEN
C     DO THE PLOTTING OF THE LEGEND FOR WAVELENGTH 10
C     DRAW THE WV10 LINE
              COLPAS=COLR10
              CALL MY_COLTYP(COLPAS)
              CALL MY_PLOT(1500,410,0,9,-10,10010,-10,7010)
              CALL MY_PLOT(2000,410,1,9,-10,10010,-10,7010)
              CALL MY_PLOT(2100,350,0,9,-10,10010,-10,7010)
              IF(WAVE5.GT.99.9D0) THEN
                  WRITE(B,180)REAL(WAVE10)
                  READ(B,200) VALUE1
                  NNTT3=VALUE1
                  COLPAS=COLLBL
                  CALL MY_COLTYP(COLPAS)
                  CALL MY_SETFONT(1,0)
                  CALL MY_SETCHARASPECT(1.0,1.0)
                  CALL MY_JUSTSTRING(2100,350,NNTT3(1:10),0,NT1SIZ,3)
              ELSE
                  WRITE(B,181)REAL(WAVE10)
                  READ(B,201) VALUE
                  NNTT1=VALUE
                  COLPAS=COLLBL
                  CALL MY_COLTYP(COLPAS)
                  CALL MY_SETFONT(1,0)
                  CALL MY_SETCHARASPECT(1.0,1.0)
                  CALL MY_JUSTSTRING(2100,350,NNTT1(1:8),0,NT1SIZ,3)
              END IF
              CALL MY_SETCHARASPECT(1.5,1.5)
              CALL MY_SETFONT(2,0)
              NNTT2=" um"
              COLPAS=COLLBL
              CALL MY_COLTYP(COLPAS)
              CALL MY_JUSTSTRING(2650,350,NNTT2(1:3),0,NT1SIZ,3)
              CALL MY_SETFONT(1,0)
              CALL MY_SETCHARASPECT(1.0,1.0)
          END IF
          IF(FAN9.NE.0) THEN
C     DO THE PLOTTING OF THE LEGEND FOR WAVELENGTH 9
C     DRAW THE WV9 LINE
              COLPAS=COLR9
              CALL MY_COLTYP(COLPAS)
              CALL MY_PLOT(1500,610,0,8,-10,10010,-10,7010)
              CALL MY_PLOT(2000,610,1,8,-10,10010,-10,7010)
              CALL MY_PLOT(2100,550,0,8,-10,10010,-10,7010)
              IF(WAVE4.GT.99.9D0) THEN
                  WRITE(B,180)REAL(WAVE9)
                  READ(B,200) VALUE1
                  NNTT3=VALUE1
                  COLPAS=COLLBL
                  CALL MY_COLTYP(COLPAS)
                  CALL MY_SETFONT(1,0)
                  CALL MY_SETCHARASPECT(1.0,1.0)
                  CALL MY_JUSTSTRING(2100,550,NNTT3(1:10),0,NT1SIZ,3)
              ELSE
                  WRITE(B,181)REAL(WAVE9)
                  READ(B,201) VALUE
                  NNTT1=VALUE
                  COLPAS=COLLBL
                  CALL MY_COLTYP(COLPAS)
                  CALL MY_SETFONT(1,0)
                  CALL MY_SETCHARASPECT(1.0,1.0)
                  CALL MY_JUSTSTRING(2100,550,NNTT1(1:8),0,NT1SIZ,3)
              END IF
              CALL MY_SETCHARASPECT(1.5,1.5)
              CALL MY_SETFONT(2,0)
              NNTT2=" um"
              COLPAS=COLLBL
              CALL MY_COLTYP(COLPAS)
              CALL MY_JUSTSTRING(2650,550,NNTT2(1:3),0,NT1SIZ,3)
              CALL MY_SETFONT(1,0)
              CALL MY_SETCHARASPECT(1.0,1.0)
          END IF
          IF(FAN8.NE.0) THEN
C     DO THE PLOTTING OF THE LEGEND FOR WAVELENGTH 8
C     DRAW THE WV8 LINE
              COLPAS=COLR8
              CALL MY_COLTYP(COLPAS)
              CALL MY_PLOT(1500,810,0,7,-10,10010,-10,7010)
              CALL MY_PLOT(2000,810,1,7,-10,10010,-10,7010)
              CALL MY_PLOT(2100,750,0,7,-10,10010,-10,7010)
              IF(WAVE3.GT.99.9D0) THEN
                  WRITE(B,180)REAL(WAVE8)
                  READ(B,200) VALUE1
                  NNTT3=VALUE1
                  COLPAS=COLLBL
                  CALL MY_COLTYP(COLPAS)
                  CALL MY_SETFONT(1,0)
                  CALL MY_SETCHARASPECT(1.0,1.0)
                  CALL MY_JUSTSTRING(2100,750,NNTT3(1:10),0,NT1SIZ,3)
              ELSE
                  WRITE(B,181)REAL(WAVE8)
                  READ(B,201) VALUE
                  NNTT1=VALUE
                  COLPAS=COLLBL
                  CALL MY_COLTYP(COLPAS)
                  CALL MY_SETFONT(1,0)
                  CALL MY_SETCHARASPECT(1.0,1.0)
                  CALL MY_JUSTSTRING(2100,750,NNTT1(1:8),0,NT1SIZ,3)
              END IF
              CALL MY_SETCHARASPECT(1.5,1.5)
              CALL MY_SETFONT(2,0)
              NNTT2=" um"
              COLPAS=COLLBL
              CALL MY_COLTYP(COLPAS)
              CALL MY_JUSTSTRING(2650,750,NNTT2(1:3),0,NT1SIZ,3)
              CALL MY_SETFONT(1,0)
              CALL MY_SETCHARASPECT(1.0,1.0)
          END IF
          IF(FAN7.NE.0) THEN
C     DO THE PLOTTING OF THE LEGEND FOR WAVELENGTH 7
C     DRAW THE WV7 LINE
              COLPAS=COLR7
              CALL MY_COLTYP(COLPAS)
              CALL MY_PLOT(1500,1010,0,6,-10,10010,-10,7010)
              CALL MY_PLOT(2000,1010,1,6,-10,10010,-10,7010)
              CALL MY_PLOT(2100,950,0,6,-10,10010,-10,7010)
              IF(WAVE2.GT.99.9D0) THEN
                  WRITE(B,180)REAL(WAVE7)
                  READ(B,200) VALUE1
                  NNTT3=VALUE1
                  COLPAS=COLLBL
                  CALL MY_COLTYP(COLPAS)
                  CALL MY_SETFONT(1,0)
                  CALL MY_SETCHARASPECT(1.0,1.0)
                  CALL MY_JUSTSTRING(2100,950,NNTT3(1:10),0,NT1SIZ,3)
              ELSE
                  WRITE(B,181)REAL(WAVE7)
                  READ(B,201) VALUE
                  NNTT1=VALUE
                  COLPAS=COLLBL
                  CALL MY_COLTYP(COLPAS)
                  CALL MY_SETFONT(1,0)
                  CALL MY_SETCHARASPECT(1.0,1.0)
                  CALL MY_JUSTSTRING(2100,950,NNTT1(1:8),0,NT1SIZ,3)
              END IF
              CALL MY_SETCHARASPECT(1.5,1.5)
              CALL MY_SETFONT(2,0)
              NNTT2=" um"
              COLPAS=COLLBL
              CALL MY_COLTYP(COLPAS)
              CALL MY_JUSTSTRING(2650,950,NNTT2(1:3),0,NT1SIZ,3)
              CALL MY_SETFONT(1,0)
              CALL MY_SETCHARASPECT(1.0,1.0)
          END IF
          IF(FAN6.NE.0) THEN
C     DO THE PLOTTING OF THE LEGEND FOR WAVELENGTH 6
C     DOTTED LINE BUT WITH BIG DOTS
              COLPAS=COLR6
              CALL MY_COLTYP(COLPAS)
              CALL MY_PLOT(1500,1210,0,5,-10,10010,-10,7010)
              CALL MY_PLOT(2000,1210,1,5,-10,10010,-10,7010)
              CALL MY_PLOT(2100,1150,0,5,-10,10010,-10,7010)
              IF(WAVE1.GT.99.9D0) THEN
                  WRITE(B,180) REAL(WAVE6)
                  READ(B,200) VALUE1
                  NNTT3=VALUE1
                  COLPAS=COLLBL
                  CALL MY_COLTYP(COLPAS)
                  CALL MY_SETFONT(1,0)
                  CALL MY_SETCHARASPECT(1.0,1.0)
                  CALL MY_JUSTSTRING(2100,1150,NNTT3(1:10),0,NT1SIZ,3)
              ELSE
                  WRITE(B,181) REAL(WAVE6)
                  READ(B,201) VALUE
                  NNTT1=VALUE
                  COLPAS=COLLBL
                  CALL MY_COLTYP(COLPAS)
                  CALL MY_SETFONT(1,0)
                  CALL MY_SETCHARASPECT(1.0,1.0)
                  CALL MY_JUSTSTRING(2100,1150,NNTT1(1:8),0,NT1SIZ,3)
              END IF
              CALL MY_SETCHARASPECT(1.5,1.5)
              CALL MY_SETFONT(2,0)
              NNTT2=" um"
              COLPAS=COLLBL
              CALL MY_COLTYP(COLPAS)
              CALL MY_JUSTSTRING(2650,1150,NNTT2(1:3),0,NT1SIZ,3)
              CALL MY_SETFONT(1,0)
              CALL MY_SETCHARASPECT(1.0,1.0)
          ELSE
          END IF
C
C     NOW BOX THE LEGENDS OFF
C
          COLPAS=COLFRM
          CALL MY_COLTYP(COLPAS)
C     LIFT PEN, MOVE TO TOP
          CALL MY_PLOT(0,1325,0,0,-10,10010,-10,7010)
C     DROP PEN, DRAW LINE
          CALL MY_PLOT(3000,1325,1,0,-10,10010,-10,7010)
          CALL MY_PLOT(3000,250,1,0,-10,10010,-10,7010)
          CALL MY_PLOT(0,1325,0,0,-10,10010,-10,7010)
          CALL MY_PLOT(10000,1325,1,0,-10,10010,-10,7010)
C     LIFT PEN, RETURN TO 0,0
          CALL MY_PLOT(0,0,0,0,-10,10010,-10,7010)
C
          IF(.NOT.PLEXIS) PLEXIS=.TRUE.
C
          RETURN
      END
