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

C       LENS EDITOR DIALOG HANDLER
C
      IF(COMMAND.EQ.'SURFACEEDITOR_RD') THEN
C
!        CALL WDIALOGSELECT(SURFACEEDITOR_RD)
C
          SELECT CASE(MESSAGE)
C
            CASE ('FLIP')
              !        CALL WDIALOGLOAD(FLIP)
              !        CALL WDIALOGSELECT(FLIP)
!        CALL WDIALOGPUTINTEGER(STARTSURF,1)
!        CALL WDIALOGPUTINTEGER(STOPSURF,INT(SYSTEM1(20)))
!        CALL WDialogShow(-1,-1,0,Modeless)
C
            CASE ('SCALE')
              !        CALL WDIALOGLOAD(SCALE)
              !        CALL WDIALOGSELECT(SCALE)
!        CALL WDIALOGPUTINTEGER(STARTSURF,0)
!        CALL WDIALOGPUTINTEGER(STOPSURF,INT(SYSTEM1(20)))
!        CALL WDIALOGPUTDOUBLE(FACTOR,1.0D0)
!        CALL WDialogShow(-1,-1,0,Modeless)
C
            CASE ('VIE')
              !        CALL WDIALOGLOAD(VIE)
              !        CALL WDIALOGSELECT(VIE)
!        CALL WDIALOGPUTINTEGER(STARTSURF,1)
!        CALL WDIALOGPUTINTEGER(STOPSURF,INT(SYSTEM1(20)))
!        CALL WDIALOGPUTDOUBLE(FACTOR,0.10D0)
!        CALL WDialogShow(-1,-1,0,Modeless)
C
            CASE ('ED1')
!        CALL WDIALOGLOAD(NONSURF1)
              INCLUDE 'NONSURF1FRESH.INC'
C
            CASE ('ED2')
!        CALL WDIALOGLOAD(NONSURF2)
              INCLUDE 'NONSURF2FRESH.INC'
C
            CASE ('UNDONEWLENS')
C       MAKE LASTCURLENS.DAT INTO LENSTEXT.DAT
              INPUT='OUT NULL'
              CALL PROCES
              INPUT='IN FILE CURLENS\LASTCURLENS.DAT'
              CALL PROCES
              INPUT='OUT FILE CURLENS\LENSTEXT.DAT'
              CALL PROCES
              INPUT='OUT TP'
              CALL PROCES
              INPUT='IN TP'
              CALL PROCES
C       RELOAD DIALOG

C       DUMP CURRENT LENS TO LENSBAK.DAT SO WE CAN UNDO
              INPUT='OUT FILE CURLENS\LENSBACK.DAT'
              CALL PROCES
              INPUT='LENO'
              CALL PROCES
              INPUT='OUT TP'
              CALL PROCES
              INPUT='IN TP'
              CALL PROCES
C
              INCLUDE 'LOADSHEET.INC'
C
              WRITE(OUTLYNE,*) 'ALL SESSION CHANGES HAVE BEEN "UNDONE".'
              CALL SHOWIT(1)

            CASE ('UNDO')
C       MAKE LENSBACK.DAT INTO LENSTEXT.DAT
              INPUT='OUT NULL'
              CALL PROCES
              INPUT='IN FILE CURLENS\LENSBACK.DAT'
              CALL PROCES
              INPUT='OUT FILE CURLENS\LENSTEXT.DAT'
              CALL PROCES
              INPUT='OUT TP'
              CALL PROCES
              INPUT='IN TP'
              CALL PROCES
C       RELOAD DIALOG

C       DUMP CURRENT LENS TO LENSBAK.DAT SO WE CAN UNDO
              INPUT='OUT FILE CURLENS\LENSBACK.DAT'
              CALL PROCES
              INPUT='LENO'
              CALL PROCES
              INPUT='OUT TP'
              CALL PROCES
              INPUT='IN TP'
              CALL PROCES
C
              INCLUDE 'LOADSHEET.INC'
C
              WRITE(OUTLYNE,*) 'ALL SESSION CHANGES HAVE BEEN "UNDONE".'
              CALL SHOWIT(1)
C
            CASE ('INS1')

C       INSERT A SURFACE INFRONT OF CURRENT SURFACE
!        CALL WDIALOGGETINTEGER(SURFTOBECUR,ISET)
              IF(ISET.EQ.0) THEN
                  WRITE(OUTLYNE,*)
     1            'SURFACE INSERTION INFRONT OF THE OBJECT SURFACE IS NOT ALLOWED'
                  CALL SHOWIT(1)
              END IF
C               INPUT='OUT NULL'
C               CALL PROCES
              INPUT='U L'
              CALL PROCES
              WRITE(INPUT,*) 'CHG,',ISET
              CALL PROCES
              INPUT='INS'
              CALL PROCES
              INPUT='EOS'
              CALL PROCES
              INPUT='OUT TP'
              CALL PROCES
C       RELOAD LENS INTO SPREAD SHEET
C
              INCLUDE 'LOADSHEET.INC'
C
            CASE ('INS2')
C       INSERT A SURFACE AFTER THE CURRENT SURFACE
!        CALL WDIALOGGETINTEGER(SURFTOBECUR,ISET)
              IF((ISET).EQ.INT(SYSTEM1(20))) THEN
                  WRITE(OUTLYNE,*)
     1            'SURFACE INSERTION AFTER OF THE IMAGE SURFACE IS NOT ALLOWED'
                  CALL SHOWIT(1)
              END IF
              INPUT='OUT NULL'
              CALL PROCES
              INPUT='U L'
              CALL PROCES
              WRITE(INPUT,*) 'CHG,',ISET+1
              CALL PROCES
              INPUT='INS'
              CALL PROCES
              INPUT='EOS'
              CALL PROCES
              INPUT='OUT TP'
              CALL PROCES
C       RELOAD LENS INTO SPREAD SHEET
C
              INCLUDE 'LOADSHEET.INC'
C
            CASE ('DEL')
C       DELETE CURRENT SURFACE
!        CALL WDIALOGGETINTEGER(SURFTOBECUR,ISET)

              IF(ISET.EQ.0) THEN
                  WRITE(OUTLYNE,*)
     1            'OBJECT SURFACE MAY NOT BE DELETED'
                  CALL SHOWIT(1)
              END IF
              IF(ISET.EQ.INT(SYSTEM1(20))) THEN
                  WRITE(OUTLYNE,*)
     1            'IMAGE SURFACE MAY NOT BE DELETED'
                  CALL SHOWIT(1)
              END IF
              IF(INT(SYSTEM1(20)).LE.3) THEN
                  WRITE(OUTLYNE,*)
     1            'LENS IS OF MINIMUM SIZE, NO MORE SURFACES MAY BE DELETED'
                  CALL SHOWIT(1)
              END IF
              INPUT='OUT NULL'
              CALL PROCES
              INPUT='U L'
              CALL PROCES
              WRITE(INPUT,*) 'CHG,',ISET
              CALL PROCES
              INPUT='DEL'
              CALL PROCES
              INPUT='EOS'
              CALL PROCES
              INPUT='OUT TP'
              CALL PROCES
C       RELOAD LENS INTO SPREAD SHEET
C
              INCLUDE 'LOADSHEET.INC'
C
            CASE ('IDOK')
              EDITOROPEN=.FALSE.
!        CALL WDialogHide()
!        CALL WDialogUnload()
C
            CASE('IDCANCEL')
C       MAKE LENSBACK.DAT INTO LENSTEXT.DAT
              INPUT='OUT NULL'
              CALL PROCES
              INPUT='IN FILE CURLENS\LENSBACK.DAT'
              CALL PROCES
              INPUT='OUT FILE CURLENS\LENSTEXT.DAT'
              CALL PROCES
              INPUT='OUT TP'
              CALL PROCES
              INPUT='IN TP'
              CALL PROCES
              EDITOROPEN=.FALSE.
!        CALL WDialogHide()
!        CALL WDialogUnload()
          END SELECT
      END IF

      IF(COMMAND.EQ.'NONSURF1') THEN
C
!        CALL WDIALOGSELECT(NONSURF1)
C
          SELECT CASE(MESSAGE)
C
            CASE ('B1')
              SYSTEM1(60)=1.0D0
              SYSTEM1(61)=1.0D0
              SYSTEM1(18)=0.0D0
              SYSTEM1(94)=0.0D0
              SYSTEM1(95)=0.0D0
              SYSTEM1(98)=0.0D0
              SYSTEM1(99)=0.0D0
              INCLUDE 'NONSURF3FRESH.INC'
            CASE ('B2')
              SYSTEM1(60)=1.0D0
              SYSTEM1(61)=1.0D0
              SYSTEM1(18)=1.0D0
              SYSTEM1(94)=0.0D0
              SYSTEM1(95)=0.0D0
              SYSTEM1(98)=0.0D0
              SYSTEM1(99)=0.0D0
              INCLUDE 'NONSURF3FRESH.INC'
            CASE ('B3')
              SYSTEM1(60)=0.0D0
              SYSTEM1(61)=0.0D0
              SYSTEM1(94)=-1.0D0
              SYSTEM1(95)=-1.0D0
              SYSTEM1(98)=0.0D0
              SYSTEM1(99)=0.0D0
              INCLUDE 'NONSURF3FRESH.INC'
            CASE ('B4')
              SYSTEM1(60)=0.0D0
              SYSTEM1(61)=0.0D0
              SYSTEM1(94)=-1.0D0
              SYSTEM1(95)=-1.0D0
              SYSTEM1(98)=0.0D0
              SYSTEM1(99)=0.0D0
              INCLUDE 'NONSURF3FRESH.INC'
            CASE ('B5')
              SYSTEM1(60)=0.0D0
              SYSTEM1(61)=0.0D0
              SYSTEM1(98)=-1.0D0
              SYSTEM1(99)=-1.0D0
              SYSTEM1(94)=0.0D0
              SYSTEM1(95)=0.0D0
              INCLUDE 'NONSURF3FRESH.INC'
            CASE ('B6')
              SYSTEM1(60)=0.0D0
              SYSTEM1(61)=0.0D0
              SYSTEM1(98)=1.0D0
              SYSTEM1(99)=1.0D0
              SYSTEM1(94)=0.0D0
              SYSTEM1(95)=0.0D0
              INCLUDE 'NONSURF3FRESH.INC'
C
            CASE ('IDOK')
C       LOAD UP VALUES
C
C       CW,PWP,SWP
!        CALL WDIALOGGETINTEGER(CW,ISET)
              CALL DTOA23(DBLE(ISET),AW1)
              INPUT='U L'
              CALL PROCES
              INPUT='CW,'//AW1
              CALL PROCES
!        CALL WDIALOGGETINTEGER(PWP1,ISET)
!        CALL WDIALOGGETINTEGER(PWP2,JSET)
              CALL DTOA23(DBLE(ISET),AW1)
              CALL DTOA23(DBLE(JSET),AW2)
              INPUT='PCW,'//AW1//','//AW2
              CALL PROCES
!        CALL WDIALOGGETINTEGER(SWP1,ISET)
!        CALL WDIALOGGETINTEGER(SWP2,JSET)
              CALL DTOA23(DBLE(ISET),AW1)
              CALL DTOA23(DBLE(JSET),AW2)
              INPUT='SCW,'//AW1//','//AW2
              CALL PROCES
!        CALL WDIALOGGETDOUBLE(WV1,DW1)
!        CALL WDIALOGGETDOUBLE(WV2,DW2)
!        CALL WDIALOGGETDOUBLE(WV3,DW3)
!        CALL WDIALOGGETDOUBLE(WV4,DW4)
!        CALL WDIALOGGETDOUBLE(WV5,DW5)
              CALL DTOA23(DW1,AW1)
              CALL DTOA23(DW2,AW2)
              CALL DTOA23(DW3,AW3)
              CALL DTOA23(DW4,AW4)
              CALL DTOA23(DW5,AW5)
              INPUT='WV,'//AW1//','//AW2//','//AW3//','//AW4//','//AW5
              CALL PROCES
!        CALL WDIALOGGETDOUBLE(WV6,DW1)
!        CALL WDIALOGGETDOUBLE(WV7,DW2)
!        CALL WDIALOGGETDOUBLE(WV8,DW3)
!        CALL WDIALOGGETDOUBLE(WV9,DW4)
!        CALL WDIALOGGETDOUBLE(WV10,DW5)
              CALL DTOA23(DW1,AW1)
              CALL DTOA23(DW2,AW2)
              CALL DTOA23(DW3,AW3)
              CALL DTOA23(DW4,AW4)
              CALL DTOA23(DW5,AW5)
              INPUT='WV2,'//AW1//','//AW2//','//AW3//','//AW4//','//AW5
              CALL PROCES
!        CALL WDIALOGGETDOUBLE(SPTWT1,DW1)
!        CALL WDIALOGGETDOUBLE(SPTWT2,DW2)
!        CALL WDIALOGGETDOUBLE(SPTWT3,DW3)
!        CALL WDIALOGGETDOUBLE(SPTWT4,DW4)
!        CALL WDIALOGGETDOUBLE(SPTWT5,DW5)
              CALL DTOA23(DW1,AW1)
              CALL DTOA23(DW2,AW2)
              CALL DTOA23(DW3,AW3)
              CALL DTOA23(DW4,AW4)
              CALL DTOA23(DW5,AW5)
              INPUT='SPTWT,'//AW1//','//AW2//','//AW3//','//AW4//','//AW5
              CALL PROCES
!        CALL WDIALOGGETDOUBLE(SPTWT6,DW1)
!        CALL WDIALOGGETDOUBLE(SPTWT7,DW2)
!        CALL WDIALOGGETDOUBLE(SPTWT8,DW3)
!        CALL WDIALOGGETDOUBLE(SPTWT9,DW4)
!        CALL WDIALOGGETDOUBLE(SPTWT10,DW5)
              CALL DTOA23(DW1,AW1)
              CALL DTOA23(DW2,AW2)
              CALL DTOA23(DW3,AW3)
              CALL DTOA23(DW4,AW4)
              CALL DTOA23(DW5,AW5)
              INPUT='SPTWT2,'//AW1//','//AW2//','//AW3//','//AW4//','//AW5
              CALL PROCES
C
C       UNITS
C
!        CALL WDIALOGGETRADIOBUTTON(UNITS1,ISET)
              IF(ISET.EQ.1) INPUT='UNITS IN'
              IF(ISET.EQ.2) INPUT='UNITS CM'
              IF(ISET.EQ.3) INPUT='UNITS MM'
              IF(ISET.EQ.4) INPUT='UNITS M'
              CALL PROCES
C
C       LI
!        CALL WDIALOGGETSTRING(LI,LI)
              IF(LI(1:20).NE.AA) THEN
                  INPUT='LI '//LI
                  CALL PROCES
              END IF
C
C       LIC1
!        CALL WDIALOGGETSTRING(LIC1,LIC(1))
!        CALL WDIALOGGETSTRING(LIC2,LIC(2))
!        CALL WDIALOGGETSTRING(LIC3,LIC(3))
!        CALL WDIALOGGETSTRING(LIC4,LIC(4))
              IF(LIC(1)(1:20).NE.AA.OR.LIC(2)(1:20).NE.AA.OR.
     1        LIC(3)(1:20).NE.AA.OR.LIC(4)(1:20).NE.AA) THEN
                  IF(LIC(1)(1:20).NE.AA) THEN
                      INPUT='LIC '//LIC(1)
                      CALL PROCES
                  END IF
                  IF(LIC(2)(1:20).NE.AA) THEN
                      INPUT='LIC '//LIC(2)
                      CALL PROCES
                  END IF
                  IF(LIC(3)(1:20).NE.AA) THEN
                      INPUT='LIC '//LIC(3)
                      CALL PROCES
                  END IF
                  IF(LIC(4)(1:20).NE.AA) THEN
                      INPUT='LIC '//LIC(4)
                      CALL PROCES
                  END IF
              END IF
C       INI
!        CALL WDIALOGGETSTRING(INI,INNI)
              IF(INNI(1:20).NE.AA) THEN
                  INPUT='INI '//INNI
                  CALL PROCES
              END IF
C
C       APERTURES
C
!        CALL WDIALOGGETCHECKBOX(SAMEAPE,ISET)
!        CALL WDIALOGGETRADIOBUTTON(SAY1,JSET)
C
C       NAO
              IF(JSET.EQ.2) THEN
!        CALL WDIALOGGETDOUBLE(X,DW1)
!        CALL WDIALOGGETDOUBLE(Y,DW2)
                  CALL DTOA23(DW1,AW1)
                  CALL DTOA23(DW2,AW2)
                  IF(ISET.EQ.1) THEN
                      INPUT='NAOY,'//AW2
                      CALL PROCES
                      INPUT='NAOX,'//AW2
                      CALL PROCES
                  ELSE
                      INPUT='NAOY,'//AW2
                      CALL PROCES
                      INPUT='NAOX,'//AW1
                      CALL PROCES
                  END IF
              END IF
C
C       FNO
              IF(JSET.EQ.3) THEN
!        CALL WDIALOGGETDOUBLE(X,DW1)
!        CALL WDIALOGGETDOUBLE(Y,DW2)
                  CALL DTOA23(DW1,AW1)
                  CALL DTOA23(DW2,AW2)
                  IF(ISET.EQ.1) THEN
                      INPUT='FNOY,'//AW2
                      CALL PROCES
                      INPUT='FNOX,'//AW2
                      CALL PROCES
                  ELSE
                      INPUT='FNOY,'//AW2
                      CALL PROCES
                      INPUT='FNOX,'//AW1
                      CALL PROCES
                  END IF
              END IF

              IF(JSET.EQ.4) THEN
C       SAY/SAX FLOAT
                  IF(ISET.EQ.1) THEN
                      INPUT='SAY FLOAT'
                      CALL PROCES
                  ELSE
                      INPUT='SAY FLOAT'
                      CALL PROCES
                      INPUT='SAX FLOAT'
                      CALL PROCES
                  END IF
              END IF
              IF(JSET.EQ.1) THEN
C       SAY/SAX
!        CALL WDIALOGGETDOUBLE(X,DW1)
!        CALL WDIALOGGETDOUBLE(Y,DW2)
                  DW1=DW1/2.0D0
                  DW2=DW2/2.0D0
                  CALL DTOA23(DW1,AW1)
                  CALL DTOA23(DW2,AW2)
                  IF(ISET.EQ.1) THEN
                      INPUT='SAY,'//AW2
                      CALL PROCES
                      INPUT='SAX,'//AW2
                      CALL PROCES
                  ELSE
                      INPUT='SAY,'//AW2
                      CALL PROCES
                      INPUT='SAX,'//AW1
                      CALL PROCES
                  END IF
              END IF
C
C       FIELDS
C
!        CALL WDIALOGGETCHECKBOX(SAMEFIELD,ISET)
!        CALL WDIALOGGETRADIOBUTTON(F1,JSET)
C
              IF(JSET.EQ.1) THEN
C       OBJ HT
!        CALL WDIALOGGETDOUBLE(XREF,DW1)
!        CALL WDIALOGGETDOUBLE(YREF,DW2)
                  CALL DTOA23(DW1,AW1)
                  CALL DTOA23(DW2,AW2)
                  IF(ISET.EQ.1) THEN
                      INPUT='SCY,'//AW2
                      CALL PROCES
                      INPUT='SCX,'//AW2
                      CALL PROCES
                  ELSE
                      INPUT='SCY,'//AW2
                      CALL PROCES
                      INPUT='SCX,'//AW1
                      CALL PROCES
                  END IF
              END IF
C
              IF(JSET.EQ.2) THEN
C       OBJ ANG
!        CALL WDIALOGGETDOUBLE(XREF,DW1)
!        CALL WDIALOGGETDOUBLE(YREF,DW2)
                  CALL DTOA23(DW1,AW1)
                  CALL DTOA23(DW2,AW2)
                  IF(ISET.EQ.1) THEN
                      INPUT='SCY FANG,'//AW2
                      CALL PROCES
                      INPUT='SCX FANG,'//AW2
                      CALL PROCES
                  ELSE
                      INPUT='SCY FANG,'//AW2
                      CALL PROCES
                      INPUT='SCX FANG,'//AW1
                      CALL PROCES
                  END IF
              END IF
C
              IF(JSET.EQ.3) THEN
C       PIM HT
!        CALL WDIALOGGETDOUBLE(XREF,DW1)
!        CALL WDIALOGGETDOUBLE(YREF,DW2)
                  CALL DTOA23(DW1,AW1)
                  CALL DTOA23(DW2,AW2)
                  IF(ISET.EQ.1) THEN
                      INPUT='PYIM,'//AW2
                      CALL PROCES
                      INPUT='PXIM,'//AW2
                      CALL PROCES
                  ELSE
                      INPUT='PYIM,'//AW2
                      CALL PROCES
                      INPUT='PXIM,'//AW1
                      CALL PROCES
                  END IF
              END IF
C
              IF(JSET.EQ.4) THEN
C       PIM ANG
!        CALL WDIALOGGETDOUBLE(XREF,DW1)
!        CALL WDIALOGGETDOUBLE(YREF,DW2)
                  CALL DTOA23(DW1,AW1)
                  CALL DTOA23(DW2,AW2)
                  IF(ISET.EQ.1) THEN
                      INPUT='PYIM FANG,'//AW2
                      CALL PROCES
                      INPUT='PXIM FANG,'//AW2
                      CALL PROCES
                  ELSE
                      INPUT='PYIM FANG,'//AW2
                      CALL PROCES
                      INPUT='PXIM FANG,'//AW1
                      CALL PROCES
                  END IF
              END IF
C
              IF(JSET.EQ.5) THEN
C       RIM HT
!        CALL WDIALOGGETDOUBLE(XREF,DW1)
!        CALL WDIALOGGETDOUBLE(YREF,DW2)
                  CALL DTOA23(DW1,AW1)
                  CALL DTOA23(DW2,AW2)
                  IF(ISET.EQ.1) THEN
                      INPUT='RYIM,'//AW2
                      CALL PROCES
                      INPUT='RXIM,'//AW2
                      CALL PROCES
                  ELSE
                      INPUT='RYIM,'//AW2
                      CALL PROCES
                      INPUT='RXIM,'//AW1
                      CALL PROCES
                  END IF
              END IF
C
              IF(JSET.EQ.6) THEN
C       RIM ANG
!        CALL WDIALOGGETDOUBLE(XREF,DW1)
!        CALL WDIALOGGETDOUBLE(YREF,DW2)
                  CALL DTOA23(DW1,AW1)
                  CALL DTOA23(DW2,AW2)
                  IF(ISET.EQ.1) THEN
                      INPUT='RYIM FANG,'//AW2
                      CALL PROCES
                      INPUT='RXIM FANG,'//AW2
                      CALL PROCES
                  ELSE
                      INPUT='RYIM FANG,'//AW2
                      CALL PROCES
                      INPUT='RXIM FANG,'//AW1
                      CALL PROCES
                  END IF
              END IF
C
              INPUT='EOS'
              CALL PROCES
C
C       LENS MODE
!        CALL WDIALOGGETRADIOBUTTON(MODE1,ISET)
              IF(ISET.EQ.1) INPUT='MODE FOCAL'
              IF(ISET.EQ.3) INPUT='MODE UFOCAL'
              IF(ISET.EQ.2) INPUT='MODE AFOCAL'
              IF(ISET.EQ.4) INPUT='MODE UAFOCAL'
              CALL PROCES
C
C       AIMING
!        CALL WDIALOGGETRADIOBUTTON(AIM1,ISET)
              IF(ISET.EQ.1) INPUT='AIMRAY ON'
              IF(ISET.EQ.2) INPUT='AIMRAY OFF'
              IF(ISET.EQ.3) INPUT='TEL ON'
              IF(ISET.EQ.4) INPUT='AIMAPL ON'
              CALL PROCES
C
!        CALL WDialogHide()
!        CALL WDialogUnload()


            CASE ('IDCANCEL')
!        CALL WDialogHide()
!        CALL WDialogUnload()

          END SELECT
      END IF
C
      IF(COMMAND.EQ.'NONSURF2') THEN
C
!        CALL WDIALOGSELECT(NONSURF2)
C
          SELECT CASE(MESSAGE)
C
            CASE ('MIDEAL')
C
C       IDEAL
C
!        CALL WDIALOGPUTCHECKBOX(IDEAL,1)
!        CALL WDIALOGPUTCHECKBOX(PERFECT,0)
!        CALL WDIALOGCLEARFIELD(IDEALEFL)
!        CALL WDIALOGFIELDSTATE(IDEALEFL,1)
              IF(ALENS(121,INT(SYSTEM1(20))-1).EQ.0.0D0) THEN
                  ALENS(121,INT(SYSTEM1(20))-1)=ALENS(3,INT(SYSTEM1(20))-1)
              END IF
              IF(ALENS(121,INT(SYSTEM1(20))-1).EQ.0.0D0)
     1        ALENS(121,INT(SYSTEM1(20))-1)=1.0D0
              GLANAM(INT(SYSTEM1(20)-1),2)='AIR          '
              INCLUDE 'NONSURF2FRESH.INC'
C
            CASE ('MPERFECT')
C
C       PERFECT
C
!        CALL WDIALOGPUTCHECKBOX(IDEAL,0)
!        CALL WDIALOGPUTCHECKBOX(PERFECT,1)
!        CALL WDIALOGCLEARFIELD(IDEALEFL)
!        CALL WDIALOGFIELDSTATE(IDEALEFL,0)
              ALENS(121,INT(SYSTEM1(20))-1)=0.0D0
              GLANAM(INT(SYSTEM1(20)-1),2)='PERFECT      '
              INCLUDE 'NONSURF2FRESH.INC'
C
            CASE ('IDOK')
C
              INPUT='U L'
              CALL PROCES
C       WRX,WRY,BDX,BDY
!        CALL WDIALOGGETDOUBLE(WRX,DW1)
              CALL DTOA23(DW1,AW1)
              INPUT='WRX,'//AW1
              CALL PROCES
!        CALL WDIALOGGETDOUBLE(WRY,DW1)
              CALL DTOA23(DW1,AW1)
              INPUT='WRY,'//AW1
              CALL PROCES
!        CALL WDIALOGGETDOUBLE(BDX,DW1)
              CALL DTOA23(DW1,AW1)
              INPUT='BDX,'//AW1
              CALL PROCES
!        CALL WDIALOGGETDOUBLE(BDX,DW1)
              CALL DTOA23(DW1,AW1)
              INPUT='BDY,'//AW1
              CALL PROCES
C
C       AUTOFUNC
C
!        CALL WDIALOGGETCHECKBOX(AUTOFUNC,ISET)
!        CALL WDIALOGGETINTEGER(FUNCNUM,JSET)
              IF(ISET.EQ.1) THEN
                  IF(JSET.GE.1.AND.JSET.LE.10) THEN
                      CALL ITOAA(JSET,AJ)
                      INPUT='AUTOFUNC,'//AJ
                  END IF
              END IF
C
C       IDEAL
C
!        CALL WDIALOGGETCHECKBOX(IDEAL,ISET)
              IF(ISET.EQ.1) THEN
!        CALL WDIALOGGETDOUBLE(IDEALEFL,DW1)
                  IF(DW1.EQ.0.0D0)
     1            DW1=ALENS(3,INT(SYSTEM1(20))-1)
                  IF(DW1.NE.0.0D0) THEN
                      WRITE(INPUT,*) 'CHG,',INT(SYSTEM1(20))-1
                      CALL PROCES
                      WRITE(INPUT,*) 'IDEAL ,',DW1
                      CALL PROCES
                  END IF
                  INCLUDE 'LOADSHEET.INC'
                  INCLUDE 'NONSURF2FRESH.INC'
              END IF
C
C       PERFECT
C
!        CALL WDIALOGGETCHECKBOX(PERFECT,ISET)
              IF(ISET.EQ.1) THEN
                  WRITE(INPUT,*) 'CHG,',INT(SYSTEM1(20))-1
                  CALL PROCES
                  WRITE(INPUT,*) 'PERFECT'
                  CALL PROCES
                  ALENS(121,INT(SYSTEM1(20))-1)=0.0D0
                  INCLUDE 'LOADSHEET.INC'
                  INCLUDE 'NONSURF2FRESH.INC'
              END IF
C
              INPUT='EOS'
              CALL PROCES
C
C       MULTI-FIELDS
C
!        CALL WDIALOGGETINTEGER(MAXFLDS,ISET)
              CALL ITOAA(ISET,AJ)
              INPUT='FLDS MAX,'//AJ
              CALL PROCES
              IF(ISET.GE.1) THEN
!        CALL WDIALOGGETDOUBLE(X1,DW1)
!        CALL WDIALOGGETDOUBLE(Y1,DW2)
                  CALL DTOA23(DW1,AW1)
                  CALL DTOA23(DW2,AW2)
                  CALL ITOAA(1,AJ)
                  INPUT='FLDS,'//AJ//','//AW1//','//AW2
                  CALL PROCES
              END IF
              IF(ISET.GE.2) THEN
!        CALL WDIALOGGETDOUBLE(X2,DW1)
!        CALL WDIALOGGETDOUBLE(Y2,DW2)
                  CALL DTOA23(DW1,AW1)
                  CALL DTOA23(DW2,AW2)
                  CALL ITOAA(2,AJ)
                  INPUT='FLDS,'//AJ//','//AW1//','//AW2
                  CALL PROCES
              END IF
              IF(ISET.GE.3) THEN
!        CALL WDIALOGGETDOUBLE(X3,DW1)
!        CALL WDIALOGGETDOUBLE(Y3,DW2)
                  CALL DTOA23(DW1,AW1)
                  CALL DTOA23(DW2,AW2)
                  CALL ITOAA(3,AJ)
                  INPUT='FLDS,'//AJ//','//AW1//','//AW2
                  CALL PROCES
              END IF
              IF(ISET.GE.4) THEN
!        CALL WDIALOGGETDOUBLE(X4,DW1)
!        CALL WDIALOGGETDOUBLE(Y4,DW2)
                  CALL DTOA23(DW1,AW1)
                  CALL DTOA23(DW2,AW2)
                  CALL ITOAA(4,AJ)
                  INPUT='FLDS,'//AJ//','//AW1//','//AW2
                  CALL PROCES
              END IF
              IF(ISET.GE.5) THEN
!        CALL WDIALOGGETDOUBLE(X5,DW1)
!        CALL WDIALOGGETDOUBLE(Y5,DW2)
                  CALL DTOA23(DW1,AW1)
                  CALL DTOA23(DW2,AW2)
                  CALL ITOAA(5,AJ)
                  INPUT='FLDS,'//AJ//','//AW1//','//AW2
                  CALL PROCES
              END IF
              IF(ISET.GE.6) THEN
!        CALL WDIALOGGETDOUBLE(X6,DW1)
!        CALL WDIALOGGETDOUBLE(Y6,DW2)
                  CALL DTOA23(DW1,AW1)
                  CALL DTOA23(DW2,AW2)
                  CALL ITOAA(6,AJ)
                  INPUT='FLDS,'//AJ//','//AW1//','//AW2
                  CALL PROCES
              END IF
              IF(ISET.GE.7) THEN
!        CALL WDIALOGGETDOUBLE(X7,DW1)
!        CALL WDIALOGGETDOUBLE(Y7,DW2)
                  CALL DTOA23(DW1,AW1)
                  CALL DTOA23(DW2,AW2)
                  CALL ITOAA(7,AJ)
                  INPUT='FLDS,'//AJ//','//AW1//','//AW2
                  CALL PROCES
              END IF
              IF(ISET.GE.8) THEN
!        CALL WDIALOGGETDOUBLE(X8,DW1)
!        CALL WDIALOGGETDOUBLE(Y8,DW2)
                  CALL DTOA23(DW1,AW1)
                  CALL DTOA23(DW2,AW2)
                  CALL ITOAA(8,AJ)
                  INPUT='FLDS,'//AJ//','//AW1//','//AW2
                  CALL PROCES
              END IF
              IF(ISET.GE.9) THEN
!        CALL WDIALOGGETDOUBLE(X9,DW1)
!        CALL WDIALOGGETDOUBLE(Y9,DW2)
                  CALL DTOA23(DW1,AW1)
                  CALL DTOA23(DW2,AW2)
                  CALL ITOAA(9,AJ)
                  INPUT='FLDS,'//AJ//','//AW1//','//AW2
                  CALL PROCES
              END IF
              IF(ISET.GE.10) THEN
!        CALL WDIALOGGETDOUBLE(X10,DW1)
!        CALL WDIALOGGETDOUBLE(Y10,DW2)
                  CALL DTOA23(DW1,AW1)
                  CALL DTOA23(DW2,AW2)
                  CALL ITOAA(10,AJ)
                  INPUT='FLDS,'//AJ//','//AW1//','//AW2
                  CALL PROCES
              END IF
!        CALL WDialogHide()
!        CALL WDialogUnload()

            CASE ('TEM00X')
              INPUT='U L'
              CALL PROCES
              INPUT='BDX TEM00'
              CALL PROCES
              INPUT='EOS'
              CALL PROCES
              INCLUDE 'NONSURF2FRESH.INC'
C
            CASE ('TEM00Y')
              INPUT='U L'
              CALL PROCES
              INPUT='BDY TEM00'
              CALL PROCES
              INPUT='EOS'
              CALL PROCES
              INCLUDE 'NONSURF2FRESH.INC'
C
            CASE ('MAX1')
              INPUT='FLDS MAX, 1'
              CALL PROCES
              INCLUDE 'NONSURF2FRESH.INC'
            CASE ('MAX2')
              INPUT='FLDS MAX, 2'
              CALL PROCES
              INCLUDE 'NONSURF2FRESH.INC'
            CASE ('MAX3')
              INPUT='FLDS MAX, 3'
              CALL PROCES
              INCLUDE 'NONSURF2FRESH.INC'
            CASE ('MAX4')
              INPUT='FLDS MAX, 4'
              CALL PROCES
              INCLUDE 'NONSURF2FRESH.INC'
            CASE ('MAX5')
              INPUT='FLDS MAX, 5'
              CALL PROCES
              INCLUDE 'NONSURF2FRESH.INC'
            CASE ('MAX6')
              INPUT='FLDS MAX, 6'
              CALL PROCES
              INCLUDE 'NONSURF2FRESH.INC'
            CASE ('MAX7')
              INPUT='FLDS MAX, 7'
              CALL PROCES
              INCLUDE 'NONSURF2FRESH.INC'
            CASE ('MAX8')
              INPUT='FLDS MAX, 8'
              CALL PROCES
              INCLUDE 'NONSURF2FRESH.INC'
            CASE ('MAX9')
              INPUT='FLDS MAX, 9'
              CALL PROCES
              INCLUDE 'NONSURF2FRESH.INC'
            CASE ('MAX10')
              INPUT='FLDS MAX, 10'
              CALL PROCES
              INCLUDE 'NONSURF2FRESH.INC'

            CASE ('IDCANCEL')
!        CALL WDialogHide()
!        CALL WDialogUnload()

          END SELECT
      END IF
C
      IF(COMMAND.EQ.'FLIP') THEN
C
!        CALL WDIALOGSELECT(FLIP)
C
          SELECT CASE(MESSAGE)
C
            CASE ('IDOK')
C
!        CALL WDIALOGGETINTEGER(STARTSURF,ISET)
!        CALL WDIALOGGETINTEGER(STOPSURF,JSET)
              IF(ISET.LT.1) ISET=1
              IF(JSET.GT.INT(SYSTEM1(20))) JSET=INT(SYSTEM1(20))
              IF(ISET.LT.JSET) THEN
                  CALL ITOAA(ISET,AJ)
                  CALL ITOAA(JSET,AK)
                  INPUT='FLIP,'//AJ//','//AK
                  CALL PROCES
              END IF
!        CALL WDialogHide()
!        CALL WDialogUnload()
              INCLUDE 'LOADSHEET.INC'

            CASE ('IDCANCEL')
!        CALL WDialogHide()
!        CALL WDialogUnload()

          END SELECT
      END IF
C
      IF(COMMAND.EQ.'SCALE') THEN
C
!        CALL WDIALOGSELECT(SCALE)
C
          SELECT CASE(MESSAGE)
C
            CASE ('IDOK')
C
!        CALL WDIALOGGETINTEGER(STARTSURF,ISET)
!        CALL WDIALOGGETINTEGER(STOPSURF,JSET)
!        CALL WDIALOGGETDOUBLE(FACTOR,DW1)
!        CALL WDIALOGGETRADIOBUTTON(SC1,KSET)
              IF(ISET.LT.0) ISET=0
              IF(JSET.GT.INT(SYSTEM1(20))) JSET=INT(SYSTEM1(20))
              IF(ISET.LT.JSET) THEN
                  CALL ITOAA(ISET,AJ)
                  CALL ITOAA(JSET,AK)
                  CALL DTOA23(DW1,AW1)
                  IF(KSET.EQ.1) THEN
                      INPUT='WSC,'//AW1//','//AJ//','//AK
                      CALL PROCES
                  END IF
                  IF(KSET.EQ.2) THEN
                      INPUT='SC,'//AW1//','//AJ//','//AK
                      CALL PROCES
                  END IF
                  IF(KSET.EQ.3) THEN
                      INPUT='WSC FY,'//AW1//','//AJ//','//AK
                      CALL PROCES
                  END IF
                  IF(KSET.EQ.4) THEN
                      INPUT='SC FY,'//AW1//','//AJ//','//AK
                      CALL PROCES
                  END IF
              END IF
!        CALL WDialogHide()
!        CALL WDialogUnload()
              INCLUDE 'LOADSHEET.INC'

            CASE ('IDCANCEL')
!        CALL WDialogHide()
!        CALL WDialogUnload()

          END SELECT
      END IF
C
      IF(COMMAND.EQ.'VIE') THEN
C
!        CALL WDIALOGSELECT(VIE)
C
          SELECT CASE(MESSAGE)
C
            CASE ('IDOK')
C
!        CALL WDIALOGGETINTEGER(STARTSURF,ISTARTSURF)
!        CALL WDIALOGGETINTEGER(STOPSURF,ISTOPSURF)
!        CALL WDIALOGGETDOUBLE(FACTOR,DW1)
!        CALL WDIALOGGETDOUBLE(EL,DW2)
!        CALL WDIALOGGETDOUBLE(AZ,DW3)
!        CALL WDIALOGGETDOUBLE(ROT,DW4)
!        CALL WDIALOGGETINTEGER(OFFX,JX)
!        CALL WDIALOGGETINTEGER(OFFY,JY)
!        CALL WDIALOGGETRADIOBUTTON(VIE1,ISET)
!        CALL WDIALOGGETRADIOBUTTON(VIG1,JSET)
!        CALL WDIALOGGETRADIOBUTTON(SYM1,KSET)
!        CALL WDIALOGGETRADIOBUTTON(SC1,NSET)
!        CALL WDIALOGGETCHECKBOX(MX,OSET)
!        CALL WDIALOGGETCHECKBOX(MY,PSET)
!        CALL WDIALOGGETCHECKBOX(CX,QSET)
!        CALL WDIALOGGETCHECKBOX(CY,RSET)
C       VIEVIG
              IF(JSET.EQ.1) THEN
                  INPUT='VIEVIG ON'
                  CALL PROCES
              END IF
              IF(JSET.EQ.2) THEN
                  INPUT='VIEVIG OFF'
                  CALL PROCES
              END IF
C
C       VIESYM
              IF(KSET.EQ.1) THEN
                  INPUT='VIESYM ON'
                  CALL PROCES
              END IF
              IF(KSET.EQ.2) THEN
                  INPUT='VIESYM OFF'
                  CALL PROCES
              END IF
C
C       VIEOFF
              CALL ITOA6(JX,AJX)
              CALL ITOA6(JY,AJY)
              CALL DTOA23(DW4,AW4)
              INPUT='VIEOFF,'//AJX//','//AJY//','//AW4
              CALL PROCES


              IF(JSET.EQ.1) THEN
C       USE VIECO
                  IF(ISET.EQ.1) THEN
C       YZ PLANE
                      IF(NSET.EQ.1) THEN
C       AUTOSCALE
                          CALL ITOAA(ISTARTSURF,AJ)
                          CALL ITOAA(ISTOPSURF,AK)
                          INPUT='VIECO,'//','//AJ//','//AK//',1'
                          CALL PROCES
                          IF(OSET.EQ.1) THEN
                              INPUT='PLOT PMRAYX,'//AJ//','//AK
                              CALL PROCES
                          END IF
                          IF(PSET.EQ.1) THEN
                              INPUT='PLOT PMRAYY,'//AJ//','//AK
                              CALL PROCES
                          END IF
                          IF(QSET.EQ.1) THEN
                              INPUT='PLOT PCRAYX,'//AJ//','//AK
                              CALL PROCES
                          END IF
                          IF(RSET.EQ.1) THEN
                              INPUT='PLOT PCRAYY,'//AJ//','//AK
                              CALL PROCES
                          END IF

                      ELSE
C       USER SCALE
                          CALL ITOAA(ISTARTSURF,AJ)
                          CALL ITOAA(ISTOPSURF,AK)
                          CALL DTOA23(DW1,AW1)
                          INPUT='VIECO,'//AW1//','//AJ//','//AK//',1'
                          CALL PROCES
                          IF(OSET.EQ.1) THEN
                              INPUT='PLOT PMRAYX,'//AJ//','//AK
                              CALL PROCES
                          END IF
                          IF(PSET.EQ.1) THEN
                              INPUT='PLOT PMRAYY,'//AJ//','//AK
                              CALL PROCES
                          END IF
                          IF(QSET.EQ.1) THEN
                              INPUT='PLOT PCRAYX,'//AJ//','//AK
                              CALL PROCES
                          END IF
                          IF(RSET.EQ.1) THEN
                              INPUT='PLOT PCRAYY,'//AJ//','//AK
                              CALL PROCES
                          END IF
                      END IF
                  END IF
                  IF(ISET.EQ.2) THEN
C       XZ PLANE
                      IF(NSET.EQ.1) THEN
C       AUTOSCALE
                          CALL ITOAA(ISTARTSURF,AJ)
                          CALL ITOAA(ISTOPSURF,AK)
                          INPUT='VIECO XZ,'//','//AJ//','//AK//',1'
                          CALL PROCES
                          IF(OSET.EQ.1) THEN
                              INPUT='PLOT PMRAYX,'//AJ//','//AK
                              CALL PROCES
                          END IF
                          IF(PSET.EQ.1) THEN
                              INPUT='PLOT PMRAYY,'//AJ//','//AK
                              CALL PROCES
                          END IF
                          IF(QSET.EQ.1) THEN
                              INPUT='PLOT PCRAYX,'//AJ//','//AK
                              CALL PROCES
                          END IF
                          IF(RSET.EQ.1) THEN
                              INPUT='PLOT PCRAYY,'//AJ//','//AK
                              CALL PROCES
                          END IF
                      ELSE
C       USER SCALE
                          CALL ITOAA(ISTARTSURF,AJ)
                          CALL ITOAA(ISTOPSURF,AK)
                          CALL DTOA23(DW1,AW1)
                          INPUT='VIECO XZ,'//AW1//','//AJ//','//AK//',1'
                          CALL PROCES
                          IF(OSET.EQ.1) THEN
                              INPUT='PLOT PMRAYX,'//AJ//','//AK
                              CALL PROCES
                          END IF
                          IF(PSET.EQ.1) THEN
                              INPUT='PLOT PMRAYY,'//AJ//','//AK
                              CALL PROCES
                          END IF
                          IF(QSET.EQ.1) THEN
                              INPUT='PLOT PCRAYX,'//AJ//','//AK
                              CALL PROCES
                          END IF
                          IF(RSET.EQ.1) THEN
                              INPUT='PLOT PCRAYY,'//AJ//','//AK
                              CALL PROCES
                          END IF
                      END IF
                  END IF
                  IF(ISET.EQ.3) THEN
C       XY PLANE
                      IF(NSET.EQ.1) THEN
C       AUTOSCALE
                          WRITE(OUTLYNE,*)
     1                    'EXPLICIT SCALE FACTOR NEEDED WITH "VIE XY"'
                          CALL SHOWIT(1)
                      ELSE
C       USER SCALE
                          CALL ITOAA(ISTARTSURF,AJ)
                          CALL ITOAA(ISTOPSURF,AK)
                          CALL DTOA23(DW1,AW1)
                          INPUT='VIECO XY,'//AW1//','//AJ//','//AK//',1'
                          CALL PROCES
                          IF(OSET.EQ.1) THEN
                              INPUT='PLOT PMRAYX,'//AJ//','//AK
                              CALL PROCES
                          END IF
                          IF(PSET.EQ.1) THEN
                              INPUT='PLOT PMRAYY,'//AJ//','//AK
                              CALL PROCES
                          END IF
                          IF(QSET.EQ.1) THEN
                              INPUT='PLOT PCRAYX,'//AJ//','//AK
                              CALL PROCES
                          END IF
                          IF(RSET.EQ.1) THEN
                              INPUT='PLOT PCRAYY,'//AJ//','//AK
                              CALL PROCES
                          END IF
                      END IF
                  END IF
                  IF(ISET.EQ.4) THEN
C       ORTHO
                      IF(NSET.EQ.1) THEN
                          CALL DTOA23(DW2,AW2)
                          CALL DTOA23(DW3,AW3)
                          INPUT='PLOT VIEW,'//AW2//','//AW3
                          CALL PROCES
C       AUTOSCALE
                          CALL ITOAA(ISTARTSURF,AJ)
                          CALL ITOAA(ISTOPSURF,AK)
                          INPUT='VIECO ORTHO,'//','//AJ//','//AK//',1,1'
                          CALL PROCES
                          IF(OSET.EQ.1) THEN
                              INPUT='PLOT PMRAYX,'//AJ//','//AK
                              CALL PROCES
                          END IF
                          IF(PSET.EQ.1) THEN
                              INPUT='PLOT PMRAYY,'//AJ//','//AK
                              CALL PROCES
                          END IF
                          IF(QSET.EQ.1) THEN
                              INPUT='PLOT PCRAYX,'//AJ//','//AK
                              CALL PROCES
                          END IF
                          IF(RSET.EQ.1) THEN
                              INPUT='PLOT PCRAYY,'//AJ//','//AK
                              CALL PROCES
                          END IF
                      ELSE
                          CALL DTOA23(DW2,AW2)
                          CALL DTOA23(DW3,AW3)
                          INPUT='PLOT VIEW,'//AW2//','//AW3
                          CALL PROCES
C       USER SCALE
                          CALL ITOAA(ISTARTSURF,AJ)
                          CALL ITOAA(ISTOPSURF,AK)
                          CALL DTOA23(DW1,AW1)
                          INPUT='VIECO ORTHO,'//AW1//','//AJ//','//AK//',1,1'
                          CALL PROCES
                          IF(OSET.EQ.1) THEN
                              INPUT='PLOT PMRAYX,'//AJ//','//AK
                              CALL PROCES
                          END IF
                          IF(PSET.EQ.1) THEN
                              INPUT='PLOT PMRAYY,'//AJ//','//AK
                              CALL PROCES
                          END IF
                          IF(QSET.EQ.1) THEN
                              INPUT='PLOT PCRAYX,'//AJ//','//AK
                              CALL PROCES
                          END IF
                          IF(RSET.EQ.1) THEN
                              INPUT='PLOT PCRAYY,'//AJ//','//AK
                              CALL PROCES
                          END IF
                      END IF
                  END IF
              ELSE
C       USE VIE
                  IF(ISET.EQ.1) THEN
C       YZ PLANE
                      IF(NSET.EQ.1) THEN
C       AUTOSCALE
                          CALL ITOAA(ISTARTSURF,AJ)
                          CALL ITOAA(ISTOPSURF,AK)
                          INPUT='VIE,'//','//AJ//','//AK//',1'
                          CALL PROCES
                          IF(OSET.EQ.1) THEN
                              INPUT='PLOT PMRAYX,'//AJ//','//AK
                              CALL PROCES
                          END IF
                          IF(PSET.EQ.1) THEN
                              INPUT='PLOT PMRAYY,'//AJ//','//AK
                              CALL PROCES
                          END IF
                          IF(QSET.EQ.1) THEN
                              INPUT='PLOT PCRAYX,'//AJ//','//AK
                              CALL PROCES
                          END IF
                          IF(RSET.EQ.1) THEN
                              INPUT='PLOT PCRAYY,'//AJ//','//AK
                              CALL PROCES
                          END IF
                      ELSE
C       USER SCALE
                          CALL ITOAA(ISTARTSURF,AJ)
                          CALL ITOAA(ISTOPSURF,AK)
                          CALL DTOA23(DW1,AW1)
                          INPUT='VIE,'//AW1//','//AJ//','//AK//',1'
                          CALL PROCES
                          IF(OSET.EQ.1) THEN
                              INPUT='PLOT PMRAYX,'//AJ//','//AK
                              CALL PROCES
                          END IF
                          IF(PSET.EQ.1) THEN
                              INPUT='PLOT PMRAYY,'//AJ//','//AK
                              CALL PROCES
                          END IF
                          IF(QSET.EQ.1) THEN
                              INPUT='PLOT PCRAYX,'//AJ//','//AK
                              CALL PROCES
                          END IF
                          IF(RSET.EQ.1) THEN
                              INPUT='PLOT PCRAYY,'//AJ//','//AK
                              CALL PROCES
                          END IF
                      END IF
                  END IF
                  IF(ISET.EQ.2) THEN
C       XZ PLANE
                      IF(NSET.EQ.1) THEN
C       AUTOSCALE
                          CALL ITOAA(ISTARTSURF,AJ)
                          CALL ITOAA(ISTOPSURF,AK)
                          INPUT='VIE XZ,'//','//AJ//','//AK//',1'
                          CALL PROCES
                          IF(OSET.EQ.1) THEN
                              INPUT='PLOT PMRAYX,'//AJ//','//AK
                              CALL PROCES
                          END IF
                          IF(PSET.EQ.1) THEN
                              INPUT='PLOT PMRAYY,'//AJ//','//AK
                              CALL PROCES
                          END IF
                          IF(QSET.EQ.1) THEN
                              INPUT='PLOT PCRAYX,'//AJ//','//AK
                              CALL PROCES
                          END IF
                          IF(RSET.EQ.1) THEN
                              INPUT='PLOT PCRAYY,'//AJ//','//AK
                              CALL PROCES
                          END IF
                      ELSE
C       USER SCALE
                          CALL ITOAA(ISTARTSURF,AJ)
                          CALL ITOAA(ISTOPSURF,AK)
                          CALL DTOA23(DW1,AW1)
                          INPUT='VIE XZ,'//AW1//','//AJ//','//AK//',1'
                          CALL PROCES
                          IF(OSET.EQ.1) THEN
                              INPUT='PLOT PMRAYX,'//AJ//','//AK
                              CALL PROCES
                          END IF
                          IF(PSET.EQ.1) THEN
                              INPUT='PLOT PMRAYY,'//AJ//','//AK
                              CALL PROCES
                          END IF
                          IF(QSET.EQ.1) THEN
                              INPUT='PLOT PCRAYX,'//AJ//','//AK
                              CALL PROCES
                          END IF
                          IF(RSET.EQ.1) THEN
                              INPUT='PLOT PCRAYY,'//AJ//','//AK
                              CALL PROCES
                          END IF
                      END IF
                  END IF
                  IF(ISET.EQ.3) THEN
C       XY PLANE
                      IF(NSET.EQ.1) THEN
C       AUTOSCALE
                          WRITE(OUTLYNE,*)
     1                    'EXPLICIT SCALE FACTOR NEEDED WITH "VIE XY"'
                          CALL SHOWIT(1)
                      ELSE
C       USER SCALE
                          CALL ITOAA(ISTARTSURF,AJ)
                          CALL ITOAA(ISTOPSURF,AK)
                          CALL DTOA23(DW1,AW1)
                          INPUT='VIE XY,'//AW1//','//AJ//','//AK//',1'
                          CALL PROCES
                          IF(OSET.EQ.1) THEN
                              INPUT='PLOT PMRAYX,'//AJ//','//AK
                              CALL PROCES
                          END IF
                          IF(PSET.EQ.1) THEN
                              INPUT='PLOT PMRAYY,'//AJ//','//AK
                              CALL PROCES
                          END IF
                          IF(QSET.EQ.1) THEN
                              INPUT='PLOT PCRAYX,'//AJ//','//AK
                              CALL PROCES
                          END IF
                          IF(RSET.EQ.1) THEN
                              INPUT='PLOT PCRAYY,'//AJ//','//AK
                              CALL PROCES
                          END IF
                      END IF
                  END IF
                  IF(ISET.EQ.4) THEN
C       ORTHO
                      IF(NSET.EQ.1) THEN
                          CALL DTOA23(DW2,AW2)
                          CALL DTOA23(DW3,AW3)
                          INPUT='PLOT VIEW,'//AW2//','//AW3
                          CALL PROCES
C       AUTOSCALE
                          CALL ITOAA(ISTARTSURF,AJ)
                          CALL ITOAA(ISTOPSURF,AK)
                          INPUT='VIE ORTHO,'//','//AJ//','//AK//',1,1'
                          CALL PROCES
                          IF(OSET.EQ.1) THEN
                              INPUT='PLOT PMRAYX,'//AJ//','//AK
                              CALL PROCES
                          END IF
                          IF(PSET.EQ.1) THEN
                              INPUT='PLOT PMRAYY,'//AJ//','//AK
                              CALL PROCES
                          END IF
                          IF(QSET.EQ.1) THEN
                              INPUT='PLOT PCRAYX,'//AJ//','//AK
                              CALL PROCES
                          END IF
                          IF(RSET.EQ.1) THEN
                              INPUT='PLOT PCRAYY,'//AJ//','//AK
                              CALL PROCES
                          END IF
                      ELSE
                          CALL DTOA23(DW2,AW2)
                          CALL DTOA23(DW3,AW3)
                          INPUT='PLOT VIEW,'//AW2//','//AW3
                          CALL PROCES
C       USER SCALE
                          CALL ITOAA(ISTARTSURF,AJ)
                          CALL ITOAA(ISTOPSURF,AK)
                          CALL DTOA23(DW1,AW1)
                          INPUT='VIE ORTHO,'//AW1//','//AJ//','//AK//',1,1'
                          CALL PROCES
                          IF(OSET.EQ.1) THEN
                              INPUT='PLOT PMRAYX,'//AJ//','//AK
                              CALL PROCES
                          END IF
                          IF(PSET.EQ.1) THEN
                              INPUT='PLOT PMRAYY,'//AJ//','//AK
                              CALL PROCES
                          END IF
                          IF(QSET.EQ.1) THEN
                              INPUT='PLOT PCRAYX,'//AJ//','//AK
                              CALL PROCES
                          END IF
                          IF(RSET.EQ.1) THEN
                              INPUT='PLOT PCRAYY,'//AJ//','//AK
                              CALL PROCES
                          END IF
                      END IF
                  END IF
              END IF
C       OUTPUT SECTION
              CALL GRAPHOUTPUT

            CASE ('IDCANCEL')
!        CALL WDialogHide()
!        CALL WDialogUnload()

          END SELECT
      END IF
