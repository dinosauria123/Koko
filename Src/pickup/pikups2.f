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

C       SECOND FILE OF PIKUP ROUTINES

C SUB PIKCV.FOR
      SUBROUTINE PIKCV
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE PIKCV. THIS IS THE SUBROUTINE WHICH
C       HANDLES RADIUS AND CURVATURE PIKUPS AT THE LENS INPUT
C       AND UPDATE LENS LEVEL.
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
C
C       FILE LENSSTORE.DOC CONTAINS INFORMATION ON THE
C       SPECIFIC STORAGE LOCATIONS FOR LENS DATA,SOLVE AND
C       PIKUP ITEMS.
C
C       TEST TO SEE IF THE SURFACE FROM WHICH WE ARE PIKING UP
C       DATA IS THE SAME AS THE SURFACE TO WHICH THE PIKUP
C       WILL BE ASSINGED. OPERATION NOT LEGAL
C
          IF(SURF.EQ.INT(W1)) THEN
              OUTLYNE='A SURFACE CAN NOT PIKUP ITS OWN VALUES'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
C
C       SINCE THIS IS A RADIUS OR CURVATURE PIKUP,
C       THE THIRD DIMENSION WILL
C       BE 1 OR 2 RESPECTIVELY
C       THE PIKUP EXISTENCE FLAG IS PIKUP(1,I,J) FOR EVERY
C       PIKUP ON EVERY SURFACE. IF IT IS 1.0D0 THE PIKUP EXISTS
C       IF IT IS 0.0D0 THE PIKUP DOES NOT EXIST
C
C       STORE NUMERIC INPUT AND HANDEL DEFAULT DATA

C       IF DF1=1 THIS MEANS PIKING UP FROM THE OBJECT SURFACE
C       ON TO THE CURRENT SURFACE
          IF(DF1.EQ.1) W1=0.0D0
C       IF DF2=1 THIS MEANS MULTIPLIER IS UNITY
          IF(DF2.EQ.1) W2=1.0D0
C       IF DF3=1 TIS MEANS ADDITIVE CONSTANT ZERO
          IF(DF3.EQ.1) W3=0.0D0
C       IF DF4=1 THEN W4=0.0D0
C       IF DF5=1 THEN W5=0.0D0
          IF(DF4.EQ.1) W4=0.0D0
          IF(DF5.EQ.1) W5=0.0D0
          IF(W1.LT.0.0D0) W1=SURF+W1
          IF(W1.LT.0.0D0.OR.W1.GT.SYSTEM1(20)) THEN
              OUTLYNE='SOURCE SURFACE NUMBER BEYOND LEGAL RANGE'
              CALL SHOWIT(1)
              OUTLYNE='PIKUP NOT ALLOWED'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(WQ.EQ.'RD') THEN
C               INCREMENT THE PIKUP COUNTER
C               I.E. ALENS(32,SURF)
              IF(PIKUP(1,SURF,2).EQ.0.0D0)
     1        ALENS(32,SURF)= ALENS(32,SURF)+1.0D0
              PIKUP(1,SURF,1)=1.0D0
              PIKUP(1,SURF,2)=0.0D0
              PIKUP(2,SURF,1)=W1
              PIKUP(3,SURF,1)=W2
              PIKUP(4,SURF,1)=W3
              PIKUP(5,SURF,1)=W4
              PIKUP(6,SURF,1)=W5
          ELSE
              IF(WQ.EQ.'CV') THEN
C               INCREMENT THE PIKUP COUNTER
C               I.E. ALENS(32,SURF)
                  IF(PIKUP(1,SURF,1).EQ.0.0D0)
     1            ALENS(32,SURF)= ALENS(32,SURF)+1.0D0
                  ALENS(32,SURF)= ALENS(32,SURF)+1.0D0
                  PIKUP(1,SURF,1)=0.0D0
                  PIKUP(1,SURF,2)=1.0D0
                  PIKUP(2,SURF,2)=W1
                  PIKUP(3,SURF,2)=W2
                  PIKUP(4,SURF,2)=W3
                  PIKUP(5,SURF,2)=W4
                  PIKUP(6,SURF,2)=W5
              ELSE
              END IF
          END IF
C       DUMP PIKUP PRO AND NPRO IF FOUND
          IF(PIKUP(1,SURF,11).GT.0.0D0) THEN
              PIKUP(1,SURF,11)=0.0D0
              PIKUP(2,SURF,11)=0.0D0
              PIKUP(3,SURF,11)=0.0D0
              PIKUP(4,SURF,11)=0.0D0
              PIKUP(5,SURF,11)=0.0D0
              PIKUP(6,SURF,11)=0.0D0
              ALENS(32,SURF)=ALENS(32,SURF)-1.0D0
              WRITE(OUTLYNE,*)'SURFACE',SURF,' :PIKUP (PRO) DELETED'
              CALL SHOWIT(1)
          END IF
          IF(PIKUP(1,SURF,12).GT.0.0D0) THEN
              PIKUP(1,SURF,12)=0.0D0
              PIKUP(2,SURF,12)=0.0D0
              PIKUP(3,SURF,12)=0.0D0
              PIKUP(4,SURF,12)=0.0D0
              PIKUP(5,SURF,12)=0.0D0
              PIKUP(6,SURF,12)=0.0D0
              ALENS(32,SURF)=ALENS(32,SURF)-1.0D0
              WRITE(OUTLYNE,*)'SURFACE',SURF,' :PIKUP (NPRO) DELETED'
              CALL SHOWIT(1)
          END IF
C
C       IF THE SURFACE IS NON-TORIC OR IF IT IS A YTORIC
C       THEN DELETE YZ PLANE SOLVES. DON'T DELETE THEM IF SURFACE
C       IS AN XTORIC
          IF(ALENS(23,SURF).EQ.0.0D0.OR.ALENS(23,SURF).EQ.1.0D0) THEN
              IF(SOLVE(8,SURF).GT.0.0D0) THEN
                  SOLVE(8,SURF)=0.0D0
                  SOLVE(9,SURF)=0.0D0
C       RECALCULATE ALENS(33,SURF)
                  ALENS(33,SURF)=0.0D0
                  IF(SOLVE(6,SURF).GT.0.0D0) ALENS(33,SURF)=ALENS(33,SURF)+1.0D0
                  IF(SOLVE(4,SURF).GT.0.0D0) ALENS(33,SURF)=ALENS(33,SURF)+0.1D0
                  IF(SOLVE(8,SURF).GT.0.0D0) ALENS(33,SURF)=ALENS(33,SURF)+2.0D0
                  IF(SOLVE(2,SURF).GT.0.0D0) ALENS(33,SURF)=ALENS(33,SURF)+0.2D0
                  WRITE(OUTLYNE,*)
     1            'SURFACE',SURF,' :YZ PLANE CURVATURE SOLVE DELETED'
                  CALL SHOWIT(1)
              END IF
          END IF
C       IF SURFACE IS X-TORIC OR NON-TORIC, DELETE XZ PLANE
C       CURVATURE SOLVES
          IF(ALENS(23,SURF).EQ.0.0D0.OR.ALENS(23,SURF).EQ.2.0D0) THEN
              IF(SOLVE(2,SURF).GT.0.0D0) THEN
                  SOLVE(2,SURF)=0.0D0
                  SOLVE(1,SURF)=0.0D0
C       RECALCULATE ALENS(33,SURF)
                  ALENS(33,SURF)=0.0D0
                  IF(SOLVE(6,SURF).GT.0.0D0) ALENS(33,SURF)=ALENS(33,SURF)+1.0D0
                  IF(SOLVE(4,SURF).GT.0.0D0) ALENS(33,SURF)=ALENS(33,SURF)+0.1D0
                  IF(SOLVE(8,SURF).GT.0.0D0) ALENS(33,SURF)=ALENS(33,SURF)+2.0D0
                  IF(SOLVE(2,SURF).GT.0.0D0) ALENS(33,SURF)=ALENS(33,SURF)+0.2D0
                  WRITE(OUTLYNE,*)
     1            'SURFACE',SURF,' :XZ PLANE CURVATURE SOLVE DELETED'
                  CALL SHOWIT(1)
              END IF
          END IF
          RETURN
      END
C SUB PIKCON.FOR
      SUBROUTINE PIKCON
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE PIKCON. THIS IS THE SUBROUTINE WHICH
C       HANDLES CONIC AND TORIC CONIC CONSTANT PIKUPS AT THE LENS INPUT
C       AND UPDATE LENS LEVEL.
C
          INTEGER CT
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
C       TEST TO SEE IF THE SURFACE FROM WHICH WE ARE PIKING UP
C       DATA IS THE SAME AS THE SURFACE TO WHICH THE PIKUP
C       WILL BE ASSINGED. OPERATION NOT LEGAL
C
          IF(SURF.EQ.INT(W1)) THEN
              OUTLYNE='A SURFACE CAN NOT PIKUP ITS OWN VALUES'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
          IF(WQ.EQ.'CC') CT=4
          IF(WQ.EQ.'CCTOR') CT=21
C
C       THE PIKUP EXISTENCE FLAG IS PIKUP(1,I,J) FOR EVERY
C       PIKUP ON EVERY SURFACE. IF IT IS 1.0D0 THE PIKUP EXISTS
C       IF IT IS 0.0D0 THE PIKUP DOES NOT EXIST
C
C       STORE NUMERIC INPUT AND HANDEL DEFAULT DATA

C       IF DF1=1 THIS MEANS PIKING UP FROM THE OBJECT SURFACE
C       ON TO THE CURRENT SURFACE
          IF(DF1.EQ.1) W1=0.0D0
C       IF DF2=1 THIS MEANS MULTIPLIER IS UNITY
          IF(DF2.EQ.1) W2=1.0D0
C       IF DF3=1 TIS MEANS ADDITIVE CONSTANT ZERO
          IF(DF3.EQ.1) W3=0.0D0
C       IF DF4=1 THEN W4=0.0D0
C       IF DF5=1 THEN W5=0.0D0
          IF(DF4.EQ.1) W4=0.0D0
          IF(DF5.EQ.1) W5=0.0D0
C
          IF(W1.LT.0.0D0) W1=SURF+W1
          IF(W1.LT.0.0D0.OR.W1.GT.SYSTEM1(20)) THEN
              OUTLYNE='SOURCE SURFACE NUMBER BEYOND LEGAL RANGE'
              CALL SHOWIT(1)
              OUTLYNE='PIKUP NOT ALLOWED'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C               INCREMENT THEPIKUP COUNTER BY 1.0D0
C               I.E. ALENS(32,SURF)
          ALENS(32,SURF)=ALENS(32,SURF)+1.0D0
          PIKUP(1,SURF,CT)=1.0D0
          PIKUP(2,SURF,CT)=W1
          PIKUP(3,SURF,CT)=W2
          PIKUP(4,SURF,CT)=W3
          PIKUP(5,SURF,CT)=W4
          PIKUP(6,SURF,CT)=W5
C       DUMP PIKUP PRO AND NPRO IF FOUND
          IF(PIKUP(1,SURF,11).GT.0.0D0) THEN
              PIKUP(1,SURF,11)=0.0D0
              PIKUP(2,SURF,11)=0.0D0
              PIKUP(3,SURF,11)=0.0D0
              PIKUP(4,SURF,11)=0.0D0
              PIKUP(5,SURF,11)=0.0D0
              PIKUP(6,SURF,11)=0.0D0
              ALENS(32,SURF)=ALENS(32,SURF)-1.0D0
              WRITE(OUTLYNE,*)'SURFACE',SURF,' :PIKUP (PRO) DELETED'
              CALL SHOWIT(1)
          END IF
          IF(PIKUP(1,SURF,12).GT.0.0D0) THEN
              PIKUP(1,SURF,12)=0.0D0
              PIKUP(2,SURF,12)=0.0D0
              PIKUP(3,SURF,12)=0.0D0
              PIKUP(4,SURF,12)=0.0D0
              PIKUP(5,SURF,12)=0.0D0
              PIKUP(6,SURF,12)=0.0D0
              ALENS(32,SURF)=ALENS(32,SURF)-1.0D0
              WRITE(OUTLYNE,*)'SURFACE',SURF,' :PIKUP (NPRO) DELETED'
              CALL SHOWIT(1)
          END IF
C
C       THERE ARE NO SOLVES FOR CONIC CONSTANT
C       SO THERE ARE NO SOLVE DELETIONS TO HANDEL.
C
          RETURN
      END
C SUB PIKASP.FOR
      SUBROUTINE PIKASP
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE PIKASP. THIS IS THE SUBROUTINE WHICH
C       HANDLES ASPHERIC OR ASPHERIC TORIC
C       TERM PIKUPS AT THE LENS INPUT
C       AND UPDATE LENS LEVEL. IF THE SURFACE TO WHICH THE PIKUP
C       IS GOING IS NOT DEFINED AS ASPHERIC OR ASPHERIC TORIC
C       , IT IS SET AS SUCH
C       AUTOMATICALLY AND A MESSAGE IS PRINTED.
C
          INTEGER FSURF,CT
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
C       FILE LENSSTORE.DOC CONTAINS INFORMATION ON THE
C       SPECIFIC STORAGE LOCATIONS FOR LENS DATA,SOLVE AND
C       PIKUP ITEMS.
C
C       TEST TO SEE IF THE SURFACE FROM WHICH WE ARE PIKING UP
C       DATA IS THE SAME AS THE SURFACE TO WHICH THE PIKUP
C       WILL BE ASSINGED. OPERATION NOT LEGAL
C
          IF(SURF.EQ.INT(W1)) THEN
              OUTLYNE='A SURFACE CAN NOT PIKUP ITS OWN VALUES'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
          IF(WQ.EQ.'AC') CT=26
          IF(WQ.EQ.'AD') CT=5
          IF(WQ.EQ.'AE') CT=6
          IF(WQ.EQ.'AF') CT=7
          IF(WQ.EQ.'AG') CT=8
          IF(WQ.EQ.'AH') CT=27
          IF(WQ.EQ.'AI') CT=28
          IF(WQ.EQ.'AJ') CT=29
          IF(WQ.EQ.'AK') CT=30
          IF(WQ.EQ.'AL') CT=31
          IF(WQ.EQ.'ADTOR') CT=22
          IF(WQ.EQ.'AETOR') CT=23
          IF(WQ.EQ.'AFTOR') CT=24
          IF(WQ.EQ.'AGTOR') CT=25
C       FOR AC THIRD TERM IS 26
C       FOR AD THIRD TERM IS 5
C       FOR AE THIRD TERM IS 6
C       FOR AF THIRD TERM IS 7
C       FOR AG THIRD TERM IS 8
C       FOR AH THIRD TERM IS 27
C       FOR AI THIRD TERM IS 28
C       FOR AJ THIRD TERM IS 29
C       FOR AK THIRD TERM IS 30
C       FOR AL THIRD TERM IS 31
C       FOR ADTOR THIRD TERM IS 22
C       FOR AETOR THIRD TERM IS 23
C       FOR AFTOR THIRD TERM IS 24
C       FOR AGTOR THIRD TERM IS 25
C
C       THE PIKUP EXISTENCE FLAG IS PIKUP(1,I,J) FOR EVERY
C       PIKUP ON EVERY SURFACE. IF IT IS 1.0D0 THE PIKUP EXISTS
C       IF IT IS 0.0D0 THE PIKUP DOES NOT EXIST
C
C       STORE NUMERIC INPUT AND HANDEL DEFAULT DATA

C       IF DF1=1 THIS MEANS PIKING UP FROM THE OBJECT SURFACE
C       ON TO THE CURRENT SURFACE
          IF(DF1.EQ.1) W1=0.0D0
C       IF DF2=1 THIS MEANS MULTIPLIER IS UNITY
          IF(DF2.EQ.1) W2=1.0D0
C       IF DF3=1 TIS MEANS ADDITIVE CONSTANT ZERO
          IF(DF3.EQ.1) W3=0.0D0
C       IF DF4=1 THEN W4=0.0D0
C       IF DF5=1 THEN W5=0.0D0
          IF(DF4.EQ.1) W4=0.0D0
          IF(DF5.EQ.1) W5=0.0D0
          IF(W1.LT.0.0D0) W1=SURF+W1
C
          IF(W1.LT.0.0D0.OR.W1.GT.SYSTEM1(20)) THEN
              OUTLYNE='SOURCE SURFACE NUMBER BEYOND LEGAL RANGE'
              CALL SHOWIT(1)
              OUTLYNE='PIKUP NOT ALLOWED'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
C       THE SURFACE BEING PIKED UP FROM MUST BE ASPHERIC OR THE
C       PIKUP IS NOT ALLOWED.
C
          FSURF=INT(W1)
          IF(WQ.EQ.'AC'.OR.WQ.EQ.'AF'.OR.WQ.EQ.'AD'.OR.WQ.EQ.'AH'.OR.
     1    WQ.EQ.'AI'.OR.WQ.EQ.'AJ'.OR.WQ.EQ.'AK'.OR.WQ.EQ.'AL'
     1    .OR.WQ.EQ.'AE'.OR.WQ.EQ.'AG')
     1    THEN
              IF(ALENS(8,FSURF).EQ.0D0) THEN
                  WRITE(OUTLYNE,*)
     1              'SURFACE TO BE PIKED UP FROM IS NOT DEFINED AS ASPHERIC'
                  CALL SHOWIT(1)
                  OUTLYNE='PIKUP ('//WQ(1:2)//') NOT ALLOWED'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
          IF(WQ.EQ.'ADTOR'.OR.WQ.EQ.'AFTOR'.OR.WC.EQ.'AETOR'.OR.
     1    WQ.EQ.'AGTOR') THEN
              IF(ALENS(36,FSURF).EQ.0D0) THEN
                  WRITE(OUTLYNE,*)
     1              'SURFACE TO BE PIKED UP FROM IS NOT DEFINED AS ASPHERIC TORIC'
                  CALL SHOWIT(1)
                  OUTLYNE='PIKUP ('//WQ(1:5)//') NOT ALLOWED'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              ELSE
              END IF
          ELSE
          END IF
C
C               INCR.PIKUP INDICATOR BY 1.0D0
C               I.E. ALENS(32,SURF)
          ALENS(32,SURF)=ALENS(32,SURF)+1.0D0
          PIKUP(1,SURF,CT)=1.0D0
          PIKUP(2,SURF,CT)=W1
          PIKUP(3,SURF,CT)=W2
          PIKUP(4,SURF,CT)=W3
          PIKUP(5,SURF,CT)=W4
          PIKUP(6,SURF,CT)=W5
C       DUMP PIKUP PRO AND NPRO IF FOUND
          IF(PIKUP(1,SURF,11).GT.0.0D0) THEN
              PIKUP(1,SURF,11)=0.0D0
              PIKUP(2,SURF,11)=0.0D0
              PIKUP(3,SURF,11)=0.0D0
              PIKUP(4,SURF,11)=0.0D0
              PIKUP(5,SURF,11)=0.0D0
              PIKUP(6,SURF,11)=0.0D0
              ALENS(32,SURF)=ALENS(32,SURF)-1.0D0
              WRITE(OUTLYNE,*)'SURFACE',SURF,' :PIKUP (PRO) DELETED'
              CALL SHOWIT(1)
          END IF
          IF(PIKUP(1,SURF,12).GT.0.0D0) THEN
              PIKUP(1,SURF,12)=0.0D0
              PIKUP(2,SURF,12)=0.0D0
              PIKUP(3,SURF,12)=0.0D0
              PIKUP(4,SURF,12)=0.0D0
              PIKUP(5,SURF,12)=0.0D0
              PIKUP(6,SURF,12)=0.0D0
              ALENS(32,SURF)=ALENS(32,SURF)-1.0D0
              WRITE(OUTLYNE,*)'SURFACE',SURF,' :PIKUP (NPRO) DELETED'
              CALL SHOWIT(1)
          END IF
C
C
          IF(WQ.EQ.'AD'.OR.WQ.EQ.'AE'.OR.WQ.EQ.'AF'.OR.WQ.EQ.'AH'.OR.
     1    WQ.EQ.'AI'.OR.WQ.EQ.'AJ'.OR.WQ.EQ.'AJ'.OR.WQ.EQ.'AL'
     1    .OR.WQ.EQ.'AG'.OR.WQ.EQ.'AC')THEN
C       IF ALENS(8,SURF)=0.0D0 THEN THE SURFACE WAS NOT AN ASPHERE
C       MAKE IT ONE AND PRINT MESSAGE ELSE DO NOTHING
C
              IF(ALENS(8,SURF).EQ.0.0D0) THEN
                  WRITE(OUTLYNE,*)'SURFACE',SURF,' :RE-DEFINED AS ASPHERIC'
                  CALL SHOWIT(1)
                  ALENS(8,SURF)=1.0D0
              ELSE
              END IF
          ELSE
          END IF
C
          IF(WQ.EQ.'ADTOR'.OR.WQ.EQ.'AETOR'.OR.WQ.EQ.'AFTOR'
     1    .OR.WQ.EQ.'AGTOR')THEN
C       IF ALENS(36,SURF)=0.0D0 THEN THE SURFACE WAS NOT AN ASPHERE
C       TORIC, MAKE IT ONE AND PRINT MESSAGE ELSE DO NOTHING
C
              IF(ALENS(36,SURF).EQ.0.0D0) THEN
                  WRITE(OUTLYNE,*)'SURFACE',SURF,' :RE-DEFINED AS ASPHERIC TORIC'
                  CALL SHOWIT(1)
                  ALENS(36,SURF)=1.0D0
              END IF
          END IF
C
C       THERE ARE NO SOLVE DELETIONS TO HANDEL.
C
          RETURN
      END
C SUB PIKAPE.FOR
      SUBROUTINE PIKAPE
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE PIKAPE. THIS IS THE SUBROUTINE WHICH
C       HANDLES CLEAR APERTURE (CLAP) PIKUP AND OBSCURATION PIKUPS
C       AT THE LENS INPUT
C       AND UPDATE LENS LEVEL.
C
          INTEGER CT,I
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
C       FILE LENSSTORE.DOC CONTAINS INFORMATION ON THE
C       SPECIFIC STORAGE LOCATIONS FOR LENS DATA,SOLVE AND
C       PIKUP ITEMS.
C
C       TEST TO SEE IF THE SURFACE FROM WHICH WE ARE PIKING UP
C       DATA IS THE SAME AS THE SURFACE TO WHICH THE PIKUP
C       WILL BE ASSINGED. OPERATION NOT LEGAL
C
          IF(SURF.EQ.INT(W1)) THEN
              OUTLYNE='A SURFACE CAN NOT PIKUP ITS OWN VALUES'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
          IF(WQ.EQ.'CLAP') CT=18
          IF(WQ.EQ.'COBS') CT=19
C
C       THE PIKUP EXISTENCE FLAG IS PIKUP(1,I,J) FOR EVERY
C       PIKUP ON EVERY SURFACE. IF IT IS 1.0D0 THE PIKUP EXISTS
C       IF IT IS 0.0D0 THE PIKUP DOES NOT EXIST
C
C       STORE NUMERIC INPUT AND HANDEL DEFAULT DATA

C       IF DF1=1 THIS MEANS PIKING UP FROM THE OBJECT SURFACE
C       ON TO THE CURRENT SURFACE
          IF(DF1.EQ.1) W1=0.0D0
C       IF DF2=1 THIS MEANS MULTIPLIER IS UNITY
          IF(DF2.EQ.1) W2=1.0D0
C       IF DF3=1 TIS MEANS ADDITIVE CONSTANT ZERO
          IF(DF3.EQ.1) W3=0.0D0
C       IF DF4=1 THEN W4=0.0D0
C       IF DF5=1 THEN W5=0.0D0
          IF(DF4.EQ.1) W4=0.0D0
          IF(DF5.EQ.1) W5=0.0D0
          IF(DF2.NE.1.OR.DF3.NE.1.OR.DF4.NE.1) THEN
              OUTLYNE=
     1        '"PIKUP (CLAP OR COBS)" ONLY TAKE NUMERIC WORD #1 AND #5 INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              RETURN
          END IF
          IF(W1.LT.0.0D0) W1=SURF+W1
          IF(W1.LT.0.0D0.OR.W1.GT.SYSTEM1(20)) THEN
              OUTLYNE='SOURCE SURFACE NUMBER BEYOND LEGAL RANGE'
              CALL SHOWIT(1)
              OUTLYNE='PIKUP NOT ALLOWED'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
C       CHECK THAT THE SURFACE BEING PIKED UP
C       FROM HAS A CLEAR APERTURE ASSIGNED. IF NOT
C       PRINT MESSAGE AND DON'T ASSIGN PIKUP
C
          IF(WQ.EQ.'CLAP'.AND.ALENS(9,INT(W1)).EQ.0.0D0) THEN
              OUTLYNE='SOURCE SURFACE OF (CLAP) PIKUP'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'(SURFACE',INT(W1),') HAS NO (CLAP)'
              CALL SHOWIT(1)
              OUTLYNE='ASSIGNED :  PIKUP (CLAP) NOT ALLOWED'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(WQ.EQ.'COBS'.AND.ALENS(16,INT(W1)).EQ.0.0D0) THEN
              OUTLYNE='SOURCE SURFACE OF (COBS) PIKUP'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'(SURFACE',INT(W1),') HAS NO (COBS)'
              CALL SHOWIT(1)
              OUTLYNE='ASSIGNED :  PIKUP (COBS) NOT ALLOWED'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
C               INCR PIKUP INDICATOR BY 1.0D0
C               I.E. ALENS(32,SURF)
          ALENS(32,SURF)=ALENS(32,SURF)+1.0D0
          PIKUP(1,SURF,CT)=1.0D0
          PIKUP(2,SURF,CT)=W1
          PIKUP(3,SURF,CT)=0.0D0
          PIKUP(4,SURF,CT)=0.0D0
          PIKUP(5,SURF,CT)=0.0D0
          PIKUP(6,SURF,CT)=W5
C
C       NOW SET THE CLAP AND COBS FLAG CORRECTLY
C       IF IT IS SET ON THE SURFACE BEING PIKED UP FROM
C       IT SHOULD BE SET ON THE TARGET SURFACE
C       THIS IMMEDIATELLY SETS THE CLAP TYPE
C       ON THE PIKING SURFACE.
          IF(ALENS(9,INT(W1)).NE.0.0D0) THEN
              ALENS(9,SURF)=ALENS(9,INT(W1))
              ALENS(51,SURF)=ALENS(51,INT(W1))
          ELSE
              ALENS(9,SURF)=0.0D0
              ALENS(51,SURF)=0.0D0
          END IF
          IF(ALENS(127,INT(W1)).NE.0.0D0) THEN
              ALENS(127,SURF)=ALENS(127,INT(W1))
              I=INT(ALENS(127,INT(W1)))
              MULTCLAP(1:I,1:2,SURF)=MULTCLAP(1:I,1:2,INT(W1))
              MULTCOBS(1:I,1:2,SURF)=MULTCOBS(1:I,1:2,INT(W1))
          END IF
          IF(ALENS(16,INT(W1)).NE.0.0D0) THEN
              ALENS(16,SURF)=ALENS(16,INT(W1))
              ALENS(61,SURF)=ALENS(61,INT(W1))
          ELSE
              ALENS(16,SURF)=0.0D0
              ALENS(61,SURF)=0.0D0
          END IF
C
C       THERE ARE NO SOLVE DELETIONS TO HANDEL.
C
C
          RETURN
      END
C SUB SPIKD.FOR
      SUBROUTINE SPIKD
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE SPIKD. THIS IS THE SUBROUTINE WHICH
C       HANDLES THE LENS PARAMETER PIKUP DELETIONS AT THE
C       UPDATE LENS LEVEL.
C
          INTEGER PIKCNT,I,K,SF

          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
C       FILE LENSSTORE.DOC CONTAINS INFORMATION ON THE
C       SPECIFIC STORAGE LOCATIONS FOR LENS DATA,SOLVE AND
C       PIKUP ITEMS.
C
          IF(SST.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
              OUTLYNE=
     1        '"PIKD" TAKES NO STRING OR NUMERIC WORD #3 THROUGH #5 INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(DF1.EQ.1.AND.DF2.EQ.1) THEN
              W1=DBLE(SURF)
              W2=DBLE(SURF)
              S1=1
              S2=1
              DF1=0
              DF2=0
          END IF
          IF(DF1.EQ.0.AND.DF2.EQ.1) THEN
              W2=W1
              S1=1
              S2=1
              DF1=0
              DF2=0
          END IF
          IF(DF1.EQ.1.AND.DF2.EQ.0.OR.DF1.EQ.0.AND.DF2.EQ.1) THEN
              OUTLYNE=
     1        '"PIKD" USES EITHER TWO OR ZERO NUMERIC WORDS'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(INT(W1).LT.0) THEN
              OUTLYNE=
     1        'STARTING SURFACE NUMBER MUST BE GREATER THAN OR EQUAL TO 0'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(INT(W2).GT.INT(SYSTEM1(20))) THEN
              WRITE(OUTLYNE,*)
     1        'ENDING SURFACE NUMBER MUST BE LESS THAN OR EQUAL TO ',
     1        INT(SYSTEM1(20))
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(W1.GT.W2) THEN
              OUTLYNE=
     1        'THE ENDING SURFACE # MUST BE GREATER THAN OR EQUAL TO#'
              CALL SHOWIT(1)
              OUTLYNE=
     1        'THE STARTING SURFACE #'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          DO SF=INT(W1),INT(W2)
C
C       HERE IS WHERE THE DECISION ON SPECIFIC SUBROUTINE
C       CALLS IS MADE FOR THE PROPER DISPOSITION OF PIKUP
C       INPUT DATA.
C
C       DECIDE WHICH PARAMETER PIKUP IS TO BE DELETED
C       IF SQ=0, ALL PIKUPS MUST GO
              IF(SQ.EQ.0) THEN
                  IF(ALENS(32,SF).EQ.0.0D0) THEN
C
C       NO PIKUPS TO DELETE
C
                      WRITE(OUTLYNE,*)'SURFACE',SF,' :NO PIKUPS TO BE DELETED'
                      CALL SHOWIT(1)
                  END IF
                  ALENS(32,SF)=0.0D0
                  PIKUP(1:6,SF,1:PSIZ)=0.0D0

              ELSE
C       SQ NOT 0 SO THERE WAS A QUALIFIER AND ONLY SOME OF THE
C       PIKUPS MUST GO.
C
                  K=-1
                  IF(WQ.EQ.'RD      ') K=1
                  IF(WQ.EQ.'CV      ') K=2
                  IF(WQ.EQ.'TH      ') K=3
                  IF(WQ.EQ.'CC      ') K=4
                  IF(WQ.EQ.'AD      ') K=5
                  IF(WQ.EQ.'AE      ') K=6
                  IF(WQ.EQ.'AF      ') K=7
                  IF(WQ.EQ.'AG      ') K=8
                  IF(WQ.EQ.'CVTOR   ') K=9
                  IF(WQ.EQ.'RDTOR   ') K=10
                  IF(WQ.EQ.'PRO     ') K=11
                  IF(WQ.EQ.'NPRO    ') K=12
                  IF(WQ.EQ.'YD      ') K=13
                  IF(WQ.EQ.'XD      ') K=14
                  IF(WQ.EQ.'ALPHA   ') K=15
                  IF(WQ.EQ.'BETA    ') K=16
                  IF(WQ.EQ.'GAMMA   ') K=17
                  IF(WQ.EQ.'CLAP    ') K=18
                  IF(WQ.EQ.'COBS    ') K=19
                  IF(WQ.EQ.'GLASS   ') K=20
                  IF(WQ.EQ.'CCTOR   ') K=21
                  IF(WQ.EQ.'ADTOR   ') K=22
                  IF(WQ.EQ.'AETOR   ') K=23
                  IF(WQ.EQ.'AFTOR   ') K=24
                  IF(WQ.EQ.'AGTOR   ') K=25
                  IF(WQ.EQ.'AC      ') K=26
                  IF(WQ.EQ.'AH      ') K=27
                  IF(WQ.EQ.'AI      ') K=28
                  IF(WQ.EQ.'AJ      ') K=29
                  IF(WQ.EQ.'AK      ') K=30
                  IF(WQ.EQ.'AL      ') K=31
                  IF(WQ.EQ.'THOAL   ') K=32
                  IF(WQ.EQ.'ZD      ') K=33
                  IF(WQ.EQ.'PIVX    ') K=34
                  IF(WQ.EQ.'PIVY    ') K=35
                  IF(WQ.EQ.'PIVZ    ') K=36
                  IF(WQ.EQ.'GDX     ') K=37
                  IF(WQ.EQ.'GDY     ') K=38
                  IF(WQ.EQ.'GDZ     ') K=39
                  IF(WQ.EQ.'GALPHA  ') K=40
                  IF(WQ.EQ.'GBETA   ') K=41
                  IF(WQ.EQ.'GGAMMA  ') K=42
                  IF(WQ.EQ.'GRT     ') K=43
                  IF(WQ.EQ.'COATING ') K=44
                  IF(K.EQ.-1) THEN
                      OUTLYNE='INVALID QUALIFIER WORD ISSUED WITH "PIKD"'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
C
                  IF(PIKUP(1,SF,K).EQ.0.0D0) THEN
C
C       PIKUP NOT THERE TO BE DELETED
                      WRITE(OUTLYNE,*)
     1                'SURFACE',SF,' :',TRIM(WQ),' PIKUP TO BE DELETED WAS NOT FOUND'
                      CALL SHOWIT(1)
                  ELSE
                      PIKUP(1:6,SF,K)=0.0D0
                      WRITE(OUTLYNE,*)'SURFACE',SF,' ',WQ,' :PIKUP DELETED'
                      CALL SHOWIT(1)
C
C       DECREMENT THE PIKUP COUNTER
C
                      ALENS(32,SF)=ALENS(32,SF)-1.0D0
                  END IF
C
C       NOW CHECK IF THERE ARE ANY PIKUPS LEFT.
C       IF NOT,SET ALENS(32,SF) TO 0.0D0
C
                  PIKCNT=0
                  DO 4 I=1,PSIZ
                      IF(PIKUP(1,SF,I).NE.0.0D0) THEN
                          PIKCNT=PIKCNT+1
                      ELSE
                      END IF
 4                CONTINUE
                  IF(PIKCNT.EQ.0) ALENS(32,SF)=0.0D0
              END IF
C       THATS IT,ALL DONE!
          END DO
C
          RETURN
      END
      SUBROUTINE LINKIT(LERROR)
          IMPLICIT NONE
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          INTEGER IVAL
          LOGICAL LERROR
          CHARACTER IAVAL*3
          SAVE_KDP(1)=SAVEINPT(1)
          INPUT='UPDATE LENS'
          CALL PROCES
          REST_KDP(1)=RESTINPT(1)
          IVAL=INT(W1)
          IF(IVAL.LT.0.OR.IVAL.GT.SYSTEM1(20))
     1    OUTLYNE='INVALID TARGET SURFACE NUMBER FOR LINKING'
          IF(IVAL.LT.0.OR.IVAL.GT.SYSTEM1(20))
     1    CALL SHOWIT(1)
          IF(IVAL.LT.0.OR.IVAL.GT.SYSTEM1(20)) THEN
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
          END IF
          LERROR=.TRUE.
          CALL MACFAL
          CALL NTOAN1(IVAL,IAVAL)
          IF(WC.EQ.'LINK') THEN
              SAVE_KDP(1)=SAVEINPT(1)
              INPUT='CHG '//IAVAL
              CALL PROCES
              REST_KDP(1)=RESTINPT(1)
          END IF
          IF(WC.EQ.'LINK') THEN
              SAVE_KDP(1)=SAVEINPT(1)
              W1=W2
              W2=W3
              W3=W4
              W4=W5
              S1=S2
              S2=S3
              S3=S4
              S4=S5
              DF1=DF2
              DF2=DF3
              DF3=DF4
              DF4=DF5
              DF5=1
              S5=0
              W5=0.0D0
              IF(WC.EQ.'LINK') WC='PIKUP'
              IF(WC.EQ.'PIKUP')CALL SPIKUP
              REST_KDP(1)=RESTINPT(1)
          END IF
          IF(WC.EQ.'LINKD') WC='PIKD'
          IF(WC.EQ.'PIKD')CALL SPIKD
          SAVE_KDP(1)=SAVEINPT(1)
          INPUT='EOS'
          CALL PROCES
          REST_KDP(1)=RESTINPT(1)
          LERROR=.FALSE.
          RETURN
      END
C SUB PIKANG.FOR
      SUBROUTINE PIKANG
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE PIKANG. THIS IS THE SUBROUTINE WHICH
C       HANDLES ALPHA,BETA AND GAMMA ANGLE PIKUP AT THE LENS INPUT
C       AND UPDATE LENS LEVEL. IF THE SURFACE TO WHICH THE PIKUP
C       IS GOING IS NOT DEFINED AS TILTED, THE PIKUP IS NOT ALLOWED.
C     AND ALSO GLOBAL TILTS
C
          INTEGER FSURF,CT
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
C
C       FILE LENSSTORE.DOC CONTAINS INFORMATION ON THE
C       SPECIFIC STORAGE LOCATIONS FOR LENS DATA,SOLVE AND
C       PIKUP ITEMS.
C
C       TEST TO SEE IF THE SURFACE FROM WHICH WE ARE PIKING UP
C       DATA IS THE SAME AS THE SURFACE TO WHICH THE PIKUP
C       WILL BE ASSINGED. OPERATION NOT LEGAL
C
          IF(SURF.EQ.INT(W1)) THEN
              OUTLYNE='A SURFACE CAN NOT PIKUP ITS OWN VALUES'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
          IF(WQ.EQ.'ALPHA') CT=15
          IF(WQ.EQ.'BETA') CT=16
          IF(WQ.EQ.'GAMMA') CT=17
          IF(WQ.EQ.'GALPHA') CT=40
          IF(WQ.EQ.'GBETA') CT=41
          IF(WQ.EQ.'GGAMMA') CT=42
C
C       THE PIKUP EXISTENCE FLAG IS PIKUP(1,I,J) FOR EVERY
C       PIKUP ON EVERY SURFACE. IF IT IS 1.0D0 THE PIKUP EXISTS
C       IF IT IS 0.0D0 THE PIKUP DOES NOT EXIST
C
C       STORE NUMERIC INPUT AND HANDEL DEFAULT DATA

C       IF DF1=1 THIS MEANS PIKING UP FROM THE OBJECT SURFACE
C       ON TO THE CURRENT SURFACE
          IF(DF1.EQ.1) W1=0.0D0
C       IF DF2=1 THIS MEANS MULTIPLIER IS UNITY
          IF(DF2.EQ.1) W2=1.0D0
C       IF DF3=1 TIS MEANS ADDITIVE CONSTANT ZERO
          IF(DF3.EQ.1) W3=0.0D0
C       IF DF4=1 THEN W4=0.0D0
C       IF DF5=1 THEN W5=0.0D0
          IF(DF4.EQ.1) W4=0.0D0
          IF(DF5.EQ.1) W5=0.0D0
          IF(W1.LT.0.0D0) W1=SURF+W1
          IF(W1.LT.0.0D0.OR.W1.GT.SYSTEM1(20)) THEN
              OUTLYNE='SOURCE SURFACE NUMBER BEYOND LEGAL RANGE'
              CALL SHOWIT(1)
              OUTLYNE='PIKUP NOT ALLOWED'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
C       IF ALENS(25,SURF)=0.0D0 THEN THE SURFACE WAS NOT TILTED
C       SINCE THE SURFACE COULD BE ONE OF SEVERAL TILT TYPES
C       THE PIKUP IS NOT ALLOWED TILL THE TILT DEFINITION
C       IS MADE.
C
          IF(ALENS(25,SURF).EQ.0.0D0) THEN
              WRITE(OUTLYNE,*)'SURFACE',SURF,' :SURFACE NOT DEFINED TILTED'
              CALL SHOWIT(1)
              IF(WQ.EQ.'ALPHA') OUTLYNE='PIKUP (ALPHA) NOT ALLOWED'
              IF(WQ.EQ.'BETA')  OUTLYNE='PIKUP (BETA) NOT ALLOWED'
              IF(WQ.EQ.'GAMMA') OUTLYNE='PIKUP (GAMMA) NOT ALLOWED'
              IF(WQ.EQ.'GALPHA')OUTLYNE='PIKUP (GALPHA) NOT ALLOWED'
              IF(WQ.EQ.'GBETA') OUTLYNE='PIKUP (GBETA) NOT ALLOWED'
              IF(WQ.EQ.'GGAMMA')OUTLYNE='PIKUP (GGAMMA) NOT ALLOWED'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
C       THE SURFACE BEING PIKED UP FROM MUST
C       BE DEFINED AS TILTED OR THE
C       PIKUP IS NOT ALLOWED.
C
          FSURF=INT(W1)
          IF(ALENS(25,FSURF).EQ.0D0) THEN
C
              IF(WQ.EQ.'ALPHA') THEN
                  OUTLYNE=
     1            'SURFACE TO BE PIKED UP FROM IS NOT DEFINED AS TILTED'
                  CALL SHOWIT(1)
                  IF(WQ.EQ.'ALPHA ')  OUTLYNE='PIKUP (ALPHA) NOT ALLOWED'
                  IF(WQ.EQ.'BETA  ')  OUTLYNE='PIKUP (BETA) NOT ALLOWED'
                  IF(WQ.EQ.'GAMMA ')  OUTLYNE='PIKUP (GAMMA) NOT ALLOWED'
                  IF(WQ.EQ.'GALPHA')  OUTLYNE='PIKUP (GALPHA) NOT ALLOWED'
                  IF(WQ.EQ.'GBETA ')  OUTLYNE='PIKUP (GBETA) NOT ALLOWED'
                  IF(WQ.EQ.'GGAMMA')  OUTLYNE='PIKUP (GGAMMA) NOT ALLOWED'
                  CALL SHOWIT(1)
              END IF
              CALL MACFAL
              RETURN
          END IF
C
C               INCR.PIKUP INDICATOR BY 1.0D0
C               I.E. ALENS(32,SURF)
          ALENS(32,SURF)=ALENS(32,SURF)+1.0D0
          PIKUP(1,SURF,CT)=1.0D0
          PIKUP(2,SURF,CT)=W1
          PIKUP(3,SURF,CT)=W2
          IF(WQ.EQ.'ALPHA')
     1    PIKUP(4,SURF,CT)=W3
          IF(WQ.EQ.'BETA')
     1    PIKUP(4,SURF,CT)=W3
          IF(WQ.EQ.'GALPHA')
     1    PIKUP(4,SURF,CT)=W3
          IF(WQ.EQ.'GBETA')
     1    PIKUP(4,SURF,CT)=W3
          PIKUP(5,SURF,CT)=W4
          PIKUP(6,SURF,CT)=W5
C
C       IF THE SURFACE HAS A TILT AUTO THEN REMOVE THE
C       AUTO AND PRINT MESSAGE
C
          IF(ALENS(25,SURF).EQ.2.0D0.OR.ALENS(25,SURF).EQ.
     1    3.0D0) THEN
              ALENS(25,SURF)=1.0D0
              IF(ALENS(25,SURF).EQ.2.0D0) WRITE(OUTLYNE,*)
     1        '"AUTO" ADJUST REMOVED FROM SURFACE TILT DEFINITION'
              IF(ALENS(25,SURF).EQ.3.0D0) WRITE(OUTLYNE,*)
     1        '"AUTOM" ADJUST REMOVED FROM SURFACE TILT DEFINITION'
              CALL SHOWIT(1)
          END IF
C       THERE ARE NO SOLVE DELETIONS TO HANDEL.
C
          RETURN
      END
C SUB SPIKUP.FOR
      SUBROUTINE SPIKUP
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE SPIKUP. THIS IS THE SUBROUTINE WHICH
C       HANDLES THE LENS PARAMETER PIKUPS AT THE LENS INPUT
C       AND UPDATE LENS LEVEL.
C
          INTEGER I
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
          IF(STI.EQ.1) THEN
 200          FORMAT('FROM SURFACE #',I3)
 300          FORMAT('A(MULT) = ',G23.15)
 400          FORMAT('B(ADD) = ',G23.15)
              DO I=1,43
                  IF(PIKUP(1,SURF,I).EQ.1.0D0) THEN
C       PIKUP EXISTS, WRITE SOMETHING
C
                      IF(I.EQ.1) THEN
                          WRITE(OUTLYNE,101)SURF
                          CALL SHOWIT(0)
                          WRITE(OUTLYNE,200) INT(PIKUP(2,SURF,I))
                          CALL SHOWIT(0)
                          WRITE(OUTLYNE,300) (PIKUP(3,SURF,I))
                          CALL SHOWIT(0)
                          WRITE(OUTLYNE,400) (PIKUP(4,SURF,I))
                          CALL SHOWIT(0)
 101                      FORMAT('"RD" PIKUP ON SURFACE #',I3)
                      ELSE
                      END IF
C
                      IF(I.EQ.2) THEN
                          WRITE(OUTLYNE,102)SURF
                          CALL SHOWIT(0)
                          WRITE(OUTLYNE,200) INT(PIKUP(2,SURF,I))
                          CALL SHOWIT(0)
                          WRITE(OUTLYNE,300) (PIKUP(3,SURF,I))
                          CALL SHOWIT(0)
                          WRITE(OUTLYNE,400) (PIKUP(4,SURF,I))
                          CALL SHOWIT(0)
 102                      FORMAT('"CV" PIKUP ON SURFACE #',I3)
                      ELSE
                      END IF
C
                      IF(I.EQ.3) THEN
                          WRITE(OUTLYNE,103)SURF
                          CALL SHOWIT(0)
                          WRITE(OUTLYNE,200) INT(PIKUP(2,SURF,I))
                          CALL SHOWIT(0)
                          WRITE(OUTLYNE,300) (PIKUP(3,SURF,I))
                          CALL SHOWIT(0)
                          WRITE(OUTLYNE,400) (PIKUP(4,SURF,I))
                          CALL SHOWIT(0)
 103                      FORMAT('"TH" PIKUP ON SURFACE #',I3)
                      ELSE
                      END IF
C
                      IF(I.EQ.32) THEN
                          WRITE(OUTLYNE,901)SURF
                          CALL SHOWIT(0)
                          WRITE(OUTLYNE,902) INT(PIKUP(2,SURF,I))
                          CALL SHOWIT(0)
                          WRITE(OUTLYNE,903) INT(PIKUP(3,SURF,I))
                          CALL SHOWIT(0)
                          WRITE(OUTLYNE,904) (PIKUP(4,SURF,I))
                          CALL SHOWIT(0)
                          WRITE(OUTLYNE,905) (PIKUP(5,SURF,I))
                          CALL SHOWIT(0)
 902                      FORMAT('FROM SURFACE #',I3)
 903                      FORMAT('  TO SURFACE #',I3)
 904                      FORMAT('A(MULT) = ',G23.15)
 905                      FORMAT('B(ADD) = ',G23.15)
 901                      FORMAT('"THOAL" PIKUP ON SURFACE #',I3)
                      ELSE
                      END IF
C
                      IF(I.EQ.4) THEN
                          WRITE(OUTLYNE,104)SURF
                          CALL SHOWIT(0)
                          WRITE(OUTLYNE,200) INT(PIKUP(2,SURF,I))
                          CALL SHOWIT(0)
                          WRITE(OUTLYNE,300) (PIKUP(3,SURF,I))
                          CALL SHOWIT(0)
                          WRITE(OUTLYNE,400) (PIKUP(4,SURF,I))
                          CALL SHOWIT(0)
 104                      FORMAT('"CC" PIKUP ON SURFACE #',I3)
                      ELSE
                      END IF
C
                      IF(I.EQ.5) THEN
                          WRITE(OUTLYNE,105)SURF
                          CALL SHOWIT(0)
                          WRITE(OUTLYNE,200) INT(PIKUP(2,SURF,I))
                          CALL SHOWIT(0)
                          WRITE(OUTLYNE,300) (PIKUP(3,SURF,I))
                          CALL SHOWIT(0)
                          WRITE(OUTLYNE,400) (PIKUP(4,SURF,I))
                          CALL SHOWIT(0)
 105                      FORMAT('"AD" PIKUP ON SURFACE #',I3)
                      ELSE
                      END IF
C
                      IF(I.EQ.6) THEN
                          WRITE(OUTLYNE,106)SURF
                          CALL SHOWIT(0)
                          WRITE(OUTLYNE,200) INT(PIKUP(2,SURF,I))
                          CALL SHOWIT(0)
                          WRITE(OUTLYNE,300) (PIKUP(3,SURF,I))
                          CALL SHOWIT(0)
                          WRITE(OUTLYNE,400) (PIKUP(4,SURF,I))
                          CALL SHOWIT(0)
 106                      FORMAT('"AE" PIKUP ON SURFACE #',I3)
                      ELSE
                      END IF
C
                      IF(I.EQ.7) THEN
                          WRITE(OUTLYNE,107)SURF
                          CALL SHOWIT(0)
                          WRITE(OUTLYNE,200) INT(PIKUP(2,SURF,I))
                          CALL SHOWIT(0)
                          WRITE(OUTLYNE,300) (PIKUP(3,SURF,I))
                          CALL SHOWIT(0)
                          WRITE(OUTLYNE,400) (PIKUP(4,SURF,I))
                          CALL SHOWIT(0)
 107                      FORMAT('"AF" PIKUP ON SURFACE #',I3)
                      ELSE
                      END IF
C
                      IF(I.EQ.8) THEN
                          WRITE(OUTLYNE,108)SURF
                          CALL SHOWIT(0)
                          WRITE(OUTLYNE,200) INT(PIKUP(2,SURF,I))
                          CALL SHOWIT(0)
                          WRITE(OUTLYNE,300) (PIKUP(3,SURF,I))
                          CALL SHOWIT(0)
                          WRITE(OUTLYNE,400) (PIKUP(4,SURF,I))
                          CALL SHOWIT(0)
 108                      FORMAT('"AG" PIKUP ON SURFACE #',I3)
                      ELSE
                      END IF
C
                      IF(I.EQ.9) THEN
                          WRITE(OUTLYNE,109)SURF
                          CALL SHOWIT(0)
                          WRITE(OUTLYNE,200) INT(PIKUP(2,SURF,I))
                          CALL SHOWIT(0)
                          WRITE(OUTLYNE,300) (PIKUP(3,SURF,I))
                          CALL SHOWIT(0)
                          WRITE(OUTLYNE,400) (PIKUP(4,SURF,I))
                          CALL SHOWIT(0)
 109                      FORMAT('"CVTOR" PIKUP ON SURFACE #',I3)
                      ELSE
                      END IF
C
                      IF(I.EQ.10) THEN
                          WRITE(OUTLYNE,110)SURF
                          CALL SHOWIT(0)
                          WRITE(OUTLYNE,200) INT(PIKUP(2,SURF,I))
                          CALL SHOWIT(0)
                          WRITE(OUTLYNE,300) (PIKUP(3,SURF,I))
                          CALL SHOWIT(0)
                          WRITE(OUTLYNE,400) (PIKUP(4,SURF,I))
                          CALL SHOWIT(0)
 110                      FORMAT('"RDTOR" PIKUP ON SURFACE #',I3)
                      ELSE
                      END IF
C
                      IF(I.EQ.11) THEN
                          WRITE(OUTLYNE,111)SURF
                          CALL SHOWIT(0)
                          WRITE(OUTLYNE,200) INT(PIKUP(2,SURF,I))
                          CALL SHOWIT(0)
 111                      FORMAT('"PRO" PIKUP ON SURFACE #',I3)
                      ELSE
                      END IF
C
                      IF(I.EQ.12) THEN
                          WRITE(OUTLYNE,112)SURF
                          CALL SHOWIT(0)
                          WRITE(OUTLYNE,200) INT(PIKUP(2,SURF,I))
                          CALL SHOWIT(0)
 112                      FORMAT('"NPRO" PIKUP ON SURFACE #',I3)
                      ELSE
                      END IF
C
                      IF(I.EQ.13) THEN
                          WRITE(OUTLYNE,113)SURF
                          CALL SHOWIT(0)
                          WRITE(OUTLYNE,200) INT(PIKUP(2,SURF,I))
                          CALL SHOWIT(0)
                          WRITE(OUTLYNE,300) (PIKUP(3,SURF,I))
                          CALL SHOWIT(0)
                          WRITE(OUTLYNE,400) (PIKUP(4,SURF,I))
                          CALL SHOWIT(0)
 113                      FORMAT('"YD" PIKUP ON SURFACE #',I3)
                      ELSE
                      END IF
C
                      IF(I.EQ.14) THEN
                          WRITE(OUTLYNE,114)SURF
                          CALL SHOWIT(0)
                          WRITE(OUTLYNE,200) INT(PIKUP(2,SURF,I))
                          CALL SHOWIT(0)
                          WRITE(OUTLYNE,300) (PIKUP(3,SURF,I))
                          CALL SHOWIT(0)
                          WRITE(OUTLYNE,400) (PIKUP(4,SURF,I))
                          CALL SHOWIT(0)
 114                      FORMAT('"XD" PIKUP ON SURFACE #',I3)
                      ELSE
                      END IF
C
                      IF(I.EQ.15) THEN
                          WRITE(OUTLYNE,115)SURF
                          CALL SHOWIT(0)
                          WRITE(OUTLYNE,200) INT(PIKUP(2,SURF,I))
                          CALL SHOWIT(0)
                          WRITE(OUTLYNE,300) (PIKUP(3,SURF,I))
                          CALL SHOWIT(0)
                          WRITE(OUTLYNE,400) (PIKUP(4,SURF,I))
                          CALL SHOWIT(0)
 115                      FORMAT('"ALPHA" PIKUP ON SURFACE #',I3)
                      ELSE
                      END IF
C
                      IF(I.EQ.16) THEN
                          WRITE(OUTLYNE,116)SURF
                          CALL SHOWIT(0)
                          WRITE(OUTLYNE,200) INT(PIKUP(2,SURF,I))
                          CALL SHOWIT(0)
                          WRITE(OUTLYNE,300) (PIKUP(3,SURF,I))
                          CALL SHOWIT(0)
                          WRITE(OUTLYNE,400) (PIKUP(4,SURF,I))
                          CALL SHOWIT(0)
 116                      FORMAT('"BETA" PIKUP ON SURFACE #',I3)
                      ELSE
                      END IF
C
                      IF(I.EQ.17) THEN
                          WRITE(OUTLYNE,117)SURF
                          CALL SHOWIT(0)
                          WRITE(OUTLYNE,200) INT(PIKUP(2,SURF,I))
                          CALL SHOWIT(0)
                          WRITE(OUTLYNE,300) (PIKUP(3,SURF,I))
                          CALL SHOWIT(0)
                          WRITE(OUTLYNE,400) (PIKUP(4,SURF,I))
                          CALL SHOWIT(0)
 117                      FORMAT('"GAMMA" PIKUP ON SURFACE #',I3)
                      ELSE
                      END IF
C
                      IF(I.EQ.18) THEN
                          WRITE(OUTLYNE,118)SURF
                          CALL SHOWIT(0)
                          WRITE(OUTLYNE,200) INT(PIKUP(2,SURF,I))
                          CALL SHOWIT(0)
 118                      FORMAT('"CLAP" PIKUP ON SURFACE #',I3)
                      ELSE
                      END IF
C
                      IF(I.EQ.19) THEN
                          WRITE(OUTLYNE,119)SURF
                          CALL SHOWIT(0)
                          WRITE(OUTLYNE,200) INT(PIKUP(2,SURF,I))
                          CALL SHOWIT(0)
 119                      FORMAT('"COBS" PIKUP ON SURFACE #',I3)
                      ELSE
                      END IF
C
                      IF(I.EQ.20) THEN
                          WRITE(OUTLYNE,120)SURF
                          CALL SHOWIT(0)
                          WRITE(OUTLYNE,200) INT(PIKUP(2,SURF,I))
                          CALL SHOWIT(0)
 120                      FORMAT('"GLASS" PIKUP ON SURFACE #',I3)
                      ELSE
                      END IF
C
                      IF(I.EQ.21) THEN
                          WRITE(OUTLYNE,121)SURF
                          CALL SHOWIT(0)
                          WRITE(OUTLYNE,200) INT(PIKUP(2,SURF,I))
                          CALL SHOWIT(0)
                          WRITE(OUTLYNE,300) (PIKUP(3,SURF,I))
                          CALL SHOWIT(0)
                          WRITE(OUTLYNE,400) (PIKUP(4,SURF,I))
                          CALL SHOWIT(0)
 121                      FORMAT('"CCTOR" PIKUP ON SURFACE #',I3)
                      ELSE
                      END IF
C
                      IF(I.EQ.22) THEN
                          WRITE(OUTLYNE,122)SURF
                          CALL SHOWIT(0)
                          WRITE(OUTLYNE,200) INT(PIKUP(2,SURF,I))
                          CALL SHOWIT(0)
                          WRITE(OUTLYNE,300) (PIKUP(3,SURF,I))
                          CALL SHOWIT(0)
                          WRITE(OUTLYNE,400) (PIKUP(4,SURF,I))
                          CALL SHOWIT(0)
 122                      FORMAT('"ADTOR" PIKUP ON SURFACE #',I3)
                      ELSE
                      END IF
C
                      IF(I.EQ.23) THEN
                          WRITE(OUTLYNE,123)SURF
                          CALL SHOWIT(0)
                          WRITE(OUTLYNE,200) INT(PIKUP(2,SURF,I))
                          CALL SHOWIT(0)
                          WRITE(OUTLYNE,300) (PIKUP(3,SURF,I))
                          CALL SHOWIT(0)
                          WRITE(OUTLYNE,400) (PIKUP(4,SURF,I))
                          CALL SHOWIT(0)
 123                      FORMAT('"AETOR" PIKUP ON SURFACE #',I3)
                      ELSE
                      END IF
C
                      IF(I.EQ.24) THEN
                          WRITE(OUTLYNE,124)SURF
                          CALL SHOWIT(0)
                          WRITE(OUTLYNE,200) INT(PIKUP(2,SURF,I))
                          CALL SHOWIT(0)
                          WRITE(OUTLYNE,300) (PIKUP(3,SURF,I))
                          CALL SHOWIT(0)
                          WRITE(OUTLYNE,400) (PIKUP(4,SURF,I))
                          CALL SHOWIT(0)
 124                      FORMAT('"AFTOR" PIKUP ON SURFACE #',I3)
                      ELSE
                      END IF
C
                      IF(I.EQ.25) THEN
                          WRITE(OUTLYNE,125)SURF
                          CALL SHOWIT(0)
                          WRITE(OUTLYNE,200) INT(PIKUP(2,SURF,I))
                          CALL SHOWIT(0)
                          WRITE(OUTLYNE,300) (PIKUP(3,SURF,I))
                          CALL SHOWIT(0)
                          WRITE(OUTLYNE,400) (PIKUP(4,SURF,I))
                          CALL SHOWIT(0)
 125                      FORMAT('"AGTOR" PIKUP ON SURFACE #',I3)
                      ELSE
                      END IF
C
                      IF(I.EQ.26) THEN
                          WRITE(OUTLYNE,126)SURF
                          CALL SHOWIT(0)
                          WRITE(OUTLYNE,200) INT(PIKUP(2,SURF,I))
                          CALL SHOWIT(0)
                          WRITE(OUTLYNE,300) (PIKUP(3,SURF,I))
                          CALL SHOWIT(0)
                          WRITE(OUTLYNE,400) (PIKUP(4,SURF,I))
                          CALL SHOWIT(0)
 126                      FORMAT('"AC" PIKUP ON SURFACE #',I3)
                      ELSE
                      END IF
C
                      IF(I.EQ.27) THEN
                          WRITE(OUTLYNE,127)SURF
                          CALL SHOWIT(0)
                          WRITE(OUTLYNE,200) INT(PIKUP(2,SURF,I))
                          CALL SHOWIT(0)
                          WRITE(OUTLYNE,300) (PIKUP(3,SURF,I))
                          CALL SHOWIT(0)
                          WRITE(OUTLYNE,400) (PIKUP(4,SURF,I))
                          CALL SHOWIT(0)
 127                      FORMAT('"AH" PIKUP ON SURFACE #',I3)
                      ELSE
                      END IF
C
                      IF(I.EQ.28) THEN
                          WRITE(OUTLYNE,128)SURF
                          CALL SHOWIT(0)
                          WRITE(OUTLYNE,200) INT(PIKUP(2,SURF,I))
                          CALL SHOWIT(0)
                          WRITE(OUTLYNE,300) (PIKUP(3,SURF,I))
                          CALL SHOWIT(0)
                          WRITE(OUTLYNE,400) (PIKUP(4,SURF,I))
                          CALL SHOWIT(0)
 128                      FORMAT('"AI" PIKUP ON SURFACE #',I3)
                      ELSE
                      END IF
C
                      IF(I.EQ.29) THEN
                          WRITE(OUTLYNE,129)SURF
                          CALL SHOWIT(0)
                          WRITE(OUTLYNE,200) INT(PIKUP(2,SURF,I))
                          CALL SHOWIT(0)
                          WRITE(OUTLYNE,300) (PIKUP(3,SURF,I))
                          CALL SHOWIT(0)
                          WRITE(OUTLYNE,400) (PIKUP(4,SURF,I))
                          CALL SHOWIT(0)
 129                      FORMAT('"AJ" PIKUP ON SURFACE #',I3)
                      ELSE
                      END IF
C
                      IF(I.EQ.30) THEN
                          WRITE(OUTLYNE,130)SURF
                          CALL SHOWIT(0)
                          WRITE(OUTLYNE,200) INT(PIKUP(2,SURF,I))
                          CALL SHOWIT(0)
                          WRITE(OUTLYNE,300) (PIKUP(3,SURF,I))
                          CALL SHOWIT(0)
                          WRITE(OUTLYNE,400) (PIKUP(4,SURF,I))
                          CALL SHOWIT(0)
 130                      FORMAT('"AK" PIKUP ON SURFACE #',I3)
                      ELSE
                      END IF
C
                      IF(I.EQ.31) THEN
                          WRITE(OUTLYNE,131)SURF
                          CALL SHOWIT(0)
                          WRITE(OUTLYNE,200) INT(PIKUP(2,SURF,I))
                          CALL SHOWIT(0)
                          WRITE(OUTLYNE,300) (PIKUP(3,SURF,I))
                          CALL SHOWIT(0)
                          WRITE(OUTLYNE,400) (PIKUP(4,SURF,I))
                          CALL SHOWIT(0)
 131                      FORMAT('"AL" PIKUP ON SURFACE #',I3)
                      ELSE
                      END IF
                      IF(I.EQ.33) THEN
                          WRITE(OUTLYNE,133)SURF
                          CALL SHOWIT(0)
                          WRITE(OUTLYNE,200) INT(PIKUP(2,SURF,I))
                          CALL SHOWIT(0)
                          WRITE(OUTLYNE,300) (PIKUP(3,SURF,I))
                          CALL SHOWIT(0)
                          WRITE(OUTLYNE,400) (PIKUP(4,SURF,I))
                          CALL SHOWIT(0)
 133                      FORMAT('"ZD" PIKUP ON SURFACE #',I3)
                      ELSE
                      END IF
                      IF(I.EQ.34) THEN
                          WRITE(OUTLYNE,134)SURF
                          CALL SHOWIT(0)
                          WRITE(OUTLYNE,200) INT(PIKUP(2,SURF,I))
                          CALL SHOWIT(0)
                          WRITE(OUTLYNE,300) (PIKUP(3,SURF,I))
                          CALL SHOWIT(0)
                          WRITE(OUTLYNE,400) (PIKUP(4,SURF,I))
                          CALL SHOWIT(0)
 134                      FORMAT('"PIVX" PIKUP ON SURFACE #',I3)
                      ELSE
                      END IF
                      IF(I.EQ.35) THEN
                          WRITE(OUTLYNE,135)SURF
                          CALL SHOWIT(0)
                          WRITE(OUTLYNE,200) INT(PIKUP(2,SURF,I))
                          CALL SHOWIT(0)
                          WRITE(OUTLYNE,300) (PIKUP(3,SURF,I))
                          CALL SHOWIT(0)
                          WRITE(OUTLYNE,400) (PIKUP(4,SURF,I))
                          CALL SHOWIT(0)
 135                      FORMAT('"PIVY" PIKUP ON SURFACE #',I3)
                      ELSE
                      END IF
                      IF(I.EQ.36) THEN
                          WRITE(OUTLYNE,136)SURF
                          CALL SHOWIT(0)
                          WRITE(OUTLYNE,200) INT(PIKUP(2,SURF,I))
                          CALL SHOWIT(0)
                          WRITE(OUTLYNE,300) (PIKUP(3,SURF,I))
                          CALL SHOWIT(0)
                          WRITE(OUTLYNE,400) (PIKUP(4,SURF,I))
                          CALL SHOWIT(0)
 136                      FORMAT('"PIVZ" PIKUP ON SURFACE #',I3)
                      ELSE
                      END IF
                      IF(I.EQ.37) THEN
                          WRITE(OUTLYNE,137)SURF
                          CALL SHOWIT(0)
                          WRITE(OUTLYNE,200) INT(PIKUP(2,SURF,I))
                          CALL SHOWIT(0)
                          WRITE(OUTLYNE,300) (PIKUP(3,SURF,I))
                          CALL SHOWIT(0)
                          WRITE(OUTLYNE,400) (PIKUP(4,SURF,I))
                          CALL SHOWIT(0)
 137                      FORMAT('"GDX" PIKUP ON SURFACE #',I3)
                      ELSE
                      END IF
                      IF(I.EQ.38) THEN
                          WRITE(OUTLYNE,138)SURF
                          CALL SHOWIT(0)
                          WRITE(OUTLYNE,200) INT(PIKUP(2,SURF,I))
                          CALL SHOWIT(0)
                          WRITE(OUTLYNE,300) (PIKUP(3,SURF,I))
                          CALL SHOWIT(0)
                          WRITE(OUTLYNE,400) (PIKUP(4,SURF,I))
                          CALL SHOWIT(0)
 138                      FORMAT('"GDY" PIKUP ON SURFACE #',I3)
                      ELSE
                      END IF
                      IF(I.EQ.39) THEN
                          WRITE(OUTLYNE,139)SURF
                          CALL SHOWIT(0)
                          WRITE(OUTLYNE,200) INT(PIKUP(2,SURF,I))
                          CALL SHOWIT(0)
                          WRITE(OUTLYNE,300) (PIKUP(3,SURF,I))
                          CALL SHOWIT(0)
                          WRITE(OUTLYNE,400) (PIKUP(4,SURF,I))
                          CALL SHOWIT(0)
 139                      FORMAT('"GDZ" PIKUP ON SURFACE #',I3)
                      ELSE
                      END IF
                      IF(I.EQ.40) THEN
                          WRITE(OUTLYNE,140)SURF
                          CALL SHOWIT(0)
                          WRITE(OUTLYNE,200) INT(PIKUP(2,SURF,I))
                          CALL SHOWIT(0)
                          WRITE(OUTLYNE,300) (PIKUP(3,SURF,I))
                          CALL SHOWIT(0)
                          WRITE(OUTLYNE,400) (PIKUP(4,SURF,I))
                          CALL SHOWIT(0)
 140                      FORMAT('"GALPHA" PIKUP ON SURFACE #',I3)
                      ELSE
                      END IF
                      IF(I.EQ.41) THEN
                          WRITE(OUTLYNE,141)SURF
                          CALL SHOWIT(0)
                          WRITE(OUTLYNE,200) INT(PIKUP(2,SURF,I))
                          CALL SHOWIT(0)
                          WRITE(OUTLYNE,300) (PIKUP(3,SURF,I))
                          CALL SHOWIT(0)
                          WRITE(OUTLYNE,400) (PIKUP(4,SURF,I))
                          CALL SHOWIT(0)
 141                      FORMAT('"GBETA" PIKUP ON SURFACE #',I3)
                      ELSE
                      END IF
                      IF(I.EQ.42) THEN
                          WRITE(OUTLYNE,142)SURF
                          CALL SHOWIT(0)
                          WRITE(OUTLYNE,200) INT(PIKUP(2,SURF,I))
                          CALL SHOWIT(0)
                          WRITE(OUTLYNE,300) (PIKUP(3,SURF,I))
                          CALL SHOWIT(0)
                          WRITE(OUTLYNE,400) (PIKUP(4,SURF,I))
                          CALL SHOWIT(0)
 142                      FORMAT('"GGAMMA" PIKUP ON SURFACE #',I3)
                      ELSE
                      END IF
                      IF(I.EQ.43) THEN
                          WRITE(OUTLYNE,143)SURF
                          CALL SHOWIT(0)
                          WRITE(OUTLYNE,200) INT(PIKUP(2,SURF,I))
                          CALL SHOWIT(0)
 143                      FORMAT('"GRT" PIKUP ON SURFACE #',I3)
                      ELSE
                      END IF
                      IF(I.EQ.44) THEN
                          WRITE(OUTLYNE,144)SURF
                          CALL SHOWIT(0)
                          WRITE(OUTLYNE,200) INT(PIKUP(2,SURF,I))
                          CALL SHOWIT(0)
 144                      FORMAT('"COATING" PIKUP ON SURFACE #',I3)
                      ELSE
                      END IF
C
                  ELSE
C       GO TO NEXT I
                  END IF
              END DO
              RETURN
          ELSE
          END IF
C       FILE LENSSTORE.DOC CONTAINS INFORMATION ON THE
C       SPECIFIC STORAGE LOCATIONS FOR LENS DATA,SOLVE AND
C       PIKUP ITEMS.
C
C       IF THE FIRST NUMERIC WORD OF A PIKUP RECORD IS
C       NEGATIVE, THE PIKUP REFERS TO THE SURFACE WHICH IS
C       BACK IN THE LENS BY THAT MANY SURFACES. SET THAT
C       ABSOLUTE ADDRESS NOW.
C       SURF IS THE SURFACE UPON WHICH THE PIKUP WILL BE
C       PLACED. SO IF W1=-3.0D0, W1 SHOULD BE RESET TO
C       W1=DBLE(SURF)+W1 AND ONLY IF W1 IS LESS THAN ZERO.
C
          IF(W1.LT.0.0D0) W1=DBLE(SURF)+W1
C
C       THE PIKUP COMMAND NEVER TAKES STRING INPUT
C
          IF(SST.EQ.1) THEN
              OUTLYNE='"PIKUP" TAKES NO STRING INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
C       HERE IS WHERE THE DECISION ON SPECIFIC SUBROUTINE
C       CALLS IS MADE FOR THE PROPER DISPOSITION OF PIKUP
C       INPUT DATA.
C
C       PIKUP TH
          IF(WQ.EQ.'TH') THEN
              CALL PIKTH
              RETURN
          ELSE
          END IF
C       PIKUP TH
          IF(WQ.EQ.'THOAL') THEN
              CALL PIKTHOAL
              RETURN
          ELSE
          END IF
C       PIKUP RD OR PIKUP CV
          IF(WQ.EQ.'RD'.OR.WQ.EQ.'CV') THEN
              IF(SURF.EQ.0) THEN
                  OUTLYNE=
     1            '"'//WQ(1:2)//'" PIKUP NOT ALLOWED ON OBJECT SURFACE'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              CALL PIKCV
              RETURN
          END IF
C       PIKUP CC
          IF(WQ.EQ.'CC'.OR.WC.EQ.'CCTOR') THEN
              IF(SURF.EQ.0) THEN
                  IF(WQ.EQ.'CC') OUTLYNE=
     1            '"'//WQ(1:2)//'" PIKUP NOT ALLOWED ON OBJECT SURFACE'
                  IF(WQ.EQ.'CCTOR') OUTLYNE=
     1            '"'//WQ(1:5)//'" PIKUP NOT ALLOWED ON OBJECT SURFACE'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              CALL PIKCON
              RETURN
          END IF
C       PIKUP ASPHERIC TERMS
          IF(WQ.EQ.'AD'.OR.WQ.EQ.'AE'.OR.WQ.EQ.'AF'
     1    .OR.WQ.EQ.'AG'.OR.WQ.EQ.'AC'.OR.WQ.EQ.'AH'.OR.WQ.EQ.'AI'
     2    .OR.WQ.EQ.'AJ'.OR.WQ.EQ.'AK'.OR.WQ.EQ.'AL') THEN
              IF(SURF.EQ.0) THEN
                  OUTLYNE=
     1            '"'//WQ(1:2)//'" PIKUP NOT ALLOWED ON OBJECT SURFACE'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              CALL PIKASP
              RETURN
          END IF
C       PIKUP ASPHERIC TORIC TERMS
          IF(WQ.EQ.'ADTOR'.OR.WQ.EQ.'AETOR'.OR.WQ.EQ.'AFTOR'
     1    .OR.WQ.EQ.'AGTOR') THEN
              IF(SURF.EQ.0) THEN
                  OUTLYNE=
     1            '"'//WQ(1:5)//'" PIKUP NOT ALLOWED ON OBJECT SURFACE'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              CALL PIKASP
              RETURN
          END IF
C       PIKUP CVTOR OR RDTOR
          IF(WQ.EQ.'CVTOR'.OR.WQ.EQ.'RDTOR') THEN
              IF(SURF.EQ.0) THEN
                  OUTLYNE=
     1            '"'//WQ(1:5)//'" PIKUP NOT ALLOWED ON OBJECT SURFACE'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              CALL PIKCVT
              RETURN
          END IF
C       PIKUP PRO OR NPRO
          IF(WQ.EQ.'PRO'.OR.WQ.EQ.'NPRO') THEN
              IF(SURF.EQ.0.AND.WQ.EQ.'PRO') THEN
                  OUTLYNE='"PRO" PIKUP NOT ALLOWED ON OBJECT SURFACE'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(SURF.EQ.0.AND.WQ.EQ.'NPRO') THEN
                  OUTLYNE='"NPRO" PIKUP NOT ALLOWED ON OBJECT SURFACE'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              CALL PIKPRO
              RETURN
          ELSE
          END IF
C       PIKUP XD YD ZD
          IF(WQ.EQ.'XD'.OR.WQ.EQ.'YD'.OR.WQ.EQ.'ZD') THEN
              IF(SURF.EQ.0) THEN
                  OUTLYNE=
     1            '"'//WQ(1:2)//'" PIKUP NOT ALLOWED ON OBJECT SURFACE'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              CALL PIKXYD
              RETURN
          END IF
C       PIKUP GDX, GDY, GDZ
          IF(WQ.EQ.'GDX'.OR.WQ.EQ.'GDY'.OR.WQ.EQ.'GDZ') THEN
              IF(SURF.EQ.0) THEN
                  OUTLYNE=
     1            '"'//WQ(1:3)//'" PIKUP NOT ALLOWED ON OBJECT SURFACE'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              CALL PIKXYD
              RETURN
          END IF
C       PIKUP ALPHA,BETA,GAMMA
          IF(WQ.EQ.'ALPHA'.OR.WQ.EQ.'BETA'.OR.WQ.EQ.
     1    'GAMMA'.OR.WQ.EQ.'GALPHA'.OR.WQ.EQ.'GBETA'.OR.
     2    WQ.EQ.'GGAMMA') THEN
              IF(SURF.EQ.0) THEN
                  IF(WQ.EQ.'ALPHA')
     1            OUTLYNE='"ALPHA" PIKUP NOT ALLOWED ON OBJECT SURFACE'
                  IF(WQ.EQ.'BETA')
     1            OUTLYNE='"BETA" PIKUP NOT ALLOWED ON OBJECT SURFACE'
                  IF(WQ.EQ.'GAMMA')
     1            OUTLYNE='"GAMMA" PIKUP NOT ALLOWED ON OBJECT SURFACE'
                  IF(WQ.EQ.'GALPHA')
     1            OUTLYNE='"GALPHA" PIKUP NOT ALLOWED ON OBJECT SURFACE'
                  IF(WQ.EQ.'GBETA')
     1            OUTLYNE='"GBETA" PIKUP NOT ALLOWED ON OBJECT SURFACE'
                  IF(WQ.EQ.'GGAMMA')
     1            OUTLYNE='"GGAMMA" PIKUP NOT ALLOWED ON OBJECT SURFACE'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(ALENS(25,SURF).NE.6.0D0) THEN
              ELSE
                  IF(WQ.EQ.'ALPHA')
     1            OUTLYNE='"ALPHA" PIKUP NOT ALLOWED ON A "TILT RET" SURFACE'
                  IF(WQ.EQ.'BETA')
     1            OUTLYNE='"BETA" PIKUP NOT ALLOWED ON A "TILT RET" SURFACE'
                  IF(WQ.EQ.'GAMMA')
     1            OUTLYNE='"GAMMA" PIKUP NOT ALLOWED ON A "TILT RET" SURFACE'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(WQ.EQ.'ALPHA'.OR.WQ.EQ.'BETA'.OR.WQ.EQ.'GAMMA'.OR.
     1        WQ.EQ.'GALPHA'.OR.WQ.EQ.'GBETA'.OR.WQ.EQ.'GGAMMA') CALL PIKANG
              RETURN
          ELSE
          END IF
C       PIKUP CLAP AND COBS
          IF(WQ.EQ.'CLAP'.OR.WQ.EQ.'COBS') THEN
              CALL PIKAPE
              RETURN
          ELSE
          END IF
C       PIKUP GLASS
          IF(WQ.EQ.'GLASS') THEN
              CALL PIKGLS
              RETURN
          ELSE
          END IF
C       PIKUP COATING
          IF(WQ.EQ.'COATING') THEN
              CALL PIKCOAT
              RETURN
          ELSE
          END IF
C       PIKUP GRT
          IF(WQ.EQ.'GRT') THEN
              CALL PIKGRT
              RETURN
          ELSE
          END IF
C       PIKUP PIVX OR PIVY OR PIVZ
          IF(WQ.EQ.'PIVX'.OR.WQ.EQ.'PIVY'.OR.WQ.EQ.'PIVZ') THEN
              CALL PIKPXYD
              RETURN
          ELSE
          END IF
          OUTLYNE='INVALID QUALIFIER FOUND'
          CALL SHOWIT(1)
          OUTLYNE='RE-ENTER COMMAND'
          CALL SHOWIT(1)
          CALL MACFAL
          RETURN
      END
