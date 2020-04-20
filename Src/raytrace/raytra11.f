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

C      THIS IS THE ELEVENTH FILE OF RAYTRACING ROUTINES

C SUB SAVE_RAY_DATA.FOR
      SUBROUTINE SAVE_RAY_DATA
C
          IMPLICIT NONE
C
C     THIS COMMAND, "SAVERAY" CAUSES ALL OF THE DATA ASSOCIATED WITH THE
C     LAST RAY AND IT'S ASSOCIATED CHIEF RAY AND ALL ASSOCIATED DIFFERENTIAL
C     RAY DATA TO BE SAVED FOR LATER USE. EVERYTHING IS SAVED INCLUDING
C     THE SURFACE NUMBER WHERE RAY TRACING IS TO RESUME IF IT HAD NOT BEEN
C     COMPLETED TO THE FINAL SURFACE
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
          IF(STI.EQ.1) THEN
              OUTLYNE=
     1        '"SAVERAY" SAVES EXISTING RAY AND REFERENCE RAY DATA FOR'
              CALL SHOWIT(1)
              OUTLYNE='LATER USE.'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C       CHECK FOR INPUT
          IF(SST.EQ.1.OR.SN.EQ.1.OR.SQ.EQ.1) THEN
              OUTLYNE='"SAVERAY" TAKES NO INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(.NOT.REFEXT.AND..NOT.RAYEXT) THEN
              OUTLYNE='NO RAY DATA EXISTS TO SAVE WITH "SAVERAY"'
              CALL SHOWIT(1)
              OUTLYNE='NO ACTION TAKEN'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(REFEXT.AND..NOT.RAYEXT) THEN
              OUTLYNE=
     1        'ONLY REFERENCE RAY DATA EXISTS TO SAVE WITH "SAVERAY"'
              CALL SHOWIT(1)
          END IF
          IF(.NOT.REFEXT.AND.RAYEXT) THEN
              OUTLYNE='ONLY RAY DATA EXISTS TO SAVE WITH "SAVERAY"'
              CALL SHOWIT(1)
          END IF
          IF(REFEXT.AND.RAYEXT) THEN
              OUTLYNE='SAVING THE CURRENT RAY AND REFERENCE RAY DATA'
              CALL SHOWIT(1)
          END IF
C     HERE IS WHERE THE CODE GOES TO SAVE THE REFERENCE RAY DATA
          CALL LASTRAY(1)
          RETURN
      END
C SUB REST_RAY_DATA.FOR
      SUBROUTINE REST_RAY_DATA
C
          IMPLICIT NONE
C
C     THIS COMMAND, "RESTRAY" CAUSES ALL OF THE DATA ASSOCIATED WITH THE
C     LAST RAY AND IT'S ASSOCIATED CHIEF RAY AND ALL ASSOCIATED DIFFERENTIAL
C     RAY DATA TO BE RESTORED FOR LATER USE. EVERYTHING IS RESTORED INCLUDING
C     THE SURFACE NUMBER WHERE RAY TRACING IS TO RESUME IF IT HAD NOT BEEN
C     COMPLETED TO THE FINAL SURFACE
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
          IF(STI.EQ.1) THEN
              OUTLYNE=
     1        '"RESTRAY" RESTORES SAVED RAY AND REFERENCE RAY DATA'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C       CHECK FOR INPUT
          IF(SST.EQ.1.OR.SN.EQ.1.OR.SQ.EQ.1) THEN
              OUTLYNE='"RESTRAY" TAKES NO INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(.NOT.SREFEXT.AND..NOT.SRAYEXT) THEN
              OUTLYNE='NO RAY DATA EXISTS TO RESTORE WITH "RESTRAY"'
              CALL SHOWIT(1)
              OUTLYNE='NO ACTION TAKEN'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SREFEXT.AND..NOT.SRAYEXT) THEN
              OUTLYNE=
     1        'ONLY REFERENCE RAY DATA EXISTS TO RESTORE WITH "RESTRAY"'
              CALL SHOWIT(1)
          END IF
          IF(.NOT.SREFEXT.AND.SRAYEXT) THEN
              OUTLYNE='ONLY RAY DATA EXISTS TO RESTORE WITH "RESTRAY"'
              CALL SHOWIT(1)
          END IF
          IF(SREFEXT.AND.SRAYEXT) THEN
              OUTLYNE='RESTORING SAVED RAY AND REFERENCE RAY DATA'
              CALL SHOWIT(1)
          END IF
C     HERE IS WHERE THE CODE GOES TO RESTORE THE SAVED RAY DATA
          CALL LASTRAY(2)
          RETURN
      END
C SUB CLEAR_RAY_DATA.FOR
      SUBROUTINE CLEAR_RAY_DATA
C
          IMPLICIT NONE
C
C     THIS COMMAND, "CLEARRAY" CAUSES ALL OF THE DATA ASSOCIATED WITH THE
C     LAST RAY AND IT'S ASSOCIATED CHIEF RAY AND THE ASSOCIATED ALLOCATED
C     ARRAY MEMORY TO BE CLEARED AND DEALLOCATED
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
          IF(STI.EQ.1) THEN
              OUTLYNE=
     1        '"CLEARRAY" CLEARS SAVED RAY AND REFERENCE RAY DATA MEMORY'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C       CHECK FOR INPUT
          IF(SST.EQ.1.OR.SN.EQ.1.OR.SQ.EQ.1) THEN
              OUTLYNE='"CLEARRAY" TAKES NO INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(.NOT.SREFEXT.AND..NOT.SRAYEXT) THEN
              OUTLYNE='NO SAVED RAY DATA EXISTS TO CLEAR WITH "CLEARRAY"'
              CALL SHOWIT(1)
              OUTLYNE='NO ACTION TAKEN'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          OUTLYNE=
     1    'SAVED RAY AND REFERENCE RAY DATA MEMORY CLEARED'
          CALL SHOWIT(1)
          CALL LASTRAY(3)
          RETURN
      END



      SUBROUTINE LASTRAY(ICODE)
          IMPLICIT NONE
          INTEGER ICODE,I,J
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          LOGICAL SLDIF,SLDIF2
C     ICODE = 1, SAVE RAY DATA
          IF(ICODE.EQ.1) THEN
              IF(REFEXT) THEN
                  SREFEXT=.TRUE.
                  SLFOB=LFOB
                  SLFOBA=LFOBA
                  OLD_REF=REFRY
                  OLD_REF_DIF=RFDIFF
              END IF
              IF(RAYEXT) THEN
                  SRAYEXT=.TRUE.
                  OLD_RAY=RAYRAY
                  O_PXTRAY=PXTRAY
                  O_PXTRAX=PXTRAX
                  OLD_RAY_DIF=DIFF
              END IF
              SLDIF=LDIF
              SLDIF2=LDIF2
              RETURN
          END IF
C     ICODE = 2, RESTORE SAVED RAY DATA
          IF(ICODE.EQ.2) THEN
              IF(SREFEXT) THEN
                  REFEXT=.TRUE.
                  SLDIF=.TRUE.                 !Add by ENDO
                  SLDIF2=.TRUE.                !Add by ENDO

                  DO I=1,7
                      LFOB(I)=SLFOB(I)
                  END DO
                  DO I=1,3
                      LFOBA(I)=SLFOBA(I)
                  END DO
                  DO J=0,INT(SYSTEM1(20))
                      DO I=1,35
                          REFRY(I,J)=OLD_REF(I,J)
                      END DO
                  END DO
                  DO J=0,INT(SYSTEM1(20))
                      DO I=1,18
                          RFDIFF(I,J)=OLD_REF_DIF(I,J)
                      END DO
                  END DO
              END IF
              IF(SRAYEXT) THEN
                  RAYEXT=.TRUE.
                  DO J=0,INT(SYSTEM1(20))
                      DO I=1,35
                          RAYRAY(I,J)=OLD_RAY(I,J)
                      END DO
                      DO I=1,8
                          PXTRAY(I,J)=O_PXTRAY(I,J)
                          PXTRAX(I,J)=O_PXTRAX(I,J)
                      END DO
                  END DO
                  DO J=0,INT(SYSTEM1(20))
                      DO I=1,18
                          DIFF(I,J)=OLD_RAY_DIF(I,J)
                      END DO
                  END DO
              END IF
              LDIF=SLDIF
              LDIF2=SLDIF2
              RETURN
          END IF
C     ICODE = 3, CLEAR RAY DATA SAVE ARRAYS
          IF(ICODE.EQ.3) THEN
              SREFEXT=.FALSE.
              SRAYEXT=.FALSE.
              SLDIF=.FALSE.
              SLDIF2=.FALSE.
              DO J=0,MAXSUR
                  DO I=1,35
                      OLD_RAY(I,J)=0.0D0
                      OLD_REF(I,J)=0.0D0
                  END DO
                  DO I=1,8
                      O_PXTRAY(I,J)=0.0D0
                      O_PXTRAX(I,J)=0.0D0
                  END DO
              END DO
              DO J=0,MAXSUR
                  DO I=1,18
                      OLD_RAY_DIF(I,J)=0.0D0
                      OLD_REF_DIF(I,J)=0.0D0
                  END DO
              END DO
              RETURN
          END IF
      END
C SUB OLDREFDAT.FOR
      SUBROUTINE OLDREFDAT
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE OLDREFDAT.FOR. THIS SUBROUTINE
C     SETS USEOLREF ON OR OFF
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
C       CHECK FOR STRING INPUT
          IF(STI.EQ.1) THEN
              IF(.NOT.USEOLREF) WRITE(OUTLYNE,10)
              IF(USEOLREF) WRITE(OUTLYNE,11)
              CALL SHOWIT(0)
 10           FORMAT('USEOLREF IS CURRENTLY TURNED "OFF"')
 11           FORMAT('USEOLREF IS CURRENTLY TURNED "ON"')
              RETURN
          END IF
          IF(SST.EQ.1) THEN
              OUTLYNE='"USEOLREF" TAKES NO STRING INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(WQ.EQ.'ON'.OR.WQ.EQ.'OFF') THEN
              IF(SN.EQ.1) THEN
                  OUTLYNE='"USEOLREF ("ON" OR "OFF")'//
     1            ' TAKES NO NUMERIC INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
          IF(STI.EQ.0) THEN
              IF(WQ.NE.'ON'.AND.WQ.NE.'OFF') THEN
                  OUTLYNE='"ON", "YES", "NO" AND "OFF"ARE THE ONLY'
                  CALL SHOWIT(1)
                  OUTLYNE='VALID QUALIFIERS USED WITH "USEOLREF"'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C     STI=1
          END IF
          IF(WQ.EQ.'OFF') THEN
              USEOLREF=.FALSE.
              SAVEREF=.FALSE.
          END IF
          IF(WQ.EQ.'ON') THEN
              USEOLREF=.TRUE.
          END IF
          RETURN
      END
C SUB SAVEREFDATA.FOR
      SUBROUTINE SAVEREFDATA
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE SAVEREF.FOR. THIS SUBROUTINE
C       CAUSES ALL REFERENCE RAY DATA TO BE SAVED
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
          IF(STI.EQ.1) THEN
              OUTLYNE='"SAVEREF" SAVES EXISTING REFERENCE RAY DATA FOR'
              CALL SHOWIT(1)
              OUTLYNE='USE IN FUTURE CAPFN CALCULATIONS'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C       CHECK FOR INPUT
          IF(SST.EQ.1.OR.SN.EQ.1.OR.SQ.EQ.1) THEN
              OUTLYNE='"SAVEREF" TAKES NO INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(.NOT.REFEXT) THEN
              OUTLYNE='NO REFERENCE RAY DATA EXISTS TO SAVE WITH "SAVEREF"'
              CALL SHOWIT(1)
              OUTLYNE='NO ACTION TAKEN'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C     HERE IS WHERE THE CODE GOES TO SAVE THE REFERENCE RAY DATA
          SAVEREF=.TRUE.
          CALL SAVE_CHIEF_RAY_DATA
          RETURN
      END
C SUB NOAIMM.FOR
      SUBROUTINE NOAIMM
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE NOAIMM.FOR. THIS SUBROUTINE STOPS ALL
C     RAY AIMING AND SETS RAY AIMING OFFSETS
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
C       CHECK FOR STRING INPUT
          IF(SST.EQ.1) THEN
              OUTLYNE='"'//WC(1:6)//'" TAKES NO STRING INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(WQ.EQ.'OFFSET') THEN
              IF(S4.EQ.1.OR.S5.EQ.1) THEN
                  OUTLYNE='"AIMRAY OFFSET"'//
     1            ' TAKES NO NUMERIC WORD #4 OR #5 INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
          IF(WQ.EQ.'ON'.OR.WQ.EQ.'OFF') THEN
              IF(SN.EQ.1) THEN
                  OUTLYNE='"AIMRAY ("ON" OR "OFF")'//
     1            ' TAKES NO NUMERIC INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
          IF(STI.EQ.0) THEN
              IF(WQ.NE.'ON'.AND.WQ.NE.'OFF'.AND.WQ.NE.'OFFSET') THEN
                  OUTLYNE='"ON", "YES", "NO", "OFF" AND "OFFSET" ARE THE ONLY'
                  CALL SHOWIT(1)
                  OUTLYNE='VALID QUALIFIERS USED WITH "'//WC(1:6)//'"'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C     STI=1
          END IF
          IF(STI.EQ.1) THEN
              IF(SYSTEM1(62).EQ.0.0D0) WRITE(OUTLYNE,10)
              IF(SYSTEM1(62).EQ.1.0D0) WRITE(OUTLYNE,11)
              CALL SHOWIT(0)
 10           FORMAT('RAY AIMING IS CURRENTLY TURNED "OFF"')
 11           FORMAT('RAY AIMING IS CURRENTLY TURNED "ON"')
              WRITE(OUTLYNE,12) SYSTEM1(81)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,13) SYSTEM1(82)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,14) SYSTEM1(89)
              CALL SHOWIT(0)
 12           FORMAT('CURRENT RAY AIMING X-OFFSET = ',D23.15,' LENS UNITS')
 13           FORMAT('CURRENT RAY AIMING Y-OFFSET = ',D23.15,' LENS UNITS')
 14           FORMAT('CURRENT RAY AIMING Z-OFFSET = ',D23.15,' LENS UNITS')
              RETURN
          END IF
          IF(WQ.EQ.'OFF') THEN
              SYSTEM1(62)=0.0D0
              IF(F12.EQ.1) SYSP(62)=0.0D0
          END IF
          IF(WQ.EQ.'ON') THEN
              SYSTEM1(62)=1.0D0
              SYSTEM1(70)=0.0D0
              SYSTEM1(63)=0.0D0
              IF(F12.EQ.1) SYSP(62)=1.0D0
              IF(F12.EQ.1) SYSP(63)=0.0D0
          END IF
          IF(WQ.EQ.'OFFSET') THEN
              IF(DF1.EQ.1)W1=0.0D0
              IF(DF2.EQ.1)W2=0.0D0
              IF(DF3.EQ.1)W3=0.0D0
              SYSTEM1(81)=W1
              SYSTEM1(82)=W2
              SYSTEM1(89)=W3
              IF(F12.EQ.1) SYSP(81)=W1
              IF(F12.EQ.1) SYSP(82)=W2
              IF(F12.EQ.1) SYSP(89)=W3
          END IF
          RETURN
      END
C SUB MTFGRID.FOR
      SUBROUTINE MTFGRID
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE MTFGRID.FOR. THIS SUBROUTINE
C     SETS THE FLAG THAT CAUSES A GRID TO BE DRAWN ON MTF PLOTS
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
          IF(STI.EQ.1) THEN
              IF(.NOT.MTFGRIDS) WRITE(OUTLYNE,10)
              IF(MTFGRIDS) WRITE(OUTLYNE,11)
              CALL SHOWIT(0)
 10           FORMAT('GRID PLOTTING IS CURRENTLY TURNED "OFF"')
 11           FORMAT('GRID PLOTTING IS CURRENTLY TURNED "ON"')
              RETURN
          END IF
          IF(WQ.NE.'ON'.AND.WQ.NE.'OFF') THEN
              OUTLYNE='"ON", "YES", "NO", "OFF" AND "OFFSET" ARE THE ONLY'
              CALL SHOWIT(1)
              OUTLYNE='VALID QUALIFIERS USED WITH "GRID"'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C       CHECK FOR STRING INPUT
          IF(SST.EQ.1) THEN
              OUTLYNE='"GRID" TAKES NO STRING INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SN.EQ.1) THEN
              OUTLYNE='"GRID" TAKES NO NUMERIC INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(WQ.EQ.'OFF') THEN
              MTFGRIDS=.FALSE.
          END IF
          IF(WQ.EQ.'ON') THEN
              MTFGRIDS=.TRUE.
          END IF
          RETURN
      END
C SUB NOAIMALP.FOR
      SUBROUTINE NOAIMAPL
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE NOAIMAPL.FOR.
C     THIS SETS APLANATIC RAY AIMING ON OR OFF
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
C       CHECK FOR STRING INPUT
          IF(SST.EQ.1) THEN
              OUTLYNE='"'//WC(1:6)//'" TAKES NO STRING INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(WQ.EQ.'ON'.OR.WQ.EQ.'OFF') THEN
              IF(SN.EQ.1) THEN
                  OUTLYNE='"AIMAPL ("ON" OR "OFF")'//
     1            ' TAKES NO NUMERIC INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
          IF(STI.EQ.0) THEN
              IF(WQ.NE.'ON'.AND.WQ.NE.'OFF') THEN
                  OUTLYNE='"ON", "YES", "NO" AND "OFF" ARE THE ONLY'
                  CALL SHOWIT(1)
                  OUTLYNE='VALID QUALIFIERS USED WITH "'//WC(1:6)//'"'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C     STI=1
          END IF
          IF(STI.EQ.1) THEN
              IF(SYSTEM1(70).EQ.0.0D0) WRITE(OUTLYNE,10)
              IF(SYSTEM1(70).EQ.1.0D0) WRITE(OUTLYNE,11)
              CALL SHOWIT(0)
 10           FORMAT('APLANATIC RAY AIMING IS CURRENTLY TURNED "OFF"')
 11           FORMAT('APLANATIC RAY AIMING IS CURRENTLY TURNED "ON"')
              RETURN
          END IF
          IF(WQ.EQ.'OFF') THEN
              SYSTEM1(70)=0.0D0
              IF(F12.EQ.1) SYSP(70)=0.0D0
          END IF
          IF(WQ.EQ.'ON') THEN
              SYSTEM1(70)=1.0D0
              SYSTEM1(62)=0.0D0
              SYSTEM1(63)=0.0D0
              IF(F12.EQ.1) SYSP(70)=1.0D0
          END IF
          RETURN
      END
C SUB SET_REVRAY.FOR
      SUBROUTINE SET_REVRAY
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE SET_REVRAY.FOR. THIS SUBROUTINE
C       TURNS SIMULATED REVERSE RAYTRACING ON OR OFF.
C       IT ONLY WORKS IF RXIM/YRIM ARE IN EFFECT
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
          IF(STI.EQ.1) THEN
              IF(SYSTEM1(100).EQ.0.0D0) WRITE(OUTLYNE,10)
              IF(SYSTEM1(100).EQ.1.0D0) WRITE(OUTLYNE,11)
              CALL SHOWIT(0)
 10           FORMAT('SIMULATED REVERSE RAY TRACING IS CURRENTLY TURNED "OFF"')
 11           FORMAT('SIMULATED REVERSE RAY TRACING IS CURRENTLY TURNED "ON"')
              RETURN
          END IF
C       CHECK FOR STRING INPUT
          IF(SST.EQ.1) THEN
              OUTLYNE='"REVRAY" TAKES NO STRING INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SN.EQ.1) THEN
              OUTLYNE='"REVRAY" TAKES NO NUMERIC INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SYSTEM1(98).EQ.0.0D0.OR.SYSTEM1(99).EQ.0.0D0) THEN
              OUTLYNE='"REVRAY" REQUIRES "RXIM" AND "RYIM" TO BE EXPLICITLY SET'
              CALL SHOWIT(1)
              OUTLYNE='NO ACTION TAKEN'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(STI.EQ.0) THEN
              IF(WQ.NE.'ON'.AND.WQ.NE.'OFF') THEN
                  OUTLYNE='"ON", "YES", "NO" AND "OFF" ARE THE ONLY'
                  CALL SHOWIT(1)
                  OUTLYNE='VALID QUALIFIERS USED WITH "REVRAY"'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C     STI=1
          END IF
          IF(WQ.EQ.'OFF') THEN
              SYSTEM1(100)=0.0D0
              IF(F12.EQ.1) SYSP(100)=0.0D0
          END IF
          IF(WQ.EQ.'ON') THEN
              SYSTEM1(100)=1.0D0
              IF(F12.EQ.1) SYSP(100)=1.0D0
          END IF
          RETURN
      END
C SUB MACFAIL.FOR
      SUBROUTINE MACFAIL
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE MACFAIL.FOR. THIS SUBROUTINE CONTROLS
C       THE BEHAVIOR WHEN MACFAL IS CALLED
C
          INCLUDE 'datmai.inc'
C
C       CHECK FOR STRING INPUT
          IF(SST.EQ.1.OR.SN.EQ.1) THEN
              OUTLYNE='"MACFAIL" TAKES NO STRING OR NUMERIC INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SQ.EQ.1) THEN
              IF(WQ.NE.'ON'.AND.WQ.NE.'OFF') THEN
                  OUTLYNE='"MACFAIL" TAKES "ON", "OFF", "YES" OR "NO"'
                  CALL SHOWIT(1)
                  OUTLYNE='AS VAILD QUALIFIER INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
          IF(STI.EQ.1.OR.SQ.EQ.0) THEN
              IF(MACFAILURE)      WRITE(OUTLYNE,10)
              IF(.NOT.MACFAILURE) WRITE(OUTLYNE,11)
              CALL SHOWIT(0)
 10           FORMAT('"MACFAIL" IS CURRENTLY ON')
 11           FORMAT('"MACFAIL" IS CURRENTLY OFF')
              RETURN
          END IF
          IF(WQ.EQ.'OFF')
     1    MACFAILURE=.FALSE.
          IF(WQ.EQ.'ON')
     1    MACFAILURE=.TRUE.
          RETURN
      END
C SUB OBJLEV.FOR
      SUBROUTINE OBJLEV
          USE GLOBALS
          IMPLICIT NONE
C
C       SETS AN INTENSITY LEVEL FOR THE CHIEF RAY AT THE OBJECT SURFACE
C
          INCLUDE 'datmai.inc'
C
C       CHECK FOR STRING INPUT
          IF(SST.EQ.1.OR.SN.EQ.1) THEN
              OUTLYNE='"OBJLEV" TAKES NO STRING OR QUALIFIER INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(STI.EQ.1.OR.SQ.EQ.0) THEN
              WRITE(OUTLYNE,*) OBJLEVEL
              CALL SHOWIT(0)
! 10   FORMAT('OBJECT LEVEL IS: ',G15.7)
              RETURN
          END IF
          IF(S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
              WRITE(OUTLYNE,*) '"OBJLEV" ONLY TAKES NUMERIC WORD #1 INPUT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*) 'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(W1.LT.0.0D0) W1=1.0D0
          IF(W1.GT.1.0D0) W1=1.0D0
          IF(DF1.EQ.1) W1=1.0D0
          OBJLEVEL=W1
          RETURN
      END

C SUB RHISTORY.FOR
      SUBROUTINE RHISTORY
          USE GLOBALS
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE RHIST.FOR. THIS SUBROUTINE STARTS
C     AND STOPS SEQUENTIAL RAY RAY HISTORY RECORDING
C
          INTEGER ALLOERR,J,K
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
C       CHECK FOR STRING INPUT
          IF(SST.EQ.1.OR.SN.EQ.1) THEN
              OUTLYNE='"RHIST" TAKES NO STRING OR NUMERIC INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(WQ.EQ.'ON'.OR.WQ.EQ.'OFF'.OR.WQ.EQ.'WRITE'.OR.
     1    WQ.EQ.'SWRITE') THEN
              IF(SN.EQ.1) THEN
                  OUTLYNE='"RHIST ("ON" OR "OFF" OR "WRITE" OR "SWRITE")'//
     1            ' TAKES NO NUMERIC INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
          IF(STI.EQ.0) THEN
              IF(WQ.NE.'ON'.AND.WQ.NE.'OFF'.AND.WQ.NE.'WRITE'.AND.
     1        WQ.NE.'SWRITE') THEN
                  OUTLYNE='"ON", "YES", "NO", "OFF", "WRITE" AND "SWRITE"'//
     1            'ARE THE ONLY'
                  CALL SHOWIT(1)
                  OUTLYNE='VALID QUALIFIERS USED WITH "'//WC(1:6)//'"'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C     STI=1
          END IF
          IF(STI.EQ.1) THEN
              IF(.NOT.RAY_HIST_FLAG) WRITE(OUTLYNE,10)
              IF(RAY_HIST_FLAG) WRITE(OUTLYNE,11)
              CALL SHOWIT(0)
 10           FORMAT('RAY HISTORY RECORDING IS CURRENTLY TURNED "OFF"')
 11           FORMAT('RAY HISTORY RECORDING IS CURRENTLY TURNED "ON"')
              RETURN
          END IF
          IF(WQ.EQ.'OFF') THEN
              RAY_HIST_FLAG=.FALSE.
              SHORTHIST=.FALSE.
              LONGHIST=.FALSE.
              RAY_HIST_NUM=0
              DEALLOCATE(RHIST,STAT=ALLOERR)
          END IF
          IF(WQ.EQ.'ON') THEN
              RAY_HIST_FLAG=.TRUE.
              SHORTHIST=.FALSE.
              LONGHIST=.FALSE.
              RAY_HIST_NUM=0
              DEALLOCATE(RHIST,STAT=ALLOERR)
              ALLOCATE(RHIST(1:93,1:RHIST_MAXRAYS,0:INT(SYSTEM1(20))),
     1        STAT=ALLOERR)
              RHIST(1:93,1:RHIST_MAXRAYS,0:INT(SYSTEM1(20)))=0.0D0
          END IF
          IF(WQ.EQ.'WRITE'.AND..NOT.RAY_HIST_FLAG) THEN
              OUTLYNE='NO RAY HISTORY DATA EXISTS TO BE WRITTEN TO RAYHIST.DAT'
              CALL SHOWIT(1)
              OUTLYNE='NO ACTION TAKEN'
              CALL SHOWIT(1)
              RETURN
          END IF
          IF(WQ.EQ.'WRITE'.AND.RAY_HIST_FLAG) THEN
              OPEN(UNIT=48,FILE=trim(HOME)//'RAYHIST.DAT')
              CALL CLOSE_FILE(48,0)
              OPEN(UNIT=48,FILE=trim(HOME)//'RAYHIST.DAT')
              OUTLYNE='WRITTING LONG LIST RAY HISTORIES TO RAYHIST.DAT...'
              CALL SHOWIT(1)
              WRITE(UNIT=48,FMT=121) RAY_HIST_NUM,INT(SYSTEM1(20)),FOBNUMBER
              DO K=1,RAY_HIST_NUM
                  DO J=1,INT(SYSTEM1(20))
                      WRITE(UNIT=48,FMT=12) J,K,RHIST(1:93,K,J)
                  END DO
              END DO
              LONGHIST=.TRUE.
              SHORTHIST=.FALSE.
 12           FORMAT(I3,I10,93(1X,D23.15))
 121          FORMAT(I10,I4,I10)
              OUTLYNE=
     1         'LONG LIST RAY HISTORIES HAVE BEEN WRITTEN TO RAYHIST.DAT'
              CALL SHOWIT(1)
              CALL CLOSE_FILE(48,1)
          END IF
          IF(WQ.EQ.'SWRITE'.AND.RAY_HIST_FLAG) THEN
              OPEN(UNIT=48,FILE=trim(HOME)//'RAYHIST.DAT')
              CALL CLOSE_FILE(48,0)
              OPEN(UNIT=48,FILE=trim(HOME)//'RAYHIST.DAT')
              OUTLYNE='WRITTING SHORT LIST RAY HISTORIES TO RAYHIST.DAT...'
              CALL SHOWIT(1)
              WRITE(UNIT=48,FMT=131) RAY_HIST_NUM,INT(SYSTEM1(20)),FOBNUMBER
              DO K=1,RAY_HIST_NUM
                  DO J=1,INT(SYSTEM1(20))
                      WRITE(UNIT=48,FMT=13) J,K,
     1                RHIST(1,K,J),
     1                RHIST(2,K,J),
     1                RHIST(3,K,J),
     1                RHIST(9,K,J),
     1                RHIST(25,K,J),
     1                RHIST(39,K,J),
     1                RHIST(87,K,J),
     1                RHIST(88,K,J),
     1                RHIST(89,K,J),
     1                RHIST(90,K,J),
     1                RHIST(91,K,J),
     1                RHIST(92,K,J),
     1                RHIST(93,K,J)
                  END DO
              END DO
              LONGHIST=.FALSE.
              SHORTHIST=.TRUE.
 131          FORMAT(I10,I4,I10)
 13           FORMAT(I3,I10,14(1X,D23.15))
              OUTLYNE=
     1         'SHORT LIST RAY HISTORIES HAVE BEEN WRITTEN TO RAYHIST.DAT'
              CALL SHOWIT(1)
              CALL CLOSE_FILE(48,1)
          END IF
          RETURN
      END
C SUB RHIST_INTENSITY.FOR
      SUBROUTINE RHIST_INTENSITY
          USE GLOBALS
C
          IMPLICIT NONE
C
          REAL*8 XDEL,YDEL,XCORRI,YCORRI,IMAX,IMIN
          REAL*8 RRXI,RRYI,IPTOV,IRMS,ITOTINTEN,ITOTRAYS
C       INTEN_ARRAY
C       DIM 1 IS THE X-INDEX (DIMENSIONED 1 TO INT(W2))
C       DIM 2 IS THE Y-INDEX (DIMENSIONED 1 TO INT(W2))
C       DIM 3 IS DATA TYPE DESIGNATOR (DIMENSIONED 1 TO 10)
C               1=TOTAL NUMBER OF RAYS IN EACH PIXEL
C               2=RAW SUMMED INTENSITIES
C               3=RAW SUMMED AOI FOR RAYS IN BOX
C               4=RAW SUMMED X-AOI FOR RAYS IN BOX
C               5=RAW SUMMED Y-AOI FOR RAYS IN BOX
C               6=SUMMED AND AVERAGED INTENSITIES (MORE THAN ONE RAY PER BOX)
C               7=AVERAGED AOI FOR RAYS IN BOX
C               8=AVERAGED X-AOI FOR RAYS IN BOX
C               9=AVERAGED Y-AOI FOR RAYS IN BOX
C              10=FOB#
C
          INTEGER ALLOERR,I,J,K,JJ,J1,K1,NUM_RAYS,NUM_SURFS,INTEN_NUM
          INTEGER NX,NY,IX,IY,NUMFOB,L
          LOGICAL XODD,YODD,EXIST102
C
C       THIS IS SUBROUTINE RHIST_INTENSITY WHICH CREATES AN INTEN.DAT
C     FILE FROM A SHORT RAYHIST.DAT FILE
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
          INTENEXIST=.FALSE.
C
C       CHECK FOR STRING INPUT
          IF(SST.EQ.1.OR.SQ.EQ.1) THEN
              OUTLYNE='"INTEN" TAKES NO STRING OR QUALIFIER INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(S4.EQ.1.OR.S5.EQ.1) THEN
              OUTLYNE='"INTEN" TAKES NO NUMERIC WORD #4 OR #5 INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(S1.EQ.0.OR.S2.EQ.0.OR.S3.EQ.0) THEN
              OUTLYNE='"INTEN" REQUIES EXPLICIT NUMERIC WORD #1 TO #3 INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(INT(W1).LT.NEWOBJ.OR.INT(W1).GT.NEWIMG) THEN
              OUTLYNE='SURFACE NUMBER BEYOND LEGAL RANGE'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(W2.LT.1.0D0) THEN
              OUTLYNE='NUMBER OF INTENSITY PIXELS MUST BE AT LEAST 1'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(W3.LT.0.0D0) THEN
              OUTLYNE='SIDE LENGTH OF THE INTENSITY ARRAY'
              CALL SHOWIT(1)
              OUTLYNE='MUST BE GREATER THAN ZERO'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(.NOT.SHORTHIST) THEN
              OUTLYNE='NO SHORT HISTORY FILE EXISTS TO BE PROCESSED'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
          INTENNUM_PIXELS=INT(DABS(W2))
          INTENLENGTH_ARRAY=DABS(W3)
          INTENPIXDIM=INTENLENGTH_ARRAY/DBLE(INTENNUM_PIXELS)
          CALL CLOSE_FILE(48,1)
          OPEN(UNIT=48,FILE=trim(HOME)//'RAYHIST.DAT')
          JJ=INT(W1)
          INTEN_NUM=0
          REWIND(UNIT=48)
          READ(UNIT=48,FMT=131) NUM_RAYS,NUM_SURFS,FOBNUMBER
          ALLOCATE(TEMPHIST(1:14,1:NUM_RAYS,0:NUM_SURFS),STAT=ALLOERR)
          ALLOCATE(TEMPINTEN(1:14,1:NUM_RAYS),STAT=ALLOERR)
          TEMPHIST(1:14,1:NUM_RAYS,0:NUM_SURFS)=0.0D0
          TEMPINTEN(1:14,1:NUM_RAYS)=0.0D0
          DO K=1,NUM_RAYS
              DO J=1,NUM_SURFS
                  READ(UNIT=48,FMT=13) J1,K1,TEMPHIST(1:14,K,J)
                  IF(J1.EQ.JJ) THEN
                      INTEN_NUM=INTEN_NUM+1
                      TEMPINTEN(1:14,INTEN_NUM)=TEMPHIST(1:14,K1,JJ)
                  END IF
              END DO
          END DO
          CALL CLOSE_FILE(48,1)
C       CREATE THE RHFOOT.DAT FILE FOR THE DESIGNATED SURFACE
          OPEN(UNIT=103,FILE=trim(HOME)//'RHFOOT.DAT')
          CALL CLOSE_FILE(103,0)
          OPEN(UNIT=103,FILE=trim(HOME)//'RHFOOT.DAT')
          WRITE(UNIT=103,FMT=*) INTEN_NUM
          DO I=1,INTEN_NUM
              IF(TEMPINTEN(5,I).GT.0.0D0)
     1        WRITE(UNIT=103,FMT=*) TEMPINTEN(1:2,I)
          END DO
          CALL CLOSE_FILE(103,1)
C       NOW ALLOCATE THE ARRAYS USED TO PROCESS THE INTENSITIES
C
          ALLOCATE(INTEN_YARRAY(1:INTENNUM_PIXELS,1:INTENNUM_PIXELS),
     1    STAT=ALLOERR)
          ALLOCATE(INTEN_XARRAY(1:INTENNUM_PIXELS,1:INTENNUM_PIXELS),
     1    STAT=ALLOERR)
          ALLOCATE(INTEN_ARRAY(1:INTENNUM_PIXELS,1:INTENNUM_PIXELS,1:10),
     1    STAT=ALLOERR)
          INTEN_XARRAY(1:INTENNUM_PIXELS,1:INTENNUM_PIXELS)=0.0D0
          INTEN_YARRAY(1:INTENNUM_PIXELS,1:INTENNUM_PIXELS)=0.0D0
          INTEN_ARRAY(1:INTENNUM_PIXELS,1:INTENNUM_PIXELS,1:10)=0.0D0
          XDEL=INTENPIXDIM
          YDEL=INTENPIXDIM
C       ARE THE MEM DIMENSIONS ODD OR EVEN
C       ODD OR EVEN
          NX=INTENNUM_PIXELS
          NY=INTENNUM_PIXELS
          XODD=.FALSE.
          IF((DBLE(NX)/2.0D0)-DBLE(NX/2).NE.0.0D0) XODD=.TRUE.
          YODD=.FALSE.
          IF((DBLE(NY)/2.0D0)-DBLE(NY/2).NE.0.0D0) YODD=.TRUE.
C
C       ESTABLISH THE PIXEL CENTER LOCATION VALUES
          IF(XODD) THEN
              DO I=0,NX-1
                  DO K=1,NY
                      INTEN_XARRAY(I+1,K)=-(XDEL*DBLE((NX-1)/2))+(DBLE(I)*XDEL)
                  END DO
              END DO
          END IF
          IF(.NOT.XODD) THEN
              DO I=1,NX
                  DO K=1,NY
                      INTEN_XARRAY(I,K)=-(XDEL*(DBLE(NX+1)/2.0D0))+(DBLE(I)*XDEL)
                  END DO
              END DO
          END IF
          IF(YODD) THEN
              DO I=1,NX
                  DO K=0,NY-1
                      INTEN_YARRAY(I,K+1)=-(YDEL*DBLE((NY-1)/2))+(DBLE(K)*YDEL)
                  END DO
              END DO
          END IF
          IF(.NOT.YODD) THEN
              DO I=1,NX
                  DO K=1,NY
                      INTEN_YARRAY(I,K)=-(YDEL*(DBLE(NY+1)/2.0D0))+(DBLE(K)*YDEL)
                  END DO
              END DO
          END IF
C
C       IS THERE A SCENE FILE. IF THERE IS, OPEN IT AND READ IT INTO THE
C       SCENE ARRAY
          EXIST102=.FALSE.
          INQUIRE(FILE=trim(HOME)//'SCENE.DAT',EXIST=EXIST102)
          IF(EXIST102) THEN
C       A SCENE.DAT FILE EXISTS
              CALL CLOSE_FILE(102,1)
              OPEN(UNIT=102,FILE=trim(HOME)//'SCENE.DAT')
              READ(UNIT=102,FMT=*) NUMFOB
              IF(NUMFOB.NE.FOBNUMBER) THEN
                  WRITE(OUTLYNE,*)
     1            'THE NUMBER OF ENTRIES IN THE SCENE.DAT FILE DOES NOT MATCH'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)
     1            'THE NUMBER OF FOBS IN THE SHORT RAYHIST.DAT FILE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)
     1            'INTENSITY CALCULATION ABORTED'
                  CALL SHOWIT(1)
                  DEALLOCATE(TEMPHIST,TEMPINTEN,INTEN_ARRAY,
     1            INTEN_XARRAY,INTEN_YARRAY
     2            ,STAT=ALLOERR)
                  CALL CLOSE_FILE(48,1)
                  CALL CLOSE_FILE(102,1)
                  CALL MACFAL
                  RETURN
              ELSE
C       FOB NUMBERS MATCH, PROCEED
              END IF
              ALLOCATE(SCENE_ARRAY(1:3,NUMFOB),STAT=ALLOERR)
              SCENE_ARRAY(1:3,1:NUMFOB)=0.0D0
              DO I=1,NUMFOB
                  READ(UNIT=102,FMT=*) SCENE_ARRAY(1,I)
              END DO
C       CLOSE THE SCENE FILE
              CALL CLOSE_FILE(102,1)
          ELSE
C       NO SCENE FILE, CREATE A UNIFORM SCENE_ARRAY
              ALLOCATE(SCENE_ARRAY(1:3,FOBNUMBER),STAT=ALLOERR)
              SCENE_ARRAY(1:3,1:FOBNUMBER)=1.0D0
              WRITE(OUTLYNE,*) 'NO SCENE.DAT FILE EXISTS'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*) 'A UNIFORM SCENE WILL BE ASSUMED'
              CALL SHOWIT(1)
          END IF
C       THE SCENE ARRAY IS LOADED
C       MULTIPLY IT INTO THE TEMPINTEN ARRAY
          WRITE(OUTLYNE,*) 'ADDING SCENE INTENSITIES. PLEASE WAIT...'
          CALL SHOWIT(1)
          DO I=1,NUM_RAYS
              DO K=1,FOBNUMBER
C       IF THE FOB NUMBER FOR ENTRY I IN THE TEMPINTEN
C       ARRAY IS THE SAME AS THE FOB NUMBER IN THE SCENE FILE
C       MULTIPLY THE SCENE INTENSITY INTO THE STORED RAY INTENSITY
C       THUS ADDING THE SCENE INFORMATION TO THE TEMPINTEN ARRAY.
                  IF(INT(TEMPINTEN(12,I)).EQ.SCENE_ARRAY(1,K)) THEN
                      TEMPINTEN(5,I)=SCENE_ARRAY(1,K)*TEMPINTEN(5,I)
                  END IF
              END DO
          END DO
          WRITE(OUTLYNE,*) 'SCENE DATA HAS BEEN ADDED'
          CALL SHOWIT(1)
C
C       WHAT PIXEL IS HIT BY THE RAY WITH COORDINATES RRX,RRY
          WRITE(OUTLYNE,*) 'CREATING FINAL INTENSITY ARRAY...'
          CALL SHOWIT(1)
          MAX_INTEN_NUM=0
          DO I=1,INTEN_NUM
              XCORRI=INTEN_XARRAY(1,1)
              YCORRI=INTEN_YARRAY(1,1)
              RRXI=TEMPINTEN(1,I)
              RRYI=TEMPINTEN(2,I)
              IX=NINT(((RRXI-XCORRI)/XDEL))+1
              IY=NINT(((RRYI-YCORRI)/YDEL))+1
              IF(IX.LT.1.OR.IX.GT.NX.OR.IY.LT.1.OR.IY.GT.NY) THEN
C       RAY WAS OUTSIDE THE INTENSITY ARRAY GRID AND WILL NOT BE FURTHER PROCESSED
              ELSE
C       PROCESS RAY
C
C       INTEN_ARRAY
C       DIM 1 IS THE X-INDEX (DIMENSIONED 1 TO INT(W2))
C       DIM 2 IS THE Y-INDEX (DIMENSIONED 1 TO INT(W2))
C       DIM 3 IS DATA TYPE DESIGNATOR (DIMENSIONED 1 TO 10)
C               1=TOTAL NUMBER OF RAYS IN EACH PIXEL
C               2=RAW SUMMED INTENSITIES
C               3=RAW SUMMED AOI FOR RAYS IN BOX
C               4=RAW SUMMED X-AOI FOR RAYS IN BOX
C               5=RAW SUMMED Y-AOI FOR RAYS IN BOX
C               6=SUMMED AND AVERAGED INTENSITIES (MORE THAN ONE RAY PER BOX)
C               7=AVERAGED AOI FOR RAYS IN BOX
C               8=AVERAGED X-AOI FOR RAYS IN BOX
C               9=AVERAGED Y-AOI FOR RAYS IN BOX
C              10=FOB NUMBER
C       IF IT IS A NON-FAILED RAY
                  IF(TEMPINTEN(13,I).EQ.0.0D0) THEN
C       RAYCOD(1) WAS 0, PROCEED
C       SAVE NUMBER OF RAYS
                      INTEN_ARRAY(IX,IY,1)=INTEN_ARRAY(IX,IY,1)+1.0D0
C       SAVE RAW INTENSITY SUM I IMES COS(AOI)
                      INTEN_ARRAY(IX,IY,2)=INTEN_ARRAY(IX,IY,2)+
     1                TEMPINTEN(5,I)*(TEMPINTEN(4,I))
C       SAVE RAW AOI SUM
                      INTEN_ARRAY(IX,IY,3)=INTEN_ARRAY(IX,IY,3)+
     1                DACOS(TEMPINTEN(4,I))
C       SAVE RAW X-AOI SUM
                      INTEN_ARRAY(IX,IY,4)=INTEN_ARRAY(IX,IY,4)+
     1                (DACOS(TEMPINTEN(4,I))*DSIN(TEMPINTEN(6,I)))
C       SAVE RAW Y-AOI SUM
                      INTEN_ARRAY(IX,IY,5)=INTEN_ARRAY(IX,IY,5)+
     1                (DACOS(TEMPINTEN(4,I))*DCOS(TEMPINTEN(6,I)))
C       SAVE SUMMED AND AVERAGED INTENSITY
                      INTEN_ARRAY(IX,IY,6)=INTEN_ARRAY(IX,IY,2)/INTEN_ARRAY(IX,IY,1)
C       SAVE SUMMED AND AVERAGED AOI
                      INTEN_ARRAY(IX,IY,7)=INTEN_ARRAY(IX,IY,4)/INTEN_ARRAY(IX,IY,1)
C       SAVE SUMMED AND AVERAGED X-AOI
                      INTEN_ARRAY(IX,IY,8)=INTEN_ARRAY(IX,IY,5)/INTEN_ARRAY(IX,IY,1)
C       SAVE SUMMED AND AVERAGED Y-AOI
                      INTEN_ARRAY(IX,IY,9)=INTEN_ARRAY(IX,IY,6)/INTEN_ARRAY(IX,IY,1)
C       FOB NUMBER
                      INTEN_ARRAY(IX,IY,10)=TEMPINTEN(12,I)
                      MAX_INTEN_NUM=MAX_INTEN_NUM+1
                  END IF
              END IF
C       DO NEXT TEMPINTEN ENTRY
          END DO
          INTENEXIST=.TRUE.
          WRITE(OUTLYNE,*) 'FINAL INTENSITY ARRAY HAS BEEN CREATED'
          CALL SHOWIT(1)
          WRITE(OUTLYNE,*) 'WRITING THE INTENSITY FILE, INTEN.DAT...'
          CALL SHOWIT(1)
C       WRITE THE INTEN FILE
          OPEN(UNIT=101,FILE=trim(HOME)//'INTEN.DAT')
          CALL CLOSE_FILE(101,1)
          OPEN(UNIT=101,FILE=trim(HOME)//'INTEN.DAT')
          WRITE(UNIT=101,FMT=141)JJ,INTENNUM_PIXELS,INTENLENGTH_ARRAY
          DO J=1,NY
              DO I=1,NX
                  WRITE(UNIT=101,FMT=14) I,J,INTEN_ARRAY(I,J,1:10)
              END DO
          END DO
          WRITE(OUTLYNE,*) '"INTEN" IS FINISHED. INTEN.DAT NOW EXISTS'
          CALL SHOWIT(1)
          WRITE(OUTLYNE,*) 'CREATING SUMMARY INTENSITY REPORT'
          CALL SHOWIT(1)
          WRITE(OUTLYNE,*)
     1    'NUMBER OF FOBS IN RAY HISTORY REPORT = ',FOBNUMBER
          CALL SHOWIT(0)
          DO I=1,FOBNUMBER
              WRITE(OUTLYNE,*) 'FOR FOB# = ',I
              CALL SHOWIT(1)
              IMAX=-1.0D300
              IMIN=1.0D300
              IPTOV=0.0D0
              IRMS=0.0D0
              ITOTINTEN=0.0D0
              ITOTRAYS=0.0D0
              DO K=1,NX
                  DO L=1,NY
                      IF(INT(INTEN_ARRAY(K,L,10)).EQ.I) THEN
                          IF(INTEN_ARRAY(K,L,2).NE.0.0D0) THEN
C       ONLY CONSIDER RAYS WITH ENERGY IN THEM
                              IF(INTEN_ARRAY(K,L,2).GT.IMAX) IMAX=INTEN_ARRAY(K,L,2)
                              IF(INTEN_ARRAY(K,L,2).LT.IMIN) IMIN=INTEN_ARRAY(K,L,2)
                              ITOTINTEN=ITOTINTEN+INTEN_ARRAY(K,L,2)
                              IRMS=IRMS+(INTEN_ARRAY(K,L,2)**2)
                              ITOTRAYS=ITOTRAYS+INTEN_ARRAY(K,L,1)
                          END IF
                      END IF
                  END DO
              END DO
              WRITE(OUTLYNE,*)
     1        '           FOR FOB# = ',I
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)
     1        '     TOTAL INTENSITY = ',ITOTINTEN
              CALL SHOWIT(0)
              WRITE(OUTLYNE,*)
     1        'TOTAL NUMBER OF RAYS = ',ITOTRAYS
              WRITE(OUTLYNE,*)
     1        '       P-V INTENSITY = ',IPTOV
              CALL SHOWIT(0)
              WRITE(OUTLYNE,*)
     1        '            '
              CALL SHOWIT(1)
          END DO

 14       FORMAT(I10,I10,10(1X,D23.15))
 141      FORMAT(I4,I10,1X,D23.15)
          CALL CLOSE_FILE(101,1)

          DEALLOCATE(TEMPHIST,TEMPINTEN,
     2    SCENE_ARRAY,STAT=ALLOERR)
          RETURN
 13       FORMAT(I3,I10,14(1X,D23.15))
 131      FORMAT(I10,I4,I10)
      END
C SUB SCREENIT.FOR
      SUBROUTINE SCREENIT
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE SCREENIT. THIS SETS ONE SURFACE'S SCREEN PROPERTY TO "ON" OR "OFF"
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
          IF(STI.EQ.1) THEN
              IF(SYSTEM1(103).EQ.1.0D0) THEN
                  WRITE(OUTLYNE,11)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,12) INT(SYSTEM1(104))
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,13) SYSTEM1(105)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,14) SYSTEM1(106)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,15) SYSTEM1(107)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,16) SYSTEM1(108)
                  CALL SHOWIT(0)
              ELSE
                  WRITE(OUTLYNE,10)
                  CALL SHOWIT(0)
              END IF
              RETURN
          END IF
C       CHECK FOR STRING INPUT
          IF(SST.EQ.1) THEN
              OUTLYNE='"SCREEN" TAKES NO STRING'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(WQ.EQ.'ON') THEN
              IF(DF1.EQ.1.OR.DF2.EQ.1.OR.DF3.EQ.1.OR.
     1        DF4.EQ.1.OR.DF5.EQ.1) THEN
                  OUTLYNE=
     1            '"SCREEN ON" REQUIRES EXPLICIT NUMERIC WORD 1,2,3,4 AND 5 INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
          IF(WQ.EQ.'OFF') THEN
              IF(SN.EQ.1) THEN
                  OUTLYNE=
     1            '"SCREEN OFF" TAKES NO NUMERIC INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
C       NUMERIC WORD 1
          IF(INT(W1).LT.NEWOBJ.OR.INT(W1).GT.NEWIMG) THEN
              OUTLYNE='NUMERIC WORD 1 (SURFACE NUMBER) IS BEYOND LEGAL BOUNDS'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(WQ.NE.'ON'.AND.WQ.NE.'OFF') THEN
              OUTLYNE='"SCREEN ONLY TAKES "ON" OR "OFF" AS QUALIFIER INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(WQ.EQ.'OFF') THEN
              SYSTEM1(103)=0.0D0
              SYSTEM1(104)=0.0D0
              SYSTEM1(105)=0.0D0
              SYSTEM1(106)=0.0D0
              SYSTEM1(107)=0.0D0
              SYSTEM1(108)=0.0D0
          END IF
          IF(WQ.EQ.'ON') THEN
              SYSTEM1(103)=1.0D0
              SYSTEM1(104)=W1
              SYSTEM1(105)=W2
              SYSTEM1(106)=W3
              SYSTEM1(107)=W4
              SYSTEM1(108)=W5
          END IF
          RETURN
 10       FORMAT('"SCREEN" IS CURRENTLY TURNED "OFF"')
 11       FORMAT('"SCREEN" IS CURRENTLY TURNED "ON"')
 12       FORMAT('"SCREEN" SURFACE NUMBER    = ',I3)
 13       FORMAT('"SCREEN" HOLE DIAMETER     = ',D23.15)
 14       FORMAT('"SCREEN" THICKNESS         = ',D23.15)
 15       FORMAT('"SCREEN" HOLE SPACING      = ',D23.15)
 16       FORMAT('"SCREEN" EXCLUSION ANGLE   = ',D23.15)
      END
C SUB FLIPREF.FOR
      SUBROUTINE FLIPREF
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE FLIPREF.FOR.
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
C       CHECK FOR STRING INPUT
          IF(SST.EQ.1) THEN
              OUTLYNE='"'//WC(1:8)//'" TAKES NO STRING INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(WQ.EQ.'ON'.OR.WQ.EQ.'OFF') THEN
              IF(SN.EQ.1) THEN
                  OUTLYNE=WC(1:8)//' ("ON" OR "OFF")'//
     1            ' TAKES NO NUMERIC INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
          IF(STI.EQ.0) THEN
              IF(WQ.NE.'ON'.AND.WQ.NE.'OFF') THEN
                  OUTLYNE='"ON", "YES", "NO" AND "OFF" ARE THE ONLY'
                  CALL SHOWIT(1)
                  OUTLYNE='VALID QUALIFIERS USED WITH "'//WC(1:8)//'"'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C     STI=1
          END IF
          IF(STI.EQ.1) THEN
              IF(WC.EQ.'FLIPREFX') THEN
                  IF(SYSTEM1(128).EQ.0.0D0) WRITE(OUTLYNE,10)
                  IF(SYSTEM1(128).EQ.1.0D0) WRITE(OUTLYNE,11)
                  CALL SHOWIT(0)
              END IF
              IF(WC.EQ.'FLIPREFY') THEN
                  IF(SYSTEM1(129).EQ.0.0D0) WRITE(OUTLYNE,12)
                  IF(SYSTEM1(129).EQ.1.0D0) WRITE(OUTLYNE,13)
                  CALL SHOWIT(0)
              END IF
              RETURN
          END IF
          IF(WC.EQ.'FLIPREFX') THEN
              IF(WQ.EQ.'OFF') THEN
                  SYSTEM1(128)=0.0D0
              END IF
              IF(WQ.EQ.'ON') THEN
                  SYSTEM1(128)=1.0D0
              END IF
          END IF
          IF(WC.EQ.'FLIPREFY') THEN
              IF(WQ.EQ.'OFF') THEN
                  SYSTEM1(129)=0.0D0
              END IF
              IF(WQ.EQ.'ON') THEN
                  SYSTEM1(129)=1.0D0
              END IF
          END IF
 10       FORMAT('FLIPREFX IS CURRENTLY TURNED "OFF"')
 11       FORMAT('FLIPREFX IS CURRENTLY TURNED "ON"')
 12       FORMAT('FLIPREFY IS CURRENTLY TURNED "OFF"')
 13       FORMAT('FLIPREFY IS CURRENTLY TURNED "ON"')
          RETURN
      END
