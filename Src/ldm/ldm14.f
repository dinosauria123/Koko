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

C       FOURTEENTH FILE FOR LENS DATABASE MANAGER FILES

C SUB SCOATING.FOR
      SUBROUTINE SCOATING
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE SAIR WHICH IMPLEMENTS THE COATING
C       COMMAND AT THE LENS OF UPDATE LENS LEVEL.
C
          INTEGER PIKCNT,I
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
C
C               CHECK FOR PRESENCE OF QUALIFIER OR STRING INPUT
C               AND FOR NUMERIC INPUT
C               PRINT ERROR AND RETURN IF DISCOVERED.
C
          IF(SQ.EQ.1.OR.SST.EQ.1)THEN
              OUTLYNE='"COATING" TAKES NO QUALIFIER OR STRING INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
C
          ELSE
          END IF
          IF(S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1)THEN
              OUTLYNE='"COATING" TAKES NO NUMERIC WORD #2 TO #5 INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
C
          ELSE
          END IF
          IF(S1.EQ.0)THEN
              OUTLYNE='"COATING" REQUIRES EXPLICIT NUMERIC WORD #1 INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
C
          ELSE
          END IF
          IF(W1.LT.0.0D0.OR.W1.GT.1000.0D0)THEN
              OUTLYNE='"COATING" REQUIRES NUMERIC WORD #1 INPUT'
              CALL SHOWIT(1)
              OUTLYNE='IN THE RANGE 0 TO 1000'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
C
          ELSE
          END IF
C
          ALENS(112,SURF)=W1
C
          IF(PIKUP(1,SURF,44).EQ.0.0) THEN
C
C       NO COATING PIKUPS, JUST RETURN
              RETURN
          ELSE
          END IF
C
C       DELETE THE PIKUP
          PIKUP(1:6,SURF,44)=0.0
          ALENS(32,SURF)=ALENS(32,SURF)-1.0
C
C
C       PRINT MESSAGE
          WRITE(OUTLYNE,*)'SURFACE',SURF,' :PIKUP (COATING) DELETED'
          CALL SHOWIT(1)
C
          PIKCNT=0
          DO 10 I=1,PSIZ
              IF(PIKUP(1,SURF,I).NE.0.0) THEN
                  PIKCNT=PIKCNT+1
              ELSE
              END IF
 10       CONTINUE
C
          IF(PIKCNT.EQ.0.0) ALENS(32,SURF)=0.0
C
          CALL MACFAL
          RETURN
      END
C SUB SREFL.FOR
      SUBROUTINE SREFL
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE SREFL WHICH IMPLEMENTS THE REFL
C       COMMAND AT THE LENS OF UPDATE LENS LEVEL.
C
C
          INTEGER PIKCNT,I
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
C
C       "REFL" IS A SURFACE TERMINAL COMMAND. IN THE LENS INPUT
C       ROUTINE F5=1, "REFL" CAUSES THE SURFACE COUNTING VARIABLE
C       (SURF) AND SYSTEM1(20) TO BE INCREMENTED BY 1 AND 1.0
C       AND CAUSES THE NEXT SURFACE DATA TO BE STORED FOR THE NEXT
C       SURFACE. AT F6=1 (UPDATE ROUTINE) THIS INCREMENTING DOES
C       NO OCCUR. THE LAST SURFACE FOR WHICH DATA MAY BE ENTERED
C       IS SURFACE MAXSUR. SURF AND SYSTEM1(20) ARE NOT INCREMENTED
C       PASSED MAXSUR SO IF AN ATTEMPT IS MADE TO INPUT PASSED
C       SURFACE MAXSUR IN THE LENS INPUT MODE, THE DATA FOR SURFACE
C       MAXSUR WILL BE OVERWRITTEN.
C
C               CHECK FOR STRING,NUMERIC OR QUALIFIER INPUT
C               PRINT ERROR AND RETURN IF DISCOVERED.
C
          IF(SST.EQ.1) THEN
              OUTLYNE='"REFL","REFLTIR" AND "REFLTIRO" TAKE NO EXPLICIT INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
          IF(F5.EQ.1.OR.F6.EQ.1) THEN
              IF(SURF.EQ.0) THEN
                  OUTLYNE='OBJECT SURFACE CAN NOT BE A REFLECTOR'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C
              IF(SURF.EQ.0) THEN
                  OUTLYNE=
     1            'REFLECTIVE SURFACE NOT VALID ON OBJECT SURFACE'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C
C               WE ARE AT LENS INPUT OR LENS UPDATE LEVEL
C
C       DEFAULT INDICES ALWAYS GO TO THE NEGATIVE VALUE OF THE
C       INDICES OF THE PRECEEDING SURFACE.
C

              ALENS(46,SURF)=-ALENS(46,(SURF-1))
              ALENS(47,SURF)=-ALENS(47,(SURF-1))
              ALENS(48,SURF)=-ALENS(48,(SURF-1))
              ALENS(49,SURF)=-ALENS(49,(SURF-1))
              ALENS(50,SURF)=-ALENS(50,(SURF-1))
              ALENS(71,SURF)=-ALENS(71,(SURF-1))
              ALENS(72,SURF)=-ALENS(72,(SURF-1))
              ALENS(73,SURF)=-ALENS(73,(SURF-1))
              ALENS(74,SURF)=-ALENS(74,(SURF-1))
              ALENS(75,SURF)=-ALENS(75,(SURF-1))
              GLANAM(SURF,1)='             '
              IF(WC.EQ.'REFL')     GLANAM(SURF,2)='REFL         '
              IF(WC.EQ.'REFLTIRO') GLANAM(SURF,2)='REFLTIRO     '
              IF(WC.EQ.'REFLTIR')  GLANAM(SURF,2)='REFLTIR      '
              IF(WC.EQ.'REFL')     ALENS(125,SURF)=0.0D0
              IF(WC.EQ.'REFLTIRO') ALENS(125,SURF)=1.0D0
              IF(WC.EQ.'REFLTIR')  ALENS(125,SURF)=2.0D0
C
C       NOW HANDEL EXISTING PIKUPS OF GLASS TYPE, I E DELETE THEM.
C       CHECK FOR CC PIKUPS AND DELETE IF FOUND
C
C
              IF(PIKUP(1,SURF,20).EQ.0.0) THEN
C
C       NO GLASS PIKUPS, JUST RETURN
C
                  IF(F5.EQ.1.AND.SURF.LT.MAXSUR) THEN
                      SURF=SURF+1
                      ALENS(1:LSIZ,SURF)=0.0D0
                      SYSTEM1(20)=DBLE(SURF)
                  ELSE
C       DON'T INCREMENT SURF AND SYSTEM1(20)
                  END IF
                  F22=1
                  RETURN
              ELSE
              END IF
C
C       DELETE THE PIKUP
              PIKUP(1:6,SURF,20)=0.0
              ALENS(32,SURF)=ALENS(32,SURF)-1.0
C
C
C       PRINT MESSAGE
              WRITE(OUTLYNE,*)'SURFACE',SURF,' :PIKUP (GLASS) DELETED'
              CALL SHOWIT(1)
C
C       ARE THERE MORE PIKUPS? IF NOT SET ALENS(32,SURF) TO ZERO.
C
              PIKCNT=0
              DO 10 I=1,PSIZ
                  IF(PIKUP(1,SURF,I).NE.0.0) THEN
                      PIKCNT=PIKCNT+1
                  ELSE
                  END IF
 10           CONTINUE
C
              IF(PIKCNT.EQ.0.0) ALENS(32,SURF)=0.0
C
              IF(F5.EQ.1.AND.SURF.LT.MAXSUR) THEN
                  SURF=SURF+1
                  ALENS(1:LSIZ,SURF)=0.0D0
                  SYSTEM1(20)=DBLE(SURF)
              ELSE
C       DON'T INCREMENT SURF AND SYSTEM1(20)
              END IF
C
          ELSE
C
              F22=1
              OUTLYNE='"REFL" NOT VALID AT THIS PROGRAM LEVEL'
              CALL SHOWIT(1)
          END IF
          CALL MACFAL
          RETURN
      END
C SUB SAIR.FOR
      SUBROUTINE SAIR
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE SAIR WHICH IMPLEMENTS THE AIR
C       COMMAND AT THE LENS OF UPDATE LENS LEVEL.
C
          INTEGER PIKCNT,I
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
C
C       "AIR" IS A SURFACE TERMINAL COMMAND. IN THE LENS INPUT
C       ROUTINE F5=1, "AIR" CAUSES THE SURFACE COUNTING VARIABLE
C       (SURF) AND SYSTEM1(20) TO BE INCREMENTED BY 1 AND 1.0
C       AND CAUSES THE NEXT SURFACE DATA TO BE STORED FOR THE NEXT
C       SURFACE. AT F6=1 (UPDATE ROUTINE) THIS INCREMENTING DOES
C       NO OCCUR. THE LAST SURFACE FOR WHICH DATA MAY BE ENTERED
C       IS SURFACE MAXSUR. SURF AND SYSTEM1(20) ARE NOT INCREMENTED
C       PASSED MAXSUR SO IF AN ATTEMPT IS MADE TO INPUT PASSED
C       SURFACE MAXSUR IN THE LENS INPUT MODE, THE DATA FOR SURFACE
C       MAXSUR WILL BE OVERWRITTEN.
C
C               CHECK FOR PRESENCE OF QUALIFIER OR STRING INPUT
C               AND FOR NUMERIC INPUT
C               PRINT ERROR AND RETURN IF DISCOVERED.
C
          IF(SQ.EQ.1.OR.SST.EQ.1.OR.SN.EQ.1)THEN
              OUTLYNE='"AIR" TAKES NO EXPLICIT INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
C
          ELSE
          END IF
C
          IF(F5.EQ.1.OR.F6.EQ.1) THEN
C
C               WE ARE AT LENS INPUT OR LENS UPDATE LEVEL
C
              ALENS(46:50,SURF)=1.0
              ALENS(71:75,SURF)=1.0
              GLANAM(SURF,1)='             '
              GLANAM(SURF,2)='AIR          '
C
C       NOW HANDEL EXISTING PIKUPS OF GLASS TYPE, I E DELETE THEM.
C       CHECK FOR CC PIKUPS AND DELETE IF FOUND
C
C
              IF(PIKUP(1,SURF,20).EQ.0.0) THEN
C
C       NO GLASS PIKUPS, JUST RETURN
C
                  F22=1
                  IF(F5.EQ.1.AND.SURF.LT.MAXSUR) THEN
                      SURF=SURF+1
                      ALENS(1:LSIZ,SURF)=0.0D0
                      SYSTEM1(20)=DBLE(SURF)
                  ELSE
C       DON'T INCREMENT SURF AND SYSTEM1(20)
                  END IF
                  RETURN
              ELSE
              END IF
C
C       DELETE THE PIKUP
              PIKUP(1:6,SURF,20)=0.0
              ALENS(32,SURF)=ALENS(32,SURF)-1.0
C
C
C       PRINT MESSAGE
              WRITE(OUTLYNE,*)'SURFACE',SURF,' :PIKUP (GLASS) DELETED'
              CALL SHOWIT(1)
C
              PIKCNT=0
              DO 10 I=1,PSIZ
                  IF(PIKUP(1,SURF,I).NE.0.0) THEN
                      PIKCNT=PIKCNT+1
                  ELSE
                  END IF
 10           CONTINUE
C
              IF(PIKCNT.EQ.0.0) ALENS(32,SURF)=0.0
              IF(F5.EQ.1.AND.SURF.LT.MAXSUR) THEN
                  SURF=SURF+1
                  ALENS(1:LSIZ,SURF)=0.0D0
                  SYSTEM1(20)=DBLE(SURF)
              ELSE
C       DON'T INCREMENT SURF AND SYSTEM1(20)
              END IF
C
              F22=1
          ELSE
              OUTLYNE='"AIR" NOT VALID AT THIS PROGRAM LEVEL'
              CALL SHOWIT(1)
          END IF
          CALL MACFAL
          RETURN
      END
C SUB GLSCAT.FOR
      SUBROUTINE GLSCAT
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE GLSCAT WHICH IMPLEMENTS THE SCHOTT
C       SCH2000,GLASS,OHARA,HOYA,CHANCE,CORNIN,PLASTIC,RADHARD,GLCAT
C       HIKARI AND USER COMMAND AT THE LENS OF UPDATE LENS LEVEL.
C
          INTEGER PIKCNT,I
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
C
C       THIS IS A SURFACE TERMINAL COMMAND. IN THE LENS INPUT
C       ROUTINE F5=1, AND CAUSES THE SURFACE COUNTING VARIABLE
C       (SURF) AND SYSTEM1(20) TO BE INCREMENTED BY 1 AND 1.0
C       AND CAUSES THE NEXT SURFACE DATA TO BE STORED FOR THE NEXT
C       SURFACE. AT F6=1 (UPDATE ROUTINE) THIS INCREMENTING DOES
C       NO OCCUR. THE LAST SURFACE FOR WHICH DATA MAY BE ENTERED
C       IS SURFACE MAXSUR. SURF AND SYSTEM1(20) ARE NOT INCREMENTED
C       PASSED MAXSUR SO IF AN ATTEMPT IS MADE TO INPUT PASSED
C       SURFACE MAXSUR IN THE LENS INPUT MODE, THE DATA FOR SURFACE
C       MAXSUR WILL BE OVERWRITTEN.
C
C               CHECK FOR STRING INPUT
C               PRINT ERROR AND RETURN IF DISCOVERED.
C
          IF(SN.EQ.1) THEN
              OUTLYNE='"'//WC//'" zAKES NO NUMERIC INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL MACFAL
              RETURN
C
          ELSE
          END IF
C
          ALENS(46:50,SURF)=1.0D0
          ALENS(71:75,SURF)=1.0D0
          GLANAM(SURF,1)=WC(1:8)//'   '
          GLANAM(SURF,2)= WS(1:13)
          F22=1
C
C       NOW HANDEL EXISTING PIKUPS OF GLASS TYPE, I E DELETE THEM.
C       CHECK FOR CC PIKUPS AND DELETE IF FOUND
C
C
          IF(PIKUP(1,SURF,20).EQ.0.0) THEN
C
C       NO GLASS PIKUPS, JUST RETURN
              IF(F5.EQ.1.AND.SURF.LT.MAXSUR) THEN
                  SURF=SURF+1
                  ALENS(1:LSIZ,SURF)=0.0D0
                  SYSTEM1(20)=DBLE(SURF)
              ELSE
C       DON'T INCREMENT SURF AND SYSTEM1(20)
              END IF
C
              RETURN
          ELSE
          END IF
C
C       DELETE THE PIKUP
          PIKUP(1:6,SURF,20)=0.0
          ALENS(32,SURF)=ALENS(32,SURF)-1.0
C
C
C       PRINT MESSAGE
          WRITE(OUTLYNE,*)'SURFACE',SURF,' :PIKUP (GLASS) DELETED'
          CALL SHOWIT(1)
C
          PIKCNT=0
          DO 10 I=1,PSIZ
              IF(PIKUP(1,SURF,I).NE.0.0) THEN
                  PIKCNT=PIKCNT+1
              ELSE
              END IF
 10       CONTINUE
C
          IF(PIKCNT.EQ.0.0) ALENS(32,SURF)=0.0
          IF(F5.EQ.1.AND.SURF.LT.MAXSUR) THEN
              SURF=SURF+1
              ALENS(1:LSIZ,SURF)=0.0D0
              SYSTEM1(20)=DBLE(SURF)
          ELSE
C       DON'T INCREMENT SURF AND SYSTEM1(20)
          END IF
C
      END
C SUB SINDEX.FOR
      SUBROUTINE SINDEX
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE SINDEX. THIS IS THE SUBROUTINE WHICH
C       HANDLES THE INDEX,DISPERSION AND V-NUMBER CALCULATIONS
C       FOR THE RTG AND CTG COMMANDS
C
          REAL*8 INDEX,DISP,VNUM,DISA,DISB,PARTL
C
          INTEGER NF
C
          COMMON/NSIN/NF,INDEX,DISP,VNUM,PARTL
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
C
C       THE CONTROL WAVLENGTH NUMBER IS STORED IN SYSTEM1(11)
C
          IF(SYSTEM1(11).EQ.1.0) INDEX=ALENS(46,NF)
          IF(SYSTEM1(11).EQ.2.0) INDEX=ALENS(47,NF)
          IF(SYSTEM1(11).EQ.3.0) INDEX=ALENS(48,NF)
          IF(SYSTEM1(11).EQ.4.0) INDEX=ALENS(49,NF)
          IF(SYSTEM1(11).EQ.5.0) INDEX=ALENS(50,NF)
          IF(SYSTEM1(11).EQ.6.0) INDEX=ALENS(71,NF)
          IF(SYSTEM1(11).EQ.7.0) INDEX=ALENS(72,NF)
          IF(SYSTEM1(11).EQ.8.0) INDEX=ALENS(73,NF)
          IF(SYSTEM1(11).EQ.9.0) INDEX=ALENS(74,NF)
          IF(SYSTEM1(11).EQ.10.0) INDEX=ALENS(75,NF)
C
C       THE PRIMARY WAVELENGTH PAIR NUMBERS ARE STORED IN
C       SYSTEM1(7) AND SYSTEM1(8)
C
          IF(SYSTEM1(7).EQ.1.0) DISA=ALENS(46,NF)
          IF(SYSTEM1(7).EQ.2.0) DISA=ALENS(47,NF)
          IF(SYSTEM1(7).EQ.3.0) DISA=ALENS(48,NF)
          IF(SYSTEM1(7).EQ.4.0) DISA=ALENS(49,NF)
          IF(SYSTEM1(7).EQ.5.0) DISA=ALENS(50,NF)
          IF(SYSTEM1(7).EQ.6.0) DISA=ALENS(71,NF)
          IF(SYSTEM1(7).EQ.7.0) DISA=ALENS(72,NF)
          IF(SYSTEM1(7).EQ.8.0) DISA=ALENS(73,NF)
          IF(SYSTEM1(7).EQ.9.0) DISA=ALENS(74,NF)
          IF(SYSTEM1(7).EQ.10.0) DISA=ALENS(75,NF)
          IF(SYSTEM1(8).EQ.1.0) DISB=ALENS(46,NF)
          IF(SYSTEM1(8).EQ.2.0) DISB=ALENS(47,NF)
          IF(SYSTEM1(8).EQ.3.0) DISB=ALENS(48,NF)
          IF(SYSTEM1(8).EQ.4.0) DISB=ALENS(49,NF)
          IF(SYSTEM1(8).EQ.5.0) DISB=ALENS(50,NF)
          IF(SYSTEM1(8).EQ.6.0) DISB=ALENS(71,NF)
          IF(SYSTEM1(8).EQ.7.0) DISB=ALENS(72,NF)
          IF(SYSTEM1(8).EQ.8.0) DISB=ALENS(73,NF)
          IF(SYSTEM1(8).EQ.9.0) DISB=ALENS(74,NF)
          IF(SYSTEM1(8).EQ.10.0) DISB=ALENS(74,NF)
C
          DISP=DABS(DISA)-DABS(DISB)
C       CALC V-NUMBER
C
          IF(DISP.EQ.0.0D0) THEN
              VNUM=0.0D0
              PARTL=0.0D0
          ELSE
              VNUM=(DABS(INDEX)-1.0)/(DISP)
              PARTL=(DABS(INDEX)-DISB)/(DISP)
!                write(6,*) INDEX,DISA,DISB

          END IF
          RETURN
      END
C SUB SPERFECT.FOR
      SUBROUTINE SPERFECT
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE SPERFECT WHICH IMPLEMENTS THE PERFECT
C       COMMAND AT THE LENS OF UPDATE LENS LEVEL.
C
          INTEGER PIKCNT,I
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
C
          IF(SQ.EQ.1.OR.SST.EQ.1.OR.SN.EQ.1)THEN
              OUTLYNE='"PERFECT" TAKES NO EXPLICIT INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
C
          ELSE
          END IF
C
          ALENS(46:50,SURF)=1.0
          ALENS(71:75,SURF)=1.0
          GLANAM(SURF,1)='             '
          GLANAM(SURF,2)='PERFECT      '
C
C       NOW HANDEL EXISTING PIKUPS OF GLASS TYPE, I E DELETE THEM.
C       CHECK FOR CC PIKUPS AND DELETE IF FOUND
C
C
          IF(PIKUP(1,SURF,20).EQ.0.0) THEN
C
C       NO GLASS PIKUPS, JUST RETURN
C
              F22=1
              IF(F5.EQ.1.AND.SURF.LT.MAXSUR) THEN
                  SURF=SURF+1
                  ALENS(1:LSIZ,SURF)=0.0D0
                  SYSTEM1(20)=DBLE(SURF)
              ELSE
C       DON'T INCREMENT SURF AND SYSTEM1(20)
              END IF
              RETURN
          ELSE
          END IF
C
C       DELETE THE PIKUP
          PIKUP(1:6,SURF,20)=0.0
          ALENS(32,SURF)=ALENS(32,SURF)-1.0
C
C
C       PRINT MESSAGE
          WRITE(OUTLYNE,*)'SURFACE',SURF,' :PIKUP (GLASS) DELETED'
          CALL SHOWIT(1)
C
          PIKCNT=0
          DO 10 I=1,PSIZ
              IF(PIKUP(1,SURF,I).NE.0.0) THEN
                  PIKCNT=PIKCNT+1
              ELSE
              END IF
 10       CONTINUE
C
          IF(PIKCNT.EQ.0.0) ALENS(32,SURF)=0.0
          IF(F5.EQ.1.AND.SURF.LT.MAXSUR) THEN
              SURF=SURF+1
              ALENS(1:LSIZ,SURF)=0.0D0
              SYSTEM1(20)=DBLE(SURF)
          ELSE
C       DON'T INCREMENT SURF AND SYSTEM1(20)
          END IF
C
          F22=1
          CALL MACFAL
          RETURN
      END
C SUB SIDEAL.FOR
      SUBROUTINE SIDEAL
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE SIDEAL WHICH IMPLEMENTS THE IDEAL
C       COMMAND AT THE LENS OF UPDATE LENS LEVEL.
C
          INTEGER PIKCNT,I
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
C
          IF(SQ.EQ.1.OR.SST.EQ.1)THEN
              OUTLYNE='"IDEAL" TAKES NO QUALIFIER OR STRING INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          ELSE
          END IF
          IF(S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1)THEN
              OUTLYNE='"IDEAL" ONLY TAKES NUMERIC WORD #1 INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          ELSE
          END IF
          IF(S1.EQ.0)THEN
              OUTLYNE='"IDEAL" REQUIRES EXPLICIT WORD #1 INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          ELSE
          END IF
          IF(W1.EQ.0.0D0)THEN
              OUTLYNE='"IDEAL" REQUIRES NON-ZERO NUMERIC WORD #1 INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          ELSE
          END IF
C
          ALENS(121,SURF)=W1
          ALENS(46:50,SURF)=1.0
          ALENS(71:75,SURF)=1.0
          GLANAM(SURF,1)='             '
          GLANAM(SURF,2)='IDEAL        '
C
C       NOW HANDEL EXISTING PIKUPS OF GLASS TYPE, I E DELETE THEM.
C       CHECK FOR CC PIKUPS AND DELETE IF FOUND
C
C
          IF(PIKUP(1,SURF,20).EQ.0.0) THEN
C
C       NO GLASS PIKUPS, JUST RETURN
C
              F22=1
              IF(F5.EQ.1.AND.SURF.LT.MAXSUR) THEN
                  SURF=SURF+1
                  ALENS(1:LSIZ,SURF)=0.0D0
                  SYSTEM1(20)=DBLE(SURF)
              ELSE
C       DON'T INCREMENT SURF AND SYSTEM1(20)
              END IF
              RETURN
          ELSE
          END IF
C
C       DELETE THE PIKUP
          PIKUP(1:6,SURF,20)=0.0
          ALENS(32,SURF)=ALENS(32,SURF)-1.0
C
C
C       PRINT MESSAGE
          WRITE(OUTLYNE,*)'SURFACE',SURF,' :PIKUP (GLASS) DELETED'
          CALL SHOWIT(1)
C
          PIKCNT=0
          DO 10 I=1,PSIZ
              IF(PIKUP(1,SURF,I).NE.0.0) THEN
                  PIKCNT=PIKCNT+1
              ELSE
              END IF
 10       CONTINUE
C
          IF(PIKCNT.EQ.0.0) ALENS(32,SURF)=0.0
          IF(F5.EQ.1.AND.SURF.LT.MAXSUR) THEN
              SURF=SURF+1
              ALENS(1:LSIZ,SURF)=0.0D0
              SYSTEM1(20)=DBLE(SURF)
          ELSE
C       DON'T INCREMENT SURF AND SYSTEM1(20)
          END IF
C
          F22=1
          CALL MACFAL
          RETURN
      END
C SUB LQUERY.FOR
      SUBROUTINE LQUERY
C
          IMPLICIT NONE
C
C       THIS SUBROUTINE DISPLAYS THE CURRENT VALUE OF A LENS
C       SYSTEM PARAMETER FROM WITHIN THE LENS INPUT
C       LEVEL IN RESPONSE TO A COMMAND NAME FOLLOWED BY A ?
C
          REAL*8 VALUE1,VALUE2,VALUE3,VALUE4,VALUE5,VALUE6
C
          INTEGER V1,V2,V3,V4,V5,V6,VA1,VA1WS1,VA1WS2,VA1WS3,VA1WS4
     1    ,MAT,REPEAT,I
C
          CHARACTER VAL*80,VALWS1*80,VALWS2*80,VALWS3*80
     1    ,VALWS4*80
C
          INCLUDE 'datcfg.inc'
          INCLUDE 'datmac.inc'
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
C
          VALUE1=0.0D0
          VALUE2=0.0D0
          VALUE3=0.0D0
          VALUE4=0.0D0
          VALUE5=0.0D0
          VALUE6=0.0D0
          VAL=AA//AA//AA//AA
          VALWS1=AA//AA//AA//AA
          VALWS2=AA//AA//AA//AA
          VALWS3=AA//AA//AA//AA
          VALWS4=AA//AA//AA//AA
          V1=0
          V2=0
          V3=0
          V4=0
          V5=0
          V6=0
          VA1=0
          VA1WS1=0
          VA1WS2=0
          VA1WS3=0
          VA1WS4=0
          REPEAT=0
C
          MAT=0
          IF(WC.EQ.'AIR') MAT=1
          IF(WC.EQ.'REFL') MAT=1
          IF(WC.EQ.'REFLTIRO') MAT=1
          IF(WC.EQ.'REFLTIR') MAT=1
          IF(WC.EQ.'PERFECT') MAT=1
          IF(WC.EQ.'IDEAL') MAT=1
          IF(WC.EQ.'GLA') MAT=1
          IF(WC.EQ.'GLASS') MAT=1
          IF(WC.EQ.'MODEL') MAT=1
          IF(WC.EQ.'OHARA') MAT=1
          IF(WC.EQ.'HOYA') MAT=1
          IF(WC.EQ.'HIKARI') MAT=1
          IF(WC.EQ.'SCHOTT') MAT=1
          IF(WC.EQ.'SCH2000') MAT=1
          IF(WC.EQ.'CHANCE') MAT=1
          IF(WC.EQ.'CORNIN') MAT=1
          IF(WC.EQ.'USER') MAT=1
          IF(WC.EQ.'RADHARD') MAT=1
          IF(WC.EQ.'GLCAT') MAT=1
          IF(WC.EQ.'MATL') MAT=1
          IF(WC.EQ.'RUSSIAN') MAT=1
          IF(WC.NE.'IDEAL') THEN
              IF(MAT.EQ.1) THEN
                  VA1WS1=1
                  VALWS1=GLANAM(SURF,1)//GLANAM(SURF,2)
                  VAL='CURRENT SURFACE MATERIAL TYPE IS:'
                  VA1=1
                  IF(WC.EQ.'MODEL') THEN
                      V1=1
                      VALUE1=ALENS(86,SURF)
                      V2=1
                      VALUE2=ALENS(87,SURF)
                      V3=1
                      VALUE3=ALENS(89,SURF)
                  END IF
                  GO TO 200
              ELSE
              END IF
              IF(WC.EQ.'N1'.OR.WC.EQ.'N2'.OR.WC.EQ.'N3'.OR.WC.EQ.'N4'
     1        .OR.WC.EQ.'N5'.OR.WC.EQ.'N6'.OR.WC.EQ.'N7'.OR.WC.EQ.'N8'
     2        .OR.WC.EQ.'N9'.OR.WC.EQ.'N10') THEN
                  IF((SURF-1).LT.0) THEN
                      OUTLYNE=
     1                'INDEX CHANGE COMMANDS WORK ON THE PREVIOUS SURFACE IN LENS'
                      CALL SHOWIT(1)
                      OUTLYNE=
     1                'INPUT MODE BUT THERE ARE NO SURFACES AS YET'
                      OUTLYNE='NO VALUE TO DISPLAY'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  IF(WC.EQ.'N1') THEN
                      VA1=1
                      VAL='THE REFRACTIVE INDEX FOR WAVELENGTH #1 IS:'
                      V1=1
                      VALUE1=ALENS(46,SURF-1)
                      GO TO 200
                  ELSE
                  END IF
                  IF(WC.EQ.'N2') THEN
                      VA1=1
                      VAL='THE REFRACTIVE INDEX FOR WAVELENGTH #2 IS:'
                      V1=1
                      VALUE1=ALENS(47,SURF-1)
                      GO TO 200
                  ELSE
                  END IF
                  IF(WC.EQ.'N3') THEN
                      VA1=1
                      VAL='THE REFRACTIVE INDEX FOR WAVELENGTH #3 IS:'
                      V1=1
                      VALUE1=ALENS(48,SURF-1)
                      GO TO 200
                  ELSE
                  END IF
                  IF(WC.EQ.'N4') THEN
                      VA1=1
                      VAL='THE REFRACTIVE INDEX FOR WAVELENGTH #4 IS:'
                      V1=1
                      VALUE1=ALENS(49,SURF-1)
                      GO TO 200
                  ELSE
                  END IF
                  IF(WC.EQ.'N5') THEN
                      VA1=1
                      VAL='THE REFRACTIVE INDEX FOR WAVELENGTH #5 IS:'
                      V1=1
                      VALUE1=ALENS(50,SURF-1)
                      GO TO 200
                  ELSE
                  END IF
                  IF(WC.EQ.'N6') THEN
                      VA1=1
                      VAL='THE REFRACTIVE INDEX FOR WAVELENGTH #6 IS:'
                      V1=1
                      VALUE1=ALENS(71,SURF-1)
                      GO TO 200
                  ELSE
                  END IF
                  IF(WC.EQ.'N7') THEN
                      VA1=1
                      VAL='THE REFRACTIVE INDEX FOR WAVELENGTH #7 IS:'
                      V1=1
                      VALUE1=ALENS(72,SURF-1)
                      GO TO 200
                  ELSE
                  END IF
                  IF(WC.EQ.'N8') THEN
                      VA1=1
                      VAL='THE REFRACTIVE INDEX FOR WAVELENGTH #8 IS:'
                      V1=1
                      VALUE1=ALENS(73,SURF-1)
                      GO TO 200
                  ELSE
                  END IF
                  IF(WC.EQ.'N9') THEN
                      VA1=1
                      VAL='THE REFRACTIVE INDEX FOR WAVELENGTH #9 IS:'
                      V1=1
                      VALUE1=ALENS(74,SURF-1)
                      GO TO 200
                  ELSE
                  END IF
                  IF(WC.EQ.'N10') THEN
                      VA1=1
                      VAL='THE REFRACTIVE INDEX FOR WAVELENGTH #10 IS:'
                      V1=1
                      VALUE1=ALENS(75,SURF-1)
                      GO TO 200
                  ELSE
                  END IF
              ELSE
              END IF
          ELSE
C     WC IS 'IDEAL'
              IF(MAT.EQ.1) THEN
                  VA1WS1=1
                  V1=1
                  VALWS1=GLANAM(SURF,1)//GLANAM(SURF,2)
                  VAL='CURRENT SURFACE MATERIAL TYPE IS:'
                  VALUE1=ALENS(121,SURF)
                  VA1=1
              ELSE
              END IF
          END IF
          IF(WC.EQ.'UNITS') THEN
              IF(SYSTEM1(6).EQ.1.0D0) VALWS1='INCHES'
              IF(SYSTEM1(6).EQ.2.0D0) VALWS1='CM    '
              IF(SYSTEM1(6).EQ.3.0D0) VALWS1='MM    '
              IF(SYSTEM1(6).EQ.4.0D0) VALWS1='METERS'
              VA1WS1=1
              VAL='CURRENT SYSTEM UNITS ARE:'
              VA1=1
              GO TO 200
          ELSE
          END IF
          IF(WC.EQ.'REAL'.OR.WC.EQ.'PARAX') THEN
              IF(ALENS(124,SURF).EQ.0.0D0) VALWS1='REAL'
              IF(ALENS(124,SURF).EQ.1.0D0) VALWS1='PARAX'
              VA1WS1=1
              VAL='SURFACE TYPE IS:'
              VA1=1
              GO TO 200
          ELSE
          END IF
          IF(WC.EQ.'INDEX'.OR.WC.EQ.'VNUM'.OR.WC.EQ.'DPART') THEN
              IF(GLANAM(SURF-1,1).NE.'MODEL') THEN
                  OUTLYNE='NO "INDEX", "VNUM" OR "DPART" VALUES EXIST'
                  CALL SHOWIT(1)
                  OUTLYNE='FOR NON-"MODEL" GLASSES'
                  CALL SHOWIT(1)
              ELSE
                  IF((SURF-1).LT.0) THEN
                      OUTLYNE=
     1                '"INDEX", "VNUM" AND "DPART"'
                      CALL SHOWIT(1)
                      OUTLYNE=
     1                'COMMANDS WORK ON THE PREVIOUS SURFACE IN LENS'
                      CALL SHOWIT(1)
                      OUTLYNE=
     1                'INPUT MODE BUT THERE ARE NO SURFACES AS YET'
                      OUTLYNE='NO VALUE TO DISPLAY'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  IF(WC.EQ.'INDEX') THEN
                      VA1=1
                      VAL='THE "INDEX" VALUE IS:'
                      V1=1
                      VALUE1=ALENS(86,SURF-1)
                      GO TO 200
                  END IF
                  IF(WC.EQ.'VNUM') THEN
                      VA1=1
                      VAL='THE "VNUM" VALUE IS:'
                      V1=1
                      VALUE1=ALENS(87,SURF-1)
                      GO TO 200
                  END IF
                  IF(WC.EQ.'DPART') THEN
                      VA1=1
                      VAL='THE "DPART" VALUE IS:'
                      V1=1
                      VALUE1=ALENS(89,SURF-1)
                      GO TO 200
                  END IF
              END IF
          END IF
          IF(WC.EQ.'UNITS') THEN
              IF(SYSTEM1(6).EQ.1.0D0) VALWS1='INCHES'
              IF(SYSTEM1(6).EQ.2.0D0) VALWS1='CM    '
              IF(SYSTEM1(6).EQ.3.0D0) VALWS1='MM    '
              IF(SYSTEM1(6).EQ.4.0D0) VALWS1='METERS'
              VA1WS1=1
              VAL='CURRENT SYSTEM UNITS ARE:'
              VA1=1
              GO TO 200
          ELSE
          END IF
          IF(WC.EQ.'LBL'.OR.WC.EQ.'LABEL') THEN
              VAL=LBL(SURF)
              IF(ALENS(44,SURF).EQ.0.0D0)
     1        VAL='"THERE IS NO LABEL FOR THE CURRENT SURFACE"'
              VA1=1
              GO TO 200
          ELSE
          END IF
          IF(WC.EQ.'LI') THEN
              VA1=1
              VAL='THE CURRENT LENS IDENTIFIER IS:'
              VA1WS1=1
              VALWS1=LI
              IF(LI.EQ.AA) VALWS1='"LI IS ALL BLANK"'
              VA1WS1=1
              GO TO 200
          ELSE
          END IF
          IF(WC.EQ.'MFG') THEN
              VA1=1
              VAL='THE CURRENT MFG IS:'
              VA1WS1=1
              VALWS1=MFG
              IF(MFG.EQ.AA) VALWS1='"MFG IS ALL BLANK"'
              VA1WS1=1
              GO TO 200
          ELSE
          END IF
          IF(WC.EQ.'CATNUM') THEN
              VA1=1
              VAL='THE CURRENT CATNUM IS:'
              VA1WS1=1
              VALWS1=CATNUM
              IF(CATNUM.EQ.AA) VALWS1='"CATNUM IS ALL BLANK"'
              VA1WS1=1
              GO TO 200
          ELSE
          END IF
          IF(WC.EQ.'INI') THEN
              VA1=1
              VAL='THE CURRENT DESIGNER IDENTIFIER IS:'
              VA1WS1=1
              VALWS1=INNI
              IF(INNI.EQ.AA) VALWS1='"INI IS ALL BLANK"'
              VA1WS1=1
              GO TO 200
          ELSE
          END IF
          IF(WC.EQ.'LTYPE') THEN
              VA1=1
              VAL='THE CURRENT LENS TYPE IDENTIFIER IS:'
              VA1WS1=1
              VALWS1=LLTYPE(1:5)
              IF(INNI.EQ.AA) VALWS1='"LTYPE IS ALL BLANK"'
              VA1WS1=1
              GO TO 200
          ELSE
          END IF
          IF(WC.EQ.'LIC') THEN
              VAL='THE CURRENT LENS IDENTIFIER CONTINUATION IS:'
              VA1=1
              VALWS1=LIC(1)
              IF(LIC(1).EQ.AA) VALWS1='"LIC (LINE 1) IS ALL BLANK"'
              VA1WS1=1
              VALWS2=LIC(2)
              IF(LIC(2).EQ.AA) VALWS2='"LIC (LINE 2) IS ALL BLANK"'
              VA1WS2=1
              VALWS3=LIC(3)
              IF(LIC(3).EQ.AA) VALWS3='"LIC (LINE 3) IS ALL BLANK"'
              VA1WS3=1
              VALWS4=LIC(4)
              IF(LIC(4).EQ.AA) VALWS4='"LIC (LINE 4) IS ALL BLANK"'
              VA1WS4=1
              GO TO 200
          ELSE
          END IF
          IF(WC.EQ.'REFS') THEN
              VA1=1
              VAL=
     1        'CURRENT REFERENCE SURFACE NUMBER AND ORIENTATION ANGLE(DEG) ARE:'
              V1=1
              VALUE1=SYSTEM1(25)
              V2=1
              VALUE2=SYSTEM1(59)
              GO TO 200
          ELSE
          END IF
          IF(WC.EQ.'SAY'.OR.WC.EQ.'SAX') THEN
              IF(SYSTEM1(64).NE.0.0D0) THEN
                  WRITE(OUTLYNE,*)'"'//WC(1:3)//'" HAS NOT BEEN EXPLICITLY SET'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'BUT IS HELD WITH AN "NAO(X OR Y)" ASSIGNMENT'
                  CALL SHOWIT(1)
              ELSE
              END IF
              IF(SYSTEM1(67).NE.0.0D0) THEN
                  WRITE(OUTLYNE,*)'"'//WC(1:3)//'" HAS NOT BEEN EXPLICITLY SET'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'BUT IS HELD WITH AN "FNO(X OR Y)" ASSIGNMENT'
                  CALL SHOWIT(1)
              ELSE
              END IF
              IF(SYSTEM1(83).NE.0.0D0) THEN
                  WRITE(OUTLYNE,*)'"SAY" IS CURRENTLY FLOATING'
                  CALL SHOWIT(1)
              ELSE
              END IF
              IF(SYSTEM1(84).NE.0.0D0) THEN
                  WRITE(OUTLYNE,*)'"SAX" IS CURRENTLY FLOATING'
                  CALL SHOWIT(1)
              ELSE
              END IF
              VA1=1
              IF(WC.EQ.'SAY') VAL='CURRENT "SAY" VALUE IS:'
              IF(WC.EQ.'SAX') VAL='CURRENT "SAX" VALUE IS:'
              V1=1
              IF(WC.EQ.'SAY') VALUE1=SYSTEM1(12)
              IF(WC.EQ.'SAX') VALUE1=SYSTEM1(13)
              GO TO 200
          ELSE
          END IF
C
          IF(WC.EQ.'WRY'.OR.WC.EQ.'WRX'.OR.WC.EQ.'BDX'.OR.
     1    WC.EQ.'BDY') THEN
              VA1=1
              IF(WC.EQ.'WRY') VAL='CURRENT "WRY" VALUE IS:'
              IF(WC.EQ.'WRX') VAL='CURRENT "WRX" VALUE IS:'
              IF(WC.EQ.'BDY') VAL='CURRENT "BDY" VALUE IS:'
              IF(WC.EQ.'BDX') VAL='CURRENT "BDX" VALUE IS:'
              V1=1
              IF(WC.EQ.'WRX') VALUE1=SYSTEM1(85)
              IF(WC.EQ.'WRY') VALUE1=SYSTEM1(86)
              IF(WC.EQ.'BDX') VALUE1=SYSTEM1(87)
              IF(WC.EQ.'BDY') VALUE1=SYSTEM1(88)
              GO TO 200
          ELSE
          END IF
          IF(WC.EQ.'NAOY'.OR.WC.EQ.'NAOX') THEN
              IF(SYSTEM1(64).NE.1.0D0) THEN
                  OUTLYNE='"'//WC(1:4)//'" HAS NOT BEEN EXPLICITLY SET'
                  CALL SHOWIT(1)
                  IF(INT(SYSTEM1(11)).GE.1.AND.INT(SYSTEM1(11)).LE.5) THEN
                      SYSTEM1(65)=(ALENS(45+INT(SYSTEM1(11)),0)*SYSTEM1(12))/
     1                DSQRT((ALENS(3,0)**2)+(SYSTEM1(12)**2))
                      SYSTEM1(66)=(ALENS(45+INT(SYSTEM1(11)),0)*SYSTEM1(13))/
     1                DSQRT((ALENS(3,0)**2)+(SYSTEM1(13)**2))
                      SYSTEM1(83)=0.0D0
                      SYSTEM1(84)=0.0D0
                  END IF
                  IF(INT(SYSTEM1(11)).GE.6.AND.INT(SYSTEM1(11)).LE.10) THEN
                      SYSTEM1(65)=(ALENS(70-5+INT(SYSTEM1(11)),0)*SYSTEM1(12))/
     1                DSQRT((ALENS(3,0)**2)+(SYSTEM1(12)**2))
                      SYSTEM1(66)=(ALENS(70-5+INT(SYSTEM1(11)),0)*SYSTEM1(13))/
     1                DSQRT((ALENS(3,0)**2)+(SYSTEM1(13)**2))
                      SYSTEM1(83)=0.0D0
                      SYSTEM1(84)=0.0D0
                  END IF
              ELSE
              END IF
              VA1=1
              IF(WC.EQ.'NAOY') VAL='CURRENT "NAOY" VALUE IS:'
              IF(WC.EQ.'NAOX') VAL='CURRENT "NAOX" VALUE IS:'
              V1=1
              IF(WC.EQ.'NAOY') VALUE1=SYSTEM1(65)
              IF(WC.EQ.'NAOX') VALUE1=SYSTEM1(66)
              GO TO 200
          ELSE
          END IF
          IF(WC.EQ.'FNOY'.OR.WC.EQ.'FNOX') THEN
              IF(SYSTEM1(67).NE.1.0D0) THEN
                  OUTLYNE='"'//WC(1:4)//'" HAS NOT BEEN EXPLICITLY SET'
                  CALL SHOWIT(1)
                  SYSTEM1(68)=1.0D0/((2.0D0*SYSTEM1(12))/ALENS(3,0))
                  SYSTEM1(69)=1.0D0/((2.0D0*SYSTEM1(13))/ALENS(3,0))
                  SYSTEM1(83)=0.0D0
                  SYSTEM1(84)=0.0D0
              ELSE
              END IF
              VA1=1
              IF(WC.EQ.'FNOY') VAL='CURRENT "FNOY" VALUE IS:'
              IF(WC.EQ.'FNOX') VAL='CURRENT "FNOX" VALUE IS:'
              V1=1
              IF(WC.EQ.'FNOY') VALUE1=SYSTEM1(68)
              IF(WC.EQ.'FNOX') VALUE1=SYSTEM1(69)
              GO TO 200
          ELSE
          END IF
          IF(WC.EQ.'SCY'.OR.WC.EQ.'SCX') THEN
              VA1=1
              IF(WC.EQ.'SCY'.AND.SYSTEM1(18).EQ.0.0D0)
     1        VAL='CURRENT "SCY" VALUES ARE:'
              IF(WC.EQ.'SCY'.AND.SYSTEM1(18).EQ.1.0D0)
     1        VAL='CURRENT "SCY FANG" VALUES ARE:'
              IF(WC.EQ.'SCX'.AND.SYSTEM1(19).EQ.0.0D0)
     1        VAL='CURRENT "SCX" VALUES ARE:'
              IF(WC.EQ.'SCX'.AND.SYSTEM1(19).EQ.1.0D0)
     1        VAL='CURRENT "SCX FANG" VALUES ARE:'
              V1=1
              V2=1
              IF(WC.EQ.'SCY'.AND.SYSTEM1(18).EQ.0.0D0) THEN
                  VALUE1=SYSTEM1(14)
                  VALUE2=SYSTEM1(15)
              ELSE
              END IF
              IF(WC.EQ.'SCY'.AND.SYSTEM1(18).EQ.1.0D0) THEN
                  VALUE1=SYSTEM1(21)
                  VALUE2=SYSTEM1(22)
              ELSE
              END IF
              IF(WC.EQ.'SCX'.AND.SYSTEM1(19).EQ.0.0D0) THEN
                  VALUE1=SYSTEM1(16)
                  VALUE2=SYSTEM1(17)
              ELSE
              END IF
              IF(WC.EQ.'SCX'.AND.SYSTEM1(19).EQ.1.0D0) THEN
                  VALUE1=SYSTEM1(23)
                  VALUE2=SYSTEM1(24)
              ELSE
              END IF
              GO TO 200
          ELSE
          END IF
          IF(WC.EQ.'PXIM'.OR.WC.EQ.'PYIM') THEN
              VA1=1
              IF(WC.EQ.'PXIM'.AND.SYSTEM1(94).EQ.0.0D0)
     1        VAL='CURRENT "PXIM" VALUES ARE:'
              IF(WC.EQ.'PXIM'.AND.SYSTEM1(94).EQ.1.0D0)
     1        VAL='CURRENT "PXIM FANG" VALUES ARE:'
              IF(WC.EQ.'PYIM'.AND.SYSTEM1(95).EQ.0.0D0)
     1        VAL='CURRENT "PYIM" VALUES ARE:'
              IF(WC.EQ.'PYIM'.AND.SYSTEM1(95).EQ.1.0D0)
     1        VAL='CURRENT "PYIM FANG" VALUES ARE:'
              V1=1
              IF(WC.EQ.'PXIM') THEN
                  VALUE1=SYSTEM1(92)
              ELSE
              END IF
              IF(WC.EQ.'PYIM') THEN
                  VALUE1=SYSTEM1(93)
              ELSE
              END IF
              GO TO 200
          ELSE
          END IF
          IF(WC.EQ.'RXIM'.OR.WC.EQ.'RYIM') THEN
              VA1=1
              IF(WC.EQ.'RXIM'.AND.SYSTEM1(98).EQ.0.0D0)
     1        VAL='CURRENT "RXIM" VALUES ARE:'
              IF(WC.EQ.'RXIM'.AND.SYSTEM1(98).EQ.1.0D0)
     1        VAL='CURRENT "RXIM FANG" VALUES ARE:'
              IF(WC.EQ.'RYIM'.AND.SYSTEM1(99).EQ.0.0D0)
     1        VAL='CURRENT "RYIM" VALUES ARE:'
              IF(WC.EQ.'RXIM'.AND.SYSTEM1(99).EQ.1.0D0)
     1        VAL='CURRENT "RYIM FANG" VALUES ARE:'
              V1=1
              IF(WC.EQ.'RXIM') THEN
                  VALUE1=SYSTEM1(96)
              ELSE
              END IF
              IF(WC.EQ.'RYIM') THEN
                  VALUE1=SYSTEM1(97)
              ELSE
              END IF
              GO TO 200
          ELSE
          END IF
          IF(WC.EQ.'ASTOP') THEN
              VA1=1
              VAL='CURRENT APERTURE STOP SURFACE NUMBER IS:'
              V1=1
              VALUE1=SYSTEM1(26)
              GO TO 200
          ELSE
          END IF
          IF(WC.EQ.'AUTOFUNC') THEN
              VA1=1
              VAL='CURRENT AUTOFUNC FUNCTION NUMBER IS:'
              V1=1
              VALUE1=SYSTEM1(91)
              GO TO 200
          ELSE
          END IF
          IF(WC.EQ.'DEFORM') THEN
              IF(ALENS(103,SURF).EQ.0.0D0) THEN
                  VA1=1
                  VAL='CURRENT SURFACE IS NOT A DEFORMABLE SURFACE'
              ELSE
                  VA1=1
                  VAL='CURRENT SURFACE IS A DEFORMABLE SURFACE'
                  V1=1
                  VALUE1=ALENS(104,SURF)
                  V2=1
                  VALUE2=ALENS(105,SURF)
                  V3=1
                  VALUE2=ALENS(106,SURF)
              END IF
              GO TO 200
          ELSE
          END IF
          IF(WC.EQ.'SPTWT') THEN
              VA1=1
              VAL='CURRENT SPECTRAL WEIGHTING FACTORS (1-5) ARE:'
              V1=1
              VALUE1=SYSTEM1(31)
              V2=1
              VALUE2=SYSTEM1(32)
              V3=1
              VALUE3=SYSTEM1(33)
              V4=1
              VALUE4=SYSTEM1(34)
              V5=1
              VALUE5=SYSTEM1(35)
              GO TO 200
          ELSE
          END IF
          IF(WC.EQ.'SPTWT2') THEN
              VA1=1
              VAL='CURRENT SPECTRAL WEIGHTING FACTORS (6-10) ARE:'
              V1=1
              VALUE1=SYSTEM1(76)
              V2=1
              VALUE2=SYSTEM1(77)
              V3=1
              VALUE3=SYSTEM1(78)
              V4=1
              VALUE4=SYSTEM1(79)
              V5=1
              VALUE5=SYSTEM1(80)
              GO TO 200
          ELSE
          END IF
          IF(WC.EQ.'CW') THEN
              VA1=1
              VAL='CURRENT CONTROL WAVELENGTH NUMBER IS:'
              V1=1
              VALUE1=SYSTEM1(11)
              GO TO 200
          ELSE
          END IF
          IF(WC.EQ.'PCW') THEN
              VA1=1
              VAL=
     1        'CURRENT PRIMARY WAVELENGTH PAIR NUMBERS ARE:'
              V1=1
              VALUE1=SYSTEM1(7)
              V2=1
              VALUE2=SYSTEM1(8)
              GO TO 200
          ELSE
          END IF
          IF(WC.EQ.'SCW') THEN
              VA1=1
              VAL=
     1        'CURRENT SECONDARY WAVELENGTH PAIR NUMBERS ARE:'
              V1=1
              VALUE1=SYSTEM1(9)
              V1=2
              VALUE1=SYSTEM1(10)
              GO TO 200
          ELSE
          END IF
          IF(WC.EQ.'MODE') THEN
              VA1=1
              IF(SYSTEM1(30).EQ.1) VAL='CURRENT MODE IS "FOCAL"'
              IF(SYSTEM1(30).EQ.2) VAL='CURRENT MODE IS "UFOCAL"'
              IF(SYSTEM1(30).EQ.3) VAL='CURRENT MODE IS "AFOCAL"'
              IF(SYSTEM1(30).EQ.4) VAL='CURRENT MODE IS "UAFOCAL"'
              GO TO 200
          ELSE
          END IF
          IF(WC.EQ.'CV') THEN
              VA1=1
              VAL='CURRENT SURFACE CURVATURE IS:'
              V1=1
              VALUE1=ALENS(1,SURF)
              GO TO 200
          ELSE
          END IF
          IF(WC.EQ.'INR') THEN
              VA1=1
              VAL='CURRENT SURFACE "INR" VALUE IS:'
              V1=1
              VALUE1=ALENS(76,SURF)
              GO TO 200
          ELSE
          END IF
          IF(WC.EQ.'CVTOR') THEN
              VA1=1
              VAL='CURRENT SURFACE TORIC CURVATURE IS:'
              V1=1
              VALUE1=ALENS(24,SURF)
              GO TO 200
          ELSE
          END IF
          IF(WC.EQ.'TH') THEN
              VA1=1
              VAL='CURRENT SURFACE THICKNESS IS:'
              V1=1
              VALUE1=ALENS(3,SURF)
              GO TO 200
          ELSE
          END IF
          IF(WC.EQ.'THM') THEN
              VA1=1
              VAL='CURRENT MIRROR THICKNESS IS:'
              V1=1
              VALUE1=ALENS(110,SURF)
              GO TO 200
          ELSE
          END IF
          IF(WC.EQ.'PRICE') THEN
              VA1=1
              VAL='CURRENT PRICE PER UNIT MASS IS:'
              V1=1
              VALUE1=ALENS(111,SURF)
              GO TO 200
          ELSE
          END IF
          IF(WC.EQ.'AC') THEN
              VA1=1
              VAL='CURRENT 2ND ORDER ASPHERIC SURFACE COEFFICIENT IS:'
              V1=1
              VALUE1=ALENS(43,SURF)
              GO TO 200
          ELSE
          END IF
          IF(WC.EQ.'AD') THEN
              VA1=1
              VAL='CURRENT 4TH ORDER ASPHERIC SURFACE COEFFICIENT IS:'
              V1=1
              VALUE1=ALENS(4,SURF)
              GO TO 200
          ELSE
          END IF
          IF(WC.EQ.'AE') THEN
              VA1=1
              VAL='CURRENT 6TH ORDER ASPHERIC SURFACE COEFFICIENT IS:'
              V1=1
              VALUE1=ALENS(5,SURF)
              GO TO 200
          ELSE
          END IF
          IF(WC.EQ.'AF') THEN
              VA1=1
              VAL='CURRENT 8TH ORDER ASPHERIC SURFACE COEFFICIENT IS:'
              V1=1
              VALUE1=ALENS(6,SURF)
              GO TO 200
          ELSE
          END IF
          IF(WC.EQ.'AG') THEN
              VA1=1
              VAL='CURRENT 10TH ORDER ASPHERIC SURFACE COEFFICIENT IS:'
              V1=1
              VALUE1=ALENS(7,SURF)
              GO TO 200
          ELSE
          END IF
          IF(WC.EQ.'AH') THEN
              VA1=1
              VAL='CURRENT 12TH ORDER ASPHERIC SURFACE COEFFICIENT IS:'
              V1=1
              VALUE1=ALENS(81,SURF)
              GO TO 200
          ELSE
          END IF
          IF(WC.EQ.'AI') THEN
              VA1=1
              VAL='CURRENT 14TH ORDER ASPHERIC SURFACE COEFFICIENT IS:'
              V1=1
              VALUE1=ALENS(82,SURF)
              GO TO 200
          ELSE
          END IF
          IF(WC.EQ.'AJ') THEN
              VA1=1
              VAL='CURRENT 16TH ORDER ASPHERIC SURFACE COEFFICIENT IS:'
              V1=1
              VALUE1=ALENS(83,SURF)
              GO TO 200
          ELSE
          END IF
          IF(WC.EQ.'AK') THEN
              VA1=1
              VAL='CURRENT 18TH ORDER ASPHERIC SURFACE COEFFICIENT IS:'
              V1=1
              VALUE1=ALENS(84,SURF)
              GO TO 200
          ELSE
          END IF
          IF(WC.EQ.'AL') THEN
              VA1=1
              VAL='CURRENT 20TH ORDER ASPHERIC SURFACE COEFFICIENT IS:'
              V1=1
              VALUE1=ALENS(85,SURF)
              GO TO 200
          ELSE
          END IF
          IF(WC.EQ.'ARRAY'.AND.ALENS(133,SURF).EQ.0.0D0) THEN
              VA1=1
              VAL=
     1        'SURFACE IS CURRENTLY NOT AN ARRAY LENS'
              GO TO 200
          ELSE
          END IF
          IF(WC.EQ.'ARRAY'.AND.ALENS(133,SURF).EQ.-1.0D0) THEN
              VA1=1
              VAL=
     1        'CURRENT ODD ARRAY SURFACE VALUES'
              VA1WS1=1
              VALWS1='(DX AND DY) ARE:'
              V1=1
              VALUE1=ALENS(131,SURF)
              V2=1
              VALUE2=ALENS(132,SURF)
              GO TO 200
          ELSE
          END IF
          IF(WC.EQ.'ARRAY'.AND.ALENS(133,SURF).EQ.1.0D0) THEN
              VA1=1
              VAL=
     1        'CURRENT EVEN ARRAY SURFACE VALUES'
              VA1WS1=1
              VALWS1='(DX AND DY) ARE:'
              V1=1
              VALUE1=ALENS(131,SURF)
              V2=1
              VALUE2=ALENS(132,SURF)
              GO TO 200
          ELSE
          END IF
          IF(WC.EQ.'ASPH') THEN
              VA1=1
              VAL=
     1        'CURRENT ASPHERIC SURFACE COEFFICIENTS'
              VA1WS1=1
              VALWS1='(AD, AE, AF, AG AND AC) ARE:'
              V1=1
              VALUE1=ALENS(4,SURF)
              V2=1
              VALUE2=ALENS(5,SURF)
              V3=1
              VALUE3=ALENS(6,SURF)
              V4=1
              VALUE4=ALENS(7,SURF)
              V5=1
              VALUE5=ALENS(43,SURF)
              GO TO 200
          ELSE
          END IF
          IF(WC.EQ.'ASPH2') THEN
              VA1=1
              VAL=
     1        'CURRENT ASPHERIC SURFACE COEFFICIENTS'
              VA1WS1=1
              VALWS1='(AH, AI, AJ, AK AND AL) ARE:'
              V1=1
              VALUE1=ALENS(81,SURF)
              V2=1
              VALUE2=ALENS(82,SURF)
              V3=1
              VALUE3=ALENS(83,SURF)
              V4=1
              VALUE4=ALENS(84,SURF)
              V5=1
              VALUE5=ALENS(85,SURF)
              GO TO 200
          ELSE
          END IF
          IF(WC.EQ.'TASPH') THEN
              VA1=1
              VAL=
     1        'CURRENT ANAMORPHIC ASPHERIC SURFACE COEFFICIENTS'
              VA1WS1=1
              VALWS1='(ADTOR, AETOR, AFTOR AND AGTOR) ARE:'
              V1=1
              VALUE1=ALENS(37,SURF)
              V2=1
              VALUE2=ALENS(38,SURF)
              V3=1
              VALUE3=ALENS(39,SURF)
              V4=1
              VALUE4=ALENS(40,SURF)
              GO TO 200
          ELSE
          END IF
          IF(WC.EQ.'ADTOR') THEN
              VA1=1
              VAL=
     1        'CURRENT 4TH ORDER ANAMORPHIC ASPHERIC SURFACE COEFFICIENT IS:'
              V1=1
              VALUE1=ALENS(37,SURF)
              GO TO 200
          ELSE
          END IF
          IF(WC.EQ.'AETOR') THEN
              VA1=1
              VAL=
     1        'CURRENT 6TH ORDER ANAMORPHIC ASPHERIC SURFACE COEFFICIENT IS:'
              V1=1
              VALUE1=ALENS(38,SURF)
              GO TO 200
          ELSE
          END IF
          IF(WC.EQ.'AFTOR') THEN
              VA1=1
              VAL=
     1        'CURRENT 8TH ORDER ANAMORPHIC ASPHERIC SURFACE COEFFICIENT IS:'
              V1=1
              VALUE1=ALENS(39,SURF)
              GO TO 200
          ELSE
          END IF
          IF(WC.EQ.'AGTOR') THEN
              VA1=1
              VAL=
     1        'CURRENT 10TH ORDER ANAMORPHIC ASPHERIC SURFACE COEFFICIENT IS:'
              V1=1
              VALUE1=ALENS(40,SURF)
              GO TO 200
          ELSE
          END IF
          IF(WC.EQ.'CC') THEN
              VA1=1
              VAL='SURFACE CONIC CONSTANT IS:'
              V1=1
              VALUE1=ALENS(2,SURF)
              GO TO 200
          ELSE
          END IF
          IF(WC.EQ.'CCTOR') THEN
              VA1=1
              VAL='CURRENT SURFACE ANAMORPHIC CONIC CONSTANT IS:'
              V1=1
              VALUE1=ALENS(41,SURF)
              GO TO 200
          ELSE
          END IF
          IF(WC.EQ.'GDX') THEN
              VA1=1
              VAL='SURFACE GLOBAL X-DECENTER IS:'
              V1=1
              VALUE1=ALENS(90,SURF)
              GO TO 200
          ELSE
          END IF
          IF(WC.EQ.'GDY') THEN
              VA1=1
              VAL='SURFACE GLOBAL Y-DECENTER IS:'
              V1=1
              VALUE1=ALENS(91,SURF)
              GO TO 200
          ELSE
          END IF
          IF(WC.EQ.'GDZ') THEN
              VA1=1
              VAL='SURFACE GLOBAL Z-DECENTER IS:'
              V1=1
              VALUE1=ALENS(92,SURF)
              GO TO 200
          ELSE
          END IF
          IF(WC.EQ.'GALPHA') THEN
              VA1=1
              VAL='SURFACE GLOBAL ALPHA TILT ANGLE IN DEGREES IS:'
              V1=1
              VALUE1=ALENS(93,SURF)
              GO TO 200
          ELSE
          END IF
          IF(WC.EQ.'GBETA') THEN
              VA1=1
              VAL='SURFACE GLOBAL BETA TILT ANGLE IN DEGREES IS:'
              V1=1
              VALUE1=ALENS(94,SURF)
              GO TO 200
          ELSE
          END IF
          IF(WC.EQ.'GGAMMA') THEN
              VA1=1
              VAL='SURFACE GLOBAL GAMMA TILT ANGLE IN DEGREES IS:'
              V1=1
              VALUE1=ALENS(95,SURF)
              GO TO 200
          ELSE
          END IF
          IF(WC.EQ.'ALPHA') THEN
              VA1=1
              VAL='SURFACE ALPHA TILT ANGLE IN DEGREES IS:'
              V1=1
              VALUE1=ALENS(118,SURF)
              GO TO 200
          ELSE
          END IF
          IF(WC.EQ.'BETA') THEN
              VA1=1
              VAL='SURFACE BETA TILT ANGLE IN DEGREES IS:'
              V1=1
              VALUE1=ALENS(119,SURF)
              GO TO 200
          ELSE
          END IF
          IF(WC.EQ.'GAMMA') THEN
              VA1=1
              VAL='SURFACE GAMMA TILT ANGLE IN DEGREES IS:'
              V1=1
              VALUE1=ALENS(120,SURF)
              GO TO 200
          ELSE
          END IF
          IF(WC.EQ.'TILT'.OR.WC.EQ.'RTILT') THEN
              IF(ALENS(25,SURF).EQ.0.0D0) THEN
                  VA1=1
                  VAL='SURFACE HAS NO TILT OR RTILT VALUES'
                  GO TO 200
              ELSE
              END IF
              VA1WS1=1
              VALWS1='SURFACE TILTS (ALPHA, BETA AND GAMMA) IN DEGREES ARE:'
              VA1=1
              VAL='SURFACE IS NOT CURRENTLY TILTED'
              IF(ALENS(25,SURF).EQ.1.0D0)
     1        VAL='CURRENT SURFACE HAS A "TILT" DEFINED ON IT'
              IF(ALENS(25,SURF).EQ.-1.0D0)
     1        VAL='CURRENT SURFACE HAS AN "RTILT" DEFINED ON IT'
              IF(ALENS(25,SURF).EQ.2.0D0)
     1        VAL='CURRENT SURFACE HAS A "TILT AUTO" DEFINED ON IT'
              IF(ALENS(25,SURF).EQ.3.0D0)
     1        VAL='CURRENT SURFACE HAS A "TILT AUTOM" DEFINED ON IT'
              IF(ALENS(25,SURF).EQ.4.0D0)
     1        VAL='CURRENT SURFACE HAS A "TILT BEN" DEFINED ON IT'
              IF(ALENS(25,SURF).EQ.5.0D0)
     1        VAL='CURRENT SURFACE HAS A "TILT DAR" DEFINED ON IT'
              IF(ALENS(25,SURF).EQ.7.0D0)
     1        VAL='CURRENT SURFACE HAS A "TILT REV" DEFINED ON IT'
              IF(ALENS(25,SURF).EQ.6.0D0.OR.ALENS(25,SURF).EQ.1.0D0.AND.
     1        ALENS(77,SURF).EQ.1.0D0)
     1        VAL='CURRENT SURFACE HAS A "TILT RET" DEFINED ON IT'
              IF(ALENS(25,SURF).EQ.6.0D0.OR.ALENS(25,SURF).EQ.1.0D0
     1        .AND.ALENS(77,SURF).EQ.1.0D0) THEN
                  V1=1
                  VALUE1=ALENS(70,SURF)
              ELSE
                  V1=1
                  VALUE1=ALENS(118,SURF)
                  V2=1
                  VALUE2=ALENS(119,SURF)
                  V3=1
                  VALUE3=ALENS(120,SURF)
              END IF
              GO TO 200
          ELSE
          END IF
          IF(WC.EQ.'XD') THEN
              VA1=1
              VAL='CURRENT SURFACE X-DECENTRATION IS:'
              V1=1
              VALUE1=ALENS(114,SURF)
              GO TO 200
          ELSE
          END IF
          IF(WC.EQ.'ZD') THEN
              VA1=1
              VAL='CURRENT SURFACE Z-DECENTRATION IS:'
              V1=1
              VALUE1=ALENS(116,SURF)
              GO TO 200
          ELSE
          END IF
          IF(WC.EQ.'YD') THEN
              VA1=1
              VAL='CURRENT SURFACE Y-DECENTRATION IS:'
              V1=1
              VALUE1=ALENS(115,SURF)
              GO TO 200
          ELSE
          END IF
          IF(WC.EQ.'DEC') THEN
              VA1=1
              VAL='CURRENT SURFACE Y, X AND Z DECENTRATIONS ARE:'
              V1=1
              VALUE1=ALENS(115,SURF)
              V2=1
              VALUE2=ALENS(114,SURF)
              V3=1
              VALUE3=ALENS(116,SURF)
              GO TO 200
          ELSE
          END IF
          IF(WC.EQ.'PIVOT') THEN
              VA1=1
              VAL='CURRENT SURFACE ALTERNATE PIVOT DEFINITIONS ARE:'
              V1=1
              VALUE1=ALENS(78,SURF)
              V2=1
              VALUE2=ALENS(79,SURF)
              V3=1
              VALUE3=ALENS(80,SURF)
              GO TO 200
          ELSE
          END IF
          IF(WC.EQ.'PIVX') THEN
              VA1=1
              VAL='CURRENT SURFACE X-PIVOT POSITION IS:'
              V1=1
              VALUE1=ALENS(78,SURF)
              GO TO 200
          ELSE
          END IF
          IF(WC.EQ.'PIVY') THEN
              VA1=1
              VAL='CURRENT SURFACE Y-PIVOT POSITION IS:'
              V1=1
              VALUE1=ALENS(79,SURF)
              GO TO 200
          ELSE
          END IF
          IF(WC.EQ.'PIVZ') THEN
              VA1=1
              VAL='CURRENT SURFACE Z-PIVOT POSITION IS:'
              V1=1
              VALUE1=ALENS(80,SURF)
              GO TO 200
          ELSE
          END IF
          IF(WC.EQ.'RD') THEN
              VA1=1
              VAL='CURRENT SURFACE RADIUS OF CURVATURE IS:'
              IF(DABS(ALENS(1,SURF)).LT.1D-30) THEN
                  V1=1
                  VALUE1=0.0D0
              ELSE
                  V1=1
                  VALUE1=1.0D0/ALENS(1,SURF)
              END IF
              GO TO 200
          ELSE
          END IF
          IF(WC.EQ.'XTORIC'.OR.WC.EQ.'YTORIC') THEN
              VA1=1
              VAL='THE SURFACE IS CURRENTLY:'
              IF(ALENS(23,SURF).EQ.0.0D0)
     1        VALWS1='NOT DEFINED AS A TORIC'
              IF(ALENS(23,SURF).EQ.1.0D0)
     1        VALWS1='DEFINED AS A Y-TORIC'
              IF(ALENS(23,SURF).EQ.2.0D0)
     1        VALWS1='DEFINED AS AN X-TORIC'
              VA1WS1=1
              GO TO 200
          ELSE
          END IF
          IF(WC.EQ.'GRT') THEN
              VA1=1
              VAL='THE SURFACE IS CURRENTLY:'
              IF(ALENS(96,SURF).EQ.0.0D0)
     1        VALWS1='NOT A LINEAR DIFFRATION GRATING'
              IF(ALENS(96,SURF).EQ.1.0D0)
     1        VALWS1='A LINEAR DIFFRATION GRATING'
              GO TO 200
          ELSE
          END IF
          IF(WC.EQ.'GRO') THEN
              IF(ALENS(96,SURF).EQ.0.0D0) THEN
                  VA1=1
                  VAL='THE SURFACE NOT A LINEAR DIFFRATION GRATING:'
              ELSE
                  V1=1
                  VALUE1=ALENS(97,SURF)
              END IF
              GO TO 200
          ELSE
          END IF
          IF(WC.EQ.'GRS') THEN
              IF(ALENS(96,SURF).EQ.0.0D0) THEN
                  VA1=1
                  VAL='THE SURFACE NOT A LINEAR DIFFRATION GRATING:'
              ELSE
                  V1=1
                  VALUE1=ALENS(98,SURF)
              END IF
              GO TO 200
          ELSE
          END IF
          IF(WC.EQ.'GRX') THEN
              IF(ALENS(96,SURF).EQ.0.0D0) THEN
                  VA1=1
                  VAL='THE SURFACE NOT A LINEAR DIFFRATION GRATING:'
              ELSE
                  V1=1
                  VALUE1=ALENS(99,SURF)
              END IF
              GO TO 200
          ELSE
          END IF
          IF(WC.EQ.'GRY') THEN
              IF(ALENS(96,SURF).EQ.0.0D0) THEN
                  VA1=1
                  VAL='THE SURFACE NOT A LINEAR DIFFRATION GRATING:'
              ELSE
                  V1=1
                  VALUE1=ALENS(100,SURF)
              END IF
              GO TO 200
          ELSE
          END IF
          IF(WC.EQ.'GRZ') THEN
              IF(ALENS(96,SURF).EQ.0.0D0) THEN
                  VA1=1
                  VAL='THE SURFACE NOT A LINEAR DIFFRATION GRATING:'
              ELSE
                  V1=1
                  VALUE1=ALENS(101,SURF)
              END IF
              GO TO 200
          ELSE
          END IF
          IF(WC.EQ.'SPGR') THEN
              V1=1
              VALUE1=ALENS(102,SURF)
              GO TO 200
          ELSE
          END IF
          IF(WC.EQ.'RDTOR') THEN
              VA1=1
              VAL='CURRENT SURFACE TORIC RADIUS OF CURVATURE IS:'
              IF(DABS(ALENS(24,SURF)).LT.1D-30) THEN
                  V1=1
                  VALUE1=0.0D0
              ELSE
                  V1=1
                  VALUE1=1.0D0/ALENS(24,SURF)
              END IF
              GO TO 200
          ELSE
          END IF
          IF(WC.EQ.'CCR') THEN
              IF(ALENS(126,SURF).EQ.2.0D0) THEN
                  VA1=1
                  VAL='CORNER CUBE DATA IS:'
                  V1=1
                  VALUE1=ALENS(138,SURF)
                  V1=2
                  VALUE1=ALENS(139,SURF)
                  V1=3
                  VALUE1=ALENS(140,SURF)
                  V1=4
                  VALUE1=ALENS(141,SURF)
              ELSE
                  VA1=1
                  VAL='SURFACE IS NOT A CORNER CUBE'
              END IF
              GO TO 200
          ELSE
          END IF
          IF(WC.EQ.'ROO') THEN
              IF(ALENS(126,SURF).EQ.1.0D0) THEN
                  VA1=1
                  VAL='ROOF DATA IS:'
                  V1=1
                  VALUE1=ALENS(138,SURF)
                  V1=2
                  VALUE1=ALENS(139,SURF)
              ELSE
                  VA1=1
                  VAL='SURFACE IS NOT A ROOF SURFACE'
              END IF
              GO TO 200
          ELSE
          END IF
          IF(WC.EQ.'RAYERROR') THEN
              VA1=1
              VAL='RANDON SURFACE RAY ERROR IS:'
              V1=1
              VALUE1=ALENS(144,SURF)
              GO TO 200
          ELSE
          END IF
          IF(WC.EQ.'COATING') THEN
              VA1=1
              VAL='CURRENT SURFACE COATING NUMBER IS:'
              V1=1
              VALUE1=ALENS(112,SURF)
              GO TO 200
          ELSE
          END IF
          IF(WC.EQ.'FOOTBLOK') THEN
              VA1=1
              IF(ALENS(58,SURF).EQ.0.0D0)
     1        VAL='CURRENT SURFACE DOES NOT HAVE A "FOOTBLOK" DEFINITION ON IT'
              IF(ALENS(58,SURF).EQ.1.0D0)
     1        VAL='CURRENT SURFACE HAS A "FOOTBLOK" DEFINITION ON IT'
              GO TO 200
          ELSE
          END IF
          IF(WC.EQ.'PIVAXIS') THEN
              VA1=1
              IF(ALENS(113,SURF).EQ.0.0D0)
     1        VAL='CURRENT SURFACE HAS "PIVAXIS" SET TO "LOCAL"'
              IF(ALENS(113,SURF).EQ.1.0D0)
     1        VAL='CURRENT SURFACE HAS "PIVAXIS" SET TO "NORMAL"'
              GO TO 200
          ELSE
          END IF
          IF(WC.EQ.'NODUM') THEN
              VA1=1
              IF(ALENS(68,SURF).EQ.0.0D0)
     1        VAL='SURFACE NOT FORCED TO DUMMY'
              IF(ALENS(68,SURF).EQ.1.0D0)
     1        VAL='SURFACE FORCED TO DUMMY'
              GO TO 200
          ELSE
          END IF
C       WV
          IF(WC.EQ.'WV') THEN
              VA1=1
              VAL='CURRENT OPTICAL SYSTEM WAVELENGTHS (1-5) ARE:'
              V1=1
              VALUE1=SYSTEM1(1)
              V2=1
              VALUE2=SYSTEM1(2)
              V3=1
              VALUE3=SYSTEM1(3)
              V4=1
              VALUE4=SYSTEM1(4)
              V5=1
              VALUE5=SYSTEM1(5)
              GO TO 200
          ELSE
          END IF
          IF(WC.EQ.'WV2') THEN
              VA1=1
              VAL='CURRENT OPTICAL SYSTEM WAVELENGTHS (6-10) ARE:'
              V1=1
              VALUE1=SYSTEM1(71)
              V2=1
              VALUE2=SYSTEM1(72)
              V3=1
              VALUE3=SYSTEM1(73)
              V4=1
              VALUE4=SYSTEM1(74)
              V5=1
              VALUE5=SYSTEM1(75)
              GO TO 200
          ELSE
          END IF
          IF(WC.EQ.'SPIDER') THEN
              IF(ALENS(134,SURF).EQ.0.0D0) THEN
                  VA1=1
                  VAL='NO SPIDER IS DEFINED ON THIS SURFACE'
              ELSE
                  VA1=1
                  VAL='CURRENT SPIDER VALUES ARE:'
                  V1=1
                  VALUE1=ALENS(135,SURF)
                  V2=1
                  VALUE2=ALENS(136,SURF)
                  V3=1
                  VALUE3=ALENS(137,SURF)
                  V4=1
                  VALUE4=ALENS(138,SURF)
                  GO TO 200
              END IF
          ELSE
          END IF
          IF(WC.EQ.'MULTCLAP') THEN
              OUTLYNE='"MULTCLAP" QUERRY IS NOT AVAILABLE. USE "CAOB" FROM THE'
              CALL SHOWIT(1)
              OUTLYNE='CMD PROGRAM LEVEL'
              CALL SHOWIT(1)
              CALL MACFAL
              GO TO 200
          END IF
          IF(WC.EQ.'MULTCOBS') THEN
              OUTLYNE='"MULTCOBS" QUERRY IS NOT AVAILABLE. USE "CAOB" FROM THE'
              CALL SHOWIT(1)
              OUTLYNE='CMD PROGRAM LEVEL'
              CALL SHOWIT(1)
              CALL MACFAL
              GO TO 200
          END IF
          IF(WC.EQ.'CLAP') THEN
              IF(DABS(ALENS(9,SURF)).EQ.0.0D0) THEN
                  ALENS(51:57,SURF)=0.0D0
                  OUTLYNE='NO "CLEAR APERTURE DATA" ON CURRENT SURFACE"'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  GO TO 200
              ELSE
                  VA1=1
                  IF(ALENS(9,SURF).EQ.1.0D0)
     1            VAL='CURRENT SURFACE "CLAP" VALUES ARE:'
                  IF(ALENS(9,SURF).EQ.2.0D0)
     1            VAL='CURRENT SURFACE "CLAP RECT" VALUES ARE:'
                  IF(ALENS(9,SURF).EQ.3.0D0)
     1            VAL='CURRENT SURFACE"CLAP ELIP" VALUES ARE:'
                  IF(ALENS(9,SURF).EQ.4.0D0)
     1            VAL='CURRENT SURFACE "CLAP RCTK" VALUES ARE:'
                  IF(ALENS(9,SURF).EQ.5.0D0)
     1            VAL='CURRENT SURFACE "CLAP POLY" VALUES ARE:'
                  IF(ALENS(9,SURF).EQ.6.0D0)
     1            VAL='CURRENT SURFACE "CLAP IPOLY" VALUES ARE:'
                  V1=1
                  VALUE1=ALENS(10,SURF)
                  V2=1
                  VALUE2=ALENS(11,SURF)
                  V3=1
                  VALUE3=ALENS(12,SURF)
                  V4=1
                  VALUE4=ALENS(13,SURF)
                  V5=1
                  VALUE5=ALENS(14,SURF)
                  V6=1
                  VALUE6=ALENS(15,SURF)
                  GO TO 200
              END IF
          ELSE
C       NOT CLAP
          END IF
          IF(WC.EQ.'CLAP') THEN
              IF(DABS(ALENS(51,SURF)).EQ.0.0D0) THEN
                  ALENS(51:57,SURF)=0.0D0
                  OUTLYNE='NO "CLEAR APERTURE ERASE DATA" ON CURRENT SURFACE"'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  GO TO 200
              ELSE
                  VA1=1
                  IF(ALENS(51,SURF).EQ.1.0D0)
     1            VAL='CURRENT SURFACE "CLAP ERASE" VALUES ARE:'
                  IF(ALENS(51,SURF).EQ.2.0D0)
     1            VAL='CURRENT SURFACE "CLAP RECTE" VALUES ARE:'
                  IF(ALENS(51,SURF).EQ.3.0D0)
     1            VAL='CURRENT SURFACE "CLAP ELIPE" VALUES ARE:'
                  IF(ALENS(51,SURF).EQ.4.0D0)
     1            VAL='CURRENT SURFACE "CLAP RCTKE" VALUES ARE:'
                  IF(ALENS(51,SURF).EQ.5.0D0)
     1            VAL='CURRENT SURFACE "CLAP POLYE" VALUES ARE:'
                  IF(ALENS(51,SURF).EQ.6.0D0)
     1            VAL='CURRENT SURFACE "CLAP IPOLYE" VALUES ARE:'
                  V1=1
                  VALUE1=ALENS(52,SURF)
                  V2=1
                  VALUE2=ALENS(53,SURF)
                  V3=1
                  VALUE3=ALENS(54,SURF)
                  V4=1
                  VALUE4=ALENS(55,SURF)
                  V5=1
                  VALUE5=ALENS(56,SURF)
                  V6=1
                  VALUE6=ALENS(57,SURF)
                  GO TO 200
              END IF
          ELSE
C       NOT CLAP
          END IF
          IF(WC.EQ.'COBS') THEN
              IF(DABS(ALENS(16,SURF)).EQ.0.0D0) THEN
                  ALENS(61:67,SURF)=0.0D0
                  OUTLYNE='NO "OBSCURATION DATA" ON CURRENT SURFACE"'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  GO TO 200
              ELSE
                  VA1=1
                  IF(ALENS(16,SURF).EQ.1.0D0)
     1            VAL='CURRENT SURFACE "COBS" VALUES ARE:'
                  IF(ALENS(16,SURF).EQ.2.0D0)
     1            VAL='CURRENT SURFACE "COBS RECT" VALUES ARE:'
                  IF(ALENS(16,SURF).EQ.3.0D0)
     1            VAL='CURRENT SURFACE "COBS ELIP" VALUES ARE:'
                  IF(ALENS(16,SURF).EQ.4.0D0)
     1            VAL='CURRENT SURFACE "COBS RCTK" VALUES ARE:'
                  IF(ALENS(16,SURF).EQ.5.0D0)
     1            VAL='CURRENT SURFACE "COBS POLY" VALUES ARE:'
                  IF(ALENS(16,SURF).EQ.6.0D0)
     1            VAL='CURRENT SURFACE "COBS IPOLY" VALUES ARE:'
                  V1=1
                  VALUE1=ALENS(17,SURF)
                  V2=1
                  VALUE2=ALENS(18,SURF)
                  V3=1
                  VALUE3=ALENS(19,SURF)
                  V4=1
                  VALUE4=ALENS(20,SURF)
                  V5=1
                  VALUE5=ALENS(21,SURF)
                  V6=1
                  VALUE6=ALENS(22,SURF)
                  GO TO 200
              END IF
          ELSE
C       NOT COBS
          END IF
          IF(WC.EQ.'COBS') THEN
              IF(DABS(ALENS(61,SURF)).EQ.0.0D0) THEN
                  ALENS(61:67,SURF)=0.0D0
                  OUTLYNE='NO "OBSCURATION ERASE DATA" ON CURRENT SURFACE"'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  GO TO 200
              ELSE
                  VA1=1
                  IF(ALENS(61,SURF).EQ.1.0D0)
     1            VAL='CURRENT SURFACE "COBS ERASE" VALUES ARE:'
                  IF(ALENS(61,SURF).EQ.2.0D0)
     1            VAL='CURRENT SURFACE "COBS RECTE" VALUES ARE:'
                  IF(ALENS(61,SURF).EQ.3.0D0)
     1            VAL='CURRENT SURFACE "COBS ELIPE" VALUES ARE:'
                  IF(ALENS(61,SURF).EQ.4.0D0)
     1            VAL='CURRENT SURFACE "COBS RCTKE" VALUES ARE:'
                  IF(ALENS(61,SURF).EQ.5.0D0)
     1            VAL='CURRENT SURFACE "COBS POLYE" VALUES ARE:'
                  IF(ALENS(61,SURF).EQ.6.0D0)
     1            VAL='CURRENT SURFACE "COBS IPOLYE" VALUES ARE:'
                  V1=1
                  VALUE1=ALENS(62,SURF)
                  V2=1
                  VALUE2=ALENS(63,SURF)
                  V3=1
                  VALUE3=ALENS(64,SURF)
                  V4=1
                  VALUE4=ALENS(65,SURF)
                  V5=1
                  VALUE5=ALENS(66,SURF)
                  V6=1
                  VALUE6=ALENS(67,SURF)
                  GO TO 200
              END IF
          ELSE
C       NOT COBS
          END IF
C       SOLVES
          IF(WC.EQ.'PY'.OR.WC.EQ.'PX'.OR.WC.EQ.'PCY'.OR.WC.EQ.'PCX'
     1    .OR.WC.EQ.'CAY'.OR.WC.EQ.'CAX'.OR.WC.EQ.'PUY'.OR.WC.EQ.'PUX'
     2    .OR.WC.EQ.'PUCY'.OR.WC.EQ.'PUCX'.OR.WC.EQ.'COCY'.OR.WC.EQ.
     3    'COCX'.OR.WC.EQ.'APY'.OR.WC.EQ.'APX'.OR.WC.EQ.'APCY'.OR.
     4    WC.EQ.'APCX'.OR.WC.EQ.'PIY'.OR.WC.EQ.'PIX'.OR.WC.EQ.
     5    'PICY'.OR.WC.EQ.'PICX') THEN
C
              IF(SOLVE(6,SURF).EQ.1.0D0) THEN
                  VA1=1
                  VAL='CURRENT SURFACE SOLVE IS A "PY" SOLVE WITH TARGET VALUE:'
                  V1=1
                  VALUE1=SOLVE(7,SURF)
                  GO TO 200
              ELSE
              END IF
              IF(SOLVE(6,SURF).EQ.2.0D0) THEN
                  VA1=1
                  VAL='CURRENT SURFACE SOLVE IS A "PCY" SOLVE WITH TARGET VALUE:'
                  V1=1
                  VALUE1=SOLVE(7,SURF)
                  GO TO 200
              ELSE
              END IF
              IF(SOLVE(6,SURF).EQ.3.0D0) THEN
                  VA1=1
                  VAL='CURRENT SURFACE SOLVE IS A "CAY" SOLVE WITH TARGET VALUE:'
                  V1=1
                  VALUE1=SOLVE(7,SURF)
                  GO TO 200
              ELSE
              END IF
C
              IF(SOLVE(4,SURF).EQ.4.0D0) THEN
                  VA1=1
                  VAL='CURRENT SURFACE SOLVE IS A "PX" SOLVE WITH TARGET VALUE:'
                  V1=1
                  VALUE1=SOLVE(3,SURF)
                  GO TO 200
              ELSE
              END IF
              IF(SOLVE(4,SURF).EQ.5.0D0) THEN
                  VA1=1
                  VAL='CURRENT SURFACE SOLVE IS A "PCX" SOLVE WITH TARGET VALUE:'
                  V1=1
                  VALUE1=SOLVE(3,SURF)
                  GO TO 200
              ELSE
              END IF
              IF(SOLVE(4,SURF).EQ.6.0D0) THEN
                  VA1=1
                  VAL='CURRENT SURFACE SOLVE IS A "CAX" SOLVE WITH TARGET VALUE:'
                  V1=1
                  VALUE1=SOLVE(3,SURF)
                  GO TO 200
              ELSE
              END IF
C
              IF(SOLVE(8,SURF).EQ.1.0D0) THEN
                  VA1=1
                  VAL='CURRENT SURFACE SOLVE IS AN "APY" SOLVE'
                  GO TO 200
              ELSE
              END IF
              IF(SOLVE(8,SURF).EQ.2.0D0) THEN
                  VA1=1
                  VAL='CURRENT SURFACE SOLVE IS A "PIY" SOLVE WITH TARGET VALUE:'
                  V1=1
                  VALUE1=SOLVE(9,SURF)
                  GO TO 200
              ELSE
              END IF
              IF(SOLVE(8,SURF).EQ.3.0D0) THEN
                  VA1=1
                  VAL='CURRENT SURFACE SOLVE IS A "PUY" SOLVE WITH TARGET VALUE:'
                  V1=1
                  VALUE1=SOLVE(9,SURF)
                  GO TO 200
              ELSE
              END IF
              IF(SOLVE(8,SURF).EQ.4.0D0) THEN
                  VA1=1
                  VAL='CURRENT SURFACE SOLVE IS AN "APCY" SOLVE'
                  GO TO 200
              ELSE
              END IF
              IF(SOLVE(8,SURF).EQ.5.0D0) THEN
                  VA1=1
                  VAL='CURRENT SURFACE SOLVE IS A "PICY" SOLVE WITH TARGET VALUE:'
                  V1=1
                  VALUE1=SOLVE(9,SURF)
                  GO TO 200
              ELSE
              END IF
              IF(SOLVE(8,SURF).EQ.6.0D0) THEN
                  VA1=1
                  VAL='CURRENT SURFACE SOLVE IS A "PUCY" SOLVE WITH TARGET VALUE:'
                  V1=1
                  VALUE1=SOLVE(9,SURF)
                  GO TO 200
              ELSE
              END IF
              IF(SOLVE(8,SURF).EQ.7.0D0) THEN
                  VA1=1
                  VAL='CURRENT SURFACE SOLVE IS A "COCY" SOLVE TO SURFACE #:'
                  V1=1
                  VALUE1=SOLVE(9,SURF)
                  GO TO 200
              ELSE
              END IF
C
              IF(SOLVE(2,SURF).EQ.8.0D0) THEN
                  VA1=1
                  VAL='CURRENT SURFACE SOLVE IS AN "APX" SOLVE'
                  GO TO 200
              ELSE
              END IF
              IF(SOLVE(2,SURF).EQ.9.0D0) THEN
                  VA1=1
                  VAL='CURRENT SURFACE SOLVE IS A "PIX" SOLVE WITH TARGET VALUE:'
                  V1=1
                  VALUE1=SOLVE(1,SURF)
                  GO TO 200
              ELSE
              END IF
              IF(SOLVE(2,SURF).EQ.10.0D0) THEN
                  VA1=1
                  VAL='CURRENT SURFACE SOLVE IS A "PUX" SOLVE WITH TARGET VALUE:'
                  V1=1
                  VALUE1=SOLVE(1,SURF)
                  GO TO 200
              ELSE
              END IF
              IF(SOLVE(2,SURF).EQ.11.0D0) THEN
                  VA1=1
                  VAL='CURRENT SURFACE SOLVE IS AN "APCX" SOLVE'
                  GO TO 200
              ELSE
              END IF
              IF(SOLVE(2,SURF).EQ.12.0D0) THEN
                  VA1=1
                  VAL='CURRENT SURFACE SOLVE IS A "PICX" SOLVE WITH TARGET VALUE:'
                  V1=1
                  VALUE1=SOLVE(1,SURF)
                  GO TO 200
              ELSE
              END IF
              IF(SOLVE(2,SURF).EQ.13.0D0) THEN
                  VA1=1
                  VAL='CURRENT SURFACE SOLVE IS A "PUCX" SOLVE WITH TARGET VALUE:'
                  V1=1
                  VALUE1=SOLVE(1,SURF)
                  GO TO 200
              ELSE
              END IF
              IF(SOLVE(2,SURF).EQ.14.0D0) THEN
                  VA1=1
                  VAL='CURRENT SURFACE SOLVE IS A "COCX" SOLVE TO SURFACE #:'
                  V1=1
                  VALUE1=SOLVE(1,SURF)
                  GO TO 200
              ELSE
              END IF
              IF(SOLVE(6,SURF).EQ.0.0D0.AND.SOLVE(4,SURF).EQ.0.0D0
     1        .AND.SOLVE(8,SURF).EQ.0.0D0.AND.SOLVE(2,SURF).EQ.0.0D0)THEN
                  VA1=1
                  VAL='CURRENT SURFACE HAS NO SOLVES'
                  GO TO 200
              ELSE
              END IF
          ELSE
C       NOT A SOLVE
          END IF
 300      CONTINUE
          IF(WC.EQ.'PIKUP') THEN
C       PIKUPS HERE
              DO I=1,PSIZ
                  IF(PIKUP(1,SURF,I).NE.0.0D0) REPEAT=REPEAT+1
              END DO
              IF(REPEAT.EQ.0) THEN
                  VA1=1
                  VAL='CURRENT SURFACE HAS NO PIKUPS DEFINED ON IT'
                  GO TO 200
              ELSE
C       THERE ARE PIKUPS, TAKE APPROPRIATE ACTION
                  DO I=1,PSIZ
                      IF(PIKUP(1,SURF,I).NE.0.0D0) THEN
C
                          IF(I.EQ.1) THEN
                              VA1=1
                              VAL='CURRENT SURFACE HAS AN "RD" PIKUP ON IT WITH VALUES:'
                              V1=1
                              V2=1
                              V3=1
                              V4=1
                              V5=1
                              VALUE1=PIKUP(2,SURF,I)
                              VALUE2=PIKUP(3,SURF,I)
                              VALUE3=PIKUP(4,SURF,I)
                              VALUE4=PIKUP(5,SURF,I)
                              VALUE5=PIKUP(6,SURF,I)
                              GO TO 200
                          ELSE
C       PROCEED TO NEXT PIKUP TYPE
                          END IF
C
                          IF(I.EQ.2) THEN
                              VA1=1
                              VAL='CURRENT SURFACE HAS AN "CV" PIKUP ON IT WITH VALUES:'
                              V1=1
                              V2=1
                              V3=1
                              V4=1
                              V5=1
                              VALUE1=PIKUP(2,SURF,I)
                              VALUE2=PIKUP(3,SURF,I)
                              VALUE3=PIKUP(4,SURF,I)
                              VALUE4=PIKUP(5,SURF,I)
                              VALUE5=PIKUP(6,SURF,I)
                              GO TO 200
                          ELSE
C       PROCEED TO NEXT PIKUP TYPE
                          END IF
C
                          IF(I.EQ.3) THEN
                              VA1=1
                              VAL='CURRENT SURFACE HAS AN "TH" PIKUP ON IT WITH VALUES:'
                              V1=1
                              V2=1
                              V3=1
                              V4=1
                              V5=1
                              VALUE1=PIKUP(2,SURF,I)
                              VALUE2=PIKUP(3,SURF,I)
                              VALUE3=PIKUP(4,SURF,I)
                              VALUE4=PIKUP(5,SURF,I)
                              VALUE5=PIKUP(6,SURF,I)
                              GO TO 200
                          ELSE
C       PROCEED TO NEXT PIKUP TYPE
                          END IF
C
                          IF(I.EQ.32) THEN
                              VA1=1
                              VAL='CURRENT SURFACE HAS AN "THOAL" PIKUP ON IT WITH VALUES:'
                              V1=1
                              V2=1
                              V3=1
                              V4=1
                              V5=1
                              VALUE1=PIKUP(2,SURF,I)
                              VALUE2=PIKUP(3,SURF,I)
                              VALUE3=PIKUP(4,SURF,I)
                              VALUE4=PIKUP(5,SURF,I)
                              VALUE5=PIKUP(6,SURF,I)
                              GO TO 200
                          ELSE
C       PROCEED TO NEXT PIKUP TYPE
                          END IF
C
                          IF(I.EQ.4) THEN
                              VA1=1
                              VAL='CURRENT SURFACE HAS AN "CC" PIKUP ON IT WITH VALUES:'
                              V1=1
                              V2=1
                              V3=1
                              V4=1
                              V5=1
                              VALUE1=PIKUP(2,SURF,I)
                              VALUE2=PIKUP(3,SURF,I)
                              VALUE3=PIKUP(4,SURF,I)
                              VALUE4=PIKUP(5,SURF,I)
                              VALUE5=PIKUP(6,SURF,I)
                              GO TO 200
                          ELSE
C       PROCEED TO NEXT PIKUP TYPE
                          END IF
C
                          IF(I.EQ.5) THEN
                              VA1=1
                              VAL='CURRENT SURFACE HAS AN "AD" PIKUP ON IT WITH VALUES:'
                              V1=1
                              V2=1
                              V3=1
                              V4=1
                              V5=1
                              VALUE1=PIKUP(2,SURF,I)
                              VALUE2=PIKUP(3,SURF,I)
                              VALUE3=PIKUP(4,SURF,I)
                              VALUE4=PIKUP(5,SURF,I)
                              VALUE5=PIKUP(6,SURF,I)
                              GO TO 200
                          ELSE
C       PROCEED TO NEXT PIKUP TYPE
                          END IF
C
                          IF(I.EQ.6) THEN
                              VA1=1
                              VAL='CURRENT SURFACE HAS AN "AE" PIKUP ON IT WITH VALUES:'
                              V1=1
                              V2=1
                              V3=1
                              V4=1
                              V5=1
                              VALUE1=PIKUP(2,SURF,I)
                              VALUE2=PIKUP(3,SURF,I)
                              VALUE3=PIKUP(4,SURF,I)
                              VALUE4=PIKUP(5,SURF,I)
                              VALUE5=PIKUP(6,SURF,I)
                              GO TO 200
                          ELSE
C       PROCEED TO NEXT PIKUP TYPE
                          END IF
C
                          IF(I.EQ.7) THEN
                              VA1=1
                              VAL='CURRENT SURFACE HAS AN "AF" PIKUP ON IT WITH VALUES:'
                              V1=1
                              V2=1
                              V3=1
                              V4=1
                              V5=1
                              VALUE1=PIKUP(2,SURF,I)
                              VALUE2=PIKUP(3,SURF,I)
                              VALUE3=PIKUP(4,SURF,I)
                              VALUE4=PIKUP(5,SURF,I)
                              VALUE5=PIKUP(6,SURF,I)
                              GO TO 200
                          ELSE
C       PROCEED TO NEXT PIKUP TYPE
                          END IF
C
                          IF(I.EQ.8) THEN
                              VA1=1
                              VAL='CURRENT SURFACE HAS AN "AG" PIKUP ON IT WITH VALUES:'
                              V1=1
                              V2=1
                              V3=1
                              V4=1
                              V5=1
                              VALUE1=PIKUP(2,SURF,I)
                              VALUE2=PIKUP(3,SURF,I)
                              VALUE3=PIKUP(4,SURF,I)
                              VALUE4=PIKUP(5,SURF,I)
                              VALUE5=PIKUP(6,SURF,I)
                              GO TO 200
                          ELSE
C       PROCEED TO NEXT PIKUP TYPE
                          END IF
C
                          IF(I.EQ.9) THEN
                              VA1=1
                              VAL='CURRENT SURFACE HAS AN "CVTOR" PIKUP ON IT WITH VALUES:'
                              V1=1
                              V2=1
                              V3=1
                              V4=1
                              V5=1
                              VALUE1=PIKUP(2,SURF,I)
                              VALUE2=PIKUP(3,SURF,I)
                              VALUE3=PIKUP(4,SURF,I)
                              VALUE4=PIKUP(5,SURF,I)
                              VALUE5=PIKUP(6,SURF,I)
                              GO TO 200
                          ELSE
C       PROCEED TO NEXT PIKUP TYPE
                          END IF
C
                          IF(I.EQ.10) THEN
                              VA1=1
                              VAL='CURRENT SURFACE HAS AN "RDTOR" PIKUP ON IT WITH VALUES:'
                              V1=1
                              V2=1
                              V3=1
                              V4=1
                              V5=1
                              VALUE1=PIKUP(2,SURF,I)
                              VALUE2=PIKUP(3,SURF,I)
                              VALUE3=PIKUP(4,SURF,I)
                              VALUE4=PIKUP(5,SURF,I)
                              VALUE5=PIKUP(6,SURF,I)
                              GO TO 200
                          ELSE
C       PROCEED TO NEXT PIKUP TYPE
                          END IF
C
                          IF(I.EQ.11) THEN
                              VA1=1
                              VAL='CURRENT SURFACE HAS AN "PRO" PIKUP ON IT WITH VALUES:'
                              V1=1
                              V2=1
                              V3=1
                              V4=1
                              V5=1
                              VALUE1=PIKUP(2,SURF,I)
                              VALUE2=PIKUP(3,SURF,I)
                              VALUE3=PIKUP(4,SURF,I)
                              VALUE4=PIKUP(5,SURF,I)
                              VALUE5=PIKUP(6,SURF,I)
                              GO TO 200
                          ELSE
C       PROCEED TO NEXT PIKUP TYPE
                          END IF
C
                          IF(I.EQ.12) THEN
                              VA1=1
                              VAL='CURRENT SURFACE HAS AN "NPRO" PIKUP ON IT WITH VALUES:'
                              V1=1
                              V2=1
                              V3=1
                              V4=1
                              V5=1
                              VALUE1=PIKUP(2,SURF,I)
                              VALUE2=PIKUP(3,SURF,I)
                              VALUE3=PIKUP(4,SURF,I)
                              VALUE4=PIKUP(5,SURF,I)
                              VALUE5=PIKUP(6,SURF,I)
                              GO TO 200
                          ELSE
C       PROCEED TO NEXT PIKUP TYPE
                          END IF
C
                          IF(I.EQ.13) THEN
                              VA1=1
                              VAL='CURRENT SURFACE HAS AN "YD" PIKUP ON IT WITH VALUES:'
                              V1=1
                              V2=1
                              V3=1
                              V4=1
                              V5=1
                              VALUE1=PIKUP(2,SURF,I)
                              VALUE2=PIKUP(3,SURF,I)
                              VALUE3=PIKUP(4,SURF,I)
                              VALUE4=PIKUP(5,SURF,I)
                              VALUE5=PIKUP(6,SURF,I)
                              GO TO 200
                          ELSE
C       PROCEED TO NEXT PIKUP TYPE
                          END IF
C
                          IF(I.EQ.14) THEN
                              VA1=1
                              VAL='CURRENT SURFACE HAS AN "XD" PIKUP ON IT WITH VALUES:'
                              V1=1
                              V2=1
                              V3=1
                              V4=1
                              V5=1
                              VALUE1=PIKUP(2,SURF,I)
                              VALUE2=PIKUP(3,SURF,I)
                              VALUE3=PIKUP(4,SURF,I)
                              VALUE4=PIKUP(5,SURF,I)
                              VALUE5=PIKUP(6,SURF,I)
                              GO TO 200
                          ELSE
C       PROCEED TO NEXT PIKUP TYPE
                          END IF
C
                          IF(I.EQ.15) THEN
                              VA1=1
                              VAL='CURRENT SURFACE HAS AN "ALPHA" PIKUP ON IT WITH VALUES:'
                              V1=1
                              V2=1
                              V3=1
                              V4=1
                              V5=1
                              VALUE1=PIKUP(2,SURF,I)
                              VALUE2=PIKUP(3,SURF,I)
                              VALUE3=PIKUP(4,SURF,I)
                              VALUE4=PIKUP(5,SURF,I)
                              VALUE5=PIKUP(6,SURF,I)
                              GO TO 200
                          ELSE
C       PROCEED TO NEXT PIKUP TYPE
                          END IF
C
                          IF(I.EQ.16) THEN
                              VA1=1
                              VAL='CURRENT SURFACE HAS AN "BETA" PIKUP ON IT WITH VALUES:'
                              V1=1
                              V2=1
                              V3=1
                              V4=1
                              V5=1
                              VALUE1=PIKUP(2,SURF,I)
                              VALUE2=PIKUP(3,SURF,I)
                              VALUE3=PIKUP(4,SURF,I)
                              VALUE4=PIKUP(5,SURF,I)
                              VALUE5=PIKUP(6,SURF,I)
                              GO TO 200
                          ELSE
C       PROCEED TO NEXT PIKUP TYPE
                          END IF
C
                          IF(I.EQ.17) THEN
                              VA1=1
                              VAL='CURRENT SURFACE HAS AN "GAMMA" PIKUP ON IT WITH VALUES:'
                              V1=1
                              V2=1
                              V3=1
                              V4=1
                              V5=1
                              VALUE1=PIKUP(2,SURF,I)
                              VALUE2=PIKUP(3,SURF,I)
                              VALUE3=PIKUP(4,SURF,I)
                              VALUE4=PIKUP(5,SURF,I)
                              VALUE5=PIKUP(6,SURF,I)
                              GO TO 200
                          ELSE
C       PROCEED TO NEXT PIKUP TYPE
                          END IF
C
                          IF(I.EQ.18) THEN
                              VA1=1
                              VAL='CURRENT SURFACE HAS AN "CLAP" PIKUP ON IT WITH VALUES:'
                              V1=1
                              V2=1
                              V3=1
                              V4=1
                              V5=1
                              VALUE1=PIKUP(2,SURF,I)
                              VALUE2=PIKUP(3,SURF,I)
                              VALUE3=PIKUP(4,SURF,I)
                              VALUE4=PIKUP(5,SURF,I)
                              VALUE5=PIKUP(6,SURF,I)
                              GO TO 200
                          ELSE
C       PROCEED TO NEXT PIKUP TYPE
                          END IF
C
                          IF(I.EQ.19) THEN
                              VA1=1
                              VAL='CURRENT SURFACE HAS AN "COBS" PIKUP ON IT WITH VALUES:'
                              V1=1
                              V2=1
                              V3=1
                              V4=1
                              V5=1
                              VALUE1=PIKUP(2,SURF,I)
                              VALUE2=PIKUP(3,SURF,I)
                              VALUE3=PIKUP(4,SURF,I)
                              VALUE4=PIKUP(5,SURF,I)
                              VALUE5=PIKUP(6,SURF,I)
                              GO TO 200
                          ELSE
C       PROCEED TO NEXT PIKUP TYPE
                          END IF
C
                          IF(I.EQ.20) THEN
                              VA1=1
                              VAL='CURRENT SURFACE HAS AN "GLASS" PIKUP ON IT WITH VALUES:'
                              V1=1
                              V2=1
                              V3=1
                              V4=1
                              V5=1
                              VALUE1=PIKUP(2,SURF,I)
                              VALUE2=PIKUP(3,SURF,I)
                              VALUE3=PIKUP(4,SURF,I)
                              VALUE4=PIKUP(5,SURF,I)
                              VALUE5=PIKUP(6,SURF,I)
                              GO TO 200
                          ELSE
C       PROCEED TO NEXT PIKUP TYPE
                          END IF
C
                          IF(I.EQ.21) THEN
                              VA1=1
                              VAL='CURRENT SURFACE HAS AN "CCTOR" PIKUP ON IT WITH VALUES:'
                              V1=1
                              V2=1
                              V3=1
                              V4=1
                              V5=1
                              VALUE1=PIKUP(2,SURF,I)
                              VALUE2=PIKUP(3,SURF,I)
                              VALUE3=PIKUP(4,SURF,I)
                              VALUE4=PIKUP(5,SURF,I)
                              VALUE5=PIKUP(6,SURF,I)
                              GO TO 200
                          ELSE
C       PROCEED TO NEXT PIKUP TYPE
                          END IF
C
                          IF(I.EQ.22) THEN
                              VA1=1
                              VAL='CURRENT SURFACE HAS AN "ADTOR" PIKUP ON IT WITH VALUES:'
                              V1=1
                              V2=1
                              V3=1
                              V4=1
                              V5=1
                              VALUE1=PIKUP(2,SURF,I)
                              VALUE2=PIKUP(3,SURF,I)
                              VALUE3=PIKUP(4,SURF,I)
                              VALUE4=PIKUP(5,SURF,I)
                              VALUE5=PIKUP(6,SURF,I)
                              GO TO 200
                          ELSE
C       PROCEED TO NEXT PIKUP TYPE
                          END IF
C
                          IF(I.EQ.23) THEN
                              VA1=1
                              VAL='CURRENT SURFACE HAS AN "AETOR" PIKUP ON IT WITH VALUES:'
                              V1=1
                              V2=1
                              V3=1
                              V4=1
                              V5=1
                              VALUE1=PIKUP(2,SURF,I)
                              VALUE2=PIKUP(3,SURF,I)
                              VALUE3=PIKUP(4,SURF,I)
                              VALUE4=PIKUP(5,SURF,I)
                              VALUE5=PIKUP(6,SURF,I)
                              GO TO 200
                          ELSE
C       PROCEED TO NEXT PIKUP TYPE
                          END IF
C
                          IF(I.EQ.24) THEN
                              VA1=1
                              VAL='CURRENT SURFACE HAS AN "AFTOR" PIKUP ON IT WITH VALUES:'
                              V1=1
                              V2=1
                              V3=1
                              V4=1
                              V5=1
                              VALUE1=PIKUP(2,SURF,I)
                              VALUE2=PIKUP(3,SURF,I)
                              VALUE3=PIKUP(4,SURF,I)
                              VALUE4=PIKUP(5,SURF,I)
                              VALUE5=PIKUP(6,SURF,I)
                              GO TO 200
                          ELSE
C       PROCEED TO NEXT PIKUP TYPE
                          END IF
C
                          IF(I.EQ.25) THEN
                              VA1=1
                              VAL='CURRENT SURFACE HAS AN "AGTOR" PIKUP ON IT WITH VALUES:'
                              V1=1
                              V2=1
                              V3=1
                              V4=1
                              V5=1
                              VALUE1=PIKUP(2,SURF,I)
                              VALUE2=PIKUP(3,SURF,I)
                              VALUE3=PIKUP(4,SURF,I)
                              VALUE4=PIKUP(5,SURF,I)
                              VALUE5=PIKUP(6,SURF,I)
                              GO TO 200
                          ELSE
C       PROCEED TO NEXT PIKUP TYPE
                          END IF
C
                          IF(I.EQ.26) THEN
                              VA1=1
                              VAL='CURRENT SURFACE HAS AN "AC" PIKUP ON IT WITH VALUES:'
                              V1=1
                              V2=1
                              V3=1
                              V4=1
                              V5=1
                              VALUE1=PIKUP(2,SURF,I)
                              VALUE2=PIKUP(3,SURF,I)
                              VALUE3=PIKUP(4,SURF,I)
                              VALUE4=PIKUP(5,SURF,I)
                              VALUE5=PIKUP(6,SURF,I)
                              GO TO 200
                          ELSE
C       PROCEED TO NEXT PIKUP TYPE
                          END IF
C
                          IF(I.GT.26) THEN
C                 DO NOTHING
                          ELSE
                          END IF
                      ELSE
C       PROCEED BY INCREMENTING I BY 1
                      END IF
                  END DO
              END IF
          ELSE
C       WC NOT PIKUP
          END IF
          RETURN
C
 200      CALL LQAS(VALUE1,VALUE2,VALUE3,VALUE4,VALUE5,VAL,
     1    V1,V2,V3,V4,V5,VA1,V6,VALUE6,VA1WS1,VA1WS2,VA1WS3,VA1WS4
     2    ,VALWS1,VALWS2,VALWS3,VALWS4,REPEAT)
C
          IF(REPEAT.NE.0) GO TO 300
          RETURN
      END
C SUB LQAS.FOR
      SUBROUTINE LQAS(VALUE1,VALUE2,VALUE3,VALUE4,VALUE5,VAL,
     1V1,V2,V3,V4,V5,VA1,V6,VALUE6,VA1WS1,VA1WS2,VA1WS3,VA1WS4
     2,VALWS1,VALWS2,VALWS3,VALWS4,REPEAT)
C
          IMPLICIT NONE
C
          CHARACTER*80 VAL,VALWS1,VALWS2,VALWS3,VALWS4
C
          INTEGER V1,V2,V3,V4,V5,V6,VA1,VA1WS1,VA1WS2,VA1WS3,VA1WS4
     1    ,REPEAT
C
          REAL*8 VALUE1,VALUE2,VALUE3,VALUE4,VALUE5,VALUE6
C
          INCLUDE 'datmai.inc'
C
          IF(VA1.NE.0) WRITE(OUTLYNE,10) VAL
          IF(VA1.NE.0) CALL SHOWIT(1)
          IF(VA1WS1.NE.0) WRITE(OUTLYNE,10) VALWS1
          IF(VA1WS1.NE.0) CALL SHOWIT(1)
          IF(VA1WS2.NE.0) WRITE(OUTLYNE,10) VALWS2
          IF(VA1WS2.NE.0) CALL SHOWIT(1)
          IF(VA1WS3.NE.0) WRITE(OUTLYNE,10)VALWS3
          IF(VA1WS3.NE.0) CALL SHOWIT(1)
          IF(VA1WS4.NE.0) WRITE(OUTLYNE,10) VALWS4
          IF(VA1WS4.NE.0) CALL SHOWIT(1)
          IF(V1.NE.0)  WRITE(OUTLYNE,20) VALUE1
          IF(V1.NE.0)  CALL SHOWIT(1)
          IF(V2.NE.0)  WRITE(OUTLYNE,21) VALUE2
          IF(V2.NE.0)  CALL SHOWIT(1)
          IF(V3.NE.0)  WRITE(OUTLYNE,22) VALUE3
          IF(V3.NE.0)  CALL SHOWIT(1)
          IF(V4.NE.0)  WRITE(OUTLYNE,23) VALUE4
          IF(V4.NE.0)  CALL SHOWIT(1)
          IF(V5.NE.0)  WRITE(OUTLYNE,24) VALUE5
          IF(V5.NE.0)  CALL SHOWIT(1)
          IF(V6.NE.0)  WRITE(OUTLYNE,25) VALUE6
          IF(V6.NE.0)  CALL SHOWIT(1)
          IF(V6.NE.0)  WRITE(OUTLYNE,26)
          IF(V6.NE.0)  CALL SHOWIT(1)
 10       FORMAT(A79)
 20       FORMAT('NUMERIC VALUE #1 = ',D23.15)
 21       FORMAT('NUMERIC VALUE #2 = ',D23.15)
 22       FORMAT('NUMERIC VALUE #3 = ',D23.15)
 23       FORMAT('NUMERIC VALUE #4 = ',D23.15)
 24       FORMAT('NUMERIC VALUE #5 = ',D23.15)
 25       FORMAT('NUMERIC VALUE #6 = ',D23.15)
 26       FORMAT(
     1    'NOTE: VALUE #6 FOR "CLAP" OR "COBS" IS THE "TILT" VALUE')
          IF(REPEAT.NE.0) REPEAT=REPEAT-1
          RETURN
      END
C SUB MGADJ.FOR
      SUBROUTINE MGADJ(MAG,I,J)
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE MGADJ WHICH IMPLEMENTS THE MAGX
C       AND MAGY ADJUSTMEMT.
C
          REAL*8 MAG,OBJ,IMG,EFLX,EFLY,ARG
C
          INTEGER I,J,KK
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
C
          IF(WC.EQ.'MAGX') THEN
              DO KK=I,J-1
                  IF(ALENS(33,KK).NE.0.0D0) THEN
                      OUTLYNE='ALL SOLVES FROM'
                      CALL SHOWIT(1)
                      OUTLYNE='SURFACE (I) TO (J-1) MUST BE REMOVED'
                      CALL SHOWIT(1)
                      OUTLYNE='MAGNIFICATION ADJUSTMENT NOT PERFORMED'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END DO
              SAVE_KDP(1)=SAVEINPT(1)
              W1=DBLE(I+1)
              W2=DBLE(J-1)
              S3=0
              S4=0
              S5=0
              DF1=0
              DF2=0
              DF3=1
              DF4=1
              DF5=1
              SN=1
              SST=0
              STI=0
              SQ=1
              WC='FIRD'
              WQ='QUIET'
              CALL FIRD
              REST_KDP(1)=RESTINPT(1)
C     NOW ATTEMPT THE MAG ADJUSTMENT
C     NOW SET DISTANCES
              EFLX=GPREG(2)
              IF(EFLX.EQ.0.0D0) THEN
                  OUTLYNE='EFLX IS ZERO'
                  CALL SHOWIT(1)
                  OUTLYNE='MAGNIFICATION ADJUSTMENT NOT PERFORMED'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              OBJ=((MAG+1.0D0)*EFLX)/MAG
              IF(OBJ.EQ.0.0D0) THEN
                  OUTLYNE='OBJECT DISTANCE IS ZERO'
                  CALL SHOWIT(1)
                  OUTLYNE='MAGNIFICATION ADJUSTMENT NOT PERFORMED'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              ARG=((1.0D0/EFLX)-(1.0D0/OBJ))
              IF(ARG.EQ.0.0D0) THEN
                  OUTLYNE='IMAGE DISTANCE DISTANCE IS INFINITE'
                  CALL SHOWIT(1)
                  OUTLYNE='MAGNIFICATION ADJUSTMENT NOT PERFORMED'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IMG=1.0D0/ARG
              ALENS(3,I)=OBJ-GPREG(9)
              ALENS(3,J-1)=IMG+GPREG(10)
              F1=0
              F6=1
              F22=0
              LNSTYP=1
              CALL LNSEOS
              WRITE(OUTLYNE,110) F12
              CALL SHOWIT(0)
              WRITE(OUTLYNE,201)I,J-1
              CALL SHOWIT(0)
              WRITE(OUTLYNE,202)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,200)I,J,MAG
              CALL SHOWIT(0)
              RETURN
          ELSE
C       WC NOT MAGX
          END IF
C
          IF(WC.EQ.'MAGY') THEN
C       CHECK FOR EXTRA SOLVES
              DO KK=I,J-1
                  IF(ALENS(33,KK).NE.0.0D0) THEN
                      OUTLYNE='ALL SOLVES FROM'
                      CALL SHOWIT(1)
                      OUTLYNE='SURFACE (I) TO (J-1) MUST BE REMOVED'
                      CALL SHOWIT(1)
                      OUTLYNE='MAGNIFICATION ADJUSTMENT NOT PERFORMED'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END DO
              SAVE_KDP(1)=SAVEINPT(1)
              W1=DBLE(I+1)
              W2=DBLE(J-1)
              S3=0
              S4=0
              S5=0
              DF1=0
              DF2=0
              DF3=1
              DF4=1
              DF5=1
              SN=1
              SST=0
              STI=0
              SQ=1
              WC='FIRD'
              WQ='QUIET'
              CALL FIRD
              REST_KDP(1)=RESTINPT(1)
C     NOW ATTEMPT THE MAG ADJUSTMENT
C     NOW SET DISTANCES
              EFLY=GPREG(1)
              IF(EFLY.EQ.0.0D0) THEN
                  OUTLYNE='EFLY IS ZERO'
                  CALL SHOWIT(1)
                  OUTLYNE='MAGNIFICATION ADJUSTMENT NOT PERFORMED'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              OBJ=((MAG+1.0D0)*EFLY)/MAG
              IF(OBJ.EQ.0.0D0) THEN
                  OUTLYNE='OBJECT DISTANCE IS ZERO'
                  CALL SHOWIT(1)
                  OUTLYNE='MAGNIFICATION ADJUSTMENT NOT PERFORMED'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              ARG=((1.0D0/EFLY)-(1.0D0/OBJ))
              IF(ARG.EQ.0.0D0) THEN
                  OUTLYNE='IMAGE DISTANCE DISTANCE IS INFINITE'
                  CALL SHOWIT(1)
                  OUTLYNE='MAGNIFICATION ADJUSTMENT NOT PERFORMED'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IMG=1.0D0/ARG
              ALENS(3,I)=OBJ-GPREG(7)
              ALENS(3,J-1)=IMG+GPREG(8)
              F1=0
              F6=1
              F22=0
              LNSTYP=1
              CALL LNSEOS
              WRITE(OUTLYNE,110) F12
              CALL SHOWIT(0)
              WRITE(OUTLYNE,201)I,J-1
              CALL SHOWIT(0)
              WRITE(OUTLYNE,202)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,400)I,J,MAG
              CALL SHOWIT(0)
              RETURN
          ELSE
C       NOT MAGY
          END IF
 110      FORMAT('FOR CURRENT CONFIGURATION # ',I2)
 201      FORMAT('AXIAL THICKNESSES OF SURFACES ',I3,' AND ',I3)
 202      FORMAT('HAVE BEEN RESET SO THAT THE TRANSVERSE MAGNIFICTION')
 200      FORMAT('IN THE XZ-PLANE FROM SURFACE '
     1    ,I3,' TO SURFACE ',I3, ' = ',G11.3,'X')
 400      FORMAT('IN THE YZ-PLANE FROM SURFACE '
     1    ,I3,' TO SURFACE ',I3, ' = ',G11.3,'X')
          RETURN
      END
      SUBROUTINE ROO
          IMPLICIT NONE
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          IF(STI.EQ.1) THEN
              OUTLYNE=
     1        '"ROO" SETS UP A SINGLE SURFACE OPTICAL ROOF'
              CALL SHOWIT(1)
              RETURN
          END IF
          IF(SST.EQ.1.OR.SQ.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
              OUTLYNE=
     1        '"ROO" TAKES NO QUALIFIER, STRING OR NUMERIC WORD #3 TO #5 INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(W1.LE.0.0D0.OR.S1.EQ.0) THEN
              OUTLYNE=
     1        '"ROO" REQUIRES AN EXPLICIT, POSITIVE APEX HEIGHT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(DF2.EQ.1) W2=0.0D0
          IF(DF3.EQ.1) W3=0.0D0
          ALENS(138,SURF) =W1
          ALENS(139,SURF) =W2
          ALENS(126,SURF) =1.0D0
          RETURN
      END
      SUBROUTINE CCR
          IMPLICIT NONE
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          IF(STI.EQ.1) THEN
              OUTLYNE=
     1        '"CCR" SETS UP A SINGLE SURFACE CORNER CUBE'
              CALL SHOWIT(1)
              RETURN
          END IF
          IF(SST.EQ.1.OR.SQ.EQ.1) THEN
              OUTLYNE=
     1        '"CCR" TAKES NO QUALIFIER OR STRING INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(S5.EQ.1) THEN
              OUTLYNE=
     1        '"CCR" TAKES NO NUMERIC WORK #5 INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(W1.LE.0.0D0.OR.S1.EQ.0) THEN
              OUTLYNE=
     1        '"CCR" REQUIRES AN EXPLICIT, POSITIVE APEX HEIGHT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(DF2.EQ.1) W2=0.0D0
          IF(DF3.EQ.1) W3=0.0D0
          IF(DF4.EQ.1) W4=0.0D0
          ALENS(138,SURF) =W1
          ALENS(139,SURF) =W1
          ALENS(140,SURF) =W2
          ALENS(141,SURF) =W4
          ALENS(126,SURF) =2.0D0
          RETURN
      END
      SUBROUTINE SHOWNSS
C     THIS DISPLAYS ROOF AND CCR DATA IN THE SEQUENTIAL DATABASE.
          IMPLICIT NONE
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          LOGICAL NONSS
          INTEGER I
          IF(STI.EQ.1) THEN
              OUTLYNE=
     1        '"SHOWNSS" LISTS ALL ROOFS AND CORNER CUBES IN'
              CALL SHOWIT(1)
              OUTLYNE=
     1        'THE CURRENT LENS CONFIGURATION'
              CALL SHOWIT(1)
              RETURN
          END IF
          IF(SST.EQ.1.OR.SQ.EQ.1.OR.SN.EQ.1) THEN
              OUTLYNE=
     1        '"NSSRTG" TAKES NO NUMERIC, QUALIFIER OR STRING INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          NONSS=.TRUE.
          DO I=0,INT(SYSTEM1(20))
              IF(ALENS(126,I).NE.0D0) NONSS=.FALSE.
          END DO
          IF(NONSS) THEN
              OUTLYNE='NO ROOF OR CCR DATA EXISTS FOR'
              CALL SHOWIT(1)
              OUTLYNE='THE CURRENT LENS CONFIGURATION'
              CALL SHOWIT(1)
              RETURN
          END IF
          OUTLYNE='ROOF/CCR DATA FOR THE CURRENT LENS CONFIGURATION'
          CALL SHOWIT(0)
          OUTLYNE='                 '
          CALL SHOWIT(0)
          DO I=0,INT(SYSTEM1(20))
              IF(ALENS(126,I).EQ.1.0D0) THEN
                  WRITE(OUTLYNE,*)'ROOF/CCR DATA FOR SURFACE NUMBER: ',I
                  CALL SHOWIT(0)
C     NSS DATA EXISTS FOR SURFACE I, LIST IT
C
C     ROO
                  IF(ALENS(126,I).EQ.1.0D0) THEN
                      OUTLYNE='SURFACE IS DEFINED AS "ROO"'
                      CALL SHOWIT(0)
                      WRITE(OUTLYNE,*) ' ROO APEX DISTANCE = ',ALENS(138,I)
                      CALL SHOWIT(0)
                      WRITE(OUTLYNE,*) ' ROO ANGLE ERROR   = ',ALENS(139,I),
     1                ' ARCSEC'
                      CALL SHOWIT(0)
                  END IF
C
C     CCR
                  IF(ALENS(126,I).EQ.2.0D0) THEN
                      OUTLYNE='SURFACE IS DEFINED AS "CCR"'
                      CALL SHOWIT(0)
                      WRITE(OUTLYNE,*) '   CCR APEX DISTANCE = ',ALENS(138,I)
                      CALL SHOWIT(0)
                      WRITE(OUTLYNE,*) 'CCR ANGLE ERROR #1   = ',ALENS(139,I),
     1                ' ARCSEC'
                      CALL SHOWIT(0)
                      WRITE(OUTLYNE,*) 'CCR ANGLE ERROR #1   = ',ALENS(140,I),
     1                ' ARCSEC'
                      CALL SHOWIT(0)
                      WRITE(OUTLYNE,*) 'CCR ANGLE ERROR #1   = ',ALENS(141,I),
     1                ' ARCSEC'
                      CALL SHOWIT(0)
                  END IF
              END IF
          END DO
          RETURN
      END
C SUB DUMOUT.FOR
      SUBROUTINE DUMOUT
C
          IMPLICIT NONE
C
          INTEGER I
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
C
C               CHECK FOR ADDITIONAL INPUT AND
C               PRINT ERROR AND RETURN IF DISCOVERED.
C
          IF(SST.EQ.1) THEN
              OUTLYNE='AT THE CMD LEVEL, "DUMOUT" TAKES NO STRING INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SQ.EQ.1.AND.S1.EQ.1) THEN
              OUTLYNE=
     1        'AT THE CMD LEVEL, "DUMOUT" TAKES EITHER QUALIFIER OR'
              CALL SHOWIT(1)
              OUTLYNE='NUMERIC WORD #1 INPUT BUT NOT BOTH'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
              OUTLYNE=
     1        'AT THE CMD LEVEL,'
              CALL SHOWIT(1)
              OUTLYNE=
     1        '"DUMOUT" TAKES NO NUMERIC WORD #2 THROUGH #5 INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C       WHAT IF NO SURFACES EXIST
          IF(SYSTEM1(20).EQ.0.0D0) THEN
              OUTLYNE='NO NODUM DATA EXISTS'
              CALL SHOWIT(1)
              OUTLYNE='LENS SYSTEM HAS NO SURFACES'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
C       W1 DESIGNATES THE SURFACE FOR WHICH THE NORMAL
C       DATA IS TO BE OUTPUT.
C       IF THE QUALIFIER "ALL" IS USED, THEN THE NORMAL DATA FOR
C       THE ENTIRE LENS IS PRINTED.
C
C       PRINT OUT FOR AN INDIVIDUAL SURFACE
C
          IF(WQ.EQ.'OB'.OR.WQ.EQ.'OBJ') THEN
C       THIS IS THE SAME AS NORMAL,0
              SQ=0
              WQ='        '
              S1=1
              W1=0.0D0
              DF1=0
          ELSE
C       NOT "OB" OR "OBJ"
          END IF
C
          IF(SQ.EQ.0) THEN
              IF(W1.LT.0.0D0) W1=SYSTEM1(20)+W1
              IF(DF1.EQ.1) W1=DBLE(INT(SYSTEM1(20)))
              I=INT(W1)
              IF(I.GT.INT(SYSTEM1(20)).OR.I.LT.0) THEN
                  OUTLYNE='SURFACE NUMBER BEYOND LEGAL RANGE'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(HEADIN) WRITE(OUTLYNE,500)
              IF(HEADIN) CALL SHOWIT(0)
              IF(ALENS(68,I).EQ.0.0D0) WRITE(OUTLYNE,100)I,'NO/OFF '
              IF(ALENS(68,I).EQ.1.0D0) WRITE(OUTLYNE,100)I,'YES/ON '
              CALL SHOWIT(0)
              CALL SHOWIT(0)
              RETURN
          ELSE
C       THERE WAS A QUALIFIER.
              IF(WQ.NE.'ALL') THEN
                  OUTLYNE='INVALID QUALIFIER WORD'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C
C       CHECK FOR NO DATA
C
C       PRINT HEADER MESSAGE
              WRITE(OUTLYNE,400)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,401)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,500)
              CALL SHOWIT(0)
              DO I=0,INT(SYSTEM1(20))
                  IF(ALENS(68,I).EQ.0.0D0) WRITE(OUTLYNE,100)I,'NO/OFF '
                  IF(ALENS(68,I).EQ.1.0D0) WRITE(OUTLYNE,100)I,'YES/ON '
                  CALL SHOWIT(0)
              END DO
C
              RETURN
          END IF
 400      FORMAT('SURFACE (FORCED DUMMY) DATA')
 401      FORMAT(1X)
 500      FORMAT('SURF',5X,'FORCED DUMMY SETTING')
 100      FORMAT(I3,11X,A8)
      END
C SUB BLKOUT.FOR
      SUBROUTINE BLKOUT
C
          IMPLICIT NONE
C
          INTEGER I
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
C
C               CHECK FOR ADDITIONAL INPUT AND
C               PRINT ERROR AND RETURN IF DISCOVERED.
C
          IF(SST.EQ.1) THEN
              OUTLYNE='AT THE CMD LEVEL, "FOOTBLOK" TAKES NO STRING INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SQ.EQ.1.AND.S1.EQ.1) THEN
              OUTLYNE=
     1        'AT THE CMD LEVEL, "FOOTBLOK" TAKES EITHER QUALIFIER OR'
              CALL SHOWIT(1)
              OUTLYNE='NUMERIC WORD #1 INPUT BUT NOT BOTH'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
              OUTLYNE=
     1        'AT THE CMD LEVEL,'
              CALL SHOWIT(1)
              OUTLYNE=
     1        '"FOOTBLOK" TAKES NO NUMERIC WORD #2 THROUGH #5 INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C       WHAT IF NO SURFACES EXIST
          IF(SYSTEM1(20).EQ.0.0D0) THEN
              OUTLYNE='NO FOOTBLOK DATA EXISTS'
              CALL SHOWIT(1)
              OUTLYNE='LENS SYSTEM HAS NO SURFACES'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
C       W1 DESIGNATES THE SURFACE FOR WHICH THE FOOTBLOK
C       DATA IS TO BE OUTPUT.
C       IF THE QUALIFIER "ALL" IS USED, THEN THE FOOTBLOK DATA FOR
C       THE ENTIRE LENS IS PRINTED.
C
C       PRINT OUT FOR AN INDIVIDUAL SURFACE
C
          IF(WQ.EQ.'OB'.OR.WQ.EQ.'OBJ') THEN
C       THIS IS THE SAME AS FOOTBLOK,0
              SQ=0
              WQ='        '
              S1=1
              W1=0.0D0
              DF1=0
          ELSE
C       NOT "OB" OR "OBJ"
          END IF
C
          IF(SQ.EQ.0) THEN
              IF(W1.LT.0.0D0) W1=SYSTEM1(20)+W1
              IF(DF1.EQ.1) W1=DBLE(INT(SYSTEM1(20)))
              I=INT(W1)
              IF(I.GT.INT(SYSTEM1(20)).OR.I.LT.0) THEN
                  OUTLYNE='SURFACE NUMBER BEYOND LEGAL RANGE'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(HEADIN) WRITE(OUTLYNE,500)
              CALL SHOWIT(0)
              IF(ALENS(58,I).EQ.0.0D0) WRITE(OUTLYNE,100)I,'OFF     '
              IF(ALENS(58,I).EQ.1.0D0) WRITE(OUTLYNE,100)I,'ON      '
              CALL SHOWIT(0)
              RETURN
          ELSE
C       THERE WAS A QUALIFIER.
              IF(WQ.NE.'ALL') THEN
                  OUTLYNE='INVALID QUALIFIER WORD'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C
C       CHECK FOR NO DATA
C
C       PRINT HEADER MESSAGE
              WRITE(OUTLYNE,400)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,401)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,500)
              CALL SHOWIT(0)
              DO I=0,INT(SYSTEM1(20))
                  IF(ALENS(58,I).EQ.0.0D0) WRITE(OUTLYNE,100)I,'OFF     '
                  IF(ALENS(58,I).EQ.1.0D0) WRITE(OUTLYNE,100)I,'ON      '
                  CALL SHOWIT(0)
              END DO
C
              RETURN
          END IF
 400      FORMAT('SURFACE FOOTBLOK DATA')
 401      FORMAT(1X)
 500      FORMAT('SURF',5X,'FOOTBLOK DEFINITION')
 100      FORMAT(I3,11X,A8)
      END
C SUB PIVAXOUT.FOR
      SUBROUTINE PIVAXOUT
C
          IMPLICIT NONE
C
          INTEGER I
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
C      PIVAXIS AT THE COMMAND LEVEL
C
C               CHECK FOR ADDITIONAL INPUT AND
C               PRINT ERROR AND RETURN IF DISCOVERED.
C
          IF(SST.EQ.1) THEN
              OUTLYNE='AT THE CMD LEVEL, "PIVAXIS" TAKES NO STRING INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SQ.EQ.1.AND.S1.EQ.1) THEN
              OUTLYNE=
     1        'AT THE CMD LEVEL, "PIVAXIS" TAKES EITHER QUALIFIER OR'
              CALL SHOWIT(1)
              OUTLYNE='NUMERIC WORD #1 INPUT BUT NOT BOTH'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
              OUTLYNE=
     1        'AT THE CMD LEVEL,'
              CALL SHOWIT(1)
              OUTLYNE=
     1        '"PIVAXIS" TAKES NO NUMERIC WORD #2 THROUGH #5 INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C       WHAT IF NO SURFACES EXIST
          IF(SYSTEM1(20).EQ.0.0D0) THEN
              OUTLYNE='NO PIVAXIS DATA EXISTS'
              CALL SHOWIT(1)
              OUTLYNE='LENS SYSTEM HAS NO SURFACES'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
C       W1 DESIGNATES THE SURFACE FOR WHICH THE PIVAXIS
C       DATA IS TO BE OUTPUT.
C       IF THE QUALIFIER "ALL" IS USED, THEN THE PIVAXIS DATA FOR
C       THE ENTIRE LENS IS PRINTED.
C
C       PRINT OUT FOR AN INDIVIDUAL SURFACE
C
          IF(WQ.EQ.'OB'.OR.WQ.EQ.'OBJ') THEN
C       THIS IS THE SAME AS PIVAXIS,0
              SQ=0
              WQ='        '
              S1=1
              W1=0.0D0
              DF1=0
          ELSE
C       NOT "OB" OR "OBJ"
          END IF
C
          IF(SQ.EQ.0) THEN
              IF(W1.LT.0.0D0) W1=SYSTEM1(20)+W1
              IF(DF1.EQ.1) W1=DBLE(INT(SYSTEM1(20)))
              I=INT(W1)
              IF(I.GT.INT(SYSTEM1(20)).OR.I.LT.0) THEN
                  OUTLYNE='SURFACE NUMBER BEYOND LEGAL RANGE'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(HEADIN) WRITE(OUTLYNE,500)
              CALL SHOWIT(0)
              IF(ALENS(113,I).EQ.0.0D0) WRITE(OUTLYNE,100)I,'VERTEX  '
              IF(ALENS(113,I).EQ.1.0D0) WRITE(OUTLYNE,100)I,'NORMAL  '
              CALL SHOWIT(0)
              RETURN
          ELSE
C       THERE WAS A QUALIFIER.
              IF(WQ.NE.'ALL') THEN
                  OUTLYNE='INVALID QUALIFIER WORD'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C
C       CHECK FOR NO DATA
C
C       PRINT HEADER MESSAGE
              WRITE(OUTLYNE,400)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,401)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,500)
              CALL SHOWIT(0)
              DO I=0,INT(SYSTEM1(20))
                  IF(ALENS(113,I).EQ.0.0D0) WRITE(OUTLYNE,100)I,'VERTEX  '
                  IF(ALENS(113,I).EQ.1.0D0) WRITE(OUTLYNE,100)I,'NORMAL  '
                  CALL SHOWIT(0)
              END DO
C
              RETURN
          END IF
 400      FORMAT('SURFACE PIVAXIS DATA')
 401      FORMAT(1X)
 500      FORMAT('SURF',5X,'PIVAXIS DEFINITION')
 100      FORMAT(I3,11X,A8)
      END
C SUB SURFTYPE.FOR
      SUBROUTINE SURFTYPE
C
          IMPLICIT NONE
C
          INTEGER I
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C     DOES THE SURTYPE COMMAND
C
C
C               CHECK FOR ADDITIONAL INPUT AND
C               PRINT ERROR AND RETURN IF DISCOVERED.
C
          IF(SST.EQ.1) THEN
              OUTLYNE='AT THE CMD LEVEL, "SURTYPE" TAKES NO STRING INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SQ.EQ.1.AND.S1.EQ.1) THEN
              OUTLYNE=
     1        'AT THE CMD LEVEL, "SURTYPE" TAKES EITHER QUALIFIER OR'
              CALL SHOWIT(1)
              OUTLYNE='NUMERIC WORD #1 INPUT BUT NOT BOTH'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
              OUTLYNE=
     1        'AT THE CMD LEVEL,'
              CALL SHOWIT(1)
              OUTLYNE=
     1        '"SURTYPE" TAKES NO NUMERIC WORD #2 THROUGH #5 INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C       WHAT IF NO SURFACES EXIST
          IF(SYSTEM1(20).EQ.0.0D0) THEN
              OUTLYNE='NO SURFACE TYPE DATA EXISTS'
              CALL SHOWIT(1)
              OUTLYNE='LENS SYSTEM HAS NO SURFACES'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
C       W1 DESIGNATES THE SURFACE FOR WHICH THE SURFACE TYPE
C       DATA IS TO BE OUTPUT.
C       IF THE QUALIFIER "ALL" IS USED, THEN THE SURFACE DATA FOR
C       THE ENTIRE LENS IS PRINTED.
C
C       PRINT OUT FOR AN INDIVIDUAL SURFACE
C
          IF(WQ.EQ.'OB'.OR.WQ.EQ.'OBJ') THEN
C       THIS IS THE SAME AS SURTYPE,0
              SQ=0
              WQ='        '
              S1=1
              W1=0.0D0
              DF1=0
          ELSE
C       NOT "OB" OR "OBJ"
          END IF
C
          IF(SQ.EQ.0) THEN
              IF(W1.LT.0.0D0) W1=SYSTEM1(20)+W1
              IF(DF1.EQ.1) W1=DBLE(INT(SYSTEM1(20)))
              I=INT(W1)
              IF(I.GT.INT(SYSTEM1(20)).OR.I.LT.0) THEN
                  OUTLYNE='SURFACE NUMBER BEYOND LEGAL RANGE'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(HEADIN) WRITE(OUTLYNE,500)
              CALL SHOWIT(0)
              IF(ALENS(124,I).EQ.0.0D0) WRITE(OUTLYNE,100)I,'REAL    '
              IF(ALENS(124,I).EQ.1.0D0) WRITE(OUTLYNE,100)I,'PARAXIAL'
              CALL SHOWIT(0)
              RETURN
          ELSE
C       THERE WAS A QUALIFIER.
              IF(WQ.NE.'ALL') THEN
                  OUTLYNE='INVALID QUALIFIER WORD'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C
C       CHECK FOR NO DATA
C
C       PRINT HEADER MESSAGE
              WRITE(OUTLYNE,400)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,401)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,500)
              CALL SHOWIT(0)
              DO I=0,INT(SYSTEM1(20))
                  IF(ALENS(124,I).EQ.0.0D0) WRITE(OUTLYNE,100)I,'REAL    '
                  IF(ALENS(124,I).EQ.1.0D0) WRITE(OUTLYNE,100)I,'PARAXIAL'
                  CALL SHOWIT(0)
              END DO
C
              RETURN
          END IF
 400      FORMAT('SURFACE TYPE DATA')
 401      FORMAT(1X)
 500      FORMAT('SURF',5X,'  SURFACE TYPE')
 100      FORMAT(I3,11X,A8)
      END
C SUB SGLASS.FOR
      SUBROUTINE SGLASS
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE SGLASS WHICH IMPLEMENTS THE GLASS
C       AND MODEL COMMAND AT THE LENS OF UPDATE LENS LEVEL.
C
          INTEGER PIKCNT,I
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
C
C       "GLASS" IS A SURFACE TERMINAL COMMAND. IN THE LENS INPUT
C       ROUTINE F5=1, "GLASS" CAUSES THE SURFACE COUNTING VARIABLE
C       (SURF) AND SYSTEM1(20) TO BE INCREMENTED BY 1 AND 1.0
C       AND CAUSES THE NEXT SURFACE DATA TO BE STORED FOR THE NEXT
C       SURFACE. AT F6=1 (UPDATE ROUTINE) THIS INCREMENTING DOES
C       NO OCCUR. THE LAST SURFACE FOR WHICH DATA MAY BE ENTERED
C       IS SURFACE MAXSUR. SURF AND SYSTEM1(20) ARE NOT INCREMENTED
C       PASSED MAXSUR SO IF AN ATTEMPT IS MADE TO INPUT PASSED
C       SURFACE MAXSUR IN THE LENS INPUT MODE, THE DATA FOR SURFACE
C       MAXSUR WILL BE OVERWRITTEN.
C
C       "MODEL" PUTS IN A CODE-V LIKE FICTICIOUS GLASS WHICH HAS THE
C       INDEX RESOLVED IN THE LNSEOS SUBROUTINE
C
C               CHECK FOR STRING INPUT
C               PRINT ERROR AND RETURN IF DISCOVERED.
C
          SST=0
          IF(SST.EQ.1) THEN
              IF(WC.EQ.'GLASS') OUTLYNE='"GLASS" TAKES NO STRING INPUT'
              IF(WC.EQ.'MODEL') OUTLYNE='"MODEL" TAKES NO STRING INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SQ.EQ.0) THEN
              IF(WC.EQ.'GLASS')
     1        OUTLYNE='"GLASS" REQUIRES EXPLICIT QUALIFIER INPUT'
              IF(WC.EQ.'MODEL')
     1        OUTLYNE='"MODEL" REQUIRES EXPLICIT QUALIFIER INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
C               WE ARE AT LENS INPUT OR LENS UPDATE LEVEL
C
C       DEFAULT INDICES ALWAYS GO TO 1.0 NOT TO 0.0
C
C
          IF(WC.EQ.'MODEL'.AND.S4.EQ.1.OR.WC.EQ.'MODEL'.AND.S5.EQ.1) THEN
              OUTLYNE='"MODEL" TAKES NO NUMERIC WORD #3 THROUGH #5 INPUT'
              CALL SHOWIT(0)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(0)
              CALL MACFAL
              RETURN
          END IF
          IF(WC.NE.'MODEL') THEN
              IF(DF1.EQ.1) W1=1.0D0
              IF(DF2.EQ.1) W2=1.0D0
              IF(DF3.EQ.1) W3=1.0D0
              IF(DF4.EQ.1) W4=1.0D0
              IF(DF5.EQ.1) W5=1.0D0
          END IF
          IF(WC.EQ.'MODEL') THEN
              IF(DF1.EQ.1) W1=1.0D0
              IF(DF2.EQ.1) W2=0.0D0
              IF(DF3.EQ.1) W3=0.0D0
          END IF
          IF(WC.EQ.'GLASS') THEN
              IF(W1.EQ.0.0D0.OR.W2.EQ.0.0D0.OR.W3.EQ.0.0D0.OR.W4.EQ.0.0D0
     1        .OR.W5.EQ.0.0D0) THEN
                  OUTLYNE=
     1            '"GLASS" TAKES NO ZERO VALUES FOR REFRACTIVE INDICES'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
          IF(WC.NE.'MODEL') THEN
              IF(W1.EQ.0.0D0) THEN
                  OUTLYNE=
     1            '"MODEL" TAKES NO ZERO VALUES FOR THE "INDEX" VALUE'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              ALENS(46,SURF)=W1
              ALENS(47,SURF)=W2
              ALENS(48,SURF)=W3
              ALENS(49,SURF)=W4
              ALENS(50,SURF)=W5
              GLANAM(SURF,1)='GLASS      '
              GLANAM(SURF,2)= WQ//'     '
          END IF
          IF(WC.EQ.'MODEL') THEN
              IF(W1.LE.0.0D0) THEN
                  OUTLYNE=
     1            '"MODEL" REQUIRES A NON-ZERO, POSITIVE INPUT INDEX VALUE'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              ALENS(86,SURF)=W1
              ALENS(87,SURF)=W2
              ALENS(89,SURF)=W3
              GLANAM(SURF,1)='MODEL        '
              GLANAM(SURF,2)= WQ//'     '
          END IF
C
C       NOW HANDEL EXISTING PIKUPS OF GLASS TYPE, I E DELETE THEM.
C       CHECK FOR CC PIKUPS AND DELETE IF FOUND
C
C
          IF(PIKUP(1,SURF,20).EQ.0.0) THEN
C
C       NO GLASS PIKUPS, JUST RETURN
C
              IF(F5.EQ.1.AND.SURF.LT.MAXSUR) THEN
                  SURF=SURF+1
                  ALENS(1:LSIZ,SURF)=0.0D0
                  SYSTEM1(20)=DBLE(SURF)
              ELSE
C       DON'T INCREMENT SURF AND SYSTEM1(20)
              END IF
              F22=1
              RETURN
          ELSE
          END IF
C
C       DELETE THE PIKUP
          PIKUP(1:6,SURF,20)=0.0
          ALENS(32,SURF)=ALENS(32,SURF)-1.0
C
C
C       PRINT MESSAGE
          WRITE(OUTLYNE,*)'SURFACE',SURF,' :PIKUP (GLASS) DELETED'
          CALL SHOWIT(1)
C
          PIKCNT=0
          DO 10 I=1,PSIZ
              IF(PIKUP(1,SURF,I).NE.0.0) THEN
                  PIKCNT=PIKCNT+1
              ELSE
              END IF
 10       CONTINUE
C
          IF(PIKCNT.EQ.0.0) ALENS(32,SURF)=0.0
          IF(F5.EQ.1.AND.SURF.LT.MAXSUR) THEN
              SURF=SURF+1
              ALENS(1:LSIZ,SURF)=0.0D0
              SYSTEM1(20)=DBLE(SURF)
          ELSE
C       DON'T INCREMENT SURF AND SYSTEM1(20)
          END IF
C
          F22=1
          RETURN
      END
      SUBROUTINE RAYERROR
          IMPLICIT NONE
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          IF(STI.EQ.1) THEN
              OUTLYNE=
     1        '"RAYERROR" SETS A RAYDOM RAY/SURFACE ANGLE ERROR'
              CALL SHOWIT(1)
              RETURN
          END IF
          IF(SST.EQ.1.OR.SQ.EQ.1) THEN
              OUTLYNE=
     1        '"RAYERROR" TAKES NO QUALIFIER OR STRING INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
              OUTLYNE=
     1        '"RAYERROR" TAKES NO NUMERIC WORD #2 THROUGH #5 INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          ALENS(144,SURF) =W1
          RETURN
      END
