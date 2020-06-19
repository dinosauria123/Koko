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

C       SECOND FILE OF PLOT/CAD ROUTINES

C ALL INTERACTER/WINTERACTER CALLS HAVE BEEN MOVED TO ISS.FOR
C
C SUB PLTRST1.FOR
      SUBROUTINE PLTRST1
C
          IMPLICIT NONE
C
C       THIS ROUTINE RESETS PLOT PARAMETERS TO STARTING VALES
C       AFTER PLOT DEV, PLOT SCREEN OR PLOT PRINTER
C
          INTEGER I,FANWAV
C
          INTEGER JK_WAV(1:10)
C
          REAL*8 VIEROT
C
          INTEGER VIEXOF,VIEYOF
C
          COMMON/OFFVIE/VIEXOF,VIEYOF,VIEROT
C
          COMMON/WAVER/JK_WAV
C
          COMMON/FANNER/FANWAV
C
          LOGICAL FANEXT,RIM
C
          COMMON/RINSHT/RIM
C
          COMMON/FANEXI/FANEXT
C
          CHARACTER FANNAM*8,FANQAL*8,DRWNAM*11
C
          COMMON/DRWTAG/DRWNAM
C
          COMMON/PASFAN/FANNAM,FANQAL
C
          INTEGER XTENT,YTENT,CLIP
C
          COMMON/USEFAN/XTENT,YTENT,CLIP
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          INCLUDE 'datspd.inc'
          INCLUDE 'dathgr.inc'
C
C     USER-DEFINED FAN PLOTTING SCALE FACTOR
C     DEFAULT MEANS PROGRAM CALCULATES AUTOSCALE
C       RESET 3D LINE VALUES
          LX1=0.0D0
          LY1=0.0D0
          LZ1=0.0D0
          LX2=0.0D0
          LY2=0.0D0
          LZ2=0.0D0
C       NORAYPLOT
          NORAYPLOT=.TRUE.
C
          FSSIFLG=.FALSE.
          FSSI=0.0D0
          FANAXX=5000
          FANAXY=3500
          XTENT=2500
          YTENT=2000
          CLIP=0
C
          CALL MY_DEL_NEUT

C     RESET PLOTNAME TO NEUTRAL.DAT
          DRWNAM='NEUTRAL.DAT'
          FIXUP=.FALSE.
          POLDX=0.0D0
          POLDY=0.0D0
          POLDZ=0.0D0
          PCURX=0.0D0
          PCURY=0.0D0
          PCURZ=0.0D0
          PNEWX=0.0D0
          PNEWY=0.0D0
          PNEWZ=0.0D0
C     DEFAULT PEN COLOR IS BLACK
          COLDEF=15
C
C     DEFAULT PLOT ORIGIN IS IN THE LOWER LEFT CORNER AT Y=0, X=0
C     (BOTH INTEGER)
          ORX=0
          ORY=0
C
C     DEFAULT PEN POSITIONS ARE 0 AND 0 BUT ARE NEVER USED
C     SINCE PENSTA BY DEFAULT IS 0 (BOTH INTEGER)
          XPEN=0
          YPEN=0
C
C     PLOTTING LOOK VECTOR
C         DRALOK=.FALSE.
C      LOKFLG=.FALSE.
C
C     PLOTTING VIG MESSAGE
          PLTVIG=.FALSE.
          VIGFLG=.FALSE.
C
C     PLOTTING VIEW ANGLES
          DRAVUE=.FALSE.
          VUEFLG=.FALSE.
C
C     DEFAULT OLD X-PEN POSITION IS 0 (INTEGER)
          XPENOL=0
C
C     DEFAULT OLD Y-PEN POSITION IS 0 (INTEGER)
          YPENOL=0
C
C     THE DEFAULT PEN STATUS IS 3, PEN STATUS NOT SET TO A USEFUL VALUE
          PENSTA=3
C
C     DEFAULT LINE TYPE IS 0, A SOLID LINE
          LNTYPE=0
          OLLNTP=0
C
C     DEFAULT SYMBOL TYPE IS 8, AN ASTERIX
          SYMB=8
C
C     DEFAULT CHARACTER SIZE FOR SYMBOL IS 1
          SYMSIZ=1
C
C     DEFAULT CHARACTER SIZE FOR LABLE IS 1
          LABSIZ=1
C
C     DEFAULT CHARACTER SIZE FOR NOTES IS 1
          NTSIZ=1
C
C     DEFAULT ANGLE FOR SYMBOL PLOTTING IS 0 DEGREES (INTEGER)
          SYMANG=0
C
C     DEFAULT ANGLE FOR LABLE PLOTTING IS 0 DEGREES (INTEGER)
          LABANG=0
C
C     DEFAULT ANGLE FOR NOTE PLOTTING IS 0 DEGREES (INTEGER)
          NTANG=0
C
C     DEFAULT Y-DIRECTION COSINE OF PLOT LOOK IS 0.0
C        LOOKY=0.0D0
C
C     DEFAULT X-DIRECTION COSINE OF PLOT LOOK IS -1.0
C        LOOKX=-1.0D0
C
C     DEFAULT Z-DIRECTION COSINE OF PLOT LOOK IS 0.0
C        LOOKZ=0.0D0
C
C     SET XSHIFT,YSHIFT AND ZSHIFT
          PXSHFT=0
          PYSHFT=0
          PGAMMA=0
          LORIENT=.FALSE.
C
C     DEFAULT "ALPHA" FOR PLOT VIEW IS 0.0 DEGREES
C       VIEALF=0.0D0
C
C     DEFAULT "PHI" FOR PLOT VIEW IS 270.0 DEGREES
C       VIEPHI=270.0D0
C
C     BY DEFAULT, GRAPHICS IS SET "ON"
          GRASET=.TRUE.
C
C     DON'T REST DUMMY SURFACES PLOTTED AS DASHED LINES. LEAVE AS SET
C
C     NOW THE FLAGS TO TRACK IF THINGS HAVE BEEN DONE ONCE
C
C     FLAG TO SEE IF "LB" HAS BEEN PLOTTED
C     BY DEFAULT SET TO FALSE
          LBLFLG=.FALSE.
          PLTLBL=.FALSE.
C
C     FLAG TO SEE IF "LI" HAS BEEN PLOTTED
C     BY DEFAULT SET TO FALSE
          LIFLG=.FALSE.
          PLTLLI=.FALSE.
C
C     FLAG TO SEE IF "AXIS" SHOULD BE PLOTTED
C     BY DEFAULT SET TO FALSE
          PLTAXS=.FALSE.
C
C     FLAG TO SEE IF "AXIS" HAS BEEN PLOTTED
C     BY DEFAULT SET TO FALSE
          AXFLG=.FALSE.
C
C     MAX AND MIN DEVICE INDEPENDENT COORDINATES
          XXMAX=10010
          XXMIN=-10
          YYMAX=7010
          YYMIN=-10
          XPEN=0
          YPEN=0
          XPENOL=0
          YPENOL=0
          PENSTA=3
C
C     FLAG TO SEE IF "LABLE" HAS BEEN PLOTTED
C     BY DEFAULT SET TO FALSE
          LBLFLG=.FALSE.
C
C     RIGHT,CENTER AND LEFT PLOT JUSTIFICATION FLAG FOR PLOTS
C     OF THE LENS DATA
C
C     RCL=1 MEANS LEFT JUSTIFY
C     RCL=2 MEANS CENTER JUSTIFY(DEFAULT VALUE)
C     RCL=3 MEANS RIGHT JUSTIFY
          RCL=-2
          JUSOFF=5000.0D0
C
          ROTSET=.FALSE.
          XROT=0.0D0
          YROT=0.0D0
          ZROT=0.0D0
C
C
C     PLOT SCALE STUFF
C
C     BY DEFAULT, AUTOSCALE SHOULD BE DONE
          AUTSL=.TRUE.
C
C     PLOT SCALE SHOULD OR SHOUD NOT BE DRAWN
          DRASCL=.FALSE.
C
C     PLOT SCALE IS NOT EXPLICITLY SET
          PLSC=.FALSE.
C
C     PLOT SCALE HAS NOT BEEN DRAWN YET
          SCFLG=.FALSE.
C
C     FLAG TO SEE IF AN AUTO-SCALE FACTOR HAS BEEN CALCULATED
C     BY DEFAULT SET TO FALSE
          ASCFLG=.FALSE.
C
          SCFAY=1.0D0
          SCFAX=1.0D0
          SCFAYP=1.0D0
          SCFAXP=1.0D0
C
C     PLOT SIZE STUFF
C
C     PLOT SIZE SHOULD OR SHOUD NOT BE DRAWN
          DRASZZ=.FALSE.
C
C     PLOT SIZE IS NOT EXPLICITLY SET
          PLSZ=.FALSE.
C
C     PLOT SIZE HAS NOT BEEN DRAWN YET
          SZFLG=.FALSE.
C
          RIM=.FALSE.
C
          IF(.NOT.FANEXT) THEN
C     RESET
C     FAN PLOTTING DEFAULT VALUES
              YFOB1=0.0D0
              YFOB2=0.0D0
              YFOB3=0.0D0
              XFOB1=0.0D0
              XFOB2=0.0D0
              XFOB3=0.0D0
              FANOFF=0.0D0
C     FAN WAVELENGTHS
C     RESTORE THE DEFAULT WAVELENGTH SETTINGS
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
              JK_WAV(1:10)=0
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
 900          CONTINUE
C
              FANNUM=1
              FANNM1=3
              FANNM2=3
              FANTYP=5
              QALTYP=0
              SSIFLG=.TRUE.
              REFWV=INT(SYSTEM1(11))
C     FAN PLOTTING DEFAULT VALUES HAVE BEEN SET
              FANNOB=0
              FANNRF=INT(SYSTEM1(25))
              FANNIM=INT(SYSTEM1(20))
              MAXFAN=20
              STEPJP=125.0D0
          ELSE
C     FANEXT=TRUE
          END IF
C     ADDED SO AS TO PLOT THE LAST FAN PRINTED USING
C     A CMD LEVEL FANS GENERATION COMMAND
          IF(FANEXT) THEN
              FANNUM=1
              FANNM1=3
              FANNM2=3
              YFOB1=LFOB(1)
              XFOB1=LFOB(2)
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
              IF(FANWAV.EQ.1) JK_WAV(1)=1
              IF(FANWAV.EQ.2) JK_WAV(1)=2
              IF(FANWAV.EQ.3) JK_WAV(1)=3
              IF(FANWAV.EQ.4) JK_WAV(1)=4
              IF(FANWAV.EQ.5) JK_WAV(1)=5
              IF(FANWAV.EQ.6) JK_WAV(1)=6
              IF(FANWAV.EQ.7) JK_WAV(1)=7
              IF(FANWAV.EQ.8) JK_WAV(1)=8
              IF(FANWAV.EQ.9) JK_WAV(1)=9
              IF(FANWAV.EQ.10) JK_WAV(1)=10
              JK_WAV(2:10)=0
              FANOFF=0.0D0
              SSI=0.0D0
              REFWV=INT(LFOB(4))
              IF(FANNAM(1:4).EQ.'YFAN') FANTYP=1
              IF(FANNAM(1:4).EQ.'XFAN') FANTYP=2
              IF(FANNAM(1:4).EQ.'NFAN') FANTYP=3
              IF(FANNAM(1:4).EQ.'PFAN') FANTYP=4
              IF(FANNAM(1:5).EQ.'XYFAN') FANTYP=5
              IF(FANNAM(1:5).EQ.'YXFAN') FANTYP=6
              IF(FANQAL(1:4).EQ.'    ') QALTYP=0
              IF(FANQAL(1:3).EQ.'OPD')  QALTYP=1
              IF(FANQAL(1:2).EQ.'CD')   QALTYP=2
              IF(FANQAL(1:2).EQ.'LA')   QALTYP=3
              FANNOB=0
              FANNRF=INT(SYSTEM1(25))
              FANNIM=INT(SYSTEM1(20))
              IF(LFOB(5).EQ.0.0D0) LFOB(5)=DBLE(NEWOBJ)
              IF(LFOB(6).EQ.0.0D0) LFOB(6)=DBLE(NEWREF)
              IF(LFOB(7).EQ.0.0D0) LFOB(7)=DBLE(NEWIMG)
              FANNOB=INT(LFOB(5))
              FANNRF=INT(LFOB(6))
              FANNIM=INT(LFOB(7))
              MAXFAN=20
              STEPJP=125.0D0
              SSIFLG=.TRUE.
C     FAN PLOTTING DEFAULT VALUES HAVE BEEN SET
              MAXFAN=20
              STEPJP=125.0D0

          ELSE
C     NO FAN WAS DONE
              FANEXT=.FALSE.
          END IF

          RETURN
      END
C       SECOND FILE OF PLOT/CAD ROUTINES

C ALL INTERACTER/WINTERACTER CALLS HAVE BEEN MOVED TO ISS.FOR
C
C SUB PLTRST.FOR
      SUBROUTINE PLTRST

          USE rgb
          IMPLICIT NONE
C
C       THIS ROUTINE RESETS PLOT PARAMETERS TO STARTING VALES
C       AFTER PLOT DEV, PLOT SCREEN OR PLOT PRINTER
C
          INTEGER I,FANWAV
C
          INTEGER JK_WAV(1:10)
C
          REAL*8 VIEROT
C
          INTEGER VIEXOF,VIEYOF
C
          COMMON/OFFVIE/VIEXOF,VIEYOF,VIEROT
C
          COMMON/WAVER/JK_WAV
C
          COMMON/FANNER/FANWAV
C
          LOGICAL FANEXT,RIM
C
          COMMON/RINSHT/RIM
C
          COMMON/FANEXI/FANEXT
C
          CHARACTER FANNAM*8,FANQAL*8,DRWNAM*11
C
          COMMON/DRWTAG/DRWNAM
C
          COMMON/PASFAN/FANNAM,FANQAL
C
          INTEGER XTENT,YTENT,CLIP
C
          COMMON/USEFAN/XTENT,YTENT,CLIP
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          INCLUDE 'datspd.inc'
          INCLUDE 'dathgr.inc'
C
C     USER-DEFINED FAN PLOTTING SCALE FACTOR
C     DEFAULT MEANS PROGRAM CALCULATES AUTOSCALE
C       RESET 3D LINE VALUES
          LX1=0.0D0
          LY1=0.0D0
          LZ1=0.0D0
          LX2=0.0D0
          LY2=0.0D0
          LZ2=0.0D0
C       NORAYPLOT
          NORAYPLOT=.TRUE.
C
          FSSIFLG=.FALSE.
          FSSI=0.0D0
          FANAXX=5000
          FANAXY=3500
          XTENT=2500
          YTENT=2000
          CLIP=0
C
          CALL MY_DEL_NEUT

C     RESET PLOTNAME TO NEUTRAL.DAT
          DRWNAM='NEUTRAL.DAT'
          FIXUP=.FALSE.
          POLDX=0.0D0
          POLDY=0.0D0
          POLDZ=0.0D0
          PCURX=0.0D0
          PCURY=0.0D0
          PCURZ=0.0D0
          PNEWX=0.0D0
          PNEWY=0.0D0
          PNEWZ=0.0D0
C     DEFAULT PEN COLOR IS BLACK
          CALL rgbint("black", COLDEF)
C
C     DEFAULT PLOT ORIGIN IS IN THE LOWER LEFT CORNER AT Y=0, X=0
C     (BOTH INTEGER)
          ORX=0
          ORY=0
C
C     DEFAULT PEN POSITIONS ARE 0 AND 0 BUT ARE NEVER USED
C     SINCE PENSTA BY DEFAULT IS 0 (BOTH INTEGER)
          XPEN=0
          YPEN=0
C
C     PLOTTING LOOK VECTOR
          DRALOK=.FALSE.
          LOKFLG=.FALSE.
C
C     PLOTTING VIG MESSAGE
          PLTVIG=.FALSE.
          VIGFLG=.FALSE.
C
C     PLOTTING VIEW ANGLES
          DRAVUE=.FALSE.
          VUEFLG=.FALSE.
C
C     DEFAULT OLD X-PEN POSITION IS 0 (INTEGER)
          XPENOL=0
C
C     DEFAULT OLD Y-PEN POSITION IS 0 (INTEGER)
          YPENOL=0
C
C     THE DEFAULT PEN STATUS IS 3, PEN STATUS NOT SET TO A USEFUL VALUE
          PENSTA=3
C
C     DEFAULT LINE TYPE IS 0, A SOLID LINE
          LNTYPE=0
          OLLNTP=0
C
C     DEFAULT SYMBOL TYPE IS 8, AN ASTERIX
          SYMB=8
C
C     DEFAULT CHARACTER SIZE FOR SYMBOL IS 1
          SYMSIZ=1
C
C     DEFAULT CHARACTER SIZE FOR LABLE IS 1
          LABSIZ=1
C
C     DEFAULT CHARACTER SIZE FOR NOTES IS 1
          NTSIZ=1
C
C     DEFAULT ANGLE FOR SYMBOL PLOTTING IS 0 DEGREES (INTEGER)
          SYMANG=0
C
C     DEFAULT ANGLE FOR LABLE PLOTTING IS 0 DEGREES (INTEGER)
          LABANG=0
C
C     DEFAULT ANGLE FOR NOTE PLOTTING IS 0 DEGREES (INTEGER)
          NTANG=0
C
C     DEFAULT Y-DIRECTION COSINE OF PLOT LOOK IS 0.0
          LOOKY=0.0D0
C
C     DEFAULT X-DIRECTION COSINE OF PLOT LOOK IS -1.0
          LOOKX=-1.0D0
C
C     DEFAULT Z-DIRECTION COSINE OF PLOT LOOK IS 0.0
          LOOKZ=0.0D0
C
C     SET XSHIFT,YSHIFT AND ZSHIFT
          PXSHFT=0
          PYSHFT=0
          PGAMMA=0
          LORIENT=.FALSE.
C
C     DEFAULT "ALPHA" FOR PLOT VIEW IS 0.0 DEGREES
          VIEALF=0.0D0
C
C     DEFAULT "PHI" FOR PLOT VIEW IS 270.0 DEGREES
          VIEPHI=270.0D0
C
C     BY DEFAULT, GRAPHICS IS SET "ON"
          GRASET=.TRUE.
C
C     DUMMY SURFACES PLOTTED AS DASHED LINES
          DASHH=.TRUE.
C
C     NOW THE FLAGS TO TRACK IF THINGS HAVE BEEN DONE ONCE
C
C     FLAG TO SEE IF "LB" HAS BEEN PLOTTED
C     BY DEFAULT SET TO FALSE
          LBLFLG=.FALSE.
          PLTLBL=.FALSE.
C
C     FLAG TO SEE IF "LI" HAS BEEN PLOTTED
C     BY DEFAULT SET TO FALSE
          LIFLG=.FALSE.
          PLTLLI=.FALSE.
C
C     FLAG TO SEE IF "AXIS" SHOULD BE PLOTTED
C     BY DEFAULT SET TO FALSE
          PLTAXS=.FALSE.
C
C     FLAG TO SEE IF "AXIS" HAS BEEN PLOTTED
C     BY DEFAULT SET TO FALSE
          AXFLG=.FALSE.
C
C     MAX AND MIN DEVICE INDEPENDENT COORDINATES
          XXMAX=10010
          XXMIN=-10
          YYMAX=7010
          YYMIN=-10
          XPEN=0
          YPEN=0
          XPENOL=0
          YPENOL=0
          PENSTA=3
C
C     FLAG TO SEE IF "LABLE" HAS BEEN PLOTTED
C     BY DEFAULT SET TO FALSE
          LBLFLG=.FALSE.
C
C     RIGHT,CENTER AND LEFT PLOT JUSTIFICATION FLAG FOR PLOTS
C     OF THE LENS DATA
C
C     RCL=1 MEANS LEFT JUSTIFY
C     RCL=2 MEANS CENTER JUSTIFY(DEFAULT VALUE)
C     RCL=3 MEANS RIGHT JUSTIFY
          RCL=-2
          JUSOFF=5000.0D0
C
          ROTSET=.FALSE.
          XROT=0.0D0
          YROT=0.0D0
          ZROT=0.0D0
C
C
C     PLOT SCALE STUFF
C
C     BY DEFAULT, AUTOSCALE SHOULD BE DONE
          AUTSL=.TRUE.
C
C     PLOT SCALE SHOULD OR SHOUD NOT BE DRAWN
          DRASCL=.FALSE.
C
C     PLOT SCALE IS NOT EXPLICITLY SET
          PLSC=.FALSE.
C
C     PLOT SCALE HAS NOT BEEN DRAWN YET
          SCFLG=.FALSE.
C
C     FLAG TO SEE IF AN AUTO-SCALE FACTOR HAS BEEN CALCULATED
C     BY DEFAULT SET TO FALSE
          ASCFLG=.FALSE.
C
          SCFAY=1.0D0
          SCFAX=1.0D0
          SCFAYP=1.0D0
          SCFAXP=1.0D0
C
C     PLOT SIZE STUFF
C
C     PLOT SIZE SHOULD OR SHOUD NOT BE DRAWN
          DRASZZ=.FALSE.
C
C     PLOT SIZE IS NOT EXPLICITLY SET
          PLSZ=.FALSE.
C
C     PLOT SIZE HAS NOT BEEN DRAWN YET
          SZFLG=.FALSE.
C
          RIM=.FALSE.
C
          IF(.NOT.FANEXT) THEN
C     RESET
C     FAN PLOTTING DEFAULT VALUES
              YFOB1=0.0D0
              YFOB2=0.0D0
              YFOB3=0.0D0
              XFOB1=0.0D0
              XFOB2=0.0D0
              XFOB3=0.0D0
              FANOFF=0.0D0
C     FAN WAVELENGTHS
C     RESTORE THE DEFAULT WAVELENGTH SETTINGS
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
              JK_WAV(1:10)=0
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
 900          CONTINUE
C
              FANNUM=1
              FANNM1=3
              FANNM2=3
              FANTYP=5
              QALTYP=0
              SSIFLG=.TRUE.
              REFWV=INT(SYSTEM1(11))
C     FAN PLOTTING DEFAULT VALUES HAVE BEEN SET
              FANNOB=0
              FANNRF=INT(SYSTEM1(25))
              FANNIM=INT(SYSTEM1(20))
              MAXFAN=20
              STEPJP=125.0D0
          ELSE
C     FANEXT=TRUE
          END IF
C     ADDED SO AS TO PLOT THE LAST FAN PRINTED USING
C     A CMD LEVEL FANS GENERATION COMMAND
          IF(FANEXT) THEN
              FANNUM=1
              FANNM1=3
              FANNM2=3
              YFOB1=LFOB(1)
              XFOB1=LFOB(2)
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
              IF(FANWAV.EQ.1) JK_WAV(1)=1
              IF(FANWAV.EQ.2) JK_WAV(1)=2
              IF(FANWAV.EQ.3) JK_WAV(1)=3
              IF(FANWAV.EQ.4) JK_WAV(1)=4
              IF(FANWAV.EQ.5) JK_WAV(1)=5
              IF(FANWAV.EQ.6) JK_WAV(1)=6
              IF(FANWAV.EQ.7) JK_WAV(1)=7
              IF(FANWAV.EQ.8) JK_WAV(1)=8
              IF(FANWAV.EQ.9) JK_WAV(1)=9
              IF(FANWAV.EQ.10) JK_WAV(1)=10
              JK_WAV(2:10)=0
              FANOFF=0.0D0
              SSI=0.0D0
              REFWV=INT(LFOB(4))
              IF(FANNAM(1:4).EQ.'YFAN') FANTYP=1
              IF(FANNAM(1:4).EQ.'XFAN') FANTYP=2
              IF(FANNAM(1:4).EQ.'NFAN') FANTYP=3
              IF(FANNAM(1:4).EQ.'PFAN') FANTYP=4
              IF(FANNAM(1:5).EQ.'XYFAN') FANTYP=5
              IF(FANNAM(1:5).EQ.'YXFAN') FANTYP=6
              IF(FANQAL(1:4).EQ.'    ') QALTYP=0
              IF(FANQAL(1:3).EQ.'OPD')  QALTYP=1
              IF(FANQAL(1:2).EQ.'CD')   QALTYP=2
              IF(FANQAL(1:2).EQ.'LA')   QALTYP=3
              FANNOB=0
              FANNRF=INT(SYSTEM1(25))
              FANNIM=INT(SYSTEM1(20))
              IF(LFOB(5).EQ.0.0D0) LFOB(5)=DBLE(NEWOBJ)
              IF(LFOB(6).EQ.0.0D0) LFOB(6)=DBLE(NEWREF)
              IF(LFOB(7).EQ.0.0D0) LFOB(7)=DBLE(NEWIMG)
              FANNOB=INT(LFOB(5))
              FANNRF=INT(LFOB(6))
              FANNIM=INT(LFOB(7))
              MAXFAN=20
              STEPJP=125.0D0
              SSIFLG=.TRUE.
C     FAN PLOTTING DEFAULT VALUES HAVE BEEN SET
              MAXFAN=20
              STEPJP=125.0D0

          ELSE
C     NO FAN WAS DONE
              FANEXT=.FALSE.
          END IF

          RETURN
      END


C SUB PLTRED.FOR
      SUBROUTINE PLTRED
C
          IMPLICIT NONE
C
C       THIS SUBROUTINE IS CALLED TO PLOT RED AT THE CMD LEVEL
C
          CHARACTER BL20*20,NNTT1*99,CRANGE*8,LABX*40,LABY*40
     1    ,TMY*8,DTY*10,UNN*9,UNN1*9,BLNOTE*80,B*80
C
          REAL GDTARP(1:101),GDTAR(1:101),AGDTARP(1:101),AGDTAR(1:101)
     1    ,DELX1,RDELA,LLIM,ULIM
C
          REAL*8 COLPAS,RANGE1,MTHETA,SPDELX,SPDELY,DELZ
C
          INTEGER NX,NY,ORTAG,I,ENNL,ENN,NT1ANG,NT1SIZ,DFLAG,MYJK
C
          COMMON/REDPASS/ENNL,GDTARP,GDTAR,MTHETA,SPDELX,SPDELY,DELZ
C
          LOGICAL DEXTRED,DEXTREDSQ
C
          COMMON/DREDEXT/DEXTRED
C
          COMMON/DREDEXTSQ/DEXTREDSQ
C
          LOGICAL EXTRED,EXTKED,CENTRED,SUMMS
C
          LOGICAL EXTREDSQ,SQUARE,ROUND
C
          COMMON/REDEXT/EXTRED,EXTKED
C
          COMMON/REDEXTSQ/EXTREDSQ,CENTRED,SUMMS
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          INCLUDE 'dathgr.inc'
          SQUARE=.FALSE.
          ROUND=.FALSE.
          IF(EXTRED.OR.DEXTRED) THEN
              ROUND=.TRUE.
              SQUARE=.FALSE.
          END IF
          IF(EXTREDSQ.OR.DEXTREDSQ) THEN
              ROUND=.FALSE.
              SQUARE=.TRUE.
          END IF
C
          MYJK=0
C
          IF(STI.EQ.1) THEN
              OUTLYNE=
     1        '"PLTRED" PLOTS THE EXISTING ENERGY DISTRIBUTION'
              CALL SHOWIT(1)
              RETURN
          ELSE
          END IF
          IF(SST.EQ.1.OR.SQ.EQ.1) THEN
              OUTLYNE=
     1        '"PLTRED" TAKES NO QUALIFIER OR STRING INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          ELSE
          END IF
          IF(S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
              OUTLYNE=
     1        '"PLTRED" ONLY TAKES NUMERIC WORD #1 AND #2 INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          ELSE
          END IF
          IF(S2.EQ.1.AND.W2.LE.0.0D0) THEN
              OUTLYNE=
     1        'THE EXTENT OF THE PLOT, NUMERIC WORD 2, MUST BE'
              CALL SHOWIT(1)
              OUTLYNE=
     1        'GREATER THAN ZERO'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          ELSE
          END IF
C
          IF(WC.EQ.'PLTRED'.AND..NOT.EXTRED.AND..NOT.EXTREDSQ
     1    .AND..NOT.DEXTRED.AND..NOT.DEXTREDSQ) THEN
              OUTLYNE=
     1        'ENERGY DISTRIBUTION VALUES DO NOT EXIST'
              CALL SHOWIT(1)
              OUTLYNE='NO ACTION TAKEN'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
          IF(WC.EQ.'PLTESED'.AND..NOT.EXTKED) THEN
              OUTLYNE=
     1        'EXPANDING SLIT ENERGY DISTRIBUTION VALUES DO NOT EXIST'
              CALL SHOWIT(1)
              OUTLYNE='NO ACTION TAKEN'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(ENNL.LT.10) THEN
              OUTLYNE=
     1        'LESS THAN 10 ENERGY VALUES IN TABLE'
              CALL SHOWIT(1)
              OUTLYNE='NO ACTION TAKEN'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(DF1.EQ.1.OR.DF1.EQ.0.AND.W1.EQ.0.0D0) DFLAG=0
          IF(DF1.EQ.0.AND.W1.NE.0.0D0) DFLAG=1
C     GENERATE GRAPHIC
C     DO A PLOT NEW
          DEVTYP=1
          LOOKY=0.0D0
          LOOKX=-1.0D0
          LOOKZ=0.0D0
          CALL PLTDEV
          GRASET=.TRUE.
          PLEXIS=.TRUE.
C     SET LETTER SIZE AND ANGLE
          BL20='                    '
          BLNOTE=BL20//BL20//BL20//BL20
          NT1SIZ=1
          NT1ANG=0
          CALL MY_SETCHARASPECT(1.5,1.5)
          CALL PLOTBOX
C
          COLPAS=COLLBL
          CALL MY_COLTYP(COLPAS)
C     DO THE PLOTTING OF THE LENS IDENTIFIER
C     AT X=200, Y=500
          NT1SIZ=1
          NT1ANG=0
          IF(STMPT) CALL MYTIME(TMY)
          IF(STMPD) CALL MYDATE(DTY)
          IF(.NOT.STMPT.AND..NOT.STMPD) NNTT1=LI
          IF(STMPT.AND.STMPD) NNTT1=TMY//' '//DTY//LI
          IF(STMPT.AND..NOT.STMPD) NNTT1=TMY//' '//LI
          IF(.NOT.STMPT.AND.STMPD) NNTT1=DTY//LI
          IF(NNTT1.NE.BLNOTE) THEN
              CALL MY_JUSTSTRING(200,500,NNTT1(1:80),NT1ANG,NT1SIZ,3)
          ELSE
C     LI BLANK, NOT ACTION
          END IF
C     DO THE PLOTTING OF THE RSPH STATUS
C     AT X=200, Y=250
          NT1SIZ=1
          NT1ANG=0
          IF(.NOT.SUMMS) THEN
              IF(CENTRED)
     1        NNTT1(1:80)='IMAGE POINT LOCATED AT SPOT CENTROID'
              IF(.NOT.CENTRED) THEN
                  IF(SPDELX.EQ.0.0D0.AND.SPDELY.EQ.0.0D0)
     1            NNTT1(1:80)='IMAGE POINT LOCATED ON THE CHIEF RAY'
                  IF(SPDELX.EQ.0.0D0.AND.SPDELY.EQ.0.0D0)
     1            NX=36
                  IF(SPDELX.NE.0.0D0.OR.SPDELY.NE.0.0D0)
     1            NNTT1(1:80)='IMAGE POINT LOCATED ON THE CHIEF RAY WITH OFFSETS'
                  IF(SPDELX.NE.0.0D0.OR.SPDELY.NE.0.0D0)
     1            NX=49
              END IF
              CALL MY_JUSTSTRING(200,250,NNTT1(1:NX),NT1ANG,NT1SIZ,3)
          END IF
C
C     TARGET ORIEMNTATION LEGEND
          IF(EXTKED) THEN
C
              CALL MY_PLOT(200,6725,0,0,0,10000,0,7000)
              CALL MY_PLOT(1200,6725,1,0,0,10000,0,7000)
C
              WRITE(B,101) MTHETA
              READ(B,200) CRANGE
              ORTAG=0
101           FORMAT(F8.3)
200           FORMAT(A8)
              IF(MTHETA.EQ.0.0D0.OR.MTHETA.EQ.180.0D0.OR.MTHETA.EQ.360.0D0)
     1         THEN
                  NNTT1=
     1            'VERTICAL OBJECT ORIENTATION'
                  ORTAG=1
              END IF
              IF(MTHETA.EQ.90.0D0.OR.MTHETA.EQ.270.0D0) THEN
                  NNTT1=
     1            'HORIZONTAL OBJECT ORIENTATION'
                  ORTAG=1
              END IF
              IF(ORTAG.EQ.0) NNTT1='OBJECT ORIENTATION ='
C
              CALL MY_JUSTSTRING(1400,6700,NNTT1(1:20),NT1ANG,NT1SIZ,3)

              IF(ORTAG.EQ.0) THEN
                  NNTT1=' '//CRANGE//' '//'DEGREES'
                  CALL MY_JUSTSTRING(2700,6700,NNTT1(1:17),NT1ANG,NT1SIZ,3)
              END IF
              ORTAG=0
          ELSE
C     WAS A RED OR REDSQ, DON'T DO THE ORIENTATION
          END IF
C
C
C       LOWER AND UPPER LIMITS OF RED ARE 0 AND 100.0%
C       LOWER AND UPPER LIMITS OF RANGE ARE 0 AND GDTARP(ENN+1)
          LLIM=0.0
          IF(DF2.EQ.1) ULIM=GDTARP(ENNL+1)
          IF(DF2.EQ.0) ULIM=REAL(W2)
C

          RDELA=ULIM
C
          IF(ROUND) THEN
              IF(SYSTEM1(30).GE.3.0D0) THEN
                  LABX='SEMI-DIAMETER (mrad)                    '
                  NX=20
              ELSE
                  IF(SYSTEM1(6).EQ.1.0D0)
     1              LABX='SEMI-DIAMETER (in)                      '
                  IF(SYSTEM1(6).EQ.2.0D0)
     1            LABX='SEMI-DIAMETER (cm)                      '
                  IF(SYSTEM1(6).EQ.3.0D0)
     1            LABX='SEMI-DIAMETER (mm)                      '
                  IF(SYSTEM1(6).EQ.4.0D0)
     1            LABX='SEMI-DIAMETER (meter)                   '
                  IF(SYSTEM1(6).EQ.1.0D0)
     1              NX=18
                  IF(SYSTEM1(6).EQ.2.0D0)
     1            NX=18
                  IF(SYSTEM1(6).EQ.3.0D0)
     1            NX=18
                  IF(SYSTEM1(6).EQ.4.0D0)
     1            NX=21
              END IF
          END IF
          IF(SQUARE) THEN
              IF(SYSTEM1(30).GE.3.0D0) THEN
                  LABX='SQUARE SIDE LENGTH (mrad)               '
                  NX=25
              ELSE
                  IF(SYSTEM1(6).EQ.1.0D0)
     1            LABX='SQUARE SIDE LENGTH (in)                 '
                  IF(SYSTEM1(6).EQ.2.0D0)
     1            LABX='SQUARE SIDE LENGTH (cm)                 '
                  IF(SYSTEM1(6).EQ.3.0D0)
     1            LABX='SQUARE SIDE LENGTH (mm)                 '
                  IF(SYSTEM1(6).EQ.4.0D0)
     1            LABX='SQUARE SIDE LENGTH (meter)              '
                  IF(SYSTEM1(6).EQ.1.0D0)
     1            NX=23
                  IF(SYSTEM1(6).EQ.2.0D0)
     1            NX=23
                  IF(SYSTEM1(6).EQ.3.0D0)
     1            NX=23
                  IF(SYSTEM1(6).EQ.4.0D0)
     1            NX=26
              END IF
          END IF

          IF(.NOT.SUMMS) THEN
              IF(EXTRED)
     1        LABY='% ENERGY (RED)                          '
              IF(EXTKED)
     1        LABY='% ENERGY (ESED)                         '
              IF(EXTREDSQ)
     1        LABY='% ENERGY (REDSQ)                        '
              IF(DEXTRED)
     1        LABY='% ENERGY (DRED)                         '
              IF(DEXTREDSQ)
     1        LABY='% ENERGY (DREDSQ)                       '
              IF(EXTRED)
     1        NY=14
              IF(EXTKED)
     1        NY=15
              IF(EXTREDSQ)
     1        NY=16
              IF(DEXTRED)
     1        NY=15
              IF(DEXTREDSQ)
     1        NY=17
          END IF
          IF(SUMMS) THEN
              IF(EXTRED)
     1        LABY='% ENERGY (SUMMED RED)                   '
              IF(EXTKED)
     1        LABY='% ENERGY (SUMMED ESED)                  '
              IF(EXTREDSQ)
     1        LABY='% ENERGY (SUMMED REDSQ)                 '
              IF(EXTRED)
     1        NY=21
              IF(EXTKED)
     1        NY=22
              IF(EXTREDSQ)
     1        NY=23
          END IF
C
C     PLOT THE AXES AND TICS
          CALL PLOTAXES4
C     PLOT THE HORIZONTAL AXIS NAME
          CALL PLOTHNAME(LABX,NX)
C     PLOT THE VERTICAL AXIS NAME
          CALL PLOTVNAME(LABY,NY)

C     PLOT THE VALUES FOR THE TIC MARKS
          CALL PLOTVVAL(0.0,10.0)
          DELX1=(ULIM-LLIM)/5.0
          CALL PLOTHVAL(LLIM,DELX1)

C
          DO I=1,101
              AGDTARP(I)=GDTARP(I)
              AGDTAR(I)=GDTAR(I)
          END DO
          ENN=ENNL
          CALL PLOTFUNC2(AGDTARP,AGDTAR,ENN+1,LLIM,ULIM,0.0,100.0,150)
          IF(.NOT.PLEXIS) PLEXIS=.TRUE.
C
          COLPAS=COLLBL
          CALL MY_COLTYP(COLPAS)
C     FIELD OF VIEW DATA
          IF(SYSTEM1(30).EQ.1.0D0.OR.SYSTEM1(30).EQ.2.0D0) THEN
C     FOCAL OR UFOCAL
              IF(SYSTEM1(6).EQ.1.0D0) UNN='inch     '
              IF(SYSTEM1(6).EQ.2.0D0) UNN='cm       '
              IF(SYSTEM1(6).EQ.3.0D0) UNN='mm       '
              IF(SYSTEM1(6).EQ.4.0D0) UNN='meter    '
          END IF
C
          IF(.NOT.SUMMS) THEN
              IF(DABS(ALENS(3,NEWOBJ)).GE.1.0D10) THEN
C     SCX FANG
                  RANGE1=SYSTEM1(23)*LFOB(2)
                  UNN1='DEGREE(S)'
              ELSE
                  RANGE1=SYSTEM1(16)*LFOB(2)
                  UNN1(1:9)=UNN(1:9)
C     SCX
              END IF
              WRITE(B,101) RANGE1
              READ(B,200) CRANGE
              NNTT1='OBJECT POINT-X'
C
              CALL MY_JUSTSTRING(1400,6500,NNTT1(1:14),NT1ANG,NT1SIZ,3)
              NNTT1=' = '//CRANGE//' '//UNN1
C
              CALL MY_JUSTSTRING(2700,6500,NNTT1(1:21),NT1ANG,NT1SIZ,3)
C
C
              IF(DABS(ALENS(3,NEWOBJ)).GE.1.0D10) THEN
C     SCY FANG
                  RANGE1=SYSTEM1(21)*LFOB(1)
                  UNN1='DEGREE(S)'
              ELSE
                  RANGE1=SYSTEM1(14)*LFOB(1)
                  UNN1(1:9)=UNN(1:9)
C     SCY
              END IF
              WRITE(B,101) RANGE1
              READ(B,200) CRANGE
              NNTT1='OBJECT POINT-Y'
C
              CALL MY_JUSTSTRING(1400,6300,NNTT1(1:14),NT1ANG,NT1SIZ,3)
              NNTT1=' = '//CRANGE//' '//UNN1
C
              CALL MY_JUSTSTRING(2700,6300,NNTT1(1:21),NT1ANG,NT1SIZ,3)
          END IF
C
          IF(.NOT.SUMMS) THEN
              IF(SPDELX.NE.0.0D0) THEN
                  RANGE1=SPDELX
                  UNN1(1:9)=UNN(1:9)
                  WRITE(B,101) RANGE1
                  READ(B,200) CRANGE
                  NNTT1='OFFSET  IN   X'
C
                  CALL MY_JUSTSTRING(1400,5900,NNTT1(1:14),NT1ANG,NT1SIZ,3)
                  NNTT1=' = '//CRANGE//' '//UNN1
C
                  CALL MY_JUSTSTRING(2700,5900,NNTT1(1:21),NT1ANG,NT1SIZ,3)
              END IF
              IF(SPDELY.NE.0.0D0) THEN
                  RANGE1=SPDELY
                  UNN1(1:9)=UNN(1:9)
                  WRITE(B,101) RANGE1
                  READ(B,200) CRANGE
                  NNTT1='OFFSET  IN   Y'
C
                  CALL MY_JUSTSTRING(1400,5700,NNTT1(1:14),NT1ANG,NT1SIZ,3)
                  NNTT1=' = '//CRANGE//' '//UNN1
C
                  CALL MY_JUSTSTRING(2700,5700,NNTT1(1:21),NT1ANG,NT1SIZ,3)
              END IF
          END IF
          IF(.NOT.SUMMS) THEN
              IF(SYSTEM1(30).LE.2.0D0) THEN
C     FOCAL OR UFOCAL, PRINT THE DEFOCUS
                  IF(DELZ.NE.0.0D0) THEN
                      RANGE1=DELZ
                      UNN1(1:9)=UNN(1:9)
                      WRITE(B,101) RANGE1
                      READ(B,200) CRANGE
                      NNTT1='DE-FOCUS IN  Z'
C
                      CALL MY_JUSTSTRING(1400,6100,NNTT1(1:14),NT1ANG,NT1SIZ,3)
                      NNTT1=' = '//CRANGE//' '//UNN1
C
                      CALL MY_JUSTSTRING(2700,6100,NNTT1(1:21),NT1ANG,NT1SIZ,3)
                  END IF
              END IF
          END IF
C
          IF(DFLAG.EQ.0) THEN
              SAVE_KDP(1)=SAVEINPT(1)
              INPUT='DRAW'
              CALL PROCES
              REST_KDP(1)=RESTINPT(1)
          END IF
          RETURN
      END


C SUB THREEDEESET
      SUBROUTINE THREEDEESET
          IMPLICIT NONE
          INCLUDE 'datmai.inc'
          IF(STI.EQ.1.OR.SN.EQ.0.AND.SQ.EQ.0.AND.SST.EQ.0) THEN
              IF(WC(1:2).EQ.'X1') THEN
                  WRITE(OUTLYNE,*) 'X1=',LX1
                  CALL SHOWIT(1)
              END IF
              IF(WC(1:2).EQ.'Y1') THEN
                  WRITE(OUTLYNE,*) 'Y1=',LY1
                  CALL SHOWIT(1)
              END IF
              IF(WC(1:2).EQ.'Z1') THEN
                  WRITE(OUTLYNE,*) 'Z1=',LZ1
                  CALL SHOWIT(1)
              END IF
              IF(WC(1:2).EQ.'X2') THEN
                  WRITE(OUTLYNE,*) 'X2=',LX2
                  CALL SHOWIT(1)
              END IF
              IF(WC(1:2).EQ.'Y2') THEN
                  WRITE(OUTLYNE,*) 'Y2=',LY2
                  CALL SHOWIT(1)
              END IF
              IF(WC(1:2).EQ.'Z2') THEN
                  WRITE(OUTLYNE,*) 'Z2=',LZ2
                  CALL SHOWIT(1)
              END IF
          END IF
          IF(SQ.EQ.1.OR.SST.EQ.1) THEN
              OUTLYNE=
     1        WC(1:2)//' TAKES NO QUALIFIER OR STRING INPUT'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
              OUTLYNE=
     1        WC(1:2)//' ONLY TAKES NUMERIC WORD #1 INPUT'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(WC.EQ.'X1') LX1=W1
          IF(WC.EQ.'Y1') LY1=W1
          IF(WC.EQ.'Z1') LZ1=W1
          IF(WC.EQ.'X2') LX2=W1
          IF(WC.EQ.'Y2') LY2=W1
          IF(WC.EQ.'Z2') LZ2=W1
          RETURN
      END


C SUB PLTLINE.FOR
      SUBROUTINE PLTLINE
          USE GLOBALS
C
          IMPLICIT NONE
C
C       THIS ROUTINE DOES THE 3D PLOT LINE COMMAND AT THE CMD LEVEL
C
          REAL*8 X,Y,Z,XN,YN,ZN,ROT1X,ROT1Z,ROT2Y,
     1    ROT2Z,AX,AY,AZ,AALF,APHI
     2    ,XNEW,YNEW,LKG,VIEPH,VIEAL
C
          LOGICAL GGO
C
          INTEGER IX,IY,I,IPST,COLPAS
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
          IF(MSG) THEN
              OUTLYNE='GENERATING LINE PLOTTING DATA...'
              CALL SHOWIT(1)
          END IF
C       CHECK SYNTAX
          IF(SST.EQ.1.OR.SN.EQ.1) THEN
              OUTLYNE=
     1        '"PLOT LINE" TAKES NO ADDITIONAL INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(STI.EQ.1) THEN
              WRITE(OUTLYNE,800)
              CALL SHOWIT(1)
 800          FORMAT('QUERRY (?) HAS NO MEANING WITH "PLOT LINE"')
              RETURN
          ELSE
          END IF
C       ALL INPUT IS OK, KEEP GOING
C       NOW WE HAVE THE REAL WORLD COORDINATES OF THE RAY TO BE PLOTTED
C       THEY ARE STORED IN THE ARRAY GLBRAY PASSED IN A COMMON BLOCK
C       VIA THE "INCLUDE 'datlen.inc'" STATEMENT.
C
C     HERE IS THE LOOP WHICH PLOTS THE LINE USING CALLS TO
C     PENMV1A(IX,IY,IPST)
c
          DO I=1,2
              IF(I.EQ.1) THEN
                  X=LX1
                  Y=LY1
                  Z=LZ1
              END IF
              IF(I.EQ.2) THEN
                  X=LX2
                  Y=LY2
                  Z=LZ2
              END IF
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
              Y=(Y/SCFAY)*1000.0D0
C
              X=(X/SCFAX)*1000.0D0
C
C     STEP 2: APPLY THE SCREEN Y-OFFSET AND SCREEN GAMMA ROTATION
C
C     APPLY THE PLOT LOOK/VIEW Y-OFFSET VALUE
C
              IF(LORIENT) CALL ORSHIFT
              Y=Y+3500.0D0+DBLE(PYSHFT)
              X=X+DBLE(PXSHFT)
C
              X=X+JUSOFF
C
C     IF THERE IS A NON-ZERO GAMMA SPECIFIED IN PLOT LOOK OR PLOT VIEW THEN
C     ROTATE IN GAMMA ON THE SCREEN ABOUT THE CENTER OF THE SCREEN
C     WHICH HAS COORDINATES X=5000.0D0,Y=3500.0D0
C
C     FIRST SHIFT THE COORDINATE ORIGIN TO THE CETER OF THE DISPLAY
C
              X=X-5000.0D0
              Y=Y-3500.0D0
C     THE SCREEN COORDINATE IN REAL*8 IN THE SHIFTED COORDINATE
C     FRAME IS NOW X AND Y

              IF(DBLE(PGAMMA).NE.0.0D0) THEN
                  LKG=(PII/180.0D0)*DBLE(PGAMMA)
                  XNEW=((X*DCOS(LKG))-(Y*DSIN(LKG)))
                  YNEW=((X*DSIN(LKG))+(Y*DCOS(LKG)))
                  X=XNEW
                  Y=YNEW
              ELSE
              END IF

C     THE ROTATION IS DONE, NOW SHIFT THE ORIGIN BACK TO THE BOTTOM
C     LEFT HAND CORNER
              X=X+5000.0D0
              Y=Y+3500.0D0
C
              IF(I.EQ.1) THEN
C     PUT INSTRUCTIONS IN P1ARAY TO LIFT PEN, GO TO STARTING POINT
                  IPST=0
                  IX=INT(X)
                  IY=INT(Y)
C
                  P1ARAY(I,1,1)=IX
                  P1ARAY(I,2,1)=IY
                  P1ARAY(I,3,1)=0
                  IF(.NOT.PLEXIS) PLEXIS=.TRUE.
              ELSE
              END IF
              IF(I.GT.1) THEN
C     PUT INSTRUCTIONS IN P1ARAY TO DROP PEN AND DRAW
                  IPST=1
                  IX=INT(X)
                  IY=INT(Y)
C     MOVE TO NEW POSITION WITH LOWERED PEN
                  P1ARAY(I,1,1)=IX
                  P1ARAY(I,2,1)=IY
                  IPST=1
                  P1ARAY(I,3,1)=IPST
                  IF(.NOT.PLEXIS) PLEXIS=.TRUE.
              END IF
C     IF FINISHED, LIFT PEN
              IF(I.EQ.STPSUR) IPST=0
          END DO
C
C     NOW ISSUE THE PLOTTING COMMANDS STORED IN THE P1ARAY ARRAY
C
          COLPAS=COLLBL
          CALL MY_COLTYP(COLPAS)
          FIXUP=.FALSE.
          DO I=1,2
              IF(I.EQ.1)
     1        CALL PENMV1A(P1ARAY(I,1,1),P1ARAY(I,2,1),P1ARAY(I,3,1))
              IF(I.GT.1) THEN
                  GGO=.TRUE.
C     TEST IF LAST POINT IS NEW POINT, IF SO DON'T MOVE
                  IF(P1ARAY(I,1,1).EQ.P1ARAY(I-1,1,1).AND.P1ARAY(I,2,1).EQ.
     1            P1ARAY(I-1,2,1)) GGO=.FALSE.
                  IF(P1ARAY(I,3,1).NE.0.AND.GGO)
     1            CALL PENMV1A(P1ARAY(I,1,1),P1ARAY(I,2,1),P1ARAY(I,3,1))
              END IF
          END DO
          COLPAS=COLLBL
          CALL MY_COLTYP(COLPAS)
C     HERE WE DO THE PLOT AXIS DRAWING
          IF(.NOT.VIGFLG.AND.PLTVIG) THEN
              CALL VIGSHO
              VIGFLG=.TRUE.
          ELSE
          END IF
          RETURN
      END


C SUB VIERAY.FOR
      SUBROUTINE VIERAY(VIEW2,VIEW3,VDF2,VDF3,VS2,VS3)
          REAL*8 VIEW2,VIEW3
          INTEGER VDF2,VDF3,VS2,VS3
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C             PLOT THE RAY
          W1=VIEW2
          W2=VIEW3
          S1=VS2
          S2=VS3
          S3=0
          S4=0
          S5=0
          SN=1
          DF1=VDF2
          DF2=VDF3
          DF3=1
          DF4=1
          DF5=1
          WQ='RAY'
          SQ=1
          STI=0
          SST=0
          CALL PLTRAE
          RETURN
      END


C SUB PLTPEN.FOR
      SUBROUTINE PLTPEN
C
          IMPLICIT NONE
C
C       THIS ROUTINE DOES THE PLOT PEN COMMAND AT THE CMD LEVEL
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          INTEGER COLPAS
C
C       PLOT PEN
C       CHECK SYNTAX
          IF(S4.EQ.1.OR.S5.EQ.1) THEN
              OUTLYNE=
     1        '"PLOT PEN" TAKES NO NUMERIC WORD #4 OR #5 INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SST.EQ.1) THEN
              OUTLYNE=
     1        '"PLOT PEN" TAKES NO STRING INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(STI.EQ.0) THEN
              IF(DF1.EQ.1.OR.DF2.EQ.1.OR.DF3.EQ.1) THEN
                  OUTLYNE=
     1            '"PLOT PEN" REQUIRES EXPLICIT NUMERIC WORD #1, #2 AND #3 INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C
              IF(INT(W3).LT.1.OR.INT(W3).GT.3) THEN
                  OUTLYNE=
     1            'NUMERIC WORD #3 BEYOND LEGAL RANGE FOR "PLOT PEN"'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
C       STI
          IF(STI.EQ.1) THEN
              WRITE(OUTLYNE,100)
              CALL SHOWIT(1)
              WRITE(OUTLYNE,200) XPEN
              CALL SHOWIT(1)
              WRITE(OUTLYNE,300) YPEN
              CALL SHOWIT(1)
              WRITE(OUTLYNE,400) PENSTA
              CALL SHOWIT(1)
              RETURN
          END IF
C       NW1 IS THE X COORDINATE
C       NW2 IS THE Y COORDINATE
C       NW3 IS THE PEN STATUS INDICATOR:
C
          XPENOL=XPEN
          YPENOL=YPEN
          XPEN=INT(W1)
          YPEN=INT(W2)
          PENSTA=INT(W3)
C
C               1=NO CHANGE IN PEN STATUS
C               2=LOWER PEN BEFORE MOVE
C               3=LIFT PEN BEFORE MOVE
C
C       VARAIBLE IS PENSTA THAT IS PASSED TO THE PLOT COMMAND
C       IN PENMV.FOR
C       NOW MOVE THE PEN
          COLPAS=COLPEN
          CALL MY_COLTYP(COLPAS)
          CALL PENMV
          IF(.NOT.PLEXIS) PLEXIS=.TRUE.
C       PEN POSITIONS ARE STORED IN XPEN AND YPEN
100       FORMAT('CURRENT "PLOT PEN" PARAMETERS ARE:')
200       FORMAT('     X-PEN POSITION = ',I5)
300       FORMAT('     Y-PEN POSITION = ',I5)
400       FORMAT('         PEN STATUS = ',I5)
          RETURN
      END
C SUB VERTLINE.FOR
      SUBROUTINE VERTLINE
          USE GLOBALS
C
          IMPLICIT NONE
C
C       THIS ROUTINE DOES THE PLOT LINE CONNECTING SURFACE VERTICES
C
          REAL*8 X,Y,Z,XN,YN,ZN,ROT1X,ROT1Z,ROT2Y,
     1    ROT2Z,AX,AY,AZ,AALF,APHI,YMAXI,YMINI,XMAXI,XMINI
     2    ,XNEW,YNEW,LKG,VIEPH,VIEAL
C
          LOGICAL GGO
C
          INTEGER IX,IY,I,IPST,COLPAS
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
C     LNTYPE=0
          LNTYPE=0
          X=0.0D0
          Y=0.0D0
C
          VIEPH=(PII/180.0D0)*VIEPHI
          VIEAL=(PII/180.0D0)*VIEALF
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
          GLOBE=.TRUE.
          OFFX=0.0D0
          OFFY=0.0D0
          OFFZ=0.0D0
          OFFA=0.0D0
          OFFB=0.0D0
          OFFC=0.0D0
          CALL GLVERT
          GLOBE=.FALSE.
          IF(MSG) THEN
              OUTLYNE='GENERATING VERTEX LINE PLOTTING DATA...'
              CALL SHOWIT(1)
          END IF
C       CHECK SYNTAX
          IF(SST.EQ.1) THEN
              OUTLYNE=
     1        '"PLOT VERTLINE" TAKES NO STRING INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
              OUTLYNE=
     1        '"PLOT VERTLINE" ONLY TAKES NUMERIC WORDS #1 AND #2 INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(STI.EQ.1) THEN
              WRITE(OUTLYNE,800)
              CALL SHOWIT(1)
 800          FORMAT('QUERRY (?) HAS NO MEANING WITH "PLOT VERTLINE"')
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
          IF(DF2.EQ.1) THEN
              IF(DABS(ALENS(3,(NEWIMG-1))).GT.1.0D10) THEN
                  W2=DBLE(NEWIMG-1)
              ELSE
                  W2=DBLE(NEWIMG)
              END IF
          ELSE
C       DF2 NOT 1, W2 EXPLICITLY ENTERED
          END IF
          STASUR=INT(W1)
          STPSUR=INT(W2)
          IF(STASUR.LT.NEWOBJ) STASUR=NEWOBJ
          IF(STPSUR.LT.NEWOBJ) STPSUR=NEWOBJ
C
          IF(INT(W1).LT.0) THEN
C       INVALID NUMERIC WORD #1
              OUTLYNE=
     1        'SURFACE NUMBER (NUMERIC WORD #1) IS BEYOND LEGAL RANGE'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(INT(W2).GT.NEWIMG) THEN
C       INVALID NUMERIC WORD #2
              OUTLYNE=
     1        'SURFACE NUMBER (NUMERIC WORD #2) IS BEYOND LEGAL RANGE'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(INT(W2).LE.INT(W1)) THEN
C       W2 LESS THAN OR EQUAL TO W1
              OUTLYNE=
     1        'NUMERIC WORD #2 MUST BE GREATER THAN NUMERIC WORD #1'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C       ALL INPUT IS OK, KEEP GOING
C       NOW WE HAVE THE REAL WORLD COORDINATES OF THE VERTEX LINE TO BE PLOTTED
C       THEY ARE STORED IN THE ARRAY VERTEX PASSED IN A COMMON BLOCK
C       VIA THE "INCLUDE 'datlen.inc'" STATEMENT.
C
C     HERE IS WHERE REAL WORK BEGINS. FIRST PROJECT THE 3D WORLD COORDINATES
C     OF THE VERTICIES ONTO THE 2D PLANE INDICATED BY THE PLOT LOOK/ PLOT VIEW
C     DATA. THE DIRECTION COSINES OF THE LOOK VECTOR ARE LOOKX, LOOKY AND
C     LOOKZ. THE TRANSFORMATIONS ARE:
C
          CALL ROT1
C
C     AFTER THIS TRANSFORMATION, THE WORLD X COORDINATE IS PLOTTED IN THE
C     HORIZONTAL X-AXIS OF THE DISPLAY AND THE Y WORLD COORDINATE IS PLOTTED
C     IN THE VERTICAL Y-AXIS OF THE DISPLAY AFTER A CONVERSION TO
C     DEVICE INDEPENDENT COORDINATES ARE MADE.
C
          CALL PLTSC1(XMINI,XMAXI,YMINI,YMAXI)
C
C     HERE IS THE LOOP WHICH PLOTS VERTEX LINE USING CALLS TO
C     PENMV1A(IX,IY,IPST)
c
          IF(STPSUR.LT.STASUR) STPSUR=STASUR
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
              Y=(Y/SCFAY)*1000.0D0
C
              X=(X/SCFAX)*1000.0D0
C
C     STEP 2: APPLY THE SCREEN Y-OFFSET AND SCREEN GAMMA ROTATION
C
C     APPLY THE PLOT LOOK/VIEW Y-OFFSET VALUE
C
              IF(LORIENT) CALL ORSHIFT
              Y=Y+3500.0D0+DBLE(PYSHFT)
              X=X+DBLE(PXSHFT)
C
C     IF THERE ARE X-OFFSETS (JUSTIFICATION) TO APPLY, DO THEM HERE.
C
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
              X=X+JUSOFF
C
C     IF THERE IS A NON-ZERO GAMMA SPECIFIED IN PLOT LOOK OR PLOT VIEW THEN
C     ROTATE IN GAMMA ON THE SCREEN ABOUT THE CENTER OF THE SCREEN
C     WHICH HAS COORDINATES X=5000.0D0,Y=3500.0D0
C
C     FIRST SHIFT THE COORDINATE ORIGIN TO THE CETER OF THE DISPLAY
C
              X=X-5000.0D0
              Y=Y-3500.0D0
C     THE SCREEN COORDINATE IN REAL*8 IN THE SHIFTED COORDINATE
C     FRAME IS NOW X AND Y

              IF(DBLE(PGAMMA).NE.0.0D0) THEN
                  LKG=(PII/180.0D0)*DBLE(PGAMMA)
                  XNEW=((X*DCOS(LKG))-(Y*DSIN(LKG)))
                  YNEW=((X*DSIN(LKG))+(Y*DCOS(LKG)))
                  X=XNEW
                  Y=YNEW
              ELSE
              END IF

C     THE ROTATION IS DONE, NOW SHIFT THE ORIGIN BACK TO THE BOTTOM
C     LEFT HAND CORNER
              X=X+5000.0D0
              Y=Y+3500.0D0
C
              IF(I.EQ.STASUR) THEN
C     PUT INSTRUCTIONS IN P1ARAY TO LIFT PEN, GO TO STARTING POINT
                  IPST=0
                  IX=INT(X)
                  IY=INT(Y)
C
                  P1ARAY(I,1,1)=IX
                  P1ARAY(I,2,1)=IY
                  P1ARAY(I,3,1)=0
                  IF(.NOT.PLEXIS) PLEXIS=.TRUE.
              ELSE
              END IF
              IF(I.GT.STASUR) THEN
C     PUT INSTRUCTIONS IN P1ARAY TO DROP PEN AND DRAW
                  IPST=1
                  IX=INT(X)
                  IY=INT(Y)
C     IF RAY IS VIRTUAL AND NOVIRT IS TRUE, LIFT PEN ANYWAY
                  IF(NOVIRT.AND.GLVIRT(I).AND.I.NE.STPSUR.AND.DF2.EQ.1) THEN
C     LEAVE PEN ALONE AT LAST POSITION WITH PEN UP
                      IPST=0
                      IF(I.NE.0) P1ARAY(I,1,1)=P1ARAY(I-1,1,1)
                      IF(I.NE.0) P1ARAY(I,2,1)=P1ARAY(I-1,2,1)
                  ELSE
C     MOVE TO NEW POSITION WITH LOWERED PEN
                      P1ARAY(I,1,1)=IX
                      P1ARAY(I,2,1)=IY
                      IPST=1
                  END IF
                  P1ARAY(I,3,1)=IPST
                  IF(.NOT.PLEXIS) PLEXIS=.TRUE.
              END IF
C     IF FINISHED, LIFT PEN
              IF(I.EQ.STPSUR) IPST=0
          END DO
C
C     NOW ISSUE THE PLOTTING COMMANDS STORED IN THE P1ARAY ARRAY
C
          COLPAS=COLRAY
          CALL MY_COLTYP(COLPAS)
          FIXUP=.FALSE.
          DO I=STASUR,STPSUR
              IF(I.EQ.STASUR)
     1        CALL PENMV1A(P1ARAY(I,1,1),P1ARAY(I,2,1),P1ARAY(I,3,1))
              IF(I.GT.STASUR) THEN
                  GGO=.TRUE.
C     TEST IF LAST POINT IS NEW POINT, IF SO DON'T MOVE
                  IF(P1ARAY(I,1,1).EQ.P1ARAY(I-1,1,1).AND.P1ARAY(I,2,1).EQ.
     1            P1ARAY(I-1,2,1)) GGO=.FALSE.
                  IF(P1ARAY(I,3,1).NE.0.AND.GGO)
     1            CALL PENMV1A(P1ARAY(I,1,1),P1ARAY(I,2,1),P1ARAY(I,3,1))
              END IF
          END DO
          COLPAS=COLLBL
          CALL MY_COLTYP(COLPAS)
C     HERE WE DO THE PLOT LI AND PLOT AXIS DRAWING
          IF(.NOT.VIGFLG.AND.PLTVIG) THEN
              CALL VIGSHO
              VIGFLG=.TRUE.
          ELSE
          END IF
          RETURN
      END


C SUB VIE.FOR
      SUBROUTINE VIE(CACOCHVIE)
C
          IMPLICIT NONE
C
C       THIS PROGRAM CONTROLS THE "VIE" AND "VIECO" COMMANDS
C
          REAL*8 VIEROT,VHI,VLO,XVHI,XVLO,YVHI,YVLO
C
          INTEGER DFLAG,VIEXOF,VIEYOF
C
          COMMON/OFFVIE/VIEXOF,VIEYOF,VIEROT
C
          INTEGER COLPAS,VDF1,VDF2,VDF3,VS1,VS2
     1    ,VS3,I,J,CACOCHVIE
C
          CHARACTER VIEWQ*8
C
          REAL*8 VIEW1,VIEW2,VIEW3,MDX,MDY,SFI,GAMGAM
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          INCLUDE 'datmac.inc'
C
          IF(DEVTYP.EQ.0.AND.VIEOVERLAY) VIEOVERLAY=.FALSE.
C       VIG OVERLAY ADDED 4/16/2004
          IF(.NOT.VIEOVERLAY) THEN
              PLEXIS=.FALSE.
              DEVTYP=0
              GRASET=.FALSE.
          END IF
C
C     W1 IS FOR SF
C     W2 IS FOR I
C     W3 IS FOR J
C
          IF(DF2.EQ.1) THEN
              IF(DABS(ALENS(3,NEWOBJ)).GT.1.0D10) THEN
                  W2=DBLE(NEWOBJ+1)
                  DF2=0
                  S2=1
              ELSE
                  W2=DBLE(NEWOBJ)
                  DF2=0
                  S2=1
              END IF
          ELSE
C       DF2 NOT 1, W2 EXPLICITLY ENTERED
          END IF
          IF(DF3.EQ.1) THEN
              IF(DABS(ALENS(3,(NEWIMG-1))).GT.1.0D10) THEN
                  W3=DBLE(NEWIMG-1)
                  DF3=0
                  S3=1
              ELSE
                  W3=DBLE(NEWIMG)
                  DF3=0
                  S3=1
              END IF
          ELSE
C       DF3 NOT 1, W3 EXPLICITLY ENTERED
          END IF
C
C     QUALIFIER WORDS:
C         XZ
C         YZ
C         XY
C         ORTHO
C
          IF(SQ.EQ.0) THEN
              SQ=1
              WQ='YZ      '
          END IF
          IF(WQ.NE.'XZ'.AND.WQ.NE.'YZ'.AND.WQ.NE.'XY'.AND.
     1    WQ.NE.'ORTHO') THEN
              OUTLYNE= 'FOR "VIE" AND "VIECO"'
              CALL SHOWIT(1)
              OUTLYNE=
     1        'THE ONLY VALID QUALIFIER WORDS ARE:'
              CALL SHOWIT(1)
              OUTLYNE='"YZ     - FOR YZ-VIEWS'
              CALL SHOWIT(1)
              OUTLYNE='"XZ     - FOR XZ-VIEWS'
              CALL SHOWIT(1)
              OUTLYNE='"XY     - FOR XY-VIEWS'
              CALL SHOWIT(1)
              OUTLYNE='"ORTHO" - FOR ORTHOGRAPHIC'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(STI.EQ.1) THEN
              OUTLYNE= '"VIE" AND "VIECO" PERFOM AUTOMATED LENS PLOTTING'
              CALL SHOWIT(1)
              RETURN
          END IF
          IF(DF1.EQ.0.AND.W1.LE.0.0D0) THEN
              OUTLYNE= 'THE "SF" VALUE MUST BE GREATER THAN 0.0'
              CALL SHOWIT(1)
              OUTLYNE= 'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(DF2.EQ.0.AND.W2.LT.0.0D0) THEN
              OUTLYNE=
     1        'THE FIRST SURFACE # CAN NOT BE LESS THAN 0'
              CALL SHOWIT(1)
              OUTLYNE= 'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(DF2.EQ.0.AND.W2.GT.SYSTEM1(20)) THEN
              OUTLYNE=
     1        'THE FIRST SURFACE CAN NOT BE BEYOND THE IMAGE SURFACE'
              CALL SHOWIT(1)
              OUTLYNE= 'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(DF3.EQ.0.AND.W3.LT.0.0D0) THEN
              OUTLYNE=
     1        'THE LAST SURFACE # CAN NOT BE LESS THAN 0'
              CALL SHOWIT(1)
              OUTLYNE= 'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(DF3.EQ.0.AND.W3.GT.SYSTEM1(20)) THEN
              OUTLYNE=
     1        'THE LAST SURFACE CAN NOT BE BEYOND THE IMAGE SURFACE'
              CALL SHOWIT(1)
              OUTLYNE= 'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(DF2.EQ.0.AND.DF3.EQ.0.AND.W2.GE.W3) THEN
              OUTLYNE=
     1        'THE FIRST SURFACE MUST BE IN FRONT OF THE SECOND SURFACE'
              CALL SHOWIT(1)
              OUTLYNE= 'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(DF4.EQ.1.OR.DF4.EQ.0.AND.W4.EQ.0.0D0) DFLAG=0
          IF(DF4.EQ.0.AND.W4.NE.0.0D0) DFLAG=1
C
          VIEWQ=WQ
          VIEW1=W1
          VIEW2=W2
          VIEW3=W3
          VDF1=DF1
          VDF2=DF2
          VDF3=DF3
          VS1=S1
          VS2=S2
          VS3=S3
C
C     DO A PLOT NEW
C
          DEVTYP=1
          IF(WC.EQ.'VIE') OUTLYNE='"VIE" IS PROCESSING LENS DATA'
          IF(WC.EQ.'VIECO') OUTLYNE='"VIECO" IS PROCESSING LENS DATA'
          CALL SHOWIT(1)
          OUTLYNE='PLEASE WAIT...'
          CALL SHOWIT(1)
C
C******************************************
C
          IF(.NOT.VIEOVERLAY) THEN
C       NOT A VIE OVERLAY
              CALL PLTDEV1
              GRASET=.TRUE.
C     DO A FRAME
              CALL PLOTBOX
              SAVE_KDP(1)=SAVEINPT(1)
              INPUT='PLOT LI'
              CALL PROCES
              REST_KDP(1)=RESTINPT(1)
          END IF
C
C******************************************
C
C     OFFSETS AND PLOT ROTATION
!     Draw axis 'ORTHO'

          IF(VIEXOF.NE.0) THEN
              SAVE_KDP(1)=SAVEINPT(1)
C             XSHIFT
              W1=DBLE(VIEXOF)
              S1=1
              S2=0
              S3=0
              S4=0
              S5=0
              SN=1
              DF1=0
              DF2=1
              DF3=1
              DF4=1
              DF5=1
              WQ='XSHIFT'
              SQ=1
              STI=0
              SST=0
              CALL XYGAMA
              REST_KDP(1)=RESTINPT(1)
          END IF
          IF(VIEYOF.NE.0) THEN
              SAVE_KDP(1)=SAVEINPT(1)
C             XSHIFT
              W1=DBLE(VIEYOF)
              S1=1
              S2=0
              S3=0
              S4=0
              S5=0
              SN=1
              DF1=0
              DF2=1
              DF3=1
              DF4=1
              DF5=1
              WQ='YSHIFT'
              SQ=1
              STI=0
              SST=0
              CALL XYGAMA
              REST_KDP(1)=RESTINPT(1)
          END IF
          IF(VIEROT.NE.0.0D0) THEN
              SAVE_KDP(1)=SAVEINPT(1)
C             XSHIFT
              W1=VIEROT
              S1=1
              S2=0
              S3=0
              S4=0
              S5=0
              SN=1
              DF1=0
              DF2=1
              DF3=1
              DF4=1
              DF5=1
              WQ='GAMMA'
              SQ=1
              STI=0
              SST=0
              CALL XYGAMA
              REST_KDP(1)=RESTINPT(1)
          END IF
C
C     SET UP THE SCALE FACTOR
          IF(VDF1.EQ.0) THEN
              AUTSL=.FALSE.
              SCFAYP=1.0D0/VIEW1
              SCFAXP=1.0D0/VIEW1
              PSIZYP=VIEW1
              PSIZXP=VIEW1
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
          ELSE
          END IF
C
C     SCALE FACTOR DONE
C
C     NOW SET LOOK VECTOR
          SAVE_KDP(1)=SAVEINPT(1)
          IF(VIEWQ.EQ.'YZ') THEN
              INPUT='PLOT LOOK -1 0 0'
              CALL PROCES
          END IF
          IF(VIEWQ.EQ.'XZ') THEN
              INPUT='PLOT LOOK 0 1 0'
              CALL PROCES
          END IF
          IF(VIEWQ.EQ.'XY') THEN
              INPUT='PLOT LOOK 0 0 -1'
              CALL PROCES
          END IF
          IF(VIEWQ.EQ.'ORTHO') THEN
              IF(DF5.EQ.1) INPUT='PLOT VIEW 26.2 232.2'
              CALL PROCES
          END IF
          REST_KDP(1)=RESTINPT(1)
          IF(.NOT.VIEOVERLAY) THEN
              SAVE_KDP(1)=SAVEINPT(1)
              INPUT='PLOT AXIS'
              CALL PROCES
              REST_KDP(1)=RESTINPT(1)
              SAVE_KDP(1)=SAVEINPT(1)
              INPUT='PLOT YESLOOK'
              CALL PROCES
              REST_KDP(1)=RESTINPT(1)
          END IF

C
C  GUT RAY
          SAVE_KDP(1)=SAVEINPT(1)
          INPUT='FOB'
          CALL PROCES
          REST_KDP(1)=RESTINPT(1)
C
          SAVE_KDP(1)=SAVEINPT(1)
          WW1=0.0D0
          WW2=0.0D0
          WW3=SYSTEM1(11)
          WVN=WW3
          MSG=.FALSE.
          WW4=1.0D0
          NOCOAT=.TRUE.
          GRASET=.TRUE.
          IF(CACOCHVIE.EQ.1) CACOCH=1
          CALL RAYTRA
          REST_KDP(1)=RESTINPT(1)
C

          SAVE_KDP(1)=SAVEINPT(1)
          IF(RAYEXT) CALL VIERAY(VIEW2,VIEW3,VDF2,VDF3,VS2,VS3)
          REST_KDP(1)=RESTINPT(1)

C
          IF(VIEWQ.EQ.'YZ'.OR.VIEWQ.EQ.'ORTHO'.OR.VIEWQ.EQ.'XY') THEN
C     PLOT PROFY
              SAVE_KDP(1)=SAVEINPT(1)
C             PLOT PROFY
              W1=VIEW2
              W2=VIEW3
              W3=90.0D0
              S1=VS2
              S2=VS3
              S3=1
              S4=0
              S5=0
              SN=1
              DF1=VDF2
              DF2=VDF3
              DF3=0
              DF4=1
              DF5=1
              WQ='PROF'
              SQ=1
              STI=0
              SST=0
              CALL PLTPRO1
              REST_KDP(1)=RESTINPT(1)
          END IF

          IF(VIEWQ.EQ.'YZ'.OR.VIEWQ.EQ.'ORTHO'.OR.VIEWQ.EQ.'XY') THEN
C     PLOT EDGEY
              SAVE_KDP(1)=SAVEINPT(1)
C             PLOT EDGEY
              W1=VIEW2
              W2=VIEW3
              S1=VS2
              S2=VS3
              S3=0
              S4=0
              S5=0
              SN=1
              DF1=VDF2
              DF2=VDF3
              DF3=1
              DF4=1
              DF5=1
              WQ='EDGEY'
              SQ=1
              STI=0
              SST=0
              CALL PLTEDG
              REST_KDP(1)=RESTINPT(1)
          END IF
C
          IF(VIEWQ.EQ.'XZ'.OR.VIEWQ.EQ.'ORTHO'.OR.VIEWQ.EQ.'XY') THEN
C     PLOT PROFX
              SAVE_KDP(1)=SAVEINPT(1)
C             PLOT PROFX
              W1=VIEW2
              W2=VIEW3
              W3=0.0D0
              S1=VS2
              S2=VS3
              S3=1
              S4=0
              S5=0
              SN=1
              DF1=VDF2
              DF2=VDF3
              DF3=0
              DF4=1
              DF5=1
              WQ='PROF'
              SQ=1
              STI=0
              SST=0
              CALL PLTPRO1
              REST_KDP(1)=RESTINPT(1)
          END IF
          IF(VIEWQ.EQ.'XZ'.OR.VIEWQ.EQ.'ORTHO'.OR.VIEWQ.EQ.'XY') THEN
C     PLOT EDGEX
              SAVE_KDP(1)=SAVEINPT(1)
C             PLOT EDGEX
              W1=VIEW2
              W2=VIEW3
              S1=VS2
              S2=VS3
              S3=0
              S4=0
              S5=0
              SN=1
              DF1=VDF2
              DF2=VDF3
              DF3=1
              DF4=1
              DF5=1
              WQ='EDGEX'
              SQ=1
              STI=0
              SST=0
              CALL PLTEDG
              REST_KDP(1)=RESTINPT(1)
          END IF
C
          IF(VIEWQ.EQ.'XY'.OR.VIEWQ.EQ.'ORTHO') THEN
C     PLOT CLAP
              SAVE_KDP(1)=SAVEINPT(1)
C             PLOT CLAP
              W1=VIEW2
              W2=VIEW3
              S1=VS2
              S2=VS3
              S3=0
              S4=0
              S5=0
              SN=1
              DF1=VDF2
              DF2=VDF3
              DF3=1
              DF4=1
              DF5=1
              WQ='CLAP'
              SQ=1
              STI=0
              SST=0
              SFI=1.0D0

!       Draw lens
              DO I=INT(W1),INT(W2)
                  IF(ALENS(127,I).NE.0.0D0) THEN
                      DO J=1,INT(ALENS(127,I))
                          MDX=MULTCLAP(J,1,I)
                          MDY=MULTCLAP(J,2,I)
                          GAMGAM=MULTCLAP(J,2,I)
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
              REST_KDP(1)=RESTINPT(1)
          END IF


C
C     OTHER RAYS
C
C     Y FIELDS OF VIEW ARE DONE WHEN VIEW IS YZ, XY OR ORTHO
          IF(VIEWQ.EQ.'YZ'.OR.VIEWQ.EQ.'ORTHO'.OR.VIEWQ.EQ.'XY') THEN
C     DO YZ AND XZ PLANE RAYS
C
C     FIRST FOB 0
              SAVE_KDP(1)=SAVEINPT(1)
              INPUT='FOB'
              CALL PROCES
              REST_KDP(1)=RESTINPT(1)
              IF(.NOT.VIGOFF) THEN
                  CALL VIGCAL(10,VHI,VLO,2)
                  YVHI=VHI
                  YVLO=VLO
              ELSE
                  YVHI=1.0D0
                  YVLO=-1.0D0
              END IF
              IF(.NOT.VIGOFF) THEN
                  CALL VIGCAL(10,VHI,VLO,1)
                  XVHI=VHI
                  XVLO=VLO
              ELSE
                  XVHI=1.0D0
                  XVLO=-1.0D0
              END IF
              SAVE_KDP(1)=SAVEINPT(1)
C     GUT RAY A SECOND TIME
              WW1=0.0D0
              WW2=0.0D0
              WW3=SYSTEM1(11)
              WVN=WW3
              MSG=.FALSE.
              WW4=1.0D0
              NOCOAT=.TRUE.
              GRASET=.TRUE.
              IF(CACOCHVIE.EQ.1) CACOCH=1
              CALL RAYTRA
              REST_KDP(1)=RESTINPT(1)
              SAVE_KDP(1)=SAVEINPT(1)
              IF(RAYEXT) CALL VIERAY(VIEW2,VIEW3,VDF2,VDF3,VS2,VS3)
              REST_KDP(1)=RESTINPT(1)
C     FIRST MARGINAL RAYS
C     PLUS Y-RAY
              SAVE_KDP(1)=SAVEINPT(1)
              WW1=YVHI
              WW2=0.0D0
              WW3=SYSTEM1(11)
              WVN=WW3
              MSG=.FALSE.
              WW4=1.0D0
              NOCOAT=.TRUE.
              GRASET=.TRUE.
              IF(CACOCHVIE.EQ.1) CACOCH=1
              CALL RAYTRA
              REST_KDP(1)=RESTINPT(1)
C
              SAVE_KDP(1)=SAVEINPT(1)
              IF(RAYEXT) CALL VIERAY(VIEW2,VIEW3,VDF2,VDF3,VS2,VS3)
              REST_KDP(1)=RESTINPT(1)
C     MINUS Y-RAY
              SAVE_KDP(1)=SAVEINPT(1)
              WW1=YVLO
              WW2=0.0D0
              WW3=SYSTEM1(11)
              WVN=WW3
              MSG=.FALSE.
              WW4=1.0D0
              NOCOAT=.TRUE.
              GRASET=.TRUE.
              IF(CACOCHVIE.EQ.1) CACOCH=1
              CALL RAYTRA
              REST_KDP(1)=RESTINPT(1)
C
              SAVE_KDP(1)=SAVEINPT(1)
              IF(RAYEXT) CALL VIERAY(VIEW2,VIEW3,VDF2,VDF3,VS2,VS3)
              REST_KDP(1)=RESTINPT(1)
              IF(VIEWQ.NE.'YZ') THEN
C     PLUS X-RAY
                  SAVE_KDP(1)=SAVEINPT(1)
                  WW1=0.0D0
                  WW2=XVHI
                  WW3=SYSTEM1(11)
                  WVN=WW3
                  MSG=.FALSE.
                  WW4=1.0D0
                  NOCOAT=.TRUE.
                  GRASET=.TRUE.
                  IF(CACOCHVIE.EQ.1) CACOCH=1
                  CALL RAYTRA
                  REST_KDP(1)=RESTINPT(1)
C
                  SAVE_KDP(1)=SAVEINPT(1)
                  IF(RAYEXT) CALL VIERAY(VIEW2,VIEW3,VDF2,VDF3,VS2,VS3)
                  REST_KDP(1)=RESTINPT(1)
C     MINUS X-RAY
                  SAVE_KDP(1)=SAVEINPT(1)
                  WW1=0.0D0
                  WW2=XVLO
                  WW3=SYSTEM1(11)
                  WVN=WW3
                  MSG=.FALSE.
                  WW4=1.0D0
                  NOCOAT=.TRUE.
                  GRASET=.TRUE.
                  IF(CACOCHVIE.EQ.1) CACOCH=1
                  CALL RAYTRA
                  REST_KDP(1)=RESTINPT(1)
C
                  SAVE_KDP(1)=SAVEINPT(1)
                  IF(RAYEXT) CALL VIERAY(VIEW2,VIEW3,VDF2,VDF3,VS2,VS3)
                  REST_KDP(1)=RESTINPT(1)
              END IF
C     TOP Y FOV
C     FIRST FOB 1
              SAVE_KDP(1)=SAVEINPT(1)
              INPUT='FOB 1'
              CALL PROCES
              REST_KDP(1)=RESTINPT(1)
              IF(.NOT.VIGOFF) THEN
                  CALL VIGCAL(10,VHI,VLO,2)
                  YVHI=VHI
                  YVLO=VLO
              ELSE
                  YVHI=1.0D0
                  YVLO=-1.0D0
              END IF
              IF(.NOT.VIGOFF) THEN
                  CALL VIGCAL(10,VHI,VLO,1)
                  XVHI=VHI
                  XVLO=VLO
              ELSE
                  XVHI=1.0D0
                  XVLO=-1.0D0
              END IF
C
C     FIRST MARGINAL RAYS
C     GUT RAY

              SAVE_KDP(1)=SAVEINPT(1)
              WW1=0.0D0
              WW2=0.0D0
              WW3=SYSTEM1(11)
              WVN=WW3
              MSG=.FALSE.
              WW4=1.0D0
              NOCOAT=.TRUE.
              GRASET=.TRUE.
              IF(CACOCHVIE.EQ.1) CACOCH=1
              CALL RAYTRA
              REST_KDP(1)=RESTINPT(1)
C
              SAVE_KDP(1)=SAVEINPT(1)
              IF(RAYEXT) CALL VIERAY(VIEW2,VIEW3,VDF2,VDF3,VS2,VS3)
              REST_KDP(1)=RESTINPT(1)
C
C     MARGINAL RAYS
C     PLUS Y-RAY
              SAVE_KDP(1)=SAVEINPT(1)
              WW1=YVHI
              WW2=0.0D0
              WW3=SYSTEM1(11)
              WVN=WW3
              MSG=.FALSE.
              WW4=1.0D0
              NOCOAT=.TRUE.
              GRASET=.TRUE.
              IF(CACOCHVIE.EQ.1) CACOCH=1
              CALL RAYTRA
              REST_KDP(1)=RESTINPT(1)
C
              SAVE_KDP(1)=SAVEINPT(1)
              IF(RAYEXT) CALL VIERAY(VIEW2,VIEW3,VDF2,VDF3,VS2,VS3)
              REST_KDP(1)=RESTINPT(1)
C     MINUS Y-RAY
              SAVE_KDP(1)=SAVEINPT(1)
              WW1=YVLO
              WW2=0.0D0
              WW3=SYSTEM1(11)
              WVN=WW3
              MSG=.FALSE.
              WW4=1.0D0
              NOCOAT=.TRUE.
              GRASET=.TRUE.
              IF(CACOCHVIE.EQ.1) CACOCH=1
              CALL RAYTRA
              REST_KDP(1)=RESTINPT(1)
C
              SAVE_KDP(1)=SAVEINPT(1)
              IF(RAYEXT) CALL VIERAY(VIEW2,VIEW3,VDF2,VDF3,VS2,VS3)
              REST_KDP(1)=RESTINPT(1)
              IF(VIEWQ.NE.'YZ') THEN
C     PLUS X-RAY
                  SAVE_KDP(1)=SAVEINPT(1)
                  WW1=0.0D0
                  WW2=XVHI
                  WW3=SYSTEM1(11)
                  WVN=WW3
                  MSG=.FALSE.
                  WW4=1.0D0
                  NOCOAT=.TRUE.
                  GRASET=.TRUE.
                  IF(CACOCHVIE.EQ.1) CACOCH=1
                  CALL RAYTRA
                  REST_KDP(1)=RESTINPT(1)
C
                  SAVE_KDP(1)=SAVEINPT(1)
                  IF(RAYEXT) CALL VIERAY(VIEW2,VIEW3,VDF2,VDF3,VS2,VS3)
                  REST_KDP(1)=RESTINPT(1)
C     MINUS X-RAY
                  SAVE_KDP(1)=SAVEINPT(1)
                  WW1=0.0D0
                  WW2=XVLO
                  WW3=SYSTEM1(11)
                  WVN=WW3
                  MSG=.FALSE.
                  WW4=1.0D0
                  NOCOAT=.TRUE.
                  GRASET=.TRUE.
                  IF(CACOCHVIE.EQ.1) CACOCH=1
                  CALL RAYTRA
                  REST_KDP(1)=RESTINPT(1)
C
                  SAVE_KDP(1)=SAVEINPT(1)
                  IF(RAYEXT) CALL VIERAY(VIEW2,VIEW3,VDF2,VDF3,VS2,VS3)
                  REST_KDP(1)=RESTINPT(1)
              END IF
C     BOTTOM Y FOV
C     FOB -1
              SAVE_KDP(1)=SAVEINPT(1)
              IF(VSYM) INPUT='FOB -1'
              IF(.NOT.VSYM) INPUT='FOB 0.7'
              CALL PROCES
              REST_KDP(1)=RESTINPT(1)
              IF(.NOT.VIGOFF) THEN
                  CALL VIGCAL(10,VHI,VLO,2)
                  YVHI=VHI
                  YVLO=VLO
              ELSE
                  YVHI=1.0D0
                  YVLO=-1.0D0
              END IF
              IF(.NOT.VIGOFF) THEN
                  CALL VIGCAL(10,VHI,VLO,1)
                  XVHI=VHI
                  XVLO=VLO
              ELSE
                  XVHI=1.0D0
                  XVLO=-1.0D0
              END IF
C
C     FIRST MARGINAL RAYS
C     GUT RAY
              SAVE_KDP(1)=SAVEINPT(1)
              WW1=0.0D0
              WW2=0.0D0
              WW3=SYSTEM1(11)
              WVN=WW3
              MSG=.FALSE.
              WW4=1.0D0
              NOCOAT=.TRUE.
              GRASET=.TRUE.
              IF(CACOCHVIE.EQ.1) CACOCH=1
              CALL RAYTRA
              REST_KDP(1)=RESTINPT(1)
C
              SAVE_KDP(1)=SAVEINPT(1)
              IF(RAYEXT) CALL VIERAY(VIEW2,VIEW3,VDF2,VDF3,VS2,VS3)
              REST_KDP(1)=RESTINPT(1)
C
C     MARGINAL RAYS
C     PLUS Y-RAY
              SAVE_KDP(1)=SAVEINPT(1)
              WW1=YVHI
              WW2=0.0D0
              WW3=SYSTEM1(11)
              WVN=WW3
              MSG=.FALSE.
              WW4=1.0D0
              NOCOAT=.TRUE.
              GRASET=.TRUE.
              IF(CACOCHVIE.EQ.1) CACOCH=1
              CALL RAYTRA
              REST_KDP(1)=RESTINPT(1)
C
              SAVE_KDP(1)=SAVEINPT(1)
              IF(RAYEXT) CALL VIERAY(VIEW2,VIEW3,VDF2,VDF3,VS2,VS3)
              REST_KDP(1)=RESTINPT(1)
C     MINUS Y-RAY
              SAVE_KDP(1)=SAVEINPT(1)
              WW1=YVLO
              WW2=0.0D0
              WW3=SYSTEM1(11)
              WVN=WW3
              MSG=.FALSE.
              WW4=1.0D0
              NOCOAT=.TRUE.
              GRASET=.TRUE.
              IF(CACOCHVIE.EQ.1) CACOCH=1
              CALL RAYTRA
              REST_KDP(1)=RESTINPT(1)
C
              SAVE_KDP(1)=SAVEINPT(1)
              IF(RAYEXT) CALL VIERAY(VIEW2,VIEW3,VDF2,VDF3,VS2,VS3)
              REST_KDP(1)=RESTINPT(1)
              IF(VIEWQ.NE.'YZ') THEN
C     PLUS X-RAY
                  SAVE_KDP(1)=SAVEINPT(1)
                  WW1=0.0D0
                  WW2=XVHI
                  WW3=SYSTEM1(11)
                  WVN=WW3
                  MSG=.FALSE.
                  WW4=1.0D0
                  NOCOAT=.TRUE.
                  GRASET=.TRUE.
                  IF(CACOCHVIE.EQ.1) CACOCH=1
                  CALL RAYTRA
                  REST_KDP(1)=RESTINPT(1)
C
                  SAVE_KDP(1)=SAVEINPT(1)
                  IF(RAYEXT) CALL VIERAY(VIEW2,VIEW3,VDF2,VDF3,VS2,VS3)
                  REST_KDP(1)=RESTINPT(1)
C     MINUS X-RAY
                  SAVE_KDP(1)=SAVEINPT(1)
                  WW1=0.0D0
                  WW2=XVLO
                  WW3=SYSTEM1(11)
                  WVN=WW3
                  MSG=.FALSE.
                  WW4=1.0D0
                  NOCOAT=.TRUE.
                  GRASET=.TRUE.
                  IF(CACOCHVIE.EQ.1) CACOCH=1
                  CALL RAYTRA
                  REST_KDP(1)=RESTINPT(1)
C
                  SAVE_KDP(1)=SAVEINPT(1)
                  IF(RAYEXT) CALL VIERAY(VIEW2,VIEW3,VDF2,VDF3,VS2,VS3)
                  REST_KDP(1)=RESTINPT(1)
              END IF
          ELSE
C     NOT YZ PLANE FIELDS OF VIEW
          END IF
          IF(VIEWQ.EQ.'XZ'.OR.VIEWQ.EQ.'ORTHO'.OR.VIEWQ.EQ.'XY') THEN
C     DO XZ PLANE FIELDS
C
C     FIRST FOB 0
              SAVE_KDP(1)=SAVEINPT(1)
              INPUT='FOB'
              CALL PROCES
              REST_KDP(1)=RESTINPT(1)
              IF(.NOT.VIGOFF) THEN
                  CALL VIGCAL(10,VHI,VLO,2)
                  YVHI=VHI
                  YVLO=VLO
              ELSE
                  YVHI=1.0D0
                  YVLO=-1.0D0
              END IF
              IF(.NOT.VIGOFF) THEN
                  CALL VIGCAL(10,VHI,VLO,1)
                  XVHI=VHI
                  XVLO=VLO
              ELSE
                  XVHI=1.0D0
                  XVLO=-1.0D0
              END IF
C     FIRST MARGINAL RAYS
              IF(VIEWQ.NE.'XZ') THEN
C     PLUS Y-RAY
                  SAVE_KDP(1)=SAVEINPT(1)
                  WW1=YVHI
                  WW2=0.0D0
                  WW3=SYSTEM1(11)
                  WVN=WW3
                  MSG=.FALSE.
                  WW4=1.0D0
                  NOCOAT=.TRUE.
                  GRASET=.TRUE.
                  IF(CACOCHVIE.EQ.1) CACOCH=1
                  CALL RAYTRA
                  REST_KDP(1)=RESTINPT(1)
C
                  SAVE_KDP(1)=SAVEINPT(1)
                  IF(RAYEXT) CALL VIERAY(VIEW2,VIEW3,VDF2,VDF3,VS2,VS3)
                  REST_KDP(1)=RESTINPT(1)
C     MINUS Y-RAY
                  SAVE_KDP(1)=SAVEINPT(1)
                  WW1=YVLO
                  WW2=0.0D0
                  WW3=SYSTEM1(11)
                  WVN=WW3
                  MSG=.FALSE.
                  WW4=1.0D0
                  NOCOAT=.TRUE.
                  GRASET=.TRUE.
                  IF(CACOCHVIE.EQ.1) CACOCH=1
                  CALL RAYTRA
                  REST_KDP(1)=RESTINPT(1)
C
                  SAVE_KDP(1)=SAVEINPT(1)
                  IF(RAYEXT) CALL VIERAY(VIEW2,VIEW3,VDF2,VDF3,VS2,VS3)
                  REST_KDP(1)=RESTINPT(1)
              END IF
C     PLUS X-RAY
              SAVE_KDP(1)=SAVEINPT(1)
              WW1=0.0D0
              WW2=XVHI
              WW3=SYSTEM1(11)
              WVN=WW3
              MSG=.FALSE.
              WW4=1.0D0
              NOCOAT=.TRUE.
              GRASET=.TRUE.
              IF(CACOCHVIE.EQ.1) CACOCH=1
              CALL RAYTRA
              REST_KDP(1)=RESTINPT(1)
C
              SAVE_KDP(1)=SAVEINPT(1)
              IF(RAYEXT) CALL VIERAY(VIEW2,VIEW3,VDF2,VDF3,VS2,VS3)
              REST_KDP(1)=RESTINPT(1)
C     MINUS X-RAY
              SAVE_KDP(1)=SAVEINPT(1)
              WW1=0.0D0
              WW2=XVLO
              WW3=SYSTEM1(11)
              WVN=WW3
              MSG=.FALSE.
              WW4=1.0D0
              NOCOAT=.TRUE.
              GRASET=.TRUE.
              IF(CACOCHVIE.EQ.1) CACOCH=1
              CALL RAYTRA
              REST_KDP(1)=RESTINPT(1)
C
              SAVE_KDP(1)=SAVEINPT(1)
              IF(RAYEXT) CALL VIERAY(VIEW2,VIEW3,VDF2,VDF3,VS2,VS3)
              REST_KDP(1)=RESTINPT(1)
C     TOP X FOV
C     FIRST FOB 0 1
              SAVE_KDP(1)=SAVEINPT(1)
              INPUT='FOB 0 1'
              CALL PROCES
              REST_KDP(1)=RESTINPT(1)
              IF(.NOT.VIGOFF) THEN
                  CALL VIGCAL(10,VHI,VLO,1)
                  XVHI=VHI
                  XVLO=VLO
              ELSE
                  XVHI=1.0D0
                  XVLO=-1.0D0
              END IF
              IF(.NOT.VIGOFF) THEN
                  CALL VIGCAL(10,VHI,VLO,2)
                  YVHI=VHI
                  YVLO=VLO
              ELSE
                  YVHI=1.0D0
                  YVLO=-1.0D0
              END IF
C
C     FIRST MARGINAL RAYS
C     GUT RAY
              SAVE_KDP(1)=SAVEINPT(1)
              WW1=0.0D0
              WW2=0.0D0
              WW3=SYSTEM1(11)
              WVN=WW3
              MSG=.FALSE.
              WW4=1.0D0
              NOCOAT=.TRUE.
              GRASET=.TRUE.
              IF(CACOCHVIE.EQ.1) CACOCH=1
              CALL RAYTRA
              REST_KDP(1)=RESTINPT(1)
C
              SAVE_KDP(1)=SAVEINPT(1)
              IF(RAYEXT) CALL VIERAY(VIEW2,VIEW3,VDF2,VDF3,VS2,VS3)
              REST_KDP(1)=RESTINPT(1)
C     FIRST MARGINAL RAYS
              IF(VIEWQ.NE.'XZ') THEN
C     PLUS Y-RAY
                  SAVE_KDP(1)=SAVEINPT(1)
                  WW1=YVHI
                  WW2=0.0D0
                  WW3=SYSTEM1(11)
                  WVN=WW3
                  MSG=.FALSE.
                  WW4=1.0D0
                  NOCOAT=.TRUE.
                  GRASET=.TRUE.
                  IF(CACOCHVIE.EQ.1) CACOCH=1
                  CALL RAYTRA
                  REST_KDP(1)=RESTINPT(1)
C
                  SAVE_KDP(1)=SAVEINPT(1)
                  IF(RAYEXT) CALL VIERAY(VIEW2,VIEW3,VDF2,VDF3,VS2,VS3)
                  REST_KDP(1)=RESTINPT(1)
C     MINUS Y-RAY
                  SAVE_KDP(1)=SAVEINPT(1)
                  WW1=YVLO
                  WW2=0.0D0
                  WW3=SYSTEM1(11)
                  WVN=WW3
                  MSG=.FALSE.
                  WW4=1.0D0
                  NOCOAT=.TRUE.
                  GRASET=.TRUE.
                  IF(CACOCHVIE.EQ.1) CACOCH=1
                  CALL RAYTRA
                  REST_KDP(1)=RESTINPT(1)
C
                  SAVE_KDP(1)=SAVEINPT(1)
                  IF(RAYEXT) CALL VIERAY(VIEW2,VIEW3,VDF2,VDF3,VS2,VS3)
                  REST_KDP(1)=RESTINPT(1)
              END IF
C     PLUS X-RAY
              SAVE_KDP(1)=SAVEINPT(1)
              WW1=0.0D0
              WW2=XVHI
              WW3=SYSTEM1(11)
              WVN=WW3
              MSG=.FALSE.
              WW4=1.0D0
              NOCOAT=.TRUE.
              GRASET=.TRUE.
              IF(CACOCHVIE.EQ.1) CACOCH=1
              CALL RAYTRA
              REST_KDP(1)=RESTINPT(1)
C
              SAVE_KDP(1)=SAVEINPT(1)
              IF(RAYEXT) CALL VIERAY(VIEW2,VIEW3,VDF2,VDF3,VS2,VS3)
              REST_KDP(1)=RESTINPT(1)
C     MINUS X-RAY
              SAVE_KDP(1)=SAVEINPT(1)
              WW1=0.0D0
              WW2=XVLO
              WW3=SYSTEM1(11)
              WVN=WW3
              MSG=.FALSE.
              WW4=1.0D0
              NOCOAT=.TRUE.
              GRASET=.TRUE.
              IF(CACOCHVIE.EQ.1) CACOCH=1
              CALL RAYTRA
              REST_KDP(1)=RESTINPT(1)
C
              SAVE_KDP(1)=SAVEINPT(1)
              IF(RAYEXT) CALL VIERAY(VIEW2,VIEW3,VDF2,VDF3,VS2,VS3)
              REST_KDP(1)=RESTINPT(1)
C     BOTTOM X FOV
C     FOB 0 -1
              SAVE_KDP(1)=SAVEINPT(1)
              IF(VSYM) INPUT='FOB 0 -1'
              IF(.NOT.VSYM) INPUT='FOB 0 0.7'
              CALL PROCES
              REST_KDP(1)=RESTINPT(1)
              IF(.NOT.VIGOFF) THEN
                  CALL VIGCAL(10,VHI,VLO,1)
                  XVHI=VHI
                  XVLO=VLO
              ELSE
                  XVHI=1.0D0
                  XVLO=-1.0D0
              END IF
              IF(.NOT.VIGOFF) THEN
                  CALL VIGCAL(10,VHI,VLO,2)
                  YVHI=VHI
                  YVLO=VLO
              ELSE
                  YVHI=1.0D0
                  YVLO=-1.0D0
              END IF
C
C     FIRST MARGINAL RAYS
C     GUT RAY
              SAVE_KDP(1)=SAVEINPT(1)
              WW1=0.0D0
              WW2=0.0D0
              WW3=SYSTEM1(11)
              WVN=WW3
              MSG=.FALSE.
              WW4=1.0D0
              NOCOAT=.TRUE.
              GRASET=.TRUE.
              IF(CACOCHVIE.EQ.1) CACOCH=1
              CALL RAYTRA
              REST_KDP(1)=RESTINPT(1)
C
              SAVE_KDP(1)=SAVEINPT(1)
              IF(RAYEXT) CALL VIERAY(VIEW2,VIEW3,VDF2,VDF3,VS2,VS3)
              REST_KDP(1)=RESTINPT(1)
C
C     FIRST MARGINAL RAYS
              IF(VIEWQ.NE.'XZ') THEN
C     PLUS Y-RAY
                  SAVE_KDP(1)=SAVEINPT(1)
                  WW1=YVHI
                  WW2=0.0D0
                  WW3=SYSTEM1(11)
                  WVN=WW3
                  MSG=.FALSE.
                  WW4=1.0D0
                  NOCOAT=.TRUE.
                  GRASET=.TRUE.
                  IF(CACOCHVIE.EQ.1) CACOCH=1
                  CALL RAYTRA
                  REST_KDP(1)=RESTINPT(1)
C
                  SAVE_KDP(1)=SAVEINPT(1)
                  IF(RAYEXT) CALL VIERAY(VIEW2,VIEW3,VDF2,VDF3,VS2,VS3)
                  REST_KDP(1)=RESTINPT(1)

C     MINUS Y-RAY
                  SAVE_KDP(1)=SAVEINPT(1)
                  WW1=YVLO
                  WW2=0.0D0
                  WW3=SYSTEM1(11)
                  WVN=WW3
                  MSG=.FALSE.
                  WW4=1.0D0
                  NOCOAT=.TRUE.
                  GRASET=.TRUE.
                  IF(CACOCHVIE.EQ.1) CACOCH=1
                  CALL RAYTRA
                  REST_KDP(1)=RESTINPT(1)
C
                  SAVE_KDP(1)=SAVEINPT(1)
                  IF(RAYEXT) CALL VIERAY(VIEW2,VIEW3,VDF2,VDF3,VS2,VS3)
                  REST_KDP(1)=RESTINPT(1)
              END IF
C     PLUS X-RAY
              SAVE_KDP(1)=SAVEINPT(1)
              WW1=0.0D0
              WW2=XVHI
              WW3=SYSTEM1(11)
              WVN=WW3
              MSG=.FALSE.
              WW4=1.0D0
              NOCOAT=.TRUE.
              GRASET=.TRUE.
              IF(CACOCHVIE.EQ.1) CACOCH=1
              CALL RAYTRA
              REST_KDP(1)=RESTINPT(1)
C
              SAVE_KDP(1)=SAVEINPT(1)
              IF(RAYEXT) CALL VIERAY(VIEW2,VIEW3,VDF2,VDF3,VS2,VS3)
              REST_KDP(1)=RESTINPT(1)
C     MINUS X-RAY
              SAVE_KDP(1)=SAVEINPT(1)
              WW1=0.0D0
              WW2=XVLO
              WW3=SYSTEM1(11)
              WVN=WW3
              MSG=.FALSE.
              WW4=1.0D0
              NOCOAT=.TRUE.
              GRASET=.TRUE.
              IF(CACOCHVIE.EQ.1) CACOCH=1
              CALL RAYTRA
              REST_KDP(1)=RESTINPT(1)
C
              SAVE_KDP(1)=SAVEINPT(1)
              IF(RAYEXT) CALL VIERAY(VIEW2,VIEW3,VDF2,VDF3,VS2,VS3)
              REST_KDP(1)=RESTINPT(1)
          ELSE
C     NOT YZ PLANE FIELDS
          END IF

C     NOW CLAPS,PROFS AND EDGES
          IF(VIEWQ.EQ.'YZ'.OR.VIEWQ.EQ.'ORTHO'.OR.VIEWQ.EQ.'XY') THEN
C     PLOT PROFY
              SAVE_KDP(1)=SAVEINPT(1)
C             PLOT PROFY
              W1=VIEW2
              W2=VIEW3
              W3=90.0D0
              S1=VS2
              S2=VS3
              S3=1
              S4=0
              S5=0
              SN=1
              DF1=VDF2
              DF2=VDF3
              DF3=0
              DF4=1
              DF5=1
              WQ='PROF'
              SQ=1
              STI=0
              SST=0
              CALL PLTPRO1
              REST_KDP(1)=RESTINPT(1)
          END IF
          IF(VIEWQ.EQ.'YZ'.OR.VIEWQ.EQ.'ORTHO'.OR.VIEWQ.EQ.'XY') THEN
C     PLOT EDGEY
              SAVE_KDP(1)=SAVEINPT(1)
C             PLOT EDGEY
              W1=VIEW2
              W2=VIEW3
              S1=VS2
              S2=VS3
              S3=0
              S4=0
              S5=0
              SN=1
              DF1=VDF2
              DF2=VDF3
              DF3=1
              DF4=1
              DF5=1
              WQ='EDGEY'
              SQ=1
              STI=0
              SST=0
              CALL PLTEDG
              REST_KDP(1)=RESTINPT(1)
          END IF
C
          IF(VIEWQ.EQ.'XZ'.OR.VIEWQ.EQ.'ORTHO'.OR.VIEWQ.EQ.'XY') THEN
C     PLOT PROFX
              SAVE_KDP(1)=SAVEINPT(1)
C             PLOT PROFX
              W1=VIEW2
              W2=VIEW3
              W3=0.0D0
              S1=VS2
              S2=VS3
              S3=1
              S4=0
              S5=0
              SN=1
              DF1=VDF2
              DF2=VDF3
              DF3=0
              DF4=1
              DF5=1
              WQ='PROF'
              SQ=1
              STI=0
              SST=0
              CALL PLTPRO1
              REST_KDP(1)=RESTINPT(1)
          END IF
          IF(VIEWQ.EQ.'XZ'.OR.VIEWQ.EQ.'ORTHO'.OR.VIEWQ.EQ.'XY') THEN
C     PLOT EDGEX
              SAVE_KDP(1)=SAVEINPT(1)
C             PLOT EDGEX
              W1=VIEW2
              W2=VIEW3
              S1=VS2
              S2=VS3
              S3=0
              S4=0
              S5=0
              SN=1
              DF1=VDF2
              DF2=VDF3
              DF3=1
              DF4=1
              DF5=1
              WQ='EDGEX'
              SQ=1
              STI=0
              SST=0
              CALL PLTEDG
              REST_KDP(1)=RESTINPT(1)
          END IF
C
          IF(VIEWQ.EQ.'XY'.OR.VIEWQ.EQ.'ORTHO') THEN
C     PLOT CLAP
              SAVE_KDP(1)=SAVEINPT(1)
C             PLOT CLAP
              W1=VIEW2
              W2=VIEW3
              S1=VS2
              S2=VS3
              S3=0
              S4=0
              S5=0
              SN=1
              DF1=VDF2
              DF2=VDF3
              DF3=1
              DF4=1
              DF5=1
              WQ='CLAP'
              SQ=1
              STI=0
              SST=0
              SFI=1.0D0
              DO I=INT(W1),INT(W2)
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
              REST_KDP(1)=RESTINPT(1)
          END IF
C     PLOT SCALE FACTOR
C     DO A FRAME
          COLPAS=COLLBL
          CALL MY_COLTYP(COLPAS)
          CALL DOSZ

          call setthreecolors2
C
          IF(DFLAG.EQ.0) THEN
              SAVE_KDP(1)=SAVEINPT(1)
              INPUT='DRAW'
              CALL PROCES
              REST_KDP(1)=RESTINPT(1)
          END IF

          RETURN
      END


C SUB INSIDEIT.FOR

      FUNCTION INSIDEIT(I,XONE,YONE,XTWO,YTWO)
C
          IMPLICIT NONE
C
          EXTERNAL INSID1,INSID2
C
          INTEGER I,COFLG,N,ITIIT,III
C
          REAL*8 X,Y,XONE,YONE,XTWO,YTWO,ANGLE,
     1    XR,YR,LS,RS,XRD,YRD,X1,X2,X3,X4,Y1,Y2,Y3,Y4,
     2    X5,X6,X7,X8,Y5,Y6,Y7,Y8,XC1,XC2,XC3,XC4,YC1,YC2,
     3    A22,YC3,YC4,RAD2,MAXSID,CS1,CS2,CS3,CS4
C
          LOGICAL INSIDEIT,INS,INSID1,INSID2,INITIT(1:2)
C
          INTEGER CAERAS,COERAS
C
          COMMON/CACO/CAERAS,COERAS,LS
C
C     VARIABLES FOR SPOT TRACING
C
          LOGICAL SPDTRA
C
          COMMON/SPRA1/SPDTRA
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
C
C       I IS THE CURRENT SURFACE NUMBER
C
C       THIS ROUTINE SETS THE FLAGS
C       CAERAS AND COERAS ARE
C       FLAGS SET IF THE NEXT CURRENT SURFACE HAS A COBS OR
C       CLAP ERASE.
C
          IF(ALENS(16,I).EQ.0.0D0) THEN
              COFLG=0
C
C       NO CLAPS OR COBS, JUST RETURN
              INSIDEIT=.FALSE.
              RETURN
          ELSE
              COFLG=INT(ALENS(16,I))
C       THERE IS COBS, PROCEED CHECKING
          END IF
C
          INSIDEIT=.FALSE.
C
          DO ITIIT=1,2
              INITIT(ITIIT)=.FALSE.
              IF(ITIIT.EQ.1) THEN
                  X=XONE
                  Y=YONE
              ELSE
                  X=XTWO
                  Y=YTWO
              END IF
C
C******************************************************************
C       COFLG NON-ZERO, RESOLVE ALL COBS BLOCKAGES NOW
C******************************************************************
C
C       COFLG=1 CIRCULAR COBS, DOES IT STOP THE RAY
              IF(COFLG.EQ.1) THEN
C
                  LS=0.0D0
C       CIRCULAR COBS EQUATION IS:
C
C       (X)**2 + (Y)**2 = R**2
C
C       SUB X AND Y IN THE LEFT SIDE. IF THE LEFT SIDE IS
C       LESS THAN OR EQUAL TO THE RIGHT SIDE, THE RAY IS BLOCKED
C       ELSE IT IS NOT BLOCKED.
C
C       IF NON-ZERO COBS DECENTRATIONS EXISTS, THEY MUST BE CONSIDERED
C       IN COBS CHECKING. WE CONVERT THE RAY COORDINATES X AND Y
C       TO XR AND YR IN THE COORDINATE SYSTEM OF THE DECENTERED
C       OBSCURATION. REMEMBER.
C
                  XR=X-ALENS(20,I)
                  YR=Y-ALENS(19,I)
C
                  LS=DSQRT((XR**2)+(YR**2))
C
                  RS=DSQRT(ALENS(17,I)**2)-AIMTOL
                  IF(REAL(LS).LT.REAL(RS)) THEN
C       RAY IS BLOCKED BY COBS ON SURFACE I
                      LS=10.0D0
                      INITIT(ITIIT)=.TRUE.
                  ELSE
                      LS=0.0D0
                      INITIT(ITIIT)=.FALSE.
                  END IF
C       COFLG NOT 1
              END IF
C
C       COFLG=2 RECTANGULAR OBSCURATION, DOES IT STOP THE RAY
              IF(COFLG.EQ.2) THEN
C
                  LS=0.0D0
                  INITIT(ITIIT)=.FALSE.
C
C       IN THE COORDINATE SYSTEM OF THE RECTANGLE, THE CORNER
C       COORDINATES ARE:
                  X1=-ALENS(18,I)-AIMTOL
                  Y1=ALENS(17,I)+AIMTOL
                  X2=-ALENS(18,I)-AIMTOL
                  Y2=-ALENS(17,I)-AIMTOL
                  X3=ALENS(18,I)+AIMTOL
                  Y3=-ALENS(17,I)-AIMTOL
                  X4=ALENS(18,I)+AIMTOL
                  Y4=ALENS(17,I)+AIMTOL
C
                  XRD=X
                  YRD=Y

C       IF NON-ZERO COBS DECENTRATIONS EXISTS, THEY MUST BE CONSIDERED
C       IN COBS CHECKING. WE CONVERT THE RAY COORDINATES X AND Y
C       TO XRD AND YRD IN THE COORDINATE SYSTEM OF THE DECENTERED
C       OBSCURATION. REMEMBER.
C
                  XRD=XRD-ALENS(20,I)
                  YRD=YRD-ALENS(19,I)
C
C       IF A NON-ZERO COBS TILT EXISTS, IT MUST BE CONSIDERED
C       IN COBS CHECKING. WE CONVERT THE RAY COORDINATES XRD AND YRD
C       TO XR AND YR IN THE COORDINATE SYSTEM OF THE ROTATED
C       OBSCURATION. REMEMBER. GAMMA ROTATION OF A CLAP OR
C       COBS HAS THE SAME SIGN AS IN A SURFACE ROTATION

                  A22=ALENS(22,I)*PII/180.0D0
                  XR=(XRD*DCOS(A22))+(YRD*DSIN(A22))
                  YR=(YRD*DCOS(A22))-(XRD*DSIN(A22))
C
C
C
C       ARE THE POINTS XR AND YR ON THE RECTANGLE OR OUTSIDE
C       IF YES, SET LS=10.0D0
C       IF NO SET LS=0.0D0
                  XT(1)=X1
                  YT(1)=Y1
                  XT(2)=X2
                  YT(2)=Y2
                  XT(3)=X3
                  YT(3)=Y3
                  XT(4)=X4
                  YT(4)=Y4
                  NP=4
                  X0=XR
                  Y0=YR
                  INS=INSID2()
                  IF(INS) THEN
C       RAY  BLOCKED
                      LS=10.0D0
                      INITIT(ITIIT)=.TRUE.
                  ELSE
                      LS=0.0D0
                      INITIT(ITIIT)=.FALSE.
C       RAY NOT BLOCKED
                  END IF
C
C       COFLG NOT 2
              END IF
C
C       COFLG=3 ELLIPTICAL COBS, DOES IT STOP THE RAY
              IF(COFLG.EQ.3) THEN
C
                  LS=0.0D0
C       ELLIPTICAL COBS EQUATION IS:
C
C       (X)**2/A**2  + (Y)**2/B**2 = 1
C
C       SUB X AND Y IN THE LEFT SIDE. IF THE LEFT SIDE IS
C       LESS THAN 1.0D0, THE RAY IS BLOCKED
C       ELSE IT IS NOT BLOCKED.
C
                  XRD=X
                  YRD=Y

C       IF NON-ZERO COBS DECENTRATIONS EXISTS, THEY MUST BE CONSIDERED
C       IN COBS CHECKING. WE CONVERT THE RAY COORDINATES X AND Y
C       TO XRD AND YRD IN THE COORDINATE SYSTEM OF THE DECENTERED
C       OBSCURATION. REMEMBER.
C
                  XRD=XRD-ALENS(20,I)
                  YRD=YRD-ALENS(19,I)
C
C       IF A NON-ZERO COBS TILT EXISTS, IT MUST BE CONSIDERED
C       IN COBS CHECKING. WE CONVERT THE RAY COORDINATES XRD AND YRD
C       TO XR AND YR IN THE COORDINATE SYSTEM OF THE ROTATED
C       OBSCURATION. REMEMBER. GAMMA ROTATION OF A CLAP OR
C       COBS HAS THE SAME SIGN AS IN A SURFACE ROTATION

                  A22=ALENS(22,I)*PII/180.0D0
                  XR=(XRD*DCOS(A22))+(YRD*DSIN(A22))
                  YR=(YRD*DCOS(A22))-(XRD*DSIN(A22))
C
C
                  LS=((XR**2)/(ALENS(18,I)**2))+
     1               ((YR**2)/(ALENS(17,I)**2))
C
                  IF(REAL(LS).LT.(1.0-(AIMTOL**2))) THEN
C       RAY IS BLOCKED BY COBS ON SURFACE I
                      LS=10.0D0
                      INITIT(ITIIT)=.TRUE.
                  ELSE
                      LS=0.0D0
                      INITIT(ITIIT)=.FALSE.
                  END IF
C       COFLG NOT 3
              END IF
C
C
C       COFLG=4 RACETRACK COBS, DOES IT STOP THE RAY
              IF(COFLG.EQ.4) THEN
C
                  LS=0.0D0
C
C       IF THE RADIUS OF THE CORNERS IS LESS THAN THE LARGER
C       OF THE TWO SIDE DIMENSIONS, AN 8 SIDED POLYGON NEEDS TO BE
C       CHECKED AS WELL AS THE FOUR CIRCLES. IF NOT, THE BASE RECTANGE
C       AND THE FOUR CIRCLES NEED TO BE CHECKED.
C
                  IF(ALENS(17,I).LE.ALENS(18,I)) THEN
C       ALENS(18,I) = MAXSID
                      MAXSID=ALENS(18,I)
                  ELSE
                      MAXSID=ALENS(17,I)
                  END IF
                  IF(ALENS(21,I).LT.MAXSID) THEN
C       SETUP THE 8 SIDED BOX
                      N=8
                      X1=-ALENS(18,I)+ALENS(21,I)+AIMTOL
                      Y1=ALENS(17,I)-AIMTOL
                      X2=-ALENS(18,I)+AIMTOL
                      Y2=ALENS(17,I)-ALENS(21,I)-AIMTOL
                      X3=-ALENS(18,I)+AIMTOL
                      Y3=-ALENS(17,I)+ALENS(21,I)+AIMTOL
                      X4=-ALENS(18,I)+ALENS(21,I)+AIMTOL
                      Y4=-ALENS(17,I)+AIMTOL
                      X5=ALENS(18,I)-ALENS(21,I)-AIMTOL
                      Y5=-ALENS(17,I)+AIMTOL
                      X6=ALENS(18,I)-AIMTOL
                      Y6=-ALENS(17,I)+ALENS(21,I)+AIMTOL
                      X7=ALENS(18,I)-AIMTOL
                      Y7=ALENS(17,I)-ALENS(21,I)-AIMTOL
                      X8=ALENS(18,I)-ALENS(21,I)-AIMTOL
                      Y8=ALENS(17,I)-AIMTOL
                  ELSE
C       SET UP THE FOUR SIDED BOX
                      N=4
                      X1=-ALENS(18,I)-AIMTOL
                      Y1=ALENS(17,I)+AIMTOL
                      X2=-ALENS(18,I)-AIMTOL
                      Y2=-ALENS(17,I)-AIMTOL
                      X3=ALENS(18,I)+AIMTOL
                      Y3=-ALENS(17,I)-AIMTOL
                      X4=ALENS(18,I)+AIMTOL
                      Y4=ALENS(17,I)+AIMTOL
                  END IF
C
                  XRD=X
                  YRD=Y
C
C       IF NON-ZERO COBS DECENTRATIONS EXISTS, THEY MUST BE CONSIDERED
C       IN COBS CHECKING. WE CONVERT THE RAY COORDINATES X AND Y
C       TO XRD AND YRD IN THE COORDINATE SYSTEM OF THE DECENTERED
C       OBSCURATION. REMEMBER.
C
                  XRD=XRD-ALENS(20,I)
                  YRD=YRD-ALENS(19,I)
C
C       IF A NON-ZERO COBS TILT EXISTS, IT MUST BE CONSIDERED
C       IN COBS CHECKING. WE CONVERT THE RAY COORDINATES XRD AND YRD
C       TO XR AND YR IN THE COORDINATE SYSTEM OF THE ROTATED
C       OBSCURATION. REMEMBER. GAMMA ROTATION OF A CLAP OR
C       COBS HAS THE SAME SIGN AS IN A SURFACE ROTATION

                  A22=ALENS(22,I)*PII/180.0D0
                  XR=(XRD*DCOS(A22))+(YRD*DSIN(A22))
                  YR=(YRD*DCOS(A22))-(XRD*DSIN(A22))
C
C       ARE THE POINTS XR AND YR ON THE POLYGON OR OUTSIDE
C       IF YES, SET LS=10.0D0
C       IF NO SET LS=0.0D0
                  IF(N.EQ.4) THEN
                      XT(1)=X1
                      YT(1)=Y1
                      XT(2)=X2
                      YT(2)=Y2
                      XT(3)=X3
                      YT(3)=Y3
                      XT(4)=X4
                      YT(4)=Y4
                  ELSE
                      XT(1)=X1
                      YT(1)=Y1
                      XT(2)=X2
                      YT(2)=Y2
                      XT(3)=X3
                      YT(3)=Y3
                      XT(4)=X4
                      YT(4)=Y4
                      XT(5)=X5
                      YT(5)=Y5
                      XT(6)=X6
                      YT(6)=Y6
                      XT(7)=X7
                      YT(7)=Y7
                      XT(8)=X8
                      YT(8)=Y8
                  END IF
                  NP=N
                  X0=XR
                  Y0=YR
                  INS=INSID2()
                  IF(INS) THEN
C       RAY BLOCKED
                      LS=10.0D0
                      INITIT(ITIIT)=.TRUE.
                  ELSE
                      LS=0.0D0
                      INITIT(ITIIT)=.FALSE.
C       RAY NOT BLOCKED
                  END IF
C NOW IS THE POINT OUTSIDE OR ON ANY OF THE FOUR CIRCLES
C       CENTER OF THE FIRST CIRCLE IS AT
                  XC1=-ALENS(18,I)+ALENS(21,I)
                  YC1= ALENS(17,I)-ALENS(21,I)
                  CS1=DSQRT(((XR-XC1)**2)+((YR-YC1)**2))
C       CENTER OF THE SECOND CIRCLE IS AT
                  XC2= -ALENS(18,I)+ALENS(21,I)
                  YC2= -ALENS(17,I)+ALENS(21,I)
                  CS2=DSQRT(((XR-XC2)**2)+((YR-YC2)**2))
C       CENTER OF THE THIRD CIRCLE IS AT
                  XC3= ALENS(18,I)-ALENS(21,I)
                  YC3=-ALENS(17,I)+ALENS(21,I)
                  CS3=DSQRT(((XR-XC3)**2)+((YR-YC3)**2))
C       CENTER OF THE FIRST CIRCLE IS AT
                  XC4=ALENS(18,I)-ALENS(21,I)
                  YC4=ALENS(17,I)-ALENS(21,I)
                  CS4=DSQRT(((XR-XC4)**2)+((YR-YC4)**2))
C
                  RAD2=DSQRT(ALENS(21,I)**2)-AIMTOL
C
C     RAY BLOCKED BY THE BOX AND CIRCLES
                  IF(INS.OR.
     1            REAL(CS1).LT.REAL(RAD2).OR.REAL(CS2).LT.REAL(RAD2)
     1            .OR.REAL(CS3).LT.REAL(RAD2).OR.REAL(CS4).LT.REAL(RAD2))
     1                              THEN
C       RAY IS BLOCKED BY ONE OF THE CIRCLES
                      LS=10.0D0
                      INITIT(ITIIT)=.TRUE.
                  ELSE
                      LS=0.0D0
                      INITIT(ITIIT)=.FALSE.
                  END IF
C       COFLG NOT 4
              END IF
C
C       COFLG=5 POLY CLAP, DOES IT STOP THE RAY
              IF(COFLG.EQ.5) THEN
                  LS=0.0D0
C
C       IN THE COORDINATE SYSTEM OF THE POLYGON, THE CORNER
C       NUMBER OF POINTS IS ALENS(18,I), CENTER TO CORNER DISTANCE
C       IS ALENS(17,I). POINTS GO COUNTER CLOCKWISE LOOKING
C       TOWARD THE +Z DIRECTION
C       COORDINATES ARE:
                  ANGLE=0.0D0
                  DO III=1,INT(ALENS(18,I))
                      XT(III)=ALENS(17,I)*DCOS(ANGLE+(PII/2.0D0))
                      YT(III)=ALENS(17,I)*DSIN(ANGLE+(PII/2.0D0))
                      ANGLE=ANGLE+((TWOPII)/ALENS(18,I))
                  END DO
                  XRD=X
                  YRD=Y
C
C       IF A NON-ZERO COBS TILT EXISTS, IT MUST BE CONSIDERED
C       IN COBS CHECKING. WE CONVERT THE RAY COORDINATES XRD AND YRD
C       TO XR AND YR IN THE COORDINATE SYSTEM OF THE ROTATED

                  A22=ALENS(22,I)*PII/180.0D0
                  XR=(XRD*DCOS(A22))+(YRD*DSIN(A22))
                  YR=(YRD*DCOS(A22))-(XRD*DSIN(A22))
C
C       IF NON-ZERO CLAP ERASE DECENTRATIONS EXISTS, THEY MUST BE CONSIDERED
C       IN CLAP ERASE CHECKING. WE CONVERT THE RAY COORDINATES X AND Y
C       TO XRD AND YRD IN THE COORDINATE SYSTEM OF THE DECENTERED
C       CLEAR APERTURE ERASE. REMEMBER.
C
                  XR=XR-ALENS(20,I)
                  YR=YR-ALENS(19,I)
C
C       ARE THE POINTS XR AND YR ON THE RECTANGLE OR OUTSIDE
                  X0=XR
                  Y0=YR
                  NP=INT(ALENS(18,I))
                  INS=INSID2()
                  IF(INS) THEN
C       RAY BLOCKED
                      INITIT(ITIIT)=.TRUE.
                      LS=10.0D0
                  ELSE
C       RAY NOT BLOCKED
                      INITIT(ITIIT)=.FALSE.
                      LS=0.0D0
                  END IF
C
              ELSE
C       COFLG NOT 5
              END IF
          END DO
          INSIDEIT=.FALSE.
          IF(INITIT(1).AND.INITIT(2))INSIDEIT=.TRUE.

          RETURN
      END

C SUB PLTPRAE.FOR
      SUBROUTINE PLTPRAE(K)
          USE GLOBALS
C
          IMPLICIT NONE
C
C       THIS ROUTINE DOES THE PLOT PRAY COMMAND AT THE CMD LEVEL
C
          REAL*8 X,Y,Z,XN,YN,ZN,ROT1X,ROT1Z,ROT2Y,
     1    ROT2Z,AX,AY,AZ,AALF,APHI,YMAXI,YMINI,XMAXI,XMINI
     2    ,XNEW,YNEW,LKG,VIEPH,VIEAL
C
          LOGICAL GGO
C
          INTEGER IX,IY,I,IPST,COLPAS,STOPAT,K
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
C     LNTYPE=0
          LNTYPE=0
C
          X=0.0D0
          Y=0.0D0
C
C     DOES A RAY EXIST
          IF(.NOT.GPRAYEXT) THEN
              IF(MSG) THEN
                  OUTLYNE='ERROR'
                  CALL SHOWIT(1)
                  OUTLYNE='NO GENERALIZED PARAXIAL RAYS EXISTS'
                  CALL SHOWIT(1)
                  OUTLYNE='NO GENERALIZED PARAXIAL RAYS EXISTS TO BE PLOTTED'
                  CALL SHOWIT(1)
              END IF
              IF(F34.NE.1) CALL MACFAL
              RETURN
          END IF
          IF(RAYCOD(1).EQ.0)  STOPAT=NEWIMG
          IF(RAYCOD(1).EQ.1)  STOPAT=RAYCOD(2)-1
          IF(RAYCOD(1).EQ.2)  STOPAT=RAYCOD(2)-1
          IF(RAYCOD(1).EQ.3)  STOPAT=0
          IF(RAYCOD(1).EQ.4)  STOPAT=RAYCOD(2)
          IF(RAYCOD(1).EQ.5)  STOPAT=RAYCOD(2)
          IF(RAYCOD(1).EQ.6)  STOPAT=RAYCOD(2)
          IF(RAYCOD(1).EQ.7)  STOPAT=RAYCOD(2)
          IF(RAYCOD(1).EQ.8)  STOPAT=0
          IF(RAYCOD(1).EQ.9)  STOPAT=RAYCOD(2)
          IF(RAYCOD(1).EQ.10) STOPAT=0
          IF(RAYCOD(1).EQ.11) STOPAT=0
          IF(RAYCOD(1).EQ.12) STOPAT=0
          IF(RAYCOD(1).EQ.13) STOPAT=RAYCOD(2)-1
          IF(RAYCOD(1).EQ.14) STOPAT=RAYCOD(2)
          IF(RAYCOD(1).EQ.15) STOPAT=0
          IF(RAYCOD(1).EQ.16) STOPAT=0
          IF(RAYCOD(1).EQ.17) STOPAT=0
          IF(RAYCOD(1).EQ.18) STOPAT=RAYCOD(2)
          VIEPH=(PII/180.0D0)*VIEPHI
          VIEAL=(PII/180.0D0)*VIEALF
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
          GLOBE=.TRUE.
          OFFX=0.0D0
          OFFY=0.0D0
          OFFZ=0.0D0
          OFFA=0.0D0
          OFFB=0.0D0
          OFFC=0.0D0
          CALL GLVERT
          CALL PGLPRY
          GLOBE=.FALSE.
          IF(MSG) THEN
              OUTLYNE='GENERATING PARAXIAL RAY PLOTTING DATA...'
              CALL SHOWIT(1)
          END IF
C       CHECK SYNTAX
          IF(SST.EQ.1) THEN
              OUTLYNE=
     1        '"PLOT PRAY" TAKES NO STRING INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
              OUTLYNE=
     1        '"PLOT "'//WQ//' ONLY TAKES NUMERIC WORDS #1 AND #2 INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(STI.EQ.1) THEN
              WRITE(OUTLYNE,800) WQ
              CALL SHOWIT(1)
 800          FORMAT('QUERRY (?) HAS NO MEANING WITH "PLOT"',A8)
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
          IF(DF2.EQ.1) THEN
              IF(DABS(ALENS(3,(NEWIMG-1))).GT.1.0D10) THEN
                  W2=DBLE(NEWIMG-1)
              ELSE
                  W2=DBLE(NEWIMG)
              END IF
          ELSE
C       DF2 NOT 1, W2 EXPLICITLY ENTERED
          END IF
          STASUR=INT(W1)
          STPSUR=INT(W2)
          IF(STASUR.LT.NEWOBJ) STASUR=NEWOBJ
          IF(STPSUR.LT.NEWOBJ) STPSUR=NEWOBJ
C
          IF(INT(W1).LT.0) THEN
C       INVALID NUMERIC WORD #1
              OUTLYNE=
     1        'SURFACE NUMBER (NUMERIC WORD #1) IS BEYOND LEGAL RANGE'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(INT(W2).GT.NEWIMG) THEN
C       INVALID NUMERIC WORD #2
              OUTLYNE=
     1        'SURFACE NUMBER (NUMERIC WORD #2) IS BEYOND LEGAL RANGE'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(INT(W2).LE.INT(W1)) THEN
C       W2 LESS THAN OR EQUAL TO W1
              OUTLYNE=
     1        'NUMERIC WORD #2 MUST BE GREATER THAN NUMERIC WORD #1'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C       ALL INPUT IS OK, KEEP GOING
C       NOW WE HAVE THE REAL WORLD COORDINATES OF THE RAY TO BE PLOTTED
C       THEY ARE STORED IN THE ARRAY GLBRAY PASSED IN A COMMON BLOCK
C       VIA THE "INCLUDE 'datlen.inc'" STATEMENT.
C
C     HERE IS WHERE REAL WORK BEGINS. FIRST PROJECT THE 3D WORLD COORDINATES
C     OF THE VERTICIES ONTO THE 2D PLANE INDICATED BY THE PLOT LOOK/ PLOT VIEW
C     DATA. THE DIRECTION COSINES OF THE LOOK VECTOR ARE LOOKX, LOOKY AND
C     LOOKZ. THE TRANSFORMATIONS ARE:
C
          CALL ROT1
C
C     AFTER THIS TRANSFORMATION, THE WORLD X COORDINATE IS PLOTTED IN THE
C     HORIZONTAL X-AXIS OF THE DISPLAY AND THE Y WORLD COORDINATE IS PLOTTED
C     IN THE VERTICAL Y-AXIS OF THE DISPLAY AFTER A CONVERSION TO
C     DEVICE INDEPENDENT COORDINATES ARE MADE.
C
          CALL PLTSC1(XMINI,XMAXI,YMINI,YMAXI)
C
C     HERE IS THE LOOP WHICH PLOTS THE RAYS USING CALLS TO
C     PENMV1A(IX,IY,IPST)
c
          IF(STPSUR.GE.STOPAT) STPSUR=STOPAT
          IF(STASUR.LT.NEWOBJ) STASUR=NEWOBJ
          IF(STPSUR.LT.NEWOBJ) STPSUR=NEWOBJ
          IF(STPSUR.LT.STASUR) STPSUR=STASUR
          DO I=STASUR,STPSUR
              IF(K.EQ.1) THEN
                  X=PGLPRAY(1,I)+REFRY(1,I)
                  Y=PGLPRAY(2,I)+REFRY(2,I)
                  Z=PGLPRAY(3,I)+REFRY(3,I)
              END IF
              IF(K.EQ.2) THEN
                  X=-PGLPRAY(1,I)+REFRY(1,I)
                  Y=-PGLPRAY(2,I)+REFRY(2,I)
                  Z=PGLPRAY(3,I)+REFRY(3,I)
              END IF
              IF(K.EQ.3) THEN
                  X=PGLPRAY(4,I)+REFRY(1,I)
                  Y=PGLPRAY(5,I)+REFRY(2,I)
                  Z=PGLPRAY(6,I)+REFRY(3,I)
              END IF
              IF(K.EQ.4) THEN
                  X=-PGLPRAY(4,I)+REFRY(1,I)
                  Y=-PGLPRAY(5,I)+REFRY(2,I)
                  Z=PGLPRAY(6,I)+REFRY(3,I)
              END IF
              IF(K.EQ.5) THEN
                  X=PGLPRAY(7,I)+REFRY(1,I)
                  Y=PGLPRAY(8,I)+REFRY(2,I)
                  Z=PGLPRAY(9,I)+REFRY(3,I)
              END IF
              IF(K.EQ.6) THEN
                  X=-PGLPRAY(7,I)+REFRY(1,I)
                  Y=-PGLPRAY(8,I)+REFRY(2,I)
                  Z=PGLPRAY(9,I)+REFRY(3,I)
              END IF
              IF(K.EQ.7) THEN
                  X=PGLPRAY(10,I)+REFRY(1,I)
                  Y=PGLPRAY(11,I)+REFRY(2,I)
                  Z=PGLPRAY(12,I)+REFRY(3,I)
              END IF
              IF(K.EQ.8) THEN
                  X=-PGLPRAY(10,I)+REFRY(1,I)
                  Y=-PGLPRAY(11,I)+REFRY(2,I)
                  Z=PGLPRAY(12,I)+REFRY(3,I)
              END IF
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
              Y=(Y/SCFAY)*1000.0D0
C
              X=(X/SCFAX)*1000.0D0
C
C     STEP 2: APPLY THE SCREEN Y-OFFSET AND SCREEN GAMMA ROTATION
C
C     APPLY THE PLOT LOOK/VIEW Y-OFFSET VALUE
C
              IF(LORIENT) CALL ORSHIFT
              Y=Y+3500.0D0+DBLE(PYSHFT)
              X=X+DBLE(PXSHFT)
C
C     IF THERE ARE X-OFFSETS (JUSTIFICATION) TO APPLY, DO THEM HERE.
C
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
              X=X+JUSOFF
C
C     IF THERE IS A NON-ZERO GAMMA SPECIFIED IN PLOT LOOK OR PLOT VIEW THEN
C     ROTATE IN GAMMA ON THE SCREEN ABOUT THE CENTER OF THE SCREEN
C     WHICH HAS COORDINATES X=5000.0D0,Y=3500.0D0
C
C     FIRST SHIFT THE COORDINATE ORIGIN TO THE CETER OF THE DISPLAY
C
              X=X-5000.0D0
              Y=Y-3500.0D0
C     THE SCREEN COORDINATE IN REAL*8 IN THE SHIFTED COORDINATE
C     FRAME IS NOW X AND Y

              IF(DBLE(PGAMMA).NE.0.0D0) THEN
                  LKG=(PII/180.0D0)*DBLE(PGAMMA)
                  XNEW=((X*DCOS(LKG))-(Y*DSIN(LKG)))
                  YNEW=((X*DSIN(LKG))+(Y*DCOS(LKG)))
                  X=XNEW
                  Y=YNEW
              ELSE
              END IF

C     THE ROTATION IS DONE, NOW SHIFT THE ORIGIN BACK TO THE BOTTOM
C     LEFT HAND CORNER
              X=X+5000.0D0
              Y=Y+3500.0D0
              IF(I.EQ.STASUR) THEN
C     PUT INSTRUCTIONS IN P1ARAY TO LIFT PEN, GO TO STARTING POINT
                  IPST=0
                  IX=INT(X)
                  IY=INT(Y)
C
                  P1ARAY(I,1,1)=IX
                  P1ARAY(I,2,1)=IY
                  P1ARAY(I,3,1)=0
                  IF(.NOT.PLEXIS) PLEXIS=.TRUE.
              ELSE
              END IF
              IF(I.GT.STASUR) THEN
C     PUT INSTRUCTIONS IN P1ARAY TO DROP PEN AND DRAW
                  IPST=1
                  IX=INT(X)
                  IY=INT(Y)
C     IF RAY IS VIRTUAL AND NOVIRT IS TRUE, LIFT PEN ANYWAY
                  IF(NOVIRT.AND.GLVIRT(I).AND.I.NE.STPSUR.AND.DF3.EQ.1) THEN
C     LEAVE PEN ALONE AT LAST POSITION WITH PEN UP
                      IPST=0
                      IF(I.NE.0) P1ARAY(I,1,1)=P1ARAY(I-1,1,1)
                      IF(I.NE.0) P1ARAY(I,2,1)=P1ARAY(I-1,2,1)
                  ELSE
C     MOVE TO NEW POSITION WITH LOWERED PEN
                      P1ARAY(I,1,1)=IX
                      P1ARAY(I,2,1)=IY
                      IPST=1
                  END IF
                  P1ARAY(I,3,1)=IPST
                  IF(.NOT.PLEXIS) PLEXIS=.TRUE.
              END IF
C     IF FINISHED, LIFT PEN
              IF(I.EQ.STPSUR) IPST=0
          END DO
C
C     NOW ISSUE THE PLOTTING COMMANDS STORED IN THE P1ARAY ARRAY
C
          COLPAS=3
          CALL MY_COLTYP(COLPAS)
          FIXUP=.FALSE.
          DO I=STASUR,STPSUR
              IF(I.EQ.STASUR)
     1        CALL PENMV1A(P1ARAY(I,1,1),P1ARAY(I,2,1),P1ARAY(I,3,1))
              IF(I.GT.STASUR) THEN
                  GGO=.TRUE.
C     TEST IF LAST POINT IS NEW POINT, IF SO DON'T MOVE
                  IF(P1ARAY(I,1,1).EQ.P1ARAY(I-1,1,1).AND.P1ARAY(I,2,1).EQ.
     1            P1ARAY(I-1,2,1)) GGO=.FALSE.
                  IF(P1ARAY(I,3,1).NE.0.AND.GGO)
     1            CALL PENMV1A(P1ARAY(I,1,1),P1ARAY(I,2,1),P1ARAY(I,3,1))
              END IF
          END DO
          COLPAS=COLLBL
          CALL MY_COLTYP(COLPAS)
          NORAYPLOT=.FALSE.
          RETURN
      END


C SUB PLTRAE.FOR
      SUBROUTINE PLTRAE
          USE GLOBALS
C
          IMPLICIT NONE
C
C       THIS ROUTINE DOES THE PLOT RAY COMMAND AT THE CMD LEVEL
C
          REAL*8 X,Y,Z,XN,YN,ZN,ROT1X,ROT1Z,ROT2Y,
     1    ROT2Z,AX,AY,AZ,AALF,APHI,YMAXI,YMINI,XMAXI,XMINI
     2    ,XNEW,YNEW,LKG,VIEPH,VIEAL
C
          LOGICAL GGO
C
          INTEGER IX,IY,I,IPST,COLPAS,STOPAT,J
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
C     LNTYPE=0
          LNTYPE=0
C
          X=0.0D0
          Y=0.0D0
C
C     DOES A RAY EXIST
          IF(.NOT.REFEXT.AND..NOT.RAYEXT) THEN
              IF(MSG) THEN
                  OUTLYNE='ERROR'
                  CALL SHOWIT(1)
                  OUTLYNE='NO REFERENCE RAY EXISTS'
                  CALL SHOWIT(1)
                  OUTLYNE='NO RAY EXISTS TO BE PLOTTED'
                  CALL SHOWIT(1)
              END IF
              IF(F34.NE.1) CALL MACFAL
              RETURN
          END IF
          IF(RAYCOD(1).EQ.0)  STOPAT=NEWIMG
          IF(RAYCOD(1).EQ.1)  STOPAT=RAYCOD(2)-1
          IF(RAYCOD(1).EQ.2)  STOPAT=RAYCOD(2)-1
          IF(RAYCOD(1).EQ.3)  STOPAT=0
          IF(RAYCOD(1).EQ.4)  STOPAT=RAYCOD(2)
          IF(RAYCOD(1).EQ.5)  STOPAT=RAYCOD(2)
          IF(RAYCOD(1).EQ.6)  STOPAT=RAYCOD(2)
          IF(RAYCOD(1).EQ.7)  STOPAT=RAYCOD(2)
          IF(RAYCOD(1).EQ.8)  STOPAT=0
          IF(RAYCOD(1).EQ.9)  STOPAT=RAYCOD(2)
          IF(RAYCOD(1).EQ.10) STOPAT=0
          IF(RAYCOD(1).EQ.11) STOPAT=0
          IF(RAYCOD(1).EQ.12) STOPAT=0
          IF(RAYCOD(1).EQ.13) STOPAT=RAYCOD(2)-1
          IF(RAYCOD(1).EQ.14) STOPAT=RAYCOD(2)
          IF(RAYCOD(1).EQ.15) STOPAT=0
          IF(RAYCOD(1).EQ.16) STOPAT=0
          IF(RAYCOD(1).EQ.17) STOPAT=0
          IF(RAYCOD(1).EQ.18) STOPAT=RAYCOD(2)
          VIEPH=(PII/180.0D0)*VIEPHI
          VIEAL=(PII/180.0D0)*VIEALF
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
          GLOBE=.TRUE.
          OFFX=0.0D0
          OFFY=0.0D0
          OFFZ=0.0D0
          OFFA=0.0D0
          OFFB=0.0D0
          OFFC=0.0D0
          CALL GLVERT
          GLOBE=.FALSE.
          IF(MSG) THEN
              OUTLYNE='GENERATING RAY PLOTTING DATA...'
              CALL SHOWIT(1)
          END IF
C       CHECK SYNTAX
          IF(SST.EQ.1) THEN
              OUTLYNE=
     1        '"PLOT RAY" TAKES NO STRING INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
              OUTLYNE=
     1        '"PLOT RAY" ONLY TAKES NUMERIC WORDS #1 AND #2 INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(STI.EQ.1) THEN
              WRITE(OUTLYNE,800)
              CALL SHOWIT(1)
 800          FORMAT('QUERRY (?) HAS NO MEANING WITH "PLOT RAY"')
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
          IF(DF2.EQ.1) THEN
              IF(DABS(ALENS(3,(NEWIMG-1))).GT.1.0D10) THEN
                  W2=DBLE(NEWIMG-1)
              ELSE
                  W2=DBLE(NEWIMG)
              END IF
          ELSE
C       DF2 NOT 1, W2 EXPLICITLY ENTERED
          END IF
          STASUR=INT(W1)
          STPSUR=INT(W2)
          IF(STASUR.LT.NEWOBJ) STASUR=NEWOBJ
          IF(STPSUR.LT.NEWOBJ) STPSUR=NEWOBJ
C
          IF(INT(W1).LT.0) THEN
C       INVALID NUMERIC WORD #1
              OUTLYNE=
     1        'SURFACE NUMBER (NUMERIC WORD #1) IS BEYOND LEGAL RANGE'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(INT(W2).GT.NEWIMG) THEN
C       INVALID NUMERIC WORD #2
              OUTLYNE=
     1        'SURFACE NUMBER (NUMERIC WORD #2) IS BEYOND LEGAL RANGE'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(INT(W2).LE.INT(W1)) THEN
C       W2 LESS THAN OR EQUAL TO W1
              OUTLYNE=
     1        'NUMERIC WORD #2 MUST BE GREATER THAN NUMERIC WORD #1'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C       ALL INPUT IS OK, KEEP GOING
C       NOW WE HAVE THE REAL WORLD COORDINATES OF THE RAY TO BE PLOTTED
C       THEY ARE STORED IN THE ARRAY GLBRAY PASSED IN A COMMON BLOCK
C       VIA THE "INCLUDE 'datlen.inc'" STATEMENT.
C
C     HERE IS WHERE REAL WORK BEGINS. FIRST PROJECT THE 3D WORLD COORDINATES
C     OF THE VERTICIES ONTO THE 2D PLANE INDICATED BY THE PLOT LOOK/ PLOT VIEW
C     DATA. THE DIRECTION COSINES OF THE LOOK VECTOR ARE LOOKX, LOOKY AND
C     LOOKZ. THE TRANSFORMATIONS ARE:
C
          CALL ROT1
C
C     AFTER THIS TRANSFORMATION, THE WORLD X COORDINATE IS PLOTTED IN THE
C     HORIZONTAL X-AXIS OF THE DISPLAY AND THE Y WORLD COORDINATE IS PLOTTED
C     IN THE VERTICAL Y-AXIS OF THE DISPLAY AFTER A CONVERSION TO
C     DEVICE INDEPENDENT COORDINATES ARE MADE.
C
          CALL PLTSC1(XMINI,XMAXI,YMINI,YMAXI)
C
C     HERE IS THE LOOP WHICH PLOTS THE RAYS USING CALLS TO
C     PENMV1A(IX,IY,IPST)
C
          IF(STPSUR.GE.STOPAT) STPSUR=STOPAT
          IF(STASUR.LT.NEWOBJ) STASUR=NEWOBJ
          IF(STPSUR.LT.NEWOBJ) STPSUR=NEWOBJ
C     HERE IS WHERE THE CODE GOES TO PLOT THE RAYS INSIDE THE NSS SURFACE
          IF(STPSUR.LT.STASUR) STPSUR=STASUR

          DO I=STASUR,STPSUR
              IF(NUMHITS(I-1).GT.1.AND.I.GE.1) THEN
C     THERE ARE NSS RAY POINTS TO PLOT BEFORE PLOTTING TO THE RAY COORDINATE
C     AT SURFACE I
                  DO J=2,NUMHITS(I-1)
C
C     PLOT TO THE RAY COORDINATE AT SURFACE I
                      X=GLOBAL_MULTIRAY_DATA(1,I-1,J)
                      Y=GLOBAL_MULTIRAY_DATA(2,I-1,J)
                      Z=GLOBAL_MULTIRAY_DATA(3,I-1,J)

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
                      Y=(Y/SCFAY)*1000.0D0
C
                      X=(X/SCFAX)*1000.0D0
C
C     STEP 2: APPLY THE SCREEN Y-OFFSET AND SCREEN GAMMA ROTATION
C
C     APPLY THE PLOT LOOK/VIEW Y-OFFSET VALUE
C
                      IF(LORIENT) CALL ORSHIFT
                      Y=Y+3500.0D0+DBLE(PYSHFT)
                      X=X+DBLE(PXSHFT)
C
C     IF THERE ARE X-OFFSETS (JUSTIFICATION) TO APPLY, DO THEM HERE.
C
                      IF(RCL.EQ.1.OR.RCL.EQ.-1) THEN
                          JUSOFF=500.0D0-((XMINI/SCFAX)*1000.0D0)
                          RCL=-1
                      END IF
                      IF(RCL.EQ.2.OR.RCL.EQ.-2) THEN
                          RCL=-2
                          JUSOFF=5000.0D0
                      END IF
                      IF(RCL.EQ.3.OR.RCL.EQ.-3) THEN
                          JUSOFF=9500.0D0-((XMAXI/SCFAX)*1000.0D0)
                          RCL=-3
                      END IF
C
                      X=X+JUSOFF
C
C     IF THERE IS A NON-ZERO GAMMA SPECIFIED IN PLOT LOOK OR PLOT VIEW THEN
C     ROTATE IN GAMMA ON THE SCREEN ABOUT THE CENTER OF THE SCREEN
C     WHICH HAS COORDINATES X=5000.0D0,Y=3500.0D0
C
C     FIRST SHIFT THE COORDINATE ORIGIN TO THE CETER OF THE DISPLAY
C
                      X=X-5000.0D0
                      Y=Y-3500.0D0
C     THE SCREEN COORDINATE IN REAL*8 IN THE SHIFTED COORDINATE
C     FRAME IS NOW X AND Y

                      IF(DBLE(PGAMMA).NE.0.0D0) THEN
                          LKG=(PII/180.0D0)*DBLE(PGAMMA)
                          XNEW=((X*DCOS(LKG))-(Y*DSIN(LKG)))
                          YNEW=((X*DSIN(LKG))+(Y*DCOS(LKG)))
                          X=XNEW
                          Y=YNEW
                      END IF

C     THE ROTATION IS DONE, NOW SHIFT THE ORIGIN BACK TO THE BOTTOM
C     LEFT HAND CORNER
                      X=X+5000.0D0
                      Y=Y+3500.0D0
C     MOVE TO NEW POSITION WITH LOWERED PEN
                      IX=INT(X)
                      IY=INT(Y)
                      P1ARAY(I-1,1,J+1)=IX
                      P1ARAY(I-1,2,J+1)=IY
                      P1ARAY(I-1,3,J+1)=1
                      IF(.NOT.PLEXIS) PLEXIS=.TRUE.
                  END DO
              END IF
C     PLOT TO THE RAY COORDINATE AT SURFACE I
              X=GLPRAY(1,I)
              Y=GLPRAY(2,I)
              Z=GLPRAY(3,I)
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
              Y=(Y/SCFAY)*1000.0D0
C
              X=(X/SCFAX)*1000.0D0
C
C     STEP 2: APPLY THE SCREEN Y-OFFSET AND SCREEN GAMMA ROTATION
C
C     APPLY THE PLOT LOOK/VIEW Y-OFFSET VALUE
C
              IF(LORIENT) CALL ORSHIFT
              Y=Y+3500.0D0+DBLE(PYSHFT)
              X=X+DBLE(PXSHFT)
C
C     IF THERE ARE X-OFFSETS (JUSTIFICATION) TO APPLY, DO THEM HERE.
C
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
              X=X+JUSOFF
C
C     IF THERE IS A NON-ZERO GAMMA SPECIFIED IN PLOT LOOK OR PLOT VIEW THEN
C     ROTATE IN GAMMA ON THE SCREEN ABOUT THE CENTER OF THE SCREEN
C     WHICH HAS COORDINATES X=5000.0D0,Y=3500.0D0
C
C     FIRST SHIFT THE COORDINATE ORIGIN TO THE CETER OF THE DISPLAY
C
              X=X-5000.0D0
              Y=Y-3500.0D0

C     THE SCREEN COORDINATE IN REAL*8 IN THE SHIFTED COORDINATE
C     FRAME IS NOW X AND Y

              IF(DBLE(PGAMMA).NE.0.0D0) THEN
                  LKG=(PII/180.0D0)*DBLE(PGAMMA)

                  XNEW=((X*DCOS(LKG))-(Y*DSIN(LKG)))
                  YNEW=((X*DSIN(LKG))+(Y*DCOS(LKG)))
                  X=XNEW
                  Y=YNEW
              ELSE
              END IF

C     THE ROTATION IS DONE, NOW SHIFT THE ORIGIN BACK TO THE BOTTOM
C     LEFT HAND CORNER
              X=X+5000.0D0
              Y=Y+3500.0D0

              IF(I.EQ.STASUR) THEN
C     PUT INSTRUCTIONS IN P1ARAY TO LIFT PEN, GO TO STARTING POINT
                  IPST=1
                  IX=INT(X)
                  IY=INT(Y)
C
                  P1ARAY(I,1,1)=IX
                  P1ARAY(I,2,1)=IY
                  P1ARAY(I,3,1)=IPST

                  IF(.NOT.PLEXIS) PLEXIS=.TRUE.
              ELSE
              END IF

              IF(I.GT.STASUR) THEN
C     PUT INSTRUCTIONS IN P1ARAY TO DROP PEN AND DRAW
                  IPST=1
                  IX=INT(X)
                  IY=INT(Y)
C     IF RAY IS VIRTUAL AND NOVIRT IS TRUE, LIFT PEN ANYWAY
                  IF(NOVIRT.AND.GLVIRT(I).AND.I.NE.STPSUR.AND.DF3.EQ.1) THEN
C     LEAVE PEN ALONE AT LAST POSITION WITH PEN UP
                      IF(I.NE.0) P1ARAY(I,1,1)=P1ARAY(I-1,1,1)
                      IF(I.NE.0) P1ARAY(I,2,1)=P1ARAY(I-1,2,1)

                  ELSE
C     MOVE TO NEW POSITION WITH LOWERED PEN
                      IPST=1
                      P1ARAY(I,1,1)=IX
                      P1ARAY(I,2,1)=IY
C     IF FINISHED, LIFT PEN
                      IF(I.EQ.STPSUR) IPST=1

                  END IF
                  P1ARAY(I,3,1)=IPST

                  IF(.NOT.PLEXIS) PLEXIS=.TRUE.
              END IF

          END DO
C
C     NOW ISSUE THE PLOTTING COMMANDS STORED IN THE P1ARAY ARRAY
C
          COLPAS=COLRAY
          CALL MY_COLTYP(COLPAS)
          FIXUP=.FALSE.

          DO I=STASUR,STPSUR
              IF(I.GE.1.AND.NUMHITS(I-1).GT.1) THEN
C                       PLOT NSS RAY DATA FIRST
                  DO J=2,NUMHITS(I-1)
!      CALL PENMV1A(
!     1P1ARAY(I-1,1,J+1),P1ARAY(I-1,2,J+1),P1ARAY(I-1,3,J+1))
                      call drawdatasave(P1ARAY(I-1,1,J+1),P1ARAY(I-1,2,J+1),
     &                P1ARAY(I-1,3,J+1),0)
                  END DO
              END IF

              IF(I.EQ.STASUR)
!     1CALL PENMV1A(P1ARAY(I,1,1),P1ARAY(I,2,1),P1ARAY(I,3,1))
     1        CALL drawdatasave(P1ARAY(I,1,1),P1ARAY(I,2,1),0,0)

              IF(I.GT.STASUR) THEN
                  GGO=.TRUE.
C     TEST IF LAST POINT IS NEW POINT, IF SO DON'T MOVE
                  IF(P1ARAY(I,1,1).EQ.P1ARAY(I-1,1,1).AND.P1ARAY(I,2,1).EQ.
     1            P1ARAY(I-1,2,1)) GGO=.FALSE.
                  IF(P1ARAY(I,3,1).NE.0.AND.GGO)
!     1CALL PENMV1A(P1ARAY(I,1,1),P1ARAY(I,2,1),P1ARAY(I,3,1))
     &            CALL drawdatasave(P1ARAY(I,1,1),P1ARAY(I,2,1),P1ARAY(I,3,1),0)
              END IF
              IF(I.EQ.STPSUR)
!     1CALL PENMV1A(P1ARAY(I,1,1),P1ARAY(I,2,1),P1ARAY(I,3,1))
     1        CALL drawdatasave(P1ARAY(I,1,1),P1ARAY(I,2,1),1,0)

          END DO

          COLPAS=COLLBL
          CALL MY_COLTYP(COLPAS)
          NORAYPLOT=.FALSE.
C     HERE WE DO THE PLOT AXIS DRAWING
          IF(.NOT.VIGFLG.AND.PLTVIG) THEN
              CALL VIGSHO
              VIGFLG=.TRUE.
          ELSE
          END IF
          RETURN
      END


C SUB PLTRAYS.FOR
      SUBROUTINE PLTRAYS()
C
          IMPLICIT NONE
C
C       THIS PROGRAM CONTROLS THE "PLOT RAYS"
C
          REAL*8 VIEROT,VHI,VLO,XVHI,XVLO,YVHI,YVLO
C
          INTEGER DFLAG,VIEXOF,VIEYOF
C
          COMMON/OFFVIE/VIEXOF,VIEYOF,VIEROT
C
          INTEGER VDF2,VDF3,VS2,VS3,I,J
C
!      CHARACTER VIEWQ*8
C
          REAL*8 RAYS1,RAYS2,RAYS3,RAYS4,RAYS5
C
          REAL*8 VIEW2,VIEW3,DELTAX,DELTAY
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          INCLUDE 'datmac.inc'
C
C
          IF(DEVTYP.EQ.0) THEN
              WRITE(OUTLYNE,*) 'NO PLOT NEW HAS BEEN ISSUED'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*) '"PLOT RAYS" CAN NOT PROCEED'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(.NOT.REFEXT) THEN
              WRITE(OUTLYNE,*) 'NO REREFENCE RAY EXISTS'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*) '"PLOT RAYS" CAN NOT PROCEED'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
C     W1 IS FOR I
C     W2 IS FOR J
C     W3 IS NUMBER OF RAYS
C     W4 IS THE WAVELENGTH NUMBER
C     W5 IS ORIENTATION FLAG, 0 FOR YZ, 1 FOR XZ
C
          IF(DF1.EQ.1) THEN
              IF(DABS(ALENS(3,NEWOBJ)).GT.1.0D10) THEN
                  W1=DBLE(NEWOBJ+1)
                  DF1=0
                  S1=1
              ELSE
                  W1=DBLE(NEWOBJ)
                  DF1=0
                  S1=1
              END IF
          ELSE
C       DF1 NOT 1, W1 EXPLICITLY ENTERED
          END IF
          IF(DF2.EQ.1) THEN
              IF(DABS(ALENS(3,(NEWIMG-1))).GT.1.0D10) THEN
                  W2=DBLE(NEWIMG-1)
                  DF2=0
                  S2=1
              ELSE
                  W2=DBLE(NEWIMG)
                  DF2=0
                  S2=1
              END IF
          ELSE
C       DF2 NOT 1, W2 EXPLICITLY ENTERED
          END IF
          IF(DF3.EQ.1) THEN
              W3=11.0D0
              DF3=0
              S3=1
          ELSE
C       DF3 NOT 1, W3 EXPLICITLY ENTERED
          END IF
          W3=INT(W3)
          IF(W3.LE.3.0D0) W3=11.0D0
          IF(DF4.EQ.1) THEN
              W4=SYSTEM1(11)
              DF4=0
              S4=1
          ELSE
C       DF4 NOT 1, W4 EXPLICITLY ENTERED
          END IF
          W4=INT(W4)
          IF(W4.LT.1.0D0.OR.W4.GT.10.0D0) W4=SYSTEM1(11)
C
          IF(STI.EQ.1) THEN
              OUTLYNE= '"PLOT RAYS" PERFOMS AUTOMATED RAY FAN PLOTTING'
              CALL SHOWIT(1)
              RETURN
          END IF
          IF(DF1.EQ.0.AND.W1.LT.0.0D0) THEN
              OUTLYNE=
     1        'THE FIRST SURFACE # CAN NOT BE LESS THAN 0'
              CALL SHOWIT(1)
              OUTLYNE= 'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(DF1.EQ.0.AND.W1.GT.SYSTEM1(20)) THEN
              OUTLYNE=
     1        'THE FIRST SURFACE CAN NOT BE BEYOND THE IMAGE SURFACE'
              CALL SHOWIT(1)
              OUTLYNE= 'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(DF2.EQ.0.AND.W2.LT.0.0D0) THEN
              OUTLYNE=
     1        'THE LAST SURFACE # CAN NOT BE LESS THAN 0'
              CALL SHOWIT(1)
              OUTLYNE= 'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(DF2.EQ.0.AND.W2.GT.SYSTEM1(20)) THEN
              OUTLYNE=
     1        'THE LAST SURFACE CAN NOT BE BEYOND THE IMAGE SURFACE'
              CALL SHOWIT(1)
              OUTLYNE= 'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(DF1.EQ.0.AND.DF2.EQ.0.AND.W1.GE.W2) THEN
              OUTLYNE=
     1        'THE FIRST SURFACE MUST BE IN FRONT OF THE SECOND SURFACE'
              CALL SHOWIT(1)
              OUTLYNE= 'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(INT(W5).NE.0.AND.INT(W5).NE.1.AND.INT(W5).NE.2) THEN
              OUTLYNE=
     1        'FAN TYPE MUST BE 0 - YZ, 1 - XZ'
              CALL SHOWIT(1)
              OUTLYNE= 'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C       SET DRAW FLAG TO 0 OR NO.
          DFLAG=0
C
          RAYS1=W1
          RAYS2=W2
          RAYS3=W3
          RAYS4=W4
          RAYS5=W5
          VIEW2=RAYS1
          VIEW3=RAYS2
          VDF2=0
          VDF3=0
          VS2=1
          VS3=1
C
          IF(.NOT.VIGOFF) THEN
              CALL VIGCAL(10,VHI,VLO,2)
              YVHI=VHI
              YVLO=VLO
          ELSE
              YVHI=1.0D0
              YVLO=-1.0D0
          END IF
          IF(.NOT.VIGOFF) THEN
              CALL VIGCAL(10,VHI,VLO,1)
              XVHI=VHI
              XVLO=VLO
          ELSE
              XVHI=1.0D0
              XVLO=-1.0D0
          END IF
C     NOW TRACE THE RAYS
C
          IF(RAYS5.EQ.0.0D0) THEN
C     YZ-PLANE
C
              DELTAY=(YVHI-YVLO)/(RAYS3-1.0D0)
              WW1=YVHI
              DO I=1,INT(RAYS3)
                  SAVE_KDP(1)=SAVEINPT(1)
                  WW2=0.0D0
                  WW3=RAYS4
                  WVN=WW3
                  MSG=.FALSE.
                  WW4=1.0D0
                  NOCOAT=.TRUE.
                  GRASET=.TRUE.
                  CACOCH=1
                  CALL RAYTRA
                  REST_KDP(1)=RESTINPT(1)
C
                  SAVE_KDP(1)=SAVEINPT(1)
                  IF(RAYEXT) CALL VIERAY(VIEW2,VIEW3,VDF2,VDF3,VS2,VS3)
                  REST_KDP(1)=RESTINPT(1)
                  IF(I.EQ.INT(RAYS3)) THEN
                      WW1=YVLO
                  ELSE
                      WW1=WW1-DELTAY
                  END IF
              END DO
          END IF
          IF(RAYS5.EQ.1.0D0) THEN
C     XZ-PLANE
C
              DELTAX=(XVHI-XVLO)/(RAYS3-1.0D0)
              WW2=XVHI
              DO I=1,INT(RAYS3)
                  SAVE_KDP(1)=SAVEINPT(1)
                  WW1=0.0D0
                  WW3=RAYS4
                  WVN=WW3
                  MSG=.FALSE.
                  WW4=1.0D0
                  NOCOAT=.TRUE.
                  GRASET=.TRUE.
                  CACOCH=1
                  CALL RAYTRA
                  REST_KDP(1)=RESTINPT(1)
C
                  SAVE_KDP(1)=SAVEINPT(1)
                  IF(RAYEXT) CALL VIERAY(VIEW2,VIEW3,VDF2,VDF3,VS2,VS3)
                  REST_KDP(1)=RESTINPT(1)
                  IF(I.EQ.INT(RAYS3)) THEN
                      WW2=XVLO
                  ELSE
                      WW2=WW2-DELTAX
                  END IF
              END DO
          END IF
          IF(RAYS5.EQ.2.0D0) THEN
C     XZ-PLANE
C
              DELTAY=(YVHI-YVLO)/(RAYS3-1.0D0)
              DELTAX=(XVHI-XVLO)/(RAYS3-1.0D0)
              WW2=XVHI
              DO J=1,INT(RAYS3)
C       X LOOP
                  WW1=YVHI
                  DO I=1,INT(RAYS3)
C       Y LOOP
                      SAVE_KDP(1)=SAVEINPT(1)
                      WW3=RAYS4
                      WVN=WW3
                      MSG=.FALSE.
                      WW4=1.0D0
                      NOCOAT=.TRUE.
                      GRASET=.TRUE.
                      CACOCH=1
                      CALL RAYTRA
                      REST_KDP(1)=RESTINPT(1)
C
                      SAVE_KDP(1)=SAVEINPT(1)
                      IF(RAYEXT) CALL VIERAY(VIEW2,VIEW3,VDF2,VDF3,VS2,VS3)
                      REST_KDP(1)=RESTINPT(1)
                      IF(I.EQ.INT(RAYS3)) THEN
                          WW1=YVLO
                      ELSE
                          WW1=WW1-DELTAY
                      END IF
                  END DO
                  IF(I.EQ.INT(RAYS3)) THEN
                      WW2=XVLO
                  ELSE
                      WW2=WW2-DELTAX
                  END IF
              END DO
          END IF
          RETURN
      END

C SUB PLTPRO1.FOR
      SUBROUTINE PLTPRO1
          USE GLOBALS
C
          IMPLICIT NONE
C
C       THIS ROUTINE DOES THE PLOT PROF COMMAND AT THE CMD LEVEL
C
          REAL*8 X,Y,Z,XN,YN,ZN,ROT1X,ROT1Z,ROT2Y,XX1,XX2,YY1,
     1    ROT2Z,AX,AY,AZ,AALF,APHI,YMAXI,YMINI,XMAXI,XMINI
     2    ,XNEW,YNEW,LKG,VIEPH,VIEAL,Z1,YY2,AX1,AX2,AY1,AY2,ZDELZ
     3    ,X00,Y00,Z0,LX0,LY0,LZ0,ACALL1,ACALL2,XM,YM,ZCORR,ZDELZ1
     4    ,X1,X2,Y1,Y2,SLOPE,DELXX,DELYY,MX0,MY0,MZ0,NX0,NY0,NZ0
C
          LOGICAL ALT,VERT,INSIT,INSIDEIT,YESONE1,YESONE2,YESONE3,YESONE4
     1    ,YESONE5,YESONE6,SECPLT(0:499)
C
          COMMON/YESSIR/YESONE1,YESONE2,YESONE3,YESONE4,YESONE5,YESONE6
C
          EXTERNAL INSIDEIT
C
          INTEGER M1,M2,M3,M4,CAFLG,COFLG,J,IK,III,NO,CLRR,ALLOERR
     1    ,COLPAS,KKK
C
          REAL*8 XMIN,YMIN,XMAX,YMAX,ZA,ZB,ZM,FRACRAD,
     1    XMINO,YMINO,XMAXO,YMAXO,DRAPRO,THETA
     2    ,YMIN2,XMIN2,YMAX2,XMAX2
C
          INTEGER IX,IY,I,II,IPST
C
          INTEGER STARTPOINT,STOPPOINT
          DIMENSION STARTPOINT(:,:,:),STOPPOINT(:,:,:)
          ALLOCATABLE :: STARTPOINT,STOPPOINT
C
          LOGICAL NOPLOT
C
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
          REAL*8 PRO
C
          DIMENSION PRO(:,:,:)
          ALLOCATABLE :: PRO
C     LOOK.VIEW TRANSFORMS

          ROT1X(AX,AZ,APHI)=((AX*DCOS(APHI))-(AZ*DSIN(APHI)))
          ROT1Z(AX,AZ,APHI)=((AX*DSIN(APHI))+(AZ*DCOS(APHI)))
C
          ROT2Z(AZ,AY,AALF)=((AZ*DCOS(AALF))+(AY*DSIN(AALF)))
          ROT2Y(AZ,AY,AALF)=((-AZ*DSIN(AALF))+(AY*DCOS(AALF)))
          DEALLOCATE (PRO,STARTPOINT,STOPPOINT,STAT=ALLOERR)
          ALLOCATE (STARTPOINT(1:4,1:3,0:INT(SYSTEM1(20))),STAT=ALLOERR)
          ALLOCATE (STOPPOINT(1:4,1:3,0:INT(SYSTEM1(20))),STAT=ALLOERR)
          LNTYPE=0
          I=INT(SYSTEM1(20))
          STARTPOINT(1:4,1:3,0:I)=0
          STOPPOINT(1:4,1:3,0:I)=0
C
          IF(MSG) THEN
              OUTLYNE='GENERATING SURFACE PROFILE PLOTTING DATA...'
              CALL SHOWIT(1)
          END IF
          DO KKK=1,2
C     IF KKK=1, REGULAR SURFACE PROFILE
C     IF KKK=2, DRAW MIRROR BACKING IF ALENS(110, ) NOT ZERO
C     BY ADDING AN APPROPRIATE Z VALUE TO THE SURFACE LOCATION
C
              M1=360
              M2=4
              M3=0
              M4=INT(SYSTEM1(20))
              DEALLOCATE (PRO,STAT=ALLOERR)
              ALLOCATE (PRO(M1,M2,M3:M4),STAT=ALLOERR)
              PRO(1:360,1:4,0:M4)=0.0D0
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
                  DEALLOCATE(STARTPOINT,STOPPOINT,PRO,STAT=ALLOERR)
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
C       CHECK SYNTAX
              IF(SST.EQ.1) THEN
                  OUTLYNE=
     1            '"PLOT PROF" TAKES NO STRING INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  DEALLOCATE(STARTPOINT,STOPPOINT,PRO,STAT=ALLOERR)
                  RETURN
              END IF
              IF(S4.EQ.1) THEN
                  OUTLYNE=
     1            '"PLOT PROF" ONLY TAKES NUMERIC WORDS #1, #2,#3 AND #5 INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  DEALLOCATE(STARTPOINT,STOPPOINT,PRO,STAT=ALLOERR)
                  RETURN
              END IF
              NOPLOT=.FALSE.
              IF(DF5.EQ.0) NOPLOT=.TRUE.
              IF(STI.EQ.1) THEN
                  WRITE(OUTLYNE,801)
                  CALL SHOWIT(1)
 801              FORMAT('QUERRY (?) HAS NO MEANING WITH "PLOT PROF"')
                  DEALLOCATE(STARTPOINT,STOPPOINT,PRO,STAT=ALLOERR)
                  RETURN
              END IF
              IF(DF1.EQ.0.AND.W1.LT.0.0D0) W1=SYSTEM1(20)+W1
              IF(DF2.EQ.0.AND.W2.LT.0.0D0) W2=SYSTEM1(20)+W2
              IF(DF3.EQ.1) W3=0.0D0
              IF(W3.LT.0.0D0.OR.W3.GT.360.0D0) THEN
                  OUTLYNE='THE ANGLE "THETA", NUMERIC WORD #3 MUST BE IN THE'
                  CALL SHOWIT(1)
                  OUTLYNE='RANGE 0.0 TO 360 DEGREES'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  DEALLOCATE(STARTPOINT,STOPPOINT,PRO,STAT=ALLOERR)
                  RETURN
              END IF
C     CONVERT THETA TO RADIAN MEASURE
              IF(W3.GE.89.95D0.AND.W3.LE.90.05D0) W3=90.001D0
              IF(W3.GE.269.95D0.AND.W3.LE.270.05D0) W3=270.001D0
              THETA=W3*PII/180.0D0
C       DEFAULT VALUES
              IF(DF1.EQ.1) THEN
                  IF(DABS(ALENS(3,0)).GT.1.0D10) THEN
                      W1=1.0D0
                  ELSE
                      W1=0.0D0
                  END IF
              ELSE
C       DF1 NOT 1, W1 EXPLICITLY ENTERED
              END IF
              STASUR=INT(W1)
              IF(DF2.EQ.1) THEN
                  W2=SYSTEM1(20)
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
                  DEALLOCATE(STARTPOINT,STOPPOINT,PRO,STAT=ALLOERR)
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
                  DEALLOCATE(STARTPOINT,STOPPOINT,PRO,STAT=ALLOERR)
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
                  DEALLOCATE(STARTPOINT,STOPPOINT,PRO,STAT=ALLOERR)
                  RETURN
              END IF
C
C       ALL INPUT IS OK, KEEP GOING
C     THE ARRAY CONTAINING SURFACE PROFILE DATA ARE:
C     SURPRO(N,Q,I) WHERE
C     Q=1:3(X,Y AND Z GLOBAL COORDINATES,I=0:MAXSUR, AND N = TOTAL NUMBER OF
C     DATA POINTS IN THE PROFILE=360
C     INTEGER SURA,SURB THE STARTING AND ENDING SURFACES OF THE
C     REQUESTED RANGE OF SURFACE PROFILES TO BE PLOTTED
C
C     PRO(1:360,1:3,0:MAXSUR)
C
C     THE FIRST DIMENSION IS FOR THE DATA POINT NUMBER
C     THE SECOND DIMENSION IS FOR THE X,Y AND Z COORDINATES OR THE POINT
C
C     WE NEED TO LOAD THE ARRAY BEFORE PLOTTING
C
C     THE PROCEDURE IS:
C     1. DETERMINE THE MAXIMUM AND MINIMUM LOCAL X AND Y
c               COORDINATES FOR A SURFACE FOR WHICH THE PROFILE
C               IS TO BE CALCULATED.
C
C     CYCLE THROUGH ALL THE SURFACES BUT SKIP SURFACES WITH MULTIPLE
C     APERTURE DEFS ON IT
C

              DO II=STASUR,STPSUR
                  YESONE1=.FALSE.
                  YESONE2=.FALSE.
                  YESONE3=.FALSE.
                  YESONE4=.FALSE.
                  YESONE5=.FALSE.
                  YESONE6=.FALSE.
                  X1=0.0D0
                  X2=0.0D0
                  Y1=0.0D0
                  Y2=0.0D0
                  AX1=0.0D0
                  AX2=0.0D0
                  AY1=0.0D0
                  AY2=0.0D0
                  XX1=0.0D0
                  XX2=0.0D0
                  YY1=0.0D0
                  YY2=0.0D0
                  XMIN=0.0D0
                  YMIN=0.0D0
                  XMAX=0.0D0
                  YMAX=0.0D0
                  XMIN2=0.0D0
                  YMIN2=0.0D0
                  XMAX2=0.0D0
                  YMAX2=0.0D0
                  XMINO=0.0D0
                  YMINO=0.0D0
                  XMAXO=0.0D0
                  YMAXO=0.0D0
                  CAFLG=0
                  COFLG=0
                  III=II
C
C     NOW FOR THE II SURFACE WE CALCULATE ALL THE END POINTS
C
                  ZDELZ=0.0D0
                  CALL CAOJK(YMIN,XMIN,YMAX,XMAX,
     1            YMINO,XMINO,YMAXO,XMAXO,CAFLG,
     2            COFLG,III,
     3            YMIN2,XMIN2,YMAX2,XMAX2,THETA,ZDELZ)
C
C     NOW WE HAVE THE END POINTS, CYCLE THROUGH ALL FOUR PAIRS
C
C     2. USE APPROPRIATE CALLS TO THE SAGPLT.FOR ROUTINE
C               TO CALCULATE THE SAG AND MAKE
C               CERTAIN THE SIGN IS CORRECT FOR A LOCAL Z COORDINATE
C
                  X1=XMIN
                  Y1=YMIN
                  X2=XMAX
                  Y2=YMAX
                  AX1=XMIN2
                  AY1=YMIN2
                  AX2=XMAX2
                  AY2=YMAX2
                  XX1=XMINO
                  YY1=YMINO
                  XX2=XMAXO
                  YY2=YMAXO
C     SET UP THE LINE SLOPE AND THE INITIAL DELXX VALUE
                  IF((X2-X1).NE.0.0D0)SLOPE=(Y2-Y1)/(X2-X1)
                  VERT=.FALSE.
                  IF((X2-X1).EQ.0.0D0) VERT=.TRUE.
C
                  DELXX=(X2-X1)/(360.0D0-1.0D0)
                  DELYY=(Y2-Y1)/(360.0D0-1.0D0)
C     NOW WE HAVE THE END POINTS, CYCLE THROUGH ALL THE PAIRS AND LOAD
C     ALL THE ARRAYS WITH SURFACE LOCAL X,Y AND Z DATA
C
                  DO J=1,360
                      IF(J.EQ.1) THEN
                          X=X1
                      ELSE
                          X=X+DELXX
                      END IF
                      IF(.NOT.VERT) Y=(SLOPE*(X-X1))+Y1
                      IF(VERT) THEN
                          IF(J.EQ.1) THEN
                              Y=Y1
                          ELSE
                              Y=Y+DELYY
                          END IF
                      ELSE
                      END IF
C
C     CALLS TO SAGPLT GO HERE
                      III=II
                      ACALL1=X
                      ACALL2=Y
                      ALT=.FALSE.
                      IF(ALENS(34,III).NE.18.0D0) THEN
                          ALT=.FALSE.
                          IF(X.LT.AX1) THEN
                              ACALL1=AX1
                              ALT=.TRUE.
                          END IF
                          IF(X.GT.AX2) THEN
                              ACALL1=AX2
                              ALT=.TRUE.
                          END IF
                          IF(Y.LT.AY1) THEN
                              ACALL2=AY1
                              ALT=.TRUE.
                          END IF
                          IF(Y.GT.AY2) THEN
                              ACALL2=AY2
                              ALT=.TRUE.
                          END IF
                      END IF
                      IF(ALENS(9,III).EQ.1.0D0) THEN
C     CIRCULAR CLEAR APERTURE, MAY BE TYPE 18 SPECIAL SURFACE
                          ALT=.FALSE.
                          IF(X.LT.AX1) THEN
                              ACALL1=AX1
                              ALT=.TRUE.
                          END IF
                          IF(X.GT.AX2) THEN
                              ACALL1=AX2
                              ALT=.TRUE.
                          END IF
                          IF(Y.LT.AY1) THEN
                              ACALL2=AY1
                              ALT=.TRUE.
                          END IF
                          IF(Y.GT.AY2) THEN
                              ACALL2=AY2
                              ALT=.TRUE.
                          END IF
                      END IF
                      CALL SAGPLT(III,ACALL1,ACALL2,Z,NO)

                      IF(ALENS(9,III).EQ.5.0D0.OR.ALENS(9,III).EQ.6.0D0) THEN
                          ZDELZ=0.0D0
                          ZDELZ1=0.0D0
                      ELSE
                          IF(ALENS(9,III).NE.1.0D0.OR.ALENS(9,III).EQ.1.0D0.AND.
     1                    ALENS(12,III).NE.0.0D0.OR.ALENS(9,III).EQ.1.0D0.AND.
     2                    ALENS(13,III).NE.0.0D0) THEN
                              FRACRAD=0.0D0
                          ELSE
                              IF(ALENS(10,III).NE.ALENS(11,III)) THEN
                                  FRACRAD=((DSQRT((X**2)+(Y**2))-ALENS(11,III))/
     1                            (ALENS(10,III)-ALENS(11,III)))
                                  IF(FRACRAD.LE.0.0D0) FRACRAD=0.0D0
                              ELSE
                                  FRACRAD=0.0D0
                              END IF
                          END IF
                      END IF
                      ZDELZ1=ZDELZ*FRACRAD
                      Z=Z+ZDELZ1
                      IF(NO.EQ.1) DRAPRO=0.0D0
                      IF(NO.NE.1) DRAPRO=1.0D0
C
                      IF(ALENS(34,II).NE.18.0D0) THEN
C     ASSIGN ARRAY VALUES
                          PRO(J,1,II)=X
                          PRO(J,2,II)=Y
                          IF(J.GT.1) THEN
                              XM=PRO(J-1,1,II)
                              YM=PRO(J-1,2,II)
                          ELSE
                              XM=PRO(J,1,II)
                              YM=PRO(J,2,II)
                          END IF
C     DO WE HAVE TO DO A FIX FOR AN OBSCURATION
C
C IF XM,YM AND X,Y ARE INSIDE OBS, DRAPRO=0.0D0
C                  OTHERWISE
C                  DRAPRO=1.0D0

                          INSIT=.FALSE.
                          INSIT=INSIDEIT(II,X,Y,XM,YM)
                          IF(INSIT) DRAPRO=0.0D0
                          IF(.NOT.INSIT)DRAPRO=1.0D0
                          PRO(J,3,II)=Z
                          PRO(J,4,II)=DRAPRO
                      ELSE
C*****************************************************************************
C TYPE 18 SPECIAL SURFACE
                          ZA=FTFL01(1,II)
                          ZB=FTFL01(2,II)
                          PRO(J,1,II)=X
                          PRO(J,2,II)=Y
                          III=II
                          CALL SAGPLT(III,X,Y,Z,NO)
                          PRO(J,3,II)=Z
                          PRO(J,4,II)=1.0D0
C
C     DO WE HAVE TO DO A FIX FOR A SURFACE LIMIT
C
                          IF(J.GT.1) THEN
C     REMEMBER THE PREVIUOS Z VALUE
                              ZM=PRO(J-1,3,II)
                              XM=PRO(J-1,1,II)
                              YM=PRO(J-1,2,II)
                          END IF
                          IF(J.GT.1) THEN
C     DOES THE PREVIOUS VALUE AND THE CURRENT VALUE STRADEL THE ZA AND ZB
C     VALUES
                              IF(DABS(ZM).GT.DABS(ZA).AND.DABS(Z).LT.DABS(ZB).AND..NOT.YESONE5)
     1                        THEN
                                  PRO(J-1,1,II)=XM
                                  PRO(J-1,2,II)=YM
                                  PRO(J-1,3,II)=ZA
                                  ZM=ZA
                                  PRO(J-1,4,II)=1.0D0
                                  PRO(J,1,II)=X
                                  PRO(J,2,II)=Y
                                  PRO(J,3,II)=ZB
                                  Z=ZB
                                  PRO(J,4,II)=1.0D0
                                  YESONE5=.TRUE.
                              END IF
                              IF(DABS(ZM).LT.DABS(ZB).AND.DABS(Z).GT.DABS(ZA).AND..NOT.YESONE6)
     1                         THEN
                                  PRO(J-1,1,II)=XM
                                  PRO(J-1,2,II)=YM
                                  PRO(J-1,3,II)=ZB
                                  ZM=ZB
                                  PRO(J-1,4,II)=1.0D0
                                  PRO(J,1,II)=X
                                  PRO(J,2,II)=Y
                                  PRO(J,3,II)=ZA
                                  Z=ZA
                                  PRO(J,4,II)=1.0D0
                                  YESONE6=.TRUE.
                              END IF
C     DOES THE PREVIOUS VALUE AND THE CURRENT VALUE STRADLE THE OUTER
C     BOUNDARY VALUE AND THE CHECK WAS NOT YET MADE, THEN CHECK
                              IF(DABS(ZM).GT.DABS(ZA).AND.DABS(Z).LT.DABS(ZA).AND..NOT.YESONE3)
     1                         THEN
C     WE JUMPED OVER THE INSIDE BOUBDARY ON THE WAY IN
C     SET THE CURRENT VALUE TO THE INNER BOUNDARY LIMIT AND SET TH
C     DRAW FLAG TO 1
                                  PRO(J,3,II)=ZA
                                  Z=ZA
                                  PRO(J,4,II)=1.0D0
                                  YESONE3=.TRUE.
                              END IF
                              IF(DABS(ZM).LT.DABS(ZA).AND.DABS(Z).GT.DABS(ZA).AND..NOT.YESONE2)
     1                         THEN
C     WE JUMPED OVER THE INSIDE BOUBDARY ON THE WAY OUT
C     SET THE CURRENT VALUE TO THE INNER BOUNDARY LIMIT AND SET TH
C     DRAW FLAG TO 1
                                  PRO(J,3,II)=ZA
                                  Z=ZA
                                  PRO(J,4,II)=1.0D0
                                  YESONE4=.TRUE.
                              END IF
C     DOES THE PREVIOUS VALUE AND THE CURRENT VALUE STRADLE THE INNER
C     BOUNDARY VALUE AND THE CHECK WAS NOT YET MADE, THEN CHECK
                              IF(DABS(ZM).GT.DABS(ZB).AND.DABS(Z).LT.DABS(ZB).AND..NOT.YESONE1)
     1                         THEN
C     WE JUMPED OVER THE INSIDE BOUBDARY ON THE WAY IN
C     SET THE CURRENT VALUE TO THE INNER BOUNDARY LIMIT AND SET TH
C     DRAW FLAG TO 1
                                  PRO(J,3,II)=ZB
                                  Z=ZB
                                  PRO(J,4,II)=1.0D0
                                  YESONE1=.TRUE.
                              END IF
                              IF(DABS(ZM).LT.DABS(ZB).AND.DABS(Z).GT.DABS(ZB).AND..NOT.YESONE2)
     1                         THEN
C     WE JUMPED OVER THE INSIDE BOUBDARY ON THE WAY OUT
C     SET THE CURRENT VALUE TO THE INNER BOUNDARY LIMIT AND SET TH
C     DRAW FLAG TO 1
                                  PRO(J,3,II)=ZB
                                  Z=ZB
                                  PRO(J,4,II)=1.0D0
                                  YESONE2=.TRUE.
                              END IF
                          END IF
                          IF(DABS(Z).LT.DABS(ZB)) PRO(J,4,II)=0.0D0
                          IF(DABS(Z).GT.DABS(ZA)) PRO(J,4,II)=0.0D0
                          IF(DABS(Z).GE.DABS(ZB).AND.DABS(Z).LE.DABS(ZA)) PRO(J,4,II)=1.0D0
C         IF(NO.EQ.1) DRAPRO=0.0D0
C         IF(NO.NE.1) DRAPRO=1.0D0
                          IF(J.EQ.1) PRO(J,4,II)=1.0D0
                          IF(J.EQ.360) PRO(J,4,II)=1.0D0
C*****************************************************************************
                      END IF
C
C               CYCLE THROUGH THE NEXT DATA PAIR
                  END DO
C
C               CYCLE THROUGH THE NEXT SURFACE
              END DO
C
C     3. THE ARRAYS NOW HAVE LOCAL X,Y AND Z VALUES STORED IN THEM
C     CONVERT THE LOCAL X ANY Y PROFILES TO GLOBAL NUMBERS
C     GLOBAL VERTEX DATA IS
              DO II=STASUR,STPSUR
                  DO I=1,360
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
                      X=PRO(I,1,II)
                      Y=PRO(I,2,II)
                      SECPLT(II)=.FALSE.
                      IF(KKK.EQ.2) THEN
                          IF(ALENS(110,II).NE.0.0D0) THEN
                              ZCORR=DABS(ALENS(110,II))
                              IF(ALENS(46,II).LT.0.0D0) PRO(I,3,II)=PRO(I,3,II)+ZCORR
                              IF(ALENS(46,II).GT.0.0D0) PRO(I,3,II)=PRO(I,3,II)-ZCORR
                              SECPLT(II)=.TRUE.
                          ELSE
                              SECPLT(II)=.FALSE.
                          END IF
                      END IF
                      Z=PRO(I,3,II)
C
                      X1=X00+((LX0*(X))+(LY0*(Y))
     1                +(LZ0*(Z)))
                      Y1=Y00+((MX0*(X))+(MY0*(Y))
     1                +(MZ0*(Z)))
                      Z1=Z0+((NX0*(X))+(NY0*(Y))
     1                +(NZ0*(Z)))
                      PRO(I,1,II)=X1
                      PRO(I,2,II)=Y1
                      PRO(I,3,II)=Z1
                  END DO
              END DO
C
C     4. NOW DETERMINE THE BEST PLACE FOR THE ROTATION POINT FOR
C               PLOT LOOK/VIEW
C
              CALL ROT2(PRO,M1,M2,M3,M4)
C
C     5.  CONVERT THE GLOBAL X ANY Y PROFILES
C               USING THE LOOK/VIEW VALUES
              DO II=STASUR,STPSUR
                  DO I=1,360
                      X=PRO(I,1,II)
                      Y=PRO(I,2,II)
                      Z=PRO(I,3,II)
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
                      PRO(I,1,II)=XN
                      PRO(I,2,II)=YN
                      PRO(I,3,II)=ZN

                  END DO
              END DO
C
C     THE ARRAYS NOW HAVE GLOBAL SURFACE PROFILE DATA IN THEM
C
C     6.IF NEEDED, DETERMINE SCALE FACTORS AND PLOT RANGE
C
              CALL PLTSC2(XMINI,XMAXI,YMINI,YMAXI,PRO,M1,M2,M3,M4)
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
              DO II=STASUR,STPSUR
                  DO I=1,360
                      PRO(I,1,II)=(PRO(I,1,II)/SCFAX)*1000.0D0
                      PRO(I,2,II)=(PRO(I,2,II)/SCFAY)*1000.0D0
                  END DO
              END DO
C
C     8. APPLY THE PLOT XSHIFT AND YSHIFT VALUES
              DO I=1,360
                  DO II=STASUR,STPSUR
                      IF(LORIENT) CALL ORSHIFT
                      PRO(I,1,II)=PRO(I,1,II)+DBLE(PXSHFT)
                      PRO(I,2,II)=PRO(I,2,II)+3500.0D0+DBLE(PYSHFT)
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
              DO I=1,360
                  DO II=STASUR,STPSUR
                      PRO(I,1,II)=PRO(I,1,II)+JUSOFF
                  END DO
              END DO
C     9. PLOT GAMMA
C     IF THERE IS A NON-ZERO GAMMA SPECIFIED IN PLOT LOOK OR PLOT VIEW THEN
C     ROTATE IN GAMMA ON THE SCREEN ABOUT THE CENTER OF THE SCREEN
C     WHICH HAS COORDINATES X=5000.0D0,Y=3500.0D0
C
C     FIRST SHIFT THE COORDINATE ORIGIN TO THE CETER OF THE DISPLAY
C
              DO I=1,360
                  DO II=STASUR,STPSUR
                      PRO(I,1,II)=PRO(I,1,II)-5000.0D0
                      PRO(I,2,II)=PRO(I,2,II)-3500.0D0
                  END DO
              END DO
C     THE SCREEN COORDINATE IN REAL*8 IN THE SHIFTED COORDINATE
C     FRAME IS NOW X AND Y

              IF(DBLE(PGAMMA).NE.0.0D0) THEN
                  LKG=(PII/180.0D0)*DBLE(PGAMMA)

                  DO I=1,360
                      DO II=STASUR,STPSUR
                          X=PRO(I,1,II)
                          Y=PRO(I,2,II)
                          XNEW=((X*DCOS(LKG))-(Y*DSIN(LKG)))
                          YNEW=((X*DSIN(LKG))+(Y*DCOS(LKG)))
                          PRO(I,1,II)=XNEW
                          PRO(I,2,II)=YNEW
                      END DO
                  END DO
              ELSE
              END IF
C     THE ROTATION IS DONE, NOW SHIFT THE ORIGIN BACK TO THE BOTTOM
C     LEFT HAND CORNER
              DO I=1,360
                  DO II=STASUR,STPSUR
                      PRO(I,1,II)=PRO(I,1,II)+5000.0D0
                      PRO(I,2,II)=PRO(I,2,II)+3500.0D0
                  END DO
              END DO
C
              DO I=STASUR,STPSUR
                  IF(ALENS(127,I).NE.0.0D0) GO TO 51
C     NOW DRAW THE X PROFILE OF THE CLAP AT SURFACE I
C     WITH THE PEN UP, GO TO THE STARTING PLOT POSITION
                  IF(.NOT.PLEXIS) PLEXIS=.TRUE.
                  IF(WQ.EQ.'PROF') THEN
C     FIRST CLAP PROFILE
                      DO J=1,360
C     PUT INSTRUCTIONS IN P1ARAY TO DROP PEN AND DRAW
                          DRAPRO=PRO(J,4,I)

                          IF(J.EQ.1) IPST=1
                          IF(J.GT.1.AND.DRAPRO.EQ.1.0D0.AND.PRO(J,4,I).EQ.0.0D0) IPST=0
                          IF(J.GT.1.AND.DRAPRO.EQ.1.0D0.AND.PRO(J,4,I).EQ.1.0D0) IPST=1
                          IF(J.GT.1.AND.DRAPRO.EQ.0.0D0) IPST=0
                          IF(PRO(J,1,I).GT.1.0D6) PRO(J,1,I)=1.0D6
                          IF(PRO(J,2,I).GT.1.0D6) PRO(J,2,I)=1.0D6
                          IF(PRO(J,1,I).LT.-1.0D6) PRO(J,1,I)=-1.0D6
                          IF(PRO(J,2,I).LT.-1.0D6) PRO(J,2,I)=-1.0D6
                          IX=INT(PRO(J,1,I))
                          IY=INT(PRO(J,2,I))


                          P1ARAY(J,1,1)=IX
                          P1ARAY(J,2,1)=IY
!      P1ARAY(J+1,3,1)=IPST
                          P1ARAY(J,3,1)=IPST
                          IF(.NOT.PLEXIS) PLEXIS=.TRUE.
                      END DO
C     FINISHED WITH THAT PROFILE, LIFT PEN
                      IPST=0
C     NOW ISSUE THE PLOTTING COMMANDS STORED IN THE P1ARAY ARRAY
C
C     LINE TYPE SETTING
                      COLPAS=COLPRO
                      CALL MY_COLTYP(COLPAS)
                      OLLNTP=LNTYPE
                      LNTYPE=0
C     DASH, SOLID OR INVISIBLE
                      CLRR=0
                      IF(DUMMMY(I).AND.ALENS(9,I).EQ.0.0D0)
     1                CLRR=-1
                      IF(DUMMMY(I).AND.ALENS(9,I).NE.0.0D0) THEN
                          IF(DASHH) LNTYPE=2
                      ELSE
C     LEAVE LINE ALONE
                      END IF
C
                      IF(CLRR.NE.-1) THEN
                          FIXUP=.FALSE.
                          DO IK=1,360
                              IF(IK.EQ.1) THEN
                                  IF(P1ARAY(IK,3,1).NE.0) P1ARAY(IK,3,1)=0
                              END IF
                              IF(IK.GT.1.AND.IK.LE.360) THEN
                                  IF(P1ARAY(IK-1,1,1).LE.0.OR.P1ARAY(IK-1,2,1).LE.0
     1                            .OR.P1ARAY(IK,1,1).LE.0.OR.P1ARAY(IK,2,1).LE.0) P1ARAY(IK,3,1)=0
                              END IF
                          END DO
!     Draw Mirrors
                          DO IK=1,360

                              IF(KKK.EQ.2.AND.SECPLT(I).OR.KKK.EQ.1) THEN
                                  IF(KKK.EQ.1) THEN
                                      IF(IK.EQ.1) THEN
                                          STARTPOINT(1,1,I)=P1ARAY(1,1,1)
                                          STARTPOINT(1,2,I)=P1ARAY(1,2,1)
                                          STARTPOINT(1,3,I)=P1ARAY(1,3,1)
                                      END IF
                                      IF(IK.EQ.360) THEN
                                          STARTPOINT(4,1,I)=P1ARAY(360,1,1)
                                          STARTPOINT(4,2,I)=P1ARAY(360,2,1)
                                          STARTPOINT(4,3,I)=P1ARAY(360,3,1)
                                      END IF
                                  END IF
                                  IF(KKK.EQ.2.AND.SECPLT(I)) THEN
                                      IF(IK.EQ.1) THEN
                                          STOPPOINT(1,1,I)=P1ARAY(1,1,1)
                                          STOPPOINT(1,2,I)=P1ARAY(1,2,1)
                                          STOPPOINT(1,3,I)=P1ARAY(1,3,1)
                                      END IF
                                      IF(IK.EQ.360) THEN
                                          STOPPOINT(4,1,I)=P1ARAY(360,1,1)
                                          STOPPOINT(4,2,I)=P1ARAY(360,2,1)
                                          STOPPOINT(4,3,I)=P1ARAY(360,3,1)
                                      END IF
                                  END IF
                                  IF(IK.EQ.1) THEN
                                      IF(P1ARAY(IK,3,1).NE.0) P1ARAY(IK,3,1)=0
                                  END IF
                                  IF(IK.GT.1.AND.IK.LE.360) THEN
                                      IF(P1ARAY(IK-1,1,1).LE.0.OR.P1ARAY(IK-1,2,1).LE.0
     1                                .OR.P1ARAY(IK,1,1).LE.0.OR.P1ARAY(IK,2,1).LE.0) P1ARAY(IK,3,1)=0
                                  END IF

                                  IF(.NOT.NOPLOT.OR.NOPLOT.AND.ALENS(9,I).NE.0.0D0)
     1                            call drawdatasave(P1ARAY(IK,1,1),P1ARAY(IK,2,1),P1ARAY(IK,3,1),1)
!     1CALL PENMV1(P1ARAY(IK,1,1),P1ARAY(IK,2,1),P1ARAY(IK,3,1))

                              END IF
                          END DO

C     NOW DO THE EDGES AT THE OBSCURATIONS
                          DO IK=3,359
                              IF(KKK.EQ.1) THEN
                                  IF(P1ARAY(IK-1,3,1).EQ.0.AND.P1ARAY(IK,3,1).EQ.1) THEN
                                      STARTPOINT(2,1,I)=P1ARAY(IK,1,1)
                                      STARTPOINT(2,2,I)=P1ARAY(IK,2,1)
                                      STARTPOINT(2,3,I)=P1ARAY(IK,3,1)
                                      GO TO 20
                                  END IF
                              END IF
                          END DO
C     NOW DO THE EDGES AT THE OBSCURATIONS
 20                       DO IK=3,359
                              IF(KKK.EQ.1) THEN
                                  IF(P1ARAY(IK-1,3,1).EQ.1.AND.P1ARAY(IK,3,1).EQ.0) THEN
                                      STARTPOINT(3,1,I)=P1ARAY(IK,1,1)
                                      STARTPOINT(3,2,I)=P1ARAY(IK,2,1)
                                      STARTPOINT(3,3,I)=P1ARAY(IK,3,1)
                                      GO TO 30
                                  END IF
                              END IF
                          END DO
 30                       DO IK=3,359
                              IF(KKK.EQ.2.AND.SECPLT(I)) THEN
                                  IF(P1ARAY(IK-1,3,1).EQ.0.AND.P1ARAY(IK,3,1).EQ.1) THEN
                                      STOPPOINT(2,1,I)=P1ARAY(IK,1,1)
                                      STOPPOINT(2,2,I)=P1ARAY(IK,2,1)
                                      STOPPOINT(2,3,I)=P1ARAY(IK,3,1)
                                      GO TO 40
                                  END IF
                              END IF
                          END DO
 40                       DO IK=3,359
                              IF(KKK.EQ.2.AND.SECPLT(I)) THEN
                                  IF(P1ARAY(IK-1,3,1).EQ.1.AND.P1ARAY(IK,3,1).EQ.0) THEN
                                      STOPPOINT(3,1,I)=P1ARAY(IK,1,1)
                                      STOPPOINT(3,2,I)=P1ARAY(IK,2,1)
                                      STOPPOINT(3,3,I)=P1ARAY(IK,3,1)
                                      GO TO 50
                                  END IF
                              END IF
                          END DO
 50                       CONTINUE
                      ELSE
                          CLRR=0
                      END IF
                  ELSE
                  END IF
C     DO NEXT SURFACE
 51               CONTINUE
              END DO
C     DO SECOND PASS FOR MIRROR BACKING
              DEALLOCATE(PRO,STAT=ALLOERR)
          END DO
          FIXUP=.FALSE.
          DO I=0,INT(SYSTEM1(20))
              IF(.NOT.NOPLOT.OR.NOPLOT.AND.ALENS(9,I).NE.0.0D0) THEN
                  IF(ALENS(110,I).NE.0.0D0) THEN
                      CALL drawdatasave(STARTPOINT(1,1,I),STARTPOINT(1,2,I),0,1)
                      CALL drawdatasave(STOPPOINT(1,1,I),STOPPOINT(1,2,I),1,1)
                      CALL drawdatasave(STARTPOINT(2,1,I),STARTPOINT(2,2,I),0,1)
                      CALL drawdatasave(STOPPOINT(2,1,I),STOPPOINT(2,2,I),1,1)
                      CALL drawdatasave(STARTPOINT(3,1,I),STARTPOINT(3,2,I),0,1)
                      CALL drawdatasave(STOPPOINT(3,1,I),STOPPOINT(3,2,I),1,1)
                      CALL drawdatasave(STARTPOINT(4,1,I),STARTPOINT(4,2,I),0,1)
                      CALL drawdatasave(STOPPOINT(4,1,I),STOPPOINT(4,2,I),1,1)
                  END IF
              END IF
          END DO
C
C     HERE WE DO THE PLOT LI AND PLOT AXIS DRAWING
          IF(.NOT.VIGFLG.AND.PLTVIG) THEN
              CALL VIGSHO
              VIGFLG=.TRUE.
          ELSE
          END IF
          LNTYPE=OLLNTP
          DEALLOCATE(STARTPOINT,STOPPOINT,PRO,STAT=ALLOERR)
          DEALLOCATE (PRO,STARTPOINT,STOPPOINT,STAT=ALLOERR)
          LNTYPE=0
          RETURN
      END

