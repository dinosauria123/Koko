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

      SUBROUTINE DXO()
          IMPLICIT NONE
          include 'datmai.inc'
          OPEN(39,FILE=trim(HOME)//'DXF3D.DXF',STATUS='UNKNOWN')
          CALL DXH
          RETURN
      END


      SUBROUTINE DXC()
          IMPLICIT NONE
          CALL DXT
          CALL CLOSE_FILE(39,1)
          RETURN
      END


      SUBROUTINE DXH()
          IMPLICIT NONE
          WRITE(39,5110)
          WRITE(39,5120)
          WRITE(39,5130)
          WRITE(39,5150)
 5150     FORMAT('HEADER')
 5160     FORMAT('  9')
          WRITE(39,5160)
 5170     FORMAT('$CECOLOR')
          WRITE(39,5170)
 5200     FORMAT(' 62')
          WRITE(39,5200)
 5180     FORMAT('     0')
          WRITE(39,5180)
          WRITE(39,5110)
 5190     FORMAT('ENDSEC')
          WRITE(39,5190)
          WRITE(39,5110)
 5110     FORMAT('  0')
          WRITE(39,5120)
 5120     FORMAT('SECTION')
          WRITE(39,5130)
 5130     FORMAT('  2')
          WRITE(39,5140)
 5140     FORMAT('ENTITIES')
          RETURN
      END
C
      SUBROUTINE DXT()
          IMPLICIT NONE
          WRITE(39,5110)
 5110     FORMAT('  0')
          WRITE(39,5111)
 5111     FORMAT('ENDSEC')
          WRITE(39,5112)
 5112     FORMAT('  0')
          WRITE(39,5113)
 5113     FORMAT('EOF')
          RETURN
      END
      SUBROUTINE DXL(CLX)
          IMPLICIT NONE
          COMMON/DXFLAYR/CLAYER
          CHARACTER*8 CLAYER
          CHARACTER*8 CLX
          CLAYER=CLX
          RETURN
      END
      SUBROUTINE DXWLAYR
          IMPLICIT NONE
          COMMON/DXFLAYR/CLAYER
          CHARACTER*8 CLAYER
          WRITE(39,5200)
 5200     FORMAT('  8')
          WRITE(39,5201)CLAYER
 5201     FORMAT(A8)
          RETURN
      END


      SUBROUTINE DXPOLY3(XA,YA,ZA,NP,IOP)
          IMPLICIT NONE
          INTEGER I,NP,IOP,IOP2,IOP3
          REAL*8 XA,YA,ZA
          DIMENSION XA(1),YA(1),ZA(1)
C     Polyline 3D with layer
C     XA,YA,ZA arrays for x,y,z coordinates for polyline
C     NP       number of points in arrays XA,YA
C     IOP      =0 for open polyline
C              =1 for closed polyline (1st and last points connected)
          IOP2=IOP+8
          IOP3=32
          WRITE(39,5010)
 5010     FORMAT('  0')
          WRITE(39,5011)
 5011     FORMAT('POLYLINE')
          CALL DXWLAYR
          WRITE(39,5030)
 5030     FORMAT(' 66' )
          WRITE(39,5031)
 5031     FORMAT('     1' )
          WRITE(39,5040)
 5040     FORMAT(' 70')
          WRITE(39,5041)IOP2
 5041     FORMAT(I6)
          DO 100 ,I=1,NP
              WRITE(39,5050)
 5050         FORMAT('  0')
              WRITE(39,5051)
 5051         FORMAT('VERTEX')
              CALL DXWLAYR
              WRITE(39,5070)
 5070         FORMAT(' 10')
              WRITE(39,5071)XA(I)
 5071         FORMAT(F15.6)
              WRITE(39,5072)
 5072         FORMAT(' 20')
              WRITE(39,5073)YA(I)
 5073         FORMAT(F15.6)
              WRITE(39,5074)
 5074         FORMAT(' 30')
              WRITE(39,5075)ZA(I)
 5075         FORMAT(F15.6)
              WRITE(39,5076)
 5076         FORMAT(' 70')
              WRITE(39,5077)IOP3
 5077         FORMAT(I6)
  100     CONTINUE
          WRITE(39,5080)
 5080     FORMAT('  0')
          WRITE(39,5081)
 5081     FORMAT('SEQEND')
          CALL DXWLAYR
          RETURN
      END
C SUB DDXFF.FOR
      SUBROUTINE DDXFF
          USE NSSMOD
C
          IMPLICIT NONE
C
C       THIS PROGRAM CONTROLS THE ALL DXF PROCEDURES.
C
          LOGICAL DLQ,EXIS92
C
          REAL*8 SFI,MDX,MDY,GAMGAM
C
          INTEGER I,J
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
          DLQ=.FALSE.
          IF(WQ.EQ.'END')         DLQ=.TRUE.
          IF(WQ.EQ.'NEW')         DLQ=.TRUE.
          IF(WQ.EQ.'LAYER')       DLQ=.TRUE.
          IF(WQ.EQ.'RAY')         DLQ=.TRUE.
          IF(WQ.EQ.'PROF')        DLQ=.TRUE.
          IF(WQ.EQ.'PROFX')       DLQ=.TRUE.
          IF(WQ.EQ.'PROFY')       DLQ=.TRUE.
          IF(WQ.EQ.'EDGEX')       DLQ=.TRUE.
          IF(WQ.EQ.'EDGEY')       DLQ=.TRUE.
          IF(WQ.EQ.'CLAP')        DLQ=.TRUE.
          IF(WQ.EQ.'VERTLINE')    DLQ=.TRUE.
          IF(WQ.EQ.'GLBSURF')     DLQ=.TRUE.
          IF(WQ.EQ.'NSSRAYS')     DLQ=.TRUE.
          IF(WQ.EQ.'NSSSURFS')    DLQ=.TRUE.
          IF(WQ.EQ.'LINE')        DLQ=.TRUE.
          IF(.NOT.DLQ) THEN
              OUTLYNE='INVALID QUALIFIER USED WITH "DXF"'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
C       DXF NEW
          IF(WC.EQ.'DXF'.AND.WQ.EQ.'NEW') THEN
C       INITIALIZE DXF OUTPUT
C       CHECK SYNTAX
              IF(SN.EQ.1
     1        .OR.SST.EQ.1.AND.STI.NE.1) THEN
                  OUTLYNE='"DXF NEW" TAKES NO ADDITIONAL INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(STI.EQ.1) THEN
                  OUTLYNE='"DXF NEW ?" HAS NO MEANING'
                  CALL SHOWIT(1)
                  RETURN
              END IF
              F34=0
              MSG=.FALSE.
              DEVTYP=0
              GRASET=.FALSE.
              PLEXIS=.FALSE.
              LX1=0.0D0
              LY1=0.0D0
              LZ1=0.0D0
              LX2=0.0D0
              LY2=0.0D0
              LX2=0.0D0
              CALL DXFDEV
              RETURN
          END IF
          IF(WQ.NE.'NEW') THEN
              IF(DEVTYP.NE.2) THEN
                  OUTLYNE=
     1            '"DXF NEW" MUST BE ISSUED BEFORE PLOTTING CAN PROCEED'
                  CALL SHOWIT(1)
                  OUTLYNE=
     1            'NO ACTION TAKEN'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
          IF(WQ.EQ.'PROFY'.OR.WQ.EQ.'PROFX'.OR.WQ.EQ.'CLAP'.OR.WQ
     1    .EQ.'EDGEY'.OR.WQ.EQ.'EDGEX'.OR.WQ.EQ.'PROF'.OR.WQ.EQ.
     2    'VERTLINE') THEN
              COATSET=.FALSE.
              SAVE_KDP(1)=SAVEINPT(1)
              INPUT='FOB'
              CALL PROCES
              REST_KDP(1)=RESTINPT(1)
              SAVE_KDP(1)=SAVEINPT(1)
              DXFSET=.TRUE.
              INPUT='RAY'
              CALL PROCES
              REST_KDP(1)=RESTINPT(1)
          END IF
C
C       DXF END
          IF(WC.EQ.'DXF'.AND.WQ.EQ.'END') CALL DXFSTP
C
C       LAYER
          IF(WQ.EQ.'LAYER') CALL DXFLAYER
C
C       LINE
c       IF(WQ.EQ.'LINE') CALL DXFLINE
C
C       RAY
          IF(WQ.EQ.'RAY') CALL DXFRAE
C
C       VERTLINE
          IF(WQ.EQ.'VERTLINE') CALL DXFVERTLINE
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
              CALL DXFPRO1
              RETURN
          END IF
C
C       EDGEX OR EDGEY
          IF(WQ.EQ.'EDGEX'.OR.WQ.EQ.'EDGEY') CALL DXFEDG

C       CLAP
          IF(WQ.EQ.'CLAP') THEN
C
C       CHECK SYNTAX
              IF(SST.EQ.1) THEN
                  OUTLYNE=
     1            '"DXF CLAP" TAKES NO STRING INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(S4.EQ.1.OR.S5.EQ.1) THEN
                  OUTLYNE=
     1            '"DXF CLAP" ONLY TAKES NUMERIC WORDS #1, #2 and #3 INPUT'
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
 800              FORMAT('QUERRY (?) HAS NO MEANING WITH "DXF CLAP"')
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
              IF(MSG) THEN
                  OUTLYNE='GENERATING SURFACE CLEAR APERTURE DXF DATA...'
                  CALL SHOWIT(1)
              END IF
              SFI=W3
              MDX=0.0D0
              MDY=0.0D0
              IF(SFI.EQ.0.0D0) SFI=1.0D0
              DO I=STASUR,STPSUR
                  IF(ALENS(127,I).NE.0.0D0) THEN
                      DO J=1,INT(ALENS(127,I))
                          MDX=MULTCLAP(J,1,I)
                          MDY=MULTCLAP(J,2,I)
                          GAMGAM=MULTCLAP(J,3,I)
                          CALL DXFCLP(1,I,SFI,MDX,MDY,GAMGAM)
                      END DO
                  ELSE
                      CALL DXFCLP(1,I,SFI,0.0D0,0.0D0,0.0D0)
                  END IF
                  IF(ALENS(127,I).NE.0.0D0) THEN
                      DO J=1,INT(ALENS(127,I))
                          MDX=MULTCLAP(J,1,I)
                          MDY=MULTCLAP(J,2,I)
                          GAMGAM=MULTCLAP(J,3,I)
                          CALL DXFCLP(2,I,SFI,MDX,MDY,GAMGAM)
                      END DO
                  ELSE
                      CALL DXFCLP(2,I,SFI,0.0D0,0.0D0,0.0D0)
                  END IF
              END DO
          END IF
C
C       NSSSURFS
          IF(WQ.EQ.'NSSSURFS') THEN
              IF(NEXISTN) THEN
                  CALL DXFNSSSFS
                  RETURN
              ELSE
                  OUTLYNE=
     1            'NO NSS DATABASE EXISTS, NO NSS SURFACES CAN BE ADDED'
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
                  CALL DXFNSSRY
                  CALL CLOSE_FILE(92,1)
                  RETURN
              ELSE
                  IF(.NOT.NEXISTN) THEN
                      OUTLYNE=
     1                'NO NSS DATABASE EXISTS, NO NSS RAYS CAN BE ADDED'
                      CALL SHOWIT(1)
                  END IF
                  IF(.NOT.EXIS92) THEN
                      OUTLYNE=
     1                'NO NSS RAY HISTORY FILE EXISTS, NO NSS RAYS CAN BE ADDED'
                      CALL SHOWIT(1)
                  END IF
                  CALL MACFAL
                  RETURN
              END IF
          END IF

C       GLBSURF
          IF(WQ.EQ.'GLBSURF') CALL GLBSURF
          RETURN
      END
      SUBROUTINE DXFLAYER
          IMPLICIT NONE
          CHARACTER*8 LAYERNAME
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
          IF(STI.EQ.1) THEN
              OUTLYNE='THE CURRENT DXF LAYER NAME IS :'//LAYER
              CALL SHOWIT(1)
              RETURN
          END IF
          IF(SN.EQ.1) THEN
              OUTLYNE='"DXF LAYER" TAKES NO NUMERIC INPUT'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(WS(1:1).EQ.' '.OR.WS(2:2).EQ.' '.OR.WS(3:3).EQ.' '
     1    .OR.WS(4:4).EQ.' '.OR.WS(5:5).EQ.' '.OR.WS(6:6).EQ.' '.OR.
     2    WS(7:7).EQ.' '.OR.WS(8:8).EQ.' ') THEN
              OUTLYNE=
     1        'DXF LAYER NAME NAME MUST BE EXACTLY 8 NON-BLANK CHARACTERS'
              CALL SHOWIT(1)
              OUTLYNE=
     1        'IN LENGTH'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          ELSE
          END IF
          LAYER=WS(1:8)
          LAYERNAME=WS(1:8)
          CALL DXL(LAYERNAME)
          RETURN
      END
      SUBROUTINE GLBSURF
          IMPLICIT NONE
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
          IF(STI.EQ.1) THEN
              WRITE(OUTLYNE,*) 'THE DXF GLOBAL SURFACE IS :', GLSURF
              CALL SHOWIT(1)
              RETURN
          END IF
          IF(SST.EQ.1.OR.S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1
     1    .OR.S5.EQ.1) THEN
              OUTLYNE='"DXF GLBSURF" ONLY TAKES NUMERIC WORD #1 INPUT'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(DF1.EQ.1) THEN
              OUTLYNE=
     1        '"DXF GLBSURF" REQUIRES EXPLICIT NUMERIC WORD #1 INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          ELSE
          END IF
          IF(W1.LT.0.0D0.OR.W1.GT.SYSTEM1(20)) THEN
              OUTLYNE=
     1        'DXF GLOBAL SURFACE NUMBER BEYOND LEGAL BOUNDS'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          ELSE
          END IF
          GLSURF=INT(W1)
          GLOBE=.TRUE.
          OFFX=0.0D0
          OFFY=0.0D0
          OFFZ=0.0D0
          OFFA=0.0D0
          OFFB=0.0D0
          OFFC=0.0D0
C     RESET NEWIMG,0 AND NEWREF
          CALL RESSUR
          CALL GLVERT
          CALL OLDSUR
          RETURN
      END
C SUB DXFSTP.FOR
      SUBROUTINE DXFSTP
C
          IMPLICIT NONE
C
C       THIS ROUTINE DOES "DXF END"
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
          IF(WC.EQ.'DXF'.AND.WQ.EQ.'END') THEN
C       STOP PLOTTING
C       CHECK SYNTAX
              IF(SST.EQ.1.OR.SN.EQ.1) THEN
                  OUTLYNE='"DXF END" TAKES NO STRING OR NUMERIC INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(DEVTYP.NE.2) THEN
                  OUTLYNE='WARNING: NO DXF EXISTS TO CLOSE'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              CALL DXC()
              LAYER='LAYER001'
              DXFEXIS=.FALSE.
              DXFSET=.FALSE.
              DEVTYP=0
          END IF
          RETURN
      END


C SUB DXFLINE.FOR
      SUBROUTINE DXFLINE
          USE GLOBALS
C
          IMPLICIT NONE
C
C       THIS ROUTINE DOES THE DXFLINE COMMAND AT THE CMD LEVEL
C
          INTEGER I,NUMPEE,ALLOERR
C
          REAL*8 DXF_X,DXF_Y,DXF_Z
          DIMENSION DXF_X(:),DXF_Y(:),DXF_Z(:)
          ALLOCATABLE :: DXF_X,DXF_Y,DXF_Z
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
          IF(MSG) THEN
              OUTLYNE='GENERATING LINE DATA...'
              CALL SHOWIT(1)
          END IF
C       CHECK SYNTAX
          IF(SST.EQ.1.OR.SN.EQ.1) THEN
              OUTLYNE=
     1        '"DXF LINE" TAKES NO ADDITIONAL INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(STI.EQ.1) THEN
              WRITE(OUTLYNE,800)
              CALL SHOWIT(1)
 800          FORMAT('QUERRY (?) HAS NO MEANING WITH "DXF LINE"')
              RETURN
          ELSE
          END IF
C       ALL INPUT IS OK, KEEP GOING
C
C     RIGHT NOW,COORDINATES ARE IN WORLD COORDINATES
C
          IF(.NOT.DXFEXIS) DXFEXIS=.TRUE.
C
C     NOW ISSUE THE PLOTTING COMMANDS STORED IN THE P1ARAY ARRAY
C
          DEALLOCATE(DXF_X,DXF_Y,DXF_Z
     1    ,STAT=ALLOERR)
          ALLOCATE(DXF_X(1:MAXSUR+1),DXF_Y(1:MAXSUR+1),DXF_Z(1:MAXSUR+1)
     1    ,STAT=ALLOERR)
          DO I=1,2
              IF(I.EQ.1) THEN
                  DXF_X(1)=REAL(LX1)
                  DXF_Y(1)=REAL(LY1)
                  DXF_Z(1)=REAL(LZ1)
              END IF
              IF(I.EQ.2) THEN
                  DXF_X(1)=REAL(LX2)
                  DXF_Y(1)=REAL(LY2)
                  DXF_Z(1)=REAL(LZ2)
              END IF
          END DO
          NUMPEE=2
          CALL DXPOLY3(DXF_X,DXF_Y,DXF_Z,NUMPEE,0)
          DEALLOCATE(DXF_X,DXF_Y,DXF_Z
     1    ,STAT=ALLOERR)
          RETURN
      END


C SUB DXFPRO1.FOR
      SUBROUTINE DXFPRO1
C
          IMPLICIT NONE
C
C       THIS ROUTINE DOES THE DXF PROF COMMAND AT THE CMD LEVEL
C
          REAL*8 X,Y,Z,ROT1X,ROT1Z,ROT2Y,XX1,XX2,YY1,
     1    ROT2Z,AX,AY,AZ,AALF,APHI
     2    ,VIEPH,VIEAL,Z1,YY2,AX1,AX2,AY1,AY2
     3    ,X00,Y00,Z0,LX0,LY0,LZ0,ACALL1,ACALL2,XM,YM
     4    ,X1,X2,Y1,Y2,SLOPE,DELXX,DELYY,MX0,MY0,MZ0,NX0,NY0,NZ0
C
          LOGICAL ALT,VERT,INSIT,INSIDEIT,YESONE1,YESONE2,YESONE3,YESONE4
     1    ,YESONE5,YESONE6,VIS
C
          COMMON/YESSIR/YESONE1,YESONE2,YESONE3,YESONE4,YESONE5,YESONE6
C
          EXTERNAL INSIDEIT
C
          INTEGER M1,M2,M3,M4,CAFLG,COFLG,J,K,III,NO,ALLOERR
C
          REAL*8 XMIN,YMIN,XMAX,YMAX,ZA,ZB,ZM,ZDELZ,
     1    XMINO,YMINO,XMAXO,YMAXO,DRAPRO,THETA,FRACRAD,ZDELZ1
     2    ,YMIN2,XMIN2,YMAX2,XMAX2
C
          INTEGER I,II,NUMPEE
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
          REAL*8 PRO
C
          DIMENSION PRO(:,:,:)
          ALLOCATABLE :: PRO
C     LOOK.VIEW TRANSFORMS
          REAL*8 DXF_X,DXF_Y,DXF_Z
          DIMENSION DXF_X(:),DXF_Y(:),DXF_Z(:),VIS(:)
          ALLOCATABLE :: DXF_X,DXF_Y,DXF_Z,VIS

          ROT1X(AX,AZ,APHI)=((AX*DCOS(APHI))-(AZ*DSIN(APHI)))
          ROT1Z(AX,AZ,APHI)=((AX*DSIN(APHI))+(AZ*DCOS(APHI)))
C
          ROT2Z(AZ,AY,AALF)=((AZ*DCOS(AALF))+(AY*DSIN(AALF)))
          ROT2Y(AZ,AY,AALF)=((-AZ*DSIN(AALF))+(AY*DCOS(AALF)))
C
C     IREGULAR SURFACE PROFILE
C
          M1=360
          M2=4
          M3=0
          M4=INT(SYSTEM1(20))
          DEALLOCATE (PRO,VIS,STAT=ALLOERR)
          ALLOCATE (PRO(M1,M2,M3:M4),VIS(0:INT(SYSTEM1(20))),STAT=ALLOERR)
          PRO(1:360,1:4,0:M4)=0.0D0
C
          X=0.0D0
          Y=0.0D0
C
          VIEPH=(PII/180.0D0)*VIEPHI
          VIEAL=(PII/180.0D0)*VIEALF
          IF(MSG) THEN
              OUTLYNE='GENERATING SURFACE PROFILE DXF DATA...'
              CALL SHOWIT(1)
              OUTLYNE='                                      '
          END IF
C
C       CHECK SYNTAX
          IF(SST.EQ.1) THEN
              OUTLYNE=
     1        '"DXF PROF" TAKES NO STRING INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              DEALLOCATE(PRO,VIS,STAT=ALLOERR)
              RETURN
          END IF
          IF(S4.EQ.1.OR.S5.EQ.1) THEN
              OUTLYNE=
     1        '"DXF PROF" ONLY TAKES NUMERIC WORDS #1, #2 AND #3 INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              DEALLOCATE(PRO,VIS,STAT=ALLOERR)
              RETURN
          END IF
          IF(STI.EQ.1) THEN
              WRITE(OUTLYNE,801)
              CALL SHOWIT(1)
 801          FORMAT('QUERRY (?) HAS NO MEANING WITH "DXF PROF"')
              DEALLOCATE(PRO,VIS,STAT=ALLOERR)
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
              RETURN
          END IF
C     CONVERT THETA TO RADIAN MEASURE
          IF(W3.GE.89.95D0.AND.W3.LE.90.05D0) W3=90.0D0
          IF(W3.GE.269.95D0.AND.W3.LE.270.05D0) W3=270.0D0
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
              W2=DBLE(NEWIMG)
          ELSE
C       DF2 NOT 1, W2 EXPLICITLY ENTERED
          END IF
          STPSUR=INT(W2)
          IF(INT(W1).LT.0) THEN
C       INVALID NUMERIC WORD #1
              OUTLYNE=
     1        'SURFACE NUMBER (NUMERIC WORD #1) IS BEYOND LEGAL RANGE'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              DEALLOCATE(PRO,VIS,STAT=ALLOERR)
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
              DEALLOCATE(PRO,VIS,STAT=ALLOERR)
              RETURN
          END IF
          IF(INT(W2).LT.INT(W1)) THEN
C       W2 LESS THAN OR EQUAL TO W1
              OUTLYNE=
     1        'NUMERIC WORD #2 MAY NOT BE LESS THAN NUMERIC WORD #1'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              DEALLOCATE(PRO,VIS,STAT=ALLOERR)
              RETURN
          END IF
C
C       ALL INPUT IS OK, KEEP GOING
C     THE ARRAY CONTAINING SURFACE PROFILE DATA ARE:
C     PRO(N,Q,I) WHERE
C     Q=1:3(X,Y AND Z GLOBAL COORDINATES,I=0:MAXSUR, AND N = TOTAL NUMBER OF
C     DATA POINTS IN THE PROFILE=360
C     INTEGER SURA,SURB THE STARTING AND ENDING SURFACES OF THE
C     REQUESTED RANGE OF SURFACE PROFILES TO BE DXF'ED
C
C     PRO(1:360,1:3,0:MAXSUR)
C
C     THE FIRST DIMENSION IS FOR THE DATA POINT NUMBER
C     THE SECOND DIMENSION IS FOR THE X,Y AND Z COORDINATES OR THE POINT
C
C     WE NEED TO LOAD THE ARRAY
C
C     THE PROCEDURE IS:
C     1. DETERMINE THE MAXIMUM AND MINIMUM LOCAL X AND Y
c               COORDINATES FOR A SURFACE FOR WHICH THE PROFILE
C               IS TO BE CALCULATED.
C
C     CYCLE THROUGH ALL THE SURFACES
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
     1        YMINO,XMINO,YMAXO,XMAXO,CAFLG,
     2        COFLG,III,
     3        YMIN2,XMIN2,YMAX2,XMAX2,THETA,ZDELZ)
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
     1                ALENS(12,III).NE.0.0D0.OR.ALENS(9,III).EQ.1.0D0.AND.
     2                ALENS(13,III).NE.0.0D0) THEN
                          FRACRAD=0.0D0
                      ELSE
                          IF(ALENS(10,III).NE.ALENS(11,III)) THEN
                              FRACRAD=((DSQRT((X**2)+(Y**2))-ALENS(11,III))/
     1                        (ALENS(10,III)-ALENS(11,III)))
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
     1                    THEN
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
     1                     THEN
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
     1                     THEN
C     WE JUMPED OVER THE INSIDE BOUBDARY ON THE WAY IN
C     SET THE CURRENT VALUE TO THE INNER BOUNDARY LIMIT AND SET TH
C     DRAW FLAG TO 1
                              PRO(J,3,II)=ZA
                              Z=ZA
                              PRO(J,4,II)=1.0D0
                              YESONE3=.TRUE.
                          END IF
                          IF(DABS(ZM).LT.DABS(ZA).AND.DABS(Z).GT.DABS(ZA).AND..NOT.YESONE2)
     1                     THEN
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
     1                     THEN
C     WE JUMPED OVER THE INSIDE BOUBDARY ON THE WAY IN
C     SET THE CURRENT VALUE TO THE INNER BOUNDARY LIMIT AND SET TH
C     DRAW FLAG TO 1
                              PRO(J,3,II)=ZB
                              Z=ZB
                              PRO(J,4,II)=1.0D0
                              YESONE1=.TRUE.
                          END IF
                          IF(DABS(ZM).LT.DABS(ZB).AND.DABS(Z).GT.DABS(ZB).AND..NOT.YESONE2)
     1                     THEN
C     WE JUMPED OVER THE INSIDE BOUNDARY ON THE WAY OUT
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
                  Z=PRO(I,3,II)
C
                  X1=X00+((LX0*(X))+(LY0*(Y))
     1            +(LZ0*(Z)))
                  Y1=Y00+((MX0*(X))+(MY0*(Y))
     1            +(MZ0*(Z)))
                  Z1=Z0+((NX0*(X))+(NY0*(Y))
     1            +(NZ0*(Z)))
                  PRO(I,1,II)=X1
                  PRO(I,2,II)=Y1
                  PRO(I,3,II)=Z1
              END DO
          END DO
C
          DEALLOCATE(DXF_X,DXF_Y,DXF_Z,STAT=ALLOERR)
          ALLOCATE(DXF_X(1:360),DXF_Y(1:360),DXF_Z(1:360),STAT=ALLOERR)
C
C     NOW WRITE DATA TO THE DXF FILE
C
          DO I=STASUR,STPSUR
C     SET VISIBILITY FACTOR
              VIS(I)=.TRUE.
              IF(DUMMMY(I).AND.ALENS(9,I).EQ.0.0D0.AND.
     1        ALENS(16,I).EQ.0.0D0.AND.I.NE.STASUR.AND.I.NE.STPSUR)
     1        VIS(I)=.FALSE.
          END DO
C
          IF(.NOT.DXFEXIS) DXFEXIS=.TRUE.
C
          DO I=STASUR,STPSUR
              IF(ALENS(127,I).NE.0.0D0) GO TO 51
              IF(VIS(I)) THEN
C     ADD DATA TO DXF FILE
                  K=1
                  DO J=1,360
C     PUT INSTRUCTIONS IN P1ARAY TO DROP PEN AND DRAW
                      DXF_X(K)=REAL(PRO(J,1,I))
                      DXF_Y(K)=REAL(PRO(J,2,I))
                      DXF_Z(K)=REAL(PRO(J,3,I))
                      NUMPEE=K
                      K=K+1
                  END DO
                  CALL DXPOLY3(DXF_X,DXF_Y,DXF_Z,NUMPEE,0)
C     DO NEXT SURFACE
              ELSE
C     SURFACE PROFILE INVISIBLE
              END IF
 51           CONTINUE
          END DO
          DEALLOCATE(DXF_X,DXF_Y,DXF_Z,STAT=ALLOERR)
          DEALLOCATE(PRO,VIS,STAT=ALLOERR)
          RETURN
      END

C SUB DXFRAE.FOR
      SUBROUTINE DXFRAE
          USE GLOBALS
C
          IMPLICIT NONE
C
C       THIS ROUTINE DOES THE DXFRAY COMMAND AT THE CMD LEVEL
C
          INTEGER K,I,NUMPEE,STOPAT,ALLOERR
C
          REAL*8 DXF_X,DXF_Y,DXF_Z
          DIMENSION DXF_X(:),DXF_Y(:),DXF_Z(:)
          ALLOCATABLE :: DXF_X,DXF_Y,DXF_Z
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
C     DOES A RAY EXIST
          IF(.NOT.REFEXT.AND..NOT.RAYEXT) THEN
              IF(MSG) THEN
                  OUTLYNE='ERROR'
                  CALL SHOWIT(1)
                  OUTLYNE='NO REFERENCE RAY EXISTS'
                  CALL SHOWIT(1)
                  OUTLYNE='NO RAY EXISTS TO BE INCLUDED IN THE DXF FILE'
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
          IF(MSG) THEN
              OUTLYNE='GENERATING RAY DXF DATA...'
              CALL SHOWIT(1)
          END IF
C       CHECK SYNTAX
          IF(SST.EQ.1) THEN
              OUTLYNE=
     1        '"DXF RAY" TAKES NO STRING INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
              OUTLYNE=
     1        '"DXF RAY" ONLY TAKES NUMERIC WORDS #1 AND #2 INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(STI.EQ.1) THEN
              WRITE(OUTLYNE,800)
              CALL SHOWIT(1)
 800          FORMAT('QUERRY (?) HAS NO MEANING WITH "DXF RAY"')
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
          IF(STPSUR.GE.STOPAT) STPSUR=STOPAT
          IF(STASUR.LT.NEWOBJ) STASUR=NEWOBJ
          IF(STPSUR.LT.NEWOBJ) STPSUR=NEWOBJ
          IF(STPSUR.LT.STASUR) STPSUR=STASUR
C
C     RIGHT NOW,COORDINATES ARE IN WORLD COORDINATES
C
          IF(.NOT.DXFEXIS) DXFEXIS=.TRUE.
C
C     NOW ISSUE THE PLOTTING COMMANDS STORED IN THE P1ARAY ARRAY
C
          DEALLOCATE(DXF_X,DXF_Y,DXF_Z
     1    ,STAT=ALLOERR)
          ALLOCATE(DXF_X(1:MAXSUR+1),DXF_Y(1:MAXSUR+1),DXF_Z(1:MAXSUR+1)
     1    ,STAT=ALLOERR)
          DO I=STASUR,STPSUR
              GLVIRT(I)=.FALSE.
C     IF THE CURRENT SURFACE IS A TRUE DUMMY
              IF(DUM(I).AND.ALENS(34,I).EQ.0.0D0
     1        .AND.I.NE.NEWIMG) GLVIRT(I)=.TRUE.
              IF(I.EQ.NEWIMG.OR.I.EQ.STASUR.OR.I.EQ.STPSUR) GLVIRT(I)=.FALSE.
          END DO
          K=1
          DO I=STASUR,STPSUR
              IF(NOVIRT.AND.GLVIRT(I).AND.I.NE.STPSUR.AND.DF2.EQ.1) THEN
              ELSE
                  DXF_X(K)=REAL(GLRAY(1,I))
                  DXF_Y(K)=REAL(GLRAY(2,I))
                  DXF_Z(K)=REAL(GLRAY(3,I))
                  NUMPEE=K
                  K=K+1
              END IF
          END DO
          CALL DXPOLY3(DXF_X,DXF_Y,DXF_Z,NUMPEE,0)
          DEALLOCATE(DXF_X,DXF_Y,DXF_Z
     1    ,STAT=ALLOERR)
          RETURN
      END


C SUB DXFVERTLINE.FOR
      SUBROUTINE DXFVERTLINE
          USE GLOBALS
C
          IMPLICIT NONE
C
C       THIS ROUTINE DOES THE PLOT LINE CONNECTING SURFACE VERTICES
C
          REAL*8 X,Y
C
          !     LOGICAL GGO
C
          INTEGER NUMPEE,IOP,ALLOERR,K,I
C
          REAL*8 DXF_X,DXF_Y,DXF_Z
          DIMENSION DXF_X(:),DXF_Y(:),DXF_Z(:)
          ALLOCATABLE:: DXF_X,DXF_Y,DXF_Z
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
          X=0.0D0
          Y=0.0D0
C
C     MAKE SURE YOU HAVE VERTEX DATA WITHOUT OFFSETS AND SET TO
C     THE FIRST SURFACE WITHOUT AN INFINITE THICKNESS
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
          DEALLOCATE(DXF_X,DXF_Y,DXF_Z
     1    ,STAT=ALLOERR)
          ALLOCATE(DXF_X(1:MAXSUR+1),DXF_Y(1:MAXSUR+1),DXF_Z(1:MAXSUR+1)
     1    ,STAT=ALLOERR)
          DO I=STASUR,STPSUR
              GLVIRT(I)=.FALSE.
C     IF THE CURRENT SURFACE IS A TRUE DUMMY
              IF(DUM(I).AND.ALENS(34,I).EQ.0.0D0
     1        .AND.I.NE.NEWIMG) GLVIRT(I)=.TRUE.
              IF(I.EQ.NEWIMG.OR.I.EQ.STASUR.OR.I.EQ.STPSUR) GLVIRT(I)=.FALSE.
          END DO
          K=1
          IF(STPSUR.LT.STASUR) STPSUR=STASUR
          DO I=STASUR,STPSUR
              DXF_X(K)=REAL(VERTEX(1,I))
              DXF_Y(K)=REAL(VERTEX(2,I))
              DXF_Z(K)=REAL(VERTEX(3,I))
              NUMPEE=K
              K=K+1
C     RIGHT NOW,COORDINATES ARE IN WORLD 3D-COORDINATES
          END DO
          IOP=0
          CALL DXPOLY3(DXF_X,DXF_Y,DXF_Z,NUMPEE,IOP)
          DEALLOCATE(DXF_X,DXF_Y,DXF_Z,STAT=ALLOERR)
          RETURN
      END


C SUB DXFEDG.FOR
      SUBROUTINE DXFEDG
          USE GLOBALS
C
          IMPLICIT NONE
C
C       THIS ROUTINE DOES THE DXF EDGEX/EDGEY COMMAND AT THE CMD LEVEL
C
          REAL*8 X,Y,Z,XX1,XX2,YY1
     1
     2    ,Z1,YY2
     3    ,X00,Y00,Z0,LX0,LY0,LZ0
     4    ,X1,X2,Y1,Y2,MX0,MY0,MZ0,NX0,NY0,NZ0
C
          INTEGER M1,M2,M3,CAFLG,COFLG,IK,III,NO,ALLOERR
C
          REAL*8 XLFT,YLFT,XRHT,YRHT,XTOP,YTOP,XBOT,YBOT
     1    ,XLFTO,YLFTO,XRHTO,YRHTO,XTOPO,YTOPO,XBOTO,YBOTO
     2    ,YLFT2,XLFT2,YRHT2,XRHT2,XTOP2,YTOP2,XBOT2,YBOT2,ZDELZ
C
          INTEGER I,II,IPST,J
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
          REAL*8 EDGE
          INTEGER IPX
          DIMENSION EDGE(:,:,:),IPX(:)
          ALLOCATABLE :: EDGE,IPX
          REAL*8 DXF_X,DXF_Y,DXF_Z
          DIMENSION DXF_X(:),DXF_Y(:),DXF_Z(:)
          ALLOCATABLE :: DXF_X,DXF_Y,DXF_Z
C
          M1=4
          M2=0
          M3=INT(SYSTEM1(20))
          DEALLOCATE (EDGE,STAT=ALLOERR)
          ALLOCATE (EDGE(M1,M1,M2:M3),STAT=ALLOERR)
          DEALLOCATE(DXF_X,STAT=ALLOERR)
          DEALLOCATE(DXF_Y,STAT=ALLOERR)
          DEALLOCATE(DXF_Z,STAT=ALLOERR)
          DEALLOCATE(IPX,STAT=ALLOERR)
          ALLOCATE(DXF_X(1:2),STAT=ALLOERR)
          ALLOCATE(DXF_Y(1:2),STAT=ALLOERR)
          ALLOCATE(DXF_Z(1:2),STAT=ALLOERR)
          ALLOCATE(IPX(0:MAXSUR),STAT=ALLOERR)
          X=0.0D0
          Y=0.0D0
          EDGE(1:4,1:4,0:M3)=0.0
          IPX(0:MAXSUR)=0
C
          IF(MSG) THEN
              OUTLYNE='GENERATING SURFACE EDGE DATA...'
              CALL SHOWIT(1)
          END IF
C
C       CHECK SYNTAX
          IF(SST.EQ.1) THEN
              IF(WQ.EQ.'EDGEX')OUTLYNE=
     1        '"DXF EDGEX" TAKES NO STRING INPUT'
              IF(WQ.EQ.'EDGEY')OUTLYNE=
     1        '"DXF EDGEY" TAKES NO STRING INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              DEALLOCATE(EDGE,STAT=ALLOERR)
              DEALLOCATE(DXF_X,STAT=ALLOERR)
              DEALLOCATE(DXF_Y,STAT=ALLOERR)
              DEALLOCATE(DXF_Z,STAT=ALLOERR)
              DEALLOCATE(IPX,STAT=ALLOERR)
              RETURN
          END IF
          IF(S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
              IF(WQ.EQ.'EDGEX')OUTLYNE=
     1        '"DXF EDGEX" ONLY TAKES NUMERIC WORDS #1 AND #2 INPUT'
              IF(WQ.EQ.'EDGEY')OUTLYNE=
     1        '"DXF EDGEY" ONLY TAKES NUMERIC WORDS #1 AND #2 INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              DEALLOCATE(EDGE,STAT=ALLOERR)
              DEALLOCATE(DXF_X,STAT=ALLOERR)
              DEALLOCATE(DXF_Y,STAT=ALLOERR)
              DEALLOCATE(DXF_Z,STAT=ALLOERR)
              DEALLOCATE(IPX,STAT=ALLOERR)
              RETURN
          END IF
          IF(STI.EQ.1) THEN
              IF(WQ.EQ.'EDGEX')WRITE(OUTLYNE,800)
              IF(WQ.EQ.'EDGEY')WRITE(OUTLYNE,801)
              CALL SHOWIT(1)
 800          FORMAT('QUERRY (?) HAS NO MEANING WITH "DXF EDGEX"')
 801          FORMAT('QUERRY (?) HAS NO MEANING WITH "DXF EDGEY"')
              DEALLOCATE(EDGE,STAT=ALLOERR)
              DEALLOCATE(DXF_X,STAT=ALLOERR)
              DEALLOCATE(DXF_Y,STAT=ALLOERR)
              DEALLOCATE(DXF_Z,STAT=ALLOERR)
              DEALLOCATE(IPX,STAT=ALLOERR)
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
     1        'SURFACE NUMBER (NUMERIC WORD #1) IS BEYOND LEGAL RANGE'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              DEALLOCATE(EDGE,STAT=ALLOERR)
              DEALLOCATE(DXF_X,STAT=ALLOERR)
              DEALLOCATE(DXF_Y,STAT=ALLOERR)
              DEALLOCATE(DXF_Z,STAT=ALLOERR)
              DEALLOCATE(IPX,STAT=ALLOERR)
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
              DEALLOCATE(EDGE,STAT=ALLOERR)
              DEALLOCATE(DXF_X,STAT=ALLOERR)
              DEALLOCATE(DXF_Y,STAT=ALLOERR)
              DEALLOCATE(DXF_Z,STAT=ALLOERR)
              DEALLOCATE(IPX,STAT=ALLOERR)
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
              DEALLOCATE(EDGE,STAT=ALLOERR)
              DEALLOCATE(DXF_X,STAT=ALLOERR)
              DEALLOCATE(DXF_Y,STAT=ALLOERR)
              DEALLOCATE(DXF_Z,STAT=ALLOERR)
              DEALLOCATE(IPX,STAT=ALLOERR)
              RETURN
          END IF
C
C       ALL INPUT IS OK, KEEP GOING
C     THE ARRAY CONTAINING SURFACE EDGE DATA ARE:
C
C     EDGE(1:4,1:3,0:MAXSUR)
C
C     THE FIRST DIMENSION IS FOR THE DATA POINT NUMBER
C     FOR YEDGE
C               1=BOT   (CLAP)
C               2=TOP   (CLAP)
C               3=BOT   (COBS)
C               4=TOP   (COBS)
C     FOR YEDGE
C               1=LFT   (CLAP)
C               2=RHT   (CLAP)
C               3=LFT   (COBS)
C               4=RHT   (COBS)
C     THE SECOND DIMENSION IS FOR THE X,Y AND Z COORDINATES OR THE POINT
C
C     WE NEED TO LOAD THE ARRAY BEFORE DXFING
C
C     THE PROCEDURE IS:
C     1. DETERMINE THE MAXIMUM AND MINIMUM LOCAL X AND Y
c               COORDINATES FOR A SURFACE FOR WHICH THE EDGE
C               IS TO BE CALCULATED.
C
C     CYCLE THROUGH ALL THE SURFACES
C
          DO II=STASUR,STPSUR
              X1=0.0D0
              X2=0.0D0
              Y1=0.0D0
              Y2=0.0D0
              XX1=0.0D0
              XX2=0.0D0
              YY1=0.0D0
              YY2=0.0D0
              XLFT=0.0D0
              YLFT=0.0D0
              XRHT=0.0D0
              YRHT=0.0D0
              XBOT=0.0D0
              YBOT=0.0D0
              XTOP=0.0D0
              YTOP=0.0D0
              XLFTO=0.0D0
              YLFTO=0.0D0
              XRHTO=0.0D0
              YRHTO=0.0D0
              XBOTO=0.0D0
              YBOTO=0.0D0
              XTOPO=0.0D0
              YTOPO=0.0D0
              CAFLG=0
              COFLG=0
              III=II
C
C     NOW FOR THE II SURFACE WE CALCULATE ALL THE END POINTS
C
              CALL CAO(YLFT,XLFT,YRHT,XRHT,XTOP,YTOP,XBOT,YBOT,
     1        YLFTO,XLFTO,YRHTO,XRHTO,XTOPO,YTOPO,XBOTO,YBOTO,CAFLG,
     2        COFLG,III
     3        ,YLFT2,XLFT2,YRHT2,XRHT2,XTOP2,YTOP2,XBOT2,YBOT2,ZDELZ)
C
C
              IF(WQ.EQ.'EDGEY') THEN
                  EDGE(1,1,II)=XBOT
                  EDGE(1,2,II)=YBOT
                  EDGE(2,1,II)=XTOP
                  EDGE(2,2,II)=YTOP
                  EDGE(3,1,II)=XBOTO
                  EDGE(3,2,II)=YBOTO
                  EDGE(4,1,II)=XTOPO
                  EDGE(4,2,II)=YTOPO
              ELSE
              END IF
C
              IF(WQ.EQ.'EDGEX') THEN
                  EDGE(1,1,II)=XLFT
                  EDGE(1,2,II)=YLFT
                  EDGE(2,1,II)=XRHT
                  EDGE(2,2,II)=YRHT
                  EDGE(3,1,II)=XLFTO
                  EDGE(3,2,II)=YLFTO
                  EDGE(4,1,II)=XRHTO
                  EDGE(4,2,II)=YRHTO
              ELSE
              END IF
C
C     NOW WE HAVE THE END POINTS, CYCLE THROUGH ALL FOUR PAIRS
C
C     2. USE APPROPRIATE CALLS TO THE SAGPLT.FOR ROUTINE
C               TO CALCULATE THE SAG AND MAKE
C               CETRAIN THE SIGN IS CORRECT FOR A LOCAL Z COORDINATE
C
              IF(WQ.EQ.'EDGEX') THEN
                  X1=XLFT2
                  Y1=YLFT2
                  X2=XRHT2
                  Y2=YRHT2
                  XX1=XLFTO
                  YY1=YLFTO
                  XX2=XRHTO
                  YY2=YRHTO
              ELSE
              END IF
              IF(WQ.EQ.'EDGEY') THEN
                  X1=XBOT2
                  Y1=YBOT2
                  X2=XTOP2
                  Y2=YTOP2
                  XX1=XBOTO
                  YY1=YBOTO
                  XX2=XTOPO
                  YY2=YTOPO
              ELSE
              END IF
              CALL SAGPLT(III,X1,Y1,Z,NO)
              EDGE(1,3,II)=Z+ZDELZ
              CALL SAGPLT(III,X2,Y2,Z,NO)
              EDGE(2,3,II)=Z+ZDELZ
              CALL SAGPLT(III,XX1,YY1,Z,NO)
              EDGE(3,3,II)=Z+ZDELZ
              CALL SAGPLT(III,XX2,YY2,Z,NO)
              EDGE(4,3,II)=Z+ZDELZ
C
C               CYCLE THROUGH THE NEXT SURFACE
          END DO
C
C     3. THE ARRAYS NOW HAVE LOCAL X,Y AND Z VALUES STORED IN THEM
C     CONVERT THE LOCAL X ANY Y EDGES TO GLOBAL NUMBERS
C     GLOBAL VERTEX DATA IS
          DO II=STASUR,STPSUR
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
              X=EDGE(1,1,II)
              Y=EDGE(1,2,II)
              Z=EDGE(1,3,II)
C
              X1=X00+((LX0*(X))+(LY0*(Y))
     1        +(LZ0*(Z)))
              Y1=Y00+((MX0*(X))+(MY0*(Y))
     1        +(MZ0*(Z)))
              Z1=Z0+((NX0*(X))+(NY0*(Y))
     1        +(NZ0*(Z)))
              EDGE(1,1,II)=X1
              EDGE(1,2,II)=Y1
              EDGE(1,3,II)=Z1
              X=EDGE(2,1,II)
              Y=EDGE(2,2,II)
              Z=EDGE(2,3,II)
C
              X1=X00+((LX0*(X))+(LY0*(Y))
     1        +(LZ0*(Z)))
              Y1=Y00+((MX0*(X))+(MY0*(Y))
     1        +(MZ0*(Z)))
              Z1=Z0+((NX0*(X))+(NY0*(Y))
     1        +(NZ0*(Z)))
              EDGE(2,1,II)=X1
              EDGE(2,2,II)=Y1
              EDGE(2,3,II)=Z1
              X=EDGE(3,1,II)
              Y=EDGE(3,2,II)
              Z=EDGE(3,3,II)
C
              X1=X00+((LX0*(X))+(LY0*(Y))
     1        +(LZ0*(Z)))
              Y1=Y00+((MX0*(X))+(MY0*(Y))
     1        +(MZ0*(Z)))
              Z1=Z0+((NX0*(X))+(NY0*(Y))
     1        +(NZ0*(Z)))
              EDGE(3,1,II)=X1
              EDGE(3,2,II)=Y1
              EDGE(3,3,II)=Z1
              X=EDGE(4,1,II)
              Y=EDGE(4,2,II)
              Z=EDGE(4,3,II)
C
              X1=X00+((LX0*(X))+(LY0*(Y))
     1        +(LZ0*(Z)))
              Y1=Y00+((MX0*(X))+(MY0*(Y))
     1        +(MZ0*(Z)))
              Z1=Z0+((NX0*(X))+(NY0*(Y))
     1        +(NZ0*(Z)))
              EDGE(4,1,II)=X1
              EDGE(4,2,II)=Y1
              EDGE(4,3,II)=Z1
          END DO
C
          IF(.NOT.DXFEXIS) DXFEXIS=.TRUE.
C
          IF(WQ.EQ.'EDGEX') THEN
C     FIRST DO THE EDGES OF THE CLAPS
C     DRAW THE X EDGES AND RETURN
              DO J=1,4
                  IF(J.EQ.1.OR.J.EQ.2) THEN
                      DO I=STASUR,STPSUR
                          IF(I.EQ.0) THEN
                              IPST=0
                              IPX(I)=IPST
                          ELSE
C                       NOT OBJECT
C
C     NOW DRAW THE X EDGES OF THE CLAP AT SURFACE I
C     WITH THE PEN UP, GO TO THE STARTING PLOT POSITION
C
C     IF THE CURRENT SURFACE IS PRECEEDED BY AIR OR REFL
C     AND ALL THE REFRACTIVE INDICES ARE 1 OR -1, DON'T
C     DRAW THE EDGE. IN ALL OTHER CASES, DRAW THE EDGE.
                              IF(GLANAM(I-1,2).EQ.'AIR          '.OR.
     1                        GLANAM(I-1,2).EQ.'PERFECT      '.OR.
     1                        GLANAM(I-1,2).EQ.'IDEAL        '.OR.
     1                        GLANAM(I-1,2).EQ.'REFLTIRO     '.OR.
     1                        GLANAM(I-1,2).EQ.'REFLTIR      '.OR.
     2                        GLANAM(I-1,2).EQ.'REFL         '.AND.
     3                        DABS(ALENS(46,I-1)).EQ.1.0D0.AND.
     4                        DABS(ALENS(47,I-1)).EQ.1.0D0.AND.
     5                        DABS(ALENS(48,I-1)).EQ.1.0D0.AND.
     6                        DABS(ALENS(49,I-1)).EQ.1.0D0.AND.
     7                        DABS(ALENS(50,I-1)).EQ.1.0D0) THEN
                                  IPST=0
                                  IPX(I)=IPST
                              ELSE
                                  IPST=1
                                  IF(I.EQ.STASUR.AND.GLANAM(I,2).EQ.'AIR') IPST=0
                                  IPX(I)=IPST
                              END IF
                          END IF
                          IF(GLANAM(I-1,1).EQ.'MYGLASS') THEN
                              IPST=1
                              IPX(I)=IPST
                          END IF
C
                          IF(.NOT.DXFEXIS) DXFEXIS=.TRUE.
                      END DO
C     FINISHED WITH THAT EDGE, LIFT PEN
                      IPST=0
C     NOW ISSUE THE PLOTTING COMMANDS STORED IN THE ARRAY
C
                      DO IK=STASUR,STPSUR-1
                          IF(ALENS(127,IK).NE.0.0D0.OR.IK.NE.STASUR.AND.
     1                    ALENS(127,IK-1).NE.0.0D0) THEN
                          ELSE
                              IF(IPX(IK+1).EQ.1) THEN
C     DRAW A LINE
                                  DXF_X(1)=EDGE(1,1,IK)
                                  DXF_Y(1)=EDGE(1,2,IK)
                                  DXF_Z(1)=EDGE(1,3,IK)
                                  DXF_X(2)=EDGE(1,1,IK+1)
                                  DXF_Y(2)=EDGE(1,2,IK+1)
                                  DXF_Z(2)=EDGE(1,3,IK+1)
                                  CALL DXPOLY3(DXF_X,DXF_Y,DXF_Z,2,0)
                              END IF
                          END IF
                      END DO
                      DO IK=STASUR,STPSUR-1
                          IF(ALENS(127,IK).NE.0.0D0.OR.IK.NE.STASUR.AND.
     1                    ALENS(127,IK-1).NE.0.0D0) THEN
                          ELSE
                              IF(IPX(IK+1).EQ.1) THEN
C     DRAW A LINE
                                  DXF_X(1)=EDGE(2,1,IK)
                                  DXF_Y(1)=EDGE(2,2,IK)
                                  DXF_Z(1)=EDGE(2,3,IK)
                                  DXF_X(2)=EDGE(2,1,IK+1)
                                  DXF_Y(2)=EDGE(2,2,IK+1)
                                  DXF_Z(2)=EDGE(2,3,IK+1)
                                  CALL DXPOLY3(DXF_X,DXF_Y,DXF_Z,2,0)
                              END IF
                          END IF
                      END DO
C     NOW DO THE EDGES OF THE COBS
                  ELSE
C     J NOT 1 OR 2
                  END IF
C
                  IF(J.EQ.3.OR.J.EQ.4) THEN

C     DRAW THE X EDGES AND RETURN
                      DO I=STASUR,STPSUR
C
C     NOW DRAW THE X EDGES OF THE COBS AT SURFACE I
C     WITH THE PEN UP, GO TO THE STARTING PLOT POSITION
C
                          IF(I.EQ.0) THEN
                              IPST=0
                              IPX(I)=IPST
                          ELSE
C                       NOT OBJECT
C
C     NOW DRAW THE X EDGES OF THE CLAP AT SURFACE I
C     WITH THE PEN UP, GO TO THE STARTING PLOT POSITION
C
C     IF THE CURRENT SURFACE IS PRECEEDED BY AIR OR REFL
C     AND ALL THE REFRACTIVE INDICES ARE 1 OR -1, DON'T
C     DRAW THE EDGE. IN ALL OTHER CASES, DRAW THE EDGE.
                              IF(GLANAM(I-1,2).EQ.'AIR          '.OR.
     1                        GLANAM(I-1,2).EQ.'PERFECT      '.OR.
     1                        GLANAM(I-1,2).EQ.'IDEAL        '.OR.
     1                        GLANAM(I-1,2).EQ.'REFLTIRO     '.OR.
     1                        GLANAM(I-1,2).EQ.'REFLTIR      '.OR.
     2                        GLANAM(I-1,2).EQ.'REFL         '.AND.
     3                        DABS(ALENS(46,I-1)).EQ.1.0D0.AND.
     4                        DABS(ALENS(47,I-1)).EQ.1.0D0.AND.
     5                        DABS(ALENS(48,I-1)).EQ.1.0D0.AND.
     6                        DABS(ALENS(49,I-1)).EQ.1.0D0.AND.
     7                        DABS(ALENS(50,I-1)).EQ.1.0D0) THEN
                                  IPST=0
                                  IPX(I)=IPST
                              ELSE
                                  IF(ALENS(16,I).NE.0.0D0.AND.ALENS(16,I-1).NE.0.0D0) THEN
                                      IPST=1
                                      IF(I.EQ.STASUR.AND.GLANAM(I,2).EQ.'AIR') IPST=0
                                      IPX(I)=IPST
                                  ELSE
                                      IPST=0
                                      IPX(I)=IPST
                                  END IF
                              END IF
                          END IF
                          IF(GLANAM(I-1,1).EQ.'MYGLASS') THEN
                              IPST=1
                              IPX(I)=IPST
                          END IF
                          IF(.NOT.DXFEXIS) DXFEXIS=.TRUE.
                      END DO
C     FINISHED WITH THAT EDGE, LIFT PEN
                      IPST=0
C     NOW ISSUE THE PLOTTING COMMANDS STORED IN THE ARRAY
C
                      DO IK=STASUR,STPSUR-1
                          IF(ALENS(127,IK).NE.0.0D0.OR.IK.NE.STASUR.AND.
     1                    ALENS(127,IK-1).NE.0.0D0) THEN
                          ELSE
                              IF(IPX(IK+1).EQ.1) THEN
C     DRAW A LINE
                                  DXF_X(1)=EDGE(3,1,IK)
                                  DXF_Y(1)=EDGE(3,2,IK)
                                  DXF_Z(1)=EDGE(3,3,IK)
                                  DXF_X(2)=EDGE(3,1,IK+1)
                                  DXF_Y(2)=EDGE(3,2,IK+1)
                                  DXF_Z(2)=EDGE(3,3,IK+1)
                                  CALL DXPOLY3(DXF_X,DXF_Y,DXF_Z,2,0)
                              END IF
                          END IF
                      END DO
                      DO IK=STASUR,STPSUR-1
                          IF(ALENS(127,IK).NE.0.0D0.OR.IK.NE.STASUR.AND.
     1                    ALENS(127,IK-1).NE.0.0D0) THEN
                          ELSE
                              IF(IPX(IK+1).EQ.1) THEN
C     DRAW A LINE
                                  DXF_X(1)=EDGE(4,1,IK)
                                  DXF_Y(1)=EDGE(4,2,IK)
                                  DXF_Z(1)=EDGE(4,3,IK)
                                  DXF_X(2)=EDGE(4,1,IK+1)
                                  DXF_Y(2)=EDGE(4,2,IK+1)
                                  DXF_Z(2)=EDGE(4,3,IK+1)
                                  CALL DXPOLY3(DXF_X,DXF_Y,DXF_Z,2,0)
                              END IF
                          END IF
                      END DO
                  ELSE
C     J NOT 3 OR 4
                  END IF
              END DO
          ELSE
C     NOT EDGEX
          END IF
C
          IF(WQ.EQ.'EDGEY') THEN
C     FIRST DO THE EDGES OF THE CLAPS
C     DRAW THE Y EDGES AND RETURN
              DO J=1,4
                  IF(J.EQ.1.OR.J.EQ.2) THEN
                      DO I=STASUR,STPSUR
                          IF(I.EQ.0) THEN
                              IPST=0
                              IPX(I)=IPST
                          ELSE
C                       NOT OBJECT
C
C     NOW DRAW THE Y EDGES OF THE CLAP AT SURFACE I
C     WITH THE PEN UP, GO TO THE STARTING PLOT POSITION
C
C     IF THE CURRENT SURFACE IS PRECEEDED BY AIR OR REFL
C     AND ALL THE REFRACTIVE INDICES ARE 1 OR -1, DON'T
C     DRAW THE EDGE. IN ALL OTHER CASES, DRAW THE EDGE.
                              IF(GLANAM(I-1,2).EQ.'AIR          '.OR.
     1                        GLANAM(I-1,2).EQ.'PERFECT      '.OR.
     1                        GLANAM(I-1,2).EQ.'IDEAL        '.OR.
     1                        GLANAM(I-1,2).EQ.'REFLTIRO     '.OR.
     1                        GLANAM(I-1,2).EQ.'REFLTIR      '.OR.
     2                        GLANAM(I-1,2).EQ.'REFL         '.AND.
     3                        DABS(ALENS(46,I-1)).EQ.1.0D0.AND.
     4                        DABS(ALENS(47,I-1)).EQ.1.0D0.AND.
     5                        DABS(ALENS(48,I-1)).EQ.1.0D0.AND.
     6                        DABS(ALENS(49,I-1)).EQ.1.0D0.AND.
     7                        DABS(ALENS(50,I-1)).EQ.1.0D0) THEN
                                  IPST=0
                                  IPX(I)=IPST
                              ELSE
                                  IPST=1
                                  IF(I.EQ.STASUR.AND.GLANAM(I,2).EQ.'AIR') IPST=0
                                  IPX(I)=IPST
                              END IF
                          END IF
                          IF(GLANAM(I-1,1).EQ.'MYGLASS') THEN
                              IPST=1
                              IPX(I)=IPST
                          END IF
C
                          IF(.NOT.DXFEXIS) DXFEXIS=.TRUE.
                      END DO
C     FINISHED WITH THAT EDGE, LIFT PEN
                      IPST=0
C     NOW ISSUE THE PLOTTING COMMANDS STORED IN THE ARRAY
C
                      DO IK=STASUR,STPSUR-1
                          IF(ALENS(127,IK).NE.0.0D0.OR.IK.NE.STASUR.AND.
     1                    ALENS(127,IK-1).NE.0.0D0) THEN
                          ELSE
                              IF(IPX(IK+1).EQ.1) THEN
C     DRAW A LINE
                                  DXF_X(1)=EDGE(1,1,IK)
                                  DXF_Y(1)=EDGE(1,2,IK)
                                  DXF_Z(1)=EDGE(1,3,IK)
                                  DXF_X(2)=EDGE(1,1,IK+1)
                                  DXF_Y(2)=EDGE(1,2,IK+1)
                                  DXF_Z(2)=EDGE(1,3,IK+1)
                                  CALL DXPOLY3(DXF_X,DXF_Y,DXF_Z,2,0)
                              END IF
                          END IF
                      END DO
                      DO IK=STASUR,STPSUR-1
                          IF(ALENS(127,IK).NE.0.0D0.OR.IK.NE.STASUR.AND.
     1                    ALENS(127,IK-1).NE.0.0D0) THEN
                          ELSE
                              IF(IPX(IK+1).EQ.1) THEN
C     DRAW A LINE
                                  DXF_X(1)=EDGE(2,1,IK)
                                  DXF_Y(1)=EDGE(2,2,IK)
                                  DXF_Z(1)=EDGE(2,3,IK)
                                  DXF_X(2)=EDGE(2,1,IK+1)
                                  DXF_Y(2)=EDGE(2,2,IK+1)
                                  DXF_Z(2)=EDGE(2,3,IK+1)
                                  CALL DXPOLY3(DXF_X,DXF_Y,DXF_Z,2,0)
                              END IF
                          END IF
                      END DO
                  ELSE
C     J NOT 1 OR 2
                  END IF
C
                  IF(J.EQ.3.OR.J.EQ.4) THEN

C     DRAW THE Y EDGES AND RETURN
                      DO I=STASUR,STPSUR
C
C     NOW DRAW THE Y EDGES OF THE COBS AT SURFACE I
C     WITH THE PEN UP, GO TO THE STARTING PLOT POSITION
C
                          IF(I.EQ.0) THEN
                              IPST=0
                              IPX(I)=IPST
                          ELSE
C                       NOT OBJECT
C
C     NOW DRAW THE X EDGES OF THE CLAP AT SURFACE I
C     WITH THE PEN UP, GO TO THE STARTING PLOT POSITION
C
C     IF THE CURRENT SURFACE IS PRECEEDED BY AIR OR REFL
C     AND ALL THE REFRACTIVE INDICES ARE 1 OR -1, DON'T
C     DRAW THE EDGE. IN ALL OTHER CASES, DRAW THE EDGE.
                              IF(GLANAM(I-1,2).EQ.'AIR          '.OR.
     1                        GLANAM(I-1,2).EQ.'PERFECT      '.OR.
     1                        GLANAM(I-1,2).EQ.'IDEAL        '.OR.
     1                        GLANAM(I-1,2).EQ.'REFLTIR      '.OR.
     1                        GLANAM(I-1,2).EQ.'REFLTIRO     '.OR.
     2                        GLANAM(I-1,2).EQ.'REFL         '.AND.
     3                        DABS(ALENS(46,I-1)).EQ.1.0D0.AND.
     4                        DABS(ALENS(47,I-1)).EQ.1.0D0.AND.
     5                        DABS(ALENS(48,I-1)).EQ.1.0D0.AND.
     6                        DABS(ALENS(49,I-1)).EQ.1.0D0.AND.
     7                        DABS(ALENS(50,I-1)).EQ.1.0D0) THEN
                                  IPST=0
                                  IPX(I)=IPST
                              ELSE
                                  IF(ALENS(16,I).NE.0.0D0.AND.ALENS(16,I-1).NE.0.0D0) THEN
                                      IPST=1
                                      IF(I.EQ.STASUR.AND.GLANAM(I,2).EQ.'AIR') IPST=0
                                      IPX(I)=IPST
                                  ELSE
                                      IPST=0
                                      IPX(I)=IPST
                                  END IF
                              END IF
                          END IF
                          IF(GLANAM(I-1,1).EQ.'MYGLASS') THEN
                              IPST=1
                              IPX(I)=IPST
                          END IF
                          IF(.NOT.DXFEXIS) DXFEXIS=.TRUE.
                      END DO
C     FINISHED WITH THAT EDGE, LIFT PEN
                      IPST=0
C     NOW ISSUE THE PLOTTING COMMANDS STORED IN THE ARRAY
C
                      DO IK=STASUR,STPSUR-1
                          IF(ALENS(127,IK).NE.0.0D0.OR.IK.NE.STASUR.AND.
     1                    ALENS(127,IK-1).NE.0.0D0) THEN
                          ELSE
                              IF(IPX(IK+1).EQ.1) THEN
C     DRAW A LINE
                                  DXF_X(1)=EDGE(3,1,IK)
                                  DXF_Y(1)=EDGE(3,2,IK)
                                  DXF_Z(1)=EDGE(3,3,IK)
                                  DXF_X(2)=EDGE(3,1,IK+1)
                                  DXF_Y(2)=EDGE(3,2,IK+1)
                                  DXF_Z(2)=EDGE(3,3,IK+1)
                                  CALL DXPOLY3(DXF_X,DXF_Y,DXF_Z,2,0)
                              END IF
                          END IF
                      END DO
                      DO IK=STASUR,STPSUR-1
                          IF(ALENS(127,IK).NE.0.0D0.OR.IK.NE.STASUR.AND.
     1                    ALENS(127,IK-1).NE.0.0D0) THEN
                          ELSE
                              IF(IPX(IK+1).EQ.1) THEN
C     DRAW A LINE
                                  DXF_X(1)=EDGE(4,1,IK)
                                  DXF_Y(1)=EDGE(4,2,IK)
                                  DXF_Z(1)=EDGE(4,3,IK)
                                  DXF_X(2)=EDGE(4,1,IK+1)
                                  DXF_Y(2)=EDGE(4,2,IK+1)
                                  DXF_Z(2)=EDGE(4,3,IK+1)
                                  CALL DXPOLY3(DXF_X,DXF_Y,DXF_Z,2,0)
                              END IF
                          END IF
                      END DO
                  ELSE
C     J NOT 3 OR 4
                  END IF
              END DO
          ELSE
C     NOT EDGEY
          END IF
          DEALLOCATE(EDGE,STAT=ALLOERR)
          DEALLOCATE(DXF_X,STAT=ALLOERR)
          DEALLOCATE(DXF_Y,STAT=ALLOERR)
          DEALLOCATE(DXF_Z,STAT=ALLOERR)
          DEALLOCATE(IPX,STAT=ALLOERR)
          RETURN
      END
C SUB DXFDEV.FOR
      SUBROUTINE DXFDEV
          USE GLOBALS
C
          IMPLICIT NONE
C
C       THIS ROUTINE DOES THE "DXF NEW" COMMAND AT THE CMD
C       AND SPECT PROGRAM LEVELS
C
          LOGICAL OPEN39
C
          CHARACTER LAYERPASS*8
C
          INTEGER I
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
C       THIS ROUTINE SETS DXFSET AND DEVTYP
          GLSURF=-99
          DO I=NEWIMG,NEWOBJ,-1
              IF(DABS(ALENS(3,I)).LE.1.0D10) GLSURF=I
          END DO
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
          IF(GLSURF.EQ.-99) THEN
              GLOBE=.FALSE.
              OUTLYNE='ALL SURFACES WERE OF INFINITE THICKNESS'
              CALL SHOWIT(1)
              OUTLYNE='NO OPTICAL SYSTEM DXF FILE COULD BE MADE'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          DEVTYP=2
          DXFSET=.TRUE.
          LAYERPASS='LAYER001'
          LAYER='LAYER001'
          OPEN39=.FALSE.
          INQUIRE(FILE=trim(HOME)//'DXF3D.DXF',OPENED=OPEN39)
          IF(OPEN39) CALL CLOSE_FILE(39,0)
          DXFEXIS=.FALSE.
          CALL DXO()
          CALL DXL(LAYERPASS)
          RETURN
      END
C SUB DXFCLP.FOR
      SUBROUTINE DXFCLP(CLPTYPE,SURFACEI,SFI,MDX,MDY,GAMGAM)
C
          IMPLICIT NONE
C
C       THIS ROUTINE DOES THE DXF CLAP COMMAND
C
          REAL*8 X,Y,Z,MDX,MDY,SFI
     1
     2    ,Z1,ANGLE,AN2
     3    ,X00,Y00,Z0,LX0,LY0,LZ0,ZDELZ
     4    ,X1,Y1,MX0,MY0,MZ0,NX0,NY0,NZ0,XID,YID,GAMGAM
C
          INTEGER ALLOERR,JJ,I,II,III,J,L,M1,M2,M3,M4,M5,IIRUN,JJSTOP
C
          INTEGER P,NUMPEE,IOP,NO,CLPTYPE,SURFACEI
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
          REAL*8 CLPDAT,DXF_X,DXF_Y,DXF_Z
          DIMENSION CLPDAT(:,:,:,:),DXF_X(:),DXF_Y(:),DXF_Z(:)
          ALLOCATABLE :: CLPDAT,DXF_X,DXF_Y,DXF_Z

          M1=0
          M2=180
          M3=3
          M4=INT(SYSTEM1(20))
          M5=2
          DEALLOCATE(CLPDAT,STAT=ALLOERR)
          DEALLOCATE(DXF_X,STAT=ALLOERR)
          DEALLOCATE(DXF_Y,STAT=ALLOERR)
          DEALLOCATE(DXF_Z,STAT=ALLOERR)
          ALLOCATE(CLPDAT(M1:M2,M3,M1:M4,M5),STAT=ALLOERR)
          ALLOCATE(DXF_X(1:361),STAT=ALLOERR)
          ALLOCATE(DXF_Y(1:361),STAT=ALLOERR)
          ALLOCATE(DXF_Z(1:361),STAT=ALLOERR)
C
          II=SURFACEI
C
C     MAKE SURE YOU HAVE VERTEX DATA WITHOUT OFFSETS AND SET TO
C     THE FIRST SURFACE WITHOUT AN INFINITE THICKNESS
          DO IIRUN=1,2
              CLPDAT(1:180,1:3,0:M4,1:2)=0.0
              X=0.0D0
              Y=0.0D0
              XID=0.0D0
              YID=0.0D0
C
C       ALL INPUT IS OK, KEEP GOING
C     THE ARRAY CONTAINING SURFACE CLAP DATA IS:
C     CLPDAT(0:180,1:3,0:MAXSUR,1:2)
C
C     THE FIRST DIMENSION IS FOR THE DATA POINT NUMBER
C     THE SECOND DIMENSION IS FOR THE X,Y AND Z COORDINATES OR THE POINT
C     THE THIRD IS THE SURFACE NUMBER
C
C     WE NEED TO LOAD THE ARRAY BEFORE DXFING
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
                  DO J=0,180
                      ANGLE=(DBLE(2*J)*PII)/180.0D0
                      AN2=(DBLE((2*J)+2)*PII)/180.0D0
C     NOW ALONG THIS ANGLED LINE, WHAT ARE THE X AND Y COORDINATES
C     OF THE CLEAR APERTURE
                      III=II
C
                      CALL CAO1(X,Y,ANGLE,III,AN2,JJ,XID,YID,IIRUN,CLPTYPE,ZDELZ
     1                ,MDX,MDY,GAMGAM)
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
                  DO I=0,180
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
     1                +(LZ0*(Z)))
                      Y1=Y00+((MX0*(X))+(MY0*(Y))
     1                +(MZ0*(Z)))
                      Z1=Z0+((NX0*(X))+(NY0*(Y))
     1                +(NZ0*(Z)))
                      CLPDAT(I,1,II,JJ)=X1
                      CLPDAT(I,2,II,JJ)=Y1
                      CLPDAT(I,3,II,JJ)=Z1
                  END DO
              END DO
C
C     PUT THE CALLS TO THE DXF ROUTINES HERE FOR ALL THE SURFACES
              IOP=1
              IF(DUMMMY(II).AND.ALENS(9,II).EQ.0.0D0.AND.
     1        II.NE.STASUR.AND.II.NE.STPSUR) THEN
              ELSE
                  DO L=1,2
                      P=1
                      DO I=0,180
                          DXF_X(P)=CLPDAT(I,1,II,L)
                          DXF_Y(P)=CLPDAT(I,2,II,L)
                          DXF_Z(P)=CLPDAT(I,3,II,L)
                          NUMPEE=P
                          P=P+1
                      END DO
                      CALL DXPOLY3(DXF_X,DXF_Y,DXF_Z,NUMPEE,IOP)
                  END DO
              END IF
C
C     THIS LAST END DO ID FOR THE IIRUN LOOP
          END DO
C
          DEALLOCATE(CLPDAT,STAT=ALLOERR)
          DEALLOCATE(DXF_X,STAT=ALLOERR)
          DEALLOCATE(DXF_Y,STAT=ALLOERR)
          DEALLOCATE(DXF_Z,STAT=ALLOERR)
          RETURN
      END
