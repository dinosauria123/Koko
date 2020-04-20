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

      SUBROUTINE FULLIMAGING
          USE GLOBALS
!        USE WINTERACTER
          IMPLICIT NONE
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          INCLUDE 'datspd.inc'
          REAL*8 PEAKVAL,A,B,LLIM,PITVAL
          REAL*8 DNX,DNY,XCORR,YCORR
          REAL*8 WAVLN,XCHIEF,YCHIEF,X2,Y2,PSFXCORNER,PSFYCORNER
          REAL*8 PSFX,PSFY,SYSSUM,IWW3,IWW4,TRIMMER,WRD1
          CHARACTER*80 BMPFILE
          CHARACTER WWQW*8
          COMMON/FILEBMP/BMPFILE
          INTEGER NX,NY,ALLOERR,I,J,K,LENBMP,IX,IY,II,JJ,NSTART,NSTOP
          INTEGER L,M,N,P,KK,SL,SCL,KKK,SKIPX,SKIPY,SKIPIT,irec
C
C       SET THE SKIP THE PSF CALCULATION COUNTER TO 10 FOR IMTRACE3
          SKIPIT = 10
C
C       COLOR
C       G=1 CONTROL WAVELENGTH, SYSTEM1(11)
C       B=2 FIRST COLOR, SWCONDARY CHROMATIC PAIR, SYSTEM1(9)
C       R=3 SECOND COLOR, SECONDARY CHROMATIC PAIR, SYSTEM1(10)
C
          IF(WC.EQ.'COLOR') THEN
              IF(STI.EQ.1) THEN
                  IF(NUMCOLORS.EQ.1) WRITE(OUTLYNE,*)
     1            '8 BIT MONOCHROME IMAGERY IN EFFECT'
                  IF(NUMCOLORS.EQ.3) WRITE(OUTLYNE,*)
     1            '24 BIT RGB INAGERY IN EFFECT'
                  CALL SHOWIT(1)
                  RETURN
              END IF
              IF(SST.EQ.1.OR.SN.EQ.1) THEN
                  WRITE(OUTLYNE,*)
     1            '"COLOR" TAKES NO STRING OR NUMERIC INPUT'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(SQ.EQ.0) THEN
                  WRITE(OUTLYNE,*)
     1            '"COLOR" REQUIRES EXPLICIT QUALIFIER INPUT'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(WQ.NE.'MONO'.AND.WQ.NE.'RGB') THEN
                  WRITE(OUTLYNE,*)
     1            '"COLOR" TAKES EITHER "MONO" OR "RGB" FOR QUALIFIER INPUT'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(WQ.EQ.'MONO') NUMCOLORS=1
              IF(WQ.EQ.'RGB') NUMCOLORS=3
              IF(WQ.EQ.'MONO'.OR.WQ.EQ.'RGB') THEN
                  OBJNX=0
                  OBJNY=0
                  IMGNX=0
                  IMGNY=0
              END IF
          END IF
C
C       IMTRACE1
          IF(WC.EQ.'IMTRACE1') THEN
              IF(SQ.NE.0.OR.SST.NE.0) THEN
                  WRITE(OUTLYNE,*)
     1            WC,' TAKES NO QUALIFIER OR STRING INPUT'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(SN.EQ.1) THEN
                  WRITE(OUTLYNE,*)
     1            WC,' TAKES NO NUMERIC INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)
     1            'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(SYSTEM1(30).GT.2.0D0) THEN
                  WRITE(OUTLYNE,*)
     1            WC,' REQUIRES "FOCAL" OR "UFOCAL" MODE'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(OBJNX.EQ.0.OR.OBJNY.EQ.0) THEN
                  WRITE(OUTLYNE,*) 'NO OBJECT PLANE ARRAY EXISTS'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*) 'NO IMAGE TRACES CAN BE PERFORMED'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(IMGNX.EQ.0.OR.IMGNY.EQ.0) THEN
                  WRITE(OUTLYNE,*) 'NO IMAGE PLANE ARRAY EXISTS'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*) 'NO IMAGE TRACES CAN BE PERFORMED'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C       INDEX CREATION
C       INDEX 1,1 IS LOCATED AT THE -X,-Y LIMIT OF THE
C       CENTRAL HALF OF THE IMAGE X AND Y ARRAYS
              SAVE_KDP(28)=SAVEINPT(28)
              WRITE(INPUT,*) 'U L'
              CALL PROCES
              WRITE(INPUT,*) 'SCY,',(ODELY*DBLE(REAL(OBJNY-1))/2.0D0)
              CALL PROCES
              WRITE(INPUT,*) 'SCX,',(ODELX*DBLE(REAL(OBJNX-1))/2.0D0)
              CALL PROCES
              WRITE(INPUT,*) 'EOS'
              CALL PROCES
              REST_KDP(28)=RESTINPT(28)
C
              WC='FOB'
              SQ=0
              SST=0
              STI=0
              DF1=0
              DF2=0
              DF3=0
              DF4=0
              DF5=1
              SN=1
              S1=1
              S2=1
              S3=1
              S4=1
              S5=0
              W3=0.0D0
              XCORR=IIMAGEX2(1,1)
              YCORR=IIMAGEY2(1,1)
              IIMAGEV(1:IMGNX,1:IMGNY,1:3,1:4)=0.0D0
              SYSSUM=0.0D0

              DO K=1,NUMCOLORS
                  SYSSUM=SYSSUM+SYSTEM1(30+K)
              END DO

              SYSSUM=SYSSUM/DBLE(K)
              irec=1
              DO K=1,NUMCOLORS
                  WRITE(OUTLYNE,*) 'TRACING RAYS I COLOR # ',K
                  CALL SHOWIT(1)
                  DO J=1,OBJNY
                      DO I=1,OBJNX

                          IF(K.EQ.1) WAVLN=SYSTEM1(7)
                          IF(K.EQ.2) WAVLN=SYSTEM1(11)
                          IF(K.EQ.3) WAVLN=SYSTEM1(8)
                          IF(K.EQ.1) KKK=3
                          IF(K.EQ.2) KKK=1
                          IF(K.EQ.3) KKK=2
                          W1=(IOBJECTY(I,J))/DABS(IOBJECTY(1,1))
                          W2=(IOBJECTX(I,J))/DABS(IOBJECTX(1,1))
                          W3=0.0D0
                          W4=WAVLN
                          CALL FASTFFOB(IOBJECTV(I,J,K))

                          X2=REFRY(1,INT(SYSTEM1(20)))
                          Y2=REFRY(2,INT(SYSTEM1(20)))
                          IX=NINT(((X2-XCORR)/IDELX))+1
                          IY=NINT(((Y2-YCORR)/IDELY))+1

                          IF(IX.LE.IMGNX.AND.IY.LE.IMGNY.AND.IX.GE.1.AND.
     1                    IY.GE.1) THEN

                              IIMAGEV(IX,IY,K,1)=IIMAGEV(IX,IY,K,1)
     1                        +(REFRY(25,INT(SYSTEM1(20)))
     1                        *(SYSTEM1(30+KKK)/SYSSUM))
                          END IF

                      END DO
                  END DO
              END DO

              IIMAGEX(1:IMGNX,1:IMGNY)=IIMAGEX2(1:IMGNX,1:IMGNY)
              IIMAGEY(1:IMGNX,1:IMGNY)=IIMAGEY2(1:IMGNX,1:IMGNY)
              PEAKVAL=-1.0D30
              PITVAL=1.0D30
              DO J=1,IMGNY
                  DO I=1,IMGNX
                      DO M=1,NUMCOLORS
                          IF(IIMAGEV(I,J,M,1).LT.PITVAL) PITVAL=IIMAGEV(I,J,M,1)
                      END DO
                  END DO
              END DO
C
              IIMAGEV(1:IMGNX,1:IMGNY,1:NUMCOLORS,1)
     1        =IIMAGEV(1:IMGNX,1:IMGNY,1:NUMCOLORS,1)
     1        -PITVAL
C
              DO I=1,IMGNX
                  DO J=1,IMGNY
                      DO M=1,NUMCOLORS
                          IF(IIMAGEV(I,J,M,1).GT.PEAKVAL) PEAKVAL=IIMAGEV(I,J,M,1)
                      END DO
                  END DO
              END DO

              IF(PEAKVAL.EQ.0.0D0) PEAKVAL=1.0D0

              DO M=1,NUMCOLORS
                  IIMAGEV(1:IMGNX,1:IMGNY,M,1)=
     1            ((IIMAGEV(1:IMGNX,1:IMGNY,M,1)/PEAKVAL)*255.0D0)
              END DO

              WRITE(OUTLYNE,*) 'IMTRACE1 IMAGE TRACING DONE'
              CALL SHOWIT(1)
          END IF
C
C       IMTRACE2
          IF(WC.EQ.'IMTRACE2') THEN
              IF(SQ.NE.0.OR.SST.NE.0.OR.SN.EQ.1) THEN
                  WRITE(OUTLYNE,*)
     1            WC,' TAKES NO ADDITIONAL INPUT'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(SYSTEM1(30).GT.2.0D0) THEN
                  WRITE(OUTLYNE,*)
     1            WC,' REQUIRES "FOCAL" OR "UFOCAL" MODE'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(OBJNX.EQ.0.OR.OBJNY.EQ.0) THEN
                  WRITE(OUTLYNE,*) 'NO OBJECT PLANE ARRAY EXISTS'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*) 'NO IMAGE TRACES CAN BE PERFORMED'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(IMGNX.EQ.0.OR.IMGNY.EQ.0) THEN
                  WRITE(OUTLYNE,*) 'NO IMAGE PLANE ARRAY EXISTS'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*) 'NO IMAGE TRACES CAN BE PERFORMED'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C
C       CREATE A PSF AT EACH OR THE FIRST THREE WAVELENGTH FROM THE ON-AXIS
C       OBJECT POSITION
              SAVE_KDP(28)=SAVEINPT(28)
              WRITE(INPUT,*) 'U L'
              CALL PROCES
              WRITE(INPUT,*) 'SCY,',(ODELY*DBLE(REAL(OBJNY-1))/2.0D0)
              CALL PROCES
              WRITE(INPUT,*) 'SCX,',(ODELX*DBLE(REAL(OBJNX-1))/2.0D0)
              CALL PROCES
              WRITE(INPUT,*) 'EOS'
              CALL PROCES
              REST_KDP(28)=RESTINPT(28)
              IIMAGEV(1:IMGNX,1:IMGNY,1:3,1:4)=0.0D0
C
              SYSSUM=0.0D0
              DO K=1,NUMCOLORS
                  SYSSUM=SYSSUM+SYSTEM1(30+K)
              END DO
              SYSSUM=SYSSUM/DBLE(K)
              DO K=1,NUMCOLORS
                  WAVLN=K
                  SAVE_KDP(26)=SAVEINPT(26)
                  INPUT='OUT NULL'
                  CALL PROCES
                  INPUT='RSPH CHIEF'
                  CALL PROCES
                  INPUT='FOB'
                  CALL PROCES
                  INPUT='PSFPLOT NO'
                  CALL PROCES
                  INPUT='PSFWRITE NO'
                  CALL PROCES
                  IF(NUMCOLORS.NE.1) THEN
                      WRITE(INPUT,*) 'PSF,,',WAVLN
                  ELSE
                      WRITE(INPUT,*) 'PSF'
                  END IF
                  CALL PROCES
                  INPUT='PSFPLOT YES'
                  CALL PROCES
                  INPUT='PSFWRITE YES'
                  CALL PROCES
                  INPUT='OUT TP'
                  CALL PROCES
                  REST_KDP(26)=RESTINPT(26)
                  WRITE(OUTLYNE,*) 'PSF CREATED AT WAVELENGTH # ',K,' AND ON-AXIS'
                  CALL SHOWIT(1)
C       NEGATIVE CORNER OF THE PSF IS:
                  PSFXCORNER=-((DBLE(PGR)-1.0D0)/2.0D0)*GRI
                  PSFYCORNER=-((DBLE(PGR)-1.0D0)/2.0D0)*GRI
C       INDEX CREATION
C       INDEX 1,1 IS LOCATED AT THE -X,-Y LIMIT OF THE
C       CENTRAL HALF OF THE IMAGE X AND Y ARRAYS
C
                  WC='FOB'
                  SQ=0
                  SST=0
                  STI=0
                  DF1=0
                  DF2=0
                  DF3=0
                  DF4=0
                  DF5=1
                  SN=1
                  S1=1
                  S2=1
                  S3=1
                  S4=1
                  S5=0
                  W3=0.0D0
C       XCORR AND YCORR ARE THE LOCATIONS OF THE CENTER OF THE IMAGE PLANE
C       LEFT HAND BOTTOM CORNER PIXEL
                  XCORR=IIMAGEX2(1,1)
                  YCORR=IIMAGEY2(1,1)

                  WRITE(OUTLYNE,*) 'TRACING RAYS AT COLOR # ',K
                  CALL SHOWIT(1)
                  DO J=1,OBJNY
                      DO I=1,OBJNX
                          IF(K.EQ.1) WAVLN=SYSTEM1(7)
                          IF(K.EQ.2) WAVLN=SYSTEM1(11)
                          IF(K.EQ.3) WAVLN=SYSTEM1(8)
                          IF(K.EQ.1) KKK=3
                          IF(K.EQ.2) KKK=1
                          IF(K.EQ.3) KKK=2

C       W1 AND W2 ARE THE CORRECTLY SIGNED FOBS
                          W1=(IOBJECTY(I,J))/DABS(IOBJECTY(1,1))
                          W2=(IOBJECTX(I,J))/DABS(IOBJECTX(1,1))
                          W4=WAVLN
                          CALL FASTFFOB(IOBJECTV(I,J,K))
                          X2=REFRY(1,INT(SYSTEM1(20)))
                          Y2=REFRY(2,INT(SYSTEM1(20)))
C       APPLI THE STORED PSF
                          DO L=1,PGR
                              DO M=1,PGR
C       REFERENCED TO THE CENTER OF THE IMAGE PLANE (PSF CENTERED ON CHIEF RAY)
                                  PSFX=X2+(PSFXCORNER+((L-1)*GRI))
                                  PSFY=Y2+(PSFYCORNER+((M-1)*GRI))
                                  IX=NINT(((PSFX-XCORR)/IDELX))+1
                                  IY=NINT(((PSFY-YCORR)/IDELY))+1
                                  IF(IX.LE.IMGNX.AND.IY.LE.IMGNY.AND.IX.GE.1.AND.
     1                            IY.GE.1) THEN
                                      IIMAGEV(IX,IY,K,1)=IIMAGEV(IX,IY,K,1)+
     1                                ((FIMG(L,M)*(SYSTEM1(30+KKK)/SYSSUM))
     1                                *REFRY(25,INT(SYSTEM1(20))))
                                  END IF
                              END DO
                          END DO
                      END DO
                  END DO
              END DO
              IIMAGEX(1:IMGNX,1:IMGNY)=IIMAGEX2(1:IMGNX,1:IMGNY)
              IIMAGEY(1:IMGNX,1:IMGNY)=IIMAGEY2(1:IMGNX,1:IMGNY)
              PEAKVAL=-1.0D30
              PITVAL=1.0D30
              DO J=1,IMGNY
                  DO I=1,IMGNX
                      DO M=1,NUMCOLORS
                          IF(IIMAGEV(I,J,M,1).LT.PITVAL) PITVAL=IIMAGEV(I,J,M,1)
                      END DO
                  END DO
              END DO
C
              IIMAGEV(1:IMGNX,1:IMGNY,1:NUMCOLORS,1)
     1        =IIMAGEV(1:IMGNX,1:IMGNY,1:NUMCOLORS,1)
     1        -PITVAL
C
              DO J=1,IMGNY
                  DO I=1,IMGNX
                      DO M=1,NUMCOLORS
                          IF(IIMAGEV(I,J,M,1).GT.PEAKVAL) PEAKVAL=IIMAGEV(I,J,M,1)
                      END DO
                  END DO
              END DO
              IF(PEAKVAL.EQ.0.0D0) PEAKVAL=1.0D0
              DO M=1,NUMCOLORS
                  IIMAGEV(1:IMGNX,1:IMGNY,M,1)=
     1            ((IIMAGEV(1:IMGNX,1:IMGNY,M,1)/PEAKVAL)*255.0D0)
              END DO
              WRITE(OUTLYNE,*) 'IMTRACE2 TRACING DONE'
              CALL SHOWIT(1)
          END IF
C
C       IMTRACE3
          IF(WC.EQ.'IMTRACE3') THEN
              IF(SQ.NE.0.OR.SST.NE.0.OR.SN.EQ.1) THEN
                  WRITE(OUTLYNE,*)
     1            WC,' TAKES NO ADDITIONAL INPUT'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(SYSTEM1(30).GT.2.0D0) THEN
                  WRITE(OUTLYNE,*)
     1            WC,' REQUIRES "FOCAL" OR "UFOCAL" MODE'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(OBJNX.EQ.0.OR.OBJNY.EQ.0) THEN
                  WRITE(OUTLYNE,*) 'NO OBJECT PLANE ARRAY EXISTS'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*) 'NO IMAGE TRACES CAN BE PERFORMED'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(IMGNX.EQ.0.OR.IMGNY.EQ.0) THEN
                  WRITE(OUTLYNE,*) 'NO IMAGE PLANE ARRAY EXISTS'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*) 'NO IMAGE TRACES CAN BE PERFORMED'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C
C       CREATE A PSF AT EACH OR THE FIRST THREE WAVELENGTH FROM THE ON-AXIS
C       OBJECT POSITION
              SAVE_KDP(28)=SAVEINPT(28)
              WRITE(INPUT,*) 'U L'
              CALL PROCES
              WRITE(INPUT,*) 'SCY,',(ODELY*DBLE(REAL(OBJNY-1))/2.0D0)
              CALL PROCES
              WRITE(INPUT,*) 'SCX,',(ODELX*DBLE(REAL(OBJNX-1))/2.0D0)
              CALL PROCES
              WRITE(INPUT,*) 'EOS'
              CALL PROCES
              REST_KDP(28)=RESTINPT(28)
              IIMAGEV(1:IMGNX,1:IMGNY,1:3,1:4)=0.0D0
C
              SYSSUM=0.0D0
              DO K=1,NUMCOLORS
                  SYSSUM=SYSSUM+SYSTEM1(30+K)
              END DO
              SYSSUM=SYSSUM/DBLE(K)
              DO K=1,NUMCOLORS
                  WAVLN=K
                  XCORR=IIMAGEX2(1,1)
                  YCORR=IIMAGEY2(1,1)

                  WRITE(OUTLYNE,*) 'TRACING RAYS AT COLOR # ',K
                  CALL SHOWIT(1)
                  SKIPY=0
                  SKIPX=0
                  DO J=1,OBJNY
                      DO I=1,OBJNX
                          IF(K.EQ.1) WAVLN=SYSTEM1(7)
                          IF(K.EQ.2) WAVLN=SYSTEM1(11)
                          IF(K.EQ.3) WAVLN=SYSTEM1(8)
                          IF(K.EQ.1) KKK=3
                          IF(K.EQ.2) KKK=1
                          IF(K.EQ.3) KKK=2
                          WC='FOB'
                          SQ=0
                          SST=0
                          STI=0
                          DF1=0
                          DF2=0
                          DF3=0
                          DF4=0
                          DF5=1
                          SN=1
                          S1=1
                          S2=1
                          S3=1
                          S4=1
                          S5=0
                          W3=0.0D0
                          W1=(IOBJECTY(I,J))/DABS(IOBJECTY(1,1))
                          W2=(IOBJECTX(I,J))/DABS(IOBJECTX(1,1))
                          W4=WAVLN
                          IF(SKIPX.EQ.0.AND.SKIPY.EQ.0) THEN
C       DO A NEW PSF
                              CALL SLOWFFOB(IOBJECTV(I,J,K))
                              X2=REFRY(1,INT(SYSTEM1(20)))
                              Y2=REFRY(2,INT(SYSTEM1(20)))
                              SAVE_KDP(26)=SAVEINPT(26)
                              INPUT='OUT NULL'
                              CALL PROCES
                              INPUT='RSPH CHIEF'
                              CALL PROCES
                              INPUT='PSFPLOT NO'
                              CALL PROCES
                              INPUT='PSFWRITE NO'
                              CALL PROCES
                              IF(NUMCOLORS.NE.1) THEN
                                  WRITE(INPUT,*) 'PSF,,',WAVLN
                              ELSE
                                  WRITE(INPUT,*) 'PSF'
                              END IF
                              CALL PROCES
                              INPUT='PSFPLOT YES'
                              CALL PROCES
                              INPUT='PSFWRITE YES'
                              CALL PROCES
                              INPUT='OUT TP'
                              CALL PROCES
                              REST_KDP(26)=RESTINPT(26)
                          ELSE
C       USE EXISTING PSF
                              CALL FASTFFOB(IOBJECTV(I,J,K))
                              X2=REFRY(1,INT(SYSTEM1(20)))
                              Y2=REFRY(2,INT(SYSTEM1(20)))
                          END IF
C       NEGATIVE CORNER OF THE PSF IS:
                          PSFXCORNER=-((DBLE(PGR)-1.0D0)/2.0D0)*GRI
                          PSFYCORNER=-((DBLE(PGR)-1.0D0)/2.0D0)*GRI
C       APPLI THE STORED PSF
                          DO L=1,PGR
                              DO M=1,PGR
C       REFERENCED TO THE CENTER OF THE IMAGE PLANE (PSF CENTERED ON CHIEF RAY)
                                  PSFX=X2+(PSFXCORNER+((L-1)*GRI))
                                  PSFY=Y2+(PSFYCORNER+((M-1)*GRI))
                                  IX=NINT(((PSFX-XCORR)/IDELX))+1
                                  IY=NINT(((PSFY-YCORR)/IDELY))+1
                                  IF(IX.LE.IMGNX.AND.IY.LE.IMGNY.AND.IX.GE.1.AND.
     1                            IY.GE.1) THEN
                                      IIMAGEV(IX,IY,K,1)=IIMAGEV(IX,IY,K,1)+
     1                                ((FIMG(L,M)*(SYSTEM1(30+KKK)/SYSSUM))
     1                                *REFRY(25,INT(SYSTEM1(20))))
                                  END IF
                              END DO
                          END DO
                          SKIPX=SKIPX+1
                          IF(SKIPX.EQ.SKIPIT) SKIPX=0
                      END DO
                      SKIPY=SKIPY+1
                      IF(SKIPY.EQ.SKIPIT) SKIPY=0
                  END DO
              END DO

              IIMAGEX(1:IMGNX,1:IMGNY)=IIMAGEX2(1:IMGNX,1:IMGNY)
              IIMAGEY(1:IMGNX,1:IMGNY)=IIMAGEY2(1:IMGNX,1:IMGNY)
              PEAKVAL=-1.0D30
              PITVAL=1.0D30
              DO J=1,IMGNY
                  DO I=1,IMGNX
                      DO M=1,NUMCOLORS
                          IF(IIMAGEV(I,J,M,1).LT.PITVAL) PITVAL=IIMAGEV(I,J,M,1)
                      END DO
                  END DO
              END DO
C
              IIMAGEV(1:IMGNX,1:IMGNY,1:NUMCOLORS,1)
     1        =IIMAGEV(1:IMGNX,1:IMGNY,1:NUMCOLORS,1)
     1        -PITVAL
C
              DO J=1,IMGNY
                  DO I=1,IMGNX
                      DO M=1,NUMCOLORS
                          IF(IIMAGEV(I,J,M,1).GT.PEAKVAL) PEAKVAL=IIMAGEV(I,J,M,1)
                      END DO
                  END DO
              END DO
              IF(PEAKVAL.EQ.0.0D0) PEAKVAL=1.0D0
              DO M=1,NUMCOLORS
                  IIMAGEV(1:IMGNX,1:IMGNY,M,1)=
     1            ((IIMAGEV(1:IMGNX,1:IMGNY,M,1)/PEAKVAL)*255.0D0)
              END DO
              WRITE(OUTLYNE,*) 'IMTRACE3 TRACING DONE'
              CALL SHOWIT(1)
          END IF
C
C
C       COLOR
C       G=1 CONTROL WAVELENGTH, SYSTEM1(11)
C       B=2 FIRST COLOR, SWCONDARY CHROMATIC PAIR, SYSTEM1(9)
C       R=3 SECOND COLOR, SECONDARY CHROMATIC PAIR, SYSTEM1(10)
C
          IF(WC.EQ.'COLOR') THEN
              IF(STI.EQ.1) THEN
                  IF(NUMCOLORS.EQ.1) WRITE(OUTLYNE,*)
     1            '8 BIT MONOCHROME IMAGERY IN EFFECT'
                  IF(NUMCOLORS.EQ.3) WRITE(OUTLYNE,*)
     1            '24 BIT RGB INAGERY IN EFFECT'
                  CALL SHOWIT(1)
                  RETURN
              END IF
              IF(SST.EQ.1.OR.SN.EQ.1) THEN
                  WRITE(OUTLYNE,*)
     1            '"COLOR" TAKES NO STRING OR NUMERIC INPUT'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(SQ.EQ.0) THEN
                  WRITE(OUTLYNE,*)
     1            '"COLOR" REQUIRES EXPLICIT QUALIFIER INPUT'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(WQ.NE.'MONO'.AND.WQ.NE.'RGB') THEN
                  WRITE(OUTLYNE,*)
     1            '"COLOR" TAKES EITHER "MONO" OR "RGB" FOR QUALIFIER INPUT'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(WQ.EQ.'MONO') NUMCOLORS=1
              IF(WQ.EQ.'RGB') NUMCOLORS=3
              IF(WQ.EQ.'MONO'.OR.WQ.EQ.'RGB') THEN
                  OBJNX=0
                  OBJNY=0
                  IMGNX=0
                  IMGNY=0
              END IF
          END IF
C
C
C       IOBJECT
C
          IF(WC.EQ.'IOBJECT') THEN
              IF(STI.EQ.1) THEN
                  WRITE(OUTLYNE,*)
     1            'OBJECT PLANE ARRAY DIMENSION NX = ',OBJNX
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)
     1            'OBJECT PLANE ARRAY DIMENSION NY = ',OBJNY
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)
     1            '    OBJECT PLANE PIXEL X-LENGTH = ',ODELX
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)
     1            '    OBJECT PLANE PIXEL Y-LENGTH = ',ODELY
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)
     1            '    OBJECT PLANE ARRAY X-EXTENT = ',REAL(OBJNX-1)*ODELX
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)
     1            '    OBJECT PLANE ARRAY Y-EXTENT = ',REAL(OBJNY-1)*ODELY
                  CALL SHOWIT(1)
                  RETURN
              END IF
              IF(S1.EQ.0) THEN
                  WRITE(OUTLYNE,*)
     1            WC,' REQUIRES EXPLICIT NUMERIC WORD #1 INPUT'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(S2.EQ.0) THEN
                  WRITE(OUTLYNE,*)
     1            WC,' REQUIRES EXPLICIT NUMERIC WORD #2 INPUT'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(S3.EQ.0) THEN
                  WRITE(OUTLYNE,*)
     1            WC,' REQUIRES EXPLICIT NUMERIC WORD #3 INPUT'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(S4.EQ.0) THEN
                  WRITE(OUTLYNE,*)
     1            WC,' REQUIRES EXPLICIT NUMERIC WORD #4 INPUT'
C       COMPUTE ARRAY DIMENTIONS
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              DNX=(W1/W3)
              NX=INT(DNX)+1
              IF(DNX-DBLE(INT(DNX)).NE.0.0D0) THEN
                  W1=DBLE(INT(DNX)-1)*W3
                  WRITE(OUTLYNE,*)
     1            '"X" HAS BEEN ADJUSTED TO BE AN INTEGER MULTIPLE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)
     1            'OF "DELTA-X", NEW X = ',W1
                  CALL SHOWIT(1)
                  DNX=W1/W3
                  NX=NINT(W1/W3)+1
              END IF
              DNY=W2/W4
              NY=INT(DNY)+1
              IF(DNY-DBLE(INT(DNY)).NE.0.0D0) THEN
                  W2=DBLE(INT(DNY)-1)*W4
                  WRITE(OUTLYNE,*)
     1            '"Y" HAS BEEN ADJUSTED TO BE AN INTEGER MULTIPLE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)
     1            'OF "DELTA-Y", NEW Y = ',W2
                  CALL SHOWIT(1)
                  DNY=W2/W4
                  NY=NINT(W2/W4)+1
              END IF
              WRITE(OUTLYNE,*)
     1        'OBJECT PLANE ARRAY DIMENSION NX = ',OBJNX
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)
     1        'OBJECT PLANE ARRAY DIMENSION NY = ',OBJNY
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)
     1        '    OBJECT PLANE PIXEL X-LENGTH = ',W3
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)
     1        '    OBJECT PLANE PIXEL Y-LENGTH = ',W4
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)
     1        '    OBJECT PLANE ARRAY X-EXTENT = ',W1
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)
     1        '    OBJECT PLANE ARRAY Y-EXTENT = ',W2
              CALL SHOWIT(1)
              DEALLOCATE (IOBJECTX,IOBJECTY,
     1        IOBJECTV,STAT=ALLOERR)
              ALLOCATE (IOBJECTX(NX,NY),IOBJECTY(NX,NY),
     1        IOBJECTV(NX,NY,3),STAT=ALLOERR)
              OBJNX=NX
              OBJNY=NY
              ODELX=W3
              ODELY=W4
              IOBJECTX(1:NX,1:NY)=0.0D0
              IOBJECTY(1:NX,1:NY)=0.0D0
              IOBJECTV(1:NX,1:NY,1:3)=0.0D0
C
C       LOAD ARRAYS
              OXODD=.FALSE.
              IF((DBLE(NX)/2.0D0)-DBLE(NX/2).NE.0.0D0) OXODD=.TRUE.
              OYODD=.FALSE.
              IF((DBLE(NY)/2.0D0)-DBLE(NY/2).NE.0.0D0) OYODD=.TRUE.
              IWW3=ODELX
              IWW4=ODELY
C
              IF(OXODD) THEN
                  DO I=0,NX-1
                      DO J=1,NY
                          IOBJECTX(I+1,J)=-(IWW3*DBLE((NX-1)/2))+(DBLE(I)*IWW3)
                      END DO
                  END DO
              END IF
              IF(.NOT.OXODD) THEN
                  DO I=1,NX
                      DO J=1,NY
                          IOBJECTX(I,J)=-(IWW3*(DBLE(NX+1)/2.0D0))+(DBLE(I)*IWW3)
                      END DO
                  END DO
              END IF
              IF(OYODD) THEN
                  DO I=1,NX
                      DO J=0,NY-1
                          IOBJECTY(I,J+1)=-(IWW4*DBLE((NY-1)/2))+(DBLE(J)*IWW4)
                      END DO
                  END DO
              END IF
              IF(.NOT.OYODD) THEN
                  DO I=1,NX
                      DO J=1,NY
                          IOBJECTY(I,J)=-(IWW4*(DBLE(NY+1)/2.0D0))+(DBLE(J)*IWW4)
                      END DO
                  END DO
              END IF
              RETURN
          END IF
C
C       IOBJECTN
C
          IF(WC.EQ.'IOBJECTN') THEN
              IF(STI.EQ.1) THEN
                  WRITE(OUTLYNE,*)
     1            'OBJECT PLANE ARRAY DIMENSION NX = ',OBJNX
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)
     1            'OBJECT PLANE ARRAY DIMENSION NY = ',OBJNY
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)
     1            '    OBJECT PLANE PIXEL X-LENGTH = ',ODELX
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)
     1            '    OBJECT PLANE PIXEL Y-LENGTH = ',ODELY
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)
     1            '    OBJECT PLANE ARRAY X-EXTENT = ',REAL(OBJNX-1)*ODELX
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)
     1            '    OBJECT PLANE ARRAY Y-EXTENT = ',REAL(OBJNY-1)*ODELY
                  CALL SHOWIT(1)
                  RETURN
              END IF
              IF(S1.EQ.0) THEN
                  WRITE(OUTLYNE,*)
     1            WC,' REQUIRES EXPLICIT NUMERIC WORD #1 INPUT'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(S2.EQ.0) THEN
                  WRITE(OUTLYNE,*)
     1            WC,' REQUIRES EXPLICIT NUMERIC WORD #2 INPUT'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(S3.EQ.0) THEN
                  WRITE(OUTLYNE,*)
     1            WC,' REQUIRES EXPLICIT NUMERIC WORD #3 INPUT'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(S4.EQ.0) THEN
                  WRITE(OUTLYNE,*)
     1            WC,' REQUIRES EXPLICIT NUMERIC WORD #4 INPUT'
C       COMPUTE ARRAY DIMENTIONS
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C       COMPUTE ARRAY DIMENTIONS
              NX=INT(W3)
              NY=INT(W4)
              DNX=W3
              DNY=W4
              ODELX=(W1+W3)/DNX
              ODELY=(W2+W4)/DNY
              WRITE(OUTLYNE,*)
     1        'OBJECT PLANE ARRAY DIMENSION NX = ',NX
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)
     1        'OBJECT PLANE ARRAY DIMENSION NY = ',NY
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)
     1        '    OBJECT PLANE PIXEL X-LENGTH = ',ODELX
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)
     1        '    OBJECT PLANE PIXEL Y-LENGTH = ',ODELY
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)
     1        '    OBJECT PLANE ARRAY X-EXTENT = ',W1
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)
     1        '    OBJECT PLANE ARRAY Y-EXTENT = ',W2
              CALL SHOWIT(1)
              DEALLOCATE (IOBJECTX,IOBJECTY,
     1        IOBJECTV,STAT=ALLOERR)
              ALLOCATE (IOBJECTX(NX,NY),IOBJECTY(NX,NY),
     1        IOBJECTV(NX,NY,3),STAT=ALLOERR)
              OBJNX=NX
              OBJNY=NY
              IOBJECTX(1:NX,1:NY)=0.0D0
              IOBJECTY(1:NX,1:NY)=0.0D0
              IOBJECTV(1:NX,1:NY,1:3)=0.0D0
              IWW3=ODELX
              IWW4=ODELY
C
C       LOAD ARRAYS
              OXODD=.FALSE.
              IF((DBLE(NX)/2.0D0)-DBLE(NX/2).NE.0.0D0) OXODD=.TRUE.
              OYODD=.FALSE.
              IF((DBLE(NY)/2.0D0)-DBLE(NY/2).NE.0.0D0) OYODD=.TRUE.
C
              IF(OXODD) THEN
                  DO I=0,NX-1
                      DO J=1,NY
                          IOBJECTX(I+1,J)=-(IWW3*DBLE((NX-1)/2))+(DBLE(I)*IWW3)
                      END DO
                  END DO
              END IF
              IF(.NOT.OXODD) THEN
                  DO I=1,NX
                      DO J=1,NY
                          IOBJECTX(I,J)=-(IWW3*(DBLE(NX+1)/2.0D0))+(DBLE(I)*IWW3)
                      END DO
                  END DO
              END IF
              IF(OYODD) THEN
                  DO I=1,NX
                      DO J=0,NY-1
                          IOBJECTY(I,J+1)=-(IWW4*DBLE((NY-1)/2))+(DBLE(J)*IWW4)
                      END DO
                  END DO
              END IF
              IF(.NOT.OYODD) THEN
                  DO I=1,NX
                      DO J=1,NY
                          IOBJECTY(I,J)=-(IWW4*(DBLE(NY+1)/2.0D0))+(DBLE(J)*IWW4)
                      END DO
                  END DO
              END IF
              RETURN
          END IF

C
C       IIMAGE
C
          IF(WC.EQ.'IIMAGE') THEN
              IF(STI.EQ.1) THEN
                  WRITE(OUTLYNE,*)
     1            'IMAGE PLANE ARRAY DIMENSION NX = ',IMGNX
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)
     1            'IMAGE PLANE ARRAY DIMENSION NY = ',IMGNX
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)
     1            '    IMAGE PLANE PIXEL X-LENGTH = ',IDELX
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)
     1            '    IMAGE PLANE PIXEL Y-LENGTH = ',IDELY
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)
     1            '    IMAGE PLANE ARRAY X=EXTENT = ',REAL(IMGNX-1)*IDELX
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)
     1            '    IMAGE PLANE ARRAY Y-EXTENT = ',REAL(IMGNY-1)*IDELY
                  CALL SHOWIT(1)
                  RETURN
              END IF
              IF(SQ.NE.0.OR.SST.NE.0) THEN
                  WRITE(OUTLYNE,*)
     1            WC,'TAKES NO QUALIFIER OR STRING INPUT'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(S1.EQ.0) THEN
                  WRITE(OUTLYNE,*)
     1            WC,' REQUIRES EXPLICIT NUMERIC WORD #1 INPUT'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(S2.EQ.0) THEN
                  WRITE(OUTLYNE,*)
     1            WC,' REQUIRES EXPLICIT NUMERIC WORD #2 INPUT'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(S3.EQ.0) THEN
                  WRITE(OUTLYNE,*)
     1            WC,' REQUIRES EXPLICIT NUMERIC WORD #3 INPUT'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(S4.EQ.0) THEN
                  WRITE(OUTLYNE,*)
     1            WC,' REQUIRES EXPLICIT NUMERIC WORD #4 INPUT'
C       COMPUTE ARRAY DIMENTIONS
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C       COMPUTE ARRAY DIMENTIONS
              DNX=W1/W3
              NX=INT(DNX)+1
              IF(DNX-DBLE(INT(DNX)).NE.0.0D0) THEN
                  W1=DBLE(INT(DNX)-1)*W3
                  WRITE(OUTLYNE,*)
     1            '"X" HAS BEEN ADJUSTED TO BE AN INTEGER MULTIPLE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)
     1            'OF "DELTA-X", NEW X = ',W1
                  CALL SHOWIT(1)
                  DNY=W1/W3
                  NY=NINT(W1/W3)+1
              END IF
              DNY=W2/W4
              NY=INT(DNY)+1
              IF(DNY-DBLE(INT(DNY)).NE.0.0D0) THEN
                  W2=DBLE(INT(DNY)-1)*W4
                  WRITE(OUTLYNE,*)
     1            '"Y" HAS BEEN ADJUSTED TO BE AN INTEGER MULTIPLE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)
     1            'OF "DELTA-Y", NEW Y = ',W2
                  DNY=W2/W4
                  NY=NINT(W2/W4)+1
              END IF
              WRITE(OUTLYNE,*)
     1        'IMAGE PLANE ARRAY DIMENSION NX = ',NX
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)
     1        'IMAGE PLANE ARRAY DIMENSION NY = ',NY
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)
     1        '    IMAGE PLANE PIXEL X-LENGTH = ',W3
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)
     1        '    IMAGE PLANE PIXEL Y-LENGTH = ',W4
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)
     1        '    IMAGE PLANE ARRAY X-EXTENT = ',W1
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)
     1        '    IMAGE PLANE ARRAY Y-EXTENT = ',W2
              CALL SHOWIT(1)
              DEALLOCATE (IIMAGEX2,IIMAGEY2,IIMAGEX,IIMAGEY,
     1        IIMAGEV,IMGTEMP,STAT=ALLOERR)
              ALLOCATE (IIMAGEX2(NX,NY),
     1        IIMAGEY2(NX,NY),
     1        IIMAGEX((-NX/2):NX+(NX/2),(-NY/2):NY+(NY/2)),
     1        IIMAGEY((-NX/2):NX+(NX/2),(-NY/2):NY+(NY/2)),
     1        IIMAGEV((-NX/2):NX+(NX/2),(-NY/2):(NY+(NY/2)),3,4),
     1        IMGTEMP((-NX/2):NX+(NX/2),(-NY/2):NY+(NY/2),3,4),STAT=ALLOERR)
              IMGNX=NX
              IMGNY=NY
              IDELX=W3
              IDELY=W4
              IIMAGEX2(1:NX,1:NY)=0.0D0
              IIMAGEY2(1:NX,1:NY)=0.0D0
              IIMAGEX((-NX/2):NX+(NX/2),(-NY/2):NY+(NY/2))=0.0D0
              IIMAGEY((-NX/2):NX+(NX/2),(-NY/2):NY+(NY/2))=0.0D0
              IIMAGEV((-NX/2):NX+(NX/2),(-NY/2):(NY+(NY/2)),1:3,1:4)=0.0D0
              IMGTEMP((-NX/2):NX+(NX/2),(-NY/2):NY+(NY/2),1:3,1:4)=0.0D0
              IWW3=IDELX
              IWW4=IDELY
C
C
C       LOAD ARRAYS
              IXODD=.FALSE.
              IF((DBLE(NX)/2.0D0)-DBLE(NX/2).NE.0.0D0) IXODD=.TRUE.
              IYODD=.FALSE.
              IF((DBLE(NY)/2.0D0)-DBLE(NY/2).NE.0.0D0) IYODD=.TRUE.
C
              IF(IXODD) THEN
                  DO I=0,NX-1
                      DO J=1,NY
                          IIMAGEX2(I+1,J)=-(IWW3*DBLE((NX-1)/2))+(DBLE(I)*IWW3)
                      END DO
                  END DO
              END IF
              IF(.NOT.IXODD) THEN
                  DO I=1,NX
                      DO J=1,NY
                          IIMAGEX2(I,J)=-(IWW3*(DBLE(NX+1)/2.0D0))+(DBLE(I)*IWW3)
                      END DO
                  END DO
              END IF
              IF(IYODD) THEN
                  DO I=1,NX
                      DO J=0,NY-1
                          IIMAGEY2(I,J+1)=-(IWW4*DBLE((NY-1)/2))+(DBLE(J)*IWW4)
                      END DO
                  END DO
              END IF
              IF(.NOT.IYODD) THEN
                  DO I=1,NX
                      DO J=1,NY
                          IIMAGEY2(I,J)=-(IWW4*(DBLE(NY+1)/2.0D0))+(DBLE(J)*IWW4)
                      END DO
                  END DO
              END IF
              RETURN
          END IF
C
C       IIMAGEN
C
          IF(WC.EQ.'IIMAGEN') THEN
              IF(STI.EQ.1) THEN
                  WRITE(OUTLYNE,*)
     1            'IMAGE PLANE ARRAY DIMENSION NX = ',IMGNX
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)
     1            'IMAGE PLANE ARRAY DIMENSION NY = ',IMGNY
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)
     1            '    IMAGE PLANE PIXEL X-LENGTH = ',IDELX
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)
     1            '    IMAGE PLANE PIXEL Y-LENGTH = ',IDELY
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)
     1            '    IMAGE PLANE ARRAY X=EXTENT = ',REAL(IMGNX-1)*IDELX
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)
     1            '    IMAGE PLANE ARRAY Y-EXTENT = ',REAL(IMGNY-1)*IDELY
                  CALL SHOWIT(1)
                  RETURN
              END IF
              IF(SQ.NE.0.OR.SST.NE.0) THEN
                  WRITE(OUTLYNE,*)
     1            WC,'TAKES NO QUALIFIER OR STRING INPUT'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(S1.EQ.0) THEN
                  WRITE(OUTLYNE,*)
     1            WC,' REQUIRES EXPLICIT NUMERIC WORD #1 INPUT'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(S2.EQ.0) THEN
                  WRITE(OUTLYNE,*)
     1            WC,' REQUIRES EXPLICIT NUMERIC WORD #2 INPUT'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(S3.EQ.0) THEN
                  WRITE(OUTLYNE,*)
     1            WC,' REQUIRES EXPLICIT NUMERIC WORD #3 INPUT'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(S4.EQ.0) THEN
                  WRITE(OUTLYNE,*)
     1            WC,' REQUIRES EXPLICIT NUMERIC WORD #4 INPUT'
C       COMPUTE ARRAY DIMENTIONS
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C       COMPUTE ARRAY DIMENTIONS
              NX=INT(W3)
              NY=INT(W4)
              DNX=W3-1.0D0
              DNY=W4-1.0D0
              IDELX=W1/DNX
              IDELY=W2/DNY
              WRITE(OUTLYNE,*)
     1        'IMAGE PLANE ARRAY DIMENSION NX = ',NX
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)
     1        'IMAGE PLANE ARRAY DIMENSION NY = ',NY
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)
     1        '    IMAGE PLANE PIXEL X-LENGTH = ',IDELX
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)
     1        '    IMAGE PLANE PIXEL Y-LENGTH = ',IDELY
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)
     1        '    IMAGE PLANE ARRAY X-EXTENT = ',W1
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)
     1        '    IMAGE PLANE ARRAY Y-EXTENT = ',W2
              CALL SHOWIT(1)
              DEALLOCATE (IIMAGEX2,IIMAGEY2,IIMAGEX,IIMAGEY,
     1        IIMAGEV,IMGTEMP,STAT=ALLOERR)
              ALLOCATE (IIMAGEX2(NX,NY),
     1        IIMAGEY2(NX,NY),
     1        IIMAGEX((-NX/2):NX+(NX/2),(-NY/2):NY+(NY/2)),
     1        IIMAGEY((-NX/2):NX+(NX/2),(-NY/2):NY+(NY/2)),
     1        IIMAGEV((-NX/2):NX+(NX/2),(-NY/2):(NY+(NY/2)),3,4),
     1        IMGTEMP((-NX/2):NX+(NX/2),(-NY/2):(NY+(NY/2)),3,4),STAT=ALLOERR)
              IMGNX=NX
              IMGNY=NY
              IIMAGEX2(1:NX,1:NY)=0.0D0
              IIMAGEY2(1:NX,1:NY)=0.0D0
              IIMAGEX((-NX/2):NX+(NX/2),(-NY/2):NY+(NY/2))=0.0D0
              IIMAGEY((-NX/2):NX+(NX/2),(-NY/2):NY+(NY/2))=0.0D0
              IIMAGEV((-NX/2):NX+(NX/2),(-NY/2):(NY+(NY/2)),1:3,1:4)=0.0D0
              IMGTEMP((-NX/2):NX+(NX/2),(-NY/2):NY+(NY/2),1:3,1:4)=0.0D0
              IWW3=IDELX
              IWW4=IDELY
C
C       LOAD ARRAYS
              IXODD=.FALSE.
              IF((DBLE(NX)/2.0D0)-DBLE(NX/2).NE.0.0D0) IXODD=.TRUE.
              IYODD=.FALSE.
              IF((DBLE(NY)/2.0D0)-DBLE(NY/2).NE.0.0D0) IYODD=.TRUE.
C
              IF(IXODD) THEN
                  DO I=0,NX-1
                      DO J=1,NY
                          IIMAGEX2(I+1,J)=-(IWW3*DBLE((NX-1)/2))+(DBLE(I)*IWW3)
                      END DO
                  END DO
              END IF
              IF(.NOT.IXODD) THEN
                  DO I=1,NX
                      DO J=1,NY
                          IIMAGEX2(I,J)=-(IWW3*(DBLE(NX+1)/2.0D0))+(DBLE(I)*IWW3)
                      END DO
                  END DO
              END IF
              IF(IYODD) THEN
                  DO I=1,NX
                      DO J=0,NY-1
                          IIMAGEY2(I,J+1)=-(IWW4*DBLE((NY-1)/2))+(DBLE(J)*IWW4)
                      END DO
                  END DO
              END IF
              IF(.NOT.IYODD) THEN
                  DO I=1,NX
                      DO J=1,NY
                          IIMAGEY2(I,J)=-(IWW4*(DBLE(NY+1)/2.0D0))+(DBLE(J)*IWW4)
                      END DO
                  END DO
              END IF
              RETURN
          END IF
C
C       IIMAGED
C
          IF(WC.EQ.'IIMAGED') THEN
              IF(STI.EQ.1) THEN
                  WRITE(OUTLYNE,*)
     1            'IMAGE PLANE ARRAY DIMENSION NX = ',IMGNX
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)
     1            'IMAGE PLANE ARRAY DIMENSION NY = ',IMGNY
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)
     1            '    IMAGE PLANE PIXEL X-LENGTH = ',IDELX
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)
     1            '    IMAGE PLANE PIXEL Y-LENGTH = ',IDELY
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)
     1            '    IMAGE PLANE ARRAY X=EXTENT = ',REAL(IMGNX-1)*IDELX
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)
     1            '    IMAGE PLANE ARRAY Y-EXTENT = ',REAL(IMGNY-1)*IDELY
                  CALL SHOWIT(1)
                  RETURN
              END IF
              IF(SQ.NE.0.OR.SST.NE.0) THEN
                  WRITE(OUTLYNE,*)
     1            WC,'TAKES NO QUALIFIER OR STRING INPUT'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(S1.EQ.0) THEN
                  WRITE(OUTLYNE,*)
     1            WC,' REQUIRES EXPLICIT NUMERIC WORD #1 INPUT'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(S2.EQ.0) THEN
                  WRITE(OUTLYNE,*)
     1            WC,' REQUIRES EXPLICIT NUMERIC WORD #2 INPUT'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(S3.EQ.0) THEN
                  WRITE(OUTLYNE,*)
     1            WC,' REQUIRES EXPLICIT NUMERIC WORD #3 INPUT'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(S4.EQ.0) THEN
                  WRITE(OUTLYNE,*)
     1            WC,' REQUIRES EXPLICIT NUMERIC WORD #4 INPUT'
C       COMPUTE ARRAY DIMENTIONS
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C       COMPUTE ARRAY DIMENTIONS
              NX=INT(W3)
              NY=INT(W4)
              DNX=W3
              DNY=W4
              IDELX=W1
              IDELY=W2
              WRITE(OUTLYNE,*)
     1        'IMAGE PLANE ARRAY DIMENSION NX = ',NX
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)
     1        'IMAGE PLANE ARRAY DIMENSION NY = ',NY
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)
     1        '    IMAGE PLANE PIXEL X-LENGTH = ',IDELX
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)
     1        '    IMAGE PLANE PIXEL Y-LENGTH = ',IDELY
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)
     1        '    IMAGE PLANE ARRAY X-EXTENT = ',W1*(W3-1.0D0)
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)
     1        '    IMAGE PLANE ARRAY Y-EXTENT = ',W2*(W4-1.0D0)
              CALL SHOWIT(1)
              DEALLOCATE (IIMAGEX2,IIMAGEY2,IIMAGEX,IIMAGEY,
     1        IIMAGEV,IMGTEMP,STAT=ALLOERR)
              ALLOCATE (IIMAGEX2(NX,NY),
     1        IIMAGEY2(NX,NY),
     1        IIMAGEX((-NX/2):NX+(NX/2),(-NY/2):NY+(NY/2)),
     1        IIMAGEY((-NX/2):NX+(NX/2),(-NY/2):NY+(NY/2)),
     1        IIMAGEV((-NX/2):NX+(NX/2),(-NY/2):NY+(NY/2),3,4),
     1        IMGTEMP((-NX/2):NX+(NX/2),(-NY/2):(NY+(NY/2)),3,4),STAT=ALLOERR)
              IMGNX=NX
              IMGNY=NY
              IIMAGEX2(1:NX,1:NY)=0.0D0
              IIMAGEY2(1:NX,1:NY)=0.0D0
              IIMAGEX((-NX/2):NX+(NX/2),(-NY/2):NY+(NY/2))=0.0D0
              IIMAGEY((-NX/2):NX+(NX/2),(-NY/2):NY+(NY/2))=0.0D0
              IIMAGEV((-NX/2):NX+(NX/2),(-NY/2):NY+(NY/2),1:3,1:4)=0.0D0
              IMGTEMP((-NX/2):NX+(NX/2),(-NY/2):(NY+(NY/2)),1:3,1:4)=0.0D0
              IWW3=IDELX
              IWW4=IDELY
C
C       LOAD ARRAYS
              IXODD=.FALSE.
              IF((DBLE(NX)/2.0D0)-DBLE(NX/2).NE.0.0D0) IXODD=.TRUE.
              IYODD=.FALSE.
              IF((DBLE(NY)/2.0D0)-DBLE(NY/2).NE.0.0D0) IYODD=.TRUE.
              IF(IXODD) THEN
                  DO I=0,NX-1
                      DO J=1,NY
                          IIMAGEX2(I+1,J)=-(IWW3*DBLE((NX-1)/2))+(DBLE(I)*IWW3)
                      END DO
                  END DO
              END IF
              IF(.NOT.IXODD) THEN
                  DO I=1,NX
                      DO J=1,NY
                          IIMAGEX2(I,J)=-(IWW3*(DBLE(NX+1)/2.0D0))+(DBLE(I)*IWW3)
                      END DO
                  END DO
              END IF
              IF(IYODD) THEN
                  DO I=1,NX
                      DO J=0,NY-1
                          IIMAGEY2(I,J+1)=-(IWW4*DBLE((NY-1)/2))+(DBLE(J)*IWW4)
                      END DO
                  END DO
              END IF
              IF(.NOT.IYODD) THEN
                  DO I=1,NX
                      DO J=1,NY
                          IIMAGEY2(I,J)=-(IWW4*(DBLE(NY+1)/2.0D0))+(DBLE(J)*IWW4)
                      END DO
                  END DO
              END IF
              RETURN
          END IF
C
C       IOBJECTD
C
          IF(WC.EQ.'IOBJECTD') THEN
              IF(STI.EQ.1) THEN
                  WRITE(OUTLYNE,*)
     1            'OBJECT PLANE ARRAY DIMENSION NX = ',OBJNX
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)
     1            'OBJECT PLANE ARRAY DIMENSION NY = ',OBJNY
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)
     1            '    OBJECT PLANE PIXEL X-LENGTH = ',ODELX
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)
     1            '    OBJECT PLANE PIXEL Y-LENGTH = ',ODELY
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)
     1            '    OBJECT PLANE ARRAY X=EXTENT = ',REAL(OBJNX-1)*ODELX
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)
     1            '    OBJECT PLANE ARRAY Y-EXTENT = ',REAL(OBJNY-1)*ODELY
                  CALL SHOWIT(1)
                  RETURN
              END IF
              IF(SQ.NE.0.OR.SST.NE.0) THEN
                  WRITE(OUTLYNE,*)
     1            WC,'TAKES NO QUALIFIER OR STRING INPUT'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(S1.EQ.0) THEN
                  WRITE(OUTLYNE,*)
     1            WC,' REQUIRES EXPLICIT NUMERIC WORD #1 INPUT'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(S2.EQ.0) THEN
                  WRITE(OUTLYNE,*)
     1            WC,' REQUIRES EXPLICIT NUMERIC WORD #2 INPUT'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(S3.EQ.0) THEN
                  WRITE(OUTLYNE,*)
     1            WC,' REQUIRES EXPLICIT NUMERIC WORD #3 INPUT'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(S4.EQ.0) THEN
                  WRITE(OUTLYNE,*)
     1            WC,' REQUIRES EXPLICIT NUMERIC WORD #4 INPUT'
C       COMPUTE ARRAY DIMENTIONS
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C       COMPUTE ARRAY DIMENTIONS
              NX=INT(W3)
              NY=INT(W4)
              DNX=W3
              DNY=W4
              ODELX=W1
              ODELY=W2
              WRITE(OUTLYNE,*)
     1        'OBJECT PLANE ARRAY DIMENSION NX = ',NX
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)
     1        'OBJECT PLANE ARRAY DIMENSION NY = ',NY
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)
     1        '    OBJECT PLANE PIXEL X-LENGTH = ',ODELX
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)
     1        '    OBJECT PLANE ARRAY Y-LENGTH = ',ODELY
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)
     1        '    OBJECT PLANE ARRAY X-EXTENT = ',W1*(W3-1.0D0)
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)
     1        '    OBJECT PLANE ARRAY Y-EXTENT = ',W2*(W4-1.0D0)
              CALL SHOWIT(1)
              DEALLOCATE (IOBJECTX2,IOBJECTY2,IOBJECTX,IOBJECTY,
     1        IOBJECTV,STAT=ALLOERR)
              ALLOCATE (IOBJECTX2(NX,NY),
     1        IOBJECTY2(NX,NY),
     1        IOBJECTX((-NX/2):NX+(NX/2),(-NY/2):NY+(NY/2)),
     1        IOBJECTY((-NX/2):NX+(NX/2),(-NY/2):NY+(NY/2)),
     1        IOBJECTV((-NX/2):NX+(NX/2),(-NY/2):NY+(NY/2),3),
     1        STAT=ALLOERR)
              OBJNX=NX
              OBJNY=NY
              IOBJECTX2(1:NX,1:NY)=0.0D0
              IOBJECTY2(1:NX,1:NY)=0.0D0
              IOBJECTX((-NX/2):NX+(NX/2),(-NY/2):NY+(NY/2))=0.0D0
              IOBJECTY((-NX/2):NX+(NX/2),(-NY/2):NY+(NY/2))=0.0D0
              IOBJECTV((-NX/2):NX+(NX/2),(-NY/2):NY+(NY/2),1:3)=0.0D0
              IWW3=ODELX
              IWW4=ODELY
C
C       LOAD ARRAYS
              OXODD=.FALSE.
              IF((DBLE(NX)/2.0D0)-DBLE(NX/2).NE.0.0D0) OXODD=.TRUE.
              OYODD=.FALSE.
              IF((DBLE(NY)/2.0D0)-DBLE(NY/2).NE.0.0D0) OYODD=.TRUE.
              IF(OXODD) THEN
                  DO I=0,NX-1
                      DO J=1,NY
                          IOBJECTX(I+1,J)=-(IWW3*DBLE((NX-1)/2))+(DBLE(I)*IWW3)
                      END DO
                  END DO
              END IF
              IF(.NOT.OXODD) THEN
                  DO I=1,NX
                      DO J=1,NY
                          IOBJECTX(I,J)=-(IWW3*(DBLE(NX+1)/2.0D0))+(DBLE(I)*IWW3)
                      END DO
                  END DO
              END IF
              IF(OYODD) THEN
                  DO I=1,NX
                      DO J=0,NY-1
                          IOBJECTY(I,J+1)=-(IWW4*DBLE((NY-1)/2))+(DBLE(J)*IWW4)
                      END DO
                  END DO
              END IF
              IF(.NOT.OYODD) THEN
                  DO I=1,NX
                      DO J=1,NY
                          IOBJECTY(I,J)=-(IWW4*(DBLE(NY+1)/2.0D0))+(DBLE(J)*IWW4)
                      END DO
                  END DO
              END IF
              RETURN
          END IF
C
C       OBJVAL
C
          IF(WC.EQ.'OBJVAL') THEN
              IF(S4.NE.0.OR.S5.NE.0) THEN
                  WRITE(OUTLYNE,*)
     1            '"OBJVAL" TAKES NO NUMERIC WORD #4 OR #5 INPUT'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(SST.NE.0) THEN
                  WRITE(OUTLYNE,*)
     1            '"OBJVAL" TAKES NO STRING INPUT'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(SQ.EQ.0) THEN
                  WRITE(OUTLYNE,*)
     1            '"OBJVAL" REQUIRES QUALIFIER INPUT'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(WQ.NE.'M'.AND.WQ.NE.'R'.AND.WQ.NE.'G'.AND.WQ.NE.'B') THEN
                  WRITE(OUTLYNE,*)
     1            '"OBJVAL" REQUIRES QUALIFIER "M", "R", "G" OR "B"'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(S1.EQ.0) THEN
                  WRITE(OUTLYNE,*)
     1            '"OBJVAL" REQUIRES EXPLICIT NUMERIC WORD #1 INPUT'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(S2.EQ.0) THEN
                  WRITE(OUTLYNE,*)
     1            '"OBJVAL" REQUIRES EXPLICIT NUMERIC WORD #2 INPUT'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(S3.EQ.0) THEN
                  WRITE(OUTLYNE,*)
     1            '"OBJVAL" REQUIRES EXPLICIT NUMERIC WORD #3 INPUT'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF

              OBJNY=OBJNY

              IF(W1.LT.0.0D0) W1=0.0D0
              IF(W1.GT.DBLE(OBJNX)) W1=OBJNX
              IF(W2.LT.0.0D0) W2=0.0D0
              IF(W2.GT.DBLE(OBJNY)) W2=OBJNY
              IF(WQ.EQ.'M') THEN
                  IF(OBJNX.NE.0.AND.OBJNY.NE.0)
     1            IOBJECTV(INT(W1),INT(W2),1)=W3
              END IF
              IF(WQ.EQ.'R') THEN
                  IF(OBJNX.NE.0.AND.OBJNY.NE.0)
     1            IOBJECTV(INT(W1),INT(W2),3)=W3
              END IF
              IF(WQ.EQ.'G') THEN
                  IF(OBJNX.NE.0.AND.OBJNY.NE.0)
     1            IOBJECTV(INT(W1),INT(W2),1)=W3
              END IF
              IF(WQ.EQ.'B') THEN
                  IF(OBJNX.NE.0.AND.OBJNY.NE.0)
     1            IOBJECTV(INT(W1),INT(W2),2)=W3
              END IF
              RETURN
          END IF
C
C
C       PLTOBJ
C
          IF(WC.EQ.'PLTOBJ') THEN
              IF(STI.EQ.1) THEN
                  WRITE(OUTLYNE,*)
     1            '"PLTOBJ" PLOTS THE OBJECT ARRAY'
                  CALL SHOWIT(1)
                  RETURN
              END IF
              IF(SST.EQ.1.OR.SN.EQ.1.OR.SQ.EQ.1) THEN
                  WRITE(OUTLYNE,*)
     1            '"PLTOBJ" TAKES NO INPUT'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              TRIMMER=0.0D0
              IF(OBJNY.NE.0.AND.OBJNX.NE.0) THEN
                  CALL PLOTIMAGEARRAY(1,OBJNX*OBJNY,TRIMMER)
              ELSE
                  WRITE(OUTLYNE,*)
     1            'NO OBJECT ARRAY EXISTS TO PLOT'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
C
C       PLTIMG
C
          IF(WC.EQ.'PLTIMG') THEN
              IF(STI.EQ.1) THEN
                  WRITE(OUTLYNE,*)
     1            '"PLTIMG" PLOTS THE OBJECT ARRAY'
                  CALL SHOWIT(1)
                  RETURN
              END IF
              IF(SST.EQ.1.OR.SQ.EQ.1) THEN
                  WRITE(OUTLYNE,*)
     1            '"PLTIMG" TAKES NO QUALIFIER OR STRING INPUT'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
                  WRITE(OUTLYNE,*)
     1            '"PLTIMG" TAKES NO NUMERIC WORD #2 THROUGH #5 INPUT'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(W1.LT.0.0D0) THEN
                  WRITE(OUTLYNE,*)
     1            '"PLTIMG", NUMERIC WORD #1 NAY NOT BE NEGATIVE'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF((2.0D0*W1).GE.DBLE(REAL(IMGNX)).OR.
     1        (2.0D0*W1).GE.DBLE(REAL(IMGNY))) THEN
                  WRITE(OUTLYNE,*)
     1            'NUMBER OF TRIMMED PIXELS EXCEEDS IMAGE SIZE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)
     1            'NO ACTION TAKEN'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(DF1.EQ.1) W1=0.0D0
              TRIMMER=W1
              IF(IMGNY.NE.0.AND.IMGNX.NE.0) THEN
                  CALL PLOTIMAGEARRAY(2,IMGNX*IMGNY,TRIMMER)
              ELSE
                  WRITE(OUTLYNE,*)
     1            'NO IMAGE ARRAY EXISTS TO PLOT'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
C
C       OTOBMP
C
          IF(WC.EQ.'OTOBMP') THEN
              IF(STI.EQ.1) THEN
                  WRITE(OUTLYNE,*)
     1            '"OTOBMP" WRITES THE OBJECT ARRAY AS A .BMP FILE'
                  CALL SHOWIT(1)
                  RETURN
              END IF
              IF(SN.EQ.1.OR.SQ.EQ.1) THEN
                  WRITE(OUTLYNE,*)
     1            '"OTOBMP" TAKES NO QUALIFIER OR NUMERIC NO INPUT'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(SST.EQ.0) WS='OBJECTARRAY'
              BMPFILE=trim(HOME)//TRIM(TRIM(WS)//'.BMP')
              IF(OBJNY.NE.0.AND.OBJNX.NE.0) THEN
                  LENBMP=OBJNX*OBJNY*3
                  CALL WRITEIMAGEARRAY(1,LENBMP,TRIM(BMPFILE))
              ELSE
                  WRITE(OUTLYNE,*)
     1            'NO OBJECT ARRAY EXISTS TO WRITE'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
C
C       ITOBMP
C
          IF(WC.EQ.'ITOBMP') THEN
              IF(STI.EQ.1) THEN
                  WRITE(OUTLYNE,*)
     1            '"ITOBMP" WRITES THE IMAGE ARRAY AS A .BMP FILE'
                  CALL SHOWIT(1)
                  RETURN
              END IF
              IF(SN.EQ.1.OR.SQ.EQ.1) THEN
                  WRITE(OUTLYNE,*)
     1            '"ITOBMP" TAKES NO QUALIFIER OR NUMERIC NO INPUT'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(SST.EQ.0) WS='IMAGEARRAY'
              BMPFILE=trim(HOME)//TRIM(TRIM(WS)//'.BMP')
              IF(IMGNY.NE.0.AND.IMGNX.NE.0) THEN
                  LENBMP=IMGNX*IMGNY*3
                  CALL WRITEIMAGEARRAY(2,LENBMP,TRIM(BMPFILE))
              ELSE
                  WRITE(OUTLYNE,*)
     1            'NO IMAGE ARRAY EXISTS TO WRITE'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
C
C       OFROMBMP
C
          IF(WC.EQ.'OFROMBMP') THEN
              IF(STI.EQ.1) THEN
                  WRITE(OUTLYNE,*)
     1            '"OFROMBMP" READS A .BMP FILE INTO THE OBJECT ARRAY'
                  CALL SHOWIT(1)
                  RETURN
              END IF
              IF(S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1.OR.SQ.EQ.1) THEN
                  WRITE(OUTLYNE,*)
     1            '"OFROMBMP" TAKES NO QUALIFIER OR NUMERIC WORD #2 TO #5 INPUT'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(S1.EQ.0) THEN
                  WRITE(OUTLYNE,*)
     1            '"OFROMBMP" REQUIRES EXPILIT NUMERIC WORD #1 INPUT'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(SST.EQ.0) THEN
                  WRITE(OUTLYNE,*)
     1            '"OFROMBMP" REQUIRES A BMP FILE NAME WITHOUT AN EXTENSION'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              BMPFILE=trim(HOME)//TRIM(WS)//'.BMP'
              WRD1=W1
              CALL READIMAGEARRAY(1,WRD1)

          END IF
C
C       IFROMBMP
C
          IF(WC.EQ.'IFROMBMP') THEN
              IF(STI.EQ.1) THEN
                  WRITE(OUTLYNE,*)
     1            '"IFROMBMP" READS A .BMP FILE INTO THE IMAGE ARRAY'
                  CALL SHOWIT(1)
                  RETURN
              END IF
              IF(SST.EQ.0) THEN
                  WRITE(OUTLYNE,*)
     1            '"IFROMBMP" REQUIRES A BMP FILE NAME WITHOUT AN EXTENSION'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1.OR.SQ.EQ.1) THEN
                  WRITE(OUTLYNE,*)
     1            '"IFROMBMP" TAKES NO QUALIFIER OR NUMERIC WORD #2 TO #5 INPUT'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(S1.EQ.0) THEN
                  WRITE(OUTLYNE,*)
     1            '"IFROMBMP" REQUIRES EXPILIT NUMERIC WORD #1 INPUT'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF

              BMPFILE=trim(HOME)//TRIM(TRIM(WS)//'.BMP')
              WRD1=0.0D0

              CALL READIMAGEARRAY(2,WRD1)
          END IF
C
C       BMPREADR
C
          IF(WC.EQ.'BMPREADR') THEN
              IF(STI.EQ.1) THEN
                  WRITE(OUTLYNE,*)
     1            '"BMPREADR" SETS THE NAME OF THE "BMP" FILE DISPLAY APPLICATION'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)
     1            TRIM(BMPREADR)//' IS THE CURRENT "BMP" DISPLAY PROGRAM IN USE'
                  CALL SHOWIT(1)
                  RETURN
              END IF
              IF(SST.EQ.0) THEN
                  WRITE(OUTLYNE,*)
     1            '"BMPREADR" REQUIRES EXPLICIT STRING INPUT'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(SN.EQ.1.OR.SQ.EQ.1) THEN
                  WRITE(OUTLYNE,*)
     1            '"BMPREADR" TAKES NO QUALIFIER OR NUMERIC NO INPUT'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              BMPREADR=TRIM(WS)
          END IF
C
C     PSFTOIMG
C
          IF(WC.EQ.'PSFTOIMG') THEN
              IF(.NOT.PSFEXT) THEN
                  WRITE(OUTLYNE,*)
     1            'NO PSF EXISTS TO SAVE'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(S2.NE.0.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
                  WRITE(OUTLYNE,*)
     1            WC,' TAKES NO NUMERIC WORD #2 THROUGH #5 INPUT'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(SQ.EQ.1.OR.SST.EQ.1) THEN
                  WRITE(OUTLYNE,*)
     1            WC,' TAKES NO QUALIFIER OR STRING INPUT'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(DF1.EQ.1) THEN
                  W1=1.0D0
              END IF
              IF(W1.NE.1.0D0.AND.W1.NE.2.0D0.AND.W1.NE.3.0D0
     1        .AND.W1.NE.4.0D0) THEN
                  WRITE(OUTLYNE,*)
     1            'NUMERIC WORD ONE MAY BE ONLY "1", "2", "3" OR "4"'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(SST.NE.0) THEN
                  WRITE(OUTLYNE,*)
     1            WC,' TAKES NO STRING INPUT'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(IMGNX.EQ.0.OR.IMGNY.EQ.0) THEN
                  WRITE(OUTLYNE,*) 'NO IMAGE PLANE ARRAY EXISTS'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*) 'NO IMAGE TRACES CAN BE PERFORMED'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF



C       INDEX CREATION
C       INDEX 1,1 IS LOCATED AT THE -X,-Y LIMIT OF THE
C       CENTRAL HALF OF THE IMAGE X AND Y ARRAYS
C
C       USE THE COORDINATES OF EACH GRID IN THE CURRENT PSF WITH THE
C       CENTER OF THE PSF LOCATED WHERE THE CHIEF RAY PASSES THROUGH
C       THE IMAGE PLANE
C       CURRENT CHIEF RAY X AND Y IS LOCATED AT
              XCHIEF=XCENTOFF*GRIIMG
              YCHIEF=YCENTOFF*GRIIMG
C       NEGATIVE CORNER OF THE IMAGE ARRAY IS:
              XCORR=IIMAGEX2(1,1)
              YCORR=IIMAGEY2(1,1)
C       NEGATIVE CORNER OF THE PSF IS:
              PSFXCORNER=XCHIEF-(GRIIMG*(DBLE(PGRIMG-1)/2.0D0))
              PSFYCORNER=YCHIEF-(GRIIMG*(DBLE(PGRIMG-1)/2.0D0))
C       INDEX COUNTERS FOR THE PSF ARE LIMITED BY NSTART AND NSTOP
              NSTART=((MMMIMG-1)/2)-((PGRIMG-1)/2)+1
              NSTOP=((MMMIMG-1)/2)+((PGRIMG-1)/2)+1
C
C       SET X2 AND Y2 TO THE NEGATIVE LIMITS OF THE PSF
              Y2=PSFYCORNER-GRIIMG
              DO J=NSTART,NSTOP,1
                  Y2=Y2+GRIIMG
                  X2=PSFXCORNER-GRIIMG
                  DO I=NSTART,NSTOP,1
                      X2=X2+GRIIMG
                      IF(XCORR.LT.0.0D0) THEN
                          IX=NINT((X2-XCORR)/IDELX)+1
                      ELSE
                          IX=-NINT((X2-XCORR)/IDELX)+1
                      END IF
                      IF(YCORR.LT.0.0D0) THEN
                          IY=NINT((Y2-YCORR)/IDELY)+1
                      ELSE
                          IY=-NINT((Y2-YCORR)/IDELY)+1
                      END IF
                      IF(IX.LE.IMGNX.AND.IY.LE.IMGNY.AND.IX.GE.1.AND.
     1                IY.GE.1) THEN
                          IIMAGEV(IX,IY,1,INT(W1))=IIMAGEV(IX,IY,1,INT(W1))+
     1                    FIMG(I,J)
                      END IF
                  END DO
              END DO
              IIMAGEX(1:IMGNX,1:IMGNY)=IIMAGEX2(1:IMGNX,1:IMGNY)
              IIMAGEY(1:IMGNX,1:IMGNY)=IIMAGEY2(1:IMGNX,1:IMGNY)
          END IF

C
C
C     INTTOIMG
C
          IF(WC.EQ.'INTTOIMG') THEN
              IF(.NOT.INTENEXIST) THEN
                  WRITE(OUTLYNE,*)
     1            'NO INTENSITY DATA EXISTS TO SAVE'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(S2.NE.0.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
                  WRITE(OUTLYNE,*)
     1            WC,' TAKES NO NUMERIC WORD #2 THROUGH #5 INPUT'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(SQ.EQ.1.OR.SST.EQ.1) THEN
                  WRITE(OUTLYNE,*)
     1            WC,' TAKES NO QUALIFIER OR STRING INPUT'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(DF1.EQ.1) THEN
                  W1=1.0D0
              END IF
              IF(W1.NE.1.0D0.AND.W1.NE.2.0D0.AND.W1.NE.3.0D0
     1        .AND.W1.NE.4.0D0) THEN
                  WRITE(OUTLYNE,*)
     1            'NUMERIC WORD ONE MAY BE ONLY "1", "2", "3" OR "4"'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(SST.NE.0) THEN
                  WRITE(OUTLYNE,*)
     1            WC,' TAKES NO STRING INPUT'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(IMGNX.EQ.0.OR.IMGNY.EQ.0) THEN
                  WRITE(OUTLYNE,*) 'NO IMAGE PLANE ARRAY EXISTS'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*) 'NO IMAGE TRACES CAN BE PERFORMED'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C
C       SET X2 AND Y2 TO THE NEGATIVE LIMITS OF THE PSF
              Y2=PSFYCORNER-GRIIMG
              DO J=1,INTENNUM_PIXELS
                  DO I=1,INTENNUM_PIXELS

                      IF(I.LE.IMGNX.AND.J.LE.IMGNY.AND.I.GE.1.AND.
     1                J.GE.1) THEN
                          IIMAGEV(I,J,1,INT(W1))=IIMAGEV(I,J,1,INT(W1))+
     1                    INTEN_ARRAY(I,J,2)
                      END IF
                  END DO
              END DO
              IIMAGEX(1:IMGNX,1:IMGNY)=IIMAGEX2(1:IMGNX,1:IMGNY)
              IIMAGEY(1:IMGNX,1:IMGNY)=IIMAGEY2(1:IMGNX,1:IMGNY)
          END IF
C
          IF(WC.EQ.'IMGSLICE') THEN
              IF(DF2.EQ.1) W2=2.0D0
              IF(DF3.EQ.1) SL=1
              IF(DF3.EQ.0) SL=2
              IF(DF4.EQ.1) SCL=0
              IF(DF4.EQ.0) SCL=INT(W4)
              IF(W2.NE.1.0D0.AND.W2.NE.2.0D0.AND.W2.NE.3.0D0) THEN
                  WRITE(OUTLYNE,*)'"COLOR NUMBER MUST BE 1, 2 OR 3 FOR R, G OR B'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(W2.EQ.1.0D0) K=3
              IF(W2.EQ.2.0D0) K=1
              IF(W2.EQ.3.0D0) K=2

              IF(SQ.EQ.0) WQ='X'
              IF(WQ.NE.'X'.AND.WQ.NE.'Y'.AND.WQ.NE.'DIAG') THEN
                  WRITE(OUTLYNE,*)
     1            '"IMGSLICE" ONLY TAKES "X", "Y" AND "DIAG" AS VALID QUALIFIERS'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)
     1            'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(STI.EQ.1) THEN
                  WRITE(OUTLYNE,*) '"IMSLICE" PLOTS A SLICE THROUGH AN IMAGE'
                  CALL SHOWIT(1)
              END IF
              IF(SST.EQ.1) THEN
                  WRITE(OUTLYNE,*) '"IMSLICE" TAKES NO STRING INPUT'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(WQ.EQ.'DIAG'.AND.IMGNX.NE.IMGNY) THEN
                  WRITE(OUTLYNE,*) '"IMSLICE DIAG" ONLY WORKS FOR SQUARE IMAGES'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(W1.GT.IMGNX.AND.WQ.EQ.'X') THEN
                  WRITE(OUTLYNE,*)
     1            'X-OFFSET EXCEEDS THE SIZE OF THE IMAGE DIMENSION IN X'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(W1.GT.IMGNY.AND.WQ.EQ.'Y') THEN
                  WRITE(OUTLYNE,*)
     1            'Y-OFFSET EXCEEDS THE SIZE OF THE IMAGE DIMENSION IN Y'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF

C       NW1 IS OFFSET IN IMAGE PIXELS FROM THE BOTTOM OF THE IMAGE
C       WHEN "X" IS USED, AND THE OFFSET IN IMAGE PIXELS WHEN "Y" IS USED.
C       DEFAULT W1 WILL BE INT(DBLE(IMG(NX-1 OR NY-1))/2.0D0)+1
C
              IF(DF1.EQ.1.AND.WQ.EQ.'X') THEN
                  W1=NINT(DBLE(IMGNX-1)/2.0D0)
                  JJ=INT(W1)
              END IF
              IF(DF1.EQ.1.AND.WQ.EQ.'Y') THEN
                  W1=NINT(DBLE(IMGNY-1)/2.0D0)
                  II=INT(W1)
              END IF
              IF(SL.EQ.1) THEN
C       LINEAR PLOT
                  PEAKVAL=-1.0D300
                  IMGTEMP(1:IMGNX,1:IMGNY,1:3,1:4)=
     1            IIMAGEV(1:IMGNX,1:IMGNY,1:3,1:4)
                  DO J=1,IMGNY
                      DO I=1,IMGNX
                          DO M=1,3
                              DO P=1,4
                                  IF(IMGTEMP(I,J,M,P).GT.PEAKVAL) PEAKVAL=IMGTEMP(I,J,M,P)
                              END DO
                          END DO
                      END DO
                  END DO
              ELSE
C       LOG PLOT
                  PEAKVAL=-1.0D300
                  IMGTEMP(1:IMGNX,1:IMGNY,1:3,1:4)=
     1            (IIMAGEV(1:IMGNX,1:IMGNY,1:3,1:4))
                  DO J=1,IMGNY
                      DO I=1,IMGNX
                          DO M=1,3
                              DO P=1,4
                                  IF(IMGTEMP(I,J,M,P).LT.1.0D0) IMGTEMP(I,J,M,P)=1.0D0
                              END DO
                          END DO
                      END DO
                  END DO
                  IMGTEMP(1:IMGNX,1:IMGNY,1:3,1:4)=
     1            DLOG10(IMGTEMP(1:IMGNX,1:IMGNY,1:3,1:4))
                  DO J=1,IMGNY
                      DO I=1,IMGNX
                          DO M=1,3
                              DO P=1,4
                                  IF(IMGTEMP(I,J,M,P).GT.PEAKVAL) PEAKVAL=IMGTEMP(I,J,M,P)
                              END DO
                          END DO
                      END DO
                  END DO
              END IF
C
              WWQW=WQ
              DO KK=1,4
C       PROCEED WITH PLOTTING
C       LOAD THE INDEPENDENT VALRIABLE
                  IF(WWQW.EQ.'X') THEN
                      L=101
                      DO II=1,IMGNX
                          GPREG(L)=IIMAGEX(II,JJ)
                          L=L+1
                      END DO
                      DO II=1,IMGNX
                          GPREG(L)=IMGTEMP(II,JJ,K,KK)
                          IF(K.EQ.1.AND.KK.EQ.2.AND.IMGTEMP(II,JJ,K,KK).NE.0.0D0) THEN
                          END IF
                          L=L+1
                      END DO
                  END IF
                  IF(WWQW.EQ.'Y') THEN
                      L=101
                      DO JJ=1,IMGNY
                          GPREG(L)=IIMAGEY(II,JJ)
                          L=L+1
                      END DO
                      DO JJ=1,IMGNY
                          GPREG(L)=IMGTEMP(II,JJ,K,KK)
                          L=L+1
                      END DO
                  END IF
                  IF(WWQW.EQ.'DIAG') THEN
                      L=101
                      DO II=1,IMGNY
                          GPREG(L)=IIMAGEY(II,II)*DSQRT(2.0D0)
                          L=L+1
                      END DO
                      DO II=1,IMGNY
                          GPREG(L)=IMGTEMP(II,JJ,K,KK)
                          L=L+1
                      END DO
                  END IF
                  IF(WWQW.EQ.'X') N=1
                  IF(WWQW.EQ.'Y') N=2
                  IF(WWQW.EQ.'DIAG') N=3
                  IF(N.EQ.1) THEN
                      WRITE(INPUT,*) 'PLOT UXAXIS,',GPREG(101),GPREG(100+IMGNX)
                      CALL PROCES
                      IF(SYSTEM1(30).GE.3.0D0)
     1                INPUT='PLOT UXAXISLB X-EXTENT (RADIANS)'
                      IF(SYSTEM1(30).LE.2.0D0) THEN
                          IF(SYSTEM1(6).EQ.1.0D0)
     1                    INPUT='PLOT UXAXISLB X-EXTENT (INCH)'
                          IF(SYSTEM1(6).EQ.2.0D0)
     1                    INPUT='PLOT UXAXISLB X-EXTENT (CM)'
                          IF(SYSTEM1(6).EQ.3.0D0)
     1                    INPUT='PLOT UXAXISLB X-EXTENT (MM)'
                          IF(SYSTEM1(6).EQ.4.0D0)
     1                    INPUT='PLOT UXAXISLB X-EXTENT (METER)'
                      END IF
                      CALL PROCES
                  END IF
                  IF(N.EQ.2) THEN
                      WRITE(INPUT,*) 'PLOT UXAXIS,',GPREG(101),GPREG(100+IMGNY)
                      CALL PROCES
                      IF(SYSTEM1(30).GE.3.0D0)
     1                INPUT='PLOT UXAXISLB Y-EXTENT (RADIANS)'
                      IF(SYSTEM1(30).LE.2.0D0) THEN
                          IF(SYSTEM1(6).EQ.1.0D0)
     1                    INPUT='PLOT UXAXISLB Y-EXTENT (INCH)'
                          IF(SYSTEM1(6).EQ.2.0D0)
     1                    INPUT='PLOT UXAXISLB Y-EXTENT (CM)'
                          IF(SYSTEM1(6).EQ.3.0D0)
     1                    INPUT='PLOT UXAXISLB Y-EXTENT (MM)'
                          IF(SYSTEM1(6).EQ.4.0D0)
     1                    INPUT='PLOT UXAXISLB Y-EXTENT (METER)'
                      END IF
                      CALL PROCES
                  END IF
                  IF(N.EQ.3) THEN
                      WRITE(INPUT,*) 'PLOT UXAXIS,',GPREG(101),GPREG(100+IMGNY)
                      CALL PROCES
                      IF(SYSTEM1(30).GE.3.0D0)
     1                INPUT='PLOT UXAXISLB DIAG-EXTENT (RADIANS)'
                      IF(SYSTEM1(30).LE.2.0D0) THEN
                          IF(SYSTEM1(6).EQ.1.0D0)
     1                    INPUT='PLOT UXAXISLB DIAG-EXTENT (INCH)'
                          IF(SYSTEM1(6).EQ.2.0D0)
     1                    INPUT='PLOT UXAXISLB DIAG-EXTENT (CM)'
                          IF(SYSTEM1(6).EQ.3.0D0)
     1                    INPUT='PLOT UXAXISLB DIAG-EXTENT (MM)'
                          IF(SYSTEM1(6).EQ.4.0D0)
     1                    INPUT='PLOT UXAXISLB DIAG-EXTENT (METER)'
                      END IF
                      CALL PROCES
                  END IF
C
                  IF(SL.EQ.1) THEN
                      IF(SCL.EQ.0) THEN
                          WRITE(INPUT,*)
     1                    'PLOT UYAXIS, 0,',INT(NINT(PEAKVAL))
                      ELSE
                          WRITE(INPUT,*)
     1                    'PLOT UYAXIS, 0,',SCL
                      END IF
                      CALL PROCES
                      WRITE(INPUT,*)
     1                'PLOT UXLINE,0.0'
                      CALL PROCES
                      INPUT='PLOT UYAXISLB INTENSITY'
                      CALL PROCES
                      IF(N.EQ.1.OR.N.EQ.3) THEN
                          WRITE(INPUT,*) 'PLOT UPLOT,',101,100+IMGNX
     1                    ,101+IMGNX,100+IMGNX+IMGNX
                          CALL PROCES
                      ELSE
                          WRITE(INPUT,*) 'PLOT UPLOT,',101,100+IMGNY
     1                    ,101+IMGNY,100+IMGNY+IMGNY
                          CALL PROCES
                      END IF
                  ELSE
                      IF(SCL.EQ.0) THEN
                          WRITE(INPUT,*)
     1                    'PLOT UYAXIS, 0,',INT(NINT(PEAKVAL))
                      ELSE
                          WRITE(INPUT,*)
     1                    'PLOT UYAXIS, 0,',SCL
                      END IF
                      CALL PROCES
                      WRITE(INPUT,*)
     1                'PLOT UXLINE,0.0'
                      CALL PROCES
                      INPUT='PLOT UYAXISLB LOG INTENSITY'
                      CALL PROCES
                      IF(N.EQ.1.OR.N.EQ.3) THEN
                          WRITE(INPUT,*) 'PLOT UPLOT,',101,100+IMGNX
     1                    ,101+IMGNX,100+IMGNX+IMGNX
                          CALL PROCES
                      ELSE
                          WRITE(INPUT,*) 'PLOT UPLOT,',101,100+IMGNY
     1                    ,101+IMGNY,100+IMGNY+IMGNY
                          CALL PROCES
                      END IF
                  END IF
              END DO
          END IF
          IF(WC.EQ.'LMINUSR') THEN
              IF(DF1.EQ.1) W1=0.001D0
              IF(DF2.EQ.1) W2=2.0D0
              LLIM=W1
              IF(W2.NE.1.0D0.AND.W2.NE.2.0D0.AND.W2.NE.3.0D0) THEN
                  WRITE(OUTLYNE,*)'"COLOR NUMBER MUST BE 1, 2 OR 3 FOR R, G OR B'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(W2.EQ.1.0D0) K=3
              IF(W2.EQ.2.0D0) K=1
              IF(W2.EQ.3.0D0) K=2

              IF(SQ.NE.0) THEN
                  WRITE(OUTLYNE,*)
     1            '"LMINUSR" TAKES NO QUALIFIER INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)
     1            'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(STI.EQ.1) THEN
                  WRITE(OUTLYNE,*) '"RMINUSL" PLOTS A RIGHT MINUS LEFT ANALYSIS'
                  CALL SHOWIT(1)
              END IF
              IF(SST.EQ.1) THEN
                  WRITE(OUTLYNE,*) '"RMINUSL" TAKES NO STRING INPUT'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              JJ=NINT(DBLE(IMGNX-1)/2.0D0)
              PEAKVAL=-1.0D300
              IMGTEMP(1:IMGNX,1:IMGNY,1:3,1:4)=
     1        IIMAGEV(1:IMGNX,1:IMGNY,1:3,1:4)
              DO J=1,IMGNY
                  DO I=1,IMGNX
                      DO M=1,3
                          DO P=1,4
                              IF(IMGTEMP(I,J,M,P).GT.PEAKVAL) PEAKVAL=IMGTEMP(I,J,M,P)
                          END DO
                      END DO
                  END DO
              END DO
              IF(PEAKVAL.EQ.0.0D0) PEAKVAL=1.0D0
              DO J=1,IMGNY
                  DO I=1,IMGNX
                      DO M=1,3
                          DO P=1,4
                              IMGTEMP(I,J,M,P)=(IMGTEMP(I,J,M,P)/PEAKVAL)
                          END DO
                      END DO
                  END DO
              END DO
C
C       PROCEED WITH PLOTTING
C       LOAD THE INDEPENDENT VALRIABLE
              L=101
              DO II=1,IMGNX
                  GPREG(L)=IIMAGEX(II,JJ)
                  L=L+1
              END DO
              DO II=1,IMGNX
                  IF(IMGTEMP(II,JJ,K,1).GT.LLIM.AND.
     1            IMGTEMP(II,JJ,K,2).GT.LLIM.AND.
     2            IMGTEMP(II,JJ,K,3).GT.LLIM.AND.
     3            IMGTEMP(II,JJ,K,4).GT.LLIM) THEN
                      A=IMGTEMP(II,JJ,K,1)+IMGTEMP(II,JJ,K,2)
                      B=IMGTEMP(II,JJ,K,3)+IMGTEMP(II,JJ,K,4)
                  ELSE
                      A=0.0D0
                      B=0.0D0
                  END IF
                  IF((A+B).EQ.0.0D0) THEN
                      GPREG(L)=0.0D0
                  ELSE
                      GPREG(L)=(A-B)/(A+B)
                  END IF
                  L=L+1
              END DO

              WRITE(INPUT,*) 'PLOT UXAXIS,',GPREG(101),GPREG(100+IMGNX)
              CALL PROCES
              INPUT='PLOT UXAXISLB EXTENT OF IMAGE'
              CALL PROCES
              WRITE(INPUT,*)
     1        'PLOT UYAXIS, -1,1'
              CALL PROCES
              WRITE(INPUT,*)
     1        'PLOT UXLINE,0.0'
              CALL PROCES
              INPUT='PLOT UYAXISLB RIGHT MINUS LEFT'
              CALL PROCES
              WRITE(INPUT,*) 'PLOT UPLOT,',101,100+IMGNX
     1        ,101+IMGNX,100+IMGNX+IMGNX
              CALL PROCES
          END IF
          RETURN
C
          RETURN
      END
