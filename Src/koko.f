!///////////////////////////////////////////////////////////////////////
!/
!/ Copyright (C) 2020 The Koko Project Developers
!/
!/ See the file COPYRIGHT.md in the top-level directory of this
!/ distribution
!/
!/ This file is part of Koko.
!/
!/ Koko is free software: you can redistribute it and/or modify it
!/ under the terms of the GNU General Public License as published by
!/ the Free Software Foundation, either version 3 of the License, or
!/ (at your option) any later version.
!/
!/ Koko is distributed in the hope that it will be useful, but
!/ WITHOUT ANY WARRANTY; without even the implied warranty of
!/ MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!/ GNU General Public License for more details.
!/
!/ You should have received a copy of the GNU General Public License
!/ along with Koko; see the file COPYING.  If not, see
!/ <https://www.gnu.org/licenses/>.
!/
!///////////////////////////////////////////////////////////////////////

      PROGRAM KOKO

          USE GLOBALS
          USE NSSMOD
          USE opsys
          USE configfile
          USE getoptions

          IMPLICIT NONE

!       THIS PROGRAM CONTROLS ALL INPUT AND OUTPUT AND CAN
!       CALL SUROUTINES WHICH CAN PERFORM ANY DESIRED COMPUTATION.
!
!       READ THE COMMENTS, THEY TELL YOU WHAT EVERYTHING
!       DOES (SOMETIMES).
!
!       THE COMPLETE LIST OF VALID PROGRAM COMMANDS IS GIVEN
!       WHERE THE ARRAY WCC IS DEFINED IN NAMES.FOR

          REAL*8 VIEROT

          INTEGER OLDOUTOLD,FOTLIM,VIEXOF,VIEYOF,N3,OIN,IREND
          integer CMDNO !Number of input commands

          COMMON/LIMMER/FOTLIM
          COMMON/OFFVIE/VIEXOF,VIEYOF,VIEROT

          REAL*8 F1X,F1Y,F2X,F2Y,F3X,F3Y
          COMMON/FANFOB/F1X,F1Y,F2X,F2Y,F3X,F3Y

          LOGICAL FANEXT,EXIS22
          COMMON/FANEXI/FANEXT

          LOGICAL EXIS7,OPENIN
          LOGICAL ATTOP,ATBOT,EXIS89,OPEN44,EXIS44

          COMMON/TOPBOT/ATTOP,ATBOT

          CHARACTER MCNAM1*8,FILNAM*10,DOSKEY(1:100)*140,
     1    DDDAT*10,TTTIM*8,DMNAM*10,STRUC*140,AI4*4

          COMMON/JKSTRUC/STRUC

          INTEGER N,DOSCNT(1:100),OLDIN,ALLOERR
          COMMON/DOSKEE/DOSKEY ! ??? not used
          COMMON/DOCKNT/DOSCNT

          COMMON/STRNGR/DDDAT,TTTIM,MCNAM1,DMNAM
          CHARACTER AI*3,BLANK*80,BL140*140,DRWNAM*11

          COMMON/DRWTAG/DRWNAM

          INTEGER J,I,K,L,FLG(0:20),MAXPAG,LINBOT,LINCUR,KLI

          LOGICAL DEG,RAD,TANG
          COMMON/ANGMOD/DEG,RAD,TANG

          LOGICAL EXISMC

          COMMON/FFL/FLG

          COMMON/STAT13/LINCUR,LINBOT,MAXPAG

          CHARACTER HCOL(0:9)*12,HCOL2(0:9)*12,
     1    HROW(0:100)*12,HROW2(0:100)*12,WCOLD*8,ffile*40

          COMMON/HDTAB/HCOL,HCOL2,HROW,HROW2

          INTEGER NF
          COMMON/NEFER/NF

          COMMON/COMNAM/FILNAM

          LOGICAL LBLHT

          COMMON/JKA/LBLHT

          INTEGER PIXAR1,PIXAR2,II

          CHARACTER*80 DATA

          COMMON/TOYSTORY/PIXAR1,PIXAR2

          REAL*8 DILUPPER,DILLOWER
          COMMON/DILLIM/DILUPPER,DILLOWER

          LOGICAL GETTER,DERVAL
          COMMON/RETTEG/GETTER,DERVAL

          LOGICAL ROTSAGFL
          COMMON/SAGFLROT/ROTSAGFL

          LOGICAL NOCOBSPSF
          COMMON/PSFCOBS/NOCOBSPSF

          LOGICAL TABEXIST
          COMMON/EXISTAB/TABEXIST

          LOGICAL GROPEN(1:10)
          COMMON/OPENGR/GROPEN

          LOGICAL ITERROR

          INCLUDE 'datlen.inc'
          INCLUDE 'datcfg.inc'
          INCLUDE 'datmac.inc'
          INCLUDE 'datsub.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datspd.inc'
          INCLUDE 'datsp1.inc'
          INCLUDE 'dathgr.inc'

          LOGICAL has_userhome, has_kodsdir

          ! for configuration file
          CHARACTER(len=256) :: cfg_file, test_file
          TYPE(CFG_t)        :: koko_cfg

          ! for command line options
          character          :: ch
          logical            :: quiet
          
!--- Configuration Options ----------------------------------
          
          ! first look up user home directory
          CALL user_home_directory(has_userhome, USERHOME)

          ! check if the KODS directory is in the user's home directory
          IF ( has_userhome .AND. kods_dir_exists(USERHOME) ) THEN
             CALL dir_path_append(HOME, USERHOME, "KODS")
          END IF

          ! set directory for temporary files
          CALL set_kods_temp_dir(TMPDIR)
          IF (len_TRIM(TMPDIR) == 0) THEN
             TMPDIR = HOME ! last resort
          END IF
          
          ! parse the configuration file
          IF ( has_userhome ) THEN
             CALL dir_path_append(cfg_file, USERHOME, '.kokorc')
             IF ( file_exists(cfg_file) ) THEN
                ! define config default values
                CALL CFG_add(koko_cfg, "directories%home", HOME,      "Koko lib directory")
                CALL CFG_add(koko_cfg, "directories%temp", TMPDIR,    "Koko temp directory")
                CALL CFG_add(koko_cfg, "graphics%viewer",  "gnuplot", "Koko graphics viewer")
                CALL CFG_add(koko_cfg, "text%editor",      "vi",      "Koko text editor")
                
                CALL CFG_read_file(koko_cfg, cfg_file)

                ! update config variables from file
                CALL CFG_get(koko_cfg, "directories%home", HOME)
                CALL CFG_get(koko_cfg, "directories%temp", TMPDIR)
                CALL CFG_get(koko_cfg, "graphics%viewer",  BMPREADR)
                CALL CFG_get(koko_cfg, "text%editor",      TXTEDITOR)
             END IF
          END IF

          ! error check
          CALL dir_path_append(test_file, HOME, "README_DATA")
          INQUIRE(file = test_file, exist = has_kodsdir)
          IF ( .NOT. has_kodsdir ) THEN
             WRITE (*,*) ' '
             WRITE (*,*) 'Fatal error: KODS library directory not found.'
             WRITE (*,*) ' '
             STOP 1
          END IF

!--- Command Line Options ----------------------------------

          quiet     = .FALSE.
          batchmode = .FALSE.
          
          IF ( command_argument_count() > 0 ) THEN
             DO
                ch = getopt("bdhqt:")
                SELECT CASE( ch )
                CASE (char(0))
                   EXIT
                CASE ('b')
                   IF ( len_trim(optarg) == 0 ) THEN
                      WRITE (*,*) "Missing argument for command option -b"
                      STOP
                   END IF
                   batchmode = .TRUE.
                   BATCHFILE = optarg
                CASE ('d')
                   IF ( len_trim(optarg) == 0 ) THEN
                      WRITE (*,*) "Missing argument for command option -d"
                      STOP
                   END IF
                   HOME = optarg
                CASE ('h')
                   CALL print_usage()
                   STOP
                CASE ('t')
                   IF ( len_trim(optarg) == 0 ) THEN
                      WRITE (*,*) "Missing argument for command option -t"
                      STOP
                   END IF
                   TMPDIR = optarg
                CASE ('q' )
                   quiet = .TRUE.
                CASE default
                   WRITE (*,*) "Unrecognized command flag -"//ch
                   STOP
                END SELECT
             END DO
          END IF
          
          ! HOME needs to terminate with (back-) slash ...
          CALL add_dir_slash( HOME )
          
!--- Initialize Koko ---------------------------------------

!     NO NSS SURFACES
          SYSTEM1(102)=0.0D0

          MRAYS=200

!       USER PLOT RANGES AND OFFSETS
          UIXSTART=2000
          UIXSTOP=8000
          UIYSTART=1400
          UIYSTOP=5600
          UUDX=5.0
          UUDY=10.0

!       MULTICOM SET TO .FALSE. IS THE DEFAULT
          MULTICOM=.FALSE.

!       RAY HISTORY MAXRAYS
          RHIST_MAXRAYS=1000
          SHORTHIST=.FALSE.
          LONGHIST=.FALSE.

!       DIFFERENTIAL RAY AIMING ON
          AIMRFDIF=.TRUE.
          AIMRYDIF=.TRUE.

!       FRAME DRAWING FLAG
          FRAME=.FALSE.

          GUIERROR=.FALSE.
          NUMCOLORS=1
          OBJLEVEL=1.0D0
          SYSTEM1(11)=1.0D0
          SYSTEM1(7)=2.0D0
          SYSTEM1(8)=3.0D0

!       SET ITERROR TO FALSE
          ITERROR=.FALSE.

!       INITIALIZE NRECL TO 4 FOR NON-VAX (NUMBER OF BYTES)
!       REAL*8 = 2*4=8 BYTES
          NRECL=4

!       DIMENSIONS FOR IMAGING ARRAYS 9/2002
          OBJNX=0
          OBJNY=0

!       Apodization
          APODGAUSS=.FALSE.
          APODDBLOSS=0.0D0

          AA='                    '
          BB='        '
          CNULL=AA//AA//AA

          MFG='                 '

          GROPEN(1:10)=.FALSE.

          NEUTFILE=.TRUE.

          THMINLIM=-1.0D20
          THMAXLIM=1.0D20
          RDNEGLIM=-1.0D-20
          RDPOSLIM=1.0D-20

          MACFAILURE=.TRUE.
!
          TABEXIST=.FALSE.

!       SET MAKEAUTO TO MONOCHROMATIC MODE
          CHROMATIC=.FALSE.

!       SET NEXISTN TO FALSE
          NEXISTN=.FALSE.

          RAYCLEAR=.TRUE.
!     RAY CLEAR IS ONLY FALSE DURING CONFIGS, SPSRF TYPE 13 RAY TRACING
!
!       SET NODRAW AND NOWMF TO DEFAULTS
          NODRAW=.FALSE.
          NOWMF=.TRUE.

          HALTING=.FALSE.

!     INITIALIZE THE NIM/MAX REGISTERS
          MIN_REG(1:100)=1.0D300
          MAX_REG(1:100)=-1.0D300
          MAXNEUTRAL=50000
          COHERENCE_FACTOR=0.0D0

          CHRSHIFTEXT=.FALSE.

          SCFAY=1.0D0
          SCFAX=1.0D0
          SCFAYP=1.0D0
          SCFAXP=1.0D0

!     ASSIGN 3D LINE DEFAULT VALUES
          LX1=0.0D0
          LY1=0.0D0
          LZ1=0.0D0
          LX2=0.0D0
          LY2=0.0D0
          LZ2=0.0D0

          DLEICA(0)=.TRUE.
          DLEICA(1:10)=.TRUE.
          GLEICA(1:10)=.TRUE.

!     SET FOR FAR OBJECTS IN DOTF/GOTF
          NEAR_FAR=1

          IF ( .not. quiet ) THEN
             CALL GREETING
          END IF

          OPTM_INIT=1
!
!     LIN/LOG PSF SCALING
          PSFLIN=1
          PSFLOG=2
          ID_SYSTEM=-1000
          CALL GETOPSYS(ID_SYSTEM)
!
!     SET USING OLD REFERENCE RAY DATA TO FALSE
!
          USEOLREF=.FALSE.
          SAVEREF=.FALSE.
          NOCOBSPSF=.FALSE.
          
!     INITIALIZE PROGRAM RANDOM NUMBER GENERATOR
          CALL random_seed() ! Fortran RNG, new sequence
!
!     ADDED 1/12/99
          CARTMAN=.TRUE.
          ROTSAGFL=.FALSE.
!
!     NO STREHL RATIO EXISTS YET
          RSTREHL_EXIST=.FALSE.

!     NEXT TWO VARIABLE TRACK STATUS FOR CAPFNIN COMMAND
!
          ALLO=.FALSE.
          ALLONUM=0
!
          OUT=98
          SN=0
          STI=0
          SST=0
          SQ=0
          SST=1
          WS=trim(HOME)//'LENSES  '
          CALL LENSLOC
!     INPUT='OUT TP'
          CALL PROCES
          OUT=6
!
          TFTMIN=-0.005D0
          TFTMAX=0.005D0
          TFDELT=0.001D0
          TFDIRECTION= 2
          DILLOWER=1.0D0
          DILUPPER=1000.0D0
          GETTER=.FALSE.
!     THIS IS USED WHEN GETTING A DERIVATIVE VALUE
          GRSPT=0
          GOOY=.FALSE.
          MTFGRIDS=.FALSE.
          PLTLLI=.FALSE.
          PLTLBL=.FALSE.
          LBLSURF=-99
!
!     SET MACRO OVERWRITEING NOW ALWAYS ON
          ORITEM=.TRUE.
!
!     SET VIGOFF TO .FALSE.
          VIGOFF=.FALSE.
!     SET VSYM TO .TRUE.
          VSYM=.TRUE.
!     SET APPEND TO FALSE FOR EDITTEXT.DAT, CARDTEXT.DAT AND PUNCH.DAT

          ffile=trim(HOME)//'SENSIOUT.DAT'
          OPEN(UNIT=19,ACCESS='APPEND',BLANK='NULL'
     1      ,FORM='FORMATTED',FILE=ffile
     2      ,STATUS='UNKNOWN')
          CALL CLOSE_FILE(19,0)

          ffile=trim(HOME)//'ISENSOUT.DAT'
          OPEN(UNIT=19,ACCESS='APPEND',BLANK='NULL'
     1      ,FORM='FORMATTED',FILE=ffile
     2      ,STATUS='UNKNOWN')
          CALL CLOSE_FILE(19,0)

          ffile=trim(HOME)//'MONTEOUT.DAT'
          OPEN(UNIT=19,ACCESS='APPEND',BLANK='NULL'
     1      ,FORM='FORMATTED',FILE=ffile
     2      ,STATUS='UNKNOWN')
          CALL CLOSE_FILE(19,0)

          APPEND=.FALSE.
          ALLSET=.FALSE.

!     SET TIME AND DATE STAMPING OF THE LI TO "OFF"
          STMPT=.FALSE.
          STMPD=.FALSE.

          XPEN=0
          XPENOL=0
          YPEN=0
          YPENOL=0
          PENSTA=3

          RADUNI=1
!
!     SET DINCR DEFAULTS
!
          DINC1=1.0D-5
          DINC2=1.0D-5
          DINC3=1.0D-5
          DINC4=1.0D-7
          DINC5A=1.0D-11
          DINC6A=1.0D-15
          DINC7A=1.0D-19
          DINC8A=1.0D-23
          DINC9A=1.0D-27
          DINC10A=1.0D-31
          DINC11A=1.0D-35
          DINC12A=1.0D-39
          DINC13A=1.0D-42
          DINC14A=1.0D-7
          DINC5B=1.0D-7
          DINC6B=1.0D-7
          DINC7B=1.0D-7
          DINC8B=1.0D-7
          DINC9B=1.0D-7
          DINC10B=1.0D-7
          DINC11B=1.0D-7
          DINC12B=1.0D-7
          DINC13B=1.0D-7
          DINC14B=1.0D-7
          DINC15=1.0D-5
          DINC16=1.0D-5
          DINC17=1.0D-7
          DINC18A=1.0D-11
          DINC19A=1.0D-15
          DINC20A=1.0D-19
          DINC21A=1.0D-23
          DINC18B=1.0D-7
          DINC19B=1.0D-7
          DINC20B=1.0D-7
          DINC21B=1.0D-7
          DINC22=1.0D-7
          DINC23=1.0D-7
          DINC24=1.0D-7
          DINC25=1.0D-7
          DINC26=1.0D-7
          DINC27=1.0D-6
          DINC28=1.0D-10
          DINC29=1.0D-7
          DINC30=0.1D-3
          NSSDINC(1:120)=1.0D-7
!
!     SET DELTT DEFAULTS
!
          DELTT1=0.1D0
          DELTT2=0.1D0
          DELTT1A=4.0D0
          DELTT2A=4.0D0
          DELTT3=0.001D0
          DELTT4=1.0D-6
          DELTT5A=1.0D-10
          DELTT6A=1.0D-14
          DELTT7A=1.0D-18
          DELTT8A=1.0D-22
          DELTT9A=1.0D-26
          DELTT10A=1.0D-30
          DELTT11A=1.0D-34
          DELTT12A=1.0D-36
          DELTT13A=1.0D-40
          DELTT14A=1.0D-6
          DELTT5B=1.0D-6
          DELTT6B=1.0D-6
          DELTT7B=1.0D-6
          DELTT8B=1.0D-6
          DELTT9B=1.0D-6
          DELTT10B=1.0D-6
          DELTT11B=1.0D-6
          DELTT12B=1.0D-6
          DELTT13B=1.0D-6
          DELTT14B=1.0D-6
          DELTT15=0.025D0
          DELTT16=0.025D0
          DELTT15A=1.0D0
          DELTT16A=1.0D0
          DELTT17=1.0D-6
          DELTT18A=1.0D-10
          DELTT19A=1.0D-14
          DELTT20A=1.0D-18
          DELTT21A=1.0D-22
          DELTT18B=1.0D-6
          DELTT19B=1.0D-6
          DELTT20B=1.0D-6
          DELTT21B=1.0D-6
          DELTT22=0.057D0
          DELTT23=0.057D0
          DELTT24=0.057D0
          DELTT25=0.001D0
          DELTT26=0.001D0
          DELTT27=1.0D-5
          DELTT28=1.0D-10
          DELTT29=0.001D0
!
!
!
!     FOOTPRINT GRID SEMI-EXTENT = 8
          FOTLIM=8
!
!     SET DIFFRACTION SPACE TO "I"
          SPACEBALL=2
!
          OPEN (UNIT=98,BLANK='NULL'
     1    ,FILE=trim(HOME)//'NUL')
!
!     DEFAULTS FOR VIE PLOTS
          VIEXOF=0
          VIEYOF=0
          VIEROT=0.0D0
!
!     SET DEFAULTS FOR AUTO FAN PLOTS
!
          F1Y=1.0D0
          F2Y=0.7D0
          F3Y=0.0D0
          F1X=0.0D0
          F2X=0.0D0
          F3X=0.0D0
!
!     TELLS PLOTTING FANS THAT A FAN DOES NOT EXIST
!               FANEXT=.FALSE.
!
!     PSF TAGGER DEFAULT
          PSFTAG='     PSF.DAT'
!
!     PSF LI DEFAULT
          PSFLI=
     1    'THIS IS AN ASCII REPRESENTATION OF A POINT SPREAD FUNCTION'
!
          CAPDEF=16
!     CAPFN GRID DEFAULT IN OPTIMIZATION
          OPNRD=16
!     CAPFN GRID DEFAULT IN TOLERANCING
          TOLNRD=16
!     CAPFN GRID DEFAULT IN PSF CALCULATIONS
          NRD=16
          NRDFLG=1
!     FFT GRID DEFAULT IN PSF CALCULATIONS
          TGR=64
          PGR=TGR-1
          TGRFLG=1
          GRIFLG=0
!     PSF WRITE AND PLOT ON, PLOT ROTATION FALSE
          PSFPLOT=.TRUE.
          PSFWRITE=.TRUE.
          PSFBIN=.FALSE.
          ROTPSF=.FALSE.
!     STREAKED PSFS WRITE AND PLOT ON
          STKPLT=.TRUE.
          STKWRT=.TRUE.
!
!     PIXEL DEFAULT VALUES
          PIXAR1=3
          PIXAR2=3
!
!     DEFAULT VALUES FOR SPOT DIAGRAMS
          NRECT=10
          RINGTOT=4
          RNUMBR=200
!     RING GRID IS DEFAULT
          SPDTYPE=2
!
          RINGRAD(0)=0.0D0
          RINGRAD(1)=0.4D0
          RINGRAD(2)=0.707D0
          RINGRAD(3)=0.866D0
          RINGRAD(4)=1.0D0
          RINGPNT(0)=1
          RINGPNT(1)=8
          RINGPNT(2)=8
          RINGPNT(3)=8
          RINGPNT(4)=8
          RINGANG(0)=0.0D0
          RINGANG(1)=0.0D0
          RINGANG(2)=0.0D0
          RINGANG(3)=0.0D0
          RINGANG(4)=0.0D0
!     DEFAULT VALUES FOR SPOT DIAGRAMS FOR OPTIM
          OPNRECT=10
          OPRINGTOT=4
          OPRNUMBR=200
!     RING GRID
          OPSPDTYPE=2
          OPRINGRAD(0)=0.0D0
          OPRINGRAD(1)=0.4D0
          OPRINGRAD(2)=0.7D0
          OPRINGRAD(3)=0.866D0
          OPRINGRAD(4)=1.0D0
          OPRINGPNT(0)=1
          OPRINGPNT(1)=8
          OPRINGPNT(2)=8
          OPRINGPNT(3)=8
          OPRINGPNT(4)=8
          OPRINGANG(0)=0.0D0
          OPRINGANG(1)=0.0D0
          OPRINGANG(2)=0.0D0
          OPRINGANG(3)=0.0D0
          OPRINGANG(4)=0.0D0

!     SET DEFAULT FORMAT FOR "WRITE" TO G23.15
          WFORM='(G23.15)'

!     SET STATS TO FULL FOR SPOT DIAGRAMS
          STATSP=.TRUE.

!       RE-INITIALIZE OPERATING CONDITIONS
          WCOLD=WC
          WC='OPCON'
          CALL PM
          WC=WCOLD

!     INITIALIZE NON-LENS GLASS WAVELENGTH
          GLSWV(1)=0.58756D0
          GLSWV(2)=0.48613D0
          GLSWV(3)=0.65627D0
          GLSWV(4)=0.43584D0
          GLSWV(5)=0.70652D0

!       INITIALIZE CHANGE VECTOR LENGTH
          LCVLCV=0.0D0

!       SET OPTMES TO TRUE, DON'T BLOCK MESSAGES AS IN OPTIM.
          OPTMES=.TRUE.

!     SET THE GLASS LIBRARY DIRECTORY NAME
          LIBGLA=trim(trim(HOME)//'LIBGLA/')

!     INITIALIZE ALL COLORS TO THEIR DEFAULTS
          COLDEF=15
          COLRAY=15
          COLCLP=3
          COLCOB=9
          COLEDG=1
          COLPRO=1
          COLAXS=15
          COLBAC=0

          COLR1=15
          COLR2=12
          COLR3=2
          COLR4=3
          COLR5=4
          COLR6=5
          COLR7=6
          COLR8=7
          COLR9=8
          COLR10=9

          COLFRM=15
          COLLBL=15
          COLSPE=15
          COLPEN=15

          N=11

          BLANK=AA//AA//AA//AA
          BL140=AA//AA//AA//AA//AA//AA//AA

!     INITIALIZE FIGURE TITLE
          FIGTITLE=BLANK

!     INITIALIZE DOSKEY AND DOSCNT
          DOSKEY(1:100)=BL140
          DOSCNT(1:100)=0

!     SET AUTOMATIC EXIT PUPIL LOCATION CALCULATION TO ON
          EXPAUT=.TRUE.

!     SET DEFAULT RSPH TO CHIEF
          REFLOC=1

!     INITIALIZE ALTERNAMTE MACRO FUNCTION NAMES
          FNAMED(1)='FUN01'
          FNAMED(2)='FUN02'
          FNAMED(3)='FUN03'
          FNAMED(4)='FUN04'
          FNAMED(5)='FUN05'
          FNAMED(6)='FUN06'
          FNAMED(7)='FUN07'
          FNAMED(8)='FUN08'
          FNAMED(9)='FUN09'
          FNAMED(10)='FUN10'
          DRWNAM='NEUTRAL.DAT'

!      Library directories
          LIBLEN=trim(trim(HOME)//'LIBLEN/')
          LIBMAC=trim(trim(HOME)//'LIBMAC/')
          LIBTRA=trim(trim(HOME)//'LIBTRA/')
          LIBSPO=trim(trim(HOME)//'LIBSPO/')
          LIBAUT=trim(trim(HOME)//'LIBAUT/')
          LIBPLO=trim(trim(HOME)//'LIBPLO/')
          NSSDIR=trim(trim(HOME)//'NSSDIR/')

!       SET DEFAULTS FOR "GLOBAL" RAYTRACE OPTION
          GLOBE=.FALSE.
          GLSURF=1
          OFFX=0.0D0
          OFFY=0.0D0
          OFFZ=0.0D0
          OFFA=0.0D0
          OFFB=0.0D0
          OFFC=0.0D0

!       CALCULATION OF GLOBAL RAY PLOTTING DATA IS "OFF"
          PLTRAY=.FALSE.

!       SET FLAG FOR FUNCTIONS TO FALSE
          FUNEXT(1:10)=.FALSE.

!       THE UNIT NUMBERS USED FOR FILE ACCESS ARE:
!       5,6,7,8,9,10,11,12,13,16,17,18,19
!       20,21,22,23,24,25,26,27,28,29
!       30,31,32,33,34,35,36,37,38,39
!       40,41,42 USED BY WINTERACTER
!       43
!       44 IS FOR INPUT FROM BATCH FILE
!       45 AND 46 ARE FOR THE GRIDAPD AND GRID OPD FILES CREATED BY
!       47,49
!       48 FOR NSS RAY HISTORY FILE AND ALSO FOR RAYHIST.DAT FOR SEQUENTIAL RAY HISTORIES
!       THE CAPGRID COMMAND (4/28/99)
!       50,51,52,53,54,55,56,57,58,59
!       60,61,62,63,64,65,66,69,
!       67 IS USED FOR USER DEFINED CLEAR APERTURES (9/27/2004)
!       68 FOR MACNEW.DAT
!       70,71,72,73,75,76,77,78,79
!       80 AND 81 ARE 2 OUTPUT FILES DURING GUI OPERATIONS
!       80 IS APPEND AND HOLDS ALL TEXT OUTPUT SINCE SESSION START
!       81 IS REPLACE AND HOLDS LAST OUTPUT
!       82 USED FOR MEM FILE IN NSS
!       83 USED FOR DELETING THE ENTERPRISE PROFESSIONAL FILE
!       84 USED FOR LENO.CSV
!       85,86,87,88 USED FOR FSMEDIT OPERATION
!       89 USED FOR LENSTEXT.DAT
!       90 USED FOR SAGFILE PLOTTING
!       91 USED FOR USER READABLE NSS RAY HISTORY CODE
!       92 USED FOR NSSRAY DATA FILE FOR NSS RAY PLOTTING PLTNSSRY.DAT
!       96 FOR 'QUIET.DAT' INPUT FILE DURING GUI INTERFACE OPERATIONS
!       97 FOR THE GENERAL FILENAME SPECIFIED FOR OUTPUT AND FOR NSS DATABASE STORAGE
!       OR INPUT
!       98 FOR THE NUL FILE
!       99 FOR FILES IN FINDGLASS OPERATION
!       32 FOR THE SPOT DIAGRAM STORAGE TO DISK
!       100 USED FOR NSSSPOT DIAGRAM FILE
!       101 USED FOR THE INTEN.DAT FILE
!       102 USED FOR THE SCENE.DAT FILE
!       103 USED FOR THE RHFOOT.DAT FILE
!       104 USED FOR MTRACEI OUTPUT FILE
!       105 USED FOR MTRACEI BINARY IRRADIANCE FILE
!       106 USED FOR PSFBIN 1/18/2005
!       110 FONTSAVE TO SAVE FONT SIZE
!
!     SAVE AND RELOAD INPUT LEVELS USED 1 - 14
!
!       STANDARD FOR WARNING MESSAGES IS:
!       FIRST QUALIFIER
!       THEN  STRING
!       THEN  NUMERIC INPUT
!
!       SET MAXIMUMS FOR PROGRAM, PASSED AS VARAIBLES
!       IN THE DAT***.FOR ROUTINES IN INCLUDES
          PII=3.14159265358979323846D0
          TWOPII=2.0D0*PII
          HALFPII=PII/2.0D0
          RPII=SNGL(PII)
          RTWOPII=SNGL(TWOPII)
          RHALFPII=SNGL(HALFPII)
!       MAX NUMBER OF CONFIGS
!       MAXIMUM NUMBER OF SURFACES = MAXSUR
!       MAXIMUM NUMBER OF MACRO LINES
!       MAXIMUM NUMBER OF MACROS
!
          MAXLIN=1024
          MAXSUR=499
          MAXMAC=999
          MAXCFG=75

          ALLOCATE(NUMHITS(0:MAXSUR),STAT=ALLOERR)
          NUMHITS(0:MAXSUR)=1
          
!       REMEMBER TO SET THE ABOUT BOX AND THE PROGRAM NAME MESSAGE CORRECTLY
!       IN GUICODE.FOR AND HARDWAR3.FOR

!       MAXIMUM NUMBER OF FITTED DATA PAIRS
          MAXSPS=2000
          
!       MAXIMUM NUMBER OF VARIABLES IN VARIABLE SUBFILE SET IN PM
!       MAXIMUM NUMBER OF TOLERANCE VARIABLES SET IN PM
!       MAXIMUM NUMBER OF TOLERANCE COMPENSATORS
          MAXCMP=10
!       MAXIMUM NUMBER OF TOLERANCE OPERANDS
          MAXTOP=5
!       MAXIMUM NUMBER OF FOCUS CRITERIA OPERANDS
          MAXFOCRIT=5
!       MAX GRID SIZE FOR SPOT DIAGRAMS IS MAXGRD x MAXGRD
          MAXGRD=128
!       MAX SYSTEM ARRAY SIZE (MUST BE A MULTIPLE OF 5)
          SSIZ=150
!       MAX LENS ARRAY SIZE (MUST BE A MULTIPLE OF 5)
          LSIZ=160
!       MAX PIKUP ARRAY SIZE (MUST BE A MULTIPLE OF 5)
          PSIZ=45
!       MAX REAL RAY ARRAY SIZE
          RAYSIZ=50
!       MAX REAL RAY DIFF ARRAY SIZE
          DIFSIZ=18
!       SET LDIF AND LDIF2 TO TRUE
          LDIF=.TRUE.
          LDIF2=.TRUE.
          OPDIF=.TRUE.
!       SET LVIG TO TRUE
          LVIG=.TRUE.
!       SET NOVIRT TO TRUE
          NOVIRT=.TRUE.
!       SET ANAAIM TO .TRUE. (ONLY FALSE IN CAPFN TRACING)
          ANAAIM=.TRUE.
          
!     PFIND DEFAULT VALUES
          PFDELM=0.6D0
          PFDELA=0.01D0
          MAXFAIL=2
          PFNUM=10

!       RE-INITIALIZE OPERATING CONDITIONS
          WCOLD=WC
          WC='OPCON'
          CALL PM
          WC=WCOLD

!       SET HEADIN TO FALSE FOR ACCOS LIKE PRINOUT OF SINGLE
!       LINES OF DATA SUCH AS RTG,4
          HEADIN=.FALSE.
!       VARIABLES INITIALIZATION
          VBCNT=0
!       MERIT (OPERAND) INITIALIZATION
          OPCNT=0
          TOPCNT=0
          FCCNT=0
          CMPCNT=0
          FMTEXT=.FALSE.
!       TOLERANCE VARIABLES INITIALIZATION
          TVBCNT=0
!       FMT INITIALIZATION
          FMTFMT=0.0D0
          FMTFLG=.FALSE.
!     DERIVATIVE MATRIX INITIALIZATION
          DEREXT=.FALSE.
          ITERROR=.FALSE.
          CALL ITER(0,0,ITERROR)
          SOLEXT=.FALSE.
          CFCH=.FALSE.
          DERSIZ=0

!       SET DEFAULT ANGMOD TO DEGREES FOR ALL ANGULAR
!       RAYTRACE OUTPUT
          DEG=.TRUE.
          RAD=.FALSE.
          TANG=.FALSE.

!       THIS PROGRAM CONSISTS OF A CMD OR MAIN LEVEL AND
!       A NUMBER OF SUB- LEVELS.
!
!       FLAG F1=1 REPRESENTS THE CMD LEVEL OF THE PROGRAM
!       FLAG F1=0 REPRESENTS NOT THE CMD LEVEL.
!
!       FLAG F2=1 REPRESENTS NEW MACRO CREATION
!       FLAG F2=0 REPRESENTS NOT NEW MACRO CREATION
!
!       (OTHER FLAG STATUS COMMENTS WILL GO HERE AS THE
!       PROGRAM IS DEVELOPED)
!
!       INITIALIZE THE RAY-DIRECTION COSINE TRACKING FLAGS
!       TO +Z DIRECTION, OTHERWISE THE RAYTRACE GIVES INCORRECT
!       RESULTS.
!
!       START THE PROGRAM ALWAYS AT CMD LEVEL. SET FLAG F1
          F1=1

!       AND FLAG F2 (MACRO CREATION) IS SET TO 0
          F2=0

!       FLAG F3 IS INITIALLY SET TO 0. WHEN SET TO 1
!       THE MACRO EDIT OR MEDIT LEVEL OF THE PROGRAM
!       IS IN EFFECT. ONLY THE FL COMMAND CAN RESET
!       FLAG F3 TO ZERO.
          F3=0

!       FLAG F4 WHEN SET MEANS THAT A MACRO IS BEING EXECUTED.
!       THE INITIAL DEFAULT SETTING FOR F4 IS F4 = 0
          F4=0

!       THE SPECIAL TRACE FLAG TF(NEST) TRACKS STATUS
!       OF TRACE ON OR OFF IN MACRO EXECUTION AT VARIOUS
!       NESTING LEVELS.
!       TF(NEST)=1 IS TRACE ON, TF(NEST)=0 IS TRACE OFF
!       DEFAULT IS TF(NEST)=0
          TF(0:20)=0

!       FLAG F5 TRACKS LENS INPUT MODE WHICH IS INITIALIZED
!       BY THE 'LENS' COMMAND AND TERMINATED WITH EOS (END OF SUBFILE).
!       THE DEFAULT VALUE OF F5 IS 0. DURING LENS INPUT F5=1 AND F1=0.
          F5=0

!       FLAG F6 TRACKS LENS UPDATES INITIALIZED BY THE 'UPDATE'
!       COMMAND WITH THE 'LENS' QUALIFIER WORD. THE DEFAULT VALUE OF
!       F6=0. DURING LENS UPDATING F6=0 AND F1=0.
          F6=0

!       FLAG F7 TRACKS SPSRF (SPECIAL SURFACE) INPUT MODE
!       INITIATED BY THE COMMAND (SPSRF). DEFAULT IS F7=0
          F7=0

!       FLAG F8 TRACKS UPDATE SPSRF (SPECIAL SURFACE)
!       UPDATE MODE BY THE COMMAND (UPDATE SPSRF) DEFAULT VALUE
!       IS F8=0
          F8=0

!       FLAG F9 TRACKS THE SPFIT LEVEL ENTERED BY (SPFIT)
!       DEFAULT VALUE IS F9=0
          F9=0

!       FLAG F10 TRACKS THE CONFIGS INPUT LEVEL ENTERED BY COMMAND
!       (CONFIGS) ITS DEFAULT VALUE IS 0
          F10=0

!       FLAG F11 TRACKS THE CONFIGS UPDATE LEVEL ENTERED BY COMMAND
!       (UPDATE CONFIGS) ITS DEFAULT VALUE IS 0
          F11=0

!       FLAG F12 TRACKS THE CURRENT CONFIGURATION NUMBER
!       THE STARTING AND DEFAULT VALUE IS 1
          F12=1

!       FLAGS F13 AND F14 TRACK THE USAGE OF UPDATE LENS
!       AND UPDATE SPSRF FROM WITHIN CONFIGS OR UPDATE
!       CONFIGS AND ARE USED BY QUERRY TO PRINT CORRECT
!       RESPONSE TO A ? AT THE CONFIGS/UPDATE CONFIGS LEVEL
!       DEFAULT VALUES ARE BOTH 0
          F13=0
          F14=0

!       FLAG F15 IS SET TO 1 WHEN THE PROCESS OF ACTIVATING
!       A CONFIGURATION IS BEING PERFORMED. OTHERWISE
!       ITS DEFAULT VALUE IS 0
          F15=0

!       FLAG F16 TRACKS WHETHER OR NOT A MACRO OF THE SAME NAME AS
!       AN EXISTING MACRO IS TRYING TO BE CREATED BY THE "MACRO"
!       COMMAND. THE DEFAULT VALUE ID F16=0. IF A MACRO OF THE SAME
!       NAME DOES EXIST, F16 IS SET TO 1
          F16=0

!       FLAG F17 TRACKS THE SPECT OR SPECTRAL ANALYSIS
!       LEVEL OF THE PROGRAM. IF F17=1 THEN F1 IS = 1 NOT 0.
!       SPECT HAS AVAILABLE, MANY CMD LEVEL COMMANDS.
!       AND WE ARE AT THE SPECT SUB-LEVEL OF THE PROGRAM.
!       THE DEFAULT IS F17=0
          F17=0

!       FLAG F18 TRACKS THE TABLE INPUT LEVEL WITHIN THE SPECT
!       LEVEL OF THE PROGRAM. IF F18=1 AND F17=1 THEN
!       AND WE ARE AT THE TABLE SUB-LEVEL OF SPECT.
!       THE DEFAULT IS F18=0
          F18=0

!       FLAG F19 TRACKS THE EXISTENCE OF TABLE DATA.
!       THE DEFAULT IS F19=0 FOR NO DATA.
          F19=0

!       FLAG F20 IS AVAILABLE FOR USE

!       FLAG F21 IS USED TO TELL THE PROGRAM WHEN
!       REFRACTIVE INDICES SHOULD BE MADE NEGATIVE.
!       EACH TIME A REFECTIVE SURFACE IS ENCOUNTERED, THE
!       VALUE OF FLAG F21 IS REVERSED. THE DEFAULT VALUE IS
!       F21=0
          F21=0

!       AT LENS EOS IN SUBROUTINE LNSEOS, ALL RESOLUTION OF
!       SIGN OF REFRACTIVE INDICES IS HANDLED AS ONE OF THE
!       FIRST DUTIES IN THAT SUBROUTINE.
!
!       F22
!       IF F22=0, THE GLASS CATALOG IS NOT REFERENCED. IF
!       F22=1, A CATALOG LOOKUP IS PERFORMED.
!       F22 IS ALWAYS SET TO 1 DURING LENS INPUT. AFTER
!       LENS INPUT OR LENS UPDATE OR CONFIG CHANGE, F22=0
!       THE STARTING DEFAULT IS F22=0
!       DURING LENS UPDATE, THE FOLLOWING OPERATIONS
!       SET F22 TO 1
!                       ANY CHANGE OF MATERIAL
!                       ANY SUFACE DELETION OR INSERTION
!                       ANY CHANGE TO THE REFRACTIVE INDICES
!                       ANY CHANGE TO THE WAVELENGTHS
!       LIB GET ALSO SETS F22=1
          F22=0

!       F23 TRACKS IF TYPE OR SURF WAS ENTERED IN THE
!       SPFIT ROUTINES. IF TYPE IS SET, F23=1,
!       IF SURF IS SET, F23=2
!       DEFAULT IS NOT F23=0
          F23=0

!       F24 TRACKS THE PRESENTS OF DATA TO BE FITTED, THAT IS
!       DATA ACCUMULATED WITH THE PROER COEFFS AND FUNCTIONAL FORM
!       NO DATA IS F24=0, DATA = F24=1
          F24=0

!       F25 IS SET TO 1 IF FITTED DATA EXISTS TO BE EVALUATED.
!       IF A FIT IS NOT PERFORMED TO THE POINT THAT COEFS ARE
!       AVAILABLE FOR EVALUATION, F24=0 (DEFAULT)
          F25=0

!       FLAG F26 IS 1 WHEN A MACRO FUNCTION IS RUNNING
          F26=0

!       FLAG F27 TRACKS IF THE PROGRAM IS IN THE MERIT LEVEL
!       AND THE UPDATE MERIT (U M) LEVEL
!       IF F27=0, NOT
!       IF F27=1, MERIT CREATION LEVEL
!       IF F27=2, UPDATE MERIT LEVEL
          F27=0

!       FLAG F28 TRACKS THE PROGRAM OPERATION DURING OPTIMIZATION
!       CALCULATIONS
!       IF F28=0, NOT
!       IF F28=1, OPTIMIZATION CALCULATIONS IN PROGRESS
          F28=0

!       F35 IS LIKE F28 BUT ONLY USED WITH INTERNAL CALLS TO ITER
!       FLAG F35 TRACKS THE PROGRAM OPERATION DURING OPTIMIZATION
!       CALCULATIONS
!       IF F35=0, NOT
!       IF F35=1, OPTIMIZATION CALCULATIONS IN PROGRESS
          F35=0

!       FLAG F29 TRACKS THE VARIABLES LEVEL
!       AND THE UPDATE VARIABLES LEVEL
!     IF F29=0 ; NOT
!     IF F29=1 ; VARIABLES
!     IF F29=2 ; UPDATE VARIABLES
          F29=0

!       FLAG F30 NOT USED
          F30=0

!       FLAG F31 TRACKS THE PROGRAM OPERATION DURING TOLERANCE
!       CALCULATIONS
!       IF F31=0, NOT
!       IF F31=1, TOLERANCING CALCULATIONS IN PROGRESS
          F31=0

!     FLAG F32 TRACKS THE LENSDRAWING INPUT LEVEL
!
!       FLAG 46 IS USED TO TRACK A BLANK MACRO
!
!       FLAG F47 IS USED TO FACILITATE CALLING SUBROUTINE
!       MREA DURING A REPLACEMENT OF A MACRO LINE DURING
!       THE MEDIT PROCESS.
!
!       FLAGS F45,F48 AND F49 ARE USED IN ITF,ILF AND IMF PROCEED
!       PROCEEDURES. THE MAKE ITF,IMF AND ILF WORK CORRECTLY.
!       INITIAL VALUES ARE ZERO. FLAG F33 IS USED FOR IPF (PLOT FILE
!       INITIALIZATION)
          F33=0
          F45=0
          F48=0
          F49=0

!     F34=1 IF VIE PLOTTING IS BEING DONE
          F34=0

!       FLAG F50 TRACKS STATUS TO DETERMINE IF A MESSAGE
!       RESPONSE IS REQUIRED FOR BLANK LINE INPUT.
!       DEFAULT IF F50=0
          F50=0

!     FLAG F51 TRACKS TOLERANCE VARIABLE INPUT AND UPDATE (0,1 OR 2)
!     FLAG F52 TRACKS TOLERANCE COMPENSATOR INPUT AND UPDATE (0,1 OR 2)
!     FLAG F53 TRACKS TOLERANCE OPERAND INPUT AND UPDATE (0,1 OR 2)
!     FLAG F54 TRACKS TOLERANCE FOCRIT INPUT AND UPDATE (0,1 OR 2)
!     FLAG F55 TRACKS OPERATION OF THE TOLERANCE ANALYSIS
!       F55=0 MEANS NONE BEING DONE
!       F55=1 MEANS SENSI
!       F55=2 MEANS INVERSE SENSI
!       F55=3 MEANS MONTE-CARLO
          F51=0
          F52=0
          F53=0
          F54=0
          F55=0

!     FLAGS 51 TO 100 ADDED ON 9/5/92 FOR FUTURE EXPANSION AND SET TO
!     ZERO HERE
!     F56 IS 0 IF DEFORMABLE SURFACE DATA HAS NOT BEEN READ FROM DISK
!     AND ID 1 IF IT ALREADY HAS BEEN
          F56=0

!     FLAG F57 IS 0 IF NO AUTOFUNC FUNCTION IS TO BE EXECUTED, IT IS 1
!     IF THERE IS AN UNRESOLVED AUTOFUNC FUNCTION
          F57=0

!     FLAG F58 IS 1 IF FANS ARE BEING GENERATED, ELSE F58=0
          F58=0

!     Remainder is unused
          F59=0
          F60=0
          F61=0
          F62=0
          F63=0
          F64=0
          F65=0
          F66=0
          F67=0
          F68=0
          F69=0
          F70=0
          F71=0
          F72=0
          F73=0
          F74=0
          F75=0
          F76=0
          F77=0
          F78=0
          F79=0
          F80=0
          F81=0
          F82=0
          F83=0
          F84=0
          F85=0
          F86=0
          F87=0
          F88=0
          F89=0
          F90=0
          F91=0
          F92=0
          F93=0
          F94=0
          F95=0
          F96=0
          F97=0
          F98=0
          F99=0
          F100=0
         
!     PIVAXIS MODE ALWAYS STARTS AS "VERTEX" BECAUSE SYSTEM1(101) ALWAYS
!     STARTS AS 0

!     THE PROGRAM USER FLAGS FLG(0:20) ARE INITIALIZED HERE
          FLG(0)=0
          FLG(1:20)=-1

!       INITIALIZE BOTTOM LINE OF FILE UNIT 13
          LINBOT=0
          
!       INITIALIZE CURRENT LINE IN FILE UNIT 13
          LINCUR=0

!       SET REFEXT TO NO INDICATING THAT NO REFERENCE RAY DATA EXISTS
!       AND NULL IN FOB IS .FALSE.
!
          FOBYES=.FALSE.
          REFEXT=.FALSE.
          SPDEXT=.FALSE.
          GSPDEXT=.FALSE.
          CPFNEXT=.FALSE.
          CALL DELPSF
          NULL=.FALSE.
          RAYEXT=.FALSE.
          POLEXT=.FALSE.
          FAIL=.TRUE.

!***********************************************************************
!
!       INITIALIZE TIMER
          CALL SETTIM
!
!***********************************************************************
!       INITIALIZE MACRO FUNCTIONS
          EXISMC=.FALSE.
          INQUIRE(FILE=trim(LIBMAC)//'MAC.DAT',EXIST=EXISMC)

          IF(EXISMC) THEN
!       LOAD FUNCTIONS
              OPEN(UNIT=20,ACCESS='DIRECT',FILE=trim(LIBMAC)//'MAC.DAT',FORM=
     1        'UNFORMATTED',RECL=(40*NRECL),STATUS='UNKNOWN')

              DO I=1,MAXMAC
                  READ(UNIT=20,REC=I,ERR=1012) MCDIR1(I),MCDIR2(1,I),MCDIR2(2,I),
     1            MCDIR2(3,I),MCDIR3(I)

                  KLI=0
                  IF(MCDIR1(I).EQ.'FUN01   ') KLI=1
                  IF(MCDIR1(I).EQ.'FUN02   ') KLI=2
                  IF(MCDIR1(I).EQ.'FUN03   ') KLI=3
                  IF(MCDIR1(I).EQ.'FUN04   ') KLI=4
                  IF(MCDIR1(I).EQ.'FUN05   ') KLI=5
                  IF(MCDIR1(I).EQ.'FUN06   ') KLI=6
                  IF(MCDIR1(I).EQ.'FUN07   ') KLI=7
                  IF(MCDIR1(I).EQ.'FUN08   ') KLI=8
                  IF(MCDIR1(I).EQ.'FUN09   ') KLI=9
                  IF(MCDIR1(I).EQ.'FUN10   ') KLI=10

                  IF(KLI.NE.0) THEN
                      FUNEXT(KLI)=.TRUE.
                      FCDIR1(KLI)=MCDIR1(I)
                      FCDIR2(1,KLI)=MCDIR2(1,I)
                      FCDIR2(2,KLI)=MCDIR2(2,I)
                      FCDIR2(3,KLI)=MCDIR2(3,I)
!       LOAD MACRO FUNCTIONS INTO THE FUNCTION STORAGE
!
!       NOW READ THE BODY OF THE MACRO STARTING AT RECORD 1 FROM FILE
!       FILNAM AFTER DETERMINING FILNAM
                      NF=I

                      CALL MACFIL
                      OPEN(UNIT=30,ACCESS='DIRECT',FILE=trim(LIBMAC)//FILNAM,FORM=
     1                'UNFORMATTED',RECL=(55*NRECL),STATUS='UNKNOWN')
                      DO K=1,(MCDIR2(2,NF))
                          L=K-1
                          READ(UNIT=30,REC=K)
     1                    MACCW(L),MACQW(L),MACSTR(L),MACNW(1,L),
     1                    MACNW(2,L),MACNW(3,L),MACNW(4,L),MACNW(5,L),MACSTA(1,L),
     2                    MACSTA(2,L),MACSTA(3,L),MACSTA(4,L),MACSTA(5,L),MACSTA(6,L),
     3                    MACSTA(7,L),MACSTA(8,L),MACSTA(9,L),MACSTA(10,L),MACSTA(11,L),
     4                    MACSTA(12,L),MACSTA(13,L),MACSTA(14,L),MACSTA(15,L),MACSTA(16,L)
     5                    ,MACSTA(17,L),MACSTA(18,L),MACSTA(19,L),MACSTA(20,L)

                      END DO
! 123    CALL CLOSE_FILE(30,1)
                      DO K=1,(FCDIR2(2,KLI))
                          L=K-1
                          FUNCW(KLI,L)=MACCW(L)
                          FUNQW(KLI,L)=MACQW(L)
                          FUNSTR(KLI,L)=MACSTR(L)
                          FUNNW(KLI,1,L)=MACNW(1,L)
                          FUNNW(KLI,2,L)=MACNW(2,L)
                          FUNNW(KLI,3,L)=MACNW(3,L)
                          FUNNW(KLI,4,L)=MACNW(4,L)
                          FUNNW(KLI,5,L)=MACNW(5,L)
                          FUNSTA(KLI,1,L)=MACSTA(1,L)
                          FUNSTA(KLI,2,L)=MACSTA(2,L)
                          FUNSTA(KLI,3,L)=MACSTA(3,L)
                          FUNSTA(KLI,4,L)=MACSTA(4,L)
                          FUNSTA(KLI,5,L)=MACSTA(5,L)
                          FUNSTA(KLI,6,L)=MACSTA(6,L)
                          FUNSTA(KLI,7,L)=MACSTA(7,L)
                          FUNSTA(KLI,8,L)=MACSTA(8,L)
                          FUNSTA(KLI,9,L)=MACSTA(9,L)
                          FUNSTA(KLI,10,L)=MACSTA(10,L)
                          FUNSTA(KLI,11,L)=MACSTA(11,L)
                          FUNSTA(KLI,12,L)=MACSTA(12,L)
                          FUNSTA(KLI,13,L)=MACSTA(13,L)
                          FUNSTA(KLI,14,L)=MACSTA(14,L)
                          FUNSTA(KLI,15,L)=MACSTA(15,L)
                          FUNSTA(KLI,16,L)=MACSTA(16,L)
                          FUNSTA(KLI,17,L)=MACSTA(17,L)
                          FUNSTA(KLI,18,L)=MACSTA(18,L)
                          FUNSTA(KLI,19,L)=MACSTA(19,L)
                          FUNSTA(KLI,20,L)=MACSTA(20,L)
                      END DO
!       FUNCTION LOADED
!       CHECK FOR NEXT MACRO NAME
                  END IF
 1012             CONTINUE
              END DO
!
!       CLOSE UNIT 20 TO I/O
!
              CALL CLOSE_FILE(20,1)
!       PROCEDD WITHOUT ATTEMPED FUNCTION LOADING
          END IF
!
!***********************************************************************
!
!                       PLOT INITIALIZATION
!     IS PERFORMED BT .FOR WHEN ENTERING THE GRAPHIC MODE
!     VIA THE PLOT NEW COMMAND
!
!       SET PPLI TO BLANK
          PPLI(1:80)=BLANK(1:80)
!
!**********************************************************************
!               INPUT IS ALWAYS AN 140 CHARACTER,CHARACTER
!               VARIABLE.
!
!               THE STATEMENTS WHICH FOLLOW UP TO BUT NOT
!               INCLUDING STATEMENT 1 ARE EXECUTED ONLY ONCE
!               AT PROGRAM STARTUP. THEY INITIALIZE PROGRAM VARIABLES.
!
!               HERE IS WHERE ALL POSSIBLE VALID PROGRAM COMMAND WORDS
!               ARE INITIALIZED AND STORED IN ARRAY WCC WHICH IS PASSED
!               VIA COMWDS COMMON. THIS IS DONE IN SUBROUTINE 'NAMES'
!
          CALL NAMES

          IN  = 5
          OUT = 6
          ECH = 0
!       INITIALIZE MEMORY REGISTERS TO ZERO (REG)

          REG(1:50)=0.0D0
!       INITIALIZE THE GENERAL PURPOSE CHARACTER REGISTERS TO ZERO
          AGPREG(1:100000)=' '
          GPREG(1:100000)=0.0D0

!       BEFORE THE MAIN READ STATEMENT, CHECK IF
!       A DEFAULTS.DAT FILE EXISTS. IF SO, READ AND PROCESS THE
!       INSTRUCTIONS THERE.


          OPEN(UNIT=16,ACCESS='SEQUENTIAL',
     1    BLANK='NULL',FORM='FORMATTED',FILE=trim(HOME)//'DEFAULTS.DAT',
     1    STATUS='UNKNOWN')
          DO I=1,99999
              READ(UNIT=16,FMT=100,END=9887,ERR=9877) INPUT(1:140)
              MULTICOM=.TRUE.
              CALL PROCES
              MULTICOM=.FALSE.
          END DO

 9887     CALL CLOSE_FILE(16,1)
          INPUT='FIELDS RESET'
          MULTICOM=.TRUE.
          CALL PROCES
          MULTICOM=.FALSE.
          INPUT='RAYS RESET'
          MULTICOM=.TRUE.
          CALL PROCES
          MULTICOM=.FALSE.
!
!     INITIALIZE OPERAND VALUES AND DESCRIPTORS
          I=MAXOPT
          OPERND(1:I,1)=0.0D0
          OPERDESC(1:I)(1:80)=' '
          OPERND(1:I,1:20)=0.0D0
!
!       SET THE COUNTER TO THE TOP OF THE MERIT ARRAY STRUCTURE.
          OPCNT=0
          FCCNT=0
          TOPCNT=0
          FMTEXT=.FALSE.
!
          CORMOD=1
!       INITIALIZE THE CURRENT CONFIGURATION NUMBER TO 1
          CURFIG=1
!
!               IN AND OUT ARE THE DEFAULT READ/WRITE UNIT
!               NUMBERS.
!*******************************************************************
!       LOAD INITIAL LENS DATA
          EXIS89=.FALSE.
          OLDIN=IN
          IN=89
          OPTMES=.FALSE.
          EXIS89=.FALSE.
          INQUIRE(FILE=trim(HOME)//'CURLENS/LENSTEXT.DAT',
     1    EXIST=EXIS89)
          IF(EXIS89) THEN
!       LENSTEXT EXISTS, READ IT
!
!               THIS IS THE LENSTEXT.DAT INPUT SECTION
!               THAT IS USED TO RESTORE THE LAST CURRENT LENS
!               FROM UNIT 89
!
              OPEN(UNIT=89,ACCESS='SEQUENTIAL',BLANK='NULL'
     1        ,FORM='FORMATTED',FILE=trim(HOME)//'CURLENS/LENSTEXT.DAT'
     2        ,STATUS='UNKNOWN')
              REWIND(UNIT=89)
 3141         READ(UNIT=89,FMT=100,END=8888,ERR=8887) INPUT(1:140)
              IF(INPUT(1:3).EQ.'OUT') GO TO 3141
              IF(INPUT(1:1).EQ.' ') INPUT(1:140)=INPUT(2:140)//' '
              IF(INPUT(1:1).EQ.' ') INPUT(1:140)=INPUT(2:140)//' '
              IF(INPUT(1:1).EQ.' ') INPUT(1:140)=INPUT(2:140)//' '
              IF(INPUT(1:1).EQ.' ') INPUT(1:140)=INPUT(2:140)//' '
              IF(INPUT(1:1).EQ.' ') INPUT(1:140)=INPUT(2:140)//' '
              IF(INPUT(1:1).EQ.' ') INPUT(1:140)=INPUT(2:140)//' '
              IF(INPUT(1:1).EQ.' ') INPUT(1:140)=INPUT(2:140)//' '
              IF(INPUT(1:8).EQ.'FLDSRAYS') THEN
                  IREND=500
                  IF(INPUT(10:13).EQ.' 500') IREND=500
                  IF(INPUT(10:13).EQ.'1000') IREND=1000
                  IF(INPUT(10:13).EQ.'1500') IREND=1500
                  IF(INPUT(10:13).EQ.'2000') IREND=2000
                  IF(INPUT(10:13).EQ.'2500') IREND=2500
                  IF(INPUT(10:13).EQ.'3000') IREND=3000
                  IF(INPUT(10:13).EQ.'3500') IREND=3500
                  IF(INPUT(10:13).EQ.'4000') IREND=4000
                  IF(INPUT(10:13).EQ.'4500') IREND=4500
                  IF(INPUT(10:13).EQ.'5000') IREND=5000
                  DO I=1,200
                      READ(89,*) AI,FIELDY(I),FIELDX(I),
     1                FIELDZ(I),N3
                      FIELDW(I)=DBLE(N3)
                      IF(FIELDW(I).EQ.0.0D0) THEN
                          FIELDW(I)=SYSTEM1(11)
                      END IF
                  END DO
                  DO I=1,IREND
                      READ(89,*,ERR=8887,END=8887) AI4,RAYY(I),RAYX(I),N3
                      RAYW(I)=DBLE(N3)
                      IF(RAYW(I).EQ.0.0D0) THEN
                          IF(I.GE.1.AND.I.LE.41) RAYW(I)=SYSTEM1(11)
                          IF(I.GE.42.AND.I.LE.82) RAYW(I)=SYSTEM1(7)
                          IF(I.GE.83.AND.I.LE.123) RAYW(I)=SYSTEM1(8)
                      END IF
                  END DO
              ELSE
                  IF(INPUT(1:1).EQ.'.'.OR.
     1            INPUT(1:1).EQ.'1'.OR.
     1            INPUT(1:1).EQ.'2'.OR.
     1            INPUT(1:1).EQ.'3'.OR.
     1            INPUT(1:1).EQ.'4'.OR.
     1            INPUT(1:1).EQ.'5'.OR.
     1            INPUT(1:1).EQ.'6'.OR.
     1            INPUT(1:1).EQ.'7'.OR.
     1            INPUT(1:1).EQ.'8'.OR.
     1            INPUT(1:1).EQ.'9'.OR.
     1            INPUT(1:1).EQ.'0'
     1            .OR.INPUT(1:8).EQ.'FLDSRAYS') THEN
                  ELSE
                      CALL PROCES
                  END IF
              END IF
              GO TO 3141
!     LENSTEXT.DAT HAS BEEN LOADED
 8888         BACKSPACE(UNIT=89)
              REWIND (UNIT=89)
              CALL CLOSE_FILE(89,1)
              GO TO 8889
 8887         BACKSPACE(UNIT=89)
              BACKSPACE(UNIT=89)
              GO TO 3141
 8889         CONTINUE
          ELSE
!     NO LENSTEXT EXISTS,OR AN ERROR EXITSED
          END IF
          IN=OLDIN
          OPTMES=.TRUE.
!
!       SET UP DEFAULT MERIT FUNCTION CONDITIONS
!       DFGRID=1 (HEX) OR =2 FOR (RECT-THE DEFAULT)
          DFGRID=1
          DFDEL=0.385
          DFSEC=8
          DFRIN=2
          DFWT1=1.0D0
          DFWT2=1.0D0
          DFWT3=1.0D0
          DEFAULT_FOB(1,1:25)=0.0D0
          DEFAULT_FOB(2,1:25)=0.0D0
          DEFAULT_FOB(3,1:25)=1.0D0
          DEFAULT_FOB(4,1:25)=SYSTEM1(11)
          DEFAULT_FOB(1,1)=0.0D0
          DEFAULT_FOB(2,1)=0.0D0
          DEFAULT_FOB(3,1)=1.0D0
          DEFAULT_FOB(4,1)=SYSTEM1(11)
          DEFAULT_FOB(1,2)=0.7D0
          DEFAULT_FOB(2,2)=0.0D0
          DEFAULT_FOB(3,2)=1.0D0
          DEFAULT_FOB(4,2)=SYSTEM1(11)
          DEFAULT_FOB(1,3)=1.0D0
          DEFAULT_FOB(2,3)=0.0D0
          DEFAULT_FOB(3,3)=1.0D0
          DEFAULT_FOB(4,3)=SYSTEM1(11)
          DFPNUMB=3
          DFWAVENUMB=INT(SYSTEM1(11))
          DFTYPENUMB=1
          DF_CFG=1
          IN=5

          EXIS7=.FALSE.
          INQUIRE(FILE=trim(HOME)//'PRINTER.TXT',EXIST=EXIS7)
          IF(EXIS7) THEN
              OPEN(UNIT=7,
     1        BLANK='NULL',FORM='FORMATTED',FILE=trim(HOME)//'PRINTER.TXT',
     1        STATUS='UNKNOWN')
              CALL CLOSE_FILE(7,0)
          END IF
          EXIS7=.FALSE.

!
!*******************************************************************
!               THIS IS THE PROGRAM MAIN READ STATEMENT.
!******************************************************************
!
!       THE NEXT FEW STATEMENTS CONTROL THE INPUT LINE
!       AND THE PROGRAM CURSOR. THEY ARE THE ONLY NON-
!       ANSI FORTRAN 77 STATEMENTS IN THE PROGRAN.
!
          CMDNO = 1 ! # of input commands starts at 1

                           !*******************************
 1        OLDOUTOLD = OUT  ! Command input loop starts here
                           !*******************************
!
!     ALL KEYBOARD AND LATER WINFILE INPUT IS DONE WITH A CALL TO
!     userinput
!
!       FIX THE DOGTAG
!
          EXIS22=.FALSE.
          EXIS27=.FALSE.
          INQUIRE(FILE=trim(LIBLEN)//'LIB.DAT',EXIST=EXIS22)
          INQUIRE(FILE=trim(LIBLEN)//'LIBTAG.DAT',EXIST=EXIS27)
          IF(EXIS22) THEN
              OPEN(UNIT=22,ACCESS='DIRECT',FILE=trim(LIBLEN)//'LIB.DAT',
     1        FORM='UNFORMATTED',RECL=(84*NRECL),STATUS='UNKNOWN')
              DO I=1,999
                  READ(UNIT=22,REC=I) II,DATA
              END DO
              CALL CLOSE_FILE(22,1)
              GO TO 667
! 666            CONTINUE
              OPEN(UNIT=22,ACCESS='DIRECT',FILE=trim(LIBLEN)//'LIB.DAT',
     1        FORM='UNFORMATTED',RECL=(84*NRECL),STATUS='UNKNOWN')
              OPEN(UNIT=27,ACCESS='DIRECT',FILE=trim(LIBLEN)//'LIBTAG.DAT',
     1        FORM='UNFORMATTED',RECL=(84*NRECL),STATUS='UNKNOWN')
              CALL CLOSE_FILE(22,0)
              CALL CLOSE_FILE(27,0)
              GO TO 669
 667          CONTINUE
              OPEN(UNIT=22,ACCESS='DIRECT',FILE=trim(LIBLEN)//'LIB.DAT',
     1        FORM='UNFORMATTED',RECL=(84*NRECL),STATUS='UNKNOWN')
              IF(EXIS27) THEN
                  OPEN(UNIT=27,ACCESS='DIRECT',FILE=trim(LIBLEN)//'LIBTAG.DAT',
     1            FORM='UNFORMATTED',RECL=(84*NRECL),STATUS='UNKNOWN'
     2            ,ERR=668)
                  DO I=1,999
                      READ(UNIT=22,REC=I) II,DATA
                      DO J=1,10
                          READ(UNIT=27,REC=I-1+J,ERR=668) IDTAG(J)(1:75)
                      END DO
                  END DO
                  CALL CLOSE_FILE(22,1)
                  CALL CLOSE_FILE(27,1)
                  GO TO 669
 668              CONTINUE
                  OPEN(UNIT=27,ACCESS='DIRECT',FILE=trim(LIBLEN)//'LIBTAG.DAT',
     1            FORM='UNFORMATTED',RECL=(84*NRECL),STATUS='UNKNOWN')
                  CALL CLOSE_FILE(27,0)
              END IF
 669          CONTINUE
          END IF

          IF( batchmode ) THEN
              EXIS44=.FALSE.
              OPEN44=.FALSE.
              INQUIRE(FILE=BATCHFILE, EXIST=EXIS44)
              INQUIRE(FILE=BATCHFILE, OPENED=OPEN44)

              IF (.not.EXIS44) THEN

                 OUTLYNE='BATCH FILE DOES NOT EXIST'
                 CALL SHOWIT(1)
                 SQ=0
                 SST=0
                 SN=0
                 STI=0
                 CALL EXITT(1)
                  
               ELSE

                  IF(OPEN44) CALL CLOSE_FILE(44,1)
!     OPEN AND PROCESS CONTENTS
                  OPEN(UNIT=44,ACCESS='SEQUENTIAL',FILE=trim(HOME)//'BATCH.DAT',
     1            FORM='FORMATTED',STATUS='UNKNOWN')
                  REWIND(UNIT=44)
!     READ AND PROCESS INSTRUCTIONS
                  DO I=1,99999
                      READ(UNIT=44,FMT=100,END=9743,ERR=9744) INPUT(1:140)
                      IF(INPUT(1:3).NE.'OUT') THEN
                          IF(INPUT(1:1).EQ.' ') INPUT(1:140)=INPUT(2:140)//' '
                          IF(INPUT(1:1).EQ.' ') INPUT(1:140)=INPUT(2:140)//' '
                          IF(INPUT(1:1).EQ.' ') INPUT(1:140)=INPUT(2:140)//' '
                          IF(INPUT(1:1).EQ.' ') INPUT(1:140)=INPUT(2:140)//' '
                          IF(INPUT(1:1).EQ.' ') INPUT(1:140)=INPUT(2:140)//' '
                          IF(INPUT(1:1).EQ.' ') INPUT(1:140)=INPUT(2:140)//' '
                          IF(INPUT(1:1).EQ.' ') INPUT(1:140)=INPUT(2:140)//' '
                          IF(INPUT(1:8).EQ.'FLDSRAYS') THEN
                              IREND=500
                              IF(INPUT(10:13).EQ.' 500') IREND=500
                              IF(INPUT(10:13).EQ.'1000') IREND=1000
                              IF(INPUT(10:13).EQ.'1500') IREND=1500
                              IF(INPUT(10:13).EQ.'2000') IREND=2000
                              IF(INPUT(10:13).EQ.'2500') IREND=2500
                              IF(INPUT(10:13).EQ.'3000') IREND=3000
                              IF(INPUT(10:13).EQ.'3500') IREND=3500
                              IF(INPUT(10:13).EQ.'4000') IREND=4000
                              IF(INPUT(10:13).EQ.'4500') IREND=4500
                              IF(INPUT(10:13).EQ.'5000') IREND=5000
                              DO J=1,200
                                  READ(44,*) AI,FIELDY(I),FIELDX(I),
     1                            FIELDZ(I),N3
                                  FIELDW(I)=DBLE(N3)
                                  IF(FIELDW(I).EQ.0.0D0) THEN
                                      FIELDW(I)=SYSTEM1(11)
                                  END IF
                              END DO
                              DO J=1,IREND
                                  READ(44,*,ERR=9678,END=9678) AI4,RAYY(I),RAYX(I),N3
                                  RAYW(I)=DBLE(N3)
                                  IF(RAYW(I).EQ.0.0D0) THEN
                                      IF(I.GE.1.AND.I.LE.41) RAYW(I)=SYSTEM1(11)
                                      IF(I.GE.42.AND.I.LE.82) RAYW(I)=SYSTEM1(7)
                                      IF(I.GE.83.AND.I.LE.123) RAYW(I)=SYSTEM1(8)
                                  END IF
                              END DO
                          ELSE
                          END IF
                          IF(F16.EQ.1.AND.INPUT(1:3).NE.'EOM') GO TO 9677
                          IF(F16.EQ.1.AND.INPUT(1:3).EQ.'EOM') THEN
                              F16=0
                              GO TO 9677
                          END IF
 9677                     CONTINUE
                          IF(INPUT(1:1).EQ.'.'.OR.
     1                    INPUT(1:1).EQ.'1'.OR.
     1                    INPUT(1:1).EQ.'2'.OR.
     1                    INPUT(1:1).EQ.'3'.OR.
     1                    INPUT(1:1).EQ.'4'.OR.
     1                    INPUT(1:1).EQ.'5'.OR.
     1                    INPUT(1:1).EQ.'6'.OR.
     1                    INPUT(1:1).EQ.'7'.OR.
     1                    INPUT(1:1).EQ.'8'.OR.
     1                    INPUT(1:1).EQ.'9'.OR.
     1                    INPUT(1:1).EQ.'0'
     1                    .OR.INPUT(1:8).EQ.'FLDSRAYS') THEN
                          ELSE
                              CALL PROCES
                          END IF
!     AFTER PROCESS, CLOSE AND KEEP BOTH 80 AND 81
                          GO TO 9679
 9678                     BACKSPACE(UNIT=44)
                          BACKSPACE(UNIT=44)
 9679                     CONTINUE
                      END IF
                  END DO
                  CALL CLOSE_FILE(44,1)

!     finished processing batch file
                  SQ=0
                  SST=0
                  SN=0
                  STI=0
                  CALL EXITT(0)
 
 9744             CONTINUE
                  OUTLYNE='ERROR READING BATCH FILE'
                  CALL SHOWIT(1)
                  SQ=0
                  SST=0
                  SN=0
                  STI=0
                  CALL EXITT(1)

 9743             CONTINUE
                  OUTLYNE='UNEXPECTED END OF BATCH FILE'
                  CALL SHOWIT(1)
                  SQ=0
                  SST=0
                  SN=0
                  STI=0
                  CALL EXITT(1)
              END IF
          END IF

          IF ( IN == 5 .AND. len_trim(BATCHFILE) > 0 ) THEN
              HALTING=.FALSE.

              open(unit=115,file=trim(HOME)//'plotdata/yellow.txt')   ! ???
              open(unit=116,file=trim(HOME)//'plotdata/magenta.txt')
              open(unit=117,file=trim(HOME)//'plotdata/red.txt')
              open(unit=118,file=trim(HOME)//'plotdata/cyan.txt')
              open(unit=119,file=trim(HOME)//'plotdata/contdata.txt')
              open(unit=130,file=trim(HOME)//'plotdata/black.txt')
              open(unit=131,file=trim(HOME)//'plotdata/breakblack.txt')
              open(unit=150,file=trim(HOME)//'drawcmd3.txt')

              CALL userinput(CMDNO)

              close(115)
              close(116)
              close(117)
              close(118)
              close(130)
              close(131)
              close(150)

              GO TO 1
          END IF

          IF(IN.NE.5) THEN
!       IN NOT EQUAL TO 5, USE A LOOP TO READ FROM DISK
!
!               IN IS NOT EQUAL TO 5 (KEYBOARD INPUT)
              OPENIN=.FALSE.
              INQUIRE(UNIT=IN,OPENED=OPENIN)
              IF(OPENIN) THEN
                  REWIND(UNIT=IN)
                  DO I=1,99999
                      READ(UNIT=IN,FMT=100,END=9999,ERR=9889) INPUT(1:140)
                      IF(INPUT(1:3).NE.'OUT'.OR.F2.EQ.1.OR.F3.EQ.1.OR.F4.EQ.1) THEN
                          IF(INPUT(1:1).EQ.' ') INPUT(1:140)=INPUT(2:140)//' '
                          IF(INPUT(1:1).EQ.' ') INPUT(1:140)=INPUT(2:140)//' '
                          IF(INPUT(1:1).EQ.' ') INPUT(1:140)=INPUT(2:140)//' '
                          IF(INPUT(1:1).EQ.' ') INPUT(1:140)=INPUT(2:140)//' '
                          IF(INPUT(1:1).EQ.' ') INPUT(1:140)=INPUT(2:140)//' '
                          IF(INPUT(1:1).EQ.' ') INPUT(1:140)=INPUT(2:140)//' '
                          IF(INPUT(1:1).EQ.' ') INPUT(1:140)=INPUT(2:140)//' '
                          OIN=IN
                          IF(INPUT(1:8).EQ.'FLDSRAYS') THEN
                              IREND=500
                              IF(INPUT(10:13).EQ.' 500') IREND=500
                              IF(INPUT(10:13).EQ.'1000') IREND=1000
                              IF(INPUT(10:13).EQ.'1500') IREND=1500
                              IF(INPUT(10:13).EQ.'2000') IREND=2000
                              IF(INPUT(10:13).EQ.'2500') IREND=2500
                              IF(INPUT(10:13).EQ.'3000') IREND=3000
                              IF(INPUT(10:13).EQ.'3500') IREND=3500
                              IF(INPUT(10:13).EQ.'4000') IREND=4000
                              IF(INPUT(10:13).EQ.'4500') IREND=4500
                              IF(INPUT(10:13).EQ.'5000') IREND=5000
                              DO J=1,200
                                  READ(IN,*) AI,FIELDY(I),FIELDX(I),
     1                            FIELDZ(I),N3
                                  FIELDW(I)=DBLE(N3)
                                  IF(FIELDW(I).EQ.0.0D0) THEN
                                      FIELDW(I)=SYSTEM1(11)
                                  END IF
                              END DO
                              DO J=1,IREND
                                  READ(IN,*,ERR=9778,END=9778) AI4,RAYY(I),RAYX(I),N3
                                  RAYW(I)=DBLE(N3)
                                  IF(RAYW(I).EQ.0.0D0) THEN
                                      IF(I.GE.1.AND.I.LE.41) RAYW(I)=SYSTEM1(11)
                                      IF(I.GE.42.AND.I.LE.82) RAYW(I)=SYSTEM1(7)
                                      IF(I.GE.83.AND.I.LE.123) RAYW(I)=SYSTEM1(8)
                                  END IF
                              END DO
                          ELSE
                          END IF
                          IF(F16.EQ.1.AND.INPUT(1:3).NE.'EOM') GO TO 222
                          IF(F16.EQ.1.AND.INPUT(1:3).EQ.'EOM') THEN
                              F16=0
                              GO TO 222
                          END IF
 222                      CONTINUE
                          IF(INPUT(1:1).EQ.'.'.OR.
     1                    INPUT(1:1).EQ.'1'.OR.
     1                    INPUT(1:1).EQ.'2'.OR.
     1                    INPUT(1:1).EQ.'3'.OR.
     1                    INPUT(1:1).EQ.'4'.OR.
     1                    INPUT(1:1).EQ.'5'.OR.
     1                    INPUT(1:1).EQ.'6'.OR.
     1                    INPUT(1:1).EQ.'7'.OR.
     1                    INPUT(1:1).EQ.'8'.OR.
     1                    INPUT(1:1).EQ.'9'.OR.
     1                    INPUT(1:1).EQ.'0'
     1                    .OR.INPUT(1:8).EQ.'FLDSRAYS') THEN
                          ELSE
                              MULTICOM=.TRUE.
                              CALL PROCES
                              MULTICOM=.FALSE.
                          END IF
                          IF(IN.EQ.5) GO TO 226
                          GO TO 9779
 9778                     BACKSPACE(UNIT=IN)
                          BACKSPACE(UNIT=IN)
 9779                     CONTINUE
                      END IF
                  END DO
              ELSE
              END IF
 226          CONTINUE
          END IF
          IF(IN.EQ.5) GO TO 1
 9999     OPENIN=.FALSE.
          INQUIRE(UNIT=IN,OPENED=OPENIN)
          IF(OPENIN) THEN
              BACKSPACE(UNIT=IN)
              REWIND (UNIT=IN)
              CALL CLOSE_FILE(IN,1)
          END IF
          IF(IN.NE.8.AND.IN.NE.9.AND.IN.NE.97) IN=5

          IF(IN.EQ.8) THEN
              IN=5
              LASTFIL=OFILN
              OFILN='            '
              WRITE(OUTLYNE,*)'INPUT FROM FILE "CARDTEXT.DAT" COMPLETED'
              CALL SHOWIT(1)
          END IF
          IF(IN.EQ.97) THEN
              IN=5
              LASTFIL=OFILN
              WRITE(OUTLYNE,*)'INPUT FROM FILE '//OFILN//' COMPLETED'
              CALL SHOWIT(1)
          END IF
          IF(IN.EQ.9) THEN
              IN=5
              LASTFIL=OFILN
              OFILN='            '
              WRITE(OUTLYNE,*)'INPUT FROM FILE "EDITTEXT.DAT" COMPLETED'
              CALL SHOWIT(1)
          END IF
          LASTFIL=OFILN
          OFILN='            '
!     ONLY PRINT I/O RESET IF NOT COMMING FROM INSIDE A MACRO
          IF(F4.NE.1)
     1      WRITE(OUTLYNE,*)
     1      'INPUT RESET TO "TP"'
          CALL SHOWIT(1)
          GO TO 1
 9877     OUTLYNE='AN INPUT FILE ERROR WAS DETECTED'
          CALL SHOWIT(1)
          OUTLYNE='THE INPUT FILE HAS BEEN DELETED'
          CALL SHOWIT(1)
          CALL CLOSE_FILE(16,0)
          GO TO 1  ! process next command

100       FORMAT(A140)

 9889     OUTLYNE='AN INPUT FILE ERROR WAS DETECTED'
          CALL SHOWIT(1)
          OUTLYNE='THE INPUT FILE HAS BEEN DELETED'
          CALL SHOWIT(1)
          OUTLYNE='INPUT RESET TO "TP"'
          CALL SHOWIT(1)
          IN=5
          LASTFIL=OFILN
          OFILN='            '
          GO TO 1  ! process next command

      END PROGRAM KOKO


      SUBROUTINE NEWFIELD
          IMPLICIT NONE

          INCLUDE 'datsub.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'

          FIELDX(1)=0.0D0
          FIELDX(2)=0.0D0
          FIELDX(3)=0.0D0
          FIELDX(4)=1.0D0
          FIELDX(5)=-1.0D0
          FIELDX(6)=0.0D0
          FIELDX(7)=0.0D0
          FIELDX(8)=0.866D0
          FIELDX(9)=-0.866D0
          FIELDX(10)=0.0D0
          FIELDX(11)=0.0D0
          FIELDX(12)=0.707D0
          FIELDX(13)=-0.707D0
          FIELDX(14)=0.0D0
          FIELDX(15)=0.0D0
          FIELDX(16)=0.5D0
          FIELDX(17)=-0.5D0
          FIELDX(18)=1.0D0
          FIELDX(19)=-1.0D0
          FIELDX(20)=1.0D0
          FIELDX(21)=-1.0D0
          FIELDX(22)=0.866D0
          FIELDX(23)=-0.866D0
          FIELDX(24)=0.866D0
          FIELDX(25)=-0.866D0
          FIELDX(26)=0.707D0
          FIELDX(27)=-0.707D0
          FIELDX(28)=0.707D0
          FIELDX(29)=-0.707D0
          FIELDX(30)=0.5D0
          FIELDX(31)=-0.5D0
          FIELDX(32)=0.5D0
          FIELDX(33)=-0.5D0
          FIELDX(34)=0.612D0
          FIELDX(35)=-0.612D0
          FIELDX(36)=0.612D0
          FIELDX(37)=-0.612D0
          FIELDX(38)=0.354D0
          FIELDX(39)=-0.354D0
          FIELDX(40)=0.354D0
          FIELDX(41)=-0.354D0
          FIELDY(1)=0.0D0
          FIELDY(2)=1.0D0
          FIELDY(3)=-1.0D0
          FIELDY(4)=0.0D0
          FIELDY(5)=0.0D0
          FIELDY(6)=0.866D0
          FIELDY(7)=-0.866D0
          FIELDY(8)=0.0D0
          FIELDY(9)=0.0D0
          FIELDY(10)=0.707D0
          FIELDY(11)=-0.707D0
          FIELDY(12)=0.0D0
          FIELDY(13)=0.D0
          FIELDY(14)=0.5D0
          FIELDY(15)=-0.5D0
          FIELDY(16)=0.0D0
          FIELDY(17)=0.0D0
          FIELDY(18)=1.0D0
          FIELDY(19)=1.0D0
          FIELDY(20)=-1.0D0
          FIELDY(21)=-1.0D0
          FIELDY(22)=0.866D0
          FIELDY(23)=0.866D0
          FIELDY(24)=-0.866D0
          FIELDY(25)=-0.866D0
          FIELDY(26)=0.707D0
          FIELDY(27)=0.707D0
          FIELDY(28)=-0.707D0
          FIELDY(29)=-0.707D0
          FIELDY(30)=0.5D0
          FIELDY(31)=0.5D0
          FIELDY(32)=-0.5D0
          FIELDY(33)=-0.5D0
          FIELDY(34)=0.612D0
          FIELDY(35)=0.612D0
          FIELDY(36)=-0.612D0
          FIELDY(37)=-0.612D0
          FIELDY(38)=0.354D0
          FIELDY(39)=0.354D0
          FIELDY(40)=-0.354D0
          FIELDY(41)=-0.354D0
          FIELDW(1:41)=SYSTEM1(11)
          FIELDZ(1:41)=0.0D0

      END SUBROUTINE NEWFIELD


      SUBROUTINE NEWRAY
          IMPLICIT NONE
          INTEGER I
          INCLUDE 'datsub.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          I=41
          RAYW(1:41)=SYSTEM1(11)
          RAYW(42:82)=SYSTEM1(7)
          RAYW(83:123)=SYSTEM1(8)
          RAYX(1)    =0.0D0
          RAYX(1+41) =0.0D0
          RAYX(1+82) =0.0D0

          RAYX(2)    =0.0D0
          RAYX(2+41) =0.0D0
          RAYX(2+82) =0.0D0

          RAYX(3)    =0.0D0
          RAYX(3+41) =0.0D0
          RAYX(3+82) =0.0D0

          RAYX(4)    =1.0D0
          RAYX(4+41) =1.0D0
          RAYX(4+82) =1.0D0

          RAYX(5)    =-1.0D0
          RAYX(5+41) =-1.0D0
          RAYX(5+82) =-1.0D0

          RAYX(6)    =0.0D0
          RAYX(6+41) =0.0D0
          RAYX(6+82) =0.0D0

          RAYX(7)    =0.0D0
          RAYX(7+41) =0.0D0
          RAYX(7+82) =0.0D0

          RAYX(8)    =0.866D0
          RAYX(8+41) =0.866D0
          RAYX(8+82) =0.866D0

          RAYX(9)    =-0.866D0
          RAYX(9+41) =-0.866D0
          RAYX(9+82) =-0.866D0

          RAYX(10)    =0.0D0
          RAYX(10+41) =0.0D0
          RAYX(10+82) =0.0D0

          RAYX(11)    =0.0D0
          RAYX(11+41) =0.0D0
          RAYX(11+82) =0.0D0

          RAYX(12)    =0.707D0
          RAYX(12+41) =0.707D0
          RAYX(12+82) =0.707D0

          RAYX(13)    =-0.707D0
          RAYX(13+41) =-0.707D0
          RAYX(13+82) =-0.707D0

          RAYX(14)    =0.0D0
          RAYX(14+41) =0.0D0
          RAYX(14+82) =0.0D0

          RAYX(15)    =0.0D0
          RAYX(15+41) =0.0D0
          RAYX(15+82) =0.0D0

          RAYX(16)    =0.5D0
          RAYX(16+41) =0.5D0
          RAYX(16+82) =0.5D0

          RAYX(17)    =-0.5D0
          RAYX(17+41) =-0.5D0
          RAYX(17+82) =-0.5D0

          RAYX(18)    =1.0D0
          RAYX(18+41) =1.0D0
          RAYX(18+82) =1.0D0

          RAYX(19)    =-1.0D0
          RAYX(19+41) =-1.0D0
          RAYX(19+82) =-1.0D0

          RAYX(20)    =1.0D0
          RAYX(20+41) =1.0D0
          RAYX(20+82) =1.0D0

          RAYX(21)    =-1.0D0
          RAYX(21+41) =-1.0D0
          RAYX(21+82) =-1.0D0

          RAYX(22)    =0.866D0
          RAYX(22+41) =0.866D0
          RAYX(22+82) =0.866D0

          RAYX(23)    =-0.866D0
          RAYX(23+41) =-0.866D0
          RAYX(23+82) =-0.866D0

          RAYX(24)    =0.866D0
          RAYX(24+41) =0.866D0
          RAYX(24+82) =0.866D0

          RAYX(25)    =-0.866D0
          RAYX(25+41) =-0.866D0
          RAYX(25+82) =-0.866D0

          RAYX(26)    =0.707D0
          RAYX(26+41) =0.707D0
          RAYX(26+82) =0.707D0

          RAYX(27)    =-0.707D0
          RAYX(27+41) =-0.707D0
          RAYX(27+82) =-0.707D0

          RAYX(28)    =0.707D0
          RAYX(28+41) =0.707D0
          RAYX(28+82) =0.707D0

          RAYX(29)    =-0.707D0
          RAYX(29+41) =-0.707D0
          RAYX(29+82) =-0.707D0

          RAYX(30)    =0.5D0
          RAYX(30+41) =0.5D0
          RAYX(30+82) =0.5D0

          RAYX(31)    =-0.5D0
          RAYX(31+41) =-0.5D0
          RAYX(31+82) =-0.5D0

          RAYX(32)    =0.5D0
          RAYX(32+41) =0.5D0
          RAYX(32+82) =0.5D0

          RAYX(33)    =-0.5D0
          RAYX(33+41) =-0.5D0
          RAYX(33+82) =-0.5D0

          RAYX(34)    =0.612D0
          RAYX(34+41) =0.612D0
          RAYX(34+82) =0.612D0

          RAYX(35)    =-0.612D0
          RAYX(35+41) =-0.612D0
          RAYX(35+82) =-0.612D0

          RAYX(36)    =0.612D0
          RAYX(36+41) =0.612D0
          RAYX(36+82) =0.612D0

          RAYX(37)    =-0.612D0
          RAYX(37+41) =-0.612D0
          RAYX(37+82) =-0.612D0

          RAYX(38)    =0.354D0
          RAYX(38+41) =0.354D0
          RAYX(38+82) =0.354D0

          RAYX(39)    =-0.354D0
          RAYX(39+41) =-0.354D0
          RAYX(39+82) =-0.354D0

          RAYX(40)    =0.354D0
          RAYX(40+41) =0.354D0
          RAYX(40+82) =0.354D0

          RAYX(41)    =-0.354D0
          RAYX(41+41) =-0.354D0
          RAYX(41+82) =-0.354D0

          RAYY(1)    =0.0D0
          RAYY(1+41) =0.0D0
          RAYY(1+82) =0.0D0

          RAYY(2)    =1.0D0
          RAYY(2+41) =1.0D0
          RAYY(2+82) =1.0D0

          RAYY(3)    =-1.0D0
          RAYY(3+41) =-1.0D0
          RAYY(3+82) =-1.0D0

          RAYY(4)    =0.0D0
          RAYY(4+41) =0.0D0
          RAYY(4+82) =0.0D0

          RAYY(5)    =0.0D0
          RAYY(5+41) =0.0D0
          RAYY(5+82) =0.0D0

          RAYY(6)    =0.866D0
          RAYY(6+41) =0.866D0
          RAYY(6+82) =0.866D0

          RAYY(7)    =-0.866D0
          RAYY(7+41) =-0.866D0
          RAYY(7+82) =-0.866D0

          RAYY(8)    =0.0D0
          RAYY(8+41) =0.0D0
          RAYY(8+82) =0.0D0

          RAYY(9)    =0.0D0
          RAYY(9+41) =0.0D0
          RAYY(9+82) =0.0D0

          RAYY(10)    =0.707D0
          RAYY(10+41) =0.707D0
          RAYY(10+82) =0.707D0

          RAYY(11)    =-0.707D0
          RAYY(11+41) =-0.707D0
          RAYY(11+82) =-0.707D0

          RAYY(12)    =0.0D0
          RAYY(12+41) =0.0D0
          RAYY(12+82) =0.0D0

          RAYY(13)    =0.0D0
          RAYY(13+41) =0.0D0
          RAYY(13+82) =0.0D0

          RAYY(14)    =0.5D0
          RAYY(14+41) =0.5D0
          RAYY(14+82) =0.5D0

          RAYY(15)    =-0.5D0
          RAYY(15+41) =-0.5D0
          RAYY(15+82) =-0.5D0

          RAYY(16)    =0.0D0
          RAYY(16+41) =0.0D0
          RAYY(16+82) =0.0D0

          RAYY(17)    =0.0D0
          RAYY(17+41) =0.0D0
          RAYY(17+82) =0.0D0

          RAYY(18)    =1.0D0
          RAYY(18+41) =1.0D0
          RAYY(18+82) =1.0D0

          RAYY(19)    =1.0D0
          RAYY(19+41) =1.0D0
          RAYY(19+82) =1.0D0

          RAYY(20)    =-1.0D0
          RAYY(20+41) =-1.0D0
          RAYY(20+82) =-1.0D0

          RAYY(21)    =-1.0D0
          RAYY(21+41) =-1.0D0
          RAYY(21+82) =-1.0D0

          RAYY(22)    =0.866D0
          RAYY(22+41) =0.866D0
          RAYY(22+82) =0.866D0

          RAYY(23)    =0.866D0
          RAYY(23+41) =0.866D0
          RAYY(23+82) =0.866D0

          RAYY(24)    =-0.866D0
          RAYY(24+41) =-0.866D0
          RAYY(24+82) =-0.866D0

          RAYY(25)    =-0.866D0
          RAYY(25+41) =-0.866D0
          RAYY(25+82) =-0.866D0

          RAYY(26)    =0.707D0
          RAYY(26+41) =0.707D0
          RAYY(26+82) =0.707D0

          RAYY(27)    =0.707D0
          RAYY(27+41) =0.707D0
          RAYY(27+82) =0.707D0

          RAYY(28)    =-0.707D0
          RAYY(28+41) =-0.707D0
          RAYY(28+82) =-0.707D0

          RAYY(29)    =-0.707D0
          RAYY(29+41) =-0.707D0
          RAYY(29+82) =-0.707D0

          RAYY(30)    =0.5D0
          RAYY(30+41) =0.5D0
          RAYY(30+82) =0.5D0

          RAYY(31)    =0.5D0
          RAYY(31+41) =0.5D0
          RAYY(31+82) =0.5D0

          RAYY(32)    =-0.5D0
          RAYY(32+41) =-0.5D0
          RAYY(32+82) =-0.5D0

          RAYY(33)    =-0.5D0
          RAYY(33+41) =-0.5D0
          RAYY(33+82) =-0.5D0

          RAYY(34)    =0.612D0
          RAYY(34+41) =0.612D0
          RAYY(34+82) =0.612D0

          RAYY(35)    =0.612D0
          RAYY(35+41) =0.612D0
          RAYY(35+82) =0.612D0

          RAYY(36)    =-0.612D0
          RAYY(36+41) =-0.612D0
          RAYY(36+82) =-0.612D0

          RAYY(37)    =-0.612D0
          RAYY(37+41) =-0.612D0
          RAYY(37+82) =-0.612D0

          RAYY(38)    =0.354D0
          RAYY(38+41) =0.354D0
          RAYY(38+82) =0.354D0

          RAYY(39)    =0.354D0
          RAYY(39+41) =0.354D0
          RAYY(39+82) =0.354D0

          RAYY(40)    =-0.354D0
          RAYY(40+41) =-0.354D0
          RAYY(40+82) =-0.354D0

          RAYY(41)    =-0.354D0
          RAYY(41+41) =-0.354D0
          RAYY(41+82) =-0.354D0

      END SUBROUTINE NEWRAY


      SUBROUTINE SELECTKOKO(KKDP)
          IMPLICIT NONE
          CHARACTER KKDP*3
          INCLUDE 'datlen.inc'
          INCLUDE 'datcfg.inc'
          INCLUDE 'datmac.inc'
          INCLUDE 'datsub.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datspd.inc'
          INCLUDE 'datsp1.inc'

          IF (F1.EQ.1.AND.F17.EQ.0) THEN
              KKDP='cmd'
          ELSE IF (F1.EQ.1.AND.F17.EQ.1) THEN
              KKDP='spe'
          ELSE IF (F2.EQ.1) THEN
              KKDP='mac'
          ELSE IF (F3.EQ.1) THEN
              KKDP='med'
          ELSE IF (F5.EQ.1) THEN
              KKDP='len'
          ELSE IF (F6.EQ.1) THEN
              KKDP='uln'
          ELSE IF (F7.EQ.1) THEN
              KKDP='sps'
          ELSE IF (F8.EQ.1) THEN
              KKDP='usp'
          ELSE IF (F9.EQ.1) THEN
              KKDP='fit'
          ELSE IF (F10.EQ.1) THEN
              KKDP='cfg'
          ELSE IF (F11.EQ.1) THEN
              KKDP='ucf'
          ELSE IF (F27.EQ.1) THEN
              KKDP='mer'
          ELSE IF (F27.EQ.2) THEN
              KKDP='umr'
          ELSE IF (F29.EQ.1) THEN
              KKDP='var'
          ELSE IF (F29.EQ.2) THEN
              KKDP='uvb'
          ELSE IF (F51.EQ.1) THEN
              KKDP='tvb'
          ELSE IF (F51.EQ.2) THEN
              KKDP='utv'
          ELSE IF (F52.EQ.1) THEN
              KKDP='cmp'
          ELSE IF (F52.EQ.2) THEN
              KKDP='ucp'
          ELSE IF (F53.EQ.1) THEN
              KKDP='top'
          ELSE IF (F53.EQ.2) THEN
              KKDP='utp'
          ELSE IF (F54.EQ.1) THEN
              KKDP='foc'
          ELSE IF (F54.EQ.2) THEN
              KKDP='ufc'
          ELSE 
             KKDP='cmd'
          END IF
      END SUBROUTINE SELECTKOKO

      
      SUBROUTINE PROGSIZE

          IMPLICIT NONE

          INCLUDE 'datlen.inc'
          INCLUDE 'datcfg.inc'
          INCLUDE 'datmac.inc'
          INCLUDE 'datsub.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datspd.inc'
          INCLUDE 'datsp1.inc'
          INCLUDE 'dathgr.inc'
          
          WRITE(OUTLYNE,*)'         MAXIMUM LENS SURFACES = ',MAXSUR-1
          CALL SHOWIT(1)
          WRITE(OUTLYNE,*)'      MAXIMUM NUMBER OF MACROS = ',MAXMAC
          CALL SHOWIT(1)
          WRITE(OUTLYNE,*)' MAXIMUM MACRO LINES PER MACRO = ',MAXLIN
          CALL SHOWIT(1)
          WRITE(OUTLYNE,*)' MAX# ALTERNATE CONFIGURATIONS = ',MAXCFG
          CALL SHOWIT(1)
          IF(MAXSUR.NE.499) THEN
              OUTLYNE='IN ORDER TO GET'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'500 LENS SURFACES'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'999 MACROS'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'1024 MACRO LINES PER MACRO'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'75 ALTERNATE LENS CONFIGURATIONS'
              CALL SHOWIT(1)
          END IF

      END SUBROUTINE PROGSIZE

      
      SUBROUTINE userinput(ncmd)

          IMPLICIT NONE

          INCLUDE 'datmai.inc'

          INTEGER ncmd
          CHARACTER KKDP*3

          CALL SELECTKOKO(KKDP)

          WRITE (*,'(a,i0,a)',advance='no') ' ',ncmd,':'//KKDP//'> '

!      call disphistory(KKDP,I,INPUT)  !If you use history, remove ! this line
          READ (*,'(a)') INPUT

          WC='        '

          CALL upper_case(INPUT)

          IF (INPUT.EQ.'') THEN
              INPUT=' '
          END IF

          IF (ECH.EQ.1) THEN
              OUTLYNE='> '//TRIM(INPUT)
              CALL SHOWIT(1)
          END IF

          CALL PROCES

      END SUBROUTINE userinput


      SUBROUTINE disphistory(KKDP,I,INPUT0)
          IMPLICIT NONE

          INCLUDE 'datmai.inc'

          CHARACTER KKDP*3,HISTORY*15,INPUT0*15
          CHARACTER :: CR = CHAR(13)
          INTEGER KEY0,KEY1,KEY2,I,HISTNO
          LOGICAL f_exist

          KEY0=0
          KEY1=0
          KEY2=0

          CALL SYS_KEYSET(1)

          HISTNO=I
          HISTORY=""

          IF (I.EQ.1) HISTNO=1

          !INPUT must be charactor

          DO WHILE (KEY0.LT.32)

              CALL SYS_KEYIN(KEY0)

              IF (KEY0.EQ.27) THEN
                  CALL SYS_KEYIN(KEY1)
                  CALL SYS_KEYIN(KEY2)

                  IF ((KEY2.EQ.67).OR.(KEY2.EQ.68)) CONTINUE

                  IF (KEY2.EQ.65) THEN !Up Arrow
                      HISTNO=HISTNO-1
                      IF (HISTNO.LE.0) HISTNO=I-1
                      IF (I.EQ.1) HISTNO=1
                  ENDIF
                  IF (KEY2.EQ.66) THEN !Down Arrow
                      HISTNO=HISTNO+1
                      IF (HISTNO.GE.I) HISTNO=1
                      IF (I.EQ.1) HISTNO=I
                  ENDIF

                  INQUIRE(file=TRIM(HOME)//'HISTORY.DAT', exist=f_exist)

                  IF (f_exist.EQV..TRUE.) THEN
                      OPEN(170,file=TRIM(HOME)//'HISTORY.DAT',status='old',
     &                access='direct',recl=15,form='formatted')
                  ELSE
                      OPEN(170,file=TRIM(HOME)//'HISTORY.DAT',status='new',
     &                access='direct',recl=15,form='formatted')
                      WRITE(170,'(A15)',rec=1) "NO INPUT"
                  ENDIF

                  READ(170,'(A15)',rec=HISTNO) HISTORY
                  CLOSE(170)

                  WRITE(6,'(A$)') CR//'               '
                  WRITE(6,'(A$)') CR//KKDP//'> '//TRIM(HISTORY)

              ENDIF

              IF (KEY0.EQ.10) THEN
                  CALL SYS_KEYSET(0)
                  WRITE(6,'(A)') CR//KKDP//'> '//HISTORY
                  INPUT=HISTORY
                  RETURN
              END IF

              IF (KEY0.EQ.127) THEN
                  CALL SYS_KEYSET(0)
                  WRITE(6,'(A$)') CR//'               '
                  WRITE(6,'(A)') CR//KKDP//'> '
                  INPUT=""
                  RETURN
              END IF

          END DO

          CALL SYS_KEYSET(0)

          WRITE(6,'(A$)') CR//KKDP//'> '//CHAR(KEY0)
          READ(5,'(a$)') INPUT0
          INPUT=CHAR(KEY0)//INPUT0

          OPEN(170,file=TRIM(HOME)//'HISTORY.DAT',
     &    access='direct',recl=15,form='formatted')

          IF (INPUT.NE."") THEN
              WRITE(170,'(A15)',rec=I) INPUT
              I=I+1
          ENDIF

          CLOSE(170)

          RETURN
      END SUBROUTINE disphistory


      SUBROUTINE GREETING

        INCLUDE 'buildinfo.inc'      

        WRITE (*,*)
        WRITE (*,*) 'Koko Optical Design Software (KODS)'
        WRITE (*,*)
        WRITE (*,*) 'This is free software. There is ABSOLUTELY NO WARRANTY, '
        WRITE (*,*) 'not even for merchantability or fitness for a particular purpose.'
        WRITE (*,*) 'See COPYING, LICENSE, and AUTHORS in the source distribution for details.'
        WRITE (*,*) TRIM(buildstr)
        WRITE (*,*)
        
        RETURN
      END SUBROUTINE GREETING
      

      SUBROUTINE print_usage()

        WRITE (*,*) "Usage: koko-cli [-h] [-q] [-d[=]<datadir>] [-t[=]<tempdir>] [-b[=]<batchfile>]"
        WRITE (*,*) "Options and arguments:"
        WRITE (*,*) " -h            :  print this help message"
        WRITE (*,*) " -d <datadir>  :  set data directory for KODS"
        WRITE (*,*) " -t <tempdir>  :  set temporary file directory"
        WRITE (*,*) " -b <lensfile> :  excecute a lens file in batch mode"
        WRITE (*,*) " -q            :  run quietly, without printing the greeting"
        
      END SUBROUTINE print_usage
