! Copyright (C) 2020 The Koko Project Developers
!
! See the file COPYRIGHT.md in the top-level directory of this
! distribution
!
! This file is part of Koko.
!
! Koko is free software: you can redistribute it and/or modify it
! under the terms of the GNU General Public License as published by
! the Free Software Foundation, either version 3 of the License, or
! (at your option) any later version.
!
! Koko is distributed in the hope that it will be useful, but
! WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.
!
! You should have received a copy of the GNU General Public License
! along with Koko; see the file COPYING.  If not, see
! <https://www.gnu.org/licenses/>.

MODULE kokoconfig

  ! Functions and data structures for parsing the Koko configuration files

  USE configfile

  TYPE(CFG_t), PUBLIC :: koko_cfg
  LOGICAL, PRIVATE    :: koko_config_initialized = .FALSE.
  INTEGER, PARAMETER  :: dp = kind(1.0D0)

  
CONTAINS

  !-------------------------------------------------------------
  ! Defines default settings for the Koko configuration
  ! configuration data
  !
  ! INPUT
  ! default_kods :  default value of KODS library directory
  ! default_temp :  default value of the temp directory
  !
  ! OUTPUT
  ! config data are stored in koko_cfg
  !
  SUBROUTINE config_defaults( default_kods, default_temp )

    CHARACTER(LEN=*), INTENT(IN) :: default_kods, default_temp

    ! directories
    CALL CFG_add(koko_cfg, "directories%home",  default_kods, "Koko lib directory")
    CALL CFG_add(koko_cfg, "directories%temp",  default_temp, "Koko temp directory")

    ! graphics
    CALL CFG_add(koko_cfg, "graphics%viewer",   "gnuplot",    "Koko graphics viewer")
    CALL CFG_add(koko_cfg, "graphics%terminal", "wxt",        "gnuplot default terminal")
    CALL CFG_add(koko_cfg, "graphics%fontlrg",  "Courier,12", "Large graphics font")
    CALL CFG_add(koko_cfg, "graphics%fontmed",  "Courier,9",  "Medium graphics font")
    CALL CFG_add(koko_cfg, "graphics%fontsml",  "Courier,5",  "Small graphics font")
    CALL CFG_add(koko_cfg, "graphics%linewidth", 0.7_dp,      "Plotter line width")

    ! plot colors
    CALL CFG_add(koko_cfg, "color%default",      "black",     "default plotting color")
    CALL CFG_add(koko_cfg, "color%background",   "white",     "plot background")
    CALL CFG_add(koko_cfg, "color%rays",         "goldenrod", "ray color")
    CALL CFG_add(koko_cfg, "color%aperture",     "green3",    "aperture color")
    CALL CFG_add(koko_cfg, "color%obscuration",  "green3",    "obscurations")
    CALL CFG_add(koko_cfg, "color%edge",         "blue3",     "lens edge color")
    CALL CFG_add(koko_cfg, "color%profile",      "blue3",     "lens profile color")
    CALL CFG_add(koko_cfg, "color%axes",         "slategrey", "plot axes")
    CALL CFG_add(koko_cfg, "color%frame",        "black",     "plot frame")
    CALL CFG_add(koko_cfg, "color%label",        "black",     "label color")
    CALL CFG_add(koko_cfg, "color%spectral",     "black",     "for spectral plots")
    CALL CFG_add(koko_cfg, "color%airy",         "red3",      "airy circle")
    CALL CFG_add(koko_cfg, "color%marker",       "midnightblue", "splot diagram markers")
    CALL CFG_add(koko_cfg, "color%wavelength1",  "#FFDD33",   "yellow wavelength")
    CALL CFG_add(koko_cfg, "color%wavelength2",  "green3",    "green wavelength")
    CALL CFG_add(koko_cfg, "color%wavelength3",  "#FF8C19",   "orange wavelength")
    CALL CFG_add(koko_cfg, "color%wavelength4",  "blue",      "blue wavelength")
    CALL CFG_add(koko_cfg, "color%wavelength5",  "red2",      "red wavelength")
    CALL CFG_add(koko_cfg, "color%wavelength6",  "black",     "optional wavelength 1")
    CALL CFG_add(koko_cfg, "color%wavelength7",  "black",     "optional wavelength 2")
    CALL CFG_add(koko_cfg, "color%wavelength8",  "black",     "optional wavelength 3")
    CALL CFG_add(koko_cfg, "color%wavelength9",  "black",     "optional wavelength 4")
    CALL CFG_add(koko_cfg, "color%wavelength10", "black",     "optional wavelength 5")
    
    ! text
    CALL CFG_add(koko_cfg, "text%editor",       "vi",        "Koko text editor")
       
    ! cli
    CALL CFG_add(koko_cfg, "cli%promptcolor",    0,          "Prompt color default (black)")
    
    koko_config_initialized = .TRUE.
    
  END SUBROUTINE config_defaults
  
    
  !-------------------------------------------------------------
  ! Reads a configuration file and updates the structure holding
  ! configuration data
  !
  ! INPUT
  ! fname :         string with fully qualified name of config file
  ! default_kods :  default value of KODS library directory
  ! default_temp :  default value of the temp directory
  !
  ! OUTPUT
  ! config data are stored in koko_cfg
  !
  SUBROUTINE parse_config_file( fname )

    CHARACTER(LEN=*), INTENT(IN) :: fname

    IF (.NOT. koko_config_initialized) THEN
       WRITE (*,*)
       WRITE (*,*) "Fatal error: Koko configuration is not initialized"
       WRITE (*,*)
       STOP
    END IF
    
    ! read the configuration file
    CALL CFG_read_file(koko_cfg, fname)
   
  END SUBROUTINE parse_config_file

  
  !-------------------------------------------------------------
  ! Display the current configuration settings of Koko
  !
  SUBROUTINE print_koko_config()

    CHARACTER(LEN=256) :: cval
    REAL(KIND=dp)      :: rval
    INTEGER            :: ival

    WRITE (*,*)
    WRITE (*,*) "Koko Configuration Settings"

    ! directories
    WRITE (*,*)
    WRITE (*,*) "[directories]"
    CALL CFG_get(koko_cfg, "directories%home", cval)
    WRITE (*,*) "    home = "//TRIM(cval)
    CALL CFG_get(koko_cfg, "directories%temp", cval)
    WRITE (*,*) "    temp = "//TRIM(cval)

    ! basic graphics settings
    WRITE (*,*)
    WRITE (*,*) "[graphics]"
    CALL CFG_get(koko_cfg, "graphics%viewer", cval)
    WRITE (*,*) "    viewer = "//TRIM(cval)
    CALL CFG_get(koko_cfg, "graphics%terminal", cval)
    WRITE (*,*) "    terminal = "//TRIM(cval)
    CALL CFG_get(koko_cfg, "graphics%fontlrg", cval)
    WRITE (*,*) "    fontlrg = "//TRIM(cval)
    CALL CFG_get(koko_cfg, "graphics%fontmed", cval)
    WRITE (*,*) "    fontmed = "//TRIM(cval)
    CALL CFG_get(koko_cfg, "graphics%fontsml", cval)
    WRITE (*,*) "    fontsml = "//TRIM(cval)
    CALL CFG_get(koko_cfg, "graphics%linewidth", rval)
    WRITE (*,"(A,F5.2)") "     linewidth = ", rval

    ! plot color settings
    WRITE (*,*)
    WRITE (*,*) "[color]"
    CALL CFG_get(koko_cfg, "color%default",      cval)
    WRITE (*,*) "    default = "//TRIM(cval)
    CALL CFG_get(koko_cfg, "color%background",   cval)
    WRITE (*,*) "    background = "//TRIM(cval)
    CALL CFG_get(koko_cfg, "color%rays",         cval)
    WRITE (*,*) "    rays = "//TRIM(cval)
    CALL CFG_get(koko_cfg, "color%aperture",     cval)
    WRITE (*,*) "    aperture = "//TRIM(cval)
    CALL CFG_get(koko_cfg, "color%obscuration",  cval)
    WRITE (*,*) "    obscuration = "//TRIM(cval)
    CALL CFG_get(koko_cfg, "color%edge",         cval)
    WRITE (*,*) "    edge = "//TRIM(cval)
    CALL CFG_get(koko_cfg, "color%profile",      cval)
    WRITE (*,*) "    profile = "//TRIM(cval)
    CALL CFG_get(koko_cfg, "color%axes",         cval)
    WRITE (*,*) "    axes = "//TRIM(cval)
    CALL CFG_get(koko_cfg, "color%frame",        cval)
    WRITE (*,*) "    frame = "//TRIM(cval)
    CALL CFG_get(koko_cfg, "color%label",        cval)
    WRITE (*,*) "    label = "//TRIM(cval)
    CALL CFG_get(koko_cfg, "color%spectral",     cval)
    WRITE (*,*) "    spectral = "//TRIM(cval)
    CALL CFG_get(koko_cfg, "color%airy",         cval)
    WRITE (*,*) "    airy = "//TRIM(cval)
    CALL CFG_get(koko_cfg, "color%marker",       cval)
    WRITE (*,*) "    marker = "//TRIM(cval)
    CALL CFG_get(koko_cfg, "color%wavelength1",  cval)
    WRITE (*,*) "    wavelength1 = "//TRIM(cval)
    CALL CFG_get(koko_cfg, "color%wavelength2",  cval)
    WRITE (*,*) "    wavelength2 = "//TRIM(cval)
    CALL CFG_get(koko_cfg, "color%wavelength3",  cval)
    WRITE (*,*) "    wavelength3 = "//TRIM(cval)
    CALL CFG_get(koko_cfg, "color%wavelength4",  cval)
    WRITE (*,*) "    wavelength4 = "//TRIM(cval)
    CALL CFG_get(koko_cfg, "color%wavelength5",  cval)
    WRITE (*,*) "    wavelength5 = "//TRIM(cval)
    CALL CFG_get(koko_cfg, "color%wavelength6",  cval)
    WRITE (*,*) "    wavelength6 = "//TRIM(cval)
    CALL CFG_get(koko_cfg, "color%wavelength7",  cval)
    WRITE (*,*) "    wavelength7 = "//TRIM(cval)
    CALL CFG_get(koko_cfg, "color%wavelength8",  cval)
    WRITE (*,*) "    wavelength8 = "//TRIM(cval)
    CALL CFG_get(koko_cfg, "color%wavelength9",  cval)
    WRITE (*,*) "    wavelength9 = "//TRIM(cval)
    CALL CFG_get(koko_cfg, "color%wavelength10", cval)
    WRITE (*,*) "    wavelength10 = "//TRIM(cval)
    
    
    ! text editor
    WRITE (*,*)
    WRITE (*,*) "[text]"
    CALL CFG_get(koko_cfg, "text%editor", cval)
    WRITE (*,*) "    editor = "//TRIM(cval)

    ! prompt color
    WRITE (*,*)
    WRITE (*,*) "[cli]"
    CALL CFG_get(koko_cfg, "cli%promptcolor", ival)
    WRITE (*,"(A,I2)") "     promptcolor = ", ival

    WRITE (*,*)
    
  END SUBROUTINE print_koko_config
    
END MODULE kokoconfig
