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

MODULE opsys

  ! Provides platform-independent access to operating system services
  !
  ! Ulf GRIESMANN, May 2020

CONTAINS

  SUBROUTINE user_home_directory( has_homedir, homedir )

    ! returns the home directory of the user running Koko
    !
    ! OUTPUT
    ! has_homedir :  returns T if the user has a home directory
    ! homedir :      the fully qualified path of the home directory,
    !                including, in Windows, the home drive.
    
    CHARACTER(len=*), INTENT(out) :: homedir
    LOGICAL, INTENT(out)          :: has_homedir
    
    INTEGER            :: status, length

# if defined( WINDOWS )
    CHARACTER(len=4)   :: homedrive
    CHARACTER(len=256) :: homepath
#endif
    
#if defined( LINUX ) || defined( MACOSX )
    CALL get_environment_variable("HOME", homedir, length, status)
    IF (status .NE. 0) THEN
       homedir = " "
       has_homedir = .FALSE.
    ELSE
       has_homedir = .TRUE.
    END IF
#endif

#if defined( WINDOWS )
    CALL get_environment_variable("HOMEDRIVE", homedrive, length, status)
    IF (status .NE. 0) THEN
       has_homedir = .FALSE.
       RETURN
    END IF
    
    CALL get_environment_variable("HOMEPATH", homepath, length, status)
    IF (status .NE. 0) THEN
       has_homedir = .FALSE.
       RETURN
    END IF

    homedir = TRIM(homedrive)//TRIM(homepath)
    has_homedir = .TRUE.
#endif
    
  END SUBROUTINE user_home_directory


  !----------------------------------------------------------
  LOGICAL FUNCTION kods_dir_exists( topdir )

    ! Checks if the data directory for KODS exists. Since the
    ! Fortran 'inquire' function cannot check for the existence of
    ! directories, this function checks for the existence of a file
    ! 'topdir/KODS/README_DATA'
    !
    ! INPUT
    ! topdir :  directory containing the KODS directory
    !
    ! OUTPUT
    ! returns T if the directory exists, F otherwise

    CHARACTER(len=*), INTENT(in) :: topdir
    CHARACTER(len=256)           :: testfile
    
#if defined( LINUX ) || defined( MACOSX )
    testfile = TRIM(topdir)//"/KODS/README_DATA"
#endif    

#if defined( WINDOWS )
    testfile = TRIM(topdir)//"\KODS\README_DATA"
#endif    

    INQUIRE(file = testfile, exist = kods_dir_exists)
    
  END FUNCTION kods_dir_exists


  !----------------------------------------------------------  
  LOGICAL FUNCTION file_exists( fname )

    ! A function the check for the existence of a file;
    ! makes the inquire statement easier to use in logical
    ! expressions

    CHARACTER(len=*), INTENT(in) :: fname

    INQUIRE(file = fname, exist = file_exists)
    
  END FUNCTION file_exists
  

  !----------------------------------------------------------
  SUBROUTINE sys_config_file(cfgfile)
    
    ! Returns the system wide config file name on any platform
    !
    ! OUTPUT
    ! cfgfile :  fully qualified name of the Koko configuration file

    CHARACTER(len=*), INTENT(out) :: cfgfile

# if defined( WINDOWS )
    INTEGER            :: status, length
    CHARACTER(len=256) :: programdata
#endif

#if defined( LINUX ) || defined( MACOSX )
    cfgfile = "/etc/kokorc"
#endif
#if defined( WINDOWS )
    CALL get_environment_variable("PROGRAMDATA", programdata, length, status)
    cfgfile = TRIM(programdata)//"\koko\kokorc"
#endif

  END SUBROUTINE sys_config_file


  !----------------------------------------------------------
  SUBROUTINE dir_path_append(fullpath, partpath, pathitem)

    ! A platform-independent way to append a directory to a path
    !
    ! INPUT
    ! partpath :  a path, possibly containing a drive designator
    ! pathitem :  name of a component to be appended to the path
    !
    ! OUTPUT
    ! fullpath :  the concatenated path name

    CHARACTER(len=*), INTENT(in)  :: partpath, pathitem
    CHARACTER(len=*), INTENT(out) :: fullpath

#if defined( LINUX ) || defined( MACOSX )
    fullpath = TRIM(partpath)//'/'//TRIM(pathitem)
#endif

#if defined( WINDOWS )
    fullpath = TRIM(partpath)//'\'//TRIM(pathitem)
#endif
    
  END SUBROUTINE dir_path_append


  !----------------------------------------------------------
  SUBROUTINE set_kods_temp_dir( tmpdir )

    ! Returns a directory for storing temporary files

    CHARACTER(len=*), INTENT(out) :: tmpdir
    CHARACTER(len=256)            :: tdir
    INTEGER                       :: length, status

    ! first check environment variables
    CALL get_environment_variable("TEMP", tdir, length, status)
    IF (status == 0) THEN
       tmpdir(1:length) = tdir(1:length)
    ELSE       
#if defined( LINUX ) || defined( MACOSX )
       tmpdir = "/tmp" ! fallback
#endif
    END IF

  END SUBROUTINE set_kods_temp_dir


  !----------------------------------------------------------
  SUBROUTINE add_dir_slash( dirname )

    ! adds a (back-) slash to a directory name

    CHARACTER(len=*), INTENT(inout) :: dirname
    CHARACTER                       :: last_char
    INTEGER                         :: lc

    lc = LEN_TRIM(dirname)
    last_char = dirname(lc:lc)
    
#if defined( LINUX ) || defined( MACOSX )
    IF (last_char /= '/') THEN
       dirname = TRIM(dirname)//'/'
    END IF
#endif
#if defined( WINDOWS )
    IF (last_char /= '\') THEN
       dirname = TRIM(dirname)//'\'
    END IF
#endif
    
  END SUBROUTINE add_dir_slash

  
  !----------------------------------------------------------
  SUBROUTINE os_delete( filename )

    ! deletes a file

    CHARACTER(len=*), INTENT(in) :: filename
    CHARACTER(len=4)             :: cmd

#if defined( LINUX ) || defined( MACOSX )
    cmd = "rm"
#endif
#if defined( WINDOWS )
    cmd = "del"
#endif

    IF ( file_exists(filename) ) THEN
       CALL shell_command( TRIM(cmd)//" "//TRIM(filename) )
    END IF

  END SUBROUTINE os_delete

  
  !----------------------------------------------------------
  SUBROUTINE os_copy( from_name, to_name )

    ! copies a file
    
    CHARACTER(len=*), INTENT(in) :: from_name, to_name
    CHARACTER(len=8)             :: cmd

#if defined( LINUX ) || defined( MACOSX )
    cmd = "cp"
#endif
#if defined( WINDOWS )
    cmd = "copy"
#endif
    
    IF ( file_exists(from_name) ) THEN
       CALL shell_command( TRIM(cmd)//" "//TRIM(from_name)//" "//TRIM(to_name) )
    ELSE
       WRITE (*,*) "File does not exist"
    END IF

  END SUBROUTINE os_copy

  
  !----------------------------------------------------------
  SUBROUTINE os_newdir( dir_name )

    ! creates a new directory
    
    CHARACTER(len=*), INTENT(in) :: dir_name
    CHARACTER(len=12)            :: cmd

#if defined( LINUX ) || defined( MACOSX )
    cmd = "mkdir -p"
#endif
#if defined( WINDOWS )
    cmd = "md"
#endif
    
    CALL shell_command( TRIM(cmd)//" "//TRIM(dir_name) )

  END SUBROUTINE os_newdir

  
  !----------------------------------------------------------
  SUBROUTINE os_listdir( dir_name )

    ! creates a new directory
    
    CHARACTER(len=*), INTENT(in) :: dir_name
    CHARACTER(len=8)             :: cmd

#if defined( LINUX ) || defined( MACOSX )
    cmd = "ls"
#endif
#if defined( WINDOWS )
    cmd = "dir"
#endif
    
    CALL shell_command( TRIM(cmd)//" "//TRIM(dir_name) )

  END SUBROUTINE os_listdir

  
  !----------------------------------------------------------
  SUBROUTINE shell_command( command )

    ! a simplified interface to executing a shell command. On unix
    ! systems the command is passed to 'sh', on Windows systems to
    ! 'cmd.exe'

    CHARACTER(len=*), INTENT(in) :: command
    INTEGER                      :: exitstat, cmdstat

    CALL execute_command_LINE(TRIM(command), .TRUE., exitstat, cmdstat)
    
  END SUBROUTINE shell_command
  
END MODULE opsys
