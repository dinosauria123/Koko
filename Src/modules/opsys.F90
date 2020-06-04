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
    
    CHARACTER(len=*), INTENT(OUT) :: homedir
    LOGICAL, INTENT(OUT)          :: has_homedir
    
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
  !
  LOGICAL FUNCTION kods_dir_exists( topdir )

    CHARACTER(len=*), INTENT(IN) :: topdir
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
  ! A function the check for the existence of a file;
  ! makes the inquire statement easier to use in logical
  ! expressions
  !
  LOGICAL FUNCTION file_exists( fname )

    CHARACTER(len=*), INTENT(IN) :: fname

    INQUIRE(file = fname, exist = file_exists)
    
  END FUNCTION file_exists
  

  !----------------------------------------------------------
  ! Returns the system wide config file name on any platform
  !
  ! OUTPUT
  ! cfgfile :  fully qualified name of the Koko configuration file
  !
  SUBROUTINE sys_config_file(cfgfile)
    
    CHARACTER(len=*), INTENT(OUT) :: cfgfile

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
  ! A platform-independent way to append a directory to a path
  !
  ! INPUT
  ! partpath :  a path, possibly containing a drive designator
  ! pathitem :  name of a component to be appended to the path
  !
  ! OUTPUT
  ! fullpath :  the concatenated path name
  !
  SUBROUTINE dir_path_append(partpath, pathitem, fullpath)

    CHARACTER(len=*) :: partpath, pathitem, fullpath
    INTEGER          :: lc

    ! last character
    lc = LEN_TRIM(partpath)
    
#if defined( LINUX ) || defined( MACOSX )
    IF (partpath(lc:lc) == '/') THEN
       fullpath = TRIM(partpath)//TRIM(pathitem)
    ELSE
       fullpath = TRIM(partpath)//'/'//TRIM(pathitem)
    END IF
#endif

#if defined( WINDOWS )
    IF (partpath(lc:lc) == '\') THEN
       fullpath = TRIM(partpath)//TRIM(pathitem)
    ELSE
       fullpath = TRIM(partpath)//'\'//TRIM(pathitem)
    END IF
#endif
    
  END SUBROUTINE dir_path_append


  !----------------------------------------------------------
  ! Returns a directory for storing temporary files. The
  ! subroutine first reads the contents of the environment
  ! variable TEMP. If it is empty, a default value is returned.
  !
  ! INPUT
  ! tmpdir :  a string with the directory name
  !
  SUBROUTINE set_kods_temp_dir( tmpdir )

    CHARACTER(len=*), INTENT(OUT) :: tmpdir
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
  ! adds a (back-) slash to a directory name
  !
  SUBROUTINE add_dir_slash( dirname )

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
  ! deletes a file
  !
  SUBROUTINE os_delete( filename )

    CHARACTER(len=*), INTENT(IN) :: filename
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
  ! Copies a file
  !
  SUBROUTINE os_copy( from_name, to_name )

    CHARACTER(len=*), INTENT(IN) :: from_name, to_name
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
  ! Appends two files at the OS level
  !
  ! Unix:    cat file_a file_b > file_out 
  ! Windows: type file_a file_b > file_out
  !
  SUBROUTINE append_files( file_a, file_b, file_out )

    CHARACTER(len=*), INTENT(IN) :: file_a, file_b, file_out
    CHARACTER(len=8)             :: cmd

#if defined( LINUX ) || defined( MACOSX )
    cmd = "cat"
#endif
#if defined( WINDOWS )
    cmd = "type"
#endif

    CALL shell_command(TRIM(cmd)//" "//TRIM(file_a)//" "//TRIM(file_b)//" > "//TRIM(file_out))

  END SUBROUTINE append_files


  !----------------------------------------------------------
  ! Creates a new directory
  !
  SUBROUTINE os_newdir( dir_name )

    CHARACTER(len=*), INTENT(IN) :: dir_name
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
  ! Lists the contents of a directory
  !
  SUBROUTINE os_listdir( dir_name )

    CHARACTER(len=*), INTENT(IN) :: dir_name
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
  ! a simplified interface to executing a shell command. On unix
  ! systems the command is passed to 'sh', on Windows systems to
  ! 'cmd.exe'
  !
  SUBROUTINE shell_command( command )

    CHARACTER(len=*), INTENT(IN) :: command
    INTEGER                      :: exitstat, cmdstat

    CALL execute_command_LINE(TRIM(command), .TRUE., exitstat, cmdstat)
    
  END SUBROUTINE shell_command
  
END MODULE opsys
