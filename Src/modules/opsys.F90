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
    INTEGER          :: lc, i
    CHARACTER(LEN(partpath))   :: ppath
    CHARACTER(LEN(pathitem))   :: pitem

    ! The config parser may store values as NUL-terminated C strings,
    ! and gfortran's TRIM() does not strip NUL (ASCII 0). Copy through
    ! the first NUL (or the full length) for both arguments so a NUL
    ! block never propagates into the joined path (which would corrupt
    ! file names such as "/home/.../KODS\0\0.../KOBJ.BMP").
    i = INDEX(partpath, ACHAR(0))
    IF (i <= 1) i = LEN(partpath) + 1
    ppath = partpath(1:i-1)
    i = INDEX(pathitem, ACHAR(0))
    IF (i <= 1) i = LEN(pathitem) + 1
    pitem = pathitem(1:i-1)

    ! last character
    lc = LEN_TRIM(ppath)
    
#if defined( LINUX ) || defined( MACOSX )
    IF (ppath(lc:lc) == '/') THEN
       fullpath = TRIM(ppath)//TRIM(pitem)
    ELSE
       fullpath = TRIM(ppath)//'/'//TRIM(pitem)
    END IF
#endif

#if defined( WINDOWS )
    IF (ppath(lc:lc) == '\') THEN
       fullpath = TRIM(ppath)//TRIM(pitem)
    ELSE
       fullpath = TRIM(ppath)//'\'//TRIM(pitem)
    END IF
#endif
    
  END SUBROUTINE dir_path_append

  !----------------------------------------------------------
  ! Strip any trailing directory separator ('/' or '\') and
  ! trailing blanks from a path string in place. Prevents
  ! downstream path joins like TRIM(path)//'/'//name from
  ! producing a double separator (e.g. "//name") when the
  ! config parser leaves a trailing '/' on a directory value.
  !
  ! INPUT/OUTPUT
  ! path :  the path string to normalize (modified in place)
  !
  SUBROUTINE strip_trailing_sep(path)

    CHARACTER(len=*) :: path
    INTEGER          :: lc, i, inul

    ! CFG_get may return the value as a NUL-terminated C string padded
    ! out to the full CHARACTER*N length, and gfortran's TRIM() only
    ! strips ASCII blanks, NOT NUL (ASCII 0). Worse, a previous full path
    ! value can remain past the NUL block (e.g. ".../KODS\0\0.../KOBJ.BMP").
    ! If a NUL is present, blank from there to the end (truncating any
    ! stray tail), then strip trailing separators/blanks.
    inul = INDEX(path, ACHAR(0))
    IF (inul > 0) THEN
       DO i = inul, LEN(path)
          path(i:i) = ' '
       END DO
    END IF

    lc = LEN_TRIM(path)
    DO WHILE (lc > 0)
       IF (path(lc:lc) == '/' .OR. path(lc:lc) == '\' &
            .OR. path(lc:lc) == ' ') THEN
          path(lc:lc) = ' '
          lc = lc - 1
       ELSE
          EXIT
       END IF
    END DO

  END SUBROUTINE strip_trailing_sep


  !----------------------------------------------------------
  ! Return a clean copy of a path string. gfortran's TRIM() does
  ! not stop at NUL (ASCII 0) and HOME is a fixed-length COMMON
  ! field whose tail can hold stale garbage (e.g. a previously
  ! built "/home/.../KODS/KOBJ.BMP"). TRIM() would otherwise drag
  ! that tail into path joins and produce unopenable filenames
  ! (symptom: "Cannot open file '/home/dino/KODS<sp>...'").
  !
  ! Truncate at the first NUL or space (a KODS directory path
  ! contains neither) and strip a trailing separator/blank.
  !
  FUNCTION clean_path(p) RESULT(res)
    CHARACTER(len=*), INTENT(IN) :: p
    CHARACTER(LEN(p))            :: res
    INTEGER                      :: i, lc
    res = p
    i = INDEX(res, ACHAR(0))
    IF (i > 1) res = res(1:i-1)
    i = INDEX(res, ' ')
    IF (i > 1) res = res(1:i-1)
    lc = LEN_TRIM(res)
    DO WHILE (lc > 0)
       IF (res(lc:lc) == '/' .OR. res(lc:lc) == '\' &
            .OR. res(lc:lc) == ' ') THEN
          res(lc:lc) = ' '
          lc = lc - 1
       ELSE
          EXIT
       END IF
    END DO
  END FUNCTION clean_path
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
  ! Appends one file to another IN PLACE (file_out may already
  ! exist and must not be truncated first).
  !
  ! Unix:    cat file_b >> file_out
  ! Windows: type file_b >> file_out
  !
  ! NOTE: append_files(a, b, out) with out == a is BROKEN: the
  ! shell truncates the redirect target before cat reads it.
  !
  SUBROUTINE append_file_to( file_b, file_out )

    CHARACTER(len=*), INTENT(IN) :: file_b, file_out
    CHARACTER(len=8)             :: cmd

#if defined( LINUX ) || defined( MACOSX )
    cmd = "cat"
#endif
#if defined( WINDOWS )
    cmd = "type"
#endif

    CALL shell_command(TRIM(cmd)//" "//TRIM(file_b)//" >> "//TRIM(file_out))

  END SUBROUTINE append_file_to


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

#if defined(WINDOWS)
 subroutine replace_slash(path)

    CHARACTER(len=*) :: path
    INTEGER          :: lc, i
    i = 1
    ! last character
    lc = LEN_TRIM(path)
	
	do while (i .le. lc)
	    if (path(i:i) == "\") then
		    path(i:i) = "/"
	    end if
		i=i+1
    end do

  end subroutine replace_slash
#endif
 