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

module opsys

  ! Provides platform-independent access to operating system services
  !
  ! Ulf GRIESMANN, May 2020

contains

  subroutine user_home_directory( has_homedir, homedir )

    ! returns the home directory of the user running Koko
    !
    ! OUTPUT
    ! has_homedir :  returns T if the user has a home directory
    ! homedir :      the fully qualified path of the home directory,
    !                including, in Windows, the home drive.
    
    character(len=*), intent(out) :: homedir
    logical, intent(out)          :: has_homedir
    
    integer            :: status, length

# if defined( WINDOWS )
    character(len=4)   :: homedrive
    character(len=256) :: homepath
#endif
    
#if defined( LINUX ) || defined( MACOSX )
    call get_environment_variable("HOME", homedir, length, status)
    if (status .ne. 0) then
       homedir = " "
       has_homedir = .false.
    else
       has_homedir = .true.
    end if
#endif

#if defined( WINDOWS )
    call get_environment_variable("HOMEDRIVE", homedrive, length, status)
    if (status .ne. 0) then
       has_homedir = .false.
       return
    end if
    
    call get_environment_variable("HOMEPATH", homepath, length, status)
    if (status .ne. 0) then
       has_homedir = .false.
       return
    end if

    homedir = trim(homedrive)//trim(homepath)
    has_homedir = .true.
#endif
    
  end subroutine user_home_directory


  logical function kods_dir_exists( topdir )

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

    character(len=*), intent(in) :: topdir
    character(len=256)           :: testfile
    
#if defined( LINUX ) || defined( MACOSX )
    testfile = trim(topdir)//"/KODS/README_DATA"
#endif    

#if defined( WINDOWS )
    testfile = trim(topdir)//"\KODS\README_DATA"
#endif    

    inquire(file = testfile, exist = kods_dir_exists)
    
  end function kods_dir_exists


  logical function file_exists( fname )

    ! A function the check for the existence of a file;
    ! makes the inquire statement easier to use in logical
    ! expressions

    character(len=*), intent(in) :: fname

    inquire(file = fname, exist = file_exists)
    
  end function file_exists
  

  subroutine dir_path_append(fullpath, partpath, pathitem)

    ! A platform-independent way to append a directory to a path
    !
    ! INPUT
    ! partpath :  a path, possibly containing a drive designator
    ! pathitem :  name of a component to be appended to the path
    !
    ! OUTPUT
    ! fullpath :  the concatenated path name

    character(len=*), intent(in)  :: partpath, pathitem
    character(len=*), intent(out) :: fullpath

#if defined( LINUX ) || defined( MACOSX )
    fullpath = trim(partpath)//"/"//trim(pathitem)
#endif

#if defined( WINDOWS )
    fullpath = trim(partpath)//"\\"//trim(pathitem)
#endif
    
  end subroutine dir_path_append


  subroutine set_kods_temp_dir( tmpdir )

    ! Returns a directory for storing temporary files

    character(len=*), intent(out) :: tmpdir
    character(len=256)            :: tdir
    integer                       :: length, status

    ! first check environment variables
    call get_environment_variable("TEMP", tdir, length, status)
    if (status == 0) then
       tmpdir(1:length) = tdir(1:length)
    else       
#if defined( LINUX ) || defined( MACOSX )
       tmpdir = "/tmp" ! fallback
#endif
    end if

  end subroutine set_kods_temp_dir


  subroutine add_dir_slash( dirname )

    ! adds a (back-) slash to a directory name

    character(len=*), intent(inout) :: dirname

#if defined( LINUX ) || defined( MACOSX )
    dirname = trim(dirname)//"/"
#endif
#if defined( WINDOWS )
    dirname = trim(dirname)//"\\"
#endif
    
  end subroutine add_dir_slash
  
end module opsys
