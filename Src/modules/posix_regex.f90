! The MIT License (MIT)
! 
! Copyright (c) 2016 Seiji Ueno
! 
! Permission is hereby granted, free of charge, to any person obtaining a copy
! of this software and associated documentation files (the "Software"), to deal
! in the Software without restriction, including without limitation the rights
! to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
! copies of the Software, and to permit persons to whom the Software is
! furnished to do so, subject to the following conditions:
! 
! The above copyright notice and this permission notice shall be included in all
! copies or substantial portions of the Software.
! 
! THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
! IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
! FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
! AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
! LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
! OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
! SOFTWARE.

module posix_regex

  ! This module provides access to the POSIX regular expression matching
  ! functions that are part of the GNU C library.
  
  use iso_c_binding
  implicit none

  integer, parameter :: REG_ICASE = 2
  integer, parameter :: REG_NEWLINE = 8
  integer, parameter :: REG_NOSUB = 4
  integer, parameter :: REG_NOTBOL = 1
  integer, parameter :: REG_NOTEOL = 2

  integer(c_size_t), parameter :: max_group = 256
  integer, parameter ::           ERROR_BUF_SIZE = 100
  include "regex_f90.inc"         ! defines REGEX_T_SIZE
  
  type, bind(C) :: regmatch_t
    integer(c_int64_t) :: rm_so
    integer(c_int64_t) :: rm_eo
  end type regmatch_t

  type :: regex_t
    byte, pointer :: buffer(:) => null()
    type(C_ptr) :: preg = C_NULL_ptr
  contains
    final :: dest
  end type regex_t

  type :: regex
    private
    character(:), allocatable :: target_string
    character(:), allocatable :: pattern_string
    type(regmatch_t), allocatable :: match_pos(:)
    type(regex_t) :: rem_data
    integer(c_size_t) :: match_number = max_group
    logical :: match_executed = .false.
    logical :: compiled = .false.
    logical :: compile_flags(4) = (/.true., .false., .false., .false./)
    integer :: compile_status = -1
    integer :: match_status = -1
  contains
    procedure :: compile => re_compile
    procedure :: set_nmatch => re_set_nmatch
    procedure :: match => re_match
    procedure :: group => re_group
    procedure :: matched => re_get_matched
    procedure :: print_status => re_print_status
    procedure :: get_comp_stat => re_get_comp_stat
    procedure :: get_match_stat => re_get_match_stat
    procedure :: get_error_message => re_get_error_message
  end type regex

  interface
    function C_regcomp(reg, pattern, flags) result(status) bind(C,name="regcomp")
      import
      type(C_ptr), value :: reg
      character(kind=C_char), intent(in) :: pattern(*)
      integer(c_int), intent(in), value :: flags
      integer(C_int) :: status
    end function C_regcomp
    
    function C_regexec(reg,string,nmatch,matches,flags) result(status) bind(C,name="regexec")
      import
      type(C_ptr), intent(in), value :: reg
      character(kind=C_char), intent(in) :: string(*)
      type(regmatch_t), intent(out) :: matches
      integer(C_size_t), intent(in), value :: nmatch
      integer(C_int), intent(in), value :: flags
      integer(C_int) :: status
    end function C_regexec
    
    function C_regerror(errcode, reg, errbuf, errbuf_size) result(regerror) bind(C,name="regerror")
      import
      integer(C_size_t) :: regerror
      integer(C_int), value :: errcode
      type(C_ptr), intent(in), value :: reg
      character(kind=C_char), intent(out) :: errbuf
      integer(C_size_t), value :: errbuf_size
    end function C_regerror
    
    subroutine C_regfree(reg) bind(C,name="regfree")
      import
      type(C_ptr), intent(in), value :: reg
    end subroutine C_regfree  
 end interface
 
 interface operator(.rem.)
    module procedure match_operator
 end interface operator(.rem.)
 
 interface assignment(=)
    module procedure regex_eq
 end interface assignment(=)
 
contains
  
  function re_compile(this, pattern, extended, ignore_case, no_sub, newline) &
&   result(status)
    class(regex),intent(inout) :: this
    character(len=*), intent(in) :: pattern
    logical, intent(in), optional :: extended, ignore_case, no_sub, newline
    integer :: status
    integer(C_int) :: flags_

    if(present(extended))    this%compile_flags(1) = extended
    if(present(ignore_case)) this%compile_flags(2) = ignore_case
    if(present(no_sub))      this%compile_flags(3) = no_sub
    if(present(newline))     this%compile_flags(4) = newline

    flags_ = 0
    if(this%compile_flags(1)) flags_ = 1
    if(this%compile_flags(2)) flags_ = or(flags_, int(REG_ICASE, kind=c_int))
    if(this%compile_flags(3)) flags_ = or(flags_, int(REG_NOSUB, kind=c_int))
    if(this%compile_flags(4)) flags_ = or(flags_, int(REG_NEWLINE, kind=c_int))

    call dest(this%rem_data)
    this%compiled = .false.

    allocate(this%rem_data%buffer(REGEX_T_SIZE))
    this%rem_data%preg = c_loc(this%rem_data%buffer(1))
    if(.not.allocated(this%match_pos))then
      allocate(this%match_pos(this%match_number))
    endif

    this%match_executed = .false.
    this%match_status = -1
    this%pattern_string = trim(pattern) // C_NULL_char
    status = C_regcomp(this%rem_data%preg, this%pattern_string, flags_)
    this%compile_status = status
    if (status/=0) return
    this%compiled = .true.
  end function re_compile

  subroutine re_set_nmatch(this, nmatch)
    class(regex), intent(inout) :: this
    integer, intent(in) :: nmatch
    if(allocated(this%match_pos)) deallocate(this%match_pos)
    allocate(this%match_pos(nmatch))
    this%match_number = nmatch
  end subroutine re_set_nmatch

  function re_match(this, string, notBOL, notEOL) result(status)
    class(regex), intent(inout) :: this
    character(len=*, kind=c_char), intent(in) :: string
    logical, intent(in), optional :: notBOL, notEOL
    integer :: status
    integer(c_int) :: flags_

    if(.not. this%compiled)then
      status = -1
      return
    endif
    flags_ = 1
    if(present(notBOL))then
      if(notBOL) flags_ = or(flags_, int(REG_NOTBOL, c_int))
    endif
    if(present(notEOL))then
      if(notEOL) flags_ = or(flags_, int(REG_NOTEOL, c_int))
    endif
    this%target_string = trim(string) // C_NULL_char
    status = C_regexec(this%rem_data%preg, this%target_string, this%match_number, this%match_pos(1), flags_)
    this%match_status = status
    this%match_executed = .true.
    this%target_string = trim(string)
  end function re_match

  function re_get_matched(this) result(r)
    class(regex), intent(in) :: this
    logical :: r
    r = this%match_status == 0
  end function re_get_matched

  function re_group(this, imatch) result(buf)
    class(regex), intent(in) :: this
    character(:), allocatable :: buf
    integer, intent(in) :: imatch
    integer :: i_s, i_e
    buf = ""
    if(.not. this%match_executed) return
    if(this%match_status /= 0) return
    if(imatch < 0 .or. imatch >= this%match_number) return
    i_s = int(this%match_pos(imatch + 1)%rm_so)
    i_e = int(this%match_pos(imatch + 1)%rm_eo)
    if(i_s < 0 .or. i_e < 0) return
    buf = this%target_string(i_s + 1:i_e)
  end function re_group

  function re_get_comp_stat(this) result(r)
    class(regex) :: this
    integer :: r
    r = this%compile_status
  end function re_get_comp_stat

  function re_get_match_stat(this) result(r)
    class(regex) :: this
    integer :: r
    r = this%match_status
  end function re_get_match_stat

  function re_get_error_message(this, stat) result(msg)
    class(regex) :: this
    integer,intent(in) :: stat
    integer(c_int) :: stat_
    character(len=:),allocatable :: msg
    integer(c_size_t) :: errorbuf_len, regerror
    character(kind=c_char, len=ERROR_BUF_SIZE) :: errorbuf
    stat_ = stat
    errorbuf_len = len(errorbuf)
    regerror = C_regerror(stat_, this%rem_data%preg, errorbuf, errorbuf_len)
    msg = errorbuf(1:index(errorbuf, C_NULL_char))
  end function re_get_error_message

  subroutine re_print_status(this)
    class(regex) :: this
    character(len=:), allocatable :: msg
    if(this%compile_status == -1) then
      print *, "Pattern has not been yet compiled"
      return
    endif
    if(.not. this%compiled)then
      msg = re_get_error_message(this, this%compile_status)
      print *, "Error message for compile: ", msg
      return
    endif
    print *, "Pattern compilation has been successfully done."

    if(.not. this%match_executed)then
      print *, "Regular expression match has not been yet done."
      return
    endif
    if(this%match_status > 0)then
      msg = this%get_error_message(this%match_status)
      print *, "Error message for match: ", msg
      return
    endif
    print *, "Regular expression match has been successfully done."
  end subroutine re_print_status

  subroutine dest(this)
    type(regex_t), intent(inout) :: this
    if(associated(this%buffer)) then
      call C_regfree(this%preg)
      deallocate(this%buffer)
    endif
  end subroutine dest

  subroutine regex_eq(u, v)
    class(regex), intent(inout) :: u
    class(regex), intent(in) :: v
    integer :: status
    call dest(u%rem_data)
    if(v%compiled) then
      u%compile_flags = v%compile_flags
      status = u%compile(v%pattern_string)
    endif
    if(v%match_executed) then
      u%match_executed = .true.
      u%match_status = v%match_status
      u%match_pos = v%match_pos
      u%target_string = v%target_string
    endif
  end subroutine regex_eq

  function match_operator(str, re) result(r)
    character(len=*), intent(in) :: str, re
    type(regex) :: r
    integer :: status
    status = r%compile(re)
    status = r%match(str)
  end function match_operator
  
end module posix_regex
