! MIT License
!
! Copyright (c) 2019 Takuma Yoshida
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

#define keytype1 character(:),allocatable
#define keytype2 character(*)
#define valtype integer

module dictionary

  ! High level wrapper of dictionary data structure

  use treap, only: node, my_count, insert, erase, find_node, kth_node, delete_all, inorder

  implicit none

  private
  public :: dict, get_val, insert_or_assign, exists, remove, get_keys_vals, get_size, get_kth_key

  type dict
    type(node), pointer :: root => null()
    integer :: randstate = 1231767121
    contains
    final :: destruct_dict
  end type dict

  contains

  pure function xorshift32(i)
    implicit none
    integer(4), intent(in) :: i
    integer(4) :: xorshift32
    if (i == 0) then
      xorshift32 = 1231767121
    else
      xorshift32 = i
    end if
    xorshift32 = ieor(xorshift32, ishft(xorshift32, 13))
    xorshift32 = ieor(xorshift32, ishft(xorshift32, -17))
    xorshift32 = ieor(xorshift32, ishft(xorshift32, 15))
  end function xorshift32

  function get_val(t, key)
    implicit none
    type(dict), intent(in) :: t
    keytype2, intent(in) :: key
    type(node), pointer :: nd
    valtype :: get_val
    nd => find_node(t%root, key)
    if (.not. associated(nd)) then
      stop 105
    end if
    get_val = nd%val
  end function get_val

  function exists(t, key)
    implicit none
    type(dict), intent(in) :: t
    keytype2, intent(in) :: key
    type(node), pointer :: nd
    logical :: exists
    nd => find_node(t%root, key)
    exists = (associated(nd))
  end function exists

  subroutine insert_or_assign(t, key, val)
    implicit none
    type(dict), intent(inout) :: t
    keytype2, intent(in) :: key
    valtype, intent(in) :: val
    type(node), pointer :: nd
    nd => find_node(t%root, key)
    if (associated(nd)) then
      nd%val = val
    else  ! This implementation is not optimal
      t%root => insert(t%root, key, val, t%randstate)
      t%randstate = xorshift32(t%randstate)
    end if
  end subroutine insert_or_assign

  subroutine remove(t, key)
    implicit none
    type(dict), intent(inout) :: t
    keytype2, intent(in) :: key
    t%root => erase(t%root, key)
  end subroutine remove

  function get_kth_key(t, k)
    implicit none
    type(dict), intent(in) :: t
    integer, intent(in) :: k
    type(node), pointer :: res
    keytype1 :: get_kth_key
    if (k < 1 .or. k > my_count(t%root)) then
      print *, "get_kth_key failed"
      stop 2
    else
      res => kth_node(t%root, k)
      get_kth_key = res%key
    end if
  end function get_kth_key

  subroutine get_keys_vals(t, keys, vals, n)
    implicit none
    type(dict), intent(in) :: t
    integer, intent(in) :: n
    keytype2, intent(out) :: keys(n)
    valtype, intent(out) :: vals(n)
    integer :: counter
    if (my_count(t%root) /= n) stop 5
    counter = 0
    call inorder(t%root, keys, vals, counter)
  end subroutine get_keys_vals

  function get_size(t)
    implicit none
    type(dict), intent(in) :: t
    integer :: get_size
    get_size = my_count(t%root)
  end function get_size

  subroutine destruct_dict(t)
    implicit none
    type(dict), intent(inout) :: t
    call delete_all(t%root)
  end subroutine destruct_dict

end module dictionary
