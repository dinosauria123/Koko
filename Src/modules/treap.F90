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

module treap

  ! Low level data structure and operations of treap.
  ! This allows multiple nodes with a same key.

  implicit none

  type node
    type(node), pointer :: left => null(), right => null()
    keytype1 :: key
    valtype :: val
    integer :: pri  ! min-heap
    integer :: cnt = 1
  end type node

  contains

  subroutine update(root)
    implicit none
    type(node), pointer, intent(in) :: root
    root%cnt = my_count(root%left) + my_count(root%right) + 1
  end subroutine update

  function my_count(root)
    implicit none
    type(node), pointer, intent(in) :: root
    integer :: my_count
    if (associated(root)) then
      my_count = root%cnt
    else
      my_count = 0
    end if
  end function my_count

  function rotate_ccw(root)
    implicit none
    type(node), pointer, intent(in) :: root
    type(node), pointer :: tmp, rotate_ccw
    if (.not. associated(root%right)) stop 1
    tmp => root%right
    root%right => tmp%left
    tmp%left => root
    rotate_ccw => tmp
    call update(root)
    call update(tmp)
  end function rotate_ccw

  function rotate_cw(root)
    implicit none
    type(node), pointer, intent(in) :: root
    type(node), pointer :: tmp, rotate_cw
    if (.not. associated(root%left)) stop 1
    tmp => root%left
    root%left => tmp%right
    tmp%right => root
    rotate_cw => tmp
    call update(root)
    call update(tmp)
  end function rotate_cw

  recursive function insert(root, key, val, pri) result(res)
    implicit none
    type(node), pointer, intent(in) :: root
    integer, intent(in) :: pri
    keytype2, intent(in) :: key
    valtype, intent(in) :: val
    type(node), pointer :: res

    if (.not. associated(root)) then
      allocate(res)
      res%key = key
      res%pri = pri
      res%val = val
    else
      res => root
      if (key > root%key) then
        root%right => insert(root%right, key, val, pri)
        call update(root)
        if (root%pri > root%right%pri) then
          res => rotate_ccw(res)
        end if
      else
        root%left => insert(root%left, key, val, pri)
        call update(root)
        if (root%pri > root%left%pri) then
          res => rotate_cw(res)
        end if
      end if
    end if
  end function insert

  recursive function erase(root, key) result(res)
    implicit none
    type(node), pointer, intent(in) :: root
    keytype2, intent(in) :: key
    type(node), pointer :: res, tmp

    if (.not. associated(root)) then
      print *, "Erase failed"
      stop 1
    end if

    if (key < root%key) then
      root%left => erase(root%left, key)
      res => root
    else if (key > root%key) then
      root%right => erase(root%right, key)
      res => root
    else
      if ((.not. associated(root%left)) .or. (.not. associated(root%right))) then
        tmp => root
        if (.not. associated(root%left)) then
          res => root%right
        else
          res => root%left
        end if
        deallocate(tmp)
      else
        if (root%left%pri < root%right%pri) then
          res => rotate_ccw(root)
          res%left => erase(res%left, key)
        else
          res => rotate_cw(root)
          res%right => erase(res%right, key)
        end if
      end if
    end if
    if (associated(res)) call update(res)
  end function erase

  recursive function find_node(root, key) result(res)
    implicit none
    type(node), pointer, intent(in) :: root
    keytype2, intent(in) :: key
    type(node), pointer :: res
    if (.not. associated(root)) then
      res => null()
    else if (root%key == key) then
      res => root
    else if (key < root%key) then
      res => find_node(root%left, key)
    else
      res => find_node(root%right, key)
    end if
  end function find_node

  recursive function kth_node(root, k) result(res)
    implicit none
    type(node), pointer, intent(in) :: root
    integer, intent(in) :: k
    type(node), pointer :: res
    if (.not. associated(root)) then
      res => null()
    else if (k <= my_count(root%left)) then
      res => kth_node(root%left, k)
    else if (k == my_count(root%left) + 1) then
      res => root
    else
      res => kth_node(root%right, k - my_count(root%left) - 1)
    end if
  end function kth_node

  recursive subroutine delete_all(root)
    implicit none
    type(node), pointer, intent(inout) :: root
    if (.not. associated(root)) return

    call delete_all(root%left)
    call delete_all(root%right)
    deallocate(root)
    nullify(root)
  end subroutine delete_all

  recursive subroutine inorder(root, keys, vals, counter)
    implicit none
    type(node), pointer, intent(in) :: root
    keytype2, intent(inout) :: keys(:)
    valtype, intent(inout) :: vals(:)
    integer, intent(inout) :: counter
    if (.not. associated(root)) return

    call inorder(root%left, keys, vals, counter)
    counter = counter + 1
    keys(counter) = root%key
    vals(counter) = root%val
    call inorder(root%right, keys, vals, counter)
  end subroutine inorder
  
end module treap
