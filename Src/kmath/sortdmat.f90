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
      
subroutine sortdmat(A, nr, nc, ic, ier)
  !
  ! Sorts a selected column of a matrix in ascending or descending
  ! order and applies the same permutation to the other columns. The
  ! subroutine is based on the SLATEC subroutine 'dpsort'.
  !
  ! INPUT
  ! A  :  a 2-dimensional double precision matrix
  !       with nr number of rows and nc number of columns
  ! ic :  the index of a matrix column. A(:,ic) are sorted in ascending
  !       order when ic > 0, in descending order when ic < 0.
  ! ier : > 0 when an error occurred
  !
  ! OUTPUT
  ! A  :  contains the sorted matrix on return
  !
  ! Ulf Griesmann, April 2020

  implicit none

  integer, parameter :: dp = kind(1.0d0)
  
  real (dp), intent(inout) :: A(nr,*)
  integer, intent(in)      :: nr, nc, ic
  integer, intent(out)     :: ier
  
  real (dp), allocatable :: C(:)
  integer, allocatable   :: iperm(:)
  integer                :: m, n, kflag

  ! sanity check for input
  if ((ic == 0) .or. (abs(ic) > nc)) then
     ier = 1
     return
  end if

  allocate(iperm(nr))  ! index array
  allocate(C(nr))      ! matrix column workspace
  
  ! permutation needed to sort the selected column
  kflag = sign(1,ic)
  call dpsort(A(:,abs(ic)), nr, iperm, kflag, ier)
  if (ier > 0) then
     return
  end if
  
  ! apply permutation to the columns of A
  do n=1,abs(nc)
     C = A(:,n)
     do m=1,nr
        A(m,n) = C(iperm(m))
     end do
  end do

end subroutine sortdmat
