C///////////////////////////////////////////////////////////////////////
C/
C/ Copyright (C) 2020 The Koko Project Developers
C/
C/ See the file COPYRIGHT.md in the top-level directory of this
C/ distribution
C/
C/ This file is part of Koko.
C/
C/ Koko is free software: you can redistribute it and/or modify it
C/ under the terms of the GNU General Public License as published by
C/ the Free Software Foundation, either version 3 of the License, or
C/ (at your option) any later version.
C/
C/ Koko is distributed in the hope that it will be useful, but
C/ WITHOUT ANY WARRANTY; without even the implied warranty of
C/ MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
C/ GNU General Public License for more details.
C/
C/ You should have received a copy of the GNU General Public License
C/ along with Koko; see the file COPYING.  If not, see
C/ <https://www.gnu.org/licenses/>.
C/
C///////////////////////////////////////////////////////////////////////

      SUBROUTINE sort(n,arr)
          INTEGER n,M,NSTACK
          REAL arr(n)
          PARAMETER (M=7,NSTACK=50)
          INTEGER i,ir,j,jstack,k,l,istack(NSTACK)
          REAL a,temp
          INCLUDE "datmai.inc"

          jstack=0
          l=1
          ir=n
1         if(ir-l.lt.M)then
              do 12 j=l+1,ir
                  a=arr(j)
                  do 11 i=j-1,1,-1
                      if(arr(i).le.a)goto 2
                      arr(i+1)=arr(i)
11                continue
                  i=0
2                 arr(i+1)=a
12            continue
              if(jstack.eq.0)return
              ir=istack(jstack)
              l=istack(jstack-1)
              jstack=jstack-2
          else
              k=(l+ir)/2
              temp=arr(k)
              arr(k)=arr(l+1)
              arr(l+1)=temp
              if(arr(l+1).gt.arr(ir))then
                  temp=arr(l+1)
                  arr(l+1)=arr(ir)
                  arr(ir)=temp
              endif
              if(arr(l).gt.arr(ir))then
                  temp=arr(l)
                  arr(l)=arr(ir)
                  arr(ir)=temp
              endif
              if(arr(l+1).gt.arr(l))then
                  temp=arr(l+1)
                  arr(l+1)=arr(l)
                  arr(l)=temp
              endif
              i=l+1
              j=ir
              a=arr(l)
3             continue
              i=i+1
              if(arr(i).lt.a)goto 3
4             continue
              j=j-1
              if(arr(j).gt.a)goto 4
              if(j.lt.i)goto 5
              temp=arr(i)
              arr(i)=arr(j)
              arr(j)=temp
              goto 3
5             arr(l)=arr(j)
              arr(j)=a
              jstack=jstack+2

              if(jstack.gt.NSTACK) THEN
                  WRITE(OUTLYNE,*) 'NSTACK too small in sort'
              END IF

              if(ir-i+1.ge.j-l)then
                  istack(jstack)=ir
                  istack(jstack-1)=i
                  ir=j-1
              else
                  istack(jstack)=j-1
                  istack(jstack-1)=l
                  l=i
              endif
          endif
          goto 1
      END
C  (C) Copr. 1986-92 Numerical Recipes Software *)-+ZV.
