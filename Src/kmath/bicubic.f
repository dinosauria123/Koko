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

      SUBROUTINE BICUBIC(X1,X2,X3,X4,Y1,Y2,F1,F2,F3,F4,X,Y,F)
          IMPLICIT NONE
          REAL*8 X1,X2,X3,X4,Y1,Y2,F1,F2,F3,F4,X,Y,F
          REAL*8 M1X,M2X,FX1,FX2,MY
          
C       X DIRECTION
          M1X=(F2-F1)/(X2-X1)
          M2X=(F4-F3)/(X4-X3)
          FX1=((M1X)*(X-X1))+F1
          FX2=((M2X)*(X-X3))+F3
          MY=(FX2-FX1)/(Y2-Y1)
          F=(MY)*(Y-Y1)+FX1
          
          RETURN
      END
