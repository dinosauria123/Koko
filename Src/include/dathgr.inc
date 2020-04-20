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

C     INCLUDE FILE FOR ROUTINES THAT CALL HGRAPH
      INTEGER PENPOSX,PENPOSY,NEUTTOTAL
      COMMON/TOTALNEUT/NEUTTOTAL
      COMMON/PENPEN2/PENPOSX,PENPOSY
      LOGICAL EXIS27,OPEN27
      LOGICAL EXIS28,OPEN28
      COMMON/TWENTY7/OPEN27,EXIS27
      COMMON/TWENTY8/OPEN28,EXIS28
      CHARACTER OUTNEUT*80
      COMMON/NEUTOUT/OUTNEUT
      LOGICAL NODRAW,NOWMF
      COMMON/DRAWNO/NODRAW,NOWMF
      LOGICAL DOENDPLT
      COMMON/ENDPLOTTER/DOENDPLT
      REAL*8 POLDX,POLDY,POLDZ,PCURX,PCURY,PCURZ,PNEWX,PNEWY,PNEWZ
      COMMON/PENMOVER/POLDX,POLDY,POLDZ,PCURX,PCURY,PCURZ,PNEWX,
     1 PNEWY,PNEWZ
C
C     USER DEFINED FAN AXIS PLOTS
      INTEGER FANAXX,FANAXY
      COMMON/AXISFANS/FANAXX,FANAXY
C
C     SIZE OF NETARRAY
      INTEGER MAXNEUTRAL
      COMMON/NEUTMAX/MAXNEUTRAL
C
      LOGICAL NEUTFILE
      COMMON/FILENEUT/NEUTFILE
C
      CHARACTER PLOTNOTE*80
      COMMON/NOTEPLOT/PLOTNOTE
C
C       RANGE OF USER-DEFINED PLOTS
      INTEGER UIXSTART,UIXSTOP,UIYSTART,UIYSTOP
      COMMON/USERSTARTSTOP/UIXSTART,UIXSTOP,UIYSTART,UIYSTOP
C
C       X AND Y NUMBER OF TICKS
      REAL UUDX,UUDY
      COMMON/USERXYDF/UUDX,UUDY
C
