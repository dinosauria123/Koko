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

C       THIS IS THE CONFIGS INCLUDE FILE SET FOR 10 CONFIGS
C       EACH CONFIG WITH 2000 ENTRIES
C
      CHARACTER CONFG(2:75,1:2000)*140,EE12*140,HOLDER*140
C
      INTEGER CFGCNT(2:75),MAXCFG
C
      COMMON/MXCFG/MAXCFG
C
      COMMON/CFG/CFGCNT
C
      COMMON/CCFG/CONFG,EE12,HOLDER
C
      INTEGER SPECF2(2:75,0:499),SPECFF(2:75,0:499)
C
      COMMON/SPSPCN/SPECFF,SPECF2
C
