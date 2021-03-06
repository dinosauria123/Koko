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

C       MACRO SPECIFIC INCLUDE FILE (SET FOR 999, 512 LINE MACROS)
C
      CHARACTER MACCW(0:1023)*8,MACQW(0:1023)*8,
     1MACSTR(0:1023)*80
C
      CHARACTER FUNCW(1:10,0:1023)*8,FUNQW(1:10,0:1023)*8,
     1FUNSTR(1:10,0:1023)*80,FNAMED(1:10)*8
C
      CHARACTER MAC_COM1*40
      COMMON/COM1_MAC/MAC_COM1
C
      COMMON/FFNNAA/FNAMED
C
C       IF AN ELEMENT IS TRUE, THE FUNCTION EXISTS, ELSE IT DOES NOT
C
C       NESFUN=TRUE IS A FUNCTION IS RUNNING AND FALSE IF A MACRO
C       IS RUNNING AT A PARTICULAR NESTING LEVEL.
      LOGICAL FUNEXT(0:10),NESFUN(0:10)
C
      COMMON/NSFUNC/NESFUN
C
      COMMON/FNEXIS/FUNEXT
C
      COMMON/FUN1/FUNCW,FUNQW,FUNSTR
C
      INTEGER MACSTA(1:20,0:1023),MAXLIN,FUNSTA(1:10,1:20,0:1023)
C
      COMMON/MXLIN/MAXLIN
C
      REAL*8 MACNW(1:5,0:1023),FUNNW(1:10,1:5,0:1023)
C
      COMMON/FUN2/FUNNW,FUNSTA
C
      COMMON/MACRO1/MACCW,MACQW,MACSTR
C
      COMMON/MACRO2/MACNW,MACSTA
C
C       MACRO DIRECTORY DATA
C
      INTEGER OLDIJ,CURLIN,TF(0:20),SSTEP(0:20)
C
      COMMON/TEEFF/TF,SSTEP
C
      COMMON/MCPAS2/OLDIJ,CURLIN
C
      CHARACTER MCDIR1(1:999)*8,MCDIR3(1:999)*20
     1,FCDIR1(1:10)*8,FCDIR3(1:10)*20
C
      INTEGER MCDIR2(1:3,1:999),FCDIR2(1:3,1:10)
C
      COMMON/DIRMC1/MCDIR1,MCDIR3,FCDIR1,FCDIR3
C
      COMMON/DIRMC2/MCDIR2,FCDIR2
C
      CHARACTER ASORT(1:999)*8,BSORT(1:999)*8
      CHARACTER AISORT(1:999)*80,BISORT(1:999)*80
C
      COMMON/JK_ASRT1/ASORT,BSORT,AISORT,BISORT
C
C       MAXMAC IS THE MAXIMUM NUMBER OF MACROS IN THE MACRO DIRECTORY
C       THE VALUE IS SET IN PROGRAM.FOR
C
      INTEGER MAXMAC
C
      COMMON/MXMAC/MAXMAC
C
      CHARACTER MWQ(0:20)*8,MWS(0:20)*80,FMWQ*8,FMWS*80
C
      COMMON/MACWDS/MWQ,MWS,FMWQ,FMWS
C
      INTEGER MDF(0:20,1:5),MSQ(0:20),FMSQ,MST(0:20),FMST
C
      REAL*8 MNW(0:20,1:5),FMNW(1:5),REG9(1:10),FMDF(1:5)
C
      COMMON/REGINALD/REG9
C
      COMMON/MCNW/MNW,MDF,MSQ,FMNW,FMDF,FMSQ,MST,FMST
C
      LOGICAL NOEXTRACT,NOMEDIT,ORITEM
C
      COMMON/OWRITE/ORITEM
C
      COMMON/EXTRACTOR/NOEXTRACT,NOMEDIT
C
C     CONTROLS OUTPUT MESSAGE DISPLAY IN MMDEL.FOR

!      LOGICAL MACSILENT

!      SAVE MACSILENT
C
C
      LOGICAL RESOLVEIT
      COMMON/ITRESOLVE/RESOLVEIT
C
      INTEGER NESTIJ(0:20),NESTI(0:20),NEST,NESTER
C
      COMMON/MNEST/NEST,NESTI,NESTIJ
C
      COMMON/FESTER/NESTER
C
C       ARRAY TO SUPPORT MACRO GUI
      CHARACTER MACARRAY(1:1024)*8
      COMMON/ARRAYMAC/MACARRAY
C
