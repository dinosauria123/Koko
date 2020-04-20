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

C     VARIABLES FOR DSPOTT USAGE IN SPOTIT.FOR ADDED 6/99
      INTEGER ID,NDSPOTT,RINGTOT
      COMMON/SPOTTER/ID,NDSPOTT
C     END OF WORK ON SPOTIT.FOR
C
      REAL*8 DSPOT(1:60),RINGRAD(0:50),RINGANG(0:50)
      INTEGER RNUMBR
C
      LOGICAL APODGAUSS
      REAL*8 APODDBLOSS
      COMMON/APODLOSS/APODGAUSS,APODDBLOSS
C
      REAL*8 FCSFT,FCSFTX,FCSFTY
C
      LOGICAL STATSP
C
      COMMON/FULSTA/STATSP
C
      COMMON/FCSHFT/FCSFT,FCSFTX,FCSFTY
C
      REAL*8 OPRINGRAD(0:50),OPRINGANG(0:50),OPRINGTOT,OPRNUMBR
C
      INTEGER OPRINGPNT(0:2500),OPSPDTYPE,OPNRECT
C
      INTEGER RINGPNT(0:2500),SPDTYPE,NRECT
C
      COMMON/RINGER/RINGRAD,RINGANG,RINGPNT,RINGTOT,RNUMBR
     1,SPDTYPE,NRECT,OPSPDTYPE,
     3OPRINGRAD,OPRINGANG,OPRINGPNT,OPRINGTOT,OPRNUMBR,
     1OPNRECT
C
      COMMON/CR2/DSPOT
C
      LOGICAL LSF
C
      COMMON/LSFTF/LSF
C
      INTEGER ITOT
C
      REAL*8 DDEELL
C
      COMMON/TTTOT/ITOT,DDEELL
