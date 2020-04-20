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

C       HERE WE PLACE THE CODE THAT UPDATES THE LENS AND RELOADES THE LENS
C       GET CELL VALUE AND PUT IN TEMPVAL
C
C
C       RADIUS OF CURVATURE CHANGES
      IF(PREVCOL.EQ.1) THEN
          CALL WGRIDGETCELLDOUBLE(IDF_LENGRID1,PREVCOL,PREVROW,TEMPVAL)
          IF(TEMPVAL.NE.RD((PREVROW-1),2)) THEN
              INPUT='U L'
              CALL PROCES
              WRITE(INPUT,*) 'CHG,',PREVROW-1
              CALL PROCES
              IF(RDM) THEN
                  WRITE(INPUT,*) 'RD,',TEMPVAL
                  CALL PROCES
              ELSE
                  WRITE(INPUT,*) 'CV,',TEMPVAL
                  CALL PROCES
              END IF
              INPUT='EOS'
              CALL PROCES
              INPUT='OUT TP'
              CALL PROCES
              INCLUDE 'LOADSHEET.INC'
          END IF
      END IF
C       THICKNESS CHANGES
      IF(PREVCOL.EQ.2) THEN
          CALL WGRIDGETCELLDOUBLE(IDF_LENGRID1,PREVCOL,PREVROW,TEMPVAL)
          IF(TEMPVAL.NE.TH((PREVROW-1),2)) THEN
              INPUT='U L'
              CALL PROCES
              CALL ITOAA((PREVROW-1),AJ)
              INPUT='CHG '//AJ
              CALL PROCES
              CALL DTOA23(TEMPVAL,ATEMPVAL)
              INPUT='TH '//ATEMPVAL
              CALL PROCES
              INPUT='EOS'
              CALL PROCES
              INPUT='OUT TP'
              CALL PROCES
              INCLUDE 'LOADSHEET.INC'
          END IF
      END IF
