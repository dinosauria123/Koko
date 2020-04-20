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

!        CALL WDIALOGSELECT(IDD_SURFACEEDITOR_RD)
C
!        IF(RDM) CALL WGRIDLABELCOLUMN(IDF_LENGRID1,1,'Radius')
!        IF(.NOT.RDM) CALL WGRIDLABELCOLUMN(IDF_LENGRID1,1,'Curvature')
C
C       HERE IS WHERE THE CURRENT LENS SURFACE DATA IS INITIALLY LOADED INTO THE GRID DIALOG
C
!        CALL WGRIDROWS(IDF_LENGRID1,INT(SYSTEM1(20))+1)
      DO I=0,INT(SYSTEM1(20))
          J=I+1
          CALL ITOAA(I,AJ)
!        CALL WGRIDLABELROW(IDF_LENGRID1,J,AJ)
      END DO
C       NOW LOAD IN Radius, Thickness and Glass
      DO I=0,INT(SYSTEM1(20))
C
C       COLUMN 1
C
          RD(I,1)=ALENS(1,I)
          IF(RDM) THEN
              IF(ALENS(1,I).NE.0.0D0) RD(I,1)=1.0D0/ALENS(1,I)
          END IF
          RD(I,2)=RD(I,1)
!        CALL WGRIDPUTCELLDOUBLE(IDF_LENGRID1,1,I+1,RD(I,1),'(G23.15)')
C
C       COLUMN 2
C
          TH(I,1)=ALENS(3,I)
          TH(I,2)=ALENS(3,I)
!        CALL WGRIDPUTCELLDOUBLE(IDF_LENGRID1,2,I+1,TH(I,1),'(G23.15)')
C
C       COLUMN 3
C
          IF(GLANAM(I,1).EQ.'             ') THEN
              GL=TRIM(GLANAM(I,2))
          ELSE
              GL=TRIM(GLANAM(I,1))//' '//TRIM(GLANAM(I,2))
          END IF
          GL2(I,1)='  '//GL(1:27)
          GL2(I,2)=GL2(I,1)
!        CALL WGRIDPUTCELLSTRING(IDF_LENGRID1,3,I+1,GL2(I,1))
C
C
C       NOW LOAD UP THE STRING STATUS CHECK MARKS IN ALL THE OTHER CELLS
C       COLUMN 4, STOP SURFACE
          IF(I.EQ.INT(SYSTEM1(26))) THEN
!        CALL WGRIDPUTCELLSTRING(IDF_LENGRID1,4,I+1,'X')
          ELSE
!        CALL WGRIDPUTCELLSTRING(IDF_LENGRID1,4,I+1,' ')
          END IF
C       COLUMN 5, REF SURFACE
          IF(I.EQ.INT(SYSTEM1(25))) THEN
!        CALL WGRIDPUTCELLSTRING(IDF_LENGRID1,5,I+1,'X')
          ELSE
!        CALL WGRIDPUTCELLSTRING(IDF_LENGRID1,5,I+1,' ')
          END IF
C       COLUMN 6, APERTURES
          IF(ALENS(9,I).NE.0.0D0.OR.ALENS(16,I).NE.0.0D0.OR.
     1    ALENS(51,I).NE.0.0D0.OR.ALENS(61,I).NE.0.0D0) THEN
!        CALL WGRIDPUTCELLSTRING(IDF_LENGRID1,6,I+1,'X')
          ELSE
!        CALL WGRIDPUTCELLSTRING(IDF_LENGRID1,6,I+1,' ')
          END IF
C       COLUMN 7, TILTS AND DECENTERS
          IF(ALENS(25,I).NE.0.0D0.OR.ALENS(29,I).NE.0.0D0) THEN
!        CALL WGRIDPUTCELLSTRING(IDF_LENGRID1,7,I+1,'X')
          ELSE
!        CALL WGRIDPUTCELLSTRING(IDF_LENGRID1,7,I+1,' ')
          END IF
C       COLUMN 8, TILTS AND DECENTERS
          IF(ALENS(1,I).EQ.0.0D0.AND.ALENS(2,I).NE.0.0D0) THEN
              ALENS(2,I)=0.0D0
          END IF
          IF(ALENS(1,I).NE.0.0D0.AND.ALENS(43,I).NE.0.0D0) THEN
              ALENS(43,I)=0.0D0
          END IF
          IF(ALENS(4,I).EQ.0.0D0.AND.ALENS(5,I).EQ.0.0D0.AND.
     1    ALENS(6,I).EQ.0.0D0.AND.ALENS(7,I).EQ.0.0D0.AND.
     1    ALENS(81,I).EQ.0.0D0.AND.ALENS(82,I).EQ.0.0D0.AND.
     1    ALENS(83,I).EQ.0.0D0.AND.ALENS(84,I).EQ.0.0D0.AND.
     1    ALENS(85,I).EQ.0.0D0.AND.ALENS(43,I).EQ.0.0D0) THEN
              ALENS(8,I)=0.0D0
          END IF
          IF(ALENS(23,I).NE.0.0D0.OR.ALENS(8,I).NE.0.0D0.OR.
     1    ALENS(2,I).NE.0.0D0.OR.ALENS(4,I).NE.0.0D0.OR.
     1    ALENS(5,I).NE.0.0D0.OR.ALENS(6,I).NE.0.0D0.OR.
     1    ALENS(7,I).NE.0.0D0.OR.ALENS(36,I).NE.0.0D0.OR.
     1    ALENS(37,I).NE.0.0D0.OR.ALENS(38,I).NE.0.0D0.OR.
     1    ALENS(39,I).NE.0.0D0.OR.ALENS(40,I).NE.0.0D0.OR.
     1    ALENS(41,I).NE.0.0D0.OR.ALENS(43,I).NE.0.0D0.OR.
     1    ALENS(81,I).NE.0.0D0.OR.ALENS(82,I).NE.0.0D0.OR.
     1    ALENS(83,I).NE.0.0D0.OR.ALENS(85,I).NE.0.0D0.OR.
     1    ALENS(85,I).NE.0.0D0) THEN
!        CALL WGRIDPUTCELLSTRING(IDF_LENGRID1,8,I+1,'X')
          ELSE
!        CALL WGRIDPUTCELLSTRING(IDF_LENGRID1,8,I+1,' ')
          END IF
C       COLUMN 9, PIKUPS AND SOLVES
          IF(ALENS(32,I).NE.0.0D0.OR.ALENS(33,I).NE.0.0D0) THEN
!        CALL WGRIDPUTCELLSTRING(IDF_LENGRID1,9,I+1,'X')
          ELSE
!        CALL WGRIDPUTCELLSTRING(IDF_LENGRID1,9,I+1,' ')
          END IF
C       COLUMN 10, ARRAY AND GRATING SURFACES
          IF(ALENS(96,I).NE.0.0D0.OR.ALENS(133,I).NE.0.0D0) THEN
!        CALL WGRIDPUTCELLSTRING(IDF_LENGRID1,10,I+1,'X')
          ELSE
!        CALL WGRIDPUTCELLSTRING(IDF_LENGRID1,10,I+1,' ')
          END IF
C       COLUMN 11, SPECIAL SURFACES
          IF(ALENS(34,I).NE.0.0D0) THEN
!        CALL WGRIDPUTCELLSTRING(IDF_LENGRID1,11,I+1,'X')
          ELSE
!        CALL WGRIDPUTCELLSTRING(IDF_LENGRID1,11,I+1,' ')
          END IF
C       COLUMN 12, OTHER
          IF(ALENS(143,I).NE.0.0D0.OR.ALENS(124,I).NE.0.0D0.OR.
     1    ALENS(58,I).NE.0.0D0.OR.ALENS(68,I).NE.0.0D0.OR.
     2    ALENS(110,I).NE.0.0D0.OR.ALENS(102,I).NE.0.0D0.OR.
     3    ALENS(111,I).NE.0.0D0.OR.ALENS(112,I).NE.0.0D0.OR.
     4    LBL(I)(1:20).NE.'                    '.OR.
     5    ALENS(144,I).NE.0.0D0) THEN
!        CALL WGRIDPUTCELLSTRING(IDF_LENGRID1,12,I+1,'X')
          ELSE
!        CALL WGRIDPUTCELLSTRING(IDF_LENGRID1,12,I+1,' ')
          END IF
      END DO
C
!        CALL WDialogShow(-1,-1,0,Modeless)
