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

C       TENTH SET OF OPTIMIZATION ROUTINES

      SUBROUTINE VUNITS(PNAME,VUNI)
          IMPLICIT NONE
          CHARACTER PNAME*8,UVAL*8,VUNI*8,ANNVAL*8,IUVAL*8,FRVAL*8
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          IF(SYSTEM1(6).EQ.1.0D0) UVAL='INCH    '
          IF(SYSTEM1(6).EQ.2.0D0) UVAL='CM      '
          IF(SYSTEM1(6).EQ.3.0D0) UVAL='MM      '
          IF(SYSTEM1(6).EQ.4.0D0) UVAL='METER   '
          IF(SYSTEM1(6).EQ.1.0D0) IUVAL='(1/INCH)'
          IF(SYSTEM1(6).EQ.2.0D0) IUVAL='(1/CM)  '
          IF(SYSTEM1(6).EQ.3.0D0) IUVAL='(1/MM)  '
          IF(SYSTEM1(6).EQ.4.0D0) IUVAL='(1/M)   '
          ANNVAL='DEGREE  '
          FRVAL='FRINGE  '
          IF(PNAME.EQ.'RD      ') VUNI=UVAL
          IF(PNAME.EQ.'CV      ') VUNI=IUVAL
          IF(PNAME.EQ.'RD_FR   ') VUNI=UVAL
          IF(PNAME.EQ.'CV_FR   ') VUNI=IUVAL
          IF(PNAME.EQ.'TH      ') VUNI=UVAL
          IF(PNAME.EQ.'CC      ') VUNI='        '
          IF(PNAME.EQ.'AD      ') VUNI='        '
          IF(PNAME.EQ.'AE      ') VUNI='        '
          IF(PNAME.EQ.'AF      ') VUNI='        '
          IF(PNAME.EQ.'AG      ') VUNI='        '
          IF(PNAME.EQ.'AH      ') VUNI='        '
          IF(PNAME.EQ.'AI      ') VUNI='        '
          IF(PNAME.EQ.'AD      ') VUNI='        '
          IF(PNAME.EQ.'AK      ') VUNI='        '
          IF(PNAME.EQ.'RDTOR   ') VUNI=UVAL
          IF(PNAME.EQ.'CVTOR   ') VUNI=IUVAL
          IF(PNAME.EQ.'RDTFR   ') VUNI=UVAL
          IF(PNAME.EQ.'CVTFR   ') VUNI=IUVAL
          IF(PNAME.EQ.'CCTOR   ') VUNI='        '
          IF(PNAME.EQ.'ADTOR   ') VUNI='        '
          IF(PNAME.EQ.'AETOR   ') VUNI='        '
          IF(PNAME.EQ.'AFTOR   ') VUNI='        '
          IF(PNAME.EQ.'AGTOR   ') VUNI='        '
          IF(PNAME.EQ.'ALPHA   ') VUNI=ANNVAL
          IF(PNAME.EQ.'BETA    ') VUNI=ANNVAL
          IF(PNAME.EQ.'GAMMA   ') VUNI=ANNVAL
          IF(PNAME.EQ.'STILTA  ') VUNI=ANNVAL
          IF(PNAME.EQ.'STILTB  ') VUNI=ANNVAL
          IF(PNAME.EQ.'STILTG  ') VUNI=ANNVAL
          IF(PNAME.EQ.'BTILTA  ') VUNI=ANNVAL
          IF(PNAME.EQ.'BTILTB  ') VUNI=ANNVAL
          IF(PNAME.EQ.'BTILTG  ') VUNI=ANNVAL
          IF(PNAME.EQ.'XD      ') VUNI=UVAL
          IF(PNAME.EQ.'YD      ') VUNI=UVAL
          IF(PNAME.EQ.'ZD      ') VUNI=UVAL
          IF(PNAME.EQ.'DISPX   ') VUNI=UVAL
          IF(PNAME.EQ.'DISPY   ') VUNI=UVAL
          IF(PNAME.EQ.'DISPX   ') VUNI=UVAL
          IF(PNAME.EQ.'ROLLX   ') VUNI=UVAL
          IF(PNAME.EQ.'ROLLY   ') VUNI=UVAL
          IF(PNAME.EQ.'GALPHA  ') VUNI=ANNVAL
          IF(PNAME.EQ.'GBETA   ') VUNI=ANNVAL
          IF(PNAME.EQ.'GGAMMA  ') VUNI=ANNVAL
          IF(PNAME.EQ.'GDX     ') VUNI=UVAL
          IF(PNAME.EQ.'GDY     ') VUNI=UVAL
          IF(PNAME.EQ.'GDZ     ') VUNI=UVAL
          IF(PNAME.EQ.'PIVX    ') VUNI=UVAL
          IF(PNAME.EQ.'PIVY    ') VUNI=UVAL
          IF(PNAME.EQ.'PIVZ    ') VUNI=UVAL
          IF(PNAME.EQ.'CLPX    ') VUNI=UVAL
          IF(PNAME.EQ.'CLPY    ') VUNI=UVAL
          IF(PNAME.EQ.'N1      ') VUNI='        '
          IF(PNAME.EQ.'N2      ') VUNI='        '
          IF(PNAME.EQ.'N3      ') VUNI='        '
          IF(PNAME.EQ.'N4      ') VUNI='        '
          IF(PNAME.EQ.'N5      ') VUNI='        '
          IF(PNAME.EQ.'N6      ') VUNI='        '
          IF(PNAME.EQ.'N7      ') VUNI='        '
          IF(PNAME.EQ.'N8      ') VUNI='        '
          IF(PNAME.EQ.'N9      ') VUNI='        '
          IF(PNAME.EQ.'N10     ') VUNI='        '
          IF(PNAME.EQ.'INDEX   ') VUNI='        '
          IF(PNAME.EQ.'VNUM    ') VUNI='        '
          IF(PNAME.EQ.'DPART   ') VUNI='        '
          IF(PNAME.EQ.'C1      ') VUNI='        '
          IF(PNAME.EQ.'C2      ') VUNI='        '
          IF(PNAME.EQ.'C3      ') VUNI='        '
          IF(PNAME.EQ.'C4      ') VUNI='        '
          IF(PNAME.EQ.'C5      ') VUNI='        '
          IF(PNAME.EQ.'C6      ') VUNI='        '
          IF(PNAME.EQ.'C7      ') VUNI='        '
          IF(PNAME.EQ.'C8      ') VUNI='        '
          IF(PNAME.EQ.'C9      ') VUNI='        '
          IF(PNAME.EQ.'C10     ') VUNI='        '
          IF(PNAME.EQ.'C11     ') VUNI='        '
          IF(PNAME.EQ.'C12     ') VUNI='        '
          IF(PNAME.EQ.'C13     ') VUNI='        '
          IF(PNAME.EQ.'C14     ') VUNI='        '
          IF(PNAME.EQ.'C15     ') VUNI='        '
          IF(PNAME.EQ.'C16     ') VUNI='        '
          IF(PNAME.EQ.'C17     ') VUNI='        '
          IF(PNAME.EQ.'C18     ') VUNI='        '
          IF(PNAME.EQ.'C19     ') VUNI='        '
          IF(PNAME.EQ.'C10     ') VUNI='        '
          IF(PNAME.EQ.'C21     ') VUNI='        '
          IF(PNAME.EQ.'C22     ') VUNI='        '
          IF(PNAME.EQ.'C23     ') VUNI='        '
          IF(PNAME.EQ.'C24     ') VUNI='        '
          IF(PNAME.EQ.'C25     ') VUNI='        '
          IF(PNAME.EQ.'C26     ') VUNI='        '
          IF(PNAME.EQ.'C27     ') VUNI='        '
          IF(PNAME.EQ.'C28     ') VUNI='        '
          IF(PNAME.EQ.'C29     ') VUNI='        '
          IF(PNAME.EQ.'C30     ') VUNI='        '
          IF(PNAME.EQ.'C31     ') VUNI='        '
          IF(PNAME.EQ.'C32     ') VUNI='        '
          IF(PNAME.EQ.'C33     ') VUNI='        '
          IF(PNAME.EQ.'C34     ') VUNI='        '
          IF(PNAME.EQ.'C35     ') VUNI='        '
          IF(PNAME.EQ.'C36     ') VUNI='        '
          IF(PNAME.EQ.'C37     ') VUNI='        '
          IF(PNAME.EQ.'C38     ') VUNI='        '
          IF(PNAME.EQ.'C39     ') VUNI='        '
          IF(PNAME.EQ.'C40     ') VUNI='        '
          IF(PNAME.EQ.'C41     ') VUNI='        '
          IF(PNAME.EQ.'C42     ') VUNI='        '
          IF(PNAME.EQ.'C43     ') VUNI='        '
          IF(PNAME.EQ.'C44     ') VUNI='        '
          IF(PNAME.EQ.'C45     ') VUNI='        '
          IF(PNAME.EQ.'C46     ') VUNI='        '
          IF(PNAME.EQ.'C47     ') VUNI='        '
          IF(PNAME.EQ.'C48     ') VUNI='        '
          IF(PNAME.EQ.'C49     ') VUNI='        '
          IF(PNAME.EQ.'C50     ') VUNI='        '
          IF(PNAME.EQ.'C51     ') VUNI='        '
          IF(PNAME.EQ.'C52     ') VUNI='        '
          IF(PNAME.EQ.'C53     ') VUNI='        '
          IF(PNAME.EQ.'C54     ') VUNI='        '
          IF(PNAME.EQ.'C55     ') VUNI='        '
          IF(PNAME.EQ.'C56     ') VUNI='        '
          IF(PNAME.EQ.'C57     ') VUNI='        '
          IF(PNAME.EQ.'C58     ') VUNI='        '
          IF(PNAME.EQ.'C59     ') VUNI='        '
          IF(PNAME.EQ.'C60     ') VUNI='        '
          IF(PNAME.EQ.'C61     ') VUNI='        '
          IF(PNAME.EQ.'C62     ') VUNI='        '
          IF(PNAME.EQ.'C63     ') VUNI='        '
          IF(PNAME.EQ.'C64     ') VUNI='        '
          IF(PNAME.EQ.'C65     ') VUNI='        '
          IF(PNAME.EQ.'C66     ') VUNI='        '
          IF(PNAME.EQ.'C67     ') VUNI='        '
          IF(PNAME.EQ.'C68     ') VUNI='        '
          IF(PNAME.EQ.'C69     ') VUNI='        '
          IF(PNAME.EQ.'C70     ') VUNI='        '
          IF(PNAME.EQ.'C71     ') VUNI='        '
          IF(PNAME.EQ.'C72     ') VUNI='        '
          IF(PNAME.EQ.'C73     ') VUNI='        '
          IF(PNAME.EQ.'C74     ') VUNI='        '
          IF(PNAME.EQ.'C75     ') VUNI='        '
          IF(PNAME.EQ.'C76     ') VUNI='        '
          IF(PNAME.EQ.'C77     ') VUNI='        '
          IF(PNAME.EQ.'C78     ') VUNI='        '
          IF(PNAME.EQ.'C79     ') VUNI='        '
          IF(PNAME.EQ.'C80     ') VUNI='        '
          IF(PNAME.EQ.'C81     ') VUNI='        '
          IF(PNAME.EQ.'C82     ') VUNI='        '
          IF(PNAME.EQ.'C83     ') VUNI='        '
          IF(PNAME.EQ.'C84     ') VUNI='        '
          IF(PNAME.EQ.'C85     ') VUNI='        '
          IF(PNAME.EQ.'C86     ') VUNI='        '
          IF(PNAME.EQ.'C87     ') VUNI='        '
          IF(PNAME.EQ.'C88     ') VUNI='        '
          IF(PNAME.EQ.'C89     ') VUNI='        '
          IF(PNAME.EQ.'C90     ') VUNI='        '
          IF(PNAME.EQ.'C91     ') VUNI='        '
          IF(PNAME.EQ.'C92     ') VUNI='        '
          IF(PNAME.EQ.'C93     ') VUNI='        '
          IF(PNAME.EQ.'C94     ') VUNI='        '
          IF(PNAME.EQ.'C95     ') VUNI='        '
          IF(PNAME.EQ.'C96     ') VUNI='        '
          IF(PNAME.EQ.'AC      ') VUNI='        '
          RETURN
      END
      SUBROUTINE INVSENSIOUT(IV)
          IMPLICIT NONE
C
          INCLUDE 'datsub.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          INCLUDE 'datmac.inc'
C
          REAL*8 DELTEST,PV4,MOT(1:10)
C
          COMMON/LOCOMOTION/MOT
C
          REAL*8 CRITCENT(1:10),OCRIT(10),NCRIT(10),CRITCENT2(1:10)
C
          COMMON/ONCRIT/OCRIT,NCRIT
C
          COMMON/CENTCRIT/CRITCENT,CRITCENT2
C
          REAL*8 CHGVAL(10),OPVVAL(10),V1,V4
C
          CHARACTER PNAME*8
C
          COMMON/NAMEP/PNAME
C
          INTEGER IIV,I,II,NVN,VTYPE,CALLNUM,OLDCALL,IV
C
          LOGICAL SENSERR,CRITTERS,SECONDCALL
C
          COMMON/ERRSENS/SENSERR
C
          COMMON/CALLCNT/SECONDCALL,CALLNUM,OLDCALL
C
          INTEGER VARSURF,VARSURF2,VARSURF3
C
          COMMON/OUTP1/CHGVAL,V1,V4,NVN,VTYPE,VARSURF,OPVVAL,VARSURF2
     1    ,VARSURF3
C
          INTEGER CMPSURF(10)
C
          COMMON/SURFCMP/CMPSURF
C
          CHARACTER VUNI*8
C
C     NVN IS VARIABLE COUNT NUMBER, PNAME IS TOL VARIABLE NAME USED
C
C     VTYPE IS TOL VARIABLE CODED ID VALUE
C
C     THIS ROUTINE DOES THE SENSITIVITY OUTPUT FOR ONE VARIABLE
C     DEIGNATED BY PNAME,VTYPE AND NVN. IT IS CALLED ONCE FOR + DELTA
C     AND ONCE FOR - DELTA
C
          IIV=IV
          IIV=IIV-MAXCMP
C
          OLDCALL=CALLNUM
          CALLNUM=NVN
          IF(OLDCALL.EQ.CALLNUM) SECONDCALL=.TRUE.
          IF(OLDCALL.NE.CALLNUM) SECONDCALL=.FALSE.
C
 3701     FORMAT(A8,'::',A69)
 4701     FORMAT(A8,'::',A69)
C     OUTPUT TOL VARIABLE DATA
          WRITE(OUTLYNE,300)
          CALL SHOWIT(0)
          WRITE(19,1300)
 300      FORMAT('**************************************************',
     1    '****************************')
 1300     FORMAT('**************************************************',
     1    '****************************')
          IF(.NOT.SECONDCALL) THEN
              WRITE(OUTLYNE,100) PNAME,CALLNUM
              CALL SHOWIT(0)
              WRITE(19,1100) PNAME,CALLNUM
              IF(VTYPE.LE.153.OR.VTYPE.GE.157.AND.VTYPE.LE.159) THEN
                  WRITE(OUTLYNE,200) VARSURF
                  CALL SHOWIT(0)
                  WRITE(19,2200) VARSURF
              END IF
              IF(VTYPE.GE.154.AND.VTYPE.LE.156.OR.
     1        VTYPE.GE.160.AND.VTYPE.LE.164) THEN
                  WRITE(OUTLYNE,400) VARSURF,VARSURF2
                  CALL SHOWIT(0)
                  WRITE(19,4400) VARSURF,VARSURF2
              END IF
              IF(VTYPE.GE.163.AND.VTYPE.LE.164) THEN
                  WRITE(OUTLYNE,600) VARSURF3
                  CALL SHOWIT(0)
                  WRITE(19,6600) VARSURF3
              END IF
          ELSE
              WRITE(OUTLYNE,100) PNAME,OLDCALL
              CALL SHOWIT(0)
              WRITE(19,1100) PNAME,OLDCALL
              IF(VTYPE.LE.153.OR.VTYPE.GE.157.AND.VTYPE.LE.159) THEN
                  WRITE(OUTLYNE,200) VARSURF
                  CALL SHOWIT(0)
                  WRITE(19,2200) VARSURF
              END IF
              IF(VTYPE.GE.154.AND.VTYPE.LE.156.OR.
     1        VTYPE.GE.160.AND.VTYPE.LE.164) THEN
                  WRITE(OUTLYNE,400) VARSURF,VARSURF2
                  CALL SHOWIT(0)
                  WRITE(19,4400) VARSURF,VARSURF2
              END IF
              IF(VTYPE.GE.163.AND.VTYPE.LE.164) THEN
                  WRITE(OUTLYNE,600) VARSURF3
                  CALL SHOWIT(0)
                  WRITE(19,6600) VARSURF3
              END IF
          END IF
 100      FORMAT('TOLERANCE VARIABLE NAME   = ',
     1    A8,1X,'TOLERANCE VARIABLE # = ',I3)
 1100     FORMAT('TOLERANCE VARIABLE NAME   = ',
     1    A8,1X,'TOLERANCE VARIABLE # = ',I3)
 400      FORMAT('TOLERANCE VARIABLE SURFACES = #',I3,' TO #',I3)
 4400     FORMAT('TOLERANCE VARIABLE SURFACES = #',I3,' TO #',I3)
 200      FORMAT('TOLERANCE VARIABLE SURFACE #  = ',I3)
 2200     FORMAT('TOLERANCE VARIABLE SURFACE #  = ',I3)
 600      FORMAT('ROLL IS ABOUT MOUNTING SURFACE OF SURFACE #  = ',I3)
 6600     FORMAT('ROLL IS ABOUT MOUNTING SURFACE OF SURFACE #  = ',I3)
C
C     NOW OUTPUT THE TOL VARIABLE CHANGE VALUE V1
          IF(VTYPE.EQ.1.OR.VTYPE.EQ.2.OR.VTYPE.EQ.9.OR.VTYPE.EQ.10) THEN
 501          FORMAT('TOLERANCE VARIABLE % CHANGE = ',G15.8)
 502          FORMAT('TOLERANCE VARIABLE CHANGE VALUE = ',G15.8,1X,A8)
 1501         FORMAT('TOLERANCE VARIABLE % CHANGE = ',G15.8)
 1502         FORMAT('TOLERANCE VARIABLE CHANGE VALUE = ',G15.8,1X,A8)
              WRITE(OUTLYNE,501) V1
              CALL SHOWIT(0)
              WRITE(19,1501) V1
              CALL VUNITS(PNAME,VUNI)
              WRITE(OUTLYNE,502) V4,VUNI
              CALL SHOWIT(0)
              WRITE(19,1502) V4,VUNI
          END IF
          IF(VTYPE.GE.3.AND.VTYPE.LE.8.OR.
     1    VTYPE.GE.11.AND.VTYPE.LE.133.OR.VTYPE.EQ.138.OR.
     1    VTYPE.EQ.139.OR.VTYPE.EQ.140.OR.VTYPE.GE.141.0D0.AND.
     2    VTYPE.LE.164.0D0) THEN
 601          FORMAT('TOLERANCE VARIABLE CHANGE VALUE = ',G15.8,1X,A8)
 1601         FORMAT('TOLERANCE VARIABLE CHANGE VALUE = ',G15.8,1X,A8)
              CALL VUNITS(PNAME,VUNI)
              WRITE(OUTLYNE,601) V1,VUNI
              CALL SHOWIT(0)
              WRITE(19,1601) V1,VUNI
          END IF
          IF(VTYPE.GE.134.AND.VTYPE.LE.137) THEN
 701          FORMAT('TOLERANCE VARIABLE CHANGE (FRINGE) = ',G15.8)
 702          FORMAT('TOLERANCE VARIABLE CHANGE VALUE = ',G15.8,1X,A8)
 1701         FORMAT('TOLERANCE VARIABLE CHANGE (FRINGE) = ',G15.8)
 1702         FORMAT('TOLERANCE VARIABLE CHANGE VALUE = ',G15.8,1X,A8)
              WRITE(OUTLYNE,701) V1
              CALL SHOWIT(0)
              WRITE(19,1701) V1
              CALL VUNITS(PNAME,VUNI)
              WRITE(OUTLYNE,702) V4,VUNI
              CALL SHOWIT(0)
              WRITE(19,1702) V4,VUNI
          END IF
C     NOW OUTPUT TOL OPERAND CHANGE VALUES AND THEIR OPTIONAL DESCRIPTIONS
 801      FORMAT('(TOLERANCE OPERAND SENSITIVITIES)')
 1801     FORMAT('(TOLERANCE OPERAND SENSITIVITIES)')
          WRITE(OUTLYNE,801)
          CALL SHOWIT(0)
          WRITE(19,1801)
 800      FORMAT('TOLERANCE OPERAND(#',I3,') = ',
     1    A8,'*CHANGE = ',1X,G15.8)
 1800     FORMAT('TOLERANCE OPERAND(#',I3,') = ',
     1    A8,'*CHANGE = ',1X,G15.8)
 803      FORMAT('TOLERANCE OPERAND(#',I3,') = ',
     1    A8,'  VALUE = ',1X,G15.8)
 1803     FORMAT('TOLERANCE OPERAND(#',I3,') = ',
     1    A8,'  VALUE = ',1X,G15.8)
          DO I=1,MAXTOP
              II=I+MAXFOCRIT
              IF(ISTOP(I)) THEN
                  WRITE(OUTLYNE,800) I,OPNAM(II),CHGVAL(I)
                  CALL SHOWIT(0)
                  WRITE(19,1800) I,OPNAM(II),CHGVAL(I)
                  WRITE(OUTLYNE,803) I,OPNAM(II),OPVVAL(I)
                  CALL SHOWIT(0)
                  WRITE(19,1803) I,OPNAM(II),OPVVAL(I)
                  IF(OPERDESC(II)(1:8).NE.'        ')
     1            WRITE(OUTLYNE,3701) OPNAM(II),OPERDESC(II)(1:69)
                  IF(OPERDESC(II)(1:8).NE.'        ')
     1            CALL SHOWIT(0)
                  IF(OPERDESC(II)(1:8).NE.'        ')
     1            WRITE(19,4701) OPNAM(II),OPERDESC(II)(1:69)
              END IF
          END DO
C     ARE THERE COMPENSATORS
          CRITTERS=.FALSE.
          DO I=1,MAXFOCRIT
              IF(ISCRIT(I)) CRITTERS=.TRUE.
          END DO
C
C     NOW OUTPUT COMPENSATOR MOTIONS
C
C     NOW OUTPUT FORCRIT CHANGE AND FOCRIT OPTIONAL DESCRIPTIONS
C
 802      FORMAT('(FOCUS CRITERIA (FOCRIT) DATA)')
 1802     FORMAT('(FOCUS CRITERIA (FOCRIT) DATA)')
          IF(CRITTERS) WRITE(OUTLYNE,802)
          IF(CRITTERS) CALL SHOWIT(0)
          IF(CRITTERS) WRITE(19,1802)
 806      FORMAT('FOCRIT(#',I3,') = ',
     1    A8,'      FOCRIT STARTING VALUE = ',1X,G15.8)

 1806     FORMAT('FOCRIT(#',I3,') = ',
     1    A8,'      FOCRIT STARTING VALUE = ',1X,G15.8)
 808      FORMAT('FOCRIT(#',I3,') = ',
     1    A8,'FOCRIT FINAL RESTORED VALUE = ',1X,G15.8)

 1808     FORMAT('FOCRIT(#',I3,') = ',
     1    A8,'FOCRIT FINAL RESTORED VALUE = ',1X,G15.8)

          DO I=1,MAXFOCRIT
              IF(ISCRIT(I)) THEN
                  WRITE(OUTLYNE,806) I,OPNAM(I),OCRIT(I)
                  CALL SHOWIT(0)
                  WRITE(19,1806) I,OPNAM(I),OCRIT(I)
                  WRITE(OUTLYNE,808) I,OPNAM(I),OCRIT(I)
                  CALL SHOWIT(0)
                  WRITE(19,1808) I,OPNAM(I),NCRIT(I)
                  IF(OPERDESC(I)(1:8).NE.'        ')
     1            WRITE(OUTLYNE,3701) OPNAM(I),OPERDESC(I)(1:69)
                  IF(OPERDESC(I)(1:8).NE.'        ')
     1            CALL SHOWIT(0)
                  IF(OPERDESC(I)(1:8).NE.'        ')
     1            WRITE(19,4701) OPNAM(I),OPERDESC(I)(1:69)
              END IF
          END DO
 901      FORMAT('(COMPENSATOR VARIABLE DATA)')
 1901     FORMAT('(COMPENSATOR VARIABLE DATA)')
          IF(CRITTERS) WRITE(OUTLYNE,901)
          IF(CRITTERS) CALL SHOWIT(0)
          IF(CRITTERS) WRITE(19,1901)
 900      FORMAT('COMPENSATOR VARIABLE(#',I3,') = ',
     1    A8,' MOTION = ',1X,G15.8)
 1900     FORMAT('COMPENSATOR VARIABLE(#',I3,') = ',
     1    A8,' MOTION = ',1X,G15.8)
 902      FORMAT('COMPENSATOR VARIABLE SURFACE #',I3)
 1902     FORMAT('COMPENSATOR VARIABLE SURFACE #',I3)
          DO I=1,VBCNT
              WRITE(OUTLYNE,900) I,VARNAM(I),MOT(I)
              CALL SHOWIT(0)
              WRITE(19,1900) I,VARNAM(I),MOT(I)
              WRITE(OUTLYNE,902) CMPSURF(I)
              CALL SHOWIT(0)
              WRITE(19,1902) CMPSURF(I)
          END DO
          WRITE(OUTLYNE,8321)
          CALL SHOWIT(0)
          WRITE(19,9321)
 8321     FORMAT(' ')
 9321     FORMAT(' ')
 1001     FORMAT(
     1    '(INVERSE SENSITIVITY TOLERANCE VARIABLE CHANGE PROJECTION)')
 2001     FORMAT(
     1    '(INVERSE SENSITIVITY TOLERANCE VARIABLE CHANGE PROJECTION)')
          WRITE(OUTLYNE,1001)
          CALL SHOWIT(0)
          WRITE(19,2001)
 1000     FORMAT('TOLERANCE VARIABLE(#',I3,') = ',
     1    A8,'REQUIRED MOTION = ',1X,G15.8)
 2000     FORMAT('TOLERANCE VARIABLE(#',I3,') = ',
     1    A8,'REQUIRED MOTION = ',1X,G15.8)
 1005     FORMAT('TOLERANCE OPERAND(#',I3,') = ',
     1    A8,' DESIRED CHANGE = ',1X,G15.8)
 2005     FORMAT('TOLERANCE OPERAND(#',I3,') = ',
     1    A8,' DESIRED CHANGE = ',1X,G15.8)
          DO I=1,MAXTOP
              II=I+MAXTOP
              IF(ISTOP(I)) THEN
                  WRITE(OUTLYNE,1005) I,OPNAM(II),OPERND(II,20)
                  CALL SHOWIT(0)
                  WRITE(19,2005) I,OPNAM(II),OPERND(II,20)
                  IF(CHGVAL(I).NE.0.0D0) THEN
                      PV4=(CHGVAL(I)/DABS(CHGVAL(I)))*(OPERND(II,20)*V4)/CHGVAL(I)
                      DELTEST=DABS(PV4/V4)
                  ELSE
                      PV4=0.0D0
                  END IF
                  WRITE(OUTLYNE,1000) IIV,VARNAM(IV),PV4
                  CALL SHOWIT(0)
                  WRITE(19,2000) IIV,VARNAM(IV),PV4
                  IF(DELTEST.GE.10.0D0) THEN
C     WARN
 9101                 FORMAT('WARINING:')
 9102                 FORMAT('PROJECTED VARIABLE CHANGE IS 10 OR MORE TIMES')
 9103                 FORMAT('GREATER THAN THE INPUT "DELTA" FOR THE CURRENT')
 9104                 FORMAT('VARIABLE. LINEARITY MAY NOT BE VALID.')
 9201                 FORMAT('WARINING:')
 9202                 FORMAT('PROJECTED VARIABLE CHANGE IS 10 OR MORE TIMES')
 9203                 FORMAT('GREATER THAN THE INPUT "DELTA" FOR THE CURRENT')
 9204                 FORMAT('VARIABLE. LINEARITY MAY NOT BE VALID.')
                      WRITE(OUTLYNE,9101)
                      CALL SHOWIT(0)
                      WRITE(19,9201)
                      WRITE(OUTLYNE,9102)
                      CALL SHOWIT(0)
                      WRITE(19,9202)
                      WRITE(OUTLYNE,9103)
                      CALL SHOWIT(0)
                      WRITE(19,9203)
                      WRITE(OUTLYNE,9104)
                      CALL SHOWIT(0)
                      WRITE(19,9204)
                  END IF
              END IF
          END DO
C
          CALL TOLREST
C
          RETURN
      END
      SUBROUTINE SENSIOUT
          IMPLICIT NONE
C
          INCLUDE 'datsub.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          INCLUDE 'datmac.inc'
C
          CHARACTER VUNI*8
C
          REAL*8 MOT(1:10)
C
          COMMON/LOCOMOTION/MOT
C
          REAL*8 CRITCENT(1:10),OCRIT(10),NCRIT(10),CRITCENT2(1:10)
C
          COMMON/ONCRIT/OCRIT,NCRIT
C
          COMMON/CENTCRIT/CRITCENT,CRITCENT2
C
          REAL*8 CHGVAL(10),V1,V4,OPVVAL(10)
C
          CHARACTER PNAME*8
C
          COMMON/NAMEP/PNAME
C
          INTEGER I,II,NVN,VTYPE,CALLNUM,OLDCALL
C
          LOGICAL CRITTERS,SECONDCALL
C
          COMMON/CALLCNT/SECONDCALL,CALLNUM,OLDCALL
C
          INTEGER VARSURF,VARSURF2,VARSURF3
C
          COMMON/OUTP1/CHGVAL,V1,V4,NVN,VTYPE,VARSURF,OPVVAL,VARSURF2
     1    ,VARSURF3
C
          INTEGER CMPSURF(10)
C
          COMMON/SURFCMP/CMPSURF
C
C     NVN IS VARIABLE COUNT NUMBER, PNAME IS TOL VARIABLE NAME USED
C
C     VTYPE IS TOL VARIABLE CODED ID VALUE
C
C     THIS ROUTINE DOES THE SENSITIVITY OUTPUT FOR ONE VARIABLE
C     DEIGNATED BY PNAME,VTYPE AND NVN. IT IS CALLED ONCE FOR + DELTA
C     AND ONCE FOR - DELTA
C
          OLDCALL=CALLNUM
          CALLNUM=NVN
          IF(OLDCALL.EQ.CALLNUM) SECONDCALL=.TRUE.
          IF(OLDCALL.NE.CALLNUM) SECONDCALL=.FALSE.
C
 3701     FORMAT(A8,'::',A69)
 4701     FORMAT(A8,'::',A69)
C     OUTPUT TOL VARIABLE DATA
          WRITE(OUTLYNE,300)
          CALL SHOWIT(0)
          WRITE(19,1300)
 300      FORMAT('**************************************************',
     1    '****************************')
 1300     FORMAT('**************************************************',
     1    '****************************')
          IF(.NOT.SECONDCALL) THEN
              WRITE(OUTLYNE,100) PNAME,CALLNUM
              CALL SHOWIT(0)
              WRITE(19,1100) PNAME,CALLNUM
              IF(VTYPE.LE.153.OR.VTYPE.GE.157.AND.VTYPE.LE.159) THEN
                  WRITE(OUTLYNE,200) VARSURF
                  CALL SHOWIT(0)
                  WRITE(19,2200) VARSURF
              END IF
              IF(VTYPE.GE.154.AND.VTYPE.LE.156.OR.
     1        VTYPE.GE.160.AND.VTYPE.LE.164) THEN
                  WRITE(OUTLYNE,400) VARSURF,VARSURF2
                  CALL SHOWIT(0)
                  WRITE(19,4400) VARSURF,VARSURF2
              END IF
              IF(VTYPE.GE.163.AND.VTYPE.LE.164) THEN
                  WRITE(OUTLYNE,600) VARSURF3
                  CALL SHOWIT(0)
                  WRITE(19,6600) VARSURF3
              END IF
          ELSE
              WRITE(OUTLYNE,100) PNAME,OLDCALL
              CALL SHOWIT(0)
              WRITE(19,1100) PNAME,OLDCALL
              IF(VTYPE.LE.153.OR.VTYPE.GE.157.AND.VTYPE.LE.159) THEN
                  WRITE(OUTLYNE,200) VARSURF
                  CALL SHOWIT(0)
                  WRITE(19,2200) VARSURF
              END IF
              IF(VTYPE.GE.154.AND.VTYPE.LE.156.OR.
     1        VTYPE.GE.160.AND.VTYPE.LE.164) THEN
                  WRITE(OUTLYNE,400) VARSURF,VARSURF2
                  CALL SHOWIT(0)
                  WRITE(19,4400) VARSURF,VARSURF2
              END IF
              IF(VTYPE.GE.163.AND.VTYPE.LE.164) THEN
                  WRITE(OUTLYNE,600) VARSURF3
                  CALL SHOWIT(0)
                  WRITE(19,6600) VARSURF3
              END IF
          END IF
 100      FORMAT('TOLERANCE VARIABLE NAME   = ',
     1    A8,1X,'TOLERANCE VARIABLE # = ',I3)
 1100     FORMAT('TOLERANCE VARIABLE NAME   = ',
     1    A8,1X,'TOLERANCE VARIABLE # = ',I3)
 400      FORMAT('TOLERANCE VARIABLE SURFACES = #',I3,' TO #',I3)
 4400     FORMAT('TOLERANCE VARIABLE SURFACES = #',I3,' TO #',I3)
 200      FORMAT('TOLERANCE VARIABLE SURFACE #  = ',I3)
 2200     FORMAT('TOLERANCE VARIABLE SURFACE #  = ',I3)
 600      FORMAT('ROLL IS ABOUT MOUNTING SURFACE OF SURFACE #  = ',I3)
 6600     FORMAT('ROLL IS ABOUT MOUNTING SURFACE OF SURFACE #  = ',I3)
C
C     NOW OUTPUT THE TOL VARIABLE CHANGE VALUE V1
          IF(VTYPE.EQ.1.OR.VTYPE.EQ.2.OR.VTYPE.EQ.9.OR.VTYPE.EQ.10) THEN
 501          FORMAT('TOLERANCE VARIABLE % CHANGE = ',G15.8)
 502          FORMAT('TOLERANCE VARIABLE CHANGE VALUE = ',G15.8,1X,A8)
 1501         FORMAT('TOLERANCE VARIABLE % CHANGE = ',G15.8)
 1502         FORMAT('TOLERANCE VARIABLE CHANGE VALUE = ',G15.8,1X,A8)
              WRITE(OUTLYNE,501) V1
              CALL SHOWIT(0)
              WRITE(19,1501) V1
              CALL VUNITS(PNAME,VUNI)
              WRITE(OUTLYNE,502) V4,VUNI
              CALL SHOWIT(0)
              WRITE(19,1502) V4,VUNI
          END IF
          IF(VTYPE.GE.3.AND.VTYPE.LE.8.OR.
     1    VTYPE.GE.11.AND.VTYPE.LE.133.OR.VTYPE.EQ.138.OR.
     1    VTYPE.EQ.139.OR.VTYPE.EQ.140.OR.VTYPE.GE.141.0D0.AND.VTYPE
     2    .LE.164.0D0) THEN
 601          FORMAT('TOLERANCE VARIABLE CHANGE VALUE = ',G15.8,1X,A8)
 1601         FORMAT('TOLERANCE VARIABLE CHANGE VALUE = ',G15.8,1X,A8)
              CALL VUNITS(PNAME,VUNI)
              WRITE(OUTLYNE,601) V1,VUNI
              CALL SHOWIT(0)
              WRITE(19,1601) V1,VUNI
          END IF
          IF(VTYPE.GE.134.AND.VTYPE.LE.137) THEN
 701          FORMAT('TOLERANCE VARIABLE CHANGE (FRINGE) = ',G15.8)
 702          FORMAT('TOLERANCE VARIABLE CHANGE VALUE = ',G15.8,1X,A8)
 1701         FORMAT('TOLERANCE VARIABLE CHANGE (FRINGE) = ',G15.8)
 1702         FORMAT('TOLERANCE VARIABLE CHANGE VALUE = ',G15.8,1X,A8)
              WRITE(OUTLYNE,701) V1
              CALL SHOWIT(0)
              WRITE(19,1701) V1
              CALL VUNITS(PNAME,VUNI)
              WRITE(OUTLYNE,702) V4,VUNI
              CALL SHOWIT(0)
              WRITE(19,1702) V4,VUNI
          END IF
C     NOW OUTPUT TOL OPERAND CHANGE VALUES AND THEIR OPTIONAL DESCRIPTIONS
 801      FORMAT('(TOLERANCE OPERAND SENSITIVITIES)')
 1801     FORMAT('(TOLERANCE OPERAND SENSITIVITIES)')
          WRITE(OUTLYNE,801)
          CALL SHOWIT(0)
          WRITE(19,1801)
 800      FORMAT('TOLERANCE OPERAND(#',I3,') = ',
     1    A8,'*CHANGE = ',1X,G15.8)
 1800     FORMAT('TOLERANCE OPERAND(#',I3,') = ',
     1    A8,'*CHANGE = ',1X,G15.8)
 803      FORMAT('TOLERANCE OPERAND(#',I3,') = ',
     1    A8,'  VALUE = ',1X,G15.8)
 1803     FORMAT('TOLERANCE OPERAND(#',I3,') = ',
     1    A8,'  VALUE = ',1X,G15.8)
          DO I=1,MAXTOP
              II=I+MAXFOCRIT
              IF(ISTOP(I)) THEN
                  WRITE(OUTLYNE,800) I,OPNAM(II),CHGVAL(I)
                  CALL SHOWIT(0)
                  WRITE(19,1800) I,OPNAM(II),CHGVAL(I)
                  WRITE(OUTLYNE,803) I,OPNAM(II),OPVVAL(I)
                  CALL SHOWIT(0)
                  WRITE(19,1803) I,OPNAM(II),OPVVAL(I)
                  IF(OPERDESC(II)(1:8).NE.'        ')
     1            WRITE(OUTLYNE,3701) OPNAM(II),OPERDESC(II)(1:69)
                  IF(OPERDESC(II)(1:8).NE.'        ')
     1            CALL SHOWIT(0)
                  IF(OPERDESC(II)(1:8).NE.'        ')
     1            WRITE(OUTLYNE,4701) OPNAM(II),OPERDESC(II)(1:69)
              END IF
          END DO
C     ARE THERE COMPENSATORS
          CRITTERS=.FALSE.
          DO I=1,MAXFOCRIT
              IF(ISCRIT(I)) CRITTERS=.TRUE.
          END DO
C
C     NOW OUTPUT COMPENSATOR MOTIONS
C
C     NOW OUTPUT FORCRIT CHANGE AND FOCRIT OPTIONAL DESCRIPTIONS
C
 802      FORMAT('(FOCUS CRITERIA (FOCRIT) DATA)')
 1802     FORMAT('(FOCUS CRITERIA (FOCRIT) DATA)')
          IF(CRITTERS) WRITE(OUTLYNE,802)
          IF(CRITTERS) CALL SHOWIT(0)
          IF(CRITTERS) WRITE(19,1802)
 806      FORMAT('FOCRIT(#',I3,') = ',
     1    A8,'      FOCRIT STARTING VALUE = ',1X,G15.8)

 1806     FORMAT('FOCRIT(#',I3,') = ',
     1    A8,'      FOCRIT STARTING VALUE = ',1X,G15.8)
 808      FORMAT('FOCRIT(#',I3,') = ',
     1    A8,'FOCRIT FINAL RESTORED VALUE = ',1X,G15.8)

 1808     FORMAT('FOCRIT(#',I3,') = ',
     1    A8,'FOCRIT FINAL RESTORED VALUE = ',1X,G15.8)
          DO I=1,MAXFOCRIT
              IF(ISCRIT(I)) THEN
                  WRITE(OUTLYNE,806) I,OPNAM(I),OCRIT(I)
                  CALL SHOWIT(0)
                  WRITE(19,1806) I,OPNAM(I),OCRIT(I)
                  WRITE(OUTLYNE,808) I,OPNAM(I),NCRIT(I)
                  CALL SHOWIT(0)
                  WRITE(19,1808) I,OPNAM(I),NCRIT(I)
                  IF(OPERDESC(I)(1:8).NE.'        ')
     1            WRITE(OUTLYNE,3701) OPNAM(I),OPERDESC(I)(1:69)
                  IF(OPERDESC(I)(1:8).NE.'        ')
     1            CALL SHOWIT(0)
                  IF(OPERDESC(I)(1:8).NE.'        ')
     1            WRITE(OUTLYNE,4701) OPNAM(I),OPERDESC(I)(1:69)
              END IF
          END DO
 901      FORMAT('(COMPENSATOR VARIABLE DATA)')
 1901     FORMAT('(COMPENSATOR VARIABLE DATA)')
          IF(CRITTERS) WRITE(OUTLYNE,901)
          IF(CRITTERS) CALL SHOWIT(0)
          IF(CRITTERS) WRITE(19,1901)
 900      FORMAT('COMPENSATOR VARIABLE(#',I3,') = ',
     1    A8,'MOTION = ',1X,G15.8)
 1900     FORMAT('COMPENSATOR VARIABLE(#',I3,') = ',
     1    A8,'MOTION = ',1X,G15.8)
 902      FORMAT('COMPENSATOR VARIABLE SURFACE #',I3)
 1902     FORMAT('COMPENSATOR VARIABLE SURFACE #',I3)
          DO I=1,VBCNT
              WRITE(OUTLYNE,900) I,VARNAM(I),MOT(I)
              CALL SHOWIT(0)
              WRITE(19,1900) I,VARNAM(I),MOT(I)
              WRITE(OUTLYNE,902) CMPSURF(I)
              CALL SHOWIT(0)
              WRITE(19,1902) CMPSURF(I)
          END DO
C
          CALL TOLREST
C
          RETURN
      END
      SUBROUTINE SENSIHDR
          IMPLICIT NONE
C
!      LOGICAL NOPRT
C
!      INTEGER I,II
C
          INCLUDE 'datsub.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          INCLUDE 'datmac.inc'
C
C     INITIALIZE THE SENIOUT.DAT FILE
C     WHICH IS INITIALIZED THE SAME AS EDITTEXT.DAT WITH APPEND TRUE
C     NOW OPEN SENSIOUT.DAT
          OPEN(UNIT=19,ACCESS='APPEND',BLANK='NULL'
     1      ,FORM='FORMATTED',FILE=trim(HOME)//'SENSIOUT.DAT'
     2      ,STATUS='UNKNOWN')
C
C     PRINT HEADER INFORMATION
          WRITE(OUTLYNE,100)
          CALL SHOWIT(0)
          WRITE(19,1100)
 100      FORMAT(23x,'****** SENSITIVITY ANALYSIS ******')
 1100     FORMAT(23x,'****** SENSITIVITY ANALYSIS ******')
          WRITE(OUTLYNE,101)
          CALL SHOWIT(0)
          WRITE(19,1101)
 101      FORMAT(' ')
 1101     FORMAT(' ')
C
          RETURN
      END
      SUBROUTINE INVSENSIHDR
          IMPLICIT NONE
C
!      LOGICAL NOPRT
C
!      INTEGER I,II
C
          INCLUDE 'datsub.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          INCLUDE 'datmac.inc'
C
C     INITIALIZE THE ISENSOUT.DAT FILE
C     WHICH IS INITIALIZED THE SAME AS EDITTEXT.DAT WITH APPEND TRUE
C     NOW OPEN ISENSOUT.DAT
          OPEN(UNIT=19,ACCESS='APPEND',BLANK='NULL'
     1      ,FORM='FORMATTED',FILE=trim(HOME)//'ISENSOUT.DAT'
     2      ,STATUS='UNKNOWN')
C
C     PRINT HEADER INFORMATION
C
          WRITE(OUTLYNE,100)
          CALL SHOWIT(0)
          WRITE(19,1100)
 100      FORMAT(19x,'****** INVERSE SENSITIVITY ANALYSIS ******')
 1100     FORMAT(19x,'****** INVERSE SENSITIVITY ANALYSIS ******')
          WRITE(OUTLYNE,101)
          CALL SHOWIT(0)
          WRITE(19,1101)
 101      FORMAT(' ')
 1101     FORMAT(' ')
C
          RETURN
      END
      SUBROUTINE SENSIEND
          IMPLICIT NONE
C
!      INTEGER II
C
          INCLUDE 'datsub.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          INCLUDE 'datmac.inc'
C
C     PRINT HEADER INFORMATION
C
          WRITE(OUTLYNE,300)
          CALL SHOWIT(0)
          WRITE(19,1300)
 300      FORMAT('**************************************************',
     1    '****************************')
 1300     FORMAT('**************************************************',
     1    '****************************')
          WRITE(OUTLYNE,100)
          CALL SHOWIT(0)
          WRITE(19,1100)
 100      FORMAT(18x,'****** SENSITIVITY ANALYSIS COMPLETED ******')
 1100     FORMAT(18x,'****** SENSITIVITY ANALYSIS COMPLETED ******')
C
C     CLOSE SENIOUT.DAT FILE
C
          CALL CLOSE_FILE(19,1)
C
          RETURN
      END
      SUBROUTINE INVSENSIEND
          IMPLICIT NONE
C
          INCLUDE 'datsub.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          INCLUDE 'datmac.inc'
C
C     PRINT HEADER INFORMATION
C
          WRITE(OUTLYNE,300)
          CALL SHOWIT(0)
          WRITE(19,1300)
 300      FORMAT('**************************************************',
     1    '****************************')
 1300     FORMAT('**************************************************',
     1    '****************************')
          WRITE(OUTLYNE,100)
          CALL SHOWIT(0)
          WRITE(19,1100)
 100      FORMAT(14x,
     1    '****** INVERSE SENSITIVITY ANALYSIS COMPLETED ******')
 1100     FORMAT(14x,
     1    '****** INVERSE SENSITIVITY ANALYSIS COMPLETED ******')
C
C     CLOSE SENIOUT.DAT FILE
C
          CALL CLOSE_FILE(19,1)
C
          RETURN
      END
      SUBROUTINE FOCOMP
          IMPLICIT NONE
C
          INTEGER I,II,IVI
C
          REAL*8 MOT(1:10)
C
          COMMON/LOCOMOTION/MOT
C
          REAL*8 CRITCENT(1:10),OCRIT(10),NCRIT(10),CRITCENT2(1:10)
C
          COMMON/ONCRIT/OCRIT,NCRIT
C
          COMMON/CENTCRIT/CRITCENT,CRITCENT2
C
          LOGICAL DORET
          LOGICAL ITERROR
C
          INCLUDE 'datsub.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          INCLUDE 'datmac.inc'
C
C     THIS DOES ONE PASS OF FOCUSING DURING TOLERANCING
C
C     SET UP ALL THE OPTIMIZATION PARAMETERS
          DEREXT=.FALSE.
          ITERROR=.FALSE.
          CALL ITER(0,0,ITERROR)
          DORET=.TRUE.
          VBCNT=0
          OPCNT=0
          DO I=1,MAXFOCRIT
              IF(ISCRIT(I)) DORET=.FALSE.
              IF(ISCRIT(I)) OPCNT=OPCNT+1
          END DO
          DO I=MAXCMP,1,-1
              IF(ISCOMP(I)) THEN
                  VBCNT=I
                  IVI=I
                  GO TO 13
              END IF
          END DO
 13       CONTINUE
          IF(DORET) RETURN
C     THE ORIGINAL FOCRIT VALUES ARE:
C     RUN TWICE AS MANY CYCLES OF ITER POWELL AS THERE ARE
C     COMPENSATORS. CHANGED ON 3/13/2006
          SAVE_KDP(34)=SAVEINPT(34)
          WRITE(INPUT,*) 'IT P,',MAXCMP
          CALL PROCES
          REST_KDP(34)=RESTINPT(34)
C
C                  CALL ITERIP(5,IVI,ITERROR)
C     NOW EVALUATE THE OPERANDS AGAIN
C     CALCULATE OPERANDS AND LOAD THEM
          OPCALC_TYPE=1
          CALL OPCALC
          IF(F31.EQ.0) RETURN
          CALL OPLOAD
          IF(F31.EQ.0) RETURN

          DO II=1,MAXFOCRIT
              IF(ISCRIT(II)) NCRIT(II)=OPERND(II,4)
          END DO
C     HERE THE CHANGES ARE COMPUTED.
          DO II=1,MAXFOCRIT
              IF(ISCRIT(II)) CRITCENT(II)=
     1        ((NCRIT(II)-OCRIT(II)))
              IF(.NOT.ISCRIT(II)) CRITCENT(II)=0.0D0
              IF(ISCRIT(II)) CRITCENT2(II)=
     1        (((NCRIT(II)-OCRIT(II)))/OCRIT(II))*100.0D0
              IF(.NOT.ISCRIT(II)) CRITCENT2(II)=0.0D0
          END DO
          IF(F31.EQ.0) RETURN
          SOLEXT=.TRUE.
C
C     NOW SET ALL THE CURRENT VARIABLES TO THEIR ORIGINAL VALUE
          VARABL(1:MAXCMP,4)=VARABL(1:MAXCMP,13)
          RETURN
      END
C SUB SENSI.FOR
      SUBROUTINE SENSI(ITY)
C
          IMPLICIT NONE
C
          CHARACTER PNAME*8
C
          COMMON/NAMEP/PNAME
C
          INTEGER IJ,CALLNUM,OLDCALL,ITY,NVN,I,J,VTYPE,II
C
          LOGICAL SENSERR,SECONDCALL
C
          COMMON/ERRSENS/SENSERR
C
          COMMON/CALLCNT/SECONDCALL,CALLNUM,OLDCALL
C
          INTEGER AM1,AM2,AM3,AM4,AM5,AM6,AM7,AM8,AM9,AM10
C
          CHARACTER LICA*80,LIA*80,GLANMA*13,ALBL*80,LLTYPEA*80,INNIA*80
C
          REAL*8 SYSA,ALENA,SLVA,PIKA,FT01A,MULTCLAPA,MULTCOBSA
C
          REAL*8 AIPOLYX,AIPOLYY
C
          DIMENSION SYSA(:),ALENA(:,:),SLVA(:,:),PIKA(:,:,:),
     1    FT01A(:,:),LICA(:),GLANMA(:,:),ALBL(:),MULTCLAPA(:,:,:),
     2    MULTCOBSA(:,:,:),AIPOLYX(:,:,:),AIPOLYY(:,:,:)
C
          ALLOCATABLE :: SYSA,ALENA,SLVA,PIKA,FT01A,LICA,GLANMA,ALBL
     1    ,MULTCLAPA,MULTCOBSA,AIPOLYX,AIPOLYY
C
          LOGICAL TORRY
C
          LOGICAL COMPYES,TOLYES
C
          COMMON/YESCOMP/COMPYES
C
          COMMON/YESTOL/TOLYES
C
          REAL*8 CHGVAL(1:10),V1,V2,V3,V4,OPVVAL(10)
C
          INTEGER VARSURF,VARSURF2,VARSURF3
C
          COMMON/OUTP1/CHGVAL,V1,V4,NVN,VTYPE,VARSURF,OPVVAL,VARSURF2,
     1    VARSURF3
C
          REAL*8 OCRIT(10),NCRIT(10)
C
          COMMON/ONCRIT/OCRIT,NCRIT
C
          INTEGER CMPSURF(10),ALLOERR
C
          COMMON/SURFCMP/CMPSURF
C
          LOGICAL PLL
C
          COMMON/PLLPLL/PLL
C
          INCLUDE 'datsub.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          INCLUDE 'datmac.inc'
C
          CALLNUM=0
          OLDCALL=0
C
C       THIS IS SUBROUTINE SENSI. THIS IS THE SUBROUTINE WHICH
C       HANDLES A SENSITIVITY ANALYSIS
C
C     PREPARE THE ARCHIEVE ARRAYS
C
C     ALLOCATE
          AM1=SSIZ
          AM2=LSIZ
          AM3=MAXSUR
          AM4=PSIZ
          AM5=1
          AM6=9
          AM7=6
          AM8=96
          AM9=2
          AM10=0
          DEALLOCATE(SYSA,ALENA,SLVA
     1    ,PIKA,FT01A,LICA,AIPOLYX,AIPOLYY,
     2    GLANMA,ALBL,MULTCLAPA,MULTCOBSA,STAT=ALLOERR)
          ALLOCATE(SYSA(AM1),ALENA(AM2,AM10:AM3),SLVA(AM5:AM6,AM10:AM3)
     1    ,PIKA(AM7,AM10:AM3,AM4),FT01A(AM8,AM10:AM3),LICA(AM6),
     2    GLANMA(AM10:AM3,AM9),ALBL(AM10:AM3)
     3    ,MULTCLAPA(1:1000,1:3,0:AM3),MULTCOBSA(1:1000,1:3,0:AM3)
     4    ,AIPOLYX(1:200,AM10:AM3,1:4),AIPOLYY(1:200,AM10:AM3,1:4)
     4    ,STAT=ALLOERR)
C       BEFORE READING IN THE LENS LIBRARY, WE MUST DO SOME
C       THINGS WITH THE CURRENT LENS.
          SYSA(1:AM1)=0.0D0
          LICA(1:AM6)(1:80)=' '
          ALBL(AM10:AM3)(1:80)=' '
          ALENA(1:AM2,AM10:AM3)=0.0D0
          AIPOLYX(1:200,AM10:AM3,1:4)=0.0D0
          AIPOLYY(1:200,AM10:AM3,1:4)=0.0D0
          MULTCLAPA(1:1000,1:3,0:AM3)=0.0D0
          MULTCOBSA(1:1000,1:3,0:AM3)=0.0D0
          SLVA(AM5:AM6,AM10:AM3)=0.0D0
          PIKA(1:AM7,AM10:AM3,1:AM4)=0.0D0
          FT01A(1:AM8,AM10:AM3)=0.0D0
          GLANMA(AM10:AM3,1:AM9)='             '
C
C       NOW DELETE ALL BUT THE MAIN CFG
          SYSTEM1(50)=1.0D0
          SYSTEM1(56)=1.0D0
C
C
C       NOW SAVE LENS 1, MAIN CONFIG, TO THE ACHIEVE LENS STORAGE
          CALL CTOA(AM1,AM2,AM3,AM4,AM5,AM6,AM7,AM8,AM9,AM10
     1    ,SYSA,ALENA,SLVA,PIKA,FT01A,LIA,LICA,GLANMA,ALBL,LLTYPEA,INNIA
     2    ,MULTCLAPA,MULTCOBSA,AIPOLYX,AIPOLYY)
C
C     NOW WE CHECK FOR VALID INPUT
C     SENSI TAKES NO INPUT
C
          IF(STI.EQ.1) THEN
              IF(ITY.EQ.0) WRITE(OUTLYNE,*)
     1        '"SENSI" INITIATES A SENSITIVITY ANALYSIS'
              IF(ITY.EQ.1) WRITE(OUTLYNE,*)
     1        '"INVSENSI" INITIATES AN INVERSE SENSITIVITY ANALYSIS'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SST.EQ.1.OR.SN.EQ.1.OR.SQ.EQ.1) THEN
              IF(ITY.EQ.0) WRITE(OUTLYNE,*)
     1        '"SENSI" TAKES NO INPUT'
              IF(ITY.EQ.1) WRITE(OUTLYNE,*)
     1        '"INVSENSI" TAKES NO INPUT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
C     NOW DO SENSI
C     CHECK THAT STUFF EXISTS
          IF(TVBCNT.EQ.0) THEN
              IF(ITY.EQ.0) WRITE(OUTLYNE,*)
     1        '"SENSI" REQUIRES TOLERANCE VARIABLES TO BE DEFINED'
              IF(ITY.EQ.1) WRITE(OUTLYNE,*)
     1        '"INVSENSI" REQUIRES TOLERANCE VARIABLES TO BE DEFINED'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'NO ACTION TAKEN'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
          TOLYES=.FALSE.
          DO I=1,MAXTOP
              IF(ISTOP(I)) THEN
C     SET TOLOPS NOMINAL VALUES
C     COMPYES TRUE MEANS WE CALCULATE FOCRITS IN OPCALC
                  TOLYES=.TRUE.
              END IF
          END DO
C
          COMPYES=.FALSE.
          DO I=1,MAXFOCRIT
              IF(ISCRIT(I)) THEN
C     SET COMPYES
C     COMPYES TRUE MEANS WE CALCULATE FOCRITS IN OPCALC
                  COMPYES=.TRUE.
              END IF
          END DO
          IF(.NOT.TOLYES.AND.TVBCNT.NE.0) THEN
              IF(ITY.EQ.0) WRITE(OUTLYNE,*)
     1        '"SENSI" REQUIRES TOLERANCE OPERANDS TO BE DEFINED'
              IF(ITY.EQ.1) WRITE(OUTLYNE,*)
     1        '"INVSENSI" REQUIRES TOLERANCE OPERANDS TO BE DEFINED'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'NO ACTION TAKEN'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(TVBCNT.EQ.0.AND.TOLYES) THEN
              IF(ITY.EQ.0) WRITE(OUTLYNE,*)
     1        '"SENSI" REQUIRES TOLERANCE VARIABLES TO BE DEFINED'
              IF(ITY.EQ.1) WRITE(OUTLYNE,*)
     1        '"INVSENSI" REQUIRES TOLERANCE VARIABLES TO BE DEFINED'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'NO ACTION TAKEN'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(TVBCNT.EQ.0.AND..NOT.TOLYES) THEN
              IF(ITY.EQ.0) WRITE(OUTLYNE,*)
     1        '"SENSI" REQUIRES TOLERANCE VARIABLES AND OPERANDS TO BE DEFINED'
              IF(ITY.EQ.1) WRITE(OUTLYNE,*)
     1        '"SENSI" REQUIRES TOLERANCE VARIABLES AND OPERANDS TO BE DEFINED'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'NO ACTION TAKEN'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
          IF(COMPYES) THEN
C     THERE ARE FOCRIT OPERANDS, EVALUATE THEM AND SET
C     THE NOMINALS TO BE THE CURRENT VALUES. THIS IS DONE JUST ONCE.
C     EACH TIME THE LENS IS RESTORED TO THE ORIGINAL LENS
              OPCALC_TYPE=1
              CALL OPCALC
              IF(F31.EQ.0) RETURN
              CALL OPLOAD
              IF(F31.EQ.0) RETURN
              DO II=1,MAXFOCRIT
                  IF(ISCRIT(II)) OCRIT(II)=OPERND(II,4)
              END DO
          END IF
C
          IF(TOLYES) THEN
C     THERE ARE TOLERANCE OPERANDS, EVALUATE THEM AND SET
C     THE NOMINALS TO BE THE CURRENT VALUES. THIS IS DONE JUST ONCE.
C     EACH TIME THE LENS IS RESTORED TO THE ORIGINAL LENS
              OPCALC_TYPE=2
              CALL OPCALC
              IF(F31.EQ.0) RETURN
              CALL OPLOAD
              IF(F31.EQ.0) RETURN
C     THE TOLOP NOMINALS HAVE BEEN SET
              DO II=MAXFOCRIT+1,MAXFOCRIT+MAXTOP
                  OLDOP(II,1:20)=OPERND(II,1:20)
              END DO
          END IF
C
C     SET FLAG THAT TRACKS IF COMPENSATION MUST BE DONE
          COMPYES=.FALSE.
          DO IJ=1,MAXFOCRIT
              IF(ISCOMP(IJ).AND.ISCRIT(IJ)) THEN
C     COMPYES TRUE MEANS WE CALCULATE FOCRITS IN OPCALC
                  COMPYES=.TRUE.
              END IF
          END DO
          IF(COMPYES) THEN
C     SET MAX COUNTERS FOR OPTIMIZATION
C                  NO DERIVATIVES EXIST
C     THERE ARE COMPENSATOR OPERANDS, EVALUATE THEM AND SET
C     THE TARGETS TO BE THE CURRENT VALUES. THIS IS DONE JUST ONCE.
              DO II=1,MAXCMP
                  OPERND(II,2)=OCRIT(II)
              END DO
          END IF
C     THE COMPENSATOR TARGETS HAVE BEEN SET
C
C     PRINT THE COMPENSATOR TARGETS
C
C     NOW ALL OPERANDS HAVE THEIR ORIGINAL VALUES OR TARGET VALUES
C     AND AT LEAST TVARS AND TOPERS EXIST
C
C     IF INVERSE SENSI, CHECK TVAL VALUES
          DO I=1,MAXTOP
              II=I+MAXFOCRIT
              IF(ISTOP(I))THEN
                  IF(OPERND(II,20).EQ.0.0D0.AND.ITY.NE.0) THEN
                      WRITE(OUTLYNE,*)
     1                'AN EXPLICIT "tval" MUST BE ENTERED FOR EACH TOLERANCE OPERAND'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'BEFORE AN INVERSE SENSITIVITY ANALYSIS CAN BE PERFORMED'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'NO ACTION TAKEN'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
          END DO
C
C     NOW DO A SENSI
C
C     WRITE OUT THE HEADER INFORMATION
C
          IF(ITY.EQ.0) CALL SENSIHDR
          IF(ITY.EQ.1) CALL INVSENSIHDR
C
C     WE CHANGE A VARIABLE, THEN REFOCUS, THE RECALC ALL TOPERS AND
C     CALCULATE TOPER CHANGES. THEN OUTPUT TVAR CHANGE, TOPER CHANGES,
C     FOCRIT CHANGE AND COMPVAR MOTION.
C     THEN DO NEXT VARIABLE.
C
          DO I=1+MAXCMP,TVBCNT+MAXCMP
              NVN=I-MAXCMP
C     GET THE DATA TYPE NUMBER OF THE VARIABLE
              VTYPE=INT(VARABL(I,1))
              VARSURF=INT(VARABL(I,3))
              IF(VTYPE.GE.154.AND.VTYPE.LE.156) VARSURF2=INT(VARABL(I,7))
              IF(VTYPE.GE.160.AND.VTYPE.LE.164) VARSURF2=INT(VARABL(I,7))
              IF(VTYPE.EQ.163.OR.VTYPE.LE.164)  VARSURF3=INT(VARABL(I,12))
C     NVN COUNTS THE NUMBER OF TOL VARAIBLES AND IS PASSED TO
C     THE OUTPUT ROUTINE
C
C     HERE IS WHERE WE DO THE CMD LEVEL VARIABLES LIKE STILT,BTILT,DISP
C     AND ROLL
C
C
C
C
C              COMPOUND VARIABLES
C
C
C
C
              IF(VTYPE.GE.157.AND.VTYPE.LE.159) THEN
C
C     PLUS DELTA
C     NEW VALUE IS:
C     UPDATE THE LENS
                  SURF=INT(VARABL(I,3))
                  SAVE_KDP(7)=SAVEINPT(7)
                  IF(VTYPE.EQ.157)PNAME='STILTA  '
                  IF(VTYPE.EQ.158)PNAME='STILTB  '
                  IF(VTYPE.EQ.159)PNAME='STILTG  '
C     DO A PIVOT
                  WC='STILT   '
                  WQ='PIVOT   '
                  SQ=1
                  S1=1
                  S2=1
                  S3=1
                  S4=0
                  S5=0
                  DF1=0
                  DF2=0
                  DF3=0
                  DF4=1
                  DF5=1
                  SN=1
                  SST=0
                  W1=VARABL(I,9)
                  W2=VARABL(I,10)
                  W3=VARABL(I,11)
                  CALL HEXSTILT
C     DO THE STILT
                  WC='STILT   '
                  WQ='        '
                  SQ=0
                  S1=1
                  S2=1
                  S3=1
                  S4=1
                  S5=0
                  DF1=0
                  DF2=0
                  DF3=0
                  DF4=0
                  DF5=1
                  SN=1
                  SST=0
                  W1=VARABL(I,3)
                  IF(VTYPE.EQ.157) THEN
                      W2=DABS(VARABL(I,8))
                      W3=0.0D0
                      W4=0.0D0
                  END IF
                  IF(VTYPE.EQ.158) THEN
                      W2=0.0D0
                      W3=DABS(VARABL(I,8))
                      W4=0.0D0
                  END IF
                  IF(VTYPE.EQ.159) THEN
                      W2=0.0D0
                      W3=0.0D0
                      W4=DABS(VARABL(I,8))
                  END IF
                  V1=VARABL(I,8)
                  V4=V1
                  CALL HEXSTILT
                  REST_KDP(7)=RESTINPT(7)
                  LNSTYP=3
                  F6=1
                  F1=0
                  F22=0
                  CALL LNSEOS
C     FOCUS COMPENSATION GOES HERE
                  CALL FOCOMP
C     CALCULATE OPERANDS AND LOAD THEM
                  OPCALC_TYPE=2
                  CALL OPCALC
                  IF(F31.EQ.0) RETURN
                  CALL OPLOAD
                  IF(F31.EQ.0) RETURN
C     FOR EACH OPERAND, CALCULATE AND STORE THE CHANGE
                  DO J=MAXFOCRIT+1,MAXTOP+MAXFOCRIT
                      IF(ISTOP(J-MAXFOCRIT)) THEN
                          CHGVAL(J-MAXFOCRIT)=(OPERND(J,4)-OLDOP(J,4))
                          OPVVAL(J-MAXFOCRIT)=OPERND(J,4)
C     LOOP TO NEXT CHGVAL
                      END IF
                  END DO
                  IF(ITY.EQ.0) CALL SENSIOUT
                  IF(ITY.EQ.1) CALL INVSENSIOUT(I)
C     DONE FOR VARIABLE I PLUS
C     RESTORE THE LENS
                  F6=1
                  F1=0
                  F22=0
                  CALL ATOC(AM1,AM2,AM3,AM4,AM5,AM6,AM7,AM8,AM9,AM10
     1            ,SYSA,ALENA,SLVA,PIKA,FT01A,LIA,LICA,GLANMA,ALBL,LLTYPEA,INNIA
     2            ,MULTCLAPA,MULTCOBSA,AIPOLYX,AIPOLYY)
                  LNSTYP=3
                  CALL LNSEOS
C                       PLUS DELTA DONE
C     MINUS DELTA
C     NEW VALUE IS:
C     UPDATE THE LENS
                  SURF=INT(VARABL(I,3))
                  SAVE_KDP(7)=SAVEINPT(7)
                  IF(VTYPE.EQ.157)PNAME='STILTA  '
                  IF(VTYPE.EQ.158)PNAME='STILTB  '
                  IF(VTYPE.EQ.159)PNAME='STILTG  '
C     DO A PIVOT
                  WC='STILT   '
                  WQ='PIVOT   '
                  SQ=1
                  S1=1
                  S2=1
                  S3=1
                  S4=0
                  S5=0
                  DF1=0
                  DF2=0
                  DF3=0
                  DF4=1
                  DF5=1
                  SN=1
                  SST=0
                  W1=VARABL(I,9)
                  W2=VARABL(I,10)
                  W3=VARABL(I,11)
                  CALL HEXSTILT
C     DO THE TILT
                  WC='STILT   '
                  WQ='        '
                  SQ=0
                  S1=1
                  S2=1
                  S3=1
                  S4=1
                  S5=0
                  DF1=0
                  DF2=0
                  DF3=0
                  DF4=0
                  DF5=1
                  SN=1
                  SST=0
                  W1=VARABL(I,3)
                  IF(VTYPE.EQ.157) THEN
                      W2=-DABS(VARABL(I,8))
                      W3=0.0D0
                      W4=0.0D0
                  END IF
                  IF(VTYPE.EQ.158) THEN
                      W2=0.0D0
                      W3=-DABS(VARABL(I,8))
                      W4=0.0D0
                  END IF
                  IF(VTYPE.EQ.159) THEN
                      W2=0.0D0
                      W3=0.0D0
                      W4=-DABS(VARABL(I,8))
                  END IF
                  V1=-VARABL(I,8)
                  V4=V1
                  CALL HEXSTILT
                  REST_KDP(7)=RESTINPT(7)
                  LNSTYP=3
                  F6=1
                  F1=0
                  F22=0
                  CALL LNSEOS
C     FOCUS COMPENSATION GOES HERE
                  CALL FOCOMP
C     CALCULATE OPERANDS AND LOAD THEM
                  OPCALC_TYPE=2
                  CALL OPCALC
                  IF(F31.EQ.0) RETURN
                  CALL OPLOAD
                  IF(F31.EQ.0) RETURN
C     FOR EACH OPERAND, CALCULATE AND STORE THE CHANGE
                  DO J=MAXFOCRIT+1,MAXTOP+MAXFOCRIT
                      IF(ISTOP(J-MAXFOCRIT)) THEN
                          CHGVAL(J-MAXFOCRIT)=(OPERND(J,4)-OLDOP(J,4))
                          OPVVAL(J-MAXFOCRIT)=OPERND(J,4)
C     LOOP TO NEXT CHGVAL
                      END IF
                  END DO
                  IF(ITY.EQ.0) CALL SENSIOUT
                  IF(ITY.EQ.1) CALL INVSENSIOUT(I)
C     DONE FOR VARIABLE I MINUS
C     RESTORE THE LENS
                  F6=1
                  F1=0
                  F22=0
                  CALL ATOC(AM1,AM2,AM3,AM4,AM5,AM6,AM7,AM8,AM9,AM10
     1            ,SYSA,ALENA,SLVA,PIKA,FT01A,LIA,LICA,GLANMA,ALBL,LLTYPEA,INNIA
     2            ,MULTCLAPA,MULTCOBSA,AIPOLYX,AIPOLYY)
                  LNSTYP=3
                  CALL LNSEOS
C                       MINUS DELTA DONE
              END IF
              IF(VTYPE.GE.160.AND.VTYPE.LE.162) THEN
C
C     PLUS DELTA
C     NEW VALUE IS:
C     UPDATE THE LENS
                  SURF=INT(VARABL(I,3))
                  SAVE_KDP(7)=SAVEINPT(7)
                  IF(VTYPE.EQ.160)PNAME='BTILTA  '
                  IF(VTYPE.EQ.161)PNAME='BTILTB  '
                  IF(VTYPE.EQ.162)PNAME='BTILTG  '
C     DO A PIVOT
                  WC='BTILT   '
                  WQ='PIVOT   '
                  SQ=1
                  S1=1
                  S2=1
                  S3=1
                  S4=0
                  S5=0
                  DF1=0
                  DF2=0
                  DF3=0
                  DF4=1
                  DF5=1
                  SN=1
                  SST=0
                  W1=VARABL(I,9)
                  W2=VARABL(I,10)
                  W3=VARABL(I,11)
                  CALL HEXBTILT
C     DO THE BTILT
                  WC='BTILT   '
                  WQ='        '
                  SQ=0
                  S1=1
                  S2=1
                  S3=1
                  S4=1
                  S5=1
                  DF1=0
                  DF2=0
                  DF3=0
                  DF4=0
                  DF5=0
                  SN=1
                  SST=0
                  W1=VARABL(I,3)
                  W2=VARABL(I,7)
                  IF(VTYPE.EQ.160) THEN
                      W3=DABS(VARABL(I,8))
                      W4=0.0D0
                      W5=0.0D0
                  END IF
                  IF(VTYPE.EQ.161) THEN
                      W3=0.0D0
                      W4=DABS(VARABL(I,8))
                      W5=0.0D0
                  END IF
                  IF(VTYPE.EQ.162) THEN
                      W3=0.0D0
                      W4=0.0D0
                      W5=DABS(VARABL(I,8))
                  END IF
                  V1=VARABL(I,8)
                  V4=V1
                  CALL HEXBTILT
                  REST_KDP(7)=RESTINPT(7)
                  LNSTYP=3
                  F6=1
                  F1=0
                  F22=0
                  CALL LNSEOS
C     FOCUS COMPENSATION GOES HERE
                  CALL FOCOMP
C     CALCULATE OPERANDS AND LOAD THEM
                  OPCALC_TYPE=2
                  CALL OPCALC
                  IF(F31.EQ.0) RETURN
                  CALL OPLOAD
                  IF(F31.EQ.0) RETURN
C     FOR EACH OPERAND, CALCULATE AND STORE THE CHANGE
                  DO J=MAXFOCRIT+1,MAXTOP+MAXFOCRIT
                      IF(ISTOP(J-MAXFOCRIT)) THEN
                          CHGVAL(J-MAXFOCRIT)=(OPERND(J,4)-OLDOP(J,4))
                          OPVVAL(J-MAXFOCRIT)=OPERND(J,4)
C     LOOP TO NEXT CHGVAL
                      END IF
                  END DO
                  IF(ITY.EQ.0) CALL SENSIOUT
                  IF(ITY.EQ.1) CALL INVSENSIOUT(I)
C     DONE FOR VARIABLE I PLUS
C     RESTORE THE LENS
                  F6=1
                  F1=0
                  F22=0
                  CALL ATOC(AM1,AM2,AM3,AM4,AM5,AM6,AM7,AM8,AM9,AM10
     1            ,SYSA,ALENA,SLVA,PIKA,FT01A,LIA,LICA,GLANMA,ALBL,LLTYPEA,INNIA
     2            ,MULTCLAPA,MULTCOBSA,AIPOLYX,AIPOLYY)
                  LNSTYP=3
                  CALL LNSEOS
C                       PLUS DELTA DONE
C     MINUS DELTA
C     NEW VALUE IS:
C     UPDATE THE LENS
                  SURF=INT(VARABL(I,3))
                  SAVE_KDP(7)=SAVEINPT(7)
                  IF(VTYPE.EQ.160)PNAME='BTILTA  '
                  IF(VTYPE.EQ.161)PNAME='BTILTB  '
                  IF(VTYPE.EQ.162)PNAME='BTILTG  '
C     DO A PIVOT
                  WC='BTILT   '
                  WQ='PIVOT   '
                  SQ=1
                  S1=1
                  S2=1
                  S3=1
                  S4=0
                  S5=0
                  DF1=0
                  DF2=0
                  DF3=0
                  DF4=1
                  DF5=1
                  SN=1
                  SST=0
                  W1=VARABL(I,9)
                  W2=VARABL(I,10)
                  W3=VARABL(I,11)
                  CALL HEXBTILT
C     DO THE TILT
                  WC='BTILT   '
                  WQ='        '
                  SQ=0
                  S1=1
                  S2=1
                  S3=1
                  S4=1
                  S5=1
                  DF1=0
                  DF2=0
                  DF3=0
                  DF4=0
                  DF5=0
                  SN=1
                  SST=0
                  W1=VARABL(I,3)
                  W2=VARABL(I,7)
                  IF(VTYPE.EQ.160) THEN
                      W3=-DABS(VARABL(I,8))
                      W4=0.0D0
                      W5=0.0D0
                  END IF
                  IF(VTYPE.EQ.161) THEN
                      W3=0.0D0
                      W4=-DABS(VARABL(I,8))
                      W5=0.0D0
                  END IF
                  IF(VTYPE.EQ.162) THEN
                      W3=0.0D0
                      W4=0.0D0
                      W5=-DABS(VARABL(I,8))
                  END IF
                  V1=-VARABL(I,8)
                  V4=V1
                  CALL HEXBTILT
                  REST_KDP(7)=RESTINPT(7)
                  LNSTYP=3
                  F6=1
                  F1=0
                  F22=0
                  CALL LNSEOS
C     FOCUS COMPENSATION GOES HERE
                  CALL FOCOMP
C     CALCULATE OPERANDS AND LOAD THEM
                  OPCALC_TYPE=2
                  CALL OPCALC
                  IF(F31.EQ.0) RETURN
                  CALL OPLOAD
                  IF(F31.EQ.0) RETURN
C     FOR EACH OPERAND, CALCULATE AND STORE THE CHANGE
                  DO J=MAXFOCRIT+1,MAXTOP+MAXFOCRIT
                      IF(ISTOP(J-MAXFOCRIT)) THEN
                          CHGVAL(J-MAXFOCRIT)=(OPERND(J,4)-OLDOP(J,4))
                          OPVVAL(J-MAXFOCRIT)=OPERND(J,4)
C     LOOP TO NEXT CHGVAL
                      END IF
                  END DO
                  IF(ITY.EQ.0) CALL SENSIOUT
                  IF(ITY.EQ.1) CALL INVSENSIOUT(I)
C     DONE FOR VARIABLE I PLUS
C     RESTORE THE LENS
                  F6=1
                  F1=0
                  F22=0
                  CALL ATOC(AM1,AM2,AM3,AM4,AM5,AM6,AM7,AM8,AM9,AM10
     1            ,SYSA,ALENA,SLVA,PIKA,FT01A,LIA,LICA,GLANMA,ALBL,LLTYPEA,INNIA
     2            ,MULTCLAPA,MULTCOBSA,AIPOLYX,AIPOLYY)
                  LNSTYP=3
                  CALL LNSEOS
C                       MINUS DELTA DONE
              END IF
              IF(VTYPE.GE.154.AND.VTYPE.LE.156) THEN
C
C     PLUS DELTA
C     NEW VALUE IS:
C     UPDATE THE LENS
                  SURF=INT(VARABL(I,3))
                  SAVE_KDP(7)=SAVEINPT(7)
                  IF(VTYPE.EQ.154)PNAME='DISPX   '
                  IF(VTYPE.EQ.155)PNAME='DISPY   '
                  IF(VTYPE.EQ.156)PNAME='DISPZ   '
                  WC='DISP    '
C     DO THE DISP
                  WQ='        '
                  SQ=0
                  S1=1
                  S2=1
                  S3=1
                  S4=1
                  S5=1
                  DF1=0
                  DF2=0
                  DF3=0
                  DF4=0
                  DF5=0
                  SN=1
                  SST=0
                  W1=VARABL(I,3)
                  W2=VARABL(I,7)
                  IF(VTYPE.EQ.154) THEN
                      W3=DABS(VARABL(I,8))
                      W4=0.0D0
                      W5=0.0D0
                  END IF
                  IF(VTYPE.EQ.155) THEN
                      W3=0.0D0
                      W4=DABS(VARABL(I,8))
                      W5=0.0D0
                  END IF
                  IF(VTYPE.EQ.156) THEN
                      W3=0.0D0
                      W4=0.0D0
                      W5=DABS(VARABL(I,8))
                  END IF
                  V1=VARABL(I,8)
                  V4=V1
                  CALL HEXDISP
                  REST_KDP(7)=RESTINPT(7)
                  LNSTYP=3
                  F6=1
                  F1=0
                  F22=0
                  CALL LNSEOS
C     FOCUS COMPENSATION GOES HERE
                  CALL FOCOMP
C     CALCULATE OPERANDS AND LOAD THEM
                  OPCALC_TYPE=2
                  CALL OPCALC
                  IF(F31.EQ.0) RETURN
                  CALL OPLOAD
                  IF(F31.EQ.0) RETURN
C     FOR EACH OPERAND, CALCULATE AND STORE THE CHANGE
                  DO J=MAXFOCRIT+1,MAXTOP+MAXFOCRIT
                      IF(ISTOP(J-MAXFOCRIT)) THEN
                          CHGVAL(J-MAXFOCRIT)=(OPERND(J,4)-OLDOP(J,4))
                          OPVVAL(J-MAXFOCRIT)=OPERND(J,4)
C     LOOP TO NEXT CHGVAL
                      END IF
                  END DO
                  IF(ITY.EQ.0) CALL SENSIOUT
                  IF(ITY.EQ.1) CALL INVSENSIOUT(I)
C     DONE FOR VARIABLE I PLUS
C     RESTORE THE LENS
                  F6=1
                  F1=0
                  F22=0
                  CALL ATOC(AM1,AM2,AM3,AM4,AM5,AM6,AM7,AM8,AM9,AM10
     1            ,SYSA,ALENA,SLVA,PIKA,FT01A,LIA,LICA,GLANMA,ALBL,LLTYPEA,INNIA
     2            ,MULTCLAPA,MULTCOBSA,AIPOLYX,AIPOLYY)
                  LNSTYP=3
                  CALL LNSEOS
C                       PLUS DELTA DONE
C     MINUS DELTA
C     NEW VALUE IS:
C     UPDATE THE LENS
                  SURF=INT(VARABL(I,3))
                  SAVE_KDP(7)=SAVEINPT(7)
                  IF(VTYPE.EQ.154)PNAME='DISPX   '
                  IF(VTYPE.EQ.155)PNAME='DISPY   '
                  IF(VTYPE.EQ.156)PNAME='DISPZ   '
                  WC='DISP    '
C     DO THE DISP
                  WQ='        '
                  SQ=0
                  S1=1
                  S2=1
                  S3=1
                  S4=1
                  S5=1
                  DF1=0
                  DF2=0
                  DF3=0
                  DF4=0
                  DF5=0
                  SN=1
                  SST=0
                  W1=VARABL(I,3)
                  W2=VARABL(I,7)
                  IF(VTYPE.EQ.154) THEN
                      W3=-DABS(VARABL(I,8))
                      W4=0.0D0
                      W5=0.0D0
                  END IF
                  IF(VTYPE.EQ.155) THEN
                      W3=0.0D0
                      W4=-DABS(VARABL(I,8))
                      W5=0.0D0
                  END IF
                  IF(VTYPE.EQ.156) THEN
                      W3=0.0D0
                      W4=0.0D0
                      W5=-DABS(VARABL(I,8))
                  END IF
                  V1=-VARABL(I,8)
                  V4=V1
                  CALL HEXDISP
                  REST_KDP(7)=RESTINPT(7)
                  LNSTYP=3
                  F6=1
                  F1=0
                  F22=0
                  CALL LNSEOS
C     FOCUS COMPENSATION GOES HERE
                  CALL FOCOMP
C     CALCULATE OPERANDS AND LOAD THEM
                  OPCALC_TYPE=2
                  CALL OPCALC
                  IF(F31.EQ.0) RETURN
                  CALL OPLOAD
                  IF(F31.EQ.0) RETURN
C     FOR EACH OPERAND, CALCULATE AND STORE THE CHANGE
                  DO J=MAXFOCRIT+1,MAXTOP+MAXFOCRIT
                      IF(ISTOP(J-MAXFOCRIT)) THEN
                          CHGVAL(J-MAXFOCRIT)=(OPERND(J,4)-OLDOP(J,4))
                          OPVVAL(J-MAXFOCRIT)=OPERND(J,4)
C     LOOP TO NEXT CHGVAL
                      END IF
                  END DO
                  IF(ITY.EQ.0) CALL SENSIOUT
                  IF(ITY.EQ.1) CALL INVSENSIOUT(I)
C     DONE FOR VARIABLE I PLUS
C     RESTORE THE LENS
                  F6=1
                  F1=0
                  F22=0
                  CALL ATOC(AM1,AM2,AM3,AM4,AM5,AM6,AM7,AM8,AM9,AM10
     1            ,SYSA,ALENA,SLVA,PIKA,FT01A,LIA,LICA,GLANMA,ALBL,LLTYPEA,INNIA,
     2            MULTCLAPA,MULTCOBSA,AIPOLYX,AIPOLYY)
                  LNSTYP=3
                  CALL LNSEOS
C                       MINUS DELTA DONE
              END IF
              IF(VTYPE.GE.163.AND.VTYPE.LE.164) THEN
C
C     PLUS DELTA
C     NEW VALUE IS:
C     UPDATE THE LENS
                  SURF=INT(VARABL(I,3))
                  SAVE_KDP(7)=SAVEINPT(7)
                  IF(VTYPE.EQ.163)PNAME='ROLLX   '
                  IF(VTYPE.EQ.164)PNAME='ROLLY   '
                  WC='ROLL    '
C     DO THE ROLL
                  WQ='        '
                  SQ=0
                  S1=1
                  S2=1
                  S3=1
                  S4=1
                  S5=1
                  DF1=0
                  DF2=0
                  DF3=0
                  DF4=0
                  DF5=0
                  SN=1
                  SST=0
                  W1=VARABL(I,3)
                  W2=VARABL(I,7)
                  W5=VARABL(I,12)
                  IF(VTYPE.EQ.163) THEN
                      W3=DABS(VARABL(I,8))
                      W4=0.0D0
                  END IF
                  IF(VTYPE.EQ.164) THEN
                      W3=0.0D0
                      W4=DABS(VARABL(I,8))
                  END IF
                  V1=VARABL(I,8)
                  V4=V1
                  CALL HEXROLL
                  REST_KDP(7)=RESTINPT(7)
                  LNSTYP=3
                  F6=1
                  F1=0
                  F22=0
                  CALL LNSEOS
C     FOCUS COMPENSATION GOES HERE
                  CALL FOCOMP
C     CALCULATE OPERANDS AND LOAD THEM
                  OPCALC_TYPE=2
                  CALL OPCALC
                  IF(F31.EQ.0) RETURN
                  CALL OPLOAD
                  IF(F31.EQ.0) RETURN
C     FOR EACH OPERAND, CALCULATE AND STORE THE CHANGE
                  DO J=MAXFOCRIT+1,MAXTOP+MAXFOCRIT
                      IF(ISTOP(J-MAXFOCRIT)) THEN
                          CHGVAL(J-MAXFOCRIT)=(OPERND(J,4)-OLDOP(J,4))
                          OPVVAL(J-MAXFOCRIT)=OPERND(J,4)
C     LOOP TO NEXT CHGVAL
                      END IF
                  END DO
                  IF(ITY.EQ.0) CALL SENSIOUT
                  IF(ITY.EQ.1) CALL INVSENSIOUT(I)
C     DONE FOR VARIABLE I PLUS
C     RESTORE THE LENS
                  F6=1
                  F1=0
                  F22=0
                  CALL ATOC(AM1,AM2,AM3,AM4,AM5,AM6,AM7,AM8,AM9,AM10
     1            ,SYSA,ALENA,SLVA,PIKA,FT01A,LIA,LICA,GLANMA,ALBL,LLTYPEA,INNIA
     2            ,MULTCLAPA,MULTCOBSA,AIPOLYX,AIPOLYY)
                  LNSTYP=3
                  CALL LNSEOS
C                       PLUS DELTA DONE
C     MINUS DELTA
C     NEW VALUE IS:
C     UPDATE THE LENS
                  SURF=INT(VARABL(I,3))
                  SAVE_KDP(7)=SAVEINPT(7)
                  IF(VTYPE.EQ.163)PNAME='ROLLX   '
                  IF(VTYPE.EQ.164)PNAME='ROLLY   '
                  WC='ROLL    '
C     DO THE ROLL
                  WQ='        '
                  SQ=0
                  S1=1
                  S2=1
                  S3=1
                  S4=1
                  S5=1
                  DF1=0
                  DF2=0
                  DF3=0
                  DF4=0
                  DF5=0
                  SN=1
                  SST=0
                  W1=VARABL(I,3)
                  W2=VARABL(I,7)
                  W5=VARABL(I,12)
                  IF(VTYPE.EQ.163) THEN
                      W3=-DABS(VARABL(I,8))
                      W4=0.0D0
                  END IF
                  IF(VTYPE.EQ.164) THEN
                      W3=0.0D0
                      W4=-DABS(VARABL(I,8))
                  END IF
                  V1=-VARABL(I,8)
                  V4=V1
                  CALL HEXROLL
                  REST_KDP(7)=RESTINPT(7)
                  LNSTYP=3
                  F6=1
                  F1=0
                  F22=0
                  CALL LNSEOS
C     FOCUS COMPENSATION GOES HERE
                  CALL FOCOMP
C     CALCULATE OPERANDS AND LOAD THEM
                  OPCALC_TYPE=2
                  CALL OPCALC
                  IF(F31.EQ.0) RETURN
                  CALL OPLOAD
                  IF(F31.EQ.0) RETURN
C     FOR EACH OPERAND, CALCULATE AND STORE THE CHANGE
                  DO J=MAXFOCRIT+1,MAXTOP+MAXFOCRIT
                      IF(ISTOP(J-MAXFOCRIT)) THEN
                          CHGVAL(J-MAXFOCRIT)=(OPERND(J,4)-OLDOP(J,4))
                          OPVVAL(J-MAXFOCRIT)=OPERND(J,4)
C     LOOP TO NEXT CHGVAL
                      END IF
                  END DO
                  IF(ITY.EQ.0) CALL SENSIOUT
                  IF(ITY.EQ.1) CALL INVSENSIOUT(I)
C     DONE FOR VARIABLE I PLUS
C     RESTORE THE LENS
                  F6=1
                  F1=0
                  F22=0
                  CALL ATOC(AM1,AM2,AM3,AM4,AM5,AM6,AM7,AM8,AM9,AM10
     1            ,SYSA,ALENA,SLVA,PIKA,FT01A,LIA,LICA,GLANMA,ALBL,LLTYPEA,INNIA
     2            ,MULTCLAPA,MULTCOBSA,AIPOLYX,AIPOLYY)
                  LNSTYP=3
                  CALL LNSEOS
C                       MINUS DELTA DONE
              END IF
C
C     DONE WITH COMPOUND VARIABLES
C
C
C
              IF(VTYPE.EQ.1.OR.VTYPE.EQ.2.OR.VTYPE.EQ.134.OR.VTYPE.EQ.135) THEN
C     SURFACE RADIUS OR CURVATURE
C     PLUS DELTA
C     NEW VALUE IS:
C     UPDATE THE LENS
                  F6=1
                  F1=0
                  F22=0
                  SURF=INT(VARABL(I,3))
                  SAVE_KDP(7)=SAVEINPT(7)
                  IF(VTYPE.EQ.2.OR.VTYPE.EQ.135) WC='CV'
                  IF(VTYPE.EQ.1.OR.VTYPE.EQ.134) WC='RD'
                  IF(VTYPE.EQ.1.OR.VTYPE.EQ.2) WQ='CENT'
                  IF(VTYPE.EQ.134.OR.VTYPE.EQ.135) WQ='DELTFR'
                  IF(VTYPE.EQ.2) PNAME='CV'
                  IF(VTYPE.EQ.1) PNAME='RD'
                  IF(VTYPE.EQ.135) PNAME='CV_FR'
                  IF(VTYPE.EQ.134) PNAME='RD_FR'
                  SQ=1
                  S1=1
                  S2=0
                  S3=0
                  S4=0
                  S5=0
                  DF1=0
                  DF2=1
                  DF3=1
                  DF4=1
                  DF5=1
                  SN=1
                  SST=0
                  W1=DABS(VARABL(I,8))
                  V1=W1
                  IF(VTYPE.EQ.1.OR.VTYPE.EQ.134) THEN
                      IF(ALENS(1,SURF).EQ.0.0D0) V2=ALENS(1,SURF)
                      IF(ALENS(1,SURF).NE.0.0D0) V2=1.0D0/ALENS(1,SURF)
                  END IF
                  IF(VTYPE.EQ.2.OR.VTYPE.EQ.135) THEN
                      V2=ALENS(1,SURF)
                  END IF
                  CALL LENUP
                  IF(VTYPE.EQ.1.OR.VTYPE.EQ.134) THEN
                      IF(ALENS(1,SURF).EQ.0.0D0) V3=ALENS(1,SURF)
                      IF(ALENS(1,SURF).NE.0.0D0) V3=1.0D0/ALENS(1,SURF)
                  END IF
                  IF(VTYPE.EQ.2.OR.VTYPE.EQ.135) THEN
                      V3=ALENS(1,SURF)
                  END IF
                  V4=V3-V2
                  IF(V3.EQ.0.0D0.AND.V2.NE.0.0D0.OR.V3.NE.0.0D0.AND.V2.EQ.0.0D0)
     1            THEN
                      IF(VTYPE.EQ.1.OR.VTYPE.EQ.134) V4=0.0D0
                  END IF
                  REST_KDP(7)=RESTINPT(7)
                  LNSTYP=3
                  CALL LNSEOS
C     FOCUS COMPENSATION GOES HERE
                  CALL FOCOMP
C     CALCULATE OPERANDS AND LOAD THEM
                  OPCALC_TYPE=2
                  CALL OPCALC
                  IF(F31.EQ.0) RETURN
                  CALL OPLOAD
                  IF(F31.EQ.0) RETURN
C     FOR EACH OPERAND, CALCULATE AND STORE THE CHANGE
                  DO J=MAXFOCRIT+1,MAXTOP+MAXFOCRIT
                      IF(ISTOP(J-MAXFOCRIT)) THEN
                          CHGVAL(J-MAXFOCRIT)=(OPERND(J,4)-OLDOP(J,4))
                          OPVVAL(J-MAXFOCRIT)=OPERND(J,4)
C     LOOP TO NEXT CHGVAL
                      END IF
                  END DO
                  IF(SENSERR) THEN
                      SENSERR=.FALSE.
                      WRITE(OUTLYNE,3009)
                      CALL SHOWIT(0)
                      WRITE(19,1309)
 3009                 FORMAT('WARNING: TOLERANCE DATA FOR NEXT VARIABLE IS IN ERROR')
 1309                 FORMAT(
     1                'WARNING: TOLERANCE DATA FOR NEXT VARIABLE IS IN ERROR')
                  END IF
                  IF(ITY.EQ.0) CALL SENSIOUT
                  IF(ITY.EQ.1) CALL INVSENSIOUT(I)
C     DONE FOR VARIABLE I PLUS
C     RESTORE THE LENS
                  F6=1
                  F1=0
                  F22=0
                  CALL ATOC(AM1,AM2,AM3,AM4,AM5,AM6,AM7,AM8,AM9,AM10
     1            ,SYSA,ALENA,SLVA,PIKA,FT01A,LIA,LICA,GLANMA,ALBL,LLTYPEA,INNIA
     2            ,MULTCLAPA,MULTCOBSA,AIPOLYX,AIPOLYY)
                  LNSTYP=3
                  CALL LNSEOS
C                       PLUS DELTA DONE
C     MINUS DELTA
C     NEW VALUE IS:
C     UPDATE THE LENS
                  F6=1
                  F1=0
                  F22=0
                  SURF=INT(VARABL(I,3))
                  SAVE_KDP(7)=SAVEINPT(7)
                  IF(VTYPE.EQ.2.OR.VTYPE.EQ.135) WC='CV'
                  IF(VTYPE.EQ.1.OR.VTYPE.EQ.134) WC='RD'
                  IF(VTYPE.EQ.1.OR.VTYPE.EQ.2) WQ='CENT'
                  IF(VTYPE.EQ.134.OR.VTYPE.EQ.135) WQ='DELTFR'
                  IF(VTYPE.EQ.2) PNAME='CV'
                  IF(VTYPE.EQ.1) PNAME='RD'
                  IF(VTYPE.EQ.135) PNAME='CV_FR'
                  IF(VTYPE.EQ.134) PNAME='RD_FR'
                  SQ=1
                  S1=1
                  S2=0
                  S3=0
                  S4=0
                  S5=0
                  DF1=0
                  DF2=1
                  DF3=1
                  DF4=1
                  DF5=1
                  SN=1
                  SST=0
                  W1=-DABS(VARABL(I,8))
                  V1=W1
                  IF(VTYPE.EQ.1.OR.VTYPE.EQ.134) THEN
                      IF(ALENS(1,SURF).EQ.0.0D0) V2=ALENS(1,SURF)
                      IF(ALENS(1,SURF).NE.0.0D0) V2=1.0D0/ALENS(1,SURF)
                  END IF
                  IF(VTYPE.EQ.2.OR.VTYPE.EQ.135) THEN
                      V2=ALENS(1,SURF)
                  END IF
                  CALL LENUP
                  IF(VTYPE.EQ.1.OR.VTYPE.EQ.134) THEN
                      IF(ALENS(1,SURF).EQ.0.0D0) V3=ALENS(1,SURF)
                      IF(ALENS(1,SURF).NE.0.0D0) V3=1.0D0/ALENS(1,SURF)
                  END IF
                  IF(VTYPE.EQ.2.OR.VTYPE.EQ.135) THEN
                      V3=ALENS(1,SURF)
                  END IF
                  V4=V3-V2
                  IF(V3.EQ.0.0D0.AND.V2.NE.0.0D0.OR.V3.NE.0.0D0.AND.V2.EQ.0.0D0)
     1            THEN
                      IF(VTYPE.EQ.1.OR.VTYPE.EQ.134) V4=0.0D0
                  END IF
                  REST_KDP(7)=RESTINPT(7)
                  LNSTYP=3
                  CALL LNSEOS
C     FOCUS COMPENSATION GOES HERE
                  CALL FOCOMP
C     CALCULATE OPERANDS AND LOAD THEM
                  OPCALC_TYPE=2
                  CALL OPCALC
                  IF(F31.EQ.0) RETURN
                  CALL OPLOAD
                  IF(F31.EQ.0) RETURN
C     FOR EACH OPERAND, CALCULATE AND STORE THE CHANGE
                  DO J=MAXFOCRIT+1,MAXTOP+MAXFOCRIT
                      IF(ISTOP(J-MAXFOCRIT)) THEN
                          CHGVAL(J-MAXFOCRIT)=(OPERND(J,4)-OLDOP(J,4))
                          OPVVAL(J-MAXFOCRIT)=OPERND(J,4)
C     LOOP TO NEXT CHGVAL
                      END IF
                  END DO
                  IF(ITY.EQ.0) CALL SENSIOUT
                  IF(ITY.EQ.1) CALL INVSENSIOUT(I)
C     DONE FOR VARIABLE I MINUS
C     RESTORE THE LENS
                  F6=1
                  F1=0
                  F22=0
                  CALL ATOC(AM1,AM2,AM3,AM4,AM5,AM6,AM7,AM8,AM9,AM10
     1            ,SYSA,ALENA,SLVA,PIKA,FT01A,LIA,LICA,GLANMA,ALBL,LLTYPEA,INNIA
     2            ,MULTCLAPA,MULTCOBSA,AIPOLYX,AIPOLYY)
                  LNSTYP=3
                  CALL LNSEOS
C                       MINUS DELTA DONE
              END IF

              IF(VTYPE.GE.3.AND.VTYPE.LE.8.OR.VTYPE.GE.11.AND.VTYPE.LE.
     1        25.OR.VTYPE.EQ.75.OR.VTYPE.GE.124.AND.VTYPE.LT.133.OR.VTYPE
     2        .EQ.138.OR.VTYPE.GE.141.0D0.AND.VTYPE.LE.153.0D0.OR.
     1        VTYPE.EQ.139.OR.VTYPE.EQ.140) THEN
C
C     PLUS DELTA
C     NEW VALUE IS:
C     UPDATE THE LENS
                  F6=1
                  F1=0
                  F22=0
                  SURF=INT(VARABL(I,3))
                  SAVE_KDP(7)=SAVEINPT(7)
                  WC='        '
                  IF(VTYPE.EQ.4) WC='CC'
                  IF(VTYPE.EQ.3) WC='TH'
                  IF(VTYPE.EQ.5) WC='AD'
                  IF(VTYPE.EQ.6) WC='AE'
                  IF(VTYPE.EQ.7) WC='AF'
                  IF(VTYPE.EQ.8) WC='AG'
                  IF(VTYPE.EQ.11) WC='CCTOR'
                  IF(VTYPE.EQ.12) WC='ADTOR'
                  IF(VTYPE.EQ.13) WC='AETOR'
                  IF(VTYPE.EQ.14) WC='AFTOR'
                  IF(VTYPE.EQ.15) WC='AGTOR'
                  IF(VTYPE.EQ.16) WC='ALPHA'
                  IF(VTYPE.EQ.17) WC='BETA'
                  IF(VTYPE.EQ.18) WC='GAMMA'
                  IF(VTYPE.EQ.19) WC='XD'
                  IF(VTYPE.EQ.20) WC='YD'
                  IF(VTYPE.EQ.21) WC='N1'
                  IF(VTYPE.EQ.22) WC='N2'
                  IF(VTYPE.EQ.23) WC='N3'
                  IF(VTYPE.EQ.24) WC='N4'
                  IF(VTYPE.EQ.25) WC='N5'
                  IF(VTYPE.EQ.75) WC='AC'
                  IF(VTYPE.EQ.124) WC='N6'
                  IF(VTYPE.EQ.125) WC='N7'
                  IF(VTYPE.EQ.126) WC='N8'
                  IF(VTYPE.EQ.127) WC='N9'
                  IF(VTYPE.EQ.128) WC='N10'
                  IF(VTYPE.EQ.129) WC='AH'
                  IF(VTYPE.EQ.130) WC='AI'
                  IF(VTYPE.EQ.131) WC='AJ'
                  IF(VTYPE.EQ.132) WC='AK'
                  IF(VTYPE.EQ.133) WC='AL'
                  IF(VTYPE.EQ.138) WC='ZD'
                  IF(VTYPE.EQ.139) WC='INDEX'
                  IF(VTYPE.EQ.140) WC='VNUM'
                  IF(VTYPE.EQ.141) WC='PIVX'
                  IF(VTYPE.EQ.142) WC='PIVY'
                  IF(VTYPE.EQ.143) WC='PIVZ'
                  IF(VTYPE.EQ.144) WC='DPART'
                  IF(VTYPE.EQ.145) WC='CLPX '
                  IF(VTYPE.EQ.146) WC='CLPY '
                  IF(VTYPE.EQ.147) WC='GDX  '
                  IF(VTYPE.EQ.148) WC='GDY  '
                  IF(VTYPE.EQ.149) WC='GDZ  '
                  IF(VTYPE.EQ.150) WC='GALPHA'
                  IF(VTYPE.EQ.151) WC='GBETA'
                  IF(VTYPE.EQ.152) WC='GGAMMA'
                  IF(VTYPE.EQ.153) WC='GRS'
                  PNAME=WC
                  WQ='DELT'
                  SQ=1
                  S1=1
                  S2=0
                  S3=0
                  S4=0
                  S5=0
                  DF1=0
                  DF2=1
                  DF3=1
                  DF4=1
                  DF5=1
                  SN=1
                  SST=0
                  W1=DABS(VARABL(I,8))
                  V1=W1
                  V4=V1
                  CALL LENUP
                  REST_KDP(7)=RESTINPT(7)
                  LNSTYP=3
                  CALL LNSEOS
C     FOCUS COMPENSATION GOES HERE
                  CALL FOCOMP
C     CALCULATE OPERANDS AND LOAD THEM
                  OPCALC_TYPE=2
                  CALL OPCALC
                  IF(F31.EQ.0) RETURN
                  CALL OPLOAD
                  IF(F31.EQ.0) RETURN
C     FOR EACH OPERAND, CALCULATE AND STORE THE CHANGE
                  DO J=MAXFOCRIT+1,MAXTOP+MAXFOCRIT
                      IF(ISTOP(J-MAXFOCRIT)) THEN
                          CHGVAL(J-MAXFOCRIT)=(OPERND(J,4)-OLDOP(J,4))
                          OPVVAL(J-MAXFOCRIT)=OPERND(J,4)
C     LOOP TO NEXT CHGVAL
                      END IF
                  END DO
                  IF(ITY.EQ.0) CALL SENSIOUT
                  IF(ITY.EQ.1) CALL INVSENSIOUT(I)
C     DONE FOR VARIABLE I PLUS
C     RESTORE THE LENS
                  F6=1
                  F1=0
                  F22=0
                  CALL ATOC(AM1,AM2,AM3,AM4,AM5,AM6,AM7,AM8,AM9,AM10
     1            ,SYSA,ALENA,SLVA,PIKA,FT01A,LIA,LICA,GLANMA,ALBL,LLTYPEA,INNIA
     2            ,MULTCLAPA,MULTCOBSA,AIPOLYX,AIPOLYY)
                  LNSTYP=3
                  CALL LNSEOS
C                       PLUS DELTA DONE
C     MINUS DELTA
C     NEW VALUE IS:
C     UPDATE THE LENS
                  F6=1
                  F1=0
                  F22=0
                  SURF=INT(VARABL(I,3))
                  SAVE_KDP(7)=SAVEINPT(7)
                  WC='        '
                  IF(VTYPE.EQ.4) WC='CC'
                  IF(VTYPE.EQ.3) WC='TH'
                  IF(VTYPE.EQ.5) WC='AD'
                  IF(VTYPE.EQ.6) WC='AE'
                  IF(VTYPE.EQ.7) WC='AF'
                  IF(VTYPE.EQ.8) WC='AG'
                  IF(VTYPE.EQ.11) WC='CCTOR'
                  IF(VTYPE.EQ.12) WC='ADTOR'
                  IF(VTYPE.EQ.13) WC='AETOR'
                  IF(VTYPE.EQ.14) WC='AFTOR'
                  IF(VTYPE.EQ.15) WC='AGTOR'
                  IF(VTYPE.EQ.16) WC='ALPHA'
                  IF(VTYPE.EQ.17) WC='BETA'
                  IF(VTYPE.EQ.18) WC='GAMMA'
                  IF(VTYPE.EQ.19) WC='XD'
                  IF(VTYPE.EQ.20) WC='YD'
                  IF(VTYPE.EQ.21) WC='N1'
                  IF(VTYPE.EQ.22) WC='N2'
                  IF(VTYPE.EQ.23) WC='N3'
                  IF(VTYPE.EQ.24) WC='N4'
                  IF(VTYPE.EQ.25) WC='N5'
                  IF(VTYPE.EQ.75) WC='AC'
                  IF(VTYPE.EQ.124) WC='N6'
                  IF(VTYPE.EQ.125) WC='N7'
                  IF(VTYPE.EQ.126) WC='N8'
                  IF(VTYPE.EQ.127) WC='N9'
                  IF(VTYPE.EQ.128) WC='N10'
                  IF(VTYPE.EQ.129) WC='AH'
                  IF(VTYPE.EQ.130) WC='AI'
                  IF(VTYPE.EQ.131) WC='AJ'
                  IF(VTYPE.EQ.132) WC='AK'
                  IF(VTYPE.EQ.133) WC='AL'
                  IF(VTYPE.EQ.138) WC='ZD'
                  IF(VTYPE.EQ.139) WC='INDEX'
                  IF(VTYPE.EQ.140) WC='VNUM'
                  IF(VTYPE.EQ.141) WC='PIVX'
                  IF(VTYPE.EQ.142) WC='PIVY'
                  IF(VTYPE.EQ.143) WC='PIVZ'
                  IF(VTYPE.EQ.144) WC='DPART'
                  IF(VTYPE.EQ.145) WC='CLPX '
                  IF(VTYPE.EQ.146) WC='CLPY '
                  IF(VTYPE.EQ.147) WC='GDX  '
                  IF(VTYPE.EQ.148) WC='GDY  '
                  IF(VTYPE.EQ.149) WC='GDZ  '
                  IF(VTYPE.EQ.150) WC='GALPHA'
                  IF(VTYPE.EQ.151) WC='GBETA'
                  IF(VTYPE.EQ.152) WC='GGAMMA'
                  IF(VTYPE.EQ.153) WC='GRS'
                  WQ='DELT'
                  PNAME=WC
                  SQ=1
                  S1=1
                  S2=0
                  S3=0
                  S4=0
                  S5=0
                  DF1=0
                  DF2=1
                  DF3=1
                  DF4=1
                  DF5=1
                  SN=1
                  SST=0
                  W1=-DABS(VARABL(I,8))
                  V1=W1
                  V4=V1
                  CALL LENUP
                  REST_KDP(7)=RESTINPT(7)
                  LNSTYP=3
                  CALL LNSEOS
C     FOCUS COMPENSATION GOES HERE
                  CALL FOCOMP
C     CALCULATE OPERANDS AND LOAD THEM
                  OPCALC_TYPE=2
                  CALL OPCALC
                  IF(F31.EQ.0) RETURN
                  CALL OPLOAD
                  IF(F31.EQ.0) RETURN
C     FOR EACH OPERAND, CALCULATE AND STORE THE CHANGE
                  DO J=MAXFOCRIT+1,MAXTOP+MAXFOCRIT
                      IF(ISTOP(J-MAXFOCRIT)) THEN
                          CHGVAL(J-MAXFOCRIT)=(OPERND(J,4)-OLDOP(J,4))
                          OPVVAL(J-MAXFOCRIT)=OPERND(J,4)
C     LOOP TO NEXT CHGVAL
                      END IF
                  END DO
                  IF(ITY.EQ.0) CALL SENSIOUT
                  IF(ITY.EQ.1) CALL INVSENSIOUT(I)
C     DONE FOR VARIABLE I MINUS
C     RESTORE THE LENS
                  F6=1
                  F1=0
                  F22=0
                  CALL ATOC(AM1,AM2,AM3,AM4,AM5,AM6,AM7,AM8,AM9,AM10
     1            ,SYSA,ALENA,SLVA,PIKA,FT01A,LIA,LICA,GLANMA,ALBL,LLTYPEA,INNIA
     2            ,MULTCLAPA,MULTCOBSA,AIPOLYX,AIPOLYY)
                  LNSTYP=3
                  CALL LNSEOS
C                       MINUS DELTA DONE
              END IF
C
              IF(VTYPE.EQ.9.OR.VTYPE.EQ.10) THEN
C     TORIC RADIUS OR CURVATUE
C     PLUS DELTA
C     NEW VALUE IS:
C     UPDATE THE LENS
                  F6=1
                  F1=0
                  F22=0
                  SURF=INT(VARABL(I,3))
                  IF(ALENS(23,SURF).EQ.0.0D0) THEN
C     NOT TORIC SET AS SO
                      TORRY=.FALSE.
                      SAVE_KDP(7)=SAVEINPT(7)
                      WC='YTORIC'
                      SQ=0
                      S1=0
                      S2=0
                      S3=0
                      S4=0
                      S5=0
                      DF1=1
                      DF2=1
                      DF3=1
                      DF4=1
                      DF5=1
                      SN=0
                      SST=0
                      CALL LENUP
                      REST_KDP(7)=RESTINPT(7)

                  ELSE
                      TORRY=.TRUE.
                  END IF
                  SAVE_KDP(7)=SAVEINPT(7)
                  IF(VTYPE.EQ.9.OR.VTYPE.EQ.136) WC='RDTOR'
                  IF(VTYPE.EQ.10.OR.VTYPE.EQ.137) WC='CVTOR'
                  IF(VTYPE.EQ.9.OR.VTYPE.EQ.10) WQ='CENT'
                  IF(VTYPE.EQ.136.OR.VTYPE.EQ.137) WQ='DELTFR'
                  IF(VTYPE.EQ.10) PNAME='CVTOR'
                  IF(VTYPE.EQ.9) PNAME='RDTOR'
                  IF(VTYPE.EQ.136) PNAME='RDTOR_FR'
                  IF(VTYPE.EQ.137) PNAME='CVTOR_FR'
                  SQ=1
                  S1=1
                  S2=0
                  S3=0
                  S4=0
                  S5=0
                  DF1=0
                  DF2=1
                  DF3=1
                  DF4=1
                  DF5=1
                  SN=1
                  SST=0
                  W1=DABS(VARABL(I,8))
                  V1=W1
                  IF(VTYPE.EQ.9.OR.VTYPE.EQ.136) THEN
                      IF(ALENS(24,SURF).EQ.0.0D0) V2=ALENS(24,SURF)
                      IF(ALENS(24,SURF).NE.0.0D0) V2=1.0D0/ALENS(24,SURF)
                  END IF
                  IF(VTYPE.EQ.10.OR.VTYPE.EQ.137) THEN
                      V2=ALENS(24,SURF)
                  END IF
                  CALL LENUP
                  IF(VTYPE.EQ.9.OR.VTYPE.EQ.136) THEN
                      IF(ALENS(24,SURF).EQ.0.0D0) V3=ALENS(24,SURF)
                      IF(ALENS(24,SURF).NE.0.0D0) V3=1.0D0/ALENS(24,SURF)
                  END IF
                  IF(VTYPE.EQ.10.OR.VTYPE.EQ.137) THEN
                      V3=ALENS(24,SURF)
                  END IF
                  V4=V3-V2
                  IF(V3.EQ.0.0D0.AND.V2.NE.0.0D0.OR.V3.NE.0.0D0.AND.V2.EQ.0.0D0)
     1            THEN
                      IF(VTYPE.EQ.1.OR.VTYPE.EQ.134) V4=0.0D0
                  END IF
                  REST_KDP(7)=RESTINPT(7)
                  LNSTYP=3
                  CALL LNSEOS
C     FOCUS COMPENSATION GOES HERE
                  CALL FOCOMP
C     CALCULATE OPERANDS AND LOAD THEM
                  OPCALC_TYPE=2
                  CALL OPCALC
                  IF(F31.EQ.0) RETURN
                  CALL OPLOAD
                  IF(F31.EQ.0) RETURN
C     FOR EACH OPERAND, CALCULATE AND STORE THE CHANGE
                  DO J=MAXFOCRIT+1,MAXTOP+MAXFOCRIT
                      IF(ISTOP(J-MAXFOCRIT)) THEN
                          CHGVAL(J-MAXFOCRIT)=(OPERND(J,4)-OLDOP(J,4))
                          OPVVAL(J-MAXFOCRIT)=OPERND(J,4)
C     LOOP TO NEXT CHGVAL
                      END IF
                  END DO
                  IF(ITY.EQ.0) CALL SENSIOUT
                  IF(ITY.EQ.1) CALL INVSENSIOUT(I)
C     DONE FOR VARIABLE I PLUS
C     RESTORE THE LENS
                  F6=1
                  F1=0
                  F22=0
                  CALL ATOC(AM1,AM2,AM3,AM4,AM5,AM6,AM7,AM8,AM9,AM10
     1            ,SYSA,ALENA,SLVA,PIKA,FT01A,LIA,LICA,GLANMA,ALBL,LLTYPEA,INNIA,
     2            MULTCLAPA,MULTCOBSA,AIPOLYX,AIPOLYY)
                  LNSTYP=3
                  CALL LNSEOS
C                       PLUS DELTA DONE
C     MINUS DELTA
C     NEW VALUE IS:
C     UPDATE THE LENS
                  F6=1
                  F1=0
                  F22=0
                  SURF=INT(VARABL(I,3))
                  SAVE_KDP(7)=SAVEINPT(7)
                  IF(VTYPE.EQ.9.OR.VTYPE.EQ.136) WC='RDTOR'
                  IF(VTYPE.EQ.10.OR.VTYPE.EQ.137) WC='CVTOR'
                  IF(VTYPE.EQ.9.OR.VTYPE.EQ.10) WQ='CENT'
                  IF(VTYPE.EQ.136.OR.VTYPE.EQ.137) WQ='DELTFR'
                  IF(VTYPE.EQ.10) PNAME='CVTOR'
                  IF(VTYPE.EQ.9) PNAME='RDTOR'
                  IF(VTYPE.EQ.136) PNAME='RDTOR_FR'
                  IF(VTYPE.EQ.137) PNAME='CVTOR_FR'
                  SQ=1
                  S1=1
                  S2=0
                  S3=0
                  S4=0
                  S5=0
                  DF1=0
                  DF2=1
                  DF3=1
                  DF4=1
                  DF5=1
                  SN=1
                  SST=0
                  W1=-DABS(VARABL(I,8))
                  V1=W1
                  IF(VTYPE.EQ.9.OR.VTYPE.EQ.136) THEN
                      IF(ALENS(24,SURF).EQ.0.0D0) V2=ALENS(24,SURF)
                      IF(ALENS(24,SURF).NE.0.0D0) V2=1.0D0/ALENS(24,SURF)
                  END IF
                  IF(VTYPE.EQ.10.OR.VTYPE.EQ.137) THEN
                      V2=ALENS(24,SURF)
                  END IF
                  CALL LENUP
                  IF(VTYPE.EQ.9.OR.VTYPE.EQ.136) THEN
                      IF(ALENS(24,SURF).EQ.0.0D0) V3=ALENS(24,SURF)
                      IF(ALENS(24,SURF).NE.0.0D0) V3=1.0D0/ALENS(24,SURF)
                  END IF
                  IF(VTYPE.EQ.10.OR.VTYPE.EQ.137) THEN
                      V3=ALENS(24,SURF)
                  END IF
                  V4=V3-V2
                  IF(V3.EQ.0.0D0.AND.V2.NE.0.0D0.OR.V3.NE.0.0D0.AND.V2.EQ.0.0D0)
     1            THEN
                      IF(VTYPE.EQ.1.OR.VTYPE.EQ.134) V4=0.0D0
                  END IF
                  REST_KDP(7)=RESTINPT(7)
                  LNSTYP=3
                  CALL LNSEOS
C     FOCUS COMPENSATION GOES HERE
                  CALL FOCOMP
C     CALCULATE OPERANDS AND LOAD THEM
                  OPCALC_TYPE=2
                  CALL OPCALC
                  IF(F31.EQ.0) RETURN
                  CALL OPLOAD
                  IF(F31.EQ.0) RETURN
C     FOR EACH OPERAND, CALCULATE AND STORE THE CHANGE
                  DO J=MAXFOCRIT+1,MAXTOP+MAXFOCRIT
                      IF(ISTOP(J-MAXFOCRIT)) THEN
                          CHGVAL(J-MAXFOCRIT)=(OPERND(J,4)-OLDOP(J,4))
                          OPVVAL(J-MAXFOCRIT)=OPERND(J,4)
C     LOOP TO NEXT CHGVAL
                      END IF
                  END DO
                  IF(ITY.EQ.0) CALL SENSIOUT
                  IF(ITY.EQ.1) CALL INVSENSIOUT(I)
C     DONE FOR VARIABLE I MINUS
C     RESTORE THE LENS
                  F6=1
                  F1=0
                  F22=0
                  CALL ATOC(AM1,AM2,AM3,AM4,AM5,AM6,AM7,AM8,AM9,AM10
     1            ,SYSA,ALENA,SLVA,PIKA,FT01A,LIA,LICA,GLANMA,ALBL,LLTYPEA,INNIA,
     2            MULTCLAPA,MULTCOBSA,AIPOLYX,AIPOLYY)
                  LNSTYP=3
                  CALL LNSEOS
              END IF
C                       MINUS DELTA DONE

              IF(VTYPE.GE.27.AND.VTYPE.LE.74) THEN
C     SPECIAL SURFACE COEFFICIENTS
C     PLUS DELTA VALUE
                  IF(VTYPE.EQ.27) PNAME='C1'
                  IF(VTYPE.EQ.28) PNAME='C2'
                  IF(VTYPE.EQ.29) PNAME='C3'
                  IF(VTYPE.EQ.30) PNAME='C4'
                  IF(VTYPE.EQ.31) PNAME='C5'
                  IF(VTYPE.EQ.32) PNAME='C6'
                  IF(VTYPE.EQ.33) PNAME='C7'
                  IF(VTYPE.EQ.34) PNAME='C8'
                  IF(VTYPE.EQ.35) PNAME='C9'
                  IF(VTYPE.EQ.36) PNAME='C10'
                  IF(VTYPE.EQ.37) PNAME='C11'
                  IF(VTYPE.EQ.38) PNAME='C12'
                  IF(VTYPE.EQ.39) PNAME='C13'
                  IF(VTYPE.EQ.40) PNAME='C14'
                  IF(VTYPE.EQ.41) PNAME='C15'
                  IF(VTYPE.EQ.42) PNAME='C16'
                  IF(VTYPE.EQ.43) PNAME='C17'
                  IF(VTYPE.EQ.44) PNAME='C18'
                  IF(VTYPE.EQ.45) PNAME='C19'
                  IF(VTYPE.EQ.46) PNAME='C20'
                  IF(VTYPE.EQ.47) PNAME='C21'
                  IF(VTYPE.EQ.48) PNAME='C22'
                  IF(VTYPE.EQ.49) PNAME='C23'
                  IF(VTYPE.EQ.50) PNAME='C24'
                  IF(VTYPE.EQ.51) PNAME='C25'
                  IF(VTYPE.EQ.52) PNAME='C26'
                  IF(VTYPE.EQ.53) PNAME='C27'
                  IF(VTYPE.EQ.54) PNAME='C28'
                  IF(VTYPE.EQ.55) PNAME='C29'
                  IF(VTYPE.EQ.56) PNAME='C30'
                  IF(VTYPE.EQ.57) PNAME='C31'
                  IF(VTYPE.EQ.58) PNAME='C32'
                  IF(VTYPE.EQ.59) PNAME='C33'
                  IF(VTYPE.EQ.60) PNAME='C34'
                  IF(VTYPE.EQ.61) PNAME='C35'
                  IF(VTYPE.EQ.62) PNAME='C36'
                  IF(VTYPE.EQ.63) PNAME='C37'
                  IF(VTYPE.EQ.64) PNAME='C38'
                  IF(VTYPE.EQ.65) PNAME='C39'
                  IF(VTYPE.EQ.66) PNAME='C40'
                  IF(VTYPE.EQ.67) PNAME='C41'
                  IF(VTYPE.EQ.68) PNAME='C42'
                  IF(VTYPE.EQ.69) PNAME='C43'
                  IF(VTYPE.EQ.70) PNAME='C44'
                  IF(VTYPE.EQ.71) PNAME='C45'
                  IF(VTYPE.EQ.72) PNAME='C46'
                  IF(VTYPE.EQ.73) PNAME='C47'
                  IF(VTYPE.EQ.74) PNAME='C48'
C     NEW VALUE IS:
                  V1=DABS(VARABL(I,8))
                  V4=V1
                  FTFL01((VTYPE-26),INT(VARABL(I,3)))=V1+
     1            FTFL01((VTYPE-26),INT(VARABL(I,3)))
C     UPDATE THE LENS
                  F6=1
                  F1=0
                  F22=0
                  LNSTYP=3
                  CALL LNSEOS
C     FOCUS COMPENSATION GOES HERE
                  CALL FOCOMP
C     CALCULATE OPERANDS AND LOAD THEM
                  OPCALC_TYPE=2
                  CALL OPCALC
                  IF(F31.EQ.0) RETURN
                  CALL OPLOAD
                  IF(F31.EQ.0) RETURN
                  DO J=MAXFOCRIT+1,MAXTOP+MAXFOCRIT
                      IF(ISTOP(J-MAXFOCRIT)) THEN
                          CHGVAL(J-MAXFOCRIT)=(OPERND(J,4)-OLDOP(J,4))
                          OPVVAL(J-MAXFOCRIT)=OPERND(J,4)
                      END IF
                  END DO
                  IF(ITY.EQ.0) CALL SENSIOUT
                  IF(ITY.EQ.1) CALL INVSENSIOUT(I)
C     RESTORE THE LENS
                  CALL ATOC(AM1,AM2,AM3,AM4,AM5,AM6,AM7,AM8,AM9,AM10
     1            ,SYSA,ALENA,SLVA,PIKA,FT01A,LIA,LICA,GLANMA,ALBL,LLTYPEA,INNIA
     2            ,MULTCLAPA,MULTCOBSA,AIPOLYX,AIPOLYY)
                  F1=0
                  F6=1
                  F22=0
                  LNSTYP=3
                  CALL LNSEOS
                  OPCALC_TYPE=2
                  CALL OPCALC
                  IF(F31.EQ.0) RETURN
                  CALL OPLOAD
                  IF(F31.EQ.0) RETURN
C     THE TOLOP NOMINALS HAVE BEEN SET
C
C     SET FLAG THAT TRACKS IF COMPENSATION MUST BE DONE
                  COMPYES=.FALSE.
                  DO IJ=1,MAXFOCRIT
                      IF(ISCOMP(IJ).AND.ISCRIT(IJ)) THEN
C     COMPYES TRUE MEANS WE CALCULATE FOCRITS IN OPCALC
                          COMPYES=.TRUE.
                      END IF
                  END DO
                  IF(COMPYES) THEN
C     SET MAX COUNTERS FOR OPTIMIZATION
C                  NO DERIVATIVES EXIST
C     THERE ARE COMPENSATOR OPERANDS, EVALUATE THEM AND SET
C     THE TARGETS TO BE THE CURRENT VALUES. THIS IS DONE JUST ONCE.
                      DO II=1,MAXCMP
                          OPERND(II,2)=OCRIT(II)
                      END DO
                  END IF
C     THE COMPENSATOR TARGETS HAVE BEEN SET
C     NOW MINUS DELTA
C     NEW VALUE IS:
                  IF(VTYPE.EQ.27) PNAME='C1'
                  IF(VTYPE.EQ.28) PNAME='C2'
                  IF(VTYPE.EQ.29) PNAME='C3'
                  IF(VTYPE.EQ.30) PNAME='C4'
                  IF(VTYPE.EQ.31) PNAME='C5'
                  IF(VTYPE.EQ.32) PNAME='C6'
                  IF(VTYPE.EQ.33) PNAME='C7'
                  IF(VTYPE.EQ.34) PNAME='C8'
                  IF(VTYPE.EQ.35) PNAME='C9'
                  IF(VTYPE.EQ.36) PNAME='C10'
                  IF(VTYPE.EQ.37) PNAME='C11'
                  IF(VTYPE.EQ.38) PNAME='C12'
                  IF(VTYPE.EQ.39) PNAME='C13'
                  IF(VTYPE.EQ.40) PNAME='C14'
                  IF(VTYPE.EQ.41) PNAME='C15'
                  IF(VTYPE.EQ.42) PNAME='C16'
                  IF(VTYPE.EQ.43) PNAME='C17'
                  IF(VTYPE.EQ.44) PNAME='C18'
                  IF(VTYPE.EQ.45) PNAME='C19'
                  IF(VTYPE.EQ.46) PNAME='C20'
                  IF(VTYPE.EQ.47) PNAME='C21'
                  IF(VTYPE.EQ.48) PNAME='C22'
                  IF(VTYPE.EQ.49) PNAME='C23'
                  IF(VTYPE.EQ.50) PNAME='C24'
                  IF(VTYPE.EQ.51) PNAME='C25'
                  IF(VTYPE.EQ.52) PNAME='C26'
                  IF(VTYPE.EQ.53) PNAME='C27'
                  IF(VTYPE.EQ.54) PNAME='C28'
                  IF(VTYPE.EQ.55) PNAME='C29'
                  IF(VTYPE.EQ.56) PNAME='C30'
                  IF(VTYPE.EQ.57) PNAME='C31'
                  IF(VTYPE.EQ.58) PNAME='C32'
                  IF(VTYPE.EQ.59) PNAME='C33'
                  IF(VTYPE.EQ.60) PNAME='C34'
                  IF(VTYPE.EQ.61) PNAME='C35'
                  IF(VTYPE.EQ.62) PNAME='C36'
                  IF(VTYPE.EQ.63) PNAME='C37'
                  IF(VTYPE.EQ.64) PNAME='C38'
                  IF(VTYPE.EQ.65) PNAME='C39'
                  IF(VTYPE.EQ.66) PNAME='C40'
                  IF(VTYPE.EQ.67) PNAME='C41'
                  IF(VTYPE.EQ.68) PNAME='C42'
                  IF(VTYPE.EQ.69) PNAME='C43'
                  IF(VTYPE.EQ.70) PNAME='C44'
                  IF(VTYPE.EQ.71) PNAME='C45'
                  IF(VTYPE.EQ.72) PNAME='C46'
                  IF(VTYPE.EQ.73) PNAME='C47'
                  IF(VTYPE.EQ.74) PNAME='C48'
                  V1=-DABS(VARABL(I,8))
                  V4=V1
                  FTFL01((VTYPE-26),INT(VARABL(I,3)))=V1+
     1            FTFL01((VTYPE-26),INT(VARABL(I,3)))
C     UPDATE THE LENS
                  F6=1
                  F1=0
                  F22=0
                  LNSTYP=3
                  CALL LNSEOS
C     FOCUS COMPENSATION GOES HERE
C     CALCULATE OPERANDS AND LOAD THEM
                  OPCALC_TYPE=2
                  CALL OPCALC
                  IF(F31.EQ.0) RETURN
                  CALL OPLOAD
                  IF(F31.EQ.0) RETURN
                  DO J=MAXFOCRIT+1,MAXTOP+MAXFOCRIT
                      IF(ISTOP(J-MAXFOCRIT)) THEN
                          CHGVAL(J-MAXFOCRIT)=(OPERND(J,4)-OLDOP(J,4))
                          OPVVAL(J-MAXFOCRIT)=OPERND(J,4)
                      END IF
                  END DO
                  IF(ITY.EQ.0) CALL SENSIOUT
                  IF(ITY.EQ.1) CALL INVSENSIOUT(I)
C     RESTORE THE LENS
                  CALL ATOC(AM1,AM2,AM3,AM4,AM5,AM6,AM7,AM8,AM9,AM10
     1            ,SYSA,ALENA,SLVA,PIKA,FT01A,LIA,LICA,GLANMA,ALBL,LLTYPEA,INNIA
     2            ,MULTCLAPA,MULTCOBSA,AIPOLYX,AIPOLYY)
                  F1=0
                  F6=1
                  F22=0
                  LNSTYP=3
                  CALL LNSEOS
                  OPCALC_TYPE=2
                  CALL OPCALC
                  IF(F31.EQ.0) RETURN
                  CALL OPLOAD
                  IF(F31.EQ.0) RETURN
C     THE TOLOP NOMINALS HAVE BEEN SET
C
C     SET FLAG THAT TRACKS IF COMPENSATION MUST BE DONE
                  COMPYES=.FALSE.
                  DO IJ=1,MAXFOCRIT
                      IF(ISCOMP(IJ).AND.ISCRIT(IJ)) THEN
C     COMPYES TRUE MEANS WE CALCULATE FOCRITS IN OPCALC
                          COMPYES=.TRUE.
                      END IF
                  END DO
                  IF(COMPYES) THEN
C     SET MAX COUNTERS FOR OPTIMIZATION
C                  NO DERIVATIVES EXIST
C     THERE ARE COMPENSATOR OPERANDS, EVALUATE THEM AND SET
C     THE TARGETS TO BE THE CURRENT VALUES. THIS IS DONE JUST ONCE.
                      DO II=1,MAXCMP
                          OPERND(II,2)=OCRIT(II)
                      END DO
                  END IF
C     THE COMPENSATOR TARGETS HAVE BEEN SET
              END IF
              IF(VTYPE.GE.76.AND.VTYPE.LE.123) THEN
C     SPECIAL SURFACE COEFFICIENTS
                  IF(VTYPE.EQ.76) PNAME='C49'
                  IF(VTYPE.EQ.77) PNAME='C50'
                  IF(VTYPE.EQ.78) PNAME='C51'
                  IF(VTYPE.EQ.79) PNAME='C52'
                  IF(VTYPE.EQ.80) PNAME='C53'
                  IF(VTYPE.EQ.81) PNAME='C54'
                  IF(VTYPE.EQ.82) PNAME='C55'
                  IF(VTYPE.EQ.83) PNAME='C56'
                  IF(VTYPE.EQ.84) PNAME='C57'
                  IF(VTYPE.EQ.85) PNAME='C58'
                  IF(VTYPE.EQ.86) PNAME='C69'
                  IF(VTYPE.EQ.87) PNAME='C60'
                  IF(VTYPE.EQ.88) PNAME='C61'
                  IF(VTYPE.EQ.89) PNAME='C62'
                  IF(VTYPE.EQ.90) PNAME='C63'
                  IF(VTYPE.EQ.91) PNAME='C64'
                  IF(VTYPE.EQ.92) PNAME='C65'
                  IF(VTYPE.EQ.93) PNAME='C66'
                  IF(VTYPE.EQ.94) PNAME='C67'
                  IF(VTYPE.EQ.95) PNAME='C68'
                  IF(VTYPE.EQ.96) PNAME='C69'
                  IF(VTYPE.EQ.97) PNAME='C70'
                  IF(VTYPE.EQ.98) PNAME='C71'
                  IF(VTYPE.EQ.99) PNAME='C72'
                  IF(VTYPE.EQ.100) PNAME='C73'
                  IF(VTYPE.EQ.101) PNAME='C74'
                  IF(VTYPE.EQ.102) PNAME='C75'
                  IF(VTYPE.EQ.103) PNAME='C76'
                  IF(VTYPE.EQ.104) PNAME='C77'
                  IF(VTYPE.EQ.105) PNAME='C78'
                  IF(VTYPE.EQ.106) PNAME='C79'
                  IF(VTYPE.EQ.107) PNAME='C80'
                  IF(VTYPE.EQ.108) PNAME='C81'
                  IF(VTYPE.EQ.109) PNAME='C82'
                  IF(VTYPE.EQ.110) PNAME='C83'
                  IF(VTYPE.EQ.111) PNAME='C84'
                  IF(VTYPE.EQ.112) PNAME='C85'
                  IF(VTYPE.EQ.113) PNAME='C86'
                  IF(VTYPE.EQ.114) PNAME='C87'
                  IF(VTYPE.EQ.115) PNAME='C88'
                  IF(VTYPE.EQ.116) PNAME='C89'
                  IF(VTYPE.EQ.117) PNAME='C90'
                  IF(VTYPE.EQ.118) PNAME='C91'
                  IF(VTYPE.EQ.119) PNAME='C92'
                  IF(VTYPE.EQ.120) PNAME='C93'
                  IF(VTYPE.EQ.121) PNAME='C94'
                  IF(VTYPE.EQ.122) PNAME='C95'
                  IF(VTYPE.EQ.123) PNAME='C96'
C     NEW VALUE IS:
C     PLUS DELTA
                  V1=DABS(VARABL(I,8))
                  V4=V1
                  FTFL01((VTYPE-27),INT(VARABL(I,3)))=V1+
     1            FTFL01((VTYPE-27),INT(VARABL(I,3)))
C     UPDATE THE LENS
                  F6=1
                  F1=0
                  F22=0
                  LNSTYP=3
                  CALL LNSEOS
C     FOCUS COMPENSATION GOES HERE
                  CALL FOCOMP
C     CALCULATE OPERANDS AND LOAD THEM
                  OPCALC_TYPE=2
                  CALL OPCALC
                  IF(F31.EQ.0) RETURN
                  CALL OPLOAD
                  IF(F31.EQ.0) RETURN
                  DO J=MAXFOCRIT+1,MAXTOP+MAXFOCRIT
                      IF(ISTOP(J-MAXFOCRIT)) THEN
                          CHGVAL(J-MAXFOCRIT)=(OPERND(J,4)-OLDOP(J,4))
                          OPVVAL(J-MAXFOCRIT)=OPERND(J,4)
                      END IF
                  END DO
                  IF(ITY.EQ.0) CALL SENSIOUT
                  IF(ITY.EQ.1) CALL INVSENSIOUT(I)
C     DONE FOR VARIABLE I
C     RESTORE THE LENS
                  CALL ATOC(AM1,AM2,AM3,AM4,AM5,AM6,AM7,AM8,AM9,AM10
     1            ,SYSA,ALENA,SLVA,PIKA,FT01A,LIA,LICA,GLANMA,ALBL,LLTYPEA,INNIA
     2            ,MULTCLAPA,MULTCOBSA,AIPOLYX,AIPOLYY)
                  F1=0
                  F6=1
                  F22=0
                  LNSTYP=3
                  CALL LNSEOS
                  OPCALC_TYPE=2
                  CALL OPCALC
                  IF(F31.EQ.0) RETURN
                  CALL OPLOAD
                  IF(F31.EQ.0) RETURN
C     THE TOLOP NOMINALS HAVE BEEN SET
C
C     SET FLAG THAT TRACKS IF COMPENSATION MUST BE DONE
                  COMPYES=.FALSE.
                  DO IJ=1,MAXFOCRIT
                      IF(ISCOMP(IJ).AND.ISCRIT(IJ)) THEN
C     COMPYES TRUE MEANS WE CALCULATE FOCRITS IN OPCALC
                          COMPYES=.TRUE.
                      END IF
                  END DO
                  IF(COMPYES) THEN
C     SET MAX COUNTERS FOR OPTIMIZATION
C                  NO DERIVATIVES EXIST
C     THERE ARE COMPENSATOR OPERANDS, EVALUATE THEM AND SET
C     THE TARGETS TO BE THE CURRENT VALUES. THIS IS DONE JUST ONCE.
                      DO II=1,MAXCMP
                          OPERND(II,2)=OCRIT(II)
                      END DO
                  END IF
C     THE COMPENSATOR TARGETS HAVE BEEN SET
C     MINUS DELTA
C     NEW VALUE IS:
                  IF(VTYPE.EQ.76) PNAME='C49'
                  IF(VTYPE.EQ.77) PNAME='C50'
                  IF(VTYPE.EQ.78) PNAME='C51'
                  IF(VTYPE.EQ.79) PNAME='C52'
                  IF(VTYPE.EQ.80) PNAME='C53'
                  IF(VTYPE.EQ.81) PNAME='C54'
                  IF(VTYPE.EQ.82) PNAME='C55'
                  IF(VTYPE.EQ.83) PNAME='C56'
                  IF(VTYPE.EQ.84) PNAME='C57'
                  IF(VTYPE.EQ.85) PNAME='C58'
                  IF(VTYPE.EQ.86) PNAME='C69'
                  IF(VTYPE.EQ.87) PNAME='C60'
                  IF(VTYPE.EQ.88) PNAME='C61'
                  IF(VTYPE.EQ.89) PNAME='C62'
                  IF(VTYPE.EQ.90) PNAME='C63'
                  IF(VTYPE.EQ.91) PNAME='C64'
                  IF(VTYPE.EQ.92) PNAME='C65'
                  IF(VTYPE.EQ.93) PNAME='C66'
                  IF(VTYPE.EQ.94) PNAME='C67'
                  IF(VTYPE.EQ.95) PNAME='C68'
                  IF(VTYPE.EQ.96) PNAME='C69'
                  IF(VTYPE.EQ.97) PNAME='C70'
                  IF(VTYPE.EQ.98) PNAME='C71'
                  IF(VTYPE.EQ.99) PNAME='C72'
                  IF(VTYPE.EQ.100) PNAME='C73'
                  IF(VTYPE.EQ.101) PNAME='C74'
                  IF(VTYPE.EQ.102) PNAME='C75'
                  IF(VTYPE.EQ.103) PNAME='C76'
                  IF(VTYPE.EQ.104) PNAME='C77'
                  IF(VTYPE.EQ.105) PNAME='C78'
                  IF(VTYPE.EQ.106) PNAME='C79'
                  IF(VTYPE.EQ.107) PNAME='C80'
                  IF(VTYPE.EQ.108) PNAME='C81'
                  IF(VTYPE.EQ.109) PNAME='C82'
                  IF(VTYPE.EQ.110) PNAME='C83'
                  IF(VTYPE.EQ.111) PNAME='C84'
                  IF(VTYPE.EQ.112) PNAME='C85'
                  IF(VTYPE.EQ.113) PNAME='C86'
                  IF(VTYPE.EQ.114) PNAME='C87'
                  IF(VTYPE.EQ.115) PNAME='C88'
                  IF(VTYPE.EQ.116) PNAME='C89'
                  IF(VTYPE.EQ.117) PNAME='C90'
                  IF(VTYPE.EQ.118) PNAME='C91'
                  IF(VTYPE.EQ.119) PNAME='C92'
                  IF(VTYPE.EQ.120) PNAME='C93'
                  IF(VTYPE.EQ.121) PNAME='C94'
                  IF(VTYPE.EQ.122) PNAME='C95'
                  IF(VTYPE.EQ.123) PNAME='C96'
                  V1=-DABS(VARABL(I,8))
                  V4=V1
                  FTFL01((VTYPE-27),INT(VARABL(I,3)))=V1+
     1            FTFL01((VTYPE-27),INT(VARABL(I,3)))
C     UPDATE THE LENS
                  F6=1
                  F1=0
                  F22=0
                  LNSTYP=3
                  CALL LNSEOS
C     FOCUS COMPENSATION GOES HERE
                  CALL FOCOMP
C     CALCULATE OPERANDS AND LOAD THEM
                  OPCALC_TYPE=2
                  CALL OPCALC
                  IF(F31.EQ.0) RETURN
                  CALL OPLOAD
                  IF(F31.EQ.0) RETURN
                  DO J=MAXFOCRIT+1,MAXTOP+MAXFOCRIT
                      IF(ISTOP(J-MAXFOCRIT)) THEN
                          CHGVAL(J-MAXFOCRIT)=(OPERND(J,4)-OLDOP(J,4))
                          OPVVAL(J-MAXFOCRIT)=OPERND(J,4)
                      END IF
                  END DO
                  IF(ITY.EQ.0) CALL SENSIOUT
                  IF(ITY.EQ.1) CALL INVSENSIOUT(I)
C     DONE FOR VARIABLE I
C     RESTORE THE LENS
                  CALL ATOC(AM1,AM2,AM3,AM4,AM5,AM6,AM7,AM8,AM9,AM10
     1            ,SYSA,ALENA,SLVA,PIKA,FT01A,LIA,LICA,GLANMA,ALBL,LLTYPEA,INNIA
     2            ,MULTCLAPA,MULTCOBSA,AIPOLYX,AIPOLYY)
                  F1=0
                  F6=1
                  F22=0
                  LNSTYP=3
                  CALL LNSEOS
                  OPCALC_TYPE=2
                  CALL OPCALC
                  IF(F31.EQ.0) RETURN
                  CALL OPLOAD
                  IF(F31.EQ.0) RETURN
C     THE TOLOP NOMINALS HAVE BEEN SET
C
C     SET FLAG THAT TRACKS IF COMPENSATION MUST BE DONE
                  COMPYES=.FALSE.
                  DO IJ=1,MAXFOCRIT
                      IF(ISCOMP(IJ).AND.ISCRIT(IJ)) THEN
C     COMPYES TRUE MEANS WE CALCULATE FOCRITS IN OPCALC
                          COMPYES=.TRUE.
                      END IF
                  END DO
                  IF(COMPYES) THEN
C     SET MAX COUNTERS FOR OPTIMIZATION
C                  NO DERIVATIVES EXIST
C     THERE ARE COMPENSATOR OPERANDS, EVALUATE THEM AND SET
C     THE TARGETS TO BE THE CURRENT VALUES. THIS IS DONE JUST ONCE.
                      DO II=1,MAXCMP
                          OPERND(II,2)=OCRIT(II)
                      END DO
                  END IF
C     THE COMPENSATOR TARGETS HAVE BEEN SET
C                       SPECIAL SURFACE COEFICIENTS DONE
              END IF
C     LOOP TO NEXT VARIABL
C     COPY THE PERM LENS TO THE CURRENT LENS
C     BEFORE DOING THE NEXT VARIABLE
          END DO
C
C     WRITE FOOTER INFO AND CLOSE SENSIOUT.DAT
C
          IF(ITY.EQ.0) CALL SENSIEND
          IF(ITY.EQ.1) CALL INVSENSIEND
C
          DEALLOCATE(SYSA,ALENA,SLVA
     1    ,PIKA,FT01A,LICA,AIPOLYX,AIPOLYY,
     2    GLANMA,ALBL,MULTCLAPA,MULTCOBSA,STAT=ALLOERR)
C
          RETURN
      END
C SUB CVARBLL.FOR
      SUBROUTINE CVARBLL
C
          IMPLICIT NONE
C
          LOGICAL YES,GOFORIT
C
          INTEGER I,VALT,DFDINC,VBSURF
C
          LOGICAL CNOT
C
          REAL*8 WEIT,DINCR,VLOW,VHIGH,SYS12,SYS13,REFHTT
     1    ,COMPNUM,WAVERCW,HM1,HC1
C
          INCLUDE 'datsub.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          INCLUDE 'datcfg.inc'
C
          WEIT=1.0D0
C
C       THIS IS SUBROUTINE CVARBLL. THIS IS THE SUBROUTINE WHICH
C       HANDLES COMP VARIABLE INPUT AND UPDATE COMMANDS AND COMP VARIABLE
C       OUTPUT COMMANDS AT THE CMD LEVEL
C
C       THE ARRAY VARBLL STORES VARIABLE INFORMATION
C       IT IS PASSED IN COMMON IN THE INCLUDE FILE DATSUB.FOR
C
C       VARBLL(I,J) WHERE I COUNTS THE NUMBER OF VARIABLE ENTRIES
C       AND J TAKES ON THE FOLLOWING VALUES AND MEANIINGS.
C
C       J=1  > 1 THROUGH 142, A VARIABLE TYPE DESIGNATOR
C       J=2  > = 1
C       J=3  > 0 THROUGH MAXSUR, THE SURFACE NUMBER DESIGNATOR
C       J=4  > VARIABLE CURRENT VALUE
C       J=5  > VARIABLE PREVIOUS VALUE
C       J=6  > LAST VARIABLE CHANGE VALUE
C       J=7  > WT, THE WEIGHTING FACTOR
C       J=8  > DINCR (DERIVATIVE CHANGE VALUE)
C       J=9  > LOW LIMIT VALUE
C       J=10 > HIGH LIMIT VALUE
C       J=11 > SEQUENTIAL IDENTIFIER IDENTIFYING COMPENSATOR NUMBER
C       J=12 > DEFAULT DINCR FLAG (1=DEFFAULT, 0=USER SET)
C       J=13 > ORIGINAL VALUE
C       J=14 > ENTRY NUMBER IN THE CFADD ARRAY THAT REFERS TO
C              THIS VARIABLE
C       J=15 > DF2
C       J=16 > DF4
C       J=17 > DF5
C
          IF(WC.EQ.'M'.OR.WC.EQ.'C') THEN
              CALL MESCOM
              RETURN
          END IF
C
C       'COMPS' OUTPUT COMP VARIABLE DATA FROM INSIDE
          IF(WC.EQ.'COMPS') THEN
              CALL CVBA
              RETURN
          END IF
C
C       NOW DO CASE OF WC = EOS
C
C***********************************************************************
C       DEAL WITH WC=EOS
          IF(WC.EQ.'EOS') THEN
              IF(SST.EQ.1.OR.SQ.EQ.1.OR.SN.EQ.1)THEN
                  WRITE(OUTLYNE,*)'"EOS" TAKES NO EXPLICIT INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C       PROCEED WITH ACTION FOR COMMAND
              F1=1
              F52=0
              YES=.FALSE.
              DO I=1,MAXCMP
                  IF(ISCOMP(I)) YES=.TRUE.
              END DO
              IF(.NOT.YES) CMPCNT=0
              IF(YES) CMPCNT=1
              IF(CMPCNT.EQ.0) THEN
                  WRITE(OUTLYNE,*)'THE COMPENSATION VARIABLE SUBFILE IS EMPTY'
                  CALL SHOWIT(1)
              END IF
              F1=1
              F52=0
              RETURN
C       ACTION COMPLETED
          END IF
C
C       EOS DONE
C***********************************************************************
C
C       NOW DO WC=DEL
          IF(WC.EQ.'DEL') THEN
              IF(F52.NE.2) THEN
                  WRITE(OUTLYNE,*)
     1            '"DEL" IS ONLY AVAILABLE FROM THE "UPDATE COMPVAR" LEVEL'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'NO ACTION TAKEN'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(SST.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1.OR.S2.EQ.1
     1        .OR.SQ.EQ.1) THEN
                  WRITE(OUTLYNE,*)
     1            '"DEL" ONLY TAKES NUMERIC WORD #1'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(DF1.EQ.1) THEN
                  WRITE(OUTLYNE,*)
     1            '"DEL" REQUIRES EXPLICIT NUMERIC WORD #1 INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(W1.LT.1.0D0.OR.W1.GT.MAXCMP) THEN
                  WRITE(OUTLYNE,*)
     1            '"DEL" REQUIRES NUMERIC INPUT FROM 1 TO 5 ONLY'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(.NOT.ISCOMP(INT(W1))) THEN
                  WRITE(OUTLYNE,*)
     1            'COMPENSATION VARIABLE ',INT(W1),' NOT CURRENTLY DEFINED'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C     HERE IS WHERE VARIABLE IS DELETED
              ISCOMP(INT(W1))=.FALSE.
              VARABL(INT(W1),1:17)=0.0D0
              YES=.FALSE.
              DO I=1,MAXCMP
                  IF(ISCOMP(I)) YES=.TRUE.
              END DO
              IF(.NOT.YES) CMPCNT=0
              IF(YES) CMPCNT=1
              IF(CMPCNT.EQ.0) THEN
                  WRITE(OUTLYNE,*)'THE COMPENSATION VARIABLE SUBFILE IS EMPTY'
                  CALL SHOWIT(1)
              END IF
              RETURN
          END IF
C
C     START DOING THE VARIABLE NAMES HERE
          IF(WC.EQ.'RD') THEN
              WRITE(OUTLYNE,*)
     1        '"RD" IS A POORLY BEHAVED VARIABLE AND WILL BE AUTOMATICALLY'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)
     1        'CONVERTED TO CURVATURE "CV" WHICH IS BETTER BEHAVED.'
              CALL SHOWIT(1)
              WC(1:2)='CV'
          END IF
          IF(WC.EQ.'RDTOR') THEN
              WRITE(OUTLYNE,*)
     1        '"RDTOR" IS A POORLY BEHAVED VARIABLE AND WILL BE AUTOMATICALLY'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)
     1        'CONVERTED TO CURVATURE "CVTOR" WHICH IS BETTER BEHAVED.'
              CALL SHOWIT(1)
              WC(1:5)='CVTOR'
          END IF
          IF(WC.EQ.'RD      ') VALT=1
          IF(WC.EQ.'CV      ') VALT=2
          IF(WC.EQ.'TH      ') VALT=3
          IF(WC.EQ.'CC      ') VALT=4
          IF(WC.EQ.'AD      ') VALT=5
          IF(WC.EQ.'AE      ') VALT=6
          IF(WC.EQ.'AF      ') VALT=7
          IF(WC.EQ.'AG      ') VALT=8
          IF(WC.EQ.'RDTOR   ') VALT=9
          IF(WC.EQ.'CVTOR   ') VALT=10
          IF(WC.EQ.'CCTOR   ') VALT=11
          IF(WC.EQ.'ADTOR   ') VALT=12
          IF(WC.EQ.'AETOR   ') VALT=13
          IF(WC.EQ.'AFTOR   ') VALT=14
          IF(WC.EQ.'AGTOR   ') VALT=15
          IF(WC.EQ.'ALPHA   ') VALT=16
          IF(WC.EQ.'BETA    ') VALT=17
          IF(WC.EQ.'GAMMA   ') VALT=18
          IF(WC.EQ.'XD      ') VALT=19
          IF(WC.EQ.'YD      ') VALT=20
          IF(WC.EQ.'N1      ') VALT=21
          IF(WC.EQ.'N2      ') VALT=22
          IF(WC.EQ.'N3      ') VALT=23
          IF(WC.EQ.'N4      ') VALT=24
          IF(WC.EQ.'N5      ') VALT=25
          IF(WC.EQ.'C1      ') VALT=27
          IF(WC.EQ.'C2      ') VALT=28
          IF(WC.EQ.'C3      ') VALT=29
          IF(WC.EQ.'C4      ') VALT=30
          IF(WC.EQ.'C5      ') VALT=31
          IF(WC.EQ.'C6      ') VALT=32
          IF(WC.EQ.'C7      ') VALT=33
          IF(WC.EQ.'C8      ') VALT=34
          IF(WC.EQ.'C9      ') VALT=35
          IF(WC.EQ.'C10     ') VALT=36
          IF(WC.EQ.'C11     ') VALT=37
          IF(WC.EQ.'C12     ') VALT=38
          IF(WC.EQ.'C13     ') VALT=39
          IF(WC.EQ.'C14     ') VALT=40
          IF(WC.EQ.'C15     ') VALT=41
          IF(WC.EQ.'C16     ') VALT=42
          IF(WC.EQ.'C17     ') VALT=43
          IF(WC.EQ.'C18     ') VALT=44
          IF(WC.EQ.'C19     ') VALT=45
          IF(WC.EQ.'C20     ') VALT=46
          IF(WC.EQ.'C21     ') VALT=47
          IF(WC.EQ.'C22     ') VALT=48
          IF(WC.EQ.'C23     ') VALT=49
          IF(WC.EQ.'C24     ') VALT=50
          IF(WC.EQ.'C25     ') VALT=51
          IF(WC.EQ.'C26     ') VALT=52
          IF(WC.EQ.'C27     ') VALT=53
          IF(WC.EQ.'C28     ') VALT=54
          IF(WC.EQ.'C29     ') VALT=55
          IF(WC.EQ.'C30     ') VALT=56
          IF(WC.EQ.'C31     ') VALT=57
          IF(WC.EQ.'C32     ') VALT=58
          IF(WC.EQ.'C33     ') VALT=59
          IF(WC.EQ.'C34     ') VALT=60
          IF(WC.EQ.'C35     ') VALT=61
          IF(WC.EQ.'C36     ') VALT=62
          IF(WC.EQ.'C37     ') VALT=63
          IF(WC.EQ.'C38     ') VALT=64
          IF(WC.EQ.'C39     ') VALT=65
          IF(WC.EQ.'C40     ') VALT=66
          IF(WC.EQ.'C41     ') VALT=67
          IF(WC.EQ.'C42     ') VALT=68
          IF(WC.EQ.'C43     ') VALT=69
          IF(WC.EQ.'C44     ') VALT=70
          IF(WC.EQ.'C45     ') VALT=71
          IF(WC.EQ.'C46     ') VALT=72
          IF(WC.EQ.'C47     ') VALT=73
          IF(WC.EQ.'C48     ') VALT=74
          IF(WC.EQ.'AC      ') VALT=75
          IF(WC.EQ.'C49     ') VALT=76
          IF(WC.EQ.'C50     ') VALT=77
          IF(WC.EQ.'C51     ') VALT=78
          IF(WC.EQ.'C52     ') VALT=79
          IF(WC.EQ.'C53     ') VALT=80
          IF(WC.EQ.'C54     ') VALT=81
          IF(WC.EQ.'C55     ') VALT=82
          IF(WC.EQ.'C56     ') VALT=83
          IF(WC.EQ.'C57     ') VALT=84
          IF(WC.EQ.'C58     ') VALT=85
          IF(WC.EQ.'C59     ') VALT=86
          IF(WC.EQ.'C60     ') VALT=87
          IF(WC.EQ.'C61     ') VALT=88
          IF(WC.EQ.'C62     ') VALT=89
          IF(WC.EQ.'C63     ') VALT=90
          IF(WC.EQ.'C64     ') VALT=91
          IF(WC.EQ.'C65     ') VALT=92
          IF(WC.EQ.'C66     ') VALT=93
          IF(WC.EQ.'C67     ') VALT=94
          IF(WC.EQ.'C68     ') VALT=95
          IF(WC.EQ.'C69     ') VALT=96
          IF(WC.EQ.'C70     ') VALT=97
          IF(WC.EQ.'C71     ') VALT=98
          IF(WC.EQ.'C72     ') VALT=99
          IF(WC.EQ.'C73     ') VALT=100
          IF(WC.EQ.'C74     ') VALT=101
          IF(WC.EQ.'C75     ') VALT=102
          IF(WC.EQ.'C76     ') VALT=103
          IF(WC.EQ.'C77     ') VALT=104
          IF(WC.EQ.'C78     ') VALT=105
          IF(WC.EQ.'C79     ') VALT=106
          IF(WC.EQ.'C80     ') VALT=107
          IF(WC.EQ.'C81     ') VALT=108
          IF(WC.EQ.'C82     ') VALT=109
          IF(WC.EQ.'C83     ') VALT=110
          IF(WC.EQ.'C84     ') VALT=111
          IF(WC.EQ.'C85     ') VALT=112
          IF(WC.EQ.'C86     ') VALT=113
          IF(WC.EQ.'C87     ') VALT=114
          IF(WC.EQ.'C88     ') VALT=115
          IF(WC.EQ.'C89     ') VALT=116
          IF(WC.EQ.'C90     ') VALT=117
          IF(WC.EQ.'C91     ') VALT=118
          IF(WC.EQ.'C92     ') VALT=119
          IF(WC.EQ.'C93     ') VALT=120
          IF(WC.EQ.'C94     ') VALT=121
          IF(WC.EQ.'C95     ') VALT=122
          IF(WC.EQ.'C96     ') VALT=123
          IF(WC.EQ.'N6      ') VALT=124
          IF(WC.EQ.'N7      ') VALT=125
          IF(WC.EQ.'N8      ') VALT=126
          IF(WC.EQ.'N9      ') VALT=127
          IF(WC.EQ.'N10     ') VALT=128
          IF(WC.EQ.'AH      ') VALT=129
          IF(WC.EQ.'AI      ') VALT=130
          IF(WC.EQ.'AJ      ') VALT=131
          IF(WC.EQ.'AK      ') VALT=132
          IF(WC.EQ.'AL      ') VALT=133
          IF(WC.EQ.'ZD      ') VALT=134
          IF(WC.EQ.'INDEX   ') VALT=135
          IF(WC.EQ.'VNUM    ') VALT=136
          IF(WC.EQ.'PIVX    ') VALT=137
          IF(WC.EQ.'PIVY    ') VALT=138
          IF(WC.EQ.'PIVZ    ') VALT=139
          IF(WC.EQ.'DPART   ') VALT=140
          IF(WC.EQ.'CLPX    ') VALT=141
          IF(WC.EQ.'CLPY    ') VALT=142
          IF(WC.EQ.'GDX     ') VALT=143
          IF(WC.EQ.'GDY     ') VALT=144
          IF(WC.EQ.'GDZ     ') VALT=145
          IF(WC.EQ.'GALPHA  ') VALT=146
          IF(WC.EQ.'GBETA   ') VALT=147
          IF(WC.EQ.'GGAMMA  ') VALT=148
          IF(WC.EQ.'GRS     ') VALT=149
          IF(VALT.EQ.-1) THEN
              WRITE(OUTLYNE,*)
     1        'INVALID VARIABLE NAME'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
C     NUMERIC WORDS AND DEFAULTS
C     NW1 IS SEQUENTIAL COUNTER
          IF(DF1.EQ.1.OR.W1.LT.1.0D0.OR.W1.GT.5.0D0) THEN
C     DEFAULT INPUT
              WRITE(OUTLYNE,*)
     1        'NUMERIC WORD #1 (COMPENSATOR NUMBER) MISSING OR'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)
     1        'BEYOND LEGAL BOUNDS'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          ELSE
              COMPNUM=W1
          END IF
C     NW2 IS SURFACE NUMBER
          IF(DF2.EQ.1) THEN
              WRITE(OUTLYNE,*)'VARIABLE NAME = ',WC
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'REQUIRES EXPLICIT NUMERIC WORD #2 INPUT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(W2.LT.0.0D0) W2=SYSTEM1(20)+W2
          IF(INT(W2).LT.0.OR.INT(W2).GT.INT(SYSTEM1(20))) THEN
              WRITE(OUTLYNE,*)
     1        'NUMERIC WORD #2 (SURFACE NUMBER) BEYOND LEGAL BOUNDS'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          ELSE
C     NW2 OK, PROCEED
              VBSURF=INT(W2)
          END IF
C     NW3 IS DINCR VALUE
          DFDINC=0
          IF(DF3.EQ.1) THEN
              DFDINC=1
C
C     CAL MAX REF AP HT FOR ASPHERIC DINCR SETTINGS
C
C       CLAP IS ON REFERENCE SURFACE
C
C       CIRCULAR CLAP
C
              IF(ALENS(127,NEWREF).EQ.0.0D0) THEN
                  IF(DABS(ALENS(9,NEWREF)).EQ.1.0D0) THEN
                      SYS12=ALENS(10,NEWREF)
                      SYS13=ALENS(10,NEWREF)
                  END IF
C        RECT CLAP
C
                  IF(DABS(ALENS(9,NEWREF)).EQ.2.0D0) THEN
                      SYS12=ALENS(10,NEWREF)
                      SYS13=ALENS(11,NEWREF)
                  END IF
C        ELIP CLAP
C
                  IF(DABS(ALENS(9,NEWREF)).EQ.3.0D0) THEN
                      SYS12=ALENS(10,NEWREF)
                      SYS13=ALENS(11,NEWREF)
                  END IF
C        RCTK CLAP
C
                  IF(DABS(ALENS(9,NEWREF)).EQ.4.0D0) THEN
                      SYS12=ALENS(10,NEWREF)
                      SYS13=ALENS(11,NEWREF)
                  END IF
C        POLY CLAP
C
                  IF(DABS(ALENS(9,NEWREF)).EQ.5.0D0) THEN
                      SYS12=ALENS(10,NEWREF)
                      SYS13=ALENS(10,NEWREF)
                  END IF
C        IPOLY CLAP
C
                  IF(DABS(ALENS(9,NEWREF)).EQ.6.0D0) THEN
                      SYS12=ALENS(14,NEWREF)
                      SYS13=ALENS(14,NEWREF)
                  END IF
              END IF
C
C       NO CLAP ON REF SURF.
              IF(DABS(ALENS(9,NEWREF)).EQ.0.0D0) THEN
C       NO CLAP ON REF SURF.
                  SYS13=PXTRAX(1,NEWREF)
                  SYS12=PXTRAY(1,NEWREF)
              END IF
              IF(SYS12.GT.SYS13) REFHTT=SYS12
              IF(SYS12.LE.SYS13) REFHTT=SYS13
C
C     THE DEFAULT DINCR FOR CV AND CVR MUST BE ADJUSTED
C
C     DEFAULT INPUT
              IF(WC.EQ.'RD      ') DINCR=DINC2
              IF(WC.EQ.'CV      ') DINCR=DINC2
              IF(WC.EQ.'TH      ') DINCR=DINC3
              IF(WC.EQ.'CC      ') DINCR=DINC4
              IF(WC.EQ.'AD'.OR.WC.EQ.'AE'.OR.WC.EQ.'AF'.OR.
     2        WC.EQ.'AG'.OR.WC.EQ.'AH'.OR.WC.EQ.'AI'.OR.
     3        WC.EQ.'AJ'.OR.WC.EQ.'AK'.OR.WC.EQ.'AL'.OR.WC.EQ.'AC') THEN
                  GOFORIT=.FALSE.
                  IF(ALENS(23,INT(W2)).EQ.0.0D0.OR.ALENS(23,INT(W2)).EQ.1.0D0) THEN
C     NO TORIC OR Y-TORIC
                      IF(PXTRAY(1,INT(W2)).NE.0.0D0.OR.PXTRAY(5,INT(W2)).NE.0.0D0) THEN
                          HM1=PXTRAY(1,INT(W2))
                          HC1=PXTRAY(5,INT(W2))
                          GOFORIT=.TRUE.
                      END IF
                  ELSE
C     X-TORIC
                      IF(PXTRAX(1,INT(W2)).NE.0.0D0.OR.PXTRAX(5,INT(W2)).NE.0.0D0) THEN
                          HM1=PXTRAX(1,INT(W2))
                          HC1=PXTRAX(5,INT(W2))
                          GOFORIT=.TRUE.
                      END IF
                  END IF
                  IF(GOFORIT) THEN
C     USE SWANTNER'S ALGORITHM TO CALCULATE DINCR BASED ON 1/4 WAVE
C     AT THE CONTROL WAVELENGTH
                      IF(SYSTEM1(11).EQ.1.0D0)  WAVERCW=SYSTEM1(1)/4.0D0
                      IF(SYSTEM1(11).EQ.2.0D0)  WAVERCW=SYSTEM1(2)/4.0D0
                      IF(SYSTEM1(11).EQ.3.0D0)  WAVERCW=SYSTEM1(3)/4.0D0
                      IF(SYSTEM1(11).EQ.4.0D0)  WAVERCW=SYSTEM1(4)/4.0D0
                      IF(SYSTEM1(11).EQ.5.0D0)  WAVERCW=SYSTEM1(5)/4.0D0
                      IF(SYSTEM1(11).EQ.6.0D0)  WAVERCW=SYSTEM1(71)/4.0D0
                      IF(SYSTEM1(11).EQ.7.0D0)  WAVERCW=SYSTEM1(72)/4.0D0
                      IF(SYSTEM1(11).EQ.8.0D0)  WAVERCW=SYSTEM1(73)/4.0D0
                      IF(SYSTEM1(11).EQ.9.0D0)  WAVERCW=SYSTEM1(74)/4.0D0
                      IF(SYSTEM1(11).EQ.10.0D0) WAVERCW=SYSTEM1(75)/4.0D0
                      IF(SYSTEM1(6).EQ.1.0D0) WAVERCW=WAVERCW*(1.0D-3)/25.4D0
                      IF(SYSTEM1(6).EQ.2.0D0) WAVERCW=WAVERCW*(1.0D-4)
                      IF(SYSTEM1(6).EQ.3.0D0) WAVERCW=WAVERCW*(1.0D-3)
                      IF(SYSTEM1(6).EQ.4.0D0) WAVERCW=WAVERCW*(1.0D-6)
                      IF(WC.EQ.'AC') THEN
                          DINCR=WAVERCW/
     1                    ((DABS(HM1)+DABS(HC1))**2)
                      END IF
                      IF(WC.EQ.'AD') THEN
                          DINCR=WAVERCW/
     1                    ((DABS(HM1)+DABS(HC1))**4)
                      END IF
                      IF(WC.EQ.'AE') THEN
                          DINCR=WAVERCW/
     1                    ((DABS(HM1)+DABS(HC1))**6)
                      END IF
                      IF(WC.EQ.'AF') THEN
                          DINCR=WAVERCW/
     1                    ((DABS(HM1)+DABS(HC1))**8)
                      END IF
                      IF(WC.EQ.'AG') THEN
                          DINCR=WAVERCW/
     1                    ((DABS(HM1)+DABS(HC1))**10)
                      END IF
                      IF(WC.EQ.'AH') THEN
                          DINCR=WAVERCW/
     1                    ((DABS(HM1)+DABS(HC1))**12)
                      END IF
                      IF(WC.EQ.'AI') THEN
                          DINCR=WAVERCW/
     1                    ((DABS(HM1)+DABS(HC1))**14)
                      END IF
                      IF(WC.EQ.'AJ') THEN
                          DINCR=WAVERCW/
     1                    ((DABS(HM1)+DABS(HC1))**16)
                      END IF
                      IF(WC.EQ.'AK') THEN
                          DINCR=WAVERCW/
     1                    ((DABS(HM1)+DABS(HC1))**18)
                      END IF
                      IF(WC.EQ.'AL') THEN
                          DINCR=WAVERCW/
     1                    ((DABS(HM1)+DABS(HC1))**20)
                      END IF
                  ELSE
C     DON'T USE SWANTNER'S ALGORITHM FOR THE DEFAULT
                      IF(REFHTT.GE.1.0D0) THEN
                          IF(WC.EQ.'AD      ') DINCR=DINC5A
                          IF(WC.EQ.'AE      ') DINCR=DINC6A
                          IF(WC.EQ.'AF      ') DINCR=DINC7A
                          IF(WC.EQ.'AG      ') DINCR=DINC8A
                          IF(WC.EQ.'AH      ') DINCR=DINC9A
                          IF(WC.EQ.'AI      ') DINCR=DINC10A
                          IF(WC.EQ.'AJ      ') DINCR=DINC11A
                          IF(WC.EQ.'AK      ') DINCR=DINC12A
                          IF(WC.EQ.'AL      ') DINCR=DINC13A
                          IF(WC.EQ.'AC      ') DINCR=DINC14A
                      ELSE
                          IF(WC.EQ.'AD      ') DINCR=DINC5B
                          IF(WC.EQ.'AE      ') DINCR=DINC6B
                          IF(WC.EQ.'AF      ') DINCR=DINC7B
                          IF(WC.EQ.'AG      ') DINCR=DINC8B
                          IF(WC.EQ.'AH      ') DINCR=DINC9B
                          IF(WC.EQ.'AI      ') DINCR=DINC10B
                          IF(WC.EQ.'AJ      ') DINCR=DINC11B
                          IF(WC.EQ.'AK      ') DINCR=DINC12B
                          IF(WC.EQ.'AL      ') DINCR=DINC13B
                          IF(WC.EQ.'AC      ') DINCR=DINC14B
                      END IF
                  END IF
              END IF
              IF(WC.EQ.'RDTOR   ') DINCR=DINC16
              IF(WC.EQ.'CVTOR   ') DINCR=DINC16
              IF(WC.EQ.'CCTOR   ') DINCR=DINC17
              IF(WC.EQ.'ADTOR'.OR.WC.EQ.'AETOR'.OR.WC.EQ.'AFTOR'.OR.
     2        WC.EQ.'AGTOR') THEN
                  GOFORIT=.FALSE.
                  IF(ALENS(23,INT(W2)).EQ.0.0D0.OR.ALENS(23,INT(W2)).EQ.1.0D0) THEN
C     NO TORIC OR Y-TORIC
                      IF(PXTRAX(1,INT(W2)).NE.0.0D0.OR.PXTRAX(5,INT(W2)).NE.0.0D0) THEN
                          HM1=PXTRAX(1,INT(W2))
                          HC1=PXTRAX(5,INT(W2))
                          GOFORIT=.TRUE.
                      END IF
                  ELSE
C     X-TORIC
                      IF(PXTRAY(1,INT(W2)).NE.0.0D0.OR.PXTRAY(5,INT(W2)).NE.0.0D0) THEN
                          HM1=PXTRAY(1,INT(W2))
                          HC1=PXTRAY(5,INT(W2))
                          GOFORIT=.TRUE.
                      END IF
                  END IF
                  IF(GOFORIT) THEN
C     USE SWANTNER'S ALGORITHM TO CALCULATE DINCR BASED ON 1/4 WAVE
C     AT THE CONTROL WAVELENGTH
                      IF(SYSTEM1(11).EQ.1.0D0)  WAVERCW=SYSTEM1(1)/4.0D0
                      IF(SYSTEM1(11).EQ.2.0D0)  WAVERCW=SYSTEM1(2)/4.0D0
                      IF(SYSTEM1(11).EQ.3.0D0)  WAVERCW=SYSTEM1(3)/4.0D0
                      IF(SYSTEM1(11).EQ.4.0D0)  WAVERCW=SYSTEM1(4)/4.0D0
                      IF(SYSTEM1(11).EQ.5.0D0)  WAVERCW=SYSTEM1(5)/4.0D0
                      IF(SYSTEM1(11).EQ.6.0D0)  WAVERCW=SYSTEM1(71)/4.0D0
                      IF(SYSTEM1(11).EQ.7.0D0)  WAVERCW=SYSTEM1(72)/4.0D0
                      IF(SYSTEM1(11).EQ.8.0D0)  WAVERCW=SYSTEM1(73)/4.0D0
                      IF(SYSTEM1(11).EQ.9.0D0)  WAVERCW=SYSTEM1(74)/4.0D0
                      IF(SYSTEM1(11).EQ.10.0D0) WAVERCW=SYSTEM1(75)/4.0D0
                      IF(SYSTEM1(6).EQ.1.0D0) WAVERCW=WAVERCW*(1.0D-3)/25.4D0
                      IF(SYSTEM1(6).EQ.2.0D0) WAVERCW=WAVERCW*(1.0D-4)
                      IF(SYSTEM1(6).EQ.3.0D0) WAVERCW=WAVERCW*(1.0D-3)
                      IF(SYSTEM1(6).EQ.4.0D0) WAVERCW=WAVERCW*(1.0D-6)
                      IF(WC.EQ.'ADTOR') THEN
                          DINCR=WAVERCW/
     1                    ((DABS(HM1)+DABS(HC1))**4)
                      END IF
                      IF(WC.EQ.'AETOR') THEN
                          DINCR=WAVERCW/
     1                    ((DABS(HM1)+DABS(HC1))**6)
                      END IF
                      IF(WC.EQ.'AFTOR') THEN
                          DINCR=WAVERCW/
     1                    ((DABS(HM1)+DABS(HC1))**8)
                      END IF
                      IF(WC.EQ.'AGTOR') THEN
                          DINCR=WAVERCW/
     1                    ((DABS(HM1)+DABS(HC1))**10)
                      END IF
                  ELSE
C     DON'T USE SWANTNER'S ALGORITHM FOR THE DEFAULT
                      IF(REFHTT.GE.1.0D0) THEN
                          IF(WC.EQ.'ADTOR   ') DINCR=DINC18A
                          IF(WC.EQ.'AETOR   ') DINCR=DINC19A
                          IF(WC.EQ.'AFTOR   ') DINCR=DINC20A
                          IF(WC.EQ.'AGTOR   ') DINCR=DINC21A
                      ELSE
                          IF(WC.EQ.'ADTOR   ') DINCR=DINC18B
                          IF(WC.EQ.'AETOR   ') DINCR=DINC19B
                          IF(WC.EQ.'AFTOR   ') DINCR=DINC20B
                          IF(WC.EQ.'AGTOR   ') DINCR=DINC21B
                      END IF
                  END IF
              END IF
              IF(WC.EQ.'ALPHA   ') DINCR=DINC22
              IF(WC.EQ.'BETA    ') DINCR=DINC23
              IF(WC.EQ.'GAMMA   ') DINCR=DINC24
              IF(WC.EQ.'XD      ') DINCR=DINC25
              IF(WC.EQ.'YD      ') DINCR=DINC26
              IF(WC.EQ.'ZD      ') DINCR=DINC29
              IF(WC.EQ.'GALPHA  ') DINCR=DINC22
              IF(WC.EQ.'GBETA   ') DINCR=DINC23
              IF(WC.EQ.'GGAMMA  ') DINCR=DINC24
              IF(WC.EQ.'GDX     ') DINCR=DINC25
              IF(WC.EQ.'GDY     ') DINCR=DINC26
              IF(WC.EQ.'GDZ     ') DINCR=DINC29
              IF(WC.EQ.'PIVX    ') DINCR=DINC25
              IF(WC.EQ.'PIVY    ') DINCR=DINC26
              IF(WC.EQ.'PIVZ    ') DINCR=DINC29
              IF(WC.EQ.'N1      ') DINCR=DINC27
              IF(WC.EQ.'N2      ') DINCR=DINC27
              IF(WC.EQ.'N3      ') DINCR=DINC27
              IF(WC.EQ.'N4      ') DINCR=DINC27
              IF(WC.EQ.'N5      ') DINCR=DINC27
              IF(WC.EQ.'N6      ') DINCR=DINC27
              IF(WC.EQ.'N7      ') DINCR=DINC27
              IF(WC.EQ.'N8      ') DINCR=DINC27
              IF(WC.EQ.'N9      ') DINCR=DINC27
              IF(WC.EQ.'N10     ') DINCR=DINC27
              IF(WC.EQ.'C1      ') DINCR=DINC28
              IF(WC.EQ.'C2      ') DINCR=DINC28
              IF(WC.EQ.'C3      ') DINCR=DINC28
              IF(WC.EQ.'C4      ') DINCR=DINC28
              IF(WC.EQ.'C5      ') DINCR=DINC28
              IF(WC.EQ.'C6      ') DINCR=DINC28
              IF(WC.EQ.'C7      ') DINCR=DINC28
              IF(WC.EQ.'C8      ') DINCR=DINC28
              IF(WC.EQ.'C9      ') DINCR=DINC28
              IF(WC.EQ.'C10     ') DINCR=DINC28
              IF(WC.EQ.'C11     ') DINCR=DINC28
              IF(WC.EQ.'C12     ') DINCR=DINC28
              IF(WC.EQ.'C13     ') DINCR=DINC28
              IF(WC.EQ.'C14     ') DINCR=DINC28
              IF(WC.EQ.'C15     ') DINCR=DINC28
              IF(WC.EQ.'C16     ') DINCR=DINC28
              IF(WC.EQ.'C17     ') DINCR=DINC28
              IF(WC.EQ.'C18     ') DINCR=DINC28
              IF(WC.EQ.'C19     ') DINCR=DINC28
              IF(WC.EQ.'C20     ') DINCR=DINC28
              IF(WC.EQ.'C21     ') DINCR=DINC28
              IF(WC.EQ.'C22     ') DINCR=DINC28
              IF(WC.EQ.'C23     ') DINCR=DINC28
              IF(WC.EQ.'C24     ') DINCR=DINC28
              IF(WC.EQ.'C25     ') DINCR=DINC28
              IF(WC.EQ.'C26     ') DINCR=DINC28
              IF(WC.EQ.'C27     ') DINCR=DINC28
              IF(WC.EQ.'C28     ') DINCR=DINC28
              IF(WC.EQ.'C29     ') DINCR=DINC28
              IF(WC.EQ.'C30     ') DINCR=DINC28
              IF(WC.EQ.'C31     ') DINCR=DINC28
              IF(WC.EQ.'C32     ') DINCR=DINC28
              IF(WC.EQ.'C33     ') DINCR=DINC28
              IF(WC.EQ.'C34     ') DINCR=DINC28
              IF(WC.EQ.'C35     ') DINCR=DINC28
              IF(WC.EQ.'C36     ') DINCR=DINC28
              IF(WC.EQ.'C37     ') DINCR=DINC28
              IF(WC.EQ.'C38     ') DINCR=DINC28
              IF(WC.EQ.'C39     ') DINCR=DINC28
              IF(WC.EQ.'C40     ') DINCR=DINC28
              IF(WC.EQ.'C41     ') DINCR=DINC28
              IF(WC.EQ.'C42     ') DINCR=DINC28
              IF(WC.EQ.'C43     ') DINCR=DINC28
              IF(WC.EQ.'C44     ') DINCR=DINC28
              IF(WC.EQ.'C45     ') DINCR=DINC28
              IF(WC.EQ.'C46     ') DINCR=DINC28
              IF(WC.EQ.'C47     ') DINCR=DINC28
              IF(WC.EQ.'C48     ') DINCR=DINC28
              IF(WC.EQ.'C49     ') DINCR=DINC28
              IF(WC.EQ.'C50     ') DINCR=DINC28
              IF(WC.EQ.'C51     ') DINCR=DINC28
              IF(WC.EQ.'C52     ') DINCR=DINC28
              IF(WC.EQ.'C53     ') DINCR=DINC28
              IF(WC.EQ.'C54     ') DINCR=DINC28
              IF(WC.EQ.'C55     ') DINCR=DINC28
              IF(WC.EQ.'C56     ') DINCR=DINC28
              IF(WC.EQ.'C57     ') DINCR=DINC28
              IF(WC.EQ.'C58     ') DINCR=DINC28
              IF(WC.EQ.'C59     ') DINCR=DINC28
              IF(WC.EQ.'C60     ') DINCR=DINC28
              IF(WC.EQ.'C61     ') DINCR=DINC28
              IF(WC.EQ.'C62     ') DINCR=DINC28
              IF(WC.EQ.'C63     ') DINCR=DINC28
              IF(WC.EQ.'C64     ') DINCR=DINC28
              IF(WC.EQ.'C65     ') DINCR=DINC28
              IF(WC.EQ.'C66     ') DINCR=DINC28
              IF(WC.EQ.'C67     ') DINCR=DINC28
              IF(WC.EQ.'C68     ') DINCR=DINC28
              IF(WC.EQ.'C69     ') DINCR=DINC28
              IF(WC.EQ.'C70     ') DINCR=DINC28
              IF(WC.EQ.'C71     ') DINCR=DINC28
              IF(WC.EQ.'C72     ') DINCR=DINC28
              IF(WC.EQ.'C73     ') DINCR=DINC28
              IF(WC.EQ.'C74     ') DINCR=DINC28
              IF(WC.EQ.'C75     ') DINCR=DINC28
              IF(WC.EQ.'C76     ') DINCR=DINC28
              IF(WC.EQ.'C77     ') DINCR=DINC28
              IF(WC.EQ.'C78     ') DINCR=DINC28
              IF(WC.EQ.'C79     ') DINCR=DINC28
              IF(WC.EQ.'C80     ') DINCR=DINC28
              IF(WC.EQ.'C81     ') DINCR=DINC28
              IF(WC.EQ.'C82     ') DINCR=DINC28
              IF(WC.EQ.'C83     ') DINCR=DINC28
              IF(WC.EQ.'C84     ') DINCR=DINC28
              IF(WC.EQ.'C85     ') DINCR=DINC28
              IF(WC.EQ.'C86     ') DINCR=DINC28
              IF(WC.EQ.'C87     ') DINCR=DINC28
              IF(WC.EQ.'C88     ') DINCR=DINC28
              IF(WC.EQ.'C89     ') DINCR=DINC28
              IF(WC.EQ.'C90     ') DINCR=DINC28
              IF(WC.EQ.'C91     ') DINCR=DINC28
              IF(WC.EQ.'C92     ') DINCR=DINC28
              IF(WC.EQ.'C93     ') DINCR=DINC28
              IF(WC.EQ.'C94     ') DINCR=DINC28
              IF(WC.EQ.'C95     ') DINCR=DINC28
              IF(WC.EQ.'C96     ') DINCR=DINC28
              IF(WC.EQ.'GRT     ') DINCR=DINC28
          END IF
          IF(DF3.EQ.0) DINCR=W3
          IF(DF4.EQ.0.OR.DF5.EQ.0) THEN
              WRITE(OUTLYNE,*)
     1        'COMPENSATOR INPUT USES NO NUMERIC WORD #4 OR #5 INPUT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C     THE LOW VALUE
C
          VLOW=-1.0D20
C     THE HIGH VALUE
C
          VHIGH=+1.0D20
C     NOW BUILD THE VARIABLE ENTRY
C       J=1  > 1 THROUGH 142, A VARIABLE TYPE DESIGNATOR
C       J=2  > 1 THROUGH MAXCFG, THE CONFIGURATION DESIGNATOR
C       J=3  > 0 THROUGH MAXSUR, THE SURFACE NUMBER DESIGNATOR
C       J=4  > VARIABLE CURRENT VALUE
C       J=5  > VARIABLE PREVIOUS VALUE
C       J=6  > LAST VARIABLE CHANGE VALUE
C       J=7  > WT, THE WEIGHTING FACTOR
C       J=8  > DINCR (DERIVATIVE CHANGE VALUE)
C       J=9  > LOW LIMIT VALUE
C       J=10 > HIGH LIMIT VALUE
C       J=11 > SEQUENTIAL IDENTIFIER OF COMPENSATOR
C       J=12 > DEFAULT DINCR FLAG (1=DEFFAULT, 0=USER SET)(DF3)
C       J=13 > ORIGINAL VALUE
C       J=14 > ENTRY NUMBER IN THE CFADD ARRAY THAT REFERS TO
C       J=15 > DF2
C       J=16 > DF4
C       J=17 > DF5
C
          VARNAM(INT(W1))=WC
          VARABL(INT(W1),1) =DBLE(VALT)
          VARABL(INT(W1),2) =1.0D0
          VARABL(INT(W1),3) =DBLE(VBSURF)
          VARABL(INT(W1),4) =0.0D0
          VARABL(INT(W1),5) =0.0D0
          VARABL(INT(W1),6) =0.0D0
          VARABL(INT(W1),7) =WEIT
          VARABL(INT(W1),8) =DINCR
          VARABL(INT(W1),9) =VLOW
          VARABL(INT(W1),10)=VHIGH
          VARABL(INT(W1),11)=COMPNUM
          DFDINC=DF3
          VARABL(INT(W1),12)=DBLE(DFDINC)
          VARABL(INT(W1),13)=0.0D0
          VARABL(INT(W1),14)=0.0D0
          VARABL(INT(W1),15)=DBLE(DF2)
          VARABL(INT(W1),16)=DBLE(DF4)
          VARABL(INT(W1),17)=DBLE(DF5)

C
C     THIS LINE IS ALWAYS TRUE WHEN THE CONFIG IS #1
          VARABL(INT(W1),14)=0.0D0
C     WE USE THE LENS DATA ARRAY
C       J=4  > VARIABLE CURRENT VALUE
C       J=5  > VARIABLE PREVIOUS VALUE
C     VARABL(INT(W1),4)
C     VARABL(INT(W1),5)
C     WE FIRST CHECK TO SEE IF THE VARIABLE IS BEING
C     CONTROLLED BY A SOLVE. IF IT IS, THE ADDITION
C     OF THIS VARIABLE TO THE VARIABLE SUBFIL WILL BE
C     DISALLOWED AND THE COUNTER WILL NOT BE ADVANCED.
C     IF IT IS NOT CONTROLLED BY A SOLVE, THEN PIKUPS WILL BE
C     CHECKED FOLLOWED BY TILT AUTO ASSIGNMENTS CHECKS AS NECESSARY.
C
 666      FORMAT(
     1    'VARIABLE NAME = ',A8,'AT SURFACE # = ',I3)
C
C     DO CV AND RD
          IF(VALT.EQ.2) THEN
C     IF SURFACE IS NON-TORIC WITH A CURVATURE SOLVE OR THE
C     SURFACE IS X-TORIC WITH AN X CURVATURE SOLVE OR THE
C     SURFACE IS Y-TORIC WITH AN Y CURVATURE SOLVE
C     THEN DISALLOW VARIABLE
              IF(ALENS(23,VBSURF).EQ.0.0D0.AND.ALENS(33,VBSURF).GT.1.0D0
     2        .OR.ALENS(23,VBSURF).EQ.1.0D0.AND.
     3        SOLVE(8,VBSURF).GT.0.0D0.OR.
     2        ALENS(23,VBSURF).EQ.2.0D0.AND.
     3        SOLVE(2,VBSURF).GT.0.0D0) THEN
                  WRITE(OUTLYNE,*)
     1            'A CONFLICTING CURVATURE SOLVE EXISTS ON THIS SURFACE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,666) WC,INT(W2)
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'VARIABLE ASSIGNMENT WAS NOT MADE'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C     PIKUP CHECK
C     IF A CV,RD,PRO OR NPRO PIKUPS EXISTS THEN
              IF(PIKUP(1,VBSURF,1).EQ.1.0D0
     1        .OR.PIKUP(1,VBSURF,2).EQ.1.0D0
     1        .OR.PIKUP(1,VBSURF,11).EQ.1.0D0
     1        .OR.PIKUP(1,VBSURF,12).EQ.1.0D0) THEN
C     A CV OR RD PIKUP EXISTS, DISSALLOW THE VARIABLE ASSIGNMENT
                  WRITE(OUTLYNE,*)
     1            'A CONFLICTING SURFACE SHAPE PIKUP EXISTS ON THIS SURFACE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,666) WC,INT(W2)
                  WRITE(OUTLYNE,*)'VARIABLE ASSIGNMENT WAS NOT MADE'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              ELSE
C     NO CONFLICTING PIKUP OR SOLVES EXIST, ASSIGN THE VARIABLE
C
                  IF(VALT.EQ.2) VARABL(INT(W1),4)=ALENS(1,VBSURF)
                  IF(VALT.EQ.2) VARABL(INT(W1),5)=ALENS(1,VBSURF)
                  IF(VALT.EQ.2) VARABL(INT(W1),13)=ALENS(1,VBSURF)
                  ISCOMP(INT(W1))=.TRUE.
C
              END IF
              RETURN
          END IF
C
C     DO TH VARIABLE
          IF(VALT.EQ.3) THEN
              IF(VBSURF.EQ.NEWIMG) THEN
                  WRITE(OUTLYNE,999) WC
 999              FORMAT(
     1            'VARIABLE NAME = ',A8,'NOT USABLE AT THE IMAGE SURFACE')
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(ALENS(33,VBSURF).EQ.1.0D0.OR.ALENS(33,VBSURF).EQ.3.0D0) THEN
C     TH SOLVE EXISTS, DISALLOW VARIABLE
                  WRITE(OUTLYNE,*)
     1            'A CONFLICTING THICKNESS SOLVE EXISTS ON THIS SURFACE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,666) WC,INT(W2)
                  WRITE(OUTLYNE,*)'VARIABLE ASSIGNMENT WAS NOT MADE'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C     PIKUP CHECK
              IF(PIKUP(1,VBSURF,3).EQ.1.0D0.OR.PIKUP(1,VBSURF,32).EQ.1.0D0)
     1         THEN
C     A THICKNESS PIKUP EXISTS, DISSALLOW THE VARIABLE ASSIGNMENT
                  WRITE(OUTLYNE,*)
     1            'A CONFLICTING THICKNESS PIKUP EXISTS ON THIS SURFACE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,666) WC,INT(W2)
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'VARIABLE ASSIGNMENT WAS NOT MADE'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              ELSE
C     NO TH PIKUP OR SOLVES EXIST, ASSIGN THE VARIABLE
C
                  VARABL(INT(W1),4)=ALENS(3,VBSURF)
C
                  VARABL(INT(W1),5)=ALENS(3,VBSURF)
                  VARABL(INT(W1),13)=ALENS(3,VBSURF)
                  ISCOMP(INT(W1))=.TRUE.
              END IF
              RETURN
          END IF
C     DO CC
          IF(VALT.EQ.4) THEN
C     PIKUP CHECK
C     IF A CC,PRO OR NPRO PIKUPS EXISTS THEN
              IF(PIKUP(1,VBSURF,4).EQ.1.0D0
     1        .OR.PIKUP(1,VBSURF,11).EQ.1.0D0
     1        .OR.PIKUP(1,VBSURF,12).EQ.1.0D0) THEN
C     A CV OR RD PIKUP EXISTS, DISSALLOW THE VARIABLE ASSIGNMENT
                  WRITE(OUTLYNE,*)
     1            'A CONFLICTING SURFACE SHAPE PIKUP EXISTS ON THIS SURFACE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,666) WC,INT(W2)
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'VARIABLE ASSIGNMENT WAS NOT MADE'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              ELSE
C     NO CONFLICTING PIKUP EXISTS, ASSIGN THE VARIABLE
C
                  IF(VALT.EQ.4) VARABL(INT(W1),4)=ALENS(2,VBSURF)
C
                  IF(VALT.EQ.4) VARABL(INT(W1),5)=ALENS(2,VBSURF)
                  IF(VALT.EQ.4) VARABL(INT(W1),13)=ALENS(2,VBSURF)
                  ISCOMP(INT(W1))=.TRUE.
              END IF
              RETURN
          END IF
          IF(VALT.EQ.5) THEN
              IF(ALENS(8,VBSURF).NE.1.0D0) ALENS(8,VBSURF)=1.0D0
C     PIKUP CHECK
C     IF A AD,PRO OR NPRO PIKUPS EXISTS THEN
              IF(PIKUP(1,VBSURF,5).EQ.1.0D0
     1        .OR.PIKUP(1,VBSURF,11).EQ.1.0D0
     1        .OR.PIKUP(1,VBSURF,12).EQ.1.0D0) THEN
C     A CONFLICTING SHAPE PIKUP EXISTS, DISSALLOW THE VARIABLE ASSIGNMENT
                  WRITE(OUTLYNE,*)
     1            'A CONFLICTING SURFACE SHAPE PIKUP EXISTS ON THIS SURFACE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,666) WC,INT(W2)
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'VARIABLE ASSIGNMENT WAS NOT MADE'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              ELSE
C     NO CONFLICTING PIKUP EXISTS, ASSIGN THE VARIABLE
C
                  IF(VALT.EQ.5) VARABL(INT(W1),4)=ALENS(4,VBSURF)
C
                  IF(VALT.EQ.5) VARABL(INT(W1),5)=ALENS(4,VBSURF)
                  IF(VALT.EQ.5) VARABL(INT(W1),13)=ALENS(4,VBSURF)
                  ISCOMP(INT(W1))=.TRUE.
              END IF
              RETURN
          END IF
C
          IF(VALT.EQ.6) THEN
              IF(ALENS(8,VBSURF).NE.1.0D0) ALENS(8,VBSURF)=1.0D0
C     PIKUP CHECK
C     IF A AE,PRO OR NPRO PIKUPS EXISTS THEN
              IF(PIKUP(1,VBSURF,6).EQ.1.0D0
     1        .OR.PIKUP(1,VBSURF,11).EQ.1.0D0
     1        .OR.PIKUP(1,VBSURF,12).EQ.1.0D0) THEN
C     A CONFLICTING SHAPE PIKUP EXISTS, DISSALLOW THE VARIABLE ASSIGNMENT
                  WRITE(OUTLYNE,*)
     1            'A CONFLICTING SURFACE SHAPE PIKUP EXISTS ON THIS SURFACE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,666) WC,INT(W2)
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'VARIABLE ASSIGNMENT WAS NOT MADE'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              ELSE
C     NO CONFLICTING PIKUP EXISTS, ASSIGN THE VARIABLE
C
                  IF(VALT.EQ.6) VARABL(INT(W1),4)=ALENS(5,VBSURF)
C
                  IF(VALT.EQ.6) VARABL(INT(W1),5)=ALENS(5,VBSURF)
                  IF(VALT.EQ.6) VARABL(INT(W1),13)=ALENS(5,VBSURF)
                  ISCOMP(INT(W1))=.TRUE.
              END IF
              RETURN
          END IF
C
          IF(VALT.EQ.7) THEN
              IF(ALENS(8,VBSURF).NE.1.0D0) ALENS(8,VBSURF)=1.0D0
C     PIKUP CHECK
C     IF A AF,PRO OR NPRO PIKUPS EXISTS THEN
              IF(PIKUP(1,VBSURF,7).EQ.1.0D0
     1        .OR.PIKUP(1,VBSURF,11).EQ.1.0D0
     1        .OR.PIKUP(1,VBSURF,12).EQ.1.0D0) THEN
C     A CONFLICTING SHAPE PIKUP EXISTS, DISSALLOW THE VARIABLE ASSIGNMENT
                  WRITE(OUTLYNE,*)
     1            'A CONFLICTING SURFACE SHAPE PIKUP EXISTS ON THIS SURFACE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,666) WC,INT(W2)
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'VARIABLE ASSIGNMENT WAS NOT MADE'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              ELSE
C     NO CONFLICTING PIKUP EXISTS, ASSIGN THE VARIABLE
C
                  IF(VALT.EQ.7) VARABL(INT(W1),4)=ALENS(6,VBSURF)
C
                  IF(VALT.EQ.7) VARABL(INT(W1),5)=ALENS(6,VBSURF)
                  IF(VALT.EQ.7) VARABL(INT(W1),13)=ALENS(6,VBSURF)
                  ISCOMP(INT(W1))=.TRUE.
              END IF
              RETURN
          END IF
C
          IF(VALT.EQ.8) THEN
              IF(ALENS(8,VBSURF).NE.1.0D0) ALENS(8,VBSURF)=1.0D0
C     PIKUP CHECK
C     IF A AG,PRO OR NPRO PIKUPS EXISTS THEN
              IF(PIKUP(1,VBSURF,8).EQ.1.0D0
     1        .OR.PIKUP(1,VBSURF,11).EQ.1.0D0
     1        .OR.PIKUP(1,VBSURF,12).EQ.1.0D0) THEN
C     A CONFLICTING SHAPE PIKUP EXISTS, DISSALLOW THE VARIABLE ASSIGNMENT
                  WRITE(OUTLYNE,*)
     1            'A CONFLICTING SURFACE SHAPE PIKUP EXISTS ON THIS SURFACE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,666) WC,INT(W2)
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'VARIABLE ASSIGNMENT WAS NOT MADE'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              ELSE
C     NO CONFLICTING PIKUP EXISTS, ASSIGN THE VARIABLE
C
                  IF(VALT.EQ.8) VARABL(INT(W1),4)=ALENS(7,VBSURF)
C
                  IF(VALT.EQ.8) VARABL(INT(W1),5)=ALENS(7,VBSURF)
                  IF(VALT.EQ.8) VARABL(INT(W1),13)=ALENS(7,VBSURF)
                  ISCOMP(INT(W1))=.TRUE.
              END IF
              RETURN
          END IF
C
C     DO CVTOR
          IF(VALT.EQ.10) THEN
              IF(ALENS(23,VBSURF).EQ.0.0D0) THEN
                  WRITE(OUTLYNE,*)
     1            'THE SPECIFIED SURFACE IS NOT TOROIDAL'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'VARIABLE ASSIGNMENT WAS NOT MADE'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C     THE
C     SURFACE IS X-TORIC WITH AN Y CURVATURE SOLVE OR THE
C     SURFACE IS Y-TORIC WITH AN X CURVATURE SOLVE
C     THEN DISALLOW VARIABLE
              IF(ALENS(23,VBSURF).EQ.1.0D0.AND.
     3        SOLVE(2,VBSURF).GT.0.0D0.OR.
     2        ALENS(23,VBSURF).EQ.2.0D0.AND.
     3        SOLVE(8,VBSURF).GT.0.0D0) THEN
                  WRITE(OUTLYNE,*)
     1            'A CONFLICTING CURVATURE SOLVE EXISTS ON THIS SURFACE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,666) WC,INT(W2)
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'VARIABLE ASSIGNMENT WAS NOT MADE'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C     PIKUP CHECK
C     IF A CV,RD,PRO OR NPRO PIKUPS EXISTS THEN
              IF(PIKUP(1,VBSURF,9).EQ.1.0D0
     1        .OR.PIKUP(1,VBSURF,10).EQ.1.0D0
     1        .OR.PIKUP(1,VBSURF,11).EQ.1.0D0
     1        .OR.PIKUP(1,VBSURF,12).EQ.1.0D0) THEN
C     A CV OR RD PIKUP EXISTS, DISSALLOW THE VARIABLE ASSIGNMENT
                  WRITE(OUTLYNE,*)
     1            'A CONFLICTING SURFACE SHAPE PIKUP EXISTS ON THIS SURFACE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,666) WC,INT(W2)
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'VARIABLE ASSIGNMENT WAS NOT MADE'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              ELSE
C     NO CONFLICTING PIKUP OR SOLVES EXIST, ASSIGN THE VARIABLE
C
C
                  IF(VALT.EQ.10) VARABL(INT(W1),4)=ALENS(24,VBSURF)
C
                  IF(VALT.EQ.10) VARABL(INT(W1),5)=ALENS(24,VBSURF)
                  IF(VALT.EQ.10) VARABL(INT(W1),13)=ALENS(24,VBSURF)
                  ISCOMP(INT(W1))=.TRUE.
              END IF
              RETURN
          END IF
C
C     DO CCTOR
          IF(VALT.EQ.11) THEN
              IF(ALENS(23,VBSURF).EQ.0.0D0) THEN
                  WRITE(OUTLYNE,*)
     1            'THE SPECIFIED SURFACE IS NOT TOROIDAL'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'VARIABLE ASSIGNMENT WAS NOT MADE'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C     PIKUP CHECK
C     IF A CCTOR,PRO OR NPRO PIKUPS EXISTS THEN
              IF(PIKUP(1,VBSURF,21).EQ.1.0D0
     1        .OR.PIKUP(1,VBSURF,11).EQ.1.0D0
     1        .OR.PIKUP(1,VBSURF,12).EQ.1.0D0) THEN
C     A CV OR RD PIKUP EXISTS, DISSALLOW THE VARIABLE ASSIGNMENT
                  WRITE(OUTLYNE,*)
     1            'A CONFLICTING SURFACE SHAPE PIKUP EXISTS ON THIS SURFACE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,666) WC,INT(W2)
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'VARIABLE ASSIGNMENT WAS NOT MADE'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              ELSE
C     NO CONFLICTING PIKUP EXISTS, ASSIGN THE VARIABLE
C
                  IF(VALT.EQ.11) VARABL(INT(W1),4)=ALENS(41,VBSURF)
C
                  IF(VALT.EQ.11) VARABL(INT(W1),5)=ALENS(41,VBSURF)
                  IF(VALT.EQ.11) VARABL(INT(W1),13)=ALENS(41,VBSURF)
                  ISCOMP(INT(W1))=.TRUE.
              END IF
              RETURN
          END IF
C
C     DO ADTOR
          IF(VALT.EQ.12) THEN
              IF(ALENS(23,VBSURF).EQ.0.0D0) THEN
                  WRITE(OUTLYNE,*)
     1            'THE SPECIFIED SURFACE IS NOT TOROIDAL'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'VARIABLE ASSIGNMENT WAS NOT MADE'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C     PIKUP CHECK
C     IF A ADT,PRO OR NPRO PIKUPS EXISTS THEN
              IF(PIKUP(1,VBSURF,22).EQ.1.0D0
     1        .OR.PIKUP(1,VBSURF,11).EQ.1.0D0
     1        .OR.PIKUP(1,VBSURF,12).EQ.1.0D0) THEN
C     A CV OR RD PIKUP EXISTS, DISSALLOW THE VARIABLE ASSIGNMENT
                  WRITE(OUTLYNE,*)
     1            'A CONFLICTING SURFACE SHAPE PIKUP EXISTS ON THIS SURFACE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,666) WC,INT(W2)
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'VARIABLE ASSIGNMENT WAS NOT MADE'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              ELSE
C     NO CONFLICTING PIKUP EXISTS, ASSIGN THE VARIABLE
C
                  IF(VALT.EQ.12) VARABL(INT(W1),4)=ALENS(37,VBSURF)
C
                  IF(VALT.EQ.12) VARABL(INT(W1),5)=ALENS(37,VBSURF)
                  IF(VALT.EQ.12) VARABL(INT(W1),13)=ALENS(37,VBSURF)
                  ISCOMP(INT(W1))=.TRUE.
              END IF
              RETURN
          END IF
C
C     DO AETOR
          IF(VALT.EQ.13) THEN
              IF(ALENS(23,VBSURF).EQ.0.0D0) THEN
                  WRITE(OUTLYNE,*)
     1            'THE SPECIFIED SURFACE IS NOT TOROIDAL'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'VARIABLE ASSIGNMENT WAS NOT MADE'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C     PIKUP CHECK
C     IF A AETOR,PRO OR NPRO PIKUPS EXISTS THEN
              IF(PIKUP(1,VBSURF,23).EQ.1.0D0
     1        .OR.PIKUP(1,VBSURF,11).EQ.1.0D0
     1        .OR.PIKUP(1,VBSURF,12).EQ.1.0D0) THEN
C     A CV OR RD PIKUP EXISTS, DISSALLOW THE VARIABLE ASSIGNMENT
                  WRITE(OUTLYNE,*)
     1            'A CONFLICTING SURFACE SHAPE PIKUP EXISTS ON THIS SURFACE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,666) WC,INT(W2)
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'VARIABLE ASSIGNMENT WAS NOT MADE'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              ELSE
C     NO CONFLICTING PIKUP EXISTS, ASSIGN THE VARIABLE
C
                  IF(VALT.EQ.13) VARABL(INT(W1),4)=ALENS(38,VBSURF)
C
                  IF(VALT.EQ.13) VARABL(INT(W1),5)=ALENS(38,VBSURF)
                  IF(VALT.EQ.13) VARABL(INT(W1),13)=ALENS(38,VBSURF)
                  ISCOMP(INT(W1))=.TRUE.
              END IF
              RETURN
          END IF
C
C     DO AFTOR
          IF(VALT.EQ.14) THEN
              IF(ALENS(23,VBSURF).EQ.0.0D0) THEN
                  WRITE(OUTLYNE,*)
     1            'THE SPECIFIED SURFACE IS NOT TOROIDAL'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'VARIABLE ASSIGNMENT WAS NOT MADE'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C     PIKUP CHECK
C     IF A AFTOR,PRO OR NPRO PIKUPS EXISTS THEN
              IF(PIKUP(1,VBSURF,24).EQ.1.0D0
     1        .OR.PIKUP(1,VBSURF,11).EQ.1.0D0
     1        .OR.PIKUP(1,VBSURF,12).EQ.1.0D0) THEN
C     A CV OR RD PIKUP EXISTS, DISSALLOW THE VARIABLE ASSIGNMENT
                  WRITE(OUTLYNE,*)
     1            'A CONFLICTING SURFACE SHAPE PIKUP EXISTS ON THIS SURFACE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,666) WC,INT(W2)
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'VARIABLE ASSIGNMENT WAS NOT MADE'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              ELSE
C     NO CONFLICTING PIKUP EXISTS, ASSIGN THE VARIABLE
C
                  IF(VALT.EQ.14) VARABL(INT(W1),4)=ALENS(39,VBSURF)
C
                  IF(VALT.EQ.14) VARABL(INT(W1),5)=ALENS(39,VBSURF)
                  IF(VALT.EQ.14) VARABL(INT(W1),13)=ALENS(39,VBSURF)
                  ISCOMP(INT(W1))=.TRUE.
              END IF
              RETURN
          END IF
C
C     DO AGTOR
          IF(VALT.EQ.15) THEN
              IF(ALENS(23,VBSURF).EQ.0.0D0) THEN
                  WRITE(OUTLYNE,*)
     1            'THE SPECIFIED SURFACE IS NOT TOROIDAL'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'VARIABLE ASSIGNMENT WAS NOT MADE'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C     PIKUP CHECK
C     IF A AGTOR,PRO OR NPRO PIKUPS EXISTS THEN
              IF(PIKUP(1,VBSURF,25).EQ.1.0D0
     1        .OR.PIKUP(1,VBSURF,11).EQ.1.0D0
     1        .OR.PIKUP(1,VBSURF,12).EQ.1.0D0) THEN
C     A CV OR RD PIKUP EXISTS, DISSALLOW THE VARIABLE ASSIGNMENT
                  WRITE(OUTLYNE,*)
     1            'A CONFLICTING SURFACE SHAPE PIKUP EXISTS ON THIS SURFACE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,666) WC,INT(W2)
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'VARIABLE ASSIGNMENT WAS NOT MADE'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              ELSE
C     NO CONFLICTING PIKUP EXISTS, ASSIGN THE VARIABLE
C
                  IF(VALT.EQ.15) VARABL(INT(W1),4)=ALENS(40,VBSURF)
                  IF(VALT.EQ.15) VARABL(INT(W1),5)=ALENS(40,VBSURF)
                  IF(VALT.EQ.15) VARABL(INT(W1),13)=ALENS(40,VBSURF)
                  ISCOMP(INT(W1))=.TRUE.
              END IF
              RETURN
          END IF
C
C     DO ALPHA
          IF(VALT.EQ.16) THEN
              IF(ALENS(25,VBSURF).EQ.0.0D0) THEN
                  WRITE(OUTLYNE,*)
     1            'THE SPECIFIED SURFACE IS NOT TILTED'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'VARIABLE ASSIGNMENT WAS NOT MADE'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(ALENS(25,VBSURF).EQ.2.0D0.OR.ALENS(25,VBSURF).EQ.3.0D0) THEN
                  WRITE(OUTLYNE,*)
     1            'THE SPECIFIED SURFACE HAS AN AUTOMATIC TILT DEFINITION'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'VARIABLE ASSIGNMENT WAS NOT MADE'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C     PIKUP CHECK
C     IF AN ALPHA PIKUPS EXISTS THEN
              IF(PIKUP(1,VBSURF,15).EQ.1.0D0) THEN
C     A CONFLICTING PIKUP EXISTS, DISSALLOW THE VARIABLE ASSIGNMENT
                  WRITE(OUTLYNE,*)
     1            'A CONFLICTING ALPHA PIKUP EXISTS ON THIS SURFACE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,666) WC,INT(W2)
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'VARIABLE ASSIGNMENT WAS NOT MADE'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              ELSE
C     NO CONFLICTING PIKUP EXISTS, ASSIGN THE VARIABLE
C
                  IF(VALT.EQ.16) VARABL(INT(W1),4)=ALENS(118,VBSURF)
C
                  IF(VALT.EQ.16) VARABL(INT(W1),5)=ALENS(118,VBSURF)
                  IF(VALT.EQ.16) VARABL(INT(W1),13)=ALENS(118,VBSURF)
                  ISCOMP(INT(W1))=.TRUE.
              END IF
              RETURN
          END IF
C
C     DO BETA
          IF(VALT.EQ.17) THEN
              IF(ALENS(25,VBSURF).EQ.0.0D0) THEN
                  WRITE(OUTLYNE,*)
     1            'THE SPECIFIED SURFACE IS NOT TILTED'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'VARIABLE ASSIGNMENT WAS NOT MADE'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(ALENS(25,VBSURF).EQ.2.0D0.OR.ALENS(25,VBSURF).EQ.3.0D0) THEN
                  WRITE(OUTLYNE,*)
     1            'THE SPECIFIED SURFACE HAS AN AUTOMATIC TILT DEFINITION'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'VARIABLE ASSIGNMENT WAS NOT MADE'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C     PIKUP CHECK
C     IF AN BETA PIKUPS EXISTS THEN
              IF(PIKUP(1,VBSURF,16).EQ.1.0D0) THEN
C     A CONFLICTING PIKUP EXISTS, DISSALLOW THE VARIABLE ASSIGNMENT
                  WRITE(OUTLYNE,*)
     1            'A CONFLICTING BETA PIKUP EXISTS ON THIS SURFACE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,666) WC,INT(W2)
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'VARIABLE ASSIGNMENT WAS NOT MADE'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              ELSE
C     NO CONFLICTING PIKUP EXISTS, ASSIGN THE VARIABLE
C
                  IF(VALT.EQ.17) VARABL(INT(W1),4)=ALENS(119,VBSURF)
C
                  IF(VALT.EQ.17) VARABL(INT(W1),5)=ALENS(119,VBSURF)
                  IF(VALT.EQ.17) VARABL(INT(W1),13)=ALENS(119,VBSURF)
                  ISCOMP(INT(W1))=.TRUE.
              END IF
              RETURN
          END IF
C
C     DO GAMMA
          IF(VALT.EQ.18) THEN
              IF(ALENS(25,VBSURF).EQ.0.0D0) THEN
                  WRITE(OUTLYNE,*)
     1            'THE SPECIFIED SURFACE IS NOT TILTED'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'VARIABLE ASSIGNMENT WAS NOT MADE'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(ALENS(25,VBSURF).EQ.2.0D0.OR.ALENS(25,VBSURF).EQ.3.0D0) THEN
                  WRITE(OUTLYNE,*)
     1            'THE SPECIFIED SURFACE HAS AN AUTOMATIC TILT DEFINITION'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'VARIABLE ASSIGNMENT WAS NOT MADE'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C     PIKUP CHECK
C     IF AN GAMMA PIKUPS EXISTS THEN
              IF(PIKUP(1,VBSURF,17).EQ.1.0D0) THEN
C     A CONFLICTING PIKUP EXISTS, DISSALLOW THE VARIABLE ASSIGNMENT
                  WRITE(OUTLYNE,*)
     1            'A CONFLICTING GAMMA PIKUP EXISTS ON THIS SURFACE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,666) WC,INT(W2)
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'VARIABLE ASSIGNMENT WAS NOT MADE'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              ELSE
C     NO CONFLICTING PIKUP EXISTS, ASSIGN THE VARIABLE
C
                  IF(VALT.EQ.18) VARABL(INT(W1),4)=ALENS(120,VBSURF)
C
                  IF(VALT.EQ.18) VARABL(INT(W1),5)=ALENS(120,VBSURF)
                  IF(VALT.EQ.18) VARABL(INT(W1),13)=ALENS(120,VBSURF)
                  ISCOMP(INT(W1))=.TRUE.
              END IF
              RETURN
          END IF
C
C     DO XD
          IF(VALT.EQ.19) THEN
C     PIKUP CHECK
C     IF AN XD PIKUPS EXISTS THEN
              IF(ALENS(25,VBSURF).EQ.2.0D0.OR.ALENS(25,VBSURF).EQ.3.0D0) THEN
                  WRITE(OUTLYNE,*)
     1            'THE SPECIFIED SURFACE HAS AN AUTOMATIC TILT DEFINITION'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'VARIABLE ASSIGNMENT WAS NOT MADE'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(PIKUP(1,VBSURF,14).EQ.1.0D0) THEN
C     A CONFLICTING PIKUP EXISTS, DISSALLOW THE VARIABLE ASSIGNMENT
                  WRITE(OUTLYNE,*)
     1            'A CONFLICTING XD PIKUP EXISTS ON THIS SURFACE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,666) WC,INT(W2)
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'VARIABLE ASSIGNMENT WAS NOT MADE'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              ELSE
C     NO CONFLICTING PIKUP EXISTS, ASSIGN THE VARIABLE
C
                  IF(VALT.EQ.19) VARABL(INT(W1),4)=ALENS(114,VBSURF)
C
                  IF(VALT.EQ.19) VARABL(INT(W1),5)=ALENS(114,VBSURF)
                  IF(VALT.EQ.19) VARABL(INT(W1),13)=ALENS(114,VBSURF)
                  ISCOMP(INT(W1))=.TRUE.
              END IF
              RETURN
          END IF
C     DO CLPX
          IF(VALT.EQ.141) THEN
              IF(ALENS(9,VBSURF).EQ.0.0D0) THEN
                  WRITE(OUTLYNE,*)
     1            'THE SPECIFIED SURFACE HAS NO CLEAR APERTURE ASSIGNED'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'VARIABLE ASSIGNMENT WAS NOT MADE'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(ALENS(9,VBSURF).EQ.1.0D0) THEN
                  WRITE(OUTLYNE,*)
     1            'THE SPECIFIED SURFACE HAS A CIRCULAR CLEAR APERTURE ASSIGNED'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'"CLPX" VARIABLE ASSIGNMENT WAS NOT MADE'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C     PIKUP CHECK
C     IF A CLAP PIKUPS EXISTS THEN
              IF(PIKUP(1,VBSURF,18).EQ.1.0D0) THEN
C     A CONFLICTING PIKUP EXISTS, DISSALLOW THE VARIABLE ASSIGNMENT
                  WRITE(OUTLYNE,*)
     1            'A CONFLICTING CLAP PIKUP EXISTS ON THIS SURFACE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,666) WC,INT(W1),VBCFG
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'VARIABLE ASSIGNMENT WAS NOT MADE'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              ELSE
C     NO CONFLICTING PIKUP EXISTS, ASSIGN THE VARIABLE
C
                  IF(VALT.EQ.141) VARABL(INT(W1),4)=ALENS(11,VBSURF)
                  IF(VALT.EQ.141) VARABL(INT(W1),5)=ALENS(11,VBSURF)
                  IF(VALT.EQ.141) VARABL(INT(W1),13)=ALENS(11,VBSURF)
                  ISCOMP(INT(W1))=.TRUE.
              END IF
              RETURN
          END IF
C     DO CLPY
          IF(VALT.EQ.142) THEN
              IF(ALENS(9,VBSURF).EQ.0.0D0) THEN
                  WRITE(OUTLYNE,*)
     1            'THE SPECIFIED SURFACE HAS NO CLEAR APERTURE ASSIGNED'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'VARIABLE ASSIGNMENT WAS NOT MADE'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C     PIKUP CHECK
C     IF A CLAP PIKUPS EXISTS THEN
              IF(PIKUP(1,VBSURF,18).EQ.1.0D0) THEN
C     A CONFLICTING PIKUP EXISTS, DISSALLOW THE VARIABLE ASSIGNMENT
                  WRITE(OUTLYNE,*)
     1            'A CONFLICTING CLAP PIKUP EXISTS ON THIS SURFACE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,666) WC,INT(W1),VBCFG
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'VARIABLE ASSIGNMENT WAS NOT MADE'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              ELSE
C     NO CONFLICTING PIKUP EXISTS, ASSIGN THE VARIABLE
C
                  IF(VALT.EQ.142) VARABL(INT(W1),4)=ALENS(10,VBSURF)
                  IF(VALT.EQ.142) VARABL(INT(W1),5)=ALENS(10,VBSURF)
                  IF(VALT.EQ.142) VARABL(INT(W1),13)=ALENS(10,VBSURF)
                  ISCOMP(INT(W1))=.TRUE.
              END IF
              RETURN
          END IF
C
C     DO GDX
          IF(VALT.EQ.143) THEN
C     PIKUP CHECK
C     IF AN GDX PIKUPS EXISTS THEN
              IF(PIKUP(1,VBSURF,37).EQ.1.0D0) THEN
C     A CONFLICTING PIKUP EXISTS, DISSALLOW THE VARIABLE ASSIGNMENT
                  WRITE(OUTLYNE,*)
     1            'A CONFLICTING GDX PIKUP EXISTS ON THIS SURFACE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,666) WC,INT(W2)
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'VARIABLE ASSIGNMENT WAS NOT MADE'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              ELSE
C     NO CONFLICTING PIKUP EXISTS, ASSIGN THE VARIABLE
C
                  IF(VALT.EQ.143) VARABL(INT(W1),4)=ALENS(90,VBSURF)
                  IF(VALT.EQ.143) VARABL(INT(W1),5)=ALENS(90,VBSURF)
                  IF(VALT.EQ.143) VARABL(INT(W1),13)=ALENS(90,VBSURF)
                  ISCOMP(INT(W1))=.TRUE.
              END IF
              RETURN
          END IF
C
C     DO GDY
          IF(VALT.EQ.144) THEN
C     PIKUP CHECK
C     IF AN GDY PIKUPS EXISTS THEN
              IF(PIKUP(1,VBSURF,38).EQ.1.0D0) THEN
C     A CONFLICTING PIKUP EXISTS, DISSALLOW THE VARIABLE ASSIGNMENT
                  WRITE(OUTLYNE,*)
     1            'A CONFLICTING GDY PIKUP EXISTS ON THIS SURFACE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,666) WC,INT(W2)
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'VARIABLE ASSIGNMENT WAS NOT MADE'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              ELSE
C     NO CONFLICTING PIKUP EXISTS, ASSIGN THE VARIABLE
C
                  IF(VALT.EQ.144) VARABL(INT(W1),4)=ALENS(91,VBSURF)
                  IF(VALT.EQ.144) VARABL(INT(W1),5)=ALENS(91,VBSURF)
                  IF(VALT.EQ.144) VARABL(INT(W1),13)=ALENS(91,VBSURF)
                  ISCOMP(INT(W1))=.TRUE.
              END IF
              RETURN
          END IF
C
C     DO GDZ
          IF(VALT.EQ.145) THEN
C     PIKUP CHECK
C     IF AN GDZ PIKUPS EXISTS THEN
              IF(PIKUP(1,VBSURF,39).EQ.1.0D0) THEN
C     A CONFLICTING PIKUP EXISTS, DISSALLOW THE VARIABLE ASSIGNMENT
                  WRITE(OUTLYNE,*)
     1            'A CONFLICTING GDY PIKUP EXISTS ON THIS SURFACE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,666) WC,INT(W2)
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'VARIABLE ASSIGNMENT WAS NOT MADE'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              ELSE
C     NO CONFLICTING PIKUP EXISTS, ASSIGN THE VARIABLE
C
                  IF(VALT.EQ.145) VARABL(INT(W1),4)=ALENS(92,VBSURF)
                  IF(VALT.EQ.145) VARABL(INT(W1),5)=ALENS(92,VBSURF)
                  IF(VALT.EQ.145) VARABL(INT(W1),13)=ALENS(92,VBSURF)
                  ISCOMP(INT(W1))=.TRUE.
              END IF
              RETURN
          END IF
C
C     DO GALPHA
          IF(VALT.EQ.146) THEN
C     PIKUP CHECK
C     IF AN GALPHA PIKUPS EXISTS THEN
              IF(PIKUP(1,VBSURF,40).EQ.1.0D0) THEN
C     A CONFLICTING PIKUP EXISTS, DISSALLOW THE VARIABLE ASSIGNMENT
                  WRITE(OUTLYNE,*)
     1            'A CONFLICTING GALPHA PIKUP EXISTS ON THIS SURFACE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,666) WC,INT(W2)
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'VARIABLE ASSIGNMENT WAS NOT MADE'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              ELSE
C     NO CONFLICTING PIKUP EXISTS, ASSIGN THE VARIABLE
C
                  IF(VALT.EQ.146) VARABL(INT(W1),4)=ALENS(93,VBSURF)
                  IF(VALT.EQ.146) VARABL(INT(W1),5)=ALENS(93,VBSURF)
                  IF(VALT.EQ.146) VARABL(INT(W1),13)=ALENS(93,VBSURF)
                  ISCOMP(INT(W1))=.TRUE.
              END IF
              RETURN
          END IF
C
C     DO GBETA
          IF(VALT.EQ.147) THEN
C     PIKUP CHECK
C     IF AN GBETA PIKUPS EXISTS THEN
              IF(PIKUP(1,VBSURF,41).EQ.1.0D0) THEN
C     A CONFLICTING PIKUP EXISTS, DISSALLOW THE VARIABLE ASSIGNMENT
                  WRITE(OUTLYNE,*)
     1            'A CONFLICTING GBETA PIKUP EXISTS ON THIS SURFACE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,666) WC,INT(W2)
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'VARIABLE ASSIGNMENT WAS NOT MADE'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              ELSE
C     NO CONFLICTING PIKUP EXISTS, ASSIGN THE VARIABLE
C
                  IF(VALT.EQ.147) VARABL(INT(W1),4)=ALENS(94,VBSURF)
                  IF(VALT.EQ.147) VARABL(INT(W1),5)=ALENS(94,VBSURF)
                  IF(VALT.EQ.147) VARABL(INT(W1),13)=ALENS(94,VBSURF)
                  ISCOMP(INT(W1))=.TRUE.
              END IF
              RETURN
          END IF
C
C     DO GGAMMA
          IF(VALT.EQ.148) THEN
C     PIKUP CHECK
C     IF AN GGAMMA PIKUPS EXISTS THEN
              IF(PIKUP(1,VBSURF,42).EQ.1.0D0) THEN
C     A CONFLICTING PIKUP EXISTS, DISSALLOW THE VARIABLE ASSIGNMENT
                  WRITE(OUTLYNE,*)
     1            'A CONFLICTING GGAMMA PIKUP EXISTS ON THIS SURFACE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,666) WC,INT(W2)
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'VARIABLE ASSIGNMENT WAS NOT MADE'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              ELSE
C     NO CONFLICTING PIKUP EXISTS, ASSIGN THE VARIABLE
C
                  IF(VALT.EQ.148) VARABL(INT(W1),4)=ALENS(95,VBSURF)
                  IF(VALT.EQ.148) VARABL(INT(W1),5)=ALENS(95,VBSURF)
                  IF(VALT.EQ.148) VARABL(INT(W1),13)=ALENS(95,VBSURF)
                  ISCOMP(INT(W1))=.TRUE.
              END IF
              RETURN
          END IF
C     DO GRT
          IF(VALT.EQ.149) THEN
C     PIKUP CHECK
C     IF AN GRT PIKUPS EXISTS THEN
              IF(ALENS(96,VBSURF).EQ.1.0D0) THEN
              ELSE
                  WRITE(OUTLYNE,*)
     1            'THE SPECIFIED SURFACE REQUIRES A "GRT" DEFINITION'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'VARIABLE ASSIGNMENT WAS NOT MADE'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(PIKUP(1,VBSURF,43).EQ.1.0D0) THEN
C     A CONFLICTING PIKUP EXISTS, DISSALLOW THE VARIABLE ASSIGNMENT
                  WRITE(OUTLYNE,*)
     1            'A CONFLICTING GRT PIKUP EXISTS ON THIS SURFACE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,666) WC,INT(W2)
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'VARIABLE ASSIGNMENT WAS NOT MADE'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              ELSE
C     NO CONFLICTING PIKUP EXISTS, ASSIGN THE VARIABLE
C
                  IF(VALT.EQ.149) VARABL(INT(W1),4)=ALENS(98,VBSURF)
                  IF(VALT.EQ.149) VARABL(INT(W1),5)=ALENS(98,VBSURF)
                  IF(VALT.EQ.149) VARABL(INT(W1),13)=ALENS(98,VBSURF)
                  ISCOMP(INT(W1))=.TRUE.
              END IF
              RETURN
          END IF
C     DO PIVX
          IF(VALT.EQ.137) THEN
C     PIKUP CHECK
C     IF AN PIVX PIKUPS EXISTS THEN
              IF(ALENS(25,VBSURF).EQ.2.0D0.OR.ALENS(25,VBSURF).EQ.3.0D0) THEN
                  WRITE(OUTLYNE,*)
     1            'THE SPECIFIED SURFACE HAS AN AUTOMATIC TILT DEFINITION'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'VARIABLE ASSIGNMENT WAS NOT MADE'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(PIKUP(1,VBSURF,34).EQ.1.0D0) THEN
C     A CONFLICTING PIKUP EXISTS, DISSALLOW THE VARIABLE ASSIGNMENT
                  WRITE(OUTLYNE,*)
     1            'A CONFLICTING PIVX PIKUP EXISTS ON THIS SURFACE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,666) WC,INT(W2)
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'VARIABLE ASSIGNMENT WAS NOT MADE'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              ELSE
C     NO CONFLICTING PIKUP EXISTS, ASSIGN THE VARIABLE
C
                  IF(VALT.EQ.137) VARABL(INT(W1),4)=ALENS(78,VBSURF)
C
                  IF(VALT.EQ.137) VARABL(INT(W1),5)=ALENS(78,VBSURF)
                  IF(VALT.EQ.137) VARABL(INT(W1),13)=ALENS(78,VBSURF)
                  ISCOMP(INT(W1))=.TRUE.
              END IF
              RETURN
          END IF
C     DO PIVY
          IF(VALT.EQ.138) THEN
C     PIKUP CHECK
C     IF AN PIVY PIKUPS EXISTS THEN
              IF(ALENS(25,VBSURF).EQ.2.0D0.OR.ALENS(25,VBSURF).EQ.3.0D0) THEN
                  WRITE(OUTLYNE,*)
     1            'THE SPECIFIED SURFACE HAS AN AUTOMATIC TILT DEFINITION'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'VARIABLE ASSIGNMENT WAS NOT MADE'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(PIKUP(1,VBSURF,35).EQ.1.0D0) THEN
C     A CONFLICTING PIKUP EXISTS, DISSALLOW THE VARIABLE ASSIGNMENT
                  WRITE(OUTLYNE,*)
     1            'A CONFLICTING PIVY PIKUP EXISTS ON THIS SURFACE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,666) WC,INT(W2)
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'VARIABLE ASSIGNMENT WAS NOT MADE'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              ELSE
C     NO CONFLICTING PIKUP EXISTS, ASSIGN THE VARIABLE
C
                  IF(VALT.EQ.138) VARABL(INT(W1),4)=ALENS(79,VBSURF)
C
                  IF(VALT.EQ.138) VARABL(INT(W1),5)=ALENS(79,VBSURF)
                  IF(VALT.EQ.138) VARABL(INT(W1),13)=ALENS(79,VBSURF)
                  ISCOMP(INT(W1))=.TRUE.
              END IF
              RETURN
          END IF
C     DO PIVZ
          IF(VALT.EQ.139) THEN
C     PIKUP CHECK
C     IF AN PIVZ PIKUPS EXISTS THEN
              IF(ALENS(25,VBSURF).EQ.2.0D0.OR.ALENS(25,VBSURF).EQ.3.0D0) THEN
                  WRITE(OUTLYNE,*)
     1            'THE SPECIFIED SURFACE HAS AN AUTOMATIC TILT DEFINITION'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'VARIABLE ASSIGNMENT WAS NOT MADE'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(PIKUP(1,VBSURF,36).EQ.1.0D0) THEN
C     A CONFLICTING PIKUP EXISTS, DISSALLOW THE VARIABLE ASSIGNMENT
                  WRITE(OUTLYNE,*)
     1            'A CONFLICTING PIVZ PIKUP EXISTS ON THIS SURFACE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,666) WC,INT(W2)
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'VARIABLE ASSIGNMENT WAS NOT MADE'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              ELSE
C     NO CONFLICTING PIKUP EXISTS, ASSIGN THE VARIABLE
C
                  IF(VALT.EQ.139) VARABL(INT(W1),4)=ALENS(80,VBSURF)
C
                  IF(VALT.EQ.139) VARABL(INT(W1),5)=ALENS(80,VBSURF)
                  IF(VALT.EQ.139) VARABL(INT(W1),13)=ALENS(80,VBSURF)
                  ISCOMP(INT(W1))=.TRUE.
              END IF
              RETURN
          END IF
C
C     DO YD
          IF(VALT.EQ.20) THEN
              IF(ALENS(25,VBSURF).EQ.2.0D0.OR.ALENS(25,VBSURF).EQ.3.0D0) THEN
                  WRITE(OUTLYNE,*)
     1            'THE SPECIFIED SURFACE HAS AN AUTOMATIC TILT DEFINITION'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'VARIABLE ASSIGNMENT WAS NOT MADE'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C     PIKUP CHECK
C     IF AN YD PIKUPS EXISTS THEN
              IF(PIKUP(1,VBSURF,13).EQ.1.0D0) THEN
C     A CONFLICTING PIKUP EXISTS, DISSALLOW THE VARIABLE ASSIGNMENT
                  WRITE(OUTLYNE,*)
     1            'A CONFLICTING YD PIKUP EXISTS ON THIS SURFACE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,666) WC,INT(W2)
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'VARIABLE ASSIGNMENT WAS NOT MADE'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              ELSE
C     NO CONFLICTING PIKUP EXISTS, ASSIGN THE VARIABLE
C
                  IF(VALT.EQ.20) VARABL(INT(W1),4)=ALENS(115,VBSURF)
C
                  IF(VALT.EQ.20) VARABL(INT(W1),5)=ALENS(115,VBSURF)
                  IF(VALT.EQ.20) VARABL(INT(W1),13)=ALENS(115,VBSURF)
                  ISCOMP(INT(W1))=.TRUE.
              END IF
              RETURN
          END IF
C
C     DO ZD
          IF(VALT.EQ.134) THEN
              IF(ALENS(25,VBSURF).EQ.2.0D0.OR.ALENS(25,VBSURF).EQ.3.0D0) THEN
                  WRITE(OUTLYNE,*)
     1            'THE SPECIFIED SURFACE HAS AN AUTOMATIC TILT DEFINITION'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'VARIABLE ASSIGNMENT WAS NOT MADE'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C     PIKUP CHECK
C     IF AN YD PIKUPS EXISTS THEN
              IF(PIKUP(1,VBSURF,33).EQ.1.0D0) THEN
C     A CONFLICTING PIKUP EXISTS, DISSALLOW THE VARIABLE ASSIGNMENT
                  WRITE(OUTLYNE,*)
     1            'A CONFLICTING ZD PIKUP EXISTS ON THIS SURFACE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,666) WC,INT(W2)
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'VARIABLE ASSIGNMENT WAS NOT MADE'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              ELSE
C     NO CONFLICTING PIKUP EXISTS, ASSIGN THE VARIABLE
C
                  IF(VALT.EQ.134) VARABL(INT(W1),4)=ALENS(116,VBSURF)
C
                  IF(VALT.EQ.134) VARABL(INT(W1),5)=ALENS(116,VBSURF)
                  IF(VALT.EQ.134) VARABL(INT(W1),13)=ALENS(116,VBSURF)
                  ISCOMP(INT(W1))=.TRUE.
              END IF
              RETURN
          END IF
C
C     DO N1
          IF(VALT.EQ.21) THEN
              IF(GLANAM(VBSURF,1)(1:5).NE.'GLASS') THEN
                  WRITE(OUTLYNE,*)
     1            'GLASS MUST BE CONVERTED FROM A CATALOG GLASS'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)
     1            'TO A "GLASS" TYPE OF ENTRY BEFORE INDEX CAN BE A VARIABLE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*) 'NO-ACTION TAKEN'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C     PIKUP CHECK
C     IF AN PIKUP GLASS EXISTS THEN
              IF(PIKUP(1,VBSURF,20).EQ.1.0D0) THEN
C     A CONFLICTING PIKUP EXISTS, DISSALLOW THE VARIABLE ASSIGNMENT
                  WRITE(OUTLYNE,*)
     1            'A CONFLICTING GLASS PIKUP EXISTS ON THIS SURFACE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,666) WC,INT(W2)
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'VARIABLE ASSIGNMENT WAS NOT MADE'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              ELSE
C     NO CONFLICTING PIKUP EXISTS, ASSIGN THE VARIABLE
C
                  IF(VALT.EQ.21) VARABL(INT(W1),4)=ALENS(46,VBSURF)
C
                  IF(VALT.EQ.21) VARABL(INT(W1),5)=ALENS(46,VBSURF)
                  IF(VALT.EQ.21) VARABL(INT(W1),13)=ALENS(46,VBSURF)
                  ISCOMP(INT(W1))=.TRUE.
              END IF
              RETURN
          END IF
C
C     DO N2
          IF(VALT.EQ.22) THEN
              IF(GLANAM(VBSURF,1)(1:5).NE.'GLASS') THEN
                  WRITE(OUTLYNE,*)
     1            'GLASS MUST BE CONVERTED FROM A CATALOG GLASS'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)
     1            'TO A "GLASS" TYPE OF ENTRY BEFORE INDEX CAN BE A VARIABLE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*) 'NO-ACTION TAKEN'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C     PIKUP CHECK
C     IF AN PIKUP GLASS EXISTS THEN
              IF(PIKUP(1,VBSURF,20).EQ.1.0D0) THEN
C     A CONFLICTING PIKUP EXISTS, DISSALLOW THE VARIABLE ASSIGNMENT
                  WRITE(OUTLYNE,*)
     1            'A CONFLICTING GLASS PIKUP EXISTS ON THIS SURFACE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,666) WC,INT(W2)
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'VARIABLE ASSIGNMENT WAS NOT MADE'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              ELSE
C     NO CONFLICTING PIKUP EXISTS, ASSIGN THE VARIABLE
C
                  IF(VALT.EQ.22) VARABL(INT(W1),4)=ALENS(47,VBSURF)
C
                  IF(VALT.EQ.22) VARABL(INT(W1),5)=ALENS(47,VBSURF)
                  IF(VALT.EQ.22) VARABL(INT(W1),13)=ALENS(47,VBSURF)
                  ISCOMP(INT(W1))=.TRUE.
              END IF
              RETURN
          END IF
C
C     DO N3
          IF(VALT.EQ.23) THEN
              IF(GLANAM(VBSURF,1)(1:5).NE.'GLASS') THEN
                  WRITE(OUTLYNE,*)
     1            'GLASS MUST BE CONVERTED FROM A CATALOG GLASS'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)
     1            'TO A "GLASS" TYPE OF ENTRY BEFORE INDEX CAN BE A VARIABLE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*) 'NO-ACTION TAKEN'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C     PIKUP CHECK
C     IF AN PIKUP GLASS EXISTS THEN
              IF(PIKUP(1,VBSURF,20).EQ.1.0D0) THEN
C     A CONFLICTING PIKUP EXISTS, DISSALLOW THE VARIABLE ASSIGNMENT
                  WRITE(OUTLYNE,*)
     1            'A CONFLICTING GLASS PIKUP EXISTS ON THIS SURFACE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,666) WC,INT(W2)
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'VARIABLE ASSIGNMENT WAS NOT MADE'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              ELSE
C     NO CONFLICTING PIKUP EXISTS, ASSIGN THE VARIABLE
C
                  IF(VALT.EQ.23) VARABL(INT(W1),4)=ALENS(48,VBSURF)
C
                  IF(VALT.EQ.23) VARABL(INT(W1),5)=ALENS(48,VBSURF)
                  IF(VALT.EQ.23) VARABL(INT(W1),13)=ALENS(48,VBSURF)
                  ISCOMP(INT(W1))=.TRUE.
              END IF
              RETURN
          END IF
C
C     DO N4
          IF(VALT.EQ.24) THEN
              IF(GLANAM(VBSURF,1)(1:5).NE.'GLASS') THEN
                  WRITE(OUTLYNE,*)
     1            'GLASS MUST BE CONVERTED FROM A CATALOG GLASS'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)
     1            'TO A "GLASS" TYPE OF ENTRY BEFORE INDEX CAN BE A VARIABLE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*) 'NO-ACTION TAKEN'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C     PIKUP CHECK
C     IF AN PIKUP GLASS EXISTS THEN
              IF(PIKUP(1,VBSURF,20).EQ.1.0D0) THEN
C     A CONFLICTING PIKUP EXISTS, DISSALLOW THE VARIABLE ASSIGNMENT
                  WRITE(OUTLYNE,*)
     1            'A CONFLICTING GLASS PIKUP EXISTS ON THIS SURFACE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,666) WC,INT(W2)
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'VARIABLE ASSIGNMENT WAS NOT MADE'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              ELSE
C     NO CONFLICTING PIKUP EXISTS, ASSIGN THE VARIABLE
C
                  IF(VALT.EQ.24) VARABL(INT(W1),4)=ALENS(49,VBSURF)
C
                  IF(VALT.EQ.24) VARABL(INT(W1),5)=ALENS(49,VBSURF)
                  IF(VALT.EQ.24) VARABL(INT(W1),13)=ALENS(49,VBSURF)
                  ISCOMP(INT(W1))=.TRUE.
              END IF
              RETURN
          END IF
C
C     DO N5
          IF(VALT.EQ.25) THEN
              IF(GLANAM(VBSURF,1)(1:5).NE.'GLASS') THEN
                  WRITE(OUTLYNE,*)
     1            'GLASS MUST BE CONVERTED FROM A CATALOG GLASS'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)
     1            'TO A "GLASS" TYPE OF ENTRY BEFORE INDEX CAN BE A VARIABLE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*) 'NO-ACTION TAKEN'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C     PIKUP CHECK
C     IF AN PIKUP GLASS EXISTS THEN
              IF(PIKUP(1,VBSURF,20).EQ.1.0D0) THEN
C     A CONFLICTING PIKUP EXISTS, DISSALLOW THE VARIABLE ASSIGNMENT
                  WRITE(OUTLYNE,*)
     1            'A CONFLICTING GLASS PIKUP EXISTS ON THIS SURFACE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,666) WC,INT(W2)
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'VARIABLE ASSIGNMENT WAS NOT MADE'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              ELSE
C     NO CONFLICTING PIKUP EXISTS, ASSIGN THE VARIABLE
C
                  IF(VALT.EQ.25) VARABL(INT(W1),4)=ALENS(50,VBSURF)
C
                  IF(VALT.EQ.25) VARABL(INT(W1),5)=ALENS(50,VBSURF)
                  IF(VALT.EQ.25) VARABL(INT(W1),13)=ALENS(50,VBSURF)
                  ISCOMP(INT(W1))=.TRUE.
              END IF
              RETURN
          END IF
C
C     DO N6
          IF(VALT.EQ.124) THEN
              IF(GLANAM(VBSURF,1)(1:5).NE.'GLASS') THEN
                  WRITE(OUTLYNE,*)
     1            'GLASS MUST BE CONVERTED FROM A CATALOG GLASS'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)
     1            'TO A "GLASS" TYPE OF ENTRY BEFORE INDEX CAN BE A VARIABLE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*) 'NO-ACTION TAKEN'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C     PIKUP CHECK
C     IF AN PIKUP GLASS EXISTS THEN
              IF(PIKUP(1,VBSURF,20).EQ.1.0D0) THEN
C     A CONFLICTING PIKUP EXISTS, DISSALLOW THE VARIABLE ASSIGNMENT
                  WRITE(OUTLYNE,*)
     1            'A CONFLICTING GLASS PIKUP EXISTS ON THIS SURFACE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,666) WC,INT(W2)
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'VARIABLE ASSIGNMENT WAS NOT MADE'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              ELSE
C     NO CONFLICTING PIKUP EXISTS, ASSIGN THE VARIABLE
C
                  IF(VALT.EQ.124) VARABL(INT(W1),4)=ALENS(71,VBSURF)
C
                  IF(VALT.EQ.124) VARABL(INT(W1),5)=ALENS(71,VBSURF)
                  IF(VALT.EQ.124) VARABL(INT(W1),13)=ALENS(71,VBSURF)
                  ISCOMP(INT(W1))=.TRUE.
              END IF
              RETURN
          END IF
C     DO N7
          IF(VALT.EQ.125) THEN
              IF(GLANAM(VBSURF,1)(1:5).NE.'GLASS') THEN
                  WRITE(OUTLYNE,*)
     1            'GLASS MUST BE CONVERTED FROM A CATALOG GLASS'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)
     1            'TO A "GLASS" TYPE OF ENTRY BEFORE INDEX CAN BE A VARIABLE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*) 'NO-ACTION TAKEN'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C     PIKUP CHECK
C     IF AN PIKUP GLASS EXISTS THEN
              IF(PIKUP(1,VBSURF,20).EQ.1.0D0) THEN
C     A CONFLICTING PIKUP EXISTS, DISSALLOW THE VARIABLE ASSIGNMENT
                  WRITE(OUTLYNE,*)
     1            'A CONFLICTING GLASS PIKUP EXISTS ON THIS SURFACE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,666) WC,INT(W2)
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'VARIABLE ASSIGNMENT WAS NOT MADE'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              ELSE
C     NO CONFLICTING PIKUP EXISTS, ASSIGN THE VARIABLE
C
                  IF(VALT.EQ.125) VARABL(INT(W1),4)=ALENS(72,VBSURF)
C
                  IF(VALT.EQ.125) VARABL(INT(W1),5)=ALENS(72,VBSURF)
                  IF(VALT.EQ.125) VARABL(INT(W1),13)=ALENS(72,VBSURF)
                  ISCOMP(INT(W1))=.TRUE.
              END IF
              RETURN
          END IF
C     DO N8
          IF(VALT.EQ.126) THEN
              IF(GLANAM(VBSURF,1)(1:5).NE.'GLASS') THEN
                  WRITE(OUTLYNE,*)
     1            'GLASS MUST BE CONVERTED FROM A CATALOG GLASS'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)
     1            'TO A "GLASS" TYPE OF ENTRY BEFORE INDEX CAN BE A VARIABLE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*) 'NO-ACTION TAKEN'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C     PIKUP CHECK
C     IF AN PIKUP GLASS EXISTS THEN
              IF(PIKUP(1,VBSURF,20).EQ.1.0D0) THEN
C     A CONFLICTING PIKUP EXISTS, DISSALLOW THE VARIABLE ASSIGNMENT
                  WRITE(OUTLYNE,*)
     1            'A CONFLICTING GLASS PIKUP EXISTS ON THIS SURFACE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,666) WC,INT(W2)
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'VARIABLE ASSIGNMENT WAS NOT MADE'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              ELSE
C     NO CONFLICTING PIKUP EXISTS, ASSIGN THE VARIABLE
C
                  IF(VALT.EQ.126) VARABL(INT(W1),4)=ALENS(73,VBSURF)
C
                  IF(VALT.EQ.126) VARABL(INT(W1),5)=ALENS(73,VBSURF)
                  IF(VALT.EQ.126) VARABL(INT(W1),13)=ALENS(73,VBSURF)
                  ISCOMP(INT(W1))=.TRUE.
              END IF
              RETURN
          END IF
C     DO N9
          IF(VALT.EQ.127) THEN
              IF(GLANAM(VBSURF,1)(1:5).NE.'GLASS') THEN
                  WRITE(OUTLYNE,*)
     1            'GLASS MUST BE CONVERTED FROM A CATALOG GLASS'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)
     1            'TO A "GLASS" TYPE OF ENTRY BEFORE INDEX CAN BE A VARIABLE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*) 'NO-ACTION TAKEN'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C     PIKUP CHECK
C     IF AN PIKUP GLASS EXISTS THEN
              IF(PIKUP(1,VBSURF,20).EQ.1.0D0) THEN
C     A CONFLICTING PIKUP EXISTS, DISSALLOW THE VARIABLE ASSIGNMENT
                  WRITE(OUTLYNE,*)
     1            'A CONFLICTING GLASS PIKUP EXISTS ON THIS SURFACE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,666) WC,INT(W2)
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'VARIABLE ASSIGNMENT WAS NOT MADE'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              ELSE
C     NO CONFLICTING PIKUP EXISTS, ASSIGN THE VARIABLE
C
                  IF(VALT.EQ.127) VARABL(INT(W1),4)=ALENS(74,VBSURF)
C
                  IF(VALT.EQ.127) VARABL(INT(W1),5)=ALENS(74,VBSURF)
                  IF(VALT.EQ.127) VARABL(INT(W1),13)=ALENS(74,VBSURF)
                  ISCOMP(INT(W1))=.TRUE.
              END IF
              RETURN
          END IF
C     DO N10
          IF(VALT.EQ.128) THEN
              IF(GLANAM(VBSURF,1)(1:5).NE.'GLASS') THEN
                  WRITE(OUTLYNE,*)
     1            'GLASS MUST BE CONVERTED FROM A CATALOG GLASS'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)
     1            'TO A "GLASS" TYPE OF ENTRY BEFORE INDEX CAN BE A VARIABLE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*) 'NO-ACTION TAKEN'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C     PIKUP CHECK
C     IF AN PIKUP GLASS EXISTS THEN
              IF(PIKUP(1,VBSURF,20).EQ.1.0D0) THEN
C     A CONFLICTING PIKUP EXISTS, DISSALLOW THE VARIABLE ASSIGNMENT
                  WRITE(OUTLYNE,*)
     1            'A CONFLICTING GLASS PIKUP EXISTS ON THIS SURFACE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,666) WC,INT(W2)
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'VARIABLE ASSIGNMENT WAS NOT MADE'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              ELSE
C     NO CONFLICTING PIKUP EXISTS, ASSIGN THE VARIABLE
C
                  IF(VALT.EQ.128) VARABL(INT(W1),4)=ALENS(75,VBSURF)
C
                  IF(VALT.EQ.128) VARABL(INT(W1),5)=ALENS(75,VBSURF)
                  IF(VALT.EQ.128) VARABL(INT(W1),13)=ALENS(75,VBSURF)
                  ISCOMP(INT(W1))=.TRUE.
              END IF
              RETURN
          END IF
C
C     DO INDEX
          IF(VALT.EQ.135) THEN
              IF(GLANAM(VBSURF,1)(1:5).NE.'MODEL') THEN
                  WRITE(OUTLYNE,*)
     1            'GLASS MUST BE CONVERTED FROM A CATALOG GLASS'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)
     1            'TO A "MODEL" TYPE OF ENTRY BEFORE INDEX CAN BE A VARIABLE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*) 'NO-ACTION TAKEN'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C     PIKUP CHECK
C     IF AN PIKUP GLASS EXISTS THEN
              IF(PIKUP(1,VBSURF,20).EQ.1.0D0) THEN
C     A CONFLICTING PIKUP EXISTS, DISSALLOW THE VARIABLE ASSIGNMENT
                  WRITE(OUTLYNE,*)
     1            'A CONFLICTING GLASS PIKUP EXISTS ON THIS SURFACE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,666) WC,INT(W2)
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'VARIABLE ASSIGNMENT WAS NOT MADE'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              ELSE
C     NO CONFLICTING PIKUP EXISTS, ASSIGN THE VARIABLE
C
                  IF(VALT.EQ.135) VARABL(INT(W1),4)=ALENS(86,VBSURF)
C
                  IF(VALT.EQ.135) VARABL(INT(W1),5)=ALENS(86,VBSURF)
                  IF(VALT.EQ.135) VARABL(INT(W1),13)=ALENS(86,VBSURF)
                  ISCOMP(INT(W1))=.TRUE.
              END IF
              RETURN
          END IF
C
C     DO VNUM
          IF(VALT.EQ.136) THEN
              IF(GLANAM(VBSURF,1)(1:5).NE.'MODEL') THEN
                  WRITE(OUTLYNE,*)
     1            'GLASS MUST BE CONVERTED FROM A CATALOG GLASS'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)
     1            'TO A "MODEL" TYPE OF ENTRY BEFORE INDEX CAN BE A VARIABLE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*) 'NO-ACTION TAKEN'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C     PIKUP CHECK
C     IF AN PIKUP GLASS EXISTS THEN
              IF(PIKUP(1,VBSURF,20).EQ.1.0D0) THEN
C     A CONFLICTING PIKUP EXISTS, DISSALLOW THE VARIABLE ASSIGNMENT
                  WRITE(OUTLYNE,*)
     1            'A CONFLICTING GLASS PIKUP EXISTS ON THIS SURFACE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,666) WC,INT(W2)
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'VARIABLE ASSIGNMENT WAS NOT MADE'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              ELSE
C     NO CONFLICTING PIKUP EXISTS, ASSIGN THE VARIABLE
C
                  IF(VALT.EQ.136) VARABL(INT(W1),4)=ALENS(87,VBSURF)
C
                  IF(VALT.EQ.136) VARABL(INT(W1),5)=ALENS(87,VBSURF)
                  IF(VALT.EQ.136) VARABL(INT(W1),13)=ALENS(87,VBSURF)
                  ISCOMP(INT(W1))=.TRUE.
              END IF
              RETURN
          END IF
C
C
C     DO DPART
          IF(VALT.EQ.140) THEN
              IF(GLANAM(VBSURF,1)(1:5).NE.'MODEL') THEN
                  WRITE(OUTLYNE,*)
     1            'GLASS MUST BE CONVERTED FROM A CATALOG GLASS'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)
     1            'TO A "MODEL" TYPE OF ENTRY BEFORE INDEX CAN BE A VARIABLE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*) 'NO-ACTION TAKEN'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C     PIKUP CHECK
C     IF AN PIKUP GLASS EXISTS THEN
              IF(PIKUP(1,VBSURF,20).EQ.1.0D0) THEN
C     A CONFLICTING PIKUP EXISTS, DISSALLOW THE VARIABLE ASSIGNMENT
                  WRITE(OUTLYNE,*)
     1            'A CONFLICTING GLASS PIKUP EXISTS ON THIS SURFACE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,666) WC,INT(W2)
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'VARIABLE ASSIGNMENT WAS NOT MADE'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              ELSE
C     NO CONFLICTING PIKUP EXISTS, ASSIGN THE VARIABLE
C
                  IF(VALT.EQ.140) VARABL(INT(W1),4)=ALENS(89,VBSURF)
C
                  IF(VALT.EQ.140) VARABL(INT(W1),5)=ALENS(89,VBSURF)
                  IF(VALT.EQ.140) VARABL(INT(W1),13)=ALENS(89,VBSURF)
                  ISCOMP(INT(W1))=.TRUE.
              END IF
              RETURN
          END IF
C
          IF(VALT.EQ.129) THEN
              IF(ALENS(8,VBSURF).NE.1.0D0) ALENS(8,VBSURF)=1.0D0
C     PIKUP CHECK
C     IF A AH,PRO OR NPRO PIKUPS EXISTS THEN
              IF(PIKUP(1,VBSURF,27).EQ.1.0D0
     1        .OR.PIKUP(1,VBSURF,11).EQ.1.0D0
     1        .OR.PIKUP(1,VBSURF,12).EQ.1.0D0) THEN
C     A CONFLICTING SHAPE PIKUP EXISTS, DISSALLOW THE VARIABLE ASSIGNMENT
                  WRITE(OUTLYNE,*)
     1            'A CONFLICTING SURFACE SHAPE PIKUP EXISTS ON THIS SURFACE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,666) WC,INT(W2)
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'VARIABLE ASSIGNMENT WAS NOT MADE'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              ELSE
C     NO CONFLICTING PIKUP EXISTS, ASSIGN THE VARIABLE
C
                  IF(VALT.EQ.129) VARABL(INT(W1),4)=ALENS(81,VBSURF)
C
                  IF(VALT.EQ.129) VARABL(INT(W1),5)=ALENS(81,VBSURF)
                  IF(VALT.EQ.129) VARABL(INT(W1),13)=ALENS(81,VBSURF)
                  ISCOMP(INT(W1))=.TRUE.
              END IF
              RETURN
          END IF
C
          IF(VALT.EQ.130) THEN
              IF(ALENS(8,VBSURF).NE.1.0D0) ALENS(8,VBSURF)=1.0D0
C     PIKUP CHECK
C     IF A AI,PRO OR NPRO PIKUPS EXISTS THEN
              IF(PIKUP(1,VBSURF,28).EQ.1.0D0
     1        .OR.PIKUP(1,VBSURF,11).EQ.1.0D0
     1        .OR.PIKUP(1,VBSURF,12).EQ.1.0D0) THEN
C     A CONFLICTING SHAPE PIKUP EXISTS, DISSALLOW THE VARIABLE ASSIGNMENT
                  WRITE(OUTLYNE,*)
     1            'A CONFLICTING SURFACE SHAPE PIKUP EXISTS ON THIS SURFACE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,666) WC,INT(W2)
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'VARIABLE ASSIGNMENT WAS NOT MADE'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              ELSE
C     NO CONFLICTING PIKUP EXISTS, ASSIGN THE VARIABLE
C
                  IF(VALT.EQ.130) VARABL(INT(W1),4)=ALENS(82,VBSURF)
C
                  IF(VALT.EQ.130) VARABL(INT(W1),5)=ALENS(82,VBSURF)
                  IF(VALT.EQ.130) VARABL(INT(W1),13)=ALENS(82,VBSURF)
                  ISCOMP(INT(W1))=.TRUE.
              END IF
              RETURN
          END IF
C
          IF(VALT.EQ.131) THEN
              IF(ALENS(8,VBSURF).NE.1.0D0) ALENS(8,VBSURF)=1.0D0
C     PIKUP CHECK
C     IF A AJ,PRO OR NPRO PIKUPS EXISTS THEN
              IF(PIKUP(1,VBSURF,29).EQ.1.0D0
     1        .OR.PIKUP(1,VBSURF,11).EQ.1.0D0
     1        .OR.PIKUP(1,VBSURF,12).EQ.1.0D0) THEN
C     A CONFLICTING SHAPE PIKUP EXISTS, DISSALLOW THE VARIABLE ASSIGNMENT
                  WRITE(OUTLYNE,*)
     1            'A CONFLICTING SURFACE SHAPE PIKUP EXISTS ON THIS SURFACE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,666) WC,INT(W2)
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'VARIABLE ASSIGNMENT WAS NOT MADE'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              ELSE
C     NO CONFLICTING PIKUP EXISTS, ASSIGN THE VARIABLE
C
                  IF(VALT.EQ.131) VARABL(INT(W1),4)=ALENS(83,VBSURF)
C
                  IF(VALT.EQ.131) VARABL(INT(W1),5)=ALENS(83,VBSURF)
                  IF(VALT.EQ.131) VARABL(INT(W1),13)=ALENS(83,VBSURF)
                  ISCOMP(INT(W1))=.TRUE.
              END IF
              RETURN
          END IF
C
          IF(VALT.EQ.132) THEN
              IF(ALENS(8,VBSURF).NE.1.0D0) ALENS(8,VBSURF)=1.0D0
C     PIKUP CHECK
C     IF A AK,PRO OR NPRO PIKUPS EXISTS THEN
              IF(PIKUP(1,VBSURF,30).EQ.1.0D0
     1        .OR.PIKUP(1,VBSURF,11).EQ.1.0D0
     1        .OR.PIKUP(1,VBSURF,12).EQ.1.0D0) THEN
C     A CONFLICTING SHAPE PIKUP EXISTS, DISSALLOW THE VARIABLE ASSIGNMENT
                  WRITE(OUTLYNE,*)
     1            'A CONFLICTING SURFACE SHAPE PIKUP EXISTS ON THIS SURFACE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,666) WC,INT(W2)
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'VARIABLE ASSIGNMENT WAS NOT MADE'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              ELSE
C     NO CONFLICTING PIKUP EXISTS, ASSIGN THE VARIABLE
C
                  IF(VALT.EQ.132) VARABL(INT(W1),4)=ALENS(84,VBSURF)
C
                  IF(VALT.EQ.132) VARABL(INT(W1),5)=ALENS(84,VBSURF)
                  IF(VALT.EQ.132) VARABL(INT(W1),13)=ALENS(84,VBSURF)
                  ISCOMP(INT(W1))=.TRUE.
              END IF
              RETURN
          END IF
C
          IF(VALT.EQ.133) THEN
              IF(ALENS(8,VBSURF).NE.1.0D0) ALENS(8,VBSURF)=1.0D0
C     PIKUP CHECK
C     IF A AL,PRO OR NPRO PIKUPS EXISTS THEN
              IF(PIKUP(1,VBSURF,31).EQ.1.0D0
     1        .OR.PIKUP(1,VBSURF,11).EQ.1.0D0
     1        .OR.PIKUP(1,VBSURF,12).EQ.1.0D0) THEN
C     A CONFLICTING SHAPE PIKUP EXISTS, DISSALLOW THE VARIABLE ASSIGNMENT
                  WRITE(OUTLYNE,*)
     1            'A CONFLICTING SURFACE SHAPE PIKUP EXISTS ON THIS SURFACE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,666) WC,INT(W2)
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'VARIABLE ASSIGNMENT WAS NOT MADE'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              ELSE
C     NO CONFLICTING PIKUP EXISTS, ASSIGN THE VARIABLE
C
                  IF(VALT.EQ.133) VARABL(INT(W1),4)=ALENS(85,VBSURF)
C
                  IF(VALT.EQ.133) VARABL(INT(W1),5)=ALENS(85,VBSURF)
                  IF(VALT.EQ.133) VARABL(INT(W1),13)=ALENS(85,VBSURF)
                  ISCOMP(INT(W1))=.TRUE.
              END IF
              RETURN
          END IF
C
          IF(VALT.GE.27.AND.VALT.LE.74.OR.
     1    VALT.GE.76.AND.VALT.LE.123) THEN
              CNOT=.FALSE.
              IF(DABS(ALENS(34,VBSURF)).EQ.1.0D0.OR.
     1        DABS(ALENS(34,VBSURF)).EQ.6.0D0) THEN
                  IF(WC.EQ.'C1') CNOT=.TRUE.
                  IF(WC.EQ.'C2') CNOT=.TRUE.
                  IF(WC.EQ.'C3') CNOT=.TRUE.
                  IF(WC.EQ.'C4') CNOT=.TRUE.
                  IF(WC.EQ.'C5') CNOT=.TRUE.
                  IF(WC.EQ.'C6') CNOT=.TRUE.
                  IF(WC.EQ.'C7') CNOT=.TRUE.
                  IF(WC.EQ.'C8') CNOT=.TRUE.
                  IF(WC.EQ.'C49') CNOT=.TRUE.
                  IF(WC.EQ.'C50') CNOT=.TRUE.
                  IF(WC.EQ.'C51') CNOT=.TRUE.
                  IF(WC.EQ.'C52') CNOT=.TRUE.
                  IF(WC.EQ.'C53') CNOT=.TRUE.
                  IF(WC.EQ.'C54') CNOT=.TRUE.
                  IF(WC.EQ.'C55') CNOT=.TRUE.
                  IF(WC.EQ.'C56') CNOT=.TRUE.
                  IF(WC.EQ.'C57') CNOT=.TRUE.
                  IF(WC.EQ.'C58') CNOT=.TRUE.
                  IF(WC.EQ.'C59') CNOT=.TRUE.
                  IF(WC.EQ.'C60') CNOT=.TRUE.
                  IF(WC.EQ.'C61') CNOT=.TRUE.
                  IF(WC.EQ.'C62') CNOT=.TRUE.
                  IF(WC.EQ.'C63') CNOT=.TRUE.
                  IF(WC.EQ.'C64') CNOT=.TRUE.
                  IF(WC.EQ.'C65') CNOT=.TRUE.
                  IF(WC.EQ.'C66') CNOT=.TRUE.
                  IF(WC.EQ.'C67') CNOT=.TRUE.
                  IF(WC.EQ.'C68') CNOT=.TRUE.
                  IF(WC.EQ.'C69') CNOT=.TRUE.
                  IF(WC.EQ.'C70') CNOT=.TRUE.
                  IF(WC.EQ.'C71') CNOT=.TRUE.
                  IF(WC.EQ.'C72') CNOT=.TRUE.
                  IF(WC.EQ.'C73') CNOT=.TRUE.
                  IF(WC.EQ.'C74') CNOT=.TRUE.
                  IF(WC.EQ.'C75') CNOT=.TRUE.
                  IF(WC.EQ.'C76') CNOT=.TRUE.
                  IF(WC.EQ.'C77') CNOT=.TRUE.
                  IF(WC.EQ.'C78') CNOT=.TRUE.
                  IF(WC.EQ.'C79') CNOT=.TRUE.
                  IF(WC.EQ.'C80') CNOT=.TRUE.
                  IF(WC.EQ.'C81') CNOT=.TRUE.
                  IF(WC.EQ.'C82') CNOT=.TRUE.
                  IF(WC.EQ.'C83') CNOT=.TRUE.
                  IF(WC.EQ.'C84') CNOT=.TRUE.
                  IF(WC.EQ.'C85') CNOT=.TRUE.
                  IF(WC.EQ.'C86') CNOT=.TRUE.
                  IF(WC.EQ.'C87') CNOT=.TRUE.
                  IF(WC.EQ.'C88') CNOT=.TRUE.
                  IF(WC.EQ.'C89') CNOT=.TRUE.
                  IF(WC.EQ.'C90') CNOT=.TRUE.
                  IF(WC.EQ.'C91') CNOT=.TRUE.
                  IF(WC.EQ.'C92') CNOT=.TRUE.
                  IF(WC.EQ.'C93') CNOT=.TRUE.
                  IF(WC.EQ.'C94') CNOT=.TRUE.
                  IF(WC.EQ.'C95') CNOT=.TRUE.
                  IF(WC.EQ.'C96') CNOT=.TRUE.
                  IF(CNOT) THEN
                      WRITE(OUTLYNE,*)
     1                'SPECIAL SURFACE TYPES 1 AND 6'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'ONLY USE COEFFICIENTS C9 THROUGH C48'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
              IF(DABS(ALENS(34,VBSURF)).EQ.2.0D0.OR.
     1        DABS(ALENS(34,VBSURF)).EQ.9.0D0) THEN
                  IF(WC.EQ.'C31') CNOT=.TRUE.
                  IF(WC.EQ.'C32') CNOT=.TRUE.
                  IF(WC.EQ.'C33') CNOT=.TRUE.
                  IF(WC.EQ.'C34') CNOT=.TRUE.
                  IF(WC.EQ.'C35') CNOT=.TRUE.
                  IF(WC.EQ.'C36') CNOT=.TRUE.
                  IF(WC.EQ.'C37') CNOT=.TRUE.
                  IF(WC.EQ.'C38') CNOT=.TRUE.
                  IF(WC.EQ.'C39') CNOT=.TRUE.
                  IF(WC.EQ.'C40') CNOT=.TRUE.
                  IF(WC.EQ.'C41') CNOT=.TRUE.
                  IF(WC.EQ.'C42') CNOT=.TRUE.
                  IF(WC.EQ.'C43') CNOT=.TRUE.
                  IF(WC.EQ.'C44') CNOT=.TRUE.
                  IF(WC.EQ.'C45') CNOT=.TRUE.
                  IF(WC.EQ.'C46') CNOT=.TRUE.
                  IF(WC.EQ.'C47') CNOT=.TRUE.
                  IF(WC.EQ.'C48') CNOT=.TRUE.
                  IF(WC.EQ.'C49') CNOT=.TRUE.
                  IF(WC.EQ.'C50') CNOT=.TRUE.
                  IF(WC.EQ.'C51') CNOT=.TRUE.
                  IF(WC.EQ.'C52') CNOT=.TRUE.
                  IF(WC.EQ.'C53') CNOT=.TRUE.
                  IF(WC.EQ.'C54') CNOT=.TRUE.
                  IF(WC.EQ.'C55') CNOT=.TRUE.
                  IF(WC.EQ.'C56') CNOT=.TRUE.
                  IF(WC.EQ.'C57') CNOT=.TRUE.
                  IF(WC.EQ.'C58') CNOT=.TRUE.
                  IF(WC.EQ.'C59') CNOT=.TRUE.
                  IF(WC.EQ.'C60') CNOT=.TRUE.
                  IF(WC.EQ.'C61') CNOT=.TRUE.
                  IF(WC.EQ.'C62') CNOT=.TRUE.
                  IF(WC.EQ.'C63') CNOT=.TRUE.
                  IF(WC.EQ.'C64') CNOT=.TRUE.
                  IF(WC.EQ.'C65') CNOT=.TRUE.
                  IF(WC.EQ.'C66') CNOT=.TRUE.
                  IF(WC.EQ.'C67') CNOT=.TRUE.
                  IF(WC.EQ.'C68') CNOT=.TRUE.
                  IF(WC.EQ.'C69') CNOT=.TRUE.
                  IF(WC.EQ.'C70') CNOT=.TRUE.
                  IF(WC.EQ.'C71') CNOT=.TRUE.
                  IF(WC.EQ.'C72') CNOT=.TRUE.
                  IF(WC.EQ.'C73') CNOT=.TRUE.
                  IF(WC.EQ.'C74') CNOT=.TRUE.
                  IF(WC.EQ.'C75') CNOT=.TRUE.
                  IF(WC.EQ.'C76') CNOT=.TRUE.
                  IF(WC.EQ.'C77') CNOT=.TRUE.
                  IF(WC.EQ.'C78') CNOT=.TRUE.
                  IF(WC.EQ.'C79') CNOT=.TRUE.
                  IF(WC.EQ.'C80') CNOT=.TRUE.
                  IF(WC.EQ.'C81') CNOT=.TRUE.
                  IF(WC.EQ.'C82') CNOT=.TRUE.
                  IF(WC.EQ.'C83') CNOT=.TRUE.
                  IF(WC.EQ.'C84') CNOT=.TRUE.
                  IF(WC.EQ.'C85') CNOT=.TRUE.
                  IF(WC.EQ.'C86') CNOT=.TRUE.
                  IF(WC.EQ.'C87') CNOT=.TRUE.
                  IF(WC.EQ.'C88') CNOT=.TRUE.
                  IF(WC.EQ.'C89') CNOT=.TRUE.
                  IF(WC.EQ.'C90') CNOT=.TRUE.
                  IF(WC.EQ.'C91') CNOT=.TRUE.
                  IF(WC.EQ.'C92') CNOT=.TRUE.
                  IF(WC.EQ.'C93') CNOT=.TRUE.
                  IF(WC.EQ.'C94') CNOT=.TRUE.
                  IF(WC.EQ.'C95') CNOT=.TRUE.
                  IF(WC.EQ.'C96') CNOT=.TRUE.
                  IF(CNOT) THEN
                      WRITE(OUTLYNE,*)
     1                'SPECIAL SURFACE TYPES 2 AND 9'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'ONLY USE COEFFICIENTS C1 THROUGH C30'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
              IF(DABS(ALENS(34,VBSURF)).EQ.3.0D0.OR.
     1        DABS(ALENS(34,VBSURF)).EQ.10.0D0) THEN
                  IF(WC.EQ.'C38') CNOT=.TRUE.
                  IF(WC.EQ.'C39') CNOT=.TRUE.
                  IF(WC.EQ.'C40') CNOT=.TRUE.
                  IF(WC.EQ.'C41') CNOT=.TRUE.
                  IF(WC.EQ.'C42') CNOT=.TRUE.
                  IF(WC.EQ.'C43') CNOT=.TRUE.
                  IF(WC.EQ.'C44') CNOT=.TRUE.
                  IF(WC.EQ.'C45') CNOT=.TRUE.
                  IF(WC.EQ.'C46') CNOT=.TRUE.
                  IF(WC.EQ.'C47') CNOT=.TRUE.
                  IF(WC.EQ.'C48') CNOT=.TRUE.
                  IF(WC.EQ.'C49') CNOT=.TRUE.
                  IF(WC.EQ.'C50') CNOT=.TRUE.
                  IF(WC.EQ.'C51') CNOT=.TRUE.
                  IF(WC.EQ.'C52') CNOT=.TRUE.
                  IF(WC.EQ.'C53') CNOT=.TRUE.
                  IF(WC.EQ.'C54') CNOT=.TRUE.
                  IF(WC.EQ.'C55') CNOT=.TRUE.
                  IF(WC.EQ.'C56') CNOT=.TRUE.
                  IF(WC.EQ.'C57') CNOT=.TRUE.
                  IF(WC.EQ.'C58') CNOT=.TRUE.
                  IF(WC.EQ.'C59') CNOT=.TRUE.
                  IF(WC.EQ.'C60') CNOT=.TRUE.
                  IF(WC.EQ.'C61') CNOT=.TRUE.
                  IF(WC.EQ.'C62') CNOT=.TRUE.
                  IF(WC.EQ.'C63') CNOT=.TRUE.
                  IF(WC.EQ.'C64') CNOT=.TRUE.
                  IF(WC.EQ.'C65') CNOT=.TRUE.
                  IF(WC.EQ.'C66') CNOT=.TRUE.
                  IF(WC.EQ.'C67') CNOT=.TRUE.
                  IF(WC.EQ.'C68') CNOT=.TRUE.
                  IF(WC.EQ.'C69') CNOT=.TRUE.
                  IF(WC.EQ.'C70') CNOT=.TRUE.
                  IF(WC.EQ.'C71') CNOT=.TRUE.
                  IF(WC.EQ.'C72') CNOT=.TRUE.
                  IF(WC.EQ.'C73') CNOT=.TRUE.
                  IF(WC.EQ.'C74') CNOT=.TRUE.
                  IF(WC.EQ.'C75') CNOT=.TRUE.
                  IF(WC.EQ.'C76') CNOT=.TRUE.
                  IF(WC.EQ.'C77') CNOT=.TRUE.
                  IF(WC.EQ.'C78') CNOT=.TRUE.
                  IF(WC.EQ.'C79') CNOT=.TRUE.
                  IF(WC.EQ.'C80') CNOT=.TRUE.
                  IF(WC.EQ.'C81') CNOT=.TRUE.
                  IF(WC.EQ.'C82') CNOT=.TRUE.
                  IF(WC.EQ.'C83') CNOT=.TRUE.
                  IF(WC.EQ.'C84') CNOT=.TRUE.
                  IF(WC.EQ.'C85') CNOT=.TRUE.
                  IF(WC.EQ.'C86') CNOT=.TRUE.
                  IF(WC.EQ.'C87') CNOT=.TRUE.
                  IF(WC.EQ.'C88') CNOT=.TRUE.
                  IF(WC.EQ.'C89') CNOT=.TRUE.
                  IF(WC.EQ.'C90') CNOT=.TRUE.
                  IF(WC.EQ.'C91') CNOT=.TRUE.
                  IF(WC.EQ.'C92') CNOT=.TRUE.
                  IF(WC.EQ.'C93') CNOT=.TRUE.
                  IF(WC.EQ.'C94') CNOT=.TRUE.
                  IF(WC.EQ.'C95') CNOT=.TRUE.
                  IF(WC.EQ.'C96') CNOT=.TRUE.
                  IF(CNOT) THEN
                      WRITE(OUTLYNE,*)
     1                'SPECIAL SURFACE TYPES 3 AND 10'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'ONLY USE COEFFICIENTS C1 THROUGH C37'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
              IF(DABS(ALENS(34,VBSURF)).EQ.4.0D0) THEN
                  IF(WC.NE.'C1'.AND.WC.NE.'C2'.AND.WC.NE.'C3'
     1            .AND.WC.NE.'C4'.AND.WC.NE.'C5'.AND.WC.NE.'C6'
     1            .AND.WC.NE.'C7'.AND.WC.NE.'C8'.AND.WC.NE.'C9'
     1            .AND.WC.NE.'C10'.AND.WC.NE.'C11'.AND.WC.NE.'C12'
     1            .AND.WC.NE.'C13'.AND.WC.NE.'C14'.AND.WC.NE.'C15') THEN
                      WRITE(OUTLYNE,*)
     1                'SPECIAL SURFACE TYPE 4 ONLY USES COEFFICIENTS C1 THROUGH C15'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
              IF(DABS(ALENS(34,VBSURF)).EQ.18.0D0) THEN
                  IF(WC.NE.'C1'.AND.WC.NE.'C2'.AND.WC.NE.'C3'.AND.WC.NE.'C4'
     1            .AND.WC.NE.'C5'.AND.WC.NE.'C6'.AND.WC.NE.'C7'.AND.
     1            WC.NE.'C8'.AND.WC.NE.'C9'.AND.WC.NE.'C10'.AND.
     1            WC.NE.'C11'.AND.WC.NE.'C12'.AND.WC.NE.'C13'.AND.
     1            WC.NE.'C14'.AND.WC.NE.'C15'.AND.WC.NE.'C16'.AND.
     1            WC.NE.'C17'.AND.WC.NE.'C18') THEN
                      WRITE(OUTLYNE,*)
     1                'SPECIAL SURFACE TYPE 18 ONLY USES COEFFICIENTS C1 THROUGH C18'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
              IF(DABS(ALENS(34,VBSURF)).EQ.7.0D0.OR.
     1        DABS(ALENS(34,VBSURF)).EQ.8.0D0) THEN
                  IF(WC.EQ.'C92') CNOT=.TRUE.
                  IF(WC.EQ.'C93') CNOT=.TRUE.
                  IF(WC.EQ.'C94') CNOT=.TRUE.
                  IF(WC.EQ.'C95') CNOT=.TRUE.
                  IF(WC.EQ.'C96') CNOT=.TRUE.
                  IF(CNOT) THEN
                      WRITE(OUTLYNE,*)
     1                'SPECIAL SURFACE TYPES 7 AND 8'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'ONLY USE COEFFICIENTS C1 THROUGH C91'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
              IF(DABS(ALENS(34,VBSURF)).EQ.12.0D0) THEN
                  IF(WC.EQ.'C24') CNOT=.TRUE.
                  IF(WC.EQ.'C25') CNOT=.TRUE.
                  IF(WC.EQ.'C26') CNOT=.TRUE.
                  IF(WC.EQ.'C27') CNOT=.TRUE.
                  IF(WC.EQ.'C28') CNOT=.TRUE.
                  IF(WC.EQ.'C29') CNOT=.TRUE.
                  IF(WC.EQ.'C30') CNOT=.TRUE.
                  IF(WC.EQ.'C31') CNOT=.TRUE.
                  IF(WC.EQ.'C32') CNOT=.TRUE.
                  IF(WC.EQ.'C33') CNOT=.TRUE.
                  IF(WC.EQ.'C34') CNOT=.TRUE.
                  IF(WC.EQ.'C35') CNOT=.TRUE.
                  IF(WC.EQ.'C36') CNOT=.TRUE.
                  IF(WC.EQ.'C37') CNOT=.TRUE.
                  IF(WC.EQ.'C38') CNOT=.TRUE.
                  IF(WC.EQ.'C39') CNOT=.TRUE.
                  IF(WC.EQ.'C40') CNOT=.TRUE.
                  IF(WC.EQ.'C41') CNOT=.TRUE.
                  IF(WC.EQ.'C42') CNOT=.TRUE.
                  IF(WC.EQ.'C43') CNOT=.TRUE.
                  IF(WC.EQ.'C44') CNOT=.TRUE.
                  IF(WC.EQ.'C45') CNOT=.TRUE.
                  IF(WC.EQ.'C46') CNOT=.TRUE.
                  IF(WC.EQ.'C47') CNOT=.TRUE.
                  IF(WC.EQ.'C48') CNOT=.TRUE.
                  IF(WC.EQ.'C49') CNOT=.TRUE.
                  IF(WC.EQ.'C50') CNOT=.TRUE.
                  IF(WC.EQ.'C51') CNOT=.TRUE.
                  IF(WC.EQ.'C52') CNOT=.TRUE.
                  IF(WC.EQ.'C53') CNOT=.TRUE.
                  IF(WC.EQ.'C54') CNOT=.TRUE.
                  IF(WC.EQ.'C55') CNOT=.TRUE.
                  IF(WC.EQ.'C56') CNOT=.TRUE.
                  IF(WC.EQ.'C57') CNOT=.TRUE.
                  IF(WC.EQ.'C58') CNOT=.TRUE.
                  IF(WC.EQ.'C59') CNOT=.TRUE.
                  IF(WC.EQ.'C60') CNOT=.TRUE.
                  IF(WC.EQ.'C61') CNOT=.TRUE.
                  IF(WC.EQ.'C62') CNOT=.TRUE.
                  IF(WC.EQ.'C63') CNOT=.TRUE.
                  IF(WC.EQ.'C64') CNOT=.TRUE.
                  IF(WC.EQ.'C65') CNOT=.TRUE.
                  IF(WC.EQ.'C66') CNOT=.TRUE.
                  IF(WC.EQ.'C67') CNOT=.TRUE.
                  IF(WC.EQ.'C68') CNOT=.TRUE.
                  IF(WC.EQ.'C69') CNOT=.TRUE.
                  IF(WC.EQ.'C70') CNOT=.TRUE.
                  IF(WC.EQ.'C71') CNOT=.TRUE.
                  IF(WC.EQ.'C72') CNOT=.TRUE.
                  IF(WC.EQ.'C73') CNOT=.TRUE.
                  IF(WC.EQ.'C74') CNOT=.TRUE.
                  IF(WC.EQ.'C75') CNOT=.TRUE.
                  IF(WC.EQ.'C76') CNOT=.TRUE.
                  IF(WC.EQ.'C77') CNOT=.TRUE.
                  IF(WC.EQ.'C78') CNOT=.TRUE.
                  IF(WC.EQ.'C79') CNOT=.TRUE.
                  IF(WC.EQ.'C80') CNOT=.TRUE.
                  IF(WC.EQ.'C81') CNOT=.TRUE.
                  IF(WC.EQ.'C82') CNOT=.TRUE.
                  IF(WC.EQ.'C83') CNOT=.TRUE.
                  IF(WC.EQ.'C84') CNOT=.TRUE.
                  IF(WC.EQ.'C85') CNOT=.TRUE.
                  IF(WC.EQ.'C86') CNOT=.TRUE.
                  IF(WC.EQ.'C87') CNOT=.TRUE.
                  IF(WC.EQ.'C88') CNOT=.TRUE.
                  IF(WC.EQ.'C89') CNOT=.TRUE.
                  IF(WC.EQ.'C90') CNOT=.TRUE.
                  IF(WC.EQ.'C91') CNOT=.TRUE.
                  IF(WC.EQ.'C92') CNOT=.TRUE.
                  IF(WC.EQ.'C93') CNOT=.TRUE.
                  IF(WC.EQ.'C94') CNOT=.TRUE.
                  IF(WC.EQ.'C95') CNOT=.TRUE.
                  IF(WC.EQ.'C96') CNOT=.TRUE.
                  IF(CNOT) THEN
                      WRITE(OUTLYNE,*)
     1                'SPECIAL SURFACE TYPE 12'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'ONLY USES COEFFICIENTS C1 THROUGH C23'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
          END IF

C     DO C1 TO 96
          IF(VALT.GE.27.AND.VALT.LE.74) THEN
              IF(ALENS(34,VBSURF).EQ.0.0D0) THEN
                  WRITE(OUTLYNE,*)
     1            'THE SURFACE IS NOT DEFINED AS A SPECIAL SURFACE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'VARIABLE ASSIGNMENT WAS NOT MADE'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C     PIKUP CHECK
C     IF AN PIKUP GLASS EXISTS THEN
              IF(PIKUP(1,VBSURF,11).EQ.1.0D0
     1        .OR.PIKUP(1,VBSURF,12).EQ.1.0D0) THEN
C     A CONFLICTING PIKUP EXISTS, DISSALLOW THE VARIABLE ASSIGNMENT
                  WRITE(OUTLYNE,*)
     1            'A CONFLICTING SURFACE SHAPE PIKUP EXISTS ON THIS SURFACE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,666) WC,INT(W2)
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'VARIABLE ASSIGNMENT WAS NOT MADE'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              ELSE
C     NO CONFLICTING PIKUP EXISTS, ASSIGN THE VARIABLE
C
                  IF(VALT.EQ.27) VARABL(INT(W1),4)=FTFL01(1,VBSURF)
                  IF(VALT.EQ.28) VARABL(INT(W1),4)=FTFL01(2,VBSURF)
                  IF(VALT.EQ.29) VARABL(INT(W1),4)=FTFL01(3,VBSURF)
                  IF(VALT.EQ.30) VARABL(INT(W1),4)=FTFL01(4,VBSURF)
                  IF(VALT.EQ.31) VARABL(INT(W1),4)=FTFL01(5,VBSURF)
                  IF(VALT.EQ.32) VARABL(INT(W1),4)=FTFL01(6,VBSURF)
                  IF(VALT.EQ.33) VARABL(INT(W1),4)=FTFL01(7,VBSURF)
                  IF(VALT.EQ.34) VARABL(INT(W1),4)=FTFL01(8,VBSURF)
                  IF(VALT.EQ.35) VARABL(INT(W1),4)=FTFL01(9,VBSURF)
                  IF(VALT.EQ.36) VARABL(INT(W1),4)=FTFL01(10,VBSURF)
                  IF(VALT.EQ.37) VARABL(INT(W1),4)=FTFL01(11,VBSURF)
                  IF(VALT.EQ.38) VARABL(INT(W1),4)=FTFL01(12,VBSURF)
                  IF(VALT.EQ.39) VARABL(INT(W1),4)=FTFL01(13,VBSURF)
                  IF(VALT.EQ.40) VARABL(INT(W1),4)=FTFL01(14,VBSURF)
                  IF(VALT.EQ.41) VARABL(INT(W1),4)=FTFL01(15,VBSURF)
                  IF(VALT.EQ.42) VARABL(INT(W1),4)=FTFL01(16,VBSURF)
                  IF(VALT.EQ.43) VARABL(INT(W1),4)=FTFL01(17,VBSURF)
                  IF(VALT.EQ.44) VARABL(INT(W1),4)=FTFL01(18,VBSURF)
                  IF(VALT.EQ.45) VARABL(INT(W1),4)=FTFL01(19,VBSURF)
                  IF(VALT.EQ.46) VARABL(INT(W1),4)=FTFL01(20,VBSURF)
                  IF(VALT.EQ.47) VARABL(INT(W1),4)=FTFL01(21,VBSURF)
                  IF(VALT.EQ.48) VARABL(INT(W1),4)=FTFL01(22,VBSURF)
                  IF(VALT.EQ.49) VARABL(INT(W1),4)=FTFL01(23,VBSURF)
                  IF(VALT.EQ.50) VARABL(INT(W1),4)=FTFL01(24,VBSURF)
                  IF(VALT.EQ.51) VARABL(INT(W1),4)=FTFL01(25,VBSURF)
                  IF(VALT.EQ.52) VARABL(INT(W1),4)=FTFL01(26,VBSURF)
                  IF(VALT.EQ.53) VARABL(INT(W1),4)=FTFL01(27,VBSURF)
                  IF(VALT.EQ.54) VARABL(INT(W1),4)=FTFL01(28,VBSURF)
                  IF(VALT.EQ.55) VARABL(INT(W1),4)=FTFL01(29,VBSURF)
                  IF(VALT.EQ.56) VARABL(INT(W1),4)=FTFL01(30,VBSURF)
                  IF(VALT.EQ.57) VARABL(INT(W1),4)=FTFL01(31,VBSURF)
                  IF(VALT.EQ.58) VARABL(INT(W1),4)=FTFL01(32,VBSURF)
                  IF(VALT.EQ.59) VARABL(INT(W1),4)=FTFL01(33,VBSURF)
                  IF(VALT.EQ.60) VARABL(INT(W1),4)=FTFL01(34,VBSURF)
                  IF(VALT.EQ.61) VARABL(INT(W1),4)=FTFL01(35,VBSURF)
                  IF(VALT.EQ.62) VARABL(INT(W1),4)=FTFL01(36,VBSURF)
                  IF(VALT.EQ.63) VARABL(INT(W1),4)=FTFL01(37,VBSURF)
                  IF(VALT.EQ.64) VARABL(INT(W1),4)=FTFL01(38,VBSURF)
                  IF(VALT.EQ.65) VARABL(INT(W1),4)=FTFL01(39,VBSURF)
                  IF(VALT.EQ.66) VARABL(INT(W1),4)=FTFL01(40,VBSURF)
                  IF(VALT.EQ.67) VARABL(INT(W1),4)=FTFL01(41,VBSURF)
                  IF(VALT.EQ.68) VARABL(INT(W1),4)=FTFL01(42,VBSURF)
                  IF(VALT.EQ.69) VARABL(INT(W1),4)=FTFL01(43,VBSURF)
                  IF(VALT.EQ.70) VARABL(INT(W1),4)=FTFL01(44,VBSURF)
                  IF(VALT.EQ.71) VARABL(INT(W1),4)=FTFL01(45,VBSURF)
                  IF(VALT.EQ.72) VARABL(INT(W1),4)=FTFL01(46,VBSURF)
                  IF(VALT.EQ.73) VARABL(INT(W1),4)=FTFL01(47,VBSURF)
                  IF(VALT.EQ.74) VARABL(INT(W1),4)=FTFL01(48,VBSURF)
                  IF(VALT.GE.76.AND.VALT.LE.123)
     1            VARABL(INT(W1),4)=FTFL01(VALT-27,VBSURF)
C
                  IF(VALT.EQ.27) VARABL(INT(W1),5)=FTFL01(1,VBSURF)
                  IF(VALT.EQ.28) VARABL(INT(W1),5)=FTFL01(2,VBSURF)
                  IF(VALT.EQ.29) VARABL(INT(W1),5)=FTFL01(3,VBSURF)
                  IF(VALT.EQ.30) VARABL(INT(W1),5)=FTFL01(4,VBSURF)
                  IF(VALT.EQ.31) VARABL(INT(W1),5)=FTFL01(5,VBSURF)
                  IF(VALT.EQ.32) VARABL(INT(W1),5)=FTFL01(6,VBSURF)
                  IF(VALT.EQ.33) VARABL(INT(W1),5)=FTFL01(7,VBSURF)
                  IF(VALT.EQ.34) VARABL(INT(W1),5)=FTFL01(8,VBSURF)
                  IF(VALT.EQ.35) VARABL(INT(W1),5)=FTFL01(9,VBSURF)
                  IF(VALT.EQ.36) VARABL(INT(W1),5)=FTFL01(10,VBSURF)
                  IF(VALT.EQ.37) VARABL(INT(W1),5)=FTFL01(11,VBSURF)
                  IF(VALT.EQ.38) VARABL(INT(W1),5)=FTFL01(12,VBSURF)
                  IF(VALT.EQ.39) VARABL(INT(W1),5)=FTFL01(13,VBSURF)
                  IF(VALT.EQ.40) VARABL(INT(W1),5)=FTFL01(14,VBSURF)
                  IF(VALT.EQ.41) VARABL(INT(W1),5)=FTFL01(15,VBSURF)
                  IF(VALT.EQ.42) VARABL(INT(W1),5)=FTFL01(16,VBSURF)
                  IF(VALT.EQ.43) VARABL(INT(W1),5)=FTFL01(17,VBSURF)
                  IF(VALT.EQ.44) VARABL(INT(W1),5)=FTFL01(18,VBSURF)
                  IF(VALT.EQ.45) VARABL(INT(W1),5)=FTFL01(19,VBSURF)
                  IF(VALT.EQ.46) VARABL(INT(W1),5)=FTFL01(20,VBSURF)
                  IF(VALT.EQ.47) VARABL(INT(W1),5)=FTFL01(21,VBSURF)
                  IF(VALT.EQ.48) VARABL(INT(W1),5)=FTFL01(22,VBSURF)
                  IF(VALT.EQ.49) VARABL(INT(W1),5)=FTFL01(23,VBSURF)
                  IF(VALT.EQ.50) VARABL(INT(W1),5)=FTFL01(24,VBSURF)
                  IF(VALT.EQ.51) VARABL(INT(W1),5)=FTFL01(25,VBSURF)
                  IF(VALT.EQ.52) VARABL(INT(W1),5)=FTFL01(26,VBSURF)
                  IF(VALT.EQ.53) VARABL(INT(W1),5)=FTFL01(27,VBSURF)
                  IF(VALT.EQ.54) VARABL(INT(W1),5)=FTFL01(28,VBSURF)
                  IF(VALT.EQ.55) VARABL(INT(W1),5)=FTFL01(29,VBSURF)
                  IF(VALT.EQ.56) VARABL(INT(W1),5)=FTFL01(30,VBSURF)
                  IF(VALT.EQ.57) VARABL(INT(W1),5)=FTFL01(31,VBSURF)
                  IF(VALT.EQ.58) VARABL(INT(W1),5)=FTFL01(32,VBSURF)
                  IF(VALT.EQ.59) VARABL(INT(W1),5)=FTFL01(33,VBSURF)
                  IF(VALT.EQ.60) VARABL(INT(W1),5)=FTFL01(34,VBSURF)
                  IF(VALT.EQ.61) VARABL(INT(W1),5)=FTFL01(35,VBSURF)
                  IF(VALT.EQ.62) VARABL(INT(W1),5)=FTFL01(36,VBSURF)
                  IF(VALT.EQ.63) VARABL(INT(W1),5)=FTFL01(37,VBSURF)
                  IF(VALT.EQ.64) VARABL(INT(W1),5)=FTFL01(38,VBSURF)
                  IF(VALT.EQ.65) VARABL(INT(W1),5)=FTFL01(39,VBSURF)
                  IF(VALT.EQ.66) VARABL(INT(W1),5)=FTFL01(40,VBSURF)
                  IF(VALT.EQ.67) VARABL(INT(W1),5)=FTFL01(41,VBSURF)
                  IF(VALT.EQ.68) VARABL(INT(W1),5)=FTFL01(42,VBSURF)
                  IF(VALT.EQ.69) VARABL(INT(W1),5)=FTFL01(43,VBSURF)
                  IF(VALT.EQ.70) VARABL(INT(W1),5)=FTFL01(44,VBSURF)
                  IF(VALT.EQ.71) VARABL(INT(W1),5)=FTFL01(45,VBSURF)
                  IF(VALT.EQ.72) VARABL(INT(W1),5)=FTFL01(46,VBSURF)
                  IF(VALT.EQ.73) VARABL(INT(W1),5)=FTFL01(47,VBSURF)
                  IF(VALT.EQ.74) VARABL(INT(W1),5)=FTFL01(48,VBSURF)
                  IF(VALT.GE.76.AND.VALT.LE.123)
     1            VARABL(INT(W1),5)=FTFL01(VALT-27,VBSURF)
                  IF(VALT.EQ.27) VARABL(INT(W1),13)=FTFL01(1,VBSURF)
                  IF(VALT.EQ.28) VARABL(INT(W1),13)=FTFL01(2,VBSURF)
                  IF(VALT.EQ.29) VARABL(INT(W1),13)=FTFL01(3,VBSURF)
                  IF(VALT.EQ.30) VARABL(INT(W1),13)=FTFL01(4,VBSURF)
                  IF(VALT.EQ.31) VARABL(INT(W1),13)=FTFL01(5,VBSURF)
                  IF(VALT.EQ.32) VARABL(INT(W1),13)=FTFL01(6,VBSURF)
                  IF(VALT.EQ.33) VARABL(INT(W1),13)=FTFL01(7,VBSURF)
                  IF(VALT.EQ.34) VARABL(INT(W1),13)=FTFL01(8,VBSURF)
                  IF(VALT.EQ.35) VARABL(INT(W1),13)=FTFL01(9,VBSURF)
                  IF(VALT.EQ.36) VARABL(INT(W1),13)=FTFL01(10,VBSURF)
                  IF(VALT.EQ.37) VARABL(INT(W1),13)=FTFL01(11,VBSURF)
                  IF(VALT.EQ.38) VARABL(INT(W1),13)=FTFL01(12,VBSURF)
                  IF(VALT.EQ.39) VARABL(INT(W1),13)=FTFL01(13,VBSURF)
                  IF(VALT.EQ.40) VARABL(INT(W1),13)=FTFL01(14,VBSURF)
                  IF(VALT.EQ.41) VARABL(INT(W1),13)=FTFL01(15,VBSURF)
                  IF(VALT.EQ.42) VARABL(INT(W1),13)=FTFL01(16,VBSURF)
                  IF(VALT.EQ.43) VARABL(INT(W1),13)=FTFL01(17,VBSURF)
                  IF(VALT.EQ.44) VARABL(INT(W1),13)=FTFL01(18,VBSURF)
                  IF(VALT.EQ.45) VARABL(INT(W1),13)=FTFL01(19,VBSURF)
                  IF(VALT.EQ.46) VARABL(INT(W1),13)=FTFL01(20,VBSURF)
                  IF(VALT.EQ.47) VARABL(INT(W1),13)=FTFL01(21,VBSURF)
                  IF(VALT.EQ.48) VARABL(INT(W1),13)=FTFL01(22,VBSURF)
                  IF(VALT.EQ.49) VARABL(INT(W1),13)=FTFL01(23,VBSURF)
                  IF(VALT.EQ.50) VARABL(INT(W1),13)=FTFL01(24,VBSURF)
                  IF(VALT.EQ.51) VARABL(INT(W1),13)=FTFL01(25,VBSURF)
                  IF(VALT.EQ.52) VARABL(INT(W1),13)=FTFL01(26,VBSURF)
                  IF(VALT.EQ.53) VARABL(INT(W1),13)=FTFL01(27,VBSURF)
                  IF(VALT.EQ.54) VARABL(INT(W1),13)=FTFL01(28,VBSURF)
                  IF(VALT.EQ.55) VARABL(INT(W1),13)=FTFL01(29,VBSURF)
                  IF(VALT.EQ.56) VARABL(INT(W1),13)=FTFL01(30,VBSURF)
                  IF(VALT.EQ.57) VARABL(INT(W1),13)=FTFL01(31,VBSURF)
                  IF(VALT.EQ.58) VARABL(INT(W1),13)=FTFL01(32,VBSURF)
                  IF(VALT.EQ.59) VARABL(INT(W1),13)=FTFL01(33,VBSURF)
                  IF(VALT.EQ.60) VARABL(INT(W1),13)=FTFL01(34,VBSURF)
                  IF(VALT.EQ.61) VARABL(INT(W1),13)=FTFL01(35,VBSURF)
                  IF(VALT.EQ.62) VARABL(INT(W1),13)=FTFL01(36,VBSURF)
                  IF(VALT.EQ.63) VARABL(INT(W1),13)=FTFL01(37,VBSURF)
                  IF(VALT.EQ.64) VARABL(INT(W1),13)=FTFL01(38,VBSURF)
                  IF(VALT.EQ.65) VARABL(INT(W1),13)=FTFL01(39,VBSURF)
                  IF(VALT.EQ.66) VARABL(INT(W1),13)=FTFL01(40,VBSURF)
                  IF(VALT.EQ.67) VARABL(INT(W1),13)=FTFL01(41,VBSURF)
                  IF(VALT.EQ.68) VARABL(INT(W1),13)=FTFL01(42,VBSURF)
                  IF(VALT.EQ.69) VARABL(INT(W1),13)=FTFL01(43,VBSURF)
                  IF(VALT.EQ.70) VARABL(INT(W1),13)=FTFL01(44,VBSURF)
                  IF(VALT.EQ.71) VARABL(INT(W1),13)=FTFL01(45,VBSURF)
                  IF(VALT.EQ.72) VARABL(INT(W1),13)=FTFL01(46,VBSURF)
                  IF(VALT.EQ.73) VARABL(INT(W1),13)=FTFL01(47,VBSURF)
                  IF(VALT.EQ.74) VARABL(INT(W1),13)=FTFL01(48,VBSURF)
                  IF(VALT.GE.76.AND.VALT.LE.123)
     1            VARABL(INT(W1),13)=FTFL01(VALT-27,VBSURF)
                  ISCOMP(INT(W1))=.TRUE.
C
              END IF
              RETURN
          END IF
C
C     DO AC
          IF(VALT.EQ.75) THEN
              IF(ALENS(1,VBSURF).NE.0.0D0) THEN
                  WRITE(OUTLYNE,*)
     1            'THE SPECIFIED SURFACE IS NOT PLANO'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'VARIABLE ASSIGNMENT WAS NOT MADE'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(ALENS(8,VBSURF).NE.1.0D0) ALENS(8,VBSURF)=1.0D0
C     PIKUP CHECK
C     IF A AC,PRO OR NPRO PIKUPS EXISTS THEN
              IF(PIKUP(1,VBSURF,26).EQ.1.0D0
     1        .OR.PIKUP(1,VBSURF,11).EQ.1.0D0
     1        .OR.PIKUP(1,VBSURF,12).EQ.1.0D0) THEN
C     A CV OR RD PIKUP EXISTS, DISSALLOW THE VARIABLE ASSIGNMENT
                  WRITE(OUTLYNE,*)
     1            'A CONFLICTING SURFACE SHAPE PIKUP EXISTS ON THIS SURFACE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,666) WC,INT(W2)
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'VARIABLE ASSIGNMENT WAS NOT MADE'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              ELSE
C     NO CONFLICTING PIKUP EXISTS, ASSIGN THE VARIABLE
C
                  IF(VALT.EQ.75) VARABL(INT(W1),4)=ALENS(43,VBSURF)
C
                  IF(VALT.EQ.75) VARABL(INT(W1),5)=ALENS(43,VBSURF)
                  IF(VALT.EQ.75) VARABL(INT(W1),13)=ALENS(43,VBSURF)
                  ISCOMP(INT(W1))=.TRUE.
              END IF
              RETURN
          END IF
          RETURN
C       ALL DONE
      END
C SUB CVCHECK.FOR
      SUBROUTINE CVCHECK
C
          IMPLICIT NONE
C
          LOGICAL YES,OLD(1:10)
C
          CHARACTER VNA*8
C
          REAL*8 CFER,JKVAR
C
          DIMENSION JKVAR(:,:)
C
          ALLOCATABLE JKVAR
C
          INTEGER I,J,JKVBCNT,ALLOERR
C
          INCLUDE 'datsub.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          INCLUDE 'datcfg.inc'
          DEALLOCATE(JKVAR,STAT=ALLOERR)
          ALLOCATE(JKVAR(MAXOPT,17),STAT=ALLOERR)
C
C     THIS READS THE CURRENT COMP VARIABLES LIST AND RE-SUBMITS THE COMMANDS
C     TO THE COMMAND PROCESSOR IF THE LENS HAS BEEN MODIFIED
C     WITH AN UPDATE LENS COMMAND OR AN UPDATE CONFIGS COMMAND
C     FROM THE KEYBOARD.
C
C       VARBLL(I,J) WHERE I COUNTS THE NUMBER OF VARIABLE ENTRIES
C       AND J TAKES ON THE FOLLOWING VALUES AND MEANIINGS.
C
C       J=1  > 1 THROUGH 142, A VARIABLE TYPE DESIGNATOR
C       J=2  > 1 ALWAYS = 1
C       J=3  > 0 THROUGH MAXSUR, THE SURFACE NUMBER DESIGNATOR
C       J=4  > VARIABLE CURRENT VALUE
C       J=5  > VARIABLE PREVIOUS VALUE
C       J=6  > LAST VARIABLE CHANGE VALUE
C       J=7  > WT, THE WEIGHTING FACTOR
C       J=8  > DINCR (DERIVATIVE CHANGE VALUE)
C       J=9  > LOW LIMIT VALUE
C       J=10 > HIGH LIMIT VALUE
C       J=11 > SEQUENTIAL IDENTIFIER IN THE AUXCFG FILES
C       J=12 > DEFAULT DINCR FLAG (1=DEFFAULT, 0=USER SET)
C       J=13 > ORIGINAL VALUE
C       J=14 > ENTRY NUMBER IN THE CFADD ARRAY THAT REFERS TO
C              THIS VARIABLE
C       J=15 > DF2
C       J=16 > DF4
C       J=17 > DF5
C     IF NO VARIABLES, JUST RETURN
          YES=.FALSE.
          DO I=1,MAXCMP
              IF(ISCOMP(I)) YES=.TRUE.
          END DO
          IF(.NOT.YES) CMPCNT=0
          IF(YES) CMPCNT=1
          IF(CMPCNT.EQ.0) DEALLOCATE(JKVAR,STAT=ALLOERR)
          IF(CMPCNT.EQ.0) RETURN
C     THERE WERE VARIABLES, PROCEED RE-ISSUING THE VARIABLES COMMANDS
          JKVBCNT=MAXCMP
          DO I=1,MAXCMP
              DO J=1,17
                  JKVAR(I,J)=VARABL(I,J)
              END DO
          END DO

          DO I=1,MAXCMP
              OLD(I)=ISCOMP(I)
          END DO
          SAVE_KDP(1)=SAVEINPT(1)
          INPUT='COMPVAR'
          CALL PROCES
          REST_KDP(1)=RESTINPT(1)
          CFER=1.0D0
          DO I=1,MAXCMP
              IF(OLD(I)) THEN
                  ISCOMP(I)=OLD(I)
                  VNA(1:4)='SNIT'
                  IF(JKVAR(I,1).EQ.1.0D0) VNA='CV'
                  IF(JKVAR(I,1).EQ.2.0D0) VNA='CV'
                  IF(JKVAR(I,1).EQ.3.0D0) VNA='TH'
                  IF(JKVAR(I,1).EQ.4.0D0) VNA='CC'
                  IF(JKVAR(I,1).EQ.5.0D0) VNA='AD'
                  IF(JKVAR(I,1).EQ.6.0D0) VNA='AE'
                  IF(JKVAR(I,1).EQ.7.0D0) VNA='AF'
                  IF(JKVAR(I,1).EQ.8.0D0) VNA='AG'
                  IF(JKVAR(I,1).EQ.9.0D0) VNA='CVTOR'
                  IF(JKVAR(I,1).EQ.10.0D0) VNA='CVTOR'
                  IF(JKVAR(I,1).EQ.11.0D0) VNA='CCTOR'
                  IF(JKVAR(I,1).EQ.12.0D0) VNA='ADTOR'
                  IF(JKVAR(I,1).EQ.13.0D0) VNA='AETOR'
                  IF(JKVAR(I,1).EQ.14.0D0) VNA='AFTOR'
                  IF(JKVAR(I,1).EQ.15.0D0) VNA='AGTOR'
                  IF(JKVAR(I,1).EQ.16.0D0) VNA='ALPHA'
                  IF(JKVAR(I,1).EQ.17.0D0) VNA='BETA'
                  IF(JKVAR(I,1).EQ.18.0D0) VNA='GAMMA'
                  IF(JKVAR(I,1).EQ.19.0D0) VNA='XD'
                  IF(JKVAR(I,1).EQ.20.0D0) VNA='YD'
                  IF(JKVAR(I,1).EQ.21.0D0) VNA='N1'
                  IF(JKVAR(I,1).EQ.22.0D0) VNA='N2'
                  IF(JKVAR(I,1).EQ.23.0D0) VNA='N3'
                  IF(JKVAR(I,1).EQ.24.0D0) VNA='N4'
                  IF(JKVAR(I,1).EQ.25.0D0) VNA='N5'
                  IF(JKVAR(I,1).EQ.27.0D0) VNA='C1'
                  IF(JKVAR(I,1).EQ.28.0D0) VNA='C2'
                  IF(JKVAR(I,1).EQ.29.0D0) VNA='C3'
                  IF(JKVAR(I,1).EQ.30.0D0) VNA='C4'
                  IF(JKVAR(I,1).EQ.31.0D0) VNA='C5'
                  IF(JKVAR(I,1).EQ.32.0D0) VNA='C6'
                  IF(JKVAR(I,1).EQ.33.0D0) VNA='C7'
                  IF(JKVAR(I,1).EQ.34.0D0) VNA='C8'
                  IF(JKVAR(I,1).EQ.35.0D0) VNA='C9'
                  IF(JKVAR(I,1).EQ.36.0D0) VNA='C10'
                  IF(JKVAR(I,1).EQ.37.0D0) VNA='C11'
                  IF(JKVAR(I,1).EQ.38.0D0) VNA='C12'
                  IF(JKVAR(I,1).EQ.39.0D0) VNA='C13'
                  IF(JKVAR(I,1).EQ.40.0D0) VNA='C14'
                  IF(JKVAR(I,1).EQ.41.0D0) VNA='C15'
                  IF(JKVAR(I,1).EQ.42.0D0) VNA='C16'
                  IF(JKVAR(I,1).EQ.43.0D0) VNA='C17'
                  IF(JKVAR(I,1).EQ.44.0D0) VNA='C18'
                  IF(JKVAR(I,1).EQ.45.0D0) VNA='C19'
                  IF(JKVAR(I,1).EQ.46.0D0) VNA='C20'
                  IF(JKVAR(I,1).EQ.47.0D0) VNA='C21'
                  IF(JKVAR(I,1).EQ.48.0D0) VNA='C22'
                  IF(JKVAR(I,1).EQ.49.0D0) VNA='C23'
                  IF(JKVAR(I,1).EQ.50.0D0) VNA='C24'
                  IF(JKVAR(I,1).EQ.51.0D0) VNA='C25'
                  IF(JKVAR(I,1).EQ.52.0D0) VNA='C26'
                  IF(JKVAR(I,1).EQ.53.0D0) VNA='C27'
                  IF(JKVAR(I,1).EQ.54.0D0) VNA='C28'
                  IF(JKVAR(I,1).EQ.55.0D0) VNA='C29'
                  IF(JKVAR(I,1).EQ.56.0D0) VNA='C30'
                  IF(JKVAR(I,1).EQ.57.0D0) VNA='C31'
                  IF(JKVAR(I,1).EQ.58.0D0) VNA='C32'
                  IF(JKVAR(I,1).EQ.59.0D0) VNA='C33'
                  IF(JKVAR(I,1).EQ.60.0D0) VNA='C34'
                  IF(JKVAR(I,1).EQ.61.0D0) VNA='C35'
                  IF(JKVAR(I,1).EQ.62.0D0) VNA='C36'
                  IF(JKVAR(I,1).EQ.63.0D0) VNA='C37'
                  IF(JKVAR(I,1).EQ.64.0D0) VNA='C38'
                  IF(JKVAR(I,1).EQ.65.0D0) VNA='C39'
                  IF(JKVAR(I,1).EQ.66.0D0) VNA='C40'
                  IF(JKVAR(I,1).EQ.67.0D0) VNA='C41'
                  IF(JKVAR(I,1).EQ.68.0D0) VNA='C42'
                  IF(JKVAR(I,1).EQ.69.0D0) VNA='C43'
                  IF(JKVAR(I,1).EQ.70.0D0) VNA='C44'
                  IF(JKVAR(I,1).EQ.71.0D0) VNA='C45'
                  IF(JKVAR(I,1).EQ.72.0D0) VNA='C46'
                  IF(JKVAR(I,1).EQ.73.0D0) VNA='C47'
                  IF(JKVAR(I,1).EQ.74.0D0) VNA='C48'
                  IF(JKVAR(I,1).EQ.75.0D0) VNA='AC'
                  IF(JKVAR(I,1).EQ.76.0D0) VNA='C49'
                  IF(JKVAR(I,1).EQ.77.0D0) VNA='C50'
                  IF(JKVAR(I,1).EQ.78.0D0) VNA='C51'
                  IF(JKVAR(I,1).EQ.79.0D0) VNA='C52'
                  IF(JKVAR(I,1).EQ.80.0D0) VNA='C53'
                  IF(JKVAR(I,1).EQ.81.0D0) VNA='C54'
                  IF(JKVAR(I,1).EQ.82.0D0) VNA='C55'
                  IF(JKVAR(I,1).EQ.83.0D0) VNA='C56'
                  IF(JKVAR(I,1).EQ.84.0D0) VNA='C57'
                  IF(JKVAR(I,1).EQ.85.0D0) VNA='C58'
                  IF(JKVAR(I,1).EQ.86.0D0) VNA='C59'
                  IF(JKVAR(I,1).EQ.87.0D0) VNA='C60'
                  IF(JKVAR(I,1).EQ.88.0D0) VNA='C61'
                  IF(JKVAR(I,1).EQ.89.0D0) VNA='C62'
                  IF(JKVAR(I,1).EQ.90.0D0) VNA='C63'
                  IF(JKVAR(I,1).EQ.91.0D0) VNA='C64'
                  IF(JKVAR(I,1).EQ.92.0D0) VNA='C65'
                  IF(JKVAR(I,1).EQ.93.0D0) VNA='C66'
                  IF(JKVAR(I,1).EQ.94.0D0) VNA='C67'
                  IF(JKVAR(I,1).EQ.95.0D0) VNA='C68'
                  IF(JKVAR(I,1).EQ.96.0D0) VNA='C69'
                  IF(JKVAR(I,1).EQ.97.0D0) VNA='C70'
                  IF(JKVAR(I,1).EQ.98.0D0) VNA='C71'
                  IF(JKVAR(I,1).EQ.99.0D0) VNA='C72'
                  IF(JKVAR(I,1).EQ.100.0D0) VNA='C73'
                  IF(JKVAR(I,1).EQ.101.0D0) VNA='C74'
                  IF(JKVAR(I,1).EQ.102.0D0) VNA='C75'
                  IF(JKVAR(I,1).EQ.103.0D0) VNA='C76'
                  IF(JKVAR(I,1).EQ.104.0D0) VNA='C77'
                  IF(JKVAR(I,1).EQ.105.0D0) VNA='C78'
                  IF(JKVAR(I,1).EQ.106.0D0) VNA='C79'
                  IF(JKVAR(I,1).EQ.107.0D0) VNA='C80'
                  IF(JKVAR(I,1).EQ.108.0D0) VNA='C81'
                  IF(JKVAR(I,1).EQ.109.0D0) VNA='C82'
                  IF(JKVAR(I,1).EQ.110.0D0) VNA='C83'
                  IF(JKVAR(I,1).EQ.111.0D0) VNA='C84'
                  IF(JKVAR(I,1).EQ.112.0D0) VNA='C85'
                  IF(JKVAR(I,1).EQ.113.0D0) VNA='C86'
                  IF(JKVAR(I,1).EQ.114.0D0) VNA='C87'
                  IF(JKVAR(I,1).EQ.115.0D0) VNA='C88'
                  IF(JKVAR(I,1).EQ.116.0D0) VNA='C89'
                  IF(JKVAR(I,1).EQ.117.0D0) VNA='C90'
                  IF(JKVAR(I,1).EQ.118.0D0) VNA='C91'
                  IF(JKVAR(I,1).EQ.119.0D0) VNA='C92'
                  IF(JKVAR(I,1).EQ.120.0D0) VNA='C93'
                  IF(JKVAR(I,1).EQ.121.0D0) VNA='C94'
                  IF(JKVAR(I,1).EQ.122.0D0) VNA='C95'
                  IF(JKVAR(I,1).EQ.123.0D0) VNA='C96'
                  IF(JKVAR(I,1).EQ.124.0D0) VNA='N6'
                  IF(JKVAR(I,1).EQ.125.0D0) VNA='N7'
                  IF(JKVAR(I,1).EQ.126.0D0) VNA='N8'
                  IF(JKVAR(I,1).EQ.127.0D0) VNA='N9'
                  IF(JKVAR(I,1).EQ.128.0D0) VNA='N10'
                  IF(JKVAR(I,1).EQ.129.0D0) VNA='AH'
                  IF(JKVAR(I,1).EQ.131.0D0) VNA='AI'
                  IF(JKVAR(I,1).EQ.131.0D0) VNA='AJ'
                  IF(JKVAR(I,1).EQ.132.0D0) VNA='AK'
                  IF(JKVAR(I,1).EQ.133.0D0) VNA='AL'
                  IF(JKVAR(I,1).EQ.134.0D0) VNA='ZD'
                  IF(JKVAR(I,1).EQ.135.0D0) VNA='INDEX'
                  IF(JKVAR(I,1).EQ.136.0D0) VNA='VNUM'
                  IF(JKVAR(I,1).EQ.137.0D0) VNA='PIVX'
                  IF(JKVAR(I,1).EQ.138.0D0) VNA='PIVY'
                  IF(JKVAR(I,1).EQ.139.0D0) VNA='PIVZ'
                  IF(JKVAR(I,1).EQ.140.0D0) VNA='DPART'
                  IF(JKVAR(I,1).EQ.141.0D0) VNA='CLPX '
                  IF(JKVAR(I,1).EQ.142.0D0) VNA='CLPY '
                  IF(JKVAR(I,1).EQ.143.0D0) VNA='GDX  '
                  IF(JKVAR(I,1).EQ.144.0D0) VNA='GDY  '
                  IF(JKVAR(I,1).EQ.145.0D0) VNA='GDZ  '
                  IF(JKVAR(I,1).EQ.146.0D0) VNA='GALPHA'
                  IF(JKVAR(I,1).EQ.147.0D0) VNA='GBETA '
                  IF(JKVAR(I,1).EQ.148.0D0) VNA='GGAMMA'
                  IF(JKVAR(I,1).EQ.149.0D0) VNA='GRS'
                  IF(VNA(1:4).NE.'SNIT') THEN
                      SAVE_KDP(1)=SAVEINPT(1)
                      WC=VNA
                      W1=JKVAR(I,11)
                      W2=JKVAR(I,3)
                      W3=JKVAR(I,8)
                      S1=1
                      S2=1
                      S3=1
                      S4=0
                      S5=0
                      DF1=0
                      DF2=0
                      DF3=0
                      DF4=1
                      DF5=1
                      SN=1
                      SQ=0
                      SST=0
                      SSI=0
                      CALL CONTRO
                      REST_KDP(1)=RESTINPT(1)
                  ELSE
                      ISCOMP(I)=.FALSE.
                  END IF
              END IF
          END DO
          SAVE_KDP(1)=SAVEINPT(1)
          INPUT='EOS'
          CALL PROCES
          REST_KDP(1)=RESTINPT(1)
          F52=0
          DEALLOCATE(JKVAR,STAT=ALLOERR)
          RETURN
      END
C SUB DINCIT.FOR
      SUBROUTINE DINCIT
C
          IMPLICIT NONE
C
          LOGICAL DINO
C
          REAL*8 D
C
          INCLUDE 'datsub.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          INCLUDE 'datcfg.inc'
C
          IF(STI.EQ.1.AND.SQ.EQ.0) THEN
              WRITE(OUTLYNE,*)
     1        '"DINCR" SETS DEFAULT DINCR VALUES FOR CLASSES OF VARIABLES'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)
     1        'AND TAKES QUALIFIER AND NUMERIC WORD #1 INPUT'
              CALL SHOWIT(1)
              RETURN
          END IF
          IF(STI.EQ.1.AND.SQ.EQ.1) THEN
              IF(WQ.EQ.'CV      ') D=DINC2
              IF(WQ.EQ.'TH      ') D=DINC3
              IF(WQ.EQ.'CLPX    ') D=DINC3
              IF(WQ.EQ.'CLPY    ') D=DINC3
              IF(WQ.EQ.'CC      ') D=DINC4
              IF(WQ.EQ.'AD      ') D=DINC5A
              IF(WQ.EQ.'AE      ') D=DINC6A
              IF(WQ.EQ.'AF      ') D=DINC7A
              IF(WQ.EQ.'AG      ') D=DINC8A
              IF(WQ.EQ.'AH      ') D=DINC9A
              IF(WQ.EQ.'AI      ') D=DINC10A
              IF(WQ.EQ.'AJ      ') D=DINC11A
              IF(WQ.EQ.'AK      ') D=DINC12A
              IF(WQ.EQ.'AL      ') D=DINC13A
              IF(WQ.EQ.'AC      ') D=DINC14A
              IF(WQ.EQ.'AD2     ') D=DINC5B
              IF(WQ.EQ.'AE2     ') D=DINC6B
              IF(WQ.EQ.'AF2     ') D=DINC7B
              IF(WQ.EQ.'AG2     ') D=DINC8B
              IF(WQ.EQ.'AH2     ') D=DINC9B
              IF(WQ.EQ.'AI2     ') D=DINC10B
              IF(WQ.EQ.'AJ2     ') D=DINC11B
              IF(WQ.EQ.'AK2     ') D=DINC12B
              IF(WQ.EQ.'AL2     ') D=DINC13B
              IF(WQ.EQ.'AC2     ') D=DINC14B
              IF(WQ.EQ.'CVTOR   ') D=DINC16
              IF(WQ.EQ.'CCTOR   ') D=DINC17
              IF(WQ.EQ.'ADTOR   ') D=DINC18A
              IF(WQ.EQ.'AETOR   ') D=DINC19A
              IF(WQ.EQ.'AFTOR   ') D=DINC20A
              IF(WQ.EQ.'AGTOR   ') D=DINC21A
              IF(WQ.EQ.'ADTOR2  ') D=DINC18B
              IF(WQ.EQ.'AETOR2  ') D=DINC19B
              IF(WQ.EQ.'AFTOR2  ') D=DINC20B
              IF(WQ.EQ.'AGTOR2  ') D=DINC21B
              IF(WQ.EQ.'ALPHA   ') D=DINC22
              IF(WQ.EQ.'BETA    ') D=DINC23
              IF(WQ.EQ.'GAMMA   ') D=DINC24
              IF(WQ.EQ.'GALPHA  ') D=DINC22
              IF(WQ.EQ.'GBETA   ') D=DINC23
              IF(WQ.EQ.'GGAMMA  ') D=DINC24
              IF(WQ.EQ.'XD      ') D=DINC25
              IF(WQ.EQ.'YD      ') D=DINC26
              IF(WQ.EQ.'ZD      ') D=DINC29
              IF(WQ.EQ.'GDX     ') D=DINC25
              IF(WQ.EQ.'GDY     ') D=DINC26
              IF(WQ.EQ.'GDZ     ') D=DINC29
              IF(WQ.EQ.'PIVX    ') D=DINC25
              IF(WQ.EQ.'PIVY    ') D=DINC26
              IF(WQ.EQ.'PIVZ    ') D=DINC29
              IF(WQ.EQ.'N1      ') D=DINC27
              IF(WQ.EQ.'N2      ') D=DINC27
              IF(WQ.EQ.'N3      ') D=DINC27
              IF(WQ.EQ.'N4      ') D=DINC27
              IF(WQ.EQ.'N5      ') D=DINC27
              IF(WQ.EQ.'N6      ') D=DINC27
              IF(WQ.EQ.'N7      ') D=DINC27
              IF(WQ.EQ.'N8      ') D=DINC27
              IF(WQ.EQ.'N9      ') D=DINC27
              IF(WQ.EQ.'N10     ') D=DINC27
              IF(WQ.EQ.'INDEX   ') D=DINC27
              IF(WQ.EQ.'VNUM    ') D=DINC27
              IF(WQ.EQ.'DPART   ') D=DINC27
              IF(WQ.EQ.'C1      ') D=DINC28
              IF(WQ.EQ.'C2      ') D=DINC28
              IF(WQ.EQ.'C3      ') D=DINC28
              IF(WQ.EQ.'C4      ') D=DINC28
              IF(WQ.EQ.'C5      ') D=DINC28
              IF(WQ.EQ.'C6      ') D=DINC28
              IF(WQ.EQ.'C7      ') D=DINC28
              IF(WQ.EQ.'C8      ') D=DINC28
              IF(WQ.EQ.'C9      ') D=DINC28
              IF(WQ.EQ.'C10     ') D=DINC28
              IF(WQ.EQ.'C11     ') D=DINC28
              IF(WQ.EQ.'C12     ') D=DINC28
              IF(WQ.EQ.'C13     ') D=DINC28
              IF(WQ.EQ.'C14     ') D=DINC28
              IF(WQ.EQ.'C15     ') D=DINC28
              IF(WQ.EQ.'C16     ') D=DINC28
              IF(WQ.EQ.'C17     ') D=DINC28
              IF(WQ.EQ.'C18     ') D=DINC28
              IF(WQ.EQ.'C19     ') D=DINC28
              IF(WQ.EQ.'C20     ') D=DINC28
              IF(WQ.EQ.'C21     ') D=DINC28
              IF(WQ.EQ.'C22     ') D=DINC28
              IF(WQ.EQ.'C23     ') D=DINC28
              IF(WQ.EQ.'C24     ') D=DINC28
              IF(WQ.EQ.'C25     ') D=DINC28
              IF(WQ.EQ.'C26     ') D=DINC28
              IF(WQ.EQ.'C27     ') D=DINC28
              IF(WQ.EQ.'C28     ') D=DINC28
              IF(WQ.EQ.'C29     ') D=DINC28
              IF(WQ.EQ.'C30     ') D=DINC28
              IF(WQ.EQ.'C31     ') D=DINC28
              IF(WQ.EQ.'C32     ') D=DINC28
              IF(WQ.EQ.'C33     ') D=DINC28
              IF(WQ.EQ.'C34     ') D=DINC28
              IF(WQ.EQ.'C35     ') D=DINC28
              IF(WQ.EQ.'C36     ') D=DINC28
              IF(WQ.EQ.'C37     ') D=DINC28
              IF(WQ.EQ.'C38     ') D=DINC28
              IF(WQ.EQ.'C39     ') D=DINC28
              IF(WQ.EQ.'C40     ') D=DINC28
              IF(WQ.EQ.'C41     ') D=DINC28
              IF(WQ.EQ.'C42     ') D=DINC28
              IF(WQ.EQ.'C43     ') D=DINC28
              IF(WQ.EQ.'C44     ') D=DINC28
              IF(WQ.EQ.'C45     ') D=DINC28
              IF(WQ.EQ.'C46     ') D=DINC28
              IF(WQ.EQ.'C47     ') D=DINC28
              IF(WQ.EQ.'C48     ') D=DINC28
              IF(WQ.EQ.'C49     ') D=DINC28
              IF(WQ.EQ.'C50     ') D=DINC28
              IF(WQ.EQ.'C51     ') D=DINC28
              IF(WQ.EQ.'C52     ') D=DINC28
              IF(WQ.EQ.'C53     ') D=DINC28
              IF(WQ.EQ.'C54     ') D=DINC28
              IF(WQ.EQ.'C55     ') D=DINC28
              IF(WQ.EQ.'C56     ') D=DINC28
              IF(WQ.EQ.'C57     ') D=DINC28
              IF(WQ.EQ.'C58     ') D=DINC28
              IF(WQ.EQ.'C59     ') D=DINC28
              IF(WQ.EQ.'C60     ') D=DINC28
              IF(WQ.EQ.'C61     ') D=DINC28
              IF(WQ.EQ.'C62     ') D=DINC28
              IF(WQ.EQ.'C63     ') D=DINC28
              IF(WQ.EQ.'C64     ') D=DINC28
              IF(WQ.EQ.'C65     ') D=DINC28
              IF(WQ.EQ.'C66     ') D=DINC28
              IF(WQ.EQ.'C67     ') D=DINC28
              IF(WQ.EQ.'C68     ') D=DINC28
              IF(WQ.EQ.'C69     ') D=DINC28
              IF(WQ.EQ.'C70     ') D=DINC28
              IF(WQ.EQ.'C71     ') D=DINC28
              IF(WQ.EQ.'C72     ') D=DINC28
              IF(WQ.EQ.'C73     ') D=DINC28
              IF(WQ.EQ.'C74     ') D=DINC28
              IF(WQ.EQ.'C75     ') D=DINC28
              IF(WQ.EQ.'C76     ') D=DINC28
              IF(WQ.EQ.'C77     ') D=DINC28
              IF(WQ.EQ.'C78     ') D=DINC28
              IF(WQ.EQ.'C79     ') D=DINC28
              IF(WQ.EQ.'C80     ') D=DINC28
              IF(WQ.EQ.'C81     ') D=DINC28
              IF(WQ.EQ.'C82     ') D=DINC28
              IF(WQ.EQ.'C83     ') D=DINC28
              IF(WQ.EQ.'C84     ') D=DINC28
              IF(WQ.EQ.'C85     ') D=DINC28
              IF(WQ.EQ.'C86     ') D=DINC28
              IF(WQ.EQ.'C87     ') D=DINC28
              IF(WQ.EQ.'C88     ') D=DINC28
              IF(WQ.EQ.'C89     ') D=DINC28
              IF(WQ.EQ.'C90     ') D=DINC28
              IF(WQ.EQ.'C91     ') D=DINC28
              IF(WQ.EQ.'C92     ') D=DINC28
              IF(WQ.EQ.'C93     ') D=DINC28
              IF(WQ.EQ.'C94     ') D=DINC28
              IF(WQ.EQ.'C95     ') D=DINC28
              IF(WQ.EQ.'C96     ') D=DINC28
              IF(WQ.EQ.'GRS     ') D=DINC28
              IF(WQ.EQ.'MACVAR  ') D=DINC28
              IF(WQ(1:3).EQ.'ACT') D=DINC30
              IF(WQ.EQ.'P001') D=NSSDINC(1)
              IF(WQ.EQ.'P002') D=NSSDINC(2)
              IF(WQ.EQ.'P003') D=NSSDINC(3)
              IF(WQ.EQ.'P004') D=NSSDINC(4)
              IF(WQ.EQ.'P005') D=NSSDINC(5)
              IF(WQ.EQ.'P006') D=NSSDINC(6)
              IF(WQ.EQ.'P007') D=NSSDINC(7)
              IF(WQ.EQ.'P008') D=NSSDINC(8)
              IF(WQ.EQ.'P009') D=NSSDINC(9)
              IF(WQ.EQ.'P010') D=NSSDINC(10)
              IF(WQ.EQ.'P011') D=NSSDINC(11)
              IF(WQ.EQ.'P012') D=NSSDINC(12)
              IF(WQ.EQ.'P013') D=NSSDINC(13)
              IF(WQ.EQ.'P014') D=NSSDINC(14)
              IF(WQ.EQ.'P015') D=NSSDINC(15)
              IF(WQ.EQ.'P016') D=NSSDINC(16)
              IF(WQ.EQ.'P017') D=NSSDINC(17)
              IF(WQ.EQ.'P018') D=NSSDINC(18)
              IF(WQ.EQ.'P019') D=NSSDINC(19)
              IF(WQ.EQ.'P020') D=NSSDINC(20)
              IF(WQ.EQ.'P021') D=NSSDINC(21)
              IF(WQ.EQ.'P022') D=NSSDINC(22)
              IF(WQ.EQ.'P023') D=NSSDINC(23)
              IF(WQ.EQ.'P024') D=NSSDINC(24)
              IF(WQ.EQ.'P025') D=NSSDINC(25)
              IF(WQ.EQ.'P026') D=NSSDINC(26)
              IF(WQ.EQ.'P027') D=NSSDINC(27)
              IF(WQ.EQ.'P028') D=NSSDINC(28)
              IF(WQ.EQ.'P029') D=NSSDINC(29)
              IF(WQ.EQ.'P030') D=NSSDINC(30)
              IF(WQ.EQ.'P031') D=NSSDINC(31)
              IF(WQ.EQ.'P032') D=NSSDINC(32)
              IF(WQ.EQ.'P033') D=NSSDINC(33)
              IF(WQ.EQ.'P034') D=NSSDINC(34)
              IF(WQ.EQ.'P035') D=NSSDINC(35)
              IF(WQ.EQ.'P036') D=NSSDINC(36)
              IF(WQ.EQ.'P037') D=NSSDINC(37)
              IF(WQ.EQ.'P038') D=NSSDINC(38)
              IF(WQ.EQ.'P039') D=NSSDINC(39)
              IF(WQ.EQ.'P040') D=NSSDINC(40)
              IF(WQ.EQ.'P041') D=NSSDINC(41)
              IF(WQ.EQ.'P042') D=NSSDINC(42)
              IF(WQ.EQ.'P043') D=NSSDINC(43)
              IF(WQ.EQ.'P044') D=NSSDINC(44)
              IF(WQ.EQ.'P045') D=NSSDINC(45)
              IF(WQ.EQ.'P046') D=NSSDINC(46)
              IF(WQ.EQ.'P047') D=NSSDINC(47)
              IF(WQ.EQ.'P048') D=NSSDINC(48)
              IF(WQ.EQ.'P049') D=NSSDINC(49)
              IF(WQ.EQ.'P050') D=NSSDINC(50)
              IF(WQ.EQ.'P051') D=NSSDINC(51)
              IF(WQ.EQ.'P052') D=NSSDINC(52)
              IF(WQ.EQ.'P053') D=NSSDINC(53)
              IF(WQ.EQ.'P054') D=NSSDINC(54)
              IF(WQ.EQ.'P055') D=NSSDINC(55)
              IF(WQ.EQ.'P056') D=NSSDINC(56)
              IF(WQ.EQ.'P057') D=NSSDINC(57)
              IF(WQ.EQ.'P058') D=NSSDINC(58)
              IF(WQ.EQ.'P059') D=NSSDINC(59)
              IF(WQ.EQ.'P060') D=NSSDINC(60)
              IF(WQ.EQ.'P061') D=NSSDINC(61)
              IF(WQ.EQ.'P062') D=NSSDINC(62)
              IF(WQ.EQ.'P063') D=NSSDINC(63)
              IF(WQ.EQ.'P064') D=NSSDINC(64)
              IF(WQ.EQ.'P065') D=NSSDINC(65)
              IF(WQ.EQ.'P066') D=NSSDINC(66)
              IF(WQ.EQ.'P067') D=NSSDINC(67)
              IF(WQ.EQ.'P068') D=NSSDINC(68)
              IF(WQ.EQ.'P069') D=NSSDINC(69)
              IF(WQ.EQ.'P070') D=NSSDINC(70)
              IF(WQ.EQ.'P071') D=NSSDINC(71)
              IF(WQ.EQ.'P072') D=NSSDINC(72)
              IF(WQ.EQ.'P073') D=NSSDINC(73)
              IF(WQ.EQ.'P074') D=NSSDINC(74)
              IF(WQ.EQ.'P075') D=NSSDINC(75)
              IF(WQ.EQ.'P076') D=NSSDINC(76)
              IF(WQ.EQ.'P077') D=NSSDINC(77)
              IF(WQ.EQ.'P078') D=NSSDINC(78)
              IF(WQ.EQ.'P079') D=NSSDINC(79)
              IF(WQ.EQ.'P080') D=NSSDINC(80)
              IF(WQ.EQ.'P081') D=NSSDINC(81)
              IF(WQ.EQ.'P082') D=NSSDINC(82)
              IF(WQ.EQ.'P083') D=NSSDINC(81)
              IF(WQ.EQ.'P084') D=NSSDINC(81)
              IF(WQ.EQ.'P085') D=NSSDINC(85)
              IF(WQ.EQ.'P086') D=NSSDINC(86)
              IF(WQ.EQ.'P087') D=NSSDINC(87)
              IF(WQ.EQ.'P088') D=NSSDINC(88)
              IF(WQ.EQ.'P089') D=NSSDINC(89)
              IF(WQ.EQ.'P090') D=NSSDINC(90)
              IF(WQ.EQ.'P091') D=NSSDINC(91)
              IF(WQ.EQ.'P092') D=NSSDINC(92)
              IF(WQ.EQ.'P093') D=NSSDINC(93)
              IF(WQ.EQ.'P094') D=NSSDINC(94)
              IF(WQ.EQ.'P095') D=NSSDINC(95)
              IF(WQ.EQ.'P096') D=NSSDINC(96)
              IF(WQ.EQ.'P097') D=NSSDINC(97)
              IF(WQ.EQ.'P098') D=NSSDINC(98)
              IF(WQ.EQ.'P099') D=NSSDINC(99)
              IF(WQ.EQ.'P100') D=NSSDINC(100)
              IF(WQ.EQ.'P101') D=NSSDINC(101)
              IF(WQ.EQ.'P102') D=NSSDINC(102)
              IF(WQ.EQ.'P103') D=NSSDINC(103)
              IF(WQ.EQ.'P104') D=NSSDINC(104)
              IF(WQ.EQ.'P105') D=NSSDINC(105)
              IF(WQ.EQ.'P106') D=NSSDINC(106)
              IF(WQ.EQ.'P107') D=NSSDINC(107)
              IF(WQ.EQ.'P108') D=NSSDINC(108)
              IF(WQ.EQ.'P109') D=NSSDINC(109)
              IF(WQ.EQ.'P110') D=NSSDINC(110)
              IF(WQ.EQ.'P111') D=NSSDINC(111)
              IF(WQ.EQ.'P112') D=NSSDINC(112)
              IF(WQ.EQ.'P113') D=NSSDINC(113)
              IF(WQ.EQ.'P114') D=NSSDINC(114)
              IF(WQ.EQ.'P115') D=NSSDINC(115)
              IF(WQ.EQ.'P116') D=NSSDINC(116)
              IF(WQ.EQ.'P117') D=NSSDINC(117)
              IF(WQ.EQ.'P118') D=NSSDINC(118)
              IF(WQ.EQ.'P119') D=NSSDINC(119)
              IF(WQ.EQ.'P120') D=NSSDINC(120)
              IF(WQ.EQ.'P121') D=NSSDINC(121)
              IF(WQ.EQ.'P122') D=NSSDINC(122)
              IF(WQ.EQ.'P123') D=NSSDINC(123)
              IF(WQ.EQ.'P124') D=NSSDINC(124)
              IF(WQ.EQ.'P125') D=NSSDINC(125)
              IF(WQ.EQ.'P126') D=NSSDINC(126)
              IF(WQ.EQ.'P127') D=NSSDINC(127)
              IF(WQ.EQ.'P128') D=NSSDINC(128)
              IF(WQ.EQ.'P129') D=NSSDINC(129)
              IF(WQ.EQ.'P130') D=NSSDINC(130)
              IF(WQ.EQ.'P131') D=NSSDINC(131)
              IF(WQ.EQ.'P132') D=NSSDINC(132)
              IF(WQ.EQ.'P133') D=NSSDINC(133)
              IF(WQ.EQ.'P134') D=NSSDINC(134)
              IF(WQ.EQ.'P135') D=NSSDINC(135)
              IF(WQ.EQ.'P136') D=NSSDINC(136)
              IF(WQ.EQ.'P137') D=NSSDINC(137)
              IF(WQ.EQ.'P138') D=NSSDINC(138)
              IF(WQ.EQ.'P139') D=NSSDINC(139)
              IF(WQ.EQ.'P140') D=NSSDINC(140)
              IF(WQ.EQ.'P141') D=NSSDINC(141)
              IF(WQ.EQ.'P142') D=NSSDINC(142)
              IF(WQ.EQ.'P143') D=NSSDINC(143)
              IF(WQ.EQ.'P144') D=NSSDINC(144)
              IF(WQ.EQ.'P145') D=NSSDINC(145)
              IF(WQ.EQ.'P146') D=NSSDINC(146)
              IF(WQ.EQ.'P147') D=NSSDINC(147)
              IF(WQ.EQ.'P148') D=NSSDINC(148)
              IF(WQ.EQ.'P149') D=NSSDINC(149)
              IF(WQ.EQ.'P150') D=NSSDINC(150)
              IF(WQ.EQ.'P151') D=NSSDINC(151)
              IF(WQ.EQ.'P152') D=NSSDINC(152)
              IF(WQ.EQ.'P153') D=NSSDINC(153)
              IF(WQ.EQ.'P154') D=NSSDINC(154)
              IF(WQ.EQ.'P155') D=NSSDINC(155)
              IF(WQ.EQ.'P156') D=NSSDINC(156)
              IF(WQ.EQ.'P157') D=NSSDINC(157)
              IF(WQ.EQ.'P158') D=NSSDINC(158)
              IF(WQ.EQ.'P159') D=NSSDINC(159)
              IF(WQ.EQ.'P160') D=NSSDINC(160)
              IF(WQ.EQ.'P161') D=NSSDINC(161)
              IF(WQ.EQ.'P162') D=NSSDINC(162)
              IF(WQ.EQ.'P163') D=NSSDINC(163)
              IF(WQ.EQ.'P164') D=NSSDINC(164)
              IF(WQ.EQ.'P165') D=NSSDINC(165)
              IF(WQ.EQ.'P166') D=NSSDINC(166)
              IF(WQ.EQ.'P167') D=NSSDINC(167)
              IF(WQ.EQ.'P168') D=NSSDINC(168)
              IF(WQ.EQ.'P169') D=NSSDINC(169)
              IF(WQ.EQ.'P170') D=NSSDINC(170)
              IF(WQ.EQ.'P171') D=NSSDINC(171)
              IF(WQ.EQ.'P172') D=NSSDINC(172)
              IF(WQ.EQ.'P173') D=NSSDINC(173)
              IF(WQ.EQ.'P174') D=NSSDINC(174)
              IF(WQ.EQ.'P175') D=NSSDINC(175)
              IF(WQ.EQ.'P176') D=NSSDINC(176)
              IF(WQ.EQ.'P177') D=NSSDINC(177)
              IF(WQ.EQ.'P178') D=NSSDINC(178)
              IF(WQ.EQ.'P179') D=NSSDINC(179)
              IF(WQ.EQ.'P180') D=NSSDINC(180)
              IF(WQ.EQ.'P181') D=NSSDINC(181)
              IF(WQ.EQ.'P182') D=NSSDINC(182)
              IF(WQ.EQ.'P183') D=NSSDINC(181)
              IF(WQ.EQ.'P184') D=NSSDINC(181)
              IF(WQ.EQ.'P185') D=NSSDINC(185)
              IF(WQ.EQ.'P186') D=NSSDINC(186)
              IF(WQ.EQ.'P187') D=NSSDINC(187)
              IF(WQ.EQ.'P188') D=NSSDINC(188)
              IF(WQ.EQ.'P189') D=NSSDINC(189)
              IF(WQ.EQ.'P190') D=NSSDINC(190)
              IF(WQ.EQ.'P191') D=NSSDINC(191)
              IF(WQ.EQ.'P192') D=NSSDINC(192)
              IF(WQ.EQ.'P193') D=NSSDINC(193)
              IF(WQ.EQ.'P194') D=NSSDINC(194)
              IF(WQ.EQ.'P195') D=NSSDINC(195)
              IF(WQ.EQ.'P196') D=NSSDINC(196)
              IF(WQ.EQ.'P197') D=NSSDINC(197)
              IF(WQ.EQ.'P198') D=NSSDINC(198)
              IF(WQ.EQ.'P199') D=NSSDINC(199)
              IF(WQ.EQ.'P200') D=NSSDINC(200)
              IF(WQ.EQ.'NSSXPOS') D=NSSDINC(201)
              IF(WQ.EQ.'NSSYPOS') D=NSSDINC(202)
              IF(WQ.EQ.'NSSZPOS') D=NSSDINC(203)
              IF(WQ.EQ.'NSSALPH') D=NSSDINC(204)
              IF(WQ.EQ.'NSSBETA') D=NSSDINC(205)
              IF(WQ.EQ.'NSSGAMM') D=NSSDINC(206)
              IF(WQ.EQ.'V1') D=NSSDINC(207)
              IF(WQ.EQ.'V2') D=NSSDINC(208)
              IF(WQ.EQ.'V3') D=NSSDINC(209)
              IF(WQ.EQ.'V4') D=NSSDINC(210)
              IF(WQ.EQ.'V5') D=NSSDINC(211)
 654          FORMAT('DINCR   ',A8,' = ',D23.15)
              WRITE(OUTLYNE,654) WQ,D
              CALL SHOWIT(1)
              RETURN
          END IF

          IF(SST.EQ.1.OR.S2.EQ.1.OR.3.EQ.1.OR.S4.EQ.4.OR.S5.EQ.1) THEN
              WRITE(OUTLYNE,*)
     1        '"DINCR" ONLY TAKES QUALIFIER AND NUMERIC WORD #1 INPUT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)
     1        'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SQ.EQ.0.OR.S1.EQ.0) THEN
              WRITE(OUTLYNE,*)
     1        '"DINCR" REQUIRES EXPLICIT QUALIFIER AND NUMERIC WORD #1 INPUT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)
     1        'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
C     DEFAULT INPUT
          DINO=.FALSE.
          IF(WQ.EQ.'CV      ') DINC2=W1
          IF(WQ.EQ.'TH      ') DINC3=W1
          IF(WQ.EQ.'CLPX    ') DINC3=W1
          IF(WQ.EQ.'CLPY    ') DINC3=W1
          IF(WQ.EQ.'CC      ') DINC4=W1
          IF(WQ.EQ.'AD      ') DINC5A=W1
          IF(WQ.EQ.'AE      ') DINC6A=W1
          IF(WQ.EQ.'AF      ') DINC7A=W1
          IF(WQ.EQ.'AG      ') DINC8A=W1
          IF(WQ.EQ.'AH      ') DINC9A=W1
          IF(WQ.EQ.'AI      ') DINC10A=W1
          IF(WQ.EQ.'AJ      ') DINC11A=W1
          IF(WQ.EQ.'AK      ') DINC12A=W1
          IF(WQ.EQ.'AL      ') DINC13A=W1
          IF(WQ.EQ.'AC      ') DINC14A=W1
          IF(WQ.EQ.'AD2     ') DINC5B=W1
          IF(WQ.EQ.'AE2     ') DINC6B=W1
          IF(WQ.EQ.'AF2     ') DINC7B=W1
          IF(WQ.EQ.'AG2     ') DINC8B=W1
          IF(WQ.EQ.'AH2     ') DINC9B=W1
          IF(WQ.EQ.'AI2     ') DINC10B=W1
          IF(WQ.EQ.'AJ2     ') DINC11B=W1
          IF(WQ.EQ.'AK2     ') DINC12B=W1
          IF(WQ.EQ.'AL2     ') DINC13B=W1
          IF(WQ.EQ.'AC2     ') DINC14B=W1
          IF(WQ.EQ.'CVTOR   ') DINC16=W1
          IF(WQ.EQ.'CCTOR   ') DINC17=W1
          IF(WQ.EQ.'ADTOR   ') DINC18A=W1
          IF(WQ.EQ.'AETOR   ') DINC19A=W1
          IF(WQ.EQ.'AFTOR   ') DINC20A=W1
          IF(WQ.EQ.'AGTOR   ') DINC21A=W1
          IF(WQ.EQ.'ADTOR2  ') DINC18B=W1
          IF(WQ.EQ.'AETOR2  ') DINC19B=W1
          IF(WQ.EQ.'AFTOR2  ') DINC20B=W1
          IF(WQ.EQ.'AGTOR2  ') DINC21B=W1
          IF(WQ.EQ.'GALPHA  ') DINC22=W1
          IF(WQ.EQ.'GBETA   ') DINC23=W1
          IF(WQ.EQ.'GGAMMA  ') DINC24=W1
          IF(WQ.EQ.'ALPHA   ') DINC22=W1
          IF(WQ.EQ.'BETA    ') DINC23=W1
          IF(WQ.EQ.'GAMMA   ') DINC24=W1
          IF(WQ.EQ.'XD      ') DINC25=W1
          IF(WQ.EQ.'YD      ') DINC26=W1
          IF(WQ.EQ.'ZD      ') DINC29=W1
          IF(WQ.EQ.'GDX     ') DINC25=W1
          IF(WQ.EQ.'GDY     ') DINC26=W1
          IF(WQ.EQ.'GDZ     ') DINC29=W1
          IF(WQ.EQ.'PIVX    ') DINC25=W1
          IF(WQ.EQ.'PIVY    ') DINC26=W1
          IF(WQ.EQ.'PIVZ    ') DINC29=W1
          IF(WQ.EQ.'N1      ') DINC27=W1
          IF(WQ.EQ.'N2      ') DINC27=W1
          IF(WQ.EQ.'N3      ') DINC27=W1
          IF(WQ.EQ.'N4      ') DINC27=W1
          IF(WQ.EQ.'N5      ') DINC27=W1
          IF(WQ.EQ.'N6      ') DINC27=W1
          IF(WQ.EQ.'N7      ') DINC27=W1
          IF(WQ.EQ.'N8      ') DINC27=W1
          IF(WQ.EQ.'N9      ') DINC27=W1
          IF(WQ.EQ.'N10     ') DINC27=W1
          IF(WQ.EQ.'INDEX   ') DINC27=W1
          IF(WQ.EQ.'VNUM    ') DINC27=W1
          IF(WQ.EQ.'DPART   ') DINC27=W1
          IF(WQ.EQ.'C1      ') DINC28=W1
          IF(WQ.EQ.'C2      ') DINC28=W1
          IF(WQ.EQ.'C3      ') DINC28=W1
          IF(WQ.EQ.'C4      ') DINC28=W1
          IF(WQ.EQ.'C5      ') DINC28=W1
          IF(WQ.EQ.'C6      ') DINC28=W1
          IF(WQ.EQ.'C7      ') DINC28=W1
          IF(WQ.EQ.'C8      ') DINC28=W1
          IF(WQ.EQ.'C9      ') DINC28=W1
          IF(WQ.EQ.'C10     ') DINC28=W1
          IF(WQ.EQ.'C11     ') DINC28=W1
          IF(WQ.EQ.'C12     ') DINC28=W1
          IF(WQ.EQ.'C13     ') DINC28=W1
          IF(WQ.EQ.'C14     ') DINC28=W1
          IF(WQ.EQ.'C15     ') DINC28=W1
          IF(WQ.EQ.'C16     ') DINC28=W1
          IF(WQ.EQ.'C17     ') DINC28=W1
          IF(WQ.EQ.'C18     ') DINC28=W1
          IF(WQ.EQ.'C19     ') DINC28=W1
          IF(WQ.EQ.'C20     ') DINC28=W1
          IF(WQ.EQ.'C21     ') DINC28=W1
          IF(WQ.EQ.'C22     ') DINC28=W1
          IF(WQ.EQ.'C23     ') DINC28=W1
          IF(WQ.EQ.'C24     ') DINC28=W1
          IF(WQ.EQ.'C25     ') DINC28=W1
          IF(WQ.EQ.'C26     ') DINC28=W1
          IF(WQ.EQ.'C27     ') DINC28=W1
          IF(WQ.EQ.'C28     ') DINC28=W1
          IF(WQ.EQ.'C29     ') DINC28=W1
          IF(WQ.EQ.'C30     ') DINC28=W1
          IF(WQ.EQ.'C31     ') DINC28=W1
          IF(WQ.EQ.'C32     ') DINC28=W1
          IF(WQ.EQ.'C33     ') DINC28=W1
          IF(WQ.EQ.'C34     ') DINC28=W1
          IF(WQ.EQ.'C35     ') DINC28=W1
          IF(WQ.EQ.'C36     ') DINC28=W1
          IF(WQ.EQ.'C37     ') DINC28=W1
          IF(WQ.EQ.'C38     ') DINC28=W1
          IF(WQ.EQ.'C39     ') DINC28=W1
          IF(WQ.EQ.'C40     ') DINC28=W1
          IF(WQ.EQ.'C41     ') DINC28=W1
          IF(WQ.EQ.'C42     ') DINC28=W1
          IF(WQ.EQ.'C43     ') DINC28=W1
          IF(WQ.EQ.'C44     ') DINC28=W1
          IF(WQ.EQ.'C45     ') DINC28=W1
          IF(WQ.EQ.'C46     ') DINC28=W1
          IF(WQ.EQ.'C47     ') DINC28=W1
          IF(WQ.EQ.'C48     ') DINC28=W1
          IF(WQ.EQ.'C49     ') DINC28=W1
          IF(WQ.EQ.'C50     ') DINC28=W1
          IF(WQ.EQ.'C51     ') DINC28=W1
          IF(WQ.EQ.'C52     ') DINC28=W1
          IF(WQ.EQ.'C53     ') DINC28=W1
          IF(WQ.EQ.'C54     ') DINC28=W1
          IF(WQ.EQ.'C55     ') DINC28=W1
          IF(WQ.EQ.'C56     ') DINC28=W1
          IF(WQ.EQ.'C57     ') DINC28=W1
          IF(WQ.EQ.'C58     ') DINC28=W1
          IF(WQ.EQ.'C59     ') DINC28=W1
          IF(WQ.EQ.'C60     ') DINC28=W1
          IF(WQ.EQ.'C61     ') DINC28=W1
          IF(WQ.EQ.'C62     ') DINC28=W1
          IF(WQ.EQ.'C63     ') DINC28=W1
          IF(WQ.EQ.'C64     ') DINC28=W1
          IF(WQ.EQ.'C65     ') DINC28=W1
          IF(WQ.EQ.'C66     ') DINC28=W1
          IF(WQ.EQ.'C67     ') DINC28=W1
          IF(WQ.EQ.'C68     ') DINC28=W1
          IF(WQ.EQ.'C69     ') DINC28=W1
          IF(WQ.EQ.'C70     ') DINC28=W1
          IF(WQ.EQ.'C71     ') DINC28=W1
          IF(WQ.EQ.'C72     ') DINC28=W1
          IF(WQ.EQ.'C73     ') DINC28=W1
          IF(WQ.EQ.'C74     ') DINC28=W1
          IF(WQ.EQ.'C75     ') DINC28=W1
          IF(WQ.EQ.'C76     ') DINC28=W1
          IF(WQ.EQ.'C77     ') DINC28=W1
          IF(WQ.EQ.'C78     ') DINC28=W1
          IF(WQ.EQ.'C79     ') DINC28=W1
          IF(WQ.EQ.'C80     ') DINC28=W1
          IF(WQ.EQ.'C81     ') DINC28=W1
          IF(WQ.EQ.'C82     ') DINC28=W1
          IF(WQ.EQ.'C83     ') DINC28=W1
          IF(WQ.EQ.'C84     ') DINC28=W1
          IF(WQ.EQ.'C85     ') DINC28=W1
          IF(WQ.EQ.'C86     ') DINC28=W1
          IF(WQ.EQ.'C87     ') DINC28=W1
          IF(WQ.EQ.'C88     ') DINC28=W1
          IF(WQ.EQ.'C89     ') DINC28=W1
          IF(WQ.EQ.'C90     ') DINC28=W1
          IF(WQ.EQ.'C91     ') DINC28=W1
          IF(WQ.EQ.'C92     ') DINC28=W1
          IF(WQ.EQ.'C93     ') DINC28=W1
          IF(WQ.EQ.'C94     ') DINC28=W1
          IF(WQ.EQ.'C95     ') DINC28=W1
          IF(WQ.EQ.'C96     ') DINC28=W1
          IF(WQ.EQ.'GRS     ') DINC28=W1
          IF(WQ.EQ.'MACVAR  ') DINC28=W1
          IF(WQ(1:3).EQ.'ACT') DINC30=W1
          IF(WQ.EQ.'P001') NSSDINC(1)=W1
          IF(WQ.EQ.'P002') NSSDINC(2)=W1
          IF(WQ.EQ.'P003') NSSDINC(3)=W1
          IF(WQ.EQ.'P004') NSSDINC(4)=W1
          IF(WQ.EQ.'P005') NSSDINC(5)=W1
          IF(WQ.EQ.'P006') NSSDINC(6)=W1
          IF(WQ.EQ.'P007') NSSDINC(7)=W1
          IF(WQ.EQ.'P008') NSSDINC(8)=W1
          IF(WQ.EQ.'P009') NSSDINC(9)=W1
          IF(WQ.EQ.'P010') NSSDINC(10)=W1
          IF(WQ.EQ.'P011') NSSDINC(11)=W1
          IF(WQ.EQ.'P012') NSSDINC(12)=W1
          IF(WQ.EQ.'P013') NSSDINC(13)=W1
          IF(WQ.EQ.'P014') NSSDINC(14)=W1
          IF(WQ.EQ.'P015') NSSDINC(15)=W1
          IF(WQ.EQ.'P016') NSSDINC(16)=W1
          IF(WQ.EQ.'P017') NSSDINC(17)=W1
          IF(WQ.EQ.'P018') NSSDINC(18)=W1
          IF(WQ.EQ.'P019') NSSDINC(19)=W1
          IF(WQ.EQ.'P020') NSSDINC(20)=W1
          IF(WQ.EQ.'P021') NSSDINC(21)=W1
          IF(WQ.EQ.'P022') NSSDINC(22)=W1
          IF(WQ.EQ.'P023') NSSDINC(23)=W1
          IF(WQ.EQ.'P024') NSSDINC(24)=W1
          IF(WQ.EQ.'P025') NSSDINC(25)=W1
          IF(WQ.EQ.'P026') NSSDINC(26)=W1
          IF(WQ.EQ.'P027') NSSDINC(27)=W1
          IF(WQ.EQ.'P028') NSSDINC(28)=W1
          IF(WQ.EQ.'P029') NSSDINC(29)=W1
          IF(WQ.EQ.'P030') NSSDINC(30)=W1
          IF(WQ.EQ.'P031') NSSDINC(31)=W1
          IF(WQ.EQ.'P032') NSSDINC(32)=W1
          IF(WQ.EQ.'P033') NSSDINC(33)=W1
          IF(WQ.EQ.'P034') NSSDINC(34)=W1
          IF(WQ.EQ.'P035') NSSDINC(35)=W1
          IF(WQ.EQ.'P036') NSSDINC(36)=W1
          IF(WQ.EQ.'P037') NSSDINC(37)=W1
          IF(WQ.EQ.'P038') NSSDINC(38)=W1
          IF(WQ.EQ.'P039') NSSDINC(39)=W1
          IF(WQ.EQ.'P040') NSSDINC(40)=W1
          IF(WQ.EQ.'P041') NSSDINC(41)=W1
          IF(WQ.EQ.'P042') NSSDINC(42)=W1
          IF(WQ.EQ.'P043') NSSDINC(43)=W1
          IF(WQ.EQ.'P044') NSSDINC(44)=W1
          IF(WQ.EQ.'P045') NSSDINC(45)=W1
          IF(WQ.EQ.'P046') NSSDINC(46)=W1
          IF(WQ.EQ.'P047') NSSDINC(47)=W1
          IF(WQ.EQ.'P048') NSSDINC(48)=W1
          IF(WQ.EQ.'P049') NSSDINC(49)=W1
          IF(WQ.EQ.'P050') NSSDINC(50)=W1
          IF(WQ.EQ.'P051') NSSDINC(51)=W1
          IF(WQ.EQ.'P052') NSSDINC(52)=W1
          IF(WQ.EQ.'P053') NSSDINC(53)=W1
          IF(WQ.EQ.'P054') NSSDINC(54)=W1
          IF(WQ.EQ.'P055') NSSDINC(55)=W1
          IF(WQ.EQ.'P056') NSSDINC(56)=W1
          IF(WQ.EQ.'P057') NSSDINC(57)=W1
          IF(WQ.EQ.'P058') NSSDINC(58)=W1
          IF(WQ.EQ.'P059') NSSDINC(59)=W1
          IF(WQ.EQ.'P060') NSSDINC(60)=W1
          IF(WQ.EQ.'P061') NSSDINC(61)=W1
          IF(WQ.EQ.'P062') NSSDINC(62)=W1
          IF(WQ.EQ.'P063') NSSDINC(63)=W1
          IF(WQ.EQ.'P064') NSSDINC(64)=W1
          IF(WQ.EQ.'P065') NSSDINC(65)=W1
          IF(WQ.EQ.'P066') NSSDINC(66)=W1
          IF(WQ.EQ.'P067') NSSDINC(67)=W1
          IF(WQ.EQ.'P068') NSSDINC(68)=W1
          IF(WQ.EQ.'P069') NSSDINC(69)=W1
          IF(WQ.EQ.'P070') NSSDINC(70)=W1
          IF(WQ.EQ.'P071') NSSDINC(71)=W1
          IF(WQ.EQ.'P072') NSSDINC(72)=W1
          IF(WQ.EQ.'P073') NSSDINC(73)=W1
          IF(WQ.EQ.'P074') NSSDINC(74)=W1
          IF(WQ.EQ.'P075') NSSDINC(75)=W1
          IF(WQ.EQ.'P076') NSSDINC(76)=W1
          IF(WQ.EQ.'P077') NSSDINC(77)=W1
          IF(WQ.EQ.'P078') NSSDINC(78)=W1
          IF(WQ.EQ.'P079') NSSDINC(79)=W1
          IF(WQ.EQ.'P080') NSSDINC(80)=W1
          IF(WQ.EQ.'P081') NSSDINC(81)=W1
          IF(WQ.EQ.'P082') NSSDINC(82)=W1
          IF(WQ.EQ.'P083') NSSDINC(81)=W1
          IF(WQ.EQ.'P084') NSSDINC(81)=W1
          IF(WQ.EQ.'P085') NSSDINC(85)=W1
          IF(WQ.EQ.'P086') NSSDINC(86)=W1
          IF(WQ.EQ.'P087') NSSDINC(87)=W1
          IF(WQ.EQ.'P088') NSSDINC(88)=W1
          IF(WQ.EQ.'P089') NSSDINC(89)=W1
          IF(WQ.EQ.'P090') NSSDINC(90)=W1
          IF(WQ.EQ.'P091') NSSDINC(91)=W1
          IF(WQ.EQ.'P092') NSSDINC(92)=W1
          IF(WQ.EQ.'P093') NSSDINC(93)=W1
          IF(WQ.EQ.'P094') NSSDINC(94)=W1
          IF(WQ.EQ.'P095') NSSDINC(95)=W1
          IF(WQ.EQ.'P096') NSSDINC(96)=W1
          IF(WQ.EQ.'P097') NSSDINC(97)=W1
          IF(WQ.EQ.'P098') NSSDINC(98)=W1
          IF(WQ.EQ.'P099') NSSDINC(99)=W1
          IF(WQ.EQ.'P100') NSSDINC(100)=W1
          IF(WQ.EQ.'P101') NSSDINC(101)=W1
          IF(WQ.EQ.'P102') NSSDINC(102)=W1
          IF(WQ.EQ.'P103') NSSDINC(103)=W1
          IF(WQ.EQ.'P104') NSSDINC(104)=W1
          IF(WQ.EQ.'P105') NSSDINC(105)=W1
          IF(WQ.EQ.'P106') NSSDINC(106)=W1
          IF(WQ.EQ.'P107') NSSDINC(107)=W1
          IF(WQ.EQ.'P108') NSSDINC(108)=W1
          IF(WQ.EQ.'P109') NSSDINC(109)=W1
          IF(WQ.EQ.'P110') NSSDINC(110)=W1
          IF(WQ.EQ.'P111') NSSDINC(111)=W1
          IF(WQ.EQ.'P112') NSSDINC(112)=W1
          IF(WQ.EQ.'P113') NSSDINC(113)=W1
          IF(WQ.EQ.'P114') NSSDINC(114)=W1
          IF(WQ.EQ.'P115') NSSDINC(115)=W1
          IF(WQ.EQ.'P116') NSSDINC(116)=W1
          IF(WQ.EQ.'P117') NSSDINC(117)=W1
          IF(WQ.EQ.'P118') NSSDINC(118)=W1
          IF(WQ.EQ.'P119') NSSDINC(119)=W1
          IF(WQ.EQ.'P120') NSSDINC(120)=W1
          IF(WQ.EQ.'P121') NSSDINC(121)=W1
          IF(WQ.EQ.'P122') NSSDINC(122)=W1
          IF(WQ.EQ.'P123') NSSDINC(123)=W1
          IF(WQ.EQ.'P124') NSSDINC(124)=W1
          IF(WQ.EQ.'P125') NSSDINC(125)=W1
          IF(WQ.EQ.'P126') NSSDINC(126)=W1
          IF(WQ.EQ.'P127') NSSDINC(127)=W1
          IF(WQ.EQ.'P128') NSSDINC(128)=W1
          IF(WQ.EQ.'P129') NSSDINC(129)=W1
          IF(WQ.EQ.'P130') NSSDINC(130)=W1
          IF(WQ.EQ.'P131') NSSDINC(131)=W1
          IF(WQ.EQ.'P132') NSSDINC(132)=W1
          IF(WQ.EQ.'P133') NSSDINC(133)=W1
          IF(WQ.EQ.'P134') NSSDINC(134)=W1
          IF(WQ.EQ.'P135') NSSDINC(135)=W1
          IF(WQ.EQ.'P136') NSSDINC(136)=W1
          IF(WQ.EQ.'P137') NSSDINC(137)=W1
          IF(WQ.EQ.'P138') NSSDINC(138)=W1
          IF(WQ.EQ.'P139') NSSDINC(139)=W1
          IF(WQ.EQ.'P140') NSSDINC(140)=W1
          IF(WQ.EQ.'P141') NSSDINC(141)=W1
          IF(WQ.EQ.'P142') NSSDINC(142)=W1
          IF(WQ.EQ.'P143') NSSDINC(143)=W1
          IF(WQ.EQ.'P144') NSSDINC(144)=W1
          IF(WQ.EQ.'P145') NSSDINC(145)=W1
          IF(WQ.EQ.'P146') NSSDINC(146)=W1
          IF(WQ.EQ.'P147') NSSDINC(147)=W1
          IF(WQ.EQ.'P148') NSSDINC(148)=W1
          IF(WQ.EQ.'P149') NSSDINC(149)=W1
          IF(WQ.EQ.'P150') NSSDINC(150)=W1
          IF(WQ.EQ.'P151') NSSDINC(151)=W1
          IF(WQ.EQ.'P152') NSSDINC(152)=W1
          IF(WQ.EQ.'P153') NSSDINC(153)=W1
          IF(WQ.EQ.'P154') NSSDINC(154)=W1
          IF(WQ.EQ.'P155') NSSDINC(155)=W1
          IF(WQ.EQ.'P156') NSSDINC(156)=W1
          IF(WQ.EQ.'P157') NSSDINC(157)=W1
          IF(WQ.EQ.'P158') NSSDINC(158)=W1
          IF(WQ.EQ.'P159') NSSDINC(159)=W1
          IF(WQ.EQ.'P160') NSSDINC(160)=W1
          IF(WQ.EQ.'P161') NSSDINC(161)=W1
          IF(WQ.EQ.'P162') NSSDINC(162)=W1
          IF(WQ.EQ.'P163') NSSDINC(163)=W1
          IF(WQ.EQ.'P164') NSSDINC(164)=W1
          IF(WQ.EQ.'P165') NSSDINC(165)=W1
          IF(WQ.EQ.'P166') NSSDINC(166)=W1
          IF(WQ.EQ.'P167') NSSDINC(167)=W1
          IF(WQ.EQ.'P168') NSSDINC(168)=W1
          IF(WQ.EQ.'P169') NSSDINC(169)=W1
          IF(WQ.EQ.'P170') NSSDINC(170)=W1
          IF(WQ.EQ.'P171') NSSDINC(171)=W1
          IF(WQ.EQ.'P172') NSSDINC(172)=W1
          IF(WQ.EQ.'P173') NSSDINC(173)=W1
          IF(WQ.EQ.'P174') NSSDINC(174)=W1
          IF(WQ.EQ.'P175') NSSDINC(175)=W1
          IF(WQ.EQ.'P176') NSSDINC(176)=W1
          IF(WQ.EQ.'P177') NSSDINC(177)=W1
          IF(WQ.EQ.'P178') NSSDINC(178)=W1
          IF(WQ.EQ.'P179') NSSDINC(179)=W1
          IF(WQ.EQ.'P180') NSSDINC(180)=W1
          IF(WQ.EQ.'P181') NSSDINC(181)=W1
          IF(WQ.EQ.'P182') NSSDINC(182)=W1
          IF(WQ.EQ.'P183') NSSDINC(181)=W1
          IF(WQ.EQ.'P184') NSSDINC(181)=W1
          IF(WQ.EQ.'P185') NSSDINC(185)=W1
          IF(WQ.EQ.'P186') NSSDINC(186)=W1
          IF(WQ.EQ.'P187') NSSDINC(187)=W1
          IF(WQ.EQ.'P188') NSSDINC(188)=W1
          IF(WQ.EQ.'P189') NSSDINC(189)=W1
          IF(WQ.EQ.'P190') NSSDINC(190)=W1
          IF(WQ.EQ.'P191') NSSDINC(191)=W1
          IF(WQ.EQ.'P192') NSSDINC(192)=W1
          IF(WQ.EQ.'P193') NSSDINC(193)=W1
          IF(WQ.EQ.'P194') NSSDINC(194)=W1
          IF(WQ.EQ.'P195') NSSDINC(195)=W1
          IF(WQ.EQ.'P196') NSSDINC(196)=W1
          IF(WQ.EQ.'P197') NSSDINC(197)=W1
          IF(WQ.EQ.'P198') NSSDINC(198)=W1
          IF(WQ.EQ.'P199') NSSDINC(199)=W1
          IF(WQ.EQ.'P200') NSSDINC(200)=W1
          IF(WQ.EQ.'NSSXPOS') D=NSSDINC(201)
          IF(WQ.EQ.'NSSYPOS') D=NSSDINC(202)
          IF(WQ.EQ.'NSSZPOS') D=NSSDINC(203)
          IF(WQ.EQ.'NSSALPH') D=NSSDINC(204)
          IF(WQ.EQ.'NSSBETA') D=NSSDINC(205)
          IF(WQ.EQ.'NSSGAMM') D=NSSDINC(206)
          IF(WQ.EQ.'V1') D=NSSDINC(207)
          IF(WQ.EQ.'V2') D=NSSDINC(208)
          IF(WQ.EQ.'V3') D=NSSDINC(209)
          IF(WQ.EQ.'V4') D=NSSDINC(210)
          IF(WQ.EQ.'V5') D=NSSDINC(211)
C
          IF(WQ.EQ.'CV      ') DINO=.TRUE.
          IF(WQ.EQ.'TH      ') DINO=.TRUE.
          IF(WQ.EQ.'CLPX    ') DINO=.TRUE.
          IF(WQ.EQ.'CLPY    ') DINO=.TRUE.
          IF(WQ.EQ.'CC      ') DINO=.TRUE.
          IF(WQ.EQ.'AD      ') DINO=.TRUE.
          IF(WQ.EQ.'AE      ') DINO=.TRUE.
          IF(WQ.EQ.'AF      ') DINO=.TRUE.
          IF(WQ.EQ.'AG      ') DINO=.TRUE.
          IF(WQ.EQ.'AH      ') DINO=.TRUE.
          IF(WQ.EQ.'AI      ') DINO=.TRUE.
          IF(WQ.EQ.'AJ      ') DINO=.TRUE.
          IF(WQ.EQ.'AK      ') DINO=.TRUE.
          IF(WQ.EQ.'AL      ') DINO=.TRUE.
          IF(WQ.EQ.'AC      ') DINO=.TRUE.
          IF(WQ.EQ.'AD2     ') DINO=.TRUE.
          IF(WQ.EQ.'AE2     ') DINO=.TRUE.
          IF(WQ.EQ.'AF2     ') DINO=.TRUE.
          IF(WQ.EQ.'AG2     ') DINO=.TRUE.
          IF(WQ.EQ.'AH2     ') DINO=.TRUE.
          IF(WQ.EQ.'AI2     ') DINO=.TRUE.
          IF(WQ.EQ.'AJ2     ') DINO=.TRUE.
          IF(WQ.EQ.'AK2     ') DINO=.TRUE.
          IF(WQ.EQ.'AL2     ') DINO=.TRUE.
          IF(WQ.EQ.'AC2     ') DINO=.TRUE.
          IF(WQ.EQ.'CVTOR   ') DINO=.TRUE.
          IF(WQ.EQ.'CCTOR   ') DINO=.TRUE.
          IF(WQ.EQ.'ADTOR   ') DINO=.TRUE.
          IF(WQ.EQ.'AETOR   ') DINO=.TRUE.
          IF(WQ.EQ.'AFTOR   ') DINO=.TRUE.
          IF(WQ.EQ.'AGTOR   ') DINO=.TRUE.
          IF(WQ.EQ.'ADTOR2  ') DINO=.TRUE.
          IF(WQ.EQ.'AETOR2  ') DINO=.TRUE.
          IF(WQ.EQ.'AFTOR2  ') DINO=.TRUE.
          IF(WQ.EQ.'AGTOR2  ') DINO=.TRUE.
          IF(WQ.EQ.'ALPHA   ') DINO=.TRUE.
          IF(WQ.EQ.'BETA    ') DINO=.TRUE.
          IF(WQ.EQ.'GAMMA   ') DINO=.TRUE.
          IF(WQ.EQ.'XD      ') DINO=.TRUE.
          IF(WQ.EQ.'YD      ') DINO=.TRUE.
          IF(WQ.EQ.'ZD      ') DINO=.TRUE.
          IF(WQ.EQ.'GALPHA  ') DINO=.TRUE.
          IF(WQ.EQ.'GBETA   ') DINO=.TRUE.
          IF(WQ.EQ.'GGAMMA  ') DINO=.TRUE.
          IF(WQ.EQ.'GDX     ') DINO=.TRUE.
          IF(WQ.EQ.'GDY     ') DINO=.TRUE.
          IF(WQ.EQ.'GDZ     ') DINO=.TRUE.
          IF(WQ.EQ.'PIVX    ') DINO=.TRUE.
          IF(WQ.EQ.'PIVY    ') DINO=.TRUE.
          IF(WQ.EQ.'PIVZ    ') DINO=.TRUE.
          IF(WQ.EQ.'N1      ') DINO=.TRUE.
          IF(WQ.EQ.'N2      ') DINO=.TRUE.
          IF(WQ.EQ.'N3      ') DINO=.TRUE.
          IF(WQ.EQ.'N4      ') DINO=.TRUE.
          IF(WQ.EQ.'N5      ') DINO=.TRUE.
          IF(WQ.EQ.'N6      ') DINO=.TRUE.
          IF(WQ.EQ.'N7      ') DINO=.TRUE.
          IF(WQ.EQ.'N8      ') DINO=.TRUE.
          IF(WQ.EQ.'N9      ') DINO=.TRUE.
          IF(WQ.EQ.'N10     ') DINO=.TRUE.
          IF(WQ.EQ.'INDEX   ') DINO=.TRUE.
          IF(WQ.EQ.'VNUM    ') DINO=.TRUE.
          IF(WQ.EQ.'DPART   ') DINO=.TRUE.
          IF(WQ.EQ.'C1      ') DINO=.TRUE.
          IF(WQ.EQ.'C2      ') DINO=.TRUE.
          IF(WQ.EQ.'C3      ') DINO=.TRUE.
          IF(WQ.EQ.'C4      ') DINO=.TRUE.
          IF(WQ.EQ.'C5      ') DINO=.TRUE.
          IF(WQ.EQ.'C6      ') DINO=.TRUE.
          IF(WQ.EQ.'C7      ') DINO=.TRUE.
          IF(WQ.EQ.'C8      ') DINO=.TRUE.
          IF(WQ.EQ.'C9      ') DINO=.TRUE.
          IF(WQ.EQ.'C10     ') DINO=.TRUE.
          IF(WQ.EQ.'C11     ') DINO=.TRUE.
          IF(WQ.EQ.'C12     ') DINO=.TRUE.
          IF(WQ.EQ.'C13     ') DINO=.TRUE.
          IF(WQ.EQ.'C14     ') DINO=.TRUE.
          IF(WQ.EQ.'C15     ') DINO=.TRUE.
          IF(WQ.EQ.'C16     ') DINO=.TRUE.
          IF(WQ.EQ.'C17     ') DINO=.TRUE.
          IF(WQ.EQ.'C18     ') DINO=.TRUE.
          IF(WQ.EQ.'C19     ') DINO=.TRUE.
          IF(WQ.EQ.'C20     ') DINO=.TRUE.
          IF(WQ.EQ.'C21     ') DINO=.TRUE.
          IF(WQ.EQ.'C22     ') DINO=.TRUE.
          IF(WQ.EQ.'C23     ') DINO=.TRUE.
          IF(WQ.EQ.'C24     ') DINO=.TRUE.
          IF(WQ.EQ.'C25     ') DINO=.TRUE.
          IF(WQ.EQ.'C26     ') DINO=.TRUE.
          IF(WQ.EQ.'C27     ') DINO=.TRUE.
          IF(WQ.EQ.'C28     ') DINO=.TRUE.
          IF(WQ.EQ.'C29     ') DINO=.TRUE.
          IF(WQ.EQ.'C30     ') DINO=.TRUE.
          IF(WQ.EQ.'C31     ') DINO=.TRUE.
          IF(WQ.EQ.'C32     ') DINO=.TRUE.
          IF(WQ.EQ.'C33     ') DINO=.TRUE.
          IF(WQ.EQ.'C34     ') DINO=.TRUE.
          IF(WQ.EQ.'C35     ') DINO=.TRUE.
          IF(WQ.EQ.'C36     ') DINO=.TRUE.
          IF(WQ.EQ.'C37     ') DINO=.TRUE.
          IF(WQ.EQ.'C38     ') DINO=.TRUE.
          IF(WQ.EQ.'C39     ') DINO=.TRUE.
          IF(WQ.EQ.'C40     ') DINO=.TRUE.
          IF(WQ.EQ.'C41     ') DINO=.TRUE.
          IF(WQ.EQ.'C42     ') DINO=.TRUE.
          IF(WQ.EQ.'C43     ') DINO=.TRUE.
          IF(WQ.EQ.'C44     ') DINO=.TRUE.
          IF(WQ.EQ.'C45     ') DINO=.TRUE.
          IF(WQ.EQ.'C46     ') DINO=.TRUE.
          IF(WQ.EQ.'C47     ') DINO=.TRUE.
          IF(WQ.EQ.'C48     ') DINO=.TRUE.
          IF(WQ.EQ.'C49     ') DINO=.TRUE.
          IF(WQ.EQ.'C50     ') DINO=.TRUE.
          IF(WQ.EQ.'C51     ') DINO=.TRUE.
          IF(WQ.EQ.'C52     ') DINO=.TRUE.
          IF(WQ.EQ.'C53     ') DINO=.TRUE.
          IF(WQ.EQ.'C54     ') DINO=.TRUE.
          IF(WQ.EQ.'C55     ') DINO=.TRUE.
          IF(WQ.EQ.'C56     ') DINO=.TRUE.
          IF(WQ.EQ.'C57     ') DINO=.TRUE.
          IF(WQ.EQ.'C58     ') DINO=.TRUE.
          IF(WQ.EQ.'C59     ') DINO=.TRUE.
          IF(WQ.EQ.'C60     ') DINO=.TRUE.
          IF(WQ.EQ.'C61     ') DINO=.TRUE.
          IF(WQ.EQ.'C62     ') DINO=.TRUE.
          IF(WQ.EQ.'C63     ') DINO=.TRUE.
          IF(WQ.EQ.'C64     ') DINO=.TRUE.
          IF(WQ.EQ.'C65     ') DINO=.TRUE.
          IF(WQ.EQ.'C66     ') DINO=.TRUE.
          IF(WQ.EQ.'C67     ') DINO=.TRUE.
          IF(WQ.EQ.'C68     ') DINO=.TRUE.
          IF(WQ.EQ.'C69     ') DINO=.TRUE.
          IF(WQ.EQ.'C70     ') DINO=.TRUE.
          IF(WQ.EQ.'C71     ') DINO=.TRUE.
          IF(WQ.EQ.'C72     ') DINO=.TRUE.
          IF(WQ.EQ.'C73     ') DINO=.TRUE.
          IF(WQ.EQ.'C74     ') DINO=.TRUE.
          IF(WQ.EQ.'C75     ') DINO=.TRUE.
          IF(WQ.EQ.'C76     ') DINO=.TRUE.
          IF(WQ.EQ.'C77     ') DINO=.TRUE.
          IF(WQ.EQ.'C78     ') DINO=.TRUE.
          IF(WQ.EQ.'C79     ') DINO=.TRUE.
          IF(WQ.EQ.'C80     ') DINO=.TRUE.
          IF(WQ.EQ.'C81     ') DINO=.TRUE.
          IF(WQ.EQ.'C82     ') DINO=.TRUE.
          IF(WQ.EQ.'C83     ') DINO=.TRUE.
          IF(WQ.EQ.'C84     ') DINO=.TRUE.
          IF(WQ.EQ.'C85     ') DINO=.TRUE.
          IF(WQ.EQ.'C86     ') DINO=.TRUE.
          IF(WQ.EQ.'C87     ') DINO=.TRUE.
          IF(WQ.EQ.'C88     ') DINO=.TRUE.
          IF(WQ.EQ.'C89     ') DINO=.TRUE.
          IF(WQ.EQ.'C90     ') DINO=.TRUE.
          IF(WQ.EQ.'C91     ') DINO=.TRUE.
          IF(WQ.EQ.'C92     ') DINO=.TRUE.
          IF(WQ.EQ.'C93     ') DINO=.TRUE.
          IF(WQ.EQ.'C94     ') DINO=.TRUE.
          IF(WQ.EQ.'C95     ') DINO=.TRUE.
          IF(WQ.EQ.'C96     ') DINO=.TRUE.
          IF(WQ.EQ.'GRS     ') DINO=.TRUE.
          IF(WQ.EQ.'MACVAR  ') DINO=.TRUE.
          IF(WQ(1:3).EQ.'ACT') DINO=.TRUE.
          IF(WQ(1:2).EQ.'P0') DINO=.TRUE.
          IF(WQ(1:2).EQ.'P1') DINO=.TRUE.
          IF(WQ.EQ.'NSSXPOS') DINO=.TRUE.
          IF(WQ.EQ.'NSSYPOS') DINO=.TRUE.
          IF(WQ.EQ.'NSSZPOS') DINO=.TRUE.
          IF(WQ.EQ.'NSSALPH') DINO=.TRUE.
          IF(WQ.EQ.'NSSBETA') DINO=.TRUE.
          IF(WQ.EQ.'NSSGAMM') DINO=.TRUE.
          IF(WQ.EQ.'V1') DINO=.TRUE.
          IF(WQ.EQ.'V2') DINO=.TRUE.
          IF(WQ.EQ.'V3') DINO=.TRUE.
          IF(WQ.EQ.'V4') DINO=.TRUE.
          IF(WQ.EQ.'V5') DINO=.TRUE.
          IF(DINO) RETURN
          WRITE(OUTLYNE,*)
     1    'INVALID QUALIFIER ISSUED WITH "DINCR"'
          CALL SHOWIT(1)
          WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
          CALL SHOWIT(1)
          CALL MACFAL
          RETURN
      END



      SUBROUTINE POWELL(V,VN,PCNT,P)
          IMPLICIT NONE
          INTEGER ITER,NUB,VN,PCNT
          COMMON/ITTERY/ITER
          REAL*8 FRET,FUNCIP,P
          COMMON/FRETTY/FRET
          EXTERNAL FUNCIP
C     USES LINMIN,FUNCIP
          INTEGER I,IBIG,ALLOERR
          REAL*8 DEL,FP,FPTT,V
          DIMENSION V(VN,VN)
C
          REAL*8 PT,XIT
     1    ,XDUM(1:100000)
C
          DIMENSION P(PCNT),PT(:),XIT(:)
C
          ALLOCATABLE :: PT,XIT
C
          COMMON/FNPEE/XDUM
C
          REAL*8
     2    X(1:100000)
C
          COMMON/SVD2/X
C
          INCLUDE 'datsub.inc'
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
          NUB=VBCNT+1
          DEALLOCATE(PT,XIT,STAT=ALLOERR)
          ALLOCATE(PT(NUB),XIT(NUB),STAT=ALLOERR)
          XDUM(1:VBCNT)=P(1:VBCNT)
          FRET=FUNCIP()
          PT(1:VBCNT)=P(1:VBCNT)
          ITER=0
          ITER=ITER+1
          FP=FRET
          IBIG=0
          DEL=0.0D0
          DO I=1,VBCNT
              XIT(1:VBCNT)=V(1:VBCNT,I)
              FPTT=FRET
C     LINMIN IS CALLED ONCE FOR EACH VARIABLE (THE ITH VARIABLE)
              CALL LINMIN(PCNT,P,XIT)
              IF(DABS(FPTT-FRET).GT.DEL) THEN
                  DEL=DABS(FPTT-FRET)
                  IBIG=I
              END IF
          END DO
          DEALLOCATE(PT,XIT,STAT=ALLOERR)
          RETURN
      END
C

      SUBROUTINE LINMIN(PCNT,P,XXI)
          IMPLICIT NONE
          INTEGER NUB,PCNT,ALLOERR
          REAL*8 FRET,P,XXI
          DIMENSION P(PCNT),XXI(PCNT)
          COMMON/FRETTY/FRET
CU    USES JACME
          INTEGER J,NCOM
          REAL*8 AX,BX,FA,FB,FX,XMIN,XX,PCOM,JKX
          REAL*8 JKAX,JKXX,JKBX,JKXMIN,XICOM,JACME
          DIMENSION PCOM(:),XICOM(:)
          ALLOCATABLE :: PCOM,XICOM
          COMMON/MNBRAC/AX,XX,BX,FA,FX,FB
          COMMON /F1COM/NCOM
          COMMON/F2COM/JKX
          COMMON/JACMEON/JKAX,JKXX,JKBX,JKXMIN
          EXTERNAL JACME
!      DIMENSION V(VN,VN)
C
          INCLUDE 'datsub.inc'
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
          NUB=VBCNT+1
          DEALLOCATE(PCOM,XICOM,STAT=ALLOERR)
          ALLOCATE(PCOM(NUB),XICOM(NUB),STAT=ALLOERR)
          NCOM=NUB
          J=NUB
          PCOM(1:J)=P(1:J)
          XICOM(1:J)=XXI(1:J)
          AX=-1.0D0
          XX=0.0D0
          BX=1.0D0
          XMIN=0.0D0                ! ADD by ENDO
          JKAX=AX
          JKXX=XX
          JKBX=BX
          JKXMIN=XMIN
          FRET=JACME(NUB,PCOM,XICOM)
          AX=JKAX
          XX=JKXX
          BX=JKBX
          XMIN=JKXMIN
          J=NUB
          XXI(1:J)=XMIN*XXI(1:J)
          P(1:J)=P(1:J)+XXI(1:J)
          DEALLOCATE(PCOM,STAT=ALLOERR)
          RETURN
      END


C
      FUNCTION JACME(NUB,PCOM,XICOM)
          IMPLICIT NONE
          INTEGER NUB
          REAL*8 JACME,AX,BX,CX,F1DIM,JKX,FAX,FBX,FCX
     1    ,FX,X,XMIN,FDEL1,FDEL2,FDEL3,DX1,DX2,DX3,DX12,DX22,DX32
     2    ,AXX,BXX,CXX,AAEE,BBEE,CCEE,PCOM,XICOM
          DIMENSION PCOM(NUB),XICOM(NUB)
          EXTERNAL F1DIM
          COMMON/F2COM/JKX
          COMMON/JACMEON/AX,BX,CX,XMIN
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datsub.inc'
C
C     AX,BX,CX ALL EQUAL
          IF(AX.EQ.BX.AND.AX.EQ.CX) THEN
              JKX=AX
              FAX=F1DIM(NUB,PCOM,XICOM)
              XMIN=AX
              JACME=FAX
              RETURN
          END IF
C
C     TWO VALUES EQUAL
          IF(AX.EQ.BX.AND.AX.NE.CX.OR.
     1    BX.EQ.CX.AND.AX.NE.CX.OR.
     1    AX.NE.BX.AND.AX.EQ.CX.OR.
     1    BX.NE.CX.AND.AX.EQ.CX) THEN
C
              IF(AX.EQ.BX.AND.AX.NE.CX.OR.
     1        BX.EQ.CX.AND.AX.NE.CX) AXX=(AX+CX)/2.0D0
              IF(AX.NE.BX.AND.AX.EQ.CX.OR.
     1        BX.NE.CX.AND.AX.EQ.CX) AXX=(BX+CX)/2.0D0
              JKX=AXX
              FAX=F1DIM(NUB,PCOM,XICOM)
              XMIN=AXX
              JACME=FAX
              RETURN
          END IF
C     AX,BX,CX ALL DIFFERENT
C     LARGEST VALUE
          CXX=DMAX1(AX,BX,CX)
C     SMALLEST VALUE
          AXX=DMIN1(AX,BX,CX)
C     MIDDLE VALUE
          IF(AX.LT.CXX.AND.AX.GT.AXX) BXX=AX
          IF(BX.LT.CXX.AND.BX.GT.AXX) BXX=BX
          IF(CX.LT.CXX.AND.CX.GT.AXX) BXX=CX
          JKX=AXX
          FAX=F1DIM(NUB,PCOM,XICOM)
          JKX=BXX
          FBX=F1DIM(NUB,PCOM,XICOM)
          JKX=CXX
          FCX=F1DIM(NUB,PCOM,XICOM)
C     FIT TO A PARABOLA AND SOLVE FOR THE MINIMUM
C     THE X FOR THE MINIMUM WILL BE XMIN
C     WITH FUNCTIONAL VALUE JACME
C     THIS IS TO DO AFTER MY RUN
          FDEL1=FAX-FBX
          FDEL2=FAX-FCX
          FDEL3=FBX-FCX
          DX1=AXX-BXX
          DX2=AXX-CXX
          DX3=BXX-CXX
          DX12=(AXX**2)-(BXX**2)
          DX22=(AXX**2)-(CXX**2)
          DX32=(BXX**2)-(CXX**2)
C
          IF(FDEL1.EQ.0.0D0.OR.FDEL2.EQ.0.0D0.OR.FDEL3.EQ.0.0D0) THEN
C     USE MIDDLE VALUE
              JKX=BXX
              FBX=F1DIM(NUB,PCOM,XICOM)
              XMIN=BXX
              JACME=FBX
              RETURN
          ELSE
C     SOLVE FOR X AND FX ON A PARABOLA
C
              IF(((DX1*DX22)-(DX2*DX12)).EQ.0.0D0.OR.DX12.EQ.0.0D0) THEN
                  X=BXX
                  FX=FBX
              ELSE
                  BBEE=((FDEL1*DX22)-(FDEL2*DX12))/((DX1*DX22)-(DX2*DX12))
                  AAEE=(FDEL1-(BBEE*DX1))/DX12
                  CCEE=FAX-(AAEE*DX12)-(BBEE*DX1)
                  IF(BBEE.EQ.0.0D0) THEN
                      X=0.0D0
                  ELSE
                      IF(AAEE.EQ.0.0D0) THEN
                          X=BXX
                      ELSE
                          X=-BBEE/(2.0D0*AAEE)
                      END IF
                  END IF
                  FX=(AAEE*(X**2))+(BBEE*X)+CCEE
              END IF
              XMIN=X
              JACME=FX
              RETURN
          END IF
      END



      FUNCTION F1DIM(NUB,PCOM,XICOM)
          IMPLICIT NONE
          INTEGER ALLOERR
          REAL*8 F1DIM,FUNCIP,JKX,XDUM(1:100000)
          COMMON/FNPEE/XDUM
CU    USES FUNCIP
          EXTERNAL FUNCIP
          INTEGER J,NCOM,NUB
          REAL*8 XT,PCOM,XICOM
          DIMENSION PCOM(NUB),XICOM(NUB),XT(:)
          ALLOCATABLE :: XT
          COMMON /F1COM/NCOM
          COMMON/F2COM/JKX
          INCLUDE 'datsub.inc'
          INCLUDE 'datmai.inc'
          DEALLOCATE (XT,STAT=ALLOERR)
          ALLOCATE(XT(NCOM),STAT=ALLOERR)
          J=NCOM
          XT(1:J)=PCOM(1:J)+JKX*XICOM(1:J)
          XDUM(1:J)=XT(1:J)
          F1DIM=FUNCIP()
          DEALLOCATE (XT,STAT=ALLOERR)
          RETURN
      END


C
C FUN FUNCIP.FOR
      FUNCTION FUNCIP()
C
          IMPLICIT NONE
C
          INTEGER I,VCFG,VTYPE,ALTYPE,VADD,NUB
C
          CHARACTER AN1*23,AV1*23,OLDAV1*23
C
          INTEGER ISURF
C
          REAL*8 NEWDEFVAL
C
          COMMON/DEFVALCOM/NEWDEFVAL
C
          REAL*8 IPFUN,V1,OLDV1,N1,FUNCIP,XDUM(1:100000)

          LOGICAL COMPOPS,ERR1,ERR2
          COMMON/OPSCOMP/COMPOPS
C
          COMMON/FNPEE/XDUM
C
          EXTERNAL IPFUN
C
          COMMON/CAUX1/N1,AN1
C
C     THIS ROUTINE APPLIES THE VARIABLES CONTAINED IN THE XDIM
C     ARRAY TO THE CURRENT LENS AND THEN COMPUTES THE NEW RESULTANT FMT
C
C     IPFMT IS THE RESULTING FIGURE OF MERIT
C
          INCLUDE 'datsub.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          INCLUDE 'datcfg.inc'
          INCLUDE 'datmac.inc'
C
C       THIS IS SUBROUTINE FUNCIP. THIS IS THE SUBROUTINE WHICH
C       CALCULATES THE FMT FOR POWELL OPTIMIZATION
C       FOR THE VARIABLE VALUES IN ARRAY XDUM
C
          NUB=VBCNT+1
C
C     SAVE THE ORIGINAL OPERANDS BEFORE WE START
          OLDOP(1:OPCNT,1:20)=OPERND(1:OPCNT,1:20)
          DO I=1,VBCNT
C     NOW FOR VARIABLE I MAKE THE APPROPRIATE
C     CHANGE TO THE LENS
C     FIRST, IF THE VARIABLE IS A CONFIG 1 VARIABLE
C     NEXT IF THE VARIABLE IS IN A CONFIG OTHER THAN 1
              IF(COMPOPS) VARABL(I,2)=1.0D0
              IF(VARABL(I,2).EQ.1.0D0) THEN
C     NON-CONFIGS VARIABLE MEANING CONFIG 1
C     THIS IS A LENS LEVEL VARIABLE CHANGE AND DERIVATIVE STUFF
C     GET THE DATA TYPE NUMBER OF THE VARIABLE
                  VTYPE=INT(VARABL(I,1))
C                          CURVATURE
                  IF(VTYPE.EQ.2.OR.VTYPE.EQ.1) THEN
C     SURFACE CURVATURE
C     NEW VALUE IS:
                      V1=XDUM(I)
                      ALENS(1,INT(VARABL(I,3)))=V1
C                     CURVATURE DONE
                  END IF
                  IF(VTYPE.EQ.10.OR.VTYPE.EQ.9) THEN
C     SURFACE TORIC CURVATURE
C     NEW VALUE IS:
                      V1=XDUM(I)
                      ALENS(24,INT(VARABL(I,3)))=V1
                  END IF
                  IF(VTYPE.GE.3.AND.VTYPE.LE.8) THEN
                      IF(VTYPE.EQ.3) ALTYPE=3
                      IF(VTYPE.EQ.4) ALTYPE=2
                      IF(VTYPE.EQ.5) ALTYPE=4
                      IF(VTYPE.EQ.6) ALTYPE=5
                      IF(VTYPE.EQ.7) ALTYPE=6
                      IF(VTYPE.EQ.8) ALTYPE=7
C     NEW VALUE IS:
                      V1=XDUM(I)
                      ALENS(ALTYPE,INT(VARABL(I,3)))=V1
                  END IF
                  IF(VTYPE.GE.11.AND.VTYPE.LE.25.OR.VTYPE.EQ.75.OR.VTYPE.GE.
     1            124.AND.VTYPE.LE.149) THEN
                      IF(VTYPE.EQ.11) ALTYPE=41
                      IF(VTYPE.EQ.12) ALTYPE=37
                      IF(VTYPE.EQ.13) ALTYPE=38
                      IF(VTYPE.EQ.14) ALTYPE=39
                      IF(VTYPE.EQ.15) ALTYPE=40
                      IF(VTYPE.EQ.16) ALTYPE=118
                      IF(VTYPE.EQ.17) ALTYPE=119
                      IF(VTYPE.EQ.18) ALTYPE=120
                      IF(VTYPE.EQ.19) ALTYPE=114
                      IF(VTYPE.EQ.20) ALTYPE=115
                      IF(VTYPE.EQ.21) ALTYPE=46
                      IF(VTYPE.EQ.22) ALTYPE=47
                      IF(VTYPE.EQ.23) ALTYPE=48
                      IF(VTYPE.EQ.24) ALTYPE=49
                      IF(VTYPE.EQ.25) ALTYPE=50
                      IF(VTYPE.EQ.75) ALTYPE=43
                      IF(VTYPE.EQ.124) ALTYPE=71
                      IF(VTYPE.EQ.125) ALTYPE=72
                      IF(VTYPE.EQ.126) ALTYPE=73
                      IF(VTYPE.EQ.127) ALTYPE=74
                      IF(VTYPE.EQ.128) ALTYPE=75
                      IF(VTYPE.EQ.129) ALTYPE=81
                      IF(VTYPE.EQ.130) ALTYPE=82
                      IF(VTYPE.EQ.131) ALTYPE=83
                      IF(VTYPE.EQ.132) ALTYPE=84
                      IF(VTYPE.EQ.133) ALTYPE=85
                      IF(VTYPE.EQ.134) ALTYPE=116
                      IF(VTYPE.EQ.135) ALTYPE=86
                      IF(VTYPE.EQ.136) ALTYPE=87
                      IF(VTYPE.EQ.137) ALTYPE=78
                      IF(VTYPE.EQ.138) ALTYPE=79
                      IF(VTYPE.EQ.139) ALTYPE=80
                      IF(VTYPE.EQ.140) ALTYPE=89
                      IF(VTYPE.EQ.141) ALTYPE=11
                      IF(VTYPE.EQ.142) ALTYPE=10
                      IF(VTYPE.EQ.143) ALTYPE=90
                      IF(VTYPE.EQ.144) ALTYPE=91
                      IF(VTYPE.EQ.145) ALTYPE=92
                      IF(VTYPE.EQ.146) ALTYPE=93
                      IF(VTYPE.EQ.147) ALTYPE=94
                      IF(VTYPE.EQ.148) ALTYPE=95
                      IF(VTYPE.EQ.149) ALTYPE=98
C     NEW VALUE IS:
                      V1=XDUM(I)
                      ALENS(ALTYPE,INT(VARABL(I,3)))=V1
                  END IF
                  IF(VTYPE.EQ.150) THEN
                      V1=XDUM(I)
                      GPREG(INT(VARABL(I,3)))=V1
                  END IF
                  IF(VTYPE.GE.250.AND.VTYPE.LE.4218) THEN
                      ALTYPE=VTYPE-249
C     NEW VALUE IS:
                      V1=VARABL(I,4)
C     RESET THE APPRORIATE ARRAY VALUE IN THE DEFORMABLE SURFACE
                      ISURF=INT(VARABL(I,3))
                      DEFGR1=ALENS(103,ISURF)
                      DEFGR2=ALENS(104,ISURF)
                      DEFGR3=ALENS(105,ISURF)
                      DEFGR4=ALENS(106,ISURF)
                      DEFGR5=ALENS(107,ISURF)
                      DEFGR6=0.0D0
                      DEFGR7=ALENS(109,ISURF)
                      DEFGR8=0.0D0
                      ACTNUM=ALTYPE
                      NEWDEFVAL=V1
                      ERR1=.FALSE.
                      ERR2=.FALSE.
                      CALL DEFGRIDS(6,ISURF,ERR1,ERR2)
                  END IF
                  IF(VTYPE.GE.27.AND.VTYPE.LE.74) THEN
C     SPECIAL SURFACE COEFFICIENTS
C     NEW VALUE IS:
                      V1=XDUM(I)
                      FTFL01((VTYPE-26),INT(VARABL(I,3)))=V1
C                       SPECIAL SURFACE COEFICIENTS DONE
                  END IF
                  IF(VTYPE.GE.76.AND.VTYPE.LE.123) THEN
C     SPECIAL SURFACE COEFFICIENTS
C     NEW VALUE IS:
                      V1=XDUM(I)
                      FTFL01((VTYPE-27),INT(VARABL(I,3)))=V1
                  END IF
              ELSE
C     CONFIGS VARIABLE
C     VCFG IS THE CONFIG NUMBER
                  VCFG=INT(VARABL(I,2))
C     VTYPE IS THE VARIABLE TYPE NUMBER AS USED IN THE VARIABLES ARRAYS
                  VTYPE=INT(VARABL(I,1))
C
C     FOR VARIABLE I, APPLY THE SPECIFIED CHANGE TO THE SPECIFIED
C     CONFIG, DO AN EOS AND EVALUATE ALL OPERANDS
C
C     THE OLD VARIABLE VALUE IS:
                  OLDV1=VARABL(I,4)
C     CONVERT THIS VALUE TO A CHARACTER*23 CHARACTER VARIABLE
C     OLDVA1 IS THE VALUE WE WILL USE IN THE RESTORATION PROCESS
C     AFTER THE DERIVATIVE IS CALCULATED
                  VADD=INT(VARABL(I,14))
                  IF(CFADD(VADD,1).EQ.1.AND.OLDV1.NE.0.0D0) OLDV1=1.0D0/OLDV1
                  IF(CFADD(VADD,1).EQ.9.AND.OLDV1.NE.0.0D0) OLDV1=1.0D0/OLDV1
                  N1=OLDV1
                  CALL AUXNTA
                  OLDAV1=AN1
C     THE NEW VARAIBLE VALUE IS JUST
                  V1=XDUM(I)
C     CONVERT THIS VALUE TO A CHARACTER*23 CHARACTER VARIABLE
                  IF(CFADD(VADD,1).EQ.1.AND.V1.NE.0.0D0) V1=1.0D0/V1
                  IF(CFADD(VADD,1).EQ.9.AND.V1.NE.0.0D0) V1=1.0D0/V1
                  N1=V1
                  CALL AUXNTA
                  AV1=AN1
C
C     THE POSITION IN THE CFADD,CFVAL AND CFCHAR ARRAYS WHERE THIS
C     VARIABLE IS FOUND IS:
C
                  IF(CFADD(VADD,1).GE.27.AND.CFADD(VADD,1).LE.74.OR.
     1            CFADD(VADD,1).GE.76.AND.CFADD(VADD,1).LE.123
     1            .OR.CFADD(VADD,1).EQ.141) THEN
                      CFVAL(VADD,2)=V1
                      CFCHAR(VADD,2)=AV1
                  ELSE
                      CFVAL(VADD,1)=V1
                      CFCHAR(VADD,1)=AV1
                  END IF
C     NOW UPDATE THE CONFIG ARRAY
C     NOW LOOK UP WHERE THIS CHARACTER REPRESENTATION OF THE NEW VALUE
C     SHOULD BE STUFFED INTO THE CONFIG ARRAYS CONFG AND
C     STUFF IT THERE.
C     NOW UPDATE THE CONFIG ARRAY
                  VADD=INT(VARABL(I,14))
                  IF(CFADD(VADD,1).GE.27.AND.CFADD(VADD,1).LE.74.OR.
     1            CFADD(VADD,1).GE.76.AND.CFADD(VADD,1).LE.123
     1            .OR.CFADD(VADD,1).EQ.141) THEN
                      CONFG(CFADD(VADD,3),CFADD(VADD,9))(CFADD(VADD,6):CFADD(VADD,7))
     1                =AV1(1:23)
                  ELSE
                      CONFG(CFADD(VADD,3),CFADD(VADD,9))(CFADD(VADD,4):CFADD(VADD,5))
     1                =AV1(1:23)
                  END IF
C
C     VARIABLE DONE
C     FINISHED WITH A CONFIG VARIABLE
              END IF
          END DO
C     EVALUATE THE MERIT FUNCTION
C     UPDATE THE LENS
          F6=1
          F1=0
          F22=0
          IF(.NOT.COMPOPS)LNSTYP=2
          IF(COMPOPS)LNSTYP=3
          CALL LNSEOS
C     CALCULATE OPERANDS AND LOAD THEM
          IF(.NOT.COMPOPS)OPCALC_TYPE=3
          IF(COMPOPS) OPCALC_TYPE=1
          CALL OPCALC
          IF(.NOT.COMPOPS.AND.F28.EQ.0) then
              FUNCIP=0                            !Add by ENDO
              RETURN
          end if
          IF(COMPOPS.AND.F31.EQ.0) then
              FUNCIP=0                            !Add by ENDO
              RETURN
          end if
          CALL OPLOAD
          IF(KILOPT) THEN
C     NO SOLUTION IS ATTEMPTED
              OUTLYNE='SOME OPERANDS ARE NOT CALCULABLE.'
              CALL SHOWIT(1)
              OUTLYNE='"ITER POWELL" CAN NOT PROCEED.'
              CALL SHOWIT(1)
              IF(.NOT.COMPOPS) F28=0
              IF(COMPOPS) F31=0
              CALL MACFAL
              FUNCIP=0.0D0
              RETURN
          END IF
          IF(.NOT.COMPOPS.AND.F28.EQ.0) then
              FUNCIP=0
              RETURN
          end if
          IF(COMPOPS.AND.F31.EQ.0) then
              FUNCIP=0
              RETURN
          end if
C     CALCULATE IPFMT
          IPFMT=IPFUN()
          FUNCIP=IPFMT
C
C     NOW RESTORE THE LENS
          DO I=1,VBCNT
              IF(VARABL(I,2).EQ.1.0D0) THEN
C     NON-CONFIGS VARIABLE MEANING CONFIG 1
C     THIS IS A LENS LEVEL VARIABLE CHANGE AND DERIVATIVE STUFF
C     GET THE DATA TYPE NUMBER OF THE VARIABLE
                  VTYPE=INT(VARABL(I,1))
C                          CURVATURE
                  IF(VTYPE.EQ.2.OR.VTYPE.EQ.1) THEN
C     SURFACE CURVATURE
C     RESTORE THE LENS
                      ALENS(1,INT(VARABL(I,3)))=VARABL(I,4)
C                       CURVATURE DONE
                  END IF
                  IF(VTYPE.EQ.10.OR.VTYPE.EQ.9) THEN
C     SURFACE TORIC CURVATURE
C     RESTORE THE LENS
                      ALENS(24,INT(VARABL(I,3)))=VARABL(I,4)
C                       TORIC CURVATURE DONE
                  END IF
                  IF(VTYPE.GE.3.AND.VTYPE.LE.8) THEN
                      IF(VTYPE.EQ.3) ALTYPE=3
                      IF(VTYPE.EQ.4) ALTYPE=2
                      IF(VTYPE.EQ.5) ALTYPE=4
                      IF(VTYPE.EQ.6) ALTYPE=5
                      IF(VTYPE.EQ.7) ALTYPE=6
                      IF(VTYPE.EQ.8) ALTYPE=7
C     RESTORE THE LENS
                      ALENS(ALTYPE,INT(VARABL(I,3)))=VARABL(I,4)
C                       THESE VARIABLES DONE
                  END IF
                  IF(VTYPE.GE.11.AND.VTYPE.LE.25.OR.VTYPE.EQ.75.OR.VTYPE.GE.
     1            124.AND.VTYPE.LE.149) THEN
                      IF(VTYPE.EQ.11) ALTYPE=41
                      IF(VTYPE.EQ.12) ALTYPE=37
                      IF(VTYPE.EQ.13) ALTYPE=38
                      IF(VTYPE.EQ.14) ALTYPE=39
                      IF(VTYPE.EQ.15) ALTYPE=40
                      IF(VTYPE.EQ.16) ALTYPE=118
                      IF(VTYPE.EQ.17) ALTYPE=119
                      IF(VTYPE.EQ.18) ALTYPE=120
                      IF(VTYPE.EQ.19) ALTYPE=114
                      IF(VTYPE.EQ.20) ALTYPE=115
                      IF(VTYPE.EQ.21) ALTYPE=46
                      IF(VTYPE.EQ.22) ALTYPE=47
                      IF(VTYPE.EQ.23) ALTYPE=48
                      IF(VTYPE.EQ.24) ALTYPE=49
                      IF(VTYPE.EQ.25) ALTYPE=50
                      IF(VTYPE.EQ.75) ALTYPE=43
                      IF(VTYPE.EQ.124) ALTYPE=71
                      IF(VTYPE.EQ.125) ALTYPE=72
                      IF(VTYPE.EQ.126) ALTYPE=73
                      IF(VTYPE.EQ.127) ALTYPE=74
                      IF(VTYPE.EQ.128) ALTYPE=75
                      IF(VTYPE.EQ.129) ALTYPE=81
                      IF(VTYPE.EQ.130) ALTYPE=82
                      IF(VTYPE.EQ.131) ALTYPE=83
                      IF(VTYPE.EQ.132) ALTYPE=84
                      IF(VTYPE.EQ.133) ALTYPE=85
                      IF(VTYPE.EQ.134) ALTYPE=116
                      IF(VTYPE.EQ.135) ALTYPE=86
                      IF(VTYPE.EQ.136) ALTYPE=87
                      IF(VTYPE.EQ.137) ALTYPE=78
                      IF(VTYPE.EQ.138) ALTYPE=79
                      IF(VTYPE.EQ.139) ALTYPE=80
                      IF(VTYPE.EQ.140) ALTYPE=89
                      IF(VTYPE.EQ.141) ALTYPE=11
                      IF(VTYPE.EQ.142) ALTYPE=10
                      IF(VTYPE.EQ.143) ALTYPE=90
                      IF(VTYPE.EQ.144) ALTYPE=91
                      IF(VTYPE.EQ.145) ALTYPE=92
                      IF(VTYPE.EQ.146) ALTYPE=93
                      IF(VTYPE.EQ.147) ALTYPE=94
                      IF(VTYPE.EQ.148) ALTYPE=95
                      IF(VTYPE.EQ.149) ALTYPE=98
C     RESTORE THE LENS
                      ALENS(ALTYPE,INT(VARABL(I,3)))=VARABL(I,4)
                  END IF
                  IF(VTYPE.EQ.150) THEN
                      V1=VARABL(I,4)
                      GPREG(INT(VARABL(I,3)))=V1
                  END IF
                  IF(VTYPE.GE.250.AND.VTYPE.LE.4218) THEN
                      ALTYPE=VTYPE-249
C     NEW VALUE IS:
                      V1=VARABL(I,4)
C     RESET THE APPRORIATE ARRAY VALUE IN THE DEFORMABLE SURFACE
                      ISURF=INT(VARABL(I,3))
                      DEFGR1=ALENS(103,ISURF)
                      DEFGR2=ALENS(104,ISURF)
                      DEFGR3=ALENS(105,ISURF)
                      DEFGR4=ALENS(106,ISURF)
                      DEFGR5=ALENS(107,ISURF)
                      DEFGR6=0.0D0
                      DEFGR7=ALENS(109,ISURF)
                      DEFGR8=0.0D0
                      ACTNUM=ALTYPE
                      NEWDEFVAL=V1
                      ERR1=.FALSE.
                      ERR2=.FALSE.
                      CALL DEFGRIDS(6,ISURF,ERR1,ERR2)
                  END IF
                  IF(VTYPE.GE.27.AND.VTYPE.LE.74) THEN
C     SPECIAL SURFACE COEFFICIENTS
C     RESTORE THE LENS
                      FTFL01((VTYPE-26),INT(VARABL(I,3)))=VARABL(I,4)
C                       SPECIAL SURFACE COEFICIENTS DONE
                  END IF
                  IF(VTYPE.GE.76.AND.VTYPE.LE.123) THEN
C     SPECIAL SURFACE COEFFICIENTS
C     RESTORE THE LENS
                      FTFL01((VTYPE-27),INT(VARABL(I,3)))=VARABL(I,4)
C                       SPECIAL SURFACE COEFICIENTS DONE
                  END IF
              ELSE
C     CONFIGS VARIABLE
C     VCFG IS THE CONFIG NUMBER
                  VCFG=INT(VARABL(I,2))
C     VTYPE IS THE VARIABLE TYPE NUMBER AS USED IN THE VARIABLES ARRAYS
                  VTYPE=INT(VARABL(I,1))
C
C     FOR VARIABLE I, APPLY THE SPECIFIED CHANGE TO THE SPECIFIED
C     CONFIG, DO AN EOS AND EVALUATE ALL OPERANDS
C
C     THE OLD VARIABLE VALUE IS:
                  OLDV1=VARABL(I,4)
C     CONVERT THIS VALUE TO A CHARACTER*23 CHARACTER VARIABLE
C     OLDVA1 IS THE VALUE WE WILL USE IN THE RESTORATION PROCESS
C     AFTER THE DERIVATIVE IS CALCULATED
                  VADD=INT(VARABL(I,14))
                  IF(CFADD(VADD,1).EQ.1.AND.OLDV1.NE.0.0D0) OLDV1=1.0D0/OLDV1
                  IF(CFADD(VADD,1).EQ.9.AND.OLDV1.NE.0.0D0) OLDV1=1.0D0/OLDV1
                  N1=OLDV1
                  CALL AUXNTA
                  OLDAV1=AN1
C
C     THE POSITION IN THE CFADD,CFVAL AND CFCHAR ARRAYS WHERE THIS
C     VARIABLE IS FOUND IS:
C
C
C     STUFF OLDAV1
                  IF(CFADD(VADD,1).GE.27.AND.CFADD(VADD,1).LE.74.OR.
     1            CFADD(VADD,1).GE.76.AND.CFADD(VADD,1).LE.123
     1            .OR.CFADD(VADD,1).EQ.141) THEN
                      CFVAL(VADD,2)=OLDV1
                      CFCHAR(VADD,2)=OLDAV1
C     NOW UPDATE THE CONFIG ARRAY
                      CONFG(CFADD(VADD,3),CFADD(VADD,9))(CFADD(VADD,6):CFADD(VADD,6))
     1                =OLDAV1(1:23)
                  ELSE
                      CFVAL(VADD,1)=OLDV1
                      CFCHAR(VADD,1)=OLDAV1
C     NOW UPDATE THE CONFIG ARRAY
                      CONFG(CFADD(VADD,3),CFADD(VADD,9))(CFADD(VADD,4):CFADD(VADD,5))
     1                =OLDAV1(1:23)
                  END IF

C     VARIABLE DONE
C     FINISHED WITH A CONFIG VARIABLE
              END IF
          END DO
C
C     WE JUST DID THE LAST CALCULATION AND WE WANT TO
C     RESTORE THE ORIGINAL OPERANDS
          OPERND(1:OPCNT,1:20)=OLDOP(1:OPCNT,1:20)
C     UPDATE THE LENS OR THE PERMANENT LENS WILL BE ******!
          F6=1
          F1=0
          F22=0
          IF(.NOT.COMPOPS)LNSTYP=2
          IF(COMPOPS)LNSTYP=3
          CALL LNSEOS
          RETURN
      END


      FUNCTION IPFUN()
          IMPLICIT NONE
          REAL*8 IPFUN
          INTEGER I
          INCLUDE 'datsub.inc'
          INCLUDE 'datmai.inc'
C
          IPFUN=0.0D0
          DO I=1,OPCNT
              IPFUN=IPFUN+(OPERND(I,14)**2)
          END DO
          RETURN
      END


      SUBROUTINE ITERIP(VN,IITTPP)
          IMPLICIT NONE
C     SUBROUTINE WHICH RUNS ONE CYCLE OF POWELL'S OPTIMIZATION
C     SPECIAL VARIABLES ARE IN DATLEN.FOR
C     AND DATSUB.FOR
C
          REAL*8 P
C
          DIMENSION P(:)
C
          ALLOCATABLE :: P
C
          REAL*8 OCRIT(10),NCRIT(10)
C
          COMMON/ONCRIT/OCRIT,NCRIT
C
          INTEGER NUB,ALLOERR,PCNT
C
          INTEGER ISURF
C
          REAL*8 NEWDEFVAL
C
          COMMON/DEFVALCOM/NEWDEFVAL
C
          INTEGER CMPSURF(10)
C
          COMMON/SURFCMP/CMPSURF
C
          REAL*8 MOT(1:10)
C
          COMMON/LOCOMOTION/MOT
C
          INTEGER VN,VTYPE,ALTYPE,VADD,VCFG,IITTPP
C
          REAL*8 X(1:100000),V1,N1
C
!      LOGICAL LVAL
!      LOGICAL ITERROR
C
          CHARACTER AN1*23,AV1*23
C
          COMMON/SVD2/X
C
          INTEGER I,ITER
          COMMON/ITTERY/ITER
C
          REAL*8 FRET,V
          DIMENSION V(:,:)
          ALLOCATABLE :: V
          COMMON/FRETTY/FRET
C
          LOGICAL PLL,ERR1,ERR2
          LOGICAL COMPOPS
          COMMON/OPSCOMP/COMPOPS
          COMMON/PLLPLL/PLL
C
          COMMON/CAUX1/N1,AN1
C
          INCLUDE 'datsub.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          INCLUDE 'datcfg.inc'
          INCLUDE 'datmac.inc'
C
          INTEGER J
          REAL*8 FUNCIP
          EXTERNAL FUNCIP
C
C
          COMPOPS=.FALSE.
          IF(IITTPP.NE.0) THEN
              COMPOPS=.TRUE.
              F31=1
          END IF
          PCNT=VBCNT+1
C
          DEALLOCATE(V,STAT=ALLOERR)
          DEALLOCATE(P,STAT=ALLOERR)
          ALLOCATE(V(VN,VN),STAT=ALLOERR)
          ALLOCATE(P(PCNT),STAT=ALLOERR)
C
          IF(OPCNT.EQ.0.OR.VBCNT.EQ.0) THEN
              WRITE(OUTLYNE,*)
     1        '"ITER POWL" OR "IT P" OR "ITER POWELL"'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)
     1        'REQUIRES VARIABLES AND OPERANDS TO EXIST'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)
     1        'BEFORE IT CAN FUNCTION'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'NO ACTION TAKEN'
              CALL SHOWIT(1)
              CALL MACFAL
              DEALLOCATE(V,P,STAT=ALLOERR)
              RETURN
          END IF
          IF(.NOT.FMTEXT) THEN
              SAVE_KDP(4)=SAVEINPT(4)
              IF(.NOT.COMPOPS) F28=1
              IF(COMPOPS) F31=1
              MSG=.FALSE.
              OPTMES=.FALSE.
              WC='FMT'
              IF(F31.EQ.1) THEN
                  WQ='NP'
                  SQ=1
              ELSE
                  WQ='        '
                  SQ=0
              END IF
              SN=0
              SST=0
              WS=' '
              DF1=1
              DF2=1
              DF3=1
              DF4=1
              DF5=1
              S1=0
              S2=0
              S3=0
              S4=0
              S5=0
              STI=0
              CALL FMT4
              REST_KDP(4)=RESTINPT(4)
          END IF
          OLDOP(1:OPCNT,1:20)=OPERND(1:OPCNT,1:20)
          OPCALC_TYPE=1
          CALL OPCALC
          IF(.NOT.COMPOPS.AND.F28.EQ.0) RETURN
          IF(COMPOPS.AND.F31.EQ.0) RETURN
          CALL OPLOAD
          IF(.NOT.COMPOPS.AND.F28.EQ.0) RETURN
          IF(COMPOPS.AND.F31.EQ.0) RETURN
          OPERND(1:OPCNT,1:20)=OLDOP(1:OPCNT,1:20)
C
C     SET THE ORIGINAL P AND XI ARRAY VALUES
          DO I=1,VBCNT
              P(I)=VARABL(I,4)
              DO J=1,VBCNT
C     BTB IS ARRAY XI IN NUM REC
                  IF(I.EQ.J) V(I,J)=DINMUL*VARABL(I,8)
                  IF(I.NE.J) V(I,J)=0.0D0
              END DO
          END DO
          CALL POWELL(V,VN,PCNT,P)
          NUB=VBCNT+1
          X(1:VBCNT)=P(1:VBCNT)

C     NOW APPLY THE SOLUTION VECTOR X(I) TO THE VARABL ARRAY AND TO THE LENS
C     THE ITH SOLUTION VECTOR COMPONENT APPLIES TO THE ITH VARIABLE
          LCVLCV=0.0D0
          DO I=1,VBCNT
C
C     THE CURRENT VARIABLE VALUE BECOMES THE PREVIOUS VARIABLE VALUE
              VARABL(I,5)=VARABL(I,4)
C
C     THE NEW SOLUTION VECTOR ADDED TO THE PREVIOS VARIABLE VALUE
C     BECOMES THE NEW SOLUTION VECTOR
C     VALUE
              LCVLCV=LCVLCV+DABS(((X(I)))/(DINMUL*VARABL(I,8)))
              VARABL(I,4)=VARABL(I,4)+(X(I))
C
              IF(VARABL(I,1).EQ.1.0D0) THEN
                  IF(VARABL(I,9).LT.THMINLIM) VARABL(I,9)=THMINLIM
                  IF(VARABL(I,10).GT.THMAXLIM) VARABL(I,10)=THMAXLIM
              END IF
              IF(VARABL(I,1).EQ.2.0D0.OR.VARABL(I,1).EQ.10.0D0) THEN
                  IF(RDNEGLIM.EQ.0.0D0) RDNEGLIM=-1.0D-20
                  IF(RDPOSLIM.EQ.0.0D0) RDPOSLIM=1.0D-20
                  IF(VARABL(I,9).LT.(1.0D0/RDNEGLIM)) VARABL(I,9)=1.0D0/RDNEGLIM
                  IF(VARABL(I,10).GT.(1.0D0/RDPOSLIM)) VARABL(I,10)=1.0D0/RDPOSLIM
              END IF
C     BOUNDS CHECKER
              IF(VARABL(I,4).LT.VARABL(I,9)) THEN
                  VARABL(I,4)=VARABL(I,9)
                  WRITE(OUTLYNE,*)
     1            'WARNING: '
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)
     1            'VARIABLE # ',I,' HAS REACHED ITS LOWER BOUNDARY VALUE'
                  CALL SHOWIT(1)
              END IF
              IF(VARABL(I,4).GT.VARABL(I,10)) THEN
                  VARABL(I,4)=VARABL(I,10)
                  WRITE(OUTLYNE,*)
     1            'WARNING: '
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)
     1            'VARIABLE # ',I,' HAS REACHED ITS UPPER BOUNDARY VALUE'
                  CALL SHOWIT(1)
              END IF
C
C
C     THE VARIABLE CHANGE IS (THE NEW CURRENT VALUE-OLD LAST CURRENT VALUE)
              VARABL(I,6)=VARABL(I,4)-VARABL(I,5)
C
C     DINCRS ARE ONLY CHANGED BY HAND AND BY THE ITER ADJUST COMMAND
C
          END DO
          LCVLCV=(LCVLCV)/DBLE(VBCNT)
C
C     NOW APPLY THE CHANGE VECTOR TO THE LENS
C**********************************************************************
          DO I=1,VBCNT
              IF(I.LE.10) CMPSURF(I)=INT(VARABL(I,3))
C     FIRST, IF THE VARIABLE IS A CONFIG 1 VARIABLE
C     NEXT IF THE VARIABLE IS IN A CONFIG OTHER THAN 1
              IF(VARABL(I,2).EQ.1.0D0) THEN
C     NON-CONFIGS VARIABLE MEANING CONFIG 1
C     THIS IS A LENS LEVEL VARIABLE CHANGE
C     GET THE DATA TYPE NUMBER OF THE VARIABLE
                  VTYPE=INT(VARABL(I,1))
C                          CURVATURE
                  IF(VTYPE.EQ.2.OR.VTYPE.EQ.1) THEN
C     SURFACE CURVATURE
C     NEW VALUE IS:
                      V1=VARABL(I,4)
                      ALENS(1,INT(VARABL(I,3)))=V1
C                       CURVATURE DONE
                  END IF
                  IF(VTYPE.EQ.10.OR.VTYPE.EQ.9) THEN
C     SURFACE TORIC CURVATURE
C     NEW VALUE IS:
                      V1=VARABL(I,4)
                      ALENS(24,INT(VARABL(I,3)))=V1
C                       TORIC CURVATURE DONE
                  END IF
                  IF(VTYPE.GE.3.AND.VTYPE.LE.8) THEN
                      IF(VTYPE.EQ.3) ALTYPE=3
                      IF(VTYPE.EQ.4) ALTYPE=2
                      IF(VTYPE.EQ.5) ALTYPE=4
                      IF(VTYPE.EQ.6) ALTYPE=5
                      IF(VTYPE.EQ.7) ALTYPE=6
                      IF(VTYPE.EQ.8) ALTYPE=7
C     NEW VALUE IS:
                      V1=VARABL(I,4)
                      ALENS(ALTYPE,INT(VARABL(I,3)))=V1
C                       THESE VARIABLES DONE
                  END IF
                  IF(VTYPE.GE.11.AND.VTYPE.LE.25.OR.VTYPE.EQ.75.OR.VTYPE.GE.
     1            124.AND.VTYPE.LE.149) THEN
                      IF(VTYPE.EQ.11) ALTYPE=41
                      IF(VTYPE.EQ.12) ALTYPE=37
                      IF(VTYPE.EQ.13) ALTYPE=38
                      IF(VTYPE.EQ.14) ALTYPE=39
                      IF(VTYPE.EQ.15) ALTYPE=40
                      IF(VTYPE.EQ.16) ALTYPE=118
                      IF(VTYPE.EQ.17) ALTYPE=119
                      IF(VTYPE.EQ.18) ALTYPE=120
                      IF(VTYPE.EQ.19) ALTYPE=114
                      IF(VTYPE.EQ.20) ALTYPE=115
                      IF(VTYPE.EQ.21) ALTYPE=46
                      IF(VTYPE.EQ.22) ALTYPE=47
                      IF(VTYPE.EQ.23) ALTYPE=48
                      IF(VTYPE.EQ.24) ALTYPE=49
                      IF(VTYPE.EQ.25) ALTYPE=50
                      IF(VTYPE.EQ.75) ALTYPE=43
                      IF(VTYPE.EQ.124) ALTYPE=71
                      IF(VTYPE.EQ.125) ALTYPE=72
                      IF(VTYPE.EQ.126) ALTYPE=73
                      IF(VTYPE.EQ.127) ALTYPE=74
                      IF(VTYPE.EQ.128) ALTYPE=75
                      IF(VTYPE.EQ.129) ALTYPE=81
                      IF(VTYPE.EQ.130) ALTYPE=82
                      IF(VTYPE.EQ.131) ALTYPE=83
                      IF(VTYPE.EQ.132) ALTYPE=84
                      IF(VTYPE.EQ.133) ALTYPE=85
                      IF(VTYPE.EQ.134) ALTYPE=116
                      IF(VTYPE.EQ.135) ALTYPE=86
                      IF(VTYPE.EQ.136) ALTYPE=87
                      IF(VTYPE.EQ.137) ALTYPE=78
                      IF(VTYPE.EQ.138) ALTYPE=79
                      IF(VTYPE.EQ.139) ALTYPE=80
                      IF(VTYPE.EQ.140) ALTYPE=89
                      IF(VTYPE.EQ.141) ALTYPE=11
                      IF(VTYPE.EQ.142) ALTYPE=10
                      IF(VTYPE.EQ.143) ALTYPE=90
                      IF(VTYPE.EQ.144) ALTYPE=91
                      IF(VTYPE.EQ.145) ALTYPE=92
                      IF(VTYPE.EQ.146) ALTYPE=93
                      IF(VTYPE.EQ.147) ALTYPE=94
                      IF(VTYPE.EQ.148) ALTYPE=95
                      IF(VTYPE.EQ.149) ALTYPE=98
C     NEW VALUE IS:
                      V1=VARABL(I,4)
                      ALENS(ALTYPE,INT(VARABL(I,3)))=V1
                  END IF
                  IF(VTYPE.EQ.150) THEN
                      V1=VARABL(I,4)
                      GPREG(INT(VARABL(I,3)))=V1
                  END IF
                  IF(VTYPE.GE.250.AND.VTYPE.LE.4218) THEN
                      ALTYPE=VTYPE-249
C     NEW VALUE IS:
                      V1=VARABL(I,4)
C     RESET THE APPRORIATE ARRAY VALUE IN THE DEFORMABLE SURFACE
                      ISURF=INT(VARABL(I,3))
                      DEFGR1=ALENS(103,ISURF)
                      DEFGR2=ALENS(104,ISURF)
                      DEFGR3=ALENS(105,ISURF)
                      DEFGR4=ALENS(106,ISURF)
                      DEFGR5=ALENS(107,ISURF)
                      DEFGR6=0.0D0
                      DEFGR7=ALENS(109,ISURF)
                      DEFGR8=0.0D0
                      ACTNUM=ALTYPE
                      NEWDEFVAL=V1
                      ERR1=.FALSE.
                      ERR2=.FALSE.
                      CALL DEFGRIDS(6,ISURF,ERR1,ERR2)
                  END IF
                  IF(VTYPE.GE.27.AND.VTYPE.LE.74) THEN
C     SPECIAL SURFACE COEFFICIENTS
C     NEW VALUE IS:
                      V1=VARABL(I,4)
                      FTFL01((VTYPE-26),INT(VARABL(I,3)))=V1
C                       SPECIAL SURFACE COEFICIENTS DONE
                  END IF
                  IF(VTYPE.GE.76.AND.VTYPE.LE.123) THEN
C     SPECIAL SURFACE COEFFICIENTS
C     NEW VALUE IS:
                      V1=VARABL(I,4)
                      FTFL01((VTYPE-27),INT(VARABL(I,3)))=V1
C                       SPECIAL SURFACE COEFICIENTS DONE
                  END IF
C
              ELSE
C     CONFIGS VARIABLE
C     VCFG IS THE CONFIG NUMBER
                  VCFG=INT(VARABL(I,2))
C     VTYPE IS THE VARIABLE TYPE NUMBER AS USED IN THE VARIABLES ARRAYS
                  VTYPE=INT(VARABL(I,1))
C
C     FOR VARIABLE I, APPLY THE SPECIFIED CHANGE TO THE SPECIFIED
C     CONFIG
C
C     THE NEW VARAIBLE VALUE IS JUST
                  V1=VARABL(I,4)
C     CONVERT THIS VALUE TO A CHARACTER*23 CHARACTER VARIABLE
                  VADD=INT(VARABL(I,14))
                  IF(CFADD(VADD,1).EQ.1.AND.V1.NE.0.0D0) V1=1.0D0/V1
                  IF(CFADD(VADD,1).EQ.9.AND.V1.NE.0.0D0) V1=1.0D0/V1
                  N1=V1
                  CALL AUXNTA
                  AV1=AN1
C
C     THE POSITION IN THE CFADD,CFVAL AND CFCHAR ARRAYS WHERE THIS
C     VARIABLE IS FOUND IS:
C
                  IF(CFADD(VADD,1).GE.27.AND.CFADD(VADD,1).LE.74.OR.
     1            CFADD(VADD,1).GE.76.AND.CFADD(VADD,1).LE.123
     1            .OR.CFADD(VADD,1).EQ.141) THEN
                      CFVAL(VADD,2)=V1
                      CFCHAR(VADD,2)=AV1
C     NOW UPDATE THE CONFIG ARRAY
                      CONFG(CFADD(VADD,3),CFADD(VADD,9))(CFADD(VADD,6):CFADD(VADD,7))
     1                =AV1(1:23)
                  ELSE
                      CFVAL(VADD,1)=V1
                      CFCHAR(VADD,1)=AV1
C     NOW UPDATE THE CONFIG ARRAY
                      CONFG(CFADD(VADD,3),CFADD(VADD,9))(CFADD(VADD,4):CFADD(VADD,5))
     1                =AV1(1:23)
                  END IF
C     NOW LOOK UP WHERE THIS CHARACTER REPRESENTATION OF THE NEW VALUE
C     SHOULD BE STUFFED INTO THE CONFIG ARRAYS CONFG AND
C     STUFF IT THERE.
C
C     NOW LOOP BACK AND REPEAT FOR THE NEXT VARIABLE.
C     FINISHED WITH A CONFIG VARIABLE
              END IF
C     LOOP TO NEXT VARIABL
          END DO
C     UPDATE THE LENS OR THE PERMANENT LENS WILL BE ******!
          CALL FIXDEFORMFILE
          F6=1
          F1=0
          F22=0
          IF(.NOT.COMPOPS)LNSTYP=2
          IF(COMPOPS)LNSTYP=3
          CALL LNSEOS
C**********************************************************************
C     AND RE-EVALUATE THE OPERANDS AND DISPLAY
C     THE NEW FIGURE OF MERIT, THE OLD FIGURE OF MERIT  AND ITS CHANGE
          SAVE_KDP(4)=SAVEINPT(4)
          IF(.NOT.COMPOPS) F28=1
          IF(COMPOPS) F31=1
          MSG=.FALSE.
          OPTMES=.FALSE.
          WC='FMT'
          IF(F31.EQ.1) THEN
              WQ='NP'
              SQ=1
          ELSE
              WQ='        '
              SQ=0
          END IF
          SN=0
          SST=0
          WS=' '
          DF1=1
          DF2=1
          DF3=1
          DF4=1
          DF5=1
          S1=0
          S2=0
          S3=0
          S4=0
          S5=0
          STI=0
          CALL FMT2
          REST_KDP(4)=RESTINPT(4)
          DEALLOCATE(V,P,STAT=ALLOERR)
          RETURN
      END
