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

C       SECOND SET OF OPTIMIZATION ROUTINES

C SUB VARBLL.FOR
      SUBROUTINE VARBLL
C
          IMPLICIT NONE
C
          INTEGER I,IC,NCNT,J,VALT,DFDINC,VBSURF,I4,ISURF
C
          COMMON/SURFVB/VBSURF
C
          CHARACTER A4*4
C
          LOGICAL VAROK,CNOT,GOFORIT,ISRDCV,ISRDCVT,ERR1,ERR2
C
          REAL*8 WEIT,DINCR,VLOW,VHIGH,WORD2,SYS12,SYS13,REFHTT
     1    ,V4,V5,WAVERCW,HM1,HC1
C
          INCLUDE 'datsub.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          INCLUDE 'datcfg.inc'
C
C       THIS IS SUBROUTINE VARBLL. THIS IS THE SUBROUTINE WHICH
C       HANDLES VARIABLE INPUT AND VARIABLE UPDATE COMMANDS AND VARIABLE
C       OUTPUT COMMANDS AT THE CMD LEVEL
C
C       THE ARRAY VARABL STORES VARIABLE INFORMATION
C       IT IS PASSED IN COMMON IN THE INCLUDE FILE DATSUB.FOR
C
C       VARABL(I,J) WHERE I COUNTS THE NUMBER OF VARIABLE ENTRIES
C       AND J TAKES ON THE FOLLOWING VALUES AND MEANIINGS.
C
C       J=1  > 1 THROUGH 150 AND 250 AND ON FOR ACT., A VARIABLE TYPE DESIGNATOR
C       J=2  > 1 THROUGH MAXCFG, THE CONFIGURATION DESIGNATOR
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
C
          IF(WC.EQ.'M'.OR.WC.EQ.'C') THEN
              CALL MESCOM
              RETURN
          END IF
C
C       'VB' AND 'VBA' OUTPUT VARIABLE DATA FROM INSIDE
C       AND OUTSIDE OF THE VARIABLE SUBFILE VIA SUBROUTINE VBA.FOR.
          IF(WC.EQ.'VBA'.OR.WC.EQ.'VB') THEN
              CALL VBA
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
              F29=0
              IF(VBCNT.EQ.0) THEN
                  WRITE(OUTLYNE,*)'THE VARIABLE SUBFILE IS EMPTY'
                  CALL SHOWIT(1)
              END IF
              CALL VARCLN
              F1=1
              F29=0
              RETURN
C       ACTION COMPLETED
          END IF
C
C       EOS DONE
C***********************************************************************
C
C       NOW DO WC=CFG
C
          IF(WC.EQ.'CFG') THEN
              IF(SST.EQ.1.OR.SQ.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1.OR.
     1        S3.EQ.1.OR.S2.EQ.1) THEN
                  WRITE(OUTLYNE,*)'"CFG" ONLY TAKES NUMERIC WORD #1 INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(DF1.EQ.1) THEN
                  WRITE(OUTLYNE,*)'"CFG" REQUIRES EXPLICIT NUMERIC WORD #1 INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(INT(W1).GT.MAXCFG) THEN
                  WRITE(OUTLYNE,*)'NUMERIC WORD #1 (CFG#)'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'IS BEYOND CURRENTLY DEFINED BOUNDS'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(INT(W1).GT.LASCFG.AND.INT(W1).NE.1) THEN
                  WRITE(OUTLYNE,*)
     1            'REQUESTED CONFIGURATION IS CURRENTLY EMPTY'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'VARIABLE ASSIGNMENT CAN NOT BE MADE'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              VBCFG=INT(W1)
              CALL VARCLN
              RETURN
          END IF
C
C       NOW DO WC=DEL
          IF(WC.EQ.'DEL') THEN
              IF(F29.NE.2) THEN
                  WRITE(OUTLYNE,*)
     1            '"DEL" IS ONLY AVAILABLE FROM THE "UPDATE VARIABLE" LEVEL'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'NO ACTION TAKEN'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(SST.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1
     1        ) THEN
                  WRITE(OUTLYNE,*)
     1            '"DEL" ONLY TAKES QUALIFIER AND NUMERIC WORDS #1 AND #2'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(DF1.EQ.1.OR.SQ.EQ.0) THEN
                  WRITE(OUTLYNE,*)
     1            '"DEL" REQUIRES EXPLICIT QUALIFIER AND NUMERIC WORD #1 INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C     DEFAULT CONFIG IS #1
              IF(DF2.EQ.1) WORD2=1.0D0
              IF(DF2.EQ.0) WORD2=W2
C     CHECK FOR VALID QUALIFIER WORDS AND ASSIGN ASSOCIATED NUMERICAL
C     VALUES
              DELVAL = -1
              IF(WQ.EQ.'CV      ') DELVAL=2
              IF(WQ.EQ.'TH      ') DELVAL=3
              IF(WQ.EQ.'CC      ') DELVAL=4
              IF(WQ.EQ.'AD      ') DELVAL=5
              IF(WQ.EQ.'AE      ') DELVAL=6
              IF(WQ.EQ.'AF      ') DELVAL=7
              IF(WQ.EQ.'AG      ') DELVAL=8
              IF(WQ.EQ.'CVTOR   ') DELVAL=10
              IF(WQ.EQ.'CCTOR   ') DELVAL=11
              IF(WQ.EQ.'ADTOR   ') DELVAL=12
              IF(WQ.EQ.'AETOR   ') DELVAL=13
              IF(WQ.EQ.'AFTOR   ') DELVAL=14
              IF(WQ.EQ.'AGTOR   ') DELVAL=15
              IF(WQ.EQ.'ALPHA   ') DELVAL=16
              IF(WQ.EQ.'BETA    ') DELVAL=17
              IF(WQ.EQ.'GAMMA   ') DELVAL=18
              IF(WQ.EQ.'XD      ') DELVAL=19
              IF(WQ.EQ.'YD      ') DELVAL=20
              IF(WQ.EQ.'N1      ') DELVAL=21
              IF(WQ.EQ.'N2      ') DELVAL=22
              IF(WQ.EQ.'N3      ') DELVAL=23
              IF(WQ.EQ.'N4      ') DELVAL=24
              IF(WQ.EQ.'N5      ') DELVAL=25
              IF(WQ.EQ.'C1      ') DELVAL=27
              IF(WQ.EQ.'C2      ') DELVAL=28
              IF(WQ.EQ.'C3      ') DELVAL=29
              IF(WQ.EQ.'C4      ') DELVAL=30
              IF(WQ.EQ.'C5      ') DELVAL=31
              IF(WQ.EQ.'C6      ') DELVAL=32
              IF(WQ.EQ.'C7      ') DELVAL=33
              IF(WQ.EQ.'C8      ') DELVAL=34
              IF(WQ.EQ.'C9      ') DELVAL=35
              IF(WQ.EQ.'C10     ') DELVAL=36
              IF(WQ.EQ.'C11     ') DELVAL=37
              IF(WQ.EQ.'C12     ') DELVAL=38
              IF(WQ.EQ.'C13     ') DELVAL=39
              IF(WQ.EQ.'C14     ') DELVAL=40
              IF(WQ.EQ.'C15     ') DELVAL=41
              IF(WQ.EQ.'C16     ') DELVAL=42
              IF(WQ.EQ.'C17     ') DELVAL=43
              IF(WQ.EQ.'C18     ') DELVAL=44
              IF(WQ.EQ.'C19     ') DELVAL=45
              IF(WQ.EQ.'C20     ') DELVAL=46
              IF(WQ.EQ.'C21     ') DELVAL=47
              IF(WQ.EQ.'C22     ') DELVAL=48
              IF(WQ.EQ.'C23     ') DELVAL=49
              IF(WQ.EQ.'C24     ') DELVAL=50
              IF(WQ.EQ.'C25     ') DELVAL=51
              IF(WQ.EQ.'C26     ') DELVAL=52
              IF(WQ.EQ.'C27     ') DELVAL=53
              IF(WQ.EQ.'C28     ') DELVAL=54
              IF(WQ.EQ.'C29     ') DELVAL=55
              IF(WQ.EQ.'C30     ') DELVAL=56
              IF(WQ.EQ.'C31     ') DELVAL=57
              IF(WQ.EQ.'C32     ') DELVAL=58
              IF(WQ.EQ.'C33     ') DELVAL=59
              IF(WQ.EQ.'C34     ') DELVAL=60
              IF(WQ.EQ.'C35     ') DELVAL=61
              IF(WQ.EQ.'C36     ') DELVAL=62
              IF(WQ.EQ.'C37     ') DELVAL=63
              IF(WQ.EQ.'C38     ') DELVAL=64
              IF(WQ.EQ.'C39     ') DELVAL=65
              IF(WQ.EQ.'C40     ') DELVAL=66
              IF(WQ.EQ.'C41     ') DELVAL=67
              IF(WQ.EQ.'C42     ') DELVAL=68
              IF(WQ.EQ.'C43     ') DELVAL=69
              IF(WQ.EQ.'C44     ') DELVAL=70
              IF(WQ.EQ.'C45     ') DELVAL=71
              IF(WQ.EQ.'C46     ') DELVAL=72
              IF(WQ.EQ.'C47     ') DELVAL=73
              IF(WQ.EQ.'C48     ') DELVAL=74
              IF(WQ.EQ.'AC      ') DELVAL=75
              IF(WQ.EQ.'C49     ') DELVAL=76
              IF(WQ.EQ.'C50     ') DELVAL=77
              IF(WQ.EQ.'C51     ') DELVAL=78
              IF(WQ.EQ.'C52     ') DELVAL=79
              IF(WQ.EQ.'C53     ') DELVAL=80
              IF(WQ.EQ.'C54     ') DELVAL=81
              IF(WQ.EQ.'C55     ') DELVAL=82
              IF(WQ.EQ.'C56     ') DELVAL=83
              IF(WQ.EQ.'C57     ') DELVAL=84
              IF(WQ.EQ.'C58     ') DELVAL=85
              IF(WQ.EQ.'C59     ') DELVAL=86
              IF(WQ.EQ.'C60     ') DELVAL=87
              IF(WQ.EQ.'C61     ') DELVAL=88
              IF(WQ.EQ.'C62     ') DELVAL=89
              IF(WQ.EQ.'C63     ') DELVAL=90
              IF(WQ.EQ.'C64     ') DELVAL=91
              IF(WQ.EQ.'C65     ') DELVAL=92
              IF(WQ.EQ.'C66     ') DELVAL=93
              IF(WQ.EQ.'C67     ') DELVAL=94
              IF(WQ.EQ.'C68     ') DELVAL=95
              IF(WQ.EQ.'C69     ') DELVAL=96
              IF(WQ.EQ.'C70     ') DELVAL=97
              IF(WQ.EQ.'C71     ') DELVAL=98
              IF(WQ.EQ.'C72     ') DELVAL=99
              IF(WQ.EQ.'C73     ') DELVAL=100
              IF(WQ.EQ.'C74     ') DELVAL=101
              IF(WQ.EQ.'C75     ') DELVAL=102
              IF(WQ.EQ.'C76     ') DELVAL=103
              IF(WQ.EQ.'C77     ') DELVAL=104
              IF(WQ.EQ.'C78     ') DELVAL=105
              IF(WQ.EQ.'C79     ') DELVAL=106
              IF(WQ.EQ.'C80     ') DELVAL=107
              IF(WQ.EQ.'C81     ') DELVAL=108
              IF(WQ.EQ.'C82     ') DELVAL=109
              IF(WQ.EQ.'C83     ') DELVAL=110
              IF(WQ.EQ.'C84     ') DELVAL=111
              IF(WQ.EQ.'C85     ') DELVAL=112
              IF(WQ.EQ.'C86     ') DELVAL=113
              IF(WQ.EQ.'C87     ') DELVAL=114
              IF(WQ.EQ.'C88     ') DELVAL=115
              IF(WQ.EQ.'C89     ') DELVAL=116
              IF(WQ.EQ.'C90     ') DELVAL=117
              IF(WQ.EQ.'C91     ') DELVAL=118
              IF(WQ.EQ.'C92     ') DELVAL=119
              IF(WQ.EQ.'C93     ') DELVAL=120
              IF(WQ.EQ.'C94     ') DELVAL=121
              IF(WQ.EQ.'C95     ') DELVAL=122
              IF(WQ.EQ.'C96     ') DELVAL=123
              IF(WQ.EQ.'N6      ') DELVAL=124
              IF(WQ.EQ.'N7      ') DELVAL=125
              IF(WQ.EQ.'N8      ') DELVAL=126
              IF(WQ.EQ.'N9      ') DELVAL=127
              IF(WQ.EQ.'N10     ') DELVAL=128
              IF(WQ.EQ.'AH      ') DELVAL=129
              IF(WQ.EQ.'AI      ') DELVAL=130
              IF(WQ.EQ.'AJ      ') DELVAL=131
              IF(WQ.EQ.'AK      ') DELVAL=132
              IF(WQ.EQ.'AL      ') DELVAL=133
              IF(WQ.EQ.'ZD      ') DELVAL=134
              IF(WQ.EQ.'INDEX   ') DELVAL=135
              IF(WQ.EQ.'VNUM    ') DELVAL=136
              IF(WQ.EQ.'PIVX    ') DELVAL=137
              IF(WQ.EQ.'PIVY    ') DELVAL=138
              IF(WQ.EQ.'PIVZ    ') DELVAL=139
              IF(WQ.EQ.'DPART   ') DELVAL=140
              IF(WQ.EQ.'CLPX    ') DELVAL=141
              IF(WQ.EQ.'CLPY    ') DELVAL=142
              IF(WQ.EQ.'GDX     ') DELVAL=143
              IF(WQ.EQ.'GDY     ') DELVAL=144
              IF(WQ.EQ.'GDZ     ') DELVAL=145
              IF(WQ.EQ.'GALPHA  ') DELVAL=146
              IF(WQ.EQ.'GBETA   ') DELVAL=147
              IF(WQ.EQ.'GGAMMA  ') DELVAL=148
              IF(WQ.EQ.'GRS     ') DELVAL=149
              IF(WQ.EQ.'MACVAR  ') DELVAL=150
              IF(WQ(1:3).EQ.'ACT') THEN
C     CONVERT WQ(4:7) INTO A NUMBER AND ASSIGN AN APPROPRIATE
C     DELVAL TO IT
                  A4=WQ(4:7)
                  CALL ATOI4(A4,I4)
                  DELVAL=249+I4
C     DELVAL TO IT
C
              END IF
C
              IF(DELVAL.EQ.-1) THEN
                  WRITE(OUTLYNE,*)
     1            'INVALID QUALIFIER WORD USED WITH "DEL"'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(WQ.NE.'MACVAR'.AND.
     1        WQ.NE.'NSSXPOS'.AND.
     1        WQ.NE.'NSSYPOS'.AND.
     1        WQ.NE.'NSSZPOS'.AND.
     1        WQ.NE.'NSSALPH'.AND.
     1        WQ.NE.'NSSBETA'.AND.
     1        WQ.NE.'NSSGAMM'.AND.
     1        WQ.NE.'V1'.AND.
     1        WQ.NE.'V2'.AND.
     1        WQ.NE.'V3'.AND.
     1        WQ.NE.'V4'.AND.
     1        WQ.NE.'V5'.AND.
     1        WQ(1:3).NE.'PAR') THEN
                  IF(W1.LT.0.0D0) W1=SYSTEM1(20)+W1
                  IF(INT(W1).LT.0.OR.INT(W1).GT.INT(SYSTEM1(20))) THEN
                      WRITE(OUTLYNE,*)
     1                'NUMERIC WORD #1 (SURFACE NUMBER) BEYOND LEGAL BOUNDS'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  ELSE
C     NW1 OK, PROCEED
                  END IF
              ELSE
                  IF(WQ.EQ.'MACVAR') THEN
                      IF(INT(W1).LT.1.OR.INT(W1).GT.MAXREG) THEN
                          WRITE(OUTLYNE,*)
     1                    'NUMERIC WORD #1 (GP REGISTER NUMBER) BEYOND LEGAL BOUNDS'
                          CALL SHOWIT(1)
                          WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                          CALL SHOWIT(1)
                          CALL MACFAL
                          RETURN
                      ELSE
C     NW1 OK, PROCEED
                      END IF
                  END IF
              END IF
              VBSURF=INT(W1)
C     HERE IS WHERE VARIABLE IS DELETED
C     DOES THE DESIGNATED VARIABLE EXIST ?
              J=0
              DO I=1,VBCNT
                  IF(INT(VARABL(I,1)).EQ.DELVAL.AND.INT(VARABL(I,2))
     1            .EQ.INT(WORD2).AND.INT(VARABL(I,3)).EQ.INT(W1)) THEN
                      J=J+1
                      DELTAG=I
C     FOUND A MATCH, TAG IT FOR DELETION AND GO TO 95 AND DELETE IT
                      WRITE(OUTLYNE,195)WQ,VBSURF
                      CALL SHOWIT(1)
195                   FORMAT('DELETING VARIABLE ',A8,' FROM SURFACE NUMBER ',I3)
                      GO TO 95
                  END IF
              END DO
 95           IF(J.GT.0) THEN
C     DELETE TAGGED ITEMS
C     DELETE ITEM I=DELTAG
                  IF(VBCNT.GT.1) THEN
                      DO I=DELTAG,VBCNT-1
                          VARNAM(I)=VARNAM(I+1)
                          VARABL(I,1:17)=VARABL(I+1,1:17)
                      END DO
                      VBCNT=VBCNT-1
                  ELSE
C     VBCNT WAS 1
C     MAKE IT ZERO
                      VBCNT=0
                  END IF
C     ALL DELETIONS COMPLETED
                  CALL VARCLN
                  RETURN
              ELSE
                  WRITE(OUTLYNE,*)
     1            TRIM(WQ),' VARIABLE FOR SURFACE ',INT(W1)
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)
     1            'WAS NOT IN THE VARIABLE SUBFILE, NO DELETION PERFORMED'
                  CALL SHOWIT(1)
                  CALL MACFAL
C     ITEM DESIGNATED WAS NOT FOUND
              END IF
              RETURN
C
          END IF
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
          IF(WC.EQ.'CV      ') VALT=2
          IF(WC.EQ.'TH      ') VALT=3
          IF(WC.EQ.'CC      ') VALT=4
          IF(WC.EQ.'AD      ') VALT=5
          IF(WC.EQ.'AE      ') VALT=6
          IF(WC.EQ.'AF      ') VALT=7
          IF(WC.EQ.'AG      ') VALT=8
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
          IF(WC.EQ.'MACVAR  ') VALT=150
          IF(WC(1:3).EQ.'ACT') THEN
              IF(VBCFG.EQ.1) THEN
C     ASSIGN VALT # FOR THE 3969 ACTUATOR VARIABLES
C     CONVERT WC(4:7) INTO A NUMBER AND ASSIGN AN APPROPRIATE
C     VALT VALUE
                  A4=WC(4:7)
                  CALL ATOI4(A4,I4)
                  VALT=249+I4
C
              ELSE
C     NOT AT CFG 1, NO ASSIGNMENT MADE
                  WRITE(OUTLYNE,*)
     1            'DEFORMABLE SURFACE ACTUATOR VARIABLES ARE ONLY AVAILABLE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)
     1            'AT THE CFG 1, ALTERNATE CONFIGURATION LEVEL'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'NO ACTION TAKEN'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
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
C     NW1 IS SURFACE NUMBER
          IF(DF1.EQ.1) THEN
              WRITE(OUTLYNE,*)'VARIABLE NAME = ',WC
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'REQUIRES EXPLICIT NUMERIC WORD #1 INPUT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(WC.NE.'MACVAR') THEN
              IF(W1.LT.0.0D0) W1=SYSTEM1(20)+W1
              IF(INT(W1).LT.0.OR.INT(W1).GT.INT(SYSTEM1(20))) THEN
                  WRITE(OUTLYNE,*)
     1            'NUMERIC WORD #1 (SURFACE NUMBER) BEYOND LEGAL BOUNDS'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              ELSE
C     NW1 OK, PROCEED
                  VBSURF=INT(W1)
              END IF
          ELSE
              IF(INT(W1).LT.1.OR.INT(W1).GT.MAXREG) THEN
                  WRITE(OUTLYNE,*)
     1            'NUMERIC WORD #1 (GP REGISTER NUMBER) BEYOND LEGAL BOUNDS'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              ELSE
C     NW1 OK, PROCEED
                  VBSURF=INT(W1)
              END IF
          END IF
C     NW2 IS WEIGHT
          IF(DF2.EQ.1) THEN
C     DEFAULT INPUT
              WEIT=1.0D0
          ELSE
              IF(W2.LT.0.0D0) THEN
                  WRITE(OUTLYNE,*)
     1            'NUMERIC WORD #2 (WEIGHT) MUST NOT BE NEGATIVE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              WEIT=W2
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
                      IF(ALENS(10,NEWREF).LE.ALENS(11,NEWREF)) THEN
                          SYS12=ALENS(10,NEWREF)
                          SYS13=ALENS(10,NEWREF)
                      ELSE
                          SYS12=ALENS(11,NEWREF)
                          SYS13=ALENS(11,NEWREF)
                      END IF
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
              IF(DABS(ALENS(9,NEWREF)).EQ.0.0D0.OR.
     1        ALENS(127,NEWREF).NE.0.0D0) THEN
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
              IF(WC.EQ.'CV      ') DINCR=DINC2
              IF(WC.EQ.'TH      ') DINCR=DINC3
              IF(WC.EQ.'CLPX    ') DINCR=DINC3
              IF(WC.EQ.'CLPY    ') DINCR=DINC3
              IF(WC.EQ.'CC      ') DINCR=DINC4
              IF(WC.EQ.'AD'.OR.WC.EQ.'AE'.OR.WC.EQ.'AF'.OR.
     2        WC.EQ.'AG'.OR.WC.EQ.'AH'.OR.WC.EQ.'AI'.OR.
     3        WC.EQ.'AJ'.OR.WC.EQ.'AK'.OR.WC.EQ.'AL'.OR.WC.EQ.'AC') THEN
                  GOFORIT=.FALSE.
                  IF(ALENS(23,INT(W1)).EQ.0.0D0.OR.ALENS(23,INT(W1)).EQ.1.0D0) THEN
C     NO TORIC OR Y-TORIC
                      IF(PXTRAY(1,INT(W1)).NE.0.0D0.OR.PXTRAY(5,INT(W1)).NE.0.0D0) THEN
                          HM1=PXTRAY(1,INT(W1))
                          HC1=PXTRAY(5,INT(W1))
                          GOFORIT=.TRUE.
                      END IF
                  ELSE
C     X-TORIC
                      IF(PXTRAX(1,INT(W1)).NE.0.0D0.OR.PXTRAX(5,INT(W1)).NE.0.0D0) THEN
                          HM1=PXTRAX(1,INT(W1))
                          HC1=PXTRAX(5,INT(W1))
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
              IF(WC.EQ.'CVTOR   ') DINCR=DINC16
              IF(WC.EQ.'CCTOR   ') DINCR=DINC17
              IF(WC.EQ.'ADTOR'.OR.WC.EQ.'AETOR'.OR.WC.EQ.'AFTOR'.OR.
     2        WC.EQ.'AGTOR') THEN
                  GOFORIT=.FALSE.
                  IF(ALENS(23,INT(W1)).EQ.0.0D0.OR.ALENS(23,INT(W1)).EQ.1.0D0) THEN
C     NO TORIC OR Y-TORIC
                      IF(PXTRAX(1,INT(W1)).NE.0.0D0.OR.PXTRAX(5,INT(W1)).NE.0.0D0) THEN
                          HM1=PXTRAX(1,INT(W1))
                          HC1=PXTRAX(5,INT(W1))
                          GOFORIT=.TRUE.
                      END IF
                  ELSE
C     X-TORIC
                      IF(PXTRAY(1,INT(W1)).NE.0.0D0.OR.PXTRAY(5,INT(W1)).NE.0.0D0) THEN
                          HM1=PXTRAY(1,INT(W1))
                          HC1=PXTRAY(5,INT(W1))
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
              IF(WC.EQ.'INDEX   ') DINCR=DINC27
              IF(WC.EQ.'VNUM    ') DINCR=DINC27
              IF(WC.EQ.'DPART   ') DINCR=DINC27
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
              IF(WC.EQ.'GRS     ') DINCR=DINC28
              IF(WC.EQ.'MACVAR  ') DINCR=DINC28
              IF(WC(1:3).EQ.'ACT') DINCR=DINC30
          ELSE
              DFDINC=0
              DINCR=W3
          END IF
C     NW4 IS THE LOW VALUE
          IF(DF4.EQ.1) THEN
C     DEFAULT INPUT
              V4=-1.0D20
          ELSE
              V4=W4
          END IF
C     NW5 IS THE HIGH VALUE
          IF(DF5.EQ.1) THEN
C
C     DEFAULT INPUT
              V5=+1.0D20
          ELSE
              V5=W5
          END IF
          IF(V4.LE.V5) THEN
              VLOW=V4
              VHIGH=V5
          ELSE
              VLOW=V5
              VHIGH=V4
          END IF
          IF(VHIGH.LE.VLOW) THEN
              WRITE(OUTLYNE,*)
     1        'NUMERIC WORD #4 (LOW LIMIT) MUST BE LESS THAN W5 (HIGH LIMIT)'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(VALT.GE.250.AND.VALT.LE.4218) THEN
              IF(DF4.EQ.1) VLOW=-1.0D0
              IF(DF5.EQ.1) VHIGH=1.0D0
          END IF
          IF(VALT.EQ.136) THEN
              IF(DF4.EQ.1) VLOW=18.0D0
              IF(DF5.EQ.1) VHIGH=90.0D0
          END IF
          IF(VALT.EQ.140) THEN
              IF(DF4.EQ.1) VLOW=-2000.0D0
              IF(DF5.EQ.1) VHIGH=2000.0D0
          END IF
C     HIGH AND LOW FOR TILTS
          IF(VALT.EQ.149) THEN
              IF(DF4.EQ.1) VLOW=0.0D0
          END IF
C     NOW BUILD THE VARIABLE ENTRY
C       J=1  > 1 THROUGH 4118, A VARIABLE TYPE DESIGNATOR
C       J=2  > 1 THROUGH MAXCFG, THE CONFIGURATION DESIGNATOR
C       J=3  > 0 THROUGH MAXSUR, THE SURFACE NUMBER DESIGNATOR
C       J=4  > VARIABLE CURRENT VALUE
C       J=5  > VARIABLE PREVIOUS VALUE
C       J=6  > LAST VARIABLE CHANGE VALUE
C       J=7  > WT, THE WEIGHTING FACTOR
C       J=8  > DINCR (DERIVATIVE CHANGE VALUE)
C       J=9  > LOW LIMIT VALUE
C       J=10 > HIGH LIMIT VALUE
C       J=11 > SEQUENTIAL IDENTIFIER IN THE AUXCFG FILES
C       J=12 > DEFAULT DINCR FLAG (1=DEFFAULT, 0=USER SET)(DF3)
C       J=13 > ORIGINAL VALUE
C       J=14 > ENTRY NUMBER IN THE CFADD ARRAY THAT REFERS TO
C       J=15 > DF2
C       J=16 > DF4
C       J=17 > DF5
C
C
C     CHECK FOR A POSSIBLE OVERFLOW
          IF(VBCNT.EQ.MAXVB) THEN
C     NO MORE VARIABLES
              WRITE(OUTLYNE,*)
     1        'THE MAXIMUM OF ',MAXVB,' VARIABLES HAS BEEN REACHED'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'NO MORE MAY BE ENTERED'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
          VARNAM(VBCNT+1)=WC
          VARABL(VBCNT+1,1) =DBLE(VALT)
          VARABL(VBCNT+1,2) =DBLE(VBCFG)
          VARABL(VBCNT+1,3) =DBLE(VBSURF)
          VARABL(VBCNT+1,4) =0.0D0
          VARABL(VBCNT+1,5) =0.0D0
          VARABL(VBCNT+1,6) =0.0D0
          VARABL(VBCNT+1,7) =WEIT
          VARABL(VBCNT+1,8) =DINCR
          VARABL(VBCNT+1,9) =VLOW
          VARABL(VBCNT+1,10)=VHIGH
          VARABL(VBCNT+1,11)=0.0D0
C     DFDINC=DF3
          VARABL(VBCNT+1,12)=DBLE(DFDINC)
          VARABL(VBCNT+1,13)=0.0D0
          VARABL(VBCNT+1,14)=0.0D0
          VARABL(VBCNT+1,15)=DBLE(DF2)
          VARABL(VBCNT+1,16)=DBLE(DF4)
          VARABL(VBCNT+1,17)=DBLE(DF5)
C     NOW, IF CFG = 1
C
          IF(VBCFG.EQ.1) THEN
C     THIS LINE IS ALWAYS TRUE WHEN THE CONFIG IS #1
              VARABL(VBCNT+1,14)=0.0D0
C     WE USE THE LENS DATA ARRAY
C       J=4  > VARIABLE CURRENT VALUE
C       J=5  > VARIABLE PREVIOUS VALUE
C     VARABL(VBCNT+1,4)
C     VARABL(VBCNT+1,5)
C     WE FIRST CHECK TO SEE IF THE VARIABLE IS BEING
C     CONTROLLED BY A SOLVE. IF IT IS, THE ADDITION
C     OF THIS VARIABLE TO THE VARIABLE SUBFIL WILL BE
C     DISALLOWED AND THE VBCNT COUNTER WILL NOT BE ADVANCED.
C     IF IT IS NOT CONTROLLED BY A SOLVE, THEN PIKUPS WILL BE
C     CHECKED FOLLOWED BY TILT AUTO ASSIGNMENTS CHECKS AS NECESSARY.
C
 666          FORMAT(
     1        'VARIABLE NAME = ',A8,'AT SURFACE # = ',I3,
     1        '  AND IN CFG # = ',I2)
C
C     DO CV AND RD
              IF(VALT.EQ.1.OR.VALT.EQ.2) THEN
C     IF SURFACE IS NON-TORIC WITH A CURVATURE SOLVE OR THE
C     SURFACE IS X-TORIC WITH AN X CURVATURE SOLVE OR THE
C     SURFACE IS Y-TORIC WITH AN Y CURVATURE SOLVE
C     THEN DISALLOW VARIABLE
                  IF(ALENS(23,VBSURF).EQ.0.0D0.AND.ALENS(33,VBSURF).GT.1.0D0
     2            .OR.ALENS(23,VBSURF).EQ.1.0D0.AND.
     3            SOLVE(8,VBSURF).GT.0.0D0.OR.
     2            ALENS(23,VBSURF).EQ.2.0D0.AND.
     3            SOLVE(2,VBSURF).GT.0.0D0) THEN
                      WRITE(OUTLYNE,*)
     1                'A CONFLICTING CURVATURE SOLVE EXISTS ON THIS SURFACE'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,666) WC,INT(W1),VBCFG
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'VARIABLE ASSIGNMENT WAS NOT MADE'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
C     PIKUP CHECK
C     IF A CV,RD,PRO OR NPRO PIKUPS EXISTS THEN
                  IF(PIKUP(1,VBSURF,1).EQ.1.0D0
     1            .OR.PIKUP(1,VBSURF,2).EQ.1.0D0
     1            .OR.PIKUP(1,VBSURF,11).EQ.1.0D0
     1            .OR.PIKUP(1,VBSURF,12).EQ.1.0D0) THEN
C     A CV OR RD PIKUP EXISTS, DISSALLOW THE VARIABLE ASSIGNMENT
                      WRITE(OUTLYNE,*)
     1                'A CONFLICTING SURFACE SHAPE PIKUP EXISTS ON THIS SURFACE'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,666) WC,INT(W1),VBCFG
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'VARIABLE ASSIGNMENT WAS NOT MADE'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  ELSE
C     NO CONFLICTING PIKUP OR SOLVES EXIST, ASSIGN THE VARIABLE
C
                      IF(VALT.EQ.2.OR.VALT.EQ.1) VARABL(VBCNT+1,4)=ALENS(1,VBSURF)
                      IF(VALT.EQ.2.OR.VALT.EQ.1) VARABL(VBCNT+1,5)=ALENS(1,VBSURF)
                      IF(VALT.EQ.2.OR.VALT.EQ.1) VARABL(VBCNT+1,13)=ALENS(1,VBSURF)
                      VARABL(VBCNT+1,11)=1.0D0
C
                      VBCNT=VBCNT+1
                  END IF
                  CALL VARCLN
                  RETURN
              END IF
C
C     DO TH VARIABLE
              IF(VALT.EQ.3) THEN
                  IF(VBSURF.EQ.NEWIMG) THEN
 999                  FORMAT(
     1                'VARIABLE NAME = ',A8,'NOT USABLE AT THE IMAGE SURFACE')
                      WRITE(OUTLYNE,999) WC
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  IF(ALENS(33,VBSURF).EQ.1.0D0.OR.ALENS(33,VBSURF).EQ.3.0D0) THEN
C     TH SOLVE EXISTS, DISALLOW VARIABLE
                      WRITE(OUTLYNE,*)
     1                'A CONFLICTING THICKNESS SOLVE EXISTS ON THIS SURFACE'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,666) WC,INT(W1),VBCFG
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'VARIABLE ASSIGNMENT WAS NOT MADE'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
C     PIKUP CHECK
                  IF(PIKUP(1,VBSURF,3).EQ.1.0D0.OR.PIKUP(1,VBSURF,32).EQ.1.0D0)
     1             THEN
C     A THICKNESS PIKUP EXISTS, DISSALLOW THE VARIABLE ASSIGNMENT
                      WRITE(OUTLYNE,*)
     1                'A CONFLICTING THICKNESS PIKUP EXISTS ON THIS SURFACE'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,666) WC,INT(W1),VBCFG
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'VARIABLE ASSIGNMENT WAS NOT MADE'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  ELSE
C     NO TH PIKUP OR SOLVES EXIST, ASSIGN THE VARIABLE
C
                      VARABL(VBCNT+1,4)=ALENS(3,VBSURF)
                      VARABL(VBCNT+1,5)=ALENS(3,VBSURF)
                      VARABL(VBCNT+1,13)=ALENS(3,VBSURF)
                      VARABL(VBCNT+1,11)=3.0D0
                      VBCNT=VBCNT+1
                  END IF
                  CALL VARCLN
                  RETURN
              END IF
C     DO CC
              IF(VALT.EQ.4) THEN
C     PIKUP CHECK
C     IF A CC,PRO OR NPRO PIKUPS EXISTS THEN
                  IF(PIKUP(1,VBSURF,4).EQ.1.0D0
     1            .OR.PIKUP(1,VBSURF,11).EQ.1.0D0
     1            .OR.PIKUP(1,VBSURF,12).EQ.1.0D0) THEN
C     A CV OR RD PIKUP EXISTS, DISSALLOW THE VARIABLE ASSIGNMENT
                      WRITE(OUTLYNE,*)
     1                'A CONFLICTING SURFACE SHAPE PIKUP EXISTS ON THIS SURFACE'
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
                      IF(VALT.EQ.4) VARABL(VBCNT+1,4)=ALENS(2,VBSURF)
                      IF(VALT.EQ.4) VARABL(VBCNT+1,5)=ALENS(2,VBSURF)
                      IF(VALT.EQ.4) VARABL(VBCNT+1,13)=ALENS(2,VBSURF)
                      VARABL(VBCNT+1,11)=2.0D0
                      VBCNT=VBCNT+1
                  END IF
                  CALL VARCLN
                  RETURN
              END IF
              IF(VALT.EQ.5) THEN
                  IF(ALENS(8,VBSURF).NE.1.0D0) ALENS(8,VBSURF)=1.0D0
C     PIKUP CHECK
C     IF A AD,PRO OR NPRO PIKUPS EXISTS THEN
                  IF(PIKUP(1,VBSURF,5).EQ.1.0D0
     1            .OR.PIKUP(1,VBSURF,11).EQ.1.0D0
     1            .OR.PIKUP(1,VBSURF,12).EQ.1.0D0) THEN
C     A CONFLICTING SHAPE PIKUP EXISTS, DISSALLOW THE VARIABLE ASSIGNMENT
                      WRITE(OUTLYNE,*)
     1                'A CONFLICTING SURFACE SHAPE PIKUP EXISTS ON THIS SURFACE'
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
                      IF(VALT.EQ.5) VARABL(VBCNT+1,4)=ALENS(4,VBSURF)
                      IF(VALT.EQ.5) VARABL(VBCNT+1,5)=ALENS(4,VBSURF)
                      IF(VALT.EQ.5) VARABL(VBCNT+1,13)=ALENS(4,VBSURF)
                      VARABL(VBCNT+1,11)=4.0D0
                      VBCNT=VBCNT+1
                  END IF
                  CALL VARCLN
                  RETURN
              END IF
C
              IF(VALT.EQ.6) THEN
                  IF(ALENS(8,VBSURF).NE.1.0D0) ALENS(8,VBSURF)=1.0D0
C     PIKUP CHECK
C     IF A AE,PRO OR NPRO PIKUPS EXISTS THEN
                  IF(PIKUP(1,VBSURF,6).EQ.1.0D0
     1            .OR.PIKUP(1,VBSURF,11).EQ.1.0D0
     1            .OR.PIKUP(1,VBSURF,12).EQ.1.0D0) THEN
C     A CONFLICTING SHAPE PIKUP EXISTS, DISSALLOW THE VARIABLE ASSIGNMENT
                      WRITE(OUTLYNE,*)
     1                'A CONFLICTING SURFACE SHAPE PIKUP EXISTS ON THIS SURFACE'
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
                      IF(VALT.EQ.6) VARABL(VBCNT+1,4)=ALENS(5,VBSURF)
                      IF(VALT.EQ.6) VARABL(VBCNT+1,5)=ALENS(5,VBSURF)
                      IF(VALT.EQ.6) VARABL(VBCNT+1,13)=ALENS(5,VBSURF)
                      VARABL(VBCNT+1,11)=5.0D0
                      VBCNT=VBCNT+1
                  END IF
                  CALL VARCLN
                  RETURN
              END IF
C
              IF(VALT.EQ.7) THEN
                  IF(ALENS(8,VBSURF).NE.1.0D0) ALENS(8,VBSURF)=1.0D0
C     PIKUP CHECK
C     IF A AF,PRO OR NPRO PIKUPS EXISTS THEN
                  IF(PIKUP(1,VBSURF,7).EQ.1.0D0
     1            .OR.PIKUP(1,VBSURF,11).EQ.1.0D0
     1            .OR.PIKUP(1,VBSURF,12).EQ.1.0D0) THEN
C     A CONFLICTING SHAPE PIKUP EXISTS, DISSALLOW THE VARIABLE ASSIGNMENT
                      WRITE(OUTLYNE,*)
     1                'A CONFLICTING SURFACE SHAPE PIKUP EXISTS ON THIS SURFACE'
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
                      IF(VALT.EQ.7) VARABL(VBCNT+1,4)=ALENS(6,VBSURF)
                      IF(VALT.EQ.7) VARABL(VBCNT+1,5)=ALENS(6,VBSURF)
                      IF(VALT.EQ.7) VARABL(VBCNT+1,13)=ALENS(6,VBSURF)
                      VARABL(VBCNT+1,11)=6.0D0
                      VBCNT=VBCNT+1
                  END IF
                  CALL VARCLN
                  RETURN
              END IF
C
              IF(VALT.EQ.8) THEN
                  IF(ALENS(8,VBSURF).NE.1.0D0) ALENS(8,VBSURF)=1.0D0
C     PIKUP CHECK
C     IF A AG,PRO OR NPRO PIKUPS EXISTS THEN
                  IF(PIKUP(1,VBSURF,8).EQ.1.0D0
     1            .OR.PIKUP(1,VBSURF,11).EQ.1.0D0
     1            .OR.PIKUP(1,VBSURF,12).EQ.1.0D0) THEN
C     A CONFLICTING SHAPE PIKUP EXISTS, DISSALLOW THE VARIABLE ASSIGNMENT
                      WRITE(OUTLYNE,*)
     1                'A CONFLICTING SURFACE SHAPE PIKUP EXISTS ON THIS SURFACE'
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
                      IF(VALT.EQ.8) VARABL(VBCNT+1,4)=ALENS(7,VBSURF)
                      IF(VALT.EQ.8) VARABL(VBCNT+1,5)=ALENS(7,VBSURF)
                      IF(VALT.EQ.8) VARABL(VBCNT+1,13)=ALENS(7,VBSURF)
                      VARABL(VBCNT+1,11)=7.0D0
                      VBCNT=VBCNT+1
                  END IF
                  CALL VARCLN
                  RETURN
              END IF
C
C     DO CVTOR
              IF(VALT.EQ.10.OR.VALT.EQ.9) THEN
                  IF(ALENS(23,VBSURF).EQ.0.0D0) THEN
                      WRITE(OUTLYNE,*)
     1                'THE SPECIFIED SURFACE IS NOT TOROIDAL'
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
     3            SOLVE(2,VBSURF).GT.0.0D0.OR.
     2            ALENS(23,VBSURF).EQ.2.0D0.AND.
     3            SOLVE(8,VBSURF).GT.0.0D0) THEN
                      WRITE(OUTLYNE,*)
     1                'A CONFLICTING CURVATURE SOLVE EXISTS ON THIS SURFACE'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,666) WC,INT(W1),VBCFG
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'VARIABLE ASSIGNMENT WAS NOT MADE'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
C     PIKUP CHECK
C     IF A CV,RD,PRO OR NPRO PIKUPS EXISTS THEN
                  IF(PIKUP(1,VBSURF,9).EQ.1.0D0
     1            .OR.PIKUP(1,VBSURF,10).EQ.1.0D0
     1            .OR.PIKUP(1,VBSURF,11).EQ.1.0D0
     1            .OR.PIKUP(1,VBSURF,12).EQ.1.0D0) THEN
C     A CV OR RD PIKUP EXISTS, DISSALLOW THE VARIABLE ASSIGNMENT
                      WRITE(OUTLYNE,*)
     1                'A CONFLICTING SURFACE SHAPE PIKUP EXISTS ON THIS SURFACE'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,666) WC,INT(W1),VBCFG
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'VARIABLE ASSIGNMENT WAS NOT MADE'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  ELSE
C     NO CONFLICTING PIKUP OR SOLVES EXIST, ASSIGN THE VARIABLE
C
                      IF(ALENS(24,VBSURF).EQ.0.0D0) THEN
C
                          IF(VALT.EQ.10.OR.VALT.EQ.9) VARABL(VBCNT+1,4)=0.0D0
                          IF(VALT.EQ.10.OR.VALT.EQ.9) VARABL(VBCNT+1,5)=0.0D0
                          IF(VALT.EQ.10.OR.VALT.EQ.9) VARABL(VBCNT+1,13)=0.0D0
                          IF(VALT.EQ.10.OR.VALT.EQ.9) VARABL(VBCNT+1,11)=24.0D0
                      ELSE
                          IF(VALT.EQ.10.OR.VALT.EQ.9) VARABL(VBCNT+1,4)=ALENS(24,VBSURF)
                          IF(VALT.EQ.10.OR.VALT.EQ.9) VARABL(VBCNT+1,5)=ALENS(24,VBSURF)
                          IF(VALT.EQ.10.OR.VALT.EQ.9) VARABL(VBCNT+1,13)=ALENS(24,VBSURF)
                          IF(VALT.EQ.10.OR.VALT.EQ.9) VARABL(VBCNT+1,11)=24.0D0
                      END IF
                      VBCNT=VBCNT+1
                  END IF
                  CALL VARCLN
                  RETURN
              END IF
C
C     DO CCTOR
              IF(VALT.EQ.11) THEN
                  IF(ALENS(23,VBSURF).EQ.0.0D0) THEN
                      WRITE(OUTLYNE,*)
     1                'THE SPECIFIED SURFACE IS NOT TOROIDAL'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'VARIABLE ASSIGNMENT WAS NOT MADE'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
C     PIKUP CHECK
C     IF A CCTOR,PRO OR NPRO PIKUPS EXISTS THEN
                  IF(PIKUP(1,VBSURF,21).EQ.1.0D0
     1            .OR.PIKUP(1,VBSURF,11).EQ.1.0D0
     1            .OR.PIKUP(1,VBSURF,12).EQ.1.0D0) THEN
C     A CV OR RD PIKUP EXISTS, DISSALLOW THE VARIABLE ASSIGNMENT
                      WRITE(OUTLYNE,*)
     1                'A CONFLICTING SURFACE SHAPE PIKUP EXISTS ON THIS SURFACE'
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
                      IF(VALT.EQ.11) VARABL(VBCNT+1,4)=ALENS(41,VBSURF)
                      IF(VALT.EQ.11) VARABL(VBCNT+1,5)=ALENS(41,VBSURF)
                      IF(VALT.EQ.11) VARABL(VBCNT+1,13)=ALENS(41,VBSURF)
                      IF(VALT.EQ.11) VARABL(VBCNT+1,11)=41.0D0
                      VBCNT=VBCNT+1
                  END IF
                  CALL VARCLN
                  RETURN
              END IF
C
C     DO ADTOR
              IF(VALT.EQ.12) THEN
                  IF(ALENS(23,VBSURF).EQ.0.0D0) THEN
                      WRITE(OUTLYNE,*)
     1                'THE SPECIFIED SURFACE IS NOT TOROIDAL'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'VARIABLE ASSIGNMENT WAS NOT MADE'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
C     PIKUP CHECK
C     IF A ADT,PRO OR NPRO PIKUPS EXISTS THEN
                  IF(PIKUP(1,VBSURF,22).EQ.1.0D0
     1            .OR.PIKUP(1,VBSURF,11).EQ.1.0D0
     1            .OR.PIKUP(1,VBSURF,12).EQ.1.0D0) THEN
C     A CV OR RD PIKUP EXISTS, DISSALLOW THE VARIABLE ASSIGNMENT
                      WRITE(OUTLYNE,*)
     1                'A CONFLICTING SURFACE SHAPE PIKUP EXISTS ON THIS SURFACE'
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
                      IF(VALT.EQ.12) VARABL(VBCNT+1,4)=ALENS(37,VBSURF)
                      IF(VALT.EQ.12) VARABL(VBCNT+1,5)=ALENS(37,VBSURF)
                      IF(VALT.EQ.12) VARABL(VBCNT+1,13)=ALENS(37,VBSURF)
                      IF(VALT.EQ.12) VARABL(VBCNT+1,11)=37.0D0
                      VBCNT=VBCNT+1
                  END IF
                  CALL VARCLN
                  RETURN
              END IF
C
C     DO AETOR
              IF(VALT.EQ.13) THEN
                  IF(ALENS(23,VBSURF).EQ.0.0D0) THEN
                      WRITE(OUTLYNE,*)
     1                'THE SPECIFIED SURFACE IS NOT TOROIDAL'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'VARIABLE ASSIGNMENT WAS NOT MADE'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
C     PIKUP CHECK
C     IF A AETOR,PRO OR NPRO PIKUPS EXISTS THEN
                  IF(PIKUP(1,VBSURF,23).EQ.1.0D0
     1            .OR.PIKUP(1,VBSURF,11).EQ.1.0D0
     1            .OR.PIKUP(1,VBSURF,12).EQ.1.0D0) THEN
C     A CV OR RD PIKUP EXISTS, DISSALLOW THE VARIABLE ASSIGNMENT
                      WRITE(OUTLYNE,*)
     1                'A CONFLICTING SURFACE SHAPE PIKUP EXISTS ON THIS SURFACE'
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
                      IF(VALT.EQ.13) VARABL(VBCNT+1,4)=ALENS(38,VBSURF)
                      IF(VALT.EQ.13) VARABL(VBCNT+1,5)=ALENS(38,VBSURF)
                      IF(VALT.EQ.13) VARABL(VBCNT+1,13)=ALENS(38,VBSURF)
                      IF(VALT.EQ.13) VARABL(VBCNT+1,11)=38.0D0
                      VBCNT=VBCNT+1
                  END IF
                  CALL VARCLN
                  RETURN
              END IF
C
C     DO AFTOR
              IF(VALT.EQ.14) THEN
                  IF(ALENS(23,VBSURF).EQ.0.0D0) THEN
                      WRITE(OUTLYNE,*)
     1                'THE SPECIFIED SURFACE IS NOT TOROIDAL'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'VARIABLE ASSIGNMENT WAS NOT MADE'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
C     PIKUP CHECK
C     IF A AFTOR,PRO OR NPRO PIKUPS EXISTS THEN
                  IF(PIKUP(1,VBSURF,24).EQ.1.0D0
     1            .OR.PIKUP(1,VBSURF,11).EQ.1.0D0
     1            .OR.PIKUP(1,VBSURF,12).EQ.1.0D0) THEN
C     A CV OR RD PIKUP EXISTS, DISSALLOW THE VARIABLE ASSIGNMENT
                      WRITE(OUTLYNE,*)
     1                'A CONFLICTING SURFACE SHAPE PIKUP EXISTS ON THIS SURFACE'
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
                      IF(VALT.EQ.14) VARABL(VBCNT+1,4)=ALENS(39,VBSURF)
                      IF(VALT.EQ.14) VARABL(VBCNT+1,5)=ALENS(39,VBSURF)
                      IF(VALT.EQ.14) VARABL(VBCNT+1,13)=ALENS(39,VBSURF)
                      IF(VALT.EQ.14) VARABL(VBCNT+1,11)=39.0D0
                      VBCNT=VBCNT+1
                  END IF
                  CALL VARCLN
                  RETURN
              END IF
C
C     DO AGTOR
              IF(VALT.EQ.15) THEN
                  IF(ALENS(23,VBSURF).EQ.0.0D0) THEN
                      WRITE(OUTLYNE,*)
     1                'THE SPECIFIED SURFACE IS NOT TOROIDAL'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'VARIABLE ASSIGNMENT WAS NOT MADE'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
C     PIKUP CHECK
C     IF A AGTOR,PRO OR NPRO PIKUPS EXISTS THEN
                  IF(PIKUP(1,VBSURF,25).EQ.1.0D0
     1            .OR.PIKUP(1,VBSURF,11).EQ.1.0D0
     1            .OR.PIKUP(1,VBSURF,12).EQ.1.0D0) THEN
C     A CV OR RD PIKUP EXISTS, DISSALLOW THE VARIABLE ASSIGNMENT
                      WRITE(OUTLYNE,*)
     1                'A CONFLICTING SURFACE SHAPE PIKUP EXISTS ON THIS SURFACE'
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
                      IF(VALT.EQ.15) VARABL(VBCNT+1,4)=ALENS(40,VBSURF)
                      IF(VALT.EQ.15) VARABL(VBCNT+1,5)=ALENS(40,VBSURF)
                      IF(VALT.EQ.15) VARABL(VBCNT+1,13)=ALENS(40,VBSURF)
                      IF(VALT.EQ.15) VARABL(VBCNT+1,11)=40.0D0
                      VBCNT=VBCNT+1
                  END IF
                  CALL VARCLN
                  RETURN
              END IF
C
C     DO ALPHA
              IF(VALT.EQ.16) THEN
                  IF(ALENS(25,VBSURF).EQ.0.0D0) THEN
                      WRITE(OUTLYNE,*)
     1                'THE SPECIFIED SURFACE IS NOT TILTED'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'VARIABLE ASSIGNMENT WAS NOT MADE'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  IF(ALENS(25,VBSURF).EQ.2.0D0.OR.ALENS(25,VBSURF).EQ.3.0D0) THEN
                      WRITE(OUTLYNE,*)
     1                'THE SPECIFIED SURFACE HAS AN AUTOMATIC TILT DEFINITION'
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
     1                'A CONFLICTING ALPHA PIKUP EXISTS ON THIS SURFACE'
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
                      IF(VALT.EQ.16) VARABL(VBCNT+1,4)=ALENS(118,VBSURF)
                      IF(VALT.EQ.16) VARABL(VBCNT+1,5)=ALENS(118,VBSURF)
                      IF(VALT.EQ.16) VARABL(VBCNT+1,13)=ALENS(118,VBSURF)
                      IF(VALT.EQ.16) VARABL(VBCNT+1,11)=118.0D0
                      VBCNT=VBCNT+1
                  END IF
                  CALL VARCLN
                  RETURN
              END IF
C
C     DO BETA
              IF(VALT.EQ.17) THEN
                  IF(ALENS(25,VBSURF).EQ.0.0D0) THEN
                      WRITE(OUTLYNE,*)
     1                'THE SPECIFIED SURFACE IS NOT TILTED'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'VARIABLE ASSIGNMENT WAS NOT MADE'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  IF(ALENS(25,VBSURF).EQ.2.0D0.OR.ALENS(25,VBSURF).EQ.3.0D0) THEN
                      WRITE(OUTLYNE,*)
     1                'THE SPECIFIED SURFACE HAS AN AUTOMATIC TILT DEFINITION'
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
     1                'A CONFLICTING BETA PIKUP EXISTS ON THIS SURFACE'
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
                      IF(VALT.EQ.17) VARABL(VBCNT+1,4)=ALENS(119,VBSURF)
                      IF(VALT.EQ.17) VARABL(VBCNT+1,5)=ALENS(119,VBSURF)
                      IF(VALT.EQ.17) VARABL(VBCNT+1,13)=ALENS(119,VBSURF)
                      IF(VALT.EQ.17) VARABL(VBCNT+1,11)=119.0D0
                      VBCNT=VBCNT+1
                  END IF
                  CALL VARCLN
                  RETURN
              END IF
C
C     DO GAMMA
              IF(VALT.EQ.18) THEN
                  IF(ALENS(25,VBSURF).EQ.0.0D0) THEN
                      WRITE(OUTLYNE,*)
     1                'THE SPECIFIED SURFACE IS NOT TILTED'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'VARIABLE ASSIGNMENT WAS NOT MADE'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  IF(ALENS(25,VBSURF).EQ.2.0D0.OR.ALENS(25,VBSURF).EQ.3.0D0) THEN
                      WRITE(OUTLYNE,*)
     1                'THE SPECIFIED SURFACE HAS AN AUTOMATIC TILT DEFINITION'
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
     1                'A CONFLICTING GAMMA PIKUP EXISTS ON THIS SURFACE'
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
                      IF(VALT.EQ.18) VARABL(VBCNT+1,4)=ALENS(120,VBSURF)
                      IF(VALT.EQ.18) VARABL(VBCNT+1,5)=ALENS(120,VBSURF)
                      IF(VALT.EQ.18) VARABL(VBCNT+1,13)=ALENS(120,VBSURF)
                      IF(VALT.EQ.18) VARABL(VBCNT+1,11)=120.0D0
                      VBCNT=VBCNT+1
                  END IF
                  CALL VARCLN
                  RETURN
              END IF
C
C     DO XD
              IF(VALT.EQ.19) THEN
C     PIKUP CHECK
C     IF AN XD PIKUPS EXISTS THEN
                  IF(ALENS(25,VBSURF).EQ.2.0D0.OR.ALENS(25,VBSURF).EQ.3.0D0) THEN
                      WRITE(OUTLYNE,*)
     1                'THE SPECIFIED SURFACE HAS AN AUTOMATIC TILT DEFINITION'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'VARIABLE ASSIGNMENT WAS NOT MADE'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  IF(PIKUP(1,VBSURF,14).EQ.1.0D0) THEN
C     A CONFLICTING PIKUP EXISTS, DISSALLOW THE VARIABLE ASSIGNMENT
                      WRITE(OUTLYNE,*)
     1                'A CONFLICTING XD PIKUP EXISTS ON THIS SURFACE'
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
                      IF(VALT.EQ.19) VARABL(VBCNT+1,4)=ALENS(114,VBSURF)
                      IF(VALT.EQ.19) VARABL(VBCNT+1,5)=ALENS(114,VBSURF)
                      IF(VALT.EQ.19) VARABL(VBCNT+1,13)=ALENS(114,VBSURF)
                      IF(VALT.EQ.19) VARABL(VBCNT+1,11)=114.0D0
                      VBCNT=VBCNT+1
                  END IF
                  CALL VARCLN
                  RETURN
              END IF
C
C     DO YD
              IF(VALT.EQ.20) THEN
                  IF(ALENS(25,VBSURF).EQ.2.0D0.OR.ALENS(25,VBSURF).EQ.3.0D0) THEN
                      WRITE(OUTLYNE,*)
     1                'THE SPECIFIED SURFACE HAS AN AUTOMATIC TILT DEFINITION'
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
     1                'A CONFLICTING YD PIKUP EXISTS ON THIS SURFACE'
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
                      IF(VALT.EQ.20) VARABL(VBCNT+1,4)=ALENS(115,VBSURF)
                      IF(VALT.EQ.20) VARABL(VBCNT+1,5)=ALENS(115,VBSURF)
                      IF(VALT.EQ.20) VARABL(VBCNT+1,13)=ALENS(115,VBSURF)
                      IF(VALT.EQ.20) VARABL(VBCNT+1,11)=115.0D0
                      VBCNT=VBCNT+1
                  END IF
                  CALL VARCLN
                  RETURN
              END IF
C
C     DO ZD
              IF(VALT.EQ.134) THEN
                  IF(ALENS(25,VBSURF).EQ.2.0D0.OR.ALENS(25,VBSURF).EQ.3.0D0) THEN
                      WRITE(OUTLYNE,*)
     1                'THE SPECIFIED SURFACE HAS AN AUTOMATIC TILT DEFINITION'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'VARIABLE ASSIGNMENT WAS NOT MADE'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
C     PIKUP CHECK
C     IF AN ZD PIKUPS EXISTS THEN
                  IF(PIKUP(1,VBSURF,33).EQ.1.0D0) THEN
C     A CONFLICTING PIKUP EXISTS, DISSALLOW THE VARIABLE ASSIGNMENT
                      WRITE(OUTLYNE,*)
     1                'A CONFLICTING ZD PIKUP EXISTS ON THIS SURFACE'
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
                      IF(VALT.EQ.134) VARABL(VBCNT+1,4)=ALENS(116,VBSURF)
                      IF(VALT.EQ.134) VARABL(VBCNT+1,5)=ALENS(116,VBSURF)
                      IF(VALT.EQ.134) VARABL(VBCNT+1,13)=ALENS(116,VBSURF)
                      IF(VALT.EQ.134) VARABL(VBCNT+1,11)=116.0D0
                      VBCNT=VBCNT+1
                  END IF
                  CALL VARCLN
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
     1                'A CONFLICTING GDX PIKUP EXISTS ON THIS SURFACE'
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
                      IF(VALT.EQ.143) VARABL(VBCNT+1,4)=ALENS(90,VBSURF)
                      IF(VALT.EQ.143) VARABL(VBCNT+1,5)=ALENS(90,VBSURF)
                      IF(VALT.EQ.143) VARABL(VBCNT+1,13)=ALENS(90,VBSURF)
                      IF(VALT.EQ.143) VARABL(VBCNT+1,11)=90.0D0
                      VBCNT=VBCNT+1
                  END IF
                  CALL VARCLN
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
     1                'A CONFLICTING GDY PIKUP EXISTS ON THIS SURFACE'
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
                      IF(VALT.EQ.144) VARABL(VBCNT+1,4)=ALENS(91,VBSURF)
                      IF(VALT.EQ.144) VARABL(VBCNT+1,5)=ALENS(91,VBSURF)
                      IF(VALT.EQ.144) VARABL(VBCNT+1,13)=ALENS(91,VBSURF)
                      IF(VALT.EQ.144) VARABL(VBCNT+1,11)=91.0D0
                      VBCNT=VBCNT+1
                  END IF
                  CALL VARCLN
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
     1                'A CONFLICTING GDZ PIKUP EXISTS ON THIS SURFACE'
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
                      IF(VALT.EQ.145) VARABL(VBCNT+1,4)=ALENS(92,VBSURF)
                      IF(VALT.EQ.145) VARABL(VBCNT+1,5)=ALENS(92,VBSURF)
                      IF(VALT.EQ.145) VARABL(VBCNT+1,13)=ALENS(92,VBSURF)
                      IF(VALT.EQ.145) VARABL(VBCNT+1,11)=92.0D0
                      VBCNT=VBCNT+1
                  END IF
                  CALL VARCLN
                  RETURN
              END IF
C
C     DO GALPHA
              IF(VALT.EQ.146) THEN
C     PIKUP CHECK
C     IF AN GDX PIKUPS EXISTS THEN
                  IF(PIKUP(1,VBSURF,40).EQ.1.0D0) THEN
C     A CONFLICTING PIKUP EXISTS, DISSALLOW THE VARIABLE ASSIGNMENT
                      WRITE(OUTLYNE,*)
     1                'A CONFLICTING GALPHA PIKUP EXISTS ON THIS SURFACE'
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
                      IF(VALT.EQ.146) VARABL(VBCNT+1,4)=ALENS(93,VBSURF)
                      IF(VALT.EQ.146) VARABL(VBCNT+1,5)=ALENS(93,VBSURF)
                      IF(VALT.EQ.146) VARABL(VBCNT+1,13)=ALENS(93,VBSURF)
                      IF(VALT.EQ.146) VARABL(VBCNT+1,11)=93.0D0
                      VBCNT=VBCNT+1
                  END IF
                  CALL VARCLN
                  RETURN
              END IF
C
C     DO GBETA
              IF(VALT.EQ.147) THEN
C     PIKUP CHECK
C     IF AN GDY PIKUPS EXISTS THEN
                  IF(PIKUP(1,VBSURF,41).EQ.1.0D0) THEN
C     A CONFLICTING PIKUP EXISTS, DISSALLOW THE VARIABLE ASSIGNMENT
                      WRITE(OUTLYNE,*)
     1                'A CONFLICTING GBETA PIKUP EXISTS ON THIS SURFACE'
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
                      IF(VALT.EQ.147) VARABL(VBCNT+1,4)=ALENS(94,VBSURF)
                      IF(VALT.EQ.147) VARABL(VBCNT+1,5)=ALENS(94,VBSURF)
                      IF(VALT.EQ.147) VARABL(VBCNT+1,13)=ALENS(94,VBSURF)
                      IF(VALT.EQ.147) VARABL(VBCNT+1,11)=94.0D0
                      VBCNT=VBCNT+1
                  END IF
                  CALL VARCLN
                  RETURN
              END IF
C
C     DO GGAMMA
              IF(VALT.EQ.148) THEN
C     PIKUP CHECK
C     IF AN GDZ PIKUPS EXISTS THEN
                  IF(PIKUP(1,VBSURF,42).EQ.1.0D0) THEN
C     A CONFLICTING PIKUP EXISTS, DISSALLOW THE VARIABLE ASSIGNMENT
                      WRITE(OUTLYNE,*)
     1                'A CONFLICTING GGAMMA PIKUP EXISTS ON THIS SURFACE'
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
                      IF(VALT.EQ.148) VARABL(VBCNT+1,4)=ALENS(95,VBSURF)
                      IF(VALT.EQ.148) VARABL(VBCNT+1,5)=ALENS(95,VBSURF)
                      IF(VALT.EQ.148) VARABL(VBCNT+1,13)=ALENS(95,VBSURF)
                      IF(VALT.EQ.148) VARABL(VBCNT+1,11)=95.0D0
                      VBCNT=VBCNT+1
                  END IF
                  CALL VARCLN
                  RETURN
              END IF
C     DO GRT
              IF(VALT.EQ.149) THEN
                  IF(ALENS(96,VBSURF).EQ.1.0D0) THEN
                  ELSE
                      WRITE(OUTLYNE,*)
     1                'THE SPECIFIED SURFACE REQUIRES A "GRT" DEFINITION'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'VARIABLE ASSIGNMENT WAS NOT MADE'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
C     PIKUP CHECK
C     IF AN GRT PIKUPS EXISTS THEN
                  IF(PIKUP(1,VBSURF,43).EQ.1.0D0) THEN
C     A CONFLICTING PIKUP EXISTS, DISSALLOW THE VARIABLE ASSIGNMENT
                      WRITE(OUTLYNE,*)
     1                'A CONFLICTING GRT PIKUP EXISTS ON THIS SURFACE'
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
                      IF(VALT.EQ.149) VARABL(VBCNT+1,4)=ALENS(98,VBSURF)
                      IF(VALT.EQ.149) VARABL(VBCNT+1,5)=ALENS(98,VBSURF)
                      IF(VALT.EQ.149) VARABL(VBCNT+1,13)=ALENS(98,VBSURF)
                      IF(VALT.EQ.149) VARABL(VBCNT+1,11)=98.0D0
                      VBCNT=VBCNT+1
                  END IF
                  CALL VARCLN
                  RETURN
              END IF
C     DO MACVAR
              IF(VALT.EQ.150) THEN
                  IF(VALT.EQ.150) VARABL(VBCNT+1,4)=GPREG(VBSURF)
                  IF(VALT.EQ.150) VARABL(VBCNT+1,5)=GPREG(VBSURF)
                  IF(VALT.EQ.150) VARABL(VBCNT+1,13)=GPREG(VBSURF)
                  IF(VALT.EQ.150) VARABL(VBCNT+1,11)=0.0D0
                  VBCNT=VBCNT+1
                  CALL VARCLN
                  RETURN
              END IF
C     DO PIVX
              IF(VALT.EQ.137) THEN
                  IF(ALENS(25,VBSURF).EQ.2.0D0.OR.ALENS(25,VBSURF).EQ.3.0D0) THEN
                      WRITE(OUTLYNE,*)
     1                'THE SPECIFIED SURFACE HAS AN AUTOMATIC TILT DEFINITION'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'VARIABLE ASSIGNMENT WAS NOT MADE'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
C     PIKUP CHECK
C     IF AN PIVX PIKUPS EXISTS THEN
                  IF(PIKUP(1,VBSURF,34).EQ.1.0D0) THEN
C     A CONFLICTING PIKUP EXISTS, DISSALLOW THE VARIABLE ASSIGNMENT
                      WRITE(OUTLYNE,*)
     1                'A CONFLICTING PIVX PIKUP EXISTS ON THIS SURFACE'
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
                      IF(VALT.EQ.137) VARABL(VBCNT+1,4)=ALENS(78,VBSURF)
                      IF(VALT.EQ.137) VARABL(VBCNT+1,5)=ALENS(78,VBSURF)
                      IF(VALT.EQ.137) VARABL(VBCNT+1,13)=ALENS(78,VBSURF)
                      IF(VALT.EQ.137) VARABL(VBCNT+1,11)=78.0D0
                      VBCNT=VBCNT+1
                  END IF
                  CALL VARCLN
                  RETURN
              END IF
C     DO PIVY
              IF(VALT.EQ.138) THEN
                  IF(ALENS(25,VBSURF).EQ.2.0D0.OR.ALENS(25,VBSURF).EQ.3.0D0) THEN
                      WRITE(OUTLYNE,*)
     1                'THE SPECIFIED SURFACE HAS AN AUTOMATIC TILT DEFINITION'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'VARIABLE ASSIGNMENT WAS NOT MADE'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
C     PIKUP CHECK
C     IF AN PIVY PIKUPS EXISTS THEN
                  IF(PIKUP(1,VBSURF,33).EQ.1.0D0) THEN
C     A CONFLICTING PIKUP EXISTS, DISSALLOW THE VARIABLE ASSIGNMENT
                      WRITE(OUTLYNE,*)
     1                'A CONFLICTING PIVY PIKUP EXISTS ON THIS SURFACE'
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
                      IF(VALT.EQ.138) VARABL(VBCNT+1,4)=ALENS(79,VBSURF)
                      IF(VALT.EQ.138) VARABL(VBCNT+1,5)=ALENS(79,VBSURF)
                      IF(VALT.EQ.138) VARABL(VBCNT+1,13)=ALENS(79,VBSURF)
                      IF(VALT.EQ.138) VARABL(VBCNT+1,11)=79.0D0
                      VBCNT=VBCNT+1
                  END IF
                  CALL VARCLN
                  RETURN
              END IF
C     DO PIVZ
              IF(VALT.EQ.139) THEN
                  IF(ALENS(25,VBSURF).EQ.2.0D0.OR.ALENS(25,VBSURF).EQ.3.0D0) THEN
                      WRITE(OUTLYNE,*)
     1                'THE SPECIFIED SURFACE HAS AN AUTOMATIC TILT DEFINITION'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'VARIABLE ASSIGNMENT WAS NOT MADE'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
C     PIKUP CHECK
C     IF AN ZD PIKUPS EXISTS THEN
                  IF(PIKUP(1,VBSURF,36).EQ.1.0D0) THEN
C     A CONFLICTING PIKUP EXISTS, DISSALLOW THE VARIABLE ASSIGNMENT
                      WRITE(OUTLYNE,*)
     1                'A CONFLICTING PIVZ PIKUP EXISTS ON THIS SURFACE'
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
                      IF(VALT.EQ.139) VARABL(VBCNT+1,4)=ALENS(80,VBSURF)
                      IF(VALT.EQ.139) VARABL(VBCNT+1,5)=ALENS(80,VBSURF)
                      IF(VALT.EQ.139) VARABL(VBCNT+1,13)=ALENS(80,VBSURF)
                      IF(VALT.EQ.139) VARABL(VBCNT+1,11)=80.0D0
                      VBCNT=VBCNT+1
                  END IF
                  CALL VARCLN
                  RETURN
              END IF
C     DO CLPX
              IF(VALT.EQ.141) THEN
                  IF(ALENS(127,VBSURF).NE.0.0D0) THEN
                      WRITE(OUTLYNE,*)
     1                'THE SPECIFIED SURFACE HAS MULTIPLE APERTURES ASSIGNED'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'VARIABLE ASSIGNMENT WAS NOT MADE'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  IF(ALENS(9,VBSURF).EQ.0.0D0) THEN
                      WRITE(OUTLYNE,*)
     1                'THE SPECIFIED SURFACE HAS NO CLEAR APERTURE ASSIGNED'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'VARIABLE ASSIGNMENT WAS NOT MADE'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  IF(ALENS(9,VBSURF).EQ.1.0D0) THEN
                      WRITE(OUTLYNE,*)
     1                'THE SPECIFIED SURFACE HAS A CIRCULAR CLEAR APERTURE ASSIGNED'
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
     1                'A CONFLICTING CLAP PIKUP EXISTS ON THIS SURFACE'
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
                      IF(VALT.EQ.141) VARABL(VBCNT+1,4)=ALENS(11,VBSURF)
                      IF(VALT.EQ.141) VARABL(VBCNT+1,5)=ALENS(11,VBSURF)
                      IF(VALT.EQ.141) VARABL(VBCNT+1,13)=ALENS(11,VBSURF)
                      IF(VALT.EQ.141) VARABL(VBCNT+1,11)=80.0D0
                      VBCNT=VBCNT+1
                  END IF
                  CALL VARCLN
                  RETURN
              END IF
C     DO CLPY
              IF(VALT.EQ.142) THEN
                  IF(ALENS(127,VBSURF).NE.0.0D0) THEN
                      WRITE(OUTLYNE,*)
     1                'THE SPECIFIED SURFACE HAS MULTIPLE APERTURES ASSIGNED'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'VARIABLE ASSIGNMENT WAS NOT MADE'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  IF(ALENS(9,VBSURF).EQ.0.0D0) THEN
                      WRITE(OUTLYNE,*)
     1                'THE SPECIFIED SURFACE HAS NO CLEAR APERTURE ASSIGNED'
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
     1                'A CONFLICTING CLAP PIKUP EXISTS ON THIS SURFACE'
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
                      IF(VALT.EQ.142) VARABL(VBCNT+1,4)=ALENS(10,VBSURF)
                      IF(VALT.EQ.142) VARABL(VBCNT+1,5)=ALENS(10,VBSURF)
                      IF(VALT.EQ.142) VARABL(VBCNT+1,13)=ALENS(10,VBSURF)
                      IF(VALT.EQ.142) VARABL(VBCNT+1,11)=80.0D0
                      VBCNT=VBCNT+1
                  END IF
                  CALL VARCLN
                  RETURN
              END IF
C
C     DO N1
              IF(VALT.EQ.21) THEN
                  IF(GLANAM(VBSURF,1)(1:5).NE.'GLASS') THEN
                      WRITE(OUTLYNE,*)
     1                'GLASS MUST BE CONVERTED FROM A CATALOG GLASS'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'TO A "GLASS" TYPE OF ENTRY BEFORE INDEX CAN BE A VARIABLE'
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
     1                'A CONFLICTING GLASS PIKUP EXISTS ON THIS SURFACE'
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
                      IF(VALT.EQ.21) VARABL(VBCNT+1,4)=ALENS(46,VBSURF)
                      IF(VALT.EQ.21) VARABL(VBCNT+1,5)=ALENS(46,VBSURF)
                      IF(VALT.EQ.21) VARABL(VBCNT+1,13)=ALENS(46,VBSURF)
                      IF(VALT.EQ.21) VARABL(VBCNT+1,11)=46.0D0
                      VBCNT=VBCNT+1
                  END IF
                  CALL VARCLN
                  RETURN
              END IF
C
C     DO N2
              IF(VALT.EQ.22) THEN
                  IF(GLANAM(VBSURF,1)(1:5).NE.'GLASS') THEN
                      WRITE(OUTLYNE,*)
     1                'GLASS MUST BE CONVERTED FROM A CATALOG GLASS'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'TO A "GLASS" TYPE OF ENTRY BEFORE INDEX CAN BE A VARIABLE'
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
     1                'A CONFLICTING GLASS PIKUP EXISTS ON THIS SURFACE'
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
                      IF(VALT.EQ.22) VARABL(VBCNT+1,4)=ALENS(47,VBSURF)
                      IF(VALT.EQ.22) VARABL(VBCNT+1,5)=ALENS(47,VBSURF)
                      IF(VALT.EQ.22) VARABL(VBCNT+1,13)=ALENS(47,VBSURF)
                      IF(VALT.EQ.22) VARABL(VBCNT+1,11)=47.0D0
                      VBCNT=VBCNT+1
                  END IF
                  CALL VARCLN
                  RETURN
              END IF
C
C     DO N3
              IF(VALT.EQ.23) THEN
                  IF(GLANAM(VBSURF,1)(1:5).NE.'GLASS') THEN
                      WRITE(OUTLYNE,*)
     1                'GLASS MUST BE CONVERTED FROM A CATALOG GLASS'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'TO A "GLASS" TYPE OF ENTRY BEFORE INDEX CAN BE A VARIABLE'
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
     1                'A CONFLICTING GLASS PIKUP EXISTS ON THIS SURFACE'
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
                      IF(VALT.EQ.23) VARABL(VBCNT+1,4)=ALENS(48,VBSURF)
                      IF(VALT.EQ.23) VARABL(VBCNT+1,5)=ALENS(48,VBSURF)
                      IF(VALT.EQ.23) VARABL(VBCNT+1,13)=ALENS(48,VBSURF)
                      IF(VALT.EQ.23) VARABL(VBCNT+1,11)=48.0D0
                      VBCNT=VBCNT+1
                  END IF
                  CALL VARCLN
                  RETURN
              END IF
C
C     DO N4
              IF(VALT.EQ.24) THEN
                  IF(GLANAM(VBSURF,1)(1:5).NE.'GLASS') THEN
                      WRITE(OUTLYNE,*)
     1                'GLASS MUST BE CONVERTED FROM A CATALOG GLASS'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'TO A "GLASS" TYPE OF ENTRY BEFORE INDEX CAN BE A VARIABLE'
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
     1                'A CONFLICTING GLASS PIKUP EXISTS ON THIS SURFACE'
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
                      IF(VALT.EQ.24) VARABL(VBCNT+1,4)=ALENS(49,VBSURF)
                      IF(VALT.EQ.24) VARABL(VBCNT+1,5)=ALENS(49,VBSURF)
                      IF(VALT.EQ.24) VARABL(VBCNT+1,13)=ALENS(49,VBSURF)
                      IF(VALT.EQ.24) VARABL(VBCNT+1,11)=49.0D0
                      VBCNT=VBCNT+1
                  END IF
                  CALL VARCLN
                  RETURN
              END IF
C
C     DO N5
              IF(VALT.EQ.25) THEN
                  IF(GLANAM(VBSURF,1)(1:5).NE.'GLASS') THEN
                      WRITE(OUTLYNE,*)
     1                'GLASS MUST BE CONVERTED FROM A CATALOG GLASS'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'TO A "GLASS" TYPE OF ENTRY BEFORE INDEX CAN BE A VARIABLE'
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
     1                'A CONFLICTING GLASS PIKUP EXISTS ON THIS SURFACE'
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
                      IF(VALT.EQ.25) VARABL(VBCNT+1,4)=ALENS(50,VBSURF)
                      IF(VALT.EQ.25) VARABL(VBCNT+1,5)=ALENS(50,VBSURF)
                      IF(VALT.EQ.25) VARABL(VBCNT+1,13)=ALENS(50,VBSURF)
                      IF(VALT.EQ.25) VARABL(VBCNT+1,11)=50.0D0
                      VBCNT=VBCNT+1
                  END IF
                  CALL VARCLN
                  RETURN
              END IF
C
C     DO N6
              IF(VALT.EQ.124) THEN
                  IF(GLANAM(VBSURF,1)(1:5).NE.'GLASS') THEN
                      WRITE(OUTLYNE,*)
     1                'GLASS MUST BE CONVERTED FROM A CATALOG GLASS'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'TO A "GLASS" TYPE OF ENTRY BEFORE INDEX CAN BE A VARIABLE'
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
     1                'A CONFLICTING GLASS PIKUP EXISTS ON THIS SURFACE'
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
                      IF(VALT.EQ.124) VARABL(VBCNT+1,4)=ALENS(71,VBSURF)
                      IF(VALT.EQ.124) VARABL(VBCNT+1,5)=ALENS(71,VBSURF)
                      IF(VALT.EQ.124) VARABL(VBCNT+1,13)=ALENS(71,VBSURF)
                      IF(VALT.EQ.124) VARABL(VBCNT+1,11)=71.0D0
                      VBCNT=VBCNT+1
                  END IF
                  CALL VARCLN
                  RETURN
              END IF
C     DO N7
              IF(VALT.EQ.125) THEN
                  IF(GLANAM(VBSURF,1)(1:5).NE.'GLASS') THEN
                      WRITE(OUTLYNE,*)
     1                'GLASS MUST BE CONVERTED FROM A CATALOG GLASS'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'TO A "GLASS" TYPE OF ENTRY BEFORE INDEX CAN BE A VARIABLE'
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
     1                'A CONFLICTING GLASS PIKUP EXISTS ON THIS SURFACE'
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
                      IF(VALT.EQ.125) VARABL(VBCNT+1,4)=ALENS(72,VBSURF)
                      IF(VALT.EQ.125) VARABL(VBCNT+1,5)=ALENS(72,VBSURF)
                      IF(VALT.EQ.125) VARABL(VBCNT+1,13)=ALENS(72,VBSURF)
                      IF(VALT.EQ.125) VARABL(VBCNT+1,11)=72.0D0
                      VBCNT=VBCNT+1
                  END IF
                  CALL VARCLN
                  RETURN
              END IF
C     DO N8
              IF(VALT.EQ.126) THEN
                  IF(GLANAM(VBSURF,1)(1:5).NE.'GLASS') THEN
                      WRITE(OUTLYNE,*)
     1                'GLASS MUST BE CONVERTED FROM A CATALOG GLASS'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'TO A "GLASS" TYPE OF ENTRY BEFORE INDEX CAN BE A VARIABLE'
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
     1                'A CONFLICTING GLASS PIKUP EXISTS ON THIS SURFACE'
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
                      IF(VALT.EQ.126) VARABL(VBCNT+1,4)=ALENS(73,VBSURF)
                      IF(VALT.EQ.126) VARABL(VBCNT+1,5)=ALENS(73,VBSURF)
                      IF(VALT.EQ.126) VARABL(VBCNT+1,13)=ALENS(73,VBSURF)
                      IF(VALT.EQ.126) VARABL(VBCNT+1,11)=73.0D0
                      VBCNT=VBCNT+1
                  END IF
                  CALL VARCLN
                  RETURN
              END IF
C     DO N9
              IF(VALT.EQ.127) THEN
                  IF(GLANAM(VBSURF,1)(1:5).NE.'GLASS') THEN
                      WRITE(OUTLYNE,*)
     1                'GLASS MUST BE CONVERTED FROM A CATALOG GLASS'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'TO A "GLASS" TYPE OF ENTRY BEFORE INDEX CAN BE A VARIABLE'
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
     1                'A CONFLICTING GLASS PIKUP EXISTS ON THIS SURFACE'
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
                      IF(VALT.EQ.127) VARABL(VBCNT+1,4)=ALENS(74,VBSURF)
                      IF(VALT.EQ.127) VARABL(VBCNT+1,5)=ALENS(74,VBSURF)
                      IF(VALT.EQ.127) VARABL(VBCNT+1,13)=ALENS(74,VBSURF)
                      IF(VALT.EQ.127) VARABL(VBCNT+1,11)=74.0D0
                      VBCNT=VBCNT+1
                  END IF
                  CALL VARCLN
                  RETURN
              END IF
C     DO N10
              IF(VALT.EQ.128) THEN
                  IF(GLANAM(VBSURF,1)(1:5).NE.'GLASS') THEN
                      WRITE(OUTLYNE,*)
     1                'GLASS MUST BE CONVERTED FROM A CATALOG GLASS'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'TO A "GLASS" TYPE OF ENTRY BEFORE INDEX CAN BE A VARIABLE'
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
     1                'A CONFLICTING GLASS PIKUP EXISTS ON THIS SURFACE'
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
                      IF(VALT.EQ.128) VARABL(VBCNT+1,4)=ALENS(75,VBSURF)
                      IF(VALT.EQ.128) VARABL(VBCNT+1,5)=ALENS(75,VBSURF)
                      IF(VALT.EQ.128) VARABL(VBCNT+1,13)=ALENS(75,VBSURF)
                      IF(VALT.EQ.128) VARABL(VBCNT+1,11)=75.0D0
                      VBCNT=VBCNT+1
                  END IF
                  CALL VARCLN
                  RETURN
              END IF
C
C     DO INDEX
              IF(VALT.EQ.135) THEN
                  IF(GLANAM(VBSURF,1)(1:5).NE.'MODEL') THEN
                      WRITE(OUTLYNE,*)
     1                'GLASS MUST BE CONVERTED FROM A CATALOG GLASS'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'TO A "MODEL" TYPE OF ENTRY BEFORE INDEX CAN BE A VARIABLE'
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
     1                'A CONFLICTING GLASS PIKUP EXISTS ON THIS SURFACE'
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
                      IF(VALT.EQ.135) VARABL(VBCNT+1,4)=ALENS(86,VBSURF)
                      IF(VALT.EQ.135) VARABL(VBCNT+1,5)=ALENS(86,VBSURF)
                      IF(VALT.EQ.135) VARABL(VBCNT+1,13)=ALENS(86,VBSURF)
                      IF(VALT.EQ.135) VARABL(VBCNT+1,11)=86.0D0
                      VBCNT=VBCNT+1
                  END IF
                  CALL VARCLN
                  RETURN
              END IF
C
C     DO VNUM
              IF(VALT.EQ.136) THEN
                  IF(GLANAM(VBSURF,1)(1:5).NE.'MODEL') THEN
                      WRITE(OUTLYNE,*)
     1                'GLASS MUST BE CONVERTED FROM A CATALOG GLASS'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'TO A "MODEL" TYPE OF ENTRY BEFORE INDEX CAN BE A VARIABLE'
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
     1                'A CONFLICTING GLASS PIKUP EXISTS ON THIS SURFACE'
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
                      IF(VALT.EQ.136) VARABL(VBCNT+1,4)=ALENS(87,VBSURF)
                      IF(VALT.EQ.136) VARABL(VBCNT+1,5)=ALENS(87,VBSURF)
                      IF(VALT.EQ.136) VARABL(VBCNT+1,13)=ALENS(87,VBSURF)
                      IF(VALT.EQ.136) VARABL(VBCNT+1,11)=87.0D0
                      VBCNT=VBCNT+1
                  END IF
                  CALL VARCLN
                  RETURN
              END IF
C
C
C     DO DPART
              IF(VALT.EQ.140) THEN
                  IF(GLANAM(VBSURF,1)(1:5).NE.'MODEL') THEN
                      WRITE(OUTLYNE,*)
     1                'GLASS MUST BE CONVERTED FROM A CATALOG GLASS'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'TO A "MODEL" TYPE OF ENTRY BEFORE INDEX CAN BE A VARIABLE'
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
     1                'A CONFLICTING GLASS PIKUP EXISTS ON THIS SURFACE'
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
                      IF(VALT.EQ.140) VARABL(VBCNT+1,4)=ALENS(89,VBSURF)
                      IF(VALT.EQ.140) VARABL(VBCNT+1,5)=ALENS(89,VBSURF)
                      IF(VALT.EQ.140) VARABL(VBCNT+1,13)=ALENS(89,VBSURF)
                      IF(VALT.EQ.140) VARABL(VBCNT+1,11)=87.0D0
                      VBCNT=VBCNT+1
                  END IF
                  CALL VARCLN
                  RETURN
              END IF
C
              IF(VALT.EQ.129) THEN
                  IF(ALENS(8,VBSURF).NE.1.0D0) ALENS(8,VBSURF)=1.0D0
C     PIKUP CHECK
C     IF A AH,PRO OR NPRO PIKUPS EXISTS THEN
                  IF(PIKUP(1,VBSURF,27).EQ.1.0D0
     1            .OR.PIKUP(1,VBSURF,11).EQ.1.0D0
     1            .OR.PIKUP(1,VBSURF,12).EQ.1.0D0) THEN
C     A CONFLICTING SHAPE PIKUP EXISTS, DISSALLOW THE VARIABLE ASSIGNMENT
                      WRITE(OUTLYNE,*)
     1                'A CONFLICTING SURFACE SHAPE PIKUP EXISTS ON THIS SURFACE'
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
                      IF(VALT.EQ.129) VARABL(VBCNT+1,4)=ALENS(81,VBSURF)
                      IF(VALT.EQ.129) VARABL(VBCNT+1,5)=ALENS(81,VBSURF)
                      IF(VALT.EQ.129) VARABL(VBCNT+1,13)=ALENS(81,VBSURF)
                      VARABL(VBCNT+1,11)=81.0D0
                      VBCNT=VBCNT+1
                  END IF
                  CALL VARCLN
                  RETURN
              END IF
C
              IF(VALT.EQ.130) THEN
                  IF(ALENS(8,VBSURF).NE.1.0D0) ALENS(8,VBSURF)=1.0D0
C     PIKUP CHECK
C     IF A AI,PRO OR NPRO PIKUPS EXISTS THEN
                  IF(PIKUP(1,VBSURF,28).EQ.1.0D0
     1            .OR.PIKUP(1,VBSURF,11).EQ.1.0D0
     1            .OR.PIKUP(1,VBSURF,12).EQ.1.0D0) THEN
C     A CONFLICTING SHAPE PIKUP EXISTS, DISSALLOW THE VARIABLE ASSIGNMENT
                      WRITE(OUTLYNE,*)
     1                'A CONFLICTING SURFACE SHAPE PIKUP EXISTS ON THIS SURFACE'
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
                      IF(VALT.EQ.130) VARABL(VBCNT+1,4)=ALENS(82,VBSURF)
                      IF(VALT.EQ.130) VARABL(VBCNT+1,5)=ALENS(82,VBSURF)
                      IF(VALT.EQ.130) VARABL(VBCNT+1,13)=ALENS(82,VBSURF)
                      VARABL(VBCNT+1,11)=82.0D0
                      VBCNT=VBCNT+1
                  END IF
                  CALL VARCLN
                  RETURN
              END IF
C
              IF(VALT.EQ.131) THEN
                  IF(ALENS(8,VBSURF).NE.1.0D0) ALENS(8,VBSURF)=1.0D0
C     PIKUP CHECK
C     IF A AJ,PRO OR NPRO PIKUPS EXISTS THEN
                  IF(PIKUP(1,VBSURF,29).EQ.1.0D0
     1            .OR.PIKUP(1,VBSURF,11).EQ.1.0D0
     1            .OR.PIKUP(1,VBSURF,12).EQ.1.0D0) THEN
C     A CONFLICTING SHAPE PIKUP EXISTS, DISSALLOW THE VARIABLE ASSIGNMENT
                      WRITE(OUTLYNE,*)
     1                'A CONFLICTING SURFACE SHAPE PIKUP EXISTS ON THIS SURFACE'
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
                      IF(VALT.EQ.131) VARABL(VBCNT+1,4)=ALENS(83,VBSURF)
                      IF(VALT.EQ.131) VARABL(VBCNT+1,5)=ALENS(83,VBSURF)
                      IF(VALT.EQ.131) VARABL(VBCNT+1,13)=ALENS(83,VBSURF)
                      VARABL(VBCNT+1,11)=83.0D0
                      VBCNT=VBCNT+1
                  END IF
                  CALL VARCLN
                  RETURN
              END IF
C
              IF(VALT.EQ.132) THEN
                  IF(ALENS(8,VBSURF).NE.1.0D0) ALENS(8,VBSURF)=1.0D0
C     PIKUP CHECK
C     IF A AK,PRO OR NPRO PIKUPS EXISTS THEN
                  IF(PIKUP(1,VBSURF,30).EQ.1.0D0
     1            .OR.PIKUP(1,VBSURF,11).EQ.1.0D0
     1            .OR.PIKUP(1,VBSURF,12).EQ.1.0D0) THEN
C     A CONFLICTING SHAPE PIKUP EXISTS, DISSALLOW THE VARIABLE ASSIGNMENT
                      WRITE(OUTLYNE,*)
     1                'A CONFLICTING SURFACE SHAPE PIKUP EXISTS ON THIS SURFACE'
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
                      IF(VALT.EQ.132) VARABL(VBCNT+1,4)=ALENS(84,VBSURF)
                      IF(VALT.EQ.132) VARABL(VBCNT+1,5)=ALENS(84,VBSURF)
                      IF(VALT.EQ.132) VARABL(VBCNT+1,13)=ALENS(84,VBSURF)
                      VARABL(VBCNT+1,11)=84.0D0
                      VBCNT=VBCNT+1
                  END IF
                  CALL VARCLN
                  RETURN
              END IF
C
              IF(VALT.EQ.133) THEN
                  IF(ALENS(8,VBSURF).NE.1.0D0) ALENS(8,VBSURF)=1.0D0
C     PIKUP CHECK
C     IF A AL,PRO OR NPRO PIKUPS EXISTS THEN
                  IF(PIKUP(1,VBSURF,31).EQ.1.0D0
     1            .OR.PIKUP(1,VBSURF,11).EQ.1.0D0
     1            .OR.PIKUP(1,VBSURF,12).EQ.1.0D0) THEN
C     A CONFLICTING SHAPE PIKUP EXISTS, DISSALLOW THE VARIABLE ASSIGNMENT
                      WRITE(OUTLYNE,*)
     1                'A CONFLICTING SURFACE SHAPE PIKUP EXISTS ON THIS SURFACE'
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
                      IF(VALT.EQ.133) VARABL(VBCNT+1,4)=ALENS(85,VBSURF)
                      IF(VALT.EQ.133) VARABL(VBCNT+1,5)=ALENS(85,VBSURF)
                      IF(VALT.EQ.133) VARABL(VBCNT+1,13)=ALENS(85,VBSURF)
                      VARABL(VBCNT+1,11)=85.0D0
                      VBCNT=VBCNT+1
                  END IF
                  CALL VARCLN
                  RETURN
              END IF
C
C
C     NOW DO THE COEFFS FOR CFG1
C
              IF(VALT.GE.27.AND.VALT.LE.74.OR.
     1        VALT.GE.76.AND.VALT.LE.123) THEN
                  CNOT=.FALSE.
                  IF(DABS(ALENS(34,VBSURF)).EQ.1.0D0.OR.
     1            DABS(ALENS(34,VBSURF)).EQ.6.0D0) THEN
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
     1                    'SPECIAL SURFACE TYPES 1 AND 6'
                          CALL SHOWIT(1)
                          WRITE(OUTLYNE,*)
     1                    'ONLY USE COEFFICIENTS C9 THROUGH C48'
                          CALL SHOWIT(1)
                          WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                          CALL SHOWIT(1)
                          CALL MACFAL
                          RETURN
                      END IF
                  END IF
                  IF(DABS(ALENS(34,VBSURF)).EQ.2.0D0.OR.
     1            DABS(ALENS(34,VBSURF)).EQ.9.0D0) THEN
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
     1                    'SPECIAL SURFACE TYPES 2 AND 9'
                          CALL SHOWIT(1)
                          WRITE(OUTLYNE,*)
     1                    'ONLY USE COEFFICIENTS C1 THROUGH C30'
                          CALL SHOWIT(1)
                          WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                          CALL SHOWIT(1)
                          CALL MACFAL
                          RETURN
                      END IF
                  END IF
                  IF(DABS(ALENS(34,VBSURF)).EQ.3.0D0.OR.
     1            DABS(ALENS(34,VBSURF)).EQ.10.0D0) THEN
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
     1                    'SPECIAL SURFACE TYPES 3 AND 10'
                          CALL SHOWIT(1)
                          WRITE(OUTLYNE,*)
     1                    'ONLY USE COEFFICIENTS C1 THROUGH C37'
                          CALL SHOWIT(1)
                          WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                          CALL SHOWIT(1)
                          CALL MACFAL
                          RETURN
                      END IF
                  END IF
                  IF(DABS(ALENS(34,VBSURF)).EQ.4.0D0) THEN
                      IF(WC.NE.'C1'.AND.WC.NE.'C2'.AND.WC.NE.'C3'
     1                .AND.WC.NE.'C4'.AND.WC.NE.'C5'.AND.WC.NE.'C6'
     1                .AND.WC.NE.'C7'.AND.WC.NE.'C8'.AND.WC.NE.'C9'
     1                .AND.WC.NE.'C10'.AND.WC.NE.'C11'.AND.WC.NE.'C12'
     1                .AND.WC.NE.'C13'.AND.WC.NE.'C14'.AND.WC.NE.'C15') THEN
                          WRITE(OUTLYNE,*)
     1                    'SPECIAL SURFACE TYPE 4 ONLY USES COEFFICIENTS C1 THROUGH C15'
                          CALL SHOWIT(1)
                          WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                          CALL SHOWIT(1)
                          CALL MACFAL
                          RETURN
                      END IF
                  END IF
                  IF(DABS(ALENS(34,VBSURF)).EQ.18.0D0) THEN
                      IF(WC.NE.'C1'.AND.WC.NE.'C2'.AND.WC.NE.'C3'.AND.WC.NE.'C4'
     1                .AND.WC.NE.'C5'.AND.WC.NE.'C6'.AND.WC.NE.'C7'.AND.
     1                WC.NE.'C8'.AND.WC.NE.'C9'.AND.WC.NE.'C10'.AND.
     1                WC.NE.'C11'.AND.WC.NE.'C12'.AND.WC.NE.'C13'.AND.
     1                WC.NE.'C14'.AND.WC.NE.'C15'.AND.WC.NE.'C16'.AND.
     1                WC.NE.'C17'.AND.WC.NE.'C18') THEN
                          WRITE(OUTLYNE,*)
     1                    'SPECIAL SURFACE TYPE 18 ONLY USES COEFFICIENTS C1 THROUGH C18'
                          CALL SHOWIT(1)
                          WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                          CALL SHOWIT(1)
                          CALL MACFAL
                          RETURN
                      END IF
                  END IF
                  IF(DABS(ALENS(34,VBSURF)).EQ.7.0D0.OR.
     1            DABS(ALENS(34,VBSURF)).EQ.8.0D0) THEN
                      IF(WC.EQ.'C92') CNOT=.TRUE.
                      IF(WC.EQ.'C93') CNOT=.TRUE.
                      IF(WC.EQ.'C94') CNOT=.TRUE.
                      IF(WC.EQ.'C95') CNOT=.TRUE.
                      IF(WC.EQ.'C96') CNOT=.TRUE.
                      IF(CNOT) THEN
                          WRITE(OUTLYNE,*)
     1                    'SPECIAL SURFACE TYPES 7 AND 8'
                          CALL SHOWIT(1)
                          WRITE(OUTLYNE,*)
     1                    'ONLY USE COEFFICIENTS C1 THROUGH C91'
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
     1                    'SPECIAL SURFACE TYPE 12'
                          CALL SHOWIT(1)
                          WRITE(OUTLYNE,*)
     1                    'ONLY USES COEFFICIENTS C1 THROUGH C23'
                          CALL SHOWIT(1)
                          WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                          CALL SHOWIT(1)
                          CALL MACFAL
                          RETURN
                      END IF
                  END IF
              END IF

C     DO C1 TO 96
              IF(VALT.GE.27.AND.VALT.LE.74
     1        .OR.VALT.GE.76.AND.VALT.LE.123) THEN
                  IF(ALENS(34,VBSURF).EQ.0.0D0) THEN
                      WRITE(OUTLYNE,*)
     1                'THE SURFACE IS NOT DEFINED AS A SPECIAL SURFACE'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'VARIABLE ASSIGNMENT WAS NOT MADE'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
C     PIKUP CHECK
C     IF AN PIKUP GLASS EXISTS THEN
                  IF(PIKUP(1,VBSURF,11).EQ.1.0D0
     1            .OR.PIKUP(1,VBSURF,12).EQ.1.0D0) THEN
C     A CONFLICTING PIKUP EXISTS, DISSALLOW THE VARIABLE ASSIGNMENT
                      WRITE(OUTLYNE,*)
     1                'A CONFLICTING SURFACE SHAPE PIKUP EXISTS ON THIS SURFACE'
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
                      IF(VALT.EQ.27) VARABL(VBCNT+1,4)=FTFL01(1,VBSURF)
                      IF(VALT.EQ.28) VARABL(VBCNT+1,4)=FTFL01(2,VBSURF)
                      IF(VALT.EQ.29) VARABL(VBCNT+1,4)=FTFL01(3,VBSURF)
                      IF(VALT.EQ.30) VARABL(VBCNT+1,4)=FTFL01(4,VBSURF)
                      IF(VALT.EQ.31) VARABL(VBCNT+1,4)=FTFL01(5,VBSURF)
                      IF(VALT.EQ.32) VARABL(VBCNT+1,4)=FTFL01(6,VBSURF)
                      IF(VALT.EQ.33) VARABL(VBCNT+1,4)=FTFL01(7,VBSURF)
                      IF(VALT.EQ.34) VARABL(VBCNT+1,4)=FTFL01(8,VBSURF)
                      IF(VALT.EQ.35) VARABL(VBCNT+1,4)=FTFL01(9,VBSURF)
                      IF(VALT.EQ.36) VARABL(VBCNT+1,4)=FTFL01(10,VBSURF)
                      IF(VALT.EQ.37) VARABL(VBCNT+1,4)=FTFL01(11,VBSURF)
                      IF(VALT.EQ.38) VARABL(VBCNT+1,4)=FTFL01(12,VBSURF)
                      IF(VALT.EQ.39) VARABL(VBCNT+1,4)=FTFL01(13,VBSURF)
                      IF(VALT.EQ.40) VARABL(VBCNT+1,4)=FTFL01(14,VBSURF)
                      IF(VALT.EQ.41) VARABL(VBCNT+1,4)=FTFL01(15,VBSURF)
                      IF(VALT.EQ.42) VARABL(VBCNT+1,4)=FTFL01(16,VBSURF)
                      IF(VALT.EQ.43) VARABL(VBCNT+1,4)=FTFL01(17,VBSURF)
                      IF(VALT.EQ.44) VARABL(VBCNT+1,4)=FTFL01(18,VBSURF)
                      IF(VALT.EQ.45) VARABL(VBCNT+1,4)=FTFL01(19,VBSURF)
                      IF(VALT.EQ.46) VARABL(VBCNT+1,4)=FTFL01(20,VBSURF)
                      IF(VALT.EQ.47) VARABL(VBCNT+1,4)=FTFL01(21,VBSURF)
                      IF(VALT.EQ.48) VARABL(VBCNT+1,4)=FTFL01(22,VBSURF)
                      IF(VALT.EQ.49) VARABL(VBCNT+1,4)=FTFL01(23,VBSURF)
                      IF(VALT.EQ.50) VARABL(VBCNT+1,4)=FTFL01(24,VBSURF)
                      IF(VALT.EQ.51) VARABL(VBCNT+1,4)=FTFL01(25,VBSURF)
                      IF(VALT.EQ.52) VARABL(VBCNT+1,4)=FTFL01(26,VBSURF)
                      IF(VALT.EQ.53) VARABL(VBCNT+1,4)=FTFL01(27,VBSURF)
                      IF(VALT.EQ.54) VARABL(VBCNT+1,4)=FTFL01(28,VBSURF)
                      IF(VALT.EQ.55) VARABL(VBCNT+1,4)=FTFL01(29,VBSURF)
                      IF(VALT.EQ.56) VARABL(VBCNT+1,4)=FTFL01(30,VBSURF)
                      IF(VALT.EQ.57) VARABL(VBCNT+1,4)=FTFL01(31,VBSURF)
                      IF(VALT.EQ.58) VARABL(VBCNT+1,4)=FTFL01(32,VBSURF)
                      IF(VALT.EQ.59) VARABL(VBCNT+1,4)=FTFL01(33,VBSURF)
                      IF(VALT.EQ.60) VARABL(VBCNT+1,4)=FTFL01(34,VBSURF)
                      IF(VALT.EQ.61) VARABL(VBCNT+1,4)=FTFL01(35,VBSURF)
                      IF(VALT.EQ.62) VARABL(VBCNT+1,4)=FTFL01(36,VBSURF)
                      IF(VALT.EQ.63) VARABL(VBCNT+1,4)=FTFL01(37,VBSURF)
                      IF(VALT.EQ.64) VARABL(VBCNT+1,4)=FTFL01(38,VBSURF)
                      IF(VALT.EQ.65) VARABL(VBCNT+1,4)=FTFL01(39,VBSURF)
                      IF(VALT.EQ.66) VARABL(VBCNT+1,4)=FTFL01(40,VBSURF)
                      IF(VALT.EQ.67) VARABL(VBCNT+1,4)=FTFL01(41,VBSURF)
                      IF(VALT.EQ.68) VARABL(VBCNT+1,4)=FTFL01(42,VBSURF)
                      IF(VALT.EQ.69) VARABL(VBCNT+1,4)=FTFL01(43,VBSURF)
                      IF(VALT.EQ.70) VARABL(VBCNT+1,4)=FTFL01(44,VBSURF)
                      IF(VALT.EQ.71) VARABL(VBCNT+1,4)=FTFL01(45,VBSURF)
                      IF(VALT.EQ.72) VARABL(VBCNT+1,4)=FTFL01(46,VBSURF)
                      IF(VALT.EQ.73) VARABL(VBCNT+1,4)=FTFL01(47,VBSURF)
                      IF(VALT.EQ.74) VARABL(VBCNT+1,4)=FTFL01(48,VBSURF)
                      IF(VALT.GE.76.AND.VALT.LE.123)
     1                VARABL(VBCNT+1,4)=FTFL01(VALT-27,VBSURF)
                      IF(VALT.EQ.27) VARABL(VBCNT+1,5)=FTFL01(1,VBSURF)
                      IF(VALT.EQ.28) VARABL(VBCNT+1,5)=FTFL01(2,VBSURF)
                      IF(VALT.EQ.29) VARABL(VBCNT+1,5)=FTFL01(3,VBSURF)
                      IF(VALT.EQ.30) VARABL(VBCNT+1,5)=FTFL01(4,VBSURF)
                      IF(VALT.EQ.31) VARABL(VBCNT+1,5)=FTFL01(5,VBSURF)
                      IF(VALT.EQ.32) VARABL(VBCNT+1,5)=FTFL01(6,VBSURF)
                      IF(VALT.EQ.33) VARABL(VBCNT+1,5)=FTFL01(7,VBSURF)
                      IF(VALT.EQ.34) VARABL(VBCNT+1,5)=FTFL01(8,VBSURF)
                      IF(VALT.EQ.35) VARABL(VBCNT+1,5)=FTFL01(9,VBSURF)
                      IF(VALT.EQ.36) VARABL(VBCNT+1,5)=FTFL01(10,VBSURF)
                      IF(VALT.EQ.37) VARABL(VBCNT+1,5)=FTFL01(11,VBSURF)
                      IF(VALT.EQ.38) VARABL(VBCNT+1,5)=FTFL01(12,VBSURF)
                      IF(VALT.EQ.39) VARABL(VBCNT+1,5)=FTFL01(13,VBSURF)
                      IF(VALT.EQ.40) VARABL(VBCNT+1,5)=FTFL01(14,VBSURF)
                      IF(VALT.EQ.41) VARABL(VBCNT+1,5)=FTFL01(15,VBSURF)
                      IF(VALT.EQ.42) VARABL(VBCNT+1,5)=FTFL01(16,VBSURF)
                      IF(VALT.EQ.43) VARABL(VBCNT+1,5)=FTFL01(17,VBSURF)
                      IF(VALT.EQ.44) VARABL(VBCNT+1,5)=FTFL01(18,VBSURF)
                      IF(VALT.EQ.45) VARABL(VBCNT+1,5)=FTFL01(19,VBSURF)
                      IF(VALT.EQ.46) VARABL(VBCNT+1,5)=FTFL01(20,VBSURF)
                      IF(VALT.EQ.47) VARABL(VBCNT+1,5)=FTFL01(21,VBSURF)
                      IF(VALT.EQ.48) VARABL(VBCNT+1,5)=FTFL01(22,VBSURF)
                      IF(VALT.EQ.49) VARABL(VBCNT+1,5)=FTFL01(23,VBSURF)
                      IF(VALT.EQ.50) VARABL(VBCNT+1,5)=FTFL01(24,VBSURF)
                      IF(VALT.EQ.51) VARABL(VBCNT+1,5)=FTFL01(25,VBSURF)
                      IF(VALT.EQ.52) VARABL(VBCNT+1,5)=FTFL01(26,VBSURF)
                      IF(VALT.EQ.53) VARABL(VBCNT+1,5)=FTFL01(27,VBSURF)
                      IF(VALT.EQ.54) VARABL(VBCNT+1,5)=FTFL01(28,VBSURF)
                      IF(VALT.EQ.55) VARABL(VBCNT+1,5)=FTFL01(29,VBSURF)
                      IF(VALT.EQ.56) VARABL(VBCNT+1,5)=FTFL01(30,VBSURF)
                      IF(VALT.EQ.57) VARABL(VBCNT+1,5)=FTFL01(31,VBSURF)
                      IF(VALT.EQ.58) VARABL(VBCNT+1,5)=FTFL01(32,VBSURF)
                      IF(VALT.EQ.59) VARABL(VBCNT+1,5)=FTFL01(33,VBSURF)
                      IF(VALT.EQ.60) VARABL(VBCNT+1,5)=FTFL01(34,VBSURF)
                      IF(VALT.EQ.61) VARABL(VBCNT+1,5)=FTFL01(35,VBSURF)
                      IF(VALT.EQ.62) VARABL(VBCNT+1,5)=FTFL01(36,VBSURF)
                      IF(VALT.EQ.63) VARABL(VBCNT+1,5)=FTFL01(37,VBSURF)
                      IF(VALT.EQ.64) VARABL(VBCNT+1,5)=FTFL01(38,VBSURF)
                      IF(VALT.EQ.65) VARABL(VBCNT+1,5)=FTFL01(39,VBSURF)
                      IF(VALT.EQ.66) VARABL(VBCNT+1,5)=FTFL01(40,VBSURF)
                      IF(VALT.EQ.67) VARABL(VBCNT+1,5)=FTFL01(41,VBSURF)
                      IF(VALT.EQ.68) VARABL(VBCNT+1,5)=FTFL01(42,VBSURF)
                      IF(VALT.EQ.69) VARABL(VBCNT+1,5)=FTFL01(43,VBSURF)
                      IF(VALT.EQ.70) VARABL(VBCNT+1,5)=FTFL01(44,VBSURF)
                      IF(VALT.EQ.71) VARABL(VBCNT+1,5)=FTFL01(45,VBSURF)
                      IF(VALT.EQ.72) VARABL(VBCNT+1,5)=FTFL01(46,VBSURF)
                      IF(VALT.EQ.73) VARABL(VBCNT+1,5)=FTFL01(47,VBSURF)
                      IF(VALT.EQ.74) VARABL(VBCNT+1,5)=FTFL01(48,VBSURF)
                      IF(VALT.GE.76.AND.VALT.LE.123)
     1                VARABL(VBCNT+1,5)=FTFL01(VALT-27,VBSURF)
                      IF(VALT.EQ.27) VARABL(VBCNT+1,13)=FTFL01(1,VBSURF)
                      IF(VALT.EQ.28) VARABL(VBCNT+1,13)=FTFL01(2,VBSURF)
                      IF(VALT.EQ.29) VARABL(VBCNT+1,13)=FTFL01(3,VBSURF)
                      IF(VALT.EQ.30) VARABL(VBCNT+1,13)=FTFL01(4,VBSURF)
                      IF(VALT.EQ.31) VARABL(VBCNT+1,13)=FTFL01(5,VBSURF)
                      IF(VALT.EQ.32) VARABL(VBCNT+1,13)=FTFL01(6,VBSURF)
                      IF(VALT.EQ.33) VARABL(VBCNT+1,13)=FTFL01(7,VBSURF)
                      IF(VALT.EQ.34) VARABL(VBCNT+1,13)=FTFL01(8,VBSURF)
                      IF(VALT.EQ.35) VARABL(VBCNT+1,13)=FTFL01(9,VBSURF)
                      IF(VALT.EQ.36) VARABL(VBCNT+1,13)=FTFL01(10,VBSURF)
                      IF(VALT.EQ.37) VARABL(VBCNT+1,13)=FTFL01(11,VBSURF)
                      IF(VALT.EQ.38) VARABL(VBCNT+1,13)=FTFL01(12,VBSURF)
                      IF(VALT.EQ.39) VARABL(VBCNT+1,13)=FTFL01(13,VBSURF)
                      IF(VALT.EQ.40) VARABL(VBCNT+1,13)=FTFL01(14,VBSURF)
                      IF(VALT.EQ.41) VARABL(VBCNT+1,13)=FTFL01(15,VBSURF)
                      IF(VALT.EQ.42) VARABL(VBCNT+1,13)=FTFL01(16,VBSURF)
                      IF(VALT.EQ.43) VARABL(VBCNT+1,13)=FTFL01(17,VBSURF)
                      IF(VALT.EQ.44) VARABL(VBCNT+1,13)=FTFL01(18,VBSURF)
                      IF(VALT.EQ.45) VARABL(VBCNT+1,13)=FTFL01(19,VBSURF)
                      IF(VALT.EQ.46) VARABL(VBCNT+1,13)=FTFL01(20,VBSURF)
                      IF(VALT.EQ.47) VARABL(VBCNT+1,13)=FTFL01(21,VBSURF)
                      IF(VALT.EQ.48) VARABL(VBCNT+1,13)=FTFL01(22,VBSURF)
                      IF(VALT.EQ.49) VARABL(VBCNT+1,13)=FTFL01(23,VBSURF)
                      IF(VALT.EQ.50) VARABL(VBCNT+1,13)=FTFL01(24,VBSURF)
                      IF(VALT.EQ.51) VARABL(VBCNT+1,13)=FTFL01(25,VBSURF)
                      IF(VALT.EQ.52) VARABL(VBCNT+1,13)=FTFL01(26,VBSURF)
                      IF(VALT.EQ.53) VARABL(VBCNT+1,13)=FTFL01(27,VBSURF)
                      IF(VALT.EQ.54) VARABL(VBCNT+1,13)=FTFL01(28,VBSURF)
                      IF(VALT.EQ.55) VARABL(VBCNT+1,13)=FTFL01(29,VBSURF)
                      IF(VALT.EQ.56) VARABL(VBCNT+1,13)=FTFL01(30,VBSURF)
                      IF(VALT.EQ.57) VARABL(VBCNT+1,13)=FTFL01(31,VBSURF)
                      IF(VALT.EQ.58) VARABL(VBCNT+1,13)=FTFL01(32,VBSURF)
                      IF(VALT.EQ.59) VARABL(VBCNT+1,13)=FTFL01(33,VBSURF)
                      IF(VALT.EQ.60) VARABL(VBCNT+1,13)=FTFL01(34,VBSURF)
                      IF(VALT.EQ.61) VARABL(VBCNT+1,13)=FTFL01(35,VBSURF)
                      IF(VALT.EQ.62) VARABL(VBCNT+1,13)=FTFL01(36,VBSURF)
                      IF(VALT.EQ.63) VARABL(VBCNT+1,13)=FTFL01(37,VBSURF)
                      IF(VALT.EQ.64) VARABL(VBCNT+1,13)=FTFL01(38,VBSURF)
                      IF(VALT.EQ.65) VARABL(VBCNT+1,13)=FTFL01(39,VBSURF)
                      IF(VALT.EQ.66) VARABL(VBCNT+1,13)=FTFL01(40,VBSURF)
                      IF(VALT.EQ.67) VARABL(VBCNT+1,13)=FTFL01(41,VBSURF)
                      IF(VALT.EQ.68) VARABL(VBCNT+1,13)=FTFL01(42,VBSURF)
                      IF(VALT.EQ.69) VARABL(VBCNT+1,13)=FTFL01(43,VBSURF)
                      IF(VALT.EQ.70) VARABL(VBCNT+1,13)=FTFL01(44,VBSURF)
                      IF(VALT.EQ.71) VARABL(VBCNT+1,13)=FTFL01(45,VBSURF)
                      IF(VALT.EQ.72) VARABL(VBCNT+1,13)=FTFL01(46,VBSURF)
                      IF(VALT.EQ.73) VARABL(VBCNT+1,13)=FTFL01(47,VBSURF)
                      IF(VALT.EQ.74) VARABL(VBCNT+1,13)=FTFL01(48,VBSURF)
                      IF(VALT.GE.76.AND.VALT.LE.123)
     1                VARABL(VBCNT+1,13)=FTFL01(VALT-27,VBSURF)
                      IF(VALT.EQ.27) VARABL(VBCNT+1,11)=1.0D0
                      IF(VALT.EQ.28) VARABL(VBCNT+1,11)=2.0D0
                      IF(VALT.EQ.29) VARABL(VBCNT+1,11)=3.0D0
                      IF(VALT.EQ.30) VARABL(VBCNT+1,11)=4.0D0
                      IF(VALT.EQ.31) VARABL(VBCNT+1,11)=5.0D0
                      IF(VALT.EQ.32) VARABL(VBCNT+1,11)=6.0D0
                      IF(VALT.EQ.33) VARABL(VBCNT+1,11)=7.0D0
                      IF(VALT.EQ.34) VARABL(VBCNT+1,11)=8.0D0
                      IF(VALT.EQ.35) VARABL(VBCNT+1,11)=9.0D0
                      IF(VALT.EQ.36) VARABL(VBCNT+1,11)=10.0D0
                      IF(VALT.EQ.37) VARABL(VBCNT+1,11)=11.0D0
                      IF(VALT.EQ.38) VARABL(VBCNT+1,11)=12.0D0
                      IF(VALT.EQ.39) VARABL(VBCNT+1,11)=13.0D0
                      IF(VALT.EQ.40) VARABL(VBCNT+1,11)=14.0D0
                      IF(VALT.EQ.41) VARABL(VBCNT+1,11)=15.0D0
                      IF(VALT.EQ.42) VARABL(VBCNT+1,11)=16.0D0
                      IF(VALT.EQ.43) VARABL(VBCNT+1,11)=17.0D0
                      IF(VALT.EQ.44) VARABL(VBCNT+1,11)=18.0D0
                      IF(VALT.EQ.45) VARABL(VBCNT+1,11)=19.0D0
                      IF(VALT.EQ.46) VARABL(VBCNT+1,11)=20.0D0
                      IF(VALT.EQ.47) VARABL(VBCNT+1,11)=21.0D0
                      IF(VALT.EQ.48) VARABL(VBCNT+1,11)=22.0D0
                      IF(VALT.EQ.49) VARABL(VBCNT+1,11)=23.0D0
                      IF(VALT.EQ.50) VARABL(VBCNT+1,11)=24.0D0
                      IF(VALT.EQ.51) VARABL(VBCNT+1,11)=25.0D0
                      IF(VALT.EQ.52) VARABL(VBCNT+1,11)=26.0D0
                      IF(VALT.EQ.53) VARABL(VBCNT+1,11)=27.0D0
                      IF(VALT.EQ.54) VARABL(VBCNT+1,11)=28.0D0
                      IF(VALT.EQ.55) VARABL(VBCNT+1,11)=29.0D0
                      IF(VALT.EQ.56) VARABL(VBCNT+1,11)=30.0D0
                      IF(VALT.EQ.57) VARABL(VBCNT+1,11)=31.0D0
                      IF(VALT.EQ.58) VARABL(VBCNT+1,11)=32.0D0
                      IF(VALT.EQ.59) VARABL(VBCNT+1,11)=33.0D0
                      IF(VALT.EQ.60) VARABL(VBCNT+1,11)=34.0D0
                      IF(VALT.EQ.61) VARABL(VBCNT+1,11)=35.0D0
                      IF(VALT.EQ.62) VARABL(VBCNT+1,11)=36.0D0
                      IF(VALT.EQ.63) VARABL(VBCNT+1,11)=37.0D0
                      IF(VALT.EQ.64) VARABL(VBCNT+1,11)=38.0D0
                      IF(VALT.EQ.65) VARABL(VBCNT+1,11)=39.0D0
                      IF(VALT.EQ.66) VARABL(VBCNT+1,11)=40.0D0
                      IF(VALT.EQ.67) VARABL(VBCNT+1,11)=41.0D0
                      IF(VALT.EQ.68) VARABL(VBCNT+1,11)=42.0D0
                      IF(VALT.EQ.69) VARABL(VBCNT+1,11)=43.0D0
                      IF(VALT.EQ.70) VARABL(VBCNT+1,11)=44.0D0
                      IF(VALT.EQ.71) VARABL(VBCNT+1,11)=45.0D0
                      IF(VALT.EQ.72) VARABL(VBCNT+1,11)=46.0D0
                      IF(VALT.EQ.73) VARABL(VBCNT+1,11)=47.0D0
                      IF(VALT.EQ.74) VARABL(VBCNT+1,11)=48.0D0
                      IF(VALT.GE.76.AND.VALT.LE.123)
     1                VARABL(VBCNT+1,11)=DBLE(VALT-26)
C
                      VBCNT=VBCNT+1
                  END IF
                  CALL VARCLN
                  RETURN
              END IF
C
C     DO AC
              IF(VALT.EQ.75) THEN
                  IF(ALENS(1,VBSURF).NE.0.0D0) THEN
                      WRITE(OUTLYNE,*)
     1                'THE SPECIFIED SURFACE IS NOT PLANO'
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
     1            .OR.PIKUP(1,VBSURF,11).EQ.1.0D0
     1            .OR.PIKUP(1,VBSURF,12).EQ.1.0D0) THEN
C     A CV OR RD PIKUP EXISTS, DISSALLOW THE VARIABLE ASSIGNMENT
                      WRITE(OUTLYNE,*)
     1                'A CONFLICTING SURFACE SHAPE PIKUP EXISTS ON THIS SURFACE'
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
                      IF(VALT.EQ.75) VARABL(VBCNT+1,4)=ALENS(43,VBSURF)
                      IF(VALT.EQ.75) VARABL(VBCNT+1,5)=ALENS(43,VBSURF)
                      IF(VALT.EQ.75) VARABL(VBCNT+1,13)=ALENS(43,VBSURF)
                      IF(VALT.EQ.75) VARABL(VBCNT+1,11)=49.0D0
                      VBCNT=VBCNT+1
                  END IF
                  CALL VARCLN
                  RETURN
              END IF
C
C     DO ACT(0001 TO 4118)
              IF(VALT.GE.250.AND.VALT.LE.4218) THEN

                  IF(ALENS(103,VBSURF).EQ.1.0D0) THEN
                  ELSE
                      WRITE(OUTLYNE,*)
     1                'THE SPECIFIED SURFACE REQUIRES A "DEFORM" DEFINITION'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'VARIABLE ASSIGNMENT WAS NOT MADE'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
C
                  NCNT=INT(ALENS(105,VBSURF))**2
                  DO IC=1,NCNT
                      IF(VALT.EQ.IC+249) THEN
                          ISURF=VBSURF
                          DEFGR1=ALENS(103,ISURF)
                          DEFGR2=ALENS(104,ISURF)
                          DEFGR3=ALENS(105,ISURF)
                          DEFGR4=ALENS(106,ISURF)
                          DEFGR5=ALENS(107,ISURF)
                          DEFGR6=0.0D0
                          DEFGR7=ALENS(109,ISURF)
                          DEFGR8=0.0D0
                          ACTNUM=IC
                          ERR1=.FALSE.
                          ERR2=.FALSE.
                          CALL DEFGRIDS(2,ISURF,ERR1,ERR2)
                          VARABL(VBCNT+1,11)=0.0D0
                          VBCNT=VBCNT+1
                          CALL VARCLN
                      END IF
                  END DO
                  RETURN
              END IF
          ELSE
C     USE AUXCFG ARRAY
C     FIRST WE SEE IF THE CONFIG CALLED FOR IS AN ACTIVE (I.E.
C     SOMETHING IN THAT CONFIG) CONFIGURATION.
C
              IF(CFGCNT(VBCFG).EQ.0.AND.VBCFG.NE.1) THEN
C     CONFIG NOT DEFINED AS ACTIVE, DISSALLOW DEFINITION
                  WRITE(OUTLYNE,*)
     1            'REQUESTED CONFIGURATION IS CURRENTLY EMPTY'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,666) WC,INT(W1),VBCFG
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'VARIABLE ASSIGNMENT WAS NOT MADE'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C
C     THEN WE CHECK TO SEE IF THE VARIABLE IS IN THE
C     CONFIG SUBFILE DEFINITION FOR THAT CONFIGURATION
              VAROK=.FALSE.
              DO I=1,AUXMAX
                  IF(CFADD(I,2).EQ.VBSURF.AND.
     1            CFADD(I,3).EQ.VBCFG.AND.CFADD(I,1).EQ.VALT) VAROK=.TRUE.
              END DO
              IF(.NOT.VAROK) THEN
                  WRITE(OUTLYNE,*)
     1            'REQUESTED VARIABLE NOT EXPLICITLY DEFINED IN SPECIFIED'
     1            ,' CONFIGURATION'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,666) WC,INT(W1),VBCFG
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'VARIABLE ASSIGNMENT WAS NOT MADE'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              DO I=1,AUXMAX
                  IF(CFADD(I,2).EQ.VBSURF.AND.
     1            CFADD(I,3).EQ.VBCFG) THEN
C
C     WE HAVE A CONFIG AND A SURFACE NUMBER MATCH
C
                      ISRDCV=.FALSE.
                      ISRDCVT=.FALSE.
C     REMEMBER WHERE THE VARIABLE IS IN THE AUXILLIARY ARRAYS
                      IF(VALT.EQ.1.AND.CFADD(I,1).EQ.1.OR.VALT.EQ.2.AND.CFADD(I,1)
     1                .EQ.2) ISRDCV=.TRUE.
                      IF(VALT.EQ.9.AND.CFADD(I,1).EQ.9.OR.VALT.EQ.10.AND.CFADD(I,1)
     1                .EQ.10) ISRDCV=.TRUE.
                      IF(ISRDCV.OR.ISRDCVT) THEN
                          IF(ISRDCV) THEN
                              IF(CFADD(I,1).EQ.2.AND.VALT.EQ.2) THEN
                                  VARABL(VBCNT+1,14)=DBLE(I)
                                  VARABL(VBCNT+1,4) =CFVAL(I,1)
                                  VARABL(VBCNT+1,5) =CFVAL(I,1)
                                  VARABL(VBCNT+1,13) =CFVAL(I,1)
                                  VARABL(VBCNT+1,11)=DBLE(I)
                                  VBCNT=VBCNT+1
                                  GO TO 2000
                              ELSE
                                  IF(CFVAL(I,1).EQ.0.0D0) THEN
                                      VARABL(VBCNT+1,14)=DBLE(I)
                                      VARABL(VBCNT+1,4) =CFVAL(I,1)
                                      VARABL(VBCNT+1,5) =CFVAL(I,1)
                                      VARABL(VBCNT+1,13) =CFVAL(I,1)
                                      VARABL(VBCNT+1,11)=DBLE(I)
                                      VBCNT=VBCNT+1
                                      GO TO 2000
                                  ELSE
                                      VARABL(VBCNT+1,14)=DBLE(I)
                                      VARABL(VBCNT+1,4) =1.0D0/CFVAL(I,1)
                                      VARABL(VBCNT+1,5) =1.0D0/CFVAL(I,1)
                                      VARABL(VBCNT+1,13) =1.0D0/CFVAL(I,1)
                                      VARABL(VBCNT+1,11)=DBLE(I)
                                      VBCNT=VBCNT+1
                                      GO TO 2000
                                  END IF
                              END IF
                          END IF
                          IF(ISRDCVT) THEN
                              IF(CFADD(I,1).EQ.10.AND.VALT.EQ.10) THEN
                                  VARABL(VBCNT+1,14)=DBLE(I)
                                  VARABL(VBCNT+1,4) =CFVAL(I,1)
                                  VARABL(VBCNT+1,5) =CFVAL(I,1)
                                  VARABL(VBCNT+1,13) =CFVAL(I,1)
                                  VARABL(VBCNT+1,11)=DBLE(I)
                                  VBCNT=VBCNT+1
                                  GO TO 2000
                              ELSE
                                  IF(CFVAL(I,1).EQ.0.0D0) THEN
                                      VARABL(VBCNT+1,14)=DBLE(I)
                                      VARABL(VBCNT+1,4) =CFVAL(I,1)
                                      VARABL(VBCNT+1,5) =CFVAL(I,1)
                                      VARABL(VBCNT+1,13) =CFVAL(I,1)
                                      VARABL(VBCNT+1,11)=DBLE(I)
                                      VBCNT=VBCNT+1
                                      GO TO 2000
                                  ELSE
                                      VARABL(VBCNT+1,14)=DBLE(I)
                                      VARABL(VBCNT+1,4) =1.0D0/CFVAL(I,1)
                                      VARABL(VBCNT+1,5) =1.0D0/CFVAL(I,1)
                                      VARABL(VBCNT+1,13) =1.0D0/CFVAL(I,1)
                                      VARABL(VBCNT+1,11)=DBLE(I)
                                      VBCNT=VBCNT+1
                                      GO TO 2000
                                  END IF
                              END IF
                          END IF
                      END IF
                      IF(.NOT.ISRDCV.AND..NOT.ISRDCVT) THEN
C     NOT RD/CV OR RDTOR/CVTOR
                          VARABL(VBCNT+1,14)=DBLE(I)
C     WE FOUND THE VARIABLE DEFINED EXPLICITLY IN THE CONFIG
C     FILE SO ALL IS OK
C     ASSIGN WHAT NEEDS TO BE ASSIGNED  THEN GO TO 2000 FOR FURTHER
C     PROCESSING
                          IF(CFADD(I,1).GE.27.AND.CFADD(I,1).LE.74.OR.
     1                    CFADD(I,1).GE.76.AND.CFADD(I,1).LE.123) THEN
                              IF(CFADD(I,1).EQ.VALT) THEN
                                  VARABL(VBCNT+1,4) =CFVAL(I,2)
                                  VARABL(VBCNT+1,5) =CFVAL(I,2)
                                  VARABL(VBCNT+1,13) =CFVAL(I,2)
                                  VARABL(VBCNT+1,11)=DBLE(I)
                                  VBCNT=VBCNT+1
                                  GO TO 2000
                              END IF
                          ELSE
                              IF(CFADD(I,1).EQ.VALT) THEN
                                  VARABL(VBCNT+1,4) =CFVAL(I,1)
                                  VARABL(VBCNT+1,5) =CFVAL(I,1)
                                  VARABL(VBCNT+1,13) =CFVAL(I,1)
                                  VARABL(VBCNT+1,11)=DBLE(I)
                                  VBCNT=VBCNT+1
                                  GO TO 2000
                              END IF
                          END IF
C     MATCH VARIABLE TYPE AND SURFACE NUMBER IN THIS CONFIGUATION
                      END IF
                  END IF
 2000             CONTINUE
              END DO
C     FALLING THROUGH MEANS NO MATCH
C
          END IF
C
          CALL VARCLN
          RETURN
C       ALL DONE
      END
C SUB VCHECK.FOR
      SUBROUTINE VCHECK
C
          IMPLICIT NONE
C
          CHARACTER VNA*8,TAGNAM*4
C
          INTEGER I,JKVBCNT,ALLOERR,TAG4
C
          REAL*8 CFER,JKVAR
C
          DIMENSION JKVAR(:,:)
C
          ALLOCATABLE :: JKVAR
C
          INCLUDE 'datsub.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          INCLUDE 'datcfg.inc'
          DEALLOCATE(JKVAR,STAT=ALLOERR)
          ALLOCATE(JKVAR(1:4000,1:17),STAT=ALLOERR)
C
C     THIS READS THE CURRENT VARIABLES LIST AND RE-SUBMITS THE COMMANDS
C     TO THE COMMAND PROCESSOR IF THE LENS HAS BEEN MODIFIED
C     WITH AN UPDATE LENS COMMAND OR AN UPDATE CONFIGS COMMAND
C     FROM THE KEYBOARD.
C
C       VARABL(I,J) WHERE I COUNTS THE NUMBER OF VARIABLE ENTRIES
C       AND J TAKES ON THE FOLLOWING VALUES AND MEANIINGS.
C
C       J=1  > 1 THROUGH 150, PLUS ACT STARTING AT 250 A VARIABLE TYPE DESIGNATOR
C       J=2  > 1 THROUGH MAXCFG, THE CONFIGURATION DESIGNATOR
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
          IF(VBCNT.EQ.0) DEALLOCATE (JKVAR,STAT=ALLOERR)
          IF(VBCNT.EQ.0) RETURN
C     THERE WERE VARIABLES, PROCEED RE-ISSUING THE VARIABLES COMMANDS
          LASCFG=INT(SYSTEM1(56))
          JKVBCNT=VBCNT
          DO I=1,VBCNT
              JKVAR(I,1:17)=VARABL(I,1:17)
          END DO
C
 19       FORMAT(A8,',',G13.6,',',G13.6,',',G13.6,',',G13.6,',',G13.6)

          SAVE_KDP(1)=SAVEINPT(1)
          INPUT='VARIABLES'
          CALL PROCES
          REST_KDP(1)=RESTINPT(1)
          CFER=1.0D0
          DO I=1,JKVBCNT
              IF(JKVAR(I,1).EQ.2.0D0) VNA='CV      '
              IF(JKVAR(I,1).EQ.3.0D0) VNA='TH      '
              IF(JKVAR(I,1).EQ.4.0D0) VNA='CC      '
              IF(JKVAR(I,1).EQ.5.0D0) VNA='AD      '
              IF(JKVAR(I,1).EQ.6.0D0) VNA='AE      '
              IF(JKVAR(I,1).EQ.7.0D0) VNA='AF      '
              IF(JKVAR(I,1).EQ.8.0D0) VNA='AG      '
              IF(JKVAR(I,1).EQ.10.0D0) VNA='CVTOR   '
              IF(JKVAR(I,1).EQ.11.0D0) VNA='CCTOR   '
              IF(JKVAR(I,1).EQ.12.0D0) VNA='ADTOR   '
              IF(JKVAR(I,1).EQ.13.0D0) VNA='AETOR   '
              IF(JKVAR(I,1).EQ.14.0D0) VNA='AFTOR   '
              IF(JKVAR(I,1).EQ.15.0D0) VNA='AGTOR   '
              IF(JKVAR(I,1).EQ.16.0D0) VNA='ALPHA   '
              IF(JKVAR(I,1).EQ.17.0D0) VNA='BETA    '
              IF(JKVAR(I,1).EQ.18.0D0) VNA='GAMMA   '
              IF(JKVAR(I,1).EQ.19.0D0) VNA='XD      '
              IF(JKVAR(I,1).EQ.20.0D0) VNA='YD      '
              IF(JKVAR(I,1).EQ.21.0D0) VNA='N1      '
              IF(JKVAR(I,1).EQ.22.0D0) VNA='N2      '
              IF(JKVAR(I,1).EQ.23.0D0) VNA='N3      '
              IF(JKVAR(I,1).EQ.24.0D0) VNA='N4      '
              IF(JKVAR(I,1).EQ.25.0D0) VNA='N5      '
              IF(JKVAR(I,1).EQ.27.0D0) VNA='C1      '
              IF(JKVAR(I,1).EQ.28.0D0) VNA='C2      '
              IF(JKVAR(I,1).EQ.29.0D0) VNA='C3      '
              IF(JKVAR(I,1).EQ.30.0D0) VNA='C4      '
              IF(JKVAR(I,1).EQ.31.0D0) VNA='C5      '
              IF(JKVAR(I,1).EQ.32.0D0) VNA='C6      '
              IF(JKVAR(I,1).EQ.33.0D0) VNA='C7      '
              IF(JKVAR(I,1).EQ.34.0D0) VNA='C8      '
              IF(JKVAR(I,1).EQ.35.0D0) VNA='C9      '
              IF(JKVAR(I,1).EQ.36.0D0) VNA='C10     '
              IF(JKVAR(I,1).EQ.37.0D0) VNA='C11     '
              IF(JKVAR(I,1).EQ.38.0D0) VNA='C12     '
              IF(JKVAR(I,1).EQ.39.0D0) VNA='C13     '
              IF(JKVAR(I,1).EQ.40.0D0) VNA='C14     '
              IF(JKVAR(I,1).EQ.41.0D0) VNA='C15     '
              IF(JKVAR(I,1).EQ.42.0D0) VNA='C16     '
              IF(JKVAR(I,1).EQ.43.0D0) VNA='C17     '
              IF(JKVAR(I,1).EQ.44.0D0) VNA='C18     '
              IF(JKVAR(I,1).EQ.45.0D0) VNA='C19     '
              IF(JKVAR(I,1).EQ.46.0D0) VNA='C20     '
              IF(JKVAR(I,1).EQ.47.0D0) VNA='C21     '
              IF(JKVAR(I,1).EQ.48.0D0) VNA='C22     '
              IF(JKVAR(I,1).EQ.49.0D0) VNA='C23     '
              IF(JKVAR(I,1).EQ.50.0D0) VNA='C24     '
              IF(JKVAR(I,1).EQ.51.0D0) VNA='C25     '
              IF(JKVAR(I,1).EQ.52.0D0) VNA='C26     '
              IF(JKVAR(I,1).EQ.53.0D0) VNA='C27     '
              IF(JKVAR(I,1).EQ.54.0D0) VNA='C28     '
              IF(JKVAR(I,1).EQ.55.0D0) VNA='C29     '
              IF(JKVAR(I,1).EQ.56.0D0) VNA='C30     '
              IF(JKVAR(I,1).EQ.57.0D0) VNA='C31     '
              IF(JKVAR(I,1).EQ.58.0D0) VNA='C32     '
              IF(JKVAR(I,1).EQ.59.0D0) VNA='C33     '
              IF(JKVAR(I,1).EQ.60.0D0) VNA='C34     '
              IF(JKVAR(I,1).EQ.61.0D0) VNA='C35     '
              IF(JKVAR(I,1).EQ.62.0D0) VNA='C36     '
              IF(JKVAR(I,1).EQ.63.0D0) VNA='C37     '
              IF(JKVAR(I,1).EQ.64.0D0) VNA='C38     '
              IF(JKVAR(I,1).EQ.65.0D0) VNA='C39     '
              IF(JKVAR(I,1).EQ.66.0D0) VNA='C40     '
              IF(JKVAR(I,1).EQ.67.0D0) VNA='C41     '
              IF(JKVAR(I,1).EQ.68.0D0) VNA='C42     '
              IF(JKVAR(I,1).EQ.69.0D0) VNA='C43     '
              IF(JKVAR(I,1).EQ.70.0D0) VNA='C44     '
              IF(JKVAR(I,1).EQ.71.0D0) VNA='C45     '
              IF(JKVAR(I,1).EQ.72.0D0) VNA='C46     '
              IF(JKVAR(I,1).EQ.73.0D0) VNA='C47     '
              IF(JKVAR(I,1).EQ.74.0D0) VNA='C48     '
              IF(JKVAR(I,1).EQ.75.0D0) VNA='AC      '
              IF(JKVAR(I,1).EQ.76.0D0) VNA='C49     '
              IF(JKVAR(I,1).EQ.77.0D0) VNA='C50     '
              IF(JKVAR(I,1).EQ.78.0D0) VNA='C51     '
              IF(JKVAR(I,1).EQ.79.0D0) VNA='C52     '
              IF(JKVAR(I,1).EQ.80.0D0) VNA='C53     '
              IF(JKVAR(I,1).EQ.81.0D0) VNA='C54     '
              IF(JKVAR(I,1).EQ.82.0D0) VNA='C55     '
              IF(JKVAR(I,1).EQ.83.0D0) VNA='C56     '
              IF(JKVAR(I,1).EQ.84.0D0) VNA='C57     '
              IF(JKVAR(I,1).EQ.85.0D0) VNA='C58     '
              IF(JKVAR(I,1).EQ.86.0D0) VNA='C59     '
              IF(JKVAR(I,1).EQ.87.0D0) VNA='C60     '
              IF(JKVAR(I,1).EQ.88.0D0) VNA='C61     '
              IF(JKVAR(I,1).EQ.89.0D0) VNA='C62     '
              IF(JKVAR(I,1).EQ.90.0D0) VNA='C63     '
              IF(JKVAR(I,1).EQ.91.0D0) VNA='C64     '
              IF(JKVAR(I,1).EQ.92.0D0) VNA='C65     '
              IF(JKVAR(I,1).EQ.93.0D0) VNA='C66     '
              IF(JKVAR(I,1).EQ.94.0D0) VNA='C67     '
              IF(JKVAR(I,1).EQ.95.0D0) VNA='C68     '
              IF(JKVAR(I,1).EQ.96.0D0) VNA='C69     '
              IF(JKVAR(I,1).EQ.97.0D0) VNA='C70     '
              IF(JKVAR(I,1).EQ.98.0D0) VNA='C71     '
              IF(JKVAR(I,1).EQ.99.0D0) VNA='C72     '
              IF(JKVAR(I,1).EQ.100.0D0) VNA='C73     '
              IF(JKVAR(I,1).EQ.101.0D0) VNA='C74     '
              IF(JKVAR(I,1).EQ.102.0D0) VNA='C75     '
              IF(JKVAR(I,1).EQ.103.0D0) VNA='C76     '
              IF(JKVAR(I,1).EQ.104.0D0) VNA='C77     '
              IF(JKVAR(I,1).EQ.105.0D0) VNA='C78     '
              IF(JKVAR(I,1).EQ.106.0D0) VNA='C79     '
              IF(JKVAR(I,1).EQ.107.0D0) VNA='C80     '
              IF(JKVAR(I,1).EQ.108.0D0) VNA='C81     '
              IF(JKVAR(I,1).EQ.109.0D0) VNA='C82     '
              IF(JKVAR(I,1).EQ.110.0D0) VNA='C83     '
              IF(JKVAR(I,1).EQ.111.0D0) VNA='C84     '
              IF(JKVAR(I,1).EQ.112.0D0) VNA='C85     '
              IF(JKVAR(I,1).EQ.113.0D0) VNA='C86     '
              IF(JKVAR(I,1).EQ.114.0D0) VNA='C87     '
              IF(JKVAR(I,1).EQ.115.0D0) VNA='C88     '
              IF(JKVAR(I,1).EQ.116.0D0) VNA='C89     '
              IF(JKVAR(I,1).EQ.117.0D0) VNA='C90     '
              IF(JKVAR(I,1).EQ.118.0D0) VNA='C91     '
              IF(JKVAR(I,1).EQ.119.0D0) VNA='C92     '
              IF(JKVAR(I,1).EQ.120.0D0) VNA='C93     '
              IF(JKVAR(I,1).EQ.121.0D0) VNA='C94     '
              IF(JKVAR(I,1).EQ.122.0D0) VNA='C95     '
              IF(JKVAR(I,1).EQ.123.0D0) VNA='C96     '
              IF(JKVAR(I,1).EQ.124.0D0) VNA='N6      '
              IF(JKVAR(I,1).EQ.125.0D0) VNA='N7      '
              IF(JKVAR(I,1).EQ.126.0D0) VNA='N8      '
              IF(JKVAR(I,1).EQ.127.0D0) VNA='N9      '
              IF(JKVAR(I,1).EQ.128.0D0) VNA='N10     '
              IF(JKVAR(I,1).EQ.129.0D0) VNA='AH      '
              IF(JKVAR(I,1).EQ.131.0D0) VNA='AI      '
              IF(JKVAR(I,1).EQ.131.0D0) VNA='AJ      '
              IF(JKVAR(I,1).EQ.132.0D0) VNA='AK      '
              IF(JKVAR(I,1).EQ.133.0D0) VNA='AL      '
              IF(JKVAR(I,1).EQ.134.0D0) VNA='ZD      '
              IF(JKVAR(I,1).EQ.135.0D0) VNA='INDEX   '
              IF(JKVAR(I,1).EQ.136.0D0) VNA='VNUM    '
              IF(JKVAR(I,1).EQ.137.0D0) VNA='PIVX    '
              IF(JKVAR(I,1).EQ.138.0D0) VNA='PIVY    '
              IF(JKVAR(I,1).EQ.139.0D0) VNA='PIVZ    '
              IF(JKVAR(I,1).EQ.140.0D0) VNA='DPART   '
              IF(JKVAR(I,1).EQ.141.0D0) VNA='CLPX    '
              IF(JKVAR(I,1).EQ.142.0D0) VNA='CLPY    '
              IF(JKVAR(I,1).EQ.143.0D0) VNA='GDX     '
              IF(JKVAR(I,1).EQ.144.0D0) VNA='GDY     '
              IF(JKVAR(I,1).EQ.145.0D0) VNA='GDZ     '
              IF(JKVAR(I,1).EQ.146.0D0) VNA='GALPHA  '
              IF(JKVAR(I,1).EQ.147.0D0) VNA='GBETA   '
              IF(JKVAR(I,1).EQ.148.0D0) VNA='GGAMMA  '
              IF(JKVAR(I,1).EQ.149.0D0) VNA='GRS     '
              IF(JKVAR(I,1).EQ.150.0D0) VNA='MACVAR  '
C     VNAME ACT0001 TO ACT3969 HAVE NUMBERS 250 TO 4218
              IF(JKVAR(I,1).GT.249.0D0.AND.JKVAR(I,1).LT.4219.0D0) THEN
                  TAG4=INT(JKVAR(I,1))-249
                  CALL ITOA4(TAGNAM,TAG4)
                  VNA='ACT'//TAGNAM//' '
              END IF
C     BUILD A LINES AND ISSUE TO CONTRO
C
              IF(INT(JKVAR(I,2)).GT.LASCFG.OR.
     1        INT(JKVAR(I,2)).GT.MAXCFG) THEN
                  WRITE(OUTLYNE,*)'REQUESTED LENS CONFIGURATION DOES NOT EXISTS'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'VARIABLE ENTRY:'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,19) VNA,W1,W2,W3,W4,W5
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'DROPPED FROM VARIABLE LIST'
                  CALL SHOWIT(1)
              ELSE
                  SAVE_KDP(1)=SAVEINPT(1)
                  CFER=JKVAR(I,2)
                  WC='CFG'
                  W1=CFER
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
                  SQ=0
                  SST=0
                  SSI=0
                  CALL CONTRO
                  REST_KDP(1)=RESTINPT(1)
                  SAVE_KDP(1)=SAVEINPT(1)
                  WC=VNA
                  W1=JKVAR(I,3)
                  W2=JKVAR(I,7)
                  W3=JKVAR(I,8)
                  W4=JKVAR(I,9)
                  W5=JKVAR(I,10)
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
                  SQ=0
                  SST=0
                  SSI=0
                  CALL CONTRO
                  REST_KDP(1)=RESTINPT(1)
              END IF
          END DO
          SAVE_KDP(1)=SAVEINPT(1)
          INPUT='EOS'
          CALL PROCES
          REST_KDP(1)=RESTINPT(1)
          F29=0
          DEALLOCATE(JKVAR,STAT=ALLOERR)
          RETURN
      END
C ROUTINE AUTOFF.FOR
C
      SUBROUTINE AUTOFF
C
          IMPLICIT NONE
C
C       THIS SHUTS OFF OPTIMIZATION DEFINITIONS
C
          INCLUDE 'datsub.inc'
          INCLUDE 'datmai.inc'
C
          TOPCNT=0
          TVBCNT=0
          IF(OPTM_INIT.EQ.1) THEN
C       MERIT OPERANDS INITIALIZATION
              OPCNT=0
              FMTEXT=.FALSE.
C       VARIABLES INITIALIZATION
              VBCNT=0
C       TOLERANCE VARIABLES INITIALIZATION
              TVBCNT=0
              FMTFLG=.FALSE.
              FMTFMT=0.0D0
          END IF
C
          RETURN
      END
