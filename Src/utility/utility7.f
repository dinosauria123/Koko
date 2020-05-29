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

C       SEVENTH SET OF UTILTIY ROUTINES GO HERE

C SUB LWRITE.FOR
      SUBROUTINE LWRITE
C
          IMPLICIT NONE
C
C       THIS SUBROUTINE IS CALLED TO WRITE OUT A LINE OF 5
C     GENERAL PURPOSE NUMERIC STORAGE REGISTERS
C     USING THE FORMAT SPECIFIED BY THE LFORMAT COMMAND
C
          CHARACTER CSTRING*132,LFORM1*141,LLAB*80,WSSA*150
C
          INTEGER LL,I,J,K,NVNVAL,NVNVALL,II
C
          REAL*8 V(1:5)
C
          INCLUDE 'datmai.inc'
C
          IF(STI.EQ.1) THEN
              WRITE(OUTLYNE,*) 'NO ADDITIONAL INFORMATION AVAILABLE'
              CALL SHOWIT(1)
              RETURN
          END IF
          IF(SST.EQ.1) THEN
              WRITE(OUTLYNE,*) WC,' TAKES NO STRING INPUT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SN.EQ.0) THEN
              WRITE(OUTLYNE,*) WC,' REQUIRES EXPLICIT NUMERIC INPUT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C     QUALIFIER CHECK
          IF(SQ.EQ.1) THEN
C     CHECK FOR VALID QUALIFIER WORDS FOR "LWRITE"
              IF(WQ.EQ.'A1')   GO TO 20
              IF(WQ.EQ.'A2')   GO TO 20
              IF(WQ.EQ.'A3')   GO TO 20
              IF(WQ.EQ.'A4')   GO TO 20
              IF(WQ.EQ.'A5')   GO TO 20
              IF(WQ.EQ.'A6')   GO TO 20
              IF(WQ.EQ.'A7')   GO TO 20
              IF(WQ.EQ.'A8')   GO TO 20
              IF(WQ.EQ.'A9')   GO TO 20
              IF(WQ.EQ.'A10')  GO TO 20
              IF(WQ.EQ.'A11')  GO TO 20
              IF(WQ.EQ.'A12')  GO TO 20
              IF(WQ.EQ.'A13')  GO TO 20
              IF(WQ.EQ.'A14')  GO TO 20
              IF(WQ.EQ.'A15')  GO TO 20
              IF(WQ.EQ.'A16')  GO TO 20
              IF(WQ.EQ.'A17')  GO TO 20
              IF(WQ.EQ.'A18')  GO TO 20
              IF(WQ.EQ.'A19')  GO TO 20
              IF(WQ.EQ.'A20')  GO TO 20
              IF(WQ.EQ.'A21')  GO TO 20
              IF(WQ.EQ.'A22')  GO TO 20
              IF(WQ.EQ.'A23')  GO TO 20
              IF(WQ.EQ.'A24')  GO TO 20
              IF(WQ.EQ.'A25')  GO TO 20
              IF(WQ.EQ.'A26')  GO TO 20
              IF(WQ.EQ.'A27')  GO TO 20
              IF(WQ.EQ.'A28')  GO TO 20
              IF(WQ.EQ.'A29')  GO TO 20
              IF(WQ.EQ.'A30')  GO TO 20
              IF(WQ.EQ.'A31')  GO TO 20
              IF(WQ.EQ.'A32')  GO TO 20
              IF(WQ.EQ.'A33')  GO TO 20
              IF(WQ.EQ.'A34')  GO TO 20
              IF(WQ.EQ.'A35')  GO TO 20
              IF(WQ.EQ.'A36')  GO TO 20
              IF(WQ.EQ.'A37')  GO TO 20
              IF(WQ.EQ.'A38')  GO TO 20
              IF(WQ.EQ.'A39')  GO TO 20
              IF(WQ.EQ.'A40')  GO TO 20
              IF(WQ.EQ.'A41')  GO TO 20
              IF(WQ.EQ.'A42')  GO TO 20
              IF(WQ.EQ.'A43')  GO TO 20
              IF(WQ.EQ.'A44')  GO TO 20
              IF(WQ.EQ.'A45')  GO TO 20
              IF(WQ.EQ.'A46')  GO TO 20
              IF(WQ.EQ.'A47')  GO TO 20
              IF(WQ.EQ.'A48')  GO TO 20
              IF(WQ.EQ.'A49')  GO TO 20
              IF(WQ.EQ.'A50')  GO TO 20
              IF(WQ.EQ.'A51')  GO TO 20
              IF(WQ.EQ.'A52')  GO TO 20
              IF(WQ.EQ.'A53')  GO TO 20
              IF(WQ.EQ.'A54')  GO TO 20
              IF(WQ.EQ.'A55')  GO TO 20
              IF(WQ.EQ.'A56')  GO TO 20
              IF(WQ.EQ.'A57')  GO TO 20
              IF(WQ.EQ.'A58')  GO TO 20
              IF(WQ.EQ.'A59')  GO TO 20
              IF(WQ.EQ.'A60')  GO TO 20
              IF(WQ.EQ.'A61')  GO TO 20
              IF(WQ.EQ.'A62')  GO TO 20
              IF(WQ.EQ.'A63')  GO TO 20
              IF(WQ.EQ.'A64')  GO TO 20
              IF(WQ.EQ.'A65')  GO TO 20
              IF(WQ.EQ.'A66')  GO TO 20
              IF(WQ.EQ.'A67')  GO TO 20
              IF(WQ.EQ.'A68')  GO TO 20
              IF(WQ.EQ.'A69')  GO TO 20
              IF(WQ.EQ.'A70')  GO TO 20
              IF(WQ.EQ.'A71')  GO TO 20
              IF(WQ.EQ.'A72')  GO TO 20
              IF(WQ.EQ.'A73')  GO TO 20
              IF(WQ.EQ.'A74')  GO TO 20
              IF(WQ.EQ.'A75')  GO TO 20
              IF(WQ.EQ.'A76')  GO TO 20
              IF(WQ.EQ.'A77')  GO TO 20
              IF(WQ.EQ.'A78')  GO TO 20
              IF(WQ.EQ.'A79')  GO TO 20
              IF(WQ.EQ.'A80')  GO TO 20
              IF(WQ.EQ.'A81')  GO TO 20
              IF(WQ.EQ.'A82')  GO TO 20
              IF(WQ.EQ.'A83')  GO TO 20
              IF(WQ.EQ.'A84')  GO TO 20
              IF(WQ.EQ.'A85')  GO TO 20
              IF(WQ.EQ.'A86')  GO TO 20
              IF(WQ.EQ.'A87')  GO TO 20
              IF(WQ.EQ.'A88')  GO TO 20
              IF(WQ.EQ.'A89')  GO TO 20
              IF(WQ.EQ.'A90')  GO TO 20
              IF(WQ.EQ.'A91')  GO TO 20
              IF(WQ.EQ.'A92')  GO TO 20
              IF(WQ.EQ.'A93')  GO TO 20
              IF(WQ.EQ.'A94')  GO TO 20
              IF(WQ.EQ.'A95')  GO TO 20
              IF(WQ.EQ.'A96')  GO TO 20
              IF(WQ.EQ.'A97')  GO TO 20
              IF(WQ.EQ.'A98')  GO TO 20
              IF(WQ.EQ.'A99')  GO TO 20
              IF(WQ.EQ.'A100') GO TO 20
              IF(WQ.EQ.'A101')  GO TO 20
              IF(WQ.EQ.'A102')  GO TO 20
              IF(WQ.EQ.'A103')  GO TO 20
              IF(WQ.EQ.'A104')  GO TO 20
              IF(WQ.EQ.'A105')  GO TO 20
              IF(WQ.EQ.'A106')  GO TO 20
              IF(WQ.EQ.'A107')  GO TO 20
              IF(WQ.EQ.'A108')  GO TO 20
              IF(WQ.EQ.'A109')  GO TO 20
              IF(WQ.EQ.'A110')  GO TO 20
              IF(WQ.EQ.'A111')  GO TO 20
              IF(WQ.EQ.'A112')  GO TO 20
              IF(WQ.EQ.'A113')  GO TO 20
              IF(WQ.EQ.'A114')  GO TO 20
              IF(WQ.EQ.'A115')  GO TO 20
              IF(WQ.EQ.'A116')  GO TO 20
              IF(WQ.EQ.'A117')  GO TO 20
              IF(WQ.EQ.'A118')  GO TO 20
              IF(WQ.EQ.'A119')  GO TO 20
              IF(WQ.EQ.'A120')  GO TO 20
              IF(WQ.EQ.'A121')  GO TO 20
              IF(WQ.EQ.'A122')  GO TO 20
              IF(WQ.EQ.'A123')  GO TO 20
              IF(WQ.EQ.'A124')  GO TO 20
              IF(WQ.EQ.'A125')  GO TO 20
              IF(WQ.EQ.'A126')  GO TO 20
              IF(WQ.EQ.'A127')  GO TO 20
              IF(WQ.EQ.'A128')  GO TO 20
              IF(WQ.EQ.'A129')  GO TO 20
              IF(WQ.EQ.'A130')  GO TO 20
              IF(WQ.EQ.'A131')  GO TO 20
              IF(WQ.EQ.'A132')  GO TO 20
              IF(WQ.EQ.'A133')  GO TO 20
              IF(WQ.EQ.'A134')  GO TO 20
              IF(WQ.EQ.'A135')  GO TO 20
              IF(WQ.EQ.'A136')  GO TO 20
              IF(WQ.EQ.'A137')  GO TO 20
              IF(WQ.EQ.'A138')  GO TO 20
              IF(WQ.EQ.'A139')  GO TO 20
              IF(WQ.EQ.'A140')  GO TO 20
              IF(WQ.EQ.'A141')  GO TO 20
              IF(WQ.EQ.'A142')  GO TO 20
              IF(WQ.EQ.'A143')  GO TO 20
              IF(WQ.EQ.'A144')  GO TO 20
              IF(WQ.EQ.'A145')  GO TO 20
              IF(WQ.EQ.'A146')  GO TO 20
              IF(WQ.EQ.'A147')  GO TO 20
              IF(WQ.EQ.'A148')  GO TO 20
              IF(WQ.EQ.'A149')  GO TO 20
              IF(WQ.EQ.'A150')  GO TO 20
              IF(WQ.EQ.'A151')  GO TO 20
              IF(WQ.EQ.'A152')  GO TO 20
              IF(WQ.EQ.'A153')  GO TO 20
              IF(WQ.EQ.'A154')  GO TO 20
              IF(WQ.EQ.'A155')  GO TO 20
              IF(WQ.EQ.'A156')  GO TO 20
              IF(WQ.EQ.'A157')  GO TO 20
              IF(WQ.EQ.'A158')  GO TO 20
              IF(WQ.EQ.'A159')  GO TO 20
              IF(WQ.EQ.'A160')  GO TO 20
              IF(WQ.EQ.'A161')  GO TO 20
              IF(WQ.EQ.'A162')  GO TO 20
              IF(WQ.EQ.'A163')  GO TO 20
              IF(WQ.EQ.'A164')  GO TO 20
              IF(WQ.EQ.'A165')  GO TO 20
              IF(WQ.EQ.'A166')  GO TO 20
              IF(WQ.EQ.'A167')  GO TO 20
              IF(WQ.EQ.'A168')  GO TO 20
              IF(WQ.EQ.'A169')  GO TO 20
              IF(WQ.EQ.'A170')  GO TO 20
              IF(WQ.EQ.'A171')  GO TO 20
              IF(WQ.EQ.'A172')  GO TO 20
              IF(WQ.EQ.'A173')  GO TO 20
              IF(WQ.EQ.'A174')  GO TO 20
              IF(WQ.EQ.'A175')  GO TO 20
              IF(WQ.EQ.'A176')  GO TO 20
              IF(WQ.EQ.'A177')  GO TO 20
              IF(WQ.EQ.'A178')  GO TO 20
              IF(WQ.EQ.'A179')  GO TO 20
              IF(WQ.EQ.'A180')  GO TO 20
              IF(WQ.EQ.'A181')  GO TO 20
              IF(WQ.EQ.'A182')  GO TO 20
              IF(WQ.EQ.'A183')  GO TO 20
              IF(WQ.EQ.'A184')  GO TO 20
              IF(WQ.EQ.'A185')  GO TO 20
              IF(WQ.EQ.'A186')  GO TO 20
              IF(WQ.EQ.'A187')  GO TO 20
              IF(WQ.EQ.'A188')  GO TO 20
              IF(WQ.EQ.'A189')  GO TO 20
              IF(WQ.EQ.'A190')  GO TO 20
              IF(WQ.EQ.'A191')  GO TO 20
              IF(WQ.EQ.'A192')  GO TO 20
              IF(WQ.EQ.'A193')  GO TO 20
              IF(WQ.EQ.'A194')  GO TO 20
              IF(WQ.EQ.'A195')  GO TO 20
              IF(WQ.EQ.'A196')  GO TO 20
              IF(WQ.EQ.'A197')  GO TO 20
              IF(WQ.EQ.'A198')  GO TO 20
              IF(WQ.EQ.'A199')  GO TO 20
              IF(WQ.EQ.'A200')  GO TO 20
              IF(WQ.EQ.'A201')  GO TO 20
              IF(WQ.EQ.'A202')  GO TO 20
              IF(WQ.EQ.'A203')  GO TO 20
              IF(WQ.EQ.'A204')  GO TO 20
              IF(WQ.EQ.'A205')  GO TO 20
              IF(WQ.EQ.'A206')  GO TO 20
              IF(WQ.EQ.'A207')  GO TO 20
              IF(WQ.EQ.'A208')  GO TO 20
              IF(WQ.EQ.'A209')  GO TO 20
              IF(WQ.EQ.'A210')  GO TO 20
              IF(WQ.EQ.'A211')  GO TO 20
              IF(WQ.EQ.'A212')  GO TO 20
              IF(WQ.EQ.'A213')  GO TO 20
              IF(WQ.EQ.'A214')  GO TO 20
              IF(WQ.EQ.'A215')  GO TO 20
              IF(WQ.EQ.'A216')  GO TO 20
              IF(WQ.EQ.'A217')  GO TO 20
              IF(WQ.EQ.'A218')  GO TO 20
              IF(WQ.EQ.'A219')  GO TO 20
              IF(WQ.EQ.'A220')  GO TO 20
              IF(WQ.EQ.'A221')  GO TO 20
              IF(WQ.EQ.'A222')  GO TO 20
              IF(WQ.EQ.'A223')  GO TO 20
              IF(WQ.EQ.'A224')  GO TO 20
              IF(WQ.EQ.'A225')  GO TO 20
              IF(WQ.EQ.'A226')  GO TO 20
              IF(WQ.EQ.'A227')  GO TO 20
              IF(WQ.EQ.'A228')  GO TO 20
              IF(WQ.EQ.'A229')  GO TO 20
              IF(WQ.EQ.'A230')  GO TO 20
              IF(WQ.EQ.'A231')  GO TO 20
              IF(WQ.EQ.'A232')  GO TO 20
              IF(WQ.EQ.'A233')  GO TO 20
              IF(WQ.EQ.'A234')  GO TO 20
              IF(WQ.EQ.'A235')  GO TO 20
              IF(WQ.EQ.'A236')  GO TO 20
              IF(WQ.EQ.'A237')  GO TO 20
              IF(WQ.EQ.'A238')  GO TO 20
              IF(WQ.EQ.'A239')  GO TO 20
              IF(WQ.EQ.'A240')  GO TO 20
              IF(WQ.EQ.'A241')  GO TO 20
              IF(WQ.EQ.'A242')  GO TO 20
              IF(WQ.EQ.'A243')  GO TO 20
              IF(WQ.EQ.'A244')  GO TO 20
              IF(WQ.EQ.'A245')  GO TO 20
              IF(WQ.EQ.'A246')  GO TO 20
              IF(WQ.EQ.'A247')  GO TO 20
              IF(WQ.EQ.'A248')  GO TO 20
              IF(WQ.EQ.'A249')  GO TO 20
              IF(WQ.EQ.'A250')  GO TO 20
              IF(WQ.EQ.'A251')  GO TO 20
              IF(WQ.EQ.'A252')  GO TO 20
              IF(WQ.EQ.'A253')  GO TO 20
              IF(WQ.EQ.'A254')  GO TO 20
              IF(WQ.EQ.'A255')  GO TO 20
              IF(WQ.EQ.'A256')  GO TO 20
              IF(WQ.EQ.'A257')  GO TO 20
              IF(WQ.EQ.'A258')  GO TO 20
              IF(WQ.EQ.'A259')  GO TO 20
              IF(WQ.EQ.'A260')  GO TO 20
              IF(WQ.EQ.'A261')  GO TO 20
              IF(WQ.EQ.'A262')  GO TO 20
              IF(WQ.EQ.'A263')  GO TO 20
              IF(WQ.EQ.'A264')  GO TO 20
              IF(WQ.EQ.'A265')  GO TO 20
              IF(WQ.EQ.'A266')  GO TO 20
              IF(WQ.EQ.'A267')  GO TO 20
              IF(WQ.EQ.'A268')  GO TO 20
              IF(WQ.EQ.'A269')  GO TO 20
              IF(WQ.EQ.'A270')  GO TO 20
              IF(WQ.EQ.'A271')  GO TO 20
              IF(WQ.EQ.'A272')  GO TO 20
              IF(WQ.EQ.'A273')  GO TO 20
              IF(WQ.EQ.'A274')  GO TO 20
              IF(WQ.EQ.'A275')  GO TO 20
              IF(WQ.EQ.'A276')  GO TO 20
              IF(WQ.EQ.'A277')  GO TO 20
              IF(WQ.EQ.'A278')  GO TO 20
              IF(WQ.EQ.'A279')  GO TO 20
              IF(WQ.EQ.'A280')  GO TO 20
              IF(WQ.EQ.'A281')  GO TO 20
              IF(WQ.EQ.'A282')  GO TO 20
              IF(WQ.EQ.'A283')  GO TO 20
              IF(WQ.EQ.'A284')  GO TO 20
              IF(WQ.EQ.'A285')  GO TO 20
              IF(WQ.EQ.'A286')  GO TO 20
              IF(WQ.EQ.'A287')  GO TO 20
              IF(WQ.EQ.'A288')  GO TO 20
              IF(WQ.EQ.'A289')  GO TO 20
              IF(WQ.EQ.'A290')  GO TO 20
              IF(WQ.EQ.'A291')  GO TO 20
              IF(WQ.EQ.'A292')  GO TO 20
              IF(WQ.EQ.'A293')  GO TO 20
              IF(WQ.EQ.'A294')  GO TO 20
              IF(WQ.EQ.'A295')  GO TO 20
              IF(WQ.EQ.'A296')  GO TO 20
              IF(WQ.EQ.'A297')  GO TO 20
              IF(WQ.EQ.'A298')  GO TO 20
              IF(WQ.EQ.'A299')  GO TO 20
              IF(WQ.EQ.'A300')  GO TO 20
              IF(WQ.EQ.'A301')  GO TO 20
              IF(WQ.EQ.'A302')  GO TO 20
              IF(WQ.EQ.'A303')  GO TO 20
              IF(WQ.EQ.'A304')  GO TO 20
              IF(WQ.EQ.'A305')  GO TO 20
              IF(WQ.EQ.'A306')  GO TO 20
              IF(WQ.EQ.'A307')  GO TO 20
              IF(WQ.EQ.'A308')  GO TO 20
              IF(WQ.EQ.'A309')  GO TO 20
              IF(WQ.EQ.'A310')  GO TO 20
              IF(WQ.EQ.'A311')  GO TO 20
              IF(WQ.EQ.'A312')  GO TO 20
              IF(WQ.EQ.'A313')  GO TO 20
              IF(WQ.EQ.'A314')  GO TO 20
              IF(WQ.EQ.'A315')  GO TO 20
              IF(WQ.EQ.'A316')  GO TO 20
              IF(WQ.EQ.'A317')  GO TO 20
              IF(WQ.EQ.'A318')  GO TO 20
              IF(WQ.EQ.'A319')  GO TO 20
              IF(WQ.EQ.'A320')  GO TO 20
              IF(WQ.EQ.'A321')  GO TO 20
              IF(WQ.EQ.'A322')  GO TO 20
              IF(WQ.EQ.'A323')  GO TO 20
              IF(WQ.EQ.'A324')  GO TO 20
              IF(WQ.EQ.'A325')  GO TO 20
              IF(WQ.EQ.'A326')  GO TO 20
              IF(WQ.EQ.'A327')  GO TO 20
              IF(WQ.EQ.'A328')  GO TO 20
              IF(WQ.EQ.'A329')  GO TO 20
              IF(WQ.EQ.'A330')  GO TO 20
              IF(WQ.EQ.'A331')  GO TO 20
              IF(WQ.EQ.'A332')  GO TO 20
              IF(WQ.EQ.'A333')  GO TO 20
              IF(WQ.EQ.'A334')  GO TO 20
              IF(WQ.EQ.'A335')  GO TO 20
              IF(WQ.EQ.'A336')  GO TO 20
              IF(WQ.EQ.'A337')  GO TO 20
              IF(WQ.EQ.'A338')  GO TO 20
              IF(WQ.EQ.'A339')  GO TO 20
              IF(WQ.EQ.'A340')  GO TO 20
              IF(WQ.EQ.'A341')  GO TO 20
              IF(WQ.EQ.'A342')  GO TO 20
              IF(WQ.EQ.'A343')  GO TO 20
              IF(WQ.EQ.'A344')  GO TO 20
              IF(WQ.EQ.'A345')  GO TO 20
              IF(WQ.EQ.'A346')  GO TO 20
              IF(WQ.EQ.'A347')  GO TO 20
              IF(WQ.EQ.'A348')  GO TO 20
              IF(WQ.EQ.'A349')  GO TO 20
              IF(WQ.EQ.'A350')  GO TO 20
              IF(WQ.EQ.'A351')  GO TO 20
              IF(WQ.EQ.'A352')  GO TO 20
              IF(WQ.EQ.'A353')  GO TO 20
              IF(WQ.EQ.'A354')  GO TO 20
              IF(WQ.EQ.'A355')  GO TO 20
              IF(WQ.EQ.'A356')  GO TO 20
              IF(WQ.EQ.'A357')  GO TO 20
              IF(WQ.EQ.'A358')  GO TO 20
              IF(WQ.EQ.'A359')  GO TO 20
              IF(WQ.EQ.'A360')  GO TO 20
              IF(WQ.EQ.'A361')  GO TO 20
              IF(WQ.EQ.'A362')  GO TO 20
              IF(WQ.EQ.'A363')  GO TO 20
              IF(WQ.EQ.'A364')  GO TO 20
              IF(WQ.EQ.'A365')  GO TO 20
              IF(WQ.EQ.'A366')  GO TO 20
              IF(WQ.EQ.'A367')  GO TO 20
              IF(WQ.EQ.'A368')  GO TO 20
              IF(WQ.EQ.'A369')  GO TO 20
              IF(WQ.EQ.'A370')  GO TO 20
              IF(WQ.EQ.'A371')  GO TO 20
              IF(WQ.EQ.'A372')  GO TO 20
              IF(WQ.EQ.'A373')  GO TO 20
              IF(WQ.EQ.'A374')  GO TO 20
              IF(WQ.EQ.'A375')  GO TO 20
              IF(WQ.EQ.'A376')  GO TO 20
              IF(WQ.EQ.'A377')  GO TO 20
              IF(WQ.EQ.'A378')  GO TO 20
              IF(WQ.EQ.'A379')  GO TO 20
              IF(WQ.EQ.'A380')  GO TO 20
              IF(WQ.EQ.'A381')  GO TO 20
              IF(WQ.EQ.'A382')  GO TO 20
              IF(WQ.EQ.'A383')  GO TO 20
              IF(WQ.EQ.'A384')  GO TO 20
              IF(WQ.EQ.'A385')  GO TO 20
              IF(WQ.EQ.'A386')  GO TO 20
              IF(WQ.EQ.'A387')  GO TO 20
              IF(WQ.EQ.'A388')  GO TO 20
              IF(WQ.EQ.'A389')  GO TO 20
              IF(WQ.EQ.'A390')  GO TO 20
              IF(WQ.EQ.'A391')  GO TO 20
              IF(WQ.EQ.'A392')  GO TO 20
              IF(WQ.EQ.'A393')  GO TO 20
              IF(WQ.EQ.'A394')  GO TO 20
              IF(WQ.EQ.'A395')  GO TO 20
              IF(WQ.EQ.'A396')  GO TO 20
              IF(WQ.EQ.'A397')  GO TO 20
              IF(WQ.EQ.'A398')  GO TO 20
              IF(WQ.EQ.'A399')  GO TO 20
              IF(WQ.EQ.'A400')  GO TO 20
              WRITE(OUTLYNE,*)
     1        'INVALID QUALIFIER WORD USED WITH "LWRITE"'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)
     1        'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
 20           CONTINUE
              IF(WQ.EQ.'A1')   I=1
              IF(WQ.EQ.'A2')   I=2
              IF(WQ.EQ.'A3')   I=3
              IF(WQ.EQ.'A4')   I=4
              IF(WQ.EQ.'A5')   I=5
              IF(WQ.EQ.'A6')   I=6
              IF(WQ.EQ.'A7')   I=7
              IF(WQ.EQ.'A8')   I=8
              IF(WQ.EQ.'A9')   I=9
              IF(WQ.EQ.'A10')  I=10
              IF(WQ.EQ.'A11')  I=11
              IF(WQ.EQ.'A12')  I=12
              IF(WQ.EQ.'A13')  I=13
              IF(WQ.EQ.'A14')  I=14
              IF(WQ.EQ.'A15')  I=15
              IF(WQ.EQ.'A16')  I=16
              IF(WQ.EQ.'A17')  I=17
              IF(WQ.EQ.'A18')  I=18
              IF(WQ.EQ.'A19')  I=19
              IF(WQ.EQ.'A20')  I=20
              IF(WQ.EQ.'A21')  I=21
              IF(WQ.EQ.'A22')  I=22
              IF(WQ.EQ.'A23')  I=23
              IF(WQ.EQ.'A24')  I=24
              IF(WQ.EQ.'A25')  I=25
              IF(WQ.EQ.'A26')  I=26
              IF(WQ.EQ.'A27')  I=27
              IF(WQ.EQ.'A28')  I=28
              IF(WQ.EQ.'A29')  I=29
              IF(WQ.EQ.'A30')  I=30
              IF(WQ.EQ.'A31')  I=31
              IF(WQ.EQ.'A32')  I=32
              IF(WQ.EQ.'A33')  I=33
              IF(WQ.EQ.'A34')  I=34
              IF(WQ.EQ.'A35')  I=35
              IF(WQ.EQ.'A36')  I=36
              IF(WQ.EQ.'A37')  I=37
              IF(WQ.EQ.'A38')  I=38
              IF(WQ.EQ.'A39')  I=39
              IF(WQ.EQ.'A30')  I=40
              IF(WQ.EQ.'A41')  I=41
              IF(WQ.EQ.'A42')  I=42
              IF(WQ.EQ.'A43')  I=43
              IF(WQ.EQ.'A44')  I=44
              IF(WQ.EQ.'A45')  I=45
              IF(WQ.EQ.'A46')  I=46
              IF(WQ.EQ.'A47')  I=47
              IF(WQ.EQ.'A48')  I=48
              IF(WQ.EQ.'A49')  I=49
              IF(WQ.EQ.'A50')  I=50
              IF(WQ.EQ.'A51')  I=51
              IF(WQ.EQ.'A52')  I=52
              IF(WQ.EQ.'A53')  I=53
              IF(WQ.EQ.'A54')  I=54
              IF(WQ.EQ.'A55')  I=55
              IF(WQ.EQ.'A56')  I=56
              IF(WQ.EQ.'A57')  I=57
              IF(WQ.EQ.'A58')  I=58
              IF(WQ.EQ.'A59')  I=59
              IF(WQ.EQ.'A60')  I=60
              IF(WQ.EQ.'A61')  I=61
              IF(WQ.EQ.'A62')  I=62
              IF(WQ.EQ.'A63')  I=63
              IF(WQ.EQ.'A64')  I=64
              IF(WQ.EQ.'A65')  I=65
              IF(WQ.EQ.'A66')  I=66
              IF(WQ.EQ.'A67')  I=67
              IF(WQ.EQ.'A68')  I=68
              IF(WQ.EQ.'A69')  I=69
              IF(WQ.EQ.'A70')  I=70
              IF(WQ.EQ.'A71')  I=71
              IF(WQ.EQ.'A72')  I=72
              IF(WQ.EQ.'A73')  I=73
              IF(WQ.EQ.'A74')  I=74
              IF(WQ.EQ.'A75')  I=75
              IF(WQ.EQ.'A76')  I=76
              IF(WQ.EQ.'A77')  I=77
              IF(WQ.EQ.'A78')  I=78
              IF(WQ.EQ.'A79')  I=79
              IF(WQ.EQ.'A80')  I=80
              IF(WQ.EQ.'A81')  I=81
              IF(WQ.EQ.'A82')  I=82
              IF(WQ.EQ.'A83')  I=83
              IF(WQ.EQ.'A84')  I=84
              IF(WQ.EQ.'A85')  I=85
              IF(WQ.EQ.'A86')  I=86
              IF(WQ.EQ.'A87')  I=87
              IF(WQ.EQ.'A88')  I=88
              IF(WQ.EQ.'A89')  I=89
              IF(WQ.EQ.'A90')  I=90
              IF(WQ.EQ.'A91')  I=91
              IF(WQ.EQ.'A92')  I=92
              IF(WQ.EQ.'A93')  I=93
              IF(WQ.EQ.'A94')  I=94
              IF(WQ.EQ.'A95')  I=95
              IF(WQ.EQ.'A96')  I=96
              IF(WQ.EQ.'A97')  I=97
              IF(WQ.EQ.'A98')  I=98
              IF(WQ.EQ.'A99')  I=99
              IF(WQ.EQ.'A100') I=100
              IF(WQ.EQ.'A101')  I=101
              IF(WQ.EQ.'A102')  I=102
              IF(WQ.EQ.'A103')  I=103
              IF(WQ.EQ.'A104')  I=104
              IF(WQ.EQ.'A105')  I=105
              IF(WQ.EQ.'A106')  I=106
              IF(WQ.EQ.'A107')  I=107
              IF(WQ.EQ.'A108')  I=108
              IF(WQ.EQ.'A109')  I=109
              IF(WQ.EQ.'A110')  I=110
              IF(WQ.EQ.'A111')  I=111
              IF(WQ.EQ.'A112')  I=112
              IF(WQ.EQ.'A113')  I=113
              IF(WQ.EQ.'A114')  I=114
              IF(WQ.EQ.'A115')  I=115
              IF(WQ.EQ.'A116')  I=116
              IF(WQ.EQ.'A117')  I=117
              IF(WQ.EQ.'A118')  I=118
              IF(WQ.EQ.'A119')  I=119
              IF(WQ.EQ.'A120')  I=120
              IF(WQ.EQ.'A121')  I=121
              IF(WQ.EQ.'A122')  I=122
              IF(WQ.EQ.'A123')  I=123
              IF(WQ.EQ.'A124')  I=124
              IF(WQ.EQ.'A125')  I=125
              IF(WQ.EQ.'A126')  I=126
              IF(WQ.EQ.'A127')  I=127
              IF(WQ.EQ.'A128')  I=128
              IF(WQ.EQ.'A129')  I=129
              IF(WQ.EQ.'A130')  I=130
              IF(WQ.EQ.'A131')  I=131
              IF(WQ.EQ.'A132')  I=132
              IF(WQ.EQ.'A133')  I=133
              IF(WQ.EQ.'A134')  I=134
              IF(WQ.EQ.'A135')  I=135
              IF(WQ.EQ.'A136')  I=136
              IF(WQ.EQ.'A137')  I=137
              IF(WQ.EQ.'A138')  I=138
              IF(WQ.EQ.'A139')  I=139
              IF(WQ.EQ.'A140')  I=140
              IF(WQ.EQ.'A141')  I=141
              IF(WQ.EQ.'A142')  I=142
              IF(WQ.EQ.'A143')  I=143
              IF(WQ.EQ.'A144')  I=144
              IF(WQ.EQ.'A145')  I=145
              IF(WQ.EQ.'A146')  I=146
              IF(WQ.EQ.'A147')  I=147
              IF(WQ.EQ.'A148')  I=148
              IF(WQ.EQ.'A149')  I=149
              IF(WQ.EQ.'A150')  I=150
              IF(WQ.EQ.'A151')  I=151
              IF(WQ.EQ.'A152')  I=152
              IF(WQ.EQ.'A153')  I=153
              IF(WQ.EQ.'A154')  I=154
              IF(WQ.EQ.'A155')  I=155
              IF(WQ.EQ.'A156')  I=156
              IF(WQ.EQ.'A157')  I=157
              IF(WQ.EQ.'A158')  I=158
              IF(WQ.EQ.'A159')  I=159
              IF(WQ.EQ.'A160')  I=160
              IF(WQ.EQ.'A161')  I=161
              IF(WQ.EQ.'A162')  I=162
              IF(WQ.EQ.'A163')  I=163
              IF(WQ.EQ.'A164')  I=164
              IF(WQ.EQ.'A165')  I=165
              IF(WQ.EQ.'A166')  I=166
              IF(WQ.EQ.'A167')  I=167
              IF(WQ.EQ.'A168')  I=168
              IF(WQ.EQ.'A169')  I=169
              IF(WQ.EQ.'A170')  I=170
              IF(WQ.EQ.'A171')  I=171
              IF(WQ.EQ.'A172')  I=172
              IF(WQ.EQ.'A173')  I=173
              IF(WQ.EQ.'A174')  I=174
              IF(WQ.EQ.'A175')  I=175
              IF(WQ.EQ.'A176')  I=176
              IF(WQ.EQ.'A177')  I=177
              IF(WQ.EQ.'A178')  I=178
              IF(WQ.EQ.'A179')  I=179
              IF(WQ.EQ.'A180')  I=180
              IF(WQ.EQ.'A181')  I=181
              IF(WQ.EQ.'A182')  I=182
              IF(WQ.EQ.'A183')  I=183
              IF(WQ.EQ.'A184')  I=184
              IF(WQ.EQ.'A185')  I=185
              IF(WQ.EQ.'A186')  I=186
              IF(WQ.EQ.'A187')  I=187
              IF(WQ.EQ.'A188')  I=188
              IF(WQ.EQ.'A189')  I=189
              IF(WQ.EQ.'A190')  I=190
              IF(WQ.EQ.'A191')  I=191
              IF(WQ.EQ.'A192')  I=192
              IF(WQ.EQ.'A193')  I=193
              IF(WQ.EQ.'A194')  I=194
              IF(WQ.EQ.'A195')  I=195
              IF(WQ.EQ.'A196')  I=196
              IF(WQ.EQ.'A197')  I=197
              IF(WQ.EQ.'A198')  I=198
              IF(WQ.EQ.'A199')  I=199
              IF(WQ.EQ.'A200')  I=200
              IF(WQ.EQ.'A201')  I=201
              IF(WQ.EQ.'A202')  I=202
              IF(WQ.EQ.'A203')  I=203
              IF(WQ.EQ.'A204')  I=204
              IF(WQ.EQ.'A205')  I=205
              IF(WQ.EQ.'A206')  I=206
              IF(WQ.EQ.'A207')  I=207
              IF(WQ.EQ.'A208')  I=208
              IF(WQ.EQ.'A209')  I=209
              IF(WQ.EQ.'A210')  I=210
              IF(WQ.EQ.'A211')  I=211
              IF(WQ.EQ.'A212')  I=212
              IF(WQ.EQ.'A213')  I=213
              IF(WQ.EQ.'A214')  I=214
              IF(WQ.EQ.'A215')  I=215
              IF(WQ.EQ.'A216')  I=216
              IF(WQ.EQ.'A217')  I=217
              IF(WQ.EQ.'A218')  I=218
              IF(WQ.EQ.'A219')  I=219
              IF(WQ.EQ.'A220')  I=220
              IF(WQ.EQ.'A221')  I=221
              IF(WQ.EQ.'A222')  I=222
              IF(WQ.EQ.'A223')  I=223
              IF(WQ.EQ.'A224')  I=224
              IF(WQ.EQ.'A225')  I=225
              IF(WQ.EQ.'A226')  I=226
              IF(WQ.EQ.'A227')  I=227
              IF(WQ.EQ.'A228')  I=228
              IF(WQ.EQ.'A229')  I=229
              IF(WQ.EQ.'A230')  I=230
              IF(WQ.EQ.'A231')  I=231
              IF(WQ.EQ.'A232')  I=232
              IF(WQ.EQ.'A233')  I=233
              IF(WQ.EQ.'A234')  I=234
              IF(WQ.EQ.'A235')  I=235
              IF(WQ.EQ.'A236')  I=236
              IF(WQ.EQ.'A237')  I=237
              IF(WQ.EQ.'A238')  I=238
              IF(WQ.EQ.'A239')  I=239
              IF(WQ.EQ.'A240')  I=240
              IF(WQ.EQ.'A241')  I=241
              IF(WQ.EQ.'A242')  I=242
              IF(WQ.EQ.'A243')  I=243
              IF(WQ.EQ.'A244')  I=244
              IF(WQ.EQ.'A245')  I=245
              IF(WQ.EQ.'A246')  I=246
              IF(WQ.EQ.'A247')  I=247
              IF(WQ.EQ.'A248')  I=248
              IF(WQ.EQ.'A249')  I=249
              IF(WQ.EQ.'A250')  I=250
              IF(WQ.EQ.'A251')  I=251
              IF(WQ.EQ.'A252')  I=252
              IF(WQ.EQ.'A253')  I=253
              IF(WQ.EQ.'A254')  I=254
              IF(WQ.EQ.'A255')  I=255
              IF(WQ.EQ.'A256')  I=256
              IF(WQ.EQ.'A257')  I=257
              IF(WQ.EQ.'A258')  I=258
              IF(WQ.EQ.'A259')  I=259
              IF(WQ.EQ.'A260')  I=260
              IF(WQ.EQ.'A261')  I=261
              IF(WQ.EQ.'A262')  I=262
              IF(WQ.EQ.'A263')  I=263
              IF(WQ.EQ.'A264')  I=264
              IF(WQ.EQ.'A265')  I=265
              IF(WQ.EQ.'A266')  I=266
              IF(WQ.EQ.'A267')  I=267
              IF(WQ.EQ.'A268')  I=268
              IF(WQ.EQ.'A269')  I=269
              IF(WQ.EQ.'A270')  I=270
              IF(WQ.EQ.'A271')  I=271
              IF(WQ.EQ.'A272')  I=272
              IF(WQ.EQ.'A273')  I=273
              IF(WQ.EQ.'A274')  I=274
              IF(WQ.EQ.'A275')  I=275
              IF(WQ.EQ.'A276')  I=276
              IF(WQ.EQ.'A277')  I=277
              IF(WQ.EQ.'A278')  I=278
              IF(WQ.EQ.'A279')  I=279
              IF(WQ.EQ.'A280')  I=280
              IF(WQ.EQ.'A281')  I=281
              IF(WQ.EQ.'A282')  I=282
              IF(WQ.EQ.'A283')  I=283
              IF(WQ.EQ.'A284')  I=284
              IF(WQ.EQ.'A285')  I=285
              IF(WQ.EQ.'A286')  I=286
              IF(WQ.EQ.'A287')  I=287
              IF(WQ.EQ.'A288')  I=288
              IF(WQ.EQ.'A289')  I=289
              IF(WQ.EQ.'A290')  I=290
              IF(WQ.EQ.'A291')  I=291
              IF(WQ.EQ.'A292')  I=292
              IF(WQ.EQ.'A293')  I=293
              IF(WQ.EQ.'A294')  I=294
              IF(WQ.EQ.'A295')  I=295
              IF(WQ.EQ.'A296')  I=296
              IF(WQ.EQ.'A297')  I=297
              IF(WQ.EQ.'A298')  I=298
              IF(WQ.EQ.'A299')  I=299
              IF(WQ.EQ.'A300')  I=300
              IF(WQ.EQ.'A301')  I=301
              IF(WQ.EQ.'A302')  I=302
              IF(WQ.EQ.'A303')  I=303
              IF(WQ.EQ.'A304')  I=304
              IF(WQ.EQ.'A305')  I=305
              IF(WQ.EQ.'A306')  I=306
              IF(WQ.EQ.'A307')  I=307
              IF(WQ.EQ.'A308')  I=308
              IF(WQ.EQ.'A309')  I=309
              IF(WQ.EQ.'A310')  I=310
              IF(WQ.EQ.'A311')  I=311
              IF(WQ.EQ.'A312')  I=312
              IF(WQ.EQ.'A313')  I=313
              IF(WQ.EQ.'A314')  I=314
              IF(WQ.EQ.'A315')  I=315
              IF(WQ.EQ.'A316')  I=316
              IF(WQ.EQ.'A317')  I=317
              IF(WQ.EQ.'A318')  I=318
              IF(WQ.EQ.'A319')  I=319
              IF(WQ.EQ.'A320')  I=320
              IF(WQ.EQ.'A321')  I=221
              IF(WQ.EQ.'A322')  I=322
              IF(WQ.EQ.'A323')  I=223
              IF(WQ.EQ.'A324')  I=324
              IF(WQ.EQ.'A325')  I=325
              IF(WQ.EQ.'A326')  I=326
              IF(WQ.EQ.'A327')  I=327
              IF(WQ.EQ.'A328')  I=328
              IF(WQ.EQ.'A329')  I=329
              IF(WQ.EQ.'A330')  I=330
              IF(WQ.EQ.'A331')  I=331
              IF(WQ.EQ.'A332')  I=332
              IF(WQ.EQ.'A333')  I=333
              IF(WQ.EQ.'A334')  I=334
              IF(WQ.EQ.'A335')  I=335
              IF(WQ.EQ.'A336')  I=336
              IF(WQ.EQ.'A337')  I=337
              IF(WQ.EQ.'A338')  I=338
              IF(WQ.EQ.'A339')  I=339
              IF(WQ.EQ.'A340')  I=340
              IF(WQ.EQ.'A341')  I=341
              IF(WQ.EQ.'A342')  I=342
              IF(WQ.EQ.'A343')  I=343
              IF(WQ.EQ.'A344')  I=344
              IF(WQ.EQ.'A345')  I=345
              IF(WQ.EQ.'A346')  I=346
              IF(WQ.EQ.'A347')  I=347
              IF(WQ.EQ.'A348')  I=348
              IF(WQ.EQ.'A349')  I=349
              IF(WQ.EQ.'A350')  I=350
              IF(WQ.EQ.'A351')  I=351
              IF(WQ.EQ.'A352')  I=352
              IF(WQ.EQ.'A353')  I=353
              IF(WQ.EQ.'A354')  I=354
              IF(WQ.EQ.'A355')  I=355
              IF(WQ.EQ.'A356')  I=356
              IF(WQ.EQ.'A357')  I=357
              IF(WQ.EQ.'A358')  I=358
              IF(WQ.EQ.'A359')  I=359
              IF(WQ.EQ.'A360')  I=360
              IF(WQ.EQ.'A361')  I=361
              IF(WQ.EQ.'A362')  I=362
              IF(WQ.EQ.'A363')  I=363
              IF(WQ.EQ.'A364')  I=364
              IF(WQ.EQ.'A365')  I=365
              IF(WQ.EQ.'A366')  I=366
              IF(WQ.EQ.'A367')  I=367
              IF(WQ.EQ.'A368')  I=368
              IF(WQ.EQ.'A369')  I=369
              IF(WQ.EQ.'A370')  I=370
              IF(WQ.EQ.'A371')  I=371
              IF(WQ.EQ.'A372')  I=372
              IF(WQ.EQ.'A373')  I=373
              IF(WQ.EQ.'A374')  I=374
              IF(WQ.EQ.'A375')  I=375
              IF(WQ.EQ.'A376')  I=376
              IF(WQ.EQ.'A377')  I=377
              IF(WQ.EQ.'A378')  I=378
              IF(WQ.EQ.'A379')  I=379
              IF(WQ.EQ.'A380')  I=380
              IF(WQ.EQ.'A381')  I=381
              IF(WQ.EQ.'A382')  I=382
              IF(WQ.EQ.'A383')  I=383
              IF(WQ.EQ.'A384')  I=384
              IF(WQ.EQ.'A385')  I=385
              IF(WQ.EQ.'A386')  I=386
              IF(WQ.EQ.'A387')  I=387
              IF(WQ.EQ.'A388')  I=388
              IF(WQ.EQ.'A389')  I=389
              IF(WQ.EQ.'A390')  I=390
              IF(WQ.EQ.'A391')  I=391
              IF(WQ.EQ.'A392')  I=392
              IF(WQ.EQ.'A393')  I=393
              IF(WQ.EQ.'A394')  I=394
              IF(WQ.EQ.'A395')  I=395
              IF(WQ.EQ.'A396')  I=396
              IF(WQ.EQ.'A397')  I=397
              IF(WQ.EQ.'A398')  I=398
              IF(WQ.EQ.'A399')  I=399
              IF(WQ.EQ.'A400')  I=400
C     CHECK CONTENTS OF ALPHA REGISTER
              DO J=80,1,-1
                  IF(AGPREG(I)(J:J).NE.' ') THEN
                      NVNVAL=J
                      GO TO 21
                  END IF
              END DO
 21           CONTINUE
              IF(NVNVAL.EQ.0) SQ=0
              IF(NVNVAL.GT.0.0D0) LLAB(1:NVNVAL)=AGPREG(I)(1:NVNVAL)
          END IF
          DO I=1,5
              IF(I.EQ.1.AND.INT(W1).LT.1.AND.DF1.EQ.0
     1        .OR.I.EQ.1.AND.INT(W1).GT.400..AND.DF1.EQ.0.OR.
     1        I.EQ.2.AND.INT(W2).LT.1.AND.DF2.EQ.0
     1        .OR.I.EQ.1.AND.INT(W1).GT.400.AND.DF2.EQ.0.OR.
     1        I.EQ.3.AND.INT(W3).LT.1.AND.DF3.EQ.0
     1        .OR.I.EQ.3.AND.INT(W1).GT.400.AND.DF2.EQ.0.OR.
     1        I.EQ.4.AND.INT(W4).LT.1.AND.DF4.EQ.0
     1        .OR.I.EQ.4.AND.INT(W1).GT.400.AND.DF2.EQ.0.OR.
     1        I.EQ.5.AND.INT(W5).LT.1.AND.DF5.EQ.0
     1        .OR.I.EQ.5.AND.INT(W1).GT.400.AND.DF2.EQ.0) THEN
                  WRITE(OUTLYNE,*) 'NUMERIC STORAGE REGISTED DOES NOT EXIST'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*) 'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END DO
C
          J=0
          DO I=1,5
              IF(I.EQ.1.AND.DF1.EQ.0) THEN
                  J=J+1
                  V(J)=GPREG(INT(W1))
              END IF
              IF(I.EQ.2.AND.DF2.EQ.0) THEN
                  J=J+1
                  V(J)=GPREG(INT(W2))
              END IF
              IF(I.EQ.3.AND.DF3.EQ.0) THEN
                  J=J+1
                  V(J)=GPREG(INT(W3))
              END IF
              IF(I.EQ.4.AND.DF4.EQ.0) THEN
                  J=J+1
                  V(J)=GPREG(INT(W4))
              END IF
              IF(I.EQ.5.AND.DF5.EQ.0) THEN
                  J=J+1
                  V(J)=GPREG(INT(W5))
              END IF
          END DO
          DO I=80,1,-1
              IF(LFORM(I:I).NE.' ') THEN
                  LL=I
                  GO TO 96
              END IF
          END DO
 96       CONTINUE
          LFORM1(1:LL+2)='('//LFORM(1:LL)//')'
          IF(SQ.EQ.0) THEN
              WRITE(UNIT=CSTRING,FMT=LFORM1(1:LL+2),ERR=69) (V(K), K=1,J)
              NVNVALL=132
              DO II=132,1,-1
                  IF(CSTRING(II:II).NE.' ') THEN
                      NVNVALL=II
                      GO TO 93
                  END IF
              END DO
 93           CONTINUE
              IF(NVNVALL.LE.79) WRITE(OUTLYNE,1100) CSTRING(1:79)
              IF(NVNVALL.GT.79) WRITE(OUTLYNE,1250) CSTRING(1:131)
              CALL SHOWIT(0)
 1100         FORMAT(A79)
 1250         FORMAT(A131)
 1200         FORMAT(A149)
              RETURN
          END IF
          IF(SQ.EQ.1) THEN
              WRITE(UNIT=CSTRING,FMT=LFORM1(1:LL+2),ERR=69) (V(K), K=1,J)
              NVNVALL=132
              DO II=132,1,-1
                  IF(CSTRING(II:II).NE.' ') THEN
                      NVNVALL=II
                      GO TO 94
                  END IF
              END DO
 94           CONTINUE
C
              WSSA(1:150)=LLAB(1:NVNVAL)//' '//CSTRING(1:NVNVALL)
              NVNVALL=NVNVAL+NVNVALL+1
              IF(NVNVALL.LE.79) WRITE(OUTLYNE,1100) WSSA(1:79)
              IF(NVNVALL.GT.79) WRITE(OUTLYNE,1200) WSSA(1:149)
              CALL SHOWIT(0)
          END IF
          RETURN
   69     CONTINUE
          WRITE(OUTLYNE,*)
     1    'LINE FORMAT DOES NOT AGREE WITH OUTPUT LIST'
          CALL SHOWIT(1)
          WRITE(OUTLYNE,*)
     1    'RE-ISSUE THE "LFORMAT" OR THE "LWRITE" COMMAND'
          CALL SHOWIT(1)
          CALL MACFAL
          RETURN
      END


C SUB LFORMER.FOR
      SUBROUTINE LFORMER
C
          IMPLICIT NONE
C
C     THIS DOES THE "LFORMAT" COMMAND
C
          INCLUDE 'datmai.inc'
C
          IF(STI.EQ.1) THEN
              WRITE(OUTLYNE,100)
              CALL SHOWIT(0)
 100          FORMAT('THE CURRENT LINE FORMAT IS : ')
              WRITE(OUTLYNE,200) LFORM
              CALL SHOWIT(0)
 200          FORMAT(A79)
              RETURN
          END IF
          IF(SQ.EQ.1.OR.S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
              WRITE(OUTLYNE,*)
     1        WC,' ONLY TAKES STRING OR NUMERIC WORD #1 INPUT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SST.EQ.1.AND.S1.EQ.1) THEN
              WRITE(OUTLYNE,*) WC,' TAKES STRING OR NUMERIC WORD #1 INPUT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*) 'BUT NOT BOTH'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(S1.EQ.1.AND.INT(W1).LT.1.OR.S1.EQ.1
     1    .AND.INT(W1).GT.400) THEN
              WRITE(OUTLYNE,*) 'ALPHA NUMERIC STORAGE REGISTED DOES NOT EXIST'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*) 'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SST.EQ.1) THEN
              LFORM=' '
              LFORM=WS
              RETURN
          END IF
          IF(S1.EQ.1) THEN
              LFORM=' '
              LFORM=AGPREG(INT(W1))
              RETURN
          END IF
          RETURN
      END


C SUB MYSYS.FOR
          SUBROUTINE MYSYS

          USE opsys
          USE strings  
            
          IMPLICIT NONE
          CHARACTER WWSS*80
C
          INTEGER N,I
C
          INCLUDE 'datmai.inc'
C
          IF(WC(1:1).EQ.'S') THEN
              IF(STI.EQ.1) THEN
                  OUTLYNE=
     1            '"SYS(TEM)" IS USED EXECUTE AN OPERATING SYSTEM COMMAND'
                  CALL SHOWIT(1)
                  RETURN
              ELSE
              END IF
              IF(SN.EQ.1.OR.SQ.EQ.1) THEN
                  OUTLYNE='"SYS(TEM)" ONLY TAKES STRING INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              ELSE
              END IF
              N=1
              CALL to_lower(WS)
              DO I=80,1,-1
                  IF(WS(I:I).NE.' ') THEN
                      N=I
                      GO TO 10
                  ELSE
                  END IF
              END DO
 10           CONTINUE
              DO I=1,80
                  WWSS(I:I)=' '
              END DO
              WWSS(1:N)=WS(1:N)
              CALL shell_command( WWSS )
          END IF
           
          RETURN
      END


C SUB NTOA2.FOR
      SUBROUTINE NTOA2(N1,N2,N3,N4,N5,BN1,BN2,BN3,BN4,BN5)
C
          IMPLICIT NONE
C
C       THIS SUBROUTINE USES WRITE AND READS TO AN INTERNAL
C       FILE TO CONVERT REAL*8 NUMERIC WORDS
C       TO CHARACTER VARIABLES
C
          REAL*8 N1,N2,N3,N4,N5,N
C
          CHARACTER B*140,BN*10,BN1*10,BN2*10,BN3*10,BN4*10,BN5*10
C
          INTEGER I
C
          DO 10 I=1,5
              IF(I.EQ.1) N=N1
              IF(I.EQ.2) N=N2
              IF(I.EQ.3) N=N3
              IF(I.EQ.4) N=N4
              IF(I.EQ.5) N=N5
              IF(DABS(N).GE.10000.0D0.OR.DABS(N).LT.0.0001D0) THEN
                  WRITE(B,100) N
                  READ(B,300) BN
                  IF(I.EQ.1) BN1=BN
                  IF(I.EQ.2) BN2=BN
                  IF(I.EQ.3) BN3=BN
                  IF(I.EQ.4) BN4=BN
                  IF(I.EQ.5) BN5=BN
              ELSE
              END IF
              IF(DABS(N).GE.1000.0D0.AND.DABS(N).LT.10000.0D0.OR.
     1        DABS(N).LE.0.001D0.AND.DABS(N).GT.0.0001D0) THEN
                  WRITE(B,180) N
                  READ(B,300) BN
                  IF(I.EQ.1) BN1=BN
                  IF(I.EQ.2) BN2=BN
                  IF(I.EQ.3) BN3=BN
                  IF(I.EQ.4) BN4=BN
                  IF(I.EQ.5) BN5=BN
              ELSE
              END IF
              IF(DABS(N).GE.100.0D0.AND.DABS(N).LT.1000.0D0.OR.
     1        DABS(N).LE.0.01D0.AND.DABS(N).GT.0.001D0) THEN
                  WRITE(B,170) N
                  READ(B,300) BN
                  IF(I.EQ.1) BN1=BN
                  IF(I.EQ.2) BN2=BN
                  IF(I.EQ.3) BN3=BN
                  IF(I.EQ.4) BN4=BN
                  IF(I.EQ.5) BN5=BN
              ELSE
              END IF
              IF(DABS(N).GE.10.0D0.AND.DABS(N).LT.100.0D0.OR.
     1        DABS(N).LE.0.1D0.AND.DABS(N).GT.0.01D0) THEN
                  WRITE(B,160) N
                  READ(B,300) BN
                  IF(I.EQ.1) BN1=BN
                  IF(I.EQ.2) BN2=BN
                  IF(I.EQ.3) BN3=BN
                  IF(I.EQ.4) BN4=BN
                  IF(I.EQ.5) BN5=BN
              ELSE
              END IF
              IF(DABS(N).GE.1.0D0.AND.DABS(N).LT.10.0D0.OR.
     1        DABS(N).LE.1.0D0.AND.DABS(N).GT.0.1D0) THEN
                  WRITE(B,150) N
                  READ(B,300) BN
                  IF(I.EQ.1) BN1=BN
                  IF(I.EQ.2) BN2=BN
                  IF(I.EQ.3) BN3=BN
                  IF(I.EQ.4) BN4=BN
                  IF(I.EQ.5) BN5=BN
              ELSE
              END IF
 10       CONTINUE
 100      FORMAT(D10.3)
 150      FORMAT(F10.7)
 160      FORMAT(F10.6)
 170      FORMAT(F10.5)
 180      FORMAT(F10.4)
 300      FORMAT(A10)
          RETURN
      END


C SUB NWTOAW.FOR
      SUBROUTINE NWTOAW(N1,N2,N3,N4,N5,AN1,AN2,AN3,AN4,AN5
     1,BN1,BN2,BN3,BN4,BN5)
C
          IMPLICIT NONE
C
C       THIS SUBROUTINE USES WRITE AND READS TO AN INTERNAL
C       FILE TO CONVERT REAL*8 NUMERIC WORDS
C       TO CHARACTER VARIABLES
C
          REAL*8 N1,N2,N3,N4,N5,N
C
          CHARACTER B*140,AN1*23,AN2*23,AN3*23,AN4*23,AN5*23,AN*23
     1    ,C*140,BN*11,BN1*11,BN2*11,BN3*11,BN4*11,BN5*11
C
          INTEGER I
          include 'datmai.inc'
C
          DO 10 I=1,5
              IF(I.EQ.1) N=N1
              IF(I.EQ.2) N=N2
              IF(I.EQ.3) N=N3
              IF(I.EQ.4) N=N4
              IF(I.EQ.5) N=N5
              WRITE(B,100) N
              WRITE(C,250) N
              READ(B,200) AN
              READ(C,300) BN
              IF(N.EQ.0.0D0) AN='                    0.0'
              IF(N.EQ.0.0D0) BN='        0.0'
              IF(I.EQ.1) AN1=AN
              IF(I.EQ.2) AN2=AN
              IF(I.EQ.3) AN3=AN
              IF(I.EQ.4) AN4=AN
              IF(I.EQ.5) AN5=AN
              IF(I.EQ.1) BN1=BN
              IF(I.EQ.2) BN2=BN
              IF(I.EQ.3) BN3=BN
              IF(I.EQ.4) BN4=BN
              IF(I.EQ.5) BN5=BN
 10       CONTINUE
 100      FORMAT(G23.15)
 200      FORMAT(A23)
 250      FORMAT(G11.4)
 300      FORMAT(A11)
          RETURN
      END


C SUB NWTOAWB.FOR
      SUBROUTINE NWTOAWB(N1,N2,N3,N4,N5,AN1,AN2,AN3,AN4,AN5
     1,BN1,BN2,BN3,BN4,BN5)
C
          IMPLICIT NONE
C
C       THIS SUBROUTINE USES WRITE AND READS TO AN INTERNAL
C       FILE TO CONVERT REAL*8 NUMERIC WORDS
C       TO CHARACTER VARIABLES
C
          REAL*8 N1,N2,N3,N4,N5,N
C
          CHARACTER B*140,AN1*23,AN2*23,AN3*23,AN4*23,AN5*23,AN*23
     1    ,C*140,BN*11,BN1*11,BN2*11,BN3*11,BN4*11,BN5*11
C
          INTEGER I
          INCLUDE 'datmai.inc'
C
          DO 10 I=1,5
              IF(I.EQ.1) N=N1
              IF(I.EQ.2) N=N2
              IF(I.EQ.3) N=N3
              IF(I.EQ.4) N=N4
              IF(I.EQ.5) N=N5
              WRITE(B,100) N
              WRITE(C,250) N
              READ(B,200) AN
              READ(C,300) BN
              IF(I.EQ.1) AN1=AN
              IF(I.EQ.2) AN2=AN
              IF(I.EQ.3) AN3=AN
              IF(I.EQ.4) AN4=AN
              IF(I.EQ.5) AN5=AN
              IF(I.EQ.1) BN1=BN
              IF(I.EQ.2) BN2=BN
              IF(I.EQ.3) BN3=BN
              IF(I.EQ.4) BN4=BN
              IF(I.EQ.5) BN5=BN
 10       CONTINUE
 100      FORMAT(D23.15)
 200      FORMAT(A23)
 250      FORMAT(D11.4)
 300      FORMAT(A11)
          RETURN
      END


C SUB ALINE1.FOR
      SUBROUTINE ALINE1(I,LINE1,ALN1)
C
          IMPLICIT NONE
C
C       THIS SUBROUTINE USES WRITE AND READS TO AN INTERNAL
C       FILE TO CONVERT REAL*8 NUMERIC WORDS
C       TO CHARACTER VARIABLES
C
          CHARACTER A*3,AI*3,LINE1*80,ALN1*80
C
          INTEGER I
C
          WRITE(A,100) I
          READ(A,300) AI
          ALN1=AI//LINE1(1:77)
 100      FORMAT(I3)
 300      FORMAT(A3)
          RETURN
      END
C SUB MESCOM.FOR
      SUBROUTINE MESCOM
C
          IMPLICIT NONE
C
          INCLUDE 'datmai.inc'
C
          IF(WC.EQ.'M') THEN
C
              IF(SQ.EQ.1.OR.SN.EQ.1) THEN
                  OUTLYNE='"M" TAKES ONLY STRING INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C
C       'M'- CAUSES THE MESSAGE IN THE STRING TO BE PRINTED TO THE
C       DEFAULT OUTPUT DEVICE. REMEMBER THE STRING BEGINS WITH A COLON.
C
C       EXAMPLE:
C
C       M,:THE NEXT INSTRUCTION CALCULATES POINT SPREAD FUNCTION.
C
C       M ,1 PRINTS MESSAGE STORED IN THE AGPREG(1) REGISTER
C
C       PRODUCES AT THE CURRENT OUTPUT DEVICE:
C
C               THE NEXT INSTRUCTION CALCULATES POINT SPREAD FUNCTION.
              WRITE(OUTLYNE,1000) WS
              CALL SHOWIT(0)
 1000         FORMAT(A79)
              RETURN
          ELSE
C       NOT MESAGE
          END IF

          IF(WC.EQ.'C') THEN
              IF(SQ.EQ.1.OR.SN.EQ.1) THEN
                  OUTLYNE='"C" TAKES ONLY STRING INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C
C       BLANK INPUT EQUATES TO :(BLANK)
C
C       'C'- THE COMMENT DOESN'T DO ANYTHING. IT JUST ALLOWS
C       ANNOTATION OF INPUT  OR ANNOTATION
C       OF MACROS. IT WORKS LIKE THE COMMENT LINE IN FORTRAN.
C
              RETURN
          ELSE
C       NOT COMENT
          END IF
      END

      
C SUB FIGURE.FOR
      SUBROUTINE FIGURE
C
          IMPLICIT NONE
C
          INCLUDE 'datmai.inc'
C
          IF(STI.EQ.1) THEN
              OUTLYNE='THE CURRENT FIGURE HEADING IS:'
              CALL SHOWIT(1)
              OUTLYNE=FIGTITLE
              CALL SHOWIT(1)
              RETURN
          END IF
          IF(SQ.EQ.1.OR.SN.EQ.1) THEN
              OUTLYNE='"FIGURE" TAKES ONLY STRING INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          FIGTITLE=WS
          RETURN
      END

      
C SUB VECTOROP.FOR
      SUBROUTINE VECTOROP
C
          IMPLICIT NONE
C
          DOUBLE PRECISION AX,AY,AZ,BX,BY,BZ,DP,PX,PY,PZ
C
          INCLUDE 'datmai.inc'

C
          IF(STI.EQ.1.OR.DF1.EQ.1.AND.DF2.EQ.1.AND.DF3.EQ.1) THEN
              IF(WC.EQ.'AVEC') THEN
                  OUTLYNE='CURRENT AVEC IS:'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*) 'X=',AXVEC
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*) 'Y=',AYVEC
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*) 'Z=',AZVEC
                  CALL SHOWIT(1)
                  RETURN
              END IF
              IF(WC.EQ.'BVEC') THEN
                  OUTLYNE='CURRENT BVEC IS:'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*) 'X=',BXVEC
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*) 'Y=',BYVEC
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*) 'Z=',BZVEC
                  CALL SHOWIT(1)
                  RETURN
              END IF
          END IF
          IF(STI.EQ.1) THEN
              IF(WC.EQ.'DOT') THEN
                  OUTLYNE='DOT RETURNS THE DOT PRODUCT OF AVEC AND BVEC'
                  CALL SHOWIT(1)
                  RETURN
              END IF
              IF(WC.EQ.'CROSS') THEN
                  OUTLYNE='DOT RETURNS THE CROSS PRODUCT OF AVEC AND BVEC'
                  CALL SHOWIT(1)
                  RETURN
              END IF
          END IF
          IF(WC.EQ.'DOT') THEN
              IF(SQ.EQ.1.OR.SN.EQ.1.OR.SST.EQ.1) THEN
                  WRITE(OUTLYNE,*) '"DOT" TAKES NO ADDITIONAL INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
          IF(WC.EQ.'CROSS') THEN
              IF(SQ.EQ.1.OR.SN.EQ.1.OR.SST.EQ.1) THEN
                  WRITE(OUTLYNE,*) '"CROSS" TAKES NO ADDITIONAL INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
          IF(WC.EQ.'AVEC') THEN
              IF(S4.EQ.1.OR.S5.EQ.1.OR.SST.EQ.1.OR.SQ.EQ.1) THEN
                  WRITE(OUTLYNE,*)
     1            '"AVEC" ONLY TAKES NUMERIC WORD #1, #2 AND #3 INPUT'
                  CALL SHOWIT(1)
                  CALL MACFAL
              END IF
              AXVEC=W1
              AYVEC=W2
              AZVEC=W3
              IF(DF1.EQ.1) AXVEC=0.0D0
              IF(DF2.EQ.1) AYVEC=0.0D0
              IF(DF3.EQ.1) AZVEC=0.0D0
          END IF
          IF(WC.EQ.'BVEC') THEN
              IF(S4.EQ.1.OR.S5.EQ.1.OR.SST.EQ.1.OR.SQ.EQ.1) THEN
                  WRITE(OUTLYNE,*)
     1            '"BVEC" ONLY TAKES NUMERIC WORD #1, #2 AND #3 INPUT'
                  CALL SHOWIT(1)
                  CALL MACFAL
              END IF
              BXVEC=W1
              BYVEC=W2
              BZVEC=W3
              IF(DF1.EQ.1) BXVEC=0.0D0
              IF(DF2.EQ.1) BYVEC=0.0D0
              IF(DF3.EQ.1) BZVEC=0.0D0
          END IF
          AX=AXVEC
          AY=AYVEC
          AZ=AZVEC
          BX=BXVEC
          BY=BYVEC
          BZ=BZVEC
          IF(WC.EQ.'DOT') THEN
              CALL DOT_PRODUCT(DP,AX,AY,AZ,BX,BY,BZ)
              REG(40)=REG(9)
              REG(9)=DP
              WRITE(OUTLYNE,*) 'DOT PRODUCT = ',DP
              CALL SHOWIT(1)
              RETURN
          END IF
          IF(WC.EQ.'CROSS') THEN
              CALL CROSS_PRODUCT(PX,PY,PZ,AX,AY,AZ,BX,BY,BZ)
              REG(40)=REG(9)
              REG(11)=PX
              REG(10)=PY
              REG(9)=PZ
              WRITE(OUTLYNE,*) 'CROSS PRODUCT (X-COMPONENT) = ',PX
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*) 'CROSS PRODUCT (Y-COMPONENT) = ',PY
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*) 'CROSS PRODUCT (Z-COMPONENT) = ',PZ
              CALL SHOWIT(1)
              RETURN
          END IF
          RETURN
      END


C SUB LINE_TO_PLANE
      SUBROUTINE LINE_TO_PLANE
     1(X,Y,Z,X0,Y0,Z0,L,M,N,XP,YP,ZP,LP,MP,NP,ERROR)
C
C       RETURNS THE X,Y,Z LOCATION WHEN VECTOR X,Y,Z,L,M,N (STARTING
C       AT X0,Y0,Z0 INTERSECTS THE PLANE WHOSE NORMAL VECTOR IS IN THE
C       LP,MP,NP DIRECTION, THE NORMAL PASSING THROUGH POINT XP,YP,ZP
C       ERROR SET TO TRUE IS NO INTERSECTION IS POSSIBLE
C
          IMPLICIT NONE
C
          REAL*8 X,Y,Z,L,M,N,X0,Y0,Z0
C
          REAL*8 XP,YP,ZP,LP,MP,NP,NU,NOM,DENOM
C
          LOGICAL ERROR
C
          ERROR=.FALSE.
C
          NOM=((X0-XP)*LP)+((Y0-YP)*MP)+((Z0-ZP)*NP)
          DENOM=(L*LP)+(M*MP)+(N*NP)
          IF(DENOM.EQ.0.0D0) THEN
              ERROR=.TRUE.
              RETURN
          ELSE
              NU=NOM/DENOM
              X=X0-(NU*L)
              Y=Y0-(NU*M)
              Z=Z0-(NU*N)
          END IF
          RETURN
      END
