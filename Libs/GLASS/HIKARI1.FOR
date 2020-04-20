        PROGRAM MAIN
C
        IMPLICIT NONE
C
        CHARACTER NAME(1:5000)*13,NUMBER1(1:5000)*13,NUMBER2(1:5000)*13
     1  ,DUMMY*20,N*20
C
        INTEGER I,J,JJ
C
        DOUBLE PRECISION A0(1:5000),A1(1:5000),
     1  A2(1:5000),A3(1:5000),A4(1:5000),A5(1:5000)
C
        OPEN(UNIT=10,ACCESS='SEQUENTIAL',FILE='HIKARI.DAT',
     1  FORM='FORMATTED',STATUS='UNKNOWN')
        REWIND (UNIT=10)
                J=1
 50             READ(10,105) NUMBER(J)
C
        IF(NUMBER(J)(1:3).EQ.'EOS') THEN
                JJ=J-1
                GO TO 5000
                ELSE
                END IF
                READ(10,100) DUMMY
                READ(10,105) NAME(J)
                PRINT*,NAME(J),J
                READ(10,100) N
        IF(N(1:1).EQ.'N') THEN
                A0(J)=0.0D0
                ELSE
        BACKSPACE(10)
        READ(10,120) A0(J)
 120    FORMAT(D20.10)
                END IF
                READ(10,100) N
        IF(N(1:1).EQ.'N') THEN
                A1(J)=0.0D0
                ELSE
        BACKSPACE(10)
        READ(10,120) A1(J)
                END IF
                READ(10,100) N
        IF(N(1:1).EQ.'N') THEN
                A2(J)=0.0D0
                ELSE
        BACKSPACE(10)
        READ(10,120) A2(J)
                END IF
                READ(10,100) N
        IF(N(1:1).EQ.'N') THEN
                A3(J)=0.0D0
                ELSE
        BACKSPACE(10)
        READ(10,120) A3(J)
                END IF
                READ(10,100) N
        IF(N(1:1).EQ.'N') THEN
                A4(J)=0.0D0
                ELSE
        BACKSPACE(10)
        READ(10,120) A4(J)
                END IF
                READ(10,100) N
        IF(N(1:1).EQ.'N') THEN
                A5(J)=0.0D0
                ELSE
        BACKSPACE(10)
        READ(10,120) A5(J)
                END IF
                DO 200 I=10,148
                READ(10,100) DUMMY
 200            CONTINUE
                J=J+1
                GO TO 50
 5000           CONTINUE
        PRINT*,'READ COMPLETE'
        CLOSE(UNIT=10,STATUS='KEEP')
        OPEN(UNIT=30,ACCESS='DIRECT',FILE='HOYA.BIN',
     1  FORM='UNFORMATTED',RECL=132,STATUS='UNKNOWN')
        WRITE(UNIT=30,REC=1) JJ
        print*,jj
                DO 6000 I=1,JJ
        PRINT*,NAME(I),NUMBER(I),A0(I),A1(I),
     1  A2(I),A3(I),A4(I),A5(I)
        WRITE(UNIT=30,REC=I+1) NAME(I),NUMBER(I),A0(I),A1(I),
     1  A2(I),A3(I),A4(I),A5(I)
 6000           CONTINUE
        PRINT*,'WRITE COMPLETE'
                CLOSE(UNIT=30)
 100    FORMAT(A20)
 105    FORMAT(A13)
                STOP
                END