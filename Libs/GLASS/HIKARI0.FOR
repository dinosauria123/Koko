        PROGRAM MAIN
C
        IMPLICIT NONE
C
        CHARACTER NAME*13,NUMBER1*13,NUMBER2*13
C
        INTEGER I
C
        DOUBLE PRECISION A0,A1,
     1  A2,A3,A4,A5
C
        OPEN(UNIT=10,ACCESS='SEQUENTIAL',FILE='HIKARI.TXT',
     1  STATUS='UNKNOWN')
        OPEN(UNIT=11,ACCESS='SEQUENTIAL',FILE='HIKARI.DAT',
     1  STATUS='UNKNOWN')
                DO I=1,134
        READ(10,*) NAME,NUMBER1,NUMBER2,A0,A1,A2,A3,A4,A5
        WRITE(11,11) NAME
        WRITE(11,11) NUMBER1
        WRITE(11,*) A0
        WRITE(11,*) A1
        WRITE(11,*) A2
        WRITE(11,*) A3
        WRITE(11,*) A4
        WRITE(11,*) A5
                        END DO
      REWIND(UNIT=10)
                DO I=1,134
        READ(10,*) NAME,NUMBER1,NUMBER2,A0,A1,A2,A3,A4,A5
        WRITE(11,11) NAME
        WRITE(11,11) NUMBER2
        WRITE(11,*) A0
        WRITE(11,*) A1
        WRITE(11,*) A2
        WRITE(11,*) A3
        WRITE(11,*) A4
        WRITE(11,*) A5
                        END DO
        CLOSE(UNIT=10,STATUS='KEEP')
        CLOSE(UNIT=11,STATUS='KEEP')
 11   FORMAT(A13)
                STOP
                END