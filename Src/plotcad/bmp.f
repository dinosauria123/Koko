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

      SUBROUTINE READIMAGEARRAY(II,WRD1)

          USE GLOBALS
          IMPLICIT NONE
          INTEGER I,J,K,L,INFO(3),NX,NY,II
          INTEGER IR,IG,IB
          integer ALLOERR,IERROR
          REAL*8 WRD1
          INTEGER ABMPDATA24
          DIMENSION ABMPDATA24(:)
          CHARACTER*80 BMPFILE
          COMMON/FILEBMP/BMPFILE
          INCLUDE 'datmai.inc'
          ALLOCATABLE :: ABMPDATA24

          call bmpinfo(BMPFILE,INFO)

          IF(II.EQ.1) THEN
              OBJNX=INFO(2)
              NX=INFO(2)
              OBJNY=INFO(3)
              NY=INFO(3)
              DEALLOCATE (IOBJECTX,IOBJECTY,
     1        IOBJECTV,STAT=ALLOERR)
              ALLOCATE (IOBJECTX(NX,NY),IOBJECTY(NX,NY),
     1        IOBJECTV(NX,NY,3),STAT=ALLOERR)
              IOBJECTX(1:NX,1:NY)=0.0D0
              IOBJECTY(1:NX,1:NY)=0.0D0
              IOBJECTV(1:NX,1:NY,1:3)=0.0D0
              ODELX=WRD1/DBLE(REAL(NX-1))
              ODELY=ODELX
C       LOAD ARRAYS
              OXODD=.FALSE.
              IF((DBLE(NX)/2.0D0)-DBLE(NX/2).NE.0.0D0) OXODD=.TRUE.
              OYODD=.FALSE.
              IF((DBLE(NY)/2.0D0)-DBLE(NY/2).NE.0.0D0) OYODD=.TRUE.
C
              IF(OXODD) THEN
                  DO I=0,NX-1
                      DO J=1,NY
                          IOBJECTX(I+1,J)=-(ODELX*DBLE((NX-1)/2))+(DBLE(I)*ODELX)
                      END DO
                  END DO
              END IF
              IF(.NOT.OXODD) THEN
                  DO I=1,NX
                      DO J=1,NY
                          IOBJECTX(I,J)=-(ODELX*(DBLE(NX+1)/2.0D0))+(DBLE(I)*ODELX)
                      END DO
                  END DO
              END IF
              IF(OYODD) THEN
                  DO I=1,NX
                      DO J=0,NY-1
                          IOBJECTY(I,J+1)=-(ODELY*DBLE((NY-1)/2))+(DBLE(J)*ODELY)
                      END DO
                  END DO
              END IF
              IF(.NOT.OYODD) THEN
                  DO I=1,NX
                      DO J=1,NY
                          IOBJECTY(I,J)=-(ODELY*(DBLE(NY+1)/2.0D0))+(DBLE(J)*ODELY)
                      END DO
                  END DO
              END IF
          END IF
          IF(II.EQ.2) THEN
              IMGNX=INFO(2)
              NX=INFO(2)
              IMGNY=INFO(3)
              NY=INFO(3)
              DEALLOCATE (IIMAGEX,IIMAGEY,
     1        IIMAGEV,STAT=ALLOERR)
              ALLOCATE (IIMAGEX(NX,NY),IIMAGEY(NX,NY),
     1        IIMAGEV(NX,NY,3,4),STAT=ALLOERR)
              IIMAGEX(1:NX,1:NY)=0.0D0
              IIMAGEY(1:NX,1:NY)=0.0D0
              IIMAGEV(1:NX,1:NY,1:3,1:4)=0.0D0
              IDELX=W1/DBLE(NX-1)
              IDELY=IDELX
C       LOAD ARRAYS
              IXODD=.FALSE.
              IF ((DBLE(NX)/2.0D0)-DBLE(NX/2).NE.0.0D0) IXODD=.TRUE.
              IYODD=.FALSE.
              IF ((DBLE(NY)/2.0D0)-DBLE(NY/2).NE.0.0D0) IYODD=.TRUE.
C
              IF (IXODD) THEN
                  DO I=0,NX-1
                      DO J=1,NY
                          IIMAGEX(I+1,J)=-(IDELX*DBLE((NX-1)/2))+(DBLE(I)*IDELX)
                      END DO
                  END DO
              END IF
              IF(.NOT.IXODD) THEN
                  DO I=1,NX
                      DO J=1,NY
                          IIMAGEX(I,J)=-(IDELX*(DBLE(NX+1)/2.0D0))+(DBLE(I)*IDELX)
                      END DO
                  END DO
              END IF
              IF(IYODD) THEN
                  DO I=1,NX
                      DO J=0,NY-1
                          IIMAGEY(I,J+1)=-(IDELY*DBLE((NY-1)/2))+(DBLE(J)*IDELY)
                      END DO
                  END DO
              END IF
              IF(.NOT.IYODD) THEN
                  DO I=1,NX
                      DO J=1,NY
                          IIMAGEY(I,J)=-(IDELY*(DBLE(NY+1)/2.0D0))+(DBLE(J)*IDELY)
                      END DO
                  END DO
              END IF
          END IF
C
          DEALLOCATE (ABMPDATA24,STAT=ALLOERR)
          ALLOCATE (ABMPDATA24(INFO(2)*INFO(3)*3),STAT=ALLOERR)
          IF(II.EQ.1) THEN
C       READ OBJECT
              IF(NUMCOLORS.EQ.1) THEN
                  L=1
                  IERROR=0

                  call loadbmp(BMPFILE,ABMPDATA24)

                  DO K=1,OBJNY
                      DO J=1,OBJNX
                          call RGBsplit(ABMPDATA24,L,IR,IB,IG)
                          IOBJECTV(J,K,3)=DBLE(IR)
                          IOBJECTV(J,K,1)=DBLE(IG)
                          IOBJECTV(J,K,2)=DBLE(IB)
                          L=L+1
                      END DO
                  END DO
              END IF
              IF(NUMCOLORS.EQ.3) THEN
                  L=1
                  IERROR=0

                  call loadbmp(BMPFILE,ABMPDATA24)

                  DO K=1,OBJNY
                      DO J=1,OBJNX
                          call RGBsplit(ABMPDATA24,L,IR,IB,IG)
                          IOBJECTV(J,K,3)=DBLE(IR)
                          IOBJECTV(J,K,1)=DBLE(IG)
                          IOBJECTV(J,K,2)=DBLE(IB)
                          L=L+3
                      END DO
                  END DO
              END IF

          END IF
          IF(II.EQ.2) THEN
C       READ IMAGE
              IF(NUMCOLORS.EQ.1) THEN
                  L=1
                  IERROR=0

                  call loadbmp(BMPFILE,ABMPDATA24)

                  DO K=1,IMGNY
                      DO J=1,IMGNX
                          call RGBsplit(ABMPDATA24,L,IR,IB,IG)
                          IIMAGEV(J,K,3,1)=DBLE(IR)
                          IIMAGEV(J,K,1,1)=DBLE(IG)
                          IIMAGEV(J,K,2,1)=DBLE(IB)
                          L=L+3
                      END DO
                  END DO
              END IF
              IF(NUMCOLORS.EQ.3) THEN
                  L=1
                  IERROR=0

                  call loadbmp(BMPFILE,ABMPDATA24)

                  DO K=1,IMGNY
                      DO J=1,IMGNX
                          call RGBsplit(ABMPDATA24,L,IR,IB,IG)
                          IIMAGEV(J,K,3,1)=DBLE(IR)
                          IIMAGEV(J,K,1,1)=DBLE(IG)
                          IIMAGEV(J,K,2,1)=DBLE(IB)
                          L=L+3
                      END DO
                  END DO
              END IF

          END IF
          RETURN
      END


      SUBROUTINE PLOTIMAGEARRAY(I,LENBMP,TRIMMER)
          USE opsys
          USE GLOBALS
          IMPLICIT NONE
          INTEGER I,J,K,L
          INTEGER LENBMP
          INTEGER*4 INTERM
          INTEGER*4 BMPDATA24(1:LENBMP*3)
          integer iwidth,iheight
          character header(54)
          REAL*8 PEAKER,TRIMMER
          CHARACTER*80 BMPFILE
          common iwidth,iheight,header
          INCLUDE 'datmai.inc'

          BMPFILE=trim(HOME)//'PLOTBMP.BMP'

          IF(I.EQ.1) THEN
C       PLOT OBJECT
              IF(NUMCOLORS.EQ.1) THEN
                  PEAKER=-1.0D300
                  DO K=1,OBJNY
                      DO J=1,OBJNX
                          IF(IOBJECTV(J,K,1).GT.PEAKER) PEAKER=IOBJECTV(J,K,1)
                      END DO
                  END DO
                  IF(PEAKER.LE.255.0D0) PEAKER=1.0D0
                  IF(PEAKER.GT.255.0D0) PEAKER=PEAKER/255.0D0
                  L=1
                  DO K=1,OBJNY
                      DO J=1,OBJNX
                          INTERM=INT((IOBJECTV(J,K,1)/PEAKER))
                          BMPDATA24(L)=INTERM+(256*INTERM)+(256*256*INTERM)
                          L=L+1
                      END DO
                  END DO

                  CALL os_delete(trim(HOME)//'PLOTBMP.BMP')
                  CALL savebmp(BMPFILE,BMPDATA24)
                  CALL plotbmp(BMPFILE)

              END IF
              IF(NUMCOLORS.EQ.3) THEN
                  L=1
                  PEAKER=-1.0D300
                  DO K=1,OBJNY
                      DO J=1,OBJNX
                          DO L=1,3
                              IF(IOBJECTV(J,K,L).GT.PEAKER) PEAKER=IOBJECTV(J,K,L)
                          END DO
                      END DO
                  END DO
                  IF(PEAKER.LE.255.0D0) PEAKER=1.0D0
                  IF(PEAKER.GT.255.0D0) PEAKER=PEAKER/255.0D0
                  L=1
                  DO K=1,OBJNY
                      DO J=1,OBJNX
                          BMPDATA24(L)=int(((IOBJECTV(J,K,3))/PEAKER))
                          BMPDATA24(L+1)=int(((IOBJECTV(J,K,2))/PEAKER))
                          BMPDATA24(L+2)=int(((IOBJECTV(J,K,1))/PEAKER))
                          L=L+3
                      END DO
                  END DO

                  CALL os_delete(trim(HOME)//'PLOTBMP.BMP')
                  CALL savebmp(BMPFILE,BMPDATA24)
                  CALL plotbmp(BMPFILE)

              END IF

          END IF
          IF(I.EQ.2) THEN
C       PLOT IMAGE
              IF(NUMCOLORS.EQ.1) THEN
                  L=1
                  DO K=1+INT(TRIMMER),IMGNY-INT(TRIMMER)
                      DO J=1+INT(TRIMMER),IMGNX-INT(TRIMMER)
                          INTERM=INT((IIMAGEV(J,K,1,1)))
                          BMPDATA24(L)=INTERM+(256*INTERM)+(256*256*INTERM)
                          L=L+1
                      END DO
                  END DO

                  CALL os_delete(trim(HOME)//'PLOTBMP.BMP')
                  CALL savebmp(BMPFILE,BMPDATA24)
                  CALL plotbmp(BMPFILE)

              END IF

              IF(NUMCOLORS.EQ.3) THEN
                  L=1
                  DO K=1+INT(TRIMMER),IMGNY-INT(TRIMMER)
                      DO J=1+INT(TRIMMER),IMGNX-INT(TRIMMER)
                          BMPDATA24(L)=int(((IIMAGEV(J,K,3,1))))
                          BMPDATA24(L+1)=int(((IIMAGEV(J,K,2,1))))
                          BMPDATA24(L+2)=int(((IIMAGEV(J,K,1,1))))
                          L=L+3
                      END DO
                  END DO

                  CALL os_delete(trim(HOME)//'PLOTBMP.BMP')
                  CALL savebmp(BMPFILE,BMPDATA24)
                  CALL plotbmp(BMPFILE)

              END IF

          END IF
          RETURN
      END


      SUBROUTINE WRITEIMAGEARRAY(I,LENBMP)

          USE GLOBALS
          IMPLICIT NONE
          INTEGER I,J,K,L
          INTEGER LENBMP
          INTEGER*4 INTERM
          INTEGER*4 BMPDATA24(1:LENBMP)
          REAL*8 PEAKER
          CHARACTER*80 BMPFILE
          INCLUDE 'datmai.inc'

          IF(I.EQ.1) THEN
C       PLOT OBJECT
              IF(NUMCOLORS.EQ.1) THEN
                  PEAKER=-1.0D300
                  DO K=1,OBJNY
                      DO J=1,OBJNX
                          IF(IOBJECTV(J,K,1).GT.PEAKER) PEAKER=IOBJECTV(J,K,1)
                      END DO
                  END DO
                  IF(PEAKER.LE.255.0D0) PEAKER=1.0D0
                  IF(PEAKER.GT.255.0D0) PEAKER=PEAKER/255.0D0
                  L=1
                  DO K=1,OBJNY
                      DO J=1,OBJNX
                          INTERM=INT((IOBJECTV(J,K,1)/PEAKER))
                          BMPDATA24(L)=INTERM+(256*INTERM)+(256*256*INTERM)
                          L=L+1
                      END DO
                  END DO
              END IF

              IF(NUMCOLORS.EQ.3) THEN
                  L=1
                  PEAKER=-1.0D300
                  DO K=1,OBJNY
                      DO J=1,OBJNX
                          DO L=1,3
                              IF(IOBJECTV(J,K,L).GT.PEAKER) PEAKER=IOBJECTV(J,K,L)
                          END DO
                      END DO
                  END DO
                  IF(PEAKER.LE.255.0D0) PEAKER=1.0D0
                  IF(PEAKER.GT.255.0D0) PEAKER=PEAKER/255.0D0
                  DO K=1,OBJNY
                      DO J=1,OBJNX
                          BMPDATA24(L)=int(((IOBJECTV(J,K,3))/PEAKER))
                          BMPDATA24(L+1)=int(((IOBJECTV(J,K,2))/PEAKER))
                          BMPDATA24(L+2)=int(((IOBJECTV(J,K,1))/PEAKER))
                          L=L+3
                      END DO
                  END DO
                  call savebmp(BMPFILE,BMPDATA24)
              END IF

          END IF
          IF(I.EQ.2) THEN
C       PLOT IMAGE
              IF(NUMCOLORS.EQ.1) THEN
                  L=1
                  DO K=1,IMGNY
                      DO J=1,IMGNX
                          INTERM=INT((IIMAGEV(J,K,1,1)))
                          BMPDATA24(L)=INTERM+(256*INTERM)+(256*256*INTERM)
                          L=L+1
                      END DO
                  END DO
              END IF
              IF(NUMCOLORS.EQ.3) THEN
                  L=1
                  DO K=1,IMGNY
                      DO J=1,IMGNX
                          BMPDATA24(L)=int(((IOBJECTV(J,K,3))/PEAKER))
                          BMPDATA24(L+1)=int(((IOBJECTV(J,K,2))/PEAKER))
                          BMPDATA24(L+2)=int(((IOBJECTV(J,K,1))/PEAKER))
                          L=L+3

                      END DO
                  END DO
                  call savebmp(BMPFILE,BMPDATA24)
              END IF
          END IF
          RETURN
      END


      SUBROUTINE loadbmp(BMPFILE,ABMPDATA24)

!     BMP reading routine obtained from
!     http://www.rhinocerus.net/forum/lang-fortran/93927-read-bmp-into-fortran.html

          INCLUDE 'datmai.inc'
          INTEGER Maxwidth,Maxheight,irec,iwidth,iheight,i,j,L,ipad
          PARAMETER(Maxwidth=3220,Maxheight=2415)
          CHARACTER header(54),cval1
          CHARACTER*80 BMPFILE
          INTEGER ABMPDATA24(iwidth*iheight*3)
          COMMON iwidth,iheight,header,image

          OPEN(114,file=TRIM(BMPFILE),form='unformatted',
     &    access='direct',recl=1)

          DO irec=1,54
              READ(114,rec=irec) header(irec)
          END DO

          IF(ICHAR(header(11)).NE.54.OR.ICHAR(header(29)).ne .
     &    24.OR.ICHAR(header(31)).NE.0) THEN
              PRINT*,'sorry, can not handle this file'
          END IF

          ! get image height and width
          iheight=ICHAR(header(23))+256*(ICHAR(header(24))
     &    +256*(ICHAR(header(25))+256*ICHAR(header(26))))
          iwidth=ICHAR(header(19))+256*(ICHAR(header(20))
     &    +256*(ICHAR(header(21))+256*ICHAR(header(22))))

          ipad=(Maxwidth-iwidth)*3-((Maxwidth-iwidth)*3/4)*4
          irec=54
          L=1
          DO i=1,iheight*3
              DO j=1,iwidth
                  READ(114,rec=irec) cval1
                  ABMPDATA24(L)=ICHAR(cval1)
                  L=L+1
                  irec=irec+1
              END DO
          END DO

          CLOSE(114)

          RETURN
      END


      SUBROUTINE savebmp(BMPFILE,BMPDATA24)

          INCLUDE 'datmai.inc'
          INTEGER Maxwidth,Maxheight,iheight,iwidth,irec,L,I,J
          PARAMETER(Maxwidth=3220,Maxheight=2415)
          INTEGER BMPDATA24(iwidth*iheight*3)
          CHARACTER header(54),bmpdataR,bmpdataG,bmpdataB
          CHARACTER*80 BMPFILE
          COMMON iwidth,iheight,header

          OPEN(112,file=TRIM(BMPFILE),form='unformatted',
     &    access='direct',recl=1)

          DO irec=1,54
              WRITE(112,rec=irec) header(irec)
          END DO

          irec=55
          L=1

          DO I=1,iheight*3
              DO J=1,iwidth
                  bmpdataG=CHAR(BMPDATA24(L))
                  WRITE(112,rec=irec) bmpdataG
                  bmpdataR=CHAR(BMPDATA24(L+1))
                  WRITE(112,rec=irec+1) bmpdataR
                  bmpdataB=CHAR(BMPDATA24(L+2))
                  WRITE(112,rec=irec+2) bmpdataB
                  irec=irec+3
                  IF (L.GE.iheight*iwidth*3-3) EXIT
                  L=L+3
              END DO
          END DO

          CLOSE(112)
          RETURN
      END

      
      SUBROUTINE bmpinfo(BMPFILE,info)

          INCLUDE 'datmai.inc'
          INTEGER irec,iwidth,iheight
          INTEGER info(3)
          CHARACTER header(54)
          CHARACTER*80 BMPFILE
          COMMON iwidth,iheight,header

          OPEN(114,file=TRIM(BMPFILE),form='unformatted',
     &    access='direct',recl=1)

          DO irec=1,54
              READ(114,rec=irec) header(irec)
          END DO

          IF(ICHAR(header(11)).NE.54.OR.ICHAR(header(29)).ne .
     &    24.OR.ICHAR(header(31)).NE.0) THEN
              PRINT*,'sorry, can not handle this file'
          END IF

          ! get image height and width
          info(3)=ICHAR(header(23))+256*(ICHAR(header(24))
     &    +256*(ICHAR(header(25))+256*ICHAR(header(26))))
          info(2)=ICHAR(header(19))+256*(ICHAR(header(20))
     &    +256*(ICHAR(header(21))+256*ICHAR(header(22))))

          iwidth=info(3)
          iheight=info(2)

          CLOSE(114)

          RETURN
      END


      SUBROUTINE RGBsplit(ABMPDATA24,L,IR,IB,IG)

          INCLUDE 'datmai.inc'
          INTEGER IR,IB,IG,L,ABMPDATA24
          DIMENSION ABMPDATA24(iwidth*iheight*3)
          COMMON iwidth,iheight

          IF (L.GE.iwidth*iheight*3-3) RETURN

          IG=INT(ABMPDATA24(L))
          IR=INT(ABMPDATA24(L+1))
          IB=INT(ABMPDATA24(L+2))

          RETURN
      END
      
