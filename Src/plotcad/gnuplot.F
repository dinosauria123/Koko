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

      subroutine datacolorssave(I1,I2,I3,I4)

          implicit none
          integer I1,I2,I3,I4

          if (I1.LT.0.OR.I2.LT.0.OR.I1.GT.10000.OR.I2.GT.7000) then
              return
          end if

          if (I4.eq.0) then
              if (I3.eq.1) then
                  write(130,'(2I5)') I1,I2
              else
                  write(130,*)
                  write(130,'(2I5)') I1,I2
              end if
          end if

          if (I4.eq.1) then
              if (I3.eq.1) then
                  write(115,'(2I5)') I1,I2
              else
                  write(115,*)
                  write(115,'(2I5)') I1,I2

              end if
          end if

          if (I4.eq.2) then
              if (I3.eq.1) then
                  write(116,'(2I5)') I1,I2
              else
                  write(116,*)
                  write(116,'(2I5)') I1,I2
              end if
          end if

          if (I4.eq.3) then
              if (I3.eq.1) then
                  write(117,'(2I5)') I1,I2
              else
                  write(117,*)
                  write(117,'(2I5)') I1,I2
              end if
          end if

          if (I4.eq.4) then
              if (I3.eq.1) then
                  write(118,'(2I5)') I1,I2
              else
                  write(118,*)
                  write(118,'(2I5)') I1,I2
              end if
          end if

          return
      end


      subroutine drawdatasave(I1,I2,I3,I4)

          implicit none
          integer I1,I2,I3,I4

          if (I1.LT.0.OR.I2.LT.0.OR.I1.GT.10000.OR.I2.GT.7000) then
              return
          end if

          if (I4.eq.0) then
              if (I3.eq.1) then
                  write(130,'(2I5)') I1,I2
              else
                  write(130,*)
                  write(130,'(2I5)') I1,I2
              end if
          end if

          if (I4.eq.1) then
              if (I3.eq.1) then
                  write(115,'(2I5)') I1,I2
              else
                  write(115,*)
                  write(115,'(2I5)') I1,I2
              end if
          end if

          if (I4.eq.2) then
              if (I3.eq.1) then
                  write(116,'(2I5)') I1,I2
              else
                  write(116,*)
                  write(116,'(2I5)') I1,I2

              end if
          end if

          if (I4.eq.3) then
              if (I3.eq.1) then
                  write(117,'(2I5)') I1,I2
              else
                  write(117,*)
                  write(117,'(2I5)') I1,I2
              end if
          end if

          if (I4.eq.4) then
              if (I3.eq.1) then
                  write(118,'(2I5)') I1,I2
              else
                  write(118,*)
                  write(118,'(2I5)') I1,I2

              end if
          end if

          if (I4.eq.2) then
              if (I3.eq.1) then
                  write(131,'(2I5)') I1,I2
              else
                  write(131,*)
                  write(131,'(2I5)') I1,I2

              end if
          end if

          return
      end


      subroutine gnuplotlabel(I1,I2,label,I3,I4)

          implicit none
          integer I1,I2,I3,I4
          real X,Y
          character label*80

          X=real(I1)/10500.0+0.02
          Y=real(I2)/7350.0+0.03

          if ((I4.eq.2).and.(I3.eq.90)) then
              write(150,'(A,F6.3,A1,F6.3,A2,A,A)') 'set label font "Courier,12"
     1center at screen ',real(I1)/10000.0,",",real(I2/7000.0)+0.01,
     2        ' "',trim(label),'" rotate by 90'
              goto 10

          else if (I4.eq.2) then
              write(150,'(A,F6.3,A1,F6.3,A2,A,A)') 'set label font "Courier,12"
     1center at screen ',real(I1)/10000.0,",",real(I2/7000.0)+0.01,
     2        ' "',trim(label),'"'
              goto 10

          else
              write(150,'(A,F6.3,A1,F6.3,A2,A,A)') 'set label at screen '
     1        ,X,",",Y,' "',trim(label),'"'

          end if

   10     return
      end


      subroutine contlabel(X0,Y0,label)

          implicit none

          real X0,Y0,X,Y
          character label*80

          X=real(300.0/10000.0*X0)
          Y=real(210.0/7000.0*Y0)

          write(150,'(A,F6.3,A1,F6.3,A2,A,A)') 'set label at screen ',
     1    X,",",Y,' "',label,'"'

          return
      end



      subroutine drawcmdsave

          include 'datmai.inc'
          character*150 catcommand

          catcommand="cat "//trim(HOME)//"drawcmd0.txt "
     1    //trim(HOME)//"drawcmd3.txt "//"> "//trim(HOME)//"drawcmd.txt"

          call system(catcommand)

          return
      end


      subroutine drawcmdsave2

          include 'datmai.inc'
          character*150 catcommand2

          catcommand2="cat "//trim(HOME)//"drawcmd3.txt "
     1    //trim(HOME)//"plotdata/plotcont.txt"//"> "
     2    //trim(HOME)//"drawcmd.txt"

          call system(catcommand2)

          return
      end


      subroutine setonecolors

          include 'datmai.inc'

          write(150,*) 'plot [0:10000] [0:7000] "'//
     1     trim(HOME)//'plotdata/black.txt" lc rgb "black" lw 0.5 w l'
          write(150,*) 'pause -1'

          return
      end

      subroutine setonecolors2

          include 'datmai.inc'

          write(150,*) 'plot [0:10000] [0:7000] "'//
     1     trim(HOME)//'plotdata/black.txt" lc rgb "black"
     1 lw 0.5 w l,"'//
     1     trim(HOME)//'plotdata/breakblack.txt" with lines lt 0
     1 lc rgb "black" lw 2'

          write(150,*) 'pause -1'

          return
      end


      subroutine settwocolors

          include 'datmai.inc'

          write(150,*) 'plot [0:10000] [0:7000] "'//
     1     trim(HOME)//'plotdata/black.txt" lc rgb "black"
     1 lw 0.5 w l,"'//
     1     trim(HOME)//'plotdata/yellow.txt" lc rgb "dark-yellow"
     1 lw 0.5 w l'
          write(150,*) 'pause -1'

          return
      end

      subroutine settwocolors2

          include 'datmai.inc'

          write(150,*) 'plot [0:10000] [0:7000] "'//
     1     trim(HOME)//'plotdata/black.txt" lc rgb "black"
     1 lw 0.5 w l,"'//
     1     trim(HOME)//'plotdata/red.txt" lc rgb "red"
     1 lw 0.5 w l'
          write(150,*) 'pause -1'

          return
      end

      subroutine settwocolors3

          include 'datmai.inc'

          write(150,*) 'plot [0:10000] [0:7000] "'//
     1     trim(HOME)//'plotdata/black.txt" lc rgb "black"
     1 lw 0.5 w l,"'//
     1     trim(HOME)//'plotdata/red.txt" lc rgb "red"
     1 pt 7 ps 0.3'
          write(150,*) 'pause -1'

          return
      end

      subroutine setthreecolors

          include 'datmai.inc'

          write(150,*) 'plot [0:10000] [0:7000] "'//
     1     trim(HOME)//'plotdata/black.txt" lc rgb "black"
     1 lw 0.5 w l,"'//
     1     trim(HOME)//'plotdata/yellow.txt" lc rgb "dark-yellow"
     1 lw 0.5 w l,"'//
     1     trim(HOME)//'plotdata/magenta.txt" lc rgb "magenta"
     1 lw 0.5 w l'
          write(150,*) 'pause -1'

          return
      end

      subroutine setthreecolors2

          include 'datmai.inc'

          write(150,*) 'plot [0:10000] [0:7000] "'//
     1     trim(HOME)//'plotdata/black.txt" lc rgb "black"
     1 lw 0.5 w l,"'//
     1     trim(HOME)//'plotdata/yellow.txt" lc rgb "dark-yellow"
     1 lw 0.5 w l,"'//
     1     trim(HOME)//'plotdata/red.txt" lc rgb "red" lw 0.5 w l'
          write(150,*) 'pause -1'

          return
      end


      subroutine setfourcolors

          include 'datmai.inc'

          write(150,*) 'plot [0:10000] [0:7000] "'//
     1     trim(HOME)//'plotdata/black.txt" lc rgb "black"
     1 lw 0.5 w l,"'//
     1     trim(HOME)//'plotdata/yellow.txt" lc rgb "dark-yellow"
     1 lw 0.5 w l,"'//
     1     trim(HOME)//'plotdata/magenta.txt" lc rgb "magenta"
     1 lw 0.5 w l,"'//
     1     trim(HOME)//'plotdata/red.txt" lc rgb "red" lw 0.5 w l'
          write(150,*) 'pause -1'

          return
      end

      SUBROUTINE MAC_EDITOR
        
          USE opsys
          USE GLOBALS

          IMPLICIT NONE
          
          INCLUDE 'datmai.inc'
          INCLUDE 'datmac.inc'

          CALL shell_command(TRIM(TXTEDITOR)//" "//"MAC_EDIT.DAT")

          RETURN
      END


      subroutine plotbmp(BMPFILE)

          include 'datmai.inc'
          character*80 BMPFILE
          logical BMPEXIST

          open(113,file=trim(HOME)//'PLOTBMP.TXT')

#ifdef LINUX
          write(113,*) 'set terminal wxt font "Courier,5"'
#endif
#ifdef OSX
          write(113,*) 'set terminal aqua font "Courier,9"'
#endif
          write(113,*) 'set noborder'
          write(113,*) 'set nokey'
          write(113,*) 'set notics'
          write(113,*) 'plot "'//trim(BMPFILE)//'" binary
     1array=(320,240) skip=54 format="%uint8" using 3:2:1
     2with rgbimage'
          write(113,*) 'pause -1'
          close(113)

#ifdef OSX
          call system('/opt/local/bin/gnuplot '//trim(HOME)//'PLOTBMP.TXT')
#endif
#ifdef LINUX
          call system('gnuplot '//trim(HOME)//'PLOTBMP.TXT')
#endif

          inquire(file=trim(BMPFILE),exist=BMPEXIST)

          if (.not.BMPEXIST) then

              WRITE(OUTLYNE,*) 'ERROR OPENING ',TRIM(BMPFILE)
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN

          END IF
          return
      end


      subroutine loadbmp(BMPFILE,ABMPDATA24)

!     BMP reading routine obtained from
!     http://www.rhinocerus.net/forum/lang-fortran/93927-read-bmp-into-fortran.html

          include 'datmai.inc'
          integer Maxwidth,Maxheight,irec,iwidth,iheight,i,j,L,ipad
          parameter(Maxwidth=3220,Maxheight=2415)
          character header(54),cval1
          character*80 BMPFILE
          integer ABMPDATA24(iwidth*iheight*3)
          common iwidth,iheight,header,image

          open(114,file=trim(BMPFILE),form='unformatted',
     &    access='direct',recl=1)

          do irec=1,54
              read(114,rec=irec) header(irec)
          end do

          if(ichar(header(11)).ne.54.or.ichar(header(29)).ne .
     &    24.or.ichar(header(31)).ne.0) then
              print*,'sorry, can not handle this file'
          end if

          ! get image height and width
          iheight=ichar(header(23))+256*(ichar(header(24))
     &    +256*(ichar(header(25))+256*ichar(header(26))))
          iwidth=ichar(header(19))+256*(ichar(header(20))
     &    +256*(ichar(header(21))+256*ichar(header(22))))

          ipad=(Maxwidth-iwidth)*3-((Maxwidth-iwidth)*3/4)*4
          irec=54
          L=1
          do i=1,iheight*3
              do j=1,iwidth
                  read(114,rec=irec) cval1
                  ABMPDATA24(L)=ichar(cval1)
                  L=L+1
                  irec=irec+1
              end do
          end do

          close(114)

          return
      end


      subroutine savebmp(BMPFILE,BMPDATA24)

          include 'datmai.inc'
          integer Maxwidth,Maxheight,iheight,iwidth,irec,L,I,J
          parameter(Maxwidth=3220,Maxheight=2415)
          integer BMPDATA24(iwidth*iheight*3)
          character header(54),bmpdataR,bmpdataG,bmpdataB
          character*80 BMPFILE
          common iwidth,iheight,header

          open(112,file=trim(BMPFILE),form='unformatted',
     &    access='direct',recl=1)

          do irec=1,54
              write(112,rec=irec) header(irec)
          end do

          irec=55
          L=1

          do I=1,iheight*3
              do J=1,iwidth
                  bmpdataG=char(BMPDATA24(L))
                  write(112,rec=irec) bmpdataG
                  bmpdataR=char(BMPDATA24(L+1))
                  write(112,rec=irec+1) bmpdataR
                  bmpdataB=char(BMPDATA24(L+2))
                  write(112,rec=irec+2) bmpdataB
                  irec=irec+3
                  if (L.ge.iheight*iwidth*3-3) exit
                  L=L+3
              end do
          end do

          close(112)
          return
      end

      
      subroutine bmpinfo(BMPFILE,info)

          include 'datmai.inc'
          integer irec,iwidth,iheight
          integer info(3)
          character header(54)
          character*80 BMPFILE
          common iwidth,iheight,header

          open(114,file=trim(BMPFILE),form='unformatted',
     &    access='direct',recl=1)

          do irec=1,54
              read(114,rec=irec) header(irec)
          end do

          if(ichar(header(11)).ne.54.or.ichar(header(29)).ne .
     &    24.or.ichar(header(31)).ne.0) then
              print*,'sorry, can not handle this file'
          end if

          ! get image height and width
          info(3)=ichar(header(23))+256*(ichar(header(24))
     &    +256*(ichar(header(25))+256*ichar(header(26))))
          info(2)=ichar(header(19))+256*(ichar(header(20))
     &    +256*(ichar(header(21))+256*ichar(header(22))))

          iwidth=info(3)
          iheight=info(2)

          close(114)

          return
      end


      subroutine RGBsplit(ABMPDATA24,L,IR,IB,IG)

          include 'datmai.inc'
          integer IR,IB,IG,L,ABMPDATA24
          dimension ABMPDATA24(iwidth*iheight*3)
          common iwidth,iheight

          if (L.ge.iwidth*iheight*3-3) return

          IG=int(ABMPDATA24(L))
          IR=int(ABMPDATA24(L+1))
          IB=int(ABMPDATA24(L+2))

          return
      end


      subroutine saveeps(GRFILN)

          include 'datmai.inc'
          CHARACTER GRFILN*12

#ifdef OSX
          call system('sed -e "s/aqua/eps/g" -e "s/pause -1//g" '
     &    //trim(HOME)//'drawcmd.txt > '//trim(HOME)//'epsout.txt')
          call system('/opt/local/bin/gnuplot '//trim(HOME)//'epsout.txt > '
     &    //trim(USERHOME)//'/'//trim(GRFILN))
#endif

#ifdef LINUX
          call system('sed -e "s/wxt/eps/g" -e "s/pause -1//g" '
     &    //trim(HOME)//'drawcmd.txt > '//trim(HOME)//'epsout.txt')
          call system('/usr/bin/gnuplot '//trim(HOME)//'epsout.txt > '
     &    //trim(USERHOME)//'/'//trim(GRFILN))
#endif
          return
      end


      subroutine savejpg(GRFILN)

          include 'datmai.inc'
          CHARACTER GRFILN*12

#ifdef OSX
          call system('sed -e "s/aqua/jpeg/g" -e "s/pause -1//g" '
     &    //trim(HOME)//'drawcmd.txt > '//trim(HOME)//'jpgout.txt')
          call system('/opt/local/bin/gnuplot '//trim(HOME)//'jpgout.txt > '
     &    //trim(USERHOME)//'/'//trim(GRFILN))
#endif

#ifdef LINUX
          call system('sed -e "s/wxt/jpeg/g" -e "s/pause -1//g" '
     &    //trim(HOME)//'drawcmd.txt > '//trim(HOME)//'jpgout.txt')
          call system('/usr/bin/gnuplot '//trim(HOME)//'jpgout.txt > '
     &    //trim(USERHOME)//'/'//trim(GRFILN))
#endif
          return
      end

      subroutine savepdf(GRFILN)

          include 'datmai.inc'
          CHARACTER GRFILN*12

#ifdef OSX
          call system('sed -e "s/aqua/pdfcairo/g" -e "s/pause -1//g" '
     &    //trim(HOME)//'drawcmd.txt > '//trim(HOME)//'pdfout.txt')
          call system('/opt/local/bin/gnuplot '//trim(HOME)//'pdfout.txt > '
     &    //trim(USERHOME)//'/'//trim(GRFILN))
#endif

#ifdef LINUX
          call system('sed -e "s/wxt/pdfcairo/g" -e "s/pause -1//g" '
     &    //trim(HOME)//'drawcmd.txt > '//trim(HOME)//'pdfout.txt')
          call system('/usr/bin/gnuplot '//trim(HOME)//'pdfout.txt > '
     &    //trim(USERHOME)//'/'//trim(GRFILN))
#endif
          return
      end

      subroutine del_col_files

          include 'datmai.inc'

          call system('rm '//trim(HOME)//'plotdata/*.txt')
          call system('touch '//trim(HOME)//'plotdata/black.txt')
          call system('touch '//trim(HOME)//'plotdata/breakblack.txt')
          call system('touch '//trim(HOME)//'plotdata/red.txt')
          call system('touch '//trim(HOME)//'plotdata/yellow.txt')
          call system('touch '//trim(HOME)//'plotdata/magenta.txt')
          return
      end
