      subroutine hcmnd
c
c     ******************************************************************
c     *                                                                *
c     *      hcmnd is used to output a list of the game moves that     *
c     *  it kept by the program.  it also keeps up with the time used  *
c     *  by each side to make each move and prints these times in      *
c     *  parenthe8e8 (.*.) with each move.  a '+' appears by the       *
c     *  color of which side blitz is playing,                         *
c     *                                                                *
c     ******************************************************************
c
      implicit integer (a-z)
      real relap1, relap2
      integer buf1(30), buf2(30), format(20), digits(40), lim(5)
      common /mov cnt/ npmovs, nomovs
      common /tree/ moves(2000), first(30), last(30), which(30),
     *              in chk(30), giv chk(30)
      common /colr cm/ color
      equivalence (buf1(1),moves(100)),(buf2(1),moves(200))
      common /chrset/ alpha(46)
      equivalence (period,alpha(42)),(blank,alpha(44))
      data format/'(1x,' , 'i3,' , '1x,' , ' ' , 'a1,' ,
     *  '1x,' , 'f' , ' ' , '.2,t' , ' ' , ''':''' , 't25,' ,
     *  ' ' , 'a1,' , '1x', 'f' , ' ' , '.2,t' , ' ' , ''':'')' /
      data digits/'1','2','3','4','5','6','7','8','9','10',
     *'11','12','13','14','15','16','17','18','19','20','21','22',
     *'23','24','25','26','27','28','29','30','31','32','33','34',
     *'35','36','37','38','39','40' /
      data lim / -1, 60, 600, 6000, 60000 /
c
c------------------------------< initialize.
c
      m=0
      i=0
c
c------------------------------< output heading
c
      if(color .eq. 2) print 100
      if(color .eq. 1) print 200
100   format(5x,'white',15x,'black+')
200   format(5x,'white+',14x,'black')
300   continue
          do 400 j=1,8
              buf2(j)=period
400       continue
          do 500 j=9,30
              buf2(j)=blank
500       continue
c
c------------------------------< read moves from history file
c
          elap2=0
          i=i+1
          if(i .gt. npmovs+nomovs) go to 800
          read(1'i) buf1, elap1
          i=i+1
          if(i .gt. npmovs+nomovs) go to 600
          read(1'i) buf2, elap2
600       continue
          m=m+1

c
c------------------------------< set up variable format to output
c------------------------------< '(movetime)'
c
          do 620 j=1,5
              if(elap1 .ge. lim(j)) w1=j+3
              if(elap2 .ge. lim(j)) w2=j+3
620       continue
          do 650 j=1,15
              if(buf1(16-j) .ne. blank) go to 675
650       continue
675       continue
          end1=16-j
          format(4)=digits(end1)
          format(8)=digits(w1)
          do 680 j=1,15
              if(buf2(16-j).ne.blank) go to 685
680       continue
685       continue
          end2=16-j
          format(13)=digits(end2)
          format(17)=digits(w2)
          format(10)=digits(4+end1+w1)
          format(19)=digits(23+end2+w2)
      relap1=elap1/60
      relap1=relap1+float(mod(elap1,60))/100
      relap2=elap2/60
      relap2=relap2+float(mod(elap2,60))/100
          print format, m, (buf1(j),j=1,end1), relap1,
     *                     (buf2(j),j=1,end2), relap2
      go to 300
800   continue
      return
      end


