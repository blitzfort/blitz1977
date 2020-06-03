      subroutine sbcmnd
c
c     ******************************************************************
c     *                                                                *
c     *      sbcmnd is used to set up the board in any position        *
c     *  desired.  it uses a forsythe like string of characters        *
c     *  to describe the board position.  the board squares            *
c     *  are numbered as follows:                                      *
c     *                                                                *
c     *                   1  2  3  4  5  6  7  8                       *
c     *                   9 1o 11 12 13 14 15 16                       *
c     *                  17 18 19 20 21 22 23 24                       *
c     *                  25 26 27 28 29 30 31 32                       *
c     *                  33 34 35 36 37 38 39 40                       *
c     *                  41 42 43 44 45 46 47 48                       *
c     *                  49 50 51 52 53 54 55 56                       *
c     *                  57 58 59 60 61 62 63 64                       *
c     *                                                                *
c     *      using standard piece codes, simply type in the            *
c     *  code for the piece you wish to place there starting           *
c     *  with computer pieces.  use a number between 1 and 8 to        *
c     *  indicate empty squares.  when all computer pieces are         *
c     *  defined, type a '.' or period in the string. continueing      *
c     *  start back at square 1 and start defining human pieces.       *
c     *  for example, 'k2r4ppp.88888q75ppp7k ' would define the        *
c     *  following board position:                                     *
c     *                                                                *
c     *                  k  *  *  r  *  *  *  *                        *
c     *                  p  p  p  *  *  *  *  *                        *
c     *                  *  *  *  *  *  *  *  *                        *
c     *                  *  *  *  *  *  *  *  *                        *
c     *                  *  *  *  *  *  *  *  *                        *
c     *                 -q  *  *  *  *  *  *  *                        *
c     *                  *  *  *  *  * -p -p -p                        *
c     *                  *  *  *  *  *  *  * =k                        *
c     *                                                                *
c     ******************************************************************
c
      implicit integer(a-z)
      logical pmoved, moved, over
      logical in book, repchk, rtemp
      logical modify
      common /depth/ sdepth, depth, ply
      common /colr cm/ color
      common /castcm/ pmoved(3,2), moved(3,30)
      common /board/ board(120)
      common /tree/ moves(2000), first(30), last(30), which(30),
     *              in chk(30), giv chk(30)
      common /trcecm/ strace(32)
      common /l move/ lmovep, lmoveo
      common /predcm/ ptext(30), pmove, ptype$, pfrom$, pto$
      common /bookcm/ in book, key
      common /buffer/ text(80)
      common /dup/ bdsave(1040), point
      common /chr set/ alpha(44)
      equivalence(o,alpha(15)),(x,alpha(24))
      equivalence(moves(900), input(1))
      common /over/ over
      dimension char(16), input(80)
      data char / 1hp, 1hn, 1hb, 1hr, 1hq, 1hk, 1h , 1h., 1h1,
     *           1h2, 1h3, 1h4, 1h5, 1h6, 1h7, 1h8 /
c
c------------------------------< initialize.
c
      in book=.false.
      over=.false.
      modify=text(3) .eq. 0
9999  continue
      print 6
6     format(1x,'enter board position')
      call read
      if(text(1) .eq. char(7)) go to 6000
      do 1 i=1,2
          do 2 j=1,3
              pmoved(j,i)=.false.
2         continue
1     continue
      do 442 i=1,1040
          bdsave(i)=0
442   continue
      point=1
      if(modify) go to 9
      do 400 i=2,9
          do 390 j=2,9
            board(i*10+j)=0
390       continue
400   continue
9     continue
      sq=21
      side=1
      do 110 j=1,80
          do 100 k=1,16
              if(text(j) .eq. char(k)) go to 101
100       continue
          if(.not. modify) go to 120
          if(text(j) .ne. x) go to 120
          sq=sq+1
          if(mod(sq-1,10) .gt. 8) sq=sq+2
          board(sq)=0
          go to 110
101       continue
          if(k .eq. 7) go to 80
          if(k .eq. 8) go to 60
          if(k .gt. 8) go to 70
c
c------------------------------< character indicates a piece
c
50        continue
          sq=sq+1
          if(mod(sq-1,10) .gt. 8) sq=sq+2
          if(sq .gt. 99) go to 120
          if(board(sq).ne.0 .and. .not.modify) go to 120
          board(sq)=k*side
          go to 110
c
c------------------------------< period indicates end of listing
c------------------------------< of computer pieces
c
60        continue
          if(sq .gt. 99) go to 120
          if(side .ne. 1) go to 120
          side=-1
          sq=21
          go to 110
c
c------------------------------< number indicates empty square(s)
c
70        continue
          lim=k-8
          do 71 i=1, lim
               sq=sq+1
               if(mod(sq-1,10).gt.8 .or. mod(sq-1,10).lt.1) sq=sq+2
71        continue
110    continue
c
c------------------------------< blank indicates end of
c------------------------------< string
c
80    continue
      pkings=0
      okings=0
      do 41 i=2,9
          do 40 j=2,9
              sq=i*10+j
              if(board(sq) .eq. 6) pkings=pkings+1
              if(board(sq) .eq. -6) okings=okings+1
40        continue
41    continue
      if(okings.ne.1 .or. pkings.ne.1) go to 140
      kfile=4+color
      if(board(20+kfile) .ne. 6) pmoved(1,1)=.true.
      if(board(90+kfile) .ne. -6) pmoved(1,2)=.true.
      if(board(22) .ne. 4) pmoved(2,1)=.true.
      if(board(29) .ne. 4) pmoved(3,1)=.true.
      if(board(92) .ne. -4) pmoved(2,2)=.true.
      if(board(99) .ne. -4) pmoved(3,2)=.true.
      do 449 i=1,32
          strace(i)=0
449   continue
      lmovep=0
      lmoveo=0
      pmove=0
      ply=0
      rtemp=repchk(count)
6000  continue
      return
c
c------------------------------< input error
c
120   continue
      print 130
130   format(1x,'input error, try again')
      go to 9999
140   continue
      print 150
150   format(1x,'each side must have 1 king(k)')
      go to 9999
      end


