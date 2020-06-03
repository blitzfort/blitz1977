      subroutine fcmnd
c
c     ******************************************************************
c     *                                                                *
c     *      fcmnd is used to force blitz to make a specific move      *
c     *  that the normal search or book search would not choose.  it   *
c     *  prompts the operator for the move to make and if it is legal  *
c     *  will actually make it on the game board as though blitz had   *
c     *  chosen it all along.  the move actually last made by blitz    *
c     *  will naturally be retracted since a new move is being made.   *
c     *                                                                *
c     ******************************************************************
c
      implicit integer (a-z)
      logical check, in book, repchk, rtemp, over
      common /colr cm/ color
      common /buffer/ text(80)
      common /mov cnt/ npmovs, nomovs
      common /info/ from$, to$, type$, propc$, cappc$
      common /predcm/ ptext(30), pmove, ptype$, prom$, pto$
      common /types/ normal, castkg, castqn, enpass, promot
      common /move cm/ side, player, square, mpiece
      common /tree/ moves(2000), first(30), last(30), which(30),
     *              in chk(30), giv chk(30)
      common /srchcm/ value(30), from(30), to(30), type(30), cappc(30)
      common /namecm/ name(5)
      common /prev mv/ prevmv(6)
      common /depth/ sdepth, depth, ply
      common /trcecm/ strace(32)
      common /l move/ lmovep, lmoveo
      common /bookcm/ in book, key
      common /chrset/ alpha(46)
      equivalence (blank,alpha(44))
      common /over/ over
      common /return/ return
      data zero /0/
c
c------------------------------< input new move to make
c
10    continue
      print 20
20    format(1x,'enter my move')
      call read
      if(text(1) .eq. blank) return
c
c------------------------------< if program has already made a move,
c------------------------------< it must be 'unmade'
c
      ply=1
      player=1
      side=1
      from$=lmovep
      if(from$ .eq. 0) go to 30
          call extrct
          cappc(1)=-cappc$
          call pumver
30    continue
      do 40 i=1,32
          strace(i)=0
40    continue
      pmove=0
c
c------------------------------< convert and make the new move
c
      call input(.true.)
      if(return .ne. 0) go to 10
      lmovep=from$+to$*256+type$*65536+cappc$*16777216
      lmovep=lmovep+propc$*1048576
      prevmv(1)=from$
      prevmv(2)=to$
      prevmv(3)=type$
      over=.false.
      call pmover
      ply=0
      rtemp=repchk(count)
      if(check(1)) go to 80
      nmoves=npmovs+nomovs
      write(1,nmoves) (text(l),l=1,30), zero
c
c------------------------------< determine if this forced move is
c------------------------------< still part of a known book line. if
c------------------------------< still 'in book', get the pointer to
c------------------------------< the list of responses.
c
      if(.not. in book) return
      key=key/10000
      read(3'key)(moves(i),i=1,40)
      fromsq=from$
      tosq=to$
      typemv=type$
      if(color .eq. 1) go to 50
          fromsq=121-fromsq
          tosq=121-tosq
50    continue
      do 60,i=1,20
          from$=moves(i)
          call extrct
          if(typemv .ne. type$) go to 60
          if(typemv.eq.castkg .or. typemv.eq.castqn) go to 70
          if(from$.eq.fromsq .and. to$.eq.tosq) go to 70
60    continue
      in book=.false.
      return
70    continue
      key=key*10000+moves(i+20)
      return
c
c------------------------------< can't make a move that leaves
c------------------------------< the king in check
c
80    continue
      print 90,name
90    format(1x,'my king is in check, ',5a4)
      go to 10
      end


