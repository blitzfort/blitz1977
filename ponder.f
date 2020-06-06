      subroutine ponder
c
c     ******************************************************************
c     *                                                                *
c     *      ponder is the driver for the think on the opponent's time *
c     *  algorithm.  briefly, ponder takes the predicted move from the *
c     *  previous search and makes it.  then driver is called to find  *
c     *  a response to that move.  this response is saved, after the   *
c     *  opponent's move is entered, match determines if the move is   *
c     *  the same as the expected move.  if so, the response is ready  *
c     *  and no search is required.  when the program is pondering in  *
c     *  this mode, it is necessary to hit the break before a move     *
c     *  can be entered since the program ponders continuously and     *
c     *  does not hang a read to the terminal.  when the break key is  *
c     *  hit, two cases arise:                                         *
c     *                                                                *
c     *      1)  if a command or move is entered, the entire search    *
c     *          must be aborted to get back to main to interpret the  *
c     *          command;                                              *
c     *                                                                *
c     *      2)  the characters 'ok' can be entered which indicated    *
c     *          that the opponent has moved and the move matches the  *
c     *          expected or predicted move.  in this, case the        *
c     *          opponent's clock is stopped, the program's clock is   *
c     *          started and the search continues without interruption *
c     *          saving the time used already.  the move can be        *
c     *          entered exactly as printed in the 'pondering a        *
c     *          reply to...' message with the same results.           *
c     *                                                                *
c     ******************************************************************
c
      implicit integer (a-z)
      logical cputim, cquery, temp, repchk
      logical tmode, smode, pndrng, foundm, matchd
      logical inbook, autos, eboard, englsh
      logical check
      logical abort
      common /timecm/ gmoves, gelap, smoves, secap, surpls, cputim,
     *                 cquery, pelap, oelap, psec1, psec2, osec1,
     *                 osec2, avgtim, expect, fsec1
      common /mode/ tmode, smode, pndrng, foundm, matchd
      common /predcm/ ptext(30), pmove, ptype$, pfrom$, pto$
      common /depth/ sdepth, depth, ply
      common /bookcm/ inbook, key
      common /buffer/ text(80)
      common /board/ board(120)
      common /info/ from$, to$, type$,  propc$,  cappc$
      common /movecm/ side, player, square, mpiece
      common /prevmv/ prevmv(6)
      common /abort/ abort
      common /autos/ autos
      common /eboard/ eboard
      common /typntn/ englsh
      common /dup/ bdsave(1040), point
      common /movcnt/ npmovs, nomovs
      common /chrset/ alpha(46)
      equivalence (o,alpha(15)),(colon,alpha(45)),(dash,alpha(38))
      common /return/ return
c
c------------------------------< if still in the book or not in
c------------------------------< tournament mode, return. if there is
c------------------------------< no predicted move or ponder has already
c------------------------------< found a move, return also.
c
      if(foundm .or. inbook) go to 9999
      if(pmove.eq.0 .or. .not.t mode) go to 9999
c
c------------------------------< remember predicted move
c
      from$=pmove
      call extrct
      matchd=.false.
      prevmv(4)=from$
      prevmv(5)=to$
      prevmv(6)=type$
      ply=2
      side=-1
      player=2
      mtype=0
      callmover
      if(check(1)) mtype=1
      call umover
      call output(mtype,board,.true.)
      do 200 i=1,30
          ptext(i)=text(i)
200   continue
      if(t mode) print 210, ptext
210   format(/1x,'pondering a reply to ',30a1/)
      if(.not. eboard) go to 250
          temp=englsh
          englsh=.false.
          call output(0,board,.false.)
          englsh=temp
          lim1=2
          lim2=6
          if(text(1) .ne. o) go to 220
              lim1=1
              lim2=3
              if(text(5) .eq. o) lim2=5
220       continue
          sub=16
          do 230 i=lim1,lim2
              ptext(sub)=text(i)
              sub=sub+1
230       continue
250   continue
      call save gb
      call pmover
c
c------------------------------< actual pondering starts here.
c
      point=point+1
      if(point .gt. 130) point=1
      nomovs=nomovs+1
      ply=0
      temp=repchk(count)
      pndrng=.true.
      psec1=time()
      call driver
      point=point-1
      if(point .lt. 1) point=130
      nomovs=nomovs-1
      if(abort .and. .not.matchd) go to 100
      psec2=time()
      foundm=.true.
      call rest gb
      pndrng=.false.
      if(matchd) go to 300
      go to 9999
c
c------------------------------< operator hit break and did not
c------------------------------< type in a fully qualified move
c------------------------------< (or he typed in a command) so
c------------------------------< abort the look-ahead and see
c------------------------------< what he has on his mind.
c
100   continue
      foundm=.false.
      pndrng=.false.
      call rest gb
      return=1
      return
c
c------------------------------< the operator typed 'ok' which indicates
c------------------------------< that the program correctly predicted
c------------------------------< the opponent's reply. set the input
c------------------------------< buffer so that the operator won't have
c------------------------------< to enter the move.
c
300   continue
      from$=pmove
      call extrct
      do 310 i=1,30
          text(i)=ptext(i)
310   continue
      return=2
      return
9999  continue
      return=0
      return
      end



