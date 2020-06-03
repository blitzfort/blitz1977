      subroutine rcmnd
c
c     ******************************************************************
c     *                                                                *
c     *      rcmnd is used to process the 'r' command which can        *
c     *  reset the board to the position after any prior move.         *
c     *  the board is always reset to the position for the move        *
c     *  indicated, so that it is the opponent's turn to move.  for    *
c     *  example, if the computer is white, and you wish to back       *
c     *  up to move 15, the move for black listed under move 15        *
c     *  would not be made.                                            *
c     *      'rk' is an alternative way to set the board to            *
c     *  some position.  in this mode, you can key moves in            *
c     *  and have them made as you go.  you can terminate the          *
c     *  moves with an end of file, or by typing in the number         *
c     *  of moves you indicated would be entered.                      *
c     *                                                                *
c     ******************************************************************
c
      implicit integer (a-z)
      logical in book
      logical over
      logical keyin, repchk, rtemp
      common /buffer/ text(80)
      common /mov cnt/ npmovs, nomovs
      common /colr cm/ color
      common /depth/ sdepth, depth, ply
      common /trcecm/ strace(32)
      common /l move/ lmovep, lmoveo
      common /info/ from$, to$, type$, propc$,  cappc$
      common /predcm/ ptext(30), pmove, ptype$,  pfrom$,  pto$
      common /bookcm/ in book, key
      common /over/ over
      common /move cm/ side, player, square, mpiece
      common /dup/ bdsave(1040), point
      common /timecm/ gmoves, gelap, smoves,  selap, surpls,  cputim,
     *                 cquery, pelap, oelap, psec1,  psec2, osec1,
     *                 osec2, avgtim, expect,  fsec1
      common /chr set/ alpha(46)
      common /prev mv/ prev mv(6)
      common /return/ return
      equivalence (k,alpha(11)),(y,alpha(25)),(blank,alpha(44))
      data zero / 0 /
c
c------------------------------< initialize
c
      keyin=.false.
      if(text(2) .eq. k) keyin=.true.
      temp=3
      if(text(3) .eq. blank) temp=4
      moveno=scanin(temp)
      if(moveno .eq. 0) return
      in book=.false.
      over=.false.
      do 3 i=1,1040
          bdsave(i)=0
3     continue
      player=1
      if(color .eq. 1) player=2
      side=1
      if(player .eq. 2) side=-1
      point=0
      pelap=0
      oelap=0
c
c------------------------------< determine exactly how many moves
c------------------------------< to process.  leave the opponent's
c------------------------------< last move out so it will be his turn.
c
      npmovs=moveno-1
      nomovs=moveno-1
      if(color .eq. 1) npmovs=npmovs+1
      moveno=(moveno-1)*2
      if(color .eq. 1) moveno=moveno+1
      call set gb(.false.)
      count=0
c
c------------------------------< now, either read the move file or
c------------------------------< from the user the list of moves
c------------------------------< to make.
c
30    continue
          player=2-mod(player+1,2)
          ply=player
          side=-side
          count=count+1
          time=0
          if(count .gt. moveno) go to 45
          if(keyin) go to 35
              read(1'count) (text(ll),ll=1,30), time
              go to 38
35        continue
          if(mod(count,2) .eq. 0) go to 36
              n=count/2+1
              print 37,n
37            format(1x,'move',i3)
36        continue
          read(5,100,end=45) text
100       format(80a1)
          write(1,count) (text(ll),ll=1,30), zero
c
c------------------------------< decode the move and make it on
c------------------------------< the board.
c
38        continue
          if(side .eq. 1) pelap=pelap+time
          if(side .eq. -1) oelap=oelap+time
          call input(.true.)
          if(return .ne. 0) go to 35
          prevmv(1+(player-1)*3)=from$
          prevmv(2+(player-1)*3)=to$
          prevmv(3+(player-1)*3)=type$
          call pmover
          ply=0
          point=point+1
          if(point .gt. 130) point=1
          rtemp=repchk(dummy)
      go to 30
45    continue
      lmovep=from$+to$*256+type$*65536+cappc$*16777216
      lmovep=lmovep+propc$*1048576
      lmoveo=0
      do 20 i=1,32
          strace(i)=0
20    continue
      pmove=0
      return
      end




