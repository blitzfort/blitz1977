      subroutine forced
c
c     ******************************************************************
c     *                                                                *
c     *      forced is used to determine if the opponent's move is     *
c     *  forced (he has only one legalmove).  if it is, the move will *
c     *  be made without any action required from the operator.  the   *
c     *  move will be printed for the operator just as if he had       *
c     *  typed it himself.  the move will have the '(forced)' legend   *
c     *  attached to indicate the forced nature of the move.           *
c     *                                                                *
c     ******************************************************************
c
      implicit integer (a-z)
      logical check
      common /board/ board(120)
      common /tree/ moves(2000), first(30), last(30), which(30),
     *              inchk(30), givchk(30)
ccccc bug.  the arrays are supposed to be 30.
ccccc      common /srchcm/ value(30), from(20), to(20), type(20), cappc(20)
      common /srchcm/ value(30), from(30), to(30), type(30), cappc(30)
      common /info/ from$, to$, type$, propc$, cappc$
      common /movecm/ side, player, square, mpiece
      common /depth/ sdepth, depth, ply
      common /lmove/ lmovep, lmoveo
      common /buffer/ text(80)
      common /namecm/ name(5)
      common /chrset/ alpha(46)
      equivalence (blank,alpha(44)),(alphaf,alpha(6)),
     * (alphao,alpha(15)),(alphar,alpha(18)),(alphac,alpha(3)),
     * (alphae,alpha(5)),(alphad,alpha(4))
      common /return/ return
      data lparen, rparen / '(', ')' /
c
c------------------------------< determine if the opponent has only one
c------------------------------< legalmove to make
c
      from$=lmovep
      if(from$ .eq. 0) go to 9999
      call extrct
      from(1)=from$
      to(1)=to$
      ply=2
      player=2
      side=-1
      first(2)=1
      depth=ply+1
      call movgen
c
c------------------------------< determine if there is only one
c------------------------------< legalmove.
c
      nmoves=0
      end=last(2)
      do 7 where=1,end
          from$=moves(where)
          call extrct
          callmover
          if(check(-1)) go to 6
              nmoves=nmoves+1
              good=where
6         continue
          call umover
7     continue
      if(nmoves .ne. 1) go to 9999
c
c------------------------------< the opponent has only one legalmove.
c------------------------------< print it as if he had entered it with the
c------------------------------< (forced) legend and return skipping
c------------------------------< the move input read
c
      from$=moves(good)
      call extrct
      mtype=0
      callmover
      if(check(1)) mtype=1
      call umover
      call output(mtype,board,.true.)
c
c------------------------------< add the (forced) indicator to the
c------------------------------< end of the move text.
c
      do 10 i=1,30
          if(text(31-i).ne.blank) go to 20
10    continue
20    continue
      i=33-i
      text(i)=lparen
      text(i+1)=alphaf
      text(i+2)=alphao
      text(i+3)=alphar
      text(i+4)=alphac
      text(i+5)=alphae
      text(i+6)=alphad
      text(i+7)=rparen
      print 30,name,(text(i),i=1,30)
30    format(1x,'your move, ',5a4/'?',30a1)
      return=1
      return
9999  continue
      return=0
      return
      end


