      subroutine mover
c
c     ******************************************************************
c     *                                                                *
c     *      mover is used to make all moves on the game board,  it    *
c     *  saves the value of the 'to' square before making a move so    *
c     *  that the move can be 'unmade' later.                          *
c     *                                                                *
c     ******************************************************************
c
c
c    type*:  1 => normal mqve
c            2 => castle king-side
c            3 => ca8tle queen-side
c            4 => en passant pawn capture
c            5 => pawn promotion move
c
      implicit integer (a-z)
      logical pmoved, moved
      common /board/ board(120)
      common /colr cm/ color
      common /depth/ sdepth, depth, ply
      common /info/ from$, to$, type$, propc$, cappc$
      common /move cm/ side, player, square, mpiece
      common /castcm/ pmoved(3,2), moved(3,30)
      common /srchcm/ value(30), from(30), to(30), type(30), cappc(30)
      common /cbias/ cbias(2)
      common /mscore/ sscore, mscore, pscore, tscore
      common /piec cm/ pieces(6)
c
c------------------------------< initialize.
c
      bias=cbias(player)
      go to (1,2,4,6,7),type$
c
c------------------------------< normal moves
c
1     continue
      cappc(ply)=board(to$)
      board(to$)=board(from$)
      board(from$)=0
      if(board(to$)*side .eq. 6) moved(1,ply)=.true.
      if(from$ .eq. bias+22) moved(2,ply)=.true.
      if(from$ .eq. bias+29) moved(3,ply)=.true.
      moved(1,ply)=moved(1,ply) .or. (moved(2,ply).and.moved(3,ply))
      if(cappc$ .eq. 0) return
          val=pieces(cappc$)
          mscore=mscore+side*val
          if(cappc$ .eq. 1) return
              sscore=sscore+val
              return
c
c------------------------------<  castle king-side
c
2     continue
      cappc(ply)=0
      moved(1,ply)=.true.
      if(color .ne. 1) go to 3
          board(bias+22)=0
          board(bias+23)=6*side
          board(bias+24)=4*side
          board(bias+25)=0
          return
3     continue
          board(bias+26)=0
          board(bias+27)=4*side
          board(bias+28)=6*side
          board(bias+29)=0
          return
c
c------------------------------< castle queen-side
c
4     continue
      cappc(ply)=0
      moved(1,ply)=.true.
      if(color .ne. 1) go to 5
          board(bias+25)=0
          board(bias+26)=4*side
          board(bias+27)=6*side
          board(bias+29)=0
          return
5     continue
          board(bias+22)=0
          board(bias+24)=6*side
          board(bias+25)=4*side
          board(bias+26)=0
          return
c
c------------------------------< en passant pawn capture
c
6     continue
      board(to$)=board(from$)
      board(from$)=0
      board(to$-10*side)=0
      cappc(ply)=-1*side
      mscore=mscore+side*pieces(1)
      return
c
c------------------------------< pawn promotion
c
7     continue
      board(from$)=propc$*side
      mscore=mscore+side*(pieces(propc$)-pieces(1))
      go to 1
      end



