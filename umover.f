      subroutine umover
c
c     ******************************************************************
c     *                                                                *
c     *      umover is used to unmake allmoves on the game board.     *
c     *  it restores the board to the position before the current      *
c     *  move was made.                                                *
c     *                                                                *
c     ******************************************************************
c
c
c    type$:  1 => normalmove
c            2 => castle king-side
c            3 => castle queen-side
c            4 =) en passant pawn capture
c            5 => pawn promotton move
c
      implicit integer (a-z)
      logical pmoved, moved
      common /board/ board(120)
      common /colrcm/ color
      common /depth/ sdepth, depth, ply
      common /info/ from$, to$, type$, propc$, cappc$
      common /movecm/ side, player, square, mpiece
      common /castcm/ pmoved(3,2), moved(3,30)
      common /srchcm/ value(30), from(30), to(30), type(30), cappc(30)
      common /cbias/ cbias(2)
      common /pieccm/ pieces(6)
      common /mscore/ sscore, mscore, pscore, tscore
c
c------------------------------< initialize.
c
      bias=cbias(player)
c
c------------------------------< status for plies 3-n is passed down
c------------------------------< from previous plies.
c
      if(pmoved(1,player)) go to 200
          if(ply.le.2) go to 100
          do 10 i=1,3
              moved(i,ply)=moved(i,ply-2)
10        continue
          go to 200
c
c------------------------------< status for the first 2 plies comes
c------------------------------< from the premanent status
c
100       continue
          do 20 i=1,3
              moved(i,ply)=pmoved(i,ply)
20        continue
200   continue
      go to (1,2,4,6,7),type$
c
c------------------------------< retract normalmove
c
1     continue
      board(from$)=board(to$)
      board(to$)=cappc(ply)
      cappc$=iabs(cappc(ply))
      if(cappc$ .eq. 0) return
          val=pieces(cappc$)
          mscore=mscore-side*val
          if(cappc$ .eq. 1) return
              sscore=sscore-val
              return
c
c------------------------------< retract castle king-side
c
2     continue
      if(color .ne. 1) go to 3
          board(bias+22)=4*side
          board(bias+23)=0
          board(bias+24)=0
          board(bias+25)=6*side
          return
3     continue
          board(bias+26)=6*side
          board(bias+27)=0
          board(bias+28)=0
          board(bias+29)=4*side
          return
c
c------------------------------< retract castle queen-side
c
4     continue
      if(color .ne. 1) go to 5
          board(bias+25)=6*side
          board(bias+26)=0
          board(bias+27)=0
          board(bias+29)=4*side
          return
5     continue
          board(bias+22)=4*side
          board(bias+24)=0
          board(bias+25)=0
          board(bias+26)=6*side
          return
c
c------------------------------< retract en passant pawn capture
c
6     continue
      board(from$)=board(to$)
      board(to$)=0
      board(to$-10*side)=-1*side
      mscore=mscore-side*pieces(1)
      return
c
c------------------------------< retract pawn promotion
c
7     continue
      propc$=iabs(board(to$))
      board(to$)=1*side
      mscore=mscore-side*(pieces(propc$)-pieces(1))
      go to 1
      end

