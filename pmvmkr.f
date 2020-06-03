      subroutine pmvmkr
c
c     ******************************************************************
c     *                                                                *
c     *      pmvmkr is used to make moves on the game board and to     *
c     *  set the premenant castling status if the king or rooks are    *
c     *  moved.  it is only called to make moves immediately after     *
c     *  they are actually chosen, not during the minimax search.      *
c     *                                                                *
c     ******************************************************************
c
      implicit integer (a-z)
      logical pmoved, moved, pmsave
      common /savecm/ sboard(100), pmsave(3,2)
      common /info/ from$, to$, type$, propc$, cappc$
      common /castcm/ pmoved(3,2), moved(3,30)
      common /types/ normal, castkg, castqn, enpass, promot
      common /depth/ sdepth, depth, ply
      common /cbias/ cbias(2)
      common /board/ board(120)
c
c------------------------------< permanantly unmake a move. first,
c------------------------------< restore the castling status.
c
      entry pumver
      do 1 i=1,2
          do 2 j=1,3
              pmoved(j,i)=pmsave(j,i)
2         continue
1     continue
c
c------------------------------< then, unmake the move.
c
10    continue
      call umover
      return
c
c------------------------------< permanantly make a move. first,
c------------------------------< save the castling status.
c
      entry pmover
      do 3 i=1,2
          do 4 j=1,3
              pmsave(j,i)=pmoved(j,i)
4         continue
3     continue
c
c------------------------------< make the move.
c
      call mover
c
c------------------------------< then determine what castling status
c------------------------------< has changed and set the appropriate
c------------------------------< status flags.
c
      if(type$.eq.castkg.or.type$.eq.castqn) go to 30
          bias=cbias(ply)
          if(from$.eq.bias+22) pmoved(2,ply)=.true.
          if(from$.eq.bias+29) pmoved(3,ply)=.true.
          if(iabs(board(to$)) .eq. 6) go to 30
          if(pmoved(2,ply) .and. pmoved(3,ply)) go to 30
          return
30    continue
      pmoved(1,ply)=.true.
      return
      end



