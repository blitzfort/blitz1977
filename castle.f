      subroutine castle
c
c     ******************************************************************
c     *                                                                *
c     *      castle is used to generate all castle type moves,         *
c     *  castle only generates legal moves, unlike the regular move    *
c     *  generators which may generate moves leaving the king in       *
c     *  check.  subroutine 'attack' is used to make sure that the     *
c     *  king will not pass over a square attacked by the enemy when   *
c     *  performing the castle move.  it also makes sure that the      *
c     *  king and rook(s) haven't moved rendering castling illegal.    *
c     *                                                                *
c     ******************************************************************
c
      implicit integer (a-z)
      logical attack, pmoved, moved, in chk
      common /board/ board(120)
      common /tree/ moves(2000), first(30), last(30), which(30),
     *              in chk(30), giv chk(30)
      common /castcm/ pmoved(3,2), moved(3,30)
      common /move cm/ side, player, square, mpiece
      common /types/ normal, castkg, castqn, enpass, promot
      common /depth/ sdepth, depth, ply
      common /colr cm/ color
      common /cbias/ cbias(2)
c
c------------------------------< initialize.
c
      bias=cbias(player)
c
c------------------------------< if in check, castling is ille6al.
c
      if(in chk(ply)) go to 9999
c
c------------------------------< if the king has moved, castling is
c------------------------------< not legal.
c
1000  continue
      if(moved(1,ply)) go to 9999
      if(color .eq. 1) go to 1100
c
c------------------------------< program is black. generate queen-side
c------------------------------< castling moves.
c
      if(moved(2,ply)) go to 1010
      if(board(bias+22) .ne. 4*side)  go to 1010
      if(board(bias+23).ne.0 .or. board(bias+24).ne.0
     *                      .or. board(bias+25).ne.0) go to 1010
      if(attack(-side,bias+24))  go to 1010
      if(attack(-side,bias+25))  go to 1010
          last(ply)=last(ply)+1
          moves(last(ply))=castqn*65536
1010  continue
c
c------------------------------< program is black. generate king-side
c------------------------------< castling moves.
c
      if(moved(3,ply)) go to 9999
      if(board(bias+29) .ne. 4*side) go to 9999
      if(board(bias+27).ne.0 .or. board(bias+28).ne.0) go to 9999
      if(attack(-side,bias+27))  go to 1020
      if(attack(-side,bias+28))  go to 1020
          last(ply)=last(ply)+1
          moves(last(ply))=castkg*66536
1020  continue
      go to 9999
c
c------------------------------< program is white, generate king-side
c------------------------------< castling moves.
c
1100  continue
      if(moved(2,ply)) go to 1110
      if(board(bias+22) .ne. 4*side) go to 1110
      if(board(bias+23).ne.0 .or. board(bias+24).ne.0) go to 1110
      if(attack(-side,bias+23))  go to 1110
      if(attack(-side,bias+24))  go to 1110
          last(ply)=last(ply)+1
          moves(last(ply))=castkg*65536
1110  continue
c
c------------------------------< program is white. generate queen-side
c------------------------------< castling moves.
c
      if(moved(3,ply)) go to 9999
      if(board(bias+29) .ne. 4*side) go to 9999
      if(board(bias+26).ne.0 .or. board(bias+27).ne.0
     *                      .or.  board(bias+28).ne.0) go to 9999
      if(attack(-side,bias+26))  go to 1120
      if(attack(-side,bias+27))  go to 1120
          last(ply)=last(ply)+1
          moves(last(ply))=castqn*66536
1120  continue
9999  return
      end


