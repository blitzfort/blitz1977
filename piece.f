      subroutine piece
c
c     ******************************************************************
c     *                                                                *
c     *      piece is used to generate all piece moves (excluding      *
c     *  pawn moves and castling which are special cases handled       *
c     *  elsewhere).  the array containing the move directions is      *
c     *  used to generate the destination squares.  the moves are      *
c     *  checked to make sure that a friendly piece is not being       *
c     *  captured.                                                     *
c     *      if a move leaves the king in check, this will be found    *
c     *  as the search progresses deeper when the king is captured.    *
c     *  at this point the previous move will be rejected as illegal.  *
c     *  since the underlying idea of this program is speed, this      *
c     *  seems to be faster than checking for 'in check' after each    *
c     *  move is generated since only a small percentage of moves      *
c     *  actually leave the king in check.                             *
c     *                                                                *
c     ******************************************************************
c
      implicit integer (a-z)
      logical in chk, giv chk
      common /board/ board(120)
      common /tree/ moves(2000), first(30), last(30), which(30),
     *              in chk(30), giv chk(30)
      common /depth/ sdepth, depth, ply
      common /movdir/ movdir(28), piecem(28), begin(6), end(6)
      common /move cm/ side, player, square, mpiece
      common /types/ normal, castkg, castqn, enpass, promot
      common /return/ return
c
c------------------------------< initialze.
c
      sq1=begin(mpiece)
      sq2=end(mpiece)
c
c------------------------------< begin iteration to generate all legal
c------------------------------< moves from 'square'.
c
      do 3000 i=sq1,sq2
          tosq=square
1000      continue
              tosq=tosq+movdir(i)
c
c------------------------------< make sure the piece is not capturing a
c------------------------------< friendly piece or moving off the edge
c------------------------------< of the board.
c
              cpiece=-side*board(tosq)
              if(cpiece .lt. 0) go to 3000
              if(cpiece .gt. 6) go to 3000
              if(cpiece .eq. 6) go to 9998
              if(ply.gt.depth  .and.
     *               cpiece.eq.0  .and.
     *                   .not. in chk(ply) .and.
     *                       .not. giv chk(ply)) go to 2000
c
c------------------------------< this move is legal. enter it into
c------------------------------< the move list for consideration
c
                  last(ply)=last(ply)+1
                  moves(last(ply))=square+tosq*256+normal*65536
     *                                            +cpiece*16777216
2000              continue
                  if(sq1 .gt. 8) go to 3000
          if(cpiece .eq. 0) go to 1000
3000  continue
c
c------------------------------< move generation for the piece on
c------------------------------< 'square' is complete. return.
c
      return=0
      return
c
c------------------------------< we're capturing the opponent's king.
c------------------------------< the move at the previous level was
c------------------------------< illegal, return 1 to reject it.
c
9998  continue
      return=1
      return
      end


