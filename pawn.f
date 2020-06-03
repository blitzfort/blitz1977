      subroutine pawn
c
c     ******************************************************************
c     *                                                                *
c     *      pawn is used to generate all pawn moves.  the array       *
c     *  containing the move directions is used to generate the        *
c     *  destination squares.  the moves are check to make sure that   *
c     *  a friendly piece is not being captured.                       *
c     *      en passant captures are generated based on the previous   *
c     *  move.  pawn promotion moves cause four (4) moves to be        *
c     *  generated:  promotion to queen, rook, bishiop and knight      *
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
      common /srchcm/ value(30), from(30), to(30), type(30), cappc(30)
      common /depth/ sdepth, depth, ply
      common /movdir/ movdir(28), piecem(28), begin(6), end(6)
      common /prev mv/ prevmv(6)
      common /move cm/ side, player, square,  mpiece
      common /types/ normal, castkg, castqn, enpass, promot
      common /return/ return
c
c------------------------------< initialize.
c
      lim=27
      if(side.gt.0 .and. square.lt.40) lim=28
      if(side.lt.0 .and. square.gt.80) lim=28
      if(ply.le.depth .or. in chk(ply) .or. giv chk(ply)) go to 10
          lim=26
          if(side.gt.0 .and. square.gt.80) lim=27
          if(side.lt.0 .and. square.lt.40) lim=27
10    continue
      if(ply .gt. 1) go to 40
          tlast=prevmv(5)
          flast=prevmv(4)
          go to 50
40    continue
      tlast=to(ply-1)
      flast=from(ply-1)
50    continue
c
c------------------------------< begin iteration to generate all legal
c------------------------------< moves from 'square'.
c
      do 2000 i=25,lim
          tosq=square+movdir(i)*side
          cpiece=-side*board(tosq)
          if(cpiece .lt. 0) go to 1000
          if(cpiece .gt. 6) go to 2000
c
c------------------------------< process capturing moves.
c
          if(i .gt. 26) go to 1010
          if(cpiece .eq. 6) go to 3000
          if(cpiece .ne. 0) go to 1020
c
c------------------------------< process 'en passant' captures.
c
          if(board(tlast) .ne. -side) go to 2000
          if(flast .ne. tosq+10*side) go to 2000
          if(tlast .ne. tosq-10*side) go to 2000
              last(ply)=last(ply)+1
              moves(last(ply))=square+tosq*256+enpass*65536+16777216
              go to 2000
c
c------------------------------< process regular moves.
c
1010      continue
          if(cpiece .ne. 0) go to 2100
1020      continue
c
c------------------------------< move is legal.  now determine if it
c------------------------------< is a promotion type move.
c
          if(side .lt. 0) go to 1030
          if(tosq .gt. 90) go to 1040
          go to 1050
1030      continue
          if(tosq .gt. 30) go to 1050
1040      continue
c
c------------------------------< move is a promotion type move, enter
c------------------------------< all four (f) promotions into the move
c------------------------------< list for further consideration.
c
          temp=square+tosq*256+promot*65536+cpiece*16777216
          moves(last(ply)+1)=temp+5*1048576
          moves(last(ply)+2)=temp+4*1048576
          moves(last(ply)+3)=temp+3*1048576
          moves(last(ply)+4)=temp+2*1048576
          last(ply)=last(ply)+4
          go to 2000
c
c------------------------------< move is normal type move.  enter it
c------------------------------< into the move list for further
c------------------------------< consideration
c
1050      continue
          last(ply)=last(ply)+1
          moves(last(ply))=square+tosq*256+normal*65536+cpiece*16777216
          go to 2000
1000      continue
          if(i .ge. 27) go to 2100
2000  continue
2100  continue
      return=0
      return
c
c------------------------------< we're capturing the opponent's king
c------------------------------< the move at the previous level is
c------------------------------< illegal.  return 1 to reject it.
c
3000  continue
      return=1
      return
      end


