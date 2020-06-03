      subroutine movgen
c
c     ******************************************************************
c     *                                                                *
c     *      movgen is used to generate all moves.  it acts as a       *
c     *  driver for the individual move generators.  the entire board  *
c     *  is scanned and as a piece for the side on move is detected,   *
c     *  the correct generator is called.                              *
c     *                                                                *
c     ******************************************************************
c
      implicit integer (a-z)
      logical pmoved, moved, in chk, giv chk
      common /board/ board(120)
      common /tree/ moves(2000), first(30), last(30), which(30),
     *              in chk(30), giv chk(30)
      common /move cm/ side, player, square, mpiece
      common /depth/ sdepth, depth, ply
      common /castcm/ pmoved(3,2), moved(3,30)
      common /return/ return
c
c------------------------------< initialize.
c
      last(ply)=first(ply)-1
c
c------------------------------< status for plies 3-n is passed down
c------------------------------< from previous plies.
c
      if(pmoved(1,player)) go to 40
          if(ply.le.2) go to 20
              do 10 i=1,3
                  moved(i,ply)=moved(i,ply-2)
10            continue
              go to 40
20        continue
c
c------------------------------< status for the first 2 plies comes
c------------------------------< from the premanant status.
c
          do 30 i=1,3
              moved(i,ply)=pmoved(i,ply)
30        continue
40    continue
c
c------------------------------< scan the entire board to find all
c------------------------------< pieces of the side to move.  gen-
c------------------------------< erate and score all moves for them.
c
      do 2000 square=22,99
          mpiece=side*board(square)
          if(mpiece .le. 0) go to 2000
          if(mpiece .gt. 6) go to 2000
          if(mpiece .gt. 1) go to 1000
c
c------------------------------< generate pawn moves.
c
          call pawn
          if(return .ne. 0) go to 9999
          go to 2000
c
c------------------------------< generate piece moves.
c
1000      continue
          call piece
          if(return .ne. 0) go to 9999
2000  continue
c
c------------------------------< generate castling moves.
c
      if(ply .gt. depth) go to 9999
          if(.not. pmoved(1,player)) call castle
9999  continue
      return
      end

