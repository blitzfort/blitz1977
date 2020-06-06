      subroutine killer(move)
c
c     ******************************************************************
c     *                                                                *
c     *      killer is used to maintain the 'killer' move list which   *
c     *  is usec by phase 4 move selection.  basically, a killer       *
c     *  move is either one of two possibilities:                      *
c     *                                                                *
c     *      1)  it is the best move found at this level after the     *
c     *          other side made some move from the same parent        *
c     *          position;                                             *
c     *                                                                *
c     *      2)  or it is a move that refutes the move made by the     *
c     *          other side from the same parent position.             *
c     *                                                                *
c     *      the purpose of 'killer' moves is to provide a good move   *
c     *  early in the move list in order to maximize the number of     *
c     *  alpha/beta cutoffs.  it is based on the principle that most   *
c     *  moves from a parent position can be refuted by the same       *
c     *  move; whether the move is a capture, a fork, or simply a      *
c     *  strong positionalmove such as claimimg an open file.  this   *
c     *  list will have up to ten such moves to help the program       *
c     *  'learn' as it progresses through the search.                  *
c     *                                                                *
c     ******************************************************************
c
      implicit integer (a-z)
      logical done
      common /info/ from$, to$, type$, propc$, cappc$
      common /srchcm/ value(30), from(30), to(30), type(30), cappc(30)
      common /depth/ sdepth, depth, ply
      common /movecm/ side, player, square, mpiece
      common /killmv/ killmv(20,30)
      common /window/ window(2)
c
c------------------------------< remember the move causing the curent
c------------------------------< cutoff as a 'killer' move.  these moves
c------------------------------< will be tried first each time a move
c------------------------------< is examined from the current ply.
c------------------------------< don't enter a move that captures the
c------------------------------< last piece moved since it probably
c------------------------------< only refutes that move.
c
      if(move .eq. 0) go to 9999
      from$=move
      call extrct
      if(cappc$ .eq. 0) go to 100
          if(to$ .eq. to(ply-1)) go to 9999
100   continue
c
c------------------------------< first, check to see if the current
c------------------------------< move is already in the 'killer' list.
c
      if(value(ply) .eq. window(player)) go to 9999
      do 200 where=1,10
          if(killmv(where,ply) .eq. move) go to 300
200   continue
c
c------------------------------< it is not in the 'killer' list.  find
c------------------------------< the least used entry to replace with
c------------------------------< the current move.
c
      where=10
      killmv(where,ply)=move
      killmv(where+10,ply)=1
      go to 400
c
c------------------------------< the current move is alredy in the
c------------------------------< 'killer' list.  bump it's popularity
c------------------------------< count so that it will not be lost
c------------------------------< easily.
c
300   continue
      killmv(where+10,ply)=killmv(where+10,ply)+1
400   continue
          if(where .le. 1) go to 9999
          if(killmv(where+10,ply) .le. killmv(where+9,ply)) go to 9999
              temp=killmv(where,ply)
              killmv(where,ply)=killmv(where-1,ply)
              killmv(where-1,ply)=temp
              temp=killmv(where+10,ply)
              killmv(where+10,ply)=killmv(where+9,ply)
              killmv(where+9,ply)=temp
              where=where-1
              go to 400
9999  continue
      return
      end


