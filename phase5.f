      subroutine phase5
c
c     ******************************************************************
c     *                                                                *
c     *      phase 5 is used to select all moves that are left in      *
c     *  the move list after phases 1, 2, 3 and 4 have been            *
c     *  executed.                                                     *
c     *                                                                *
c     ******************************************************************
c
      implicit integer (a-z)
      logical mated, in chk, giv chk
      logical checks
      logical check, ctemp
      common /tree/ moves(2000), first(30), last(30), which(30),
     *              in chk(30),giv chk(30)
      common /srchcm/ value(30), from(30), to(30), type(30), cappc(30)
      common /phas cm/ phase(30), status(30)
      common /depth/ sdepth, depth, ply
      common /info/ from$, to$, type$, propc$, cappc$
      common /move cm/ side, player, square, mpiece
      common /mated/ mated(30)
      common /mscore/ sscore, mscore, pscore, tscore
      common /minmax/ minmax(2)
      common /return/ return
c
c------------------------------< if this is the first entry for this
c------------------------------< level, initialize.
c
      if(status(ply) .ne. 0) go to 100
          which(ply)=first(ply)-1
          status(ply)=1
100   continue
c
c------------------------------< advance to the next move and
c------------------------------< determine if it should be ex-
c------------------------------< amined.
c
200   continue
          which(ply)=which(ply)+1
          if(which(ply) .gt. last(ply)) go to 9999
          from$=moves(which(ply))
          if(from$ .eq. 0) go to 200
c
c------------------------------< if the current move is a check or
c------------------------------< is a capture, then the tree search
c------------------------------< must be used to accept/refute it. if
c------------------------------< not, and the side to move is behind
c------------------------------< in material where it is not at earlier
c------------------------------< plies, ignore this move as hopeless
c
          if(ply .ne. depth) go to 400
          if(mated(ply)) go to 400
          if(in chk(ply)) go to 400
              call extrct
              if(cappc$ .ne. 0) go to 500
              call mover
              ctemp=check(-side)
              call umover
              if(ctemp) go to 500
c
c------------------------------< now determine if the material score
c------------------------------< will cause an alpha/beta cutoff without
c------------------------------< calculating the positional score
c
              tscore=side*(mscore+minmax(3-player))
              do 300 level=player,ply,2
                  if(tscore .le. side*value(level)) go to 200
300           continue
          go to 500
c
c------------------------------< this move should be examined.
c------------------------------< select it and return to begin
c------------------------------< the search.
c
400   continue
      call extrct
c
c------------------------------< determine if only moves that give
c------------------------------< check should be considered. if so,
c------------------------------< eliminate all others.
c
      if(.not. giv chk(ply)) go to 500
          call mover
          ctemp=check(-side)
          call umover
          if(.not. ctemp) go to 200
500   continue
      return=1
      return
c
c------------------------------< there are no move moves at this
c------------------------------< level, return to indicate that
c------------------------------< this phase has been completed.
c
9999  continue
      return=0
      return
      end


