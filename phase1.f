      subroutine phase1
c
c     ******************************************************************
c     *                                                                *
c     *      phase 1 is called to select the move at this ply that     *
c     *  follows the principle variation.  if the variation doesn't    *
c     *  extend to this level, or if previous levels have deviated     *
c     *  from the principle variation, then no move will be selected   *
c     *  if the previous levels are still following the principle      *
c     *  variation, then the matching move from this level will be     *
c     *  examined first in an attempt to gain something from the       *
c     *  previous iteration.                                           *
c     *                                                                *
c     ******************************************************************
c
      implicit integer (a-z)
      common /tree/ moves(2000), first(30), last(30), which(30),
     *              in chk(30), giv chk(30)
      common /phas cm/ phase(30), status(30)
      common /depth/ sdepth, depth, ply
      common /info/ from$, to$, type$, propc$, cappc$
      common /trcecm/ strace(32)
      common /return/ return
c
c------------------------------< if this is not the first entry for this
c------------------------------< phase, there is nothing more to do.
c
      if(status(ply) .ne. 0) go to 9999
      status(ply)=1
c
c------------------------------< if the principle variation doesn't
c------------------------------< extend this deep, then return.
c
      if(mod(strace(31),1000) .lt. ply) go to 9999
c
c------------------------------< locate the principle variation move
c------------------------------< in the move list
c
      start=first(ply)
      end=last(ply)
      do 100 where=start,end
          if(moves(where) .eq. strace(ply)) go to 200
100   continue
      go to 9999
c
c------------------------------< the principle variation move was
c------------------------------< found. return to examine it.
c
200   continue
      which(ply)=where
      from$=moves(where)
      call extrct
      return=1
      return
c
c------------------------------< the principle variation move was
c------------------------------< not found, return to begin
c------------------------------< selection phase 2.
c
9999  continue
      return=0
      return
      end

