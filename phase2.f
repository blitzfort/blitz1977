      subroutine phase2
c
c     ******************************************************************
c     *                                                                *
c     *      phase 2 is used to select the move saved in the hashed    *
c     *  position transposition table.  if this position was found in  *
c     *  the table, then a suggested best move has also been stored.   *
c     *  this move will be tried first in an attempt to get a quick    *
c     *  cutoff.                                                       *
c     *                                                                *
c     ******************************************************************
c
      implicit integer (a-z)
      common /tree/ moves(2000), first(30), last(30),  which(30),
     *              in chk(30), giv chk(30)
      common /depth/ sdepth, depth, ply
      common /info/ from$, to$, type$, propc$, cappc$
      common /hmove/ hmove
      common /return/ return
c
c------------------------------< if the current position was found
c------------------------------< in the transposition hash table
c------------------------------< then 'hmove' contains a suggested
c------------------------------< move to try first.
c
      if(hmove .eq. 0) go to 9999
c
c------------------------------< find the suggested move and return
c------------------------------< to analyze it first.
c
      start=first(ply)
      end=last(ply)
      do 100 where=start,end
          if(hmove .eq. moves(where)) go to 200
100   continue
      go to 9999
c
c------------------------------< the suggested move was found, select
c------------------------------< it and return to examine it to
c------------------------------< determine if it will cause a quick
c------------------------------< cutoff in this position.
c
200   continue
      which(ply)=where
      from$=moves(where)
      call extrct
      return=1
      return
c
c------------------------------< the suggested move was not found,
c------------------------------< return to begin selection phase 3.
c
9999  continue
      return=0
      return
      end

