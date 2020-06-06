      subroutine phase4
c
c     ******************************************************************
c     *                                                                *
c     *      phase 4 is used to select the 'killer' moves for this     *
c     *  ply from the 'killer' move list as the next move(s) to be     *
c     *  considered.  the 'killer' moves are examined in order of      *
c     *  their frequency of use in an attempt to try the best one      *
c     *  first.  these 'killer' moves are simply moves that were       *
c     *  found to be best at this level from the same previous         *
c     *  parent position.  by hopefully looking at the best move       *
c     *  first, the search will optimize the number of alpha/beta      *
c     *  cutoffs that occur.                                           *
c     *                                                                *
c     ******************************************************************
c
      implicit integer (a-z)
      common /tree/ moves(2000), first(30), last(30),  which(30),
     *              inchk(30), givchk(30)
      common /phascm/ phase(30), status(30)
      common /depth/ sdepth, depth, ply
      common /info/ from$, to$, type$, propc$, cappc$
      common /killmv/ killmv(20,30)
      common /return/ return
c
c------------------------------< advance to the next killer fo
c------------------------------< this level.
c
      begin=status(ply)+1
      if(begin .gt. 10) go to 9999
c
c------------------------------< determine if any of the killer
c------------------------------< moves are in the current move
c------------------------------< list. if so, examine them next.
c
      start=first(ply)
      end=last(ply)
      do 200 kill=begin,10
          move=killmv(kill,ply)
          if(move .eq. 0) go to 9999
          do 100 where=start,end
              if(move .eq. moves(where)) go to 300
100           continue
200   continue
c
c------------------------------< no matches found, the current
c------------------------------< move list has no known killer
c------------------------------< moves in it.  return to begin
c------------------------------< selection phase 5
c
      go to 9999
c
c------------------------------< a killer move was found, select
c------------------------------< it and return to examine it to
c------------------------------< see if it causes an alpha/beta
c------------------------------< cutoff in this position.
c
300   continue
      status(ply)=kill
      which(ply)=where
      from$=moves(where)
      call extrct
      return=1
      return
c
c------------------------------< no 'killer' moves were found,
c------------------------------< return to begin selection phase 5.
c
9999  continue
      return=0
      return
      end


