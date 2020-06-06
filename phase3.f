      subroutine phase3
c
c     ******************************************************************
c     *                                                                *
c     *      phase 3 is called to select captures which seem to gain   *
c     *  material.  the captures are ordered based on the value of     *
c     *  the captured piece.  'remove' is called to analyze the        *
c     *  attackers and defenders of each piece that can be captured    *
c     *  to determine the expected gain.                               *
c     *                                                                *
c     ******************************************************************
c
      implicit  integer (a-z)
      logical done
      common /board/ board(120)
      common /tree/  moves(2000), first(30), last(30), which(30),
     *              inchk(30),  givchk(30)
      common /phascm/ phase(30),  status(30)
      common /depth/ sdepth, depth, ply
      common /info/  from$, to$,  type$, propc$,  cappc$
      common /movecm/ side, player, square, mpiece
      common /pieccm/ pieces(6)
      common /sortcm/ val(100)
      common /return/  return
c
c------------------------------< if this is the first entry for this
c------------------------------< set of moves, the captures need to
c------------------------------< be analyzed.  if not, the list is
c------------------------------< already in order.
c
      if(status(ply) .ne. 0)  go to 700
c
c------------------------------< it is necessary to scan the move
c------------------------------< list to determine if there are any
c------------------------------< captures which seem to win material.
c
      count=0
      where=first(ply)-1
100   continue
          where=where+1
          if(where .gt. last(ply)) go to 200
c
c------------------------------< determine if this move is a
c------------------------------< capture.  if not, skip it.
c
          from$=moves(where)
          cappc$=from$/1048576
          if(cappc$ .eq. 0) go to 100
c
c------------------------------< the score is equal to the value
c------------------------------< of the piece being captured.  if
c------------------------------< the piece is defended, subtract
c------------------------------< the value of the capturing piece.
c
          call extrct
          callmover
          score=min0(0,-ripoff(to$))
          if(cappc$ .ne. 0) score=score+pieces(cappc$)
          if(propc$ .ne. 0) score=score+pieces(propc$)
          call umover
c
c------------------------------< if the score is less than zero (0)
c------------------------------< leave the move where it is and do
c------------------------------< not consider it a phase 3 capture
c
          if(score .lt. 0) go to 100
          temp=moves(first(ply)+count)
          moves(first(ply)+count)=moves(where)
          moves(where)=temp
          count=count+1
          val(count)=score
      go to 100
200   continue
c
c------------------------------< if there is one or less capture
c------------------------------< that seem to win, there is no need
c------------------------------< to sort the list.
c
      if(count .eq. 0) go to 9999
      if(count .eq. 1) go to 600
c
c------------------------------< now order the captures based on
c------------------------------< the expected gain of material
c------------------------------< computed above.
c
      start=first(ply)
300   continue
          done=.true.
          where=0
400       continue
              where=where+1
              if(where .ge. count) go to 500
              if(val(where) .ge. val(where+1)) go to 400
              temp=val(where)
              val(where)=val(where+1)
              val(where+1)=temp
              temp=moves(start+where-1)
              moves(start+where-1)=moves(start+where)
              moves(start+where)=temp
              done=.false.
          go to 400
500   continue
      if(.not. done) go to 300
c
c------------------------------< remember how many captures were
c------------------------------< selected so that phase 3 can exit
c------------------------------< when they have been examined.
c
600   continue
      status(ply)=count+1
      which(ply)=first(ply)-1
c
c------------------------------< the move list has the winning
c------------------------------< captures first.  select the next
c------------------------------< move.
c
700   continue
      status(ply)=status(ply)-1
      if(status(ply) .eq. 0) go to 9999
      which(ply)=which(ply)+1
      from$=moves(which(ply))
      call extrct
      return=1
      return
c
c------------------------------< no more winning captures are in
c------------------------------< the move list, return to begin
c------------------------------< selection phase 4.
c
9999  continue
      return=0
      return
      end


