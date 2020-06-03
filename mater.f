      subroutine mater
c
c     ******************************************************************
c     *                                                                *
c     *      mater is called to determine how deeply checks should be  *
c     *  evaluated.  it gives blitz it's ''bloodthirsty'' attitude     *
c     *  by attempting to find mates at extremely deep nodes.  it      *
c     *  operates as follows:                                          *
c     *                                                                *
c     *        1)  count the number of checking moves that occurred    *
c     *            for the side on move in the basic search (plies     *
c     *            not greater than depth).                            *
c     *                                                                *
c     *        2)  make sure that all moves since the basic search     *
c     *            ended are also checks, since the opposing side      *
c     *            can simply 'stand pat' to avoid a mate if they      *
c     *            are not;  if they are the opponent must try all     *
c     *            legal moves to avoid mate;                          *
c     *                                                                *
c     *        3)  count the number of non-capturing checks that have  *
c     *            been included in the line under analysis;  if this  *
c     *            number is less than the number of basic search      *
c     *            checks found in (1) above, then we will include     *
c     *            all checking moves for this level in the tree       *
c     *            to be examined,                                     *
c     *                                                                *
c     ******************************************************************
c
      implicit integer (a-z)
      logical in chk, giv chk
      common /depth/ sdepth, depth, ply
      common /tree/ moves(2000), first(30), last(30), which(30),
     *              in chk(30), giv chk(30)
      common /srchcm/ value(30), from(30), to(30), type(30), cappc(30)
      common /phas cm/ phase(30), stats(30)
      common /move cm/ side, player, square, mpiece
      common /types/ normal, castkg, castqn, enpass, promot
c
c
c------------------------------< if the side on move is already in
c------------------------------< check, then the search for this level
c------------------------------< is exhaustive anyway,  return.
c
      if(in chk(ply)) go to 9999
c
c------------------------------< count the number of check that
c------------------------------< occurred in the basic search.
c
          end=depth+1
          start=2+mod(ply+1,2)
          if(start .gt. end) go to 9999
              bcheck=0
              do 100 level=start,end,2
                  if(in chk(level)) bcheck=bcheck+1
100           continue
              if(bcheck .eq. 0) go to 9999
c
c------------------------------< make sure that the opponent was in
c------------------------------< check at all nodes that are in the
c------------------------------< quiescence search.  if not, mates
c------------------------------< cannot be found since the opponent
c------------------------------< can stand pat at any node rather
c------------------------------< than taking the search evaluation.
c
              start=depth+1
              if(2-mod(start,2) .eq. player) start=start+1
              end=ply-1
              if(start .gt. end) go to 300
              do 200 level=start,end,2
                  if(.not. in chk(level)) go to 9999
200           continue
c
c------------------------------< count the number of non-capturing
c------------------------------< checks that have already occurred
c------------------------------< in the quiescence search.
c
300           continue
              start=depth+1
              if(mod(start,2) .ne. mod(player,2)) start=start+1
              end=ply-2
              qcheck=0
              ccheck=0
              if(start .gt. end) go to 500
              do 400 level=start,end,2
                  if(cappc(level) .ne. 0) go to 450
                  if(type(level) .eq. promot) go to 450
                      qcheck=qcheck+1
                      go to 400
450               continue
                      ccheck=ccheck+1
400           continue
              qcheck=qcheck+ccheck/2
c
c------------------------------< if there were fewer non-capturing
c------------------------------< checksin the quiescence search than
c------------------------------< there were in the basic search, then
c------------------------------< examine all checks at this level.
c
500       continue
          if(qcheck .lt. bcheck) giv chk(ply)=.true.
9999  continue
      return
      end


