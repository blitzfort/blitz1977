      subroutine match
c
c     ******************************************************************
c     *                                                                *
c     *      match is used to determine if the opponent's move matched *
c     *  the programs predicted response.  if it did, there are two    *
c     *  actions to be taken:                                          *
c     *                                                                *
c     *      1)  increment 'pright' to count the number of correct     *
c     *          predictions, and;                                     *
c     *                                                                *
c     *      2)  determine if think-on-the-opponent's-time has already *
c     *          found a move while waiting on the opponent.  if so,   *
c     *          no search is required, simply make the move that      *
c     *          has alreaoy been found, thereby saving time, and,     *
c     *                                                                *
c     *      3)  set the starting iteration depth to zero if the move  *
c     *          is not what was expected and clobber the principle    *
c     *          variation so that it will not be followed.            *
c     *                                                                *
c     ******************************************************************
c
      implicit integer (a-z)
      logical tmode, smode, pndrng, foundm, matchd
      common /depth/ sdepth, depth, ply
      common /info/ from$, to$, type$, propc$, cappc$
      common /statcm/ nodes(30), times(30), aborts(30), tnodes, snodes,
     *                 hashes, pright
      common /predcm/ ptext(30), pmove, ptype$, pfrom$, pto$
      common /trcecm/ strace(32)
      common /types/ normal, castkg, castqn, enpass, promot
      common /mode/ tmode, smode, pndrng, foundm, matchd
      common /window/ window(2)
      common /return/ return
c
c------------------------------< did the human's move match the
c------------------------------< predicted move?
c
      if(ptype$ .ne. type$) go to 20
      if(type$.eq.castkg .or. type$.eq.castqn) go to 10
      if(pto$.ne.to$ .or. pfrom$.ne.from$) go to 20
c
c------------------------------< predicted move was made.  increment
c------------------------------< count and return if not in think-
c------------------------------< ahead mode or if move calculation
c------------------------------< was not completed.  return 1 if a
c------------------------------< response is ready.
c
10    continue
      pright=pright+1
      if(.not. foundm) go to 9999
      return=1
      return
c
c------------------------------< move didn't match predicted move.
c------------------------------< set the starting iteration depth
c------------------------------< to zero and clobber the variation
c------------------------------< so that the predicted move won't
c------------------------------< show up first and waste time.
c
20    continue
      sdepth=0
      do 30 i=1,32
          strace(i)=0
30    continue
      window(1)=-9999999
      window(2)= 9999999
9999  continue
      return=0
      return
      end



