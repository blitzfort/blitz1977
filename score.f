      subroutine score
c
c     ******************************************************************
c     *                                                                *
c     *      score is called to evaluate the material/positional       *
c     *  status for the current terminal position.  it first           *
c     *  evaluates the material score to determine if it will cause    *
c     *  an immediate alpha/beta cutoff without evaluating all of      *
c     *  the positional factors.  if not, the board is scanned and     *
c     *  various positional scoring functions are called to compute    *
c     *  a positional evaluation.                                      *
c     *                                                                *
c     ******************************************************************
c
      implicit integer (a-z)
      logical ppass, opass, pmoved, moved
      common /board/ board(120)
      common /depth/ sdepth, depth, ply
      common /tree/ moves(2000), first(30), last(30), which(30),
     *              inchk(30), givchk(30)
      common /srchcm/ value(30), from(30), to(30), type(30), cappc(30)
      common /mscore/ sscore, mscore, pscore, tscore
      common /movecm/ side, player, sqfrom, mpiece
      common /statcm/ nodes(30), times(30), aborts(30), tnodes, snodes,
     *                 hashes, pright
      common /minmax/ minmax(2)
      common /pawns/ pfirst(10), plast(10), pcount(10), ppass(10),
     *               ofirst(10), olast(10), ocount(10), opass(10),
     *               arank, afile
      common /pieces/ nppwns, nppcs, pqueen, nopwns, nopcs, oqueen
      common /kloc/ pkingl, prank, pfile, okingl, orank, ofile
      common /scorcm/ sign, square, rank, file
      common /castcm/ pmoved(3,2), moved(3,35)
      common /pieccm/ pieces(6)
      common /colrcm/ color
      common /types/ normal, castkg, castqn, enpass, promot
      common /movcnt/ npmovs, nomovs
c
c------------------------------< initialize.
c
      pscore=0
c
c------------------------------< increment the number of board
c------------------------------< evaluations for the search statistics
c------------------------------< display
c
      snodes=snodes+1
c
c------------------------------< call 'locate' to initialize tables
c------------------------------< containing information about piece
c------------------------------< location, etc.
c
      call locate
c
c------------------------------< determine if 'scorem' should be called
c------------------------------< to evaluate the position, yes if there
c------------------------------< are no pawns left.
c
      if(nppwns+nopwns .ne. 0) go to 100
          call scorem
          go to 2000
100   continue
c
c------------------------------< call 'scorep' to evaluate the pawn
c------------------------------< structure.
c
      call scorep
c
c------------------------------< scan the board and compute the value
c------------------------------< of each piece found relative to where
c------------------------------< it is on the board.
c
      do 1000 square=22,99
          piece=board(square)
          if(piece .eq. 0) go to 1000
          if(piece .gt. 6) go to 1000
          index=iabs(piece)
          sign=isign(1,piece)
          file=mod(square,10)
          rank=square/10
          go to (1000,1200,1300,1400,1500,1600),index
c
c------------------------------< knights
c
1200      continue
          call scoren
          go to 1000
c
c------------------------------< bishops
c
1300      continue
          call scoreb
          go to 1000
c
c------------------------------< rooks
c
1400      continue
          call scorer
          go to 1000
c
c------------------------------< queens
c
1500      continue
          call scoreq
          go to 1000
c
c------------------------------< kings
c
1600      continue
          call scorek
1000  continue
c
c------------------------------< evaluate development, until the king
c------------------------------< has safely castled, penalize moves
c------------------------------< that move the king or rooks.
c
2000  continue
      if(pmoved(1,1)) go to 2200
          limit=(ply+1)/2*2-1
          if(which(limit) .eq. 0) limit=limit-2
          if(limit .lt. 1) go to 2200
          if(moved(1,limit)) go to 2100
          if(moved(2,limit)) pscore=pscore-150
          if(moved(3,limit)) pscore=pscore-150
          go to 2200
2100      continue
              do 2150 temp=1,limit,2
                  if(type(temp) .eq. cast kg) go to 2200
                  if(type(temp) .eq. cast qn) go to 2200
2150          continue
              pscore=pscore-300
2200  continue
      if(pmoved(1,2)) go to 2400
          limit=ply/2*2
          if(which(limit) .eq. 0) limit=limit-2
          if(limit .lt. 2) go to 2400
          if(moved(1,limit)) go to 2300
          if(moved(2,limit)) pscore=pscore+100
          if(moved(3,limit)) pscore=pscore+100
          go to 2400
2300      continue
              do 2350 temp=2,limit,2
                  if(type(temp) .eq. cast kg) go to 2400
                  if(type(temp) .eq. cast qn) go to 2400
2350          continue
              psccre=pscore+200
2400  continue
c
c------------------------------< evaluate the remaining material
c------------------------------< and encourage the side with is
c------------------------------< ahead to exchange pieces but not
c------------------------------< pawns whenever possible.
c
      if(mscore .eq. 0) go to 3200
      if(sscore .eq. 0) go to 3200
          escore=(sscore/pieces(1))*(mscore*50/(6*pieces(1)))
          pscore=pscore+escore
          if(mscore .lt. 0) go to 3100
              pscore=pscore-(8-nppwns)*30
              go to 3200
3100      continue
              pscore=pscore+(8-nopwns)*30
3200  continue
c
c------------------------------< now add in the positional score
c------------------------------< computed to the material balance
c------------------------------< score to compute the net worth
c------------------------------< of the current position.
c
      tscore=mscore+pscore
      if(pscore .lt. minmax(1)) minmax(1)=pscore
      if(pscore .gt. minmax(2)) minmax(2)=pscore
      return
      end


