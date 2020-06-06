      subroutine select
c
c     ******************************************************************
c     *                                                                *
c     *      select is used to select the next move to be examined by  *
c     *  'search'.  the move selection process is executed in several  *
c     *  phases:                                                       *
c     *                                                                *
c     *      phase 1 : if allmoves at previous levels are part        *
c     *              of the principle variation from the previous      *
c     *              iteration, try the corresponding move at this     *
c     *              level if it exists.                               *
c     *                                                                *
c     *      phase 2 : if this position was found in the transposition *
c     *              ha8h table, try the suggested move that was       *
c     *              stored with it,  this move was found to be the    *
c     *              best in this position either during this          *
c     *              iteration or during a previous iteration.  it is  *
c     *              similar to a killer move but is a killer for      *
c     *              this particular position, rather than for this    *
c     *              particular ply.                                   *
c     *                                                                *
c     *      phase 3 : try all captures that do not seem to be out-    *
c     *              right material loses. ie, pxn is safe, but qxr    *
c     *              is safe only if the rook is undefended.           *
c     *                                                                *
c     *      phase 4 : try the 'killer' moves to see if any of them    *
c     *              are legal.  these moves have been found to be     *
c     *              good at other descendants of the same parent      *
c     *              node that this position has and should be tried   *
c     *              first to hopefully force an alpha/beta cutoff.    *
c     *                                                                *
c     *      phase 5 : try the rest of the moves in more or less       *
c     *              random order.                                     *
c     *                                                                *
c     ******************************************************************
c
      implicit integer (a-z)
      logical cputim,  cquery, inchk, givchk, abort
      logical easy, mated
      logical tmode, smode, pndrng, foundm, matchd
      common /tree/ moves(2000), first(30), last(30), which(30),
     *              inchk(30), givchk(30)
      common /srchcm/ value(30), from(30), to(30), type(30), cappc(30)
      common /phascm/ phase(30), status(30)
      common /info/ from$, to$, type$, propc$, cappc$
      common /depth/ sdepth, depth, ply
      common /limits/ fdepth, ftime
      common /timecm/ gmoves, gelap, smoves, selap, surpls, cputim,
     *                 cquery, pelap, oelap, psec1, psec2, osec1,
     *                 osec2, avgtim, expect, fsec1
      common /window/ window(2)
      common /easy/ easy, easyv
      common /mode/ tmode, smode, pndrng, foundm, matchd
      common /abort/ abort
      common /eval/ eval, peval
      common /statcm/ nodes(30), times(30), aborts(30), tnodes, snodes,
     *                 hashes, pright
      common /ctime/ dnodes, xnodes
      common /mated/ mated(30)
      common /movcnt/ npmovs, nomovs
      common /return/ return
c
c------------------------------< first make sure that there are some
c------------------------------< moves to choose from: if not, return
c------------------------------< with no further ado.
c
      if(first(ply) .gt. last(ply)) go to 9998
c
c------------------------------< determine if another ply n move
c------------------------------< can be considered without blowing
c------------------------------< the clock time.
c
      if(tnodes-xnodes .lt. dnodes) go to 210
      if(smode) go to 210
          xnodes=tnodes
          if(pndrng .and. .not.matchd) go to 210
          if(fdepth .ne. 0) go to 210
          psec2=time()
          if(cputim) psec2=msec2
          if(psec2-fsec1 .lt. avgtim) go to 130
          if(which(1) .gt. 1) go to 90
          if(value(1) .ne. -9999999) go to 80
              if(psec2-psec1 .lt. avgtim) go to 130
              go to 90
80        continue
              if(expect .gt. avgtim) go to 120
90        continue
          if(value(1) .gt. -900000) go to 100
          if(value(1) .lt. -9000000) go to 100
              maxlim=(gelap-pelap-surpls/2)/2
              if((psec2-psec1)/100 .gt. maxlim) go to 120
              go to 210
100       continue
              if(psec2-psec1 .lt. avgtim) go to 130
              if(depth .gt. 1) go to 110
                  if(which(1) .ge. 1) go to 110
                      if(ply .gt. 1) go to 210
110           continue
                  if(value(1)+750 .gt. eval) go to 120
                  if(psec2-psec1 .lt. avgtim*2) go to 210
                  target=(gelap-surpls)/gmoves
                  averag=pelap/npmovs
                  extra=(target-averag)*npmovs
                  if(psec2-psec1 .lt. avgtim+extra/2) go to 210
120           continue
              abort=.true.
              return=2
              return
130       continue
c
c------------------------------< determine if it is possible to stop
c------------------------------< this iteration early. if this is the
c------------------------------< last iteration and the same move has
c------------------------------< been best each preceeding iteration,
c------------------------------< stop if the move is no worse than
c------------------------------< expected.
c
          if(.not. easy)  go to 200
              if(psec2-fsec1 .lt. avgtim/3) go to 200
              easy=easy .and. easyv.lt.value(1)
              if(easy) go to 120
200       continue
210   continue
      if(ply .gt. 1) go to 299
          if(which(1) .eq.  0) go to 4500
          if(mated(1)) go to 220
              if(value(1) .le. window(1)) go to 9998
220       continue
          if(value(1) .ge.  window(2)) go to 9998
          if(value(1) .lt.  900000) go to 230
          if(value(1) .gt.  9000000) go to 230
              if(value(1) .gt. peval) go to 9998
230       continue
          go to 5000
299   continue
c
c------------------------------< if this is the first 'select' call
c------------------------------< for this node, set up for phase 1;
c------------------------------< otherwise, zero the last move examined
c------------------------------< so that is will not be considered
c------------------------------< again laster.
c
      if(which(ply) .eq. 0) go to 300
          moves(which(ply))=0
          go to 600
300   continue
      phase(ply)=1
      status(ply)=0
      if(which(1) .ne. 1) go to 500
          do 400 i=2,ply
              if(phase(i) .ne. 1) go to 500
400       continue
          go to 600
500   continue
      phase(ply)=2
600   continue
c
c------------------------------< now, off to the proper phase
c------------------------------< to select the next move.
c
      index=phase(ply)
      go to (1000,2000,3000,4000,5000),index
c
c------------------------------< phase 1 : determine if it is
c------------------------------< possible to follow the principle
c------------------------------< variation further.
c
1000  continue
      call phase1
      if(return .ne. 0) go to 9999
      phase(ply)=2
      status(ply)=0
c
c------------------------------< phase 2 : try the move stored in the
c------------------------------< hash table.
c
2000  continue
      call phase2
      if(return .ne. 0) go to 9999
      phase(ply)=3
      status(ply)=0
c
c------------------------------< phase 3 : try captures which don't
c------------------------------< seem to lose material. in the quiescent
c------------------------------< search, try all captures.
c
3000  continue
      call phase3
      if(return .ne. 0) go to 9999
      phase(ply)=4
      status(ply)=0
      if(inchk(ply)) go to 4000
      if(ply .le. depth) go to 4000
      if(givchk(ply)) go to 4500
          phase(ply)=0
          go to 9998
c
c------------------------------< phase 4 : try the 'killer' move to
c------------------------------< see if they are legal. if so, examine
c------------------------------< them next.
c
4000  continue
      call phase4
      if(return .ne. 0) go to 9999
4500  continue
      phase(ply)=5
      status(ply)=0
c
c------------------------------< phase 5: try all remaining moves
c------------------------------< that were left after 1/2/3/4.
c
5000  continue
      call phase5
      if(return .ne. 0) go to 9999
      phase(ply)=0
c
c------------------------------< no moves were found, return to
c------------------------------< indicate search is complete.
c
9998  continue
      return=1
      return
c
c------------------------------< a move was found, return to examine
c------------------------------< it.
c
9999  continue
      return=0
      return
      end



