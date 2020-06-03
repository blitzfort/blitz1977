      subroutine status
c
c     ******************************************************************
c     *                                                                *
c     *      status is used to inform the operator of the current      *
c     *  search status in response to the 'break' or 'attn' key being  *
c     *  hit on the terminal.  the format of the status is as follows: *
c     *                                                                *
c     *      depth: 6  time:  133 seconds                              *
c     *      best:    1236  nxbp                                       *
c     *      12 of 17 moves remaining                                  *
c     *                                                                *
c     *      if 'pndrng' is true, it indicates that the program is     *
c     *  'thinking' on the opponent's time.  a break here indicates    *
c     *  that a move is ready.  if the operator types 'ok', then the   *
c     *  predicted move was made and the search will continue.  if     *
c     *  anything else is typed, the current search is aborted and     *
c     *  control returns to main.                                      *
c     *                                                                *
c     ******************************************************************
c
      implicit integer (a-z)
      real ratio, re
      logical cputim, cquery, eboard
      logical tmode, smode, pndrng, foundm, matchd
      logical broke
      logical abort, autos
      common /depth/ sdepth, depth, ply
      common /tree/ moves(2000), first(30), last(30), which(30),
     *              in chk(30), giv chk(30)
      common /srchcm/ value(30), from(30), to(30), type(30), cappc(30)
      common /info/ from$, to$, type$, propc$, cappc$
      common /timecm/ gmoves, gelap, smoves, selap,  surpls, cputim,
     *                 cquery, pelap, oelap, psec1, psec2, osec1,
     *                 osec2, avgtim, expect, fsec1
      common /statcm/ nodes(30), times(30), aborts(30), tnodes, snodes,
     *                 hashes, pright
      common /mode/ tmode, smode, pndrng, foundm, matchd
      common /predcm/ ptext(30), pmove, ptype$, pfrom$, pto$
      common /savecm/ sboard(100), pmsave(3,2)
      common /buffer/ text(80)
      common /broke/ broke
      common /abort/ abort
      common /chr set/ alpha(46)
      common /ratio/ ratio
      common /mov cnt/ npmovs, nomovs
      common /autos/ autos
      common /eboard/ eboard, lbrack, rbrack
      equivalence (blank,alpha(44)), (alphao,alpha(15)),
     *            (alphak,alpha(11)),(quest,alpha(43))
c
c------------------------------< if thinking on opponent's time, break
c------------------------------< means read in a move or command.
c
      broke=.false.
      if(smode) go to 200
      if(.not. (pndrng .and. .not.matchd)) go to 150
          call read
          if(text(1) .eq. blank) go to 150
          if(text(1).eq.alphao .and. text(2).eq.alphak) go to 400
          inc=0
          if(eboard) inc=15
          do 350 i=1,15
              if(text(i) .eq. ptext(i+inc)) go to 350
                  abort=.true.
                  return
350       continue
400       continue
          do 360 i=1,15
              text(i)=ptext(i)
360       continue
          call setclk(2)
          matchd=.true.
          psec1=time()
          if(cputim) psec1=msec1
100   continue
      if(.not. autos) return
c
c------------------------------< determine how many moves are left
c------------------------------< for examination
c
150   continue
      remain=last(1)-which(1)+1
      total=last(1)-first(1)+1
      psec2=time()
      e=(psec2-psec1)/100
      re=e/60
      re=re+float(mod(e,60))/100
      print 40, depth, re
40    format(/1x,'depth:',i3,1x,'time:',f7.2,t21,':')
c
c------------------------------< decode and output the current
c------------------------------< move being analyzed and the
c------------------------------< best move found so far.
c
      if(which(1) .lt. first(1)) go to 444
      if(which(1) .gt. last(1)) go to 444
          from$=moves(which(1))
          call extrct
          call output(0,sboard,.false.)
          print 441,(text(l),l=1,30)
441       format(1x,'current move:',1x,30a1)
          from$=moves(1)
          call extrct
          call output(0,sboard,.false.)
          print 44,value(1),(text(l),l=1,30)
44        format(1x,'best move: ',i8,2x,30a1)
          print 443,remain,total
443       format(1x,i3,' of',i3,' moves remainins'/)
444   continue
      return
c
c------------------------------< program is in simultaneous mode.
c------------------------------< ask the operator if it is time to
c------------------------------< make a move.
c
200   continue
      call read
      if(text(1) .ne. quest) go to 150
      if(depth  .ne. 1) go to 201
          if(which(1) .eq. 1) go to 100
201   continue
      abort=.true.
      return
      end


