      subroutine driver
c
c     ******************************************************************
c     *                                                                *
c     *      driver is used to control the iterated search.  it        *
c     *  generates the ply one move list and then begins the iter-     *
c     *  ative process.  basically, it does a complete one ply ex-     *
c     *  haustive search, then a complete two ply exhaustive search,   *
c     *  and so forth until it has used all available time or until    *
c     *  another iteration would use excessive time based on the time  *
c     *  used by the previous iteration,                               *
c     *      each new iteration uses the same ply one move list which  *
c     *  is re-ordered by 'backup' each time a new best move is backed *
c     *  up to ply one.  in this way, the program 'learns' more about  *
c     *  the position with each iteration and also gets maximum        *
c     *  benefits from the alpha/beta algorithm due to looking at      *
c     *  the best moves first,                                         *
c     *                                                                *
c     ******************************************************************
c
      implicit integer (a-z)
      logical cputim,  cquery
      logical abort, autos
      real ratio, newr, diff, ravg
      logical tmode, smode, pndrng, foundm, matchd
      integer fudge(2)
      logical inbook, easy
      common /board/ board(120)
      common /pieccm/ pieces(6)
      common /depth/ sdepth, depth, ply
      common /limits/  fdepth,  ftime
      common /movecm/ side, player, square,  mpiece
      common /tree/ moves(2000),  first(30), last(30),  which(30),
     *              inchk(30),  givchk(30)
      common /srchcm/ value(30), from(30), to(30), type(30), cappc(30)
      common /timecm/ gmoves, gelap,  smoves, selap,  surpls, cputim,
     *                 cquery, pelap,  oelap,  psec1,  psec2,  osec1,
     *                 osec2,  avgtim, expect,  fsec1
      common /movcnt/ npmovs, nomovs
      common /trace/ trace(32,30)
      common /tflag/ tflag
      common /easy/ easy, easyv
      common /trcecm/ strace(32)
      common /window/  window(2)
      common /abort/ abort
      common /ratio/ ratio
      common /mscore/ sscore, mscore, pscore, tscore
      common /bookcm/ inbook, key
      common /statcm/ nodes(30),times(30), aborts(30), tnodes, snodes,
     *                 hashes, pright
      common /mode/ tmode, smode, pndrng, foundm, matchd
      common /chrset/ alpha(46)
      equivalence (aster,alpha(40)),(blank,alpha(44))
      common /eval/ eval, peval
      common /autos/ autos
      common /ctime/ dnodes, xnodes
      common /return/ return
      data fudge / 100, -100 /
c
c------------------------------< initialize.
c
      abort=.false.
      elapp=0
      ttime=0
      xnodes=0
      if(sdepth .ne. 0) expect=ratio*times(sdepth)
      do 80 i=1,30
          times(i)=0
          aborts(i)=blank
80    continue
c
c------------------------------< determine if the program and
c------------------------------< opponent are still following
c------------------------------< known book analysis. if so,
c------------------------------< extract the next book move and
c------------------------------< return.
c
      if(.not. inbook) go to 85
          call book
          if(return .ne. 0) go to 9999
85    continue
c
c------------------------------< set up the timing controls by
c------------------------------< calculating how much time should
c------------------------------< be used per move.
c
      num=gelap-pelap-surpls
      denom=gmoves-npmovs
      if(.not. pndrng) go to 40
          if(npmovs .ne. gmoves) go to 40
          if(nomovs .ne. gmoves) go to 40
              num=num+selap
              denom=denom+smoves
40    continue
      avgtim=num/denom
      tavg=(num+selap)/(denom+smoves)
      avgtim=min0(avgtim,tavg)
      if(avgtim .gt. 0) go to 50
          avgtim=(gelap-pelap)
          avgtim=max0(3,avgtim/((gmoves-npmovs)*3))
50    continue
      if(ftime .ne. 0) avgtim=ftime
      avgtim=avgtim*100
c
c------------------------------< initialize for the minimax search
c------------------------------< procedure.
c
      call base
      tnodes=0
      snodes=0
      hashes=0
      depth=sdepth
c
c------------------------------< print the target time for this
c------------------------------< iteration and the number of ply 1
c------------------------------< moves.
c
      nmoves=last(1)-first(1)+1
      iavg=float(avgtim)/100
      ravg=iavg/60
      ravg=ravg+float(mod(iavg,60))/100
      if(autos .and. .not.smode) print 101, ravg, nmoves
101   format(/1x,'target:',f7.2,' for',i3,' moves',t13,':')
      fsec1=time()
c
c------------------------------< if there is only one legalmove
c------------------------------< that can be made, there is no
c------------------------------< need to waste time by doing a
c------------------------------< tree search.
c
      if(last(1) .ne. 1) go to 1000
      if(pndrng) go to 1000
c
c------------------------------< the program has only one legal
c------------------------------< move. store it in the saved
c------------------------------< variation along with a depth
c------------------------------< limit of one and return.
c
          strace(1)=moves(1)
          strace(31)=1
          strace(32)=1
          value(ply)=-9999999
          return
c
c------------------------------< increment the basic search depth for
c------------------------------< this iteration.
c
1000  continue
          depth=depth+1
          if(depth .gt. 29) go to 9999
          if(tflag .gt. 0) write(6,88) depth
88        format(1x,'*'/1x,'* iteration ',i2/1x,'*')
c
c------------------------------< evaluate the material on the board
c------------------------------< for the current position.
c
          mscore=0
          sscore=0
          do 10 sq=22,99
              temp=board(sq)
              if(temp .eq. 0) go to 10
              if(temp .gt. 6) go to 10
              mscore=mscore+isign(1,temp)*pieces(iabs(temp))
10        continue
c
c------------------------------< call search to analyze the current
c------------------------------< board position.
c
97        continue
          which(1)=0
98        continue
          tstart=time()
          twhich=which(1)+1
          call search
          psec2=time()
          elap=psec2-tstart
          elapp=psec2-psec1
          times(depth)=elap
c
c------------------------------< determine if the window was too
c------------------------------< narrow. if so, extend the edge
c------------------------------< that caused the failure.
c
          if(abort) go to 99
          if(value(1).gt.window(1).and.value(1).lt.window(2)) go to 99
              aborts(depth)=aster
              if(value(1) .le. window(1)) window(1)=-9999999
              if(value(1) .lt. window(2)) go to 97
                  window(2) = 9999999
                  which(1)=which(1)-1
                  if(which(1).eq.0 .and. depth.gt.1) go to 98
                  strace(1)=moves(1)
                  strace(31)=1
                  strace(32)=1
              go to 98
99        continue
c
c------------------------------< save the principle variation from this
c------------------------------< iteration to use as a guide for the
c------------------------------< next iteration.
c
          if(.not. abort) go to 150
              if(depth .eq. 1) go to 150
              if(which(1) .le. twhich) go to 160
150       continue
          do 100 i=1,32
              strace(i)=trace(i,1)
100       continue
          eval=value(1)
160       continue
c
c------------------------------< now determine if another iteration
c------------------------------< is possible by analyzing the time
c------------------------------< for the last iteration.
c
          ttime=ttime+elap/100
          if(ttime .le. 0) go to 166
              dnodes=max0(tnodes/ttime,(tnodes/ttime*avgtim/(100*60)))
166       continue
c
c------------------------------< set the alpha/beta window for the
c------------------------------< next iteration based on the expected
c------------------------------< score returned from the last iteration.
c
          gain=fudge(2-mod(depth+1,2))
          window(1)=eval+gain-500
          window(2)=eval+gain+500
          if(abort) go to 9999
c
c------------------------------< if checkmate has been found, there is
c------------------------------< no need to continue the search since
c------------------------------< the mate is forced at the curent
c------------------------------< depth.
c
          if(value(1) .lt. -900000) go to 9999
          if(value(1) .lt. 900000) go to 170
              if(value(1) .gt. peval) go to 9999
170       continue
c
c------------------------------< adjust the ratio for the timed
c------------------------------< expected for the next iteration
c------------------------------< based on the ratio of the times
c------------------------------< for the last 2 iterations.
c
          if(depth .le. 1) go to 77
          if(easy) go to 77
          if(times(depth).eq.0 .or. times(depth-1).eq.0) go to 77
          newr=float(times(depth))/float(times(depth-1))
          diff=ratio-newr
          if(diff .gt. 1) diff=1;
          if(diff .lt. -1) diff=-1;
          ratio=ratio-diff/5.0
77        continue
          expect=ratio*elap
c
c------------------------------< since the expected time for another
c------------------------------< iteration is ratio*elap(depth,1),
c------------------------------< anotehr iteration can be started
c------------------------------< if the total time used so far plus
c------------------------------< the time for the last iteration x ratio
c------------------------------< does not exceed the average time
c------------------------------< the program must average. this will
c------------------------------< allow at lease some of the level one
c------------------------------< moves to be examined deeper.
c
          if(fdepth .gt. depth) go to 185
          if(pndrng .and. .not.matchd) go to 185
          if(fdepth .ne. 0) go to 9999
          if(psec2-psec1 .ge. avgtim) go to 9999
c
c------------------------------< call inform if necessary to keep the
c------------------------------< operator informed of what's happening.
c
185       continue
          if(autos .and. times(depth).gt.500)
     *                           call inform(.false.,.false.)
      go to 1000
c
c------------------------------< iteration done. return to calling
c------------------------------< routine.
c
9999  continue
      return
      end



