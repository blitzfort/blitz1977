      subroutine lookup
c
c     ******************************************************************
c     *                                                                *
c     *      lookup is used to examine the hash table to determine if  *
c     *  further analysis is needed for the current position.  as each *
c     *  position is evaluated, it is entered into the hash table      *
c     *  along with the best/worst/current scores for this position.   *
c     *  whenever a new position is reached for analysis, first this   *
c     *  table is examined to determine if this position has been      *
c     *  reached before.  if so, one of three possible cases arises:   *
c     *                                                                *
c     *      1)  the score saved for this position is perfectly        *
c     *          valid.  in this case, return this score and consider  *
c     *          this node complete without doing a search.            *
c     *                                                                *
c     *      2)  the score saved for this position is only known to    *
c     *          be better than some upper bound or worse than some    *
c     *          lower bound.  the score saved is actually the bound   *
c     *          in question.  if it is an upper bound, and it is      *
c     *          less than the current lower bound, or it is a lower   *
c     *          bound that is greater than the current upper bound,   *
c     *          then an immediate alpha/beta cutoff will occur with   *
c     *          no additional searching from this node.  if it fails  *
c     *          this test, it still might be used to increase the     *
c     *          lower bound or reduce the upper bound to further      *
c     *          speed up the search.                                  *
c     *                                                                *
c     *      3)  the score saved for this position is useless.  in     *
c     *          this case, return and continue the search normally.   *
c     *                                                                *
c     ******************************************************************
c
      implicit integer (a-z)
      logical mated
      logical tf(2)
      common /board/ board(120)
      common /hashcm/ random(100,13), hboard
      common /htable/ hsize, htable(32768)
      common /tree/ moves(2000), first(30), last(30), which(30),
     *              inchk(30), givchk(30)
      common /trace/ trace(32,30)
      common /srchcm/ value(30), from(30), to(30), type(30), cappc(30)
      common /depth/ sdepth, depth, ply
      common /movecm/ side, player, square, mpiece
      common /movcnt/ npmovs, nomovs
      common /hmove/ hmove
      common /statcm/ nodes(30), times(30), aborts(30), tnodes, snodes,
     *                 hashes, pright
      common /mated/ mated(30)
      common /window/ window(2)
      common /prevmv/ prevmv(6)
      common /return/ return
      data tf / .false., .true. /
c
c------------------------------< hash the current position to get the
c------------------------------< pointer to the hash table.
c
shit             go to 9999
      hmove=0
      hboard=0
      do 100 square=22,99
          temp=board(square)+7
          if(temp.eq.7 .or. temp.gt.13) go to 100
              hboard=ieor(hboard,random(square,temp))
100   continue
      if(ply .gt. 1) go to 110
          tosq=prevmv(5)
           fromsq=prevmv(4)
          go to 120
110   continue
          tosq=to(ply-1)
          fromsq=from(ply-1)
120   continue
      if(iabs(board(tosq)) .ne. 1) go to 130
      if(iabs(fromsq-tosq) .ne. 20) go to 130
          file=mod(tosq,10)
          hboard=ieor(hboard,random(file,1))
130   continue
      hkey=iand(hboard,hsize-1)
c
c------------------------------< now determine if this position is
c------------------------------< in the table, for collisions, use
c------------------------------< the random probe technique to search
c------------------------------< 25 additional entries in an attempt
c------------------------------< to locate the correct one.
c
      rindex=1
      colsns=25
      rkey=hkey
200   continue
          rkey=(rkey+(player-1)*hsize)*4
          if(hboard .eq. htable(rkey+1)) go to 300
c
c------------------------------< a collision has occured.  determine
c------------------------------< if another probe should be made.  if
c------------------------------< so, compute the next hash key and try
c------------------------------< again for a match.
c
          colsns=colsns-1
          if(colsns .lt. 0) go to 9999
          rindex=mod(5*rindex,hsize*4)
          if(rindex .eq. 1) go to 9999
          rkey=hkey+rindex/4
          if(rkey .gt. hsize-1) rkey=rkey-hsize
      go to 200
c
c------------------------------< the current position is in the table.
c------------------------------< determine if it is for the current
c------------------------------< iteration with the correct side on
c------------------------------< move
c
300   continue
      mdepth=htable(rkey+2)/32
      mdepth=mod(mdepth,32)
      mply=mod(htable(rkey+2),32)
      mmove=htable(rkey+2)/65536
      hashes=hashes+1
      hmove=htable(rkey+3)
      tbound=htable(rkey+4)
      mated(ply-1)=.false.
      mtype=htable(rkey+2)/1024
      mtype=mod(mtype,4)
      go to (400,500,600),mtype
c
c------------------------------< the current position has been found.
c------------------------------< the stored score is valid.  return it
c------------------------------< to search and consider this node as
c------------------------------< completed with no further searching
c------------------------------< required. (hooray!)
c
400   continue
      play=2-mod(player+1,2)
      mside=-side
      bound=mside*value(play)
      temp=play+2
      if(temp .gt. ply) go to 420
          do 410 level=temp,ply,2
              bound=max0(bound,mside*value(level))
410       continue
420   continue
      if(iabs(tbound) .lt. 900000) go to 430
      if(iabs(tbound) .gt. 9000000) go to 430
          tbound=tbound+isign(1,tbound)*(npmovs+nomovs-mmove)
          mdepth=999
430   continue
      if(mdepth-mply .lt. depth-ply) go to 9999
      if (mside*tbound .le. bound) go to 9997
      mmated=htable(rkey+2)/4096
      mmated=mod(mmated,4)
      mated(ply)=tf(mmated)
      limit=ply-1
      do 440 level=1,limit
          trace(level,ply)=moves(which(level))
440   continue
      trace(31,ply)=1000+ply-1
      trace(32,ply)=depth
      value(ply)=tbound
      go to 9998
c
c------------------------------< the current position has been found.
c------------------------------< however, in this position, allmoves
c------------------------------< for the side on move were pruned
c------------------------------< causing the applicable search bound
c------------------------------< to be backed up. if the bound has
c------------------------------< not changed, simply back it up again.
c
500   continue
      if(mdepth-mply .lt. depth-ply) go to 9999
      bound=side*value(player)
      lim1=player+2
      lim2=ply-2
      if(lim1 .gt. lim2) go to 520
          do 510 level=lim1,lim2,2
              bound=max0(bound,side*value(level))
510       continue
520   continue
      if(side*tbound .gt. bound) go to 9999
      value(ply)=side*bound
      go to 9998
c
c------------------------------< the current position has been found.
c------------------------------< however, the score is only known to
c------------------------------< be outside some alpha/beta bound. if
c------------------------------< the stored bound is outside the current
c------------------------------< bound, a cutoff will occur.
c
c
600   continue
      if(mdepth-mply .lt. depth-ply) go to 9999
      play=2-mod(player+1,2)
      mside=-side
      bound=mside*value(play)
      temp=play+2
      if(temp .gt. ply) go to 620
          do 610 level=temp,ply,2
              bound=max0(bound,mside*value(level))
610       continue
620   continue
      if(mside*tbound .gt. bound) go to 9999
      go to 9997
c
c------------------------------< return
c
9997  continue
      return=2
      return
9998  continue
      return=1
      return
9999  continue
      return=0
      return
      end



