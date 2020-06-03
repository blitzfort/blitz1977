      subroutine store(smove,stype)
c
c     ******************************************************************
c     *                                                                *
c     *      store is used to enter positions into the hash table      *
c     *  for later use by 'lookup'. if a collision occurs, the current *
c     *  position will overlay the table  position if the table        *
c     *  position is from an earlier iteration or if the current       *
c     *  position is at a lower (numeric) depth than the table         *
c     *  position.                                                     *
c     *      store uses the random probe algorithm to resolve all      *
c     *  collisions.  it tries to resolve collisions in two passes;    *
c     *  pass one attempts to find a slot that is empty or that was    *
c     *  stored from an earlier move.  pass two tries to find a        *
c     *  position stored during the current move that was stored       *
c     *  from a greater depth than the current search depth.  the      *
c     *  entire table  is not searched, just n random entries (n=25).  *
c     *                                                                *
c     ******************************************************************
c
      implicit integer (a-z)
      logical mated, failed
      common /board/ board(120)
      common /hash cm/ random(100,13), hboard
      common /htable/ hsize, htable(32768)
      common /srchcm/ value(30),  from(30), to(30), type(30), cappc(30)
      common /depth/ sdepth, depth, ply
      common /move cm/ side, player, square, mpiece
      common /mov cnt/ npmovs, nomovs
      common /window/ window(2)
      common /statcm/ nodes(30), times(30), aborts(30), tnodes, snodes,
     *                 hashes, pright
      common /mated/ mated(30)
      common /prev mv/ prevmv(6)
c
c------------------------------< hash the current position to get the
c------------------------------< pointer to the hash table.
c
      hboard=0
      do 50 square=22,99
          temp=board(square)+7
          if(temp.eq.7 .or. temp.gt.13) go to 50
              hboard=ieor(hboard,random(square,temp))
50    continue
      if(ply .gt. 1) go to 110
          tosq=prevmv(5)
           fromsc=prevmv(4)
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
c------------------------------< in the table.  for collisions, use
c------------------------------< the random probe technique to search
c------------------------------< 25 additional entries in an attempt
c------------------------------< to locate the correct one.
c
      failed=.false.
200   continue
          rkey=hkey
          rindex=1
          colsns=25
300       continue
              rkey=(rkey+(player-1)*hsize)*4
              if(hboard .eq. htable(rkey+1)) go to 600
              mmove=htable(rkey+2)/65536
              if(mmove .ne. npmovs+nomovs) go to 700
              if(.not. failed) go to 400
                  mdepth=htable(rkey+2)/32
                  mdepth=mod(mdepth,32)
                  mply=mod(htable(rkey+2),32)
                  if(mdepth-mply .lt. depth-ply) go to 700
400           continue
c
c------------------------------< a collision has occured. determine
c------------------------------< if another probe should be made. if
c------------------------------< so, compute the next hash key and try
c------------------------------< again for a match.
c
              colsns=colsns-1
              if(colsns .lt. 0) go to 500
              rindex=mod(5*rindex,hsize*4)
              if(rindex .eq. 1) go to 500
              rkey=hkey+rindex/4
              if(rkey .gt. hsize-1) rkey=rkey-hsize
          go to 300
c
c------------------------------< the first pass failed, now try to
c------------------------------< find any slot that is usable, not
c------------------------------< just an empty slot.
c
500       continue
          if(failed) return
          failed=.true.
      go to 200
c
c------------------------------< this position is already in the table,
c------------------------------< only store the one from the lowest
c------------------------------< ply as it is the most useful.
c
600   continue
      mdepth=htable(rkey+2)/32
      mdepth=mod(mdepth,32)
      mply=mod(htable(rkey+2),32)
      if(mdepth-mply .le. depth-ply) go to 700
      mtype=htable(rkey+2)/1024
      mtype=mod(mtype,4)
      if(mtype .eq. 1) return
c
c------------------------------< this position should be replaced by
c------------------------------< the current board position. first,
c------------------------------< store the hashed board position into
c------------------------------< the first word.
c
700   continue
      htable(rkey+1)=hboard
c
c------------------------------< store the current iteration depth
c------------------------------< and current search depth in word 2.
c
      mmated=1
      if(mated(ply)) mmated=2
      htable(rkey+2)=ply+depth*32+mmated*4096+(npmovs+nomovs)*65536
c
c------------------------------< store the suggested move from this
c------------------------------< position for use in phase 2 if this
c------------------------------< position occurs again.
c
      htable(rkey+3)=smove
      if(stype .eq. 1) go to 900
      if(value(ply) .le. window(1)) go to 800
      if(value(ply) .ge. window(2)) go to 800
c
c------------------------------< the backed up score is usable as is,
c------------------------------< enter it into the hash table along with
c------------------------------< a 'completely useful' flag.
c
      htable(rkey+2)=htable(rkey+2)+1*1024
      htable(rkey+4)=value(ply)
      return
c
c------------------------------< a window is being backed up, remember
c------------------------------< it as a lower or upper bound.
c
800   continue
      htable(rkey+2)=htable(rkey+2)+2*1024
      bound=side*value(player)
      temp=player+2
      if(temp .gt. ply) go to 820
          do 810 level=temp,ply,2
              bound=max0(bound,side*value(level))
810       continue
820   continue
      htable(rkey+3)=0
      htable(rkey+4)=side*bound
      return
c
c------------------------------< the backed up score is partially
c------------------------------< usable. it is only known that the score
c------------------------------< is better or worse than some search
c------------------------------< bound. therefore, remember the lowest
c------------------------------< upper bound or the highest lower bound
c------------------------------< for possible use later.
c
900   continue
      play=2-mod(player+1,2)
      mside=-side
      bound=mside*value(play)
      temp=play+2
      if(temp .gt. ply) go to 920
          do 910 level=temp,ply,2
              bound=max0(bound,mside*value(level))
910       continue
920   continue
      htable(rkey+2)=htable(rkey+2)+3*1024
      htable(rkey+4)=mside*bound
      return
      end
