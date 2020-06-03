      logical function repchk(count)
c
c     ******************************************************************
c     *                                                                *
c     *      repchk is used to detect a draw by repetition.  it        *
c     *  saves the current position in the position table each time    *
c     *  it is called.  the position that is destroyed is based on     *
c     *  the depth of the search in progress.  if a position at ply 5  *
c     *  was stored earlier, in this search, that position will be     *
c     *  the one that will be replaced.                                *
c     *      repchk then scans the table to determine how many times   *
c     *  this position has occured before.   more than once will be    *
c     *  treated as a draw by 'search', while 3 or more times will     *
c     *  be treated as a draw by 'main'.                               *
c     *                                                                *
c     ******************************************************************
c
      implicit integer (a-z)
      common /board/ board(120)
      common /dup/ bdsave(8,130), point
      common /depth/ sdepth, depth, ply
      common /rpt cm/ rmoves(2)
c
c------------------------------< initialize.
c
      repchk=.false.
c
c------------------------------< compress the board into the next
c------------------------------< slot in the position table.
c
      temp=point+ply
      if(temp .gt. 130) temp=temp-130
      sq=21
      do 200 i=1,8
          btemp=0
          do 100 j=1,8
              sq=sq+1
              btemp=btemp*16+board(sq)+7
100       continue
          bdsave(i,temp)=btemp
          sq=sq+2
200   continue
c
c------------------------------< now clobber any positions that were
c------------------------------< entered during deeper search that
c------------------------------< really no longer exist.
c
      lim1=temp
      lim2=point+30
      if(lim2 .gt. 130) lim2=lim2-130
300   continue
          lim1=lim1+1
          if(lim1 .gt. 130) lim1=lim1-130
          bdsave(1,lim1)=-1
      if(lim1 .ne. lim2) go to 300
c
c------------------------------< scan prior positions list to
c------------------------------< determine if this position has
c------------------------------< occured before
c
      count=1
      start=2-mod(temp,2)
      do 500 i=start,130,2
          if(i .eq. temp) go to 500
          do 400 j=1,8
              if(bdsave(j,temp) .ne. bdsave(j,i)) go to 500
400       continue
          rmoves(count)=(i+1)/2
          count=count+1
500   continue
      if(count .lt. 2) return
      repchk=.true.
      return
      end
