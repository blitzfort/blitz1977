      integer function ripoff(square)
c
c     ******************************************************************
c     *                                                                *
c     *      ripoff is used to evaluate the safety of a piece located  *
c     *  on 'square'.  it determines if the piece located on 'square'  *
c     *  can be profitably captured by the opposing side.  it eval-    *
c     *  uates the pieces bearing on 'square' and simulates a series   *
c     *  of captures to see if capturing the piece will win or lose    *
c     *  material.  this check is a cursory examination, pieces that   *
c     *  are pinned or overloaded are still considred to bear on the   *
c     *  square even though their participation in the sequence of ex- *
c     *  changes might result in a greater loss elsewhere on the board.*
c     *                                                                *
c     ******************************************************************
c
      implicit integer (a-z)
      integer pmoves(10), omoves(10), list(20)
      logical player
      logical pqueen, oqueen
      common /board/ board(120)
      common /movdir/ movdir(28), piecem(28), begin(6), end(6)
      common /pieccm/ pieces(6)
c
c------------------------------< initialize.
c
      ripoff=0
      pmovs=0
      omovs=0
c
c------------------------------< pawn captures
c
      if(board(square-9) .ne. 1) go to 1
          pmovs=pmovs+1
          pmoves(pmovs)=pieces(1)
1     continue
      if(board(square-11) .ne. 1) go to 2
          pmovs=pmovs+1
          pmoves(pmovs)=pieces(1)
2     continue
      if(board(square+9) .ne. -1) go to 3
          omovs=omovs+1
          omoves(omovs)=pieces(1)
3     continue
      if(board(square+11) .ne. -1) go to 4
          omovs=omovs+1
          omoves(omovs)=pieces(1)
4     continue
c
c------------------------------< knight captures
c
      do 6 i=9,16
          loc=square+movdir(i)
          if(board(loc) .ne. 2)  go to 5
              pmovs=pmovs+1
              pmoves(pmovs)=pieces(1)
              go to 6
5         continue
          if(board(loc) .ne. -2) go to 6
              omovs=omovs+1
              omoves(omovs)=pieces(1)
6     continue
c
c------------------------------< bishop captures
c
      pqueen=.false.
      oqueen=.false.
      do 12 i=5,8
          direc=movdir(i)
          loc=square
9         loc=loc+direc
          temp=board(loc)
          if(temp .gt. 6) go to 12
          index=temp+7
          go to (12,111,12,11,12,12,9,12,12,10,12,101,12),index
10        continue
              pmovs=pmovs+1
              pmoves(pmovs)=pieces(iabs(temp))
              go to 9
101       continue
              pqueen=.true.
              go to 9
11        continue
              omovs=omovs+1
              omoves(omovs)=pieces(iabs(temp))
              go to 9
111       continue
              oqueen=.true.
              go to 9
12    continue
c
c------------------------------< rook captures
c
      do 16 i=1,4
          direc=movdir(i)
          loc=square
13        loc=loc+direc
          temp=board(loc)
          if(temp .gt. 6) go to 16
          index=temp+7
          go to (16,151,15,16,16,16,13,16,16,16,14,141,16),index
14        continue
              pmovs=pmovs+1
              pmoves(pmovs)=pieces(iabs(temp))
              go to 13
141       continue
              pqueen=.true.
              go to 13
15        continue
              omovs=omovs+1
              omoves(omovs)=pieces(iabs(temp))
              go to 13
151       continue
              oqueen=.true.
              go to 13
16    continue
c
c------------------------------< queen captures
c
      if(.not. pqueen) go to 17
          pmovs=pmovs+1
          pmoves(pmovs)=pieces(5)
17    continue
      if(.not. oqueen) go to 18
          omovs=omovs+1
          omoves(omovs)=pieces(5)
18    continue
c
c------------------------------< king captures
c
      do 20 i=17,24
          loc=square+movdir(i)
          if(board(loc) .ne. 6) go to 19
              pmovs=pmovs+1
              pmoves(pmovs)=pieces(6)
              go to 20
19        continue
          if(board(loc) .ne. -6) go to 20
              omovs=omovs+1
              omoves(omovs)=pieces(6)
20    continue
c
c------------------------------< after storing the moves, evaluate the
c------------------------------< exchanges that are possible to
c------------------------------< determine the loss/gain.
c
      ipmovs=0
      iomovs=0
      list(1)=0
      sign=1
      mdepth=1
      piece=pieces(iabs(board(square)))
      player=board(square) .lt. 0
c
c------------------------------< change sides
c
100   continue
          player=.not. player
          if(player) go to 250
c
c------------------------------< it is the machines turn to capture.
c------------------------------< if it doesn't have any more pieces
c------------------------------< bearing on the squrae, time to quit.
c
150           continue
              ipmovs=ipmovs+1
              if(ipmovs.gt.pmovs) go to 400
              tpiece=pmoves(ipmovs)
              go to 300
c
c------------------------------< it is the humans turn to capture,
c------------------------------< if he doesn't have any more pieces
c------------------------------< bearing on the square, time to quit.
c
250       continue
          iomovs=iomovs+1
          if(iomovs.gt.omovs) go to 400
          tpiece=omoves(iomovs)
c
c------------------------------< perform all indicated captures, by
c------------------------------< minimaxing the scores.
c
300       mdepth=mdepth+1
          list(mdepth)=list(mdepth-1)+sign*piece
          piece=tpiece
          sign=-sign
      go to 100
c
c------------------------------< now that scoring is complete, scan
c------------------------------< the exchange list to determine when
c------------------------------< the sequence of exchanges would
c------------------------------< stop.
c
400   continue
      if(mdepth .le. 1) go to 9999
      sign=1
      if(mod(mdepth,2) .eq. 0) sign=-1
500   continue
          if(mdepth .eq. 2) go to 700
          if(sign*list(mdepth)  .gt. sign*list(mdepth-1))  go to 600
              list(mdepth-1)=list(mdepth)
600       continue
          mdepth=mdepth-1
          sign=-sign
      go to 500
c
c------------------------------< the exchange sequences have been
c------------------------------< evaluated.  now return the expected
c------------------------------< gain.
c
700   continue
      ripoff=list(2)
9999  continue
      return
      end


