      subroutine scorer
c
c     ******************************************************************
c     *                                                                *
c     *      scorer is used to evaluate the positional score for all   *
c     *  rooks.  there are six major terms that are examined:  king    *
c     *  tropism (closeness to the opponent's king), open and half-    *
c     *  open files (files with no pawns or no friendly pawns). having *
c     *  a rook behind a passed pawn to support it's advance, occu-    *
c     *  pation of the seventh rank depending on how many enemy pawns  *
c     *  are on it, mobility (how many empty squares the rook attacks) *
c     *  and development where the rook should not move until the      *
c     *  king has castled.                                             *
c     *                                                                *
c     ******************************************************************
c
      implicit integer (a-z)
      logical ppass, opass
      logical attack, pmoved, moved
      common /board/ board(120)
      common /depth/ sdepth,  depth, ply
      common /pawns/ pfirst(10), plast(10), pcount(10), ppass(10),
     *               ofirst(10), olast(10), ocount(10), opass(10),
     *               arank, afile
      common /kloc/ pkingl, prank, pfile, okingl, orank, ofile
      common /scorcm/ sign, square, rank, file
      common /mscore/ sscore, mscore, pscore, tscore
      common /castcm/ pmoved(3,2), moved(3,30)
      common /movdir/ movdir(28), piecem(28), begin(6), end(6)
c
c------------------------------< determine how much mobility this
c------------------------------< rook has by counting the squares
c------------------------------< it can move to.  also, determine if
c------------------------------< another friendly rook is on the same
c------------------------------< file (doubled rooks), add in a bonus
c------------------------------< if one is found.
c
      double=0
      conect=0
      moves=0
      do 20 i=1,4
          dir=movdir(i)
          loc=square
10        continue
              loc=loc+dir
              temp=board(loc)
              if(temp .eq. 99) go to 20
                  temp=temp*sign
                  if(temp .lt. -3) go to 13
                  if(temp .gt. 1) go to 11
                      if(temp .gt. 0) go to 15
                      if(sign*board(loc+sign*9) .eq. -1) go to 10
                      if(sign*board(loc+sign*11) .eq. -1) go to 10
11                    continue
                          moves=moves+1
                          go to 10
13                continue
                      moves=moves+2
                      if(temp .lt. -4) go to 10
                      go to 20
15        continue
              if(i .gt. 2) go to 18
              if(temp*sign .eq. 4) double=20
              go to 20
18        continue
              if(temp*sign .eq. 4) conect=50
20    continue
      pscore=pscore+sign*4*(moves-7)
c
c------------------------------< evaluate king tropism.
c
      if(sign .lt. 0) go to 30
          pscore=pscore-6*min0(iabs(orank-rank),iabs(ofile-file))
          go to 40
30    continue
          pscore=pscore+6*min0(iabs(prank-rank),iabs(pfile-file))
40    continue
c
c------------------------------< evaluate half/open and open files
c------------------------------< by checking the file's pawn count.
c------------------------------< add in a bonus for doubling rooks
c------------------------------< only if they are on a file with no
c------------------------------< pawns.
c
      if(pcount(file)+ocount(file) .ne. 0) go to 50
          pscore=pscore+sign*(50+double)
          go to 70
50    continue
      if(sign .lt. 0) go to 60
          if(pcount(file) .ne. 0) go to 70
              pscore=pscore+20
              go to 70
60    continue
          if(ocount(file) .ne. 0) go to 70
              pscore=pscore-20
70    continue
c
c------------------------------< evaluate the usefulness of
c------------------------------< a rook on the 7th rank.
c
      if(sign .lt. 0) go to 90
          if(rank .ne. 8) go to 120
          count=0
          do 80 sq=82,89
              if(board(sq) .eq. -1) count=count+1
80        continue
          go to 110
90        continue
          if(rank .ne. 3) go to 120
          count=0
          do 100 sq=32,39
              if(board(sq) .eq. 1) count=count+1
100       continue
110   continue
      if(count .eq. 1) pscore=pscore+sign*(25*conect)
      if(count .eq. 2) pscore=pscore+sign*(50+conect*2)
      if(count .eq. 3) p8core=pscore+sign*(100*conect*4)
      if(count .ge. 4) pscore=pscore+sign*(150*conect*7)
120   continue
c
c------------------------------< evaluate the usefullness of
c------------------------------< having a rook behind a passed
c------------------------------< pawn to support it.
c
      if(.not. ppass(file)) go to 130
          if(rank .gt. plast(file)) go to 130
              pscore=pscore+sign*2*plast(file)**2
              go to 140
130   continue
      if(.not. opass(file)) go to 140
          if(rank .lt. olast(file)) go to 140
              pscore=pscore+sign*2*(11-olast(file))**2
140       continue
      return
      end


