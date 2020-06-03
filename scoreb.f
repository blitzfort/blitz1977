      subroutine scoreb
c
c     ******************************************************************
c     *                                                                *
c     *      scoreb is used to evaluate the positional score for all   *
c     *  bishops.  there are two major terms that are examined;  the   *
c     *  number of squares directly attacked (unblocked) and de-       *
c     *  velopment.  any bishop still on the back rank before move     *
c     *  16 is penalized for not being developed.  this causes the     *
c     *  pieces to be developed rapidly before any tactical skir-      *
c     *  mishes occur.  attract bishops to a diagonal that bears       *
c     *  directly on the opponent's king.  this encourages tactics     *
c     *  and also posts bishops where they will have a lasting effect  *
c     *  on the game.                                                  *
c     *                                                                *
c     ******************************************************************
c
      implicit integer (a-z)
      common /board/ board(120)
      common /movdir/ movdir(28), piecem(28), begin(6), end(6)
      common /mov cnt/ npmovs, nomovs
      common /scor cm/ sign, square, rank, file
      common /mscore/ sscore, mscore, pscore, tscore
      common /k loc cm/ pkingl, prank, pfile, okingl, orank, ofile
c
c------------------------------< add a bonus for each bishop found since
c------------------------------< a bishop is worth more than a knight
c------------------------------< due to greater mobility and range.
c
      pscore=pscore+sign*200
c
c------------------------------< determine how much mobility this
c------------------------------< bishop has by counting the squares
c------------------------------< it can move to.
c
      moves=0
      do 20 i=5,8
          dir=movdir(i)
          loc=square
10        continue
              loc=loc+dir
              temp=board(loc)
              if(temp .eq. 99) go to 20
                  temp=temp*sign
                  if(temp .lt. -1) go to 13
                  if(temp .gt. 1) go to 11
                      if(temp .gt. 0) go to 20
                      if(sign*board(loc+sign*9) .eq. -1) go to 20
                      if(sign*board(loc+sign*11) .eq. -1) go to 20
11                    continue
                          moves=moves+1
                          go to 10
13                continue
                      moves=moves+2
                      if(temp .lt. -3) go to 10
20    continue
      pscore=pscore+sign*6*(moves-7)
c
c------------------------------< evaluate the diagonals that the bishop
c------------------------------< is on to determine how blocked they are
c------------------------------< by pawns. give a penalty for each pawn
c------------------------------< over 2 on the diagonls.
c
      pawns=0
      do 40 i=5,8
          dir=movdir(i)
          loc=square
30        continue
              loc=loc+dir
              temp=iabs(board(loc))
              if(temp .eq. 99) go to 40
              if(temp .eq. 1) pawns=pawns+1
          go to 30
40    continue
      pscore=pscore+sign*6*(2-pawns)
c
c------------------------------< attract bishops to the diagonals
c------------------------------< that directly bear on the opponent's
c------------------------------< king.  include a penalty for each
c------------------------------< diagonal away from the king diagonal
c
      if(sign .lt. 0) go to 50
          diag=iabs(iabs(ofile-mod(square,10))-iabs(orank-square/10))
          go to 60
50    continue
          diag=iabs(iabs(pfile-mod(square,10))-iabs(prank-square/10))
60    continue
      pscore=pscore-sign*6*diag
c
c------------------------------< evaluate development, until
c------------------------------< move 16. any bishops on the
c------------------------------< back rank are considered un-
c------------------------------< developed and receive a severe
c------------------------------< penalty.
c
      if(npmovs+nomovs .gt. 30) go to 80
      if(sign .lt. 0) go to 70
          if(rank .eq. 2) pscore=pscore-60
          go to 80
70    continue
          if(rank .eq. 9) pscore=pscore+60
80    continue
c
c------------------------------< determine if the bishop is in
c------------------------------< front of a friendly king or
c------------------------------< queen pawn which hasn't been
c------------------------------< moved yet. if so, a penalty
c------------------------------< is in order to avoid blocking
c------------------------------< the pawn.
c
      if(sign .lt. 0) go to 90
          if(square.eq.45 .and. board(35).eq.1) pscore=pscore-100
          if(square.eq.46 .and. board(36).eq.1) pscore=pscore-100
          go to 100
90    continue
          if(square.eq.75 .and. board(85).eq.-1) pscore=pscore+100
          if(square.eq.76 .and. board(86).eq.-1) pscore=pscore+100
100   continue
      return
      end

