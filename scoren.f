      subroutine scoren
c
c     ******************************************************************
c     *                                                                *
c     *      scoren is used to evaluate the positional score for all   *
c     *  knights.  there are four major terms that are examined:       *
c     *  center tropism (closeness to center of the board), king       *
c     *  tropism (closeness to the opponent's king), outpost knight    *
c     *  (knight on ranks 5-7 supported by a pawn) and development.    *
c     *  any knight still on the back rank before move 16 is penalized *
c     *  for not being developed.  this causes the pieces to be de-    *
c     *  veloped rapidly before tactical skirmishes occur.             *
c     *                                                                *
c     ******************************************************************
c
      implicit integer (a-z)
      common /board/ board(120)
      common /kloc/ pkingl, prank, pfile, okingl, orank, ofile
      common /pawns/ pfirst(10), plast(10), pcount(10), ppass(10),
     *               ofirst(10), olast(10), ocount(10), opass(10),
     *               arank, afile
      common /scorcm/ sign, square, rank, file
      common /movcnt/ npmovs, nomovs
      common /mscore/ sscore, mscore, pscore, tscore
c
c------------------------------< evaluate center tropism,
c
      pscore=pscore+sign*6*(30-3*iabs(11-2*rank)-2*iabs(11-2*file))
c
c------------------------------< evaluate king tropism.
c
      if(sign .lt. 0) go to 10
          pscore=pscore+3*(5-iabs(orank-rank)-iabs(ofile-file))
          go to 20
10    continue
          pscore=pscore-3*(5-iabs(prank-rank)-iabs(pfile-file))
20    continue
c
c------------------------------< evaluate outpost knights. the knight
c------------------------------< must be on the 5th or 6th rank and
c------------------------------< must be in a 'hole' in the opponent's
c------------------------------< pawns to get a bonus. a larger bonus
c------------------------------< can be had if the knight is supported
c------------------------------< by a pawn. an additional bonus is
c------------------------------< given if a pawn sitting on the knight's
c------------------------------< square would be passed making it a
c------------------------------< difficult decision to capture the
c------------------------------< knight.
c
      if(sign .lt. 0) go to 30
      if(rank.lt.6 .or. rank.gt.7) go to 40
      if(file.lt.4 .or. file.gt.7) go to 40
      if(ofirst(file-1) .gt. rank) go to 40
      if(ofirst(file+1) .gt. rank) go to 40
          pscore=pscore+45
          if(board(square-9) .eq. 1) go to 25
          if(board(square-11) .ne. 1) go to 40
25        continue
              pscore=pscore+30
              if(ocount(file) .ne. 0) go to 40
              if(board(square-9) .ne. 1) go to 40
              if(board(square-11) .ne. 1) go to 40
                  pscore=pscore+50
                  go to 40
30    continue
      if(rank.gt.5 .or. rank.lt.4) go to 40
      if(file.lt.4 .or. file.gt.7) go to 40
      if(pfirst(file-1) .lt. rank) go to 40
      if(pfirst(file+1) .lt. rank) go to 40
          pscore=pscore-45
          if(board(square+9) .eq. -1) go to 35
          if(board(square+11) .ne. -1) go to 40
35        continue
              pscore=pscore-30
              if(pcount(file) .ne. 0) go to 40
              if(board(square+9) .ne. -1) go to 40
              if(board(square+11) .ne. -1) go to 40
                  pscore=pscore-50
40    continue
c
c------------------------------< penalize a knight that is on the
c------------------------------< edge of the board since it has
c------------------------------< little mobility. remember the
c------------------------------< chess proverb, 'a knight on the
c------------------------------< rim is dim'.
c
      if(rank.eq.2 .or. rank.eq.9 .or.
     *   file.eq.2 .or. file.eq.9)
     *                     pscore=pscore-sign*100
c
c------------------------------< determine if the knight is in
c------------------------------< front of a friendly king or
c------------------------------< queen pawn which hasn't been
c------------------------------< moved yet. if so, a penalty
c------------------------------< is in order to avoid blocking
c------------------------------< the pawn.
c
      if(sign .lt. 0) go to 50
          if(square.eq.45 .and. board(35).eq.1) pscore=pscore-100
          if(square.eq.46 .and. board(36).eq.1) pscore=pscore-100
          go to 60
50    continue
          if(square.eq.75 .and. board(85).eq.-1) pscore=pscore+100
          if(square.eq.76 .and. board(86).eq.-1) pscore=pscore+100
60    continue
      return
      end


