      subroutine scorek
c
c     ******************************************************************
c     *                                                                *
c     *      scorek is used to evaluate the positional score for all   *
c     *  kings.  the king is encouraged to stay at r1, r2, n1, or n2   *
c     *  after the rook has moved.  the pawn structure in front of the *
c     *  king is analyzed in an attempt to keep all pawns on their     *
c     *  original squares when possible.  the king is also encouraged  *
c     *  to stay near friendly pieces/pawns rather than by itself where*
c     *  it would have no protection.  after most of the pieces have   *
c     *  been exchanged, the king is encouraged to come out and advance*
c     *  toward the center of the board and toward any pawns that      *
c     *  are left.                                                     *
c     *                                                                *
c     ******************************************************************
c
      implicit integer (a-z)
      logical pmoved, moved
      integer emptsc(4)
      common /board/ board(120)
      common /depth/ sdepth, depth, ply
      common /movdir/ movdir(28), piecem(28), begin(6), end(6)
      common /scorcm/ sign, square, rank, file
      common /pawns/ pfirst(10), plast(10), pcount(10), ppass(10),
     *               ofirst(10), olast(10), ocount(10), opass(10),
     *               arank, afile
      common /castcm/ pmoved(3,2), moved(3,30)
      common /pieces/ nppwns, nppcs, pqueen, nopwns, nopcs, oqueen
      common /kloc/ pkingl, prank, pfile, okingl, orank, ofile
      common /tree/ moves(2000), first(30), last(30), which(30),
     *              inchk(30), givchk(30)
      common /srchcm/ value(30), from(30), to(30), type(30), cappc(30)
      common /mscore/ sscore, mscore, pscore, tscore
      common /types/ normal, castkg, castqn, enpass, promot
      data emptsc / 3, 0, 8, 12 /
c
c------------------------------< determine if the king should
c------------------------------< be concerned about safety by
c------------------------------< counting enemy pieces and the
c------------------------------< queen.
c
      if(sign .lt. 0) go to 101
          safe=nopcs+oqueen-3
          go to 201
101   continue
           safe=nppcs+pqueen-3
201   continue
       if(safe .le. 2) go to 1000
c
c------------------------------< encourage the king to stay at
c------------------------------< r1, r2, n1, or n2 after the
c------------------------------< rook has left r1.  this will give
c------------------------------< a bonus for castling, also penalize
c------------------------------< a rook pawn for advancing more than one
c------------------------------< rank and a knight pawn for moving.
c
      safety=0
      if(moved(1,ply)) go to 100
          if(file .eq. 6) go to 40
              if(.not.  moved(2,ply)) go to 100
              if(.not.  moved(3,ply)) go to 110
                  go to 200
40         continue
              if(.not.  moved(3,ply)) go to 110
              if(.not.  moved(2,ply)) go to 100
                  go to 200
100    continue
       if(file .gt. 3) go to 110
       if(sign .lt. 0) go to 150
           safety=5*(8-rank)
           if(pfirst(2) .gt. 4) safety=safety-10
           if(pfirst(3) .gt. 3) safety=safety-15
           if(pfirst(3).gt.3 .and. pfirst(4).gt.3) safety=safety-10
           go to 200
110    continue
       if(file .lt. 8) go to 200
       if(sign .lt. 0) go to 160
           safety=5*(8-rank)
           if(pfirst(9) .gt. 4) safety=safety-10
           if(pfirst(8) .gt. 3) safety=safety-15
           if(pfirst(8).gt.3 .and. pfirst(7).gt.3) safety=safety-10
          go to 200
150    continue
          safety=5*(rank-3)
           if(ofirst(2) .lt. 7) safety=safety-10
           if(ofirst(3) .lt. 8) safety=safety-15
           if(ofirst(3).lt.8 .and. ofirst(4).lt.8) safety=safety-10
          go to 200
160   continue
          safety=5*(rank-3)
          if(ofirst(9) .lt. 7) safety=safety-10
          if(ofirst(8) .lt. 8) safety=safety-15
          if(ofirst(8).lt.8 .and. ofirst(7).lt.8) safety=safety-10
200   continue
c
c------------------------------< the king should be encouraged to
c------------------------------< stay near to friendly pawns and
c------------------------------< pieces rather than be in a barren
c------------------------------< location subjec to attacks.
c
      piecec=0
      do 300 i=17,24
          piece=board(square+movdir(i))
          if(piece.eq.0 .or. piece.gt.6) go to 300
              piece=piece*sign
              if(piece .gt. 0) piecec=piecec+1
300   continue
      if(piecec .lt. 3) safety=safety-5
      if(piecec .lt. 2) safety=safety-5
c
c------------------------------< examine the king's file and both
c------------------------------< adjacent files to determine if they
c------------------------------< are open. if so, penalize the score
c------------------------------< since rooks/queens have an avenu
c------------------------------< of attack.
c
      if(sign .lt. 0) go to 400
          if(pcount(pfile) .eq. 0) safety=safety-10
          if(pfile.gt.2 .and. pcount(pfile-1).eq.0) go to 350
          if(pfile.lt.9 .and. pcount(pfile+1).eq.0) go to 350
          go to 500
350       continue
              safety=safety-5
          go to 500
400   continue
          if(ocount(ofile) .eq. 0) safety=safety-10
          if(ofile.gt.2 .and. ocount(ofile-1).eq.0) go to 450
          if(ofile.lt.9 .and. ocount(ofile+1).eq.0) go to 450
          go to 500
450       continue
              safety=safety-5
500   continue
c
c------------------------------< penalize a king that is trapped on
c------------------------------< the back rank with no 'breathing
c------------------------------< space' on the rank in front of it.
c------------------------------< that is, avoid back rank mating
c------------------------------< situations whenever possible.
c
      if(sign .lt. 0) go to 600
          if(rank .ne. 2) go to 700
          empty=1
          do 610 i=9,11
              if(board(pkingl+i) .eq. 0) empty=empty+1
610       continue
          safety=safety-emptsc(empty)
          go to 700
600   continue
          if(rank .ne. 9) go to 700
          empty=1
          do 710 i=9,11
              if(board(okingl-i) .eq. 0) empty=empty+1
710       continue
          safety=safety-emptsc(empty)
700   continue
c
c------------------------------< determine if the files adjacent to
c------------------------------< the king are half open.  if so,
c------------------------------< penalize the score for each file.
c
      if(sign .lt. 0) go to 800
          start=pfile-1
          stop=pfile+1
          if(start .lt. 2) start=2
          if(stop .gt. 9) stop=9
          do 810 kfile=start,stop
              if(ocount(kfile) .eq. 0) safety=safety-5
810       continue
          go to 900
800   continue
          start=ofile-1
          stop=ofile+1
          if(start .lt. 2) start=2
          if(stop .gt. 9) stop=9
          do 910 kfile=start,stop
              if(pcount(kfile) .eq. 0) safety=safety-3
910        continue
900   continue
c
c------------------------------< now add the safety score to
c------------------------------< the positional score.
c
      pscore=pscore+sign*safety*safe
      go to 9999
c
c------------------------------< king safety is not important,
c------------------------------< attract the king to the center
c------------------------------< of the board and to the center
c------------------------------< of the pawns.
c
1000  continue
      if(sign .lt. 0) go to 1100
          center=iabs(11-2*prank)+iabs(11-2*pfile)
          go to 1200
1100  continue
          center=iabs(11-2*orank)+iabs(11-2*ofile)
1200  continue
      pscore=pscore-sign*6*center
c
c------------------------------< attract the king to the center of
c------------------------------< the pawns left on the board.
c
      if(sign .lt. 0) go to 1300
          center=iabs(arank-prank)+iabs(afile-pfile)
          go to 1400
1300  continue
          center=iabs(arank-orank)+iabs(afile-ofile)
1400  continue
      pscore=pscore-sign*20*center
9999  continue
      return
      end


