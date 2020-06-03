      subroutine scorem
c
c     ******************************************************************
c     *                                                                *
c     *      scorem is a special scoring function used to force check- *
c     *  mate.  it is called whenever there are no pawns left to       *
c     *  promote.  scorem tries to accomplish three goals:             *
c     *                                                                *
c     *      1)  drive the losing king to the edge of the board and    *
c     *          eventually to a corner.                               *
c     *                                                                *
c     *      2)  attract all pieces toward the kings for offense       *
c     *          or defense.                                           *
c     *                                                                *
c     *      3)  keep the winning king two squares away from the       *
c     *          edge of the board to make checkmating easier.         *
c     *                                                                *
c     *      the reason for having no pawns is simplicity.  it is much *
c     *  easier to deliver mate with one or more queens than with only *
c     *  a rook and bishop when both sides have material.  it is also  *
c     *  easy to blunder and let the losing side queen a pawn and      *
c     * clutch defeat from the jaws of victory.                        *
c     *                                                                *
c     ******************************************************************
c
      implicit integer (a-z)
      common /board/ board(120)
      common /mscore/ sscore, mscore, pscore, tscore
      common /k loc cm/ pkingl, prank, pfile,  okinql, orank, ofile
c
c------------------------------< determine the rank and file of
c------------------------------< the winning and losing side and
c------------------------------< the color (sign) of the winning
c------------------------------< side
c
      if(mscore .lt. 0) go to 100
         sign=1
         wrank=prank
         wfile=pfile
         lrank=orank
         lfile=ofile
         go to 200
100   continue
         sign=-1
         wrank=orank
         wfile=ofile
         lrank=prank
         lfile=pfile
200   continue
c
c------------------------------< the king on the side with less material
c------------------------------< should try to stay in the center of the
c------------------------------< board and avoid the edges and
c------------------------------< particularly avoid the corners.
c
      pscore=pscore+sign*50*(iabs(lrank*2-11)+iabs(lfile*2-11))
c
c------------------------------< all pieces should move toward the
c------------------------------< losing king for attack or defense.
c
      do 500 square=22,99
          piece=board(square)
          if(piece .gt. 6) go to 500
          if(piece .eq. 0) go to 500
              rank=square/10
              file=mod(square,10)
              pscore=pscore-sign*5*(iabs(lfile-file)+iabs(lrank-rank))
500   continue
c
c------------------------------< the winning king should try to stay
c------------------------------< two squares away from the edge of the
c------------------------------< board to make mates easier.
c
      if(wfile.lt.4 .or. wfile.gt.7) go to 600
      if(wrank.lt.4 .or. wrank.gt.7) go to 600
          pscore=pscore+sign*15
600   continue
c
c------------------------------< the king should try to stay on
c------------------------------< a rank or file two squares away
c------------------------------< from the edge of the board so
c------------------------------< that checkmate is easier to find.
c
      if(wfile.eq.4 .or. wfile.eq.7) pscore=pscore+sign*5
      if(wrank.eq.4 .or. wrank.eq.7) pscore=pscore+sign*5
700   continue
      return
      end

