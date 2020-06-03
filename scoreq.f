      subroutine scoreq
c
c     ******************************************************************
c     *                                                                *
c     *      scoreq is used to evaluate the positional score for all   *
c     *  queens.  there is only one term that is examined:  king       *
c     *  tropism (closeness to the opponent's king).  the queen is     *
c     *  attracted to the opponent's king to maintain constant         *
c     *  tactical threats via checks and forks.                        *
c     *                                                                *
c     ******************************************************************
c
      implicit integer (a-z)
      common /k loc cm/ pkingl, prank, pfile, okingl, orank, ofile
      common /scor cm/ sign, square, rank, file
      common /mscore/ sscore, mscore, pscore, tscore
c
c------------------------------< evaluate king tropism.
c
      if(sign .lt. 0) go to 10
          pscore=pscore-6*iabs(orank-rank)
          pscore=pscore-10*iabs(ofile-file)
          go to 20
10    continue
          pscore=pscore+6*iabs(prank-rank)
          pscore=pscore+10*iabs(pfile-file)
20    continue
      return
      end


