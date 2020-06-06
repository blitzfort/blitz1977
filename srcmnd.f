      subroutine srcmnd
c
c     ******************************************************************
c     *                                                                *
c     *      this subroutine is used to set the performance rating for *
c     *  the opponent.  this is used in determining when to accept or  *
c     *  reject a draw or stalement.  a 'contempt' factor is computed  *
c     *  based on how much better or worse the program is than the     *
c     *  opponent.  this factor is used in the search whenever a draw  *
c     *  or stalemate is found to determine if it should be accepted   *
c     *  or avoided.                                                   *
c     *                                                                *
c     ******************************************************************
c
      implicit integer (a-z)
      common /ratecm/ prate, orate
      common /drawcm/ drawsc
      common /buffer/ text(80)
      common /chrset/ alpha(46)
      equivalence (d,alpha(4))
c
c------------------------------< check to see if the ratings should
c------------------------------< be displayed only.
c
      if(text(3).eq.d) go to 20
c
c------------------------------< get the opponent's rating
c
      temp=4
      rtemp=scanin(temp)
      if(rtemp.eq.0) go to 10
          orate=rtemp
          drawsc=(orate-prate)*5
10    continue
      return
c
c------------------------------< output both ratings
c
20    continue
      print 30,prate,orate
30    format(1x,'blitz:',i5,3x,'opponent: ',i5)
      return
      end


