      subroutine test
c
c     ******************************************************************
c     *                                                                *
c     *      test is used to determine if a move is ambiguous when     *
c     *  attempting to simplify moves for output.  it remeves all      *
c     *  blanks and uses 'input' to analyze the move.                  *
c     *                                                                *
c     ******************************************************************
c
      implicit integer (a-z)
      common /buffer/ text(80)
      common /chrset/ alpha(46)
      common /redcm/ saver(30)
      equivalence (blank,alpha(44))
      common /return/ return
c
c------------------------------< remove all blanks
c
      do 300 count=1,3
          do 200 i=1,29
              if(text(i).ne.blank) go to 200
              do 100 j=i,29
                  text(j)=text(j+1)
100           continue
200       continue
300   continue
c
c------------------------------< now try to decode the move to see if
c------------------------------< it is ambiguous, return or return 1
c
      call input(.false.)
      if(return .ne. 0) go to 9998
      return=1
      return
9998  continue
      return=0
      return
      end



