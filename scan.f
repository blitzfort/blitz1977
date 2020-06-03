      integer function scanin(column)
c
c     ******************************************************************
c     *                                                                *
c     *      scanin is used to decode numeric imput in free format     *
c     *  from the terminal/card reader. rather than using a specificc  *
c     *  'i' type format, numeric input is read in alphabetically      *
c     *  and decoded as needed to make input simpler and more error    *
c     *  free.                                                         *
c     *                                                                *
c     ******************************************************************
c
      implicit integer (a-z)
      integer digits(10)
      common /chrset/ alpha(46)
      equivalence (digits(1),alpha(27)),(minus,alpha(38)),
     *(plus,alpha(37)),(blank,alpha(44)),(comma,alpha(46))
      common /buffer/ text(80)
c
c------------------------------< initialize.
c
      scanin=0
      sign=1
c
c------------------------------< process each of 80 columns.
c
      do 600 i=column,80
c
c------------------------------< blank indicates the end of the number.
c
          if(text(i).eq.blank) go to 700
          if(text(i).eq.comma) go to 700
c
c------------------------------< match the digits with 0-9
c
          do 300 j=1,10
              if(text(i).eq.digits(j)) go to 500
300       continue
c
c------------------------------< it may be + or - also.
c
          if(text(i).eq.plus) sign=1
          if(text(i).eq.minus) sign=-1
          if(text(i).eq.plus.or.text(i).eq.minus) go to 600
c
c------------------------------< not 0-9, + or -.  it's illegal input
c
          print 400
400       format(1x,'illegal numeric input - try again')
          go to 700
c
c------------------------------< it's a valid digit. add it in.
c
500       continue
          scanin=scanin*10+j-1
600   continue
c
c------------------------------< include sign.
c
700   continue
      scanin=scanin*sign
      column=i
999   continue
      return
      end


