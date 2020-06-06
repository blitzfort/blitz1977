      subroutine setup
c
c     ******************************************************************
c     *                                                                *
c     *      setup is called upon entry to blitz.  it initializes the  *
c     *  table of random numbers used to hash the chess board for      *
c     *  the transposition table.  this routine should be modified to  *
c     *  use the maximum word length of the machine blitz is running   *
c     *  on.                                                           *
c     *      setup also zeros the transposition table so that random   *
c     *  garbage will not appear to be a valid table entry causing     *
c     *  an unnecessary collision.                                     *
c     *                                                                *
c     ******************************************************************
c
      implicit integer (a-z)
      real rannum
      integer rannumi
      equivalence (rannum, rannumi)
      common /hashcm/ random(1300), hboard
      common /htable/ hsize, htable(32768)
c
c------------------------------< initialize table of random numbers.
c
      nseed=1234567891
      do 200 i=1,1300
          if((i-1)/100+1 .ne. 7) go to 100
              random(i)=0
              go to 200
100       continue
          nseed=nseed*316227
          rannum=abs(float(nseed)*4656613e-9)
          random(i)=iand(rannumi,65535)*2**16
          nseed=nseed*316227
          rannum=abs(float(nseed)*4656613e-9)
          random(i)=random(i)+iand(rannumi,65535)
200   continue
c
c------------------------------< zero the hash table to eliminate
c------------------------------< unnecessary collisions.
c
      lim=hsize*8
      do 300 i=2,lim,4
          htable(i)=0
300   continue
      return
      end

