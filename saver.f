      subroutine saver
c
c     ******************************************************************
c     *                                                                *
c     *      saver with it's two entry points 'savegb' and 'resgb'     *
c     *  is used to save/restore the game board and related castling   *
c     *  status.  this is used by routines which must modify the       *
c     *  game board or castling status and need to restore it after    *
c     *  finishing whatever processing was required.                   *
c     *                                                                *
c     ******************************************************************
c
      implicit integer (a-z)
      logical pmoved, moved
      logical msave(3,2), gsave(3,2)
      integer gboard(100)
      common /board/ board(120)
      common /savecm/ sboard(100), pmsave(3,2)
      common /castcm/ pmoved(3,2), moved(3,30)
c
c------------------------------< save the game board and castling
c------------------------------< status.
c
      entry save gb
      do 10 i=22,99
         sboard(i)=board(i)
10     continue
      do 1 i=1,2
         do 2 j=1,3
             msave(j,i)=pmoved(j,i)
2         continue
1      continue
      return
      entry save g
      do 100 i=22,99
         gboard(i)=board(i)
100   continue
      do 120 i=1,2
         do 110 j=1,3
             gsave(j,i)=pmoved(j,i)
110       continue
120   continue
      return
c
c------------------------------< restore the game board and
c------------------------------< castling status.
c
      entry rest gb
      do 20 i=22,99
          board(i)=sboard(i)
20    continue
      do 3 i=1,2
          do 4 j=1,3
              pmoved(j,i)=msave(j,i)
4         continue
3     continue
      return
      entry rest g
      do 200 i=22,99
          board(i)=gboard(i)
200   continue
      do 220 i=1,2
          do 210 j=1,3
              pmoved(j,i)=gsave(j,i)
210       continue
220   continue
      return
      end


