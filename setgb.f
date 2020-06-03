      subroutine set gb(init)
c
c     ******************************************************************
c     *                                                                *
c     *      setgb is used to initialize the playing version of the    *
c     *  game board as well as castling status.                        *
c     *                                                                *
c     ******************************************************************
c
      implicit integer (a-z)
      logical pmoved, moved, init
      integer piece(8)
      common /board/ board(120)
      common /colr cm/ color
      common /castcm/ pmoved(3,2), moved(3,30)
      common /pfiles/ pfiles(10)
      data piece/4,2,3,5,6,3,2,4/
c
c------------------------------< clear the board
c
      do 11 i=4,7
          do 10 j=2,9
              board(i*10+j)=0
10        continue
11    continue
c
c------------------------------< set pieces and pawns
c
      do 20 i=2,9
          board(20+i)=piece(i-1)
          board(30+i)=1
          board(80+i)=-1
          board(90+i)=-piece(i-1)
20    continue
c
c------------------------------< if program is black, the king adn
c------------------------------< queen must be reversed.
c
      if(color .ne. 1) go to 30
          board(25)=6
          board(26)=5
          board(95)=-6
          board(96)=-5
30    continue
c
c------------------------------< initialize castling status
c
      do 50 i=1,2
          do 40 j=1,3
              pmoved(j,i)=.false.
40        continue
50    continue
c
c------------------------------< set the pawn advance scores to
c------------------------------< encourage king-pawn advances more
c------------------------------< than queen-pawn advances.
c
      if(.not. init) go to 70
      if(color .ne. 1) go to 70
      do 60 i=2,5
          temp=pfiles(i)
          pfiles(i)=pfiles(11-i)
          pfiles(11-i)=temp
60    continue
70    continue
      call locate
      return
      end


