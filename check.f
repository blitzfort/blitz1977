      logical function check(sign)
c
c     ******************************************************************
c     *                                                                *
c     *      check is used t8 determine if the king is in check.  it   *
c     *  calls 'attack' after locating the king to determine if the    *
c     *  square that the king is on is under attack by any enemy       *
c     *  pieces.  if so, the king is in check.                         *
c     *                                                                *
c     ******************************************************************
c
      implicit integer (a-z)
      logical attack
      common /board/ board(120)
      common /movecm/ side, player, square, mpiece
      common /kloc/ kingl(3,2)
c
c------------------------------< first, locate the king.
c
      kingsq=kingl(1,player)
      if(board(kingsq)*sign .eq. 6) go to 300
          do 100 kingsq=22,99
              if(board(kingsq)*sign .eq. 6) go to 200
100       continue
200       continue
300   continue
c
c------------------------------< determine if the square that the
c------------------------------< king is on is under attack by enemy
c------------------------------< pieces. if so, the king is in check.
c
      check=attack(-sign,kingsq)
      return
      end


