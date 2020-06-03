      logical function attack(side,square)
c
c     ******************************************************************
c     *                                                                *
c     *      attack is used to determine the least valuable piece      *
c     *  of the moving side that is attacking 'sqjare'.  it generates  *
c     *  piece moves from 'square' for each type of piece.  if it      *
c     *  encounters the piece it is generating moves for, then that    *
c     *  piece bearing on 'square'.  enpassant pawn captures are       *
c     *  not considered, also, pieces pinned on the king will still    *
c     *  be considered as legality checks would be too time con-       *
c     *  suming.                                                       *
c     *                                                                *
c     ******************************************************************
c
      implicit integer (a-z)
      common /board/ board(120)
      common /movdir/ movdir(28), piecem(28), begin(6), end(6)
c
c------------------------------< initialize.
c
      attack=.false.
c
c------------------------------< determine if the opponent's pawns
c------------------------------< are attacking 'square'.
c
      if(board(square-side*9) .eq. 1*side) go to 400
      if(board(square-side*11) .eq. 1*side) go to 400
c
c------------------------------< determine if the opponent's bishop,
c------------------------------< rook, or queen is attacking 'square'.
c
      do 200 i=1,8
          loc=square
100       continue
              loc=loc+movdir(i)
              temp=side*board(loc)
          if(temp .eq. 0) go to 100
          if(temp .eq. piecem(i)) go to 400
          if(temp .eq. 5) go to 400
200   continue
c
c------------------------------< determine if the opponent's knight
c------------------------------< or kinq is attacking 'square',
c
      do 300 i=9,24
          if(board(square+movdir(i))*side .eq. piecem(i)) go to 400
300   continue
      return
c
c------------------------------< 'square' is under attack, return
c------------------------------< a 'true' indication.
c
400   continue
      attack=.true.
      return
      end


