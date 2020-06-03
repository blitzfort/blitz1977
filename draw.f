      subroutine draw
c
c     ******************************************************************
c     *                                                                *
c     *      draw is used to detect those situations that are a draw   *
c     *  due to the lack of sufficient material by eitmer side to be   *
c     *  able tc force a mate.  generally, if either side has a queen, *
c     *  rook, or pawn then the position is assumed winnable.  also,   *
c     *  if either side has 2 or more bishops, 3 or more knights, or   *
c     *  a knight and bishop, then the position is assumed winnable.   *
c     *                                                                *
c     ******************************************************************
c
      implicit integer (a-z)
      common /board/ board(120)
      common /return/ return
c
c------------------------------< determin if either side has enough
c------------------------------< material to force checkmate.  if not,
c------------------------------< consider the position a draw and
c------------------------------< return.
c
      pbish=0
      obish=0
      pnight=0
      onight=0
      do 30  sq=22,99
          temp=board(sq)+7
          if(temp .eq. 7) go to 30
          if(temp .gt. 13) go to 30
          go to(30,9999,9999,11,12,9999,30,9999,21,22,9999,9999,30),temp
11        continue
              if(onight .gt. 0) go to 9999
              obish=obish+1
              go to 30
12        continue
              if(obish .gt. 0) go to 9999
              onight=onight+1
              go to 30
21        continue
              if(pbish .gt. 0) go to 9999
              pnight=pnight+1
              go to 30
22        continue
              if(pnight .gt. 0) go to 9999
              pbish=pbish+1
30    continue
      if(pbish .ge. 2) go to 9999
      if(obish .ge. 2) go to 9999
      if(pnight .ge. 3) go to 9999
      if(onight .ge. 3) go to 9999
c
c------------------------------< the game is drawn due to lack of
c------------------------------< enough material by either side
c------------------------------< to be able to win.
c
      return=1
      return
c
c------------------------------< one side has a queen, rook or a
c------------------------------< pawn.  the position is assumed
c------------------------------< to be winnable.
c
9999  continue
      return=0
      return
      end


