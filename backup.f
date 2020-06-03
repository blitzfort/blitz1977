      subroutine backup
c
c     ******************************************************************
c     *                                                                *
c     *      backup is called to backup the best score/variation       *
c     *  that has been found below this node.  when backing up to      *
c     *  ply one, the current move at ply one is moved up to the       *
c     *  top of the move list and others are moved down one position   *
c     *  so that the next search iteration will have a more accurate   *
c     *  move list order to help increase the number of alpha/beta     *
c     *  cutoffs that occur.                                           *
c     *                                                                *
c     ******************************************************************
c
      implicit integer (a-z)
      logical easy
      common /tree/ moves(2000), first(30), last(30), which(30),
     *              in chk(30), giv chk(30)
      common /srchcm/ value(30), from(30), to(30), type(30), cappc(30)
      common /depth/ sdepth, depth, ply
      common /trace/ trace(32,30)
      common /easy/ easy, easyv
      common /window/ window(2)
c
c------------------------------< backup the best score found by
c------------------------------< examining the tree below this node.
c
      value(ply-1)=value(ply)
c
c------------------------------< now, backup the principle variation
c------------------------------< that leads to the backed up score.
c
      do 100 level=1,32
          trace(level,ply-1)=trace(level,ply)
100   continue
c
c------------------------------< if backing up a new best move to
c------------------------------< ply one, pop the current move to
c------------------------------< the top of the ply one move list
c------------------------------< to be considered first in the
c------------------------------< next iteration.
c
      if(ply .ne. 2) go  to 9999
      if(value(1) .eq. window(2)) go to 9999
      where=which(1)
      if(where .eq. 1) go to 9999
      easy=.false.
      temp=moves(where)
300   continue
          if(where .le. first(1)) go to 400
          moves(where)=moves(where-1)
          where=where-1
      go to 300
400   continue
      moves(where)=temp
9999  continue
      return
      end


