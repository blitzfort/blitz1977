      subroutine base
c
c     ******************************************************************
c     *                                                                *
c     *      base is used to set up the ply one move list.  it is a    *
c     *  (hopefully) more accurate ordering of the move list than that *
c     *  done for plies deeper than one.  briefly, 'score' is used to  *
c     *  evaluate the positional/material score for each move and then *
c     *  the status of friendly pieces is added in.  'ripoff' is used  *
c     *  to evaluate the safety of each friendly piece by analyzing    *
c     *  attackers and defenders and returning the expected loss.      *
c     *                                                                *
c     ******************************************************************
c
      implicit integer (a-z)
      logical done, easy, check, in chk, giv chk
      common /board/ board(120)
      common /info/ from$, to$, type$, propc$, cappc$
      common /depth/ sdepth, depth, ply
      common /tree/ moves(2000), first(30), last(30), which(30),
     *              in chk(30), giv chk(30)
      common /srchcm/ value(30), from(30), to(30), type(30), cappc(30)
      common /trcecm/ strace(32)
      common /move cm/ side, player, square, mpiece
      common /mscore/ sscore, mscore, pscore, tscore
      common /piec cm/ pieces(6)
      common /sort cm/ val(100)
      common /minmax/ minmax(2)
      common /easy/ easy, easyv
      common /hmove/ hmove
      common /return/ return
c
c------------------------------< set up and call 'movgen' to generate
c------------------------------< the ply one move list.
c
      first(1)=1
      ply=1
      player=1
      side=1
      depth=ply+1
      in chk(1)=check(1)
      giv chk(1)=.false.
      call movgen
      if(return .ne. 0) go to 700
      end=last(1)
      avg=0
      call lookup
c
c------------------------------< now make each move and evaluate
c------------------------------< the positional score via 'score'.
c
      do 300 where=1,end
          from$=moves(where)
          tscore=-1000000000
          call extrct
          call mover
          if(check(1)) go to 250
              if(moves(where) .ne. strace(1)) go to 150
                  tscore=100000000
                  go to 250
150           continue
              if(moves(where) .ne. hmove) go to 160
                  tscore=100000000
                  go to 250
160           continue
              from(1)=from$
              call score
              avg=avg+pscore
c
c------------------------------< now analyze the state of any friendly
c------------------------------< pieces that may be hanging. if a piece
c------------------------------< is attacked and undefendedd, it may be
c------------------------------< be lost. if it is attacked and defended
c------------------------------< the loss is assumed to be the material
c------------------------------< difference if the attacking pice is
c------------------------------< worth less than the piece it attacks,
c------------------------------< and zero otherwise.
c
              do 200 square=22,99
                  piece=board(square)
                  if(piece  .le. 0) go to 200
                  if(piece  .gt. 5) go to 200
                      tscore=tscore-max0(0,ripoff(square))
200           continue
250       continue
          call umover
          val(where)=tscore
300   continue
c
c------------------------------< now calculate the average of the
c------------------------------< positional scores found so far. add
c------------------------------< +/-600 to it to set the initial
c------------------------------< window of min/max scores.
c
      avg=avg/end
      minmax(1)=avg-600
      minmax(2)=avg+600
c
c------------------------------< now sort the moves based on the
c------------------------------< preliminary scorinq from above,
c
      end=end-1
400   continue
          done=.true.
          do 500 where=1,end
              if(val(where) .ge. val(where+1)) go to 500
              temp=val(where)
              val(where)=val(where+1)
              val(where+1)=temp
              temp=moves(where)
              moves(where)=moves(where+1)
              moves(where+1)=temp
              done=.false.
500       continue
      if(.not. done) go to 400
c
c------------------------------< eliminate moves which 'lost' the
c------------------------------< king and are illegal.
c
      end=end+1
600   continue
          if(val(end) .gt. -10000000) go to 700
          end=end-1
          if(end .ge. 1) go to 600
700   continue
      last(1)=end
c
c------------------------------< if the first move in the list is
c------------------------------< clearly best, set 'easy' to .true.
c------------------------------< to cause the fimal iteration to
c------------------------------< stop early if possible.
c
      from$=moves(1)
      call extrct
      easy=val(1) .gt. val(2)+1500
      easy=easy .and. (cappc$.ne.0 .or. propc$.ne.0)
      easyv=val(2)+1500
      return
      end


