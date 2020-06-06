      subroutine search
c
c     ******************************************************************
c     *                                                                *
c     *      search is the driver for the basic look ahead procedure   *
c     *  used to select a move.  it is called by 'driver' after the    *
c     *  depth has been set to control the search.  search uses the    *
c     *  variable depth minimax search with alpha/beta backward        *
c     *  pruning as proposed by shannon.  the search is exhaustive     *
c     *  rather than selective with captures and certain types of      *
c     *  checking moves carried out to whatever depth is necessary     *
c     *  to obtain a quiesced position so that the terminal node       *
c     *  state scoring routine will not return bogus scores.           *
c     *      the basic search is iterated; that is, a complete search  *
c     *  is done with the basic depth at one, then a complete search   *
c     *  is done with the basic depth at two, three, ... until the     *
c     *  allotted move time is used or it becomes apparent that an     *
c     *  additional search will use too much time based on previous    *
c     *  search times.  each time a score/move is backed up to ply     *
c     *  one, that move is shuffled to the top of the move list to     *
c     *  consider first for the next iteration.                        *
c     *                                                                *
c     ******************************************************************
c
      implicit integer (a-z)
      logical pmoved, moved
      logical mated
      logical abort
      logical broke
      logical inchk, givchk
      integer tab(30), fmt(12)
      logical check, repchk
      common /board/ board(120)
      common /mscore/ sscore, mscore, pscore, tscore
      common /tree/ moves(2000), first(30), last(30), which(30),
     *              inchk(30), givchk(30)
      common /srchcm/ value(30), from(30), to(30), type(30), cappc(30)
      common /killmv/ killmv(20,30)
      common /phascm/ phase(30), stats(30)
      common /depth/ sdepth, depth, ply
      common /movecm/ side, player, square, mpiece
      common /info/ from$, to$, type$, propc$, cappc$
      common /types/ normal, castkg, castqn, enpass, promot
      common /trace/ trace(32,30)
      common /tflag/ tflag
      common /statcm/ nodes(30), times(30), aborts(30), tnodes, snodes,
     *                 hashes, pright
      common /mated/ mated(30)
      common /window/ window(2)
      common /abort/ abort
      common /drawcm/ drawsc
      common /buffer/ text(80)
      common /broke/ broke
      common /minmax/ minmax(2)
      common /return/ return
      data tab /'3','6','9','12','15','18','21','24','27','30',
     * '33','36','39','42','45','48','51','54','57','60','63','66',
     * '69','72','75','78','81','84','87','9o'/
      data fmt /'(  t',' ',',i2,','1x,','20a1',',2x,',
     * '''pha','se'',','i2,','3x,','i8',')'/
c
c------------------------------< initialize.
c
      ply=0
      side=-1
      do 3002 i=1,30
          nodes(i)=0
3002  continue
      count=0
c
c------------------------------< advance to the next level.
c
1     continue
          if(abort) go to 19
          ply=ply+1
          player=2-mod(ply,2)
          value(ply)=window(player)
          side=-side
          if(ply .ge. 30) go to 92
          if(ply .le. 1) go to 42
              call lookup
              if(return .eq.  1)  go to 92
              if(return .eq.  2)  go to 81
42        continue
c
c------------------------------< zero the killer move counts so that
c------------------------------< killers found will be placed first
c------------------------------< although older ones will be kept.
c
          do 6655 i=11,20
              killmv(i,ply+1)=0
6655      continue
          if(broke .and. side.eq.1) call status
c
c------------------------------< generate allmoves from the current
c------------------------------< board position, set the indicator
c------------------------------< that controls checks in the quiscence
c------------------------------< search if all previous moves were
c------------------------------< also checks. remember if the side
c------------------------------< to move is in check.
c
          if(ply .eq. 1) go to 100
              givchk(ply)=.false.
              inchk(ply)=check(side)
              if(ply .gt. depth) call mater
              call movgen
              if(return .ne. 0) go to 81
              count=100
              call draw
              if(return .ne. 0) go to 17
              count=0
              if(ply .gt. 1) mated(ply-1)=.false.
              nodes(ply)=nodes(ply)+1
              which(ply)=0
100       continue
          first(ply+1)=last(ply)+1
          mated(ply)=ply.le.depth .or. inchk(ply)
c
c------------------------------< determine if terminal node scoring
c------------------------------< should be done. if below the basic
c------------------------------< iteration depth, it is not needed.
c------------------------------< if in the quiescence search, don't
c------------------------------< use very large or small values for
c------------------------------< alpha and beta but use the positional
c------------------------------< score before any move at the current
c------------------------------< level is made. in effect, the side
c------------------------------< to move can try a forcing move or
c------------------------------< 'stand pat' and accept the current
c------------------------------< score as the best that can happen.
c
2000      continue
              if(ply.le.depth .or. inchk(ply)) go to 210
c
c------------------------------< now determine if the material score
c------------------------------< will cause an alpha/beta cutoff without
c------------------------------< calculating the positional score
c
              temp=3-player
              if(temp .gt. ply) go to 500
                  tscore=side*(mscore+minmax(player))
                  do 40 level=temp,ply,2
                      if(tscore .ge. side*value(level)) go to 8
40                continue
500           continue
              call score
c
c------------------------------< now determine if this score is
c------------------------------< good enough for the side to move
c------------------------------< so that the opposing side would
c------------------------------< never make the move leading to
c------------------------------< this position.
c
              if(temp .gt. ply) go to 9910
                  do 9900 level=temp,ply,2
                      if(side*tscore .ge. side*value(level)) go to 8
9900              continue
9910          continue
c
c------------------------------< if this score is worse for the side
c------------------------------< to move than others already found,
c------------------------------< there is no need to remember the
c------------------------------< score or variation unless it is
c------------------------------< backed up later.
c
              if(side*tscore .le. side*value(ply)) go to 210
              value(ply)=tscore
c
c------------------------------< back up the variation leading to
c------------------------------< this terminal score so that if the
c------------------------------< side to move 'stands pat', the
c------------------------------< variation leading to the current
c------------------------------< score will be saved.
c
              do 7 level=1,ply
                  trace(level,ply)=moves(which(level))
7             continue
              trace(31,ply)=ply-1
              trace(32,ply)=depth
210       continue
c
c------------------------------< call 'select' to select the next
c------------------------------< move for consideration. 'select'
c------------------------------< will return 1 when no further moves
c------------------------------< should be considered.
c
          call select
          if(return .eq. 1) go to 9
          if(return .eq. 2) go to 191
          tnodes=tnodes+1
          to(ply)=to$
          from(ply)=from$
          type(ply)=type$
c
c------------------------------< perform trace analysis output if the
c------------------------------< flag has been set.
c
          if(ply .gt. tflag) go to 203
              call output(0,board,.false.)
              fmt(2)=tab(ply)
              write(6,fmt)ply,(text(ix),ix=1,20),phase(ply),value(ply)
203       continue
          callmover
c
c------------------------------< if in check, make sure each move
c------------------------------< is legal to save time later.
c
          if(.not. inchk(ply)) go to 204
              if(.not. check(side)) go to 204
                  call umover
                  go to 2000
204       continue
c
c------------------------------< determine if this position has occurred
c------------------------------< earlier in this or previous search,
c------------------------------< if so, consider it a drawn position
c------------------------------< and evaluate no deeper.
c
          if(inchk(ply) .or. givchk(ply)) go to 205
              if(ply .gt. depth) go to 1
205       continue
          if(.not. repchk(count)) go to 1
              mated(ply)=.false.
              ply=ply+1
              player=2-mod(ply,2)
              side=-side
              go to 17
c
c------------------------------< the move at the current level is
c------------------------------< so good that the move at the previous
c------------------------------< level would never be made. this
c------------------------------< is an alpha/beta cutoff. remember
c------------------------------< this move as a 'killer' if it is
c------------------------------< worthwhile and return to the previous
c------------------------------< level to try another move discarding
c------------------------------< the current move at that level.
c
8     continue
      if(ply .le. depth) call killer(moves(which(ply)))
      call store(moves(which(ply)),1)
81    continue
      ply=ply-1
      player=2-mod(ply,2)
      side=-side
      from$=from(ply)
      to$=to(ply)
      type$=type(ply)
      call umover
      go to 2000
c
c------------------------------< allmoves this level have been
c------------------------------< examined. if no legalmoves were
c------------------------------< found, the side to move has been
c------------------------------< mated or stalemated....report the
c------------------------------< correct score. otherwise, backup
c------------------------------< the best score found along with
c------------------------------< the variation leading to that score.
c
9     continue
      if(mated(ply)) go to 17
91    continue
      if(ply .le. 1) go to 18
      if(ply .le. depth) call killer(trace(ply,ply))
      call store(trace(ply,ply),2)
92    continue
      call backup
      ply=ply-1
      player=2-mod(ply,2)
      side=-side
      from$=from(ply)
      to$=to(ply)
      type$=type(ply)
      call umover
c
c------------------------------< check to see if the alpha/beta
c------------------------------< algorithm will cause a cutoff on
c------------------------------< here. briefly stated, if this move
c------------------------------< is better for the moving side than
c------------------------------< moves already found at previous nodes
c------------------------------< in the tree for the opposite side,
c------------------------------< then this is a 'refutation' type move
c------------------------------< which forces the move at the previous
c------------------------------< level to be discarded with no further
c------------------------------< analysis.
c
111   continue
      temp=3-player
      if(temp .gt. ply) go to 2000
      do 1801 level=temp,ply,2
          if(side*value(ply) .ge. side*value(level)) go to 8
1801  continue
      go to 2000
c
c------------------------------< no legalmoves were generated for
c------------------------------< the current level. it is either
c------------------------------< checkmate or stalemate. set the
c------------------------------< correct score before backing up.
c
17    continue
      tlimit=ply-1
ccccc hand written note - there apears to be a 7 or > at the end of
ccccc above line.  i'm not sure to what / where it refers to.
      do 171 level=1,tlimit
          trace(level,ply)=moves(which(level))
171   continue
      trace(31,ply)=tlimit
      trace(32,ply)=depth
      if(count .gt. 1) go to 172
          if(check(side)) go to 174
172   continue
ccccc hand written note - there is a -1 here, under 'side'
c
c------------------------------< stalemate or draw.
c
      value(ply)=drawsc
      temp=3-player
      if(temp .gt. ply) go to 91
      do 173 level=temp,ply,2
          if(side*value(ply) .ge. side*value(level)) go to 81
173   continue
      go to 91
c
c------------------------------< checkmate.
c
174   continue
      value(ply)=-side*(1000000-ply)
      last(ply-1)=first(ply-1)-1
      go to 91
c
c------------------------------< the minimax search is complete. the
c------------------------------< program's best move is at the top
c------------------------------< of the ply one move list. return
c------------------------------< to 'driver' to initialize for the
c------------------------------< next iteration if necessary.
c
18    continue
      return
c
c------------------------------< search was aborted, back out
c------------------------------< gracefully and return a.s.a.p.
c
19    continue
          call umover
191       continue
          if(ply .le. 1) return
          ply=ply-1
          side=-side
          player=2-mod(ply,2)
          from$=from(ply)
          to$=to(ply)
          type$=type(ply)
      go to 19
      end



