c
c     ******************************************************************
c     *                                                                *
c     *                                                                *
c     *       bbbbbbb    ll         iiii   tttttttt   zzzzzzzz         *
c     *       bb    bb   ll          ii       tt           zz          *
c     *       bb    bb   ll          ii       tt          zz           *
c     *       bbbbbbb    ll          ii       tt         zz            *
c     *       bb    bb   ll          ii       tt        zz             *
c     *       bb    bb   ll          ii       tt       zz              *
c     *       bbbbbbb    llllllll   iiii      tt      zzzzzzzz         *
c     *                                                                *
c     *                                                                *
c     ******************************************************************
c     *                                                                *
c     *       copyright 1977  Robert Hyatt                             *
c     *                                                                *
c     *       all rights reserved,  no part of this program may        *
c     *       be reproduced, transmitted, or stored in any form        *
c     *       or by any means, without the prior written consent       *
c     *       of the author.                                           *
c     *                                                                *
c     ******************************************************************
c     *                                                                *
c     *          Robert M Hyatt                                        *
c     *                                                                *
c     ******************************************************************
c     *                                                                *
c     *      blitz version 6 is the sixth major revision of the chess  *
c     *  program 'blitz' written at the university of southern         *
c     *  mississippi.  it uses shannon's type 'b' minimax search       *
c     *  strategy with alpha/reta backward pruning.                    *
c     *      blitz uses the highly controversial 'full width' search   *
c     *  technique rather than trying to selectively include good      *
c     *  moves with some type of plausibility move ordering.  this     *
c     *  has been hotly discussed and i felt that some experience      *
c     *  with it wculd be valuable for future research with computer   *
c     *  chess.  for time control, it uses another widely debated      *
c     *  technique, that of the 'iterated search'.  with this method,  *
c     *  a full width 1 ply search is done, then a full width 2 ply    *
c     *  search is done, and so forth until no further time can be     *
c     *  spent wtthout the search taking an astronomical amount of     *
c     *  time.                                                         *
c     *      the only search control blitz has is to set the chess     *
c     *  clock to force the program to make so many moves per period.  *
c     *  it will average whatever is necessary in order to meet the    *
c     *  time control.                                                 *
c     *                                                                *
c     *      main is the driver of the program.  it inputs moves/      *
c     *  commands, calls the appropriate routines and outputs the      *
c     *  program's chosen move whenever necessary.  it also performs   *
c     *  timing functions for both the player and program notifying    *
c     *  the operator of the amountt of time used in move selection.   *
c     *                                                                *
c     ******************************************************************
c
      implicit integer (a-z)
      real relap
c     define file 1(600,32,u,iav)
      logical cputim, cquery, beep
      logical tmode, smode, pondr,  foundm, matchd
      logical over, check, autos
      logical easy
      logical repchk, eboard, englsh, temp
      common /tree/ moves(2000), first(30), last(30), which(30),
     *              in chk(30), giv chk(30)
      common /srchcm/ value(30), from(30), to(30), type(30), cappc(30)
      common /killmv/ killmv(20,30)
      common /buffer/ text(80)
      common /board/ board(120)
      common /depth/ sdepth, depth, ply
      common /tflag/ tflag
      common /trcecm/ strace(32)
      common /info/ from$, to$, type$, propc$, cappc$
      common /move cm/ side, player, square, mpiece
      common /timecm/ gmoves, gelap, smoves, selap, surpls, cputim,
     *                 cquery, pelap, oelap, psec1, psec2, osec1,
     *                 osec2, avgtim, expect, fsec1
      common /mode/ tmode, smode, pondr,  foundm, matcmd
ccccc 'pondr' doesn't match other modules which has pndrng.
      common /namecm/ name(5)
      common /colr cm/ color
      common /chrset/ alpha(46)
      equivalence (blank,alpha(44))
      equivalence (g,alpha(7)),(o,alpha(15)),(y,alpha(25))
      common /mov cnt/ npmovs, nomovs
      common /prev mv/ prevmv(6)
      common /over/ over
      common /easy/ easy, easyv
      common /predcm/ ptext(30), pmove, ptype$, pfrom$, pto$
      common /l move/ lmovep, lmoveo
      common /dup/ bdsave(1040), point
      common /rpt cm/ rmoves(2)
      common /mscore/ sscore, mscore, pscore, tscore
      common /piec cm/ pieces(6)
      common /eval/ eval, peval
      common /autos/ autos
      common /beep/ beep
      common /eboard/ eboard, lbrack, rbrack
      common /typ ntn/ englsh
      common /return/ return
c
c------------------------------< determine color desired and
c------------------------------< initialize data area
c
      call setup
      print 1
1     format(1x,'blitz here'/1x,'what is your name?')
      read 2,name
2     format(5a4)
      call setio
      if(return .ne. 0) go to 5
          print 3
3         format(1x,'should i play white?')
          call read
          if(text(1) .eq. y) color=1
          call set gb(.true.)
5     continue
      call locate
      call break
c
c*************************************<
c*************************************<  m a i n   l o o p
c*************************************<
c
100   continue
          osec1=time()
          psec1=osec1
          call query
          call save gm
          if(over) go to 110
              osec2=osec1
              if(.not.tmode .or. eboard) go to 110
                  call forced
                  if(return .ne. 0) go to 300
110       continue
          go to 200
c
c------------------------------< input command or move
c
199       continue
          if(eboard) print 198,lbrack,rbrack
198       format(a1,'e',a1,3('g         '))
200       continue
              call save gm
              print 210,name
210           format(1x,'your move, ',5a4)
              if(over) go to 220
                  call ponder
                  if(return .eq. 1) go to 240
                  if(return .eq. 2) go to 300
220           continue
              call read
              if(text(1) .eq. blank) go to 220
240           continue
          osec2=time()
          msec1=osec2
          call option
          if(return .ne. 0) go to 200
          if(text(1).eq.g .and. text(2).eq.o) go to 400
          if(over) go to 200
          ply=2
          temp=englsh
          if(eboard) englsh=.false.
          call input(.true.)
          englsh=temp
          if(return .ne. 0) go to 199
          if(eboard) call output(0,board,.true.)
c
c------------------------------< determine how much time was used
c------------------------------< by the opponent to chose his move
c------------------------------< and adjust his chess clock to
c------------------------------< reflect it.
c
300       continue
          elapo=(osec2-osec1)/100
          nomovs=nomovs+1
          nmoves=npmovs+nomovs
c         write(1,nmoves)(text(l),l=1,30),elapo
          oelap=oelap+elapo
          relap=elapo/60
          relap=relap+float(mod(elapo,60))/100
          if(.not. tmode) print 310, relap
310       format(/1x,'elapsed time was ',f7.2,t23,':'/)
          call setclk(2)
c
c------------------------------< remember his move for en passant
c------------------------------< capture analysis and then make
c------------------------------< it on the game board.
c
          prevmv(4)=from$
          prevmv(5)=to$
          prevmv(6)=type$
          ply=2
          player=2
          side=-1
          call pmover
          lmoveo=from$+to$*256+type$*65536+propc$*1048576
     *           +cappc$*16777216

c
c------------------------------< remember the current position and also
c------------------------------< determine if a draw by repetitioin has
c------------------------------< occurred.
c
          point=point+1
          if(point .gt. 130) point=1
          ply=0
          call draw
          if(return .ne. 0) go to 560
          if(.not. repchk(count)) go to 330
              if(count .ge. 3) go to 520
330       continue
c
c------------------------------< now call 'driver' to select the
c------------------------------< program's next move....from the
c------------------------------< book database or via the minimax
c------------------------------< tree search.
c
400       continue
          if(tmode) go to 405
              do 404 i=1,28
                  strace(i)=strace(i+2)
404           continue
              stemp=strace(31)/1000*1000
              strace(31)=stemp+max0(mod(strace(31),1000)-2,0)
              strace(32)=max0(strace(32)-2,0)
405       continue
          call match
          if(return .ne. 0) go to 410
          psec1=time()
          call save gb
          call driver
410       continue
          if(value(1) .eq. -999999) go to 500
c
c------------------------------< determine how much time was used
c------------------------------< by blitz to chose it's move and
c------------------------------< adjust it's chess clock to reflect
c------------------------------< it,
c
          psec2=time()
          elapp=(psec2-psec1)/100
c
c------------------------------< encode a message to the electronic
c------------------------------< board to indicate that the program
c------------------------------< has made a move.
c
          if(mod(strace(31),1000) .eq. 0) go to 580
          ply=1
          player=1
          side=1
          from$=strace(1)
          call extrct
          if(.not. eboard) go to 465
              temp=englsh
              englsh=.false.
              call output(0,board,.false.)
              englsh=temp
              lim1=2
              lim2=6
              if(text(1) .ne. o) go to 464
                  lim1=1
                  lim2=3
                  if(text(5) .eq. o) lim2=5
464           continue
              print 466,lbrack,(text(i),i=lim1,lim2),rbrack
466           format(10a1)
465       continue
c
c------------------------------< store predicted move, trace, and other
c------------------------------< useful info for statistical purposes.
c------------------------------< then adjust them so that they will be
c------------------------------< useful in the next search (or possibly
c------------------------------< while thinking on the opponent's time)
c
          sdepth=min0(strace(31)-2,mod(strace(32),1000)-2)
          sdepth=max0(sdepth,0)
          foundm=.false.
          tflag=0
c
c------------------------------< shift the killer array up  since the
c------------------------------< search will continue two plies deeper
c------------------------------< for the next pro3ram move.
c
          do 450 j=1,28
              do 440 i=1,20
                  killmv(i,j)=killmv(i,j+2)
440           continue
450       continue
c
c------------------------------< announce mate if one found
c
          if(autos) call inform(.false.,.true.)
          ply=1
          player=1
          side=1
          from$=strace(1)
          call extrct
          if(value(1).lt.900000) go to 460
          n=(1000000-value(1))/2
          if(n.eq.1) go to 460
              if(beep) print 472
              print 451,n
451           format(////'    blitz will mate in',i2,' moves.'///)
460       continue
c
c------------------------------< output the move returned by
c------------------------------< 'driver'.
c
          side=1
          player=1
          ply=1
          mtype=0
          call mover
          if(check(-1)) mtype=1
          call umover
          if(value(1) .eq. 999998) mtype=2
          call output(mtype,board,.true.)
          if(beep) print 472
          print 470,(text(i),i=1,30)
          if(easy) print 471
          if(value(1) .gt. 900000) go to 475
              if(eval-peval .gt. pieces(1)*1.75) print 473
474           continue
              peval=eval
475       continue
470       format(//1x,'my move is ',30a1//)
471       format(1x,'that was easy !'//)
472       format(1x,'g')
473       format(1x,'be careful !'//)
c
c------------------------------< remember the program's move for
c------------------------------< en passant capture analysis and
c------------------------------< then make it on the game board.
c
          prevmv(1)=from$
          prevmv(2)=to$
          prevmv(3)=type$
          call pmover
          lmovep=strace(1)
c
c------------------------------< remember the current position and also
c------------------------------< determine if a draw by repetition has
c------------------------------< occured.
c
          npmovs=npmovs+1
          nmoves=npmovs+nomovs
c         write(1,nmoves)(text(l),l=1,30),elapp
          pelap=pelap+elapp
          relap=elapp/60
          relap=relap+float(mod(elapp,60))/100
          if(.not. tmode) print 480, relap
480       format(/1x,'computation time was ',f7.2,t27,':'/)
          call setclk(1)
          from$=strace(2)
          if(mod(strace(31),1000) .lt. 2) from$=0
          call extrct
          ptype$=type$
          pfrom$=from$
          pto$=to$
          pmove=strace(2)
          if(mod(strace(31),1000) .lt. 2) pmove=0
          if(value(1) .eq. 999998) go to 590
          point=point+1
          if(point .gt. 130) point=1
          ply=0
          call draw
          if(return .ne. 0) go to 560
          if(.not. repchk(count)) go to 490
              if(count .ge. 3) go to 520
490       continue
          if(.not. tmode) go to 100
              do 430 i=1,28
                  strace(i)=strace(i+2)
430           continue
              stemp=strace(31)/1000*1000
              strace(31)=stemp+max0(mod(strace(31),1000)-2,0)
              strace(32)=max0(0,strace(32)-2)
      go to 100
c
c------------------------------< game is over, set the 'over'
c------------------------------< indicator to inhibit any more
c------------------------------< moves and return to the command
c------------------------------< input loop for cleanup and exit.
c
500   continue
      print 510,name
510   format(1x,'checkmate ',5a4)
      go to 590
520   continue
      move=(npmovs+nomovs+1)/2
      do 540 i=1,5
          do 530 j=1,2
              if(rmoves(j) .lt. move-50) rmoves(j)=rmoves(j)+50
530       continue
540   continue
      print 550,name,rmoves
550   format(/1x,'i claim a draw by repetition, ',5a4/
     *1x,'this position repeats the position at move',i3,' and',i3/)
      go to 590
560   continue
      print 570
570   format(/1x,'game is a draw due to insufficient material'/
     *1x,'by either side to force checkmate'/)
      goto 590
580   continue
      print 581, name
581   format(1x,'game is a stalemate, ',5a4)
590   continue
      over=.true.
      go to 100
      end


