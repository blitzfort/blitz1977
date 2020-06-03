      subroutine option
c
c     ******************************************************************
c     *                                                                *
c     *      option is the driver for all of the seperate              *
c     *  command modules.  it decodes the first character of the       *
c     *  input and calls the correct module.  the only direct          *
c     *  input to option is 'help' which requests a listing of all     *
c     *  legal ccmmands.                                               *
c     *                                                                *
c     ******************************************************************
c
      implicit integer (a-z)
      logical cputim, cquery,  autos, beep
      logical englsh, eboard
      logical tmode, smode, pndrng, foundm, matchd
      common/buffer/text(80)
      common /trace/ trace(32,30)
      common /tflag/ tflag
      common /timecm/ gmoves, gelap, smoves, selap, surpls, cputim,
     *                 cquery, pelap, oelap, psec1, psec2, osec1,
     *                 osec2,  avgtim, expect, fsec1
      common /depth/ sdepth, depth, ply
      common /limits/ fdepth,  ftime
      common /move cm/ side, player, sqfrom, mpiece
      common /mov cnt/ npmovs, nomovs
      common /typ ntn/ englsh
      common /mode/ tmode, smode, pndrng, foundm, matchd
      common /chrset/ alpha(46)
      equivalence (a,alpha(1)),(b,alpha(2)),(c,alpha(3)),
     *(d,alpha(4)),(e,alpha(5)),(f,alpha(6)),(g,alpha(7)),
     *(h,alpha(8)),(i, alpha(9)),(j,alpha(10)),(k,alpha(11)),
     *(l,alpha(12)),(m,alpha(13)),(n,alpha(14)),(o,alpha(15)),
     *(p,alpha(16)),(q,alpha(17)),(r,alpha(18)),(s,alpha(19)),
     *(t,alpha(20)),(u,alpha(21)),(v,alpha(22)),(w,alpha(23)),
     *(x,alpha(24)),(y,alpha(25)),(z,alpha(26)),(blank,alpha(44)),
     *(quest,alpha(43))
      common /mscore/ sscore,  mscore, pscore, tscore
      common /namecm/ name(5)
      common /draw cm/ drawsc
      common /eval/ eval,  peval
      common /autos/ autos
      common /beep/ beep
      common /eboard/ eboard,  lbrack, rbrack
      common /return/ return
c
c------------------------------< don't treat algebraic moves as
c------------------------------< commands.  simply return.
c
      return=0
      do 10 ii=1,3
          if(ii.lt.3 .and. text(ii).eq.blank) go to 20
          if(text(ii).eq.alpha(41) .or. text(ii).eq.alpha(39)) go to 20
          if(text(ii).gt.alpha(27) .and. text(ii).le.alpha(36)) return
10    continue
20    continue
c
c------------------------------< now, off to the correct routine.
c
      if(text(1).eq.a .and. text(2).eq.l) go to 300
      if(text(1).eq.b .and. text(2).eq.e) go to 320
      if(text(1).eq.b .and. text(2).eq.m) go to 350
      if(text(1).eq.c) go to 400
      if(text(1).eq.d .and. text(2).eq.r) go to 600
      if(text(1).eq.d) go to 500
      if(text(1).eq.e .and. text(2).eq.b) go to 650
      if(text(1).eq.e .and. text(2).eq.n .and. text(3).eq.g) go to 700
      if(text(1).eq.e .and. text(2).eq.n .and. text(3).eq.d) go to 2000
      if(text(1).eq.f .and. text(2).eq.blank) go to 900
      if(text(1).eq.h .and. text(2).eq.e) go to 100
      if(text(1).eq.h) go to 1000
      if(text(1).eq.q .and. text(2).eq.u .and. text(3).eq.i) go to 2000
      if(text(1).eq.r .and.
     *   (text(2).eq.blank .or. text(2).eq.k)) go to 1100
      if(text(1).eq.s .and. text(2).eq.blank) go to 1200
      if(text(1).eq.s .and. text(2).eq.b) go to 1300
      if(text(1).eq.s .and. text(2).eq.d) go to 1400
      if(text(1).eq.s .and. text(2).eq.m) go to 1500
      if(text(1).eq.s .and. text(2).eq.r) go to 1600
      if(text(1).eq.s .and. text(2).eq.t) go to 1700
      if(text(1).eq.t .and. text(2).eq.m) go to 1800
      if(text(1).eq.t .and. text(2).eq.r) go to 1900
      if(text(1).eq.quest) go to 200
      return=0
      return
c
c------------------------------< help : display commands and explanation
c
100   continue
      print 110
110   format(//
     *1x,'?   : repeat the program''s last move'/
     *1x,'alg : use algebraic notation'/
     *1x,'beep: toggle beep announcing move on/off'/
     *1x,'bm  : set blitz mode (5 min)'/
     *1x,'c?  : control clock'/
     *1x,'d   : display board'/
     *1x,'draw: offer program a draw'/
     *1x,'eb  : activate electronic board'/
     *1x,'end : terminate execution'/
     *1x,'eng : use english descriptive notation'/
     *1x,'f   : force blitz to make a specific move'/
     *1x,'h   : list game move history'/
     *1x,'r   : reset board to a prior position'/
     *1x,'s   : print search statistics'/
     *1x,'sb  : set up a specific board position'/
     *1x,'sd  : set a specific iteration depth'/
     *1x,'sm  : set simultaneous mode'/
     *1x,'sr  : set/display uscf ratings'/
     *1x,'st  : set a specific time per move'/
     *1x,'tm  : toggle tournament mode on/off'/
     *1x,'tr  : trace search'/
     *)
      print 120
120   format(1x,'type return to continue')
      call read
130   print 140
140   format(
     *1x,'moves are entered using standard english descriptive'/
     *1x,'notation.  p-k4, n-kb3, rxb, kpxn, n/1-b3 are some'/
     *1x,'examples of correct input.  castling is indicated by'/
     *1x,'o-o for king-side and o-o-o for queen-side.  checks'/
     *1x,'can be followed by + (not required) for clarity and'/
     *1x,'en passant pawn captures can be followed by ep.  a'/
     *1x,'move can be entered with as little information as'/
     *1x,'desired just so it is not ambiguous.'/
     *1x,'algebraic notation can be used if desired.  if '/
     *1x,'moves are entered in algebraic, the program will'/
     *1x,'use algebraic for output also. normal moves are'/
     *1x,'entered as nb1-c3 (n-qb3 for white). to indicate'/
     *1x,'captures, use a '':'' or ''x'' such as nc3:e4'/)
      go to 9998
c
c------------------------------< ? : repeat last move
c
200   continue
      key=npmovs+nomovs
      read(1'key)(text(i),i=1,30)
      print 210,(text(i),i=1,30)
210   format(1x,'my move was ',30a1)
      go to 9998
c
c------------------------------< alg : use algebraic notation
c
300   continue
      englsh=.false.
      go to 9998
c
c------------------------------< beep : toggle beep on/off
c
320   continue
      beep=.not. beep
      if(beep) print 321
      if(.not. beep) print 322
      go to 9998
321   format(1x,'beep on')
322   format(1x,'beep off')
c
c------------------------------< bm : set blitz mode (5 min)
c
350   continue
      tmode=.not. tmode
      smode=.false.
      autos=text(3).eq.s
      gelap=60*5
      selap=0
      gmoves=60
      smoves=0
      surpls=0
      cquery=.false.
      cputim=.true.
      if(tmode) print 351
351   format(1x,'blitz mode')
      go to 9998
c
c------------------------------< c : control clock
c
400   continue
      call ccmnd
      go to 9998
c
c------------------------------< d: display game board
c
500   continue
      call dcmnd
      go to 9998
c
c------------------------------< draw : offer program a draw
c
600   continue
      if(peval .lt. drawsc) go to 620
      print 610, name
610   format(/1x,'draw refused, ',5a4/)
      go to 9998
620   continue
      print 630, name
630   format(/1x,'draw accepted,',5a4)
      go to 9998
c
c------------------------------< eb : activate electronic board
c
650   continue
      eboard=.not. eboard
      if(eboard) print 651
      if(.not. eboard) print 652
      go to 9998
651   format(1x,'electrcnic board on')
652   format(1x,'electronic board off')
c
c------------------------------< eng : use english descriptive notation
c
700   continue
      englsh=.true.
      go to 9998
c
c------------------------------< f : force blitz to make specific move.
c
900   continue
      call fcmnd
      go to 9998
c
c------------------------------< h : list game history
c
1000  continue
      call hcmnd
      go to 9998
c
c------------------------------< r : reset board to a prior position
c
1100  continue
      call rcmnd
      go to 9998
c
c------------------------------< s : print search statistics
c
1200  continue
      call inform(.true.,.true.)
      go to 9998
c
c------------------------------< sb : set up a specific board position
c
1300  continue
      call sbcmnd
      go to 9998
c
c------------------------------< sd : set a specific iteration depth
c
1400  continue
      if(text(3) .eq. blank) go to 1420
      col=4
      fdepth=scanin(col)
      if(fdepth.ge.0 .and. fdepth.le.30) go to 9998
      print 1410
1410  format(1x,'legal range is 0-30')
      fdepth=0
      go to 9998
1420  continue
      print 1430, fdepth
1430  format(1x,'absolute search depth is',i3)
      go to 9998
c
c------------------------------< sm : toggle simultaneous mode
c
1500  continue
      smode=.not. smode
      tmode=.false.
      autos=text(3).eq.s
      if(.not. smode) go to 1510
      fdepth=30
      print 1551
1551  format(1x,'simultaneous mode')
      go to 9998
1510  continue
      fdepth=0
      go to 9998
c
c------------------------------< sr : set/display ratings
c
1600  continue
      call srcmnd
      go to 9998
c
c------------------------------< st : set a specific time per move
c
1700  continue
      if(text(3) .eq. blank) go to 1720
      col=4
      ftime=scanin(col)
      if(ftime .ge. 0) go to 9998
      print 1710
1710  format(1x,'time limit must be > 0')
      ftime=0
      go to 9998
1720  continue
      print 1730, ftime
1730  format(1x,'absolute time per move is',i4)
      go to 9998
c
c------------------------------< tm : toggle tournament mode
c
1800  continue
      tmode=.not. tmode
      smode=.false.
      autos=text(3).eq.s
      gelap=60*120
      selap=60*30
      gmoves=40
      smoves=10
      surpls=60*10
      cquery=.true.
      cputim=.false.
      if(tmode) print 1851
1851  format(1x,'tournament mode')
      go to 9998
c
c------------------------------< tr : trace move look ahead
c
1900  continue
      temp=4
      tflag=scanin(temp)
      go to 9998
c
c------------------------------< end : terminate play
c
2000  continue
      stop
c
c------------------------------< return
c
9998  continue
      return=1
      return
      end


