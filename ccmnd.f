      subroutine ccmnd
c
c     ******************************************************************
c     *                                                                *
c     *      ccmnd handles all 'chess clock' commands.  there          *
c     *  are five basic operations that can be performed by this       *
c     *  routine:                                                      *
c     *      1)  the chess clock can be set to some specific           *
c     *          value along with a move counter to force the          *
c     *          program and human to make a certain number of         *
c     *          moves in a specified time period. for example         *
c     *          40 moves in 10 minutes for high-speed chess.          *
c     *                                                                *
c     *      2)  the chess clock can be displayed, telling how         *
c     *          much time each side has on it's clock to see          *
c     *          how each side is doing with respect to time.          *
c     *                                                                *
c     *      3)  the computer's time per move is usually in            *
c     *          computer time which may be much slower than           *
c     *          real or wall clock time.  the computer may be         *
c     *          forced to use elapsed time just as the human          *
c     *          always does if desired.  this may get the prog-       *
c     *          ram into time trouble, but may be required for        *
c     *          tournament play where computer time is not            *
c     *          understood by everyone.                               *
c     *                                                                *
c     *      4)  the program can be instructed to periodically         *
c     *          ask the terminal operator how much time is            *
c     *          left on it's clock since computer timing may          *
c     *          not exactly agree with the chess clock.  see          *
c     *          module 'query' for further details.                   *
c     *                                                                *
c     *      5)  the program can be instructed to complete the time    *
c     *          control with a certain amount of time left over as    *
c     *          a surplus or 'pad'.  this is usually 10 minutes for   *
c     *          tournament mode.                                      *
c     *                                                                *
c     ******************************************************************
c
      implicit integer  (a-z)
      logical  cputim, cquery, eboard
      common /buffer/ text(80)
      common /timecm/  gmoves, gelap, smoves, selap, surpls, cputim,
     *                  cquery, pelap, oelap, psec1, psec2, osec1,
     *                  osec2, avgtim, expect, fsec1
      common /movcnt/ npmovs, nomovs
      common /chrset/ alpha(46)
      equivalence (d,alpha(4)),(l,alpha(12)),(t,alpha(20)),(e,alpha(5)),
     *(c,alpha(3)),(blank,alpha(44)),(quest,alpha(43)),(q,alpha(17)),
     *(o,alpha(15)),(p,alpha(16)),(s,alpha(19)),(zero,alpha(27))
      common /eboard/ eboard, lbrack, rbrack
c
c------------------------------< determine which option was
c------------------------------< requested.
c
      if(text(2) .eq. c) go to 200
      if(text(2) .eq. d) go to 70
      if(text(2) .eq. e) go to 100
      if(text(2) .eq. t) go to 110
      if(text(2) .eq. s) go to 650
      if(text(2) .eq. q) go to 400
c
c------------------------------< c : set chess clock
c
      col=3
      temp=scanin(col)
      if(temp .eq. 0) go to 41
      gmoves=temp
      col=col+1
      gelap=scanin(col)*60
      col=col+1
      smoves=scanin(col)
      col=col+1
      selap=scanin(col)*60
ccccc avg=gelap/moves
ccccc above line is in error.  probably should be...
      avg=gelap/gmoves
41    continue
      gelap1=gelap/60
      selap1=selap/60
      print 42, gmoves,  gelap1, smoves, selap1
42    format(1x,i2,' moves in ',i3,' minutes'/
     *1x,'then ',i2,' moves in ',i3,' minutes.')
      return
c
c------------------------------< ce : set electronic clock
c
100   continue
      if(.not. eboard) return
          h1=max0(0,(gelap-pelap)/3600)
          m1=max0(0,(gelap-pelap)/60-h1*60)
          h2=max0(0,(gelap-oelap)/3600)
          m2=max0(0,(gelap-oelap)/60-h2*60)
          print 421, lbrack, h1, m1, h2, m2, rbrack
421       format(a1,'t1:',2i2,',',2i2,a1)
      return
c
c------------------------------< cc : correct chess clock
c
200   continue
      code=text(3)
      if(code.ne.p .and. code.ne.blank) go to 220
      print 210
210   format(1x,'how much time have i used?')
      call read
      temp=1
      utime=scanin(temp)
      if(utime .eq. 0) go to 220
      pelap=utime*60
220   continue
      if(code.ne.o .and. code.ne.blank) go to  240
      print 230
230   format(1x,'how much timf have you used?')
      call read
      temp=1
      utime=scanin(temp)
      if(utime .eq. 0) go to 240
      oelap=utime*60
240   continue
      return
c
c------------------------------< cd : display chess clock
c
70    continue
      d1=gelap-pelap
      d2=gelap-oelap
      if(d1 .lt. 0) d1=0
      if(d2 .lt. 0) d2=0
      h1=d1/60
      h2=d2/60
      m1=mod(d1,60)
      m2=mod(d2,60)
      pm=gmoves-npmovs
      om=gmoves-nomovs
      print 80, h1, m1, h2, m2, pm, om
80    format(7x,'blitz',12x,'opponent'/
     *1x,i3,' mins ',i2,' secs',2x,i3,' mins ',i2,' secs',
     */1x,'to make ',i2,' moves',2x,'to make ',i2,' moves')
      return
c
c------------------------------< cq : set clock query flag
c
400   continue
      cquery=.not. cquery
      if(cquery) print 410
      if(.not. cquery) print 420
      return
410   format(1x,'i will ask about the chess clock time')
420   format(1x,'i will not ask about the chess clock time')
c
c------------------------------< cs : set surplus time
c
650   continue
      col=4
      temp=scanin(col)
      if(temp.eq.0 .and. text(4).ne.zero) go to 652
      surpls=temp
      if(surpls .lt. 0) go to 660
      return
652   continue
      print 653, surpls
653   format(1x,'time surplus is',i4,' seconds.')
      return
660   continue
      print 661
661   format(1x,'clock surpls cannot be negative!')
      return
c
c------------------------------< ct : set clock type (elapsed/cpu)
c
110   continue
      col=4
      temp=scanin(col)
      if(temp .eq. 0) go to 170
      type=temp
      if(type .eq. 0) go to 170
      if(type.ne.1 .and. type.ne.2) go to 140
      cputim=type  .eq. 1
170   continue
      if(cputim) print 120
      if(.not. cputim) print 130
      return
120   format(1x,'clock is using cpu time')
130   format(1x,'clock is using elapsed time')
140   print 141
141   format(1x,'legal values are 1 for cpu and 2 for elapsed.')
      return
      end


