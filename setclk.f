      subroutine setclk(whom)
c
c     ******************************************************************
c     *                                                                *
c     *      setclk is used to update the chess clock whenever a time  *
c     *  control is reached.  it also checks to determine if either    *
c     *  blitz or the opponent has missed the time control and lost    *
c     *  on time.  if so, the appropriate message is printed.          *
c     *                                                                *
c     ******************************************************************
c
      implicit integer (a-z)
      logical cputim, cquery, eboard
      logical tmode, smode, pndrng, foundm, matchd
      common /timecm/ gmoves, gelap, smoves, selap, surpls, cputim,
     *                 cquery, pelap, oelap, psec1, psec2, osec1,
     *                 osec2, avgtim, expect, fsec1
      common /movcnt/ npmovs, nomovs
      common /chrset/ alpha(46)
      equivalence (alphad,alpha(4))
      common /buffer/ text(80)
      common /mode/ tmode, smode, pndrng, foundm, matchd
      common /eboard/ eboard, lbrack, rbrack
      data cntrls / 0 /
c
c------------------------------< if not at the preset time limit,
c------------------------------< return without resetting the clocks.
c
      if(.not. eboard) go to 100
          h=selap/3600
          m=selap/60-h*60
          if(npmovs .ne. gmoves) go to 20
              if(whom .ne. 1) go to 20
              print 10, lbrack, h, m, rbrack
10            format(a1,'t2:',2i2,a1)
20        continue
          if(nomovs .ne. gmoves) go to 40
              if(whom .ne. 2) go to 40
              print 30, lbrack, h, m, rbrack
30            format(a1,'t3:',2i2,a1)
40        continue
100   continue
      if(npmovs+nomovs .ne. 2*gmoves) go to 200
          gmoves=gmoves+smoves
          gelap=gelap+selap
          cntrls=cntrls+1
          print 110, cntrls
110       format(/1x,'time control',i3,' reached'/)
          text(2)=alphad
          call ccmnd
200   continue
c
c------------------------------< determine if either side has exceeded
c------------------------------< the time control limits. if so, output
c------------------------------< an appropriate message.
c
      if(tmode) return
      if(pelap .gt. gelap) print 201
201   format(1x,'time forfeit...my clock flag has fallen')
      if(oelap .gt. gelap) print 202
202   format(1x,'time forfeit,.,your clock flag has fallen')
      return
      end


