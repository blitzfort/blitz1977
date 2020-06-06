      subroutine query
c
c     ******************************************************************
c     *                                                                *
c     *      query is used to ask the operator about the amount of     *
c     *  time that has elapsed on the program's chess clock.  the      *
c     *  elapsed time will be asked for every 10 moves until the       *
c     *  program is within 10 moves of the time control that has been  *
c     *  set.  then the program will ask every 3 moves to make sure    *
c     *  that the time control doesn't slip up on it due to some       *
c     *  type of timing error.  for example, in a 40/2-10/30min        *
c     *  time control, the program would ask after move 10,20,30,33,   *
c     *  36,39,43,46,49, etc.                                          *
c     *                                                                *
c     ******************************************************************
c
      implicit integer (a-z)
      logical cputim, cquery, eboard
      common /movcnt/ npmovs, nomovs
      common /chrset/ alpha(46)
      equivalence (c,alpha(3)),(d,alpha(4)),(p,alpha(16))
      common /timecm/ gmoves, gelap, smoves, selap, surpls, cputim,
     *                 cquery, pelap, oelap, psec1, psec2, osec1,
     *                 osec2, avgtim, expect, fsec1
      common /buffer/ text(80)
      common /eboard/ eboard, lbrack, rbrack
c
c------------------------------< determine if the program should ask
c------------------------------< the operator about the time that
c------------------------------< has been used.
c
      if(.not. cquery) go to 999
c
c------------------------------< if more than 10 moves remain in the
c------------------------------< timing interval, only ask every
c------------------------------< 10 moves.
c
      moves=gmoves-npmovs
      if(moves .le. 10) go to 100
      if(mod(npmovs,10) .eq. 0) go to 200
      go to 999
c
c------------------------------< if fewer than 10 moves remain, ask on
c------------------------------< each 3rd move.  not counting each
c------------------------------< 10th move(33,36,39,43,46,49,etc)
c
100   continue
      moves=10-moves
      if(moves.eq.10 .or. moves.eq.0) go to 999
      if(mod(moves,3) .ne. 0) go to 999
200   continue
      text(2)=d
      call ccmnd
      print 300
300   format(1x,'how much time have i used?')
      if(.not. eboard) go to 400
          print 310,lbrack,rbrack
310       format(a1,'t4',a1)
400   continue
      call read
      col=1
      utime=scanin(col)
      if(utime .eq. 0) return
      if(.not. eboard) go to 500
          utime=(utime/10000*60+mod(utime,10000)/100)*60+mod(utime,100)
          pelap=gelap-utime
          go to 999
500   continue
          pelap=utime
999   continue
      return
      end


