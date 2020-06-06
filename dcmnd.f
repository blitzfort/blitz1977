      subroutine dcmnd
c
c     ******************************************************************
c     *                                                                *
c     *      dcmnd is used to display the game board in response to    *
c     *  'd' or 'db' commands,  parentheses bracket the program's      *
c     *  pieces and pluses bracket the opponent's pieces.              *
c     *                                                                *
c     ******************************************************************
c
      implicit integer (a-z)
      integer bdchar(64)
      integer char(13)
      common /board/ board(120)
      common /tree/ moves(2000), first(30), last(30), which(30),
     *              inchk(30), givchk(30)
      common /colrcm/ color
      common /buffer/ text(80)
      common /chrset/ alpha(46)
      equivalence (blank,alpha(44))
      equivalence (bdchar(1),moves(800))
      data char /':k:',':q:',':r:',':b:',':n:',':p:',' * ',
     *                  '(p)','(n)','(b)','(r)','(q)','(k)' /
      data wsq / ' + ' /
c
c------------------------------< set up the display board
c
      sq=21
      do 200 i=1,8
          do 100 j=1,8
              sq=sq+1
              sub=(i-1)*8+j
              bdchar(sub)=char(board(sq)+7)
100       continue
          sq=sq+2
200   continue
      fudge=0
      do 400 i=1,8
          do 300 j=1,7,2
              if(bdchar((i-1)*8+j+fudge) .eq. char(7))
     *               bdchar((i-1)*8+j+fudge)=wsq
300       continue
          fudge=mod(fudge+1,2)
400   continue
c
c------------------------------< output board
c
      if (color .eq. 1) print 501
      if (color .eq. 2) print 502
501   format(/16x,'white'/)
502   format(/16x,'black'/)
      do 700 i=1,8
          j=(i-1)*8+1
          k=j+7
          print 600,(bdchar(l),l=j,k)
600       format(3x,8(a3,1x)/)
700   continue
      if (color .eq. 2) print 501
      if (color .eq. 1) print 502
800   format(16x,a5/)
      return
      end


