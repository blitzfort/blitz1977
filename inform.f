      subroutine inform(made,all)
c
c     ******************************************************************
c     *                                                                *
c     *      inform is used to inform the operator about search        *
c     *  statistics.  it is called after the final search iteration    *
c     *  when in tournament mode and can be called at any time by      *
c     *  using the 's' command.                                        *
c     *      it prints the followin6 information;                      *
c     *         a)  evaluation for board position;                     *
c     *         b)  total/chess clock time used;                       *
c     *         c)  iteration depth;                                   *
c     *         d)  number of terminal nodes scored;                   *
c     *         e)  number of non-terminal nodes at each level;        *
c     *         f)  principle variation.                               *
c     *                                                                *
c     ******************************************************************
c
      implicit integer (a-z)
      logical cputim, cquery
      real ratio,elapp, ttime
      integer buf(72)
      logical check, made, all
      common /board/ board(120)
      common /info/ from$, to$, type$, propc$, cappc$
      common /movecm/ side, player, square, mpiece
      common /statcm/ nodes(30), times(30), aborts(30), tnodes, snodes,
     *                 hashes, pright
      common /timecm/ gmoves, gelap, smoves, selap, surpls, cputim,
     *                 cquery, pelap, oelap, psec1, psec2, osec1,
     *                 osec2, avgtim, expect, fsec1
      common /depth/ sdepth, depth, ply
      common /movcnt/ npmovs,nomovs
      common /buffer/ text(80)
      common /srchcm/ value(30), from(30), to(30), type(30), cappc(30)
      common /trcecm/ strace(32)
      common /ratio/ ratio
      common /eval/ eval, peval
      common /htable/ hsize, htable(32768)
      common /chrset/ alpha(46)
      equivalence (aster,alpha(40)),(blank,alpha(44)),
     *            (period,alpha(42))
c
c------------------------------< determine if a search was done.
c------------------------------< if not, there are no statistics.
c
      if(tnodes .eq. 0) go to 9999
      print 51
      pdepth=strace(32)
c
c------------------------------< determine how much time was used
c------------------------------< to select the previous move
c
      elapi=0
      do 67 i=1,30
          elapi=elapi+times(i)
67    continue
      itime=float(elapi)/100
      nmove=npmovs+nomovs
      if(made) nmove=nmove-1
      used=0
      lim=hsize*8+1
      do 66 node=1,lim,4
          mmove=htable(node+1)/65536
          if(mmove .eq. nmove) used=used+1
66    continue
      used=(used*100)/(hsize*2)
      found=hashes*100/tnodes
      move=(npmovs+nomovs-1)/2+1
      p1=pright/1000
      p2=mod(pright,1000)
      if(.not. made) move=(npmovs+nomovs)/2+1
c
c------------------------------< output board evaluation,time info.
c------------------------------< depth and predictions.
c
      if(all) print 1, move, p1, p2
1     format(1x,'move:',i3,2x,'book:',i3,2x,'expected:',i3)
      ttime=itime/60
      ttime=ttime+float(mod(itime,60))/100
      print 4, pdepth, ttime, ratio, eval, tnodes, snodes,
     *         found,  used
4     format(
     *1x,'depth:',i3,2x,'time:',f7.2,2x,'ratio:',f4.1,2x,'eval:',i8,
     *t22,':'/
     *1x,'nodes:',i8,2x,'evals:',i8,2x,'lookups',i4,'x',2x,
     *'table',i4,'x')
      if(.not. all) go to 1000
      if(times(pdepth) .lt. 1000) go to 1000
c
c------------------------------< output time for each iteration
c
      print 10,(times(j)/100,aborts(j),j=1,pdepth)
10    format(1x,'times: ',10(i5,a1)/8x,10(i5,a1)/8x,10(i5,a1))
c
c------------------------------< output nodes examined
c
      do 30 i=2,30
          if(nodes(i) .eq. 0) go to 40
30    continue
40    continue
      i=i-1
      print 50,(nodes(j),j=2,i)
50    format(1x,'nodes:',10i6/7x,10i6/7x,10i6)
      print 51
51    format(1x)
c
c------------------------------< output the principle variation.
c
1000  continue
      nchars=0
      call save g
      from$=strace(1)
      if(from$ .eq. 0) go to 9998
      if(.not. made) go to 801
          ply=1
          player=1
          side=1
          call extrct
          cappc(ply)=-cappc$
          call pumver
801   continue
c
c------------------------------< determine depth of variation
c
      matply=999
      maxd=mod(strace(31),1000)
      if(iabs(eval).lt.900000) go to 44
          matply=999999-iabs(eval)
          maxd=min0(maxd,matply)
44    continue
c
c------------------------------< convert moves to text for output.
c
      side=-1
      do 100 ply=1,maxd
          from$=strace(ply)
          call extrct
          side=-side
          player=2-mod(ply,2)
          etype$=0
          callmover
          if(check(-side)) etype$=1
          call output(etype$,board,.true.)
          if(ply .eq. matply) etype$=2
          call output(etype$,board,.true.)
c
c------------------------------< compress moves into the buffer.
c
          nchars=nchars+1
          j=0
          do 60 i=nchars,72
              j=j+1
              buf(i)=text(j)
              if(text(j).eq.blank .and. text(j+1).eq.blank) go to 70
60        continue
c
c------------------------------< output a line when the buffer is full
c------------------------------< or when the variation is complete
c
70        continue
          nchars=i
          if(nchars.lt.60) go to 90
              print 80,(buf(l),l=1,nchars)
80            format(1x,72a1)
              nchars=0
90        continue
          from(ply)=from$
          to(ply)=to$
          callmover
100   continue
c
c------------------------------< output the buffer if anything is
c------------------------------< left in it.
c
      if(strace(31) .lt. 1000) go to 200
          l1=nchars+1
          l2=nchars+5
          do 633 i=l1,l2
              buf(i)=period
633       continue
          nchars=nchars+5
200   continue
      if(nchars.gt.0) print 80,(buf(l),l=1,nchars)
      print 94
94    format(1x)
9998  continue
      call rest g
9999  continue
      return
      end


