      subroutine output(mtype,board,packer)
c
c     ******************************************************************
c     *                                                                *
c     *      output is used to convert the internal move format to     *
c     *  a text string so that it can be printed,  packer is a flag    *
c     *  used to indicate whether (true) or not (false) to reduce the  *
c     *  text string to it's simple8t form,  this reduction cannot     *
c     *  be done while the search is active since it causes move       *
c     *  generations and alters the move list.  in these cases, packer *
c     *  will always be false to avoid trouble.                        *
c     *                                                                *
c     ******************************************************************
c
      implicit integer (a-z)
      integer file(8), pieces(6)
      integer rank(8)
      logical englsh
      logical packer
      integer board(120)
      common /info/ from$, to$, type$, propc$, cappc$
      common /depth/ sdepth, depth, ply
      common /colr cm/ color
      common /types/ normal, castkg, castqn, enpass, promot
      common /chrset/ alpha(46)
      equivalence (rank(1),alpha(28)),(alphae,alpha(5)),
     *(alphap,alpha(16)),(alphax,alpha(24)),(alphao,alpha(15)),
     *(plus,alpha(37)),(minus,alpha(38)),(slash,alpha(39)),
     *(equal,alpha(41)),(blank,alpha(44)),(colon,alpha(45)),
     *(dash,alpha(38))
      common /buffer/ text(80)
      common /typ ntn/ englsh
      data file /'r','n','b',' ',' ','b','n','r'/
      data pieces /'p','n','b','r','q','k'/
c
c------------------------------< initialze.
c
      do 6666 i=1,80
          text(i)=blank
6666  continue
c
c------------------------------< determine which side is moving
c
      player=mod(ply,2)
c
c------------------------------< get and convert piece being moved.
c
      piece=iabs(board(from$))
c
c------------------------------< process origin file/square
c------------------------------< and destination file/square
c
      go to (6,15,19,6,6),type$
c
c------------------------------< check to see if the move should be
c------------------------------< displayed in algebraic or english
c------------------------------< descriptive notation.
c
6     continue
      if(.not. englsh) go to 7000
      ofileq=pieces(6)
      dfileq=pieces(6)
      ofile=mod(from$-1,10)
      dfile=mod(to$-1,10)
      if(color .ne. 1) go to 8
          if(ofile .gt. 4) ofileq=pieces(5)
          if(dfile .gt. 4) dfileq=pieces(5)
          go to 9
8     continue
      if(ofile .lt. 5) ofileq=pieces(5)
      if(dfile .lt. 5) dfileq=pieces(5)
9     continue
      orank=(from$-1)/10-1
      drank=(to$-1)/10-1
      if(type$ .eq. enpass)  drank=drank-1
      if(player .ne. 0) go to 775
          orank=9-orank
          drank=9-drank
          if(type$ .eq. enpass) drank=drank-2
775   continue
c
c------------------------------< now build text string to output
c------------------------------< based on information from above.
c
      text(1)=pieces(piece)
      text(2)=slash
      text(3)=ofileq
      ptr=4
      text(ptr)=file(ofile)
      if(text(ptr) .ne. file(4)) ptr=ptr+1
      text(ptr)=rank(orank)
      ptr=ptr+1
      text(ptr)=minus
      ptr=ptr+1
      if(cappc$ .eq. 0) go to 10
          text(ptr-1)=alphax
          text(ptr)=pieces(cappc$)
          text(ptr+1)=slash
          ptr=ptr+2
10    continue
      text(ptr)=dfileq
      ptr=ptr+1
      text(ptr)=file(dfile)
      if(text(ptr) .ne. file(4)) ptr=ptr+1
      text(ptr)=rank(drank)
      ptr=ptr+1
      if(type$ .ne. promot) go to 11
          text(1)=pieces(1)
          text(ptr)=equal
          text(ptr+1)=pieces(propc$)
          ptr=ptr+2
11    continue
c
c------------------------------< add 'ep' for en passant captures
c
      if(type$ .ne. enpass) go to 12
          text(ptr)=alphae
          text(ptr+1)=alphap
          ptr=ptr+2
12    continue
c
c------------------------------< add '+' if the move is a check
c
      if(mtype .ne. 1) go to 13
          text(ptr)=plus
13    continue
c
c------------------------------< now reduce the move text to the
c------------------------------< minimum required for non-ambiguity.
c
      if(packer) call reduce
      do 121 ptr=1,30
          if(text(ptr) .eq. blank) go to 122
121   continue
122   continue
c
c------------------------------< add '++' if the move gives
c------------------------------< checkmate.
c
      if(mtype .ne. 2) go to 14
          text(ptr)=plus
          text(ptr+1)=plus
14    continue
      return
c
c------------------------------< process castling moves
c
19    continue
      text(4)=minus
      text(5)=alphao
15    continue
      text(1)=alphao
      text(2)=minus
      text(3)=alphao
      go to 14
c
c------------------------------< output the move in algebraic notation
c
7000  continue
      fr=9-from$/10+1
      ff=mod(from$,10)-1
      tr=9-to$/10+1
      tf=mod(to$,10)-1
      if(color .ne. 1) go to 7100
          fr=9-fr
          ff=9-ff
          tr=9-tr
          tf=9-tf
7100  continue
      text(1)=pieces(piece)
      text(2)=alpha(ff)
      text(3)=rank(fr)
      text(4)=dash
      if(cappc$ .ne. 0) text(4)=colon
      text(5)=alpha(tf)
      text(6)=rank(tr)
      ptr=7
      go to 12
      end

