      subroutine input(error)
c
c     ******************************************************************
c     *                                                                *
c     *      input is used to translate all chess moves from text      *
c     *  strings to the internal form used by the program.  it will    *
c     *  accept standard english descriptive notation (ie p-k4) , or   *
c     *  it will accept algebraic notation (nb1-c3).  it will allow    *
c     *  the user to enter the move using the minimum amount of        *
c     *  qualification necessary to eliminate ambiguity.  it does      *
c     *  this by generating all legalmoves and using the input text   *
c     *  to eliminate incorrect moves.  for example, 'p-5' would       *
c     *  eliminate allmoves but pawn moves to the 5th rank.  if       *
c     *  only one move is left, the move is accepted; otherwise, the   *
c     *  'ambiguous' error message is printed to force the operator    *
c     *  to further qualify the move.  only two forms of algebraic     *
c     *  input are accepted: 1) nf3 says move the knight to square     *
c     *  f3 and ng1-f3 says move the knight on square g1 to square f3, *
c     *  to indicate captures, nf3: or ng1:f3 may be used.             *
c     *      the 'error' parameter is a flag which can inhibit error   *
c     *  messages if true.  this is used in reducing moves to the      *
c     *  simplified form for output.                                   *
c     *                                                                *
c     ******************************************************************
c
      implicit integer (a-z)
      integer pieces(6), files(5)
      integer sqfrom(200),sqto(200),fpiece(200),tpiece(200)
      integer digits(9)
      logical check, ep, pnprom, error, chk
      logical englsh, inchk, givchk
      integer sides(2)
      common /board/ board(120)
      common /movecm/ side, player, square,  mpiece
      common /info/ from$, to$, type$, propc$, cappc$
      common /colrcm/ color
      common /depth/ sdepth, depth, ply
      common /tree/ moves(2000), first(30), last(30), which(30),
     *              inchk(30), givchk(30)
      common /srchcm/ value(30), from(30), to(30), type(30), cappc(30)
      common /prevmv/ prevmv(6)
      equivalence (sqfrom(1),moves(201)),(sqto(1),moves(401)),
     *(fpiece(1),moves(601)),(tpiece(1),moves(801))
      common /buffer/ text(80)
      common /types/ normal, castkg, castqn, enpass, promot
      common /chrset/ alpha(46)
      equivalence  (dash,alpha(38)),(equal,alpha(41)),
     *(slash,alpha(39)),(zero,alpha(27)),(nine,alpha(36)),
     *(alphao,alpha(15)),(alphap,alpha(16)),(alphax,alpha(24)),
     *(alphae,alpha(5)),(blank,alpha(44)),(digits(1),alpha(28)),
     *(plus,alpha(37)),(colon,alpha(45))
      common /typntn/ englsh
      common /return/ return
      data pieces/'p','n','b','r','q','k'/
      data files/'r','n','b','q','k'/
      data sides / 1, -1 /
c
c------------------------------< initialize.
c
      mtype=normal
      chrpos=1
      if(color .ne. 1) go to 3
          files(4)=pieces(6)
          files(5)=pieces(5)
3     continue
      orank=0
      ofile=0
      ofileq=0
      drank=0
      dfile=0
      dfileq=0
      piece=0
      cpiece=0
      ppiece=0
      fromsq=0
      tosq=0
      ep=.false.
      pnprom=.false.
      chk=.false.
c
c------------------------------< generate all legalmoves
c------------------------------< and load for elimination
c
      from(1)=prevmv(1)
      to(1)=prevmv(2)
      tempf=first(ply)
      templ=last(ply)
      first(ply)=1001
      player=2-mod(ply,2)
      side=sides(player)
      tempd=depth
      depth=ply+1
      inchk(ply)=check(side)
      call movgen
      istop=last(ply)-1000
      first(ply)=tempf
      last(ply)=templ
      depth=tempd
c      print 173,istop
c173   format(/1x,'there were ',i3,' moves')
      do 155 i=1,istop
          from$=moves(i+1000)
          call extrct
          sqfrom(i)=from$
          sqto(i)=to$
          fpiece(i)=board(from$)
          tpiece(i)=cappc$

c          print 174,i,from$,to$,board(from$),cappc$,type$
c174       format(1x,'move=',i2,' fsq=',i4,' tsq=',i4,' p=',i3,' c=',
c     *              i3,' type=',i3)
c          englsh=.false.
c          englsh=.true.
c          call output(0,board,.false.)
c          print 175,(text(i3),i3=1,30)
c175       format(1x,'the move was ',30a1)
c
c      print 3141, piece,ofile,orank,cpiece,dfile,drank
c3141  format (1x,'p=',i2,' of=',i2,' or=',i2,' c=',i2,
c     *        ' df=',i2,' dr=',i2)



          callmover
          if(check(side)) sqfrom(i)=0
          call umover
155   continue
      if(text(1) .eq. alphao) go to 30
      if(.not. englsh) go to 8000
c
c------------------------------< process 'from' square and piece
c
      do 351 i=1,4
          if(text(i) .eq. slash)  go to 361
          if(text(i) .eq. dash) go to 361
          if(text(i) .eq. alphax) go to 361
351   continue
      go to 103
c
c------------------------------< moving piece : 'kbp'
c
361   continue
      if(i .lt. 4) go to 371
          ofileq=text(1)
          ofile=text(2)
          chrpos=3
          go to 38
371   continue
c
c------------------------------< moving piece : 'bp'
c
      if(i .lt. 3) go to 38
          ofileq=text(1)
           if(text(2) .ne. alphap)  go to 4463
               ofile=ofileq
               ofileq=0
4463      continue
          chrpos=2
38    continue
c
c------------------------------< moving piece : 'p'
c
      if(i .lt. 2) go to 385
      piece=text(chrpos)
      chrpos=chrpos+1
      if(text(chrpos) .ne. slash) go to 385
      chrpos=chrpos+1
      do 381 i=1,4
          if(text(chrpos-1+i) .eq. dash) go to 382
          if(text(chrpos-1+i) .eq. alphax) go  to 382
381   continue
      go to 103
c
c------------------------------< from square : '/kb3'
c
382   continue
      if(i .lt. 4) go to 383
          ofileq=text(chrpos)
          ofile=text(chrpos+1)
          orank=text(chrpos+2)
          chrpos=chrpos+3
          go to 385
383   continue
c
c------------------------------< from square : '/b3'
c
      if(i .lt. 3) go to 384
          ofile=text(chrpos)
          orank=text(chrpos+1)
          chrpos=chrpos+2
          go to 385
384   continue
c
c------------------------------< from square : '/3' or '/b'
c
      if(i .lt. 2) go to 103
      orank=text(chrpos)
      chrpos=chrpos+1
      if(orank.ge.zero .and. orank.le.nine) go to 385
      ofile=orank
      orank=0
c
c------------------------------< move type: 'x' or '-'
c
385   continue
      stype=text(chrpos)
      chrpos=chrpos+1
      if(stype.ne.dash .and. stype.ne.alphax) go to 103
      if(stype .eq. dash) go to 5
c
c------------------------------< process 'to' square and piece
c
      do 45 i=1,4
          if(text(chrpos-1+i) .eq. slash) go to 46
          if(text(chrpos-1+i) .eq. blank) go to 46
          if(text(chrpos-1+i) .eq. alphae) go to 46
          if(text(chrpos-1+i) .eq. plus) go to 46
          if(text(chrpos-1+i) .eq. equal) go to 46
45    continue
      go to 103
c
c------------------------------< captured piece : 'kbp'
c
46    continue
      if(i .lt. 4) go to 47
          dfileq=text(chrpos)
          dfile=text(chrpos+1)
          chrpos=chrpos+2
          go to 50
47    continue
c
c------------------------------< captured piece : 'bp'
c
      if(i .lt. 3) go to 50
          dfileq=text(chrpos)
          if(text(chrpos+1) .ne. alphap) go to 4464
              dfile=dfileq
              dfileq=0
4464      continue
          chrpos=chrpos+1
50    continue
c
c------------------------------< captured piece : 'p'
c
      if(i .lt. 2) go to 585
      cpiece=text(chrpos)
      chrpos=chrpos+1
      if(text(chrpos) .ne. slash) go to 585
      chrpos=chrpos+1
5     continue
      do 581 i=1,4
      if(text(chrpos-1+i) .eq. alphae) go to 582
          if(text(chrpos-1+i) .eq. blank) go to 582
          if(text(chrpos-1+i) .eq. equal) go to 582
          if(text(chrpos-1+i) .eq. plus) go to 582
581   continue
      go to 103
c
c------------------------------< to square : 'kn4'
c
582   continue
      if(i .lt. 4) go to 583
          dfileq=text(chrpos)
          dfile=text(chrpos+1)
          drank=text(chrpos+2)
          chrpos=chrpos+3
          go to 585
583   continue
c
c------------------------------< to square : 'n4'
c
      if(i .lt. 3) go to 584
          dfile=text(chrpos)
          drank=text(chrpos+1)
          chrpos=chrpos+2
          go to 585
584   continue
c
c------------------------------< to square : '4' or 'n'
c
      if(i .lt. 2) go to 103
      drank=text(chrpos)
      chrpos=chrpos+1
      if(drank.ge.zero .and. drank.le.nine) go to 585
          dfile=drank
          drank=0
585   continue
c
c------------------------------< process promotion '=q'
c
      if(text(chrpos) .ne. equal) go to 6
          ppiece=text(chrpos+1)
          pnprom=.true.
          chrpos=chrpos+2
6     continue
c
c------------------------------< process en passant 'ep'
c
      if(text(chrpos) .ne. alphae) go to 61
          ep=.true.
          chrpos=chrpos+2
61    continue
c
c------------------------------< process checks '+'
c
      if(text(chrpos) .ne. plus) go to 611
          chk=.true.
          chrpos=chrpos+1
611   continue
      if(text(chrpos) .ne. blank) go to 103
      if(ofileq .ne. 0) go to 6645
          if(ofile.eq.pieces(6) .or. ofile.eq.pieces(5)) ofileq=ofile
6645  continue
      if(dfileq .ne. 0) go to 66451
          if(dfile.eq.pieces(6) .or. dfile.eq.pieces(5)) dfileq=dfile
66451 continue
c
c------------------------------< convert character strings to
c------------------------------< integers for processing
c
      do 7 i=1,8
          if(orank .eq. digits(i)) orank=i+2
          if(drank .eq. digits(i)) drank=i+2
7     continue
      if(drank.lt.0 .or. orank.lt.0) go to 103
      if(ep .and. drank.ne.0) drank=drank+1
      if(side.lt.0 .and. orank.ne.0) orank=13-orank
      if(side.lt.0 .and. drank.ne.0) drank=13-drank
      do 8 i=1,5
          if(ofileq .eq. files(i)) ofileq=i+1
          if(dfileq .eq. files(i)) dfileq=i+1
          if(ofile .eq. files(i)) ofile=i+1
          if(dfile .eq. files(i)) dfile=i+1
8     continue
      if(dfile.lt.0 .or. ofile.lt.0) go to 103
      if(dfileq.lt.0 .or. ofileq.lt.0) go to 103
      do 9 i=1,6
          if(ppiece .eq. pieces(i)) ppiece=i
          if(cpiece .eq. pieces(i)) cpiece=i
          if(piece .eq. pieces(i)) piece=i
9     continue
      if(piece.lt.1 .or. piece.gt.6) go to 103
      if(cpiece.lt.0 .or. cpiece.gt.6) go to 103
      if(ppiece.lt.0 .or. ppiece.gt.6) go to 103
      piece=piece*side
c
c------------------------------< eliminate castling moves
c------------------------------< en passant, promotion, etc.
c
      do 10 i=1,istop
          from$=moves(i+1000)
          call extrct
          if(type$.eq.castkg .or. type$.eq.castqn) sqfrom(i)=0
          if(ep .and. type$.ne.enpass) sqfrom(i)=0
          if(pnprom .and.
     *        (type$.ne.promot .or. ppiece.ne.propc$)) sqfrom(i)=0
10    continue
c
c------------------------------< eliminate allmoves except
c------------------------------< moves of correct piece
c
7000  continue
      do 12 i=1,istop
          if(piece .eq. 0) go to 111
              if(piece .ne. fpiece(i)) sqfrom(i)=0
111       continue
          if(stype .ne. alphax) go to 11
          if(tpiece(i) .eq. 0) sqfrom(i)=0
          if(.not. englsh) go to 12
          if(tpiece(i) .ne. cpiece) sqfrom(i)=0
          go to 12
11        continue
          if(.not. englsh) go to 12
          if(tpiece(i) .ne. 0) sqfrom(i)=0
12    continue
c
c------------------------------< eliminate allmoves except
c------------------------------< moves on correct side of board
c
      do 14 i=1,istop
          if(ofileq .eq. 0) go to 13
          t=6-mod((sqfrom(i)-1)/5+1,2)
          if(t .ne. ofileq) sqfrom(i)=0
13        continue
          if(dfileq .eq. 0) go to 14
          t=6-mod((sqto(i)-1)/5+1,2)
          if(t .ne. dfileq) sqfrom(i)=0
14    continue
c
c------------------------------< eliminate allmoves except
c------------------------------< moves on correct files
c
      do 16 i=1,istop
          if(ofile .eq. 0) go to 15
          t=mod(sqfrom(i)-1,10)+1
          if(t.ne.ofile .and. t.ne.11-ofile) sqfrom(i)=0
15        continue
          if(dfile .eq. 0) go to 16
          t=mod(sqto(i)-1,10)+1
          if(t.ne.dfile .and. t.ne.11-dfile) sqfrom(i)=0
16    continue
c
c------------------------------< eliminate allmoves except
c------------------------------< those on the correct rank
c
      do 18 i=1,istop
          if(orank.eq.0) go to 17
          t=(sqfrom(i)-1)/10+1
          if(t.ne.orank) sqfrom(i)=0
17        continue
          if(drank .eq. 0) go to 18
          t=(sqto(i)-1)/10+1
          if(t .ne. drank) sqfrom(i)=0
18    continue
c
c------------------------------< eliminate allmoves but checks
c------------------------------< if so indicated.
c
      if(.not.chk) go to 189
      do 185 i=1,istop
          from$=moves(i+1000)
          call extrct
          callmover
          if(.not. check(-side)) go to 184
          call umover
          go to 185
184       continue
          call umover
          sqfrom(i)=0
185   continue
189   continue
c
c------------------------------< check to make sure only
c------------------------------< one move is left in moves
c
      ic=0
      do 19 i=1,istop
          if(sqfrom(i) .eq. 0) go to 19
          ic=ic+1
          imove=i
19    continue
      if(ic .gt. 1) go to 101
      if(ic .eq. 0) go to 102
      from$=moves(imove+1000)
      call extrct
      return=0
      return
c
c------------------------------< check castling moves
c
30    continue
      if(text(2).ne.dash .or. text(3).ne.alphao) go to 102
      if(text(4) .ne. blank) go to 35
32    continue
      typemv=castkg
      go to 36
35    continue
      if(text(4).ne.dash .or. text(5).ne.alphao) go to 102
      typemv=castqn
36    continue
      do 37 i=1,istop
          from$=moves(i+1000)
          call extrct
          if(typemv .eq. type$) go to 39
37    continue
      go to 102
39    continue
      return=0
      return
c
c------------------------------< decode algebraic notation.
c------------------------------< process 'pe2-e4' type input.
c
8000  continue
      do 8025 i=1,10
          if(text(i)  .eq.  plus) chk=.true.
          if(text(i)  .ne.  equal) go to 8025
              pnprom=.true.
              ppiece=text(i+1)
8025  continue
      piece=text(1)
      chrpos=1
      if(text(2).lt.zero .or. text(2).gt.nine) go to 8050
          chrpos=0
          piece=pieces(1)
8050  continue
      stype=text(chrpos+3)
      if(stype .eq. colon) stype=alphax
      do 8100 i=1,8
          if(text(chrpos+1) .eq. alpha(i)) ofile=10-i
          if(text(chrpos+2) .eq. digits(i)) orank=i+2
          if(text(chrpos+4) .eq. alpha(i)) dfile=10-i
          if(text(chrpos+5) .eq. digits(i)) drank=i+2
8100  continue
      do 7001 i=1,6
          if(piece .eq. pieces(i)) piece=i*side
          if(ppiece .eq. pieces(i)) piece=i*side
7001  continue
      if(text(5) .eq. blank) go to 7500
      if(orank.eq.0 .or. ofile.eq.0) go to 103
      if(drank.eq.0 .or. dfile.eq.0) go to 103
      piece=0
7002  continue
      if(color.eq.1) go to 7005
          if(ofile .ne. 0) ofile=11-ofile
          if(orank.ne.0) orank=13-orank
          if(dfile .ne. 0) dfile=11-dfile
          if(drank .ne. 0) drank=13-drank
7005  continue
      ofileq=5
      dfileq=5
      if(ofile .gt. 5) ofileq=6
      if(dfile .gt. 5) dfileq=6
      if(ofile .eq. 0) ofileq=0
      if(dfile .eq. 0) dfileq=0
      go to 7000
c
c------------------------------< process the simplest form of input
c------------------------------< of the form 'pe4'
c
7500  continue
      dfile=ofile
      drank=orank
      ofile=0
      orank=0
      go to 7002
c
c------------------------------< error messages
c
101   if(.not. error) go to 1011
      print 201
201   format(1x,'that''s ambiquous')
1011  continue
      return=1
      return
102   if(.not. error) go to 1021
      print 202
202   format(1x,'illegalmove')
1021  continue
      return=1
      return
103   if(.not. error) go to 1031
      print 203
203   format(1x,'unrecognizable move')
1031  continue
      return=1
      return
      end


