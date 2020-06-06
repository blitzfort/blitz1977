      subroutine book
c
c     ******************************************************************
c     *                                                                *
c     *      book search the chess library of book moves for a         *
c     *  response to the opponent's last move.  in most cases, there   *
c     *  will be severalmoves to choose from.  the program will choose*
c     *  a random move ignoring any 'poor' or '?' type moves that may  *
c     *  be present.  this feature provides variety so that it is      *
c     *  more difficult to find an area of weak play and captitalize   *
c     *  on it over and over.  due to the structure of the book data   *
c     *  base, this routine will never require over two i/o operations *
c     *  to find a response making it very effective as a time-saver.  *
c     *     a seperate program is used to construct the library of     *
c     *  moves to keep unnecessary programming code out of the prime   *
c     *  chess playing program.                                        *
c     *                                                                *
c     ******************************************************************
c
      implicit integer (a-z)
      logical inbook
c     define file 3(5000,40,u,iav)
      common /tree/ moves(2000),dummy(150)
      common /trcecm/ strace(32)
      common /prevmv/ prevmv(6)
      common /movcnt/ npmovs, nomovs
      common /colrcm/ color
      common /statcm/ nodes(30), times(30), aborts(30), tnodes, snodes,
     *                  hashes, pright
      common /types/ normal, castkg, castqn, enpass, promot
      common /info/ from$, to$, type$, propc$, cappc$
      common /bookcm/ inbook, key
      common /return/ return
c
c------------------------------< if it is the first move and the
c------------------------------< computer is white, choose the first
c------------------------------< move in the book file.
c
      if(npmovs+nomovs .eq. 0) go to 5
c
c------------------------------< read in the record with the human's
c------------------------------< possible responses.  then match the
c------------------------------< human's move against the list to
c------------------------------< get the pointer to the record with
c------------------------------< the computer's possible replies.
c
      key=mod(key,10000)
      if(key .eq. 0) go to 3
      read(3'key)(moves(i),i=1,40)
c
c------------------------------< match the opponent's move
c
      do 2 i=1,20
          from$=moves(i)
          call extrct
          if(color .eq. 1) go to 1
              from$=121-from$
              to$=121-to$
1         continue
          if(type$ .ne. prevmv(6)) go to 2
          if(type$.eq.castkg .or. type$.eq.castqn) go to 4
          if(from$.eq.prevmv(4) .and. to$.eq.prevmv(5)) go to 4
2     continue
3     continue
      inbook=.false.
      return=0
      return
4     continue
      key=moves(i+20)
      if(key .eq. 0) go to 3
c
c------------------------------< read in the record with the program's
c------------------------------< possible replise to the last human
c------------------------------< move and chose one of them. the
c------------------------------< program normally chooses a random
c------------------------------< move.
c
5     continue
      read(3'key)(moves(i),i=1,40)
c
c------------------------------< choose a random move for variety.
c
100   continue
      max=0
      do 400 i=1,20
          if(moves(i) .eq. 0) go to 500
          if(moves(i)/2**28 .eq. 0) max=max+1
400   continue
500   continue
      rtemp=time()
      mvnumb=mod(rtemp,max)+1
c
c------------------------------< find the n'th ''unquestioned'' move
c------------------------------< in the data base.
c
300   continue
      do 800 which=1,20
          if(moves(which) .eq. 0) go to 800
          if(moves(which)/2**28 .ne. 0) go to 800
          mvnumb=mvnumb-1
          if(mvnumb.le.0) go to 900
800   continue
c
c------------------------------< no safe move exists, return with
c------------------------------< no book move.
c
      go to 3
c
c------------------------------< a safe move has been found, correct
c------------------------------< it for the program's color and set
c------------------------------< up for the next match.
c
900   continue
      from$=moves(which)
      call extrct
      if(color .eq. 1) go to 6
          from$=121-from$
          to$=121-to$
6     continue
      strace(1)=from$+to$*256*type$*65536+propc$*1048576+cappc$*16777216
      strace(31)=1
      strace(32)=0
      key=key*10000+moves(which+20)
c
c------------------------------< count bookmoves as correct preditions
c------------------------------< for statistical purposes.
c
      pright=pright+1000
      return=1
      return
      end


