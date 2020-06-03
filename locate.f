      subroutine locate
c
c     ******************************************************************
c     *                                                                *
c     *      locate is called to set up various tables for use by the  *
c     *  scoring functions.  it scans the board to locate pawns and    *
c     *  pieces and stores information about piece/pawn location in    *
c     *  tables for later use,  the kings are located so that tropism  *
c     *  can be evaluated later also,  pieces are counted for use in   *
c     *  evaluating king safety later.                                 *
c     *                                                                *
c     ******************************************************************
c
      implicit integer (a-z)
      logical ppass, opass
      common /board/ board(120)
      common /pawns/ pfirst(10), plast(10), pcount(10), ppass(10),
     *               ofirst(10), olast(10), ocount(10), opass(10),
     *               arank, afile
      common /k loc cm/ pkingl, prank, pfile, okingl, orank, ofile
      common /pieces/ nppwns, nppcs, pqueen, nopwns, nopcs, oqueen
      common /piec cm/ pieces(6)
c
c------------------------------< first, analyze the pawn structure to
c------------------------------< determine the following factors:
c------------------------------< (1) hwo many pawns are on each file,
c------------------------------< (2) the rank of the first pawn on each
c------------------------------< file, (3) the rank of the last pawn on
c------------------------------< each file. these will be used to
c------------------------------< determine the pawn scoring.
c------------------------------< also, count pieces and pawns
c------------------------------< for each side so that king
c------------------------------< safety can be evaluated later.
c
      pqueen=0
      oqueen=0
      nppwns=0
      nppcs=0
      nopwns=0
      nopcs=0
      arank=0
      afile=0
      acount=0
c
c------------------------------< process each rank.
c
      do 400 sq=22,29
          file=sq-20
          pfirst(file)=100
          plast(file)=0
          pcount(file)=0
          ofirst(file)=0
          olast(file)=100
          ocount(file)=0
c
c------------------------------< process each file
c
          do 300 square=sq,100,10
              rank=square/10
              piece=board(square)
              if(piece .gt. 6) go to 300
c hand written note - at the side of the page - (circled) 8+3=11
c hand written note -                                          8
              index=piece+7
              go to (260,250,220,220,220,210,300,
     *               110,120,120,120,150,160),index
c
c------------------------------< program pieces are counted by
c------------------------------< this routine.
c
110           continue
                  nppwns=nppwns+1
                  pcount(file)=pcount(file)+1
                  plast(file)=rank
                  if(pfirst(file) .eq. 100) pfirst(file)=rank
                  arank=arank+rank
                  afile=afile+file
                  acount=acount+1
                  go to 300
150           continue
                  pqueen=3
120           continue
                  nppcs=nppcs+1
                  go to 300
160           continue
                  pkingl=square
                  prank=rank
                  pfile=file
                  nppcs=nppcs+1
                  go to 300
c
c------------------------------< opponent pieces are counted by
c------------------------------< this routine
c
210           continue
                  nopwns=nopwns+1
                  ocount(file)=ocount(file)+1
                  ofirst(file)=rank
                  if(olast(file) .eq. 100) olast(file)=rank
                  arank=arank+rank
                  afile=afile+file
                  acount=acount+1
                  go to 300
250           continue
                  oqueen=3
220           continue
                  nopcs=nopcs+1
                  go to 300
260           continue
                  okingl=square
                  orank=rank
                  ofile=file
                  nopcs=nopcs+1
300       continue
400   continue
c
c------------------------------< now calculate the location of the
c------------------------------< 'average' pawn position so that kings
c------------------------------< can move toward where the pawns are.
c
      arank=arank/acount
      afile=afile/acount
      return
      end


