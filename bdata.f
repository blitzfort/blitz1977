      block data
c
c     ******************************************************************
c     *                                                                *
c     *      bdata is used to initialize all labelled common.          *
c     *                                                                *
c     ******************************************************************
c
      implicit integer (a-z)
c
c
c
      logical over
      logical cputim, cquery
      logical englsh
      logical tmode, smode, pndrng, foundm, matchd
      logical broke, autos
      logical in book
      real ratio
      logical easy
      logical abort
      logical beep
      logical eboard
c
c
c
      common /board/ board(120)
      common /srchcm/ value(30), from(30), to(30), type(30), cappc(30)
      common /depth/ sdepth, depth, ply
      common /limits/ fdepth, ftime
      common /movdir/ movdir(28), piecem(28), begin(6), end(6)
      common /types/ normal, castkg, castqn, enpass, promot
      common /colr cm/ color
      common /prev mv/ prevmv(6)
      common /mov cnt/ npmovs, nomovs
      common /chrset/ alpha(46)
      common /over/ over
      common /timecm/ gmoves, gelap, smoves, selap, surpls, cputim,
     *                 cquery, pelap, oelap, psec1, psec2, osec1,
     *                 osec2, avgtim, expect, fsec1
      common /cbias/ cbias(2)
      common /piec cm/ pieces(6)
      common /typ ntn/ englsh
      common /mode/ tmode, smode, pndrng, foundm, matchd
      common /tflag/ tflag
      common /trcecm/ strace(32)
      common /pawns/ pfirst(10), plast(10),  pcount(10),  ppass(10),
     *               ofirst(10), olast(10),  ocount(10),  opass(10),
     *               arank, afile
      common /pfiles/ pfiles(10), pranks(9)
      common /broke/ broke
      common /bookcm/ in book, key
      common /ratio/ ratio
      common /ratecm/ prate, orate
      common /easy/ easy, easyv
      common /killmv/ killmv(20,30)
      common /statcm/ nodes(30), times(30), aborts(30), tnodes, snodes,
     *                 hashes, pright
      common /l move/ lmovep, lmoveo
      common /dup/ bdsave(1040), point
      common /draw cm/ drawsc
      common /abort/ abort
      common /mscore/ sscore, mscore, pscore, tscore
      common /eval/ eval, peval
      common /autos/ autos
      common /beep/ beep
      common /eboard/ eboard, lbrack, rbrack
      common /ctime/ dnodes, xnodes
      common /htable/ hsize, htable(32768)
c
c
c
      data board / 120*99 /
      data value(1) / 0 /
      data sdepth, depth, ply / 0, 0, 0 /
      data fdepth, ftime / 0, 0 /
      data movdir /  +10, -10, +1,  -1, +11, -11,  +9,  -9,
     *               +8, +12, +19, +21,  -8, -12, -19, -21,
     *               +1,  +9, +10, +11,  -1,  -9, -10, -11,
     *               +9, +11, +10, +20 /
      data piecem / 4*4, 4*3, 8*2, 8*6, 4*1 /
      data begin / 25,  9,  5,  1,  1, 17 /
      data end   / 28, 16,  8,  4,  8, 24 /
      data normal, castkg, castqn, enpass, promot
     *   /      1,      2,      3,      4,      5        /
      data color / 2 /
      data prevmv / 6*0 /
      data npmovs, nomovs / 0, 0 /
      data alpha /'a','b','c','d','e','f','g','h','i','j','k','l',
     *'m','n','o','p','q','r','s','t','u','v','w','x','y','z','0',
     *'1','2','3','4','5','6','7','8','9','+','-','/','*','=','.',
     *'?',' ',':',','/
      data over /  .false.  /
      data gmoves, gelap, smoves, selap, surpls
     * /       60,  3600,     30,  1800,      0  /
      data cputim, cquery, pelap,  oelap
     * /   .true.,.false.,     0,      0 /
      data cbias / 0, 70 /
      data pieces / 1000,  3000, 3000,  5000,  9000,  1000000000  /
      data englsh / .true. /
      data tmode,   smode,    pndrng,  foundm, matchd /
     *   .false., .false., .false.,  .false., .false. /
      data tflag / 0 /
      data strace / 32*0 /
      data pfirst, plast,  pcount, ofirst,  olast,  ocount
     * /   10*100,  10*0,   10*0,    10*0, 10*100,    10*0  /
      data pfiles / 0, 0,  0, 10, 12,  15, 8,  0, 0,  0  /
      data pranks / 0, 0,  0, 1, 4, 1,  1, 1,  1  /
      data broke / .false. /
      data in book / .false. /
      data key / 1 /
      data ratio / 5.0 /
      data prate, orate / 1670, 1500 /
      data easy / .false. /
      data killmv / 600*0 /
      data pright / 0 /
      data tnodes / 0 /
      data lmovep, lmoveo / 0, 0 /
      data bdsave, point / 1040*0, 0 /
      data drawsc  / 0 /
      data abort / .false. /
      data sscore, mscore, pscore, tscore /  0, 0, 0, 0 /
      data eval,  peval / 0, 0 /
      data autos  / .false. /
      data beep  /  .false. /
      data eboard, lbrack, rbrack / .false., '[', ']' /
      data dnodes, xnodes / 100,  0 /
      data hsize  / 4096 /
      end
