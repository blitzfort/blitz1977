      subroutine setio
c
c     ******************************************************************
c     *                                                                *
c     *      setio is used to open fortran i/o units 1 and 2 to        *
c     *  specific file names based on the opponent's name.             *
c     *      'savegm' is an entry point used to save the entire        *
c     *  program data area after each move so that the game may be     *
c     *  restarted after system/communication failure without losing   *
c     *  any statu8.                                                   *
c     *      setio asks the user if the game is being restarted.  if   *
c     *  so, the entire data area is restored from disc to return      *
c     *  to the game as it was at the time of failure,                 *
c     *                                                                *
c     ******************************************************************
c
      implicit integer (a-z)
      common /autos/ autos
      common /beep/ beep
      common /board/ board(120)
      common /bookcm/ bookcm(2)
      common /castcm/ castcm(96)
      common /colr cm/ colrcm
      common /depth/ depth(3)
      common /drawcm/ drawcm
      common /dup/ dup(1041)
      common /eval/ eval(2)
      common /kill mv/ killmv(600)
      common /limits/ limits(2)
      common /lmove/ lmove(2)
      common /mode/ mode(5)
      common /mov cnt/ movcnt(2)
      common /namecm/ namecm(5)
      common /predcm/ predcm(34)
      common /prev mv/ prevmv(6)
      common /ratecm/ ratecm(2)
      common /ratio/ ratio
      common /savecm/ savecm(106)
      common /srchcm/ srchcm(150)
      common /statcm/ statcm(94)
      common /timecm/ timecm(16)
      common /trcecm/ trcecm(32)
      common /typ ntn/ typntn
      common /window/ window(2)
      common /chr set/ alpha(46)
      equivalence (alphan,alpha(14)),(alphay,alpha(25))
      common /buffer/ text(80)
      common /return/ return
c
c------------------------------< determine whether to create new
c------------------------------< save files or read in existing ones
c------------------------------< to restart an old game.
c
10    continue
      print 20,namecm
20    format(1x,'are you continuing your last game, ',5a4)
      call read
      if(text(1).ne.alphay .and. text(1).ne.alphan) go to 10
110   continue
c
c------------------------------< if not restarting a game, initialize
c------------------------------< the save files by opening in output
c------------------------------< mode and then closing to create them.
c------------------------------< if restarting, open the save files
c------------------------------< and read in all of the old status.
c
      if(text(1).eq.alphay) go to 50
50    continue
      if(text(1).eq.alphan) go to 9999
      read(2) autos, beep, board, bookcm,  castcm,  colrcm, depth,
     *        drawcm, dup, eval, killmv, limits, lmove, mode, movcnt,
     *        namecm, predcm, prevmv, ratecm, ratio, savecm, srchcm,
     *        statcm, timecm, trcecm, typntn, window
      return=1
      return
c
c------------------------------< save gm is called to write all
c------------------------------< useful common blocks to fortran unit
c------------------------------< 2 so that a game may be restarted
c------------------------------< if necessary.
c
      entry save gm
      rewind 2
      write(2) autos, beep, board, bookcm, castcm,  colrcm,  depth,
     *         drawcm,  dup, eval, killmv, limits, lmove, mode, movcnt,
     *         namecm,  predcm, prevmv, ratecm, ratio, savecm, srchcm,
     *         statcm, timecm, trcecm, typntn, window
9999  continue
      return=0
      return
      end
