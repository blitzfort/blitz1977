      subroutine scorep
c
c     ******************************************************************
c     *                                                                *
c     *      scorep is called to evaluate the pawn structure for both  *
c     *  sides.  it operates in two pahses:  phase 1 simply locates    *
c     *  the most advanced and least advanced pawn on each file for    *
c     *  both sides; phase 2 uses this information to determine if     *
c     *  pawns are isolated, connected, doubled, tripped, backward,    *
c     *  passed or form a phalnx.  also, scores for advancement are    *
c     *  computed to encourage certain types of advances.              *
c     *                                                                *
c     ******************************************************************
c
      implicit integer (a-z)
      logical ppass, opass, attack
      common /board/ board(120)
      common /mscore/ sscore, mscore, pscore, tscore
      common /move cm/ side, player, square, mpiece
      common /pawns/ pfirst(10), plast(10), pcount(10), ppass(10),
     *               ofirst(10), olast(10), ocount(10), opass(10),
     *               arank, afile
      common /pfiles/ pfiles(10), pranks(9)
      common /k loc cm/ pkingl, prank, pfile, okingl, orank, ofile
      common /pieces/ nppwns, nppcs, pqueen, nopwns, nopcs, oqueen
      common /piec cm/ pieces(6)
      common /movdir/ movdir(28), piecem(28), begin(6), end(6)
c
c------------------------------< initialize.
c
      pqdist=100
      oqdist=100
c
c------------------------------< now that all pawns have been located
c------------------------------< and counted, analyze the relationships
c------------------------------< to compute a relative score for the
c------------------------------< pawn structure.
c
      do 2000 file=2,9
          ppass(file)=.false.
          opass(file)=.false.
          countp=pcount(file)
          counto=ocount(file)
          lastp=plast(file)
          lasto=olast(file)
          firstp=pfirst(file)
          firsto=ofirst(file)
c
c------------------------------< score pawns based on how many ranks
c------------------------------< they have been advanced. only score
c------------------------------< the first and last on a file.,ignore
c------------------------------< the others(only present when tripled,
c------------------------------< a severe weakness anyway).
c
          if(countp .eq. 0) go to 1010
          pawnsq=lastp*10+file
              pscore=pscore+pfiles(file)*pranks(lastp)
              if(firstp .eq. lastp) go to 1010
              pscore=pscore+pfiles(file)*pranks(firstp)
1010      continue
          if(counto .eq. 0) go to 1020
          pawnsq=lasto*10+file
              pscore=pscore-pfiles(file)*pranks(10-lasto)
              if(firsto .eq. lasto) go to 1020
              pscore=pscore-pfiles(file)*pranks(10-firsto)
1020      continue
c
c------------------------------< determine if the pawn is isolated.
c------------------------------< if one pawn on the file is isolated,
c------------------------------< then other pawns on the same file
c------------------------------< are also isolated.
c
          if(countp .eq. 0) go to 1030
          if(pcount(file-1) .ne. 0) go to 1030
          if(pcount(file+1) .ne. 0) go to 1030
              pscore=pscore-countp*60
1030      continue
          if(counto .eq. 0) go to 1040
          if(ocount(file-1) .ne. 0) go to 1040
          if(ocount(file+1) .ne. 0) go to 1040
              pscore=pscore+counto*60
1040      continue
c
c------------------------------< determine if more than one pawn
c------------------------------< is on this file. if so, penalize
c------------------------------< the score for doubled/trippled pawns.
c
          if(countp .eq. 2) pscore=pscore-25
          if(countp .eq. 3) pscore=pscore-800
          if(counto .eq. 2) pscore=pscore+25
          if(counto .eq. 3) pscore=pscore+800
c
c------------------------------< determine if the most advanced pawn
c------------------------------< on this file is passed. if no enemy
c------------------------------< pawns are in front of it either on
c------------------------------< this or an adjacent file, it is passed
c------------------------------< and valuable.
c
          if(countp .eq. 0)  go to 1060
          if(lastp .lt. ofirst(file)) go to 1060
          if(lastp .lt. ofirst(file-1)) go to 1060
          if(lastp .lt. ofirst(file+1)) go to 1060
              passed=20
              if(iabs(plast(file-1)-lastp) .lt. 2) go to 1050
              if(iabs(plast(file+1)-lastp) .ge. 2) go to 1055
1050          continue
                  passed=passed+10
1055          continue
              if(board(lastp*10+file+10) .lt. 0) passed=passed-10
              ppassc=passed*(lastp-1)**2
              pscore=pscore+ppassc
              ppass(file)=.true.
1060      continue
          if(counto .eq. 0)  go to 1080
          if(lasto .gt. pfirst(file)) go to 1080
          if(lasto .gt. pfirst(file-1)) go to 1080
          if(lasto .gt. pfirst(file+1)) go to 1080
              passed=20
              if(iabs(olast(file-1)-lasto) .lt. 2) go to 1070
              if(iabs(olast(file+1)-lasto) .ge. 2) go to 1075
1070          continue
                  passed=passed+10
1075          continue
              if(board(lasto*10+file-10) .gt. 0) passed=passed-10
              opassc=passed*(10-lasto)**2
              pscore=pscore-opassc
              opass(file)=.true.
1080      continue
c
c------------------------------< determine if the most advanced pawn
c------------------------------< on this file is on a half-open file.
c------------------------------< if so, it must be supported by a
c------------------------------< friendly pawn or it must be able to
c------------------------------< be supported by a pawn.  if not,
c------------------------------< it is considered backwared and weak
c
          if(countp .eq. 0) go to 1090
          if(counto .ne. 0) go to 1090
          if(pfirst(file-1) .le. lastp) go to 1090
          if(pfirst(file+1) .le. lastp) go to 1090
              pscore=pscore-25
              if(board(lastp+21) .eq. -1) go to 1085
              if(board(lastp+19) .ne. -1) go to 1090
1085              continue
                  pscore=pscore-25
1090      continue
          if(counto .eq. 0) go to 1100
          if(countp .ne. 0) go to 1100
          if(ofirst(file-1) .ge. lasto) go to 1100
          if(ofirst(file+1) .ge. lasto) go to 1100
              pscore=pscore+25
              if(board(lasto-21) .eq. 1) go to 1095
              if(board(lasto-19) .ne. 1) go to 1100
1095              continue
                  pscore=pscore+25
1100      continue
c
c------------------------------< determine if any passed pawns exist. if
c------------------------------< so and the opposing side has a no pieces
c------------------------------< other than the king, determine if the
c------------------------------< passed pawn can queen before the king
c------------------------------< can get to it. if so, remember the
c------------------------------< distance to the queening square.
c
          if(.not. ppass(file)) go to 1130
          if(nopcs .gt. 1) go to 1130
              pdist=9-lastp
              odist=max0(9-orank,iabs(ofile-file))
              kdist=max0(9-prank,iabs(pfile-file))
              if(player .eq. 2) odist=odist-1
              if(pfile.eq.file .and. prank.gt.lastp) pdist=pdist+1
              if(lastp .eq. 3) pdist=pdist-1
              if(kdist .ne. 1) go to 1122
                  if(file.ne.2 .and. file.ne.9) go to 1121
                      if(pfile .ne. file) go to 1121
                      if(prank .lt. lastp) go to 1121
                      if(iabs(prank-orank) .gt. player+1) go to 1121
                      if(iabs(pfile-ofile) .gt. player+1) go to 1121
                      go to 1130
1121              continue
                  distp=max0(iabs(pfile-file),iabs(prank-lastp))
                  disto=max0(iabs(ofile-file),iabs(orank-lastp))
                  if(distp .le. disto+2-player) go to 1125
1122          continue
              if(odist .le. pdist)  go to 1130
1125          continue
              if(pqdist .lt. pdist) go to 1130
                  pqdist=pdist
                  ppnsq=lastp*10+file
1130      continue
          if(.not. opass(file)) go to 1140
          if(nppcs .gt. 1) go to 1140
              pdist=lasto-2
              kdist=max0(orank-2,iabs(ofile-file))
              odist=max0(prank-2,iabs(pfile-file))
              if(player .eq. 1) odist=odist-1
              if(ofile.eq.file .and. orank.lt.lasto) pdist=pdist+1
              if(lasto .eq. 8) pdist=pdist-1
              if(kdist .ne. 1) go to 1132
                  if(file.ne.2 .and. file.ne.9) go to 1131
                      if(ofile .ne. file) go to 1131
                      if(orank .gt. lasto) go  to 1131
                      if(iabs(orank-prank) .gt. 4-player) go to 1131
                      if(iabs(ofile-pfile) .gt. 4-player) go to 1131
                      go to 1140
1131              continue
                  disto=max0(iabs(ofile-file),iabs(orank-lasto))
                  distp=max0(iabs(pfile-file),iabs(prank-lasto))
                  if(disto .le. distp+player-1) go to 1135
1132          continue
              if(odist .le. pdist)  go to 1140
1135          continue
              if(oqdist .lt. pdist) go to 1140
                  oqdist=pdist
                  opnsq=lasto*10+file
1140      continue
2000  continue
c
c------------------------------< now evaluate any passed pawns that can
c------------------------------< queen with no interference. if one pawn
c------------------------------< queens two ore move before the
c------------------------------< opponent, give that side credit for a
c------------------------------< new queen. if they queen together, give
c------------------------------< no credit unless the first one queens
c------------------------------< with check.
c
      if(pqdist+oqdist .eq. 200) go to 2400
      ptemp=pqdist
      otemp=oqdist
      if(player .eq. 1) ptemp=ptemp-1
      if(player .eq. 2) otemp=otemp-1
      if(oqdist .le. ptemp) go to 2200
          if(pqdist .lt. otemp) go to 2100
              qnsq=90+mod(ppnsq,10)
              board(ppnsq)=0
              temp=board(qnsq)
              board(qnsq)=5
              if(attack(1,okingl)) go to 2050
              if(mod(ppnsq,10) .ne. mod(opnsq,10)) go to 2030
              if(orank .gt. 3) go to 2050
              if(iabs(ofile-mod(opnsq,10)) .ne. 1) go to 2050
2030          continue
                  pqdist=100
2050          continue
              board(ppnsq)=1
              board(qnsq)=temp
              if(pqdist .eq. 100) go to 2400
2100      continue
          pscore=pscore+pieces(5)-2*pieces(1)
          go to 3000
2200  continue
          if(oqdist .lt. ptemp) go to 2300
              qnsq=20+mod(opnsq,10)
              board(opnsq)=0
              temp=board(qnsq)
              board(qnsq)=-5
              if(attack(-1,pkingl)) go to 2250
              if(mod(opnsq,10) .ne. mod(ppnsq,10)) go to 2230
              if(prank .lt. 8) go to 2250
              if(iabs(pfile-mod(ppnsq,10)) .ne. 1) go to 2250
2230          continue
                  oqdist=100
2250          continue
              board(opnsq)=-1
              board(qnsq)=temp
              if(oqdist .eq. 100) go to 2400
2300      continue
          pscore=pscore-pieces(5)+2*pieces(1)
          go to 3000
2400  continue
c
c------------------------------< evaluate an ending with only one pawn.
c------------------------------< if the winning side's king is two ranks
c------------------------------< in front of the pawn (not rook pawn),
c------------------------------< the game is won. if the king is one
c------------------------------< square in front of the pawn with the
c------------------------------< opposition, the game is won, all others
c------------------------------< are evaluated as draws.
c
      if(nppcs+nopcs .gt. 2) go to 3000
      if(nppwns+nopwns .ne. 1) go to 3000
          if(nppwns .eq. 0) go to 3200
              ppfile=mod(pawnsq,10)
              if(ppfile.eq.2 .or. ppfile.eq.9) go to 3000
                  pprank=pawnsq/10
                  if(iabs(pfile-ppfile) .gt. 1) go to 3000
                  if(orank-prank .lt. 2) go to 3100
                  if(pprank-orank .ge. 2) go to 3100
                  if(prank-pprank .ge. 2) go to 3100
                      if(prank-pprank .ne. 1) go to 3000
                      if(player .eq. 2) go to 3050
                          do 3010 i=17,24
                              tkingl=pkingl+movdir(i)
                              tfile=mod(tkingl,10)
                              trank=tkingl/10
                              if(tfile .ne. ofile) go to 3010
                              if(mod(iabs(trank-orank),2) .eq. 0)
     *                                                    go to 3075
3010                      continue
                          go to 3000
3050                  continue
                          if(iabs(ppile-ofile).eq.2 .and.
     *                       iabs(prank-orank).eq.2) go to 3075
                              if(pfile .ne. ofile) go to 3000
                              if(mod(iabs(prank-orank),2) .eq. 0)
     *                                                    go to 3075
                              go to 3000
3075                      continue
                          if(ppfile .eq. pfile) go to 3100
                              pscore=pscore-pieces(1)
3100              continue
                  if(iabs(ofile-ppfile) .gt. 1) go to 3150
                  if(orank.lt.prank .and. orank.gt.pprank) go to 3000
3150              continue
                  pscore=pscore+pieces(5)-3*pieces(1)
                  if(prank .lt. 8) go to 3000
                  if(iabs(pfile-ppfile) .gt. 1) go to 3000
                      pscore=pscore+pieces(1)
                  go to 3000
3200      continue
              opfile=mod(pawnsq,10)
              if(opfile.eq.2 .or. opfile.eq.9) go to 3000
                  oprank=pawnsq/10
                  if(iabs(ofile-opfile) .gt. 1) go to 3000
                  if(orank-prank .lt. 2) go to 3300
                  if(prank-oprank .ge. 2) go to 3300
                  if(oprank-orank .ge. 2) go to 3300
                      if(oprank-orank .ne. 1) go to 3000
                      if(player .eq. 1) go to 3250
                          do 3210 i=17,24
                              tkingl=okingl+movdir(i)
                              tfile=mod(tkingl,10)
                              trank=tkingl/10
                              if(tfile .ne. pfile) go to 3210
                              if(mod(iabs(trank-prank),2) .eq. 0)
     *                                                    go to 3275
3210                      continue
                          go to 3000
3250                  continue
                          if(iabs(ofile-pfile).eq.2 .and.
     *                       iabs(orank-prank).eq.2) go to 3275
                              if(ofile .ne. pfile) go to 3000
                              if(mod(iabs(orank-prank),2) .eq. 0)
     *                                                    go to 3275
                              go to 3275
3275                      continue
                          if(opfile .eq. ofile) go to 3300
                              pscore=pscore+pieces(1)
3300              continue
                  if(iabs(pfile-opfile) .gt. 1) go to 3350
                  if(prank.gt.orank .and. prank.lt.oprank) go to 3000
3350              continue
                  pscore=pscore-pieces(5)+3*pieces(1)
                  if(orank .gt.3) go to 3000
                  if(iabs(ofile-opfile) .gt. 1) go to 3000
                      pscore=pscore-pieces(1)
3000  continue
      return
      end


