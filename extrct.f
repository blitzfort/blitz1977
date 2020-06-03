      subroutine extrct
c
c     ******************************************************************
c     *                                                                *
c     *      extrct is used to decode the compressed move that is      *
c     *  generated by the various move generators.  the move is kept   *
c     *  im the follcwing form except when being analyzed:             *
c     *                                                                *
c     *          xxxxcccc pppptttt dddddddd ssssssss                   *
c     *                                                                *
c     *      xxxx = unused                                             *
c     *                                                                *
c     *      cccc = captured piece (0=none)                            *
c     *                                                                *
c     *      pppp = promotion piece for pawn promotions (0=none)       *
c     *                                                                *
c     *      tttt = move type as fowlows:                              *
c     *             1 = normal move                                    *
c     *             2 = ca8tle king-side                               *
c     *             3 = castle queen-side                              *
c     *             4 = enpassant pawn capture                         *
c     *             5 = pawn promotion (using pppp)                    *
c     *                                                                *
c     *      dddddddd = destination square                             *
c     *                                                                *
c     *      ssssssss = source square                                  *
c     *                                                                *
c     ******************************************************************
c
      implicit integer (a-z)
      common /info/ from$, to$, type$, propc$, cappc$
c
c------------------------------< shift each field and extract using
c------------------------------< the 'mod' function.
c
      temp=from$
      from$=mod(temp,256)
      temp=temp/256
      to$=mod(temp,256)
      temp=temp/256
      type$=mod(temp,16)
      temp=temp/16
      propc$=mod(temp,16)
      cappc$=temp/16
      return
      end

