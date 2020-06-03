      subroutine break
c
c     ******************************************************************
c     *                                                                *
c     *      break is used to control the use of the 'break' or        *
c     *  'attn' key on timesharing terminals.  it must set some type   *
c     *  of opepating system flag so that a break or attn keyin will   *
c     *  cause control to temporarily go to a routine that sets the    *
c     *  variable 'broke' in labeled common block 'broke' to a logical *
c     *  true.                                                         *
c     *      the break control routine simply sets a flag 'broke'      *
c     *  true so that 'status' will be called as soon as possible      *
c     *  to notify the user of the current search status.              *
c     *                                                                *
c     ******************************************************************
c
      implicit integer (a-z)
      logical broke
      common /tflag/ tflag
      common /broke/ broke
c
c------------------------------< get break control
c
      return
c
c------------------------------< reset the trace level flag and
c------------------------------< set the 'broke' indicator so
c------------------------------< that 'status' will be called at
c------------------------------< the first convenient time.
c
100   continue
      tflag=0
      broke=.true.
      end


