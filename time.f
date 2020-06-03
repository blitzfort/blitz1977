      subroutine time(rsecs,msecs)
c
c     ******************************************************************
c     *                                                                *
c     *      time is used to monitor time utilization.  the real or    *
c     *  wall clock time is returned in variable 'rsecs' and the       *
c     *  computer or processor time is returned in variable 'msecs'.   *
c     *      all times are in 1/100 seconds and are cumulative for the *
c     *  current run; to time a process, call time before and after    *
c     *  the process and compute the difference to get the time spent  *
c     *  in processing.                                                *
c     *                                                                *
c     ******************************************************************
c
      implicit integer (a-z)
      data ielap /0/
      rsecs=ielap
      msecs=i/5
      return
      end

