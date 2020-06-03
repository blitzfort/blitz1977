      subroutine read
c
c     ******************************************************************
c     *                                                                *
c     *      read is the only subroutine in the entire chess program   *
c     *  that does i/o reads to the user terminal/console/whatever.    *
c     *  any record required by the program is read through this       *
c     *  routine.  other subroutines perform conversion from text to   *
c     *  to integer, etc.  to operate blitz in batch mode, this rou-   *
c     *  tine could be modified to accept input from the operator      *
c     *  console or from a disk data file as necessary.                *
c     *                                                                *
c     ******************************************************************
c
      implicit integer (a-z)
      common /buffer/ text(80)
c
c------------------------------< read in the string to process.
c
      read(5,200,end=999)text
200   format(80a1)
999   continue
      return
      end



