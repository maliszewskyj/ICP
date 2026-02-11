C     Logfile utility

      subroutine logfileinit

      integer*4 ios

      open(unit=15, file='fndlam.log',form='formatted',
     $     access='append',status='unknown',iostat=ios)

      end

      subroutine logfileclose
      close(unit=15)
      end

      subroutine putlog(string)
      character*(*) string

      write(unit=15,fmt='(a)') string
      write(*,fmt='(a)') string
      end

      
