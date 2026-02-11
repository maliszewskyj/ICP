#include "icp.h"
      subroutine rd_trj(filename, status)

      use trj
      character*12 filename
      integer status
      integer lfile
      integer pts

      OPEN (UNIT= 36, FILE= filename, STATUS= 'OLD',
     $     FORM= 'FORMATTED', ERR= 100)
      READ (UNIT=36, fmt='(a)',ERR=110) comments
      READ (UNIT=36, fmt=*,ERR=110) npts
      READ (UNIT=36, fmt=*,ERR=110) nmots
      READ (UNIT=36, fmt=*,ERR=110) mpf, mon, mt
      READ (UNIT=36, fmt=*,ERR=110) (axes(i),i=1,nmots)
      pts = 0
      DO j=1,npts
         READ(UNIT=36, fmt=*,ERR=120) (angles(i,j),i=1,nmots)
         pts = pts + 1
      ENDDO


      CLOSE (UNIT=36)

C      print *,'MPF = ',mpf,' MON = ',mon,' MT = ',mt
C      print *,'A1=',axes(1),' A2=',axes(2),' A3=',axes(3),
C     $     ' A4=',axes(4),' A5=',axes(5),' A6=',axes(6)
      status = 1
      return
 100  print *, 'Error opening specified run file'
      status = 0
      return
 110  print *, 'Error parsing run file'
      status = 0
      return
 120  print *, 'Parse error: truncating scan to ',pts, ' points'
      npts = pts
      status = 0
      return
      end
