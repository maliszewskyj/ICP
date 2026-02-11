#include "icp.h"
	subroutine read_asdfile

	use asd
	implicit none
	integer*4  i, io
	character*30	str30
	character*80 gethome

	  OPEN (UNIT= 1, FILE= ASDPARAMS_PAR,
     1	       STATUS= 'OLD', ACCESS= 
	1     'SEQUENTIAL', FORM= 'FORMATTED', ERR= 130)
	  READ (1, FMT= *)noslices
	  do i = 1,noslices
	    READ (1, FMT= *,err=135) slices(i,1),slices(i,2),slices(i,3),slices(i,4)
	  enddo
 	  CLOSE(1)
c	  write(unit=str30,fmt=*,iostat=io)noslices
c	  call putcontr(' noslices: '//str30)
c	  write(unit=str30,fmt=*,iostat=io)slices(1,1)
c	  call putcontr(' first row: '//str30)
c	  write(unit=str30,fmt=*,iostat=io)slices(noslices,1)
c	  call putcontr(' last row: '//str30)

	  return
 130	  call putcontr(' Could not open asdparams.par')
	  return
 135	  call putcontr(' Err reading asdparams.par')
	  close(1)
	  return
	  end
