#include "icp.h"
C  Subroutine for communicating with AD201 via RS232 port
C  If there was anything in buffer, lhld =1 and hldchar contains
C   last character read

	SUBROUTINE AD232empty(hldchar,lhld)

	IMPLICIT NONE

	character*1 rdchar,hldchar !char returned by RS232 device
	integer*4 lrd,lhld !number of chars returned (0 or 1)

	call ad232in(rdchar,lrd)
	if (lrd .eq. 0)THEN !buffer was empty
	  lhld = 0
	  return
	endif
	lhld = 1
	do while (lrd .ne. 0)
	  hldchar = rdchar
	  call ad232in(rdchar,lrd)
	enddo

	return
	end
