#include "icp.h"
c	
	SUBROUTINE NL232(WRSTRING,LWR,RDSTRING,LRD)
C
	use iso_c_binding
	implicit none
	interface
#include "c_rs232_tc.h"
	end interface
	integer*4 mask(2) /0,8192/	!Identifies CR as terminator
C
c
c  calling parameters
	character*10 wrstring !string to be written to RS232 device
	character*35 rdstring !string returned by RS232 device
	integer*4 lwr, !length of portion used of wrstring
     1	   lrd !length of rdstsring
	
	call RS232TC_NL(WRSTRING,LWR,RDSTRING,LRD)
	return
	END
