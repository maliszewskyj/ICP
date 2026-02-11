#include "icp.h"
c	
C  Subroutine for communicating with OMEGA temp controller via RS232 port
c
	SUBROUTINE OM232(WRSTRING,LWR,RDSTRING,LRD)
C
	use tc
	use iso_c_binding
	implicit none
	interface
#include "c_rs232_tc.h"	   
	end interface
C
	integer*4 mask(2)/0,8192/
c
c  calling parameters
	character*20 wrstring !string to be written to RS232 device
	character*35 rdstring !string returned by RS232 device
	integer*4 lwr, !length of portion used of wrstring
     1	   lrd !length of portion of rdstring used
C
	call RS232TC_OM(WRSTRING,LWR,RDSTRING,LRD)
	return
c
c
	END
