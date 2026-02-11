#include "icp.h"
C  Subroutine for communicating with OMEGA temp controller via RS232 port
c
	SUBROUTINE NEWOM_232(WRSTRING,LWR,RDSTRING,LRD)
	use iso_c_binding
	implicit none
	interface
#include "c_rs232_tc.h"
	end interface

	integer*4 mask(2)/0,8192/
c
c  calling parameters
	character*20 wrstring !string to be written to RS232 device
	character*35 rdstring !string returned by RS232 device
	integer*4 lwr, !length of portion used of wrstring
     1	   lrd !length of portion of rdstring used
C
C NOT USED!!!	
	return
c
c
	END
