#include "icp.h"
c	
C  Subroutine for communicating with temp controller via RS232 port
c
	SUBROUTINE RD232(WRSTRING,LWR,RDSTRING,LRD)
C
	use iso_c_binding
	implicit none
	interface
#include "c_rs232_tc.h"
	end interface
C
c
c  calling parameters
	character*10 wrstring !string to be written to RS232 device
	character*35 rdstring !string returned by RS232 device
	integer*4 lwr, !length of portion used of wrstring
     1	   lrd !length of portion of rdstring used
C
	integer*4 rdreply

	rdreply = 0

	CALL RS232TC_RD(WRSTRING,LWR,RDSTRING,LRD)
	return
c
c
c
	END

	SUBROUTINE WR232(WRSTRING,LWR)
	use iso_c_binding
	implicit none
	interface
#include "c_rs232_tc.h"
	end interface
	character*10 wrstring !string to be written to RS232 device
	character*35 rdstring !string returned by RS232 device
	integer*4 lwr, !length of portion used of wrstring
     1	   lrd !length of portion of rdstring used
	integer*4 rdreply

	rdreply = 0

	CALL RS232TC_RD(WRSTRING,LWR,RDSTRING,LRD)
	END
