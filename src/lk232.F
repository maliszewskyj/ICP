#include "icp.h"
c	
C  Subroutine for communicating with temp controller via RS232 port
c
	SUBROUTINE LK232(WRSTRING,LWR,RDSTRING,LRD,rdreply)
C
	use tc
	implicit none
	interface
#include "c_rs232_tc.h"	   
	end interface
C
c
c  calling parameters
	character*20 wrstring !string to be written to RS232 device
	character*35 rdstring !string returned by RS232 device
	integer*4 lwr, !length of portion used of wrstring
     1	   lrd !length of portion of rdstring used
	integer*4   rdreply !true if a reply to command is expected

	CALL RS232TC_LK(WRSTRING,LWR,RDSTRING,LRD,rdreply)

	RETURN
	END
