#include "icp.h"
c	
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C									   C
C  Subroutine for communicating with shear cell  via RS232 port            C
C									   C
C  Gayle Greene, 1998							   C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
c
	SUBROUTINE SHEAR232(WRSTRING,LWR,RDSTRING,LRD,getreply)
C
	use tc
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
     1	   lrd !length of rdstsring
	integer*4 getreply !indicates if reply expected
C
	CALL RS232TC_LK(WRSTRING,LWR,RDSTRING,LRD,getreply)
	return
	END
