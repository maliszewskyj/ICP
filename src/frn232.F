#include "icp.h"
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C									   C
C  Subroutine for communicating with furnace controller via RS232 port     C
C									   C
C  Gayle Greene, 1997							   C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
c
	SUBROUTINE FRN232(WRSTRING,LWR,RDSTRING,LRD,TCHAR,CKSUM)
	use tc
	use iso_c_binding
	implicit none
	interface
#include "c_rs232_tc.h"
	end interface

c
c  calling parameters
	character*80 wrstring, !string to be written to RS232 device
     1	   rdstring !string returned by RS232 device
	integer*4 lwr, !length of portion used of wrstring
     1	   lrd, !length of rdstsring
     1	   tchar !terminating character in read reply
	integer*4 cksum  !true if check sum to be added to string
C

	CALL RS232TC_FRN(WRSTRING,LWR,RDSTRING,LRD,TCHAR,CKSUM)
	return
	END
