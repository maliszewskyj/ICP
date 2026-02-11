#include "icp.h"
c	
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C									   C
C  Subroutine for communicating with LR700  via RS232 port                 C
C									   C
C  Gayle Greene, 1995							   C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
c
	SUBROUTINE LR700_232(WRSTRING,LWR,RDSTRING,LRD)
	use iso_c_binding
	implicit none
	interface
#include "c_rs232_tc.h"
	end interface

c  calling parameters
	character*80 wrstring, !string to be written to RS232 device
     1	   rdstring !string returned by RS232 device
	integer*4 lwr, !length of portion used of wrstring
     1	   lrd !length of rdstsring
C
	CALL RS232TC_LR700(WRSTRING,LWR,RDSTRING,LRD)

	return
	END
