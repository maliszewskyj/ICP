#include "icp.h"
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C									   C
C  Subroutine for communicating with OXFORD via RS232 port                 C
C									   C
C  Gayle Greene, June, 1998 						   C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
c
	SUBROUTINE OX232(WRSTRING,LWR,RDSTRING,LRD)
C       
	use iso_c_binding
	implicit none
	interface
#include "c_rs232_hc.h"
	end interface

c
c  calling parameters
	character*80 wrstring, !string to be written to RS232 device
     1	   rdstring !string returned by RS232 device
	integer*4 lwr, !length of portion used of wrstring
     1	   lrd !length of rdstsring
C

	call rs232_ox(WRSTRING,LWR,RDSTRING,LRD)
	return
	END
