#include "icp.h"
c	
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C									   C
C  Subroutine for communicating with TDBoard via RS232 port                C
C  									   C
C  Gayle Greene, 1997							   C
C  Routine officially does nothing - NCM 11/1/2011                         C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
c
	SUBROUTINE tdb232(WRSTRING,LWR,RDSTRING,LRD)
C
	use tc
	implicit none
C
c
c  calling parameters
	character*80 wrstring, !string to be written to RS232 device
     1	   rdstring !string returned by RS232 device
	integer*2 lwr, !length of portion used of wrstring
     1	   lrd !length of rdstsring
C
C       REMOVED REFERENCES TO VMS 6/24/2010
C
C
	return
	END
