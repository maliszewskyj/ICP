#include "icp.h"
c	
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C									   C
C  Subroutine for communicating with SR LIA  via RS232 port                 C
C									   C
C  Gayle Greene, 1998							   C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
c
	SUBROUTINE SRLia232(WRSTRING,LWR,RDSTRING,LRD,getreply)
C
	use tc
	implicit none
C
c
c  calling parameters
	character*80 wrstring, !string to be written to RS232 device
     1	   rdstring !string returned by RS232 device
	integer*4 lwr, !length of portion used of wrstring
     1	   lrd !length of rdstsring
	logical getreply !indicates if reply expected
C
C  local variables for writing to RS232 device 
	character*80 outstring
	byte outbyte(80) !bytes seem to work better in call to sys$qio
	equivalence (outstring(1:1),outbyte(1))
	integer*4 lout !length of portion of outbyte containing command

C
C Turns out this routine does nothing. Who knew? - NCM 11/1/2011
C
	return
	END
