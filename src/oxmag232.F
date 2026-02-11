#include "icp.h"
C  Subroutine for communicating with Oxford magnet controller via RS232 port
C  Note: command C3 is sent out before each command to ensure local mode
	SUBROUTINE OXMAG232(WRSTRING,LWR,RDSTRING,LRD,getreply)
	use iso_c_binding
	implicit none
	interface
#include "c_rs232_hc.h"
	end interface
	integer*4 getreply
	INTEGER*4 STATUS,MASK(2)/0,8192/

C  CALLING PARAMETERS
	character*80 wrstring, !string to be written to RS232 device
     1     rdstring !string returned by RS232 device
	integer*4 lwr, !length of portion used of wrstring
     1	   lrd !length of portion of rdstring used
C  LOCAL VARIABLES FOR WRITING TO RS232 DEVICE 
	character*80 outstring
	integer*4 lout !length of portion of outbyte containing command

	outstring(1:)='C3'
	lout = 2
	CALL RS232_OXMAG(outstring,lout,rdstring,lrd,getreply)	
	CALL RS232_OXMAG(WRSTRING,LWR,RDSTRING,LRD,getreply)
	return
	end
