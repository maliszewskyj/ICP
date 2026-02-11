#include "icp.h"
C  Subroutine for communicating with AD201 via RS232 port

	SUBROUTINE AD232(WRSTRING,LWR,RDSTRING,LRD,getreply)
C  At call: wrstring(1:lwr) has string to send out
C           if getreply is true, then lrd has length of expected reply
C  At return:
C           if getreply is true, then lrd has length of actual reply
C	    reply(1:lreply) has the reply

	IMPLICIT NONE

C  CALLING PARAMETERS
	character*80 wrstring, !string to be written to RS232 device
     1     rdstring !string returned by RS232 device
	integer*4 lwr, !length of portion used of wrstring
     1	   lrd !length of portion of rdstring used

	LOGICAL  GETREPLY

C NOT USED!!
	return
	end

