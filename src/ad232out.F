#include "icp.h"
C  Subroutine for communicating with AD201 via RS232 port

	SUBROUTINE AD232OUT(WRSTRING,LWR)

	IMPLICIT NONE

	INTEGER*4 STATUS, i, ios

C  CALLING PARAMETERS
	character*80 wrstring !string to be written to RS232 device
	integer*4 lwr !length of portion used of wrstring

C NOT USED!!
	return
	end
