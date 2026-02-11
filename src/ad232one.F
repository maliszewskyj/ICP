#include "icp.h"
C  Subroutine for communicating with AD201 via RS232 port
	SUBROUTINE AD232ONE(WRCHAR,RDCHAR,LRD)

	IMPLICIT NONE

C  CALLING PARAMETERS
	character*1 wrchar, !string to be written to RS232 device
     1     rdchar !string returned by RS232 device
	integer*4 lrd !length of portion of rdstring used

	INTEGER*4 STATUS, i, ios

C NOT USED!!
	return
	end
