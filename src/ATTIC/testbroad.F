#include "icp.h"
	include COMPILE_OPTS
C	**********************************************

	IMPLICIT      none
 	CHARACTER*80  info(4)

	info(1) = 'This is a test.'
	info(2) = 'Using new ip address.'
	info(3) = ' From icp type program'

	call broadcast(info(1),info(2),info(3))

	END
 
