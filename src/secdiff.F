#include "icp.h"
	real function SecDiff()
	use instr
	implicit none



C	dsec = stopsecs - startsecs
	REAL*4	      dsec

	dsec = duration
	if (dsec .le. 0.) dsec = 1.0

	SecDiff = dsec

	return
	end

