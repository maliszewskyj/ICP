#include "icpdef.h"
	MODULE motors
	logical*1   moving(MAXMOTS)
	logical*1   overshot(MAXMOTS)
	real*4      destination(MAXMOTS)
	integer*2   mcchan(MAXTYPES) !channels assigned to motor controllers
	integer*4   motor_wait_time(2)
	character*2 motorstring(MAXMOTS)
	logical	    noback !no backlash correction
	END MODULE motors
