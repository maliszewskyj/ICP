#include "icp.h"
	SUBROUTINE Sched_Action(timeflag,deltatime)
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C									      C
C  Sched_Action: add deltatime to current time to calculate time after which  C
C    something should happen. Store resultant time in timeflag.               C
C                                                                             C
C	Gayle Greene, December, 1994                                          C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C

	IMPLICIT NONE

C  Calling parameters 
c   these are 2-d only for vms compatibility
c    (Only (1) is used in each of following under linux)
	INTEGER*4	timeflag(2), !at return, will contain time to take action
     1			deltatime(2) !delta time in seconds

C  Local variables
	INTEGER*4	status, curtime(2)


C  Function call to get time (in seconds since 1970)
	INTEGER		time

C  Get current time (in seconds since 1970)
	curtime(1) = time()
c	type *,' current time in sched_action: ',curtime(1)
	IF (deltatime(1) .EQ. 0)THEN 
	  timeflag(1) = curtime(1)
	ELSE !Add delta time to current time
	  timeflag(1) = curtime(1) + deltatime(1)
	ENDIF

	RETURN
	END


