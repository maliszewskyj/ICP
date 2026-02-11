#include "icp.h"
	LOGICAL FUNCTION Timeyet(timeflag)
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C									      C
C  Timeyet will compare time in timeflag (internal format) with current time. C
C    It will return TRUE if current time is later than or equal to timeflag.  C
C                   FALSE otherwise                                           C
C									      C
C                                                                             C
C	Gayle Greene, December, 1994                                          C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C

	IMPLICIT NONE

C  Calling parameters (Only 2-d for vms compatibility) (Only (1) is used)
	INTEGER*4	timeflag(2) !time with which to compare current time

C  Local variables
	INTEGER		curtime

C  Unix Function call to get current time
	INTEGER		time

C  Local variables
	INTEGER*4	status, i
	INTEGER*2	timefl_array(7),cur_array(7)

C  First make sure timeflag contains something other than "no action" 0
	timeyet = .false.
	If (timeflag(1) .EQ. 0)THEN
	  Timeyet = .FALSE. !0 means no action scheduled so it is never time
	ELSE !Get current time and compare with timeflag
	  curtime = time()
C	  type *,' curtime in Timeyet: ',curtime
	  if (timeflag(1) .LE. curtime) timeyet=.true.
	ENDIF

C	RETURN
	END


