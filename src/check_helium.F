#include "icp.h"
	Subroutine Check_Helium(alarm,hlevel,comerr,shutdone)
	use instr
	use tc
	Implicit NONE

C  Calling parameters
	logical		alarm, !true if helium alarm detected
     1			shutdone, !true if magnet shutdown & magnctr set to false
     1			comerr    !true if err communicating with magnet
	real*4		hlevel

C  local variables
	logical		triggered
	character*80	response

	IF (rmag1 .EQ. 1.)THEN !flag saying helium should be checked
	  hlevel = -999. !default in case it can't be or isn't read
	  alarm = .false.
	  CALL Check_Alarm(triggered,comerr)
	  IF (.NOT. comerr)THEN
	    IF (triggered) THEN
	      Call shut_mag
	      magnctr = .FALSE.
	      alarm = .TRUE.
	      shutdone = .TRUE.
	      response(1:)='Magnet shutdown because Helium Level alarm set'
	      CALL Putcontr(response)
	      RETURN
	    ENDIF
	  ENDIF !not comerr

	  CALL Read_Helium(hlevel,comerr)
	  IF (.not. comerr)THEN
	    IF (hlevel .EQ. 0.) THEN
	      Call shut_mag
	      shutdone = .TRUE.
	      magnctr = .FALSE.
	      response(1:)='Magnet shutdown because Helium Level is zero'
	      CALL Putcontr(response)
	      RETURN
	    ENDIF
	  ENDIF

	ELSE !helium censor isn't working. don't check helium
	  hlevel = 999.
	  alarm = .false.
	  shutdone = .FALSE.
	  comerr = .FALSE.
	ENDIF !helium should be checked

	END
