#include "icp.h"
	subroutine verify_motors(dmottype,dpulse,problem)
c
	use instr
	implicit none
	integer*4	dmottype(MAXMOTS),dpulse(MAXMOTS)
	logical		problem

C  Local variables
	integer*4	motorno
	logical		need_reset


c  Verify that motor types and pulses/degree are consistent
	need_reset = .FALSE.
	DO motorno=1,MAXMOTS
         IF (motfl(motorno))THEN !motor is in use
	   IF (mottype(motorno) .NE. dmottype(motorno))THEN
	     print *,' Motor type of motor ',motorno,
     1		' has changed since last use. '
	     need_reset = .TRUE.
	   ENDIF
	   IF (pulse(motorno) .NE. dpulse(motorno))THEN
	     print *,' Steps/degree of motor ',motorno,
     1		' has changed since last use. '
	     need_reset = .TRUE.
	   ENDIF
	 ENDIF !motor is in use
	ENDDO !for each motors
	IF (need_reset)THEN
	  problem = .TRUE.
	  print *,' Verify Instr.Cfg is set up correctly and run Imotors!!'
	ELSE
	  problem = .FALSE.
	ENDIF !need_reset

	RETURN
	END
