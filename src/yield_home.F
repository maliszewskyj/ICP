#include "icp.h"
C Yield_Home: wait for motor motorno to stop moving
c
	subroutine yield_home(motorno,athome)
c
	use instr
	implicit none

	integer*4	motorno !number of motor heading home
	logical		athome !true if motor on home when stopped
	real*4		data

	CALL CLREF(contr_a)
	data = 0.25
	CALL Hold (data)

	CALL CLREF(stop)
	CALL CLREF(contr_a)

	athome = .TRUE.
	return 
	end
