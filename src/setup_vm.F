#include "icp.h"
	SUBROUTINE Setup_VM
C	*******************

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C	                                                                      C
C	Routine to initial virtual motor variables and lists		      C
C	                                                                      C
C	Gayle Greene: August, 1996					      C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
	use instr
	use motors
	use virtmot
	IMPLICIT     none

	integer*4 motorno, i
	logical   using_virt

	do i = 1, MAXMOTS !set all tables to default values
	  numreal(i) = 0
	  virtpos(i) = -999.99
	  pntlist(i) = 0
	  motlist(i) = 0
	  poslist(i) = -999.99
	enddo

	using_virt = .FALSE. !not using virtual motors by default
	do motorno=1,MAXMOTS
          IF (motfl(motorno) .AND. mottype(motorno) .EQ. VIRT)THEN
	 	using_virt = .TRUE.
	  ENDIF
	enddo !for each motor

	IF (using_virt)THEN
	  IF (nsta .EQ. 8)THEN
	    CALL VINIT_BT8
          ELSE IF (nsta .EQ. 9) THEN
	    CALL VINIT_BT9
	  ENDIF
	ENDIF

	RETURN
	END
