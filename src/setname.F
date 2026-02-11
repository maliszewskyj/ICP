#include "icp.h"
	Subroutine Setname
C	*******************

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C
C  Setname sets the process name to BTx_control.
C   Only called if priv
C	Gayle Greene: July, 1998                                              C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
	use instr
	IMPLICIT     none
	CHARACTER*11 newname/'BT0_Control'/

	IF (nsta .gt. 0) THEN
	     newname(1:2)='BT'
	ELSEIF (nsta .lt. 0) THEN
	     newname(1:2)='NG'
	ELSE !nsta = 0
	     newname(1:2)='XR'
	ENDIF
	WRITE(unit=newname(3:3),fmt='(I1)') abs(nsta)
C NCM - Not sure if this actually does anything useful now
	RETURN
	END
