#include "icp.h"
	SUBROUTINE Contr_Ring_Bell(numb)
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C	Dummy routine which interfaces with DEC SMG$RING_BELL                 C
C                 For CONTR screen                                            C
C	Gayle Greene, March , 1995                                            C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C
	use iso_c_binding
	IMPLICIT NONE
	interface
#include "c_libwait.h"
	end interface

C  Calling parameters
	INTEGER*4	NUMB    !Number of times

	INTEGER*4	I, STATUS
	DO I=1,NUMB
	WRITE(*,fmt='(a)') CHAR(7)
	CALL LIBWAIT(0.5,STATUS)
	ENDDO

	RETURN
	END
