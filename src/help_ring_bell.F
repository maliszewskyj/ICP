#include "icp.h"
	SUBROUTINE help_Ring_Bell(numb)
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C	Dummy routine which interfaces with DEC SMG$RING_BELL                 C
C                 For help screen                                            C
C	Gayle Greene, March , 1995                                            C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C
	IMPLICIT NONE

C  Calling parameters
	INTEGER*4	NUMB    !Number of times

	write(*,fmt='(a)') char(7)
	RETURN
	END
