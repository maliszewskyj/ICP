#include "icp.h"
	SUBROUTINE Cluster64
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C	Dummy routine which interfaces with DEC SYS$ASCEFC for cluster #64    C
C                                                                             C
C	Gayle Greene, April 1998                                              C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C
	use eventflag
	IMPLICIT NONE

	INTEGER*4	i
C  init all event flags to not set
	do i = 1,90
	  evflags(i) = .false.
	enddo

	RETURN
	END
