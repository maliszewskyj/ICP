#include "icp.h"
	SUBROUTINE Priv_Bypass (c)
C	**************************

C
C	this is an undocumented feature
C
C	Beat Schmid,  November 1989


	logical       c
	character*132 line
	integer*2     length              ! local length of command line
	integer       i

C	call lib$get_foreign (line,, length)
C	if (length .ne. 0) then
C	   if (line(1:length) .eq. 'PRIV') c = .true.
C	end if 

	c = .false.
	end
