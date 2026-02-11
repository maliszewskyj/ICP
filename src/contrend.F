#include "icp.h"
	SUBROUTINE ContrEnd

	implicit none
	character*80 command
	integer retn, system		

	command(1:) = 'stty sane'
	retn = system(command)


	RETURN
	END      

