#include "icp.h"
	SUBROUTINE Forward_700
	use smg_prep
	IMPLICIT NONE

	integer*2	lcom,lreply
	character*80	command,reply
	integer*4	j

 10	command(1:)=' '
	CALL READ_STRING (keyb, command, 
     1	     'Enter command: (or return to exit): ',
     1		80,j,j,j, lcom,j, contr)  !j is for unix

	IF (lcom .eq. 0)RETURN

	call lr700_232(command,lcom,reply,lreply)
	call PUTLINE(contr,reply(1:lreply))

	GOTO 10

	END
