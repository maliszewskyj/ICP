#include "icp.h"
	include COMPILE_OPTS
	SUBROUTINE Goniometer_Powerup
C     	***********************************
	
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C                                                                             C
C	Called from: Parse_Contr                                              C
C                                                                             C
C	Gayle Greene, April, 1993                                             C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C

	IMPLICIT     none
	CHARACTER*80 command, reply
	INTEGER*2    lcom, lreply
	LOGICAL      rserr
	INTEGER*4    secwait


C------------------- NOW SEND OUT NUMBER OF STEPS TO MOVE --------------------C
	command(1:) = 'WM4F'
	lcom = 4
	secwait = 10
	rserr = .false.
	call rs232x(command,lcom,reply,lreply,secwait,rserr)

	RETURN
	END
