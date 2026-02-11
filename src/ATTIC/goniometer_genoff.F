#include "icp.h"
	include COMPILE_OPTS
	SUBROUTINE Goniometer_GenOff
C     	***********************************
	
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C                                                                             C
C	Called from: Parse_Contr                                              C
C                                                                             C
C	Gayle Greene, Dec, 1999                                             C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C

	IMPLICIT     none
	CHARACTER*80 command, reply
	INTEGER*2    lcom, lreply
	LOGICAL      rserr
	INTEGER*4    secwait


	command(1:) = 'WM-21F'
	lcom = 6
	secwait = 10
	rserr = .false.
	call rs232x(command,lcom,reply,lreply,secwait,rserr)

	RETURN
	END
