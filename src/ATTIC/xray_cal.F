#include "icp.h"

	include COMPILE_OPTS
	SUBROUTINE XRAY_CAL(motorno)
C     	***********************************
	
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C                                                                             C
C	XRAY_CAL: Axis calibration of  motor indicated.                       C
C	Called from: Parse_Contr                                              C
C                                                                             C
C	Gayle Greene, January 1993                                            C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C

	IMPLICIT     none
	INTEGER*4    motorno
	CHARACTER*80 command, reply
	INTEGER*2    lcom, lreply
	LOGICAL	     rserr
	INTEGER*4    secwait

	INCLUDE INSTR_CMN
	INCLUDE MOTORS_CMN

	IF (motorno .eq. 3) THEN !omega
	  command(1:)='VD'
	ELSE !two-theta
	  command(1:)='UD'
	ENDIF
	lcom=2
	secwait=36 !wait up to 36 loops for motor to settle
	rserr = .false.
	call rs232x(command,lcom,reply,lreply,secwait,rserr)

	RETURN
	END
