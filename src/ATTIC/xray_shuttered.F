#include "icp.h"
	include COMPILE_OPTS
	SUBROUTINE XRAY_SHUTTERED(invar)
C     	***********************************
	
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C                                                                             C
C	XRAY_SHUTTER: If invar is true, close it. Else open it.             C
C	Called from: Parse_Contr                                              C
C                                                                             C
C	Gayle Greene, January 1993                                            C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C

	IMPLICIT     none
	CHARACTER*80 command, reply
	INTEGER*2    lcom, lreply
	BYTE	      byte
	LOGICAL	     invar
	LOGICAL	     rserr
	INTEGER*4    secwait

	INCLUDE INSTR_CMN
	INCLUDE MOTORS_CMN

	IF (invar) THEN !close it
	  command(1:)='WM-1F'
	  lcom=5
	  byte=0
	ELSE !open it
	  command(1:)='WM1F'
	  lcom=4
	  byte=1
	ENDIF

	call AMDIO_OUT(byte)


	RETURN
	END
