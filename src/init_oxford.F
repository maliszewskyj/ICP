#include "icp.h"
	SUBROUTINE Init_Oxford(errflag)
C	*********************

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C                                                                             C
C     ROUTINE TO INITIALIZE OXFORD TEMPERATURE CONTROLLER          C
C     Called from: Tconfig                                                    C
C                                                                             C
C     GAYLE GREENE, June, 1998                                                C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
	use smg_prep
	use tc
	use iso_c_binding
	IMPLICIT NONE
	interface
#include "c_rs232_tc.h"
	end interface

	CHARACTER*80  command,reply
	INTEGER*4     lreply,lcom
	INTEGER*4     errflag
	CHARACTER*4   str4
	INTEGER*4     ig, nochan, j


C  put controller in remote mode
	command(1:) = 'C3'
	lcom = 2
	CALL RS232TC_OX(command,lcom,reply,lreply)
	IF (lreply .le. 0) THEN
	  call putcontr
     1		(' Warning: no reply when putting controller in remote mode')
	ELSEIF (reply(1:1) .ne. 'C')THEN
	  call putcontr
     1	   (' Warning: unexpected reply when putting controller in remote mode')
	ENDIF

C  ask user which sample chan
 25 	CALL READ_STRING(keyb, str4,
     1		'Use which sample channel (1-3): ',4,
     1		j,j,j, ig,j, contr)  !j is for unix
	IF (ig .EQ. 0)GOTO 25
	READ(unit=str4(1:ig),fmt=*,err=25)nochan
	IF (nochan .lt. 1 .or. nochan .gt. 3) GOTO 25
	excitlr = FLOAT(nochan) !memorize for rdtemp

	errflag =0
	RETURN 
	END




