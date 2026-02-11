#include "icp.h"
	SUBROUTINE IcpAbort (command, *)
C	*****************************

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C                                                                             C
C     Routine To Tell User That Demanded Operation Was Aborted By Cntrl_A.    C
C     Returns Control To Statement Label * In The Calling Program             C
C     Called from: Parse_Contr                                                C
C                                                                             C
C     Beat Schmid,  July 1989                                                 C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
	use instr
	use smg_prep
	IMPLICIT      none
	INTEGER*4     i, ig, idum
	CHARACTER*80  response
	CHARACTER*50  command
	CHARACTER*33  spcommand/'RUN $DISK1:[BT8.ALERT]abort_alert'/
	CHARACTER*3   inp
	LOGICAL	      Readef !function

	IF (.NOT. READEF(Hdw_event))THEN !not remote abort
	  response = '  Request aborted by Contrl_A'
	  CALL PUTLINE (contr, response)
	  i = INDEX (command, ';')
	  IF (nsta .NE. 8)THEN
	    IF (i .NE. 0) THEN
	     CALL READ_STRING (keyb, inp, 
     1		'* Abort entire command line [NO]? ', 3,
     1		idum,idum,idum,ig,idum, contr) !idum = dummy param for absoft
	     IF (inp(1:1) .EQ. 'Y') command (1:) = ' '
	    END IF
	  ELSE !bt8
	    IF (i .NE. 0) command (1:) = ' '
	  ENDIF
	ELSE !remote abort
	  response = '  Request aborted by Remote Request'
	  CALL PUTLINE (contr, response)
	  i = INDEX (command, ';')
	ENDIF !remote abort
	CALL PUTLINE_NF(contr,' ')
	CALL Status_Line
	RETURN 1
	END

