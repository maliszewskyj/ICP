#include "icp.h"
	SUBROUTINE Init_LR400(errflag)
C	*********************

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C                                                                             C
C     ROUTINE TO INITIALIZE LR400 TEMPERATURE CONTROLLER                      C
C     Called from: Tconfig                                                    C
C                                                                             C
C     GAYLE GREENE, FEBRUARY, 1993                                            C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C

	CHARACTER*80  response,command,reply
	INTEGER*4     lreply,lcom
	INTEGER*4     errflag,noreply/0/


	ERRFLAG = 0
	command(1:)='@'
	lcom=1
	call lr232(command,lcom,reply,lreply,noreply)
	command(1:) = 'C2XF3X'
	lcom = 6
	call lr232(command,lcom,reply,lreply,noreply)
	command(1:)='P2XD16ZXP1XD1ZXH2X'
	lcom=18
	call lr232(command,lcom,reply,lreply,noreply)
	command(1:) = 'P1XD4ZXH2X'
	lcom = 10
	call lr232(command,lcom,reply,lreply,noreply)
	RETURN 
	END




