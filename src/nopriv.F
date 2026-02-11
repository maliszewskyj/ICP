#include "icp.h"
	SUBROUTINE NoPriv (*)
C	*********************

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C                                                                             C
C     ROUTINE TO TELL USER THAT DEMANDED OPERATION IS NOT ALLOWED FROM THE    C
C     CURRENT TERMINAL. RETURNS CONTROL TO STATEMENT LABEL * IN THE CALLING   C
C     PROGRAM                                                                 C
C     Called from: Parse_Contr                                                C
C                                                                             C
C     Beat Schmid,  July 1989                                                 C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C

	CHARACTER*80  response



	response = ' On the current terminal you have no privilege'//
     1	  ' for the attempted operation!!'
	CALL PUTCONTR (response)
	RETURN 1
	END




