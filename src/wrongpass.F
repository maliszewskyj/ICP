#include "icp.h"
	SUBROUTINE WrongPass (*)
C	*********************

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C                                                                             C
C     ROUTINE TO TELL USER THAT DEMANDED OPERATION WILL NOT BE EXECUTED.      C
C     WRONG PASSWORD. RETURNS CONTROL TO STATEMENT LABEL * IN THE CALLING     C
C     PROGRAM                                                                 C
C     Called from: Parse_Contr                                                C
C                                                                             C
C     Gayle Greene, Oct. 1996                                                 C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C

	CHARACTER*80  response

	response = ' Requested action cannot be taken without proper password!!'
	CALL PUTCONTR (response)
	RETURN 1
	END




