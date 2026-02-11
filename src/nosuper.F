#include "icp.h"
	SUBROUTINE NoSuper (*)
C	*********************

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C                                                                             C
C     ROUTINE TO TELL USER THAT DEMANDED OPERATION IS NOT ALLOWED UNLESS IN    C
C     SUPER MODE.   RETURNS CONTROL TO STATEMENT LABEL * IN THE CALLING   C
C     PROGRAM                                                                 C
C     Called from: Parse_Contr                                                C
C                                                                             C
C     Gayle Greene, Dec  1997                                                 C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C

	CHARACTER*80  response



	response = ' Super Privileges required to execute this command.'
	CALL PUTCONTR_NT (response)
	RETURN 1
	END




