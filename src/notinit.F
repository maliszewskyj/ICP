#include "icp.h"
	SUBROUTINE NoTinit (*)
C	*********************

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C                                                                             C
C     ROUTINE TO TELL USER THAT TEMPERATURE CONFIGURATION HAS NOT BEEN        C
C     SPECIFIED. RETURNS CONTROL TO STATEMENT LABEL * IN THE CALLING          C
C     PROGRAM                                                                 C
C     Called from: Parse_Contr                                                C
C                                                                             C
C     GAYLE GREENE, FEBRUARY, 1993                                            C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C

	CHARACTER*80  response



	response = ' You must define your temperature device'//
     1	' before executing this operation!' 
	CALL PUTCONTR (response)
	response = ' Use the command TDEV.'
	CALL PUTCONTR (response)
	RETURN 1
	END




