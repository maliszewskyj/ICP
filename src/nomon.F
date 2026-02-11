#include "icp.h"
	SUBROUTINE NoMon (*)
C	*********************

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C                                                                             C
C     ROUTINE TO TELL USER THAT THERE IS NO MONITOR ON THE                    C
C     CURRENT INSTRUMENT. RETURNS CONTROL TO STATEMENT LABEL * IN THE CALLING C
C     PROGRAM                                                                 C
C     Called from: Parse_Contr                                                C
C                                                                             C
C     Gayle Greene, July, 1993                                                C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C

	CHARACTER*80  response


	response = ' There is no monitor on this instrument!'
	CALL PUTCONTR (response)
	RETURN 1
	END




