#include "icp.h"
	LOGICAL FUNCTION LIMPROB(motorno)
C     	***********************************
	
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C                                                                             C
C  LIMPROB returns TRUE if
C    a) cannot communicate with motor or format error in motor reply
C    b) motor is on a limit
C  LIMPROB returns FALSE otherwise
C                                                                             C
C	Gayle Greene, January 1998                                            C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C

	IMPLICIT     none

	integer*4  motorno, ios
	logical	   stopped, plus, minus, home, comerr
	character*2  str2

	Call Check_Limits(motorno,stopped,plus,minus,home,comerr)
	IF (comerr .or. .not. stopped)THEN
	  write(unit=str2,fmt='(i2)',iostat=ios)motorno
	  CALL Putcontr_NT(' Error verifying motor '//str2//' did not hit limit')
	  limprob = .TRUE.
	ELSEIF (plus .or. minus)THEN
	  write(unit=str2,fmt='(i2)',iostat=ios)motorno
	  CALL Putcontr_NT(' Err: motor '//str2//' hit limit')
	  limprob = .TRUE.
	ELSE
	  limprob = .FALSE. !default
	ENDIF

	RETURN
	END
