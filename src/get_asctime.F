#include "icp.h"
	SUBROUTINE GET_ASCTIME(timestr,status)
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C                                                                             C
C	Gayle Greene, December , 1994                                         C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C
	IMPLICIT NONE

C  Calling parameters
	INTEGER*4	status
	CHARACTER*(*)	timestr

        character*24 my_date
        integer time, a
        a=time()
        my_date = ctime(a)
	timestr(1:)=my_date(9:10)//'-'//my_date(5:7)//'-'//my_date(21:24)//
     1		' '//my_date(12:19)
	status = -1

	RETURN
	END
