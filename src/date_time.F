#include "icp.h"
	SUBROUTINE Date_Time (tstring)
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C	Dummy routine which interfaces with DEC LIB$DATE_STRING		      C
C                                                                             C
C	Gayle Greene, April    , 1998                                         C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C
	IMPLICIT NONE

C  Calling parameters
	CHARACTER*28	tstring


        character*24 my_date
        integer time, a
        a=time()
c        write(*,*) ' numerical time: ',a
        my_date = ctime(a)
c       write(*,*) ' character time: ',my_date
	tstring(1:)=my_date(5:10)//my_date(20:24)//my_date(11:19)

 	RETURN
	END
