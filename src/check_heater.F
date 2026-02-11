#include "icp.h"
	Subroutine check_heater
C	******************************************************

C-----------------------------------------------------------------------------C
c	Check that Lakeshore has not turned off heater power
c	  If it has, turn it back on          
C       Should only be called 
c		if (tempctr .and. (ibuf.tmp .ne. 0) .and. (ibuf.tw .ne. 0))
C-----------------------------------------------------------------------------C
	use tc
	use iso_c_binding
	IMPLICIT     none
	interface
#include "c_debugval.h"
#include "c_libwait.h"
	end interface
	REAL*4        params(6)
	CHARACTER*8   str8
	INTEGER*4	status	

	IF (temptype .le. 2) THEN !lakeshore
	CALL Rdpar (params)
	If (params(4).eq. 0.)then !heater is off
	  call libwait(.5,status)
	  if (power.eq.0)then
	    call wrmaxpower
   	    CALL PUTCONTR (
     1		    ' Heater power to temp controller reset to max')
	  else
	    CALL wrpower
	    write(unit=str8,fmt='(f8.3)')power
   	    CALL PUTCONTR (
     1		  ' Heater power to temp controller reset to '//str8)
	  endif
	ENDIF
	ENDIF !lakeshore

	RETURN
	END


