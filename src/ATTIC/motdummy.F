#include "icp.h"
	include COMPILE_OPTS

	SUBROUTINE Drive(motorno,targetangle)
	IMPLICIT     none
	INTEGER*4    motorno
	REAL*4       targetangle
	LOGICAL*1    slowflag
	INCLUDE INSTR_CMN
	INCLUDE MOTORS_CMN
	destination(motorno)=targetangle
	RETURN
	END

	subroutine yield
	implicit none
	return
	end

	SUBROUTINE HOME(motorno,dirc)
	IMPLICIT     none
	INTEGER*4    motorno
	CHARACTER*1  dirc
	RETURN
	END

	FUNCTION RdAngl(motorno)
	IMPLICIT     none
	REAL*4       RdAngl
	INTEGER*4    motorno
	INCLUDE INSTR_CMN
	INCLUDE MOTORS_CMN
	RdAngl = destination(motorno)
	return
	END

	SUBROUTINE Iang(motorno,currentpos)
	IMPLICIT     none
	INTEGER*4    motorno
	REAL*4       currentpos
	INCLUDE INSTR_CMN
	INCLUDE MOTORS_CMN
	destination(motorno)=currentpos
	RETURN
	END

	subroutine MOTPAR(motorno)
	implicit none
	integer*4 motorno
	return
	end

	subroutine yield_home(motorno,athome)
	implicit none
	integer*4	motorno !number of motor heading home
	logical		athome !true if motor on home when stopped
	return 
	end

	Subroutine yield_or_abort(motorno,errflag)
	IMPLICIT     none
	integer*4	motorno !number of motor that is moving
	logical		errflag !true at return if error
	errflag = .false.
	RETURN
	end

	SUBROUTINE Yield_Auxiliary
	RETURN
	END

      	SUBROUTINE Drive_Auxiliary (mot_no, target) 
      	IMPLICIT     none
      	INTEGER*4    mot_no
      	REAL*4       target
	RETURN
      	END

	SUBROUTINE Init_Auxiliary (mot_no, aux)
	IMPLICIT     none
	INTEGER*4    mot_no, aux
	return
	END

	SUBROUTINE RS232_INIT
	RETURN
	END

	SUBROUTINE RS232(WRSTRING,LWR,RDSTRING,LRD)
	implicit none
	character*80 wrstring, !string to be written to RS232 device
     1	   rdstring !string returned by RS232 device
	integer*2 lwr, !length of portion used of wrstring
     1	   lrd !length of rdstsring
	lrd=0
	return
	END

	SUBROUTINE RS232end 
	IMPLICIT NONE
	RETURN
	END

	SUBROUTINE RS232X(WRSTRING,LWR,RDSTRING,LRD,SECWAIT,REPLY)
	implicit none
	integer*4 SECWAIT
	LOGICAL Reply !true at call if reply needed
	character*80 wrstring, !string to be written to RS232 device
     1	   rdstring !string returned by RS232 device
	integer*2 lwr, !length of portion used of wrstring
     1	   lrd !length of rdstsring
	lrd = 0
	RETURN
	END

	SUBROUTINE TrashDrive(targetangle)
	IMPLICIT none
	INTEGER*4    motorno, oldpos,newpos,ichange
	INTEGER*4    amotor
	REAL*4       targetangle, currentangle, atargetangle, anglecushion
	REAL*4	rdangl
	logical      dobacklash, gopositive
	INCLUDE INSTR_CMN
	INCLUDE MOTORS_CMN
	motorno = 2               ! Move monochromator drum
	amotor  = 9               ! Move analyzer cart
	oldpos = rdangl(motorno)	
	newpos = NINT(targetangle * pulse(motorno))
	ichange = newpos - oldpos
	IF (ABS(ichange) .LE. merr(motorno)) RETURN
	gopositive = (ichange .gt. 0)
	dobacklash = (((ichange .LT. 0) .AND. (back(motorno) .GT. 0))
     1     .OR. ((ichange .GT. 0) .AND. (back(motorno) .LT. 0)))
	if (gopositive .and. dobacklash) then
         anglecushion = (ABS(back(motorno)) * 1.1) / pulse(motorno)
	else
         anglecushion = (ABS(back(motorno)) * 0.1) / pulse(motorno)
	endif
	atargetangle = targetangle + anglecushion
	destination(motorno) = targetangle
	destination(amotor)  = atargetangle
	RETURN
	END

	subroutine verify_motors(dmottype,dpulse,problem)
	implicit none
	INCLUDE INSTR_CMN
	integer*4	dmottype(maxmots),dpulse(maxmots)
	logical		problem
	RETURN
	END

	subroutine go_limit(motorno, go_negative, comerr)
	implicit none
	integer*4 motorno
	logical go_negative, comerr
	comerr=.false.
	return
	end

	subroutine rs232xend
	implicit none
	return
	end

	subroutine rs232_xinit
	implicit none
	return
	end

	subroutine rs232p_init
	implicit none
	return
	end

	subroutine udpinit
	implicit none
	return
	end

	SUBROUTINE RS232F(WRSTRING,LWR,RDSTRING,LRD)
	implicit none
	character*80 wrstring, !string to be written to RS232 device
     1	   rdstring !string returned by RS232 device
	integer*2 lwr, !length of portion used of wrstring
     1	   lrd !length of rdstsring
	return
	END

	SUBROUTINE RS232PC(WRSTRING,LWR,RDSTRING,LRD)
	implicit none
	character*80 wrstring, !string to be written to RS232 device
     1	   rdstring !string returned by RS232 device
	integer*2 lwr, !length of portion used of wrstring
     1	   lrd !length of rdstsring
	return
	END

	subroutine debuglevel(i)
	integer*4 i
	return
	end
