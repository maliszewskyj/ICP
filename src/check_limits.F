#include "icp.h"
C	check_limits: check to see if motor is on a hardware limit
c	returns   stopped = true if motor finished moving
c		  if stopped then
c		    plus = true if on positive limit
C		    minus = true if on negative limit
c		    both are false if not on a limit
c		    home = true if on home signal, false if not
c		  endif
c		  comerr is true if cannot communicate
c
	Subroutine Check_Limits(motorno,stopped,plus,minus,home,comerr)
c
	use instr
	use motors
	use iso_c_binding
	implicit none
	interface
#include "c_rs232_acs.h"
#include "c_vme.h"
	end interface
	integer*4 motorno
	logical stopped,plus,minus,home,comerr
	integer*4 istopped, iplus,iminus,ihome

	integer*4 io_stat,state, ios
	integer*4 ipos,    istat
	integer*4 status
	character*80 command,reply
	character*2	str2
	integer*4 lcom,lreply
	logical	  bad_reply

	plus = .FALSE.  !set defaults
	minus = .FALSE.
	home = .FALSE.
	stopped = .TRUE.
	comerr = .FALSE.

C  Send out command to check limits
	lreply=0		!no answer yet
	reply(1:)=' '		!no valid reply yet
	IF (mottype(motorno).EQ. ACS)THEN !focusing on NG5
	   command(1:3)=motorstring(motorno)//'E'
	   lcom=3	
	   call rs232(command,lcom,reply,lreply) !repeat once
	   bad_reply = lreply.EQ.0 .OR. reply(4:4).NE.'='
	ELSEIF (mottype(motorno).EQ. VME)THEN
	   io_stat = VMESTATUS(motorno,istat,status)
	   if (io_stat.lt.0) then
	      comerr=.true.
	      return
	   endif
! istat - moving
! status - limit status: +1 positive, -1 negative
	   if (istat.gt.0) stopped=.false.
	   if (status.ne.0) then
	      if (mod(status,2).eq.1) then
		 minus=.true.
		 status = status - 1
	      endif
	      if (status.ge.2) then
		 plus=.true.
	      endif
	   endif

	   return
	ELSE			!not implemented
	   CALL Putcontr_NT(' Limit check not implemented for this controller')
	   comerr = .TRUE.
	   return
	ENDIF			!choice on motortype
	if (bad_reply)then	!no good reply after 10 tries
	   comerr = .TRUE.
	   return
	endif
c       Have a valid reply so check it out
	IF (mottype(motorno).EQ. ACS)THEN !focusing on NG5
	   if (reply(9:9).eq.'0')then !motor is no longer moving
	      if (reply(5:5).eq.'-') minus = .TRUE. !on limit!
	      if (reply(7:7).eq.'+') plus =  .TRUE. !on limit!
	      if (reply(6:6).eq.'H') home =  .TRUE. !on home signal
	   ELSE			!still moving
	      stopped = .false.
	   ENDIF		!still moving
	ENDIF			!choice on motor type

	RETURN
	END
