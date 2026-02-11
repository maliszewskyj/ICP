#include "icp.h"
      subroutine go_limit(motorno, go_negative, comerr)
C
C     Check_Limits: Return status of limit switches for designated axis
C
C     Version = $Id$
C 
	use instr
	use motors
	use iso_c_binding
	implicit none
	interface
#include "c_rs232_acs.h"
#include "c_vme.h"
	end interface
	integer*4 motorno
	logical go_negative, comerr
	character*80 command, reply
	integer*4 lcom, lreply
	


	lreply = 0
	reply(1:) = ' '
	if (mottype(motorno) .eq. ACS) then
	   command(1:4) = motorstring(motorno)//'L+'
	   if (go_negative) then
	      command(4:4) = '-'
	   endif            
	   lcom = 4
	   call rs232(command,lcom,reply,lreply)
	   moving(motorno) = .true.
	else if (mottype(motorno) .eq. VME) then
	   lreply = 1
	   if (go_negative) lreply = 0
	   call vmegolim(motorno, lreply)
	   moving(motorno) = .true.
	else
	   call putcontr_nt(' Limit check not implemented for this controller')
	   comerr = .true.
	   return
	endif

	end
