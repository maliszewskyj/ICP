#include "icp.h"
	SUBROUTINE PICODrive(motorno,targetangle)
C       *************************************

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C                                                                             C
C       Drive: Move Motor Specified To Physical Angle Indicated               C
C       Called from: Drive                                                    C
C                                                                             C
C       Nick Maliszewskyj          January 2000                               C
C       Version = $Id$                                                        C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C

	use instr
	use motors
	use iso_c_binding
	IMPLICIT     none
	interface
#include "c_debugval.h"
#include "c_pico.h"
	end interface
	INTEGER*2    notries
	INTEGER*4    motorno
	REAL*4       targetangle,oldpos, newpos, change,m_err,m_back,rdangl
	CHARACTER*10 stepstring
	LOGICAL*1    retry
	INTEGER*4    ichange
	INTEGER*4    retn, dbg

	CALL DEBUGVAL(dbg)


	retry = .true.
	if (mottype(motorno) .ne. PICO) then
	   write(stepstring,fmt='(i2)') motorno
	   call putcontr('PICODRIVE: motor '//stepstring(1:2)//
	1	'is not a PICO motor')
	   return
	endif

	m_err  = merr(motorno)
	m_back = back(motorno)
C       
C       Get current motor position
C       
	oldpos = rdangl(motorno);
C       
C       Determine distance and direction of move
C       
	newpos = targetangle 
	change = newpos - oldpos
	ichange = change * pulse(motorno)

	if (ABS(change).le.m_err) THEN
	   if (dbg .ge. 1) call putcontr('Position within error, not moving')
	   return		! return if in position
	endif
	moving(motorno) = .true.
	destination(motorno) = targetangle

				! work out backlash
	if (change .lt. 0 .and. m_back .gt. 0) then
	   newpos = targetangle - m_back
	   change = newpos - oldpos
	   ichange = change * pulse(motorno)
	   overshot(motorno) = .true. ! set flag to indicate intermediate
				! target
	else 
	   overshot(motorno) = .false.
	endif      
C       
C       Tell controller to move to target position
C       
	call memcurpos(motorno,1,newpos)
	retn = PICOGOREL(motorno,ichange)

	RETURN
	END
