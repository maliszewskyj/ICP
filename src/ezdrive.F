#include "icp.h"
	SUBROUTINE EZDrive(motorno,targetangle)
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C       C
C       Drive: Move Motor Specified To Physical Angle Indicated                 C
C       Note: Entry Drives no longer moves motor slowly. All drive is same speedC
C       Called from: Parse_Contr, Run_Q_Buf, Run_I_Buf, Run_T_Buf, Find_Peak    C
C       C
C       Nick Maliszewskyj          January 2000                                 C
C       Version = $Id$    C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C

	use instr
	use motors
	use iso_c_binding
	IMPLICIT     none
	interface
#include "c_debugval.h"
#include "c_ez.h"
	end interface
	INTEGER*2    notries
	INTEGER*4    motorno
	REAL*4       targetangle,oldpos, newpos, change,m_err,m_back
	CHARACTER*10 stepstring
	LOGICAL*1    retry
	INTEGER*4    retn, dbg

	CALL DEBUGVAL(dbg)

	retry = .true.
	if (mottype(motorno) .ne. EZ) then
	   write(stepstring,fmt='(i2)') motorno
	   call putcontr('EZDRIVE: motor '//stepstring(1:2)//
	1	'is not a EZ motor')
	   return
	endif

	m_err  = merr(motorno)
	m_back = back(motorno)
C       print *, 'EZDRIVE: move ',motorno,' to ',targetangle

C       
C       Get current motor position
C       
	notries = 10
	retn = -1
	do while ((notries .gt. 0) .and. (retn .lt. 0)) 
	   retn = EZGETPOS(motorno,oldpos)
	   if (retn .lt. 0) call sleep(1)
	   notries = notries - 1
	enddo
	if (retn .lt. 0) then
	   call putcontr('EZDRIVE: could not determine position of motor')
	   return
	endif        
C       
C       Determine distance and direction of move
C       
	newpos = targetangle * pulse(motorno)
	change = newpos - oldpos
	if (dbg .ge. 1) THEN
	   print *,'EZDRIVE: m_err = ',m_err
	   print *,'EZDRIVE: m_back = ',m_back
	   print *,'EZDRIVE: oldpos = ',oldpos
	   print *,'EZDRIVE: newpos = ',newpos
	   print *,'EZDRIVE: change = ',change
	endif
	if (ABS(change).le.m_err) THEN
	   if (dbg .ge. 1) call putcontr('Position within error, not moving')
	   return		! return if in position
	endif
	moving(motorno) = .true.
	destination(motorno) = targetangle

! work out backlash
	if (change .lt. 0) then
	   newpos = newpos - m_back
	   targetangle = newpos / pulse(motorno)
	   overshot(motorno) = .true. ! set flag to indicate intermediate
! target
	else 
	   overshot(motorno) = .false.
	endif      

C       
C       Tell controller to move to target position
C       
	if(trackmoves)call memcurpos(motorno,1,targetangle)
	retn = EZGOPOS(motorno,newpos)

! Ask controller whether we're actually moving

	RETURN
	END
