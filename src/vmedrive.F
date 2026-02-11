#include "icp.h"
	SUBROUTINE VMEDrive(motorno,targetangle)
C     *************************************

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C                                                                             C
C     Drive: Move Motor Specified To Physical Angle Indicated                 C
C     Called from: Parse_Contr, Run_Q_Buf, Run_I_Buf, Run_T_Buf, Find_Peak    C
C                                                                             C
C     Nick Maliszewskyj          January 2000                                 C
C                                October 2011 - Migrate to gfortran           C
C     Version = $Id$    C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C

	use instr
	use motors
	use iso_c_binding
	
	IMPLICIT     none
	interface
#include "c_debugval.h"
#include "c_vme.h"
	end interface
	CHARACTER*80 command, reply, gocommand
	INTEGER*2    lcom, lreply, errno, notries, gocom
	INTEGER*4    motorno, nsteps
	INTEGER*4    io_stat, imove, secwait, status
	REAL*4       targetangle,oldpos, newpos, change,m_err,m_back
	CHARACTER*4  integer_fmt, errstr
	CHARACTER*1  size, DIRC
	CHARACTER*10 stepstring
	LOGICAL*1    retry,goretry
	LOGICAL	   rserr, dobacklash
	INTEGER*4    retn, dbg

	CALL DEBUGVAL(dbg)


	retry = .true.
	if (mottype(motorno) .ne. VME) then
	   write(stepstring,fmt='(i2)') motorno
	   call putcontr('VMEDRIVE: motor '//stepstring(1:2)//
	1	'is not a VME motor')
	   return
	endif

	m_err  = float(merr(motorno))/float(pulse(motorno))
	m_back = float(back(motorno))/float(pulse(motorno))
C       print *, 'VMEDRIVE: move ',motorno,' to ',targetangle

C       
C       Get current motor position
C       
	notries = 10
	retn = -1
	do while ((notries .gt. 0) .and. (retn .lt. 0)) 
	   retn = VMEGETPOS(motorno,oldpos)
	   if (retn .lt. 0) call sleep(1)
	   notries = notries - 1
	enddo
	if (retn .lt. 0) then
	   call putcontr('VMEDRIVE: could not determine position of motor')
	   return
	endif        
C       
C       Determine distance and direction of move
C       
	newpos = targetangle
	change = newpos - oldpos
	dobacklash = (((change .LT. 0) .AND. (m_back .GT. 0))
	1    .OR. ((change .GT. 0) .AND. (m_back .LT. 0)))
	if (dbg .ge. 1) THEN
	   print *,'VMEDRIVE: m_err = ',m_err
	   print *,'VMEDRIVE: m_back = ',m_back
	   print *,'VMEDRIVE: oldpos = ',oldpos
	   print *,'VMEDRIVE: newpos = ',newpos
	   print *,'VMEDRIVE: change = ',change
	endif
	if (ABS(change).le.abs(float(merr(motorno))/float(pulse(motorno)))) then
	   if (dbg .ge. 1) call putcontr('Position within error, not moving')
	   return		! return if in position
	endif
	moving(motorno) = .true.
	destination(motorno) = targetangle

! work out backlash
	if (dobacklash) then
	   newpos = targetangle - ((float(back(motorno)))/float(pulse(motorno)))
	   overshot(motorno) = .true. ! set flag to indicate intermediate
! target
	else 
	   overshot(motorno) = .false.
	endif      

C       
C       Tell controller to move to target position
C       
	if(trackmoves)call memcurpos(motorno,1,newpos)
	if (nsta.eq.-11) call VMEENABLE(motorno) ! Enable motor to be moved
	retn = VMEGOPOS(motorno,newpos)

! Ask controller whether we're actually moving


	RETURN
	END
