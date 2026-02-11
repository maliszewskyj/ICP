#include "icp.h"
	SUBROUTINE PiDrive(motorno,targetangle)
C     *************************************

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C                                                                             C
C     Drive: Move Motor Specified To Physical Angle Indicated                 C
C     Note: Entry Drives no longer moves motor slowly. All drive is same speedC
C     Called from: Parse_Contr, Run_Q_Buf, Run_I_Buf, Run_T_Buf, Find_Peak    C
C                                                                             C
C     Nick Maliszewskyj          January 2000                                 C
C     Version = $Id$    C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
	use instr
	use motors
	use iso_c_binding
	IMPLICIT     none
	interface
#include "c_pi.h"	   
	end interface
	CHARACTER*80 command, reply, gocommand
	INTEGER*2    lcom, lreply, errno, notries, gocom
	INTEGER*4    motorno, nsteps, oldpos,newpos,ichange
	INTEGER*4    io_stat, imove, secwait, status
	REAL*4       targetangle
	CHARACTER*4  integer_fmt, errstr
	CHARACTER*1  size, DIRC
	CHARACTER*10 stepstring
	LOGICAL*1    retry,goretry
	LOGICAL	   rserr

	INTEGER*4    retn

	retry = .true.
	if (mottype(motorno) .ne. PIMOT) then
	   write(stepstring,fmt='(i2)') motorno
	   call putcontr('PIDRIVE: motor '//stepstring(1:2)//
	1	'is not a PI motor')
	   return
	endif

C       
C       Get current motor position
C       
	notries = 10
	retn = -1
	do while ((notries .gt. 0) .and. (retn .lt. 0)) 
	   retn = PIGETPOS(motorno,oldpos)
	   if (retn .lt. 0) call sleep(1)
	   notries = notries - 1
	enddo
	if (retn .lt. 0) then
	   call putcontr('PIDRIVE: could not determine position of motor')
	   return
	endif        
C       
C       Determine distance and direction of move
C       
	newpos = NINT(targetangle * pulse(motorno))
	ichange = newpos - oldpos
	if (ABS(ichange) .le. merr(motorno)) return ! return if in position
	moving(motorno) = .true.
	destination(motorno) = targetangle

! work out backlash
	if (ichange .lt. 0) then
	   newpos = newpos - back(motorno)
	   overshot(motorno) = .true. ! set flag to indicate intermediate
! target
	else 
	   overshot(motorno) = .false.
	endif      

	imove = IABS(newpos)

C       
C       Tell controller to move to target position
C       
	if(trackmoves)call memcurpos(motorno,1,newpos)
	retn = PIGOPOS(motorno,newpos)

! Ask controller whether we're actually moving

	RETURN
	END
