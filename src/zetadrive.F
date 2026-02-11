#include "icp.h"
	 SUBROUTINE ZETADrive(motorno,targetangle)
C     *************************************

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C                                                                             C
C     Drive: Move Motor Specified To Physical Angle Indicated                 C
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
#include "c_debugval.h"
#include "c_zeta.h"
	 end interface
	 CHARACTER*80 command, reply, gocommand
	 INTEGER*2    lcom, lreply, errno, notries, gocom
	 INTEGER*4    motorno, nsteps, m_err, m_back, newpos, oldpos
	 INTEGER*4    io_stat, imove, secwait, status, ichange
	 REAL*4       targetangle
	 CHARACTER*4  integer_fmt, errstr
	 CHARACTER*1  size, DIRC
	 CHARACTER*10 stepstring
	 LOGICAL*1    retry,goretry
	 LOGICAL      rserr, dobacklash
	 INTEGER*4    retn, dbg

	 CALL DEBUGVAL(dbg)

	 retry = .true.
	 if (mottype(motorno) .ne. ZETA) then
	    write(stepstring,fmt='(i2)') motorno
	    call putcontr('ZETADRIVE: motor '//stepstring(1:2)//
	1	 'is not a ZETA motor')
	    return
	 endif

	 m_err  = merr(motorno)
	 m_back = back(motorno)
C       
C       Get current motor position
C       
	 notries = 10
	 retn = -1
	 do while ((notries .gt. 0) .and. (retn .lt. 0)) 
	    retn = ZETAGETPOS(motorno,oldpos)
	    if (retn .lt. 0) call sleep(1)
	    notries = notries - 1
	 enddo
	 if (retn .lt. 0) then
	    call putcontr('ZETADRIVE: could not determine position')
	    return
	 endif        
C       
C       Determine distance and direction of move
C       
	 newpos = NINT(targetangle*pulse(motorno))	
	 ichange = newpos - oldpos
	 if (dbg .ge. 1) THEN
	    print *,'ZETADRIVE: m_err = ',m_err
	    print *,'ZETADRIVE: m_back = ',m_back
	    print *,'ZETADRIVE: oldpos = ',oldpos
	    print *,'ZETADRIVE: newpos = ',newpos
	    print *,'ZETADRIVE: change = ',ichange
	 endif
	 if (ABS(ichange).le.m_err) then 
	    if (dbg .ge. 1)
	1	 call putcontr('Position within error, not moving')
	    return		! return if in position
	 endif
	 moving(motorno) = .true.
	 destination(motorno) = targetangle

! work out backlash
	 dobacklash = (((ichange .LT. 0) .AND. (back(motorno) .GT. 0))
	1     .OR. ((ichange .GT. 0) .AND. (back(motorno) .LT. 0)))


	 if (dobacklash) then
	    newpos = newpos-back(motorno)
	    overshot(motorno) = .true. ! set flag to indicate intermediate
	 else 
	    overshot(motorno) = .false.
	 endif      

C       
C       Tell controller to move to target position
C       
	 if(trackmoves)call memcurpos(motorno,1,newpos)
	 retn = ZETAGOPOS(motorno,newpos)

! Ask controller whether we're actually moving

	 RETURN
	 END
