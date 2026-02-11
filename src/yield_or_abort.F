#include "icp.h"
	Subroutine yield_or_abort(motorno,errflag)
C     	***********************************
	
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C                                                                             C
C  Yield_or_abort: wait for motor motorno to stop moving 
c    check for abort periodically and stop motor if detected
C   At return: errflag is true if motor not stopped normally
C
C	Called from: newmono                                              C
C                                                                             C
C	Gayle Greene, January 1998                                            C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
	use instr
	use motors
	use iso_c_binding
	IMPLICIT     none
	interface
#include "c_libwait.h"
#include "c_rs232_acs.h"
#include "c_vme.h"
	end interface
	
	integer*4	motorno !number of motor that is moving
	logical		errflag !true at return if error

	integer*4	i,status, notries, 
     1			maxtries/24/ !2 minutes worth for motor to stop
	real*4		wait1/1./
	logical		finished,stoperr

	character*80	command,reply !for abort
	integer*4	lcom,lreply
	integer*4       st
	logical         readef
	logical		onhome,comerr,plus,minus,stopped

	finished = .false.
	notries = maxtries
	DO WHILE (.NOT. finished .and. notries .GT. 0)
	  DO i = 1,5
	    IF ( READEF(contr_a) ) GOTO 800
	    CALL libwait(wait1,status) !wait 1 second
	    if (i .eq. 5) THEN
	      call Check_Limits(motorno,stopped,plus,minus,onhome,comerr)
	      if (.not. comerr .and. stopped) finished = .true.
	    endif
	  ENDDO !5 second blocks
	  notries = notries - 1
	ENDDO !while not finished
	IF (.not. finished)THEN
	  call putcontr_nt('Gave up waiting for '//motorstring(motorno)//
     1		' to finish moving')
	  errflag = .TRUE. 
	else
	  errflag = .FALSE.
	endif
	return 

C  Abort detected. Go stop motor.
 800 	notries=maxtries
 	stoperr=.true.
	DO WHILE (stoperr .and. notries .GT. 0)
 	    lcom=3
	    command=motorstring(motorno)//'Q'
	    IF (mottype(motorno).EQ. ACS)THEN
	       call rs232(command,lcom,reply,lreply)
	    ENDIF
	    IF (lreply.eq.0)THEN !no reply
	      stoperr=.true.
	    ELSE
	      stoperr=.false.
	      CALL Putcontr_NT('Movement of motor '//motorstring(motorno)//
     1		' aborted by user')
	    ENDIF
	    notries = notries - 1
 	ENDDO
	IF (stoperr)CALL Putcontr_NT('Could not confirm receipt of abort by motor')
	errflag = .TRUE. 

	RETURN
	end
