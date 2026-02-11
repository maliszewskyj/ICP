#include "icp.h"
      include COMPILE_OPTS
      SUBROUTINE RS232PC_RETRY(command,lcom,reply,lreply,errno)
C     *************************************

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C                                                                             C
C                                                                             C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C

	IMPLICIT     none
	CHARACTER*80 command, reply, local
	INTEGER*2    lcom, lreply, errno, lstr

	INTEGER*4	notries, status, state
#if defined(VMS)
	INTEGER*4       SYS$READEF
#else
        LOGICAL         readef
#endif
	LOGICAL		retry

	INCLUDE INSTR_CMN
	INCLUDE MOTORS_CMN
#if defined(VMS)
	INCLUDE '($ssdef)'
#endif

	local(1:) = command(1:lcom)
	lstr = lcom
	errno = 0 !no error by default

	notries=20 !try up to 20 times 
 20	call rs232PC(local,lstr,reply,lreply)
	if (reply(1:2).ne. command(1:2)
     1		.or.lreply.eq.0)then !error in reply
	  if (notries.gt.0)then
	    notries=notries-1
	    call putcontr(' RS232PC_Retry: will retry command: '
     1		//command(1:lcom)//' since:')
	    IF (lreply .NE. 0)THEN
	      IF (lreply .LT. 20)THEN
	            call putcontr('   bad reply received: '//reply(1:lreply))
	      ELSE
	            call putcontr('   reply received too long')
	      ENDIF
	    ELSE !lreply .eq. 0. No reply
		    call putcontr('   no reply received')
	    ENDIF
#if defined(VMS)
            status = SYS$READEF (%VAL(contr_a), state)  !check for control a
            IF (status .EQ. SS$_WASSET) THEN
#else
            IF (readef(contr_a)) THEN
#endif
	      errno = -1
	      RETURN
	    ELSE
#if defined(VMS)
	      call lib$wait(%ref(.5))
#else
	      CALL LIBWAIT(.5,status)
#endif
	      goto 20
	    ENDIF
	  else !no more tries left
	        errno=1
		CALL Putcontr(' RS232PC_Retry: giving up')
	  endif
	endif !no reply or not correct one


	RETURN
	END
