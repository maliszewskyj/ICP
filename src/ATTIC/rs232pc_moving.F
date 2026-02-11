#include "icp.h"
	include COMPILE_OPTS
	SUBROUTINE RS232PC_MOVING(WRSTRING,LWR,RDSTRING,LRD)
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C									   C
C  Subroutine for communicating with motor controllers via PC              C
C	Checks for control A in between communication tries.               C
C									   C
C  Gayle Greene, 1995							   C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
c
C
	implicit none
C
	include INSTR_CMN !for maxmots
	include MOTORS_CMN
c
c  calling parameters
	character*80 wrstring, !string to be written to RS232 device
     1	   rdstring !string returned by RS232 device
	integer*2 lwr, !length of portion used of wrstring
     1	   lrd !length of rdstsring
C
C  local variables for writing to RS232 device 
	character*80 outstring
	byte outbyte(80) !bytes seem to work better in call to sys$qio
	equivalence (outstring(1:1),outbyte(1))
	integer*4 lout !length of portion of outbyte containing command
c
c  local variables for reading from RS232 device
	character*80 instring !message returned by controller
	byte inbyte(80)
	equivalence (instring(1:1),inbyte(1))
c
c  parameters for qio call
	integer*2 qiocode !qio parameter indicating operation
	integer*2 is(4) !status returned from qio. is(2): length of string returned
	integer*4 status,state,motorno
#if defined(VMS)
	integer*4 sys$qiow,sys$qio,sys$setef,sys$readef
	integer*4 notries, i
c
c  qio symbols
	include '($iodef)' !defines sys$qio function codes
	include '($ssdef)'
	include '($clidef)'
C
	integer*4 k
C
c  clear local write variable just to be sure last command is not passed
	outstring=' '
c  move string to write to local write variable
	outstring(1:lwr)=wrstring(1:lwr)
c
C
c  add cr to end of command
	outbyte(lwr+1)=13 !cr at end of command
	Lout=lwr+1	
c	call putcontr(' mv sending: '//outstring(1:lwr))
C
C  first write out command string
	notries = 5 !try up to 5 times
	instring(1:)=' '
 20 	IF (nsta .NE. 8)THEN
	  qiocode=io$_readprompt.or.io$m_timed.or.io$m_noecho
	  status=sys$qiow(,%val(mcchan(PC)),%val(qiocode),
     1	    is,,,inbyte,%val(80),%val(2),,outbyte(1),%val(lout))
	ELSE !use char by char on BT8
 	  QIOCODE=IO$_WRITEPBLK.or.IO$M_NOFORMAT.or.IO$M_NOECHO
	  DO I=1,LOUT-1
	    STATUS=SYS$QIO(,%VAL(MCCHAN(pc)),%VAL(QIOCODE),
     1	     IS,,,OUTBYTE(I),%VAL(1),,,,,)
	    if (.not.status) call putcontr('***** qio status*** ')
	    CALL LIB$WAIT (%REF(0.002))
c	    CALL LIBWAIT(.002,status)
	  ENDDO
c  	write last char as a read with prompt to get back answer
	  qiocode=io$_readprompt.or.io$m_timed.or.IO$M_NOECHO
	  status=sys$qiow(,%val(mcchan(pc)),%val(qiocode),
     1	    is,,,inbyte,%val(80),%val(2),,outbyte(LOUT),%val(1))
	ENDIF !bt8 or not

	if (.not.status)then
	  call putcontr(' RS232 ERR. COMMAND: '//WRSTRING(1:LWR))
	  call putcontr('***** qiow status prob')
c	  call sys$setef(%val(camerr)) !set event flag for hardware error
	endif
	if (is(2).eq.0)then !reply length=0, so error
	  IF (notries .GT. 0)THEN !
	    notries = notries - 1
            status = SYS$READEF (%VAL(contr_a), state)
            IF (status .EQ. SS$_WASSET) THEN
	      do motorno=1,maxmots !stop all
                IF (moving(motorno).AND. mottype(motorno) .EQ. PC) THEN
	         outstring(2:4)=motorstring(motorno)//'Q'
	         outbyte(5)=13
	         lout=5
	         status=sys$qiow(,%val(mcchan(PC)),%val(qiocode),
     1	         is,,,inbyte,%val(80),%val(2),,outbyte,%val(lout))
	         if (is(2).Ne.0) moving(motorno)=.false.
	        endif
	      enddo
	      CALL Putcontr('Control a detected in rs232pc_moving')
	      lrd=-1
	      return
	    ENDIF !control a detected
	    GOTO 20
	  ENDIF
	  call putcontr(' RS232PC_MOVING ERR. NO REPLY. COMMAND: '//WRSTRING(1:LWR))
C	  type *,' IS: ',(is(k),k=1,4)
	  lrd=0
c	  call sys$setef(%val(camerr)) !set event flag for hardware error
	else
	  lrd=is(2)
	  rdstring(1:lrd)=instring(1:lrd)
	endif
C
C
c	CALL LIB$WAIT (%REF(0.1))
#endif
	return
	END
