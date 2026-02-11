#include "icp.h"
c	
	include COMPILE_OPTS
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C									   C
C  Subroutine for communicating with FOCUS motor controllers via RS232 portC
C									   C
C  Gayle Greene, 1989							   C
C  Modified by Gayle Greene, March 1991, to output character by  	   C
C         character with wait in between				   C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
c
	SUBROUTINE RS232F(WRSTRING,LWR,RDSTRING,LRD)
C
	implicit none
	INCLUDE INSTR_CMN !for maxmots & maxtypes
	INCLUDE MOTORS_CMN !for mcchan
C
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
	integer*4 status,I, attempts, maxattempts/5/
#if defined(VMS)
	integer*4 sys$qiow,sys$qio,sys$setef
	LOGICAL*1 RETRY
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
	outstring(2:lwr+1)=wrstring(1:lwr)
	outstring(1:1)='#'
	lwr=lwr+1
c
C
c  add cr to end of command
	outbyte(lwr+1)=13 !cr at end of command
	Lout=lwr+1	
C
C  first write out command string
	RETRY=.TRUE.
	attempts=0
c	type *,' Sending: ',outstring(1:lout-1)
 20	QIOCODE=IO$_WRITEPBLK.or.IO$M_NOFORMAT.or.IO$M_NOECHO
	DO I=1,LOUT-1
	  STATUS=SYS$QIO(,%VAL(MCCHAN(ACSF)),%VAL(QIOCODE),
     1	   IS,,,OUTBYTE(I),%VAL(1),,,,,)
	  if (.not.status) type *,'***** qio status: ',status
	  CALL LIB$WAIT (%REF(0.002))
c	  CALL LIBWAIT(.002,status)
	ENDDO
c  write last char as a read with prompt to get back answer
	qiocode=io$_readprompt.or.io$m_timed.or.IO$M_NOECHO
	status=sys$qiow(,%val(MCchan(ACSF)),%val(qiocode),
     1	  is,,,inbyte,%val(80),%val(2),,outbyte(LOUT),%val(1))
	if (.not.status)then
	  type *,' RS232 ERR. COMMAND: ',WRSTRING(1:Lout-1)
	  type *,' ***** qiow status: ',status
	endif
	if (is(2).eq.0)then !reply length=0, so error
	  if (retry)then
	    IF (attempts.ge.maxattempts)retry=.false.
	    attempts=attempts+1
	    call lib$wait(%ref(.1))
c	    CALL LIBWAIT(.1,status)
	    type *,' rs232 retrying: ',outstring(1:lwr)
	    GOTO 20
	  else
	    type *,' RS232 ERR. NO REPLY. COMMAND: ',WRSTRING(1:Lout-1)
	    lrd=0
	  endif
	else !remove initial '#' and return reply
	  lrd=is(2)-1
	  rdstring(1:lrd)=instring(2:lrd+1)
	endif
C
C
	
	CALL LIB$WAIT (%REF(0.1))
c	CALL LIBWAIT(.1,status)
#endif
	return
	END
