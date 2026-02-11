#include "icp.h"
	include COMPILE_OPTS
	SUBROUTINE RS232F_MOVING(WRSTRING,LWR,RDSTRING,LRD)
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C									   C
C  Subroutine for communicating with FOC motor controllers via RS232 port  C
C	Checks for control A in between communication tries.               C
C									   C
C  Gayle Greene, 1990							   C
C  Modified by Gayle Greene, March 1991, to output character by  	   C
C         character with wait in between				   C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
c
C
	implicit none
C
	INCLUDE INSTR_CMN !for contr_a
	INCLUDE MOTORS_CMN 
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
	integer*4 attempts,maxattempts/5/,i
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
 20	QIOCODE=IO$_WRITEPBLK.or.IO$M_NOFORMAT.or.IO$M_NOECHO
	DO I=1,LOUT-1
	  STATUS=SYS$QIO(,%VAL(MCCHAN(ACSF)),%VAL(QIOCODE),
     1	   IS,,,OUTBYTE(I),%VAL(1),,,,,)
	  if (.not.status) type *,'***** qio status: ',status
	  CALL LIB$WAIT (%REF(0.002))
c	  CALL LIBWAIT(.002,status)
	ENDDO
c  write last char as a read with prompt to get back answer
 	qiocode=io$_readprompt.or.io$m_timed.or.io$m_noecho
	status=sys$qiow(,%val(mcchan(ACSF)),%val(qiocode),
     1	  is,,,inbyte,%val(80),%val(2),,outbyte(lout),%val(1))
	if (.not.status)then
	  type *,' RS232 ERR. COMMAND: ',WRSTRING(1:LWR)
	  type *,'***** qiow status: ',status
	endif
	if (is(2).eq.0)then !reply length=0, so error
	  IF (RETRY)THEN !NEED TO TRY 2X FOR STANDARD COMMAND E
             status = SYS$READEF (%VAL(contr_a), state)
             IF (status .EQ. SS$_WASSET) THEN
	       do motorno=1,maxmots !stop all
               IF (moving(motorno)) THEN
	         outstring(2:4)=motorstring(motorno)//'Q'
	         outbyte(5)=13
	         lout=5
	         status=sys$qiow(,%val(mcchan(ACSF)),%val(qiocode),
     1	         is,,,inbyte,%val(80),%val(2),,outbyte,%val(lout))
	         if (is(2).Ne.0) moving(motorno)=.false.
	       endif
	       enddo
	       lrd=-1
	       return
	     endif
	    if (attempts.ge.maxattempts)RETRY=.FALSE.
	    attempts=attempts+1
	    GOTO 20
	  ENDIF
	  type *,' RS232 ERR. NO REPLY. COMMAND: ',WRSTRING(1:LWR)
C	  type *,' IS: ',(is(k),k=1,4)
	  lrd=0
	else
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
