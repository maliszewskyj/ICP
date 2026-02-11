#include "icp.h"
	include COMPILE_OPTS
C XDTSUB: SUBROUTINE TO READ CURVE TABLE FROM LAKESHORE TEMP CONTROLLER
	subroutine XDTSUB(nopoints,curveid,rderr)
C
	IMPLICIT NONE
c
c  Calling parameters
	integer*4 nopoints(0:15) !no. of points or 0 if no curve
	character*6 curveid(0:15) !has 6 char id for each curve
	logical rderr !true if no table available

#if defined(VMS)
C
C  VARIABLES FOR READING AND DECODING CURVE TABLE
	CHARACTER*1600 REPLY
	INTEGER*2 LCOM,LREPLY
	INTEGER*4 ICRV,IBUF,I

c
C  local variables for writing to RS232 device 
	character*81 outstring
	byte outbyte(81) !bytes seem to work better in call to sys$qio
	equivalence (outstring(1:1),outbyte(1))
	integer*4 lout !length of portion of outbyte containing command
c
c  local variables for reading from RS232 device
	character*1600 instring !message returned by controller
	byte inbyte(1600)
	equivalence (instring(1:1),inbyte(1))
c
c  parameters for qio call
	integer*2 qiocode !qio parameter indicating operation
	integer*2 is(4) !status returned from qio. is(2): length of string returned
	integer*4 status
	integer*4 sys$qiow
c
c  qio symbols
	include '($iodef)' !defines sys$qio function codes
	include '($clidef)'
C
	include TC_CMN

	RDERR=.FALSE.
C
	reply=' '
c  move string to write to local write variable
	outstring='XDT'
	lcom=3
c
C
c  add lf to end of command
	outbyte(lcom+1)=10 !lf at end of command
	Lout=lcom+1
	lreply=0
C
C  write out command string & read reply
	qiocode=io$_readprompt.or.io$m_timed !wait up to set time for answer
	status=sys$qiow(,%val(tempchan),%val(qiocode),
     1	  is,,,inbyte,%val(1600),%val(8),,outbyte,%val(lout))
	if (.not.status)then
	  goto 900
	endif
 	if (is(2).eq.0)then !reply length=0, so error
	  goto 900
	else
	  lreply=is(2)
	  reply(1:lreply)=instring(1:lreply)
	endif
C
	IF (REPLY(1:3).EQ.'ERR')THEN
	  GOTO 900 !RETURN WITH ERROR
	ENDIF
C
C: Fill parameters with XDT information from buffer REPLY
	  ICRV=0
	  IBUF=39
	  DO I=0,3 !CURVES 0 - 3 ALWAYS THERE
	    READ(REPLY(IBUF:IBUF+1),FMT='(I2)',ERR=900)ICRV
	    READ(REPLY(IBUF+3:IBUF+4),FMT='(I2)',ERR=900)NOPOINTS(ICRV)
	    READ(REPLY(IBUF+11:IBUF+16),FMT='(A6)',ERR=900)CURVEID(ICRV)
	    IBUF=IBUF+18
	  ENDDO
 500	  READ(REPLY(IBUF:IBUF+1),FMT='(i2)',ERR=600,END=600)ICRV
	  IF (ICRV.GE.4.AND.ICRV.LE.15)THEN
	    READ(REPLY(IBUF+3:IBUF+4),FMT='(I2)',ERR=900)NOPOINTS(ICRV)
	    READ(REPLY(IBUF+11:IBUF+16),FMT='(A6)',ERR=900)CURVEID(ICRV)
	    IBUF=IBUF+18
	    GOTO 500
	  ENDIF
C
C
 600	GOTO 999
 900	RDERR=.TRUE. !JUMP HERE WITH ERRORS
#endif
 999	CONTINUE
	END
C
