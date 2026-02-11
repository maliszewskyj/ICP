#include "icp.h"

	include COMPILE_OPTS
C  Subroutine for communicating via RS232 port with SCINTAG hardware
C  Note: this routine should not be used for initial power_up command.
C   (Nothing is returned from power up command)
c
	SUBROUTINE RS232X(WRSTRING,LWR,RDSTRING,LRD,SECWAIT,REPLY)
C
	implicit none
C
	include INSTR_CMN
	include MOTORS_CMN
	integer*4 SECWAIT
	LOGICAL Reply !true at call if reply needed
c

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
	integer*4 state,iicount,iflag,timeflag
c
c  local variables for reading from RS232 device
	character*80 instring !message returned by controller
	byte inbyte(80)
	equivalence (instring(1:1),inbyte(1))
	BYTE 	    EQBYTE(2)/61,13/ !"=",CR
	byte cc/3/ !control c to use after abort
c
c  parameters for qio call
	integer*2 qiocode !qio parameter indicating operation
	integer*2 is(4) !status returned from qio. is(2): length of string returned
	integer*4 status
	integer*4 sys$qiow,sys$qio,sys$readef,sys$clref

c  parameters for wflor
	integer*4 cluster,evflmask,sys$wflor

c  qio symbols
	include '($iodef)' !defines sys$qio function codes
	include '($ssdef)'
	include '($clidef)'
C
C Send out command string 
c  move string to write to local write variable
	outstring(1:)=wrstring(1:lwr)
c  add CR to end of command
	outbyte(lwr+1)=13 !CR at end of command
	Lout=lwr+1	
C  write out command string
	qiocode=io$_writepblk.or.io$m_noformat.or.io$m_noecho
	status=sys$qiow(,%val(mcchan(xprom)),%val(qiocode),
     1	  is,,,outbyte,%val(lout),,,,,) 
	if (.not.status)call lib$signal(%val(status))
c	if (.not. reply)return

c  get back reply or detect control a
	IFLAG=contr_a+1 !event flag to signal read has completed
	call sys$clref (%val(iflag))

	cluster = iflag
	evflmask = 0
	evflmask = jibset(evflmask,iflag)
	evflmask = jibset(evflmask,contr_a)
#if defined(VMS)
	call lib$wait(%ref(.01)) !wait before reading
#else
	CALL LIBWAIT(.01,status)
#endif

C  read in reply to command string
	instring(1:)=' ' !init to blank
	QIOcode=io$_readvblk.or.io$m_timed
	status=sys$qio(%val(iflag),%val(mcchan(xprom)),%val(qiocode),
     1	   is,,,inbyte,%VAL(80),%val(secwait),,,,) 
	if (.not.status) call lib$signal(%val(status))
	status = sys$wflor(%val(cluster),%val(evflmask))
#if defined(VMS)
	call lib$wait(%ref(.01))
#else
	CALL LIBWAIT(.01,status)
#endif
        if (sys$readef(%val(Contr_a),state) .eq. ss$_wasset) GOTO 8888 !abort
        if (sys$readef(%val(iflag),state) .eq. ss$_wasset) THEN !reply received
	  lrd=is(2)
	  rdstring(1:lrd)=instring(1:lrd)
#if defined(VMS)
	  call lib$wait(%ref(.01))
#else
	  CALL LIBWAIT(.01,status)
#endif
	  EQBYTE(2)=13
	  IF (RDSTRING(1:1) .NE. '=')THEN
	    QIOcode=io$_readvblk
	    status=sys$qio(,%val(mcchan(xprom)),%val(qiocode),
     1	      is,,,inbyte,%VAL(80),%val(10),,,,) 
	    if (.not.status)call lib$signal(%val(status))
#if defined(VMS)
	    call lib$wait(%ref(.01))
#else
	    CALL LIBWAIT(.01,status)
#endif
	  ENDIF
#if defined(VMS)
	  call lib$wait(%ref(.01))
#else
	  CALL LIBWAIT(.01,status)
#endif
	  qiocode=io$_WRITEPBLK.OR.IO$M_NOFORMAT.OR.IO$M_NOECHO
	  status=sys$qio(,%val(mcchan(xprom)),%val(qiocode),
     1	    is,,,EQbyte,%VAL(2),,,,,) 
	  if (.not.status)call lib$signal(%val(status))
	else
	  lrd = 0
	  CALL Putcontr(' no reply received for: '//outstring(1:lout-1))
 	endif

#if defined(VMS)
	call lib$wait(%ref(.01))
#else
	CALL LIBWAIT(.01,status)
#endif
 
	RETURN

C treat ABORT here
 8888  	CALL Putcontr(' Aborting current command')
#if defined(VMS)
	call lib$wait(%ref(.1))
#else
	CALL LIBWAIT(.1,status)
#endif
	qiocode=io$_writepblk.or.io$m_noformat.or.io$m_noecho
	status=sys$qiow(,%val(mcchan(xprom)),%val(qiocode),
     1	  	   is,,,cc,%val(1),,,,,) 
         if (.not.status)call lib$signal(%val(status))

c	Loop here checking for = response
	iicount = 0
	DO WHILE (instring(1:1).NE.'=' .and. iicount .le. 10)
	  qiocode=io$_readprompt.or.io$m_timed !wait up to set time for answer
	  status=sys$qiow(,%val(mcchan(xprom)),%val(qiocode),
     1	  	   is,,,inbyte,%val(80),%val(3),,eqbyte,%val(2)) !
	  if (.not.status)call lib$signal(%val(status))
	  iicount = iicount + 1
	  type *,' synchronizing'
	ENDDO !waiting for '='CR from SCINTAG processor after abort
#if defined(VMS)
	call lib$wait(%ref(10.))  !wait 10 secs for motors to finish
#else
	CALL LIBWAIT(10.,status)
#endif
	lrd = 0

	RETURN
	END
