#include "icp.h"
c	
	include COMPILE_OPTS
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C									   C
C  Subroutine for communicating with furnace controller via RS232 port     C
C									   C
C  Gayle Greene, 1997							   C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
c
	SUBROUTINE FRN232(WRSTRING,LWR,RDSTRING,LRD,TCHAR,CKSUM)
C
	implicit none
C
c	include INSTR_CMN !for camerr
	include TC_CMN
c
c  calling parameters
	character*80 wrstring, !string to be written to RS232 device
     1	   rdstring !string returned by RS232 device
	integer*2 lwr, !length of portion used of wrstring
     1	   lrd, !length of rdstsring
     1	   tchar !terminating character in read reply
	logical	 cksum  !true if check sum to be added to string
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
	integer*4 status,I
	integer*4 sys$qiow,sys$qio

	character*80 tstring
c
c  qio symbols
	include '($iodef)' !defines sys$qio function codes
	include '($ssdef)'
	include '($clidef)'
C
	integer*4 k, isum

c  furnace id string
	character*4	idstr/'0011'/
C
C  Structure for creating terminator mask
	STRUCTURE /Masklist/
	  UNION
	    MAP
		INTEGER*2  notused,masksize
		INTEGER*4  mask_adr
	    END MAP
 	  END UNION
	END STRUCTURE 
	RECORD/Masklist/mask
	INTEGER*4 term_mask(2)/2097224,0/ !set to 3 terminators:
C        2**3 + 2**6 + 2**21 = 8 + 64 + 2097152 = ETX + ACK + NAK
c	INTEGER*4 term_mask(2)/0,0/ !set to no terminator
C	INTEGER*4 term_mask(2)/0,16/ !set $ to be only terminator
C	  0 refers to NUL(0)->US(31). 
C         16 is bit 5 set. This refers to 31 + 5 = 36 ascii ($)
C  set up terminator mask
	mask.notused = 8
	mask.masksize = 8
	mask.mask_adr = %LOC(term_mask)	

c  clear local write variable just to be sure last command isn't passed
	outstring=' '
c  move string to write to local write variable
	outstring(1:lwr)=wrstring(1:lwr)
c
C
c  IF CKSUM: add checksum to end of string
	IF (CKSUM)THEN !add checksum
	  isum = 0 !calculate checksum
	  do i = 7, lwr
	    isum = isum .XOR. outbyte(i)
	  enddo
	  outstring(lwr+1:lwr+1) = CHAR(isum)
	  lout = lwr + 1
	ELSE
	  lout = lwr
	ENDIF

C  Write command string to screen and log file
	tstring(1:) = ' '
	do i = 1,lout
	  IF (outbyte(i) .GE. 32 .AND. outbyte(i) .LE. 126)THEN
	      tstring(i:i) = outstring(i:i)
	  ELSE
	      tstring(i:i) = ' '
	  ENDIF
	enddo
	call putcontr(' command: '//tstring(1:lout))
	tstring(1:)=' '
	write(unit=tstring,fmt='(1x,a,80(1x,Z2.2))')'         ',(outbyte(i),i=1,lout)
	call putcontr(tstring)

C
C  first write out command string
 20	QIOCODE=IO$_WRITEPBLK.or.IO$M_NOFORMAT.or.IO$M_NOECHO
	DO I=1,LOUT-1
	  STATUS=SYS$QIO(,%VAL(tempchan),%VAL(QIOCODE),
     1	   IS,,,OUTBYTE(I),%VAL(1),,,,,)
	  if (.not.status) CALL Putcontr('***** qio status prob')
	  CALL LIBWAIT (0.2,status)
	ENDDO
c  write last char as a read with prompt to get back answer
	instring(1:)=' '
	qiocode=io$_readprompt.or.io$m_timed.or.IO$M_NOECHO
	status=sys$qiow(,%val(tempchan),%val(qiocode),
     1	  is,,,inbyte,%val(80),%val(7),MASK,outbyte(LOUT),%val(1))
	if (.not.status)then
	  call putcontr(' FRN_232 ERR. COMMAND: '//WRSTRING(1:LWR))
	  call putcontr(' ***** qiow status prob')
c	  call sys$setef(%val(camerr)) !set event flag for hardware error
	endif
	tchar = is(3) !return terminating character of reply
	lrd = is(2) !return length of reply
	if (is(2).eq.0 .AND. is(3).eq.0)then !reply length=0 & no special term, so error
	  call putcontr(' FRN_232 ERR. NO REPLY. COMMAND: '//WRSTRING(1:LWR))
c	  call sys$setef(%val(camerr)) !set event flag for hardware error
	else
	  rdstring(1:lrd)=instring(1:lrd)
	  tstring(1:)=' '
	  do i = 1,lrd
	    IF (inbyte(i) .GE. 32 .AND. inbyte(i) .LE. 126)THEN
	      tstring(i:i) = instring(i:i)
	    ELSE
	      tstring(i:i) = ' '
	    ENDIF
	  enddo
c	  Type reply to string.
	  IF (lrd .LE. 80)THEN !reasonable length reply
	    call putcontr(' reply: '//tstring(1:lrd))
	    write(unit=tstring,fmt='(1x,a,80(1x,Z2.2))')'       ',(inbyte(i),i=1,lrd)
	    call putcontr(tstring)
	  ELSE
	    write(unit=7,fmt='(1x,a)')'Reply too long to show'
	    TYPE *,' Reply too long to show'
	  ENDIF
	endif
C
c	CALL LIBWAIT (0.1,status)
	return
	END
