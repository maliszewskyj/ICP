#include "icp.h"
	SUBROUTINE Init_AD201(errflag)
C	*********************

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C                                                                             C
C     ROUTINE TO INITIALIZE AD201                      C
C     Called from: Tconfig                                                    C
C                                                                             C
C     GAYLE GREENE, February, 1998                                            C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
	use smg_prep
	use ad201
	use iso_c_binding
	implicit none
	interface
#include "c_libwait.h"
	end interface

	LOGICAL	      errflag

	integer*4 i, ios, ival, l, j, status
	integer*4 modeRegHi, modeRegLo, modeRegMid, outb(2),average,filter,
     1		inpbyte, channel,extccode,orgchan,
     1		 wordcnt, baud
	logical  noreply/.false./,getreply/.true./, awake, lresp
	character*1 rdchar,wrchar
	integer*4 lrd
	CHARACTER*80  response,command,reply
	INTEGER*4     lreply,lcom
	character*4 str4
	character*20 str20

	errflag = .FALSE. !default is no error

C  some initialization
  	modeRegHi = 0
C	modeRegHi = iand(modeRegHi,'FE'X) !clear powerdown bit
	modeRegHi = iand(modeRegHi,254) !clear powerdown bit
	modeRegMid = 135
	modeRegLo = 161
	baud = 5
	average = 0
	filter = 1
	channel = 0
	extccode = 0
	wordcnt = 3

	do i = 1,6
	  chanarr(i) = -1
	enddo

C Wakeup AD201
	CALL Putcontr('Signing on to AD201')
	awake = .false.
	i = 40
	wrchar = char(0)
	call ad232empty(rdchar,lrd)
	do while ( i .GT. 0 .and. .not. awake)
	  call ad232one(wrchar,rdchar,lrd)
	  if (lrd .gt. 0)THEN
	    if (ichar(rdchar) .EQ. 3)awake = .true.
	  endif
	  i = i - 1
	enddo

	if (.not. awake)THEN
	  call putcontr(' could not wake up')
	  errflag = .true.
	  return
	endif

C	wrchar = char('88'X)
	wrchar = char(136)
	call ad232one(wrchar,rdchar,lrd)
	wrchar = char(baud)
	call ad232one(wrchar,rdchar,lrd)
	if (lrd .gt. 0)THEN
	  if (ichar(rdchar) .EQ. baud)THEN
	    call putcontr(' baud rate confirmed')
	  else
	    call putcontr(' Error. Could not confirm baudrate')
	    errflag = .true.
	    return
	  endif
	else
	  call putcontr(' Error. No reply confirming baud rate.')
	  errflag = .true.
	  return
	endif

C	command(1:1) = char('0'X)
	command(1:1) = char(0)
	lcom = 1
	call ad232out(command,lcom)

C Setmode
	Call Putcontr(' Setting mode')
	outb(1) = modeRegHi
	outb(2) = modeRegMid
	call sendpacket(outb,reply,lreply,noreply)
	outb(1) = modeRegLo
	call sendpacket(outb,reply,lreply,noreply)
C  Send average and filter
	outb(1) = average
	outb(2) = filter
	call sendpacket(outb,reply,lreply,noreply)
C Read back mode registers
	outb(2) = 1 !set 1 for polled mode
	lreply = 3
	call sendpacket(outb,reply,lreply,getreply)
	if (lreply .eq. 0)then
	  CALL Putcontr('ERR: No reply from AD201 when reading registers')
	  errflag = .TRUE.
	elseif (lreply .lt. 3)then
	  errflag = .TRUE.
	else !have a reply
C	  inpbyte = iand( ichar(reply(1:1)), 'FF'X)
	  inpbyte = iand( ichar(reply(1:1)), 255)
C	  if ( iand(inpbyte,'1F'X) .NE. iand(modeRegHi,'1F'X) )THEN
	  if ( iand(inpbyte,31) .NE. iand(modeRegHi,31) )THEN
	    errflag = .TRUE.
	  endif
C	  inpbyte = iand( ichar(reply(2:2)), 'FF'X)
	  inpbyte = iand( ichar(reply(2:2)), 255)
	  if ( inpbyte .NE. modeRegMid )THEN
	    errflag = .TRUE.
	  endif
C	  inpbyte = iand( ichar(reply(3:3)), 'FF'X)
	  inpbyte = iand( ichar(reply(3:3)), 255)
	  if ( inpbyte .NE. modeRegLo )THEN
	    errflag = .TRUE.
	  endif
	endif
	if (errflag) then
	  Call putcontr(' ERR: Mode registers not confirmed')
	  return
	endif
C  Setccode: verify A/D mode registers and set channel
	outb(1) = 1
	outb(2) = (channel*16) + extccode
	call sendpacket(outb,reply,lreply,noreply)
C  Syscal: calibrate before displaying data
	CALL Putcontr(' Doing system calibration')
	orgchan = channel
	channel = 7
	outb(2) = (channel*16) + extccode
	outb(1) = 130
	lreply = wordcnt + 1
	call sendpacket(outb,reply,lreply,getreply)
	if (lreply .ne. wordcnt+1) THEN 
	  CALL Putcontr('ERR doing offset calib')
	  errflag = .TRUE.
	  return
	endif
	channel = 6
	outb(2) = (channel*16) + extccode
	outb(1) = 131
	lreply = wordcnt+1
	call sendpacket(outb,reply,lreply,getreply)
	if (lreply .ne. wordcnt+1) THEN
	  CALL Putcontr('ERR doing full-scale calib')
	  errflag = .TRUE.
	  return
	endif
	channel = orgchan
C  setccode: restore original channel
	outb(1) = 1
	outb(2) = (channel*16) + extccode
	call sendpacket(outb,reply,lreply,noreply)
c  wait for A/D to settle
	call libwait(.5,status) !wait .5 seconds

	CALL Putcontr('AD201 hardware setup complete')

 10	CALL Putcontr_NF (' ')
	CALL READ_STRING (keyb, str4, 'Enter number of channels to scan: ',
     1			 4,j,j,j, l,j, contr)  !j is for unix   
	READ(unit=str4,fmt=*,err=10)adnumchan
	IF (adnumchan .LT. 1 .or. adnumchan .GT. 6) GOTO 10
	do i = 1, adnumchan
 15	  CALL READ_STRING (keyb, str4, 'Enter a channel to scan: ',
     1			 4,j,j,j, l,j, contr)  !j is for unix   
	  READ(unit=str4,fmt=*,err=15)ival
	  If (ival .lt. 0 .or. ival .GT. 5) GOTO 15
	  chanarr(i) = ival
	enddo

	call putcontr(' Channels to be scanned:')
	do i = 1, adnumchan
	  write(unit=str20,fmt=*,iostat=ios)chanarr(i)
	  call putcontr('   '//str20)
	enddo
 20	CALL READ_STRING (keyb, str4, 'Is this correct? (T/F): ',
     1			 4,j,j,j, l,j, contr) !j is for unix   
	read(unit=str4,fmt='(L1)',err=20)lresp
	IF (.not. lresp)GOTO 10

	RETURN 
	END

