#include "icp.h"
	include COMPILE_OPTS
c Modify motor: Set up automatic and manual motor movement parameters
c   (baserate,slewrate,ramp, conversion constant,rate division factor)
c   in MCU register. They should then remain fixed since no other program
c   may access this RS232. They are not written to non_volatile memory,
c   so power outage will restore default values.
c
	subroutine MOTPAR(motorno)
c
	implicit none
	character*80 command,reply
	integer*2 lcom,lreply
	integer*4 io_stat,motorno,nslew,nbase,slewrate,baserate,
     1	  ramprate,npulse,pulsedeg
	character*4 integer_fmt
	character*1 size,rampstring
	character*10 basestring,slewstring,pulsestring
	integer*4 sys$bintim,status
c
	include INSTR_CMN
	include MOTORS_CMN
c
c
      IF (motfl(motorno)) THEN
	  baserate=base(motorno)
	  slewrate=top(motorno)
	  ramprate=ramp(motorno)
c
c         Set up character strings with appropriate parameters & send out RS232
c
c         Write baserate to base register as well as jog register 
	  nbase=1
	  do while (10**nbase.le.baserate)  !base.lt.10**9
	    nbase=nbase+1
	  enddo
	  write(size,60,iostat=io_stat)nbase
	  if (io_stat.ne.0)goto 750
 60	  format(i1)
	  integer_fmt='(i'//size//')'
	  write(basestring,fmt=integer_fmt,iostat=io_stat)baserate 
	  if (io_stat.ne.0)goto 750
	  command=motorstring(motorno)//'B='//basestring(1:nbase)
	  lcom=4+nbase
	  call rs232(command,lcom,reply,lreply)
	  if (lreply.eq.0)goto 900 !return with no further action if error
	  command=motorstring(motorno)//'J='//basestring(1:nbase) !set Jog speed for manual
	  call rs232(command,lcom,reply,lreply)
	  if (lreply.eq.0)goto 900 !return with no further action if error
c
c         write top speed (slewrate) to velocity register for auto (01) & manual (00)
	  nslew=1 !slew rate
	  do while (10**nslew.le.slewrate) !slew .lt.10**9
	    nslew=nslew+1
	  enddo
	  write(size,60,iostat=io_stat)nslew
	  if (io_stat.ne.0)goto 750
	  integer_fmt='(i'//size//')'
	  write(slewstring,fmt=integer_fmt,iostat=io_stat)slewrate !slewrate(1:nslew)<-slew
	  if (io_stat.ne.0)goto 750
	  command=motorstring(motorno)//'V01='//slewstring(1:nslew) !auto mode
	  lcom=6+nslew
	  call rs232(command,lcom,reply,lreply)
	  if (lreply.eq.0)goto 900 !return with no further action if error
	  command=motorstring(motorno)//'V00='//slewstring(1:nslew) !manual mode
	  lcom=6+nslew
	  call rs232(command,lcom,reply,lreply)
	  if (lreply.eq.0)goto 900 !return with no further action if error
c
C         write ramp for auto (01) and manaul (00)
	  write(rampstring,60,iostat=io_stat)ramprate !ramp .lt. 10
	  if (io_stat.ne.0)goto 750
  	  command=motorstring(motorno)//'R01='//rampstring !for auto mode
	  lcom=7
	  call rs232(command,lcom,reply,lreply)
	  if (lreply.eq.0)goto 900 !return with no further action if error
  	  command=motorstring(motorno)//'R00='//rampstring !for manual mode
	  lcom=7
	  call rs232(command,lcom,reply,lreply)
	  if (lreply.eq.0)goto 900 !return with no further action if error
c
	  pulsedeg=pulse(motorno)
c	  write conversion constant & rate division constant
c         Write baserate to base register as well as jog register 
	  npulse=1
	  do while (10**npulse.le.pulsedeg)  
	    npulse=npulse+1
	  enddo
	  write(size,60,iostat=io_stat)npulse
	  if (io_stat.ne.0)goto 750
	  integer_fmt='(i'//size//')'
	  write(pulsestring,fmt=integer_fmt,iostat=io_stat)pulsedeg 
	  if (io_stat.ne.0)goto 750
	  command=motorstring(motorno)//'C='//pulsestring(1:npulse)
	  lcom=4+npulse
	  call rs232(command,lcom,reply,lreply)
	  if (lreply.eq.0)goto 900 !return with no further action if error


	  IF (nsta .NE. 8)THEN
	    command=motorstring(motorno)//'K=1' !No rate division
	    lcom=5
	    call rs232(command,lcom,reply,lreply)
	    if (lreply.eq.0)goto 900 !return with no further action if error
	  ENDIF
c
c  Update motors.buf file to reflect current values
	  OPEN (UNIT= 2, FILE= MOTORS_BUF, STATUS= 'OLD', 
	1	    ACCESS= 'DIRECT', FORM= 'UNFORMATTED', SHARED,
     1		    RECL=MOTBUF_RECL)
	  WRITE (2,REC=motorno) base(motorno), ramp(motorno), top(motorno),
     1	    pulse(motorno), back(motorno), merr(motorno)
	  CLOSE (2)
c
      END IF
c
c
c
	goto 900
 750	CALL Putcontr(' Error in motor parameters for motor: '//
     1		motorstring(motorno))
 800	continue
c 800	call setef(camerr) !set error flag
 900	return
	end
