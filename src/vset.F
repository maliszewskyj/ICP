#include "icp.h"
	SUBROUTINE Vset (psno,volts,curlimit)
C     	****************************

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C       C
C       Routine to setup proper format for sending set volts command to power   C
C       supply controller no. psno . WRPOL is called to actually send command   C
C       via CAMAC GBIP module                                                   C
C       C
C       Gayle Greene, February 1991					      C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C

	use instr
	use polar
	use iso_c_binding
	IMPLICIT    none
	interface
#include "c_gpiben.h"
#include "c_gpibct.h"
	end interface
	INTEGER*4   psno	!power supply number (1 to 6)
        INTEGER*2   cc/2/	!control character for Kepco units
	REAL*4      volts	!voltage in range 0 --> 1.0 for BT2
	REAL*4	    tvolts, E0max, curlimit, I0max/20./
	CHARACTER*20 outstr,reply
	LOGICAL     errflg, highvolts
	INTEGER*4   output,chan,lstr,ivolts,clim,lreply,ierr
	byte	    blf/10/,bcr/13/
	character*1 cr,lf
	equivalence (lf,blf),(cr,bcr)

	IF (psno .gt. pols) GOTO 999      
	IF (psno .le. 0) GOTO 999
C-------Set power supply psno to listener, exit command mode ----------------C
	Call Gpib_listen(psno,errflg)

C-------Set up string in format for indicated power supply controller -------C
	if (poltype(psno).eq.0)then !HP
	   WRITE (UNIT=outstr(1:), FMT= '(a6,f6.3,a1)', ERR= 100) 
	1	';VSET ', volts, ';'
	1	
	   lstr=13
	else if (poltype(psno).eq.2)then !Sorenson
	   output=nint(10000.*volts) !nearest integer
	   write(outstr(1:4),fmt='(i4.4)',err=100)output
	   lstr=4 
	else if (poltype(psno).eq.1) then !old Kepco
	   output=nint(volts*4096.)
	   write(outstr(3:5),fmt='(z3.3)',err=100)output
	   write(outstr(2:2),fmt='(i1)',err=100)cc !control character
	   chan=2-mod(psno,2)	!if psno is even,chan=2; else chan=1
	   write(outstr(1:1),fmt='(i1)',err=100)chan
	   lstr=5
	else if (poltype(psno).eq.3) then !new Kepco
	   if (abs(volts).lt. 2.)then
	      highvolts = .false.
	      E0max = 2.
	   else
	      highvolts = .true.
	      E0max = 20.
	   endif
	   if (volts.lt. 0.)then
	      tvolts = -volts
	      if (highvolts) then 
		 outstr(1:1) = '1' !high neg
	      else
		 outstr(1:1) = '3' !low neg
	      endif
	   else			!positive volts
	      tvolts = volts
	      if (highvolts) then 
		 outstr(1:1) = '0' !high pos
	      else
		 outstr(1:1) = '2' !low pos
	      endif
	   endif
	   ivolts=nint((tvolts*999.0)/E0max)
	   write(outstr(2:4),fmt='(i3.3)',err=90)ivolts
	   clim = nint((99.0*abs(curlimit))/I0max)
	   write(outstr(5:6),fmt='(i2.2)',err=90)clim
	   outstr(1:)=outstr(1:6)//lf//cr
	   lstr=8
c       CALL SMG$PUT_LINE (contr, ' Sending: '//outstr(1:6)) !remove
	else if (poltype(psno).eq.4) then
	   WRITE (UNIT=outstr(1:), FMT='(a10,f6.3,a1)', ERR=100)
	1	'SOUR:VOLT ',volts, ';'
	   lstr = 17
	else
	   call putcontr(' Power Supply type not supported')
	   goto 999
	endif

C-------Send out string to power supply controller via CAMAC GPIB module ----C
	call Wrpol(outstr,lstr,errflg,poltype(psno))

	IF (poltype(psno).eq.4) then
	   outstr(1:) = 'SOUR:VOLT?'
	   lstr = 10
	   CALL WrPol(outstr, lstr, errflg,poltype(psno))
	   CALL Gpib_Unlisten(errflg)
	   CALL Gpib_Talk(psno, errflg)
	   IF (gpibenet) THEN
	      CALL GPIBEN_RD(reply,80,lreply,ierr)
	   ELSE
	      Call GPIBCT_RD(reply,80,lreply,ierr)
	   ENDIF
	   if (ierr.ne.0) errflg=.true.
	   IF (lreply .ge. 5) THEN
	      READ(unit=reply(1:6),fmt=*,err=90) volts
	   ENDIF
	   call putcontr('Voltage set to '//reply)
	ENDIF

	Goto 100
C-------Error treatment -----------------------------------------------------C
 90	call putcontr(' Error: voltage or current out of range')
	Goto 100
 95	call putcontr(' Error reading back voltage')
C-------Switch back to command mode and unlisten all devices  ---------------C 
 100	call Gpib_unlisten(errflg)

 999	RETURN
	END
	
	
