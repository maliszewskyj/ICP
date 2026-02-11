#include "icp.h"
	SUBROUTINE Iset (psno,amps,vlimit)
C     	****************************

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C                                                                             C
C     Routine to send amps to power supply controller no. psno in proper      C
C     format via CAMAC GBIP module                                            C
C									      C
C     Gayle Greene, January 1991					      C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C

	use instr
	use polar
	use iso_c_binding
	IMPLICIT    none
	interface
#include "c_libwait.h"
#include "c_rs232_hc.h"
	end interface
	INTEGER*4  psno !power supply number (1 to 6)
	INTEGER*4  retries
     	INTEGER*2  i2data
	REAL*4     amps,volts, tamps, vlimit, I0max, V0max/20./
	CHARACTER*20 outstr
	CHARACTER*80 errstr
	INTEGER*4   lstr,iamps,vlim,sign,busy,ierr
	LOGICAL     errflg, highamps
	byte	    blf/10/,bcr/13/
	character*1 cr,lf
	equivalence (lf,blf),(cr,bcr)

c HP power supply:
	IF (psno .gt. pols) GOTO 999
	IF (psno .lt. 1)    GOTO 999
 500	IF (poltype(psno) .EQ. 0) THEN ! HP
	  CALL Gpib_Listen(psno, errflg)   ! set up power supply psno to listen
	  WRITE (UNIT=outstr(1:), FMT='(a6,f7.4,a1)', ERR=900) 
	1        ';ISET ', amps, ';'
	  lstr = 14
	  CALL WrPol(outstr, lstr, errflg,poltype(psno))
	  CALL Gpib_Unlisten(errflg)

	  ! Check to make sure that we've actually reached the desired current
	  retries = 0
	  CALL libwait(0.1,lstr)
 800	  CALL Iget(psno, tamps)
	  IF (abs(amps - tamps) .gt. 0.05) THEN
	     retries = retries + 1
	     if ( retries .gt. 10) goto 990
	     goto 800
	  ENDIF

c sorenson or old kepco: Convert to volts and call vset
	ELSE IF (poltype(psno) .EQ. 1 .or. poltype(psno) .EQ. 2) then
	  IF (fullamp(psno) .eq. 0)then     
	    CALL putcontr(' Error: Must run famp for all power supplies')
	    goto 999
	  ENDIF
	  volts= (.999*amps) / fullamp(psno)
	  IF (volts .GT. 999) volts = .999
	  CALL Vset(psno, volts)

c new kepco:
	ELSE IF (poltype(psno) .EQ. 3) then !new kepko
	  CALL Gpib_Listen(psno, errflg)   ! set up power supply psno to listen
	  if (abs(amps).lt. 2.)then
	    highamps = .false.
	    I0max = 2.
	  else
	    highamps = .true.
	    I0max = 20.
	  endif
	  if (amps .lt. 0.)then
	    tamps = -amps
	    if (highamps) then 
	      outstr(1:1) = '5' !high neg
	    else
	      outstr(1:1) = '7' !low neg
	    endif
	  else	 !positive amps
	    tamps = amps
	    if (highamps) then 
	      outstr(1:1) = '4' !high pos
	    else
	      outstr(1:1) = '6' !low pos
	    endif
	  endif
	  iamps=nint((tamps*999.0)/I0max)
	  write(outstr(2:4),fmt='(i3.3)',err=900)iamps
	  vlim = nint((99.0*abs(vlimit))/V0max)
	  write(outstr(5:6),fmt='(i2.2)',err=900)vlim
	  outstr(1:)=outstr(1:6)//lf//cr
	  lstr=6
	  CALL WrPol(outstr, lstr, errflg,poltype(psno))
	  CALL Gpib_Unlisten(errflg)
	ELSE IF (poltype(psno) .EQ. 4) then ! Sorenson DCS 60-50E
	   ! First read current setting
	   ! Then determine if we're going through zero
	   ! If we're going through zero:
	   !    1. Send current to zero
	   !    2. Open the isolation relay
	   !    3. Reverse the polarity
	   !    4. Close the isolation relay
           !    5. Ramp up to desired voltage
	   CALL PSSW_STATE(sign,busy,ierr)
	   if (ierr.ne.0) goto 991
	   IF (((amps .lt. 0) .and. (sign .ge. 0)) .or.
	1	((amps.ge. 0) .and. (sign .lt. 0))) THEN

	      outstr(1:)='SOUR:CURR 0.00;'
	      lstr=15
	      CALL WrPol(outstr,lstr,errflg,poltype(psno))
	      retries = 0
	      CALL libwait(0.1,lstr)
 850	      CALL Iget(psno, tamps)
	      IF (abs(tamps) .gt. 0.05) THEN
		 retries = retries + 1
		 if ( retries .gt. 10) goto 980
		 goto 850
	      ENDIF

	      CALL PSSW_TOGGLE(ierr)
 851	      CALL PSSW_STATE(sign,busy,ierr)
	      IF (errflg) goto 991
	      IF (busy .gt. 0) goto 851
	   ENDIF
	   tamps = abs(amps)
	   write(unit=outstr(1:),fmt='(a10,f5.2,a1)') 'SOUR:CURR ',tamps,';'
	   lstr=15
	   CALL WrPol(outstr,lstr,errflg,poltype(psno))

	  retries = 0
	  CALL libwait(0.1,lstr)
 860	  CALL Iget(psno, tamps)
	  IF (abs(amps - tamps) .gt. 0.05) THEN
	     retries = retries + 1
	     if ( retries .gt. 10) goto 990
	     goto 860
	  ENDIF

	ELSE
	   call putcontr(' Power Supply type not supported')
	   goto 999
	ENDIF

	goto 999
C------Error treatment
 900	call putcontr(' Error: volt or current value out of range')
 	CALL Gpib_Unlisten(errflg)
	goto 999
 980	call putcontr(' Power could not run down to zero amps')
	goto 999
 990	write(unit=errstr(1:), fmt='(a,f7.2,a,f7.2,a)') 'Error : current (',
	1    tamps,') has not reached setpoint (',amps,')'
   	call putcontr(errstr)
	goto 999
 991	call putcontr(' Error: unspecified error changing current')

 999	return
	END
      
      
