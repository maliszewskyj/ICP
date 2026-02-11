#include "icp.h"
C  Subroutine for communicating with magnet controller via RS232 port
	SUBROUTINE SC232(WRSTRING,LWR,RDSTRING,LRD,getreply)
	use tc
	use magnmodel
	use iso_c_binding
	IMPLICIT NONE
	interface
#include "c_rs232_hc.h"
#include "c_rs232_tc.h"
#include "c_libwait.h"
	end interface
	integer*4 GETREPLY

	INTEGER*4 STATUS,MASK(2)/0,8192/

C       CALLING PARAMETERS
	character*80 wrstring,	!string to be written to RS232 device
	1  rdstring		!string returned by RS232 device
	integer*4 lwr,		!length of portion used of wrstring
	1  lrd		!length of portion of rdstring used

C       LOCAL VARIABLES FOR WRITING TO RS232 DEVICE 
	character*80 outstring
	byte outbyte(80)	!bytes seem to work better in call to sys$qio
	equivalence (outstring(1:1),outbyte(1))
	integer*4 lout		!length of portion of outbyte containing command
	INTEGER*4	LASTOUT

C       LOCAL VARIABLES FOR READING FROM RS232 DEVICE
	character*80 instring	!message returned by controller
	byte inbyte(80)
	equivalence (instring(1:1),inbyte(1))

	IF (magmodel .eq. 0) then
	   status = 1
	ELSE
	   status = 0
	ENDIF
	CALL RS232HC_SC(WRSTRING,LWR,RDSTRING,LRD,status,getreply)
	return
	end

C  -----------------------------------------------------------------
C     Subroutine for communicating with magnet controller via RS232 port
C     ensuring result
C     
C 
C     Logical REPLY determines whether to expect a reply from the device.
C     Write WRSTRING to device. If reply is not expected, send RDVERSTRING
C     to verify set value against VERIFY. If reply is expected, return
C     in RDVERSTRING. In either case, keep trying until something is read 
C     back from device. Return logical value in MAGSHUTDOWN if communications
C     fail and magnet is shutdown.
C
C     Note: VERIFY value does not matter if REPLY is .TRUE.
C     In any case, returned value from device 
C     must be of numeric type for internal error checking to work.
C 
	SUBROUTINE VSC232(WRSTRING,LWR,RDVERSTRING,LRDVER,REPLY,VERIFY,MAGSHUTDOWN)
	use instr
	use iso_c_binding
	IMPLICIT NONE
	interface
#include "c_rs232_hc.h"
#include "c_rs232_tc.h"
#include "c_libwait.h"
	end interface

C       CALLING PARAMETERS
	character*80 wrstring,	!string to be written to RS232 device
	1    rdverstring	!readback from or verification string to device

	integer*4 lwr,		!length of portion used of wrstring
	1    lrdver		!length of portion of rdverstring used

	LOGICAL  REPLY		!expect reply from device?
	real*4 verify		!value against which to verify set value
	LOGICAL MAGSHUTDOWN	!TRUE if error forced shutdown
C       LOCAL VARIABLES FOR READING FROM RS232 DEVICE
	CHARACTER*80 rdstring   !readback string from device
	INTEGER*4 lrd,		!length of portion of rdstring used
	1    nattempts,		!number of attempts at communication
	1    maxattempts	!max number of attempts allowed
	REAL*4 rdverify		!value read back from device
	REAL*4 close
	INTEGER*4 ios
	integer*4 NOREPLY,GETREPLY
	character*80 outstring,str20 !strings for error messages

	MAGSHUTDOWN = .FALSE.	!default value, everything good
	maxattempts = 10	!max number of attempts
	nattempts = 1
	rdverify = verify - 1	!anything not equal to verify
	NOREPLY=0
	GETREPLY=1
	DO WHILE( nattempts .LE. maxattempts )
	   
	   IF( .NOT. reply )THEN !trying to set a value
	      CALL sc232(wrstring,lwr,rdstring,lrd,NOREPLY)
	      CALL LIBWAIT(0.1,ios)
	      CALL sc232(rdverstring,lrdver,rdstring,lrd,GETREPLY)
	      IF( lrd .NE. 0 ) READ(UNIT=rdstring(1:lrd),FMT=*,IOSTAT=ios)rdverify
	      close = abs( (rdverify - verify) / (verify+.05) )
	      IF( ios .EQ. 0 .AND. close .LE. 0.01 ) RETURN
	   ELSE			!trying to read a value
	      CALL sc232(wrstring,lwr,rdverstring,lrdver,GETREPLY)
	      IF( lrdver .NE. 0 ) READ(UNIT=rdverstring(1:lrdver),FMT=*,IOSTAT=ios)rdverify
	      IF( ios .EQ. 0 ) RETURN
	   END IF
	   IF( nattempts .GT. 1 ) CALL Putcontr(' Retrying '//wrstring(1:lwr))
	   nattempts = nattempts + 1
	   CALL Holds(2.)
	END DO

C       PROBLEM: maxattempts and still no communication. SHUTDOWN
	MAGSHUTDOWN = .TRUE.
	str20(1:) = ' '
	write(unit=str20(1:5),fmt='(i5)') maxattempts
	outstring = 'Attempted '//str20//' times to communicate with magnet and failed.'
	CALL Shut_mag
	CALL Putcontr(' Magnet COMMUNICATION Error forced SHUTDOWN !')
	CALL Putcontr(' Verify true status of magnet.')
	CALL Putcontr(' Magnet device will have to be reset using hdev.')
	magnctr = .FALSE.     
	RETURN		
	END
