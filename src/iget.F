#include "icp.h"
	SUBROUTINE Iget (psno,amps)
C     	****************************

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C       C
C       Routine to read current in amps from power supply no. psno              C
C       C
C       Nick Maliszewskyj, July 2002					      C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C

	use instr
	use polar
	use iso_c_binding
	IMPLICIT    none
	interface
#include "c_gpiben.h"
#include "c_gpibct.h"
#include "c_rs232_hc.h"
	end interface
	INTEGER*4  psno		!power supply number (1 to 6)
	REAL*4     amps
	CHARACTER*20 outstr
	INTEGER*4   lstr,iamps,vlim,lreply,error,sign,busy
	CHARACTER*80  reply
	LOGICAL     errflg, highamps
	byte	    blf/10/,bcr/13/
	character*1 cr,lf, ichar
	equivalence (lf,blf),(cr,bcr)

c       HP power supply:
	
	amps = -999.9		! Set a nonsense default
	IF (psno .lt. 1) GOTO 95
	IF (psno .gt. pols) GOTO 95	
!       Make an unwarranted assumption: addresses greater than 6 belong to HP
	IF (poltype(psno) .EQ. 0) THEN ! HP
	   CALL Gpib_Listen(psno, errflg) ! set up power supply psno to listen
	   WRITE (UNIT=outstr(1:), FMT='(a7)', ERR=95) ';IOUT?;'
	   lstr = 7
	   CALL WrPol(outstr, lstr, errflg,poltype(psno))
	   CALL Gpib_Unlisten(errflg)

	   CALL Gpib_Talk(psno,errflg)
	   IF (gpibenet) THEN
	      CALL GPIBEN_RD(reply,80,lreply,error)
	   ELSE
	      Call GPIBCT_RD(reply,80,lreply,error)
	   ENDIF

!       FIX ME - This is a kludge
	   if (psno .eq. 5 .and. ((nsta .eq. -1).or.(nsta.eq.-11))) then
	      lstr = index(reply,'IOUT ')
	      if (lstr .le. 0) goto 95
	      if (lstr .gt. lreply) goto 95 !Avoid core dumps
!       May have to strip off cr
	      IF (lreply .ge. 12) THEN
C       print *,'reply(6:12)=',reply(6:12)
		 READ(unit=reply(6:12),fmt=*,err=95) amps
	      ENDIF
	   else
	      IF (lreply .ge. 6) THEN
		 READ(unit=reply(1:6),fmt=*,err=95) amps
	      ENDIF
	   endif


        ELSE IF (poltype(psno) .EQ. 1) THEN
	   CALL Gpib_Listen(psno, errflg) ! set up power supply psno to listen
	   outstr(1:) = ';IOUT?;'//cr
	   lstr = 8
	   CALL WrPol(outstr, lstr, errflg,poltype(psno))
	   CALL Gpib_Unlisten(errflg)

	   CALL Gpib_Talk(psno,errflg)
	   IF (gpibenet) THEN
	      CALL GPIBEN_RD(reply,80,lreply,error)
	   ELSE
	      Call GPIBCT_RD(reply,80,lreply,error)
	   ENDIF

	ELSE IF (poltype(psno) .EQ. 4) THEN
! First read amplitude
	   CALL Gpib_Listen(psno, errflg)
	   outstr(1:) = 'SOUR:CURR?'
	   lstr = 10
	   CALL WrPol(outstr, lstr, errflg,poltype(psno))
	   CALL Gpib_Unlisten(errflg)
	   CALL Gpib_Talk(psno, errflg)
	   IF (gpibenet) THEN
	      CALL GPIBEN_RD(reply,80,lreply,error)
	   ELSE
	      Call GPIBCT_RD(reply,80,lreply,error)
	   ENDIF
	   IF (lreply .ge. 5) THEN
	      READ(unit=reply(1:6),fmt=*,err=95) amps
	   ENDIF

! Then read polarity
! TBD TBD TBD
	   CALL PSSW_STATE(sign, busy, error)
	   IF (error .eq. 0) THEN
	      amps = sign * amps
	   ENDIF

! TBD TBD TBD
	ELSE
	   call putcontr(' Power Supply type not supported')
	   goto 95
	ENDIF

	RETURN
 95	amps = -999.9
	RETURN
	END
