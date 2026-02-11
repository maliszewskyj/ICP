#include "icp.h"
	SUBROUTINE ASD_TIMED(loopno,secs)
C     	****************************

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C                                                                             C
C     WRPOL is called to actually send command to enable ASD via CAMAC GPIB   C
C   									      C
C     Gayle Greene, May 1996					              C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C

	use asd
	IMPLICIT    none

	INTEGER*4	loopno
	REAL*4		secs

	CHARACTER*30 outstr
	LOGICAL     errflg
	INTEGER*4   lstr
	INTEGER*2   ktype/0/ !determines whether a wait between chars is done
	byte	    blf/10/,bcr/13/
	character*1 cr,lf
	equivalence (lf,blf),(cr,bcr)
	character*8	secstr

	write(unit=secstr,fmt='(I8.8)',err=100)NINT(secs)
      
C------- Set ASD to listener                          ----------------C
	Call Gpib_listen(asd_addr,errflg)

C------- Set up string in format for ASD -----------------------------C
	IF (loopno .eq. 1)THEN
	  outstr(1:)='ac('//secstr//',32,0,0)'//cr
	ELSE
	  outstr(1:)='ac('//secstr//',34,0,0)'//cr
	ENDIF
	lstr=20
	Call asd_WrPol(outstr,lstr,errflg,ktype)

C       do i need to wait here?

	Call Gpib_unlisten(errflg)

	RETURN

 100	call putcontr(' Err converting time. ASD not started')
	RETURN
	END
