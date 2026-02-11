#include "icp.h"
	SUBROUTINE ASD_ON(loopno)
C     	****************************

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C                                                                             C
C     WRPOL is called to actually send command to enable ASD via CAMAC GPIB   C
C   									      C
C     Gayle Greene, May 1996					              C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C

	use asd
	use iso_c_binding
	IMPLICIT    none
	interface
#include "c_libwait.h"
	end interface

	INTEGER*4	loopno !which prefactor for cumulating counts
	CHARACTER*30 outstr
	LOGICAL     errflg
	INTEGER*4   lstr, status
	INTEGER*2   ktype/0/ !determines whether a wait between chars is done
	byte	    blf/10/,bcr/13/
	character*1 cr,lf
	equivalence (lf,blf),(cr,bcr)
	real*4	waittime/2./
      
C------- Set ASD to listener                          ----------------C
	Call Gpib_listen(asd_addr,errflg)

C------- Set up string in format for ASD -----------------------------C
	IF (loopno .eq. 1)THEN
	  outstr(1:)='ac(16777215,36,0,0)'//cr
	ELSE
	  outstr(1:)='ac(16777215,34,0,0)'//cr
	ENDIF
	lstr=20
	Call ASD_WrPol(outstr,lstr,errflg,ktype)


	Call Gpib_unlisten(errflg)

c	call lib$wait(%ref(waittime))

	Call Gpib_listen(asd_addr,errflg)
        call libwait(waittime,status)
	CALL Gpib_Execute(errflg)
c	call lib$wait(%ref(waittime))
	Call Gpib_unlisten(errflg)
	RETURN
	END
