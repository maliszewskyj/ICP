#include "icp.h"
	SUBROUTINE Kfetch (reply,lreply)
C     	****************************
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C                                                                             C
C       Routine to read back value from KEITHLEY IEEE module using FETCH cmd  C
C       WRPOL is called to actually send command                              C
C       RdKeith is called to read back command                                C
C       via CAMAC GBIP module                                                 C
C                                                                             C
C       Gayle Greene, March 1994					      C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C

	use instr
	IMPLICIT    none
	CHARACTER*20 outstr
	CHARACTER*50 reply
	INTEGER*2   lreply
	LOGICAL     errflg
	INTEGER*4   lstr
	INTEGER*4   gpadr/16/
	INTEGER*2   ktype/0/	!determines whether a wait between chars is done
	byte	    blf/10/,bcr/13/
	character*1 cr,lf
	equivalence (lf,blf),(cr,bcr)

C-------Set Keithly to listener                          ----------------C
	Call Gpib_listen(gpadr,errflg)

C-------Set up string in format for Keithly -----------------------------C
	outstr(1:)=':FETCH?'//lf
	lstr=8
	Call WrPol(outstr,lstr,errflg,ktype)

	Call Gpib_unlisten(errflg)

	Call Gpib_talk(gpadr,errflg)

C-------Read back string from KEITHLY via CAMAC GPIB module ----C
	call RdKeith(reply,lreply,errflg)

C-------Switch back to command mode and unlisten all devices  ---------------C 
 100	call Gpib_unlisten(errflg)

	RETURN
	END
	
      
