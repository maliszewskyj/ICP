#include "icp.h"
	SUBROUTINE HpVolt (reply,lreply)
C     	****************************

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C                                                                             C
C     Routine to read back voltage from HP power supply used for field controlC
C     WRPOL is called to actually send command                                C
C     get_hpvolt is called to read back command                                C
C     via CAMAC GBIP module                                                   C
C   									      C
C     Gayle Greene, March 1994					              C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C

	use tc
	IMPLICIT    none
	CHARACTER*20 outstr
	CHARACTER*80 reply
	INTEGER*2   lreply
	LOGICAL     errflg
	INTEGER*4   lstr, maxtries/5/, i
	LOGICAL	    okreply
     	INTEGER*2  hpdev/0/ !HP format will be used in comm with mag controller


	okreply = .FALSE.
	i = 1
	DO WHILE (i .LT. maxtries .AND. .NOT. okreply)
	 i = i + 1
C------- Set HP to listener                          ----------------C
	 Call Gpib_listen(magport,errflg)

C------- Set up string in format for HP -----------------------------C
	 outstr(1:)=';VOUT?;'
	 lstr=7
	 Call WrPol(outstr,lstr,errflg,hpdev)

	 Call Gpib_unlisten(errflg)

	 Call Gpib_talk(magport,errflg)

C------- Read back string from HP via CAMAC GPIB module ----C
	 call get_hpvolt(reply,lreply,errflg)

C------- Switch back to command mode and unlisten all devices  ---------------C 
 	 call Gpib_unlisten(errflg)

C------- Check reply ----------------------------------------------C
	 IF (lreply .GT. 4 .AND. reply(1:4) .EQ. 'VOUT') okreply = .TRUE.

	ENDDO

	RETURN
	END
      
      
