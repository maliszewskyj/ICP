#include "icp.h"
	SUBROUTINE Init_Tdb(errflag)
C	*********************

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C                                                                             C
C     ROUTINE TO INITIALIZE Transducer Board                          C
C     Called from: Tconfig                                                    C
C                                                                             C
C     GAYLE GREENE, September, 1997                                        C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
	use tdb
	use smg_prep
	implicit none

	CHARACTER*80  response,command,reply
	INTEGER*4     lreply,lcom
	INTEGER*4     errflag,noreply/0/
	CHARACTER*50  spconfig(3)
	INTEGER*4  j,ios,l,ig, idum
	CHARACTER*4   str4
	CHARACTER*20   str20

	DATA spconfig /
	1  '        SS:       Stress Mode              ',
	2  '        RA:       Strain Mode              ',
	3  '        NO:       No Log Mode              '/

	CALL Putcontr_NF ('Select Mode: ')
	CALL Putcontr_NF (' ')
	DO j = 1, 3
	      CALL Putcontr_NF (spconfig(j))
	END DO
	CALL Putcontr_NF (' ')
	CALL READ_STRING (keyb, str4, 'Selection (SS/RA/NO): ',
     1			 4,idum,idum,idum, l,idum, contr) !idum is for unix  
 10	IF (str4(1:2) .EQ. 'NO' .OR. str4(1:2) .EQ. 'no')THEN
	  tdbmode = 3 !no log mode
	  tdbconv = 1.
	ELSEIF (str4(1:2) .EQ. 'SS' .OR. str4(1:2) .EQ. 'ss')THEN
	  tdbmode = 1 !stress mode
 25 	  CALL READ_STRING(keyb, str20,
     1		'Enter conversion factor (default: 1): ',20,
     1		idum,idum,idum, ig,idum, contr) !idum is for unix
	  IF (ig .EQ. 0)THEN
	    tdbconv = 1.
	  ELSE
	    READ(unit=str20(1:ig),fmt=*,err=25)tdbconv
	  ENDIF
	ELSEIF (str4(1:2) .EQ. 'RA' .OR. str4(1:2) .EQ. 'ra')THEN
	  tdbmode = 2 !strain mode
 35 	  CALL READ_STRING(keyb, str20,
     1		'Enter conversion factor (default: 1): ',20,
     1		idum,idum,idum, ig,idum, contr) !idum is for unix
	  IF (ig .EQ. 0)THEN
	    tdbconv = 1.
	  ELSE
	    READ(unit=str20(1:ig),fmt=*,err=35)tdbconv
	  ENDIF
	ELSE
	  CALL Putcontr_NF (' ')
	  CALL READ_STRING (keyb, str4, 
     1	   'Please enter SS,RA,NO or return for exit: ',
     1			 4,idum,idum,idum, l,idum, contr) !idum is for unix
	  READ (UNIT=str4(1:l), FMT=*,iostat=ios) j
	  IF (j .eq. 0) THEN
	    ERRFLAG = 1
	    CALL Putcontr(' Initialization not completed.')
	    RETURN
	  ENDIF
	ENDIF


	ERRFLAG = 0

	RETURN 
	END

