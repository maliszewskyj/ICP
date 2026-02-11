#include "icp.h"
      	SUBROUTINE Init_oxmag(errflag)
C     	*******************************************
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C Send initialization strings to superconducting magnet                       C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
	use tc
	use smg_prep
	use magnmodel
	use iso_c_binding
      	IMPLICIT     none
	interface
#include "c_rs232_hc.h"
	end interface
	logical	     errflag, NOREPLY/.FALSE./
	character*80	command,reply
	integer*2	lcom, lreply, lstr
	character*4	str4
	character*20    str20
	integer*4       ios, pstatus, j
	logical		iserr, getreply/.TRUE./
	real*4		pshis, rval

	CALL putline(contr,' ')
	IF (magtype .eq. 4)THEN !SCmag with persistent switch
	   CALL putline(contr,' ')
	   CALL Read_STRING (keyb, str4, 
	1	'Is magnet using persistent mode? (T/F(default)): '
	2	, 4,j,j,j, lstr,j, contr) !j for 1   
	   IF (str4(1:1) .EQ. 'T' .OR. str4(1:1) .EQ. 't')THEN
	      hfmode = 1
	   ELSE
	      hfmode = 0
	   ENDIF
	   CALL putline(contr,' ')
	   str4(1:)=' '
	   CALL Read_STRING (keyb, str4, 
	1	'Is magnet connected & ready for initialization? (T/F): '
	2	, 4,j,j,j, lstr,j, contr) !j for 1   
	   IF (str4(1:1) .EQ. 'T' .OR. str4(1:1) .EQ. 't')THEN
C RWE Jan 99 skip ISTEP limit setup.  User discretion from front panel
C			  command(1:)='ISTP 10.0' !program output current step limit
C			  lcom = 9
C			  CALL oxmag232(command,lcom,reply,lreply,NOREPLY)
C			  command(1:)='ISTPS11' !status of output current step limit
C			  lcom = 7
C			  CALL oxmag232(command,lcom,reply,lreply,NOREPLY)
	      command(1:)='M9'	!set display units to tesla
	      lcom = 2
	      CALL oxmag232(command,lcom,reply,lreply,NOREPLY)
	      CALL putline(contr,
	1	   ' Use command HM=P or HM=NP to change persistance mode')
	      CALL putline(contr,' ')
	      errflag = .FALSE.
	   ELSE
	      CALL Putcontr('Magnet must be connected first')
	      errflag = .TRUE.
	      RETURN
	   ENDIF
	ENDIF			!magtype .eq. 4
	IF (magtype .EQ. 4 )THEN !SCmag persistent switch
	   command(1:)='X'	!get persistent switch heater status
	   lcom = 1
	   CALL oxmag232(command,lcom,reply,lreply,GETREPLY)
	   pstatus = 0		!default if error reading or decoding 0 = OFF
	   IF (lreply .NE. 0) READ(UNIT=reply(9:9),FMT=*,IOSTAT=ios)pstatus
	   IF (lreply .EQ. 0 .OR. ios .NE. 0 )THEN
	      CALL Putcontr(' Error reading in psh') !Debug
	      iserr = .TRUE.
	   ENDIF
	   IF (pstatus .EQ. 5)THEN ! switch heater fault
	      CALL Putcontr('Controller detects fault in switch heater')
	      iserr = .TRUE.
	   ELSE IF (pstatus .EQ. 8)THEN ! no switch fitted
	      CALL Putcontr('Controller indicates switch heater is not fitted')
	      iserr = .TRUE.
	   ELSE IF (pstatus .EQ. 0 .OR. pstatus .EQ. 2)THEN !off, H=0 or H.ne.0
	      command(1:)='R16' !get Current setting when PSH was turned off
	      lcom = 3
	      CALL oxmag232(command,lcom,reply,lreply,GETREPLY)
	      iserr = .FALSE.
	      IF (lreply .NE. 0 ) READ(UNIT=reply(2:lreply),FMT=*,IOSTAT=ios)pshis
	      IF (lreply .EQ. 0 .OR. ios .NE. 0 )THEN
		 CALL Putcontr('Err reading or decoding current sent from controller')
		 iserr = .TRUE.
	      ENDIF
	      IF (iserr)pshis = 0.
	      write(unit=str20,fmt=*,iostat=ios)pshis
	      CALL putline(contr,' ')
	      CALL Read_STRING (keyb, str4, 
	1	   'Does the current in magnet = '//str20//' amps? (T(default)/F): '
	2	   , 4,j,j,j, lstr,j, contr) !j for 1   
	      IF (str4(1:1) .NE. 'F' .AND. str4(1:1) .NE. 'f')THEN
		 fieldlast = pshis
	      ELSE		!not equal. ask user:
 100		 CALL putline(contr,' ')
		 CALL Read_STRING (keyb, str20, 
	1	      'What current is in the magnet (amps)? ', 
	2	      20,j,j,j, lstr,j, contr) !j for 1   
		 READ(unit=str20(1:lstr),fmt=*,err=100)rval
		 fieldlast = rval
	      ENDIF
	   ENDIF
	ENDIF
	RETURN
	END
