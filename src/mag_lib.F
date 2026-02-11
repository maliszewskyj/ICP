#include "icp.h"
C Mag_lib: magnet subroutines
C  Gayle Greene, Mar 1994
      	SUBROUTINE Shut_mag
C     	*******************************************
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C	Shut down magnet controller                                           C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
	use tc
	use instr
	use magnmodel
	use iso_c_binding
      	IMPLICIT     none
	interface
#include "c_rs232_hc.h"
#include "c_gpiben.h"
	end interface
	CHARACTER*80  reply,command
	INTEGER*4	lcom,lreply
	integer*4     NOREPLY/0/,GETREPLY/1/

	IF (magtype .eq. 4 .OR. magtype .eq. 3 .or. magtype .eq. 5) THEN !White superconducting magnet only. Right Ross
	   IF (magmodel .ne. 2) THEN !Lakeshore Models 612,622
C NO gayle both types of SC magnets and do not bother with the persisitent mode switch
C	    command(1:)='PSH11' !Turn on Persistent Switch Heater
C	    lcom = 5
C	    CALL sc232(command,lcom,reply,lreply,NOREPLY)
		 command(1:)='ISET 0.' !set output current to zero
		 lcom = 6
		 CALL sc232(command,lcom,reply,lreply,NOREPLY)
	   ELSE				  !Oxford Model IPS120-10
		 command(1:)='A2'	  !set output current to zero
		 lcom = 2
		 CALL oxmag232(command,lcom,reply,lreply,GETREPLY)
	   ENDIF
	ENDIF
	minit_done = .false.
	magnctr = .false.
	RETURN
	END

      	SUBROUTINE IniMag
C     	*******************************************
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C	Set magnet controller to read 0                                       C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
	use instr
	use tc
	use iso_c_binding
      	IMPLICIT     none
	interface
#include "c_rs232_acs.h"
	end interface
	CHARACTER*6  str6
	CHARACTER*80  response
	INTEGER*4     i

	IF (magtype .eq. 1) THEN
	  IF (tdev%mot .NE. 0)THEN
	      WRITE (UNIT=str6, FMT= '(i2.2,a4)') tdev%mot, 'P=+0'
	      CALL RS232 (str6, 6, response, i)
	  ENDIF
c	ELSEIF (magtype .eq. 2) THEN !HCmag
c	ELSEIF (magtype .eq. 3) THEN !HP/PS
	ENDIF
	RETURN
	END

	SUBROUTINE SCport_INIT(errflag)
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
c    Set up RS232 line to characteristics required by magnet controller  C
c    Assign channel to this line after giving process    		 C
c    neccessary privilege						 C
c									 C
C    Gayle Greene, July, 1993 						 C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C
	use instr
	use tc
	use magnmodel
	implicit none
	interface
#include "c_rs232_hc.h"
	end interface
c
	integer*4 status,magline
	character*4 magstr
	integer*4 errflag

	IF (magmodel .eq.0) then       ! Lakeshore 612
	   CALL RS232HC_INIT(300, 7, 2, 1)
	ELSE IF (magmodel .eq. 1) then ! Lakeshore 622
	   CALL RS232HC_INIT(9600, 8, 0, 2)
        ELSE IF (magmodel .eq. 2) then ! Oxford IPS120-10
	   CALL RS232HC_INIT(9600, 8, 0, 2)
	ELSE IF (magmodel .eq. 3) then ! Lakeshore 421
	   CALL RS232HC_INIT(9600, 7, 1, 1)
	ELSE IF (magmodel .eq. 4) then ! ADAM 4021 DAC
	   CALL RS232HC_INIT(9600, 8, 0, 1) 
        ELSE 
	   call putcontr(' magnet model not recognized')
	   ERRFLAG=1
	ENDIF

	ERRFLAG = 0
	RETURN 
	END

	SUBROUTINE Init_HPmag (errflg)
C     	****************************
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C                                                                             C
C     Routine to initialize HP magnet controller                              C
C        via CAMAC GBIP module                                                C
C	Magport in tc.cmn holds the (offset-1) of magnet controller from      C
C        1st power supply used on instrument.                                 C
C	 (ie: if psno1 used gpib adr 1 and magn contr uses 5, magport has 5)  C
C									      C
C     Gayle Greene, January 1994					      C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
	use tc
	IMPLICIT    none
     	INTEGER*2  hpdev/0/ !HP format will be used in comm with mag controller
	REAL*4     amps,volts, tamps, vlimit, I0max, V0max/20./
	CHARACTER*20 outstr
	INTEGER*4   lstr,iamps,vlim
	LOGICAL     errflg, highamps
	byte	    blf/10/,bcr/13/
	character*1 cr,lf
	equivalence (lf,blf),(cr,bcr)


	CALL Gpib_Listen(magport, errflg) ! set up controller to listen
	outstr(1:)=';FOLD CV;'
	lstr = 9
	CALL WrPol(outstr, lstr, errflg,hpdev)
	outstr(1:)=';VSET 27V;'
	lstr = 10
	CALL WrPol(outstr, lstr, errflg,hpdev)
	outstr(1:)=';IMAX 15A;'
	lstr = 10
	CALL WrPol(outstr, lstr, errflg,hpdev)
	outstr(1:)=';VMAX 30V;'
	lstr = 10
	CALL WrPol(outstr, lstr, errflg,hpdev)
	CALL Gpib_Unlisten(errflg)
	goto 999
C------ Error treatment
 900	call putcontr(' Error: volt or current value out of range')
 	CALL Gpib_Unlisten(errflg)
 999	return
	END

      	SUBROUTINE RdField (field) 
C     	*******************************************
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C	Read current field value at controller (& return field val in amps)   C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
	use instr
	use tc
	use magnmodel
	use iso_c_binding
      	IMPLICIT     none
	interface
#include "c_rs232_hc.h"
#include "c_rs232_tc.h"
	end interface
	REAL	     field, volt
	CHARACTER*9  newstr
	CHARACTER*80  command,reply
	INTEGER*4     lcom,lreply
     	INTEGER*2  hpdev/0/ !used in comm with HP mag controller
	CHARACTER*20  outstr
	INTEGER*4     lstr,status
	INTEGER*4     errflg, GETREPLY/1/

	IF (magmodel .eq. 3) THEN
	     command(1:)='FIELD?'
	     lcom = 6
	     CALL RS232HC_SC(command,lcom,reply,lreply,status,GETREPLY)
	     IF (lreply.ne.0) THEN
		read(unit=reply(1:lreply),fmt=*,err=85)field ! Read current
	     ELSE
		call putcontr(' No reply reading field')
		field = -999.9
             ENDIF
	     IF (field .ne. -999.0) THEN
		hfconv = 1
		command(1:)='FIELDM?'
		lcom = 7
		CALL RS232HC_SC(command,lcom,reply,lreply,status,GETREPLY)
		IF (reply(1:1).eq.'m') THEN
		   hfconv = 0.001
		ELSEIF (reply(1:1).eq.'u') THEN
		   hfconv = 0.000001
		ELSEIF (reply(1:1).eq.'k') THEN
		   hfconv = 1000
		ENDIF
		field = field * hfconv		   
	     ENDIF
	ELSEIF (magtype .eq. 4) THEN !SCmag with persistent switch
	     IF (magmodel .ne. 2) THEN
C Ross used to read the field sensor. Assume we don't have one
C	    command(1:)='BFLD?'
C	    lcom=5
C Read the current set when the persistent switch heater was turned off
	    command(1:)='PSHIS?'
	    lcom=6
	    call sc232(command,lcom,reply,lreply,GETREPLY)
	    IF (lreply.ne. 0)THEN
	      read(unit=reply(1:lreply),fmt=*,err=85)field
	    ELSE !no reply from controller
	      call putcontr(' No reply reading field')
	      field = -999.9
	    ENDIF
	  ELSE 
	     command(1:)='R16'	!Read back current 
	     lcom = 3
	     CALL oxmag232(command,lcom,reply,lreply,GETREPLY)
	     field =-999.9
	     IF (lreply .NE. 0) READ(UNIT=reply(2:lreply),FMT=*,
	1	  err=85)field
	     field = field * hfconv
!field cannot be read back from Oxford

	  ENDIF
	ELSEIF (magtype .eq. 3) THEN !SCMag, no persistent switch
	  IF (magmodel .ne. 2) THEN
	     command(1:)='IOUT?'
	     lcom=5
	     call sc232(command,lcom,reply,lreply,GETREPLY)
	     IF (lreply.ne.0) THEN
		read(unit=reply(1:lreply),fmt=*,err=85)field ! Read current
		field = field * hfconv
	     ELSE
		call putcontr(' No reply reading current')
		field = -999.9
             ENDIF
          ELSE
	     command(1:)='R0'	!Read back current 
	     lcom = 2
	     CALL oxmag232(command,lcom,reply,lreply,GETREPLY)
	     field =-999.9
	     IF (lreply .NE. 0) READ(UNIT=reply(2:lreply),FMT=*,
	1	  err=85)field
	     field = field * hfconv 
	  ENDIF
	ELSEIF (magtype .eq. 2) THEN !HP power supply - get volts
	  CALL Hpvolt(reply,lreply)
	  IF (lreply.gt. 0)THEN
	    read(unit=reply(6:lreply),fmt=*,err=95)field
	  ELSE !no reply from controller
c	    call putcontr(' No reply reading voltage')
	    field = -999.9
	  ENDIF
	ELSEIF (magtype .eq. 6) THEN ! 12T magnet
	  command(1:)='GETMAG'
	  lcom=6
	  call rs232tc_lv(command,lcom,reply,lreply,1)
	  IF (lreply.gt.0) THEN
	     read(unit=reply(1:lreply),fmt=*,err=95)field
	  ELSE
	     field = -999.9
          ENDIF
	ELSEIF (magtype .eq. 7) THEN ! Electromagnet
	  call ADAM_GETVOLT(volt,status)
	  if (status.ne.0) then
	     field = hfconv * volt;
	  else
	     field = -999.9
	  endif
	ELSE !field cannot be read back
	  call putcontr(' Field cannot be read for this type magnet')
	  field=-999.9
	ENDIF
	RETURN
 85	call putcontr(' Err decoding field reply from controller')
 95	field = -999.9
	RETURN
	END

      	SUBROUTINE Init_SCmag(errflag)
C     	*******************************************
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C Send initialization strings to superconducting magnet                       C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
	use tc
	use smg_prep
	use magnmodel
      	IMPLICIT     none
	integer*4       errflag, NOREPLY/0/,getreply/1/
	character*80	command,reply
	integer*4	lcom, lreply, lstr
	character*4	str4
	character*20    str20
	integer*4       ios, pstatus, j
	logical		iserr
	real*4		pshis, rval

	IF (magmodel .eq. 2) THEN !oxford
	  rmag1 = 0.
	  call init_oxmag(errflag)
	  return
	endif

	CALL putline(contr,' ')
	CALL Read_STRING (keyb, str4, 
     1		'Should helium level be checked? (T/F(default)): '
     1			   , 4,j,j,j, lstr,j, contr)  !j for 1   
	IF (str4(1:1) .EQ. 'T' .OR. str4(1:1) .EQ. 't')THEN
		           rmag1 = 1.
	ELSE
			   rmag1 = 0.
	ENDIF
	IF (magtype .eq. 4)THEN !SCmag with persistent switch
		        CALL putline(contr,' ')
	     		CALL Read_STRING (keyb, str4, 
     1			  'Is magnet using persistent mode? (T/F(default)): '
     1			   , 4,j,j,j, lstr,j, contr)  !j for 1   
			IF (str4(1:1) .EQ. 'T' .OR. str4(1:1) .EQ. 't')THEN
		           hfmode = 1
			ELSE
			   hfmode = 0
		        ENDIF
		        CALL putline(contr,' ')
		        str4(1:)=' '
	     		CALL Read_STRING (keyb, str4, 
     1			  'Is magnet connected & ready for initialization? (T/F): '
     1			   , 4,j,j,j, lstr,j, contr)  !j for 1   
C			call putcontr('reply***'//str4//'****')
c			str4(1:1)='T'
			IF (str4(1:1) .EQ. 'T' .OR. str4(1:1) .EQ. 't')THEN
C RWE Jan 99 skip ISTEP limit setup.  User discretion from front panel
C			  command(1:)='ISTP 10.0' !program output current step limit
C			  lcom = 9
C			  CALL sc232(command,lcom,reply,lreply,NOREPLY)
C			  command(1:)='ISTPS11' !status of output current step limit
C			  lcom = 7
C			  CALL sc232(command,lcom,reply,lreply,NOREPLY)

C     Turn off ISTPS because of bugs in Lakeshore 622 implementation
			   IF( magmodel .EQ. 1 )THEN ! Lakeshore 622
				 command(1:)='ISTPS0' !status of output current step limit
				 lcom = 7
				 CALL sc232(command,lcom,reply,lreply,NOREPLY)
			   ENDIF
			   
			  command(1:)='BUNI T' !program field units to tesla
			  lcom = 6
			  CALL sc232(command,lcom,reply,lreply,NOREPLY)
			  command(1:)='LUNI %' !program level units to percent
			  lcom = 6
			  CALL sc232(command,lcom,reply,lreply,NOREPLY)
		          CALL putline(contr,
     1		           ' Use command HM=P or HM=NP to change persistance mode')
		          CALL putline(contr,' ')
			  errflag = 0
			ELSE
			  CALL Putcontr('Magnet must be connected first')
			  errflag = 1
			  RETURN
			ENDIF
	ENDIF !magtype .eq. 4
	IF (magtype .EQ. 4 )THEN !SCmag persistent switch
	    command(1:)='PSH?' !get persistent switch heater status
	    lcom = 4
	    CALL sc232(command,lcom,reply,lreply,GETREPLY)
	    pstatus = 0 !default if error reading or decoding 0 = OFF
	    IF (lreply .NE. 0)THEN
	      READ(UNIT=reply(1:lreply),FMT=*,IOSTAT=ios)pstatus
	    ELSE
	      CALL Putcontr(' Error reading in psh') !Debug
	    ENDIF
	    IF (pstatus .EQ. 0)THEN
	      command(1:)='PSHIS?' !get Current setting when PSH was turned off
	      lcom = 6
	      CALL sc232(command,lcom,reply,lreply,GETREPLY)
	      iserr = .FALSE.
	      IF (lreply .EQ. 0)THEN
		CALL Putcontr('Err reading current back from controller')
		iserr = .TRUE.
	      ELSE
	        READ(UNIT=reply(1:lreply),FMT=*,IOSTAT=ios)pshis
		IF (ios .NE.0)THEN
		  iserr = .TRUE.
		  CALL Putcontr('Err decoding current sent from controller')
		ENDIF
	      ENDIF
	      IF (iserr)pshis = 0.
	      write(unit=str20,fmt=*,iostat=ios)pshis
	      CALL putline(contr,' ')
	      CALL Read_STRING (keyb, str4, 
     1		'Does the current in magnet = '//str20//' amps? (T(default)/F): '
     1			   , 4,j,j,j, lstr,j, contr) !j for 1   
	      IF (str4(1:1) .NE. 'F' .AND. str4(1:1) .NE. 'f')THEN
	        fieldlast = pshis
	      ELSE !not equal. ask user:
 100		CALL putline(contr,' ')
	        CALL Read_STRING (keyb, str20, 
     1		  'What current is in the magnet (amps)? ', 
     1		   20,j,j,j, lstr,j, contr)  !j for 1   
		READ(unit=str20(1:lstr),fmt=*,err=100)rval
		fieldlast = rval
	      ENDIF
	    ENDIF
	ENDIF
C	write(unit=str20,fmt=*,iostat=ios)fieldlast
C	CALL Putcontr('Using current field of: '//str20)
	RETURN
	END

      	SUBROUTINE Upd_Helium
C     	*******************************************
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C Tell superconducting magnet to update the helium level reading              C
C  WARNING: The update process takes 5 seconds.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
	use tc
	use magnmodel
      	IMPLICIT       none
	CHARACTER*80  reply,command
	INTEGER*4	lcom,lreply
	INTEGER*4	NOREPLY/0/

	IF (rmag1 .eq. 1.) THEN
	  command(1:)='LEVL1'
	  lcom = 5
	  CALL sc232(command,lcom,reply,lreply,NOREPLY)
	ENDIF
	RETURN
	END

       	SUBROUTINE Read_Helium(hlevel,errflag)
C     	*******************************************
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C Ask superconducting magnet result of last helium level reading              C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
	use tc
	use magnmodel
      	IMPLICIT       none
	REAL*4		hlevel
	INTEGER*4	errflag, GETREPLY/1/
	CHARACTER*80  reply,command
	INTEGER*4	lcom,lreply
	
	hlevel = -999.9
	IF (rmag1 .eq. 1.) THEN
	  errflag = 0
	  command(1:)='LEVL?'
	  lcom = 5
	  CALL sc232(command,lcom,reply,lreply,GETREPLY)
	  IF (lreply .EQ. 0)GOTO 300 !error reading reply
	  read(unit=reply(1:lreply),fmt=*,err=300)hlevel
	ENDIF
	errflag = 0
	RETURN
 300	errflag = 1
	RETURN
	END

       	SUBROUTINE Alert_On
C     	*******************************************
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C Enable Helium Alert                                                         C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
	use tc
	use magnmodel
      	IMPLICIT       none
	CHARACTER*80  reply,command
	INTEGER*4	lcom,lreply
	INTEGER*4	NOREPLY/0/
	IF (rmag1 .eq. 1.) THEN
	    command(1:)='LALMS1'
	    lcom = 6
	    CALL sc232(command,lcom,reply,lreply,NOREPLY)
	ENDIF
	RETURN
	END

       	SUBROUTINE Check_Alarm(triggered,errflag)
C     	*******************************************
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C Ask superconducting magnet if alarm triggered                               C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
	use tc
	use magnmodel
      	IMPLICIT       none
	LOGICAL		triggered
	LOGICAL		errflag
	CHARACTER*80  reply,command
	INTEGER*4	lcom,lreply
	integer*4	ival
	integer*4	GETREPLY/1/

	triggered = .FALSE.
	IF (rmag1 .eq. 1.) THEN
	  command(1:)='LALMS?'
	  lcom = 6
	  CALL sc232(command,lcom,reply,lreply,GETREPLY)
	  IF (lreply .EQ. 0)GOTO 300 !error reading reply
	  ival = 0 !default
	  read(unit=reply(1:lreply),fmt=*,err=300)ival
	  IF (ival .EQ. 1)triggered = .TRUE.
	ENDIF
	errflag = .FALSE.
	RETURN
 300	errflag = .TRUE.
	RETURN
	END

C Operate SPINS Field Cancellation Coils
C
C
	SUBROUTINE CANCELFIELD(ERRFLAG)
	use instr
	use iso_c_binding
	implicit none
	interface
#include "c_gpiben.h"
	   subroutine CANCELCOIL(field, energy, err) bind(C,name="CANCELCOIL")
	   use iso_c_binding
	   real(c_float) ,intent(in)::field
	   real(c_float) ,intent(in)::energy
	   integer(c_int),intent(out)::err
	   end subroutine CANCELCOIL
	end interface
	INTEGER*4 ERRFLAG
	REAL FIELD
	REAL ENERGY, POS2, POS1, Z2
	REAL RDANGL, FZ
	REAL*4 Angle_Energy

	CALL ENET_INIT ! Call just in case controller was not initialized

	CALL RDFIELD(FIELD)
	POS2 = RDANGL(2)
	Z2   = FZ(2,POS2)
	POS2 = POS2 - Z2
	POS1 = POS2 / 2
	
	ENERGY = Angle_Energy(POS1,DM)
C	print *,' CANCELFIELD(): H2 = ',POS2,' ENERGY = ',ENERGY
	CALL CANCELCOIL(FIELD, ENERGY, ERRFLAG)
	
	
	RETURN
	END
