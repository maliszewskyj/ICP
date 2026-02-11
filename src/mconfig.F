#include "icp.h"
	SUBROUTINE MCONFIG
C	*********************
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C                                                                             C
C     Subroutine to select magnet configuration                          C
C     Called from: Parse_Contr                                                C
C                                                                             C
C     GAYLE GREENE, November, 1993                      C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
	use smg_prep
	use instr
	use tc
	use magnmodel
	use iso_c_binding
	IMPLICIT NONE
	interface
	   subroutine ICOILCONFIG(which,err) bind(C,name="ICOILCONFIG")
	   use iso_c_binding
	   integer(c_int), intent(in) ::which
	   integer(c_int), intent(out):: err
	   end subroutine ICOILCONFIG
#include "c_rs232_tc.h"
	end interface
	INTEGER*4    J,L,old_mot, iadr, idum,ierr
	CHARACTER*80 response
	CHARACTER*80 hconfig(7) !magnet controller
	CHARACTER*4  str4
	CHARACTER*10 str10
	CHARACTER*20 str20
	LOGICAL      errflag,ready
	CHARACTER*50 models(5)

	DATA hconfig/
     1	   '	    No:  1    ACS Motor Controller Unit',
     1	   '	    No:  2    Hewlett Packard Power Supply',
     1	   '	    No:  3    Superconducting magnet without persistent switch',
     1	   '	    No:  4    Superconducting magnet with persistent switch',
     1 	   '	    No:  5    Superconducting magnet supply, no voltage limit',
     1     '	    No:  6    12T Magnet                                     ',
     1     '	    No:  7    DAC-Controlled Electromagnet'/
	DATA models/
     1	   '	    No:  A    Lakeshore 612',
     1	   '	    No:  B    Lakeshore 622',
     1	   '	    No:  C    Oxford 120-10',
     1	   '	    No:  D    Lakeshore 421',
     1	   '        No:  E    ADAM 4021    '/
	IF (minit_done) THEN
	     CALL putline_NT (contr, 'Device #'// hconfig(magtype)(9:))
	     IF (magtype.eq.3 .OR. magtype .EQ. 4 .OR. magtype .EQ. 5)THEN
	       CALL putcontr(models(magmodel+1))
	       Write(str20,fmt='(f20.5)')hfconv
	       CALL putline_NT(contr,
     1	        '               Conversion constant (units/amp): '//str20)
	       IF (magtype .EQ. 4)CALL putline_NT(contr,
     1	        '                 (Field will be written to data file in tesla)')
	     ENDIF	
	     IF (magtype.eq.4)THEN
	       IF (hfmode.NE.0)THEN
	         response(1:)='               Controller is in Persistent Mode'
	       ELSE
	         response(1:)='               Controller is NOT in Persistent Mode'
	       ENDIF
	       CALL putline_NT(contr,response)
	     ELSEIF (magtype.eq.1) THEN
	       WRITE (UNIT= str4(1:2), FMT= '(i2)') ABS(tdev%mot)
	       response(1:) = 'Motor no. '// str4(1:2) //' assigned to magnet controller' 
	       CALL putline_NT (contr, response)
	     END IF
	     CALL READ_STRING (keyb, str4, 'Is this correct? (T/F): ', 4,
     1		idum,idum,idum, l,idum, contr) !idum is for 1   
	ELSE
	     str4(1:1)='F' !if no config defined
	ENDIF !minit_done
	IF (str4(1:1) .ne. 'T' .and. str4(1:1) .ne. 't') THEN !redefine
 211	        CALL putline_NF(contr, 
     1		 'Select one of the supported configurations listed below: ')
	        CALL putline_NF (contr, ' ')
	        DO j = 1,7
	          CALL putline_NF (contr, hconfig(j))
	        END DO
	        CALL putline_NF(contr, ' ')
	        CALL READ_STRING 
     1			(keyb, str4, 'Configuration no: (return to exit)',
     1			4,idum,idum,idum, l,idum, contr) !idum is for 1   
	        CALL SCROLL_DISPLAY (contr)
	        CALL CURSOR_ABS (contr, 19, 1) 
	        READ (UNIT=str4(1:l), FMT= *) j
	        IF (j .eq. 0) return !no action
	        IF (j .LT. 1 .OR. j .GT. 7) THEN
	          CALL putline_NT (contr, 'Invalid configuration selected, please reenter!')
	          CALL putline_NF (contr, ' ')
	          GOTO 211
	        ENDIF 
	        magtype = j
		IF (magtype .eq. 3 .or. magtype .eq. 4 .or. magtype .eq. 5) THEN
 311	          CALL putline_NF(contr, 
     1		   'Select one of the supported controllers listed below: ')
	          CALL putline_NF (contr, ' ')
	          DO j = 1,5
	            CALL putline_NF (contr, models(j))
	          END DO
	          CALL putline_NF(contr, ' ')
	          CALL READ_STRING 
     1			(keyb, str4, 'Model Choice (A/B/C/D/E): ',
     1			4,idum,idum,idum, l,idum, contr) !idum is for 1   
	          CALL SCROLL_DISPLAY (contr)
	          CALL CURSOR_ABS (contr, 19, 1) 
		  IF (str4(1:1) .eq. 'A' .or. str4(1:1) .eq. 'a')THEN
			magmodel = 0
		  ELSEIF (str4(1:1) .eq. 'B' .or. str4(1:1) .eq. 'b')THEN
			magmodel = 1
		  ELSEIF (str4(1:1) .eq. 'C' .or. str4(1:1) .eq. 'c')THEN
			magmodel = 2
		  ELSEIF (str4(1:1) .eq. 'D' .or. str4(1:1) .eq. 'd')THEN
			magmodel = 3
		  ELSEIF (str4(1:1) .eq. 'E' .or. str4(1:1) .eq. 'e')THEN
			magmodel = 4
	          ELSE
		    CALL putcontr (' Reply of A B or C was expected.')
		    GOTO 211
		  ENDIF
		ELSE IF (magtype .eq. 7) THEN
		  MAGMODEL=4
		ENDIF
		IF (magtype .eq. 1) THEN !using motor controller
 202		  CALL READ_STRING (keyb, str4, 
     1		    'Motor no. assigned to controller: ', 4,
     1		    idum,idum,idum, l,idum, contr) !idum is for 1   
	   	  CALL SCROLL_DISPLAY (contr)
	   	  CALL CURSOR_ABS (contr, 19, 1) 
	   	  READ (UNIT=str4(1:l), FMT= *) j
	          IF (j .eq. 0) goto 211 !invalid config
		  IF (j .GT. 10 .OR. j .LT. 1) THEN
	            CALL putline_NT (contr, 'Invalid motor no., please reenter!')
	      	    CALL putline_NF (contr, ' ')
	      	    GOTO 202
	          END IF 
	          tdev%mot = j
		ELSE IF (magtype .eq. 3 .OR. magtype .EQ. 4 .OR. magtype .EQ. 5) THEN !SC Mag go init port
	          tdev%mot = 0
		  IF (nsta.eq.2 .or. nsta.eq.9 .or. nsta.eq.-5 .or. nsta .eq. 5
     1			.OR. nsta.eq.7 .OR. nsta.eq.1 .OR. nsta .eq. -1.or.nsta.eq.-11) THEN
		      call SCport_init(errflag)
		      if (nsta .eq. -5) THEN
			 call ICOILCONFIG(magtype,ierr)
			 if (ierr .ne. 0) then
			    CALL putline_NT (contr, 'Error reading cancellation coil config')
			    CALL putline_NF (contr, ' ')
			 endif
		      ENDIF
		      IF (magmodel.eq.3) THEN
			 hfconv=1
			 minit_done=.true.
			 RETURN
		      ENDIF
		      CALL Init_SCmag(errflag)
		      IF (errflag)RETURN !magnet not initialized. Hdev must be redone
		      CALL Alert_On
		  ELSE
		      GOTO 211 !invalid for this instrument
		  ENDIF
	        ELSE IF (magtype .eq. 7) THEN
                   MAGMODEL=4
		   CALL SCPORT_INIT(errflag)
		   IF (.not.minit_done) THEN
		      hfconv=1
		   ENDIF
		   IF (errflag) RETURN
		   CALL Alert_On
                ELSE IF (magtype .eq. 6) THEN ! 12T magnet (labview)
		   CALL READ_STRING 	
     1			(keyb, str20, 'Sweep rate: ',
     1			4,idum,idum,idum, l,idum, contr) !idum is for 1   
		   IF (l.ne.0)read(str20(1:l),fmt=*,err=350)sweeprate
		   CALL READ_STRING 
     1			(keyb, str4, 'Persistent mode? (T/F): ',4,
     1			idum,idum,idum, l,idum, contr) !idum is for 1   
		   CALL putline_NF (contr, ' ')
		   read(unit=str4,fmt='(l1)',err=211)persist
                  tdev%mot = 0
		  call RS232TC_INIT(9600, 8, 0) ! 12 T Magnet
		  IF (nsta .eq. -5) THEN
		     call ICOILCONFIG(magtype,ierr)
		     if (ierr .ne. 0) then
			CALL putline_NT (contr, 'Error reading cancellation coil config')
			CALL putline_NF (contr, ' ')
		     endif
		  ENDIF
		ELSE IF (magtype .eq. 2) THEN !BT7 HP power supply controlled
		  tdev%mot = 0
		  if (nsta.eq. 7 .or. nsta .eq. -1 .or. nsta.eq.-5.or.nsta.eq.-11) then
	              tdev%mot = 0
		      IF (pols.EQ.0)call igpib !go init gpib port and devices
		      CALL putline_NT(contr,
     1			' Controller must be connected now & address set to 5')
	              CALL READ_STRING 
     1			(keyb, str4, 'Ready to continue? (T/F): ',4,
     1			idum,idum,idum, l,idum, contr) !idum is for 1   
	              CALL putline_NF (contr, ' ')
		      read(unit=str4,fmt='(l1)',err=211)ready
		      IF (.not.ready)goto 211
		      magport = 5 !magport will hold gpib (offset-1) from 1st ps of mag controller
		      CALL INIT_HPmag(errflag)
		      IF (nsta .eq. -5) THEN
			 call ICOILCONFIG(magtype,ierr)
			 if (ierr .ne. 0) then
			    CALL putline_NT (contr, 'Error reading cancellation coil config')
			    CALL putline_NF (contr, ' ')
			 endif
		      ENDIF
	          else
	            CALL putline_NT (contr, 'Invalid configuration selected, please reenter!')
	            CALL putline_NF (contr, ' ')
	            goto 211
		  endif
	       ENDIF !magtype
	     IF (magtype.ne.1) THEN   ! Magnet not used
C	      IF (old_mot .NE. 0 .AND. motfl(old_mot)) THEN        ! give motor 
C		 fixed(ABS(old_mot)) = .FALSE.                     ! back
C		 CALL MotPar (old_mot)
C	      END IF
	     ELSE                                            ! disable that motor
	      IF (motfl(ABS(tdev%mot))) fixed(ABS(tdev%mot)) = .TRUE.
C	      CALL Init_Auxiliary (tdev%mot, 1)            ! Initialize magnet
	     END IF
	   CALL Status_Line !write tdev to motors.buf
	ENDIF !redefine
 350	IF (magtype.eq.3 .OR. magtype .EQ. 4 .OR. magtype .EQ. 5) THEN !superconducting
	  Write(str20,fmt='(f20.5)')hfconv
	  CALL putline_NT(contr,
     1	   'Conversion constant (units/amp): '//str20)
	  IF (magtype .eq. 4)THEN !superconducting with persistent switch
	    CALL putline_NT(contr,
     1	   '  Standard: 877.7 gauss/amp symmetric   544 gauss/amp asymmetric')
	  ELSE !superconducting magnet without persistent switch
	    CALL putline_NT(contr,
     1	   '  Standard: 800 gauss/amp symmetric only')
	  ENDIF
	  CALL READ_STRING (keyb, str20, 
     1	    'Enter new constant (ex: 877.7 or 544) or return to keep current value: ', 
     1	      20,idum,idum,idum, l,idum, contr) !idum is for 1   
	  IF (l.ne.0)read(str20(1:l),fmt=*,err=350)hfconv
	  IF (hfconv.eq.0) THEN
	    CALL putline_NT(contr,' Zero not allowed. Use 1 if unknown.')
	    GOTO 350
	  ENDIF
	  IF (magtype .EQ. 4)CALL putline_NT(contr,
     1	        '          (Note: Field will be written to data file in tesla)')
	  CALL putline_NF(contr,' ')
 	ELSE IF (magtype .eq. 7) THEN
 375	   Write(str20,fmt='(f20.5)')hfconv
	  CALL putline_NT(contr,
     1	   'Conversion constant (units/volt): '//str20)
	  CALL READ_STRING (keyb, str20, 
     1	    'Enter new constant (ex: 1.000 or 544) or return to keep current value: ', 
     1	      20,idum,idum,idum, l,idum, contr) !idum is for 1   
	  IF (l.ne.0)read(str20(1:l),fmt=*,err=350)hfconv
	  IF (hfconv.eq.0) THEN
	    CALL putline_NT(contr,' Zero not allowed. Use 1 if unknown.')
	    GOTO 375
	  ENDIF	   
	ENDIF
 450	IF (magtype.eq.3 .OR. magtype .EQ. 4 .OR. magtype .EQ. 5) THEN !superconducting
	  Write(str10,fmt='(f10.2)')fieldmax !fieldmax is in amps
	  CALL putline_NT(contr,
     1	   'Maximum current allowed (amps): '//str10)
	  CALL READ_STRING (keyb, str10, 
     1	    'Enter new maximum or return to keep current value: ', 
     1	     10,idum,idum,idum, l,idum, contr) !idum is for 1   
	  IF (l.ne.0)read(str10(1:l),fmt=*,err=350)fieldmax
	  fieldmax = abs(fieldmax)
	  IF (fieldmax.GT.130.) THEN
	    CALL putline_NT(contr,' Max field is absolute value up to 130.')
	    GOTO 450
	  ENDIF
	  CALL putline_NT(contr,' ')
	ENDIF
	minit_done = .true.
	RETURN
	ENTRY PMCONFIG !Print magnet configuration
	CALL putline_NT (contr,'Device #'// hconfig(magtype)(9:)) 
	IF (magtype .eq. 1) THEN !MCU
	      WRITE (UNIT= str4(1:2), FMT= '(i2)') ABS(tdev%mot)
	      response(1:) = 'Motor no. '// str4(1:2) //' assigned to magnet controller' 
	      CALL putline_NT (contr, response)
	ENDIF
	IF (magtype.eq.3 .OR. magtype .eq. 4 .OR. magtype .eq. 5) THEN !superconducting
	  CALL putcontr(models(magmodel+1))
	  Write(str20,fmt='(f20.5)')hfconv
	  CALL putline_NT(contr,
     1	   '             Conversion constant (units/amp): '//str20)
	  IF (magtype .EQ. 4)CALL putline_NT(contr,
     1	   '                 (Field will be written to data file in tesla)')
	     IF (magtype .eq. 4)THEN
	       IF (hfmode.NE.0)THEN
	         response(1:)='               Controller is in Persistent Mode'
	       ELSE
	         response(1:)='               Controller is NOT in Persistent Mode'
	       ENDIF
	       CALL putline_NT(contr,response)
	     ENDIF
	  Write(str10,fmt='(f10.2)')fieldmax
	  CALL putline_NT(contr,
     1	   '             Maximum current: '//str10//' amps')
	ENDIF
	RETURN
	end
