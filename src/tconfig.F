#include "icp.h"
	SUBROUTINE TCONFIG
C	*********************

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C                                                                             C
C     Subroutine to select temperature configuration                          C
C     Called from: Parse_Contr                                                C
C                                                                             C
C     GAYLE GREENE, (Based on B. Schmidt) FEBRUARY, 1993                      C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C

	use instr
	use smg_prep
	use tc
	IMPLICIT NONE

	INTEGER*4    J,L,nopnts,itemno,curveno,i,index,irange
	CHARACTER*80 response,curvefile,inbuf
	CHARACTER*50 lconfig(25),lvsens(5)
	CHARACTER*4  str4
	LOGICAL      errflag
	CHARACTER*20 cryo,sensorid,standard
	CHARACTER*20 hldcryo,hldid,hldstand
	CHARACTER*3  hlditem
	CHARACTER*4  itemstr
	CHARACTER*8  whichstr(3)/'Sample','Control','Setpoint'/
	CHARACTER*7  str7
	CHARACTER*80 GETHOME

	DATA lconfig /
	1    '        No:  1    LAKESHORE DRC 82C        ',
	2    '        No:  2    LAKESHORE DRC 93C        ',
	9    '        No:  3    LAKESHORE     330        ',
	3    '        No:  4    LINEAR RESEARCH LR400    ',
	1    '        No:  5    OMEGA                    ',
	1    '        No:  6    NESLAB                   ',
	1    '        No:  7    LR700                    ',
	1    '        No:  8    NEW NESLAB               ',
	1    '        No:  9    OMEGA 8500               ',
	1    '        No: 10    LAKESHORE 331 or 340     ',
	1    '        No: 11    EUROTHERM FURNACE        ',
	1    '        No: 12    DIGITAL TRANSDUCER AMP   ',
	2    '        No: 13    SR830 LOCK IN AMP        ',
	1    '        No: 14    AD201                    ',
	1    '        No: 15    OXFORD                   ',
	1    '        No: 16    LTC-21                   ',
	1    '        No: 17    SHEAR CELL               ',
	1    '        No: 18    OMEGA 6070               ',
	2    '        No: 19    12 T Magnet              ',
	2    '        No: 20    OMEGA 3200 Series        ',
	2    '        No: 21    Cryocon Model 34         ',
	2    '        No: 22    Thermo RTE-7 bath        ',
	3    '        No: 23    OMEGA CNi Series         ',
	3    '        No: 24    Lakeshore 370 Resistance ',
	4    '        No: 25    Petite Fleur Bath        '/

	DATA lvsens /
	1    '      0: STILL',
	2    '      1: COLD P',
	3    '      2: MC TOP',
	4    '      3: CERNOX [Ohm]',
	5    '      4: MC SLG'/

	IF (tinit_done) THEN
	   CALL Putline_NF (contr, 'Current device: ')
	   CALL Putline_NF (contr, lconfig(temptype)(9:)) 
	   IF (temptype .lt.3 )THEN
	      response(1:)=' Storing '//whichstr(which_temp)//' in data file'
	      CALL Putline_NF (contr,response)
	   ELSEIF (temptype .eq.4 .OR. temptype .EQ. 7)THEN
	      IF (tdev%cfg .EQ. temptype)THEN
		 response(1:)=' Using sensor '//snsname(1:7)
	      ELSE
		 response(1:)=' Using resistance'
	      ENDIF
	      CALL Putline_NF (contr,response)
	   ENDIF
	   CALL Read_STRING (keyb, str4, 'Exit with no changes? (T/F): ', 
     1		4,j,j,j, l,j, contr)   
	ELSE
	   str4(1:1)='F'	!if no config defined
	ENDIF			!tinit_done
	IF (str4(1:1) .ne. 'T' .and. str4(1:1) .ne. 't') THEN 
 201	   CALL Putline_NF (contr,
	1	'Select one of the supported configurations listed below: ')
	   CALL Putline_NF (contr, ' ')
	   DO j = 1, 	25
	      CALL Putline_NF (contr, lconfig(j))
	   END DO
	   CALL Putline_NF (contr, ' ')
	   CALL Read_STRING (keyb, str4, 'Configuration no: (return to exit)',
	1	4,j,j,j, l,j, contr) !j is dummy param for absoft  
	   CALL SCROLL_DISPLAY (contr)
	   CALL CURSOR_ABS (contr, 19, 1) 
	   READ (UNIT=str4(1:l), FMT=*) j
	   IF (j .eq. 0) return !no action

	   IF (j .LT. 0 .OR. j .GT. 25)THEN
	      CALL Putline_NF (contr, 'Invalid configuration selected, please reenter !')
	      CALL Putline_NF (contr, ' ')
	      GOTO 201
	   ELSE 
	      IF (tcareful .AND. (tdev%cfg .LE. 3 .OR. tdev%cfg .EQ. 10))
	1	   CALL Putline_NF (contr,
	2	   ' Use control mode command TC+ if you want to reenter careful mode')
	      tcareful = .FALSE.
	      tdev%cfg = j
	      temptype = tdev%cfg
	   END IF
	   CALL Status_Line	!write tdev to motors.buf
	ELSE			!exit with no changes
	   RETURN
	ENDIF			!redefine

	CALL Temp_Init		!set up port for specified controller
C       Perform any controller specific initializations
	which_temp = 1		!default for all controllers
	IF (temptype .eq. 4
	1    .OR. temptype .eq. 7
	1    .OR. temptype .eq. 11
	2    .OR. temptype .eq. 12
	2    .OR. temptype .eq. 13
	3    .OR. temptype .eq. 14
	3    .OR. temptype .eq. 15
	4    .OR. temptype .eq. 16) then !LR furnace etc
 250	   response(1:)='This controller must be initialized by Tdev%'
	   CALL Putline_NF(contr,response)
	   response(1:)='If it is not connected, you must run Tdev when it is.'
	   CALL Putline_NF(contr,response)
	   CALL Read_STRING (keyb, str4, 
	1	'Is the controller connected for initialization? (T/F): ',
	1	4,j,j,j, l,j, contr) !j is dummy param for absoft   
	   IF (str4(1:1) .eq. 'T' .or. str4(1:1) .eq. 't') THEN
	      IF (temptype .eq. 4)THEN
		 CALL init_lr400(errflag)
	      ELSEIF (temptype .eq. 7)THEN
		 CALL init_lr700(errflag)
	      ELSEIF (temptype .eq. 11)THEN
		 CALL init_furnace(errflag)
	      ELSEIF (temptype .eq. 12)THEN
		 CALL init_tdb(errflag)
	      ELSEIF (temptype .eq. 13)THEN
		 CALL init_lia(errflag)
	      ELSEIF (temptype .eq. 14)THEN
		 CALL init_ad201(errflag)
	      ELSEIF (temptype .eq. 15)THEN
		 CALL init_oxford(errflag)
	      ELSEIF (temptype .eq. 16)THEN
		 CALL init_ltc(errflag)
	      ENDIF
	      IF (errflag) THEN
		 response(1:) = '*****Error initializing temp controller.*****'
		 CALL Putline_NF(contr,response)
		 GOTO 250
	      ELSE		!no error
		 response(1:) = 'Temp controller initialized.'
		 CALL Putline_NF(contr,response)
		 IF (temptype .EQ. 4)rangelr = 200 !read back for lr700 from controller
		 IF (temptype .EQ. 4 .or. temptype .EQ. 7)excitlr = .06
	      ENDIF
	      IF (temptype .EQ. 4
	1	   .or. temptype .EQ. 7)
	2	   CALL Putline_NF(contr,'Put and leave controller on remote.')
	      IF (temptype .EQ. 4)
	1	   CALL Putline_NF(contr,'Do SETP0 range, excitation before using.') 
	   ELSE			!controller not connected
	      dbcurve = .false.
	      tinit_done = .false.
	      response(1:) = 'Repeat TDEV once controller is connected.'
	      CALL Putline_NF(contr,response)
	      CALL Putline_NF(contr,' ')
	      RETURN
	   ENDIF
        ELSEIF (temptype .eq. 19) THEN
	   samp_sensor = 2
	   DO j = 1,5
	      CALL Putline_NF (contr, lvsens(j))
	   END DO
	   CALL Read_STRING (keyb, str4,
	1	' Enter sensor to be used for sample temp [default=2]: ',
	1	4,j,j,j, l,j, contr) 
	   if (l.eq.0) then
	      j=2
	   else
	      read(unit=str4,fmt=*)j
	      if (j.ge.0 .and. j.le.4) samp_sensor=j
	   endif
	   tdev%cfg = j
	ELSEIf (temptype .le. 3 .or. temptype .eq. 10.or.temptype.eq.22.or.temptype.eq.21)THEN
	   IF (temptype .eq. 10.or.temptype.eq.21) THEN
 271	      CALL Read_STRING (keyb, str4, 
	1	   ' Enter sensor to be used for sample temp (A/B/C/D)(default=A): ',
	2	   4,j,j,j, l,j, contr) !j is dummy param for absoft
	      IF (l .eq. 0) THEN
		 samp_sensor = 1
	      ELSE   
	         IF (str4(1:1) .eq. 'A' .or. str4(1:1) .eq. 'a') THEN
		    samp_sensor = 1
	         ELSEIF (str4(1:1) .eq. 'B' .or. str4(1:1) .eq. 'b') THEN
		    samp_sensor = 2
	         ELSEIF (str4(1:1) .eq. 'C' .or. str4(1:1) .eq. 'c') THEN
		    samp_sensor = 3
	         ELSEIF (str4(1:1) .eq. 'D' .or. str4(1:1) .eq. 'd') THEN
		    samp_sensor = 4
	         ELSE
		    CALL Putline_NF(contr,' Please enter A, B, C, or D ')
		    GOTO 271
	         ENDIF
	      ENDIF
 272	      CALL Read_STRING (keyb, str4, 
	1	   ' Enter sensor to be used for control temp (A/B/C/D)(default=B): ',
	2	   4,j,j,j, l,j, contr) !j is dummy param for absoft   
	      IF (l .eq. 0) THEN
		 contr_sensor = 2
	      ELSE
	         IF (str4(1:1) .eq. 'A' .or. str4(1:1) .eq. 'a') THEN
		    contr_sensor = 1
	         ELSEIF (str4(1:1) .eq. 'B' .or. str4(1:1) .eq. 'b') THEN
		    contr_sensor = 2
	         ELSEIF (str4(1:1) .eq. 'C' .or. str4(1:1) .eq. 'c') THEN
		    contr_sensor = 3
	         ELSEIF (str4(1:1) .eq. 'D' .or. str4(1:1) .eq. 'd') THEN
		    contr_sensor = 4
	         ELSE
		    CALL Putline_NF(contr,' Please enter A, B, C, or D ')
		    GOTO 272
	         ENDIF
	      ENDIF
	   ELSEIF (temptype.eq.22) THEN
 273	      CALL Read_STRING (keyb, str4, 
	1	   ' Enter sensor to be used for sample temp '//
	2	   '(I=internal/X=external) (default=I): ',
	3	   4,j,j,j, l,j, contr) !j is dummy param for absoft
	      IF (l .eq. 0) THEN
		 samp_sensor = 1
	      ELSE   
	         IF (str4(1:1) .eq. 'I' .or. str4(1:1) .eq. 'i') THEN
		    samp_sensor = 1
	         ELSEIF (str4(1:1) .eq. 'X' .or. str4(1:1) .eq. 'x') THEN
		    samp_sensor = 2
	         ELSE
		    CALL Putline_NF(contr,' Please enter I or X ')
		    GOTO 273
	         ENDIF
	      ENDIF
 274	      CALL Read_STRING (keyb, str4, 
	1	   ' Enter sensor to be used for control temp '//
	2	   '(I=internal/X=external) (default=I): ',
	3	   4,j,j,j, l,j, contr) !j is dummy param for absoft
	      IF (l .eq. 0) THEN
		 contr_sensor = 1
	      ELSE   
	         IF (str4(1:1) .eq. 'I' .or. str4(1:1) .eq. 'i') THEN
		    contr_sensor = 1
	         ELSEIF (str4(1:1) .eq. 'X' .or. str4(1:1) .eq. 'x') THEN
		    contr_sensor = 2
	         ELSE
		    CALL Putline_NF(contr,' Please enter I or X ')
		    GOTO 274
	         ENDIF
	      ENDIF
	   ENDIF
 275	   CALL Putline_NF(contr,
     1		' You may select which temperature is stored in data file: ')
	   CALL Putline_NF(contr,
     1		'   S: Sample Temperature (default)')
	   CALL Putline_NF(contr,
     1		'   C: Control Temperature')
	   CALL Putline_NF(contr,
     1		'   P: Setpoint ')
	   CALL Read_STRING (keyb, str4, 
	1	' Use letter indicated above to make choice (S/C/P): ',
	2	4,j,j,j, l,j, contr) !j is dummy param for absoft   
	   IF (str4(1:1) .eq. 'S' .or. str4(1:1) .eq. 's') THEN
	      which_temp = 1
	   ELSEIF (str4(1:1) .eq. 'C' .or. str4(1:1) .eq. 'c') THEN
	      which_temp = 2
	   ELSEIF (str4(1:1) .eq. 'P' .or. str4(1:1) .eq. 'p') THEN
	      which_temp = 3
	   ELSEIF (str4(1:1) .eq. ' ') THEN
	      CALL Putline_NF(contr,' Sample temp will be stored.')
	      which_temp = 1
	   ELSE
	      goto 275
	   ENDIF
	ELSEIF (temptype.eq.24) THEN
 276	   CALL Read_STRING (keyb, str4, 
     1	        ' Enter sensor to be used for sample temp '//
     1	        '(1 - 16) (default=1): ',
	1	4,j,j,j, l,j, contr) !j is dummy param for absoft
	   IF (l .eq. 0) THEN
	      samp_sensor = 1
	   ELSE
	      read(unit=str4(1:1),fmt=*,err=276)l
	      if (l .lt. 1 .or. l .gt. 16) goto 276
	      samp_sensor = l
	   ENDIF
 277	   CALL Read_STRING (keyb, str4, 
     1	        ' Enter sensor to be used for control temp '//
     1	        '(1 - 16) (default=1): ',
	1	4,j,j,j, l,j, contr) !j is dummy param for absoft
	   IF (l .eq. 0) THEN
	      contr_sensor = 1
	   ELSE
	      read(unit=str4(1:1),fmt=*,err=277)l
	      if (l .lt. 1 .or. l .gt. 16) goto 277
	      contr_sensor = l
	   ENDIF
 278	   CALL Putline_NF(contr,
     1		' You may select which temperature is stored in data file: ')
	   CALL Putline_NF(contr,
     1		'   S: Sample Temperature (default)')
	   CALL Putline_NF(contr,
     1		'   C: Control Temperature')
	   CALL Putline_NF(contr,
     1		'   P: Setpoint ')
	   CALL Read_STRING (keyb, str4, 
	1	' Use letter indicated above to make choice (S/C/P): ',
	2	4,j,j,j, l,j, contr) !j is dummy param for absoft   
	   IF (str4(1:1) .eq. 'S' .or. str4(1:1) .eq. 's') THEN
	      which_temp = 1
	   ELSEIF (str4(1:1) .eq. 'C' .or. str4(1:1) .eq. 'c') THEN
	      which_temp = 2
	   ELSEIF (str4(1:1) .eq. 'P' .or. str4(1:1) .eq. 'p') THEN
	      which_temp = 3
	   ELSEIF (str4(1:1) .eq. ' ') THEN
	      CALL Putline_NF(contr,' Sample temp will be stored.')
	      which_temp = 1
	   ELSE
	      goto 278
	   ENDIF
	ENDIF
C       Check communications
 300	IF (temptype .ne. 14)THEN
	   CALL Read_STRING (keyb, str4, 
	1	'Do you wish to verify communication with temperature controller? (T/F): ',
	2	4,j,j,j, l,j, contr) !j is dummy param for absoft   
	   IF (str4(1:1) .eq. 'T' .or. str4(1:1) .eq. 't') THEN 
	      Call checktemp(errflag)
	      IF (errflag) then
		 CALL Putline_NF (contr, '*****Error reading temperature*****.')
		 GOTO 300
	      ELSE
		 CALL Putline_NF (contr, 'Display read without error.')
	      ENDIF
	   ENDIF		!true reply
	ENDIF			!temptype .ne. 14
C       Get sensor info and coefficients for Linear Research devices
	IF (temptype .eq. 4 .OR. temptype .EQ. 7)THEN !get sensor & coeff info
	   CALL Read_STRING (keyb, str4, 
	1	'Are you using a sensor for which table is defined(T/F): ',
	2	4,j,j,j, l,j, contr) !j is dummy param for absoft   
	   IF (str4(1:1) .eq. 'T' .or. str4(1:1) .eq. 't') THEN 
	      CALL Read_STRING (keyb, str7, 
	1	   'Enter sensor code (7 characters): ',
	2	   7,j,j,j, l,j, contr) !j is dummy param for absoft
	      snsname(1:7) = str7

	      curvefile(1:) = trim(gethome())//
	1	   'lakeshore/lrcoeffs/'//snsname(1:7)//'.T_OF_R'

	      OPEN(UNIT=2,FILE=curvefile,STATUS='OLD',err=3209)
	      READ(2,fmt='(a1)',err=3209)str4(1:1) !skip header or ***
	      READ(2,fmt=*,err=3209)hdwlong !memorize number of ranges
C       Loop through each temp range reading in info
	      index = 1		!initialize array index to 1st position
	      DO irange = 1,hdwlong !read in each temp range list
		 READ(2,fmt='(a1)',err=3209)str4(1:1) !skip ***
		 READ(2,fmt=*,err=3209)incfs(irange)%lotemp,
	1	      incfs(irange)%hitemp !real temp range
		 READ(2,fmt=*,err=3209)incfs(irange)%loz,incfs(irange)%hiz !lz and uz
		 READ(2,fmt=*,err=3209)incfs(irange)%numb
		 incfs(irange)%start = index
		 DO i = 1,incfs(irange)%numb
		    READ(unit=2,fmt=*,err=3209)hdwtemp(index)
		    index = index + 1
		 ENDDO
	      ENDDO
	      CLOSE(UNIT=2)

	      curvefile(1:) = trim(gethome())//
	1	   'lakeshore/lrcoeffs/'//snsname(1:7)//'.R_OF_T'

	      OPEN(UNIT=2,FILE=curvefile,STATUS='OLD',err=3209)
	      READ(2,fmt='(a1)',err=3209)str4(1:1) !skip header 
	      READ(2,fmt=*,err=3209)snslong !memorize number of ranges
C       Loop through each temp range reading in info
	      index = 1
	      DO irange = 1, snslong !read in each temp range list
		 READ(2,fmt='(a1)',err=3209)str4(1:1) !skip ***
		 READ(2,fmt=*,err=3209)outcfs(irange)%lotemp,
	1	      outcfs(irange)%hitemp !real temp range
		 READ(2,fmt=*,err=3209)outcfs(irange)%loz,outcfs(irange)%hiz !lz and uz
		 READ(2,fmt=*,err=3209)outcfs(irange)%numb
		 outcfs(irange)%start = index
		 DO i = 1, outcfs(irange)%numb
		    READ(unit=2,fmt=*,err=3209)snstemp(index)
		    index = index + 1
		 ENDDO
	      ENDDO
	      CLOSE(UNIT=2)
	   ELSE			!no sensor
	      tdev%cfg = -tdev%cfg !flag that no sensor is being used
	      CALL Putcontr(' Resistance values will be used')
	   ENDIF
	ENDIF			!LR400 or LR700

c       Unless Lakeshore, do not continue
	IF (temptype .GT. 2) THEN
	   dbcurve = .false.
	   tinit_done = .true.
	   CALL Putline_NF (contr, ' ')
	   GOTO 888		!RETURN
	ENDIF

	IF (tinit_done .and. dbcurve) then
	   CALL Putline_NF(contr,'Using database curves '//hdwname//
	1	' and '//snsname)
	   CALL Read_STRING (keyb, str4, 'Is this correct? (T/F): ', 
     1		4,j,j,j, l,j, contr) !j is dummy param for absoft   
	   IF (str4(1:1) .eq. 'T' .or. str4(1:1) .eq. 't') GOTO 2200 !no change
	ENDIF			!tinit_done


	CALL Putline_NF (contr, ' ')
	response=
	1    'Temperature can be controlled using a curve in the controller or from a'
	CALL Putline_NF (contr, response)
	response=
	1    'database. To use a curve in the controller, the curve must be in the'	
	CALL Putline_NF (contr, response)
	response=
	1    'controller PROM and the controller must be set up properly. Using'
	CALL Putline_NF (contr, response)
	response=
	1    'a curve from a database may result in an inaccurate display on the'
	CALL Putline_NF (contr, response)
	response=
	1    'controller itself. The temperature will, however, be controlled and'
	CALL Putline_NF (contr, response)
	response=
	1    'recorded correctly on the screen and in the data file.'
	CALL Putline_NF (contr, response)
	CALL Putline_NF (contr, ' ')
	CALL Read_STRING (keyb, str4, 
	1    'Do you wish to use a curve from a database (new option)? [T/F]',
	2    4,j,j,j, l,j, contr) !j is dummy param for absoft   
	IF (str4(1:1) .eq. 'F' .or. str4(1:1) .eq. 'f' .or.
	1    str4(1:1) .eq. ' ') THEN 
	   IF (temptype .eq. 1) then !old lakeshore: DRC 82C
	      response(1:)='Please be sure dip switches are set appropriately.'
	   ELSE IF (temptype .eq. 2) then !new lakeshore: DRC 93C
	      response(1:)='Verify proper curve is selected on front panel'
	   ELSE IF (temptype .eq. 3 .or. temptype .eq. 6 .or. 
	1	   temptype .eq. 4 .or. temptype .eq.8) then !LR400
	      response(1:)=' '
	   ENDIF
	   CALL Putline_NF (contr, response)
	   CALL Putline_NF (contr, ' ')
	   dbcurve = .false.
	   tinit_done = .true.
	   GOTO 888		!RETURN
	ENDIF


	hdwname(1:)=' '
	IF (temptype .eq. 1) then !old lakeshore: DRC 82C
	   CALL Putline_NF(contr,' ')
	   response(1:)=
	1	'Select DRC-D and set dipswitches 5,6,7,& 8 to zero or'
	   CALL Putline_NF (contr, response)
	   response(1:)=
	1	'Select CRV10 and set dipswitches 5,6,7,& 8 to 0010 or'
	   CALL Putline_NF (contr, response)
	   response(1:)=
	1	'Select curve of your choice and set dipswitches appropriately.'
	   CALL Putline_NF (contr, response)
	   CALL Read_STRING (keyb, hdwname, 
	1	'Enter curve name or RETURN to exit: (Ex: crv10 or DRC-D): ',
	2	10,j,j,j, l,j, contr) !j is dummy param for absoft   
	   j = 1
	   DO WHILE (hdwname(j:j) .eq. ' ' .and. j .lt. 10)
	      j=j+1
	   ENDDO
	   IF (j .ge. 10)THEN
	      CALL Putline_NF (contr, ' ')
	      GOTO 888		!RETURN
	   ENDIF
	   hdwname(1:)=hdwname(j:) !remove leading blanks
	   j = 1
	   DO WHILE (hdwname(j:j) .ne. ' ' .and. j .le. 6)
	      j=j+1
	   ENDDO
	   hdwname(1:)=hdwname(1:j-1)//'.CRV'
	ELSE IF (temptype .eq. 2) then !new lakeshore: DRC 93C
	   CALL Putline_NF(contr,' ')
	   response(1:)=
	1	'Select curve on temperature controller and enter name below.'
	   CALL Putline_NF (contr, response)
	   response(1:)=
	1	' Use crv10 for standard diode and set controller to curve 2.'
	   CALL Putline_NF (contr, response)
	   response(1:)=
	1	' Use din-pt for standard platinum and set controller.'
	   CALL Putline_NF (contr, response)
	   CALL Read_STRING (keyb, hdwname, 
	1	'Enter curve name or RETURN to exit: (Ex: crv10 or din-pt): ',
	2	10,j,j,j, l,j, contr) !j is a dummy param for absoft   
	   j = 1
	   DO WHILE (hdwname(j:j) .eq. ' ' .and. j .lt. 10)
	      j=j+1
	   ENDDO
	   IF (j .ge. 10)THEN
	      CALL Putline_NF (contr, ' ')
	      GOTO 888		!RETURN
	   ENDIF
	   hdwname(1:)=hdwname(j:) !remove leading blanks
	   j = 1
	   DO WHILE (hdwname(j:j) .ne. ' ' .and. j .le. 6)
	      j=j+1
	   ENDDO
	   hdwname(1:)=hdwname(1:j-1)//'.CRV'
	ELSE IF (temptype .eq. 4) then !LR400
c       not possible for now
	ENDIF

	curvefile = trim(gethome())//'lakeshore/pdpcurves/'//hdwname

	OPEN(UNIT=2,FILE=curvefile,STATUS='OLD',err=3209)
	READ(2,fmt='(1x,a20)',err=3209)INBUF !header
C       LOOP THROUGH POINTS READING EACH INTO ARRAYS CRVOLTS & CRTEMP
	NOPNTS=1
	DO WHILE (NOPNTS .le. maxpnts)
	   READ(unit=2,fmt=*,END=400,err=3209)hdwvolts(nopnts),hdwtemp(nopnts)
	   NOPNTS=NOPNTS+1
	ENDDO
 400	CLOSE(UNIT=2)
	hdwlong=NOPNTS-1	!NO OF POINTS ACTUALLY READ IN IS IN hdwarr

	response(1:)='All xxxx points of curve: '//hdwname//' read in'
	write(unit=response(5:8),fmt='(i4)',err=3209)hdwlong
	CALL Putline_NF (contr, response)

c	Ask for sensor name (give them table)
	CALL Putline_NF(contr,' ')
	response(1:) = 
	1    'Item: CRYOSTAT  SENSOR ID         Item: CRYOSTAT  SENSOR ID'
	CALL Putline_NF(contr,response)
 500	itemno = 0
	OPEN (UNIT=2, FILE= trim(gethome())//'lakeshore/sensors/SENSORS.TABLE',
	1    STATUS='OLD')
	READ (2, FMT='(a1)') response ! skip header
 504	READ (2, FMT= *, END= 505) cryo, sensorid, standard
	itemno = itemno + 1
	j = 1
	DO WHILE (cryo(j:j) .eq. ' ' .AND. j .lt. 20) 
	   j = j+1
	ENDDO
	hldcryo(1:)=cryo(j:20)
	j = 1
	DO WHILE (sensorid(j:j) .eq. ' ' .AND. j .lt. 20) 
	   j = j+1
	ENDDO
	hldid(1:)=sensorid(j:20)
	j = 1
	DO WHILE (standard(j:j) .eq. ' ' .AND. j .lt. 20) 
	   j = j+1
	ENDDO
	hldstand(1:)=standard(j:20)
	write(hlditem(1:3),fmt='(I3)')itemno
	cryo(1:) = ' '		!set to blank in case no more entries
	sensorid(1:) = ' '
	standard(1:) = ' '
	itemstr(1:4) = '    '
	READ (2, FMT= *, END= 502) cryo, sensorid, standard
	itemno = itemno + 1
	j = 1
	DO WHILE (cryo(j:j) .eq. ' ' .AND. j .lt. 20) 
	   j = j+1
	ENDDO
	cryo(1:)=cryo(j:20)
	j = 1
	DO WHILE (sensorid(j:j) .eq. ' ' .AND. j .lt. 20) 
	   j = j+1
	ENDDO
	sensorid(1:)=sensorid(j:20)
	j = 1
	DO WHILE (standard(j:j) .eq. ' ' .AND. j .lt. 20) 
	   j = j+1
	ENDDO
	standard(1:)=standard(j:20)
	write(itemstr(1:3),fmt='(I3)')itemno
	itemstr(4:4) = ':'
 502	response(1:)  = ' '
	response(2:)=hlditem//': '//hldcryo(1:12)//hldid(1:8)//hldstand(1:6)//
	1    '   '//itemstr//' '//cryo(1:12)//sensorid(1:8)//standard(1:6)
        CALL Putline_NF (contr, response)
        GOTO 504
 505	CLOSE(2)
	CALL Read_STRING (keyb, str4, 'Select curve by entering item number:  ',
	1    4,j,j,j, l,j, contr) !j is a dummy param for absoft   
	read(str4(1:4),fmt=*,err=500)curveno
	IF (curveno .LE. 0 .OR. curveno .GT. itemno) GOTO 500
 	itemno = 0

	OPEN (UNIT=2, FILE= trim(gethome())//'lakeshore/sensors/SENSORS.TABLE',
	1    STATUS='OLD')

	READ (2, FMT='(a1)') response ! skip header
	DO WHILE (itemno .ne. curveno)
	   itemno = itemno + 1
	   READ (2, FMT= *, END= 507) cryo, sensorid, standard
	ENDDO
 507	CLOSE(2)

c	Use that sensorid to read file
	j=1
	DO WHILE (sensorid(j:j) .NE. ' ' .AND. j .LT. 20)
	   j = j+1
	ENDDO
	snsname=sensorid(1:j-1)//'.CRV'
	curvefile=trim(gethome())//'lakeshore/pdpcurves/'//snsname


	OPEN(UNIT=2,FILE=curvefile,STATUS='OLD',err=3209)
	READ(2,fmt='(1x,a20)',err=3209)INBUF !header
C       LOOP THROUGH POINTS READING EACH INTO ARRAYS CRVOLTS & CRTEMP
	NOPNTS=1
	DO WHILE (NOPNTS .le. maxpnts)
	   READ(unit=2,fmt=*,END=600,err=3209)snsvolts(nopnts),snstemp(nopnts)
	   NOPNTS=NOPNTS+1
	ENDDO
 600	CLOSE(UNIT=2)
	snslong=NOPNTS-1	!NO OF POINTS ACTUALLY READ IN IS IN snsarr

	response(1:)='All xxxx points of curve: '//snsname//' read in'
	write(unit=response(5:8),fmt='(i4)',err=3209)snslong
	CALL Putline_NF (contr, response)
	CALL Putline_NF (contr, ' ')

	dbcurve = .true.
	tinit_done = .true.

 2200	response(1:)='Temperature configuration selected: '
	CALL Putline_NF(contr,response)
	CALL Putline_NF (contr, lconfig(temptype)(9:)) 
	IF (dbcurve) 
	1    CALL Putline_NF(contr,' Using database curves '//hdwname//
	2    ' and '//snsname)
	IF (temptype .eq. 1) then !old lakeshore: DRC 82C
	   response(1:)=' *****Remember to set dipswitches before use.*****'
	   CALL Putline_NF(contr,response)
	ELSE IF (temptype .eq. 2) then !new lakeshore: DRC 93C
	   response(1:)=' *****Remember to select Curve 2 on front panel.*****'
	   CALL Putline_NF(contr,response)
	ELSE IF (temptype .eq. 4) then !LR400
c       no message for now
	ENDIF
	CALL Read_STRING (keyb, str4, 
     1	     'Hit RETURN to continue: ',
	1    4,j,j,j, l,j, contr) !j is a dummy param for absoft   
	CALL Putline_NF(contr,' ')


	ENTRY PTCONFIG		!Print temp configuration
 888	IF (.NOT. tinit_done)THEN
	   CALL Putline_NT (contr,' Temp Controller not initialized')
	   RETURN
	ENDIF
 	CALL Putline_NT (contr, lconfig(temptype)(9:)) 
	IF (temptype .lt. 3 )THEN
	   response(1:)=' Storing '//whichstr(which_temp)//' in data file'
	   CALL Putline_NT (contr,response)
	ELSEIF (temptype .eq. 4 .or. temptype .eq.7)THEN
	   IF (tdev%cfg .EQ. temptype)THEN
	      response(1:)=' Using sensor '//snsname(1:7)
	   ELSE
	      response(1:)=' Using resistance'
	   ENDIF
	   CALL Putline_NT (contr,response)
	ENDIF
	IF (dbcurve) 
	1    CALL Putline_NT(contr,' Using database curves '//hdwname//
	2    ' and '//snsname)
	
	CALL Putline_NF (contr, ' ')
	RETURN

 3209	response(1:)=' *****Error reading file '//curvefile(1:32)//' *****'
	CALL Putline_NT (contr, response)
	CALL Putline_NF (contr, ' ')
	tinit_done = .false.
	dbcurve = .false.
	Return

	end
