#include "icp.h"
	SUBROUTINE Scan_Range(mot, range, delta, mon, keep, type, x, y, int,
     1            jmax, jmin, npt, maxpos1, ierr, montemp, minpos1, twotheta,
     1		  psfile, fitinten)
C	***************************************************

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C                                                                             C
C	Routine scans the given motor around the current angle within range   C
C At call:								      C
C	mot = motor number
C	range = range in degrees covered by scan with current position as center
C	delta = step size in degrees
C	mon = neutrons or time to count at each point
C       keep is true if a file is to be kept of counts at each position
C	type = 1 searches one angle
C	type = 2, does a 2:1 scan                C
C	type = 3 is for monitor peak                                          C
C	type = 4 is for special xray scan. Twotheta only used for fpx         C
C	type = 5 is for RM scan
C	type = 6 is for RA scan
C	type = 7 searches one angle with sigmoidal curve 
C       type = -x is for scan at detector x (FPD and FPL)		      C
C	montemp is true if the temp is to be read after each point in scan
C	twotheta/2 is used as scan center for type=4
C	psfile = name of file in which scan data is stored
C  At return:
C	npt = number of points at which counts were made
C       x(1->npt) = position of mot at each point
C       y(1->npt) = counts at each point (fratio at each point for type 5 & 6)
C       int has intensity for fitting
C	jmax = index of x,y corresponding to the position with max intensity
C	jmin = index of x,y corresponding to the position with min intensity
C	maxpos1 = position of second angle when max intensity was obtained
C	minpos1 = position of second angle when min intensity was obtained
C	ierr = 0 if scan proceeded without problem
C	ierr = 999 if scan contained more than the 100 point max. Can not fit. 
C	ierr = 3 is fatal error such as motor out of limits, contr_a, or
C                flipping ratio involving division by zero
C                                                                             C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
	use instr
	use smg_prep
	use histo
	use tc
	use asd
	use iso_c_binding
	IMPLICIT      none
	interface
#include "c_libwait.h"
	end interface
	LOGICAL	     montemp, keep, inlimits, flippers, fitinten, Readef
	INTEGER*4    j, mot, points, counts(37), npt
	INTEGER*4    type, m, mn, k, ierr, i
	INTEGER*4    jmax, jmin, ios, ij
        REAL*4       range, delta, step(2), mon, center(2), start(2), 
     1			maxpos1, minpos1, rmaxint, rminint,rcounts
	REAL*4       pos(2), RdAngl, x(100), y(100), int,
     1		     twotheta, rfirst
 	CHARACTER*80 response
	REAL*4	     temp, dummy, RdAm, zm, FZ
 	CHARACTER*10 str10
	CHARACTER*17 tmpstr
	CHARACTER*3  rstr, namestr
	CHARACTER*12 psfile,fstring
	CHARACTER*28 sys_time
	CHARACTER*15 vstr
	INTEGER*4	lb, lv, status
	CHARACTER*2	str2, str2b
	BYTE	    tabbyte/9/
	CHARACTER*1 tab
	EQUIVALENCE (tabbyte,tab)

	finishup=.false. ! Prepare for shortening scan using ctrl-F (ctrl-\)
C  Open data file. For align_peak, same file is used for all scans
	IF (keep) THEN !name datafile
	  OPEN (unit=1,file=psfile,status='UNKNOWN',access='APPEND',
     1		iostat=ios) !unknown & append for ap command recalls
	  IF (ios .NE. 0) THEN
	    keep = .FALSE.
	    CALL Putcontr_NT ('Error opening output file')
	  ENDIF
	  fstring=psfile
	ELSE
	  fstring(1:)=' '
	ENDIF !keep

	IF (nsta .gt. 0) THEN
	   namestr(1:) = 'BT%'
	ELSEIF (nsta .eq. 0) THEN         
	   namestr(1:) = 'XR%'
	ELSE
	   namestr(1:) = 'NG%'         
	ENDIF
	IF (nsta .eq. -11) THEN
	   namestr(1:3) = 'CG1'
	ELSE
	   WRITE (UNIT= namestr(3:3), FMT= '(i1)') iabs(nsta)
	ENDIF

C  Write header line to data file
 	IF (KEEP) THEN
	  CALL DATE_TIME(sys_time) !get time string
	  IF (montemp)THEN
	    tmpstr(1:) = '      TEMP'
	  ELSE
	    tmpstr(1:) = sys_time(1:17)
	  ENDIF
	  if (type.eq.2 .or. type.eq.4)then !FPT or FPX
	    IF (nsta .EQ. -5 .AND. use_asd)THEN
	      WRITE (UNIT = 1,fmt='(a,i2,a,i2,a,a)',iostat=ios)
	1	    ' Motor no. ',mot-1,'     Motor no. ',mot,
	1	    '    ASD Intensity   ',tmpstr
	    ELSE
	      WRITE (UNIT = 1,fmt='(a,i2,a,i2,a,a)',iostat=ios)
	1	    ' Motor no. ',mot-1,'     Motor no. ',mot,
	1	    '    Intensity   ',tmpstr
	    ENDIF
	  elseif (type .lt. 0) THEN !FPD or FPL (FP for specified detector)
	    WRITE (UNIT = 1,fmt='(a,i2,a,i4,a)',iostat=ios)
	1	  ' Motor no. ',mot,'    Intensity at Detector: ',
	1	  iabs(type),tmpstr
	  elseif (type .eq. 5 .OR. type .eq. 6)THEN !RM or RA
	    IF (type .eq. 5)then !FP/RM
	      rstr(1:) = 'RM '
	    ELSE !FP/RA
	      rstr(1:) = 'RA '
	    ENDIF
	    WRITE (UNIT = 1,fmt='(a,i2,a,a,2x,a)',iostat=ios)
     1	       ' Motor no. ',mot,' Flipping Ratio ',rstr,tmpstr
	  else !FP or FM type
	    IF (nsta .EQ. -5 .AND. use_asd)THEN
	      WRITE (UNIT = 1,fmt='(a,i2,a,a)',iostat=ios)
     1	       ' Motor no. ',mot,'    ASD Intensity   ',tmpstr
	    ELSE
	      WRITE (UNIT = 1,fmt='(a,i2,a,a)',iostat=ios)
     1	       ' Motor no. ',mot,'    Intensity   ',tmpstr
	    ENDIF
	  endif
	END IF !keep datafile
C  Set up scan and Write summary of scan to screen
 	step(2)    = delta
	points     = NINT (range/ABS(step(2))) ! Take care of neg step(2)
	IF (type.ne.4)THEN !all but special xray
	  RdAm = RdAngl(mot)
	  zm = Fz(mot,RdAm)
	  center(2) = RdAm - zm        ! typically A4 in 2:1, A3 else
	  start(2)   = center(2) - points/2 * step(2)
	ELSE !special xray
	  start(2) = twotheta/2. - range/2. !motor 4
	  center(2) = twotheta/2.
	ENDIF
	CALL DATE_TIME(sys_time)
	CALL Putline_NT (contr, 'Find_peak:  '//sys_time(1:17))
	WRITE (UNIT= response(1:), FMT= 
     1	'(''Angle no = '', i2,'',  Range = ['', f8.3, '' ...'',f8.3, '' ],  Step ='',f8.3,'', Mon ='', i8)',
     1		iostat=ios) 
	3      mot, start(2), start(2) + points*step(2), step(2), NINT(mon)
	CALL Putline_NT (contr, response)
	IF (type .EQ. 2 .or. type.eq.4) THEN   ! we have a 2:1 scan here or special xray
	   IF (type.eq.2)THEN !2:1 scan
		RdAm = RdAngl(mot-1)
		zm = Fz(mot-1,RdAm)
	        center(1) = RdAm - zm
	        step(1)    = 0.5 * delta
	        start(1)   = center(1) - points/2 * step(1)
	   ELSE		      !special xray scan
		start(1)   = twotheta/2. + range/2. !motor 3
		center(1) = twotheta/2.
		step(1) =  -delta
	   ENDIF
	   CALL Putline_NT (contr, 'Find_peak:')
	   WRITE (UNIT= response(1:), FMT= 
     1	   '(''Angle no = '', i2,'',  Range = ['', f8.3, '' ...'', f8.3, '' ],  Step = '',f8.3)',
     1		iostat=ios) mot-1, start(1), start(1) + points*step(1), step(1)
 	   CALL Putline_NT (contr, response)
	END IF
	npt   = points + 1
	if (npt .gt. 100) npt = 100 ! NCM: Fix boundary error problem

	vstr(1:)='VARY='
	lv = 5
	write(unit=str2,fmt='(i2.2)',iostat=ios)mot !write motno
	vstr(1:)=vstr(1:lv)//'A'//str2//' '
	lv = lv + 4
	IF (type .eq.2 .or. type .eq.4)THEN !moving two motors
	  write(unit=str2,fmt='(i2.2)',iostat=ios)mot-1 !write motno
	  vstr(1:)=vstr(1:lv)//'A'//str2//' '
	  lv = lv + 4
	ENDIF
	IF (READEF (contr_A))THEN 
	   CALL Putline_NT (contr, ' **** ABORT during Find_Peak ****')
	   ierr = 3 ! fatal error on return
	   RETURN
	END IF


	write(unit=str10,fmt='(I10)',iostat=ios)npt
	response(1:)=namestr//':START'//tab//'NPTS='//str10//
     1		tab//'FILE='//fstring//tab//vstr(1:lv)//tab//
     1		'Find_Peak'
	lb = 54+lv !length of filled part of string
	CALL Gobroad(lb,response)
C  Do scan
	int       = 0.
	rmaxint    = 0.
	rminint    = 65535.
	jmax = npt/2
	jmin = npt/2
	maxpos1 = center(1)
	minpos1 = center(1)
	ierr = 3 !init for fatal err at return
	write(unit=str2,fmt='(i2.2)',iostat=ios)mot !write motno
	IF (type.eq.2 .or. type.eq.4)
	1    write(unit=str2b,fmt='(i2.2)',iostat=ios)mot-1 !write motno-1
	DO j = 1, npt
	   IF (READEF (pause))THEN !pause?
	      CALL PUTCHARS_REV (stat, '*** PAUSE ***', 1, 15)
	      CALL CURSOR_OFF (paste)
	      CALL HIBER()                                   
	      CALL clref (contr_A)
	      CALL clref (hdw_event) !remote abort
	      CALL clref (pause)
	      CALL PUTCHARS_REV (stat, '             ', 1, 15)
	      CALL CURSOR_ON (paste)
	      CALL SCROLL_DISPLAY (contr)
	      CALL CURSOR_ABS (contr, 19, 1)
	   END IF 
C      Move motor(s) to position for this point
	   IF ( type .EQ. 2 .or. type.eq.4) THEN !2:1 or special xray
	      k = 1 !use "first" motor also
	   ELSE
	      K = 2 !only move "second" motor
	   END IF
	   DO m = 2, k, -1 !move one or two motors 
	      mn = mot + m - 2 !either mot or mot-1
	      pos(m) = start(m) + (j-1)*step(m)
	      zm = Fz(mn,pos(m))
	      IF (.NOT. inlimits(mn,pos(m)+zm,contr))THEN
	         CALL Putline(contr,'Find_Peak aborted')
	         RETURN
	      END IF
              CALL Drive (mn, pos(m)+zm)
	   END DO
	   CALL Yield
	   IF (READEF (contr_A))THEN 
	      CALL Putline_NT (contr, ' **** ABORT during Find_Peak ****')
	      RETURN
	   END IF
	   IF (liqwait.ne.0.)CALL libwait(liqwait,status) !for ng7 liquid samples
C      Count at this point. Count at each config if using flippers.
	   IF (type .EQ. 5 .OR. type .EQ. 6)THEN !are we using flippers
	     ij = 2
	     flippers = .TRUE.
	   ELSE
	     ij = 1
	     flippers = .FALSE.
	   ENDIF
	   IF (type .EQ. 3) mon = -1 * ABS(mon)    ! make sure we count on time
	   DO i = 1, ij !count ij times at this motor position
	     IF (flippers)THEN !set flippers
	       IF (i .eq. 1 .and. type .eq. 5)THEN
		 CALL Set_Flippers(8) !turn RM flippers to off
	       ELSEIF (i .eq. 1 .and. type .eq. 6)THEN
		 CALL Set_Flippers(6) !turn RA flippers to off
	       ELSEIF (i .eq. 2 .and. type .eq. 5)THEN
		 CALL Set_Flippers(7) !turn RM flippers to on
	       ELSEIF (i .eq. 2 .and. type .eq. 6)THEN
		 CALL Set_Flippers(5) !turn RA flippers to on
	       ENDIF
	     ENDIF
	     CALL Reset_Display 
	     IF (mon .GT. 0.) THEN
	       IF (nsta .EQ. -5 .AND. use_asd)THEN
		 CALL zero_asd
	         CALL asd_on(1)
	       ENDIF
	       CALL Count(mon)
	     ELSE
	       IF (nsta .EQ. -5 .AND. use_asd)THEN
		 CALL zero_asd
	         CALL asd_timed(1,ABS(mon))
	       ENDIF
               CALL CnTime (ABS(mon))
	     END IF
	     CALL Scaler (counts)
	     IF (READEF (contr_A))THEN 
	       CALL Putline_NT (contr, ' **** ABORT during Find_Peak ****')
	       RETURN
	     END IF	   
C        Get counts from appropiate place according to type scan --> rcounts
	     m = 2                            ! select default scaler
	     IF (type .EQ. 3) m = 1           ! 1 = monitor counts
	     IF (type .LT. 0) m = iabs(type) + 1 !using some other scaler or detector
	     IF (type .GE. 0) THEN !not single PSD detector
	       IF (nsta .EQ. -5 .AND. use_asd)THEN
	         rcounts = asd_data(1)
	       ELSEIF (nsta.eq.-11.and.use_asd)THEN	
		 rcounts=NINT(fpwindow)
	       ELSEIF (PSD .AND. type .NE. 3)THEN !use intensity sum
		 rcounts = fpwindow
	       ELSE
	         rcounts = FLOAT(counts(m))
	       ENDIF
	     ELSE !PSD detector or scaler other than 1
	       IF (psd)THEN
		  if (type .lt. 0) then
		     rcounts = FLOAT(histodata(iabs(type)))
		  else
		     rcounts = fpwindow
		  endif
	       ELSE
	         rcounts = FLOAT(counts(m))
	       ENDIF
	     ENDIF
	     IF (flippers)THEN !compute ratio or memorize counts
	       IF (i .EQ. 1)THEN !flippers off
		rfirst = rcounts !remember mcounts for first flipper position
	        IF (rfirst .EQ. 0.0) THEN
		  CALL Putcontr('Error: Zero counts while flippers On')
	          if (keep) close(unit=1,iostat=ios)
		  RETURN
		ENDIF !
	       ELSE !flippers on
		rcounts = rcounts/rfirst
	       ENDIF !flipper position
	     ENDIF !flippers
	   ENDDO !for each counting period at a given motor position
C      Compare counts at this point against minimum and maximum    
	   IF (rcounts .GT. rmaxint) THEN
	      rmaxint = rcounts	
	      jmax = j
	      maxpos1 = pos(1)
	   ELSE IF (rcounts .LT. rminint) THEN
	      rminint = rcounts
	      jmin = j
	      minpos1 = pos(1)
	   ENDIF
C      Go read temp if option selected to monitor at each point
	   IF (montemp) then
	     IF (which_temp.eq.2)THEN !control
	       CALL RdTemp (dummy, temp, dummy)
	     ELSEIF (which_temp.eq.3) THEN !setpoint
	       CALL RdTemp (dummy, dummy, temp)
	     ELSE !sample
	       CALL RdTemp (temp, dummy, dummy)
	     ENDIF
	     IF (temptype .EQ. tdev%cfg) THEN !using degrees
		  CALL putmax(temp,str10,10,4)
	     ELSE !using resistance
		  CALL putmax(temp,str10,10,9)
	     ENDIF
	   ENDIF
C      Write out counts at this point to screen and file
	   IF ( type .EQ. 2 .or. type.eq.4) THEN !2:1 or special xray
	    IF (montemp)THEN
	      WRITE (UNIT= response(1:), FMT= 
     1	    '('' Angle'',a2,''='',f9.3,'' Angle'',a2,''='',f9.3, '' Intensity= '', i10,'' T='',a10)',
     1			 iostat=ios) 
	2            str2b,pos(1),str2, pos(2), NINT(rcounts), str10
	      if (keep) WRITE (UNIT = 1,fmt=
     1	        '(f10.4,4x,f10.4,4x,i10,4x,a10)',iostat=ios)
     1	        pos(1),pos(2),NINT(rcounts),str10
	    ELSE !not montemp
	      WRITE (UNIT= response(1:), FMT= 
     1	     '(''  Angle'',a2,''='', f9.3,''   Angle'',a2,''='', f9.3, 3x, ''Intensity = '', i10)',
     1		     iostat=ios) 
	2            str2b,pos(1),str2, pos(2), NINT(rcounts)
	      if (keep) WRITE (UNIT = 1,fmt='(f10.4,4x,f10.4,4x,i10)',iostat=ios)
     1	        pos(1),pos(2),NINT(rcounts)
	    ENDIF !montemp
	   ELSEIF (type .EQ. 5 .or. type .EQ. 6)THEN !write ratio
	    IF (montemp) THEN
	      WRITE (UNIT= response(1:), FMT= 
     1	 '(''   Angle'',a2,''='',f9.3,3x,''Ratio '//rstr//' = '',f10.3,'' Temp='',a10)',
     1		     iostat=ios) str2,pos(2), rcounts, str10
	      if (keep) WRITE (UNIT = 1,fmt='(f10.4,4x,f10.3,4x,a10)',
     1		iostat=ios) pos(2),rcounts, str10
	    ELSE !not montemp
	      WRITE (UNIT= response(1:), FMT= 
     1	      '(''   Angle'',a2,''='',f9.3,3x,''Ratio '//rstr//' = '',f10.3)',
     1		iostat=ios) 
     1		     str2,pos(2), rcounts
	      if (keep) WRITE (UNIT = 1,fmt='(f10.4,4x,f10.3)',iostat=ios)
     1	        pos(2),rcounts
	    ENDIF !montemp
	   ELSE !type is not 2,4,5, or 6
	    IF (montemp)THEN
	      WRITE (UNIT= response(1:), FMT= 
     1		'(''   Angle'',a2,''='',f9.3,3x,''Intensity = '',i10,'' Temp='',a10)',
     1		  iostat=ios) 
     1		     str2,pos(2), NINT(rcounts),str10
	      if (keep) WRITE (UNIT = 1,fmt='(f10.4,4x,i10,4x,a10)',iostat=ios)
     1	        pos(2),NINT(rcounts), str10
	    ELSE !not montemp
	      WRITE (UNIT= response(1:), FMT= 
     1	      '(''   Angle'',a2,''='', f9.3, 3x,''Intensity = '', i10)',
     1		iostat=ios) str2,pos(2), NINT(rcounts)
	      if (keep) WRITE (UNIT = 1,fmt='(f10.4,4x,i10)',iostat=ios)
     1	        pos(2),NINT(rcounts)
	    ENDIF !montemp
	   END IF !type
 200	   IF (keep)THEN
	     CALL Putline_NF (contr, response)
	   ELSE
	     CALL Putline_NT (contr, response)
	   ENDIF
C      Save position and counts for this point
	   x(j) = pos(2)                   ! save position for fitting
 	   y(j) = rcounts                  ! save intensity for fitting
c	   int  = int + y(j) * step(2)     ! fp was like this
C      Broadcast point
	   write(unit=str10,fmt='(I10)',iostat=ios)j
	   response(1:)=namestr//':'//tab//'PT='//str10  !BT2:tab PT= and clear rest
	   lb = 18 !length of filled part of response
	   write(unit=str10,fmt='(F9.3)',iostat=ios)pos(2)
	   response(1:)=response(1:lb)//tab//'A'//str2//'='//str10
	   lb = lb + 15
	   write(unit=str10,fmt='(I10)',iostat=ios) NINT(rcounts)
	   response(1:)=response(1:lb)//tab//'DATA='//str10
	   lb = lb + 16
	   CALL Gobroad(lb,response)
	   IF (j .ge. 100) THEN ! Truncate the scan if it becomes too large for storage
	      npt = j
	      points = npt - 1
	      GOTO 100
	   END IF
	   IF (finishup) THEN ! Set in READEF() - end the scan
	      npt = j
	      points = npt - 1
	      GOTO 100
	   END IF
	END DO !end of scan through each point
C  End of scan.
C  Find integrated intensity to return for fitting   
 100	int = .5*(y(1) + y(npt))
	DO j=2,points
	   int = int + y(j)
	END DO
	int = int * step(2)
C  Close data file and return with error report  
	if (keep) close(unit=1,iostat=ios)
	IF (npt .GT. 100)THEN
	  ierr = 999 !ICP crashes if try to fit with too many
	ELSE
	  ierr = 0
	ENDIF
	RETURN
	END
