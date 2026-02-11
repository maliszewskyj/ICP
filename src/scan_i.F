#include "icp.h"
	SUBROUTINE Scan_I (psup, lower, delta, upper, mon, keep, arch)
C	************************************************

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C                                                                             C
C	Routine scans the given power supply current in the given range.      C
C	Called from: Parse_Contr                                              C
C                                                                             C
C	Beat Schmid, January 1991                                             C
C       Modified by Gayle Greene, April 1991. (Centroid calc. & FCAL added)   C
C	Modified by Gayle Greene, Dec. 1991. Fit added                        C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
	use instr
	use polar
	use smg_prep
	use asd
	use iso_c_binding
	IMPLICIT     none
	interface
#include "c_libwait.h"
	end interface
	INTEGER*4    j, m, psup, pts, counts(37), maxint, minint,i
	INTEGER*4    state, flipx, ierr,maxtry, right,idum,
     1		      ios,notries/20/,openfai/30/ !from $fordef
        REAL*4       delta, mon, current, maxpos, minpos, center,
     1	    	     posit, dspace, RdAngl,energy, x(100), y(100), e(100),
     1	             p(6), pi, mindif, maxdif, pos, lower, upper
 	CHARACTER*80 response
	CHARACTER*80   inp, subdir, info(4)
	Logical keep,arch,Readef
C---- variables for writing file --------------------------------------------C
	INTEGER*4     context,  status
	INTEGER*4     fver, fversion
	CHARACTER*80  res_fname,fitstr1,fitstr2
	character*1 coeff1/'+'/
	character*7 coeff2/'*cos(x*'/
	character*1 coeff3/'+'/
	character*1 coeff4/')'/
	CHARACTER*12  psfile, fstring
	CHARACTER*3  namestr
	CHARACTER*10  str10
	CHARACTER*100  bstr
	integer*4 lb
	CHARACTER*17  bfit
	BYTE	    tabbyte/9/
	CHARACTER*1 tab
	EQUIVALENCE (tabbyte,tab)

	integer*4 idata(100),ig
	CHARACTER*80 GETHOME
	REAL*4 ATAN2D,SIND

	Pi = ATAN2 (0., -1.) 

	pts = iabs (int (((upper + .5 * delta) - lower) / delta)) + 1
	If (pts.gt.100)then
	  CALL Putline_NT (contr, 'Number of points cannot exceed 100')
	  RETURN
	ENDIF


C-----------------open file in which points are written-------------------C
	if (keep) then !open datafile
	  Psfile(1:5) = 'SCAN0'
	  Write(psfile(5:5),fmt='(i1)',err=100)psup
	  IF (nsta.gt.0)THEN
 	    Psfile(6:11) = '???.bt'         
	  ELSE IF (nsta .eq. 0) THEN
 	    Psfile(6:11) = '???.xr'         
	  ELSE IF (nsta .eq. -11) THEN
 	    Psfile(6:11) = '???.cg'         
	  ELSE
 	    Psfile(6:11) = '???.ng'         
	  ENDIF
	  WRITE (UNIT= Psfile(12:12), FMT= '(i1)',err=100) iabs(nsta)
	  context = 0
	  fversion = 1
	  CALL FIND_FILE (Psfile, res_fname, context,status)
	  DO WHILE (status.ne.0)
C	   i = INDEX (res_fname, ']')
           i=0
	   READ (UNIT= res_fname(i+6:i+8), FMT= *,err=100) fver
	   IF (fver .GE. fversion) fversion = fver + 1
	   CALL FIND_FILE (Psfile, res_fname, context,status)
	  END DO
	  CALL FIND_FILE_END (context)
	  WRITE (UNIT= Psfile(6:8), FMT= '(i3.3)',err=100) fversion
	  CALL Putline_NT (contr, 'Scan file created: '//psfile(1:12))

	  OPEN (unit=1,file=psfile,status='new',err=100)
	  WRITE (unit=1,fmt='(a)',err=100)'   Current        Intensity'
	  fstring=psfile
	ELSE
	  fstring(1:)=' '
	END IF !datafile

	IF (nsta .gt. 0) THEN
	   namestr(1:) = 'BT%'
	ELSEIF (nsta .eq. 0) THEN         
	   namestr(1:) = 'XR%'
	ELSE
	   namestr(1:) = 'NG%'         
	ENDIF
	IF (nsta .eq. -11) THEN
	   namestr(1:) = 'CG1'
	ELSE
	   WRITE (UNIT= namestr(3:3), FMT= '(i1)') iabs(nsta)
	ENDIF

	goto 150

 100	keep=.false.
	CALL Putline_NT (contr, 'Error opening file '//psfile(1:12))


C------------------------ FIND OUT WHERE WE ARE HERE -------------------------C

 150	CALL Putline_NT (contr, 'Scan power supply current:')
	WRITE (UNIT= response(1:), FMT= 
     1	'(''PS no = '',i2,'',  Start = '',f8.3, ''  Step = '',f6.3, ''  #pts = '', i3,'',  Mon = '',i9)')
     1	 psup, lower, delta, pts, NINT(mon)
	CALL Putline_NT (contr, response)
	CALL Putline_NF (contr, ' ')

	write(unit=str10,fmt='(I10)',iostat=ios)pts
	response(1:)=namestr//':START'//tab//'NPTS='//str10//
     1		tab//'FILE='//fstring//tab//'VARY=CUR'//tab//
     1		'ISCAN'
	lb = 58 !length of filled part of string
	CALL Gobroad(lb,response)

	DO j = 1, pts 

!       C----------------- CHECK PAUSE & STOP FLAGS --------------------------C

	   IF (READEF (pause))THEN
	      CALL PUTCHARS_REV (stat, '*** PAUSE ***', 1, 15)
	      CALL CURSOR_OFF (paste)
	      CALL HIBER()                                   
	      CALL clref (contr_A)
	      CALL clref (hdw_event)  !remote abort
	      CALL clref (pause)
	      CALL PUTCHARS_REV (stat, '             ', 1, 15)
	      CALL CURSOR_ON (paste)
	      CALL SCROLL_DISPLAY (contr)
	      CALL CURSOR_ABS (contr, 19, 1)
	   END IF 

!          C------------- SET NEW CURRENT THEN COUNT -------------------------C

	   current = lower + (j-1) * delta
           CALL Iset (psup, current)
	   IF (mon .GT. 0.) THEN
	      IF (nsta .eq. -5 .and. use_asd) THEN
		use_asdfile=.false.
		call zero_asd
		call asd_on(1)
	      ENDIF
	      CALL Count(mon)
	   ELSE
	      IF (nsta .eq. -5 .and. use_asd) THEN
		use_asdfile=.false.
		call zero_asd
		call asd_timed(1,ABS(mon))
	      ENDIF
              CALL CnTime (ABS(mon))
	   END IF
	   CALL Scaler (counts)
	   IF (nsta .eq. -5 .and. use_asd)counts(2)=NINT(asd_data(1))
	   IF (nsta.eq.-11.and.use_asd)	counts(2)=NINT(fpwindow)

	   IF (READEF (contr_a))THEN
	      CALL Putline_NT (contr, ' **** ABORT during Scan Current ****')
	      if (keep)THEN
		CLOSE(unit=1)
		IF (arch)CALL ArchFile(psfile,12)
	      endif
	      RETURN
	   END IF	   

!          C---------------- SORT COUNTS & PRINT -----------------------------C

	   m = 2                              ! select scaler | 2 = detector
	IF (J.EQ.1)THEN
	  MINPOS=CURRENT
	  MAXPOS=CURRENT
	  MAXINT=COUNTS(M)
	  MININT=COUNTS(M)
	ENDIF
	   IF (counts(m) .GT. maxint) THEN
	      maxint = counts(m)	
	      maxpos = current
	   ELSE IF (counts(m) .LT. minint) THEN
	      minint = counts(m)
	      minpos = current
	   END IF
	   WRITE (UNIT= response(1:), FMT= 
     1		'(''   Current= '', f9.4, 5x, ''Intensity = '', i10)') current, counts(m)
	   IF (keep)THEN
	     CALL Putline_NF (contr, response)
	   ELSE
	     CALL Putline_NT (contr, response)
	   ENDIF
	   If (Keep)WRITE (UNIT=1,fmt='(1x,f9.4,5x,i10)',err=200)current,counts(m)
	   
 200	   x(j) = current
	   y(j) = FLOAT(counts(m))
	   e(j) = SQRT(y(j))                        ! save intensity error " "

C          Broadcast point
	   write(unit=str10,fmt='(I10)',iostat=ios)j
	   response(1:)=namestr//':'//tab//'PT='//str10  !BT2:tab PT= and clear rest
	   lb = 18 !length of filled part of response
	   write(unit=str10,fmt='(F10.4)',iostat=ios)current
	   response(1:)=response(1:lb)//tab//'CUR='//str10
	   lb = lb + 15
	   write(unit=str10,fmt='(I10)',iostat=ios) counts(m)
	   response(1:)=response(1:lb)//tab//'DATA='//str10
	   lb = lb + 16
	   CALL Gobroad(lb,response)
	END DO   
	IF (keep) close(unit=1)

C------------------------- SAY IF PEAK FOUND OR NOT --------------------------C

	CALL Putline_NF (contr, ' ')
	IF (maxint-minint .GT. 4*SQRT(FLOAT(maxint))) THEN
	  center = lower + .5 * delta * float(pts)
	  mindif = ABS (minpos - center)
	  maxdif = ABS (maxpos - center)
	  IF (maxdif .GT. mindif )THEN
	    p(2) = -FLOAT(maxint - minint)
	    p(3) = pi/minpos
	  ELSE
	    p(2) = FLOAT(maxint - minint)
	    p(3) = pi/maxpos
	  ENDIF
	    p(1) = FLOAT(minint)
	    p(4) = 0.
	ELSE
	  WRITE (UNIT=response(1:),FMT='(''Extremum not found. '')')
	  CALL Putline_NT (contr, response)
	  IF (keep.and. arch)CALL ArchFile(psfile,12)
	  RETURN  
	ENDIF

        CALL Fitsine (pts, x, y, e, p, ierr)
	fitstr1(1:)=' Fit: '
	fitstr2(1:)=' **'

	bstr(1:)=namestr//':END'//tab
	lb=8
	IF (ierr .EQ. 0) THEN                               ! Convergence
	  bstr(1:)=bstr(1:lb)//'TYPE=ISCAN'//tab
	  lb=lb+11
		 WRITE (UNIT=response(1:), FMT= '(a,f10.3)')
     1              ' Fit Results:   BG = ', p(1)
		 CALL Putline_NT (contr, response)
		 WRITE (UNIT=response(1:), FMT= '(a,f10.3)')
     1              '            Height = ', p(2)
		 CALL Putline_NT (contr, response)
		  Pos = (pi-p(4))/p(3)
		 WRITE (UNIT=response(1:), FMT= '(a,f10.3)')
     1              '               Pos = ', pos
		 CALL Putline_NT (contr, response)
		 CALL Putline_NF (contr, ' ')
		 IF (KEEP) THEN
		   WRITE(unit=fitstr1(1:),
     1			fmt='(1x,3(a,f10.3))',err=350)
     1			' Fit: Pos= ',pos,
     1			' BG= ',p(1),' Height= ',
     1	                p(2)
		   WRITE(unit=fitstr2(1:),
     1			fmt='(1x,4(a,f10.3),a)',err =350)' Fit eq: ',
     1	                p(1),coeff1,p(2),coeff2,p(3),coeff3,
     1			p(4),coeff4
		 ENDIF
		 bfit = 'FIT_P1='
	  	 do i = 1,4
	    	   write(unit=bfit(6:6),fmt='(I1)',iostat=ios)i
	    	   write(unit=bfit(8:),fmt='(f10.3)',iostat=ios)p(i)
	    	   bstr(1:)=bstr(1:lb)//bfit//tab
	    	   lb=lb+18
	  	 enddo
	         CALL GoBroad(lb,bstr)
C		 ! Test if fitted position is in scanned range
 350		 IF (pos .LE. MAX( x(1), x(pts) ) .AND. 
     1            pos .GE. MIN( x(1), x(pts) )) THEN
		    CALL Iset(psup,pos)
		    WRITE (UNIT=response(1:),FMT=
     1			'(''Current set to peak: '',f8.3,'' amps'')')pos
		    CALL Putline_NT (contr, response)
		 ELSE
		    CALL Putline_NT (contr, 
     1		   'Fitted peak position lies outside scanned region!')
		    IF (keep.and.arch)CALL ArchFile(psfile,12)
	            RETURN
		 END IF
	ELSE
	  bstr(1:)=bstr(1:lb)//'TYPE=NOCONV'
	  lb=lb+11
	  CALL Gobroad(lb,bstr)
		    WRITE (UNIT=response(1:),FMT=
     1		  '(''Fit did not converge. Ierr: '',i2)')ierr
		    CALL Putline_NT (contr, response)
		 IF (keep.and.arch)CALL ArchFile(psfile,12)
		 RETURN
	END IF
	      IF (keep) THEN !rewrite file here with fit info first
	        maxtry = 0
	        OPEN (unit=2,file=psfile,status='old',IOSTAT=ios)
		DO WHILE (ios .EQ. openfai .AND. maxtry .LT. notries)
		  CALL libwait(.2,status)
	          OPEN (unit=2,file=psfile,status='old',IOSTAT=ios)
		  maxtry = maxtry + 1
	        ENDDO
		IF (ios .NE. 0) GOTO 500 !err opening file. Give up.
 445		OPEN (UNIT=3,file=psfile,status='new')
		WRITE (3, '(a)') fitstr1
		WRITE (3, '(a)') fitstr2
 450		READ (2, '(a)', END= 499) response
		WRITE(3, '(a)') response
		GOTO 450
 499	       CLOSE(2)
		CLOSE (3)
	        Call getdir(subdir,right)		! Get name of subdir
	        info(1)(1:) = ' Executing:  FindPeak   '// 
	1              '                Datafile: '
	        info(1)(1:) = info(1)(1:50) //  subdir(1:right)//psfile(1:12)
	        info(2)(1:)= ' '
	        info(3)(1:)= ' '
	        info(4)(1:)= ' '
		call status_file(info)
	      ENDIF !keep


c-----------------------MODIFY FCAL?   ------------------------------------C

 500	CALL READ_STRING (keyb, inp, 'Set FCAL to peak value [YES]? ',
     1	     3,idum,idum,idum,idum,idum, contr)

	CALL UPCASE (inp, inp)

	CALL Putline_NF (contr, ' ')
	   IF (inp(1:1) .NE. 'N') THEN
             f_on(psup)%cur  = pos	          ! current
	     IF (nsta .eq. 2) THEN
	     flipx=psup/3+1 !find if mono (1) or anal (2)
	     posit = 2.  * 20.51   !BT7
	     if (flipx.eq.1)then !monochromator
	       dspace=dm
	       if (nsta .EQ. 2) posit=RdAngl(2) - z(2)
	     else !analyzer
	       dspace=da
	       if (nsta .EQ. 2) posit=RdAngl(6) - z(6)
	     endif
	     energy = 81.80310 / (2. * dspace * SIND(posit/2.))**2
	     f_on(psup)%engy = energy		  ! energy [meV]
	     END IF
	     WRITE (UNIT=response(1:), FMT= '(a,i1,a,f8.3,a,f8.3,a)') 
     1           'Flipper Calibration: PS no: ', psup, ' ==>  current: ',
     2	          f_on(psup)%cur, ' amps at ', f_on(psup)%engy, ' meV'
	     OPEN (UNIT= 1, FILE= MOTORS_BUF, STATUS= 'OLD', 
     1	          ACCESS= 'DIRECT', FORM= 'UNFORMATTED', RECL=MOTBUF_RECL)
	     WRITE (1,REC=MAXMOTS+5) (fixed(i), i= 1, MAXMOTS), 
     1			file, statistic, tempctr, 
     1		    polarized, psd,
     1              tdev, magnctr
	     WRITE (1,REC=MAXMOTS+6) (fullamp(i), i=1, pols), (f_on(i)%cur, i= 1, pols),
     2   	       (f_on(i)%engy, i=1, pols)
	     CLOSE(1)
	     CALL Putline_NT (contr, response)
           END IF

	IF (keep.and. arch)CALL ArchFile(psfile,12)
	END

