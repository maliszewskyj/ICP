#include "icp.h"
	SUBROUTINE Exam_Peak(callid, mot, range, type, x, y, int,
     1            jmax, jmin, npt, maxpos1, ierr, minpos1, twotheta,
     1		  current, psfile, keep, fitmin, fitok,fpos, halfh)
C	***************************************************

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C                                                                             C
C	Routine examines the intensity data collected during a scan. It       C
C        does a fit if appropriate.
C        It moves motors to final positions.
C At call:
C       callid = 1: find_peak
C	         2: find_oeak in command file
C                3: align_peak						      C
C	mot = motor number
C	range = range in degrees covered by scan with current position as center
C       keep is true if a file is to be kept of counts at each position
C	type = 1 searches one angle
C	type = 2, does a 2:1 scan                C
C	type = 3 is for monitor peak                                          C
C	type = 4 is for special xray scan. Twotheta only used for fpx         C
C	type = 5 is for RM scan
C	type = 6 is for RA scan
C	type = 7 searches one angle with sigmoidal curve 
C       type = -x is for scan at detector x (FPD and FPL)		      C
C	twotheta/2 is used as scan center for type=4
C	psfile = name of file in which scan data is stored
C	npt = number of points at which counts were made
C       x(1->npt) = position of mot at each point
C       y(1->npt) = counts at each point (fratio at each point for type 5 & 6)
C	e(1->npt) = intensity error at each point
C       int has intensity for fitting
C	jmax = index of x,y, e corresponding to the position with max intensity
C	jmin = index of x,y, e corresponding to the position with min intensity
C	maxpos1 = position of second angle when max intensity was obtained
C	minpos1 = position of second angle when min intensity was obtained
C  At return:
C	ierr = 0 if treatment proceeded without problem
C	fitok = .TRUE. if fitted position is a valid one
c	fpos(1 & 2) has fitted positions for one of two (if 2:1) angles
C                                                                             C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C	INCLUDE     INSTR_CMN
C	INCLUDE     SMGPREP_CMN
C	INCLUDE     HISTO_CMN
C	INCLUDE     TC_CMN
C	INCLUDE     ASD_CMN
	use instr
	use smg_prep
	use histo
	use tc
	use asd
	IMPLICIT     none

	LOGICAL	     keep, fitok
	INTEGER*4    mot, npt, callid
	INTEGER*4    type, ierr
	INTEGER*4    jmax, jmin
        REAL*4       range,  current(2), fitpos(2),
     1			maxpos1, minpos1, fpos(2)
	REAL*4       pos(2), RdAngl, x(100), y(100), e(100), int,
     1		     twotheta
	LOGICAL	     fitmin, halfh
	CHARACTER*12 psfile
 	CHARACTER*80 response
	REAL*4	     RdAm, zm, zm1, FZ, rfitint, p(6), diff, delta
	INTEGER*4    j, ij, leng, ios
	LOGICAL	     chgfile, peakfound, conv, Readef

	ierr = 3 !default if abort
	fitok = .FALSE. !default

C------------------------- SAY IF PEAK FOUND OR NOT --------------------------C

 300	IF (fitmin)THEN
	  fitpos(1) = minpos1
	  fitpos(2) = x(jmin)
	  rfitint = y(jmin)
	ELSE
	  fitpos(1) = maxpos1
	  fitpos(2) = x(jmax)
	  rfitint = y(jmax)
	ENDIF

	CALL Putcontr_NF ( ' ')
	peakfound = .FALSE.

	IF (type .EQ. 7)THEN !sigmoidal curve
	  IF (abs(y(jmax)-y(jmin)) .GT. 4*SQRT(MAX(1.,abs(rfitint)))) peakfound = .true.
	ELSE
	  IF (y(jmax)-y(jmin) .GT. 4*SQRT(MAX(1.,rfitint))) peakfound = .true.
	ENDIF
c	IF (y(jmax)-y(jmin) .GT. 4*SQRT(MAX(1.,y(jmax)))) peakfound = .true.
	IF (peakfound) THEN
	   IF ( type .EQ. 2 .or. type .EQ. 4) THEN
	      WRITE (UNIT=response(1:), FMT= 
     1	 '(''Peak found at '',f8.3,3x,f8.3, ''  with an intensity of '', i10)',
     1	       iostat=ios) fitpos(1), fitpos(2), NINT(rfitint)
	   ELSEIF ( type .EQ. 5 .or. type .EQ. 6) THEN !using flipping ratio
	      WRITE (UNIT=response(1:), FMT= 
     1	      '(''Peak found at '', f8.3,''  with a ratio of '', f10.3)',
     1		iostat=ios) fitpos(2), rfitint
	   ELSE !not type 2 or 4
	      WRITE (UNIT=response(1:), FMT= 
     1	      '(''Peak found at '', f8.3, ''  with an intensity of '', i10)',
     1		iostat=ios) fitpos(2), NINT(rfitint)
	   END IF
	   CALL Putcontr_NT(response)
	   IF (callid .EQ. 1)THEN !ask for interactive find_peak
	     IF (halfh)THEN
	       CALL READ_STRING (keyb, response, 
     1		'* Want me to find half height and drive there [Y]?', 1,
     1		ij,ij,ij,leng,ij, contr) !ij = dummy param for absoft
	     ELSE
	       CALL READ_STRING (keyb, response, 
     1		'* Want me to fit a gaussian and drive there [Y]?', 1,
     1		ij,ij,ij,leng,ij, contr) !ij = dummy param for absoft
	     ENDIF
	     CALL SCROLL_DISPLAY (contr)
	     CALL CURSOR_ABS (contr, 19, 1) 
	     IF (response(1:1) .EQ. 'N' .OR. response(1:1) .EQ. 'n') THEN
		ierr=0 !no error
		RETURN
	     ENDIF !response is no
	   ENDIF

	      do j = 1,npt
	   	e(j) = SQRT(Max(1.,y(j))) 	   ! calc. intensity error
	      enddo

	      IF (fitmin)THEN !subtract largest value from each point to reverse peak
		diff = y(jmax)
	        do ij = 1, npt
	          y(ij) = abs(y(ij) - diff)
	        enddo
	        p(1) = y(jmax)                      !lowest now  Background 
	      ELSE
	        p(1) = y(jmin)                                     ! Background 
	      ENDIF
	      int  = int - p(1) * range				   ! integ. int
              p(2) = 0.0
              p(3) = 0.0

	      IF (fitmin)THEN
	        p(4) = abs(y(jmin)-y(jmax))	                   ! Height
	        p(5) = x(jmin)                                     ! Pos
	      ELSE
	        p(4) = y(jmax) - p(1)		                   ! Height
	        p(5) = x(jmax)                                     ! Pos
	      ENDIF
	      IF(abs(p(4)) .LT. 1.) p(4) = 1.
	      IF (halfh) THEN !put half height pos in p(5),counts in p(4)
		CALL FindHalfH(npt,x,y,p,fitmin, conv) 
	      ELSE
	        p(6) = int * 2. * SQRT (log (2.) / 3.14159) / p(4)   ! Halfwidth
	        CALL Fit1Gauss (npt, x, y, e, p, ierr)
	        IF (ierr .EQ. 0) THEN                                ! Convergence
	          conv = .TRUE.
	        ELSEIF (callid .eq. 3 .AND. ierr .LT. 3)THEN !for ap, allow < 3
		  conv = .TRUE.
	        ELSE
	          conv = .FALSE.
	        ENDIF
	        chgfile = keep !update file if exists
	        IF (callid .EQ. 3) chgfile = chgfile .AND. conv !only if conv for ap 
	        Call writefit(p,conv,chgfile,psfile) !write out fit
	      ENDIF
	      IF (conv)THEN !					  ! Convergence
C		! Test if fitted position is in scanned range
 350		 IF (p(5) .LE. MAX( x(1), x(npt) ) .AND.  !in scanned range
	1            p(5) .GE. MIN( x(1), x(npt) )) THEN
		    fitok = .TRUE.
		    RdAm = RdAngl(mot)
		    zm = Fz(mot,RdAm)
		    pos(2) = RdAm - zm
	    	    IF (type .EQ. 2) THEN
		      RdAm = RdAngl(mot-1)
		      zm1 = Fz(mot-1,RdAm)
		      pos(1) = RdAm - zm1
		      delta  = 0.5 * ( pos(2) - p(5) )
		      CALL Drive (mot-1, pos(1)-delta+zm1  )
		      fpos(1) = pos(1)-delta
		    END IF
		    IF (type .EQ. 4) THEN !special xray
		       diff  = p(5) - twotheta/2. !=p(5) = 2theta/2 + diff, so
		       pos(1) = twotheta/2. - diff
		       CALL Drive (mot-1, pos(1)+z(mot-1)  )
		    ENDIF
	   	    CALL Drive (mot, p(5)+zm)
		    fpos(2) = p(5)
		    if (halfh)then
	   	      CALL Putcontr_NT ( 'Driving to halfheight position!')
		    else
	   	      CALL Putcontr_NT ( 'Driving to fitted peak position!')
		    endif
		 ELSE !not in scanned range
		     if (halfh) then
		       CALL Putcontr_NT 
     1			('Half height  position lies outside scanned region!')
		     else
		       CALL Putcontr_NT 
     1		      ('Fitted peak position lies outside scanned region!')
		     endif
		     CALL Putcontr_NT ( 'Driving to max intensity position!')
	   	     IF (type .EQ. 2 .or. type.eq.4) THEN
		        zm1 = Fz(mot-1,fitpos(1))
     			CALL Drive (mot-1, fitpos(1)+zm1)
		     ENDIF
		     zm = Fz(mot,fitpos(2))
	   	     CALL Drive (mot, fitpos(2)+zm)
		 END IF !in scanned range
	      ELSE  !no convergence
		 if (halfh) then
		   CALL Putcontr_NT('Err finding half height')
	         else
		   CALL Putcontr_NT(' Fit did not converge')
		 endif
	   	 IF (type .EQ. 2 .or. type.eq.4) THEN 
		      zm1 = Fz(mot-1,fitpos(1))
     		     CALL Drive (mot-1, fitpos(1)+zm1)
		 ENDIF
	         zm = Fz(mot,fitpos(2))
	   	 CALL Drive (mot, fitpos(2)+zm)
	   	 CALL Putcontr_NT ( 'Driving to peak position!')
	      END IF !convergence
	ELSE !peak not found
	  IF (type .EQ. 2 .or. type.eq.4) THEN 
		zm1 = Fz(mot-1,current(1))
     		CALL Drive (mot-1, current(1)+zm1)
	  ENDIF
		zm = Fz(mot,current(2))
	  CALL Drive (mot, current(2)+zm)

	   IF (type .ne.4)THEN !not special xray
	     CALL Putcontr_NT ( 
     1	     'No peak found   ===>  driving to start position!')
	   ELSE !special xray
	     CALL Putcontr_NT ( 
     1	     'No peak found   ===>  driving to 2theta position!')
	   ENDIF
	END IF !peak found
   	CALL Yield
	IF (READEF(contr_a))THEN         
	   CALL Putcontr_NT ( ' **** ABORT during Find_Peak ****')
	   RETURN
	END IF
	ierr= 0 !return without err
	END

