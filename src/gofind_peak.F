#include "icp.h"
	SUBROUTINE GoFind_Peak (callid,mot, range, delta, mon, keep, type, 
     1		   fitmin, fitinten, montemp, twotheta,arch,callpos,ierr,
     1		   fitok,fpos,halfh)
C	***************************************************

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C                                                                             C
C	Routine scans the given motor around the current angle within range   C
C	and tries to find a peak. If a peak is found, we prompt for fitting   C
C	or whatelse to do.                                                    C
C	type = 1 searches one angle 
C	type = 2, does a 2:1 scan                
C	type = 3 is for monitor peak                                          C
C	type = 4 is for special xray scan. Twotheta only used for fpx         C
C	type = 5 is for RM scan
C	type = 6 is for RA scan
C	type = 7 searches one angle with sigmoidal curve 
C       type = -x is for scan at detector x (FPD and FPL)		      C
C  At return:
C	fitok is true if fit was fully successful and 
C          fpos(1 & 2) have fitted positions of motors mot-1 and mot
C	Called from: Parse_Align                                              C
C                                                                             C
C	Beat Schmid, March 1990  (based on Ross Erwin)                        C
C       Modified by Gayle Greene					      C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
	use instr
	use histo
	use asd
	use globals
	IMPLICIT     none
	INTEGER*4    mot
	INTEGER*4    type, ierr, right
	INTEGER*4	callid
        REAL*4       range, delta, mon, callpos(2), maxpos1
	REAL*4       x(100), y(100), int
	REAL*4	     twotheta,  minpos1
 	CHARACTER*80 subdir, info(4)
	Logical fitmin,fitinten,keep, montemp, arch, fitok, halfh
	INTEGER*4     jmax, jmin, npt, errno, nit
	CHARACTER*12  psfile !name of data file
	REAL*4		zm,zm1, Fz
	REAL*4	      range2,mon2,facval,tmpint, fpos(2)

	IF (nsta .EQ. -5 .AND. use_asd)use_asdfile = .FALSE.

	fitok = .false. !default is no fit
	IF (type .lt. 0)THEN !verify channel selected within range of PSD/scalers--C
	  IF (PSD)THEN
	      IF (iabs(type) .gt. nocells) THEN
	        CALL Putcontr_NT (' Error in channel specified')
		ierr=3
		return !return
	      ENDIF
	  ELSE !not PSD
	      IF (iabs(type) .gt. scl) THEN !see if that scaler in use
	        CALL Putcontr_NT (' Error in scaler specified')
		ierr=3
		return
	      ENDIF
	  ENDIF
	ELSEIF (type .eq. 4)then !special polymers xray scan
	    mot = 4
	ENDIF

	IF (keep) THEN !name datafile
	  CALL namescan(callid,type,mot,psfile,errno)
	  IF (errno .LT. 0) THEN
	    ierr=4 !type of scan is not recognized
	    fitok =.false.
	    return
	  ELSEIF (errno .EQ. 1) THEN
	    keep = .FALSE.
	    CALL Putcontr_NT ('Error opening output file')
	  ENDIF
	ENDIF !keep


 	CALL scan_range(mot, range, delta, mon, keep, type, x, y, int,
     1            jmax, jmin, npt, maxpos1, ierr, montemp, minpos1,twotheta,
     1		  psfile,fitinten)
	IF ( ierr .NE. 0 ) RETURN

C  Loop until peak found if align command
	IF (callid .EQ. 3)THEN !align command
	  CALL Putcontr_NF ( ' ')
	  nit = 0
	  range2 = range
	  mon2 = mon
	  DO WHILE ( (y(jmax)-y(jmin) .LT. 4*SQRT(max(1.,y(jmax)))) .AND. 
     2		 (nit .LT. 3))
	    IF (finishup) GOTO 100 ! Bail if we have received the finish
	    IF (type .EQ. 2 .or. type.eq.4) THEN 
		zm1 = Fz(mot-1,callpos(1))
     		CALL Drive (mot-1, callpos(1)+zm1)
	    ENDIF
	    zm = Fz(mot,callpos(2))
	    CALL Drive (mot, callpos(2)+zm)
	    CALL Putcontr_NT('Cannot find peak. Increasing range and retrying.')
	    CALL Putcontr_NF ( ' ')
	    range2 = 2.0 * range2 ! Increase range
	    CALL YIELD		! NCM addition
	    CALL scan_range(mot, range2, delta, mon2, keep, type, x, y, int,
     1            jmax, jmin, npt, maxpos1, ierr,montemp, minpos1,
     1		  twotheta,psfile,fitinten)
	    IF ( ierr .NE. 0 ) return 
	    IF (fitmin)THEN
	      tmpint = y(jmax) - y(jmin)
	      IF (tmpint .LE. 0)tmpint = 1
	      facval = (4*SQRT(y(jmax))/tmpint)		
	    ELSE !fit to max as usual
	      IF ( y(jmax) .LE. y(jmin) ) y(jmax) = y(jmin) + 1
	      facval = (4*SQRT(y(jmax))/(y(jmax)-y(jmin)))
	    ENDIF
	    IF ( facval .GT. 1. ) THEN
	      mon2 = 1.5*facval*facval*mon2 ! Increase statistics?
	      IF (type .EQ. 2 .or. type.eq.4) THEN 
		 zm1 = Fz(mot-1,callpos(1))
     		 CALL Drive (mot-1, callpos(1)+zm1)
	      ENDIF
	      zm = Fz(mot,callpos(2))
	      CALL Drive (mot, callpos(2)+zm)
	      CALL YIELD
	      CALL scan_range(mot, range2, delta, mon2, keep, type, x, y, int,
     1            jmax, jmin, npt, maxpos1, ierr, montemp, minpos1,
     1		  twotheta,psfile,fitinten)
	      IF ( ierr .NE. 0 ) return 
	      IF (finishup) GOTO 100 ! Bail if we have received the finish
	    END IF
	    nit = nit + 1
	  END DO
	  IF ( nit .GE. 3 ) THEN
	    ierr = 1
	    return 
	  END IF
	ENDIF !align command

C  Finished scanning.

C  Take derivative of data if scan was of sigmoidal curve (type = 7)
 100	IF (type .eq. 7)THEN
	  CALL derive_curve( range, delta, x, y, int, jmax, jmin, npt, ierr)
	  IF ( ierr .NE. 0 ) RETURN
	ENDIF

C  Now look at what we found.
	CALL Exam_Peak(callid, mot, range, type, x, y, int,
     1            jmax, jmin, npt, maxpos1, ierr, minpos1, twotheta,
     1		  callpos, psfile, keep, fitmin, fitok,fpos,halfh)

	IF (keep .and. callid .LT.3)THEN !only find-peak commands
	        Call getdir(subdir,right)		! Get name of subdir
	        info(1)(1:) = ' Finished Find_Peak     '// 
	1              '                Datafile: '
	        info(1)(1:) = info(1)(1:50) //  subdir(1:right)//psfile(1:12)
	        info(2)(1:)= ' '
	        info(3)(1:)= ' '
	        info(4)(1:)= ' '
		call status_file(info)
	ENDIF
	IF (keep .and. arch) CALL archfile(psfile,12)

	RETURN
	END

