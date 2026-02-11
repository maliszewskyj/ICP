#include "icp.h"
	SUBROUTINE Status_Line
C	**********************

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C                                                                             C
C     Routine To Update The Status Line After A Change Of A Displayed         C
C     Parameter                                                               C
C     Called from: Parse_Contr                                                C
C                                                                             C
C     Beat Schmid,  August 1989                                               C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
	use instr
	use smg_prep
	use polar
	use tc
	use refl
	use histo
	use focus
	use asd
	IMPLICIT NONE

	INTEGER*4     j, k, l, m
	CHARACTER*2   str2(5),psdstr,magstr,focstr,rkstr,asdstr,alstr,vbstr
	CHARACTER*13  fix
	CHARACTER*17  retstr
	CHARACTER*80  response
	CHARACTER*5   supstr
 	CHARACTER*12  p17/'Status Flags'/
        CHARACTER*80  gethome

	if (pgaa) THEN !interprocess communication between Lindstrom PGAA & NG7
	  retstr(1:)='     **PGAA**    '
	else
 	  retstr(1:)=' '
	endif
	str2(1) = 'W+'
	IF (.NOT. makelog) str2(1) = 'W-'
	str2(2) = 'S+'
	IF (.NOT. statistic) str2(2) = 'S-'
	str2(3) = 'T-'
	IF (tempctr) str2(3) = 'T+'
	IF (tempctr .and. dbcurve) str2(3) = 'T#'
	IF (tempctr .AND. tcareful) str2(3) = 'T*'
	str2(4) = 'P-'
	IF (polarized) str2(4) = 'P+'
	asdstr = 'A-'
	IF (use_asd)THEN
	  IF (keep_rawasd)THEN
	    asdstr = 'A*'
	  ELSE
	    asdstr = 'A+'
	  ENDIF
	ENDIF
	alstr = 'A-' !only shown for bt1 at present
	IF (autolim)alstr = 'A+'
	psdstr= 'L-'
	IF (psd) then
	  IF (liqwait.eq.0.) THEN
	    psdstr = 'L+'
	  ELSE
	    psdstr = 'L*'
	  ENDIF
	ELSE !not psd
	  IF (nsta .eq. -7 .and. liqwait.ne.0.) THEN
	    retstr(1:)='        Q+      '
	  ENDIF
	ENDIF
	IF (negqpos)retstr(1:)='!!Q+ CCW A3,A4!!'
	focstr= 'F-'
	IF (focus5) focstr='F+'
	magstr= 'H-'
	IF (magnctr) THEN
	  IF (hfmode .EQ. 0)THEN !mag is not in persistent mode
	    magstr = 'H+'
	  ELSE
	    magstr = 'H*'
	  ENDIF
	ENDIF
	rkstr = 'R+'
	IF (.NOT. rockfl) rkstr = 'R-'
	vbstr='  '
	IF (verbose_run)vbstr='V+'
	IF (super) THEN
	  supstr = 'SUPER'
	ELSE
	  supstr(1:) = ' '
	ENDIF
	IF (nsta .eq. 4) THEN
	  IF (trashmode) THEN
	     supstr = '  TR+'
	  ELSE
	     supstr = '  TR-'
	  ENDIF
	ENDIF
	IF (nsta .eq. 1 .and. .not. super)THEN
	     l = 3
	     m = 4
	ELSE
	     l = 1
	     m = MAXMOTS
	ENDIF
	k = 0
	fix(1:) = ' '
	DO j = l,m
	   IF (fixed(j).AND.motfl(j)) THEN
	      WRITE (UNIT= str2(5), FMT= '(i2)') j
	      IF (k .EQ. 0) THEN
		 fix(1:2) = str2(5)
	      ELSE
		 fix = fix(1:k) // str2(5)
	      END IF
	      k = k + 2
	   END IF
	END DO
	IF (k .GT. 0) fix(1:) = 'F:'// fix 
	IF (k .GT.10) fix(13:13)='*'

	IF (abs(nsta).eq.7 .OR. nsta .EQ. -1.or.nsta.eq.8) then !only bt7, ng7, ng1, bt8
	   response = p17//'  '//supstr//' '//vbstr//' '//psdstr//' '
	1	// str2(1)//' '// 
	1	str2(2) //' '// str2(3) //' '//magstr//' '
	2	// str2(4) //' '
	2	// fix //retstr
	ELSEIF ((nsta).eq.-5) then !should be only nsta = -5
	   response = p17//supstr//' '//vbstr//' '//asdstr//' '//focstr//' '
	1	// str2(1)//' '// 
	1	str2(2) //' '// str2(3) //' '//magstr//' '
	2	// str2(4) //' '
	2	// fix //retstr
	ELSEIF ((nsta).eq. 1) then !should be only bt1
	   response = p17//'  '//supstr//'   '//alstr//'  '//rkstr//'  '// 
	1	str2(1)//'  '// 
	1	str2(2) //'  '// str2(3) //'  '//magstr//'  '
	2	// str2(4) //'  '
	2	// fix //retstr
	ELSE			!all other stations
	   response = p17//'  '//supstr//'  '//vbstr//'   '// str2(1)//'  '// 
	1	str2(2) //'  '// str2(3) //'  '//magstr//'  '
	2	// str2(4) //'  '
	2	// fix //retstr
	endif

	CALL CURSOR_ABS (stat, 1, 1)
	CALL PUTLINE_REVERSE (stat, response)
	CALL CURSOR_ABS (contr, 19, 1)
	clrstat = .FALSE.
	IF (.NOT. priv) RETURN


C--------------------- UPDATE FLAG STATUS IN MOTORS.BUF ----------------------C

	OPEN (UNIT= 1, FILE= MOTORS_BUF, STATUS= 'OLD',
	1    ACCESS= 'DIRECT', FORM= 'UNFORMATTED',
	1    RECL=MOTBUF_RECL)
	WRITE (1,REC=MAXMOTS+5) (fixed(j), j= 1, MAXMOTS), file,
	2    statistic, tempctr, polarized, psd, tdev, magnctr
	WRITE (1,REC=MAXMOTS+6) (fullamp(j), j=1, pols),
	3    (f_on(j)%cur, j= 1, pols),
	2    (f_on(j)%engy, j=1, pols)
	WRITE (1,REC=MAXMOTS+7) focus5,fdad,fdsa,asd_xmin,asd_ymin,
	1    asd_xmax,asd_ymax,asd_numx,asd_numy,negqpos 
	CLOSE(1)
	END
