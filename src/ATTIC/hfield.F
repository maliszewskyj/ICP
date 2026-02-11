#include "icp.h"


	include COMPILE_OPTS
	SUBROUTINE HFIELD(field,fieldi,hwait,hfhow)
C     	****************************************

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C                                                                             C
C     Routine To Determine Magnetic Field Setting and Variance                C
C     Called from: Prepare                                                    C
C                                                                             C
C     Gayle Greene, February 1994                                             C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C

	IMPLICIT     none
	REAL	     field,fieldi,hwait, !field, incr, conversion, wait time
     1		hfhow !(0=not used,1=mon only,2=both,3=control)
	LOGICAL      chgfield,monfield
	INTEGER*4    leng, trm, i, iwait, ios
	CHARACTER*20 str20,errstr/'-----'/

	INCLUDE SMGPREP_CMN               ! Program specific SMG-definitions
	INCLUDE INSTR_CMN                ! instrument and hardware parameters
	INCLUDE TC_CMN 

	iwait = NINT(hwait)

C----------------------------------------------------------------------------C

	CALL SMG$ERASE_DISPLAY (moni, 1, 1)
	CALL SMG$PASTE_VIRTUAL_DISPLAY (moni, paste, 5, 27)
	CALL PUTCHARS_BOLD (moni, ' MAGNETIC FIELD ', 1, 19)
 	CALL PUTCHARS_REV (moni, 'RETURN: F20 or Ctrl-G', 12, 20)

	CALL PUTCHARS (moni, 'Initial Field  (user units): ', 5,2)
	CALL PUTCHARS (moni, 'Field Increment(user units): ', 6,2)
	CALL PUTCHARS (moni, 'Max Wait Per Point (secs)  : ', 7,2)
	CALL PUTCHARS (moni, ' (Wait Used in non-persitent mode on SC only)',
     1			  8,4)
	CALL PUTCHARS (moni, ' (Reply 0 to get default optimum wait times)',
     1			  9,4)
	IF (magnctr)THEN
	  CALL Putmax(hfconv,str20,20,5)
	  CALL PUTCHARS(moni,'Conversion constant: '//str20,10,2)
	ENDIF

	CALL Putmax(field,str20,20,4)
 	CALL PUTCHARS_REV (moni, str20, 5, 30)
	CALL Putmax(fieldi,str20,20,4)
 	CALL PUTCHARS_REV (moni, str20, 6, 30)
	write(unit=str20,fmt='(I20)',iostat=ios)iwait
c	CALL Putmax(hwait,str20,20,1) 
 	CALL PUTCHARS_REV (moni, str20, 7, 30)
	CALL CURSOR_ON (paste)
	GOTO 40


 38	CALL PUTCHARS_BLINK (moni, errstr, 5, 30)
	CALL Ring_Bell (moni,2)
 40	CALL CURSOR_ABS (moni, 5, 30)
	CALL SMG$READ_STRING (keyb, str20,,20,m$_nenr,,,leng,trm,moni)
	IF (trm .eq. SMG$K_TRM_F20 .or. trm .eq. 7) GOTO 99
	IF (LENG .GE. 1 )THEN
	  read(unit=str20(1:leng),fmt=*,err=38)field
	  CALL Putmax(field,str20,20,4)
	  CALL PUTCHARS_REV (moni, str20, 5, 30)
	  CALL CURSOR_ON (paste)
	ENDIF
	IF (trm .EQ. SMG$K_TRM_CR .OR. trm .EQ. SMG$K_TRM_DOWN .OR.
     1	     trm .EQ. SMG$K_TRM_BUFFER_FULL.or. trm .EQ. SMG$K_TRM_RIGHT)GOTO 50
	IF (trm .EQ. SMG$K_TRM_UP.or.trm.eq. SMG$K_TRM_LEFT) GOTO 60
	CALL Ring_Bell (moni,1)
	GOTO 40

 48	CALL PUTCHARS_BLINK (moni, errstr, 6, 30)
	CALL Ring_Bell (moni,2)
 50	CALL CURSOR_ABS (moni, 6, 30)
	CALL SMG$READ_STRING (keyb, str20,,20,m$_nenr,,,leng,trm,moni)
	IF (trm .eq. SMG$K_TRM_F20 .or. trm .eq. 7) GOTO 99
	IF (LENG .GE. 1 )THEN
	  read(unit=str20(1:leng),fmt=*,err=48)fieldi
	  CALL Putmax(fieldi,str20,20,4)
	  CALL PUTCHARS_REV (moni, str20, 6, 30)
	  CALL CURSOR_ON (paste)
	ENDIF
	IF (trm .EQ. SMG$K_TRM_CR .OR. trm .EQ. SMG$K_TRM_DOWN .OR.
     1	     trm .EQ. SMG$K_TRM_BUFFER_FULL.or. trm .EQ. SMG$K_TRM_RIGHT)GOTO 60
	IF (trm .EQ. SMG$K_TRM_UP.or.trm.eq. SMG$K_TRM_LEFT) GOTO 40
	CALL Ring_Bell (moni,1)
	GOTO 50

 58	CALL PUTCHARS_BLINK (moni, errstr, 7, 30)
	CALL Ring_Bell (moni,2)
 60	CALL CURSOR_ABS (moni, 7, 30)
	CALL SMG$READ_STRING (keyb, str20,,20,m$_nenr,,,leng,trm,moni)
	IF (trm .eq. SMG$K_TRM_F20 .or. trm .eq. 7) GOTO 99
	IF (LENG .GE. 1 )THEN
	  read(unit=str20(1:leng),fmt=*,err=58)iwait
	  write(unit=str20,fmt='(I20)',iostat=ios)iwait
	  hwait = FLOAT(iwait)
c	  CALL Putmax(hwait,str20,20,1)
	  CALL PUTCHARS_REV (moni, str20, 7, 30)
	  CALL CURSOR_ON (paste)
	ENDIF
	IF (trm .EQ. SMG$K_TRM_CR .OR. trm .EQ. SMG$K_TRM_DOWN .OR.
     1	     trm .EQ. SMG$K_TRM_BUFFER_FULL.or. trm .EQ. SMG$K_TRM_RIGHT)GOTO 40
	IF (trm .EQ. SMG$K_TRM_UP.or.trm.eq. SMG$K_TRM_LEFT) GOTO 50

 99	CALL CURSOR_OFF (paste)
	CALL SMG$UNPASTE_VIRTUAL_DISPLAY (moni, paste)

	END
