#include "icp.h"
	include COMPILE_OPTS
	SUBROUTINE AutoMon (typ, ibuf, qbuf, bbuf)
C     	************************************

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C                                                                             C
C     Routine To Determine The Monitor When Fixed Measurement-Time Is Given   C
C     Called from: Prepare                                                    C
C                                                                             C
C     Beat Schmid, June 1989                                                  C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C

	IMPLICIT     none
	INTEGER*4    deltad, hour, min, sec, leng, pos, trm, counts(37), typ
	INTEGER*4    startime(2), deltatime(2), fintime(2)
	INTEGER*4    m, dspid, LIB$_DS /25/, LIB$_DD /22/, status
	REAL         rate, af, pts, time, abeg, inc, twait, 
     1		     lth0,lth, lihf, fwait, lt0,ltinc
	CHARACTER*1  str1
	CHARACTER*2  dd, hh, mm
	CHARACTER*4  mtype
	CHARACTER*6  str6
	CHARACTER*7  str7
	CHARACTER*16 str16
	CHARACTER*23 sys_time, end_time
	CHARACTER*80 line
	LOGICAL	usewait,useholds,fieldwaits

	INCLUDE SMGPREP_CMN               ! Program specific SMG-definitions
	INCLUDE INSTR_CMN                ! instrument and hardware parameters

	RECORD /i_buf/ ibuf
	RECORD /q_buf/ qbuf
	RECORD /b_buf/ bbuf


C------------------------------ CHECK FOR PRIV -------------------------------C

	CALL SMG$ERASE_DISPLAY (moni, 1, 1)
	CALL SMG$PASTE_VIRTUAL_DISPLAY (moni, paste, 5, 27)
	CALL PUTCHARS_BOLD (moni, ' AUTOMONITOR ', 1, 19)
	IF (.NOT. priv) THEN
	   CALL PUTCHARS (moni, ' Sorry, no privilege for this command',
	1                      6, 6)
	   CALL LIBWAIT (2.0,status)
	   CALL SMG$UNPASTE_VIRTUAL_DISPLAY (moni, paste)
	   RETURN
	END IF


C------------------- DISPLAY INFO ACCORDING TO BUFFER ------------------------C

	IF (typ .EQ. 1) THEN				       ! const-I buffer
	   lt0 = ibuf.tmp
	   ltinc = ibuf.it
	   lth0 = ibuf.th0
	   lth = ibuf.th
	   lihf = ibuf.ihf
	   fwait = ibuf.hfw
	   twait = ibuf.tw
	   m = 1 			          ! find first motor that moves
	   DO WHILE ((.NOT. motfl(m) .OR. ibuf.i(m) .EQ. 0.) .AND. m .LE. 6)
	      m = m + 1
	   END DO	   
	   pts  = ibuf.pts
	   IF (m .LE. 6 .OR. ibuf.it .NE. 0.) THEN          ! something changes
           
!          C--------- DISPLAY ANGLES AND NUMBER OF POINTS --------------------C

	      IF (m .LE. 6) THEN		   ! angle(s) change during run
		 abeg = ibuf.a(m)
		 inc  = ibuf.i(m)
	         WRITE (UNIT= str1, FMT= '(i1)') m
	         str16(1:13) = ' A'// str1 //':  Start: '
	      ELSE                                        ! temperature changes
		 abeg = ibuf.tmp
	         inc  = ibuf.it
	         str16(1:13) = ' T:   Start: '
 	      END IF	      
 	      CALL PUTCHARS (moni, str16(1:13), 3, 2)
	      WRITE (UNIT= str6, FMT='(f6.2)') abeg
	      CALL PUTCHARS_BOLD (moni, str6, 3, 14)
	      CALL PUTCHARS (moni, ' Inc: ', 3, 21)
	      WRITE (UNIT= str6, FMT= '(f6.2)') inc
	      CALL PUTCHARS_BOLD (moni, str6, 3, 27)
	      CALL PUTCHARS (moni, ' End: ', 3, 34)
	      IF (pts .NE. 0.) THEN
	         af = abeg + (pts - 1) * inc
	      ELSE
	         af = abeg
	      END IF
	      WRITE (UNIT= str6, FMT= '(f6.2)') af
	      CALL PUTCHARS_BOLD (moni, str6, 3, 40) 
	   ELSE                                              ! no change at all
	      CALL PUTCHARS (moni, 
     1		' NO ANGLE MOVEMENT OR TEMPERATURE CHANGE', 3, 2)
	   END IF 	     
	ELSE IF (typ .EQ. 5) THEN			       ! Bragg buffer
	   lt0 = bbuf.tmp
	   ltinc = bbuf.it
	   lth0 = bbuf.th0
	   lth = bbuf.th
	   lihf = bbuf.ihf
	   fwait = bbuf.hfw
	   twait = bbuf.tw
	   m = 1 			          ! find first motor that moves
	   DO WHILE ((.NOT. motfl(m) .OR. bbuf.i(m) .EQ. 0.) .AND. m .LE. 6)
	      m = m + 1
	   END DO	   
	   pts  = bbuf.pts
	   IF (m .LE. 6 .OR. bbuf.it .NE. 0.) THEN          ! something changes
           
!          C--------- DISPLAY ANGLES AND NUMBER OF POINTS --------------------C

	      IF (m .LE. 6) THEN		   ! angle(s) change during run
		 abeg = bbuf.a(m)
		 inc  = bbuf.i(m)
	         WRITE (UNIT= str1, FMT= '(i1)') m
	         str16(1:13) = ' A'// str1 //':  Start: '
	      ELSE                                        ! temperature changes
		 abeg = bbuf.tmp
	         inc  = bbuf.it
	         str16(1:13) = ' T:   Start: '
 	      END IF	      
 	      CALL PUTCHARS (moni, str16(1:13), 3, 2)
	      WRITE (UNIT= str6, FMT='(f6.2)') abeg
	      CALL PUTCHARS_BOLD (moni, str6, 3, 14)
	      CALL PUTCHARS (moni, ' Inc: ', 3, 21)
	      WRITE (UNIT= str6, FMT= '(f6.2)') inc
	      CALL PUTCHARS_BOLD (moni, str6, 3, 27)
	      CALL PUTCHARS (moni, ' End: ', 3, 34)
	      IF (pts .NE. 0.) THEN
	         af = abeg + (pts - 1) * inc
	      ELSE
	         af = abeg
	      END IF
	      WRITE (UNIT= str6, FMT= '(f6.2)') af
	      CALL PUTCHARS_BOLD (moni, str6, 3, 40) 
	   ELSE                                              ! no change at all
	      CALL PUTCHARS (moni, 
     1		' NO ANGLE MOVEMENT OR TEMPERATURE CHANGE', 3, 2)
	   END IF 	     
	ELSE 	   
	   lt0 = qbuf.tmp
	   ltinc = qbuf.it
	   lth0 = qbuf.th0
	   lth = qbuf.th
	   lihf = qbuf.ihf
	   fwait = qbuf.hfw
	   pts   = qbuf.pts
	   twait = qbuf.tw
	   IF (qbuf.es .NE. 0.) THEN
	      CALL PUTCHARS (moni, 
     1	      ' CONST-Q: Monitor estimation may be inaccurate', 3, 2)
	   ELSE IF (qbuf.hs .NE. 0. .OR. qbuf.ks .NE. 0. .OR. qbuf.ls .NE. 0.)
     1     THEN
	      CALL PUTCHARS (moni, 
     1	    ' CONST-E SCAN: Make sure drum has correct energy now', 3, 2)
	   ELSE IF (qbuf.it .NE. 0.) THEN
	      abeg = qbuf.tmp
	      inc  = qbuf.it
	      str16(1:13) = ' T:   Start: '
 	      CALL PUTCHARS (moni, str16(1:13), 3, 2)
	      WRITE (UNIT= str6, FMT='(f6.2)') abeg
	      CALL PUTCHARS_BOLD (moni, str6, 3, 14)
	      CALL PUTCHARS (moni, ' Inc: ', 3, 21)
	      WRITE (UNIT= str6, FMT= '(f6.2)') inc
	      CALL PUTCHARS_BOLD (moni, str6, 3, 27)
	      CALL PUTCHARS (moni, ' End: ', 3, 34)
	      IF (pts .NE. 0.) THEN
	         af = abeg + (pts - 1) * inc
	      ELSE
	         af = abeg
	      END IF
	      WRITE (UNIT= str6, FMT= '(f6.2)') af
	      CALL PUTCHARS_BOLD (moni, str6, 3, 40) 
	   ELSE                                              ! no change at all
	      CALL PUTCHARS (moni, 
     1	      ' NO ANGLE MOVEMENT OR TEMPERATURE CHANGE', 3, 2)
	   END IF
	END IF


C----------------------------- GENERAL DISPLAY -------------------------------C

	CALL PUTCHARS (moni, ' ======> number of points: ', 4, 2)
	WRITE (UNIT= str6, FMT= '(i6)') NINT (pts)
	CALL PUTCHARS_BOLD (moni, str6, 4, 29)               
C  ask about hold and wait if not zero at this line (5)
 10	CALL SMG$ERASE_LINE (moni, 5, 1)
	CALL SMG$SET_CURSOR_MODE (paste, SMG$M_CURSOR_ON)
	CALL Cursor_Abs (moni, 5, 2)
	if (lth0 .ne. 0. .or. lth .ne. 0. .or. fwait .ne. 0. .or.
     1		(lt0 .ne. 0. .and. twait .ne. 0.))
     1		CALL PUTCHARS (moni, ' [Y/N]: Use ', 5, 2)               
	if (lth0 .ne. 0. .or. lth .ne. 0.)THEN
 	  CALL SMG$READ_STRING (keyb, str1,' Holds? ', 1,,,,,trm, moni)
	  IF (trm .EQ. SMG$K_TRM_F20) GOTO 99
	  IF (str1(1:1) .EQ. 'N' .OR. str1(1:1) .EQ. 'n') THEN
	    useholds = .FALSE.
	  ELSEIF (str1(1:1) .EQ. 'Y' .OR. str1(1:1) .EQ. 'y')THEN
	    useholds=.true.
	  ELSE
	    GOTO 10
	  ENDIF
	endif
	if (lt0 .ne. 0. .and. twait .ne. 0.)THEN
	  CALL SMG$READ_STRING (keyb, str1,' TempWait?: ', 1,,,,,trm, moni)
	  IF (trm .EQ. SMG$K_TRM_F20) GOTO 99
	  IF (str1(1:1) .EQ. 'N' .OR. str1(1:1) .EQ. 'n') THEN
	    usewait = .FALSE.
	  ELSEIF (str1(1:1) .EQ. 'Y' .OR. str1(1:1) .EQ. 'y')THEN
	    usewait = .true.
	  ELSE
	    GOTO 10
	  ENDIF
	endif
	if (fwait .ne. 0.)THEN
	  CALL SMG$READ_STRING (keyb, str1,' HFwait?: ', 1,,,,,trm, moni)
	  IF (trm .EQ. SMG$K_TRM_F20) GOTO 99
	  IF (str1(1:1) .EQ. 'N' .OR. str1(1:1) .EQ. 'n') THEN
	    fieldwaits = .FALSE.
	  ELSEIF (str1(1:1) .EQ. 'Y' .OR. str1(1:1) .EQ. 'y')THEN
	    fieldwaits=.true.
	  ELSE
	    GOTO 10
	  ENDIF
	endif
	IF (pts .LE. 1) THEN
	   CALL PUTCHARS (moni, ' Something wrong with your angle range ',
	1                      8, 2)
	   CALL PUTCHARS (moni, ' Press any key to go back ', 10, 2)
	   CALL SMG$READ_KEYSTROKE (keyb, trm)
	   CALL SMG$UNPASTE_VIRTUAL_DISPLAY (moni, paste)
	   RETURN      	
	END IF


C----------------------- ASK FOR DELTA DAYS AND TIME -------------------------C

	CALL LIB$DATE_TIME (sys_time)
	CALL PUTCHARS (moni, ' Todays day and time: ', 6, 2)
	CALL PUTCHARS_BOLD (moni, sys_time(1: 17), 6, 24)
	CALL SMG$SET_CURSOR_MODE (paste, SMG$M_CURSOR_ON)
    	CALL SMG$ERASE_DISPLAY (moni, 7, 1)
   20 	CALL SMG$ERASE_LINE (moni, 7, 1)
	CALL PUTCHARS_REV (moni, 'ABORT: F20', 12, 20)
	CALL Cursor_Abs (moni, 7, 2)
	CALL SMG$READ_STRING (keyb, dd, ' Give delta-day(s): ', 2,,,,, trm, 
	1		      moni)
	IF (trm .EQ. SMG$K_TRM_F20) GOTO 99
	READ (UNIT= dd, FMT= *, ERR= 20) deltad
   30 	CALL SMG$ERASE_LINE (moni, 7, 25)
	CALL Cursor_Abs (moni, 7, 25)
	CALL SMG$READ_STRING (keyb, str6, ' Time (hh:mm): ', 6,,,,leng, trm,
	1		      moni)
	IF (trm .EQ. SMG$K_TRM_F20) GOTO 99
	pos = INDEX (str6,':')
	IF (pos .EQ. 0) THEN
	   READ (UNIT= str6, FMT= *, ERR= 30) hour
	ELSE IF (pos .EQ. 1) THEN
	   hour = 0
	ELSE
	   READ (UNIT= str6(1:pos-1), FMT= *, ERR= 30) hour
	END IF
	IF (leng .GT. pos .AND. pos .NE. 0) THEN
	   READ (UNIT= str6(pos+1:leng), FMT= *, ERR= 30) min
	ELSE
	   min = 0
	END IF      
	IF (hour .GT. 24 .OR. min .GT. 59 .OR. (hour .EQ. 24 .AND. min .GT. 0))
	1 THEN
	   CALL SMG$RING_BELL (moni, 2)
	   CALL Cursor_Abs (moni, 9, 2)
	   CALL PUTLINE (moni, ' Wrong input format ...')
	   GOTO 30
	END IF


C--------------------------- SHOW THE TIME GIVEN -----------------------------C

	CALL SMG$ERASE_DISPLAY (moni, 9, 2)
	WRITE (UNIT= dd, FMT= '(i2.2)') deltad
	WRITE (UNIT= hh, FMT= '(i2.2)') hour
	WRITE (UNIT= mm, FMT= '(i2.2)') min
	CALL Cursor_Abs (moni, 8, 2)
	line = ' Intended end time is in '// dd(1:2) //' day(s) at '//
	1 hh(1:2) // ':'// mm(1:2)
	CALL PUTLINE (moni, line)
	CALL SMG$READ_STRING (keyb, str1,'  OK? [Y]/N: ', 1,,,,,, moni)
	IF (str1(1:1) .EQ. 'N' .OR. str1(1:1) .EQ. 'n') GOTO 10



C--------------------------- CALCULATE DELTA TIME ----------------------------C


	CALL LIB$CONVERT_DATE_STRING (sys_time, startime)   ! cnvrt to quadword
	IF (deltad .NE. 0) THEN
	   CALL LIB$CVT_TO_INTERNAL_TIME (LIB$_DD, deltad, deltatime)
	   CALL LIB$ADD_TIMES (startime, deltatime, fintime)  ! add the two times
	   CALL LIB$FORMAT_DATE_TIME (end_time, fintime)      ! convert to string
	ELSE
	   end_time = sys_time
	END IF
	end_time(13:14) = hh(1:2)                             ! end hour
	end_time(16:17) = mm(1:2)                             ! end minutes
	CALL LIB$CONVERT_DATE_STRING (end_time, fintime)      ! cnvrt to quadword
	CALL LIB$SUB_TIMES (fintime, startime, deltatime)     ! sub the two times
	CALL LIB$CVT_FROM_INTERNAL_TIME (LIB$_DS, sec, deltatime)  ! cnvrt to integer
	IF (sec .LE. 0) THEN
	   CALL SMG$RING_BELL (moni, 2)
	   CALL Cursor_Abs (moni, 9, 2)
	   CALL PUTLINE (moni, ' Time Problem ...')
	   GOTO 10
	END IF
	IF (useholds)THEN
	  sec = sec - NINT(pts*lth*60.) - NINT(lth0*60.)  !holds are in minutes
	ENDIF
	IF (usewait)THEN
	  IF (ltinc .eq. 0.0)THEN  !no temp incr.
	    sec = sec - NINT(twait*60.) !just wait for first point
	  ELSE
	    sec = sec - NINT(twait*60.*pts) !wait at each point
	  ENDIF
	ENDIF
	IF (fieldwaits)THEN
	  IF (lihf .eq. 0.0)THEN  !no temp incr.
	    sec = sec - NINT(fwait) !just wait for first point
	  ELSE
	    sec = sec - NINT(fwait*pts) !wait at each point
	  ENDIF
	ENDIF
	IF (sec .LE. 0) THEN
	   CALL SMG$RING_BELL (moni, 2)
	   CALL Cursor_Abs (moni, 9, 2)
	   CALL PUTLINE (moni, ' Time Problem ...')
	   GOTO 10
	END IF


C----------------- CALCULATE MONITOR BASED ON CURRENT FLUX -------------------C

	CALL PUTCHARS_BOLD (moni,'===>  MEASURING', 9, 29)
	time = 10.
	CALL CnTime (time)
	CALL Scaler (counts)
	rate = counts (1) / time            
	IF (rate .LE. 5.) THEN 
	CALL SMG$ERASE_DISPLAY (moni)
	CALL PUTCHARS (moni, 
     1	' WARNING: rate is very low, beam tube may be closed', 6, 1)
	CALL PUTCHARS (moni, ' Monitor not changed !!', 7, 1)
	CALL LIBWAIT (2.0,status)
	CALL SMG$UNPASTE_VIRTUAL_DISPLAY (moni, paste)
	RETURN
	END IF       
	IF (typ .EQ. 1) THEN
	   ibuf.mon = FLOAT(sec) * rate / pts / ibuf.mpf
	   ibuf.mt  = 'NEUT'
	   WRITE (UNIT= str7, FMT= '(i7)') NINT(ibuf.mon)
	   CALL PUTCHARS_REV (prpi, str7, mon_line, 19, 0)
	   CALL PUTCHARS_REV (prpi, ibuf.mt, mon_line, 48, 0)
	ELSE IF (typ .EQ. 5) THEN !Bragg
	   bbuf.mon = FLOAT(sec) * rate / pts / bbuf.mpf
	   bbuf.mt  = 'NEUT'
	   WRITE (UNIT= str7, FMT= '(i7)') NINT(bbuf.mon)
	   CALL PUTCHARS_REV (prpB, str7, mon_line, 19, 0)
	   CALL PUTCHARS_REV (prpB, bbuf.mt, mon_line, 46, 0)
	ELSE
	   qbuf.mon = FLOAT(sec) * rate / pts / qbuf.mpf
	   qbuf.mt  = 'NEUT'
	   WRITE (UNIT= str7, FMT= '(i7)') NINT(qbuf.mon)
	   CALL PUTCHARS_REV (prpq, str7, mon_line, 19)
	   CALL PUTCHARS_REV (prpq, qbuf.mt, mon_line, 46)
	   CALL PUTCHARS_REV (moni, 'Monitor updated', 12, 1)
	END IF
	CALL Cursor_Abs (moni, 12, 30)
	CALL SMG$READ_STRING (keyb, str1,'RETURN? [Y]/N: ', 1,,,,,, moni)
	IF (str1(1:1) .EQ. 'N' .OR. str1(1:1) .EQ. 'n') GOTO 10
  99  	CALL SMG$SET_CURSOR_MODE (paste, SMG$M_CURSOR_OFF)
	CALL SMG$UNPASTE_VIRTUAL_DISPLAY (moni, paste)
	END
