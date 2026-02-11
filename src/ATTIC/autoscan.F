#include "icp.h"
	include COMPILE_OPTS
	SUBROUTINE AutoScan (typ,bbuf)
C     	************************************

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C                                                                             C
C     Routine To Determine The Monitor When Fixed Measurement-Time Is Given   C
C     Called from: Prepare                                                    C
C                                                                             C
C     Beat Schmid, June 1989                                                  C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
	use smg_prep
	use instr
	IMPLICIT     none
	INTEGER*4    deltad, hour, min, sec, leng, pos, trm, counts(37), typ
	INTEGER*4    startime(2), deltatime(2), fintime(2)
	INTEGER*4    m, dspid, LIB$_DS /25/, LIB$_DD /22/, status
	REAL         rate, af, pts, time, abeg, inc, twait
	CHARACTER*1  str1
	CHARACTER*2  dd, hh, mm
	CHARACTER*4  mtype
	CHARACTER*6  str6
	CHARACTER*7  str7
	CHARACTER*8  str8
	CHARACTER*16 str16
	CHARACTER*23 sys_time, end_time
	CHARACTER*80 line

	TYPE(b_buf) bbuf


C------------------------------ CHECK FOR PRIV -------------------------------C

	CALL SMG$ERASE_DISPLAY (ascan, 1, 1)
	CALL SMG$PASTE_VIRTUAL_DISPLAY (ascan, paste, 5, 27)
	CALL PUTCHARS_BOLD (ascan, ' AUTOSCAN ', 1, 19)
	IF (.NOT. priv) THEN
	   CALL PUTCHARS (ascan, ' Sorry, no privilege for this command',
	1                      6, 6)
	   CALL LIBWAIT (2.0,status)
	   CALL SMG$UNPASTE_VIRTUAL_DISPLAY (ascan, paste)
	   RETURN
	END IF


	CALL PUTCHARS (ascan, ' CHANGES IN Q: ',3,2)
	CALL PUTCHARS (ascan, '  HS:', 4, 2)
	IF (abs(bbuf%hs).lt.10.)then
	  WRITE (UNIT= str8(1:7), FMT= '(f7.4)') bbuf%hs
	ELSE
	  WRITE (UNIT= str8(1:7), FMT= '(f7.3)') bbuf%hs
	ENDIF
	CALL PUTCHARS_REV (ascan, str8(1:7), 4, 7)
	CALL PUTCHARS (ascan, '  KS:', 4, 14)
	IF (abs(bbuf%ks).lt.10.)then
	  WRITE (UNIT= str8(1:7), FMT= '(f7.4)') bbuf%ks
	ELSE
	  WRITE (UNIT= str8(1:7), FMT= '(f7.3)') bbuf%ks
	ENDIF
	CALL PUTCHARS_REV (ascan, str8(1:7), 4, 26)
	CALL PUTCHARS (ascan, '  LS:', 4, 29)
	IF (abs(bbuf%ls).lt.10.)then
	  WRITE (UNIT= str8(1:7), FMT= '(f7.4)') bbuf%ls
	ELSE
	  WRITE (UNIT= str8(1:7), FMT= '(f7.3)') bbuf%ls
	ENDIF
	CALL PUTCHARS_REV (ascan, str8(1:7), 4, 34)


	CALL LIBWAIT (2.0,status)
	CALL SMG$UNPASTE_VIRTUAL_DISPLAY (ascan, paste)
	RETURN
	END
