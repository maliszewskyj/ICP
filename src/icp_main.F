#include "icp.h"
	PROGRAM ICP_MAIN
C	****************

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C                                                                             C
C     Main Routine for Instrument Control Program   (ICP)                     C
C                                                                             C
C     Beat Schmid, June 1989                                                  C
C     Modifications: Gayle Greene
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C

      
C=============================================================================C
C                                                                             C
C	History:                                                              C
C	- Version 1.0: Initial Release January 1990                           C
C=============================================================================C

	use smg_prep
	use instr
	use tc
	use dartz
	use iso_c_binding
	IMPLICIT     none
	interface
#include "c_debugval.h"
	end interface
	INTEGER*4    choice,day_no,status,context/0/
	INTEGER*4    	i, motline, tmpline, ios, iargc
	CHARACTER*1  str1
	CHARACTER*2  str2
	CHARACTER*4  str4
	CHARACTER*28 day_time, sys_time
	CHARACTER*14 item(3) /'PREPARE MODE', 'CONTROL MODE', 'EXIT'/
	CHARACTER*16 item2(4) /'Increment Buffer', 'Const.-Q Buffer',
     1    'Bragg Buffer   ', ' '/
	CHARACTER*16 item3(2) /'X Buffer', 'D Buffer'/
	CHARACTER*35 version
	CHARACTER*80 help_line(3)
	CHARACTER*10 tcontroller
	CHARACTER*80 MR, ftmp
	CHARACTER*80 GETHOME

C------------------------------ INITIALIZATION -------------------------------C

	version = 'Version 29 -- Nov 2020 (the year to forget)'

	CALL DEBUGLEVEL(0)
	IF (iargc() .gt. 0) THEN
	   CALL getarg(1,MR)
	   IF (index(MR,'-debug') .gt. 0) CALL DEBUGLEVEL(1)
        ENDIF
	CALL Instrini                      ! Instrument and hardware parameters
	CALL SmgIni                                         ! Screen-management

C-----------------------------Create monitor.rec if not there ----------------C
	CALL FIND_FILE (MONITOR_REC, ftmp, context,status)
	CALL FIND_FILE_END(context)

C---------------------------- DISPLAY TITLE MENU -----------------------------C
	CALL PUTCHARS_BOLD (main, header, 1, 36)
	CALL PUTCHARS_BOLD (main, version, 2, 36)
	WRITE (UNIT= str4, FMT= '(i4)') abs(nsta)
	CALL PUTCHARS (main, 'Station number: '// str4, 14, 4)
	CALL SCROLL_DISPLAY (main)
	IF (.NOT. priv) THEN
	   CALL PUTCHARS (main,'You are on a nonprivileged line', 15, 40)
	   CALL SCROLL_DISPLAY (main)
	ENDIF

	IF (nsta .EQ. 1) THEN	! No choice here
	   which_one = 1
	ELSE IF (nsta .EQ. 8) THEN ! Dartz buffer
	   which_one = 6
	ELSE IF (nsta .EQ. 4) THEN ! const-I, const-Q, Trash
	   which_one = 4
	ELSE IF (nsta .EQ. -7) THEN ! Refl buffer
	   which_one = 5
	ELSE			! const-I
	   which_one = 1
	END IF
	CALL Control
	CALL Goodbye

	END      
