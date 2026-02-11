#include "icp.h"
	SUBROUTINE time_com(OUTSTR,LSTR,time,timemax,flagstr)
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C                                                                             C
C	Gayle Greene, November , 1995                                         C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C
	IMPLICIT NONE

C       Calling parameters
	CHARACTER*(*)	OUTSTR
	INTEGER*4	LSTR

	character*50	runcom
	character*20	flagstr
	INTEGER*4	lrc, ios, i
	REAL*4		time, timemax
	LOGICAL		isdigit,scrmes/.FALSE./

	time=0.			!default time in minutes
	timemax = 0.

	IF (outstr(1:4).EQ.'HOLD')THEN
	   i = 5
	   DO WHILE (.NOT. isdigit(outstr(i:i)) .AND. i .LT. lstr)
	      i = i + 1
	   ENDDO
	   read(unit=outstr(i:lstr),fmt=*,iostat=ios)time
	   timemax = time
	   flagstr(1:)=' '
	ELSEIF (outstr(1:2).EQ.'RR' .OR.
     1		outstr(1:2).EQ.'RI' .OR.
     1		outstr(1:2).EQ.'RB' .OR.
     1		outstr(1:2).EQ.'RQ' .OR.
     1		outstr(1:2).EQ.'RT' .OR.
     1		outstr(1:2).EQ.'RX') THEN !Run buffer(s)
	   runcom(1:)='HOWLONG'//outstr(2:lstr)
	   lrc = lstr+6
	   CALL HOWLONG(runcom,lrc,time,timemax,scrmes,flagstr)
	ENDIF

	RETURN
	END
