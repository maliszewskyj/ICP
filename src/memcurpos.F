#include "icp.h"
	SUBROUTINE memCurPos(motorno,nwhichval,currentval)
C     	***********************************
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C                                                                             C
C   Update position information in motpos.buf for motor motorno               C
C	If nwhichval eq 1, write currentval as new motor position             C
C		(note: this is the hardware angle.)
C	If nwhichval eq 0, write currentval as new zero offset                C
C                                                                             C
C	Gayle Greene, August 2000                                             C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
        use instr
	IMPLICIT     none
	INTEGER*4    motorno, nwhichval, ios
	REAL*4       currentval, oldpos, oldzero
	REAL*4	     FZ         !function to return zero offset (needed for bt4)
	CHARACTER*5  whichstr
	CHARACTER*28 date
        CHARACTER*80 GETHOME

c     Note, cannot use unit 2 here because of yield adjusting acsf motors
 20	OPEN (UNIT= 3, FILE= MOTPOS_BUF, STATUS= 'OLD',
     1       ACCESS= 'DIRECT', FORM= 'UNFORMATTED',
     $       RECL=MOTBUF_RECL,ERR= 30)
	READ (3,REC=motorno) oldpos,oldzero
	IF (nwhichval .eq. 1) THEN
           IF (nsta .EQ. 4 .AND. z(motorno) .EQ. -99.) THEN
              oldzero = Fz(motorno,currentval) !bt4 zero can depend on position
           ENDIF
           WRITE (3,REC=motorno)currentval,oldzero
	ELSE
           WRITE (3,REC=motorno)oldpos,currentval
	ENDIF
	CLOSE (3)
	RETURN
C     record error in errors.icp
 30     CALL DATE_TIME (date)
	if (nwhichval .eq. 0) then
           whichstr(1:)='zero: '
	else
           whichstr(1:)='pos:  '
	endif
 	OPEN (UNIT=3, FILE= ERRORS_ICP, FORM= 'FORMATTED', 
     1       ACCESS= 'APPEND',
     $       STATUS= 'UNKNOWN', IOSTAT=ios)
	IF (ios .eq. 0) THEN    !file opened ok
	   write(unit=3,fmt='(1x,a,1x,a,1x,i3,1x,a5,1x,F10.2)',iostat=ios)
     1          date(1:17),'cannot update position in file for mot: ',
     1		motorno,whichstr,currentval
	   CLOSE(3)
	ENDIF
	RETURN
	END

