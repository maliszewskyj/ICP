#include "icp.h"
	include COMPILE_OPTS
	SUBROUTINE updte_X_Buf (xbuf, k, line, *)
C	*****************************************

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C                                                                             C
C     Routine Updates xbuffer.buf With New Values For One Buffer, Creates The C
C     New Menu And Returns Control To Statement Label * In The Calling        C
C     Program                                                                 C
C     Called from: PrepS                                                      C
C                                                                             C
C     Gayle Greene, March 1996                                                C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C


	IMPLICIT     none
	INCLUDE     SMGPREP_CMN      
	INCLUDE     INSTR_CMN
	INCLUDE     TC_CMN
	INCLUDE     DARTZ_CMN

	INTEGER*4    k, j, j0, row, wtc, i, ios, lastloop
	REAL         a1f, t, posit(12), hmax, fmax, zj, Fz
	LOGICAL      limit(6), found
	CHARACTER*2  str2
	CHARACTER*8  str8
	CHARACTER*80 line(nbuf), response

	RECORD /x_buf/ xbuf

	OPEN (UNIT= 1, FILE= 'XBUFFER.BUF', STATUS= 'OLD', ACCESS= 'DIRECT', 
	1     FORM= 'UNFORMATTED', SHARED,RECL=xbuf_RECL)
                     
	WRITE (1,REC=k) xbuf
	CLOSE (1)

	a1f = xbuf.a(1) + (xbuf.lpts(1) -1)* xbuf.i(1)
	WRITE (line(k),'(i2,1x,a35,1x,i2,1x,f8.2,f6.2,f8.2,i3,''*'',i7)',
     1		iostat=ios)
	2         k, xbuf.com(1:35),xbuf.mn(1), xbuf.a(1), 
     1		  xbuf.i(1),a1f, 
	3         NINT(xbuf.mpf), NINT(xbuf.mon)


c	a1f = xbuf.a(1) + (xbuf.pts -1)* xbuf.i(1)
c	WRITE (line(k),'(i2,1x,a20,2f8.2,f6.2,i3,''*'',i7,f6.1,f7.1,f8.0)')
c	2     k, xbuf.com (1:20), xbuf.a(1), a1f, xbuf.i(1),
c	3     NINT(xbuf.mpf), NINT(xbuf.mon), xbuf.tmp, xbuf.it, xbuf.tw


C--------- CALCULATE LOWER AND UPPER ANGLES AND COMPARE WITH LIMITS ----------C

	DO i = 1, 6 
	  j = xbuf.mn(i)
	  IF (j .NE. 0)THEN
	   IF (motfl(j)) THEN
	      posit(i)   = xbuf.a(i)
	      posit(i+6) = xbuf.a(i) + (xbuf.lpts(i)-1) * xbuf.i(i)
	   END IF
	  ENDIF
	  limit(i) = .FALSE.
	END DO
	row = 4
	DO j0 = 1, 12
	  IF (j0 .GT. 6) THEN
	      i = j0 - 6
	  ELSE
	      i = j0
	  END IF
	  j = xbuf.mn(i)
	  IF (j .NE. 0) THEN
	   IF (nsta.eq.4 .AND. z(j).eq.-99.)THEN !special flag
	        zj = Fz(j,posit(i))
	   ELSE 
	        zj = z(j)
	   ENDIF
	   t = posit(j0) + zj
	   IF (.NOT. limit(i)) THEN
	      IF (motfl(j) .AND. (t .LE. ll(j) .OR. t .GE. ul(j))) THEN
	         IF (row .EQ. 4) CALL SMG$ERASE_DISPLAY (prpx, 2, 18)
	         WRITE (UNIT= str2, FMT= '(i2)') j
	         WRITE (UNIT= str8, FMT= '(f8.3)') t
	         response(1:26) = '*** WARNING ***   Angle '// str2 
	         IF (t .LE. LL(j)) THEN
		    response(27:) = '  below lower limit: '// str8
	         ELSE IF (t .GE. ul(j)) THEN
		    response(27:) = '  above upper limit: '// str8
	         END IF
	         CALL PUTCHARS (prpx, response, row, 10)
	         row = row + 1
		 limit(i) = .TRUE.		 
      	      END IF
	   END IF
	  ENDIF !j ne 0
	END DO
	IF (xbuf.ihf .NE. 0 .or. xbuf.hf .NE. 0)THEN
	  hmax = xbuf.hf + (xbuf.pts-1)*xbuf.ihf
	  IF (magnctr)THEN
	    fmax = fieldmax * hfconv
	    IF (abs(hmax) .GT. fmax .OR. abs(xbuf.hf) .GT. fmax)THEN
	      IF (row .EQ. 4) CALL SMG$ERASE_DISPLAY (prpx, 2, 18)
	      response(1:)='*** WARNING *** Hfield out of range'
	      CALL PUTCHARS (prpx, response,row,10)
	      row = row+1
	    ENDIF
	  ENDIF
	ENDIF
	lastloop = 1
	DO i = 1, 6 
	  IF (xbuf.lpn(i) .GT. lastloop)lastloop = i
	ENDDO
	found = .false.
	DO i = 1, lastloop
	  DO j = 1, numxmots
	    IF (xbuf.lpn(j).EQ.i)found=.true.
	  ENDDO
	  IF (.not.found)THEN
	      IF (row .EQ. 4) CALL SMG$ERASE_DISPLAY (prpx, 2, 18)
	      response(1:)='*** WARNING *** LOOP SEQUENCE HAS GAPS'
	      CALL PUTCHARS (prpx, response,row,10)
	      row = row+1
	  ENDIF
	ENDDO
        IF (row .GT. 4) THEN
	   IF (nsta .EQ. 1) THEN
	      row = 7
	   ELSE
	      ROW = 8
	   END IF
	   CALL CURSOR_ABS (prpx, row, 48)
	   CALL SMG$SET_CURSOR_MODE (paste, SMG$M_CURSOR_OFF)
	   CALL SMG$READ_KEYSTROKE (keyb, wtc, 
	1       '--  hit any key to continue  --',, prpx)
	   CALL SMG$SET_CURSOR_MODE (paste, SMG$M_CURSOR_ON)
	END IF


	CALL SMG$CREATE_MENU (menux, line,,, 3)
	CALL SMG$ERASE_DISPLAY (prpx, 2, 1)
	RETURN 1
	END




	include COMPILE_OPTS
	SUBROUTINE updte_X_File (xbuf, line, *)
C	***************************************

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C                                                                             C
C     Routines Updates xbuffer.buf With One New Value For All Buffers,        C
C     Creates The New Menu And Returns Control To Statement Label * In The    C
C     Calling Program                                                         C
C     Called from: Parse, PrepI                                               C
C                                                                             C
C     Beat Schmid, June 1989                                                  C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C

	IMPLICIT     none
	INCLUDE     INSTR_CMN
	INCLUDE     SMGPREP_CMN      
	INCLUDE     DARTZ_CMN
	INTEGER*4    k, ios
	REAL         a1f
	CHARACTER*80 line(nbuf), help_line(3)

	RECORD /x_buf/ xbuf(nbuf)

	OPEN (UNIT= 1, FILE= 'XBUFFER.BUF', STATUS= 'OLD', ACCESS= 'DIRECT', 
	1     FORM= 'UNFORMATTED', SHARED,RECL=xbuf_RECL)

	DO k = 1, nbuf
	   WRITE (1,REC=k) xbuf(k) 
	   a1f = xbuf(k).a(1) + (xbuf(k).lpts(1) -1)* xbuf(k).i(1)
	   WRITE (line(k),'(i2,1x,a35,1x,i2,1x,f8.2,f6.2,f8.2,i3,''*'',i7)',
     1		  iostat = ios)
	2         k, xbuf(k).com(1:35),xbuf(k).mn(1), xbuf(k).a(1), 
     1		  xbuf(k).i(1),a1f, 
	3         NINT(xbuf(k).mpf), NINT(xbuf(k).mon)


c	   a1f = xbuf(k).a(1) + (xbuf(k).pts -1)* xbuf(k).i(1)
c	   WRITE (line(k),'(i2,1x,a20,2f8.2,f6.2,i3,''*'',i7,f6.1,f7.1,f8.0)')
c	2         k, xbuf(k).com (1:20), xbuf(k).a(1), a1f, xbuf(k).i(1),
c	3         NINT(xbuf(k).mpf), NINT(xbuf(k).mon), xbuf(k).tmp, 
c	4	  xbuf(k).it, xbuf(k).tw
	END DO
	CLOSE (1)

	CALL SMG$CREATE_MENU (menux, line,,, 3)


C---------------------------- UPDATE HELP SCREEN -----------------------------C

	help_line(1) = ' Type HELP for command list and syntax' 
	help_line(2) = ' '
	help_line(3) = ' '
	CALL Put_Help (help_line)
 
	RETURN 1
	END
