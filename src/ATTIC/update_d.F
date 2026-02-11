#include "icp.h"
	include COMPILE_OPTS
	SUBROUTINE updte_D_Buf (dbuf, k, line, *)
C	*****************************************
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C                                                                             C
C     Routine Updates dbuffer.buf With New Values For One Buffer, Creates The C
C     New Menu And Returns Control To Statement Label * In The Calling        C
C     Program                                                                 C
C     Called from: PrepS                                                      C
C                                                                             C
C     Gayle Greene, March 1996                                                C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
	IMPLICIT     none
	INCLUDE INSTR_CMN      ! Instrument and hardware parameters
	INCLUDE SMGPREP_CMN
	INCLUDE TC_CMN
	INCLUDE DARTZ_CMN
	INTEGER*4    k, j, j0, row, wtc, i, ios, lastloop
	REAL         a1f, t, posit(12), hmax, fmax, zj, Fz
	LOGICAL      limit(6), found, seenx, seeny, seenz, xyzprob
	CHARACTER*2  str2
	CHARACTER*8  str8
	CHARACTER*80 line(nbuf), response
	RECORD /d_buf/ dbuf
	OPEN (UNIT= 1, FILE= 'DBUFFER.BUF', STATUS= 'OLD', ACCESS= 'DIRECT', 
	1     FORM= 'UNFORMATTED', SHARED,RECL=dbuf_RECL)
	WRITE (1,REC=k) dbuf
	CLOSE (1)
	a1f = dbuf.a(1) + (dbuf.lpts(1) -1)* dbuf.i(1)
	WRITE (line(k),'(i2,1x,a35,1x,i2,1x,f8.2,f6.2,f8.2,i3,''*'',i7)',
     1		iostat=ios)
	2         k, dbuf.com(1:35),dbuf.mn(1), dbuf.a(1), 
     1		  dbuf.i(1),a1f, 
	3         NINT(dbuf.mpf), NINT(dbuf.mon)
c	a1f = dbuf.a(1) + (dbuf.pts -1)* dbuf.i(1)
c	WRITE (line(k),'(i2,1x,a20,2f8.2,f6.2,i3,''*'',i7,f6.1,f7.1,f8.0)')
c	2     k, dbuf.com (1:20), dbuf.a(1), a1f, dbuf.i(1),
c	3     NINT(dbuf.mpf), NINT(dbuf.mon), dbuf.tmp, dbuf.it, dbuf.tw
C--------- CALCULATE LOWER AND UPPER ANGLES AND COMPARE WITH LIMITS ----------C
C--------- Also check that x, y, and z have all been declared
	seenx = .false.
	seeny = .false.
	seenz = .false.
	DO i = 1, 6 
	  if (dbuf.xyz(i) .eq. xcoord) then
		seenx = .true.
	  elseif (dbuf.xyz(i) .eq. ycoord) then
		seeny = .true.
	  elseif (dbuf.xyz(i) .eq. zcoord) then
		seenz = .true.
	  endif
	  j = dbuf.mn(i)
	  IF (j .NE. 0)THEN
	   IF (motfl(j)) THEN
	      posit(i)   = dbuf.a(i)
	      posit(i+6) = dbuf.a(i) + (dbuf.lpts(i)-1) * dbuf.i(i)
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
	  j = dbuf.mn(i)
	  IF (j .NE. 0) THEN
	   IF (nsta.eq.4 .AND. z(j).eq.-99.)THEN !special flag
	        zj = Fz(j,posit(i))
	   ELSE 
	        zj = z(j)
	   ENDIF
	   t = posit(j0) + zj
	   IF (.NOT. limit(i)) THEN
	      IF (motfl(j) .AND. (t .LE. ll(j) .OR. t .GE. ul(j))) THEN
	         IF (row .EQ. 4) CALL SMG$ERASE_DISPLAY (prpd, 2, 18)
	         WRITE (UNIT= str2, FMT= '(i2)') j
	         WRITE (UNIT= str8, FMT= '(f8.3)') t
	         response(1:26) = '*** WARNING ***   Angle '// str2 
	         IF (t .LE. LL(j)) THEN
		    response(27:) = '  below lower limit: '// str8
	         ELSE IF (t .GE. ul(j)) THEN
		    response(27:) = '  above upper limit: '// str8
	         END IF
	         CALL PUTCHARS (prpd, response, row, 10)
	         row = row + 1
		 limit(i) = .TRUE.		 
      	      END IF
	   END IF
	  ENDIF !j ne 0
	END DO
	IF (dbuf.ihf .NE. 0 .or. dbuf.hf .NE. 0)THEN
	  hmax = dbuf.hf + (dbuf.pts-1)*dbuf.ihf
	  IF (magnctr)THEN
	    fmax = fieldmax * hfconv
	    IF (abs(hmax) .GT. fmax .OR. abs(dbuf.hf) .GT. fmax)THEN
	      IF (row .EQ. 4) CALL SMG$ERASE_DISPLAY (prpd, 2, 18)
	      response(1:)='*** WARNING *** Hfield out of range'
	      CALL PUTCHARS (prpd, response,row,10)
	      row = row+1
	    ENDIF
	  ENDIF
	ENDIF
	lastloop = 1
	DO i = 1, 6 
	  IF (dbuf.lpn(i) .GT. lastloop)lastloop = i
	ENDDO
	found = .false.
	DO i = 1, lastloop
	  DO j = 1, numdmots
	    IF (dbuf.lpn(j).EQ.i)found=.true.
	  ENDDO
	  IF (.not.found)THEN
	      IF (row .EQ. 4) CALL SMG$ERASE_DISPLAY (prpd, 2, 18)
	      response(1:)='*** WARNING *** LOOP SEQUENCE HAS GAPS'
	      CALL PUTCHARS (prpd, response,row,10)
	      row = row+1
	  ENDIF
	ENDDO
	IF (.not. seenx .or. .not. seeny .or. .not. seenz) THEN
	      IF (row .EQ. 4) CALL SMG$ERASE_DISPLAY (prpd, 2, 18)
	      response(1:)='*** WARNING ***  X  Y  Z  NOT ALL DECLARED'
	      CALL PUTCHARS (prpd, response,row,10)
	      row = row+1
	ENDIF
        IF (row .GT. 4) THEN
	   IF (nsta .EQ. 1) THEN
	      row = 7
	   ELSE
	      ROW = 8
	   END IF
	   CALL CURSOR_ABS (prpd, row, 48)
	   CALL SMG$SET_CURSOR_MODE (paste, SMG$M_CURSOR_OFF)
	   CALL SMG$READ_KEYSTROKE (keyb, wtc, 
	1       '--  hit any key to continue  --',, prpd)
	   CALL SMG$SET_CURSOR_MODE (paste, SMG$M_CURSOR_ON)
	END IF
	CALL SMG$CREATE_MENU (menud, line,,, 3)
	CALL SMG$ERASE_DISPLAY (prpd, 2, 1)
	RETURN 1
	END

	include COMPILE_OPTS
	SUBROUTINE updte_D_File (dbuf, line, *)
C	***************************************
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C                                                                             C
C     Routines Updates dbuffer.buf With One New Value For All Buffers,        C
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
	RECORD /d_buf/ dbuf(nbuf)
	OPEN (UNIT= 1, FILE= 'DBUFFER.BUF', STATUS= 'OLD', ACCESS= 'DIRECT', 
	1     FORM= 'UNFORMATTED', SHARED,RECL=dbuf_RECL)
	DO k = 1, nbuf
	   WRITE (1,REC=k) dbuf(k) 
	   a1f = dbuf(k).a(1) + (dbuf(k).lpts(1) -1)* dbuf(k).i(1)
	   WRITE (line(k),'(i2,1x,a35,1x,i2,1x,f8.2,f6.2,f8.2,i3,''*'',i7)',
     1		  iostat = ios)
	2         k, dbuf(k).com(1:35),dbuf(k).mn(1), dbuf(k).a(1), 
     1		  dbuf(k).i(1),a1f, 
	3         NINT(dbuf(k).mpf), NINT(dbuf(k).mon)
c	   a1f = dbuf(k).a(1) + (dbuf(k).pts -1)* dbuf(k).i(1)
c	   WRITE (line(k),'(i2,1x,a20,2f8.2,f6.2,i3,''*'',i7,f6.1,f7.1,f8.0)')
c	2         k, dbuf(k).com (1:20), dbuf(k).a(1), a1f, dbuf(k).i(1),
c	3         NINT(dbuf(k).mpf), NINT(dbuf(k).mon), dbuf(k).tmp, 
c	4	  dbuf(k).it, dbuf(k).tw
	END DO
	CLOSE (1)
	CALL SMG$CREATE_MENU (menud, line,,, 3)
C---------------------------- UPDATE HELP SCREEN -----------------------------C
	help_line(1) = ' Type HELP for command list and syntax' 
	help_line(2) = ' '
	help_line(3) = ' '
	CALL Put_Help (help_line)
	RETURN 1
	END
