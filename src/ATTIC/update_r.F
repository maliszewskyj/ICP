#include "icp.h"
	include COMPILE_OPTS
	SUBROUTINE Updte_R_Buf (Rbuf, k, line, *)
C	*****************************************

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C                                                                             C
C     Routine Updates Rbuffer.buf With New Values For One Buffer, Creates The C
C     New Menu And Returns Control To Statement Label * In The Calling        C
C     Program                                                                 C
C     Called from: PrepR                                                      C
C                                                                             C
C     Gayle Greene, FEB 1993                                                  C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C


	IMPLICIT     none
	INCLUDE     SMGPREP_CMN      
	INCLUDE     REFL_CMN      
	INCLUDE     INSTR_CMN
	INCLUDE     TC_CMN

	INTEGER*4    k, j, j0, row, wtc,kj, i, ios
	REAL         qzf,qxf, t, posit(maxmots),qznow,qxnow,alphai,alphaf,
     1			hmax,fmax
	LOGICAL      limit, inlimits
	CHARACTER*1  str1
	character*2  str2
	CHARACTER*8  str8
	CHARACTER*80 line(nbuf), response
	INTEGER*4    tomove,motlist(10)

	RECORD /r_buf/ rbuf

	OPEN (UNIT= 1, FILE= 'RBUFFER.BUF', STATUS= 'OLD', ACCESS= 'DIRECT', 
	1     FORM= 'UNFORMATTED', SHARED,RECL=RBUF_RECL)
                     
	WRITE (1,REC=k) rbuf
	CLOSE (1)

	qzf = rbuf.qz + (rbuf.pts -1)* rbuf.qzi
	qxf = rbuf.qx + (rbuf.pts -1)* rbuf.qxi
	   IF (rbuf.mon1 .eq. 0.) THEN
	     write(str8,fmt='(i8)',iostat=ios)NINT(rbuf.mon0)
	   ELSE
	     str8='  varies'
	   ENDIF
	   WRITE (line(k),'(i2,1x,a20,f9.6,f10.6,f9.6,f10.6,i3,''*'',a8,i5)',
     1		   iostat=ios)
	2         k, rbuf.com(1:20), rbuf.qz, qzf, rbuf.qx,qxf, 
	3         NINT(rbuf.mpf), str8, NINT(rbuf.pts) 



	row = 4

c- For 2.35 Angstroms, use PG filter.  Otherwise use Be filter
	if (wavel.lt.3) then
	   tomove = PGmove
	   DO i = 1,tomove
	     motlist(i) = PGlist(i)
	   ENDDO
	else   
	   tomove = Bemove
	   DO i = 1,tomove
	     motlist(i) = Belist(i)
	   ENDDO
	endif


	IF (wavel.eq.0. .or. dm .eq. 0.) THEN
	  CALL SMG$ERASE_DISPLAY (prpr, 2, 18)
	  response(1:) =
     1	  'Cannot calculate motor positions to compare with limits because '
	  CALL PUTCHARS (prpr, response, row, 10)
	  row = row + 1
	  response(1:) = ' '
	  if (wavel.eq.0) then
	    response(34:)=' wavelength = 0.'
	  else
	    response(34:)=' dm = 0.'
	  endif
	  CALL PUTCHARS (prpr, response, row, 10)
	  row = row + 1
	ELSE !not zero
C--------- CALCULATE LOWER ANGLES AND COMPARE WITH LIMITS ----------C
           qznow = rbuf.qz
           qxnow = rbuf.qx
	   Call refl_pos(qznow,qxnow,alphai,alphaf,posit) !Puts positions in array posit
	   DO kj = 1, 4 !slit motors
	     j=slitlist(kj)
	     posit(j) = rbuf.s(kj)
	   ENDDO
	   IF (rbuf.numx .ne. 0)posit(rbuf.numx)=rbuf.sx
	   limit = .FALSE.
	   DO kj = 1, tomove !check all motors to be moved
	      j = motlist(kj)
	      t = posit(j) + z(j)
	      IF (motfl(j) .AND. .NOT. fixed(j)) THEN
		 IF (.NOT. inlimits(j,t,prpr))THEN
	           IF (row .EQ. 4) CALL SMG$ERASE_DISPLAY (prpr, 2, 18)
	           WRITE (UNIT= str2, FMT= '(i2)',iostat=ios) j
	           WRITE (UNIT= str8, FMT= '(f8.3)',iostat=ios) t
	           response(1:) = '*** WARNING ***    Angle '// str2// 
     1		    ' at '//str8//' would be outside oflimits'
	           CALL PUTCHARS (prpr, response, row, 10)
	           row = row + 1
	           limit = .TRUE.
	         ENDIF
	      END IF
	   END DO
	   DO kj = 1, 4 !check slit openings
	      j = slitlist(kj)
	      t = posit(j) + z(j)
	      IF (motfl(j) .AND. .NOT. fixed(j)) THEN
		 IF (.NOT. inlimits(j,t,prpr))THEN
	           IF (row .EQ. 4) CALL SMG$ERASE_DISPLAY (prpr, 2, 18)
	           WRITE (UNIT= str2, FMT= '(i2)',iostat=ios) j
	           WRITE (UNIT= str8, FMT= '(f8.3)',iostat=ios) t
	           response(1:) = '*** WARNING ***    Angle '// str2// 
     1		    ' at '//str8//' would be outside oflimits'
	           CALL PUTCHARS (prpr, response, row, 10)
	           row = row + 1
	           limit = .TRUE.
	         ENDIF
	      END IF
	   END DO
	   IF (rbuf.numx .NE. 0) THEN
	      j = rbuf.numx
	      t = posit(j) + z(j)
	      IF (motfl(j) .AND. .NOT. fixed(j)) THEN
		 IF (.NOT. inlimits(j,t,prpr))THEN
	           IF (row .EQ. 4) CALL SMG$ERASE_DISPLAY (prpr, 2, 18)
	           WRITE (UNIT= str2, FMT= '(i2)',iostat=ios) j
	           WRITE (UNIT= str8, FMT= '(f8.3)',iostat=ios) t
	           response(1:) = '*** WARNING ***    Angle '// str2// 
     1		    ' at '//str8//' would be outside oflimits'
	           CALL PUTCHARS (prpr, response, row, 10)
	           row = row + 1
	           limit = .TRUE.
	         ENDIF
	      END IF
	   ENDIF !numx ne 0

c  Now check upper position of motor against limits
           qznow = rbuf.qz + rbuf.qzi * (nint(rbuf.pts)-1)
           qxnow = rbuf.qx + rbuf.qxi * (nint(rbuf.pts)-1)
	   Call refl_pos(qznow,qxnow,alphai,alphaf,posit) !Puts positions in array posit
	   DO kj = 1, 4 !slit motors
	     j=slitlist(kj)
	     posit(j) = rbuf.s(kj) + rbuf.i(kj) * (nint(rbuf.pts)-1)
	   ENDDO
	   IF (rbuf.numx .ne. 0)posit(rbuf.numx)=rbuf.sx + 
     1		rbuf.ix * (nint(rbuf.pts)-1)
	   DO kj = 1, tomove !check all motors to be moved
	      j = motlist(kj)
	      t = posit(j) + z(j)
	      IF (motfl(j) .AND. .NOT. fixed(j)) THEN
		 IF (.NOT. inlimits(j,t,prpr))THEN
	           IF (row .EQ. 4) CALL SMG$ERASE_DISPLAY (prpr, 2, 18)
	           WRITE (UNIT= str2, FMT= '(i2)',iostat=ios) j
	           WRITE (UNIT= str8, FMT= '(f8.3)',iostat=ios) t
	           response(1:) = '*** WARNING ***    Angle '// str2// 
     1		    ' at '//str8//' would be outside oflimits'
	           CALL PUTCHARS (prpr, response, row, 10)
	           row = row + 1
	           limit = .TRUE.
	         ENDIF
	      END IF
	   END DO
	   DO kj = 1, 4 !check slit openings
	      j = slitlist(kj)
	      t = posit(j) + z(j)
	      IF (motfl(j) .AND. .NOT. fixed(j)) THEN
		 IF (.NOT. inlimits(j,t,prpr))THEN
	           IF (row .EQ. 4) CALL SMG$ERASE_DISPLAY (prpr, 2, 18)
	           WRITE (UNIT= str2, FMT= '(i2)',iostat=ios) j
	           WRITE (UNIT= str8, FMT= '(f8.3)',iostat=ios) t
	           response(1:) = '*** WARNING ***    Angle '// str2// 
     1		    ' at '//str8//' would be outside oflimits'
	           CALL PUTCHARS (prpr, response, row, 10)
	           row = row + 1
	           limit = .TRUE.
	         ENDIF
	      END IF
	   END DO
	   IF (rbuf.numx .NE. 0) THEN !check extra motor
	      j = rbuf.numx
	      t = posit(j) + z(j)
	      IF (motfl(j) .AND. .NOT. fixed(j)) THEN
		 IF (.NOT. inlimits(j,t,prpr))THEN
	           IF (row .EQ. 4) CALL SMG$ERASE_DISPLAY (prpr, 2, 18)
	           WRITE (UNIT= str2, FMT= '(i2)',iostat=ios) j
	           WRITE (UNIT= str8, FMT= '(f8.3)',iostat=ios) t
	           response(1:) = '*** WARNING ***    Angle '// str2// 
     1		    ' at '//str8//' would be outside oflimits'
	           CALL PUTCHARS (prpr, response, row, 10)
	           row = row + 1
	           limit = .TRUE.
	         ENDIF
	      END IF
	   ENDIF !numx ne 0
	ENDIF !wavel or dm = 0
	IF (RBUF.ihf .NE. 0 .or. RBUF.hf .NE. 0)THEN
	  hmax = RBUF.hf + (RBUF.pts-1)*RBUF.ihf
	  IF (magnctr)THEN
	    fmax = fieldmax * hfconv
	    IF (abs(hmax) .GT. fmax .OR. abs(RBUF.hf) .GT. fmax)THEN
	      IF (row .EQ. 4) CALL SMG$ERASE_DISPLAY (prpi, 2, 18)
	      response(1:)='*** WARNING *** Hfield out of range'
	      CALL PUTCHARS (prpi, response,row,10)
	      row = row+1
	    ENDIF
	  ENDIF
	ENDIF
        IF (row .GT. 4) THEN
	   IF (nsta .EQ. 1) THEN
	      row = 7
	   ELSE
	      ROW = 8
	   END IF
	   CALL CURSOR_ABS (prpr, row, 48)
	   CALL CURSOR_OFF (paste)
	   CALL SMG$READ_KEYSTROKE (keyb, wtc, 
	1       '--  hit any key to continue  --',, prpr)
	   CALL CURSOR_ON (paste)
	END IF


	CALL SMG$CREATE_MENU (MENUr, line,,, 3)
	CALL SMG$ERASE_DISPLAY (prpr, 2, 1)
	RETURN 1
	END




	include COMPILE_OPTS
	SUBROUTINE Updte_R_File (rbuf, line, *)
C	***************************************

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C                                                                             C
C     Routines Updates rbuffer.buf With One New Value For All Buffers,        C
C     Creates The New Menu And Returns Control To Statement Label * In The    C
C     Calling Program                                                         C
C     Called from: Parse, PrepR                                               C
C                                                                             C
C     Gayle Greene, February 1993                                             C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C

	IMPLICIT     none
	INCLUDE     INSTR_CMN
	INCLUDE     SMGPREP_CMN      
	INCLUDE     REFL_CMN
	INTEGER*4    k, ios
	REAL         qzf,qxf
	CHARACTER*8  str8
	CHARACTER*80 line(nbuf), help_line(3)

	RECORD /r_buf/ rbuf(nbuf)

	OPEN (UNIT= 1, FILE= 'RBUFFER.BUF', STATUS= 'OLD', ACCESS= 'DIRECT', 
	1     FORM= 'UNFORMATTED', SHARED,RECL=RBUF_RECL)

	DO k = 1, nbuf
	   WRITE (1,REC=k) rbuf(k) 
	   qzf = rbuf(k).qz + (rbuf(k).pts -1)* rbuf(k).qzi
	   qxf = rbuf(k).qx + (rbuf(k).pts -1)* rbuf(k).qxi
	   IF (rbuf(k).mon1 .eq. 0.) THEN
	     write(str8,fmt='(i8)',iostat=ios)NINT(rbuf(k).mon0)
	   ELSE
	     str8='  varies'
	   ENDIF
	   WRITE (line(k),'(i2,1x,a20,f9.6,f10.6,f9.6,f10.6,i3,''*'',a8,i5)',
     1		  iostat=ios)
	2         k, rbuf(k).com(1:20), rbuf(k).qz, qzf, rbuf(k).qx,qxf, 
	3         NINT(rbuf(k).mpf), str8, NINT(rbuf(k).pts) 
	END DO
	CLOSE (1)

	CALL SMG$CREATE_MENU (MENUr, line,,, 3)


C---------------------------- UPDATE HELP SCREEN -----------------------------C

	help_line(1) = ' Type HELP for command list and syntax' 
	help_line(2) = ' '
	help_line(3) = ' '
	CALL Put_Help (help_line)
 
	RETURN 1
	END
