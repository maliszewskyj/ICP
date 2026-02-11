#include "icp.h"
	include COMPILE_OPTS
	SUBROUTINE Updte_I_Buf (ibuf, k, line, *)
C	*****************************************

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C                                                                             C
C     Routine Updates Ibuffer.buf With New Values For One Buffer, Creates The C
C     New Menu And Returns Control To Statement Label * In The Calling        C
C     Program                                                                 C
C     Called from: PrepI                                                      C
C                                                                             C
C     Beat Schmid, June 1989                                                  C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C


	IMPLICIT     none
	INCLUDE     SMGPREP_CMN      
	INCLUDE     INSTR_CMN
	INCLUDE     TC_CMN

	INTEGER*4    k, j, j0, row, wtc, ios
	REAL         a4f, t, posit(12), hmax, fmax, zj, Fz
	LOGICAL      limit(6), inlimits
	CHARACTER*1  str1
	CHARACTER*8  str8
	CHARACTER*80 line(nbuf), response
	CHARACTER*6  str6, str6b

	RECORD /i_buf/ ibuf

	OPEN (UNIT= 1, FILE= 'IBUFFER.BUF', STATUS= 'OLD', ACCESS= 'DIRECT', 
	1     FORM= 'UNFORMATTED', SHARED,RECL=IBUF_RECL)
                     
	WRITE (1,REC=k) ibuf
	CLOSE (1)

	CALL Putmax(ibuf.tmp,str6b,6,5)
	CALL Putmax(ibuf.it,str6,6,5)
	a4f = ibuf.a(4) + (ibuf.pts -1)* ibuf.i(4)
	WRITE (line(k),'(i2,1x,a20,2f8.2,f6.2,i3,''*'',i7,1x,a6,1x,a6,f7.0)',
     1		iostat=ios)
	2     k, ibuf.com (1:20), ibuf.a(4), a4f, ibuf.i(4),
	3     NINT(ibuf.mpf), NINT(ibuf.mon), str6b, str6, ibuf.tw


C--------- CALCULATE LOWER AND UPPER ANGLES AND COMPARE WITH LIMITS ----------C

	DO j = 1, 6 
	   IF (motfl(j)) THEN
	      posit(j)   = ibuf.a(j)
	      posit(j+6) = ibuf.a(j) + (ibuf.pts-1) * ibuf.i(j)
	   END IF
	   limit(j) = .FALSE.
	END DO
	row = 4
	DO j0 = 1, 12
	  IF (nsta .ne. 1 .OR.
     1	  (nsta .eq.1 .AND. (j0.eq.3 .or. j0.eq.4 .or. j0.eq.9 .or. j0.eq.10)))THen
	   IF (j0 .GT. 6) THEN
	      j = j0 - 6
	   ELSE
	      j = j0
	   END IF
	   IF (nsta.eq.4 .AND. z(j).eq.-99.)THEN !special flag
	        zj = Fz(j,posit(j))
	   ELSE 
	        zj = z(j)
	   ENDIF
	   t = posit(j0) + zj
	   IF (.NOT. limit(j)) THEN
	      IF (motfl(j) .AND. .NOT. inlimits(j,t,prpi))THEN
	         IF (row .EQ. 4) CALL SMG$ERASE_DISPLAY (prpi, 2, 18)
	         WRITE (UNIT= str1, FMT= '(i1)',iostat=ios) j
	         WRITE (UNIT= str8, FMT= '(f8.3)',iostat=ios) t
	         response(1:) = '*** WARNING ***    Angle '// str1 //
     1			' at '//str8//' would be outside of limits' 
	         CALL PUTCHARS (prpi, response, row, 10)
	         row = row + 1
		 limit(j) = .TRUE.		 
      	      END IF
	   END IF
	 ENDIF
	END DO
	IF (ibuf.ihf .NE. 0 .or. ibuf.hf .NE. 0)THEN
	  hmax = ibuf.hf + (ibuf.pts-1)*ibuf.ihf
	  IF (magnctr)THEN
	    fmax = fieldmax * hfconv
	    IF (abs(hmax) .GT. fmax .OR. abs(ibuf.hf) .GT. fmax)THEN
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
	   CALL CURSOR_ABS (prpi, row, 48)
	   CALL SMG$SET_CURSOR_MODE (paste, SMG$M_CURSOR_OFF)
	   CALL SMG$READ_KEYSTROKE (keyb, wtc, 
	1       '--  hit any key to continue  --',, prpi)
	   CALL SMG$SET_CURSOR_MODE (paste, SMG$M_CURSOR_ON)
	END IF


	CALL SMG$CREATE_MENU (MENUI, line,,, 3)
	CALL SMG$ERASE_DISPLAY (prpi, 2, 1)
	RETURN 1
	END




	include COMPILE_OPTS
	SUBROUTINE Updte_I_File (ibuf, line, *)
C	***************************************

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C                                                                             C
C     Routines Updates Ibuffer.buf With One New Value For All Buffers,        C
C     Creates The New Menu And Returns Control To Statement Label * In The    C
C     Calling Program                                                         C
C     Called from: Parse, PrepI                                               C
C                                                                             C
C     Beat Schmid, June 1989                                                  C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C

	IMPLICIT     none
	INCLUDE     INSTR_CMN
	INCLUDE     SMGPREP_CMN      
	INTEGER*4    k, ios
	REAL         a4f
	CHARACTER*80 line(nbuf), help_line(3)
	CHARACTER*6  str6, str6b

	RECORD /i_buf/ ibuf(nbuf)

	OPEN (UNIT= 1, FILE= 'IBUFFER.BUF', STATUS= 'OLD', ACCESS= 'DIRECT', 
	1     FORM= 'UNFORMATTED', SHARED,RECL=IBUF_RECL)

	DO k = 1, nbuf
	   WRITE (1,REC=k) ibuf(k) 
	   CALL Putmax(ibuf(k).tmp,str6b,6,5)
	   CALL Putmax(ibuf(k).it,str6,6,5)
	   a4f = ibuf(k).a(4) + (ibuf(k).pts -1)* ibuf(k).i(4)
	   WRITE (line(k),'(i2,1x,a20,2f8.2,f6.2,i3,''*'',i7,1x,a6,1x,a6,f7.0)',
     1		iostat=ios)
	2         k, ibuf(k).com (1:20), ibuf(k).a(4), a4f, ibuf(k).i(4),
	3         NINT(ibuf(k).mpf), NINT(ibuf(k).mon), str6b, 
	4	  str6, ibuf(k).tw
	END DO
	CLOSE (1)

	CALL SMG$CREATE_MENU (MENUI, line,,, 3)


C---------------------------- UPDATE HELP SCREEN -----------------------------C

	help_line(1) = ' Type HELP for command list and syntax' 
	help_line(2) = ' '
	help_line(3) = ' '
	CALL Put_Help (help_line)
 
	RETURN 1
	END
