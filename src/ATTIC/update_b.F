#include "icp.h"
	include COMPILE_OPTS
	SUBROUTINE Updte_B_Buf (bbuf, k, line, bpos, *)
C	*****************************************

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C                                                                             C
C     Routine Updates bbuffer.buf With New Values For One Buffer, Creates The C
C     New Menu And Returns Control To Statement Label * In The Calling        C
C     Program                                                                 C
C     Called from: Prepb                                                      C
C                                                                             C
C     Gayle Greene, August 1992                                               C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C


	IMPLICIT     none
	INCLUDE     SMGPREP_CMN      
	INCLUDE     INSTR_CMN
	INCLUDE     TC_CMN

	INTEGER*4    k, j, j0, row, i, wtc, ios
	LOGICAL      limit(6), inlimits
	CHARACTER*1  str1
	CHARACTER*10  str10
	CHARACTER*5  str5
	CHARACTER*80 line(nbuf), response
	REAL	     posit(12),t, zj, Fz
	REAL*4	     bpos(10), hmax, fmax
	RECORD /b_buf/ bbuf


C------------------------------ UPDATE BUFFER --------------------------------C
c	calculate starting positions for motors
	  DO i=1,2
	    bbuf.a(i) = bpos(i)
	  ENDDO
	  DO i=5,6
	    bbuf.a(i) = bpos(i)
	  ENDDO
	  bbuf.a(3) = bpos(3) - int(bbuf.pts/2.)*bbuf.i(3)
	  bbuf.a(4) = bpos(4) - int(bbuf.pts/2.)*bbuf.i(4)

	OPEN (UNIT= 1, FILE= 'BBUFFER.BUF', STATUS= 'OLD', ACCESS= 'DIRECT', 
	1     FORM= 'UNFORMATTED', SHARED,RECL=BBUF_RECL)
	WRITE (1,REC=k) bbuf
	CLOSE (1)

	CALL Putmax(bbuf.tmp,str5,5,4)   
	WRITE (line(k),'(i2,1x,a20,1x,3f6.2,f6.2,a1,i7,2f6.2,1x,i3,1x,a5)',
     1		iostat=ios)
	2       k, bbuf.com (1:20), bbuf.hc, bbuf.kc, bbuf.lc,
	3 	bbuf.ef, bbuf.eft,
	4 	NINT(bbuf.mon),bbuf.i(3),bbuf.i(4),NINT(bbuf.pts),str5


C--------- CALCULATE LOWER AND UPPER ANGLES AND COMPARE WITH LIMITS ----------C

	DO j = 1, 6 
	   IF (motfl(j)) THEN
	      posit(j)   = bbuf.a(j)
	      posit(j+6) = bbuf.a(j) + (bbuf.pts-1) * bbuf.i(j)
	   END IF
	   limit(j) = .FALSE.
	END DO
	row = 4 
	DO j0 = 1, 12
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
	      IF (motfl(j) .AND. .NOT. inlimits(j,t,prpb))THEN
	         IF (row .EQ. 4) CALL SMG$ERASE_DISPLAY (prpb, 2, 18)
	         WRITE (UNIT= str1, FMT= '(i1)',iostat=ios) j
	         WRITE (UNIT= str10, FMT= '(f10.3)',iostat=ios) t
	         response(1:) = '*** WARNING ***    Angle '// str1 //
     1		   ' at '//str10//' would be outside of limits!' 
	         CALL PUTCHARS (prpb, response, row, 10)
	         row = row + 1
		 limit(j) = .TRUE.		 
      	      END IF
	   END IF
	END DO
	IF (bbuf.ihf .NE. 0 .or. bbuf.hf .NE. 0)THEN
	  hmax = bbuf.hf + (bbuf.pts-1)*bbuf.ihf
	  IF (magnctr)THEN
	    fmax = fieldmax * hfconv
	    IF (abs(hmax) .GT. fmax .OR. abs(bbuf.hf) .GT. fmax)THEN
	      IF (row .EQ. 4) CALL SMG$ERASE_DISPLAY (prpi, 2, 18)
	      response(1:)='*** WARNING *** Hfield out of range'
	      CALL PUTCHARS (prpb, response,row,10)
	      row = row+1
	    ENDIF
	  ENDIF
	ENDIF
        IF (row .GT. 4) THEN
	   ROW = 8 
	   CALL CURSOR_ABS (prpb, row, 48)
	   CALL SMG$SET_CURSOR_MODE (paste, SMG$M_CURSOR_OFF)
	   CALL SMG$READ_KEYSTROKE (keyb, wtc, 
	1       '--  hit any key to continue  --',, prpb)
	   CALL SMG$SET_CURSOR_MODE (paste, SMG$M_CURSOR_ON)
	END IF


	CALL SMG$CREATE_MENU (menub, line,,, 3)
	CALL SMG$ERASE_DISPLAY (prpb, 2, 1)
	RETURN 1
	END



	include COMPILE_OPTS
	SUBROUTINE Updte_B_File (bbuf, line, *)
C	***************************************

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C                                                                             C
C     Routines Updates bbuffer.buf With One New Value For All Buffers,        C
C     Creates The New Menu And Returns Control To Statement Label * In The    C
C     Calling Program                                                         C
C     Called from: Parse,Prepb                                                C
C                                                                             C
C     Gayle Greene, August 1992                                               C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C

	IMPLICIT     none
	INCLUDE      INSTR_CMN
	INCLUDE      SMGPREP_CMN      

	INTEGER*4    i, ios
	CHARACTER*80 line(nbuf), help_line(3)
	CHARACTER*5  str5

	RECORD /b_buf/ bbuf(nbuf)

	OPEN (UNIT= 1, FILE= 'BBUFFER.BUF', STATUS= 'OLD', ACCESS= 'DIRECT', 
	1     FORM= 'UNFORMATTED', SHARED,RECL=BBUF_RECL)

	DO i = 1, nbuf
	   WRITE (1,REC=i) bbuf(i) 

	   CALL Putmax(bbuf(i).tmp,str5,5,4)   
	   WRITE (line(i),'(i2,1x,a20,1x,3f6.2,f6.2,a1,i7,2f6.2,1x,i3,1x,a5)',
     1		iostat=ios)
	2       i, bbuf(i).com (1:20), bbuf(i).hc, bbuf(i).kc, bbuf(i).lc,
	3 	bbuf(i).ef, bbuf(i).eft,
	4 	NINT(bbuf(i).mon),bbuf(i).i(3),bbuf(i).i(4),NINT(bbuf(i).pts),
     1		str5
	END DO
	CLOSE (1)

	CALL SMG$CREATE_MENU (menub, line,,, 3)



C---------------------------- UPDATE HELP SCREEN -----------------------------C

      help_line(1) = ' Type HELP for command list and syntax' 
      help_line(2) = ' '
      help_line(3) = ' '
      CALL Put_Help (help_line)
 
      RETURN 1
      END
