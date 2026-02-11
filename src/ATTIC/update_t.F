#include "icp.h"
	include COMPILE_OPTS
	SUBROUTINE Updte_T_Buf (tbuf, k, line, *)
C	*****************************************

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C                                                                             C
C     Routine Updates Tbuffer.buf With New Values For One Buffer, Creates The C
C     New Menu And Returns Control To Statement Label * In The Calling        C
C     Program                                                                 C
C     Called from: PrepT                                                      C
C                                                                             C
C     Beat Schmid, December 1990                                              C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C


	IMPLICIT     none
	INCLUDE     SMGPREP_CMN      
	INCLUDE     INSTR_CMN
	INCLUDE     TC_CMN

	INTEGER*2    wtc
	INTEGER*4    k, j, j0, row, ios
	REAL*4	     e0, ef, Energy_Angle, posit(5)
	REAL*4	     t, halfway, hmax, fmax, zj, Fz
	LOGICAL      limit(6), inlimits
	CHARACTER*1  str1
	CHARACTER*8  str8
	CHARACTER*7  str7
	CHARACTER*80 line(nbuf), response

	RECORD /t_buf/ tbuf
	RECORD /lattice/ cr


C----------------- CALCULATE first & last VALUES FOR ENERGY ------------------C

	halfway = (tbuf.pts - 1.) / 2.
	e0    = tbuf.ec - halfway * tbuf.es
	ef    = tbuf.ec + halfway * tbuf.es

	posit(1)  = Energy_Angle (tbuf.ef+e0, dm)              ! eft always ANA
	posit(3)  = Energy_Angle (tbuf.ef+ef, dm)
	posit(2)  = 2. * posit(1)
	posit(4)  = 2. * posit(3)
	posit(5)  = Energy_Angle (tbuf.ef, da)


C------------------- CHECK THAT ANGLES ARE WITHIN LIMITS ---------------------C

	DO j = 1, 4
	   limit(j) = .FALSE.
	END DO
	row = 4
	DO j = 1, 4                                        ! check angles 1 & 2
	   j0 = j
	   IF (j .GT. 2) j0 = j - 2
	   IF (nsta.eq.4 .AND. z(j0).eq.-99.)THEN !special flag
	        zj = Fz(j0,posit(j0))
	   ELSE 
	        zj = z(j0)
	   ENDIF
	   t = posit(j) + zj
	   IF (motfl(j0) .AND. .NOT. limit(j0) .AND.
     1	      (.NOT. inlimits(j0,t,prpt)))THEN
	      IF (row .EQ. 4) CALL SMG$ERASE_DISPLAY (prpt, 2, 18)
	      WRITE (UNIT= str1, FMT= '(i1)',iostat=ios) j0
	      WRITE (UNIT= str8, FMT= '(f8.3)',iostat=ios) t
	      response(1:) = '*** WARNING ***    Angle '// str1//
     1		' at '//str8//' would be outside of limits' 
	      CALL PUTCHARS (prpt, response, row, 10)
	      row = row + 1
	      limit(j0) = .TRUE.		 
      	   END IF
	END DO
	IF (tbuf.ihf .NE. 0 .or. tbuf.hf .NE. 0)THEN
	  hmax = tbuf.hf + (tbuf.pts-1)*tbuf.ihf
	  IF (magnctr)THEN
	    fmax = fieldmax * hfconv
	    IF (abs(hmax) .GT. fmax .OR. abs(tbuf.hf) .GT. fmax)THEN
	      IF (row .EQ. 4) CALL SMG$ERASE_DISPLAY (prpi, 2, 18)
	      response(1:)='*** WARNING *** Hfield out of range'
	      CALL PUTCHARS (prpt, response,row,10)
	      row = row+1
	    ENDIF
	  ENDIF
	ENDIF
        IF (row .GT. 4) THEN
	   CALL CURSOR_ABS (prpt, 7, 48)
	   CALL SMG$SET_CURSOR_MODE (paste, SMG$M_CURSOR_OFF)
	   CALL SMG$READ_KEYSTROKE (keyb, wtc, 
	1       '--  hit any key to continue  --',, prpt)
	   CALL SMG$SET_CURSOR_MODE (paste, SMG$M_CURSOR_ON)
	END IF

C------------------------------ UPDATE BUFFER --------------------------------C

	OPEN (UNIT= 1, FILE= 'TBUFFER.BUF', STATUS= 'OLD', ACCESS= 'DIRECT', 
	1     FORM= 'UNFORMATTED', SHARED,RECL=TBUF_RECL)
	WRITE (1,REC=k) tbuf
	CLOSE (1)

	CALL Putmax(tbuf.tmp,str7,7,5)
	WRITE (line(k),'(i2,1x,a20,1x,f8.2,2f7.2,i7,1x,a7,i7)',iostat=ios)
	2       k, tbuf.com (1:20), tbuf.ec, tbuf.es, tbuf.ef, NINT(tbuf.mon),
	3       str7, NINT(tbuf.pts)

	CALL SMG$CREATE_MENU (menut, line,,, 3)
	CALL SMG$ERASE_DISPLAY (prpt, 2, 1)
	RETURN 1
	END


	include COMPILE_OPTS
	SUBROUTINE Updte_T_File (tbuf, line, *)
C	***************************************

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C                                                                             C
C     Routines Updates tbuffer.buf With One New Value For All Buffers,        C
C     Creates The New Menu And Returns Control To Statement Label * In The    C
C     Calling Program                                                         C
C     Called from: PrepT                                                      C
C                                                                             C
C     Beat Schmid, October 1989                                               C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C

	IMPLICIT     none
	INCLUDE      INSTR_CMN
	INCLUDE      SMGPREP_CMN      

	INTEGER*4    i, ios
	CHARACTER*80 line(nbuf), help_line(3)
	CHARACTER*7  str7

	RECORD /t_buf/ tbuf(nbuf)

	OPEN (UNIT= 1, FILE= 'TBUFFER.BUF', STATUS= 'OLD', ACCESS= 'DIRECT', 
	1     FORM= 'UNFORMATTED', SHARED,RECL=TBUF_RECL)

	DO i = 1, nbuf
	   WRITE (1,REC=i) tbuf(i) 

	   CALL Putmax(tbuf(i).tmp,str7,7,5)
	   WRITE (line(i),'(i2,1x,a20,1x,f8.2,2f7.2,i7,1x,a7,i7)',iostat=ios)
	2       i, tbuf(i).com (1:20), tbuf(i).ec, tbuf(i).es, tbuf(i).ef, 
	3	NINT(tbuf(i).mon), str7, NINT(tbuf(i).pts)
	END DO
	CLOSE (1)

	CALL SMG$CREATE_MENU (menut, line,,, 3)



C---------------------------- UPDATE HELP SCREEN -----------------------------C

      help_line(1) = ' Type HELP for command list and syntax' 
      help_line(2) = ' '
      help_line(3) = ' '
      CALL Put_Help (help_line)
 
      RETURN 1
      END
