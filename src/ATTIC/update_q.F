#include "icp.h"
	include COMPILE_OPTS
	SUBROUTINE Updte_Q_Buf (qbuf, k, line, *)
C	*****************************************

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C                                                                             C
C     Routine Updates Qbuffer.buf With New Values For One Buffer, Creates The C
C     New Menu And Returns Control To Statement Label * In The Calling        C
C     Program                                                                 C
C     Called from: PrepQ                                                      C
C                                                                             C
C     Beat Schmid, October 1989                                               C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C


	IMPLICIT     none
	INCLUDE     SMGPREP_CMN      
	INCLUDE     INSTR_CMN
	INCLUDE     TC_CMN
	INCLUDE     FOCUS_CMN

	INTEGER*2    wtc
	INTEGER*4    k, j, j0, row, ios
	REAL*4       q0(3), qq0(3), qf(3), qqf(3), v1(3), v2(3), v3(3), zj,Fz
	REAL*4	     e0, ef, Energy_Angle, Phi_Angle, Psi_Angle, posit(12)
	REAL*4	     Dot_Pro, tmat(3,3), t, complanar, halfway, hmax, fmax
	LOGICAL      limit(6), inlimits
	CHARACTER*1  str1
	CHARACTER*8  str8
	CHARACTER*80 line(nbuf), response

	RECORD /q_buf/ qbuf
	RECORD /lattice/ cr


C----------- CALCULATE first & last VALUES FOR ENERGY AND Q-SPACE ------------C

	CALL Rec_Lattice (qbuf, cr, tmat)
	halfway = (qbuf.pts - 1.) / 2.
	e0    = qbuf.ec - halfway * qbuf.es
	q0(1) = qbuf.hc - halfway * qbuf.hs
	q0(2) = qbuf.kc - halfway * qbuf.ks
	q0(3) = qbuf.lc - halfway * qbuf.ls
	ef    = qbuf.ec + halfway * qbuf.es
	qf(1) = qbuf.hc + halfway * qbuf.hs
	qf(2) = qbuf.kc + halfway * qbuf.ks
	qf(3) = qbuf.lc + halfway * qbuf.ls

	CALL Mat_Vec (tmat, q0, qq0)
	CALL Mat_Vec (tmat, qf, qqf)
	CALL Mat_Vec (tmat, qbuf.hkl1, v1)
	CALL Mat_Vec (tmat, qbuf.hkl2, v2)
	CALL Vec_Pro (v1, v2, v3)
	complanar = Dot_Pro (v3, qq0)

	IF (qbuf.eft .EQ. 'ANA') THEN
	   posit(1)  = Energy_Angle (qbuf.ef+e0, dm)
	   posit(7)  = Energy_Angle (qbuf.ef+ef, dm)
	   posit(2)  = 2. * posit(1)
	   posit(8)  = 2. * posit(7)
	   posit(5)  = Energy_Angle (qbuf.ef, da)
	   posit(11) = posit(5)
	   posit(6)  = 2. * posit(5)
	   posit(12) = 2. * posit(5)
	ELSE
	   posit(1)  = Energy_Angle (qbuf.ef, dm)
	   posit(7)  = posit(1)
	   posit(2)  = 2. * posit(1)
	   posit(8)  = 2. * posit(1)
	   posit(5)  = Energy_Angle (qbuf.ef-e0, da)
	   posit(11) = Energy_Angle (qbuf.ef-ef, da)
	   posit(6)  = 2. * posit(5)
	   posit(12) = 2. * posit(11)
	END IF

	IF (SIND(posit(1)) .EQ. 0. .OR. SIND(posit(5)) .EQ. 0. .OR. 
     1	    SIND(posit(7)) .EQ. 0 ..OR. SIND(posit(11)) .EQ. 0)THEN
	   CALL SMG$ERASE_DISPLAY (prpq, 2, 18)
	   CALL PUTCHARS (prpq, 'A division by zero would result '//
	1                      'from these buffer values!!!', 4, 10)
	   CALL CURSOR_ABS (prpq, 8, 48)
	   CALL SMG$SET_CURSOR_MODE (paste, SMG$M_CURSOR_OFF)
	   CALL SMG$READ_KEYSTROKE (keyb, wtc, 
	1       '--  hit any key to continue  --',, prpq)
	   CALL SMG$SET_CURSOR_MODE (paste, SMG$M_CURSOR_ON)
	   OPEN (UNIT= 1, FILE= 'QBUFFER.BUF', STATUS= 'OLD', ACCESS= 'DIRECT', 
	1     FORM= 'UNFORMATTED', SHARED,RECL=QBUF_RECL)
	   READ (1,REC=k) qbuf
	   CLOSE (1)

	   WRITE (line(k),'(i2,1x,a20,1x,3f8.4,f8.2,2f7.2,a1,i7)',iostat=ios)
	2      k, qbuf.com(1:20), qbuf.hc, qbuf.kc, qbuf.lc, qbuf.ec, 
	3      qbuf.es, qbuf.ef, qbuf.eft, NINT(qbuf.mon)

	   CALL SMG$CREATE_MENU (menuq, line,,, 3)
	   CALL SMG$ERASE_DISPLAY (prpq, 2, 1)
	   RETURN 1
	ENDIF

	posit(3)  = Psi_Angle (qq0, posit(1), posit(5), v1, qbuf.a1, v2)
	posit(9)  = Psi_Angle (qqf, posit(1), posit(5), v1, qbuf.a1, v2)
	posit(4)  = Phi_Angle (qq0, posit(1), dm, posit(5), da)
	posit(10) = Phi_Angle (qqf, posit(7), dm, posit(11), da)
	   IF (nsta .eq. -5 .and. negqpos)THEN
	     posit(3) = -posit(3)
	     posit(4) = -posit(4)
	     posit(9) = -posit(9)
	     posit(10) = -posit(10)
	   ENDIF


C------------------- CHECK THAT ANGLES ARE WITHIN LIMITS ---------------------C

	DO j = 1, 6
	   limit(j) = .FALSE.
	END DO
	row = 4
	IF (ABS(complanar) .GT. 1E-2) THEN
	   CALL SMG$ERASE_DISPLAY (prpq, 2, 18)
	   CALL PUTCHARS (prpq, 'Your Q-Vector does not lie in the '//
	1                      'scattering plane!!!', 4, 10)
	   row = 5
	END IF
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
	      IF (motfl(j) .AND. .NOT. inlimits(j,t,prpq))THEN
	         IF (row .EQ. 4) CALL SMG$ERASE_DISPLAY (prpq, 2, 18)
	         WRITE (UNIT= str1, FMT= '(i1)',iostat=ios) j
	         WRITE (UNIT= str8, FMT= '(f8.3)',iostat=ios) t
	         response(1:) = '*** WARNING ***    Angle '// str1//
     1		   ' at '//str8//' would be outside limits'
	         IF (t-z(j) .EQ. 999. ) THEN
		    response(27:) = '  momentum triangle cannot be closed!!'
	         END IF
	         CALL PUTCHARS (prpq, response, row, 10)
	         row = row + 1
		 limit(j) = .TRUE.		 
      	      END IF
	   END IF
	END DO
	IF (qbuf.ihf .NE. 0 .or. qbuf.hf .NE. 0)THEN
	  hmax = qbuf.hf + (qbuf.pts-1)*qbuf.ihf
	  IF (magnctr)THEN
	    fmax = fieldmax * hfconv
	    IF (abs(hmax) .GT. fmax .OR. abs(qbuf.hf) .GT. fmax)THEN
	      IF (row .EQ. 4) CALL SMG$ERASE_DISPLAY (prpi, 2, 18)
	      response(1:)='*** WARNING *** Hfield out of range'
	      CALL PUTCHARS (prpq, response,row,10)
	      row = row+1
	    ENDIF
	  ENDIF
	ENDIF
        IF (row .GT. 4) THEN
	   CALL CURSOR_ABS (prpq, 8, 48)
	   CALL SMG$SET_CURSOR_MODE (paste, SMG$M_CURSOR_OFF)
	   CALL SMG$READ_KEYSTROKE (keyb, wtc, 
	1       '--  hit any key to continue  --',, prpq)
	   CALL SMG$SET_CURSOR_MODE (paste, SMG$M_CURSOR_ON)
	END IF

C------------------------------ UPDATE BUFFER --------------------------------C

	OPEN (UNIT= 1, FILE= 'QBUFFER.BUF', STATUS= 'OLD', ACCESS= 'DIRECT', 
	1     FORM= 'UNFORMATTED', SHARED,RECL=QBUF_RECL)
	WRITE (1,REC=k) qbuf
	CLOSE (1)

	WRITE (line(k),'(i2,1x,a20,1x,3f8.4,f8.2,2f7.2,a1,i7)',iostat=ios)
	2      k, qbuf.com(1:20), qbuf.hc, qbuf.kc, qbuf.lc, qbuf.ec, 
	3      qbuf.es, qbuf.ef, qbuf.eft, NINT(qbuf.mon)

	CALL SMG$CREATE_MENU (menuq, line,,, 3)
	CALL SMG$ERASE_DISPLAY (prpq, 2, 1)
	RETURN 1
	END



	include COMPILE_OPTS
	SUBROUTINE Updte_Q_File (qbuf, line, *)
C	***************************************

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C                                                                             C
C     Routines Updates QBuffer.buf With One New Value For All Buffers,        C
C     Creates The New Menu And Returns Control To Statement Label * In The    C
C     Calling Program                                                         C
C     Called from: Parse, PrepQ                                               C
C                                                                             C
C     Beat Schmid, October 1989                                               C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C

	IMPLICIT     none
	INCLUDE      INSTR_CMN
	INCLUDE      SMGPREP_CMN      

	INTEGER*4    i, ios
	CHARACTER*80 line(nbuf), help_line(3)

	RECORD /q_buf/ qbuf(nbuf)

	OPEN (UNIT= 1, FILE= 'QBUFFER.BUF', STATUS= 'OLD', ACCESS= 'DIRECT', 
	1     FORM= 'UNFORMATTED', SHARED,RECL=QBUF_RECL)

	DO i = 1, nbuf
	   WRITE (1,REC=i) qbuf(i) 
	   WRITE (line(i),'(i2,1x,a20,1x,3f8.4,f8.2,2f7.2,a1,i7)',iostat=ios)
	2         i, qbuf(i).com (1:20), qbuf(i).hc, qbuf(i).kc, qbuf(i).lc,
	3         qbuf(i).ec, qbuf(i).es, qbuf(i).ef, qbuf(i).eft, 
	4	  NINT(qbuf(i).mon)
	END DO
	CLOSE (1)

	CALL SMG$CREATE_MENU (menuq, line,,, 3)



C---------------------------- UPDATE HELP SCREEN -----------------------------C

      help_line(1) = ' Type HELP for command list and syntax' 
      help_line(2) = ' '
      help_line(3) = ' '
      CALL Put_Help (help_line)
 
      RETURN 1
      END
