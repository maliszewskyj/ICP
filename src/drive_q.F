#include "icp.h"
	SUBROUTINE Drive_Q (qm,etrans)
C	***********************

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C                                                                             C
C     Drives to a given q-vector!                                             C
C     The routine uses the man_q buffer for as default for the lattice        C
C     parameters and the orientation.                                         C
C     Called from: Parse_Contr                                                C
C                                                                             C
C     Beat Schmid, August 1989                                                C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
	use focus
	use globals
	use instr
	use smg_prep
	IMPLICIT      none
	INTEGER*4     i, j, state
	REAL*4        posit(6), RdAngl, complanar, Dot_Pro, tmat(3,3)
	REAL*4	      qm(3), q(3), v1(3), v2(3), v3(3), etrans
	REAL*4	      Phi_Angle, Psi_Angle, t, Energy_Angle
	LOGICAL       limit, inlimits, Readef
	CHARACTER*3   str3(2)
	CHARACTER*7   st7(5), PutStr
	CHARACTER*80  response, error

	TYPE(lattice) cr
C	TYPE(q_buf) man_q ! in smg_perp

C	COMMON /drive_to_q/ man_q          ! Common with run_q_buf for defaults

C-------- CALCULATE STARTING VALUES FOR ENERGY AND RECIPROCAL SPACE ----------C

        CALL Rec_Lattice (man_q, cr, tmat)
	CALL Mat_Vec (tmat, man_q%hkl1, v1)
	CALL Mat_Vec (tmat, man_q%hkl2, v2)
     	CALL Mat_Vec (tmat, qm, q)
	CALL Vec_Pro (v1, v2, v3)
	complanar = Dot_Pro (v3, q)
	IF (ABS(complanar) .GT. 1E-2) THEN
	   CALL PUTLINE (contr, 
	1       'Your Q-Vector does not lie in the scattering plane!!!')
	   RETURN
	END IF	                                                   


C------------------- READ THE CURRENT MONO AND ANA ANGLES ---------------------C

	IF (nsta .NE. 7 .AND. nsta .NE. -1) THEN
	   IF (man_q%eft .EQ. 'ANA') THEN
	      posit(1) = Energy_Angle (man_q%ef+etrans, dm)
	      posit(2) = 2. * posit(1)
	      posit(5) = Energy_Angle (man_q%ef, da)
	      posit(6) = 2. * posit(5)
	   ELSE
	      posit(1) = Energy_Angle (man_q%ef, dm)
	      posit(2) = 2. * posit(1)
	      posit(5) = Energy_Angle (man_q%ef-etrans, da)
	      posit(6) = 2. * posit(5)
	   END IF
c	   posit(2) = RdAngl(2) - z(2)
c	   posit(1) = 0.5 * posit(2)
c	   posit(6) = RdAngl(6) - z(6)
c	   posit(5) = 0.5 * posit(6)
	ELSE
	   posit(1) = 20.51
	   posit(2) = 2. * posit(1)
	   posit(5) = posit(1)
	   posit(6) = posit(2)
	END IF


C------------------ CALCULATE THE MOMENTUM-TRANSFER AND THE -------------------C
C--------------------- CORRESPONDING PHI AND PSI ANGLES -----------------------C

     	posit(3) = Psi_Angle (q, posit(1), posit(5), v1, man_q%a1, v2)
 	IF (q(1) .EQ. 0. .AND. q(2) .EQ. 0. .AND. q(3) .EQ. 0.) THEN
	     IF (nsta .eq. -5 .and. negqpos)THEN
	        posit (4) = ul(4)
	     ELSE
	        posit (4) = ll(4)
	     ENDIF
	ELSE
	   posit(4) = Phi_angle (q, posit(1), dm, posit(5), da)
	END IF
	IF (nsta .eq. -5 .and. negqpos)THEN
	     posit(3) = -posit(3)
	     posit(4) = -posit(4)
	ENDIF


C------------------- CHECK THAT ANGLES ARE WITHIN LIMITS ---------------------C
C--------------------------- IF NOT GIVE WARNING -----------------------------C
                                     
	limit = .FALSE.
	DO j = 3, 4
	   t = posit(j) + z(j)
	   IF (motfl(j) .AND. .NOT. fixed(j)) THEN
	      IF (.NOT. inlimits(j,t,contr)) limit = .TRUE.
	   END IF
	END DO
	IF (limit) THEN
	   WRITE (UNIT= error, FMT= '(2f8.3)') (posit(j), j= 3, 4)
	   CALL PUTLINE (contr, ' Configuration outside limits: '// error)
	   RETURN
	END IF

          
C------------- DRIVE MOTORS TO NEW POSITIONS AND PRINT POSITION --------------C

	lasta = man_q%a
	lastb = man_q%b
	lastc = man_q%c
	lastef = man_q%ef
	st7(1) = PutStr(man_q%a)
	st7(2) = PutStr(man_q%b)
	st7(3) = PutStr(man_q%c)
	WRITE (UNIT=str3(1), FMT = '(3i1)') (NINT(man_q%hkl1(j)), j= 1, 3)
	WRITE (UNIT=str3(2), FMT = '(3i1)') (NINT(man_q%hkl2(j)), j= 1, 3)
	response(1:) = 'Parameters used:  '// st7(1) // st7(2) // st7(3) //
	1              '     '// str3(1) //'    '// str3(2)
	CALL PUTLINE (contr, response)
	st7(1) = PutStr(man_q%aa)
	st7(2) = PutStr(man_q%bb)
	st7(3) = PutStr(man_q%cc)
	response(1:) = '                  '// st7(1) // st7(2) // st7(3)
	CALL PUTLINE (contr, response)
	st7(1) = PutStr(qm(1))
	st7(2) = PutStr(qm(2))
	st7(3) = PutStr(qm(3))
	st7(4) = PutStr(posit(3))
	st7(5) = PutStr(posit(4))
	response(1:) = 'Driving to Q = ('// st7(1) //','// st7(2) //','//
	1	        st7(3) //')    A3= '// st7(4) //'  A4= '// st7(5)
	CALL PUTLINE (contr, response)
	
	DO j = 4, 3, -1                 ! (* drive motors in reverse order *)
  	   IF (motfl(j) .AND. .NOT. fixed(j)) CALL Drive (j, posit(j) + z(j))
	END DO

 	CALL Yield               
	IF (READEF(contr_A))THEN         
	   CALL PUTLINE (contr, ' **** ABORT during drive ****')
	END IF

	END
	
