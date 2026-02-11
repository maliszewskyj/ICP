#include "icp.h"
	SUBROUTINE DEFA (efa,enfa_range)
C	***********************

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C                                                                             C
C     Drives motors 5 and 6 as well as all focus motors so that energy is  C
C      focused
C     Called from: Parse_Contr                                                C
C                                                                             C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
	use instr
	use smg_prep
	use focus
	IMPLICIT      none
	INTEGER*4     i, j, state
	REAL*4        RdAngl, complanar, Dot_Pro, tmat(3,3)
	REAL*4	      q(3), v1(3), v2(3), v3(3), etrans
	REAL*4	      t, Energy_Angle, efa, enfa_range
	LOGICAL       limit,f_limit, inlimits, Readef
	CHARACTER*3   str3(2)
	CHARACTER*7   st7(5), PutStr
	CHARACTER*80  response, error

	REAL*4	      posit(MAXMOTS)

C--------Calculate positions of motors 5,6 and 11 focus motors----------------C
	CALL FocA_Energy(efa,enfa_range,posit)

C------------------- CHECK THAT ANGLES ARE WITHIN LIMITS ---------------------C
C--------------------------- IF NOT GIVE WARNING -----------------------------C
                                     
	limit = .FALSE.
	DO j = 5,6
	   t = posit(j) + z(j)
	   IF (motfl(j)) THEN
	      IF (.NOT. inlimits(j,t,contr)) limit = .TRUE.
	   END IF
	END DO
	IF (limit) THEN
	   WRITE (UNIT= error, FMT= '(2f8.3)') (posit(j), j= 3, 4)
	   CALL PUTCONTR (' Configuration outside limits: '// error)
	   RETURN
  	END IF
	limit=.false.
	DO j = first_focus,first_focus+num_focus-1
	   t = posit(j) + z(j)
	   IF (motfl(j)) THEN
	      IF (.NOT. inlimits(j,t,contr)) limit = .TRUE.
	   END IF
	END DO
	IF (limit) THEN
	   WRITE (UNIT= error, FMT= '(6f8.3)') (posit(j), 
	1	j= first_focus, first_focus+5)
	   CALL PUTCONTR (' Focus Config outside limits: '//
	1	error)	
	   WRITE (UNIT= error, FMT= '(5f8.3)') (posit(j), 
	1	j= first_focus+6, first_focus+num_focus-1)
	   CALL PUTCONTR (error)
	   RETURN
	END IF


	
C-------------DRIVE MOTORS TO NEW POSITIONS AND PRINT POSITION --------------C

	DO j = 5, 6
	   IF (READEF(contr_A))THEN  
	      CALL PUTCONTR (' **** ABORT during drive ****')
	      GOTO 50
	   END IF
	   IF (motfl(j)) CALL Drive (j, posit(j) + z(j))
	END DO

	DO j = first_focus,first_focus+num_focus-1
	   IF (READEF(contr_A))THEN  
	      CALL PUTCONTR (' **** ABORT during drive ****')
	      GOTO 50
	   END IF
	   IF (motfl(j)) 
	1	CALL Drive (j, posit(j) + z(j))
	END DO

 50 	CALL Yield               

	END
	
