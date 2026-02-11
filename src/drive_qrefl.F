#include "icp.h"
	SUBROUTINE Drive_QREFL (qznow,qxnow)
C	***********************

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C                                                                             C
C     Drives to a given q-vector on reflectometer!                            C
C     Called from: Parse_Contr                                                C
C                                                                             C
C     Gayle Greene, March 12, 1993                                            C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
	use smg_prep
	use instr
	use refl
	IMPLICIT      none
	REAL*4 	      qznow,qxnow,t
	LOGICAL	       limit, inlimits
	INTEGER*4	kj,j,state,i
	CHARACTER*80  response
	INTEGER*4	tomove,motlist(10)
	LOGICAL		readef

	REAL*4  ASIND
	REAL*4	posit(MAXMOTS),alphai,alphaf
	REAL *4 qzero(4)/.1,.1,.1,.8/ !pos for slits if qz and qx are both zero

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
	


C-------- CALCULATE STARTING VALUES FOR ENERGY AND RECIPROCAL SPACE ----------C

C          C--CALCULATE ANGLES -----------------------------------------------C

	   IF (qznow .eq. 0. .and. qxnow .eq. 0.)THEN !Drive all to 0 as per Sushil
	     DO i= 2,18
	       posit(i) = 0.
	     ENDDO
	     posit(1) = asind(wavel/(2*3.354))
	   ELSE	
	     Call refl_pos(qznow,qxnow,alphai,alphaf,posit) !Puts positions in array posit
	   ENDIF


!          C-------- CHECK THAT ANGLES ARE WITHIN LIMITS ---------------------C
!          C---------------- IF NOT GIVE WARNING -----------------------------C

	   limit = .FALSE.
	   DO kj = 1, tomove !check all motors to be moved
	      j = motlist(kj)
	      t = posit(j) + z(j)
	      IF (motfl(j) .AND. .NOT. fixed(j)) THEN
		 IF (.NOT. inlimits(j,t,contr)) limit = .TRUE.
	      END IF
	   END DO
	   IF (qznow .eq. 0. .and. qxnow .eq. 0.)THEN !Drive slits as per Sushil
	     DO kj = 1, 4 !check slit motorss
	        j = slitlist(kj)
	        t = qzero(kj) + z(j)
	        IF (motfl(j) .AND. .NOT. fixed(j)) THEN
		  IF (.NOT. inlimits(j,t,contr)) limit = .TRUE.
	        END IF
	     END DO
	   ENDIF !drive to zero entails moving slitmotors
	   IF (limit) THEN
	     CALL Putline (contr, ' Configuration outside limits. ')
	     RETURN
	   END IF

          
C------------- DRIVE MOTORS TO NEW POSITIONS  --------------C

	response(1:) = 'Driving to Q '
	CALL Putline (contr, response)

	IF (qznow .eq. 0. .and. qxnow .eq. 0.)THEN !Drive slits as per Sushil
	  DO kj = 1,4
	    j =slitlist(kj)
  	    IF (motfl(j) .AND. .NOT. fixed(j)) CALL Drive (j, qzero(kj) + z(j))
	  ENDDO
 	  CALL Yield !wait for slits to be in position
	ENDIF !slits moving cause of drive to zero

	DO kj = 1, tomove !check all motors to be moved
	   j = motlist(kj)
  	   IF (motfl(j) .AND. .NOT. fixed(j)) CALL Drive (j, posit(j) + z(j))
	END DO

 	CALL Yield               
	IF (READEF(contr_A))THEN         
	   CALL Putline (contr, ' **** ABORT during drive ****')
	END IF

	END
	
