#include "icp.h"
! inlimits  --  return true if target position pos is within limits of motor mot
      	LOGICAL FUNCTION inlimits(mot,pos,whscr)
C	****************************

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C                                                                  
C	Inlimits: return true if target position is inside of defined softwareC
C         limits for motor mot                                                C
C                                                                             C
C       Gayle Greene    July, 1996                                            C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
	use instr
	use virtmot
	use dartz
	use smg_prep
	use refl
	IMPLICIT NONE
	
C  Calling parameters
	INTEGER*4	mot, whscr
	REAL*4		pos

C  Local variables
	LOGICAL		nomessage, errflag
	INTEGER*4	ios, i, imot
	INTEGER*4	nummots, lpointer
	REAL*4		rpos
	CHARACTER*2	str2
	CHARACTER*20	str20
	CHARACTER*80	response

	nomessage = .FALSE.

	inlimits = .TRUE.	!target position OK by default
	write(unit=str2,fmt='(I2)',iostat=ios)mot
	write(unit=str20,fmt=*,iostat=ios)pos
	IF (mottype(mot) .NE. virt)THEN
	   IF (pos .GT. ul(mot)) THEN
	      IF (.NOT. nomessage)THEN
		 response(1:) = 'Motor '//str2//' at '//str20//
	1	      ' would exceed upper limit.'
		 CALL PUTLINE (whscr, response)
	      ENDIF
	      inlimits = .FALSE.
	   ELSE IF (pos .LT. ll(mot)) THEN
	      IF (.NOT. nomessage)THEN
		 response(1:) = 'Motor '//str2//' at '//str20//
	1	      ' would go beyond lower limit.'
		 CALL PUTLINE (whscr, response)
	      ENDIF
	      inlimits = .FALSE.
	   ENDIF
	ELSE			!virtual motor
	   errflag = .FALSE.
	   IF (nsta .NE. 8 .and. nsta .NE. 9)THEN
	      errflag = .TRUE.
	   ELSE
C       Calculate positions of real motors represented by virtmot. Put in realpos list.
	      CALL CALC_REALPOS(mot,pos,errflag) !go calc pos of real motors
	   ENDIF
	   IF (errflag)THEN
	      IF (.NOT. nomessage)THEN
		 response(1:) = 'Err calc positions for Virtual Motor '//str2
		 CALL PUTLINE(whscr,response)
	      ENDIF
	      inlimits = .FALSE.
	   ELSE			!no errflag
	      nummots = numreal(mot)
	      lpointer = pntlist(mot)

	      do i = lpointer, lpointer + nummots - 1
		 imot = motlist(i)
		 rpos = poslist(i) + z(imot)
		 write(unit=str2,fmt='(I2)',iostat=ios)imot
		 write(unit=str20,fmt=*,iostat=ios)rpos
C       Note: If BT4 uses virt motors, calc of z might have to be added in.
		 IF (rpos .GT. ul(imot)) THEN
		    IF (.NOT. nomessage)THEN
		       response(1:) = 'Motor '//str2//' at '//str20//
	1		    ' would exceed upper limit.'
		       CALL PUTLINE (whscr, response)
		    ENDIF
		    inlimits = .FALSE.
		 ELSE IF (rpos .LT. ll(imot)) THEN
		    IF (.NOT. nomessage)THEN
		       response(1:) = 'Motor '//str2//' at '//str20//
	1		    ' would go beyond lower limit.'
		       CALL PUTLINE (whscr, response)
		    ENDIF
		    inlimits = .FALSE.
		 ENDIF
	      enddo
	   ENDIF		!errflag set in calc_RealPos
	ENDIF			!mottype = virtual
	RETURN

	END
