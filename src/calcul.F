#include "icp.h"
	SUBROUTINE Calcul (command, *)
C	******************************

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C                                                                             C
C     Routine Performs some basic calculations.                               C
C     Called from: Parse_Contr                                                C
C                                                                             C
C     Beat Schmid, February 1990                                              C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C

	use instr
	use globals
	IMPLICIT      none
	INTEGER*4     i
	REAL*4	      Pi, arg1, arg2, zl /1./
	REAL*4        ATAN2D,TAND,ATAND
	REAL*8        c, h, mn, conv
	CHARACTER*2   op
	CHARACTER*50  command
	CHARACTER*80  response

	PARAMETER  (h  = 6.6260755)                    ! [J·s]
	PARAMETER  (mn = 1.6749286)                    ! [kg]
	PARAMETER  (conv = 1.60217733)                 ! [J/eV]

	SAVE    zl

	Pi = ATAN2 (0., -1.)
	c = 5.* h**2 / (mn * conv)

	READ (UNIT= command(1:), FMT= *, ERR= 270) arg1
	IF (arg1 .EQ. 0.) arg1 = zl
	i = INDEX (command, ',')
	IF (i .EQ. 0) GOTO 270     ! (* error *)
	command(1:) = command(i+1:)
	READ (UNIT= command(1:1), FMT= '(a1)', ERR= 270) op(1:1)
	
	IF (op(1:1) .EQ. '+' .OR. op(1:1) .EQ. '-' .OR. op(1:1) .EQ. 'X' .OR.
	1   op(1:1) .EQ. '/') THEN
	   READ (UNIT= command(2:), FMT= *, ERR= 10) arg2
	   GOTO 11
   10	   IF (command(2:2) .EQ. 'Z') THEN
	      arg2 = zl
	   ELSE 
	      GOTO 270
	   END IF
   11	   IF (op(1:1) .EQ. '+') THEN			! Addition
	      zl = arg1 + arg2
	   ELSE IF (op(1:1) .EQ. '-') THEN		! Subtraction
	      zl = arg1 - arg2
	   ELSE IF (op(1:1) .EQ. 'X') THEN		! Multiplication
	      zl = arg1 * arg2
	   ELSE IF (op(1:1) .EQ. '/') THEN		! Division
	      IF (arg2 .NE. 0.) THEN
	         zl = arg1 / arg2
	      ELSE 
	         GOTO 270
	      END IF
	   END IF
	ELSE
	   READ (UNIT= command(2:2), FMT= '(a1)', ERR= 270) op(2:2)
	   command(1:) = command (3:)
	   IF (op(1:2) .NE. 'WE' .AND. op(1:2) .NE. 'EW') THEN
	      READ (UNIT= command(1:), FMT= *, ERR= 20) arg2
	      GOTO 21
   20	      IF (command(1:1) .EQ. 'Z') THEN
		 arg2 = zl
	      ELSE
		 GOTO 270
	      END IF
   21	   END IF
	   IF (op .EQ. 'EW') THEN			! [meV] ==> [Å]
	      IF (arg1 .NE. 0.) THEN
		 zl = SQRT (c/arg1)
	         IF (nsta .ne. 1 .and. nsta .ne. -7)wavel = zl
	      ELSE
		 GOTO 270
	      END IF
	   ELSE IF (op .EQ. 'WE') THEN			! [Å] ==> [meV]
	      IF (arg1 .NE. 0.) THEN
		 zl = c / arg1**2
	      ELSE
		 GOTO 270
	      END IF
	   ELSE IF (op .EQ. 'EX') THEN			! Exponentiation
	      zl = arg1 * exp(arg2)
	   ELSE IF (op .EQ. 'RT') THEN			! Square root
	      zl = arg1 * SQRT(ABS(arg2))
	   ELSE IF (op .EQ. 'LN') THEN			! Logarithm (nat)
	      IF (arg2 .NE. 0.) THEN
	         zl = arg1 * LOG(arg2)
	      ELSE 
	         GOTO 270
	      END IF
	   ELSE IF (op .EQ. '**') THEN			! Potenziation
	      zl = arg1 **arg2   
	   ELSE IF (op .EQ. 'SN') THEN			! Sine
	      zl = arg1 * SIN(arg2)
	   ELSE IF (op .EQ. 'CS') THEN			! Cosine
	      zl = arg1 * COS(arg2)
	   ELSE IF (op .EQ. 'TA') THEN			! Tangent
	      zl = arg1 * TAN(arg2)
	   ELSE IF (op .EQ. 'AS') THEN			! Arcsine
	      IF (ABS(arg2) .LE. 1.) THEN
	         zl = arg1 * ASIN(arg2)
	      ELSE
	         GOTO 270
	      END IF	
	   ELSE IF (op .EQ. 'AC') THEN			! Arcosine
	      IF (ABS(arg2) .LE. 1.) THEN
	         zl = arg1 * ACOS(arg2)
	      ELSE
	         GOTO 270
	      END IF	
	   ELSE IF (op .EQ. 'AT') THEN			! Arctan
	      zl = arg1 * ATAN(arg2)
	   ELSE IF (op .EQ. 'DS') THEN			! ang, L ==> d
	      zl = arg2 / (2. * SIN(arg1/2.))
	   ELSE IF (op .EQ. 'TT') THEN			! d, L   ==> ang
	      IF (arg1 .NE. 0.) THEN
	         zl = 2. * ASIN(arg2/(2.*arg1))
	      ELSE
		 GOTO 270
	      END IF
	   ELSE IF (op .EQ. 'FW') THEN			! d, ang ==> L
	      zl = 2. * arg1 * SIN(arg2/2.)
	   ELSE IF (op .EQ. 'QT') THEN			! Q, L   ==> ang
	      zl = 2. * ASIN(arg1*arg2/(4.*Pi))
	   ELSE IF (op .EQ. 'TQ') THEN			! ang, L ==> Q
	      IF (arg2 .NE. 0.) THEN
	         zl = 4. * Pi * SIN(arg1/2.) / arg2
	      ELSE
	         GOTO 270
	      END IF
	   END IF
	END IF
	WRITE (UNIT= response, FMT= '(f13.4)') zl
	CALL PUTCONTR (response)
	vz = zl
	RETURN 1

  270   response = 'Error in calcul'
	CALL PUTCONTR (response)
	RETURN 1
	END
