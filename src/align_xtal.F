#include "icp.h"
	SUBROUTINE Align_Xtal (mot, range, delta, mon, keep, maxit, tol, ierr,
     1				montemp, fitmin,fitinten,arch,halfh)
C	***************************************************

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C                                                                             C
C	Routine aligns sample or analyzer by iteratively calling align_peak
C	with theta-twotheta and omega scans until tolerance or maxit is
C	reached.
C	Called from: Parse_Contr
C                                                                             C
C	Ross Erwin   June 1992                                                C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
	use instr
	IMPLICIT     none
	INTEGER*4    mot, mot2, type, ierr, i, maxit
        REAL*4       range, delta, mon, zpre(2), tol
	REAL*4       twotheta/0./ !dummy variable here
	Logical keep
	LOGICAL	     montemp,	!monitor temp if true
	1    fitmin,		!fit to minimum if true
	2    fitinten,		!fit to intensity of array if true
	3    arch,		!archive data file
	4    halfh		!drive to half height of max intensity

	type = 2
	IF (mot .EQ. 3) THEN
	   mot2 = 4
	   type = 1
	ELSE IF (mot .EQ. 4) THEN
	   mot2 = 3
	ELSE IF (mot .EQ. 5) THEN
	   mot2 = 6
	   type = 1
	ELSE IF (mot .EQ. 6) THEN
	   mot2 = 5
	ELSE
	   CALL PUTCONTR ('Can only align sample or analyzer.')
	   ierr = 3
	   RETURN
	END IF

	zpre(1) = z(mot)
	zpre(2) = z(mot2)

	DO i=1,maxit
	   CALL align_peak (mot, range, delta, mon, keep, type, tol, ierr,
	1	montemp, fitmin,fitinten,twotheta,arch,halfh)
	   IF (ierr .NE. 0) THEN
	      CALL PUTCONTR ('Failed xtal alignment.')
	      ierr = 3
	      RETURN
	   END IF
	   CALL align_peak (mot2, range, delta, mon, keep, 3-type, tol, ierr,
     1		montemp, fitmin,fitinten,twotheta,arch,halfh)
	   IF (ierr .NE. 0) THEN
	      CALL PUTCONTR ('Failed xtal alignment.')
	      ierr = 3
	      RETURN
	   END IF
	   IF ( abs(z(mot)-zpre(1))  .LE. tol .AND.
     *		abs(z(mot2)-zpre(2)) .LE. tol ) THEN
	      ierr = 0
	      RETURN
	   END IF
	   zpre(1) = z(mot)
	   zpre(2) = z(mot2)
	END DO
	CALL PUTCONTR ('Failed xtal alignment.')
	ierr = 3
	RETURN
	END
