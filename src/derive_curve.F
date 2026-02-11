#include "icp.h"
	SUBROUTINE Derive_Curve(range, delta, x, y, int,
     1            jmax, jmin, npt, ierr)
C	***************************************************

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C
C  At call:
C	range = range in degrees covered by scan with current position as center
C	delta = step size in degrees
C	npt = number of points at which counts were made
C       x(1->npt) = position of mot at each point
C       y(1->npt) = counts at each point (fratio at each point for type 5 & 6)
C       int has intensity for fitting
C	jmax = index of x,y corresponding to the position with max intensity
C	jmin = index of x,y corresponding to the position with min intensity
C  At return:
C       all of the above except for delta have been modified
C	ierr = 0 if accomplished without problem
C	ierr = 3 is fatal error 
C                                                                             C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C

	IMPLICIT     none
	INTEGER*4    npt, ierr
	INTEGER*4    jmax, jmin
        REAL*4       range, delta
	REAL*4       x(100), y(100), int, newcount(100)
	INTEGER*4    i,j

	ierr = 0 !default return

	IF (npt .lt. 5) THEN !fatal error
	  CALL Putcontr_NT(' Not enough points to fit to S-curve.') 
	  ierr = 2
	  RETURN
	ENDIF
	IF (delta .eq. 0.0)THEN
	  CALL Putcontr_NT(' Cannot fit to S-curve with 0 stepsize.')
	   ierr = 2
	  RETURN
	ENDIF
C  First, adjust points:
	do i = 3, npt-2
	  newcount(i) = (1./delta) *
     1	    ( (0.08333*y(i-2)) - (0.6667*y(i-1)) + 
     1	    (0.66667*y(i+1)) - (0.08333*y(i+2)) )
	enddo

C  Now, overwrite y array with adjusted data and shift x array accordingly:
	do i = 3, npt-2
	  j = i - 2
	  y(j) = newcount(i)
	  x(j) = x(i)
	enddo

C  Reduce number of points:
	npt = npt - 4

C  Refind location of maximum and minimum counts 
C  (Jmax and jmin are indices in the arrays x and y.)
	jmax = npt/2 !assume middle as default
	jmin = npt/2 !assume middle as default
	do i = 1, npt
	   IF (y(i) .GT. y(jmax)) THEN
	      jmax = i
	   ELSE IF (y(i) .LT. y(jmin)) THEN
	      jmin = i
	   ENDIF
	enddo

C  Recalculate integrated intensity to return for fitting   
	int = .5*(y(1) + y(npt))
	DO j= 2, npt-1
	   int = int + y(j)
	END DO
	int = int * delta  !delta is the stepsize of the motor

C  Change range since it is used in calculating fitting params:
	range = delta * (npt - 1)

	RETURN
	END
