#include "icp.h"
	SUBROUTINE Refl_Pos (qz,qx,alphai,alphaf,posit)
C	*****************************************

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C                                                                             C
C     Calculate positions for reflectometer motors based on information in    C
C	rbuf (such as QX,QZ,scan_mode). Positions are returned in             C
C       array posit. Alphai & alphaf are returned.                                                           C
C     Called from: Update_r,Run_r_buf,Dry_r_buf                               C
C                                                                             C
C     Gayle Greene, Feb    1993                                               C
C      Based on equations provided by Dave Weisler                            C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C

	use refl
	use instr
	IMPLICIT     none

	REAL	     posit(MAXMOTS),alphai,alphaf,q,pi/3.14159266/,qz,qx
	REAL*4       ASIND,ACOSD,ATAND,SIND,COSD,TAND
	CHARACTER*1  str1
	CHARACTER*7  str7,PutStr
	CHARACTER*8  str8
	CHARACTER*80 response
	INTEGER*4 I

	q = SQRT(qx**2 + qz**2)

	IF (qz .NE. 0.) THEN
	  alphaf = ASIND(q*wavel/(4.*pi)) - ATAND(qx/qz)
	  alphai = ASIND(q*wavel/(4.*pi)) + ATAND(qx/qz)
	ELSE !qz = 0.
	  alphaf = 0.
	  alphai = 0.
	ENDIF

c-- Motor definitions:
c 	 1. monochromator theta
c	 2. monochromator chi
c	 3. monochromator translation
c	 4. theta PG filter
c	 5. height of Be filter
c	 6. height of PG filter
c	 7. Slit 1 center height
c	 8. Slit 1 opening
c	 9. linear actuator on monochromator drum
c	10. linear actuator before sample
c	11. table height
c	12. sample elevation
c	13. sample omega
c	14. sample chi
c	15. Slit 2 opening
c	16. Slit 3 opening
c	17. Slit 4 opening
c	18. Detector tower height 


	posit(1) = asind(wavel/(2.*dm))
	posit(2) = asind((dm*sind(alphai))/wavel)
c 	posit(2) = alphai/2.

c- For 2.35 Angstroms, use PG filter.  Otherwise use Be filter
	if (wavel.lt.3) then
	   posit(4) = alphai
	   posit(6) = rxPG * tand(alphai) 
	else   
	   posit(5) = rxBe * tand(alphai) 
	endif

	posit(7) = rxs1 * tand(alphai)
	posit(9) = SQRT(rxp1**2 + rzp1**2 + rrp**2 - 
     1	  2 * rrp * (rxp1 * cosd(rthetap-alphai) + rzp1 * sind(rthetap-alphai)))-
     1            SQRT(rxp1**2 + rzp1**2 + rrp**2 -
     1	  2 * rrp * (rxp1 * cosd(rthetap) + rzp1 * sind(rthetap)))
	posit(10) = rxp2 * tand(alphai) + (rh1+rh2)*((1-cosd(alphai))/cosd(alphai))
	posit(11) = rxt * tand(alphai)

	IF (qz .LT. 0)THEN
	  posit(2)= -posit(2)
	  posit(5)= -posit(5)
	  posit(7)= -posit(7)
	  posit(9)= -posit(9)
	  posit(10)= -posit(10)
	  posit(11)= -posit(11)
	ENDIF
C	posit(18)  = -(rxp3 - rxt)*tan(alphaf) + rxt*tan(alphai)
C        Pat G. said 18 should not be moved except explicitly for now. 12/93.

	RETURN
	END
