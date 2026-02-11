#include "icp.h"
	SUBROUTINE Bragg_RecLat (c, cr, tmat)
C	************************************
	use smg_prep
	IMPLICIT   none
	REAL*4     tmat(3,3), vol, cosa,cosb,cosc,ctv, Pi
	REAL ATAN2D,COSD,SIND,ACOSD

	TYPE(b_buf) c
	TYPE(lattice) cr
	
	Pi = ATAN2 (0., -1.)
    	vol = c%lat_a * c%b * c%c * SQRT(1.- COSD(c%aa)**2 -
	1    COSD(c%bb)**2 - COSD(c%cc)**2 +
	1    2.*COSD(c%aa)*COSD(c%bb)*COSD(c%cc))
    	cr%a = 2.* Pi * c%b * c%c * SIND(c%aa) / vol
    	cr%b = 2.* Pi * c%c * c%lat_a * SIND(c%bb) / vol
    	cr%c = 2.* Pi * c%lat_a * c%b * SIND(c%cc) / vol
      	cosa = (COSD(c%bb) * COSD(c%cc) - COSD(c%aa)) /
	1    (SIND(c%bb) * SIND(c%cc))
    	cr%aa = ACOSD(cosa)
      	cosb = (COSD(c%cc) * COSD(c%aa) - COSD(c%bb)) /
	1    (SIND(c%cc) * SIND(c%aa))
    	cr%bb = ACOSD(cosb)
      	cosc = (COSD(c%aa) * COSD(c%bb) - COSD(c%cc)) /
	1    (SIND(c%aa) * SIND(c%bb))
    	cr%cc = ACOSD(cosc)
	ctv = (cosc * cosb - cosa)/SIND(cr%cc)/SIND(cr%bb)


C------------------ TRAFO FROM CRYSTAL TO CARTESIAN SYSTEM -------------------C

	tmat(1,1) = cr%a
	tmat(1,2) = cr%b * COSD(cr%cc)
	tmat(1,3) = cr%c * COSD(cr%bb)
	tmat(2,1) = 0.
	tmat(2,2) = cr%b * SIND(cr%cc)
	tmat(2,3) = -cr%c * SIND(cr%bb) * ctv
	tmat(3,1) = 0.
	tmat(3,2) = 0.
	tmat(3,3) = cr%c * SIND(cr%bb) * sqrt(1-ctv * ctv)
	END
