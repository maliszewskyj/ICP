#include "icp.h"
	SUBROUTINE Rec_Lattice (c, cr, tmat)
C	************************************
	use smg_prep
	IMPLICIT   none
	REAL*4     tmat(3,3), vol, cosa,cosb,cosc,ctv, Pi
	REAL*4 SIND,COSD,ATAN2D,ACOSD
	TYPE(q_buf) c
	TYPE(lattice) cr
	
	Pi = ATAN2(0., -1.)
    	vol = c%a * c%b * c%c * SQRT(1.- COSD(c%aa)**2 - COSD(c%bb)**2 -
	1   COSD(c%cc)**2 + 2.*COSD(c%aa)*COSD(c%bb)*COSD(c%cc))
    	cr%a = 2.* Pi * c%b * c%c * SIND(c%aa) / vol
    	cr%b = 2.* Pi * c%c * c%a * SIND(c%bb) / vol
    	cr%c = 2.* Pi * c%a * c%b * SIND(c%cc) / vol
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

	SUBROUTINE Mat_Vec (mat, vec1, vec2)
C	************************************

	IMPLICIT  none
	REAL*4    mat(3,3), vec1(3), vec2(3)
	INTEGER*4 i, j

	DO i = 1, 3
 	   vec2(i) = 0.
 	   DO j = 1, 3
	      vec2(i) = vec2(i) + mat(i,j) * vec1(j)
	   END DO
	END DO
	END


	REAL*4 FUNCTION Dot_Pro (u, v)
C	*******************************

	IMPLICIT   none
	REAL*4     u(3), v(3)	
	INTEGER*4  j

	Dot_Pro = 0. 
	DO j = 1, 3
	   Dot_Pro = Dot_Pro + u(j)*v(j)
	END DO
	END

	SUBROUTINE Vec_Pro (u, v, w)
C	****************************

	IMPLICIT   none
	REAL*4     u(3), v(3), w(3)	
      	w(1) = u(2)*v(3) - u(3)*v(2) 
      	w(2) = u(3)*v(1) - u(1)*v(3)
      	w(3) = u(1)*v(2) - u(2)*v(1)
 	END
	

	SUBROUTINE Mat_Mat (u, v, w)
C	****************************

	IMPLICIT  none
	REAL*4    u(3,3), v(3,3), w(3,3)
	INTEGER*4 i, j, k

	DO i = 1, 3
 	   DO j = 1, 3
	      w(i,j) = 0.
	      DO k = 1, 3
		 w(i,j) = w(i,j) + u(i,k)*v(k,j)
	      END DO		 
	   END DO
	END DO
	END




	REAL*4 FUNCTION Energy_Angle (ex, ds)
C	*************************************

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C                                                                             C
C     Calculates Monochromator Or Analyzer Angles As A Function Of Energy     C
C     And D-spacing                                                           C
C     Called from: Run_Q_Buf, Run_T_Buf, Updte_Q_Buf, Updte_T_Buf             C
C                                                                             C
C     Beat Schmid, October 1989                                               C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C


	IMPLICIT  none
	REAL*4    ex, ds, arg
	REAL*4    ASIND
	REAL*8    c, h, mn, conv
	
	PARAMETER  (h  = 6.6260755)                    ! [J·s]
	PARAMETER  (mn = 1.6749286)                    ! [kg]
	PARAMETER  (conv = 1.60217733)                 ! [J/eV]

	c = SQRT (5.* h**2 / (mn * conv))
	IF (ex .GT. 0.) THEN
	   arg = c / (2. * ds * SQRT(ex))
	   IF (arg .LE. 1.) THEN
	      Energy_Angle = ASIND (arg)
	   ELSE
	      Energy_Angle = 90.
	   END IF
	   RETURN
	END IF
	Energy_Angle = 0.
	END
C
C
C
C
	REAL*4 FUNCTION Angle_Energy (an, ds)
	IMPLICIT  none
	REAL*4    an, ex, ds, arg
	REAL*4    SIND
	REAL*8    c, h, mn, conv

	PARAMETER  (h  = 6.6260755)                    ! [J·s]
	PARAMETER  (mn = 1.6749286)                    ! [kg]
	PARAMETER  (conv = 1.60217733)                 ! [J/eV]

	c = SQRT (5.* h**2 / (mn * conv))
	ex = (c * c) / (4 * ds * ds * SIND(an) * SIND(an))
	Angle_Energy = ex
C	print *,' ANGLE_ENERGY: A1 = ',an,' ds = ',ds,' ENERGY = ',ex

	END


	REAL*4 FUNCTION Phi_Angle (q, a1, dm, a2, da)
C	************************************************ 

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C                                                                             C
C     Calculates Phi As A Function Of (hkl), Energy  And Lattice Constants.   C
C     Uses The Cosine-satz To Calculate The Unknowns Of A Triangle.           C
C     Called from: Run_Q_Buf                                                  C
C                                                                             C
C     Beat Schmid, October 1989                                               C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C


	IMPLICIT  none
	REAL*4    q(3), a1, dm, a2, da
	REAL*4	  Pi, ki, kf, q2, arg, Dot_Pro
	REAL*4    ACOSD,SIND,ATAN2D

	Pi  = ATAN2 (0., -1.)
   	q2  = Dot_Pro (q,q)
	ki  = Pi / (dm * SIND(a1))
	kf  = Pi / (da * SIND(a2))
	IF ( ABS(q2) .LT. 1e-8 ) THEN
	   Phi_Angle = 0.
	ELSE
	   arg = (ki**2 + kf**2 - q2) / ( 2. * ki * kf)
	   IF (ABS(arg) .LE. 1.) THEN
	      Phi_Angle = ACOSD (arg) 
	   ELSE
	      Phi_Angle = 999.
	   END IF
	END IF
	END



	REAL*4 FUNCTION Psi_Angle (q, a1, a2, v1, v1a, v2)
C	**********************************************************

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C                                                                             C
C     Calculates Psi As A Function Of (hkl), Energy  And Lattice Constants.   C
C     Called from: Run_Q_Buf                                                  C
C                                                                             C
C     Beat Schmid, October 1989                                               C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C

	use instr
	use focus
	IMPLICIT  none
	INTEGER   j,nmin,nmax,nshort
	REAL*4    q(3), a1, a2, v1a, v1(3), v1b, v2(3), v2b, v3(3)
	REAL*4	  Pi, ki, kf, qq, arg1, arg2, Dot_Pro, alpha, delta,
     1	         psi_angl,hard, rdangl,ll3,ul3,z3
	REAL*4    cpsi, dhard, dangmax
	REAL*4    ATAN2D,SIND,ACOSD
	Pi  = ATAN2 (0., -1.)


C----------------------------- CALCULATE DELTA -------------------------------C

   	qq  = SQRT (Dot_Pro(q,q)) 
	ki  = Pi / (dm * SIND(a1))
	kf  = Pi / (da * SIND(a2))
	IF (ABS(qq) .LT. 1e-6 ) THEN
	   arg1 = 1.
	ELSE
	   arg1 = (ki**2 + qq**2 - kf**2) / (2. * ki * qq)
	END IF
	IF (ABS(arg1) .LE. 1.) THEN
	   delta = ACOSD (arg1)
	ELSE !treat error condition by setting out of limit value
	  psi_angle=ul(3)-z(3)+1.
c	   Psi_Angle = 998.
	  RETURN
	END IF


C-------------------------- FIND ORTHOGONAL BASIS ----------------------------C

	CALL Vec_Pro (v1,v2, v3)
	CALL Vec_Pro (v3,v1, v2)
	v1b = SQRT(Dot_Pro(v1,v1))
	v2b = SQRT(Dot_Pro(v2,v2))
	DO j = 1, 3
	   v1(j) = v1(j) / v1b
	   v2(j) = v2(j) / v2b
	END DO

C----------------------------- CALCULATE ALPHA -------------------------------C

	arg1 = Dot_Pro(q,v2)
	arg2 = Dot_Pro(q,v1)
	IF (ABS(arg1) .LT. 1e-6 .AND. ABS(arg2) .LT. 1e-6) THEN
	   alpha = 0.
	ELSE
	   alpha = ATAN2D (arg1,arg2)
	END IF
	psi_angl = MOD(90. - delta + alpha, 360.)
	hard = psi_angl + z(3)
	nmin = nint((ll(3)-hard)/360. + .5)
	nmax = nint((ul(3)-hard)/360. - .5)
	if (nmax .lt. nmin) then !treat error condition by setting out of
c           limit value
	  Psi_Angle=ul(3)-z(3)+1.
	  return
 	endif
c Below removed because should never call Rdangl if not privileged.
c	if (Rdangl(3) .lt. ll(3)) then
c	    cpsi = ll(3)
c	elseif (Rdangl(3) .gt. ul(3)) then
c	    cpsi = ul(3)
c	else
c	    cpsi = Rdangl(3)
c	endif
c Replaced with:
	if (.not.priv) then
	  psi_angle=psi_angl
	  return
	endif
	cpsi=Rdangl(3)
	if (cpsi .eq. -999.0) return ! Bail out on a read error
	if (cpsi .lt. ll(3))cpsi=ll(3)
	if (cpsi .gt. ul(3))cpsi=ul(3)
C

	dhard = (cpsi - hard)/360.
	dangmax = 360.
	do j=nmin,nmax
	    cpsi = 360.*abs(dhard - j)
	    if (cpsi .lt. dangmax ) then
		nshort = j
		dangmax = cpsi
	    endif
	enddo

c	if (dhard .ge. 0.) then
c	    nshort = int(dhard + .5)
c	else
c	    nshort = int(dhard - .5)
c	endif
c	if (nshort .lt.nmin) nshort=nmin
c	if (nshort.gt.nmax)nshort=nmax
	IF (nsta .eq. -5 .and. negqpos)THEN
	  Psi_Angle = psi_angl
	ELSE
	  Psi_Angle = psi_angl + nshort*360.
	ENDIF
	END
