#include "icp.h"
	SUBROUTINE Bragg_Pos (bbuf,bpos,a_line,mposit,comflag,scrflag)
C	*****************************************

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C                                                                             C
C     Calculate central position for motors 3 & 4 and only position for       C
C	all other motors based on information in b_buf. These positions       C
C       are returned in bpos. If a complanar error results, the flag is true  C
c       and motor positions are set to -999.	                              C
C     Called from: PrepB                                                      C
C                                                                             C
C     Gayle Greene, August 1992                                               C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
	use instr
	use smg_prep
	IMPLICIT     none

	INTEGER*2    wtc
	INTEGER*4    k, j, j0, row,ig,npts,a_line(10),mposit(10)
	REAL*4       q0(3), qq0(3), qf(3), qqf(3), v1(3), v2(3), v3(3)
	REAL*4	     e0, ef, Energy_Angle, Phi_Angle, Psi_Angle, posit(12)
	REAL*4	     Dot_Pro, tmat(3,3), t, complanar, halfway, mpos(12)
	REAL*4	     bpos(10),foc7,foc8,focpos
	LOGICAL      comflag,scrflag
	CHARACTER*1  str1
	CHARACTER*7  str7,PutStr
	CHARACTER*8  str8
	CHARACTER*80 line(NBUF), response

	TYPE(b_buf) bbuf
	TYPE(lattice) cr


C----------- CALCULATE first & last VALUES FOR ENERGY AND Q-SPACE ------------C

	npts = 1 !for b_buffer, only one energy
	CALL bragg_RecLat (bbuf, cr, tmat)
	halfway = (npts - 1) / 2.
	e0    = bbuf%ec - halfway * bbuf%es
	q0(1) = bbuf%hc - halfway * bbuf%hs
	q0(2) = bbuf%kc - halfway * bbuf%ks
	q0(3) = bbuf%lc - halfway * bbuf%ls
	ef    = bbuf%ec + halfway * bbuf%es
	qf(1) = bbuf%hc + halfway * bbuf%hs
	qf(2) = bbuf%kc + halfway * bbuf%ks
	qf(3) = bbuf%lc + halfway * bbuf%ls

	CALL Mat_Vec (tmat, q0, qq0)
	CALL Mat_Vec (tmat, qf, qqf)
	CALL Mat_Vec (tmat, bbuf%hkl1, v1)
	CALL Mat_Vec (tmat, bbuf%hkl2, v2)
	CALL Vec_Pro (v1, v2, v3)
	complanar = Dot_Pro (v3, qq0)

	IF (bbuf%eft .EQ. 'ANA') THEN
	   posit(1)  = Energy_Angle (bbuf%ef+e0, dm)
	   posit(7)  = Energy_Angle (bbuf%ef+ef, dm)
	   posit(2)  = 2. * posit(1)
	   posit(8)  = 2. * posit(7)
	   posit(5)  = Energy_Angle (bbuf%ef, da)
	   posit(11) = posit(5)
	   posit(6)  = 2. * posit(5)
	   posit(12) = 2. * posit(5)
	ELSE
	   posit(1)  = Energy_Angle (bbuf%ef, dm)
	   posit(7)  = posit(1)
	   posit(2)  = 2. * posit(1)
	   posit(8)  = 2. * posit(1)
	   posit(5)  = Energy_Angle (bbuf%ef-e0, da)
	   posit(11) = Energy_Angle (bbuf%ef-ef, da)
	   posit(6)  = 2. * posit(5)
	   posit(12) = 2. * posit(11)
	END IF


	posit(3)  = Psi_Angle (qq0, posit(1), posit(5), v1, bbuf%a1, 
     1	     v2)
	posit(9)  = Psi_Angle (qqf, posit(1), posit(5), v1, bbuf%a1,
     1	     v2)
	posit(4)  = Phi_Angle (qq0, posit(1), dm, posit(5), da)
	posit(10) = Phi_Angle (qqf, posit(7), dm, posit(11), da)

	IF (ABS(complanar) .GT. 1E-2) THEN !q-vector not in scattering plane
	  comflag = .true.
	  DO j0 = 1, 6
	    bpos(j0) = -999.
	  ENDDO
	ELSE
	  comflag = .false.
	  DO j0 = 1, 6
	    bpos(j0) = posit(j0)
	  ENDDO
	END IF

	IF (.not. scrflag) return !only continue if prpb to be updated

	DO j0 = 2,6
	  IF (.not. comflag) THEN
	    str7 = PutStr (bpos(j0))
	  ELSE
	    str7(1:) = ' '
	  ENDIF
	  CALL PUTCHARS (prpb, str7, a_line(j0), mposit(j0))
	ENDDO

	IF (nsta .EQ. 4 .or. nsta .EQ. -5 .or. nsta .EQ. 2 .or.
     1	    nsta .EQ. 9) THEN
	  IF (.not. comflag) THEN !motor 1
	    str7 = PutStr (bpos(1))
	  ELSE
	    str7(1:) = ' '
	  ENDIF
	  CALL PUTCHARS (prpb, str7, a_line(1), mposit(1))
	  IF (.not. comflag) THEN
	    focpos=foc7(bpos(2))
	    str7 = PutStr (focpos) 
	  ELSE
	    str7(1:) = ' '
	  ENDIF
	  CALL PUTCHARS (prpb, str7, a_line(7), mposit(7))
	ENDIF
	IF (nsta .EQ. 4) THEN
	  IF (.not. comflag) THEN
	    focpos=foc8(bpos(2))
	    str7 = PutStr (focpos) 
	  ELSE
	    str7(1:) = ' '
	  ENDIF
	  CALL PUTCHARS (prpb, str7, a_line(8), mposit(8))
	ENDIF

	RETURN
	END
