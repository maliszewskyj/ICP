#include "icp.h"
	SUBROUTINE Dry_Q_Buf (buf_no)
C	*****************************

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C                                                                             C
C     Executes Const-Q Buffer No. buf_no in "dry mode"                        C
C     Called from: Parse_Contr                                                C
C                                                                             C
C     Beat Schmid, June 1990                                                  C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
	use instr
	use smg_prep
	use tc
	use focus
	use globals
	IMPLICIT      none
	INTEGER*4     i, j, m, typ, prf, loop, buf_no, k, ios
	REAL*4        posit(10), e0, h0, k0, l0, energy, tmax, hmax, fmax
	REAL*4	      tmat(3,3), q0(3), q(3), v1(3), v2(3), zj, Fz
	REAL*4	      Energy_Angle, Phi_Angle, Psi_Angle, temp /0./
	CHARACTER*2   bufno
	CHARACTER*6   str6(4)
	CHARACTER*7   st7(3), PutStr
	CHARACTER*8   out
	CHARACTER*10   str10
	CHARACTER*12   str12(4)
	CHARACTER*80  response, help_line(3)
	LOGICAL       limit,mlimit(10),llimit(10), inlimits

	TYPE(q_buf) qbuf
C	TYPE(q_buf) man_q ! in smg_prep
	TYPE(lattice) cr
C	COMMON /drive_to_q/ man_q

	OPEN (UNIT= 1, FILE= 'QBUFFER.BUF', STATUS= 'OLD', ACCESS= 'DIRECT', 
	1     FORM= 'UNFORMATTED', RECL=QBUF_RECL)
	READ (1,REC=buf_no) qbuf
	CLOSE (1)
	man_q = qbuf
	lasta = qbuf%a !memorize last lattice constants
	lastb = qbuf%b
	lastc = qbuf%c
	lastef = qbuf%ef


C-------------------------------PRINT HEADER --------------------------------C

	CALL Putline_NF (contr, ' ')
	CALL Putline_NF (contr, ' ')
c	CALL DATE_TIME (sys_time)
	WRITE (UNIT= bufno(1:2), FMT= '(i2)') buf_no
	response = 'QBuffer '// bufno // ':  '// qbuf%com
	CALL Putline_NT (contr, response)
	response = ' '
	CALL Putline_NF (contr, response)
	st7(1) = PutStr (qbuf%ec)
	st7(2) = PutStr (qbuf%es)
	st7(3) = PutStr (qbuf%ef)
	response = 'E-center = '// st7(1) //'  E-step = '// st7(2)
	IF (qbuf%eft .EQ. 'ANA') THEN
	   typ = 2
	   response = response(1:36) //'     Analyzer fixed at '// st7(3) //
	1	' meV'
	ELSE
	   typ = 3
	   response = response(1:36) //'     Monochromator fixed at '// st7(3)
	1	//' meV'	
	END IF
	CALL Putline_NF (contr, response)	
	WRITE (UNIT= str6(1), FMT= '(f6.3)') qbuf%hc
	WRITE (UNIT= str6(2), FMT= '(f6.3)') qbuf%kc
	WRITE (UNIT= str6(3), FMT= '(f6.3)') qbuf%lc   
	response = 'Q-center = ('// str6(1) //','// str6(2) //','//str6(3)//')'
	WRITE (UNIT= str6(1), FMT= '(f6.4)') qbuf%hs
	WRITE (UNIT= str6(2), FMT= '(f6.4)') qbuf%ks
	WRITE (UNIT= str6(3), FMT= '(f6.4)') qbuf%ls   
	response = response(1:33) //'      Q-step = ('// str6(1) //','// str6(2)
	1  //','// str6(3) //')'
	CALL Putline_NF (contr, response) 

	CALL Putmax(qbuf%tmp,str6(1),6,3)
	CALL Putmax(qbuf%it,str6(2),6,3)
	CALL putmax(qbuf%ter,str6(3)(1:3),3,2)
	WRITE (UNIT= str6(4)(1:3), FMT= '(i3)') NINT(qbuf%tw)
	call putmax(qbuf%hf,str10,10,4)
c	WRITE (UNIT= out(1:8), FMT= '(f8.2)') qbuf%hf
	response = 'Temp  = '// str6(1) //'  Inc-T= '// str6(2) //
	1    '  T-Err= '// str6(3)(1:3) //'   T-wait= '// str6(4)(1:3) 
	2    //' min  H-Field= '// str10
	CALL Putline_NF (contr, response)
	IF (qbuf%hf .NE. 0 .OR. qbuf%ihf .NE. 0) THEN !magnetic field 
	   call putmax(qbuf%hf,str12(1),12,4)
	   call putmax(qbuf%ihf,str12(2),12,4)
	   write(unit=str12(3),fmt='(I12)',iostat=ios)NINT(qbuf%hfw)
c       call putmax(qbuf%hfw,str12(3),12,4)
	   call putmax(hfconv,str12(4),12,4)
	   response(1:)='HField= '//str12(1)//' IncH= '//str12(2)//
	1	' Hwait= '//
	2	str12(3)//' Hconv= '//str12(4)
	   CALL Putline_NF (contr, response)
	ENDIF			!magnetic field

	WRITE (UNIT= out(1:7),     FMT= '(i7)') NINT(qbuf%mon)
	WRITE (UNIT= str6(1)(1:3), FMT= '(i3)') NINT(qbuf%mpf)
	WRITE (UNIT= str6(2)(1:3), FMT= '(i3)') NINT(qbuf%pts)
	response = 'Monitor= '// out(1:7) //'  Prefac= '// str6(1)(1:3)
	1  //'  Type= '// qbuf%mt //'  #pts = '// str6(2)(1:3)
	CALL Putline_NF (contr, response)
	CALL Putline_NF (contr, ' ')


C----------------------------PRINT DATA HEADER ------------------------------C

	response(1:) = ' '
	m = 1 
	DO j = 1, 6
	   IF (motfl(j)) THEN
	      WRITE (UNIT= response(m+1:m+9), FMT= '(a5,i1,a3)') 
	1	   '    A',j,'   '  
	      m = m + 9
	   END IF
	END DO
	IF (tempctr .AND. qbuf%tmp .NE. 0. .AND. qbuf%it .NE. 0.) THEN
	   WRITE (UNIT= response(m+1:m+9), FMT= '(a9)') '   T-set '
	END IF
	CALL Putline_NF (contr, response)
	response = '       H       K       L         E   '
	CALL Putline_NF (contr, response)
	CALL Putline_NF (contr, ' ')
	


C--------CALCULATE STARTING VALUES FOR ENERGY AND RECIPROCAL SPACE ----------C

	CALL Rec_Lattice (qbuf, cr, tmat)
	CALL Mat_Vec (tmat, qbuf%hkl1, v1)
	CALL Mat_Vec (tmat, qbuf%hkl2, v2)
	e0 = qbuf%ec - ((qbuf%pts -1.) / 2.) * qbuf%es
	h0 = qbuf%hc - ((qbuf%pts -1.) / 2.) * qbuf%hs
	k0 = qbuf%kc - ((qbuf%pts -1.) / 2.) * qbuf%ks
	l0 = qbuf%lc - ((qbuf%pts -1.) / 2.) * qbuf%ls
	


C-----------------------------------------------------------------------------C
C---------------------COUNT-LOOP OVER NUMBER OF POINTS ----------------------C
C-----------------------------------------------------------------------------C

	limit=.false.
	DO j = 1,6
	   llimit(j) = .false.
	ENDDO

	DO loop = 0, NINT(qbuf%pts) - 1


!       C------------ CALCULATE ENERGY-TRANSFER AND ------------------------C
!       C--------- CORRESPONDING MONO AND ANA ANGLES -----------------------C

	   energy = e0 + loop * qbuf%es		
	   IF (qbuf%eft .EQ. 'ANA') THEN
	      posit(1) = Energy_Angle (qbuf%ef+energy, dm)
	      posit(2) = 2. * posit(1)
	      posit(5) = Energy_Angle (qbuf%ef, da)
	      posit(6) = 2. * posit(5)
	   ELSE
	      posit(1) = Energy_Angle (qbuf%ef, dm)
	      posit(2) = 2. * posit(1)
	      posit(5) = Energy_Angle (qbuf%ef-energy, da)
	      posit(6) = 2. * posit(5)
	   END IF


!       C------- CALCULATE THE MOMENTUM-TRANSFER AND THE -------------------C
!       C---------- CORRESPONDING PHI AND PSI ANGLES -----------------------C

	   q0(1) = h0 + loop * qbuf%hs
	   q0(2) = k0 + loop * qbuf%ks
	   q0(3) = l0 + loop * qbuf%ls

	   CALL Mat_Vec (tmat, q0, q)
	   posit(3) = Psi_Angle (q, posit(1), posit(5), 
	1	v1, qbuf%a1, v2)
 	   IF (q(1) .EQ. 0. .AND. q(2) .EQ. 0. .AND. q(3) .EQ. 0.) THEN
	      IF (nsta .eq. -5 .and. negqpos)THEN
		 posit (4) = min(0.,ul(4))
	      ELSE
		 posit (4) = max(0.,ll(4))
	      ENDIF
c       R Erwin  Jun 4 92  posit(4) should be soft zero if q = 0
	   ELSE
	      posit(4) = Phi_angle (q, posit(1), dm, posit(5), da)
	   END IF
	   IF (nsta. eq. -5 .and. negqpos)THEN
	      posit(3) = -posit(3)
	      posit(4) = -posit(4)
	   ENDIF


!       C------------- CALCULATE NEW TEMPERATURE --------------------------C


	   temp = qbuf%tmp + qbuf%it*loop


!       C----------------------- PRINT NEW POSITION -----------------------C

	   response(1:) = ' ' 
	   m = 1
	   DO j = 1, 6		! (* print new position first *)
	      mlimit(j)=.false.
	      IF (motfl(j)) THEN
		 WRITE (UNIT= response(m+1:m+9), FMT='(f9.3)') posit(j)
	         IF (nsta.eq.4 .and. z(j).eq.-99.)THEN !special flag
		    zj = Fz(j,posit(j))
	         ELSE 
		    zj = z(j)
	         ENDIF
		 IF (.NOT. inlimits(j,posit(j)+zj,contr))THEN
	            limit = .TRUE.
	            mlimit(j) = .TRUE.
	         END IF
		 if (.not.mlimit(j))then
		    response(m+10:m+11)= '  '
	         else
		    llimit(j)=.true.
		    response(m+10:m+11)= '* '
	         endif
		 m = m + 11
	      END IF
  	   END DO	   
	   IF (tempctr .AND. qbuf%tmp .NE. 0. .AND. qbuf%it .NE. 0.) WRITE 
	1	(UNIT= response(m+1:m+9), FMT= '(f9.3)') temp
	   CALL Putline_NF (contr, response)
	   WRITE (UNIT= response, FMT= '(f11.4,2f8.4,f9.3,a4)')
	1	q0(1), q0(2), q0(3), energy, '    '
	   m = 40
	   CALL Putline_NF (contr, response(1:m))
	END DO			! (* Number of total number of points *)
	if (.not.limit) then	!no limits exceeded
	   response(1:) = 'Above angles did not exceed limits.'
	else
	   response(1:46)='WARNING: Angle limits exceeded in motors(s):  '
	   k=46
	   DO j=1,6
	      IF (llimit(j))then
		 write(unit=out(1:2),fmt='(I2)')j
		 response(1:)=response(1:k)//out(1:2)//' '
		 k=k+3
	      ENDIF
	   ENDDO
	endif
	CALL Putline_NT (contr, response)
	response(1:) = 'Angles are given in software values!'
	CALL Putline_NF (contr, response)
	CALL Putline_NF (contr, ' ')
	IF (qbuf%tmp .NE. 0 .AND. qbuf%it .NE. 0)THEN
	   CALL Putmax(qbuf%tmp,str6(1),6,3)
	   tmax = qbuf%tmp + (qbuf%pts-1)*qbuf%it
	   CALL Putmax(tmax,str6(2),6,3)
	   CALL Putline_NT(contr,'Temp range: '//str6(1)//' to '//str6(2))
	   CALL Putline_NF (contr, ' ')
	ENDIF
	IF (qbuf%ihf .NE. 0 )THEN
	   call putmax(qbuf%hf,str12(1),12,4)
	   hmax = qbuf%hf + (qbuf%pts-1)*qbuf%ihf
	   call putmax(hmax,str12(2),12,4)
	   CALL Putline_NT(contr,'Hfield range: '//str12(1)//' to '//str12(2))
	   IF (magnctr)THEN
	      fmax = fieldmax * hfconv
	      IF (abs(hmax) .GT. fmax .OR. abs(qbuf%hf) .GT. fmax)
	1	   CALL Putline_NT (contr, 'WARNING !!! Field out of range ')
	   ELSE
	      CALL Putline_NF (contr,' Hfield bounds not checked unless H+')
	   ENDIF
	   CALL Putline_NF (contr, ' ')
	ENDIF
	END
