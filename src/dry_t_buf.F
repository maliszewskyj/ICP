#include "icp.h"
	SUBROUTINE Dry_T_Buf (buf_no)
C	*****************************

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C                                                                             C
C     Executes Trash Buffer No. buf_no in dry mode, i.e. only motor 1 and 2   C
C     (ev. motor 3) are significant and "move".	                              C
C     Called from: Parse_Contr                                                C
C                                                                             C
C     Beat Schmid, June 1990                                                  C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
	use instr
	use smg_prep
	use tc
	use iso_c_binding
	IMPLICIT      none
	interface
	   subroutine FANSVEC(energy0,npts,defrac,vector)
	1	bind(C,name="FANSVEC")
	   use iso_c_binding
	   real(c_float),  intent(in)::energy0
	   integer(c_int), intent(in)::npts
	   real(c_float),  intent(in)::defrac
	   real(c_float),  intent(out)::vector(*)
	   end subroutine
	end interface
	INTEGER*4     i, j, m, prf, loop, buf_no, k, ios
	REAL*4        posit(10), e0, energy, Energy_Angle, tmax, hmax, fmax,
     1			zj, Fz
	REAL*4        ef
	REAL*4        energyvec(1024)
	REAL*4        ATAND,SIND
	CHARACTER*2   bufno
	CHARACTER*6   str6(4)
	CHARACTER*7   st7(2), PutStr
	CHARACTER*8   out
	CHARACTER*10   str10
	CHARACTER*12   str12(4)
c	CHARACTER*28  sys_time
	CHARACTER*80  response, help_line(3)
	LOGICAL	      limit,mlimit(10),llimit(10), inlimits

	TYPE(t_buf) tbuf

		
	OPEN (UNIT= 1, FILE= 'TBUFFER.BUF', STATUS= 'OLD', ACCESS= 'DIRECT', 
	1     FORM= 'UNFORMATTED', RECL=TBUF_RECL)
	READ (1,REC=buf_no) tbuf
	CLOSE (1)



C------------------------------- PRINT HEADER --------------------------------C

	CALL Putline_NF (contr, ' ')
c	CALL DATE_TIME (sys_time)
	WRITE (UNIT= bufno, FMT= '(i2)') buf_no
	response = 'Tbuffer '// bufno // ':  '// tbuf%com
	CALL Putline_NT (contr, response)
	response = ' '
	CALL Putline_NF (contr, response)
	st7(1) = PutStr (tbuf%ec)
	st7(2) = PutStr (tbuf%es)
	response = 'E-center = '// st7(1) //'  E-step = '// st7(2)
	response = response(1:36) //'     Scan in Trash Mode'
	CALL Putline_NF (contr, response)	

	CALL Putmax(tbuf%tmp,str6(1),6,3)
	CALL Putmax(tbuf%it,str6(2),6,3)
	CALL putmax(tbuf%ter,str6(3)(1:3),3,2)
	WRITE (UNIT= str6(4)(1:3), FMT= '(i3)') NINT(tbuf%tw)
	call putmax(tbuf%hf,str10,10,4)
c	WRITE (UNIT= out(1:8), FMT= '(f8.2)') tbuf%hf
	response = 'Temp  = '// str6(1) //'  Inc-T= '// str6(2) //
     	1          '  T-Err= '// str6(3)(1:3) //'   T-wait= '// str6(4)(1:3) 
	2        //' min  H-Field= '// str10
	CALL Putline_NF (contr, response)
	IF (tbuf%hf .NE. 0 .OR. tbuf%ihf .NE. 0) THEN !magnetic field 
	  call putmax(tbuf%hf,str12(1),12,4)
	  call putmax(tbuf%ihf,str12(2),12,4)
	  write(unit=str12(3),fmt='(I12)',iostat=ios)NINT(tbuf%hfw)
c	  call putmax(tbuf%hfw,str12(3),12,4)
	  call putmax(hfconv,str12(4),12,4)
	  response(1:)='HField= '//str12(1)//' IncH= '//str12(2)//
     1	    ' Hwait= '//
     1	     str12(3)//' Hconv= '//str12(4)
	  CALL Putline_NF (contr, response)
	ENDIF !magnetic field

	WRITE (UNIT= str6(1), FMT= '(i6)') NINT(tbuf%mon)
	WRITE (UNIT= str6(2)(1:3), FMT= '(i3)') NINT(tbuf%mpf)
	WRITE (UNIT= str6(3)(1:3), FMT= '(i3)') NINT(tbuf%pts)
	response = 'Monitor= '// str6(1) //'  Prefac= '// str6(2)(1:3)
     1         //'  Type= '// tbuf%mt //'  #pts = '// str6(3)(1:3)
	CALL Putline_NF (contr, response)



C---------------------------- PRINT DATA HEADER ------------------------------C

	response = ' '
	m = 1
	DO j = 1, 3
	   IF (motfl(j)) THEN
	      WRITE (UNIT= response(m+1:m+9), FMT= '(a5,i1,a3)') 
     1        '    A',j,'   '  
	      m = m + 9
	   END IF
	END DO
	response(1:) = response(1:m) //'     E '
	CALL Putline_NF (contr, response)
	CALL Putline_NF (contr, ' ')
	


C-------- CALCULATE STARTING VALUES FOR ENERGY AND RECIPROCAL SPACE ----------C

	ef = 1.2
	i = NINT(tbuf%pts)
	call FANSVEC(tbuf%ec,i,tbuf%es,energyvec(1))
	tbuf%pts = i

C-----------------------------------------------------------------------------C
C--------------------- COUNT-LOOP OVER NUMBER OF POINTS ----------------------C
C-----------------------------------------------------------------------------C

	limit=.false.
	DO j = 1,6
	  llimit(j) = .false.
	ENDDO

	DO loop = 0, NINT(tbuf%pts) - 1


!          C------------ CALCULATE ENERGY-TRANSFER AND ------------------------C
!          C--------- CORRESPONDING MONO AND ANA ANGLES -----------------------C
	   energy = energyvec(loop + 1)
	   posit(1) = Energy_Angle (ef+energy, dm)
	   posit(2) = 2. * posit(1)
	   posit(5) = Energy_Angle (ef, da)
           posit(3) = ATAND(dm * SIND(posit(1)) / (da * SIND(posit(5))))  

          
!          C-- PRINT OUT THE NEW MOTOR POSITION ------------------------------C

	   response(1:) = ' '
	   m = 1
	   DO j = 1, 3                          ! (* print new position first *)
	      mlimit(j)=.false.
	      IF (motfl(j)) THEN
		 WRITE (UNIT= response(m+1:m+9), FMT='(f9.3)') posit(j)
	         IF (nsta.eq.4 .AND. z(j).eq.-99.)THEN !special flag
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
	           llimit(j) = .true.
	           response(m+10:m+11)= '* '
	         endif
		 m = m + 11
	      END IF
  	   END DO	   
	   WRITE (UNIT= response(m+1:), FMT= '(f9.3,a4)') energy, '    '
	   CALL Putline_NF (contr, response)
	END DO  ! (* Number of total number of points *)
	if (.not.limit) then !no limits exceeded
	  response(1:) = 'Above angles did not exceed limits.'
	else
	  response(1:46)='WARNING: Angle limits exceeded in motors(s):  '
	  k=46
	  DO j=1,3
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
	IF (tbuf%tmp .NE. 0 .AND. tbuf%it .NE. 0)THEN
	  CALL Putmax(tbuf%tmp,str6(1),6,3)
     	  tmax = tbuf%tmp + (tbuf%pts-1)*tbuf%it
	  CALL Putmax(tmax,str6(2),6,3)
	  CALL Putline_NT(contr,'Temp range: '//str6(1)//' to '//str6(2))
	  CALL Putline_NF (contr, ' ')
	ENDIF
	IF (tbuf%ihf .NE. 0 )THEN
	  call putmax(tbuf%hf,str12(1),12,4)
	  hmax = tbuf%hf + (tbuf%pts-1)*tbuf%ihf
	  call putmax(hmax,str12(2),12,4)
	  CALL Putline_NT(contr,'Hfield range: '//str12(1)//' to '//str12(2))
	  IF (magnctr)THEN
	    fmax = fieldmax * hfconv
	    IF (abs(hmax) .GT. fmax .OR. abs(tbuf%hf) .GT. fmax)
     1		    CALL Putline_NT (contr, 'WARNING!!! Field out of range ')
	  ELSE
	    CALL Putline_NF(contr,' Hfield bounds not checked unless H+')
	  ENDIF
	  CALL Putline_NF (contr, ' ')
	ENDIF
	
	END
 
