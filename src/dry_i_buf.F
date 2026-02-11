#include "icp.h"
	SUBROUTINE Dry_I_Buf (buf_no)
C	*****************************

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C                                                                             C
C	Executes Increment Buffer No. buf_no in "dry form"                    C
C	Called from: Parse_Contr                                              C
C                                                                             C
C	Beat Schmid, June 1990                                                C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C

	use instr
	use smg_prep
	use tc
	use xray
	IMPLICIT      none
	INTEGER*4     j, k, loop, buf_no, ios, lowest, highest
	REAL*4        af(10), posit(10), tmax, hmax, fmax, zj, Fz
	CHARACTER*2   bufno
	CHARACTER*6   str6(4)
	CHARACTER*7   str7(3), PutStr
	CHARACTER*8   out
	CHARACTER*10   str10
	CHARACTER*12   str12(4)
	CHARACTER*80  response
	LOGICAL	      limit,mlimit(10),llimit(10), inlimits

	TYPE(i_buf) ibuf


	OPEN (UNIT= 1, FILE= 'IBUFFER.BUF', STATUS= 'OLD',
	1    ACCESS= 'DIRECT', FORM= 'UNFORMATTED', RECL= IBUF_RECL)
	READ (1,REC=buf_no) ibuf
	CLOSE (1)


C-------------------------- DETERMINE WHETHER MOTOR ONE IS MOVED--------------C
	if (nsta.eq.4 .or. nsta.eq.-5 .or. nsta.eq.2 .or.
     1	    nsta.eq.9 .or. nsta.eq.-1 .OR. nsta.eq.-11.or.
     1	    (nsta .EQ. 0 .AND. poly) ) THEN
	  lowest=1 !1 is moved on these instruments
	  highest = 6
	elseif (nsta .eq. 1) THEN
	  lowest = 3
	  highest = 4
	else
	  lowest = 2 !1 is not moved on most instruments
	  highest = 6
	endif

C------------------------------- PRINT HEADER --------------------------------C

	CALL Putline_NF (contr, ' ')
c	CALL DATE_TIME (sys_time)
	WRITE (UNIT= bufno, FMT= '(i2)') buf_no
	response = 'IBuffer '// bufno // ':  '// ibuf%com
	CALL Putline_NT (contr, response)
	CALL Putline_NF (contr, ' ')
	DO j= lowest, highest
	   IF (motfl(j)) THEN
	      af(j) = ibuf%a(j) + (ibuf%pts - 1) * ibuf%i(j)
	      str7(1) = PutStr (ibuf%a(j))
	      str7(2) = PutStr (ibuf%i(j))
	      str7(3) = PutStr (af(j))
	      WRITE (UNIT= out(1:1), FMT= '(i1)') j
	      response = 'A'// out(1:1) //'-beg= '// str7(1) //'  Inc-'// 
	1	   out(1:1) //'= '// str7(2) //'  A'// out(1:1) //
	1	   '-end= '// str7(3)
	      CALL Putline_NF (contr, response)
	   END IF
	END DO

	CALL Putmax(ibuf%tmp,str6(1),6,3)
	CALL Putmax(ibuf%it,str6(2),6,3)
	CALL putmax(ibuf%ter,str6(3)(1:3),3,2)
	WRITE (UNIT= str6(4)(1:3), FMT= '(i3)') NINT(ibuf%tw)
	call putmax(ibuf%hf,str10,10,4)
c	WRITE (UNIT= out(1:8), FMT= '(f8.2)') ibuf%hf
	response = 'Temp  = '// str6(1) //'  Inc-T= '// str6(2) //
	1    '  T-Err= '// str6(3)(1:3) //'   T-wait= '// str6(4)(1:3) 
	2    //' min  H-Field= '// str10
	CALL Putline_NF (contr, response)
	IF (ibuf%hf .NE. 0 .OR. ibuf%ihf .NE. 0) THEN !magnetic field 
	   call putmax(ibuf%hf,str12(1),12,4)
	   call putmax(ibuf%ihf,str12(2),12,4)
	   write(unit=str12(3),fmt='(I12)',iostat=ios)NINT(ibuf%hfw)
c       call putmax(ibuf%hfw,str12(3),12,4)
	   call putmax(hfconv,str12(4),12,4)
	   response(1:)='HField= '//str12(1)//' IncH= '//str12(2)//
	1	' Hwait= '//
	2	str12(3)//' Hconv= '//str12(4)
	   CALL Putline_NF (contr, response)
	ENDIF			!magnetic field

	WRITE (UNIT= str7(1), FMT= '(i7)') NINT(ibuf%mon)
	WRITE (UNIT= out(1:4),     FMT= '(i4)') NINT(ibuf%mpf)
	response = 'Monitor= '// str7(1) //'    Prefac= '// out(1:4)
	1    //'    Type= '// ibuf%mt 
	CALL Putline_NF (contr, response)
	CALL Putline_NF (contr, ' ')


C----------------------------PRINT ANGLE HEADER -----------------------------C

	response(1:) = ' '
	k = 1 
	DO j= lowest, highest
	   IF (motfl(j)) THEN
	      WRITE (UNIT= out(1:1), FMT='(i1)') j
	      response(1:k+8) = response(1:k) //'    A'// out(1:1) //'  '
	      k = k + 8
	   END IF
	END DO
	CALL Putline_NF (contr, response)
	CALL Putline_NF (contr, ' ')


C-----------------------------------------------------------------------------C
C---------------------COUNT-LOOP OVER NUMBER OF POINTS ----------------------C
C-----------------------------------------------------------------------------C

	limit=.false.
	DO j = 1,6
	   llimit(j) = .false.
	ENDDO

	DO loop = 0, NINT(ibuf%pts) - 1

!       C------------------ PRINT MOTOR POSITIONS ----------------------------C

	   response(1:1) = ' '
	   k = 1
	   DO j = lowest, highest ! (* print new position first *)
	      mlimit(j)=.false.
	      IF (motfl(j)) THEN
	         posit(j) = ibuf%a(j) + ibuf%i(j) * loop 
	         WRITE (UNIT= out(1:8), FMT='(f8.3)') posit(j)
	         IF (nsta.eq.4 .and. z(j).eq.-99.)THEN !special flag
		    zj = fz(j,posit(j))
	         ELSE 
		    zj = z(j)
	         ENDIF
		 IF (.NOT. inlimits(j,posit(j)+zj,contr))THEN
	            limit = .TRUE.
	            mlimit(j) = .TRUE.
	         END IF
		 if (.not.mlimit(j))then
		    response(1:)= response(1:k) // out(1:8) //'  '
	         else
		    llimit(j) = .true.
		    response(1:)= response(1:k) // out(1:8) //'* '
	         endif
	         k = k + 10
	      END IF
	   END DO
	   CALL Putline_NF (contr, response(1:k))
	END DO			! (* Number of points loop *)
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
	IF (ibuf%tmp .NE. 0 .AND. ibuf%it .NE. 0)THEN
	   CALL Putmax(ibuf%tmp,str6(1),6,3)
	   tmax = ibuf%tmp + (ibuf%pts-1)*ibuf%it
	   CALL Putmax(tmax,str6(2),6,3)
	   CALL Putline_NT(contr,'Temp range: '//str6(1)//' to '//str6(2))
	   CALL Putline_NF (contr, ' ')
	ENDIF
	IF (ibuf%ihf .NE. 0 )THEN
	   call putmax(ibuf%hf,str12(1),12,4)
	   hmax = ibuf%hf + (ibuf%pts-1)*ibuf%ihf
	   call putmax(hmax,str12(2),12,4)
	   CALL Putline_NT(contr,'Hfield range: '//str12(1)//' to '//str12(2))
	   IF (magnctr)THEN
	      fmax = fieldmax * hfconv
	      IF (abs(hmax) .GT. fmax .OR. abs(ibuf%hf) .GT. fmax)
	1	   CALL Putline_NT (contr, 'WARNING !!! Field out of range ')
	   ELSE
	      CALL Putline_NF(contr,' Hfield bounds not checked unless H+')
	   ENDIF
	   CALL Putline_NF (contr, ' ')
	ENDIF

	END
