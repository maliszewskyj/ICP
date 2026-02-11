#include "icp.h"
	SUBROUTINE Dry_X_Buf (buf_no)
C	*****************************

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C                                                                             C
C	Executes X Buffer No. buf_no in "dry form"                    C
C	Called from: Parse_Contr                                              C
C                                                                             C
C	Gayle Greene, March 1996                                              C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
	use instr
	use smg_prep
	use tc
	use dartz
	IMPLICIT      none

	INTEGER*4     i, j, k, prf, loop, buf_no, ios, jr
	REAL*4        af(10), posit(10), tmax, hmax, fmax, zj, Fz
	CHARACTER*2   bufno
	CHARACTER*6   str6(4)
	CHARACTER*7   str7(3), PutStr
	CHARACTER*8   out
	CHARACTER*10   str10
	CHARACTER*12   str12(4)
	CHARACTER*28  sys_time
	CHARACTER*80  response, help_line(3)

	INTEGER*4	curpoint, curstep(maxloops), steps_below(maxloops), 
     1			numsteps(maxloops), lastloop
	CHARACTER*1	str1
	LOGICAL		getnew, newpos(maxloops)

	LOGICAL		posout(MAXMOTS)
	LOGICAL	      limit,mlimit(numxmots),llimit(numxmots), inlimits

	TYPE(x_buf) xbuf

	OPEN (UNIT= 1, FILE= 'XBUFFER.BUF', STATUS= 'OLD', ACCESS= 'DIRECT', 
	1    FORM= 'UNFORMATTED', RECL= xbuf_RECL)
	READ (1,REC=buf_no) xbuf
	CLOSE (1)



C-------------------------------PRINT HEADER --------------------------------C

	CALL Putline_NF (contr, ' ')
	CALL DATE_TIME (sys_time)
	WRITE (UNIT= bufno, FMT= '(i2)') buf_no
	response = sys_time(1:17) //' xbuffer '// bufno // ':  '// xbuf%com
	CALL Putline_NT (contr, response)
	CALL Putline_NF (contr, ' ')
	DO j = 1,MAXMOTS
	   posout(j) = .FALSE.	!position has not been written
	ENDDO
	DO j= 1, numxmots 
	   jr = xbuf%mn(j)
	   IF (jr .NE. 0)THEN	!a motor number has been supplied
	      IF (motfl(jr))THEN
		 af(j) = xbuf%a(j) + (xbuf%lpts(j) - 1) * xbuf%i(j)
		 str7(1) = PutStr (xbuf%a(j))
		 str7(2) = PutStr (xbuf%i(j))
		 str7(3) = PutStr (af(j))
		 write(unit=str1,fmt='(I1)',IOSTAT=ios)xbuf%lpn(j)
		 WRITE (UNIT= out(1:2), FMT= '(i2)') jr
		 response = 'A'// out(1:2) //'-beg= '// str7(1) //'  Inc-'// 
	1	      out(1:2) //'= '// str7(2) //'  A'// out(1:2) //
	1	      '-end= '// str7(3)//' Loop #'//str1
		 CALL Putline_NF  (contr, response)
		 posout(jr) = .TRUE.
	      END IF
	   ENDIF		!jr ne 0
	END DO
	CALL Putmax(xbuf%tmp,str6(1),6,3)
	CALL Putmax(xbuf%it,str6(2),6,3)
	CALL putmax(xbuf%ter,str6(3)(1:3),3,2)
	WRITE (UNIT= str6(4)(1:3), FMT= '(i3)') NINT(xbuf%tw)
	call putmax(xbuf%hf,str10,10,4)
c	WRITE (UNIT= out(1:8), FMT= '(f8.2)') xbuf%hf
	response = 'Temp  = '// str6(1) //'  Inc-T= '// str6(2) //
	1    '  T-Err= '// str6(3)(1:3) //'   T-wait= '// str6(4)(1:3) 
	2    //' min  H-Field= '// str10
	CALL Putline_NF  (contr, response)
	IF (xbuf%hf .NE. 0 .OR. xbuf%ihf .NE. 0) THEN !magnetic field 
	   call putmax(xbuf%hf,str12(1),12,4)
	   call putmax(xbuf%ihf,str12(2),12,4)
	   write(unit=str12(3),fmt='(I12)',iostat=ios)NINT(xbuf%hfw)
c       call putmax(xbuf%hfw,str12(3),12,4)
	   call putmax(hfconv,str12(4),12,4)
	   response(1:)='HField= '//str12(1)//' IncH= '//str12(2)//
	1	' Hwait= '//
	2	str12(3)//' Hconv= '//str12(4)
	   CALL Putline_NF  (contr, response)
	ENDIF			!magnetic field

	WRITE (UNIT= str7(1), FMT= '(i7)') NINT(xbuf%mon)
	WRITE (UNIT= out(1:4),     FMT= '(i4)') NINT(xbuf%mpf)
	response = 'Monitor= '// str7(1) //'    Prefac= '// out(1:4)
	1    //'    Type= '// xbuf%mt 
	CALL Putline_NF  (contr, response)
	CALL Putline_NF (contr, ' ')


C----------------------------PRINT ANGLE HEADER -----------------------------C

	response(1:) = ' '
	k = 1 
	DO j= 1, numxmots
	   jr = xbuf%mn(j)
	   IF (jr .NE. 0)THEN
	      IF (motfl(jr))THEN
		 WRITE (UNIT= out(1:2), FMT='(i2)') jr
		 response(1:k+8) = response(1:k) //'   A'// out(1:2) //'  '
		 k = k + 8
	      END IF
	   ENDIF		!jr ne 0
	END DO
	CALL Putline_NF  (contr, response)
	CALL Putline_NF (contr, ' ')


C-------------Initialize loop variables --------------------------------------C
	lastloop = 1
	DO k = 1,maxloops
	   numsteps(xbuf%lpn(k)) = xbuf%lpts(k) !memorize no steps for each loop
	   curstep(k) = 0	!start at first step within each loop
	   IF (xbuf%lpn(k) .GT. lastloop)lastloop = xbuf%lpn(k) !find lowest loop
	   steps_below(k) = 0
	ENDDO
	DO k = 1, lastloop - 1
	   steps_below(k) = 1
	   do j = k+1, lastloop
	      steps_below(k) = steps_below(k) * numsteps(j)
	   enddo
	ENDDO

	
C-----------------------------------------------------------------------------C
C---------------------COUNT-LOOP OVER NUMBER OF POINTS ----------------------C
C-----------------------------------------------------------------------------C

	limit=.false.
	DO j = 1,numxmots
	   llimit(j) = .false.
	ENDDO


	DO curpoint = 0, NINT(xbuf%pts) - 1

!       C-------------- Calculate new motor positions as necessary

	   do i = 1, maxloops
	      newpos(i) = .false. !default is motors  in this loop are not moved
	   enddo

	   do i = 1, lastloop	!check each loop to see if its motors to move
	      IF (curpoint .EQ. 0)THEN !all motors are moved at point # 0
		 newpos(lastloop) = .TRUE.
	      ELSEIF (i .EQ. lastloop)THEN !last loop motors are always moved
		 newpos(i) = .TRUE.
	      ELSEIF ( MOD(curpoint+1,steps_below(i)) .EQ. 0)THEN
		 newpos(i) = .TRUE.
	      ENDIF
	   enddo

!       C------------------ PRINT MOTOR POSITIONS ----------------------------C

	   response(1:1) = ' '
	   k = 1

	   DO j = 1, numxmots
	      mlimit(j)=.false.
	      jr = xbuf%mn(j)
	      IF (jr .NE. 0)THEN
		 IF (motfl(jr))THEN
		    posit(j) = xbuf%a(j) + xbuf%i(j) * curstep(xbuf%lpn(j))
		    WRITE (UNIT= out(1:8), FMT='(f8.3)') posit(j)
		    IF (.NOT. inlimits(jr, posit(j)+z(jr),contr))THEN
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
	      ENDIF		!jr ne 0
	   END DO
	   CALL Putline_NF  (contr, response(1:k))

	   DO i = 1, maxloops	!increment step number in each loop for which motors moved
	      IF (newpos(i))THEN !all motors in this loop went to new positions
		 curstep(i) = curstep(i) + 1
		 IF ( curstep(i) .GE. numsteps(i) )curstep(i) = 0
	      ENDIF
	   ENDDO

	END DO			! (* Number of points loop *)
	if (.not.limit) then	!no limits exceeded
	   response(1:) = 'Above angles did not exceed limits.'
	else
	   response(1:46)='WARNING: Angle limits exceeded in motors(s):  '
	   k=46
	   DO j=1,numxmots
	      IF (llimit(j))then
		 jr = xbuf%mn(j)
		 write(unit=out(1:2),fmt='(I2)')jr
		 response(1:)=response(1:k)//out(1:2)//' '
		 k=k+3
	      ENDIF
	   ENDDO
	endif
	CALL Putline_NT (contr, response)
	response(1:) = 'Angles are given in software values!'
	CALL Putline_NF  (contr, response)
	CALL Putline_NF (contr, ' ')
	IF (xbuf%tmp .NE. 0 .AND. xbuf%it .NE. 0)THEN
	   CALL Putmax(xbuf%tmp,str6(1),6,3)
	   tmax = xbuf%tmp + (xbuf%pts-1)*xbuf%it
	   CALL Putmax(tmax,str6(2),6,3)
	   CALL Putline_NT(contr,'Temp range: '//str6(1)//' to '//str6(2))
	   CALL Putline_NF (contr, ' ')
	ENDIF
	IF (abs(xbuf%ihf) .GT. 0.000000001 )THEN
	   call putmax(xbuf%hf,str12(1),12,4)
	   hmax = xbuf%hf + (xbuf%pts-1)*xbuf%ihf
	   call putmax(hmax,str12(2),12,4)
	   CALL Putline_NT(contr,'Hfield range: '//str12(1)//' to '//str12(2))
	   IF (magnctr)THEN
	      fmax = fieldmax * hfconv
	      IF (abs(hmax) .GT. fmax .OR. abs(xbuf%hf) .GT. fmax)
	1	   CALL Putline_NT (contr, 'WARNING !!! Field out of range ')
	   ELSE
	      CALL Putline_NF(contr,' Hfield bounds not checked unless H+')
	   ENDIF
	   CALL Putline_NF (contr, ' ')
	ENDIF

	END
