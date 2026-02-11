#include "icp.h"
	SUBROUTINE Dry_R_Buf (buf_no)
C	*****************************

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C                                                                             C
C     Tests motor positions of Refl Buffer No. buf_no                         C
C     Called from: Parse_Contr                                                C
C                                                                             C
C     Gayle Greene, Feb   1993                                                C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
	use instr
	use refl
	use smg_prep
	use tc
	IMPLICIT      none
	INTEGER*4     i, j, m, status, st, typ, ver, kj
	INTEGER*4     prf, loop, buf_no, state, ios
	REAL*4        qxnow,qznow,alphai,alphaf,hmax,fmax,tmax
	REAL*4	      tmat(3,3), q0(3), q(3), v1(3), v2(3)
	REAL*4        elap_time, dsec, temp /0./, dummy, t, af
	LOGICAL       limit,varimon,inlimits, Readef
	CHARACTER*2   str2(3), bufno
	CHARACTER*6   str6(4)
	CHARACTER*7   st7(3), PutStr
	CHARACTER*8   out
	CHARACTER*10  str10(3)
	CHARACTER*12  str12(4)
	CHARACTER*28  sys_time
	CHARACTER*80  response, error
	INTEGER*4     tomove,motlist(10)

	REAL*4   posit(MAXMOTS), minpos(MAXMOTS),maxpos(MAXMOTS),slitpos(4)
	REAL*4   minhdw,maxhdw
	INTEGER*4  slitname(4)/1,2,3,4/,k 
	LOGICAL  mlimit(MAXMOTS)
   
	TYPE(r_buf) rbuf

	OPEN (UNIT= 1, FILE= 'RBUFFER.BUF', STATUS= 'OLD', ACCESS= 'DIRECT', 
	1    FORM= 'UNFORMATTED', RECL=RBUF_RECL)
	READ (1,REC=buf_no) rbuf
	CLOSE (1)


c       - For 2.35 Angstroms, use PG filter.  Otherwise use Be filter
	if (wavel.lt.3) then
	   tomove = PGmove
	   DO i = 1,tomove
	      motlist(i) = PGlist(i)
	   ENDDO
	   st7(3) = 'PG'
	else   
	   tomove = Bemove
	   DO i = 1,tomove
	      motlist(i) = Belist(i)
	   ENDDO
	   st7(3) = 'Be'
	endif

C-------------------------------PRINT HEADER --------------------------------C

	CALL Putline_NF (contr, ' ')
	CALL Putline_NF (contr, ' ')
	CALL DATE_TIME (sys_time)
	WRITE (UNIT= bufno(1:2), FMT= '(i2)') buf_no
	response = sys_time(1:17) //' Refl_buf '// bufno // ':  '// rbuf%com
	CALL Putline_NT (contr, response)
	response = ' '
	CALL Putline_NF (contr, response)

	write(unit=st7(1),fmt='(f7.4)')dm
	write(unit=st7(2),fmt='(f7.3)')wavel
	WRITE (UNIT= str6(2)(1:3), FMT= '(i3)') NINT(rbuf%pts)
	response(1:) = 'Filter: '//st7(3)(1:2)//'  Wavel: '//st7(2)//
	1  '  Dm: '//st7(1)//'  #pts= '// str6(2)(1:3)
	CALL Putline_NF (contr, response)

	af = rbuf%qz + (rbuf%pts - 1) * rbuf%qzi
	write (UNIT=str10(1),fmt='(F10.6)')rbuf%qz
	write (UNIT=str10(2),fmt='(F10.6)')rbuf%qzi
	write (UNIT=str10(3),fmt='(F10.6)')af
	response = 'QZ-beg= '// str10(1) //'  Inc-QZ= '// 
	1    str10(2) //'  QZ-end= '// str10(3)
	CALL Putline_NF (contr, response)

	af = rbuf%qx + (rbuf%pts - 1) * rbuf%qxi
	write (UNIT=str10(1),fmt='(F10.6)')rbuf%qx
	write (UNIT=str10(2),fmt='(F10.6)')rbuf%qxi
	write (UNIT=str10(3),fmt='(F10.6)')af
	response = 'QX-beg= '// str10(1) //'  Inc-QX= '// 
	1    str10(2) //'  QX-end= '// str10(3)
	CALL Putline_NF (contr, response)

	DO kj = 1, 4		!slit motors
	   j=slitlist(kj)
	   IF (motfl(j)) THEN
	      af = rbuf%s(kj) + (rbuf%pts - 1) * rbuf%i(kj)
	      st7(1) = PutStr (rbuf%s(kj))
	      st7(2) = PutStr (rbuf%i(kj))
	      st7(3) = PutStr (af)
	      WRITE (UNIT= out(1:1), FMT= '(i1)') kj
	      response = 'S'// out(1:1) //'-beg=  '// st7(1) //'    Inc-'// 
	1	   out(1:1) //'=   '// st7(2) //'    S'// out(1:1) //
	1	   '-end=  '// st7(3)
	      CALL Putline_NF (contr, response)
	   END IF
	ENDDO

	IF (rbuf%numx .NE. 0)THEN !Xtra motor
	   j=rbuf%numx
	   IF (motfl(j)) THEN
	      af = rbuf%sx + (rbuf%pts - 1) * rbuf%ix
	      st7(1) = PutStr (rbuf%sx)
	      st7(2) = PutStr (rbuf%ix)
	      st7(3) = PutStr (af)
	      WRITE (UNIT= out(1:2), FMT= '(i2)') j
	      response = out(1:2) //'-beg=  '// st7(1) //'    Inc-'// 
	1	   out(1:2) //'=  '// st7(2) //'    '// out(1:2) //
	1	   '-end=  '// st7(3)
	      CALL Putline_NF (contr, response)
	   END IF
	ENDIF

	CALL Putmax(rbuf%tmp,str6(1),6,3)
	CALL Putmax(rbuf%it,str6(2),6,3)
	CALL putmax(rbuf%ter,str6(3)(1:3),3,2)
	WRITE (UNIT= str6(4)(1:3), FMT= '(i3)') NINT(rbuf%tw)
	call putmax(rbuf%hf,str10(1),10,4)
c	WRITE (UNIT= out(1:8), FMT= '(f8.2)') rbuf%hf
	response = 'Temp  = '// str6(1) //'  Inc-T= '// str6(2) //
	1    '  T-Err= '// str6(3)(1:3) //'   T-wait= '// str6(4)(1:3) 
	2    //' min  H-Field= '// str10(1)
	CALL Putline_NF (contr, response)
	IF (rbuf%hf .NE. 0 .OR. rbuf%ihf .NE. 0) THEN !magnetic field 
	   call putmax(rbuf%hf,str12(1),12,4)
	   call putmax(rbuf%ihf,str12(2),12,4)
	   write(unit=str12(3),fmt='(I12)',iostat=ios)NINT(rbuf%hfw)
C       call putmax(rbuf%hfw,str12(3),12,4)
	   call putmax(hfconv,str12(4),12,4)
	   response(1:)='HField= '//str12(1)//' IncH= '//str12(2)//
	1	' Hwait= '//
	2	str12(3)//' Hconv= '//str12(4)
	   CALL Putline_NF (contr, response)
	ENDIF			!magnetic field

	WRITE (UNIT= out, FMT= '(i8)') NINT(rbuf%mon0)
	WRITE (UNIT= str6(1)(1:3), FMT= '(i3)') NINT(rbuf%mpf)
	response(1:) = 'Prefac= '//str6(1)(1:3)//'  Mon0='//out
	j = 27
	IF (rbuf%mon1 .ne. 0) THEN
	   write(unit= out, fmt= '(i8)')NINT(rbuf%mon1)
	   write(unit= str6(1)(1:6),fmt='(f6.2)')rbuf%exp
	   response(1:)=response(1:j)//'  Mon1= '//out//'  Exp= '//str6(1)
	   j = j + 29
	   varimon = .true.	!variable count time or neutrons
	ELSE
	   varimon = .false.
	ENDIF
	response(1:)=response(1:j)//'  Type= '//rbuf%mt
	j = j + 13
	WRITE (UNIT= str6(1)(1:5), FMT= '(f5.3)') eff(1)
	response(1:)=response(1:j)//' SF= '//str6(1)(1:5)
	CALL Putline_NF (contr, response)

	IF (polarized) THEN
	   CALL Putline_NF (contr, 'IF polarization analysis enabled, then ')
	   CALL Putline_NF (contr, 'the following cross section(s) will be taken:')
	   DO j = 1, 4
	      IF (rbuf%flip_state(j)) THEN
	         response(1:) = 'Flipper 1: '// flp(j,1) // '  Flipper 2: ' 
	1	      // flp(j,2) 
                 CALL Putline_NF (contr, response)
	      END IF
	   END DO
	ENDIF

C------------------PRINT CURRENT POSITION OF FIXED MOTORS -------------------C

	response(1:) = 'Fixed motors: '
	m = 14
	DO j= 1, MAXMOTS
	   IF (motfl(j) .AND. fixed(j)) THEN
	      WRITE (UNIT= out(1:2), FMT='(i2)') j
	      response(1:) = response(1:m) //'  A'// out(1:2) 
	      m = m + 5
	   END IF
	   IF (m .gt. 70) THEN
	      CALL Putline_NF (contr, response)
	      response(1:)=' '
	      m = 14
	   ENDIF
	END DO
	IF (m .GT. 15)CALL Putline_NF (contr, response) 
	CALL Putline_NF (contr, ' ')
	

C-----------------------------------------------------------------------------C
C---------------------LOOP OVER NUMBER OF POINTS ----------------------------C
C-----------------------------------------------------------------------------C
	CALL Putline_NF(contr,'Calculating positions of each motor...')

	DO loop = 0, NINT(rbuf%pts) - 1


C       Calculate angles of motors associated with qz and qx
           qznow = rbuf%qz + rbuf%qzi * loop
           qxnow = rbuf%qx + rbuf%qxi * loop 
	   Call refl_pos(qznow,qxnow,alphai,alphaf,posit) !Puts positions in array posit
	   IF (varimon .and. qznow .eq. 0 .and. rbuf%exp .le. 0) THEN
	      write(unit= str6(1)(1:6),fmt='(f6.2)')rbuf%exp
	      write(unit= str6(2)(1:6),fmt='(I6)')loop + 1
	      response(1:)=' *** Warning: Point #'//str6(2)//
	1	   ' skipped because qz = 0. and exp = '//str6(1)//' ***'
	      CALL Putline_NT(contr,response)
	      GOTO 999		!GO TO NEXT POINT
	   ENDIF

C       Get max and min positions of each motors
	   
	   DO kj = 1, tomove	!check all motors to be moved
	      j = motlist(kj)
	      t = posit(j)
	      IF (loop .eq. 0) THEN !first position
		 minpos(j) = t
		 maxpos(j) = t
	      ELSE		!not first position
		 IF (t .lt. minpos(j))minpos(j) = t
		 IF (t .gt. maxpos(j))maxpos(j) = t
	      ENDIF
	   END DO
	   DO kj = 1, 4		!slit motors
	      j=slitlist(kj)
	      posit(j) = rbuf%s(kj) + rbuf%i(kj) * loop
	      t = posit(j)
	      IF (loop .eq. 0) THEN !first position
		 minpos(j) = t
		 maxpos(j) = t
	      ELSE		!not first position
		 IF (t .lt. minpos(j))minpos(j) = t
		 IF (t .gt. maxpos(j))maxpos(j) = t
	      ENDIF
	   ENDDO
	   IF (rbuf%numx .NE. 0) THEN
	      posit(rbuf%numx) = rbuf%sx + 
	1	   rbuf%ix * loop
	      t = posit(rbuf%numx)
	      IF (loop .eq. 0) THEN !first position
		 minpos(rbuf%numx) = t
		 maxpos(rbuf%numx) = t
	      ELSE		!not first position
		 IF (t .lt. minpos(rbuf%numx))minpos(rbuf%numx) = t
		 IF (t .gt. maxpos(rbuf%numx))maxpos(rbuf%numx) = t
	      ENDIF
	   ENDIF

	   IF (READEF(contr_A))THEN         
	      CALL Putline_NT (contr, ' **** ABORT detected ****')
	      RETURN
	   END IF

 999	   continue		!this is an error branch. Will go to next point.

	END DO			! (* Number of total number of points *)

C       Now type out minimum & maximum positions and compare to limits
	limit = .false.
	DO j = 1,MAXMOTS
	   mlimit(j) = .false.
	ENDDO
	
	DO kj = 1, tomove	!check all motors to be moved
	   j = motlist(kj)
	   IF (motfl(j))THEN	!only check if motor is in use
	      minhdw = minpos(j) + z(j)
	      maxhdw = maxhdw + z(j)
	      IF (.NOT. inlimits(j,minhdw,prpr) .OR.
	1	   .NOT. inlimits(j,maxhdw,prpr))THEN
		 limit = .true.
		 mlimit(j) = .true.
	      ENDIF
	      response(1:)='A00: '
	      write(response(2:3),fmt='(i2.2)')j
	      write(response(10:20),fmt='(f10.4)')minpos(j)
	      write(response(30:40),fmt='(f10.4)')maxpos(j)
	      if (mlimit(j))response(50:)='**** Limit exceeded ****'
	      CALL Putline_NF(contr,response)
	   ENDIF
	ENDDO
	DO kj = 1, 4		!slit motors
	   j=slitlist(kj)
	   IF (motfl(j))THEN	!only check if motor is in use
	      minhdw = minpos(j) + z(j)
	      maxhdw = maxhdw + z(j)
	      IF (.NOT. inlimits(j,minhdw,prpr) .OR.
	1	   .NOT. inlimits(j,maxhdw,prpr))THEN
		 limit = .true.
		 mlimit(j) = .true.
	      ENDIF
	      response(1:)='A00 S00:'
	      write(response(2:3),fmt='(i2.2)')j
	      write(response(6:7),fmt='(i2.2)')kj
	      write(response(10:20),fmt='(f10.4)')minpos(j)
	      write(response(30:40),fmt='(f10.4)')maxpos(j)
	      if (mlimit(j))response(50:)='**** Limit exceeded ****'
	      CALL Putline_NF(contr,response)
	   ENDIF
	ENDDO
	IF (rbuf%numx .NE. 0) THEN
	   j = rbuf%numx
	   IF (motfl(j))THEN	!only check if motor is in use
	      minhdw = minpos(j) + z(j)
	      maxhdw = maxhdw + z(j)
	      IF (.NOT. inlimits(j,minhdw,prpr) .OR.
	1	   .NOT. inlimits(j,maxhdw,prpr))THEN
		 limit = .true.
		 mlimit(j) = .true.
	      ENDIF
	      response(1:)='A00: '
	      write(response(2:3),fmt='(i2.2)')j
	      write(response(10:20),fmt='(f10.4)')minpos(j)
	      write(response(30:40),fmt='(f10.4)')maxpos(j)
	      if (mlimit(j))response(50:)='**** Limit exceeded ****'
	      CALL Putline_NF(contr,response)
	   ENDIF
	ENDIF

C       Summarize		
	if (.not.limit) then	!no limits exceeded
	   response(1:) = 'Above angles did not exceed limits.'
	else
	   response(1:46)='WARNING: Angle limits exceeded in motors(s):  '
	   k=46
	   DO j=1,MAXMOTS
	      IF (mlimit(j))then
		 write(unit=out(1:2),fmt='(I2)')j
		 response(1:)=response(1:k)//out(1:2)//' '
		 k=k+3
	      ENDIF
	      IF (k .gt. 70) THEN
		 CALL Putline_NT (contr, response)
		 response(1:)= ' ' 
		 k=10
	      ENDIF
	   ENDDO
	endif
	CALL Putline_NT (contr, response)
	response(1:) = 'Angles are given in software values!'
	CALL Putline_NF (contr, response)
	CALL Putline_NF (contr, ' ')
	IF (rbuf%tmp .NE. 0 .AND. rbuf%it .NE. 0)THEN
	   CALL Putmax(rbuf%tmp,str6(1),6,3)
	   tmax = rbuf%tmp + (rbuf%pts-1)*rbuf%it
	   CALL Putmax(tmax,str6(2),6,3)
	   CALL Putline_NT(contr,'Temp range: '//str6(1)//' to '//str6(2))
	   CALL Putline_NF (contr, ' ')
	ENDIF
	IF (rbuf%ihf .NE. 0 )THEN
	   call putmax(rbuf%hf,str12(1),12,4)
	   hmax = rbuf%hf + (rbuf%pts-1)*rbuf%ihf
	   call putmax(hmax,str12(2),12,4)
	   CALL Putline_NT(contr,'Hfield range: '//str12(1)//' to '//str12(2))
	   IF (magnctr)THEN
	      fmax = fieldmax * hfconv
	      IF (abs(hmax) .GT. fmax .OR. abs(rbuf%hf) .GT. fmax)
	1	   CALL Putline_NT (contr, 'WARNING !!! Field out of range ')
	   ELSE
	      CALL Putline_NF(contr,' Hfield bounds not checked unless H+')
	   ENDIF
	   CALL Putline_NF (contr, ' ')
	ENDIF
	RETURN

	END
