#include "icp.h"
	SUBROUTINE Dry_B_Buf (buf_no)
C	*****************************

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C                                                                             C
C	Executes Bragg Buffer No. buf_no in "dry form"                        C
C	Called from: Parse_Contr                                              C
C                                                                             C
C	Gayle Greene, 1992                                                    C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
	use instr
	use smg_prep
	use tc
	use globals

	IMPLICIT      none
	INTEGER*4     i, j, k, prf, loop, buf_no, ios, iduma(10)
	REAL*4        af(10), posit(10), tmax, hmax, fmax, zj, Fz, bpos(10)
	CHARACTER*2   bufno
	CHARACTER*6   str6(4)
	CHARACTER*7   str7(3), PutStr
	CHARACTER*8   out,new_line/'        '/
	CHARACTER*10   str10
	CHARACTER*12   str12(4)
c	CHARACTER*28  sys_time
	CHARACTER*80  response, help_line(3)
	LOGICAL	      limit,mlimit(10),llimit(10), inlimits, comerr

	TYPE(b_buf) bbuf

	OPEN (UNIT= 1, FILE= 'BBUFFER.BUF', STATUS= 'OLD', ACCESS= 'DIRECT', 
	1    FORM= 'UNFORMATTED', RECL= BBUF_RECL)
	READ (1,REC=buf_no) bbuf
C	Recalculate motor angles since they can change with rdangl93)

        CALL Bragg_Pos(bbuf,bpos,iduma,iduma,comerr,.false.)
	IF (comerr)THEN
	   CALL Putcontr_NT('Q-vector not in scattering plane. Buffer aborted')
	   CLOSE(1)
	   RETURN
	ENDIF
        DO i=1,2
	   bbuf%a(i) = bpos(i)
        ENDDO
        DO i=5,6
	   bbuf%a(i) = bpos(i)
        ENDDO
	bbuf%a(3) = bpos(3) - int(bbuf%pts/2.)*bbuf%i(3)
	bbuf%a(4) = bpos(4) - int(bbuf%pts/2.)*bbuf%i(4)
	WRITE (1,REC=buf_no) bbuf
	CLOSE(1)
	lasta = bbuf%lat_a	!memorize last lattice constants
	lastb = bbuf%b
	lastc = bbuf%c
	lastef = bbuf%ef


C-------------------------------PRINT HEADER --------------------------------C

	CALL Putline_NF (contr, ' ')
	WRITE (UNIT= bufno, FMT= '(i2)') buf_no
	response = 'BBuffer '// bufno // ':  '// bbuf%com
	CALL Putline_NT (contr, response)
	CALL Putline_NF (contr, ' ')
	str7(1) = PutStr (bbuf%ec)
	str7(2) = PutStr (bbuf%es)
	str7(3) = PutStr (bbuf%ef)
	response = 'E-center = '// str7(1) //'  E-step = '// str7(2)
	IF (bbuf%eft .EQ. 'ANA') THEN
	   response = response(1:36) //'     Analyzer fixed at '//
	1	str7(3) //' meV'
	ELSE
	   response = response(1:36) //'     Monochromator fixed at '//
	1	str7(3)//' meV'	
	END IF
	CALL Putline_NF (contr, response)	
	WRITE (UNIT= str6(1), FMT= '(f6.3)') bbuf%hc
	WRITE (UNIT= str6(2), FMT= '(f6.3)') bbuf%kc
	WRITE (UNIT= str6(3), FMT= '(f6.3)') bbuf%lc   
	response = 'Q-center = ('// str6(1) //','// str6(2) //','//str6(3)//')'
	WRITE (UNIT= str6(1), FMT= '(f6.4)') bbuf%hs
	WRITE (UNIT= str6(2), FMT= '(f6.4)') bbuf%ks
	WRITE (UNIT= str6(3), FMT= '(f6.4)') bbuf%ls   
	response = response(1:33) //'      Q-step = ('// str6(1) //','//
	1    str6(2)//','// str6(3) //')'
	CALL Putline_NF (contr, response) 
	CALL Putline_NF (contr, new_line)
	DO j= 1, 6
	   IF (motfl(j)) THEN
	      af(j) = bbuf%a(j) + (bbuf%pts - 1) * bbuf%i(j)
	      str7(1) = PutStr (bbuf%a(j))
	      str7(2) = PutStr (bbuf%i(j))
	      str7(3) = PutStr (af(j))
	      WRITE (UNIT= out(1:1), FMT= '(i1)') j
	      response = 'A'// out(1:1) //'-beg= '// str7(1) //'  Inc-'// 
	1	   out(1:1) //'= '// str7(2) //'  A'// out(1:1) //
	1	   '-end= '// str7(3)
	      CALL Putline_NF (contr, response)
	   END IF
	END DO

	CALL Putmax(bbuf%tmp,str6(1),6,3)
	CALL Putmax(bbuf%it,str6(2),6,3)
	CALL putmax(bbuf%ter,str6(3)(1:3),3,2)
	WRITE (UNIT= str6(4)(1:3), FMT= '(i3)') NINT(bbuf%tw)
	call putmax(bbuf%hf,str10,10,4)
	response = 'Temp  = '// str6(1) //'  Inc-T= '// str6(2) //
	1    '  T-Err= '// str6(3)(1:3) //'   T-wait= '//
	1    str6(4)(1:3)//' min  H-Field= '// str10
	CALL Putline_NF (contr, response)
	IF (bbuf%hf .NE. 0 .OR. bbuf%ihf .NE. 0) THEN !magnetic field 
	   call putmax(bbuf%hf,str12(1),12,4)
	   call putmax(bbuf%ihf,str12(2),12,4)
	   write(unit=str12(3),fmt='(I12)',iostat=ios)NINT(bbuf%hfw)
c       call putmax(bbuf%hfw,str12(3),12,4)
	   call putmax(hfconv,str12(4),12,4)
	   response(1:)='HField= '//str12(1)//' IncH= '//str12(2)//
	1	' Hwait= '//str12(3)//' Hconv= '//str12(4)
	   CALL Putline_NF (contr, response)
	ENDIF			!magnetic field

	WRITE (UNIT= str7(1), FMT= '(i7)') NINT(bbuf%mon)
	WRITE (UNIT= out(1:4),     FMT= '(i4)') NINT(bbuf%mpf)
	response = 'Monitor= '// str7(1) //'    Prefac= '// out(1:4)
	1    //'    Type= '// bbuf%mt 
	CALL Putline_NF (contr, response)
	CALL Putline_NF (contr, ' ')


C----------------------------PRINT ANGLE HEADER -----------------------------C

	response(1:) = ' '
	k = 1 
	DO j= 1, 6
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

	DO loop = 0, NINT(bbuf%pts) - 1

!       C------------------ PRINT MOTOR POSITIONS ----------------------------C

	   response(1:1) = ' '
	   k = 1
	   DO j = 1, 6		! (* print new position first *)
	      mlimit(j)=.false.
	      IF (motfl(j)) THEN
	         posit(j) = bbuf%a(j) + bbuf%i(j) * loop 
	         WRITE (UNIT= out(1:8), FMT='(f8.3)') posit(j)
	         IF (nsta.eq.4 .AND. z(j).eq.-99.)THEN !special flag
		    zj = Fz(j,posit(j))
	         ELSE 
		    zj = z(j)
	         ENDIF
		 IF (.NOT. inlimits(j, posit(j)+zj,contr)) THEN
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

	IF (bbuf%tmp .NE. 0 .AND. bbuf%it .NE. 0)THEN
	   CALL Putmax(bbuf%tmp,str6(1),6,3)
	   tmax = bbuf%tmp + (bbuf%pts-1)*bbuf%it
	   CALL Putmax(tmax,str6(2),6,3)
	   CALL Putline_NT(contr,'Temp range: '//str6(1)//' to '//str6(2))
	   CALL Putline_NF (contr, ' ')
	ENDIF
	IF (bbuf%ihf .NE. 0 )THEN
	   call putmax(bbuf%hf,str12(1),12,4)
	   hmax = bbuf%hf + (bbuf%pts-1)*bbuf%ihf
	   call putmax(hmax,str12(2),12,4)
	   CALL Putline_NT(contr,'Hfield range: '//str12(1)//' to '//str12(2))
	   IF (magnctr)THEN
	      fmax = fieldmax * hfconv
	      IF (abs(hmax) .GT. fmax .OR. abs(bbuf%hf) .GT. fmax)
	1	   CALL Putline_NT (contr, 'WARNING !!! Field out of range ')
	   ELSE
	      CALL Putline_NF(contr,' Hfield bounds not checked unless H+')
	   ENDIF
	   CALL Putline_NF (contr, ' ')
	ENDIF


	END
