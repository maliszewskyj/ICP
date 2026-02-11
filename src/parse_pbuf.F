#include "icp.h"
	SUBROUTINE Parse_Pbuf (command, errflag, lastchar)
C	***********************************

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C                                                                             C
C     Routine Checks Spelling And Syntax Of The Print buffer Commands. If Ok, C
C     Executes  Command And Returns to Parse_Contr.			      C
C                                                                             C
C     Gayle Greene, 1997			                              C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C

	use instr
	use smg_prep
	use xray
	IMPLICIT      none

C  Calling Parameters
	character*(*)	command
	integer*4	lastchar !at call, has iend. at return has string shift position
	logical		errflag

C  Local Variables
	integer*4  j,k,l,m, ios, iend, lowest, highest
	real*4	   af(10)
	character*80	local, response
	character*4	str4
	character*8	str8
	character*9	str9
	character*13	str13
	character*6	str6(4)
	character*7	st7(3), PutStr

	TYPE(i_buf) ibuf
	TYPE(q_buf) qbuf 
	TYPE(b_buf) bbuf 
	TYPE(t_buf) tbuf 

	iend = lastchar
	lastchar = 2		!default

	j = INDEX (command, ';') ! Print buffer
	IF ( j .EQ. 0 ) THEN	! (* we have just one command here *)
	   local(1:) = command(1:)
	ELSE			! (* there is more to come after that *)
	   local(1:j) = command(1:j-1)
	END IF
	j = INDEX (local, '-')
	IF ( j .EQ. 0 ) THEN	! (* print one buffer only *)
	   READ (UNIT= local(3:iend), FMT= *, ERR= 270) l        
	   m = l
	ELSE			! (* print range of buffers *)
	   READ (UNIT= local(3:j-1), FMT= *, ERR= 270) l        
	   READ (UNIT= local(j+1:iend),  FMT= *, ERR= 270) m
	END IF
	IF (l .GT. NBUF .OR. l .LT. 1 .OR. m .GT. NBUF .OR.
	1    m .LT. 1) GOTO 270
	str4(1:1) = local(2:2)
	IF (l .GT. m) THEN	! Exchange l & m
	   k = l
	   l = m
	   m = k
	END IF
	IF (str4(1:1) .EQ. 'I') THEN
	   if (nsta.eq.4 .or. nsta.eq.-5 .or. nsta.eq.2 .or.
	1	nsta.eq.9 .or. nsta.eq.-1 .OR.
	2	(nsta .EQ. 0 .AND. poly) ) THEN
	      lowest=1		!1 is moved on these instruments
	      highest = 6
	   elseif (nsta .eq. 1) THEN
	      lowest = 3
	      highest = 4
	   else
	      lowest = 2	!1 is not moved on most instruments
	      highest = 6
	   endif
	   OPEN (UNIT= 1, FILE= 'IBUFFER.BUF', STATUS= 'OLD', ACCESS= 
	1	'DIRECT',RECL=IBUF_RECL, FORM= 'UNFORMATTED')
	   DO k = l, m
	      CALL Putline_NT (contr, ' ')
	      READ (1,REC=k) ibuf
	      WRITE (UNIT= st7(1)(1:2), FMT= '(i2)',iostat=ios) k         
	      response = 'IBuffer No: '//st7(1)(1:2)//'  Comment: '//
	1	   ibuf%com
	      CALL Putline_NT (contr, response)            
	      DO j= lowest, highest
		 IF (motfl(j)) THEN
		    af(j) = ibuf%a(j) + (ibuf%pts - 1) * ibuf%i(j)
		    st7(1) = PutStr (ibuf%a(j))
		    st7(2) = PutStr (ibuf%i(j))
		    st7(3) = PutStr (af(j))
		    WRITE (UNIT= str4(1:1), FMT= '(i1)',iostat=ios) j
		    response = 'A'// str4(1:1) //'-beg= '// st7(1) //
	1		 '  Inc-'// str4(1:1)// '= '// st7(2) //
	2		 '  A'// str4(1:1) //'-end= '// st7(3)
		    CALL Putline_NT (contr, response)
		 END IF
	      END DO
	      CALL Putmax(ibuf%tmp,str6(1),6,3)
	      CALL Putmax(ibuf%it,str6(2),6,3)
	      CALL Putmax(ibuf%ter,str6(3)(1:3),3,2)
	      WRITE(Unit=str8(1:3),FMT='(I3)',IOSTAT=ios)NINT(ibuf%th0)
	      WRITE (UNIT= str6(4)(1:3), FMT= '(i3)',IOSTAT=ios) NINT(ibuf%tw)
	      response = 'Temp  = '// str6(1) //'  Inc-T= '// str6(2) //
	1	   '  T-Err= '// str6(3)(1:3) //'%  T-wait= '//
	1	   str6(4)(1:3)//' min  Hold 1st = '//str8(1:3)
	      CALL Putline_NT (contr, response)
	      WRITE(Unit=str6(2)(1:3),FMT='(I3)',IOSTAT=ios)NINT(ibuf%th)
	      CALL Putmax(ibuf%hf,str13(1:8),8,4)
	      CALL Putmax(ibuf%ihf,str8,8,4)
	      WRITE(UNIT=str9,FMT='(I9)',IOSTAT=ios)NINT(ibuf%hfw)
	      response = 'Hfield = '//str13(1:8)//
	1	   '  Inc-HF = '//str8//'  HF-wait: '//str9//
	2	   '     Hold all = '//str6(2)(1:3)
	      CALL Putline_NT (contr, response)
	      WRITE (UNIT= st7(1),  FMT= '(i7)',iostat=ios) NINT(ibuf%mon)
	      WRITE (UNIT= str4,    FMT= '(i4)',iostat=ios) NINT(ibuf%mpf)
	      WRITE (UNIT= str6(1), FMT= '(i6)',iostat=ios) NINT(ibuf%pts)
	      response = 'Monitor= '// st7(1) //'    Prefac= '// str4 //
	1	   '    Type= '// ibuf%mt //'  Npoints='// str6(1)
	      CALL Putline_NT (contr, response)
	   END DO
	   CLOSE (1) 
	ELSE IF (str4(1:1) .EQ. 'Q') THEN
	   IF (nsta .EQ. 1) CALL Not_Implemented (*520)
	   OPEN (UNIT= 1, FILE= 'QBUFFER.BUF', STATUS= 'OLD', 
	1	ACCESS= 'DIRECT',RECL=QBUF_RECL, FORM= 'UNFORMATTED')
	   DO k = l, m
	      CALL Putline_NT (contr, ' ')
	      READ (1,REC=k) qbuf
	      WRITE (UNIT= st7(1)(1:2), FMT= '(i2)',iostat=ios) k         
	      response = 'QBuffer No: '//st7(1)(1:2)//'  Comment: '//
	1	   qbuf%com
	      CALL Putline_NT (contr, response)            
	      CALL Putmax(qbuf%a,st7(1),7,4)
	      CALL Putmax(qbuf%b,st7(2),7,4)
	      CALL Putmax(qbuf%c,st7(3),7,4)
	      response(1:)='Lattice constants: a = '//st7(1)//
	1	   '  b = '//st7(2)//'  c = '//st7(3)
	      CALL Putline_NT (contr, response)            
	      st7(1) = PutStr (qbuf%ec)
	      st7(2) = PutStr (qbuf%es)
	      st7(3) = PutStr (qbuf%ef)
	      response = 'E-center = '// st7(1) //'  E-step = '// st7(2)
	      IF (qbuf%eft .EQ. 'ANA') THEN
		 response = response(1:36) //'     Analyzer fixed at '// 
	1	      st7(3)
	      ELSE
		 response = response(1:36) //'     Monochromator fixed at '
	1	      // st7(3)
	      END IF
	      CALL Putline_NT (contr, response)	
	      WRITE (UNIT= str6(1), FMT= '(f6.3)',iostat=ios) qbuf%hc
	      WRITE (UNIT= str6(2), FMT= '(f6.3)',iostat=ios) qbuf%kc
	      WRITE (UNIT= str6(3), FMT= '(f6.3)',iostat=ios) qbuf%lc   
	      response = 'Q-center = ('// str6(1) //','// str6(2) //','// 
	1	   str6(3) //')'
	      WRITE (UNIT= str6(1), FMT= '(f6.4)',iostat=ios) qbuf%hs
	      WRITE (UNIT= str6(2), FMT= '(f6.4)',iostat=ios) qbuf%ks
	      WRITE (UNIT= str6(3), FMT= '(f6.4)',iostat=ios) qbuf%ls   
	      response = response(1:33) //'      Q-step = ('// str6(1) //','
	1	   // str6(2) //','// str6(3) //')'
	      CALL Putline_NT (contr, response) 
	      CALL Putmax(qbuf%tmp,str6(1),6,3)
	      CALL Putmax(qbuf%it,str6(2),6,3)
	      CALL Putmax(qbuf%ter,str6(3)(1:3),3,2)
	      WRITE(Unit=str8(1:3),FMT='(I3)',IOSTAT=ios)NINT(qbuf%th0)
	      WRITE (UNIT= str6(4)(1:3), FMT= '(i3)',IOSTAT=ios) NINT(qbuf%tw)
	      response = 'Temp  = '// str6(1) //'  Inc-T= '// str6(2) //
	1	   '  T-Err= '// str6(3)(1:3) //'%  T-wait= '//
	1	   str6(4)(1:3) //' min  Hold 1st = '//str8(1:3)
	      CALL Putline_NT (contr, response)
	      WRITE(Unit=str6(2)(1:3),FMT='(I3)',IOSTAT=ios)NINT(qbuf%th)
	      CALL Putmax(qbuf%hf,str13(1:8),8,4)
	      CALL Putmax(qbuf%ihf,str8,8,4)
	      WRITE(UNIT=str9,FMT='(I9)',IOSTAT=ios)NINT(qbuf%hfw)
	      response = 'Hfield = '//str13(1:8)//
	1	   '  Inc-HF = '//str8//'  HF-wait: '//str9//
	2	   '     Hold all = '//str6(2)(1:3)
	      CALL Putline_NT (contr, response)
	      WRITE (UNIT= st7(1), FMT= '(i7)',iostat=ios) NINT(qbuf%mon)
	      WRITE (UNIT= str6(1)(1:3), FMT= '(i3)',iostat=ios) NINT(qbuf%mpf)
	      WRITE (UNIT= str6(2)(1:3), FMT= '(i3)',iostat=ios) NINT(qbuf%pts)
	      response = 'Monitor= '// st7(1) //'  Prefac= '// 
	1	   str6(1)(1:3) // '  Type= '// qbuf%mt //
	2	   '  #pts = '// str6(2)(1:3)
	      CALL Putline_NT (contr, response)
	   END DO
	   CLOSE (1)
	ELSE IF (str4(1:1) .EQ. 'B') THEN
	   IF (nsta .EQ. 1) CALL Not_Implemented (*520)
	   OPEN (UNIT= 1, FILE= 'BBUFFER.BUF', STATUS= 'OLD', 
	1	ACCESS= 'DIRECT',RECL=BBUF_RECL, FORM= 'UNFORMATTED')
	   DO k = l, m
	      CALL Putline_NT (contr, ' ')
	      READ (1,REC=k) bbuf
	      WRITE (UNIT= st7(1)(1:2), FMT= '(i2)',iostat=ios) k         
	      response = 'BBuffer No: '//st7(1)(1:2)//'  Comment: '//
	1	   bbuf%com
	      CALL Putline_NT (contr, response)
! Comment out because range does not match declaration	      
!	      CALL Putmax(bbuf%a,st7(1),7,4)
!	      CALL Putmax(bbuf%b,st7(2),7,4)
!	      CALL Putmax(bbuf%c,st7(3),7,4)
	      CALL Putmax(bbuf%ef,str8,8,4)
	      response(1:)='Lattice constants: a = '//st7(1)//
	1	   '  b = '//st7(2)//'  c = '//st7(3)//
	1	   ' and FE = '//str8
	      CALL Putline_NT (contr, response)            
	      WRITE (UNIT= str6(1), FMT= '(f6.3)',iostat=ios) bbuf%hc
	      WRITE (UNIT= str6(2), FMT= '(f6.3)',iostat=ios) bbuf%kc
	      WRITE (UNIT= str6(3), FMT= '(f6.3)',iostat=ios) bbuf%lc   
	      response = 'Q-center = ('// str6(1) //','// str6(2) //','// 
	1	   str6(3) //')'
	      CALL Putmax(bbuf%tmp,str6(1),6,3)
	      CALL Putmax(bbuf%it,str6(2),6,3)
	      CALL Putmax(bbuf%ter,str6(3)(1:3),3,2)
	      WRITE(Unit=str8(1:3),FMT='(I3)',IOSTAT=ios)NINT(bbuf%th0)
	      WRITE (UNIT= str6(4)(1:3), FMT= '(i3)',IOSTAT=ios) NINT(bbuf%tw)
	      response = 'Temp  = '// str6(1) //'  Inc-T= '// str6(2) //
	1	   '  T-Err= '// str6(3)(1:3) //'%  T-wait= '//
	1	   str6(4)(1:3)//' min  Hold 1st = '//str8(1:3)
	      CALL Putline_NT (contr, response)
	      WRITE(Unit=str6(2)(1:3),FMT='(I3)',IOSTAT=ios)NINT(bbuf%th)
	      CALL Putmax(bbuf%hf,str13(1:8),8,4)
	      CALL Putmax(bbuf%ihf,str8,8,4)
	      WRITE(UNIT=str9,FMT='(I9)',IOSTAT=ios)NINT(bbuf%hfw)
	      response = 'Hfield = '//str13(1:8)//
	1	   '  Inc-HF = '//str8//'  HF-wait: '//str9//
	2	   '     Hold all = '//str6(2)(1:3)
	      CALL Putline_NT (contr, response)
	      WRITE (UNIT= st7(1), FMT= '(i7)',iostat=ios) NINT(bbuf%mon)
	      WRITE (UNIT= str6(1)(1:3), FMT= '(i3)',iostat=ios) NINT(bbuf%mpf)
	      WRITE (UNIT= str6(2)(1:3), FMT= '(i3)',iostat=ios) NINT(bbuf%pts)
	      response = 'Monitor= '// st7(1) //'  Prefac= '// 
	1	   str6(1)(1:3) // '  Type= '// bbuf%mt //
	2	   '  #pts = '// str6(2)(1:3)
	      CALL Putline_NT (contr, response)
	      DO j= 1, 2
		 IF (motfl(j)) THEN
		    st7(1) = PutStr (bbuf%a(j))
		    WRITE (UNIT= str4(1:1), FMT= '(i1)',iostat=ios) j
		    response = 'A'// str4(1:1) //'-pos= '// st7(1)
		    CALL Putline_NT (contr, response)
		 END IF
	      END DO
	      DO j= 3, 4
		 IF (motfl(j)) THEN
		    af(j) = bbuf%a(j) + (bbuf%pts - 1) * bbuf%i(j)
		    st7(1) = PutStr (bbuf%a(j))
		    st7(2) = PutStr (bbuf%i(j))
		    st7(3) = PutStr (af(j))
		    WRITE (UNIT= str4(1:1), FMT= '(i1)',iostat=ios) j
		    response = 'A'// str4(1:1) //'-beg= '// st7(1) //
	1		 '  Inc-'// str4(1:1)// '= '// st7(2) //
	2		 '  A'// str4(1:1) //'-end= '// st7(3)
		    CALL Putline_NT (contr, response)
		 END IF
	      END DO
	      DO j= 5, 6
		 IF (motfl(j)) THEN
		    st7(1) = PutStr (bbuf%a(j))
		    WRITE (UNIT= str4(1:1), FMT= '(i1)',iostat=ios) j
		    response = 'A'// str4(1:1) //'-pos= '// st7(1) 
		    CALL Putline_NT (contr, response)
		 END IF
	      END DO
	   END DO
	   CLOSE (1)
	ELSE IF (str4(1:1) .EQ. 'T') THEN
	   IF (nsta .NE. 4) CALL Not_Implemented (*520)
	   OPEN (UNIT= 1, FILE= 'TBUFFER.BUF', STATUS= 'OLD', 
	1	ACCESS= 'DIRECT',RECL=TBUF_RECL, FORM= 'UNFORMATTED')
	   DO k = l, m
	      CALL Putline_NT (contr, ' ')
	      READ (1,REC=k) tbuf
	      WRITE (UNIT= st7(1)(1:2), FMT= '(i2)',iostat=ios) k         
	      response = 'Tbuffer No: '//st7(1)(1:2)//'  Comment: '//
	1	   tbuf%com
	      CALL Putline_NT (contr, response)            
	      st7(1) = PutStr (tbuf%ec)
	      st7(2) = PutStr (tbuf%es)
	      st7(3) = PutStr (tbuf%ef)
	      response = 'E-center = '// st7(1) //'  E-step = '// st7(2)
	      response = response(1:36) //'     Analyzer fixed at '// st7(3)
	      CALL Putline_NT (contr, response)	
	      CALL Putmax(tbuf%tmp,str6(1),6,3)
	      CALL Putmax(tbuf%it,str6(2),6,3)
	      CALL Putmax(tbuf%ter,str6(3)(1:3),3,2)
	      WRITE(Unit=str8(1:3),FMT='(I3)',IOSTAT=ios)NINT(tbuf%th0)
	      WRITE (UNIT= str6(4)(1:3), FMT= '(i3)',IOSTAT=ios) NINT(tbuf%tw)
	      WRITE (UNIT= str6(4)(1:3), FMT= '(i3)',iostat=ios) NINT(tbuf%tw)
	      response = 'Temp  = '// str6(1) //'  Inc-T= '// str6(2) //
	1	   '  T-Err= '// str6(3)(1:3) //'%  T-wait= '// str6(4)(1:3) 
	2	   //' min  Hold 1st = '//str8(1:3)
	      CALL Putline_NT (contr, response)
	      WRITE(Unit=str6(2)(1:3),FMT='(I3)',IOSTAT=ios)NINT(tbuf%th)
	      CALL Putmax(tbuf%hf,str13(1:8),8,4)
	      CALL Putmax(tbuf%ihf,str8,8,4)
	      WRITE(UNIT=str9,FMT='(I9)',IOSTAT=ios)NINT(tbuf%hfw)
	      response = 'Hfield = '//str13(1:8)//
	1	   '  Inc-HF = '//str8//'  HF-wait: '//str9//
	2	   '     Hold all = '//str6(2)(1:3)
	      CALL Putline_NT (contr, response)
	      WRITE (UNIT= st7(1), FMT= '(i7)',iostat=ios) NINT(tbuf%mon)
	      WRITE (UNIT= str6(1)(1:3), FMT= '(i3)',iostat=ios) NINT(tbuf%mpf)
	      WRITE (UNIT= str6(2)(1:3), FMT= '(i3)',iostat=ios) NINT(tbuf%pts)
	      response = 'Monitor= '// st7(1) //'  Prefac= '// 
	1	   str6(1)(1:3) // '  Type= '// tbuf%mt //
	2	   '  #pts = '// str6(2)(1:3)
	      CALL Putline_NT (contr, response)
	   END DO
	   CLOSE (1)
	END IF

 520	errflag = .FALSE.   
	RETURN

 270	errflag = .TRUE.
	RETURN

	END
