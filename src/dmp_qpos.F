#include "icp.h"
	SUBROUTINE Dmp_qPos (fname,pts,counts,qbuf,posit,pol_csec)
C	************************************************************************

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C                                                                             C
C  Dump angle positions and data to file.bqn. Note: 			      C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
	use instr
	use focus
	use smg_prep
	use iso_c_binding
	IMPLICIT     none
	interface
#include "c_libwait.h"
	end interface

 	CHARACTER*12	fname !file name containing standard ICP data
	INTEGER*4    	pts
	REAL*4		posit(MAXMOTS) !motor positions
	INTEGER*4	counts(37)
	INTEGER*4	pol_csec

	INTEGER*4    ios,
     1			status,notries/50/,openfai/30/ !from $fordef
	INTEGER*4    i, j, maxtry, ilast, ik
	CHARACTER*12 outbuf,str12
	CHARACTER*12 locname
	CHARACTER*90 out
	CHARACTER*2  str2

	TYPE(q_buf) qbuf

	if (nsta .eq. -5) then
	   ilast = 7
	else if (nsta .eq. 4) then
	   ilast = 8
	else
	   ilast = 6
	endif

	locname=fname
	locname(11:11) = 'Q'

C---------------------------INITIALIZE DATA FILE ----------------------------C

	IF (pts .EQ. 0 .and. pol_csec .le. 1) THEN ! first call
	   maxtry = 0
	   OPEN (UNIT=2, FILE= fname, FORM= 'FORMATTED', ACCESS= 'SEQUENTIAL',
	1	STATUS= 'OLD', iostat=ios)
	   DO WHILE (ios .eq. openfai .and. maxtry .lt. notries)
	      maxtry = maxtry + 1
	      CALL libwait(.2,status)
	      OPEN (UNIT=2, FILE= fname, FORM= 'FORMATTED', ACCESS= 'SEQUENTIAL',
	1	   STATUS= 'OLD', iostat=ios)
	   ENDDO
	   IF (ios .ne. 0) RETURN

	   OPEN (UNIT=3, FILE= locname, FORM= 'FORMATTED', ACCESS= 'SEQUENTIAL',
	1	STATUS= 'NEW', err=999)

 100	   READ  (2, '(a)', END= 200,err=990) out ! Copy rest of file
	   WRITE (3, '(a)',err=990) out(1:len_trim(out))
	   GOTO 100
 200	   CLOSE (2)

c       add header of what is moving
	   i=0
	   out(1:)=' '
	   ik=1
	   DO j=1,ilast		!check if each motor moving
	      IF (motfl(j) .and. .not. fixed(j)) THEN !may move
		 WRITE (UNIT= str2, FMT='(i2)') j
		 out(1:)=out(1:ik)//  '    A'// str2 //'   '
		 ik=ik+10
		 IF (i .le. 5) THEN
		    i=i+1
		 ELSE
		    WRITE (UNIT=3,fmt='(a)',err=400)out(1:ik)
		    out(1:)=' '
		    ik=4
		    i=1 
		 ENDIF
	      ENDIF
	   ENDDO

	   IF (nsta.eq.-5 .and. focus5) THEN
	      DO j = first_focus, first_focus+num_focus-1
		 IF (motfl(j) .and. .not. fixed(j)) THEN !may move
		    WRITE (UNIT= str2, FMT='(i2)') j
		    out(1:)=out(1:ik)//  '    A'// str2 //'   '
		    ik=ik+10
		    IF (i .le. 5) THEN
		       i=i+1
		    ELSE
		       WRITE (UNIT=3,fmt='(a)',err=400)out(1:ik)
		       out(1:)=' '
		       ik=4
		       i=1 
		    ENDIF
		 ENDIF
	      ENDDO
	   ENDIF		!using focus

	   out(ik+1:ik+12)='      COUNTS'
	   ik=ik+12
	   WRITE (UNIT=3,fmt='(a)',err=400)out(1:ik)

	ELSE			!not first point
           maxtry = 0
 	   OPEN (UNIT=3, FILE= locname, FORM= 'FORMATTED', ACCESS= 'APPEND',
	1	STATUS= 'OLD', IOSTAT=ios)
	   DO WHILE (ios .EQ. openfai .AND. maxtry .LT. notries)
	      CALL libwait(.2,status)
	      OPEN (UNIT=3, FILE= locname, FORM= 'FORMATTED', ACCESS= 'APPEND',
	1	   STATUS= 'OLD', IOSTAT=ios)
	      maxtry = maxtry + 1
	   ENDDO
	   IF (ios .NE. 0)RETURN
	END IF

C---------------------------DUMP INTENSITY DATA -----------------------------C
	i=0
	out(1:)=' '
	ik=1
	DO j=1,ilast		!check if each motor moving
	   IF (motfl(j) .and. .not. fixed(j)) THEN !may move
	      CALL Putmax(posit(j),outbuf(1:9),9,3)
	      out(1:)=out(1:ik)//outbuf(1:9)//' '
	      ik=ik+10
	      IF (i .le. 5) THEN
		 i=i+1
	      ELSE
		 WRITE (UNIT=3,fmt='(a)',err=400)out(1:ik)
		 out(1:)=' '
		 ik=4
		 i=1 
	      ENDIF
	   ENDIF
	ENDDO

	IF (nsta.eq.-5 .and. focus5) THEN
	   DO j = first_focus, first_focus+num_focus-1
	      IF (motfl(j) .and. .not. fixed(j)) THEN !may move
		 CALL Putmax(posit(j),outbuf(1:9),9,3)
		 out(1:)=out(1:ik)//outbuf(1:9)//' '
		 ik=ik+10
		 IF (i .le. 5) THEN
		    i=i+1
		 ELSE
		    WRITE (UNIT=3,fmt='(a)',err=400)out(1:ik)
		    out(1:)=' '
		    ik=4
		    i=1 
		 ENDIF
	      ENDIF
	   ENDDO
	ENDIF			!using focus

	write(unit=out(ik+1:ik+12),fmt='(I12)',err=400) counts(2)
        ik=ik+12
	IF (pol_csec .GT. 1)out(1:ik-12)=' ' !do not have real pos. blank out.
	WRITE (UNIT=3,fmt='(a)',err=400)out(1:ik)

 400	CLOSE (3)
	RETURN

C       Error returns
 990	CLOSE(3)
 999	CLOSE(2)
	RETURN
	END
