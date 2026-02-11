#include "icp.h"
	SUBROUTINE PUTMAX(arg,buffer,length,iprec)
C	************************************
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C       C
C	Puts the value ARG in the string BUFFER(1:LENGTH) with as much      C
C       precision as possible up to iprec places to right of decimal. If it C
C       will not fit, fill buffer in with *****  			    C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C

	IMPLICIT NONE

	REAL*4      arg, absarg
	CHARACTER*1 buffer
	INTEGER*4   length, lbuf, iprec, sign, ldig, width, i,
	1    newprec, idig, formtyp, fpt
	CHARACTER*12 fstr
	
	formtyp = 1		!f format is default
	
	IF (arg .ge. 0) THEN	!reserve space for sign only if necessary
	   sign = 0
	ELSE
	   sign = 1
	ENDIF

	lbuf = abs(length)	!protection against improper call
	absarg = abs(arg)

	ldig = 0

	DO WHILE (absarg .ge. 10**ldig)
	   ldig = ldig + 1
	ENDDO
C	Here, ldig has no. of digits necessary to left of decimal point w/o sign
	
c       Verify that value will catch at least 1st significant digit if very small
	IF (ldig.eq.0)THEN
	   idig=1
	   DO WHILE (absarg .lt. 10**(-idig) )
	      idig=idig-1
	   ENDDO
	   IF (idig.gt.iprec)THEN !must use exponential
	      IF (lbuf .lt. (sign + 5)) goto 270 !insufficient room for exp format
	      newprec = lbuf - (sign + 4)
	      formtyp = 2
	   ENDIF
	ENDIF

	If (formtyp .ne. 2) THEN	
	   width = sign + ldig + 1 + iprec !total no. of chars needed
	   IF (width .le. lbuf) THEN !No problem. Write as requested.
	      newprec = iprec	!use requested precision
	   ELSE
	      newprec = iprec - (width - lbuf) !newprec = old - excess
	      IF (newprec .eq. -1) THEN !leave off decimal point and have enough room
		 formtyp = 0	!use integer format
	      ELSEIF (newprec .lt. -1) THEN !At this point, we must switch to exponential format
		 IF (lbuf .lt. (sign + 5)) goto 270 !insufficient room for exp format
		 newprec = lbuf - (sign + 4)
		 formtyp = 2
	      ENDIF		! newprec eq -1
	   ENDIF		!width le lbuf
	ENDIF			!formtyp ne 2

C       Using formtyp, lbuf and newprec, put together a format string
	fstr(1:)='(i'
	fpt = 3	
	if (lbuf .gt. 9) then
	   write(unit=fstr(fpt:fpt+1),fmt='(i2)',err=270)lbuf
	   fpt = fpt+2
	else
	   write(unit=fstr(fpt:fpt),fmt='(i1)',err=270)lbuf
	   fpt = fpt+1
	endif
	if (formtyp .eq. 0)then !integer. we are finished setting up string
	   fstr(fpt:fpt)=')'
	   write(unit=buffer(1:lbuf),fmt=fstr(1:fpt),err=270)INT(arg) !don't round cause may need another place
	   return
	elseif (formtyp .eq. 2) then !Exponential
	   fstr(2:2) = 'E'
	else
	   fstr(2:2) = 'f'	!floating point format
	endif
	fstr(fpt:fpt)='.'
	fpt = fpt + 1
	if (newprec .gt. 9) then
	   write(unit=fstr(fpt:fpt+1),fmt='(i2)',err=270)newprec
	   fpt = fpt+2
	else
	   write(unit=fstr(fpt:fpt),fmt='(i1)',err=270)newprec
	   fpt = fpt+1
	endif
	if (formtyp .eq. 2) then
	   fstr(fpt:fpt+1)='E1'
	   fpt = fpt+2
	endif
	fstr(fpt:fpt) = ')'

	write(unit=buffer(1:lbuf),fmt=fstr(1:fpt),err=270)arg

	RETURN

C       Error branch
 270	DO i=1,lbuf		!err branch
	   buffer(i:i)='*'
	ENDDO

	RETURN
	END
