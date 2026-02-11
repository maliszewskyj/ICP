#include "icp.h"
	SUBROUTINE Lia_WrData(tofile,errflag, npt)
C     	****************************

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C                                                                             C
C   									      C
C     Gayle Greene, Sept 1997				              C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C

	use lia
	IMPLICIT    none
	LOGICAL	    tofile,errflag
	CHARACTER*80 response1,response2
	INTEGER*4    ios, npt, i
	CHARACTER*6  nptstr
	REAL*4	     slia_x,slia_y,slia_of,slia_oa,
     1			slia_adc1,slia_adc2,slia_adc3,slia_adc4

C  Initialize running totals:
	IF (prfpoint .GT. 1)THEN
	    slia_x = 0
	    slia_y = 0
	    slia_of = 0 
	    slia_oa = 0
	    slia_adc1 = 0
	    slia_adc2 = 0
	    slia_adc3 = 0
	    slia_adc4 = 0
	ENDIF

C  Open file 
	IF (tofile)THEN
	  write(unit=nptstr,fmt='(I6)',iostat=ios)npt
	  OPEN (UNIT=2, FILE= liafile, FORM= 'FORMATTED',
	1      ACCESS= 'APPEND', STATUS= 'OLD', err=300)
	ENDIF

C  Write out each point to file. Last point to screen
	DO i = 1,prfpoint
	  write(unit=response1(1:),fmt=*,iostat=ios)
     1		rlia_x(i),rlia_y(i),rlia_of(i),rlia_oa(i)
	  write(unit=response2(1:),fmt='(1x,2(f12.3))',iostat=ios)
     1		rlia_adc1(i),rlia_adc2(i)
	  IF (tofile)THEN
	    WRITE(Unit=2,fmt='(1x,a6,1x,a60,1x,a50)',iostat=ios)nptstr,
     1	     response1(1:60),response2(1:50)
	  ENDIF
	  IF (prfpoint .GT. 1) THEN
	    slia_x = slia_x + rlia_x(i)
	    slia_y = slia_y + rlia_y(i)
	    slia_of = slia_of + rlia_of(i)
	    slia_oa = slia_oa + rlia_oa(i)
	    slia_adc1 = slia_adc1 + rlia_adc1(i)
	    slia_adc2 = slia_adc2 + rlia_adc2(i)
	    IF (i .EQ. prfpoint)THEN !last loop
	      write(unit=response1(1:),fmt=*,iostat=ios)
     1		slia_x/float(i),slia_y/float(i),slia_of/float(i),
     1		slia_oa/float(i)
	      write(unit=response2(1:),fmt='(1x,2(f12.3))',iostat=ios)
     1		slia_adc1/float(i),slia_adc2/float(i)
	    ENDIF
	  ENDIF !prfpoint .GT. 1
	  IF (i .EQ. prfpoint)THEN !last loop
	   IF (tofile)THEN
	    call putcontr_nf(response1(1:60))
	    call putcontr_nf(response2(1:50))
	   ELSE
	    call putcontr_nt(response1(1:60))
	    call putcontr_nt(response2(1:50))
	   ENDIF
	  ENDIF

	ENDDO

C  Close file
	IF (tofile) close(unit=2)

	errflag = .FALSE.
	RETURN

 300	errflag = .TRUE.
	RETURN
	END
