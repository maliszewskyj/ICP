#include "icp.h"
	SUBROUTINE AD201_WrData(tofile,errflag, npt)
C     	****************************

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C                                                                             C
C   									      C
C     Gayle Greene, Sept 1997				              C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C

	use ad201
	IMPLICIT    none
	LOGICAL	    tofile,errflag
	CHARACTER*160 response
	INTEGER*4    ios, npt, i, lres, j
	CHARACTER*6  nptstr
	CHARACTER*2 chanstr
	CHARACTER*11 str11

C  Initialize running totals:

C  Open file
	IF (tofile)THEN
	  write(unit=nptstr,fmt='(I6)',iostat=ios)npt
	  OPEN (UNIT=2, FILE= ad2file, FORM= 'FORMATTED',
	1      ACCESS= 'APPEND',STATUS= 'OLD', err=300)
	  do j = 1, adnumchan
	    write(unit=chanstr,fmt='(I2)',iostat=ios)chanarr(j)
	    response(1:)=' '
	    lres = 1
	    do i = 1,ad2point
	      write(unit=str11,fmt='(F11.4)',iostat=ios)advolts(j,i)
	      response(1:)=response(1:lres)//' '//str11
	      lres = lres + 12
	    enddo
	    WRITE(Unit=2,fmt='(1x,a6,1x,a2,1x,a)',iostat=ios)nptstr,chanstr,
     1	        response(1:lres)
	  enddo
	  close(unit=2)
	ENDIF !tofile

C  write last readings in volts to screen
	response(1:)=' '
	lres = 1
	DO j = 1,adnumchan
	    write(unit=str11,fmt='(F11.4)',iostat=ios)advolts(j,ad2point)
	    response(1:)=response(1:lres)//' '//str11
	    lres = lres + 12
	enddo
	IF (tofile)THEN
	    call putcontr_nf(response(1:lres))
	ELSE
	    call putcontr_nt(response(1:lres))
	ENDIF


	errflag = .FALSE.
	RETURN

 300	errflag = .TRUE.
	RETURN
	END
