#include "icp.h"
	SUBROUTINE ASD_IMAGE 
C	************************************************************************

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C                                                                             C
C  Dump raw data from area sensitive data to file. Note: LUN 17 is used here! C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
	use asd
	use iso_c_binding
	IMPLICIT     none

#include "gethome.h"	
 	CHARACTER*12	fname !file name containing standard ICP data
	CHARACTER*28 	sys_time

	INTEGER*4	 ee,ny,nx,l,itotal,ios,iolun/17/, status
	INTEGER*4    	 i, j, k, m,kpsd,imax/256/
	CHARACTER*80	 outpsd
	CHARACTER*20     datum
	REAL*4		 rval


C--------------------------- INITIALIZE DATA FILE ----------------------------C

	fname=HOMEDIR//'asd_data.img'

 	OPEN (UNIT=iolun, FILE= fname, FORM= 'FORMATTED', 
     1		ACCESS= 'SEQUENTIAL',
     1		STATUS= 'UNKNOWN',
     1		IOSTAT=ios)  !just return if error
	IF (ios.ne.0)return !just return if error

	CALL Date_Time(sys_time)
	WRITE (unit=iolun, fmt=*,IOSTAT=ios)sys_time
	WRITE (unit=iolun, fmt=*,IOSTAT=ios)asd_numx,asd_numy

C  Sum data from last point and write to file
	outpsd(1:)=' '
	kpsd = 2
	m = 0
	ny = imax/asd_numy
	nx = imax/asd_numx

	DO j = 1, imax, ny !vary 2nd dimension more slowly
	  DO i = 1,imax, nx
	    itotal = 0
	    DO l = i, i+nx-1
	      DO m = j, j+ny - 1
		itotal = itotal + asd_rawdata(l,m)
	      ENDDO
	    ENDDO
c-----------Now write sum to file
	       ee = 1
	       DO WHILE (itotal .GE. 10**ee)
	         ee = ee + 1
	       END DO
	       m = kpsd + ee-1	    	     
	       IF (m .GE. 80) THEN                        ! write string to file
		 WRITE (iolun, '(a80)') outpsd(1:80)
		 outpsd(1:)=' '
	         kpsd = 2
		 m = 2 + ee-1
	       END IF
#if defined(VMS)
	       WRITE (UNIT= outpsd(kpsd:m), FMT= '(i<ee>)',IOSTAT=ios)itotal
#else
	       WRITE (UNIT= datum, FMT= '(i20)',IOSTAT=ios)itotal
	       outpsd(kpsd:m) = datum(21-ee:20)
#endif
	       m = m + 1
	       outpsd(m:m) = ','
	       kpsd = m + 1
C-----------End of writing sum to file

	  END DO
	END DO
	IF (m.gt.2)write(iolun,fmt='(a)')outpsd(1:m-1) !get last line

	CLOSE (UNIT=iolun)

	RETURN
	END

