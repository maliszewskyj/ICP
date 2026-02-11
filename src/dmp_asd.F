#include "icp.h"
	SUBROUTINE Dmp_ASD (fname,pts)
C	************************************************************************

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C                                                                             C
C  Dump raw data from area sensitive data to file. Note: LUN 17 is used here! C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
	use asd
	use iso_c_binding
	implicit none
	interface
#include "c_libwait.h"
	end interface
 	CHARACTER*12	fname !file name containing standard ICP data
	INTEGER*4    	pts

	INTEGER*4	 ee,ny,nx,l,itotal,ios,iolun/17/,maxtry,
     1			 status,notries/50/,openfai/30/ !from $fordef
	INTEGER*4    	 i, j, k, m,kpsd,imax/256/
	CHARACTER*80	 outpsd
	CHARACTER*20 datum
	REAL*4		 rval

C--------------------------- INITIALIZE DATA FILE ----------------------------C

	IF (pts .EQ. 0) THEN                                       ! first call
c         write header
 	   OPEN (UNIT=iolun, FILE= fname(1:9)//'ASD', FORM= 'FORMATTED', 
	1	ACCESS= 'SEQUENTIAL',
	1	 STATUS= 'NEW')
	   WRITE (unit=iolun, fmt=*,IOSTAT=ios)asd_numx,asd_numy
	ELSE
 	   maxtry = 0
 	   OPEN (UNIT=iolun, FILE= fname(1:9)//'ASD', FORM= 'FORMATTED', 
	1	ACCESS= 'APPEND',
	1	STATUS= 'OLD', IOSTAT=ios)
	   DO WHILE (ios .EQ. openfai .AND. maxtry .LT. notries)
	     CALL libwait(.2,status)
 	     OPEN (UNIT=iolun, FILE= fname(1:9)//'ASD', FORM= 'FORMATTED', 
	1	  ACCESS= 'APPEND',
	1	  STATUS= 'OLD', IOSTAT=ios)
	     maxtry = maxtry + 1
	   ENDDO
	END IF

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
	       WRITE (UNIT= datum, FMT= '(i20)',IOSTAT=ios) itotal
	       outpsd(kpsd:m) = datum(21-ee:20)
	       m = m + 1
	       outpsd(m:m) = ','
	       kpsd = m + 1
C-----------End of writing sum to file

	  END DO
	END DO
	IF (m.gt.2)write(iolun,fmt='(a)')outpsd(1:m-1) !get last line

	CLOSE (UNIT=iolun)

	END

