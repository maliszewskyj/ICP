#include "icp.h"
	SUBROUTINE READ_ASD
C     	****************************

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C                                                                             C
C  Use use_asdfile flag to decide how many reads to do                        C
C   									      C
C     Gayle Greene, May 1996					              C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C

	use asd
	use iso_c_binding
	IMPLICIT    none
	interface
#include "c_debugval.h"
	end interface
	CHARACTER*30 outstr
	CHARACTER*50 reply
	CHARACTER*30 str30, digstr
	BYTE	digval(30)
	EQUIVALENCE (digstr,digval)
	LOGICAL     errflg
	REAL*4	    rval
	INTEGER*4   lstr, lpar(4),noreads, ee(4), i, j, ios
	INTEGER*2   ktype/0/, !determines whether a wait between chars is done
     1		    lreply
	byte	    blf/10/,bcr/13/
	character*1 cr,lf
	character*3	str(4)
	equivalence (lf,blf),(cr,bcr)
	integer*4	l,m
	integer   dbg
	logical   error

C  Download all raw data
	call get_asd_block(error)

C  Decide if and how to sum data
	IF (use_asdfile)THEN
	  noreads = noslices
	  do i = 1,4
	    lpar(i) = slices(1,i)
	  enddo
	ELSE
	  noreads = 1
	  lpar(1) = asd_xmin
	  lpar(2) = asd_ymin
	  lpar(3) = asd_xmax
	  lpar(4) = asd_ymax
	ENDIF

	CALL debugval(dbg)
	if (dbg.ne.0) then
	   print *, 'READ_ASD: use_asdfile =',use_asdfile
	   print *, 'READ_ASD: lpar(1)     =',lpar(1)
	   print *, 'READ_ASD: lpar(2)     =',lpar(2)
	   print *, 'READ_ASD: lpar(3)     =',lpar(3)
	   print *, 'READ_ASD: lpar(4)     =',lpar(4)
	endif

C Need to do both if running a buffer
	IF (use_asdfile)THEN 
	  fpwindow = 0.0
	  do l = asd_xmin, asd_xmax
	    do m = asd_ymin, asd_ymax
	      fpwindow = fpwindow + asd_rawdata(l,m)
	    enddo
	  enddo
	ENDIF

C  Sum data
        DO i = 1, noreads
	  asd_data(i)	=	0.
	   do l=lpar(1),lpar(3)
		do m=lpar(2),lpar(4)
		  asd_data(i) = asd_data(i)+asd_rawdata(l,m)
		enddo
	   enddo

	  do j = 1,4
	    lpar(j) = slices(i,j)
C	    lpar(j) = slices(i+1,j)
	  enddo
	ENDDO

	RETURN
	END
