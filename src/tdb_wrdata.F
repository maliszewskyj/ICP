#include "icp.h"
	SUBROUTINE Tdb_WrData(tofile,errflag, npt)
C     	****************************

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C  write out data collected for transducer board. IF tofile, write it to
C    tdbfile as well as to screen.					C
C   									      C
C     Gayle Greene, Sept 1997				              C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C

	use tdb
	IMPLICIT    none
	LOGICAL	    tofile,errflag
	CHARACTER*80 response
	INTEGER*4    ios, npt, iprf !# prefactors
	CHARACTER*6  nptstr,blank/'      '/
	CHARACTER*9  str9(8)
	INTEGER*4    ig,numvals, linevals, i, i1, i2, lresp

	IF (tofile)THEN !open file for writing out data
	  write(unit=nptstr,fmt='(I6)',iostat=ios)npt
	  OPEN (UNIT=2, FILE= tdbfile, FORM= 'FORMATTED',
	1      ACCESS= 'APPEND',STATUS= 'OLD', err=300)
	ENDIF

C  Write out values 7 per line to stay under 80 characters
	numvals = tdbpoint
	DO ig = 1,(numvals-1)/7 + 1 !do for each line of vals to output
	  response(1:)=' '
	  i1 = (ig-1)*7 + 1 !first index in tdbdata
	  i2 = i1 + 6      !last index in tdbdata
	  if (i2 .GT. numvals) i2 = numvals
	  linevals = 0
	  do i = i1, i2
	    linevals = linevals + 1
	    CALL Putmax (tdbdata(i),str9(linevals),9,8)
	  enddo
  	  WRITE(UNIT= response, FMT='(1x,7(1x,a9))',iostat=ios) (str9(i), i= 1,linevals)
	  lresp = 1 + (linevals * 10)
	  CALL Putcontr_NT (response(1:lresp))
	  IF (tofile) THEN
	    IF (ig .EQ. 1) THEN !write point number on first line
	      write(unit=2,fmt='(a)',iostat=ios)nptstr//response(1:lresp)
	    ELSE
	      write(unit=2,fmt='(a)',iostat=ios)blank//response(1:lresp)
	    ENDIF
	  ENDIF !tofile
	ENDDO


	IF (tofile)Close(unit=2)
	errflag = .FALSE.
	RETURN

 300	errflag = .TRUE.
	RETURN
	END
