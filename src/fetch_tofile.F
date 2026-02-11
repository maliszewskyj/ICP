#include "icp.h"
	SUBROUTINE Fetch_tofile(showvals)
C     	***********************************
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C                                                                             C
C   Update position information in motpos.buf for all motors                  C
C    Motpos.buf has hardware angle, zero 
C                                                                             C
C	Gayle Greene, August 2000                                             C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
	use instr
	IMPLICIT     none

	logical		showvals

	INTEGER*4    j, ios
	REAL*4       currentpos(MAXMOTS), FZ, zeroval(MAXMOTS), Rdangl
	CHARACTER*8	str2,str3
	CHARACTER*3	str1
	CHARACTER*80	response
	CHARACTER*80    GETHOME


C--------------------- FETCH CURRENT ANGLES INTO FILE ----------------------C
	   DO j = 1, MAXMOTS
	      IF (motfl(j)) THEN
		 currentpos(j) = RdAngl(j)
		 zeroval(j) = FZ(j,currentpos(j))
	      ELSE
		 currentpos(j) = -999.99
		 zeroval(j) = 0.0
	      END IF
	   END DO

 20	OPEN (UNIT= 1, FILE= MOTPOS_BUF, STATUS= 'OLD',
	1	ACCESS= 'DIRECT', FORM= 'UNFORMATTED',
	1	RECL=MOTBUF_RECL,IOSTAT=ios)
	if (ios .ne. 0) then
	  call putcontr(' Error updating file for Fetch')
	  return
	endif
	do j = 1,MAXMOTS
	  WRITE (1,REC=j) currentpos(j),zeroval(j)
	enddo
 	CLOSE (1)

	if (showvals) then
	  call putcontr(' Recording values for FETCH command as follows:')
	  do j = 1, MAXMOTS
	    IF (motfl(j)) THEN
	      write(unit=str1,fmt='(i3)',  iostat=ios)j
	      write(unit=str2,fmt='(f8.3)',iostat=ios)currentpos(j)
	      write(unit=str3,fmt='(f8.3)',iostat=ios)zeroval(j)
	      response(1:)=' Mot: '//str1//'    Hardware Position: '//
     1		str2//'   Zero Adjustment: '//str3
	      call putcontr_nt(response)
	    ENDIF !motfl 
	  enddo
	endif

	RETURN 
	END
