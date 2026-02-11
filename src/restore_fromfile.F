#include "icp.h"
	SUBROUTINE Restore_fromfile(motorno,showvals)
C     	***********************************
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C                                                                             C
C   Read position information in motpos.buf for all motors and write back     C
C   to cotrollers                                                             C
C   Motpos.buf has hardware angle, zero                                       C
C                                                                             C
C	Nick Maliszewskyj, March 2002                                         C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
	use instr
	IMPLICIT     none

	logical		showvals
	integer 	motorno
	INTEGER*4    j, ios
	REAL*4       currentpos(MAXMOTS), FZ, zeroval(MAXMOTS), Rdangl
	CHARACTER*8	str2,str3
	CHARACTER*3	str1
	CHARACTER*80	response
	CHARACTER*80    GETHOME


C--------------------- FETCH CURRENT ANGLES INTO FILE ----------------------C

 20	OPEN (UNIT= 1, FILE= MOTPOS_BUF, STATUS= 'OLD',
	1    ACCESS= 'DIRECT', FORM= 'UNFORMATTED',
	1    RECL=MOTBUF_RECL,IOSTAT=ios)
	if (ios .ne. 0) then
	  call putcontr(' Error updating file for Fetch')
	  return
	endif
	do j = 1,MAXMOTS
	  READ (1,REC=j) currentpos(j),zeroval(j)
	enddo
 	CLOSE (1)

	if (showvals) then
	  call putcontr(' Retrieving values for RESTORE command as follows:')
	endif

	if (motorno .eq. 0) then ! Restore all motor positions
	   do j = 1, MAXMOTS
	      IF (motfl(j)) THEN
		 if (showvals) then
		    write(unit=str1,fmt='(i3)',  iostat=ios)j
		    write(unit=str2,fmt='(f8.3)',iostat=ios)currentpos(j)
		    write(unit=str3,fmt='(f8.3)',iostat=ios)zeroval(j)
		    response(1:)=' Mot: '//str1//'    Hardware Position: '//
	1		 str2//'   Zero Adjustment: '//str3
		    call putcontr_nt(response)
		 endif
		 call iang(j,currentpos(j))
	      ENDIF		!motfl 
	   enddo
	else 
	   j = motorno
	   IF (motfl(j)) THEN
	      if (showvals) then
		 write(unit=str1,fmt='(i3)',  iostat=ios)j
		 write(unit=str2,fmt='(f8.3)',iostat=ios)currentpos(j)
		 write(unit=str3,fmt='(f8.3)',iostat=ios)zeroval(j)
		 response(1:)=' Mot: '//str1//'    Hardware Position: '//
	1	      str2//'   Zero Adjustment: '//str3
		 call putcontr_nt(response)
	      endif
	      call iang(j,currentpos(j))
	   ENDIF		!motfl 
	endif

	RETURN 
	END
