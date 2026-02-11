#include "icp.h"
	SUBROUTINE Simulate_MRAT 
C	****************************************************

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C  Input values to be used in howlong         				      C
C    (Only called from nopriv process)
C   Note: these values are not written to hwlong.buf. Only used locally
C    in this process.
C
C	Gayle Greene, Mar, 1996                                               C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
	use instr
	use smg_prep
	use hwlong
	IMPLICIT     none

C  Local variables	
	REAL*4		tmpdm, c1, c2, c3, c4, rval
	INTEGER*4	m, ios, ig, j
	CHARACTER*20    str20, outstr
	CHARACTER*1	label
	LOGICAL		isdigit
	CHARACTER*80    GETHOME

C  decide motor to memorize according to instr.
	IF (nsta .eq. 2 .or. nsta .eq. 4 .or. nsta .eq. 9 .or.
     1		nsta .eq. -5)THEN
	  m = 2
	ELSE
	  CALL Putcontr(' Invalid command for this instrument.')
	  CALL Putcontr(' Howlong does not know monochromator motor.')
	  RETURN
	ENDIF

 10	CALL Putcontr('Enter new value or hit return to keep indicated default:')
	write(unit=outstr,fmt=*,iostat=ios)cntrate
 	CALL READ_STRING(keyb, str20,
     1		'Monitor rate: '//outstr//' counts/sec. New value: ',
     1		 20,j,j,j, ig,j, contr) !j is dummy param for absoft
	IF (ig .ne. 0)THEN
	  READ(unit=str20,fmt=*,err=10)rval
	  cntrate = rval
	ENDIF
	write(unit=outstr,fmt=*,iostat=ios)monang
 	CALL READ_STRING(keyb, str20,
     1		'Mono angle: '//outstr//' degrees. New value: ',
     1		 20,j,j,j, ig,j, contr) !j is dummy param for absoft
	IF (ig .ne. 0)THEN
	  READ(unit=str20,fmt=*,err=10)rval
	  monang = rval
	ENDIF
 20	write(unit=outstr,fmt=*,iostat=ios)dlast
 	CALL READ_STRING(keyb, str20,
     1		'Monochromator dspacing: '//outstr//' ... New value: ',
     1		 20,j,j,j, ig,j, contr) !j is dummy variable for absoft
	IF (ig .ne. 0)THEN
	   j = 1
	   do while (str20(j:j) .EQ. ' ')
	     j = j + 1
	   enddo
	   IF (isdigit(str20(j:j))) THEN                    ! numerical value
	      READ (UNIT= str20(j:ig), FMT= *, ERR= 20) rval
	   ELSE					           ! alphanum reference
	      IF (str20(j:j+1) .EQ. 'PG') THEN
	        IF (str20(j+2:j+2) .eq. '2')then
		 rval=1.67708 !pg2
	        else
		 rval = 3.35416 !pg
	        endif
	      ELSE IF (str20(j:j+1) .EQ. 'CU') THEN
		 rval = 1.2780
	      ELSE IF (str20(j:j+1) .EQ. 'HS') THEN
		 rval = 3.445
	      ELSE
		 CALL Putcontr('Reply not recognized')
		 GOTO 20
	      END IF
	   END IF
	   dlast = rval
	ENDIF

	OPEN (UNIT= 1, FILE= HWCOEFFS_CFG, STATUS= 'OLD', ACCESS= 
	1     'SEQUENTIAL', FORM= 'FORMATTED', ERR= 999)
	READ (1,fmt='(a)',err=888)label
	tmpdm = -537. !pick wierd value to insure no match
	DO WHILE (abs(tmpdm - dlast) .GT. .0001) 
	    READ (1, FMT= *,err=888,end=888) tmpdm, c1, c2, c3, c4
	ENDDO 
	CLOSE(1)
	chw1 = c1
	chw2 = c2
	chw3 = c3
        chw4 = c4

	write(unit=outstr,fmt=*,iostat=ios)hwlconst
 	CALL READ_STRING(keyb, str20,
     1		'Constant overhead: '//outstr//' seconds/point. New value: ',
     1		 20,j,j,j, ig,j, contr) !j is dummy param for absoft
	IF (ig .ne. 0)THEN
	  READ(unit=str20,fmt=*,err=10)rval
	  hwlconst = rval
	ENDIF
	cnstovr = .FALSE.
	IF (hwlconst .NE. 0.)cnstovr = .TRUE.

	RETURN

 888	close(1)
 999	CALL Putcontr(' Cannot input coeffs for monochromator')
	RETURN

	END
