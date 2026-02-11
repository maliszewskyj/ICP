#include "icp.h"
	SUBROUTINE Test_Ststcs (mon, prf, counts, *, *)
C	***********************************************

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C                                                                             C
C     Routine To Check The Statistics Of The Current Prefactor Count. If Bad  C
C     Statistics During The First Three Prefactor Loops The Whole Point Is    C
C     Repeated By RETURN 1. If Bad Statistics Later, Only Part Of The Point   C
C     Is Repeated By Return2. If OK, Normal RETURN.                           C
C     Called from: Run_I_Buf, Run_Q_Buf, Run_Trash                            C
C                                                                             C
C     Beat Schmid, October 1989                                               C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C

	use instr
	use smg_prep
	IMPLICIT     none
	REAL*4       mon
	INTEGER*4    first, last, i, k, prf, retry

	INTEGER*4    counts(37), cksum(36,3), test, avcnt(36),ios,status
	LOGICAL      repeat, trash
	CHARACTER*5  out
	CHARACTER*6  str6(2)
	CHARACTER*80 response
	CHARACTER*12	fname
	CHARACTER*23	timestr

	SAVE cksum, avcnt, retry


	trash = .FALSE.
	GOTO 100
	ENTRY Test_Ststcs_BT1 (fname,mon, prf, counts, *, *)
	trash = .FALSE.
	GOTO 100
	ENTRY Test_Ststcs_T (mon, prf, counts, *, *)
	trash = .TRUE.

C--------------------DETERMINE WHICH DETECTORS TO CHECK ---------------------C

 100	IF (.NOT. trash) THEN                 
	   first = 1
	   last  = scl
	   IF (nsta .EQ. 4) last = 1
	ELSE
	   first = 2
	   last  = 2
	END IF	  

	
C------------------------NOW DO WHAT YOU HAVE TO DO -------------------------C

	repeat = .FALSE.
	DO k = first, last
	   IF (prf .LT. 3) THEN
	      cksum(k,prf) = counts(k+1)
	   ELSE IF (prf .EQ. 3) THEN
	      cksum(k,prf) = counts(k+1)
	      avcnt(k) = NINT ((cksum(k,1) + cksum(k,2) + cksum(k,3)) / 3.)
	      retry = 0
	      DO i = 1, 3
		 test = ABS(cksum(k,i) - avcnt(k)) - 
	1	      NINT(3.0 * SQRT(FLOAT(avcnt(k)))) - 10
		 IF (test .GT. 0 .AND. NINT(mon) .GT. 3) THEN
		    WRITE (UNIT= out(1:2), FMT= '(i2)') k
		    WRITE (UNIT= str6(1), FMT= '(i6)') avcnt(k)
		    WRITE (UNIT= str6(2), FMT= '(i6)') cksum(k,i)
		    response(1:) = '  Main_Statistic fail: -- detector no. '// 
	1		 out(1:2) //' -- average:'// str6(1) //
	2		 '  current:'// str6(2)
		    CALL PUTLINE (contr, ' ')

		    CALL PUTLINE (contr, response)
		    IF (nsta .EQ. 1)THEN !write response to file
		       OPEN (UNIT=2, FILE= fname, FORM= 'FORMATTED',
	1		    ACCESS= 'APPEND', STATUS= 'OLD', IOSTAT=ios)
		       IF (ios .EQ. 0)THEN !opened OK
			  call get_asctime(timestr,status)
			  response(1:) = ' Main_Stat Fail: -- det no. '// 
	1		       out(1:2) //' -- av:'// str6(1) //
	2		       '  cur:'// str6(2)//'  '//
	3		       timestr(1:6)//' '//timestr(13:)
			  write(unit=2,fmt='(a1,a78)',iostat=ios)'$',response(1:78)
			  close(unit=2,iostat=ios)
		       ENDIF	!opened OK
		    ENDIF	!BT1
		    RETURN 1
		 END IF 
	      END DO
	   ELSE
	      test = ABS(counts(k+1) - avcnt(k)) - 
	1	   NINT(3.0 * SQRT(FLOAT(avcnt(k)))) - 10
	      IF (test .GT. 0) THEN
	         retry = retry + 1
		 IF (retry .LE. 10) THEN
		    WRITE (UNIT= out(1:2), FMT= '(i2)') k
		    WRITE (UNIT= str6(1), FMT= '(i6)') avcnt(k)
		    WRITE (UNIT= str6(2), FMT= '(i6)') counts(k+1)
	            CALL PUTLINE (contr, ' ')

		    response(1:) = '  Statistic fail: -- detector no. '// 
	1		 out(1:2) //' -- average:'// str6(1) //
	2		 '  current:'// str6(2) 
		    CALL PUTLINE (contr, response)
		    IF (nsta .EQ. 1)THEN !write response to file
		       OPEN (UNIT=2, FILE= fname, FORM= 'FORMATTED',
	1		    ACCESS= 'APPEND',STATUS= 'OLD',  IOSTAT=ios)
		       IF (ios .EQ. 0)THEN !opened OK
			  call get_asctime(timestr,status)
			  response(1:) = ' Stat Fail: -- det no. '// 
	1		       out(1:2) //' -- av:'// str6(1) //
	2		       '  cur:'// str6(2)//'  '//
	3		       timestr(1:6)//' '//timestr(13:)
			  write(unit=2,fmt='(a1,a78)',iostat=ios)'$',response(1:78)
			  close(unit=2,iostat=ios)
		       ENDIF	!opened OK
		    ENDIF	!BT1
		    repeat = .TRUE.
		 ELSE
        	    CALL PUTLINE (contr, ' ')
	            response (1:) = '  Statistic failure limit reached, point starts again!'
		    CALL PUTLINE (contr, response)
	 	    RETURN 1
		 END IF
	      END IF
	   END IF		! (*  which prefactor case  *)
	END DO			! (*  loop over all detectors  *)
	IF (.NOT. repeat) THEN
	   DO k = first, last
	      avcnt(k) = (avcnt(k)*(prf-1) + counts(k+1)) / prf
	   END DO
	ELSE
	   IF (.not. vmedata) CALL Reset_Display !Reset NIM count display
 	   RETURN 2
	END IF
	RETURN
	END
