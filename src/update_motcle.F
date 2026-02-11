#include "icp.h"
	SUBROUTINE Update_Motcle(newword,lnew)
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C                                                                             C
C       Routine To Change Password					      C
C                                                                             C
C       Gayle Greene, Sept. 1996                                              C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C

	IMPLICIT NONE

C       Calling parameters
	CHARACTER*(*) newword	!new password
	INTEGER*4     lnew	!length of new password


	INTEGER*4  maxpass
	PARAMETER  (maxpass = 2)

	INTEGER*4  numwords,i,j,lbuf
	CHARACTER*60 buffer(maxpass), newbuf
	CHARACTER*3  str3

	CHARACTER*80 GETHOME

C       Set up newbuf
	newbuf(1:) = ' '
	lbuf = 1
	DO j = 1, lnew
	   write(unit=str3,fmt='(I2,1X)',err=95)ICHAR(newword(j:j))
	   newbuf(1:) = newbuf(1:lbuf)//str3
	   lbuf = lbuf+3
	ENDDO

C       READ in and overwrite contents of motcle file
	OPEN(1,FILE=MOTCLE_ICP,STATUS='OLD',ERR=95)
	READ(1,*,ERR=95)numwords
	DO I=1,numwords
	   READ(1,fmt='(a)',err=95)buffer(i)
	ENDDO
	REWIND(UNIT=1)
	WRITE(1,*,ERR=96)numwords
	DO I=1,numwords-1
	   WRITE(1,fmt='(a)',err=96)buffer(i)
	ENDDO
	WRITE(1,fmt='(a)',err=96)newbuf(1:lbuf)
	CLOSE(UNIT=1)

	RETURN


 95	CALL Putcontr(' Error overwriting password')
	GOTO 97
 96	CALL Putcontr(' Error writing out new password')
 97	CLOSE(UNIT=1,err=98)
 98	RETURN
	END

