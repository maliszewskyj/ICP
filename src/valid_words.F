#include "icp.h"
	Subroutine Valid_Words(numwords,words,lwords)
C	*********************************************

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C                                                                             C
C     Routine To Open File and Get allowed passwords into char strings	      C
C                                                                             C
C     Gayle Greene, Sept. 1996                                                C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C

	implicit none

	INTEGER*4	maxpass
	PARAMETER	(maxpass = 2)

C  Calling Parameters and their contents at return
	INTEGER*4	numwords !number of allowed passwords in list
	CHARACTER*20	words(maxpass) !contains allowed passwords
	INTEGER*4	lwords(maxpass) !length of each word in words

	BYTE		ASCWORD(maxpass,20)
	INTEGER*4	i,j,ios,np
	CHARACTER*60    buffer

	CHARACTER*80    GETHOME

C  Now get valid passwords to compare against
	OPEN(1,FILE=MOTCLE_ICP,STATUS='OLD',ERR=96)
	READ(1,*,ERR=95)numwords
	DO I=1,numwords
	  READ(1,fmt='(a)',err=95)buffer
	  ios = 1
	  np = 20
	  DO WHILE ( ios .NE. 0 .AND. np .GT. 0 )
	     READ (UNIT= buffer, FMT= *, IOSTAT= ios) (ascword(i,j), j= 1, np)
	     IF ( ios .NE. 0 ) np = np - 1
	  ENDDO
	  lwords(i) = np
	ENDDO
	CLOSE(1)

	do i = 1,numwords
	  words(i)(1:) = ' '
	  j = 1
	  do while (ascword(i,j) .GT. 32 .AND. ascword(i,j) .LT.127 .AND.
     1		j .le. lwords(i))
	    words(i)(j:j) = CHAR(ascword(i,j))
	    j = j + 1
	  enddo
	enddo

	RETURN !without error

 95	CLOSE(UNIT=1)
 96	CALL Putcontr(' Error validating password')

	RETURN
	END

