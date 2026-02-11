#include "icp.h"
	LOGICAL FUNCTION PASSWORD_OK()
C	**********************

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C                                                                             C
C     Routine To Input a password and compare it to those on file	      C
C                                                                             C
C     Gayle Greene, Sept. 1996                                                C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
	use instr
	use smg_prep
	implicit none

	INTEGER*4	maxpass
	PARAMETER	(maxpass = 2)

	CHARACTER*20    passw,CHOICE(maxpass),validstr
	INTEGER*4	nchoice, i, trm, leng, rendition,lpass,lch(maxpass)
	integer*2	irow,icol

	CALL Putcontr(' Enter Current Password: ')
	CALL Putcontr(' ')

	lpass = 20
	CALL ReadPass(contr,keyb,passw,lpass)

C  Now get valid passwords to compare against
	CALL Valid_Words(nchoice,choice,lch)

C  Make comparison
	PASSWORD_OK = .FALSE.
	i = 1
	Do While (.NOT. password_ok .AND. i .LE. nchoice)
	  validstr(1:) = choice(i)(1:lch(i))
C	  print *,'>',passw,'< >',validstr,'<'
	  IF (passw .EQ. validstr) password_ok = .TRUE.
	  i = i + 1
	End Do
C	print *,'password_ok=',password_ok

	RETURN
	END

