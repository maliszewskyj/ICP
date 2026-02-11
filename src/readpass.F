#include "icp.h"
	SUBROUTINE ReadPass(VID,KID,PWORD,LPASS)
C	************************************************

C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C                                                                              C
C	ReadPass: Get in password without displaying letters typed             C
C	Gayle Greene, Sept 1996						       C
C       At call: lpass has max length password allowed                         C
C       At return: lpass has actual length typed in                            C
C                  pword(1:lpass) has word typed in                            C
C                                                                              C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
	use iso_c_binding
	IMPLICIT NONE
	interface
	   subroutine LXREADPASS(contr,keyb,passw,lpass)
	1	bind(C,name="LXREADPASS")
	   use iso_c_binding
	   integer(c_int), intent(in)::contr
	   integer(c_int), intent(in)::keyb
	   character(kind=c_char),intent(out)::passw(*)
	   integer(c_int), intent(out)::lpass
	   end subroutine
	end interface

	INTEGER*4 STATUS,VID,KID,ROW,COL,NEWROW,NEWCOL,MODIFIERS,TRM
	INTEGER*4  LPASS, TERMINATOR, i
	CHARACTER*(*) PWORD
	CHARACTER*1 CHAR_STAR/'*'/

        CALL LXREADPASS(VID,KID,PWORD,LPASS)
	RETURN
	END
