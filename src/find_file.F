#include "icp.h"
	SUBROUTINE Find_File(fname,foundname,context,status)
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C	Dummy routine which interfaces with DEC LIB$FIND_FILE		      C
C                                                                             C
C	Gayle Greene, April    , 1998                                         C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C
	use ISO_C_BINDING
	IMPLICIT NONE
	interface
	   subroutine GLOBFILE(pattern,lpat, retfile, lret, status) bind(C,name="GLOBFILE")
	   use ISO_C_BINDING, only: c_char, c_int
	   character(kind=c_char), intent(in) :: pattern(*)
	   integer(c_int), intent(in)         :: lpat
	   character(kind=c_char), intent(out):: retfile(*)
	   integer(c_int), intent(in)        :: lret
	   integer(c_int), intent(out)        :: status
	   end subroutine GLOBFILE
	end interface

C  Calling parameters
	CHARACTER*(*)	fname !name to search for
	CHARACTER*80    foundname !name found
	INTEGER*4	context !search context
	INTEGER*4       status !result of search returned

	INTEGER*4	lname,lfound

	lname = len(fname)
	lfound= len(foundname)
	CALL GLOBFILE(fname, lname, foundname, lfound, context)
	if (context.ne.0) then
	   status = 1
	else
	   status = 0
	endif
 	RETURN
	END
