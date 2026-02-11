#include "icp.h"
	Subroutine GETDIR(VMSFILE,right)
C	*****************************

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C                                                                             C
C       Routine to determine name of local directory & return it in 	      C
C	   VMSFILE(1:right)                                                   C
C	Called from: Run_i_buf,Run_q_buf,Run_t_buf          		      C
C                                                                             C
C	Gayle Greene, August, 1992					      C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
	use iso_c_binding
	IMPLICIT	none
	interface
	   subroutine PWD(cwd, right) bind(C,name="PWD")
	   use iso_c_binding
	   character(kind=c_char), intent(out)::cwd(*)
	   integer(c_int)        , intent(out)::right
	   end subroutine
	end interface
	INTEGER*4       left, right, status, context /0/
	CHARACTER*80    vmsfile

	! Just use current working directory
C	CALL GETCWD(vmsfile,right)
	CALL PWD(vmsfile, right)
	RETURN
	END
