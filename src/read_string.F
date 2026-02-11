#include "icp.h"
	Subroutine Read_String(keys,instr,outstr,
     1		bufsize,modifs,timeout,term_mask,numread,term,vdid)

C FORTRAN 95 MAGIC
	use iso_c_binding
	implicit none
	interface
	   integer(c_int) function LXGETSTRING(prompt,response,lbuf,outlen) bind(C, name="LXGETSTRING")
	   use iso_c_binding, only: c_int, c_char
	   character(kind=c_char), intent(in) :: prompt(*)
	   character(kind=c_char), intent(out) :: response(*)
	   integer(c_int), intent(in) :: lbuf
	   integer(c_int), intent(out):: outlen
	   end function
	end interface

	integer*4 vdid, keys, bufsize, modifs, term_mask, timeout,
     1		numread,term
	character* (*) instr
	character* (*) outstr
	character*80   localstr
        integer        retn
	INTEGER*4      len1

	len1 = LEN(outstr)
	do while (len1 .GT. 1 .AND. outstr(len1:len1) .EQ. ' ')
	  len1 = len1 - 1
	enddo

	localstr(1:) = outstr(1:len1)//char(0)  !prompt

        retn = LXGETSTRING(localstr(1:len1+1), instr, bufsize, numread)
	instr(numread+1:)=' '

	RETURN
	END
