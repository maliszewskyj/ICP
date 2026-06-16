#include "icp.h"
	SUBROUTINE Get_ASD_block (error)
C     	****************************************

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C                                                                             C
C Records in asd_rawdata a 256*256 integer*4 image of memory in AIM312.	      C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
	use instr
	use asd
	use iso_c_binding
	IMPLICIT      none
	interface
#include "c_gpiben.h"
#include "c_gpibct.h"
	end interface

	LOGICAL       error
	INTEGER*4     ierr
	CHARACTER*30  outstr
	INTEGER*4     lstr
	integer*2 mode,ia(131072),ib
	byte	  ba(262144),b(2)

	equivalence (ba,ia),(ib,b)
	integer*4 ntrans,iad,i,j

        INTEGER*2   ktype/0/
        byte        blf/10/,bcr/13/
        character*1 cr,lf
        equivalence (lf,blf),(cr,bcr)

c       Setup  upload command
        outstr  =       'upl(0,0)'//cr
        lstr    =       9

C-------  Set ASD to listener --------------------------------C
        Call Gpib_listen(asd_addr,error)

C-------  Send out string to ASD -----------------------------C
        Call ASD_WrPol(outstr,lstr,error,ktype)

C-------  Set ASD to unlisten --------------------------------C
        Call Gpib_unlisten(error)


C-------  Set ASD to talk
        Call Gpib_talk(asd_addr,error)

C------- Do DMA transfer of data
	ntrans	=	131072
	mode	=	16
C<<<<<FIX ME
	IF (gpibenet) THEN
C	   CALL GPIBEN_RD_ASD(ia,ntrans,iad,ierr)
	ELSE
C	   Call GPIBCT_RDBLK(ba,ntrans,iad,error)
	ENDIF
	error = (iad .ne. ntrans)


C-------  Set ASD to unlisten --------------------------------C
        Call Gpib_unlisten(error)

C------- Pull out data from bytes returned & place in asd_rawdata
	do i=1,256
	  do j=1,256
		iad	=	2*(i+(j-1)*256)-1
		b(1)	=	ba(iad)
		b(2)	=	ba(iad+1)
		asd_rawdata(i,j) = asd_rawdata(i,j) + ib
		asd_storage(iad) = asd_rawdata(i,j)
	  enddo
	enddo

	END      
      




