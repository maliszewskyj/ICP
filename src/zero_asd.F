#include "icp.h"
	SUBROUTINE ZERO_ASD
C     	****************************

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C                                                                             C
C   									      C
C     Gayle Greene, Feb 1997					              C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
	use ASD
	IMPLICIT    none

	INTEGER*4   i,j

	do i = 1,256
	  do j = 1,256
	    asd_rawdata(i,j) = 0
	  enddo
	enddo

      
	RETURN
	END
