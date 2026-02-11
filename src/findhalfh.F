#include "icp.h"
	SUBROUTINE FindHalfH(npt,x,y,p,fitmin,conv)

C	***************************************************

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C                                                                             C
C At call:
C	npt = number of points at which counts were made
C       x(1->npt) = position of mot at each point
C       y(1->npt) = counts at each point (fratio at each point for type 5 & 6)
C	p(4) has height of peak (counts)
C	p(5) has position of peak (either x(jmin) or x(jmax)
C  At return:
C	conv = true if treatment proceeded without problem
C	p(5) has position extrapolated for this height
C                                                                             C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C

	IMPLICIT     none
	INTEGER*4    npt
	REAL*4       x(MAXPOINTS), y(MAXPOINTS)
	LOGICAL	     fitmin, conv
	REAL*4	     p(6)

 	CHARACTER*20	hstring
	REAL*4	      halfheight, Cdiff1, Cdiff0, Pdiff0, deltapos
	INTEGER*4    j, ij, ios
	LOGICAL	     found

	conv = .false. !default if abort

	halfheight = p(4)/2.
	write(unit=hstring,fmt=*,iostat=ios)halfheight
	if (fitmin) then
	  call putcontr(' Halfheight in adjusted units would be: '//hstring)
	else
	  call putcontr(' Halfheight is: '//hstring)
	endif

	found = .false.
	ij = 1
	do while (ij .lt. npt .and. .not. found)
	  if (y(ij) .eq. halfheight) then
	    found = .true.
	  else
	    if (y(ij) .lt. halfheight .and. y(ij+1) .gt. halfheight) then
	      found = .true.
	    else
	      if (y(ij) .gt. halfheight .and. y(ij+1) .lt. halfheight) then
		found = .true.
	      else
		ij = ij + 1
	      endif
	    endif
	  endif
	enddo

	if (.not. found) return

	if (y(ij) .eq. halfheight) then
	  p(5) = x(ij)
	else ! must extrapolate
	  Cdiff1 = halfheight - y(ij)
	  Pdiff0 = x(ij+1) - x(ij)
	  Cdiff0 = y(ij+1) - y(ij)
	  deltapos = ( Cdiff1 * Pdiff0 ) / Cdiff0
	  p(5) = x(ij) + deltapos	
	endif !extrapolate
	conv = .true.

	write(unit=hstring,fmt=*,iostat=ios)p(5)
	call putcontr(' position at halfheight is: '//hstring)

	return
	end

