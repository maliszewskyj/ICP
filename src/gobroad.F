#include "icp.h"
	SUBROUTINE Gobroad(lstr,bstring)
C	**********************************************

	use iso_c_binding
	IMPLICIT      none
	interface
	   subroutine BROADCAST(len, wrstring) bind(C, name="BROADCAST")
	   use iso_c_binding, only: c_int, c_char
	   integer(c_int), intent(in) :: len
	   character(kind=c_char), intent(in) :: wrstring(*)
	   end subroutine
#include "c_libwait.h"
	end interface

 	CHARACTER*(*) bstring !string to broadcast
	INTEGER*4     lstr !non-empty part of string
	BYTE	      bcr/13/,blf/10/
	CHARACTER*1   cr,lf
	EQUIVALENCE  (bcr,cr)
	EQUIVALENCE  (blf,lf)
	CHARACTER*19   xname
	INTEGER*4	maxtry, notries/4/,status,openfai/30/, !fordef
     1			ios
	CHARACTER*80 GETHOME

	xname = XPEEK_XR0
C Add linefeed to string
	lstr = lstr+1
	bstring(lstr:lstr)=lf
	IF (bstring(1:3) .eq. 'XR0')THEN
	  IF (bstring(4:9) .eq. ':START')THEN !open file as new
 	    OPEN (UNIT=17, FILE= xname, FORM= 'FORMATTED',
	1	  ACCESS= 'SEQUENTIAL',STATUS= 'UNKNOWN',
	1	  iostat=ios)
	    REWIND(UNIT=17,iostat=ios)
	  ELSE !append to existing file
            maxtry = 0
	    OPEN (UNIT=17, FILE=xname, FORM= 'FORMATTED', 
     1     	    ACCESS= 'APPEND', STATUS='UNKNOWN',
     1		    iostat=ios)
	    DO WHILE (ios .EQ. openfai .AND. maxtry .LE. notries)
	      CALL libwait(.2,status)
	      OPEN (UNIT=17, FILE=xname, FORM= 'FORMATTED', 
     1     	    ACCESS= 'APPEND', STATUS='UNKNOWN',
     1		    iostat=ios)
	      maxtry = maxtry+1
	    ENDDO
	  ENDIF !start
	  IF (ios .EQ. 0)THEN
	      write(unit=17,fmt='(1x,a)',err=109)bstring(1:lstr-1)
 109	      close(unit=17,iostat=ios)
	  ENDIF
	ENDIF 
	call broadcast(lstr,bstring) 

 	RETURN
	END
 
