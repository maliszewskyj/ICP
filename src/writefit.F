#include "icp.h"
	SUBROUTINE WriteFit (p, conv, chgfile, psfile)
C	***************************************************

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C  TypeFit: write the results of the fit to the screen
C  If chgfile is true at call, also write results to file
C  At call, p has fit params                                                  C
C  If conv is false at call, fit did not converge		              C
C                                                                             C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C

	use instr
	use iso_c_binding
	IMPLICIT NONE
	interface
#include "c_libwait.h"
	end interface
	CHARACTER*12  psfile
	LOGICAL  chgfile, conv
	REAL*4 p(6) 
	INTEGER*4  ios, status, maxtry, notries/20/,openfai/30/ !from $fordef
 	CHARACTER*80 response,fitstr1
	CHARACTER*120 fitstr2
	character*1 coeff1/'+'/
	character*4 coeff2/'*x+'/
	character*6 coeff3/'*x**2+'/
	character*10 coeff4/'*exp(-((x-'/
	character*11 coeff5/')*1.665109/'/
	character*5 coeff6/')**2)'/
	character*3  namestr
	CHARACTER*160 bstr
	integer*4  lb, i
	CHARACTER*18 bfit
	BYTE	    tabbyte/9/
	CHARACTER*1 tab
	EQUIVALENCE (tabbyte,tab)

	IF (nsta .gt. 0) THEN
	   namestr(1:) = 'BT%'
	ELSEIF (nsta .eq. 0) THEN         
	   namestr(1:) = 'XR%'
	ELSE
	   namestr(1:) = 'NG%'         
	ENDIF
	IF (nsta .eq. -11) THEN
	   namestr(1:3) = 'CG1'
	ELSE
	   WRITE (UNIT= namestr(3:3), FMT= '(i1)') abs(nsta)
	ENDIF

	bstr(1:)=namestr//':END'//tab
	lb=8
	IF (conv) THEN
	   bstr(1:)=bstr(1:lb)//'TYPE=FP'//tab
	   lb=lb+8
	   WRITE (UNIT=response(1:), FMT= '(a,f10.3)', iostat=ios)
	1	' Fit Results:   BG = ', p(1)
	   CALL Putcontr_NT ( response)
	   WRITE (UNIT=response(1:), FMT= '(a,f10.3)', iostat=ios)
	1	'            Height = ', p(4)
	   CALL Putcontr_NT ( response)
	   WRITE (UNIT=response(1:), FMT= '(a,f11.4)', iostat=ios)
	1	'               Pos = ', p(5)
	   CALL Putcontr_NT ( response)
	   WRITE (UNIT=response(1:), FMT= '(a,f11.4)', iostat=ios)
	1	'             Width = ', p(6)
	   CALL Putcontr_NT ( response)
	   CALL Putcontr_NF ( ' ')
	   IF (chgfile)THEN
	      fitstr1(1:)=' Fit: '
	      fitstr2(1:)=' **'
	      WRITE(unit=fitstr1(1:),
	1	   fmt='(1x,a,f11.4,2(a,f10.3),a,f11.4)', iostat=ios)
	2	   ' Fit: Pos= ',p(5),' BG= ',p(1),' Height= ',
	3	   p(4),' Width= ',p(6)
	      WRITE(unit=fitstr2(1:),fmt=
	1	   '(1x,a,f10.3,a,f10.3,a,f10.3,a,f10.3,a,f11.4,a,f11.4,a)',
	2	   iostat=ios)' Fit eq: ',
	3	   p(1),coeff1,p(2),coeff2,p(3),coeff3,
	4	   p(4),coeff4,p(5),coeff5,p(6),coeff6
	   ENDIF		!chgfile
	   bfit = 'FIT_P1='
	   do i = 1,6
	      write(unit=bfit(6:6),fmt='(I1)',iostat=ios)i
	      write(unit=bfit(8:),fmt='(f11.4)',iostat=ios)p(i)
	      bstr(1:)=bstr(1:lb)//bfit//tab
	      lb=lb+19
	   enddo
	ELSE			!no convergence
	   bstr(1:)=bstr(1:lb)//'TYPE=NOCONV'
	   lb=lb+11
	   CALL Putcontr_NT (' Fit did not converge !')
	   IF (chgfile) then
	      fitstr1(1:)=' Fit: ** Did not converge ** '
	      fitstr2(1:)=' **'
	   ENDIF		!chgfile
	ENDIF			!converged
	CALL GoBroad(lb,bstr)

        IF (chgfile) THEN	!rewrite file here with fit info first
	   maxtry = 0
	   OPEN (unit=2,file=psfile,status='old',IOSTAT=ios)
	   DO WHILE (ios .EQ. openfai .AND. maxtry .LT. notries)
	      CALL libwait(.2,status)
	      OPEN (unit=2,file=psfile,status='old',IOSTAT=ios)
	      maxtry = maxtry + 1
	   ENDDO
	   IF (ios .EQ. 0)THEN	!input file opened ok
	      OPEN (UNIT=3,file=psfile,status='new',iostat=ios)
	      IF (ios .EQ. 0)THEN !output file opened ok
		 WRITE (3, '(a)',iostat=ios) fitstr1
		 WRITE (3, '(a)',iostat=ios) fitstr2
 450		 READ (2, '(a)', END= 499) response
		 WRITE(3, '(a)',iostat=ios) response
		 GOTO 450
 499		 CLOSE(2)

		 CLOSE (3)
	      ENDIF		!output file opened ok
	   ENDIF		!input file opened ok
	ENDIF			!chgfile

	RETURN
	END
