#include "icp.h"
c       fit1gauss routine. RW Erwin
c       simplified June 1995 to try to fix OPCODE crashes in ICP
c       fits up to MAXPT data points to single Gaussian plus FLAT background
	subroutine fit1gauss(NPT,x,y,e,p,ierr)

	parameter (MAXPT=200,MAXP=6)

c       lsq control parameters
c       output unit=6, noprinting, chisqr convergence with chi error weighting,
c       test = .01*2**-4, maxiterations=30, sign change disallowed

	real*4 x(MAXPT), y(MAXPT), e(MAXPT), p(MAXP)

	dimension ip(MAXP)

c       declarations for other arrays used in lsqgauss call

	real*4 w(MAXPT), yc(MAXPT)

c       
	dimension lsqs(8)
c       RWErwin June 2006 changed to allow sign change of params lsqs(6)=1->0
	data lsqs/6,0,-2,-4,30,0,1,-1/

c       set up indices of free parameters
	ip(1) = 1
	ip(2) = 4
	ip(3) = 5
	ip(4) = 6
	npf = 4

c       calculate weights from errors
	do i=1,NPT
	   if(e(i).le.0.)then
	      if(y(i).ne.0.)then
		 w(i) = 1./abs(y(i))
	      else
		 w(i) = 1.
	      endif
	   else
	      w(i) = 1./e(i)**2
	   endif
	enddo

c       do the fit. currently the calculated curve, yc, is not used

        call lsqgauss(lsqs,p,npf,ip,NPT,x,y,w,yc)
	
	ierr = lsqs(1)

	return
	end


c--------------------------------------------------------------

	subroutine GAUSS(x,p,ycalc,partial)
	parameter(MAXP=6)
	real*4 x, p(MAXP), ycalc, partial(MAXP)

c       quadratic background plus 1 Gaussian
c       this is argument list required by fitting routine LSQ
c       cant handle zero width.  Sign change prevention should cure this in LSQ
c       just in case, we check for zero peak width, p(6) = 0

	parameter (fact=1.665109)


c       fact converts FWHM to sigma. i.e. sqrt(2) * sigma = FWHM/fact
c       so fact = 2sqrt(ln2) = 1.665109

c       RWE June 2006 make sure p(6) is positive since we are going to turn
c       off sign change control so that fitting position can change sign
	if( p(6) < 0. ) p(6) = -p(6) ;
	if( abs(p(6)) .gt. 0. ) then
	   wdinv=fact/p(6)
	   xdel = x - p(5)
	   xm = xdel*wdinv
	   if(abs(xm).gt.3.5)then
	      partial(4) = 0.
	   else
	      partial(4) = exp(-xm*xm)
	   endif
	   partial(5) = 2.*p(4)*xm*wdinv*partial(4)
	   partial(6) = partial(5)*xdel/p(6)
	   ycalc = p(4)*partial(4)
	else
	   partial(4) = 0.
	   partial(5) = 0.
	   partial(6) = 0.
	   ycalc = 0.
	endif

	partial(1) = 1.
	partial(2) = x
	partial(3) = x*x
        ycalc = ycalc + p(1) + p(2)*x + p(3)*partial(3)

	return
	end


c----------------------------------------------------------
c       least squares fit to a non-linear function by linearization of the
c       fitting function. a gradient search proportion is included.
c       see bevington 11-4 p.232-240
c       this version does NOT include constraint conditioning. see lsqcon.f
c       correlation matrix calculation is commented out in this version
c       
c       Version for single gaussian no constraints, no param errors



c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++c
c       c
c       fun(x(i),npar,p,yc(i),part)                                           c
c       is the external function which returns calculated values yc           c
c       and parameter derivatives part.                                       c
c       c
c       npt            no. data points                                        c
c       npar           no. parameters                                         c
c       npf            no. free parameters                                    c
c       ip(npf)        indices in p of free parameters                        c
c       p(npar)        parameters                                             c
c       x(npt)         independent variables passed to fun                    c
c       if more than one is needed users  program provides                    c
c       y(npt)         dependent variables                                    c
c       w(npt)         weights = 1/sigma**2                                   c
c       yc(npt)        calculated values returned by fun                      c
c       c
c       lsqs(8)        control flags                                          c
c       c
c       lsqs(1)        convergence flag                                       c
c       lun for writes at call time                                           c
c       0 converged                                                  c
c       1 failed maxit                                               c
c       2 failed maxitgradient                                       c
c       3 failed sign change test                                    c
c       4 failed npt .gt. npf                                        c
c       5 diagonal curvature matrix entry = zero                     c
c       6 failed mxpt or mxparm                                      c
c       c
c       lsqs(2)    print flag                                                 c
c       0 suppress printing except error messages                    c
c       1 print xsqr and final p  , ycal if nofit                    c
c       2 all but grad and curvature-1                               c
c       3 print all                                                  c
c       c
c       lsqs(3)    convergence type                                           c
c       1 p% convergence test                                        c
c       2 chi% convergence test                                      c
c       negative value to weight errors by chi                       c
c       c
c       lsqs(4)    test = .01*2**(lsqs(4))                                    c
c       c
c       lsqs(5)    max iterations allowed                                     c
c       c
c       lsqs(6)    sign change flag                                           c
c       0 allow p   to change sign                                   c
c       1 always prevent sign change                                 c
c       2 prevent sign change after maxit/2                          c
c       c
c       lsqs(7)    index of worst parameter guess                             c
c       c
c       lsqs(8)    xcut = float(lsqs(8)) for lsqs(8).gt.0                     c
c       only vary lsqs(7) paramter until xsqr is .lt.                    c
c       xcut. lsqs(8).le.0 then no control                               c
c       c
c       cm(mxpar,npar)     correlation matrix			 	      c
c       c
c       a,am,b,bm
c       ps(npar)	save parameter values				      c
c       dp(npf)	parameter steps						      c
c       part(npar)    partials returned by fun				      c
c       c
c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++c


        subroutine lsqgauss(lsqs,p,npf,ip,npt,x,y,w,yc)

	parameter (MAXPT=200,MAXP=6)

	real*4 p(MAXP), x(MAXPT), y(MAXPT), w(MAXPT), yc(MAXPT)
	dimension lsqs(8), ip(1)

c       local storage
	real*4 ps(MAXP), dp(MAXP), part(MAXP)
	real*4 dd

	real*8 b(MAXP), bm(MAXP)
	real*8 a(MAXP,MAXP), am(MAXP,MAXP)

	dimension indx(MAXP) 

	logical swap/.false./,conv/.false./

C	Provide machine epsilon - NOTE: heps is a machine-dependent
C       quantity and should really be calculated rather than hard-coded.
#if defined(VMS)
C	Note that the machine epsilon used here for the vax is about two
C	orders of magnitude lower than the real value (5.96e-8)
	parameter (tst=.01,heps=1.e-10)
#else
C       Use a small heps for Linux/Intel, but the real number is more like
C       0.11e-6 for single precision and 0.22e-15 for double precision
	parameter (tst=.01,heps=1.e-15)
#endif
	
	lun = lsqs(1)
	test=tst*2.**lsqs(4)
	npv = npf
	npar = MAXP
	
c       check for dimension error returns
	
	if(npt.le.npv)then
	   lsqs(1)=4
	   go to 800
	endif

	f=1.E-3
	
c       f is the gradient search proportion
	
c       set up for initial function call

	itot=0
	it=1
	itg=0			! counts successive gradient iterations
	itgtot=0		! counts total gradient iterations
	itntot=0		! counts total normal iterations

	xcut=float(lsqs(8))

c&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&c

c       main function calculation loop including a and b matrix calc

 100	itg=0

 101	do j=1,npv
	   b(j)=0.
	   do k=1,npv
	      a(j,k)=0.
	   enddo
	enddo

	xsqr = 0.
	do i=1,npt
	   xv = x(i)
	   call GAUSS(xv,p,yv,part)
	   yc(i) = yv
	   dy = y(i) - yc(i)
	   xsqr = xsqr + w(i)*dy*dy

c**     CALCULATE a and b MATRICES
	   do j=1,npv
	      b(j) = b(j) + w(i)*dy*part(ip(j))
	      do k=1,npv
		 a(j,k) = a(j,k) + w(i)*part(ip(j))*part(ip(k))
	      enddo
	   enddo

	enddo			! end of NPT loop

	xsqr = xsqr/(npt-npv)

	if(npv.lt.1)return	! NO FIT

	if(xsqr.le.0.)goto 800	! FINISHED

	itot = itot + 1

	if(itot.le.1)then
	   do j=1,npar
	      ps(j) = p(j)
	   enddo

	   if(lsqs(8).gt.0 .and. xsqr.le.xcut)then ! check for xsqr cutoff test

c**     FAILED xsqr CUTOFF TEST SO VARY ONLY THE WORST PARAMETER lsqs(7)

	      npvs=npv
	      npv=1
	      ips=ip(1)
	      ip(1)=lsqs(7)
	      swap = .true.
	   endif
	elseif(swap .and. (xsqr/xsqrprev.le..9.or.xsqr.le.xcut))then

c**     xsqr NOW ok SO RELOAD FREE PARAMETERS AND CONTINUE

	   swap = .false.
	   ip(1)=ips
	   npv=npvs
	endif


c       CONVERGENCE TEST if after first iteration

	if(itot.gt.1)then

	   if(iabs(lsqs(3)).eq.1)then ! p% convergence test

	      do j=1,npv
		 if(p(ip(j)).ne.0.)then
		    if(abs(dp(j)/p(ip(j))).gt.test)go to 700
		 else
		    if(abs(dp(j)).gt.test)go to 700
		 endif
	      enddo

	      lsqs(1)=0		! converged
	      go to 800

	   else			! xsqr% convergence test

	      if(xsqr.le.0.)go to 800
	      if(abs(xsqrprev-xsqr)/xsqr.le.test)then
		 lsqs(1)=0
		 go to 800
	      endif

	   endif
	endif			! end of convergence test

c       failed convergence if reach here

 700	if(itot.gt.1 .and. xsqrprev.lt.xsqr)then
	   itgtot=itgtot+1
	   conv=.false.
c       damp as chi increases and try again with previous parameter set
	   do j=1,npar
	      p(j)=ps(j)
	   enddo
	   if(swap)then		! unswap since not working properly
	      itg=itg+1
	      f=f*10.
	      swap = .false.
	      ic=0
	      npv=npvs
	      ip(1)=ips
	      go to 101		! go all the way back but dont reset itg to zero
	   end if

	   if(itg.ge.lsqs(5))then
	      lsqs(1)=2		! max gradient iterations exceded
	      goto 800
	   endif

c       reload saved matrices am,bm into a,b
	   itg=itg+1
	   f=f*10.
	   do j=1,npv
	      b(j) = bm(j)
	      do k=1,npv
		 a(j,k) = am(j,k)
	      enddo
	      a(j,j) = (1.+f)*a(j,j)
	   enddo
	   goto 200

	elseif(itot.gt.1 .and. xsqrprev.ge.xsqr)then

c       undamp as chi decreases and not too many gradient iterations
	   do j=1,npar
	      ps(j)=p(j)
	   enddo

	   itntot=itntot+1
	   if(conv)then
	      f=f*10.**sign(2,itgtot-itntot)
	   else
	      f=f*10.**sign(1,itgtot-itntot)
	   endif
	   conv=.true.
	   it=it+1
	   if(it.gt.lsqs(5))then ! max iterations exceded
	      lsqs(1)=1
	      goto 800
	   endif
	endif			! end of itot.gt.1

c       ready to calculate parameter steps

	do j=1,npv
	   if(a(j,j).le.0.)then
c       fix jth free param and continue if possible
	      if(npt.le.npv-1)then
		 lsqs(1)=4
		 go to 800
	      endif
	      do k=j,npv-1
		 ip(k) = ip(k+1)
	      enddo
	      npv = npv - 1
	      goto 100  
	   endif
        enddo
	
c       save a and b into am and bm and scale a diagonals by 1+f

        do j=1,npv
	   bm(j)=b(j)
	   do k=1,npv
	      am(j,k)=a(j,k)
	   enddo
	   a(j,j)=(1.+f)*a(j,j)
        enddo

c**     SOLVE FOR THE PARAMETER STEPS
c**     a-matrix will be returned as LU-decomposition
c**     b-vector is returned with solution

 200	call ludcmp(a,npv,MAXP,indx,dd)
	call lubksb(a,npv,MAXP,indx,b)

c       dd .eq. 0. returned likely indicates singularity in equations soln
	if(dd .eq. 0.)then
	   lsqs(1) = 7
	   goto 800
	endif

	do j=1,npv
	   dp(j)=b(j)
	   p(ip(j))=p(ip(j))+dp(j)
	enddo

c       sign change check

	if(lsqs(6).eq.1 .or. (lsqs(6).eq.2.and.it.le.lsqs(5)/2))then
	   do j=1,npv
	      h=1.

 51	      if(p(ip(j))*ps(ip(j)).ge.0.)then
		 dp(j)=h*dp(j)
	      else
		 h=.5*h
		 if(h.lt.heps)then
		    lsqs(1)=3
c**     PARAMETERS INSIST ON CHANGING SIGN SO EXIT
		    go to 800
		 endif
		 p(ip(j))=ps(ip(j))+h*dp(j)
		 go to 51
	      endif

	   enddo
	endif			! end of sign change check

c       update xsqrprev and return for next iteration
	if(conv.or.itot.le.1)then
	   xsqrprev = xsqr
	   goto 100
	else
	   goto 101
	endif

 800	return
	end


c       a = sum( w(i)*gradj(yc(i))*gradk(yc(i)) )
c       e = a inverse is the error matrix
c       b = sum( w(i)*(y(i)-yc(i))*gradk(yc(i)) )
c       dp = b*e solution gives parameter increments dp
c       chisqr = sum( w(i)*(y(i)-yc(i))**2 ) / (npt - npv)
c       sigmasqr p(j) = e(j,j)


c------------------------------------------------------------
C       ludcmp
C       Given an N x N matrix A, with physical dimension NP, this
C       routine replaces it by the LU decomposition of a rowwise 
C       permutation of itself. A and N are input. A is output, 
C       arranged as upper triangular and lower triangular in same
C       matrix; INDX is an output vector which records the row
C       permutation effected by the partial pivoting; D is output
C       as +-1 depending in whether the number of row interchanges
C       was even or odd, respectively. This routine is used in
C       combination with LUBKSB to solve linear equations or invert.

	subroutine ludcmp(a,n,np,indx,d)

	parameter (nmax=100,tiny=1.d-20)
	real*8 a(np,np), vv(nmax) ! vv STORES ROW SCALING
	dimension indx(n)
	real*4 d

	real*8 aamax, sum, dum

	d=1.
	do i=1,n		! LOOP OVER ROWS TO GET THE SCALING INFO
	   aamax=0.d0
	   do j=1,n
	      if(abs(a(i,j)).gt.aamax) aamax=abs(a(i,j))
	   enddo
	   if(aamax.eq.0.d0)then ! CHECK FOR SINGULAR MATRIX
	      d=0.
	      return
	   end if
	   vv(i)=1./aamax
	enddo

	do j=1,n

	   if(j.gt.1)then
	      do i=1,j-1
		 sum=a(i,j)
		 if(i.gt.1)then
		    do k=1,i-1
		       sum=sum-a(i,k)*a(k,j)
		    enddo
		    a(i,j)=sum
		 end if
	      enddo
	   end if

	   aamax=0.		! INITIALIZE SEARCH FOR LARGEST PIVOT ELEMENT.

	   do i=j,n
	      sum=a(i,j)
	      if(j.gt.1)then
		 do  k=1,j-1
		    sum=sum-a(i,k)*a(k,j)
		 enddo
		 a(i,j)=sum
	      end if
	      dum=vv(i)*abs(sum) ! FIGURE OF MERIT FOR THE PIVOT.
	      if(dum.ge.aamax)then
		 imax=i
		 aamax=dum
	      end if
	   enddo

	   if(j.ne.imax)then	! INTERCHANGE ROWS?
	      do k=1,n
		 dum=a(imax,k)
		 a(imax,k)=a(j,k)
		 a(j,k)=dum
	      enddo
	      d=-d
	      vv(imax)=vv(j)	!  ALSO INTERCHANGE THE SCALE FACTOR.
	   end if

	   indx(j)=imax

	   if(j.ne.n)then	!  NOW DIVIDE BY THE PIVOT ELEMENT.
	      if(a(j,j).eq.0.)then
		 a(j,j)=tiny
		 d=0.
		 return		!  SINGULAR MATRIX.
	      end if
	      dum=1./a(j,j)
	      do i=j+1,n
		 a(i,j)=a(i,j)*dum
	      enddo
	   end if

	enddo

	if(a(n,n).eq.0.)then
	   a(n,n)=tiny
	   d=0.
	end if
	return
	end

c-------------------------------------------------------------
c       lubksb
c       lubksb SOLVES THE SET OF n LINEAR EQUS ax=b.  hERE a IS INPUT
c       AS THE lu DECOMPOSITION DETERMINED BY ludcmp.  indx IS ALSO 
c       THE PERMUTATION VECTOR FROM ludcmp.  b IS INPUT AS THE RIGHT-
c       HAND-SIDE VECTOR, AND RETURNS WITH THE SOLUTION VECTOR x.
c       a, n, np AND indx ARE NOT MODIFIED AND ARE AVAILABLE FOR
c       SUCCESSIVE CALLs WITH DIFFERENT bS.

	subroutine lubksb(a,n,np,indx,b)

	real*8 a(np,np), b(n)
	dimension indx(n)

	real*8 sum

	ii=0

	do i=1,n
	   ll=indx(i)
	   sum=b(ll)
	   b(ll)=b(i)
	   if(ii.ne.0)then
	      do j=ii,i-1
		 sum=sum-a(i,j)*b(j)
	      enddo
	   else if(sum.ne.0.)then
	      ii=i
	   end if
	   b(i)=sum
	enddo

	do i=n,1,-1
	   sum=b(i)
	   if(i.lt.n)then
	      do j=i+1,n
		 sum=sum-a(i,j)*b(j)
	      enddo
	   end if
	   b(i)=sum/a(i,i)	!  STORE A COMPONENT OF THE SOLUTION VECTOR x.
	enddo
	return
	end
