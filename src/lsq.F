#include "icp.h"

c&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&c

c&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&c
c                                                                             c
c least squares fit to a non-linear function by linearization of the          c
c fitting function. a gradient search proportion is included.                 c
c see bevington 11-4 p.232-240                                                c
c this version does NOT include constraint conditioning. see lsqcon.f         c
c code reworked May 90 to get rid of local storage			      c
c correlation matrix calculation is commented out in this version             c
c                                                                             c
c&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&c


c&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&c
c                                                                             c
c Version for single gaussian no constraints				      c
c	                                                                      c
c&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&c

        subroutine lsq(fun,lsqs,npt,x,y,w,
     1			  mxpar,npar,npf,ip,p,pe,xsqr,cm,yc,
     2			  part,a,am,b,bm,wkarea,ps,dp,yce)
 
c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++c
c                                                                             c
c fun(x(i),npar,p,yc(i),part)                                                 c
c   is the external function which returns calculated values yc               c
c   and parameter derivatives part.                                           c
c                                                                             c
c npt            no. data points                                              c
c mxpar		 max no. parameters for cm ifr cn a b matrices		      c
c npar           no. parameters                                               c
c npf            no. free parameters                                          c
c ip(npf)        indices in p of free parameters                              c
c p(npar)        parameters                                                   c
c pe(npf)        approx. stnd. dev. of parameters = sqrt(e(i,i))              c
c x(npt)         independent variables passed to fun                          c
c                if more than one is needed users  program provides           c
c y(npt)         dependent variables                                          c
c w(npt)         weights = 1/sigma**2                                         c
c yc(npt)        calculated values returned by fun                            c
c yce(npt)	 error of calculated values returned by fun		      c
c xsqr           reduced chi square of fit                                    c
c									      c
c lsqs(8)        control flags                                                c
c									      c
c lsqs(1)        convergence flag                                             c
c                lun for writes at call time                                  c
c                0 converged                                                  c
c                1 failed maxit                                               c
c                2 failed maxitgradient                                       c
c                3 failed sign change test                                    c
c                4 failed npt .gt. npf                                        c
c                5 diagonal curvature matrix entry = zero                     c
c                6 failed mxpt or mxparm                                      c
c									      c
c lsqs(2)    print flag                                                       c
c                0 suppress printing except error messages                    c
c                1 print xsqr and final p  , ycal if nofit                    c
c                2 all but grad and curvature-1                               c
c                3 print all                                                  c
c									      c
c lsqs(3)    convergence type                                                 c
c                1 p% convergence test                                        c
c                2 chi% convergence test                                      c
c                negative value to weight errors by chi                       c
c									      c
c lsqs(4)    test = .01*2**(lsqs(4))                                          c
c									      c
c lsqs(5)    max iterations allowed                                           c
c									      c
c lsqs(6)    sign change flag                                                 c
c                0 allow p   to change sign                                   c
c                1 always prevent sign change                                 c
c                2 prevent sign change after maxit/2                          c
c									      c
c lsqs(7)    index of worst parameter guess                                   c
c									      c
c lsqs(8)    xcut = float(lsqs(8)) for lsqs(8).gt.0                           c
c            only vary lsqs(7) paramter until xsqr is .lt.                    c
c            xcut. lsqs(8).le.0 then no control                               c
c                                                                             c
c cm(mxpar,npar)     correlation matrix			 		      c
c									      c
c a,am,b,bm(mxpar,npf/npf+1)  work space for linearized problem		      c
c ps(npar)	save parameter values					      c
c dp(npf)	parameter steps						      c
c part(npar)    partials returned by fun				      c
c									      c
c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++c

	
	dimension lsqs(8)

	dimension x(1),y(1),w(1),yc(1)

	dimension p(1),pe(1),ip(1)


	dimension part(1),ps(1),dp(1),yce(1),cm(mxpar,npf),
	1    b(mxpar,npf+1),bm(mxpar,npf+1),
	2    a(mxpar,npf),am(mxpar,npf),wkarea(1)

	real*8 a,b,am,bm,wkarea


	logical swap/.false./,conv/.false./

	parameter (tst=.01,heps=1.e-10)

c&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&c
	
	lun = lsqs(1)
	test=tst*2.**lsqs(4)
	npv = npf
	
c       check for dimension error returns
	
	if(npt.le.npv)then
	   lsqs(1)=4
	   go to 800
	elseif(npar.gt.mxpar)then
	   lsqs(1)=6
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
	   b(j,1)=0.
	   do k=1,npv
	      a(j,k)=0.
	   enddo
	enddo

	xsqr = 0.
	do i=1,npt
	   xv = x(i)
	   call fun(xv,npar,p,yv,part)
	   yc(i) = yv
	   xsqr = xsqr + w(i)*(y(i)-yc(i))**2
	   ss = 0.
	   yce(i) = 0.

c** CALCULATE a and b MATRICES
	   do j=1,npv
	      b(j,1) = b(j,1) + w(i)*(y(i)-yc(i))*part(ip(j))
	      do k=1,npv
		 a(j,k) = a(j,k) + w(i)*part(ip(j))*part(ip(k))
		 ss = ss + part(ip(j))*part(ip(k))*b(j,k+1)
	      enddo
	   enddo

	   if(ss.gt.0.)yce(i) = sqrt(ss)

	enddo			! end of NPT loop

	xsqr = xsqr/(npt-npv)

	if(npv.lt.1)goto 1000	! NO FIT

	if(xsqr.le.0.)goto 800	! FINISHED

	itot = itot + 1

	if(itot.le.1)then
	   do j=1,npar
	      ps(j) = p(j)
	   enddo

	   if(lsqs(8).gt.0 .and. xsqr.le.xcut)then ! check for xsqr cutoff test

c** FAILED xsqr CUTOFF TEST SO VARY ONLY THE WORST PARAMETER lsqs(7)

	      npvs=npv
	      npv=1
	      ips=ip(1)
	      ip(1)=lsqs(7)
	      swap = .true.
	   endif
	elseif(swap .and. (xsqr/xsqrprev.le..9.or.xsqr.le.xcut))then

c** xsqr NOW ok SO RELOAD FREE PARAMETERS AND CONTINUE

	   swap = .false.
	   ip(1)=ips
	   npv=npvs
	endif


c CONVERGENCE TEST if after first iteration

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

c failed convergence if reach here

 700	if(itot.gt.1 .and. xsqrprev.lt.xsqr)then
          itgtot=itgtot+1
	  conv=.false.
c damp as chi increases and try again with previous parameter set
	  do j=1,npar
            p(j)=ps(j)
	  enddo
	  if(swap)then  ! unswap since not working properly
	    itg=itg+1
  	    f=f*10.
	    swap = .false.
	    ic=0
	    npv=npvs
	    ip(1)=ips
	    go to 101  ! go all the way back but dont reset itg to zero
	  end if

	  if(itg.ge.lsqs(5))then
	    lsqs(1)=2  ! max gradient iterations exceded
	    goto 800
	  endif

c reload saved matrices am,bm into a,b
	  itg=itg+1
  	  f=f*10.
	  do j=1,npv
	    b(j,1) = bm(j,1)
	    do k=1,npv
	      a(j,k) = am(j,k)
	      b(j,k+1) = bm(j,k+1)
	    enddo
	    a(j,j) = (1.+f)*a(j,j)
	  enddo
	  goto 200

	elseif(itot.gt.1 .and. xsqrprev.ge.xsqr)then

c undamp as chi decreases and not too many gradient iterations
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
	  if(it.gt.lsqs(5))then  ! max iterations exceded
	    lsqs(1)=1
	    goto 800
	  endif
	endif  ! end of itot.gt.1

c ready to calculate parameter steps

	do j=1,npv
	  if(a(j,j).le.0.)then
c fix jth free param and continue if possible
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
          do k=1,npv
            b(j,k+1)=0.
	  enddo
	  b(j,j+1)=1.
        enddo
  
c save a and b into am and bm and scale a diagonals by 1+f

        do j=1,npv
	  bm(j,1)=b(j,1)
	  do k=1,npv
	    am(j,k)=a(j,k)
	    bm(j,k+1)=b(j,k+1)
	  enddo
	  a(j,j)=(1.+f)*a(j,j)
        enddo

c** SOLVE FOR THE PARAMETER STEPS RETURNED IN COLUMN 1 OF b

c** try IMSL linear equs solver which uses LU decomp and iteration

200	idgt=0
	call leqt2f(a,npv+1,npv,mxpar,b,idgt,wkarea,ier)

c ier.ne.0 returned likely indicates singularity in equations soln
	if(ier.ne.0)then
	  lsqs(1) = 7
	  goto 800
	endif

c old version	call leq(am,bm,npv,npv+1,mxpar,mxpar,det)

	do j=1,npv
	  dp(j)=b(j,1)
          p(ip(j))=p(ip(j))+dp(j)
	enddo

c sign change check

	if(lsqs(6).eq.1 .or. (lsqs(6).eq.2.and.it.le.lsqs(5)/2))then
          do j=1,npv
	    h=1.

   51       if(p(ip(j))*ps(ip(j)).ge.0.)then
	      dp(j)=h*dp(j)
	    else
              h=.5*h
	      if(h.lt.heps)then
	        lsqs(1)=3
c** PARAMETERS INSIST ON CHANGING SIGN SO EXIT
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

c       exit code

 800	if(xsqr.gt.0.)chii = sqrt(xsqr)
	efac = 1.
	if(lsqs(3) .le. 0)efac = chii ! weight param errors by chii if request
	do j=1,npv
	   pe(ip(j)) = efac*sqrt(abs(b(j,j+1)))
	enddo

c	do i=1,npv   ! correlation matrix
c       do j=1,npv
c       cmden=sqrt(abs(b(i,i+1)))*sqrt(abs(b(j,j+1)))
c       cm(i,j)=0.
c       if(cmden.gt.0.)cm(i,j)=b(i,j+1)/cmden
c       enddo
c	enddo

 1000	return
	end


c a = sum( w(i)*gradj(yc(i))*gradk(yc(i)) )
c e = a inverse is the error matrix
c b = sum( w(i)*(y(i)-yc(i))*gradk(yc(i)) )
c dp = b*e solution gives parameter increments dp
c chisqr = sum( w(i)*(y(i)-yc(i))**2 ) / (npt - npv)
c sigmasqr p(j) = e(j,j)

	function chi(npt,npv,y,w,yc)
	dimension y(npt),w(npt),yc(npt)
	red=float(npt)-float(npv)
	chi=0.
	do i=1,npt
          chi=chi+w(i)*(y(i)-yc(i))**2
	enddo
	chi=chi/red
	return
	end



c&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&c
c IMSL routines required to solve linear equations
c error reporting has been removed and no solution refinement is done.
c&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&c


	SUBROUTINE LEQT2F (A,M,N,IA,B,IDGT,WKAREA,IER)

	DIMENSION          A(IA,1),B(IA,1),WKAREA(1)
	DOUBLE PRECISION   A,B,WKAREA,D1,D2,WA
C       FIRST EXECUTABLE STATEMENT
C       INITIALIZE IER
	IER=0
	JER=0
	J = N*N+1
	K = J+N
	MM = K+N
	KK = 0
	MM1 = MM-1
	JJ=1
	DO 5 L=1,N
	   DO 5 I=1,N
	      WKAREA(JJ)=A(I,L)
	      JJ=JJ+1
 5	   CONTINUE
C       DECOMPOSE A
	   CALL LUDATN (WKAREA,N,N,A,IA,IDGT,D1,D2,WKAREA(J),WKAREA(K),
	1	WA,IER)
	   IF (IER.GT.128) GO TO 25
c       IF (IDGT .EQ. 0 .OR. IER .NE. 0) KK = 1
	   IF (IDGT .gt. 0 .OR. IER .NE. 0) KK = 1
	   DO 15 I = 1,M
C       PERFORMS THE ELIMINATION PART OF
C       AX = B
	      CALL LUELMN (A,IA,N,B(1,I),WKAREA(J),WKAREA(MM))
	      DO 10 II=1,N
		 B(II,I) = WKAREA(MM1+II)
 10	      CONTINUE
	      IF (JER.NE.0) GO TO 20
 15	   CONTINUE
	   GO TO 25
 20	   IER = 131
 25	   JJ=1
	   DO 30 J = 1,N
	      DO 30 I = 1,N
		 A(I,J)=WKAREA(JJ)
		 JJ=JJ+1
 30	      CONTINUE
c       IF (IER .EQ. 0) GO TO 9005
c       9000 CONTINUE
c       CALL UERTST (IER,6HLEQT2F)
 9005	      RETURN
	      END

	SUBROUTINE LUDATN (A,IA,N,LU,ILU,IDGT,D1,D2,APVT,EQUIL,WA,IER)
	DIMENSION          A(IA,1),LU(ILU,1),APVT(1),EQUIL(1)
	DOUBLE PRECISION   A,LU,D1,D2,EQUIL,WA,ZERO,ONE,FOUR,SIXTN,SIXTH,
	1    RN,WREL,BIGA,BIG,P,SUM,AI,WI,T,TEST,Q,APVT
	DATA               ZERO,ONE,FOUR,SIXTN,SIXTH/0.D0,1.D0,4.D0,
	1    16.D0,.0625D0/
C       FIRST EXECUTABLE STATEMENT
C       INITIALIZATION
	IER = 0
	RN = N
	WREL = ZERO
	D1 = ONE
	D2 = ZERO
	BIGA = ZERO
	DO 10 I=1,N
	   BIG = ZERO
	   DO 5 J=1,N
	      P = A(I,J)
	      LU(I,J) = P
	      P = DABS(P)
	      IF (P .GT. BIG) BIG = P
 5	   CONTINUE
	   IF (BIG .GT. BIGA) BIGA = BIG
	   IF (BIG .EQ. ZERO) GO TO 110
	   EQUIL(I) = ONE/BIG
 10	CONTINUE
	DO 105 J=1,N
	   JM1 = J-1
	   IF (JM1 .LT. 1) GO TO 40
C       COMPUTE U(I,J), I=1,...,J-1
	   DO 35 I=1,JM1
	      SUM = LU(I,J)
	      IM1 = I-1
	      IF (IDGT .EQ. 0) GO TO 25
C       WITH ACCURACY TEST
	      AI = DABS(SUM)
	      WI = ZERO
	      IF (IM1 .LT. 1) GO TO 20
	      DO 15 K=1,IM1
		 T = LU(I,K)*LU(K,J)
		 SUM = SUM-T
		 WI = WI+DABS(T)
 15	      CONTINUE
	      LU(I,J) = SUM
 20	      WI = WI+DABS(SUM)
	      IF (AI .EQ. ZERO) AI = BIGA
	      TEST = WI/AI
	      IF (TEST .GT. WREL) WREL = TEST
	      GO TO 35
C       WITHOUT ACCURACY
 25	      IF (IM1 .LT. 1) GO TO 35
	      DO 30 K=1,IM1
		 SUM = SUM-LU(I,K)*LU(K,J)
 30	      CONTINUE
	      LU(I,J) = SUM
 35	   CONTINUE
 40	   P = ZERO
C       COMPUTE U(J,J) AND L(I,J), I=J+1,...,
	   DO 70 I=J,N
	      SUM = LU(I,J)
	      IF (IDGT .EQ. 0) GO TO 55
C       WITH ACCURACY TEST
	      AI = DABS(SUM)
	      WI = ZERO
	      IF (JM1 .LT. 1) GO TO 50
	      DO 45 K=1,JM1
		 T = LU(I,K)*LU(K,J)
		 SUM = SUM-T
		 WI = WI+DABS(T)
 45	      CONTINUE
	      LU(I,J) = SUM
 50	      WI = WI+DABS(SUM)
	      IF (AI .EQ. ZERO) AI = BIGA
	      TEST = WI/AI
	      IF (TEST .GT. WREL) WREL = TEST
	      GO TO 65
C       WITHOUT ACCURACY TEST
 55	      IF (JM1 .LT. 1) GO TO 65
	      DO 60 K=1,JM1
		 SUM = SUM-LU(I,K)*LU(K,J)
 60	      CONTINUE
	      LU(I,J) = SUM
 65	      Q = EQUIL(I)*DABS(SUM)
	      IF (P .GE. Q) GO TO 70
	      P = Q
	      IMAX = I
 70	   CONTINUE
C       TEST FOR ALGORITHMIC SINGULARITY
	   IF (RN+P .EQ. RN) GO TO 110
	   IF (J .EQ. IMAX) GO TO 80
C       INTERCHANGE ROWS J AND IMAX
	   D1 = -D1
	   DO 75 K=1,N
	      P = LU(IMAX,K)
	      LU(IMAX,K) = LU(J,K)
	      LU(J,K) = P
 75	   CONTINUE
	   EQUIL(IMAX) = EQUIL(J)
 80	   APVT(J) = IMAX
	   D1 = D1*LU(J,J)
 85	   IF (DABS(D1) .LE. ONE) GO TO 90
	   D1 = D1*SIXTH
	   D2 = D2+FOUR
	   GO TO 85
 90	   IF (DABS(D1) .GE. SIXTH) GO TO 95
	   D1 = D1*SIXTN
	   D2 = D2-FOUR
	   GO TO 90
 95	   CONTINUE
	   JP1 = J+1
	   IF (JP1 .GT. N) GO TO 105
C       DIVIDE BY PIVOT ELEMENT U(J,J)
	   P = LU(J,J)
	   DO 100 I=JP1,N
	      LU(I,J) = LU(I,J)/P
 100	   CONTINUE
 105	CONTINUE
C       PERFORM ACCURACY TEST
	IF (IDGT .EQ. 0) GO TO 9005
	P = 3*N+3
	WA = P*WREL
	IF (WA+10.D0**(-IDGT) .NE. WA) GO TO 9005
	IER = 34
	GO TO 9000
C       ALGORITHMIC SINGULARITY
 110	IER = 129
	D1 = ZERO
	D2 = ZERO
 9000	CONTINUE
C       PRINT ERROR
c       CALL UERTST(IER,6HLUDATN)
 9005	RETURN
	END

	SUBROUTINE LUELMN (A,IA,N,B,APVT,X)
	DIMENSION          A(IA,1),B(1),APVT(1),X(1)
	DOUBLE PRECISION   A,B,X,SUM,APVT
C       FIRST EXECUTABLE STATEMENT
C       SOLVE LY = B FOR Y
	DO 5 I=1,N
 5	   X(I) = B(I)
	   IW = 0
	   DO 20 I=1,N
	      IP = APVT(I)
	      SUM = X(IP)
	      X(IP) = X(I)
	      IF (IW .EQ. 0) GO TO 15
	      IM1 = I-1
	      DO 10 J=IW,IM1
		 SUM = SUM-A(I,J)*X(J)
 10	      CONTINUE
	      GO TO 20
 15	      IF (SUM .NE. 0.D0) IW = I
 20	      X(I) = SUM
C       SOLVE UX = Y FOR X
	      DO 30 IB=1,N
		 I = N+1-IB
		 IP1 = I+1
		 SUM = X(I)
		 IF (IP1 .GT. N) GO TO 30
		 DO 25 J=IP1,N
		    SUM = SUM-A(I,J)*X(J)
 25		 CONTINUE
 30		 X(I) = SUM/A(I,I)
		 RETURN
		 END
