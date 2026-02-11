#include "icp.h"
	subroutine FocA_Energy(efa,enfa_range,posit)
C	_________________________________________________________________
c	|								|
c	| CALCULATE THE SOFT ANGLES OF THE num_focus FA MOTORS AND MOTOR 5 AND  |
C	| MOTOR 6 SO THAT THE      					|
C	| ENERGY IS FOCUSED. (Modified from fa_cal.for) 		|
C	|								|
C	| Created By	-	Shaolong Ma		2/25/94		|
c	| Modified by		Collin Broholm		8/10/98		|
c	| 	to return target soft angle for A6 not hard angle	|	
c	| 								|
c	| THE MOTOR # IS ASSIGNED AS,					|
c	|	F1	-	closest to the detector,		|
c	|			its phi_i value is < 0, and is min	|
c	|	F6	-	at the ctr. Its phi_i value is = 0	|
c	|	F11	-	closest to the sample,			|
c	|			its phi_i value is > 0, and max		|
c	|								|
c	| ** look at SPECTWD.PRO FIGIURE in the folder			|
c	|								|
c	| INPUT PARAMETERS,						|
c	|	efa	:	Ef, the energy to be focused on	or the 	|
c	|			ctr of the FA energy			|
c	|   enfa_range	:	the range of the FA energy.		|
c	|   posit(5)	:	angle of the motor 5, its zero when 	|
c	|			the FA from is along the SA line	|
c	| OUTPUT PARAMETERS,						|
c	|   motor positions in degrees:                                 |
c       |   posit(5)    :       position calculated for motor 5         |
c       |   posit(6)      :       position calculated for motor 6       |
c       |   posit(first_focus->first_focus+num_focus-1     : positions 
C       |                                 calcualted for all foc motors |
c	|		the num_focus elements array for the angles of FA	|
c	|		motors relative to the FA fram	and		|
c	|		the posit(5) that tangent to the circle.		|
c	|								|
c	| **NOTE							|
c	|	to optimize the space focusing of beams at the detector	|
c	|	the assignment of energy of the FA motors are		|
c	|		Ef1	=	efa+.5*enfa_range		|
c	|		Efi	=      efa-.5*enfa_range+.1*i*enfa_range|
c	|		Ef11	=	efa+.5*enfa_range		|
c	|								|
c	|_______________________________________________________________|
C       | INTERNAL VARIABLES                                            |
c	|								|
c	|  		alfa, 	tangent of circle to analyzer fram	|
c	|			POSITIVE dir is opposit dir of a6	|
c	| 		xks ,	angle from OA(radius) to SA(or SA0)	|
c	|								|
c	| the circle goes through the sample, AC-center	and detector D.	|
c	_________________________________________________________________
c	|								|
c	| phi    : which is from SA line to SBi 	        	|
c	|				(sample to blade i)		|
c	|fd_acai: distance from ana-fram-ctr to ctr-of-blade_i		|
c	| dsa	: sample to analyzer-ctr distance 			|
c	| alfa	: the angle from the tangent of the  circle to the 	|
c	|	  fram of the analyzer. 				|
c	| xks	: angle from SA to OA, O is the radius of the circle	|
c	|_______________________________________________________________|
c	_________________________________________________________________
c	|								|
c	| FI_ANG: THE ANGLE THAT EACH FA BLADE NEEDS TO TURN RELATIVE	|
c	| TO THE FA FRAM (converted from spectgeo.pro)			| 
c	|								|
c	|	efa_i,	THE Ef ENERGY FOR THE ith FA CRYSTAL		|
c	|	fd_acai,THE DISTANCE FROM THE ith FA CRYSTAL TO THE CTR	|
c	|		ONE.						|
c	|_______________________________________________________________|
	use instr
	use focus

	implicit 	none


        real*4          efa,enfa_range
        real*4		fa_angx,fa_thitaf,phi,phi_i,thita_i
        real*4		fa_ang(11),thita_c,angg_c,ang_asaap_tancir
        real*4		fd_axs,pai
	integer*2	nana,na_c,iana
	real*4		alfa,ang_asaap,xks
	real*4		efa_i,fd_acai,fi_ang
	real*4 		dsa,dad,rdangl
	integer*4	imot
	
	real*4		posit(MAXMOTS)

	data    nana/num_focus/,fd_axs/2.1/
	data 	pai/3.14159265/
c        data    dsa/92.4/,dad/33.5/       !can be input from ICP control mode

	dsa = fdsa
	dad = fdad


C-----------------Find position of motor 5---------------------------------C
	thita_c		=	fa_thitaf(efa)
	angg_c		=	pai-2*thita_c
	xks		=	fa_angx(dsa,dad,angg_c)	        !OA SA angle
	ang_asaap_tancir=	.5*pai-xks
	posit(5)		=	ang_asaap_tancir*180/pai
	if (fixed(5))	posit(5) =    Rdangl(5)-z(5)
 	alfa		=	ang_asaap_tancir-posit(5)*pai/180.
	ang_asaap	=	posit(5)*pai/180.
	na_c		=	NINT(nana/2.)

C---------------Find position of focus motors-------------------------------C
	do iana	=	1, nana
                 efa_i	=     efa+.5*enfa_range-(iana-1.0)*enfa_range/(nana-1.)
		fd_acai	=	(iana-na_c)*fd_axs
c	        _________________________________________
c	        |				 	|
c	        | get phi, angle betw S-A0 & S-Ai	|
c	        | phi of f1 should be negative		|
c	        |_______________________________________|
	        if (fd_acai.eq.0.) then
                  phi     = 0 
	        else
       		  phi	= atan(cos(alfa+xks)/(-1.*sin(alfa+xks)+dsa/fd_acai))
	        endif
	        phi_i		=	phi
	        thita_i		=	fa_thitaf(efa_i)	!same as a5	
	        fi_ang	=	ang_asaap+phi_i-thita_i
               fa_ang(iana)    = fi_ang*180./pai
	       imot = iana + first_focus - 1 !imot<-actual motor number
	       posit(imot)   = fa_ang(iana)
	ENDDO



C-----------------------Find position of MOTOR 6 ----------------------------C
	posit(6)		=	2.*thita_c*180./pai

C----------------------------------------------------------------------------C
	return
	end



	real*4 function fa_angx(a,b,omiga)
c	_________________________________________________________________
c	|								|
c	| omiga is angle between a and b in a triangle inside 		|
c	| a circle. the function return the angle between a side	|
c	| and the redius from center to a b joint			|
c	|_______________________________________________________________|
	real*4 a,b,omiga
	fa_angx	=	atan((b/a-cos(omiga))/sin(omiga))

	end


	real*4 function fa_thitaf(e)
c	_________________________________________________________
c	|							|
c	| return the value of thita, which is half of 2-thita	|
c	| in degree.						|
c	| Assuming the lattice parameter of PG(002), which is 	|
c	| 2*!pi/3.3542=1.87325					|
c	| input : e, the energy of the neutron			|
c	| Constant corrected 8/10/98 by Collin Broholm		| 
c	|_______________________________________________________|
	real*4 		ftau_ana
	real*4		e
	data ftau_ana/1.87325/

	fa_thitaf	=	asin(ftau_ana/(2*sqrt(e/2.0717)))

	end

