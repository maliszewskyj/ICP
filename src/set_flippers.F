#include "icp.h"
	SUBROUTINE Set_Flippers (state)
C	*******************************

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C                                                                             C
C	Routine to change the polarization direction of the flippers.         C
C	Called from: Run_Q_Buf, Run_I_Buf, Run_Polar, Parse_Contr             C
C                                                                             C
C	Beat Schmid, January 1991                                             C
C	Modified by Gayle Greene, April 1991 (Changes made to allow state of  C
C		of only one pair of flippers to be changed.)                  C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C	State       Mono flippers (1 & 2)    Anal flippers (3 & 4)            C
C	 1		OFF			OFF			      C
C	 2		ON			OFF			      C
C	 3		OFF			ON			      C
C	 4		ON			ON			      C
C	 5	        Unchanged		ON			      C
C	 6		Unchanged		OFF                           C
C        7		ON			Unchanged		      C
C	 8		OFF			Unchanged		      C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
	use instr
	use polar
	IMPLICIT    none
	INTEGER*4   flip, ps, state, first_ps, last_ps
	REAL*4      RdAngl, energy, current, dspace(2), posit(2), eval
	REAL*4      SIND
	LOGICAL     f_set(8,2) /.FALSE., .TRUE., .FALSE., .TRUE., 
     1				.FALSE., .FALSE.,.TRUE., .FALSE.,
     1          	        .FALSE., .FALSE., .TRUE., .TRUE.,
     1				.TRUE.,  .FALSE.,.FALSE.,.FALSE./
	   
	
C------------------- READ THE CURRENT MONO AND ANA ANGLES AS NECESSARY --------C

	IF (iabs(nsta) .NE. 7.and. nsta .ne. -5 .and. nsta .NE. -1.and.nsta.ne.-11) THEN
	   if (state.eq.2.or.state.eq.4.or.state.eq.7)
     1	      posit(1) = RdAngl(2) - z(2) !read only if setting mono on
	   if (state.gt.2.and.state.lt.6)
     1	      posit(2) = RdAngl(6) - z(6) !read only if setting anal on
	ELSE ! nsta is -5 or 7 or ng1 or ANDR
	   posit(1) = 2. * 20.51
	   posit(2) = posit(1)
	END IF

	dspace(1) = dm
	dspace(2) = da

C-------------------Select which power supplies are to be set----------------C
	IF (state.lt.5)then !setting all flippers
	  first_ps=1
	  last_ps=4
	ELSE IF (state.eq.5.or.state.eq.6)then !setting only anal flippers
	  first_ps=3
	  last_ps=4
	ELSE IF (state.eq.7.or.state.eq.8)then !setting only mono flippers
	  first_ps=1
	  last_ps=2
        ELSE
          CALL Putcontr_NT('WARNING: Requested state out of bounds!')
	  RETURN
	ENDIF

	DO ps = first_ps,last_ps
	   flip = ps / 3 + 1
	   IF (f_set(state, flip)) THEN
	      eval = ( 2. * dspace(flip) * SIND(posit(flip)/2.))**2
	      IF (eval .EQ. 0.)THEN
		CALL Putcontr_NT
     1		(' WARNING!!!...Flippers NOT set because')
		CALL Putcontr_NT
     1		(' current motor positions would cause division by zero ')
		RETURN
	      ENDIF
	      energy = 81.80310 / eval
	      IF (nsta .EQ. 2 .AND. MOD (ps, 2) .EQ. 0) THEN
	         current = f_on(ps)%cur
	      ELSE 
		IF (f_on(ps)%engy .EQ. 0.)THEN
		  CALL Putcontr_NT
     1		  (' WARNING!!!...Flippers NOT set because energy must')
		  CALL Putcontr_NT
     1		  (' be set to nonzero value. Use FCAL command. ')
		  RETURN
		ENDIF
	        current = f_on(ps)%cur * SQRT( energy / f_on(ps)%engy )
	      END IF
	   ELSE
	      current = 0.
	   END IF
	   CALL Iset (ps, current)
           
	END DO

	END
	
