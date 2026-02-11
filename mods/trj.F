	MODULE trj
	character*50 comments
	integer*2    dum
	integer      npts, nmots
	real*4       mon, mpf
	character*4  mt
	integer      axes(6)        ! Numbers of axes to move
	real         angles(6,2000)	! space for each angle in trajectory
	END MODULE trj
