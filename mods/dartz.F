C  Common for Dartz instrument
	MODULE DARTZ
	INTEGER*4	prpx, menux, xbuf_recl, maxloops, numxmots
	PARAMETER	(xbuf_recl = 80)
	PARAMETER	(maxloops = 6)
	PARAMETER	(numxmots = 6)

	TYPE x_buf
	   CHARACTER*50 com 
	   INTEGER*4	lpn(6)
	   INTEGER*4	lpts(6)
	   INTEGER*4	mn(6) 
	   REAL*4         a(6) 
	   REAL*4         i(6)
	   REAL*4         pts , hf 
	   REAL*4         tmp , it , tw , th0 , ter 
	   REAL*4         mon , mpf 
	   CHARACTER*4  mt 
	   LOGICAL      flip_state(4) 
	   REAL*4         ihf , hfw , hfh , th 
	END TYPE

	INTEGER*4	prpd, menud, dbuf_recl, numdmots, xord
	INTEGER*4	xcoord,ycoord,zcoord,nocoord
	PARAMETER	(dbuf_recl = 336)
	PARAMETER	(numdmots = 6)
	PARAMETER	(xcoord = 1)
	PARAMETER	(ycoord = 2)
	PARAMETER	(zcoord = 3)
	PARAMETER	(nocoord = 0)

	TYPE d_buf
	   CHARACTER*50 com
	   INTEGER*2    dum
	   INTEGER*4	lpn(6)
	   INTEGER*4	lpts(6)
	   INTEGER*4    mn(6)
	   REAL*4       a(6)
	   REAL*4       i(6)
	   INTEGER*4	xyz(6)
	   REAL*4       a_xyz(6)
	   REAL*4       i_xyz(6)
	   REAL*4       pts , hf , phi , psi, phi_inc,psi_inc
	   REAL*4       tmp , it , tw , th0 , ter 
	   REAL*4       mon , mpf 
	   CHARACTER*4  mt 
	   LOGICAL      flip_state(4)
	   REAL*4       ihf , hfw , hfh , th 
	END TYPE
	END MODULE DARTZ
