C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C     POLFIG.cmn:                                                             C
C                                                                             C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
	MODULE polfig
	integer*4 maxpols
	parameter (maxpols=100)
	TYPE P_buf
	   CHARACTER*50 com !comment
	   CHARACTER*50 INFILE
	   CHARACTER*5  outname
	   REAL*4       hf 
	   REAL*4       tmp , th 
	   REAL*4       mon 
	   CHARACTER*4  mt
	END TYPE
C  Variables unique to polfig buffer
	integer*4 iph(maxpols),ipk(maxpols),ipl(maxpols)
	integer*4 mpre(maxpols),numscan,mlist(4)/4,3,8,7/ !order in which motors are moved
	real*4 tth(maxpols),memposit(4) !motor order is 4,3,8,7
	integer*4 ispeed, npol
	END MODULE polfig
