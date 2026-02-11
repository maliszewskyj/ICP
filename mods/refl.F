C Refl.cmn: common for reflectometer subroutines
	MODULE refl
	REAL*4  rxPG,	!monochromator to PG filter: horizontal 
     1		rxBe,	!monochromator to Be filter: horizontal 
     1		rxs1,	!monochromator to slit 1: horizontal 
     1		rxp1,	!monochromator to shield wall clevis point: horizontal 
     1		rzp1,	!monochromator to shield wall clevis point: vertical    
     1		rrp,	!floating pivot radius on shield wall track  
     1		rthetap,	!floating pivot angle at alpha=0 on shield wall track 
     1		rxp2,	!monochromator to front flight tube clevis: horizontal
     1		rxt,	!monochromator to sample table: horizontal         
     1		rxp3,	!monochromator to detector clevis point: horizontal
     1		rh1,	!Height of wheel center above floating clevis #2
     1		rh2,	!Height of beam above wheels at alpha = 0.      
     1		rd2,	!Distance from wheel midpt to slit 2            
     1		rd3,	!Distance from wheel midpt to slit 3            
     1		rd4	!Distance from rear fixed clevis to slit 4     
	INTEGER*4     PGlist(9),
     1		PGmove, !number of motors to scanned in rbuf if PG
     1		Belist(8),
     1		Bemove, !number of motors to scanned in rbuf if Be
     1	        slitlist(4) !motors controlling slit opening
	INTEGER*4  prpr,menur
	LOGICAL	   pgaa

        TYPE r_buf
	   CHARACTER*50 com 
	   INTEGER*2    dum
 	   REAL*4       pts
	   REAL*4	qx,qxi,qZ,qZi
	   REAL*4       scan_mode
	   REAL*4       s(4)
	   REAL*4       i(4)
	   REAL*4       tmp , it , tw , th0 , ter 
	   REAL*4       hf , mon0 , mon1,exp,mpf 
	   CHARACTER*4  mt
	   LOGICAL      flip_state(4)
	   INTEGER*4	numx !number of xtra motor
	   REAL*4       sx,ix
	   REAL*4       ihf , hfw , hfh , th 
	END TYPE

	END MODULE refl

