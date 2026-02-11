	MODULE lia
	INTEGER*4  prfpoint
	INTEGER*4  maxprf
	PARAMETER  (maxprf = 20)
	INTEGER*4  scanlia
	REAL*4    rlia_x(maxprf),rlia_y(maxprf),
     1		  rlia_of(maxprf),rlia_oa(maxprf),rlia_adc1(maxprf),
     1		  rlia_adc2(maxprf),rlia_adc3(maxprf),rlia_adc4(maxprf)
	CHARACTER*12  liafile
	END MODULE lia
