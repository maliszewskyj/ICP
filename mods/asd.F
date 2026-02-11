	MODULE ASD
	INTEGER*4    asd_addr  !gpib address
	LOGICAL	     use_asd   !using asd (true) or not (false)
	INTEGER*4    noslices  !number of slices to read
	INTEGER*4    asd_xmin, asd_ymin, asd_xmax, asd_ymax
	INTEGER*4    asd_numx, asd_numy
	INTEGER*4    asd_xdim, asd_ydim
	INTEGER*4    slices(256,4)
	REAL*4	     asd_data(256)
	INTEGER*4    asd_storage(369664)  ! size to BNL maximum
	INTEGER*4    asd_rawdata(608,608) ! size to BNL maximum
	LOGICAL	     use_asdfile,keep_rawasd
	REAL*4	     fpwindow
	END MODULE ASD
