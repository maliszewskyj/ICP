	module ad201
	INTEGER*4  ad2point
	INTEGER*4  maxad2
	PARAMETER  (maxad2 = 30)

	INTEGER*4  adnumchan,chanarr(6)
	real*4  advolts(6,maxad2)
	character*12 ad2file

	end module ad201
