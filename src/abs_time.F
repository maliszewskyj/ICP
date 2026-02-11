	subroutine ABS_TIME(isecs)

        implicit none 
	integer*4 isecs

        integer time, a1
        a1=time()
	isecs = a1

	return
	end
