#include "icp.h"
	subroutine sendpacket(outb,reply,lreply,getreply)
C
C	Gayle Greene, Feb., 1998
C        at call: lreply has length of respected reply if getreply = true
C
	IMPLICIT NONE

	character*80 reply,command
	integer*4 lreply,lcom
	integer*4 outb(2), cksum
	logical getreply


	command(1:1)=CHAR(outb(1))
	command(2:2)=CHAR(outb(2))
	cksum = iand( outb(1)+outb(2) , 255 )
	command(3:3)=CHAR(cksum)

	lcom = 3

	call ad232(command,lcom,reply,lreply,getreply)

	RETURN
	END
