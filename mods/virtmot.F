#include "icpdef.h"
	MODULE virtmot
	INTEGER*4  numreal(MAXMOTS)  !# of real mots for this virtmot
	INTEGER*4  virtpos(MAXMOTS)  !virt pos for which reals last calculated
	INTEGER*4  pntlist(MAXMOTS)  !pointer in lists for this virtmot
	INTEGER*4  motlist(MAXMOTS)  !real mot numbers
	REAL*4	   poslist(MAXMOTS)  !calculated positions of real motors
C  If motor 23 is a virtual motor causing motors 11, 12, & 13 to move, then
C     numreal(23) <- 3   !3 motors are represented by virt mot #23
C     virtpos(23) <- 264 !or whereever you want this virt mot moved
C     pntlist(23) <- 1   !displacement in motlist & poslist where motors
C				associated with virt #23 are listed
C     motlist(1) <- 11   !real motors numbers associated with virt mot 23
C     motlist(2) <- 12
C     motlist(3) <- 13
C     poslist(1) <- 180  !calculated position for motor 11
C     poslist(2) <- 275  !calculated position for motor 12
C     poslist(3) <-  90  !calculated position for motor 13
C  If motor 24 is a virtual motor causing motors 14 and 15 to move, then
C     numreal(24) <- 2    !2 motors are represented by virt mot #24
C     virtpos(24) <- 320  !or whereever you want this virt mot "moved"
c     pntlist(24) <- 4    !displacement in motlist & poslist where
c			  motors associated with virt #24 are listed
C     motlist(4)  <- 14   !real motor numbers associated with virt mot 24
C     motlist(5)  <- 15  
C     poslist(4)  <- 217  !calculated position for motor 14
C     poslist(5)  <- 333  !calculated position for motor 15
	REAL*4	vm_a(3,3), vm_a_inv(3,3) 	!coeffs for virt motors
	END MODULE virtmot
