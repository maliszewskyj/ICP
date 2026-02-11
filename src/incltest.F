#include "icp.h"
#include "icp.h"
	include 'options_ext.h'
C inctest.src
	implicit none
	include INSTR_CMN   
	type *,INSTR_CMN
	open(9,file=INSTR_CFG,status='old')
	read(unit=9,fmt='(a)')header
	read(unit=9,fmt=*)nsta
	close(unit=9)
	type *,' header: '//header
	type *,' nsta: ',nsta
      end
