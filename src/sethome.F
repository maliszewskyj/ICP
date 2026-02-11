#include "icp.h"
	Subroutine Sethome
	use homedir
	CHARACTER*80  confdir0
  	integer		retn,system

	CALL ENVHOME(confdir,retn)
	confdir0 = confdir(1:retn)//'/cfg/'
	confdir = trim(confdir0)
	return
	end

	CHARACTER*80 FUNCTION Gethome()
	use homedir

	Gethome = confdir
 	return
	end
