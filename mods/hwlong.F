#include "icpdef.h"
      MODULE hwlong
      REAL*4	    cntrate     ! Monitor counts/sec at last check
      REAL*4	    monang      ! angle of monochromator at last mrat
      REAL*4	    dlast       ! dspacing (ie: which mono) at last mrat
      LOGICAL	    motovr, cnstovr ! true if to use motor &/or const overrheads
      REAL*4	    hwlconst,hwlmwait(MAXMOTS) !vals for overhead
      REAL*4	    chw1,chw2,chw3,chw4 !constants read in for howlong calc
      END MODULE
