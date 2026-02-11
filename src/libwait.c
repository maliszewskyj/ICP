/*
 * Replacement for broken wait() function in absoft compiler
 */

#include <stdio.h>
#include <unistd.h>
#include <math.h>
#include <string.h>
#include <time.h>
#include "signalext.h"

extern int debug;
void LIBWAIT(float * fracsec, int * status) 
{

#ifdef SLEEP
  int ticks;
  if (debug & 1) printf("LIBWAIT %f\n",*fracsec);
  ticks = (int) (*fracsec * 1e6);
  usleep(ticks);
#else
  int retn;
  struct timespec t,t_rem;
  float fraction;
  
  if (debug & 1) printf("LIBWAIT %f\n",*fracsec);
  t.tv_sec = (time_t) floor(*fracsec);
  fraction = *fracsec - (float) t.tv_sec;
  t.tv_nsec = (long) 1e9 * fraction;

  retn = nanosleep(&t, &t_rem);
  while (retn < 0) {
    /* Could check to see that we've caught a signal */
    if (sigint) {
      *status = 0;
      return;
    }
    if (debug & 1) printf("LIBWAIT resuming %d seconds %ld ns\n",
			  (int) t_rem.tv_sec, (long) t_rem.tv_nsec);
    memcpy(&t,&t_rem,sizeof(struct timespec));
    retn = nanosleep(&t,&t_rem);
  }
  
#endif
  *status = 1;
}
