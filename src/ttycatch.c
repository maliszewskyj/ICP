#include <errno.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
//#include "signalext.h"
volatile int sigint;
volatile int sigquit;
volatile int sigtstp;
volatile int sigterm;

extern int debug;
/*
 * Absoft FORTRAN common blocks can be accessed by declaring the global
 * structure "_Ccommon"
 */
/*
struct {
  volatile int sigint;
  volatile int sigquit;
  volatile int sigtstp;
  volatile int sigterm;
} _CICPSIG;
*/

/*
 *  Your basic signal handler: 
 *       read the value of the signal and act accordingly
 */
void sighandler(int signum) {
  switch(signum){
  case SIGINT:
    if (debug) printf("Received SIGINT.\n");
    sigint = 1;
    break;
  case SIGQUIT:
    if (debug) printf("Received SIGQUIT.\n");
    sigquit = 1;
    break;
  case SIGTSTP:
    if (debug) printf("Received SIGTSTP.\n");
    sigtstp = 1;
    break;
  case SIGUSR1:
    debug = (debug) ? 0 : 1;
    printf("Received SIGUSR1. Going to debug level %d\n",debug);
    break;
  case SIGSEGV:
    printf("Segmentation Fault. Stopping now before I make things worse.\n");
    exit(1);
    break;
  case SIGTERM:
    if (debug) printf("Received SIGTERM. Exiting...\n");
    system("stty sane");
    sigterm = 1;
    exit(1);
    break;
  default:
    if (debug) printf("Unsupported signal: %d\n",signum);
  }
}

/*
 * FORTRAN wrapper function for signal handler
 * Because of symbol name mangling, we need to upcase this function name
 *
 */
int CATCH() 
{
  struct sigaction sa;
  int pid;

  memset(&sa,0,sizeof(sa));
  sa.sa_handler = sighandler;
  pid = (int) getpid();
  /* printf("Process started with PID = %d\n",pid); */

  if (sigaction(SIGTERM,&sa,NULL)) return -SIGTERM;
  if (sigaction(SIGSEGV,&sa,NULL)) return -SIGSEGV;
  if (sigaction(SIGINT,&sa,NULL))  return -SIGINT;

  sa.sa_flags = SA_RESTART;
  if (sigaction(SIGQUIT,&sa,NULL)) return -SIGQUIT;
  if (sigaction(SIGTSTP,&sa,NULL)) return -SIGTSTP;
  if (sigaction(SIGUSR1,&sa,NULL)) return -SIGTERM;

  return pid;
}

