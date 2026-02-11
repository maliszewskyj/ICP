/* 
 * Routines for printing debugging statements
 *
 * Author: N. C. Maliszewskyj, NIST Center for Neutron Research, Apr 2002
 * $Id$
 */
#include <stdio.h>
#include <string.h>
#include <time.h>

int debug = 0;

void TIMESTAMP() {
  time_t t;
  struct tm *tm;

  time(&t);
  tm = localtime(&t);
  printf("%02d:%02d:%02d :",tm->tm_hour,tm->tm_min,tm->tm_sec);
}

int DTRACE(char * msg, int * lmsg) {
  char outbuf[256];

  if (!debug) return 0;

  memset(outbuf,0,sizeof(outbuf));
  if (lmsg == NULL) { 
    strcpy(outbuf,msg);
  } else { 
    strncpy(outbuf,msg,*lmsg); 
  }
  TIMESTAMP(); 
  puts(outbuf);
  return 0;
}

void DEBUGLEVEL(int *mode) 
{
  debug = *mode;
}

void DEBUGVAL(int * mode) 
{
  *mode = debug;
}

