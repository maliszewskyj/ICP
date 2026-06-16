/* 
 * Routines for printing debugging statements
 *
 * Author: N. C. Maliszewskyj, NIST Center for Neutron Research, Apr 2002
 * $Id: trace.c,v 1.3 2017/09/06 12:24:52 nickm Exp $
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

void ISOTIME(int *tstamp)
{
  time_t t;
  struct tm *tm;
  time(&t);
  tm = localtime(&t);

  sprintf(tstamp,"%04d%02d%02d%02d%02d%02d",(1900+tm->tm_year),(1+tm->tm_mon),
	  tm->tm_mday,tm->tm_hour,tm->tm_min,tm->tm_sec);
  //printf("TIMESTAMP = %s\n",tstamp);
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

