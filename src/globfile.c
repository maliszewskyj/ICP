/*
 * UNIX-specific routines for finding filenames matching a pattern 
 * Author:  N. C. Maliszewskyj
 * Version: $ $
 */

#include <stdio.h>
#include <string.h>
#include <glob.h>
#include "trace.h"

int gl_active=0;
int gl_idx=0;
glob_t globbuf;

int GLOBFILE(char * pattern, int *lpat, char * retfile, int * lret, int * status) 
{
  int retn;
  int i, vlen;
  char scratch[80];

  globfree(&globbuf);
  gl_active=0;

  memset(scratch,0,sizeof(scratch));
  strncpy(scratch,pattern,*lpat);
  if (debug) TRACE("GLOBFILE: pattern= %s\n",scratch);

  if ((retn = glob(scratch, 0, NULL, &globbuf)) != 0) {
    switch(retn) {
    case GLOB_NOSPACE: fprintf(stderr,"GLOB_NOSPACE\n");  break;
    case GLOB_ABORTED: fprintf(stderr,"GLOB_ABORT\n");    break;
    case GLOB_NOMATCH: /* printf("No matches found\n");*/ break;
    default:           fprintf(stderr,"GLOB ERROR %d\n",retn);
    }
    *status = 0;
    return -1;
  }
  /*  gl_active = 1;*/

  if (globbuf.gl_pathc) {
    strcpy(retfile,globbuf.gl_pathv[globbuf.gl_pathc-1]);
    if (debug) TRACE("GLOBFILE: match = %s\n",retfile);
    /* Pad remainder with spaces for FORTRAN */
    vlen = strlen(retfile);
    for (i=vlen;i<*lret;i++) {
      retfile[i]=' ';
    }
    *status = 1;
  } else {
    *status = 0;
    return -1;
  }
  return 0;
}

int GLOBEND() 
{
  if (gl_active) globfree(&globbuf);
  gl_active=0;
  return 0;
}
