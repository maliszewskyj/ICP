#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include "serial.h"
#include "rs232.h"
#include "instrext.h"
#include "trace.h"
extern RS232PARM rs232_cnf[NUMRS232];

int acs_fd=0;
int acsf_fd=0;

/* 
 * Initialize serial port 
 */
void RS232_INIT()
{
  /* Get port number from FORTRAN side through "instr" common block */
  int portno;
  char * ptr;
  portno = mot_line[0]; 
  if (debug) { TRACE("RS232_INIT: acs portno = %d device = %s\n",
		     portno,rs232_cnf[portno].dev); }
  if (portno < 0) {
    if ((ptr = getenv("TESTING")) != NULL) {
      fprintf(stderr,"RS232_INIT: fd=-1, no services will be available\n");
      return;
    }
    fprintf(stderr,"RS232_INIT: Can't initialize portno %d\n",portno);
    exit(2);
  }
  if ((acs_fd = serialOpen(rs232_cnf[portno].dev, 
			   rs232_cnf[portno].baud, 
			   rs232_cnf[portno].bits,
			   rs232_cnf[portno].parity,0)) < 0) {
    fprintf(stderr,"RS232_INIT: open() failed!\n");
    exit(2);
  }
  if (debug) { TRACE("RS232_INIT: acs_fd = %d\n",acs_fd); }
}

void RS232END()
{
  if (acs_fd > 0) close(acs_fd);
  acs_fd = 0;
}

void RS232(char * wrstring, int * lwr, char * rdstring, int * lrd)
{
  char talkstring[80];
  int  len, n;
  
  len = *lwr;
  *lrd = 0;
  if (acs_fd <= 0) return;
  strncpy(talkstring,wrstring,len);
  talkstring[len++] = '\r';
  talkstring[len]   = 0;

  if (debug&1) { TRACE("RS232: Send ->%s\n",talkstring); }
  if ((n = write(acs_fd,talkstring,len)) != len) {
    fprintf(stderr,"RS232: unable to write full string!\n");
    return;
  }

  if ((len = terminated_read(acs_fd,talkstring,80,'\r')) < 0) {
    /* If an error occurs */
    if (debug&1) { TRACE("RS232: Error = %d\n",len); }
    *rdstring = 0;
    *lrd = 0;
    serialFlush(acs_fd);
    return;
  }
  if (debug&1) { TRACE("RS232: Recv ->%s\n",talkstring); }
  strncpy(rdstring,talkstring,len);
  *lrd = len-1;
  rdstring[len-1]=0;
  return;
}

/* 
 * Initialize serial port 
 */
void RS232F_INIT()
{
  /* Get port number from FORTRAN side through "instr" common block */
  int portno;
  char * ptr;
  portno = mot_line[2]; 
  if (debug) { TRACE("RS232F_INIT: acs portno = %d device = %s\n",
		     portno,rs232_cnf[portno].dev); }
  if (portno < 0) {
    if ((ptr = getenv("TESTING")) != NULL) {
      fprintf(stderr,"RS232F_INIT: fd=-1, no services will be available\n");
      return;
    }
    fprintf(stderr,"RS232F_INIT: Can't initialize portno %d\n",portno);
    exit(2);
  }
  if ((acsf_fd = serialOpen(rs232_cnf[portno].dev, 
			    rs232_cnf[portno].baud, 
			    rs232_cnf[portno].bits,
			    rs232_cnf[portno].parity,0)) < 0) {
    fprintf(stderr,"RS232F_INIT: open() failed!\n");
    exit(2);
  }
  if (debug) { TRACE("RS232F_INIT: acsf_fd = %d\n",acsf_fd); }
}

void RS232F_END()
{
  if (acsf_fd > 0) close(acsf_fd);
  acsf_fd = 0;
}

void RS232F(char * wrstring, int * lwr, char * rdstring, int * lrd)
{
  char talkstring[80];
  int  len, n;
  
  len = *lwr;
  *lrd = 0;
  talkstring[0] = '#';
  strncpy(talkstring+1,wrstring,len++);
  talkstring[len++] = '\r';
  talkstring[len]   = 0;

  if (debug&1) { TRACE("RS232F: Send ->%s\n",talkstring); }
  serialFlush(acsf_fd);
  if ((n = write(acsf_fd,talkstring,len)) != len) {
    fprintf(stderr,"RS232F: unable to write full string!\n");
    return;
  }

  if ((len = terminated_read(acsf_fd,talkstring,80,'\r')) < 0) {
    /* If an error occurs */
    if (debug&1) { TRACE("RS232F: Error = %d\n",len); }
    *rdstring = 0;
    *lrd = 0;
    if (debug&1) { TRACE("RS232F: Reopen\n"); }
    serialFlush(acsf_fd);
    return;
  }
  if (debug&1) { TRACE("RS232F: Recv ->%s\n",talkstring); }

  /* strip off leading # and trailing \r */
  strncpy(rdstring,talkstring+1,len-1);
  *lrd = len-2;
  rdstring[len-1]=0;
  return;
}

void RS232F_MOVING(char * wrstring, int * lwr, char * rdstring, int * lrd)
{
  RS232F(wrstring, lwr, rdstring, lrd);
}


