#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include "serial.h"
#include "rs232.h"
#include "instrext.h"
#include "trace.h"
extern RS232PARM rs232_cnf[NUMRS232];

int pc_fd=0;

/* 
 * Initialize serial port 
 */
void RS232PC_INIT()
{
  /* Get port number from FORTRAN side through "instr" common block */
  int portno;
  char * ptr;
  portno = mot_line[1]; 
  if (debug) { TRACE("RS232PC_INIT: portno = %d device = %s\n",
		     portno,rs232_cnf[portno].dev); }
  if (portno < 0) {
    if ((ptr = getenv("TESTING")) != NULL) {
      fprintf(stderr,"RS232PC_INIT: fd=-1, no services will be available\n");
      return;
    }
    fprintf(stderr,"RS232PC_INIT: Can't initialize portno %d\n",portno);
    exit(2);
  }
  if ((pc_fd = serialOpen(rs232_cnf[portno].dev, 
			   rs232_cnf[portno].baud, 
			   rs232_cnf[portno].bits,
			   rs232_cnf[portno].parity,0)) < 0) {
    fprintf(stderr,"RS232PC_INIT: open() failed!\n");
    exit(2);
  }
  if (debug) { TRACE("RS232PC_INIT: pc_fd = %d\n",pc_fd); }
}

void RS232PCEND()
{
  if (pc_fd > 0) close(pc_fd);
  pc_fd = 0;
}

int RS232PC(char * wrstring, int * lwr, char * rdstring, int * lrd)
{
  char talkstring[80];
  int  len, n;
  
  len = *lwr;
  *lrd = 0;
  if (pc_fd <= 0) return 0;
  strncpy(talkstring,wrstring,len);
  talkstring[len++] = '\r';
  talkstring[len]   = 0;

  if (debug&1) { TRACE("RS232PC: Send ->%s\n",talkstring); }
  /* Flush file descriptor before initiating transaction */
  serialFlush(pc_fd);
  if ((n = write(pc_fd,talkstring,len)) != len) {
    fprintf(stderr,"RS232PC: unable to write full string!\n");
    return -1;
  }

  if ((len = terminated_read(pc_fd,talkstring,80,'\r')) < 0) {
    /* If an error occurs */
    if (debug&1) { TRACE("RS232PC: Error = %d\n",len); }
    *rdstring = 0;
    *lrd = 0;
    serialFlush(pc_fd);
    return -1;
  }
  if (debug&1) { TRACE("RS232PC: Recv ->%s\n",talkstring); }
  strncpy(rdstring,talkstring,len);
  *lrd = len-1;
  rdstring[len-1]=0;
  return (int)*lrd;
}

int RS232PC_MOVING(char * wrstring, int * lwr, char * rdstring, int * lrd){
  return RS232PC(wrstring, lwr, rdstring, lrd);
}

