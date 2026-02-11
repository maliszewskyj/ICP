#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/ioctl.h>
#include <linux/lp.h>
#include "serial.h"
#include "rs232.h"
#include "instrext.h"
#include "trace.h"

extern RS232PARM rs232_cnf[NUMRS232];
extern int debug;
static int ez_fd;
#define MAXBUF 4096
static char cmd[80], response[MAXBUF];

/* 
 * Initialize serial port 
 * Get serial port "number" from "ACS" slot
 */
void RS232EZ_INIT(){
  /* We need to eventually get port number from FORTRAN side through commons*/
  int portno;
  char * ptr;
  portno = mot_line[2]; /* Steal port from ACS focus motors */
  ez_fd = 0;
  if (debug) { TRACE("RS232EZ_INIT: ez_m portno = %d device = %s\n",
		     portno,rs232_cnf[portno].dev); }
  if (portno < 0) {
    if ((ptr = getenv("TESTING")) != NULL) {
      fprintf(stderr,"RS232EZ_INIT: fd=-1, no services will be available\n");
      return;
    }
    fprintf(stderr,"RS232EZ_INIT: Can't initialize portno %d\n",portno);
    exit(2);
  }
  if ((ez_fd = serialOpen(rs232_cnf[portno].dev, 
			 9600, 8, 'N',0)) <= 0) {
    fprintf(stderr,"RS232EZ_INIT: open() failed!\n");
    exit(2);
  }
  if (debug) TRACE("RS232EZ_INIT: zeta fd = %d\n",ez_fd);
}

void RS232EZ_END(){
  if (debug) TRACE("RS232EZ_END\n");
  if (ez_fd > 0) close(ez_fd);
  ez_fd = 0;
}

int RS232EZ(char * wrstring, short * lwr, char * rdstring, short * lrd,
	    int * status){
  char talkstring[MAXBUF];
  int  len, n, stat;
  char *ptr,ch;
  
  len = *lwr;
  *rdstring = 0;
  *lrd = 0;

  strncpy(talkstring,wrstring,len);
  talkstring[len++] = '\r';
  talkstring[len]   = 0;

  if (debug&1) { TRACE("RS232EZ(%d): Send ->%s\n",ez_fd,talkstring); }
  /* Make sure we've got nothing hanging around to gum up the works */
  serialFlush(ez_fd); 
  if ((n = write(ez_fd,talkstring,len)) != len) {
    fprintf(stderr,"RS232EZ: Did not write full message!\n");
    *lrd = 0;
    return -1;
  }
  memset(talkstring,0,sizeof(talkstring));
  usleep(100);

  if ((len = terminated_read(ez_fd,talkstring,MAXBUF-1,'\r')) < 0) return -1;
  if (debug&1) { TRACE("RS232EZ: Recv ->%s\n",talkstring); } 

  /* Look for asterisk at beginning of response */
  if ((ptr = strstr(talkstring,"/0")) == NULL) return -1;
  ptr += 2;
  ch = ptr[0];
  ptr++;
  *status = (int)ch;
  len = strlen(ptr);
  strncpy(rdstring,ptr,len);
  *lrd=len;
  rdstring[len]=0;
  if (debug&1) { TRACE("RS232EZ: Status 0x%x Returning %d \"%s\"\n",
		       *status,len,rdstring); }
  return (int)*lrd;
}

char EZAXIS(int * motorno) {
  int addr;
  char ch;
  
  /* Map addresses via old focus motor convention */
  addr = (*motorno > 0) ? (int) vmot3[*motorno-1] : 0;
  ch = '*'; /* Bogus character */
  switch(addr) {
  case 0: ch = '_'; break;
  case 1: ch = '1'; break;
  case 2: ch = '2'; break;
  case 3: ch = '3'; break;
  case 4: ch = '4'; break;
  case 5: ch = '5'; break;
  case 6: ch = '6'; break;
  case 7: ch = '7'; break;
  case 8: ch = '8'; break;
  case 9: ch = '9'; break;
  case 10: ch = ':'; break;
  case 11: ch = ';'; break;
  case 12: ch = '<'; break;
  case 13: ch = '='; break;
  case 14: ch = '>'; break;
  case 15: ch = '?'; break;
  case 16: ch = '@'; break;
  default: ch = '*';
  }
  return ch;
}

int EZSETPOS(int * motorno, float * position) 
{
  short lcmd, lresp;
  int retn, status;
  char ax;

  ax = EZAXIS(motorno);
  sprintf(cmd,"/%cz%dR",ax,(int)*position);
  //sprintf(cmd,"PSET%f",*motorno,*position);
  lcmd = (short) strlen(cmd);
  lresp = 0;
  if ((retn = RS232EZ(cmd,&lcmd,response,&lresp,&status)) < 0) return -1;

  return 0;
}

int EZGETPOS(int * motorno, float * position) 
{
  short lcmd, lresp;
  int retn,status;
  long ipos;
  char *ptr2, ax;

  ax = EZAXIS(motorno);

  sprintf(cmd,"/%c?0",ax);
  lcmd = (short) strlen(cmd);
  lresp = 1;
  if ((retn = RS232EZ(cmd,&lcmd, response, &lresp,&status)) < 0) return -1;
  ipos = strtol(response,&ptr2,10);
  *position = (float) ipos;
  //if ((retn = sscanf(response,"%d",position)) != 1) return -3;
  return 0;
}

int EZGOPOS(int * motorno, float * position) 
{
  short lcmd, lresp;
  int retn, destination,status;
  char ax;

  ax = EZAXIS(motorno);
  destination = (int) *position;

  sprintf(cmd,"/%cA%dR",ax,destination);
  lcmd = (short) strlen(cmd);
  lresp = 0;
  if ((retn = RS232EZ(cmd,&lcmd, response, &lresp,&status)) < 0) return -1;

  return 0;
}

int EZSTATUS(int * motorno, int * moving, int * plus, int * minus, int *home) 
{
  short lcmd, lresp;
  int retn, status;
  char ax;

  ax = EZAXIS(motorno);
  *plus = 0;
  *minus = 0;
  *home = 0;
  *moving  = 0;  
  /* Determine whether axis is moving */
  sprintf(cmd,"/%cQ",ax);
  lcmd = strlen(cmd);
  if ((retn = RS232EZ(cmd,&lcmd, response, &lresp,&status)) < 0) return -1;  
  
  * moving = (status & 0x20) ? 0 : 1;
  if (debug) { printf("EZSTATUS: moving = %d\n",*moving); }
  if (*moving) return 0;

  /* Positive hard limit */
  sprintf(cmd,"/%c?4",ax);
  lcmd= strlen(cmd);
  if ((retn = RS232EZ(cmd,&lcmd, response, &lresp,&status)) < 0) return -1;
  
  retn = atoi(response);
  *home = (int) ((retn & 0x2) >> 2); 
  *plus = (int) ((retn & 0x4) >> 3);

  return 0;
}

int EZHALT(int * motorno) 
{
  short lcmd, lresp;
  int retn, status;
  char ax;

  ax = EZAXIS(motorno);
  sprintf(cmd,"/%cT",ax);
  lcmd = (short) strlen(cmd);
  lresp = 0;
  if ((retn = RS232EZ(cmd,&lcmd, response, &lresp,&status)) < 0) return -1;
  return 0;
}

void EZHOME(int * motorno, int * direction) 
{
  short lcmd, lresp;
  int retn, status;
  char ax;
  
  ax = EZAXIS(motorno);
  /* Should better specify homing parameters */
  if (*direction) {
    printf("EZ motors can't home in the positive direction\n");
    return;
  } else {
    sprintf(cmd,"/%cZ172800R",ax);
  }
  lcmd = (short) strlen(cmd);
  lresp = 0;
  if ((retn = RS232EZ(cmd,&lcmd, response, &lresp,&status)) < 0) return;

}
