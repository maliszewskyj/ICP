#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/ioctl.h>
#include <linux/lp.h>
#include <ctype.h>
#include "serial.h"
#include "rs232.h"
#include "instrext.h"
#include "trace.h"

extern RS232PARM rs232_cnf[NUMRS232];
extern int debug;
static int z_fd;
#define MAXBUF 4096
static char cmd[80], response[MAXBUF];

/* 
 * Initialize serial port 
 * Get serial port "number" from "ACS" slot
 */
void RS232Z_INIT()
{
  /* We need to eventually get port number from FORTRAN side through commons*/
  int portno;
  char * ptr;
  portno = mot_line[0];
  z_fd = 0;
  if (debug) { TRACE("RS232Z_INIT: zeta portno = %d device = %s\n",
		     portno,rs232_cnf[portno].dev); }
  if (portno < 0) {
    if ((ptr = getenv("TESTING")) != NULL) {
      fprintf(stderr,"RS232Z_INIT: fd=-1, no services will be available\n");
      return;
    }
    fprintf(stderr,"RS232Z_INIT: Can't initialize portno %d\n",portno);
    exit(2);
  }
  if ((z_fd = serialOpen(rs232_cnf[portno].dev, 
			 rs232_cnf[portno].baud, 
			 rs232_cnf[portno].bits, 
			 rs232_cnf[portno].parity,0)) <= 0) {
    fprintf(stderr,"RS232Z_INIT: open() failed!\n");
    exit(2);
  }
  if (debug) TRACE("RS232Z_INIT: zeta fd = %d\n",z_fd);
}

void RS232Z_END()
{
  if (debug) TRACE("RS232Z_END\n");
  if (z_fd > 0) close(z_fd);
  z_fd = 0;
}

int RS232Z(char * wrstring, short * lwr, char * rdstring, short * lrd, int getreply)
{
  char talkstring[MAXBUF];
  int  len, n;
  
  len = *lwr;
  *rdstring = 0;
  *lrd = 0;

  strncpy(talkstring,wrstring,len);
  talkstring[len++] = '\r';
  talkstring[len]   = 0;

  if (debug&1) { TRACE("RS232Z(%d): Send ->%s\n",z_fd,talkstring); }
  /* Make sure we've got nothing hanging around to gum up the works */
  serialFlush(z_fd); 
  if ((n = write(z_fd,talkstring,len)) != len) {
    fprintf(stderr,"RS232Z: Did not write full message!\n");
    *lrd = 0;
    return -1;
  }
  memset(talkstring,0,sizeof(talkstring));
  usleep(500);

  if ((len = terminated_read(z_fd,talkstring,MAXBUF-1,'\n')) < 0) return -1;
  if (debug&1) { TRACE("RS232Z: Recv(echo) ->%s\n",talkstring); } 
  if (!getreply) return 0;
  if ((len = terminated_read(z_fd,talkstring,MAXBUF-1,'\r')) < 0) return -1;
  if (debug&1) { TRACE("RS232Z: Recv ->%s\n",talkstring); } 

  /* Look for asterisk at beginning of response */
  strncpy(rdstring,talkstring,len);
  *lrd = len-1;
  rdstring[len-1]=0;
  return (int)*lrd;
}

int ZETASETPOS(int * motorno, int * position) 
{
  short lcmd, lresp;
  int retn;

  sprintf(cmd,"%d_PSET%d",*motorno,*position);
  //sprintf(cmd,"PSET%f",*motorno,*position);
  lcmd = (short) strlen(cmd);
  lresp = 0;
  if ((retn = RS232Z(cmd,&lcmd,response,&lresp,0)) < 0) return -1;
  //  if ((ptr = strstr(response,"OK:")) == NULL) return -2;

  return 0;
}

int ZETAGETPOS(int * motorno, int * position) 
{
  short lcmd, lresp;
  int retn;
  char ch, *ptr;

  sprintf(cmd,"%d_TPE",*motorno);
  lcmd = (short) strlen(cmd);
  lresp = 1;
  if ((retn = RS232Z(cmd,&lcmd, response, &lresp,1)) < 0) return -1;
  if ((ptr = strchr(response,'*')) == NULL) return -2;
  ptr++;
  while(isalpha(ch=*ptr)) ptr++; 
  if ((retn = sscanf(ptr,"%d",position)) != 1) return -3;
  return 0;
}

int ZETAGOPOS(int * motorno, int * position) 
{
  short lcmd, lresp;
  int retn;

  sprintf(cmd,"%d_D%d",*motorno,*position);
  lcmd = (short) strlen(cmd);
  lresp = 0;
  if ((retn = RS232Z(cmd,&lcmd, response, &lresp,0)) < 0) return -1;

  sprintf(cmd,"%d_GO",*motorno);
  lcmd = (short) strlen(cmd);
  lresp = 0;
  if ((retn = RS232Z(cmd,&lcmd, response, &lresp,0)) < 0) return -1;

  return 0;
}

int ZETASTATUS(int * motorno, int * moving, int * limstat) 
{
  short lcmd, lresp;
  int retn;
  char * ptr;

  *limstat = 0;
  *moving  = 0;  
  /* Determine whether axis is moving */
  sprintf(cmd,"%d_!tas.1",*motorno);
  lcmd= strlen(cmd);
  if ((retn = RS232Z(cmd,&lcmd, response, &lresp,1)) < 0) return -1;
  if ((ptr = strchr(response,'*')) == NULL) return -2; /* Bad reply */
  ptr++;
  *moving = atoi(ptr);

  /* Positive hard limit */
  sprintf(cmd,"%d_!tas.15",*motorno);
  lcmd= strlen(cmd);
  if ((retn = RS232Z(cmd,&lcmd, response, &lresp,1)) < 1) return -1;
  if ((ptr = strchr(response,'*')) == NULL) return -2; /* Bad reply */
  ptr++;
  retn = atoi(ptr);
  if (retn==1) *limstat = 1;
  if (debug&1) TRACE("Zeta: Positive limit=%d\n",retn);

  /* Negative hard limit */
  sprintf(cmd,"%d_!tas.16",*motorno);
  lcmd= strlen(cmd);
  if ((retn = RS232Z(cmd,&lcmd, response, &lresp,1)) < 1) return -1;
  if ((ptr = strchr(response,'*')) == NULL) return -2; /* Bad reply */
  ptr++;
  retn = atoi(ptr);
  if (retn==1) *limstat = -1;
  if (debug&1) TRACE("Zeta: Negative limit=%d\n",retn);

  return 0;
}

int ZETAHALT(int * motorno) 
{
  short lcmd, lresp;
  int retn;

  sprintf(cmd,"%d_!S",*motorno);
  lcmd = (short) strlen(cmd);
  lresp = 0;
  if ((retn = RS232Z(cmd,&lcmd, response, &lresp,0)) < 0) return -1;
  return 0;
}

int ZETAHOME(int * motorno, int * direction) 
{
  short lcmd, lresp;
  int retn;

  /* Should better specify homing parameters */
  if (*direction) {
    sprintf(cmd,"%d_HOM0",*motorno);
  } else {
    sprintf(cmd,"%d_HOM1",*motorno);
  }
  lcmd = (short) strlen(cmd);
  lresp = 0;
  if ((retn = RS232Z(cmd,&lcmd, response, &lresp,0)) < 0) return -1;
  return 0;
}
