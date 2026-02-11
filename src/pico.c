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
static int i_fd;
#define MAXBUF 4096
static char cmd[80], response[MAXBUF];

/* 
 * Initialize serial port 
 * Get serial port "number" from "ACS" slot
 */
void RS232I_INIT()
{
  /* We need to eventually get port number from FORTRAN side through commons*/
  int portno;
  char * ptr;
  portno = mot_line[0];
  i_fd = 0;
  if (debug) { TRACE("RS232I_INIT: pico portno = %d device = %s\n",
		     portno,rs232_cnf[portno].dev); }
  if (portno < 0) {
    if ((ptr = getenv("TESTING")) != NULL) {
      fprintf(stderr,"RS232I_INIT: fd=-1, no services will be available\n");
      return;
    }
    fprintf(stderr,"RS232I_INIT: Can't initialize portno %d\n",portno);
    exit(2);
  }
  /* For PICO motors, settings should be 19200, 8N1 */

  if ((i_fd = serialOpen(rs232_cnf[portno].dev, 
			 rs232_cnf[portno].baud, 
			 rs232_cnf[portno].bits, 
			 rs232_cnf[portno].parity,0)) <= 0) {
    fprintf(stderr,"RS232I_INIT: open() failed!\n");
    exit(2);
  }
  if (debug) TRACE("RS232I_INIT: pico fd = %d\n",i_fd);
}

void RS232I_END()
{
  if (debug) TRACE("RS232I_END\n");
  if (i_fd > 0) close(i_fd);
  i_fd = 0;
}

int RS232I(char * wrstring, short * lwr, char * rdstring, short * lrd, int * err, int expectreply)
{
  char talkstring[MAXBUF], ch, *ptr;
  int  len, n;
  
  *err = 0;
  len = *lwr;
  *rdstring = 0;
  *lrd = 0;

  strncpy(talkstring,wrstring,len);
  talkstring[len++] = '\r';
  talkstring[len]   = 0;

  if (debug&1) { TRACE("RS232I(%d): Send ->%s\n",i_fd,talkstring); }
  /* Make sure we've got nothing hanging around to gum up the works */
  serialFlush(i_fd); 
  if ((n = write(i_fd,talkstring,len)) != len) {
    fprintf(stderr,"RS232I: Did not write full message!\n");
    *lrd = 0;
    return -1;
  }
  memset(talkstring,0,sizeof(talkstring));
  usleep(500);
  if (expectreply) {
    if ((len = terminated_read(i_fd,talkstring,MAXBUF-1,'\n')) < 0) {
      *err = -1;
      return -1;
    }

    if (debug & 1) { TRACE("RS232I(%d): Recv ->%s\n",i_fd,talkstring); }
    // Read success/fail character
  }
  if ((timed_read(i_fd, &ch, 1)) < 0) {
    *err = -1;
    return -2;
  }
  if (debug & 1) { TRACE("RS232I(%d): Char = %c\n",i_fd, ch); }
  if (ch != '>') *err = 1;
  if (!expectreply) return 0;
  ptr = strchr(talkstring,'\r');
  *ptr = 0;
  len = strlen(talkstring);

  strncpy(rdstring,talkstring,len);
  *lrd = len;
  return (int)*lrd;
}

int PICOSETPOS(int * motorno, int * position) 
{
  short lcmd, lresp;
  int retn, err;

  sprintf(cmd,"POS A1 %d",*position);

  lcmd = (short) strlen(cmd);
  lresp = 0;
  if ((retn = RS232I(cmd,&lcmd,response,&lresp,&err,1)) < 0) return -1;

  return 0;
}

int PICOGETPOS(int * motorno, int * position) 
{
  short lcmd, lresp;
  int retn, err;
  char ch, *ptr;

  sprintf(cmd,"POS A1");
  lcmd = (short) strlen(cmd);
  lresp = 1;
  if ((retn = RS232I(cmd,&lcmd, response, &lresp,&err,1)) < 0) return -1;
  if ((retn = sscanf(response,"%d",position)) != 1) return -3;
  return 0;
}

int PICOGOREL(int * motorno, int * offset) {
  short lcmd, lresp;
  int retn, err;

  sprintf(cmd,"REL A1 %d G",*offset);
  lcmd = (short) strlen(cmd);
  lresp = 0;
  if ((retn = RS232I(cmd,&lcmd, response, &lresp,&err,0)) < 0) return -1;

  return 0;
}

int PICOSTATUS(int * motorno, int * moving) 
{
  short lcmd, lresp;
  int statbyte;
  char * ptr, *ptr2;
  int retn, err;

  *moving = 0;
  sprintf(cmd,"STA A1");
  lcmd = (short) strlen(cmd);
  lresp = 0;
  if ((retn = RS232I(cmd,&lcmd, response, &lresp,&err,1)) < 0) return -1;
  
  /* Parse response */
  if ((ptr = strchr(response,'=')) == NULL) return -2;
  ptr++;
  statbyte = strtoul(ptr, &ptr2, 0);
  if (debug&1) printf("PICOSTATUS: statbyte = 0x%02X\n",statbyte);
  *moving = (statbyte & 0x01) ? 1 : 0;

  /* No limit status for 8753 driver */

  return 0;

}

int PICOHALT() {
  short lcmd, lresp;
  int retn, err;
  sprintf(cmd,"STO A1");
  lcmd = (short) strlen(cmd);
  lresp = 0;
  if ((retn = RS232I(cmd,&lcmd, response, &lresp,&err,0)) < 0) return -1;
  return 0;
}
