/*
 * Routines to control Physik Instrumente precision motor controller
 *
 * Author: Nick Maliszewskyj, NIST Center for Neutron Research
 * $Id$
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include "serial.h"
#include "rs232.h"
#include "instrext.h"
#include "motorext.h"

extern RS232PARM rs232_cnf[NUMRS232];
extern int debug;

static int pi_fd=0;
static char tmpstr[80], cmdstr[80];

/* 
 * Initialize serial port 
 */
void
RS232P_INIT(){
  /* Get port number from FORTRAN side through "instr" common block */
  int portno;
  portno = _CINSTR.mot_line[2];
  if (debug) printf("RS232P_INIT: pi  portno = %d device = %s\n",
		    portno,rs232_cnf[portno].dev);
  if (portno < 0) {
    fprintf(stderr,"RS232P_INIT: Can't initialize portno %d\n",portno);
    exit(2);
  }
  if ((pi_fd = serialOpen(rs232_cnf[portno].dev, 
			  rs232_cnf[portno].baud, 
			  rs232_cnf[portno].bits, 
			  rs232_cnf[portno].parity,1)) < 0) {
    fprintf(stderr,"RS232P_INIT: open() failed!\n");
    exit(2);
  }
}

void
RS232P_END(){
  if (pi_fd > 0) close(pi_fd);
  pi_fd = 0;
}

int
RS232P(char * wrstring, short * lwr, char * rdstring, short * lrd, \
       short * reply){
  char talkstring[80];
  int  len;
  
  len = *lwr;
  strncpy(talkstring,wrstring,len);
  talkstring[len++] = '\n';
  talkstring[len]   = 0;

  if (debug&1) printf("RS232P: Send ->%s\n",talkstring);  
  serialFlush(pi_fd);
  write(pi_fd,talkstring,len);

  if (!*reply) return 0;
  if ((len = terminated_read(pi_fd,talkstring,80,'\n')) < 0) {
    /* If an error occurs */
    *rdstring = 0;
    *lrd = 0;
    serialFlush(pi_fd);
    return -1;
  }
  if (debug&1) printf("RS232P: Recv ->%s\n",talkstring);
  strncpy(rdstring,talkstring,len);
  *lrd = len-1;
  rdstring[len-1]=0;
  return (int)*lrd;
}

/* Extract PI axis number from motorstring */
int
PIAXISNO(int motorno, int * axis) {
  /*
  int n;

  memset(tmpstr,0,sizeof(tmpstr));
  memcpy(tmpstr,_CMCCOM.motorstring[motorno-1],2);
  if ((n = sscanf(tmpstr,"%d",axis)) != 1) return -1;
  */
  *axis = motorno;
  if ((*axis < 1) || (*axis > 4)) return -2;
  return 0; /* Success */
}

int
PISETPOS(int * motorno, int * pulses) {
  short lt, lc, reply;
  int axis;
  int retn;

  if ((retn = PIAXISNO(*motorno,&axis)) < 0) return -1;
  sprintf(cmdstr,"AXIS%2d; AXIS:POS %d",axis,*pulses);
  lc = (short) strlen(cmdstr);
  reply = 0;
  if ((retn = RS232P(cmdstr,&lc, tmpstr, &lt, &reply)) < 0) return -1;
  return 0;
}

int
PIGETPOS(int * motorno, int * pulses) {
  short lt, lc, reply;
  int axis;
  int retn;
  char *ptr;

  if ((retn = PIAXISNO(*motorno,&axis)) < 0) return -1;
  sprintf(cmdstr,"AXIS%2d; AXIS:POS?",axis);
  lc = (short) strlen(cmdstr);
  reply = 1;
  if ((retn = RS232P(cmdstr,&lc, tmpstr, &lt, &reply)) < 0) return -1;
  if ((ptr = strstr(tmpstr,"Home")) != NULL) {
    /* Check for Logical Position "Home" == 0 */
    *pulses = 0;
    return 0;
  }
  if ((retn = sscanf(tmpstr,"%d",pulses)) != 1) return -2;
  return 0;
}

int
PISETVEL(int * motorno, int * prate) {
  short lt, lc, reply;
  int axis;
  int retn;

  if ((retn = PIAXISNO(*motorno,&axis)) < 0) return -1;
  sprintf(cmdstr,"AXIS%2d; MVEL %d",axis, *prate);
  lc = (short) strlen(cmdstr);
  reply = 0;
  if ((retn = RS232P(cmdstr,&lc, tmpstr, &lt, &reply)) < 0) return -1;
  return 0;
}

int
PISETACC(int * motorno, int * acc) {
  short lt, lc, reply;
  int axis;
  int retn;

  if ((retn = PIAXISNO(*motorno,&axis)) < 0) return -1;
  sprintf(cmdstr,"AXIS%2d; ACC %d",axis, *acc);
  lc = (short) strlen(cmdstr);
  reply = 0;
  if ((retn = RS232P(cmdstr,&lc, tmpstr, &lt, &reply)) < 0) return -1;
  return 0;
}


int
PIGOPOS(int * motorno, int * pulses) {
  short lt, lc, reply;
  int axis;
  int retn;

  if ((retn = PIAXISNO(*motorno,&axis)) < 0) return -1;
  sprintf(cmdstr,"AXIS%2d; TARG %d",axis,*pulses);
  lc = (short) strlen(cmdstr);
  reply = 0;
  if ((retn = RS232P(cmdstr,&lc, tmpstr, &lt, &reply)) < 0) return -1;
  return 0;
}

int
PIHALT(int * motorno) {
  short lt, lc, reply;
  int axis;
  int retn;
  if ((retn = PIAXISNO(*motorno,&axis)) < 0) return -1;
  sprintf(cmdstr,"AXIS%2d; HALT",axis);
  lc = (short) strlen(cmdstr);
  reply = 0;
  if ((retn = RS232P(cmdstr,&lc, tmpstr, &lt, &reply)) < 0) return -1;
  return 0;
}

/*
 * Read motion status register 
 * Returns 16 bit value, which can be decoded the following way
 * Bit  0 - Axis 1 moving
 * Bit  1 - Axis 2 moving
 * Bit  2 - Axis 3 moving
 * Bit  3 - Axis 4 moving
 * Bit  4 - Axis 1 motion error
 * Bit  5 - Axis 2 motion error
 * Bit  6 - Axis 3 motion error
 * Bit  7 - Axis 4 motion error
 * Bit  8 - Axis 1 P limit
 * Bit  9 - Axis 2 P limit
 * Bit 10 - Axis 3 P limit
 * Bit 11 - Axis 4 P limit
 * Bit 12 - Axis 1 N limit
 * Bit 13 - Axis 2 N limit
 * Bit 14 - Axis 3 N limit
 * Bit 15 - Axis 4 N limit
 */

int
PISTATUS(int * motorno, int * moving, int * limstat) {
  short lt, lc, reply;
  int axis;
  int retn, n;
  if ((retn = PIAXISNO(*motorno,&axis)) < 0) return -1;
  sprintf(cmdstr,"AXIS%2d; STAT:MOT:COND?",axis);
  lc = (short) strlen(cmdstr);
  reply = 1;
  *moving = 0;
  if ((retn = RS232P(cmdstr,&lc, tmpstr, &lt, &reply)) < 0) return -2;
  /* Decode response to get status for correct axis */
  if ((n = sscanf(tmpstr,"%d",&retn)) != 1) return -3; /* Bad form */
  axis--; /* Offset axis number by one for bitwise operations */
  *moving = (retn & (1 << axis)) ? 1 : 0;
  
  /* Check positive limit */
  *limstat = (retn & (1 << (axis + 8))) ? 1 : 0;

  /* Check negative limit */
  *limstat = (retn & (1 << (axis + 12))) ? - 1 : 0;
  
  return 0;
}

int
PILIMSTAT(int * motorno, int * limstat) {
  short lt, lc, reply;
  char * ptr, ch;
  int retn, axis;

  if ((retn = PIAXISNO(*motorno,&axis)) < 0) return -1;
  sprintf(cmdstr,"AXIS%2d; AXIS:LIM:LEV?",axis);
  lc = (short) strlen(cmdstr);
  reply = 1;
  if ((retn = RS232P(cmdstr,&lc, tmpstr, &lt, &reply)) < 0) return -1;

  *limstat = 0;
  /* 
   * Form of reply:
   * N-Limitline HIGH, P-Limitline LOW"
   *
   * Parse this the cheap and dirty way
   */

  /* Check negative limit status */
  if ((ptr = strchr(tmpstr,',')) == NULL) return -1;
  ptr--;

  if ((ch = *ptr) == 'H') {
    *limstat = -1;
    return 0;
  }

  /* Check positive limit status */
  ptr = &tmpstr[lt - 2];
  if ((ch = *ptr) == 'H') {
    *limstat = 1;
    return 0;
  }
  
  return 0;
}
