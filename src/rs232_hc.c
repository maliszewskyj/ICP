/*
 * Communication with temperature controllers via RS232
 *
 * Version = $Id$
 *
 * Scaler note: treat VME scalers like area sensitive detectors
 *              will probably need to make a FORTRAN common/C Structure
 *              to share counts from individual detectors
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <termios.h>
#include <unistd.h>
#include <ctype.h>
#include "serial.h"
#include "rs232.h"
#include "trace.h"
#include "instrext.h"

extern RS232PARM rs232_cnf[NUMRS232];
extern int debug;

static int adamaddr=1;
static int hc_fd;
static int sw_fd=-1; // "Switcher" file descriptor
char * sw_dev = "/dev/ttyS0";

/* 
 * Initialize serial port 
 */
void RS232HC_INIT(int * baud, int * bits, int * parity,int *stop)
{
  int portno;
  struct termios pt;
  portno = mag_line;
  if (hc_fd > 0) close(hc_fd);

  if (debug) printf("RS232HC_INIT: tc portno = %d device = %s\n",
		    portno,rs232_cnf[portno].dev);
  if (debug) printf("RS232HC_INIT: baud = %d bits = %d parity = %d\n",
		    *baud, *bits, *parity);
  if (portno < 0) {
    fprintf(stderr,"RS232HC_INIT: Can't initialize portno %d\n",portno);
    return;
  }
  rs232_cnf[portno].baud = *baud;
  rs232_cnf[portno].bits = *bits;
  rs232_cnf[portno].parity = * parity;
  if ((hc_fd = serialOpen(rs232_cnf[portno].dev, 
			 rs232_cnf[portno].baud, 
			 rs232_cnf[portno].bits, 
			 rs232_cnf[portno].parity,0)) < 0) {

    fprintf(stderr,"RS232HC_INIT: open() failed!\n");
  }
  /* Fix up stop bits in termio structure for Oxford Magnets */
  if (*stop > 1) {
    if (debug) printf("RS232HC_INIT: set stop bits to 2\n");
    if (tcgetattr(hc_fd, &pt)) {
      printf("\nCould not get port attributes\n");
      return;
    }
    pt.c_cflag |= CSTOPB;
    if (tcsetattr(hc_fd, 0, &pt)) {
      printf("\nCould not set port attributes\n");
      return;
    }
  }
}

/*
 * Simple serial routine for Lakeshore controllers
 */
void RS232HC_SC(char * wrstring, int * lwr, char * rdstring, int * lrd, 
	   int *wait, int * rdreply)
{
  char talkstring[80], ch, *tptr;
  int  len, n,i;
  
  *lrd = 0;
  memset(talkstring,0,sizeof(talkstring));
  strncpy(talkstring,wrstring,*lwr);
  strcat(talkstring,"\r\n");
  len = *lwr + 2;

  if (debug&1) printf("RS232TC_SC: Send ->%s\n(%d) chars wait=%d\n",
		      talkstring,len,*wait);  
  /* Insert waits between transmitted chars because Gayle does */
  serialFlush(hc_fd);
  if (*wait==1) {
    for (i=0;i<len;i++) {
      ch = talkstring[i];
      /*      putchar(ch);*/ /* Print each character, just so we know what's going on */
      n = write(hc_fd,&ch,1);
      usleep(200000);
    }
  } else {
    if ((n = write(hc_fd,talkstring,len)) != len) {
      fprintf(stderr,"RS232TC_SC: unable to write full string!\n");
      return;
    }
    usleep(10000);
  }

  if (!*rdreply) return;
  if ((len = terminated_read(hc_fd,talkstring,sizeof(talkstring)-1,'\n')) < 0){
    /* If an error occurs */
    if (debug&1) printf("RS232TC_SC: Error = %d\n",len);
    *rdstring = ' ';
    *lrd = 0;
    return;
  }
  if (debug&1) printf("RS232TC_SC: len = %d\n",len);
  if (debug&1) printf("RS232TC_SC: Recv ->%s<-\n",talkstring);

  /* Check for the end of a previous terminating character (if we used \r\n */
  if ((ch = talkstring[0]) == '\n') {
    tptr = &talkstring[1];
    len--;
  } else {
    tptr = &talkstring[0];
  }

  /* Replace nonprinting characters with spaces */
  for (n=0;n<len;n++) {
    if (isspace((ch = tptr[n]))) {
      tptr[n] = ' ';
    }
  }
  tptr[len] = 0;

  if (debug&1) {
    printf("RS232TC_SC: returning ->%s<- len = %d\n",tptr,len);
    fflush(stdout);
  }
  if (len > 0) {
    /*strncpy(rdstring,tptr,len);*/
    memcpy(rdstring,tptr,len);
    *lrd = (short) len;
    rdstring[len]=' ';
  }
  usleep(100000);
}

/* Oxford magnet */
void RS232_OX(char * wrstring, int * lwr, char * rdstring, int * lrd)
{
  char talkstring[80];
  int  len, n;
  
  len = *lwr;
  *lrd = 0;
  if (hc_fd <= 0) 0;
  strncpy(talkstring,wrstring,len);
  talkstring[len++] = '\r';
  talkstring[len]   = 0;

  if (debug&1) { TRACE("RS232_OX: Send ->%s\n",talkstring); }
  if ((n = write(hc_fd,talkstring,len)) != len) {
    fprintf(stderr,"RS232_OX: unable to write full string!\n");
    return;
  }

  if ((len = terminated_read(hc_fd,talkstring,80,'\r')) < 0) {
    /* If an error occurs */
    if (debug&1) { TRACE("RS232_OX: Error = %d\n",len); }
    *rdstring = 0;
    *lrd = 0;
    serialFlush(hc_fd);
    return;
  }
  if (len < 1) len=1;
  if (debug&1) { TRACE("RS232_OX: Recv ->%s\n",talkstring); }
  strncpy(rdstring,talkstring,len);
  *lrd = len-1;
  rdstring[len-1]=0;
  return;
}

/* Oxford magnet */
void RS232_OXMAG(char * wrstring, int * lwr, char * rdstring, 
		 int * lrd, int * getreply)
{
  char talkstring[80], ch;
  int  len, n, i;
  
  len = *lwr;
  *lrd = 0;
  if (hc_fd <= 0) return;
  strncpy(talkstring,wrstring,len);
  talkstring[len++] = '\r';
  talkstring[len]   = 0;

  if (debug&1) { TRACE("RS232_OXMAG: Send ->%s\n",talkstring); }
  serialFlush(hc_fd);
#define SLOWWRITE
#ifdef SLOWWRITE
  for (i=0;i<len;i++) {
    ch = talkstring[i];
    if ((n = write(hc_fd, &ch, 1)) != 1) {
      fprintf(stderr,"RS232_OX: unable to write character!\n");
      return;
    }
    usleep(10);
  }
#else
  if ((n = write(hc_fd,talkstring,len)) != len) {
    fprintf(stderr,"RS232_OX: unable to write full string!\n");
    return;
  }
#endif
  usleep(200);

  if (!*getreply) return 0;
  if ((len = terminated_read(hc_fd,talkstring,80,'\r')) <= 0) {
    /* If an error occurs */
    if (debug&1) { TRACE("RS232_OXMAG: Error = %d\n",len); }
    *rdstring = 0;
    *lrd = 0;
    serialFlush(hc_fd);
    return;
  }
  if (len < 1) len = 1;
  if (debug&1) { TRACE("RS232_OXMAG: Recv ->%s\n",talkstring); }
  strncpy(rdstring,talkstring,len);
  *lrd = len-1;
  rdstring[len-1]=0;
  return;
}

int RS232_ADAM(char * wrstring, int * lwr, char * rdstring, int * lrd)
{
  char talkstring[80];
  int  len, n;

  *lrd = 0;
  memset(talkstring,0,sizeof(talkstring));
  strncpy(talkstring,wrstring,*lwr);
  if (debug&1) { TRACE("RS232_ADAM: Sending \"%s\"\n",talkstring); }
  strcat(talkstring,"\r");
  len = *lwr + 1;

  serialFlush(hc_fd);
  if ((n = write(hc_fd,talkstring,len)) != len) {
    fprintf(stderr,"RS232_ADAM: unable to write full string!\n");
    return -1;
  } 

  if ((len = terminated_read(hc_fd,talkstring,80,'\r')) <= 0) {
    /* If an error occurs */
    if (debug&1) { TRACE("RS232_ADAM: Error = %d\n",len); }
    *rdstring = 0;
    *lrd = 0;
    serialFlush(hc_fd);
    return -1;
  } 
  if (debug&1) { TRACE("RS232_ADAM: Recv \"%s\"\n",talkstring); }
  strncpy(rdstring,talkstring,len);
  *lrd = len-1;
  rdstring[len-1]=0;
  return (int)*lrd;
}

void ADAM_SETVOLT(float * volt,int *err)
{
  char outstr[20],instr[20],ch;
  int lout, lin, retn;
  sprintf(outstr,"#%02X%06.3f",adamaddr,*volt);
  lout = strlen(outstr);

  *err = 1;
  retn = RS232_ADAM(outstr,&lout,instr,&lin);
  if ((retn > 0) && lin) {
    ch = instr[0];
    switch (ch) {
    case '>':
      *err = 0; // Transaction successful
      break;
    case '?':   // Value out of range
    default:
      *err = 1;
      return;
    } 
  }
  return;
}

void ADAM_GETVOLT(float * volt,int *err)
{
  char outstr[20],instr[20],ch,*ptr;
  int lout, lin, retn;
  float tmpvolt;
  // Request current output setting
  sprintf(outstr,"$%02X6",adamaddr);
  lout = strlen(outstr);

  *err = 1;
  *volt = 0;
  retn = RS232_ADAM(outstr,&lout,instr,&lin);
  if ((retn > 0) && lin) {
    ch = instr[0];
    switch(ch) {
    case '!':
      ptr = &instr[3];
      sscanf(ptr,"%f",&tmpvolt);
      *volt = tmpvolt;
      *err=0;
      break;
    default:
      *err=1;
    }
  }
  return;
}

void PSSW_INIT() 
{
  if ((sw_fd = serialOpen(sw_dev, 9600, 8, 0, 0)) < 0) {
    fprintf(stderr,"RS232HC_INIT: open() failed!\n");
  }
}

int ADAM_TALK(int fd, char * outstr, int lout, char * instr, int * lin)
{
  char talkstring[80];
  int  len, n;

  *lin = 0;
  memset(talkstring,0,sizeof(talkstring));
  strncpy(talkstring,outstr,lout);
  if (debug&1) { TRACE("ADAM_TALK: Sending \"%s\"\n",talkstring); }
  strcat(talkstring,"\r");
  len = lout + 1;

  serialFlush(fd);
  if ((n = write(fd,talkstring,len)) != len) {
    fprintf(stderr,"ADAM_TALK: unable to write full string!\n");
    return -1;
  } 

  if ((len = terminated_read(fd,talkstring,20,'\r')) <= 0) {
    /* If an error occurs */
    if (debug&1) { TRACE("RS232_ADAM: Error = %d\n",len); }
    *instr = 0;
    *lin = 0;
    serialFlush(fd);
    return -1;
  } 
  if (debug&1) { TRACE("ADAM_TALK: Recv \"%s\"\n",talkstring); }
  strncpy(instr,talkstring,len);
  *lin = len-1;
  instr[len-1]=0;
  return (int)*lin;
}

void PSSW_TOGGLE(int *err)
{
  // Toggle relay 1 of an ADAM 4050 industrial control module
  char command[20], response[20];
  int lcom, lresp;
  int retn;
  *err = 1;
  

  if (sw_fd <= 0) {
    PSSW_INIT();
    if (sw_fd <= 0) {
      fprintf(stderr,"PSSW_TOGGLE: Uninitialized file descriptor");
      return;
    }
  }

  
  sprintf(command,"#011001"); // Switch on relay 0
  lcom = strlen(command);
  retn = ADAM_TALK(sw_fd, command, lcom, response, &lresp);

  usleep(100);
  sprintf(command, "#011000"); // Switch off relay 0
  lcom = strlen(command);
  retn = ADAM_TALK(sw_fd, command, lcom, response, &lresp);
  
  *err=0;
  return;
}

#define RLY_BSY 0x02
#define RLY_SET 0x20
#define RLY_RST 0x40

void PSSW_STATE(int *sign, int *busy, int * err) 
{
  char command[20], response[20];
  int lcom, lresp;
  int retn,dIn,dOut;
  char ch;

  *err=1;
  *sign = 1;
  *busy = 0;
  if (sw_fd <= 0) {
    PSSW_INIT();
    if (sw_fd <= 0) {
      fprintf(stderr,"PSSW_TOGGLE: Uninitialized file descriptor");
      return;
    }
  }

  sprintf(command,"$016");
  lcom = strlen(command);
  retn = ADAM_TALK(sw_fd, command, lcom, response, &lresp);
  if (retn < 0) {
    *err=1;
    *sign=0;
    *busy=1;
  }
  if (debug & 1) fprintf(stderr,"PSSW_STATE: reply = %s\n",response);
  if ((ch = response[0]) == '!') {
    lresp=sscanf((response+1),"%02x%02x",&dOut,&dIn);
    (*sign) = (dIn & RLY_SET) ? 1 : -1;
    (*busy) = (dIn & RLY_BSY) ? 1 : 0;
    if (debug & 1) fprintf(stderr,"PSSW_STATE: dIn = 0x%02x dOut = 0x%02x\n",dIn,dOut);
    if (debug & 1) fprintf(stderr,"PSSW_STATE: Polarity = %d Busy = %d\n",*sign,*busy);
    *err=0;
  } else {
    if (debug & 1) fprintf(stderr,"PSSW_STATE: Error parsing reply\n");
    *err = 1;
  }

  if (debug & 1)
  return;
}
