/*
 * Control of devices in VME crate via RS232
 *
 * Version = $Id$
 *
 * Scaler note: treat VME scalers like area sensitive detectors
 *              will probably need to make a FORTRAN common/C Structure
 *              to share counts from individual detectors
 *
 *
 * motor move     <axis> destination
 * motor motion   ?axis?
 * motor position <axis>
 * 
 *
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/ioctl.h>
#include <linux/lp.h>
#include "serial.h"
#include "rs232.h"
#include "icp.h"
#include "instrext.h"
#include "vdatacmn.h"
#include "trace.h"

#define BT1SHUTPORT "/dev/ttyS0"
#define BT1SHUTBAUD 300

extern RS232PARM rs232_cnf[NUMRS232];
extern int debug;
#define MAXBUF 4096
#define COUNT_TIME 0
#define COUNT_MON  1

static int v_fd;
static int ctrmode = 0;
static char cmd[80], response[MAXBUF];
static int lvhisto[256];
static int whichmon=0;
extern int vhisto[256];
extern int nvhisto;

/* 
 * Initialize serial port 
 */
void RS232V_INIT()
{
  /* We need to eventually get port number from FORTRAN side through commons*/
  int portno;
  char * ptr;
  //  portno = _CINSTR.mot_line[1];
  portno = mot_line[1];
  v_fd = 0;
  if (debug) { TRACE("RS232V_INIT: vme portno = %d device = %s\n",
		     portno,rs232_cnf[portno].dev); }
  if (portno < 0) {
    if ((ptr = getenv("TESTING")) != NULL) {
      fprintf(stderr,"RS232V_INIT: fd=-1, no services will be available\n");
      return;
    }
    fprintf(stderr,"RS232V_INIT: Can't initialize portno %d\n",portno);
    exit(2);
  }
  if ((v_fd = serialOpen(rs232_cnf[portno].dev, 
			 rs232_cnf[portno].baud, 
			 rs232_cnf[portno].bits, 
			 rs232_cnf[portno].parity,0)) < 0) {
    fprintf(stderr,"RS232V_INIT: open() failed!\n");
    exit(2);
  }
  if (debug) TRACE("RS232V_INIT: vme fd = %d\n",v_fd);
}

void RS232V_END()
{
  if (debug) TRACE("RS232V_END\n");
  if (v_fd > 0) close(v_fd);
  v_fd = 0;
}

int RS232V(char * wrstring, short * lwr, char * rdstring, short * lrd)
{
  char talkstring[MAXBUF];
  int  len, n;
  
  len = *lwr;
  *rdstring = 0;
  *lrd = 0;

  strncpy(talkstring,wrstring,len);
  talkstring[len++] = '\r';
  talkstring[len]   = 0;

  if (debug&1) { TRACE("RS232V(%d): Send ->%s\n",v_fd,talkstring); }
  /* Make sure we've got nothing hanging around to gum up the works */
  serialFlush(v_fd); 
  if ((n = write(v_fd,talkstring,len)) != len) {
    fprintf(stderr,"RS232V: Did not write full message!\n");
    *lrd = 0;
    return -1;
  }
  memset(talkstring,0,sizeof(talkstring));
  usleep(500);
  /* sleep(2); */
#ifdef REM_ECHO
  /* Fix problem with serial communications with VME system */
  if ((len = terminated_read(v_fd,talkstring,MAXBUF-1,'\r')) < 0) return -1;
  if (debug&1) { TRACE("RS232V: Recv ->%s\n",talkstring); }
  sleep(2);
#endif 

  if ((len = terminated_read(v_fd,talkstring,MAXBUF-1,'\r')) < 0) return -1;
  if (debug&1) { TRACE("RS232V: Recv ->%s\n",talkstring); } 
  strncpy(rdstring,talkstring,len);
  *lrd = len-1;
  rdstring[len-1]=0;
  return (int)*lrd;
}

/*
 * Start scaler counting
 */
void VSCALERSTART(int * preset, int * mode)
{
  short lcmd, lresp;
  int retn;
  char mono[6];

  if (nsta == 1) {
    /* Make sure we select the monitor associated 
       with the current monochromator. */
    sprintf(cmd,"scaler select %d",whichmon);
    lcmd = (short) strlen(cmd);
    retn = RS232V(cmd,&lcmd,response,&lresp);
  }


  /*
   * Count for time:    mode = 0
   * Count for monitor: mode = 1
   */
  ctrmode = (*mode!=0) ? 1 : 0;
  if (ctrmode) {
    sprintf(cmd,"scaler monitor %d",*preset);
  } else {
    sprintf(cmd,"scaler time %d",*preset);
  }
  lcmd = (short) strlen(cmd);
  retn = RS232V(cmd,&lcmd,response,&lresp);
  /* Should really do some error handling here */
}

/*
 * Test to see whether counting is in progress
 */
void VSCALERSTATUS(int *iscounting)
{
  char *ptr;
  short lcmd, lresp;
  int retn;

  *iscounting = 0;    /* No counting in progress */   
  /* Test the output for counter 1 */
  sprintf(cmd,"scaler status");
  lcmd = (short) strlen(cmd);
  lresp = 0;
  retn = RS232V(cmd,&lcmd,response,&lresp);
  if ((ptr = strchr(response,':')) == NULL) return;
  ptr++;
  *iscounting = atoi(ptr);
  if (debug) { TRACE("VSCALERSTATUS: %s counting\n", 
		     ((*iscounting) ? "" : "not")); }
  /* Sleep if we're still counting */
  if (*iscounting) usleep(500000); /* Sleep for 0.5 sec */
}

/*
 * Abort count in progress
 */
void VSCALERSTOP()
{
  char * ptr;
  short lcmd, lresp;
  int retn;

  sprintf(cmd,"scaler abort");
  lcmd = (short) strlen(cmd);
  lresp = 0;
  retn = RS232V(cmd,&lcmd,response,&lresp);
  if ((ptr = strstr(response,"OK:")) == NULL) {
    fprintf(stderr,"VSCALERSTOP: unable to stop count in progress\n");
  }
}

// Select which monochromator (and monitor) are used for BT1
void VSCALERSELECT(int * which)
{
  switch(*which) {
  case 2: whichmon = 2; // Ge733
    break;
  case 1: whichmon = 1; // Ge311
    break; 
  default: whichmon = 0;       // Cu311
  }
  
}

void VSCALERRESET()
{
  char * ptr;
  short lcmd, lresp;
  int retn, i;

  sprintf(cmd,"scaler reset");
  lcmd = (short) strlen(cmd);
  lresp = 0;
  for (i=0;i<256;i++) vhisto[i] = 0;
  retn = RS232V(cmd,&lcmd,response,&lresp);
  if ((ptr = strstr(response,"OK:")) == NULL) {
    TRACE("VSCALERRESET: unable to reset scaler display\n");
  }
}

void VSCALERADD()
{
  int i;
  for (i=0;i<256;i++) vhisto[i] += lvhisto[i];
}

/*
 * Read scaler counts
 */
void VSCALERREAD(float * seconds, int * counts)
{
  char *ptr, *ptr2;
  short lcmd, lresp;
  int i, retn, sum, retry;;
  long ctr, ticks;
  char mono[6];

  for (i=0;i<scl;i++) {
    *(counts + i) = 0;
  } 
  *seconds = 0;
  sprintf(cmd,"scaler read");
  lcmd = (short) strlen(cmd);
  lresp = 0;
  memset(lvhisto,0,sizeof(lvhisto));
  retry = 0;
  while((retn = RS232V(cmd,&lcmd,response,&lresp)) <= 0) {
    if (retry > 3) break;
    fprintf(stderr,"Could not read from scaler, retrying...\n");
    retry++;
    usleep(1000);
  }


  /* Process response string */
  /* Should perhaps give some indicator of success or failure */
  if ((ptr = strstr(response,"OK:")) == NULL) return;
  
  ptr = strchr(response,':');
  /* Now read through counter values */
  ptr++;

  // Strip out first counter (clock ticks)
  ctr = strtol(ptr,&ptr2,0);
  ptr = ptr2;
  ticks = (int) ctr;

  i = 0;
  sum = 0;
  // Strip out clock ticks 
  while(i < 128) {
    ctr = strtol(ptr,&ptr2,0);
    if (ptr == ptr2) break;
    lvhisto[i] = (int) ctr;
    //    printf("vhisto[%d] = %d\n",i,ctr);
    if (i>1) sum += lvhisto[i];
    ptr = ptr2;
    i++;
  }
  nvhisto = i+1; // Put back the element we just stripped out

  /* Treat scalers differently if we're running on BT1 */
  /* counts[0] = monitor
   * counts[1]  \
   *   ...       >-- Detectors(32)
   * counts[32] /
   * counts[33] \
   *   ...       >-- Extra detectors
   * counts[36] /
   */
  if (nsta == 1) {
    // FIXME
    for (i=1;i<33;i++) {
      *(counts + i) = lvhisto[i + 6];
    }
    /* Choose monitor */
    memset(mono,0,sizeof(mono));
    // FIX ME FOR REAL!!!!
    //memcpy(mono,monobt1,5);
    /*
    strcpy(mono,"CU311");
    if (!strcmp(mono,"GE311")) {        
      *counts = lvhisto[1];
    } else if (!strcmp(mono,"GE733")) { 
      *counts = lvhisto[2];
    } else {                            
      *counts = lvhisto[0];
    }
    */
    switch (whichmon) {
    case 2: // GE733
      *counts = lvhisto[2];
      break;
    case 1: // GE311
      *counts = lvhisto[1];
      break;
    default: // CU311
      *counts = lvhisto[0];
    }
    /* Extra detectors */
    for (i=4;i<8;i++) {
      *(counts + 29 + i) = lvhisto[i];
    }
  } else {
    // NCM (2004/07/26)
    // Zeroth channel = clock ticks
    //
    //    *seconds = vhisto[0] / 10000.0;
    *counts       = lvhisto[0];
    *(counts + 1) = lvhisto[1];
    *(counts + 2) = sum; //vhisto[2];//sum;
  }

  /* Now retrieve elapsed time */
  sprintf(cmd,"scaler elapsed");
  lcmd = (short) strlen(cmd);
  lresp = 0;
  retn = RS232V(cmd,&lcmd,response,&lresp);

  if ((ptr = strstr(response,"OK:")) == NULL) return;
  ptr = strchr(response,':');
  ptr++;
  // *seconds = strtof(ptr,&ptr2);
  *seconds = atof(ptr);
}

void VPARPORT(int * inval)
{
  int ppstat,fd;
  char ch, *ptr;
  char buf[80];
  int len,n;

  ppstat = 0;
  *inval = ppstat;
  if ((fd =  serialOpen(BT1SHUTPORT,BT1SHUTBAUD,
			8,'N',0)) < 0) {
    fprintf(stderr,"VPARPORT: open() failed!\n");
    return;
  }

  strcat(buf,"$1DI\r");
  len=strlen(buf);
  serialFlush(fd); 
  if ((n = write(fd,buf,len)) != len) {
    fprintf(stderr,"VPARPORT: Did not write full message!\n");
    close(fd);
    return;
  }
  usleep(50);
  memset(buf,0,sizeof(buf));
  /* Fix problem with serial communications with VME system */
  if ((len = terminated_read(fd,buf,sizeof(buf)-1,'\r')) < 0) {
    close(fd);
    return;
  }
  if (debug&1) { fprintf(stderr,"VPARPORT: Recv ->%s\n",buf); }

  // ROBUSTIFY THIS CODE
  ch = buf[0];
  if (ch == '*') {
    ptr = &buf[1];
    ppstat = atoi(ptr);
    *inval = ppstat;
    if (debug&1) { fprintf(stderr,"VPARPORT: Status = %d\n",ppstat); }
  }
  
  close(fd);

}

int VMESETPOS(int * motorno, float * position)
{
  short lcmd, lresp;
  int retn;
  char * ptr;

  sprintf(cmd,"motor position %d %f",*motorno,*position);
  lcmd = (short) strlen(cmd);
  lresp = 0;
  if ((retn = RS232V(cmd,&lcmd,response,&lresp)) < 0) return -1;
  if ((ptr = strstr(response,"OK:")) == NULL) return -2;

  return 0;
}

int VMEGETPOS(int * motorno, float * position)
{
  short lcmd, lresp;
  int retn;
  char *ptr;

  sprintf(cmd,"motor position %d",*motorno);
  lcmd = (short) strlen(cmd);
  lresp = 1;
  if ((retn = RS232V(cmd,&lcmd, response, &lresp)) < 0) return -1;
  if ((ptr = strstr(response,"OK:")) == NULL) return -2;
  ptr+=3;
  if ((retn = sscanf(ptr,"%f",position)) != 1) return -3;
  return 0;
}

int VMEGOPOS(int * motorno, float * position)
{
  short lcmd, lresp;
  int retn;
  char * ptr;

  sprintf(cmd,"motor move %d %f",*motorno,*position);
  lcmd = (short) strlen(cmd);
  lresp = 0;
  if ((retn = RS232V(cmd,&lcmd, response, &lresp)) < 0) return -1;
  if ((ptr = strstr(response,"OK:")) == NULL) return -2;

  return 0;
}

int VMESTATUS(int * motorno, int * moving, int * limstat)
{
  short lcmd, lresp;
  int retn;
  char * ptr;

  *limstat = 0;
  *moving  = 0;
  /* Determine if we're moving */
  sprintf(cmd,"motor motion %d",*motorno);
  lcmd = (short) strlen(cmd);
  if ((retn = RS232V(cmd,&lcmd,response,&lresp)) < 0) return -1;
  if ((ptr = strstr(response,"OK:")) == NULL) return -2;

  ptr += 3;
  *moving = atoi(ptr);

  /* Now determine whether we've hit a limit */
  sprintf(cmd,"motor limits %d",*motorno);
  lcmd = (short) strlen(cmd);
  if ((retn = RS232V(cmd,&lcmd,response,&lresp)) < 0) return -3;
  if ((ptr = strstr(response,"OK:")) == NULL) return -4;

  ptr += 3;
  *limstat = atoi(ptr); /* 1 = pos lim, -1 = neg lim, 0 = no lim */

  return 0;
}

int VMEHALT(int * motorno)
{
  short lcmd, lresp;
  int retn;

  sprintf(cmd,"motor stop %d",*motorno);
  lcmd = (short) strlen(cmd);
  lresp = 0;
  if ((retn = RS232V(cmd,&lcmd, response, &lresp)) < 0) return -1;
  return 0;
}

void VMEENABLE(int * motorno)
{
  short lcmd, lresp;
  int retn;

  sprintf(cmd,"motor enable %d",*motorno);
  lcmd = (short) strlen(cmd);
  lresp = 0;
  retn = RS232V(cmd,&lcmd, response, &lresp);
  return;     
  //return 0;
  
}

void VMEDISABLE(int * motorno)
{
  short lcmd, lresp;
  int retn;

  sprintf(cmd,"motor disable %d",*motorno);
  lcmd = (short) strlen(cmd);
  lresp = 0;
  retn = RS232V(cmd,&lcmd, response, &lresp);
  return;
  //return 0;
}

int VMEHOME(int * motorno, int * direction)
{
  short lcmd, lresp;
  int retn;

  if (*direction) {
    sprintf(cmd,"motor home %d positive",*motorno);
  } else {
    sprintf(cmd,"motor home %d negative",*motorno);
  }
  lcmd = (short) strlen(cmd);
  lresp = 0;
  if ((retn = RS232V(cmd,&lcmd, response, &lresp)) < 0) return -1;
  return 0;
}

void VMEGOLIM(int * motorno, int * direction)
{
  short lcmd, lresp;
  int retn;

  if (*direction) {
    sprintf(cmd,"motor findlim %d positive",*motorno);
  } else {
    sprintf(cmd,"motor findlim %d negative",*motorno);
  }
  lcmd = (short) strlen(cmd);
  lresp = 0;
  retn = RS232V(cmd,&lcmd, response, &lresp);
}



