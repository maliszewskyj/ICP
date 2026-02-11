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
#include <unistd.h>
#include <ctype.h>
#include <math.h>
#include "serial.h"
#include "rs232.h"
#include "instrext.h"
#include "trace.h"

extern RS232PARM rs232_cnf[NUMRS232];

static int tc_fd;

void LIBWAIT(float * fracsec, int * status);

/* 
 * Initialize serial port 
 */
void RS232TC_INIT(int * baud, int * bits, int * parity)
{
  int portno;
  portno = tmp_line;
  if (tc_fd > 0) close(tc_fd);

  if (debug) { TRACE("RS232TC_INIT: tc portno = %d device = %s\n",
		     portno,rs232_cnf[portno].dev); }
  if (debug) { TRACE("RS232TC_INIT: baud = %d bits = %d parity = %d\n",
		     *baud, *bits, *parity); }
  if (portno < 0) {
    fprintf(stderr,"RS232TC_INIT: Can't initialize portno %d\n",portno);
    return;
  }
  rs232_cnf[portno].baud = *baud;
  rs232_cnf[portno].bits = *bits;
  rs232_cnf[portno].parity = * parity;
  if ((tc_fd = serialOpen(rs232_cnf[portno].dev, 
			 rs232_cnf[portno].baud, 
			 rs232_cnf[portno].bits, 
			 rs232_cnf[portno].parity,0)) < 0) {
    fprintf(stderr,"RS232TC_INIT: open() failed!\n");
  }
}

void RS232TC_RD(char * wrstring, int * lwr, char * rdstring, int * lrd)
{
  char talkstring[80];
  int  len, n;
  
  len = *lwr;
  *lrd = 0;
  strncpy(talkstring,wrstring,len);
  talkstring[len++] = '\n';
  talkstring[len]   = 0;

  serialFlush(tc_fd); 
  if (debug&1) { TRACE("RS232TC_RD: Send ->%s\n",talkstring); }
  if ((n = write(tc_fd,talkstring,len)) != len) {
    fprintf(stderr,"RS232TC_RD: unable to write full string!\n");
    return;
  }

  if ((len = terminated_read(tc_fd,talkstring,80,'\n')) < 0) {
    /* If an error occurs */
    if (debug&1) { TRACE("RS232TC_RD: Error = %d\n",len); }
    *rdstring = 0;
    *lrd = 0;
    return;
  }
  if (debug&1) { TRACE("RS232TC_RD: Recv ->%s<-\n",talkstring); }
  strncpy(rdstring,talkstring,len);
  *lrd = len-2;
  rdstring[len-2]=0;
}

/*
 * Simple serial routine for Lakeshore controllers
 */
void RS232TC_LK(char * wrstring, int * lwr, char * rdstring, int * lrd, 
	   int * rdreply)
{
  char talkstring[128], ch, *tptr;
  int  len, n;
  
  len = *lwr;
  *lrd = 0;
  if (len > sizeof(talkstring) - 2) {
    fprintf(stderr,"RS232TC_LK: Attempt to use more than allotted scratch space\n");
    return;
  }
  strncpy(talkstring,wrstring,len);
  talkstring[len++] = '\r';
  talkstring[len++] = '\n';
  talkstring[len]   = 0;

  serialFlush(tc_fd); 
  if (debug&1) { TRACE("RS232TC_LK: Send ->%s\n",talkstring); }
  if ((n = write(tc_fd,talkstring,len)) != len) {
    fprintf(stderr,"RS232TC_LK: unable to write full string!\n");
    return;
  }

  if (!*rdreply) return;
  if ((len = terminated_read(tc_fd,talkstring,sizeof(talkstring)-1,'\r')) < 0){
    /* If an error occurs */
    if (debug&1) { TRACE("RS232TC_LK: Error = %d\n",len); }
    *rdstring = ' ';
    *lrd = 0;
    return;
  }
  if (debug&1) { TRACE("RS232TC_LK: len = %d\n",len); }
  if (debug&1) { TRACE("RS232TC_LK: Recv ->%s<-\n",talkstring); }

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
    TRACE("RS232TC_LK: returning ->%s<- len = %d\n",tptr,len);
  }
  if (len > 0) {
    /*strncpy(rdstring,tptr,len);*/
    memcpy(rdstring,tptr,len);
    *lrd = (int) len;
    rdstring[len]=' ';
  }
  if (debug&1) { TRACE("RS232TC_LK: Successfully leaving\n"); }
}

/* "Old" Omega temperature controller, Model CN2000  */
void OM232O(char * wrstring, int * lwr, char * rdstring, int * lrd)
{
  char talkstring[80];
  int  len, n, i, cksum;
  char ch, *ptr,*ptr2;
  
  len = *lwr;
  rdstring[0] = 0;
  *lrd = 0;
  memset(talkstring,0,sizeof(talkstring));
  strncpy(talkstring,wrstring,len);

  cksum = 0;
  for (i=0;i<*lwr;i++) {
    ch = talkstring[i];
    cksum += (int) ch;
  }
  cksum = (cksum & 0xFF);
  ptr = &talkstring[i];
  ptr++;
  sprintf(ptr,"%02X\r",cksum);
  len = strlen(talkstring);

  if (debug&1) { TRACE("RS232TC_OM: Send ->%s\n",talkstring); }
  serialFlush(tc_fd); 
  if ((n = write(tc_fd,talkstring,len)) != len) {
    fprintf(stderr,"RS232TC_OM: unable to write full string!\n");
    return;
  }

  memset(talkstring,0,sizeof(talkstring));
  if ((len = terminated_read(tc_fd,talkstring,80,'\n')) < 0) {
    /* If an error occurs */
    if (debug&1) { TRACE("RS232TC_OM: Error = %d\n",len); }
    *rdstring = 0;
    *lrd = 0;
  }

  if (len>0) {
    if (debug&1) { TRACE("RS232TC_OM: Recv ->%s\n",talkstring); }
    if ((ptr = strchr(talkstring,':')) != NULL) {
      ptr++;
      if ((ptr2 = strchr(ptr,':')) != NULL) {
	len = ptr2 - ptr;
	memcpy(rdstring,ptr,len);
	*lrd = len;
	rdstring[len] = 0;
      }
    }
  }
}

void RS232TC_OM(char * wrstring, int * lwr, char * rdstring, int * lrd)
{
  char talkstring[80];
  int  len, n, i;
  char ch;
  
  len = *lwr;
  *lrd = 0;
  strncpy(talkstring,wrstring,len);
  talkstring[len++] = '\r';
  talkstring[len]   = 0;

  if (debug&1) { TRACE("RS232TC_OM: Send ->%s\n",talkstring); }
  if ((n = write(tc_fd,talkstring,len)) != len) {
    fprintf(stderr,"RS232TC_OM: unable to write full string!\n");
    return;
  }

  memset(talkstring,0,sizeof(talkstring));
  serialFlush(tc_fd); 
  if ((len = terminated_read(tc_fd,talkstring,80,'\r')) < 0) {
    /* If an error occurs */
    if (debug&1) { TRACE("RS232TC_OM: Error = %d\n",len); }
    *rdstring = 0;
    *lrd = 0;
  }

  if (len>0) {
    for(i=0;i<len;i++) {
      ch = talkstring[i];
      if (ch == 0) { talkstring[i] = ' '; } /* Remove any nulls */
    }
    if (debug&1) { TRACE("RS232TC_OM: Recv ->%s\n",talkstring); }
    memcpy(rdstring,talkstring,len);
    *lrd = len-1;
    rdstring[len-1]=0;
  }
}


/* Neslab temperature controller */
void RS232TC_NL(char * wrstring, int * lwr, char * rdstring, int * lrd)
{
  char talkstring[80];
  int  len, n;
  
  len = *lwr;
  *lrd = 0;
  strncpy(talkstring,wrstring,len);
  talkstring[len++] = '\r';
  talkstring[len]   = 0;

  serialFlush(tc_fd);
  if (debug&1) { TRACE("RS232TC_NL: Send ->%s\n",talkstring); }
  if ((n = write(tc_fd,talkstring,len)) != len) {
    fprintf(stderr,"RS232TC_NL: unable to write full string!\n");
    return;
  }

  if ((len = terminated_read(tc_fd,talkstring,80,'\r')) < 0) {
    /* If an error occurs */
    if (debug&1) { TRACE("RS232TC_NL: Error = %d\n",len); }
    *rdstring = 0;
    *lrd = 0;
    return;
  }
  if (debug&1) { TRACE("RS232TC_NL: Recv ->%s\n",talkstring); }
  strncpy(rdstring,talkstring,len);
  *lrd = len-1;
  rdstring[len-1]=0;

}

/* 12T Magnet -- run by labview */
void RS232TC_LV(char * wrstring, int * lwr, char * rdstring, int * lrd, 
	   int * rdreply)
{
  char talkstring[80];
  int  len, n;
  
  len = *lwr;
  *lrd = 0;
  strncpy(talkstring,wrstring,len);
  talkstring[len++] = '\r';
  talkstring[len]   = 0;

  serialFlush(tc_fd);
  if (debug&1) { TRACE("RS232TC_LV: Send ->%s\n",talkstring); }
  if ((n = write(tc_fd,talkstring,len)) != len) {
    fprintf(stderr,"RS232TC_LV: unable to write full string!\n");
    return;
  }
  memset(talkstring,0,sizeof(talkstring));

  if (!*rdreply) return;
  sleep(1);
  if ((len = terminated_read(tc_fd,talkstring,80,'\r')) < 0) {
    /* If an error occurs */
    if (debug&1) { TRACE("RS232TC_LV: Error = %d\n",len); }
    *rdstring = 0;
    *lrd = 0;
    return;
  }
  if (debug&1) { TRACE("RS232TC_LV: Recv ->%s\n",talkstring); }
  strncpy(rdstring,talkstring,len);
  if (len < 1) len = 1;
  *lrd = (int) len-1;
  rdstring[len-1]=0;

}
/*
extern struct {
  volatile int sigint;
  volatile int sigquit;
  volatile int sigtstp;
  volatile int sigterm;
} _CICPSIG;
*/
extern volatile int sigint;

void LV_READY()
{
  char command[80], reply[80],ptr;
  float waittime=0.5;
  int status;
  int lcom, lreply, rdreply=1;
  if (debug&1) { TRACE("LV_READY: Start"); }

  while(1) {
    sleep(2);
    if (sigint) {
      return;
    }
    strcpy(command,"MAGSTAT");
    lcom = 7;
    memset(reply,0,sizeof(reply));
    RS232TC_LV(command,&lcom,reply,&lreply,&rdreply);
    if (lreply > 0) {
      // Searching for "Complete"
      if ((ptr = strstr(reply,"COMP")) != NULL) {
	break;
      }
    }
  }
}

/*
 * Serial communication with LR700 resistance bridge (temperature controller)
 * Note: may need to insert waits & retries to make this beast work properly
 */
void RS232TC_LR700(char * wrstring, int * lwr, char * rdstring, int * lrd) 
{
  char talkstring[80];
  int len, n, i;
  char ch;

  len = *lwr;
  *lrd = 0;
  strncpy(talkstring,wrstring,len);
  talkstring[len++] = '\r';
  talkstring[len]   = 0;

  if (debug&1) { TRACE("RS232TC_LR700: Send ->%s\n",talkstring); }
  /* Insert waits between transmitted chars because Gayle does */
  for (i=0;i<len;i++) {
    ch = talkstring[i];
    n = write(tc_fd,&ch,1);
    usleep(100000);
  }

  if ((len = terminated_read(tc_fd,talkstring,80,'\r')) < 0) {
    /* If an error occurs */
    if (debug&1) { TRACE("RS232TC_LR700: Error = %d\n",len); }
    *rdstring = ' ';
    *lrd = 0;
    return;
  }
  if (debug&1) { TRACE("RS232TC_LR700: Recv ->%s\n",talkstring); }
  if ((ch = talkstring[0]) == '?') {
    fprintf(stderr," LR700 sending error response\n");
    *lrd = 0;
    return;
  }
  strncpy(rdstring,talkstring,len);
  *lrd = len-1;
  rdstring[len-1]=' '; /* Don't NULL pad strings to be returned to FORTRAN */

}

void RS232TC_LTC(char * wrstring, int * lwr, char * rdstring, int * lrd, 
	   int * rdreply)
{
  char talkstring[80], ch, *tptr;
  int  len, n;
  
  len = *lwr;
  *lrd = 0;
  strncpy(talkstring,wrstring,len);
  talkstring[len]   = 0;

  if (debug&1) { TRACE("RS232TC_LTC: Send ->%s\n",talkstring); }
  if ((n = write(tc_fd,talkstring,len)) != len) {
    fprintf(stderr,"RS232TC_LTC: unable to write full string!\n");
    return;
  }

  if (!*rdreply) return;
  if ((len = terminated_read(tc_fd,talkstring,sizeof(talkstring)-1,'\n')) < 0){
    /* If an error occurs */
    if (debug&1) TRACE("RS232TC_LTC: Error = %d\n",len);
    *rdstring = ' ';
    *lrd = 0;
    return;
  }
  if (debug&1) { TRACE("RS232TC_LTC: len = %d\n",len); }
  if (debug&1) { TRACE("RS232TC_LTC: Recv ->%s<-\n",talkstring); }

  /* Check for the end of a previous terminating character (if we used \r\n */
  tptr = &talkstring[0];

  /* Replace nonprinting characters with spaces */
  for (n=0;n<len;n++) {
    if (isspace((ch = tptr[n]))) {
      tptr[n] = ' ';
    }
  }
  tptr[len] = 0;

  if (debug&1) {
    TRACE("RS232TC_LTC: returning ->%s<- len = %d\n",tptr,len);
    fflush(stdout);
  }
  if (len > 0) {
    /*strncpy(rdstring,tptr,len);*/
    memcpy(rdstring,tptr,len);
    *lrd = (int) len;
    rdstring[len]=' ';
  }
  if (debug&1) { TRACE("RS232TC_LTC: Successfully leaving\n"); }
}

/*
 * Eurotherm furnace
 *
 * Query string: <EOT> <GID> <GID> <UID> <UID> <C1> <C2> <ENQ>
 *                                              |    |
 *                    2 char command mnemonic   +----+  
 *      For us, GID=0, UID=1 so the sequence is <EOT> 0 0 1 1 <C1> <C2> <ENQ>
 *
 * Response: <STX> <C1> <C2> <D1> <D2> <D3> <D3> <D5> <ETX> <BCC>
 *
 */
void RS232TC_FRN(char * wrstring, int * lwr, char * rdstring, int * lrd, 
	   int * tchar, int * cksum)
{
  char talkstring[80], ch, csum;
  char matchstring[10];
  int  len, n, i;

  
  len = *lwr;
  *lrd = 0;
  sprintf(matchstring,"%c%c%c",ETX,ACK,NAK);
  strncpy(talkstring,wrstring,len);
  talkstring[len]   = 0;

  /* Calculate checksum between STX and ETX (including ETX) and 
     add to end of string */
  if (*cksum) {
    csum = 0;
    for (i=0;i<len;i++) {
      ch = talkstring[i];
      csum ^= ch;
      if (ch == STX) csum = 0;
    }
    ch = csum;
    talkstring[len++] = csum;
    talkstring[len] = 0;
  }

  /* Write out command string */
  if (debug) TRACE("RS232TC_FRN: Sending ->");
  for (i=0;i<len;i++) {
    ch = talkstring[i];
    if (debug & 1) {
      if (isprint(ch)) { putchar(ch); } else { putchar(' '); }
    }
    if (debug & 4) printf("<%02x>",ch); 
    n = write(tc_fd,&ch,1);
    usleep(100000); /* Wait 0.1 sec */
  }
  if (debug) printf("<-\n");
  
  /* Read reply from controller */
  if ((len = termset_read(tc_fd,talkstring,80,matchstring)) < 0) {
    /* If an error occurs */
    if (debug&1) TRACE("RS232TC_FRN: Error = %d\n",len);
    *tchar = 0;
    *rdstring = ' ';
    *lrd = 0;
    return;
  }
  ch = talkstring[len-1];
  if (debug&1) TRACE("RS232TC_FRN: Terminator = %d\n",ch);
  *tchar = (int) ch;

  /* Now read the checksum character */
  if (!*cksum) {
    if ((n = timed_read(tc_fd, &ch, 1)) < 0) {
      /* Complain because we can't read the checksum character */
      if (debug &1) TRACE("RS232TC_FRN: Problem reading checksum char\n");
    }
  }

  if (debug&1) 
    TRACE("RS232TC_FRN: returning ->%s<- len = %d\n",talkstring,len);
  if (len > 0) {
    memcpy(rdstring,talkstring,len);
    *lrd = (int) len;
    rdstring[len]=' ';
  }

}

void RS232TC_CR(char * wrstring, int * lwr, char * rdstring, int * lrd, 
	   int * rdreply) 
{
  char talkstring[80];
  int len, n, i;
  char ch;

  len = *lwr;
  *lrd = 0;
  strncpy(talkstring,wrstring,len);
  talkstring[len++] = '\r';
  talkstring[len++] = '\n';
  talkstring[len]   = 0;

  if (debug&1) { TRACE("RS232TC_CR: Send ->%s\n",talkstring); }
  if (debug&1) { TRACE("RS232TC_CR: Length = %d\n",len); }
  /* Insert waits between transmitted chars because Gayle does */
  for (i=0;i<len;i++) {
    ch = talkstring[i];
    n = write(tc_fd,&ch,1);
    usleep(10);
  }
  if (!*rdreply) return;
  

  if ((len = terminated_read(tc_fd,talkstring,80,'\n')) < 0) {
    /* If an error occurs */
    if (debug&1) { TRACE("RS232TC_CR: Error = %d\n",len); }
    *rdstring = ' ';
    *lrd = 0;
    return;
  }
  if (debug&1) { TRACE("RS232TC_CR: Recv ->%s\n",talkstring); }
  if (debug&1) { TRACE("RS232TC_CR: Rlen = %d\n",len); }
  strncpy(rdstring,talkstring,len);
  *lrd = len-1;
  rdstring[len-1]=' '; /* Don't NULL pad strings to be returned to FORTRAN */
}

/* Oxford magnet */
int RS232TC_OX(char * wrstring, int * lwr, char * rdstring, int * lrd)
{
  char talkstring[80];
  int  len, n;
  
  len = *lwr;
  *lrd = 0;
  if (tc_fd <= 0) return 0;
  strncpy(talkstring,wrstring,len);
  talkstring[len++] = '\r';
  talkstring[len]   = 0;

  if (debug&1) { TRACE("RS232TC_OX: Send ->%s\n",talkstring); }
  if ((n = write(tc_fd,talkstring,len)) != len) {
    fprintf(stderr,"RS232_OX: unable to write full string!\n");
    return -1;
  }

  if ((len = terminated_read(tc_fd,talkstring,80,'\r')) < 0) {
    /* If an error occurs */
    if (debug&1) { TRACE("RS232TC_OX: Error = %d\n",len); }
    *rdstring = 0;
    *lrd = 0;
    serialFlush(tc_fd);
    return -1;
  }
  if (debug&1) { TRACE("RS232TC_OX: Recv ->%s\n",talkstring); }
  strncpy(rdstring,talkstring,len);
  *lrd = len-1;
  rdstring[len-1]=0;
  return (int)*lrd;
}

void GET_FLOAT(char * instring, int * len, float * fval, unsigned long * error)
{
  int n;
  char buf[80];
  *fval = 0;
  *error = 0;
  strncpy(buf,instring,*len);
  if ((n = sscanf(buf,"%f",fval)) != 1) *error = 1;
}

int OMCN_Inq(int addr, int page, int menu, char * data) 
{
  int cmd = 1, stat=0, vals = 2;
  int cksum = 0,len;
  char buf[80], *rptr;

  memset(buf,0,sizeof(buf));
  cksum = 0x100 - (( addr + cmd + stat + page + menu + vals) & 0xFF);
  sprintf(buf,"%02X%02X%02X%02X%02X%02X00%02X\r",addr,cmd,stat,menu,page,vals,cksum);
  len = strlen(buf);
  if (debug & 1) TRACE("OMCN_Inq: Send ->%s<-\n",buf);
  write(tc_fd,buf,len);

  /* Now read back from controller */
  if ((len = terminated_read(tc_fd, buf, sizeof(buf)-1, '\r')) < 0) {
    if (debug&1) TRACE("OMCN_Inq: Error = %d\n",len);
    return len;
  }
  if (debug & 1) TRACE("OMCN_Inq: Retn ->%s<-\n",buf);
  if (len >= 17) {
    rptr = (buf + 6);
    strncpy(data,rptr,8);
  }
  return 0;
}

int OMCN_Put(int addr, int page, int menu, int val) 
{
  int cmd = 8, stat=0;
  int cksum = 0,len;
  char buf[80];
  int byte1, byte2;

  memset(buf,0,sizeof(buf));
  byte1 = val & 0xFF;
  byte2 = (val & 0xFF00) >> 8;
  cksum = 0x100 - (( addr + cmd + stat + page + menu + byte1 + byte2) & 0xFF);
  sprintf(buf,"%02X%02X%02X%02X%02X%02X%02X%02X\r",
	  addr,cmd,stat,menu,page,byte1,byte2,cksum);
  len = strlen(buf);
  if (debug & 1) TRACE("OMCN_Put: Send ->%s<-\n",buf);
  write(tc_fd,buf,len);

  /* Now read back from controller */
  if ((len = terminated_read(tc_fd, buf, sizeof(buf)-1, '\r')) < 0) {
    if (debug&1) TRACE("OMCN_Put: Error = %d\n",len);
    return len;
  }
  if (debug & 1) TRACE("OMCN_Put: Retn ->%s<-\n",buf);

  return 0;
}

void OMCN_READTEMPS(float * sample, float * control, float * setpoint, int * err)
{
  int addr, page, menu, retn,len,i;
  unsigned int byte1, byte2, byte3, byte4;
  char data[32];
  float fval;

  addr = 1; page = 0;
  memset(data,0,sizeof(data));
  *sample   = 0;
  *control  = 0;
  *setpoint = 0;
  *err = 0;
  /* Read process variable for loop 2 (sample) */
  menu = 2;
  if ((retn = OMCN_Inq(addr, page, menu, data))==0) {
    len = strlen(data);
    if (len == 8) {
      sscanf(data,"%02X%02X%02X%02X",&byte1,&byte2,&byte3,&byte4);
      fval = (float) ((byte2 << 8) + byte1);
      for(i=0;i<byte3;i++) fval /= 10; /* Take care of decimal places */
    }
    *sample =  fval;
  } else {
    *err = 1;
    return;
  }

  /* Read setpoint */
  menu = 3;
  if ((retn = OMCN_Inq(addr, page, menu, data))==0) {
    len = strlen(data);
    if (len == 8) {
      sscanf(data,"%02X%02X%02X%02X",&byte1,&byte2,&byte3,&byte4);
      fval = (float) ((byte2 << 8) + byte1);
      for(i=0;i<byte3;i++) fval /= 10; /* Take care of decimal places */
    }
    *setpoint = fval;
  } else {
    *err = 1;
    return;
  }
  /* Read process variable for loop 1 (control) */
  menu = 1;
  if ((retn = OMCN_Inq(addr, page, menu, data))==0) {
    len = strlen(data);
    if (len == 8) {
      sscanf(data,"%02X%02X%02X%02X",&byte1,&byte2,&byte3,&byte4);
      fval = (float) ((byte2 << 8) + byte1);
      for(i=0;i<byte3;i++) fval /= 10; /* Take care of decimal places */
    }
    *control = fval;
  } else {
    *err = 1;
    return;
  }
}

void OMCN_WRITETEMP(float * setpoint, int * err) 
{
  int addr, page, menu, retn;
  int ival;

  addr = 1; page = 1; menu = 2;
  ival = (int) *setpoint;

  *err = 0;
  if ((retn = OMCN_Put(addr, page, menu, ival))!=0) {
    *err = 1;
    return;
  }
}

void OMCN_PIDR(float * P, float * I, float * D, float *R, int * err) 
{
  int addr, page, menu, retn;
  int ival;

  *err = 0;
  addr = 1; page = 2; menu = 3; /* Proportional Band */
  ival = (int) *P;              /* Range: 0.0 - 999.0 */
  if ((retn = OMCN_Put(addr, page, menu, ival))!=0) {
    *err = 1;
    return;
  }
  
  addr = 1; page = 2; menu = 4; /* Automatic Reset */
  ival = (int) *I;              /* Range: 0.00 - 99.99 */
  if ((retn = OMCN_Put(addr, page, menu, ival))!=0) {
    *err = 1;
    return;
  }

  addr = 1; page = 2; menu = 5; /* Rate */    
  ival = (int) *D;              /* Range: 0-500 */
  if ((retn = OMCN_Put(addr, page, menu, ival))!=0) {
    *err = 1;
    return;
  }

  addr = 1; page = 2; menu = 6; /* Output Limit */    
  ival = (int) *D;              /* Range: 0.0-100.0 */
  if ((retn = OMCN_Put(addr, page, menu, ival))!=0) {
    *err = 1;
    return;
  }

  return;
}

#define THERMO_SET_SETPOINT 0xF0
#define THERMO_GET_SETPOINT 0x70
#define THERMO_GET_INTERNAL 0x20
#define THERMO_GET_EXTERNAL 0x21

/* Sushil's temperature bath */
void Thermo_PutMsg(int cmd, int ndata, void * data) 
{
  int lbuf, isum, i;
  unsigned char ch, ck, *pdata;
  char buf[80], *pbuf;
  int retn;

  serialFlush(tc_fd);

  pbuf = &buf[0];
  ch = 0xCA;
  *pbuf++ = ch;
  *pbuf++ = (ch = 0x00);
  *pbuf++ = (ch = 0x01);
  isum = 0x01;
  ch = cmd;
  *pbuf++ = ch; isum += ch;
  ch = (unsigned char) (0xFF & ndata);
  *pbuf++ = ch; isum += ch;
  lbuf = 5;
  if (ndata > 0) {
    pdata = data;
    for (i=0;i<ndata;i++) {
      ch = *pdata++;
      *pbuf++ = ch; isum += ch; lbuf++;
    }
  }
  ck = (unsigned char) ( ~(0xFF & isum));
  *pbuf++ = ck; lbuf++;

  if (debug) {
    printf("Thermo_PutMsg sending:");
    for (i=0;i<lbuf;i++) {
      ch = buf[i];
      printf("<%02X>", ch);
    }
    printf("\n");
  }
  /* Now that the message is composed, send it off to the controller */
  if ((retn = write(tc_fd,buf,lbuf)) < 0) {
    perror("Thermo_PutMsg");
  }

}

int Thermo_GetMsg(int * cmd, int * nbytes, unsigned char * data)
{
  int i, n, isum;
  unsigned char ch, ck;
  
  isum = 0;
  if (debug) { printf("Thermo_GetMsg:"); fflush(stdout); }
  for (i=0;i<3;i++) {
    if ((n = timed_read(tc_fd, (char *) &ch, 1)) < 0) {
      if (debug) { printf("Read error %d\n",n); }
      /* Complain */
      return -1;
    }
    if (debug) printf("<%02X>",ch);
    if (i!= 0) isum += ch;
  }
  
  if ((n = timed_read(tc_fd, (char *) &ch, 1)) < 0) {
    /* Complain */
    if (debug) { printf("Read error %d\n",n); }
    return -1;
  }
  /* Extract command */
  if (debug) printf("<%02X>",ch);
  *cmd = ch;
  isum += ch;

  if ((n = timed_read(tc_fd, (char *) &ch, 1)) < 0) {
    /* Complain */
    if (debug) { printf("Read error %d\n",n); }
    return -1;
  }
  if (debug) printf("<%02X>",ch);
  *nbytes = ch;
  isum += ch;

  if (*nbytes > 0) {
    for (i=0;i<*nbytes;i++) {
      if ((n=timed_read(tc_fd, (char *) &ch,1)) < 0) {
	return -1;
      }
      if (debug) printf("<%02X>",ch);
      *(data + i) = ch;
      isum += ch;
    }
  }
  /* Calculate checksum */
  ck = (unsigned char) ( ~(0xFF & isum));

  /* Read Checksum */
  if ((n = timed_read(tc_fd, (char *) &ch, 1)) < 0) {
    if (debug) { printf("Read error %d\n",n); }
    /* Complain */
    return -1;
  }
  if (debug) printf("<%02X>",ch);  
  if (ck != ch) {
    if (debug) printf(" -- bad checksum 0x%02X/0x%02X",ck,ch);
  }
  if (debug) printf("\n");
  return *nbytes;
}

void THERMO_WRITETEMP(float * setpoint, int * err)
{
  int retn;
  unsigned int sval;
  int cmd, len;
  unsigned char buf[10], *ptr;

  *err=0;
  sval = (unsigned int) (*setpoint * 10.0);
  if (debug) printf("THERMO_WRITETEMP: sval = %hd (0x%04X)\n",sval,sval);
  ptr = &buf[0];
  *ptr++ = (unsigned char) ((0xFF00 & sval) >> 8);
  *ptr   = (unsigned char) (0x00FF & sval);
  

  Thermo_PutMsg(THERMO_SET_SETPOINT, 2, buf);
  usleep(5000); /* Wait 5 milliseconds */
  retn = Thermo_GetMsg(&cmd, &len, buf);
  if (retn < 0) *err = 1;
}

void THERMO_READTEMPS(float * sample, float * control, float * setpoint, 
		      int * err)
{
  unsigned char buf[10], qb, ch;
  unsigned int sval;
  int cmd, len,retn;
  float fval;

  *sample   = -999.9;
  *control  = -999.9;
  *setpoint = -999.9;
  *err = 0;
  Thermo_PutMsg(THERMO_GET_INTERNAL, 0, &sval);
  usleep(5000); /* Wait 5 milliseconds minimum */
  retn = Thermo_GetMsg(&cmd, &len, buf);
  if (retn < 0) {
    *err = 1;
    return;
  }
  qb = buf[0];
  ch = buf[1];
  sval = (ch << 8);
  ch = buf[2];
  sval += ch;
  if (debug) printf("THERMO_READTEMPS: sval=%hd (0x%04X)\n",sval,sval);
  fval = (float) sval / 10.0;
  if (qb > 0x11) fval /= 10.0;
  *control = fval;

  Thermo_PutMsg(THERMO_GET_EXTERNAL, 0, &sval);
  usleep(5000); /* Wait 5 milliseconds minimum */
  retn = Thermo_GetMsg(&cmd, &len, buf);
  if (retn < 0) {
    *err = 1;
    return;
  }
  qb = buf[0];
  ch = buf[1];
  sval = (ch << 8);
  ch = buf[2];
  sval += ch;
  if (debug) printf("THERMO_READTEMPS: sval=%hd (0x%04X)\n",sval,sval);
  fval = (float) sval / 10.0;
  if (qb > 0x11) fval /= 10.0;

  /* Fake out sample temp for now */
  *sample = fval;

  Thermo_PutMsg(THERMO_GET_SETPOINT, 0, &sval);  
  usleep(5000); /* Wait 5 milliseconds minimum */
  retn = Thermo_GetMsg(&cmd, &len, buf);
  if (retn < 0) {
    *err = 1;
    return;
  }

  qb = buf[0];
  ch = buf[1];
  sval = (ch << 8);
  ch = buf[2];
  sval += ch;
  if (debug) printf("THERMO_READTEMPS: sval=%hd (0x%04X)\n",sval,sval);
  fval = (float) sval / 10.0;
  if (qb > 0x11) fval /= 10.0;
  *setpoint = fval;

}

unsigned int OMCNi_encode(float setpoint) 
{
  typedef struct {
    unsigned data    : 20;
    unsigned decpoint: 3;
    unsigned signbit : 1;
    unsigned dummy   : 8;
  } code;

  union {
    code code;
    unsigned int icode;
  } val;
  unsigned int ival;

  val.code.dummy = 0;
  val.code.signbit = (setpoint < 0.0) ? 1 : 0;
  if (abs(setpoint) < 10.0) {          // F.FFF
    val.code.decpoint = 5;
    ival = abs(setpoint * 1000);
  } else if (fabsf(setpoint) < 100.0) {  // FF.FF
    val.code.decpoint = 3;
    ival = abs(setpoint * 100);
  } else if (fabsf(setpoint) < 1000.0) { // FFF.F
    val.code.decpoint = 2;
    ival = abs(setpoint * 10);
  } else if (fabsf(setpoint) < 10000.0) {// FFFF.
    val.code.decpoint = 1;
    ival = abs(setpoint);
  } else {
    val.code.decpoint = 0;
    ival = 0;
  }
  val.code.data = 0xFFFFFF & ival;
  if (debug) printf("OMCNi_encode: data = %d [%x]\n",ival,ival);
  return val.icode;
}

float OMCNi_decode(unsigned int data) 
{
  struct code {
    unsigned data    : 20;
    unsigned decpoint: 3;
    unsigned signbit : 1;
    unsigned dummy   : 8;
  };

  union {
    struct code code;
    unsigned int icode;
  } val;
  float fval;
  int signbit;

  val.icode = data;

  signbit = (val.code.signbit) ? -1 : 1;
  switch(val.code.decpoint) {
  case 5:
    fval = signbit * (((float) val.code.data)/1000.0);
    break;
  case 3:
    fval = signbit * (((float) val.code.data)/100.0);
    break;
  case 2:
    fval = signbit * (((float) val.code.data)/10.0);
    break;
  case 1:
    fval = signbit * (((float) val.code.data)/1.0);
    break;
  default:
    fval = 0;
  }
  
  return fval;
}

int OMCNi_Talk(char * inbuf, int l_in, char * outbuf, int *l_out, 
	       int replyexpected)
{
  int retn;
  int nw, nr;
  retn = -1; /* Failure */
  if (debug) printf("OMCNi_Talk: Send->%s<-\n",inbuf);
  strcat(inbuf,"\r");
  l_in++;
  if ((nw = write(tc_fd,inbuf,l_in)) < l_in) {
    if (debug) printf("OMCNi_Talk: Short write\n");
    return -1;
  }
  if (!replyexpected) return nw;

  nw = *l_out; /* Hopefully set to max size of output buffer */
  memset(outbuf,0,nw);
  if ((nr = terminated_read(tc_fd,outbuf,nw,'\n')) < 0) {
    switch(nr){
    case READ_BUF_OVFL:
      if (debug) printf("OMCNi_Talk: read overflow\n");
      break;
    case READ_TIMEOUT:
      if (debug) printf("OMCNi_Talk: read timeout\n");
      break;
    default:
      if (debug) printf("OMCNi_Talk: read error\n");
    }
    return -1;
  }
  
  /* Strip off the terminating carriage return and linefeed */
  if (nr > 0) {
    outbuf[nr-1] = 0;
    nr--;
    if (debug) printf("OMCNi_Talk: Recv->%s<-\n",outbuf);
  }

  return nr;
}

void OMCNI_READTEMPS(float * sample, float * control, float * setpoint, 
		      int * err)
{
  char outbuf[80], inbuf[80];
  int retn, lout, lin;
  unsigned int uval;
  double dval;
  char *ptr,*ptr2;

  *control = -999.0;
  *sample = -999.0;
  *setpoint = -999.0;
  *err = 0;

  /* Read Sample temperature */
  strcpy(outbuf,"*X01");
  lout = strlen(outbuf);
  lin = sizeof(inbuf);
  retn = OMCNi_Talk(outbuf,lout,inbuf,&lin,1);

  if (retn > 0) {
    //ptr = (inbuf+3);
    ptr = inbuf;
    *sample = atof(ptr);
  } else {
    *sample = -999.0;
    *err = 1;
    return;
  }

  *control = *sample;

  strcpy(outbuf,"*R01");
  lout = strlen(outbuf);
  lin = sizeof(inbuf);
  retn = OMCNi_Talk(outbuf,lout,inbuf,&lin,1);
  if (retn > 0) {
    ptr = (inbuf+3);
    uval = strtoul(ptr,&ptr2,16);
    dval = OMCNi_decode(uval);
    *setpoint = dval;
  } else {
    *setpoint = -999.0;
    *err = 1;
  }
  return;
}

void OMCNI_WRITETEMP(float * setpoint, int * err)
{
  char outbuf[80],inbuf[80];
  int retn, lout,lin;
  unsigned int uval;
  double dval;

  *err = 0;
  dval = *setpoint;

  uval = OMCNi_encode(dval);
  sprintf(outbuf,"*W01%06X",uval);
  lout = strlen(outbuf);
  lin = sizeof(inbuf);
  
  retn = OMCNi_Talk(outbuf,lout,inbuf,&lin,0);
  if (retn >= 0) {
    *err = 0;
  } else {
    *err = 1;
  }
  return;

}

/* Petit Fleur Temperature Bath */
void PF_QUERY(float * newset, float * setpoint, float * sample, float * control, int * err)
{
  char talkstring[128], ch, *tptr, hexstring[5];
  int iset, isamp, icont, ltalk, chksum,i,n;

  *setpoint = -999.0;
  *sample = -999.0;
  *control = -999.0;
  *err = 0;
  if (*newset < 0) {
    sprintf(hexstring,"****");
  } else {
    if (debug&1) { TRACE("PF_QUERY: Setpoint = %f\n",*newset); }
    iset = ((int) floor(100 * (*newset))) & 0x7FFF;
    sprintf(hexstring,"%04X",iset);
  }
  sprintf(talkstring,"[M01G0D**");
  strcat(talkstring,hexstring);
  chksum = 0;
  for (i=0;i<strlen(talkstring);i++) {
    ch = talkstring[i];
    chksum += (int) ch;
  }
  chksum &= 0xFF;
  sprintf(hexstring,"%02X\r",chksum);
  strcat(talkstring,hexstring);
  ltalk = strlen(talkstring);
  serialFlush(tc_fd); 
  if (debug&1) { TRACE("PF_QUERY: Send ->%s\n",talkstring); }
  if ((n = write(tc_fd,talkstring,ltalk)) != ltalk) {
    fprintf(stderr,"PF_QUERY: unable to write full string!\n");
    *err=1;
    return;
  }
  memset(talkstring,0,sizeof(talkstring));
  if ((ltalk = terminated_read(tc_fd,talkstring,sizeof(talkstring)-1,'\r')) < 0){
    /* If an error occurs */
    if (debug&1) { TRACE("PF_QUERY: Error = %d\n",ltalk); }
    *err = 2;
    return;
  }
  if (debug&1) { TRACE("PF_QUERY: Recvlen = %d\n",ltalk); }
  if (debug&1) { TRACE("PF_QUERY: Recv ->%s<-\n",talkstring); }
  

  if (ltalk > 20) {
    tptr = &talkstring[9];
    memset(hexstring,0,sizeof(hexstring));
    strncpy(hexstring,tptr,4);
    sscanf(hexstring,"%04X",&iset);

    *setpoint = ((float) iset) / 100;

    tptr = &talkstring[13];
    memset(hexstring,0,sizeof(hexstring));
    strncpy(hexstring,tptr,4);
    sscanf(hexstring,"%04X",&isamp);

    *sample = ((float) isamp) / 100;

    tptr = &talkstring[17];
    memset(hexstring,0,sizeof(hexstring));
    strncpy(hexstring,tptr,4);
    sscanf(hexstring,"%04X",&icont);

    *control = ((float) icont) / 100;
    *err = 0;
  } else {
    *err = 3;
  }
}
