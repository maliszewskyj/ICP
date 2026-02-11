/*
 * rs232_x - serial communication with a scintag x-ray diffractometer
 *
 *
 *
 *
 *
 *
 *
 *
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <signal.h>
#include <errno.h>
#include <sys/time.h>
#include <sys/types.h>
#include "serial.h"
#include "rs232.h"
#include "instrext.h"

extern RS232PARM rs232_cnf[NUMRS232];

#define BUFSIZE 80
static int xfd     =0;
static int recvsig = 0;
extern int debug;

void xsighandler(int signum) {
  switch(signum) {
  case SIGINT:
    printf("Received SIGINT\n");
    recvsig=1;
    break;
  default:
    printf("Received bad signal: %d\n",signum);
  }
}

/* 
 * Initialize serial port 
 */
int
RS232_XINIT(){
  /* Get port number from FORTRAN side through "instr" common block */
  int portno;
  portno = mot_line[2];
  if (debug) printf("RS232_XINIT: x   portno = %d device = %s\n",
		    portno,rs232_cnf[portno].dev);
  if (portno < 0) {
    fprintf(stderr,"RS232_XINIT: Can't initialize portno %d\n",portno);
    exit(2);
  }
  if ((xfd = serialOpen(rs232_cnf[portno].dev, 
			rs232_cnf[portno].baud, 
			rs232_cnf[portno].bits, 
			rs232_cnf[portno].parity,0)) < 0) {
    fprintf(stderr,"RS232_XINIT: open() failed!\n");
    exit(2);
  }
  return xfd;
}

int
RS232XEND(){
  if (xfd > 0) close(xfd);
  xfd = 0;
  return 0;
}

void 
rs232x_synchronize(void) {
  char ch, cc=3; /* control-C for abort */
  char talkstring[BUFSIZE];
  fd_set fds;
  struct timeval timeout;
  int retn, icount;

  FD_ZERO(&fds); FD_SET(xfd,&fds); 
  timeout.tv_usec=1000; timeout.tv_sec=0; /* Allow 1ms between reads */
  memset(talkstring,0,sizeof(talkstring));
  talkstring[0] = cc;
  if ((retn = write(xfd,talkstring,1)) < 0) {
    perror("rs232x_synchronize: write 1");
  }
  
  /* Loop here, checking for a '=' response */
  icount = 0;
  while (((ch = talkstring[0]) != '=') && (icount < 5)) {
    sprintf(talkstring,"=\r");
    if ((retn = write(xfd,talkstring,2)) < 0) {
      perror("rs232x_synchronize: write 2");
    }
    talkstring[0] = 0;
    if ((retn = select(xfd+1,&fds,(fd_set*)NULL,(fd_set*)NULL,&timeout)) > 0) {
      if ((retn = read(xfd,talkstring,sizeof(talkstring)-1)) > 0) {
	talkstring[retn] = 0;
	if (debug) printf("Received: %s\n\n",talkstring); 
      } 
    } else {
      if (debug) {
	printf("rs232x_synchronize: select failed. retn = %d\n",retn);
      }
      usleep(1000);
      FD_ZERO(&fds); FD_SET(xfd,&fds); 
      timeout.tv_usec=0; timeout.tv_sec=1; /* Allow 1ms between reads */
    }
    icount++;
    printf("SYNCHRONIZING\n");
    usleep(10000); /* Sleep for 10 ms */
  }
  /* Wait 10 seconds for motors to finish */
  /* sleep(10); */
}

int
RS232X(char * wrstring, int * lwr, char * rdstring, int * lrd, int * secwait,
       int * reply){
  char ch, talkstring[BUFSIZE];
  struct sigaction sa;
  fd_set fds;
  struct timeval timeout;
  int  len, nr, retn, i, icount ;

  *lrd = 0; 	/* Default return */
  *reply = 0; 	/* Default return */
  /* Install temporary signal handler */
  recvsig=0;
  sa.sa_handler = xsighandler;
  
  len = *lwr;
  memset(talkstring,0,sizeof(talkstring));
  strncpy(talkstring,wrstring,len);
  talkstring[len++] = '\r';         /* Append carriage return as terminator */
  talkstring[len]   =   0;

  /* Write out command string */
  if(debug) printf("Sending: %s\n",talkstring); 
  if ((nr = write(xfd,talkstring,len)) < 0) {
    *rdstring = 0;
    *lrd = 0;
    /* sigaction(SIGINT ,&osa,&sa); Reinstall the old signal handler */
    return -1;
  }

  /* Get back reply or detect abort signal */
  memset(talkstring,0,sizeof(talkstring));
  
  FD_ZERO(&fds); 
  FD_SET(xfd,&fds); 
  timeout.tv_usec=0; 
  timeout.tv_sec=*secwait; 

  usleep(10000); /* Sleep for 10 ms */
  
  if (debug) printf("Hanging on select()\n");
  if ((retn = select(xfd+1,&fds,(fd_set*)NULL,(fd_set*)NULL,&timeout)) < 0) {
    //switch(errno) {
    //case EINTR: /* User abort */
      //printf("Aborting current command\n");
      //rs232x_synchronize();
      /* sigaction(SIGINT ,&osa,&sa); Reinstall the old signal handler */
      //return -2;
      //default:
      perror("RS232X:select");
      /* sigaction(SIGINT ,&osa,&sa); Reinstall the old signal handler */
      return -3;
      //}
  } else if (retn==0) {
    /* Timed out */
    /* Not sure what to do here */
    strncpy(talkstring,wrstring,*lwr);
    talkstring[*lwr] = 0;
    printf("No reply received for: %s\n",talkstring);
    *lrd = 0;
    *rdstring=0;
    /* sigaction(SIGINT ,&osa,&sa); Reinstall the old signal handler */
    return -4;
  } 

  retn = read(xfd,talkstring,BUFSIZE-1);
  *lrd = retn;
  talkstring[retn] = 0;
  strcpy(rdstring,talkstring);


  if (debug) {
     printf("First read: %s\n\n",talkstring);
     for (i=0;i<strlen(talkstring);i++) {
	ch = talkstring[i];
	printf("<%02x>",(0xff & ch));
     }

    printf("\n");
  }

  /*   
  if ((ch = rdstring[0]) != '=') {
    timeout.tv_usec=0; 
    timeout.tv_sec=*secwait; 
    if ((retn = select(xfd+1,&fds,(fd_set*)NULL,(fd_set*)NULL,&timeout))>0) {
      retn = read(xfd,talkstring,BUFSIZE-1);
      talkstring[retn] = 0;
    }
    if (debug) {
       printf("Second read: %s\n\n",talkstring);
       for (i=0;i<strlen(talkstring);i++) {
	  ch = talkstring[i];
	  printf("<%02x>",(0xff & ch));
       }
       printf("\n");
    }
  }    
*/ 
 /* Write final acknowledgement */
  sprintf(talkstring,"=\r");
  retn = write(xfd,talkstring,2);

  usleep(10000); /* Wait for another 10 ms */
  /* sigaction(SIGINT ,&osa,&sa); Reinstall the old signal handler */
  
  icount =0;
  while (((ch=rdstring[icount])!='=')&&(rdstring[icount]!=0)&&(icount<*lrd)) {
	icount++;
  }
  if ( ((ch=rdstring[icount]) == '=') || (rdstring[icount]==0)) {
	*lrd = icount-1;
  }
  return *lrd;
}

