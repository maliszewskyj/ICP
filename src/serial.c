/*
 * serial.c - A series of routines for interacting with serial ports
 *            serialOpen - opens port and sets up port parameters
 *            timed_read - reads from file descriptor with a timeout
 *            terminated_read - reads from file descriptor until terminator
 *                              character is encountered.
 *            For writing to the port, use write()
 *
 * Author: N. C. Maliszewskyj, NIST Center for Neutron Research, Jan 1997
 * $Id$
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <unistd.h>
#include <fcntl.h>
#include <termio.h>
#include <errno.h>
#include <alloca.h>
#include <signal.h>
#include <sys/ioctl.h>
#include <sys/types.h>
#include <sys/time.h>

#include "serial.h"

#define RTSCTS 0x8000000

extern int debug;

int
serialOpen(char *devicename, int baud, int databits, int parity, int flow) {
    struct termio termio;
    int termfd;

    if ((termfd = open(devicename, O_RDWR)) < 0) {
        perror("serialOpen:open");
        return termfd;          /* return bad fd */
    }

    switch(baud){
    case 300:  termio.c_cflag = B300;  break;
    case 1200: termio.c_cflag = B1200; break;
    case 2400: termio.c_cflag = B2400; break;
    case 4800: termio.c_cflag = B4800; break;
    case 9600: termio.c_cflag = B9600; break;
    case 19200: termio.c_cflag = B19200; break;
    case 38400: termio.c_cflag = B38400; break;
    default: perror("serialOpen:unsupported baud rate"); return -1;
    }

    switch(databits){
    case 7: termio.c_cflag |= CS7 | CREAD | CLOCAL; break;
    case 8: termio.c_cflag |= CS8 | CREAD | CLOCAL; break;
    default: perror("serialOpen:unsupported data bits"); return -1;
    }
    
    /* I think this is right */
    switch(parity) {
    case 1: termio.c_cflag |= PARODD | PARENB; break;
    case 2: termio.c_cflag |= PARENB; break;
    default: /* no parity */
      break;
    }

    switch(flow) {
    case 1: termio.c_cflag |= CRTSCTS; break; /* Hardware flow control */
    default: /* No hardware flow control */
      break;
    }

    termio.c_iflag = 0;
    termio.c_oflag = 0;
    termio.c_lflag = 0;
    termio.c_line  = 0;
    termio.c_cc[0] = 0;
    termio.c_cc[VMIN] = 64;
    termio.c_cc[VTIME] = 1;

    if (ioctl(termfd, TCSETA, &termio) != 0) perror("serialOpen:ioctl");
    return termfd;
}

/* Flush pending data to be read */
int
serialFlush(int fd) {
  return tcflush(fd, TCIFLUSH);
}

/* 
   timed_read : read on file descriptor fd with a timeout
*/
int
timed_read(int fd, char * buf, int count) {
  fd_set fds;
  int n;
  struct timespec timeout;
  sigset_t block;

  FD_ZERO(&fds); FD_SET(fd,&fds); timeout.tv_nsec=0; 
  timeout.tv_sec=SERIALTIMEOUT; 

  sigprocmask(SIG_BLOCK, NULL, &block);
  sigaddset(&block, SIGQUIT);
  sigaddset(&block, SIGTSTP);
  sigaddset(&block, SIGUSR1);

  if ((n = pselect(fd+1,&fds,(fd_set*)NULL,(fd_set*)NULL,&timeout,&block))<0) {
    if (debug & 2) printf("READ ERROR   fd=%d",fd);
    return READ_ERROR;
  } else if (n == 0) {
    if (debug & 2) printf("READ TIMEOUT fd=%d",fd);
    return READ_TIMEOUT;
  } else if (n > 0) {
    if ((n = read(fd, buf, count)) != count) { 
      return READ_ERROR; 
    } else {
      return n;
    }
  }
  return 0; /* Make the compiler happy */
}

/*
 * terminated_read - read from file descriptor fd until terminator is read
 */
int 
terminated_read(int fd, char * buf, int bufsz, char terminator) {
  char rcvchar;
  int len, i;

  len = 0;
  do {
    if (len > bufsz) return READ_BUF_OVFL;
    switch(i = timed_read(fd,buf,1)) {
    case 1: break;
    case READ_TIMEOUT: return READ_TIMEOUT; break;
    default: return READ_ERROR;
    }
    len++;
    rcvchar = *buf;
    if (debug) { 
      if (debug & 2) {
	if (isprint(rcvchar)) { putchar(rcvchar); } else { putchar(' '); }
      }
      if (debug & 4) printf("<%02x>",rcvchar); 
    }
  } while ( *buf++ != terminator);
  *buf = '\0';
  if ((debug&2) || (debug&4)) { printf("\n"); }
  
  return len;
}

/*
 * read from a file descriptor until any one of a set of characters is read
 */
int
termset_read(int fd, char * buf, int bufsz, char * termset) {
  char rcvchar, matchchar;
  int len, i;
  int match, matchlen;

  len = 0;
  matchlen = strlen(termset);
  match = 0;
  do {
    if (len > bufsz) return READ_BUF_OVFL;
    switch(i = timed_read(fd,buf,1)) {
    case 1: break;
    case READ_TIMEOUT: return READ_TIMEOUT; break;
    default: return READ_ERROR;
    }
    len++;
    rcvchar = *buf;
    if (debug) { 
      if (debug & 2) {
	if (isprint(rcvchar)) { putchar(rcvchar); } else { putchar(' '); }
      }
      if (debug & 4) printf("<%02x>",rcvchar); 
    }
    for (i=0;i<matchlen;i++) {
      matchchar = termset[i];
      if (rcvchar == matchchar) match = 1;
    }
    buf++;
  } while ( !match );
  *buf = '\0';
  if ((debug&2) || (debug&4)) { printf("\n"); }
  
  return len;
}
