/*
 * Support for National Instruments GPIB ENET controller
 *
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <errno.h>
#include <time.h>
#include "nienet/ugpib.h"

#define MTA(x) (0x40 + x)
#define MLA(x) (0x20 + x)
#define CHUNK  256

extern int debug;
static int master=0;
static int dev[32];
static int curdev=-1;
static int ierr=0;

static void gpiberr(char *msg) {
  char outptr[80];

  switch (ierr) {
  case EDVR: 
    sprintf(outptr, " EDVR <Error>\n"); 
    break;
  case ECIC:
    sprintf(outptr, " ECIC <Not CIC>\n");
    break;
  case ENOL:
    sprintf(outptr, " ENOL <No Listener>\n");
    break;
  case EADR:
    sprintf(outptr, " EADR <Address error>\n");
    break;
  case EARG:
    sprintf(outptr, " EADR <Address error>\n");
    break;
  case ESAC:
    sprintf(outptr, " ESAC <Not Sys Controller>\n");
    break; 
  case EABO:
    sprintf(outptr, " EABO <Op. aborted>\n");
    break;
  case ENEB:
    sprintf(outptr, " ENEB <No GPIB board>\n");
    break;
  case EDMA:
    sprintf(outptr, " EDMA <DMA hardware err>\n");
    break;
  case EBTO:
    sprintf(outptr, " EBTO <DMA uP timeout>\n");
    break;
  case EOIP:
    sprintf(outptr, " EOIP <IO already in progress>\n");
    break;
  case ECAP:
    sprintf(outptr, " ECAP <No capability>\n");
    break;
  case EFSO:
    sprintf(outptr, " EFSO <File sys. error>\n");
    break;
  case EOWN:
    sprintf(outptr, " EOWN <Board already owned>\n");
    break;
  case EBUS:
    sprintf(outptr, " EBUS <Bus error>\n");
    break;
  case ESTB:
    sprintf(outptr, " ESTB <Status byte lost>\n");
    break;
  case ESRQ:
    sprintf(outptr, " ESRQ <SRQ stuck on>\n");
    break;
  case ETAB:
    sprintf(outptr, " ETAB <Table overflow>\n");
    break;
  case ELCK:
    sprintf(outptr, " ELCK <Locked>\n");
    break;
  }

  fprintf(stderr,"%s%s",msg, outptr);
}

void ENET_INIT() 
{
  
  if (master <= 0) {
    //master = ibfind("gpib0"); /* "gpib0" must be in the /etc/hosts file */ 
    //  master = ibdev(0, 0, 0, T10s, 1, 0);
    if (master < 0) {
      /* Complain */
      gpiberr("ENET_INIT: ");
      return;
    }
    memset(dev,0,sizeof(dev));
  }
  //ibsic(master);
  //ibcac(master, 1);
}

void GPIBEN_LISTEN(int * addr, int * err) 
{
  unsigned char ch;
  if ((*addr < 0) || (*addr > 31)) return;

  *err=0;
  if (debug) printf("GPIBEN_LISTEN MLA(%d)\n",*addr);
  ch = MLA(*addr); //*err = ibcmd(master,&ch,1); /* Set device to be listener */
  ch = MTA(0);     //*err = ibcmd(master,&ch,1); /* Set controller to be talker*/

  if (dev[*addr]<=0) {
    //if ((curdev = ibdev(0, *addr, NO_SAD, T10s, 1, 0)) < 0) {
    //  gpiberr("GPIBEN_LISTEN (ibdev): ");
    //}
    dev[*addr] = curdev;
  } else {
    curdev = dev[*addr];
  }

}

void GPIBEN_UNL(int * err) 
{
  unsigned char ch;
  if (debug) printf("GPIBEN_UNL\n");
  ch = (unsigned char) UNL;
  //*err = ibcmd(master, &ch, 1);
  //  curdev = NULL;
}

void GPIBEN_TALK(int * addr, int * err) 
{
  unsigned char ch;
  if ((*addr < 0) || (*addr > 31)) return;

  *err=0;
  if (debug) printf("GPIBEN_LISTEN MLA(%d)\n",*addr);
  ch = MTA(*addr); //*err = ibcmd(master,&ch,1); /* Set device as talker */
  ch = MLA(0);     //*err = ibcmd(master,&ch,1); /* Set controller as listener*/

  if (dev[*addr]<=0) {
    //    if ((curdev = ibdev(0 , *addr, NO_SAD, T10s, 0, 0)) < 0) {
    //  gpiberr("GPIBEN_TALK (ibdev): ");
    //}
    curdev=0;
    dev[*addr] = curdev;
  } else {
    curdev = dev[*addr];
  }
}

void GPIBEN_UNT(int * err) 
{
  unsigned char ch;
  ch = (unsigned char) UNT;
  //*err = ibcmd(master,&ch,1);
  //  curdev = NULL;
}

/* Send "Group Execute Trigger" */
void GPIBEN_EXECUTE(int * err) 
{
  unsigned char ch;
  if (debug) printf("GPIBEN_EXECUTE\n");
  ch = GET;
  //*err = ibcmd(master,&ch,1);
}

int GPIBEN_WR(char * string, int * length, int * err) 
{
  int n;
  char buf[80];
  *err = 0;

  
  memset(buf,0,sizeof(buf));
  memcpy(buf,string,*length);
  if (debug) printf("GPIBEN_WR: dev=%d Sending %d bytes ->%s<-\n",
		    curdev,*length, buf);
  //n = ibwrt(curdev, buf, (long) *length);
  //  if (ibsta & ERR) {
  // gpiberr("GPIBEN_ER (ibwrt): ");
  // *err = iberr;
  //}
  //if (debug) printf("GPIBEN_WR: ibsta = 0x%x \n",ibsta);
  //return ibcntl;
  return 0;
}

int GPIBEN_RD(char * string, int * expected, int * length, int * err) 
{
  int n,i;
  time_t t1, t2;
  *err = 0;

  *length = 0;
  time(&t1);
  //n = ibrd(curdev, string, (long) *expected);
  //if (ibsta & ERR) {
  //gpiberr("GPIBEN_RD (ibrd): ");
  //*err = iberr;
  //return ibcntl;
  //return 0;
  //}
  //*length = ibcntl;
  *length=0;
  //if (debug) printf("GPIBEN_RD: Read %ld bytes\n",ibcntl);
  time(&t2);

  if (debug) printf("GPIBEN_RD: Duration %ld sec\n",(long)(t2-t1));
  if (debug & 2) {
    //for (i=0;i<ibcntl;i++) {
    //  char ch = string[i];
    //  if (i%2 == 0) { printf("%d %d ",i,ch); } else printf("%d\n",ch);
    //}
  }

  return n;
}


int GPIBEN_RDBLK(char * string, int * expected, int * length, int * err) 
{
  int n, nchunks, nlastchunk, total, i;
  char * ptr;
  time_t t1, t2;
  *err = 0;

  *length = 0;

  if (debug) printf("GPIBEN_RD: Request %d bytes from %d\n",*expected,curdev);

  nchunks = *expected / CHUNK;
  nlastchunk = *expected % CHUNK;
  if (debug) printf("GPIBEN_RD: Read %d chunks of %d bytes each\n",
		    nchunks,CHUNK);
  time(&t1);
  ptr = string;
  total = 0;
  if (nchunks) {
    //for (i=0;i<nchunks;i++) {
      //n = ibrd(curdev,ptr,CHUNK);
      //if (ibsta & ERR) {
      //	gpiberr("GPIBEN_RD (ibrd): ");
      //	//*err = iberr;
      //	return -1;
      //}
    //ptr += ibcntl;
    // total += ibcntl;
    //}
  }

  if (nlastchunk) {
    //n = ibrd(curdev,ptr, CHUNK);
    //if (ibsta & ERR) {
      /* Complain */
    //  gpiberr("GPIBEN_RD (ibrd): ");
      //*err = iberr;
    //  return -2;
    //    } 
    //total += ibcntl;
  }

  *length = total;
  if (debug) printf("GPIBEN_RD: Read %d bytes\n",total);
  time(&t2);

  if (debug) printf("GPIBEN_RD: Duration %ld sec\n",(long)(t2-t1));

  return n;
}
