/*
  SNS DAS DFS Data Forwarding and Command Request Interface
  Cobbled together in a hurry from Rick Riedel's documentation 18 Oct 2005
    AKA The Book of Ron
  
  Author: Nick Maliszewskyj, NIST Center for Neutron Research <nickm@nist.gov>
  
 */

#include <stdio.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <arpa/inet.h>
#include <netdb.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include <errno.h>
#include <time.h>
#include <signal.h>
#include "icp.h"
#include "dfs.h"

#ifdef STANDALONE
int debug = 0;
#else
extern int debug;
#endif

#define DFS_HOST "snsdet"

static unsigned long dfs_inaddr = 0;
static int dfs_command_sock;
static int dfs_notify_sock;

void DFS_Sigalrm(int sig)
{
  fprintf(stderr,"DFS_Sigalrm %d\n",sig);
  return;
}

/* Setup "connection" to DFS server. 
   Open local socket and store address of remote host */
int DFS_Setup(char * host) 
{
  int bufsize = MAXUDPBUFFER;
  struct hostent *hp;
  struct sockaddr_in servaddr;
  struct timeval tv;

  srand(getpid());

  // Store information about the host
  if ((dfs_inaddr = inet_addr(host)) != INADDR_NONE) {/* It's dotted decimal */
  } else {                                           /* It's a named address */
    if ((hp = gethostbyname(host)) == NULL) {
      perror("DFS_Setup (gethostbyname)");
      return -1;
    }
    bcopy(hp->h_addr, (char *) &dfs_inaddr, hp->h_length);
  }

  if (debug) printf("DFS_Setup: Host = %s (%lx)\n",host,dfs_inaddr);

  bzero(&servaddr, sizeof(servaddr));
  servaddr.sin_family = AF_INET;
  servaddr.sin_port = htons(DAS_NOTIFY_PORT);
  servaddr.sin_addr.s_addr = htonl(INADDR_ANY);
  if ((dfs_notify_sock = socket(AF_INET, SOCK_DGRAM, 0)) < 0) {
    perror("DFS_Setup (socket)");
    return -1;
  }
  bind(dfs_notify_sock, (struct sockaddr *)&servaddr, sizeof(servaddr));

  setsockopt(dfs_notify_sock, SOL_SOCKET, SO_SNDBUF, &bufsize,sizeof(bufsize));
  setsockopt(dfs_notify_sock, SOL_SOCKET, SO_RCVBUF, &bufsize,sizeof(bufsize));
  /* Set timeout on read */
  tv.tv_sec  = (int) DFS_TIMEOUT;
  tv.tv_usec = 0;
  setsockopt(dfs_notify_sock, SOL_SOCKET, SO_RCVTIMEO, &tv, sizeof(tv));

  if ((dfs_command_sock = socket(AF_INET, SOCK_DGRAM, 0)) < 0) {
    perror("DFS_Setup (socket)");
    return -1;
  }
  return 0;
}

int DFS_Reopen()
{
  int bufsize = MAXUDPBUFFER;
  struct sockaddr_in servaddr;
  struct timeval tv;

  printf("DFS_Reopen: Reopening listening socket\n");
  close(dfs_notify_sock);
  dfs_notify_sock = 0;

  bzero(&servaddr, sizeof(servaddr));
  servaddr.sin_family = AF_INET;
  servaddr.sin_port = htons(DAS_NOTIFY_PORT);
  servaddr.sin_addr.s_addr = htonl(INADDR_ANY);
  if ((dfs_notify_sock = socket(AF_INET, SOCK_DGRAM, 0)) < 0) {
    perror("DFS_Reopen (socket)");
    return -1;
  }
  bind(dfs_notify_sock, (struct sockaddr *)&servaddr, sizeof(servaddr));

  setsockopt(dfs_notify_sock, SOL_SOCKET, SO_SNDBUF, &bufsize,sizeof(bufsize));
  setsockopt(dfs_notify_sock, SOL_SOCKET, SO_RCVBUF, &bufsize,sizeof(bufsize));
  /* Set timeout on read */
  tv.tv_sec  = (int) DFS_TIMEOUT;
  tv.tv_usec = 0;
  setsockopt(dfs_notify_sock, SOL_SOCKET, SO_RCVTIMEO, &tv, sizeof(tv));
  return 0;
}

int DFS_Send(RXPACKET * rx)
{
  struct sockaddr_in servaddr;
  int msglen, retn;
  /* Now that the message has been composed, ship it out. */
  if (dfs_inaddr ==0) return -1; /* Uninitialized */
  bzero(&servaddr, sizeof(servaddr));
  servaddr.sin_family = AF_INET;
  servaddr.sin_port   = htons(DAS_CMDREQ_PORT);
  servaddr.sin_addr.s_addr = dfs_inaddr; 
  
  msglen = sizeof(HEADER) + rx->header.length;

  retn = sendto(dfs_command_sock, rx, msglen, 0, 
		(struct sockaddr *) &servaddr, sizeof(servaddr));
  if (retn < 0) {
    perror("DFS_Send");
    return retn;
  } else if (retn != msglen) {
    fprintf(stderr,"DFS_Send: Incomplete message sent");
    return -1;
  } else {
    return 0;
  }
  return 0; /* Not reached */
}

/* Return number of bytes read */
int DFS_Recv(RXPACKET * rx)
{
  struct sockaddr_in servaddr;
  int nbytes;
  socklen_t len;

  
  bzero(rx,sizeof(RXPACKET));
  bzero(&servaddr, sizeof(servaddr));
  len = sizeof(servaddr);
  //  alarm((int) timeout);
  
  nbytes = 0;
  nbytes = recvfrom(dfs_notify_sock, rx, sizeof(RXPACKET), 
		      0, (struct sockaddr *) &servaddr, &len);
  if (nbytes < 0) {
    if (errno == EINTR) {
      fprintf(stderr,"DFS_Recv: interrupted by signal\n");
      return -1;
    } else if (errno == EAGAIN) {
      fprintf(stderr,"DFS_Recv: timed out\n");
      return -1;
    } else {
      fprintf(stderr,"DFS_Recv: errno = %d\n",errno);
      perror("DFS_Recv");
      return nbytes;
    }
  } 

  if (nbytes > 0) { /* received valid message */
    return nbytes;
  } else { /* nbytes == 0 */
    fprintf(stderr,"DFS_Recv: timed out\n");
    return 0;
  }
  return nbytes; /* Not reached */
}

void DFS_Print(const char * purpose, RXPACKET rx)
{
  char buf[MAXUDPBUFFER];

  if (debug) {
    printf("%s\n",purpose);
    printf("  rx.header.iReceiveID = 0x%08X\n",rx.header.iReceiveID);
    printf("  rx.header.iCommandID = 0x%08X\n",rx.header.iCommandID);
    printf("  rx.header.length     = %d\n",rx.header.length);
    printf("  rx.header.Spare1     = %d\n",rx.header.Spare1);
    printf("  rx.header.Spare2     = %d\n",rx.header.Spare2);
    printf("  rx.header.Spare3     = %d\n",rx.header.Spare3);
    bzero(buf,sizeof(buf));
    bcopy(rx.buffer,buf,rx.header.length);
    printf("  rx.buffer            = \"%s\"\n",buf);
  }
}

int DFS_Save(char * title, char * comments, int * runnumber)
{
  RXPACKET rx;
  int req, retn;

  memset(&rx, 0, sizeof(rx));
  req = rand();
  rx.header.iReceiveID = req;
  rx.header.iCommandID = REQUESTSAVE;
  rx.header.Spare1 = 0x02; 
  rx.header.Spare2 = 1;         /* Run number */
  rx.header.Spare3 = 1;         /* Step number */

  sprintf(rx.buffer,"%s><%s", title, comments);
  rx.header.length = strlen(rx.buffer);

  DFS_Print("DFS_Save (send)",rx);
  if ((retn = DFS_Send(&rx)) < 0) {
    //    return retn; /* No point in continuing */
  }
  /* Read command acknowledgement */
  if ((retn = DFS_Recv(&rx)) > 0) {
    DFS_Print("DFS_Save (ack)", rx);
  } else {
    //return -1;
  }
  if (rx.header.iCommandID & ERROR) {
    printf("DFS_Save (error)\n");
    //return -1;
  } 
 
  /* Now wait for actual indication that the action is proceeding */
  *runnumber = 0;
  if ((retn = DFS_Recv(&rx)) > 0) {
    DFS_Print("DFS_Save (action)", rx);
    if ((rx.header.iReceiveID != 1) || 
	(rx.header.iCommandID != DASSAVED)) {
      printf("DFS_Save: bad iReceiveID or bad iCommandID\n");
      *runnumber = -1;
      DFS_Reopen();
    } else {
      *runnumber = rx.header.Spare1;
    }
  } else {
    //return -1;
  }

  return 0; /* Success */
}

int DFS_Start()
{
  RXPACKET rx;
  int req, retn;
  memset(&rx, 0, sizeof(rx));
  req = rand();
  rx.header.iReceiveID = req;
  rx.header.iCommandID = REQUESTSTART;

  DFS_Print("DFS_Start (send)",rx);
  if ((retn = DFS_Send(&rx)) < 0) {
    //return retn; /* No point in continuing */
  }
  /* Read command acknowledgement */
  if ((retn = DFS_Recv(&rx)) > 0) {
    DFS_Print("DFS_Start (ack)", rx);
  } else {
    //return -1;
  }
  if (rx.header.iCommandID & ERROR) {
    DFS_Print("DFS_Start (error)", rx);
    //return -1;
  } 

  /* Now wait for actual indication that the action is proceeding */
  if ((retn = DFS_Recv(&rx)) > 0) {
    DFS_Print("DFS_Start (action)", rx);
    if ((rx.header.iReceiveID != 1) || 
	(rx.header.iCommandID != DASSTARTED)) {
      printf("DFS_Start: bad iReceiveID or bad iCommandID\n");
    }
  } else {
    return -1;
  }

  return 0; /* Success */
}

int DFS_Stop()
{
  RXPACKET rx;
  int req, retn;
  memset(&rx, 0, sizeof(rx));
  req = rand();
  rx.header.iReceiveID = req;
  rx.header.iCommandID = REQUESTSTOP;

  DFS_Print("DFS_Stop (send)",rx);
  if ((retn = DFS_Send(&rx)) < 0) {
    //    return retn; /* No point in continuing */
  }
  /* Read command acknowledgement */
  if ((retn = DFS_Recv(&rx)) > 0) {
    DFS_Print("DFS_Stop (ack)", rx);
  } else {
    //return -1;
  }
  /* Check acknowledgement code */
  if (rx.header.iCommandID & ERROR) {
    DFS_Print("DFS_Stop (error)", rx);
    //return -1;
  }

  /* Now wait for actual indication that the action is proceeding */
  if ((retn = DFS_Recv(&rx)) > 0) {
    DFS_Print("DFS_Stop (action)", rx);
    if ((rx.header.iReceiveID != 1) || 
	(rx.header.iCommandID != DASSTOPPED)) {
      printf("DFS_Stop: bad iReceiveID or bad iCommandID\n");
    }
  } else {
    return -1;
  }

  return 0; /* Success */
}

int DFS_INIT()
{
  return DFS_Setup(DFS_HOST);
}

int DFS_ARM()
{
  int retn;
  retn = DFS_Start();
  return retn;
}

int DFS_DISARM()
{
  int retn;
  retn = DFS_Stop();
  return retn;
}

/* This is a cheat. We're really just saving data on the data file server */
int DFS_XFER(int * runnumber)
{
  int retn;
  char title[80], comment[80];
  strcpy(title,"NCNR Detector tests");
  strcpy(comment,"Tests of differential nonlinearity in positioning");
  retn = DFS_Save(title,comment,runnumber);
  return retn;
}

/* Test program */

#ifdef STANDALONE
#include <getopt.h>

char host[80];

int main(int argc, char * argv[])
{
  char ch;
  char title[80], comment[80];
  int dostart=0,dostop=0,dosave=0, runnumber=1;
  
  strcpy(host,"127.0.0.1");
  strcpy(title,"Example title");
  strcpy(comment,"Example comment");
  while((ch = getopt(argc, argv, "h:r:stST:c:v?")) != -1) {
    switch(ch) {
    case 'h': 
      strcpy(host,optarg);
      break;
    case 'r': 
      runnumber = atoi(optarg);
      break;
    case 's': 
      dostart = 1;
      break;
    case 't': 
      dostop = 1;
      break;
    case 'S':
      dosave = 1;
      break;
    case 'T':
      strcpy(title,optarg);
      break;
    case 'c':
      strcpy(comment,optarg);
      break;
    case 'v':
      debug=1;
      break;
    default:
      fprintf(stderr, "Usage: dfs [[-h hostname] [-r runno] -stS]\n");
      fprintf(stderr, "            -h hostname       DFS host name or IP\n");
      fprintf(stderr, "            -r runnumber      Run number for saves\n");
      fprintf(stderr, "            -T title          Title for save file\n");
      fprintf(stderr, "            -c comment        Comment for save file\n");
      fprintf(stderr, "            -s                Start acquisition\n");
      fprintf(stderr, "            -t                Stop  acquisition\n");
      fprintf(stderr, "            -S                Save run file\n");
      fprintf(stderr, "            -v                Run with lotsa words\n");
      exit(1);
    }
  }

  DFS_Setup(host);
  if (dostart) DFS_Start();
  if (dostop) DFS_Stop();
  if (dosave) DFS_Save(title,comment, &runnumber);
  
  return 0;
}
#endif
