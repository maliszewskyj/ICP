#include <netinet/in.h>
#include <arpa/inet.h>
#include <sys/types.h>
#include <sys/time.h>
#include <sys/socket.h>
#include <string.h>
#include <netdb.h>
#include <signal.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <stdint.h>
#include "spec_server.h"

int cmdno = 0;
static int spec_port=6510;
static int specfd;
//static char * spec_host = "129.6.120.83";
static char * spec_host = "127.0.0.1";
static struct sockaddr_in tcp_srv_addr;  /* Server's socket address */
static struct servent     tcp_serv_info; /* from getservbyname() */
static struct hostent     tcp_host_info; /* from gethostbyname() */

static struct svr_head svh;
static struct svr_head rsp;

int Spec_Connect()
{
  unsigned long inaddr;

  if ((specfd=socket(AF_INET,SOCK_STREAM,0))<0) {
    fprintf(stderr,"Cannot create client socket\n");
    return -1;
  }

  if ((inaddr = inet_addr(spec_host)) != INADDR_NONE) {
    bcopy((char *) &inaddr, (char *) &tcp_srv_addr.sin_addr, sizeof(inaddr));
    tcp_host_info.h_name = NULL;
  }

  /* Connect to server */
  tcp_srv_addr.sin_family = AF_INET;
  tcp_srv_addr.sin_port = htons(spec_port);
  if (connect(specfd,(struct sockaddr *)&tcp_srv_addr,sizeof(tcp_srv_addr))<0)
    {
      fprintf(stderr,"Cannot connect to server");
      close(specfd);
      return -1;
    }


  return specfd;
}

int Spec_Disconnect()
{
  struct timeval tv;
  int n, bytes;

  gettimeofday(&tv,NULL);
  svh.magic = SV_SPEC_MAGIC;
  svh.vers  = SV_VERSION;
  svh.size  = 132;
  svh.sn    = cmdno++;
  svh.sec   = tv.tv_sec;
  svh.usec  = tv.tv_usec;

  svh.cmd   = SV_CLOSE;
  n = sizeof(svh);
  printf("Spec_Close()\n");

  if ((bytes = write(specfd, &svh, n)) != n) {
    fprintf(stderr,"Short write\n");
    // return -1;
  }
  close(specfd);

  specfd = -1;
  return 0;
}

int Spec_SendAndReceive(int sockfd, int cmd, char * input, int linput,
			int *err, char * output, int *loutput)
{
  struct timeval tv, timeout;
  int n, bytes, len;
  char data[512], *pdata;
  fd_set fds;

  gettimeofday(&tv,NULL);
  svh.magic = SV_SPEC_MAGIC;
  svh.vers  = SV_VERSION;
  svh.size  = 132;
  svh.sn    = cmdno++;
  svh.sec   = tv.tv_sec;
  svh.usec  = tv.tv_usec;
  svh.cmd   = cmd;
  svh.type  = SV_STRING;

  memset(data,0,sizeof(data));
  strncpy(data,input,linput);
  pdata = &data[0];
  len = strlen(data);
  svh.len = len+1;
  svh.cols=len+1;
  svh.rows=1;
 
  n = sizeof(svh);
  printf("Spec_SendAndReceiveCommand %s\n",data);
  //PrintSpec("SVH",&svh);
  //printf("INPUT = %s\n",data);

  if ((bytes = write(sockfd, &svh, n)) != n) {
    fprintf(stderr,"Short write\n");
    // return -1;
  }
  if (len > 0) {
    // Now write data
    if ((bytes = write(sockfd, pdata, len+1)) != len+1) {
      fprintf(stderr,"Short data write\n");
    }
  }

  memset(&rsp,0,sizeof(rsp));
  len = 0;
  if ((n = read(sockfd, &rsp, sizeof(rsp))) != sizeof(rsp)) {
    fprintf(stderr,"Short read of spec packet header");
    return -2;
  }
  // PrintSpec("RSP",&rsp);
  *err = rsp.err;
  len = rsp.len;
  memset(data,0,sizeof(data));
  pdata = &data[0];
  if (len > 0) {
    bytes = read(sockfd, pdata,len);
    printf(" OUTPUT = %s\n",data);
  }
  strcpy(output,pdata);  
  *loutput = len;

  
  return 0;
}

int Spec_Cmd(char * input, int * linput, int * err, char * output, int * loutput)
{
  int retn, cmd;
  cmd = SV_CMD_WITH_RETURN;

  retn = Spec_SendAndReceive(specfd,cmd, input,*linput,err,output,loutput);

  return retn;
}
