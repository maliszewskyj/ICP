/*
 * Ethernet layer for connecting to AIM-style detectors over Ethernet
 *
 * Version = $Id: tcp_aim.c,v 1.4 2017/01/18 21:57:02 nickm Exp $
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/time.h>
#include <arpa/inet.h>
#include "trace.h"
#include "tcp_io.h"
#include "lut.h"

#define SERVERPORT 20000
#define SERVERHOST "detector" /* This should be defined in /etc/hosts */
#define CMDWAIT    500000

/* Really shouldn't need to initialize -- Perhaps we can ping? */

int AIM_INIT() 
{
  int sockfd;

  /* Check SERVERHOST and make sure we can find the host */
  if ((sockfd = net_open(SERVERHOST,NULL,SERVERPORT)) <= 0) {
    fprintf(stderr,"Can't connect to detector server \"%s\"\n",SERVERHOST);
    exit(1);
  }
  net_close(sockfd);
  usleep(CMDWAIT);
  return 0;
}

int AIM_ARM()
{
  int sockfd, len, opcode, prm, val, retn;
  net_debug(debug);
  if (debug) puts("AIM_ARM()\n");
  if ((sockfd = net_open(SERVERHOST,NULL,SERVERPORT)) <= 0) {
    fprintf(stderr,"Can't connect to detector server \"%s\"\n",SERVERHOST);
    return -1;
  }
  
  sendPKG(sockfd,OP_CMD,CMD_START,0,0);
  retn = recvPKG(sockfd,&len,&opcode, &prm, (char *)&val);

  net_close(sockfd);
  usleep(CMDWAIT);
  return 0;
}

int AIM_DISARM()
{
  int sockfd, len, opcode, prm, val, retn;
  net_debug(debug);
  if (debug) puts("AIM_DISARM()\n");
  if ((sockfd = net_open(SERVERHOST,NULL,SERVERPORT)) <= 0) {
    fprintf(stderr,"Can't connect to detector server \"%s\"\n",SERVERHOST);
    return -1;
  }
  
  sendPKG(sockfd,OP_CMD,CMD_STOP,0,0);
  retn = recvPKG(sockfd,&len,&opcode, &prm, (char *)&val);

  net_close(sockfd);
  usleep(CMDWAIT);
  return 0;
}

int AIM_CLEAR()
{
  int sockfd, len, opcode, prm, val, retn;
  net_debug(debug);
  if (debug) puts("AIM_CLEAR()\n");
  if ((sockfd = net_open(SERVERHOST,NULL,SERVERPORT)) <= 0) {
    fprintf(stderr,"Can't connect to detector server \"%s\"\n",SERVERHOST);
    return -1;
  }
  
  sendPKG(sockfd,OP_CMD,CMD_CLEAR,0,0);
  retn = recvPKG(sockfd,&len,&opcode, &prm, (char *)&val);
  
  net_close(sockfd);
  usleep(CMDWAIT);
  return 0;
}

/* Determine rank and dimensions of histogram */
int AIM_DIMS(int * xdim, int * ydim)
{
  int sockfd, retn;
  int prm;
  unsigned int dims[1024];
  memset(dims,0,sizeof(dims));
  net_debug(debug);
  if (debug) puts("AIM_DIMS()\n");
  if ((sockfd = net_open(SERVERHOST,NULL,SERVERPORT)) <= 0) {
    fprintf(stderr,"Can't connect to detector server \"%s\"\n",SERVERHOST);
    return -1;
  }
  prm = PRM_DIMS;
  if (debug) printf("AIM_DIMS: op=%d cmd=%d prm=%d\n",OP_CMD,CMD_GET,prm);
  sendPKG(sockfd,OP_CMD,CMD_GET,&prm,sizeof(prm));
  if (debug) printf("AIM_DIMS: get reply");
  retn = recvHIST(sockfd,dims,sizeof(dims));
  *xdim = dims[0];
  *ydim = dims[1];
  if (debug) printf("AIM_DIMS: xdim = %d, ydim = %d\n", *xdim, *ydim);
  
  net_close(sockfd);
  printf("AIMS_DIMS: exiting for debugging\n");

  return 0;
}

int AIM_XFER(int * hist, int * histsize)
{
  int blocksize=1024,hsize;
  int tmpval,tmp;
  int sockfd, retn,i;
  struct timeval tv1, tv2,tvd;
  
  net_debug(debug);
  hsize=*histsize;
  if (debug) printf("AIM_XFER(temphist,%d)\n",hsize);
  memset(hist,0,hsize*sizeof(int));
  if ((sockfd = net_open(SERVERHOST,NULL,SERVERPORT)) <= 0) {
    fprintf(stderr,"Can't connect to detector server \"%s\"\n",SERVERHOST);
    return -1;
  }

  tmp = blocksize*sizeof(int);
  //tmp=hsize;
  tmpval = htonl(tmp);
  sendPKG(sockfd,OP_CMD,CMD_XFER, &tmpval,sizeof(tmpval));
  gettimeofday(&tv1,NULL);
  if ((retn = recvHIST(sockfd,hist,hsize)) <= 0) {
    fprintf(stderr,"AIM_XFER(): Transfer unsuccessful [retn=%d]\n",retn);
  }
  /* 
  for (i=0;i<256;i++) {
    printf("hist[%03d] = %d (0x%08lx)\n",i,hist[i],hist[i]);
  }
  */
  gettimeofday(&tv2,NULL);
  timersub(&tv2,&tv1,&tvd);
  if (debug) printf("Transfered %d longwords in %ld usec\n",retn,
		    (tvd.tv_sec *1000000 + tvd.tv_usec));
  
  net_close(sockfd);
  usleep(CMDWAIT);
  return 0;
}

int int2uchar(long *input, unsigned char *output, int len) {
  float loval, hival, valrange;
  int i;

                                                                                
  loval = 1e20; hival = -1e20;
  for (i=0;i<len;i++) {
    if (input[i] < loval) loval = input[i];
    if (input[i] > hival) hival = input[i];
  }
  valrange = hival - loval;
                                                               
  for (i=0;i<len;i++) {
    if (valrange == 0.0) {
      output[i] = 0;
    } else {
      output[i] = (unsigned char) 255 * ((input[i]-loval)/valrange);
    }
  }

  return 0;
}

int writeppm(char * outfile, unsigned char *input, 
	     int height, int width, int grey) {
  FILE *fp;
  unsigned char r, g, b;
  int z, i,j,k;

  if ((fp = fopen(outfile,"w")) == NULL) return -1;

  /* Write header */
  fprintf(fp,"P6\n");
  fprintf(fp,"%d %d\n",width,height);
  fprintf(fp,"%d\n",255);

  /* Write pixels */
  //  for (i=0;i<height*width;i++) {
  for (i=0;i<height;i++) {
    for (j=0;j<width;j++) {
      k = i + j * height;
      //k = j + i * width;
      z = (int) input[k];
      if (grey) {
	r = input[k]; g = r; b = r;
      } else {
	r = red[k]; g = green[k]; b = blue[k];
      }
      fwrite(&r,1,1,fp);
      fwrite(&g,1,1,fp);
      fwrite(&b,1,1,fp);
    }
  }
      //  }

  fclose(fp);
  return 0;
}

void AIM_IMAGE(long * histo, int * nx, int * ny) 
{
  int stlen, retn;
  unsigned char * imgptr;
  char ppmfile[80];

  stlen = *nx * *ny;
  imgptr = (unsigned char *) calloc(stlen, sizeof(char));
  strcpy(ppmfile,"asd.ppm");

  if (debug) printf("AIM_IMAGE(): Writing %s nx = %d ny = %d\n",ppmfile,*nx,*ny);
  retn = int2uchar(histo,imgptr,stlen);
  if ((retn = writeppm(ppmfile,imgptr,*ny,*nx,0)) < 0) {
    fprintf(stderr,"AIM_IMAGE(): writeppm() unsuccessful!\n");
  }
  free(imgptr);
  return;
}

void AIM_SAVE(long * histo, int * nx, int * ny)
{
  int stlen, i,j,k;
  char rawfile[80];
  FILE *fp;

  stlen = *nx * *ny;

  strcpy(rawfile, "asd.raw");
  if (debug) printf("AIM_SAVE(): Writing %s nx = %d ny = %d\n",rawfile,*nx,*ny);
  if ((fp = fopen(rawfile,"w")) == NULL) return;
  //  for (i=0;i<stlen;i++) {
  fprintf(fp," %d %d\n",*nx,*ny);
  for (i=0;i<*nx;i++) {
    for (j=0;j<*nx;j++) {
      // k = j + i*(*ny);
      k = i + j*(*nx);
      fprintf(fp," %ld",histo[k]);
      if (((i + 1)%*nx) == 0) fprintf(fp,"\n");
    }
  }
  fclose(fp);
  return;
}
