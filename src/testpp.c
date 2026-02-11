#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/ioctl.h>
#include <linux/lp.h>


int debug=1;
int main(int argc, char * argv[])
{

  int ppstat,fd;
  char outch, ch;

  ppstat = 0;
  if ((fd = open("/dev/lp0",O_RDWR)) < 0) {
    fprintf(stderr,"Can't open parallel port\n");
    //*inval = 0;
    return;
  }

  if (ioctl(fd,LPGETSTATUS,&ppstat) < 0) {
    fprintf(stderr,"ioctl() failed\n");
    //*inval = 0;
    close(fd);
    return;
  }

  ch = (unsigned char) (0xFF & ppstat);
  if (debug) printf("VPARPORT status = 0x%0x\n",ch);
  outch = (((ch & 0x40) >> 6) |
	   ((ch & 0x20) >> 4) |
	   ((ch & 0x10) >> 2) |
	   (ch & 0x08));

  //*inval = (int) outch;
  printf(" Return val = 0x%02x\n",(int) outch);
  close(fd);
  return 0;
}
