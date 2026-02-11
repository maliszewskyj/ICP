/*
   cc32lib.c -- a simple access library for the PCICC32 PCI to 
                CAMAC Interface from ARW Elektronik

   (c) 2000 ARW Elektronik

   This source code is published under GPL (Open Source). You can use,
   redistribute and modify it unless this header is not modified or
   deleted. No warranty is given that this software will work like
   expected.  This product is not authorized for use as critical
   component in life support systems wihout the express written
   approval of ARW Elektronik Germany.
  
   Please announce changes and hints to ARW Elektronik

   first steps                                                 AR   25.02.2000

*/
 
/*--- INCLUDES --------------------------------------------------------------*/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/mman.h>
#include <errno.h>
#include <ctype.h>
#include <sys/ioctl.h>
#include "pcicc32.h" /* PCI common ioctl commands and structures between driver and library  */
#include "libcc32.h"      /* shared header between application and library  */

/*--- DEFINES ---------------------------------------------------------------*/
#define pwCC32_ADR(adr, N, A, F) (__u16 *)((N << 10) + (A << 6) + ((F & 0xF) << 2) + adr)
#define plCC32_ADR(adr, N, A, F) (__u32 *)((N << 10) + (A << 6) + ((F & 0xF) << 2) + adr)
#define WINDOW_SIZE 32768

/*--- EXTERNALS -------------------------------------------------------------*/

/*--- TYPEDEFS --------------------------------------------------------------*/
typedef struct
{
	FILE *f;		/* the handle of this device */
  	int  fileNo;		/* equals fileno(f)          */
	char *base;		/* base of range, got with mmap */
} CC32_DEVICE;

/*--- FUNCTIONS -------------------------------------------------------------*/
int cc32_open(char *cszPath, CC32_HANDLE *handle)
{
  CC32_DEVICE *dev;
	
  *handle = (CC32_HANDLE)0;
	
  dev = (CC32_DEVICE *)malloc(sizeof(CC32_DEVICE));
  //  if (!dev) return errno;
  if (!dev) return -ENOMEM;
	
  dev->fileNo = 0;
  dev->base   = (char *)0;
	
  if (!(dev->f = fopen(cszPath,"r"))) 
    {
      //      int error = errno;
      int error = -ENXIO;
		
      free(dev);
      return error;
    }
	
  dev->fileNo = fileno(dev->f);
	
  dev->base = (char *)mmap(0, WINDOW_SIZE, PROT_READ, MAP_FILE | MAP_PRIVATE, dev->fileNo, 0);
  if (dev->base == (char *)-1)
    {
      //      int error = errno;
      int error = -EFAULT;
		
      fclose(dev->f);
      free(dev);
      return error;	
    } 

  *handle = (CC32_HANDLE)dev;
	
  return 0;
}

int cc32_close(CC32_HANDLE handle)
{
  CC32_DEVICE *dev = (CC32_DEVICE *)handle;
  int error = 0;
	
  if (dev)
    {
      munmap(dev->base, WINDOW_SIZE);
			
      if (dev->f)
	fclose(dev->f);
      else
	error = -EINVAL;
	
      free(dev);
    }
  else
    error = -EINVAL;
		
  return error;
}

__u16 cc32_read_word(CC32_HANDLE handle, unsigned int N, unsigned int A, unsigned int F)
{
  CC32_DEVICE *dev = (CC32_DEVICE *)handle;
	
  return *pwCC32_ADR(dev->base, N, A, F);
}

__u32 cc32_read_long_all(CC32_HANDLE handle, unsigned int N, unsigned int A, unsigned int F)
{
  CC32_DEVICE *dev = (CC32_DEVICE *)handle;
	
  return *plCC32_ADR(dev->base, N, A, F);;
}

__u32 cc32_read_long(CC32_HANDLE handle, unsigned int N, unsigned int A, unsigned int F, int *Q, int *X)
{
  CC32_DEVICE *dev = (CC32_DEVICE *)handle;
  __u32 erg = *plCC32_ADR(dev->base, N, A, F);
	
  *Q = (erg & 0x80000000) ? 1 : 0;
  *X = (erg & 0x40000000) ? 1 : 0;
	
  return erg & 0x00FFFFFF;
}

void cc32_write_word(CC32_HANDLE handle, unsigned int N, unsigned int A, unsigned int F, __u16 uwData)
{
  CC32_DEVICE *dev = (CC32_DEVICE *)handle;
	
  *pwCC32_ADR(dev->base, N, A, F) = uwData;
}

void cc32_write_long(CC32_HANDLE handle, unsigned int N, unsigned int A, unsigned int F, __u32 ulData)
{
  CC32_DEVICE *dev = (CC32_DEVICE *)handle;
	
  *plCC32_ADR(dev->base, N, A, F) = ulData;
}

int cc32_poll_error(CC32_HANDLE handle, int *nTimeout, int *nLam)
{
  CC32_DEVICE *dev = (CC32_DEVICE *)handle;
  PCICC32_STATUS state;
  int error;
	
  //  if ((error = ioctl(dev->fileNo, PCICC32_IOSTATE, &state)))
  //    return error;
		
  *nTimeout = state.bFail;
  *nLam     = state.bIrq;
	
  if (state.bFail)  /* clear error */
    {
      if ((error = ioctl(dev->fileNo, PCICC32_IOCNTRL, 0)))
	return error;	
    }
	
  return 0;
}

