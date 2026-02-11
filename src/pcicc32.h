#ifndef __PCICC32_H__
#define __PCICC32_H__
/*

  $Id$
   pcicc32.h -- the common header for driver and applications for the
   PCICC32 PCI to CAMAC Interface from ARW Elektronik

   (c) 2000 ARW Elektronik

   This source code is published under GPL (Open Source). You can use,
   redistrubute and modify it unless this header is not modified or
   deleted. No warranty is given that this software will work like
   expected.  This product is not authorized for use as critical
   component in life support systems wihout the express written
   approval of ARW Elektronik Germany.
  
   Please announce changes and hints to ARW Elektronik

   First steps                                              AR   25.02.2000

*/

#include <asm/ioctl.h>

#define PCICC32_MAGIC ' '

typedef struct
{
  char bConnected;	/* the interface seems to be connected */
  char bFail;		/* a timeout failure is registered     */
  char bIrq;		/* a LAM wants to generate a IRQ       */
  char bReserve;		/* not yet used                        */
} PCICC32_STATUS;

/* get the status of PCICC32 */
#define PCICC32_IOSTATE _IOR(PCICC32_MAGIC, 1, sizeof(PCICC32_STATUS))  
/* reset a timeout failure, no parameters */
#define PCICC32_IOCNTRL _IO(PCICC32_MAGIC, 2)

#endif /* __PCICC32_H__ */
