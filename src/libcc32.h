#ifndef __CC32LIB_H__
#define __CC32LIB_H__
/*
   cc32lib.h -- header for a simple access library for the PCICC32 PCI
   to CAMAC Interface from ARW Elektronik

   (c) 2000 ARW Elektronik

   this source code is published under GPL (Open Source). You can use,
   redistrubute and modify it unless this header is not modified or
   deleted. No warranty is given that this software will work like
   expected.  This product is not authorized for use as critical
   component in life support systems wihout the express written
   approval of ARW Elektronik Germany.
  
   Please announce changes and hints to ARW Elektronik

   To use this library the module "pcicc32.o" have to be
   installed. There must be also a node which points with ist major
   number to the module and its minor number must equal the CC32
   module number (Jumper J301 to J304).
   
   first steps                                             AR   25.02.2000
   Release 1.0.0                                           AR   26.02.2000 
*/
 

/*--- INCLUDES --------------------------------------------------------------*/
#include <asm/types.h>

/*--- TYPEDEFS --------------------------------------------------------------*/
typedef void* CC32_HANDLE;  /* type of the device handle */

/*--- DEFINES ---------------------------------------------------------------*/

/*--- PROTOTYPES ------------------------------------------------------------*/

/* open a path to a device. E.g. "/dev/pcicc32_1" */
int   cc32_open(char *cszPath, CC32_HANDLE *handle);

/* close the opened path */
int   cc32_close(CC32_HANDLE handle); 

/* read only a word - 16 bits - from a address made out of N,A,F */
__u16 cc32_read_word(CC32_HANDLE handle, unsigned int N, 
		     unsigned int A, unsigned int F);

/* Read a long - 32 bits - 
   from a address made out of N,A,F and get the result Q and X */
__u32 cc32_read_long(CC32_HANDLE handle, unsigned int N, 
		     unsigned int A, unsigned int F, int *Q, int *X);

/* Read a long - 32 bits - without any interpretaion */
__u32 cc32_read_long_all(CC32_HANDLE handle, unsigned int N, 
			 unsigned int A, unsigned int F);

/* Write a word - 16 bits - to a destination made out of N,A,F */
void  cc32_write_word(CC32_HANDLE handle, unsigned int N, 
		      unsigned int A, unsigned int F, __u16 uwData);

/* Write a long - 32 bits - uninterpreted to a destination made out of N,A,F */
void  cc32_write_long(CC32_HANDLE handle, unsigned int N, 
		      unsigned int A, unsigned int F, __u32 ulData);

/* Poll the state of the timeout line and the LAM state. 
   The timeout line is cleared if it was set */
int   cc32_poll_error(CC32_HANDLE handle, int *nTimeout, int *nLam);

#endif /* __CC32LIB_H__ */
