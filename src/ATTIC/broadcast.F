#include "icp.h"
C  This file implements the routine to broadcast the news about the collection
C  of another data point. It will be called whenever the data file is written to.
C  This routine uses NETLIB library written by Mat Madison of TGV, which makes
C  it independent from the actual TCP/IP package (we use CMU, but the code would
C  not have to be changed if we switched to Multinet).
C  The down side is that the libraries have to be installed on the computer
C  ICP is linked, and they have to correspond to the ICP version (v.6.4 required
C  a different version). Installation is simple: make sure that
C  SYS$STARTUP:NETLIB_STARTUP.COM file exists and is called in SYSTARTUP_v5, 
C  and shared libraries NETLIB_CMU_SHR.EXE and NETLIB_SHRXFR.EXE
C  Then, once the system is restarted, the program needs linked with 
C  the proper libraries (NETLIB/OPT), where option file NETLIB.OPT contains :
C    netlib_shr/share
C  would have been nice if this line could be used directly in link line, but
C  netlib_shr is a logical, and it does not work otherwise.
 
      include COMPILE_OPTS
      subroutine broadcast(lstr,fulls)
      implicit none

      character*(*) fulls !this is the string to broadcast
	integer*4 lstr !this is the length of the nonblank string in fulls

c     these two are pointers!!!!! all the world is the VAX!!!
      integer*4 ctx,buf

      integer*4 address, len
      byte adbyte(4)
      data adbyte /129,6,121,255/ 
      equivalence (address,adbyte)  ! I know the bytes, he needs full word

      integer*4 net_assign, net_bind, udp_send, net_deassign, status
      integer*4 NET_K_UDP
      parameter(NET_K_UDP=2)
      
      call str$analyze_sdesc(fulls,len,buf) ! just need the real address
c      call str$trim(fulls,fulls,len)        ! just need the real length
      len = lstr    ! get the real length, as provided in the calling param

      status = net_assign(ctx)              ! write the ctlblock addr to ctx
      if (.not.status) goto 123
      status = net_bind(ctx,%val(NET_K_UDP),,,) ! bind UDP 'socket'
      if (.not.status) goto 123
c     send UDP datagram from BUF[0..len-1] to 129.6.121.255 port 8080
      status = udp_send(ctx,%val(address),%val(8080),%val(buf),%val(len))
      if (.not.status) goto 123
      status = net_deassign(ctx)
      if (.not.status) goto 123
      return
c 123  type *,' Error while broadcasting good news'
 123	return
      end

