/*
	INTEGER*4    nocells,histodata(256),nhisto
	REAL*4	 liqwait !seconds to wait between moving motors & counting
	parameter (nocells=256)
	common /hdata/ histodata,nhisto,liqwait
*/
struct {
  int histodata[256];
  int nhisto;
  float liqwait;
} _CHDATA;
	
