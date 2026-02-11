void ENET_INIT();
void GPIBEN_LISTEN(int * addr, int * err);
void GPIBEN_UNL(int * err);
void GPIBEN_TALK(int * addr, int * err);
void GPIBEN_UNT(int * err);
void GPIBEN_EXECUTE(int * err);
int  GPIBEN_WR(char * string, int * length, int * err);
int  GPIBEN_RD(char * string, int * expected, int * length, int * err);
int  GPIBEN_RDBLK(char * string, int * expected, int * length, int * err);
