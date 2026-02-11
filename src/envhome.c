#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#define DEFDIR "/usr/local/icp"
/*
 * Determine the home directory of the current process
 */

void ENVHOME(char * home, int * lhome) {
  char * ptr;
  if ((ptr=getenv("ICPDIR")) != NULL) {
    strcpy(home,ptr);
    //  } else if ((ptr = getenv("HOME")) != NULL) {
    //    strcpy(home,ptr);
  } else {
    /* If the system call fails, assume the home directory is one level up */ 
    sprintf(home,DEFDIR);
  }
  *lhome = strlen(home);
}

void PWD(char * dir, int * ldir) {
  if (getcwd(dir,79) == NULL) return;
  strcat(dir,"/"); /* Append trailing slash */
  *ldir = strlen(dir);
}

/*
int
main(int argc, char * argv[]) {
  char home[80];
  int lhome;

  memset(home,0,sizeof(home));
  ENVHOME(home,&lhome);
  printf("Home = ->%s<-\n",home);
  
}
*/
