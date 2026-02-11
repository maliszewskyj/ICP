#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <errno.h>
#include <string.h>
#include <sys/stat.h>
/*
 * ICP instrument privilege determination
 *
 *
 *
 *
 */
#define LOCALDISPLAY ":0"
#define VNCDISPLAY   ":1"

static char * lockfile = "/tmp/icp.lock";
static int verbose=1;

void
RemoveLock() {
  unlink(lockfile);
}

int CHKPRIV(void) {
  char * ptr, lptr[80];
  int retn, pass;
  int pid;
  pass = 0;

  if ((ptr = getenv("MAINTENANCE")) != NULL) {
    if (verbose) printf("Access granted for maintenance\n");
    pass = 1;
  }
  if (!pass && ((ptr = getenv("DISPLAY")) == NULL)) {
    if (verbose) printf("No DISPLAY environment: OK\n");
    pass = 1;
  } 

  /* 
   * Check to see if we're running from the local X-windows console
   */
  if (!pass && !strncmp(ptr,LOCALDISPLAY,strlen(LOCALDISPLAY))) {
    if (verbose) printf("Local X windows console: OK\n");
    pass = 1;
  } else if (!pass && !strncmp(ptr,VNCDISPLAY,strlen(VNCDISPLAY))) {
    /* Allow VNC-connected sessions to control as well */
    if (verbose) printf("VNC X windows console: OK\n");
    pass = 1;
  } else {
    if (verbose) printf("DISPLAY = %s\n",ptr);
  }
  if (!pass) return 0;

  /* Now should check to make sure that ICP isn't already running */
  /* Check for the existence of a lock file */
  if ((retn = readlink(lockfile,lptr,sizeof(lptr)-1)) < 0) {
    if (errno == ENOENT) {
      if (verbose) printf("No lockfile detected\n");
      pass = 1;
    }
  } else {
    /* Extract process ID owning the lockfile */
    lptr[retn] = 0; /* Correct for peculiar properties of readlink() */
    if ((ptr = strchr(lptr,':')) == NULL) {
      if (verbose) printf("Bad lockfile - should remove\n");
      unlink(lockfile);
      return 1;
    }
    ptr++;

    if ((retn = sscanf(ptr,"%d",&pid)) != 1) {
      if (verbose) printf("Could not obtain PID of lockfile\n");
      pid = 0;
    }
    if (pid > 0) {
      if ((retn = getpgid(pid)) < 0) {
	if (errno == ESRCH) {
	  if (verbose) printf("PID does not exist - we're in business!\n");
	  pass = 1;
	  unlink(lockfile);
	}
      } else {
	if (verbose) printf("ICP appears to be running\n");
	pass = 0;
      }
    }
  }
  
  if (pass) {
    /* Write lock file */
    sprintf(lptr,"%s:%d",getenv("HOSTNAME"),getpid());
    if ((retn = symlink(lptr,lockfile)) < 0) {
      if (verbose) printf("Unable to create lockfile!\n");
      pass = 0;
      return 0;
    }
    atexit(RemoveLock);
    return 1;
  } 
  return 0;
}

/*
int
main(int argc, char * argv[]) {
  int pass;

  pass = 0;
  if ((pass=CHKPRIV()) < 0) {
    fprintf(stderr,"Errors encountered in obtaining privileges\n");
  } 
  printf("Privileges %s.\n",(pass) ? "granted" : "denied");
  return 0;
}
*/
