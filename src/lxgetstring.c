#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <readline/readline.h>
#include <readline/history.h>

#define ICPMAXHIST 500

static char * histfile = ".icp_history";

void LXGETSTRING_INIT() 
{
  int retn;

  retn = read_history(histfile);
  using_history();

}

int LXGETSTRING(char * prompt, char * response, int * bufsize, int * outlen) 
{
  char * instring;
  HIST_ENTRY * hist;
  int i;

  instring = readline(prompt);
  if (instring == NULL) {
    *response = 0;
    *outlen = 0;
    return 0;
  }
  *outlen = strlen(instring);
  if (*outlen > *bufsize) { 
    *outlen = *bufsize;
    instring[*outlen] = 0;
  }
  for(i=0;i<*outlen;i++) {
    instring[i] = toupper(instring[i]);
  }
  hist = previous_history();
  if ((*outlen > 0) && ((hist == NULL) || 
			(strcmp(instring,hist->line)))) {
    /* Only add unique entries to history */
    add_history(instring); 
    stifle_history(ICPMAXHIST); /* Keep history to a manageable size */
    write_history(histfile);
  }
  strncpy(response, instring, (size_t) *bufsize); 
  free((char *) instring);
  return *outlen;
}

void LXREADPASS(int * contr, int * keyb, char * passw, int * lpass) 
{
  char *pbuf, ch;
  int lpb,i;

  pbuf = getpass("");
  lpb = strlen(pbuf);
  /* Upcase the password */
  for (i=0;i<lpb;i++) {
    ch = pbuf[i];
    pbuf[i] = (char) toupper(ch);
  }

  /*
  printf("  ENTERED PASSWORD = ->%s<-\n",pbuf);
  */
  if (lpb > *lpass) {
    strncpy(passw,pbuf,*lpass);
    return;
  } 

  /* Pad the remainder of the string with spaces */
  strcpy(passw,pbuf);
  memset(passw + lpb,' ',(*lpass - lpb));
  return;
}
