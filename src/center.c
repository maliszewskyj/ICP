#include <stdio.h>
#include <stdlib.h>
#include <ncurses.h>
#include "instrext.h"
#include "trace.h"

#define AXIS_ROTATION 3
#define AXIS_LTRANS   11
#define AXIS_UTRANS   16
#define INCR_DEF      0.2
#define WIDTH  30
#define HEIGHT 10 

int startx = 0;
int starty = 0;

float trans, incr;
float rotation;

// FORTRAN prototypes
float rdangle_(int * motorno);
void drive_(int * motorno, float * hardpos);
void yield_();
void VMEENABLE(int * motorno);
void VMEDISABLE(int * motorno);

float ReadPosition(int motorno)
{
  float hardpos, softpos, zero;
  zero = 0;
  zero = z[motorno-1];
  hardpos = rdangl_(&motorno);
  softpos = hardpos - zero;
  return softpos;
}

void MoveAxis(int motorno, float destination)
{
  float hardpos, zero;
  zero = 0;
  zero = z[motorno-1];
  hardpos = destination + zero;
  if (motorno != 3) VMEENABLE(&motorno);
  mvprintw(16, 0, "MOVING");
  refresh();
  drive_(&motorno,&hardpos);
  yield_();
  if (motorno != 3) VMEDISABLE(&motorno);
  mvprintw(16, 0, "      ");
  refresh();
}

void UpdatePositions(WINDOW *pos_win)
{
  mvwprintw(pos_win, 0, 0, "Rotation       : %8.3f", rotation);
  mvwprintw(pos_win, 1, 0, "Sample Trans   : %8.3f", trans);
  mvwprintw(pos_win, 2, 0, "Trans Increment: %8.3f", incr);
  wrefresh(pos_win);
}

void CENTER_RESET()
{

  initscr();
  clear();
  noecho();
  cbreak();	/* Line buffering disabled. pass on everything */

  mvprintw(1, 0, "Centering sample translations.");  
  refresh();
  mvprintw(2, 0, "Centering upper translation.");
  refresh();
  MoveAxis(AXIS_UTRANS,0);
  mvprintw(3, 0, "Centering lower translation.");
  refresh();
  MoveAxis(AXIS_LTRANS,0);
  endwin();
  printf("Translations centered.\n");
}

void CENTER_CHECK(int * centered)
{
  float utrans,ltrans,uerr,lerr;
  utrans = ReadPosition(AXIS_UTRANS);
  ltrans = ReadPosition(AXIS_LTRANS);

  uerr = ((float) merr[AXIS_UTRANS-1])/((float) pulse[AXIS_UTRANS-1]);
  lerr = ((float) merr[AXIS_LTRANS-1])/((float) pulse[AXIS_LTRANS-1]);
  *centered = ((utrans <= uerr) && (ltrans <= lerr)) ? 1 : 0;
}

void CENTER(int * orientation) 
{
  WINDOW *pos_win;
  int ch;
  int motorno;
  char inbuf[80], *pch;
  double ftmp;

  switch (*orientation) {
  case 0:
    CENTER_RESET();
    return;
  case 1:
    motorno = AXIS_LTRANS;
    break;
  case 2:
    motorno = AXIS_UTRANS;
    break;
  default:
    printf("Improper option to center command. Options = 0 (reset), 1 (lower), 2(upper)\n");
    return;
  }

  trans = 0.0;
  incr = INCR_DEF;
  initscr();
  clear();
  noecho();
  cbreak();	/* Line buffering disabled. pass on everything */
  startx = (80 - WIDTH) / 2;
  starty = (24 - HEIGHT) / 2;
		
  pos_win = newwin(HEIGHT, WIDTH, starty, startx);
  keypad(pos_win,TRUE);
  if (*orientation == 1) {
    mvprintw(0,0, "Sample rotation should be at -90.");
  } else if (*orientation == 2) {
    mvprintw(0,0, "Sample rotation should be at   0.");
  }
  mvprintw(1, 0, "Press LEFT and RIGHT to move sample trans.");
  mvprintw(2, 0, "Press F2 to change increment (max 1.0).");
  mvprintw(3, 0, "Press END to exit.");
  refresh();

  rotation = ReadPosition(AXIS_ROTATION);
  trans = ReadPosition(motorno);
  UpdatePositions(pos_win);
  while(1) {
    ch = wgetch(pos_win);
    if (ch == KEY_END) break;
    switch(ch) {
    case KEY_DOWN:
    case KEY_LEFT:
      trans -= incr;
      MoveAxis(motorno,trans);
      trans = ReadPosition(motorno);
      break;
    case KEY_UP:
    case KEY_RIGHT:
      trans += incr;
      MoveAxis(motorno,trans);
      trans = ReadPosition(motorno);
      break;	
    case KEY_F(2):
      mvwprintw(pos_win, 2, 0, "Trans Increment:");
      wrefresh(pos_win);
      echo();
      wgetstr(pos_win,inbuf);
      noecho();
      ftmp = strtod(inbuf,&pch);
      if (ftmp <= 0)  ftmp = INCR_DEF;
      if (ftmp > 1.0) ftmp = INCR_DEF;
      incr=ftmp;
      break;
    }
    UpdatePositions(pos_win);
    refresh();
  }
		
  endwin();			/* End curses mode		  */
  printf("GOODBYE!\n");
}

/*
int main(int argc, char * argv[])
{
  utrans = 0.0;
  ltrans = 0.0;
  uincr = 0.1;
  lincr = 0.1;
 
  CENTER();
  return 0;
}
*/
