/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2002, University of Amsterdam

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2.1 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Include file to share stuff inside this library.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

		 /*******************************
		 *	       HISTORY		*
		 *******************************/

typedef struct _history
{ int		size;			/* size of the history */
  int		tail;			/* oldest position */
  int		head;			/* newest position */
  int		current;		/* for retrieval */
  char **	lines;			/* the lines */
} history, *History;


		 /*******************************
		 *	    CONSOLE DATA	*
		 *******************************/

#define ANSI_MAX_ARGC     10		/* Ansi-escape sequence argv */
#define MAXPROMPT         80		/* max size of prompt */
#define OQSIZE	     	4096		/* output queue size */
#define MAX_USER_VALUES	  10		/* max user data-handles */

typedef struct lqueued
{ char *	  line;			/* Lines in queue */
  struct lqueued* next;			/* Next in queue */
} lqueued, *LQueued;

typedef struct
{ char *	 text;			/* the storage */
  unsigned short size;			/* #characters in line */
  unsigned	 adjusted : 1;		/* line has been adjusted? */
  unsigned	 changed : 1;		/* line needs redraw */
  unsigned	 softreturn : 1;	/* wrapped line */
} text_line, *TextLine;

typedef struct
{ unsigned long	data;			/* the data itself */
  RlcFreeDataHook hook;			/* call when destroying console */
} user_data;

#define RLC_MAGIC	0x3b75df1e	/* magic number to verify */

typedef struct
{ int		magic;
  int		height;			/* number of lines in buffer */
  int		width;			/* #characters ler line */
  int		first;			/* first line of ring */
  int		last;			/* last line of ring */
  int		caret_x;		/* cursor's x-position */
  int		caret_y;		/* its line */
  int		window_start;		/* start line of the window */
  int		window_size;		/* #lines on the window */
  TextLine 	lines;			/* the actual lines */
  int		sel_unit;		/* SEL_CHAR, SEL_WORD, SEL_LINE */
  int		sel_org_line;		/* line origin of the selection */
  int		sel_org_char;		/* char origin of the selection */
  int		sel_start_line;		/* starting line for selection */
  int		sel_start_char;		/* starting char for selection */
  int		sel_end_line;		/* ending line for selection */
  int		sel_end_char;		/* ending char for selection */
  int		cmdstat;		/* for parsing ANSI escape */
  int		argstat;		/* argument status ANSI */
  int		argc;			/* argument count for ANSI */
  int		argv[ANSI_MAX_ARGC];	/* argument vector for ANSI */
  int		scaret_x;		/* saved-caret X */
  int		scaret_y;		/* saved-caret Y */
  HWND		window;			/* MS-Window window handle */
  int		has_focus;		/* Application has the focus */
  HFONT		hfont;			/* Windows font handle */
  int		fixedfont;		/* Font is fixed */
  COLORREF	foreground;		/* Foreground (text) color */
  COLORREF	background;		/* Background color */
  COLORREF	sel_foreground;		/* Selection foreground */
  COLORREF	sel_background;		/* Selection background */
  int		cw;			/* character width */
  int		ch;			/* character height */
  int		cb;			/* baseline */
  int		changed;		/* changes to the whole screen */
  int		sb_lines;		/* #lines the scrollbar thinks */
  int		sb_start;		/* start-line scrollbar thinks */
  int		caret_is_shown;		/* is caret in the window? */
  char		current_title[RLC_TITLE_MAX]; /* window title */
					/* status */
  rlc_console_attr * create_attributes;	/* Creation attributes */
  char	       *regkey_name;		/* last part of key */
  int		win_x;			/* window top-left corner */
  int		win_y;			/* window top-left corner */
					/* output queue */
  char	        output_queue[OQSIZE];	/* The output queue */
  int		output_queued;		/* # characters in the queue */
  struct
  { char *line;				/* buffered line */
    unsigned int length;		/* length of line */
    unsigned int given;			/* how much we passed */
  } read_buffer;
					/* input queuing */
  int		imode;			/* input mode */
  int		imodeswitch;		/* switching imode */
  RlcQueue	queue;			/* input stream */
  LQueued	lhead;			/* line-queue head */
  LQueued	ltail;			/* line-queue tail */
  char		promptbuf[MAXPROMPT];	/* Buffer for building prompt */
  char		prompt[MAXPROMPT];	/* The prompt */
  int		promptlen;		/* length of the prompt */
  int		closing;		/* closing status */
  int		modified_options;	/* OPT_ */
  history	history;		/* history for this console */
					/* Thread handles */
  HANDLE	console_thread;		/* I/O thread  */
  HANDLE	application_thread;	/* The application I work for */
  DWORD		console_thread_id;	/* I/O thread id */
  DWORD		application_thread_id;
  HWND		kill_window;		/* window in app thread for destroy */
  
  user_data	values[MAX_USER_VALUES]; /* associated user data */
} rlc_data, *RlcData;


		 /*******************************
		 *	       DATA		*
		 *******************************/

extern RlcData  _rlc_stdio;		/* global default console */


		 /*******************************
		 *	    FUNCTIONS		*
		 *******************************/

extern void	rlc_assert(const char *msg);
int		rlc_at_head_history(RlcData b);
const char *	rlc_bwd_history(RlcData b);
const char *	rlc_fwd_history(RlcData b);
void		rlc_get_mark(rlc_console c, RlcMark mark);
void		rlc_goto_mark(rlc_console c, RlcMark mark,
			      const char *data, int offset);
void		rlc_erase_from_caret(rlc_console c);
void		rlc_putchar(rlc_console c, int chr);
char *		rlc_read_screen(rlc_console c,
				RlcMark from, RlcMark to);
void		rlc_update(rlc_console c);
const char *	rlc_prompt(rlc_console c, const char *prompt);
void		rlc_clearprompt(rlc_console c);


		 /*******************************
		 *	 INLINE FUNCTIONS	*
		 *******************************/

#define assert(g) if ( !(g) ) rlc_assert(#g)

__inline RlcData
rlc_get_data(rlc_console c)
{ if ( c )
  { RlcData b = c;

    assert(b->magic == RLC_MAGIC);
    return b;
  }

  return _rlc_stdio;
}


