/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
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
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*/

#ifndef _CONSOLE_H_INCLUDED
#define _CONSOLE_H_INCLUDED

#ifndef RLC_VENDOR
#define RLC_VENDOR TEXT("SWI")
#endif

#define RLC_TITLE_MAX 256		/* max length of window title */

#ifndef _export
#ifdef _MAKE_DLL
#define _export __declspec(dllexport)
#else
#define _export extern
#endif
#endif

#include <signal.h>
#include <stddef.h>
#ifdef _MSC_VER
#if (_MSC_VER < 1300)
typedef long intptr_t;
typedef unsigned long uintptr_t;
#endif
#else
#include <stdint.h>
#endif

#define RLC_APPTIMER_ID	100		/* >=100: application timer */

typedef struct
{ int		 first;
  int		 last;
  int		 size;			/* size of the buffer */
  TCHAR	        *buffer;		/* character buffer */
  int		 flags;			/* flags for the queue */
} rlc_queue, *RlcQueue;

#define RLC_EOF	0x1			/* Flags on the queue */

typedef struct
{ int		mark_x;
  int		mark_y;
} rlc_mark, *RlcMark;

typedef struct
{ const TCHAR   *title;			/* window title */
  const TCHAR   *key;			/* Last part of registry-key */
  int		width;			/* # characters(0: default) */
  int		height;			/* # characters (0: default) */
  int		x;			/* # pixels (0: default) */
  int		y;			/* # pixels (0: default) */
  int		savelines;		/* # lines to save (0: default) */
  TCHAR		face_name[32];		/* font name */
  int		font_family;		/* family id */
  int		font_size;
  int		font_weight;
  int		font_char_set;
} rlc_console_attr;

typedef void * rlc_console;		/* console handle */

typedef void	(*RlcUpdateHook)(void);	/* Graphics update hook */
typedef void	(*RlcTimerHook)(int);	/* Timer fireing hook */
typedef int	(*RlcRenderHook)(WPARAM);	/* Render one format */
typedef void	(*RlcRenderAllHook)(void); /* Render all formats */
typedef int	(*RlcMain)(rlc_console c, int, TCHAR**); /* main() */
typedef void	(*RlcInterruptHook)(rlc_console, int); /* Hook for Control-C */
typedef void	(*RlcResizeHook)(int, int); /* Hook for window change */
typedef void	(*RlcMenuHook)(rlc_console, const TCHAR *id); /* Hook for menu-selection */
typedef void	(*RlcFreeDataHook)(uintptr_t data); /* release data */

#ifdef __WINDOWS__			/* <windows.h> is included */
					/* rlc_color(which, ...) */
#define RLC_WINDOW	  (0)		/* window background */
#define RLC_TEXT	  (1)		/* text color */
#define RLC_HIGHLIGHT	  (2)		/* selected text background */
#define RLC_HIGHLIGHTTEXT (3)		/* selected text */

_export HANDLE	rlc_hinstance(void);	/* hInstance of WinMain() */
_export HWND	rlc_hwnd(rlc_console c); /* HWND of console window */
_export int	rlc_window_pos(rlc_console c,
			       HWND hWndInsertAfter,
			       int x, int y, int w, int h,
			       UINT flags); /* resize/reposition window */
_export int	rlc_main(HANDLE hI, HANDLE hPrevI,
			 LPTSTR cmd, int show, RlcMain main, HICON icon);
_export void	rlc_icon(rlc_console c, HICON icon);	/* Change icon */
_export COLORREF rlc_color(rlc_console c, int which, COLORREF color);

typedef LRESULT	(*RlcMessageHook)(HWND hwnd, UINT message,
				  WPARAM wParam, LPARAM lParam);
_export RlcMessageHook  rlc_message_hook(RlcMessageHook hook);

#endif /*__WINDOWS__*/

_export RlcUpdateHook	rlc_update_hook(RlcUpdateHook updatehook);
_export RlcTimerHook	rlc_timer_hook(RlcTimerHook timerhook);
_export RlcRenderHook   rlc_render_hook(RlcRenderHook renderhook);
_export RlcRenderAllHook rlc_render_all_hook(RlcRenderAllHook renderallhook);
_export RlcInterruptHook rlc_interrupt_hook(RlcInterruptHook interrupthook);
_export RlcResizeHook	rlc_resize_hook(RlcResizeHook resizehook);
_export RlcMenuHook	rlc_menu_hook(RlcMenuHook menuhook);
_export int		rlc_copy_output_to_debug_output(int docopy);

_export rlc_console	rlc_create_console(rlc_console_attr *attr);
_export void		rlc_title(rlc_console c,
				  TCHAR *title, TCHAR *old, int size);
_export void		rlc_yield(void);
_export void		rlc_word_char(int chr, int isword);
_export int		rlc_is_word_char(int chr);
_export int		rlc_iswin32s(void);	/* check for Win32S */

_export void		rlc_free(void *ptr);
_export void *		rlc_malloc(size_t size);
_export void *		rlc_realloc(void *ptr, size_t size);

_export size_t		rlc_read(rlc_console c, TCHAR *buf, size_t cnt);
_export size_t		rlc_write(rlc_console c, TCHAR *buf, size_t cnt);
_export int		rlc_close(rlc_console c);
_export int		rlc_flush_output(rlc_console c);

_export int		getch(rlc_console c);
_export int		getche(rlc_console c);
_export int		getkey(rlc_console c);
_export int		kbhit(rlc_console c);
_export void		ScreenGetCursor(rlc_console c, int *row, int *col);
_export void		ScreenSetCursor(rlc_console c, int row, int col);
_export int		ScreenCols(rlc_console c);
_export int		ScreenRows(rlc_console c);

_export int		rlc_insert_menu_item(rlc_console c,
					     const TCHAR *menu,
					     const TCHAR *label,
					     const TCHAR *before);
_export int		rlc_insert_menu(rlc_console c,
					const TCHAR *label,
					const TCHAR *before);

		 /*******************************
		 *	  GET/SET VALUES	*
		 *******************************/

#define RLC_APPLICATION_THREAD		0 /* thread-handle of application */
#define RLC_APPLICATION_THREAD_ID	1 /* thread id of application */
#define RLC_VALUE(N)			(1000+(N))

_export int		rlc_get(rlc_console c, int what,
				uintptr_t *val);
_export int		rlc_set(rlc_console c, int what,
				uintptr_t val,
				RlcFreeDataHook hook);


		 /*******************************
		 *	 LINE EDIT STUFF	*
		 *******************************/

typedef struct _line
{ rlc_mark	origin;			/* origin of edit */
  size_t	point;			/* location of the caret */
  size_t	size;			/* # characters in buffer */
  size_t	allocated;		/* # characters allocated */
  size_t	change_start;		/* start of change */
  int		complete;		/* line is completed */
  int		reprompt;		/* repeat the prompt */
  TCHAR	       *data;			/* the data (malloc'ed) */
  rlc_console	console;		/* console I belong to */
} line, *Line;

#define COMPLETE_MAX_WORD_LEN 256
#define COMPLETE_MAX_MATCHES 100

#define COMPLETE_INIT	   0
#define COMPLETE_ENUMERATE 1
#define COMPLETE_CLOSE	   2

struct _complete_data;

typedef int (*RlcCompleteFunc)(struct _complete_data *);

typedef struct _complete_data
{ Line		line;			/* line we are completing */
  int		call_type;		/* COMPLETE_* */
  int		replace_from;		/* index to start replacement */
  int		quote;			/* closing quote */
  int		case_insensitive;	/* if TRUE: insensitive match */
  TCHAR		candidate[COMPLETE_MAX_WORD_LEN];
  TCHAR		buf_handle[COMPLETE_MAX_WORD_LEN];
  RlcCompleteFunc function;		/* function for continuation */
  void	       *ptr_handle;		/* pointer handle for client */
  intptr_t	num_handle;		/* numeric handle for client */
} rlc_complete_data, *RlcCompleteData;

_export RlcCompleteFunc rlc_complete_hook(RlcCompleteFunc func);

_export TCHAR	*read_line(rlc_console console);
_export int	rlc_complete_file_function(RlcCompleteData data);
_export void	rlc_init_history(rlc_console c, int size);
_export void	rlc_add_history(rlc_console c, const TCHAR *line);
_export int	rlc_bind(int chr, const char *fname);
_export int	rlc_for_history(
		    rlc_console b,
		    int (*handler)(void *ctx, int no, const TCHAR *line),
		    void *ctx);

#endif /* _CONSOLE_H_INCLUDED */
