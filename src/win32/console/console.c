/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  1999-2025, University of Amsterdam
                              VU University Amsterdam
                              SWI-Prolog Solutions b.v.
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This file defines a console for porting (unix) stream-based applications
to MS-Windows. It has been developed for  SWI-Prolog. The main source is
part of SWI-Prolog.

The SWI-Prolog source is at http://www.swi-prolog.org
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Thread design:

<written as a mail to Lutz Wohlrab>

There are two threads. The Prolog engine   runs  in the main thread. The
other thread deals with the window.   Basically, it processes events and
if anything is typed it puts it into a queue.

The main thread  at  some  stage   forks  the  display  thread,  running
window_loop().  This  thread  initialises  the   input  and  then  sends
WM_RLC_READY to the main thread to indicate it is ready to accept data.

If data is to be written,  Prolog   calls  rlc_write(),  which posts the
WM_RLC_WRITE to the display thread, waiting  on the termination. If data
is to be read, rlc_read() posts  a   WM_RLC_FLUSH,  and then waits while
dispatching events, for the display-thread to   fill the buffer and send
WM_RLC_INPUT (which is just sent  to   make  GetMessage()  in rlc_read()
return).

Towards an MT version on Windows
--------------------------------

If we want to move towards a  multi-threaded version for MS-Windows, the
console code needs to be changed significantly, as we need to be able to
create multiple consoles to support thread_attach_console/0.

The most logical solution seems to   be to reverse the thread-structure,
Prolog starting and running in the   main-thread  and creating a console
creates a new thread for this console. There  are two ways to keep track
of the console to use. Cleanest might be to add an argument denoting the
allocated console and alternatively we could   use thread-local data. We
can also combine the two: add an  additional argument, but allow passing
NULL to use the default console for this thread.

Menus
-----

The current console provides a menu that can be extended from Prolog.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#ifdef O_DEBUG_HEAP
static void initHeapDebug(void);
#include <crtdbg.h>
#else
#define initHeapDebug()
#endif

#include <windows.h>
#include <tchar.h>
#ifndef WM_MOUSEWHEEL			/* sometimes not defined */
#define WM_MOUSEWHEEL 0x020A
#endif
#ifndef WM_UNICHAR
#define WM_UNICHAR 0x109
#define UNICODE_NOCHAR 0xFFFF
#endif

#ifdef _MSC_VER
#pragma warning(disable : 4996)	/* deprecate open() etc */
#endif

#include <stdlib.h>
#include <io.h>
#include <string.h>
#include <malloc.h>
#define _MAKE_DLL 1
#undef _export
#include "console.h"
#include "menu.h"
#include "common.h"
#include <signal.h>
#include <ctype.h>
#include <stdio.h>

#ifndef isletter
#define isletter(c) (_istalpha(c) || (c) == '_')
#endif

#undef PATH_MAX
#define PATH_MAX 1024

#ifndef CHAR_MAX
#define CHAR_MAX 256
#endif

#define MAXLINE	     1024		/* max chars per line */

#define CMD_INITIAL	0
#define CMD_ESC		1
#define CMD_ANSI	2
#define CMD_LINK	3
#define CMD_LINKARG	4

#define GWL_DATA	0		/* offset for client data */

#define CHG_RESET	0		/* unchanged */
#define CHG_CHANGED	1		/* changed, but no clear */
#define CHG_CLEAR	2		/* clear */
#define CHG_CARET	4		/* caret has moved */

#define SEL_CHAR	0		/* character-unit selection */
#define SEL_WORD	1		/* word-unit selection */
#define SEL_LINE	2		/* line-unit selection */

#ifndef EOS
#define EOS 0
#endif

#define ESC 27				/* the escape character */

#define WM_RLC_INPUT	 WM_USER+10	/* Just somewhere ... */
#define WM_RLC_WRITE	 WM_USER+11	/* write data */
#define WM_RLC_FLUSH	 WM_USER+12	/* flush buffered data */
#define WM_RLC_READY	 WM_USER+13	/* Window thread is ready */
#define WM_RLC_CLOSEWIN  WM_USER+14	/* Close the window */
/*#define WM_RLC_MENU	 WM_USER+15	   Insert a menu (defined in menu.h) */

#define IMODE_RAW	1		/* char-by-char */
#define IMODE_COOKED	2		/* line-by-line */

#define NextLine(b, i) ((i) < (b)->height-1 ? (i)+1 : 0)
#define PrevLine(b, i) ((i) > 0 ? (i)-1 : (b)->height-1)
#define Bounds(v, mn, mx) ((v) < (mn) ? (mn) : (v) > (mx) ? (mx) : (v))

#define Control(x) ((x) - '@')

#define streq(s, q) (_tcscmp((s), (q)) == 0)

#include "console_i.h"			/* internal package stuff */

#define OPT_SIZE	0x01
#define OPT_POSITION	0x02

		 /*******************************
		 *	       DATA		*
		 *******************************/

       RlcData  _rlc_stdio = NULL;	/* the main buffer */
static int      _rlc_show;		/* initial show */
static char	_rlc_word_chars[CHAR_MAX]; /* word-characters (selection) */
static const TCHAR *	_rlc_program;		/* name of the program */
static HANDLE   _rlc_hinstance;		/* Global instance */
static HICON    _rlc_hicon;		/* Global icon */



		 /*******************************
		 *	     FUNCTIONS		*
		 *******************************/

static LRESULT WINAPI rlc_wnd_proc(HWND win, UINT msg, WPARAM wP, LPARAM lP);

static void	rcl_setup_ansi_colors(RlcData b);
static void	rlc_place_caret(RlcData b);
static void	rlc_resize_pixel_units(RlcData b, int w, int h);
static RlcData	rlc_make_buffer(int w, int h);
static int	rlc_count_lines(RlcData b, int from, int to);
static void	rlc_add_line(RlcData b);
static void	rlc_open_line(RlcData b);
static void	rlc_update_scrollbar(RlcData b);
static void	rlc_paste(RlcData b);
static void	rlc_init_text_dimensions(RlcData b, HFONT f);
static void	rlc_save_font_options(HFONT f, rlc_console_attr *attr);
static void	rlc_get_options(rlc_console_attr *attr);
static HKEY	rlc_option_key(rlc_console_attr *attr, int create);
static void	rlc_progbase(TCHAR *path, TCHAR *base);
static int	rlc_add_queue(RlcData b, RlcQueue q, int chr);
static int	rlc_add_lines(RlcData b, int here, int add);
static void	rlc_start_selection(RlcData b, int x, int y);
static void	rlc_extend_selection(RlcData b, int x, int y);
static void	rlc_word_selection(RlcData b, int x, int y);
static int	rlc_has_selection(RlcData b);
static void	rlc_set_selection(RlcData b, int sl, int sc, int el, int ec);
static bool	rlc_clicked_link(RlcData b, int x, int y);
static const TCHAR *rlc_over_link(RlcData b, int x, int y);
static href    *rlc_add_link(TextLine tl, const TCHAR *link,
			     int start, int len);
static void	rlc_free_link(href *hr);
static void	rcl_check_links(TextLine tl);
static void	rlc_copy(RlcData b);
static void	rlc_destroy(RlcData b);
static void	rlc_request_redraw(RlcData b);
static void	rlc_redraw(RlcData b);
static int	rlc_breakargs(TCHAR *line, TCHAR **argv);
static void	rlc_resize(RlcData b, int w, int h);
static void	rlc_adjust_line(RlcData b, int line);
static int	text_width(RlcData b, HDC hdc, const text_char *text, int len);
static int	tchar_width(RlcData b, HDC hdc, const TCHAR *text, int len);
static void	rlc_queryfont(RlcData b);
static void     rlc_do_write(RlcData b, TCHAR *buf, int count);
static void     rlc_reinit_line(RlcData b, int line);
static void	rlc_free_line(RlcData b, int line);
static int	rlc_between(RlcData b, int f, int t, int v);
static void	free_user_data(RlcData b);
static RlcQueue	rlc_make_queue(int size);
static int	rlc_from_queue(RlcQueue q);
static int	rlc_is_empty_queue(RlcQueue q);

extern int	main();

static RlcUpdateHook	_rlc_update_hook;
static RlcTimerHook	_rlc_timer_hook;
static RlcRenderHook	_rlc_render_hook;
static RlcRenderAllHook _rlc_render_all_hook;
static RlcInterruptHook _rlc_interrupt_hook;
static RlcResizeHook    _rlc_resize_hook;
static RlcMenuHook	_rlc_menu_hook;
static RlcMessageHook	_rlc_message_hook;
static RlcLinkHook	_rlc_link_hook;
static int _rlc_copy_output_to_debug_output=0;	/* != 0: copy to debugger */
static int	emulate_three_buttons;
static HWND	emu_hwnd;		/* Emulating for this window */

static void _rlc_create_kill_window(RlcData b);
static DWORD WINAPI window_loop(LPVOID arg);	/* console window proc */

//#define _DEBUG 1
#ifdef _DEBUG
#include <stdarg.h>
static void Dprintf(const TCHAR *fmt, ...);
static void Dprint_links(TextLine tl, const TCHAR *msg);
static void Dprint_line(TextLine tl, bool links);
static void Dprint_lines(RlcData b, int from, int to);
#define DEBUG(Code) Code

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
It might look a bit weird not to  use <assert.h>, but for some reason it
looks as if the application thread continues if the asserting is trapped
using  the  normal  assert()!?  Just  but    a  debugger  breakpoint  on
rlc_assert() and all functions normally.

rlc_check_assertions() is a (very) incomplete   check that everything we
expect to be true about the data is indeed the case.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

void
rlc_assert(const TCHAR *msg)
{ MessageBox(NULL, msg, _T("Console assertion failed"), MB_OK|MB_TASKMODAL);
}

void
rlc_check_assertions(RlcData b)
{ int window_last = rlc_add_lines(b, b->window_start, b->window_size-1);
  int y;

  assert(b->last != b->first || b->first == 0);
  assert(b->caret_x >= 0 && b->caret_x < b->width);
					/* TBD: debug properly */
/*assert(rlc_between(b, b->window_start, window_last, b->caret_y));*/
  (void)window_last;

  for(y=0; y<b->height; y++)
  { TextLine tl = &b->lines[y];

    assert(tl->size >= 0 && tl->size <= b->width);
    (void)tl;
  }
}

#else/*_DEBUG*/

#define DEBUG(Code) ((void)0)
#define rlc_check_assertions(b) ((void)0)
static inline void Dprintf(const TCHAR *fmt, ...) {}
static inline void Dprint_links(TextLine tl, const TCHAR *msg) {}
static inline void Dprint_line(TextLine tl, bool links) {}
static inline void Dprint_lines(RlcData b, int from, int to) {}
#endif/*_DEBUG*/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
rlc_long_name(TCHAR *buffer)
	Translate a filename, possibly holding 8+3 abbreviated parts into
	the `real' filename.  I couldn't find a direct call for this.  If
	you have it, I'd be glad to receive a better implementation.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
rlc_long_name(TCHAR *file)
{ TCHAR buf[PATH_MAX];
  TCHAR *i = file;
  TCHAR *o = buf;
  TCHAR *ok = buf;
  int changed = 0;

  while(*i)
  { int dirty = false;

    while(*i && *i != '\\')
    { if ( *i == '~' )
	dirty++;
      *o++ = *i++;
    }
    if ( dirty )
    { WIN32_FIND_DATA data;
      HANDLE h;

      *o = '\0';
      if ( (h=FindFirstFile(buf, &data)) != INVALID_HANDLE_VALUE )
      { _tcscpy(ok, data.cFileName);
	FindClose(h);
	o = ok + _tcslen(ok);
	changed++;
      }
    }
    if ( *i )
      *o++ = *i++;
    ok = o;
  }

  if ( changed )
  { *o = '\0';
    _tcscpy(file, buf);
  }
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
If %PLTERM_CLASS% is in the environment, this   value is used as Windows
class identifier for the console window.   This allows external programs
to start PLWIN.EXE and find the window it  has started in order to embed
it.

In old versions this was fixed to "RlcConsole"
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static TCHAR *
rlc_window_class(HICON icon)
{ static TCHAR winclassname[32];
  static WNDCLASS wndClass;
  HINSTANCE instance = _rlc_hinstance;

  if ( !winclassname[0] )
  { if ( !GetEnvironmentVariable(_T("PLTERM_CLASS"),
				 winclassname, sizeof(winclassname)) )
      _snwprintf(winclassname, sizeof(winclassname)/sizeof(TCHAR),
		 _T("PlTerm-%d"), (int)(intptr_t)instance);

    wndClass.lpszClassName	= winclassname;
    wndClass.style		= CS_HREDRAW|CS_VREDRAW|CS_DBLCLKS;
    wndClass.lpfnWndProc	= (LPVOID) rlc_wnd_proc;
    wndClass.cbClsExtra		= 0;
    wndClass.cbWndExtra		= sizeof(intptr_t);
    wndClass.hInstance		= instance;
    if ( icon )
      wndClass.hIcon		= icon;
    else
      wndClass.hIcon		= LoadIcon(NULL, IDI_APPLICATION);
    wndClass.hCursor		= LoadCursor(NULL, IDC_IBEAM);
    wndClass.hbrBackground	= (HBRUSH) NULL;
    wndClass.lpszMenuName	= NULL;

    RegisterClass(&wndClass);
  }

  return winclassname;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
There are two ways to get the commandline.   It  is passed to WinMain as
8-bit string. This version does *not* include  the command itself. It is
also available through GetCommandLine(), which  does include the command
itself and returns LPTSTR (Unicode/ANSI). We assume the latter.

Nevertheless, for backward compatibility as well  as easy to extract the
full pathname of the  executable,  we   replace  argv[0]  with  the intptr_t
filename version of the current module, so argv[0] is guaranteed to be a
full path refering to the .exe file.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

int
rlc_main(HANDLE hInstance, HANDLE hPrevInstance,
	 LPTSTR lpszCmdLine, int nCmdShow,
	 RlcMain mainfunc, HICON icon)
{ TCHAR *	    argv[100];
  int		    argc;
  TCHAR		    program[PATH_MAX];
  TCHAR		    progbase[100];
  RlcData           b;
  rlc_console_attr  attr;

  initHeapDebug();

  _rlc_hinstance = hInstance;
  _rlc_show = nCmdShow;
  _rlc_hicon = icon;

  GetModuleFileName(hInstance, program, sizeof(program));
  rlc_long_name(program);
  argc = rlc_breakargs(lpszCmdLine, argv);
  argv[0] = program;
  rlc_progbase(argv[0], progbase);

  memset(&attr, 0, sizeof(attr));
  _rlc_program = attr.title = progbase;
  _rlc_stdio = b = rlc_create_console(&attr);

  if ( mainfunc )
    return (*mainfunc)(b, argc, argv);
  else
    return 0;
}



rlc_console
rlc_create_console(rlc_console_attr *attr)
{ RlcData b;
  MSG msg;
  const TCHAR *title;

  rlc_get_options(attr);

  if ( attr->title )
    title = attr->title;
  else
    title = _T("Untitled");

  b = rlc_make_buffer(attr->width, attr->savelines);
  b->create_attributes = attr;
  _tcscpy(b->current_title, title);
  if ( attr->key )
  { b->regkey_name = _tcsdup(attr->key);
  }

  rlc_init_text_dimensions(b, NULL);
  _rlc_create_kill_window(b);

  DuplicateHandle(GetCurrentProcess(),
		  GetCurrentThread(),
		  GetCurrentProcess(),
		  &b->application_thread,
		  0,
		  false,
		  DUPLICATE_SAME_ACCESS);
  b->application_thread_id = GetCurrentThreadId();
  b->console_thread = CreateThread(NULL,			/* security */
				   2048,			/* stack */
				   window_loop, b,		/* proc+arg */
				   0,				/* flags */
				   &b->console_thread_id);	/* id */
					/* wait till the window is created */
  GetMessage(&msg, NULL, WM_RLC_READY, WM_RLC_READY);
  b->create_attributes = NULL;		/* release this data */

  return b;
}


static void
rlc_create_window(RlcData b)
{ HWND hwnd;
  rlc_console_attr *a = b->create_attributes;
  RECT rect;
  DWORD style = (WS_OVERLAPPEDWINDOW|WS_VSCROLL);

/* One would assume AdjustWindowRect() uses WS_VSCROLL to add the width of
   the scrollbar.  I think this isn't true, but maybe there is another reason
   for getting 2 characters shorter each invocation ...
*/

  rect.left   = a->x;
  rect.top    = a->y;
  rect.right  = a->x + (a->width+2) * b->cw + GetSystemMetrics(SM_CXVSCROLL);
  rect.bottom = a->y + a->height * b->ch;

  AdjustWindowRect(&rect, style, true);
  hwnd = CreateWindow(rlc_window_class(_rlc_hicon), b->current_title,
		      style,
		      a->x, a->y,
		      rect.right - rect.left,
		      rect.bottom - rect.top,
		      NULL, NULL, _rlc_hinstance, NULL);

  b->window = hwnd;
  SetWindowLongPtr(hwnd, GWL_DATA, (LONG_PTR) b);
  SetScrollRange(hwnd, SB_VERT, 0, b->sb_lines, false);
  SetScrollPos(hwnd, SB_VERT, b->sb_start, true);

  b->queue    = rlc_make_queue(256);
  b->sb_lines = rlc_count_lines(b, b->first, b->last);
  b->sb_start = rlc_count_lines(b, b->first, b->window_start);

  rcl_setup_ansi_colors(b);
  b->foreground = GetSysColor(COLOR_WINDOWTEXT);
  b->background = GetSysColor(COLOR_WINDOW);
  b->sel_foreground = GetSysColor(COLOR_HIGHLIGHTTEXT);
  b->sel_background = GetSysColor(COLOR_HIGHLIGHT);
  if ( GetSystemMetrics(SM_CMOUSEBUTTONS) == 2 )
    emulate_three_buttons = 120;

  rlc_add_menu_bar(b->window);

  ShowWindow(hwnd, _rlc_show);
  UpdateWindow(hwnd);
}


int
rlc_iswin32s()
{ if( GetVersion() & 0x80000000 && (GetVersion() & 0xFF) ==3)
    return true;
  else
    return false;
}


static void
rlc_progbase(TCHAR *path, TCHAR *base)
{ TCHAR *s;
  TCHAR *e;

  if ( !(s=_tcsrchr(path, '\\')) )
    s = path;				/* takes the filename part */
  else
    s++;
  if ( !(e = _tcschr(s, '.')) )
    _tcscpy(base, s);
  else
  { _tcsncpy(base, s, e-s);
    base[e-s] = '\0';
  }
}

		 /*******************************
		 *	  HIDDEN WINDOW		*
		 *******************************/

static LRESULT WINAPI
rlc_kill_wnd_proc(HWND hwnd, UINT message, UINT wParam, LONG lParam)
{ switch(message)
  { case WM_DESTROY:
      PostQuitMessage(0);
      return 0;
  }

  return DefWindowProc(hwnd, message, wParam, lParam);
}

static TCHAR *
rlc_kill_window_class()
{ static TCHAR winclassname[32];
  static WNDCLASS wndClass;
  HINSTANCE instance = _rlc_hinstance;

  if ( !winclassname[0] )
  { _snwprintf(winclassname, sizeof(winclassname)/sizeof(TCHAR),
	       _T("Console-hidden-win%d"), (int)(intptr_t)instance);

    wndClass.style		= 0;
    wndClass.lpfnWndProc	= (LPVOID) rlc_kill_wnd_proc;
    wndClass.cbClsExtra		= 0;
    wndClass.cbWndExtra		= 0;
    wndClass.hInstance		= instance;
    wndClass.hIcon		= NULL;
    wndClass.hCursor		= NULL;
    wndClass.hbrBackground	= GetStockObject(WHITE_BRUSH);
    wndClass.lpszMenuName	= NULL;
    wndClass.lpszClassName	= winclassname;

    RegisterClass(&wndClass);
  }

  return winclassname;
}


static void
_rlc_create_kill_window(RlcData b)
{ b->kill_window = CreateWindow(rlc_kill_window_class(),
				_T("Console hidden window"),
				0,
				0, 0, 32, 32,
				NULL, NULL, _rlc_hinstance, NULL);
}


		 /*******************************
		 *     REGISTRY COMMUNICATION	*
		 *******************************/

#define MAXREGSTRLEN 1024

static void
reg_save_int(HKEY key, const TCHAR *name, int value)
{ DWORD val = value;

  if ( RegSetValueEx(key, name, 0,
		     REG_DWORD_LITTLE_ENDIAN,
		     (LPBYTE)&val, sizeof(val)) != ERROR_SUCCESS )
    DEBUG(MessageBox(NULL, _T("Failed to save int setting"),
		     _T("Error"), MB_OK));
}

static void
reg_save_str(HKEY key, const TCHAR *name, TCHAR *value)
{ if ( RegSetValueEx(key, name, 0, REG_SZ,
		     (LPBYTE)value, (DWORD)(_tcslen(value)+1)*sizeof(TCHAR)) != ERROR_SUCCESS )
    DEBUG(MessageBox(NULL, _T("Failed to save string setting"), _T("Error"), MB_OK));
}


static void
rlc_save_options(RlcData b)
{ HKEY key;
  rlc_console_attr attr;

  memset(&attr, 0, sizeof(attr));
  attr.key = b->regkey_name;

  if ( !(key = rlc_option_key(&attr, true)) )
    return;

  reg_save_int(key, _T("SaveLines"),  b->height);
  if ( b->modified_options & OPT_SIZE )
  { reg_save_int(key, _T("Width"),    b->width);
    reg_save_int(key, _T("Height"),   b->window_size);
  }
  if ( b->modified_options & OPT_POSITION )
  { reg_save_int(key, _T("X"),	  b->win_x);
    reg_save_int(key, _T("Y"),	  b->win_y);
  }

  rlc_save_font_options(b->hfont, &attr);
  if ( attr.face_name[0] )
  { reg_save_str(key, _T("FaceName"),    attr.face_name);
    reg_save_int(key, _T("FontFamily"),  attr.font_family);
    reg_save_int(key, _T("FontSize"),    attr.font_size);
    reg_save_int(key, _T("FontWeight"),  attr.font_weight);
    reg_save_int(key, _T("FontCharSet"), attr.font_char_set);
  }

  RegCloseKey(key);
}


static void
reg_get_int(HKEY key, const TCHAR *name, int mn, int def, int mx, int *value)
{ DWORD type;
  BYTE  data[8];
  DWORD len = sizeof(data);

  if ( *value )
    return;				/* use default */

  if ( RegQueryValueEx(key, name, NULL, &type, data, &len) == ERROR_SUCCESS )
  { switch(type)
    { /*case REG_DWORD:*/		/* Same case !? */
      case REG_DWORD_LITTLE_ENDIAN:
      { DWORD *valp = (DWORD *)data;
	int v = *valp;

	if ( mn < mx )
	{ if ( v < mn )
	    v = mn;
	  else if ( v > mx )
	    v = mx;
	}

	*value = v;
      }
    }
  } else
    *value = def;
}


static void
reg_get_str(HKEY key, const TCHAR *name, TCHAR *value, int length)
{ DWORD type;
  BYTE  data[MAXREGSTRLEN*sizeof(TCHAR)];
  DWORD len = sizeof(data);

  if ( *value )
    return;				/* use default */

  if ( RegQueryValueEx(key, name, NULL, &type, data, &len) == ERROR_SUCCESS )
  { switch(type)
    { case REG_SZ:
      { TCHAR *val = (TCHAR*)data;
	_tcsncpy(value, val, length-1);
	value[length-1] = '\0';
      }
    }
  }
}


HKEY
reg_open_key(TCHAR **which, int create)
{ HKEY key = HKEY_CURRENT_USER;
  DWORD disp;
  LONG rval;

  for( ; *which; which++)
  { HKEY tmp;

    if ( which[1] )
    { if ( RegOpenKeyEx(key, which[0], 0L, KEY_READ, &tmp) == ERROR_SUCCESS )
      { key = tmp;
	continue;
      }

      if ( !create )
	return NULL;
    }

    rval = RegCreateKeyEx(key, which[0], 0, _T(""), 0,
			  KEY_ALL_ACCESS, NULL, &tmp, &disp);
    RegCloseKey(key);
    if ( rval == ERROR_SUCCESS )
      key = tmp;
    else
      return NULL;
  }

  return key;
}


static HKEY
rlc_option_key(rlc_console_attr *attr, int create)
{ TCHAR Prog[256];
  TCHAR *address[] = { _T("Software"),
		      RLC_VENDOR,
		      Prog,
		      _T("Console"),
		      (TCHAR *)attr->key,	/* possible secondary key */
		      NULL
		    };
  const TCHAR *s;
  TCHAR *q;

  for(s=_rlc_program, q=Prog; *s; s++, q++) /* capitalise the key */
  { *q = (s==_rlc_program ? _totupper(*s) : _totlower(*s));
  }
  *q = EOS;

  return reg_open_key(address, create);
}


static void
rlc_get_options(rlc_console_attr *attr)
{ HKEY key;

  if ( !(key = rlc_option_key(attr, false)) )
  { if ( !attr->width  )    attr->width = 80;
    if ( !attr->height )    attr->height = 24;
    if ( !attr->savelines ) attr->savelines = 200;

    return;
  }

{ int minx, miny, maxx, maxy;
  RECT rect;

  SystemParametersInfo(SPI_GETWORKAREA, 0, &rect, 0);
  minx = rect.top;
  miny = rect.left;
  maxx = rect.right  - 40;
  maxy = rect.bottom - 40;

  reg_get_int(key, _T("SaveLines"),   200,  200, 100000, &attr->savelines);
  reg_get_int(key, _T("Width"),        20,	 80,    300, &attr->width);
  reg_get_int(key, _T("Height"),        5,	 24,    100, &attr->height);
  reg_get_int(key, _T("X"),		 minx, minx,   maxx, &attr->x);
  reg_get_int(key, _T("Y"),		 miny, miny,   maxy, &attr->y);
}

  reg_get_str(key, _T("FaceName"), attr->face_name,
	      sizeof(attr->face_name)/sizeof(TCHAR));
  reg_get_int(key, _T("FontFamily"),    0,  0,  0, &attr->font_family);
  reg_get_int(key, _T("FontSize"),      0,  0,  0, &attr->font_size);
  reg_get_int(key, _T("FontWeight"),    0,  0,  0, &attr->font_weight);
  reg_get_int(key, _T("FontCharSet"),   0,  0,  0, &attr->font_char_set);

  RegCloseKey(key);
}



/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Windows-'95 appears to quote  names  of   files  because  files may hold
spaces. rlc_breakargs() will pass a quoted   strings as one argument. If
it can't find the closing quote, it  will   tread  the quote as a normal
character.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
rlc_breakargs(TCHAR *line, TCHAR **argv)
{ int argc = 0;

  while(*line)
  { int q;

    while(*line && _istspace(*line))
      line++;

    if ( (q = *line) == '"' || q == '\'' )	/* quoted arguments */
    { TCHAR *start = line+1;
      TCHAR *end = start;

      while( *end && *end != q )
	end++;
      if ( *end == q )
      { *end = '\0';
        argv[argc++] = start;
	line = end+1;
	continue;
      }
    }

    if ( *line )
    { argv[argc++] = line;
      while(*line && !_istspace(*line))
	line++;
      if ( *line )
	*line++ = '\0';
    }
  }
  argv[argc] = NULL;			/* add trailing NULL pointer to argv */

  return argc;
}


		 /*******************************
		 *	    ANSI COLORS		*
		 *******************************/

/* See http://en.wikipedia.org/wiki/ANSI_escape_code */

static void
rcl_setup_ansi_colors(RlcData b)
{ b->sgr_flags = TF_DEFAULT;

#ifdef ANSI_VGA_COLORS
					/* normal versions */
  b->ansi_color[0]  = RGB(  0,  0,  0);	/* black */
  b->ansi_color[1]  = RGB(170,  0,  0);	/* red */
  b->ansi_color[2]  = RGB(0,  170,  0);	/* green */
  b->ansi_color[3]  = RGB(170, 85,  0);	/* yellow */
  b->ansi_color[4]  = RGB(  0,  0,170);	/* blue */
  b->ansi_color[5]  = RGB(170,  0,170);	/* magenta */
  b->ansi_color[6]  = RGB(  0,170,170);	/* cyan */
  b->ansi_color[7]  = RGB(170,170,170);	/* white */
					/* bright/light versions */
  b->ansi_color[8]  = RGB( 85, 85, 85);	/* black */
  b->ansi_color[9]  = RGB(255, 85, 85);	/* red */
  b->ansi_color[10] = RGB( 85,255, 85);	/* green */
  b->ansi_color[11] = RGB(255,255, 85);	/* yellow */
  b->ansi_color[12] = RGB( 85, 85,255);	/* blue */
  b->ansi_color[13] = RGB(255, 85,255);	/* magenta */
  b->ansi_color[14] = RGB( 85,255,255);	/* cyan */
  b->ansi_color[15] = RGB(255,255,255);	/* white */
#else /*XTERM*/
					/* normal versions */
  b->ansi_color[0]  = RGB(  0,  0,  0);	/* black */
  b->ansi_color[1]  = RGB(205,  0,  0);	/* red */
  b->ansi_color[2]  = RGB(0,  205,  0);	/* green */
  b->ansi_color[3]  = RGB(205,205,  0);	/* yellow */
  b->ansi_color[4]  = RGB(  0,  0,238);	/* blue */
  b->ansi_color[5]  = RGB(205,  0,205);	/* magenta */
  b->ansi_color[6]  = RGB(  0,205,205);	/* cyan */
  b->ansi_color[7]  = RGB(229,229,229);	/* white */
					/* bright/light versions */
  b->ansi_color[8]  = RGB(127,127,127);	/* black */
  b->ansi_color[9]  = RGB(255,  0,  0);	/* red */
  b->ansi_color[10] = RGB(  0,255,  0);	/* green */
  b->ansi_color[11] = RGB(255,255,  0);	/* yellow */
  b->ansi_color[12] = RGB( 92, 92,255);	/* blue */
  b->ansi_color[13] = RGB(255,  0,255);	/* magenta */
  b->ansi_color[14] = RGB(  0,255,255);	/* cyan */
  b->ansi_color[15] = RGB(255,255,255);	/* white */
#endif
}



		 /*******************************
		 *	     ATTRIBUTES		*
		 *******************************/

COLORREF
rlc_color(rlc_console con, int which, COLORREF c)
{ HDC hdc;
  COLORREF old;
  RlcData b = rlc_get_data(con);

  hdc = GetDC(NULL);
  c = GetNearestColor(hdc, c);
  ReleaseDC(NULL, hdc);

  switch(which)
  { case RLC_WINDOW:
      old = b->background;
      b->background = c;
      break;
    case RLC_TEXT:
      old = b->foreground;
      b->foreground = c;
      break;
    case RLC_HIGHLIGHT:
      old = b->sel_background;
      b->sel_background = c;
      break;
    case RLC_HIGHLIGHTTEXT:
      old = b->sel_foreground;
      b->sel_foreground = c;
      break;
    default:
      return (COLORREF)-1;
  }

  if ( b->window )
    InvalidateRect(b->window, NULL, true);

  return old;
}


static int
rlc_kill(RlcData b)
{ DWORD_PTR result;

  switch(b->closing++)
  { case 0:
      b->queue->flags |= RLC_EOF;
      PostThreadMessage(b->application_thread_id, WM_RLC_INPUT, 0, 0);
      return true;
    case 1:
      if ( _rlc_interrupt_hook )
      { (*_rlc_interrupt_hook)(b, SIGINT);
	return true;
      }
    default:
      if ( !SendMessageTimeout(b->kill_window,
			       WM_DESTROY,
			       0, 0,
			       SMTO_ABORTIFHUNG,
			       5000,
			       &result) )
      { if ( b->window )
	{ switch( MessageBox(b->window,
			     _T("Main task is not responding.")
			     _T("Click \"OK\" to terminate it"),
			     _T("Error"),
			     MB_OKCANCEL|MB_ICONEXCLAMATION|MB_APPLMODAL) )
	  { case IDCANCEL:
	      return false;
	  }
	  TerminateThread(b->application_thread, 1);

	  return true;
	}
      }
  }

  return false;
}


static void
rlc_interrupt(RlcData b)
{ if ( _rlc_interrupt_hook )
    (*_rlc_interrupt_hook)((rlc_console)b, SIGINT);
  else
    raise(SIGINT);
}


static void
typed_char(RlcData b, int chr)
{ if ( chr == Control('C') && rlc_has_selection(b) )
  { rlc_copy(b);
    return;
  }

  rlc_set_selection(b, 0, 0, 0, 0);

  if ( chr == Control('C') )
    rlc_interrupt(b);
  else if ( chr == Control('V') )
    rlc_paste(b);
  else if ( b->queue )
    rlc_add_queue(b, b->queue, chr);
}


		 /*******************************
		 *	 WINDOW PROCEDURE	*
		 *******************************/

#undef MAKEPOINTS

static inline POINTS
MAKEPOINTS(LPARAM lParam)
{ union
  { LPARAM p;
    POINTS pt;
  } u;

  u.p = lParam;
  return u.pt;
}


static void
rlc_destroy(RlcData b)
{ if ( b && b->window )
  { DestroyWindow(b->window);
    b->window = NULL;
    b->closing = 3;
  }
}


static int
IsDownKey(int code)
{ short mask = GetKeyState(code);

  return mask & 0x8000;
}


static LRESULT WINAPI
rlc_wnd_proc(HWND hwnd, UINT message, WPARAM wParam, LPARAM lParam)
{ RlcData b = (RlcData) GetWindowLongPtr(hwnd, GWL_DATA);

  switch(message)
  { case WM_CREATE:
      return 0;

    case WM_SIZE:
      if ( wParam != SIZE_MINIMIZED )
      { rlc_resize_pixel_units(b, LOWORD(lParam), HIWORD(lParam));
	b->modified_options |= OPT_SIZE;
      }
      return 0;

    case WM_MOVE:
    { WINDOWPLACEMENT placement;

      placement.length = sizeof(placement);
      GetWindowPlacement(hwnd, &placement);

      if ( placement.showCmd == SW_SHOWNORMAL )
      { b->win_x = placement.rcNormalPosition.left;
	b->win_y = placement.rcNormalPosition.top;

	b->modified_options |= OPT_POSITION;
      }

      return 0;
    }

    case WM_SETFOCUS:
      b->has_focus = true;
      CreateCaret(hwnd, NULL, b->fixedfont ? b->cw : 3, b->ch-1);
      rlc_place_caret(b);
      return 0;

    case WM_KILLFOCUS:
      b->has_focus = false;
      b->caret_is_shown = false;
      HideCaret(hwnd);
      DestroyCaret();
      return 0;

    case WM_PAINT:
      rlc_redraw(b);
      return 0;

    case WM_COMMAND:
    { UINT  item  = (UINT) LOWORD(wParam);
      const TCHAR *name;

      switch( item )
      { case IDM_PASTE:
	  rlc_paste(b);
	  return 0;
	case IDM_COPY:
	  rlc_copy(b);
	  return 0;			/* no op: already done */
	case IDM_CUT:
	  break;			/* TBD: cut */
	case IDM_BREAK:
	  rlc_interrupt(b);
	  break;
	case IDM_FONT:
	  rlc_queryfont(b);
	  return 0;
	case IDM_EXIT:
	  if ( rlc_kill(b) )
	    return 0;
	  break;
	default:
	  if ( (name = lookupMenuId(item)) )
	  { if ( _rlc_menu_hook )
	    { (*_rlc_menu_hook)(b, name);
	    }

	    return 0;
	  }
      }

      break;
    }

  { int chr;

    case WM_KEYDOWN:			/* up is sent only once */
    { switch((int) wParam)
      { case VK_DELETE:	chr = 127;		break;
	case VK_LEFT:	chr = Control('B');	break;
	case VK_RIGHT:	chr = Control('F');	break;
	case VK_UP:	chr = Control('P');	break;
	case VK_DOWN:	chr = Control('N');	break;
	case VK_HOME:	chr = Control('A');	break;
	case VK_END:	chr = Control('E');	break;
        case VK_CANCEL: rlc_interrupt(b);       return 0;

        case VK_PRIOR:			/* page up */
	{ int maxdo = rlc_count_lines(b, b->first, b->window_start);
	  int pagdo = b->window_size - 1;
	  b->window_start = rlc_add_lines(b, b->window_start,
					  -min(maxdo, pagdo));

	scrolledbykey:
	  rlc_update_scrollbar(b);
	  InvalidateRect(hwnd, NULL, false);

	  return 0;
	}
	case VK_NEXT:			/* page down */
	{ int maxup = rlc_count_lines(b, b->window_start, b->last);
	  int pagup = b->window_size - 1;
	  b->window_start = rlc_add_lines(b, b->window_start,
					  min(maxup, pagup));
	  goto scrolledbykey;
	}
	default:
	  goto break2;
      }
      if ( chr > 0 )
      { if ( IsDownKey(VK_CONTROL) )
	  typed_char(b, ESC);

	typed_char(b, chr);

	return 0;
      }
    break2:
      break;
    }
	case WM_UNICHAR:
	  chr = (int)wParam;
	  typed_char(b, chr);
	  return 0;
    case WM_SYSCHAR:	typed_char(b, ESC); /* Play escape-something */
    case WM_CHAR:	chr = (int)wParam;

      typed_char(b, chr);

      return 0;
  }

					/* selection handling */
    case WM_MBUTTONDOWN:
    middle_down:
      return 0;

    case WM_MBUTTONUP:
    middle_up:
      rlc_paste(b);

      return 0;

    case WM_LBUTTONDOWN:
    { POINTS pt;

      if ( emulate_three_buttons )
      { MSG msg;

	Sleep(emulate_three_buttons);
	if ( PeekMessage(&msg, hwnd,
			 WM_RBUTTONDOWN, WM_RBUTTONDOWN, PM_REMOVE) )
	{ emu_hwnd = hwnd;
	  goto middle_down;
	}
      }

      pt = MAKEPOINTS(lParam);
      rlc_start_selection(b, pt.x, pt.y);

      return 0;
    }

    case WM_LBUTTONUP:
      POINTS pt;
      pt = MAKEPOINTS(lParam);
      if ( rlc_clicked_link(b, pt.x, pt.y) )
	return 0;
      /*FALLTHROUGH*/
    case WM_RBUTTONUP:
      if ( emu_hwnd == hwnd )
      { if ( wParam & (MK_RBUTTON|MK_LBUTTON) )
	{ goto middle_up;
	} else
	{ emu_hwnd = 0;
	  return 0;
	}
      } else
      { rlc_copy(b);

	return 0;
      }

    case WM_LBUTTONDBLCLK:
    { POINTS pt = MAKEPOINTS(lParam);

      rlc_word_selection(b, pt.x, pt.y);

      return 0;
    }

    case WM_RBUTTONDOWN:
    { POINTS pt;

      if ( emulate_three_buttons )
      { MSG msg;

	Sleep(emulate_three_buttons);
	if ( PeekMessage(&msg, hwnd,
			 WM_LBUTTONDOWN, WM_LBUTTONDOWN, PM_REMOVE) )
	{ emu_hwnd = hwnd;
	  goto middle_down;
	}
      }

      pt = MAKEPOINTS(lParam);
      rlc_extend_selection(b, pt.x, pt.y);

      return 0;
    }

    case WM_SETCURSOR:
    { if ( LOWORD(lParam) == HTCLIENT )
      { POINT pt;

	GetCursorPos(&pt);              // Get screen coordinates
	ScreenToClient(b->window, &pt);
	const TCHAR *href = rlc_over_link(b, pt.x, pt.y);
	if ( href )
	  SetCursor(b->link_cursor);
	else
	  SetCursor(b->cursor);
	return true;
      }
      break;
    }
    case WM_MOUSEMOVE:
    { POINTS pt = MAKEPOINTS(lParam);

      if ( (wParam & (MK_LBUTTON|MK_RBUTTON)) &&
	   (wParam & (MK_LBUTTON|MK_RBUTTON)) != (MK_LBUTTON|MK_RBUTTON) )
      { rlc_extend_selection(b, pt.x, pt.y);

	return 0;
      }

      break;
    }

    case WM_MOUSEWHEEL:
    { short angle = (short)HIWORD(wParam);

      if ( angle < 0 )
      { if ( b->window_start != b->last )
	  b->window_start = NextLine(b, b->window_start);
      } else
      { if ( b->window_start != b->first )
	  b->window_start = PrevLine(b, b->window_start);
      }

      rlc_update_scrollbar(b);
      InvalidateRect(hwnd, NULL, false);

      return 0;
    }
					/* scrolling */
    case WM_VSCROLL:
    { switch( LOWORD(wParam) )
      { case SB_LINEUP:
	  if ( b->window_start != b->first )
	    b->window_start = PrevLine(b, b->window_start);
	  break;
	case SB_LINEDOWN:
	  if ( b->window_start != b->last )
	    b->window_start = NextLine(b, b->window_start);
	  break;
	case SB_PAGEUP:
	{ int maxdo = rlc_count_lines(b, b->first, b->window_start);
	  int pagdo = b->window_size - 1;
	  b->window_start = rlc_add_lines(b, b->window_start,
					  -min(maxdo, pagdo));
	  break;
	}
	case SB_PAGEDOWN:
	{ int maxup = rlc_count_lines(b, b->window_start, b->last);
	  int pagup = b->window_size - 1;
	  b->window_start = rlc_add_lines(b, b->window_start,
					  min(maxup, pagup));
	  break;
	}
	case SB_THUMBTRACK:
	  b->window_start = rlc_add_lines(b, b->first, HIWORD(wParam));
	  break;
      }

      rlc_update_scrollbar(b);
      InvalidateRect(hwnd, NULL, false);

      return 0;
    }

    case WM_TIMER:
      if ( _rlc_timer_hook && wParam >= RLC_APPTIMER_ID )
      { (*_rlc_timer_hook)((int) wParam);

	return 0;
      }
      break;

    case WM_RENDERALLFORMATS:
      if ( _rlc_render_all_hook )
      { (*_rlc_render_all_hook)();

        return 0;
      }
      break;

    case WM_RENDERFORMAT:
      if ( _rlc_render_hook && (*_rlc_render_hook)(wParam) )
        return 0;

      break;

    case WM_ERASEBKGND:
    { HDC hdc = (HDC) wParam;
      RECT rect;
      HBRUSH hbrush;
      COLORREF rgb = b->background;

      hbrush = CreateSolidBrush(rgb);
      GetClipBox(hdc, &rect);
      FillRect(hdc, &rect, hbrush);
      DeleteObject(hbrush);

      return 1;				/* non-zero: I've erased it */
    }

    case WM_SYSCOLORCHANGE:
      b->foreground     = GetSysColor(COLOR_WINDOWTEXT);
      b->background     = GetSysColor(COLOR_WINDOW);
      b->sel_foreground = GetSysColor(COLOR_HIGHLIGHTTEXT);
      b->sel_background = GetSysColor(COLOR_HIGHLIGHT);
      return 0;

    case WM_RLC_WRITE:
    { int count = (int)wParam;
      TCHAR *buf = (TCHAR *)lParam;

      if ( OQSIZE - b->output_queued > count )
      { _tcsncpy(&b->output_queue[b->output_queued], buf, count);
	b->output_queued += count;
      } else
      { if ( b->output_queued > 0 )
	  rlc_flush_output(b);

	if ( count <= OQSIZE )
	{ _tcsncpy(b->output_queue, buf, count);
	  b->output_queued = count;
	} else
	  rlc_do_write(b, buf, count);
      }

      return 0;
    }

    case WM_RLC_FLUSH:
    { rlc_flush_output(b);
      return 0;
    }

    case WM_RLC_MENU:
    { rlc_menu_action((rlc_console) b, (struct menu_data*)lParam);

      return 0;
    }

    case WM_RLC_CLOSEWIN:
      return 0;

    case WM_CLOSE:
      if ( rlc_kill(b) )
        return 0;
      break;

    case WM_DESTROY:
      b->window = NULL;
      PostQuitMessage(0);
      return 0;
  }

  return DefWindowProc(hwnd, message, wParam, lParam);
}

static int
rlc_get_message(MSG *msg, HWND hwnd, UINT low, UINT high)
{ int rc;

again:
  if ( (rc=PeekMessage(msg, hwnd, low, WM_RLC_INPUT, PM_REMOVE)) )
  { if ( _rlc_message_hook &&
	 (*_rlc_message_hook)(msg->hwnd, msg->message,
			      msg->wParam, msg->lParam) )
      goto again;
  } else if ( (rc=GetMessage(msg, hwnd, low, high)) )
  { if ( _rlc_message_hook &&
	 (*_rlc_message_hook)(msg->hwnd, msg->message,
			      msg->wParam, msg->lParam) )
      goto again;
  }

  return rc;
}


static void
rlc_dispatch(RlcData b)
{ MSG msg;

  if ( rlc_get_message(&msg, NULL, 0, 0) && msg.message != WM_RLC_CLOSEWIN )
  { /* DEBUG(Dprintf("Thread %x got message 0x%04x\n",
		     GetCurrentThreadId(), msg.message));
    */
    TranslateMessage(&msg);
    DispatchMessage(&msg);
    rlc_flush_output(b);
    return;
  } else
  { DEBUG(Dprintf(_T("Thread %x got WM_RLC_CLOSEWIN\n"),
		  GetCurrentThreadId()));
    b->queue->flags |= RLC_EOF;
  }
}


void
rlc_yield()
{ MSG msg;

  while ( PeekMessage(&msg, NULL, 0, 0, PM_REMOVE) )
  { TranslateMessage(&msg);
    DispatchMessage(&msg);
  }
}

		 /*******************************
		 *	 CHARACTER TYPES	*
		 *******************************/

static void
rlc_init_word_chars()
{ int i;

  for(i=0; i<CHAR_MAX; i++)
    _rlc_word_chars[i] = (isalnum(i) || i == '_') ? true : false;
}


void
rlc_word_char(int chr, int isword)
{ if ( chr > 0 && chr < CHAR_MAX )
    _rlc_word_chars[chr] = isword;
}


int
rlc_is_word_char(int chr)
{ if ( chr > 0 && chr < CHAR_MAX )
    return _rlc_word_chars[chr];

  return _istalnum((wint_t)chr);	/* only UNICODE version */
}


		 /*******************************
		 *	    SELECTION		*
		 *******************************/

#define SelLT(l1, c1, l2, c2) ((l1) < (l2) || ((l1) == (l2) && (c1) < (c2)))
#define SelEQ(l1, c1, l2, c2) ((l1) == (l2) && (c1) == (c2))

static int
rlc_min(RlcData b, int x, int y)
{ if ( rlc_count_lines(b, b->first, x) < rlc_count_lines(b, b->first, y) )
    return x;

  return y;
}


static int
rlc_max(RlcData b, int x, int y)
{ if ( rlc_count_lines(b, b->first, x) > rlc_count_lines(b, b->first, y) )
    return x;

  return y;
}


static void
rlc_changed_line(RlcData b, int i, int mask)
{ b->lines[i].changed |= mask;
}


static void
rlc_set_selection(RlcData b, int sl, int sc, int el, int ec)
{ int sch = rlc_min(b, sl, b->sel_start_line);
  int ech = rlc_max(b, el, b->sel_end_line);
  int nel = NextLine(b, el);
  int nsel= NextLine(b, b->sel_end_line);
  int i;
  int innow  = false;
  int insoon = false;

					/* find the lines that changed */
  for(i=sch; ; i = NextLine(b, i))
  { if ( i == sl )
    { insoon = true;
      if ( i == b->sel_start_line )
      { innow = true;
	if ( sc != b->sel_start_char ||
	     (i == el && i != b->sel_end_line) ||
	     (i == b->sel_end_line && i != el) )
	  rlc_changed_line(b, i, CHG_CHANGED);
      } else
	rlc_changed_line(b, i, CHG_CHANGED);
    } else if ( i == b->sel_start_line )
    { innow = true;
      rlc_changed_line(b, i, CHG_CHANGED);
    }

    if ( i == b->sel_end_line )
    { if ( (i == el && ec != b->sel_end_char) || el != i )
	rlc_changed_line(b, i, CHG_CHANGED);
    }

    if ( innow != insoon )
      rlc_changed_line(b, i, CHG_CHANGED);

    if ( i == nel )
    { insoon = false;
      if ( i == nsel )
	innow = false;
      else
	rlc_changed_line(b, i, CHG_CHANGED);
    } else if ( i == nsel )
    { innow = false;
      rlc_changed_line(b, i, CHG_CHANGED);
    }

    if ( i == ech )
      break;
  }

					/* update the attributes */
  b->sel_start_line = sl;
  b->sel_start_char = sc;
  b->sel_end_line   = el;
  b->sel_end_char   = ec;

					/* ... and request a repaint */
  rlc_request_redraw(b);
}


void
rlc_translate_mouse(RlcData b, int x, int y, int *line, int *chr)
{ int ln = b->window_start;
  int n = b->window_size;		/* # lines */
  TextLine tl;
  x-= b->cw;				/* margin */

  if ( !b->window )
    return;

  while( y > b->ch && ln != b->last && n-- > 0 )
  { ln = NextLine(b, ln);
    y -= b->ch;
  }
  *line = ln;
  tl = &b->lines[ln];

  if ( b->fixedfont )
  { *chr = min(x/b->cw, tl->size);
  } else if ( tl->size == 0 )
  { *chr = 0;
  } else
  { text_char *s = tl->text;
    HDC hdc = GetDC(b->window);
    int f = 0;
    int t = tl->size;
    int m = (f+t)/2;
    int i;

    SelectObject(hdc, b->hfont);

    for(i=10; --i > 0; m=(f+t)/2)
    { int w;

      w = text_width(b, hdc, s, m);
      if ( x > w )
      { int cw;

	GetCharWidth32(hdc, s[m].code, s[m].code, &cw);
	if ( x < w+cw )
	{ *chr = m;
	  return;
	}
	f = m+1;
      } else
      { t = m;
      }
    }

    *chr = m;
  }
}


static void
rlc_start_selection(RlcData b, int x, int y)
{ int l, c;

  rlc_translate_mouse(b, x, y, &l, &c);
  b->sel_unit = SEL_CHAR;
  b->sel_org_line = l;
  b->sel_org_char = c;
  rlc_set_selection(b, l, c, l, c);
}

static bool
rlc_clicked_link(RlcData b, int x, int y)
{ if ( _rlc_link_hook )
  { int l, c;

    rlc_translate_mouse(b, x, y, &l, &c);
    if ( b->sel_unit == SEL_CHAR &&
	 b->sel_org_line == l &&
	 b->sel_org_char == c )
    { TextLine tl = &b->lines[l];
      for(href *hr=tl->links; hr; hr = hr->next)
      { if ( c >= hr->start && c <= hr->start + hr->length )
	{ DEBUG(Dprintf(_T("Clicked link %ls\n"), hr->link));
	  return (*_rlc_link_hook)(b, hr->link);
	}
      }
    }
  }

  return false;
}

static const TCHAR *
rlc_over_link(RlcData b, int x, int y)
{ if ( _rlc_link_hook )
  { int l, c;

    rlc_translate_mouse(b, x, y, &l, &c);
    { TextLine tl = &b->lines[l];
      if ( c < tl->size )
      { text_char *chr = &tl->text[c];
	if ( TF_LINK(chr->flags) )
	{ DEBUG(Dprintf(_T("On link at %d,%d\n"), l, c));
	  for(href *hr=tl->links; hr; hr = hr->next)
	  { if ( c >= hr->start && c <= hr->start + hr->length )
	    { DEBUG(Dprintf(_T("  link: %d(%d) -> \"%ls\"\n"),
			    hr->start, hr->length, hr->link));
	      return hr->link;
	    }
	  }
	}
      }
    }
  }

  return NULL;
}

static int				/* v >= f && v <= t */
rlc_between(RlcData b, int f, int t, int v)
{ int h = rlc_count_lines(b, b->first, v);

  if ( h >= rlc_count_lines(b, b->first, f) &&
       h <= rlc_count_lines(b, b->first, t) )
    return true;

  return false;
}


static void
rlc_word_selection(RlcData b, int x, int y)
{ int l, c;

  rlc_translate_mouse(b, x, y, &l, &c);
  if ( rlc_between(b, b->first, b->last, l) )
  { TextLine tl = &b->lines[l];

    if ( c < tl->size && rlc_is_word_char(tl->text[c].code) )
    { int f, t;

      for(f=c; f>0 && rlc_is_word_char(tl->text[f-1].code); f--)
	;
      for(t=c; t<tl->size && rlc_is_word_char(tl->text[t].code); t++)
	;
      rlc_set_selection(b, l, f, l, t);
    }
  }

  b->sel_unit = SEL_WORD;
}


static void
rlc_extend_selection(RlcData b, int x, int y)
{ int l, c;
  int el = b->sel_org_line;
  int ec = b->sel_org_char;

  rlc_translate_mouse(b, x, y, &l, &c);
  if ( SelLT(l, c, b->sel_org_line, b->sel_org_char) )
  { if ( b->sel_unit == SEL_WORD )
    { if ( rlc_between(b, b->first, b->last, l) )
      { TextLine tl = &b->lines[l];

	if ( c < tl->size && rlc_is_word_char(tl->text[c].code) )
	  for(; c > 0 && rlc_is_word_char(tl->text[c-1].code); c--)
	    ;
      }
      if ( rlc_between(b, b->first, b->last, el) )
      { TextLine tl = &b->lines[el];

	if ( ec < tl->size && rlc_is_word_char(tl->text[ec].code) )
	  for(; ec < tl->size && rlc_is_word_char(tl->text[ec].code); ec++)
	    ;
      }
    } else if ( b->sel_unit == SEL_LINE )
      c = 0;
    rlc_set_selection(b, l, c, el, ec);
  } else if ( SelLT(b->sel_org_line, b->sel_org_char, l, c) )
  { if ( b->sel_unit == SEL_WORD )
    { if ( rlc_between(b, b->first, b->last, l) )
      { TextLine tl = &b->lines[l];

	if ( c < tl->size && rlc_is_word_char(tl->text[c].code) )
	  for(; c < tl->size && rlc_is_word_char(tl->text[c].code); c++)
	    ;
      }
      if ( rlc_between(b, b->first, b->last, el) )
      { TextLine tl = &b->lines[el];

	if ( ec < tl->size && rlc_is_word_char(tl->text[ec].code) )
	  for(; ec > 0 && rlc_is_word_char(tl->text[ec-1].code); ec--)
	    ;
      }
    } else if ( b->sel_unit == SEL_LINE )
      c = b->width;
    rlc_set_selection(b, el, ec, l, c);
  }
}


static TCHAR *
rlc_read_from_window(RlcData b, int sl, int sc, int el, int ec)
{ int bufsize = 256;
  TCHAR *buf;
  int i = 0;

  if ( el < sl || (el == sl && ec < sc) )
    return NULL;			/* invalid region */
  if ( !(buf = rlc_malloc(bufsize * sizeof(TCHAR))) )
    return NULL;			/* not enough memory */

  for( ; ; sc = 0, sl = NextLine(b, sl))
  { TextLine tl = &b->lines[sl];
    if ( tl )
    { int e = (sl == el ? ec : tl->size);

      if ( e > tl->size )
	e = tl->size;

      while(sc < e)
      { if ( i+1 >= bufsize )
	{ bufsize *= 2;
	  if ( !(buf = rlc_realloc(buf, bufsize * sizeof(TCHAR))) )
	    return NULL;		/* not enough memory */
	}
	buf[i++] = tl->text[sc++].code;
      }
    }

    if ( sl == el || sl == b->last )
    { buf[i++] = '\0';			/* Always room for the 0 */
      return buf;
    }

    if ( tl && !tl->softreturn )
    { if ( i+2 >= bufsize )
      { bufsize *= 2;
	if ( !(buf = rlc_realloc(buf, bufsize * sizeof(TCHAR))) )
	  return NULL;			/* not enough memory */
      }
      buf[i++] = '\r';			/* Bill ... */
      buf[i++] = '\n';
    }
  }
}


static int
rlc_has_selection(RlcData b)
{ if ( SelEQ(b->sel_start_line, b->sel_start_char,
	     b->sel_end_line,   b->sel_end_char) )
    return false;
  return true;
}


static TCHAR *
rlc_selection(RlcData b)
{ if ( rlc_has_selection(b) )
    return rlc_read_from_window(b,
				b->sel_start_line, b->sel_start_char,
				b->sel_end_line,   b->sel_end_char);
  return NULL;
}


static void
rlc_copy(RlcData b)
{ TCHAR *sel = rlc_selection(b);

  if ( sel && b->window )
  { size_t size = _tcslen(sel);
    HGLOBAL mem = GlobalAlloc(GMEM_MOVEABLE, (size + 1)*sizeof(TCHAR));
    TCHAR far *data;
    size_t i;

    if ( !mem )
    { MessageBox(NULL, _T("Not enough memory to copy"), _T("Error"), MB_OK);
      return;
    }
    data = GlobalLock(mem);

    for(i=0; i<size; i++)
      *data++ = sel[i];
    *data = '\0';

    GlobalUnlock(mem);
    OpenClipboard(b->window);
    EmptyClipboard();
#ifdef UNICODE
    SetClipboardData(CF_UNICODETEXT, mem);
#else
    SetClipboardData(CF_TEXT, mem);
#endif
    CloseClipboard();

    rlc_free(sel);
  }
}



		 /*******************************
		 *           REPAINT		*
		 *******************************/

static void
rlc_place_caret(RlcData b)
{ if ( b->has_focus && b->window )
  { int line = rlc_count_lines(b, b->window_start, b->caret_y);

    if ( line < b->window_size )
    { if ( b->fixedfont )
      { SetCaretPos((b->caret_x + 1) * b->cw, line * b->ch);
      } else
      { HDC hdc = GetDC(b->window);
	int tw;
	TextLine tl = &b->lines[b->caret_y];
	HFONT old;

	old = SelectObject(hdc, b->hfont);
	tw = text_width(b, hdc, tl->text, b->caret_x);
	SelectObject(hdc, old);
	ReleaseDC(b->window, hdc);

	SetCaretPos(b->cw + tw, line * b->ch);
      }
      if ( !b->caret_is_shown )
      { ShowCaret(b->window);
	b->caret_is_shown = true;

	return;
      }
    } else
    { if ( b->caret_is_shown == true )
      { HideCaret(b->window);
	b->caret_is_shown = false;
      }
    }
  }

  b->caret_is_shown = false;
}


static void
rlc_update_scrollbar(RlcData b)
{ if ( b->window )
  { int nsb_lines = rlc_count_lines(b, b->first, b->last);
    int nsb_start = rlc_count_lines(b, b->first, b->window_start);

    if ( nsb_lines != b->sb_lines ||
	 nsb_start != b->sb_start )
    { SetScrollRange(b->window, SB_VERT, 0, nsb_lines, false);
      SetScrollPos(  b->window, SB_VERT, nsb_start, true);

      b->sb_lines = nsb_lines;
      b->sb_start = nsb_start;
    }
  }
}


static void
rcl_paint_text(RlcData b, HDC hdc,
	       TextLine tl, int from, int to,
	       int ty, int *cx, int insel)
{ text_char *chars, *s;
  text_char buf[MAXLINE];
  TCHAR text[MAXLINE];
  TCHAR *t;
  int len = to-from;
  int i;

  if ( len <= 0 )
    return;

  if ( tl->text && to <= tl->size )
  { chars = &tl->text[from];
  } else
  { text_char *o;
    int copy;

    o = chars = buf;
    s = &tl->text[from];
    copy = tl->text ? tl->size-from : 0;
    for(i=0; i<copy; i++)
      *o++ = *s++;
    for(; i<len; i++, o++)
    { o->code = ' ';
      o->flags = TF_DEFAULT;
    }
  }

  for(t=text, s=chars, i=0; i < len; i++, t++, s++)
    *t = s->code;

  if ( insel )					/* TBD: Cache */
  { SetBkColor(hdc, b->sel_background);
    SetTextColor(hdc, b->sel_foreground);
    TextOut(hdc, *cx, ty, text, len);
    *cx += tchar_width(b, hdc, text, len);
  } else
  { int start, segment;

    for(start=0, s=chars, t=text;
	start<len;
	start+=segment, s+=segment, t+=segment)
    { text_flags flags = s->flags;
      int left = len-start;

      for(segment=0; s[segment].flags == flags && segment<left; segment++)
	;

      if ( TF_FG(flags) == ANSI_COLOR_DEFAULT )
	SetTextColor(hdc, b->foreground);
      else
	SetTextColor(hdc, b->ansi_color[TF_FG(flags)]);

      if ( TF_BG(flags) == ANSI_COLOR_DEFAULT )
	SetBkColor(hdc, b->background);
      else
	SetBkColor(hdc, b->ansi_color[TF_BG(flags)]);

      HFONT font = NULL, old_font = NULL;
      if ( TF_UNDERLINE(flags) )
      { if ( TF_BOLD(flags) )
	  font = b->hfont_bold_underlined;
	else
	  font = b->hfont_underlined;
      } else if ( TF_BOLD(flags) )
      { font = b->hfont_bold;
      }

      if ( font )
	old_font = (HFONT)SelectObject(hdc, font);

      TextOut(hdc, *cx, ty, t, segment);
      *cx += tchar_width(b, hdc, t, segment);

      if ( old_font )
	SelectObject(hdc, old_font);
    }
  }
}


static void
rlc_redraw(RlcData b)
{ PAINTSTRUCT ps;
  HDC hdc = BeginPaint(b->window, &ps);
  int sl = max(0, ps.rcPaint.top/b->ch);
  int el = min(b->window_size, ps.rcPaint.bottom/b->ch);
  int l = rlc_add_lines(b, b->window_start, sl);
  int pl = sl;				/* physical line */
  RECT rect;
  HBRUSH bg;
  int stockbg;
  int insel = false;			/* selected lines? */

  SelectObject(hdc, b->hfont);
  SetTextColor(hdc, b->foreground);
  SetBkColor(hdc, b->background);

  if ( b->background == RGB(255, 255, 255) )
  { bg = GetStockObject(WHITE_BRUSH);
    stockbg = true;
  } else
  { bg = CreateSolidBrush(b->background);
    stockbg = false;
  }

  if ( b->has_focus && b->caret_is_shown )
  { HideCaret(b->window);
    b->caret_is_shown = false;
  }

  if ( rlc_count_lines(b, b->first, b->sel_start_line) <
       rlc_count_lines(b, b->first, l) &&
       rlc_count_lines(b, b->first, b->sel_end_line) >=
       rlc_count_lines(b, b->first, l) )
    insel = true;

  if ( insel )
  { SetBkColor(hdc, b->sel_background);
    SetTextColor(hdc, b->sel_foreground);
  }

  for(; pl <= el; l = NextLine(b, l), pl++)
  { TextLine tl = &b->lines[l];
    int ty = b->ch * pl;
    int cx = b->cw;

    rect.top    = ty;
    rect.bottom = rect.top + b->ch;

					/* compute selection */
    if ( l == b->sel_start_line )
    { int cf = b->sel_start_char;
      int ce = (b->sel_end_line != b->sel_start_line ? b->width
						     : b->sel_end_char);

      rcl_paint_text(b, hdc, tl,  0, cf, ty, &cx, insel);
      insel = true;
      rcl_paint_text(b, hdc, tl, cf, ce, ty, &cx, insel);
      if ( l == b->sel_end_line )
      { insel = false;
	rcl_paint_text(b, hdc, tl, ce, b->width, ty, &cx, insel);
      } else
	insel = true;
    } else if ( l == b->sel_end_line )	/* end of selection */
    { int ce = b->sel_end_char;

      rcl_paint_text(b, hdc, tl, 0, ce, ty, &cx, insel);
      insel = false;
      rcl_paint_text(b, hdc, tl, ce, b->width, ty, &cx, insel);
    } else				/* entire line in/out selection */
    { rcl_paint_text(b, hdc, tl, 0, b->width, ty, &cx, insel);
    }

					/* clear remainder of line */
    if ( cx < b->width * (b->cw+1) )
    { rect.left   = cx;
      rect.right  = b->width * (b->cw+1);
      rect.top    = b->ch * pl;
      rect.bottom = rect.top + b->ch;
      FillRect(hdc, &rect, bg);
    }

    tl->changed = CHG_RESET;

    if ( l == b->last )			/* clear to end of window */
    { rect.left   = b->cw;
      rect.right  = b->width * (b->cw+1);
      rect.top    = b->ch * (pl+1);
      rect.bottom = b->ch * (el+1);
      FillRect(hdc, &rect, bg);

      break;
    }
  }
  rlc_place_caret(b);

  b->changed = CHG_RESET;
  if ( !stockbg )
    DeleteObject(bg);

  EndPaint(b->window, &ps);

  rlc_update_scrollbar(b);
}


static void
rlc_request_redraw(RlcData b)
{ if ( b->changed & CHG_CHANGED )
  { if ( b->window )
      InvalidateRect(b->window, NULL, false);
  } else
  { int i = b->window_start;
    int y = 0;
    RECT rect;
    int first = true;

    rect.left = b->cw;
    rect.right = (b->width+1) * b->cw;

    for(; y < b->window_size; y++, i = NextLine(b, i))
    { TextLine l = &b->lines[i];

      if ( l->changed & CHG_CHANGED )
      { if ( first )
	{ rect.top = y * b->ch;
	  rect.bottom = rect.top + b->ch;
	  first = false;
	} else
	  rect.bottom = (y+1) * b->ch;
      }
      if ( i == b->last )
	break;
    }

    if ( !first && b->window )
      InvalidateRect(b->window, &rect, false);
    else if ( b->changed & CHG_CARET )
      rlc_place_caret(b);
  }
}


static void
rlc_normalise(RlcData b)
{ if ( rlc_count_lines(b, b->window_start, b->caret_y) >= b->window_size )
  { b->window_start = rlc_add_lines(b, b->caret_y, -(b->window_size-1));
    b->changed |= CHG_CARET|CHG_CLEAR|CHG_CHANGED;
    rlc_request_redraw(b);
  }
}


static void
rlc_resize_pixel_units(RlcData b, int w, int h)
{ int nw = max(20, w/b->cw)-2;		/* 1 character space for margins */
  int nh = max(1, h/b->ch);

  DEBUG(Dprintf(_T("rlc_resize_pixel_units(%p, %d, %d) (%dx%d)\n"),
		b, w, h, nw, nh));

  if ( b->width == nw && b->window_size == nh )
    return;				/* no real change */

  rlc_resize(b, nw, nh);

  if ( _rlc_resize_hook )
  { (*_rlc_resize_hook)(b->width, b->window_size);
  } else
  {
#ifdef SIGWINCH
    raise(SIGWINCH);
#endif
  }

  rlc_request_redraw(b);
}

		 /*******************************
		 *	       FONT		*
		 *******************************/

static void
rlc_init_text_dimensions(RlcData b, HFONT font)
{ HDC hdc;
  TEXTMETRIC tm;

  if ( font )
  { b->hfont = font;
  } else if ( b->create_attributes )
  { rlc_console_attr *a = b->create_attributes;
    if ( !a->face_name[0] )
      b->hfont = GetStockObject(ANSI_FIXED_FONT);
    else
    { LOGFONT lfont;

      memset(&lfont, 0, sizeof(lfont));

      lfont.lfHeight          = a->font_size;
      lfont.lfWeight          = a->font_weight;
      lfont.lfPitchAndFamily  = a->font_family;
      lfont.lfCharSet	      = a->font_char_set;
      _tcsncpy(lfont.lfFaceName, a->face_name, 31);

      if ( !(b->hfont = CreateFontIndirect(&lfont)) )
	b->hfont = GetStockObject(ANSI_FIXED_FONT);

      lfont.lfUnderline = true;
      if ( !(b->hfont_underlined = CreateFontIndirect(&lfont)) )
	b->hfont_underlined = b->hfont;

      lfont.lfWeight = FW_BOLD;
      if ( !(b->hfont_bold_underlined = CreateFontIndirect(&lfont)) )
	b->hfont_bold_underlined = b->hfont;

      lfont.lfUnderline = false;
      if ( !(b->hfont_bold = CreateFontIndirect(&lfont)) )
	b->hfont_bold = b->hfont;
    }
  } else
    b->hfont = GetStockObject(ANSI_FIXED_FONT);

					/* test for fixed?*/
  hdc = GetDC(NULL);
  SelectObject(hdc, b->hfont);
  GetTextMetrics(hdc, &tm);
  b->cw = tm.tmAveCharWidth;
  b->cb = tm.tmHeight;
  b->ch = tm.tmHeight + tm.tmExternalLeading;
  b->fixedfont = (tm.tmPitchAndFamily & TMPF_FIXED_PITCH ? false : true);
  ReleaseDC(NULL, hdc);

  if ( b->window )
  { RECT rect;

    if ( b->has_focus == true )
    { CreateCaret(b->window, NULL, b->fixedfont ? b->cw : 3, b->ch-1);
      rlc_place_caret(b);
    }

    GetClientRect(b->window, &rect);
    rlc_resize_pixel_units(b, rect.right - rect.left, rect.bottom - rect.top);
  }
}


static int
text_width(RlcData b, HDC hdc, const text_char *text, int len)
{ if ( b->fixedfont )
  { return len * b->cw;
  } else
  { SIZE size;
    TCHAR tmp[MAXLINE];
    int i;

    for(i=0; i<len; i++)
      tmp[i] = text[i].code;

    GetTextExtentPoint32(hdc, tmp, len, &size);
    return size.cx;
  }
}


static int
tchar_width(RlcData b, HDC hdc, const TCHAR *text, int len)
{ if ( b->fixedfont )
  { return len * b->cw;
  } else
  { SIZE size;

    GetTextExtentPoint32(hdc, text, len, &size);
    return size.cx;
  }
}


static void
rlc_save_font_options(HFONT font, rlc_console_attr *attr)
{ if ( font == GetStockObject(ANSI_FIXED_FONT) )
  { attr->face_name[0] = '\0';
  } else
  { LOGFONT lf;

    if ( GetObject(font, sizeof(lf), &lf) )
    { memcpy(attr->face_name, lf.lfFaceName, sizeof(attr->face_name)-1);

      attr->font_family   = lf.lfPitchAndFamily;
      attr->font_size     = lf.lfHeight;
      attr->font_weight   = lf.lfWeight;
      attr->font_char_set = lf.lfCharSet;
    }
  }
}


		 /*******************************
		 *	   FONT SELECTION	*
		 *******************************/

static void
rlc_queryfont(RlcData b)
{ CHOOSEFONT cf;
  LOGFONT lf;

  memset(&cf, 0, sizeof(cf));
  memset(&lf, 0, sizeof(lf));

  lf.lfHeight          = 16;
  lf.lfWeight          = FW_NORMAL;
  lf.lfPitchAndFamily  = FIXED_PITCH|FF_MODERN;

  cf.lStructSize = sizeof(cf);
  cf.hwndOwner   = b->window;
  cf.lpLogFont   = &lf;
  cf.Flags       = CF_SCREENFONTS|
		   CF_NOVERTFONTS|
		   CF_NOSIMULATIONS|
		   CF_FORCEFONTEXIST|
		   CF_INITTOLOGFONTSTRUCT;
  cf.nFontType   = SCREEN_FONTTYPE;

  if ( ChooseFont(&cf) )
  { HFONT f;
    if ( (f = CreateFontIndirect(&lf)) )
    { rlc_init_text_dimensions(b, f);

      InvalidateRect(b->window, NULL, true);
    }
  }
}



		 /*******************************
		 *     BUFFER INITIALISATION	*
		 *******************************/

static RlcData
rlc_make_buffer(int w, int h)
{ RlcData b = rlc_malloc(sizeof(rlc_data));
  int i;

  memset(b, 0, sizeof(*b));
  b->magic = RLC_MAGIC;

  b->height         = h;
  b->width          = w;
  b->window_size    = 25;
  b->lines          = rlc_malloc(sizeof(text_line) * h);
  b->cmdstat	    = CMD_INITIAL;
  b->changed	    = CHG_CARET|CHG_CHANGED|CHG_CLEAR;
  b->imode	    = IMODE_COOKED;	/* switch on first rlc_read() call */
  b->imodeswitch    = false;
  b->lhead	    = NULL;
  b->ltail	    = NULL;
  b->cursor         = LoadCursor(NULL, IDC_IBEAM);
  b->link_cursor    = LoadCursor(NULL, IDC_HAND);
  InitializeCriticalSection(&b->lock);

  memset(b->lines, 0, sizeof(text_line) * h);
  for(i=0; i<h; i++)
    b->lines[i].adjusted = true;

  rlc_init_word_chars();

  return b;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Copy all lines one `back' (i.e.  towards   older  lines).  If the oldest
(first) line is adjacent to the last, throw it away.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
rlc_shift_lines_down(RlcData b, int line)
{ int i = b->first;
  int p = PrevLine(b, i);

  if ( p != b->last )			/* move first (oldest line) */
  { b->first = p;
    b->lines[p] = b->lines[i];
  } else				/* delete first (oldest) line */
    rlc_free_line(b, b->first);
					/* copy the lines */
  for(p=i, i = NextLine(b, i); p != line; p=i, i = NextLine(b, i))
  { b->lines[p] = b->lines[i];
    b->lines[p].line_no = p;
  }

  b->lines[line].text       = NULL;	/* make this one `free' */
  b->lines[line].links      = NULL;
  b->lines[line].size       = 0;
  b->lines[line].adjusted   = true;
  b->lines[line].softreturn = false;
}


static void
rlc_shift_lines_up(RlcData b, int line)
{ int prev = PrevLine(b, line);

  while(line != b->first)
  { b->lines[line] = b->lines[prev];
    b->lines[line].line_no = line;
    line = prev;
    prev = PrevLine(b, prev);
  }

  rlc_reinit_line(b, b->first);
  b->first = NextLine(b, b->first);
}

static void
unlink_href(TextLine from, href *hr)
{ for(href **hp = &from->links; *hp; hp = &(*hp)->next)
  { if ( *hp == hr )
    { *hp = hr->next;
      return;
    }
  }
  assert(0);
}

static void
move_href(href *hr, TextLine from, TextLine to)
{ unlink_href(from, hr);
  hr->next = to->links;
  to->links = hr;
}

/* Update  links after  `moved` chars  were moved  from `l1`  to `l2`.
 * `l1` has all the links, `l2` has none as it is a fresh line.
 */
static void
update_links(TextLine l1, TextLine l2, int moved)
{ href *next;

  Dprint_links(l1, _T("l1"));
  Dprint_links(l2, _T("l2"));
  for(href *hr = l1->links; hr; hr=next)
  { next = hr->next;

    if ( hr->start > l1->size )	/* move completely */
    { move_href(hr, l1, l2);
      hr->start -= l1->size;
    } else if ( hr->start + hr->length > l1->size )
    { rlc_add_link(l2, hr->link, 0, hr->start + hr->length - l1->size);
      hr->length = l1->size - hr->start;
    }
  }
  Dprint_links(l1, _T("l1 (after)"));
  Dprint_links(l2, _T("l2 (after)"));

  rcl_check_links(l1);
  rcl_check_links(l2);
}

static void
move_link_positions(TextLine tl, int offset)
{ for(href *hr = tl->links; hr; hr=hr->next)
    hr->start += offset;
}

static void
move_links(TextLine from, TextLine to)
{ href *next;

  Dprintf(_T("Move links from %p to %p\n"), from, to);
  Dprint_links(from, _T("On from"));
  Dprint_links(to, _T("On to"));
  for(href *hr = from->links; hr; hr=next)
  { next = hr->next;
    unlink_href(from, hr);
    for(href *hr2 = to->links; hr2; hr2=hr2->next)
    { if ( hr->start+hr->length == hr2->start &&
	   wcscmp(hr->link, hr2->link) == 0 )
      {	Dprintf(_T("Rejoin split link\n"));
	hr2->start = hr->start;
	hr2->length += hr->length;
	rlc_free_link(hr);
	goto next_link;
      }
    }
    Dprintf(_T("Moved %p\n"), hr);
    hr->next = to->links;
    to->links = hr;
  next_link:
    ;
  }

  Dprint_links(to, _T("After move"));
}

static void
move_links_soft(TextLine from, TextLine to)
{ href *next;

  Dprintf(_T("Move links from %p to %p\n"), from, to);
  Dprint_links(from, _T("On from"));
  Dprint_links(to, _T("On to"));
  for(href *hr = from->links; hr; hr=next)
  { next = hr->next;
    if ( hr->start >= from->size )
    { unlink_href(from, hr);
      hr->start -= from->size;
      for(href *hr2 = to->links; hr2; hr2=hr2->next)
      { if ( hr->start+hr->length == hr2->start &&
	     wcscmp(hr->link, hr2->link) == 0 )
	{ Dprintf(_T("Rejoin split link\n"));
	  hr2->start = hr->start;
	  hr2->length += hr->length;
	  rlc_free_link(hr);
	  goto next_link;
	}
      }
      Dprintf(_T("Moved %p\n"), hr);
      hr->next = to->links;
      to->links = hr;
    } else if ( hr->start + hr->length > from->size )
    { rlc_add_link(to, hr->link, 0, hr->start + hr->length - from->size);
      hr->length = from->size - hr->start;
    }
  next_link:
    ;
  }

  Dprint_links(from, _T("From after move"));
  Dprint_links(to, _T("To after move"));
}


static void
rlc_resize(RlcData b, int w, int h)
{ int i;

  if ( b->width == w && b->window_size == h )
    return;				/* no real change */

  DEBUG(Dprintf(_T("Resizing %dx%d --> %dx%d\n"),
		b->width, b->window_size, w, h));

  b->window_size = h;
  b->width = w;

  for(i = b->first; /*i != b->last*/; i = NextLine(b, i))
  { TextLine tl = &b->lines[i];

    if ( tl->text && tl->adjusted == false )
      rlc_adjust_line(b, i);

    Dprintf(_T("%03d: sz=%d %s\n"), i, tl->size,
	    tl->softreturn ? "(soft)" : "");
    if ( tl->size > w )
    { Dprintf(_T("  Truncate\n"));
      if ( !tl->softreturn )		/* hard --> soft */
      { Dprintf(_T("    hard -> soft\n"));
	Dprint_lines(b, i, i);
	rlc_shift_lines_down(b, i);
	DEBUG(Dprintf(_T("b->first = %d, b->last = %d\n"), b->first, b->last));
	TextLine pl = &b->lines[PrevLine(b, i)]; /* this is the moved line */
	int moved = pl->size - w;
	tl->text = rlc_malloc(moved*sizeof(text_char));
	memmove(tl->text, &pl->text[w], moved*sizeof(text_char));
	DEBUG(Dprintf(_T("Copied %d chars from line %d to %d\n"),
		      moved, pl - b->lines, i));
	tl->size = moved;
	tl->adjusted = true;
	tl->softreturn = false;
	pl->softreturn = true;
	pl->text = rlc_realloc(pl->text, w * sizeof(text_char));
	pl->size = w;
	pl->adjusted = true;
	i = (int)(pl - b->lines);
	update_links(pl, tl, moved);
//	DEBUG(Dprint_lines(b, b->first, b->last));
      } else				/* put in next line */
      { TextLine nl;
	int move = tl->size - w;

	Dprintf(_T("    soft\n"));
	if ( i == b->last )
	  rlc_add_line(b);
	nl = &b->lines[NextLine(b, i)];
	nl->text = rlc_realloc(nl->text, (nl->size + move)*sizeof(text_char));
	memmove(&nl->text[move], nl->text, nl->size*sizeof(text_char));
	memmove(nl->text, &tl->text[w], move*sizeof(text_char));
	nl->size += move;
	tl->size = w;
	move_link_positions(nl, move);
	move_links_soft(tl, nl);
      }
    } else if ( tl->text && tl->softreturn && tl->size < w )
    { TextLine nl;

      Dprintf(_T("  Merge\n"));
      if ( i == b->last )
	rlc_add_line(b);
      nl = &b->lines[NextLine(b, i)];

      nl->text = rlc_realloc(nl->text,
			     (nl->size + tl->size)*sizeof(text_char));
      memmove(&nl->text[tl->size], nl->text, nl->size*sizeof(text_char));
      move_link_positions(nl, tl->size);
      memmove(nl->text, tl->text, tl->size*sizeof(text_char));
      move_links(tl, nl);

      nl->size += tl->size;
      nl->adjusted = true;
      rcl_check_links(nl);

      rlc_shift_lines_up(b, i);
    }

    if ( i == b->last )
      break;
  }

  for(i = NextLine(b, i); i != b->first; i = NextLine(b, i))
    rlc_free_line(b, i);

  if ( rlc_count_lines(b, b->first, b->last) < h )
    b->window_start = b->first;
  else
    b->window_start = rlc_add_lines(b, b->last, -(h-1));

  b->caret_y = b->last;
  b->caret_x = b->lines[b->last].size;

  b->changed |= CHG_CARET|CHG_CHANGED|CHG_CLEAR;

  rlc_check_assertions(b);
}


static void
rlc_reinit_line(RlcData b, int line)
{ TextLine tl = &b->lines[line];

  tl->text	 = NULL;
  tl->links      = NULL;
  tl->adjusted   = false;
  tl->size       = 0;
  tl->softreturn = false;
}

static void
rlc_free_link(href *hr)
{ rlc_free(hr->link);
  rlc_free(hr);
}

static void
rlc_free_links(href *links)
{ href *next;

  for(; links; links=next)
  { next = links->next;
    rlc_free_link(links);
  }
}

static void
rlc_free_line(RlcData b, int line)
{ TextLine tl = &b->lines[line];
  if ( tl->text )
  { rlc_free(tl->text);
    rlc_reinit_line(b, line);
  }
  href *links = tl->links;
  if ( links )
  { tl->links = NULL;
    rlc_free_links(links);
  }
}


static void
rlc_adjust_line(RlcData b, int line)
{ TextLine tl = &b->lines[line];

  if ( tl->text && !tl->adjusted )
  { tl->text = rlc_realloc(tl->text, tl->size == 0
				? sizeof(text_char)
				: tl->size * sizeof(text_char));
    tl->adjusted = true;
  }
}


static void
rlc_unadjust_line(RlcData b, int line)
{ TextLine tl = &b->lines[line];

  if ( tl->text )
  { if ( tl->adjusted )
    { tl->text = rlc_realloc(tl->text, (b->width + 1)*sizeof(text_char));
      tl->adjusted = false;
    }
  } else
  { tl->text = rlc_malloc((b->width + 1)*sizeof(text_char));
    tl->adjusted = false;
    tl->size = 0;
  }
}


static void
rlc_open_line(RlcData b)
{ int i = b->last;

  if ( i == b->sel_start_line )
    rlc_set_selection(b, 0, 0, 0, 0);	/* clear the selection */
  if ( i == b->first )
  { rlc_free_line(b, b->first);
    b->first = NextLine(b, b->first);
  }

  b->lines[i].text       = rlc_malloc((b->width + 1)*sizeof(text_char));
  b->lines[i].adjusted   = false;
  b->lines[i].size       = 0;
  b->lines[i].softreturn = false;
  b->lines[i].line_no    = i;
}


static void
rlc_add_line(RlcData b)
{ b->last = NextLine(b, b->last);
  rlc_open_line(b);
}

		 /*******************************
		 *	   CALCULATIONS		*
		 *******************************/

static int
rlc_count_lines(RlcData b, int from, int to)
{ if ( to >= from )
    return to-from;

  return to + b->height - from;
}


static int
rlc_add_lines(RlcData b, int here, int add)
{ here += add;
  while ( here < 0 )
    here += b->height;
  while ( here >= b->height )
    here -= b->height;

  return here;
}


		 /*******************************
		 *    ANSI SEQUENCE HANDLING	*
		 *******************************/

static void
rlc_need_arg(RlcData b, int arg, int def)
{ if ( b->argc < arg )
  { b->argv[arg-1] = def;
    b->argc = arg;
  }
}


static void
rlc_caret_up(RlcData b, int arg)
{ while(arg-- > 0 && b->caret_y != b->first)
    b->caret_y = PrevLine(b, b->caret_y);

  b->changed |= CHG_CARET;
}


static void
rlc_caret_down(RlcData b, int arg)
{ while ( arg-- > 0 )
  { if ( b->caret_y == b->last )
      rlc_add_line(b);
    b->caret_y = NextLine(b, b->caret_y);
    b->lines[b->caret_y].softreturn = false; /* ? why not only on open? */
  }
  b->changed |= CHG_CARET;
					/* scroll? */
  if ( rlc_count_lines(b, b->window_start, b->caret_y) >= b->window_size )
  { b->window_start = rlc_add_lines(b, b->caret_y, -(b->window_size-1));
    b->changed |= CHG_CHANGED|CHG_CLEAR;
  }

  rlc_check_assertions(b);
}


static void
rlc_caret_forward(RlcData b, int arg)
{ while(arg-- > 0)
  { if ( ++b->caret_x >= b->width )
    { b->lines[b->caret_y].softreturn = true;
      b->caret_x = 0;
      rlc_caret_down(b, 1);
    }
  }

  b->changed |= CHG_CARET;
}


static void
rlc_caret_backward(RlcData b, int arg)
{ while(arg-- > 0)
  { if ( b->caret_x-- == 0 )
    { rlc_caret_up(b, 1);
      b->caret_x = b->width-1;
    }
  }

  b->changed |= CHG_CARET;
}


static void
rlc_cariage_return(RlcData b)
{ b->caret_x = 0;

  b->changed |= CHG_CARET;
}


static void
rlc_tab(RlcData b)
{ TextLine tl = &b->lines[b->caret_y];

  do
  { rlc_caret_forward(b, 1);
  } while( (b->caret_x % 8) != 0 );

  if ( tl->size < b->caret_x )
  { rlc_unadjust_line(b, b->caret_y);

    while ( tl->size < b->caret_x )
    { text_char *tc = &tl->text[tl->size++];

      tc->code = ' ';
      tc->flags = b->sgr_flags;
    }
  }

  b->changed |= CHG_CARET;
}


static void
rlc_set_caret(RlcData b, int x, int y)
{ int cy = rlc_count_lines(b, b->window_start, b->caret_y);

  y = Bounds(y, 0, b->window_size);

  if ( y < cy )
    b->caret_y = rlc_add_lines(b, b->window_start, y);
  else
    rlc_caret_down(b, y-cy);

  b->caret_x = Bounds(x, 0, b->width-1);

  b->changed |= CHG_CARET;
}


static void
rlc_save_caret_position(RlcData b)
{ b->scaret_y = rlc_count_lines(b, b->window_start, b->caret_y);
  b->scaret_x = b->caret_x;
}


static void
rlc_restore_caret_position(RlcData b)
{ rlc_set_caret(b, b->scaret_x, b->scaret_y);
}


static void
rlc_erase_saved_lines(RlcData b)
{ b->first = b->window_start;
}

static void
rlc_erase_display(RlcData b)
{ TextLine tl = &b->lines[b->window_start];

  tl->size = 0;
  b->last = b->window_start;
  b->changed |= CHG_CHANGED|CHG_CLEAR|CHG_CARET;

  rlc_set_caret(b, 0, 0);
}


static void
rlc_erase_line(RlcData b)
{ TextLine tl = &b->lines[b->caret_y];

  tl->size = b->caret_x;
  tl->changed |= CHG_CHANGED|CHG_CLEAR;
}


static void
rlc_sgr(RlcData b, int sgr)
{ if ( sgr == 0 )
  { b->sgr_flags = TF_DEFAULT;
  } else if ( sgr >= 30 && sgr <= 39 )
  { b->sgr_flags = TF_SET_FG(b->sgr_flags,
			     sgr == 39 ? ANSI_COLOR_DEFAULT : sgr-30);
  } else if ( sgr >= 40 && sgr <= 49 )
  { b->sgr_flags = TF_SET_BG(b->sgr_flags,
			     sgr == 49 ? ANSI_COLOR_DEFAULT : sgr-40);
  } else if ( sgr >= 90 && sgr <= 99 )
  { b->sgr_flags = TF_SET_FG(b->sgr_flags,
			     sgr == 99 ? ANSI_COLOR_DEFAULT : sgr-90+8);
  } else if ( sgr >= 100 && sgr <= 109 )
  { b->sgr_flags = TF_SET_BG(b->sgr_flags,
			     sgr == 109 ? ANSI_COLOR_DEFAULT : sgr-100+8);
  } else if ( sgr == 1 )
  { b->sgr_flags = TF_SET_BOLD(b->sgr_flags, 1);
  } else if ( sgr == 4 )
  { b->sgr_flags = TF_SET_UNDERLINE(b->sgr_flags, 1);
  }
}


static void
rlc_put(RlcData b, int chr)
{ TextLine tl = &b->lines[b->caret_y];
  text_char *tc;

  rlc_unadjust_line(b, b->caret_y);
  while( tl->size < b->caret_x )
  { tc = &tl->text[tl->size++];

    tc->code  = ' ';
    tc->flags = b->sgr_flags;
  }
  tc = &tl->text[b->caret_x];
  tc->code = chr;
  tc->flags = b->sgr_flags;
  if ( tl->size <= b->caret_x )
    tl->size = b->caret_x + 1;
  tl->changed |= CHG_CHANGED;

  rlc_caret_forward(b, 1);
}

static void
rcl_check_links(TextLine tl)
{
#if _DEBUG
  int links = 0;

  for(int x=0; x<tl->size; x++)
  { if ( TF_LINK(tl->text[x].flags) )
    { int start = x;
      links++;
      for(; x<tl->size && TF_LINK(tl->text[x].flags); x++)
	;
      int len = x-start;
      href *hr;
      for(hr = tl->links; hr; hr=hr->next)
      { if ( hr->start == start && hr->length == len )
	  break;
      }
      if ( !hr )
      { Dprintf(_T("CHECK: %03d could not find href for %d(%d); got: "),
		tl->line_no, start, len);
	for(hr = tl->links; hr; hr=hr->next)
	  Dprintf(_T(" %d(%d)"), hr->start, hr->length);
	Dprintf(_T("\n"));
      }
    }
  }

  int refs = 0;
  for(href *hr = tl->links; hr; hr=hr->next)
    refs++;
  if ( refs != links )
  { Dprintf(_T("CHECK: %03d found %d links; expected %d\n"),
	    tl->line_no, links, refs);
    Dprint_line(tl, true);
  }
#endif
}

static href *
rlc_add_link(TextLine tl, const TCHAR *link, int start, int len)
{ href *hr = rlc_malloc(sizeof(*hr));

  hr->link = rlc_malloc((wcslen(link)+1)*sizeof(*link));
  wcscpy(hr->link, link);
  hr->start = start;
  hr->length = len;
  hr->next = tl->links;
  tl->links = hr;
  return hr;
}

static href *
rlc_register_link(RlcData b, const TCHAR *link, int len)
{ TextLine tl = &b->lines[b->caret_y];
  return rlc_add_link(tl, link, b->caret_x, len);
}

static void
rlc_put_link(RlcData b, const TCHAR *label, const TCHAR *link)
{ text_flags flags0 = b->sgr_flags;
  rlc_sgr(b, 34);	/* blue */
  rlc_sgr(b, 4);	/* underline */
  int y = b->caret_y;
  href *hr = rlc_register_link(b, link, wcslen(label));
  b->sgr_flags = TF_SET_LINK(b->sgr_flags, true);
  for( ; *label; label++)
  { rlc_put(b, *label);
    if ( b->caret_y != y )	/* moved to next line */
    { size_t left = wcslen(label)-1;
      hr->length -= left;
      rcl_check_links(&b->lines[y]);
      hr = rlc_register_link(b, link, left);
      y = b->caret_y;
    }
  }
  rcl_check_links(&b->lines[y]);
  b->sgr_flags = flags0;
}

#ifdef _DEBUG
#define CMD(c) do {cmd = _T(#c); c;} while(0)
#else
#define CMD(c) do {c;} while(0)
#endif

static void
rlc_putansi(RlcData b, int chr)
{
#ifdef _DEBUG
  TCHAR *cmd;
#endif

  switch(b->cmdstat)
  { case CMD_INITIAL:
      switch(chr)
      { case '\b':
	  CMD(rlc_caret_backward(b, 1));
	  break;
        case Control('G'):
	  MessageBeep(MB_ICONEXCLAMATION);
	  break;
	case '\r':
	  CMD(rlc_cariage_return(b));
	  break;
	case '\n':
	  CMD(rlc_caret_down(b, 1));
	  break;
	case '\t':
	  CMD(rlc_tab(b));
	  break;
	case 27:			/* ESC */
	  b->cmdstat = CMD_ESC;
	  break;
	default:
	  CMD(rlc_put(b, chr));
	  break;
      }
      break;
    case CMD_ESC:
      switch(chr)
      { case '[':
	  b->cmdstat = CMD_ANSI;
	  b->argc    = 0;
	  b->argstat = 0;		/* no arg */
	  break;
	case ']':
	  b->cmdstat = CMD_LINK;
	  b->must_see = "8;;";
	  break;
	default:
	  b->cmdstat = CMD_INITIAL;
	  break;
      }
      break;
    case CMD_LINK:
      if ( b->must_see && b->must_see[0] == chr )
      { b->must_see++;
	if ( !b->must_see[0] )
	{ b->cmdstat = CMD_LINKARG;
	  b->link_len = 0;
	  b->must_see = NULL;
	}
	break;
      } else
      { b->cmdstat = CMD_INITIAL;
	break;
      }
    case CMD_LINKARG:
      if ( b->link_len < ANSI_MAX_LINK-1 )
      { const TCHAR *end = _T("\e]8;;\e\\");
	const TCHAR *sep = _T("\e\\");
	const size_t endl = wcslen(end);
	const size_t sepl = wcslen(sep);
	b->link[b->link_len++] = chr;
	b->link[b->link_len] = 0;
	if ( chr == '\\' &&
	     b->link_len >= endl &&
	     memcmp(end, &b->link[b->link_len-endl],
		    endl*sizeof(end[0])) == 0 )
	{ b->link_len -= endl;
	  b->cmdstat = CMD_INITIAL;
	  b->link[b->link_len] = 0;
	  TCHAR *label = wcsstr(b->link, sep);
	  TCHAR *link;
	  if ( label )
	  { *label = 0;
	    label += sepl;
	    link = b->link;
	  } else
	  { label = b->link;
	    link = NULL;
	  }
	  DEBUG(Dprintf(_T("Link: \"%ls\", Label \"%ls\"\n"), link, label));
	  if ( link )
	  { rlc_put_link(b, label, link);
	  } else
	  { for( ; *label; label++)
	      rlc_put(b, *label);
	  }
	}
	break;
      } else			/* too long; process as text */
      { b->cmdstat = CMD_INITIAL;
	b->link[b->link_len] = 0;
	for(TCHAR *split=b->link; *split; split++)
	  rlc_put(b, *split);
	break;
      }
    case CMD_ANSI:			/* ESC [ */
      if ( _istdigit((wint_t)chr) )
      { if ( !b->argstat )
	{ b->argv[b->argc] = (chr - '0');
	  b->argstat = 1;		/* positive */
	} else
	{ b->argv[b->argc] = b->argv[b->argc] * 10 + (chr - '0');
	}

	break;
      }
      if ( !b->argstat && chr == '-' )
      { b->argstat = -1;		/* negative */
	break;
      }
      if ( b->argstat )
      { b->argv[b->argc] *= b->argstat;
	if ( b->argc < (ANSI_MAX_ARGC-1) )
	  b->argc++;			/* silently discard more of them */
	b->argstat = 0;
      }
      switch(chr)
      { case ';':
	  return;			/* wait for more args */
	case 'H':
	case 'f':
	  rlc_need_arg(b, 1, 0);
	  rlc_need_arg(b, 2, 0);
	  CMD(rlc_set_caret(b, b->argv[0], b->argv[1]));
	  break;
	case 'A':
	  rlc_need_arg(b, 1, 1);
	  CMD(rlc_caret_up(b, b->argv[0]));
	  break;
	case 'B':
	  rlc_need_arg(b, 1, 1);
	  CMD(rlc_caret_down(b, b->argv[0]));
	  break;
	case 'C':
	  rlc_need_arg(b, 1, 1);
	  CMD(rlc_caret_forward(b, b->argv[0]));
	  break;
	case 'D':
	  rlc_need_arg(b, 1, 1);
	  CMD(rlc_caret_backward(b, b->argv[0]));
	  break;
	case 's':
	  CMD(rlc_save_caret_position(b));
	  break;
	case 'u':
	  CMD(rlc_restore_caret_position(b));
	  break;
	case 'J':
	  if ( b->argv[0] == 2 )
	    CMD(rlc_erase_display(b));
	  else if ( b->argv[0] == 3 )
	    CMD(rlc_erase_saved_lines(b));
	  break;
	case 'K':
	  CMD(rlc_erase_line(b));
	  break;
	case 'm':
	  { int i;
	    rlc_need_arg(b, 1, 0);

	    for(i=0; i<b->argc; i++)
	      CMD(rlc_sgr(b, b->argv[i]));
	    break;
	  }
      }
      b->cmdstat = CMD_INITIAL;
  }

  rlc_check_assertions(b);
#ifdef _DEBUG
  (void)cmd;
#endif
}


		 /*******************************
		 *	      CUT/PASTE		*
		 *******************************/

wchar_t *
rlc_clipboard_text(rlc_console c)
{ RlcData b = rlc_get_data(c);

  if ( OpenClipboard(b->window) )
  { wchar_t *str = NULL;
    HGLOBAL mem;

    if ( (mem = GetClipboardData(CF_UNICODETEXT)) )
    { wchar_t *data = GlobalLock(mem);
      int o = 0;

      str = rlc_malloc(sizeof(*str)*(wcslen(data)+1));
      for(int i=0; data[i]; i++)
      { str[o++] = data[i];
	if ( data[i] == '\r' && data[i+1] == '\n' )
	  i++;
      }
      str[o] = EOS;

      GlobalUnlock(mem);
    }
    CloseClipboard();
    return str;
  }

  return NULL;
}

static void
rlc_paste(RlcData b)
{ RlcQueue q = b->queue;
  wchar_t *text = NULL;

  if ( b->window && q && (text=rlc_clipboard_text(b)) )
  { for(int i=0; text[i]; i++)
      rlc_add_queue(b, q, text[i]);
    rlc_free(text);
  }
}

		 /*******************************
		 *	LINE-READ SUPPORT	*
		 *******************************/

void
rlc_get_mark(rlc_console c, RlcMark m)
{ RlcData b = rlc_get_data(c);

  m->mark_x = b->caret_x;
  m->mark_y = b->caret_y;
}


void
rlc_goto_mark(rlc_console c, RlcMark m, const TCHAR *data, size_t offset)
{ RlcData b = rlc_get_data(c);

  b->caret_x = m->mark_x;
  b->caret_y = m->mark_y;

  for( ; offset-- > 0; data++ )
  { switch(*data)
    { case '\t':
	rlc_tab(b);
	break;
      case '\n':
	b->caret_x = 0;
        rlc_caret_down(b, 1);
	break;
      default:
	rlc_caret_forward(b, 1);
    }
  }
}


void
rlc_erase_from_caret(rlc_console c)
{ RlcData b = rlc_get_data(c);
  int i = b->caret_y;
  int x = b->caret_x;
  int last = rlc_add_lines(b, b->window_start, b->window_size);

  do
  { TextLine tl = &b->lines[i];

    if ( tl->size != x )
    { tl->size = x;
      tl->changed |= CHG_CHANGED|CHG_CLEAR;
    }

    i = NextLine(b, i);
    x = 0;
  } while ( i != last );
}


void
rlc_putchar(rlc_console c, int chr)
{ RlcData b = rlc_get_data(c);

  rlc_putansi(b, chr);
}


TCHAR *
rlc_read_screen(rlc_console c, RlcMark f, RlcMark t)
{ RlcData b = rlc_get_data(c);
  TCHAR *buf;

  buf = rlc_read_from_window(b, f->mark_y, f->mark_x, t->mark_y, t->mark_x);

  return buf;
}


void
rlc_update(rlc_console c)
{ RlcData b = rlc_get_data(c);

  if ( b->window )
  { rlc_normalise(b);
    rlc_request_redraw(b);
    UpdateWindow(b->window);
  }
}

		 /*******************************
		 *	  UPDATE THREAD		*
		 *******************************/

DWORD WINAPI
window_loop(LPVOID arg)
{ RlcData b = (RlcData) arg;

  rlc_create_window(b);
					/* if we do not do this, all windows */
					/* created by Prolog (XPCE) will be */
					/* in the background and inactive! */
  if ( !AttachThreadInput(b->application_thread_id,
			  b->console_thread_id, true) )
    rlc_putansi(b, '!');

  PostThreadMessage(b->application_thread_id, WM_RLC_READY, 0, 0);

  while(!b->closing)
  { switch( b->imode )
    { case IMODE_COOKED:
      { TCHAR *line = read_line(b);

	if ( line != RL_CANCELED_CHARP )
	{ LQueued lq = rlc_malloc(sizeof(lqueued));

	  lq->next = NULL;
	  lq->line = line;

	  if ( b->ltail )
	  { b->ltail->next = lq;
	    b->ltail = lq;
	  } else
	  { b->lhead = b->ltail = lq;
					      /* awake main thread */
	    PostThreadMessage(b->application_thread_id, WM_RLC_INPUT, 0, 0);
	  }
	}

	break;
      }
      case IMODE_RAW:
      { MSG msg;

	if ( rlc_get_message(&msg, NULL, 0, 0) )
	{ TranslateMessage(&msg);
	  DispatchMessage(&msg);
	  rlc_flush_output(b);
	} else
	  goto out;

	if ( b->imodeswitch )
	{ b->imodeswitch = false;
	}
      }
    }
  }

  if ( b->closing <= 2 )
  { MSG msg;
    TCHAR *waiting = _T("\r\nWaiting for Prolog. ")
		     _T("Close again to force termination ..");

    rlc_write(b, waiting, _tcslen(waiting));

    while ( b->closing <= 2 && rlc_get_message(&msg, NULL, 0, 0) )
    { TranslateMessage(&msg);
      DispatchMessage(&msg);
      rlc_flush_output(b);
    }
  }

out:
{ DWORD appthread = b->application_thread_id;
  rlc_destroy(b);

  PostThreadMessage(appthread, WM_RLC_READY, 0, 0);
}
  return 0;
}


		 /*******************************
		 *	  WATCOM/DOS I/O	*
		 *******************************/

int
getch(rlc_console c)
{ RlcData b = rlc_get_data(c);
  RlcQueue q = b->queue;
  int fromcon = (GetCurrentThreadId() == b->console_thread_id);

  while( rlc_is_empty_queue(q) )
  { if ( q->flags & RLC_EOF )
      return EOF;

    if ( !fromcon )
    { MSG msg;

      if ( rlc_get_message(&msg, NULL, 0, 0) )
      { TranslateMessage(&msg);
	DispatchMessage(&msg);
      } else
	return EOF;
    } else
    { rlc_dispatch(b);
      if ( b->imodeswitch )
      { b->imodeswitch = false;
	return IMODE_SWITCH_CHAR;
      }
    }
  }

  return rlc_from_queue(q);
}


int
getche(rlc_console c)
{ RlcData b = rlc_get_data(c);
  int chr = getch(b);

  rlc_putansi(b, chr);
  return chr;
}


		 /*******************************
		 *        GO32 FUNCTIONS	*
		 *******************************/

int
getkey(rlc_console con)
{ int c;
  RlcData b = rlc_get_data(con);
  int fromcon = (GetCurrentThreadId() == b->console_thread_id);

  if ( !fromcon && b->imode != IMODE_RAW )
  { int old = b->imode;

    b->imode = IMODE_RAW;
    b->imodeswitch = true;
    c = getch(b);
    b->imode = old;
    b->imodeswitch = true;
  } else
    c = getch(b);

  return c;
}


int
kbhit(rlc_console c)
{ RlcData b = rlc_get_data(c);

  return !rlc_is_empty_queue(b->queue);
}


void
ScreenGetCursor(rlc_console c, int *row, int *col)
{ RlcData b = rlc_get_data(c);

  *row = rlc_count_lines(b, b->window_start, b->caret_y) + 1;
  *col = b->caret_x + 1;
}


void
ScreenSetCursor(rlc_console c, int row, int col)
{ RlcData b = rlc_get_data(c);

  rlc_set_caret(b, col-1, row-1);
}


int
ScreenCols(rlc_console c)
{ RlcData b = rlc_get_data(c);

  return b->width;
}


int
ScreenRows(rlc_console c)
{ RlcData b = rlc_get_data(c);

  return b->window_size;
}

		 /*******************************
		 *	      QUEUE		*
		 *******************************/

#define QN(q, i) ((i)+1 >= (q)->size ? 0 : (i)+1)
#define QP(q, i) ((i)-1 < 0 ? (q)->size-1 : (i)-1)


RlcQueue
rlc_make_queue(int size)
{ RlcQueue q;

  if ( (q = rlc_malloc(sizeof(rlc_queue))) )
  { q->first = q->last = 0;
    q->size = size;
    q->flags = 0;

    if ( (q->buffer = rlc_malloc(sizeof(TCHAR) * size)) )
      return q;
  }

  return NULL;				/* not enough memory */
}


static int
rlc_resize_queue(RlcQueue q, int size)
{ TCHAR *newbuf;

  if ( (newbuf = rlc_malloc(size*sizeof(TCHAR))) )
  { TCHAR *o = newbuf;
    int c;

    while( (c=rlc_from_queue(q)) != -1 )
      *o++ = c;

    if ( q->buffer )
      rlc_free(q->buffer);
    q->buffer = newbuf;
    q->first = 0;
    q->last  = (int)(o-newbuf);
    q->size  = size;

    return true;
  }

  return false;
}


static int
rlc_add_queue(RlcData b, RlcQueue q, int chr)
{ int empty = (q->first == q->last);

  while(q->size < 50000)
  { if ( QN(q, q->last) != q->first )
    { q->buffer[q->last] = chr;
      q->last = QN(q, q->last);

      if ( empty )
	PostThreadMessage(b->application_thread_id, WM_RLC_INPUT, 0, 0);

      return true;
    }

    rlc_resize_queue(q, q->size*2);
  }

  return false;
}


int
rlc_is_empty_queue(RlcQueue q)
{ if ( q->first == q->last )
    return true;

  return false;
}


static int
rlc_from_queue(RlcQueue q)
{ if ( q->first != q->last )
  { int chr = q->buffer[q->first];

    q->first = QN(q, q->first);

    return chr;
  }

  return -1;
}

		 /*******************************
		 *	   BUFFERED I/O		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
When using UNICODE, count is in bytes!
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

size_t
rlc_read(rlc_console c, TCHAR *buf, size_t count)
{ RlcData d = rlc_get_data(c);
  size_t give;
  MSG msg;

  if ( d->closing )
    return 0;				/* signal EOF when closing */

  PostThreadMessage(d->console_thread_id,
		    WM_RLC_FLUSH,
		    0, 0);
  if ( _rlc_update_hook )
    (*_rlc_update_hook)();

  d->promptbuf[d->promptlen] = EOS;
  _tcscpy(d->prompt, d->promptbuf);

  if ( d->read_buffer.given >= d->read_buffer.length )
  { if ( d->read_buffer.line )
    { rlc_free(d->read_buffer.line);
      d->read_buffer.line = NULL;
    }

    if ( d->imode != IMODE_COOKED )
    { d->imode = IMODE_COOKED;
      d->imodeswitch = true;
    }

    while(!d->lhead)
    { if ( rlc_get_message(&msg, NULL, 0, 0) )
      { TranslateMessage(&msg);
	DispatchMessage(&msg);
      } else
	return -1;
    }

    { LQueued lq = d->lhead;
      d->read_buffer.line = lq->line;
      if ( lq->next )
	d->lhead = lq->next;
      else
	d->lhead = d->ltail = NULL;

      rlc_free(lq);
    }

    d->read_buffer.length = _tcslen(d->read_buffer.line);
    d->read_buffer.given = 0;
  }

  if ( d->read_buffer.length - d->read_buffer.given > count )
    give = count;
  else
    give = d->read_buffer.length - d->read_buffer.given;

  _tcsncpy(buf, d->read_buffer.line+d->read_buffer.given, give);
  d->read_buffer.given += give;

  return give;
}


static void
rlc_do_write(RlcData b, TCHAR *buf, int count)
{ if ( count > 0 )
  { int n = 0;
    TCHAR *s = buf;

    while(n++ < count)
    { int chr = *s++;

      if ( chr == '\n' )
	rlc_putansi(b, '\r');
      rlc_putansi(b, chr);
    }

    rlc_normalise(b);
    if ( b->window )
    { rlc_request_redraw(b);
      UpdateWindow(b->window);
    }
  }
}


int
rlc_flush_output(rlc_console c)
{ RlcData b = rlc_get_data(c);

  if ( !b )
    return -1;

  if ( b->output_queued )
  { rlc_do_write(b, b->output_queue, b->output_queued);

    b->output_queued = 0;
  }

  return 0;
}


size_t
rlc_write(rlc_console c, TCHAR *buf, size_t count)
{ DWORD_PTR result;
  TCHAR *e, *s;
  RlcData b = rlc_get_data(c);

  if ( !b )
    return -1;

  EnterCriticalSection(&b->lock);
  for(s=buf, e=&buf[count]; s<e; s++)
  { if ( *s == '\n' )
      b->promptlen = 0;
    else if ( b->promptlen < MAXPROMPT-1 )
      b->promptbuf[b->promptlen++] = *s;
  }
  LeaveCriticalSection(&b->lock);

  if ( b->window )
  { if ( SendMessageTimeout(b->window,
			    WM_RLC_WRITE,
			    (WPARAM)count,
			    (LPARAM)buf,
			    SMTO_NORMAL,
			    10000,
			    &result) )
    { PostMessage(b->window,
		  WM_RLC_FLUSH,
		  0, 0);
      return count;
    }
  }

  return -1;				/* I/O error */
}


static void
free_rlc_data(RlcData b)
{ b->magic = 42;			/* so next gets errors */

  if ( b->lines )
  { int i;

    for(i=0; i<b->height; i++)
    { if ( b->lines[i].text )
	free(b->lines[i].text);
    }

    free(b->lines);
  }
  if ( b->read_buffer.line )
    free(b->read_buffer.line);
  DeleteCriticalSection(&b->lock);

  free(b);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
rlc_close() tries to gracefully get rid of   the console thread. It does
so by posting WM_RLC_CLOSEWIN and then waiting for a WM_RLC_READY reply.
It waits for a maximum of  1.5  second,   which  should  be  fine as the
console thread should not have intptr_t-lasting activities.

If the timeout expires it hopes for the best. This was the old situation
and proved to be sound on Windows-NT, but not on 95 and '98.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

int
rlc_close(rlc_console c)
{ RlcData b = (RlcData)c;
  MSG msg;
  int i;

  if ( b->magic != RLC_MAGIC )
    return -1;

  rlc_save_options(b);
  b->closing = 3;
  PostMessage(b->window, WM_RLC_CLOSEWIN, 0, 0);

					/* wait for termination */
  for(i=0; i<30; i++)
  { if ( PeekMessage(&msg, NULL, WM_RLC_READY, WM_RLC_READY, PM_REMOVE) )
      break;
    Sleep(50);
  }

  b->magic = 0;
  free_user_data(c);
  free_rlc_data(b);

  return 0;
}


const TCHAR *
rlc_prompt(rlc_console c, const TCHAR *new)
{ RlcData b = rlc_get_data(c);

  if ( b )
  { if ( new )
    { _tcsncpy(b->prompt, new, MAXPROMPT);
      b->prompt[MAXPROMPT-1] = EOS;
    }

    return b->prompt;
  }

  return _T("");
}


void
rlc_clearprompt(rlc_console c)
{ RlcData b = rlc_get_data(c);

  if ( b )
  { b->promptlen = 0;
    b->prompt[0] = EOS;
  }
}


		 /*******************************
		 *	    MISC STUFF		*
		 *******************************/

void
rlc_title(rlc_console c, TCHAR *title, TCHAR *old, int size)
{ RlcData b = rlc_get_data(c);

  if ( old )
    memmove(old, b->current_title, size*sizeof(TCHAR));

  if ( title )
  { if ( b->window )
      SetWindowText(b->window, title);

    memmove(b->current_title, title, RLC_TITLE_MAX*sizeof(TCHAR));
  }
}


void
rlc_icon(rlc_console c, HICON icon)
{
  SetClassLongPtr(rlc_hwnd(c), GCLP_HICON, (LONG_PTR) icon);
}


bool
rlc_window_pos(rlc_console c,
	       HWND hWndInsertAfter,
	       int x, int y, int w, int h,
	       UINT flags)
{ RlcData b = rlc_get_data(c);

  if ( b )
  { w *= b->cw;
    h *= b->ch;

    SetWindowPos(b->window, hWndInsertAfter,
		 x, y, w, h,
		 flags);

    return true;
  }

  return false;
}


HANDLE
rlc_hinstance()
{ return _rlc_hinstance;
}


HWND
rlc_hwnd(rlc_console c)
{ RlcData b = rlc_get_data(c);

  return b ? b->window : (HWND)NULL;
}

		 /*******************************
		 *	 SETTING OPTIONS	*
		 *******************************/

int
rlc_copy_output_to_debug_output(int new)
{ int old = _rlc_copy_output_to_debug_output;

  _rlc_copy_output_to_debug_output = new;

  return old;
}

RlcUpdateHook
rlc_update_hook(RlcUpdateHook new)
{ RlcUpdateHook old = _rlc_update_hook;

  _rlc_update_hook = new;
  return old;
}

RlcTimerHook
rlc_timer_hook(RlcTimerHook new)
{ RlcTimerHook old = _rlc_timer_hook;

  _rlc_timer_hook = new;
  return old;
}

RlcRenderHook
rlc_render_hook(RlcRenderHook new)
{ RlcRenderHook old = _rlc_render_hook;

  _rlc_render_hook = new;
  return old;
}

RlcRenderAllHook
rlc_render_all_hook(RlcRenderAllHook new)
{ RlcRenderAllHook old = _rlc_render_all_hook;

  _rlc_render_all_hook = new;
  return old;
}

RlcInterruptHook
rlc_interrupt_hook(RlcInterruptHook new)
{ RlcInterruptHook old = _rlc_interrupt_hook;

  _rlc_interrupt_hook = new;
  return old;
}

RlcLinkHook
rlc_link_hook(RlcLinkHook new)
{ RlcLinkHook old = _rlc_link_hook;

  _rlc_link_hook = new;
  return old;
}

RlcResizeHook
rlc_resize_hook(RlcResizeHook new)
{ RlcResizeHook old = _rlc_resize_hook;

  _rlc_resize_hook = new;
  return old;
}

RlcMenuHook
rlc_menu_hook(RlcMenuHook new)
{ RlcMenuHook old = _rlc_menu_hook;

  _rlc_menu_hook = new;
  return old;
}


RlcMessageHook
rlc_message_hook(RlcMessageHook new)
{ RlcMessageHook old = _rlc_message_hook;

  _rlc_message_hook = new;
  return old;
}


int
rlc_set(rlc_console c, int what, uintptr_t data, RlcFreeDataHook hook)
{ RlcData b = rlc_get_data(c);

  switch(what)
  { default:
      if ( what >= RLC_VALUE(0) &&
	   what <= RLC_VALUE(MAX_USER_VALUES) )
      { b->values[what-RLC_VALUE(0)].data = data;
	b->values[what-RLC_VALUE(0)].hook = hook;
        return true;
      }
      return false;
  }
}


int
rlc_get(rlc_console c, int what, uintptr_t *data)
{ RlcData b = (RlcData)c;

  if ( !b )
    return false;

  switch(what)
  { case RLC_APPLICATION_THREAD:
      *data = (uintptr_t)b->application_thread;
      return true;
    case RLC_APPLICATION_THREAD_ID:
      *data = (uintptr_t)b->application_thread_id;
      return true;
    default:
      if ( what >= RLC_VALUE(0) &&
	   what <= RLC_VALUE(MAX_USER_VALUES) )
      { *data = b->values[what-RLC_VALUE(0)].data;
        return true;
      }
      return false;
  }
}


static void
free_user_data(RlcData b)
{ user_data *d = b->values;
  int i;

  for(i=0; i<MAX_USER_VALUES; i++, d++)
  { RlcFreeDataHook hook;

    if ( (hook=d->hook) )
    { uintptr_t data = d->data;
      d->hook = NULL;
      d->data = 0L;
      (*hook)(data);
    }
  }
}

		 /*******************************
		 *	       UTIL		*
		 *******************************/

static void
noMemory()
{ MessageBox(NULL, _T("Not enough memory"), _T("Console"), MB_OK|MB_TASKMODAL);

  ExitProcess(1);
}


void *
rlc_malloc(size_t size)
{ void *ptr = malloc(size);

  if ( !ptr && size > 0 )
    noMemory();

#ifdef _DEBUG
  memset(ptr, 0xbf, size);
#endif
  return ptr;
}


void *
rlc_realloc(void *ptr, size_t size)
{ void *ptr2 = realloc(ptr, size);

  if ( !ptr2 && size > 0 )
    noMemory();

  return ptr2;
}


void
rlc_free(void *ptr)
{ free(ptr);
}

#ifndef initHeapDebug

		 /*******************************
		 *	       DEBUG		*
		 *******************************/

static void
initHeapDebug(void)
{ int tmpFlag = _CrtSetDbgFlag( _CRTDBG_REPORT_FLAG );

  if ( !(tmpFlag & _CRTDBG_CHECK_ALWAYS_DF) )
  { /*MessageBox(NULL,
	       "setting malloc() debugging",
	       "SWI-Prolog console",
	       MB_OK|MB_TASKMODAL);*/
    tmpFlag |= _CRTDBG_CHECK_ALWAYS_DF;
    _CrtSetDbgFlag(tmpFlag);
  } else
  {
    /*MessageBox(NULL,
	       "malloc() debugging lready set",
	       "SWI-Prolog console",
	       MB_OK|MB_TASKMODAL);*/
  }
}

#endif /*initHeapDebug*/

#ifdef _DEBUG
#define LOGFILE "swipl-win.log"

#ifdef LOGFILE
static FILE *log = NULL;
#endif

static void
Dprintf(const TCHAR *fmt, ...)
{ va_list args;

  va_start(args, fmt);
#ifdef LOGFILE
  if ( !log )
    log = fopen(LOGFILE, "w");
  if ( log )
  { vfwprintf(log, fmt, args);
    fflush(log);
  }
#else
  TCHAR buf[1024];
  vswprintf(buf, sizeof(buf)/sizeof(TCHAR), fmt, args);
  OutputDebugString(buf);
#endif
  va_end(args);
}

static void
Dprint_links(TextLine tl, const TCHAR *msg)
{ if ( tl->links )
  { Dprintf(_T("%03d %ls:"), tl->line_no, msg);
    for(href *hr = tl->links; hr; hr=hr->next)
      Dprintf(_T(" %p = %d(%d) -> %ls"), hr, hr->start, hr->length, hr->link);
    Dprintf(_T("\n"));
  }
}

static void
Dprint_line(TextLine tl, bool links)
{ TCHAR buf[4096];
  TCHAR *o = buf;

  for(int x=0; x<tl->size; x++)
    *o++ = tl->text[x].code;
  *o = EOS;
  Dprintf(_T("%03d: (%03d) \"%ls\"\n"), tl->line_no, tl->size, buf);
  if ( links && tl->links )
    Dprint_links(tl, _T("  links"));
}

static void
Dprint_lines(RlcData b, int from, int to)
{ for( ; ; from = NextLine(b, from))
  { Dprint_line(&b->lines[from], true);

    if ( from == to )
      break;
  }
}

#endif
