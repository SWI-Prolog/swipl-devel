/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker and Matt Lilley
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2010, VU University, Amsterdam
    Copyright (C): 2009, SCIENTIFIC SOFTWARE AND SYSTEMS LIMITED

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

#define WINDOWS_LEAN_AND_MEAN 1
#if (_MSC_VER >= 1300)
#include <winsock2.h>			/* Needed on VC8 */
#include <windows.h>
#else
#include <windows.h>			/* Needed for MSVC 5&6 */
#include <winsock2.h>
#endif
#include "pl-incl.h"

#define ANSI_MAGIC		(0x734ab9de)
#define ANSI_BUFFER_SIZE	(256)
#define ANSI_MAX_ARGC		(10)

typedef enum
{ CMD_INITIAL = 0,
  CMD_ESC,
  CMD_ANSI
} astate;

typedef enum
{ HDL_CONSOLE = 0,
  HDL_FILE
} htype;

typedef struct
{ int magic;
  HANDLE hConsole;
  IOSTREAM *pStream;
  void *saved_handle;

  wchar_t buffer[ANSI_BUFFER_SIZE];
  size_t buffered;
  int argv[ANSI_MAX_ARGC];
  int argc;
  int argstat;
  astate cmdstat;			/* State for sequence processing */
  WORD def_attr;			/* Default attributes */
  htype handletype;                     /* Type of stream handle */
} ansi_stream;


static IOFUNCTIONS con_functions;
static IOFUNCTIONS *saved_functions;

static void
Message(const char *fm, ...)
{ char buf[1024];
  va_list(args);

  return;

  va_start(args, fm);
  vsprintf(buf, fm, args);
  MessageBox(NULL, buf, "SWI-Prolog", MB_OK|MB_TASKMODAL);
  va_end(args);
}


static int
flush_ansi(ansi_stream *as)
{ size_t written = 0;

  while ( written < as->buffered )
  { BOOL rc;
    DWORD done;

    if (as->handletype == HDL_CONSOLE)
    { rc = WriteConsoleW(as->hConsole,
		         &as->buffer[written],
		         (DWORD)(as->buffered-written),
		         &done,
		         NULL);
    } else
    { rc = WriteFile(as->hConsole,
                     &as->buffer[written],
                     (DWORD)(as->buffered-written),
                     &done,
                     NULL);
    }

    if ( rc )
    { written += done;
    } else
    { as->buffered = 0;
      return -1;
    }
  }

  as->buffered = 0;
  return 0;
}


static int
send_ansi(ansi_stream *as, int chr)
{ as->buffer[as->buffered++] = chr;

  if ( as->buffered == ANSI_BUFFER_SIZE ||
       (as->pStream->flags & SIO_NBUF) ||
       (chr == '\n' && (as->pStream->flags & SIO_LBUF)) )
    return flush_ansi(as);

  return 0;
}

#define FG_MASK (FOREGROUND_RED|FOREGROUND_BLUE|FOREGROUND_GREEN)
#define BG_MASK (BACKGROUND_RED|BACKGROUND_BLUE|BACKGROUND_GREEN)

static void
set_ansi_attributes(ansi_stream *as)
{ CONSOLE_SCREEN_BUFFER_INFO info;

  if ( GetConsoleScreenBufferInfo(as->hConsole, &info) )
  { int i;
    WORD attr = info.wAttributes;

    for(i=0; i < as->argc; i++)
    { switch( as->argv[i] )
      { case 0:
	  attr = as->def_attr;
	  break;
	case 1:
	  attr |= FOREGROUND_INTENSITY;
	  break;
	case 22:
	  attr &= ~FOREGROUND_INTENSITY;
	  break;
	default:
	  if ( as->argv[i] >= 30 && as->argv[i] <= 39 )
	  { int fg = as->argv[i] - 30;

	    attr &= ~FG_MASK;

	    if ( fg == 9 )		/* default */
	    { attr |= (as->def_attr & FG_MASK);
	    } else
	    { if ( fg % 2 == 1 )
		attr |= FOREGROUND_RED;
	      if ( fg >= 4 )
		attr |= FOREGROUND_BLUE;
	      if ( (fg == 2) || (fg == 3) || (fg == 6) || (fg == 7) )
		attr |= FOREGROUND_GREEN;
	    }
	  } else if ( as->argv[i] >= 40 && as->argv[i] <= 49 )
	  { int bg = as->argv[i] - 40;

	    attr &= ~BG_MASK;
	    if ( bg == 9 )		/* default */
	    { attr |= (as->def_attr & BG_MASK);
	    } else
	    { if ( bg % 2 == 1 )
		attr |= BACKGROUND_RED;
	      if ( bg >= 4 )
		attr |= BACKGROUND_BLUE;
	      if ( (bg == 2) || (bg == 3) || (bg == 6) || (bg == 7) )
		attr |= BACKGROUND_GREEN;
	    }
	  }
      }
    }

    if ( attr != info.wAttributes )
    { flush_ansi(as);
      SetConsoleTextAttribute(as->hConsole, attr);
    }
  }
}


static void
rlc_need_arg(ansi_stream *as, int arg, int def)
{ if ( as->argc < arg )
  { as->argv[arg-1] = def;
    as->argc = arg;
  }
}


static int
put_ansi(ansi_stream *as, int chr)
{ switch(as->cmdstat)
  { case CMD_INITIAL:
      switch(chr)
      {
#if 0
	case '\b':
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
#endif
	case 27:			/* ESC */
	  as->cmdstat = CMD_ESC;
	  break;
	default:
	  return send_ansi(as, chr);
      }
      break;
    case CMD_ESC:
      switch(chr)
      { case '[':
	  as->cmdstat = CMD_ANSI;
	  as->argc    = 0;
	  as->argstat = 0;		/* no arg */
	  break;
	default:
	  as->cmdstat = CMD_INITIAL;
	  break;
      }
      break;
    case CMD_ANSI:			/* ESC [ */
      if ( chr >= '0' && chr <= '9' )
      { if ( !as->argstat )
	{ as->argv[as->argc] = (chr - '0');
	  as->argstat = 1;		/* positive */
	} else
	{ as->argv[as->argc] = as->argv[as->argc] * 10 + (chr - '0');
	}

	break;
      }
      if ( !as->argstat && chr == '-' )
      { as->argstat = -1;		/* negative */
	break;
      }
      if ( as->argstat )
      { as->argv[as->argc] *= as->argstat;
	if ( as->argc < (ANSI_MAX_ARGC-1) )
	  as->argc++;			/* silently discard more of them */
	as->argstat = 0;
      }
      switch(chr)
      { case ';':
	  return 0;			/* wait for more args */
#if 0
	case 'H':
	case 'f':
	  rlc_need_arg(as, 1, 0);
	  rlc_need_arg(as, 2, 0);
	  CMD(rlc_set_caret(as, as->argv[0], as->argv[1]));
	  break;
	case 'A':
	  rlc_need_arg(as, 1, 1);
	  CMD(rlc_caret_up(as, as->argv[0]));
	  break;
	case 'B':
	  rlc_need_arg(as, 1, 1);
	  CMD(rlc_caret_down(as, as->argv[0]));
	  break;
	case 'C':
	  rlc_need_arg(as, 1, 1);
	  CMD(rlc_caret_forward(as, as->argv[0]));
	  break;
	case 'D':
	  rlc_need_arg(as, 1, 1);
	  CMD(rlc_caret_backward(as, as->argv[0]));
	  break;
	case 's':
	  CMD(rlc_save_caret_position(as));
	  break;
	case 'u':
	  CMD(rlc_restore_caret_position(as));
	  break;
	case 'J':
	  if ( as->argv[0] == 2 )
	    CMD(rlc_erase_display(as));
	  break;
	case 'K':
	  CMD(rlc_erase_line(as));
	  break;
#endif
	case 'm':
	  rlc_need_arg(as, 1, 0);
	  set_ansi_attributes(as);
      }
      as->cmdstat = CMD_INITIAL;
  }

  return 0;
}

static ssize_t
write_ansi(void *handle, char *buffer, size_t size)
{ ansi_stream *as = handle;
  size_t  n = size/sizeof(wchar_t);
  const wchar_t *s = (const wchar_t*)buffer;
  const wchar_t *e = &s[n];

  Message("Writing %d characters", n);

  for( ; s<e; s++)
  { if ( put_ansi(as, *s) != 0 )
      return -1;			/* error */
  }

  Message("Wrote %d characters", n);
  return n * sizeof(wchar_t);
}


static int
close_ansi(void *handle)
{ ansi_stream *as = handle;

  if ( as->magic == ANSI_MAGIC )
  { as->pStream->functions = saved_functions;
    as->pStream->handle    = as->saved_handle;

    PL_free(as);
    return 0;
  }

  return -1;
}


static int
control_ansi(void *handle, int op, void *data)
{ ansi_stream *as = handle;

  switch( op )
  { case SIO_FLUSHOUTPUT:
      return flush_ansi(as);
    case SIO_SETENCODING:
      return -1;			/* We cannot change the encoding! */
    case SIO_LASTERROR:
      return 0;				/* TBD */
    case SIO_GETFILENO:
    { int *fp = data;

      *fp = (int)(intptr_t)as->saved_handle; /* is one of 0,1,2 */
      return 0;
    }
    default:
      return -1;
  }
}


		 /*******************************
		 *	USER WIN32 CONSOLE	*
		 *******************************/

static ssize_t
Sread_win32_console(void *handle, char *buffer, size_t size)
{ GET_LD
  ansi_stream *as = handle;
  BOOL rc;
  DWORD done;
  DWORD mode;
  int isRaw = FALSE;

  if ( Suser_input &&
       Suser_input->handle == handle &&
       PL_ttymode(Suser_input) == PL_RAWTTY )
  { if ( GetConsoleMode(as->hConsole, &mode) &&
	 SetConsoleMode(as->hConsole,
			mode & ~(ENABLE_LINE_INPUT|ENABLE_ECHO_INPUT)) )
      isRaw = TRUE;
  }

  if ( !PL_wait_for_console_input(as->hConsole) )
    goto error;

  rc = ReadConsoleW(as->hConsole,
		    buffer,
		    (DWORD)(size / sizeof(wchar_t)),
		    &done,
		    NULL);

  if ( rc )
  { if ( isRaw )
      SetConsoleMode(as->hConsole, mode);
    return done * sizeof(wchar_t);
  }

error:
  if ( isRaw )
    SetConsoleMode(as->hConsole, mode);

  return -1;
}


static int
wrap_console(HANDLE h, IOSTREAM *s, IOFUNCTIONS *funcs)
{ ansi_stream *as;
  DWORD mode;

  as = PL_malloc(sizeof(*as));
  memset(as, 0, sizeof(*as));

  if (GetConsoleMode(h, &mode))
    as->handletype = HDL_CONSOLE;
  else
    as->handletype = HDL_FILE;

  as->hConsole     = h;
  as->pStream      = s;
  as->saved_handle = s->handle;

  s->handle    = as;
  s->encoding  = ENC_WCHAR;
  s->functions = funcs;
  s->flags &= ~SIO_FILE;

  return TRUE;
}


static void
init_output(void *handle, CONSOLE_SCREEN_BUFFER_INFO *info)
{ ansi_stream *as = handle;

  as->def_attr = info->wAttributes;
}


int
PL_w32_wrap_ansi_console(void)
{ HANDLE hIn    = GetStdHandle(STD_INPUT_HANDLE);
  HANDLE hOut   = GetStdHandle(STD_OUTPUT_HANDLE);
  HANDLE hError = GetStdHandle(STD_ERROR_HANDLE);
  CONSOLE_SCREEN_BUFFER_INFO info;

  if ( hIn    == INVALID_HANDLE_VALUE ||
       hOut   == INVALID_HANDLE_VALUE ||
       hError == INVALID_HANDLE_VALUE ||
       !GetConsoleScreenBufferInfo(hOut, &info) )
    return FALSE;

  saved_functions       = Sinput->functions;
  con_functions	        = *Sinput->functions;
  con_functions.read    = Sread_win32_console;
  con_functions.write   = write_ansi;
  con_functions.close   = close_ansi;
  con_functions.control = control_ansi;
  con_functions.seek    = NULL;

  wrap_console(hIn,    Sinput,  &con_functions);
  wrap_console(hOut,   Soutput, &con_functions);
  wrap_console(hError, Serror,  &con_functions);

  init_output(Soutput->handle, &info);
  init_output(Serror->handle, &info);

  PL_set_prolog_flag("tty_control", PL_BOOL, TRUE);
  return TRUE;
}
